//! TODO:
//! - Refactor Code
//!   - Split into multiple files.
//!   - Replace animation code with <https://github.com/merwaaan/bevy_spritesheet_animation>.
//!   - Replace egui with <https://github.com/UmbraLuminosa/sickle_ui> maybe?
//! - Improve combat
//! - Design turn system
//!   - When in range, enemy attacks player.
//!   - Drop items in neighbouring tiles if full.
//! - Line of sight
//!   - FoW for player.
//!   - When player goes out of LoS enemies go to last known position and "wait" before returning to
//!     their spawn point.
//! - Abilities
//!   - Implement tooltip.
//!   - Make sure its easy to add new abilities.
//! - Debuffs
//!   - Implement bleeding debuff on enemies, think about presentation.
//!   - Make sure its easy to integrate with abilities and add new ones.
//!
//! Not important:
//! - Show all levels in debug mode.
//!
//! Bugs:
//! - Cursor indicator show up incorrectly when outside the level in the south or west.
//! - Sometimes levels are not despawned correctly, leading to false walls and doors being cached.
//! - Fully loaded levels, before cleanup, can sometimes be seen for a single frame.

#![allow(
	clippy::multiple_crate_versions,
	clippy::unimplemented,
	clippy::wildcard_imports
)]

mod animation;
mod gameplay;
mod util;

use std::mem;
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use bevy::color::palettes::basic::*;
use bevy::input::keyboard::KeyboardInput;
use bevy::input::mouse::MouseButtonInput;
use bevy::input::ButtonState;
use bevy::prelude::{AssetServer, *};
use bevy::utils::{Entry, HashMap, HashSet};
use bevy::window::PrimaryWindow;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::helpers::square_grid::neighbors::Neighbors;
use bevy_ecs_tilemap::prelude::*;
use bevy_egui::egui::{Margin, TextWrapMode, TopBottomPanel};
use bevy_egui::{egui, EguiPlugin, EguiSet};
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pancam::{MoveMode, PanCam, PanCamPlugin};
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween, TweeningPlugin};
use egui::{
	Align, Align2, Area, Color32, FontId, Frame, Id, Label, Layout, Pos2, RichText, Sense,
	SidePanel, Stroke, Widget,
};
use gameplay::{
	death, handle_ability_event, spawn_healthbar, tick_cooldowns, update_healthbar, AbilityEvent,
	ActiveAbility, DeathEvent, Enemy, EnemyBundle, Health, Player, PlayerBundle, Spellbook,
};
use helpers::square_grid::neighbors::SquareDirection;
use pathfinding::prelude::astar;

use self::animation::{Animation, AnimationArrived, AnimationFinish};

/// The size of the Grid in pixels.
const GRID_SIZE: i32 = 16;

/// State for controlling the current turn state of the game.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Resource, Reflect)]
#[reflect(Resource)]
enum TurnState {
	/// Waiting for player input.
	#[default]
	Waiting,
	/// Player animation running.
	Running,
	/// Waiting for enemy behavior to be scheduled.
	EnemiesWaiting,
	/// Enemy animation running.
	EnemiesRunning,
}

/// Marker components.
#[derive(Component, Default)]
struct Key;

/// Key bundle.
#[derive(Bundle, Default, LdtkEntity)]
struct KeyBundle {
	/// Key marker component.
	key:                 Key,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// Key grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
}

/// Component for tracking the item drops of enemies entities.
#[derive(Clone, Component, Debug, Default, Reflect, PartialEq, Eq)]
struct Drops(Vec<String>);

#[allow(clippy::fallible_impl_from)]
impl From<&EntityInstance> for Drops {
	fn from(entity_instance: &EntityInstance) -> Self {
		let reference = entity_instance
			.get_maybe_strings_field("Drops")
			.expect("expected entity to have non-nullable `Drops` string array field")
			.iter()
			.cloned()
			.map(|drop| drop.expect("expected non-nullable string field"))
			.collect();

		Self(reference)
	}
}

/// Caches which entity the Player is teleporting to (currently just between doors).
#[derive(Default, Resource)]
struct PlayerEntityDestination(Option<EntityIid>);

#[derive(Component, PartialEq, Eq)]
///  Marker component for the targeting marker (highlights the tile u are hovering with your mouse).
struct TargetingMarker(Option<Entity>);

/// Saves the cursor world position.
#[derive(Resource, Default, Clone, Copy, Debug)]
pub struct CursorPos {
	/// The cursor position in the Bevy world.
	world_pos: Vec2,
	/// The tile position of the cursor.
	tile_pos:  GridCoords,
}

/// Wall marker component.
#[derive(Default, Component)]
struct Wall;

/// Wall bundle.
#[derive(Default, Bundle, LdtkIntCell)]
struct WallBundle {
	/// Wall marker component.
	wall: Wall,
}

/// Component storing to which door a door leads.
#[derive(Clone, Component, Debug, Default, Reflect)]
struct Door {
	/// Level the destination door is in.
	level:  LevelIid,
	/// Entity of the destination door.
	entity: EntityIid,
}

impl From<&EntityInstance> for Door {
	fn from(entity_instance: &EntityInstance) -> Self {
		let reference = entity_instance
			.get_entity_ref_field("DestinationDoor")
			.expect("expected entity to have non-nullable `DestinationDoor` entity reference field")
			.clone();

		Self {
			level:  LevelIid::new(reference.level_iid),
			entity: EntityIid::new(reference.entity_iid),
		}
	}
}

/// Door bundle.
#[derive(Default, Bundle, LdtkEntity)]
struct DoorBundle {
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	#[from_entity_instance]
	/// Door data.
	door:                Door,
	/// Door grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
}

/// Caches everything in the current level we don't want to loop through.
#[derive(Default, Resource, Reflect)]
#[reflect(Resource)]
struct LevelCache {
	/// The cashed walls of this level.
	walls:   HashSet<GridCoords>,
	/// The cashed doors of this level.
	doors:   HashMap<GridCoords, (Entity, EntityIid, Door)>,
	/// The cashed enemies of this level.
	enemies: HashMap<GridCoords, Entity>,
	/// The cashed keys of this level.
	keys:    HashMap<GridCoords, (Entity, ItemSource)>,
	/// The level width in tiles.
	width:   i32,
	/// The level height in tiles.
	height:  i32,
}

/// Source of item could be LDTK level or drop from enemy.
#[derive(Eq, Hash, PartialEq, Reflect)]
enum ItemSource {
	/// LDTK level.
	Static(EntityIid),
	/// Drop from enemy.
	Loot(EntityIid),
}

impl LevelCache {
	/// Checks if [`GridCoords`] is a wall or outside of the Level.
	fn destination(
		&self,
		state: Option<&GameState>,
		source: GridCoords,
		destination: GridCoords,
	) -> Destination {
		if self.outside_boundary(destination) {
			if let Some((_, _, door)) = self.doors.get(&source) {
				Destination::Door(door.clone())
			} else {
				Destination::BeyondBoundary
			}
		} else if self.walls.contains(&destination) {
			Destination::Wall
		} else if let Some((_, iid, _)) = self.doors.get(&destination) {
			if state.is_some_and(|state| *state.doors.get(iid).unwrap()) {
				Destination::Walkable
			} else {
				Destination::Wall
			}
		} else if self.enemies.contains_key(&destination) {
			Destination::Enemy
		} else {
			Destination::Walkable
		}
	}

	/// Checks if [`GridCoords`] is outside of the Level.
	const fn outside_boundary(&self, grid_coords: GridCoords) -> bool {
		// Currently Grid coords can't be smaller than 0.
		grid_coords.x < 0
			|| grid_coords.y < 0
			|| grid_coords.x >= self.width
			|| grid_coords.y >= self.height
	}
}

/// What the Destination of an attempted movement of the player is.
enum Destination {
	/// The potential destination is a wall.
	Wall,
	/// The potential destination is outside of the level boundary.
	BeyondBoundary,
	/// The potential destination is walkable.
	Walkable,
	/// An enemy is occupying the potential destination.
	Enemy,
	/// The potential destination is walkable.
	Door(Door),
}

/// Startup system.
#[allow(clippy::needless_pass_by_value)]
fn startup(mut commands: Commands<'_, '_>, asset_server: Res<'_, AssetServer>) {
	let mut camera = Camera2dBundle::default();
	camera.projection.scale = 0.5;
	commands.spawn(camera).insert(PanCam {
		move_mode: MoveMode::Mouse,
		grab_buttons: Vec::new(),
		zoom_to_cursor: false,
		min_scale: 0.1,
		max_scale: Some(1.),
		..PanCam::default()
	});

	commands
		.spawn(LdtkWorldBundle {
			ldtk_handle: asset_server.load("test.ldtk"),
			..default()
		})
		.insert(Name::new("Map"));

	commands.spawn((
		SpriteBundle {
			texture: asset_server.load("tile-tip.png"),
			transform: Transform::from_translation(Vec3::new(0., 0., 5.)),
			sprite: Sprite {
				custom_size: Some(Vec2::new(16., 16.)),
				..Sprite::default()
			},
			..SpriteBundle::default()
		},
		TargetingMarker(None),
		GridCoords::default(),
		Name::new("Tile Target Marker"),
	));
}

/// Initialize states after level is spawned.
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
fn level_spawn(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut level_events: EventReader<'_, '_, LevelEvent>,
	mut texture_atlas_layouts: ResMut<'_, Assets<TextureAtlasLayout>>,
	walls: Query<'_, '_, &GridCoords, (With<Wall>, Without<Player>)>,
	enemies: Query<
		'_,
		'_,
		(&GridCoords, Entity, &EntityIid, &TextureAtlas, &Drops),
		(With<Enemy>, Without<Player>),
	>,
	keys: Query<'_, '_, (&GridCoords, Entity, &EntityIid), (With<Key>, Without<Player>)>,
	mut doors: Query<
		'_,
		'_,
		(Entity, &GridCoords, &EntityIid, &mut TextureAtlas, &Door),
		(Without<Player>, Without<Enemy>),
	>,
	mut player_entity_destination: ResMut<'_, PlayerEntityDestination>,
	ldtk_entities: Query<'_, '_, (Entity, &EntityIid), Without<Player>>,
	entity_grid_coords: Query<'_, '_, &GridCoords, Without<Player>>,
	mut player: Query<'_, '_, &mut GridCoords, With<Player>>,
	ldtk_project_entities: Query<'_, '_, &Handle<LdtkProject>>,
	ldtk_project_assets: Res<'_, Assets<LdtkProject>>,
) {
	let Some(LevelEvent::Spawned(level_iid)) = level_events.read().next() else {
		return;
	};

	// We need to get the size of the level from the ldtk_projekt_asset...
	let ldtk_project = ldtk_project_assets
		.get(ldtk_project_entities.single())
		.expect("LdtkProject should be loaded when level is spawned");
	let level = ldtk_project
		.get_raw_level_by_iid(level_iid.get())
		.expect("spawned level should exist in project");

	let mut enemies_map = HashMap::new();

	for (grid_coords, entity, iid, _, drops) in &enemies {
		enemies_map.insert(*grid_coords, entity);

		if drops.0.iter().any(|drop| drop == "Key") {
			state
				.keys
				.entry(ItemSource::Loot(iid.clone()))
				.or_insert(false);
		}
	}

	let mut keys_map = HashMap::new();

	for (position, entity, iid) in &keys {
		let taken = match state.keys.entry(ItemSource::Static(iid.clone())) {
			Entry::Occupied(value) => *value.get(),
			Entry::Vacant(entry) => *entry.insert(false),
		};

		if taken {
			commands.entity(entity).insert(Visibility::Hidden);
		} else {
			keys_map.insert(*position, (entity, ItemSource::Static(iid.clone())));
		}
	}

	let mut doors_map = HashMap::new();

	for (entity, grid_coords, iid, mut atlas, door) in &mut doors {
		doors_map.insert(*grid_coords, (entity, iid.clone(), door.clone()));

		let open = match state.doors.entry(iid.clone()) {
			Entry::Occupied(value) => *value.get(),
			Entry::Vacant(entry) => *entry.insert(false),
		};

		if open {
			atlas.index = 133;
		} else {
			atlas.index = 108;
		}

		state.doors.entry(door.entity.clone()).or_insert(open);
	}

	// ... so we can update the [`LevelCache`] resource.
	*level_cache = LevelCache {
		walls:   walls.iter().copied().collect(),
		enemies: enemies_map,
		keys:    keys_map,
		doors:   doors_map,
		width:   level.px_wid / GRID_SIZE,
		height:  level.px_hei / GRID_SIZE,
	};

	// We need to update the players grid coords on level changes to the correct "entrance".
	if let Some(destination_entity_iid) = &player_entity_destination.0 {
		let (destination_entity, _) = ldtk_entities
			.iter()
			.find(|(_, entityiid)| *entityiid == destination_entity_iid)
			.expect("the entity IID should exist");

		let destination_grid_coords = entity_grid_coords
			.get(destination_entity)
			.expect("destination entity should exist");
		let mut player_grid_coords = player.single_mut();
		*player_grid_coords = *destination_grid_coords;

		player_entity_destination.0 = None;
	}

	// Clear enemy order.
	state.enemies.clear();

	// Our Spritesheets have sprites with different sized tiles in it so we fix them.
	for (_, entity, _, enemy, _) in enemies.iter() {
		state.enemies.push((entity, true));

		let layout = texture_atlas_layouts
			.get_mut(&enemy.layout)
			.expect("texture atlas layout not found for enemy");
		for texture in layout
			.textures
			.get_mut(16..=23)
			.expect("unexpected enemy skeleton sprite sheet size")
			.iter_mut()
		{
			texture.max.y = 112;
		}

		layout.textures.truncate(24);
	}
}

/// Game state.
#[derive(Default, Reflect, Resource)]
#[reflect(Resource)]
struct GameState {
	/// The current turn.
	turn:        u64,
	/// State of each door.
	doors:       HashMap<EntityIid, bool>,
	/// State of each key.
	keys:        HashMap<ItemSource, bool>,
	/// Current enemy order.
	enemies:     Vec<(Entity, bool)>,
	/// Amount of keys the player possesses.
	player_keys: u8,
}

/// Stores debug state.
#[derive(Default, Resource)]
enum Debug {
	/// Debugging not active.
	#[default]
	Inactive,
	/// Debugging active.
	Active {
		/// Camera [`Transform`] state before debugging.
		cam_transform:  Transform,
		/// Camera [`OrthographicProjection`] state before debugging.
		cam_projection: OrthographicProjection,
	},
}

/// Enables/Disables debug mode.
#[allow(clippy::needless_pass_by_value)]
fn debug(
	mut context: EguiContexts<'_, '_>,
	mut input: EventReader<'_, '_, KeyboardInput>,
	mut debug: ResMut<'_, Debug>,
	mut cam: Query<
		'_,
		'_,
		(&mut Transform, &mut OrthographicProjection, &mut PanCam),
		With<Camera>,
	>,
) {
	if context.ctx_mut().wants_keyboard_input() {
		return;
	}

	for input in input.read() {
		if let KeyboardInput {
			key_code: KeyCode::Backquote,
			state: ButtonState::Released,
			..
		} = input
		{
			let (mut cam_transform, mut cam_projection, mut pan_cam) = cam.single_mut();

			match mem::take(debug.deref_mut()) {
				Debug::Inactive => {
					pan_cam.grab_buttons =
						vec![MouseButton::Left, MouseButton::Right, MouseButton::Middle];
					pan_cam.zoom_to_cursor = true;

					*debug = Debug::Active {
						cam_transform:  *cam_transform,
						cam_projection: cam_projection.clone(),
					}
				}
				Debug::Active {
					cam_transform: old_cam_transform,
					cam_projection: old_cam_projection,
				} => {
					*cam_transform = old_cam_transform;
					*cam_projection = old_cam_projection;

					pan_cam.grab_buttons = Vec::new();
					pan_cam.zoom_to_cursor = false;

					*debug = Debug::Inactive;
				}
			}
		}
	}
}

/// We need to keep the cursor position updated based on any `CursorMoved` events.
#[allow(clippy::needless_pass_by_value)]
fn update_cursor_pos(
	camera_q: Query<'_, '_, (&GlobalTransform, &Camera)>,
	q_windows: Query<'_, '_, &Window, With<PrimaryWindow>>,
	mut cursor_pos: ResMut<'_, CursorPos>,
) {
	if let Some(position) = q_windows.single().cursor_position() {
		// To get the mouse's world position, we have to transform its window position by
		// any transforms on the camera. This is done by projecting the cursor position into
		// camera space (world space).
		for (cam_t, cam) in camera_q.iter() {
			if let Some(pos) = cam.viewport_to_world_2d(cam_t, position) {
				if cursor_pos.world_pos != pos {
					cursor_pos.world_pos = pos;

					cursor_pos.tile_pos = utils::translation_to_grid_coords(
						cursor_pos.world_pos,
						IVec2::splat(GRID_SIZE),
					);
				}
			}
		}
	}
}

/// Lets player cast an ability with a mouseclick.
#[allow(clippy::needless_pass_by_value)]
fn cast_ability(
	mut inputs: EventReader<'_, '_, MouseButtonInput>,
	mut abilities: EventWriter<'_, AbilityEvent>,
	player: Query<'_, '_, (Entity, &ActiveAbility), With<Player>>,
	targeting_marker: Query<'_, '_, &TargetingMarker>,
) {
	let Some(target_entity) = targeting_marker.single().0 else {
		return;
	};

	for input in inputs.read() {
		let MouseButtonInput {
			state: ButtonState::Pressed,
			button: MouseButton::Left,
			..
		} = input
		else {
			continue;
		};

		let (player_entity, ability) = player.single();

		abilities.send(AbilityEvent::new(player_entity, target_entity, ability.0));

		return;
	}
}

/// Updates the `TargetingMarker` with updated `CursorPos`.
#[allow(clippy::needless_pass_by_value)]
fn update_target_marker(
	cursor_pos: Res<'_, CursorPos>,
	player: Query<'_, '_, (&GridCoords, &Spellbook, &ActiveAbility), With<Player>>,
	mut target_marker: Query<
		'_,
		'_,
		(
			&mut TargetingMarker,
			&mut Visibility,
			&mut GridCoords,
			&mut Sprite,
		),
		Without<Player>,
	>,
	level_cache: Res<'_, LevelCache>,
) {
	let (mut marker, mut visibility, mut grid_coords, mut sprite) = target_marker.single_mut();
	let Ok((player_grid_coords, spellbook, active_ability)) = player.get_single() else {
		return;
	};

	if level_cache.outside_boundary(cursor_pos.tile_pos) {
		*visibility = Visibility::Hidden;
		marker.set_if_neq(TargetingMarker(None));
	} else {
		visibility.set_if_neq(Visibility::Visible);
		grid_coords.set_if_neq(cursor_pos.tile_pos);

		if let Some(entity) = level_cache.enemies.get(grid_coords.deref()) {
			let ability = spellbook
				.0
				.get(&active_ability.0)
				.expect("Player has to have an active ability.");

			if ability.in_euclidean_range(*player_grid_coords, *grid_coords) {
				sprite.color = RED.into();
			} else {
				sprite.color = YELLOW.into();
			}

			marker.set_if_neq(TargetingMarker(Some(*entity)));
		} else {
			sprite.color = Color::WHITE;
			marker.set_if_neq(TargetingMarker(None));
		}
	}
}

/// Updates Camera with player movement.
#[allow(clippy::needless_pass_by_value)]
fn camera_update(
	player: Query<'_, '_, &Transform, (With<Player>, Changed<Transform>)>,
	mut cam: Query<'_, '_, &mut Transform, (With<Camera>, Without<Player>)>,
) {
	if let Ok(player) = player.get_single() {
		let mut cam = cam.single_mut();

		cam.translation.x = player.translation.x;
		cam.translation.y = player.translation.y;
	}
}

/// Opens and closes doors.
#[allow(clippy::needless_pass_by_value)]
fn door_interactions(
	mut state: ResMut<'_, GameState>,
	level_cache: Res<'_, LevelCache>,
	mut doors_query: Query<'_, '_, &mut TextureAtlas, With<Door>>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	player: Query<'_, '_, &GridCoords, With<Player>>,
	mut input: EventReader<'_, '_, KeyboardInput>,
) {
	let Some(input) = input.read().next() else {
		return;
	};
	let KeyboardInput {
		state: ButtonState::Pressed,
		repeat: false,
		..
	} = input
	else {
		return;
	};

	if let KeyCode::KeyF = input.key_code {
		let player_grid_coords = player.single();
		let tile_map_size = tile_map_size.single();

		for tile_pos in Neighbors::get_square_neighboring_positions(
			&TilePos::from(*player_grid_coords),
			tile_map_size,
			true,
		)
		.iter()
		{
			if let Some((entity, iid, door)) = level_cache.doors.get(&GridCoords::from(*tile_pos)) {
				let GameState {
					turn,
					doors,
					player_keys,
					..
				} = state.deref_mut();

				let open = doors.get_mut(iid).unwrap();

				if !*open && *player_keys > 0 {
					*turn += 1;

					*open = true;
					*player_keys -= 1;
					*state.doors.get_mut(&door.entity).unwrap() = true;
					doors_query.get_mut(*entity).unwrap().index = 133;
				}
			}
		}
	}
}

/// Player movement.
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
fn player_movement(
	mut state: ResMut<'_, GameState>,
	mut commands: Commands<'_, '_>,
	mut inputs: EventReader<'_, '_, KeyboardInput>,
	mut cast_ability: EventWriter<'_, AbilityEvent>,
	mut player: Query<
		'_,
		'_,
		(
			Entity,
			&ActiveAbility,
			&Transform,
			&mut GridCoords,
			&mut Sprite,
			&mut TextureAtlas,
			&mut Animation,
		),
		With<Player>,
	>,
	level_cache: Res<'_, LevelCache>,
	mut level_selection: ResMut<'_, LevelSelection>,
	mut destination_entity: ResMut<'_, PlayerEntityDestination>,
	mut animation_state: ResMut<'_, TurnState>,
	mut buffered_movement: Local<'_, Option<KeyCode>>,
) {
	for keycode in (*buffered_movement)
		.into_iter()
		.chain(inputs.read().filter_map(|input| {
			if let KeyboardInput {
				state: ButtonState::Pressed,
				repeat: false,
				..
			} = input
			{
				Some(input.key_code)
			} else {
				None
			}
		})) {
		let (direction, flip) = match keycode {
			KeyCode::KeyW => (GridCoords::new(0, 1), None),
			KeyCode::KeyA => (GridCoords::new(-1, 0), Some(true)),
			KeyCode::KeyS => (GridCoords::new(0, -1), None),
			KeyCode::KeyD => (GridCoords::new(1, 0), Some(false)),
			KeyCode::KeyQ => (GridCoords::new(-1, 1), Some(true)),
			KeyCode::KeyE => (GridCoords::new(1, 1), Some(false)),
			KeyCode::KeyZ => (GridCoords::new(-1, -1), Some(true)),
			KeyCode::KeyC => (GridCoords::new(1, -1), Some(false)),
			_ => continue,
		};

		if !matches!(animation_state.as_ref(), TurnState::Waiting) {
			*buffered_movement = Some(keycode);
			continue;
		}

		*buffered_movement = None;

		let (
			player_entity,
			active_ability,
			transform,
			mut grid_pos,
			mut sprite,
			mut atlas,
			mut animation,
		) = player.single_mut();
		let destination = *grid_pos + direction;

		match level_cache.destination(Some(state.deref()), *grid_pos, destination) {
			Destination::Walkable => {
				state.turn += 1;
				*animation_state = TurnState::Running;

				let mut player = commands.entity(player_entity);
				player.insert(Animator::new(
					Tween::new(
						EaseMethod::Linear,
						Duration::from_millis(250),
						TransformPositionLens {
							start: transform.translation,
							end:   utils::grid_coords_to_translation(
								destination,
								IVec2::splat(GRID_SIZE),
							)
							.extend(transform.translation.z),
						},
					)
					.with_completed_event(0),
				));

				*animation = PlayerBundle::walking_animation();
				atlas.index = animation.first;

				let arrived_event = if let Some((_, source)) = level_cache.keys.get(&destination) {
					*state.keys.get_mut(source).unwrap() = true;
					state.player_keys += 1;
					Some(destination)
				} else {
					None
				};

				player.insert(AnimationFinish {
					arrived_event,
					new_animation: Some(PlayerBundle::idle_animation()),
					next_state: Some(TurnState::EnemiesWaiting),
				});

				*grid_pos = destination;

				if let Some(flip) = flip {
					sprite.flip_x = flip;
				}
			}
			Destination::Wall => (),
			Destination::Enemy => {
				cast_ability.send(AbilityEvent::new(
					player_entity,
					*level_cache.enemies.get(&destination).unwrap(),
					active_ability.0,
				));
			}
			Destination::Door(door) => {
				if let LevelSelection::Iid(current_level) = level_selection.as_mut() {
					*current_level = door.level;
					destination_entity.0 = Some(door.entity);
				} else {
					unreachable!("levels should only be `LevelIid`")
				}
			}
			Destination::BeyondBoundary => unreachable!("somehow dropped off the floor"),
		}
	}
}

/// Updates [`Transform`] for [`Entity`]s with changed [`GridCoords`].
#[allow(clippy::type_complexity)]
fn translate_grid_coords_entities(
	mut entities: Query<
		'_,
		'_,
		(&mut Transform, &GridCoords),
		(Changed<GridCoords>, Without<Animator<Transform>>),
	>,
) {
	for (mut transform, grid_coords) in &mut entities {
		transform.translation =
			utils::grid_coords_to_translation(*grid_coords, IVec2::splat(GRID_SIZE))
				.extend(transform.translation.z);
	}
}

/// Item UI.
#[allow(clippy::needless_pass_by_value)]
fn item_ui(
	asset_server: Res<'_, AssetServer>,
	state: Res<'_, GameState>,
	mut contexts: EguiContexts<'_, '_>,
) {
	let key_texture =
		contexts.add_image(asset_server.load("Environment/Dungeon Prison/Assets/Props.png"));

	let Some(context) = contexts.try_ctx_mut() else {
		return;
	};

	SidePanel::right("items")
		.resizable(false)
		.show_separator_line(false)
		.frame(Frame::none())
		.show(context, |ui| {
			ui.with_layout(Layout::top_down(Align::Max), |ui| {
				if state.player_keys > 0 {
					Frame::default()
						.fill(Color32::BLACK)
						.stroke(Stroke::new(2., Color32::WHITE))
						.rounding(5.)
						.show(ui, |ui| {
							let (response, painter) = ui.allocate_painter(
								egui::Vec2::new(64., 64.),
								Sense {
									click:     false,
									drag:      false,
									focusable: false,
								},
							);

							painter.image(
								key_texture,
								response.rect,
								egui::Rect::from([
									Pos2::new(1. / 400. * 32., 1. / 400. * 64.),
									Pos2::new(1. / 400. * 48., 1. / 400. * 80.),
								]),
								Color32::WHITE,
							);

							let text = format!("{}x", state.player_keys);

							// Text shadow.
							painter.text(
								(response.rect.right_top() - Pos2::new(4., -4.)).to_pos2(),
								Align2::RIGHT_TOP,
								&text,
								FontId {
									size: 24.,
									..FontId::default()
								},
								Color32::BLACK,
							);

							painter.text(
								(response.rect.right_top() - Pos2::new(2., -2.)).to_pos2(),
								Align2::RIGHT_TOP,
								text,
								FontId {
									size: 24.,
									..FontId::default()
								},
								Color32::WHITE,
							);

							response
						});
				}
			})
		});
}

/// Turn counter UI.
#[allow(clippy::needless_pass_by_value)]
fn turn_ui(state: Res<'_, GameState>, mut contexts: EguiContexts<'_, '_>) {
	let Some(context) = contexts.try_ctx_mut() else {
		return;
	};

	Area::new(Id::new("turn"))
		.fixed_pos(Pos2::new(0., 0.))
		.show(context, |ui| {
			Frame::default()
				.fill(Color32::BLACK)
				.stroke(Stroke::new(2., Color32::WHITE))
				.rounding(5.)
				.inner_margin(Margin::symmetric(6., 4.))
				.show(ui, |ui| {
					Label::new(
						RichText::new(format!("Turn: {}", state.turn))
							.color(Color32::WHITE)
							.size(24.),
					)
					.selectable(false)
					.wrap_mode(TextWrapMode::Extend)
					.ui(ui)
				});
		});
}

/// Abilities UI.
#[allow(clippy::needless_pass_by_value)]
fn ability_ui(
	player: Query<'_, '_, (&Spellbook, &ActiveAbility), With<Player>>,
	mut contexts: EguiContexts<'_, '_>,
	state: Res<'_, GameState>,
) {
	let Ok((spellbook, active_ability)) = player.get_single() else {
		return;
	};

	let Some(context) = contexts.try_ctx_mut() else {
		return;
	};

	TopBottomPanel::bottom("abilities").show(context, |ui| {
		ui.horizontal(|ui| {
			for ((_, ability), button) in spellbook.0.iter().zip(1..) {
				let mut text = format!("{}. {}", button, ability.name);
				let mut color = None;

				if let Some(cooldown_left) = ability.cooldown_left(state.turn) {
					text = format!("{text} ({cooldown_left})");
					color = Some(Color32::RED);
				}

				if ability.id == active_ability.0 {
					ui.label(RichText::new(text).color(color.unwrap_or(Color32::GREEN)));
				} else {
					ui.label(RichText::new(text).color(color.unwrap_or(Color32::YELLOW)));
				}
			}
		});
	});
}

/// Selects ability when pressing number keys.
fn select_ability(
	mut input: EventReader<'_, '_, KeyboardInput>,
	mut player: Query<'_, '_, (&Spellbook, &mut ActiveAbility), With<Player>>,
) {
	let Ok((spellbook, mut active_ability)) = player.get_single_mut() else {
		return;
	};

	for ev in input.read() {
		let mut spellbook_iter = spellbook.0.iter();

		if let ButtonState::Pressed = ev.state {
			match ev.key_code {
				KeyCode::Digit1 => {
					active_ability.0.clone_from(
						spellbook_iter
							.next()
							.expect("first slot always needs to have autoattack")
							.0,
					);
				}
				KeyCode::Digit2 => {
					if let Some(name) = spellbook_iter.nth(1) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit3 => {
					if let Some(name) = spellbook_iter.nth(2) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit4 => {
					if let Some(name) = spellbook_iter.nth(3) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit5 => {
					if let Some(name) = spellbook_iter.nth(4) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit6 => {
					if let Some(name) = spellbook_iter.nth(5) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit7 => {
					if let Some(name) = spellbook_iter.nth(6) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit8 => {
					if let Some(name) = spellbook_iter.nth(7) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit9 => {
					if let Some(name) = spellbook_iter.nth(8) {
						active_ability.0.clone_from(name.0);
					}
				}
				_ => {}
			}
		}
	}
}

/// Moves enemies when its their turn.
#[allow(
	clippy::needless_pass_by_value,
	clippy::type_complexity,
	clippy::too_many_arguments
)]
fn move_enemies(
	mut commands: Commands<'_, '_>,
	mut game_state: ResMut<'_, GameState>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut turn_state: ResMut<'_, TurnState>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	player: Query<'_, '_, (Entity, &GridCoords), (Without<Enemy>, With<Player>)>,
	mut enemies: Query<
		'_,
		'_,
		(
			Entity,
			&mut GridCoords,
			&Transform,
			&mut Sprite,
			&Spellbook,
			&mut TextureAtlas,
			&mut Animation,
		),
		(With<Enemy>, Without<Player>),
	>,
	mut cast_ability: EventWriter<'_, AbilityEvent>,
) {
	for (enemy, ready) in game_state.enemies.iter_mut().filter(|(_, ready)| *ready) {
		*ready = false;

		let tile_map_size = tile_map_size.single();
		let (player_entity, player_pos) = player.single();
		let (
			enemy_entity,
			mut enemy_pos,
			enemy_transform,
			mut enemy_sprite,
			spellbook,
			mut enemy_atlas,
			mut enemy_animation,
		) = enemies
			.get_mut(*enemy)
			.expect("enemy for its turn not found");

		let dx = player_pos.x - enemy_pos.x;
		let dy = player_pos.y - enemy_pos.y;
		let degrees = f64::atan2(dy.into(), dx.into()).to_degrees();

		#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
		match degrees.round() as i16 {
			-89..90 => enemy_sprite.flip_x = false,
			-180..-90 | 91..=180 => enemy_sprite.flip_x = true,
			-90 | 90 => (),
			angle => unreachable!("invalid angle found: {}", angle),
		}

		let Some((path, _)) = astar(
			enemy_pos.as_ref(),
			|grid_pos| {
				/// We want cardinal directions first.
				const DIRECTIONS: [SquareDirection; 8] = [
					SquareDirection::East,
					SquareDirection::North,
					SquareDirection::West,
					SquareDirection::South,
					SquareDirection::NorthEast,
					SquareDirection::NorthWest,
					SquareDirection::SouthWest,
					SquareDirection::SouthEast,
				];
				let neighbors = Neighbors::get_square_neighboring_positions(
					&TilePos::from(*grid_pos),
					tile_map_size,
					true,
				);
				DIRECTIONS
					.into_iter()
					.filter_map(|direction| neighbors.get(direction))
					.filter_map(|pos| {
						let destination = GridCoords::from(*pos);
						let walkable = level_cache.destination(None, *enemy_pos, destination);
						matches!(walkable, Destination::Walkable).then_some((destination, 1))
					})
					.collect::<Vec<_>>()
			},
			|start| {
				#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
				let distance = util::euclidean_distance(*player_pos, *start) as i32;
				distance
			},
			|current_pos| current_pos == player_pos,
		) else {
			continue;
		};

		let destination = path.get(1).expect("found empty path");

		if destination == player_pos {
			cast_ability.send(AbilityEvent::new(enemy_entity, player_entity, 0));
			return;
		}

		let mut enemy_commands = commands.entity(enemy_entity);
		enemy_commands.insert(Animator::new(
			Tween::new(
				EaseMethod::Linear,
				Duration::from_millis(250),
				TransformPositionLens {
					start: enemy_transform.translation,
					end:   utils::grid_coords_to_translation(*destination, IVec2::splat(GRID_SIZE))
						.extend(enemy_transform.translation.z),
				},
			)
			.with_completed_event(0),
		));

		*enemy_animation = EnemyBundle::walking_animation();
		enemy_atlas.index = enemy_animation.first;

		enemy_commands.insert(AnimationFinish {
			arrived_event: None,
			new_animation: Some(EnemyBundle::idle_animation()),
			next_state:    Some(TurnState::EnemiesWaiting),
		});

		let enemy = level_cache
			.enemies
			.remove(enemy_pos.as_ref())
			.expect("found no enemy at the moved position");
		assert_eq!(enemy, enemy_entity, "wrong enemy found in level cache");
		level_cache.enemies.insert(*destination, enemy);
		*enemy_pos = *destination;

		*turn_state = TurnState::EnemiesRunning;
		return;
	}

	game_state
		.enemies
		.iter_mut()
		.for_each(|(_, ready)| *ready = true);
	*turn_state = TurnState::Waiting;
}

fn main() {
	App::new()
		.add_plugins((
			DefaultPlugins.set(ImagePlugin::default_nearest()),
			PanCamPlugin,
			TilemapPlugin,
			LdtkPlugin,
			TweeningPlugin,
			EguiPlugin,
			WorldInspectorPlugin::new()
				.run_if(|debug: Res<'_, Debug>| matches!(debug.deref(), Debug::Active { .. })),
		))
		// Fixes issues with tile bleeding.
		// See <https://github.com/bevyengine/bevy/issues/1949>.
		.insert_resource(Msaa::Off)
		.add_systems(Startup, startup)
		.add_systems(First, level_spawn)
		.add_systems(PreUpdate, debug)
		.add_systems(
			Update,
			(
				player_movement,
				move_enemies.run_if(|debug: Res<'_, TurnState>| {
					matches!(debug.deref(), TurnState::EnemiesWaiting)
				}),
				select_ability,
				door_interactions,
				spawn_healthbar,
				cast_ability.run_if(|debug: Res<'_, TurnState>| {
					matches!(debug.deref(), TurnState::Waiting)
				}),
				(handle_ability_event, update_healthbar, death).chain(),
				(update_cursor_pos, update_target_marker).chain(),
			)
				.run_if(|debug: Res<'_, Debug>| matches!(debug.deref(), Debug::Inactive)),
		)
		.add_systems(
			PostUpdate,
			(
				tick_cooldowns,
				(turn_ui, item_ui, ability_ui).before(EguiSet::ProcessOutput),
				animation::animate,
				(
					animation::finish_animation,
					animation::animation_arrived_tile,
					translate_grid_coords_entities,
					camera_update,
				)
					.chain(),
			),
		)
		.insert_resource(Debug::Inactive)
		.insert_resource(LdtkSettings {
			level_background: LevelBackground::Nonexistent,
			..default()
		})
		.insert_resource(LevelSelection::Iid(LevelIid::new(
			"32dd4990-25d0-11ef-be0e-2bd40eab6b07",
		)))
		.init_resource::<GameState>()
		.init_resource::<LevelCache>()
		.init_resource::<CursorPos>()
		.init_resource::<PlayerEntityDestination>()
		.init_resource::<TurnState>()
		.add_event::<AnimationArrived>()
		.add_event::<DeathEvent>()
		.add_event::<AbilityEvent>()
		.register_ldtk_entity::<PlayerBundle>("Player")
		.register_ldtk_entity::<KeyBundle>("Key")
		.register_ldtk_entity::<EnemyBundle>("Skeleton")
		.register_ldtk_entity::<DoorBundle>("Door")
		.register_ldtk_int_cell::<WallBundle>(1)
		.register_type::<Door>()
		.register_type::<Drops>()
		.register_type::<Health>()
		.register_type::<LevelCache>()
		.register_type::<GameState>()
		.register_type::<TurnState>()
		.register_type::<Spellbook>()
		.run();
}
