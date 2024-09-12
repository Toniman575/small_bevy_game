//! TODO:
//! - Abilities
//!   - Implement tooltip.
//!   - Make sure its easy to add new abilities.
//!   - Add resources for abilities.
//!   - Make enemies use new abilities. Probably with a simple state machine.
//! - Debuffs
//!   - Implement bleeding debuff on enemies, think about presentation.
//!   - Make sure its easy to integrate with abilities and add new ones.
//! - Bonus
//!   - Add displacing abilities.
//!   - Delete `LevelCache`.
//!   - Let players speed up turns (turn off animations, speed them up etc.).
//!   - Better horde behavior.
//!   - When player goes out of LoS enemies go to last known position and "wait" before returning to
//!     their spawn point.
//!   - Replace animation code with <https://github.com/merwaaan/bevy_spritesheet_animation>.
//!   - Refine animations and ability presentation.
//!   - Replace egui with <https://github.com/UmbraLuminosa/sickle_ui> maybe?
//!   - Show all levels in debug mode.
//!
//! Bugs:
//! - Sometimes levels are not despawned correctly, leading to false walls and doors being cached.
//! - Fully loaded levels, before cleanup, can sometimes be seen for a single frame.

#![allow(
	clippy::allow_attributes_without_reason,
	clippy::multiple_crate_versions,
	clippy::unimplemented,
	clippy::wildcard_imports
)]

mod animation;
mod fow;
mod gameplay;
mod util;

use std::mem;
use std::ops::{Deref, DerefMut};

use bevy::color::palettes::basic::*;
use bevy::input::ButtonState;
use bevy::input::keyboard::KeyboardInput;
use bevy::prelude::{AssetServer, *};
use bevy::utils::{Entry, HashMap, HashSet};
use bevy::window::PrimaryWindow;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::prelude::*;
use bevy_egui::egui::{Margin, TextWrapMode, TextureId, TopBottomPanel};
use bevy_egui::{EguiPlugin, EguiSet, egui};
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pancam::{DirectionKeys, PanCam, PanCamPlugin};
use bevy_tweening::{Animator, TweeningPlugin};
use egui::{
	Align, Align2, Area, Color32, FontId, Frame, Id, Label, Layout, Pos2, RichText, Sense,
	SidePanel, Stroke, Widget,
};
use line_drawing::WalkGrid;

use self::fow::{ApplyFoW, generate_fov, update_memory};
use self::gameplay::{
	Abilities, AbilityEffect, AbilityEvent, ActiveAbility, BaseSkeletonBundle,
	CurrentStatusEffects, DeathEvent, EffectType, Enemy, Health, MageSkeletonBundle, Player,
	PlayerBundle, Spellbook, StatusEffect, Vision, cast_ability, death, door_interactions,
	handle_ability_event, move_enemies, player_movement, select_ability, spawn_healthbar,
	tick_cooldowns, tick_status_effects, update_healthbar,
};

/// The size of the Grid in pixels.
const GRID_SIZE: i32 = 16;

/// State for controlling the current turn state of the game.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Resource, Reflect)]
#[reflect(Resource)]
enum TurnState {
	/// Waiting for player input.
	#[default]
	PlayerWaiting,
	/// Player animation running.
	PlayerBusy(PlayerBusy),
	/// Waiting for enemy behavior to be scheduled.
	EnemiesWaiting,
	/// Enemy animation running.
	EnemiesBusy,
}

/// State of player when [`TurnState::Busy`].
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Resource, Reflect)]
enum PlayerBusy {
	/// Player is attacking.
	Casting {
		/// If the camera updates with the current casting animation.
		// FP: https://github.com/rust-lang/rust-clippy/issues/13058
		camera: bool,
	},
	/// Player is moving.
	Moving,
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

/// Marker component for the targeting marker (highlights the tile u are hovering with your mouse).
#[derive(Component, PartialEq, Eq)]
struct TargetingMarker {
	/// Entity currently targeted.
	entity: Option<Entity>,
	/// If target is valid for the current ability.
	valid:  bool,
}

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
// FP: https://github.com/rust-lang/rust-clippy/issues/12996
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
		doors: &HashMap<EntityIid, bool>,
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
			if *doors.get(iid).unwrap() {
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
#[expect(clippy::needless_pass_by_value)]
fn startup(
	mut commands: Commands<'_, '_>,
	asset_server: Res<'_, AssetServer>,
	mut contexts: EguiContexts<'_, '_>,
) {
	let mut camera = Camera2dBundle::default();
	camera.projection.scale = 0.5;
	commands.spawn(camera).insert(PanCam {
		move_keys: DirectionKeys::NONE,
		grab_buttons: Vec::new(),
		zoom_to_cursor: false,
		min_scale: 0.1,
		max_scale: 1.,
		..PanCam::default()
	});

	commands.spawn(Turn(0));

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
		TargetingMarker {
			entity: None,
			valid:  false,
		},
		GridCoords::default(),
		Name::new("Tile Target Marker"),
	));

	let props = asset_server.load("Environment/Dungeon Prison/Assets/Props.png");
	let props_id = contexts.add_image(props.clone_weak());
	let arrow = asset_server.load::<Image>("arrow.webp");
	let slash_animation = asset_server.load::<Image>("slash.webp");
	let slash_animation_atlas = asset_server.add(TextureAtlasLayout::from_grid(
		UVec2::splat(64),
		7,
		1,
		None,
		None,
	));
	let auto_attack_icon = asset_server.load("auto_attack.webp");
	let auto_attack_icon_id = contexts.add_image(auto_attack_icon);
	let arrow_icon = asset_server.load("arrow_icon.webp");
	let arrow_icon_id = contexts.add_image(arrow_icon);

	let textures = Textures {
		props,
		props_id,
		arrow,
		slash_animation,
		slash_animation_atlas,
		auto_attack_icon_id,
		arrow_icon_id,
	};

	commands.insert_resource(Abilities::new(&textures));
	commands.insert_resource(textures);
}

/// Stores textures we need throughout the game.
#[derive(Resource)]
struct Textures {
	/// Dungeon tileset props.
	props:                 Handle<Image>,
	/// EGUI texture handle to the dungeon tileset props.
	props_id:              TextureId,
	/// Arrow texture.
	arrow:                 Handle<Image>,
	/// Slash animation attack texture.
	slash_animation:       Handle<Image>,
	/// Slash animation texture atlas layout.
	slash_animation_atlas: Handle<TextureAtlasLayout>,
	/// EGUI texture handle to the auto-attack icon.
	auto_attack_icon_id:   TextureId,
	/// EGUI texture handle to the arrow icon.
	arrow_icon_id:         TextureId,
}

/// Icons stored as textures.
#[derive(Clone, Copy, Reflect)]
enum TextureIcon {
	/// Auto-attack ability icon.
	AutoAttack,
	/// Arrow ability icon.
	Arrow,
}

/// Initialize states after level is spawned.
#[expect(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
fn level_spawn(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut level_events: EventReader<'_, '_, LevelEvent>,
	walls: Query<'_, '_, &GridCoords, (With<Wall>, Without<Enemy>, Without<Player>)>,
	mut enemies: Query<
		'_,
		'_,
		(&mut GridCoords, Entity, &EntityIid, &Drops),
		(With<Enemy>, Without<Player>),
	>,
	keys: Query<
		'_,
		'_,
		(&GridCoords, Entity, &EntityIid),
		(With<Key>, Without<Enemy>, Without<Player>),
	>,
	mut doors: Query<
		'_,
		'_,
		(Entity, &GridCoords, &EntityIid, &Door),
		(Without<Player>, Without<Enemy>),
	>,
	mut player_entity_destination: ResMut<'_, PlayerEntityDestination>,
	ldtk_entities: Query<'_, '_, (Entity, &EntityIid), Without<Player>>,
	door_grid_coords: Query<'_, '_, &GridCoords, (Without<Enemy>, Without<Player>)>,
	mut player: Query<'_, '_, &mut GridCoords, (Without<Enemy>, With<Player>)>,
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

	for (grid_coords, entity, iid, drops) in &enemies {
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

	for (entity, grid_coords, iid, door) in &mut doors {
		doors_map.insert(*grid_coords, (entity, iid.clone(), door.clone()));

		let open = match state.doors.entry(iid.clone()) {
			Entry::Occupied(value) => *value.get(),
			Entry::Vacant(entry) => *entry.insert(false),
		};

		if open {
			commands.trigger_targets(DoorOpen, entity);
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

		let destination_grid_coords = door_grid_coords
			.get(destination_entity)
			.expect("destination entity should exist");
		let mut player_grid_coords = player.single_mut();
		*player_grid_coords = *destination_grid_coords;

		player_entity_destination.0 = None;
	}

	// Clear enemy order.
	state.enemies.clear();

	// Set enemies to ready and add them to the state.
	for (mut grid_coords, entity, ..) in &mut enemies {
		grid_coords.set_changed();
		state.enemies.push((entity, true));
	}
}

/// Every time enemies are added we need to customize their [`TextureAtlasLayout`].
#[expect(clippy::needless_pass_by_value)]
fn fix_sprite_layout(
	mut texture_atlas_layouts: ResMut<'_, Assets<TextureAtlasLayout>>,
	enemies: Query<'_, '_, (&Enemy, &TextureAtlas), Added<Enemy>>,
) {
	for (enemy, atlas) in enemies.iter() {
		let layout = texture_atlas_layouts
			.get_mut(&atlas.layout)
			.expect("texture atlas layout not found for enemy");

		match enemy {
			Enemy::BaseSkeleton => {
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
			Enemy::MageSkeleton => {
				for texture in layout
					.textures
					.get_mut(6..=11)
					.expect("unexpected enemy skeleton sprite sheet size")
					.iter_mut()
				{
					texture.max.y = 65;
				}

				for texture in layout
					.textures
					.get_mut(12..=17)
					.expect("unexpected enemy skeleton sprite sheet size")
					.iter_mut()
				{
					texture.max.y = 121;
				}

				layout.textures.truncate(18);
			}
		}
	}
}

/// [`Trigger`] for opening a door.
#[derive(Event)]
struct DoorOpen;

/// Opens a door when triggered.
#[expect(clippy::needless_pass_by_value)]
fn door_trigger(
	trigger: Trigger<'_, DoorOpen>,
	mut doors: Query<'_, '_, &mut TextureAtlas, With<Door>>,
) {
	let mut atlas = doors.get_mut(trigger.entity()).unwrap();
	atlas.index = 133;
}

/// Game state.
#[derive(Default, Reflect, Resource)]
#[reflect(Resource)]
struct GameState {
	/// State of each door.
	doors:         HashMap<EntityIid, bool>,
	/// State of each key.
	keys:          HashMap<ItemSource, bool>,
	/// Current enemy order.
	enemies:       Vec<(Entity, bool)>,
	/// Amount of keys the player possesses.
	player_keys:   u8,
	/// Tiles already seen by the player.
	visited_tiles: HashMap<LevelIid, HashSet<GridCoords>>,
}

#[derive(Component, Default, Reflect, Resource, PartialEq, Eq)]
/// The current turn.
struct Turn(u64);

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
fn debug(
	mut commands: Commands<'_, '_>,
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
					};
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

			commands.trigger(ApplyFoW);
		}
	}
}

/// We need to keep the cursor position updated based on any `CursorMoved` events.
#[expect(clippy::needless_pass_by_value)]
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

/// Updates the `TargetingMarker` with updated `CursorPos`.
#[expect(clippy::needless_pass_by_value, clippy::too_many_lines)]
fn update_target_marker(
	cursor_pos: Res<'_, CursorPos>,
	player: Query<'_, '_, (Entity, &GridCoords, &Health, &ActiveAbility, &Vision), With<Player>>,
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
	abilites: Res<'_, Abilities>,
) {
	let (mut marker, mut visibility, mut marker_grid_coords, mut sprite) =
		target_marker.single_mut();
	let Ok((player_entity, player_grid_coords, health, active_ability, vision)) =
		player.get_single()
	else {
		return;
	};

	if level_cache.outside_boundary(cursor_pos.tile_pos)
		|| !vision.tiles.contains(&cursor_pos.tile_pos)
	{
		*visibility = Visibility::Hidden;
		marker.set_if_neq(TargetingMarker {
			entity: None,
			valid:  false,
		});
	} else {
		visibility.set_if_neq(Visibility::Inherited);
		marker_grid_coords.set_if_neq(cursor_pos.tile_pos);

		let ability = abilites
			.0
			.get(&active_ability.0)
			.expect("Player has to have an active ability.");
		let enemy = level_cache.enemies.get(marker_grid_coords.deref());

		match &ability.effect {
			AbilityEffect::Damage(_) => {
				if let Some(entity) = enemy {
					let valid =
						if ability.in_euclidean_range(*player_grid_coords, *marker_grid_coords) {
							sprite.color = RED.into();
							true
						} else {
							sprite.color = YELLOW.into();
							false
						};

					marker.set_if_neq(TargetingMarker {
						entity: Some(*entity),
						valid,
					});
				} else {
					sprite.color = Color::WHITE;
					marker.set_if_neq(TargetingMarker {
						entity: None,
						valid:  false,
					});
				}
			}
			AbilityEffect::Healing(_) => {
				let valid =
					if *player_grid_coords == *marker_grid_coords && health.current != health.max {
						sprite.color = GREEN.into();
						true
					} else {
						sprite.color = RED.into();
						false
					};

				marker.set_if_neq(TargetingMarker {
					entity: Some(player_entity),
					valid,
				});
			}
			AbilityEffect::Teleport => {
				let valid =
					if enemy.is_none() && !level_cache.walls.contains(marker_grid_coords.deref()) {
						if ability.in_euclidean_range(*player_grid_coords, *marker_grid_coords) {
							sprite.color = GREEN.into();
							true
						} else {
							sprite.color = YELLOW.into();
							false
						}
					} else {
						sprite.color = RED.into();
						false
					};

				marker.set_if_neq(TargetingMarker {
					entity: None,
					valid,
				});
			}
			AbilityEffect::StatusEffect(effect) => match effect.effect_type {
				EffectType::DefensiveBuff | EffectType::AttackBuff => {
					let valid = if *player_grid_coords == *marker_grid_coords {
						sprite.color = GREEN.into();
						true
					} else {
						sprite.color = RED.into();
						false
					};

					marker.set_if_neq(TargetingMarker {
						entity: Some(player_entity),
						valid,
					});
				}
				EffectType::DefensiveDebuff | EffectType::AttackDebuff | EffectType::Dot => {
					if let Some(entity) = enemy {
						let valid = if ability
							.in_euclidean_range(*player_grid_coords, *marker_grid_coords)
						{
							sprite.color = RED.into();
							true
						} else {
							sprite.color = YELLOW.into();
							false
						};

						marker.set_if_neq(TargetingMarker {
							entity: Some(*entity),
							valid,
						});
					} else {
						sprite.color = Color::WHITE;
						marker.set_if_neq(TargetingMarker {
							entity: None,
							valid:  false,
						});
					}
				}
			},
			AbilityEffect::Charge(_) => {
				if let Some(entity) = enemy {
					let path = WalkGrid::new(
						IVec2::from(*player_grid_coords).into(),
						IVec2::from(*marker_grid_coords).into(),
					)
					.map(|pos| GridCoords::from(IVec2::from(pos)));

					let mut valid = true;

					for pos in path {
						// reached last one
						if pos == *marker_grid_coords {
							continue;
						}

						if level_cache.walls.contains(&pos)
							|| level_cache.enemies.contains_key(&pos)
						{
							valid = false;
							break;
						}
					}

					if valid {
						sprite.color = RED.into();
					} else {
						sprite.color = YELLOW.into();
					};

					marker.set_if_neq(TargetingMarker {
						entity: Some(*entity),
						valid,
					});
				} else {
					sprite.color = Color::WHITE;
					marker.set_if_neq(TargetingMarker {
						entity: None,
						valid:  false,
					});
				}
			}
		}
	}
}

/// Updates Camera with player movement.
#[expect(clippy::needless_pass_by_value)]
fn camera_update(
	turn_state: Res<'_, TurnState>,
	player: Query<'_, '_, &Transform, (With<Player>, Changed<Transform>)>,
	mut cam: Query<'_, '_, &mut Transform, (With<Camera>, Without<Player>)>,
) {
	if let TurnState::PlayerBusy(PlayerBusy::Casting { camera: false }) = turn_state.deref() {
		return;
	}

	if let Ok(player_transform) = player.get_single() {
		let mut cam = cam.single_mut();

		cam.translation.x = player_transform.translation.x;
		cam.translation.y = player_transform.translation.y;
	}
}

/// Updates [`Transform`] for [`Entity`]s with changed [`GridCoords`].
#[expect(clippy::type_complexity)]
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
#[expect(clippy::needless_pass_by_value)]
fn item_ui(
	textures: Res<'_, Textures>,
	state: Res<'_, GameState>,
	mut contexts: EguiContexts<'_, '_>,
) {
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
							let (response, painter) =
								ui.allocate_painter(egui::Vec2::new(64., 64.), Sense {
									click:     false,
									drag:      false,
									focusable: false,
								});

							painter.image(
								textures.props_id,
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
#[expect(clippy::needless_pass_by_value)]
fn turn_ui(turn_q: Query<'_, '_, &Turn>, mut contexts: EguiContexts<'_, '_>) {
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
						RichText::new(format!("Turn: {}", turn_q.single().0))
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
#[expect(clippy::needless_pass_by_value, clippy::too_many_lines)]
fn ability_ui(
	textures: Res<'_, Textures>,
	mut player: Query<'_, '_, (&Spellbook, &mut ActiveAbility), With<Player>>,
	abilites: Res<'_, Abilities>,
	mut contexts: EguiContexts<'_, '_>,
	turn_q: Query<'_, '_, &Turn>,
) {
	let Ok((spellbook, mut active_ability)) = player.get_single_mut() else {
		return;
	};

	let Some(context) = contexts.try_ctx_mut() else {
		return;
	};

	TopBottomPanel::bottom("abilities")
		.show_separator_line(false)
		.frame(Frame::none())
		.show(context, |ui| {
			ui.horizontal(|ui| {
				#[expect(clippy::as_conversions, clippy::cast_precision_loss)]
				let range = spellbook.abilities.len() as f32 * 68.;
				let padding = (context.screen_rect().width() - range) / 2.;
				ui.add_space(padding);

				ui.spacing_mut().item_spacing = egui::Vec2::ZERO;

				let mut sorted_spellbook = spellbook.abilities.iter().collect::<Vec<_>>();

				sorted_spellbook.sort_by(|(id1, _), (id2, _)| (id1).partial_cmp(id2).unwrap());

				for ((id, spellbook_ability), button) in sorted_spellbook.iter().zip(1..) {
					let ability = abilites.0.get(*id).expect("found non-existing ability");

					let ability_color = if **id == active_ability.0 {
						Color32::GREEN
					} else {
						Color32::WHITE
					};

					#[expect(clippy::option_if_let_else)]
					let (text, text_color) = if let Some(cooldown_left) =
						spellbook_ability.cooldown_left(ability, turn_q.single().0)
					{
						(&format!("{} ({cooldown_left})", ability.name), Color32::RED)
					} else {
						(&ability.name, Color32::WHITE)
					};

					let response = Frame::default()
						.fill(Color32::BLACK)
						.stroke(Stroke::new(2., ability_color))
						.rounding(5.)
						.outer_margin(Margin::symmetric(2., 2.))
						.show(ui, |ui| {
							let (response, painter) =
								ui.allocate_painter(egui::Vec2::new(64., 64.), Sense {
									click:     false,
									drag:      false,
									focusable: false,
								});

							if let Some(icon) = ability.icon {
								painter.image(
									match icon {
										TextureIcon::AutoAttack => textures.auto_attack_icon_id,
										TextureIcon::Arrow => textures.arrow_icon_id,
									},
									response.rect,
									egui::Rect::from_min_max(Pos2::ZERO, Pos2::new(1., 1.)),
									Color32::WHITE,
								);
							} else {
								// Text shadow.
								painter.text(
									(response.rect.right_top() - Pos2::new(4., -4.)).to_pos2(),
									Align2::RIGHT_TOP,
									text,
									FontId {
										size: 12.,
										..FontId::default()
									},
									Color32::BLACK,
								);

								painter.text(
									(response.rect.right_top() - Pos2::new(2., -2.)).to_pos2(),
									Align2::RIGHT_TOP,
									text,
									FontId {
										size: 12.,
										..FontId::default()
									},
									text_color,
								);
							}

							// Text shadow.
							painter.text(
								response.rect.left_bottom() + egui::Vec2::new(4., -4.),
								Align2::LEFT_BOTTOM,
								button,
								FontId {
									size: 12.,
									..FontId::default()
								},
								Color32::BLACK,
							);

							painter.text(
								response.rect.left_bottom() + egui::Vec2::new(2., -2.),
								Align2::LEFT_BOTTOM,
								button,
								FontId {
									size: 12.,
									..FontId::default()
								},
								Color32::WHITE,
							);

							response
						})
						.response
						.interact(Sense::click());

					if response.clicked() {
						active_ability.0 = **id;
					}
				}
			})
		});
}

/// Shows the currently active [`StatusEffects`] and how long they will last on the player.
#[expect(clippy::needless_pass_by_value)]
fn player_effect_ui(
	effects: Query<'_, '_, &CurrentStatusEffects, With<Player>>,
	mut contexts: EguiContexts<'_, '_>,
	turn_q: Query<'_, '_, &Turn>,
) {
	let Some(context) = contexts.try_ctx_mut() else {
		return;
	};

	let Ok(effects) = effects.get_single() else {
		return;
	};

	SidePanel::left("effects")
		.resizable(false)
		.show_separator_line(false)
		.frame(Frame::none())
		.show(context, |ui| {
			ui.with_layout(Layout::bottom_up(Align::Min), |ui| {
				for effect in &effects.0 {
					Frame::default()
						.fill(Color32::BLACK)
						.stroke(Stroke::new(2., Color32::WHITE))
						.rounding(5.)
						.show(ui, |ui| {
							let (response, painter) =
								ui.allocate_painter(egui::Vec2::new(64., 64.), Sense {
									click:     false,
									drag:      false,
									focusable: false,
								});

							/*painter.image(
								textures.props_id,
								response.rect,
								egui::Rect::from([
									Pos2::new(1. / 400. * 32., 1. / 400. * 64.),
									Pos2::new(1. / 400. * 48., 1. / 400. * 80.),
								]),
								Color32::WHITE,
							);*/

							// Name.

							// Text shadow.
							painter.text(
								(response.rect.right_top() - Pos2::new(4., -4.)).to_pos2(),
								Align2::RIGHT_TOP,
								&effect.name,
								FontId {
									size: 12.,
									..FontId::default()
								},
								Color32::BLACK,
							);

							painter.text(
								(response.rect.right_top() - Pos2::new(2., -2.)).to_pos2(),
								Align2::RIGHT_TOP,
								&effect.name,
								FontId {
									size: 12.,
									..FontId::default()
								},
								Color32::WHITE,
							);

							// Duration

							let turn = turn_q.single().0;

							// Text shadow.
							painter.text(
								(response.rect.center() - Pos2::new(4., -4.)).to_pos2(),
								Align2::CENTER_CENTER,
								effect.turns_left(turn).unwrap(),
								FontId {
									size: 48.,
									..FontId::default()
								},
								Color32::BLACK,
							);

							painter.text(
								(response.rect.center() - Pos2::new(2., -2.)).to_pos2(),
								Align2::CENTER_CENTER,
								effect.turns_left(turn).unwrap(),
								FontId {
									size: 48.,
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
		.add_systems(PreUpdate, (fix_sprite_layout, debug, generate_fov))
		.add_systems(
			Update,
			(
				player_movement,
				move_enemies.run_if(|debug: Res<'_, TurnState>| {
					matches!(debug.deref(), TurnState::EnemiesWaiting)
				}),
				select_ability,
				spawn_healthbar,
				update_memory,
				(door_interactions, cast_ability).run_if(|debug: Res<'_, TurnState>| {
					matches!(debug.deref(), TurnState::PlayerWaiting)
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
				tick_status_effects,
				(((turn_ui, item_ui, ability_ui), player_effect_ui).chain())
					.before(EguiSet::ProcessOutput),
				animation::animate,
				(
					animation::finish_animation,
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
		.add_event::<DeathEvent>()
		.add_event::<AbilityEvent>()
		.register_ldtk_entity::<PlayerBundle>("Player")
		.register_ldtk_entity::<KeyBundle>("Key")
		.register_ldtk_entity::<BaseSkeletonBundle>("BaseSkeleton")
		.register_ldtk_entity::<MageSkeletonBundle>("MageSkeleton")
		.register_ldtk_entity::<DoorBundle>("Door")
		.register_ldtk_int_cell::<WallBundle>(1)
		.register_type::<Door>()
		.register_type::<Drops>()
		.register_type::<StatusEffect>()
		.register_type::<CurrentStatusEffects>()
		.register_type::<Health>()
		.register_type::<LevelCache>()
		.register_type::<GameState>()
		.register_type::<TurnState>()
		.register_type::<Spellbook>()
		.register_type::<Turn>()
		.observe(door_trigger)
		.observe(fow::apply_fow)
		.observe(animation::arrived_at_tile)
		.run();
}
