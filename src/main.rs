//! TODO:
//! - Refactor Code
//!   - Split into multiple files.
//!   - Replace animation code with <https://github.com/merwaaan/bevy_spritesheet_animation>.
//!   - Replace egui with <https://github.com/UmbraLuminosa/sickle_ui> maybe?
//! - Improve combat
//! - Design turn system
//!   - Assign order to each enemy.
//!   - Run system after player has made their action with each enemy doing theirs.
//!   - Enemy walks towards player in their action with A*.
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

#![allow(clippy::multiple_crate_versions, clippy::unimplemented)]

use std::collections::BTreeMap;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::time::Duration;

#[allow(clippy::wildcard_imports)]
use bevy::color::palettes::basic::*;
use bevy::input::keyboard::KeyboardInput;
use bevy::input::ButtonState;
use bevy::prelude::{AssetServer, *};
use bevy::sprite::{Anchor, MaterialMesh2dBundle};
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
use bevy_tweening::{
	Animator, EaseFunction, EaseMethod, Sequence, Tween, TweenCompleted, Tweenable, TweeningPlugin,
};
use egui::emath::Numeric;
use egui::{
	Align, Align2, Area, Color32, FontId, Frame, Id, Label, Layout, Pos2, RichText, Sense,
	SidePanel, Stroke, Widget,
};

/// The size of the Grid in pixels.
const GRID_SIZE: i32 = 16;

/// Component for tracking health in entities.
#[derive(Clone, Component, Debug, Default, Reflect, PartialEq, Eq, Copy)]
struct Health {
	/// How much health the entity currently has.
	current: u16,
	/// How much health the entity can have.
	max:     u16,
}

#[allow(clippy::fallible_impl_from)]
impl From<&EntityInstance> for Health {
	fn from(entity_instance: &EntityInstance) -> Self {
		let reference = *entity_instance
			.get_int_field("Health")
			.expect("expected entity to have non-nullable `Health` int field");

		let health_value = u16::try_from(reference).expect("invalid health value");

		Self {
			current: health_value,
			max:     health_value,
		}
	}
}

/// Healthbar marker component.
#[derive(Component)]
struct HealthBar;

/// When an entity dies, it sends this event.
#[derive(Event)]
struct DeathEvent(Entity);

/// Event that fires when a player uses an ability. If not sent through a mouseclick we need to
/// specifify which entity is the target.
#[derive(Event)]
struct AbilityEvent(Option<Entity>);

/// An ability an entity can perform.
#[derive(Reflect, Default)]
struct Ability {
	/// Identifier for the ability.
	id:        u64,
	/// Name of the ability.
	name:      String,
	/// The range in manhatten distance of the ability.
	range:     u8,
	/// The power of the ability.
	power:     u16,
	/// The Cooldown of the ability.
	cooldown:  Option<u8>,
	/// When this ability was last cast.
	last_cast: Option<u64>,
}

impl Ability {
	/// Creates an ability.
	const fn new(id: u64, name: String, range: u8, power: u16, cooldown: Option<u8>) -> Self {
		Self {
			id,
			name,
			range,
			power,
			cooldown,
			last_cast: None,
		}
	}

	/// Checks if ability is in manhatten range.
	fn in_manhatten_range(&self, origin: GridCoords, target: GridCoords) -> bool {
		let distance = (target.x - origin.x).abs() + (target.y - origin.y).abs();

		distance <= self.range.into()
	}

	/// Checks if ability is in euclidian range.
	fn in_euclidean_range(&self, origin: GridCoords, target: GridCoords) -> bool {
		let distance = ((target.x - origin.x).pow(2) + (target.y - origin.y).pow(2))
			.to_f64()
			.sqrt();

		distance.floor() <= self.range.into()
	}

	/// Calculates how much cooldown is left for this ability.
	fn cooldown_left(&self, current_turn: u64) -> Option<u64> {
		self.last_cast.and_then(|last_cast| {
			let cooldown_left = (last_cast
				+ u64::from(
					self.cooldown
						.expect("when ability has last cast it also needs to have a cooldown"),
				))
			.saturating_sub(current_turn);

			(cooldown_left > 0).then_some(cooldown_left)
		})
	}

	//fn get_animation(&self) -> impl Tweenable<TransformPositionLens> {}
}

/// The collection of abilities an entity can perform.
#[derive(Reflect, Component)]
struct Spellbook(BTreeMap<u64, Ability>);

impl Default for Spellbook {
	fn default() -> Self {
		Self(BTreeMap::from([
			(0, Ability::new(0, String::from("Autoattack"), 1, 5, None)),
			(1, Ability::new(1, String::from("Ranged"), 5, 2, None)),
			(2, Ability::new(2, String::from("Hardcore"), 1, 10, Some(3))),
		]))
	}
}

/// The currently active ability of an entity.
#[derive(Reflect, Component, Default)]
struct ActiveAbility(u64);

/// Player marker component.
#[derive(Default, Component)]
struct Player;

/// Player bundle.
#[derive(Bundle, LdtkEntity)]
struct PlayerBundle {
	/// Player marker component.
	player:              Player,
	/// Health of the player.
	#[from_entity_instance]
	health:              Health,
	/// Abilities the player can perform.
	abilities:           Spellbook,
	/// The currently active ability.
	active_ability:      ActiveAbility,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// Player grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
	/// Animation.
	animation:           Animation,
	/// Makes the player "Worldly".
	#[worldly]
	worldly:             Worldly,
}

impl Default for PlayerBundle {
	fn default() -> Self {
		Self {
			player:              Player,
			health:              Health::default(),
			abilities:           Spellbook::default(),
			active_ability:      ActiveAbility::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Self::idle_animation(),
			worldly:             Worldly::default(),
		}
	}
}

impl PlayerBundle {
	/// Return idle [`Animation`].
	fn idle_animation() -> Animation {
		Animation {
			timer:     Timer::from_seconds(0.25, TimerMode::Repeating),
			first:     0,
			last:      3,
			repeating: true,
			anchor:    None,
		}
	}

	/// Return walking [`Animation`].
	fn walking_animation() -> Animation {
		Animation {
			timer:     Timer::from_seconds(0.1, TimerMode::Repeating),
			first:     9,
			last:      14,
			repeating: true,
			anchor:    None,
		}
	}
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

/// Enemy marker component.
#[derive(Default, Component)]
struct Enemy;

/// Enemy bundle.
#[derive(Bundle, LdtkEntity)]
struct EnemyBundle {
	/// Player marker component.
	enemy:               Enemy,
	/// Health of the enemy.
	#[from_entity_instance]
	health:              Health,
	#[from_entity_instance]
	/// A list of items this enemy drops on death.
	drops:               Drops,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// Player grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
	/// Animation.
	animation:           Animation,
}

impl Default for EnemyBundle {
	fn default() -> Self {
		Self {
			enemy:               Enemy,
			health:              Health::default(),
			drops:               Drops::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Self::idle_animation(),
		}
	}
}

impl EnemyBundle {
	/// Return idle [`Animation`].
	fn idle_animation() -> Animation {
		Animation {
			timer:     Timer::from_seconds(0.25, TimerMode::Repeating),
			first:     0,
			last:      3,
			repeating: true,
			anchor:    None,
		}
	}

	/// Return death [`Animation`].
	fn death_animation() -> Animation {
		Animation {
			timer:     Timer::from_seconds(0.125, TimerMode::Repeating),
			first:     16,
			last:      23,
			repeating: false,
			anchor:    Some(Anchor::Custom(Vec2::new(
				0.,
				-(1. / 48. * ((48. - 32.) / 2.)),
			))),
		}
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
		state: &State,
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
			if *state.doors.get(iid).unwrap() {
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
	mut state: ResMut<'_, State>,
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
	mut player: Query<'_, '_, (Entity, &mut GridCoords), With<Player>>,
	ldtk_project_entities: Query<'_, '_, &Handle<LdtkProject>>,
	ldtk_project_assets: Res<'_, Assets<LdtkProject>>,
	mut complete_animation: EventWriter<'_, TweenCompleted>,
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
		let (player_entity, mut player_grid_coords) = player.single_mut();
		*player_grid_coords = *destination_grid_coords;

		// We need to remove the Animation in case there was still one running during a level
		// change.
		complete_animation.send(TweenCompleted {
			entity:    player_entity,
			user_data: 0,
		});

		player_entity_destination.0 = None;
	}

	// Our Spritesheets have sprites with different sized tiles in it so we fix them.
	if let Some((_, _, _, enemy, _)) = enemies.iter().next() {
		if let Some(layout) = texture_atlas_layouts.get_mut(&enemy.layout) {
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
}

/// Game state.
#[derive(Default, Reflect, Resource)]
#[reflect(Resource)]
struct State {
	/// The current turn.
	turn:        u64,
	/// State of each door.
	doors:       HashMap<EntityIid, bool>,
	/// State of each key.
	keys:        HashMap<ItemSource, bool>,
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

/// Handles when a player wants to cast an ability.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
fn handle_ability_event(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, State>,
	mut player: Query<
		'_,
		'_,
		(
			Entity,
			&Transform,
			&ActiveAbility,
			&GridCoords,
			&mut Spellbook,
		),
		(With<Player>, Without<Enemy>),
	>,
	target_marker: Query<'_, '_, &TargetingMarker>,
	mut enemies: Query<
		'_,
		'_,
		(&Transform, &GridCoords, &mut Health),
		(With<Enemy>, Without<Player>),
	>,
	mut abilities: EventReader<'_, '_, AbilityEvent>,
) {
	let target_marker = target_marker.single();

	for ability in abilities.read() {
		let Some(entity) = ability.0.or(target_marker.0) else {
			continue;
		};

		if let Ok((enemy_transform, enemy_grid_coord, mut health)) = enemies.get_mut(entity) {
			if let Ok((
				player_entity,
				player_transform,
				active_ability,
				player_grid_coord,
				mut spellbook,
			)) = player.get_single_mut()
			{
				let attack = spellbook
					.0
					.get_mut(&active_ability.0)
					.expect("Player has to have an active ability.");

				if attack.last_cast.is_none()
					&& attack.in_euclidean_range(*player_grid_coord, *enemy_grid_coord)
				{
					state.turn += 1;
					health.current = health.current.saturating_sub(attack.power);
					let target = player_transform.translation
						+ (enemy_transform.translation.xy() - player_transform.translation.xy())
							.normalize()
							.extend(player_transform.translation.z);

					commands.entity(player_entity).insert(Animator::new(
						Tween::new(
							EaseMethod::Linear,
							Duration::from_secs_f64(0.2),
							TransformPositionLens {
								start: player_transform.translation,
								end:   target,
							},
						)
						.then(Tween::new(
							EaseMethod::Linear,
							Duration::from_secs_f64(0.2),
							TransformPositionLens {
								start: target,
								end:   player_transform.translation,
							},
						)),
					));

					if attack.cooldown.is_some() {
						attack.last_cast = Some(state.turn);
					}
				}
			}
		}
	}
}

/// Lets player cast an ability with a mouseclick.
#[allow(clippy::needless_pass_by_value)]
fn cast_ability(
	buttons: Res<'_, ButtonInput<MouseButton>>,
	mut abilities: EventWriter<'_, AbilityEvent>,
) {
	if buttons.just_pressed(MouseButton::Left) {
		abilities.send(AbilityEvent(None));
	}
}

/// When an Entity with a healthbar is spawned we spawn a mesh to represent it.
#[allow(clippy::needless_pass_by_value)]
fn spawn_healthbar(
	added: Query<'_, '_, (Entity, Option<&Enemy>), Added<Health>>,
	mut commands: Commands<'_, '_>,
	mut meshes: ResMut<'_, Assets<Mesh>>,
	mut materials: ResMut<'_, Assets<ColorMaterial>>,
) {
	for (entity, is_enemy) in &added {
		commands.entity(entity).with_children(|entity| {
			entity.spawn((
				MaterialMesh2dBundle {
					mesh: meshes.add(Rectangle::new(32., 5.)).into(),
					material: materials.add(Color::from(RED)),
					transform: Transform::from_translation(Vec3::new(0., 20., 0.1)),
					visibility: if is_enemy.is_some() {
						Visibility::Hidden
					} else {
						Visibility::default()
					},
					..MaterialMesh2dBundle::default()
				},
				HealthBar,
				Name::new("Healthbar"),
			));
		});
	}
}

/// Update size and position of the Healthbar.
#[allow(clippy::needless_pass_by_value)]
fn update_healthbar(
	changed: Query<'_, '_, (Entity, &Health, &Children, Option<&Enemy>), Changed<Health>>,
	mut transforms: Query<'_, '_, (&mut Visibility, &mut Transform), With<HealthBar>>,
	mut death: EventWriter<'_, DeathEvent>,
) {
	for (entity, health, children, enemy) in &changed {
		if health.current == 0 {
			death.send(DeathEvent(entity));
			continue;
		}

		for child in children {
			if let Ok((mut visibility, mut transform)) = transforms.get_mut(*child) {
				let percentage = f32::from(health.current) / f32::from(health.max);
				transform.scale.x = percentage;
				transform.translation.x = -16. * (1. - percentage);

				if enemy.is_some() {
					if percentage < 1. {
						visibility.set_if_neq(Visibility::Visible);
					} else {
						visibility.set_if_neq(Visibility::Hidden);
					}
				}
			}
		}
	}
}

/// Ticks abilities currently on cooldown.
#[allow(clippy::needless_pass_by_value)]
fn tick_cooldowns(state: ResMut<'_, State>, mut spellbooks: Query<'_, '_, &mut Spellbook>) {
	if state.is_changed() {
		for mut spellbook in &mut spellbooks {
			for ability in spellbook.0.values_mut() {
				if ability.cooldown_left(state.turn).is_none() {
					ability.last_cast = None;
				}
			}
		}
	}
}

/// Handle when a death event was sent.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
fn death(
	mut commands: Commands<'_, '_>,
	asset_server: Res<'_, AssetServer>,
	mut deaths: EventReader<'_, '_, DeathEvent>,
	mut animations: Query<
		'_,
		'_,
		(
			&EntityIid,
			&GridCoords,
			&mut Transform,
			&mut TextureAtlas,
			&mut Animation,
			&mut Drops,
			Has<Enemy>,
			Has<Player>,
		),
	>,
	layers: Query<'_, '_, (Entity, &LayerMetadata)>,
	mut level_cache: ResMut<'_, LevelCache>,
	state: Res<'_, State>,
) {
	for event in deaths.read() {
		let (
			entity_iid,
			grid_coords,
			mut transform,
			mut atlas,
			mut animation,
			mut drops,
			enemy,
			player,
		) = animations.get_mut(event.0).unwrap();

		let death_animation = if enemy {
			EnemyBundle::death_animation()
		} else if player {
			unimplemented!()
		} else {
			panic!("Player can't be enemy and visa versa.")
		};

		transform.translation.z -= 0.1;

		*animation = death_animation;
		atlas.index = animation.first;

		let mut entity_commands = commands.entity(event.0);
		entity_commands.despawn_descendants();
		level_cache
			.enemies
			.remove(grid_coords)
			.expect("Enemy should be in level cache.");

		for drop in drops.0.drain(..) {
			match drop.as_str() {
				"Key" => {
					if *state
						.keys
						.get(&ItemSource::Loot(entity_iid.clone()))
						.unwrap()
					{
						continue;
					}

					let key_texture =
						asset_server.load::<Image>("Environment/Dungeon Prison/Assets/Props.png");

					let entity = commands
						.spawn((
							Name::new("Key"),
							Key,
							*grid_coords,
							SpriteBundle {
								sprite: Sprite {
									custom_size: Some(Vec2::new(16., 16.)),
									rect: Some(Rect::new(32., 64., 48., 80.)),
									..Sprite::default()
								},
								texture: key_texture,
								..SpriteBundle::default()
							},
						))
						.id();
					commands
						.entity(
							layers
								.iter()
								.find(|(_, layer)| {
									layer.iid == "95801fc0-25d0-11ef-8498-6b3d2275a196"
								})
								.unwrap()
								.0,
						)
						.add_child(entity);

					assert!(
						level_cache
							.keys
							.insert(*grid_coords, (entity, ItemSource::Loot(entity_iid.clone())))
							.is_none(),
						"found key at this position already",
					);
				}
				_ => unimplemented!(),
			}
		}
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
	mut state: ResMut<'_, State>,
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
				let State {
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

/// Character movement.
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
fn movement(
	mut state: ResMut<'_, State>,
	mut commands: Commands<'_, '_>,
	mut input: EventReader<'_, '_, KeyboardInput>,
	mut cast_ability: EventWriter<'_, AbilityEvent>,
	mut player: Query<
		'_,
		'_,
		(
			Entity,
			&mut Transform,
			&mut GridCoords,
			&mut Sprite,
			&mut TextureAtlas,
			&mut Animation,
			Option<&mut AnimationFinish>,
		),
		With<Player>,
	>,
	level_cache: Res<'_, LevelCache>,
	mut level_selection: ResMut<'_, LevelSelection>,
	mut destination_entity: ResMut<'_, PlayerEntityDestination>,
	mut arrived: EventWriter<'_, AnimationArrived>,
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

	let (direction, flip) = match input.key_code {
		KeyCode::KeyW => (GridCoords::new(0, 1), None),
		KeyCode::KeyA => (GridCoords::new(-1, 0), Some(true)),
		KeyCode::KeyS => (GridCoords::new(0, -1), None),
		KeyCode::KeyD => (GridCoords::new(1, 0), Some(false)),
		KeyCode::KeyQ => (GridCoords::new(-1, 1), Some(true)),
		KeyCode::KeyE => (GridCoords::new(1, 1), Some(false)),
		KeyCode::KeyZ => (GridCoords::new(-1, -1), Some(true)),
		KeyCode::KeyC => (GridCoords::new(1, -1), Some(false)),
		_ => return,
	};

	let (player_entity, mut transform, mut grid_pos, mut sprite, mut atlas, mut animation, finish) =
		player.single_mut();
	let destination = *grid_pos + direction;

	match level_cache.destination(state.deref(), *grid_pos, destination) {
		Destination::Walkable => {
			state.turn += 1;

			// Interrupts the animation.
			transform.translation =
				utils::grid_coords_to_translation(*grid_pos, IVec2::splat(GRID_SIZE))
					.extend(transform.translation.z);

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

			let arrived_event = if let Some((_, source)) = level_cache.keys.get(&destination) {
				*state.keys.get_mut(source).unwrap() = true;
				state.player_keys += 1;
				Some(destination)
			} else {
				None
			};

			// Interrupting the animation is fine, but send the event.
			if let Some(mut finish) = finish {
				if let Some(position) = finish.arrived_event {
					arrived.send(AnimationArrived {
						entity: player_entity,
						position,
					});
				}

				finish.arrived_event = arrived_event;
			} else {
				*animation = PlayerBundle::walking_animation();
				atlas.index = animation.first;

				player.insert(AnimationFinish {
					arrived_event,
					new_animation: Some(PlayerBundle::idle_animation()),
				});
			}

			*grid_pos = destination;

			if let Some(flip) = flip {
				sprite.flip_x = flip;
			}
		}
		Destination::Wall => (),
		Destination::Enemy => {
			cast_ability.send(AbilityEvent(Some(
				*level_cache.enemies.get(&destination).unwrap(),
			)));
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

/// Animated sprite.
#[derive(Clone, Component)]
struct Animation {
	/// Animation timing.
	timer:     Timer,
	/// First animation sprite.
	first:     usize,
	/// Last animation sprite.
	last:      usize,
	/// Animation is repeated.
	repeating: bool,
	/// Custom anchor for this animation.
	anchor:    Option<Anchor>,
}

/// Animate entities.
#[allow(clippy::needless_pass_by_value)]
fn animate(
	mut commands: Commands<'_, '_>,
	time: Res<'_, Time>,
	mut query: Query<'_, '_, (Entity, &mut Sprite, &mut TextureAtlas, &mut Animation)>,
) {
	for (entity, mut sprite, mut atlas, mut animation) in &mut query {
		if let Some(anchor) = animation.anchor {
			sprite.anchor = anchor;
		} else {
			sprite.anchor = Anchor::default();
		}

		animation.timer.tick(time.delta());

		if animation.timer.just_finished() {
			atlas.index = if atlas.index == animation.last {
				if animation.repeating {
					animation.first
				} else {
					commands.entity(entity).remove::<Animation>();
					return;
				}
			} else {
				atlas.index + 1
			};
		}
	}
}

/// What should happen after an animation has completed.
#[derive(Component)]
struct AnimationFinish {
	/// Tile animation has finished on.
	arrived_event: Option<GridCoords>,
	/// Switch to a new animation.
	new_animation: Option<Animation>,
}

/// Remove [`Animator`] after completion and potentially transition to old animation.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
fn finish_animation(
	mut commands: Commands<'_, '_>,
	mut completed: EventReader<'_, '_, TweenCompleted>,
	mut arrived: EventWriter<'_, AnimationArrived>,
	mut query: Query<
		'_,
		'_,
		(Entity, &AnimationFinish, &mut TextureAtlas, &mut Animation),
		With<Animator<Transform>>,
	>,
) {
	for completed in completed.read() {
		let mut command = commands.entity(completed.entity);

		// Transition to old animation.
		if let Ok((entity, finish, mut atlas, mut animation)) = query.get_mut(completed.entity) {
			if let Some(new_animation) = &finish.new_animation {
				*animation.deref_mut() = new_animation.clone();
				atlas.index = animation.first;
			}

			if let Some(position) = finish.arrived_event {
				arrived.send(AnimationArrived { entity, position });
			}

			command.remove::<AnimationFinish>();
		}

		command.remove::<Animator<Transform>>();
	}
}

/// Event fired when the animation has reached a tile or has been interrupted.
#[derive(Event)]
struct AnimationArrived {
	/// Entity that arriveed.
	entity:   Entity,
	/// Arrived at which grid coordinates.
	position: GridCoords,
}

#[allow(clippy::needless_pass_by_value)]
fn animation_arrived_tile(
	mut commands: Commands<'_, '_>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut arrived: EventReader<'_, '_, AnimationArrived>,
	player: Query<'_, '_, Entity, With<Player>>,
) {
	if arrived.is_empty() {
		return;
	}

	let player = player.single();

	for arrived in arrived.read() {
		if arrived.entity == player {
			if let Some((entity, _)) = level_cache.keys.remove(&arrived.position) {
				commands.entity(entity).insert(Visibility::Hidden);
			}
		}
	}
}

/// Item UI.
#[allow(clippy::needless_pass_by_value)]
fn item_ui(
	asset_server: Res<'_, AssetServer>,
	state: Res<'_, State>,
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
fn turn_ui(state: Res<'_, State>, mut contexts: EguiContexts<'_, '_>) {
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
	state: Res<'_, State>,
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
				movement,
				select_ability,
				door_interactions,
				spawn_healthbar,
				cast_ability,
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
				animate,
				(
					finish_animation,
					animation_arrived_tile,
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
		.init_resource::<State>()
		.init_resource::<LevelCache>()
		.init_resource::<CursorPos>()
		.init_resource::<PlayerEntityDestination>()
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
		.register_type::<State>()
		.register_type::<Spellbook>()
		.run();
}
