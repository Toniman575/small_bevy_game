//! TODO:
//! - show all levels in debug mode
//! - add player walking animation
//! - make doors open and close
//! - move to room when moving into the correct direction while standing inside the door
//! - don't switch level before animation is finished or interrupted

#![allow(clippy::multiple_crate_versions)]

use std::mem;
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use bevy::input::keyboard::KeyboardInput;
use bevy::input::ButtonState;
use bevy::prelude::*;
use bevy::utils::HashSet;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::prelude::*;
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pancam::{MoveMode, PanCam, PanCamPlugin};
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween, TweenCompleted, TweeningPlugin};

/// The size of the Grid in pixels.
const GRID_SIZE: i32 = 16;

/// Player marker component.
#[derive(Default, Component)]
struct Player;

/// Player bundle.
#[derive(Bundle, LdtkEntity)]
struct PlayerBundle {
	/// Player marker component.
	player:              Player,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: SpriteSheetBundle,
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
			sprite_sheet_bundle: SpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Animation {
				timer: Timer::from_seconds(0.25, TimerMode::Repeating),
				first: 0,
				last:  3,
			},
			worldly:             Worldly::default(),
		}
	}
}

/// Caches which entity the Player is teleporting to (currently just between doors).
#[derive(Default, Resource)]
struct PlayerEntityDestination(Option<EntityIid>);

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
#[derive(Debug, Default, Component, Clone)]
struct DestinationDoor {
	/// Level the destination door is in.
	level:  LevelIid,
	/// Entity of the destination door.
	entity: EntityIid,
}

impl DestinationDoor {
	/// Get destination door.
	fn from_field(entity_instance: &EntityInstance) -> Self {
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

/// Door marker component.
#[derive(Default, Component)]
struct Door;

/// Door bundle.
#[derive(Default, Bundle, LdtkEntity)]
struct DoorBundle {
	/// Door marker component.
	door:                Door,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: SpriteSheetBundle,
	#[with(DestinationDoor::from_field)]
	/// Which Door this Door leads to.
	destination_door:    DestinationDoor,
	/// Door grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
}

#[derive(Default, Resource)]
struct LevelBoundaries {
	/// The cashed walls of this level.
	walls:  HashSet<GridCoords>,
	/// The level width in tiles.
	width:  i32,
	/// The level height in tiles.
	height: i32,
}

impl LevelBoundaries {
	/// Checks if [`GridCoords`] is a wall.
	fn movable(&self, grid_coords: GridCoords) -> bool {
		grid_coords.x < 0
			|| grid_coords.y < 0
			|| grid_coords.x >= self.width
			|| grid_coords.y >= self.height
			|| self.walls.contains(&grid_coords)
	}
}
/// Startup system.
#[allow(clippy::needless_pass_by_value)]
fn startup(mut commands: Commands<'_, '_>, asset_server: Res<'_, AssetServer>) {
	commands.spawn(Camera2dBundle::default()).insert(PanCam {
		move_mode: MoveMode::Mouse,
		grab_buttons: Vec::new(),
		zoom_to_cursor: false,
		min_scale: 0.1,
		max_scale: Some(10.),
		..PanCam::default()
	});

	commands
		.spawn(LdtkWorldBundle {
			ldtk_handle: asset_server.load("test.ldtk"),
			..default()
		})
		.insert(Name::new("Map"));
}

/// Initialize states after level is spawned.
#[allow(clippy::needless_pass_by_value, clippy::too_many_arguments)]
fn level_spawn(
	mut commands: Commands<'_, '_>,
	mut level_boundaries: ResMut<'_, LevelBoundaries>,
	mut level_events: EventReader<'_, '_, LevelEvent>,
	walls: Query<'_, '_, &GridCoords, (With<Wall>, Without<Player>)>,
	mut player_entity_destination: ResMut<'_, PlayerEntityDestination>,
	ldtk_entities: Query<'_, '_, (Entity, &EntityIid), Without<Player>>,
	entity_grid_coords: Query<'_, '_, &GridCoords, Without<Player>>,
	mut player: Query<'_, '_, (Entity, &mut GridCoords), With<Player>>,
	ldtk_project_entities: Query<'_, '_, &Handle<LdtkProject>>,
	ldtk_project_assets: Res<'_, Assets<LdtkProject>>,
) {
	let Some(LevelEvent::Spawned(level_iid)) = level_events.read().next() else {
		return;
	};

	let ldtk_project = ldtk_project_assets
		.get(ldtk_project_entities.single())
		.expect("LdtkProject should be loaded when level is spawned");
	let level = ldtk_project
		.get_raw_level_by_iid(level_iid.get())
		.expect("spawned level should exist in project");

	*level_boundaries = LevelBoundaries {
		walls:  walls.iter().copied().collect(),
		width:  level.px_wid / GRID_SIZE,
		height: level.px_hei / GRID_SIZE,
	};

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

		commands
			.entity(player_entity)
			.remove::<Animator<Transform>>();

		player_entity_destination.0 = None;
	}
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

/// Character movement.
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
fn movement(
	mut commands: Commands<'_, '_>,
	debug: Res<'_, Debug>,
	mut input: EventReader<'_, '_, KeyboardInput>,
	mut player: Query<'_, '_, (Entity, &mut Transform, &mut GridCoords, &mut Sprite), With<Player>>,
	level_boundaries: Res<'_, LevelBoundaries>,
	mut level_selection: ResMut<'_, LevelSelection>,
	doors: Query<'_, '_, (&GridCoords, &DestinationDoor), (With<Door>, Without<Player>)>,
	mut destination_entity: ResMut<'_, PlayerEntityDestination>,
) {
	if let Debug::Active { .. } = debug.deref() {
		return;
	}

	let Some(input) = input.read().next() else {
		return;
	};
	let ButtonState::Released = input.state else {
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

	let (player, mut transform, mut grid_pos, mut sprite) = player.single_mut();
	let destination = *grid_pos + direction;

	if !level_boundaries.movable(destination) {
		transform.translation =
			utils::grid_coords_to_translation(*grid_pos, IVec2::splat(GRID_SIZE))
				.extend(transform.translation.z);

		commands.entity(player).insert(Animator::new(
			Tween::new(
				EaseMethod::Linear,
				Duration::from_millis(250),
				TransformPositionLens {
					start: transform.translation,
					end:   utils::grid_coords_to_translation(destination, IVec2::splat(GRID_SIZE))
						.extend(transform.translation.z),
				},
			)
			.with_completed_event(0),
		));

		*grid_pos = destination;

		if let Some(flip) = flip {
			sprite.flip_x = flip;
		}
	}

	for (door_coord, destination_door) in &doors {
		if *door_coord == destination {
			if let LevelSelection::Iid(current_level) = level_selection.as_mut() {
				*current_level = destination_door.level.clone();
				destination_entity.0 = Some(destination_door.entity.clone());
			} else {
				unreachable!("Levels should only be LevelIid")
			}
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

/// Animated sprite.
#[derive(Component)]
struct Animation {
	/// Animation timing.
	timer: Timer,
	/// First animation sprite.
	first: usize,
	/// Last animation sprite.
	last:  usize,
}

/// Animate entities.
#[allow(clippy::needless_pass_by_value)]
fn animate(
	time: Res<'_, Time>,
	mut query: Query<'_, '_, (&mut TextureAtlas, &mut Animation), With<Player>>,
) {
	if let Ok((mut atlas, mut animation)) = query.get_single_mut() {
		animation.timer.tick(time.delta());
		if animation.timer.just_finished() {
			atlas.index = if atlas.index == animation.last {
				animation.first
			} else {
				atlas.index + 1
			};
		}
	}
}

/// Remove [`Animator`] after completion.
fn finish_animation(
	mut commands: Commands<'_, '_>,
	mut completed: EventReader<'_, '_, TweenCompleted>,
) {
	for completed in completed.read() {
		commands
			.entity(completed.entity)
			.remove::<Animator<Transform>>();
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
			WorldInspectorPlugin::new()
				.run_if(|debug: Res<'_, Debug>| matches!(debug.deref(), Debug::Active { .. })),
		))
		.add_systems(Startup, startup)
		.add_systems(First, level_spawn)
		.add_systems(PreUpdate, debug)
		.add_systems(Update, movement)
		.add_systems(
			PostUpdate,
			(
				animate,
				(
					finish_animation,
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
		.init_resource::<LevelBoundaries>()
		.init_resource::<PlayerEntityDestination>()
		.register_ldtk_entity::<PlayerBundle>("Player")
		.register_ldtk_entity::<DoorBundle>("Door")
		.register_ldtk_int_cell::<WallBundle>(1)
		.run();
}
