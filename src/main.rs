//! TODO:
//! - multi-levels

#![allow(clippy::multiple_crate_versions)]

use std::{
    mem,
    ops::{Deref, DerefMut},
};

use bevy::{
    input::{keyboard::KeyboardInput, ButtonState},
    prelude::*,
    utils::HashSet,
};
use bevy_ecs_ldtk::{prelude::*, utils};
use bevy_ecs_tilemap::prelude::*;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pancam::{MoveMode, PanCam, PanCamPlugin};

/// The size of the Grid in pixels.
const GRID_SIZE: i32 = 16;

/// Player marker component.
#[derive(Default, Component)]
struct Player;

/// Player bundle.
#[derive(Default, Bundle, LdtkEntity)]
struct PlayerBundle {
    /// Player marker component.
    player: Player,
    /// Sprite bundle.
    #[sprite_sheet_bundle]
    sprite_sheet_bundle: SpriteSheetBundle,
    /// Player grid coordinates.
    #[grid_coords]
    grid_coords: GridCoords,
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

#[derive(Default, Resource)]
struct LevelWalls(HashSet<GridCoords>);

impl LevelWalls {
    /// Checks if [`GridCoords`] is a wall.
    fn in_wall(&self, grid_coords: GridCoords) -> bool {
        self.0.contains(&grid_coords)
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
#[allow(clippy::needless_pass_by_value)]
fn level_spawn(
    mut level_walls: ResMut<'_, LevelWalls>,
    mut level_events: EventReader<'_, '_, LevelEvent>,
    walls: Query<'_, '_, &GridCoords, With<Wall>>,
) {
    let Some(LevelEvent::Spawned(_)) = level_events.read().next() else {
        return;
    };

    *level_walls = LevelWalls(walls.iter().copied().collect());
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
        cam_transform: Transform,
        /// Camera [`OrthographicProjection`] state before debugging.
        cam_projection: OrthographicProjection,
    },
}

/// Enables/Disables debug mode.
#[allow(clippy::needless_pass_by_value)]
fn debug(
    mut input: EventReader<'_, '_, KeyboardInput>,
    mut debug: ResMut<'_, Debug>,
    mut cam: Query<
        '_,
        '_,
        (&mut Transform, &mut OrthographicProjection, &mut PanCam),
        With<Camera>,
    >,
) {
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
                        cam_transform: *cam_transform,
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
#[allow(clippy::needless_pass_by_value)]
fn movement(
    mut input: EventReader<'_, '_, KeyboardInput>,
    mut player: Query<'_, '_, &mut GridCoords, With<Player>>,
    level_walls: Res<'_, LevelWalls>,
) {
    let Some(input) = input.read().next() else {
        return;
    };
    let ButtonState::Released = input.state else {
        return;
    };

    let direction = match input.key_code {
        KeyCode::KeyW => GridCoords::new(0, 1),
        KeyCode::KeyA => GridCoords::new(-1, 0),
        KeyCode::KeyS => GridCoords::new(0, -1),
        KeyCode::KeyD => GridCoords::new(1, 0),
        KeyCode::KeyQ => GridCoords::new(-1, 1),
        KeyCode::KeyE => GridCoords::new(1, 1),
        KeyCode::KeyZ => GridCoords::new(-1, -1),
        KeyCode::KeyC => GridCoords::new(1, -1),
        _ => return,
    };

    let mut player = player.single_mut();
    let destination = *player + direction;

    if !level_walls.in_wall(destination) {
        *player = destination;
    }
}

/// Updates [`Transform`] for [`Entity`]s with changed [`GridCoords`].
fn translate_grid_coords_entities(
    mut grid_coords_entities: Query<'_, '_, (&mut Transform, &GridCoords), Changed<GridCoords>>,
) {
    for (mut transform, grid_coords) in &mut grid_coords_entities {
        transform.translation =
            utils::grid_coords_to_translation(*grid_coords, IVec2::splat(GRID_SIZE))
                .extend(transform.translation.z);
    }
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            PanCamPlugin,
            TilemapPlugin,
            LdtkPlugin,
            WorldInspectorPlugin::new()
                .run_if(|debug: Res<'_, Debug>| matches!(debug.deref(), Debug::Active { .. })),
        ))
        .add_systems(Startup, startup)
        .add_systems(First, level_spawn)
        .add_systems(PreUpdate, debug)
        .add_systems(Update, movement)
        .add_systems(
            PostUpdate,
            (translate_grid_coords_entities, camera_update).chain(),
        )
        .insert_resource(LevelSelection::index(0))
        .init_resource::<LevelWalls>()
        .insert_resource(Debug::Inactive)
        .register_ldtk_entity::<PlayerBundle>("Player")
        .register_ldtk_int_cell::<WallBundle>(1)
        .run();
}
