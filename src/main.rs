//! TODO

#![allow(clippy::multiple_crate_versions)]

use bevy::prelude::*;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_tilemap::prelude::*;
use bevy_pancam::{PanCam, PanCamPlugin};

/// Startup system.
#[allow(clippy::needless_pass_by_value)]
fn startup(mut commands: Commands<'_, '_>, asset_server: Res<'_, AssetServer>) {
    commands
        .spawn(Camera2dBundle::default())
        .insert(PanCam::default());

    commands.spawn(LdtkWorldBundle {
        ldtk_handle: asset_server.load("test.ldtk"),
        ..Default::default()
    });
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            PanCamPlugin,
            TilemapPlugin,
            LdtkPlugin,
        ))
        .add_systems(Startup, startup)
        .insert_resource(LevelSelection::index(0))
        .run();
}
