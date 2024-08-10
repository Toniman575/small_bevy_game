//! Fog of war functionality.

use std::ops::Deref;

use bevy::color::palettes::css::*;
use bevy::prelude::*;
use bevy_ecs_ldtk::{GridCoords, LevelSelection};
use bevy_ecs_tilemap::tiles::{TileColor, TileVisible};

use crate::gameplay::{Enemy, Player, Vision};
use crate::{util, Debug, GameState, Key, LevelCache};

/// Calculates the field of view from an entity with [`Vision`].
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
pub(crate) fn generate_fov(
	mut commands: Commands<'_, '_>,
	mut origin_q: Query<'_, '_, (&mut Vision, &GridCoords, Has<Player>), Changed<GridCoords>>,
	level_cache: ResMut<'_, LevelCache>,
	mut game_state: ResMut<'_, GameState>,
	current_level: Res<'_, LevelSelection>,
) {
	for (mut vision, origin, has_player) in &mut origin_q {
		let mut visible_tiles = vec![*origin];

		symmetric_shadowcasting::compute_fov(
			#[allow(clippy::as_conversions)]
			(origin.x as isize, origin.y as isize),
			&mut |pos| {
				#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
				let (x, y) = (pos.0 as i32, pos.1 as i32);

				level_cache.walls.contains(&GridCoords::new(x, y))
					|| u32::try_from(util::euclidean_distance(GridCoords::new(x, y), *origin))
						.unwrap() > vision.range.into()
			},
			&mut |pos| {
				#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
				let (x, y) = (pos.0 as i32, pos.1 as i32);

				if !visible_tiles.contains(&GridCoords::new(x, y)) {
					visible_tiles.push(GridCoords::new(x, y));
				}
			},
		);

		vision.tiles = visible_tiles;

		if has_player {
			let LevelSelection::Iid(current_level_iid) = current_level.as_ref() else {
				unreachable!("processing FoW without a level")
			};

			let visited_tiles = game_state
				.visited_tiles
				.entry(current_level_iid.clone())
				.or_default();
			visited_tiles.extend(&vision.tiles);

			commands.trigger(ApplyFov);
		}
	}
}

/// [`Trigger`] for applying FoV.
#[derive(Event)]
pub(crate) struct ApplyFov;

/// Applies field of view for the player;
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
pub(crate) fn apply_fov(
	_: Trigger<'_, ApplyFov>,
	debug: Res<'_, Debug>,
	player_q: Query<'_, '_, &Vision, With<Player>>,
	mut tile_visible_q: Query<'_, '_, (&GridCoords, &mut TileVisible, &mut TileColor)>,
	mut object_visible_q: Query<
		'_,
		'_,
		(
			&GridCoords,
			&mut Visibility,
			&mut Sprite,
			Has<Key>,
			Has<Enemy>,
		),
	>,
	level_cache: ResMut<'_, LevelCache>,
	mut game_state: ResMut<'_, GameState>,
	current_level: Res<'_, LevelSelection>,
) {
	let Ok(vision) = player_q.get_single() else {
		return;
	};

	let LevelSelection::Iid(current_level_iid) = current_level.as_ref() else {
		unreachable!("processing FoW without a level")
	};

	let visited_tiles = game_state
		.visited_tiles
		.entry(current_level_iid.clone())
		.or_default();

	for (grid_coords, mut tile_visible, mut tile_color) in &mut tile_visible_q {
		if matches!(debug.deref(), Debug::Active { .. }) || vision.tiles.contains(grid_coords) {
			tile_visible.set_if_neq(TileVisible(true));
			tile_color.0 = Color::Srgba(WHITE);
		} else if visited_tiles.contains(grid_coords) {
			tile_color.0 = Color::Srgba(LIGHT_GREY);
		} else {
			tile_visible.set_if_neq(TileVisible(false));
		}
	}

	for (grid_coords, mut visibility, mut sprite, has_key, has_enemy) in &mut object_visible_q {
		if has_key && !level_cache.keys.contains_key(grid_coords) {
			continue;
		}

		if matches!(debug.deref(), Debug::Active { .. }) || vision.tiles.contains(grid_coords) {
			visibility.set_if_neq(Visibility::Inherited);
			sprite.color = Color::Srgba(WHITE);
		} else if !has_enemy && visited_tiles.contains(grid_coords) {
			sprite.color = Color::Srgba(LIGHT_GREY);
		} else {
			visibility.set_if_neq(Visibility::Hidden);
		}
	}
}
