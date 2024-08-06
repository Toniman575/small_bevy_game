//! Fog of war functionality.

use bevy::prelude::*;
use bevy_ecs_ldtk::GridCoords;
use bevy_ecs_tilemap::tiles::TileVisible;

use crate::gameplay::Player;
use crate::{util, Key, LevelCache};

/// How far the player can see.
const FOV_DISTANCE: u32 = 3;

/// Calculates the field of view from the player;
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn generate_fov(
	origin_q: Query<'_, '_, &GridCoords, (With<Player>, Changed<GridCoords>)>,
	mut tile_visible_q: Query<
		'_,
		'_,
		(
			&GridCoords,
			Option<&mut TileVisible>,
			&mut Visibility,
			Has<Key>,
		),
	>,
	mut level_cache: ResMut<'_, LevelCache>,
) {
	let Ok(origin) = origin_q.get_single() else {
		return;
	};

	let mut visible_tiles = vec![*origin];

	symmetric_shadowcasting::compute_fov(
		#[allow(clippy::as_conversions)]
		(origin.x as isize, origin.y as isize),
		&mut |pos| {
			#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
			let (x, y) = (pos.0 as i32, pos.1 as i32);

			level_cache.walls.contains(&GridCoords::new(x, y))
				|| u32::try_from(util::euclidean_distance(GridCoords::new(x, y), *origin)).unwrap()
					> FOV_DISTANCE
		},
		&mut |pos| {
			#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
			let (x, y) = (pos.0 as i32, pos.1 as i32);

			if !visible_tiles.contains(&GridCoords::new(x, y)) {
				visible_tiles.push(GridCoords::new(x, y));
			}
		},
	);

	level_cache.visible_tiles = visible_tiles;

	for (grid_coords, tile_visible, mut visible, has_key) in &mut tile_visible_q {
		if has_key && !level_cache.keys.contains_key(grid_coords) {
			continue;
		}

		if level_cache.visible_tiles.contains(grid_coords) {
			if let Some(mut tile_visible) = tile_visible {
				tile_visible.set_if_neq(TileVisible(true));
			} else {
				visible.set_if_neq(Visibility::Inherited);
			}
		} else if let Some(mut tile_visible) = tile_visible {
			tile_visible.set_if_neq(TileVisible(false));
		} else {
			visible.set_if_neq(Visibility::Hidden);
		}
	}
}
