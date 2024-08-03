//! Utility functions.

use std::array::IntoIter;

use bevy::sprite::Sprite;
use bevy_ecs_ldtk::GridCoords;
use bevy_ecs_tilemap::helpers::square_grid::neighbors::{Neighbors, SquareDirection};
use bevy_ecs_tilemap::map::TilemapSize;
use bevy_ecs_tilemap::tiles::TilePos;
use bevy_egui::egui::emath::Numeric;

/// Calculates euclidian distance.
pub(crate) fn euclidean_distance(target: GridCoords, origin: GridCoords) -> f64 {
	((target.x - origin.x).pow(2) + (target.y - origin.y).pow(2))
		.to_f64()
		.sqrt()
}

/// Calculates manhatten distance.
pub(crate) const fn manhatten_distance(target: GridCoords, origin: GridCoords) -> i32 {
	(target.x - origin.x).abs() + (target.y - origin.y).abs()
}

/// Calculates angle and appopriatly flip a sprite.
pub(crate) fn flip_sprite(
	origin_grid_coord: GridCoords,
	target_grid_coord: GridCoords,
	sprite: &mut Sprite,
) {
	let dx = target_grid_coord.x - origin_grid_coord.x;
	let dy = target_grid_coord.y - origin_grid_coord.y;
	let degrees = f64::atan2(dy.into(), dx.into()).to_degrees();

	#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
	match degrees.round() as i16 {
		-89..90 => sprite.flip_x = false,
		-180..-90 | 91..=180 => sprite.flip_x = true,
		-90 | 90 => (),
		angle => unreachable!("invalid angle found: {}", angle),
	}
}

/// [`Iterator`] for [`NeighborsExt::ordered_iter()`]
pub(crate) struct OrderedNeighbors {
	/// Holds the neighbors.
	neighbors: Neighbors<TilePos>,
	/// Iterates through directions.
	iter:      IntoIter<SquareDirection, 8>,
}

impl OrderedNeighbors {
	/// Returns an iterator over [`Neighbors`] ordered to our preference.
	pub(crate) fn new(tile_pos: TilePos, map_size: TilemapSize) -> Self {
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

		let neighbors = Neighbors::get_square_neighboring_positions(&tile_pos, &map_size, true);

		Self {
			neighbors,
			iter: DIRECTIONS.into_iter(),
		}
	}
}

impl Iterator for OrderedNeighbors {
	type Item = TilePos;

	fn next(&mut self) -> Option<Self::Item> {
		self.neighbors.get(self.iter.next()?).copied()
	}
}
