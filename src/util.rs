//! Utility functions.

use bevy_ecs_ldtk::GridCoords;
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
