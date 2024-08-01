//! Enemy functionality.

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy_ecs_ldtk::prelude::*;

use super::Health;
use crate::animation::Animation;
use crate::Drops;

/// Enemy marker component.
#[derive(Default, Component)]
pub(crate) struct Enemy;

/// Enemy bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct EnemyBundle {
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
	pub(crate) fn idle_animation() -> Animation {
		Animation {
			timer:     Timer::from_seconds(0.25, TimerMode::Repeating),
			first:     0,
			last:      3,
			repeating: true,
			anchor:    None,
		}
	}

	/// Return walking [`Animation`].
	pub(crate) fn walking_animation() -> Animation {
		Animation {
			timer:     Timer::from_seconds(0.1, TimerMode::Repeating),
			first:     9,
			last:      14,
			repeating: true,
			anchor:    None,
		}
	}

	/// Return death [`Animation`].
	pub(crate) fn death_animation() -> Animation {
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
