//! Player functionality.

use bevy::prelude::*;
use bevy_ecs_ldtk::prelude::*;

use super::{ActiveAbility, Health, Spellbook};
use crate::animation::Animation;

/// Player marker component.
#[derive(Default, Component)]
pub(crate) struct Player;

/// Player bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct PlayerBundle {
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
}
