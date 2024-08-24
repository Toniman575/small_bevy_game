//! All abilities currently in the game.

use std::time::Duration;

use bevy::prelude::*;
use bevy::utils::HashMap;

use super::EffectType::{AttackBuff, AttackDebuff, DefensiveBuff, DefensiveDebuff};
use super::{Ability, AbilityEffect, AbilityId, StatusEffect};
use crate::Textures;

/// The list of all abilities.
#[derive(Resource, Reflect)]
pub(crate) struct Abilities(pub(crate) HashMap<AbilityId, Ability>);

impl Abilities {
	/// All abilities currently in the game.
	#[allow(clippy::too_many_lines)]
	pub(crate) fn new(textures: &Textures) -> Self {
		Self(HashMap::from([
			(
				AbilityId(0),
				Ability::new(
					String::from("Autoattack"),
					1,
					None,
					AbilityEffect::Damage(5),
					None,
					None,
				),
			),
			(
				AbilityId(1),
				Ability::new(
					String::from("Ranged"),
					5,
					None,
					AbilityEffect::Damage(3),
					None,
					Some(super::AbilityAnimation {
						texture:  textures.arrow.clone(),
						duration: Duration::from_secs_f64(0.2),
						scale:    0.5,
						atlas:    None,
					}),
				),
			),
			(
				AbilityId(2),
				Ability::new(
					String::from("Hardcore"),
					1,
					None,
					AbilityEffect::Damage(10),
					Some(3),
					Some(super::AbilityAnimation {
						texture:  textures.slash.clone(),
						duration: Duration::from_secs_f64(0.25),
						scale:    0.5,
						atlas:    Some((textures.slash_atlas.clone(), 6)),
					}),
				),
			),
			(
				AbilityId(3),
				Ability::new(
					String::from("Heal"),
					1,
					None,
					AbilityEffect::Healing(10),
					Some(10),
					None,
				),
			),
			(
				AbilityId(4),
				Ability::new(
					String::from("Teleport"),
					5,
					Some(0),
					AbilityEffect::Teleport,
					None,
					None,
				),
			),
			(
				AbilityId(5),
				Ability::new(
					String::from("DefensiveBuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(String::from("Force Shield"), 0.75, 3, DefensiveBuff)),
					Some(5),
					None,
				),
			),
			(
				AbilityId(6),
				Ability::new(
					String::from("AttackBuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(String::from("Muscle Up"), 2., 3, AttackBuff)),
					Some(5),
					None,
				),
			),
			(
				AbilityId(7),
				Ability::new(
					String::from("DefensiveDebuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(String::from("Rend Armor"), 0.75, 3, DefensiveDebuff)),
					Some(5),
					None,
				),
			),
			(
				AbilityId(8),
				Ability::new(
					String::from("AttackDebuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(String::from("Weaken"), 0.5, 3, AttackDebuff)),
					Some(5),
					None,
				),
			),
			(
				AbilityId(9),
				Ability::new(
					String::from("Autoattack"),
					1,
					None,
					AbilityEffect::Damage(3),
					None,
					None,
				),
			),
			(
				AbilityId(10),
				Ability::new(
					String::from("Ranged"),
					2,
					None,
					AbilityEffect::Damage(1),
					None,
					None,
				),
			),
		]))
	}
}
