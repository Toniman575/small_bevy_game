//! All abilities currently in the game.

use std::time::Duration;

use bevy::prelude::*;
use bevy::utils::HashMap;

use super::EffectType::{AttackBuff, AttackDebuff, DefensiveBuff, DefensiveDebuff, Dot};
use super::{Ability, AbilityEffect, AbilityId, StatusEffect};
use crate::{TextureIcon, Textures};

/// The list of all abilities.
#[derive(Resource, Reflect)]
pub(crate) struct Abilities(pub(crate) HashMap<AbilityId, Ability>);

impl Abilities {
	/// All abilities currently in the game.
	#[expect(clippy::too_many_lines)]
	pub(crate) fn new(textures: &Textures) -> Self {
		Self(HashMap::from([
			(
				AbilityId(0),
				Ability::new(
					String::from("Stab"),
					1,
					None,
					AbilityEffect::Damage(8),
					None,
					Some(TextureIcon::AutoAttack),
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
					Some(TextureIcon::Arrow),
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
					AbilityEffect::Damage(15),
					Some(3),
					Some(TextureIcon::Slash),
					Some(super::AbilityAnimation {
						texture:  textures.slash_animation.clone(),
						duration: Duration::from_secs_f64(0.25),
						scale:    0.5,
						atlas:    Some((textures.slash_animation_atlas.clone(), 6)),
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
					Some(10),
					None,
					Some(super::AbilityAnimation {
						texture:  textures.teleport_animation.clone(),
						duration: Duration::from_secs_f64(0.5),
						scale:    0.5,
						atlas:    Some((textures.teleport_animation_atlas.clone(), 6)),
					}),
				),
			),
			(
				AbilityId(5),
				Ability::new(
					String::from("DefensiveBuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(
						String::from("Force Shield"),
						0.75,
						3,
						DefensiveBuff,
					)),
					Some(5),
					None,
					None,
				),
			),
			(
				AbilityId(6),
				Ability::new(
					String::from("AttackBuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(
						String::from("Muscle Up"),
						2.,
						3,
						AttackBuff,
					)),
					Some(5),
					None,
					None,
				),
			),
			(
				AbilityId(7),
				Ability::new(
					String::from("DefensiveDebuff"),
					1,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(
						String::from("Rend Armor"),
						0.75,
						3,
						DefensiveDebuff,
					)),
					Some(5),
					None,
					None,
				),
			),
			(
				AbilityId(8),
				Ability::new(
					String::from("AttackDebuff"),
					3,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(
						String::from("Intimidate"),
						0.5,
						3,
						AttackDebuff,
					)),
					Some(5),
					None,
					None,
				),
			),
			(
				AbilityId(9),
				Ability::new(
					String::from("Dot"),
					3,
					None,
					AbilityEffect::StatusEffect(StatusEffect::new(
						String::from("Bleed"),
						2.,
						3,
						Dot,
					)),
					Some(5),
					None,
					None,
				),
			),
			(
				AbilityId(10),
				Ability::new(
					String::from("Skeleton Brawl"),
					1,
					None,
					AbilityEffect::Damage(3),
					None,
					None,
					None,
				),
			),
			(
				AbilityId(11),
				Ability::new(
					String::from("Bone Throw"),
					2,
					None,
					AbilityEffect::Damage(1),
					Some(2),
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
				AbilityId(12),
				Ability::new(
					String::from("Skeleton Fireball"),
					5,
					None,
					AbilityEffect::Damage(10),
					Some(5),
					None,
					Some(super::AbilityAnimation {
						texture:  textures.fire.clone(),
						duration: Duration::from_secs_f64(0.2),
						scale:    1.,
						atlas:    None,
					}),
				),
			),
			(
				AbilityId(13),
				Ability::new(
					String::from("Charge"),
					4,
					None,
					AbilityEffect::Charge(10),
					Some(6),
					Some(TextureIcon::Charge),
					Some(super::AbilityAnimation {
						texture:  textures.charge_animation.clone(),
						duration: Duration::from_secs_f64(0.25),
						scale:    0.5,
						atlas:    Some((textures.charge_animation_atlas.clone(), 6)),
					}),
				),
			),
			(
				AbilityId(14),
				Ability::new(
					String::from("Shield Slam"),
					1,
					None,
					AbilityEffect::Slam(20),
					Some(6),
					None,
					Some(super::AbilityAnimation {
						texture:  textures.shield_bash_animation.clone(),
						duration: Duration::from_secs_f64(0.25),
						scale:    0.5,
						atlas:    Some((textures.shield_bash_animation_atlas.clone(), 6)),
					}),
				),
			),
			(
				AbilityId(15),
				Ability::new(
					String::from("Cheat"),
					50,
					None,
					AbilityEffect::Damage(200),
					None,
					None,
					None,
				),
			),
			(
				AbilityId(16),
				Ability::new(
					String::from("Skeleton Slash"),
					1,
					None,
					AbilityEffect::Damage(8),
					None,
					None,
					None,
				),
			),
			(
				AbilityId(17),
				Ability::new(
					String::from("Cheat Teleport"),
					50,
					Some(0),
					AbilityEffect::Teleport,
					None,
					None,
					Some(super::AbilityAnimation {
						texture:  textures.teleport_animation.clone(),
						duration: Duration::from_secs_f64(0.5),
						scale:    0.5,
						atlas:    Some((textures.teleport_animation_atlas.clone(), 6)),
					}),
				),
			),
		]))
	}
}
