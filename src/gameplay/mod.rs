//! Shared gameplay functionality.

mod abilities;
mod enemy;
mod player;

use std::collections::VecDeque;
use std::time::Duration;

use bevy::color::palettes::css::RED;
use bevy::prelude::*;
use bevy::sprite::MaterialMesh2dBundle;
use bevy::utils::hashbrown::{HashMap, HashSet};
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::map::TilemapSize;
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween};
use line_drawing::WalkGrid;

pub(crate) use self::abilities::Abilities;
pub(crate) use self::enemy::{
	add_boss_abilities, change_enemy_gridcoords, move_enemies, BaseSkeletonBundle, Boss, Enemy,
	MageSkeletonBundle, NecromancerEnemyBundle, WarriorSkeletonBundle,
};
pub(crate) use self::player::{
	cast_ability, door_interactions, player_movement, select_ability, Player, PlayerBundle,
};
use crate::animation::{Animation, AnimationAbility, ArrivedAtTile};
use crate::util::{flip_sprite, OrderedNeighbors};
use crate::{
	util, Drops, GameState, ItemSource, LevelCache, Object, PlayerBusy, TextureIcon, Textures,
	Turn, TurnState, GRID_SIZE,
};

/// Component for tracking dead things.
#[derive(Clone, Component, Debug, Default, Reflect, PartialEq, Eq, Copy)]
pub(crate) struct Dead;

/// Component for tracking health in entities.
#[derive(Clone, Component, Debug, Default, Reflect, PartialEq, Eq, Copy)]
pub(crate) struct Health {
	/// How much health the entity currently has.
	pub(crate) current: u16,
	/// How much health the entity can have.
	pub(crate) max:     u16,
}

impl From<&EntityInstance> for Health {
	fn from(entity_instance: &EntityInstance) -> Self {
		let reference = *entity_instance
			.get_int_field("Health")
			.expect("expected entity to have non-nullable `Health` int field");

		let health_value = u16::try_from(reference).expect("invalid health value");

		Self {
			current: health_value,
			max:     health_value,
		}
	}
}

/// Healthbar marker component.
#[derive(Component)]
pub(crate) struct HealthBar;

/// Information about entities being remembered.
#[derive(Reflect)]
pub(crate) struct Memory {
	/// The [`GridCoords`] the entity was last seen at.
	pub(crate) coords:    GridCoords,
	/// The turn the entity was last seen in.
	pub(crate) last_seen: Option<u64>,
}

/// Component for tracking which tiles this entity sees.
#[derive(Component, Reflect)]
pub(crate) struct Vision {
	/// List of currently visible tiles.
	pub(crate) tiles:  Vec<GridCoords>,
	/// Memory of seen entities not currently in vision.
	pub(crate) memory: HashMap<Entity, Memory>,
	/// How far the entity can see.
	pub(crate) range:  u8,
}

impl Vision {
	/// Create a new [`Vision`] with the given `range`.
	pub(crate) fn new(range: u8) -> Self {
		Self {
			tiles: Vec::new(),
			memory: HashMap::new(),
			range,
		}
	}
}

/// When an entity dies, it sends this event.
#[derive(Event)]
pub(crate) struct DeathEvent(Entity);

/// What an ability is targeting.
pub(crate) enum AbilityEventTarget {
	/// Either an entity.
	Entity(Entity),
	/// Or a tile.
	Tile(GridCoords),
}

/// Event that fires when an entity uses an ability.
#[derive(Event)]
pub(crate) struct AbilityEvent {
	/// The entity casting the ability.
	pub(crate) caster_entity: Entity,
	/// The ability being cast.
	pub(crate) ability:       AbilityId,
	/// The target of the ability.
	pub(crate) target:        AbilityEventTarget,
}

impl AbilityEvent {
	/// Creates an abilityevent.
	pub(crate) const fn new(
		caster_entity: Entity,
		ability: AbilityId,
		target: AbilityEventTarget,
	) -> Self {
		Self {
			caster_entity,
			ability,
			target,
		}
	}
}

/// What and how an Ability affects.
#[derive(Reflect, Clone)]
pub(crate) enum AbilityEffect {
	/// Damages enemies.
	Damage(u16),
	/// Heals caster.
	Healing(u16),
	/// Affects caster or targets with statuses.
	StatusEffect(StatusEffect),
	/// Teleports.
	Teleport,
	/// Charge.
	Charge(u16),
	/// Slam(Target movement)
	Slam(u16),
}

/// Holds animation data.
#[derive(Reflect, Clone)]
pub(crate) struct AbilityAnimation {
	/// Animation texture handle.
	texture:  Handle<Image>,
	/// Animation duration.
	duration: Duration,
	/// Size of the animation.
	scale:    f32,
	/// Texture atlas.
	atlas:    Option<(Handle<TextureAtlasLayout>, usize)>,
}

/// An ability an entity can perform.
#[derive(Reflect, Clone)]
pub(crate) struct Ability {
	/// Name of the ability.
	pub(crate) name:      String,
	/// The range in manhatten distance of the ability.
	pub(crate) range:     u8,
	/// If this ability is an AoE ability, and its radius, or targets a single entity.
	pub(crate) aoe:       Option<u8>,
	/// The effect of the ability.
	pub(crate) effect:    AbilityEffect,
	/// The Cooldown of the ability.
	cooldown:             Option<u8>,
	/// Icon to use.
	pub(crate) icon:      Option<TextureIcon>,
	/// Animation data.
	pub(crate) animation: Option<AbilityAnimation>,
}

impl Ability {
	/// Creates an ability.
	const fn new(
		name: String,
		range: u8,
		aoe: Option<u8>,
		effect: AbilityEffect,
		cooldown: Option<u8>,
		icon: Option<TextureIcon>,
		animation: Option<AbilityAnimation>,
	) -> Self {
		Self {
			name,
			range,
			aoe,
			effect,
			cooldown,
			icon,
			animation,
		}
	}

	/// Checks if ability is in manhatten range.
	fn in_manhatten_range(&self, origin: GridCoords, target: GridCoords) -> bool {
		let distance = util::manhatten_distance(target, origin);

		distance <= self.range.into()
	}

	/// Checks if ability is in euclidian range.
	pub(crate) fn in_euclidean_range(&self, origin: GridCoords, target: GridCoords) -> bool {
		let distance = util::euclidean_distance_raw(target, origin);

		distance.floor() <= self.range.into()
	}
}

/// The types of Statuseffects.
#[derive(Reflect, Copy, Clone, PartialEq, Eq)]
pub(crate) enum EffectType {
	/// Damages the target over time.
	Dot,
	/// Strengthen the Defense of the caster.
	DefensiveBuff,
	/// Strengthen the Attack of the caster.
	AttackBuff,
	/// Weakens the Defense of the target.
	DefensiveDebuff,
	/// Weakens the Attack of the target.
	AttackDebuff,
}

/// A Statuseffect.
#[derive(Reflect, Clone, PartialEq)]
pub(crate) struct StatusEffect {
	/// The name of the effect.
	pub(crate) name:        String,
	/// The power of the effect.
	pub(crate) power:       f32,
	/// How many turns the effect lasts.
	pub(crate) length:      u64,
	/// When the effect was cast.
	pub(crate) last_cast:   Option<u64>,
	/// What kind of effect this is.
	pub(crate) effect_type: EffectType,
}

impl StatusEffect {
	/// Creates a [`StatusEffect`].
	const fn new(name: String, power: f32, length: u64, effect_type: EffectType) -> Self {
		Self {
			name,
			power,
			length,
			last_cast: None,
			effect_type,
		}
	}

	/// Calculates how many turns are left for this effect.
	pub(crate) fn turns_left(&self, current_turn: u64) -> Option<u64> {
		self.last_cast.and_then(|last_cast| {
			let turns_left = (last_cast + self.length).saturating_sub(current_turn);

			(turns_left > 0).then_some(turns_left)
		})
	}
}

/// Currently active Statuseffects on an entity.
#[derive(Reflect, Clone, Component, Default)]
pub(crate) struct CurrentStatusEffects(pub(crate) Vec<StatusEffect>);

/// The ID of an ability.
#[derive(Clone, Copy, Hash, Reflect, PartialEq, Eq, Default, PartialOrd, Ord)]
pub(crate) struct AbilityId(u64);

/// An ability an entity can perform.
#[derive(Default, Reflect, Clone)]
pub(crate) struct SpellbookAbility {
	/// When this ability was last cast.
	pub(crate) last_cast: Option<u64>,
}

impl SpellbookAbility {
	/// Calculates how much cooldown is left for this ability.
	pub(crate) fn cooldown_left(&self, ability: &Ability, current_turn: u64) -> Option<u64> {
		self.last_cast.and_then(|last_cast| {
			let cooldown_left = (last_cast
				+ u64::from(
					ability
						.cooldown
						.expect("when ability has last cast it also needs to have a cooldown"),
				))
			.saturating_sub(current_turn);

			(cooldown_left > 0).then_some(cooldown_left)
		})
	}
}

/// The collection of abilities an entity can perform.
#[derive(Reflect, Component)]
pub(crate) struct Spellbook {
	/// The Id of a melee autoattack of an entity.
	pub(crate) autoattack_melee:  Option<AbilityId>,
	/// The Id of a ranged autoattack of an entity.
	pub(crate) autoattack_ranged: Option<AbilityId>,
	/// The Id of a charge attack of an entity.
	pub(crate) charge:            Option<AbilityId>,
	/// The Id of a charge attack of an entity.
	pub(crate) teleport:          Option<AbilityId>,
	/// All abilities an entity can cast.
	pub(crate) abilities:         HashMap<AbilityId, SpellbookAbility>,
}

impl Spellbook {
	/// Default spellbook for the player.
	fn default_player() -> Self {
		Self {
			autoattack_melee:  Some(AbilityId(0)),
			autoattack_ranged: Some(AbilityId(1)),
			charge:            None,
			teleport:          None,
			abilities:         HashMap::from_iter([
				(AbilityId(0), SpellbookAbility::default()),
				(AbilityId(1), SpellbookAbility::default()),
				(AbilityId(14), SpellbookAbility::default()),
			]),
		}
	}

	/// Default spellbook for base skeletons.
	fn base_skeleton() -> Self {
		Self {
			autoattack_melee:  Some(AbilityId(10)),
			autoattack_ranged: Some(AbilityId(11)),
			charge:            None,
			teleport:          None,
			abilities:         HashMap::from_iter([
				(AbilityId(10), SpellbookAbility::default()),
				(AbilityId(11), SpellbookAbility::default()),
			]),
		}
	}

	/// Default spellbook for mage skeletons.
	fn mage_skeleton() -> Self {
		Self {
			autoattack_melee:  None,
			autoattack_ranged: Some(AbilityId(12)),
			charge:            None,
			teleport:          None,
			abilities:         HashMap::from_iter([(AbilityId(12), SpellbookAbility::default())]),
		}
	}

	/// Default spellbook for warrior skeletons.
	fn warrior_skeleton() -> Self {
		Self {
			autoattack_melee:  Some(AbilityId(16)),
			autoattack_ranged: Some(AbilityId(11)),
			charge:            Some(AbilityId(13)),
			teleport:          None,
			abilities:         HashMap::from_iter([
				(AbilityId(16), SpellbookAbility::default()),
				(AbilityId(11), SpellbookAbility::default()),
				(AbilityId(13), SpellbookAbility::default()),
			]),
		}
	}

	/// Default spellbook for Necromancer.
	fn necromancer() -> Self {
		Self {
			autoattack_melee:  None,
			autoattack_ranged: Some(AbilityId(12)),
			charge:            None,
			teleport:          Some(AbilityId(4)),
			abilities:         HashMap::from_iter([
				(AbilityId(12), SpellbookAbility::default()),
				(AbilityId(4), SpellbookAbility::default()),
			]),
		}
	}
}

/// The currently active ability of an entity.
#[derive(Reflect, Component, Default)]
pub(crate) struct ActiveAbility(pub(crate) AbilityId);

/// Handles when an entity wants to cast an ability.
#[expect(
	clippy::needless_pass_by_value,
	clippy::too_many_lines,
	clippy::type_complexity,
	clippy::cast_possible_truncation,
	clippy::cast_sign_loss,
	clippy::as_conversions,
	clippy::cognitive_complexity
)]
pub(crate) fn handle_ability_event(
	mut commands: Commands<'_, '_>,
	mut turn: Query<'_, '_, &mut Turn>,
	abilities: Res<'_, Abilities>,
	mut entities_q: Query<
		'_,
		'_,
		(
			Entity,
			&Transform,
			&mut GridCoords,
			&mut Spellbook,
			&mut Sprite,
			&mut Health,
			&mut Vision,
			&mut CurrentStatusEffects,
			Has<Player>,
			Has<Enemy>,
		),
		Without<Dead>,
	>,
	mut ability_events: EventReader<'_, '_, AbilityEvent>,
	mut animation_state: ResMut<'_, TurnState>,
	level_cache: ResMut<'_, LevelCache>,
) {
	let mut turn = turn.single_mut();

	for ability_event in ability_events.read() {
		let (
			caster_entity,
			caster_transform,
			mut caster_grid_coord,
			mut spellbook,
			mut caster_sprite,
			mut health,
			_,
			mut caster_status_effect,
			caster_is_player,
			..,
		) = entities_q
			.get_mut(ability_event.caster_entity)
			.expect("caster not found");

		let spellbook_ability = spellbook
			.abilities
			.get_mut(&ability_event.ability)
			.expect("requested non-existing ability");

		let ability = abilities
			.0
			.get(&ability_event.ability)
			.expect("requested non-existing ability");

		if spellbook_ability.last_cast.is_some() {
			continue;
		}

		let (target_entity, mut power) = match ability.effect.clone() {
			AbilityEffect::Damage(power) => {
				let AbilityEventTarget::Entity(entity) = ability_event.target else {
					unreachable!("sent wrong event target")
				};

				let mut power_modifier = 1.;

				for effect in &caster_status_effect.0 {
					if effect.effect_type == EffectType::AttackBuff {
						power_modifier += effect.power;
					} else if effect.effect_type == EffectType::AttackDebuff {
						power_modifier -= effect.power;
					}
				}

				(entity, (f32::from(power) * power_modifier) as u16)
			}
			AbilityEffect::Healing(power) => {
				if ability.cooldown.is_some() {
					spellbook_ability.last_cast = Some(turn.0);
				}

				health.current = (health.current + power).min(health.max);

				turn.0 += 1;
				*animation_state = TurnState::EnemiesWaiting;

				continue;
			}
			AbilityEffect::Teleport => {
				let AbilityEventTarget::Tile(target_grid_coord) = ability_event.target else {
					unreachable!("sent teleport spell on an entity")
				};

				flip_sprite(*caster_grid_coord, target_grid_coord, &mut caster_sprite);

				caster_grid_coord.set_if_neq(target_grid_coord);

				if ability.cooldown.is_some() {
					spellbook_ability.last_cast = Some(turn.0);
				}

				commands.trigger_targets(ArrivedAtTile, caster_entity);

				if caster_is_player {
					turn.0 += 1;
				}

				*animation_state = TurnState::EnemiesWaiting;

				if let Some(ability_animation) = &ability.animation {
					if let Some(atlas) = &ability_animation.atlas {
						commands.spawn((
							AnimationAbility,
							SpriteBundle {
								texture: ability_animation.texture.clone(),
								transform: Transform {
									translation: utils::grid_coords_to_translation(
										target_grid_coord,
										IVec2::splat(GRID_SIZE),
									)
									.extend(caster_transform.translation.z),
									rotation:    Quat::IDENTITY,
									scale:       (Vec2::ONE * ability_animation.scale).extend(1.),
								},
								..SpriteBundle::default()
							},
							TextureAtlas {
								layout: atlas.0.clone(),
								index:  0,
							},
							Animation {
								timer:     Timer::from_seconds(
									(ability_animation.duration / atlas.1 as u32).as_secs_f32(),
									TimerMode::Repeating,
								),
								first:     0,
								last:      atlas.1,
								repeating: false,
								anchor:    None,
							},
						));
					} else {
						unimplemented!()
					}
				}

				continue;
			}
			AbilityEffect::StatusEffect(mut effect) => {
				if ability.cooldown.is_some() {
					spellbook_ability.last_cast = Some(turn.0);
				}

				match effect.effect_type {
					EffectType::DefensiveBuff | EffectType::AttackBuff => {
						effect.last_cast = Some(turn.0);

						let mut contains_effect = None;

						for (index, old_effect) in caster_status_effect.0.iter().enumerate() {
							if effect == *old_effect {
								contains_effect = Some(index);
							}
						}

						if let Some(index) = contains_effect {
							caster_status_effect.0.remove(index);
						}

						caster_status_effect.0.push(effect);

						turn.0 += 1;
						*animation_state = TurnState::EnemiesWaiting;

						continue;
					}
					EffectType::DefensiveDebuff | EffectType::AttackDebuff | EffectType::Dot => {
						let AbilityEventTarget::Entity(entity) = ability_event.target else {
							unreachable!("sent wrong event target")
						};

						(entity, 0)
					}
				}
			}
			AbilityEffect::Charge(power) | AbilityEffect::Slam(power) => {
				let AbilityEventTarget::Entity(entity) = ability_event.target else {
					unreachable!("sent wrong event target")
				};

				(entity, power)
			}
		};

		let mut all_enemies = HashMap::new();

		for (entity, _, grid_coord, _, _, _, _, _, _, is_enemy) in &entities_q {
			if is_enemy {
				all_enemies.insert(*grid_coord, entity);
			}
		}

		let [(
			caster_entity,
			caster_transform,
			caster_grid_coord,
			mut spellbook,
			mut sprite,
			_,
			_,
			_,
			caster_is_player,
			caster_is_enemy,
		), (
			_,
			target_transform,
			target_grid_coord,
			_,
			_,
			mut target_health,
			mut target_vision,
			mut target_status_effect,
			..,
		)] = entities_q.many_mut([ability_event.caster_entity, target_entity]);

		flip_sprite(*caster_grid_coord, *target_grid_coord, &mut sprite);

		if caster_is_player {
			target_vision.memory.insert(
				caster_entity,
				Memory {
					coords:    *caster_grid_coord,
					last_seen: None,
				},
			);
			turn.0 += 1;
			*animation_state = TurnState::PlayerBusy(PlayerBusy::Casting {
				camera: matches!(ability.effect, AbilityEffect::Charge(_)),
			});
		} else if caster_is_enemy {
			target_vision.memory.insert(
				caster_entity,
				Memory {
					coords:    *caster_grid_coord,
					last_seen: Some(turn.0),
				},
			);
			*animation_state = TurnState::EnemiesBusy;
		} else {
			unreachable!("entity has to be enemy or player");
		}

		if let AbilityEffect::Charge(_) = &ability.effect {
			let target = WalkGrid::new(
				IVec2::from(*caster_grid_coord).into(),
				IVec2::from(*target_grid_coord).into(),
			)
			.steps()
			.last()
			.unwrap()
			.0;
			let target = GridCoords::from(IVec2::from(target));
			let target = utils::grid_coords_to_translation(target, IVec2::splat(GRID_SIZE))
				.extend(caster_transform.translation.z);

			commands.entity(caster_entity).insert(Animator::new(
				Tween::new(
					EaseMethod::Linear,
					Duration::from_secs_f64(0.2),
					TransformPositionLens {
						start: caster_transform.translation,
						end:   target,
					},
				)
				.with_completed_event(0),
			));
		} else {
			let target = caster_transform.translation
				+ ((target_transform.translation.xy() - caster_transform.translation.xy())
					.normalize() * 5.)
					.extend(caster_transform.translation.z);

			commands.entity(caster_entity).insert(Animator::new(
				Tween::new(
					EaseMethod::Linear,
					Duration::from_secs_f64(0.1),
					TransformPositionLens {
						start: caster_transform.translation,
						end:   target,
					},
				)
				.then(
					Tween::new(
						EaseMethod::Linear,
						Duration::from_secs_f64(0.1),
						TransformPositionLens {
							start: target,
							end:   caster_transform.translation,
						},
					)
					.with_completed_event(0),
				),
			));

			if let AbilityEffect::Slam(slam_power) = &ability.effect {
				let grid_vec = *target_grid_coord - *caster_grid_coord;
				let grid_coords = grid_vec * GridCoords::new(4, 4);
				let potential_destination = *target_grid_coord + grid_coords;

				let path = WalkGrid::new(
					IVec2::from(*target_grid_coord).into(),
					IVec2::from(potential_destination).into(),
				)
				.map(|pos| GridCoords::from(IVec2::from(pos)));

				let mut slams_into = false;

				let mut destination = *target_grid_coord;
				let mut slammed_entity = None;

				for pos in path.skip(1) {
					if level_cache.walls.contains(&pos) || all_enemies.contains_key(&pos) {
						slams_into = true;
						if let Some(entity) = all_enemies.get(&pos) {
							slammed_entity = Some(entity);
						}
						break;
					}

					// reached last one
					if pos == potential_destination {
						destination = pos;
						break;
					}

					destination = pos;
				}

				if slams_into {
					power = *slam_power;
				} else {
					power = slam_power / 4;
				}

				let mut power_modifier = 1.;

				for effect in &target_status_effect.0 {
					if effect.effect_type == EffectType::DefensiveBuff {
						power_modifier -= effect.power;
					} else if effect.effect_type == EffectType::DefensiveDebuff {
						power_modifier += effect.power;
					}
				}

				if 0 != target_health
					.current
					.saturating_sub((f32::from(power) * power_modifier) as u16)
				{
					let target =
						utils::grid_coords_to_translation(destination, IVec2::splat(GRID_SIZE))
							.extend(target_transform.translation.z);

					commands.entity(target_entity).insert(Animator::new(
						Tween::new(
							EaseMethod::Linear,
							Duration::from_secs_f64(0.2),
							TransformPositionLens {
								start: target_transform.translation,
								end:   target,
							},
						)
						.with_completed_event(slammed_entity.map_or(0, |entity| entity.to_bits())),
					));
				}
			}
		}

		let to_enemy =
			(target_transform.translation.xy() - caster_transform.translation.xy()).normalize();
		let rotate_to_enemy = Quat::from_rotation_arc(Vec3::Y, to_enemy.extend(0.));
		if let Some(ability_animation) = &ability.animation {
			if let AbilityEffect::Damage(_) | AbilityEffect::Slam(_) | AbilityEffect::Charge(_) =
				&ability.effect
			{
				if let Some(atlas) = &ability_animation.atlas {
					commands.spawn((
						AnimationAbility,
						SpriteBundle {
							texture: ability_animation.texture.clone(),
							transform: Transform {
								translation: caster_transform.translation
									+ ((target_transform.translation.xy()
										- caster_transform.translation.xy())
										* 0.6)
										.extend(5.),
								rotation:    rotate_to_enemy,
								scale:       (Vec2::ONE * ability_animation.scale).extend(1.),
							},
							..SpriteBundle::default()
						},
						TextureAtlas {
							layout: atlas.0.clone(),
							index:  0,
						},
						Animation {
							timer:     Timer::from_seconds(
								(ability_animation.duration / atlas.1 as u32).as_secs_f32(),
								TimerMode::Repeating,
							),
							first:     0,
							last:      atlas.1,
							repeating: false,
							anchor:    None,
						},
					));
				} else {
					commands.spawn((
						AnimationAbility,
						SpriteBundle {
							texture: ability_animation.texture.clone(),
							transform: Transform {
								translation: caster_transform.translation.with_z(5.),
								rotation:    rotate_to_enemy,
								scale:       (Vec2::ONE * ability_animation.scale).extend(1.),
							},
							..SpriteBundle::default()
						},
						Animator::new(
							Tween::new(
								EaseMethod::Linear,
								ability_animation.duration,
								TransformPositionLens {
									start: caster_transform.translation,
									end:   target_transform.translation,
								},
							)
							.with_completed_event(0),
						),
					));
				}
			}
		}

		if ability.cooldown.is_some() {
			let spellbook_ability = spellbook
				.abilities
				.get_mut(&ability_event.ability)
				.expect("requested non-existing ability");

			spellbook_ability.last_cast = Some(turn.0);
		}

		if let AbilityEffect::StatusEffect(effect) = &ability.effect {
			let mut contains_effect = None;

			for (index, old_effect) in target_status_effect.0.iter().enumerate() {
				if effect == old_effect {
					contains_effect = Some(index);
				}
			}

			if let Some(index) = contains_effect {
				target_status_effect.0.remove(index);
			}

			let mut effect = effect.clone();
			effect.last_cast = Some(turn.0);

			target_status_effect.0.push(effect);

			continue;
		}

		let mut power_modifier = 1.;

		for effect in &target_status_effect.0 {
			if effect.effect_type == EffectType::DefensiveBuff {
				power_modifier -= effect.power;
			} else if effect.effect_type == EffectType::DefensiveDebuff {
				power_modifier += effect.power;
			}
		}

		target_health.current = target_health
			.current
			.saturating_sub((f32::from(power) * power_modifier) as u16);
	}
}

/// When an Entity with a healthbar is spawned we spawn a mesh to represent it.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn spawn_healthbar(
	added: Query<'_, '_, (Entity, Option<&Enemy>), Added<Health>>,
	mut commands: Commands<'_, '_>,
	mut meshes: ResMut<'_, Assets<Mesh>>,
	mut materials: ResMut<'_, Assets<ColorMaterial>>,
) {
	for (entity, is_enemy) in &added {
		commands.entity(entity).with_children(|entity| {
			entity.spawn((
				MaterialMesh2dBundle {
					mesh: meshes.add(Rectangle::new(32., 5.)).into(),
					material: materials.add(Color::from(RED)),
					transform: Transform::from_translation(Vec3::new(0., 20., 0.1)),
					visibility: if is_enemy.is_some() {
						Visibility::Hidden
					} else {
						Visibility::default()
					},
					..MaterialMesh2dBundle::default()
				},
				HealthBar,
				Name::new("Healthbar"),
			));
		});
	}
}

/// Update size and position of the Healthbar.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn update_healthbar(
	changed: Query<'_, '_, (Entity, &Health, &Children, Option<&Enemy>), Changed<Health>>,
	mut transforms: Query<'_, '_, (&mut Visibility, &mut Transform), With<HealthBar>>,
	mut death: EventWriter<'_, DeathEvent>,
) {
	for (entity, health, children, enemy) in &changed {
		if health.current == 0 {
			death.send(DeathEvent(entity));
			continue;
		}

		for child in children {
			if let Ok((mut visibility, mut transform)) = transforms.get_mut(*child) {
				let percentage = f32::from(health.current) / f32::from(health.max);
				transform.scale.x = percentage;
				transform.translation.x = -16. * (1. - percentage);

				if enemy.is_some() {
					if percentage < 1. {
						visibility.set_if_neq(Visibility::Inherited);
					} else {
						visibility.set_if_neq(Visibility::Hidden);
					}
				}
			}
		}
	}
}

/// Ticks abilities currently on cooldown.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn tick_cooldowns(
	turn: Query<'_, '_, &Turn>,
	abilities: Res<'_, Abilities>,
	mut spellbooks: Query<'_, '_, &mut Spellbook>,
) {
	if let Ok(turn) = turn.get_single() {
		for mut spellbook in &mut spellbooks {
			for (id, spellbook_ability) in &mut spellbook.abilities {
				let ability = abilities.0.get(id).expect("found non-existing ability");

				if spellbook_ability.cooldown_left(ability, turn.0).is_none() {
					spellbook_ability.last_cast = None;
				}
			}
		}
	}
}

/// Ticks status effects currently on cooldown.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn tick_status_effects(
	turn: Query<'_, '_, &Turn, Changed<Turn>>,
	mut status_effects: Query<'_, '_, (&mut CurrentStatusEffects, &mut Health)>,
) {
	if let Ok(turn) = turn.get_single() {
		for (mut effects, mut health) in &mut status_effects {
			for effect in &effects.0 {
				#[expect(
					clippy::as_conversions,
					clippy::cast_possible_truncation,
					clippy::cast_sign_loss
				)]
				if effect.effect_type == EffectType::Dot {
					health.current = health.current.saturating_sub(effect.power as u16);
				}
			}

			effects
				.0
				.retain(|effect| effect.turns_left(turn.0).is_some());
		}
	}
}

/// Handle when a death event was sent.
#[expect(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::too_many_lines,
	clippy::type_complexity
)]
pub(crate) fn death(
	mut commands: Commands<'_, '_>,
	textures: Res<'_, Textures>,
	mut deaths: EventReader<'_, '_, DeathEvent>,
	mut player_q: Query<'_, '_, (&Vision, &mut Spellbook), With<Player>>,
	mut animations: Query<
		'_,
		'_,
		(
			Entity,
			&EntityIid,
			&GridCoords,
			&mut Transform,
			&mut TextureAtlas,
			&mut Animation,
			Option<&mut Drops>,
			Option<&Enemy>,
			Option<&Boss>,
			Has<Player>,
		),
	>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	layers: Query<'_, '_, (Entity, &LayerMetadata)>,
	ldtk_projects: Query<'_, '_, Entity, With<Handle<LdtkProject>>>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut game_state: ResMut<'_, GameState>,
	mut turn_state: ResMut<'_, TurnState>,
	mut level_selection: ResMut<'_, LevelSelection>,
) {
	let Ok((player_vision, mut player_spellbook)) = player_q.get_single_mut() else {
		return;
	};

	if deaths.is_empty() {
		return;
	}

	let tile_map_size = tile_map_size.iter().next().unwrap();

	for event in deaths.read() {
		let (
			entity,
			entity_iid,
			grid_coords,
			mut transform,
			mut atlas,
			mut animation,
			drops,
			enemy,
			boss,
			player,
		) = animations.get_mut(event.0).unwrap();

		let death_animation = if let Some(enemy) = enemy {
			game_state.enemies.retain(|(enemy, _)| *enemy != entity);
			enemy.death_animation()
		} else if player {
			commands.entity(ldtk_projects.single()).insert(Respawn);
			game_state.doors.clear();
			game_state.enemies.clear();
			game_state.objects.clear();
			game_state.player_items.clear();
			game_state.visited_tiles.clear();
			*turn_state = TurnState::PlayerWaiting;
			*level_selection =
				LevelSelection::Iid(LevelIid::new("32dd4990-25d0-11ef-be0e-2bd40eab6b07"));
			return;
		} else {
			unreachable!("player can't be enemy and visa versa")
		};

		transform.translation.z -= 0.1;

		*animation = death_animation;
		atlas.index = animation.first;

		commands.entity(event.0).insert(Dead);

		let mut entity_commands = commands.entity(event.0);
		entity_commands.despawn_descendants();

		for object in drops
			.unwrap()
			.0
			.drain(..)
			.map(|drop| Object::from_str(&drop).unwrap())
		{
			// This has been dropped and picked up before. So don't drop it again!
			if *game_state
				.objects
				.get(&ItemSource::Loot(entity_iid.clone()))
				.unwrap()
				.get(&object)
				.unwrap()
			{
				continue;
			}

			#[expect(clippy::shadow_same)]
			let mut grid_coords = *grid_coords;

			if level_cache.objects.get(&grid_coords).is_some() {
				let mut checked = HashSet::new();
				let mut next = VecDeque::new();
				checked.insert(grid_coords);
				next.push_back(grid_coords);

				'outer: loop {
					let Some(new_grid_coords) = next.pop_front() else {
						panic!("no empty floor found to dop items")
					};

					for new_grid_coords in
						OrderedNeighbors::new(new_grid_coords.into(), *tile_map_size)
							.map(GridCoords::from)
					{
						if !checked.insert(new_grid_coords) {
							continue;
						}

						if level_cache.walls.get(&new_grid_coords).is_some() {
							continue;
						}

						if level_cache.objects.get(&new_grid_coords).is_none() {
							grid_coords = new_grid_coords;
							break 'outer;
						}

						next.push_back(new_grid_coords);
					}
				}
			}

			let entity = commands
				.spawn((
					Name::new(object.name()),
					object,
					grid_coords,
					SpriteBundle {
						sprite: Sprite {
							custom_size: Some(Vec2::new(16., 16.)),
							rect: Some(object.texture_rect()),
							..Sprite::default()
						},
						texture: textures.props.clone(),
						visibility: if player_vision.tiles.contains(&grid_coords) {
							Visibility::default()
						} else {
							Visibility::Hidden
						},
						..SpriteBundle::default()
					},
				))
				.id();
			commands
				.entity(
					layers
						.iter()
						.find(|(_, layer)| layer.identifier == "Objects")
						.unwrap()
						.0,
				)
				.add_child(entity);

			assert!(
				level_cache
					.objects
					.insert(
						grid_coords,
						(entity, object, ItemSource::Loot(entity_iid.clone()))
					)
					.is_none(),
				"found object at this position already",
			);
		}

		if let Some(enemy) = enemy {
			if let Some(boss) = boss {
				if boss.0 {
					match enemy {
						Enemy::Warrior => {
							player_spellbook.charge = Some(AbilityId(13));
							assert!(player_spellbook
								.abilities
								.insert(AbilityId(13), SpellbookAbility { last_cast: None })
								.is_none());
						}
						Enemy::Mage => {
							assert!(player_spellbook
								.abilities
								.insert(AbilityId(2), SpellbookAbility { last_cast: None })
								.is_none());
						}
						_ => {
							unimplemented!()
						}
					}
				}
			}
		}
	}
}
