//! Enemy functionality.

use std::ops::DerefMut;
use std::time::Duration;

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy::utils::hashbrown::HashMap;
use bevy::utils::HashSet;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::map::TilemapSize;
use bevy_ecs_tilemap::tiles::TilePos;
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween};
use line_drawing::WalkGrid;
use pathfinding::prelude::*;

use super::{
	Abilities, AbilityEvent, AbilityEventTarget, AbilityId, CurrentStatusEffects, Dead, Health,
	Memory, Player, Spellbook, SpellbookAbility, Vision,
};
use crate::animation::Animation;
use crate::util::{self, flip_sprite, OrderedNeighbors};
use crate::{Destination, Drops, GameState, LevelCache, TurnState, GRID_SIZE};

/// Enemy marker component.
#[derive(Clone, Component, Copy, PartialEq, Eq, Reflect)]
pub(crate) enum Enemy {
	/// Basic skeleton.
	Base,
	/// Skeleton caster.
	Mage,
	/// Skeleton warrior.
	Warrior,
	/// Necromancer.
	Necromancer,
}

impl Enemy {
	/// Return idle [`Animation`].
	pub(crate) fn idle_animation(self) -> Animation {
		match self {
			Self::Base | Self::Mage | Self::Warrior | Self::Necromancer => Animation {
				timer:     Timer::from_seconds(0.25, TimerMode::Repeating),
				first:     0,
				last:      3,
				repeating: true,
				anchor:    None,
			},
		}
	}

	/// Return walking [`Animation`].
	pub(crate) fn walking_animation(self) -> Animation {
		match self {
			Self::Base => Animation {
				timer:     Timer::from_seconds(0.1, TimerMode::Repeating),
				first:     8,
				last:      13,
				repeating: true,
				anchor:    None,
			},
			Self::Mage => Animation {
				timer:     Timer::from_seconds(0.1, TimerMode::Repeating),
				first:     6,
				last:      11,
				repeating: true,
				anchor:    Some(Anchor::Custom(Vec2::new(
					0.,
					-(1. / 33. * ((33. - 32.) / 2.)),
				))),
			},
			Self::Warrior => Animation {
				timer:     Timer::from_seconds(0.1, TimerMode::Repeating),
				first:     9,
				last:      14,
				repeating: true,
				anchor:    None,
			},
			Enemy::Necromancer => Animation {
				timer:     Timer::from_seconds(0.1, TimerMode::Repeating),
				first:     9,
				last:      14,
				repeating: true,
				anchor:    None,
			},
		}
	}

	/// Return death [`Animation`].
	pub(crate) fn death_animation(self) -> Animation {
		match self {
			Self::Base => Animation {
				timer:     Timer::from_seconds(0.125, TimerMode::Repeating),
				first:     16,
				last:      23,
				repeating: false,
				anchor:    Some(Anchor::Custom(Vec2::new(
					0.,
					-(1. / 48. * ((48. - 32.) / 2.)),
				))),
			},
			Self::Mage => Animation {
				timer:     Timer::from_seconds(0.125, TimerMode::Repeating),
				first:     12,
				last:      17,
				repeating: false,
				anchor:    Some(Anchor::Custom(Vec2::new(
					0.,
					-(1. / 56. * ((56. - 32.) / 2.)),
				))),
			},
			Self::Warrior => Animation {
				timer:     Timer::from_seconds(0.125, TimerMode::Repeating),
				first:     18,
				last:      23,
				repeating: false,
				anchor:    Some(Anchor::Custom(Vec2::new(
					0.,
					-(1. / 46. * ((46. - 32.) / 2.)),
				))),
			},
			Self::Necromancer => Animation {
				timer:     Timer::from_seconds(0.125, TimerMode::Repeating),
				first:     18,
				last:      23,
				repeating: false,
				anchor:    None,
			},
		}
	}
}

/// If an entity is a boss or not.
#[derive(Clone, Component, Copy, PartialEq, Eq, Default)]
pub(crate) struct Boss(pub(crate) bool);

impl From<&EntityInstance> for Boss {
	fn from(entity_instance: &EntityInstance) -> Self {
		let is_boss = *entity_instance
			.get_bool_field("Boss")
			.expect("expected entity to have non-nullable `Health` bool field");

		Self(is_boss)
	}
}

/// Base Skeleton bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct BaseSkeletonBundle {
	/// Enemy marker component.
	enemy:               Enemy,
	/// Health of the enemy.
	#[from_entity_instance]
	health:              Health,
	/// Abilities the enemy can perform.
	abilities:           Spellbook,
	#[from_entity_instance]
	/// A list of items this enemy drops on death.
	drops:               Drops,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// enemy grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
	/// Animation.
	animation:           Animation,
	/// Which tiles the enemy can see.
	vision:              Vision,
	/// Active [`StatusEffects`].
	effects:             CurrentStatusEffects,
}

impl Default for BaseSkeletonBundle {
	fn default() -> Self {
		Self {
			enemy:               Enemy::Base,
			health:              Health::default(),
			abilities:           Spellbook::base_skeleton(),
			drops:               Drops::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Enemy::Base.idle_animation(),
			vision:              Vision::new(4),
			effects:             CurrentStatusEffects::default(),
		}
	}
}

/// Mage Skeleton bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct MageSkeletonBundle {
	/// Enemy marker component.
	enemy:               Enemy,
	/// If this is a boss.
	#[from_entity_instance]
	boss:                Boss,
	/// Health of the enemy.
	#[from_entity_instance]
	health:              Health,
	/// Abilities the enemy can perform.
	abilities:           Spellbook,
	#[from_entity_instance]
	/// A list of items this enemy drops on death.
	drops:               Drops,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// enemy grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
	/// Animation.
	animation:           Animation,
	/// Which tiles the enemy can see.
	vision:              Vision,
	/// Active [`StatusEffects`].
	effects:             CurrentStatusEffects,
}

impl Default for MageSkeletonBundle {
	fn default() -> Self {
		Self {
			enemy:               Enemy::Mage,
			boss:                Boss::default(),
			health:              Health::default(),
			abilities:           Spellbook::mage_skeleton(),
			drops:               Drops::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Enemy::Mage.idle_animation(),
			vision:              Vision::new(4),
			effects:             CurrentStatusEffects::default(),
		}
	}
}

/// Warrior skeleton bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct WarriorSkeletonBundle {
	/// Enemy marker component.
	enemy:               Enemy,
	/// If this is a boss.
	#[from_entity_instance]
	boss:                Boss,
	/// Health of the enemy.
	#[from_entity_instance]
	health:              Health,
	/// Abilities the enemy can perform.
	abilities:           Spellbook,
	#[from_entity_instance]
	/// A list of items this enemy drops on death.
	drops:               Drops,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// enemy grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
	/// Animation.
	animation:           Animation,
	/// Which tiles the enemy can see.
	vision:              Vision,
	/// Active [`StatusEffects`].
	effects:             CurrentStatusEffects,
}

impl Default for WarriorSkeletonBundle {
	fn default() -> Self {
		Self {
			enemy:               Enemy::Warrior,
			boss:                Boss::default(),
			health:              Health::default(),
			abilities:           Spellbook::warrior_skeleton(),
			drops:               Drops::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Enemy::Warrior.idle_animation(),
			vision:              Vision::new(4),
			effects:             CurrentStatusEffects::default(),
		}
	}
}

/// Warrior skeleton bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct NecromancerEnemyBundle {
	/// Enemy marker component.
	enemy:               Enemy,
	/// Health of the enemy.
	#[from_entity_instance]
	health:              Health,
	/// Abilities the enemy can perform.
	abilities:           Spellbook,
	#[from_entity_instance]
	/// A list of items this enemy drops on death.
	drops:               Drops,
	/// Sprite bundle.
	#[sprite_sheet_bundle]
	sprite_sheet_bundle: LdtkSpriteSheetBundle,
	/// enemy grid coordinates.
	#[grid_coords]
	grid_coords:         GridCoords,
	/// Animation.
	animation:           Animation,
	/// Which tiles the enemy can see.
	vision:              Vision,
	/// Active [`StatusEffects`].
	effects:             CurrentStatusEffects,
}

impl Default for NecromancerEnemyBundle {
	fn default() -> Self {
		Self {
			enemy:               Enemy::Necromancer,
			health:              Health::default(),
			abilities:           Spellbook::necromancer(),
			drops:               Drops::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Enemy::Warrior.idle_animation(),
			vision:              Vision::new(4),
			effects:             CurrentStatusEffects::default(),
		}
	}
}

/// Moves enemies when its their turn.
#[expect(
	clippy::cognitive_complexity,
	clippy::needless_pass_by_value,
	clippy::type_complexity,
	clippy::too_many_arguments,
	clippy::too_many_lines
)]
pub(crate) fn move_enemies(
	mut commands: Commands<'_, '_>,
	abilities: Res<'_, Abilities>,
	mut game_state: ResMut<'_, GameState>,
	level_cache: ResMut<'_, LevelCache>,
	mut turn_state: ResMut<'_, TurnState>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	player: Query<'_, '_, (Entity, &GridCoords, &Vision), (Without<Enemy>, With<Player>)>,
	mut world_enemies: Query<
		'_,
		'_,
		(
			Entity,
			&Enemy,
			&GridCoords,
			&Transform,
			&mut Sprite,
			&Spellbook,
			&mut TextureAtlas,
			&mut Animation,
			&mut Visibility,
			&mut Vision,
		),
		(Without<Player>, Without<Dead>),
	>,
	mut cast_ability: EventWriter<'_, AbilityEvent>,
) {
	let GameState { enemies, doors, .. } = game_state.deref_mut();

	let mut all_enemies = HashMap::new();

	for (entity, _, grid_coord, ..) in &world_enemies {
		all_enemies.insert(*grid_coord, entity);
	}

	let all_enemy_pos: Vec<GridCoords> = all_enemies.keys().copied().collect();

	for (enemy, ready) in enemies.iter_mut().filter(|(_, ready)| *ready) {
		*ready = false;

		let tile_map_size = tile_map_size.iter().next().unwrap();
		let (player_entity, player_pos, player_vision) = player.single();
		let (
			enemy_entity,
			enemy_type,
			enemy_pos,
			enemy_transform,
			mut enemy_sprite,
			enemy_spellbook,
			mut enemy_atlas,
			mut enemy_animation,
			mut enemy_visibility,
			mut enemy_vision,
		) = world_enemies
			.get_mut(*enemy)
			.expect("enemy for its turn not found");

		let old_player_pos = if enemy_vision.tiles.contains(player_pos) {
			// If we see the player, update player position.
			enemy_vision.memory.insert(
				player_entity,
				Memory {
					coords:    *player_pos,
					last_seen: None,
				},
			);
			player_pos
		} else if let Some(old_player_pos) = enemy_vision.memory.get(&player_entity) {
			// If we reached the memorized player position but the player wasn't here, delete the
			// memory and stop.
			if enemy_pos == &old_player_pos.coords {
				enemy_vision.memory.remove(&player_entity).unwrap();
				continue;
			}

			&old_player_pos.coords
		} else {
			continue;
		};

		flip_sprite(*enemy_pos, *old_player_pos, &mut enemy_sprite);

		let path = match enemy_type {
			Enemy::Base | Enemy::Warrior => {
				if let Some((path, _)) = astar(
					enemy_pos,
					|grid_pos| {
						OrderedNeighbors::new((*grid_pos).into(), *tile_map_size)
							.filter_map(|pos| {
								let walkable = level_cache.destination(
									doors,
									*enemy_pos,
									pos.into(),
									&all_enemy_pos,
								);
								matches!(walkable, Destination::Walkable).then_some((pos.into(), 1))
							})
							.collect::<Vec<_>>()
					},
					|start| util::euclidean_distance(*old_player_pos, *start),
					|current_pos| current_pos == old_player_pos,
				) {
					path
				} else if let Some((path, _)) = astar(
					enemy_pos,
					|grid_pos| {
						OrderedNeighbors::new((*grid_pos).into(), *tile_map_size)
							.filter_map(|pos| {
								let walkable = level_cache.destination(
									doors,
									*enemy_pos,
									pos.into(),
									&all_enemy_pos,
								);
								matches!(walkable, Destination::Walkable | Destination::Enemy)
									.then_some((pos.into(), 1))
							})
							.collect::<Vec<_>>()
					},
					|start| util::euclidean_distance(*old_player_pos, *start),
					|current_pos| current_pos == old_player_pos,
				) {
					path
				} else {
					continue;
				}
			}
			Enemy::Mage | Enemy::Necromancer => {
				let ability_id = enemy_spellbook.autoattack_ranged.expect(
					"mage skeleton has to have a ranged autoattack
				",
				);

				let distance = util::euclidean_distance(*player_pos, *enemy_pos);

				#[expect(clippy::as_conversions, clippy::cast_lossless)]
				let in_range = distance
					- abilities
						.0
						.get(&ability_id)
						.expect("abilityid has to exist")
						.range as i32;

				let mut fov = true;

				let path = WalkGrid::new(
					IVec2::from(*enemy_pos).into(),
					IVec2::from(*player_pos).into(),
				)
				.steps()
				.map(|(start, end)| {
					(
						GridCoords::from(IVec2::from(start)),
						GridCoords::from(IVec2::from(end)),
					)
				});

				for (start, end) in path.skip(1) {
					if level_cache.walls.contains(&start) {
						fov = false;
						break;
					}

					// reached last one
					if end == *player_pos {
						break;
					}
				}

				let on_cooldown = enemy_spellbook
					.abilities
					.get(&ability_id)
					.expect("abilityid has to exist")
					.last_cast
					.is_some();

				// if spell is off cooldown and we are in range and can see the player...
				if !on_cooldown && in_range <= 0 && fov {
					cast_ability.send(AbilityEvent::new(
						enemy_entity,
						ability_id,
						AbilityEventTarget::Entity(player_entity),
					));
					return;
				}

				if let Some(ability_id) = enemy_spellbook.teleport {
					let ability = enemy_spellbook
						.abilities
						.get(&ability_id)
						.expect("abilityid has to exist");

					if distance < 2 && ability.last_cast.is_none() {
						let mut frontier = vec![*enemy_pos];
						let mut reached = HashSet::new();
						reached.insert(*enemy_pos);

						while let Some(current) = frontier.pop() {
							for (next, _) in
								OrderedNeighbors::new(TilePos::from(current), *tile_map_size)
									.filter_map(|pos| {
										let walkable = level_cache.destination(
											doors,
											*enemy_pos,
											pos.into(),
											&all_enemy_pos,
										);
										(matches!(
											walkable,
											Destination::Walkable | Destination::Enemy
										) && pos != TilePos::from(*player_pos))
										.then_some((pos, 1))
									}) {
								if util::euclidean_distance(next.into(), *enemy_pos)
									<= abilities
										.0
										.get(&ability_id)
										.expect("id has to exist")
										.range
										.into() && enemy_vision.tiles.contains(&next.into())
									&& !reached.contains(&GridCoords::from(next))
								{
									frontier.push(GridCoords::from(next));
								}
								reached.insert(GridCoords::from(next));
							}
						}

						let mut furthest = *enemy_pos;
						let mut max_range = 0;

						for target in reached {
							let distance = util::euclidean_distance(target, *player_pos);
							if distance > max_range {
								furthest = target;
								max_range = distance;
							}
						}

						if furthest != *enemy_pos {
							cast_ability.send(AbilityEvent::new(
								*enemy,
								ability_id,
								AbilityEventTarget::Tile(furthest),
							));

							return;
						}
					}
				}

				// if we are JUST in range...
				if in_range == 0 {
					continue;

				// if we are closer than than we need to be ...
				} else if in_range < 0 {
					let possible_moves = [
						(0, -1),  // Up
						(0, 1),   // Down
						(-1, 0),  // Left
						(1, 0),   // Right
						(-1, -1), // Up-Left (Diagonal)
						(1, -1),  // Up-Right (Diagonal)
						(-1, 1),  // Down-Left (Diagonal)
						(1, 1),   // Down-Right (Diagonal)
					];

					let mut best_move = *enemy_pos;
					let mut max_distance = 0;

					for mv in possible_moves {
						let new_pos = GridCoords::new(enemy_pos.x + mv.0, enemy_pos.y + mv.1);

						// Ensure the move stays within the grid boundaries
						if let Destination::Walkable =
							level_cache.destination(doors, *enemy_pos, new_pos, &all_enemy_pos)
						{
							// Calculate the distance from the player to the new position
							let dist = util::euclidean_distance(new_pos, *player_pos);

							// If this move increases the distance from the player, it's a better
							// move
							if dist > max_distance {
								max_distance = dist;
								best_move = new_pos;
							}
						}
					}

					vec![*enemy_pos, best_move]
				}
				// If we are not in range
				else if let Some((path, _)) = astar(
					enemy_pos,
					|grid_pos| {
						OrderedNeighbors::new((*grid_pos).into(), *tile_map_size)
							.filter_map(|pos| {
								let walkable = level_cache.destination(
									doors,
									*enemy_pos,
									pos.into(),
									&all_enemy_pos,
								);
								matches!(walkable, Destination::Walkable).then_some((pos.into(), 1))
							})
							.collect::<Vec<_>>()
					},
					|start| util::euclidean_distance(*old_player_pos, *start),
					|current_pos| current_pos == old_player_pos,
				) {
					path
				} else {
					continue;
				}
			}
		};

		let mut destination = *path.get(1).expect("found empty path");

		match enemy_type {
			Enemy::Base | Enemy::Warrior => {
				let (ability_id, _) = enemy_spellbook
					.abilities
					.get_key_value(
						&enemy_spellbook
							.autoattack_melee
							.expect("a melee enemy has to have a melee autoattack"),
					)
					.expect("there has to be a melee autoattack");

				if old_player_pos == player_pos {
					if destination == *player_pos {
						cast_ability.send(AbilityEvent::new(
							enemy_entity,
							*ability_id,
							AbilityEventTarget::Entity(player_entity),
						));
						return;
					}

					if let Some(ability_id) = enemy_spellbook.charge {
						let path = WalkGrid::new(
							IVec2::from(*enemy_pos).into(),
							IVec2::from(*player_pos).into(),
						)
						.steps()
						.map(|(start, end)| {
							(
								GridCoords::from(IVec2::from(start)),
								GridCoords::from(IVec2::from(end)),
							)
						});

						if enemy_spellbook
							.abilities
							.get(&ability_id)
							.expect("abilityid has to exist")
							.last_cast
							.is_none()
						{
							for (start, end) in path.skip(1) {
								if level_cache.walls.contains(&start)
									|| all_enemies.contains_key(&start)
								{
									break;
								}

								// reached last one
								if end == *player_pos {
									destination = start;
									cast_ability.send(AbilityEvent::new(
										enemy_entity,
										ability_id,
										AbilityEventTarget::Entity(player_entity),
									));

									*turn_state = TurnState::EnemiesBusy;

									break;
								}
							}
						}
					}

					if all_enemies.contains_key(&destination) {
						if let Some(autoattack_ranged) = &enemy_spellbook.autoattack_ranged {
							if let Some((ability_id, _)) =
								enemy_spellbook.abilities.get_key_value(autoattack_ranged)
							{
								cast_ability.send(AbilityEvent::new(
									enemy_entity,
									*ability_id,
									AbilityEventTarget::Entity(player_entity),
								));
							}
							return;
						}
					}
				}
			}
			_ => {}
		}

		if all_enemies.contains_key(&destination) {
			continue;
		}

		if player_vision.tiles.contains(&destination) {
			enemy_visibility.set_if_neq(Visibility::Inherited);
		}

		let mut enemy_commands = commands.entity(enemy_entity);

		enemy_commands.insert(Animator::new(
			Tween::new(
				EaseMethod::Linear,
				Duration::from_millis(250),
				TransformPositionLens {
					start: enemy_transform.translation,
					end:   utils::grid_coords_to_translation(destination, IVec2::splat(GRID_SIZE))
						.extend(enemy_transform.translation.z),
				},
			)
			.with_completed_event(0),
		));

		*enemy_animation = enemy_type.walking_animation();
		enemy_atlas.index = enemy_animation.first;

		*turn_state = TurnState::EnemiesBusy;
		return;
	}

	game_state
		.enemies
		.iter_mut()
		.for_each(|(_, ready)| *ready = true);
	*turn_state = TurnState::PlayerWaiting;
}

/// When enemies spawn that are bosses we add any additional spells they might have.
pub(crate) fn add_boss_abilities(
	mut added_q: Query<'_, '_, (&Enemy, &Boss, &mut Spellbook), Added<Boss>>,
) {
	for (enemy, boss, mut spellbook) in &mut added_q {
		if boss.0 {
			match enemy {
				Enemy::Mage => {
					spellbook.teleport = Some(AbilityId(4));
					spellbook
						.abilities
						.insert(AbilityId(4), SpellbookAbility { last_cast: None });
				}
				_ => {}
			}
		}
	}
}

/// When enemies change transform we updat their gridcoords.
pub(crate) fn change_enemy_gridcoords(
	mut changed_q: Query<'_, '_, (&mut GridCoords, &Transform), (Changed<Transform>, With<Enemy>)>,
) {
	for (mut grid_coords, transform) in &mut changed_q {
		grid_coords.set_if_neq(utils::translation_to_grid_coords(
			transform.translation.truncate(),
			IVec2::new(GRID_SIZE, GRID_SIZE),
		));
	}
}
