//! Enemy functionality.

use std::ops::DerefMut;
use std::time::Duration;

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::map::TilemapSize;
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween};
use pathfinding::prelude::astar;

use super::{AbilityEvent, Health, Player, Spellbook, Vision};
use crate::animation::Animation;
use crate::gameplay::AbilityEventTarget;
use crate::util::{self, flip_sprite, OrderedNeighbors};
use crate::{Destination, Drops, GameState, LevelCache, TurnState, GRID_SIZE};

/// Enemy marker component.
#[derive(Default, Component)]
pub(crate) struct Enemy;

/// Enemy bundle.
#[derive(Bundle, LdtkEntity)]
pub(crate) struct EnemyBundle {
	/// enemy marker component.
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
}

impl Default for EnemyBundle {
	fn default() -> Self {
		Self {
			enemy:               Enemy,
			health:              Health::default(),
			abilities:           Spellbook::default_enemy(),
			drops:               Drops::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Self::idle_animation(),
			vision:              Vision::new(2),
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

/// Moves enemies when its their turn.
#[allow(
	clippy::needless_pass_by_value,
	clippy::type_complexity,
	clippy::too_many_arguments
)]
pub(crate) fn move_enemies(
	mut commands: Commands<'_, '_>,
	mut game_state: ResMut<'_, GameState>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut turn_state: ResMut<'_, TurnState>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	player: Query<'_, '_, (Entity, &GridCoords, &Vision), (Without<Enemy>, With<Player>)>,
	mut world_enemies: Query<
		'_,
		'_,
		(
			Entity,
			&GridCoords,
			&Transform,
			&mut Sprite,
			&Spellbook,
			&mut TextureAtlas,
			&mut Animation,
			&mut Visibility,
			&mut Vision,
		),
		(With<Enemy>, Without<Player>),
	>,
	mut cast_ability: EventWriter<'_, AbilityEvent>,
) {
	let GameState { enemies, doors, .. } = game_state.deref_mut();

	for (enemy, ready) in enemies.iter_mut().filter(|(_, ready)| *ready) {
		*ready = false;

		let tile_map_size = tile_map_size.single();
		let (player_entity, player_pos, player_vision) = player.single();
		let (
			enemy_entity,
			enemy_pos,
			enemy_transform,
			mut enemy_sprite,
			spellbook,
			mut enemy_atlas,
			mut enemy_animation,
			mut enemy_visibility,
			mut enemy_vision,
		) = world_enemies
			.get_mut(*enemy)
			.expect("enemy for its turn not found");

		let old_player_pos = if enemy_vision.tiles.contains(player_pos) {
			// If we see the player, update player position.
			enemy_vision.memory.insert(player_entity, *player_pos);
			player_pos
		} else if let Some(old_player_pos) = enemy_vision.memory.get(&player_entity) {
			// If we reached the memorized player position but the player wasn't here, delete the
			// memory and stop.
			if enemy_pos == old_player_pos {
				enemy_vision.memory.remove(&player_entity).unwrap();
				continue;
			}

			old_player_pos
		} else {
			continue;
		};

		flip_sprite(*enemy_pos, *old_player_pos, &mut enemy_sprite);

		let Some((path, _)) = astar(
			enemy_pos,
			|grid_pos| {
				OrderedNeighbors::new((*grid_pos).into(), *tile_map_size)
					.filter_map(|pos| {
						let walkable = level_cache.destination(doors, *enemy_pos, pos.into());
						matches!(walkable, Destination::Walkable).then_some((pos.into(), 1))
					})
					.collect::<Vec<_>>()
			},
			|start| util::euclidean_distance(*old_player_pos, *start),
			|current_pos| current_pos == old_player_pos,
		) else {
			continue;
		};

		let destination = path.get(1).expect("found empty path");

		let spell = spellbook
				.0
				.get(&0)
				.expect("there has to be at least one spell in a spellbook").clone();

		if old_player_pos == player_pos && destination == player_pos {
			cast_ability.send(AbilityEvent::new(
				enemy_entity,
				spell.id,
				AbilityEventTarget::Entity(player_entity),

			));
			return;
		}

		if player_vision.tiles.contains(destination) {
			enemy_visibility.set_if_neq(Visibility::Inherited);
		}

		let mut enemy_commands = commands.entity(enemy_entity);
		enemy_commands.insert(Animator::new(
			Tween::new(
				EaseMethod::Linear,
				Duration::from_millis(250),
				TransformPositionLens {
					start: enemy_transform.translation,
					end:   utils::grid_coords_to_translation(*destination, IVec2::splat(GRID_SIZE))
						.extend(enemy_transform.translation.z),
				},
			)
			.with_completed_event(0),
		));

		*enemy_animation = EnemyBundle::walking_animation();
		enemy_atlas.index = enemy_animation.first;

		let enemy = level_cache
			.enemies
			.remove(enemy_pos)
			.expect("found no enemy at the moved position");
		assert_eq!(enemy, enemy_entity, "wrong enemy found in level cache");
		level_cache.enemies.insert(*destination, enemy);

		*turn_state = TurnState::EnemiesBusy;
		return;
	}

	game_state
		.enemies
		.iter_mut()
		.for_each(|(_, ready)| *ready = true);
	*turn_state = TurnState::PlayerWaiting;
}
