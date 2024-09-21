//! Animation system.

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_tweening::{Animator, TweenCompleted};

use crate::gameplay::{Enemy, Player, PlayerBundle, Vision};
use crate::{GameState, LevelCache, TurnState, GRID_SIZE};

/// Animated sprite.
#[derive(Clone, Component)]
pub(crate) struct Animation {
	/// Animation timing.
	pub timer:     Timer,
	/// First animation sprite.
	pub first:     usize,
	/// Last animation sprite.
	pub last:      usize,
	/// Animation is repeated.
	pub repeating: bool,
	/// Custom anchor for this animation.
	pub anchor:    Option<Anchor>,
}

/// Ability Animation.
#[derive(Clone, Component)]
pub(crate) struct AnimationAbility;

/// Animate entities.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn animate(
	mut commands: Commands<'_, '_>,
	time: Res<'_, Time>,
	mut query: Query<
		'_,
		'_,
		(
			Entity,
			&mut Sprite,
			&mut TextureAtlas,
			&mut Animation,
			Has<AnimationAbility>,
		),
	>,
) {
	for (entity, mut sprite, mut atlas, mut animation, is_ability_animation) in &mut query {
		if let Some(anchor) = animation.anchor {
			sprite.anchor = anchor;
		} else {
			sprite.anchor = Anchor::default();
		}

		animation.timer.tick(time.delta());

		if animation.timer.just_finished() {
			atlas.index = if atlas.index == animation.last {
				if animation.repeating {
					animation.first
				} else {
					if is_ability_animation {
						commands.entity(entity).despawn_recursive();
					} else {
						commands.entity(entity).remove::<Animation>();
					}

					continue;
				}
			} else {
				atlas.index + 1
			};
		}
	}
}

/// Remove [`Animator`] after completion and potentially transition to old animation.
#[expect(clippy::needless_pass_by_value, clippy::type_complexity)]
pub(crate) fn finish_animation(
	mut commands: Commands<'_, '_>,
	mut turn_state: ResMut<'_, TurnState>,
	mut completed: EventReader<'_, '_, TweenCompleted>,
	player_q: Query<'_, '_, &Vision, With<Player>>,
	mut query: Query<
		'_,
		'_,
		(
			Entity,
			&Transform,
			Option<&mut GridCoords>,
			Option<&mut TextureAtlas>,
			Option<&mut Animation>,
			&mut Visibility,
			Has<Player>,
			Option<&Enemy>,
			Has<AnimationAbility>,
		),
	>,
) {
	let Ok(player_vision) = player_q.get_single() else {
		return;
	};

	for completed in completed.read() {
		let (
			entity,
			transform,
			grid_coord,
			atlas,
			animation,
			mut visibility,
			has_player,
			enemy_type,
			is_ability_animation,
		) = query.get_mut(completed.entity).unwrap();

		if is_ability_animation {
			commands.entity(entity).despawn_recursive();
			continue;
		}

		let mut grid_coord = grid_coord.unwrap();
		*grid_coord =
			utils::translation_to_grid_coords(transform.translation.xy(), IVec2::splat(GRID_SIZE));

		// Transition to next state
		*turn_state = TurnState::EnemiesWaiting;

		let mut animation = animation.unwrap();

		// Transition back to idle animation.
		if has_player {
			*animation = PlayerBundle::idle_animation();
		} else if let Some(enemy_type) = enemy_type {
			*animation = enemy_type.idle_animation();
		} else {
			unreachable!("found unknown entity");
		}

		atlas.unwrap().index = animation.first;
		commands
			.entity(completed.entity)
			.remove::<Animator<Transform>>();

		// Hide enemy if it makes sense.
		if enemy_type.is_some()
			&& !player_vision.tiles.contains(&grid_coord)
			&& !player_vision.memory.contains_key(&entity)
		{
			visibility.set_if_neq(Visibility::Hidden);
		}

		commands.trigger_targets(ArrivedAtTile, entity);
	}
}

/// Observer for when entities arrive at a tile.
#[derive(Event)]
pub(crate) struct ArrivedAtTile;

/// Run when an entity has arrived at a tile.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn arrived_at_tile(
	trigger: Trigger<'_, ArrivedAtTile>,
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut query: Query<'_, '_, (&GridCoords, Has<Player>)>,
) {
	let (grid_coord, has_player) = query.get_mut(trigger.entity()).expect("entity not found");

	// Pick up items if it makes sense.
	if has_player {
		if let Some((entity, object, source)) = level_cache.objects.remove(grid_coord) {
			state
				.objects
				.get_mut(&source)
				.unwrap()
				.entry(object)
				.insert(true);
			*state.player_items.entry(object).or_insert(0) += 1;
			commands.entity(entity).insert(Visibility::Hidden);
		}
	}
}
