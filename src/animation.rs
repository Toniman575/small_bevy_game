//! Animation system.

use std::ops::Deref;

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_tweening::{Animator, TweenCompleted};

use crate::gameplay::{Enemy, EnemyBundle, Player, PlayerBundle};
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

/// Animate entities.
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn animate(
	mut commands: Commands<'_, '_>,
	time: Res<'_, Time>,
	mut query: Query<'_, '_, (Entity, &mut Sprite, &mut TextureAtlas, &mut Animation)>,
) {
	for (entity, mut sprite, mut atlas, mut animation) in &mut query {
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
					commands.entity(entity).remove::<Animation>();
					return;
				}
			} else {
				atlas.index + 1
			};
		}
	}
}

/// Remove [`Animator`] after completion and potentially transition to old animation.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
pub(crate) fn finish_animation(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
	mut turn_state: ResMut<'_, TurnState>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut completed: EventReader<'_, '_, TweenCompleted>,
	mut query: Query<
		'_,
		'_,
		(
			&Transform,
			&mut GridCoords,
			&mut TextureAtlas,
			&mut Animation,
			&mut Visibility,
			Has<Player>,
			Has<Enemy>,
		),
	>,
) {
	for completed in completed.read() {
		let (
			transform,
			mut grid_coord,
			mut atlas,
			mut animation,
			mut visibility,
			has_player,
			has_enemy,
		) = query.get_mut(completed.entity).unwrap();

		*grid_coord =
			utils::translation_to_grid_coords(transform.translation.xy(), IVec2::splat(GRID_SIZE));

		// Transition to next state
		*turn_state = TurnState::EnemiesWaiting;

		// Transition back to idle animation.
		if has_player {
			*animation = PlayerBundle::idle_animation();
		} else if has_enemy {
			*animation = EnemyBundle::idle_animation();
		} else {
			unreachable!("found unknown entity");
		}

		atlas.index = animation.first;
		commands
			.entity(completed.entity)
			.remove::<Animator<Transform>>();

		// Pick up items if it makes sense.
		if has_player {
			if let Some((entity, source)) = level_cache.keys.remove(grid_coord.deref()) {
				*state.keys.get_mut(&source).unwrap() = true;
				state.player_keys += 1;
				commands.entity(entity).insert(Visibility::Hidden);
			}
		}

		// Hide enemy if it makes sense.
		if has_enemy && !level_cache.visible_tiles.contains(&grid_coord) {
			visibility.set_if_neq(Visibility::Hidden);
		}
	}
}
