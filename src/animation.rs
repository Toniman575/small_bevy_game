//! Animation system.

use std::ops::DerefMut;

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy_ecs_ldtk::prelude::*;
use bevy_tweening::{Animator, TweenCompleted};

use crate::gameplay::Player;
use crate::{LevelCache, TurnState};

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

/// What should happen after an animation has completed.
#[derive(Component)]
pub(crate) struct AnimationFinish {
	/// Tile animation has finished on.
	pub arrived_event: Option<GridCoords>,
	/// Switch to a new animation.
	pub new_animation: Option<Animation>,
}

/// Remove [`Animator`] after completion and potentially transition to old animation.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
pub(crate) fn finish_animation(
	mut commands: Commands<'_, '_>,
	mut completed: EventReader<'_, '_, TweenCompleted>,
	mut arrived: EventWriter<'_, AnimationArrived>,
	mut query: Query<
		'_,
		'_,
		(Entity, &AnimationFinish, &mut TextureAtlas, &mut Animation),
		With<Animator<Transform>>,
	>,
	mut animation_state: ResMut<'_, TurnState>,
) {
	for completed in completed.read() {
		let mut command = commands.entity(completed.entity);
		*animation_state = TurnState::EnemiesWaiting;

		// Transition to old animation.
		if let Ok((entity, finish, mut atlas, mut animation)) = query.get_mut(completed.entity) {
			if let Some(new_animation) = &finish.new_animation {
				*animation.deref_mut() = new_animation.clone();
				atlas.index = animation.first;
			}

			if let Some(position) = finish.arrived_event {
				arrived.send(AnimationArrived { entity, position });
			}

			command.remove::<AnimationFinish>();
		}

		command.remove::<Animator<Transform>>();
	}
}

/// Event fired when the animation has reached a tile or has been interrupted.
#[derive(Event)]
pub(crate) struct AnimationArrived {
	/// Entity that arriveed.
	entity:   Entity,
	/// Arrived at which grid coordinates.
	position: GridCoords,
}

// Used to despawn things when movement to a tile has finished.
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn animation_arrived_tile(
	mut commands: Commands<'_, '_>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut arrived: EventReader<'_, '_, AnimationArrived>,
	player: Query<'_, '_, Entity, With<Player>>,
) {
	if arrived.is_empty() {
		return;
	}

	let player = player.single();

	for arrived in arrived.read() {
		if arrived.entity == player {
			if let Some((entity, _)) = level_cache.keys.remove(&arrived.position) {
				commands.entity(entity).insert(Visibility::Hidden);
			}
		}
	}
}
