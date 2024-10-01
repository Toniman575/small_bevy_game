//! Player functionality.

use std::ops::DerefMut;
use std::time::Duration;

use bevy::input::keyboard::KeyboardInput;
use bevy::input::mouse::MouseButtonInput;
use bevy::input::ButtonState;
use bevy::prelude::*;
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_ldtk::utils;
use bevy_ecs_tilemap::helpers::square_grid::neighbors::Neighbors;
use bevy_ecs_tilemap::map::TilemapSize;
use bevy_ecs_tilemap::tiles::TilePos;
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween};

use super::{
	Abilities, AbilityEffect, AbilityEvent, AbilityEventTarget, ActiveAbility,
	CurrentStatusEffects, Dead, EffectType, Enemy, Health, Spellbook, Vision,
};
use crate::animation::Animation;
use crate::{
	Destination, DoorOpen, DoorState, GameState, LevelCache, Object, PlayerBusy,
	PlayerEntityDestination, TargetingMarker, Turn, TurnState, GRID_SIZE,
};

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
	/// Vision.
	vision:              Vision,
	/// Active [`StatusEffects`].
	effects:             CurrentStatusEffects,
}

impl Default for PlayerBundle {
	fn default() -> Self {
		Self {
			player:              Player,
			health:              Health::default(),
			abilities:           Spellbook::default_player(),
			active_ability:      ActiveAbility::default(),
			sprite_sheet_bundle: LdtkSpriteSheetBundle::default(),
			grid_coords:         GridCoords::default(),
			animation:           Self::idle_animation(),
			worldly:             Worldly::default(),
			vision:              Vision::new(4),
			effects:             CurrentStatusEffects::default(),
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

/// Player movement.
#[expect(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::type_complexity
)]
pub(crate) fn player_movement(
	mut state: ResMut<'_, GameState>,
	mut turn_q: Query<'_, '_, &mut Turn>,
	mut commands: Commands<'_, '_>,
	mut inputs: EventReader<'_, '_, KeyboardInput>,
	mut player: Query<
		'_,
		'_,
		(
			Entity,
			&Transform,
			&GridCoords,
			&mut Sprite,
			&mut TextureAtlas,
			&mut Animation,
		),
		With<Player>,
	>,
	enemies: Query<'_, '_, &GridCoords, (With<Enemy>, Without<Player>, Without<Dead>)>,
	level_cache: Res<'_, LevelCache>,
	mut level_selection: ResMut<'_, LevelSelection>,
	mut destination_entity: ResMut<'_, PlayerEntityDestination>,
	mut animation_state: ResMut<'_, TurnState>,
	mut buffered_movement: Local<'_, Option<KeyCode>>,
) {
	for keycode in (*buffered_movement)
		.into_iter()
		.chain(inputs.read().filter_map(|input| {
			if let KeyboardInput {
				state: ButtonState::Pressed,
				repeat: false,
				..
			} = input
			{
				Some(input.key_code)
			} else {
				None
			}
		})) {
		let mut turn = turn_q.single_mut();

		let (direction, flip) = match keycode {
			KeyCode::KeyW => (GridCoords::new(0, 1), None),
			KeyCode::KeyA => (GridCoords::new(-1, 0), Some(true)),
			KeyCode::KeyS => (GridCoords::new(0, -1), None),
			KeyCode::KeyD => (GridCoords::new(1, 0), Some(false)),
			KeyCode::KeyQ => (GridCoords::new(-1, 1), Some(true)),
			KeyCode::KeyE => (GridCoords::new(1, 1), Some(false)),
			KeyCode::KeyZ => (GridCoords::new(-1, -1), Some(true)),
			KeyCode::KeyC => (GridCoords::new(1, -1), Some(false)),
			KeyCode::Space => {
				turn.set_if_neq(Turn(turn.0 + 1));
				*animation_state = TurnState::EnemiesWaiting;
				return;
			}
			_ => continue,
		};

		if !matches!(animation_state.as_ref(), TurnState::PlayerWaiting) {
			*buffered_movement = Some(keycode);
			continue;
		}

		*buffered_movement = None;

		let (player_entity, transform, grid_pos, mut sprite, mut atlas, mut animation) =
			player.single_mut();
		let destination = *grid_pos + direction;

		match level_cache.destination(
			&state.doors,
			*grid_pos,
			destination,
			&enemies.iter().copied().collect::<Vec<_>>(),
		) {
			Destination::Walkable => {
				turn.set_if_neq(Turn(turn.0 + 1));
				*animation_state = TurnState::PlayerBusy(PlayerBusy::Moving);

				let mut player = commands.entity(player_entity);
				player.insert(Animator::new(
					Tween::new(
						EaseMethod::Linear,
						Duration::from_millis(250),
						TransformPositionLens {
							start: transform.translation,
							end:   utils::grid_coords_to_translation(
								destination,
								IVec2::splat(GRID_SIZE),
							)
							.extend(transform.translation.z),
						},
					)
					.with_completed_event(0),
				));

				*animation = PlayerBundle::walking_animation();
				atlas.index = animation.first;

				if let Some(flip) = flip {
					sprite.flip_x = flip;
				}
			}
			Destination::Wall | Destination::Enemy => (),
			Destination::Door(door) => {
				if let LevelSelection::Iid(current_level) = level_selection.as_mut() {
					*current_level = door.level;
					*state.doors.get_mut(&door.entity).unwrap() = DoorState::Passed;
					destination_entity.0 = Some(door.entity);
				} else {
					unreachable!("levels should only be `LevelIid`")
				}
			}
			Destination::BeyondBoundary => unreachable!("somehow dropped off the floor"),
		}
	}
}

/// Opens and closes doors.
#[expect(clippy::needless_pass_by_value, clippy::too_many_arguments)]
pub(crate) fn door_interactions(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
	mut animation_state: ResMut<'_, TurnState>,
	level_cache: Res<'_, LevelCache>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	mut player: Query<'_, '_, (&GridCoords, &mut Health), With<Player>>,
	mut input: EventReader<'_, '_, KeyboardInput>,
	mut turn_q: Query<'_, '_, &mut Turn>,
) {
	let Some(input) = input.read().next() else {
		return;
	};
	let KeyboardInput {
		state: ButtonState::Pressed,
		repeat: false,
		..
	} = input
	else {
		return;
	};

	if let KeyCode::KeyF = input.key_code {
		let (player_grid_coords, _) = player.single();
		let tile_map_size = tile_map_size.iter().next().unwrap();

		for tile_pos in Neighbors::get_square_neighboring_positions(
			&TilePos::from(*player_grid_coords),
			tile_map_size,
			true,
		)
		.iter()
		{
			if let Some((entity, iid, door)) = level_cache.doors.get(&GridCoords::from(*tile_pos)) {
				let GameState {
					doors,
					player_items,
					..
				} = state.deref_mut();

				let door_state = doors.get_mut(iid).unwrap();

				let keys = player_items.entry(Object::Key).or_insert(0);

				if *door_state == DoorState::Closed && *keys > 0 {
					let mut turn = turn_q.single_mut();
					turn.set_if_neq(Turn(turn.0 + 1));
					*animation_state = TurnState::EnemiesWaiting;

					*door_state = DoorState::Opened;
					*keys -= 1;
					*state.doors.get_mut(&door.entity).unwrap() = DoorState::Opened;
					commands.trigger_targets(DoorOpen, *entity);
				}
			}
		}
	}

	if let KeyCode::KeyH = input.key_code {
		let (_, mut player_health) = player.single_mut();
		let potions = state.player_items.entry(Object::Potion).or_insert(0);

		if *potions > 0 && player_health.current != player_health.max {
			*potions -= 1;
			player_health.current = u16::min(player_health.current + 50, player_health.max);
		}
	}
}

/// Lets player cast an ability with a mouseclick.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn cast_ability(
	mut inputs: EventReader<'_, '_, MouseButtonInput>,
	mut ability_events: EventWriter<'_, AbilityEvent>,
	player: Query<'_, '_, (Entity, &GridCoords, &Health, &ActiveAbility), With<Player>>,
	targeting_marker: Query<'_, '_, (&GridCoords, &TargetingMarker)>,
	abilities: Res<'_, Abilities>,
) {
	let (target_grid_coords, target_marker) = targeting_marker.single();

	if !target_marker.valid {
		return;
	}

	for input in inputs.read() {
		let MouseButtonInput {
			state: ButtonState::Pressed,
			button: MouseButton::Left,
			..
		} = input
		else {
			continue;
		};

		let (player_entity, player_grid_coords, health, active_ability) = player.single();

		let ability = abilities
			.0
			.get(&active_ability.0)
			.expect("active ability has a non valid ability id");

		if ability.aoe.is_some() {
			ability_events.send(AbilityEvent::new(
				player_entity,
				active_ability.0,
				AbilityEventTarget::Tile(*target_grid_coords),
			));
		} else if let AbilityEffect::Healing(_) = ability.effect {
			if player_grid_coords == target_grid_coords && health.current != health.max {
				ability_events.send(AbilityEvent::new(
					player_entity,
					active_ability.0,
					AbilityEventTarget::Entity(player_entity),
				));
			}
		} else if let AbilityEffect::StatusEffect(effect) = ability.effect.clone() {
			match effect.effect_type {
				EffectType::DefensiveBuff | EffectType::AttackBuff => {
					if player_grid_coords == target_grid_coords {
						ability_events.send(AbilityEvent::new(
							player_entity,
							active_ability.0,
							AbilityEventTarget::Entity(player_entity),
						));
					}
				}
				EffectType::DefensiveDebuff | EffectType::AttackDebuff | EffectType::Dot => {
					ability_events.send(AbilityEvent::new(
						player_entity,
						active_ability.0,
						AbilityEventTarget::Entity(
							target_marker
								.entity
								.expect("casting ability with invalid target"),
						),
					));
				}
			}
		} else {
			ability_events.send(AbilityEvent::new(
				player_entity,
				active_ability.0,
				AbilityEventTarget::Entity(
					target_marker
						.entity
						.expect("casting ability with invalid target"),
				),
			));
		};

		return;
	}
}

/// Selects ability when pressing number keys.
pub(crate) fn select_ability(
	mut input: EventReader<'_, '_, KeyboardInput>,
	mut player: Query<'_, '_, (&Spellbook, &mut ActiveAbility), With<Player>>,
) {
	let Ok((spellbook, mut active_ability)) = player.get_single_mut() else {
		return;
	};

	for ev in input.read() {
		let mut sorted_spellbook = spellbook.abilities.iter().collect::<Vec<_>>();

		sorted_spellbook.sort_by(|(id1, _), (id2, _)| (id1).partial_cmp(id2).unwrap());

		let mut sorted_spellbook_iter = sorted_spellbook.iter();

		if let ButtonState::Pressed = ev.state {
			match ev.key_code {
				KeyCode::Digit1 => {
					active_ability.0.clone_from(
						sorted_spellbook_iter
							.next()
							.expect("first slot always needs to have autoattack")
							.0,
					);
				}
				KeyCode::Digit2 => {
					if let Some(name) = sorted_spellbook_iter.nth(1) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit3 => {
					if let Some(name) = sorted_spellbook_iter.nth(2) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit4 => {
					if let Some(name) = sorted_spellbook_iter.nth(3) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit5 => {
					if let Some(name) = sorted_spellbook_iter.nth(4) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit6 => {
					if let Some(name) = sorted_spellbook_iter.nth(5) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit7 => {
					if let Some(name) = sorted_spellbook_iter.nth(6) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit8 => {
					if let Some(name) = sorted_spellbook_iter.nth(7) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit9 => {
					if let Some(name) = sorted_spellbook_iter.nth(8) {
						active_ability.0.clone_from(name.0);
					}
				}
				KeyCode::Digit0 => {
					if let Some(name) = sorted_spellbook_iter.nth(9) {
						active_ability.0.clone_from(name.0);
					}
				}
				_ => {}
			}
		}
	}
}
