//! Shared gameplay functionality.

mod abilities;
mod enemy;
mod player;

use std::collections::VecDeque;
use std::time::Duration;

use bevy::color::palettes::css::RED;
use bevy::prelude::*;
use bevy::sprite::MaterialMesh2dBundle;
use bevy::utils::{HashMap, HashSet};
use bevy_ecs_ldtk::prelude::*;
use bevy_ecs_tilemap::map::TilemapSize;
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween};

pub(crate) use self::abilities::Abilities;
pub(crate) use self::enemy::{move_enemies, Enemy, EnemyBundle};
pub(crate) use self::player::{
	cast_ability, door_interactions, player_movement, select_ability, Player, PlayerBundle,
};
use crate::animation::{Animation, AnimationAbility, ArrivedAtTile};
use crate::util::{flip_sprite, OrderedNeighbors};
use crate::{
	util, Destination, Drops, GameState, ItemSource, Key, LevelCache, PlayerBusy, Textures,
	TurnState,
};

/// Component for tracking health in entities.
#[derive(Clone, Component, Debug, Default, Reflect, PartialEq, Eq, Copy)]
pub(crate) struct Health {
	/// How much health the entity currently has.
	pub(crate) current: u16,
	/// How much health the entity can have.
	pub(crate) max:     u16,
}

#[allow(clippy::fallible_impl_from)]
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

/// Component for tracking which tiles this entity sees.
#[derive(Component, Reflect)]
pub(crate) struct Vision {
	/// List of currently visible tiles.
	pub(crate) tiles:  Vec<GridCoords>,
	/// Memory of seen entities not currently in vision.
	pub(crate) memory: HashMap<Entity, GridCoords>,
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
#[derive(Reflect, Copy, Clone)]
pub(crate) enum AbilityEffect {
	/// Damages enemies.
	Damage(u16),
	/// Heals player.
	Healing(u16),
	/// Buffs player.
	Buff(Buff),
	/// Teleports.
	Teleport,
}

/// Holds animation data.
#[derive(Reflect, Clone)]
pub(crate) struct AbilityAnimation {
	/// Animation texture handle.
	texture:  Handle<Image>,
	/// Animation duration.
	duration: Duration,
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
		animation: Option<AbilityAnimation>,
	) -> Self {
		Self {
			name,
			range,
			aoe,
			effect,
			cooldown,
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

/// A buff.
#[derive(Reflect, Copy, Clone, Component)]
pub(crate) struct Buff {
	/// The power of the buff.
	pub(crate) power:     f32,
	/// How many turns the buff lasts.
	pub(crate) length:    u64,
	/// When the buff was cast.
	pub(crate) last_cast: Option<u64>,
}

impl Buff {
	/// Creates a [`Buff`].
	const fn new(power: f32, length: u64) -> Self {
		Self {
			power,
			length,
			last_cast: None,
		}
	}

	/// Calculates how many turns are left for this buff.
	pub(crate) fn turns_left(&self, current_turn: u64) -> Option<u64> {
		self.last_cast.and_then(|last_cast| {
			let turns_left = (last_cast + self.length).saturating_sub(current_turn);

			(turns_left > 0).then_some(turns_left)
		})
	}
}

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
	pub(crate) autoattack_melee:  AbilityId,
	/// The Id of a ranged autoattack of an entity.
	pub(crate) autoattack_ranged: AbilityId,
	/// All abilities an entity can cast.
	pub(crate) abilities:         HashMap<AbilityId, SpellbookAbility>,
}

impl Spellbook {
	/// Default spellbook for player.
	fn default_player() -> Self {
		Self {
			autoattack_melee:  AbilityId(0),
			autoattack_ranged: AbilityId(1),
			abilities:         HashMap::from_iter([
				(AbilityId(0), SpellbookAbility::default()),
				(AbilityId(1), SpellbookAbility::default()),
				(AbilityId(2), SpellbookAbility::default()),
				(AbilityId(3), SpellbookAbility::default()),
				(AbilityId(4), SpellbookAbility::default()),
				(AbilityId(5), SpellbookAbility::default()),
			]),
		}
	}

	/// Default spellbook for enemies.
	fn default_enemy() -> Self {
		Self {
			autoattack_melee:  AbilityId(6),
			autoattack_ranged: AbilityId(7),
			abilities:         HashMap::from_iter([
				(AbilityId(6), SpellbookAbility::default()),
				(AbilityId(7), SpellbookAbility::default()),
			]),
		}
	}
}

/// The currently active ability of an entity.
#[derive(Reflect, Component, Default)]
pub(crate) struct ActiveAbility(pub(crate) AbilityId);

/// Handles when an entity wants to cast an ability.
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_lines,
	clippy::type_complexity,
	clippy::too_many_arguments
)]
pub(crate) fn handle_ability_event(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
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
			Option<&Buff>,
			Has<Player>,
			Has<Enemy>,
		),
	>,
	level_cache: ResMut<'_, LevelCache>,
	mut ability_events: EventReader<'_, '_, AbilityEvent>,
	mut animation_state: ResMut<'_, TurnState>,
) {
	for ability_event in ability_events.read() {
		let (
			caster_entity,
			_,
			mut caster_grid_coord,
			mut spellbook,
			mut caster_sprite,
			mut health,
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

		let (target_entity, power) = match ability.effect {
			AbilityEffect::Damage(power) => {
				let AbilityEventTarget::Entity(entity) = ability_event.target else {
					unreachable!("sent teleport spell on an entity")
				};

				(entity, power)
			}
			AbilityEffect::Healing(power) => {
				if ability.cooldown.is_some() {
					spellbook_ability.last_cast = Some(state.turn);
				}

				health.current = (health.current + power).min(health.max);

				state.turn += 1;
				*animation_state = TurnState::EnemiesWaiting;

				continue;
			}
			AbilityEffect::Teleport => {
				let AbilityEventTarget::Tile(target_grid_coord) = ability_event.target else {
					unreachable!("sent teleport spell on an entity")
				};

				flip_sprite(*caster_grid_coord, target_grid_coord, &mut caster_sprite);

				if !ability.in_euclidean_range(*caster_grid_coord, target_grid_coord) {
					continue;
				}

				if let Destination::Walkable =
					level_cache.destination(&state.doors, *caster_grid_coord, target_grid_coord)
				{
					caster_grid_coord.set_if_neq(target_grid_coord);

					if ability.cooldown.is_some() {
						spellbook_ability.last_cast = Some(state.turn);
					}

					commands.trigger_targets(ArrivedAtTile, caster_entity);

					state.turn += 1;
					*animation_state = TurnState::EnemiesWaiting;
				}

				continue;
			}
			AbilityEffect::Buff(mut buff) => {
				if ability.cooldown.is_some() {
					spellbook_ability.last_cast = Some(state.turn);
				}

				buff.last_cast = Some(state.turn);

				commands.entity(ability_event.caster_entity).insert(buff);

				state.turn += 1;
				*animation_state = TurnState::EnemiesWaiting;

				continue;
			}
		};

		let [
			(
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
			),
			(
				_,
				target_transform,
				target_grid_coord,
				_,
				_,
				mut target_health,
				mut vision,
				target_buff,
				..,
			),
		] = entities_q.many_mut([ability_event.caster_entity, target_entity]);

		flip_sprite(*caster_grid_coord, *target_grid_coord, &mut sprite);

		if !ability.in_euclidean_range(*caster_grid_coord, *target_grid_coord) {
			continue;
		}

		vision.memory.insert(caster_entity, *caster_grid_coord);

		if caster_is_player {
			state.turn += 1;
			*animation_state = TurnState::PlayerBusy(PlayerBusy::Casting);
		} else if caster_is_enemy {
			*animation_state = TurnState::EnemiesBusy;
		} else {
			unreachable!("entity has to be enemy or player");
		}

		let power_modifier = target_buff.map_or(1., |buff| buff.power);

		target_health.current = target_health
			.current
			.saturating_sub((power as f32 * power_modifier) as u16);
		let target = caster_transform.translation
			+ ((target_transform.translation.xy() - caster_transform.translation.xy()).normalize()
				* 5.)
				.extend(caster_transform.translation.z);

		commands.entity(caster_entity).insert((Animator::new(
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
		),));

		let to_enemy =
			(target_transform.translation.xy() - caster_transform.translation.xy()).normalize();
		let rotate_to_enemy = Quat::from_rotation_arc(Vec3::Y, to_enemy.extend(0.));
		if let Some(ability_animation) = &ability.animation {
			commands.spawn((
				AnimationAbility,
				SpriteBundle {
					texture: ability_animation.texture.clone(),
					transform: Transform {
						translation: caster_transform.translation,
						rotation: rotate_to_enemy,
						..Transform::default()
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

		if ability.cooldown.is_some() {
			let spellbook_ability = spellbook
				.abilities
				.get_mut(&ability_event.ability)
				.expect("requested non-existing ability");

			spellbook_ability.last_cast = Some(state.turn);
		}
	}
}

/// When an Entity with a healthbar is spawned we spawn a mesh to represent it.
#[allow(clippy::needless_pass_by_value)]
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
#[allow(clippy::needless_pass_by_value)]
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
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn tick_cooldowns(
	state: ResMut<'_, GameState>,
	abilities: Res<'_, Abilities>,
	mut spellbooks: Query<'_, '_, &mut Spellbook>,
) {
	if state.is_changed() {
		for mut spellbook in &mut spellbooks {
			for (id, spellbook_ability) in &mut spellbook.abilities {
				let ability = abilities.0.get(id).expect("found non-existing ability");

				if spellbook_ability
					.cooldown_left(ability, state.turn)
					.is_none()
				{
					spellbook_ability.last_cast = None;
				}
			}
		}
	}
}

/// Ticks buffs currently on cooldown.
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn tick_buff(
	mut commands: Commands<'_, '_>,
	state: ResMut<'_, GameState>,
	buffs: Query<'_, '_, (Entity, &Buff)>,
) {
	if state.is_changed() {
		for (entity, buff) in &buffs {
			if buff.turns_left(state.turn).is_none() {
				commands.entity(entity).remove::<Buff>();
			}
		}
	}
}

/// Handle when a death event was sent.
#[allow(
	clippy::needless_pass_by_value,
	clippy::too_many_arguments,
	clippy::too_many_lines,
	clippy::type_complexity
)]
pub(crate) fn death(
	mut commands: Commands<'_, '_>,
	textures: Res<'_, Textures>,
	mut deaths: EventReader<'_, '_, DeathEvent>,
	player_q: Query<'_, '_, &Vision, With<Player>>,
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
			&mut Drops,
			Has<Enemy>,
			Has<Player>,
		),
	>,
	tile_map_size: Query<'_, '_, &TilemapSize>,
	layers: Query<'_, '_, (Entity, &LayerMetadata)>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut game_state: ResMut<'_, GameState>,
) {
	let Ok(player_vision) = player_q.get_single() else {
		return;
	};

	if deaths.is_empty() {
		return;
	}

	let tile_map_size = tile_map_size.single();

	for event in deaths.read() {
		let (
			entity,
			entity_iid,
			grid_coords,
			mut transform,
			mut atlas,
			mut animation,
			mut drops,
			enemy,
			player,
		) = animations.get_mut(event.0).unwrap();

		let death_animation = if enemy {
			game_state.enemies.retain(|(enemy, _)| *enemy != entity);
			EnemyBundle::death_animation()
		} else if player {
			unimplemented!()
		} else {
			unreachable!("player can't be enemy and visa versa")
		};

		transform.translation.z -= 0.1;

		*animation = death_animation;
		atlas.index = animation.first;

		let mut entity_commands = commands.entity(event.0);
		entity_commands.despawn_descendants();
		level_cache
			.enemies
			.remove(grid_coords)
			.expect("Enemy should be in level cache.");

		for drop in drops.0.drain(..) {
			match drop.as_str() {
				"Key" => {
					// This has been dropped and picked up before. So don't drop it again!
					if *game_state
						.keys
						.get(&ItemSource::Loot(entity_iid.clone()))
						.unwrap()
					{
						continue;
					}

					#[allow(clippy::shadow_same)]
					let mut grid_coords = *grid_coords;

					if level_cache.keys.get(&grid_coords).is_some() {
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

								if level_cache.keys.get(&new_grid_coords).is_none() {
									grid_coords = new_grid_coords;
									break 'outer;
								}

								next.push_back(new_grid_coords);
							}
						}
					}

					let entity = commands
						.spawn((
							Name::new("Key"),
							Key,
							grid_coords,
							SpriteBundle {
								sprite: Sprite {
									custom_size: Some(Vec2::new(16., 16.)),
									rect: Some(Rect::new(32., 64., 48., 80.)),
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
								.find(|(_, layer)| {
									layer.iid == "95801fc0-25d0-11ef-8498-6b3d2275a196"
								})
								.unwrap()
								.0,
						)
						.add_child(entity);

					assert!(
						level_cache
							.keys
							.insert(grid_coords, (entity, ItemSource::Loot(entity_iid.clone())))
							.is_none(),
						"found key at this position already",
					);
				}
				_ => unimplemented!(),
			}
		}
	}
}
