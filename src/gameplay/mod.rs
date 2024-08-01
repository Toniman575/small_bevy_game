//! Shared gameplay functionality.

mod enemy;
mod player;

use std::collections::BTreeMap;
use std::time::Duration;

use bevy::color::palettes::css::RED;
use bevy::prelude::*;
use bevy::sprite::MaterialMesh2dBundle;
use bevy_ecs_ldtk::prelude::*;
use bevy_egui::egui::emath::Numeric;
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseMethod, Tween};

pub(crate) use self::enemy::{Enemy, EnemyBundle};
pub(crate) use self::player::{Player, PlayerBundle};
use crate::animation::Animation;
use crate::{util, Drops, GameState, ItemSource, Key, LevelCache, TargetingMarker, TurnState};

/// Component for tracking health in entities.
#[derive(Clone, Component, Debug, Default, Reflect, PartialEq, Eq, Copy)]
pub(crate) struct Health {
	/// How much health the entity currently has.
	pub(crate) current: u16,
	/// How much health the entity can have.
	max:                u16,
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

/// When an entity dies, it sends this event.
#[derive(Event)]
pub(crate) struct DeathEvent(Entity);

/// Event that fires when a player uses an ability. If not sent through a mouseclick we need to
/// specifify which entity is the target.
#[derive(Event)]
pub(crate) struct AbilityEvent(pub(crate) Option<Entity>);

/// An ability an entity can perform.
#[derive(Reflect, Default)]
pub(crate) struct Ability {
	/// Identifier for the ability.
	pub(crate) id:        u64,
	/// Name of the ability.
	pub(crate) name:      String,
	/// The range in manhatten distance of the ability.
	pub(crate) range:     u8,
	/// The power of the ability.
	power:                u16,
	/// The Cooldown of the ability.
	cooldown:             Option<u8>,
	/// When this ability was last cast.
	pub(crate) last_cast: Option<u64>,
}

impl Ability {
	/// Creates an ability.
	const fn new(id: u64, name: String, range: u8, power: u16, cooldown: Option<u8>) -> Self {
		Self {
			id,
			name,
			range,
			power,
			cooldown,
			last_cast: None,
		}
	}

	/// Checks if ability is in manhatten range.
	fn in_manhatten_range(&self, origin: GridCoords, target: GridCoords) -> bool {
		let distance = util::manhatten_distance(target, origin);

		distance <= self.range.into()
	}

	/// Checks if ability is in euclidian range.
	pub(crate) fn in_euclidean_range(&self, origin: GridCoords, target: GridCoords) -> bool {
		let distance = util::euclidean_distance(target, origin);

		distance.floor() <= self.range.into()
	}

	/// Calculates how much cooldown is left for this ability.
	pub(crate) fn cooldown_left(&self, current_turn: u64) -> Option<u64> {
		self.last_cast.and_then(|last_cast| {
			let cooldown_left = (last_cast
				+ u64::from(
					self.cooldown
						.expect("when ability has last cast it also needs to have a cooldown"),
				))
			.saturating_sub(current_turn);

			(cooldown_left > 0).then_some(cooldown_left)
		})
	}

	//fn get_animation(&self) -> impl Tweenable<TransformPositionLens> {}
}

/// The collection of abilities an entity can perform.
#[derive(Reflect, Component)]
pub(crate) struct Spellbook(pub(crate) BTreeMap<u64, Ability>);

impl Default for Spellbook {
	fn default() -> Self {
		Self(BTreeMap::from([
			(0, Ability::new(0, String::from("Autoattack"), 1, 5, None)),
			(1, Ability::new(1, String::from("Ranged"), 5, 2, None)),
			(2, Ability::new(2, String::from("Hardcore"), 1, 10, Some(3))),
		]))
	}
}

/// The currently active ability of an entity.
#[derive(Reflect, Component, Default)]
pub(crate) struct ActiveAbility(pub(crate) u64);

/// Handles when a player wants to cast an ability.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
pub(crate) fn handle_ability_event(
	mut commands: Commands<'_, '_>,
	mut state: ResMut<'_, GameState>,
	mut player: Query<
		'_,
		'_,
		(
			Entity,
			&Transform,
			&ActiveAbility,
			&GridCoords,
			&mut Spellbook,
			&mut Sprite,
		),
		(With<Player>, Without<Enemy>),
	>,
	target_marker: Query<'_, '_, &TargetingMarker>,
	mut enemies: Query<
		'_,
		'_,
		(&Transform, &GridCoords, &mut Health),
		(With<Enemy>, Without<Player>),
	>,
	mut abilities: EventReader<'_, '_, AbilityEvent>,
	mut animation_state: ResMut<'_, TurnState>,
) {
	let target_marker = target_marker.single();

	for ability in abilities.read() {
		// Attack either a explicit ability target or the target at the cursor.
		let Some(entity) = ability.0.or(target_marker.0) else {
			continue;
		};

		let (enemy_transform, enemy_grid_coord, mut health) = enemies
			.get_mut(entity)
			.expect("found no enemy at the specified position");

		let (
			player_entity,
			player_transform,
			active_ability,
			player_grid_coord,
			mut spellbook,
			mut sprite,
		) = player.single_mut();

		let attack = spellbook
			.0
			.get_mut(&active_ability.0)
			.expect("Player has to have an active ability.");

		let dx = enemy_grid_coord.x - player_grid_coord.x;
		let dy = enemy_grid_coord.y - player_grid_coord.y;
		let degrees = f64::atan2(dy.into(), dx.into()).to_degrees();

		#[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
		match degrees.round() as i16 {
			-89..90 => sprite.flip_x = false,
			-180..-90 | 91..=180 => sprite.flip_x = true,
			-90 | 90 => (),
			angle => unreachable!("invalid angle found: {}", angle),
		}

		if attack.last_cast.is_none() && {
			let distance = (dx.pow(2) + dy.pow(2)).to_f64().sqrt();
			distance.floor() <= attack.range.into()
		} {
			*animation_state = TurnState::Running;
			state.turn += 1;
			health.current = health.current.saturating_sub(attack.power);
			let target = player_transform.translation
				+ (enemy_transform.translation.xy() - player_transform.translation.xy())
					.normalize()
					.extend(player_transform.translation.z);

			commands.entity(player_entity).insert(Animator::new(
				Tween::new(
					EaseMethod::Linear,
					Duration::from_secs_f64(0.1),
					TransformPositionLens {
						start: player_transform.translation,
						end:   target,
					},
				)
				.then(
					Tween::new(
						EaseMethod::Linear,
						Duration::from_secs_f64(0.1),
						TransformPositionLens {
							start: target,
							end:   player_transform.translation,
						},
					)
					.with_completed_event(0),
				),
			));

			if attack.cooldown.is_some() {
				attack.last_cast = Some(state.turn);
			}
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
						visibility.set_if_neq(Visibility::Visible);
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
	mut spellbooks: Query<'_, '_, &mut Spellbook>,
) {
	if state.is_changed() {
		for mut spellbook in &mut spellbooks {
			for ability in spellbook.0.values_mut() {
				if ability.cooldown_left(state.turn).is_none() {
					ability.last_cast = None;
				}
			}
		}
	}
}

/// Handle when a death event was sent.
#[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
pub(crate) fn death(
	mut commands: Commands<'_, '_>,
	asset_server: Res<'_, AssetServer>,
	mut deaths: EventReader<'_, '_, DeathEvent>,
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
	layers: Query<'_, '_, (Entity, &LayerMetadata)>,
	mut level_cache: ResMut<'_, LevelCache>,
	mut game_state: ResMut<'_, GameState>,
) {
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
			panic!("Player can't be enemy and visa versa.")
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
					if *game_state
						.keys
						.get(&ItemSource::Loot(entity_iid.clone()))
						.unwrap()
					{
						continue;
					}

					let key_texture =
						asset_server.load::<Image>("Environment/Dungeon Prison/Assets/Props.png");

					let entity = commands
						.spawn((
							Name::new("Key"),
							Key,
							*grid_coords,
							SpriteBundle {
								sprite: Sprite {
									custom_size: Some(Vec2::new(16., 16.)),
									rect: Some(Rect::new(32., 64., 48., 80.)),
									..Sprite::default()
								},
								texture: key_texture,
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
							.insert(*grid_coords, (entity, ItemSource::Loot(entity_iid.clone())))
							.is_none(),
						"found key at this position already",
					);
				}
				_ => unimplemented!(),
			}
		}
	}
}
