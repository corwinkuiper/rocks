#![no_std]
#![no_main]
#![cfg_attr(test, feature(custom_test_frameworks))]
#![cfg_attr(test, reexport_test_harness_main = "test_main")]
#![cfg_attr(test, test_runner(agb::test_runner::test_runner))]
#![deny(clippy::all)]

extern crate alloc;

mod resources;

use core::ops::AddAssign;

use agb::{
    display::{
        affine::AffineMatrix,
        object::{
            AffineMatrixInstance, AffineMode, OamUnmanaged, ObjectUnmanaged, Sprite, SpriteLoader,
        },
        HEIGHT, WIDTH,
    },
    fixnum::{num, Num, Vector2D},
    input::{Button, ButtonController},
    rng::RandomNumberGenerator,
};
use alloc::vec::Vec;
use resources::{BIG_ROCKS, BULLET, SHIP, SHIP_BOOST, SHIP_PARTS, SMALL_ROCKS};

type Number = Num<i32, 10>;
type Vector = Vector2D<Number>;

const FIRST_ROCK_SPAWN: u32 = 256;
const ROCK_SPAWN_CADENCE: u32 = 256;

const ROCK_LIMIT: usize = 6;
const SHIP_PART_LIMIT: usize = 3;
const SHIP_LIMIT: usize = 1;
const AFFINE_MATRIX_LIMIT: usize = 32;
const PARTICLE_LIMIT: usize = AFFINE_MATRIX_LIMIT - SHIP_LIMIT - ROCK_LIMIT - SHIP_PART_LIMIT;

const ROCK_BULLET_MASS_RATIO_SHIFT: i32 = 3;
const SHIP_BULLET_MASS_RATIO_SHIFT: i32 = 3;

const BACKGROUND_COLOUR: u16 = 0x423;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct Angle {
    angle: Number,
}

impl Angle {
    fn transformation_matrix(self) -> AffineMatrix {
        AffineMatrix::from_rotation(-self.angle)
    }
    fn matrix_instance(self) -> AffineMatrixInstance {
        AffineMatrixInstance::new(self.transformation_matrix().to_object_wrapping())
    }
    fn unit_vector(self) -> Vector {
        Vector::new_from_angle(self.angle)
    }
    fn from_random(rng: &mut RandomNumberGenerator) -> Self {
        Angle {
            angle: Number::from_raw(rng.gen()).rem_euclid(1.into()),
        }
    }
}

impl AddAssign<Number> for Angle {
    fn add_assign(&mut self, rhs: Number) {
        self.angle += rhs;
        self.angle = self.angle.rem_euclid(1.into());
    }
}

#[derive(Clone)]
struct Ship {
    position: Vector,
    velocity: Vector,
    sprite: &'static Sprite,
    angular_velocity: Number,
    angle: Angle,
}

impl Ship {
    fn new() -> Ship {
        let mut rng = rng_source();

        Ship {
            position: screen_wrap(
                (Number::from_raw(rng.gen()), Number::from_raw(rng.gen())).into(),
                0,
            ),
            velocity: (0, 0).into(),
            sprite: SHIP.sprite(0),
            angle: Angle::from_random(&mut rng),
            angular_velocity: 0.into(),
        }
    }

    fn object(&self, loader: &mut SpriteLoader) -> ObjectUnmanaged {
        let sprite = loader.get_vram_sprite(self.sprite);
        let mut object = ObjectUnmanaged::new(sprite);
        object
            .set_affine_matrix(self.angle.matrix_instance())
            .set_position((self.position - (8, 8).into()).floor())
            .show_affine(AffineMode::Affine);
        object
    }
}

#[derive(Clone)]
struct Rock {
    position: Vector,
    velocity: Vector,
    angle: Angle,
    sprite: &'static Sprite,
    angular_velocity: Number,
}

impl Rock {
    fn object(&self, loader: &mut SpriteLoader) -> ObjectUnmanaged {
        let sprite = loader.get_vram_sprite(self.sprite);
        let mut object = ObjectUnmanaged::new(sprite);
        object
            .set_affine_matrix(self.angle.matrix_instance())
            .set_position((self.position - (8, 8).into()).floor())
            .show_affine(AffineMode::Affine);
        object
    }
}

#[derive(Clone)]
struct Dust {
    position: Vector,
    velocity: Vector,
    sprite: &'static Sprite,
}

impl Dust {
    fn object(&self, loader: &mut SpriteLoader, instance: AffineMatrixInstance) -> ObjectUnmanaged {
        let sprite = loader.get_vram_sprite(self.sprite);
        let mut object = ObjectUnmanaged::new(sprite);
        object
            .set_affine_matrix(instance)
            .set_position((self.position - (4, 4).into()).floor())
            .show_affine(AffineMode::Affine);
        object
    }
}

#[derive(Clone)]
struct DustParticles {
    parts: [Dust; 4],
    angle: Angle,
    angular_velocity: Number,
    frames_to_live: i32,
}

#[derive(Clone)]
struct ShipParticle {
    sprite: &'static Sprite,
    angle: Angle,
    angular_velocity: Number,
    velocity: Vector,
    position: Vector,
}

impl ShipParticle {
    fn object(&self, loader: &mut SpriteLoader) -> ObjectUnmanaged {
        let sprite = loader.get_vram_sprite(self.sprite);
        let mut object = ObjectUnmanaged::new(sprite);
        object
            .set_affine_matrix(self.angle.matrix_instance())
            .set_position((self.position - (8, 8).into()).floor())
            .show_affine(AffineMode::Affine);
        object
    }
}

const DUST_TTL: i32 = 120;

impl DustParticles {
    fn object(&self, loader: &mut SpriteLoader) -> [ObjectUnmanaged; 4] {
        let scale = Number::new(DUST_TTL) / self.frames_to_live.max(1);
        let matrix = self.angle.transformation_matrix()
            * AffineMatrix::from_scale((scale.change_base(), scale.change_base()).into());
        let instance = AffineMatrixInstance::new(matrix.to_object_wrapping());
        self.parts
            .each_ref()
            .map(move |x| x.object(loader, instance.clone()))
    }
}

#[derive(Clone, Debug)]
struct Bullet {
    position: Vector,
    velocity: Vector,
    inside_ship: bool,
}

impl Bullet {
    fn object(&self, loader: &mut SpriteLoader) -> ObjectUnmanaged {
        let sprite = loader.get_vram_sprite(BULLET.sprite(0));
        let mut object = ObjectUnmanaged::new(sprite);
        object
            .set_position((self.position - (4, 4).into()).floor())
            .show();
        object
    }
}

fn point_collision(a: Vector, b: Vector, a_radius: Number, b_radius: Number) -> bool {
    (a - b).magnitude_squared() < (a_radius + b_radius) * (a_radius + b_radius)
}

fn screen_wrap(v: Vector, size: i32) -> Vector {
    let bounds = Vector::new(WIDTH.into(), HEIGHT.into());
    Vector::new(
        (v.x + size / 2).rem_euclid(bounds.x + size) - size / 2,
        (v.y + size / 2).rem_euclid(bounds.y + size) - size / 2,
    )
}

type Map<T> = Vec<T>;

struct RockSpawner {
    rng: RandomNumberGenerator,
    time_to_next_rock: u32,
}

impl Default for RockSpawner {
    fn default() -> Self {
        Self {
            rng: rng_source(),
            time_to_next_rock: FIRST_ROCK_SPAWN,
        }
    }
}

impl RockSpawner {
    fn spawn_rock(&mut self, rocks: &mut Map<Rock>, ship_position: Vector) {
        self.time_to_next_rock = self.time_to_next_rock.saturating_sub(1);
        if rocks.len() > ROCK_LIMIT {
            return;
        }

        if self.time_to_next_rock == 0 {
            self.time_to_next_rock = ROCK_SPAWN_CADENCE;

            let spawn_position_randomness: Vector = (
                Number::from_raw(self.rng.gen()) % 32,
                Number::from_raw(self.rng.gen()) % 32,
            )
                .into();
            let spawn_position = screen_wrap(
                ship_position + (WIDTH / 2, HEIGHT / 2).into() + spawn_position_randomness,
                0,
            );

            #[allow(clippy::modulo_one)]
            let new_rock = Rock {
                position: spawn_position,
                velocity: (
                    Number::from_raw(self.rng.gen()) % 1,
                    Number::from_raw(self.rng.gen()) % 1,
                )
                    .into(),
                angle: Angle::from_random(&mut self.rng),
                sprite: BIG_ROCKS.animation_sprite(self.rng.gen() as usize),
                angular_velocity: Number::from_raw(self.rng.gen()) % (Number::new(1) / 50),
            };

            rocks.push(new_rock);
        }
    }
}

struct ParticleSpawner {
    rng: RandomNumberGenerator,
}

fn rng_source() -> RandomNumberGenerator {
    RandomNumberGenerator::new_with_seed([
        agb::rng::gen() as u32,
        agb::rng::gen() as u32,
        agb::rng::gen() as u32,
        agb::rng::gen() as u32,
    ])
}

impl Default for ParticleSpawner {
    fn default() -> Self {
        Self { rng: rng_source() }
    }
}

impl ParticleSpawner {
    fn spawn(&mut self, velocity: Vector, position: Vector, particles: &mut Map<DustParticles>) {
        if particles.len() >= PARTICLE_LIMIT {
            return;
        }

        #[allow(clippy::modulo_one)]
        let parts = [0; 4].map(|_| Dust {
            position,
            velocity: velocity
                + (
                    Number::from_raw(self.rng.gen()) % 1,
                    Number::from_raw(self.rng.gen()) % 1,
                )
                    .into(),
            sprite: SMALL_ROCKS.animation_sprite(self.rng.gen() as usize),
        });
        #[allow(clippy::modulo_one)]
        let dust = DustParticles {
            parts,
            angle: Angle::from_random(&mut self.rng),
            angular_velocity: Number::from_raw(self.rng.gen()) % (Number::new(1) / 50),
            frames_to_live: 120,
        };
        particles.push(dust);
    }

    fn spawn_ship(
        &mut self,
        position: Vector,
        velocity: Vector,
        angle: Angle,
        angular_velocity: Number,
        particles: &mut Map<ShipParticle>,
    ) {
        static PARTS: [&Sprite; 3] = [
            SHIP_PARTS.sprite(0),
            SHIP_PARTS.sprite(1),
            SHIP_PARTS.sprite(2),
        ];

        let perpendicular_unit_vector = Vector::new_from_angle(angle.angle + num!(0.25));

        let mut create = |which: i32| {
            let sprite: usize = (which + 1).try_into().expect("should be a sprite");
            let position = position + perpendicular_unit_vector * 3 * which;
            let velocity = velocity
                + perpendicular_unit_vector * which
                + (
                    Number::from_raw(self.rng.gen()) % num!(0.25),
                    Number::from_raw(self.rng.gen()) % num!(0.25),
                )
                    .into();

            ShipParticle {
                sprite: PARTS[sprite],
                angle,
                angular_velocity: angular_velocity
                    + Number::new(which) / 32
                    + Number::from_raw(self.rng.gen()) % (Number::new(1) / 128),
                velocity,
                position,
            }
        };

        particles.push(create(-1));
        particles.push(create(0));
        particles.push(create(1));
    }
}

#[derive(Default)]
struct BulletSpawner {}

impl BulletSpawner {
    fn spawn(
        &mut self,
        velocity: Vector,
        position: Vector,
        angle_unit_vector: Vector,
        bullets: &mut Map<Bullet>,
    ) {
        if bullets.len() > 16 {
            return;
        }

        bullets.push(Bullet {
            position,
            velocity: velocity + angle_unit_vector * 4,
            inside_ship: true,
        });
    }
}

struct Game {
    player_ship: Option<Ship>,
    spawner: RockSpawner,
    bullets: Map<Bullet>,
    rocks: Map<Rock>,
    particles: Map<DustParticles>,
    ship_particles: Map<ShipParticle>,
    particle_spawner: ParticleSpawner,
    bullet_spawner: BulletSpawner,
}

trait RetainExtension<T> {
    fn retain_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut T) -> bool;
}

impl<T> RetainExtension<T> for Option<T> {
    fn retain_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut T) -> bool,
    {
        if !self.as_mut().map_or(false, f) {
            self.take();
        }
    }
}

impl Game {
    fn new() -> Self {
        Self {
            player_ship: Some(Ship::new()),
            spawner: RockSpawner::default(),
            bullets: Map::new(),
            rocks: Map::new(),
            particles: Map::new(),
            ship_particles: Map::new(),
            particle_spawner: ParticleSpawner::default(),
            bullet_spawner: BulletSpawner::default(),
        }
    }

    fn prepare_objects(&self, loader: &mut SpriteLoader) -> Vec<ObjectUnmanaged> {
        let mut objects = Vec::new();
        objects.extend(self.player_ship.iter().map(|ship| ship.object(loader)));
        objects.extend(self.rocks.iter().map(|rock| rock.object(loader)));
        objects.extend(
            self.particles
                .iter()
                .flat_map(|particles| particles.object(loader)),
        );
        objects.extend(self.bullets.iter().map(|rock| rock.object(loader)));
        objects.extend(self.ship_particles.iter().map(|ship| ship.object(loader)));

        objects
    }

    fn update_ship_velocity_with_inputs(&mut self, input: &ButtonController) {
        let angle_change = input.x_tri() as i32;
        let angle_change: Number = angle_change.into();
        let angle_change = angle_change / 64;
        for ship in self.player_ship.iter_mut() {
            ship.angular_velocity = angle_change;
            ship.angle += angle_change;

            let angle_vector = ship.angle.unit_vector();

            if input.is_pressed(Button::A) {
                ship.sprite = SHIP_BOOST.sprite(0);
                ship.velocity += angle_vector / 16;
            } else {
                ship.sprite = SHIP.sprite(0);
            }

            if input.is_just_pressed(Button::B) {
                self.bullet_spawner.spawn(
                    ship.velocity,
                    ship.position,
                    angle_vector,
                    &mut self.bullets,
                );
            }
        }
    }
    fn update_positions(&mut self) {
        for ship in self.player_ship.iter_mut() {
            ship.position += ship.velocity;
            ship.position = screen_wrap(ship.position, 16);
        }
        for rock in self.rocks.iter_mut() {
            rock.position += rock.velocity;
            rock.angle += rock.angular_velocity;
            rock.position = screen_wrap(rock.position, 16);
        }
        for bullet in self.bullets.iter_mut() {
            bullet.position += bullet.velocity;
            bullet.position = screen_wrap(bullet.position, 4);
        }
        for particles in self.particles.iter_mut() {
            for dust in particles.parts.iter_mut() {
                dust.position += dust.velocity;
                dust.position = screen_wrap(dust.position, 8);
            }
            particles.angle += particles.angular_velocity;
        }
        for particle in self.ship_particles.iter_mut() {
            particle.angle += particle.angular_velocity;
            particle.position += particle.velocity;
            particle.position = screen_wrap(particle.position, 16);
        }
    }

    fn generate_new_rocks(&mut self) {
        self.spawner.spawn_rock(
            &mut self.rocks,
            self.player_ship
                .as_ref()
                .map(|x| x.position)
                .unwrap_or_default(),
        );
    }
    fn destroy_rocks(&mut self) {
        self.rocks.retain_mut(|rock| {
            let mut should_destroy: Option<Vector> = None;
            self.bullets.retain(|bullet| {
                let collision = point_collision(rock.position, bullet.position, 8.into(), 2.into());
                if collision {
                    should_destroy = Some(should_destroy.unwrap_or_default() + bullet.velocity);
                }
                !collision
            });

            if let Some(bullet_velocity) = should_destroy {
                self.particle_spawner.spawn(
                    rock.velocity + (bullet_velocity / (1 << ROCK_BULLET_MASS_RATIO_SHIFT)),
                    rock.position,
                    &mut self.particles,
                );
            }
            should_destroy.is_none()
        });
    }
    fn bullet_destroy_player(&mut self) {
        self.player_ship.retain_mut(|ship| {
            let mut should_destroy: Option<Vector> = None;
            self.bullets.retain(|bullet| {
                // bullet hasn't yet left the ship, therefore it cannot destroy it yet
                if bullet.inside_ship {
                    return true;
                }
                let collision = point_collision(ship.position, bullet.position, 8.into(), 2.into());
                if collision {
                    should_destroy = Some(should_destroy.unwrap_or_default() + bullet.velocity);
                }
                !collision
            });

            if let Some(bullet_velocity) = should_destroy {
                let derived_velocity =
                    ship.velocity + (bullet_velocity / (1 << SHIP_BULLET_MASS_RATIO_SHIFT));

                self.particle_spawner.spawn_ship(
                    ship.position,
                    derived_velocity,
                    ship.angle,
                    ship.angular_velocity,
                    &mut self.ship_particles,
                );
            }

            should_destroy.is_none()
        });
    }
    fn rock_destroy_player(&mut self) {
        self.player_ship.retain_mut(|ship| {
            let mut should_destroy = false;
            self.rocks.retain(|rock| {
                let collision = point_collision(ship.position, rock.position, 8.into(), 8.into());
                if collision {
                    self.particle_spawner
                        .spawn(rock.velocity, rock.position, &mut self.particles);
                }
                should_destroy |= collision;
                !collision
            });

            if should_destroy {
                self.particle_spawner.spawn_ship(
                    ship.position,
                    ship.velocity,
                    ship.angle,
                    ship.angular_velocity,
                    &mut self.ship_particles,
                );
            }

            !should_destroy
        });
    }

    fn bullets_left_player(&mut self) {
        for bullet in self.bullets.iter_mut() {
            if !bullet.inside_ship {
                continue;
            }
            if self
                .player_ship
                .iter()
                .all(|ship| !point_collision(ship.position, bullet.position, 8.into(), 2.into()))
            {
                bullet.inside_ship = false;
            }
        }
    }

    fn rock_shared_collision(&mut self) {
        let mut colliding_rock_pairs = Vec::new();
        for (idx, rock) in self.rocks.iter().enumerate() {
            for (other_idx, other_rock) in self.rocks.iter().enumerate().skip(idx + 1) {
                if point_collision(rock.position, other_rock.position, 8.into(), 8.into()) {
                    colliding_rock_pairs.push((idx, other_idx));
                }
            }
        }

        for (a, b) in colliding_rock_pairs {
            let radius_1 = Number::new(8);
            let radius_2 = Number::new(8);
            let fudge = Number::new(1);

            let v1 = self.rocks[a].velocity;
            let x1 = self.rocks[a].position;

            let v2 = self.rocks[b].velocity;
            let x2 = self.rocks[b].position;

            let r = x2 - x1;

            let derived_x1 = r * (radius_1 + radius_2 + fudge) / r.fast_magnitude();

            let v1_prime = v1 - (x1 - x2) * (v1 - v2).dot(x1 - x2) / (x1 - x2).magnitude_squared();
            let v2_prime = v2 - (x2 - x1) * (v2 - v1).dot(x2 - x1) / (x2 - x1).magnitude_squared();

            let x1_prime = (x1 + x2) / 2 - derived_x1 / 2;
            let x2_prime = (x1 + x2) / 2 + derived_x1 / 2;

            self.rocks[a].position = x1_prime;
            self.rocks[b].position = x2_prime;
            self.rocks[a].velocity = v1_prime;
            self.rocks[b].velocity = v2_prime;
        }
    }

    fn clean_particles(&mut self) {
        self.particles.retain_mut(|k| {
            k.frames_to_live -= 1;
            k.frames_to_live >= 0
        })
    }

    fn update(&mut self, input: &ButtonController) {
        self.update_ship_velocity_with_inputs(input);
        self.generate_new_rocks();
        self.destroy_rocks();
        self.bullet_destroy_player();
        self.bullets_left_player();
        self.rock_destroy_player();
        self.rock_shared_collision();
        self.clean_particles();
        self.update_positions();
    }
}

fn play_game(loader: &mut SpriteLoader, oam: &mut OamUnmanaged) {
    let mut game = Game::new();
    let vblank = agb::interrupt::VBlank::get();
    let mut input = ButtonController::new();

    loop {
        let prepared = game.prepare_objects(loader);
        vblank.wait_for_vblank();
        let frame = oam.iter();
        for (object, slot) in prepared.into_iter().zip(frame) {
            slot.set(&object);
        }

        input.update();

        if input.is_just_pressed(Button::B) && game.player_ship.is_none() {
            return;
        }

        game.update(&input);
    }
}

pub fn entry(mut gba: agb::Gba) -> ! {
    let (mut oam, mut loader) = gba.display.object.get_unmanaged();
    let (_, mut vram) = gba.display.video.tiled0();
    vram.set_background_palette_colour(0, 0, BACKGROUND_COLOUR);
    loop {
        play_game(&mut loader, &mut oam);
    }
}
