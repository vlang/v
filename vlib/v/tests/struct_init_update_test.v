module main

struct Particle {
	name string
}

struct ParticleSystem {
	name     string
	particle &Particle
}

struct Ship {
	name            string
	particle_system &ParticleSystem
}

fn test_main() {
	ship := &Ship{
		name:            'ship'
		particle_system: &ParticleSystem{
			name:     'thrust'
			particle: &Particle{
				name: 'thrust_particle'
			}
		}
	}

	ship_clone := &Ship{
		...ship
	}
	assert ship_clone.particle_system.particle == ship.particle_system.particle
}
