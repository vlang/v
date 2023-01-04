module sim

const (
	sim_test_mock_params = SimParams{
		rope_length: 0.25
		bearing_mass: 0.03
		magnet_spacing: 0.05
		magnet_height: 0.03
		magnet_strength: 10
		gravity: 4.9
	}
	sim_test_mock_state = SimState{
		position: vector(
			x: -0.016957230930171364
			y: -0.02937078552673521
			z: 0.002311063475327252
		)
		velocity: vector(
			x: -7.251158929833104
			y: -12.559375680227724
			z: -105.91539687686381
		)
		accel: vector(
			x: -8.337034766251843e-11
			y: -2.842170943040401e-10
			z: 1.2126596023639044e-10
		)
	}
)

pub fn test_satisfy_rope_constraint() {
	mut state := SimState{
		...sim.sim_test_mock_state
	}

	state.satisfy_rope_constraint(sim.sim_test_mock_params)
	assert state.position.x == -0.016957230930171364
	assert state.position.y == -0.02937078552673521
	assert state.position.z == 0.002311063475327252
	assert state.velocity.x == -7.251158929833104
	assert state.velocity.y == -12.559375680227724
	assert state.velocity.z == -105.91539687686381
	assert state.accel.x == -8.337034766251843e-11
	assert state.accel.y == -2.842170943040401e-10
	assert state.accel.z == 1.2126596023639044e-10
}

pub fn test_increment() {
	mut state := SimState{
		...sim.sim_test_mock_state
	}

	delta_t := 0.0005
	state.increment(delta_t, sim.sim_test_mock_params)
	assert state.position.x == -0.016957230930171364
	assert state.position.y == -0.02937078552673524
	assert state.position.z == 0.0023110634753272796
	assert state.velocity.x == -7.251158929833044
	assert state.velocity.y == -12.559375680227637
	assert state.velocity.z == -105.9153968768638
	assert state.accel.x == 1.2126596023639044e-10
	assert state.accel.y == 1.7431981783981126e-10
	assert state.accel.z == 3.031649005909761e-11
}
