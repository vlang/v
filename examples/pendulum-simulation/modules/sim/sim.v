module sim

pub struct SimState {
mut:
	position Vector3D
	velocity Vector3D
	accel    Vector3D
}

pub fn new_state(state SimState) SimState {
	return SimState{
		...state
	}
}

pub fn (mut state SimState) satisfy_rope_constraint(params SimParams) {
	mut rope_vector := params.get_rope_vector(state)
	rope_vector = rope_vector.scale(params.rope_length / rope_vector.norm())
	state.position = vector(z: params.rope_length) + rope_vector
}

pub fn (mut state SimState) increment(delta_t f64, params SimParams) {
	// 1. add up all forces
	// 2. get an accelleration
	// 3. add to velocity
	// 4. ensure rope constraint is satisfied

	// sum up all forces
	forces_sum := params.get_forces_sum(state)

	// get the acceleration
	accel := forces_sum.scale(1.0 / params.bearing_mass)
	state.accel = accel

	// update the velocity
	state.velocity = state.velocity + accel.scale(delta_t)

	// update the position
	state.position = state.position + state.velocity.scale(delta_t)

	// ensure the position satisfies rope constraint
	state.satisfy_rope_constraint(params)
}

pub fn (state SimState) done() bool {
	return state.velocity.norm() < 0.05 && state.accel.norm() < 0.01
}
