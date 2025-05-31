module sim

import math

const params_test_mock_params = SimParams{
	rope_length:     0.25
	bearing_mass:    0.03
	magnet_spacing:  0.05
	magnet_height:   0.03
	magnet_strength: 10
	gravity:         4.9
}
const params_test_mock_state = SimState{
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
	accel:    vector(
		x: -8.337034766251843e-11
		y: -2.842170943040401e-10
		z: 1.2126596023639044e-10
	)
}
const params_test_mock_tetha = 2.0 * math.pi / 3.0

pub fn test_get_rope_vector() {
	result := params_test_mock_params.get_rope_vector(params_test_mock_state)
	expected := vector(
		x: -0.016957230930171364
		y: -0.02937078552673521
		z: -0.24768893652467275
	)
	assert result == expected
}

pub fn test_get_forces_sum() {
	result := params_test_mock_params.get_forces_sum(params_test_mock_state)
	expected := vector(
		x: 3.410605131648481e-12
		y: 5.229594535194337e-12
		z: 9.094947017729282e-13
	)
	assert result == expected
}

pub fn test_get_grav_force() {
	result := params_test_mock_params.get_grav_force(params_test_mock_state)
	expected := vector(
		z: -0.147
	)
	assert result == expected
}

pub fn test_get_magnet_position() {
	result := params_test_mock_params.get_magnet_position(params_test_mock_tetha)
	expected := vector(
		x: -0.02499999999999999
		y: 0.04330127018922194
		z: -0.03
	)
	assert result == expected
}

pub fn test_get_magnet_force() {
	result := params_test_mock_params.get_magnet_force(params_test_mock_tetha, params_test_mock_state)
	expected := vector(
		x: -157.4572297692556
		y: 1422.736432604726
		z: -632.5695169850264
	)
	assert result == expected
}

pub fn test_get_magnet_dist() {
	result := params_test_mock_params.get_magnet_dist(params_test_mock_tetha, params_test_mock_state)
	expected := 0.07993696666249227
	assert result == expected
}

pub fn test_get_magnet1_force() {
	result := params_test_mock_params.get_magnet1_force(params_test_mock_state)
	expected := vector(
		x: 1310.8545084099674
		y: 575.0062553126633
		z: -632.5695169850262
	)
	assert result == expected
}

pub fn test_get_magnet2_force() {
	result := params_test_mock_params.get_magnet2_force(params_test_mock_state)
	expected := vector(
		x: -157.4572297692556
		y: 1422.736432604726
		z: -632.5695169850264
	)
	assert result == expected
}

pub fn test_get_magnet3_force() {
	result := params_test_mock_params.get_magnet3_force(params_test_mock_state)
	expected := vector(
		x: -1710.46541088048
		y: -2962.612996234165
		z: -6871.632889552589
	)
	assert result == expected
}

pub fn test_get_tension_force() {
	result := params_test_mock_params.get_tension_force(params_test_mock_state, vector(
		x: 0.0
		y: 0.0
		z: 0.0
	))
	expected := vector(x: 0.0, y: 0.0, z: 0.0)
	assert result == expected
}
