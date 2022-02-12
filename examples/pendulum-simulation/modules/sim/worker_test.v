module sim

const (
	worker_test_mock_params = SimParams{
		rope_length: 0.25
		bearing_mass: 0.03
		magnet_spacing: 0.05
		magnet_height: 0.03
		magnet_strength: 10
		gravity: 4.9
	}
	worker_test_mock_state = SimState{
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

fn test_compute_result() {
	request := SimRequest{
		id: 0
		params: sim.worker_test_mock_params
		state: sim.worker_test_mock_state
	}
	expected_state := SimState{
		position: vector(
			x: -0.01695723093017133
			y: -0.02937078552673517
			z: 0.002311063475327252
		)
		velocity: vector(
			x: -7.251158929832518
			y: -12.559375680226692
			z: -105.91539687685668
		)
		accel: vector(
			x: -3.789561257387201e-12
			y: 3.410605131648481e-11
			z: 3.031649005909761e-11
		)
	}
	expected := &SimResult{
		state: expected_state
		id: 0
		magnet1_distance: 0.07993696666249224
		magnet2_distance: 0.07993696666249223
		magnet3_distance: 0.03609361938278009
	}
	result := compute_result(request)
	assert result == expected
}
