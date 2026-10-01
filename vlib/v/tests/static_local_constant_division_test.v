const static_mix_samples = 4096

@[unsafe]
fn next_amplitude_sample() int {
	mut static samples := static_mix_samples / 8
	samples++
	return samples
}

@[unsafe]
fn next_remainder() int {
	mut static rest := static_mix_samples % 10
	rest += 2
	return rest
}

fn test_static_local_initialized_by_a_constant_division() {
	unsafe {
		assert next_amplitude_sample() == 513
		assert next_amplitude_sample() == 514
		assert next_remainder() == 8
		assert next_remainder() == 10
	}
}
