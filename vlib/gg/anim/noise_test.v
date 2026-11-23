module anim

fn test_noise_is_deterministic() {
	a := new_noise(42)
	b := new_noise(42)
	for i in 0 .. 20 {
		x := f32(i) * 0.37
		assert a.value2d(x, x * 0.5) == b.value2d(x, x * 0.5)
		assert a.perlin2d(x, x * 0.5) == b.perlin2d(x, x * 0.5)
	}
}

fn test_noise_seed_changes_output() {
	a := new_noise(1)
	b := new_noise(2)
	assert a.value2d(3.3, 4.4) != b.value2d(3.3, 4.4)
	assert a.perlin2d(3.3, 4.4) != b.perlin2d(3.3, 4.4)
}

fn test_noise_range() {
	n := new_noise(7)
	for i in 0 .. 100 {
		x := f32(i) * 0.13
		v := n.value2d(x, x * 0.7)
		assert v >= -1 && v <= 1
	}
}

fn test_noise_continuity() {
	n := new_noise(5)
	a := n.value2d(1.0, 1.0)
	b := n.value2d(1.001, 1.0)
	assert (a - b) < 0.01
}

fn test_fbm() {
	n := new_noise(9)
	assert n.fbm2d(1.2, 3.4, 0, 2.0, 0.5) == 0
	v := n.fbm2d(1.2, 3.4, 4, 2.0, 0.5)
	assert v >= -1 && v <= 1
}
