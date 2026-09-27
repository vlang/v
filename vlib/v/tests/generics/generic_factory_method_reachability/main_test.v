import gates

fn test_generic_factory_method_reachability() {
	assert gates.apply(f64(0)) == 1.0
	assert gates.apply(f32(0)) == 1.0
}
