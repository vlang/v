module math

fn test_gradient() {
	g := gradient([1.0, 2.0, 4.0, 7.0])
	assert g.len == 4
}
