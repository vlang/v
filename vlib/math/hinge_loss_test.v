module math

fn test_hinge() {
	assert hinge_loss(1, 0.5) == 0.5
	assert hinge_loss(1, 2) == 0
}
