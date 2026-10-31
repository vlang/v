module time

fn test_until() {
	future := now().add(1 * time.second)
	d := until(future)
	assert d > 0
}
