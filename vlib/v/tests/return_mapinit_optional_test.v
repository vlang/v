fn a() ?map[string]string {
	return {}
}

fn b() ?map[string]string {
	a := map[string]string{}
	return a
}

fn test_all() ? {
	assert a() ? == {}
	assert b() ? == {}
}
