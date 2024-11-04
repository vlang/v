module main

fn t() f64 {
	return 1.23
}

fn z() ?f64 {
	return 1.22
}

fn r() int {
	return 5
}

fn test_main() {
	y := 4
	assert y in 1..20
	assert (t() !in 1..3) == false
	assert r() in 5..6
	assert 1.22 in z()?..t()

	a := {
		'f': 2
	}
	assert a['f'] in 1..3
}
