module main

fn t() f64 {
	return 1.23
}

fn z() f64 {
	return 1.22
}

fn test_main() {
	y := 4
	assert (y in 1..20) == true
	assert (t() !in 1..3) == false
	assert (t() in z()..t()) == true
}
