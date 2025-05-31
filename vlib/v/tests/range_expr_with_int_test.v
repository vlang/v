module main

const min = 1
const max = 3

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
	assert y !in 5..20
	assert y in 4..20
	assert y !in min..max

	assert (t() !in 1..3) == false
	assert r() in 5..6
	assert r() in 4..6
	assert 1.22 in z()?..t()
	assert 1.23 !in z()?..t()
	assert 1.221 in z()?..t()

	a := {
		'f': 2
	}
	assert a['f'] in 1..3
	assert a['f'] in 2..3
	assert a['f'] !in 0..2
	assert a['f'] in min..max
}

fn test_rune() {
	assert `a` in `a`..`h`
	assert `h` !in `a`..`h`

	assert `f` in `a`..`h`
	assert `f` !in `g`..`h`
}
