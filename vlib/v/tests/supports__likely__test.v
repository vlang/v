// _likely_(expr) should be compilable, and it should return the expr
fn test_likely_type() {
	assert typeof(_likely_(false)).name == 'bool'
	assert _likely_(false) == false
	assert _likely_(true) == true
}

fn test_likely() {
	if _likely_(2 < 10) {
		assert true
		eprintln('ok, happens every time')
	} else {
		eprintln('happens *infrequently*')
		assert false
	}
}

fn test_likely_returns_the_value_of_its_bool_argument() {
	i := 123
	if _likely_(i < 2) {
		assert false
	} else {
		assert true
	}
}

// _unlikely_ is the same as _likely_ from the V point of view:
fn test_unlikely_type() {
	assert typeof(_unlikely_(false)).name == 'bool'
	assert _unlikely_(false) == false
	assert _unlikely_(true) == true
}
