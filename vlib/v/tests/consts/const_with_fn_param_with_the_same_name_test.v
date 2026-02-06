const zoom_factor = 1.1

fn abc(zoom_factor f64) {
	assert zoom_factor != 1.1, 'zoom_factor here is the parameter name, not the constant from above'
	assert zoom_factor * 10 < 2
	assert zoom_factor * 10 > 0.9
	dump(zoom_factor)
	a := if zoom_factor < 1 { 1 } else { -1 }
	assert a == 1
}

fn def(zfactor f64) {
	assert zfactor != zoom_factor, 'zfactor is different than zoom_factor in both name and value'
	dump(zfactor)
	a := if zfactor < 1 { 1 } else { -1 }
	assert a == 1
}

fn xyz(zfactor f64) {
	assert zfactor != zoom_factor, 'both zfactor and the const zoom_factor should be available, and have different values'
	dump(zfactor)
	z := if zoom_factor < 1 { 1 } else { -1 }
	assert z == -1
	dump(zoom_factor)
	assert zoom_factor * 10 > 10.0
}

fn test_a_const_should_not_be_used_inside_fn_that_have_parameters_with_the_same_name() {
	abc(0.1)
	def(0.1)
	xyz(0.1)
}
