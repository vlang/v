@[range: '1..10']
type Port = u16

fn Port.from(value u16) !Port {
	if value < 1 || value > 10 {
		return error('bad port')
	}
	return Port(value)
}

@[range: 'nonzero']
type Nonzero = int

fn Nonzero.from(value int) !Nonzero {
	if value == 0 {
		return error('zero')
	}
	return Nonzero(value)
}

fn test_refined_range_valid_literal() {
	p := Port(5)
	assert u16(p) == 5
	assert u16(Port(1)) == 1
	assert u16(Port(10)) == 10
}

fn test_refined_range_checked_constructor() {
	assert u16(Port.from(7)!) == 7
	if _ := Port.from(11) {
		assert false, 'Port.from(11) must fail'
	} else {
		assert err.msg() == 'bad port'
	}
	if _ := Port.from(0) {
		assert false, 'Port.from(0) must fail'
	} else {
		assert err.msg() == 'bad port'
	}
}

fn test_refined_nonzero_values() {
	assert int(Nonzero(5)) == 5
	assert int(Nonzero(-5)) == -5
	assert int(Nonzero.from(3)!) == 3
	if _ := Nonzero.from(0) {
		assert false, 'Nonzero.from(0) must fail'
	} else {
		assert err.msg() == 'zero'
	}
}
