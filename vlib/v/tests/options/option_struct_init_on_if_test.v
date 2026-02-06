struct Number {
	natural ?string
	integer string
}

fn number(n int) Number {
	return if n >= 0 { // a single return here
		Number{
			natural: '${n}'
			integer: '${n}'
		}
	} else {
		Number{
			integer: '${n}'
		}
	}
}

fn test_main() {
	mut ret := []Number{}
	for n in [1, 0, -1] {
		ret << number(n)
	}
	assert ret[0] == Number{
		natural: ?string('1')
		integer: '1'
	}
	assert ret[1] == Number{
		natural: ?string('0')
		integer: '0'
	}
	assert ret[2] == Number{
		natural: ?string(none)
		integer: '-1'
	}
}
