struct Aa {
	x string
}

struct Bb {
	a int
}

fn give(succ Aa) ?(Aa, Bb) {
	return match succ.x {
		'x' {
			succ, Bb{}
		}
		else {
			error('nok')
		}
	}
}

fn test_fn_with_opt_of_multi_return() {
	res, _ := give(Aa{ x: 'x' }) or { panic('got unexpected err') }

	assert res.x == 'x'
	println('success')
}
