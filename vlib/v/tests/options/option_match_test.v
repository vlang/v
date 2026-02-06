fn test_simple_match_expr() {
	mut a := ?int(12)
	match a? {
		12 {
			println(a)
		}
		else {
			println('else')
			assert false
		}
	}

	match a {
		none {
			println('none')
			assert false
		}
		else {
			println('else')
		}
	}

	a = none

	match a {
		none {
			println('none')
		}
		else {
			println('else')
			assert false
		}
	}

	mut b := ?string('aaa')
	match b? {
		'aaa' {
			println(b)
		}
		else {
			println('else')
			assert false
		}
	}

	match b {
		none {
			println('none')
			assert false
		}
		else {
			println('else')
		}
	}

	b = none
	match b {
		none {
			println('none')
		}
		else {
			println('else')
			assert false
		}
	}
}
