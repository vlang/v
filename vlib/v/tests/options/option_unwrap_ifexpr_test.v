type TestSmart = ?string | int

fn test_simple_case() {
	o := ?string('abc')
	dump(o)
	a := if o == none {
		'none'
	} else {
		'${o} exists'
	}
	dump(a)
	assert a == 'abc exists'
}

fn test_comptime_smartcast() {
	t := TestSmart(?string('foobar'))
	$for v in TestSmart.variants {
		if t is v {
			$if t is ?string {
				if t == none {
					panic('error')
				} else {
					assert t == 'foobar'
				}
			}
		}
	}
}
