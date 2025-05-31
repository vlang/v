fn foofun(op rune) fn () string {
	return match op {
		`1` {
			fn () string {
				return '1 passed'
			}
		}
		`2` {
			fn () string {
				return '2 passed'
			}
		}
		else {
			fn () string {
				return 'Nor 1 or 2 passed'
			}
		}
	}
}

fn test_anon_fn_decl_inside_ternary() {
	a := foofun(`1`)
	println(a())
	assert a() == '1 passed'
}
