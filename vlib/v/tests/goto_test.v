fn test_goto() {
	mut i := 0
	a:
	b := 1
	_ = b
	i++
	if i < 3 {
		unsafe {
			goto a
		}
	}
	assert i == 3
}

pub fn test_goto_after_return() {
	a, b, c, d := 4, 5, 6, 7
	for {
		for {
			for {
				if a == 4 {
					if b == 5 {
						if c == 6 {
							if d == 7 {
								unsafe {
									goto finally_ok
								}
							}
						}
					}
				}
			}
		}
	}
	assert false
	return
	finally_ok:
	assert true
}

fn test_goto_with_comptime_tmpl() {
	a := 22
	_ := $tmpl('./tmpl/a.txt')
	println('before goto')

	unsafe {
		goto label
	}
	println('failed goto')
	assert false

	label:
	println('goto label')
	assert true
}
