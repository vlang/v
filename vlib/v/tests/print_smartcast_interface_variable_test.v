interface Any {}

fn do(v Any) string {
	match v {
		int {
			println('> integer answer: ${2 * v}')
			return '${2 * v}'
		}
		string {
			println('> string answer: ${v}, len: ${v.len}')
			return '${v}'
		}
		else {
			return ''
		}
	}
}

fn test_printing_smartcast_interface_variable() {
	s1 := do(123)
	assert s1 == '246'

	s2 := do('abc')
	assert s2 == 'abc'
}
