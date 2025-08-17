struct Struct {
	s string
	i int
	f f64
}

fn test_for_if_fields_check() {
	tst := Struct{
		s: 'tst-s'
		i: 42
	}

	mut result := ''
	$for field in Struct.fields {
		$if field.typ is string || field.typ is string {
			if tst.$(field.name) == 'tst-s' {
				result += '|s'
			}
		} $else $if field.typ is int {
			if tst.$(field.name) == 42 {
				result += '|i'
			}
		} $else {
			result += '|f'
		}
	}
	println(result)

	assert result == '|s|i|f'
}
