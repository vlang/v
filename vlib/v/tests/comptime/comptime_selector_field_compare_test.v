struct Struct {
	s string
	i int
}

fn test_comptime_selector_field_compare() {
	tst := Struct{
		s: 'tst-s'
		i: 42
	}

	mut result := false

	$for field in Struct.fields {
		$if field.typ !is int && field.typ is string {
			if tst.$(field.name) == 'tst-s' {
				result = true
			}
		}
	}
	assert result
}
