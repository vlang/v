struct Struct {
	s string
	i int
}

fn test_comptime_if_is_selector_type() {
	tst := Struct{
		s: 'tst-s'
		i: 42
	}

	$for field in Struct.fields {
		$if field.typ is string {
			if tst.$(field.name) == '' {
				assert false
			} else if tst.$(field.name) == 'tst-s' {
				assert true
			} else {
				assert false
			}
		}
	}
	$for field in Struct.fields {
		$if field.typ is string || field.typ is string {
			if tst.$(field.name) == '' {
				assert false
			} else if tst.$(field.name) == 'tst-s' {
				assert true
			} else {
				assert false
			}
		}
	}
	$for field in Struct.fields {
		$if field.typ is string && field.typ !is int {
			if tst.$(field.name) == '' {
				assert false
			} else if tst.$(field.name) == 'tst-s' {
				assert true
			} else {
				assert false
			}
		}
	}
}
