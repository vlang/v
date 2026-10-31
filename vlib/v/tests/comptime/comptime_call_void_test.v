struct Struct {
	a int
	b []string
}

fn (s Struct) func() {}

fn (s Struct) list() []string {
	return []
}

fn (s Struct) num(x int, y []string) int {
	return x + y.len
}

fn test_main() {
	mut hits := []string{}
	$for method in Struct.methods {
		println('${method.name}: ${method.return_type}')
		$if method.return_type == 1 {
			assert method.return_type == 1
			hits << method.name
		} $else {
			assert method.return_type != 1
		}
		$if method.return_type == 11 {
			assert false
		}
	}
	assert hits == ['func']
}

fn test_type_members_compared_with_integers() {
	mut hits := []string{}
	$for method in Struct.methods {
		$if 1 != method.return_type && method.return_type <= 8 {
			hits << 'returns int: ${method.name}'
		}
		$for param in method.params {
			$if param.typ == 8 {
				hits << 'int param: ${param.name}'
			}
		}
	}
	$for field in Struct.fields {
		$if field.typ == 8 {
			assert field.typ == 8
			hits << 'int field: ${field.name}'
		}
		$if field.unaliased_typ != 8 {
			assert field.unaliased_typ != 8
			hits << 'other field: ${field.name}'
		}
	}
	assert hits == ['returns int: num', 'int param: x', 'int field: a', 'other field: b']
}
