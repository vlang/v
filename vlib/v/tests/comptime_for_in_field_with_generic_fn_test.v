module main

// exploring `decode` options with nested structs

struct Parent {
	name  string
	age   int
	child Child
}

struct Child {
	name string
	age  int
}

fn inspect<T>(t T) string {
	mut output_str := ''
	println('$T.name')
	$for field in T.fields {
		val := t.$(field.name)
		$if field.typ is string {
			println('  $field.name = $val')
			output_str += '  $field.name = $val\n'
		} $else $if field.typ is int {
			println('  $field.name = $val')
			output_str += '  $field.name = $val\n'
		} $else {
			str := inspect(val)
			output_str += str
		}
	}
	return output_str
}

fn test_comptime_for_in_field_with_generic_fn() {
	p := Parent{
		name: 'parent'
		age: 30
		child: Child{
			name: 'child'
			age: 5
		}
	}
	ret := inspect(p)
	assert ret.contains('name = parent')
	assert ret.contains('age = 30')
	assert ret.contains('name = child')
	assert ret.contains('age = 5')
}
