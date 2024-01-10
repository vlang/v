module main

struct MyStruct {
	b string
	c bool
}

fn get_type[T](t T) T {
	return t
}

fn test_main() {
	my := MyStruct{'cool', false}
	$for field2 in MyStruct.fields {
		$if field2.typ is string {
			var := get_type(my.$(field2.name))
			assert dump(var) == 'cool'
		}
		$if field2.typ is bool {
			var := get_type(my.$(field2.name))
			assert dump(var).str() == 'false'
		}
	}
}
