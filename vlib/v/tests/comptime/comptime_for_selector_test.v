struct Test {
	b ?[]int
}

fn call_generic[U](val U) int {
	mut out := 0
	$for field in U.fields {
		variable := val.$(field.name)
		println(variable)
		for element in val.$(field.name) {
			dump(element)
			out += element
		}
	}
	return out
}

fn test_main() {
	test := Test{
		b: [1, 2, 3]
	}
	assert call_generic(test) == 6
}
