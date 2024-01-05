pub struct Association {
	price       string
	association &Association = unsafe { nil }
}

fn encode_struct[U](a U) {
	mut c := 0
	$for field in U.fields {
		$if field.name == 'association' {
			println(field.name)
			c += 1
			assert ptr_str(a.$(field.name)) == '0'
		} $else {
			println(field.name)
			c += 1
			assert ptr_str(a.$(field.name)) != '0'
		}
	}
	assert c == 2
}

fn test_main() {
	a := Association{}

	assert ptr_str(a.price) != '0'
	assert ptr_str(a.association) == '0'
	encode_struct(a)
}
