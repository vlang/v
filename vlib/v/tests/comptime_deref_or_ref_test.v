pub struct Association {
	price       string
	association &Association = unsafe { nil }
}

// needed to check the condition for #20400
fn myprintln(s string) string {
	println('---------> ${s}')
	return s
}

fn encode_struct[U](a U) {
	mut c := 0
	$for field in U.fields {
		$if field.name == 'association' {
			x := myprintln(ptr_str(a.$(field.name)))
			println(field.name)
			c += 1
			assert ptr_str(a.$(field.name)) == '0'
		} $else {
			println(field.name)
			c += 1
			assert ptr_str(a.$(field.name)) != '0'
		}
		x := myprintln(ptr_str(a.$(field.name)))
		if field.name == 'association' {
			dump(x)
			assert x == '0'
		}
	}
	dump(c)
	assert c == 2
}

fn test_main() {
	a := Association{}

	assert ptr_str(a.price) != '0'
	assert ptr_str(a.association) == '0'
	encode_struct(a)
}
