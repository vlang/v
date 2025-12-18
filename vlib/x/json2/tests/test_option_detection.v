struct Test {
	name ?string
	age  ?int
}

fn check_option_detection[T](t T) {
	$for f in T.fields {
		$if f is $option {
			println('Found option field: ${f.name}')
			v := t.$(f.name)
			if v != none {
				println('  Value: ${v?}')
			} else {
				println('  Value: none')
			}
		}
	}
}

fn main() {
	test := Test{
		name: 'test'
		age: 42
	}
	check_option_detection(test)
}
