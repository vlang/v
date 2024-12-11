module main

struct Foo123 {
	field string = 'foobar'
}

fn gen_func[T](value T) string {
	$if T is i32 {
		return '123'
	} $else $if T is &Foo123 {
		return value.field
	} $else {
		return '123'
	}
}

fn test_main() {
	assert gen_func[&Foo123](&Foo123{}) == 'foobar'
}
