struct Struct {
	a     int
	txt   string
	array []string
}

fn get_string[T](s T) string {
	$for field in T.fields {
		$if field.name == 'txt' {
			println(field.name)
			return s.$(field.name)
		}
	}
	return ''
}

fn test_main() {
	s := Struct{
		txt: 'hello'
	}
	assert get_string(s) == 'hello'
}
