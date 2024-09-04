struct Abc {
	id     int
	name   string
	letter string
}

fn join[T](mut old T, new T) {
	$if T is $struct {
		default := T{}
		$for field in T.fields {
			if new.$(field.name) != default.$(field.name) {
				old.$(field.name) = new.$(field.name)
			}
		}
	}
}

fn test_main() {
	mut a := Abc{
		name:   'Peter'
		letter: 'a'
	}
	b := Abc{
		id:     1
		letter: 'b'
	}
	join(mut a, b)

	assert a == Abc{
		name:   'Peter'
		id:     1
		letter: 'b'
	}
}
