type Value = int | string

type Alias1 = Value

type Alias2 = Alias1

type Alias3 = Alias2

fn test_shared_alias_smartcast_interpolation_is_preserved() {
	shared a := Alias3(Alias2(Alias1(Value(42))))
	rlock a {
		if a is int {
			assert '${a}' == 'Alias3(Value(42))'
			assert '[${a:24}]' == '[       Alias3(Value(42))]'
		} else {
			assert false
		}
	}
}

fn shared_alias_parameter_text(shared a Alias3) string {
	rlock a {
		if a is int {
			return '${a}'
		}
	}
	return 'not int'
}

fn test_shared_alias_parameter_interpolation_is_preserved() {
	shared a := Alias3(Alias2(Alias1(Value(42))))
	assert shared_alias_parameter_text(shared a) == 'Alias3(Value(42))'
}

fn test_shared_alias_string_smartcast_interpolation_is_preserved() {
	shared a := Alias3(Alias2(Alias1(Value('abc'))))
	rlock a {
		if a is string {
			assert '${a}' == "Alias3(Value('abc'))"
			assert '[${a:24}]' == "[    Alias3(Value('abc'))]"
		} else {
			assert false
		}
	}
}
