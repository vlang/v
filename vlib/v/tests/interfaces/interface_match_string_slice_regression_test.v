interface Value {}

fn parse_verb(v Value) !Value {
	match v {
		string {
			if v.len < 4 || (v.len >= 4 && v[3] != `:`) {
				return error('bad verb')
			}
			return v[4..]
		}
		else {
			return v
		}
	}
}

fn test_interface_match_string_slice_regression() {
	got := parse_verb('txt:hello') or { panic(err) }
	assert got as string == 'hello'
}
