struct Type {
mut:
	something string
}

type Union = Type

fn (mut t Type) returns() Union {
	zero := Type{}
	match t {
		zero {
			t.something = 'else'
		}
		else {}
	}

	return t
}

fn test_match_struct_type() {
	mut t := &Type{
		something: 'something'
	}
	assert t.something == 'something'
	t.returns()
	assert t.something == 'else'
}
