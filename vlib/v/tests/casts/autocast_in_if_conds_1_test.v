type MySumType = S1 | S2

struct S1 {
	is_name bool
	name    string
}

struct S2 {
	field2 bool
}

fn test_autocast_in_if_conds() {
	s := MySumType(S1{
		is_name: true
		name:    'bob'
	})

	if s is S1 && s.is_name && s.name == 'bob' {
		println('ok')
		assert true
	} else {
		assert false
	}
}
