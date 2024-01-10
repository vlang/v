struct Type {
mut:
	something string
}

fn (mut t Type) match_mut_receiver_against_zero_struct_value() {
	zero := Type{}
	match t {
		zero {
			t.something = 'else'
		}
		else {}
	}
}

fn (t &Type) match_ref_receiver_against_zero_struct_value() bool {
	zero := Type{}
	match t {
		zero {
			return true
		}
		else {}
	}
	return false
}

//

fn test_match_struct_type_mut_receiver() {
	mut a := &Type{}
	a.match_mut_receiver_against_zero_struct_value()
	assert a.something == 'else'
	mut b := &Type{
		something: 'another'
	}
	b.match_mut_receiver_against_zero_struct_value()
	assert b.something == 'another'
}

fn test_match_struct_type_mut_receiver_with_non_ref_receiver_in_caller() {
	mut a := Type{}
	a.match_mut_receiver_against_zero_struct_value()
	assert a.something == 'else'
	mut b := Type{
		something: 'another'
	}
	b.match_mut_receiver_against_zero_struct_value()
	assert b.something == 'another'
}

//

fn test_match_struct_type_ref_receiver() {
	a := &Type{}
	assert a.match_ref_receiver_against_zero_struct_value()
	b := &Type{
		something: 'another'
	}
	assert !b.match_ref_receiver_against_zero_struct_value()
}

fn test_match_struct_type_ref_receiver_with_non_ref_receiver_in_caller() {
	a := Type{}
	assert a.match_ref_receiver_against_zero_struct_value()
	b := Type{
		something: 'another'
	}
	assert !b.match_ref_receiver_against_zero_struct_value()
}
