type Abc = bool | int | string

fn test_aggregate_is_nodetype() {
	x := Abc('test')

	match x {
		string, int {
			if x is string {
				println('it is a string')
				assert true
			} else {
				assert false
			}
		}
		else {
			assert false
		}
	}
}

fn is_aggregate_branch_string(x Abc) bool {
	match x {
		string, int {
			return x is string
		}
		else {
			return false
		}
	}
}

fn test_aggregate_is_nodetype_in_smartcast_branch() {
	assert is_aggregate_branch_string(Abc('test'))
	assert !is_aggregate_branch_string(Abc(1))
	assert !is_aggregate_branch_string(Abc(false))
}
