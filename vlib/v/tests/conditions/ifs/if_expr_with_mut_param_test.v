// An if-expression that yields a `mut` param must produce a copy of its value,
// not a pointer to it, and the value of the other branch must outlive its block.
struct IfMutParamTable {
	name string
	keys []string
}

fn if_mut_param_other(name string) IfMutParamTable {
	return IfMutParamTable{
		name: name
		keys: ['${name}_id']
	}
}

fn if_mut_param_pick(mut table IfMutParamTable, name string) IfMutParamTable {
	picked := if name == table.name {
		table
	} else {
		if_mut_param_other(name)
	}
	table = IfMutParamTable{
		name: 'changed'
	}
	return picked
}

fn test_if_expr_with_mut_param_copies_the_value() {
	mut table := IfMutParamTable{
		name: 'a'
		keys: ['a_id']
	}
	picked := if_mut_param_pick(mut table, 'a')
	assert picked.name == 'a'
	assert picked.keys == ['a_id']
	assert table.name == 'changed'
}

fn test_if_expr_with_mut_param_keeps_the_other_branch_value() {
	mut table := IfMutParamTable{
		name: 'a'
	}
	picked := if_mut_param_pick(mut table, 'b')
	assert picked.name == 'b'
	assert picked.keys == ['b_id']
}
