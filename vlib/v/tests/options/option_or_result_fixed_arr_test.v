type Abc = [2]int

// option
fn a() ?Abc {
	return [1, 2]!
}

fn b() ?Abc {
	return none
}

// result
fn aa() !Abc {
	return [1, 2]!
}

fn bb() !Abc {
	return error('b')
}

fn test_option() {
	var_a := dump(a()?)
	var_b := Abc([1, 2]!)
	assert var_a == var_b
}

fn test_result() {
	var_aa := dump(aa()!)
	var_bb := Abc([1, 2]!)
	assert var_aa == var_bb
}

fn test_opt_block() {
	var_a := b() or { [0, 0]! }
	dump(var_a)
	assert var_a == Abc([0, 0]!)
}

fn test_res_block() {
	var_a := bb() or { [0, 0]! }
	dump(var_a)
	assert var_a == Abc([0, 0]!)
}
