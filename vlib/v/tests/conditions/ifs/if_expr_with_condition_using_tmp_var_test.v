fn option() ?int {
	return 10
}

fn result() !int {
	return 10
}

fn test_if_expr_with_condition_using_tmp_var() {
	num := if option()? == 10 {
		0
	} else if option()? == 15 {
		1
	} else {
		2
	}
	assert num == 0

	num2 := if result()! == 10 {
		0
	} else if result()! == 15 {
		1
	} else {
		2
	}
	assert num2 == 0
}
