module main

fn has_optional() ?int {
	return 51
}

fn test_optionals_multi_line() {
	var11, var12, var13, var14, var15 := if true {
		println('dd')
		if true {
			has_optional()?, has_optional()?, has_optional()?, 28, has_optional()?
		} else {
			has_optional()?, has_optional()?, has_optional()?, has_optional()?, has_optional()?
		}
	} else {
		has_optional()?, 4, 4, 4, 4
	}
	println(var11)
	println(var12)
	println(var13)
	println(var14)
	println(var15)
}
