fn has_option() ?int {
	return 51
}

fn test_options_multi_line() {
	var11, var12, var13, var14, var15 := if true {
		println('dd')
		if true {
			has_option()?, has_option()?, has_option()?, 28, has_option()?
		} else {
			has_option()?, has_option()?, has_option()?, has_option()?, has_option()?
		}
	} else {
		has_option()?, 4, 4, 4, 4
	}
	println(var11)
	println(var12)
	println(var13)
	println(var14)
	println(var15)
}
