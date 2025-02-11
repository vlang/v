fn test_main() {
	_ := if true { [0]! } else { [1]! }
	_ := if true { [0] } else { [1] }

	a := if true { [0]! } else { [1]! }
	b := if true { [0] } else { [1] }
	assert a.str() == '[0]'
	assert b.str() == '[0]'
}

fn test_match() {
	grid_size := 1

	_ := match grid_size {
		3 { ['Small', '3x3']! }
		4 { ['Classic', '4x4']! }
		5 { ['Big', '5x5']! }
		else { ['Large', '6x6']! }
	}

	w := match grid_size {
		3 { ['Small', '3x3']! }
		4 { ['Classic', '4x4']! }
		5 { ['Big', '5x5']! }
		else { ['Large', '6x6']! }
	}
	assert w.str() == "['Large', '6x6']"
}
