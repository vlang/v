enum Color {
	red
}

fn test_match_one_branch() {
	col := Color.red
	match col {
		.red {
			assert true
		}
	}
}
