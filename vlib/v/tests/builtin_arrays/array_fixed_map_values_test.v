const invalid = [0, 0, 0]!

fn test_main() {
	m := {
		'1,1,1': [1, 1, 1]!
		'?':     invalid
		'2,2,2': [2, 2, 2]!
	}
	assert m['1,1,1'] == [1, 1, 1]!
	assert m['?'] == invalid
}
