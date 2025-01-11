module main

fn make_option() ?string {
	return 'abc'
}

fn main() {
	cols := [make_option()]
	if col := cols[0] {
		assert col == 'abc'
		assert true
		dump(col)
	}
}
