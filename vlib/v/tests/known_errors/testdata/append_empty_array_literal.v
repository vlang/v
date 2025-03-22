fn one(mut rows [][]string) {
	rows << ['1a', '1b']
}

fn two(empty bool, mut rows [][]string) {
	if empty {
		rows << [] // C error
		// rows << []string{} // workaround
	} else {
		rows << ['2a', '2b']
	}
}

fn main() {
	mut rows := [][]string{}
	one(mut rows)
	two(true, mut rows)
	println(rows)
}
