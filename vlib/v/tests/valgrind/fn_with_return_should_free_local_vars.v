fn many_strings() []string {
	mut res := []string{}
	for i in 0 .. 10000 {
		res << '${i:05}'
	}
	return res
}

fn abc() int {
	x := many_strings()
	n := x.len
	// Note: the local `x` is no longer used
	// it should be freed *before* the return
	return n
}

fn main() {
	for i in 0 .. 5 {
		nstrings := abc()
		eprintln('nstrings ${i:10}: $nstrings')
	}
}
