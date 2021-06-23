module main

fn a() int {
	f := fn (i int) int {
		return i
	}
	return f(1)
}
