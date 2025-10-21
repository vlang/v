enum A_ {
	b
	c
	d
	e
}

fn test_main() {
	mut a := ?A_(none)
	a = .c
	match true {
		a == ?A_(.b) {
			assert false
		}
		a == ?A_(.c) {
			assert true
		}
		a == ?A_(.d) {
			assert false
		}
		else {
			assert false
		}
	}
}
