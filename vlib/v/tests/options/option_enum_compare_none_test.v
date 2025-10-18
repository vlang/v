enum A_ {
	b
}

fn test_main() {
	mut a := ?A_(none)
	a = .b
	b := ?A_(none)
	match true {
		a == ?A_(.b) && a != b {
			assert true
		}
		else {
			assert false
		}
	}
}
