module main

enum Test {
	a
	b
}

type Sumtype = Test | int

fn test_main() {
	t := ?Sumtype(Test.a)
	match true {
		t == ?Sumtype(5) {
			assert false
		}
		t == ?Sumtype(Test.a) {
			assert true
		}
		else {
			assert false
		}
	}
}
