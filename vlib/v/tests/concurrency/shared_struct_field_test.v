module main

struct StructA {
	ints shared []int = [0]
}

fn test_main() {
	a := StructA{}
	rlock a.ints {
		dump(a)
		assert true
	}
}
