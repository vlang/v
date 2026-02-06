struct Test[T] {
pub:
	value T
}

struct AuxTest[T] {
pub:
	any_value Test[T]
}

fn decode[T](arg Test[T]) !Test[T] {
	return Test[T]{}
}

pub fn initializing[T]() !AuxTest[T] {
	return AuxTest[T]{
		any_value: decode[T](Test[T]{})!
	}
}

fn test_main() {
	dump(initializing[int]()!)
}
