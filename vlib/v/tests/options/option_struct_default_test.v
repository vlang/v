struct Element {
	a int
}

@[noinit]
struct RawElement {
mut:
	// tag Tag
	content       []u8
	default_value ?Element
}

fn test_main() {
	a := ?RawElement{}
	dump(a)
}
