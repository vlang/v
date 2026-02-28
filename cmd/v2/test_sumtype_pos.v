type MyUnion = int | string

fn (u MyUnion) tag() int {
	return match u {
		int { 0 }
		string { 1 }
		// else { -1 }
	}
}

fn make_int_union() MyUnion {
	return MyUnion(42)
}

fn make_string_union() MyUnion {
	return MyUnion('hello')
}

fn main() {
	a := make_int_union()
	t := a.tag()
	println(t.str())

	b := make_string_union()
	t2 := b.tag()
	println(t2.str())
}
