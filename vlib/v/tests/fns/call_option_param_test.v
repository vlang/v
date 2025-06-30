pub fn new_group(t ?Texts) Group {
	return Group{}
}

struct Group {
}

struct Texts {
}

fn test_main() {
	a := 0
	_ := if a != 0 { new_group(Texts{}) } else { Group{} }
	_ := if a != 0 { new_group(none) } else { Group{} }
}
