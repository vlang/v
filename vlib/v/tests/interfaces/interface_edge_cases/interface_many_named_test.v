interface Named {
	name string
}

struct Many {
pub:
	many []Named
}

struct Cat {
pub:
	name string
}

struct Dog {
pub:
	name string
}

fn test_many_named() {
	c := Cat{}
	d := Dog{}
	m := Many{
		many: [c, d]
	}
}
