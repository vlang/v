// A self-referential interface -- one with a method whose return type is the
// interface itself -- used to send the interface satisfaction check into
// unbounded recursion, exhausting the checker's stack before it could report
// anything. Declaring the interface was enough on its own; no implementing
// type, cast or call was required.
interface Cloner {
	clone() Cloner
}

struct Leaf {
	id int
}

fn (l Leaf) clone() Cloner {
	return Leaf{
		id: l.id + 1
	}
}

fn test_self_referential_interface_method() {
	c := Cloner(Leaf{
		id: 1
	})
	next := c.clone()
	assert next is Leaf
	if next is Leaf {
		assert next.id == 2
	}
}

// Mutually self-referential interfaces close the same cycle indirectly.
interface Alpha {
	to_beta() Beta
}

interface Beta {
	to_alpha() Alpha
	tag() string
}

struct Both {}

fn (b Both) to_beta() Beta {
	return Both{}
}

fn (b Both) to_alpha() Alpha {
	return Both{}
}

fn (b Both) tag() string {
	return 'both'
}

fn test_mutually_self_referential_interfaces() {
	a := Alpha(Both{})
	b := a.to_beta()
	assert b.tag() == 'both'
	assert b.to_alpha() is Both
}
