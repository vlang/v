module main

struct Parent {
	x int
}

struct Child {
	Parent
}

fn main() {
	c := Child{}
	assert c.x == 0
}
