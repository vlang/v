type Entity = int

struct Component {
	vals [256]int
}

fn (c Component) get_broken(e Entity) &int {
	return &c.vals[int(e)]
}

fn test_cast_in_ref_fixed_array() {
	c := Component{}
	a := c.get_broken(Entity(10))
	println(*a)
	assert *a == 0
}
