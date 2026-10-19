struct DefaultInner {
	a int
	b int
}

struct DefaultOuter {
	DefaultInner = DefaultInner{
		a: 3
		b: 4
	}
}

struct NestedDefaultInner {
	x int
	y int
}

struct NestedDefaultMid {
	NestedDefaultInner
}

struct NestedDefaultOuter {
	NestedDefaultMid = NestedDefaultMid{
		NestedDefaultInner: NestedDefaultInner{
			x: 1
			y: 2
		}
	}
}

fn test_promoted_field_init_keeps_the_embed_default() {
	value := DefaultOuter{
		b: 7
	}
	assert value.a == 3
	assert value.b == 7
	heap := &DefaultOuter{
		b: 8
	}
	assert heap.a == 3
	assert heap.b == 8
	plain := DefaultOuter{}
	assert plain.a == 3
	assert plain.b == 4
}

fn test_promoted_field_init_through_a_nested_embed_default() {
	nested := NestedDefaultOuter{
		x: 9
	}
	assert nested.x == 9
	assert nested.y == 2
	nested_heap := &NestedDefaultOuter{
		x: 10
	}
	assert nested_heap.x == 10
	assert nested_heap.y == 2
}
