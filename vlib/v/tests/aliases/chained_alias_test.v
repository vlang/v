// Chained aliases (`type B = A` where `A` is itself an alias, #27055) let
// modules re-export types from their dependencies without leaking the original
// module name to callers.

type Id = int
type UserId = Id
type AdminId = UserId

struct Box {
	v int
}

type Wrapped = Box
type DoubleWrapped = Wrapped

fn test_primitive_chain() {
	a := AdminId(42)
	b := UserId(a)
	c := Id(b)
	assert int(c) == 42
	assert int(a) == 42
}

fn test_struct_chain() {
	dw := DoubleWrapped{
		v: 7
	}
	w := Wrapped(dw)
	b := Box(w)
	assert b.v == 7
	assert dw.v == 7
}

fn test_chain_assignment_compat() {
	x := AdminId(1)
	mut y := Id(0)
	y = Id(x)
	assert int(y) == 1
}

struct Item {
	v int
}

type ItemPair = [2]Item
type ItemPairAlias = ItemPair

fn test_chain_through_fixed_array_of_non_builtin() {
	a := ItemPair([Item{ v: 1 }, Item{ v: 2 }]!)
	b := ItemPairAlias(a)
	assert b[0].v == 1
	assert b[1].v == 2
}
