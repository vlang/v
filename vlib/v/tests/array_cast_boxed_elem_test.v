// An explicit cast of a `[]Item` to `[]Named` (or of a `[]Circle` to `[]Shape`)
// boxes every element, like an implicit `[]Named` target does.

interface Named {
	name() string
}

struct Item {
	n int
}

fn (i Item) name() string {
	return 'item${i.n}'
}

type NamedList = []Named

fn (l NamedList) names() []string {
	return l.map(it.name())
}

fn (mut l NamedList) add(n int) {
	l << Item{n}
}

struct Circle {
	r int
}

struct Square {
	side int
}

type Shape = Circle | Square

type Shapes = []Shape

fn (s Shape) describe() string {
	return match s {
		Circle { 'circle${s.r}' }
		Square { 'square${s.side}' }
	}
}

fn test_interface_array_casts() {
	items := [Item{1}, Item{2}]
	named := []Named(items)
	assert named.map(it.name()) == ['item1', 'item2']
	assert NamedList(items).names() == ['item1', 'item2']
	mut list := NamedList(items)
	list.add(3)
	assert list.names() == ['item1', 'item2', 'item3']
	// The cast builds a new array; the source keeps its own elements.
	assert items.len == 2
	fixed := [Item{4}, Item{5}]!
	assert []Named(fixed[..]).map(it.name()) == ['item4', 'item5']
	maybe := ?[]Named(items)
	assert (maybe or { []Named{} }).map(it.name()) == ['item1', 'item2']
	rows := [[Item{7}], [Item{8}, Item{9}]]
	nested := [][]Named(rows)
	assert nested.map(it.map(it.name())) == [['item7'], ['item8', 'item9']]
}

fn test_sum_type_array_casts() {
	circles := [Circle{1}, Circle{2}]
	shapes := []Shape(circles)
	assert shapes.map(it.describe()) == ['circle1', 'circle2']
	mut alias := Shapes(circles)
	alias << Square{3}
	assert alias.map(it.describe()) == ['circle1', 'circle2', 'square3']
	assert circles.len == 2
}

fn test_matching_array_casts_are_kept() {
	named := [Named(Item{6})]
	list := NamedList(named)
	assert list.names() == ['item6']
	assert []Named(list).len == 1
}
