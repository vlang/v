// A `[]Item` stored as a `?[]Named` (or a `[]Circle` as a `?[]Shape`) is
// converted element by element, as it is for a plain `[]Named` target.

interface Named {
	name() string
}

struct Item {
	n int
}

fn (i Item) name() string {
	return 'item${i.n}'
}

struct Circle {
	r int
}

struct Square {
	side int
}

type Shape = Circle | Square

fn (s Shape) describe() string {
	return match s {
		Circle { 'circle${s.r}' }
		Square { 'square${s.side}' }
	}
}

struct Holder {
mut:
	named  ?[]Named
	shapes ?[]Shape
}

fn names(named ?[]Named) []string {
	values := named or { return ['none'] }
	return values.map(it.name())
}

fn describe_all(shapes ?[]Shape) []string {
	values := shapes or { return ['none'] }
	return values.map(it.describe())
}

fn returned_named(items []Item) ?[]Named {
	return items
}

fn returned_shapes(circles []Circle) ?[]Shape {
	return circles
}

fn maybe_items(ok bool) ?[]Item {
	if ok {
		return [Item{5}, Item{6}]
	}
	return none
}

fn maybe_circles(ok bool) ?[]Circle {
	if ok {
		return [Circle{5}]
	}
	return none
}

fn make_items() []Item {
	return [Item{7}]
}

fn test_interface_option_targets() {
	items := [Item{1}, Item{2}]
	h := Holder{
		named: items
	}
	assert names(h.named) == ['item1', 'item2']
	mut h2 := Holder{}
	assert names(h2.named) == ['none']
	h2.named = items
	assert names(h2.named) == ['item1', 'item2']
	assert names(items) == ['item1', 'item2']
	mut o := ?[]Named(none)
	o = items
	assert names(o) == ['item1', 'item2']
	assert names(returned_named(items)) == ['item1', 'item2']
	// The option holds its own boxed copy.
	mut appended := o or { []Named{} }
	appended << Item{3}
	assert names(o) == ['item1', 'item2']
	assert items.len == 2
}

fn test_sum_type_option_targets() {
	circles := [Circle{1}, Circle{2}]
	h := Holder{
		shapes: circles
	}
	assert describe_all(h.shapes) == ['circle1', 'circle2']
	mut h2 := Holder{}
	h2.shapes = circles
	assert describe_all(h2.shapes) == ['circle1', 'circle2']
	assert describe_all(circles) == ['circle1', 'circle2']
	mut o := ?[]Shape(none)
	o = circles
	assert describe_all(o) == ['circle1', 'circle2']
	assert describe_all(returned_shapes(circles)) == ['circle1', 'circle2']
}

fn test_other_array_sources() {
	items := [Item{1}, Item{2}, Item{3}]
	mut h := Holder{}
	h.named = if items.len > 1 { items } else { []Item{} }
	assert names(h.named) == ['item1', 'item2', 'item3']
	h.named = make_items()
	assert names(h.named) == ['item7']
	h.named = items.filter(it.n > 1)
	assert names(h.named) == ['item2', 'item3']
	assert names(items[..1]) == ['item1']
	h.named = maybe_items(false) or { items }
	assert names(h.named) == ['item1', 'item2', 'item3']
}

fn test_option_arrays_of_concrete_values() {
	mut h := Holder{
		named:  maybe_items(true)
		shapes: maybe_circles(true)
	}
	assert names(h.named) == ['item5', 'item6']
	assert describe_all(h.shapes) == ['circle5']
	h.named = maybe_items(false)
	assert names(h.named) == ['none']
	h.shapes = maybe_circles(false)
	assert describe_all(h.shapes) == ['none']
	assert names(maybe_items(true)) == ['item5', 'item6']
	assert describe_all(maybe_circles(true)) == ['circle5']
	mut o := ?[]Named(none)
	o = maybe_items(true)
	assert names(o) == ['item5', 'item6']
}

fn test_matching_option_arrays_are_kept() {
	named := [Named(Item{4})]
	h := Holder{
		named: named
	}
	assert names(h.named) == ['item4']
	maybe := ?[]Named(named)
	assert names(maybe) == ['item4']
	assert names(none) == ['none']
}
