// A fixed array used to initialize a dynamic array field whose elements are interface or
// sum type values must have its elements converted, as in an assignment or an argument.
interface Named {
	name() string
}

struct Item {
	n string
}

fn (i Item) name() string {
	return i.n
}

struct Circle {
	r int
}

type Shape = Circle | Item

struct Holder {
	f []Named
}

struct Outer {
	h Holder
}

struct AnonOuter {
	inner struct {
		g []Named
	}
}

@[params]
struct Opts {
	f []Named
	k int
}

struct Base {
	f []Named
}

struct Embeds {
	Base
	k int
}

struct ShapeHolder {
	f []Shape
}

struct Gen[T] {
	f []Named
	v T
}

struct FixedSrc {
	v [2]Item
}

struct IntHolder {
	f []int
}

const fixed_items = [Item{'c1'}, Item{'c2'}]!

fn names(a []Named) string {
	return a.map(it.name()).join(',')
}

fn shape_names(a []Shape) string {
	mut out := []string{}
	for s in a {
		match s {
			Circle { out << 'c${s.r}' }
			Item { out << s.n }
		}
	}
	return out.join(',')
}

fn opts_names(o Opts) string {
	return names(o.f)
}

fn make_holder(x [2]Item) Holder {
	return Holder{
		f: x
	}
}

fn test_field_init() {
	x := [Item{'a'}, Item{'b'}]!
	h := Holder{
		f: x
	}
	assert names(h.f) == 'a,b'
	positional := Holder{x}
	assert names(positional.f) == 'a,b'
	r := &Holder{
		f: x
	}
	assert names(r.f) == 'a,b'
	assert names(make_holder(x).f) == 'a,b'
	arr := [Holder{
		f: x
	}]
	assert names(arr[0].f) == 'a,b'
	m := {
		'k': Holder{
			f: x
		}
	}
	assert names(m['k'].f) == 'a,b'
}

fn test_nested_and_embedded_field_init() {
	x := [Item{'a'}, Item{'b'}]!
	o := Outer{
		h: Holder{
			f: x
		}
	}
	assert names(o.h.f) == 'a,b'
	anon := AnonOuter{
		inner: struct {
			g: x
		}
	}
	assert names(anon.inner.g) == 'a,b'
	promoted := Embeds{
		f: x
	}
	assert names(promoted.f) == 'a,b'
	embedded := Embeds{
		Base: Base{
			f: x
		}
	}
	assert names(embedded.f) == 'a,b'
	base := Holder{}
	updated := Holder{
		...base
		f: x
	}
	assert names(updated.f) == 'a,b'
	g := Gen[int]{
		f: x
		v: 3
	}
	assert names(g.f) == 'a,b'
}

fn test_params_struct_arg() {
	x := [Item{'a'}, Item{'b'}]!
	assert opts_names(f: x) == 'a,b'
	assert opts_names(Opts{ f: x, k: 1 }) == 'a,b'
}

fn test_field_init_from_const_selector_and_index() {
	x := [Item{'a'}, Item{'b'}]!
	from_const := Holder{
		f: fixed_items
	}
	assert names(from_const.f) == 'c1,c2'
	src := FixedSrc{
		v: x
	}
	from_selector := Holder{
		f: src.v
	}
	assert names(from_selector.f) == 'a,b'
	rows := [x, fixed_items]
	from_index := Holder{
		f: rows[1]
	}
	assert names(from_index.f) == 'c1,c2'
}

fn test_sum_type_field_init() {
	x := [Item{'a'}, Item{'b'}]!
	h := ShapeHolder{
		f: x
	}
	assert shape_names(h.f) == 'a,b'
}

fn test_same_element_field_init_copies() {
	mut x := [1, 2]!
	h := IntHolder{
		f: x
	}
	x[0] = 9
	assert h.f == [1, 2]
}
