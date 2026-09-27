// Appending a differently typed array (`[]Iface << []Concrete`) cannot be a plain bulk copy:
// every element has to be boxed first, so the lowering builds a freshly allocated conversion
// array and pushes from that. Nothing else refers to that array afterwards, so its backing
// buffer has to be released once `push_many` has copied the element bytes out -- while the
// elements themselves stay valid, because the destination owns them now.

interface Shape {
	area() int
}

struct Sq {
	s int
}

fn (x Sq) area() int {
	return x.s * x.s
}

struct Rect {
	w int
	h int
}

fn (x Rect) area() int {
	return x.w * x.h
}

type Num = int | string

struct Holder {
mut:
	items ?[]Shape
}

fn areas(shapes []Shape) []int {
	mut out := []int{}
	for shape in shapes {
		out << shape.area()
	}
	return out
}

fn test_appending_a_concrete_array_to_an_interface_array_boxes_every_element() {
	source := [Sq{2}, Sq{3}]
	mut out := []Shape{}
	out << source
	out << [Rect{2, 5}]
	assert areas(out) == [4, 9, 10]
}

fn test_appended_elements_stay_valid_after_the_conversion_buffer_is_released() {
	source := [Sq{2}, Sq{3}]
	mut out := []Shape{}
	// Repeated appends each build their own conversion array. Every element pushed by an
	// earlier round has to survive the release of the round that produced it.
	for _ in 0 .. 100 {
		out << source
	}
	assert out.len == 200
	assert out[0].area() == 4
	assert out[1].area() == 9
	assert out[out.len - 1].area() == 9
	assert areas(out).len == 200
}

fn test_appending_to_a_sum_type_array_wraps_every_element() {
	mut nums := []Num{}
	nums << [1, 2, 3]
	assert nums.len == 3
	assert '${nums}' == '[Num(1), Num(2), Num(3)]'
}

// The optional-array append is a mirrored copy of the same lowering, so it needs its own
// coverage: a fix applied to only one of the two leaves this path leaking.
fn test_appending_a_converted_array_through_an_optional_target() {
	source := [Sq{2}, Sq{3}]
	mut holder := Holder{
		items: []Shape{}
	}
	holder.items? << source
	holder.items? << source
	got := holder.items or { panic('the optional array must be set') }
	assert got.len == 4
	assert areas(got) == [4, 9, 4, 9]
}

fn test_an_empty_converted_append_is_a_no_op() {
	empty := []Sq{}
	mut out := []Shape{}
	out << empty
	assert out.len == 0
	out << [Sq{5}]
	assert areas(out) == [25]
}
