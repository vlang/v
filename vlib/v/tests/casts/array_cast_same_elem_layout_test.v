// Array and map casts keep their elements as they are, so they are only allowed
// between element types with the same layout. These forms stay valid.
type Bytes = []u8
type MoreBytes = Bytes
type Quad = [4]u8
type MyU8 = u8
type Rows = [][]u8
type Counts = map[string]int

interface Named {
	name() string
}

struct Item {
	id int
}

fn (i Item) name() string {
	return 'item${i.id}'
}

struct Circle {
	r int
}

struct Square {
	s int
}

type Shape = Circle | Square

fn u8s_of(n int) []u8 {
	mut r := []u8{}
	for i in 0 .. n {
		r << u8(i + 1)
	}
	return r
}

fn test_same_layout_dynamic_casts() {
	u8s := u8s_of(3)
	b := Bytes(u8s)
	assert b.len == 3 && b[2] == 3
	back := []u8(b)
	assert back == [u8(1), 2, 3]
	more := MoreBytes(b)
	assert more.hex() == '010203'
	mine := []MyU8(u8s)
	assert mine[1] == 2
	assert []u8(mine) == u8s
	rows := [u8s, u8s_of(1)]
	assert Rows(rows)[1] == [u8(1)]
	maybe := ?[]u8(u8s)
	assert maybe or { []u8{} } == u8s
	maybe_alias := ?Bytes(u8s)
	assert (maybe_alias or { Bytes([]u8{}) }).len == 3
}

fn test_same_layout_fixed_casts() {
	fixed := [u8(1), 2, 3, 4]!
	q := Quad(fixed)
	assert q[3] == 4
	plain := [4]u8(q)
	assert plain == fixed
	dyn := u8s_of(4)
	from_dyn := [4]u8(dyn)
	assert from_dyn == fixed
}

fn test_untyped_literals_take_the_cast_type() {
	wide := []u64([1, 2, 3])
	assert wide == [u64(1), 2, 3]
	from_u8 := []u64([u8(1), 2, 3])
	assert from_u8 == [u64(1), 2, 3]
	fixed_wide := [3]u64([u8(1), 2, 3]!)
	assert fixed_wide == [u64(1), 2, 3]!
	floats := []f64([1, 2])
	assert floats == [1.0, 2.0]
}

fn test_map_alias_cast() {
	m := {
		'a': 1
	}
	c := Counts(m)
	assert c['a'] == 1
}

fn test_boxed_element_casts() {
	items := [Item{1}, Item{2}]
	named := []Named(items)
	assert named.map(it.name()) == ['item1', 'item2']
	circles := [Circle{3}]
	shapes := []Shape(circles)
	assert shapes[0] is Circle
}

fn test_pointer_element_casts() {
	a := 1
	b := 2
	ptrs := [&a, &b]
	raw := []voidptr(ptrs)
	assert raw.len == 2
	assert unsafe { *(&int(raw[1])) } == 2
}
