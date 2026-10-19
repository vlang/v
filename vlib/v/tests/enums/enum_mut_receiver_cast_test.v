// A `mut` receiver or parameter is held as a pointer to the caller's value. A cast of it,
// such as `int(c)` on a `mut c Color`, converts the value, not the pointer. Inside `unsafe`,
// an integer cast keeps the address instead (see vlib/v/gen/fastc/pointer_cast_bits_test.v),
// so the casts below that need the value are made outside it.
enum Color {
	red
	green
	blue
}

enum Small as u8 {
	a
	b = 7
}

@[flag]
enum Perm {
	read
	write
	exec
}

struct Point {
mut:
	x int
}

type PointAlias = Point

type Bytes = []u8

type Dict = map[string]int

fn (mut c Color) next() {
	n := int(c) + 1
	c = unsafe { Color(n) }
}

fn (mut c Color) casts() string {
	same := Color(c)
	value := int(c)
	back := unsafe { Color(value) }
	return '${int(c)} ${u8(c)} ${i64(c)} ${f64(int(c))} ${same} ${back}'
}

fn (mut s Small) casts() string {
	return '${int(s)} ${u8(s)} ${u16(s)}'
}

fn (mut p Perm) casts() string {
	return '${int(p)} ${u32(p)}'
}

fn color_times_ten(mut c Color) int {
	return int(c) * 10
}

fn bytes_len(mut a []u8) int {
	b := Bytes(a)
	return b.len
}

fn alias_x(mut p Point) int {
	q := PointAlias(p)
	return q.x
}

fn dict_len(mut m map[string]int) int {
	d := Dict(m)
	return d.len
}

fn test_mut_enum_receiver() {
	mut c := Color.red
	c.next()
	assert c == .green
	c.next()
	assert c == .blue
	assert c.casts() == '2 2 2 2.0 blue blue'
	mut s := Small.b
	assert s.casts() == '7 7 7'
	mut p := Perm.write | Perm.exec
	assert p.casts() == '6 6'
}

fn test_mut_params() {
	mut c := Color.green
	assert color_times_ten(mut c) == 10
	mut a := [u8(1), 2, 3]
	assert bytes_len(mut a) == 3
	mut pt := Point{
		x: 5
	}
	assert alias_x(mut pt) == 5
	mut m := {
		'a': 1
		'b': 2
	}
	assert dict_len(mut m) == 2
}

fn test_fn_literal_params() {
	plus_hundred := fn (mut c Color) int {
		return int(c) + 100
	}
	mut c := Color.blue
	assert plus_hundred(mut c) == 102
	len_of := fn (mut a []u8) int {
		return Bytes(a).len
	}
	mut a := [u8(4), 5]
	assert len_of(mut a) == 2
}
