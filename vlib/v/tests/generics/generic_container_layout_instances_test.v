// Instances of generic functions whose arrays and maps have the element layout
// that the body uses them with. Others are rejected (see
// vlib/v/checker/tests/generic_instance_container_layout_err.vv).

type Byte = u8
type Bytes = []u8
type Counts8 = map[string]u8

struct Wide {
	vals []u64
}

fn to_bytes[T](a []T) []u8 {
	return []u8(a)
}

fn to_alias[T](a []T) Bytes {
	return Bytes(a)
}

fn widen[T](a []T) []u64 {
	return a
}

fn assign[T](a []T) []u64 {
	mut b := []u64{}
	b = a.clone()
	return b
}

fn wrap[T](a []T) Wide {
	return Wide{
		vals: a
	}
}

fn sum(a []i64) i64 {
	mut s := i64(0)
	for v in a {
		s += v
	}
	return s
}

fn total[T](a []T) i64 {
	return sum(a)
}

fn narrow_map[T](m map[string]T) Counts8 {
	return Counts8(m)
}

fn widen_map[T](m map[string]T) map[string]u64 {
	return m
}

fn map_total(m map[string]i64) i64 {
	mut s := i64(0)
	for _, v in m {
		s += v
	}
	return s
}

fn total_map[T](m map[string]T) i64 {
	return map_total(m)
}

fn opt[T](a ?[]T) ?[]u64 {
	return a
}

fn res[T](a []T) ![]u64 {
	return a
}

fn fixed[T](a [3]T) [3]u64 {
	return a
}

fn nested[T](a [][]T) [][]u64 {
	return a
}

fn take_mut(mut a []u64) {
	a << 9
}

fn grow[T](mut a []T) {
	take_mut(mut a)
}

fn append[T](a []T) [][]u64 {
	mut out := [][]u64{}
	out << a
	return out
}

interface Speaker {
	speak() string
}

struct Dog {
	name string
}

fn (d Dog) speak() string {
	return d.name
}

fn speakers[T](a []T) []Speaker {
	return a
}

fn test_array_instances_with_the_expected_elements() {
	assert to_bytes([u8(1), 2, 255]) == [u8(1), 2, 255]
	assert to_bytes([Byte(7), 8]) == [u8(7), 8]
	assert to_alias([u8(3), 4]) == Bytes([u8(3), 4])
	big := u64(1) << 40
	assert widen([u64(1), big]) == [u64(1), big]
	assert assign([u64(5), big]) == [u64(5), big]
	assert wrap([u64(6), big]).vals == [u64(6), big]
	assert total([i64(1), 2, i64(1) << 40]) == 3 + (i64(1) << 40)
}

fn test_wrapped_and_nested_instances_with_the_expected_elements() {
	big := u64(1) << 40
	assert opt[u64]([u64(1), big]) or { [] } == [u64(1), big]
	assert res([u64(2), big]) or { [] } == [u64(2), big]
	assert fixed([u64(3), big, 4]!) == [u64(3), big, 4]!
	assert nested([[u64(5)], [big]]) == [[u64(5)], [big]]
	mut grown := [big]
	grow(mut grown)
	assert grown == [big, 9]
	assert append([u64(6), big]) == [[u64(6), big]]
	assert speakers([Dog{'rex'}, Dog{'fido'}]).map(it.speak()) == ['rex', 'fido']
}

fn test_map_instances_with_the_expected_elements() {
	assert narrow_map({
		'a': u8(1)
		'b': 200
	}) == Counts8({
		'a': u8(1)
		'b': 200
	})
	big := u64(1) << 40
	assert widen_map({
		'a': big
	}) == {
		'a': big
	}
	assert total_map({
		'a': i64(1)
		'b': i64(1) << 40
	}) == 1 + (i64(1) << 40)
}
