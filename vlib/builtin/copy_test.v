struct CopyItem {
	id   int
	name string
}

struct CopyHolder {
mut:
	buf [4]u8
}

type CopyBytes = []u8
type CopyBuf = [4]u8

const copy_table = [10, 20, 30]!

fn copy_into_fixed_param(mut buf [4]int, src []int) int {
	return copy(mut buf, src)
}

fn copy_into_array_param(mut dst []int, src [3]int) int {
	return copy(mut dst, src)
}

fn copy_generic[T](mut dst []T, src []T) int {
	return copy(mut dst, src)
}

fn copy_make_fixed() [3]int {
	return [7, 8, 9]!
}

fn test_copy_array_to_fixed_array() {
	items := [CopyItem{1, 'a'}, CopyItem{2, 'b'}, CopyItem{3, 'c'}, CopyItem{4, 'd'}, CopyItem{5, 'e'}]
	mut fixed := [4]CopyItem{}
	assert copy(mut fixed, items) == 4
	assert fixed[0] == CopyItem{1, 'a'}
	assert fixed[3] == CopyItem{4, 'd'}
	mut short := [4]CopyItem{}
	assert copy(mut short, items[..2]) == 2
	assert short[1] == CopyItem{2, 'b'}
	assert short[2] == CopyItem{}
}

fn test_copy_any_element_type() {
	mut ints := []int{len: 3}
	assert copy(mut ints, [1, 2, 3, 4]) == 3
	assert ints == [1, 2, 3]
	mut strs := []string{len: 2}
	assert copy(mut strs, ['x', 'y', 'z']) == 2
	assert strs == ['x', 'y']
	mut floats := []f64{len: 2}
	assert copy_generic(mut floats, [1.5, 2.5, 3.5]) == 2
	assert floats == [1.5, 2.5]
	// nested arrays are copied shallowly, like an assignment
	mut nested := [][]int{len: 1}
	assert copy(mut nested, [[1, 2]]) == 1
	assert nested[0] == [1, 2]
}

fn test_copy_bytes() {
	mut buf := []u8{len: 3}
	assert copy(mut buf, [u8(1), 2, 3, 4]) == 3
	assert buf == [u8(1), 2, 3]
	mut dst := []u8{len: 6}
	assert copy(mut dst[2..], [u8(7), 8, 9]) == 3
	assert dst == [u8(0), 0, 7, 8, 9, 0]
}

fn test_copy_writes_through_fixed_array_slices() {
	mut buf := [4]u8{}
	assert copy(mut buf[1..], [u8(1), 2, 3, 4, 5]) == 3
	assert buf == [u8(0), 1, 2, 3]!
	assert copy(mut buf[..2], [u8(9), 9, 9]) == 2
	assert buf == [u8(9), 9, 2, 3]!
	mut h := CopyHolder{}
	assert copy(mut h.buf, [u8(5), 6]) == 2
	assert copy(mut h.buf[2..], h.buf[..2]) == 2
	assert h.buf == [u8(5), 6, 5, 6]!
}

fn test_copy_overlapping() {
	mut a := [1, 2, 3, 4, 5]
	assert copy(mut a[1..], a) == 4
	assert a == [1, 1, 2, 3, 4]
	mut b := [1, 2, 3, 4, 5]
	assert copy(mut b, b[2..]) == 3
	assert b == [3, 4, 5, 4, 5]
	mut f := [1, 2, 3, 4, 5]!
	assert copy(mut f, f[2..]) == 3
	assert f == [3, 4, 5, 4, 5]!
}

fn test_copy_string_source() {
	mut bytes := []u8{len: 3}
	assert copy(mut bytes, 'hello') == 3
	assert bytes == 'hel'.bytes()
	mut fixed := [8]u8{}
	word := 'hi!'
	assert copy(mut fixed, word) == 3
	assert fixed[..3] == 'hi!'.bytes()
	assert fixed[3] == 0
}

fn test_copy_fixed_array_sources() {
	mut dst := []int{len: 5}
	local := [4, 5, 6]!
	assert copy(mut dst, local) == 3
	assert dst == [4, 5, 6, 0, 0]
	assert copy(mut dst[3..], copy_table) == 2
	assert dst == [4, 5, 6, 10, 20]
	assert copy(mut dst, copy_make_fixed()) == 3
	assert dst == [7, 8, 9, 10, 20]
}

fn test_copy_mut_params() {
	mut fixed := [4]int{}
	assert copy_into_fixed_param(mut fixed, [1, 2]) == 2
	assert fixed == [1, 2, 0, 0]!
	mut arr := []int{len: 2}
	assert copy_into_array_param(mut arr, [3, 4, 5]!) == 2
	assert arr == [3, 4]
}

fn test_copy_aliases() {
	mut bytes := CopyBytes([]u8{len: 2})
	assert copy(mut bytes, 'xyz') == 2
	assert bytes[1] == `y`
	mut buf := CopyBuf([4]u8{})
	assert copy(mut buf, [u8(1), 2, 3, 4, 5]) == 4
	assert buf[3] == 4
}

fn test_copy_empty() {
	mut empty := []int{}
	assert copy(mut empty, [1, 2]) == 0
	mut ints := [1, 2]
	assert copy(mut ints, []int{}) == 0
	assert ints == [1, 2]
}
