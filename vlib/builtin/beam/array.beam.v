// BEAM array implementation
// V arrays are represented as Erlang lists on BEAM
module builtin

// array is the generic array type
// On BEAM: represented as Erlang list [...]
pub struct array {
pub mut:
	data   voidptr
	offset int // in bytes, to avoid copying data while making slices
	len    int // length of the array in elements.
	cap    int // capacity of the array in elements.
pub:
	element_size int // size in bytes of one element in the array.
}

// Internal function for new array
fn __new_array(mylen int, cap int, elm_size int) array {
	return array{
		len:          mylen
		cap:          if cap < mylen { mylen } else { cap }
		element_size: elm_size
	}
}

fn __new_array_with_default(mylen int, cap int, elm_size int, val voidptr) array {
	return array{
		len:          mylen
		cap:          if cap < mylen { mylen } else { cap }
		element_size: elm_size
	}
}

fn __new_array_with_multi_default(mylen int, cap int, elm_size int, val voidptr) array {
	return array{
		len:          mylen
		cap:          if cap < mylen { mylen } else { cap }
		element_size: elm_size
	}
}

fn __new_array_with_array_default(mylen int, cap int, elm_size int, val array, depth int) array {
	return array{
		len:          mylen
		cap:          if cap < mylen { mylen } else { cap }
		element_size: elm_size
	}
}

// len returns array length
// Codegen: length(Arr)
pub fn (a array) len() int {
	return a.len
}

// first returns the first element
// Codegen: hd(Arr)
pub fn (a array) first() voidptr {
	return unsafe { nil }
}

// last returns the last element
// Codegen: lists:last(Arr)
pub fn (a array) last() voidptr {
	return unsafe { nil }
}

// reverse returns reversed array
// Codegen: lists:reverse(Arr)
pub fn (mut a array) reverse() array {
	return a
}

// free does nothing on BEAM (garbage collected)
pub fn (a &array) free() {
}

// clone returns a copy
// Codegen: Arr (lists are immutable on BEAM)
pub fn (a &array) clone() array {
	return array{
		data:         a.data
		offset:       a.offset
		len:          a.len
		cap:          a.cap
		element_size: a.element_size
	}
}

// clear empties the array
// Codegen: []
pub fn (mut a array) clear() {
}

// trim trims the array to new length
pub fn (mut a array) trim(index int) {
}

// drop removes first n elements
// Codegen: lists:nthtail(N, Arr)
pub fn (a array) drop(num int) array {
	return a
}

// Internal: used by codegen for array indexing
// arr[i] -> lists:nth(I + 1, Arr)

// Internal: used by codegen for array append
// arr << x -> Arr ++ [X]

// Internal: used by codegen for array slice
// arr[start..end] -> lists:sublist(Arr, Start + 1, End - Start)

// NOTE: On BEAM, arrays are Erlang lists which are immutable
// All "mutation" operations return new lists
// The BEAM codegen handles the SSA transformation

// join concatenates array of strings with separator
// Codegen for []string: lists:join(Sep, Arr) or similar
pub fn (a []string) join(sep string) string {
	// Beam backend stub
	return ''
}

// bytestr converts a byte array to string
// Codegen: iolist_to_binary(Arr)
pub fn (a []u8) bytestr() string {
	// Beam backend stub
	return ''
}

// hex returns hexadecimal representation
pub fn (a []u8) hex() string {
	// Beam backend stub
	return ''
}

// contains checks if value is in array
pub fn (a []string) contains(val string) bool {
	return false
}

// filter returns elements matching predicate (generic stub)
pub fn (a array) filter(pred fn (voidptr) bool) array {
	return a
}

// map transforms elements (generic stub)
pub fn (a array) map(transform fn (voidptr) voidptr) array {
	return a
}

// sort sorts the array (generic stub)
pub fn (mut a array) sort() {
}

// sort_with_compare sorts with custom comparator
pub fn (mut a array) sort_with_compare(cmp fn (voidptr, voidptr) int) {
}

// push appends a single element to the array
// Codegen: Arr ++ [Val]
fn (mut a array) push(val voidptr) {
}

// push_many appends multiple elements from a pointer to the array
// Codegen: Arr ++ lists:sublist(Src, 1, Count) or similar
// Used internally by << operator when appending arrays: arr << other_arr
@[unsafe]
pub fn (mut a array) push_many(val voidptr, size int) {
}

// grow_len ensures that an array has a.len + amount of length
// On BEAM this is a no-op as lists are dynamically sized
@[unsafe]
pub fn (mut a array) grow_len(amount int) {
}

// grow_cap grows the array's capacity by `amount` elements.
// On BEAM this is a no-op as lists don't have separate capacity
pub fn (mut a array) grow_cap(amount int) {
}

// ensure_cap increases the cap of an array if needed
// On BEAM this is a no-op as lists don't have separate capacity
pub fn (mut a array) ensure_cap(required int) {
}

// delete_last removes and returns the last element
// Codegen: {lists:last(Arr), lists:droplast(Arr)}
pub fn (mut a []u8) delete_last() u8 {
	if a.len == 0 {
		return 0
	}
	last := a[a.len - 1]
	a = a[..a.len - 1].clone()
	return last
}

pub fn (mut a []u64) delete_last() u64 {
	if a.len == 0 {
		return 0
	}
	last := a[a.len - 1]
	a = a[..a.len - 1].clone()
	return last
}

pub fn (mut a []int) delete_last() int {
	if a.len == 0 {
		return 0
	}
	last := a[a.len - 1]
	a = a[..a.len - 1].clone()
	return last
}

pub fn (mut a []string) delete_last() string {
	if a.len == 0 {
		return ''
	}
	last := a[a.len - 1]
	a = a[..a.len - 1].clone()
	return last
}

// byterune decodes a UTF-8 byte sequence to a rune
// Codegen: would convert list of bytes to unicode codepoint
pub fn (b []u8) byterune() !rune {
	if b.len == 0 {
		return error('empty byte array')
	}
	if b.len > 4 {
		return error('more than 4 bytes')
	}
	// Stub - returns first byte as rune
	return rune(b[0])
}

// utf8_to_utf32 converts UTF-8 bytes to UTF-32 codepoint
pub fn (b []u8) utf8_to_utf32() !u32 {
	if b.len == 0 {
		return error('empty byte array')
	}
	if b.len > 4 {
		return error('more than 4 bytes')
	}
	// Stub - returns first byte
	return u32(b[0])
}

// pointers returns a new array where each element is the address
// of the corresponding element in the original array.
// On BEAM, this is used for pool processors to work with typed items.
@[unsafe]
pub fn (a array) pointers() []voidptr {
	mut res := []voidptr{}
	for i in 0 .. a.len {
		unsafe { res << a.get_unsafe(i) }
	}
	return res
}

// get_unsafe returns a pointer to the element at the given index
// without bounds checking
@[unsafe]
pub fn (a array) get_unsafe(i int) voidptr {
	// On BEAM, return a reference to the element
	// This is a stub - the actual implementation depends on how
	// the BEAM backend represents array data
	return unsafe { voidptr(usize(a.data) + usize(a.offset) + usize(i * a.element_size)) }
}
