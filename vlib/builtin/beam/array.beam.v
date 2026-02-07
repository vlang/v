// BEAM array implementation
// V arrays are represented as Erlang lists on BEAM
//
// IMPORTANT: Most array methods are intercepted by the BEAM codegen
// (core_array_method in core_exprs.v) and emitted as direct Erlang BIF calls:
//   reverse() -> lists:reverse/1
//   sort()    -> lists:sort/1
//   clone()   -> identity (lists are immutable)
//   first()   -> erlang:hd/1
//   last()    -> lists:last/1
//   pop()     -> lists:last/1
//   join()    -> erlang:iolist_to_binary(lists:join/2)
//   index()   -> erlang:length(lists:takewhile/2)
//   delete()  -> lists:delete/2
//   filter()  -> lists:filter/2
//   map()     -> lists:map/2
//   contains()-> lists:member/2
//
// These stubs exist to satisfy the V type checker. They are fallbacks
// only if the codegen does not intercept a call (which should not happen
// for the methods listed above).
module builtin

@[flag]
pub enum ArrayFlags {
	noslices
	noshrink
	nogrow
	nofree
}

// array is the generic array type
// On BEAM: represented as Erlang list [...]
pub struct array {
pub mut:
	data   voidptr
	offset int // in bytes, to avoid copying data while making slices
	len    int // length of the array in elements.
	cap    int // capacity of the array in elements.
	flags  ArrayFlags
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
// Codegen intercepts: erlang:length(Arr)
pub fn (a array) len() int {
	return a.len
}

// first returns the first element
// Codegen intercepts: erlang:hd(Arr)
pub fn (a array) first() voidptr {
	return unsafe { nil }
}

// last returns the last element
// Codegen intercepts: lists:last(Arr)
pub fn (a array) last() voidptr {
	return unsafe { nil }
}

// reverse returns reversed array
// Codegen intercepts: lists:reverse(Arr)
pub fn (mut a array) reverse() array {
	return a
}

// free does nothing on BEAM (garbage collected)
pub fn (a &array) free() {
}

// clone returns a copy
// Codegen intercepts: identity (lists are immutable on BEAM)
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
// On BEAM: returns empty list []
pub fn (mut a array) clear() {
	a.len = 0
	a.cap = 0
}

// trim trims the array to new length
// On BEAM: lists:sublist(Arr, N)
pub fn (mut a array) trim(index int) {
	if index < a.len {
		a.len = index
	}
}

// drop removes first n elements
// Codegen: lists:nthtail(N, Arr)
pub fn (a array) drop(num int) array {
	if num >= a.len {
		return array{
			element_size: a.element_size
		}
	}
	return array{
		len:          a.len - num
		cap:          a.len - num
		element_size: a.element_size
	}
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
// Codegen intercepts: erlang:iolist_to_binary(lists:join(Sep, Arr))
pub fn (a []string) join(sep string) string {
	// Codegen handles this - stub for type checker
	return ''
}

// join_lines joins a string array into a string using a newline delimiter.
@[inline]
pub fn (a []string) join_lines() string {
	return a.join('\n')
}

// bytestr converts a byte array to string
// Codegen intercepts: erlang:list_to_binary(Arr)
pub fn (a []u8) bytestr() string {
	// Codegen handles this - stub for type checker
	return ''
}

// hex returns hexadecimal representation
// Codegen intercepts: binary:encode_hex(erlang:list_to_binary(Arr))
pub fn (a []u8) hex() string {
	// Codegen handles this - stub for type checker
	return ''
}

// contains checks if value is in array
// Codegen intercepts: lists:member(Val, Arr)
pub fn (a []string) contains(val string) bool {
	for item in a {
		if item == val {
			return true
		}
	}
	return false
}

// filter returns elements matching predicate (generic stub)
// Codegen intercepts: lists:filter(Fn, Arr)
pub fn (a array) filter(pred fn (voidptr) bool) array {
	return a
}

// map transforms elements (generic stub)
// Codegen intercepts: lists:map(Fn, Arr)
pub fn (a array) map(transform fn (voidptr) voidptr) array {
	return a
}

// sort sorts the array (generic stub)
// Codegen intercepts: lists:sort(Arr)
pub fn (mut a array) sort() {
}

// sort_with_compare sorts with custom comparator
// On BEAM: lists:sort(Fun, Arr) where Fun returns bool
pub fn (mut a array) sort_with_compare(cmp fn (voidptr, voidptr) int) {
}

// push appends a single element to the array
// Codegen intercepts: Arr ++ [Val]
fn (mut a array) push(val voidptr) {
}

// push_many appends multiple elements from a pointer to the array
// Codegen intercepts: Arr ++ lists:sublist(Src, 1, Count) or similar
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
// On BEAM: uses Erlang's unicode module for proper decoding
pub fn (b []u8) byterune() !rune {
	if b.len == 0 {
		return error('empty byte array')
	}
	if b.len > 4 {
		return error('more than 4 bytes')
	}
	// Decode UTF-8 manually: single byte or multi-byte sequence
	first := b[0]
	if first < 0x80 {
		return rune(first)
	} else if first < 0xC0 {
		return error('invalid UTF-8 start byte')
	} else if first < 0xE0 {
		if b.len < 2 {
			return error('incomplete UTF-8 sequence')
		}
		return rune(((u32(first) & 0x1F) << 6) | (u32(b[1]) & 0x3F))
	} else if first < 0xF0 {
		if b.len < 3 {
			return error('incomplete UTF-8 sequence')
		}
		return rune(((u32(first) & 0x0F) << 12) | ((u32(b[1]) & 0x3F) << 6) | (u32(b[2]) & 0x3F))
	} else {
		if b.len < 4 {
			return error('incomplete UTF-8 sequence')
		}
		return rune(((u32(first) & 0x07) << 18) | ((u32(b[1]) & 0x3F) << 12) | ((u32(b[2]) & 0x3F) << 6) | (u32(b[3]) & 0x3F))
	}
}

// utf8_to_utf32 converts UTF-8 bytes to UTF-32 codepoint
pub fn (b []u8) utf8_to_utf32() !u32 {
	if b.len == 0 {
		return error('empty byte array')
	}
	if b.len > 4 {
		return error('more than 4 bytes')
	}
	first := b[0]
	if first < 0x80 {
		return u32(first)
	} else if first < 0xC0 {
		return error('invalid UTF-8 start byte')
	} else if first < 0xE0 {
		if b.len < 2 {
			return error('incomplete UTF-8 sequence')
		}
		return ((u32(first) & 0x1F) << 6) | (u32(b[1]) & 0x3F)
	} else if first < 0xF0 {
		if b.len < 3 {
			return error('incomplete UTF-8 sequence')
		}
		return ((u32(first) & 0x0F) << 12) | ((u32(b[1]) & 0x3F) << 6) | (u32(b[2]) & 0x3F)
	} else {
		if b.len < 4 {
			return error('incomplete UTF-8 sequence')
		}
		return ((u32(first) & 0x07) << 18) | ((u32(b[1]) & 0x3F) << 12) | ((u32(b[2]) & 0x3F) << 6) | (u32(b[3]) & 0x3F)
	}
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
