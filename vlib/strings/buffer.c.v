module strings

// Buffer accumulates bytes in an owned allocation. take_string transfers that
// allocation to a NUL-terminated string without copying the accumulated bytes.
// Do not copy a populated Buffer; pass it by mutable reference. No views into
// an active buffer are exposed, so growth may move its allocation.
@[nocopy]
pub struct Buffer {
mut:
	data &u8 = unsafe { nil }
	len  int
	cap  int // payload capacity, excluding the reserved trailing NUL
}

// new_buffer creates an empty buffer with room for initial_capacity bytes.
// A zero capacity allocates nothing. Negative capacities and max_int panic.
pub fn new_buffer(initial_capacity int) Buffer {
	if initial_capacity < 0 || initial_capacity == max_int {
		panic('strings.Buffer capacity exceeds addressable length')
	}
	if initial_capacity == 0 {
		return Buffer{}
	}
	return Buffer{
		data: unsafe { malloc_noscan(initial_capacity + 1) }
		cap:  initial_capacity
	}
}

// len returns the number of accumulated bytes.
@[inline]
pub fn (b &Buffer) len() int {
	return b.len
}

// capacity returns the number of bytes that fit without growing the buffer.
// The trailing NUL byte is reserved separately and is not included.
@[inline]
pub fn (b &Buffer) capacity() int {
	return b.cap
}

// reserve ensures room for additional bytes after the current content.
// It does not change the length or initialize the additional bytes.
// Negative counts and lengths exceeding max_int - 1 panic.
pub fn (mut b Buffer) reserve(additional int) {
	if additional < 0 || additional > max_int - 1 - b.len {
		panic('strings.Buffer length exceeds addressable length')
	}
	required := b.len + additional
	if required <= b.cap {
		return
	}
	mut capacity := if b.cap < 64 { 64 } else { b.cap }
	for capacity < required {
		if capacity > (max_int - 1) / 2 {
			capacity = required
			break
		}
		capacity *= 2
	}
	// Only the base allocation is retained. No borrowed views can be invalidated
	// by realloc, and the extra byte always leaves room for the terminator.
	if b.data == unsafe { nil } {
		b.data = unsafe { malloc_noscan(capacity + 1) }
	} else {
		b.data = unsafe { realloc_data(b.data, b.cap + 1, capacity + 1) }
	}
	b.cap = capacity
}

// write_u8 appends one byte.
@[inline]
pub fn (mut b Buffer) write_u8(value u8) {
	if b.len == b.cap {
		b.reserve(1)
	}
	// The capacity check establishes this slot before publishing the new length.
	unsafe { b.data[b.len] = value }
	b.len++
}

// write_string appends the bytes of value, including any embedded NUL bytes.
@[inline]
pub fn (mut b Buffer) write_string(value string) {
	// Strings cannot borrow this buffer through its safe API. Their storage
	// remains live until this synchronous copy has finished, including growth.
	unsafe { b.write_ptr(value.str, value.len) }
}

// write appends all bytes in data and implements io.Writer.
pub fn (mut b Buffer) write(data []u8) !int {
	// The buffer exposes no borrowed byte arrays; data owns separate storage.
	unsafe { b.write_ptr(data.data, data.len) }
	return data.len
}

// write_ptr appends count initialized bytes from source. count must be nonnegative.
// For a positive count, source must address that many readable bytes, must not
// point into this buffer's allocation, and must remain valid through any growth.
// The pointer is borrowed synchronously and is not retained. A zero count does nothing.
@[inline; unsafe]
pub fn (mut b Buffer) write_ptr(source &u8, count int) {
	if count == 0 {
		return
	}
	if count > b.cap - b.len || count < 0 {
		b.reserve(count)
	}
	// The caller supplies a readable independent range; reserve established the
	// writable destination. Those ranges cannot overlap and neither pointer escapes.
	unsafe { vmemcpy(b.data + b.len, source, count) }
	b.len += count
}

// set_byte replaces an accumulated byte. An index outside 0..len() panics.
@[inline]
pub fn (mut b Buffer) set_byte(index int, value u8) {
	if index < 0 || index >= b.len {
		panic('strings.Buffer index out of range')
	}
	// The range check establishes that this byte was initialized by an append.
	unsafe { b.set_byte_unchecked(index, value) }
}

// set_byte_unchecked replaces an accumulated byte without checking its index.
// The caller must ensure 0 <= index < len().
@[inline; unsafe]
pub fn (mut b Buffer) set_byte_unchecked(index int, value u8) {
	unsafe { b.data[index] = value }
}

// take_string transfers the allocation to a NUL-terminated string and empties
// the buffer. It does not copy or shrink the allocation: the result may retain
// spare capacity. Later writes and free() do not affect the returned string,
// which owns its allocation and may be freed independently.
// An unallocated empty buffer returns an empty string without allocating.
pub fn (mut b Buffer) take_string() string {
	if b.data == unsafe { nil } {
		return ''
	}
	length := b.len
	b.len = 0
	b.cap = 0
	// Every payload byte is initialized and the allocation includes len+1 bytes.
	// Clear the owner before returning a string over the original base pointer.
	unsafe {
		data := b.data
		b.data = nil
		data[length] = 0
		return data.vstring_with_len(length)
	}
}

// free releases any allocation still owned by the buffer and empties it.
// Strings returned by take_string remain valid. The empty buffer can be reused.
// No copied Buffer may share this allocation.
@[unsafe]
pub fn (mut b Buffer) free() {
	b.len = 0
	b.cap = 0
	// data is nil or the unique base allocation from malloc_noscan/realloc_data.
	unsafe {
		data := b.data
		b.data = nil
		free(data)
	}
}
