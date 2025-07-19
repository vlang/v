// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module strings

// strings.Builder is used to efficiently append many strings to a large
// dynamically growing buffer, then use the resulting large string. Using
// a string builder is much better for performance/memory usage than doing
// constantly string concatenation.
pub type Builder = []u8

// new_builder returns a new string builder, with an initial capacity of `initial_size`.
pub fn new_builder(initial_size int) Builder {
	mut res := Builder([]u8{cap: initial_size})
	unsafe { res.flags.set(.noslices) }
	return res
}

// reuse_as_plain_u8_array allows using the Builder instance as a plain []u8 return value.
// It is useful, when you have accumulated data in the builder, that you want to
// pass/access as []u8 later, without copying or freeing the buffer.
// NB: you *should NOT use* the string builder instance after calling this method.
// Use only the return value after calling this method.
@[unsafe]
pub fn (mut b Builder) reuse_as_plain_u8_array() []u8 {
	unsafe { b.flags.clear(.noslices) }
	return *b
}

// write_ptr writes `len` bytes provided byteptr to the accumulated buffer
@[unsafe]
pub fn (mut b Builder) write_ptr(ptr &u8, len int) {
	if len == 0 {
		return
	}
	unsafe { b.push_many(ptr, len) }
}

// write_rune appends a single rune to the accumulated buffer
@[manualfree]
pub fn (mut b Builder) write_rune(r rune) {
	mut buffer := [5]u8{}
	res := unsafe { utf32_to_str_no_malloc(u32(r), mut &buffer[0]) }
	if res.len == 0 {
		return
	}
	unsafe { b.push_many(res.str, res.len) }
}

// write_runes appends all the given runes to the accumulated buffer.
pub fn (mut b Builder) write_runes(runes []rune) {
	mut buffer := [5]u8{}
	for r in runes {
		res := unsafe { utf32_to_str_no_malloc(u32(r), mut &buffer[0]) }
		if res.len == 0 {
			continue
		}
		unsafe { b.push_many(res.str, res.len) }
	}
}

// write_u8 appends a single `data` byte to the accumulated buffer
@[inline]
pub fn (mut b Builder) write_u8(data u8) {
	b << data
}

// write_byte appends a single `data` byte to the accumulated buffer
@[inline]
pub fn (mut b Builder) write_byte(data u8) {
	b << data
}

// write_decimal appends a decimal representation of the number `n` into the builder `b`,
// without dynamic allocation. The higher order digits come first, i.e. 6123 will be written
// with the digit `6` first, then `1`, then `2` and `3` last.
@[direct_array_access]
pub fn (mut b Builder) write_decimal(n i64) {
	if n == 0 {
		b.write_u8(0x30)
		return
	}
	mut buf := [25]u8{}
	mut x := if n < 0 { -n } else { n }
	mut i := 24
	for x != 0 {
		nextx := x / 10
		r := x % 10
		buf[i] = u8(r) + 0x30
		x = nextx
		i--
	}
	if n < 0 {
		buf[i] = `-`
		i--
	}
	unsafe { b.write_ptr(&buf[i + 1], 24 - i) }
}

// write implements the io.Writer interface, that is why it returns how many bytes were written to the string builder.
pub fn (mut b Builder) write(data []u8) !int {
	if data.len == 0 {
		return 0
	}
	b << data
	return data.len
}

// drain_builder writes all of the `other` builder content, then re-initialises
// `other`, so that the `other` strings builder is ready to receive new content.
@[manualfree]
pub fn (mut b Builder) drain_builder(mut other Builder, other_new_cap int) {
	if other.len > 0 {
		b << *other
	}
	unsafe { other.free() }
	other = new_builder(other_new_cap)
}

// byte_at returns a byte, located at a given index `i`.
// Note: it can panic, if there are not enough bytes in the strings builder yet.
@[inline]
pub fn (b &Builder) byte_at(n int) u8 {
	return unsafe { (&[]u8(b))[n] }
}

// write appends the string `s` to the buffer
@[expand_simple_interpolation; inline]
pub fn (mut b Builder) write_string(s string) {
	if s.len == 0 {
		return
	}
	unsafe { b.push_many(s.str, s.len) }
	// for c in s {
	// b.buf << c
	// }
	// b.buf << []u8(s)  // TODO
}

// write_string2 appends the strings `s1` and `s2` to the buffer.
@[inline]
pub fn (mut b Builder) write_string2(s1 string, s2 string) {
	if s1.len != 0 {
		unsafe { b.push_many(s1.str, s1.len) }
	}
	if s2.len != 0 {
		unsafe { b.push_many(s2.str, s2.len) }
	}
}

// go_back discards the last `n` bytes from the buffer.
pub fn (mut b Builder) go_back(n int) {
	b.trim(b.len - n)
}

// spart returns a part of the buffer as a string
@[inline]
pub fn (b &Builder) spart(start_pos int, n int) string {
	unsafe {
		mut x := malloc_noscan(n + 1)
		vmemcpy(x, &u8(b.data) + start_pos, n)
		x[n] = 0
		return tos(x, n)
	}
}

// cut_last cuts the last `n` bytes from the buffer and returns them.
pub fn (mut b Builder) cut_last(n int) string {
	cut_pos := b.len - n
	res := b.spart(cut_pos, n)
	b.trim(cut_pos)
	return res
}

// cut_to cuts the string after `pos` and returns it.
// if `pos` is superior to builder length, returns an empty string
// and cancel further operations
pub fn (mut b Builder) cut_to(pos int) string {
	if pos > b.len {
		return ''
	}
	return b.cut_last(b.len - pos)
}

// go_back_to resets the buffer to the given position `pos`.
// Note: pos should be < than the existing buffer length.
pub fn (mut b Builder) go_back_to(pos int) {
	b.trim(pos)
}

// writeln appends the string `s`, and then a newline character.
@[inline]
pub fn (mut b Builder) writeln(s string) {
	// for c in s {
	// b.buf << c
	// }
	if s != '' {
		unsafe { b.push_many(s.str, s.len) }
	}
	// b.buf << []u8(s)  // TODO
	b << u8(`\n`)
}

// writeln2 appends two strings: `s1` + `\n`, and `s2` + `\n`, to the buffer.
@[inline]
pub fn (mut b Builder) writeln2(s1 string, s2 string) {
	if s1 != '' {
		unsafe { b.push_many(s1.str, s1.len) }
	}
	b << u8(`\n`)
	if s2 != '' {
		unsafe { b.push_many(s2.str, s2.len) }
	}
	b << u8(`\n`)
}

// last_n(5) returns 'world'
// buf == 'hello world'
pub fn (b &Builder) last_n(n int) string {
	if n > b.len {
		return ''
	}
	return b.spart(b.len - n, n)
}

// after(6) returns 'world'
// buf == 'hello world'
pub fn (b &Builder) after(n int) string {
	if n >= b.len {
		return ''
	}
	return b.spart(n, b.len - n)
}

// str returns a copy of all of the accumulated buffer content.
// Note: after a call to b.str(), the builder b will be empty, and could be used again.
// The returned string *owns* its own separate copy of the accumulated data that was in
// the string builder, before the .str() call.
pub fn (mut b Builder) str() string {
	b << u8(0)
	bcopy := unsafe { &u8(memdup_noscan(b.data, b.len)) }
	s := unsafe { bcopy.vstring_with_len(b.len - 1) }
	b.clear()
	return s
}

// ensure_cap ensures that the buffer has enough space for at least `n` bytes by growing the buffer if necessary.
pub fn (mut b Builder) ensure_cap(n int) {
	// code adapted from vlib/builtin/array.v
	if n <= b.cap {
		return
	}

	new_data := vcalloc(n * b.element_size)
	if b.data != unsafe { nil } {
		unsafe { vmemcpy(new_data, b.data, b.len * b.element_size) }
		// TODO: the old data may be leaked when no GC is used (ref-counting?)
		if b.flags.has(.noslices) {
			unsafe { free(b.data) }
		}
	}
	unsafe {
		b.data = new_data
		b.offset = 0
		b.cap = n
	}
}

// grow_len grows the length of the buffer by `n` bytes if necessary
@[unsafe]
pub fn (mut b Builder) grow_len(n int) {
	if n <= 0 {
		return
	}

	new_len := b.len + n
	b.ensure_cap(new_len)
	unsafe {
		b.len = new_len
	}
}

// free frees the memory block, used for the buffer.
// Note: do not use the builder, after a call to free().
@[unsafe]
pub fn (mut b Builder) free() {
	if b.data != 0 {
		unsafe { free(b.data) }
		unsafe {
			b.data = nil
		}
	}
}
