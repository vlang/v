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

// write_ptr appends `len` bytes from `ptr` to the accumulated buffer.
@[inline; unsafe]
pub fn (mut b Builder) write_ptr(ptr &u8, len int) {
	if len <= 0 || ptr == unsafe { nil } {
		return
	}
	if len <= b.cap - b.len && !b.flags.has(.is_slice) {
		// The source may borrow the builder's buffer, including unused capacity.
		unsafe { vmemmove(&u8(b.data) + b.len, ptr, len) }
		b.len += len
		return
	}
	unsafe { b.push_many(ptr, len) }
}

// write_rune appends a single rune to the accumulated buffer.
// An invalid rune (a surrogate half in 0xD800..0xDFFF, or a value above 0x10FFFF)
// is written as U+FFFD, the replacement character.
@[manualfree]
pub fn (mut b Builder) write_rune(r rune) {
	mut buffer := [5]u8{}
	res := unsafe { utf32_to_str_no_malloc(u32(r), mut &buffer[0]) }
	unsafe { b.push_many(res.str, res.len) }
}

// write_runes appends all the given runes to the accumulated buffer.
// An invalid rune is written as U+FFFD, see `write_rune`.
pub fn (mut b Builder) write_runes(runes []rune) {
	mut buffer := [5]u8{}
	for r in runes {
		res := unsafe { utf32_to_str_no_malloc(u32(r), mut &buffer[0]) }
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

// write_decimal appends a decimal representation of `n` without dynamic allocation.
// The higher order digits come first, i.e. 6123 will be written
// with the digit `6` first, then `1`, then `2` and `3` last.
pub fn (mut b Builder) write_decimal(n i64) {
	if n == 0 {
		b.write_u8(0x30)
		return
	}
	mut mag := u64(n)
	if n < 0 {
		b.write_u8(`-`)
		// Wrapping unsigned negation yields the correct magnitude even for `min_i64`,
		// whose absolute value does not fit in an i64, so this stays allocation-free for
		// every input without a special case for the signed 64-bit minimum.
		mag = u64(0) - mag
	}
	b.write_u_decimal(mag)
}

// write_u_decimal appends a decimal representation of the unsigned number `n` into the
// builder `b`, without dynamic allocation. Unlike `write_decimal`, it covers the entire
// `u64` range (values above `max_i64`). The higher order digits come first, i.e. 6123
// will be written with the digit `6` first, then `1`, then `2` and `3` last.
@[direct_array_access]
pub fn (mut b Builder) write_u_decimal(n u64) {
	if n == 0 {
		b.write_u8(0x30)
		return
	}

	mut buf := [20]u8{} // max_u64 == 18446744073709551615, i.e. 20 digits
	mut x := n
	mut i := 19
	for x != 0 {
		nextx := x / 10
		r := x % 10
		buf[i] = u8(r) + 0x30
		x = nextx
		i--
	}
	unsafe { b.write_ptr(&buf[i + 1], 19 - i) }
}

// write implements the io.Writer interface, that is why it returns how many bytes were written to the string builder.
pub fn (mut b Builder) write(data []u8) !int {
	if data.len == 0 {
		return 0
	}
	unsafe { b.push_many(data.data, data.len) }
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

// write_string appends the string `s` to the buffer.
@[expand_simple_interpolation; inline]
pub fn (mut b Builder) write_string(s string) {
	unsafe { b.write_ptr(s.str, s.len) }
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
// `n` is clamped to the buffer: when it is larger than the length of the buffer,
// the whole buffer is discarded. A negative or zero `n` does nothing.
pub fn (mut b Builder) go_back(n int) {
	if n <= 0 {
		return
	}
	mut new_len := 0
	if n < b.len {
		new_len = b.len - n
	}
	b.trim(new_len)
}

// spart returns a copy of the `n` bytes of the buffer, that start at `start_pos`, as a string.
// The requested range is clamped to the buffer: only the bytes of it that exist are
// returned. The result is shorter than `n` (or empty), when the range reaches outside
// of the buffer, and it is empty, when `n` is negative or zero.
@[inline]
pub fn (b &Builder) spart(start_pos int, n int) string {
	if n <= 0 {
		return ''
	}
	mut start := start_pos
	mut count := n
	if start < 0 {
		// The bytes before the start of the buffer do not exist.
		// `count` is positive here, so adding the negative `start` can not overflow.
		count += start
		start = 0
	}
	// `start` is not negative here, so the subtraction can not overflow.
	available := b.len - start
	if count > available {
		count = available
	}
	if count <= 0 {
		return ''
	}
	unsafe {
		mut x := malloc_noscan(count + 1)
		vmemcpy(x, &u8(b.data) + start, count)
		x[count] = 0
		return tos(x, count)
	}
}

// cut_last cuts the last `n` bytes from the buffer and returns them.
// `n` is clamped to the buffer: when it is larger than the length of the buffer,
// the whole buffer is cut and returned. A negative or zero `n` cuts nothing,
// and returns an empty string.
pub fn (mut b Builder) cut_last(n int) string {
	mut count := n
	if count > b.len {
		count = b.len
	}
	if count <= 0 {
		return ''
	}
	cut_pos := b.len - count
	res := b.spart(cut_pos, count)
	b.trim(cut_pos)
	return res
}

// cut_to cuts the string after `pos` and returns it.
// if `pos` is superior to builder length, returns an empty string
// and cancel further operations
// A negative `pos` is treated as 0: the whole buffer is cut and returned.
pub fn (mut b Builder) cut_to(pos int) string {
	if pos > b.len {
		return ''
	}
	mut n := b.len
	if pos > 0 {
		n -= pos
	}
	return b.cut_last(n)
}

// go_back_to resets the buffer to the given position `pos`.
// Note: pos should be < than the existing buffer length.
// A `pos` that is larger than the length of the buffer does nothing.
// A negative `pos` is treated as 0: the buffer is emptied.
pub fn (mut b Builder) go_back_to(pos int) {
	mut new_len := pos
	if new_len < 0 {
		new_len = 0
	}
	b.trim(new_len)
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
	// Work through the underlying array pointer, instead of taking a pointer
	// cast to the alias receiver. This keeps self-hosted builds from generating
	// an invalid `&b` cast in C.
	mut arr := unsafe { &[]u8(b) }
	arr.ensure_cap(n)
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
		mut arr := unsafe { &[]u8(b) }
		unsafe { arr.free() }
	}
}

// write_repeated_rune appends multiple copies of the same rune to the accumulated buffer.
// An invalid rune is written as U+FFFD, see `write_rune`.
@[direct_array_access]
pub fn (mut b Builder) write_repeated_rune(r rune, count int) {
	if count <= 0 {
		return
	}

	// Convert rune to UTF-8 bytes once
	mut buffer := [5]u8{}
	res := unsafe { utf32_to_str_no_malloc(u32(r), mut &buffer[0]) }

	if res.len == 1 {
		b.ensure_cap(b.len + count)
		unsafe {
			vmemset(&u8(b.data) + b.len, buffer[0], count)
			b.len += count
		}
		return
	} else {
		total_needed := count * res.len
		b.ensure_cap(b.len + total_needed)

		mut dest := unsafe { &u8(b.data) + b.len }
		for _ in 0 .. count {
			unsafe {
				vmemcpy(dest, res.str, res.len)
				dest += res.len
			}
		}
		unsafe {
			b.len += total_needed
		}
	}
}

// IndentParam holds configuration parameters for the indent() function
@[params]
pub struct IndentParam {
pub mut:
	block_start    rune = `{` // Character that starts a new block (+ indent)
	block_end      rune = `}` // Character that ends a new block (- indent)
	indent_char    rune = ` ` // Character used for indentation (space or tab)
	indent_count   int  = 4   // Number of indent_char per indentation level
	starting_level int // Initial indentation level (0 = no initial indent)
}

// IndentState represents the current parsing state of the indent() function
enum IndentState {
	normal    // Normal state, processing regular characters
	in_string // Inside a string literal, ignoring formatting characters
}

// indent formats a string by applying structured indentation based on block delimiters.
// It processes the input string `s` and writes the formatted output to the `Builder` `b`.
// The function preserves content inside string literals (both single and double quotes) and
// configures indentation behavior through the `param` structure.
//
// Key behaviors:
// 1. Removes existing indentation at the beginning of lines.
// 2. Applies new indentation based on block nesting levels.
// 3. Ignores block delimiters and formatting characters inside string literals.
// 4. Keeps empty blocks (e.g., {}) on the same line.
// 5. Inserts newlines after `block_start` and before `block_end` (except for empty blocks).
// 6. Maintains existing line breaks from the input.
//
// Example:
// ```v
// import strings
// input := 'User{name:"John" settings:{theme:"dark"}}'
// mut b := strings.new_builder(64)
// b.indent(input, indent_count: 2)
// println(b.str()) // Formatted output: 'User{\n  name:"John" settings:{\n    theme:"dark"\n  }\n}'
// ```
@[direct_array_access]
pub fn (mut b Builder) indent(s string, param IndentParam) {
	if s.len == 0 {
		return
	}

	mut state := IndentState.normal
	mut indent_level := param.starting_level
	mut string_char := `\0`
	mut at_line_start := true
	for i := 0; i < s.len; i++ {
		c := rune(s[i])
		match state {
			// Normal state: process characters outside of string literals
			.normal {
				match c {
					`"`, `'` { // Note: quote characters for editor display "
						state = .in_string
						string_char = c
						// Add indentation if at the start of a line
						if at_line_start {
							b.write_repeated_rune(param.indent_char,
								indent_level * param.indent_count)
							at_line_start = false
						}
						// Write the opening quote
						b.write_rune(c)
					}
					param.block_start {
						// Start of a new block
						// Add indentation if at the start of a line
						if at_line_start {
							b.write_repeated_rune(param.indent_char,
								indent_level * param.indent_count)
							at_line_start = false
						}

						// Write the block start character
						b.write_rune(c)

						// Check for empty block (e.g., {})
						// Empty blocks stay on the same line
						if i + 1 < s.len && s[i + 1] == param.block_end {
							b.write_rune(param.block_end)
							i++
						} else {
							// Non-empty block: increase indentation and add newline
							indent_level++
							b.write_rune(`\n`)
							at_line_start = true
						}
					}
					param.block_end {
						// End of a block
						// Decrease indentation level (but not below 0)
						if indent_level > 0 {
							indent_level--
						}

						// If not at the start of a line, add a newline
						if !at_line_start {
							b.write_rune(`\n`)
						}

						// Add indentation for the block end
						b.write_repeated_rune(param.indent_char, indent_level * param.indent_count)
						at_line_start = false

						b.write_rune(c)
					}
					` `, `\t`, `\r`, `\n` {
						// Whitespace characters
						// Only write whitespace if not at the start of a line
						if !at_line_start {
							b.write_rune(c)
						}

						// Newline resets the line start flag
						if c == `\n` {
							at_line_start = true
						}
					}
					else {
						// Any other character
						// Add indentation if at the start of a line
						if at_line_start {
							b.write_repeated_rune(param.indent_char,
								indent_level * param.indent_count)
							at_line_start = false
						}
						b.write_rune(c)
					}
				}
			}
			.in_string {
				// Inside a string literal: preserve all characters as-is
				b.write_rune(c)

				// Check for string termination
				// The character must match the opening quote and not be escaped
				if c == string_char {
					if s[i - 1] != `\\` {
						state = .normal
						string_char = `\0`
					}
				}
			}
		}
	}
}
