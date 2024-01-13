// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import strconv

/*
Note: A V string should be/is immutable from the point of view of
    V user programs after it is first created. A V string is
    also slightly larger than the equivalent C string because
    the V string also has an integer length attached.

    This tradeoff is made, since V strings are created just *once*,
    but potentially used *many times* over their lifetime.

    The V string implementation uses a struct, that has a .str field,
    which points to a C style 0 terminated memory block. Although not
    strictly necessary from the V point of view, that additional 0
    is *very useful for C interoperability*.

    The V string implementation also has an integer .len field,
    containing the length of the .str field, excluding the
    terminating 0 (just like the C's strlen(s) would do).

    The 0 ending of .str, and the .len field, mean that in practice:
      a) a V string s can be used very easily, wherever a
         C string is needed, just by passing s.str,
         without a need for further conversion/copying.

      b) where strlen(s) is needed, you can just pass s.len,
         without having to constantly recompute the length of s
         *over and over again* like some C programs do. This is because
         V strings are immutable and so their length does not change.

    Ordinary V code *does not need* to be concerned with the
    additional 0 in the .str field. The 0 *must* be put there by the
    low level string creating functions inside this module.

    Failing to do this will lead to programs that work most of the
    time, when used with pure V functions, but fail in strange ways,
    when used with modules using C functions (for example os and so on).
*/
pub struct string {
pub:
	str &u8 = 0 // points to a C style 0 terminated string of bytes.
	len int // the length of the .str field, excluding the ending 0 byte. It is always equal to strlen(.str).
	// NB string.is_lit is an enumeration of the following:
	// .is_lit == 0 => a fresh string, should be freed by autofree
	// .is_lit == 1 => a literal string from .rodata, should NOT be freed
	// .is_lit == -98761234 => already freed string, protects against double frees.
	// ---------> ^^^^^^^^^ calling free on these is a bug.
	// Any other value means that the string has been corrupted.
mut:
	is_lit int
}

// runes returns an array of all the utf runes in the string `s`
// which is useful if you want random access to them
@[direct_array_access]
pub fn (s string) runes() []rune {
	mut runes := []rune{cap: s.len}
	for i := 0; i < s.len; i++ {
		char_len := utf8_char_len(unsafe { s.str[i] })
		if char_len > 1 {
			end := if s.len - 1 >= i + char_len { i + char_len } else { s.len }
			mut r := unsafe { s[i..end] }
			runes << r.utf32_code()
			i += char_len - 1
		} else {
			runes << unsafe { s.str[i] }
		}
	}
	return runes
}

// cstring_to_vstring creates a new V string copy of the C style string,
// pointed by `s`. This function is most likely what you want to use when
// working with C style pointers to 0 terminated strings (i.e. `char*`).
// It is recommended to use it, unless you *do* understand the implications of
// tos/tos2/tos3/tos4/tos5 in terms of memory management and interactions with
// -autofree and `[manualfree]`.
// It will panic, if the pointer `s` is 0.
@[unsafe]
pub fn cstring_to_vstring(s &char) string {
	return unsafe { tos2(&u8(s)) }.clone()
}

// tos_clone creates a new V string copy of the C style string, pointed by `s`.
// See also cstring_to_vstring (it is the same as it, the only difference is,
// that tos_clone expects `&byte`, while cstring_to_vstring expects &char).
// It will panic, if the pointer `s` is 0.
@[unsafe]
pub fn tos_clone(s &u8) string {
	return unsafe { tos2(s) }.clone()
}

// tos creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will panic, when the pointer `s` is 0.
// See also `tos_clone`.
@[unsafe]
pub fn tos(s &u8, len int) string {
	if s == 0 {
		panic('tos(): nil string')
	}
	return string{
		str: unsafe { s }
		len: len
	}
}

// tos2 creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will calculate the length first, thus it is more costly than `tos`.
// It will panic, when the pointer `s` is 0.
// It is the same as `tos3`, but for &byte pointers, avoiding callsite casts.
// See also `tos_clone`.
@[unsafe]
pub fn tos2(s &u8) string {
	if s == 0 {
		panic('tos2: nil string')
	}
	return string{
		str: unsafe { s }
		len: unsafe { vstrlen(s) }
	}
}

// tos3 creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will calculate the length first, so it is more costly than tos.
// It will panic, when the pointer `s` is 0.
// It is the same as `tos2`, but for &char pointers, avoiding callsite casts.
// See also `tos_clone`.
@[unsafe]
pub fn tos3(s &char) string {
	if s == 0 {
		panic('tos3: nil string')
	}
	return string{
		str: unsafe { &u8(s) }
		len: unsafe { vstrlen_char(s) }
	}
}

// tos4 creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will calculate the length first, so it is more costly than tos.
// It returns '', when given a 0 pointer `s`, it does NOT panic.
// It is the same as `tos5`, but for &byte pointers, avoiding callsite casts.
// See also `tos_clone`.
@[unsafe]
pub fn tos4(s &u8) string {
	if s == 0 {
		return ''
	}
	return string{
		str: unsafe { s }
		len: unsafe { vstrlen(s) }
	}
}

// tos5 creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will calculate the length first, so it is more costly than tos.
// It returns '', when given a 0 pointer `s`, it does NOT panic.
// It is the same as `tos4`, but for &char pointers, avoiding callsite casts.
// See also `tos_clone`.
@[unsafe]
pub fn tos5(s &char) string {
	if s == 0 {
		return ''
	}
	return string{
		str: unsafe { &u8(s) }
		len: unsafe { vstrlen_char(s) }
	}
}

// vstring converts a C style string to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// Note: instead of `&u8(arr.data).vstring()`, do use `tos_clone(&u8(arr.data))`.
// Strings returned from this function will be normal V strings beside that,
// (i.e. they would be freed by V's -autofree mechanism, when they are no longer used).
// See also `tos_clone`.
@[unsafe]
pub fn (bp &u8) vstring() string {
	return string{
		str: unsafe { bp }
		len: unsafe { vstrlen(bp) }
	}
}

// vstring_with_len converts a C style 0 terminated string to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// This method has lower overhead compared to .vstring(), since it
// does not need to calculate the length of the 0 terminated string.
// See also `tos_clone`.
@[unsafe]
pub fn (bp &u8) vstring_with_len(len int) string {
	return string{
		str: unsafe { bp }
		len: len
		is_lit: 0
	}
}

// vstring converts a C style string to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// Strings returned from this function will be normal V strings beside that,
// (i.e. they would be freed by V's -autofree mechanism, when they are
// no longer used).
// Note: instead of `&u8(a.data).vstring()`, use `tos_clone(&u8(a.data))`.
// See also `tos_clone`.
@[unsafe]
pub fn (cp &char) vstring() string {
	return string{
		str: &u8(cp)
		len: unsafe { vstrlen_char(cp) }
		is_lit: 0
	}
}

// vstring_with_len converts a C style 0 terminated string to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// This method has lower overhead compared to .vstring(), since it
// does not calculate the length of the 0 terminated string.
// See also `tos_clone`.
@[unsafe]
pub fn (cp &char) vstring_with_len(len int) string {
	return string{
		str: &u8(cp)
		len: len
		is_lit: 0
	}
}

// vstring_literal converts a C style string to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// NB2: unlike vstring, vstring_literal will mark the string
// as a literal, so it will not be freed by -autofree.
// This is suitable for readonly strings, C string literals etc,
// that can be read by the V program, but that should not be
// managed/freed by it, for example `os.args` is implemented using it.
// See also `tos_clone`.
@[unsafe]
pub fn (bp &u8) vstring_literal() string {
	return string{
		str: unsafe { bp }
		len: unsafe { vstrlen(bp) }
		is_lit: 1
	}
}

// vstring_with_len converts a C style string to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// This method has lower overhead compared to .vstring_literal(), since it
// does not need to calculate the length of the 0 terminated string.
// See also `tos_clone`.
@[unsafe]
pub fn (bp &u8) vstring_literal_with_len(len int) string {
	return string{
		str: unsafe { bp }
		len: len
		is_lit: 1
	}
}

// vstring_literal converts a C style string char* pointer to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// See also `byteptr.vstring_literal` for more details.
// See also `tos_clone`.
@[unsafe]
pub fn (cp &char) vstring_literal() string {
	return string{
		str: &u8(cp)
		len: unsafe { vstrlen_char(cp) }
		is_lit: 1
	}
}

// vstring_literal_with_len converts a C style string char* pointer,
// to a V string.
// Note: the memory block pointed by `bp` is *reused, not copied*!
// This method has lower overhead compared to .vstring_literal(), since it
// does not need to calculate the length of the 0 terminated string.
// See also `tos_clone`.
@[unsafe]
pub fn (cp &char) vstring_literal_with_len(len int) string {
	return string{
		str: &u8(cp)
		len: len
		is_lit: 1
	}
}

// len_utf8 returns the number of runes contained in the string `s`.
pub fn (s string) len_utf8() int {
	mut l := 0
	mut i := 0
	for i < s.len {
		l++
		i += ((0xe5000000 >> ((unsafe { s.str[i] } >> 3) & 0x1e)) & 3) + 1
	}
	return l
}

// clone_static returns an independent copy of a given array.
// It should be used only in -autofree generated code.
@[inline]
fn (a string) clone_static() string {
	return a.clone()
}

// clone returns a copy of the V string `a`.
pub fn (a string) clone() string {
	if a.len == 0 {
		return ''
	}
	mut b := string{
		str: unsafe { malloc_noscan(a.len + 1) }
		len: a.len
	}
	unsafe {
		vmemcpy(b.str, a.str, a.len)
		b.str[a.len] = 0
	}
	return b
}

// replace_once replaces the first occurrence of `rep` with the string passed in `with`.
pub fn (s string) replace_once(rep string, with string) string {
	idx := s.index_(rep)
	if idx == -1 {
		return s.clone()
	}
	return s.substr(0, idx) + with + s.substr(idx + rep.len, s.len)
}

// replace replaces all occurrences of `rep` with the string passed in `with`.
@[direct_array_access]
pub fn (s string) replace(rep string, with string) string {
	if s.len == 0 || rep.len == 0 || rep.len > s.len {
		return s.clone()
	}
	if !s.contains(rep) {
		return s.clone()
	}
	// TODO PERF Allocating ints is expensive. Should be a stack array
	// Get locations of all reps within this string
	mut idxs := []int{cap: s.len / rep.len}
	defer {
		unsafe { idxs.free() }
	}
	mut idx := 0
	for {
		idx = s.index_after(rep, idx)
		if idx == -1 {
			break
		}
		idxs << idx
		idx += rep.len
	}
	// Dont change the string if there's nothing to replace
	if idxs.len == 0 {
		return s.clone()
	}
	// Now we know the number of replacements we need to do and we can calc the len of the new string
	new_len := s.len + idxs.len * (with.len - rep.len)
	mut b := unsafe { malloc_noscan(new_len + 1) } // add space for the null byte at the end
	// Fill the new string
	mut b_i := 0
	mut s_idx := 0
	for _, rep_pos in idxs {
		for i in s_idx .. rep_pos { // copy everything up to piece being replaced
			unsafe {
				b[b_i] = s[i]
			}
			b_i++
		}
		s_idx = rep_pos + rep.len // move string index past replacement
		for i in 0 .. with.len { // copy replacement piece
			unsafe {
				b[b_i] = with[i]
			}
			b_i++
		}
	}
	if s_idx < s.len { // if any original after last replacement, copy it
		for i in s_idx .. s.len {
			unsafe {
				b[b_i] = s[i]
			}
			b_i++
		}
	}
	unsafe {
		b[new_len] = 0
		return tos(b, new_len)
	}
}

struct RepIndex {
	idx     int
	val_idx int
}

// replace_each replaces all occurrences of the string pairs given in `vals`.
// Example: assert 'ABCD'.replace_each(['B','C/','C','D','D','C']) == 'AC/DC'
@[direct_array_access]
pub fn (s string) replace_each(vals []string) string {
	if s.len == 0 || vals.len == 0 {
		return s.clone()
	}
	if vals.len % 2 != 0 {
		eprintln('string.replace_each(): odd number of strings')
		return s.clone()
	}
	// `rep` - string to replace
	// `with` - string to replace with
	// Remember positions of all rep strings, and calculate the length
	// of the new string to do just one allocation.
	mut new_len := s.len
	mut idxs := []RepIndex{cap: 6}
	mut idx := 0
	s_ := s.clone()
	for rep_i := 0; rep_i < vals.len; rep_i += 2 {
		// vals: ['rep1, 'with1', 'rep2', 'with2']
		rep := vals[rep_i]
		with := vals[rep_i + 1]

		for {
			idx = s_.index_after(rep, idx)
			if idx == -1 {
				break
			}
			// The string already found is set to `/del`, to avoid duplicate searches.
			for i in 0 .. rep.len {
				unsafe {
					s_.str[idx + i] = 127
				}
			}
			// We need to remember both the position in the string,
			// and which rep/with pair it refers to.

			idxs << RepIndex{
				idx: idx
				val_idx: rep_i
			}

			idx += rep.len
			new_len += with.len - rep.len
		}
	}

	// Dont change the string if there's nothing to replace
	if idxs.len == 0 {
		return s.clone()
	}
	idxs.sort(a.idx < b.idx)
	mut b := unsafe { malloc_noscan(new_len + 1) } // add space for 0 terminator
	// Fill the new string
	mut idx_pos := 0
	mut cur_idx := idxs[idx_pos]
	mut b_i := 0
	for i := 0; i < s.len; i++ {
		if i == cur_idx.idx {
			// Reached the location of rep, replace it with "with"
			rep := vals[cur_idx.val_idx]
			with := vals[cur_idx.val_idx + 1]
			for j in 0 .. with.len {
				unsafe {
					b[b_i] = with[j]
				}
				b_i++
			}
			// Skip the length of rep, since we just replaced it with "with"
			i += rep.len - 1
			// Go to the next index
			idx_pos++
			if idx_pos < idxs.len {
				cur_idx = idxs[idx_pos]
			}
		} else {
			// Rep doesnt start here, just copy
			unsafe {
				b[b_i] = s.str[i]
			}
			b_i++
		}
	}
	unsafe {
		b[new_len] = 0
		return tos(b, new_len)
	}
}

// replace_char replaces all occurrences of the character `rep` multiple occurrences of the character passed in `with` with respect to `repeat`.
// Example: assert '\tHello!'.replace_char(`\t`,` `,8) == '        Hello!'
@[direct_array_access]
pub fn (s string) replace_char(rep u8, with u8, repeat int) string {
	$if !no_bounds_checking {
		if repeat <= 0 {
			panic('string.replace_char(): tab length too short')
		}
	}
	if s.len == 0 {
		return s.clone()
	}
	// TODO Allocating ints is expensive. Should be a stack array
	// - string.replace()
	mut idxs := []int{cap: s.len}
	defer {
		unsafe { idxs.free() }
	}
	// No need to do a contains(), it already traverses the entire string
	for i, ch in s {
		if ch == rep { // Found char? Mark its location
			idxs << i
		}
	}
	if idxs.len == 0 {
		return s.clone()
	}
	// Now we know the number of replacements we need to do and we can calc the len of the new string
	new_len := s.len + idxs.len * (repeat - 1)
	mut b := unsafe { malloc_noscan(new_len + 1) } // add space for the null byte at the end
	// Fill the new string
	mut b_i := 0
	mut s_idx := 0
	for rep_pos in idxs {
		for i in s_idx .. rep_pos { // copy everything up to piece being replaced
			unsafe {
				b[b_i] = s[i]
			}
			b_i++
		}
		s_idx = rep_pos + 1 // move string index past replacement
		for _ in 0 .. repeat { // copy replacement piece
			unsafe {
				b[b_i] = with
			}
			b_i++
		}
	}
	if s_idx < s.len { // if any original after last replacement, copy it
		for i in s_idx .. s.len {
			unsafe {
				b[b_i] = s[i]
			}
			b_i++
		}
	}
	unsafe {
		b[new_len] = 0
		return tos(b, new_len)
	}
}

// normalize_tabs replaces all tab characters with `tab_len` amount of spaces
// Example: assert '\t\tpop rax\t; pop rax'.normalize_tabs(2) == '    pop rax  ; pop rax'
@[inline]
pub fn (s string) normalize_tabs(tab_len int) string {
	return s.replace_char(`\t`, ` `, tab_len)
}

// bool returns `true` if the string equals the word "true" it will return `false` otherwise.
@[inline]
pub fn (s string) bool() bool {
	return s == 'true' || s == 't' // TODO t for pg, remove
}

// int returns the value of the string as an integer `'1'.int() == 1`.
@[inline]
pub fn (s string) int() int {
	return int(strconv.common_parse_int(s, 0, 32, false, false) or { 0 })
}

// i64 returns the value of the string as i64 `'1'.i64() == i64(1)`.
@[inline]
pub fn (s string) i64() i64 {
	return strconv.common_parse_int(s, 0, 64, false, false) or { 0 }
}

// i8 returns the value of the string as i8 `'1'.i8() == i8(1)`.
@[inline]
pub fn (s string) i8() i8 {
	return i8(strconv.common_parse_int(s, 0, 8, false, false) or { 0 })
}

// i16 returns the value of the string as i16 `'1'.i16() == i16(1)`.
@[inline]
pub fn (s string) i16() i16 {
	return i16(strconv.common_parse_int(s, 0, 16, false, false) or { 0 })
}

// f32 returns the value of the string as f32 `'1.0'.f32() == f32(1)`.
@[inline]
pub fn (s string) f32() f32 {
	return f32(strconv.atof64(s) or { 0 })
}

// f64 returns the value of the string as f64 `'1.0'.f64() == f64(1)`.
@[inline]
pub fn (s string) f64() f64 {
	return strconv.atof64(s) or { 0 }
}

// u8 returns the value of the string as u8 `'1'.u8() == u8(1)`.
@[inline]
pub fn (s string) u8() u8 {
	return u8(strconv.common_parse_uint(s, 0, 8, false, false) or { 0 })
}

// u16 returns the value of the string as u16 `'1'.u16() == u16(1)`.
@[inline]
pub fn (s string) u16() u16 {
	return u16(strconv.common_parse_uint(s, 0, 16, false, false) or { 0 })
}

// u32 returns the value of the string as u32 `'1'.u32() == u32(1)`.
@[inline]
pub fn (s string) u32() u32 {
	return u32(strconv.common_parse_uint(s, 0, 32, false, false) or { 0 })
}

// u64 returns the value of the string as u64 `'1'.u64() == u64(1)`.
@[inline]
pub fn (s string) u64() u64 {
	return strconv.common_parse_uint(s, 0, 64, false, false) or { 0 }
}

// parse_uint is like `parse_int` but for unsigned numbers
//
// This method directly exposes the `parse_uint` function from `strconv`
// as a method on `string`. For more advanced features,
// consider calling `strconv.common_parse_uint` directly.
@[inline]
pub fn (s string) parse_uint(_base int, _bit_size int) !u64 {
	return strconv.parse_uint(s, _base, _bit_size)
}

// parse_int interprets a string s in the given base (0, 2 to 36) and
// bit size (0 to 64) and returns the corresponding value i.
//
// If the base argument is 0, the true base is implied by the string's
// prefix: 2 for "0b", 8 for "0" or "0o", 16 for "0x", and 10 otherwise.
// Also, for argument base 0 only, underscore characters are permitted
// as defined by the Go syntax for integer literals.
//
// The bitSize argument specifies the integer type
// that the result must fit into. Bit sizes 0, 8, 16, 32, and 64
// correspond to int, int8, int16, int32, and int64.
// If bitSize is below 0 or above 64, an error is returned.
//
// This method directly exposes the `parse_int` function from `strconv`
// as a method on `string`. For more advanced features,
// consider calling `strconv.common_parse_int` directly.
@[inline]
pub fn (s string) parse_int(_base int, _bit_size int) !i64 {
	return strconv.parse_int(s, _base, _bit_size)
}

@[direct_array_access]
fn (s string) == (a string) bool {
	if s.str == 0 {
		// should never happen
		panic('string.eq(): nil string')
	}
	if s.len != a.len {
		return false
	}
	if s.len > 0 {
		last_idx := s.len - 1
		if s[last_idx] != a[last_idx] {
			return false
		}
	}
	unsafe {
		return vmemcmp(s.str, a.str, a.len) == 0
	}
}

// compare returns -1 if `s` < `a`, 0 if `s` == `a`, and 1 if `s` > `a`
@[direct_array_access]
pub fn (s string) compare(a string) int {
	min_len := if s.len < a.len { s.len } else { a.len }
	for i in 0 .. min_len {
		if s[i] < a[i] {
			return -1
		}
		if s[i] > a[i] {
			return 1
		}
	}
	if s.len < a.len {
		return -1
	}
	if s.len > a.len {
		return 1
	}
	return 0
}

@[direct_array_access]
fn (s string) < (a string) bool {
	for i in 0 .. s.len {
		if i >= a.len || s[i] > a[i] {
			return false
		} else if s[i] < a[i] {
			return true
		}
	}
	if s.len < a.len {
		return true
	}
	return false
}

@[direct_array_access]
fn (s string) + (a string) string {
	new_len := a.len + s.len
	mut res := string{
		str: unsafe { malloc_noscan(new_len + 1) }
		len: new_len
	}
	unsafe {
		vmemcpy(res.str, s.str, s.len)
		vmemcpy(res.str + s.len, a.str, a.len)
	}
	unsafe {
		res.str[new_len] = 0 // V strings are not null terminated, but just in case
	}
	return res
}

// split_any splits the string to an array by any of the `delim` chars.
// Example: "first row\nsecond row".split_any(" \n") == ['first', 'row', 'second', 'row']
// Split a string using the chars in the delimiter string as delimiters chars.
// If the delimiter string is empty then `.split()` is used.
@[direct_array_access]
pub fn (s string) split_any(delim string) []string {
	mut res := []string{}
	mut i := 0
	// check empty source string
	if s.len > 0 {
		// if empty delimiter string using default split
		if delim.len <= 0 {
			return s.split('')
		}
		for index, ch in s {
			for delim_ch in delim {
				if ch == delim_ch {
					res << s[i..index]
					i = index + 1
					break
				}
			}
		}
		if i < s.len {
			res << s[i..]
		}
	}
	return res
}

// rsplit_any splits the string to an array by any of the `delim` chars in reverse order.
// Example: "first row\nsecond row".rsplit_any(" \n") == ['row', 'second', 'row', 'first']
// Split a string using the chars in the delimiter string as delimiters chars.
// If the delimiter string is empty then `.rsplit()` is used.
@[direct_array_access]
pub fn (s string) rsplit_any(delim string) []string {
	mut res := []string{}
	mut i := s.len - 1
	if s.len > 0 {
		if delim.len <= 0 {
			return s.rsplit('')
		}
		mut rbound := s.len
		for i >= 0 {
			for delim_ch in delim {
				if s[i] == delim_ch {
					res << s[i + 1..rbound]
					rbound = i
					break
				}
			}
			i--
		}
		if rbound > 0 {
			res << s[..rbound]
		}
	}
	return res
}

// split splits the string into an array of strings at the given delimiter.
// Example: assert 'A B C'.split(' ') == ['A','B','C']
// If `delim` is empty the string is split by it's characters.
// Example: assert 'DEF'.split('') == ['D','E','F']
@[inline]
pub fn (s string) split(delim string) []string {
	return s.split_nth(delim, 0)
}

// rsplit splits the string into an array of strings at the given delimiter, starting from the right.
// Example: assert 'A B C'.rsplit(' ') == ['C','B','A']
// If `delim` is empty the string is split by it's characters.
// Example: assert 'DEF'.rsplit('') == ['F','E','D']
@[inline]
pub fn (s string) rsplit(delim string) []string {
	return s.rsplit_nth(delim, 0)
}

// split_once splits the string into a pair of strings at the given delimiter.
// Example:
// ```v
// path, ext := 'file.ts.dts'.split_once('.')?
// assert path == 'file'
// assert ext == 'ts.dts'
pub fn (s string) split_once(delim string) ?(string, string) {
	result := s.split_nth(delim, 2)

	if result.len != 2 {
		return none
	}

	return result[0], result[1]
}

// rsplit_once splits the string into a pair of strings at the given delimiter, starting from the right.
// Example:
// ```v
// path, ext := 'file.ts.dts'.rsplit_once('.')?
// assert path == 'file.ts'
// assert ext == 'dts'
// ```
// NOTE: rsplit_once returns the string at the left side of the delimiter as first part of the pair.
pub fn (s string) rsplit_once(delim string) ?(string, string) {
	result := s.rsplit_nth(delim, 2)

	if result.len != 2 {
		return none
	}

	return result[1], result[0]
}

// split_nth splits the string based on the passed `delim` substring.
// It returns the first Nth parts. When N=0, return all the splits.
// The last returned element has the remainder of the string, even if
// the remainder contains more `delim` substrings.
@[direct_array_access]
pub fn (s string) split_nth(delim string, nth int) []string {
	mut res := []string{}
	mut i := 0

	match delim.len {
		0 {
			i = 1
			for ch in s {
				if nth > 0 && i >= nth {
					res << s[i - 1..]
					break
				}
				res << ch.ascii_str()
				i++
			}
			return res
		}
		1 {
			mut start := 0
			delim_byte := delim[0]

			for i < s.len {
				if s[i] == delim_byte {
					was_last := nth > 0 && res.len == nth - 1
					if was_last {
						break
					}
					val := s.substr(start, i)
					res << val
					start = i + delim.len
					i = start
				} else {
					i++
				}
			}

			// Then the remaining right part of the string
			if nth < 1 || res.len < nth {
				res << s[start..]
			}
			return res
		}
		else {
			mut start := 0
			// Take the left part for each delimiter occurrence
			for i <= s.len {
				is_delim := i + delim.len <= s.len && s.substr(i, i + delim.len) == delim
				if is_delim {
					was_last := nth > 0 && res.len == nth - 1
					if was_last {
						break
					}
					val := s.substr(start, i)
					res << val
					start = i + delim.len
					i = start
				} else {
					i++
				}
			}
			// Then the remaining right part of the string
			if nth < 1 || res.len < nth {
				res << s[start..]
			}
			return res
		}
	}
}

// rsplit_nth splits the string based on the passed `delim` substring in revese order.
// It returns the first Nth parts. When N=0, return all the splits.
// The last returned element has the remainder of the string, even if
// the remainder contains more `delim` substrings.
@[direct_array_access]
pub fn (s string) rsplit_nth(delim string, nth int) []string {
	mut res := []string{}
	mut i := s.len - 1

	match delim.len {
		0 {
			for i >= 0 {
				if nth > 0 && res.len == nth - 1 {
					res << s[..i + 1]
					break
				}
				res << s[i].ascii_str()
				i--
			}
			return res
		}
		1 {
			mut rbound := s.len
			delim_byte := delim[0]

			for i >= 0 {
				if s[i] == delim_byte {
					if nth > 0 && res.len == nth - 1 {
						break
					}
					res << s[i + 1..rbound]
					rbound = i
					i--
				} else {
					i--
				}
			}

			if nth < 1 || res.len < nth {
				res << s[..rbound]
			}
			return res
		}
		else {
			mut rbound := s.len

			for i >= 0 {
				is_delim := i - delim.len >= 0 && s[i - delim.len..i] == delim
				if is_delim {
					if nth > 0 && res.len == nth - 1 {
						break
					}
					res << s[i..rbound]
					rbound = i - delim.len
					i -= delim.len
				} else {
					i--
				}
			}

			if nth < 1 || res.len < nth {
				res << s[..rbound]
			}
			return res
		}
	}
}

// split_into_lines splits the string by newline characters.
// newlines are stripped.
// `\r` (MacOS), `\n` (POSIX), and `\r\n` (WinOS) line endings are all supported (including mixed line endings).
// NOTE: algorithm is "greedy", consuming '\r\n' as a single line ending with higher priority than '\r' and '\n' as multiple endings
@[direct_array_access]
pub fn (s string) split_into_lines() []string {
	mut res := []string{}
	if s.len == 0 {
		return res
	}
	cr := `\r`
	lf := `\n`
	mut line_start := 0
	for i := 0; i < s.len; i++ {
		if line_start <= i {
			if s[i] == lf {
				res << if line_start == i { '' } else { s[line_start..i] }
				line_start = i + 1
			} else if s[i] == cr {
				res << if line_start == i { '' } else { s[line_start..i] }
				if (i + 1) < s.len && s[i + 1] == lf {
					line_start = i + 2
				} else {
					line_start = i + 1
				}
			}
		}
	}
	if line_start < s.len {
		res << s[line_start..]
	}
	return res
}

// substr returns the string between index positions `start` and `end`.
// Example: assert 'ABCD'.substr(1,3) == 'BC'
@[direct_array_access]
pub fn (s string) substr(start int, _end int) string {
	end := if _end == max_int { s.len } else { _end } // max_int
	$if !no_bounds_checking {
		if start > end || start > s.len || end > s.len || start < 0 || end < 0 {
			panic('substr(${start}, ${end}) out of bounds (len=${s.len})')
		}
	}
	len := end - start
	if len == s.len {
		return s.clone()
	}
	mut res := string{
		str: unsafe { malloc_noscan(len + 1) }
		len: len
	}
	unsafe {
		vmemcpy(res.str, s.str + start, len)
		res.str[len] = 0
	}
	return res
}

// substr_unsafe works like substr(), but doesn't copy (allocate) the substring
@[direct_array_access]
pub fn (s string) substr_unsafe(start int, _end int) string {
	end := if _end == 2147483647 { s.len } else { _end } // max_int
	len := end - start
	if len == s.len {
		return s
	}
	return string{
		str: unsafe { s.str + start }
		len: len
	}
}

// version of `substr()` that is used in `a[start..end] or {`
// return an error when the index is out of range
@[direct_array_access]
pub fn (s string) substr_with_check(start int, _end int) !string {
	end := if _end == max_int { s.len } else { _end } // max_int
	if start > end || start > s.len || end > s.len || start < 0 || end < 0 {
		return error('substr(${start}, ${end}) out of bounds (len=${s.len})')
	}
	len := end - start
	if len == s.len {
		return s.clone()
	}
	mut res := string{
		str: unsafe { malloc_noscan(len + 1) }
		len: len
	}
	unsafe {
		vmemcpy(res.str, s.str + start, len)
		res.str[len] = 0
	}
	return res
}

// substr_ni returns the string between index positions `start` and `end` allowing negative indexes
// This function always return a valid string.
@[direct_array_access]
pub fn (s string) substr_ni(_start int, _end int) string {
	mut start := _start
	mut end := if _end == max_int { s.len } else { _end } // max_int

	// borders math
	if start < 0 {
		start = s.len + start
		if start < 0 {
			start = 0
		}
	}

	if end < 0 {
		end = s.len + end
		if end < 0 {
			end = 0
		}
	}
	if end >= s.len {
		end = s.len
	}

	if start > s.len || end < start {
		return ''
	}

	len := end - start

	// string copy
	mut res := string{
		str: unsafe { malloc_noscan(len + 1) }
		len: len
	}
	unsafe {
		vmemcpy(res.str, s.str + start, len)
		res.str[len] = 0
	}
	return res
}

// index returns the position of the first character of the input string.
// It will return `-1` if the input string can't be found.
@[direct_array_access]
fn (s string) index_(p string) int {
	if p.len > s.len || p.len == 0 {
		return -1
	}
	if p.len > 2 {
		return s.index_kmp(p)
	}
	mut i := 0
	for i < s.len {
		mut j := 0
		for j < p.len && unsafe { s.str[i + j] == p.str[j] } {
			j++
		}
		if j == p.len {
			return i
		}
		i++
	}
	return -1
}

// index returns the position of the first character of the first occurance of the `needle` string in `s`.
// It will return `none` if the `needle` string can't be found in `s`.
pub fn (s string) index(p string) ?int {
	idx := s.index_(p)
	if idx == -1 {
		return none
	}
	return idx
}

// index_last returns the position of the first character of the *last* occurance of the `needle` string in `s`.
pub fn (s string) index_last(needle string) ?int {
	idx := s.index_last_(needle)
	if idx == -1 {
		return none
	}
	return idx
}

// last_index returns the position of the first character of the *last* occurance of the `needle` string in `s`.
@[deprecated: 'use `.index_last(needle string)` instead']
@[deprecated_after: '2023-12-18']
@[inline]
pub fn (s string) last_index(needle string) ?int {
	return s.index_last(needle)
}

// index_kmp does KMP search.
@[direct_array_access; manualfree]
fn (s string) index_kmp(p string) int {
	if p.len > s.len {
		return -1
	}
	mut prefix := []int{len: p.len}
	defer {
		unsafe { prefix.free() }
	}
	mut j := 0
	for i := 1; i < p.len; i++ {
		for unsafe { p.str[j] != p.str[i] } && j > 0 {
			j = prefix[j - 1]
		}
		if unsafe { p.str[j] == p.str[i] } {
			j++
		}
		prefix[i] = j
	}
	j = 0
	for i in 0 .. s.len {
		for unsafe { p.str[j] != s.str[i] } && j > 0 {
			j = prefix[j - 1]
		}
		if unsafe { p.str[j] == s.str[i] } {
			j++
		}
		if j == p.len {
			return i - p.len + 1
		}
	}
	return -1
}

// index_any returns the position of any of the characters in the input string - if found.
pub fn (s string) index_any(chars string) int {
	for i, ss in s {
		for c in chars {
			if c == ss {
				return i
			}
		}
	}
	return -1
}

// index_last_ returns the position of the last occurrence of the given string `p` in `s`.
@[direct_array_access]
fn (s string) index_last_(p string) int {
	if p.len > s.len || p.len == 0 {
		return -1
	}
	mut i := s.len - p.len
	for i >= 0 {
		mut j := 0
		for j < p.len && unsafe { s.str[i + j] == p.str[j] } {
			j++
		}
		if j == p.len {
			return i
		}
		i--
	}
	return -1
}

// index_after returns the position of the input string, starting search from `start` position.
@[direct_array_access]
pub fn (s string) index_after(p string, start int) int {
	if p.len > s.len {
		return -1
	}
	mut strt := start
	if start < 0 {
		strt = 0
	}
	if start >= s.len {
		return -1
	}
	mut i := strt
	for i < s.len {
		mut j := 0
		mut ii := i
		for j < p.len && unsafe { s.str[ii] == p.str[j] } {
			j++
			ii++
		}
		if j == p.len {
			return i
		}
		i++
	}
	return -1
}

// index_u8 returns the index of byte `c` if found in the string.
// index_u8 returns -1 if the byte can not be found.
@[direct_array_access]
pub fn (s string) index_u8(c u8) int {
	for i, b in s {
		if b == c {
			return i
		}
	}
	return -1
}

// index_u8_last returns the index of the *last* occurrence of the byte `c` (if found) in the string.
// It returns -1, if `c` is not found.
@[direct_array_access]
pub fn (s string) index_u8_last(c u8) int {
	for i := s.len - 1; i >= 0; i-- {
		if unsafe { s.str[i] == c } {
			return i
		}
	}
	return -1
}

// last_index_u8 returns the index of the last occurrence of byte `c` if found in the string.
// It returns -1, if the byte `c` is not found.
@[deprecated: 'use `.index_u8_last(c u8)` instead']
@[deprecated_after: '2023-12-18']
@[inline]
pub fn (s string) last_index_u8(c u8) int {
	return s.index_u8_last(c)
}

// count returns the number of occurrences of `substr` in the string.
// count returns -1 if no `substr` could be found.
@[direct_array_access]
pub fn (s string) count(substr string) int {
	if s.len == 0 || substr.len == 0 {
		return 0
	}
	if substr.len > s.len {
		return 0
	}

	mut n := 0

	if substr.len == 1 {
		target := substr[0]

		for letter in s {
			if letter == target {
				n++
			}
		}

		return n
	}

	mut i := 0
	for {
		i = s.index_after(substr, i)
		if i == -1 {
			return n
		}
		i += substr.len
		n++
	}
	return 0 // TODO can never get here - v doesn't know that
}

// contains_u8 returns `true` if the string contains the byte value `x`.
// See also: [`string.index_u8`](#string.index_u8) , to get the index of the byte as well.
pub fn (s string) contains_u8(x u8) bool {
	for c in s {
		if x == c {
			return true
		}
	}
	return false
}

// contains returns `true` if the string contains `substr`.
// See also: [`string.index`](#string.index)
pub fn (s string) contains(substr string) bool {
	if substr.len == 0 {
		return true
	}
	if substr.len == 1 {
		return s.contains_u8(unsafe { substr.str[0] })
	}
	return s.index_(substr) != -1
}

// contains_any returns `true` if the string contains any chars in `chars`.
pub fn (s string) contains_any(chars string) bool {
	for c in chars {
		if s.contains_u8(c) {
			return true
		}
	}
	return false
}

// contains_only returns `true`, if the string contains only the characters in `chars`.
pub fn (s string) contains_only(chars string) bool {
	if chars.len == 0 {
		return false
	}
	for ch in s {
		mut res := 0
		for i := 0; i < chars.len && res == 0; i++ {
			res += int(ch == unsafe { chars.str[i] })
		}
		if res == 0 {
			return false
		}
	}
	return true
}

// contains_any_substr returns `true` if the string contains any of the strings in `substrs`.
pub fn (s string) contains_any_substr(substrs []string) bool {
	if substrs.len == 0 {
		return true
	}
	for sub in substrs {
		if s.contains(sub) {
			return true
		}
	}
	return false
}

// starts_with returns `true` if the string starts with `p`.
@[direct_array_access]
pub fn (s string) starts_with(p string) bool {
	if p.len > s.len {
		return false
	}
	for i in 0 .. p.len {
		if unsafe { s.str[i] != p.str[i] } {
			return false
		}
	}
	return true
}

// ends_with returns `true` if the string ends with `p`.
@[direct_array_access]
pub fn (s string) ends_with(p string) bool {
	if p.len > s.len {
		return false
	}
	for i in 0 .. p.len {
		if unsafe { p.str[i] != s.str[s.len - p.len + i] } {
			return false
		}
	}
	return true
}

// to_lower returns the string in all lowercase characters.
// TODO only works with ASCII
@[direct_array_access]
pub fn (s string) to_lower() string {
	unsafe {
		mut b := malloc_noscan(s.len + 1)
		for i in 0 .. s.len {
			if s.str[i] >= `A` && s.str[i] <= `Z` {
				b[i] = s.str[i] + 32
			} else {
				b[i] = s.str[i]
			}
		}
		b[s.len] = 0
		return tos(b, s.len)
	}
}

// is_lower returns `true` if all characters in the string is lowercase.
// Example: assert 'hello developer'.is_lower() == true
@[direct_array_access]
pub fn (s string) is_lower() bool {
	for i in 0 .. s.len {
		if s[i] >= `A` && s[i] <= `Z` {
			return false
		}
	}
	return true
}

// to_upper returns the string in all uppercase characters.
// Example: assert 'Hello V'.to_upper() == 'HELLO V'
@[direct_array_access]
pub fn (s string) to_upper() string {
	unsafe {
		mut b := malloc_noscan(s.len + 1)
		for i in 0 .. s.len {
			if s.str[i] >= `a` && s.str[i] <= `z` {
				b[i] = s.str[i] - 32
			} else {
				b[i] = s.str[i]
			}
		}
		b[s.len] = 0
		return tos(b, s.len)
	}
}

// is_upper returns `true` if all characters in the string is uppercase.
// See also: [`byte.is_capital`](#byte.is_capital)
// Example: assert 'HELLO V'.is_upper() == true
@[direct_array_access]
pub fn (s string) is_upper() bool {
	for i in 0 .. s.len {
		if s[i] >= `a` && s[i] <= `z` {
			return false
		}
	}
	return true
}

// capitalize returns the string with the first character capitalized.
// Example: assert 'hello'.capitalize() == 'Hello'
@[direct_array_access]
pub fn (s string) capitalize() string {
	if s.len == 0 {
		return ''
	}
	s0 := s[0]
	letter := s0.ascii_str()
	uletter := letter.to_upper()
	if s.len == 1 {
		return uletter
	}
	srest := s[1..]
	res := uletter + srest
	return res
}

// uncapitalize returns the string with the first character uncapitalized.
// Example: assert 'Hello, Bob!'.uncapitalize() == 'hello, Bob!'
@[direct_array_access]
pub fn (s string) uncapitalize() string {
	if s.len == 0 {
		return ''
	}
	s0 := s[0]
	letter := s0.ascii_str()
	uletter := letter.to_lower()
	if s.len == 1 {
		return uletter
	}
	srest := s[1..]
	res := uletter + srest
	return res
}

// is_capital returns `true`, if the first character in the string `s`,
// is a capital letter, and the rest are NOT.
// Example: assert 'Hello'.is_capital() == true
// Example: assert 'HelloWorld'.is_capital() == false
@[direct_array_access]
pub fn (s string) is_capital() bool {
	if s.len == 0 || !(s[0] >= `A` && s[0] <= `Z`) {
		return false
	}
	for i in 1 .. s.len {
		if s[i] >= `A` && s[i] <= `Z` {
			return false
		}
	}
	return true
}

// starts_with_capital returns `true`, if the first character in the string `s`,
// is a capital letter, even if the rest are not.
// Example: assert 'Hello'.starts_with_capital() == true
// Example: assert 'Hello. World.'.starts_with_capital() == true
@[direct_array_access]
pub fn (s string) starts_with_capital() bool {
	if s.len == 0 || !(s[0] >= `A` && s[0] <= `Z`) {
		return false
	}
	return true
}

// title returns the string with each word capitalized.
// Example: assert 'hello v developer'.title() == 'Hello V Developer'
pub fn (s string) title() string {
	words := s.split(' ')
	mut tit := []string{}
	for word in words {
		tit << word.capitalize()
	}
	title := tit.join(' ')
	return title
}

// is_title returns true if all words of the string are capitalized.
// Example: assert 'Hello V Developer'.is_title() == true
pub fn (s string) is_title() bool {
	words := s.split(' ')
	for word in words {
		if !word.is_capital() {
			return false
		}
	}
	return true
}

// find_between returns the string found between `start` string and `end` string.
// Example: assert 'hey [man] how you doin'.find_between('[', ']') == 'man'
pub fn (s string) find_between(start string, end string) string {
	start_pos := s.index_(start)
	if start_pos == -1 {
		return ''
	}
	// First get everything to the right of 'start'
	val := s[start_pos + start.len..]
	end_pos := val.index_(end)
	if end_pos == -1 {
		return val
	}
	return val[..end_pos]
}

// trim_space strips any of ` `, `\n`, `\t`, `\v`, `\f`, `\r` from the start and end of the string.
// Example: assert ' Hello V '.trim_space() == 'Hello V'
@[inline]
pub fn (s string) trim_space() string {
	return s.trim(' \n\t\v\f\r')
}

// trim strips any of the characters given in `cutset` from the start and end of the string.
// Example: assert ' ffHello V ffff'.trim(' f') == 'Hello V'
pub fn (s string) trim(cutset string) string {
	if s.len < 1 || cutset.len < 1 {
		return s.clone()
	}
	left, right := s.trim_indexes(cutset)
	return s.substr(left, right)
}

// trim_indexes gets the new start and end indices of a string when any of the characters given in `cutset` were stripped from the start and end of the string. Should be used as an input to `substr()`. If the string contains only the characters in `cutset`, both values returned are zero.
// Example: left, right := '-hi-'.trim_indexes('-')
@[direct_array_access]
pub fn (s string) trim_indexes(cutset string) (int, int) {
	mut pos_left := 0
	mut pos_right := s.len - 1
	mut cs_match := true
	for pos_left <= s.len && pos_right >= -1 && cs_match {
		cs_match = false
		for cs in cutset {
			if s[pos_left] == cs {
				pos_left++
				cs_match = true
				break
			}
		}
		for cs in cutset {
			if s[pos_right] == cs {
				pos_right--
				cs_match = true
				break
			}
		}
		if pos_left > pos_right {
			return 0, 0
		}
	}
	return pos_left, pos_right + 1
}

// trim_left strips any of the characters given in `cutset` from the left of the string.
// Example: assert 'd Hello V developer'.trim_left(' d') == 'Hello V developer'
@[direct_array_access]
pub fn (s string) trim_left(cutset string) string {
	if s.len < 1 || cutset.len < 1 {
		return s.clone()
	}
	mut pos := 0
	for pos < s.len {
		mut found := false
		for cs in cutset {
			if s[pos] == cs {
				found = true
				break
			}
		}
		if !found {
			break
		}
		pos++
	}
	return s[pos..]
}

// trim_right strips any of the characters given in `cutset` from the right of the string.
// Example: assert ' Hello V d'.trim_right(' d') == ' Hello V'
@[direct_array_access]
pub fn (s string) trim_right(cutset string) string {
	if s.len < 1 || cutset.len < 1 {
		return s.clone()
	}
	mut pos := s.len - 1
	for pos >= 0 {
		mut found := false
		for cs in cutset {
			if s[pos] == cs {
				found = true
			}
		}
		if !found {
			break
		}
		pos--
	}
	if pos < 0 {
		return ''
	}
	return s[..pos + 1]
}

// trim_string_left strips `str` from the start of the string.
// Example: assert 'WorldHello V'.trim_string_left('World') == 'Hello V'
pub fn (s string) trim_string_left(str string) string {
	if s.starts_with(str) {
		return s[str.len..]
	}
	return s.clone()
}

// trim_string_right strips `str` from the end of the string.
// Example: assert 'Hello VWorld'.trim_string_right('World') == 'Hello V'
pub fn (s string) trim_string_right(str string) string {
	if s.ends_with(str) {
		return s[..s.len - str.len]
	}
	return s.clone()
}

// compare_strings returns `-1` if `a < b`, `1` if `a > b` else `0`.
pub fn compare_strings(a &string, b &string) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// compare_strings_by_len returns `-1` if `a.len < b.len`, `1` if `a.len > b.len` else `0`.
fn compare_strings_by_len(a &string, b &string) int {
	if a.len < b.len {
		return -1
	}
	if a.len > b.len {
		return 1
	}
	return 0
}

// compare_lower_strings returns the same as compare_strings but converts `a` and `b` to lower case before comparing.
fn compare_lower_strings(a &string, b &string) int {
	aa := a.to_lower()
	bb := b.to_lower()
	return compare_strings(&aa, &bb)
}

// sort_ignore_case sorts the string array using case insensitive comparing.
@[inline]
pub fn (mut s []string) sort_ignore_case() {
	s.sort_with_compare(compare_lower_strings)
}

// sort_by_len sorts the the string array by each string's `.len` length.
@[inline]
pub fn (mut s []string) sort_by_len() {
	s.sort_with_compare(compare_strings_by_len)
}

// str returns a copy of the string
@[inline]
pub fn (s string) str() string {
	return s.clone()
}

// at returns the byte at index `idx`.
// Example: assert 'ABC'.at(1) == u8(`B`)
fn (s string) at(idx int) u8 {
	$if !no_bounds_checking {
		if idx < 0 || idx >= s.len {
			panic('string index out of range: ${idx} / ${s.len}')
		}
	}
	return unsafe { s.str[idx] }
}

// version of `at()` that is used in `a[i] or {`
// return an error when the index is out of range
fn (s string) at_with_check(idx int) ?u8 {
	if idx < 0 || idx >= s.len {
		return none
	}
	unsafe {
		return s.str[idx]
	}
}

// is_space returns `true` if the byte is a white space character.
// The following list is considered white space characters: ` `, `\t`, `\n`, `\v`, `\f`, `\r`, 0x85, 0xa0
// Example: assert u8(` `).is_space() == true
@[inline]
pub fn (c u8) is_space() bool {
	// 0x85 is NEXT LINE (NEL)
	// 0xa0 is NO-BREAK SPACE
	return c == 32 || (c > 8 && c < 14) || c == 0x85 || c == 0xa0
}

// is_digit returns `true` if the byte is in range 0-9 and `false` otherwise.
// Example: assert u8(`9`).is_digit() == true
@[inline]
pub fn (c u8) is_digit() bool {
	return c >= `0` && c <= `9`
}

// is_hex_digit returns `true` if the byte is either in range 0-9, a-f or A-F and `false` otherwise.
// Example: assert u8(`F`).is_hex_digit() == true
@[inline]
pub fn (c u8) is_hex_digit() bool {
	return (c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

// is_oct_digit returns `true` if the byte is in range 0-7 and `false` otherwise.
// Example: assert u8(`7`).is_oct_digit() == true
@[inline]
pub fn (c u8) is_oct_digit() bool {
	return c >= `0` && c <= `7`
}

// is_bin_digit returns `true` if the byte is a binary digit (0 or 1) and `false` otherwise.
// Example: assert u8(`0`).is_bin_digit() == true
@[inline]
pub fn (c u8) is_bin_digit() bool {
	return c == `0` || c == `1`
}

// is_letter returns `true` if the byte is in range a-z or A-Z and `false` otherwise.
// Example: assert u8(`V`).is_letter() == true
@[inline]
pub fn (c u8) is_letter() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
}

// is_alnum returns `true` if the byte is in range a-z, A-Z, 0-9 and `false` otherwise.
// Example: assert u8(`V`).is_alnum() == true
@[inline]
pub fn (c u8) is_alnum() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)
}

// free allows for manually freeing the memory occupied by the string
@[manualfree; unsafe]
pub fn (s &string) free() {
	$if prealloc {
		return
	}
	if s.is_lit == -98761234 {
		double_free_msg := unsafe { &u8(c'double string.free() detected\n') }
		double_free_msg_len := unsafe { vstrlen(double_free_msg) }
		$if freestanding {
			bare_eprint(double_free_msg, u64(double_free_msg_len))
		} $else {
			_write_buf_to_fd(1, double_free_msg, double_free_msg_len)
		}
		return
	}
	if s.is_lit == 1 || s.str == 0 {
		return
	}
	unsafe {
		// C.printf(c's: %x %s\n', s.str, s.str)
		free(s.str)
		s.str = nil
	}
	s.is_lit = -98761234
}

// before returns the contents before `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.before('.') == '23:34:45'
// Example: assert 'abcd'.before('.') == 'abcd'
// TODO: deprecate and remove either .before or .all_before
pub fn (s string) before(sub string) string {
	pos := s.index_(sub)
	if pos == -1 {
		return s.clone()
	}
	return s[..pos]
}

// all_before returns the contents before `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.all_before('.') == '23:34:45'
// Example: assert 'abcd'.all_before('.') == 'abcd'
pub fn (s string) all_before(sub string) string {
	// TODO remove dup method
	pos := s.index_(sub)
	if pos == -1 {
		return s.clone()
	}
	return s[..pos]
}

// all_before_last returns the contents before the last occurrence of `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.all_before_last(':') == '23:34'
// Example: assert 'abcd'.all_before_last('.') == 'abcd'
pub fn (s string) all_before_last(sub string) string {
	pos := s.index_last_(sub)
	if pos == -1 {
		return s.clone()
	}
	return s[..pos]
}

// all_after returns the contents after `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.all_after('.') == '234'
// Example: assert 'abcd'.all_after('z') == 'abcd'
pub fn (s string) all_after(sub string) string {
	pos := s.index_(sub)
	if pos == -1 {
		return s.clone()
	}
	return s[pos + sub.len..]
}

// all_after_last returns the contents after the last occurrence of `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.all_after_last(':') == '45.234'
// Example: assert 'abcd'.all_after_last('z') == 'abcd'
pub fn (s string) all_after_last(sub string) string {
	pos := s.index_last_(sub)
	if pos == -1 {
		return s.clone()
	}
	return s[pos + sub.len..]
}

// all_after_first returns the contents after the first occurrence of `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.all_after_first(':') == '34:45.234'
// Example: assert 'abcd'.all_after_first('z') == 'abcd'
pub fn (s string) all_after_first(sub string) string {
	pos := s.index_(sub)
	if pos == -1 {
		return s.clone()
	}
	return s[pos + sub.len..]
}

// after returns the contents after the last occurrence of `sub` in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.after(':') == '45.234'
// Example: assert 'abcd'.after('z') == 'abcd'
// TODO: deprecate either .all_after_last or .after
@[inline]
pub fn (s string) after(sub string) string {
	return s.all_after_last(sub)
}

// after_char returns the contents after the first occurrence of `sub` character in the string.
// If the substring is not found, it returns the full input string.
// Example: assert '23:34:45.234'.after_char(`:`) == '34:45.234'
// Example: assert 'abcd'.after_char(`:`) == 'abcd'
pub fn (s string) after_char(sub u8) string {
	mut pos := -1
	for i, c in s {
		if c == sub {
			pos = i
			break
		}
	}
	if pos == -1 {
		return s.clone()
	}
	return s[pos + 1..]
}

// join joins a string array into a string using `sep` separator.
// Example: assert ['Hello','V'].join(' ') == 'Hello V'
pub fn (a []string) join(sep string) string {
	if a.len == 0 {
		return ''
	}
	mut len := 0
	for val in a {
		len += val.len + sep.len
	}
	len -= sep.len
	// Allocate enough memory
	mut res := string{
		str: unsafe { malloc_noscan(len + 1) }
		len: len
	}
	mut idx := 0
	for i, val in a {
		unsafe {
			vmemcpy(voidptr(res.str + idx), val.str, val.len)
			idx += val.len
		}
		// Add sep if it's not last
		if i != a.len - 1 {
			unsafe {
				vmemcpy(voidptr(res.str + idx), sep.str, sep.len)
				idx += sep.len
			}
		}
	}
	unsafe {
		res.str[res.len] = 0
	}
	return res
}

// join joins a string array into a string using a `\n` newline delimiter.
@[inline]
pub fn (s []string) join_lines() string {
	return s.join('\n')
}

// reverse returns a reversed string.
// Example: assert 'Hello V'.reverse() == 'V olleH'
@[direct_array_access]
pub fn (s string) reverse() string {
	if s.len == 0 || s.len == 1 {
		return s.clone()
	}
	mut res := string{
		str: unsafe { malloc_noscan(s.len + 1) }
		len: s.len
	}
	for i := s.len - 1; i >= 0; i-- {
		unsafe {
			res.str[s.len - i - 1] = s[i]
		}
	}
	unsafe {
		res.str[res.len] = 0
	}
	return res
}

// limit returns a portion of the string, starting at `0` and extending for a given number of characters afterward.
// 'hello'.limit(2) => 'he'
// 'hi'.limit(10) => 'hi'
pub fn (s string) limit(max int) string {
	u := s.runes()
	if u.len <= max {
		return s.clone()
	}
	return u[0..max].string()
}

// hash returns an integer hash of the string.
pub fn (s string) hash() int {
	mut h := u32(0)
	if h == 0 && s.len > 0 {
		for c in s {
			h = h * 31 + u32(c)
		}
	}
	return int(h)
}

// bytes returns the string converted to a byte array.
pub fn (s string) bytes() []u8 {
	if s.len == 0 {
		return []
	}
	mut buf := []u8{len: s.len}
	unsafe { vmemcpy(buf.data, s.str, s.len) }
	return buf
}

// repeat returns a new string with `count` number of copies of the string it was called on.
@[direct_array_access]
pub fn (s string) repeat(count int) string {
	if count < 0 {
		panic('string.repeat: count is negative: ${count}')
	} else if count == 0 {
		return ''
	} else if count == 1 {
		return s.clone()
	}
	mut ret := unsafe { malloc_noscan(s.len * count + 1) }
	for i in 0 .. count {
		unsafe {
			vmemcpy(ret + i * s.len, s.str, s.len)
		}
	}
	new_len := s.len * count
	unsafe {
		ret[new_len] = 0
	}
	return unsafe { ret.vstring_with_len(new_len) }
}

// fields returns a string array of the string split by `\t` and ` `
// Example: assert '\t\tv = v'.fields() == ['v', '=', 'v']
// Example: assert '  sss   ssss'.fields() == ['sss', 'ssss']
pub fn (s string) fields() []string {
	mut res := []string{}
	mut word_start := 0
	mut word_len := 0
	mut is_in_word := false
	mut is_space := false
	for i, c in s {
		is_space = c in [32, 9, 10]
		if !is_space {
			word_len++
		}
		if !is_in_word && !is_space {
			word_start = i
			is_in_word = true
			continue
		}
		if is_space && is_in_word {
			res << s[word_start..word_start + word_len]
			is_in_word = false
			word_len = 0
			word_start = 0
			continue
		}
	}
	if is_in_word && word_len > 0 {
		// collect the remainder word at the end
		res << s[word_start..s.len]
	}
	return res
}

// strip_margin allows multi-line strings to be formatted in a way that removes white-space
// before a delimiter. By default `|` is used.
// Note: the delimiter has to be a byte at this time. That means surrounding
// the value in ``.
//
// See also: string.trim_indent()
//
// Example:
// ```v
// st := 'Hello there,
//        |  this is a string,
//        |  Everything before the first | is removed'.strip_margin()
//
// assert st == 'Hello there,
//   this is a string,
//   Everything before the first | is removed'
// ```
@[inline]
pub fn (s string) strip_margin() string {
	return s.strip_margin_custom(`|`)
}

// strip_margin_custom does the same as `strip_margin` but will use `del` as delimiter instead of `|`
@[direct_array_access]
pub fn (s string) strip_margin_custom(del u8) string {
	mut sep := del
	if sep.is_space() {
		println('Warning: `strip_margin` cannot use white-space as a delimiter')
		println('    Defaulting to `|`')
		sep = `|`
	}
	// don't know how much space the resulting string will be, but the max it
	// can be is this big
	mut ret := unsafe { malloc_noscan(s.len + 1) }
	mut count := 0
	for i := 0; i < s.len; i++ {
		if s[i] in [10, 13] {
			unsafe {
				ret[count] = s[i]
			}
			count++
			// CRLF
			if s[i] == 13 && i < s.len - 1 && s[i + 1] == 10 {
				unsafe {
					ret[count] = s[i + 1]
				}
				count++
				i++
			}
			for s[i] != sep {
				i++
				if i >= s.len {
					break
				}
			}
		} else {
			unsafe {
				ret[count] = s[i]
			}
			count++
		}
	}
	unsafe {
		ret[count] = 0
		return ret.vstring_with_len(count)
	}
}

// trim_indent detects a common minimal indent of all the input lines,
// removes it from every line and also removes the first and the last
// lines if they are blank (notice difference blank vs empty).
//
// Note that blank lines do not affect the detected indent level.
//
// In case if there are non-blank lines with no leading whitespace characters
// (no indent at all) then the common indent is 0, and therefore this function
// doesn't change the indentation.
//
// Example:
// ```v
// st := '
//      Hello there,
//      this is a string,
//      all the leading indents are removed
//      and also the first and the last lines if they are blank
// '.trim_indent()
//
// assert st == 'Hello there,
// this is a string,
// all the leading indents are removed
// and also the first and the last lines if they are blank'
// ```
pub fn (s string) trim_indent() string {
	mut lines := s.split_into_lines()

	lines_indents := lines
		.filter(!it.is_blank())
		.map(it.indent_width())

	mut min_common_indent := int(max_int) // max int
	for line_indent in lines_indents {
		if line_indent < min_common_indent {
			min_common_indent = line_indent
		}
	}

	// trim first line if it's blank
	if lines.len > 0 && lines.first().is_blank() {
		lines = unsafe { lines[1..] }
	}

	// trim last line if it's blank
	if lines.len > 0 && lines.last().is_blank() {
		lines = unsafe { lines[..lines.len - 1] }
	}

	mut trimmed_lines := []string{cap: lines.len}

	for line in lines {
		if line.is_blank() {
			trimmed_lines << line
			continue
		}

		trimmed_lines << line[min_common_indent..]
	}

	return trimmed_lines.join('\n')
}

// indent_width returns the number of spaces or tabs at the beginning of the string.
// Example: assert '  v'.indent_width() == 2
// Example: assert '\t\tv'.indent_width() == 2
pub fn (s string) indent_width() int {
	for i, c in s {
		if !c.is_space() {
			return i
		}
	}

	return 0
}

// is_blank returns true if the string is empty or contains only white-space.
// Example: assert ' '.is_blank()
// Example: assert '\t'.is_blank()
// Example: assert 'v'.is_blank() == false
pub fn (s string) is_blank() bool {
	if s.len == 0 {
		return true
	}

	for c in s {
		if !c.is_space() {
			return false
		}
	}

	return true
}

// match_glob matches the string, with a Unix shell-style wildcard pattern.
// Note: wildcard patterns are NOT the same as regular expressions.
//   They are much simpler, and do not allow backtracking, captures, etc.
//   The special characters used in shell-style wildcards are:
// `*` - matches everything
// `?` - matches any single character
// `[seq]` - matches any of the characters in the sequence
// `[^seq]` - matches any character that is NOT in the sequence
//   Any other character in `pattern`, is matched 1:1 to the corresponding
// character in `name`, including / and \.
//   You can wrap the meta-characters in brackets too, i.e. `[?]` matches `?`
// in the string, and `[*]` matches `*` in the string.
// Example: assert 'ABCD'.match_glob('AB*')
// Example: assert 'ABCD'.match_glob('*D')
// Example: assert 'ABCD'.match_glob('*B*')
// Example: assert !'ABCD'.match_glob('AB')
@[direct_array_access]
pub fn (name string) match_glob(pattern string) bool {
	// Initial port based on https://research.swtch.com/glob.go
	// See also https://research.swtch.com/glob
	mut px := 0
	mut nx := 0
	mut next_px := 0
	mut next_nx := 0
	plen := pattern.len
	nlen := name.len
	for px < plen || nx < nlen {
		if px < plen {
			c := pattern[px]
			match c {
				`?` {
					// single-character wildcard
					if nx < nlen {
						px++
						nx++
						continue
					}
				}
				`*` {
					// zero-or-more-character wildcard
					// Try to match at nx.
					// If that doesn't work out, restart at nx+1 next.
					next_px = px
					next_nx = nx + 1
					px++
					continue
				}
				`[` {
					if nx < nlen {
						wanted_c := name[nx]
						mut bstart := px
						mut is_inverted := false
						mut inner_match := false
						mut inner_idx := bstart + 1
						mut inner_c := 0
						if inner_idx < plen {
							inner_c = pattern[inner_idx]
							if inner_c == `^` {
								is_inverted = true
								inner_idx++
							}
						}
						for ; inner_idx < plen; inner_idx++ {
							inner_c = pattern[inner_idx]
							if inner_c == `]` {
								break
							}
							if inner_c == wanted_c {
								inner_match = true
								for px < plen && pattern[px] != `]` {
									px++
								}
								break
							}
						}
						if is_inverted {
							if inner_match {
								return false
							} else {
								px = inner_idx
							}
						}
					}
					px++
					nx++
					continue
				}
				else {
					// an ordinary character
					if nx < nlen && name[nx] == c {
						px++
						nx++
						continue
					}
				}
			}
		}
		if 0 < next_nx && next_nx <= nlen {
			// A mismatch, try restarting:
			px = next_px
			nx = next_nx
			continue
		}
		return false
	}
	// Matched all of `pattern` to all of `name`
	return true
}

// is_ascii returns true  if all characters belong to the US-ASCII set ([` `..`~`])
@[inline]
pub fn (s string) is_ascii() bool {
	return !s.bytes().any(it < u8(` `) || it > u8(`~`))
}
