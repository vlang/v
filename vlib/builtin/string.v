// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

// V strings are not null-terminated.
struct string {
pub:
	str byteptr
	len int
}

struct ustring {
pub:
	s     string
	runes []int
	len   int
}

// For C strings only
fn C.strlen(s byteptr) int

fn todo() { } 

// Converts a C string to a V string
pub fn tos(s byteptr, len int) string {
	// This should never happen.
	if isnil(s) {
		panic('tos(): nil string')
	}
	return string {
		str: s
		len: len
	}
}

pub fn tos_clone(s byteptr) string {
	if isnil(s) {
		panic('tos: nil string')
		return string{}
	}
	len := strlen(s)
	res := tos(s, len)
	return res.clone()
}

// Same as `tos`, but calculates the length. Called by `string(bytes)` casts. 
fn tos2(s byteptr) string {
	if isnil(s) {
		panic('tos2: nil string')
		return string{}
	}
	len := C.strlen(s)
	res := tos(s, len)
	return res
}

pub fn (a string) clone() string {
	mut b := string {
		len: a.len
		str: malloc(a.len + 1)
	}
	for i := 0; i < a.len; i++ {
		b[i] = a[i]
	}
	b[a.len] = `\0`
	return b
}

pub fn (s string) cstr() byteptr {
	clone := s.clone()
	return clone.str
}

pub fn (s string) replace(rep, with string) string {
	if s.len == 0 || rep.len == 0 {
		return s
	}
	// println('"$s" replace  "$rep" with "$with" rep.len=$rep.len')
	// TODO PERF Allocating ints is expensive. Should be a stack array
	// Get locations of all reps within this string
	mut idxs := []int{}
	mut rem := s
	mut rstart := 0
	for {
		mut i := rem.index(rep)
		if i < 0 {break}
		idxs << rstart + i
		i += rep.len
		rstart += i
		rem = rem.substr(i, rem.len)
	}
	// Dont change the string if there's nothing to replace
	if idxs.len == 0 {
		return s
	}
	// Now we know the number of replacements we need to do and we can calc the len of the new string
	new_len := s.len + idxs.len * (with.len - rep.len)
	mut b := malloc(new_len + 1)// add a newline just in case
	// Fill the new string
	mut idx_pos := 0
	mut cur_idx := idxs[idx_pos]
	mut b_i := 0
	for i := 0; i < s.len; i++ {
		// Reached the location of rep, replace it with "with"
		if i == cur_idx {
			for j := 0; j < with.len; j++ {
				b[b_i] = with[j]
				b_i++
			}
			// Skip the length of rep, since we just replaced it with "with"
			i += rep.len - 1
			// Go to the next index
			idx_pos++
			if idx_pos < idxs.len {
				cur_idx = idxs[idx_pos]
			}
		}
		// Rep doesnt start here, just copy
		else {
			b[b_i] = s[i]
			b_i++
		}
	}
	b[new_len] = `\0`
	return tos(b, new_len)
}

pub fn (s string) int() int {
	return C.atoi(s.str)
}

pub fn (s string) f32() f32 {
	return C.atof(s.str)
}

// ==
fn (s string) eq(a string) bool {
	if isnil(s.str) {
		panic('string.eq(): nil string')
	}
	if s.len != a.len {
		return false
	}
	for i := 0; i < s.len; i++ {
		if s[i] != a[i] {
			return false
		}
	}
	return true
}

// !=
fn (s string) ne(a string) bool {
	return !s.eq(a)
}

// s < a
fn (s string) lt(a string) bool {
	for i := 0; i < s.len; i++ {
		if i >= a.len || s[i] > a[i] {
			return false
		}
		else if s[i] < a[i] {
			return true
		}
	}
	if s.len < a.len {
		return true
	}
	return false
}

// s <= a
fn (s string) le(a string) bool {
	return s.lt(a) || s.eq(a)
}

// s > a
fn (s string) gt(a string) bool {
	return !s.le(a)
}

// s >= a
fn (s string) ge(a string) bool {
	return !s.lt(a)
}

// TODO `fn (s string) + (a string)` ? To be consistent with operator overloading syntax.
pub fn (s string) add(a string) string {
	new_len := a.len + s.len
	mut res := string {
		len: new_len
		str: malloc(new_len + 1)
	}
	for j := 0; j < s.len; j++ {
		res[j] = s[j]
	}
	for j := 0; j < a.len; j++ {
		res[s.len + j] = a[j]
	}
	res[new_len] = `\0`// V strings are not null terminated, but just in case
	return res
}

pub fn (s string) split(delim string) []string {
	// println('string split delim="$delim" s="$s"')
	mut res := []string
	if delim.len == 0 {
		res << s
		return res
	}
	if delim.len == 1 {
		return s.split_single(delim[0])
		// println('split 1 only')
		// os.exit()
	}
	mut i := 0
	mut start := 0// - 1
	for i < s.len {
		// printiln(i)
		mut a := s[i] == delim[0]
		mut j := 1
		for j < delim.len && a {
			a = a && s[i + j] == delim[j]
			j++
		}
		last := i == s.len - 1
		if a || last {
			if last {
				i++
			}
			mut val := s.substr(start, i)
			// println('got it "$val" start=$start i=$i delim="$delim"')
			if val.len > 0 {
				// todo perf
				// val now is '___VAL'. remove '___' from the start
				if val.starts_with(delim) {
					// println('!!')
					val = val.right(delim.len)
				}
				res << val.trim_space()
			}
			start = i
		}
		i++
	}
	return res
}

pub fn (s string) split_single(delim byte) []string {
	mut res := []string
	if int(delim) == 0 {
		res << s
		return res
	}
	mut i := 0
	mut start := 0
	for i < s.len {
		a := s[i] == delim
		b := i == s.len - 1
		if a || b {
			if i == s.len - 1 {
				i++
			}
			val := s.substr(start, i)
			if val.len > 0 {
				res << val.trim_space()
			}
			start = i + 1
		}
		i++
	}
	return res
}

pub fn (s string) split_into_lines() []string {
	mut res := []string
	if s.len == 0 {
		return res
	}
	mut start := 0
	for i := 0; i < s.len; i++ {
		last := i == s.len - 1
		if int(s[i]) == 10 || last {
			if last {
				i++
			}
			line := s.substr(start, i)
			res << line
			start = i + 1
		}
	}
	return res
}

// 'hello'.left(2) => 'he'
pub fn (s string) left(n int) string {
	if n >= s.len {
		return s
	}
	return s.substr(0, n)
}

pub fn (s string) right(n int) string {
	if n >= s.len {
		return ''
	}
	return s.substr(n, s.len)
}

// Because the string is immutable, it is safe for multiple strings to share
// the same storage, so slicing s results in a new 2-word structure with a
// potentially different pointer and length that still refers to the same byte
// sequence. This means that slicing can be done without allocation or copying,
// making string slices as efficient as passing around explicit indexes.
// substr without allocations. Reuses memory and works great. BUT. This substring does not have
// a \0 at the end, and it's not possible to add it. So if we have s = 'privet'
// and substr := s.substr_fast(1, 4) ('riv')
// puts(substr.str) will print 'rivet'
// Avoid using C functions with these substrs!
pub fn (s string) substr(start, end int) string {
	/*
	if start > end || start >= s.len || end > s.len || start < 0 || end < 0 {
		panic('substr($start, $end) out of bounds (len=$s.len)')
		return ''
	}
*/
	if start >= s.len {
		return ''
	}
	len := end - start
	res := string {
		str: s.str + start
		len: len
	}
	return res
}

// KMP search
pub fn (s string) index(p string) int {
	if p.len > s.len {
		return -1
	}
	mut prefix := [0; p.len]
	mut j := 0
	for i := 1; i < p.len; i++ {
		for p[j] != p[i] && j > 0 {
			j = prefix[j - 1]
		}
		if p[j] == p[i] {
			j++
		}
		prefix[i] = j
	}
	j = 0
	for i := 0; i < s.len; i++ {
		for p[j] != s[i] && j > 0 {
			j = prefix[j - 1]
		}
		if p[j] == s[i] {
			j++
		}
		if j == p.len {
			prefix.free()
			return i - p.len + 1
		}
	}
	prefix.free()
	return -1
}

pub fn (s string) last_index(p string) int {
	if p.len > s.len {
		return -1
	}
	mut i := s.len - p.len
	for i >= 0 {
		mut j := 0
		for j < p.len && s[i + j] == p[j] {
			j++
		}
		if j == p.len {
			return i
		}
		i--
	}
	return -1
}

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
		for j < p.len && s[ii] == p[j] {
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

pub fn (s string) contains(p string) bool {
	res := s.index(p) > 0 - 1
	return res
}

pub fn (s string) starts_with(p string) bool {
	res := s.index(p) == 0
	return res
}

pub fn (s string) ends_with(p string) bool {
	if p.len > s.len {
		return false
	}
	res := s.last_index(p) == s.len - p.len
	return res
}

// TODO only works with ASCII
pub fn (s string) to_lower() string {
	mut b := malloc(s.len)// TODO + 1 ??
	for i := 0; i < s.len; i++ {
		b[i] = C.tolower(s.str[i])
	}
	return tos(b, s.len)
}

pub fn (s string) to_upper() string {
	mut b := malloc(s.len)// TODO + 1 ??
	for i := 0; i < s.len; i++ {
		b[i] = C.toupper(s.str[i])
	}
	return tos(b, s.len)
}

// 'hey [man] how you doin'
// find_between('[', ']') == 'man'
pub fn (s string) find_between(start, end string) string {
	start_pos := s.index(start)
	if start_pos == -1 {
		return ''
	}
	// First get everything to the right of 'start'
	val := s.right(start_pos + start.len)
	end_pos := val.index(end)
	if end_pos == -1 {
		return val
	}
	return val.left(end_pos)
}

// TODO generic
pub fn (ar []string) contains(val string) bool {
	for s in ar {
		if s == val {
			return true
		}
	}
	return false
}

// TODO generic
pub fn (ar []int) contains(val int) bool {
	for i, s in ar {
		if s == val {
			return true
		}
	}
	return false
}

/*
pub fn (a []string) to_c() voidptr {
	char ** res = malloc(sizeof(char*) * a.len);
	for i := 0; i < a.len; i++ {
		val := a[i]
		# res[i] = val.str;
	}
	return res;
	return 0
}
*/

fn is_space(c byte) bool {
	return C.isspace(c)
}

pub fn (c byte) is_space() bool {
	return is_space(c)
}

pub fn (s string) trim_space() string {
	if s == '' {
		return ''
	}
	// println('TRIM SPACE "$s"')
	mut i := 0
	for i < s.len && is_space(s[i]) {
		i++
	}
	mut res := s.right(i)
	mut end := res.len - 1
	for end >= 0 && is_space(res[end]) {
		// C.printf('end=%d c=%d %c\n', end, res.str[end])
		end--
	}
	res = res.left(end + 1)
	// println('after SPACE "$res"')
	return res
}

pub fn (s string) trim(c byte) string {
	if s == '' {
		return ''
	}
	mut i := 0
	for i < s.len && c == s[i] {
		i++
	}
	mut res := s.right(i)
	mut end := res.len - 1
	for end >= 0 && c == res[end] {
		end--
	}
	res = res.left(end + 1)
	return res
}

pub fn (s string) trim_left(cutset string) string {
	mut start := s.index(cutset)
	if start != 0 {
		return s
	}
	for start < s.len - 1 && s[start] == cutset[0] {
		start++
	}
	return s.right(start)
}

pub fn (s string) trim_right(cutset string) string {
	pos := s.last_index(cutset)
	if pos == -1 {
		return s
	}
	return s.left(pos)
}

// fn print_cur_thread() {
// //C.printf("tid = %08x \n", pthread_self());
// }
fn compare_strings(a, b *string) int {
	if a.lt(b) {
		return -1
	}
	if a.gt(b) {
		return 1
	}
	return 0
}

fn compare_strings_by_len(a, b *string) int {
	if a.len < b.len {
		return -1
	}
	if a.len > b.len {
		return 1
	}
	return 0
}

fn compare_lower_strings(a, b *string) int {
	aa := a.to_lower()
	bb := b.to_lower()
	return compare_strings(aa, bb)
}

pub fn (s mut []string) sort() {
	s.sort_with_compare(compare_strings)
}

pub fn (s mut []string) sort_ignore_case() {
	s.sort_with_compare(compare_lower_strings)
}

pub fn (s mut []string) sort_by_len() {
	s.sort_with_compare(compare_strings_by_len)
}

pub fn (s string) ustring() ustring {
	mut res := ustring {
		s: s
		// runes will have at least s.len elements, save reallocations
		// TODO use VLA for small strings?
		runes: new_array(0, s.len, sizeof(int))
	}
	for i := 0; i < s.len; i++ {
		char_len := utf8_char_len(s.str[i])
		res.runes << i
		i += char_len - 1
		res.len++
	}
	return res
}

// A hack that allows to create ustring without allocations.
// It's called from functions like draw_text() where we know that the string is going to be freed
// right away. Uses global buffer for storing runes []int array.
__global g_ustring_runes []int
pub fn (s string) ustring_tmp() ustring {
	mut res := ustring {
		s: s
	}
	res.runes = g_ustring_runes
	res.runes.len = s.len
	mut j := 0
	for i := 0; i < s.len; i++ {
		char_len := utf8_char_len(s.str[i])
		res.runes[j] = i
		j++
		i += char_len - 1
		res.len++
	}
	return res
}

fn (u ustring) substr(start, end int) string {
	start = u.runes[start]
	if end >= u.runes.len {
		end = u.s.len
	}
	else {
		end = u.runes[end]
	}
	return u.s.substr(start, end)
}

fn (u ustring) left(pos int) string {
	return u.substr(0, pos)
}

fn (u ustring) right(pos int) string {
	return u.substr(pos, u.len)
}

fn (s string) at(idx int) byte {
	if idx < 0 || idx >= s.len {
		panic('string index out of range: $idx / $s.len')
	}
	return s.str[idx]
}

pub fn (u ustring) at(idx int) string {
	return u.substr(idx, idx + 1)
}

fn (u ustring) free() {
	u.runes.free()
}

fn abs(a int) int {
	if a >= 0 {
		return a
	}
	return -a
}

pub fn (c byte) is_digit() bool {
	return c >= `0` && c <= `9`
}

pub fn (c byte) is_letter() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
}

pub fn (s string) free() {
	C.free(s.str)
}

fn (arr []string) free() {
	for s in arr {
		s.free()
	}
	C.free(arr.data)
}

// all_before('23:34:45.234', '.') == '23:34:45'
pub fn (s string) all_before(dot string) string {
	pos := s.index(dot)
	if pos == -1 {
		return s
	}
	return s.left(pos)
}

pub fn (s string) all_before_last(dot string) string {
	pos := s.last_index(dot)
	if pos == -1 {
		return s
	}
	return s.left(pos)
}

pub fn (s string) all_after(dot string) string {
	pos := s.last_index(dot)
	if pos == -1 {
		return s
	}
	return s.right(pos + dot.len)
}

// fn (s []string) substr(a, b int) string {
// return join_strings(s.slice_fast(a, b))
// }
pub fn (a []string) join(del string) string {
	if a.len == 0 {
		return ''
	}
	mut len := 0
	for i, val in a {
		len += val.len + del.len
	}
	len -= del.len
	// Allocate enough memory
	mut res := ''
	res.len = len
	res.str = malloc(res.len + 1)
	mut idx := 0
	// Go thru every string and copy its every char one by one
	for i, val in a {
		for j := 0; j < val.len; j++ {
			c := val[j]
			res.str[idx] = val.str[j]
			idx++
		}
		// Add del if it's not last
		if i != a.len - 1 {
			for k := 0; k < del.len; k++ {
				res.str[idx] = del.str[k]
				idx++
			}
		}
	}
	res.str[res.len] = `\0`
	return res
}

pub fn (s []string) join_lines() string {
	return s.join('\n')
}

pub fn (s string) reverse() string {
	mut res := string {
		len: s.len
		str: malloc(s.len + 1)
	}

	for i := s.len - 1; i >= 0; i-- {
        res[s.len-i-1] = s[i]
	}

	return res
}

// 'hello'.limit(2) => 'he'
// 'hi'.limit(10) => 'hi'
pub fn (s string) limit(max int) string {
	u := s.ustring()
	if u.len <= max {
		return s
	}
	return u.substr(0, max)
}

// TODO is_white_space()
pub fn (c byte) is_white() bool {
	i := int(c)
	return i == 10 || i == 32 || i == 9 || i == 13 || c == `\r`
}


pub fn (s string) hash() int {
	mut hash := int(0)
	for i := 0; i < s.len; i++ {
		// if key == 'Content-Type' {
		// println('$i) $hash')
		// }
		hash = hash * int(31) + int(s.str[i])
	}
	return hash
}

