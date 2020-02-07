// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub struct string {
//mut:
	//hash_cache int
pub:
	str byteptr
	len int
}

// For C strings only
fn C.strlen(s byteptr) int

fn todo() { }

pub fn tos(s byteptr) string {
	len := 0
	#len = s.length;
	return string{
		str: s
		len: len
	}	
}	


pub fn (a string) clone() string {
	return a
}

pub fn (s string) replace(rep, with_ string) string {
	return s
}

pub fn (s string) int() int {
	return 0
}

pub fn (s string) i64() i64 {
	return 0
}

pub fn (s string) f32() f32 {
	return 0.0
}

pub fn (s string) f64() f64 {
	return 0.0
}

pub fn (s string) u32() u32 {
	return u32(0)
}

pub fn (s string) u64() u64 {
	return u64(0)
}

pub fn (s string) split(delim string) []string {
	return s.split(delim)
}

pub fn (s string) split_into_lines() []string {
	return s.split('\n')
}

// 'hello'.left(2) => 'he'
pub fn (s string) left(n int) string {
	if n >= s.len {
		return s
	}
	return s.substr(0, n)
}
// 'hello'.right(2) => 'llo'
pub fn (s string) right(n int) string {
	if n >= s.len {
		return ''
	}
	return s.substr(n, s.len)
}

pub fn (s string) substr(start, end int) string {
	return 'a'
}

pub fn (s string) index(p string) int {
	return -1
}

pub fn (s string) index_any(chars string) int {
	return -1
}

pub fn (s string) last_index(p string) int {
	return -1
}

pub fn (s string) index_after(p string, start int) int {
	return -1
}

// counts occurrences of substr in s
pub fn (s string) count(substr string) int {
	return 0 // TODO can never get here - v doesn't know that
}

pub fn (s string) contains(p string) bool {
	return false
}

pub fn (s string) starts_with(p string) bool {
	return false
}

pub fn (s string) ends_with(p string) bool {
	return false
}

// TODO only works with ASCII
pub fn (s string) to_lower() string {
	return s
}

pub fn (s string) to_upper() string {
	return s
}

pub fn (s string) capitalize() string {
	return s
}

pub fn (s string) title() string {
	return s
}

// 'hey [man] how you doin'
// find_between('[', ']') == 'man'
pub fn (s string) find_between(start, end string) string {
	start_pos := s.index(start)
	if start_pos == -1 {
		return ''
	}
	// First get everything to the right of 'start'
	val := s[start_pos + start.len..]
	end_pos := val.index(end)
	if end_pos == -1 {
		return val
	}
	return val[..end_pos]
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


fn is_space(c byte) bool {
	return C.isspace(c)
}

pub fn (c byte) is_space() bool {
	return is_space(c)
}

pub fn (s string) trim_space() string {
	#return s.str.trim(' ');
	return ''
}

pub fn (s string) trim(cutset string) string {
	#return s.str.trim(cutset);
	return ''
}

pub fn (s string) trim_left(cutset string) string {
	#return s.str.trimLeft(cutset);
	return ''
}

pub fn (s string) trim_right(cutset string) string {
	#return s.str.trimRight(cutset);
	return ''
}

// fn print_cur_thread() {
// //C.printf("tid = %08x \n", pthread_self());
// }
pub fn (s mut []string) sort() {
	
}

pub fn (s mut []string) sort_ignore_case() {
}

pub fn (s mut []string) sort_by_len() {
}

fn (s string) at(idx int) byte {
	if idx < 0 || idx >= s.len {
		panic('string index out of range')
	}
	return s.str[idx]
}

pub fn (c byte) is_digit() bool {
	return c >= `0` && c <= `9`
}

pub fn (c byte) is_hex_digit() bool {
	return c.is_digit() || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

pub fn (c byte) is_oct_digit() bool {
	return c >= `0` && c <= `7`
}

pub fn (c byte) is_bin_digit() bool {
	return c == `0` || c == `1`
}

pub fn (c byte) is_letter() bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
}

pub fn (s string) free() {
}

/*
fn (arr []string) free() {
	for s in arr {
		s.free()
	}
	C.free(arr.data)
}
*/

// all_before('23:34:45.234', '.') == '23:34:45'
pub fn (s string) all_before(dot string) string {
	pos := s.index(dot)
	if pos == -1 {
		return s
	}
	return s[..pos]
}

pub fn (s string) all_before_last(dot string) string {
	pos := s.last_index(dot)
	if pos == -1 {
		return s
	}
	return s[..pos]
}

pub fn (s string) all_after(dot string) string {
	pos := s.last_index(dot)
	if pos == -1 {
		return s
	}
	return s[pos + dot.len..]
}

// fn (s []string) substr(a, b int) string {
// return join_strings(s.slice_fast(a, b))
// }
pub fn (a []string) join(del string) string {
	return ''
}

pub fn (s []string) join_lines() string {
	return s.join('\n')
}

pub fn (s string) reverse() string {
	return s
}

pub fn (s string) limit(max int) string {
	if s.len <= max {
		return s
	}
	return s.substr(0, max)
}

// TODO is_white_space()
pub fn (c byte) is_white() bool {
	i := int(c)
	return i == 10 || i == 32 || i == 9 || i == 13 || c == `\r`
}


pub fn (s string) hash() int {
	//mut h := s.hash_cache
	mut h := 0
	if h == 0 && s.len > 0 {
		for c in s {
			h = h * 31 + int(c)
		}
	}
	return h
}

pub fn (s string) bytes() []byte {
	return []
}
