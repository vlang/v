module builtin

pub struct string {
pub:
	str JS.String
	len int
}

pub fn (s string) runes() []rune {
	mut runes := []rune{}
	for i := 0; i < s.len; i++ {
		mut r := rune(`0`)
		#r = new rune(s.str[i.val].charCodeAt())
		runes << r
	}
	return runes
}

pub fn (s string) slice(a int, b int) string {
	return string(s.str.slice(JS.Number(a), JS.Number(b)))
}

pub fn (s string) substr(start int, end int) string {
	return s.slice(start, end)
}

pub fn (s string) after(dot string) string {
	return string(s.str.slice(JS.Number(int(s.str.lastIndexOf(dot.str)) + 1), s.str.length))
}

pub fn (s string) after_char(dot u8) string {
	// TODO: Implement after byte
	return s
}

pub fn (s string) all_after(dot string) string {
	pos := if dot.len == 0 { -1 } else { int(s.str.indexOf(dot.str)) }
	if pos == -1 {
		return s.clone()
	}
	return s[pos + dot.len..]
}

// why does this exist?
pub fn (s string) all_after_last(dot string) string {
	pos := if dot.len == 0 { -1 } else { int(s.str.lastIndexOf(dot.str)) }
	if pos == -1 {
		return s.clone()
	}
	return s[pos + dot.len..]
}

pub fn (s string) all_before(dot string) string {
	pos := if dot.len == 0 { -1 } else { int(s.str.indexOf(dot.str)) }
	if pos == -1 {
		return s.clone()
	}
	return s[..pos]
	// return string(s.str.slice(0, s.str.indexOf(dot.str)))
}

pub fn (s string) all_before_last(dot string) string {
	pos := if dot.len == 0 { -1 } else { int(s.str.lastIndexOf(dot.str)) }
	if pos == -1 {
		return s.clone()
	}
	return s[..pos]
}

pub fn (s string) bool() bool {
	return s == 'true'
}

pub fn (s string) split(dot string) []string {
	tmparr := s.str.split(dot.str).map(fn (it JS.Any) JS.Any {
		res := ''
		#res.str = it

		return res
	})
	_ := tmparr
	mut arr := []string{}
	#arr = new array(new array_buffer({arr: tmparr,index_start: new int(0),len: new int(tmparr.length)}))

	return arr
}

pub fn (s string) bytes() []u8 {
	sep := ''
	tmparr := s.str.split(sep.str).map(fn (it JS.Any) JS.Any {
		return JS.Any(u8(JS.String(it).charCodeAt(JS.Number(0))))
	})
	_ := tmparr
	mut arr := []u8{}
	#arr = new array(new array_buffer({arr: tmparr,index_start: new int(0),len: new int(tmparr.length)}))

	return arr
}

pub fn (s string) capitalize() string {
	part := string(s.str.slice(JS.Number(1), s.str.length))
	return string(s.str.charAt(JS.Number(0)).toUpperCase().concat(part.str))
}

pub fn (s string) clone() string {
	return string(s.str)
}

pub fn (s string) contains(substr string) bool {
	return bool(s.str.includes(substr.str))
}

pub fn (s string) contains_any(chars string) bool {
	sep := ''
	res := chars.str.split(sep.str)
	for i in 0 .. int(res.length) {
		if bool(s.str.includes(JS.String(res.at(JS.Number(i))))) {
			return true
		}
	}
	return false
}

pub fn (s string) contains_any_substr(chars []string) bool {
	if chars.len == 0 {
		return true
	}
	for x in chars {
		if bool(s.str.includes(x.str)) {
			return true
		}
	}
	return false
}

pub fn (s string) count(substr string) int {
	// TODO: "error: `[]JS.String` is not a struct" when returning arr.length or arr.len
	arr := s.str.split(substr.str)
	len := int(arr.length)
	if len == 0 {
		return 0
	} else {
		return len - 1
	}
}

pub fn (s string) ends_with(p string) bool {
	mut res := false
	#res.val = s.str.endsWith(p.str)

	return res
}

pub fn (s string) starts_with(p string) bool {
	return bool(s.str.startsWith(p.str))
}

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

pub fn (s string) find_between(start string, end string) string {
	return string(s.str.slice(JS.Number(int(s.str.indexOf(start.str)) + 1), s.str.indexOf(end.str)))
}

// unnecessary in the JS backend, implemented for api parity.
pub fn (s &string) free() {}

pub fn (s string) hash() int {
	mut h := u32(0)
	if h == 0 && s.len > 0 {
		for c in s {
			h = h * 31 + u32(c)
		}
	}
	return int(h)
}

// int returns the value of the string as an integer `'1'.int() == 1`.
pub fn (s string) int() int {
	return int(JS.parseInt(s.str))
}

// i64 returns the value of the string as i64 `'1'.i64() == i64(1)`.
pub fn (s string) i64() i64 {
	return i64(JS.parseInt(s.str))
}

// i8 returns the value of the string as i8 `'1'.i8() == i8(1)`.
pub fn (s string) i8() i8 {
	return i8(JS.parseInt(s.str))
}

// i16 returns the value of the string as i16 `'1'.i16() == i16(1)`.
pub fn (s string) i16() i16 {
	return i16(JS.parseInt(s.str))
}

// f32 returns the value of the string as f32 `'1.0'.f32() == f32(1)`.
pub fn (s string) f32() f32 {
	// return C.atof(&char(s.str))
	return f32(JS.parseFloat(s.str))
}

// f64 returns the value of the string as f64 `'1.0'.f64() == f64(1)`.
pub fn (s string) f64() f64 {
	return f64(JS.parseFloat(s.str))
}

// u16 returns the value of the string as u16 `'1'.u16() == u16(1)`.
pub fn (s string) u16() u16 {
	return u16(JS.parseInt(s.str))
}

// u32 returns the value of the string as u32 `'1'.u32() == u32(1)`.
pub fn (s string) u32() u32 {
	return u32(JS.parseInt(s.str))
}

// u64 returns the value of the string as u64 `'1'.u64() == u64(1)`.
pub fn (s string) u64() u64 {
	return u64(JS.parseInt(s.str))
}

pub fn (s string) u8() u64 {
	res := u8(0)
	#res.val = u8(JS.parseInt(s.str))

	return res
}

// trim_right strips any of the characters given in `cutset` from the right of the string.
// Example: assert ' Hello V d'.trim_right(' d') == ' Hello V'
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

// trim_left strips any of the characters given in `cutset` from the left of the string.
// Example: assert 'd Hello V developer'.trim_left(' d') == 'Hello V developer'
[direct_array_access]
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

// trim_prefix strips `str` from the start of the string.
// Example: assert 'WorldHello V'.trim_prefix('World') == 'Hello V'
[deprecated: 'use s.trim_string_left(x) instead']
[deprecated_after: '2022-01-19']
pub fn (s string) trim_prefix(str string) string {
	return s.trim_string_left(str)
}

// trim_suffix strips `str` from the end of the string.
// Example: assert 'Hello VWorld'.trim_suffix('World') == 'Hello V'
[deprecated: 'use s.trim_string_right(x) instead']
[deprecated_after: '2022-01-19']
pub fn (s string) trim_suffix(str string) string {
	return s.trim_string_right(str)
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

// compare_strings_reverse returns `1` if `a < b`, `-1` if `a > b` else `0`.
fn compare_strings_reverse(a &string, b &string) int {
	if a < b {
		return 1
	}
	if a > b {
		return -1
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

// at returns the byte at index `idx`.
// Example: assert 'ABC'.at(1) == u8(`B`)
fn (s string) at(idx int) u8 {
	mut result := u8(0)
	#result = new u8(s.str.charCodeAt(result))

	return result
}

pub fn (s string) to_lower() string {
	mut result := ''
	#let str = s.str.toLowerCase()
	#result = new string(str)

	return result
}

// TODO: check if that behaves the same as V's own string.replace(old_sub,new_sub):
pub fn (s string) replace(old_sub string, new_sub string) string {
	mut result := ''
	#result = new string( s.str.replaceAll(old_sub.str, new_sub.str) )

	return result
}

pub fn (s string) to_upper() string {
	mut result := ''
	#let str = s.str.toUpperCase()
	#result = new string(str)

	return result
}

// sort sorts the string array.
pub fn (mut s []string) sort() {
	s.sort_with_compare(compare_strings)
}

// sort_ignore_case sorts the string array using case insesitive comparing.
pub fn (mut s []string) sort_ignore_case() {
	s.sort_with_compare(compare_lower_strings)
}

// sort_by_len sorts the the string array by each string's `.len` length.
pub fn (mut s []string) sort_by_len() {
	s.sort_with_compare(compare_strings_by_len)
}

// str returns a copy of the string
pub fn (s string) str() string {
	return s.clone()
}

pub fn (s string) repeat(count int) string {
	mut result := ''
	#result = new string(s.str.repeat(count))

	return result
}

// TODO(playX): Use this iterator instead of using .split('').map(c => u8(c))
#function string_iterator(string) { this.stringIteratorFieldIndex = 0; this.stringIteratorIteratedString = string.str; }
#string_iterator.prototype.next = function next() {
#var done = true;
#var value = undefined;
#var position = this.stringIteratorFieldIndex;
#if (position !== -1) {
#var string = this.stringIteratorIteratedString;
#var length = string.length >>> 0;
#if (position >= length) {
#this.stringIteratorFieldIndex = -1;
#} else {
#done = false;
#var first = string.charCodeAt(position);
#if (first < 0xD800 || first > 0xDBFF || position + 1 === length)
#value = new u8(string[position]);
#else {
#value = new u8(string[position]+string[position+1])
#}
#this.stringIteratorFieldIndex = position + value.length;
#}
#}
#return {
#value, done
#}
#}
#string.prototype[Symbol.iterator] = function () { return new string_iterator(this) }

// TODO: Make these functions actually work.
// strip_margin allows multi-line strings to be formatted in a way that removes white-space
// before a delimeter. by default `|` is used.
// Note: the delimiter has to be a byte at this time. That means surrounding
// the value in ``.
//
// Example:
// st := 'Hello there,
// |this is a string,
// |    Everything before the first | is removed'.strip_margin()
// Returns:
// Hello there,
// this is a string,
// Everything before the first | is removed
pub fn (s string) strip_margin() string {
	return s.strip_margin_custom(`|`)
}

// strip_margin_custom does the same as `strip_margin` but will use `del` as delimiter instead of `|`
[direct_array_access]
pub fn (s string) strip_margin_custom(del u8) string {
	mut sep := del
	if sep.is_space() {
		eprintln('Warning: `strip_margin` cannot use white-space as a delimiter')
		eprintln('    Defaulting to `|`')
		sep = `|`
	}
	// don't know how much space the resulting string will be, but the max it
	// can be is this big
	mut ret := []u8{}
	#ret = new array()

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
	/*
	unsafe {
		ret[count] = 0
		return ret.vstring_with_len(count)
	}*/
	mut result := ''
	#for (let x of ret.arr) result.str += String.fromCharCode(x.val)

	return result
}

// split_nth splits the string based on the passed `delim` substring.
// It returns the first Nth parts. When N=0, return all the splits.
// The last returned element has the remainder of the string, even if
// the remainder contains more `delim` substrings.
[direct_array_access]
pub fn (s string) split_nth(delim string, nth int) []string {
	mut res := []string{}
	mut i := 0

	match delim.len {
		0 {
			i = 1
			for ch in s {
				if nth > 0 && i >= nth {
					res << s[i..]
					break
				}
				res << ch.str()
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
					val := s[start..i] //.substr(start, i)
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
			// Take the left part for each delimiter occurence
			for i <= s.len {
				is_delim := i + delim.len <= s.len && s[i..i + delim.len] == delim
				if is_delim {
					was_last := nth > 0 && res.len == nth - 1
					if was_last {
						break
					}
					val := s[start..i] // .substr(start, i)
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

struct RepIndex {
	idx     int
	val_idx int
}

// replace_each replaces all occurences of the string pairs given in `vals`.
// Example: assert 'ABCD'.replace_each(['B','C/','C','D','D','C']) == 'AC/DC'
[direct_array_access]
pub fn (s string) replace_each(vals []string) string {
	if s.len == 0 || vals.len == 0 {
		return s.clone()
	}

	if vals.len % 2 != 0 {
		eprintln('string.replace_each(): odd number of strings')
		return s.clone()
	}

	// `rep` - string to replace
	// `with_` - string to replace with_
	// Remember positions of all rep strings, and calculate the length
	// of the new string to do just one allocation.

	mut idxs := []RepIndex{}
	mut idx := 0
	mut new_len := s.len
	s_ := s.clone()
	#function setCharAt(str,index,chr) {
	#if(index > str.length-1) return str;
	#return str.substring(0,index) + chr + str.substring(index+1);
	#}

	for rep_i := 0; rep_i < vals.len; rep_i = rep_i + 2 {
		rep := vals[rep_i]

		mut with_ := vals[rep_i + 1]
		with_ = with_

		for {
			idx = s_.index_after(rep, idx)
			if idx == -1 {
				break
			}

			for i in 0 .. rep.len {
				mut j_ := i
				j_ = j_
				#s_.str = setCharAt(s_.str,idx + i, String.fromCharCode(127))
			}

			rep_idx := RepIndex{
				idx: 0
				val_idx: 0
			}
			// todo: primitives should always be copied
			#rep_idx.idx = idx.val
			#rep_idx.val_idx = new int(rep_i.val)
			idxs << rep_idx
			idx += rep.len
			new_len += with_.len - rep.len
		}
	}

	if idxs.len == 0 {
		return s.clone()
	}

	idxs.sort(a.idx < b.idx)

	mut b := ''
	#for (let i = 0; i < new_len.val;i++) b.str += String.fromCharCode(127)

	new_len = new_len
	mut idx_pos := 0
	mut cur_idx := idxs[idx_pos]
	mut b_i := 0
	for i := 0; i < s.len; i++ {
		if i == cur_idx.idx {
			rep := vals[cur_idx.val_idx]
			with_ := vals[cur_idx.val_idx + 1]
			for j in 0 .. with_.len {
				mut j_ := j

				j_ = j_
				#b.str = setCharAt(b.str,b_i, with_.str[j])
				//#b.str[b_i] = with_.str[j]
				b_i++
			}
			i += rep.len - 1
			idx_pos++
			if idx_pos < idxs.len {
				cur_idx = idxs[idx_pos]
			}
		} else {
			#b.str = setCharAt(b.str,b_i,s.str[i]) //b.str[b_i] = s.str[i]
			b_i++
		}
	}

	return b
}

// last_index returns the position of the last occurence of the input string.
fn (s string) last_index_(p string) int {
	if p.len > s.len || p.len == 0 {
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

// last_index returns the position of the last occurence of the input string.
pub fn (s string) last_index(p string) ?int {
	idx := s.last_index_(p)
	if idx == -1 {
		return none
	}
	return idx
}

pub fn (s string) trim_space() string {
	res := ''
	#res.str = s.str.trim()

	return res
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

pub fn (s string) split_into_lines() []string {
	mut res := []string{}
	if s.len == 0 {
		return res
	}
	#res.arr.arr = s.str.split("\n")
	#if (res.arr.arr[res.arr.arr.length-1] == "") res.arr.arr.pop();
	#res.arr.len = new int(res.arr.arr.length);
	#res.arr.cap = new int(res.arr.arr.length);

	return res
}

// replace_once replaces the first occurence of `rep` with the string passed in `with`.
pub fn (s string) replace_once(rep string, with_ string) string {
	s2 := ''
	#s2.val = s.str.replace(rep.str,with_.str)

	return s2
}

pub fn (s string) title() string {
	words := s.split(' ')
	mut tit := []string{}
	for word in words {
		tit << word.capitalize()
	}

	title := tit.join(' ')
	return title
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

// is_title returns true if all words of the string is capitalized.
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

// is_capital returns `true`, if the first character in the string `s`,
// is a capital letter, and the rest are NOT.
// Example: assert 'Hello'.is_capital() == true
// Example: assert 'HelloWorld'.is_capital() == false
[direct_array_access]
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
[direct_array_access]
pub fn (s string) starts_with_capital() bool {
	if s.len == 0 || !(s[0] >= `A` && s[0] <= `Z`) {
		return false
	}
	return true
}

// is_upper returns `true` if all characters in the string is uppercase.
// Example: assert 'HELLO V'.is_upper() == true
pub fn (s string) is_upper() bool {
	res := false
	#res.val = s.str == s.str.toUpperCase() && s.str != s.str.toLowerCase()

	return res
}

// is_upper returns `true` if all characters in the string is uppercase.
// Example: assert 'HELLO V'.is_upper() == true
pub fn (s string) is_lower() bool {
	res := false
	#res.val = s.str == s.str.toLowerCase() && s.str != s.str.toUpperCase()

	return res
}

pub fn (s string) reverse() string {
	res := ''
	#res.str = [...s.str].reverse().join('')

	return res
}

pub fn (s string) trim(cutset string) string {
	if s.len < 1 || cutset.len < 1 {
		return s.clone()
	}
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
			return ''
		}
	}
	return s.substr(pos_left, pos_right + 1)
}

pub fn (s []string) join(sep string) string {
	mut res := ''
	for i, str in s {
		res += str
		if i != s.len - 1 {
			res += sep
		}
	}
	return res
}

// There's no better way to find length of JS String in bytes.
#Object.defineProperty(string.prototype,"len", { get: function() {return new int(new TextEncoder().encode(this.str).length);}, set: function(l) {/* ignore */ } });
// index returns the position of the first character of the input string.
// It will return `none` if the input string can't be found.
pub fn (s string) index(search string) ?int {
	res := 0
	#res.val = s.str.indexOf(search)
	if res == -1 {
		return none
	}
	return res
}

pub fn (_rune string) utf32_code() int {
	res := 0
	#res.val = s.str.charCodeAt()

	return res
}

pub fn tos(jsstr JS.String) string {
	res := ''
	#res.str = jsstr

	return res
}

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
