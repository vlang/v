/*
regex 1.0 alpha

Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.
*/
module regex

import strings

/******************************************************************************
*
* Inits
*
******************************************************************************/
// regex create a regex object from the query string, retunr RE object and errors as re_err, err_pos
pub fn regex_base(pattern string) (RE, int, int) {
	// init regex
	mut re := RE{}
	re.prog = []Token{len: pattern.len + 1} // max program length, can not be longer then the pattern
	re.cc = []CharClass{len: pattern.len} // can not be more char class the the length of the pattern
	re.group_csave_flag = false // enable continuos group saving
	re.group_max_nested = pattern.len >> 1 // set max 128 group nested
	re.group_max = pattern.len >> 1 // we can't have more groups than the half of the pattern legth

	re.group_stack = []int{len: re.group_max, init: -1}
	re.group_data = []int{len: re.group_max, init: -1}

	re_err, err_pos := re.impl_compile(pattern)
	return re, re_err, err_pos
}

/******************************************************************************
*
* Utilities
*
******************************************************************************/
// get_group_bounds_by_name get a group boundaries by its name
pub fn (re RE) get_group_bounds_by_name(group_name string) (int, int) {
	if group_name in re.group_map {
		tmp_index := re.group_map[group_name] - 1
		start := re.groups[tmp_index * 2]
		end := re.groups[tmp_index * 2 + 1]
		return start, end
	}
	return -1, -1
}

// get_group_by_name get a group boundaries by its name
pub fn (re RE) get_group_by_name(in_txt string, group_name string) string {
	if group_name in re.group_map {
		tmp_index := re.group_map[group_name] - 1
		start := re.groups[tmp_index * 2]
		end := re.groups[tmp_index * 2 + 1]
		if start >= 0 && end > start {
			return in_txt[start..end]
		}
	}
	return ''
}

// get_group_by_id get a group string by its id
pub fn (re RE) get_group_by_id(in_txt string, group_id int) string {
	if group_id < (re.groups.len >> 1) {
		index := group_id * 2
		start := re.groups[index]
		end := re.groups[index + 1]
		if start >= 0 && end > start {
			return in_txt[start..end]
		}
	}
	return ''
}

// get_group_by_id get a group boundaries by its id
pub fn (re RE) get_group_bounds_by_id(group_id int) (int, int) {
	if group_id < re.group_count {
		index := group_id * 2
		return re.groups[index], re.groups[index + 1]
	}
	return -1, -1
}

pub struct Re_group {
pub:
	start int = -1
	end   int = -1
}

// get_group_list return a list of Re_group for the found groups
pub fn (re RE) get_group_list() []Re_group {
	mut res := []Re_group{len: re.groups.len >> 1}
	mut gi := 0
	// println("len: ${re.groups.len} groups: ${re.groups}")

	for gi < re.groups.len {
		if re.groups[gi] >= 0 {
			txt_st := re.groups[gi]
			txt_en := re.groups[gi + 1]

			// println("#${gi/2} start: ${re.groups[gi]} end: ${re.groups[gi + 1]} ")
			if txt_st >= 0 && txt_en > txt_st {
				tmp := Re_group{
					start: re.groups[gi]
					end: re.groups[gi + 1]
				}
				// println(tmp)
				res[gi >> 1] = tmp
			} else {
				res[gi >> 1] = Re_group{}
			}
		}
		gi += 2
	}
	return res
}

/******************************************************************************
*
* Matchers
*
******************************************************************************/
// match_string Match the pattern with the in_txt string
[direct_array_access]
pub fn (mut re RE) match_string(in_txt string) (int, int) {
	start, mut end := re.match_base(in_txt.str, in_txt.len + 1)
	if end > in_txt.len {
		end = in_txt.len
	}

	if start >= 0 && end > start {
		if (re.flag & f_ms) != 0 && start > 0 {
			return no_match_found, 0
		}
		if (re.flag & f_me) != 0 && end < in_txt.len {
			if in_txt[end] in new_line_list {
				return start, end
			}
			return no_match_found, 0
		}
		return start, end
	}
	return start, end
}

// matches_string Checks if the pattern matches the in_txt string
pub fn (mut re RE) matches_string(in_txt string) bool {
	start, _ := re.match_string(in_txt)
	return start != no_match_found
}

/******************************************************************************
*
* Finders
*
******************************************************************************/
/*
// find internal implementation HERE for reference do not remove!!
[direct_array_access]
fn (mut re RE) find_imp(in_txt string) (int,int) {
	old_flag := re.flag
	re.flag |= f_src  // enable search mode

	start, mut end := re.match_base(in_txt.str, in_txt.len + 1)
	//print("Find [$start,$end] '${in_txt[start..end]}'")
	if end > in_txt.len {
		end = in_txt.len
	}
	re.flag = old_flag

	if start >= 0 && end > start {
		return start, end
	}
	return no_match_found, 0
}
*/

// find try to find the first match in the input string
[direct_array_access]
pub fn (mut re RE) find(in_txt string) (int, int) {
	// old_flag := re.flag
	// re.flag |= f_src  // enable search mode

	mut i := 0
	for i < in_txt.len {
		mut s := -1
		mut e := -1
		unsafe {
			// tmp_str := tos(in_txt.str + i, in_txt.len - i)
			// println("Check: [$tmp_str]")
			s, e = re.match_base(in_txt.str + i, in_txt.len - i + 1)

			if s >= 0 && e > s {
				// println("find match in: ${i+s},${i+e} [${in_txt[i+s..i+e]}]")
				// re.flag = old_flag
				mut gi := 0
				for gi < re.groups.len {
					re.groups[gi] += i
					gi++
				}
				return i + s, i + e
			}
			i++
		}
	}
	// re.flag = old_flag
	return -1, -1
}

// find try to find the first match in the input string strarting from start index
[direct_array_access]
pub fn (mut re RE) find_from(in_txt string, start int) (int, int) {
	old_flag := re.flag
	// re.flag |= f_src // enable search mode

	mut i := start
	if i < 0 {
		return -1, -1
	}
	for i < in_txt.len {
		//--- speed references ---

		mut s := -1
		mut e := -1

		unsafe {
			tmp_str := tos(in_txt.str + i, in_txt.len - i)
			s, e = re.match_string(tmp_str)
		}
		//------------------------
		// s,e = re.find_imp(in_txt[i..])
		//------------------------
		if s >= 0 && e > s {
			// println("find match in: ${i+s},${i+e} [${in_txt[i+s..i+e]}]")
			re.flag = old_flag
			mut gi := 0
			for gi < re.groups.len {
				re.groups[gi] += i
				gi++
			}
			return i + s, i + e
		} else {
			i++
		}
	}
	re.flag = old_flag
	return -1, -1
}

// find_all find all the non overlapping occurrences of the match pattern
[direct_array_access]
pub fn (mut re RE) find_all(in_txt string) []int {
	// old_flag := re.flag
	// re.flag |= f_src // enable search mode

	mut i := 0
	mut res := []int{}

	for i < in_txt.len {
		mut s := -1
		mut e := -1
		unsafe {
			// tmp_str := in_txt[i..]
			// tmp_str := tos(in_txt.str + i, in_txt.len - i)
			// println("Check: [$tmp_str]")
			s, e = re.match_base(in_txt.str + i, in_txt.len + 1 - i)

			if s >= 0 && e > s {
				res << i + s
				res << i + e
				i += e
				continue
			}
			/*
			if e > 0 {
				i += e
				continue
			}
			*/
			i++
		}
	}
	// re.flag = old_flag
	return res
}

// find_all_str find all the non overlapping occurrences of the match pattern, return a string list
[direct_array_access]
pub fn (mut re RE) find_all_str(in_txt string) []string {
	// old_flag := re.flag
	// re.flag |= f_src // enable search mode

	mut i := 0
	mut res := []string{}

	for i < in_txt.len {
		mut s := -1
		mut e := -1
		unsafe {
			// tmp_str := in_txt[i..]
			// tmp_str := tos(in_txt.str + i, in_txt.len - i)
			// println("Check: [$tmp_str]")
			s, e = re.match_base(in_txt.str + i, in_txt.len + 1 - i)

			if s >= 0 && e > s {
				tmp_str := tos(in_txt.str + i, in_txt.len - i)
				mut tmp_e := if e > tmp_str.len { tmp_str.len } else { e }
				// println("Found: $s:$e [${tmp_str[s..e]}]")
				res << tmp_str[..tmp_e]
				i += e
				continue
			}
		}
		/*
		if e > 0 {
			i += e
			continue
		}
		*/
		i++
	}
	// re.flag = old_flag
	return res
}

/******************************************************************************
*
* Replacers
*
******************************************************************************/
// replace_simple return a string where the matches are replaced with the replace string
pub fn (mut re RE) replace_simple(in_txt string, repl string) string {
	pos := re.find_all(in_txt)

	if pos.len > 0 {
		mut res := ''
		mut i := 0

		mut s1 := 0
		mut e1 := in_txt.len

		for i < pos.len {
			e1 = pos[i]
			res += in_txt[s1..e1] + repl
			s1 = pos[i + 1]
			i += 2
		}

		res += in_txt[s1..]
		return res
	}
	return in_txt
}

// type of function used for custom replace
// in_txt  source text
// start   index of the start of the match in in_txt
// end     index of the end   of the match in in_txt
// the match is in in_txt[start..end]
pub type FnReplace = fn (re RE, in_txt string, start int, end int) string

// replace_by_fn return a string where the matches are replaced with the string from the repl_fn callback function
pub fn (mut re RE) replace_by_fn(in_txt string, repl_fn FnReplace) string {
	mut i := 0
	mut res := strings.new_builder(in_txt.len)
	mut last_end := 0

	for i < in_txt.len {
		// println("Find Start. $i [${in_txt[i..]}]")
		s, e := re.find_from(in_txt, i)
		// println("Find End.")
		if s >= 0 && e > s {
			// println("find match in: ${s},${e} [${in_txt[s..e]}]")

			if last_end < s {
				res.write_string(in_txt[last_end..s])
			}
			/*
			for g_i in 0 .. re.group_count {
				re.groups[g_i * 2] += i
				re.groups[(g_i * 2) + 1] += i
			}
			*/
			repl := repl_fn(re, in_txt, s, e)
			// println("repl res: $repl")
			res.write_string(repl)
			// res.write_string("[[${in_txt[s..e]}]]")

			last_end = e
			i = e
		} else {
			break
			// i++
		}
		// println(i)
	}
	if last_end >= 0 && last_end < in_txt.len {
		res.write_string(in_txt[last_end..])
	}
	return res.str()
}

fn (re RE) parsed_replace_string(in_txt string, repl string) string {
	str_lst := repl.split('\\')
	mut res := str_lst[0]
	mut i := 1
	for i < str_lst.len {
		tmp := str_lst[i]
		// println("tmp: ${tmp}")
		if tmp.len > 0 && tmp[0] >= `0` && tmp[0] <= `9` {
			group_id := int(tmp[0] - `0`)
			group := re.get_group_by_id(in_txt, group_id)
			// println("group: $group_id [$group]")
			res += '$group${tmp[1..]}'
		} else {
			res += '\\' + tmp
		}
		i++
	}
	return res
}

// replace return a string where the matches are replaced with the repl_str string,
// this function support use groups in the replace string
pub fn (mut re RE) replace(in_txt string, repl_str string) string {
	mut i := 0
	mut res := strings.new_builder(in_txt.len)
	mut last_end := 0

	for i < in_txt.len {
		// println("Find Start. $i [${in_txt[i..]}]")
		s, e := re.find_from(in_txt, i)
		// println("Find End.")
		if s >= 0 && e > s {
			// println("find match in: ${s},${e} [${in_txt[s..e]}]")

			if last_end < s {
				res.write_string(in_txt[last_end..s])
			}
			/*
			for g_i in 0 .. re.group_count {
				re.groups[g_i * 2] += i
				re.groups[(g_i * 2) + 1] += i
			}
			*/
			// repl := repl_fn(re, in_txt, s, e)
			repl := re.parsed_replace_string(in_txt, repl_str)
			// println("repl res: $repl")
			res.write_string(repl)
			// res.write_string("[[${in_txt[s..e]}]]")

			last_end = e
			i = e
		} else {
			break
			// i++
		}
		// println(i)
	}
	if last_end >= 0 && last_end < in_txt.len {
		res.write_string(in_txt[last_end..])
	}
	return res.str()
}

// replace_n return a string where the firts count matches are replaced with the repl_str string,
// if count is > 0 the replace began from the start of the string toward the end
// if count is < 0 the replace began from the end of the string toward the start
// if count is 0 do nothing
pub fn (mut re RE) replace_n(in_txt string, repl_str string, count int) string {
	mut i := 0
	mut index := 0
	mut i_p := 0
	mut res := strings.new_builder(in_txt.len)
	mut lst := re.find_all(in_txt)

	if count < 0 { // start from the right of the string
		lst = lst#[count * 2..] // limitate the number of substitions
	} else if count > 0 { // start from the left of the string
		lst = lst#[..count * 2] // limitate the number of substitions
	} else if count == 0 { // no replace
		return in_txt
	}

	// println("found: ${lst}")
	for index < lst.len {
		i = lst[index]
		res.write_string(in_txt[i_p..i])
		res.write_string(repl_str)
		index++
		i_p = lst[index]
		index++
	}
	i = i_p
	res.write_string(in_txt[i..])

	return res.str()
}
