/*

regex 1.0 alpha

Copyright (c) 2019-2020 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

*/
module regex

/******************************************************************************
*
* Inits
*
******************************************************************************/
// regex create a regex object from the query string
[deprecated]
pub fn regex(pattern string) (RE,int,int){
	// init regex
    mut re := regex.RE{}
    re.prog = []Token    {len: pattern.len + 1} // max program length, can not be longer then the pattern
    re.cc   = []CharClass{len: pattern.len}     // can not be more char class the the length of the pattern
    re.group_csave_flag = false                 // enable continuos group saving
    re.group_max_nested = 128                   // set max 128 group nested
    re.group_max        = pattern.len >> 1      // we can't have more groups than the half of the pattern legth

    re.group_stack = []int{len: re.group_max, init: -1}
	re.group_data  = []int{len: re.group_max, init: -1}

	re_err,err_pos := re.compile(pattern)
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
		tmp_index := re.group_map[group_name]-1
		start     := re.groups[tmp_index * 2]
		end       := re.groups[tmp_index * 2 + 1]
		return start,end
	}
	return -1, -1
}

// get_group_by_name get a group boundaries by its name
pub fn (re RE) get_group_by_name(in_txt string, group_name string) string {
	if group_name in re.group_map {
		tmp_index := re.group_map[group_name]-1
		start     := re.groups[tmp_index * 2]
		end       := re.groups[tmp_index * 2 + 1]
		return in_txt[start..end]
	}
	return ""
}

// get_group_by_id get a group string by its id
pub fn (re RE) get_group_by_id(in_txt string, group_id int) string {
	if group_id < (re.groups.len >> 1) {
		index := group_id << 1
		start := re.groups[index]
		end   := re.groups[index + 1]
		return in_txt[start..end]
	}
	return ""
}

// get_group_by_id get a group boundaries by its id
pub fn (re RE) get_group_bounds_by_id(group_id int) (int,int) {
	if group_id < (re.groups.len >> 1) {
		index := group_id << 1
		return re.groups[index], re.groups[index + 1]
	}
	return -1, -1
}

pub
struct Re_group {
pub:
	start int = -1
	end   int = -1
}

// get_group_list return a list of Re_group for the found groups
pub fn (re RE) get_group_list() []Re_group {
	mut res := []Re_group{len: re.groups.len >> 1}
	mut gi := 0
	//println("len: ${re.groups.len} groups: ${re.groups}")

	for gi < re.groups.len {
		if re.groups[gi] >= 0 {
			txt_st := re.groups[gi]
            txt_en := re.groups[gi+1]

            //println("#${gi/2} start: ${re.groups[gi]} end: ${re.groups[gi + 1]} ")
            if txt_st >= 0 && txt_en > txt_st {
				tmp := Re_group{ start: re.groups[gi], end: re.groups[gi + 1]}
				//println(tmp)
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
* Finders
*
******************************************************************************/
// find try to find the first match in the input string
[direct_array_access]
pub fn (mut re RE) find(in_txt string) (int,int) {
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

// find_all find all the non overlapping occurrences of the match pattern
[direct_array_access]
pub fn (mut re RE) find_all(in_txt string) []int {
	mut i := 0
	mut res := []int{}
	mut ls := -1

	for i < in_txt.len {
		//--- speed references ---
		mut s := -1
		mut e := -1
		unsafe {
			tmp_str := tos(in_txt.str+i, in_txt.len-i)
			s,e = re.find(tmp_str)
		}
		//------------------------
		//s,e := re.find(in_txt[i..])
		//------------------------
		if s >= 0 && e > s && i+s > ls {
			//println("find match in: ${i+s},${i+e} [${in_txt[i+s..i+e]}] ls:$ls")
			res << i+s
			res << i+e
			ls = i+s
			i = i+e
			continue
		} else {
			i++
		}

	}
	return res
}

// find_all_str find all the non overlapping occurrences of the match pattern, return a string list
[direct_array_access]
pub fn (mut re RE) find_all_str(in_txt string) []string {
	mut i := 0
	mut res := []string{}
	mut ls := -1

	for i < in_txt.len {
		//--- speed references ---
		mut s := -1
		mut e := -1
		unsafe {
			tmp_str := tos(in_txt.str+i, in_txt.len-i)
			s,e = re.find(tmp_str)
		}
		//------------------------
		//s,e := re.find(in_txt[i..])
		//------------------------
		if s >= 0 && e > s && i+s > ls {
			//println("find match in: ${i+s},${i+e} [${in_txt[i+s..i+e]}] ls:$ls")
			res << in_txt[i+s..i+e]
			ls = i+s
			i = i+e
			continue
		} else {
			i++
		}

	}
	return res
}
/******************************************************************************
*
* Replacers
*
******************************************************************************/
// replace return a string where the matches are replaced with the replace string
pub fn (mut re RE) replace(in_txt string, repl string) string {
	pos := re.find_all(in_txt)

	if pos.len > 0 {
		mut res := ""
		mut i := 0

		mut s1 := 0
		mut e1 := in_txt.len

		for i < pos.len {
			e1 = pos[i]
			res += in_txt[s1..e1] + repl
			s1 = pos[i+1]
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
	mut i   := 0
	mut res := ""
	mut ls  := -1
	mut s1  := 0

	for i < in_txt.len {
		s,e := re.find(in_txt[i..])
		if s >= 0 && e > s && i+s > ls {
			//println("find match in: ${i+s},${i+e} [${in_txt[i+s..i+e]}] ls:$ls")
			start := i + s
			end   := i + e
			// update grups index diplacement
			mut gi := 0
			for gi < re.groups.len {
				re.groups[gi] += i
				gi++
			}
			repl  := repl_fn(re, in_txt, start, end)

			res += in_txt[s1..start] + repl
			s1 = end 

			ls = i + s
			i  = i + e
			continue
		} else {
			i++
		}
	}
	res += in_txt[s1..]
	return res
}
