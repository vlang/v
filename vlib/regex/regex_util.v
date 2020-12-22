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

// new_regex create a RE of small size, usually sufficient for ordinary use
[deprecated]
pub fn new_regex() RE {
	return impl_new_regex_by_size(1)
}

// new_regex_by_size create a RE of large size, mult specify the scale factor of the memory that will be allocated
[deprecated]
pub fn new_regex_by_size(mult int) RE {
	return impl_new_regex_by_size(mult)
}
fn impl_new_regex_by_size(mult int) RE {
	// init regex
    mut re := regex.RE{}
    re.prog = []Token    {len: max_code_len*mult} // max program length, can not be longer then the pattern
    re.cc   = []CharClass{len: max_code_len*mult} // can not be more char class the the length of the pattern
    re.group_csave_flag = false                   // enable continuos group saving
    re.group_max_nested = 3*mult                  // set max 128 group nested
    re.group_max        = max_code_len*mult >> 1  // we can't have more groups than the half of the pattern legth

    re.group_stack = []int{len: re.group_max, init: -1}
	re.group_data  = []int{len: re.group_max, init: -1}

	return re
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

