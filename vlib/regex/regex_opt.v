module regex
import strings

// compile_opt compile RE pattern string
pub fn (mut re RE) compile_opt(pattern string) ? {
	re_err,err_pos := re.impl_compile(pattern)
	
	if re_err != compile_ok {
		mut err_msg := strings.new_builder(300)
		err_msg.write("\nquery: $pattern\n")
		line := "-".repeat(err_pos)
		err_msg.write("err  : ${line}^\n")
		err_str := re.get_parse_error_string(re_err)
		err_msg.write("ERROR: $err_str\n")
		return error_with_code(err_msg.str(), re_err)
	}
}

// new_regex create a RE of small size, usually sufficient for ordinary use
pub fn new() RE {
	// init regex
    mut re := regex.RE{}
    re.prog = []Token    {len: max_code_len + 1} // max program length, can not be longer then the pattern
    re.cc   = []CharClass{len: max_code_len}     // can not be more char class the the length of the pattern
    re.group_csave_flag = false                 // enable continuos group saving
    re.group_max_nested = 128                   // set max 128 group nested
    re.group_max        = max_code_len >> 1      // we can't have more groups than the half of the pattern legth

    re.group_stack = []int{len: re.group_max, init: -1}
	re.group_data  = []int{len: re.group_max, init: -1}

	return re
}

// regex_opt create new RE object from RE pattern string
pub fn regex_opt(pattern string) ?RE {
	// init regex
    mut re := regex.RE{}
    re.prog = []Token    {len: pattern.len + 1} // max program length, can not be longer then the pattern
    re.cc   = []CharClass{len: pattern.len}     // can not be more char class the the length of the pattern
    re.group_csave_flag = false                 // enable continuos group saving
    re.group_max_nested = 128                   // set max 128 group nested
    re.group_max        = pattern.len >> 1      // we can't have more groups than the half of the pattern legth

    re.group_stack = []int{len: re.group_max, init: -1}
	re.group_data  = []int{len: re.group_max, init: -1}

    // compile the pattern
    re.compile_opt(pattern)?

    return re
}
