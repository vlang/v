module regex
import strings

// compile_opt compile RE pattern string
pub fn (mut re RE) compile_opt(pattern string) ? {
	re_err,err_pos := re.impl_compile(pattern)
	
	if re_err != compile_ok {
		mut err_msg := strings.new_builder(300)
		err_str := re.get_parse_error_string(re_err)
		err_msg.write("$err_str\n")
		err_msg.write("      query: $pattern\n")
		line := "-".repeat(err_pos)
		err_msg.write("    err pos: ${line}^")
		return error_with_code(err_msg.str(), re_err)
	}
}

// new_regex create a RE of small size, usually sufficient for ordinary use
pub fn new() RE {
	return impl_new_regex_by_size(1)
}

// new_regex_by_size create a RE of large size, mult specify the scale factor of the memory that will be allocated
pub fn new_by_size(mult int) RE {
	return impl_new_regex_by_size(mult)
}

// regex_opt create new RE object from RE pattern string
pub fn regex_opt(pattern string) ?RE {
	mut re := new()
	re.compile_opt(pattern)?
	return re
}
