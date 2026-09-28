module driver

// c_link_input_indices identifies positional native inputs without interpreting
// an option's operand as another option or a source/object filename. Keep indices
// so callers can preserve the original argument bytes and ordering.
fn c_link_input_indices(flags []string) []int {
	mut inputs := []int{}
	mut i := 0
	for i < flags.len {
		flag := flags[i].trim_space()
		if flag == '-x' || c_flag_consumes_next_operand(flag) {
			i += 2
			continue
		}
		if flag.len > 0 && !flag.starts_with('-') {
			inputs << i
		}
		i++
	}
	return inputs
}
