import os

const or_review_vexe = @VEXE
const or_review_tests_dir = os.dir(@FILE)
const or_review_v3_dir = os.dir(or_review_tests_dir)
const or_review_vlib_dir = os.dir(or_review_v3_dir)
const or_review_v3_src = os.join_path(or_review_v3_dir, 'v3.v')

fn or_review_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_or_expr_transform_review_test')
}

fn testsuite_begin() {
	v3_bin := or_review_v3_bin_path()
	if os.exists(v3_bin) {
		os.rm(v3_bin) or {}
	}
}

fn build_v3_or_review() string {
	v3_bin := or_review_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${or_review_vexe} -gc none -path "${or_review_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${or_review_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn or_review_gen_c(v3_bin string, name string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C'
	return os.read_file(c_path) or { panic(err) }
}

fn test_channel_receive_or_stabilizes_side_effectful_source() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'channel_receive_or_source_once',
		'__global calls int\n\nfn next_channel() chan int {\n\tcalls++\n\tch := chan int{}\n\tch.close()\n\treturn ch\n}\n\nfn main() {\n\tx := (<-next_channel()) or {\n\t\tprintln(err.msg())\n\t\t-1\n\t}\n\tprintln(int_str(x))\n}\n')
	assert c_source.count('next_channel()') == 1, 'side-effectful channel source was not stabilized'
	assert c_source.contains('sync__Channel* __chan_src_'), 'missing channel source temp'
	assert c_source.contains('sync__Channel__pop(__chan_src_'), 'pop does not use channel source temp'
	assert c_source.contains('sync__Channel__closed_error(__chan_src_'), 'closed_error does not use channel source temp'
}

fn test_array_optional_element_or_uses_loaded_element_error() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'array_optional_element_error_source',
		'fn main() {\n\tmut arr := []?int{}\n\tarr << none\n\tvalue := arr[0] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n')
	assert c_source.contains('Optional __arr_opt_'), 'missing loaded optional temp'
	assert c_source.contains('IError err = __arr_opt_'), 'element failure branch does not use loaded optional err'
}
