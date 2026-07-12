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

fn or_review_run(v3_bin string, name string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	run := os.execute(bin_path)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
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

fn test_bare_channel_or_is_not_lowered_to_receive() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'bare_channel_or_not_receive',
		'fn main() {\n\tch := chan int{}\n\t_ := ch or { ch }\n}\n')
	assert !c_source.contains('sync__Channel__pop((sync__Channel*)'), 'bare channel or was lowered to a direct receive'
	assert !c_source.contains('sync__Channel__pop(__chan_src_'), 'bare channel or was lowered to a temp receive'
}

fn test_array_optional_element_or_uses_loaded_element_error() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'array_optional_element_error_source',
		'fn main() {\n\tmut arr := []?int{}\n\tarr << none\n\tvalue := arr[0] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n')
	assert c_source.contains('Optional __arr_opt_'), 'missing loaded optional temp'
	assert c_source.contains('IError err = __arr_opt_'), 'element failure branch does not use loaded optional err'
}

fn test_map_optional_element_or_uses_loaded_element_error() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'map_optional_element_error_source',
		"fn main() {\n\tmut m := map[string]?int{}\n\tm['x'] = none\n\tvalue := m['x'] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n")
	assert c_source.contains('Optional __map_opt_'), 'missing loaded map optional temp'
	assert c_source.contains('IError err = __map_opt_'), 'map element failure branch does not use loaded optional err'
}

fn test_user_defined_free_method_is_preserved() {
	v3_bin := build_v3_or_review()
	out := or_review_run(v3_bin, 'user_defined_free_method',
		'struct Hc256 {\nmut:\n\tfreed int\n}\n\nfn (mut h Hc256) free() {\n\th.freed = 7\n}\n\nfn main() {\n\tmut h := Hc256{}\n\th.free()\n\tprintln(int_str(h.freed))\n}\n')
	assert out == '7'
}

fn test_default_free_without_user_method_stays_noop() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'default_free_without_user_method',
		'struct H {}\n\nfn main() {\n\tmut h := H{}\n\th.free()\n\tprintln("ok")\n}\n')
	assert c_source.contains('((void)0);'), 'default free did not lower to no-op'
	assert !c_source.contains('H__free('), 'default free emitted missing user method call'
}

fn test_backed_enum_map_key_uses_backing_storage_size() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'backed_enum_map_key_size',
		'enum Wide as u64 {\n\ta = 1\n\tb = 2\n}\n\nfn main() {\n\tmut m := map[Wide]int{}\n\tm[.a] = 3\n\tm[.b] = 4\n\tm.delete(.a)\n\tprintln(int_str(m[.b] or { 0 }))\n}\n')
	assert c_source.contains('new_map(sizeof(u64), sizeof(int), map_hash_int_8, map_eq_int_8'), 'backed enum map key size does not match 8-byte callbacks'
	assert c_source.contains('u64 __map_key_'), 'backed enum map key temp does not use backing storage'
	assert !c_source.contains('Wide __map_key_'), 'backed enum map key temp still uses enum typedef storage'
	assert !c_source.contains('new_map(sizeof(Wide)'), 'backed enum map allocation still uses enum typedef size'
	assert !c_source.contains('&(Wide[]){'), 'backed enum map compound key literal still uses enum typedef storage'
}

fn test_pointer_channel_try_call_derefs_receiver() {
	v3_bin := build_v3_or_review()
	c_source := or_review_gen_c(v3_bin, 'pointer_channel_try_receiver',
		'fn push(mut ch chan int) bool {\n\treturn ch.try_push(7) == .success\n}\n\nfn pop(mut ch chan int, mut out int) bool {\n\treturn ch.try_pop(mut out) == .success\n}\n\nfn main() {\n\tmut ch := chan int{cap: 1}\n\tmut out := 0\n\tprintln(push(mut ch))\n\tprintln(pop(mut ch, mut out))\n\tprintln(int_str(out))\n}\n')
	assert c_source.contains('sync__Channel__try_push(*(ch),'), 'try_push on pointer channel receiver does not dereference the channel handle'
	assert c_source.contains('sync__Channel__try_pop(*(ch),'), 'try_pop on pointer channel receiver does not dereference the channel handle'
}
