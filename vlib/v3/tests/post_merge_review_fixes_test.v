import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const repo_dir = os.dir(vlib_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const compiler_src_dir = os.join_path(repo_dir, 'cmd', 'v')

fn tmp_test_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn build_v3() string {
	v3_bin := tmp_test_path('post_merge_review_fixes_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_good(v3_bin string, name string, src string) string {
	good_src := '${tmp_test_path(name)}.v'
	os.write_file(good_src, src) or { panic(err) }
	good_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := '${tmp_test_path(name)}.v'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn gen_c(v3_bin string, name string, src string) string {
	src_path := '${tmp_test_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	c_path := '${tmp_test_path(name)}.c'
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output
	assert os.exists(c_path)
	return os.read_file(c_path) or { panic(err) }
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := '${tmp_test_path(name)}_project'
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_compiler_vexe_env_uses_running_executable() {
	v3_bin := build_v3()
	c_out := os.join_path(os.temp_dir(), 'v3_review_vexe.c')
	os.rm(c_out) or {}
	gen := os.execute('${v3_bin} -o ${c_out} ${compiler_src_dir}')
	assert gen.exit_code == 0, gen.output
	assert os.exists(c_out)
	c_source := os.read_file(c_out) or { panic(err) }
	assert !c_source.contains('v3_vexe_target')
	assert !c_source.contains('fopen(v3_src')
	assert c_source.contains('snprintf(v3_checkout_vexe, sizeof(v3_checkout_vexe),')
	assert c_source.contains('if (access(v3_checkout_vexe, F_OK) == 0) v3_vexe = v3_checkout_vexe;')
	assert c_source.contains('const char* v3_vexe = v3_src_real_result != NULL ? v3_src_real : v3_arg0;')
	assert c_source.contains('_putenv_s("VEXE", v3_vexe);')
	assert c_source.contains('setenv("VEXE", v3_vexe, 1);')
}

fn test_filelock_helpers_are_inlined_in_generated_c() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'filelock_helpers_inline',
		'import os.filelock\n\nfn C.v_filelock_lock(i32, i32, i32, u64, u64) i32\nfn C.v_filelock_unlock(i32, u64, u64) i32\n\nfn main() {\n\t_ = filelock.LockMode.exclusive\n\t_ = C.v_filelock_lock(i32(-1), 1, 1, u64(0), u64(0))\n\t_ = C.v_filelock_unlock(i32(-1), u64(0), u64(0))\n}\n')
	assert !c_source.contains('filelock_helpers.h')
	assert c_source.contains('static int v_filelock_lock(')
	assert c_source.contains('static int v_filelock_unlock(')
	assert c_source.contains('#ifndef V_OS_FILELOCK_HELPERS_H')
	assert !c_source.contains('v_filelock_status')
	status_source := gen_c(v3_bin, 'filelock_custom_prefix_decl',
		'import os.filelock\n\nfn C.v_filelock_lock(i32, i32, i32, u64, u64) i32\nfn C.v_filelock_unlock(i32, u64, u64) i32\nfn C.v_filelock_status() int\n\nfn main() {\n\t_ = filelock.LockMode.exclusive\n\t_ = C.v_filelock_lock(i32(-1), 1, 1, u64(0), u64(0))\n\t_ = C.v_filelock_status()\n}\n')
	assert status_source.contains('int v_filelock_status(')
	user_name_source := gen_c(v3_bin, 'filelock_user_names_not_helpers',
		'fn v_filelock_lock() int {\n\treturn 3\n}\n\nfn v_filelock_unlock() int {\n\treturn 4\n}\n\nfn main() {\n\tprintln(int_str(v_filelock_lock() + v_filelock_unlock()))\n}\n')
	assert !user_name_source.contains('static int v_filelock_lock(')
	assert !user_name_source.contains('static int v_filelock_unlock(')
}

fn test_assoc_return_runs_defers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'assoc_return_runs_defers',
		'struct Point {\n\tx int\n\ty int\n}\n\n__global hit int\n\nfn make_point() Point {\n\tbase := Point{\n\t\tx: 1\n\t\ty: 2\n\t}\n\tdefer {\n\t\thit = 7\n\t}\n\treturn Point{\n\t\t...base\n\t\tx: 5\n\t}\n}\n\nfn main() {\n\tp := make_point()\n\tprintln(int_str(p.x))\n\tprintln(int_str(hit))\n}\n')
	assert out == '5\n7'
}

fn test_pointer_arithmetic_deref_keeps_pointer_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'pointer_arithmetic_deref',
		'fn main() {\n\tmut nums := [1, 2]!\n\tp := unsafe { &nums[0] }\n\tv := unsafe { *(p + 1) }\n\tprintln(int_str(v))\n}\n')
	assert out == '2'
}

fn test_array_alias_free_uses_array_builtin_inside_alias_method() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'array_alias_free_builtin',
		'import strings\n\nfn main() {\n\tmut b := strings.new_builder(4)\n\tb.write_string("ok")\n\tunsafe { b.free() }\n\tprintln("ok")\n}\n')
	assert out == 'ok'
}

fn test_channel_alias_close_method_wins_over_builtin() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'channel_alias_close_method_before_builtin',
		'type MyChan = chan int\n\nfn (c MyChan) close() int {\n\treturn 71\n}\n\nfn main() {\n\tch := MyChan(unsafe { nil })\n\tprintln(int_str(ch.close()))\n}\n')
	assert out == '71'
	pointer_c := gen_c(v3_bin, 'pointer_channel_close_lowers_to_runtime',
		'fn main() {\n\tmut ch := chan bool{cap: 1}\n\tp := &ch\n\tp.close()\n}\n')
	assert pointer_c.contains('sync__Channel__close(*p,')
}

fn test_qualified_enum_str_requires_exact_receiver() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'qualified_enum_str_exact_receiver', {
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tprintln(moda.Color.red.str())\n\tprintln(modb.Color.blue.str())\n}\n'
		'moda/moda.v': 'module moda\n\npub enum Color {\n\tred\n}\n'
		'modb/modb.v': "module modb\n\npub enum Color {\n\tblue\n}\n\npub fn (c Color) str() string {\n\treturn 'custom'\n}\n"
	}, 'main.v')
	assert out == 'red\ncustom'
}

fn test_array_builtin_method_fallback_keeps_return_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'array_builtin_method_fallback',
		'fn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tnums << 2\n\tnums << 3\n\tptrs := unsafe { nums.pointers() }\n\tprintln(int_str(ptrs.len))\n}\n')
	assert out == '3'
	ptr_out := run_good(v3_bin, 'array_pointers_pointer_receiver',
		'fn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tp := &nums\n\tptrs := unsafe { p.pointers() }\n\tprintln(int_str(ptrs.len))\n}\n')
	assert ptr_out == '1'
	reverse_out := run_good(v3_bin, 'array_reverse_pointer_receiver',
		'fn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tnums << 2\n\tp := &nums\n\tp.reverse()\n\tprintln("ok")\n}\n')
	assert reverse_out == 'ok'
	exact_out := run_good(v3_bin, 'exact_array_receiver_method_before_builtin',
		'fn (a []int) pointers() []int {\n\treturn a\n}\n\nfn main() {\n\tnums := [9]\n\tptrs := nums.pointers()\n\tprintln(int_str(ptrs[0]))\n}\n')
	assert exact_out == '9'
	exact_clear_out := run_good(v3_bin, 'exact_array_clear_method_before_cgen',
		'fn (a []int) clear() int {\n\treturn 5\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.clear()))\n\tprintln(int_str(nums.len))\n}\n')
	assert exact_clear_out == '5\n1'
	exact_clone_out := run_good(v3_bin, 'exact_array_clone_method_before_builtin',
		'fn (a []int) clone() int {\n\treturn 12\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.clone()))\n}\n')
	assert exact_clone_out == '12'
	exact_reverse_out := run_good(v3_bin, 'exact_array_reverse_method_before_builtin',
		'fn (a []int) reverse() int {\n\treturn 13\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.reverse()))\n}\n')
	assert exact_reverse_out == '13'
	module_array_prefix_out := run_good_project(v3_bin, 'array_prefix_module_receiver_method', {
		'main.v':            'module main\n\nimport array_utils\n\nfn main() {\n\tprintln(array_utils.run())\n}\n'
		'array_utils/mod.v': 'module array_utils\n\nfn (a []int) reverse() int {\n\treturn 73\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.reverse())\n}\n'
	}, 'main.v')
	assert module_array_prefix_out == '73'
	module_array_runtime_prefix_out := run_good_project(v3_bin,
		'array_runtime_prefix_module_receiver_method', {
		'main.v':             'module main\n\nimport array__utils\n\nfn main() {\n\tprintln(array__utils.run())\n}\n'
		'array__utils/mod.v': 'module array__utils\n\nfn (a []int) reverse() int {\n\treturn 83\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.reverse())\n}\n'
	}, 'main.v')
	assert module_array_runtime_prefix_out == '83'
	module_array_move_out := run_good_project(v3_bin, 'module_array_move_receiver_method', {
		'main.v':      'module main\n\nimport thing\n\nfn main() {\n\tprintln(thing.run())\n}\n'
		'thing/mod.v': 'module thing\n\nfn (a []int) move() int {\n\treturn 91\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.move())\n}\n'
	}, 'main.v')
	assert module_array_move_out == '91'
	exact_prepend_out := run_good(v3_bin, 'exact_array_prepend_method_before_builtin',
		'fn (a []int) prepend(x int) int {\n\treturn x + 1\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.prepend(4)))\n}\n')
	assert exact_prepend_out == '5'
	run_bad(v3_bin, 'exact_array_first_method_checked_before_builtin',
		'fn (a []int) first() string {\n\treturn "bad"\n}\n\nfn take_int(x int) {}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\ttake_int(nums.first())\n}\n',
		'cannot use `string` as argument 1 to `take_int`; expected `int`')
	fixed_dynamic_out := run_good(v3_bin, 'fixed_array_dynamic_receiver_method_before_builtin',
		'fn (a []int) pointers() int {\n\treturn 41\n}\n\nfn main() {\n\tfixed := [3]int{}\n\tprintln(int_str(fixed.pointers()))\n}\n')
	assert fixed_dynamic_out == '41'
	nested_fixed_dynamic_out := run_good(v3_bin, 'nested_fixed_array_dynamic_receiver_method',
		'fn (a [][2]int) pointers() int {\n\treturn 82\n}\n\nfn main() {\n\tfixed := [3][2]int{}\n\tprintln(int_str(fixed.pointers()))\n}\n')
	assert nested_fixed_dynamic_out == '82'
	fixed_alias_shape_out := run_good(v3_bin, 'fixed_array_builtin_not_alias_method',
		'type F = [2]int\n\nfn (f F) pointers() int {\n\treturn 66\n}\n\nfn main() {\n\tmut fixed := [2]int{}\n\tptrs := unsafe { fixed.pointers() }\n\tprintln(int_str(ptrs.len))\n}\n')
	assert fixed_alias_shape_out == '2'
	plain_array_contains_out := run_good(v3_bin, 'plain_array_contains_not_alias_method',
		'type A = []int\n\nfn (a A) contains(x int) int {\n\treturn 0\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tif nums.contains(2) {\n\t\tprintln("builtin")\n\t} else {\n\t\tprintln("alias")\n\t}\n\talias := A(nums)\n\tprintln(int_str(alias.contains(2)))\n}\n')
	assert plain_array_contains_out == 'builtin\n0'
	module_primitive_out := run_good_project(v3_bin, 'module_primitive_array_receiver_method', {
		'main.v':      'module main\n\nimport thing\n\nfn main() {\n\tprintln(thing.run())\n}\n'
		'thing/mod.v': 'module thing\n\nfn (a []int) pointers() int {\n\treturn 64\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.pointers())\n}\n'
	}, 'main.v')
	assert module_primitive_out == '64'
	fixed_out := run_good(v3_bin, 'fixed_array_pointers_original_storage',
		'fn main() {\n\tmut fixed := [3]int{}\n\tfixed[0] = 1\n\tptrs := unsafe { fixed.pointers() }\n\tunsafe {\n\t\tp0 := &int(ptrs[0])\n\t\t*p0 = 9\n\t}\n\tprintln(int_str(fixed[0]))\n}\n')
	assert fixed_out == '9'
	fixed_expr_out := run_good(v3_bin, 'fixed_array_pointers_evaluates_receiver_once',
		'__global calls int\n\nfn next() int {\n\tcalls = calls + 1\n\treturn 0\n}\n\nfn main() {\n\tmut rows := [1][2]int{}\n\trows[0][0] = 5\n\tptrs := unsafe { rows[next()].pointers() }\n\tunsafe {\n\t\tp0 := &int(ptrs[0])\n\t\t*p0 = 8\n\t}\n\tprintln(int_str(calls))\n\tprintln(int_str(rows[0][0]))\n}\n')
	assert fixed_expr_out == '1\n8'
	run_bad(v3_bin, 'fixed_array_pointers_rejects_rvalue_receiver',
		'fn make_fixed() [2]int {\n\treturn [7, 8]!\n}\n\nfn main() {\n\t_ := unsafe { make_fixed().pointers() }\n}\n',
		'fixed array receiver for `pointers` must be addressable')
	run_bad(v3_bin, 'fixed_array_pointers_rejects_map_index_receiver',
		'fn main() {\n\tmut m := map[string][2]int{}\n\tm["x"] = [1, 2]!\n\t_ := unsafe { m["x"].pointers() }\n}\n',
		'fixed array receiver for `pointers` must be addressable')
	fixed_len_expr_out := run_good(v3_bin, 'fixed_array_pointers_folds_len_expr',
		'const segs = 2\n\nfn main() {\n\tmut const_len := [segs + 1]int{}\n\tconst_ptrs := unsafe { const_len.pointers() }\n\tmut shift_len := [8 >>> 1]int{}\n\tshift_ptrs := unsafe { shift_len.pointers() }\n\tprintln(int_str(const_ptrs.len))\n\tprintln(int_str(shift_ptrs.len))\n}\n')
	assert fixed_len_expr_out == '3\n4'
	run_bad(v3_bin, 'fixed_array_pointers_rejects_extra_arg',
		'fn extra_arg() int {\n\treturn 1\n}\n\nfn main() {\n\tmut fixed := [3]int{}\n\t_ := unsafe { fixed.pointers(extra_arg()) }\n}\n',
		'argument count mismatch for `fixed.pointers`: expected 1, got 2')
}

fn test_map_builtin_method_fallback_checks_arguments() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'map_keys_rejects_extra_arg',
		'fn extra_arg() int {\n\treturn 1\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\t_ := m.keys(extra_arg())\n}\n',
		'argument count mismatch for `m.keys`: expected 0, got 1')
	run_bad(v3_bin, 'map_delete_rejects_bad_key_type',
		'fn main() {\n\tmut m := map[string]int{}\n\tm.delete(123)\n}\n',
		'cannot use `int` as argument 2 to `m.delete`; expected `string`')
	run_bad(v3_bin, 'map_reserve_rejects_bad_count_type',
		'fn main() {\n\tmut m := map[string]int{}\n\tm.reserve("bad")\n}\n',
		'cannot use `string` as argument 2 to `m.reserve`; expected `u32`')
	out := run_good(v3_bin, 'map_builtin_method_fallback',
		'fn main() {\n\tmut m := map[string]int{}\n\tm["abc"] = 42\n\tmut moved := m.move()\n\tprintln(int_str(m.len))\n\tmoved.clear()\n\tmoved.reserve(6)\n\tmoved.delete("x")\n\tkeys := moved.keys()\n\tvalues := moved.values()\n\tcloned := moved.clone()\n\tprintln(int_str(keys.len + values.len + cloned.len))\n}\n')
	assert out == '0\n0'
	pointer_out := run_good(v3_bin, 'map_move_pointer_receiver_returns_map',
		'fn take(m map[string]int) int {\n\treturn m.len\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm["abc"] = 42\n\tp := &m\n\tprintln(int_str(take(p.move())))\n\tprintln(int_str(m.len))\n}\n')
	assert pointer_out == '1\n0'
	exact_out := run_good(v3_bin, 'exact_map_receiver_method_before_builtin',
		'fn (m map[string]int) keys() int {\n\treturn 77\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\tn := m.keys()\n\tprintln(int_str(n))\n}\n')
	assert exact_out == '77'
	alias_rvalue_out := run_good(v3_bin, 'map_alias_rvalue_receiver_method_before_builtin',
		'type M = map[string]int\n\nfn (m M) delete(k string) int {\n\treturn 66\n}\n\nfn make_m() M {\n\tmut m := M(map[string]int{})\n\tm["x"] = 1\n\treturn m\n}\n\nfn main() {\n\tprintln(int_str(make_m().delete("x")))\n}\n')
	assert alias_rvalue_out == '66'
	plain_map_out := run_good(v3_bin, 'plain_map_builtin_not_alias_method',
		'type M = map[string]int\n\nfn (m M) keys() int {\n\treturn 66\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\tkeys := m.keys()\n\tprintln(int_str(keys.len))\n}\n')
	assert plain_map_out == '1'
	module_map_runtime_prefix_out := run_good_project(v3_bin,
		'map_runtime_prefix_module_receiver_method', {
		'main.v':           'module main\n\nimport map__utils\n\nfn main() {\n\tprintln(map__utils.run())\n}\n'
		'map__utils/mod.v': 'module map__utils\n\nfn (m map[string]int) keys() int {\n\treturn 84\n}\n\npub fn run() string {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\treturn int_str(m.keys())\n}\n'
	}, 'main.v')
	assert module_map_runtime_prefix_out == '84'
	module_map_out := run_good_project(v3_bin, 'map_module_receiver_method', {
		'main.v':    'module main\n\nimport map\n\nfn main() {\n\tprintln(map.run())\n}\n'
		'map/mod.v': 'module map\n\nfn (m map[string]int) keys() int {\n\treturn 85\n}\n\npub fn run() string {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\treturn int_str(m.keys())\n}\n'
	}, 'main.v')
	assert module_map_out == '85'
	fixed_key_out := run_good(v3_bin, 'fixed_array_key_map_receiver_method_before_builtin',
		'fn (m map[[2]string]int) keys() int {\n\treturn 88\n}\n\nfn main() {\n\tmut m := map[[2]string]int{}\n\tkey := ["a", "b"]!\n\tm[key] = 1\n\tprintln(int_str(m.keys()))\n}\n')
	assert fixed_key_out == '88'
	nested_fixed_key_out := run_good(v3_bin, 'nested_fixed_array_key_map_receiver_method',
		'fn (m map[[3][2]int]int) keys() int {\n\treturn 99\n}\n\nfn main() {\n\tmut m := map[[3][2]int]int{}\n\tkey := [3][2]int{}\n\tm[key] = 1\n\tprintln(int_str(m.keys()))\n}\n')
	assert nested_fixed_key_out == '99'
	module_collection_out := run_good_project(v3_bin, 'module_collection_receiver_methods', {
		'main.v':      'module main\n\nimport thing\n\nfn main() {\n\tprintln(thing.run())\n}\n'
		'thing/mod.v': 'module thing\n\nstruct Foo {}\nstruct Key {}\n\nfn (m map[string]Foo) keys() int {\n\treturn 31\n}\n\nfn (a []Foo) pointers() int {\n\treturn 42\n}\n\nfn (m map[Key]int) keys() int {\n\treturn 53\n}\n\npub fn run() string {\n\tmut m := map[string]Foo{}\n\tm["x"] = Foo{}\n\titems := [Foo{}]\n\tkeyed := map[Key]int{}\n\treturn int_str(m.keys()) + "\\n" + int_str(items.pointers()) + "\\n" + int_str(keyed.keys())\n}\n'
	}, 'main.v')
	assert module_collection_out == '31\n42\n53'
}

fn test_runtime_inits_run_before_module_init() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'runtime_inits_before_module_init', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\tprintln(int_str(moda.const_seen()))\n\tprintln(int_str(moda.global_seen()))\n}\n'
		'moda/moda.v': "module moda\n\nconst const_map = map[string]int{\n\t'const': 5\n}\n\n__global (\n\tglobal_map = map[string]int{\n\t\t'global': 7\n\t}\n\tseen_const int\n\tseen_global int\n)\n\nfn init() {\n\tseen_const = const_map['const']\n\tseen_global = global_map['global']\n}\n\npub fn const_seen() int {\n\treturn seen_const\n}\n\npub fn global_seen() int {\n\treturn seen_global\n}\n"
	}, 'main.v')
	assert out == '5\n7'
}

fn test_string_index_type_is_u8() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'string_index_type_is_u8',
		"fn main() {\n\ts := 'ABC'\n\tprintln(typeof(s[0]).name)\n\tprintln('\${s[2]}')\n}\n")
	assert out == 'u8\n67'
}

fn test_f32_map_and_fixed_array_stringification() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'f32_map_stringification',
		"fn main() {\n\tm := {\n\t\t'a': f32(1.5)\n\t}\n\tprintln(m)\n\tfixed := [f32(1.5), f32(2.25)]!\n\tmf := {\n\t\t'x': fixed\n\t}\n\tprintln(mf)\n}\n")
	assert out == "{'a': 1.5}\n{'x': [1.5, 2.25]}"
}

fn test_u8_map_stringification_is_numeric() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'u8_map_stringification',
		"fn main() {\n\tkeys := {\n\t\tu8(23): 'x'\n\t}\n\tvals := {\n\t\t'x': u8(23)\n\t}\n\tboth := {\n\t\tu8(65): u8(10)\n\t}\n\tprintln(keys)\n\tprintln(vals)\n\tprintln(both)\n}\n")
	assert out == "{23: 'x'}\n{'x': 23}\n{65: 10}"
}

fn test_map_equality_uses_semantic_value_comparison() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'map_semantic_value_equality',
		"struct Item {\n\tname string\n\tparts []string\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := {\n\t\t'x': Item{\n\t\t\tname: 'hello'.clone()\n\t\t\tparts: ['ab'.clone()]\n\t\t}\n\t}\n\tright := {\n\t\t'x': Item{\n\t\t\tname: join('he', 'llo')\n\t\t\tparts: [join('a', 'b')]\n\t\t}\n\t}\n\tarr_left := {\n\t\t'y': ['cd'.clone()]\n\t}\n\tarr_right := {\n\t\t'y': [join('c', 'd')]\n\t}\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(arr_left == arr_right)\n}\n")
	assert out == 'true\nfalse\ntrue'
}

fn test_zero_padded_interpolation_preserves_wide_integers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'wide_zero_padded_interpolation',
		"fn main() {\n\tbig := i64(5000000000)\n\tubig := u64(18446744073709551615)\n\tsmall := u64(42)\n\tprintln('\${big:012d}')\n\tprintln('\${ubig:020d}')\n\tprintln('\${small:08d}')\n}\n")
	assert out == '005000000000\n18446744073709551615\n00000042'
}

fn test_formatted_interpolation_rune_and_long_float() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'formatted_interpolation_rune_and_long_float',
		"fn main() {\n\tr := '\${rune(0x20ac):c}'\n\tprintln(int_str(r.len))\n\tprintln(int_str(int(r[0])) + ',' + int_str(int(r[1])) + ',' + int_str(int(r[2])))\n\tlong := '\${1.0:.200f}'\n\tprintln(int_str(long.len))\n\tprintln(int_str(int(long[0])) + ',' + int_str(int(long[1])) + ',' + int_str(int(long[2])) + ',' + int_str(int(long[long.len - 1])))\n}\n")
	assert out == '3\n226,130,172\n202\n49,46,48,48'
}
