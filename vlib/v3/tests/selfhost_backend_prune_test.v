import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_selfhost_prune_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_selfhost_backend_prune_boot_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn selfhost_to_c(v3_bin string, name string, flags string) string {
	out_bin := os.join_path(os.temp_dir(), 'v3_selfhost_backend_prune_${name}_${os.getpid()}')
	out_c := out_bin + '.c'
	os.rm(out_bin) or {}
	os.rm(out_c) or {}
	cmd := '${os.quoted_path(v3_bin)} --no-parallel -selfhost ${flags} -o ${os.quoted_path(out_bin)} ${os.quoted_path(v3_src)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	assert os.exists(out_c), 'missing generated C output ${out_c}'
	return os.read_file(out_c) or { panic(err) }
}

fn test_selfhost_default_prunes_optional_backends() {
	v3_bin := build_selfhost_prune_v3()
	c_src := selfhost_to_c(v3_bin, 'default', '')
	assert !c_src.contains('v3__gen__wasm__'), 'default self-host compiled the wasm backend'
	assert !c_src.contains('v3__gen__arm64__'), 'default self-host compiled the arm64 backend'
	assert !c_src.contains('v3__ssa__'), 'default self-host compiled the SSA backend'
	assert !c_src.contains('v3__eval__'), 'default self-host compiled the eval backend'
	assert c_src.contains('bool lhs_is_arr = false;'), 'array equality fallback should use an explicit presence flag'
	assert c_src.contains('bool lhs_is_fixed = false;'), 'fixed-array equality fallback should use an explicit presence flag'
	assert !c_src.contains('lhs_arr.elem_type; __sum.typ'), 'array equality fallback used zero-value sum-type sentinel'
	assert !c_src.contains('lhs_fixed.elem_type; __sum.typ'), 'fixed-array equality fallback used zero-value sum-type sentinel'
}

fn test_selfhost_compile_backend_wasm_opts_wasm_back_in() {
	if os.getenv('V3_TEST_WASM') != '1' {
		eprintln('> skipping v3 wasm self-host opt-in check; set V3_TEST_WASM=1 to run')
		return
	}
	v3_bin := build_selfhost_prune_v3()
	c_src := selfhost_to_c(v3_bin, 'wasm', '-compile-backend wasm')
	assert c_src.contains('v3__gen__wasm__Gen__new'), 'wasm backend was not compiled after opt-in'
	assert !c_src.contains('v3__gen__arm64__'), 'wasm opt-in compiled the arm64 backend'
	assert !c_src.contains('v3__ssa__'), 'wasm opt-in compiled the SSA backend'
	assert !c_src.contains('v3__eval__'), 'wasm opt-in compiled the eval backend'
}
