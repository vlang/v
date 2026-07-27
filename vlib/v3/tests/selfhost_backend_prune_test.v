import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const vlib_dir = os.dir(v3_dir)

fn cleanup_selfhost_prune_file(path string) {
	if !os.exists(path) && !os.is_link(path) {
		return
	}
	assert !os.is_dir(path) || os.is_link(path), 'cleanup path `${path}` is an unexpected directory'
	os.rm(path) or { assert false, 'failed to remove `${path}`: ${err.msg()}' }
	assert !os.exists(path) && !os.is_link(path), 'cleanup left `${path}` behind'
}

fn cleanup_selfhost_prune_exact(path string) {
	if !os.exists(path) && !os.is_link(path) {
		return
	}
	if os.is_dir(path) && !os.is_link(path) {
		os.rmdir_all(path) or {
			assert false, 'failed to remove exact tree `${path}`: ${err.msg()}'
		}
	} else {
		os.rm(path) or { assert false, 'failed to remove exact path `${path}`: ${err.msg()}' }
	}
	assert !os.exists(path) && !os.is_link(path), 'cleanup left `${path}` behind'
}

fn build_selfhost_prune_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_selfhost_backend_prune_boot_${os.getpid()}')
	assert !os.exists(v3_bin) && !os.is_link(v3_bin), 'compiler output `${v3_bin}` was stale'
	mut completed := false
	defer {
		if !completed {
			cleanup_selfhost_prune_file(v3_bin)
		}
	}
	build :=
		os.execute('${os.quoted_path(vexe)} -path ${os.quoted_path(vlib_dir)} -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	completed = true
	return v3_bin
}

fn selfhost_default_smoke_to_c(v3_bin string) string {
	name := 'default'
	out_bin := os.join_path(os.temp_dir(), 'v3_selfhost_backend_prune_${name}_${os.getpid()}')
	out_c := out_bin + '.c'
	out_v3cc := out_bin + '.v3cc'
	assert !os.exists(out_bin) && !os.is_link(out_bin), 'output `${out_bin}` was stale'
	assert !os.exists(out_c) && !os.is_link(out_c), 'output `${out_c}` was stale'
	assert !os.exists(out_v3cc) && !os.is_link(out_v3cc), 'output `${out_v3cc}` was stale'
	defer {
		cleanup_selfhost_prune_exact(out_v3cc)
		cleanup_selfhost_prune_file(out_c)
		cleanup_selfhost_prune_file(out_bin)
	}
	cmd := '${os.quoted_path(v3_bin)} --no-parallel -selfhost -o ${os.quoted_path(out_bin)} ${os.quoted_path(v3_src)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	assert os.is_file(out_bin) && !os.is_link(out_bin), 'missing compiled smoke `${out_bin}`'
	assert os.is_file(out_c) && !os.is_link(out_c), 'missing generated C output `${out_c}`'
	assert !os.exists(out_v3cc) && !os.is_link(out_v3cc), 'unexpected C build tree `${out_v3cc}`'
	return os.read_file(out_c) or { panic(err) }
}

fn selfhost_pruning_to_c(v3_bin string, name string, flags string) string {
	out_bin := os.join_path(os.temp_dir(), 'v3_selfhost_backend_prune_${name}_${os.getpid()}')
	out_c := out_bin + '.c'
	out_v3cc := out_bin + '.v3cc'
	assert !os.exists(out_bin) && !os.is_link(out_bin), 'output `${out_bin}` was stale'
	assert !os.exists(out_c) && !os.is_link(out_c), 'output `${out_c}` was stale'
	assert !os.exists(out_v3cc) && !os.is_link(out_v3cc), 'output `${out_v3cc}` was stale'
	defer {
		cleanup_selfhost_prune_exact(out_v3cc)
		cleanup_selfhost_prune_file(out_c)
		cleanup_selfhost_prune_file(out_bin)
	}
	cmd := '${os.quoted_path(v3_bin)} --no-parallel -selfhost ${flags} -o ${os.quoted_path(out_c)} ${os.quoted_path(v3_src)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	assert os.is_file(out_c) && !os.is_link(out_c), 'missing generated C output `${out_c}`'
	assert !os.exists(out_bin) && !os.is_link(out_bin), 'C-only pruning produced `${out_bin}`'
	assert !os.exists(out_v3cc) && !os.is_link(out_v3cc), 'C-only pruning produced `${out_v3cc}`'
	return os.read_file(out_c) or { panic(err) }
}

fn c_source_has_identifier_prefix(c_src string, prefix string) bool {
	if prefix.len == 0 {
		return false
	}
	mut i := 0
	for i + prefix.len <= c_src.len {
		if c_src[i..i + prefix.len] == prefix {
			if i == 0 {
				return true
			}
			previous := c_src[i - 1]
			if !((previous >= `a` && previous <= `z`)
				|| (previous >= `A` && previous <= `Z`)
				|| (previous >= `0` && previous <= `9`) || previous == `_`) {
				return true
			}
		}
		i++
	}
	return false
}

fn assert_selfhost_backend_set(c_src string, want_amd64 bool, want_arm64 bool, want_wasm bool, want_eval bool) {
	if want_amd64 {
		assert c_src.contains('amd64__Gen__new'), 'missing AMD64 constructor marker'
	} else {
		assert !c_source_has_identifier_prefix(c_src, 'amd64__'), 'AMD64 module prefix survived pruning'
	}
	if want_arm64 {
		assert c_src.contains('arm64__Gen__new'), 'missing ARM64 constructor marker'
	} else {
		assert !c_source_has_identifier_prefix(c_src, 'arm64__'), 'ARM64 module prefix survived pruning'
	}
	if want_wasm {
		assert c_src.contains('v3__gen__wasm__Gen__new'), 'missing WASM constructor marker'
	} else {
		assert !c_source_has_identifier_prefix(c_src, 'v3__gen__wasm__'), 'WASM module prefix survived pruning'
	}
	if want_eval {
		assert c_src.contains('eval__new'), 'missing eval constructor marker'
	} else {
		assert !c_source_has_identifier_prefix(c_src, 'eval__'), 'eval module prefix survived pruning'
	}
	want_ssa := want_amd64 || want_arm64
	if want_ssa {
		assert c_src.contains('ssa__build_with_used'), 'missing SSA builder marker'
		assert c_src.contains('optimize__optimize'), 'missing SSA optimizer marker'
	} else {
		assert !c_source_has_identifier_prefix(c_src, 'ssa__'), 'SSA module prefix survived pruning'
		assert !c_source_has_identifier_prefix(c_src, 'optimize__'), 'SSA optimizer prefix survived pruning'
	}
}

fn test_selfhost_default_prunes_optional_backends() {
	v3_bin := build_selfhost_prune_v3()
	defer {
		cleanup_selfhost_prune_file(v3_bin)
	}
	c_src := selfhost_default_smoke_to_c(v3_bin)
	assert_selfhost_backend_set(c_src, false, false, false, false)
	assert c_src.contains('bool lhs_is_arr = false;'), 'array equality fallback should use an explicit presence flag'
	assert c_src.contains('bool lhs_is_fixed = false;'), 'fixed-array equality fallback should use an explicit presence flag'
	assert !c_src.contains('lhs_arr.elem_type; __sum.typ'), 'array equality fallback used zero-value sum-type sentinel'
	assert !c_src.contains('lhs_fixed.elem_type; __sum.typ'), 'fixed-array equality fallback used zero-value sum-type sentinel'
}

fn test_selfhost_compile_backend_amd64_opts_in_only_amd64_and_ssa() {
	v3_bin := build_selfhost_prune_v3()
	defer {
		cleanup_selfhost_prune_file(v3_bin)
	}
	c_src := selfhost_pruning_to_c(v3_bin, 'amd64', '-compile-backend amd64')
	assert_selfhost_backend_set(c_src, true, false, false, false)
}

fn test_selfhost_compile_backend_arm64_opts_in_only_arm64_and_ssa() {
	v3_bin := build_selfhost_prune_v3()
	defer {
		cleanup_selfhost_prune_file(v3_bin)
	}
	c_src := selfhost_pruning_to_c(v3_bin, 'arm64', '-compile-backend arm64')
	assert_selfhost_backend_set(c_src, false, true, false, false)
}

fn test_selfhost_all_backends_retains_every_optional_backend() {
	v3_bin := build_selfhost_prune_v3()
	defer {
		cleanup_selfhost_prune_file(v3_bin)
	}
	c_src := selfhost_pruning_to_c(v3_bin, 'all', '-all-backends')
	assert_selfhost_backend_set(c_src, true, true, true, true)
}

fn test_selfhost_compile_backend_wasm_opts_wasm_back_in() {
	if os.getenv('V3_TEST_WASM') != '1' {
		eprintln('> skipping v3 wasm self-host opt-in check; set V3_TEST_WASM=1 to run')
		return
	}
	v3_bin := build_selfhost_prune_v3()
	defer {
		cleanup_selfhost_prune_file(v3_bin)
	}
	c_src := selfhost_pruning_to_c(v3_bin, 'wasm', '-compile-backend wasm')
	assert_selfhost_backend_set(c_src, false, false, true, false)
}
