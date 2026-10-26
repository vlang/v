import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v_dir = os.dir(tests_dir)
const v_src = os.join_path(v_dir, 'v.v')
const cmd_v_src = os.join_path(os.dir(os.dir(v_dir)), 'cmd', 'v')

// OptionalBackend is a backend module that compiler builds can leave out. `dir` is its
// directory under vlib/v. `symbol` is a call that the driver's dispatch for the backend emits
// whenever the backend is compiled in. It omits the module path, because cgen names a module by
// its short name (`ssa__`) unless another module has that name (`vlib/wasm`: `v__gen__wasm__`).
struct OptionalBackend {
	name   string
	dir    string
	symbol string
}

const optional_backends = [
	OptionalBackend{
		name:   'fastc'
		dir:    'gen/fastc'
		symbol: 'fastc__generate_files_with_source_paths('
	},
	OptionalBackend{
		name:   'wasm'
		dir:    'gen/wasm'
		symbol: 'wasm__SSAGen__gen('
	},
	OptionalBackend{
		name:   'arm64'
		dir:    'gen/arm64'
		symbol: 'arm64__Gen__gen('
	},
	OptionalBackend{
		name:   'ssa'
		dir:    'ssa'
		symbol: 'ssa__build_with_options('
	},
	OptionalBackend{
		name:   'ssa.optimize'
		dir:    'ssa/optimize'
		symbol: 'optimize__optimize('
	},
	OptionalBackend{
		name:   'eval'
		dir:    'eval'
		symbol: 'eval__new('
	},
]

struct CompilerC {
	c_src string
	// parsed_dirs holds the vlib/v module directories that the build parsed. A backend can be
	// parsed but pruned from the C (its dispatch gated out), which still costs compile time.
	parsed_dirs map[string]bool
}

// compiler_to_c generates the C of a compiler entry point with `-v`, which lists parsed files.
fn compiler_to_c(name string, flags []string, input string) CompilerC {
	out_c := os.join_path(os.vtmp_dir(), 'v_selfhost_backend_prune_${name}_${os.getpid()}.c')
	os.rm(out_c) or {}
	defer {
		os.rm(out_c) or {}
	}
	res := os.exec([vexe, '-v', '-nocache', '-no-memory-limit', ...flags, '-o', out_c, input])
	assert res.exit_code == 0, res.output
	mut parsed_dirs := map[string]bool{}
	for line in res.output.split_into_lines() {
		listed := line.trim_space()
		if !listed.starts_with('.v files:') {
			continue
		}
		for path in listed.all_after(':').fields() {
			dir := path.replace('\\', '/').all_before_last('/')
			if dir.contains('/vlib/v/') {
				parsed_dirs[dir.all_after_last('/vlib/v/')] = true
			}
		}
	}
	assert parsed_dirs.len > 0, '${name}: `-v` listed no parsed vlib/v files'
	return CompilerC{
		c_src:       os.read_file(out_c) or { panic(err) }
		parsed_dirs: parsed_dirs
	}
}

fn assert_backends(build CompilerC, name string, expected []string) {
	for backend in optional_backends {
		compiled := build.c_src.contains(backend.symbol)
		parsed := build.parsed_dirs[backend.dir]
		if backend.name in expected {
			assert compiled, '${name}: the ${backend.name} backend was not compiled in (no `${backend.symbol}`)'
			assert parsed, '${name}: vlib/v/${backend.dir} was not parsed'
		} else {
			assert !compiled, '${name}: the ${backend.name} backend was compiled in (`${backend.symbol}`)'
			assert !parsed, '${name}: vlib/v/${backend.dir} was parsed, but the ${backend.name} backend is skipped'
		}
	}
}

fn selfhost_to_c(name string, flags []string) CompilerC {
	return compiler_to_c(name, ['--no-parallel', '-selfhost', '-b', 'c', ...flags], v_src)
}

fn test_selfhost_default_prunes_optional_backends() {
	build := selfhost_to_c('default', [])
	assert_backends(build, 'default self-host', [])

	c_src := build.c_src
	assert c_src.contains('bool lhs_is_arr = false;'), 'array equality fallback should use an explicit presence flag'
	assert c_src.contains('bool lhs_is_fixed = false;'), 'fixed-array equality fallback should use an explicit presence flag'
	assert !c_src.contains('lhs_arr.elem_type; __sum.typ'), 'array equality fallback used zero-value sum-type sentinel'
	assert !c_src.contains('lhs_fixed.elem_type; __sum.typ'), 'fixed-array equality fallback used zero-value sum-type sentinel'
}

fn test_selfhost_compile_backend_fastc_opts_fastc_back_in() {
	build := selfhost_to_c('fastc', ['-compile-backend', 'fastc'])
	assert_backends(build, 'FastC opt-in', ['fastc'])
}

fn test_selfhost_compile_backend_wasm_opts_wasm_back_in() {
	if os.getenv('V3_TEST_WASM') != '1' {
		eprintln('> skipping wasm self-host opt-in check; set V3_TEST_WASM=1 to run')
		return
	}
	build := selfhost_to_c('wasm', ['-compile-backend', 'wasm'])
	// WebAssembly is generated through the shared SSA pipeline.
	assert_backends(build, 'wasm opt-in', ['wasm', 'ssa', 'ssa.optimize'])
}

// The `cmd/v` builds below take the path of `v self`: a parallel `-nocache` compiler build
// discovers its imports eagerly with a byte-level scan that cannot evaluate the
// `$if !skip_* ?` import gates in vlib/v/driver/driver.v.
fn test_cmd_v_keeps_wasm_and_fastc_and_prunes_the_rest() {
	build := compiler_to_c('cmd_v', [], cmd_v_src)
	assert_backends(build, 'cmd/v', ['fastc', 'wasm', 'ssa', 'ssa.optimize'])
}

fn test_cmd_v_skip_wasm_also_prunes_ssa() {
	build := compiler_to_c('cmd_v_skip_wasm', ['-d', 'skip_wasm'], cmd_v_src)
	assert_backends(build, 'cmd/v -d skip_wasm', ['fastc'])
}
