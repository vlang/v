// vtest build: !windows
// Regression test for -usecache: two C sources in one module, an interface
// crossed by is/as, a __global with an initializer, and integer formatting.
import os

const vexe = @VEXE

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

struct UsecacheProject {
	root      string
	cache_dir string
	vtmp_dir  string
}

fn new_usecache_project(name string) !UsecacheProject {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	p := UsecacheProject{
		root:      root
		cache_dir: os.join_path(root, '.cache')
		vtmp_dir:  os.join_path(root, '.vtmp')
	}
	os.mkdir_all(p.cache_dir)!
	os.mkdir_all(p.vtmp_dir)!
	return p
}

fn (p &UsecacheProject) write(relpath string, content string) ! {
	full := os.join_path(p.root, relpath)
	os.mkdir_all(os.dir(full))!
	os.write_file(full, content)!
}

// build_and_run compiles the project with -usecache into its own private cache,
// then runs the result and returns its trimmed output.
fn (p &UsecacheProject) build_and_run() !string {
	// keep this build's cache out of the user's, so a stale entry cannot decide it
	old_vcache := os.getenv_opt('VCACHE') or { '' }
	old_vtmp := os.getenv_opt('VTMP') or { '' }
	old_report := os.getenv_opt('V_C_ERROR_BUG_REPORT_DISABLED') or { '' }
	os.setenv('VCACHE', p.cache_dir, true)
	os.setenv('VTMP', p.vtmp_dir, true)
	// A C error here is a V bug to fix locally, not something to upload.
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	defer {
		restore_env('VCACHE', old_vcache)
		restore_env('VTMP', old_vtmp)
		restore_env('V_C_ERROR_BUG_REPORT_DISABLED', old_report)
	}
	exe := os.join_path(p.root, 'app')
	mut build := os.new_process(vexe)
	build.set_work_folder(p.root)
	// -old-compiler keeps this aimed at the V1 `-usecache` implementation; on macOS
	// the default compiler is V3, which ignores -usecache and caches modules itself.
	build.set_args(['-old-compiler', '-usecache', '-enable-globals', '-o', exe, '.'])
	build.set_redirect_stdio()
	build.wait()
	build_out := build.stdout_slurp() + build.stderr_slurp()
	build_code := build.code
	build.close()
	if build_code != 0 {
		return error('building with -usecache failed:\n${build_out}')
	}
	res := os.execute(os.quoted_path(exe))
	if res.exit_code != 0 {
		return error('the -usecache binary failed at runtime (exit ${res.exit_code}):\n${res.output}')
	}
	return res.output.trim_space()
}

fn restore_env(name string, old_value string) {
	if old_value.len == 0 {
		os.unsetenv(name)
	} else {
		os.setenv(name, old_value, true)
	}
}

fn test_usecache_project_with_c_interop_interfaces_and_globals() {
	p := new_usecache_project('v_usecache_c_interop')!
	defer {
		os.rmdir_all(p.root) or {}
	}
	p.write('v.mod', "Module {\n\tname: 'ucinterop'\n}\n")!

	// a module binding two separate C source files
	p.write('cbits/first.c', 'int uc_first(void) { return 7; }\n')!
	p.write('cbits/second.c', 'int uc_second(void) { return 35; }\n')!
	p.write('cbits/cbits.c.v', 'module cbits

#flag @VMODROOT/cbits/first.c
#flag @VMODROOT/cbits/second.c

fn C.uc_first() int
fn C.uc_second() int

pub fn first() int {
	return C.uc_first()
}

pub fn second() int {
	return C.uc_second()
}
')!

	// an interface crossed by a cached module: as_size reaches
	// v_typeof_interface_idx_*, is_large the interface index symbols
	p.write('shapes/shapes.v', 'module shapes

pub interface Sized {
	size() int
}

pub struct Small {}

pub fn (s Small) size() int {
	return 1
}

pub struct Large {}

pub fn (l Large) size() int {
	return 100
}

pub fn pick(large bool) Sized {
	if large {
		return Large{}
	}
	return Small{}
}

pub fn as_size(s Sized) int {
	l := s as Large
	return l.size()
}

pub fn is_large(s Sized) bool {
	return s is Large
}
')!

	// a global with an initializer: only the program emits the `_vinit` that assigns it
	p.write('registry/registry.v', 'module registry

__global uc_counter = new_counter()

pub struct Counter {
pub mut:
	n int
}

fn new_counter() &Counter {
	return &Counter{
		n: 42
	}
}

pub fn value() int {
	return uc_counter.n
}
')!

	p.write('main.v', "module main

import cbits
import shapes
import registry

fn main() {
	// integer formatting reads builtin's `digit_pairs` const
	println(cbits.first() + cbits.second())
	println(shapes.pick(false).size() + shapes.pick(true).size())
	println(shapes.as_size(shapes.pick(true)))
	println(shapes.is_large(shapes.pick(true)))
	println(registry.value())
}
")!

	output := p.build_and_run()!
	lines := output.split_into_lines().map(it.trim_space())
	assert lines.len == 5, output
	assert lines[0] == '42', output
	assert lines[1] == '101', output
	assert lines[2] == '100', output
	assert lines[3] == 'true', output
	assert lines[4] == '42', output
	assert os.walk_ext(p.cache_dir, '.o').len > 0, 'no module objects were cached'
}
