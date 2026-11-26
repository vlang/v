import os
import v.cmdexec
import v.tempname

const scoped_monomorph_tests_dir = os.dir(@FILE)
const scoped_monomorph_v3_dir = os.dir(scoped_monomorph_tests_dir)
const scoped_monomorph_vlib_dir = os.dir(scoped_monomorph_v3_dir)
const scoped_monomorph_v3_src = os.join_path(scoped_monomorph_v3_dir, 'v.v')

$if windows {
	const scoped_monomorph_bin_suffix = '.exe'
} $else {
	const scoped_monomorph_bin_suffix = ''
}

fn scoped_monomorph_cc() string {
	// TinyCC-built compilers disable parallel specialization even when opted in.
	for cc in ['clang', 'gcc'] {
		if path := os.find_abs_path_of_executable(cc) {
			return path
		}
	}
	panic('the scoped monomorphize regression needs clang or gcc')
}

fn scoped_monomorph_compiler(dir string) string {
	// The runner may already have a production compiler that supports scoped specialization.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	if os.getenv('V3_TEST_PROD_COMPILER') == '1' && os.is_file(shared_v3) && os.is_executable(shared_v3)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(scoped_monomorph_v3_src) {
		return os.real_path(shared_v3)
	}
	bin := os.join_path(dir, 'compiler' + scoped_monomorph_bin_suffix)
	// `-prealloc` enables memory-bounded specialization. Use Clang or GCC so the
	// compiler can enter the parallel specializer on targets where TinyCC cannot.
	build := os.exec([@VEXE, '-gc', 'none', '-cc', scoped_monomorph_cc(), '-prealloc', '-path',
		'${scoped_monomorph_vlib_dir}' + '|@vlib|@vmodules', '-o', bin, scoped_monomorph_v3_src])
	assert build.exit_code == 0, build.output
	return bin
}

// Compiler builds use `-prealloc`, and the memory-bounded monomorphize path (the
// fix for vlang/v#28564) runs for every non-empty specialization batch there.
// That path used to give two different lifted closures the same `__anon_fn_N`
// name - the module-keyed signature table then mixed their signatures up and the
// generated C did not compile - and merged specialization arguments as shallow
// `[]string` copies that still pointed into the released worker arena, so a later
// pass read freed arguments (bogus `unknown function` diagnostics or a crash).
// This is the small `veb` program from vlang/v#28489, which exercises closures
// lifted while specializing a generic helper.
fn test_scoped_monomorphize_keeps_closure_signatures_and_args() {
	$if linux && arm64 {
		// The scoped specializer is deliberately disabled on this target.
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v3_scoped_monomorphize_closure_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	v3_bin := scoped_monomorph_compiler(dir)
	os.write_file(os.join_path(dir, 'main.v'), "module main

import veb

pub struct Ctx {
	veb.Context
}

pub struct ModelApp {
	veb.Middleware[Ctx]
	veb.Controller
}

pub struct Item {
	ModelApp
}

pub struct MainApp {
	veb.Middleware[Ctx]
	veb.Controller
}

fn mw() veb.MiddlewareOptions[Ctx] {
	return veb.MiddlewareOptions[Ctx]{
		handler: fn (mut ctx Ctx) bool {
			return true
		}
	}
}

fn (mut app MainApp) common_middleware[T](mut ctrl T) {
	ctrl.use(mw())
}

fn (mut app MainApp) register_routes_no_auth[T, U](mut ctrl T, url_path string) {
	app.common_middleware[T](mut ctrl)
	app.register_controller[T, U](url_path, mut ctrl) or { panic(err) }
	ctrl.route_use('/item/*', veb.encode_auto[Ctx]())
}

fn main() {
	mut app := &MainApp{}
	app.register_routes_no_auth[Item, Ctx](mut &Item{}, '/item')
	veb.run_at[MainApp, Ctx](mut app, port: 9001) or { panic(err) }
}
") or { panic(err) }
	out := os.join_path(dir, 'app${scoped_monomorph_bin_suffix}')
	// Run V3 directly without C-compiler retries, so a failed scoped merge
	// cannot be hidden by a successful retry with another compiler.
	compile := cmdexec.run_in_merged_with_env(v3_bin, ['-no-retry-compilation', '-gc', 'none',
		'-cc', scoped_monomorph_cc(), '-nocache', '-o', out, dir], '', {
		'V3_PARALLEL_MONOMORPHIZE': '1'
	})
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert os.is_file(out), 'the compile produced no binary'
}
