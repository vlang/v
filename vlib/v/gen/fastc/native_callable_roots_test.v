module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_internal_startup_hooks_require_actual_calls() {
	marked := FastcFunctionSignature{ module_name: 'builtin', is_markused: true }
	assert !fastc_function_is_native_root('builtin.builtin_init', marked)
	assert !fastc_function_is_native_root('builtin.gc_runtime_init', marked)
	assert fastc_function_is_native_root('builtin.SomeType.builtin_init', marked)
	assert fastc_function_is_native_root('builtin.SomeType.gc_runtime_init', marked)
	assert fastc_function_is_native_root('app.gc_runtime_init', FastcFunctionSignature{
		module_name: 'app'
		is_markused: true
	})
	prefs := pref.new_preferences()
	sources := [FastcSourceFile{
		path:   'builtin_startup.v'
		source: 'module builtin\n@[markused]\nfn gc_runtime_init() { startup_dependency() }\nfn startup_dependency() {}\n'
		header: FastcSourceHeader{ module_name: 'builtin' }
	}, FastcSourceFile{
		path:   'main.v'
		source: 'module main\nfn main() { gc_runtime_init() }\n'
		header: FastcSourceHeader{ module_name: 'main' }
	}]
	functions := {
		'builtin.gc_runtime_init':    marked
		'builtin.startup_dependency': FastcFunctionSignature{ module_name: 'builtin' }
		'main':                       FastcFunctionSignature{ module_name: 'main' }
	}
	used := fastc_collect_referenced_function_names(sources, prefs, functions)
	assert used['gc_runtime_init']
	assert used['startup_dependency']
}

fn test_fastc_c_variadic_annotations_keep_only_fixed_parameters() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_c_variadic_binding_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'bindings.c.v')
	source := r'module main
fn C.typed_hook(fmt charptr, ...voidptr) i32
fn C.named_hook(fmt charptr, args ...voidptr) i32
fn C.bare_hook(charptr, ...) i32
'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target_libc_headers = true
	mut functions := map[string]FastcFunctionSignature{}
	collect_function_signatures(source, path, FastcSourceHeader{ module_name: 'main' }, prefs,
		[]int{}, map[string]bool{}, flat.TypeTextBindings{}, map[string]bool{}, mut functions) or {
		panic(err)
	}
	mut used := map[string]bool{}
	for name in ['typed_hook', 'named_hook', 'bare_hook'] {
		signature := functions['C.${name}']
		assert signature.is_variadic
		assert signature.parameter_types_text() == ['charptr']
		used[name] = true
	}
	sources := [FastcSourceFile{ path: path, source: source },
		FastcSourceFile{ path: os.join_path(root, 'main.v'), source: 'module main\nfn main() { C.typed_hook(c"", 7, u64(35)); C.named_hook(c"", 7, u64(35)); C.bare_hook(c"", 7, u64(35)) }\n' }]
	prototypes := fastc_target_c_extern_prototypes(functions, sources, used, prefs)
	assert !prototypes.contains('Array_voidptr'), prototypes
	program_c := os.join_path(root, 'program.c')
	os.write_file(program_c, fastc_target_libc_preamble() + '\n#include <stdarg.h>\n' + prototypes + r'
#define DEFINE_HOOK(name) \
int32_t name(char *fmt, ...) { \
	va_list args; va_start(args, fmt); \
	int left = va_arg(args, int); uint64_t right = va_arg(args, uint64_t); \
	va_end(args); return left + right; \
}
DEFINE_HOOK(typed_hook)
DEFINE_HOOK(named_hook)
DEFINE_HOOK(bare_hook)
int main(void) {
	return !(typed_hook("", 7, (u64)35) == 42 && named_hook("", 7, (u64)35) == 42
		&& bare_hook("", 7, (u64)35) == 42);
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
		'-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_exported_entry_retains_transitive_private_helpers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_exported_roots_${os.getpid()}')
	helper_dir := os.join_path(root, 'helper')
	os.mkdir_all(helper_dir) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import helper
@[export: "native_api"]
pub fn entry() i32 { return helper.step() }
fn main() {}
') or { panic(err) }
	os.write_file(os.join_path(helper_dir, 'helper.v'), r'module helper
fn deep() i32 { return 41 }
pub fn step() i32 { return deep() + 1 }
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	program_c := os.join_path(root, 'program.c')
	native_c := os.join_path(root, 'native.c')
	os.write_file(program_c, c_source) or { panic(err) }
	os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
extern int32_t native_api(void);
__attribute__((constructor)) static void verify_native_entry(void) {
	if (native_api() != 42) abort();
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
		native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_markused_entry_retains_native_symbol_and_private_callees() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_markused_roots_${os.getpid()}')
	helper_dir := os.join_path(root, 'helper')
	os.mkdir_all(helper_dir) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport helper\nfn main() {}\n') or { panic(err) }
	os.write_file(os.join_path(helper_dir, 'helper.v'), r'module helper
fn deep() i32 { return 41 }
@[markused]
fn retained() i32 { return deep() + 1 }
fn unused() i32 { return 99 }
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	program_c := os.join_path(root, 'program.c')
	native_c := os.join_path(root, 'native.c')
	os.write_file(program_c, c_source) or { panic(err) }
	os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
extern int32_t helper__retained(void);
__attribute__((constructor)) static void native_caller(void) {
	if (helper__retained() != 42) abort();
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
		native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
