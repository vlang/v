// vtest build: !self_sandboxed_packaging? && !sanitized_job?
import os
import time
import term
import v.util.diff
import v.util.vtest

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const local_tdata_path = 'vlib/v/gen/c/testdata'

const testdata_folder = os.real_path(os.join_path(vroot, local_tdata_path))

const show_compilation_output = os.getenv('VTEST_SHOW_COMPILATION_OUTPUT').int() == 1

const user_os = os.user_os()

const gcc_path = os.find_abs_path_of_executable('gcc') or { '' }

fn mm(s string) string {
	return term.colorize(term.magenta, s)
}

fn mj(input ...string) string {
	return mm(input.filter(it.len > 0).join(' '))
}

fn test_out_files() {
	os.chdir(vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'coutput_outs')
	os.mkdir_all(output_path)!
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.out'))
	if tests.len == 0 {
		eprintln('no `.out` tests found in ${testdata_folder}')
		return
	}
	mut total_errors := 0
	mut total_oks := 0
	mut total_oks_panic := 0
	mut total_skips := 0
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder).sorted()
	println(term.colorize(term.green,
		'> testing whether ${paths.len} .out files in ${local_tdata_path} match:'))
	for out_path in paths {
		basename, path, relpath, out_relpath := target2paths(out_path, '.out')
		if should_skip(relpath) {
			total_skips++
			continue
		}
		pexe := os.join_path(output_path, '${basename}.exe')
		//
		file_options := get_file_options(path)
		alloptions := '-o ${os.quoted_path(pexe)} ${file_options.vflags}'
		label := mj('v', file_options.vflags, 'run', relpath) + ' == ${mm(out_relpath)} '
		//
		compile_cmd := '${os.quoted_path(vexe)} ${alloptions} ${os.quoted_path(path)}'
		sw_compile := time.new_stopwatch()
		compilation := os.execute(compile_cmd)
		compile_ms := sw_compile.elapsed().milliseconds()
		ensure_compilation_succeeded(compilation, compile_cmd)
		//
		sw_run := time.new_stopwatch()
		res := os.execute(os.quoted_path(pexe))
		run_ms := sw_run.elapsed().milliseconds()
		//
		if res.exit_code < 0 {
			println('nope')
			panic(res.output)
		}
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		mut expected := os.read_file(out_path)!
		expected = expected.trim_right('\r\n').replace('\r\n', '\n')
		if expected.contains('================ V panic ================') {
			// panic include backtraces and absolute file paths, so can't do char by char comparison
			n_found := normalize_panic_message(found, vroot)
			n_expected := normalize_panic_message(expected, vroot)
			if found.contains('================ V panic ================') {
				if n_found.starts_with(n_expected) {
					vprintln('${term.green('OK (panic)')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
					total_oks_panic++
					continue
				} else {
					// Both have panics, but there was a difference...
					// Pass the normalized strings for further reporting.
					// There is no point in comparing the backtraces too.
					found = n_found
					expected = n_expected
				}
			}
		}
		if expected != found {
			println('${term.red('FAIL')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
			if diff_ := diff.compare_text(expected, found) {
				println(term.header('difference:', '-'))
				println(diff_)
			} else {
				println(term.header('expected:', '-'))
				println(expected)
				println(term.header('found:', '-'))
				println(found)
			}
			println(term.h_divider('-'))
			total_errors++
		} else {
			vprintln('${term.green('OK  ')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
			total_oks++
		}
	}
	println('>>> Summary for test_out_files: files: ${paths.len}, oks: ${total_oks}, ok panics: ${total_oks_panic}, skipped: ${total_skips}, error: ${total_errors} .')
	assert total_errors == 0
}

fn test_c_must_have_files() {
	os.chdir(vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'coutput_c_must_haves')
	os.mkdir_all(output_path)!
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.c.must_have'))
	if tests.len == 0 {
		eprintln('no `.c.must_have` files found in ${testdata_folder}')
		return
	}
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder).sorted()
	mut total_errors := 0
	mut total_oks := 0
	mut total_oks_panic := 0
	mut total_skips := 0
	mut failed_descriptions := []string{cap: paths.len}
	println(term.colorize(term.green,
		'> testing whether all line patterns in ${paths.len} `.c.must_have` files in ${local_tdata_path} match:'))
	for must_have_path in paths {
		basename, path, relpath, must_have_relpath := target2paths(must_have_path, '.c.must_have')
		if should_skip(relpath) {
			total_skips++
			continue
		}
		file_options := get_file_options(path)
		alloptions := '-o - ${file_options.vflags}'
		mut description := mj('v', alloptions, relpath) + ' matches ${mm(must_have_relpath)} '
		cmd := '${os.quoted_path(vexe)} ${alloptions} ${os.quoted_path(path)}'
		sw_compile := time.new_stopwatch()
		compilation := os.execute(cmd)
		compile_ms := sw_compile.elapsed().milliseconds()
		ensure_compilation_succeeded(compilation, cmd)
		expected_lines := os.read_lines(must_have_path) or { [] }
		generated_c_lines := compilation.output.split_into_lines()
		mut nmatches := 0
		mut failed_patterns := []string{}
		for idx_expected_line, eline in expected_lines {
			if does_line_match_one_of_generated_lines(eline, generated_c_lines) {
				nmatches++
				// eprintln('> testing: ${must_have_path} has line: ${eline}')
			} else {
				failed_patterns << eline
				description += '\n failed pattern: `${eline}`'
				println('${term.red('FAIL')} C:${compile_ms:5}ms ${description}')
				eprintln('${must_have_path}:${idx_expected_line + 1}: expected match error:')
				eprintln('`${cmd}` did NOT produce expected line:')
				eprintln(term.colorize(term.red, eline))
				if description !in failed_descriptions {
					failed_descriptions << description
				}
				total_errors++
				continue
			}
		}
		if nmatches == expected_lines.len {
			vprintln('${term.green('OK  ')} C:${compile_ms:5}ms ${description}')
			total_oks++
		} else {
			if show_compilation_output {
				eprintln('> ALL lines:')
				eprintln(compilation.output)
			}
			eprintln('--------- failed patterns: -------------------------------------------')
			for fpattern in failed_patterns {
				eprintln(fpattern)
			}
			eprintln('----------------------------------------------------------------------')
		}
	}
	if failed_descriptions.len > 0 {
		eprintln('--------- failed commands: -------------------------------------------')
		for fd in failed_descriptions {
			eprintln('  > ${fd}')
		}
		eprintln('----------------------------------------------------------------------')
	}
	println('>>> Summary for test_c_must_have_files: files: ${paths.len}, oks: ${total_oks}, ok panics: ${total_oks_panic}, skipped: ${total_skips}, error: ${total_errors} .')
	assert total_errors == 0
}

fn test_or_block_err_var_collision_does_not_emit_self_referential_err() {
	os.chdir(vroot) or {}
	path := os.join_path(testdata_folder, 'or_block_err_var_collision.vv')
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(path)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	assert !compilation.output.contains('IError err = err.err;')
	mut source_err_tmp := ''
	mut has_visible_or_block_err := false
	for line in compilation.output.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('IError _t') && trimmed.ends_with(' = err.err;') {
			source_err_tmp = trimmed.all_after('IError ').all_before(' = err.err;')
		}
		if trimmed.starts_with('IError _t') && trimmed.contains('.err;') {
			err_tmp := trimmed.all_after('IError ').all_before(' = ')
			if compilation.output.contains('IError err = ${err_tmp};') {
				has_visible_or_block_err = true
			}
		}
	}
	assert source_err_tmp != ''
	assert !compilation.output.contains('IError err = ${source_err_tmp};')
	assert has_visible_or_block_err
}

fn test_imported_empty_interface_concat_does_not_emit_noop_array_cast_helper() {
	os.chdir(vroot) or {}
	path := os.join_path(vroot,
		'vlib/v/tests/modules/interface_array_concat_from_another_module/main_test.v')
	symbol := '__v_array_to_interface_array__Array_interface_array_concat_from_another_module__mod__Value__to__Array_interface_array_concat_from_another_module__mod__Value'
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(path)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	assert !compilation.output.contains(symbol)
}

fn test_windows_sharedlive_string_interpolation_in_ternary_does_not_emit_inline_tmp_decl() {
	os.chdir(vroot) or {}
	test_source := os.join_path(os.vtmp_dir(), 'coutput_live_windows_ternary_str_intp.vv')
	os.write_file(test_source,
		"module main\n\n@[live]\nfn foo(ok bool, name string) string {\n\treturn if ok { 'Hello, \${name}!' } else { '\${u32(7)}' }\n}\n\nfn main() {\n\tprintln(foo(true, 'V'))\n}\n")!
	defer {
		os.rm(test_source) or {}
	}
	cmd := '${os.quoted_path(vexe)} -o - -os windows -sharedlive ${os.quoted_path(test_source)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	mut normalized := compilation.output.replace('\t', ' ').replace('\n', ' ')
	for normalized.contains('  ') {
		normalized = normalized.replace('  ', ' ')
	}
	assert !normalized.contains('? ( string _t')
	assert compilation.output.contains('builtin__str_intp')
}

fn test_no_main_exports_initialize_windows_runtime() {
	os.chdir(vroot) or {}
	test_source := os.join_path(os.vtmp_dir(), 'coutput_no_main_export_windows_init.vv')
	os.write_file(test_source,
		"module no_main\n\n@[export: 'v_sdl_app_quit']\npub fn app_quit() {}\n")!
	defer {
		os.rm(test_source) or {}
	}
	cmd := '${os.quoted_path(vexe)} -o - -os windows ${os.quoted_path(test_source)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	generated_c_lines := compilation.output.split_into_lines()
	expected_lines := [
		'static void _vno_main_init_caller(void);',
		'static void _vno_main_cleanup_caller(void);',
		'void v_sdl_app_quit(void) {',
		'_vno_main_init_caller();',
		'void _vinit(int ___argc, voidptr ___argv) {',
		'static bool once = false; if (once) {return;} once = true;',
		'void _vcleanup(void) {',
		'static void _vno_main_cleanup_caller(void) {',
		'static void _vno_main_init_caller(void) {',
		'con_valid = AttachConsole(ATTACH_PARENT_PROCESS);',
		'err = freopen_s(&res_fp, "NUL", "w", stdout);',
		'_vinit(0,0);',
		'atexit(_vno_main_cleanup_caller);',
	]
	for expected_line in expected_lines {
		assert does_line_match_one_of_generated_lines(expected_line, generated_c_lines)
	}
}

fn test_c_fallback_decl_uses_module_wide_c_includes() {
	os.chdir(vroot) or {}
	test_source := os.join_path(os.vtmp_dir(), 'coutput_module_c_include')
	os.rmdir_all(test_source) or {}
	os.mkdir_all(test_source)!
	defer {
		os.rmdir_all(test_source) or {}
	}
	header_path := os.join_path(test_source, 'c_header_decl.h')
	os.write_file(header_path, 'int c_header_decl(const char* input);\n')!
	header_include_path := header_path.replace('\\', '/')
	os.write_file(os.join_path(test_source, 'include.v'), 'module main

#include "${header_include_path}"
')!
	os.write_file(os.join_path(test_source, 'decl.v'), "module main

fn C.c_header_decl(input &char) int

fn main() {
	C.c_header_decl(c'text')
}
")!
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(test_source)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	assert !compilation.output.contains('extern int c_header_decl(')
}

fn test_c_fallback_decl_uses_c_helper_submodule_includes() {
	test_source := os.join_path(os.vtmp_dir(), 'coutput_c_helper_include')
	module_path := os.join_path(test_source, 'sdl')
	c_module_path := os.join_path(module_path, 'c')
	os.rmdir_all(test_source) or {}
	os.mkdir_all(c_module_path)!
	defer {
		os.rmdir_all(test_source) or {}
	}
	header_path := os.join_path(c_module_path, 'c_helper_decl.h')
	os.write_file(header_path,
		['#include <stdbool.h>', 'typedef enum { false_value, true_value } foreign_bool;', 'foreign_bool c_helper_decl(void);'].join('\n') +
		'\n')!
	header_include_path := header_path.replace('\\', '/')
	os.write_file(os.join_path(c_module_path, 'c.c.v'), 'module c

pub const used_import = 1

#include "${header_include_path}"
')!
	os.write_file(os.join_path(module_path, 'sdl.v'), 'module sdl

import sdl.c

pub const used_import = c.used_import
')!
	os.write_file(os.join_path(module_path, 'atomic.c.v'), 'module sdl

fn C.c_helper_decl() bool

pub fn call() {
	_ = C.c_helper_decl()
}
')!
	old_wd := os.getwd()
	os.chdir(test_source) or {}
	defer {
		os.chdir(old_wd) or {}
	}
	cmd := '${os.quoted_path(vexe)} -shared -o - sdl'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	assert compilation.output.contains('#include "${header_include_path}"')
	assert !compilation.output.contains('extern bool c_helper_decl(')
}

fn test_user_defined_windows_dllmain_disables_generated_entrypoint() {
	os.chdir(vroot) or {}
	test_source := os.join_path(os.vtmp_dir(), 'coutput_user_defined_windows_dllmain.vv')
	os.write_file(test_source,
		['module test', '', 'pub type C.DWORD = u32', 'pub type C.LPVOID = voidptr', '', 'fn C._vinit_caller()', 'fn C._vcleanup_caller()', '', "@[export: 'library_answer']", 'pub fn library_answer() int {', '\treturn 42', '}', '', "@[export: 'DllMain']", 'pub fn dll_main(hinst C.HINSTANCE, reason C.DWORD, reserved C.LPVOID) C.BOOL {', '\t_ = hinst', '\t_ = reserved', '\tif reason == C.DWORD(1) {', '\t\tC._vinit_caller()', '\t} else if reason == C.DWORD(0) {', '\t\tC._vcleanup_caller()', '\t}', '\treturn 1', '}'].join('\n') +
		'\n')!
	defer {
		os.rm(test_source) or {}
	}
	cmd := '${os.quoted_path(vexe)} -o - -os windows -shared -gc boehm ${os.quoted_path(test_source)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	assert compilation.output.contains('void _vinit_caller() {')
	assert compilation.output.contains('GC_set_pages_executable(0);')
	assert compilation.output.contains('GC_INIT();')
	assert compilation.output.contains('DllMain(')
	assert compilation.output.contains('_vinit_caller();')
	assert compilation.output.contains('_vcleanup_caller();')
	assert !compilation.output.contains('switch (fdwReason)')
	assert !compilation.output.contains('case DLL_PROCESS_ATTACH')
}

fn test_array_sort_with_compare_uses_stable_sort_adapters() {
	os.chdir(vroot) or {}
	test_source := os.join_path(os.vtmp_dir(), 'coutput_array_sort_with_compare_stable_sort.vv')
	source_lines := [
		'module main',
		'',
		'struct Foo {',
		'\tx int',
		'}',
		'',
		'fn by_x(a &Foo, b &Foo) int {',
		'\treturn a.x - b.x',
		'}',
		'',
		'fn main() {',
		'\tmut xs := [Foo{ x: 2 }, Foo{ x: 1 }]',
		'\txs.sort_with_compare(by_x)',
		'\tmut ys := [Foo{ x: 2 }, Foo{ x: 1 }]!',
		'\tys.sort_with_compare(by_x)',
		'\tmut zs := [Foo{ x: 2 }, Foo{ x: 1 }]',
		'\tzs.sort(a.x < b.x)',
		'}',
	]
	os.write_file(test_source, source_lines.join('\n') + '\n')!
	defer {
		os.rm(test_source) or {}
	}
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(test_source)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	mut normalized := compilation.output.replace('\t', ' ').replace('\n', ' ')
	for normalized.contains('  ') {
		normalized = normalized.replace('  ', ' ')
	}
	assert normalized.contains('int main__by_x_qsort_adapter(const void* a, const void* b) { return main__by_x((main__Foo*)a, (main__Foo*)b); }')
	assert normalized.contains('if (xs.len > 0) { v_stable_sort(xs.data, xs.len, xs.element_size, main__by_x_qsort_adapter); }')
	assert normalized.contains('v_stable_sort(&ys, 2, sizeof(main__Foo), main__by_x_qsort_adapter);')
	assert normalized.contains('_qsort_adapter(const void* a, const void* b) { return compare_')
	assert normalized.contains('v_stable_sort(zs.data, zs.len, zs.element_size, compare_')
	assert normalized.contains('_qsort_adapter);')
}

fn test_veb_implicit_ctx_alias_uses_user_context_name() {
	os.chdir(vroot) or {}
	test_source := os.join_path(os.vtmp_dir(), 'coutput_veb_implicit_ctx_alias.vv')
	os.write_file(test_source,
		['module main', '', 'import veb', '', 'struct App {}', '', 'struct Context {', '\tveb.Context', '}', '', 'fn (app App) nested(mut ctx Context) veb.Result {', "\treturn ctx.text('nested')", '}', '', 'fn (app App) log(_ Context) {', "\tprintln('hi')", '}', '', 'fn (app App) index(mut c Context) veb.Result {', '\tapp.log(c)', '\treturn app.nested()', '}', '', 'fn main() {', '\tmut app := App{}', '\tmut ctx := Context{}', '\t_ = app.index(mut ctx)', '}'].join('\n') +
		'\n')!
	defer {
		os.rm(test_source) or {}
	}
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(test_source)}'
	compilation := os.execute(cmd)
	ensure_compilation_succeeded(compilation, cmd)
	mut normalized := compilation.output.replace('\t', ' ').replace('\n', ' ')
	for normalized.contains('  ') {
		normalized = normalized.replace('  ', ' ')
	}
	assert normalized.contains('veb__Result main__App_index(main__App app, main__Context* c) { main__App_log(app, *c); GC_reachable_here(&c); return main__App_nested(app, c); }')
}

fn test_veb_implicit_ctx_alias_on_context_receiver_tmpl_not_found() {
	os.chdir(vroot) or {}
	test_dir := os.join_path(os.vtmp_dir(), 'coutput_veb_context_receiver_tmpl_not_found')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(os.join_path(test_dir, 'web'))!
	test_source := os.join_path(test_dir, 'main.v')
	os.write_file(os.join_path(test_dir, 'web', 'notfound.html'), '<h1>@ctx.req.url</h1>\n')!
	os.write_file(test_source,
		['module main', '', 'import veb', '', 'pub struct Context {', '\tveb.Context', '}', '', 'pub struct App {}', '', 'pub fn (mut c Context) not_found() veb.Result {', '\tc.res.set_status(.not_found)', "\treturn c.html(\$tmpl('web/notfound.html'))", '}', '', 'fn main() {', '\tmut app := App{}', '\tveb.run[App, Context](mut app, 8080)', '}'].join('\n') +
		'\n')!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	test_exe := os.join_path(test_dir, 'app')
	compile_cmd := '${os.quoted_path(vexe)} -gc boehm_full_opt -o ${os.quoted_path(test_exe)} ${os.quoted_path(test_source)}'
	ensure_compilation_succeeded(os.execute(compile_cmd), compile_cmd)
	c_cmd := '${os.quoted_path(vexe)} -gc boehm_full_opt -o - ${os.quoted_path(test_source)}'
	compilation := os.execute(c_cmd)
	ensure_compilation_succeeded(compilation, c_cmd)
	not_found_start := 'veb__Result main__Context_not_found(main__Context* c) {'
	assert compilation.output.contains(not_found_start)
	not_found_body :=
		compilation.output.all_after(not_found_start).all_before('VV_LOC void main__main')
	assert !not_found_body.contains('GC_reachable_here(&ctx);')
	assert not_found_body.contains('GC_reachable_here(&c);')
	assert not_found_body.contains('return veb__Context_html(&c->Context, _tmpl_res_')
}

fn does_line_match_one_of_generated_lines(line string, generated_c_lines []string) bool {
	for cline in generated_c_lines {
		if line == cline {
			return true
		}
		if cline.contains(line) {
			return true
		}
	}
	return false
}

fn normalize_panic_message(message string, vroot string) string {
	mut msg := message.all_before('=========================================')
	// change windows to nix path
	s := vroot.replace(os.path_separator, '/')
	msg = msg.replace(s + '/', '')
	msg = msg.trim_space()
	return msg
}

fn vroot_relative(opath string) string {
	nvroot := vroot.replace(os.path_separator, '/') + '/'
	npath := opath.replace(os.path_separator, '/')
	return npath.replace(nvroot, '')
}

fn ensure_compilation_succeeded(compilation os.Result, cmd string) {
	if compilation.exit_code < 0 {
		eprintln('> cmd exit_code < 0, cmd: ${cmd}')
		panic(compilation.output)
	}
	if compilation.exit_code != 0 {
		eprintln('> cmd exit_code != 0, cmd: ${cmd}')
		panic('compilation failed: ${compilation.output}')
	}
}

fn target2paths(target_path string, postfix string) (string, string, string, string) {
	basename := os.file_name(target_path).replace(postfix, '')
	target_dir := os.dir(target_path)
	path := os.join_path(target_dir, '${basename}.vv')
	relpath := vroot_relative(path)
	target_relpath := vroot_relative(target_path)
	return basename, path, relpath, target_relpath
}

struct FileOptions {
mut:
	vflags string
}

pub fn get_file_options(file string) FileOptions {
	mut res := FileOptions{}
	lines := os.read_lines(file) or { [] }
	for line in lines {
		if line.starts_with('// vtest vflags:') {
			res.vflags = line.all_after(':').trim_space()
		}
	}
	return res
}

const github_job = os.getenv('GITHUB_JOB')

fn should_skip(relpath string) bool {
	if github_job.contains('musl') && relpath.ends_with('autofree_sql_or_block.vv') {
		eprintln('> skipping ${relpath} on ${github_job}, since it uses db.sqlite, and its headers are not available to the C compiler in that environment')
		return true
	}
	if github_job.contains('musl') && (relpath.ends_with('print_boehm_leak.vv')
		|| relpath.ends_with('scope_cleanup_boehm_leak.vv')
		|| relpath.ends_with('gc_debugger_linux.vv')) {
		eprintln('> skipping ${relpath} on ${github_job}, since gc related tests are not compatible with `-gc none`')
		return true
	}
	if user_os == 'windows' {
		if relpath.contains('_nix.vv') {
			eprintln('> skipping ${relpath} on windows')
			return true
		}
		$if !msvc {
			if relpath.contains('_msvc_windows.vv') {
				eprintln('> skipping ${relpath} on !msvc')
				return true
			}
		}
		$if !gcc {
			if relpath.contains('_gcc_windows.vv') {
				eprintln('> skipping ${relpath} on !gcc')
				return true
			}
		}
		$if msvc {
			if relpath.contains('_not_msvc_windows.vv') {
				eprintln('> skipping ${relpath} on msvc')
				return true
			}
			if relpath.contains('asm_') {
				eprintln('> skipping ${relpath} on msvc, since it uses gcc-style inline asm')
				return true
			}
		}
	} else {
		if relpath.contains('_windows.vv') {
			eprintln('> skipping ${relpath} on !windows')
			return true
		}
	}
	if relpath.contains('freestanding_module_import_') {
		$if !amd64 {
			// https://github.com/vlang/v/issues/23397
			eprintln('> skipping ${relpath} on != amd64')
			return true
		}
		if user_os != 'linux' {
			eprintln('> skipping ${relpath} on != linux')
			return true
		}
		if gcc_path == '' {
			eprintln('> skipping ${relpath} since it needs gcc, which is not detected')
			return true
		}
	}
	if user_os == 'macos' {
		$if arm64 {
			if relpath.ends_with('spawn_stack_nix.vv') {
				eprintln('> skipping ${relpath} on macOS arm64, since i386 linking is unavailable')
				return true
			}
		}
	}
	if gcc_path == '' {
		test_path := os.join_path(vroot, relpath)
		file_options := get_file_options(test_path)
		if file_options.vflags.contains('-cc gcc') {
			eprintln('> skipping ${relpath} since its vflags require gcc, which is not detected')
			return true
		}
	}
	return false
}

@[if !silent ?]
fn vprintln(msg string) {
	println(msg)
}
