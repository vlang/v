module c

import v.util
import v.ast
import strings

pub const reset_dbg_line = '#line 999999999'

pub fn (mut g Gen) gen_c_main() {
	if !g.has_main {
		return
	}
	if g.pref.is_liveshared {
		return
	}
	if g.pref.is_o {
		// no main in .o files
		return
	}
	if 'no_main' in g.pref.compile_defines {
		return
	}
	g.out.writeln('')
	main_fn_start_pos := g.out.len

	is_sokol := 'sokol' in g.table.imports
	if (g.pref.os == .android && g.pref.is_apk) || (g.pref.os == .ios && is_sokol) {
		g.gen_c_android_sokol_main()
	} else {
		g.gen_c_main_header()
		g.writeln('\tmain__main();')
		g.gen_c_main_footer()
		if g.pref.printfn_list.len > 0 && 'main' in g.pref.printfn_list {
			println(g.out.after(main_fn_start_pos))
		}
	}
}

fn (mut g Gen) gen_vlines_reset() {
	if g.pref.is_vlines {
		// At this point, the v files are transpiled.
		// The rest is auto generated code, which will not have
		// different .v source file/line numbers.
		g.vlines_path = util.vlines_escape_path(g.pref.out_name_c, g.pref.ccompiler)
		g.writeln('')
		g.writeln('// Reset the C file/line numbers')
		g.writeln('${reset_dbg_line} "${g.vlines_path}"')
		g.writeln('')
	}
}

pub fn fix_reset_dbg_line(src string, out_file string) string {
	util.timing_start(@FN)
	defer {
		util.timing_measure(@FN)
	}
	// Note: using src.index() + a line counting loop + src.replace() here is slower,
	// since it has to iterate over pretty much the entire src string several times.
	// The loop below, does it just once, combining counting the lines, and finding the reset line:
	mut dbg_reset_line_idx := 0
	mut lines := 2
	for idx, ob in src {
		if ob == `\n` {
			lines++
			if unsafe { vmemcmp(src.str + idx + 1, reset_dbg_line.str, reset_dbg_line.len) } == 0 {
				dbg_reset_line_idx = idx + 1
				break
			}
		}
	}
	// find the position of the "..\..\..\src.tmp.c":
	mut first_quote_idx := 0
	for idx := dbg_reset_line_idx; idx < src.len; idx++ {
		if unsafe { src.str[idx] } == `"` {
			first_quote_idx = idx
			break
		}
	}
	// replace the reset line with the fixed line counter, keeping everything
	// before and after it unchanged:
	mut sb := strings.new_builder(src.len)
	unsafe {
		sb.write_ptr(src.str, dbg_reset_line_idx)
		sb.write_string('#line ')
		sb.write_decimal(lines)
		sb.write_ptr(src.str + first_quote_idx - 1, src.len - first_quote_idx)
	}
	$if trace_reset_dbg_line ? {
		eprintln('> reset_dbg_line: ${out_file}:${lines} | first_quote_idx: ${first_quote_idx} | src.len: ${src.len} | sb.len: ${sb.len} | sb.cap: ${sb.cap}')
	}
	return sb.str()
}

fn (mut g Gen) gen_c_main_function_only_header() {
	if g.pref.cmain != '' {
		g.writeln('int ${g.pref.cmain}(int ___argc, char** ___argv){')
		return
	}
	if g.pref.os == .windows {
		if g.is_gui_app() {
			$if msvc {
				// This is kinda bad but I dont see a way that is better
				g.writeln('#pragma comment(linker, "/SUBSYSTEM:WINDOWS")')
			}
			// GUI application
			g.writeln('int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prev_instance, LPWSTR cmd_line, int show_cmd){')
			g.writeln('\tLPWSTR full_cmd_line = GetCommandLineW(); // Note: do not use cmd_line')
			g.writeln('\ttypedef LPWSTR*(WINAPI *cmd_line_to_argv)(LPCWSTR, int*);')
			g.writeln('\tHMODULE shell32_module = LoadLibrary(L"shell32.dll");')
			g.writeln('\tcmd_line_to_argv CommandLineToArgvW = (cmd_line_to_argv)GetProcAddress(shell32_module, "CommandLineToArgvW");')
			g.writeln('\tint ___argc;')
			g.writeln('\twchar_t** ___argv = CommandLineToArgvW(full_cmd_line, &___argc);')

			g.writeln('BOOL con_valid = FALSE;')
			if g.force_main_console {
				g.writeln('con_valid = AllocConsole();')
			} else {
				g.writeln('con_valid = AttachConsole(ATTACH_PARENT_PROCESS);')
			}
			g.writeln('if (con_valid) {')
			g.writeln('\tFILE* res_fp = 0;')
			g.writeln('\terrno_t err;')
			g.writeln('\terr = freopen_s(&res_fp, "CON", "w", stdout);')
			g.writeln('\terr = freopen_s(&res_fp, "CON", "w", stderr);')
			g.writeln('\t(void)err;')
			g.writeln('}')
			return
		}
		// Console application
		g.writeln('int wmain(int ___argc, wchar_t* ___argv[], wchar_t* ___envp[]){')
		return
	}
	g.writeln('int main(int ___argc, char** ___argv){')
}

fn (mut g Gen) gen_c_main_function_header() {
	g.gen_c_main_function_only_header()
	g.gen_c_main_trace_calls_hook()
	g.writeln('\tg_main_argc = ___argc;')
	g.writeln('\tg_main_argv = ___argv;')
	if g.nr_closures > 0 {
		g.writeln('\t__closure_init(); // main()')
	}
}

fn (mut g Gen) gen_c_main_header() {
	g.gen_c_main_function_header()
	if g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt, .boehm_leak] {
		g.writeln('#if defined(_VGCBOEHM)')
		if g.pref.gc_mode == .boehm_leak {
			g.writeln('\tGC_set_find_leak(1);')
		}
		g.writeln('\tGC_set_pages_executable(0);')
		if g.pref.use_coroutines {
			g.writeln('\tGC_allow_register_threads();')
		}
		g.writeln('\tGC_INIT();')

		if g.pref.gc_mode in [.boehm_incr, .boehm_incr_opt] {
			g.writeln('\tGC_enable_incremental();')
		}
		g.writeln('#endif')
	}
	g.writeln('\t_vinit(___argc, (voidptr)___argv);')
	g.gen_c_main_profile_hook()
	if g.pref.is_livemain {
		g.generate_hotcode_reloading_main_caller()
	}
}

pub fn (mut g Gen) gen_c_main_footer() {
	g.writeln('\t_vcleanup();')
	g.writeln('\treturn 0;')
	g.writeln('}')
}

pub fn (mut g Gen) gen_c_android_sokol_main() {
	// Weave autofree into sokol lifecycle callback(s)
	if g.is_autofree {
		g.writeln('// Wrapping cleanup/free callbacks for sokol to include _vcleanup()
void (*_vsokol_user_cleanup_ptr)(void);
void (*_vsokol_user_cleanup_cb_ptr)(void *);

void (_vsokol_cleanup_cb)(void) {
	if (_vsokol_user_cleanup_ptr) {
		_vsokol_user_cleanup_ptr();
	}
	_vcleanup();
}

void (_vsokol_cleanup_userdata_cb)(void* user_data) {
	if (_vsokol_user_cleanup_cb_ptr) {
		_vsokol_user_cleanup_cb_ptr(g_desc.user_data);
	}
	_vcleanup();
}
')
	}
	g.writeln('// The sokol_main entry point on Android
sapp_desc sokol_main(int argc, char* argv[]) {
	(void)argc; (void)argv;')
	g.gen_c_main_trace_calls_hook()

	if g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt, .boehm_leak] {
		g.writeln('#if defined(_VGCBOEHM)')
		if g.pref.gc_mode == .boehm_leak {
			g.writeln('\tGC_set_find_leak(1);')
		}
		g.writeln('\tGC_set_pages_executable(0);')
		g.writeln('\tGC_INIT();')
		if g.pref.gc_mode in [.boehm_incr, .boehm_incr_opt] {
			g.writeln('\tGC_enable_incremental();')
		}
		g.writeln('#endif')
	}
	g.writeln('\t_vinit(argc, (voidptr)argv);
	')

	g.gen_c_main_profile_hook()
	g.writeln('\tmain__main();')
	if g.is_autofree {
		g.writeln('	// Wrap user provided cleanup/free functions for sokol to be able to call _vcleanup()
	if (g_desc.cleanup_cb) {
		_vsokol_user_cleanup_ptr = g_desc.cleanup_cb;
		g_desc.cleanup_cb = _vsokol_cleanup_cb;
	}
	else if (g_desc.cleanup_userdata_cb) {
		_vsokol_user_cleanup_cb_ptr = g_desc.cleanup_userdata_cb;
		g_desc.cleanup_userdata_cb = _vsokol_cleanup_userdata_cb;
	}
')
	}
	g.writeln('	return g_desc;')
	g.writeln('}')
}

pub fn (mut g Gen) write_tests_definitions() {
	g.includes.writeln('#include <setjmp.h> // write_tests_main')
	g.definitions.writeln('jmp_buf g_jump_buffer;')
}

pub fn (mut g Gen) gen_failing_error_propagation_for_test_fn(or_block ast.OrExpr, cvar_name string) {
	// in test_() functions, an `opt()?` call is sugar for
	// `or { cb_propagate_test_error(@LINE, @FILE, @MOD, @FN, err.msg() ) }`
	// and the test is considered failed
	paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
	err_msg := 'IError_name_table[${cvar_name}.err._typ]._method_msg(${cvar_name}.err._object)'
	g.writeln('\tmain__TestRunner_name_table[test_runner._typ]._method_fn_error(test_runner._object, ${paline}, tos3("${pafile}"), tos3("${pamod}"), tos3("${pafn}"), ${err_msg} );')
	g.writeln('\tlongjmp(g_jump_buffer, 1);')
}

pub fn (mut g Gen) gen_failing_return_error_for_test_fn(return_stmt ast.Return, cvar_name string) {
	// in test_() functions, a `return error('something')` is sugar for
	// `or { err := error('something') cb_propagate_test_error(@LINE, @FILE, @MOD, @FN, err.msg() ) return err }`
	// and the test is considered failed
	paline, pafile, pamod, pafn := g.panic_debug_info(return_stmt.pos)
	err_msg := 'IError_name_table[${cvar_name}.err._typ]._method_msg(${cvar_name}.err._object)'
	g.writeln('\tmain__TestRunner_name_table[test_runner._typ]._method_fn_error(test_runner._object, ${paline}, tos3("${pafile}"), tos3("${pamod}"), tos3("${pafn}"), ${err_msg} );')
	g.writeln('\tlongjmp(g_jump_buffer, 1);')
}

pub fn (mut g Gen) gen_c_main_profile_hook() {
	if g.pref.is_prof {
		g.writeln('')
		g.writeln('\tsignal(SIGINT, vprint_profile_stats_on_signal);')
		g.writeln('\tsignal(SIGTERM, vprint_profile_stats_on_signal);')
		g.writeln('\tatexit(vprint_profile_stats);')
		g.writeln('')
	}
	if g.pref.profile_file != '' {
		if 'no_profile_startup' in g.pref.compile_defines {
			g.writeln('vreset_profile_stats();')
		}
		if g.pref.profile_fns.len > 0 {
			g.writeln('vreset_profile_stats();')
			// v__profile_enabled will be set true *inside* the fns in g.pref.profile_fns:
			g.writeln('v__profile_enabled = false;')
		}
	}
}

pub fn (mut g Gen) gen_c_main_for_tests() {
	main_fn_start_pos := g.out.len
	g.writeln('')
	g.gen_c_main_function_header()
	if g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt, .boehm_leak] {
		g.writeln('#if defined(_VGCBOEHM)')
		if g.pref.gc_mode == .boehm_leak {
			g.writeln('\tGC_set_find_leak(1);')
		}
		g.writeln('\tGC_set_pages_executable(0);')
		if g.pref.use_coroutines {
			g.writeln('\tGC_allow_register_threads();')
		}
		g.writeln('\tGC_INIT();')
		if g.pref.gc_mode in [.boehm_incr, .boehm_incr_opt] {
			g.writeln('\tGC_enable_incremental();')
		}
		g.writeln('#endif')
	}
	g.writeln('\t_vinit(___argc, (voidptr)___argv);')
	g.writeln('\tmain__vtest_init();')
	g.gen_c_main_profile_hook()

	mut all_tfuncs := g.get_all_test_function_names()
	all_tfuncs = g.filter_only_matching_fn_names(all_tfuncs)
	g.writeln('\tstring v_test_file = ${ctoslit(g.pref.path)};')
	if g.pref.show_asserts {
		g.writeln('\tmain__BenchedTests bt = main__start_testing(${all_tfuncs.len}, v_test_file);')
	}
	g.writeln('')
	g.writeln('\tstruct _main__TestRunner_interface_methods _vtrunner = main__TestRunner_name_table[test_runner._typ];')
	g.writeln('\tvoid * _vtobj = test_runner._object;')
	g.writeln('')
	g.writeln('\tmain__VTestFileMetaInfo_free(test_runner.file_test_info);')
	g.writeln('\t*(test_runner.file_test_info) = main__vtest_new_filemetainfo(v_test_file, ${all_tfuncs.len});')
	g.writeln('\t_vtrunner._method_start(_vtobj, ${all_tfuncs.len});')
	g.writeln('')
	for tnumber, tname in all_tfuncs {
		tcname := util.no_dots(tname)
		testfn := unsafe { g.table.fns[tname] }
		lnum := testfn.pos.line_nr + 1
		g.writeln('\tmain__VTestFnMetaInfo_free(test_runner.fn_test_info);')
		g.writeln('\tstring tcname_${tnumber} = _SLIT("${tcname}");')
		g.writeln('\tstring tcmod_${tnumber}  = _SLIT("${testfn.mod}");')
		g.writeln('\tstring tcfile_${tnumber} = ${ctoslit(testfn.file)};')
		g.writeln('\t*(test_runner.fn_test_info) = main__vtest_new_metainfo(tcname_${tnumber}, tcmod_${tnumber}, tcfile_${tnumber}, ${lnum});')
		g.writeln('\t_vtrunner._method_fn_start(_vtobj);')
		g.writeln('\tif (!setjmp(g_jump_buffer)) {')
		//
		if g.pref.show_asserts {
			g.writeln('\t\tmain__BenchedTests_testing_step_start(&bt, tcname_${tnumber});')
		}
		g.writeln('\t\t${tcname}();')
		g.writeln('\t\t_vtrunner._method_fn_pass(_vtobj);')
		//
		g.writeln('\t}else{')
		//
		g.writeln('\t\t_vtrunner._method_fn_fail(_vtobj);')
		//
		g.writeln('\t}')
		if g.pref.show_asserts {
			g.writeln('\tmain__BenchedTests_testing_step_end(&bt);')
		}
		g.writeln('')
	}
	if g.pref.show_asserts {
		g.writeln('\tmain__BenchedTests_end_testing(&bt);')
	}
	g.writeln('')
	g.writeln('\t_vtrunner._method_finish(_vtobj);')
	g.writeln('\tint test_exit_code = _vtrunner._method_exit_code(_vtobj);')

	g.writeln('\t_vtrunner._method__v_free(_vtobj);')
	g.writeln('')
	g.writeln('\t_vcleanup();')
	g.writeln('')
	g.writeln('\treturn test_exit_code;')
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && 'main' in g.pref.printfn_list {
		println(g.out.after(main_fn_start_pos))
	}
}

pub fn (mut g Gen) filter_only_matching_fn_names(fnames []string) []string {
	if g.pref.run_only.len == 0 {
		return fnames
	}
	mut res := []string{}
	for tname in fnames {
		if tname.contains('testsuite_') {
			res << tname
			continue
		}
		mut is_matching := false
		for fn_glob_pattern in g.pref.run_only {
			if tname.match_glob(fn_glob_pattern) {
				is_matching = true
				break
			}
		}
		if !is_matching {
			continue
		}
		res << tname
	}
	return res
}

pub fn (mut g Gen) gen_c_main_trace_calls_hook() {
	if !g.pref.trace_calls {
		return
	}
	should_trace_c_main := g.pref.should_trace_fn_name('C.main')
	g.writeln('\tu8 bottom_of_stack = 0; g_stack_base = &bottom_of_stack; v__trace_calls__on_c_main(${should_trace_c_main});')
}
