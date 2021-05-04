module c

import v.util
import v.ast

pub fn (mut g Gen) gen_c_main() {
	if !g.has_main {
		return
	}
	if g.pref.is_liveshared {
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
		//
		// TODO: calculate the proper line here, based on
		// the actual C lines in all the buffers
		lines_so_far := 1000000
		g.vlines_path = util.vlines_escape_path(g.pref.out_name_c, g.pref.ccompiler)
		g.writeln('')
		g.writeln('\n// Reset the file/line numbers')
		g.writeln('\n#line $lines_so_far "$g.vlines_path"')
		g.writeln('')
	}
}

fn (mut g Gen) gen_c_main_function_header() {
	if g.pref.os == .windows {
		if g.is_gui_app() {
			$if msvc {
				// This is kinda bad but I dont see a way that is better
				g.writeln('#pragma comment(linker, "/SUBSYSTEM:WINDOWS")')
			}
			// GUI application
			g.writeln('int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prev_instance, LPWSTR cmd_line, int show_cmd){')
			g.writeln('\tLPWSTR full_cmd_line = GetCommandLineW(); // NB: do not use cmd_line')
			g.writeln('\ttypedef LPWSTR*(WINAPI *cmd_line_to_argv)(LPCWSTR, int*);')
			g.writeln('\tHMODULE shell32_module = LoadLibrary(L"shell32.dll");')
			g.writeln('\tcmd_line_to_argv CommandLineToArgvW = (cmd_line_to_argv)GetProcAddress(shell32_module, "CommandLineToArgvW");')
			g.writeln('\tint ___argc;')
			g.writeln('\twchar_t** ___argv = CommandLineToArgvW(full_cmd_line, &___argc);')
		} else {
			// Console application
			g.writeln('int wmain(int ___argc, wchar_t* ___argv[], wchar_t* ___envp[]){')
		}
	} else {
		g.writeln('int main(int ___argc, char** ___argv){')
	}
}

fn (mut g Gen) gen_c_main_header() {
	g.gen_c_main_function_header()
	if g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt, .boehm,
		.boehm_leak,
	] {
		g.writeln('#if defined(_VGCBOEHM)')
		if g.pref.gc_mode == .boehm_leak {
			g.writeln('\tGC_set_find_leak(1);')
		}
		g.writeln('\tGC_INIT();')
		if g.pref.gc_mode in [.boehm_incr, .boehm_incr_opt] {
			g.writeln('\tGC_enable_incremental();')
		}
		g.writeln('#endif')
	}
	g.writeln('\t_vinit(___argc, (voidptr)___argv);')
	if g.pref.is_prof {
		g.writeln('')
		g.writeln('\tatexit(vprint_profile_stats);')
		g.writeln('')
	}
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
	(void)argc; (void)argv;

	_vinit(argc, (voidptr)argv);
	main__main();
')
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
	g.definitions.writeln('int g_test_oks = 0;')
	g.definitions.writeln('int g_test_fails = 0;')
	g.definitions.writeln('jmp_buf g_jump_buffer;')
}

pub fn (mut g Gen) gen_failing_error_propagation_for_test_fn(or_block ast.OrExpr, cvar_name string) {
	// in test_() functions, an `opt()?` call is sugar for
	// `or { cb_propagate_test_error(@LINE, @FILE, @MOD, @FN, err.msg) }`
	// and the test is considered failed
	paline, pafile, pamod, pafn := g.panic_debug_info(or_block.pos)
	g.writeln('\tmain__cb_propagate_test_error($paline, tos3("$pafile"), tos3("$pamod"), tos3("$pafn"), *(${cvar_name}.err.msg) );')
	g.writeln('\tg_test_fails++;')
	g.writeln('\tlongjmp(g_jump_buffer, 1);')
}

pub fn (mut g Gen) gen_c_main_for_tests() {
	main_fn_start_pos := g.out.len
	g.writeln('')
	g.gen_c_main_function_header()
	if g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt, .boehm,
		.boehm_leak,
	] {
		g.writeln('#if defined(_VGCBOEHM)')
		if g.pref.gc_mode == .boehm_leak {
			g.writeln('\tGC_set_find_leak(1);')
		}
		g.writeln('\tGC_INIT();')
		if g.pref.gc_mode in [.boehm_incr, .boehm_incr_opt] {
			g.writeln('\tGC_enable_incremental();')
		}
		g.writeln('#endif')
	}
	g.writeln('\t_vinit(___argc, (voidptr)___argv);')
	all_tfuncs := g.get_all_test_function_names()
	if g.pref.is_stats {
		g.writeln('\tmain__BenchedTests bt = main__start_testing($all_tfuncs.len, _SLIT("$g.pref.path"));')
	}
	g.writeln('')
	for tname in all_tfuncs {
		tcname := util.no_dots(tname)
		if g.pref.is_stats {
			g.writeln('\tmain__BenchedTests_testing_step_start(&bt, _SLIT("$tcname"));')
		}
		g.writeln('\tif (!setjmp(g_jump_buffer)) ${tcname}();')
		if g.pref.is_stats {
			g.writeln('\tmain__BenchedTests_testing_step_end(&bt);')
		}
	}
	g.writeln('')
	if g.pref.is_stats {
		g.writeln('\tmain__BenchedTests_end_testing(&bt);')
	}
	g.writeln('\t_vcleanup();')
	g.writeln('\treturn g_test_fails > 0;')
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && 'main' in g.pref.printfn_list {
		println(g.out.after(main_fn_start_pos))
	}
}
