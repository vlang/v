module gen

import v.util

pub fn (mut g Gen) gen_c_main() {
	if !g.has_main {
		return
	}
	if g.pref.is_liveshared {
		return
	}
	g.out.writeln('')
	main_fn_start_pos := g.out.len
	if g.pref.os == .android && g.pref.is_apk {
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
		g.writeln('\n#line $lines_so_far "${g.vlines_path}"')
		g.writeln('')
	}
}

fn (mut g Gen) gen_c_main_header() {
	if g.pref.os == .windows {
		if g.is_gui_app() {
			// GUI application
			g.writeln('int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prev_instance, LPWSTR cmd_line, int show_cmd){')
		} else {
			// Console application
			g.writeln('int wmain(int ___argc, wchar_t* ___argv[], wchar_t* ___envp[]){')
		}
	} else {
		g.writeln('int main(int ___argc, char** ___argv){')
	}
	if g.pref.os == .windows && g.is_gui_app() {
		g.writeln('\tLPWSTR full_cmd_line = GetCommandLineW(); // NB: do not use cmd_line')
		g.writeln('\ttypedef LPWSTR*(WINAPI *cmd_line_to_argv)(LPCWSTR, int*);')
		g.writeln('\tHMODULE shell32_module = LoadLibrary(L"shell32.dll");')
		g.writeln('\tcmd_line_to_argv CommandLineToArgvW = (cmd_line_to_argv)GetProcAddress(shell32_module, "CommandLineToArgvW");')
		g.writeln('\tint ___argc;')
		g.writeln('\twchar_t** ___argv = CommandLineToArgvW(full_cmd_line, &___argc);')
	}
	g.writeln('\t_vinit();')
	if g.pref.is_prof {
		g.writeln('')
		g.writeln('\tatexit(vprint_profile_stats);')
		g.writeln('')
	}
	if g.is_importing_os() {
		if g.autofree {
			g.writeln('free(_const_os__args.data); // empty, inited in _vinit()')
		}
		if g.pref.os == .windows {
			g.writeln('\t_const_os__args = os__init_os_args_wide(___argc, ___argv);')
		} else {
			g.writeln('\t_const_os__args = os__init_os_args(___argc, (byteptr*)___argv);')
		}
	}
	if g.pref.is_livemain {
		g.generate_hotcode_reloading_main_caller()
	}
}

pub fn (mut g Gen) gen_c_main_footer() {
	if g.autofree {
		g.writeln('\t_vcleanup();')
	}
	g.writeln('\treturn 0;')
	g.writeln('}')
}

pub fn (mut g Gen) gen_c_android_sokol_main() {
	// TODO get autofree weaved into android lifecycle somehow
	/*
	if g.autofree {
		g.writeln('\t_vcleanup();')
	}
	*/
	// TODO do proper check for the global g_desc field we need
	g.writeln('sapp_desc sokol_main(int argc, char* argv[]) {')
	g.writeln('\t(void)argc; (void)argv;')
	g.writeln('')
	g.writeln('\t_vinit();')
	g.writeln('\tmain__main();')
	g.writeln('')
	g.writeln('\treturn g_desc;')
	g.writeln('}')
}

pub fn (mut g Gen) write_tests_main() {
	g.includes.writeln('#include <setjmp.h> // write_tests_main')
	g.definitions.writeln('int g_test_oks = 0;')
	g.definitions.writeln('int g_test_fails = 0;')
	g.definitions.writeln('jmp_buf g_jump_buffer;')
	main_fn_start_pos := g.out.len
	$if windows {
		g.writeln('int wmain() {')
	} $else {
		g.writeln('int main() {')
	}
	g.writeln('\t_vinit();')
	g.writeln('')
	all_tfuncs := g.get_all_test_function_names()
	if g.pref.is_stats {
		g.writeln('\tmain__BenchedTests bt = main__start_testing($all_tfuncs.len, tos_lit("$g.pref.path"));')
	}
	for t in all_tfuncs {
		g.writeln('')
		if g.pref.is_stats {
			g.writeln('\tmain__BenchedTests_testing_step_start(&bt, tos_lit("$t"));')
		}
		g.writeln('\tif (!setjmp(g_jump_buffer)) ${t}();')
		if g.pref.is_stats {
			g.writeln('\tmain__BenchedTests_testing_step_end(&bt);')
		}
	}
	g.writeln('')
	if g.pref.is_stats {
		g.writeln('\tmain__BenchedTests_end_testing(&bt);')
	}
	g.writeln('')
	if g.autofree {
		g.writeln('\t_vcleanup();')
	}
	g.writeln('\treturn g_test_fails > 0;')
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && 'main' in g.pref.printfn_list {
		println(g.out.after(main_fn_start_pos))
	}
}
