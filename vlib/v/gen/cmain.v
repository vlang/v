module gen

pub fn (mut g Gen) gen_c_main() {
	if !g.has_main {
		return
	}
	if g.pref.is_liveshared {
		return
	}
	g.out.writeln('')
	g.gen_c_main_header()
	g.writeln('\tmain__main();')
	g.gen_c_main_footer()
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
		g.writeln('\ttypedef LPWSTR*(WINAPI *cmd_line_to_argv)(LPCWSTR, int*);')
		g.writeln('\tHMODULE shell32_module = LoadLibrary(L"shell32.dll");')
		g.writeln('\tcmd_line_to_argv CommandLineToArgvW = (cmd_line_to_argv)GetProcAddress(shell32_module, "CommandLineToArgvW");')
		g.writeln('\tint ___argc;')
		g.writeln('\twchar_t** ___argv = CommandLineToArgvW(cmd_line, &___argc);')
	}
	g.writeln('\t_vinit();')
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
