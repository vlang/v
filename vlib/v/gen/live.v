module gen

import v.pref
import v.util

fn (mut g Gen) generate_hotcode_reloading_declarations() {
	if g.pref.os == .windows {
		if g.pref.is_livemain {
			g.hotcode_definitions.writeln('HANDLE live_fn_mutex = 0;')
		}
		if g.pref.is_liveshared {
			g.hotcode_definitions.writeln('HANDLE live_fn_mutex;')
		}
		g.hotcode_definitions.writeln('
void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}
void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}
')
	} else {
		if g.pref.is_livemain {
			g.hotcode_definitions.writeln('pthread_mutex_t live_fn_mutex = PTHREAD_MUTEX_INITIALIZER;')
		}
		if g.pref.is_liveshared {
			g.hotcode_definitions.writeln('pthread_mutex_t live_fn_mutex;')
		}
	}
}

fn (mut g Gen) generate_hotcode_reloader_code() {
	if g.pref.is_liveshared {
		g.hotcode_definitions.writeln('')
		return
	}
	// Hot code reloading
	if g.pref.is_livemain {
		mut phd := ''
		mut load_code := []string{}
		if g.pref.os != .windows {
			for so_fn in g.hotcode_fn_names {
				load_code << 'impl_live_$so_fn = dlsym(live_lib, "impl_live_$so_fn");'
			}
			phd = posix_hotcode_definitions_1
		} else {
			for so_fn in g.hotcode_fn_names {
				load_code <<
					'impl_live_$so_fn = (void *)GetProcAddress(live_lib, "impl_live_$so_fn");  '
			}
			phd = windows_hotcode_definitions_1
		}
		g.hotcode_definitions.writeln(phd.replace('@LOAD_FNS@', load_code.join('\n')))
	}
}

const (
	posix_hotcode_definitions_1   = '
void v_bind_live_symbols(void* live_lib){
	@LOAD_FNS@
}
'
	windows_hotcode_definitions_1 = '
void v_bind_live_symbols(void* live_lib){
	@LOAD_FNS@
}
'
)

fn (mut g Gen) generate_hotcode_reloading_main_caller() {
	if !g.pref.is_livemain {
		return
	}
	g.writeln('')
	// We are in live code reload mode, so start the .so loader in the background
	g.writeln('\t// live code initialization section:')
	g.writeln('\t{')
	g.writeln('\t\t// initialization of live function pointers')
	for fname in g.hotcode_fn_names {
		g.writeln('\t\timpl_live_$fname = 0;')
	}
	vexe := util.cescaped_path(pref.vexe_path())
	file := util.cescaped_path(g.pref.path)
	msvc := if g.pref.ccompiler == 'msvc' { '-cc msvc' } else { '' }
	so_debug_flag := if g.pref.is_debug { '-cg' } else { '' }
	vopts := '$msvc $so_debug_flag -sharedlive -shared'
	//
	g.writeln('\t\t// start background reloading thread')
	if g.pref.os == .windows {
		g.writeln('\t\tlive_fn_mutex = CreateMutexA(0, 0, 0);')
	}
	g.writeln('\t\tv__live__LiveReloadInfo* live_info = v__live__executable__new_live_reload_info(')
	g.writeln('\t\t\t\t\t tos2("$file"),')
	g.writeln('\t\t\t\t\t tos2("$vexe"),')
	g.writeln('\t\t\t\t\t tos2("$vopts"),')
	g.writeln('\t\t\t\t\t &live_fn_mutex,')
	g.writeln('\t\t\t\t\t v_bind_live_symbols')
	g.writeln('\t\t);')
	// g_live_info gives access to the LiveReloadInfo methods,
	// to the custom user code, through calling v_live_info()
	g.writeln('\t\t   g_live_info = (void*)live_info;')
	g.writeln('\t\tv__live__executable__start_reloader(live_info);')
	g.writeln('\t}\t// end of live code initialization section')
	g.writeln('')
}
