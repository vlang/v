module c

import os
import v.pref
import v.util

fn (mut g Gen) generate_hotcode_reloading_declarations() {
	if g.pref.os == .windows {
		g.hotcode_definitions.writeln('HANDLE live_fn_mutex = 0;')
		g.hotcode_definitions.writeln('
HANDLE* v_live_fn_mutex_ptr(void) {
	if (g_live_reload_info) {
		v__live__LiveReloadInfo* live_info = (v__live__LiveReloadInfo*)g_live_reload_info;
		if (live_info->live_fn_mutex) {
			return (HANDLE*)live_info->live_fn_mutex;
		}
	}
	if (!live_fn_mutex) {
		live_fn_mutex = CreateMutexA(0, 0, 0);
	}
	return &live_fn_mutex;
}
void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}
void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}
')
	} else {
		g.hotcode_definitions.writeln('pthread_mutex_t live_fn_mutex;')
		g.hotcode_definitions.writeln('pthread_once_t live_fn_mutex_once = PTHREAD_ONCE_INIT;')
		g.hotcode_definitions.writeln('
void v_init_live_mutex(void) {
	pthread_mutexattr_t live_fn_mutex_attr;
	pthread_mutexattr_init(&live_fn_mutex_attr);
	pthread_mutexattr_settype(&live_fn_mutex_attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&live_fn_mutex, &live_fn_mutex_attr);
	pthread_mutexattr_destroy(&live_fn_mutex_attr);
}

pthread_mutex_t* v_live_fn_mutex_ptr(void) {
	if (g_live_reload_info) {
		v__live__LiveReloadInfo* live_info = (v__live__LiveReloadInfo*)g_live_reload_info;
		if (live_info->live_fn_mutex) {
			return (pthread_mutex_t*)live_info->live_fn_mutex;
		}
	}
	pthread_once(&live_fn_mutex_once, v_init_live_mutex);
	return &live_fn_mutex;
}
')
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
				load_code << '\tvoid* live_sym_${so_fn} = dlsym(live_lib, "impl_live_${so_fn}");'
				load_code << '\tif (live_sym_${so_fn}) {'
				load_code << '\t\timpl_live_${so_fn} = live_sym_${so_fn};'
				load_code << '\t} else {'
				load_code << '\t\timpl_live_${so_fn} = no_impl_${so_fn};'
				load_code << '\t}'
			}
			load_code << 'void (* fn_set_live_reload_pointer)(void *) = (void *)dlsym(live_lib, "set_live_reload_pointer");'
			phd = posix_hotcode_definitions_1
		} else {
			for so_fn in g.hotcode_fn_names {
				load_code << '\tvoid* live_sym_${so_fn} = (void *)GetProcAddress(live_lib, "impl_live_${so_fn}");'
				load_code << '\tif (live_sym_${so_fn}) {'
				load_code << '\t\timpl_live_${so_fn} = live_sym_${so_fn};'
				load_code << '\t} else {'
				load_code << '\t\timpl_live_${so_fn} = no_impl_${so_fn};'
				load_code << '\t}'
			}
			load_code << 'void (* fn_set_live_reload_pointer)(void *) = (void *)GetProcAddress(live_lib, "set_live_reload_pointer");'
			phd = windows_hotcode_definitions_1
		}
		// Ensure that g_live_reload_info from the executable is passed to the DLL .
		// See also vlib/v/live/sharedlib/live_sharedlib.v .
		load_code << 'if(fn_set_live_reload_pointer){ fn_set_live_reload_pointer( g_live_reload_info ); }'

		g.hotcode_definitions.writeln(phd.replace('@LOAD_FNS@', load_code.join('\n')))
	}
}

const posix_hotcode_definitions_1 = '
void v_bind_live_symbols(void* live_lib){
	@LOAD_FNS@
}
'

const windows_hotcode_definitions_1 = '
void v_bind_live_symbols(void* live_lib){
	@LOAD_FNS@
}
'

fn (mut g Gen) generate_hotcode_reloading_main_caller() {
	if !g.pref.is_livemain {
		return
	}
	g.writeln('')
	// We are in live code reload mode, so start the .so loader in the background
	g.writeln2('\t// live code initialization section:', '\t{')
	g.writeln('\t\t// initialization of live function pointers')
	for fname in g.hotcode_fn_names {
		g.writeln('\t\timpl_live_${fname} = no_impl_${fname};')
	}
	vexe := util.cescaped_path(pref.vexe_path())
	file := util.cescaped_path(g.pref.path)
	ccpath := util.cescaped_path(g.pref.ccompiler)
	ccompiler := '-cc ${ccpath}'
	so_debug_flag := if g.pref.is_debug { '-cg' } else { '' }
	mut vopts := '${ccompiler} ${so_debug_flag} -sharedlive -shared'
	if g.pref.os == .windows && g.is_cc_msvc && 'sokol' in g.table.imports {
		mut import_lib_path := g.pref.out_name
		ext := os.file_ext(import_lib_path)
		if ext != '' {
			import_lib_path = import_lib_path[..import_lib_path.len - ext.len] + '.lib'
		} else {
			import_lib_path += '.lib'
		}
		escaped_import_lib_path := util.cescaped_path(os.abs_path(import_lib_path))
		vopts += " -ldflags \\\"${escaped_import_lib_path}\\\""
	}

	g.writeln('\t\t// start background reloading thread')
	g.writeln('\t\tvoid* live_fn_mutex_addr = v_live_fn_mutex_ptr();')
	g.writeln('\t\tv__live__LiveReloadInfo* live_info = v__live__executable__new_live_reload_info(')
	g.writeln('\t\t\t\t\t builtin__tos2("${file}"),')
	g.writeln('\t\t\t\t\t builtin__tos2("${vexe}"),')
	g.writeln('\t\t\t\t\t builtin__tos2("${vopts}"),')
	g.writeln('\t\t\t\t\t live_fn_mutex_addr,')
	g.writeln('\t\t\t\t\t v_bind_live_symbols')
	g.writeln('\t\t);')
	mut already_added := map[string]bool{}
	for f in g.hotcode_fpaths {
		already_added[f] = true
	}
	mut idx := 0
	for f, _ in already_added {
		fpath := os.real_path(f)
		g.writeln('\t\tv__live__executable__add_live_monitored_file(live_info, ${ctoslit(fpath)}); // source V file with @[live] ${
			idx + 1}/${already_added.len}')
		idx++
	}
	g.writeln('')
	// g_live_reload_info gives access to the LiveReloadInfo methods,
	// to the custom user code, through calling v_live_info()
	g.writeln('\t\tg_live_reload_info = (void*)live_info;')
	g.writeln('\t\tv__live__executable__start_reloader(live_info);')
	g.writeln('\t}\t// end of live code initialization section')
	g.writeln('')
}
