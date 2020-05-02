module gen

import os
import time
import v.pref
import v.util

fn (g &Gen) generate_hotcode_reloading_declarations() {
	if g.pref.os != .windows {
		if g.pref.is_livemain {
			g.hotcode_definitions.writeln('pthread_mutex_t live_fn_mutex = PTHREAD_MUTEX_INITIALIZER;')
		}
		if g.pref.is_liveshared {
			g.hotcode_definitions.writeln('pthread_mutex_t live_fn_mutex;')
		}
	} else {
		if g.pref.is_livemain {
			g.hotcode_definitions.writeln('HANDLE live_fn_mutex = 0;')
		}
		if g.pref.is_liveshared {
			g.hotcode_definitions.writeln('HANDLE live_fn_mutex;')
			g.hotcode_definitions.writeln('
void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}

void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}')
		}
	}
}

fn (g &Gen) generate_hotcode_reloader_code() {
	if g.pref.is_liveshared {
		g.hotcode_definitions.writeln('')
		g.hotcode_definitions.writeln('int load_so(byteptr path) { return 0; }')
		g.hotcode_definitions.writeln('')
		return
	}
	// Hot code reloading
	if g.pref.is_livemain {
		mut file := os.real_path(g.pref.path)
		file_base := os.file_name(file).replace('.v', '')
		// Need to build .so file before building the live application
		// The live app needs to load this .so file on initialization.
		mut vexe := pref.vexe_path()
		mut so_dir := os.cache_dir()
		if os.user_os() == 'windows' {
			vexe = util.cescaped_path(vexe)
			file = util.cescaped_path(file)
			so_dir = util.cescaped_path(so_dir)
		}
		mut msvc := ''
		if g.pref.ccompiler == 'msvc' {
			msvc = '-cc msvc'
		}
		so_debug_flag := if g.pref.is_debug { '-cg' } else { '' }
		cmd_compile_shared_library := '$vexe $msvc -cg -keepc $so_debug_flag -o ${so_dir}/${file_base} -sharedlive -shared $file'
		if g.pref.is_verbose {
			println(cmd_compile_shared_library)
		}
		ticks := time.ticks()
		so_compilation_result := os.system(cmd_compile_shared_library)
		if g.pref.is_verbose {
			diff := time.ticks() - ticks
			println('compiling shared library took $diff ms')
		}
		if so_compilation_result != 0 {
			exit(1)
		}		
		mut phd := ''
		mut load_code := []string{}
		if g.pref.os != .windows {
			for so_fn in g.hotcode_fn_names {
				load_code << 'impl_live_${so_fn} = dlsym(live_lib, "impl_live_${so_fn}");'
			}
			phd = posix_hotcode_definitions_1
		} else {
			for so_fn in g.hotcode_fn_names {
				load_code << 'impl_live_${so_fn} = (void *)GetProcAddress(live_lib, "impl_live_${so_fn}");  '
			}
			phd = windows_hotcode_definitions_1
		}
		g.hotcode_definitions.writeln(phd.replace('@LOAD_FNS@', load_code.join('\n')))
		g.hotcode_definitions.writeln('

void lfnmutex_print(char *s){
#if 0
	fflush(stderr);
	fprintf(stderr,">> live_fn_mutex: %p | %s\\n", &live_fn_mutex, s);
	fflush(stderr);
#endif
}

void remove_so_file(char *s){
	// removing the .so file from the filesystem after dlopen-ing it is safe, since it will still be mapped in memory.
	#ifndef _WIN32
		unlink(s); 
	#else
		_unlink(s);
	#endif
}

int _live_reloads = 0;
void reload_so() {
	char new_so_base[PATH_MAX] = {0};
	char new_so_name[PATH_MAX] = {0};
	char compile_cmd[PATH_MAX] = {0};
	int last = os__file_last_mod_unix(tos2("$file"));
	while (1) {
		// TODO use inotify
		int now = os__file_last_mod_unix(tos2("$file"));
		if (now != last) {
			last = now;
			_live_reloads++;

			//v -o bounce -sharedlive -shared bounce.v
			snprintf(new_so_base, sizeof (new_so_base), "${so_dir}/tmp.%d.${file_base}", _live_reloads);
			#ifdef _MSC_VER
			snprintf(new_so_name, sizeof (new_so_name), "%s.dll", new_so_base);
			#else
			snprintf(new_so_name, sizeof (new_so_name), "%s.so", new_so_base);
			#endif
			snprintf(compile_cmd, sizeof (compile_cmd), "$vexe $msvc $so_debug_flag -cg -keepc -o %s -sharedlive -shared $file", new_so_base);
			os__system(tos2(compile_cmd));

			if( !os__exists(tos2(new_so_name)) ) {
				puts("Errors while compiling $file\\n");
				continue;
			}

			lfnmutex_print("reload_so locking...");
			pthread_mutex_lock(&live_fn_mutex);
			lfnmutex_print("reload_so locked");

			load_so(new_so_name);
			remove_so_file( new_so_name );

			lfnmutex_print("reload_so unlocking...");
			pthread_mutex_unlock(&live_fn_mutex);
			lfnmutex_print("reload_so unlocked");
		}
		time__sleep_ms(100);
	}
}
')
	}
}


const (
	posix_hotcode_definitions_1 = '
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 1024
#endif
void* live_lib = 0;
int load_so(byteptr path) {
	char cpath[PATH_MAX] = {0};
	int res = snprintf(cpath, sizeof(cpath), "%s", path);
	if (res >= sizeof (cpath)) {
		fprintf (stderr, "path is too long");
		return 0;
	}
	if (live_lib) {
		int closing_status = dlclose(live_lib);
		//fprintf(stderr, "Closing status: %d\\n", closing_status);
		live_lib = 0;
	}
	//fprintf (stderr, "Opening shared library at: %s\\n", cpath);
	live_lib = dlopen(cpath, RTLD_LAZY);
	if (!live_lib) {
		fprintf(stderr, "open failed, reason: %s\\n", dlerror());
		exit(1);
		return 0;
	}
	dlerror(); // clear errors
	//fprintf(stderr, "live_lib: %p\\n", live_lib);

    @LOAD_FNS@

	char *dlsym_error = dlerror(); 
	if (dlsym_error != NULL) {
		fprintf(stderr, "dlsym failed, reason: %s\\n", dlsym_error);
	}
    return 1;
}
'
	windows_hotcode_definitions_1 = '
#ifndef PATH_MAX
#define PATH_MAX 1024
#endif
void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}
void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}
void* live_lib = NULL;
int load_so(byteptr path) {
	char cpath[PATH_MAX];
	int res = snprintf(cpath, sizeof(cpath), "%s", path);
	if (res >= sizeof(cpath)) {
		puts("path is too long\\n");
		exit(1);
		return 0;
	}
	if (live_lib) FreeLibrary(live_lib);
	live_lib = LoadLibraryA(cpath);
	if (!live_lib) {
		puts("open failed\\n");
		exit(1);
		return 0;
	}
    @LOAD_FNS@
    return 1;
}
'
)

//

fn (g &Gen) generate_hotcode_reloading_main_caller() {
	if !g.pref.is_livemain {
		return
	}
	g.writeln('')
	// We are in live code reload mode, so start the .so loader in the background
	file_base := os.file_name(g.pref.path).replace('.v', '')
	mut so_dir := os.cache_dir()
	if os.user_os() == 'windows' {
		so_dir = util.cescaped_path(so_dir)
	}

	g.writeln('\t// live code initialization section:')
	g.writeln('\t{')
	g.writeln('\t\t// initialization of live function pointers')
	for fname in g.hotcode_fn_names {
		g.writeln('\t\timpl_live_${fname} = 0;')
	}
	
	g.writeln('\t\t// start background reloading thread')
	if g.pref.os != .windows {
		// unix:
		so_name := file_base + '.so'
		g.writeln('\t\tchar *live_library_name = "${so_dir}/$so_name";')
		g.writeln('\t\tload_so(live_library_name);')
		g.writeln('\t\tpthread_t _thread_so;')
		g.writeln('\t\tpthread_create(&_thread_so , NULL, (void *)&reload_so, live_library_name);')
	} else {
		// windows:
		so_extension := if g.pref.ccompiler == 'msvc' { '.dll' } else { '.so' }
		so_name := file_base + so_extension
		g.writeln('\t\tchar *live_library_name = "${so_dir}/$so_name";')
		g.writeln('\t\tlive_fn_mutex = CreateMutexA(0, 0, 0);')
		g.writeln('\t\tload_so(live_library_name);')
		g.writeln('\t\tunsigned long _thread_so;')
		g.writeln('\t\t_thread_so = CreateThread(0, 0, (LPTHREAD_START_ROUTINE)&reload_so, 0, 0, 0);')
	}
	g.writeln('\t}\t// end of live code initialization section')
	g.writeln('')
}
