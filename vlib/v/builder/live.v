module builder

import (
	os
	time
)

fn (v &Builder) generate_hotcode_reloading_compiler_flags() []string {
	mut a := []string
	if v.pref.is_live || v.pref.is_so {
		// See 'man dlopen', and test running a GUI program compiled with -live
		if v.pref.os == .linux || os.user_os() == 'linux' {
			a << '-rdynamic'
		}
		if v.pref.os == .mac || os.user_os() == 'mac' {
			a << '-flat_namespace'
		}
	}
	return a
}

fn (v &Builder) generate_hotcode_reloading_declarations() {
	/*
	QTODO
	mut cgen := v.cgen
	if v.pref.os != .windows {
		if v.pref.is_so {
			cgen.genln('pthread_mutex_t live_fn_mutex;')
		}
		if v.pref.is_live {
			cgen.genln('pthread_mutex_t live_fn_mutex = PTHREAD_MUTEX_INITIALIZER;')
		}
	}
	else {
		if v.pref.is_so {
			cgen.genln('HANDLE live_fn_mutex;')
			cgen.genln('
void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}

void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}')
		}
		if v.pref.is_live {
			cgen.genln('HANDLE live_fn_mutex = 0;')
		}
	}
	*/
}

fn (v &Builder) generate_hotcode_reloading_main_caller() {
	// QTODO
	/*
	if !v.pref.is_live {
		return
	}
	// We are in live code reload mode, so start the .so loader in the background
	mut cgen := v.cgen
	cgen.genln('')
	file_base := os.file_name(v.pref.path).replace('.v', '')
	if v.pref.os != .windows {
		// unix:
		so_name := file_base + '.so'
		cgen.genln('  char *live_library_name = "$so_name";')
		cgen.genln('  load_so(live_library_name);')
		cgen.genln('  pthread_t _thread_so;')
		cgen.genln('  pthread_create(&_thread_so , NULL, (void *)&reload_so, live_library_name);')
	}
	else {
		// windows:
		so_name := file_base + if v.pref.ccompiler == 'msvc' { '.dll' } else { '.so' }
		cgen.genln('  char *live_library_name = "$so_name";')
		cgen.genln('  live_fn_mutex = CreateMutexA(0, 0, 0);')
		cgen.genln('  load_so(live_library_name);')
		cgen.genln('  unsigned long _thread_so;')
		cgen.genln('  _thread_so = CreateThread(0, 0, (LPTHREAD_START_ROUTINE)&reload_so, 0, 0, 0);')
	}
	*/
}

fn (v &Builder) generate_hot_reload_code() {
	/*
	QTODO
	mut cgen := v.cgen
	// Hot code reloading
	if v.pref.is_live {
		mut file := os.real_path(v.pref.path)
		file_base := os.file_name(file).replace('.v', '')
		so_name := file_base + '.so'
		// Need to build .so file before building the live application
		// The live app needs to load this .so file on initialization.
		mut vexe := os.args[0]
		if os.user_os() == 'windows' {
			vexe = cescaped_path(vexe)
			file = cescaped_path(file)
		}
		mut msvc := ''
		if v.pref.ccompiler == 'msvc' {
			msvc = '-cc msvc'
		}
		so_debug_flag := if v.pref.is_debug { '-g' } else { '' }
		cmd_compile_shared_library := '$vexe $msvc $so_debug_flag -o $file_base -solive -shared $file'
		if v.pref.verbosity.is_higher_or_equal(.level_one) {
			println(cmd_compile_shared_library)
		}
		ticks := time.ticks()
		so_compilation_result := os.system(cmd_compile_shared_library)
		if v.pref.verbosity.is_higher_or_equal(.level_two) {
			diff := time.ticks() - ticks
			println('compiling shared library took $diff ms')
			println('=========\n')
		}
		if so_compilation_result != 0 {
			exit(1)
		}
		cgen.genln('

void lfnmutex_print(char *s){
#if 0
	fflush(stderr);
	fprintf(stderr,">> live_fn_mutex: %p | %s\\n", &live_fn_mutex, s);
	fflush(stderr);
#endif
}
')
		if v.pref.os != .windows {
			cgen.genln('
#define _POSIX_C_SOURCE 1
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

void* live_lib = 0;

int load_so(byteptr path) {
	char cpath[PATH_MAX] = {0};
	int res = snprintf(cpath, sizeof (cpath), "./%s", path);
	if (res >= sizeof (cpath)) {
		fprintf (stderr, "path is too long");
		return 0;
	}
	//printf("load_so %s\\n", cpath);
	if (live_lib) dlclose(live_lib);
	live_lib = dlopen(cpath, RTLD_LAZY);
	if (!live_lib) {
		fprintf(stderr, "open failed");
		exit(1);
		return 0;
	}
')
			for so_fn in cgen.so_fns {
				cgen.genln('$so_fn = dlsym(live_lib, "$so_fn");  ')
			}
		}
		else {
			cgen.genln('

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
	int res = snprintf(cpath, sizeof (cpath), "./%s", path);
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
')
			for so_fn in cgen.so_fns {
				cgen.genln('$so_fn = (void *)GetProcAddress(live_lib, "$so_fn");  ')
			}
		}
		cgen.genln('return 1;
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

			//v -o bounce -shared bounce.v
			snprintf(new_so_base, sizeof (new_so_base), ".tmp.%d.${file_base}", _live_reloads);
			#ifdef _WIN32
			// We have to make this directory becuase windows WILL NOT
			// do it for us
			os__mkdir(string_all_before_last(tos2(new_so_base), tos2("/")));
			#endif
			#ifdef _MSC_VER
			snprintf(new_so_name, sizeof (new_so_name), "%s.dll", new_so_base);
			#else
			snprintf(new_so_name, sizeof (new_so_name), "%s.so", new_so_base);
			#endif
			snprintf(compile_cmd, sizeof (compile_cmd), "$vexe $msvc -o %s -solive -shared $file", new_so_base);
			os__system(tos2(compile_cmd));

			if( !os__exists(tos2(new_so_name)) ) {
				puts("Errors while compiling $file\\n");
				continue;
			}

			lfnmutex_print("reload_so locking...");
			pthread_mutex_lock(&live_fn_mutex);
			lfnmutex_print("reload_so locked");

			load_so(new_so_name);
			#ifndef _WIN32
			unlink(new_so_name); // removing the .so file from the filesystem after dlopen-ing it is safe, since it will still be mapped in memory.
			#else
			_unlink(new_so_name);
			#endif
			//if(0 == rename(new_so_name, "${so_name}")){
			//	load_so("${so_name}");
			//}

			lfnmutex_print("reload_so unlocking...");
			pthread_mutex_unlock(&live_fn_mutex);
			lfnmutex_print("reload_so unlocked");

		}
		time__sleep_ms(100);
	}
}
')
	}
	if v.pref.is_so {
		cgen.genln(' int load_so(byteptr path) { return 0; }')
	}
	*/
}
