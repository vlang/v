// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os
import time
import strings

const (
	Version = '0.1.17'  
)

enum BuildMode {
	// `v program.v'
	// Build user code only, and add pre-compiled vlib (`cc program.o builtin.o os.o...`)
	default_mode
	// `v -embed_vlib program.v`
	// vlib + user code in one file (slower compilation, but easier when working on vlib and cross-compiling)
	embed_vlib
	// `v -lib ~/v/os`
	// build any module (generate os.o + os.vh)
	build //TODO a better name would be smth like `.build_module` I think
}

fn modules_path() string {
	return os.home_dir() + '/.vmodules/'
}

const (
	SupportedPlatforms = ['windows', 'mac', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'msvc'] 
	ModPath            = modules_path()
)

enum OS {
	mac
	linux
	windows
	freebsd 
	openbsd 
	netbsd 
	dragonfly 
	msvc 
}

enum Pass {
	// A very short pass that only looks at imports in the beginning of
	// each file
	imports
	// First pass, only parses and saves declarations (fn signatures,
	// consts, types).
	// Skips function bodies.
	// We need this because in V things can be used before they are
	// declared.
	decl
	// Second pass, parses function bodies and generates C or machine code.
	main
}

struct V {
mut:
	os         OS // the OS to build for
	out_name_c string // name of the temporary C file
	files      []string // all V files that need to be parsed and compiled
	dir        string // directory (or file) being compiled (TODO rename to path?)
	table      *Table // table with types, vars, functions etc
	cgen       *CGen // C code generator
	pref       *Preferences // all the prefrences and settings extracted to a struct for reusability
	lang_dir   string // "~/code/v"
	out_name   string // "program.exe"
	vroot      string
	mod        string  // module being built with -lib 
}

struct Preferences {
mut:
	build_mode     BuildMode
	nofmt          bool // disable vfmt
	is_test        bool // `v test string_test.v`
	is_script      bool // single file mode (`v program.v`), main function can be skipped
	is_live        bool // for hot code reloading
	is_so          bool
	is_prof        bool // benchmark every function
	translated     bool // `v translate doom.v` are we running V code translated from C? allow globals, ++ expressions, etc
	is_prod        bool // use "-O2"
	is_verbose     bool // print extra information with `v.log()`
	obfuscate      bool // `v -obf program.v`, renames functions to "f_XXX"
	is_play        bool // playground mode
	is_repl        bool
	is_run         bool
	show_c_cmd     bool // `v -show_c_cmd` prints the C command to build program.v.c
	sanitize       bool // use Clang's new "-fsanitize" option
	is_debuggable  bool
	is_debug       bool // keep compiled C files
	no_auto_free   bool // `v -nofree` disable automatic `free()` insertion for better performance in some applications  (e.g. compilers) 
	cflags        string // Additional options which will be passed to the C compiler.
	                     // For example, passing -cflags -Os will cause the C compiler to optimize the generated binaries for size.
	                     // You could pass several -cflags XXX arguments. They will be merged with each other.
	                     // You can also quote several options at the same time: -cflags '-Os -fno-inline-small-functions'.
}


fn main() {
	// There's no `flags` module yet, so args have to be parsed manually
	args := env_vflags_and_os_args()
	// Print the version and exit.
	if '-v' in args || '--version' in args || 'version' in args {
		println('V $Version')
		return
	}
	if '-h' in args || '--help' in args || 'help' in args {
		println(HelpText)
		return
	}
	if 'translate' in args {
		println('Translating C to V will be available in V 0.3') 
		return 
	} 
	if 'up' in args {
		update_v() 
		return 
	} 
	if 'get' in args {
		println('use `v install` to install modules from vpm.vlang.io') 
		return 
	} 
	if 'install' in args {
		mod := args.last() 
		if args.len != 3 || mod.len < 2 {
			println('usage: v install [module]')	 
			return 
		} 
		vroot := os.dir(os.executable()) 
		vget := '$vroot/tools/vget' 
		if !os.file_exists(vget) {
			println('Building vget...') 
			os.chdir(vroot + '/tools') 
			vexec := os.args[0] 
			os.exec('$vexec vget.v') 
			println('Done.') 
		} 
		println('Installing module ${mod}...') 
		os.exec('$vget $mod') 
		return 
	} 
	// TODO quit if the compiler is too old 
	// u := os.file_last_mod_unix('v')
	// If there's no tmp path with current version yet, the user must be using a pre-built package
	// Copy the `vlib` directory to the tmp path.
/* 
	// TODO 
	if !os.file_exists(TmpPath) && os.file_exists('vlib') {
	}
*/ 
	// Just fmt and exit
	if 'fmt' in args { 
		file := args.last()
		if !os.file_exists(file) {
			println('"$file" does not exist')
			exit(1)
		}
		if !file.ends_with('.v') {
			println('v fmt can only be used on .v files')
			exit(1)
		}
		println('vfmt is temporarily disabled')
		return
	}
	// v get sqlite 
	if 'get' in args { 
		// Create the modules directory if it's not there. 
		if !os.file_exists(ModPath)  { 
			os.mkdir(ModPath)
		} 
	} 
	// No args? REPL
	if args.len < 2 || (args.len == 2 && args[1] == '-') {
		run_repl()
		return
	}
	// Construct the V object from command line arguments
	mut v := new_v(args)
	if v.pref.is_verbose {
		println(args)
	}
	// Generate the docs and exit
	if 'doc' in args { 
		// v.gen_doc_html_for_module(args.last())
		exit(0)
	}
  
	if 'run' in args {
		vsource := v.dir
		vtarget := final_target_out_name( v.out_name )
		if os.file_exists(vtarget) && ( os.file_last_mod_unix(vsource) <= os.file_last_mod_unix(vtarget) ) {
			//println('ALREADY BUILD FROM vsource: $vsource | vtarget: $vtarget')
			v.run_compiled_executable_and_exit()
		}
		v.compile()
		v.run_compiled_executable_and_exit()
	}
  
	v.compile()
  
	if v.pref.is_test {
		v.run_compiled_executable_and_exit()
	}
  
}

fn (v mut V) compile() {
	mut cgen := v.cgen
	cgen.genln('// Generated by V')
	v.add_v_files_to_compile()
	if v.pref.is_verbose {
		println('all .v files:')
		println(v.files)
	}
	// First pass (declarations)
	for file in v.files {
		mut p := v.new_parser(file, Pass.decl) 
		p.parse()
	}
	// Main pass
	cgen.pass = Pass.main
	if v.pref.is_play {
		cgen.genln('#define VPLAY (1) ')
	}
	cgen.genln('   
#include <stdio.h>  // TODO remove all these includes, define all function signatures and types manually 
#include <stdlib.h>
#include <signal.h>
#include <stdarg.h> // for va_list 
#include <inttypes.h>  // int64_t etc 
#include <string.h> // memcpy 

#define STRUCT_DEFAULT_VALUE {}
#define EMPTY_STRUCT_DECLARATION
#define EMPTY_STRUCT_INIT
#define OPTION_CAST(x) (x)

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

// must be included after <windows.h> 
#include <shellapi.h>

#include <io.h> // _waccess
#include <fcntl.h> // _O_U8TEXT
#include <direct.h> // _wgetcwd
//#include <WinSock2.h>
#ifdef _MSC_VER
// On MSVC these are the same (as long as /volatile:ms is passed)
#define _Atomic volatile

// MSVC can\'t parse some things properly
#undef STRUCT_DEFAULT_VALUE
#define STRUCT_DEFAULT_VALUE {0}
#undef EMPTY_STRUCT_DECLARATION
#define EMPTY_STRUCT_DECLARATION void *____dummy_variable;
#undef EMPTY_STRUCT_INIT
#define EMPTY_STRUCT_INIT 0
#undef OPTION_CAST
#define OPTION_CAST(x)
#endif

void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}

void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}
#else
#include <pthread.h> 
#endif 

//================================== TYPEDEFS ================================*/ 

typedef unsigned char byte;
typedef unsigned int uint;
typedef int64_t i64;
typedef int32_t i32;
typedef int16_t i16;
typedef int8_t i8;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;
typedef uint32_t rune;
typedef float f32;
typedef double f64; 
typedef unsigned char* byteptr;
typedef int* intptr;
typedef void* voidptr;
typedef struct array array;
typedef struct map map;
typedef array array_string; 
typedef array array_int; 
typedef array array_byte; 
typedef array array_uint; 
typedef array array_float; 
typedef array array_f32; 
typedef array array_f64; 
typedef map map_int; 
typedef map map_string; 
#ifndef bool
	typedef int bool;
	#define true 1
	#define false 0
#endif

//============================== HELPER C MACROS =============================*/ 

#define _PUSH(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array__push(arr, &tmp);}
#define _PUSH_MANY(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array__push_many(arr, tmp.data, tmp.len);}
#define _IN(typ, val, arr) array_##typ##_contains(arr, val) 
#define _IN_MAP(val, m) map__exists(m, val) 
#define ALLOC_INIT(type, ...) (type *)memdup((type[]){ __VA_ARGS__ }, sizeof(type)) 

//================================== GLOBALS =================================*/   
//int V_ZERO = 0; 
byteptr g_str_buf; 
int load_so(byteptr);
void reload_so();
void init_consts();')
	
	if v.os != .windows && v.os != .msvc {
		if v.pref.is_so {
			cgen.genln('pthread_mutex_t live_fn_mutex;')
		}  
		if v.pref.is_live {
			cgen.genln('pthread_mutex_t live_fn_mutex = PTHREAD_MUTEX_INITIALIZER;')
		}
	} else {
		if v.pref.is_so {
			cgen.genln('HANDLE live_fn_mutex;')
		}  
		if v.pref.is_live {
			cgen.genln('HANDLE live_fn_mutex = 0;')
		}
	}

	
	imports_json := v.table.imports.contains('json')
	// TODO remove global UI hack
	if v.os == .mac && ((v.pref.build_mode == .embed_vlib && v.table.imports.contains('ui')) ||
	(v.pref.build_mode == .build && v.dir.contains('/ui'))) {
		cgen.genln('id defaultFont = 0; // main.v')
	}
	// TODO remove ugly .c include once V has its own json parser
	// Embed cjson either in embedvlib or in json.o
	if (imports_json && v.pref.build_mode == .embed_vlib) ||
	(v.pref.build_mode == .build && v.out_name.contains('json.o')) {
		//cgen.genln('#include "cJSON.c" ')
	}
	// We need the cjson header for all the json decoding user will do in default mode
	if v.pref.build_mode == .default_mode {
		if imports_json {
			cgen.genln('#include "cJSON.h"')
		}
	}
	if v.pref.build_mode == .embed_vlib || v.pref.build_mode == .default_mode {
		// If we declare these for all modes, then when running `v a.v` we'll get
		// `/usr/bin/ld: multiple definition of 'total_m'`
		// TODO
		//cgen.genln('i64 total_m = 0; // For counting total RAM allocated')
		cgen.genln('int g_test_ok = 1; ')
		if v.table.imports.contains('json') {
			cgen.genln(' 
#define js_get(object, key) cJSON_GetObjectItemCaseSensitive((object), (key))
')
		}
	}
	if os.args.contains('-debug_alloc') {
		cgen.genln('#define DEBUG_ALLOC 1')
	}
	cgen.genln('/*================================== FNS =================================*/')
	cgen.genln('this line will be replaced with definitions')
	defs_pos := cgen.lines.len - 1
	for file in v.files {
		mut p := v.new_parser(file, Pass.main)
		p.parse()
		// p.g.gen_x64()
		// Format all files (don't format automatically generated vlib headers)
		if !v.pref.nofmt && !file.contains('/vlib/') {
			// new vfmt is not ready yet
		}
	}
	v.log('Done parsing.')
	// Write everything
	mut d := strings.new_builder(10000)// Avoid unnecessary allocations
	d.writeln(cgen.includes.join_lines())
	d.writeln(cgen.typedefs.join_lines())
	d.writeln(cgen.types.join_lines())
	d.writeln('\nstring _STR(const char*, ...);\n')
	d.writeln('\nstring _STR_TMP(const char*, ...);\n')
	d.writeln(cgen.fns.join_lines())
	d.writeln(cgen.consts.join_lines())
	d.writeln(cgen.thread_args.join_lines())
	if v.pref.is_prof {
		d.writeln('; // Prof counters:')
		d.writeln(v.prof_counters())
	}
	dd := d.str()
	cgen.lines.set(defs_pos, dd)// TODO `def.str()` doesn't compile
	// if v.build_mode in [.default, .embed_vlib] {
	if v.pref.build_mode == .default_mode || v.pref.build_mode == .embed_vlib {
		mut consts_init_body := cgen.consts_init.join_lines() 
		for imp in v.table.imports {
			if imp == 'http' {
				consts_init_body += '\n http__init_module();'  
			} 
		} 
		// vlib can't have `init_consts()`
		cgen.genln('void init_consts() { 
#ifdef _WIN32\n _setmode(_fileno(stdout), _O_U8TEXT);
SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), ENABLE_PROCESSED_OUTPUT | 0x0004); 
// ENABLE_VIRTUAL_TERMINAL_PROCESSING\n#endif\n g_str_buf=malloc(1000);
$consts_init_body 
}')
		// _STR function can't be defined in vlib
		cgen.genln('
string _STR(const char *fmt, ...) {
	va_list argptr;
	va_start(argptr, fmt);
	size_t len = vsnprintf(0, 0, fmt, argptr) + 1;  
	va_end(argptr);
	byte* buf = malloc(len);  
	va_start(argptr, fmt);
	vsprintf(buf, fmt, argptr);
	va_end(argptr);
#ifdef DEBUG_ALLOC 
	puts("_STR:"); 
	puts(buf); 
#endif 
	return tos2(buf);
}

string _STR_TMP(const char *fmt, ...) {
	va_list argptr;
	va_start(argptr, fmt);
	size_t len = vsnprintf(0, 0, fmt, argptr) + 1;  
	va_end(argptr);
	va_start(argptr, fmt);
	vsprintf(g_str_buf, fmt, argptr);
	va_end(argptr);
#ifdef DEBUG_ALLOC 
	//puts("_STR_TMP:"); 
	//puts(g_str_buf); 
#endif 
	return tos2(g_str_buf);
}

')
	}
	// Make sure the main function exists
	// Obviously we don't need it in libraries
	if v.pref.build_mode != .build {
		if !v.table.main_exists() && !v.pref.is_test {
			// It can be skipped in single file programs
			if v.pref.is_script {
				//println('Generating main()...')
				cgen.genln('int main() { init_consts(); $cgen.fn_main; return 0; }')
			}
			else {
				println('panic: function `main` is undeclared in the main module')
				exit(1) 
			}
		}
		// Generate `main` which calls every single test function
		else if v.pref.is_test {
			cgen.genln('int main() { init_consts();')
			for key, f in v.table.fns { 
				if f.name.starts_with('test_') {
					cgen.genln('$f.name();')
				}
			}
			cgen.genln('return g_test_ok == 0; }')
		}
	}
	// Hot code reloading 
	if v.pref.is_live {
		file := v.dir 
		file_base := v.dir.replace('.v', '') 
		so_name := file_base + '.so' 
		// Need to build .so file before building the live application 
		// The live app needs to load this .so file on initialization. 
		mut vexe := os.args[0]

		if os.user_os() == 'windows' {
			vexe = vexe.replace('\\', '\\\\')
		}

		mut msvc := ''
		if v.os == .msvc {
			msvc = '-os msvc'
		}

		mut debug := ''

		if v.pref.is_debug {
			debug = '-debug'
		}

		os.system('$vexe $msvc $debug -o $file_base -shared $file') 
		cgen.genln('

void lfnmutex_print(char *s){
	if(0){
		fflush(stderr);
		fprintf(stderr,">> live_fn_mutex: %p | %s\\n", &live_fn_mutex, s);
		fflush(stderr);
	}
}
')

		if v.os != .windows && v.os != .msvc {
			cgen.genln('
#include <dlfcn.h>
void* live_lib=0;
int load_so(byteptr path) {
	char cpath[1024];
	sprintf(cpath,"./%s", path);
	//printf("load_so %s\\n", cpath); 
	if (live_lib) dlclose(live_lib);
	live_lib = dlopen(cpath, RTLD_LAZY);
	if (!live_lib) {
		puts("open failed"); 
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
void* live_lib=0;
int load_so(byteptr path) {
	char cpath[1024];
	sprintf(cpath, "./%s", path);
	if (live_lib) FreeLibrary(live_lib);
	live_lib = LoadLibraryA(cpath);
	if (!live_lib) {
		puts("open failed");
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
	char new_so_base[1024];
	char new_so_name[1024];
	char compile_cmd[1024];
	int last = os__file_last_mod_unix(tos2("$file"));
	while (1) {
		// TODO use inotify
		int now = os__file_last_mod_unix(tos2("$file"));
		if (now != last) {
			last = now;
			_live_reloads++;

			//v -o bounce -shared bounce.v
			sprintf(new_so_base, ".tmp.%d.${file_base}", _live_reloads);
			#ifdef _WIN32
			// We have to make this directory becuase windows WILL NOT
			// do it for us
			os__mkdir(string_all_before_last(tos2(new_so_base), tos2("/")));
			#endif
			#ifdef _MSC_VER
			sprintf(new_so_name, "%s.dll", new_so_base);
			#else
			sprintf(new_so_name, "%s.so", new_so_base);
			#endif
			sprintf(compile_cmd, "$vexe $msvc -o %s -shared $file", new_so_base);
			os__system(tos2(compile_cmd));

			if( !os__file_exists(tos2(new_so_name)) ) {
				fprintf(stderr, "Errors while compiling $file\\n");
				continue;        
			}
      
			lfnmutex_print("reload_so locking...");
			pthread_mutex_lock(&live_fn_mutex);
			lfnmutex_print("reload_so locked");
        
			live_lib = 0; // hack: force skipping dlclose/1, the code may be still used...
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
' ) 
	}

	if v.pref.is_so {
		cgen.genln(' int load_so(byteptr path) { return 0; }')
	} 
	cgen.save()
	if v.pref.is_verbose {
		v.log('flags=')
		println(v.table.flags)
	}  
	v.cc()
}

fn final_target_out_name(out_name string) string {
	mut cmd := if out_name.starts_with('/') {
		out_name
	}
	else {
		'./' + out_name
	}
	$if windows {
		cmd = out_name
		cmd = cmd.replace('/', '\\')
	}
	return cmd
}

fn (v V) run_compiled_executable_and_exit() {
	if v.pref.is_verbose {
		println('============ running $v.out_name ============') 
	}	  
	mut cmd := final_target_out_name(v.out_name)
	if os.args.len > 3 {
		cmd += ' ' + os.args.right(3).join(' ')
	}
	if v.pref.is_test {
		ret := os.system(cmd)
		if ret != 0 {
			exit(1)
		}
	}    
	if v.pref.is_run {
		ret := os.system(cmd)
		// TODO: make the runner wrapping as transparent as possible 
		// (i.e. use execve when implemented). For now though, the runner 
		// just returns the same exit code as the child process 
		// (see man system, man 2 waitpid: C macro WEXITSTATUS section)
		exit( ret >> 8 ) 
	}
	exit(0)
}

fn (c mut V) cc_windows_cross() {
       if !c.out_name.ends_with('.exe') {
               c.out_name = c.out_name + '.exe'
       }
       mut args := '-o $c.out_name -w -L. '
       // -I flags
       for flag in c.table.flags {
               if !flag.starts_with('-l') {
                       args += flag
                       args += ' '
               }
       }
       mut libs := ''
       if c.pref.build_mode == .default_mode {
               libs = '"$ModPath/vlib/builtin.o"'
               if !os.file_exists(libs) {
                       println('`builtin.o` not found')
                       exit(1) 
               }
               for imp in c.table.imports {
                       libs += ' "$ModPath/vlib/${imp}.o"'
               }
       }
       args += ' $c.out_name_c '
       // -l flags (libs)
       for flag in c.table.flags {
               if flag.starts_with('-l') {
                       args += flag
                       args += ' '
               }
       }
               println('Cross compiling for Windows...')
               winroot := '$ModPath/winroot' 
	if !os.dir_exists(winroot) {
		winroot_url := 'https://github.com/vlang/v/releases/download/v0.1.10/winroot.zip' 
		println('"$winroot" not found. Download it from $winroot_url and save in $ModPath') 
		exit(1) 
 
} 
               mut obj_name := c.out_name
               obj_name = obj_name.replace('.exe', '')
               obj_name = obj_name.replace('.o.o', '.o')
               include := '-I $winroot/include '
               cmd := 'clang -o $obj_name -w $include -DUNICODE -D_UNICODE -m32 -c -target x86_64-win32 $ModPath/$c.out_name_c'
               if c.pref.show_c_cmd {
                       println(cmd)
               }
               if os.system(cmd) != 0 {
			println('Cross compilation for Windows failed. Make sure you have clang installed.') 
                       exit(1) 
               }
               if c.pref.build_mode != .build {
                       link_cmd := 'lld-link $obj_name $winroot/lib/libcmt.lib ' +
                       '$winroot/lib/libucrt.lib $winroot/lib/kernel32.lib $winroot/lib/libvcruntime.lib ' +
                       '$winroot/lib/uuid.lib'
               if c.pref.show_c_cmd {
		println(link_cmd) 
		} 

                if  os.system(link_cmd)  != 0 { 
			println('Cross compilation for Windows failed. Make sure you have lld linker installed.')  
                       exit(1) 
} 
                       // os.rm(obj_name)
               }
               println('Done!')
}

fn (v mut V) cc() {
	// Cross compiling for Windows 
	if v.os == .windows {
		$if !windows { 
			v.cc_windows_cross()  
			return 
		} 
	} 
	$if windows { 
		if v.os == .msvc {
			v.cc_msvc() 
			return
		}
	} 
	linux_host := os.user_os() == 'linux'
	v.log('cc() isprod=$v.pref.is_prod outname=$v.out_name')
	mut a := [v.pref.cflags, '-std=gnu11', '-w'] // arguments for the C compiler
	flags := v.table.flags.join(' ')
	//mut shared := ''
	if v.pref.is_so {
		a << '-shared -fPIC '// -Wl,-z,defs'
		v.out_name = v.out_name + '.so'
	}
	if v.pref.is_prod {
		a << '-O2'
	}
	else {
		a << '-g'
	}
	if v.pref.is_live || v.pref.is_so {
		// See 'man dlopen', and test running a GUI program compiled with -live
		if (v.os == .linux || os.user_os() == 'linux'){    
			a << '-rdynamic'
		}
		if (v.os == .mac || os.user_os() == 'mac'){
			a << '-flat_namespace'
		}
	}
	mut libs := ''// builtin.o os.o http.o etc
	if v.pref.build_mode == .build {
		a << '-c'
	}
	else if v.pref.build_mode == .embed_vlib {
		// 
	}
	else if v.pref.build_mode == .default_mode {
		libs = '"$ModPath/vlib/builtin.o"'
		if !os.file_exists(libs) {
			println('`builtin.o` not found')
			exit(1)
		}
		for imp in v.table.imports {
			if imp == 'webview' {
				continue
			}
			libs += ' "$ModPath/vlib/${imp}.o"'
		}
	}
	// -I flags
	/* 
mut args := '' 
	for flag in v.table.flags {
		if !flag.starts_with('-l') {
			args += flag
			args += ' '
		}
	}
*/
	if v.pref.sanitize {
		a << '-fsanitize=leak'
	}
	// Cross compiling linux
	sysroot := '/Users/alex/tmp/lld/linuxroot/'
	if v.os == .linux && !linux_host {
		// Build file.o
		a << '-c --sysroot=$sysroot -target x86_64-linux-gnu'
		// Right now `out_name` can be `file`, not `file.o`
		if !v.out_name.ends_with('.o') {
			v.out_name = v.out_name + '.o'
		}
	}
	// Cross compiling windows
	// sysroot := '/Users/alex/tmp/lld/linuxroot/'
	// Output executable name
	// else {
	a << '-o $v.out_name'
	if os.dir_exists(v.out_name) {
		panic('\'$v.out_name\' is a directory')
	}
	// The C file we are compiling
	//a << '"$TmpPath/$v.out_name_c"'
	a << '".$v.out_name_c"'
	// }
	// Min macos version is mandatory I think?
	if v.os == .mac {
		a << '-mmacosx-version-min=10.7'
	}
	a << flags
	a << libs
	// macOS code can include objective C  TODO remove once objective C is replaced with C
	if v.os == .mac {
		a << '-x objective-c'
	}
	// Without these libs compilation will fail on Linux
	// || os.user_os() == 'linux' 
	if v.pref.build_mode != .build && (v.os == .linux || v.os == .freebsd || v.os == .openbsd ||
		v.os == .netbsd || v.os == .dragonfly) { 
		a << '-lm -lpthread ' 
		// -ldl is a Linux only thing. BSDs have it in libc.
		if v.os == .linux {
			a << ' -ldl ' 
		} 
	}
	if v.os == .windows {
		a << '-DUNICODE -D_UNICODE'
	}
	// Find clang executable
	//fast_clang := '/usr/local/Cellar/llvm/8.0.0/bin/clang'
	args := a.join(' ')
	//mut cmd := if os.file_exists(fast_clang) {
	//'$fast_clang $args'
	//}
	//else {
	mut cmd := 'cc $args'
	//}
	$if windows {
		cmd = 'gcc $args' 
	}
	if v.out_name.ends_with('.c') {
		os.mv( '.$v.out_name_c', v.out_name )
		exit(0)
	}
	// Run
	ticks := time.ticks() 
	res := os.exec(cmd)
	diff := time.ticks() - ticks 
	// Print the C command
	if v.pref.show_c_cmd || v.pref.is_verbose {
		println('\n==========')
		println(cmd) 
		println('cc took $diff ms') 
		println('=========\n')
	}
	if res.contains('error: ') {
		if v.pref.is_debug { 
			println(res)
		} else {
			print(res.limit(200)) 
			if res.len > 200 { 
				println('...\n(Use `v -debug` to print the entire error message)\n')   
			} 
		} 
		panic('C error. This should never happen. ' +
			'Please create a GitHub issue: https://github.com/vlang/v/issues/new/choose')
	}
	// Link it if we are cross compiling and need an executable
	if v.os == .linux && !linux_host && v.pref.build_mode != .build {
		v.out_name = v.out_name.replace('.o', '')
		obj_file := v.out_name + '.o'
		println('linux obj_file=$obj_file out_name=$v.out_name')
		ress := os.exec('/usr/local/Cellar/llvm/8.0.0/bin/ld.lld --sysroot=$sysroot ' +
		'-v -o $v.out_name ' +
		'-m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 ' +
		'/usr/lib/x86_64-linux-gnu/crt1.o ' +
		'$sysroot/lib/x86_64-linux-gnu/libm-2.28.a ' +
		'/usr/lib/x86_64-linux-gnu/crti.o ' +
		obj_file +
		' /usr/lib/x86_64-linux-gnu/libc.so ' +
		'/usr/lib/x86_64-linux-gnu/crtn.o')
		println(ress)
		if ress.contains('error:') {
			exit(1)
		}
		println('linux cross compilation done. resulting binary: "$v.out_name"')
	}
	if !v.pref.is_debug && v.out_name_c != 'v.c' && v.out_name_c != 'v_macos.c' {
		os.rm('.$v.out_name_c') 
	} 
}

fn (v &V) v_files_from_dir(dir string) []string {
	mut res := []string
	if !os.file_exists(dir) {
		panic('$dir doesn\'t exist')
	} else if !os.dir_exists(dir) {
		panic('$dir isn\'t a directory')
	}
	mut files := os.ls(dir)
	if v.pref.is_verbose {
		println('v_files_from_dir ("$dir")')
	}
	files.sort()
	for file in files {
		if !file.ends_with('.v') && !file.ends_with('.vh') {
			continue
		}
		if file.ends_with('_test.v') {
			continue
		}
		if file.ends_with('_win.v') && (v.os != .windows && v.os != .msvc) {
			continue
		}
		if file.ends_with('_lin.v') && v.os != .linux { 
			continue
		}
		if file.ends_with('_mac.v') && v.os != .mac { 
			continue
		} 
		if file.ends_with('_nix.v') && (v.os == .windows || v.os == .msvc) {
			continue 
		} 
		res << '$dir/$file'
	}
	return res
}

// Parses imports, adds necessary libs, and then user files
fn (v mut V) add_v_files_to_compile() {
	mut dir := v.dir
	v.log('add_v_files($dir)')
	// Need to store user files separately, because they have to be added after libs, but we dont know
	// which libs need to be added yet
	mut user_files := []string
	// v volt/slack_test.v: compile all .v files to get the environment
	// I need to implement user packages! TODO
	is_test_with_imports := dir.ends_with('_test.v') &&
	(dir.contains('/volt') || dir.contains('/c2volt'))// TODO
	if is_test_with_imports {
		user_files << dir
		pos := dir.last_index('/')
		dir = dir.left(pos) + '/'// TODO WHY IS THIS .neEDED?
	}
	if dir.ends_with('.v') {
		// Just compile one file and get parent dir
		user_files << dir
		dir = dir.all_before('/')
	}
	else {
		// Add files from the dir user is compiling (only .v files)
		files := v.v_files_from_dir(dir)
		for file in files {
			user_files << file
		}
	}
	if user_files.len == 0 {
		println('No input .v files')
		exit(1)
	}
	if v.pref.is_verbose {
		v.log('user_files:')
		println(user_files)
	}
	// Parse builtin imports
	for file in v.files {
		mut p := v.new_parser(file, Pass.imports)
		p.parse()
	}
	// Parse user imports
	for file in user_files {
		mut p := v.new_parser(file, Pass.imports)
		p.parse()
	}
	// Parse lib imports
/* 
	if v.pref.build_mode == .default_mode {
		// strange ( for mod in v.table.imports ) dosent loop all items
		// for mod in v.table.imports {
		for i := 0; i < v.table.imports.len; i++ {
			mod := v.table.imports[i]
			mod_path := v.module_path(mod)
			import_path := '$ModPath/vlib/$mod_path'
			vfiles := v.v_files_from_dir(import_path)
			if vfiles.len == 0 {
				panic('cannot import module $mod (no .v files in "$import_path").')
			}
			// Add all imports referenced by these libs
			for file in vfiles {
				mut p := v.new_parser(file, Pass.imports)
				p.parse()
			}
		}
	}
	else {
*/ 
	// strange ( for mod in v.table.imports ) dosent loop all items
	// for mod in v.table.imports {
	for i := 0; i < v.table.imports.len; i++ {
		mod := v.table.imports[i]
		import_path := v.find_module_path(mod) 
		vfiles := v.v_files_from_dir(import_path)
		if vfiles.len == 0 {
			panic('cannot import module $mod (no .v files in "$import_path").')
		}
		// Add all imports referenced by these libs
		for file in vfiles {
			mut p := v.new_parser(file, Pass.imports)
			p.parse()
		}
	}
	if v.pref.is_verbose {
		v.log('imports:')
		println(v.table.imports)
	}
	// graph deps
	mut dep_graph := new_mod_dep_graph()
	dep_graph.from_import_tables(v.table.file_imports)
	deps_resolved := dep_graph.resolve()
	if !deps_resolved.acyclic {
		deps_resolved.display()
		panic('Import cycle detected.')
	}
	// add imports in correct order
	for mod in deps_resolved.imports() {
		// Building this module? Skip. TODO it's a hack. 
		if mod == v.mod {
			continue 
		} 
		mod_path := v.find_module_path(mod) 
		// If we are in default mode, we don't parse vlib .v files, but header .vh files in
		// TmpPath/vlib
		// These were generated by vfmt
/* 
		if v.pref.build_mode == .default_mode || v.pref.build_mode == .build {
			module_path = '$ModPath/vlib/$mod_p'
		}
*/ 
		vfiles := v.v_files_from_dir(mod_path) 
		for file in vfiles {
			if !file in v.files {
				v.files << file
			}
		}
	}
	// add remaining files (not modules)
	for fit in v.table.file_imports {
		//println('fit $fit.file_path') 
		if !fit.file_path in v.files {
			v.files << fit.file_path
		}
	}
}

fn get_arg(joined_args, arg, def string) string {
	key := '-$arg '
	mut pos := joined_args.index(key)
	if pos == -1 {
		return def
	}
	pos += key.len
	mut space := joined_args.index_after(' ', pos)
	if space == -1 {
		space = joined_args.len
	}
	res := joined_args.substr(pos, space)
	// println('get_arg($arg) = "$res"')
	return res
}

fn (v &V) module_path(mod string) string {
	// submodule support
	if mod.contains('.') {
		//return mod.replace('.', path_sep)
		return mod.replace('.', '/')
	}
	return mod
}

fn (v &V) log(s string) {
	if !v.pref.is_verbose {
		return
	}
	println(s)
}

fn new_v(args[]string) *V {
	mut dir := args.last()
	if args.contains('run') {
		dir = args[2]
	}
	// println('new compiler "$dir"')
	if args.len < 2 {
		dir = ''
	}
	joined_args := args.join(' ')
	target_os := get_arg(joined_args, 'os', '')
	mut out_name := get_arg(joined_args, 'o', 'a.out')
	// build mode
	mut build_mode := BuildMode.default_mode
	mut mod := '' 
	//if args.contains('-lib') {
	if joined_args.contains('build module ') { 
		build_mode = .build 
		// v -lib ~/v/os => os.o
		mod = os.dir(dir) 
		mod = mod.all_after('/')
		println('Building module  "${mod}" dir="$dir"...')
		//out_name = '$TmpPath/vlib/${base}.o'
		out_name = mod + '.o'
		// Cross compiling? Use separate dirs for each os
/* 
		if target_os != os.user_os() {
			os.mkdir('$TmpPath/vlib/$target_os')
			out_name = '$TmpPath/vlib/$target_os/${base}.o'
			println('target_os=$target_os user_os=${os.user_os()}') 
			println('!Cross compiling $out_name')
		}
*/ 
	}
	// TODO embed_vlib is temporarily the default mode. It's much slower.
	else if !args.contains('-embed_vlib') {
		build_mode = .embed_vlib
	}
	// 
	is_test := dir.ends_with('_test.v')
	is_script := dir.ends_with('.v')
	if is_script && !os.file_exists(dir) {
		println('`$dir` does not exist')
		exit(1)
	}
	// No -o provided? foo.v => foo
	if out_name == 'a.out' && dir.ends_with('.v') {
		out_name = dir.left(dir.len - 2)
	}
	// if we are in `/foo` and run `v .`, the executable should be `foo`
	if dir == '.' && out_name == 'a.out' {
		base := os.getwd().all_after('/')
		out_name = base.trim_space()
	}
	mut _os := OS.mac
	// No OS specifed? Use current system
	if target_os == '' {
		$if linux {
			_os = .linux 
		}
		$if mac {
			_os = .mac
		}
		$if windows {
			_os = .windows
		}
		$if freebsd {
			_os = .freebsd 
		}
		$if openbsd {
			_os = .openbsd 
		}
		$if netbsd {
			_os = .netbsd 
		}
		$if dragonfly {
			_os = .dragonfly 
		}
	}
	else {
		switch target_os {
		case 'linux': _os = .linux
		case 'windows': _os = .windows
		case 'mac': _os = .mac
		case 'freebsd': _os = .freebsd 
		case 'openbsd': _os = .openbsd 
		case 'netbsd': _os = .netbsd 
		case 'dragonfly': _os = .dragonfly 
		case 'msvc': _os = .msvc
		}
	}
	builtins := [
	'array.v',
	'string.v',
	'builtin.v',
	'int.v',
	'utf8.v',
	'map.v',
	'option.v',
	]
	// Location of all vlib files
	vroot := os.dir(os.executable()) 
	//println('VROOT=$vroot') 
	// v.exe's parent directory should contain vlib 
	if os.dir_exists(vroot) && os.dir_exists(vroot + '/vlib/builtin') {
 
	}  else {
		println('vlib not found. It should be next to the V executable. ')  
		println('Go to https://vlang.io to install V.') 
		exit(1) 
	} 
	mut out_name_c := out_name.all_after('/') + '.c'
	mut files := []string
	// Add builtin files
	if !out_name.contains('builtin.o') {
		for builtin in builtins {
			mut f := '$vroot/vlib/builtin/$builtin'
			// In default mode we use precompiled vlib.o, point to .vh files with signatures
			if build_mode == .default_mode || build_mode == .build {
				//f = '$TmpPath/vlib/builtin/${builtin}h'
			}
			files << f
		}
	}

	mut cflags := ''
	for ci, cv in args {
		if cv == '-cflags' {
			cflags += args[ci+1] + ' '
		}
	}

	obfuscate := args.contains('-obf')
	pref := &Preferences {
		is_test: is_test
		is_script: is_script
		is_so: args.contains('-shared')
		is_play: args.contains('play')
		is_prod: args.contains('-prod')
		is_verbose: args.contains('-verbose')
		is_debuggable: args.contains('-g') // -debuggable implys debug
		is_debug: args.contains('-debug') || args.contains('-g')
		obfuscate: obfuscate
		is_prof: args.contains('-prof')
		is_live: args.contains('-live')
		sanitize: args.contains('-sanitize')
		nofmt: args.contains('-nofmt')
		show_c_cmd: args.contains('-show_c_cmd')
		translated: args.contains('translated')
		is_run: args.contains('run')
		is_repl: args.contains('-repl')
		build_mode: build_mode
		cflags: cflags
	}  

	if pref.is_so {
		out_name_c = out_name.all_after('/') + '_shared_lib.c'
	}

	return &V {
		os: _os
		out_name: out_name
		files: files
		dir: dir
		lang_dir: vroot 
		table: new_table(obfuscate)
		out_name: out_name
		out_name_c: out_name_c
		cgen: new_cgen(out_name_c)
		vroot: vroot 
		pref: pref
		mod: mod 
	}
}

fn run_repl() []string {
	println('REPL is temporarily disabled, sorry') 
	exit(1) 
	println('V $Version')
	println('Use Ctrl-C or `exit` to exit')
	file := '.vrepl.v'
	temp_file := '.vrepl_temp.v'
	defer {
		os.rm(file) 
		os.rm(temp_file) 
	} 
	mut lines := []string
	vexe := os.args[0] 
	for {
		print('>>> ')
		mut line := os.get_raw_line()
		if line.trim_space() == '' && line.ends_with('\n') {
			continue
		}
		line = line.trim_space()
		if line == '' || line == 'exit' {
			break
		}
		// Save the source only if the user is printing something,
		// but don't add this print call to the `lines` array,
		// so that it doesn't get called during the next print.
		if line.starts_with('print') {
			source_code := lines.join('\n') + '\n' + line 
			os.write_file(file, source_code)
			s := os.exec('$vexe run $file -repl')
			mut vals := s.split('\n')
			if s.contains('panic: ') {
				if !s.contains('declared and not used') 	{
					for i:=1; i<vals.len; i++ {
						println(vals[i])
					} 
				}
				else {
					println(s)
				}
			}
			else {
				for i:=0; i < vals.len; i++ {
					println(vals[i])
				}
			}
		}
		else {
			mut temp_line := line
			mut temp_flag := false
			if !(line.contains(' ') || line.contains(':') || line.contains('=') || line.contains(',') ){
				temp_line = 'println($line)'
				temp_flag = true
			}
			temp_source_code := lines.join('\n') + '\n' + temp_line
			os.write_file(temp_file, temp_source_code)
			s := os.exec('$vexe run $temp_file -repl')
			if s.contains('panic: ') {
				if !s.contains('declared and not used') 	{
					mut vals := s.split('\n')
					for i:=0; i < vals.len; i++ {
						println(vals[i])
					} 
				}
				else {
					lines << line
				}
			}
			else {
				lines << line
				vals := s.split('\n')
				for i:=0; i<vals.len-1; i++ {
					println(vals[i])
				} 
			}
		}
	}
	return lines
}

const (
	HelpText = '
Usage: v [options] [file | directory]

Options:
  -                 Read from stdin (Default; Interactive mode if in a tty)
  -h, help          Display this information.
  -v, version       Display compiler version.
  -lib              Generate object file.
  -prod             Build an optimized executable.
  -o <file>         Place output into <file>.
  -obf              Obfuscate the resulting binary.
  -show_c_cmd       Print the full C compilation command and how much time it took. 
  -debug            Leave a C file for debugging in .program.c. 
  -live             Enable hot code reloading (required by functions marked with [live]). 
  fmt               Run vfmt to format the source code. 
  up                Update V. 
  run               Build and execute a V program. You can add arguments after the file name.


Files:
  <file>_test.v     Test file.
'
)

/* 
- To disable automatic formatting: 
v -nofmt file.v

- To build a program with an embedded vlib  (use this if you do not have prebuilt vlib libraries or if you
are working on vlib) 
v -embed_vlib file.v 
*/

fn env_vflags_and_os_args() []string {
   mut args := []string
   vflags := os.getenv('VFLAGS')
   if '' != vflags {
     args << os.args[0]
     args << vflags.split(' ')
     if os.args.len > 1 {
       args << os.args.right(1)
     }
   }else{
     args << os.args
   }
   return args
}

fn update_v() {
	println('Updating V...') 
	vroot := os.dir(os.executable()) 
	mut s := os.exec('git -C "$vroot" pull --rebase origin master') 
	println(s) 
	$if windows { 
		os.mv('$vroot/v.exe', '$vroot/v_old.exe') 
		s = os.exec('$vroot/make.bat') 
		println(s) 
	} $else { 
		s = os.exec('make -C "$vroot"') 
		println(s) 
	} 
} 

