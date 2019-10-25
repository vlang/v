// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import (
	os
	strings
)

pub const (
	Version = '0.1.21'
)

enum BuildMode {
	// `v program.v'
	// Build user code only, and add pre-compiled vlib (`cc program.o builtin.o os.o...`)
	default_mode
	// `v -lib ~/v/os`
	// build any module (generate os.o + os.vh)
	build_module
}

const (
	supported_platforms = ['windows', 'mac', 'linux', 'freebsd', 'openbsd',
		'netbsd', 'dragonfly', 'android', 'js', 'solaris']
)

enum OS {
	mac
	linux
	windows
	freebsd
	openbsd
	netbsd
	dragonfly
	js   // TODO
	android
	solaris
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
pub mut:
	os         OS           // the OS to build for
	out_name_c string       // name of the temporary C file
	files      []string     // all V files that need to be parsed and compiled
	dir        string       // directory (or file) being compiled (TODO rename to path?)
	table      &Table       // table with types, vars, functions etc
	cgen       &CGen        // C code generator
	pref       &Preferences // all the preferences and settings extracted to a struct for reusability
	lang_dir   string       // "~/code/v"
	out_name   string       // "program.exe"
	vroot      string
	mod        string       // module being built with -lib
	parsers    []Parser     // file parsers
	vgen_buf   strings.Builder // temporary buffer for generated V code (.str() etc)
	file_parser_idx map[string]int // map absolute file path to v.parsers index
	cached_mods []string
}

struct Preferences {
pub mut:
	build_mode    BuildMode
	nofmt         bool   // disable vfmt
	is_test       bool   // `v test string_test.v`
	is_script     bool   // single file mode (`v program.v`), main function can be skipped
	is_live       bool   // for hot code reloading
	is_so         bool
	is_prof       bool   // benchmark every function
	translated    bool   // `v translate doom.v` are we running V code translated from C? allow globals, ++ expressions, etc
	is_prod       bool   // use "-O2"
	is_verbose    bool   // print extra information with `v.log()`
	obfuscate     bool   // `v -obf program.v`, renames functions to "f_XXX"
	is_repl       bool
	is_run        bool
	show_c_cmd    bool   // `v -show_c_cmd` prints the C command to build program.v.c
	sanitize      bool   // use Clang's new "-fsanitize" option

	is_debug      bool   // false by default, turned on by -g or -cg, it tells v to pass -g to the C backend compiler.
	is_vlines     bool   // turned on by -g, false by default (it slows down .tmp.c generation slightly).
	is_keep_c     bool   // -keep_c , tell v to leave the generated .tmp.c alone (since by default v will delete them after c backend finishes)
	// NB: passing -cg instead of -g will set is_vlines to false and is_g to true, thus making v generate cleaner C files,
	// which are sometimes easier to debug / inspect manually than the .tmp.c files by plain -g (when/if v line number generation breaks).
	is_cache      bool   // turns on v usage of the module cache to speed up compilation.

	is_stats      bool   // `v -stats file_test.v` will produce more detailed statistics for the tests that were run
	no_auto_free  bool   // `v -nofree` disable automatic `free()` insertion for better performance in some applications  (e.g. compilers)
	cflags        string // Additional options which will be passed to the C compiler.
						 // For example, passing -cflags -Os will cause the C compiler to optimize the generated binaries for size.
						 // You could pass several -cflags XXX arguments. They will be merged with each other.
						 // You can also quote several options at the same time: -cflags '-Os -fno-inline-small-functions'.
	ccompiler     string // the name of the used C compiler
	building_v    bool
	autofree      bool
	compress      bool
	//skip_builtin  bool   // Skips re-compilation of the builtin module
						 // to increase compilation time.
						 // This is on by default, since a vast majority of users do not
						 // work on the builtin module itself.
	//generating_vh bool
}

// Should be called by main at the end of the compilation process, to cleanup
pub fn (v mut V) finalize_compilation(){
	// TODO remove
	if v.pref.autofree {
		/*
		println('started freeing v struct')
		v.table.typesmap.free()
		v.table.obf_ids.free()
		v.cgen.lines.free()
		free(v.cgen)
		for _, f in v.table.fns {
			//f.local_vars.free()
			f.args.free()
			//f.defer_text.free()
		}
		v.table.fns.free()
		free(v.table)
		//for p in parsers {}
		println('done!')
		*/
	}
}

pub fn (v mut V) add_parser(parser Parser) int {
	   v.parsers << parser
	   pidx := v.parsers.len-1
	   v.file_parser_idx[os.realpath(parser.file_path)] = pidx
	   return pidx
}

pub fn (v &V) get_file_parser_index(file string) ?int {
	file_path := os.realpath(file)
	if file_path in v.file_parser_idx {
		return v.file_parser_idx[file_path]
	}
	return error('parser for "$file" not found')
}

// find existing parser or create new one. returns v.parsers index
pub fn (v mut V) parse(file string, pass Pass) int {
	//println('parse($file, $pass)')
	pidx := v.get_file_parser_index(file) or {
		mut p := v.new_parser_from_file(file)
		p.parse(pass)
		//if p.pref.autofree {		p.scanner.text.free()		free(p.scanner)	}
		return v.add_parser(p)
	}
	// println('matched ' + v.parsers[pidx].file_path + ' with $file')
	v.parsers[pidx].parse(pass)
	//if v.parsers[i].pref.autofree {	v.parsers[i].scanner.text.free()	free(v.parsers[i].scanner)	}
	return pidx
}


pub fn (v mut V) compile() {
	// Emily: Stop people on linux from being able to build with msvc
	if os.user_os() != 'windows' && v.pref.ccompiler == 'msvc' {
		verror('Cannot build with msvc on ${os.user_os()}')
	}
	mut cgen := v.cgen
	cgen.genln('// Generated by V')
	if v.pref.is_verbose {
		println('all .v files before:')
		println(v.files)
	}
	v.add_v_files_to_compile()
	if v.pref.is_verbose || v.pref.is_debug {
		println('all .v files:')
		println(v.files)
	}
	/*
	if v.pref.is_debug {
		println('\nparsers:')
		for q in v.parsers {
			println(q.file_name)
		}
		println('\nfiles:')
		for q in v.files {
			println(q)
		}
	}
	*/
	// First pass (declarations)
	for file in v.files {
		v.parse(file, .decl)
	}

	// Main pass
	cgen.pass = Pass.main
	if v.pref.is_debug {
		$if js {
			cgen.genln('const VDEBUG = 1;\n')
		}	$else {
			cgen.genln('#define VDEBUG (1)')
		}
	}
	if v.os == .js {
		cgen.genln('#define _VJS (1) ')
	}

	v_hash := vhash()
	$if js {
		cgen.genln('const V_COMMIT_HASH = "$v_hash";\n')
	} $else {
		cgen.genln('#ifndef V_COMMIT_HASH')
		cgen.genln('#define V_COMMIT_HASH "$v_hash"')
		cgen.genln('#endif')
	}

	q := cgen.nogen // TODO hack
	cgen.nogen = false
	$if js {
		cgen.genln(js_headers)
	} $else {
		cgen.genln(CommonCHeaders)
	}
	v.generate_hotcode_reloading_declarations()
	// We need the cjson header for all the json decoding that will be done in
	// default mode
	imports_json := 'json' in v.table.imports
	if v.pref.build_mode == .default_mode {
		if imports_json {
			cgen.genln('#include "cJSON.h"')
		}
	}
	if v.pref.build_mode == .default_mode {
		// If we declare these for all modes, then when running `v a.v` we'll get
		// `/usr/bin/ld: multiple definition of 'total_m'`
		$if !js {
			cgen.genln('int g_test_oks = 0;')
			cgen.genln('int g_test_fails = 0;')
		}
		if imports_json {
			cgen.genln('
#define js_get(object, key) cJSON_GetObjectItemCaseSensitive((object), (key))
')
		}
	}
	if '-debug_alloc' in os.args {
		cgen.genln('#define DEBUG_ALLOC 1')
	}
	//cgen.genln('/*================================== FNS =================================*/')
	cgen.genln('// this line will be replaced with definitions')
	mut defs_pos := cgen.lines.len - 1
	if defs_pos == -1 {
		defs_pos = 0
	}
	cgen.nogen = q
	for file in v.files {
		v.parse(file, .main)
		//if p.pref.autofree {		p.scanner.text.free()		free(p.scanner)	}
		// Format all files (don't format automatically generated vlib headers)
		if !v.pref.nofmt && !file.contains('/vlib/') {
			// new vfmt is not ready yet
		}
	}
	// Generate .vh if we are building a module
	if v.pref.build_mode == .build_module {
		generate_vh(v.dir)
	}

	// parse generated V code (str() methods etc)
	mut vgen_parser := v.new_parser_from_string(v.vgen_buf.str())
	// free the string builder which held the generated methods
	vgen_parser.is_vgen = true 
	v.vgen_buf.free()
	vgen_parser.parse(.main)
	// v.parsers.add(vgen_parser)

	// All definitions
	mut def := strings.new_builder(10000)// Avoid unnecessary allocations
	$if !js {
		def.writeln(cgen.includes.join_lines())
		def.writeln(cgen.typedefs.join_lines())
		def.writeln(v.type_definitions())
		def.writeln('\nstring _STR(const char*, ...);\n')
		def.writeln('\nstring _STR_TMP(const char*, ...);\n')
		def.writeln(cgen.fns.join_lines()) // fn definitions
	} $else {
		def.writeln(v.type_definitions())
	}
	def.writeln(cgen.consts.join_lines())
	def.writeln(cgen.thread_args.join_lines())
	if v.pref.is_prof {
		def.writeln('; // Prof counters:')
		def.writeln(v.prof_counters())
	}
	cgen.lines[defs_pos] = def.str()
	v.generate_init()
	v.generate_main()
	v.generate_hot_reload_code()
	if v.pref.is_verbose {
		v.log('flags=')
		for flag in v.get_os_cflags() {
			println(' * ' + flag.format())
		}
	}
	$if js {
		cgen.genln('main__main();')
	}
	cgen.save()
	v.cc()
}

fn (v mut V) generate_init() {
	$if js { return }
	if v.pref.build_mode == .build_module {
		nogen := v.cgen.nogen
		v.cgen.nogen = false
		consts_init_body := v.cgen.consts_init.join_lines()
		init_fn_name := mod_gen_name(v.mod) + '__init_consts'
		v.cgen.genln('void ${init_fn_name}();\nvoid ${init_fn_name}() {\n$consts_init_body\n}')
		v.cgen.nogen = nogen
	}
	if v.pref.build_mode == .default_mode {
		mut call_mod_init := ''
		mut call_mod_init_consts := ''
		if 'builtin' in v.cached_mods {
			v.cgen.genln('void builtin__init_consts();')
			call_mod_init_consts += 'builtin__init_consts();\n'
		}
		for mod in v.table.imports {
			init_fn_name := mod_gen_name(mod) + '__init'
			if v.table.known_fn(init_fn_name) {
				call_mod_init += '${init_fn_name}();\n'
			}
			if mod in v.cached_mods {
				v.cgen.genln('void ${init_fn_name}_consts();')
				call_mod_init_consts += '${init_fn_name}_consts();\n'
			}
		}
		consts_init_body := v.cgen.consts_init.join_lines()
		// vlib can't have `init_consts()`
		v.cgen.genln('void init() {
g_str_buf=malloc(1000);
$call_mod_init_consts
$consts_init_body
builtin__init();
$call_mod_init
}')
		// _STR function can't be defined in vlib
		v.cgen.genln('
string _STR(const char *fmt, ...) {
	va_list argptr;
	va_start(argptr, fmt);
	size_t len = vsnprintf(0, 0, fmt, argptr) + 1;
	va_end(argptr);
	byte* buf = malloc(len);
	va_start(argptr, fmt);
	vsprintf((char *)buf, fmt, argptr);
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
	//size_t len = vsnprintf(0, 0, fmt, argptr) + 1;
	va_end(argptr);
	va_start(argptr, fmt);
	vsprintf((char *)g_str_buf, fmt, argptr);
	va_end(argptr);
#ifdef DEBUG_ALLOC
	//puts("_STR_TMP:");
	//puts(g_str_buf);
#endif
	return tos2(g_str_buf);
}

')
	}
}

pub fn (v mut V) generate_main() {
	mut cgen := v.cgen
	$if js { return }

	if v.pref.is_vlines {
		///// After this point, the v files are compiled.
		///// The rest is auto generated code, which will not have
		///// different .v source file/line numbers.
		lines_so_far := cgen.lines.join('\n').count('\n') + 5
		cgen.genln('')
		cgen.genln('////////////////// Reset the file/line numbers //////////')
		cgen.lines << '#line $lines_so_far "${cescaped_path(os.realpath(cgen.out_path))}"'
		cgen.genln('')
	}

	// Make sure the main function exists
	// Obviously we don't need it in libraries
	if v.pref.build_mode != .build_module {
		if !v.table.main_exists() && !v.pref.is_test {
			// It can be skipped in single file programs
			if v.pref.is_script {
				//println('Generating main()...')
				v.gen_main_start(true)
				cgen.genln('$cgen.fn_main;')
				v.gen_main_end('return 0')
			}
			else {
				verror('function `main` is not declared in the main module')
			}
		}
		else if v.pref.is_test {
			if v.table.main_exists() {
				verror('test files cannot have function `main`')
			}
			if !v.table.has_at_least_one_test_fn() {
				verror('test files need to have at least one test function')
			}
			// Generate a C `main`, which calls every single test function
			v.gen_main_start(false)

			if v.pref.is_stats { cgen.genln('BenchedTests bt = main__start_testing();') }

			for _, f in v.table.fns {
				if f.name.starts_with('main__test_') {
					if v.pref.is_stats { cgen.genln('BenchedTests_testing_step_start(&bt, tos3("$f.name"));') }
					cgen.genln('$f.name();')
					if v.pref.is_stats { cgen.genln('BenchedTests_testing_step_end(&bt);') }
				}
			}
			if v.pref.is_stats { cgen.genln('BenchedTests_end_testing(&bt);') }
			v.gen_main_end('return g_test_fails > 0')
		}
		else if v.table.main_exists() {
			v.gen_main_start(true)
			cgen.genln('  main__main();')
			v.gen_main_end('return 0')
		}
	}
}

pub fn (v mut V) gen_main_start(add_os_args bool){
	v.cgen.genln('int main(int argc, char** argv) { ')
	v.cgen.genln('  init();')
	if add_os_args && 'os' in v.table.imports {
		v.cgen.genln('  os__args = os__init_os_args(argc, (byteptr*)argv);')
	}
	v.generate_hotcode_reloading_main_caller()
	v.cgen.genln('')
}
pub fn (v mut V) gen_main_end(return_statement string){
	v.cgen.genln('')
	v.cgen.genln('  $return_statement;')
	v.cgen.genln('}')
}

pub fn final_target_out_name(out_name string) string {
	$if windows {
		return out_name.replace('/', '\\') + '.exe'
	}
	return if out_name.starts_with('/') {
		out_name
	}
	else {
		'./' + out_name
	}
}

pub fn (v V) run_compiled_executable_and_exit() {
	args := env_vflags_and_os_args()
	
	if v.pref.is_verbose {
		println('============ running $v.out_name ============')
	}
	mut cmd := '"' + final_target_out_name(v.out_name).replace('.exe','') + '"'
	
	mut args_after := ' '
	for i,a in args {
		if i == 0 { continue }
		if a.starts_with('-') { continue }
		if a in ['run','test'] {
			args_after += args.right(i+2).join(' ')
			break
		}
	}
	cmd += args_after
	
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
		// just returns the same exit code as the child process.
		exit( ret )
	}
	exit(0)
}

pub fn (v &V) v_files_from_dir(dir string) []string {
	mut res := []string
	if !os.file_exists(dir) {
		if dir == 'compiler' && os.dir_exists('vlib') {
			println('looks like you are trying to build V with an old command')
			println('use `v v.v` instead of `v -o v compiler`')
		}	
		verror("$dir doesn't exist")
	} else if !os.dir_exists(dir) {
		verror("$dir isn't a directory")
	}
	mut files := os.ls(dir) or { panic(err) }
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
		if (file.ends_with('_win.v') || file.ends_with('_windows.v')) && v.os != .windows {
			continue
		}
		if (file.ends_with('_lin.v') || file.ends_with('_linux.v')) && v.os != .linux {
			continue
		}
		if (file.ends_with('_mac.v') || file.ends_with('_darwin.v')) && v.os != .mac {
			continue
		}
		if file.ends_with('_nix.v') && v.os == .windows {
			continue
		}
		if file.ends_with('_js.v') && v.os != .js {
			continue
		}
		if file.ends_with('_c.v') && v.os == .js {
			continue
		}
		res << '$dir${os.path_separator}$file'
	}
	return res
}

// Parses imports, adds necessary libs, and then user files
pub fn (v mut V) add_v_files_to_compile() {
	mut builtin_files := v.get_builtin_files()
	// Builtin cache exists? Use it.
	builtin_vh := '$v_modules_path${os.path_separator}vlib${os.path_separator}builtin.vh'
	if v.pref.is_cache && os.file_exists(builtin_vh) {
		v.cached_mods << 'builtin'
		builtin_files = [builtin_vh]
	}
	// Parse builtin imports
	for file in builtin_files {
		// add builtins first
		v.files << file
		mut p := v.new_parser_from_file(file)
		p.parse(.imports)
		//if p.pref.autofree {		p.scanner.text.free()		free(p.scanner)	}
		v.add_parser(p)
	}
	// Parse user imports
	for file in v.get_user_files() {
		mut p := v.new_parser_from_file(file)
		p.parse(.imports)
		if p.v_script {
			v.log('imports0:')
			println(v.table.imports)
			println(v.files)
			p.register_import('os', 0)
			p.table.imports << 'os'
			p.table.register_module('os')
		}	
		//if p.pref.autofree {		p.scanner.text.free()		free(p.scanner)	}
		v.add_parser(p)
	}
	// Parse lib imports
	v.parse_lib_imports()
	if v.pref.is_verbose {
		v.log('imports:')
		println(v.table.imports)
	}
	// resolve deps and add imports in correct order
	imported_mods := v.resolve_deps().imports()
	for mod in imported_mods {
		if mod == 'builtin' || mod == 'main' {
			// builtin already added
			// main files will get added last
			continue
		}
		// use cached built module if exists
		if v.pref.build_mode != .build_module && !mod.contains('vweb') {
			mod_path := mod.replace('.', os.path_separator)
			vh_path := '$v_modules_path${os.path_separator}vlib${os.path_separator}${mod_path}.vh'
			if v.pref.is_cache && os.file_exists(vh_path) {
				eprintln('using cached module `$mod`: $vh_path')
				v.cached_mods << mod
				v.files << vh_path
				continue
			}
		}
		// standard module
		vfiles := v.get_imported_module_files(mod)
		for file in vfiles {
			v.files << file
		}
	}
	// add remaining main files last
	for p in v.parsers {
		if p.mod != 'main' { continue }
		v.files << p.file_path
	}
}

pub fn (v &V) get_builtin_files() []string {
	// .vh cache exists? Use it
	$if js {
		return v.v_files_from_dir('$v.vroot${os.path_separator}vlib${os.path_separator}builtin${os.path_separator}js')
	}
	return v.v_files_from_dir('$v.vroot${os.path_separator}vlib${os.path_separator}builtin')
}

// get user files
pub fn (v &V)  get_user_files() []string {
	mut dir := v.dir
	v.log('get_v_files($dir)')
	// Need to store user files separately, because they have to be added after
	// libs, but we dont know	which libs need to be added yet
	mut user_files := []string

	if v.pref.is_test && v.pref.is_stats {
		user_files << os.join(v.vroot, 'vlib', 'benchmark', 'tests',
			'always_imported.v')
	}

	// v volt/slack_test.v: compile all .v files to get the environment
	// I need to implement user packages! TODO
	is_test_with_imports := dir.ends_with('_test.v') &&
	(dir.contains('${os.path_separator}volt') || dir.contains('${os.path_separator}c2volt'))// TODO
	if is_test_with_imports {
		user_files << dir
		pos := dir.last_index(os.path_separator)
		dir = dir.left(pos) + os.path_separator// TODO why is this needed
	}
	if dir.ends_with('.v') || dir.ends_with('.vsh') {
		// Just compile one file and get parent dir
		user_files << dir
		dir = dir.all_before(os.path_separator)
	}
	else {
		// Add .v files from the directory being compiled
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
	return user_files
}

// get module files from already parsed imports
fn (v &V) get_imported_module_files(mod string) []string {
	mut files := []string
	for p in v.parsers {
		if p.mod == mod {
			files << p.file_path
		}
	}
	return files
}

// parse deps from already parsed builtin/user files
pub fn (v mut V) parse_lib_imports() {
	mut done_imports := []string
	for i in 0..v.parsers.len {
		for _, mod in v.parsers[i].import_table.imports {
			if mod in done_imports { continue }
			import_path := v.find_module_path(mod) or {
				v.parsers[i].error_with_token_index(
					'cannot import module "$mod" (not found)',
					v.parsers[i].import_table.get_import_tok_idx(mod))
				break
			}
			vfiles := v.v_files_from_dir(import_path)
			if vfiles.len == 0 {
				v.parsers[i].error_with_token_index(
					'cannot import module "$mod" (no .v files in "$import_path")',
					v.parsers[i].import_table.get_import_tok_idx(mod))
			}
			// Add all imports referenced by these libs
			for file in vfiles {
				pidx := v.parse(file, .imports)
				p_mod := v.parsers[pidx].mod
				if p_mod != mod {
					v.parsers[pidx].error_with_token_index(
						'bad module definition: ${v.parsers[pidx].file_path} imports module "$mod" but $file is defined as module `$p_mod`', 1)
				}
			}
			done_imports << mod
		}
	}
}

pub fn get_arg(joined_args, arg, def string) string {
	return get_param_after(joined_args, '-$arg', def)
}

pub fn get_param_after(joined_args, arg, def string) string {
	key := '$arg '
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
	return res
}

pub fn (v &V) log(s string) {
	if !v.pref.is_verbose {
		return
	}
	println(s)
}

pub fn new_v(args[]string) &V {
	// Create modules dirs if they are missing
	if !os.dir_exists(v_modules_path) {
		os.mkdir(v_modules_path)
		os.mkdir('$v_modules_path${os.path_separator}cache')
	}

	mut vgen_buf := strings.new_builder(1000)
	vgen_buf.writeln('module main\nimport strings')

	joined_args := args.join(' ')
	target_os := get_arg(joined_args, 'os', '')
	mut out_name := get_arg(joined_args, 'o', 'a.out')

	mut dir := args.last()
	if 'run' in args {
		dir = get_param_after(joined_args, 'run', '')
	}
	if dir.ends_with(os.path_separator) {
		dir = dir.all_before_last(os.path_separator)
	}
	if dir.starts_with('.$os.path_separator') {
		dir = dir.right(2)
	}
	if args.len < 2 {
		dir = ''
	}
	// build mode
	mut build_mode := BuildMode.default_mode
	mut mod := ''
	if joined_args.contains('build module ') {
		build_mode = .build_module
		// v build module ~/v/os => os.o
		mod_path := if dir.contains('vlib') {
			dir.all_after('vlib'+os.path_separator)
		}
		else if dir.starts_with('.\\') || dir.starts_with('./') {
			dir.right(2)
		}
		else if dir.starts_with(os.path_separator) {
			dir.all_after(os.path_separator)
		} else {
			dir
		}
		mod = mod_path.replace(os.path_separator, '.')
		println('Building module "${mod}" (dir="$dir")...')
		//out_name = '$TmpPath/vlib/${base}.o'
		out_name = mod
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
	is_test := dir.ends_with('_test.v')
	is_script := dir.ends_with('.v') || dir.ends_with('.vsh')
	if is_script && !os.file_exists(dir) {
		println('`$dir` does not exist')
		exit(1)
	}
	// No -o provided? foo.v => foo
	if out_name == 'a.out' && dir.ends_with('.v') && dir != '.v' {
		out_name = dir.left(dir.len - 2)
		// Building V? Use v2, since we can't overwrite a running
		// executable on Windows + the precompiled V is more
		// optimized.
		if out_name == 'v' && os.dir_exists('vlib/compiler') {
			println('Saving the resulting V executable in `./v2`')
			println('Use `v -o v v.v` if you want to replace current '+
				'V executable.')
			out_name = 'v2'
		}
	}
	// if we are in `/foo` and run `v .`, the executable should be `foo`
	if dir == '.' && out_name == 'a.out' {
		base := os.getwd().all_after(os.path_separator)
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
		$if solaris {
			_os = .solaris
		}
	}
	else {
		_os = os_from_string(target_os)
	}
	// Location of all vlib files
	vroot := os.dir(vexe_path())
	//println('VROOT=$vroot')
	// v.exe's parent directory should contain vlib
	if !os.dir_exists(vroot) || !os.dir_exists(vroot + '/vlib/builtin') {
		println('vlib not found, downloading it...')
		/*
		ret := os.system('git clone --depth=1 https://github.com/vlang/v .')
		if ret != 0 {
			println('failed to `git clone` vlib')
			println('make sure you are online and have git installed')
			exit(1)
		}
		*/
		println('vlib not found. It should be next to the V executable. ')
		println('Go to https://vlang.io to install V.')
		exit(1)
	}
	// println('out_name:$out_name')
	mut out_name_c := os.realpath('${out_name}.tmp.c')

	cflags := get_cmdline_cflags(args)

	rdir := os.realpath( dir )
	rdir_name := os.filename( rdir )

	obfuscate := '-obf' in args
	is_repl := '-repl' in args
	pref := &Preferences {
		is_test: is_test
		is_script: is_script
		is_so: '-shared' in args
		is_prod: '-prod' in args
		is_verbose: '-verbose' in args || '--verbose' in args

		is_debug:      '-g' in args || '-cg' in args
		is_vlines:     '-g' in args && !('-cg' in args)
		is_keep_c:     '-keep_c' in args
		is_cache:      '-cache' in args

		is_stats: '-stats' in args
		obfuscate: obfuscate
		is_prof: '-prof' in args
		is_live: '-live' in args
		sanitize: '-sanitize' in args
		nofmt: '-nofmt' in args
		show_c_cmd: '-show_c_cmd' in args
		translated: 'translated' in args
		is_run: 'run' in args
		autofree: '-autofree' in args
		compress: '-compress' in args
		is_repl: is_repl
		build_mode: build_mode
		cflags: cflags
		ccompiler: find_c_compiler()
		building_v: !is_repl && (rdir_name == 'compiler' || rdir_name == 'v.v'  || dir.contains('vlib'))
	}
	if pref.is_verbose || pref.is_debug {
		println('C compiler=$pref.ccompiler')
	}
	if pref.is_so {
		out_name_c = out_name.all_after(os.path_separator) + '_shared_lib.c'
	}
	return &V{
		os: _os
		out_name: out_name
		dir: dir
		lang_dir: vroot
		table: new_table(obfuscate)
		out_name_c: out_name_c
		cgen: new_cgen(out_name_c)
		vroot: vroot
		pref: pref
		mod: mod
		vgen_buf: vgen_buf
	}
}

pub fn env_vflags_and_os_args() []string {
	vosargs := os.getenv('VOSARGS')
	if '' != vosargs { return vosargs.split(' ') }

	mut args := []string
	vflags := os.getenv('VFLAGS')
	if '' != vflags {
		args << os.args[0]
		args << vflags.split(' ')
		if os.args.len > 1 {
			args << os.args.right(1)
		}
	} else{
		args << os.args
	}
	return args
}

pub fn update_v() {
	println('Updating V...')
	vroot := os.dir(vexe_path())
	s := os.exec('git -C "$vroot" pull --rebase origin master') or {
		verror(err)
		return
	}
	println(s.output)
	$if windows {
		v_backup_file := '$vroot/v_old.exe'
		if os.file_exists( v_backup_file ) {
			os.rm( v_backup_file )
		}
		os.mv('$vroot/v.exe', v_backup_file)
		s2 := os.exec('"$vroot/make.bat"') or {
			verror(err)
			return
		}
		println(s2.output)
	} $else {
		s2 := os.exec('make -C "$vroot"') or {
			verror(err)
			return
		}
		println(s2.output)
	}
}

pub fn vfmt(args[]string) {
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
}

pub fn install_v(args[]string) {
	if args.len < 3 {
		println('usage: v install [module] [module] [...]')
		return
	}
	names := args.slice(2, args.len)
	vexec := vexe_path()
	vroot := os.dir(vexec)
	vget := '$vroot/tools/vget'
	if true {
		//println('Building vget...')
		os.chdir(vroot + '/tools')
		vget_compilation := os.exec('"$vexec" -o $vget vget.v') or {
			verror(err)
			return
		}
		if vget_compilation.exit_code != 0 {
			verror( vget_compilation.output )
			return
		}
	}
	vgetresult := os.exec('$vget ' + names.join(' ')) or {
		verror(err)
		return
	}
	if vgetresult.exit_code != 0 {
		verror( vgetresult.output )
		return
	}
}

pub fn create_symlink() {
	vexe := vexe_path()
	link_path := '/usr/local/bin/v'
	ret := os.system('ln -sf $vexe $link_path')
	if ret == 0 {
		println('symlink "$link_path" has been created')
	} else {
		println('failed to create symlink "$link_path", '+
			'make sure you run with sudo')
	}
}

pub fn vexe_path() string {
	vexe := os.getenv('VEXE')
	if '' != vexe {	return vexe	}
	return os.executable()
}

pub fn verror(s string) {
	println('V error: $s')
	os.flush_stdout()
	exit(1)
}

pub fn vhash() string {
	mut buf := [50]byte
	buf[0] = 0
	C.snprintf(*char(buf), 50, '%s', C.V_COMMIT_HASH )
	return tos_clone(buf)
}

pub fn cescaped_path(s string) string {
  return s.replace('\\','\\\\')
}

pub fn os_from_string(os string) OS {
	match os {
		'linux' { return .linux}
		'windows' { return .windows}
		'mac' { return .mac}
		'freebsd' { return .freebsd}
		'openbsd' { return .openbsd}
		'netbsd' { return .netbsd}
		'dragonfly' { return .dragonfly}
		'js' { return .js}
		'solaris' { return .solaris}
		'android' { return .android}
		'msvc' {
			// notice that `-os msvc` became `-cc msvc`
			verror('use the flag `-cc msvc` to build using msvc')
		}
	}
	println('bad os $os') // todo panic?
	return .linux
}

//

pub fn set_vroot_folder(vroot_path string) {
	// Preparation for the compiler module:
	// VEXE env variable is needed so that compiler.vexe_path()
	// can return it later to whoever needs it:
	mut vname := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	os.setenv('VEXE', os.realpath( [vroot_path, vname].join(os.path_separator) ), true)
}

pub fn new_v_compiler_with_args(args []string) &V {
	vexe := vexe_path()
	mut allargs := [vexe]
	allargs << args
	os.setenv('VOSARGS', allargs.join(' '), true)
	return new_v(allargs)
}
