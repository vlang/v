// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v.cflag
import v.pref
import v.util
import v.vcache
import term

const (
	c_verror_message_marker = 'VERROR_MESSAGE '
	c_error_info            = '
==================
C error. This should never happen.

If you were not working with C interop, this is a compiler bug, please raise an issue on GitHub:

https://github.com/vlang/v/issues/new/choose

You can also use #help on Discord: https://discord.gg/vlang
'
	no_compiler_error       = '
==================
Error: no C compiler detected.

You can find instructions on how to install one in the V wiki:
https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows

If you think you have one installed, make sure it is in your PATH.
If you do have one in your PATH, please raise an issue on GitHub:
https://github.com/vlang/v/issues/new/choose

You can also use `v doctor`, to see what V knows about your current environment.

You can also seek #help on Discord: https://discord.gg/vlang
'
)

const (
	mingw_cc = 'x86_64-w64-mingw32-gcc'
)

fn (mut v Builder) find_win_cc() ? {
	$if !windows {
		return none
	}
	os.exec('$v.pref.ccompiler -v') or {
		if v.pref.is_verbose {
			println('$v.pref.ccompiler not found, looking for msvc...')
		}
		find_msvc(v.pref.m64) or {
			if v.pref.is_verbose {
				println('msvc not found, looking for thirdparty/tcc...')
			}
			vpath := os.dir(pref.vexe_path())
			thirdparty_tcc := os.join_path(vpath, 'thirdparty', 'tcc', 'tcc.exe')
			os.exec('$thirdparty_tcc -v') or {
				if v.pref.is_verbose {
					println('tcc not found')
				}
				return none
			}
			v.pref.ccompiler = thirdparty_tcc
			v.pref.ccompiler_type = .tinyc
			return
		}
		v.pref.ccompiler = 'msvc'
		v.pref.ccompiler_type = .msvc
		return
	}
	v.pref.ccompiler_type = pref.cc_from_string(v.pref.ccompiler)
}

fn (mut v Builder) show_c_compiler_output(res os.Result) {
	println('======== C Compiler output ========')
	println(res.output)
	println('=================================')
}

fn (mut v Builder) post_process_c_compiler_output(res os.Result) {
	if res.exit_code == 0 {
		if v.pref.reuse_tmpc {
			return
		}
		for tmpfile in v.pref.cleanup_files {
			if os.is_file(tmpfile) {
				if v.pref.is_verbose {
					eprintln('>> remove tmp file: $tmpfile')
				}
				os.rm(tmpfile) or { panic(err) }
			}
		}
		return
	}
	for emsg_marker in [builder.c_verror_message_marker, 'error: include file '] {
		if res.output.contains(emsg_marker) {
			emessage := res.output.all_after(emsg_marker).all_before('\n').all_before('\r').trim_right('\r\n')
			verror(emessage)
		}
	}
	if v.pref.is_debug {
		eword := 'error:'
		khighlight := if term.can_show_color_on_stdout() { term.red(eword) } else { eword }
		println(res.output.trim_right('\r\n').replace(eword, khighlight))
	} else {
		if res.output.len < 30 {
			println(res.output)
		} else {
			elines := error_context_lines(res.output, 'error:', 1, 12)
			println('==================')
			for eline in elines {
				println(eline)
			}
			println('...')
			println('==================')
			println('(Use `v -cg` to print the entire error message)\n')
		}
	}
	verror(builder.c_error_info)
}

fn (mut v Builder) rebuild_cached_module(vexe string, imp_path string) string {
	res := v.pref.cache_manager.exists('.o', imp_path) or {
		if v.pref.is_verbose {
			println('Cached $imp_path .o file not found... Building .o file for $imp_path')
		}
		// do run `v build-module x` always in main vfolder; x can be a relative path
		pwd := os.getwd()
		vroot := os.dir(vexe)
		os.chdir(vroot)
		boptions := v.pref.build_options.join(' ')
		rebuild_cmd := '$vexe $boptions build-module $imp_path'
		vcache.dlog('| Builder.' + @FN, 'vexe: $vexe | imp_path: $imp_path | rebuild_cmd: $rebuild_cmd')
		os.system(rebuild_cmd)
		rebuilded_o := v.pref.cache_manager.exists('.o', imp_path) or {
			panic('could not rebuild cache module for $imp_path, error: $err')
		}
		os.chdir(pwd)
		return rebuilded_o
	}
	return res
}

fn (mut v Builder) show_cc(cmd string, response_file string, response_file_content string) {
	if v.pref.is_verbose || v.pref.show_cc {
		println('')
		println('=====================')
		println('> C compiler cmd: $cmd')
		if v.pref.show_cc {
			println('> C compiler response file $response_file:')
			println(response_file_content)
		}
		println('=====================')
	}
}

struct CcompilerOptions {
mut:
	guessed_compiler string
	shared_postfix   string // .so, .dll
	//
	//
	debug_mode  bool
	is_cc_tcc   bool
	is_cc_gcc   bool
	is_cc_msvc  bool
	is_cc_clang bool
	//
	env_cflags  string // prepended *before* everything else
	env_ldflags string // appended *after* everything else
	//
	args         []string // ordinary C options like `-O2`
	wargs        []string // for `-Wxyz` *exclusively*
	pre_args     []string // options that should go before .o_args
	o_args       []string // for `-o target`
	source_args  []string // for `x.tmp.c`
	post_args    []string // options that should go after .o_args
	linker_flags []string // `-lm`
}

fn (mut v Builder) setup_ccompiler_options(ccompiler string) {
	mut ccoptions := CcompilerOptions{}
	//
	mut debug_options := ['-g']
	mut optimization_options := ['-O2']
	// arguments for the C compiler
	// TODO : activate -Werror once no warnings remain
	// '-Werror',
	// TODO : try and remove the below workaround options when the corresponding
	// warnings are totally fixed/removed
	ccoptions.args = [v.pref.cflags, '-std=gnu99']
	ccoptions.wargs = ['-Wall', '-Wextra', '-Wno-unused', '-Wno-missing-braces', '-Walloc-zero',
		'-Wcast-qual', '-Wdate-time', '-Wduplicated-branches', '-Wduplicated-cond', '-Wformat=2',
		'-Winit-self', '-Winvalid-pch', '-Wjump-misses-init', '-Wlogical-op', '-Wmultichar', '-Wnested-externs',
		'-Wnull-dereference', '-Wpacked', '-Wpointer-arith', '-Wshadow', '-Wswitch-default', '-Wswitch-enum',
		'-Wno-unused-parameter', '-Wno-unknown-warning-option', '-Wno-format-nonliteral']
	if v.pref.os == .ios {
		ccoptions.args << '-framework Foundation'
		ccoptions.args << '-framework UIKit'
		ccoptions.args << '-framework Metal'
		ccoptions.args << '-framework MetalKit'
		ccoptions.args << '-DSOKOL_METAL'
		ccoptions.args << '-fobjc-arc'
	}
	ccoptions.debug_mode = v.pref.is_debug
	ccoptions.guessed_compiler = v.pref.ccompiler
	if ccoptions.guessed_compiler == 'cc' && v.pref.is_prod {
		// deliberately guessing only for -prod builds for performance reasons
		if ccversion := os.exec('cc --version') {
			if ccversion.exit_code == 0 {
				if ccversion.output.contains('This is free software;')
					&& ccversion.output.contains('Free Software Foundation, Inc.') {
					ccoptions.guessed_compiler = 'gcc'
				}
				if ccversion.output.contains('clang version ') {
					ccoptions.guessed_compiler = 'clang'
				}
			}
		}
	}
	//
	ccoptions.is_cc_tcc = ccompiler.contains('tcc') || ccoptions.guessed_compiler == 'tcc'
	ccoptions.is_cc_gcc = ccompiler.contains('gcc') || ccoptions.guessed_compiler == 'gcc'
	ccoptions.is_cc_msvc = ccompiler.contains('msvc') || ccoptions.guessed_compiler == 'msvc'
	ccoptions.is_cc_clang = ccompiler.contains('clang') || ccoptions.guessed_compiler == 'clang'
	// For C++ we must be very tolerant
	if ccoptions.guessed_compiler.contains('++') {
		ccoptions.args << '-fpermissive'
		ccoptions.args << '-w'
	}
	if ccoptions.is_cc_clang {
		if ccoptions.debug_mode {
			debug_options = ['-g', '-O0']
		}
		optimization_options = ['-O3']
		mut have_flto := true
		$if openbsd {
			have_flto = false
		}
		if have_flto {
			optimization_options << '-flto'
		}
	}
	if ccoptions.is_cc_gcc {
		if ccoptions.debug_mode {
			debug_options = ['-g', '-no-pie']
		}
		optimization_options = ['-O3', '-fno-strict-aliasing', '-flto']
	}
	//
	if ccoptions.debug_mode {
		ccoptions.args << debug_options
		// $if macos {
		// args << '-ferror-limit=5000'
		// }
	}
	if v.pref.is_prod {
		ccoptions.args << optimization_options
	}
	if v.pref.is_prod && !ccoptions.debug_mode {
		// sokol and other C libraries that use asserts
		// have much better performance when NDEBUG is defined
		// See also http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
		ccoptions.args << '-DNDEBUG'
	}
	if v.pref.sanitize {
		ccoptions.args << '-fsanitize=leak'
	}
	//
	ccoptions.shared_postfix = '.so'
	$if macos {
		ccoptions.shared_postfix = '.dylib'
	} $else $if windows {
		ccoptions.shared_postfix = '.dll'
	}
	if v.pref.is_shared {
		ccoptions.linker_flags << '-shared'
		ccoptions.args << '-fPIC' // -Wl,-z,defs'
	}
	if v.pref.is_bare {
		ccoptions.args << '-fno-stack-protector'
		ccoptions.args << '-ffreestanding'
		ccoptions.linker_flags << '-static'
		ccoptions.linker_flags << '-nostdlib'
	}
	if ccoptions.debug_mode && os.user_os() != 'windows' && v.pref.build_mode != .build_module {
		ccoptions.linker_flags << '-rdynamic' // needed for nicer symbolic backtraces
	}
	if ccompiler != 'msvc' && v.pref.os != .freebsd {
		ccoptions.wargs << '-Werror=implicit-function-declaration'
	}
	if v.pref.is_liveshared || v.pref.is_livemain {
		if (v.pref.os == .linux || os.user_os() == 'linux') && v.pref.build_mode != .build_module {
			ccoptions.linker_flags << '-rdynamic'
		}
		if v.pref.os == .macos || os.user_os() == 'macos' {
			ccoptions.args << '-flat_namespace'
		}
	}
	// macOS code can include objective C  TODO remove once objective C is replaced with C
	if v.pref.os == .macos || v.pref.os == .ios {
		if !ccoptions.is_cc_tcc {
			ccoptions.source_args << '-x objective-c'
		}
	}
	// The C file we are compiling
	ccoptions.source_args << '"$v.out_name_c"'
	if v.pref.os == .macos {
		ccoptions.source_args << '-x none'
	}
	// Min macos version is mandatory I think?
	if v.pref.os == .macos {
		ccoptions.post_args << '-mmacosx-version-min=10.7'
	} else if v.pref.os == .ios {
		ccoptions.post_args << '-miphoneos-version-min=10.0'
	} else if v.pref.os == .windows {
		ccoptions.post_args << '-municode'
	}
	cflags := v.get_os_cflags()
	ccoptions.o_args << cflags.c_options_only_object_files()
	defines, others, libs := cflags.defines_others_libs()
	ccoptions.pre_args << defines
	ccoptions.pre_args << others
	ccoptions.linker_flags << libs
	// TODO: why is this duplicated from above?
	if v.pref.use_cache && v.pref.build_mode != .build_module {
		// vexe := pref.vexe_path()
		// cached_modules := ['builtin', 'os', 'math', 'strconv', 'strings', 'hash'],  // , 'strconv.ftoa']
		// for cfile in cached_modules {
		// ofile := os.join_path(pref.default_module_path, 'cache', 'vlib', cfile.replace('.', '/') +
		// '.o')
		// if !os.exists(ofile) {
		// println('${cfile}.o is missing. Building...')
		// println('$vexe build-module vlib/$cfile')
		// os.system('$vexe build-module vlib/$cfile')
		// }
		// args << ofile
		// }
		if !ccoptions.is_cc_tcc {
			$if linux {
				ccoptions.linker_flags << '-Xlinker -z'
				ccoptions.linker_flags << '-Xlinker muldefs'
			}
		}
	}
	if ccoptions.is_cc_tcc && 'no_backtrace' !in v.pref.compile_defines {
		ccoptions.post_args << '-bt25'
	}
	// Without these libs compilation will fail on Linux
	// || os.user_os() == 'linux'
	if !v.pref.is_bare && v.pref.build_mode != .build_module
		&& v.pref.os in [.linux, .freebsd, .openbsd, .netbsd, .dragonfly, .solaris, .haiku] {
		ccoptions.linker_flags << '-lm'
		ccoptions.linker_flags << '-lpthread'
		// -ldl is a Linux only thing. BSDs have it in libc.
		if v.pref.os == .linux {
			ccoptions.linker_flags << '-ldl'
		}
		if v.pref.os == .freebsd {
			// FreeBSD: backtrace needs execinfo library while linking
			ccoptions.linker_flags << '-lexecinfo'
		}
	}
	if !v.pref.is_bare && v.pref.os == .js && os.user_os() == 'linux' {
		ccoptions.linker_flags << '-lm'
	}
	ccoptions.env_cflags = os.getenv('CFLAGS')
	ccoptions.env_ldflags = os.getenv('LDFLAGS')
	$if trace_ccoptions ? {
		println('>>> setup_ccompiler_options ccompiler: $ccompiler')
		println('>>> setup_ccompiler_options ccoptions: $ccoptions')
	}
	v.ccoptions = ccoptions
	// setup the cache too, so that different compilers/options do not interfere:
	v.pref.cache_manager.set_temporary_options(ccoptions.thirdparty_object_args([ccoptions.guessed_compiler]))
}

fn (ccoptions CcompilerOptions) all_args() []string {
	mut all := []string{}
	all << ccoptions.env_cflags
	all << ccoptions.args
	all << ccoptions.o_args
	all << ccoptions.pre_args
	all << ccoptions.source_args
	all << ccoptions.post_args
	all << ccoptions.linker_flags
	all << ccoptions.env_ldflags
	return all
}

fn (ccoptions CcompilerOptions) thirdparty_object_args(middle []string) []string {
	mut all := []string{}
	all << ccoptions.env_cflags
	all << ccoptions.args
	all << middle
	all << ccoptions.env_ldflags
	return all
}

fn (mut v Builder) setup_output_name() {
	if !v.pref.is_shared && v.pref.build_mode != .build_module && os.user_os() == 'windows'
		&& !v.pref.out_name.ends_with('.exe') {
		v.pref.out_name += '.exe'
	}
	// Output executable name
	v.log('cc() isprod=$v.pref.is_prod outname=$v.pref.out_name')
	if v.pref.is_shared {
		if !v.pref.out_name.ends_with(v.ccoptions.shared_postfix) {
			v.pref.out_name += v.ccoptions.shared_postfix
		}
	}
	if v.pref.build_mode == .build_module {
		v.pref.out_name = v.pref.cache_manager.postfix_with_key2cpath('.o', v.pref.path) // v.out_name
		if v.pref.is_verbose {
			println('Building $v.pref.path to $v.pref.out_name ...')
		}
		v.pref.cache_manager.save('.description.txt', v.pref.path, '${v.pref.path:-30} @ $v.pref.cache_manager.vopts\n') or {
			panic(err)
		}
		// println('v.table.imports:')
		// println(v.table.imports)
	}
	if os.is_dir(v.pref.out_name) {
		verror("'$v.pref.out_name' is a directory")
	}
	if v.pref.os == .ios {
		bundle_name := v.pref.out_name.split('/').last()
		v.ccoptions.o_args << '-o "${v.pref.out_name}.app/$bundle_name"'
	} else {
		v.ccoptions.o_args << '-o "$v.pref.out_name"'
	}
}

fn (mut v Builder) vjs_cc() bool {
	vexe := pref.vexe_path()
	vdir := os.dir(vexe)
	// Just create a C/JavaScript file and exit
	// for example: `v -o v.c compiler`
	ends_with_c := v.pref.out_name.ends_with('.c')
	ends_with_js := v.pref.out_name.ends_with('.js')
	if ends_with_c || ends_with_js {
		v.pref.skip_running = true
		// Translating V code to JS by launching vjs.
		// Using a separate process for V.js is for performance mostly,
		// to avoid constant is_js checks.
		$if !js {
			if ends_with_js {
				vjs_path := vexe + 'js'
				if !os.exists(vjs_path) {
					println('V.js compiler not found, building...')
					// Build V.js. Specifying `-os js` makes V include
					// only _js.v files and ignore _c.v files.
					ret := os.system('$vexe -o $vjs_path -os js $vdir/cmd/v')
					if ret == 0 {
						println('Done.')
					} else {
						println('Failed.')
						exit(1)
					}
				}
				ret := os.system('$vjs_path -o $v.pref.out_name $v.pref.path')
				if ret == 0 {
					println('Done. Run it with `node $v.pref.out_name`')
					println('JS backend is at a very early stage.')
				}
			}
		}
		// v.out_name_c may be on a different partition than v.out_name
		os.mv_by_cp(v.out_name_c, v.pref.out_name) or { panic(err) }
		return true
	}
	return false
}

fn (mut v Builder) dump_c_options(all_args []string) {
	if v.pref.dump_c_flags != '' {
		non_empty_args := all_args.filter(it != '').join('\n') + '\n'
		if v.pref.dump_c_flags == '-' {
			print(non_empty_args)
		} else {
			os.write_file(v.pref.dump_c_flags, non_empty_args) or { panic(err) }
		}
	}
}

fn (mut v Builder) cc() {
	if os.executable().contains('vfmt') {
		return
	}
	if v.pref.is_verbose {
		println('builder.cc() pref.out_name="$v.pref.out_name"')
	}
	if v.pref.only_check_syntax {
		if v.pref.is_verbose {
			println('builder.cc returning early, since pref.only_check_syntax is true')
		}
		return
	}
	if v.vjs_cc() {
		return
	}
	// Cross compiling for Windows
	if v.pref.os == .windows {
		$if !windows {
			v.cc_windows_cross()
			return
		}
	}
	// Cross compiling for Linux
	if v.pref.os == .linux {
		$if !linux {
			v.cc_linux_cross()
			return
		}
	}
	//
	vexe := pref.vexe_path()
	vdir := os.dir(vexe)
	mut tried_compilation_commands := []string{}
	mut tcc_output := os.Result{}
	original_pwd := os.getwd()
	for {
		// try to compile with the choosen compiler
		// if compilation fails, retry again with another
		mut ccompiler := v.pref.ccompiler
		if v.pref.os == .ios {
			ios_sdk := if v.pref.is_ios_simulator { 'iphonesimulator' } else { 'iphoneos' }
			ios_sdk_path_res := os.exec('xcrun --sdk $ios_sdk --show-sdk-path') or {
				panic("Couldn't find iphonesimulator")
			}
			mut isysroot := ios_sdk_path_res.output.replace('\n', '')
			ccompiler = 'xcrun --sdk iphoneos clang -isysroot $isysroot'
		}
		v.setup_ccompiler_options(ccompiler)
		v.build_thirdparty_obj_files()
		v.setup_output_name()
		//
		mut libs := []string{} // builtin.o os.o http.o etc
		if v.pref.build_mode == .build_module {
			v.ccoptions.pre_args << '-c'
		} else if v.pref.use_cache {
			mut built_modules := []string{}
			builtin_obj_path := v.rebuild_cached_module(vexe, 'vlib/builtin')
			libs << builtin_obj_path
			for ast_file in v.parsed_files {
				if v.pref.is_test && ast_file.mod.name != 'main' {
					imp_path := v.find_module_path(ast_file.mod.name, ast_file.path) or {
						verror('cannot import module "$ast_file.mod.name" (not found)')
						break
					}
					obj_path := v.rebuild_cached_module(vexe, imp_path)
					libs << obj_path
					built_modules << ast_file.mod.name
				}
				for imp_stmt in ast_file.imports {
					imp := imp_stmt.mod
					// strconv is already imported inside builtin, so skip generating its object file
					// TODO: incase we have other modules with the same name, make sure they are vlib
					// is this even doign anything?
					if imp in ['strconv', 'strings'] {
						continue
					}
					if imp in built_modules {
						continue
					}
					if util.should_bundle_module(imp) {
						continue
					}
					// not working
					if imp == 'webview' {
						continue
					}
					// The problem is cmd/v is in module main and imports
					// the relative module named help, which is built as cmd.v.help not help
					// currently this got this workign by building into main, see ast.FnDecl in cgen
					if imp == 'help' {
						continue
					}
					// we are skipping help manually above, this code will skip all relative imports
					// if os.is_dir(af_base_dir + os.path_separator + mod_path) {
					// continue
					// }
					// mod_path := imp.replace('.', os.path_separator)
					// imp_path := os.join_path('vlib', mod_path)
					imp_path := v.find_module_path(imp, ast_file.path) or {
						verror('cannot import module "$imp" (not found)')
						break
					}
					obj_path := v.rebuild_cached_module(vexe, imp_path)
					libs << obj_path
					if obj_path.ends_with('vlib/ui.o') {
						v.ccoptions.post_args << '-framework Cocoa'
						v.ccoptions.post_args << '-framework Carbon'
					}
					built_modules << imp
				}
			}
			v.ccoptions.post_args << libs
		}
		//
		$if windows {
			if ccompiler == 'msvc' {
				v.cc_msvc()
				return
			}
		}
		//
		all_args := v.ccoptions.all_args()
		v.dump_c_options(all_args)
		str_args := all_args.join(' ')
		// write args to response file
		response_file := '${v.out_name_c}.rsp'
		response_file_content := str_args.replace('\\', '\\\\')
		os.write_file(response_file, response_file_content) or {
			verror('Unable to write response file "$response_file"')
		}
		if !v.ccoptions.debug_mode {
			v.pref.cleanup_files << v.out_name_c
			v.pref.cleanup_files << response_file
		}
		$if windows {
			if v.ccoptions.is_cc_tcc {
				def_name := v.pref.out_name[0..v.pref.out_name.len - 4]
				v.pref.cleanup_files << '${def_name}.def'
			}
		}
		//
		os.chdir(vdir)
		cmd := '$ccompiler "@$response_file"'
		tried_compilation_commands << cmd
		v.show_cc(cmd, response_file, response_file_content)
		// Run
		ccompiler_label := 'C ${os.file_name(ccompiler):3}'
		util.timing_start(ccompiler_label)
		res := os.exec(cmd) or {
			os.Result{
				exit_code: 111
				output: 'C compilation failed.\n$err.msg'
			}
		}
		util.timing_measure(ccompiler_label)
		if v.pref.show_c_output {
			v.show_c_compiler_output(res)
		}
		os.chdir(original_pwd)
		vcache.dlog('| Builder.' + @FN, '>       v.pref.use_cache: $v.pref.use_cache | v.pref.retry_compilation: $v.pref.retry_compilation')
		vcache.dlog('| Builder.' + @FN, '>      cmd res.exit_code: $res.exit_code | cmd: $cmd')
		vcache.dlog('| Builder.' + @FN, '>  response_file_content:\n$response_file_content')
		if res.exit_code != 0 {
			if ccompiler.contains('tcc.exe') {
				// a TCC problem? Retry with the system cc:
				if tried_compilation_commands.len > 1 {
					eprintln('Recompilation loop detected (ccompiler: $ccompiler):')
					for recompile_command in tried_compilation_commands {
						eprintln('   $recompile_command')
					}
					exit(101)
				}
				if v.pref.retry_compilation {
					tcc_output = res
					v.pref.ccompiler = pref.default_c_compiler()
					if v.pref.is_verbose {
						eprintln('Compilation with tcc failed. Retrying with $v.pref.ccompiler ...')
					}
					continue
				}
			}
			if res.exit_code == 127 {
				verror('C compiler error, while attempting to run: \n' +
					'-----------------------------------------------------------\n' + '$cmd\n' +
					'-----------------------------------------------------------\n' +
					'Probably your C compiler is missing. \n' +
					'Please reinstall it, or make it available in your PATH.\n\n' +
					missing_compiler_info())
			}
		}
		if !v.pref.show_c_output {
			// if tcc failed once, and the system C compiler has failed as well,
			// print the tcc error instead since it may contain more useful information
			// see https://discord.com/channels/592103645835821068/592115457029308427/811956304314761228
			if res.exit_code != 0 && tcc_output.output != '' {
				v.post_process_c_compiler_output(tcc_output)
			} else {
				v.post_process_c_compiler_output(res)
			}
		}
		// Print the C command
		if v.pref.is_verbose {
			println('$ccompiler')
			println('=========\n')
		}
		break
	}
	// Link it if we are cross compiling and need an executable
	/*
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
		'/usr/lib/x86_64-linux-gnu/crtn.o') or {
			verror(err.msg)
			return
		}
		println(ress.output)
		println('linux cross compilation done. resulting binary: "$v.out_name"')
	}
	*/
	if v.pref.compress {
		$if windows {
			println('-compress does not work on Windows for now')
			return
		}
		ret := os.system('strip $v.pref.out_name')
		if ret != 0 {
			println('strip failed')
			return
		}
		// NB: upx --lzma can sometimes fail with NotCompressibleException
		// See https://github.com/vlang/v/pull/3528
		mut ret2 := os.system('upx --lzma -qqq $v.pref.out_name')
		if ret2 != 0 {
			ret2 = os.system('upx -qqq $v.pref.out_name')
		}
		if ret2 != 0 {
			println('upx failed')
			$if macos {
				println('install upx with `brew install upx`')
			}
			$if linux {
				println('install upx\n' + 'for example, on Debian/Ubuntu run `sudo apt install upx`')
			}
			$if windows {
				// :)
			}
		}
	}
	// if v.pref.os == .ios {
	// ret := os.system('ldid2 -S $v.pref.out_name')
	// if ret != 0 {
	// eprintln('failed to run ldid2, try: brew install ldid')
	// }
	// }
}

fn (mut b Builder) cc_linux_cross() {
	b.setup_ccompiler_options(b.pref.ccompiler)
	b.build_thirdparty_obj_files()
	b.setup_output_name()
	parent_dir := os.vmodules_dir()
	if !os.exists(parent_dir) {
		os.mkdir(parent_dir) or { panic(err) }
	}
	sysroot := os.join_path(os.vmodules_dir(), 'linuxroot')
	if !os.is_dir(sysroot) {
		println('Downloading files for Linux cross compilation (~18 MB)...')
		zip_url := 'https://github.com/vlang/v/releases/download/0.1.27/linuxroot.zip'
		zip_file := sysroot + '.zip'
		os.system('curl -L -o $zip_file $zip_url')
		if !os.exists(zip_file) {
			verror('Failed to download `$zip_url` as $zip_file')
		}
		os.system('tar -C $parent_dir -xf $zip_file')
		if !os.is_dir(sysroot) {
			verror('Failed to unzip $zip_file to $parent_dir')
		}
	}
	obj_file := b.out_name_c + '.o'
	cflags := b.get_os_cflags()
	defines, others, libs := cflags.defines_others_libs()
	mut cc_args := []string{}
	cc_args << '-w'
	cc_args << '-fPIC'
	cc_args << '-c'
	cc_args << '-target x86_64-linux-gnu'
	cc_args << defines
	cc_args << '-I $sysroot/include '
	cc_args << others
	cc_args << '-o "$obj_file"'
	cc_args << '-c "$b.out_name_c"'
	cc_args << libs
	b.dump_c_options(cc_args)
	cc_cmd := 'cc ' + cc_args.join(' ')
	if b.pref.show_cc {
		println(cc_cmd)
	}
	cc_res := os.exec(cc_cmd) or {
		os.Result{
			exit_code: 1
			output: 'no `cc` command found'
		}
	}
	if cc_res.exit_code != 0 {
		println('Cross compilation for Linux failed (first step, cc). Make sure you have clang installed.')
		verror(cc_res.output)
		return
	}
	mut linker_args := ['-L $sysroot/usr/lib/x86_64-linux-gnu/', '--sysroot=$sysroot', '-v', '-o $b.pref.out_name',
		'-m elf_x86_64', '-dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2', '$sysroot/crt1.o $sysroot/crti.o $obj_file',
		'-lc', '-lcrypto', '-lssl', '-lpthread', '$sysroot/crtn.o']
	linker_args << cflags.c_options_only_object_files()
	// -ldl
	b.dump_c_options(linker_args)
	linker_cmd := '$sysroot/ld.lld ' + linker_args.join(' ')
	// s = s.replace('SYSROOT', sysroot) // TODO $ inter bug
	// s = s.replace('-o hi', '-o ' + c.pref.out_name)
	if b.pref.show_cc {
		println(linker_cmd)
	}
	res := os.exec(linker_cmd) or {
		println('Cross compilation for Linux failed (second step, lld).')
		verror(err.msg)
		return
	}
	if res.exit_code != 0 {
		println('Cross compilation for Linux failed (second step, lld).')
		verror(res.output)
	}
	println(b.pref.out_name + ' has been successfully compiled')
}

fn (mut c Builder) cc_windows_cross() {
	println('Cross compiling for Windows...')
	c.setup_ccompiler_options(c.pref.ccompiler)
	c.build_thirdparty_obj_files()
	c.setup_output_name()
	if !c.pref.out_name.ends_with('.exe') {
		c.pref.out_name += '.exe'
	}
	mut args := []string{}
	args << '$c.pref.cflags'
	args << '-o $c.pref.out_name'
	args << '-w -L.'
	//
	cflags := c.get_os_cflags()
	// -I flags
	if c.pref.ccompiler == 'msvc' {
		args << cflags.c_options_before_target_msvc()
	} else {
		args << cflags.c_options_before_target()
	}
	mut optimization_options := []string{}
	mut debug_options := []string{}
	if c.pref.is_prod {
		if c.pref.ccompiler != 'msvc' {
			optimization_options = ['-O3', '-fno-strict-aliasing', '-flto']
		}
	}
	if c.pref.is_debug {
		if c.pref.ccompiler != 'msvc' {
			debug_options = ['-O0', '-g', '-gdwarf-2']
		}
	}
	mut libs := []string{}
	if false && c.pref.build_mode == .default_mode {
		builtin_o := '"$pref.default_module_path/vlib/builtin.o"'
		libs << builtin_o
		if !os.exists(builtin_o) {
			verror('$builtin_o not found')
		}
		for imp in c.table.imports {
			libs << '"$pref.default_module_path/vlib/${imp}.o"'
		}
	}
	// add the thirdparty .o files, produced by all the #flag directives:
	args << cflags.c_options_only_object_files()
	args << c.out_name_c
	if c.pref.ccompiler == 'msvc' {
		args << cflags.c_options_after_target_msvc()
	} else {
		args << cflags.c_options_after_target()
	}
	/*
	winroot := '${pref.default_module_path}/winroot'
	if !os.is_dir(winroot) {
		winroot_url := 'https://github.com/vlang/v/releases/download/v0.1.10/winroot.zip'
		println('"$winroot" not found.')
		println('Download it from $winroot_url and save it in ${pref.default_module_path}')
		println('Unzip it afterwards.\n')
		println('winroot.zip contains all library and header files needed ' + 'to cross-compile for Windows.')
		exit(1)
	}
	mut obj_name := c.out_name
	obj_name = obj_name.replace('.exe', '')
	obj_name = obj_name.replace('.o.o', '.o')
	include := '-I $winroot/include '
	*/
	if os.user_os() !in ['macos', 'linux'] {
		println(os.user_os())
		panic('your platform is not supported yet')
	}
	mut all_args := []string{}
	all_args << optimization_options
	all_args << debug_options
	all_args << '-std=gnu11'
	all_args << args
	all_args << '-municode'
	c.dump_c_options(all_args)
	mut cmd := '$builder.mingw_cc ' + all_args.join(' ')
	// cmd := 'clang -o $obj_name -w $include -m32 -c -target x86_64-win32 ${pref.default_module_path}/$c.out_name_c'
	if c.pref.is_verbose || c.pref.show_cc {
		println(cmd)
	}
	if os.system(cmd) != 0 {
		println('Cross compilation for Windows failed. Make sure you have mingw-w64 installed.')
		$if macos {
			println('brew install mingw-w64')
		}
		$if linux {
			println('Try `sudo apt install -y mingw-w64` on Debian based distros, or `sudo pacman -S mingw-w64-gcc` on Arch, etc...')
		}
		exit(1)
	}
	/*
	if c.pref.build_mode != .build_module {
		link_cmd := 'lld-link $obj_name $winroot/lib/libcmt.lib ' + '$winroot/lib/libucrt.lib $winroot/lib/kernel32.lib $winroot/lib/libvcruntime.lib ' + '$winroot/lib/uuid.lib'
		if c.pref.show_cc {
			println(link_cmd)
		}
		if os.system(link_cmd) != 0 {
			println('Cross compilation for Windows failed. Make sure you have lld linker installed.')
			exit(1)
		}
		// os.rm(obj_name)
	}
	*/
	println(c.pref.out_name + ' has been successfully compiled')
}

fn (mut v Builder) build_thirdparty_obj_files() {
	v.log('build_thirdparty_obj_files: v.table.cflags: $v.table.cflags')
	for flag in v.get_os_cflags() {
		if flag.value.ends_with('.o') {
			rest_of_module_flags := v.get_rest_of_module_cflags(flag)
			if v.pref.ccompiler == 'msvc' {
				v.build_thirdparty_obj_file_with_msvc(flag.value, rest_of_module_flags)
			} else {
				v.build_thirdparty_obj_file(flag.value, rest_of_module_flags)
			}
		}
	}
}

fn (mut v Builder) build_thirdparty_obj_file(path string, moduleflags []cflag.CFlag) {
	obj_path := os.real_path(path)
	cfile := '${obj_path[..obj_path.len - 2]}.c'
	opath := v.pref.cache_manager.postfix_with_key2cpath('.o', obj_path)
	if os.exists(opath) {
		return
	}
	if os.exists(obj_path) {
		// Some .o files are distributed with no source
		// for example thirdparty\tcc\lib\openlibm.o
		// the best we can do for them is just copy them,
		// and hope that they work with any compiler...
		os.cp(obj_path, opath) or { panic(err) }
		return
	}
	println('$obj_path not found, building it in $opath ...')
	//
	// prepare for tcc, it needs relative paths to thirdparty/tcc to work:
	current_folder := os.getwd()
	os.chdir(os.dir(pref.vexe_path()))
	//
	mut all_options := []string{}
	all_options << v.pref.third_party_option
	all_options << moduleflags.c_options_before_target()
	all_options << '-o "$opath"'
	all_options << '-c "$cfile"'
	cc_options := v.ccoptions.thirdparty_object_args(all_options).join(' ')
	cmd := '$v.pref.ccompiler $cc_options'
	$if trace_thirdparty_obj_files ? {
		println('>>> build_thirdparty_obj_files cmd: $cmd')
	}
	res := os.exec(cmd) or {
		eprintln('exec failed for thirdparty object build cmd:\n$cmd')
		verror(err.msg)
		return
	}
	os.chdir(current_folder)
	if res.exit_code != 0 {
		eprintln('failed thirdparty object build cmd:\n$cmd')
		verror(res.output)
		return
	}
	v.pref.cache_manager.save('.description.txt', obj_path, '${obj_path:-30} @ $cmd\n') or {
		panic(err)
	}
	println(res.output)
}

fn missing_compiler_info() string {
	$if windows {
		return 'https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows'
	}
	$if linux {
		return 'On Debian/Ubuntu, run `sudo apt install build-essential`'
	}
	$if macos {
		return 'Install command line XCode tools with `xcode-select --install`'
	}
	return ''
}

fn error_context_lines(text string, keyword string, before int, after int) []string {
	khighlight := if term.can_show_color_on_stdout() { term.red(keyword) } else { keyword }
	mut eline_idx := 0
	mut lines := text.split_into_lines()
	for idx, eline in lines {
		if eline.contains(keyword) {
			lines[idx] = lines[idx].replace(keyword, khighlight)
			if eline_idx == 0 {
				eline_idx = idx
			}
		}
	}
	idx_s := if eline_idx - before >= 0 { eline_idx - before } else { 0 }
	idx_e := if idx_s + after < lines.len { idx_s + after } else { lines.len }
	return lines[idx_s..idx_e]
}
