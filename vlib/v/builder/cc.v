// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import time
import v.cflag
import v.pref
import term

const (
	c_verror_message_marker = 'VERROR_MESSAGE '
	c_error_info            = '
==================
C error. This should never happen.

If you were not working with C interop, please raise an issue on GitHub:

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

fn todo() {
}

fn (mut v Builder) find_win_cc() ? {
	$if !windows {
		return none
	}
	os.exec('$v.pref.ccompiler -v') or {
		if v.pref.is_verbose {
			println('$v.pref.ccompiler not found, looking for msvc...')
		}
		find_msvc() or {
			if v.pref.is_verbose {
				println('msvc not found, looking for thirdparty/tcc...')
			}
			vpath := os.dir(os.getenv('VEXE'))
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
				os.rm(tmpfile)
			}
		}
		return
	}
	for emsg_marker in [c_verror_message_marker, 'error: include file '] {
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
	verror(c_error_info)
}

fn (mut v Builder) rebuild_cached_module(vexe string, imp_path string) string {
	res := v.pref.cache_manager.exists('.o', imp_path) or {
		println('Cached $imp_path .o file not found... Building .o file for $imp_path')
		// do run `v build-module x` always in main vfolder; x can be a relative path
		pwd := os.getwd()
		vroot := os.dir(vexe)
		os.chdir(vroot)
		boptions := v.pref.build_options.join(' ')
		rebuild_cmd := '$vexe $boptions build-module $imp_path'
		// eprintln('>> rebuild_cmd: $rebuild_cmd')
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
	v.build_thirdparty_obj_files()
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
		os.mv_by_cp(v.out_name_c, v.pref.out_name) or {
			panic(err)
		}
		return
	}
	// Cross compiling for Windows
	if v.pref.os == .windows {
		$if !windows {
			v.cc_windows_cross()
			return
		}
	}
	mut ccompiler := v.pref.ccompiler
	$if windows {
		if ccompiler == 'msvc' {
			v.cc_msvc()
			return
		}
	}
	if v.pref.os == .ios {
		ios_sdk := if v.pref.is_ios_simulator { 'iphonesimulator' } else { 'iphoneos' }
		ios_sdk_path_res := os.exec('xcrun --sdk $ios_sdk --show-sdk-path') or {
			panic("Couldn\'t find iphonesimulator")
		}
		mut isysroot := ios_sdk_path_res.output.replace('\n', '')
		ccompiler = 'xcrun --sdk iphoneos clang -isysroot $isysroot'
	}
	// arguments for the C compiler
	// TODO : activate -Werror once no warnings remain
	// '-Werror',
	// TODO : try and remove the below workaround options when the corresponding
	// warnings are totally fixed/removed
	mut args := [v.pref.cflags, '-std=gnu99', '-Wall', '-Wextra', '-Wno-unused-variable', '-Wno-unused-parameter',
		'-Wno-unused-result', '-Wno-unused-function', '-Wno-missing-braces', '-Wno-unused-label']
	if v.pref.os == .ios {
		args << '-framework Foundation'
		args << '-framework UIKit'
		args << '-framework Metal'
		args << '-framework MetalKit'
		args << '-DSOKOL_METAL'
		args << '-fobjc-arc'
	}
	mut linker_flags := []string{}
	// TCC on Linux by default, unless -cc was provided
	// TODO if -cc = cc, TCC is still used, default compiler should be
	// used instead.
	if v.pref.fast {
		$if linux {
			$if !android {
				tcc_3rd := '$vdir/thirdparty/tcc/bin/tcc'
				// println('tcc third "$tcc_3rd"')
				tcc_path := '/var/tmp/tcc/bin/tcc'
				if os.exists(tcc_3rd) && !os.exists(tcc_path) {
					// println('moving tcc')
					// if there's tcc in thirdparty/, that means this is
					// a prebuilt V_linux.zip.
					// Until the libtcc1.a bug is fixed, we neeed to move
					// it to /var/tmp/
					os.system('mv $vdir/thirdparty/tcc /var/tmp/')
				}
				if v.pref.ccompiler == 'cc' && os.exists(tcc_path) {
					// TODO tcc bug, needs an empty libtcc1.a fila
					// os.mkdir('/var/tmp/tcc/lib/tcc/') or { panic(err) }
					// os.create('/var/tmp/tcc/lib/tcc/libtcc1.a')
					v.pref.ccompiler = tcc_path
					args << '-m64'
				}
			}
		} $else {
			verror('-fast is only supported on Linux right now')
		}
	}
	if !v.pref.is_shared && v.pref.build_mode != .build_module && os.user_os() == 'windows' &&
		!v.pref.out_name.ends_with('.exe') {
		v.pref.out_name += '.exe'
	}
	// linux_host := os.user_os() == 'linux'
	v.log('cc() isprod=$v.pref.is_prod outname=$v.pref.out_name')
	if v.pref.is_shared {
		linker_flags << '-shared'
		args << '-fPIC' // -Wl,-z,defs'
		$if macos {
			v.pref.out_name += '.dylib'
		} $else $if windows {
			v.pref.out_name += '.dll'
		} $else {
			v.pref.out_name += '.so'
		}
	}
	if v.pref.is_bare {
		args << '-fno-stack-protector'
		args << '-ffreestanding'
		linker_flags << '-static'
		linker_flags << '-nostdlib'
	}
	if v.pref.build_mode == .build_module {
		v.pref.out_name = v.pref.cache_manager.postfix_with_key2cpath('.o', v.pref.path) // v.out_name
		println('Building $v.pref.path to $v.pref.out_name ...')
		v.pref.cache_manager.save('.description.txt', v.pref.path, '${v.pref.path:-30} @ $v.pref.cache_manager.vopts\n')
		// println('v.table.imports:')
		// println(v.table.imports)
	}
	debug_mode := v.pref.is_debug
	mut debug_options := '-g3'
	mut optimization_options := '-O2'
	mut guessed_compiler := v.pref.ccompiler
	if guessed_compiler == 'cc' && v.pref.is_prod {
		// deliberately guessing only for -prod builds for performance reasons
		if ccversion := os.exec('cc --version') {
			if ccversion.exit_code == 0 {
				if ccversion.output.contains('This is free software;') &&
					ccversion.output.contains('Free Software Foundation, Inc.') {
					guessed_compiler = 'gcc'
				}
				if ccversion.output.contains('clang version ') {
					guessed_compiler = 'clang'
				}
			}
		}
	}
	//
	is_cc_tcc := ccompiler.contains('tcc') || guessed_compiler == 'tcc'
	is_cc_clang := !is_cc_tcc && (ccompiler.contains('clang') || guessed_compiler == 'clang')
	is_cc_gcc := !is_cc_tcc && !is_cc_clang &&
		(ccompiler.contains('gcc') || guessed_compiler == 'gcc')
	// is_cc_msvc := v.pref.ccompiler.contains('msvc') || guessed_compiler == 'msvc'
	//
	if is_cc_clang {
		if debug_mode {
			debug_options = '-g3 -O0 -no-pie'
		}
		optimization_options = '-O3'
		mut have_flto := true
		$if openbsd {
			have_flto = false
		}
		if have_flto {
			optimization_options += ' -flto'
		}
	}
	if is_cc_gcc {
		if debug_mode {
			debug_options = '-g3 -no-pie'
		}
		optimization_options = '-O3 -fno-strict-aliasing -flto'
	}
	if debug_mode {
		args << debug_options
		$if macos {
			args << ' -ferror-limit=5000 '
		}
	}
	if v.pref.is_prod {
		args << optimization_options
	}
	if v.pref.is_prod && !debug_mode {
		// sokol and other C libraries that use asserts
		// have much better performance when NDEBUG is defined
		// See also http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
		args << '-DNDEBUG'
	}
	if debug_mode && os.user_os() != 'windows' {
		linker_flags << ' -rdynamic ' // needed for nicer symbolic backtraces
	}
	if ccompiler != 'msvc' && v.pref.os != .freebsd {
		args << '-Werror=implicit-function-declaration'
	}
	if v.pref.is_liveshared || v.pref.is_livemain {
		if v.pref.os == .linux || os.user_os() == 'linux' {
			linker_flags << '-rdynamic'
		}
		if v.pref.os == .macos || os.user_os() == 'macos' {
			args << '-flat_namespace'
		}
	}
	mut libs := '' // builtin.o os.o http.o etc
	if v.pref.build_mode == .build_module {
		args << '-c'
	} else if v.pref.use_cache {
		mut built_modules := []string{}
		builtin_obj_path := v.rebuild_cached_module(vexe, 'vlib/builtin')
		libs += ' ' + builtin_obj_path
		for ast_file in v.parsed_files {
			for imp_stmt in ast_file.imports {
				imp := imp_stmt.mod
				if imp in built_modules {
					continue
				}
				// not working
				if imp == 'webview' {
					continue
				}
				// println('cache: import "$imp"')
				mod_path := imp.replace('.', os.path_separator)
				// TODO: to get import path all imports (even relative) we can use:
				// import_path := v.find_module_path(imp, ast_file.path) or {
				// verror('cannot import module "$imp" (not found)')
				// break
				// }
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
				imp_path := os.join_path('vlib', mod_path)
				obj_path := v.rebuild_cached_module(vexe, imp_path)
				libs += ' ' + obj_path
				if obj_path.ends_with('vlib/ui.o') {
					args << '-framework Cocoa -framework Carbon'
				}
				built_modules << imp
			}
		}
	}
	if v.pref.sanitize {
		args << '-fsanitize=leak'
	}
	// Cross compiling for linux
	if v.pref.os == .linux {
		$if !linux {
			v.cc_linux_cross()
			return
		}
	}
	// Cross compiling windows
	//
	// Output executable name
	if v.pref.os == .ios {
		bundle_name := v.pref.out_name.split('/').last()
		args << '-o "${v.pref.out_name}.app/$bundle_name"'
	} else {
		args << '-o "$v.pref.out_name"'
	}
	if os.is_dir(v.pref.out_name) {
		verror("'$v.pref.out_name' is a directory")
	}
	// macOS code can include objective C  TODO remove once objective C is replaced with C
	if v.pref.os == .macos || v.pref.os == .ios {
		args << '-x objective-c'
	}
	// The C file we are compiling
	args << '"$v.out_name_c"'
	if v.pref.os == .macos {
		args << '-x none'
	}
	// Min macos version is mandatory I think?
	if v.pref.os == .macos {
		args << '-mmacosx-version-min=10.7'
	}
	if v.pref.os == .ios {
		args << '-miphoneos-version-min=10.0'
	}
	if v.pref.os == .windows {
		args << '-municode'
	}
	cflags := v.get_os_cflags()
	// add .o files
	args << cflags.c_options_only_object_files()
	// add all flags (-I -l -L etc) not .o files
	args << cflags.c_options_without_object_files()
	args << libs
	// For C++ we must be very tolerant
	if guessed_compiler.contains('++') {
		args << '-fpermissive'
		args << '-w'
	}
	// TODO: why is this duplicated from above?
	if v.pref.use_cache {
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
		if !is_cc_tcc {
			$if linux {
				linker_flags << '-Xlinker -z'
				linker_flags << '-Xlinker muldefs'
			}
		}
	}
	if is_cc_tcc && 'no_backtrace' !in v.pref.compile_defines {
		args << '-bt25'
	}
	// Without these libs compilation will fail on Linux
	// || os.user_os() == 'linux'
	if !v.pref.is_bare && v.pref.build_mode != .build_module && v.pref.os in
		[.linux, .freebsd, .openbsd, .netbsd, .dragonfly, .solaris, .haiku] {
		linker_flags << '-lm'
		linker_flags << '-lpthread'
		// -ldl is a Linux only thing. BSDs have it in libc.
		if v.pref.os == .linux {
			linker_flags << '-ldl'
		}
		if v.pref.os == .freebsd {
			// FreeBSD: backtrace needs execinfo library while linking
			linker_flags << '-lexecinfo'
		}
	}
	if !v.pref.is_bare && v.pref.os == .js && os.user_os() == 'linux' {
		linker_flags << '-lm'
	}
	env_cflags := os.getenv('CFLAGS')
	env_ldflags := os.getenv('LDFLAGS')
	str_args := env_cflags + ' ' + args.join(' ') + ' ' + linker_flags.join(' ') + ' ' + env_ldflags
	if v.pref.is_verbose {
		println('cc args=$str_args')
		println(args)
	}
	// write args to response file
	response_file := '${v.out_name_c}.rsp'
	response_file_content := str_args.replace('\\', '\\\\')
	os.write_file(response_file, response_file_content) or {
		verror('Unable to write response file "$response_file"')
	}
	if !debug_mode {
		v.pref.cleanup_files << v.out_name_c
		v.pref.cleanup_files << response_file
	}
	start:
	todo()
	// TODO remove
	cmd := '$ccompiler @$response_file'
	v.show_cc(cmd, response_file, response_file_content)
	// Run
	ticks := time.ticks()
	res := os.exec(cmd) or {
		// C compilation failed.
		// If we are on Windows, try msvc
		println('C compilation failed.')
		/*
		if os.user_os() == 'windows' && v.pref.ccompiler != 'msvc' {
			println('Trying to build with MSVC')
			v.cc_msvc()
			return
		}
		*/
		verror(err)
		return
	}
	diff := time.ticks() - ticks
	v.timing_message('C ${ccompiler:3}', diff)
	if v.pref.show_c_output {
		v.show_c_compiler_output(res)
	}
	if res.exit_code == 127 {
		// the command could not be found by the system
		$if linux {
			// TCC problems on linux? Try GCC.
			if ccompiler.contains('tcc') {
				v.pref.ccompiler = 'cc'
				goto start
			}
		}
		verror('C compiler error, while attempting to run: \n' +
			'-----------------------------------------------------------\n' + '$cmd\n' +
			'-----------------------------------------------------------\n' + 'Probably your C compiler is missing. \n' +
			'Please reinstall it, or make it available in your PATH.\n\n' + missing_compiler_info())
	}
	if !v.pref.show_c_output {
		v.post_process_c_compiler_output(res)
	}
	// Print the C command
	if v.pref.is_verbose {
		println('$ccompiler took $diff ms')
		println('=========\n')
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
			verror(err)
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
				println('install upx\n' +
					'for example, on Debian/Ubuntu run `sudo apt install upx`')
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
	parent_dir := os.vmodules_dir()
	if !os.exists(parent_dir) {
		os.mkdir(parent_dir)
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
	mut cc_args := '-fPIC -w -c -target x86_64-linux-gnu -c -o $obj_file $b.out_name_c -I $sysroot/include '
	cflags := b.get_os_cflags()
	cc_args += cflags.c_options_without_object_files()
	cc_cmd := 'cc $cc_args'
	if b.pref.show_cc {
		println(cc_cmd)
	}
	cc_res := os.exec(cc_cmd) or {
		println('Cross compilation for Linux failed (first step, cc). Make sure you have clang installed.')
		verror(err)
		return
	}
	if cc_res.exit_code != 0 {
		println('Cross compilation for Linux failed (first step, cc). Make sure you have clang installed.')
		verror(cc_res.output)
	}
	linker_args := ['-L $sysroot/usr/lib/x86_64-linux-gnu/', '--sysroot=$sysroot -v -o $b.pref.out_name -m elf_x86_64',
		'-dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2', '$sysroot/crt1.o $sysroot/crti.o $obj_file',
		'-lc', '-lcrypto', '-lssl', '-lpthread', '$sysroot/crtn.o', cflags.c_options_only_object_files()]
	// -ldl
	linker_args_str := linker_args.join(' ')
	linker_cmd := '$sysroot/ld.lld $linker_args_str'
	// s = s.replace('SYSROOT', sysroot) // TODO $ inter bug
	// s = s.replace('-o hi', '-o ' + c.pref.out_name)
	if b.pref.show_cc {
		println(linker_cmd)
	}
	res := os.exec(linker_cmd) or {
		println('Cross compilation for Linux failed (second step, lld).')
		verror(err)
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
	if !c.pref.out_name.ends_with('.exe') {
		c.pref.out_name += '.exe'
	}
	mut args := '-o $c.pref.out_name -w -L. '
	cflags := c.get_os_cflags()
	// -I flags
	args += if c.pref.ccompiler == 'msvc' { cflags.c_options_before_target_msvc() } else { cflags.c_options_before_target() }
	mut optimization_options := ''
	mut debug_options := ''
	if c.pref.is_prod {
		optimization_options = if c.pref.ccompiler == 'msvc' { '' } else { ' -O3 -fno-strict-aliasing -flto ' }
	}
	if c.pref.is_debug {
		debug_options = if c.pref.ccompiler == 'msvc' { '' } else { ' -g3 -no-pie ' }
	}
	mut libs := ''
	if false && c.pref.build_mode == .default_mode {
		libs = '"$pref.default_module_path/vlib/builtin.o"'
		if !os.exists(libs) {
			verror('`$libs` not found')
		}
		for imp in c.table.imports {
			libs += ' "$pref.default_module_path/vlib/${imp}.o"'
		}
	}
	args += ' $c.out_name_c '
	args += if c.pref.ccompiler == 'msvc' { cflags.c_options_after_target_msvc() } else { cflags.c_options_after_target() }
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
	mut cmd := '$mingw_cc $optimization_options $debug_options -std=gnu11 $args -municode'
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
	if v.pref.os == .windows {
		// Cross compiling for Windows
		$if !windows {
			v.pref.ccompiler = mingw_cc
		}
	}
	opath := v.pref.cache_manager.postfix_with_key2cpath('.o', obj_path)
	if os.exists(opath) {
		return
	}
	if os.exists(obj_path) {
		// Some .o files are distributed with no source
		// for example thirdparty\tcc\lib\openlibm.o
		// the best we can do for them is just copy them,
		// and hope that they work with any compiler...
		os.cp(obj_path, opath)
		return
	}
	println('$obj_path not found, building it in $opath ...')
	cfile := '${obj_path[..obj_path.len - 2]}.c'
	btarget := moduleflags.c_options_before_target()
	atarget := moduleflags.c_options_after_target()
	cppoptions := if v.pref.ccompiler.contains('++') { ' -fpermissive -w ' } else { '' }
	cmd := '$v.pref.ccompiler $cppoptions $v.pref.third_party_option $btarget -o "$opath" -c "$cfile" $atarget'
	res := os.exec(cmd) or {
		eprintln('exec failed for thirdparty object build cmd:\n$cmd')
		verror(err)
		return
	}
	if res.exit_code != 0 {
		eprintln('failed thirdparty object build cmd:\n$cmd')
		verror(res.output)
		return
	}
	v.pref.cache_manager.save('.description.txt', obj_path, '${obj_path:-30} @ $cmd\n')
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
