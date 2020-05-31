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
c_error_info = '
==================
C error. This should never happen.

If you were not working with C interop, please raise an issue on GitHub:

https://github.com/vlang/v/issues/new/choose

You can also use #help on Discord: https://discord.gg/vlang
')


fn todo() {
}

fn (v &Builder) no_cc_installed() bool {
	$if windows {
		os.exec('$v.pref.ccompiler -v') or {
			if v.pref.is_verbose {
				println('C compiler not found, trying to build with msvc...')
			}
			return true
		}
	}
	return false
}

fn (mut v Builder) cc() {
	if os.executable().contains('vfmt') {
		return
	}
	if v.pref.is_verbose {
		println('builder.cc() pref.out_name="$v.pref.out_name"')
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
	$if windows {
		if v.pref.ccompiler == 'msvc' || v.no_cc_installed() {
			v.cc_msvc()
			return
		}
	}
	// arguments for the C compiler
	mut a := [ v.pref.cflags, '-std=gnu11', '-Wall', '-Wextra',
		// TODO : activate -Werror once no warnings remain
		// '-Werror',
		// TODO : try and remove the below workaround options when the corresponding
		// warnings are totally fixed/removed
		'-Wno-unused-variable',
		// '-Wno-unused-but-set-variable',
		'-Wno-unused-parameter', '-Wno-unused-result', '-Wno-unused-function', '-Wno-missing-braces',
			'-Wno-unused-label'
	]
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
					a << '-m64'
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
		a << '-fPIC' // -Wl,-z,defs'
		$if macos {
			v.pref.out_name += '.dylib'
		} $else {
			v.pref.out_name += '.so'
		}
	}
	if v.pref.is_bare {
		a << '-fno-stack-protector'
		a << '-ffreestanding'
		linker_flags << '-static'
		linker_flags << '-nostdlib'
	}
	if v.pref.build_mode == .build_module {
		// Create the modules & out directory if it's not there.
		mut out_dir := if v.pref.path.starts_with('vlib') { '${pref.default_module_path}${os.path_separator}cache${os.path_separator}$v.pref.path' } else { '${pref.default_module_path}${os.path_separator}$v.pref.path' }
		pdir := out_dir.all_before_last(os.path_separator)
		if !os.is_dir(pdir) {
			os.mkdir_all(pdir)
		}
		v.pref.out_name = '${out_dir}.o' // v.out_name
		println('Building ${v.pref.out_name}...')
	}
	debug_mode := v.pref.is_debug
	mut debug_options := '-g'
	mut optimization_options := '-O2'
	mut guessed_compiler := v.pref.ccompiler
	if guessed_compiler == 'cc' && v.pref.is_prod {
		// deliberately guessing only for -prod builds for performance reasons
		if ccversion := os.exec('cc --version') {
			if ccversion.exit_code == 0 {
				if ccversion.output.contains('This is free software;') && ccversion.output.contains('Free Software Foundation, Inc.') {
					guessed_compiler = 'gcc'
				}
				if ccversion.output.contains('clang version ') {
					guessed_compiler = 'clang'
				}
			}
		}
	}
	//
	is_cc_clang := v.pref.ccompiler.contains('clang') || guessed_compiler == 'clang'
	is_cc_tcc := v.pref.ccompiler.contains('tcc') || guessed_compiler == 'tcc'
	is_cc_gcc := v.pref.ccompiler.contains('gcc') || guessed_compiler == 'gcc'
	// is_cc_msvc := v.pref.ccompiler.contains('msvc') || guessed_compiler == 'msvc'
	//
	if is_cc_clang {
		if debug_mode {
			debug_options = '-g -O0 -no-pie'
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
		a << debug_options
		$if macos {
			a << ' -ferror-limit=5000 '
		}
	}
	if v.pref.is_prod {
		a << optimization_options
	}
	if debug_mode && os.user_os() != 'windows' {
		linker_flags << ' -rdynamic ' // needed for nicer symbolic backtraces
	}
	if v.pref.ccompiler != 'msvc' && v.pref.os != .freebsd {
		a << '-Werror=implicit-function-declaration'
	}
	if v.pref.is_liveshared || v.pref.is_livemain {
		if v.pref.os == .linux || os.user_os() == 'linux' {
			linker_flags << '-rdynamic'
		}
		if v.pref.os == .mac || os.user_os() == 'mac' {
			a << '-flat_namespace'
		}
	}
	mut libs := '' // builtin.o os.o http.o etc
	if v.pref.build_mode == .build_module {
		a << '-c'
	} else if v.pref.use_cache {
		/*
		QTODO
		builtin_o_path := os.join_path(pref.default_module_path, 'cache', 'vlib', 'builtin.o')
		a << builtin_o_path.replace('builtin.o', 'strconv.o') // TODO hack no idea why this is needed
		if os.exists(builtin_o_path) {
			libs = builtin_o_path
		}
		else {
			println('$builtin_o_path not found... building module builtin')
			os.system('$vexe build module vlib${os.path_separator}builtin')
		}
		for imp in v.table.imports {
			if imp.contains('vweb') {
				continue
			} // not working
			if imp == 'webview' {
				continue
			}
			imp_path := imp.replace('.', os.path_separator)
			path := '${pref.default_module_path}${os.path_separator}cache${os.path_separator}vlib${os.path_separator}${imp_path}.o'
			// println('adding ${imp_path}.o')
			if os.exists(path) {
				libs += ' ' + path
			}
			else {
				println('$path not found... building module $imp')
				os.system('$vexe build module vlib${os.path_separator}$imp_path')
			}
			if path.ends_with('vlib/ui.o') {
				a << '-framework Cocoa -framework Carbon'
			}
		}
		*/
	}
	if v.pref.sanitize {
		a << '-fsanitize=leak'
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
	a << '-o "$v.pref.out_name"'
	if os.is_dir(v.pref.out_name) {
		verror("'$v.pref.out_name' is a directory")
	}
	// macOS code can include objective C  TODO remove once objective C is replaced with C
	if v.pref.os == .mac {
		a << '-x objective-c'
	}
	// The C file we are compiling
	a << '"$v.out_name_c"'
	if v.pref.os == .mac {
		a << '-x none'
	}
	// Min macos version is mandatory I think?
	if v.pref.os == .mac {
		a << '-mmacosx-version-min=10.7'
	}
	if v.pref.os == .windows {
		a << '-municode'
	}
	cflags := v.get_os_cflags()
	// add .o files
	a << cflags.c_options_only_object_files()
	// add all flags (-I -l -L etc) not .o files
	a << cflags.c_options_without_object_files()
	a << libs
	// For C++ we must be very tolerant
	if guessed_compiler.contains('++') {
		a << '-fpermissive'
		a << '-w'
	}
	if v.pref.use_cache {
		//vexe := pref.vexe_path()

		cached_modules:= [ 'builtin', 'os', 'math', 'strconv', 'strings']
		for cfile in cached_modules{
			ofile := os.join_path(pref.default_module_path, 'cache', 'vlib', cfile + '.o')
			if !os.exists(ofile) {
				println('${cfile}.o is missing. Building...')
				println('$vexe build-module vlib/$cfile')
				os.system('$vexe build-module vlib/$cfile')
			}
			a << ofile
		}
		if !is_cc_tcc {
			$if linux {
				linker_flags << '-Xlinker -z'
				linker_flags << '-Xlinker muldefs'
			}
		}
	}
	// Without these libs compilation will fail on Linux
	// || os.user_os() == 'linux'
	if !v.pref.is_bare && v.pref.build_mode != .build_module && v.pref.os in [ .linux, .freebsd,
		.openbsd, .netbsd, .dragonfly, .solaris, .haiku] {
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
	args := a.join(' ') + ' ' + linker_flags.join(' ')
	start:
	todo()
	// TODO remove
	cmd := '${v.pref.ccompiler} $args'
	// Run
	if v.pref.is_verbose || v.pref.show_cc {
		println('\n==========')
		println(cmd)
	}
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
	if res.exit_code != 0 {
		// the command could not be found by the system
		if res.exit_code == 127 {
			$if linux {
				// TCC problems on linux? Try GCC.
				if v.pref.ccompiler.contains('tcc') {
					v.pref.ccompiler = 'cc'
					goto start
				}
			}
			verror('C compiler error, while attempting to run: \n' + '-----------------------------------------------------------\n' +
				'$cmd\n' + '-----------------------------------------------------------\n' + 'Probably your C compiler is missing. \n' +
				'Please reinstall it, or make it available in your PATH.\n\n' + missing_compiler_info())
		}
		if v.pref.is_debug {
			eword := 'error:'
			khighlight := if term.can_show_color_on_stdout() { term.red(eword) } else { eword }
			println(res.output.replace(eword, khighlight))
			verror(c_error_info)
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
			verror(c_error_info)
		}
	}
	diff := time.ticks() - ticks
	// Print the C command
	if v.pref.is_verbose {
		println('${v.pref.ccompiler} took $diff ms')
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
	if !v.pref.keep_c && v.out_name_c != 'v.c' {
		os.rm(v.out_name_c)
	}
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
}

fn (mut c Builder) cc_linux_cross() {
	parent_dir := os.home_dir() + '.vmodules'
	sysroot := os.home_dir() + '.vmodules/linuxroot/'
	if !os.is_dir(sysroot) {
		println('Downloading files for Linux cross compilation (~18 MB)...')
		zip_file := sysroot[..sysroot.len-1] + '.zip'
		os.system('curl -L -o $zip_file https://github.com/vlang/v/releases/download/0.1.27/linuxroot.zip ')
		os.system('unzip -q $zip_file -d $parent_dir')
		if !os.is_dir(sysroot) {
			println('Failed to download.')
			exit(1)
		}
	}
	mut cc_args := '-fPIC -w -c -target x86_64-linux-gnu -c -o x.o $c.out_name_c -I $sysroot/include'
	if os.system('cc $cc_args') != 0 {
		println('Cross compilation for Linux failed. Make sure you have clang installed.')
	}
	mut args := [
		'-L SYSROOT/usr/lib/x86_64-linux-gnu/'
		'--sysroot=SYSROOT -v -o hi -m elf_x86_64'
		'-dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2'
		'SYSROOT/crt1.o SYSROOT/crti.o x.o'
		'SYSROOT/lib/x86_64-linux-gnu/libc.so.6'
		'-lc'
		'SYSROOT/crtn.o'
	]
	mut s := args.join(' ')
	s = s.replace('SYSROOT', sysroot)
	if	os.system('$sysroot/ld.lld ' + s) != 0 {
		println('Cross compilation for Linux failed. Make sure you have clang installed.')
	}
	println(c.pref.out_name + ' has been successfully compiled')
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
		debug_options =  if c.pref.ccompiler == 'msvc' { '' } else { ' -g3 -no-pie ' }
	}
	mut libs := ''
	if false && c.pref.build_mode == .default_mode {
		libs = '"${pref.default_module_path}/vlib/builtin.o"'
		if !os.exists(libs) {
			println('`$libs` not found')
			exit(1)
		}
		for imp in c.table.imports {
			libs += ' "${pref.default_module_path}/vlib/${imp}.o"'
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
	if os.user_os() !in ['mac', 'darwin','linux'] {
		println(os.user_os())
		panic('your platform is not supported yet')
	}
	mut cmd := 'x86_64-w64-mingw32-gcc'
	cmd += ' $optimization_options $debug_options -std=gnu11 $args -municode'
	//cmd := 'clang -o $obj_name -w $include -m32 -c -target x86_64-win32 ${pref.default_module_path}/$c.out_name_c'
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

fn (c &Builder) build_thirdparty_obj_files() {
	for flag in c.get_os_cflags() {
		if flag.value.ends_with('.o') {
			rest_of_module_flags := c.get_rest_of_module_cflags(flag)
			if c.pref.ccompiler == 'msvc' || c.no_cc_installed() {
				build_thirdparty_obj_file_with_msvc(flag.value, rest_of_module_flags)
			} else {
				c.build_thirdparty_obj_file(flag.value, rest_of_module_flags)
			}
		}
	}
}

fn (v &Builder) build_thirdparty_obj_file(path string, moduleflags []cflag.CFlag) {
	obj_path := os.real_path(path)
	if os.exists(obj_path) {
		return
	}
	println('$obj_path not found, building it...')
	parent := os.dir(obj_path)
	files := os.ls(parent) or {
		panic(err)
	}
	mut cfiles := ''
	for file in files {
		if file.ends_with('.c') {
			cfiles += '"' + os.real_path(parent + os.path_separator + file) + '" '
		}
	}
	btarget := moduleflags.c_options_before_target()
	atarget := moduleflags.c_options_after_target()
	cppoptions := if v.pref.ccompiler.contains('++') { ' -fpermissive -w ' } else { '' }
	cmd := '$v.pref.ccompiler $cppoptions $v.pref.third_party_option $btarget -c -o "$obj_path" $cfiles $atarget '
	res := os.exec(cmd) or {
		println('failed thirdparty object build cmd: $cmd')
		verror(err)
		return
	}
	if res.exit_code != 0 {
		println('failed thirdparty object build cmd: $cmd')
		verror(res.output)
		return
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

fn error_context_lines(text, keyword string, before, after int) []string {
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
