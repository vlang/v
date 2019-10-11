// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	os
	time
)

fn (v mut V) cc() {
	v.build_thirdparty_obj_files()
	// Just create a C/JavaScript file and exit
	// for example: `v -o v.c compiler`
	if v.out_name.ends_with('.c') || v.out_name.ends_with('.js') {
		// Translating V code to JS by launching vjs
		$if !js {
			if v.out_name.ends_with('.js') {
				vexe := os.executable()
				vjs_path := vexe + 'js'
				dir := os.dir(vexe)
				if !os.file_exists(vjs_path) {
					println('V.js compiler not found, building...')
					ret := os.system('$vexe -o $vjs_path -os js $dir/compiler')
					if ret == 0 {
						println('Done.')
					} else {
						println('Failed.')
						exit(1)
					}	
				}	
				ret := os.system('$vjs_path -o $v.out_name $v.dir')
				if ret == 0 {
					println('Done. Run it with `node $v.out_name`')
					println('JS backend is at a very early stage.')
				}	
			}
		}
		os.mv(v.out_name_c, v.out_name)
		exit(0)
	}
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
	
	//linux_host := os.user_os() == 'linux'
	v.log('cc() isprod=$v.pref.is_prod outname=$v.out_name')
	mut a := [v.pref.cflags, '-std=gnu11', '-w'] // arguments for the C compiler

	if v.pref.is_so {
		a << '-shared -fPIC '// -Wl,-z,defs'
		v.out_name = v.out_name + '.so'
	}
	if v.pref.build_mode == .build_module {
		// Create the modules & out directory if it's not there.
		out_dir := '$v_modules_path${os.PathSeparator}$v.dir'
		if !os.dir_exists(out_dir) {
			// create recursive
			mut mkpath := v_modules_path
			for subdir in v.dir.split(os.PathSeparator) {
				mkpath += os.PathSeparator + subdir
				if !os.dir_exists(mkpath) {
					os.mkdir(mkpath)
				}
			}
			//os.mkdir(out_dir)
		}
		v.out_name = '${out_dir}.o' //v.out_name
		println('Building ${v.out_name}...')
	}

	mut debug_options := ''
	mut optimization_options := '-O2'
	if v.pref.ccompiler.contains('clang') {
		if v.pref.is_debuggable {
			debug_options = '-g -O0'
		}
		optimization_options = '-O3 -flto'
	}
	if v.pref.ccompiler.contains('gcc') {
		if v.pref.is_debug {
			debug_options = '-g3'
		}
		optimization_options = '-O3 -fno-strict-aliasing -flto'
	}

	if v.pref.is_prod {
		a << optimization_options
	}
	else {
		a << debug_options
	}

	if v.pref.is_debuggable && os.user_os() != 'windows'{
		a << ' -rdynamic ' // needed for nicer symbolic backtraces
	}

	if v.os != .msvc && v.os != .freebsd {
		a << '-Werror=implicit-function-declaration'
	}

	for f in v.generate_hotcode_reloading_compiler_flags() {
		a << f
	}

	mut libs := ''// builtin.o os.o http.o etc
	if v.pref.build_mode == .build_module {
		a << '-c'
	}
	else if v.pref.is_debug {
		vexe := os.executable()
		builtin_o_path := '$v_modules_path/vlib/builtin.o'
		if os.file_exists(builtin_o_path) {
			libs = builtin_o_path
		} else {
			println('$builtin_o_path not found... building module builtin')
			os.system('$vexe build module vlib/builtin')
		}
		for imp in v.table.imports {
			if imp.contains('vweb') { continue } // not working
			if imp == 'webview' { continue }
			
			imp_path := imp.replace('.', os.PathSeparator)
			path := 	'$v_modules_path/vlib/${imp_path}.o'
			println('adding ${imp_path}.o')
			if os.file_exists(path) {
				libs += ' ' + path
			} else {
				println('$path not found... building module $imp')
				os.system('$vexe build module vlib/$imp_path')
			}	
		}
	}
	if v.pref.sanitize {
		a << '-fsanitize=leak'
	}
	// Cross compiling linux TODO
	/*
	sysroot := '/tmp/lld/linuxroot/'
	if v.os == .linux && !linux_host {
		// Build file.o
		a << '-c --sysroot=$sysroot -target x86_64-linux-gnu'
		// Right now `out_name` can be `file`, not `file.o`
		if !v.out_name.ends_with('.o') {
			v.out_name = v.out_name + '.o'
		}
	}
	*/
	// Cross compiling windows
	//
	// Output executable name
	a << '-o "$v.out_name"'
	if os.dir_exists(v.out_name) {
		verror('\'$v.out_name\' is a directory')
	}
	// macOS code can include objective C  TODO remove once objective C is replaced with C
	if v.os == .mac {
		a << '-x objective-c'
	}
	// The C file we are compiling
	a << '"$v.out_name_c"'
	if v.os == .mac {
		a << '-x none'
	}
	// Min macos version is mandatory I think?
	if v.os == .mac {
		a << '-mmacosx-version-min=10.7'
	}
	cflags := v.get_os_cflags()

	// add .o files
	a << cflags.c_options_only_object_files()
	
	// add all flags (-I -l -L etc) not .o files
	a << cflags.c_options_without_object_files()
	
	a << libs
	// Without these libs compilation will fail on Linux
	// || os.user_os() == 'linux'
	if v.pref.build_mode != .build_module && (v.os == .linux || v.os == .freebsd || v.os == .openbsd ||
		v.os == .netbsd || v.os == .dragonfly || v.os == .solaris) {
		a << '-lm -lpthread '
		// -ldl is a Linux only thing. BSDs have it in libc.
		if v.os == .linux {
			a << ' -ldl '
		}
	}

	if v.os == .js && os.user_os() == 'linux' {
		a << '-lm'
	}
	
	args := a.join(' ')
	cmd := '${v.pref.ccompiler} $args'
	// Run
	if v.pref.show_c_cmd || v.pref.is_verbose {
		println('\n==========')
		println(cmd)
	}
	ticks := time.ticks()
	res := os.exec(cmd) or { verror(err) return }
	if res.exit_code != 0 {

		if res.exit_code == 127 {
			// the command could not be found by the system
			verror('C compiler error, while attempting to run: \n' +
				'-----------------------------------------------------------\n' +
				'$cmd\n' +
				'-----------------------------------------------------------\n' +
				'Probably your C compiler is missing. \n' +
				'Please reinstall it, or make it available in your PATH.')
		}

		if v.pref.is_debug {
			println(res.output)
		} else {
			partial_output := res.output.limit(200).trim_right('\r\n')
			print(partial_output)
			if res.output.len > partial_output.len {
				println('...\n(Use `v -debug` to print the entire error message)\n')
			}else{
				println('')
			}
		}
		verror('C error. This should never happen. ' +
			'Please create a GitHub issue: https://github.com/vlang/v/issues/new/choose')
	}
	diff := time.ticks() - ticks
	// Print the C command
	if v.pref.show_c_cmd || v.pref.is_verbose {
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
	if !v.pref.is_debug && v.out_name_c != 'v.c' {
		os.rm(v.out_name_c)
	}
	if v.pref.compress {
		$if windows {
			println('-compress does not work on Windows for now')
			return
		}	
		ret := os.system('strip $v.out_name')
		if ret != 0 {
			println('strip failed')
			return
		}
		ret2 := os.system('upx --lzma -qqq $v.out_name')
		if ret2 != 0 {
			println('upx failed')
			$if mac {
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
}


fn (c mut V) cc_windows_cross() {
	if !c.out_name.ends_with('.exe') {
		c.out_name = c.out_name + '.exe'
	}
	mut args := '-o $c.out_name -w -L. '
	cflags := c.get_os_cflags()
	// -I flags
	args += cflags.c_options_before_target()
	mut libs := ''
	if c.pref.build_mode == .default_mode {
		libs = '"$v_modules_path/vlib/builtin.o"'
		if !os.file_exists(libs) {
				println('`$libs` not found')
				exit(1)
		}
		for imp in c.table.imports {
				libs += ' "$v_modules_path/vlib/${imp}.o"'
		}
	}
	args += ' $c.out_name_c '
	args += cflags.c_options_after_target()
	println('Cross compiling for Windows...')
	winroot := '$v_modules_path/winroot'
	if !os.dir_exists(winroot) {
		winroot_url := 'https://github.com/vlang/v/releases/download/v0.1.10/winroot.zip'
		println('"$winroot" not found.')
		println('Download it from $winroot_url and save it in $v_modules_path')
		println('Unzip it afterwards.\n')
		println('winroot.zip contains all library and header files needed '+
			'to cross-compile for Windows.')
		exit(1)	
	}
	mut obj_name := c.out_name
	obj_name = obj_name.replace('.exe', '')
	obj_name = obj_name.replace('.o.o', '.o')
	include := '-I $winroot/include '
	cmd := 'clang -o $obj_name -w $include -m32 -c -target x86_64-win32 $v_modules_path/$c.out_name_c'
	if c.pref.show_c_cmd {
			println(cmd)
	}
	if os.system(cmd) != 0 {
		println('Cross compilation for Windows failed. Make sure you have clang installed.')
		exit(1)
	}
	if c.pref.build_mode != .build_module {
		link_cmd := 'lld-link $obj_name $winroot/lib/libcmt.lib ' +
		'$winroot/lib/libucrt.lib $winroot/lib/kernel32.lib $winroot/lib/libvcruntime.lib ' +
		'$winroot/lib/uuid.lib'
		if c.pref.show_c_cmd {
			println(link_cmd)
		}

		if os.system(link_cmd)  != 0 {
			println('Cross compilation for Windows failed. Make sure you have lld linker installed.')
			exit(1)
		}
		// os.rm(obj_name)
	}
	println('Done!')
}

fn (c &V) build_thirdparty_obj_files() {
	for flag in c.get_os_cflags() {
		if flag.value.ends_with('.o') {			
			rest_of_module_flags := c.get_rest_of_module_cflags( flag )
			if c.os == .msvc {
				build_thirdparty_obj_file_with_msvc(flag.value, rest_of_module_flags)
			}
			else {
				build_thirdparty_obj_file(flag.value, rest_of_module_flags)
			}
		}
	}
}

fn find_c_compiler() string {
	args := env_vflags_and_os_args().join(' ')
	defaultcc := find_c_compiler_default()
	return get_arg( args, 'cc', defaultcc )
}

fn find_c_compiler_default() string {
	//fast_clang := '/usr/local/Cellar/llvm/8.0.0/bin/clang'
	//if os.file_exists(fast_clang) {
	//	return fast_clang
	//}
	// TODO fix $if after 'string'
	$if windows {	return 'gcc' }
	return 'cc'
}

fn find_c_compiler_thirdparty_options() string {
	fullargs := env_vflags_and_os_args()
	mut cflags := get_cmdline_cflags( fullargs )
	$if !windows {
		cflags += ' -fPIC'
	}
	if '-m32' in fullargs {
		cflags += ' -m32'
	}
	return cflags
}

fn get_cmdline_cflags(args []string) string {
	mut cflags := ''
	for ci, cv in args {
		if cv == '-cflags' {
			cflags += args[ci+1] + ' '
		}
	}
	return cflags
}
