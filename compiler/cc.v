// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	os
	time
)

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
	if v.os != .msvc {
		a << '-Werror=implicit-function-declaration'
	}

	for f in v.generate_hotcode_reloading_compiler_flags() {
		a << f
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
	a << flags
	a << libs
	// macOS code can include objective C  TODO remove once objective C is replaced with C
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
	args := a.join(' ')
	cmd := '${v.pref.ccompiler} $args'
	if v.out_name.ends_with('.c') {
		os.mv( v.out_name_c, v.out_name )
		exit(0)
	}
	// Run
	if v.pref.show_c_cmd || v.pref.is_verbose {
		println('\n==========')
		println(cmd)
	}
	ticks := time.ticks()
	res := os.exec(cmd) or { panic(err) }
	if res.exit_code != 0 {

		if res.exit_code == 127 {
			// the command could not be found by the system
			panic('C compiler error, while attempting to run: \n' +
				'-----------------------------------------------------------\n' +
				'$cmd\n' +
				'-----------------------------------------------------------\n' +
				'Probably your C compiler is missing. \n' +
				'Please reinstall it, or make it available in your PATH.')
		}

		if v.pref.is_debug {
			println(res.output)
		} else {
			print(res.output.limit(200))
			if res.output.len > 200 {
				println('...\n(Use `v -debug` to print the entire error message)\n')
			}
		}
		panic('C error. This should never happen. ' +
			'Please create a GitHub issue: https://github.com/vlang/v/issues/new/choose')
	}
	diff := time.ticks() - ticks
	// Print the C command
	if v.pref.show_c_cmd || v.pref.is_verbose {
		println('${v.pref.ccompiler} took $diff ms')
		println('=========\n')
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
		'/usr/lib/x86_64-linux-gnu/crtn.o') or {
			panic(err)
		}
		println(ress.output)
		println('linux cross compilation done. resulting binary: "$v.out_name"')
	}
	if !v.pref.is_debug && v.out_name_c != 'v.c' && v.out_name_c != 'v_macos.c' {
		os.rm(v.out_name_c)
	}
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

		if os.system(link_cmd)  != 0 {
			println('Cross compilation for Windows failed. Make sure you have lld linker installed.')
			exit(1)
		}
		// os.rm(obj_name)
	}
	println('Done!')
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
	$if windows {	return '' }
	return '-fPIC'
}




