// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v.cflag
import v.pref
import v.util
import v.vcache
import term

const c_std = 'c99'
const c_std_gnu = 'gnu99'
const cpp_std = 'c++17'
const cpp_std_gnu = 'gnu++17'

const c_verror_message_marker = 'VERROR_MESSAGE '

const current_os = os.user_os()

const c_compilation_error_title = 'C compilation error'

fn (mut v Builder) show_c_compiler_output(ccompiler string, res os.Result) {
	header := '======== Output of the C Compiler (${ccompiler}) ========'
	println(header)
	println(res.output.trim_space())
	println('='.repeat(header.len))
}

fn (mut v Builder) post_process_c_compiler_output(ccompiler string, res os.Result) {
	if res.exit_code == 0 {
		if v.pref.reuse_tmpc {
			return
		}
		for tmpfile in v.pref.cleanup_files {
			if os.is_file(tmpfile) {
				if v.pref.is_verbose {
					eprintln('>> remove tmp file: ${tmpfile}')
				}
				os.rm(tmpfile) or {}
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
		khighlight := highlight_word(eword)
		println(res.output.trim_right('\r\n').replace(eword, khighlight))
	} else {
		if res.output.len < 30 {
			println(res.output)
		} else {
			trimmed_output := res.output.trim_space()
			original_elines := trimmed_output.split_into_lines()
			mlines := 12
			cut_off_limit := if original_elines.len > mlines + 3 { mlines } else { mlines + 3 }
			elines := error_context_lines(trimmed_output, 'error:', 1, cut_off_limit)
			header := '================== ${c_compilation_error_title} (from ${ccompiler}): =============='
			println(header)
			for eline in elines {
				println('cc: ${eline}')
			}
			if original_elines.len != elines.len {
				println('...')
				println('cc: ${original_elines#[-1..][0]}')
				println('(note: the original output was ${original_elines.len} lines long; it was truncated to its first ${elines.len} lines + the last line)')
			}
			println('='.repeat(header.len))
			println('(You can pass `-cg`, or `-show-c-output` as well, to print all the C error messages).')
		}
	}
	if os.getenv('V_NO_C_ERROR_INFO') != '' {
		eprintln('> V_NO_C_ERROR_INFO is obsoleted by either setting VQUIET to 1, or by passing `-q` on the command line')
		exit(1)
	}
	if v.pref.is_quiet {
		exit(1)
	}
	mut more_suggestions := ''
	if res.output.contains('o: unrecognized file type')
		|| res.output.contains('.o: file not recognized') {
		more_suggestions += '\n${highlight_word('Suggestion')}: try `v wipe-cache`, then repeat your compilation.'
	}
	verror('
==================
C error found. It should never happen, when compiling pure V code.
This is a V compiler bug, please report it using `v bug file.v`,
or goto https://github.com/vlang/v/issues/new/choose .
You can also use #help on Discord: https://discord.gg/vlang .${more_suggestions}')
}

fn (mut v Builder) show_cc(cmd string, response_file string, response_file_content string) {
	if v.pref.is_verbose || v.pref.show_cc {
		println('> C compiler cmd: ${cmd}')
		if v.pref.show_cc && !v.pref.no_rsp {
			println('> C compiler response file "${response_file}":')
			println(response_file_content)
		}
	}
}

pub enum CC {
	tcc
	gcc
	icc
	msvc
	clang
	emcc
	unknown
}

pub struct CcompilerOptions {
pub mut:
	guessed_compiler string
	shared_postfix   string // .so, .dll

	debug_mode bool
	cc         CC

	env_cflags  string // prepended *before* everything else
	env_ldflags string // appended *after* everything else

	args         []string // ordinary C options like `-O2`
	wargs        []string // for `-Wxyz` *exclusively*
	pre_args     []string // options that should go before .o_args
	o_args       []string // for `-o target`
	source_args  []string // for `x.tmp.c`
	post_args    []string // options that should go after .o_args
	linker_flags []string // `-lm`
	ldflags      []string // `-labcd' from `v -ldflags "-labcd"`
}

fn (mut v Builder) setup_ccompiler_options(ccompiler string) {
	mut ccoptions := CcompilerOptions{}

	mut debug_options := ['-g']
	mut optimization_options := ['-O2']
	// arguments for the C compiler
	ccoptions.args = [v.pref.cflags]
	ccoptions.ldflags = [v.pref.ldflags]
	ccoptions.wargs = [
		'-Wall',
		'-Wextra',
		'-Werror',
		// if anything, these should be a `v vet` warning instead:
		'-Wno-unused-parameter',
		'-Wno-unused',
		'-Wno-type-limits',
		'-Wno-tautological-compare',
		// these cause various issues:
		'-Wno-shadow', // the V compiler already catches this for user code, and enabling this causes issues with e.g. the `it` variable
		'-Wno-int-to-pointer-cast', // gcc version of the above
		'-Wno-trigraphs', // see stackoverflow.com/a/8435413
		'-Wno-missing-braces', // see stackoverflow.com/q/13746033
		'-Wno-enum-conversion', // silences `.dst_factor_rgb = sokol__gfx__BlendFactor__one_minus_src_alpha`
		'-Wno-enum-compare', // silences `if (ev->mouse_button == sokol__sapp__MouseButton__left) {`
		// enable additional warnings:
		'-Wno-unknown-warning', // if a C compiler does not understand a certain flag, it should just ignore it
		'-Wno-unknown-warning-option', // clang equivalent of the above
		'-Wno-excess-initializers', // vlib/v/tests/struct_init_with_complex_fields_test.v fails without that on macos clang 13
		'-Wdate-time',
		'-Wduplicated-branches',
		'-Wduplicated-cond',
		'-Winit-self',
		'-Winvalid-pch',
		'-Wjump-misses-init',
		'-Wlogical-op',
		'-Wmultichar',
		'-Wnested-externs',
		'-Wnull-dereference',
		'-Wpacked',
		'-Wpointer-arith',
	]
	if v.pref.os == .ios {
		ccoptions.args << '-fobjc-arc'
	}
	if v.pref.os == .macos && os.exists('/opt/procursus') {
		ccoptions.linker_flags << '-Wl,-rpath,/opt/procursus/lib'
	}
	mut user_darwin_version := 999_999
	mut user_darwin_ppc := false
	$if macos {
		user_darwin_version = os.uname().release.split('.')[0].int()
		if os.uname().machine == 'Power Macintosh' {
			user_darwin_ppc = true
		}
	}
	ccoptions.debug_mode = v.pref.is_debug
	ccoptions.guessed_compiler = v.pref.ccompiler
	if ccoptions.guessed_compiler == 'cc' {
		cc_ver := os.execute('cc --version').output
		if cc_ver.replace('\n', '').contains('Free Software Foundation, Inc.This is free software;') {
			// Also covers `g++`, `g++-9`, `g++-11` etc.
			ccoptions.cc = .gcc
		} else if cc_ver.contains('clang version ') {
			ccoptions.cc = .clang
		} else {
			if v.pref.is_verbose {
				eprintln('failed to detect C compiler from version info `${cc_ver}`')
			}
			eprintln('Compilation with unknown C compiler')
			ccoptions.cc = .unknown
		}
	} else {
		cc_file_name := os.file_name(ccompiler)
		ccoptions.cc = match true {
			// vfmt off
			cc_file_name.contains('tcc') || ccoptions.guessed_compiler == 'tcc' { .tcc }
			cc_file_name.contains('gcc') || cc_file_name.contains('g++') || ccoptions.guessed_compiler == 'gcc' { .gcc }
			cc_file_name.contains('clang') || ccoptions.guessed_compiler == 'clang' { .clang }
			cc_file_name.contains('msvc') || ccoptions.guessed_compiler == 'msvc' { .msvc }
			cc_file_name.contains('icc') || ccoptions.guessed_compiler == 'icc' { .icc }
			cc_file_name.contains('emcc') || ccoptions.guessed_compiler == 'emcc' { .emcc }
			else { .unknown }
			// vfmt on
		}
		if ccoptions.cc == .unknown {
			eprintln('Compilation with unknown C compiler `${cc_file_name}`')
		}
	}

	// Add -fwrapv to handle UB overflows
	if ccoptions.cc in [.gcc, .clang, .tcc] && v.pref.os in [.macos, .linux, .openbsd, .windows] {
		ccoptions.args << '-fwrapv'
	}

	// For C++ we must be very tolerant
	if ccoptions.guessed_compiler.contains('++') {
		ccoptions.args << '-fpermissive'
		ccoptions.args << '-w'
	}
	if ccoptions.cc == .clang {
		if ccoptions.debug_mode {
			debug_options = ['-g', '-O0']
		}
		optimization_options = ['-O3']
		mut have_flto := true
		$if windows {
			have_flto = false
		}
		if v.pref.parallel_cc {
			have_flto = false
		}
		if have_flto {
			optimization_options << '-flto'
		}
		ccoptions.wargs << [
			'-Wno-tautological-bitwise-compare',
			'-Wno-enum-conversion', // used in vlib/sokol, where C enums in C structs are typed as V structs instead
			'-Wno-sometimes-uninitialized', // produced after exhaustive matches
			'-Wno-int-to-void-pointer-cast',
		]
	}
	if ccoptions.cc == .gcc {
		if ccoptions.debug_mode {
			debug_options = ['-g']
			if user_darwin_version > 9 {
				debug_options << '-no-pie'
			}
		}
		optimization_options = ['-O3']
		mut have_flto := true
		if v.pref.parallel_cc {
			have_flto = false
		}
		if have_flto {
			optimization_options << '-flto'
		}
	}
	if ccoptions.cc == .icc {
		if ccoptions.debug_mode {
			debug_options = ['-g']
			if user_darwin_version > 9 {
				debug_options << '-no-pie'
			}
		}
		optimization_options = ['-Ofast']
	}

	if ccoptions.debug_mode {
		ccoptions.args << debug_options
	}
	if v.pref.is_prod {
		// don't warn for vlib tests
		if ccoptions.cc == .tcc && !(v.parsed_files.len > 0
			&& v.parsed_files.last().path.contains('vlib')) {
			eprintln('Note: tcc is not recommended for -prod builds')
		}
		if !v.pref.no_prod_options {
			ccoptions.args << optimization_options
		}
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
	if v.pref.is_o {
		ccoptions.args << '-c'
	}

	ccoptions.shared_postfix = '.so'
	if v.pref.os == .macos {
		ccoptions.shared_postfix = '.dylib'
	}
	if v.pref.os == .windows {
		ccoptions.shared_postfix = '.dll'
	}
	if v.pref.is_shared {
		ccoptions.linker_flags << '-shared'
		$if !windows {
			ccoptions.args << '-fPIC' // -Wl,-z,defs'
		}
	}
	if v.pref.is_bare && v.pref.os != .wasm32 {
		ccoptions.args << '-fno-stack-protector'
		ccoptions.args << '-ffreestanding'
		ccoptions.linker_flags << '-static'
		ccoptions.linker_flags << '-nostdlib'
	} else if v.pref.os == .wasm32 {
		ccoptions.args << '--no-standard-libraries'
		ccoptions.args << '-target wasm32-unknown-unknown'
		ccoptions.args << '-static'
		ccoptions.args << '-nostdlib'
		ccoptions.args << '-ffreestanding'
		ccoptions.args << '-Wl,--export-all'
		ccoptions.args << '-Wl,--no-entry'
	}
	if ccoptions.debug_mode && current_os != 'windows' && v.pref.build_mode != .build_module {
		if ccoptions.cc != .tcc && current_os == 'macos' {
			ccoptions.linker_flags << '-Wl,-export_dynamic' // clang for mac needs export_dynamic instead of -rdynamic
		} else {
			if v.pref.ccompiler != 'x86_64-w64-mingw32-gcc' {
				// the mingw-w64-gcc cross compiler does not support -rdynamic, and windows/wine already does have nicer backtraces
				ccoptions.linker_flags << '-rdynamic' // needed for nicer symbolic backtraces
			}
		}
	}
	if v.pref.os == .freebsd {
		// Needed for -usecache on FreeBSD 13, otherwise we get `ld: error: duplicate symbol: _const_math__bits__de_bruijn32` errors there
		if ccoptions.cc != .tcc {
			ccoptions.linker_flags << '-Wl,--allow-multiple-definition'
		} else {
			// tcc needs this, otherwise it fails to compile the runetype.h system header with:
			// /usr/include/runetype.h:94: error: ';' expected (got "const")
			ccoptions.args << '-D__RUNETYPE_INTERNAL'
		}
	}

	// Fix 'braces around scalar initializer' errors
	// on OpenBSD with clang for cstrict mode
	if v.pref.os == .openbsd && ccoptions.cc == .clang {
		ccoptions.wargs << '-Wno-braced-scalar-init'
	}

	if ccompiler != 'msvc' && v.pref.os != .freebsd {
		ccoptions.wargs << '-Werror=implicit-function-declaration'
	}
	if ccoptions.cc == .tcc {
		// tcc 806b3f98 needs this flag too:
		ccoptions.wargs << '-Wno-write-strings'
	}
	if v.pref.is_liveshared || v.pref.is_livemain {
		if v.pref.os == .linux && v.pref.build_mode != .build_module {
			ccoptions.linker_flags << '-rdynamic'
		}
		if v.pref.os == .macos {
			ccoptions.args << '-flat_namespace'
		}
	}

	// macOS code can include objective C  TODO remove once objective C is replaced with C
	if v.pref.os in [.macos, .ios] {
		if ccoptions.cc != .tcc && !user_darwin_ppc && !v.pref.is_bare && ccompiler != 'musl-gcc' {
			ccoptions.source_args << '-x objective-c'
		}
	}
	// The C file we are compiling
	if !v.pref.parallel_cc { // parallel_cc uses its own split up c files
		ccoptions.source_args << v.tcc_quoted_path(v.out_name_c)
	}
	// Min macos version is mandatory I think?
	if v.pref.os == .macos {
		if v.pref.macosx_version_min != '0' {
			ccoptions.post_args << '-mmacosx-version-min=${v.pref.macosx_version_min}'
		}
	}
	if v.pref.os == .ios {
		if v.pref.is_ios_simulator {
			ccoptions.post_args << '-miphonesimulator-version-min=10.0'
		} else {
			ccoptions.post_args << '-miphoneos-version-min=10.0'
		}
	}
	if v.pref.os == .windows {
		ccoptions.post_args << v.get_subsystem_flag()
	}
	cflags := v.get_os_cflags()

	if v.pref.build_mode != .build_module {
		only_o_files := cflags.c_options_only_object_files()
		ccoptions.o_args << only_o_files
	}

	defines, others, libs := cflags.defines_others_libs()
	ccoptions.pre_args << defines
	ccoptions.pre_args << others
	ccoptions.linker_flags << libs
	if v.pref.use_cache && v.pref.build_mode != .build_module {
		if ccoptions.cc != .tcc {
			$if linux {
				ccoptions.linker_flags << '-Xlinker -z'
				ccoptions.linker_flags << '-Xlinker muldefs'
			}
		}
	}
	if ccoptions.cc == .tcc && 'no_backtrace' !in v.pref.compile_defines {
		ccoptions.post_args << '-bt25'
	}
	// Without these libs compilation will fail on Linux
	if !v.pref.is_bare && v.pref.build_mode != .build_module
		&& v.pref.os in [.linux, .freebsd, .openbsd, .netbsd, .dragonfly, .solaris, .haiku] {
		if v.pref.os in [.freebsd, .netbsd] {
			// Free/NetBSD: backtrace needs execinfo library while linking, also execinfo depends on elf.
			ccoptions.linker_flags << '-lexecinfo'
			ccoptions.linker_flags << '-lelf'
		}
	}
	ccoptions.env_cflags = os.getenv('CFLAGS')
	ccoptions.env_ldflags = os.getenv('LDFLAGS')
	if v.pref.os == .macos {
		if v.pref.use_cache {
			ccoptions.source_args << '-x none'
		} else {
			for flag in ccoptions.linker_flags {
				if flag.starts_with('-') {
					continue
				}
				if os.is_file(flag) {
					ccoptions.source_args << '-x none'
					break
				}
				path := if flag.starts_with('"') && flag.ends_with('"') {
					flag[1..flag.len - 1]
				} else {
					flag
				}
				if os.is_dir(os.dir(path)) {
					ccoptions.source_args << '-x none'
					break
				}
			}
		}
	}
	if !v.pref.no_std {
		if v.pref.os == .linux {
			ccoptions.source_args << '-std=${c_std_gnu}'
		} else {
			ccoptions.source_args << '-std=${c_std}'
		}
		ccoptions.source_args << '-D_DEFAULT_SOURCE'
	}
	$if trace_ccoptions ? {
		println('>>> setup_ccompiler_options ccompiler: ${ccompiler}')
		println('>>> setup_ccompiler_options ccoptions: ${ccoptions}')
	}
	v.ccoptions = ccoptions
	// setup the cache too, so that different compilers/options do not interfere:
	v.pref.cache_manager.set_temporary_options(v.thirdparty_object_args(v.ccoptions, [
		ccoptions.guessed_compiler,
	], false))
}

fn (v &Builder) all_args(ccoptions CcompilerOptions) []string {
	mut all := []string{}
	all << v.only_compile_args(ccoptions)
	all << v.only_linker_args(ccoptions)
	return all
}

pub fn (v &Builder) get_compile_args() []string {
	return v.only_compile_args(v.ccoptions)
}

fn (v &Builder) only_compile_args(ccoptions CcompilerOptions) []string {
	mut all := []string{}
	all << ccoptions.env_cflags
	if v.pref.is_cstrict {
		all << ccoptions.wargs
	}
	all << ccoptions.args
	all << ccoptions.o_args
	$if windows {
		// Adding default options for tcc, gcc and clang as done in msvc.v.
		// This is done before pre_args is added so that it can be overwritten if needed.
		// -Wl,-stack=16777216 == /F 16777216
		// -Werror=implicit-function-declaration == /we4013
		// /volatile:ms - there seems to be no equivalent,
		// normally msvc should use /volatile:iso
		// but it could have an impact on vinix if it is created with msvc.
		if ccoptions.cc != .msvc {
			if v.pref.os != .wasm32_emscripten {
				all << '-Wl,-stack=16777216'
			}
			if !v.pref.is_cstrict {
				all << '-Werror=implicit-function-declaration'
			}
		}
	}
	all << ccoptions.pre_args
	all << ccoptions.source_args
	all << ccoptions.post_args
	return all
}

pub fn (v &Builder) get_linker_args() []string {
	return v.only_linker_args(v.ccoptions)
}

fn (v &Builder) only_linker_args(ccoptions CcompilerOptions) []string {
	mut all := []string{}
	// in `build-mode`, we do not need -lxyz flags, since we are
	// building an (.o) object file, that will be linked later.
	if v.pref.build_mode != .build_module {
		all << ccoptions.linker_flags
		all << ccoptions.env_ldflags
		all << ccoptions.ldflags
	}
	return all
}

fn (mut v Builder) thirdparty_object_args(ccoptions CcompilerOptions, middle []string, cpp_file bool) []string {
	mut all := []string{}

	if !v.pref.no_std {
		if v.pref.os == .linux {
			if cpp_file {
				all << '-std=${cpp_std_gnu}'
			} else {
				all << '-std=${c_std_gnu}'
			}
		} else {
			if cpp_file {
				all << '-std=${cpp_std}'
			} else {
				all << '-std=${c_std}'
			}
		}
		all << '-D_DEFAULT_SOURCE'
	}

	sysroot := os.join_path(os.vmodules_dir(), 'linuxroot')
	mut cross_compiling_from_macos_to_linux := false
	if v.pref.os == .linux && v.pref.arch == .amd64 {
		$if macos {
			cross_compiling_from_macos_to_linux = true
		}
	}

	if cross_compiling_from_macos_to_linux {
		v.ensure_linuxroot_exists(sysroot)
		all << '-target x86_64-linux-gnu'
	}

	all << ccoptions.env_cflags
	all << ccoptions.args
	all << middle
	// NOTE do not append linker flags in .o build process,
	// compilers are inconsistent about how they handle:
	// all << ccoptions.env_ldflags
	// all << ccoptions.ldflags
	if cross_compiling_from_macos_to_linux {
		// add the system include/ folder after everything else,
		// so that local folders like thirdparty/mbedtls have a
		// chance to supply their own headers
		all << '-I'
		all << os.quoted_path('${sysroot}/include')
	}
	return all
}

fn (mut v Builder) setup_output_name() {
	if !v.pref.is_shared && v.pref.build_mode != .build_module && v.pref.os == .windows
		&& !v.pref.out_name.ends_with('.exe') {
		v.pref.out_name += '.exe'
	}
	// Output executable name
	v.log('cc() isprod=${v.pref.is_prod} outname=${v.pref.out_name}')
	if v.pref.is_shared {
		if !v.pref.out_name.ends_with(v.ccoptions.shared_postfix) {
			v.pref.out_name += v.ccoptions.shared_postfix
		}
	}
	if v.pref.build_mode == .build_module {
		v.pref.out_name = v.pref.cache_manager.mod_postfix_with_key2cpath(v.pref.path,
			'.o', v.pref.path) // v.out_name
		if v.pref.is_verbose {
			println('Building ${v.pref.path} to ${v.pref.out_name} ...')
		}
		v.pref.cache_manager.mod_save(v.pref.path, '.description.txt', v.pref.path, '${v.pref.path:-30} @ ${v.pref.cache_manager.vopts}\n') or {
			panic(err)
		}
		// println('v.ast.imports:')
		// println(v.ast.imports)
	}
	if os.is_dir(v.pref.out_name) {
		verror('${os.quoted_path(v.pref.out_name)} is a directory')
	}
	if !v.pref.parallel_cc {
		// parallel_cc sets its own `-o out_n.o`
		v.ccoptions.o_args << '-o ${v.tcc_quoted_path(v.pref.out_name)}'
	}
}

pub fn (mut v Builder) tcc_quoted_path(p string) string {
	if v.ccoptions.cc == .tcc && !v.pref.no_rsp {
		// tcc has a bug, that prevents it from being able to parse names quoted with ' in .rsp files :-|
		return '"${p}"'
	}
	return os.quoted_path(p)
}

pub fn (mut v Builder) cc() {
	if os.executable().contains('vfmt') {
		return
	}
	if v.pref.is_verbose {
		println('builder.cc() pref.out_name=${os.quoted_path(v.pref.out_name)}')
	}
	if v.pref.only_check_syntax {
		if v.pref.is_verbose {
			println('builder.cc returning early, since pref.only_check_syntax is true')
		}
		return
	}
	if v.pref.check_only {
		if v.pref.is_verbose {
			println('builder.cc returning early, since pref.check_only is true')
		}
		return
	}
	if v.pref.should_output_to_stdout() {
		// output to stdout
		content := os.read_file(v.out_name_c) or { panic(err) }
		println(content)
		os.rm(v.out_name_c) or {}
		return
	}
	// whether to just create a .c or .js file and exit, for example: `v -o v.c cmd.v`
	ends_with_c := v.pref.out_name.ends_with('.c')
	ends_with_js := v.pref.out_name.ends_with('.js')
	if ends_with_c || (ends_with_js && v.pref.os != .wasm32_emscripten) {
		v.pref.skip_running = true
		msg_mv := 'os.mv_by_cp ${os.quoted_path(v.out_name_c)} => ${os.quoted_path(v.pref.out_name)}'
		util.timing_start(msg_mv)
		// v.out_name_c may be on a different partition than v.out_name
		os.mv_by_cp(v.out_name_c, v.pref.out_name) or { panic(err) }
		util.timing_measure(msg_mv)
		return
	}
	// Cross compiling for Windows
	if v.pref.os == .windows && v.pref.ccompiler != 'msvc' {
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
	// Cross compiling for FreeBSD
	if v.pref.os == .freebsd {
		$if !freebsd {
			v.cc_freebsd_cross()
			return
		}
	}

	vexe := pref.vexe_path()
	vdir := os.dir(vexe)
	mut tried_compilation_commands := []string{}
	mut tcc_output := os.Result{}
	original_pwd := os.getwd()
	for {
		// try to compile with the chosen compiler
		// if compilation fails, retry again with another
		mut ccompiler := v.pref.ccompiler
		if v.pref.os == .wasm32 {
			ccompiler = 'clang'
		}
		v.setup_ccompiler_options(ccompiler)
		v.build_thirdparty_obj_files()
		v.setup_output_name()

		if v.pref.os != .windows && ccompiler.contains('++') {
			cpp_atomic_h_path := '${@VEXEROOT}/thirdparty/stdatomic/nix/cpp/atomic.h'
			if !os.exists(cpp_atomic_h_path) {
				for file in v.parsed_files {
					if file.imports.any(it.mod.contains('sync')) {
						$if trace_stdatomic_gen ? {
							eprintln('> creating ${cpp_atomic_h_path} ...')
						}
						cppgenv := '${@VEXEROOT}/thirdparty/stdatomic/nix/cpp/gen.v'
						os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(cppgenv)} ${os.quoted_path(ccompiler)}')
						break
					}
				}
			}
		}
		if v.pref.build_mode == .build_module {
			v.ccoptions.pre_args << '-c'
		}
		v.handle_usecache(vexe)
		$if windows {
			if ccompiler == 'msvc' {
				v.cc_msvc()
				return
			}
		}
		//
		all_args := v.all_args(v.ccoptions)
		v.dump_c_options(all_args)
		str_args := if v.pref.no_rsp {
			all_args.join(' ').replace('\n', ' ')
		} else {
			all_args.join(' ')
		}
		mut cmd := '${v.quote_compiler_name(ccompiler)} ${str_args}'
		if v.pref.parallel_cc {
			// In parallel cc mode, all we want in cc() is build the str_args.
			// Actual cc logic then happens in `parallel_cc()`
			v.str_args = str_args
			return
		}
		mut response_file := ''
		mut response_file_content := str_args
		if !v.pref.no_rsp {
			response_file = '${v.out_name_c}.rsp'
			response_file_content = str_args.replace('\\', '\\\\')
			rspexpr := '@${response_file}'
			cmd = '${v.quote_compiler_name(ccompiler)} ${os.quoted_path(rspexpr)}'
			write_response_file(response_file, response_file_content)
			if !v.ccoptions.debug_mode {
				v.pref.cleanup_files << response_file
			}
		}
		if !v.ccoptions.debug_mode {
			v.pref.cleanup_files << v.out_name_c
		}
		$if windows {
			if v.ccoptions.cc == .tcc {
				def_name := v.pref.out_name[0..v.pref.out_name.len - 4]
				v.pref.cleanup_files << '${def_name}.def'
			}
		}
		//
		os.chdir(vdir) or {}
		tried_compilation_commands << cmd
		v.show_cc(cmd, response_file, response_file_content)
		// Run
		ccompiler_label := 'C ${os.file_name(ccompiler):3}'
		util.timing_start(ccompiler_label)
		res := os.execute(cmd)
		util.timing_measure(ccompiler_label)
		if v.pref.show_c_output {
			v.show_c_compiler_output(ccompiler, res)
		}
		os.chdir(original_pwd) or {}
		vcache.dlog('| Builder.' + @FN, '>       v.pref.use_cache: ${v.pref.use_cache} | v.pref.retry_compilation: ${v.pref.retry_compilation}')
		vcache.dlog('| Builder.' + @FN, '>      cmd res.exit_code: ${res.exit_code} | cmd: ${cmd}')
		vcache.dlog('| Builder.' + @FN, '>  response_file_content:\n${response_file_content}')
		if res.exit_code != 0 {
			if ccompiler.contains('tcc.exe') {
				// a TCC problem? Retry with the system cc:
				if tried_compilation_commands.len > 1 {
					eprintln('Recompilation loop detected (ccompiler: ${ccompiler}):')
					for recompile_command in tried_compilation_commands {
						eprintln('   ${recompile_command}')
					}
					exit(101)
				}
				if v.pref.retry_compilation {
					tcc_output = res
					v.pref.default_c_compiler()
					if v.pref.is_verbose {
						eprintln('Compilation with tcc failed. Retrying with ${v.pref.ccompiler} ...')
					}
					continue
				}
			}
			if res.exit_code == 127 {
				verror('C compiler error, while attempting to run: \n' +
					'-----------------------------------------------------------\n' + '${cmd}\n' +
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
				v.post_process_c_compiler_output('tcc', tcc_output)
			} else {
				v.post_process_c_compiler_output(ccompiler, res)
			}
		}
		// Print the C command
		if v.pref.is_verbose {
			println('${ccompiler}')
			println('=========\n')
		}
		break
	}
	if v.pref.compress {
		ret := os.system('strip ${os.quoted_path(v.pref.out_name)}')
		if ret != 0 {
			println('strip failed')
			return
		}
		// Note: upx --lzma can sometimes fail with NotCompressibleException
		// See https://github.com/vlang/v/pull/3528
		mut ret2 := os.system('upx --lzma -qqq ${os.quoted_path(v.pref.out_name)}')
		if ret2 != 0 {
			ret2 = os.system('upx -qqq ${os.quoted_path(v.pref.out_name)}')
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
				println('install upx')
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

fn (mut b Builder) ensure_linuxroot_exists(sysroot string) {
	crossrepo_url := 'https://github.com/vlang/linuxroot'
	sysroot_git_config_path := os.join_path(sysroot, '.git', 'config')
	if os.is_dir(sysroot) && !os.exists(sysroot_git_config_path) {
		// remove existing obsolete unarchived .zip file content
		os.rmdir_all(sysroot) or {}
	}
	if !os.is_dir(sysroot) {
		println('Downloading files for Linux cross compilation (~77MB) ...')
		os.system('git clone "${crossrepo_url}" ${os.quoted_path(sysroot)}')
		if !os.exists(sysroot_git_config_path) {
			verror('Failed to clone `${crossrepo_url}` to `${sysroot}`')
		}
		os.chmod(os.join_path(sysroot, 'ld.lld'), 0o755) or { panic(err) }
	}
}

fn (mut b Builder) ensure_freebsdroot_exists(sysroot string) {
	crossrepo_url := 'https://github.com/spytheman/freebsd_base13.2'
	sysroot_git_config_path := os.join_path(sysroot, '.git', 'config')
	if os.is_dir(sysroot) && !os.exists(sysroot_git_config_path) {
		// remove existing obsolete unarchived .zip file content
		os.rmdir_all(sysroot) or {}
	}
	if !os.is_dir(sysroot) {
		println('Downloading files for FreeBSD cross compilation (~458MB) ...')
		os.system('git clone "${crossrepo_url}" ${os.quoted_path(sysroot)}')
		if !os.exists(sysroot_git_config_path) {
			verror('Failed to clone `${crossrepo_url}` to `${sysroot}`')
		}
	}
}

fn (mut b Builder) get_subsystem_flag() string {
	return match b.pref.subsystem {
		.auto { '-municode' }
		.console { '-municode -mconsole' }
		.windows { '-municode -mwindows' }
	}
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
	b.ensure_linuxroot_exists(sysroot)
	obj_file := b.out_name_c + '.o'
	cflags := b.get_os_cflags()
	defines, others, libs := cflags.defines_others_libs()
	mut cc_args := []string{cap: 20}
	cc_args << '-w'
	cc_args << '-fPIC'
	cc_args << '-target x86_64-linux-gnu'
	cc_args << defines
	cc_args << '-I ${os.quoted_path('${sysroot}/include')} '
	cc_args << others
	cc_args << '-o ${os.quoted_path(obj_file)}'
	cc_args << '-c ${os.quoted_path(b.out_name_c)}'
	cc_args << libs
	b.dump_c_options(cc_args)
	mut cc_name := 'cc'
	mut out_name := b.pref.out_name
	$if windows {
		cc_name = 'clang.exe'
		out_name = out_name.trim_string_right('.exe')
	}
	cc_cmd := '${b.quote_compiler_name(cc_name)} ' + cc_args.join(' ')
	if b.pref.show_cc {
		println(cc_cmd)
	}
	cc_res := os.execute(cc_cmd)
	if cc_res.exit_code != 0 {
		println('Cross compilation for Linux failed (first step, cc). Make sure you have clang installed.')
		verror(cc_res.output)
		return
	}
	mut linker_args := [
		'-L',
		os.quoted_path('${sysroot}/usr/lib/x86_64-linux-gnu/'),
		'-L',
		os.quoted_path('${sysroot}/lib/x86_64-linux-gnu'),
		'--sysroot=' + os.quoted_path(sysroot),
		'-v',
		'-o',
		os.quoted_path(out_name),
		'-m elf_x86_64',
		'-dynamic-linker',
		os.quoted_path('/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2'),
		os.quoted_path('${sysroot}/crt1.o'),
		os.quoted_path('${sysroot}/crti.o'),
		os.quoted_path(obj_file),
		'-lc',
		'-lcrypto',
		'-lssl',
		'-lpthread',
		os.quoted_path('${sysroot}/crtn.o'),
		'-lm',
		'-ldl',
	]
	linker_args << cflags.c_options_only_object_files()
	// -ldl
	b.dump_c_options(linker_args)
	mut ldlld := '${sysroot}/ld.lld'
	$if windows {
		ldlld = 'ld.lld.exe'
	}
	linker_cmd := '${b.quote_compiler_name(ldlld)} ' + linker_args.join(' ')
	if b.pref.show_cc {
		println(linker_cmd)
	}
	res := os.execute(linker_cmd)
	if res.exit_code != 0 {
		println('Cross compilation for Linux failed (second step, lld).')
		verror(res.output)
		return
	}
	println(out_name + ' has been successfully cross compiled for linux.')
}

fn (mut b Builder) cc_freebsd_cross() {
	b.setup_ccompiler_options(b.pref.ccompiler)
	b.build_thirdparty_obj_files()
	b.setup_output_name()
	parent_dir := os.vmodules_dir()
	if !os.exists(parent_dir) {
		os.mkdir(parent_dir) or { panic(err) }
	}
	sysroot := os.join_path(os.vmodules_dir(), 'freebsdroot')
	b.ensure_freebsdroot_exists(sysroot)
	obj_file := b.out_name_c + '.o'
	cflags := b.get_os_cflags()
	defines, others, libs := cflags.defines_others_libs()
	mut cc_args := []string{cap: 20}
	cc_args << '-w'
	cc_args << '-fPIC'
	cc_args << '-target x86_64-unknown-freebsd14.0' // TODO custom freebsd versions
	cc_args << defines
	cc_args << '-I'
	cc_args << os.quoted_path('${sysroot}/include')
	cc_args << '-I'
	cc_args << os.quoted_path('${sysroot}/usr/include')
	cc_args << others
	cc_args << '-o'
	cc_args << os.quoted_path(obj_file)
	cc_args << '-c'
	cc_args << os.quoted_path(b.out_name_c)
	cc_args << libs
	b.dump_c_options(cc_args)
	mut cc_name := b.pref.vcross_compiler_name()
	mut out_name := b.pref.out_name
	$if windows {
		cc_name = 'clang.exe'
		out_name = out_name.trim_string_right('.exe')
	}
	cc_cmd := '${b.quote_compiler_name(cc_name)} ' + cc_args.join(' ')
	if b.pref.show_cc {
		println(cc_cmd)
	}
	cc_res := os.execute(cc_cmd)
	if cc_res.exit_code != 0 {
		println('Cross compilation for FreeBSD failed (first step, cc). Make sure you have clang installed.')
		verror(cc_res.output)
		return
	}
	mut linker_args := [
		'-L',
		os.quoted_path('${sysroot}/lib/'),
		'-L',
		os.quoted_path('${sysroot}/usr/lib/'),
		'--sysroot=' + os.quoted_path(sysroot),
		'-v',
		'-o',
		os.quoted_path(out_name),
		'-m elf_x86_64',
		'-dynamic-linker /libexec/ld-elf.so.1',
		os.quoted_path('${sysroot}/usr/lib/crt1.o'),
		os.quoted_path('${sysroot}/usr/lib/crti.o'),
		os.quoted_path(obj_file),
		os.quoted_path('${sysroot}/usr/lib/crtn.o'),
	]
	linker_args << '-lc' // needed for fwrite, strlen etc
	linker_args << '-lexecinfo' // needed for backtrace
	linker_args << cflags.c_options_only_object_files() // support custom module defined linker flags
	linker_args << libs
	// -ldl
	b.dump_c_options(linker_args)
	// mut ldlld := '${sysroot}/ld.lld'
	mut ldlld := b.pref.vcross_linker_name()
	linker_cmd := '${b.quote_compiler_name(ldlld)} ' + linker_args.join(' ')
	if b.pref.show_cc {
		println(linker_cmd)
	}
	res := os.execute(linker_cmd)
	if res.exit_code != 0 {
		println('Cross compilation for FreeBSD failed (second step, lld).')
		verror(res.output)
		return
	}
	println(out_name + ' has been successfully cross compiled for FreeBSD.')
}

fn (mut c Builder) cc_windows_cross() {
	println('Cross compiling for Windows...')
	cross_compiler_name := c.pref.vcross_compiler_name()
	cross_compiler_name_path := os.find_abs_path_of_executable(cross_compiler_name) or {
		eprintln('Could not find `${cross_compiler_name}` in your PATH.')
		eprintln('See https://github.com/vlang/v/blob/master/doc/docs.md#cross-compilation for instructions on how to fix that.')
		exit(1)
	}

	c.setup_ccompiler_options(c.pref.ccompiler)
	c.build_thirdparty_obj_files()
	c.setup_output_name()
	mut args := []string{}
	args << '${c.pref.cflags}'
	args << '-o ${os.quoted_path(c.pref.out_name)}'
	args << '-w -L.'

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
			optimization_options = ['-O3']
			mut have_flto := true
			if c.pref.parallel_cc {
				have_flto = false
			}
			if have_flto {
				optimization_options << '-flto'
			}
		}
	}
	if c.pref.is_debug {
		if c.pref.ccompiler != 'msvc' {
			debug_options = ['-O0', '-g', '-gdwarf-2']
		}
	}
	mut libs := []string{}
	if false && c.pref.build_mode == .default_mode {
		builtin_o := '${pref.default_module_path}/vlib/builtin.o'
		libs << os.quoted_path(builtin_o)
		if !os.exists(builtin_o) {
			verror('${builtin_o} not found')
		}
		for imp in c.table.imports {
			libs << os.quoted_path('${pref.default_module_path}/vlib/${imp}.o')
		}
	}
	// add the thirdparty .o files, produced by all the #flag directives:
	args << cflags.c_options_only_object_files()
	args << os.quoted_path(c.out_name_c)

	mut c_options_after_target := []string{}
	if c.pref.ccompiler == 'msvc' {
		c_options_after_target << cflags.c_options_after_target_msvc()
	} else {
		c_options_after_target << cflags.c_options_after_target()
	}
	for lf in c.ccoptions.linker_flags {
		if lf in c_options_after_target {
			continue
		}
		c_options_after_target << lf
	}
	args << c_options_after_target

	if current_os !in ['macos', 'linux', 'termux'] {
		println(current_os)
		panic('your platform is not supported yet')
	}

	mut all_args := []string{}
	all_args << '-std=gnu11'
	if !c.pref.no_prod_options {
		all_args << optimization_options
	}
	all_args << debug_options

	all_args << args
	all_args << c.get_subsystem_flag()
	all_args << c.pref.ldflags
	c.dump_c_options(all_args)
	mut cmd := cross_compiler_name_path + ' ' + all_args.join(' ')
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
	println(c.pref.out_name + ' has been successfully cross compiled for windows.')
}

fn (mut b Builder) build_thirdparty_obj_files() {
	b.log('build_thirdparty_obj_files: v.ast.cflags: ${b.table.cflags}')
	for flag in b.get_os_cflags() {
		if flag.value.ends_with('.o') {
			rest_of_module_flags := b.get_rest_of_module_cflags(flag)
			$if windows {
				if b.pref.ccompiler == 'msvc' {
					b.build_thirdparty_obj_file_with_msvc(flag.mod, flag.value, rest_of_module_flags)
					continue
				}
			}
			b.build_thirdparty_obj_file(flag.mod, flag.value, rest_of_module_flags)
		}
	}
}

fn (mut v Builder) build_thirdparty_obj_file(mod string, path string, moduleflags []cflag.CFlag) {
	obj_path := os.real_path(path)
	mut cfile := '${obj_path[..obj_path.len - 2]}.c'
	mut cpp_file := false
	if !os.exists(cfile) {
		// Guessed C file does not exist, so it may be a CPP file
		cfile += 'pp'
		cpp_file = true
	}
	opath := v.pref.cache_manager.mod_postfix_with_key2cpath(mod, '.o', obj_path)
	mut rebuild_reason_message := '${os.quoted_path(obj_path)} not found, building it in ${os.quoted_path(opath)} ...'
	if os.exists(opath) {
		if os.exists(cfile) && os.file_last_mod_unix(opath) < os.file_last_mod_unix(cfile) {
			rebuild_reason_message = '${os.quoted_path(opath)} is older than ${os.quoted_path(cfile)}, rebuilding ...'
		} else {
			return
		}
	}
	if os.exists(obj_path) {
		// Some .o files are distributed with no source
		// for example thirdparty\tcc\lib\openlibm.o
		// the best we can do for them is just copy them,
		// and hope that they work with any compiler...
		os.cp(obj_path, opath) or { panic(err) }
		return
	}
	if v.pref.is_verbose {
		println(rebuild_reason_message)
	}
	// prepare for tcc, it needs relative paths to thirdparty/tcc to work:
	current_folder := os.getwd()
	os.chdir(v.pref.vroot) or {}

	mut all_options := []string{}
	all_options << v.pref.third_party_option
	all_options << moduleflags.c_options_before_target()
	all_options << '-o ${v.tcc_quoted_path(opath)}'
	all_options << '-c ${v.tcc_quoted_path(cfile)}'
	cc_options := v.thirdparty_object_args(v.ccoptions, all_options, cpp_file).join(' ')

	// If the third party object file requires a CPP file compilation, switch to a CPP compiler
	mut ccompiler := v.pref.ccompiler
	if cpp_file {
		$if trace_thirdparty_obj_files ? {
			println('>>> build_thirdparty_obj_files switched from compiler "${ccompiler}" to "${v.pref.cppcompiler}"')
		}
		ccompiler = v.pref.cppcompiler
	}
	cmd := '${v.quote_compiler_name(ccompiler)} ${cc_options}'
	$if trace_thirdparty_obj_files ? {
		println('>>> build_thirdparty_obj_files cmd: ${cmd}')
	}
	res := os.execute(cmd)
	os.chdir(current_folder) or {}
	if res.exit_code != 0 {
		eprintln('failed thirdparty object build cmd:\n${cmd}')
		verror(res.output)
		return
	}
	v.pref.cache_manager.mod_save(mod, '.description.txt', obj_path, '${obj_path:-30} @ ${cmd}\n') or {
		panic(err)
	}
	if v.pref.show_cc {
		println('>> OBJECT FILE compilation cmd: ${cmd}')
	}
	$if trace_thirdparty_obj_files ? {
		if res.output != '' {
			println(res.output)
		}
		println('>>> build_thirdparty_obj_files done')
	}
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
	return 'Install a C compiler, like gcc or clang'
}

fn highlight_word(keyword string) string {
	return if term.can_show_color_on_stdout() { term.red(keyword) } else { keyword }
}

fn error_context_lines(text string, keyword string, before int, after int) []string {
	khighlight := highlight_word(keyword)
	mut eline_idx := -1
	mut lines := text.split_into_lines()
	for idx, eline in lines {
		if eline.contains(keyword) {
			lines[idx] = lines[idx].replace(keyword, khighlight)
			if eline_idx == -1 {
				eline_idx = idx
			}
		}
	}
	idx_s := if eline_idx - before >= 0 { eline_idx - before } else { 0 }
	idx_e := if idx_s + after < lines.len { idx_s + after } else { lines.len }
	return lines[idx_s..idx_e]
}

pub fn (mut v Builder) quote_compiler_name(name string) string {
	$if windows {
		// some compiler frontends on windows, like emcc, are a .bat file on windows.
		// Quoting the .bat file name here leads to problems with them, when they internally call python scripts for some reason.
		// Just emcc without quotes here does work, but:
		// |"emcc" -v| produces: python.exe: can't open file 'D:\programs\v\emcc.py': [Errno 2] No such file or directory
		if name.contains('/') || name.contains('\\') {
			return os.quoted_path(name)
		}
		return name
	}
	return os.quoted_path(name)
}

fn write_response_file(response_file string, response_file_content string) {
	$if windows {
		os.write_file_array(response_file, string_to_ansi_not_null_terminated(response_file_content)) or {
			write_response_file_error(response_file_content, err)
		}
	} $else {
		os.write_file(response_file, response_file_content) or {
			write_response_file_error(response_file_content, err)
		}
	}
}

fn write_response_file_error(response_file string, err IError) {
	verror('Unable to write to C response file "${response_file}", error: ${err}')
}
