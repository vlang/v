// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import hash.fnv1a
import os
import v.ast
import v.cflag
import v.pref
import v.util
import v.vcache
import term

const c_std = 'c99'
const cpp_std = 'c++17'

const c_verror_message_marker = 'VERROR_MESSAGE '

const current_os = os.user_os()

const c_compilation_error_title = 'C compilation error'
const missing_libatomic_markers = [
	"library 'atomic' not found",
	'cannot find -latomic',
	'unable to find library -latomic',
	'library not found for -latomic',
	'cannot find libatomic',
]!

fn live_windows_import_lib_path(source_path string) string {
	cache_dir := os.join_path(os.cache_dir(), 'v', 'live')
	os.mkdir_all(cache_dir) or {}
	key := fnv1a.sum64_string(os.real_path(source_path)).str()
	return os.join_path(cache_dir, 'host_symbols_${key}.a')
}

fn extract_c_struct_name(line string) string {
	start := line.index('struct ') or { return '' } + 'struct '.len
	mut end := start
	for end < line.len {
		ch := line[end]
		if !ch.is_letter() && !ch.is_digit() && ch != `_` {
			break
		}
		end++
	}
	return if end > start { line[start..end] } else { '' }
}

fn extract_quoted_identifier(line string) string {
	for quote in [u8(`"`), u8(`'`), u8(96)] {
		start := line.index_u8(quote)
		if start == -1 {
			continue
		}
		end := line[start + 1..].index_u8(quote)
		if end == -1 {
			continue
		}
		return line[start + 1..start + 1 + end]
	}
	return ''
}

fn c_output_suggests_missing_header_for_typedef_c_struct(c_output string, known_typedef_c_structs map[string]bool, known_typedef_c_struct_aliases map[string]string) string {
	if known_typedef_c_structs.len == 0 && known_typedef_c_struct_aliases.len == 0 {
		return ''
	}
	for line in c_output.split_into_lines() {
		lower_line := line.to_lower()
		name := extract_quoted_identifier(line)
		if name != '' {
			if lower_line.contains('unknown type name') && name in known_typedef_c_structs {
				return name
			}
			if name in known_typedef_c_struct_aliases
				&& (lower_line.contains('expected (got') || lower_line.contains('unknown type name')
				|| lower_line.contains('undeclared identifier')
				|| lower_line.contains('does not name a type')) {
				return known_typedef_c_struct_aliases[name]
			}
		}
	}
	return ''
}

fn c_output_suggests_missing_typedef_for_c_struct(c_output string, known_non_typedef_c_structs map[string]bool) string {
	if known_non_typedef_c_structs.len == 0 {
		return ''
	}
	mut forward_declared := map[string]bool{}
	mut incomplete := map[string]bool{}
	for line in c_output.split_into_lines() {
		name := extract_c_struct_name(line)
		if name == '' || name !in known_non_typedef_c_structs {
			continue
		}
		lower_line := line.to_lower()
		if lower_line.contains('forward declaration of') {
			if name in incomplete {
				return name
			}
			forward_declared[name] = true
			continue
		}
		if lower_line.contains('incomplete result type')
			|| lower_line.contains('has incomplete type') || lower_line.contains('incomplete type')
			|| lower_line.contains('return type is an incomplete type') {
			if name in forward_declared {
				return name
			}
			incomplete[name] = true
		}
	}
	return ''
}

fn c_output_suggests_missing_sokol_shader_symbol(c_output string) string {
	for line in c_output.split_into_lines() {
		lower_line := line.to_lower()
		if !lower_line.contains('undeclared identifier')
			&& !lower_line.contains('undeclared (first use in this function)') {
			continue
		}
		name := extract_quoted_identifier(line)
		if name.starts_with('ATTR_') || name.starts_with('SLOT_') {
			return name
		}
	}
	return ''
}

fn (v &Builder) known_non_typedef_c_structs() map[string]bool {
	mut names := map[string]bool{}
	for sym in v.table.type_symbols {
		if sym.language != .c || sym.kind != .struct || !sym.cname.starts_with('C__') {
			continue
		}
		info := sym.info as ast.Struct
		if info.is_typedef {
			continue
		}
		names[sym.cname[3..]] = true
	}
	return names
}

fn (v &Builder) known_typedef_c_structs() map[string]bool {
	mut names := map[string]bool{}
	for sym in v.table.type_symbols {
		if sym.language != .c || sym.kind != .struct || !sym.cname.starts_with('C__') {
			continue
		}
		info := sym.info as ast.Struct
		if !info.is_typedef {
			continue
		}
		names[sym.cname[3..]] = true
	}
	return names
}

fn (v &Builder) known_typedef_c_struct_aliases() map[string]string {
	mut aliases := map[string]string{}
	for sym in v.table.type_symbols {
		if sym.kind != .alias {
			continue
		}
		alias_info := sym.info as ast.Alias
		parent_sym := v.table.final_sym(alias_info.parent_type)
		if parent_sym.language != .c || parent_sym.kind != .struct
			|| !parent_sym.cname.starts_with('C__') {
			continue
		}
		parent_info := parent_sym.info as ast.Struct
		if !parent_info.is_typedef {
			continue
		}
		aliases[sym.cname] = parent_sym.cname[3..]
	}
	return aliases
}

fn c_error_looks_like_cpp_header(c_output string) bool {
	lower_output := c_output.to_lower()
	for marker in [
		"unknown type name 'namespace'",
		"unknown type name 'class'",
		"unknown type name 'template'",
		'unknown type name `namespace`',
		'unknown type name `class`',
		'unknown type name `template`',
		'error: namespace',
		'namespace does not name a type',
		"'operator' declared as",
		'`operator` declared as',
		"before 'operator'",
		'before `operator`',
		'before "operator"',
	] {
		if lower_output.contains(marker) {
			return true
		}
	}
	for line in lower_output.split_into_lines() {
		trimmed_line := line.trim_space()
		if trimmed_line.starts_with('namespace ') || trimmed_line.contains('| namespace ')
			|| trimmed_line.starts_with('class ') || trimmed_line.contains('| class ')
			|| trimmed_line.starts_with('public:') || trimmed_line.contains('| public:')
			|| trimmed_line.starts_with('private:') || trimmed_line.contains('| private:')
			|| trimmed_line.starts_with('protected:') || trimmed_line.contains('| protected:')
			|| trimmed_line.contains('template<') || trimmed_line.contains('template <')
			|| trimmed_line.contains('operator[]') || trimmed_line.contains('operator []')
			|| trimmed_line.contains('::') {
			return true
		}
	}
	return false
}

fn (v &Builder) ensure_imported_coroutines_runtime() ! {
	if 'coroutines' !in v.table.imports {
		return
	}
	pref.ensure_coroutines_runtime()!
}

fn c_error_missing_libatomic_marker(c_output string) string {
	for line in c_output.split_into_lines() {
		lower_line := line.to_lower()
		for marker in missing_libatomic_markers {
			if start := lower_line.index(marker) {
				return line[start..start + marker.len]
			}
		}
	}
	return ''
}

fn c_error_looks_like_missing_libatomic(c_output string) bool {
	return c_error_missing_libatomic_marker(c_output) != ''
}

fn c_error_missing_library_name(c_output string) string {
	for line in c_output.split_into_lines() {
		if line.contains("library '") && line.contains("' not found") {
			return line.all_after("library '").all_before("' not found")
		}
		for marker in [
			'cannot find -l',
			'unable to find library -l',
			'library not found for -l',
		] {
			if line.contains(marker) {
				lib_name := line.all_after(marker).trim_space()
				return lib_name.all_before('`').all_before("'").all_before('"').all_before(' ')
			}
		}
	}
	return ''
}

fn (mut v Builder) show_c_compiler_output(ccompiler string, res os.Result) {
	header := '======== Output of the C Compiler (${ccompiler}) ========'
	println(header)
	if res.output.len > 0 {
		println(res.output.trim_space())
	}
	println('='.repeat(header.len))
}

fn (mut v Builder) post_process_c_compiler_output(ccompiler string, res os.Result) {
	if res.exit_code == 0 {
		if v.pref.reuse_tmpc {
			return
		}
		if os.getenv('V_NO_RM_CLEANUP_FILES') != '' {
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
	libatomic_marker := c_error_missing_libatomic_marker(res.output)
	missing_library_name := if libatomic_marker == '' {
		c_error_missing_library_name(res.output)
	} else {
		''
	}
	for emsg_marker in [c_verror_message_marker, 'error: include file '] {
		if res.output.contains(emsg_marker) {
			emessage :=
				res.output.all_after(emsg_marker).all_before('\n').all_before('\r').trim_right('\r\n')
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
			mut error_keyword := 'error:'
			mut error_context_before := 1
			if libatomic_marker != '' && trimmed_output.contains(libatomic_marker) {
				error_keyword = libatomic_marker
				error_context_before = 0
			}
			elines := error_context_lines(trimmed_output, error_keyword, error_context_before,
				cut_off_limit)
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
			// Check for TCC cross-compilation errors
			if ccompiler == 'tcc' && res.output.starts_with('tcc: error: could not run') {
				println('${highlight_word('Suggestion')}: try using a different C compiler with `-cc gcc` or `-cc clang`.')
				println('${highlight_word('Suggestion')}: or build TCC for the target architecture yourself.')
				println('${highlight_word('Note')}: you should build an 32bit version of `${@VEXEROOT}/thirdparty/tcc/lib/libgc.a` first or use `-gc none`.')
				exit(1)
			} else {
				println('Try passing `-g` when compiling, to see a .v file:line information, that correlates more with the C error.')
				println('(Alternatively, pass `-show-c-output`, to print the full C error message).')
			}
		}
	}
	if v.pref.is_quiet {
		exit(1)
	}
	mut more_suggestions := ''
	if res.output.contains('o: unrecognized file type')
		|| res.output.contains('.o: file not recognized') {
		more_suggestions += '\n${highlight_word('Suggestion')}: try `v wipe-cache`, then repeat your compilation.'
	}
	missing_typedef_header_name := c_output_suggests_missing_header_for_typedef_c_struct(res.output,
		v.known_typedef_c_structs(), v.known_typedef_c_struct_aliases())
	if missing_typedef_header_name != '' {
		more_suggestions += '\n${highlight_word('Suggestion')}: the C typedef `${missing_typedef_header_name}` backing `@[typedef] struct C.${missing_typedef_header_name} {}` was not found by the C compiler. Make sure the header that defines it is included on this platform and that its `#flag -I` path is correct. If the C API actually declares `struct ${missing_typedef_header_name}` without a typedef, remove `@[typedef]` from the V redeclaration.'
	}
	missing_typedef_name := c_output_suggests_missing_typedef_for_c_struct(res.output,
		v.known_non_typedef_c_structs())
	if missing_typedef_name != '' {
		more_suggestions += '\n${highlight_word('Suggestion')}: if `${missing_typedef_name}` is declared in the C header with `typedef struct ... ${missing_typedef_name};`, add `@[typedef]` to the V redeclaration: `@[typedef] struct C.${missing_typedef_name} { ... }`.'
	}
	missing_shader_symbol := c_output_suggests_missing_sokol_shader_symbol(res.output)
	if missing_shader_symbol != '' {
		more_suggestions += '\n${highlight_word('Suggestion')}: `${missing_shader_symbol}` looks like a sokol shader symbol generated by `v shader`/`sokol-shdc`. If you renamed `C.${missing_shader_symbol}` in V, make the same change in the matching `.glsl` file and regenerate the header with `v shader .`.'
	}
	if c_error_looks_like_cpp_header(res.output) {
		verror('
==================
C error found while compiling generated C code.
It looks like a C++ header was included with `#include` (for example one that contains `namespace`).
Use a C-compatible header (for HDF5 use `hdf5.h` instead of `H5File.h`), or compile/link the C++ code separately.${more_suggestions}')
	}
	if libatomic_marker != '' {
		verror('
==================
C error found while compiling generated C code.
The C toolchain could not find `libatomic`, which V needs for `sync.stdatomic` with this compiler on this platform.
Install the system package that provides `libatomic` and retry.
On CentOS/RHEL, that is usually `libatomic` or `libatomic-devel`.${more_suggestions}')
	}
	if missing_library_name != '' {
		verror('
==================
C library `${missing_library_name}` was not found while linking the generated program.
Please install the corresponding development package/libraries and make sure the linker can find it.${more_suggestions}')
	}
	verror('
==================
C error found while compiling generated C code.
This can be caused by invalid C interop code, C compiler flags, or a V compiler bug.
If your code is pure V and this still happens, please report it using `v bug file.v`,
or goto https://github.com/vlang/v/issues/new/choose .
You can also use #help on Discord: https://discord.gg/vlang .${more_suggestions}')
}

fn (mut v Builder) show_cc(cmd string, response_file string, response_file_content string) {
	if v.pref.is_verbose || v.pref.show_cc {
		println('> C compiler cmd: ${cmd}')
		if v.pref.show_cc && !v.pref.no_rsp && response_file != '' {
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

type WindowsPathResolver = fn (string) string

fn ccompiler_type_from_name_with_ok(ccompiler string) (pref.CompilerType, bool) {
	cc_file_name := os.file_name(ccompiler).to_lower_ascii()
	if is_tinyc_compiler_label(cc_file_name) {
		return pref.CompilerType.tinyc, true
	}
	if cc_file_name.contains('gcc') {
		return pref.CompilerType.gcc, true
	}
	if cc_file_name.contains('clang') {
		return pref.CompilerType.clang, true
	}
	if cc_file_name.contains('emcc') {
		return pref.CompilerType.emcc, true
	}
	if cc_file_name == 'cl' || cc_file_name == 'cl.exe' || cc_file_name.contains('msvc') {
		return pref.CompilerType.msvc, true
	}
	if cc_file_name.contains('mingw') {
		return pref.CompilerType.mingw, true
	}
	if cc_file_name.contains('++') {
		return pref.CompilerType.cplusplus, true
	}
	return pref.CompilerType.tinyc, false
}

fn ccompiler_type_from_name(ccompiler string) ?pref.CompilerType {
	resolved, ok := ccompiler_type_from_name_with_ok(ccompiler)
	return if ok { resolved } else { none }
}

fn ccompiler_type_from_version_output_with_ok(output string) (pref.CompilerType, bool) {
	if output == '' {
		return pref.CompilerType.tinyc, false
	}
	lower_output := output.to_lower_ascii()
	if is_tinyc_version_output(lower_output) {
		return pref.CompilerType.tinyc, true
	}
	if lower_output.contains('clang') {
		return pref.CompilerType.clang, true
	}
	if lower_output.contains('gcc version') || lower_output.contains('(gcc)')
		|| lower_output.contains('free software foundation') || lower_output.contains('gcc ') {
		return pref.CompilerType.gcc, true
	}
	if lower_output.contains('emscripten') || lower_output.contains('emcc') {
		return pref.CompilerType.emcc, true
	}
	if (lower_output.contains('microsoft') && lower_output.contains('c/c++'))
		|| lower_output.contains('msvc') {
		return pref.CompilerType.msvc, true
	}
	return pref.CompilerType.tinyc, false
}

fn ccompiler_type_from_version_output(output string) ?pref.CompilerType {
	resolved, ok := ccompiler_type_from_version_output_with_ok(output)
	return if ok { resolved } else { none }
}

fn resolve_ccompiler_type(ccompiler string, fallback pref.CompilerType) pref.CompilerType {
	resolved_by_name, name_ok := ccompiler_type_from_name_with_ok(ccompiler)
	if name_ok {
		return resolved_by_name
	}
	quoted_ccompiler := os.quoted_path(ccompiler)
	for version_flag in ['--version', '-v'] {
		res := os.execute('${quoted_ccompiler} ${version_flag} 2>&1')
		resolved_by_version, version_ok := ccompiler_type_from_version_output_with_ok(res.output)
		if version_ok {
			return resolved_by_version
		}
	}
	return fallback
}

fn cc_from_pref_ccompiler_type(cc_type pref.CompilerType) CC {
	return match cc_type {
		.tinyc { .tcc }
		.gcc, .mingw { .gcc }
		.clang { .clang }
		.emcc { .emcc }
		.msvc { .msvc }
		.cplusplus { .unknown }
	}
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
		'-Wdate-time',
		'-Winit-self',
		'-Winvalid-pch',
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
	v.pref.ccompiler_type = resolve_ccompiler_type(ccompiler, v.pref.ccompiler_type)
	cc_file_name := os.file_name(ccompiler).to_lower_ascii()
	ccoptions.cc = if cc_file_name.contains('icc') || ccoptions.guessed_compiler == 'icc' {
		.icc
	} else {
		cc_from_pref_ccompiler_type(v.pref.ccompiler_type)
	}
	if ccoptions.cc == .unknown {
		eprintln('Compilation with unknown C compiler `${cc_file_name}`')
	}

	// Add -fwrapv to handle UB overflows
	if ccoptions.cc in [.gcc, .clang, .tcc]
		&& v.pref.os in [.macos, .linux, .openbsd, .freebsd, .windows] {
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
		if v.pref.is_shared || v.disable_flto {
			// Keep shared libraries away from LTO to avoid runtime loader regressions.
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
			'-Wno-excess-initializers', // vlib/v/tests/struct_init_with_complex_fields_test.v fails without that on macos clang 13
			'-Wno-unknown-warning', // if a C compiler does not understand a certain flag, it should just ignore it
			'-Wno-unknown-warning-option', // clang equivalent of the above
		]
		// Apple clang >= 17 treats -Wincompatible-function-pointer-types as an error by default.
		// V generates code with enum types (e.g. os.Signal) in callbacks where C expects int,
		// and specific struct* returns where C expects void* (e.g. sync.pool.ThreadCB).
		ccoptions.args << '-Wno-incompatible-function-pointer-types'
		ccoptions.args << '-Wno-typedef-redefinition' // V re-typedefs bool after includes to undo stdbool.h
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
		if v.pref.is_shared || v.disable_flto {
			// Keep shared libraries away from LTO to avoid runtime loader regressions.
			have_flto = false
		}
		if have_flto {
			optimization_options << '-flto'
		}
		// gcc versions newer than 10.2, produce buggy programs, usually triggered by optimising inlined small functions, when both -flto and -O3 are used.
		// Using -fno-strict-aliasing prevents that. See https://github.com/vlang/v/issues/26512 .
		optimization_options << '-fno-strict-aliasing'
		ccoptions.wargs << [
			'-Wduplicated-branches',
			'-Wduplicated-cond',
			'-Wjump-misses-init',
			'-Wlogical-op',
			'-Wno-incompatible-pointer-types', // V uses enum types (e.g. os.Signal) in callbacks where C expects int
			'-Wno-missing-field-initializers', // @[typedef] C structs may have fields not present in V binding
		]
		// On macOS, `gcc` is actually Apple clang, which splits -Wincompatible-pointer-types
		// and -Wincompatible-function-pointer-types into separate warnings.
		ccoptions.args << '-Wno-incompatible-function-pointer-types'
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
			if !v.pref.is_quiet {
				eprintln('Note: tcc is not recommended for -prod builds')
			}
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
		ccoptions.args << '-DNO_DEBUGGING' // for BDWGC
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
		if v.pref.os == .linux && 'gcboehm' in v.pref.compile_defines_all {
			// Keep shared-library GC symbols bound to the shared object itself.
			// This avoids cross-DSO symbol interposition between multiple V binaries
			// in one process (for example, host executable + loaded V plugin).
			ccoptions.linker_flags << '-Wl,-Bsymbolic'
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
		if v.pref.os in [.linux, .android, .termux] && v.pref.build_mode != .build_module {
			// The live reload shared library resolves symbols from the host executable.
			// Termux/Android need the same export behavior as Linux, otherwise plain
			// `-live` can crash while `-cg -live` happens to work via debug linker flags.
			ccoptions.linker_flags << '-rdynamic'
		}
		if v.pref.os == .macos {
			ccoptions.args << '-flat_namespace'
			if v.pref.is_liveshared {
				// Resolve sapp_* and similar host symbols when the live-reload dylib is loaded.
				ccoptions.args << '-undefined'
				ccoptions.args << 'dynamic_lookup'
			}
		}
		if v.pref.os == .windows && ccoptions.cc != .msvc {
			host_import_lib := v.tcc_quoted_path(live_windows_import_lib_path(v.pref.path))
			if v.pref.is_livemain {
				// Re-export host graphics/backend symbols so the live-reload DLL can reuse them.
				ccoptions.linker_flags << '-Wl,--export-all-symbols'
				ccoptions.linker_flags << '-Wl,--out-implib,${host_import_lib}'
			}
			if v.pref.is_liveshared {
				// Link the live-reload DLL against the host executable's import library.
				ccoptions.linker_flags << host_import_lib
			}
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
		subsystem_flag := v.get_subsystem_flag()
		if subsystem_flag != '' {
			ccoptions.post_args << subsystem_flag
		}
	}
	ccoptions.env_cflags = os.getenv('CFLAGS').replace('\n', ' ')
	ccoptions.env_ldflags = os.getenv('LDFLAGS').replace('\n', ' ')
	// Set the cache salt before resolving cached thirdparty object paths,
	// so object building and final compilation agree on the same cache entry.
	v.pref.cache_manager.set_temporary_options(v.thirdparty_object_args(ccoptions, [
		ccoptions.guessed_compiler,
	], false))
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
		ccoptions.source_args << ['-std=${c_std}', '-D_DEFAULT_SOURCE']
	}
	$if trace_ccoptions ? {
		println('>>> setup_ccompiler_options ccompiler: ${ccompiler}')
		println('>>> setup_ccompiler_options ccoptions: ${ccoptions}')
	}
	v.ccoptions = ccoptions
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
		// -Wl,-stack=33554432 == /F33554432
		// -Werror=implicit-function-declaration == /we4013
		// /volatile:ms - there seems to be no equivalent,
		// normally msvc should use /volatile:iso
		// but it could have an impact on vinix if it is created with msvc.
		if ccoptions.cc != .msvc {
			if v.pref.os != .wasm32_emscripten {
				all << '-Wl,-stack=33554432'
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
	// in `build-mode` or when producing a .o file, we do not need -lxyz flags,
	// since we are building an (.o) object file, that will be linked later.
	if v.pref.build_mode != .build_module && !v.pref.is_o {
		all << ccoptions.linker_flags
		all << ccoptions.env_ldflags
		all << ccoptions.ldflags
	}
	return all
}

struct ThirdpartyCrossCompileConfig {
	target_args           []string
	trailing_include_args []string
	sysroot               string
}

fn (v &Builder) thirdparty_cross_compile_config() ThirdpartyCrossCompileConfig {
	if v.pref.os == .linux && current_os != 'linux' {
		sysroot := os.join_path(os.vmodules_dir(), 'linuxroot')
		return ThirdpartyCrossCompileConfig{
			target_args:           ['-target x86_64-linux-gnu']
			trailing_include_args: [
				'-I',
				os.quoted_path('${sysroot}/include'),
			]
			sysroot:               sysroot
		}
	}
	if v.pref.os == .freebsd && current_os != 'freebsd' {
		sysroot := os.join_path(os.vmodules_dir(), 'freebsdroot')
		return ThirdpartyCrossCompileConfig{
			target_args:           ['-target x86_64-unknown-freebsd14.0']
			trailing_include_args: [
				'-I',
				os.quoted_path('${sysroot}/include'),
				'-I',
				os.quoted_path('${sysroot}/usr/include'),
			]
			sysroot:               sysroot
		}
	}
	return ThirdpartyCrossCompileConfig{}
}

fn (mut v Builder) ensure_thirdparty_cross_compile_sysroot(cfg ThirdpartyCrossCompileConfig) {
	if cfg.sysroot == '' {
		return
	}
	if v.pref.os == .linux {
		v.ensure_linuxroot_exists(cfg.sysroot)
		return
	}
	if v.pref.os == .freebsd {
		v.ensure_freebsdroot_exists(cfg.sysroot)
	}
}

fn (mut v Builder) thirdparty_object_args(ccoptions CcompilerOptions, middle []string, cpp_file bool) []string {
	mut all := []string{}

	if !v.pref.no_std {
		if cpp_file {
			all << '-std=${cpp_std}'
		} else {
			all << '-std=${c_std}'
		}
		all << '-D_DEFAULT_SOURCE'
	}

	cross_cfg := v.thirdparty_cross_compile_config()
	if cross_cfg.sysroot != '' {
		v.ensure_thirdparty_cross_compile_sysroot(cross_cfg)
		all << cross_cfg.target_args
	}

	all << ccoptions.env_cflags
	all << ccoptions.args
	all << middle
	// NOTE do not append linker flags in .o build process,
	// compilers are inconsistent about how they handle:
	// all << ccoptions.env_ldflags
	// all << ccoptions.ldflags
	if cross_cfg.sysroot != '' {
		// add the system include/ folder after everything else,
		// so that local folders like thirdparty/mbedtls have a
		// chance to supply their own headers
		all << cross_cfg.trailing_include_args
	}
	return all
}

fn (mut v Builder) setup_output_name() {
	if !v.pref.is_shared && v.pref.build_mode != .build_module && v.pref.os == .windows
		&& !v.pref.is_o && !v.pref.out_name.ends_with('.exe') {
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
		v.pref.out_name = v.pref.cache_manager.mod_postfix_with_key2cpath(v.pref.path, '.o',
			v.pref.path) // v.out_name
		if v.pref.is_verbose {
			println('Building ${v.pref.path} to ${v.pref.out_name} ...')
		}
		v.pref.cache_manager.mod_save(v.pref.path, '.output.description.txt', v.pref.path,
			get_dsc_content('PREF.PATH: ${v.pref.path}\nVOPTS: ${v.pref.cache_manager.vopts}\n')) or {
			panic(err)
		}
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
	return os.quoted_path(v.tcc_windows_path(p))
}

fn looks_like_windows_path(value string) bool {
	return value.contains('\\') || value.contains('/') || (value.len > 1 && value[1] == `:`)
}

fn rewrite_windows_path_arg(arg string, resolver WindowsPathResolver) string {
	if arg == '' {
		return ''
	}
	if start := arg.index('"') {
		end := arg.last_index('"') or { -1 }
		if end > start {
			path := arg[start + 1..end]
			if looks_like_windows_path(path) {
				return arg[..start] + '"${resolver(path)}"' + arg[end + 1..]
			}
		}
	}
	for prefix in ['-I', '-L', '-B', '-o ', '-c '] {
		if arg.starts_with(prefix) {
			path := arg[prefix.len..].trim_space().trim('"')
			if looks_like_windows_path(path) {
				return prefix + '"${resolver(path)}"'
			}
		}
	}
	trimmed := arg.trim_space().trim('"')
	if !arg.starts_with('-') && looks_like_windows_path(trimmed) {
		return '"${resolver(trimmed)}"'
	}
	return arg
}

fn short_windows_path(path string) string {
	$if windows {
		return os.short_path(path)
	}
	return path
}

fn (v &Builder) tcc_windows_path(p string) string {
	$if windows {
		if v.ccoptions.cc == .tcc {
			return short_windows_path(p)
		}
	}
	return p
}

fn (v &Builder) tcc_windows_path_arg(arg string) string {
	$if windows {
		if v.ccoptions.cc == .tcc {
			return rewrite_windows_path_arg(arg, short_windows_path)
		}
	}
	return arg
}

fn (v &Builder) rsp_safe_arg(arg string) string {
	if arg.starts_with('-B') && arg.len > 2 {
		path := arg[2..]
		if path.contains(' ') && !path.starts_with('"') {
			return '-B"${path}"'
		}
	}
	return arg
}

fn (v &Builder) should_use_rsp(rsp_args []string) bool {
	if v.pref.no_rsp || v.pref.os == .termux {
		return false
	}
	for arg in rsp_args {
		if arg.contains("'\\''") || arg.contains('\n') || arg.contains('\r') {
			return false
		}
	}
	return true
}

fn (v &Builder) msvc_should_use_rsp(args []string) bool {
	if !v.should_use_rsp(args) {
		return false
	}
	// Keep Unicode paths on the direct CreateProcessW command line. MSVC response
	// files still mis-handle non-ASCII file names on some Windows setups.
	for arg in args {
		if !arg.is_ascii() {
			return false
		}
	}
	return true
}

fn (v &Builder) c_project_source_name() string {
	mut output_name := os.file_name(v.pref.out_name)
	if output_name == '' {
		output_name = 'main'
	}
	base_name := output_name.all_before_last('.')
	return if base_name == '' { '${output_name}.c' } else { '${base_name}.c' }
}

fn (mut v Builder) c_project_output_name() string {
	mut output_name := os.file_name(v.pref.out_name)
	if output_name == '' {
		output_name = 'main'
	}
	if output_name.ends_with('.c') {
		output_name = output_name.trim_string_right('.c')
	}
	if output_name == '' {
		output_name = 'main'
	}
	if !v.pref.is_shared && v.pref.build_mode != .build_module && v.pref.os == .windows
		&& !v.pref.is_o && !output_name.ends_with('.exe') {
		output_name += '.exe'
	}
	if v.pref.is_shared && !output_name.ends_with(v.ccoptions.shared_postfix) {
		output_name += v.ccoptions.shared_postfix
	}
	return output_name
}

fn (mut v Builder) c_project_dependency_replacements() map[string]string {
	mut replacements := map[string]string{}
	for flag in v.get_os_cflags() {
		if !flag.value.ends_with('.o') && !flag.value.ends_with('.obj') {
			continue
		}
		cached_value := if flag.cached == '' { os.real_path(flag.value) } else { flag.cached }
		obj_path := os.real_path(flag.value)
		replacement_value := if source_path := c_project_source_from_object_path(obj_path) {
			os.quoted_path(source_path)
		} else if os.exists(obj_path) {
			os.quoted_path(obj_path)
		} else {
			os.quoted_path(cached_value)
		}
		for key in [
			cached_value,
			os.quoted_path(cached_value),
			'"${cached_value}"',
			flag.format() or { '' },
		] {
			if key == '' {
				continue
			}
			replacements[key] = replacement_value
		}
	}
	return replacements
}

fn (mut v Builder) generate_c_project() {
	if v.pref.backend != .c {
		verror('`-generate-c-project` is currently supported only for the C backend.')
	}
	mut project_dir := v.pref.generate_c_project
	if !os.is_abs_path(project_dir) {
		project_dir = os.real_path(project_dir)
	}
	if os.exists(project_dir) && !os.is_dir(project_dir) {
		verror('`-generate-c-project` expects a directory path, got file: ${os.quoted_path(project_dir)}')
	}
	os.mkdir_all(project_dir) or {
		verror('Cannot create `-generate-c-project` directory ${os.quoted_path(project_dir)}: ${err}')
	}
	c_source_path := os.join_path(project_dir, v.c_project_source_name())
	os.mv_by_cp(v.out_name_c, c_source_path) or {
		verror('Cannot write generated C source to ${os.quoted_path(c_source_path)}: ${err}')
	}

	mut ccompiler := v.pref.ccompiler
	if v.pref.os == .wasm32 {
		ccompiler = 'clang'
	}
	v.setup_ccompiler_options(ccompiler)
	if v.pref.build_mode == .build_module {
		v.ccoptions.pre_args << '-c'
	}
	mut project_o_args := v.ccoptions.o_args.filter(!it.starts_with('-o '))
	project_o_args << [
		'-o ${v.tcc_quoted_path(os.join_path(project_dir, v.c_project_output_name()))}',
	]
	v.ccoptions.o_args = project_o_args
	for idx, source_arg in v.ccoptions.source_args {
		if source_arg.contains(v.out_name_c) || source_arg.ends_with('.tmp.c')
			|| source_arg.contains(".tmp.c'") || source_arg.contains('.tmp.c"') {
			v.ccoptions.source_args[idx] = v.tcc_quoted_path(c_source_path)
		}
	}

	mut all_args := v.all_args(v.ccoptions)
	replacements := v.c_project_dependency_replacements()
	for idx, arg in all_args {
		if replacement := replacements[arg] {
			all_args[idx] = replacement
		}
	}
	v.dump_c_options(all_args)
	cc_cmd := '${v.quote_compiler_name(ccompiler)} ${all_args.join(' ')}'
	os.write_file(os.join_path(project_dir, 'build_command.txt'), cc_cmd + '\n') or {
		verror('Cannot write ${os.quoted_path(os.join_path(project_dir, 'build_command.txt'))}: ${err}')
	}
	os.write_file(os.join_path(project_dir, 'Makefile'), 'all:\n\t${cc_cmd}\n') or {
		verror('Cannot write ${os.quoted_path(os.join_path(project_dir, 'Makefile'))}: ${err}')
	}
	os.write_file(os.join_path(project_dir, 'build.sh'), '#!/bin/sh\nset -eu\n${cc_cmd}\n') or {
		verror('Cannot write ${os.quoted_path(os.join_path(project_dir, 'build.sh'))}: ${err}')
	}
	os.write_file(os.join_path(project_dir, 'build.bat'), '@echo off\r\n${cc_cmd}\r\n') or {
		verror('Cannot write ${os.quoted_path(os.join_path(project_dir, 'build.bat'))}: ${err}')
	}
	$if !windows {
		os.chmod(os.join_path(project_dir, 'build.sh'), 0o755) or {}
	}
	println('Generated C project in ${os.quoted_path(project_dir)}')
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
	v.ensure_windows_icon_flag_is_valid()
	if v.pref.should_output_to_stdout() {
		// output to stdout
		content := os.read_file(v.out_name_c) or { panic(err) }
		println(content)
		os.rm(v.out_name_c) or {}
		return
	}
	if v.pref.generate_c_project != '' {
		v.pref.skip_running = true
		v.generate_c_project()
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
	v.ensure_imported_coroutines_runtime() or { verror(err.msg()) }
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
			if v.ccoptions.cc == .msvc || v.pref.ccompiler_type == .msvc {
				v.cc_msvc()
				return
			}
		}
		//
		all_args := v.all_args(v.ccoptions)
		v.dump_c_options(all_args)
		mut rsp_args := all_args.map(v.rsp_safe_arg(it))
		rsp_args = rsp_args.map(v.tcc_windows_path_arg(it))
		shell_args := rsp_args.join(' ')
		mut should_use_rsp := v.should_use_rsp(rsp_args)
		mut str_args := if !should_use_rsp {
			shell_args.replace('\n', ' ')
		} else {
			shell_args
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
		if should_use_rsp {
			response_file = '${v.out_name_c}.rsp'
			response_file_content = str_args.replace('\\', '\\\\')
			write_response_file(response_file, response_file_content)
			rspexpr := '@${v.tcc_windows_path(response_file)}'
			cmd = '${v.quote_compiler_name(ccompiler)} ${os.quoted_path(rspexpr)}'
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
		vcache.dlog('| Builder.' + @FN,
			'>       v.pref.use_cache: ${v.pref.use_cache} | v.pref.retry_compilation: ${v.pref.retry_compilation}')
		vcache.dlog('| Builder.' + @FN, '>      cmd res.exit_code: ${res.exit_code} | cmd: ${cmd}')
		vcache.dlog('| Builder.' + @FN, '>  response_file_content:\n${response_file_content}')
		if res.exit_code != 0 {
			// Some GCC+linker setups fail bootstrapping with `-flto` and then report a missing `main` symbol.
			// Retry once without `-flto`, while still keeping the remaining -prod options.
			if v.pref.building_v && v.pref.is_prod && !v.pref.no_prod_options && !v.disable_flto
				&& v.ccoptions.cc == .gcc && response_file_content.contains('-flto')
				&& (res.output.contains('undefined symbol: main')
				|| res.output.contains('undefined reference to `main')) {
				v.disable_flto = true
				if !v.pref.is_quiet {
					eprintln('Retrying compiler build without `-flto` after a linker failure with missing `main`.')
				}
				continue
			}
			if is_tcc_compilation_failure(ccompiler, v.ccoptions.cc, res.output) {
				// A TCC problem? Retry with a non-tcc system compiler:
				if tried_compilation_commands.len > 1 {
					eprintln('Recompilation loop detected (ccompiler: ${ccompiler}):')
					for recompile_command in tried_compilation_commands {
						eprintln('   ${recompile_command}')
					}
					exit(101)
				}
				if v.pref.retry_compilation {
					tcc_output = res
					old_ccompiler := v.pref.ccompiler
					v.pref.default_c_compiler()
					if v.pref.ccompiler == ccompiler || is_tcc_compiler_name(v.pref.ccompiler)
						|| is_tcc_alias_compiler(v.pref.ccompiler) {
						v.pref.ccompiler = first_available_ccompiler([old_ccompiler, ccompiler,
							v.pref.ccompiler])
					}
					if v.pref.ccompiler != '' && v.pref.ccompiler != ccompiler {
						if v.pref.is_verbose {
							eprintln('Compilation with tcc failed. Retrying with ${v.pref.ccompiler} ...')
						}
						continue
					}
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
	v.apply_windows_icon_to_executable() or { verror(err.msg()) }
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
	// ret := os.system('ldid2 -S ${v.pref.out_name}')
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
	if b.pref.is_shared || b.pref.build_mode == .build_module || b.pref.is_o {
		return ''
	}
	return match b.pref.subsystem {
		.auto { '-municode' }
		.console { '-municode -mconsole' }
		.windows { '-municode -mwindows' }
	}
}

struct LinuxCrossTarget {
	triple           string
	lib_dir          string
	dynamic_linker   string
	linker_emulation string
}

fn linux_cross_target_for_arch(arch pref.Arch) !LinuxCrossTarget {
	if arch != .amd64 {
		return error('Linux cross compilation currently supports only `-arch amd64`; the bundled linuxroot sysroot does not provide `${arch}` runtime files.')
	}
	return LinuxCrossTarget{
		triple:           'x86_64-linux-gnu'
		lib_dir:          'x86_64-linux-gnu'
		dynamic_linker:   '/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2'
		linker_emulation: 'elf_x86_64'
	}
}

fn (mut b Builder) cc_linux_cross() {
	linux_cross_target := linux_cross_target_for_arch(b.pref.arch) or {
		verror(err.msg())
		return
	}
	b.setup_ccompiler_options(b.pref.ccompiler)
	b.build_thirdparty_obj_files()
	b.setup_output_name()
	parent_dir := os.vmodules_dir()
	if !os.exists(parent_dir) {
		os.mkdir(parent_dir) or { panic(err) }
	}
	sysroot := os.join_path(os.vmodules_dir(), 'linuxroot')
	b.ensure_linuxroot_exists(sysroot)
	obj_file := if b.pref.build_mode == .build_module {
		b.pref.out_name
	} else {
		b.out_name_c + '.o'
	}
	cflags := b.get_os_cflags()
	defines, others, libs := cflags.defines_others_libs()
	mut cc_args := []string{cap: 20}
	cc_args << '-w'
	cc_args << '-fPIC'
	cc_args << '-target ${linux_cross_target.triple}'
	cc_args << defines
	cc_args << '-I ${os.quoted_path('${sysroot}/include')} '
	cc_args << others
	cc_args << '-o ${os.quoted_path(obj_file)}'
	cc_args << '-c ${os.quoted_path(b.out_name_c)}'
	cc_args << libs
	b.dump_c_options(cc_args)
	mut cc_name := b.pref.ccompiler
	mut out_name := b.pref.out_name
	$if windows {
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
	if b.pref.build_mode == .build_module {
		return
	}
	// Compile compiler runtime builtins (provides __udivti3 etc. for 128-bit integer
	// operations used by thirdparty code like mbedtls bignum.c, since the linuxroot
	// sysroot doesn't include libgcc or compiler-rt).
	builtins_src := os.join_path(@VEXEROOT, 'thirdparty', 'builtins', 'compiler_builtins.c')
	builtins_obj := os.join_path(os.vtmp_dir(), 'compiler_builtins_${linux_cross_target.lib_dir}.o')
	if os.exists(builtins_src) {
		builtins_cmd := '${b.quote_compiler_name(cc_name)} -w -fPIC -target ${linux_cross_target.triple} -o ${os.quoted_path(builtins_obj)} -c ${os.quoted_path(builtins_src)}'
		builtins_res := os.execute(builtins_cmd)
		if builtins_res.exit_code != 0 {
			println('Warning: failed to compile compiler builtins for cross compilation.')
		}
	}
	mut linker_args := [
		'-L',
		os.quoted_path(os.join_path(sysroot, 'usr', 'lib', linux_cross_target.lib_dir)),
		'-L',
		os.quoted_path(os.join_path(sysroot, 'lib', linux_cross_target.lib_dir)),
		'--sysroot=' + os.quoted_path(sysroot),
		'-v',
		'-o',
		os.quoted_path(out_name),
		'-m ${linux_cross_target.linker_emulation}',
		'-dynamic-linker',
		os.quoted_path(linux_cross_target.dynamic_linker),
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
	if os.exists(builtins_obj) {
		linker_args << os.quoted_path(builtins_obj)
	}
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
	mut cc_name := b.pref.ccompiler
	mut out_name := b.pref.out_name
	$if windows {
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
	cross_compiler_name := c.pref.ccompiler
	cross_compiler_name_path := if cross_compiler_name.contains('/')
		|| cross_compiler_name.contains('\\') {
		cross_compiler_name
	} else {
		os.find_abs_path_of_executable(cross_compiler_name) or {
			eprintln('Could not find `${cross_compiler_name}` in your PATH.')
			eprintln('Set `-cc` or `VCROSS_COMPILER_NAME` to a working cross compiler.')
			eprintln('See https://github.com/vlang/v/blob/master/doc/docs.md#cross-compilation for instructions on how to fix that.')
			exit(1)
		}
	}

	c.setup_ccompiler_options(c.pref.ccompiler)
	c.build_thirdparty_obj_files()
	c.setup_output_name()
	icon_object := c.prepare_cross_windows_icon_resource() or { verror(err.msg()) }
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
			if c.pref.is_shared {
				// Keep shared libraries away from LTO to avoid runtime loader regressions.
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
	if icon_object != '' {
		args << os.quoted_path(icon_object)
	}

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
	subsystem_flag := c.get_subsystem_flag()
	if subsystem_flag != '' {
		all_args << subsystem_flag
	}
	all_args << c.pref.ldflags
	c.dump_c_options(all_args)
	mut cmd := cross_compiler_name_path + ' ' + all_args.join(' ')
	// cmd := 'clang -o ${obj_name} -w ${include} -m32 -c -target x86_64-win32 ${pref.default_module_path}/${c.out_name_c}'
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
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			rest_of_module_flags := b.get_rest_of_module_cflags(flag)
			$if windows {
				if b.pref.ccompiler == 'msvc' {
					b.build_thirdparty_obj_file_with_msvc(flag.mod, flag.value,
						rest_of_module_flags)
					continue
				}
			}
			b.build_thirdparty_obj_file(flag.mod, flag.value, rest_of_module_flags)
		}
	}
}

enum SourceKind {
	c
	cpp
	asm
	unknown
}

fn c_project_source_from_object_path(obj_path string) ?string {
	if !obj_path.ends_with('.o') && !obj_path.ends_with('.obj') {
		return none
	}
	base := obj_path.all_before_last('.')
	for ext in ['.c', '.cpp', '.S'] {
		source_file := base + ext
		if os.exists(source_file) {
			return source_file
		}
	}
	return none
}

fn sqlite_thirdparty_validation_error(mod string, obj_path string, source_file string, source_kind SourceKind) string {
	if mod != 'db.sqlite' || os.file_name(obj_path) != 'sqlite3.o'
		|| os.base(os.dir(obj_path)) != 'sqlite'
		|| os.base(os.dir(os.dir(obj_path))) != 'thirdparty' {
		return ''
	}
	sqlite_dir := os.dir(obj_path)
	if source_kind == .cpp && os.file_name(source_file) == 'sqlite3.cpp' {
		return 'The `db.sqlite` module expects the SQLite amalgamation files `sqlite3.c` and `sqlite3.h` in `${sqlite_dir}`. Do not rename `sqlite3.c` to `sqlite3.cpp`; run `v vlib/db/sqlite/install_thirdparty_sqlite.vsh`, or download the SQLite amalgamation package and place those files there.'
	}
	if source_kind == .unknown {
		return 'The `db.sqlite` module expects the SQLite amalgamation files `sqlite3.c` and `sqlite3.h` in `${sqlite_dir}`. Run `v vlib/db/sqlite/install_thirdparty_sqlite.vsh`, or download the SQLite amalgamation package and place those files there.'
	}
	return ''
}

fn (v &Builder) should_compile_bundled_thirdparty_object_from_source(obj_path string, source_file string, source_kind SourceKind) bool {
	if source_kind == .unknown {
		return false
	}
	if os.exists(obj_path) && os.file_last_mod_unix(obj_path) < os.file_last_mod_unix(source_file) {
		return true
	}
	return v.ccoptions.cc == .tcc && v.pref.os == .macos
}

fn (mut v Builder) build_thirdparty_obj_file(mod string, path string, moduleflags []cflag.CFlag) {
	trace_thirdparty_obj_files := 'trace_thirdparty_obj_files' in v.pref.compile_defines
	obj_path := os.real_path(path)
	opath := v.pref.cache_manager.mod_postfix_with_key2cpath(mod, '.o', obj_path)
	thirdparty_desc_path := v.pref.cache_manager.mod_postfix_with_key2cpath(mod,
		'.thirdparty.description.txt', obj_path)
	mut source_file := c_project_source_from_object_path(obj_path) or { '' }
	source_kind := if source_file.ends_with('.c') {
		SourceKind.c
	} else if source_file.ends_with('.cpp') {
		SourceKind.cpp
	} else if source_file.ends_with('.S') {
		SourceKind.asm
	} else {
		SourceKind.unknown
	}
	sqlite_validation_message := sqlite_thirdparty_validation_error(mod, obj_path, source_file,
		source_kind)
	if sqlite_validation_message != '' {
		verror(sqlite_validation_message)
	}
	compile_bundled_source := v.should_compile_bundled_thirdparty_object_from_source(obj_path,
		source_file, source_kind)
	if os.exists(obj_path) && !compile_bundled_source {
		// Some .o files are distributed with no source
		// for example thirdparty\tcc\lib\openlibm.o
		// the best we can do for them is just copy them,
		// and hope that they work with any compiler...
		os.cp(obj_path, opath) or { panic(err) }
		return
	}
	if source_kind == .unknown {
		base := obj_path.all_before_last('.')
		eprintln('> File not found: ${base}{.c,.cpp,.S}')
		verror('build_thirdparty_obj_file only support .c, .cpp, and .S source file.')
	}
	bundled_object_is_stale := os.exists(obj_path)
		&& os.file_last_mod_unix(obj_path) < os.file_last_mod_unix(source_file)
	cached_object_was_built_from_source := os.exists(thirdparty_desc_path)
	mut rebuild_reason_message := if bundled_object_is_stale {
		'${os.quoted_path(obj_path)} is older than ${os.quoted_path(source_file)}, rebuilding it in ${os.quoted_path(opath)} ...'
	} else if compile_bundled_source {
		'${os.quoted_path(obj_path)} is bundled for a different object format; rebuilding it in ${os.quoted_path(opath)} from ${os.quoted_path(source_file)} ...'
	} else {
		'${os.quoted_path(obj_path)} not found, building it in ${os.quoted_path(opath)} ...'
	}
	if os.exists(opath) {
		if compile_bundled_source && !cached_object_was_built_from_source {
			rebuild_reason_message = '${os.quoted_path(opath)} was copied from a bundled object, rebuilding it from ${os.quoted_path(source_file)} ...'
		} else if os.file_last_mod_unix(opath) < os.file_last_mod_unix(source_file) {
			rebuild_reason_message = '${os.quoted_path(opath)} is older than ${os.quoted_path(source_file)}, rebuilding ...'
		} else {
			return
		}
	}
	if v.pref.is_verbose {
		println(rebuild_reason_message)
	}
	// prepare for tcc, it needs relative paths to thirdparty/tcc to work:
	current_folder := os.getwd()
	os.chdir(v.pref.vroot) or {}

	cc_options := if source_kind == .asm {
		'-o ${v.tcc_quoted_path(opath)} -c ${v.tcc_quoted_path(source_file)}'
	} else {
		mut all_options := []string{cap: 4}
		all_options << v.pref.third_party_option
		all_options << moduleflags.c_options_before_target()
		all_options << '-o ${v.tcc_quoted_path(opath)}'
		all_options << '-c ${v.tcc_quoted_path(source_file)}'
		cpp_file := source_kind == .cpp
		v.thirdparty_object_args(v.ccoptions, all_options, cpp_file).map(v.tcc_windows_path_arg(it)).join(' ')
	}

	// If the third party object file requires a CPP file compilation, switch to a CPP compiler
	mut ccompiler := v.pref.ccompiler
	if source_kind == .cpp {
		if trace_thirdparty_obj_files {
			println('>>> build_thirdparty_obj_files switched from compiler "${ccompiler}" to "${v.pref.cppcompiler}"')
		}
		ccompiler = v.pref.cppcompiler
	}
	cmd := '${v.quote_compiler_name(ccompiler)} ${cc_options}'
	if trace_thirdparty_obj_files {
		println('>>> build_thirdparty_obj_files cmd: ${cmd}')
	}
	res := os.execute(cmd)
	os.chdir(current_folder) or {}
	if res.exit_code != 0 {
		eprintln('> Failed build_thirdparty_obj_file cmd')
		eprintln('>           mod: ${mod}')
		eprintln('>          path: ${path}')
		eprintln('>   source_file: ${source_file}')
		eprintln('> wd before cmd: ${current_folder}')
		eprintln('> getwd for cmd: ${v.pref.vroot}')
		eprintln('>           cmd: ${cmd}')
		verror(res.output)
		return
	}
	v.pref.cache_manager.mod_save(mod, '.thirdparty.description.txt', obj_path,
		get_dsc_content('OBJ_PATH: ${obj_path}\nCMD: ${cmd}\n')) or { panic(err) }
	if v.pref.show_cc {
		println('>> OBJECT FILE compilation cmd: ${cmd}')
	}
	if trace_thirdparty_obj_files {
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

fn is_tcc_compilation_failure(ccompiler string, cc_kind CC, output string) bool {
	return cc_kind == .tcc || is_tcc_compiler_name(ccompiler) || is_tcc_alias_compiler(ccompiler)
		|| is_tcc_error_output(output)
}

fn is_tinyc_compiler_label(label string) bool {
	return label.contains('tcc') || label.contains('tinyc') || label.contains('tinygcc')
		|| label.contains('tiny_gcc') || label.contains('tiny-gcc')
}

fn is_tinyc_version_output(output string) bool {
	return output.contains('tiny c compiler') || output.contains('tinycc')
		|| output.contains('tinygcc') || output.contains('tiny_gcc') || output.contains('tiny-gcc')
		|| output.contains('\ntcc') || output.starts_with('tcc')
}

fn is_tcc_compiler_name(ccompiler string) bool {
	name := os.file_name(ccompiler).to_lower()
	return is_tinyc_compiler_label(name)
}

fn is_tcc_error_output(output string) bool {
	trimmed_output := output.trim_space()
	return trimmed_output.starts_with('tcc: error:') || trimmed_output.contains('\ntcc: error:')
}

fn is_tcc_alias_compiler(ccompiler string) bool {
	if ccompiler != 'cc' {
		return false
	}
	cc_version := os.execute('cc --version')
	if cc_version.exit_code != 0 {
		return false
	}
	lcc_version := cc_version.output.to_lower()
	return is_tinyc_version_output(lcc_version)
}

fn ccompiler_is_available(ccompiler string) bool {
	if ccompiler.contains('/') || ccompiler.contains('\\') {
		return os.is_file(ccompiler) && os.is_executable(ccompiler)
	}
	os.find_abs_path_of_executable(ccompiler) or { return false }
	return true
}

fn first_available_ccompiler(excluded []string) string {
	for candidate in ['cc', 'clang', 'gcc'] {
		if candidate in excluded {
			continue
		}
		if os.find_abs_path_of_executable(candidate) or { '' } != '' {
			return candidate
		}
	}
	return ''
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
		os.write_file_array(response_file,
			string_to_ansi_not_null_terminated(response_file_content)) or {
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

fn get_dsc_content(suffix string) string {
	vargs := os.args.join(' ')
	vjobs := os.getenv('VJOBS')
	vflags := os.getenv('VFLAGS')
	return 'CLI: ${vargs}\nVFLAGS="${vflags}"\nVJOBS=${vjobs}\n${suffix}'
}
