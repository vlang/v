module driver

import os
import time
import v.cmdexec
import v.tempname

// The Windows SDK prelude of a generated C unit, as a header: what the unit
// defines and includes before anything of its own (see
// `FlatGen.emit_translation_unit_include_directives` and the lifted system
// includes that follow it). Compiled once per compile-flag set, it saves every
// module object of a Windows Clang build the parse of the SDK headers, which
// is most of the time a small unit takes. A unit uses it only when its own
// prelude is this text line for line (`v3_windows_pch_applies`), so the unit
// sees exactly the declarations and macros it would have parsed itself.
const v3_windows_pch_header = '#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#include <io.h>
#include <fcntl.h>
#include <winsock2.h>
#include <sys/stat.h>
#include <windows.h>
#include <winerror.h>
#include <process.h>
#include <synchapi.h>
#include <time.h>
#define PSAPI_VERSION 2
#include <psapi.h>
#include <stdint.h>
#include <stddef.h>
'

// The include lines of v3_windows_pch_header, in its order.
const v3_windows_pch_includes = [
	'#include <io.h>',
	'#include <fcntl.h>',
	'#include <winsock2.h>',
	'#include <sys/stat.h>',
	'#include <windows.h>',
	'#include <winerror.h>',
	'#include <process.h>',
	'#include <synchapi.h>',
	'#include <time.h>',
	'#include <psapi.h>',
	'#include <stdint.h>',
	'#include <stddef.h>',
]

// The macros a unit defines ahead of the SDK headers that configure them. A
// unit has to define each of them, as the header does, before its first include.
const v3_windows_pch_defines = ['UNICODE', '_UNICODE', 'WIN32_LEAN_AND_MEAN']

// The macros of the prelude that the SDK headers read, with the ones the
// generator's guards test (WIN32_FULL turns the WIN32_LEAN_AND_MEAN guard off,
// _WIN32 every guard): a compile flag that sets or unsets any of them
// configures the headers differently from the prelude.
const v3_windows_pch_sdk_macros = ['UNICODE', '_UNICODE', 'WIN32_LEAN_AND_MEAN', 'WIN32_FULL',
	'PSAPI_VERSION', '_WIN32']

const v3_windows_pch_format = 'v3-windows-pch-v1'

// How long a failed build of the header, recorded beside it, keeps the header
// from being built again, in seconds.
const v3_windows_pch_failed_retry = i64(60 * 60)

// How much of a unit v3_windows_pch_applies reads: the generated prelude is
// well under 4 KiB.
const v3_windows_pch_prelude_limit = 16 * 1024

// v3_windows_pch_applies reports whether `source` opens with the prelude that
// v3_windows_pch_header precompiles: the three configuring macros, then the
// header's includes in its order, with nothing in between that could change what
// the SDK headers declare. Every macro the unit defines or undefines in that
// span has to be one of V's own (`CUSTOM_DEFINE_*`, `V_*`, `V3*`, `_V*`, `v_*`),
// one of the three above, or the `PSAPI_VERSION 2` the header sets. A unit that
// defines anything else first, includes another header first, or includes the
// SDK headers in another order parses them itself.
fn v3_windows_pch_applies(source string) bool {
	mut defines := map[string]bool{}
	mut next_include := 0
	// Whether `PSAPI_VERSION 2` has been defined: psapi.h declares other names
	// without it, and the header defines it first.
	mut psapi_version := false
	// The open conditionals, innermost last: a macro counts only when every
	// conditional around it is the generator's own guard on it or on the target,
	// and no conditional may still be open at the first include.
	mut open := []string{}
	// The prelude is the first few kilobytes of a unit that can run to megabytes.
	// A prelude cut short here does not complete the sequence, which is the answer
	// for a unit whose first include comes that late anyway.
	prelude := if source.len > v3_windows_pch_prelude_limit {
		source[..v3_windows_pch_prelude_limit]
	} else {
		source
	}
	for line in prelude.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.len == 0 || trimmed.starts_with('//') {
			continue
		}
		if !trimmed.starts_with('#') {
			return false
		}
		directive := trimmed[1..].trim_left(' \t')
		if directive.starts_with('include') {
			if next_include == 0 && (defines.len != v3_windows_pch_defines.len || open.len != 0) {
				return false
			}
			if trimmed != v3_windows_pch_includes[next_include] {
				// A header included again is skipped by its include guard.
				if trimmed in v3_windows_pch_includes[..next_include] {
					continue
				}
				return false
			}
			if trimmed == '#include <psapi.h>' && !psapi_version {
				return false
			}
			next_include++
			if next_include == v3_windows_pch_includes.len {
				return true
			}
			continue
		}
		if directive.starts_with('define') || directive.starts_with('undef') {
			rest := directive.all_after(' ').trim_space()
			name := rest.all_before(' ').all_before('(')
			if name in v3_windows_pch_defines {
				if directive.starts_with('undef') || rest != name {
					return false
				}
				// Under any other conditional, a macro of a build could be left out.
				if open.any(!v3_windows_pch_guard_of(it, name)) {
					return false
				}
				defines[name] = true
				continue
			}
			if rest == 'PSAPI_VERSION 2' && directive.starts_with('define') {
				psapi_version = true
				continue
			}
			if !v3_windows_pch_own_macro(name) {
				return false
			}
			continue
		}
		if next_include == 0 && v3_windows_pch_prelude_conditional(directive) {
			if directive == 'endif' {
				if open.len == 0 {
					return false
				}
				open.pop()
			} else {
				open << directive
			}
			continue
		}
		return false
	}
	return false
}

// v3_windows_pch_guard_of reports whether the conditional `directive` is one
// the generator puts around the definition of `name`: the guard on the Windows
// target, or the guard on the macro itself.
fn v3_windows_pch_guard_of(directive string, name string) bool {
	if directive == 'if defined(_WIN32)' || directive == 'ifndef ${name}' {
		return true
	}
	return name == 'WIN32_LEAN_AND_MEAN'
		&& directive == 'if defined(_WIN32) && !defined(WIN32_FULL) && !defined(WIN32_LEAN_AND_MEAN)'
}

// v3_windows_pch_flags_configure_sdk reports whether a compile flag could make
// the SDK headers declare other names than the precompiled header has, or is
// not known not to. A `-D` or `-U` of a macro the prelude sets or its guards
// test does: with `-DWIN32_FULL` a unit leaves WIN32_LEAN_AND_MEAN undefined
// and parses the whole of windows.h, which the header did not. So does any
// flag that can carry a macro out of sight (a file included ahead of the unit,
// options passed on to the preprocessor or read from a file), which Clang does
// not check against the precompiled header. Only flags of a known shape pass:
// the definition of a macro by a plain identifier, an include directory, and
// the families that select the language standard, optimization, code
// generation and warnings.
fn v3_windows_pch_flags_configure_sdk(args []string) bool {
	mut operand_of := ''
	for arg in args {
		if operand_of != '' {
			option := operand_of
			operand_of = ''
			if option in v3_windows_pch_macro_options && !v3_windows_pch_plain_macro(arg) {
				return true
			}
			continue
		}
		if arg in v3_windows_pch_macro_options || arg in ['-I', '-isystem', '-idirafter', '-iquote'] {
			operand_of = arg
			continue
		}
		if arg.starts_with('-D') || arg.starts_with('-U') {
			if !v3_windows_pch_plain_macro(arg[2..]) {
				return true
			}
			continue
		}
		if arg.starts_with('--define-macro=') || arg.starts_with('--undefine-macro=') {
			if !v3_windows_pch_plain_macro(arg.all_after('=')) {
				return true
			}
			continue
		}
		if arg.starts_with('-I') || arg.starts_with('-O') || arg.starts_with('-std=')
			|| arg.starts_with('-f') || arg.starts_with('-m') || arg.starts_with('-g')
			|| arg in ['-w', '-pipe', '-pthread'] {
			continue
		}
		if arg.starts_with('-W') && !arg.starts_with('-Wp,') && !arg.starts_with('-Wa,')
			&& !arg.starts_with('-Wl,') {
			continue
		}
		return true
	}
	// An option left without its operand fails the compile with or without the header.
	return false
}

// The options that take the definition of a macro as their operand.
const v3_windows_pch_macro_options = ['-D', '-U', '--define-macro', '--undefine-macro']

// v3_windows_pch_plain_macro reports whether `definition`, the operand of a
// `-D` or `-U`, names by a plain identifier a macro the SDK headers do not
// read. The name runs to the `=` of a value or the `(` of parameters; Clang
// defines a macro from anything else there as well (`WIN32_FULL/**/`,
// `WIN32_FULL,1`), so no such definition passes.
fn v3_windows_pch_plain_macro(definition string) bool {
	mut end := 0
	for end < definition.len && (definition[end].is_alnum() || definition[end] == `_`) {
		end++
	}
	name := definition[..end]
	if name == '' || name in v3_windows_pch_sdk_macros {
		return false
	}
	return end == definition.len || definition[end] in [`=`, `(`]
}

// v3_windows_pch_prelude_conditional reports whether `directive` is one of the
// conditionals the generator puts around the macros of the prelude: a guard on
// the Windows target or on the macro being defined already. Any other
// conditional, in particular one between the includes, could leave a macro or
// a header out of what the compiler sees, so the unit parses the headers itself.
fn v3_windows_pch_prelude_conditional(directive string) bool {
	if directive == 'endif' || directive == 'if defined(_WIN32)'
		|| directive == 'if defined(_WIN32) && !defined(WIN32_FULL) && !defined(WIN32_LEAN_AND_MEAN)' {
		return true
	}
	if directive.starts_with('ifndef ') {
		name := directive.all_after(' ').trim_space()
		return name in v3_windows_pch_defines || v3_windows_pch_own_macro(name)
	}
	return false
}

// v3_windows_pch_own_macro reports whether `name` is a macro V emits for itself:
// one the SDK headers never read.
fn v3_windows_pch_own_macro(name string) bool {
	return name.starts_with('CUSTOM_DEFINE_') || name.starts_with('V_') || name.starts_with('V3')
		|| name.starts_with('_V') || name.starts_with('v_')
}

// v3_windows_pch_compiler reports whether `compiler` is a Clang driver, which
// takes a precompiled header with `-include-pch`. GCC looks for `.gch` files
// beside the header instead, TinyCC and MSVC's `cl` take neither.
fn v3_windows_pch_compiler(compiler string) bool {
	name := os.file_name(compiler).to_lower_ascii()
	return name.contains('clang') && !name.contains('clang-cl')
}

// v3_windows_pch_key names the precompiled header of one compiler and one
// compile-flag set. Any difference in either gives a different header, so a
// changed flag never meets a precompiled header built without it: Clang rejects
// a header whose language standard, optimization level, or macros differ.
fn v3_windows_pch_key(identity string, args []string) string {
	mut hash := u64(1469598103934665603)
	for part in [v3_windows_pch_format, identity, v3_windows_pch_header, args.join('\x00')] {
		hash = c_hash_bytes(hash, part.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

// v3_windows_pch returns the precompiled v3_windows_pch_header for a compile by
// `compiler` with `args` (everything on its command line but `-c`, the output
// and the source), building it in `cache_dir` when this flag set has none yet.
// It returns none when the compile cannot use one: another host or compiler, a
// cross target, a compiler without an identity, a flag that configures the SDK
// headers (`-DWIN32_FULL`, `-include`), or a header whose build failed within
// the hour (recorded beside it, so the build is not retried for every unit). Building
// is the compile of the header with the same arguments, so an SDK or compiler
// that changes under the cache gives a header its compiles reject; the
// compile's caller then drops it with v3_windows_pch_discard.
fn v3_windows_pch(compiler V3CachedObjectCompiler, args []string, cache_dir string) ?string {
	$if !windows {
		return none
	}
	if compiler.identity == '' || !v3_windows_pch_compiler(compiler.path)
		|| v3_c_flags_retarget_clang(args) || v3_windows_pch_flags_configure_sdk(args) {
		return none
	}
	base := os.join_path(cache_dir, 'windows_sdk_${v3_windows_pch_key(compiler.identity, args)}')
	pch := base + '.pch'
	header := base + '.h'
	failed := base + '.failed'
	if os.is_file(pch) && os.is_file(header) {
		return pch
	}
	if os.is_file(failed) {
		// A build that failed for a passing reason (a disk that was full, a
		// scanner holding the output) is tried again after a while.
		if time.now().unix() - os.file_last_mod_unix(failed) < v3_windows_pch_failed_retry {
			return none
		}
		os.rm(failed) or {}
	}
	unique := tempname.unique_token()
	tmp_pch := '${pch}.tmp.${unique}'
	defer { os.rm(tmp_pch) or {} }
	if !os.is_file(header) {
		// Clang records the header's path and modification time in the
		// precompiled header and checks them at every use, so the header stays
		// where it was compiled and is written once: its text never changes.
		// Another build that wrote it first wins, so that its compiler, which
		// may be reading it, never finds it gone.
		tmp_header := '${header}.tmp.${unique}'
		os.write_file(tmp_header, v3_windows_pch_header) or {
			os.rm(tmp_header) or {}
			return none
		}
		os.mv(tmp_header, header, overwrite: false) or {
			os.rm(tmp_header) or {}
			if !os.is_file(header) {
				return none
			}
		}
	}
	mut build_args := args.clone()
	build_args << ['-x', 'c-header', header, '-o', tmp_pch]
	trace := os.getenv('V3_CACHE_TRACE') != ''
	if trace {
		eprintln('  V3 module cache: precompiling Windows SDK headers: ${cmdexec.display(compiler.path, build_args)}')
	}
	result := cmdexec.run(compiler.path, build_args)
	if result.exit_code != 0 {
		if trace {
			eprintln('  V3 module cache: Windows SDK headers do not precompile with these flags:\n${result.output}')
		}
		os.write_file(failed, result.output) or {}
		return none
	}
	// One of the same flag set that another build published meanwhile is the
	// same header for the same compiler; it stays.
	os.mv(tmp_pch, pch, overwrite: false) or {
		if !os.is_file(pch) {
			return none
		}
	}
	return pch
}

// v3_windows_pch_discard removes a precompiled header a compile was rejected
// with. The next compile of its flag set builds it again from the current SDK
// and compiler.
fn v3_windows_pch_discard(pch string) {
	os.rm(pch) or {}
}
