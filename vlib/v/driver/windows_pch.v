module driver

import os
import time
import v.cmdexec
import v.modulecache
import v.tempname

// The Windows SDK prelude of a generated C unit, as a header: what the unit
// defines and includes before anything of its own (see
// `FlatGen.emit_translation_unit_include_directives` and the lifted system
// includes that follow it), under the same guards, so that a macro the
// compiler predefines (`-municode` is `-DUNICODE`) keeps the value it has in
// the unit. Compiled once per compile-flag set, it saves every
// module object of a Windows Clang build the parse of the SDK headers, which
// is most of the time a small unit takes. A unit uses it only when its own
// prelude is this text line for line (`v3_windows_pch_applies`), so the unit
// sees exactly the declarations and macros it would have parsed itself.
const v3_windows_pch_header = '#if defined(_WIN32)
#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif
#endif
#if defined(_WIN32) && !defined(WIN32_FULL) && !defined(WIN32_LEAN_AND_MEAN)
#define WIN32_LEAN_AND_MEAN
#endif
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
// span has to be one of V's own (`CUSTOM_DEFINE_*`, `V_*`, `V3*`, `v_*`, `_VPROFILE`),
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
// generation and warnings. A directory searched for `#include "..."` alone
// (`-iquote`, or `-I` before `-I-`) is refused as well: Clang prints the
// directories of both searches as one list, where the record would place it
// before directories it is searched after (see v3_windows_pch_inputs).
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
		if arg in v3_windows_pch_macro_options || arg in ['-I', '-isystem', '-idirafter'] {
			operand_of = arg
			continue
		}
		if arg == '-I-' {
			return true
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
// one the SDK headers never read. Of the names with a leading underscore only
// `_VPROFILE` is V's; the C headers read `_VA_LIST` and `_VA_LIST_DEFINED`.
fn v3_windows_pch_own_macro(name string) bool {
	return name.starts_with('CUSTOM_DEFINE_') || name.starts_with('V_') || name.starts_with('V3')
		|| name.starts_with('v_') || name == '_VPROFILE'
}

// v3_windows_pch_compiler reports whether `compiler` is a Clang driver, which
// takes a precompiled header with `-include-pch`. GCC looks for `.gch` files
// beside the header instead, TinyCC and MSVC's `cl` take neither.
fn v3_windows_pch_compiler(compiler string) bool {
	name := os.file_name(compiler).to_lower_ascii()
	return name.contains('clang') && !name.contains('clang-cl')
}

// v3_windows_pch_key names the precompiled header of one compiler, one
// compile-flag set and the environment they compile in. Clang rejects a header
// whose language standard or optimization level differ, but checks a
// command-line macro only when the header used it, and the SDK headers it read
// only while they are where it found them, so a precompiled header is kept with
// a record of what it was made of (see v3_windows_pch_kept). The key holds what
// commonly sends the compiler to other headers, so that two environments in use
// side by side have a header each instead of replacing each other's: the
// variables of v3_link_environment_names (CPATH, C_INCLUDE_PATH, ...), PATH (a
// MinGW Clang without a sysroot of its own takes the headers of the first MinGW
// GCC on it), and the directory that relative include directories are found from.
fn v3_windows_pch_key(identity string, args []string) string {
	mut hash := u64(1469598103934665603)
	for part in [v3_windows_pch_format, identity, v3_windows_pch_header, args.join('\x00'),
		v3_link_environment_signature(), 'PATH=${os.getenv('PATH')}', os.getwd()] {
		hash = c_hash_bytes(hash, part.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

// v3_windows_pch returns the precompiled v3_windows_pch_header for a compile by
// `compiler` with `args` (everything on its command line but `-c`, the output
// and the source), building it in `cache_dir` when this flag set has none yet,
// or none that was made of the headers the compiler finds now. It returns none
// when the compile cannot use one: another host or compiler, a cross target, a
// compiler without an identity, a flag that configures the SDK headers
// (`-DWIN32_FULL`, `-include`, CCC_OVERRIDE_OPTIONS), or a header whose build
// failed within the hour (recorded beside it, so the build is not retried for
// every unit). `known` holds the answer for each flag set of this build, so the
// compiler is asked where it searches, and the record checked, once a build. A
// compile that rejects the header anyway drops it with v3_windows_pch_discard.
fn v3_windows_pch(compiler V3CachedObjectCompiler, args []string, cache_dir string, mut known map[string]string) ?string {
	$if !windows {
		return none
	}
	// CCC_OVERRIDE_OPTIONS edits the command line out of sight, as `--config` does.
	if compiler.identity == '' || !v3_windows_pch_compiler(compiler.path)
		|| v3_c_flags_retarget_clang(args) || v3_windows_pch_flags_configure_sdk(args)
		|| os.getenv('CCC_OVERRIDE_OPTIONS') != '' {
		return none
	}
	base := os.join_path(cache_dir, 'windows_sdk_${v3_windows_pch_key(compiler.identity, args)}')
	if base in known {
		pch := known[base]
		if pch == '' {
			return none
		}
		return pch
	}
	pch := v3_windows_pch_of(compiler, args, base) or {
		known[base] = ''
		return none
	}
	known[base] = pch
	return pch
}

// v3_windows_pch_of returns the precompiled header of the flag set `base` names:
// the one its record (`base.inputs`) names when that holds for the headers the
// compiler finds now, or else a new one, built and recorded.
fn v3_windows_pch_of(compiler V3CachedObjectCompiler, args []string, base string) ?string {
	header := base + '.h'
	record := base + '.inputs'
	failed := base + '.failed'
	search := v3_windows_pch_search(compiler.path, args)?
	if text := os.read_file(record) {
		if pch := v3_windows_pch_kept(text, base, search) {
			return pch
		}
		// It was made of other headers than the compiler finds now.
		v3_windows_pch_forget(base, text)
	}
	if os.is_file(failed) {
		// A build that failed for a passing reason (a disk that was full, a
		// scanner holding the output) is tried again after a while.
		if time.now().unix() - os.file_last_mod_unix(failed) < v3_windows_pch_failed_retry {
			return none
		}
		os.rm(failed) or {}
	}
	unique := tempname.unique_token().replace('.', '_')
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
	// Each build gets a file of its own, which the record names once it is
	// complete: a compiler reading one never finds it replaced.
	pch := '${base}.${unique}.pch'
	deps := '${base}.${unique}.d'
	defer { os.rm(deps) or {} }
	// Whole seconds, and one to spare for a file system that rounds them.
	before := time.utc().unix() - 1
	trace := os.getenv('V3_CACHE_TRACE') != ''
	// The headers the compiler reads, and which files they are, before it
	// builds: a file put at the path of one of them while it builds (a directory
	// swapped in by an upgrade) keeps its times, but is another file.
	first := v3_windows_pch_reads(compiler.path, args, header) or { return none }
	mut was := map[string]string{}
	for path in first {
		was[path] = v3_windows_pch_identity_from_before(path, before) or {
			if trace {
				eprintln('  V3 module cache: the Windows SDK headers for ${pch} cannot be told apart from newer ones')
			}
			// On a file system that keeps no identity of a file (FAT, some network
			// shares) every build would build it again for nothing: this is
			// recorded as a failed build is, and tried again after a while.
			os.write_file(failed, 'the headers read cannot be told apart from newer ones\n') or {}
			return none
		}
	}
	mut build_args := args.clone()
	build_args << ['-x', 'c-header', header, '-o', pch, '-MD', '-MF', deps]
	if trace {
		eprintln('  V3 module cache: precompiling Windows SDK headers: ${cmdexec.display(compiler.path, build_args)}')
	}
	result := cmdexec.run(compiler.path, build_args)
	if result.exit_code != 0 {
		if trace {
			eprintln('  V3 module cache: Windows SDK headers do not precompile with these flags:\n${result.output}')
		}
		os.rm(pch) or {}
		os.write_file(failed, result.output) or {}
		return none
	}
	own := v3_windows_pch_path(header)
	mut read := v3_parse_dependency_file(os.read_file(deps) or { '' }).map(v3_windows_pch_path(it)).filter(it != own)
	read.sort()
	// A header that changed or moved while it was read, or one the compiler
	// did not say it read, leaves the header unknown: this build compiles
	// without it. The places of the record are taken first, and the compiler is
	// asked again after: a header that came where it looks while it built, as a
	// file or with a directory or a link moved there, keeps the times it had and
	// cannot be told from one it passed over, and one that comes after this is
	// recorded where none was.
	inputs := v3_windows_pch_inputs(read, search, header, before) or {
		if trace {
			eprintln('  V3 module cache: the places of the Windows SDK headers read for ${pch} cannot be told')
		}
		os.rm(pch) or {}
		// A file that keeps no identity, or a name a macro gives, does so in the
		// next build as well: this is recorded as a failed build is.
		os.write_file(failed, 'the headers read cannot be told apart from newer ones, or include a name a macro gives\n') or {}
		return none
	}
	after := v3_windows_pch_reads(compiler.path, args, header) or { []string{} }
	if inputs.files.len == 0 || read != first || after != first
		|| inputs.files.any(it in was && was[it] != inputs.identities[inputs.files.index(it)]) {
		if trace {
			eprintln('  V3 module cache: the Windows SDK headers changed while ${pch} was built')
		}
		os.rm(pch) or {}
		return none
	}
	tmp_record := '${record}.tmp.${unique}'
	os.write_file(tmp_record, v3_windows_pch_record_text(os.file_name(pch), search, &inputs)) or {
		os.rm(tmp_record) or {}
		os.rm(pch) or {}
		return none
	}
	os.mv(tmp_record, record, overwrite: false) or {
		// Another build recorded one meanwhile: that one is taken when it holds.
		os.rm(tmp_record) or {}
		os.rm(pch) or {}
		return v3_windows_pch_kept(os.read_file(record) or { return none }, base, search)
	}
	return pch
}

// v3_windows_pch_search returns where `compiler` looks for an included file when
// it compiles with `args` in this directory, in the order of the search, as
// v3_windows_pch_path spells them.
fn v3_windows_pch_search(compiler string, args []string) ?[]string {
	mut query := args.clone()
	// Clang does not open os.path_devnull, `\\.\nul`, as a file.
	query << ['-Wno-unused-command-line-argument', '-x', 'c', '-E', '-v', 'NUL']
	result := v3_run_in_c_locale(compiler, query, os.getwd())
	if result.exit_code != 0 {
		return none
	}
	search := v3_parse_include_search(result.output)?
	return search.dirs.map(v3_windows_pch_path(it))
}

// v3_windows_pch_path spells `path` as the records of precompiled headers do:
// absolute, without `.` or `..`, with forward slashes.
fn v3_windows_pch_path(path string) string {
	absolute := if os.is_abs_path(path) { path } else { os.join_path(os.getwd(), path) }
	return os.norm_path(absolute).replace('\\', '/')
}

// v3_windows_pch_inputs returns what a precompiled header that a compiler built
// from `header`, reading the headers `read` and looking for them in the
// directories of `search`, depends on: those files, and every place where a
// file would have been read instead of one of them or in addition to it. The
// places come from the names the headers include, as they write them (a name
// may leave its directory with `..`, and one header may be included under two
// spellings, which a directory that tells case apart holds as two files): a
// name in angle brackets in each directory of the search, and a name in
// quotation marks, or one a header asks `__has_include` about, there and in
// the directory of each header that can be on the include stack (Clang for a
// MinGW target looks in the includer's own, for an MSVC target in every one up
// to `header`'s). A file that is in such a place is an input with its metadata
// (`#include_next` passes over it), and a place where none is, an input as a
// missing file. It returns none when a header includes a name a macro gives,
// which no place can be taken of, or when a file was written or put in place at
// `before`, in seconds, or later, so that it may not be the one that was read,
// or may not have been there when the compiler looked.
fn v3_windows_pch_inputs(read []string, search []string, header string, before i64) ?V3HeaderInputs {
	mut inputs := V3HeaderInputs{}
	for path in read {
		inputs.add_windows_pch_place(path, before)?
	}
	mut includers := []string{}
	mut angled := []string{}
	mut quoted := []string{}
	mut headers := [v3_windows_pch_path(header)]
	headers << read
	for path in headers {
		dir := path.all_before_last('/')
		if dir !in includers {
			includers << dir
		}
		text := os.read_file(path) or { return none }
		angled_names, quoted_names := v3_windows_pch_include_names(text)?
		for name in angled_names {
			if name !in angled {
				angled << name
			}
		}
		for name in quoted_names {
			if name !in quoted {
				quoted << name
			}
		}
	}
	for name in angled {
		for dir in search {
			inputs.add_windows_pch_place(v3_windows_pch_path(dir + '/' + name), before)?
		}
	}
	for name in quoted {
		for dir in includers {
			inputs.add_windows_pch_place(v3_windows_pch_path(dir + '/' + name), before)?
		}
		for dir in search {
			inputs.add_windows_pch_place(v3_windows_pch_path(dir + '/' + name), before)?
		}
	}
	return inputs
}

// v3_windows_pch_include_names returns the names the C `text` includes in angle
// brackets, and those it includes in quotation marks or asks `__has_include`
// about, each as it writes them, or none when it includes or asks about a name
// a macro gives. Every directive of the text counts, the ones a conditional
// leaves out as well.
fn v3_windows_pch_include_names(text string) ?([]string, []string) {
	mut angled := []string{}
	mut quoted := []string{}
	for line in text.split_into_lines() {
		trimmed := line.trim_space()
		if !trimmed.starts_with('#') {
			continue
		}
		directive := trimmed[1..].trim_left(' \t')
		for keyword in ['include_next', 'include', 'import'] {
			if !directive.starts_with(keyword) {
				continue
			}
			rest := directive[keyword.len..].trim_left(' \t')
			close := if rest.starts_with('<') {
				'>'
			} else if rest.starts_with('"') {
				'"'
			} else {
				return none
			}
			end := rest.index_after_(close, 1)
			if end <= 1 {
				return none
			}
			name := rest[1..end]
			if close == '>' {
				if name !in angled {
					angled << name
				}
			} else if name !in quoted {
				quoted << name
			}
			break
		}
	}
	// `__has_include(` followed by anything but a written name asks about one a
	// macro gives.
	mut pos := 0
	for {
		found := text.index_after_('__has_include', pos)
		if found < 0 {
			break
		}
		pos = found + '__has_include'.len
		mut next := pos
		if text[next..].starts_with('_next') {
			next += '_next'.len
		}
		for next < text.len && text[next] in [` `, `\t`] {
			next++
		}
		if next >= text.len || text[next] != `(` {
			continue
		}
		next++
		for next < text.len && text[next] in [` `, `\t`] {
			next++
		}
		if next >= text.len || text[next] !in [`<`, `"`] {
			return none
		}
	}
	for name in v3_has_include_names(text) {
		if name !in quoted {
			quoted << name
		}
	}
	return angled, quoted
}

// v3_windows_pch_reads returns the headers that `compiler`, preprocessing
// `header` with `args` now, reads, as v3_windows_pch_path spells them, sorted.
fn v3_windows_pch_reads(compiler string, args []string, header string) ?[]string {
	deps := '${header}.check.${tempname.unique_token().replace('.', '_')}.d'
	defer { os.rm(deps) or {} }
	mut check_args := args.clone()
	check_args << ['-x', 'c-header', header, '-M', '-MF', deps]
	if cmdexec.run(compiler, check_args).exit_code != 0 {
		return none
	}
	own := v3_windows_pch_path(header)
	mut read := v3_parse_dependency_file(os.read_file(deps) or { return none }).map(v3_windows_pch_path(it)).filter(it != own)
	read.sort()
	return read
}

// add_windows_pch_place takes the place `path` into `inputs`: the file that is
// there with its metadata, or the path as one where no file is. It returns none
// when the file was written or put in place at `before` or later.
fn (mut inputs V3HeaderInputs) add_windows_pch_place(path string, before i64) ? {
	if path in inputs.files || path in inputs.missing {
		return
	}
	if !os.exists(path) {
		inputs.missing << path
		return
	}
	inputs.identities << v3_windows_pch_identity_from_before(path, before)?
	inputs.files << path
}

// v3_windows_pch_identity_from_before returns the identity of the file at `path`
// (modulecache.file_metadata_signature) when it was last written and its entry
// last changed before `before`, in seconds. The identity holds both times as the
// file system keeps them, the time of change last: a file moved or linked into
// place keeps its time of writing and of creation but gets a new time of change.
// os.stat would give the time of creation in its place on Windows, and its times
// are an hour off in the hour repeated at the end of daylight saving time.
fn v3_windows_pch_identity_from_before(path string, before i64) ?string {
	identity := modulecache.file_metadata_signature(path)
	if identity == '' || path.contains_any('\t\n') {
		return none
	}
	fields := identity.split(':')
	if fields.len < 4 {
		return none
	}
	for seconds in [fields[fields.len - 4], fields[fields.len - 2]] {
		if seconds.len == 0 || !seconds.bytes().all(it.is_digit()) || seconds.i64() >= before {
			return none
		}
	}
	return identity
}

// v3_windows_pch_record_text is the record of the precompiled header `pch_name`:
// the directories the compiler searched, in their order, and its inputs.
fn v3_windows_pch_record_text(pch_name string, search []string, inputs &V3HeaderInputs) string {
	mut text := 'pch=${pch_name}\n'
	for dir in search {
		text += 'search=${dir}\n'
	}
	return text + v3_header_inputs_text(inputs) + 'complete=1\n'
}

// v3_windows_pch_record_pch returns the precompiled header that the record
// `text` of the flag set `base` names: a file beside it of the name of one that
// v3_windows_pch_of builds.
fn v3_windows_pch_record_pch(text string, base string) ?string {
	first := text.all_before('\n')
	if !first.starts_with('pch=') {
		return none
	}
	name := first['pch='.len..]
	if !name.starts_with(os.file_name(base) + '.') || !name.ends_with('.pch')
		|| name.contains_any('/\\') {
		return none
	}
	return os.join_path(os.dir(base), name)
}

// v3_windows_pch_kept returns the precompiled header that the record `text` of
// the flag set `base` names when it holds now: the compiler searches the
// directories it searched, in their order, every file it depends on is the one
// it was, older than now, and no file is where none was.
fn v3_windows_pch_kept(text string, base string, search []string) ?string {
	pch := v3_windows_pch_record_pch(text, base)?
	lines := text.split_into_lines()
	mut recorded := []string{}
	mut i := 1
	for i < lines.len && lines[i].starts_with('search=') {
		recorded << lines[i]['search='.len..]
		i++
	}
	if recorded != search || !os.is_file(pch) || !os.is_file(base + '.h') {
		return none
	}
	rest := lines[i..]
	if rest.len == 0 || rest.last() != 'complete=1' {
		return none
	}
	mut inputs := V3HeaderInputs{}
	for line in rest[..rest.len - 1] {
		if !inputs.read_line(line) {
			return none
		}
	}
	if inputs.files.len == 0 {
		return none
	}
	now := time.utc().unix()
	for k, path in inputs.files {
		if v3_windows_pch_identity_from_before(path, now) or { return none } != inputs.identities[k] {
			return none
		}
	}
	if inputs.missing.any(os.exists(it)) {
		return none
	}
	return pch
}

// v3_windows_pch_forget removes the record `text` of the flag set `base`, and the
// precompiled header it names, unless another build replaced it meanwhile.
fn v3_windows_pch_forget(base string, text string) {
	record := base + '.inputs'
	if os.read_file(record) or { return } != text {
		return
	}
	os.rm(record) or {}
	if pch := v3_windows_pch_record_pch(text, base) {
		os.rm(pch) or {}
	}
}

// v3_windows_pch_discard removes a precompiled header a compile was rejected
// with, and its record: the next build makes it again from the current SDK and
// compiler. This build compiles the rest of the flag set without one.
fn v3_windows_pch_discard(pch string, mut known map[string]string) {
	for base in known.keys() {
		if known[base] != pch {
			continue
		}
		known[base] = ''
		record := base + '.inputs'
		if text := os.read_file(record) {
			if v3_windows_pch_record_pch(text, base) or { '' } == pch {
				os.rm(record) or {}
			}
		}
	}
	os.rm(pch) or {}
}
