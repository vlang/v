module fastc

import strings
import os
import v.pref
import v.scanner
import v.token

// fastc_target_runtime_preferences selects bodyless libc bindings supplied by
// the target headers and FastC's native declaration emitter.
fn fastc_target_runtime_preferences(prefs &pref.Preferences) pref.Preferences {
	mut scoped := *prefs
	if scoped.target_libc_headers {
		scoped.user_defines = scoped.user_defines.clone()
		for define in ['v3_c_extern_declarations', 'no_segfault_handler'] {
			if define !in scoped.user_defines {
				scoped.user_defines << define
			}
		}
	}
	return scoped
}

// fastc_target_libc_preamble uses the target's core libc headers and its real
// allocator, without requiring hosted POSIX thread-local storage.
fn fastc_target_libc_preamble() string {
	headers := '#define V_FASTC_TARGET_LIBC 1\n#include <stddef.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#define __offsetof(st, m) offsetof(st, m)\n'
	return c_selfhost_preamble.replace(c_selfhost_preamble_includes, headers) + '
typedef struct { u64 arg0; u64 arg1; } v_fastc_native_pair_u64;
static inline v_fastc_native_pair_u64 v_fastc_native_pair_from_multi(MultiReturn value) {
	v_fastc_native_pair_u64 result;
	memcpy(&result.arg0, value.values[0].data, sizeof(result.arg0));
	memcpy(&result.arg1, value.values[1].data, sizeof(result.arg1));
	return result;
}
'
}

// fastc_target_collect_native_context tracks qualified C references and header
// ownership in V source, following only selected compile-time branches. C headers
// belong to the C compiler and are never opened or parsed by FastC.
fn fastc_target_collect_native_context(source string, path string, prefs &pref.Preferences, mut references map[string]bool, mut declarations map[string]bool) bool {
	return fastc_target_collect_native_facts(source, path, prefs, mut references, mut declarations).has_header
}

struct FastcNativeSourceFacts {
mut:
	has_header   bool
	links_native bool
	// local_headers lists the resolved paths of selected `#include "path"` headers.
	local_headers []string
}

// fastc_target_collect_native_facts also records whether the file's selected
// `#flag` directives link a C/Objective-C source or object, like cgen's
// files_linking_c_sources.
fn fastc_target_collect_native_facts(source string, path string, prefs &pref.Preferences, mut references map[string]bool, mut declarations map[string]bool) FastcNativeSourceFacts {
	// Only these V-source lexemes can produce a native reference,
	// declaration or header directive. Comments and strings stay candidates.
	if !source.contains('#') && !fastc_source_contains_word(source, 'C') {
		return FastcNativeSourceFacts{}
	}
	mut scan := scanner.new_scanner(prefs, .normal)
	file := token.File.unindexed(path, source.len)
	scan.init(file, source)
	mut previous := token.Token.unknown
	mut facts := FastcNativeSourceFacts{}
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .dollar {
			mut selected_scan := scan
			if selected_scan.scan() == .key_if {
				if selected := fastc_scan_selected_comptime_branch(mut selected_scan, .key_if, path, prefs) {
					if selected.source != '' {
						branch := fastc_target_collect_native_facts(selected.source, path, prefs, mut references, mut declarations)
						facts.has_header = facts.has_header || branch.has_header
						facts.links_native = facts.links_native || branch.links_native
						facts.local_headers << branch.local_headers
					}
					scan = selected_scan
					tok = selected.tok
					previous = .unknown
					continue
				}
			}
		}
		if tok == .hash {
			directive := scan.lit.trim_space().fields()
			if directive.len > 0 && directive[0] in ['include', 'preinclude', 'insert', 'flag'] {
				qualifier := if directive.len > 1 { directive[1] } else { '' }
				if qualifier !in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd',
					'dragonfly', 'solaris', 'android'] || pref.comptime_flag_value(prefs, qualifier) {
					if directive[0] != 'flag' {
						facts.has_header = true
						if directive[0] == 'include' {
							if header := fastc_native_quoted_include_path(scan.lit, path, prefs) {
								facts.local_headers << header
							}
						}
					} else if directive[1..].any(!it.starts_with('-')
						&& (it.ends_with('.c') || it.ends_with('.cpp') || it.ends_with('.cc')
							|| it.ends_with('.m') || it.ends_with('.mm') || it.ends_with('.o')
							|| it.ends_with('.obj'))) {
						facts.links_native = true
					}
				}
			}
		}
		if tok == .name && scan.lit == 'C' && previous != .key_fn {
			mut look := scan
			if look.scan() == .dot {
				name_token := look.scan()
				if name_token == .name || name_token.is_keyword() {
					references[look.lit] = true
				}
			}
		}
		if tok == .key_fn {
			mut look := scan
			if look.scan() == .name && look.lit == 'C' && look.scan() == .dot {
				name_token := look.scan()
				if name_token == .name || name_token.is_keyword() {
					declarations[look.lit] = true
				}
			}
		}
		previous = tok
		tok = scan.scan()
	}
	return facts
}

struct FastcNativeSourceContext {
mut:
	references                   map[string]bool
	files_with_headers           map[string]bool
	files_linking_native_sources map[string]bool
	declaration_paths            map[string][]string
	// local_headers maps a V source path to its selected quoted-include headers.
	local_headers map[string][]string
}

fn fastc_native_source_context_chunk(sources []FastcSourceFile, prefs &pref.Preferences, start int, end int) FastcNativeSourceContext {
	mut context := FastcNativeSourceContext{}
	for i in start .. end {
		source := sources[i]
		mut declarations := map[string]bool{}
		facts := fastc_target_collect_native_facts(source.source, source.path, prefs, mut context.references, mut declarations)
		if facts.has_header {
			context.files_with_headers[source.path] = true
		}
		if facts.links_native {
			context.files_linking_native_sources[source.path] = true
		}
		if facts.local_headers.len > 0 {
			context.local_headers[source.path] = facts.local_headers
		}
		for name, _ in declarations {
			mut paths := context.declaration_paths[name] or { []string{} }
			paths << source.path
			context.declaration_paths[name] = paths
		}
	}
	return context
}

// Merge contiguous source chunks in order so duplicate declarations retain
// every origin in the same order as the serial scanner.
fn fastc_merge_native_source_context(mut context FastcNativeSourceContext, chunk FastcNativeSourceContext) {
	for name, _ in chunk.references {
		context.references[name] = true
	}
	for path, _ in chunk.files_with_headers {
		context.files_with_headers[path] = true
	}
	for path, _ in chunk.files_linking_native_sources {
		context.files_linking_native_sources[path] = true
	}
	for path, headers in chunk.local_headers {
		context.local_headers[path] = headers
	}
	for name, chunk_paths in chunk.declaration_paths {
		mut paths := context.declaration_paths[name] or { []string{} }
		paths << chunk_paths
		context.declaration_paths[name] = paths
	}
}

// fastc_native_quoted_include_path resolves the `"path"` operand of an `#include`
// directive body the way the C compiler will see it: `@VEXEROOT`/`@VMODROOT`
// expanded and relative paths taken from the including V file's directory.
// System (`<...>`) includes return none.
fn fastc_native_quoted_include_path(directive string, source_path string, prefs &pref.Preferences) ?string {
	open_quote := directive.index('"') or { return none }
	close_quote := directive.index_after_('"', open_quote + 1)
	if close_quote <= open_quote + 1 {
		return none
	}
	mut header := directive[open_quote + 1..close_quote]
	if header.contains('@VEXEROOT') || header.contains('@VROOT') {
		header = header.replace('@VEXEROOT', prefs.vroot).replace('@VROOT', prefs.vroot)
	}
	if header.contains('@VMODROOT') {
		header = header.replace('@VMODROOT', fastc_vmod_root_for_file(source_path))
	}
	if !os.is_abs_path(header) {
		header = os.join_path(os.dir(source_path), header)
	}
	return header
}

// fastc_c_header_declares_function reports whether the C header `text` names
// `name` as a function, i.e. `name` as a whole identifier followed by `(`.
fn fastc_c_header_declares_function(text string, name string) bool {
	if name.len == 0 {
		return false
	}
	mut start := 0
	for start < text.len {
		at := text.index_after_(name, start)
		if at < 0 {
			return false
		}
		start = at + name.len
		if at > 0 && (text[at - 1].is_alnum() || text[at - 1] == `_`) {
			continue
		}
		mut i := start
		for i < text.len && text[i] in [` `, `\t`] {
			i++
		}
		if i < text.len && text[i] == `(` {
			return true
		}
	}
	return false
}

// fastc_target_libc_declares leaves the standard headers' exact qualifiers
// and integer widths intact rather than redeclaring their approximate V view.
fn fastc_target_libc_declares(name string) bool {
	return name in ['malloc', 'calloc', 'realloc', 'free', 'exit', 'abort', 'atoi', 'atol', 'atoll',
		'atof', 'qsort', 'bsearch', 'getenv', 'setenv', 'unsetenv', 'rand', 'srand', 'memcpy',
		'memmove', 'memcmp', 'memset', 'memchr', 'strcpy', 'strncpy', 'strcmp', 'strncmp', 'strlen',
		'strchr', 'strrchr', 'strstr', 'strcat', 'strncat', 'strerror', 'printf', 'sprintf', 'snprintf',
		'fprintf', 'vprintf', 'vsprintf', 'vsnprintf', 'vfprintf', 'fflush', 'fopen', 'freopen',
		'fclose', 'fread', 'fwrite', 'fseek', 'ftell', 'rewind', 'feof', 'ferror', 'fgets', 'fputs',
		'fgetc', 'fputc', 'getc', 'putc', 'getchar', 'putchar', 'puts', 'scanf', 'sscanf', 'fscanf',
		'remove', 'rename', 'setbuf', 'setvbuf', 'va_start', 'va_end', 'va_arg', 'va_copy', '_vinit',
		'wyhash', 'wyhash64']
		|| name.starts_with('pthread_') || name.starts_with('__builtin_')
		|| name.starts_with('__atomic_') || name.starts_with('__sync_')
}

// fastc_target_c_extern_prototypes declares live native functions whose V file
// provides no header. Freestanding projects link those symbols themselves.
fn fastc_target_c_extern_prototypes(functions map[string]FastcFunctionSignature, sources []FastcSourceFile, used_names map[string]bool, prefs &pref.Preferences) string {
	return fastc_target_c_extern_prototypes_for_context(functions, used_names, prefs,
		fastc_native_source_context(sources, prefs))
}

fn fastc_target_c_extern_prototypes_for_context(functions map[string]FastcFunctionSignature, used_names map[string]bool, prefs &pref.Preferences, context FastcNativeSourceContext) string {
	mut exported := map[string]bool{}
	for _, signature in functions {
		if signature.export_name != '' && !signature.is_disabled {
			exported[signature.export_name] = true
		}
	}
	mut keys := functions.keys()
	keys.sort()
	mut out := strings.new_builder(256)
	for key in keys {
		if !key.starts_with('C.') {
			continue
		}
		name := key.all_after_first('C.')
		signature := functions[key]
		if !used_names[name] || !context.references[name] || signature.is_disabled || signature.is_c_extern
			|| fastc_target_libc_declares(name) {
			continue
		}
		paths := context.declaration_paths[name] or { [signature.path] }
		if !paths.any(!context.files_with_headers[it]) {
			continue
		}
		if prefs.target.os == 'vinix'
			&& name in ['text_start', 'text_end', 'rodata_start', 'rodata_end', 'data_start', 'data_end',
				'interrupt_thunks']
			&& !signature.path.replace('\\', '/').starts_with(os.join_path(prefs.vroot, 'vlib').replace('\\', '/') + '/') {
			continue
		}
		mut parameters := []string{}
		for i, typ in signature.parameter_types_text() {
			parameters << if i < signature.parameter_constness.len
				&& signature.parameter_constness[i] && !exported[name] {
				base := match typ {
					'voidptr' { 'void*' }
					'byteptr' { 'u8*' }
					'charptr' { 'char*' }
					else { typ }
				}
				'const ${base}'
			} else {
				typ
			}
		}
		if signature.is_variadic {
			parameters << '...'
		} else if parameters.len == 0 {
			parameters << 'void'
		}
		out.writeln('#ifndef ${name}')
		out.writeln('extern ${signature.return_type_text()} ${name}(${parameters.join(', ')});')
		out.writeln('#endif')
	}
	return out.str()
}
