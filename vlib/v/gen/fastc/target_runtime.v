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
	mut scan := scanner.new_scanner(prefs, .normal)
	file := token.File.unindexed(path, source.len)
	scan.init(file, source)
	mut previous := token.Token.unknown
	mut has_header := false
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .dollar {
			mut selected_scan := scan
			if selected_scan.scan() == .key_if {
				if selected := fastc_scan_selected_comptime_branch(mut selected_scan, .key_if, path, prefs) {
					if selected.source != '' {
						has_header = fastc_target_collect_native_context(selected.source, path, prefs, mut references, mut declarations) || has_header
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
			if directive.len > 0 && directive[0] in ['include', 'preinclude', 'insert'] {
				qualifier := if directive.len > 1 { directive[1] } else { '' }
				if qualifier !in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd',
					'dragonfly', 'solaris', 'android'] || pref.comptime_flag_value(prefs, qualifier) {
					has_header = true
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
	return has_header
}

struct FastcNativeSourceContext {
mut:
	references         map[string]bool
	files_with_headers map[string]bool
	declaration_paths  map[string][]string
}

fn fastc_native_source_context(sources []FastcSourceFile, prefs &pref.Preferences) FastcNativeSourceContext {
	mut context := FastcNativeSourceContext{}
	for source in sources {
		mut declarations := map[string]bool{}
		if fastc_target_collect_native_context(source.source, source.path, prefs, mut context.references, mut declarations) {
			context.files_with_headers[source.path] = true
		}
		for name, _ in declarations {
			mut paths := context.declaration_paths[name] or { []string{} }
			paths << source.path
			context.declaration_paths[name] = paths
		}
	}
	return context
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
