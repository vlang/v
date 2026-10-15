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

// fastc_target_collect_native_context tracks qualified C references separately
// from V names, and follows selected compile-time branches for included headers.
fn fastc_target_collect_native_context(source string, path string, prefs &pref.Preferences, project_headers []string, mut references map[string]bool, mut headers map[string]bool) bool {
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
						has_header = fastc_target_collect_native_context(selected.source, path, prefs, project_headers, mut references, mut headers) || has_header
					}
					scan = selected_scan
					tok = selected.tok
					previous = .unknown
					continue
				}
			}
		}
		if tok == .hash && scan.lit.trim_space().starts_with('include ') {
			has_header = true
			header := fastc_target_include_path(scan.lit.trim_space(), path, prefs, project_headers)
			if header != '' {
				headers[header] = true
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
		previous = tok
		tok = scan.scan()
	}
	return has_header
}

fn fastc_target_include_path(directive string, source_path string, prefs &pref.Preferences, project_headers []string) string {
	resolved := fastc_resolve_c_pseudo_paths(directive, prefs.vroot, source_path)
	argument := resolved['include '.len..].trim_space()
	if argument.len < 2 || argument[0] !in [`"`, `<`] {
		return ''
	}
	closing := if argument[0] == `"` { `"` } else { `>` }
	end := argument[1..].index_u8(closing)
	if end < 0 { return '' }
	name := argument[1..end + 1]
	if os.is_abs_path(name) {
		return if os.is_file(name) { os.real_path(name) } else { '' }
	}
	origin := os.real_path(source_path)
	origin_root := fastc_vmod_root_for_file(origin)
	for dir in [os.dir(origin), origin_root, os.join_path(origin_root, 'c'),
		prefs.module_resolution_root, os.join_path(prefs.module_resolution_root, 'c')] {
		candidate := os.join_path(dir, name)
		if os.is_file(candidate) {
			return os.real_path(candidate)
		}
	}
	// Prefer the include tree of this source's real project. Fallback-loaded
	// modules can also bring a different, unfiltered project's header tree.
	for root in [origin_root, ''] {
		mut matching := ''
		mut ambiguous := false
		for header in project_headers {
			if header.ends_with('/${name}') && (root == '' || header.starts_with('${root}/')) {
				if matching != '' && matching != header {
					ambiguous = true
					break
				}
				matching = header
			}
		}
		if ambiguous { return '' }
		if matching != '' { return matching }
	}
	return ''
}

// fastc_target_header_symbols recognizes declarations, not mentions of a name
// in comments, function bodies, or initializer expressions.
fn fastc_target_header_symbols(source string, path string, prefs &pref.Preferences) map[string]bool {
	mut scan := scanner.new_scanner(prefs, .normal)
	file := token.File.unindexed(path, source.len)
	scan.init(file, source)
	mut symbols := map[string]bool{}
	mut braces := 0
	mut parens := 0
	mut brackets := 0
	mut identifiers := 0
	mut last_identifier := ''
	mut has_extern := false
	mut has_assignment := false
	mut tok := scan.scan()
	for tok != .eof {
		top := braces == 0 && parens == 0 && brackets == 0
		if top && (tok == .name || tok.is_keyword()) {
			identifiers++
			last_identifier = scan.lit
			has_extern = has_extern || scan.lit == 'extern'
		}
		if tok == .lpar {
			if top && identifiers >= 2 && !has_assignment
				&& last_identifier !in ['__attribute__', '__declspec', 'typeof'] {
				symbols[last_identifier] = true
			}
			parens++
		} else if tok == .rpar {
			if parens > 0 { parens-- }
		} else if tok == .lsbr {
			if top && has_extern && last_identifier != '' {
				symbols[last_identifier] = true
			}
			brackets++
		} else if tok == .rsbr {
			if brackets > 0 { brackets-- }
		} else if tok == .lcbr {
			braces++
		} else if tok == .rcbr {
			if braces > 0 { braces-- }
		}
		if top && tok == .assign {
			has_assignment = true
		}
		if (top && tok == .semicolon) || (tok == .rcbr && braces == 0) {
			if top && has_extern && !has_assignment && last_identifier != '' {
				symbols[last_identifier] = true
			}
			identifiers = 0
			last_identifier = ''
			has_extern = false
			has_assignment = false
		}
		tok = scan.scan()
	}
	return symbols
}

// fastc_target_header_declares leaves the standard headers' exact qualifiers
// and integer widths intact rather than redeclaring their approximate V view.
fn fastc_target_header_declares(name string) bool {
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
	mut references := map[string]bool{}
	mut headers := map[string]bool{}
	project_headers := fastc_target_project_headers(sources, prefs)
	for source in sources {
		_ = fastc_target_collect_native_context(source.source, source.path, prefs, project_headers, mut references, mut headers)
	}
	mut header_symbols := map[string]bool{}
	mut macros := map[string]string{}
	mut visited := map[string]bool{}
	mut header_paths := headers.keys()
	header_paths.sort()
	for path in header_paths {
		visible := fastc_target_visible_header(path, prefs, project_headers, mut macros, mut visited, 0)
		for name, _ in fastc_target_header_symbols(visible, path, prefs) {
			header_symbols[name] = true
		}
	}
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
		if !used_names[name] || !references[name] || signature.is_disabled || signature.is_c_extern
			|| header_symbols[name] || fastc_target_header_declares(name) {
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
