module driver

import os
import strings
import v.cmdexec
import v.flat
import v.gen.c as cgen
import v.modulecache
import v.pref
import v.tempname

const v3_native_bindings_marker = '#define V3CACHE_NATIVE_BINDINGS 1'

fn v3_tcc_dylib_prototypes(source string) map[string]string {
	mut prototypes := map[string]string{}
	mut item := strings.new_builder(256)
	mut depth := 0
	mut i := 0
	for i < source.len {
		ch := source[i]
		if ch == `/` {
			after := v3_c_skip_comment(source, i)
			if after > i {
				item.write_u8(` `)
				i = after
				continue
			}
		}
		if ch in [`"`, `'`] {
			after := v3_c_skip_literal(source, i)
			item.write_string(source[i..after])
			i = after
			continue
		}
		if ch == `#` {
			for i < source.len {
				if source[i] == `\n` && (i == 0 || source[i - 1] != `\\`) { break }
				i++
			}
			continue
		}
		item.write_u8(ch)
		if ch == `{` { depth++ }
		if ch == `}` { depth-- }
		if ch == `;` && depth == 0 {
			text := v3_c_tokens_text(item.str()).trim_space()
			if !text.contains('{') && !text.starts_with('typedef ') {
				// The native scanner classifies function heads, including attributes.
				for name, _ in modulecache.c_source_function_identifiers(text.trim_right(';') + ' {}') {
					prototypes[name] = text
				}
			}
			item = strings.new_builder(256)
		}
		i++
	}
	return prototypes
}

fn v3_tcc_dylib_native_closure(a &flat.FlatAst, prefs &pref.Preferences, user_flags []string, compiler string) V3NativeInputClosure {
	mut flags := user_flags.clone()
	flags << cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
	flags = v3_tcc_dylib_native_flags(flags)
	for flag in flags {
		clean := flag.trim_space()
		if clean.starts_with('@') || clean.starts_with('--config') || clean.starts_with('-specs') || clean.starts_with('--specs') || clean.starts_with('-include-pch') || clean.starts_with('-Wp,') {
			return V3NativeInputClosure{ unassignable: 'opaque native compiler options' }
		}
	}
	path := os.join_path(os.vtmp_dir(), 'v3_native_bridge_${tempname.unique_token()}.m')
	defer { os.rm(path) or {} }
	os.write_file(path, cgen.cache_native_dependency_directives(a, prefs.vroot, flags, prefs.target)) or {
		return V3NativeInputClosure{ unassignable: err.msg() }
	}
	mut args := c_object_compile_flags(flags)
	language := cache_probe_language(cgen.cache_native_inputs_language(a, prefs.vroot, flags,
		prefs.c99, prefs.ccompiler, prefs.target), flags)
	args << ['-x', if language in ['c++', 'objective-c++'] {
		'objective-c++'
	} else {
		'objective-c'
	}]
	deps := c_object_dependencies(compiler, args, path)
	if deps.used_fallback {
		if os.getenv('V3_CACHE_TRACE') != '' {
			mut probe_args := args.clone()
			probe_args << ['-M', '-MT', 'v3cache', path]
			result := cmdexec.run(compiler, probe_args)
			eprintln(result.output)
		}
		return V3NativeInputClosure{ unassignable: 'the native bridge dependency manifest is unavailable' }
	}
	if deps.files.any(it.ends_with('.mm')) && deps.files.any(it != path && it.ends_with('.m')) {
		return V3NativeInputClosure{ unassignable: 'mixed Objective-C and Objective-C++ bridge contexts' }
	}
	mut directories := []string{}
	for input in cgen.native_include_input_files(a, prefs.vroot, prefs.target, flags, prefs.compile_values) {
		if !cgen.native_path_is_shipped(input, prefs.vroot) {
			directories << os.dir(input)
		} else if input.starts_with(os.join_path(prefs.vroot, 'vlib') + os.path_separator) {
			text := os.read_file(input) or { continue }
			if text.contains('<Cocoa/') || text.contains('<AppKit/') || text.contains('__bridge')
				|| text.contains('@interface') || text.contains('@implementation') {
				directories << os.dir(input)
			}
		}
	}
	mut functions := map[string]string{}
	mut headers := map[string]string{}
	mut wanted := map[string]bool{}
	mut wanted_types := map[string]bool{}
	mut native_types := map[string]string{}
	mut native_prototypes := map[string]string{}
	mut compiler_headers := map[string]bool{}
	for input in deps.files {
		if !input.contains('/lib/clang/') || !input.contains('/include/') { continue }
		name := input.all_after_last('/include/')
		if name !in ['stdint.h', 'stddef.h', 'stdbool.h', 'stdarg.h', 'stdalign.h', 'stdatomic.h',
			'float.h', 'limits.h', 'stdnoreturn.h'] {
			compiler_headers[name] = true
		}
	}
	for node in a.nodes {
		if node.kind == .c_fn_decl { wanted[node.value.all_after_last('.')] = true }
		if node.kind == .struct_decl && node.value.starts_with('C.') {
			wanted_types[node.value[2..]] = true
		}
		for text in [node.typ_text(), if node.source_value != 0 { node.value_text() } else { '' }] {
			for part in text.split('C.')[1..] {
				end := v3_c_identifier_end(part, 0)
				if end > 0 { wanted_types[part[..end]] = true }
			}
		}
	}
	mut projected_files := map[string]bool{}
	for input in deps.files {
		if input != path && directories.any(input.starts_with(it + os.path_separator)) {
			projected_files[os.real_path(input)] = true
		}
	}
	mut preprocess_args := args.clone()
	preprocess_args << ['-E', '-dI', '-dD', path]
	preprocessed := cmdexec.run(compiler, preprocess_args)
	if preprocessed.exit_code != 0 {
		return V3NativeInputClosure{ unassignable: 'the native bridge interface cannot be preprocessed' }
	}
	active_sources := v3_tcc_dylib_preprocessed_files(preprocessed.output, projected_files)
	for input in deps.files {
		if input == path || !directories.any(input.starts_with(it + os.path_separator)) { continue }
		original := os.read_file(input) or { continue }
		if v3_tcc_dylib_replicable_inline_header(original) { continue }
		text := active_sources[os.real_path(input)] or { '' }
		prototypes := modulecache.declaration_header(v3_tcc_dylib_c_text(text).replace('static ', '').replace('inline ', '').replace('__inline__ ', '').replace('__inline ', ''))
		mut exposed := strings.new_builder(256)
		for name, prototype in v3_tcc_dylib_prototypes(prototypes) {
			if wanted[name] {
				exposed.writeln(prototype)
				functions[name] = prototype
			}
		}
		native_path := os.real_path(input)
		headers[native_path + ':frameworks'] = text.split_into_lines().filter(v3_tcc_dylib_is_framework_include(it)).join('\n') + '\n'
		native_types[native_path] = v3_tcc_dylib_without_compiler_headers(modulecache.c_source_type_declarations(v3_tcc_dylib_c_text(text)), compiler_headers)
		native_prototypes[native_path] = exposed.str()
		for name in v3_c_identifiers(native_prototypes[native_path]) { wanted_types[name] = true }
	}
	// Follow type dependencies across nested project headers. Private SDK types
	// used only by implementation helpers do not belong to the TCC interface.
	for {
		before := wanted_types.len
		for _, text in native_types { v3_tcc_dylib_exposed_types(text, mut wanted_types) }
		if wanted_types.len == before { break }
	}
	for native_path, text in native_types {
		headers[native_path] = v3_tcc_dylib_exposed_types(text, mut wanted_types) + native_prototypes[native_path]
	}
	mut native_sources := native_types.keys().filter(it.ends_with('.m') || it.ends_with('.mm'))
	native_sources.sort()
	headers['__v3_native_sources__'] = native_sources.map('#include "' + c_include_path(it) + '"').join('\n') + '\n'
	headers['__v3_objcxx_sources__'] = native_sources.filter(it.ends_with('.mm')).map('#include "' + c_include_path(it) + '"').join('\n') + '\n'
	mut classes := map[string]bool{}
	for input in deps.files {
		text := os.read_file(input) or { continue }
		for name in v3_tcc_dylib_objc_classes(text) {
			if wanted_types[name] { classes[name] = true }
		}
	}
	mut class_names := classes.keys()
	class_names.sort()
	needs_objc_types := class_names.len > 0 || (deps.files.any(it.ends_with('/objc/objc.h'))
		&& ['id', 'Class', 'SEL', 'BOOL', 'IMP', 'Protocol', 'objc_object', 'objc_class',
			'objc_selector'].any(wanted_types[it]))
	headers['__v3_objc_types__'] = if needs_objc_types {
		'#include <objc/objc.h>\n' + class_names.map('#ifdef __OBJC__\n@class ${it};\n#else\ntypedef struct ${it} ${it};\n#endif').join('\n') + '\n'
	} else {
		''
	}
	return V3NativeInputClosure{
		inputs:       {
			'__v3_c_flags__': deps.files.filter(it != path)
		}
		functions:    functions
		include_dirs: v3_parallel_c_include_dirs(flags)
		tcc_headers:  headers
	}
}

// Project the native compiler's active branches and expanded macros, retaining
// include edges so nested public typedefs keep their original header ownership.
fn v3_tcc_dylib_preprocessed_files(source string, wanted map[string]bool) map[string]string {
	mut current := ''
	mut resolved := map[string]string{}
	mut lines := map[string][]string{}
	for line in source.split_into_lines() {
		if marker := v3_preprocessor_marker_file(line) {
			current = ''
			if marker.len > 0 && !marker.starts_with('<') {
				path := resolved[marker] or {
					real := os.real_path(marker)
					resolved[marker] = real
					real
				}
				if wanted[path] { current = path }
			}
			continue
		}
		if current == '' { continue }
		mut text := line
		if arg := v3_tcc_dylib_include_argument(line) {
			// Clang appends a comment to -dI include lines; it is not part of the path.
			if arg.starts_with('"') {
				text = '#include ' + arg[..v3_c_skip_literal(arg, 0)]
			} else if arg.starts_with('<') {
				if end := arg.index('>') { text = '#include ' + arg[..end + 1] }
			}
		}
		lines[current] << text
	}
	mut result := map[string]string{}
	for path, parts in lines { result[path] = parts.join('\n') + '\n' }
	return result
}

// Objective-C imports have the same header dependencies as C includes.
fn v3_tcc_dylib_include_argument(line string) ?string {
	if arg := v3_include_directive_argument(line) { return arg }
	clean := line.trim_space()
	if !clean.starts_with('#') { return none }
	directive := clean[1..].trim_left(' \t')
	if !directive.starts_with('import') || (directive.len > 6 && (directive[6].is_letter() || directive[6] == `_`)) {
		return none
	}
	return directive[6..].trim_left(' \t')
}

fn v3_tcc_dylib_without_compiler_headers(source string, compiler_headers map[string]bool) string {
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		if arg := v3_tcc_dylib_include_argument(line) {
			if compiler_headers[arg.trim('<>"')] { continue }
		}
		out.writeln(line)
	}
	return out.str()
}

fn v3_tcc_dylib_is_framework_include(line string) bool {
	arg := v3_tcc_dylib_include_argument(line.trim_space()) or { return false }
	name := arg.trim('<>')
	return arg.starts_with('<') && name.contains('/') && name.len > 0 && name[0].is_capital()
}

fn v3_tcc_dylib_replicable_inline_header(text string) bool {
	if !(text.contains('static inline ') || text.contains('static __inline'))
		|| !text.contains('always_inline')
		|| text.contains('@interface') || text.contains('@implementation')
		|| !modulecache.c_source_is_replicable(text) {
		return false
	}
	for line in text.split_into_lines() {
		if arg := v3_tcc_dylib_include_argument(line) {
			name := arg.trim('<>')
			if arg.starts_with('<') && name.contains('/') && name[0].is_capital() { return false }
		}
	}
	return true
}

fn v3_tcc_dylib_native_flags(flags []string) []string {
	mut native := []string{cap: flags.len}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		if flag == '-D' && i + 1 < flags.len && flags[i + 1].all_before('=') == '__TINYC__' {
			i += 2
			continue
		}
		if flag.all_before('=') != '-D__TINYC__' { native << flag }
		i++
	}
	return native
}

fn v3_tcc_dylib_program_link_flags(flags []string, source string, sdk string) []string {
	mut frameworks := map[string]bool{}
	for line in source.split_into_lines() {
		if arg := v3_tcc_dylib_include_argument(line) {
			if arg.starts_with('<') && arg.contains('/') {
				frameworks[arg.trim('<>').all_before('/')] = true
			}
		}
	}
	mut program_flags := []string{}
	mut i := 0
	for i < flags.len {
		if flags[i] == '-framework' && i + 1 < flags.len {
			// Native-only frameworks belong to the owner. Repeating all of them
			// can overflow TCC's fixed Mach-O load-command area.
			if frameworks[flags[i + 1]] { program_flags << flags[i..i + 2] }
			i += 2
			continue
		}
		program_flags << flags[i]
		i++
	}
	return tcc_dynamic_link_flags(v3_tcc_macos_framework_flags(program_flags, 'macos', sdk))
}

fn v3_tcc_dylib_declarations_in(source string, tinyc bool, directory string, include_dirs []string, headers map[string]string, mut active map[string]bool) string {
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		arg := v3_tcc_dylib_include_argument(line) or {
			out.writeln(line)
			continue
		}
		if !arg.starts_with('"') {
			out.writeln(line)
			continue
		}
		name := arg.trim('"')
		mut path := if os.is_abs_path(name) || directory.len == 0 {
			name
		} else {
			os.join_path(directory, name)
		}
		if !os.is_file(path) && !os.is_abs_path(name) {
			for dir in include_dirs {
				candidate := os.join_path(dir, name)
				if os.is_file(candidate) {
					path = candidate
					break
				}
			}
		}
		if !os.is_file(path) {
			out.writeln(line)
			continue
		}
		if active[path] { continue }
		if (path.ends_with('.m') || path.ends_with('.mm')) && os.real_path(path) !in headers {
			continue
		}
		text := if os.real_path(path) in headers {
			(if tinyc { '' } else { headers[os.real_path(path) + ':frameworks'] }) + headers[os.real_path(path)]
		} else {
			os.read_file(path) or { '' }
		}
		if cgen.native_path_is_shipped(path, @VMODROOT)
			&& os.real_path(path) !in headers
			&& (text.contains('SOKOL_IMPL') || text.contains('_IMPLEMENTATION') || modulecache.c_source_is_replicable(text)) {
			out.writeln('#include "' + c_include_path(path) + '"')
			continue
		}

		if tinyc && os.real_path(path) !in headers && (text.contains('@interface') || text.contains('<Cocoa/') || text.contains('<AppKit/')) {
			for declaration in modulecache.c_source_type_declarations(v3_tcc_dylib_c_text(text)).split_into_lines() {
				if declaration.trim_space().starts_with('typedef ') && declaration.trim_space().ends_with(';') {
					out.writeln(declaration)
				}
			}
			continue
		}
		active[path] = true
		declarations := v3_tcc_dylib_header_declarations(text)
		if text.split_into_lines().any(it.trim_space() == '#pragma once')
			|| os.real_path(path) in headers {
			guard := 'V3CACHE_NATIVE_HEADER_' + c_hash_bytes(u64(1469598103934665603), os.real_path(path).bytes()).hex()
			out.writeln('#ifndef ' + guard + '\n#define ' + guard)
			out.writeln(v3_tcc_dylib_declarations_in(declarations, tinyc, os.dir(path), include_dirs, headers, mut active))
			out.writeln('#endif')
		} else {
			out.writeln(v3_tcc_dylib_declarations_in(declarations, tinyc, os.dir(path), include_dirs, headers, mut active))
		}
		active.delete(path)
	}
	return out.str()
}

// Keep class declarations intact; Objective-C implementations belong to the
// native owner and cannot be processed as C function declarations.
fn v3_tcc_dylib_header_declarations(source string) string {
	mut out := strings.new_builder(source.len)
	mut pending := strings.new_builder(source.len)
	mut objc := ''
	for line in source.split_into_lines() {
		clean := line.trim_space()
		if objc.len > 0 {
			if objc == 'interface' { out.writeln(line) }
			if clean == '@end' { objc = '' }
			continue
		}
		if clean.starts_with('@implementation') || clean.starts_with('@interface') {
			out.write_string(modulecache.declaration_header(pending.str()))
			pending = strings.new_builder(256)
			objc = if clean.starts_with('@interface') { 'interface' } else { 'implementation' }
			if objc == 'interface' { out.writeln(line) }
			continue
		}
		if clean != '#pragma once' { pending.writeln(line) }
	}
	out.write_string(modulecache.declaration_header(pending.str()))
	return out.str()
}

// Native implementations and their state stay together in the owner. Typed
// Unique pointer symbols are selected only for qualified V C-function references,
// so native prototypes, aggregate tags and local variables retain their names.
struct V3TccDylibBindings {
	owner        string
	declarations string
}

fn v3_tcc_dylib_function_bindings(functions map[string]string) V3TccDylibBindings {
	mut owner := strings.new_builder(256)
	mut out := strings.new_builder(256)
	mut names := functions.keys()
	names.sort()
	for name in names {
		line := functions[name]
		alias := 'v3_native_bridge_' + name
		mut start := 0
		for start < line.len {
			end := v3_c_identifier_end(line, start)
			mut after := end
			for after < line.len && line[after].is_space() { after++ }
			if end > start && line[start..end] == name && after < line.len && line[after] == `(` {
				break
			}
			start = if end > start { end } else { start + 1 }
		}
		prototype := line[..start] + '(*' + alias + ')' + line[start + name.len..]
		out.writeln('extern ' + prototype.trim_string_left('extern ').trim_string_left('static '))
		owner.writeln('__typeof__(&' + name + ') ' + alias + ' = &' + name + ';')
	}
	return V3TccDylibBindings{ owner: owner.str(), declarations: out.str() }
}

fn v3_tcc_dylib_c_text(source string) string {
	mut out := strings.new_builder(source.len)
	mut objc := false
	mut cpp_depth := 0
	mut skip_cpp := false
	for line in v3_tcc_dylib_without_cpp_linkage(source).split_into_lines() {
		clean := line.trim_space()
		if cpp_depth == 0 && clean in ['#ifdef __cplusplus', '#ifndef __cplusplus',
			'#if defined(__cplusplus)', '#if !defined(__cplusplus)'] {
			cpp_depth = 1
			skip_cpp = clean in ['#ifdef __cplusplus', '#if defined(__cplusplus)']
			continue
		}
		if cpp_depth > 0 {
			if clean.starts_with('#if') { cpp_depth++ }
			if clean == '#else' && cpp_depth == 1 {
				skip_cpp = !skip_cpp
				continue
			}
			if clean.starts_with('#endif') {
				cpp_depth--
				if cpp_depth == 0 {
					skip_cpp = false
					continue
				}
			}
			if skip_cpp { continue }
		}
		if clean.starts_with('@interface') || clean.starts_with('@implementation') {
			objc = true
			continue
		}
		if objc {
			if clean == '@end' { objc = false }
			continue
		}
		out.writeln(line)
	}
	return out.str()
}

fn v3_tcc_dylib_without_cpp_linkage(source string) string {
	mut out := strings.new_builder(source.len)
	mut wrappers := []int{}
	mut depth := 0
	mut i := 0
	for i < source.len {
		if source[i] == `/` {
			after := v3_c_skip_comment(source, i)
			if after > i {
				out.write_string(source[i..after])
				i = after
				continue
			}
		}
		if source[i] in [`"`, `'`] {
			after := v3_c_skip_literal(source, i)
			out.write_string(source[i..after])
			i = after
			continue
		}
		if source[i..].starts_with('extern') && (i == 0 || !(source[i - 1].is_alnum() || source[i - 1] == `_`)) {
			mut after := i + 'extern'.len
			for after < source.len && source[after].is_space() { after++ }
			if source[after..].starts_with('"C"') {
				after += 3
				for after < source.len && source[after].is_space() { after++ }
				if after < source.len && source[after] == `{` {
					depth++
					wrappers << depth
					after++
				}
				i = after
				continue
			}
		}
		if source[i] == `{` { depth++ }
		if source[i] == `}` {
			if wrappers.len > 0 && depth == wrappers.last() {
				wrappers.delete_last()
				depth--
				i++
				continue
			}
			depth--
		}
		out.write_u8(source[i])
		i++
	}
	return out.str()
}

// Preserve the original file ownership of SDK C structs. Their cached objects
// are reusable, while a combined interface cannot identify each defining C file.
fn v3_tcc_dylib_needs_native_sources(state &V3ModuleCacheState, header string) bool {
	if !state.native_bridge { return false }
	text := os.read_file(header) or { return true }
	return text.contains('struct C.') || text.contains('union C.')
}

fn v3_tcc_dylib_declarations_with_dirs(source string, tinyc bool, dirs []string, headers map[string]string) string {
	mut active := map[string]bool{}
	return (if tinyc { headers['__v3_objc_types__'] } else { '' }) + v3_tcc_dylib_declarations_in(source, tinyc, '', dirs, headers, mut active)
}

// Cached V bodies call the owner's native functions too. Preserve projected
// include positions so native typedefs precede V fields that use them.
fn v3_tcc_dylib_module_declarations(source string, state &V3ModuleCacheState, bindings &V3TccDylibBindings) string {
	positioned := source.replace(v3_native_bindings_marker, state.bridge_tcc_headers['__v3_native_sources__'] + bindings.declarations)
	return v3_tcc_dylib_declarations_with_dirs(positioned, false, state.bridge_include_dirs, state.bridge_tcc_headers)
}

fn v3_tcc_dylib_objc_classes(source string) []string {
	mut names := []string{}
	mut i := 0
	for i < source.len {
		if source[i] == `/` {
			after := v3_c_skip_comment(source, i)
			if after > i {
				i = after
				continue
			}
		}
		if source[i] in [`"`, `'`] {
			i = v3_c_skip_literal(source, i)
			continue
		}
		if source[i] != `@` {
			i++
			continue
		}
		i++
		end := v3_c_identifier_end(source, i)
		keyword := source[i..end]
		i = end
		if keyword !in ['interface', 'implementation', 'class'] { continue }
		for i < source.len {
			for i < source.len && source[i].is_space() { i++ }
			name_end := v3_c_identifier_end(source, i)
			if name_end == i { break }
			names << source[i..name_end]
			i = name_end
			if keyword != 'class' { break }
			for i < source.len && source[i].is_space() { i++ }
			if i >= source.len || source[i] != `,` { break }
			i++
		}
	}
	return names
}

fn v3_tcc_dylib_exposed_types(source string, mut wanted map[string]bool) string {
	mut out := strings.new_builder(source.len)
	mut start := 0
	mut i := 0
	mut depth := 0
	mut first_brace := -1
	mut last_brace := -1
	for i < source.len {
		ch := source[i]
		if ch == `/` {
			after := v3_c_skip_comment(source, i)
			if after > i {
				i = after
				continue
			}
		}
		if ch in [`"`, `'`] {
			i = v3_c_skip_literal(source, i)
			continue
		}
		if ch == `#` {
			if depth == 0 { out.write_string(source[start..i]) }
			mut end := i
			for end < source.len {
				if source[end] == `\n` && (end == 0 || source[end - 1] != `\\`) {
					end++
					break
				}
				end++
			}
			if depth == 0 {
				line := source[i..end]
				mut keep := true
				if arg := v3_tcc_dylib_include_argument(line.trim_space()) {
					name := arg.trim('<>')
					keep = !(arg.starts_with('<') && name.contains('/') && name[0].is_capital())
				}
				if keep { out.write_string(line) }
				start = end
			}
			i = end
			continue
		}
		if ch == `{` {
			if first_brace < 0 { first_brace = i }
			depth++
		}
		if ch == `}` {
			depth--
			last_brace = i
		}
		if ch == `;` && depth == 0 {
			declaration := source[start..i + 1]
			head := source[start..if first_brace >= start { first_brace } else { i + 1 }]
			tokens := v3_c_identifiers(head)
			// Only the declarators after an aggregate's closing brace are aliases;
			// array fields inside it must not make private types appear reachable.
			mut names := modulecache.c_source_typedef_identifiers(if tokens.len > 0
				&& tokens[0] == 'typedef' && last_brace >= start {
				'typedef int ' + source[last_brace + 1..i + 1]
			} else {
				declaration
			})
			for n, token in tokens {
				if token in ['struct', 'union', 'enum'] && n + 1 < tokens.len {
					names[tokens[n + 1]] = true
				}
			}
			if names.len == 0 || names.keys().any(wanted[it]) {
				out.write_string(declaration)
				for name in v3_c_identifiers(declaration) { wanted[name] = true }
			}
			start = i + 1
			first_brace = -1
			last_brace = -1
		}
		i++
	}
	out.write_string(source[start..])
	return out.str()
}

// Expose the framework headers read by the native compiler using ordinary include
// directories: TCC does not implement Clang's framework search syntax.
fn v3_tcc_dylib_sdk_flags(cache_dir string, inputs []string) []string {
	mut flags := ['-D__MACH__=1', '-D__has_extension(x)=0']
	mut frameworks := map[string]string{}
	for input in inputs {
		if !input.contains('.framework/') || !input.contains('/Headers/') { continue }
		headers := input.all_before('/Headers/') + '/Headers'
		root := headers.all_before_last('.framework') + '.framework'
		frameworks[os.file_name(root).all_before_last('.framework')] = headers
	}
	if frameworks.len == 0 { return flags }
	mut names := frameworks.keys()
	names.sort()
	identity := names.map(it + '=' + frameworks[it]).join('\n')
	directory := os.join_path(cache_dir, 'tcc_framework_headers_' + c_hash_bytes(u64(1469598103934665603), identity.bytes()).hex())
	os.mkdir_all(directory) or { return flags }
	for name in names {
		os.symlink(frameworks[name], os.join_path(directory, name)) or {}
	}
	flags << '-I' + directory
	return flags
}
