module fastc

import os
import strings
import time
import v3.pref
import v3.scanner
import v3.token

fn collect_declared_types(source string, path string, module_name string, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut declaration_paths map[string]string, mut declaration_modules map[string]string, mut type_source strings.Builder, mut body_spans []int) !bool {
	mut scratch_body_spans := []int{}
	mut pending_fn := false
	mut fn_body_start := -1
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_c_struct_is_typedef := false
	mut next_enum_is_flag := false
	mut next_struct_is_params := false
	mut next_type_is_enabled := true
	mut has_type_declarations := false
	mut previous_tok := token.Token.unknown
	mut emitted_end := 0
	mut pending_start := -1
	mut declaration_start := -1
	mut declaration_has_block := false
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_types := strings.new_builder(256)
					selected_has_types := collect_declared_types(selected.source, path, module_name, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut declaration_paths, mut declaration_modules, mut selected_types, mut scratch_body_spans)!
					has_type_declarations = has_type_declarations || selected_has_types
					if selected_types.len > 0 {
						start := if pending_start >= 0 { pending_start } else { comptime_start }
						fastc_append_filtered_source_span(source, emitted_end, start, comptime_end, mut type_source)
						emitted_end = comptime_end
					}
				}
				pending_start = -1
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if brace_depth == 0 && tok == .attribute {
			if pending_start < 0 {
				pending_start = scan.pos
			}
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_c_struct_is_typedef = next_c_struct_is_typedef || attribute.is_typedef
			next_enum_is_flag = next_enum_is_flag || attribute.is_flag
			next_struct_is_params = next_struct_is_params || attribute.is_params
			next_type_is_enabled = next_type_is_enabled && attribute.is_enabled
			continue
		}
		if brace_depth == 0 && tok in [.key_pub, .key_static] && pending_start < 0 {
			pending_start = scan.pos
		}
		if brace_depth == 0 && tok in [.key_fn, .key_const, .key_global] {
			pending_fn = tok == .key_fn
			next_c_struct_is_typedef = false
			next_enum_is_flag = false
			next_struct_is_params = false
			next_type_is_enabled = true
			pending_start = -1
			if tok == .key_fn && declaration_start < 0 {
				// A method whose name is a keyword (`fn (e Expr) type() Type`) tokenizes
				// that name as e.g. `.key_type`. Skip the whole function header so the
				// keyword name does not open a bogus type-declaration span here. The
				// `declaration_start < 0` guard keeps the `fn` in a `type X = fn(...)`
				// alias value (which has no receiver or name) out of this path.
				previous_tok = tok
				tok = fastc_skip_function_header(mut scan)!
				continue
			}
		}
		if brace_depth == 0 && tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union] {
			has_type_declarations = true
			declaration_start = if pending_start >= 0 { pending_start } else { scan.pos }
			declaration_has_block = false
			pending_start = -1
			is_public := previous_tok == .key_pub
			kind := match tok {
				.key_enum { FastcDeclaredTypeKind.enum_ }
				.key_interface { FastcDeclaredTypeKind.interface_ }
				.key_type { FastcDeclaredTypeKind.alias_ }
				.key_union { FastcDeclaredTypeKind.union_ }
				else { FastcDeclaredTypeKind.struct_ }
			}
			tok = scan.scan()
			if tok == .name && next_type_is_enabled {
				name := scan.lit
				tok = scan.scan()
				if name == 'C' && tok == .dot {
					tok = scan.scan()
					if tok == .name && !next_c_struct_is_typedef {
						key := '#Cstruct#${scan.lit}'
						declared_types[key] = true
						declaration_paths[key] = path
						declaration_modules[key] = module_name
					}
					next_c_struct_is_typedef = false
					next_enum_is_flag = false
					next_struct_is_params = false
					continue
				}
				key := fastc_type_key(module_name, name)
				if key in declared_types {
					return error('fastc parser does not support duplicate type declaration `${name}` in module `${module_name}` in ${path}')
				}
				declared_types[key] = is_public
				declared_kinds[key] = kind
				declaration_paths[key] = path
				declaration_modules[key] = module_name
				if kind == .enum_ && next_enum_is_flag {
					enum_flags[key] = true
				}
				if kind == .struct_ && next_struct_is_params {
					params_structs[fastc_c_declared_type_name(key)] = true
				}
			}
			next_c_struct_is_typedef = false
			next_enum_is_flag = false
			next_struct_is_params = false
			next_type_is_enabled = true
			continue
		}
		if tok == .lcbr {
			if brace_depth == 0 && pending_fn && fn_body_start < 0 {
				fn_body_start = scan.pos
			}
			brace_depth++
			if declaration_start >= 0 {
				declaration_has_block = true
			}
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
			if brace_depth == 0 && fn_body_start >= 0 {
				body_spans << fn_body_start
				body_spans << scan.offset
				fn_body_start = -1
				pending_fn = false
			}
			if declaration_start >= 0 && declaration_has_block && brace_depth == 0 {
				fastc_append_filtered_source_span(source, emitted_end, declaration_start, scan.offset, mut type_source)
				emitted_end = scan.offset
				declaration_start = -1
				declaration_has_block = false
			}
		} else if tok == .semicolon && brace_depth == 0 && declaration_start >= 0 && !declaration_has_block {
			// A multi-line sum type (`type Expr = A\n\t| B\n\t| C`) has an inserted
			// semicolon after each variant. Do not close the declaration span while a
			// `|` continuation follows, or only the first variant would be captured and
			// the type would never register as a sum type.
			mut lookahead := scan
			if lookahead.scan() == .pipe {
				previous_tok = tok
				tok = scan.scan()
				continue
			}
			declaration_end := if scan.offset > scan.pos { scan.offset } else { scan.pos }
			fastc_append_filtered_source_span(source, emitted_end, declaration_start, declaration_end, mut type_source)
			emitted_end = declaration_end
			declaration_start = -1
		}
		if tok == .semicolon && brace_depth == 0 {
			pending_fn = false
		}
		previous_tok = tok
		tok = scan.scan()
	}
	if declaration_start >= 0 {
		fastc_append_filtered_source_span(source, emitted_end, declaration_start, source.len, mut type_source)
	}
	return has_type_declarations
}

fn collect_constant_names(source string, path string, module_name string, prefs &pref.Preferences, skips []int, mut constants map[string]string, mut public_constants map[string]bool, mut constant_paths map[string]string, mut constant_source strings.Builder, mut constant_spans []int) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut previous_tok := token.Token.unknown
	mut emitted_end := 0
	mut skip_index := 0
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_constants := strings.new_builder(256)
					mut selected_spans := []int{}
					collect_constant_names(selected.source, path, module_name, prefs, []int{}, mut constants, mut public_constants, mut constant_paths, mut selected_constants, mut selected_spans)!
					if selected_constants.len > 0 {
						constant_spans << constant_source.len

						fastc_append_filtered_source_span(source, emitted_end, comptime_start, comptime_end, mut constant_source)

						constant_spans << constant_source.len
						emitted_end = comptime_end
					}
				}
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if brace_depth == 0 && tok == .key_const {
			declaration_start := scan.pos
			is_public := previous_tok == .key_pub
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut at_declaration_start := true
				mut nested_depth := 0
				for tok != .eof {
					if nested_depth == 0 && tok == .rpar {
						constant_spans << constant_source.len

						fastc_append_filtered_source_span(source, emitted_end, declaration_start, scan.offset, mut constant_source)

						constant_spans << constant_source.len
						emitted_end = scan.offset
						tok = scan.scan()
						break
					}
					if nested_depth == 0 && tok == .semicolon {
						at_declaration_start = true
						tok = scan.scan()
						continue
					}
					if nested_depth == 0 && at_declaration_start && tok == .name {
						// `const C.name type` declares an external C constant; the C
						// headers already provide the symbol, so FastC does not track it.
						if scan.lit != 'C' {
							fastc_register_constant(module_name, scan.lit, is_public, path, mut constants, mut public_constants, mut constant_paths)!
						}
						at_declaration_start = false
					}
					if tok in [.lpar, .lsbr, .lcbr] {
						nested_depth++
					} else if tok in [.rpar, .rsbr, .rcbr] && nested_depth > 0 {
						nested_depth--
					}
					tok = scan.scan()
				}
				previous_tok = .unknown
				continue
			}
			if tok != .name {
				return error('fastc parser does not support constant declaration in ${path}')
			}
			// `const C.name type` declares an external C constant; the C headers
			// already provide the symbol, so FastC does not track it.
			if scan.lit != 'C' {
				fastc_register_constant(module_name, scan.lit, is_public, path, mut constants, mut public_constants, mut constant_paths)!
			}
			tok = scan.scan()
			mut nested_depth := 0
			mut emitted := false
			for tok != .eof {
				if nested_depth == 0 && tok == .semicolon {
					declaration_end := if scan.offset > scan.pos { scan.offset } else { scan.pos }
					constant_spans << constant_source.len

					fastc_append_filtered_source_span(source, emitted_end, declaration_start, declaration_end, mut constant_source)

					constant_spans << constant_source.len
					emitted_end = declaration_end
					emitted = true
					tok = scan.scan()
					break
				}
				if tok in [.lpar, .lsbr, .lcbr] {
					nested_depth++
				} else if tok in [.rpar, .rsbr, .rcbr] && nested_depth > 0 {
					nested_depth--
				}
				tok = scan.scan()
			}
			if tok == .eof && !emitted {
				constant_spans << constant_source.len

				fastc_append_filtered_source_span(source, emitted_end, declaration_start, source.len, mut constant_source)

				constant_spans << constant_source.len
				emitted_end = source.len
			}
			previous_tok = .unknown
			continue
		}
		if tok == .lcbr && brace_depth == 0 {
			skipped, next_skip := fastc_skip_recorded_body(mut scan, skips, skip_index)
			skip_index = next_skip
			if skipped {
				previous_tok = .rcbr
				tok = scan.scan()
				continue
			}
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		previous_tok = tok
		tok = scan.scan()
	}
}

// fastc_append_filtered_source_span preserves declaration pseudo-variable
// locations while omitting the tokens between declarations.
fn fastc_append_filtered_source_span(source string, previous_end int, start int, end int, mut out strings.Builder) {
	mut last_newline := -1
	for i in previous_end .. start {
		if source[i] == `\n` {
			out.write_u8(`\n`)
			last_newline = i
		}
	}
	column_start := if last_newline >= 0 { last_newline + 1 } else { previous_end }
	out.write_repeated_rune(` `, start - column_start)
	out.write_string(source[start..end])
}

fn fastc_register_constant(module_name string, name string, is_public bool, path string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_paths map[string]string) ! {
	key := fastc_constant_key(module_name, name)
	if key in constants {
		return error('fastc parser does not support duplicate constant `${name}` in ${path}')
	}
	constants[key] = fastc_c_constant_name(module_name, name)
	constant_paths[key] = path
	if is_public {
		public_constants[key] = true
	}
}

// collect_global_names registers a file's globals and appends the text of
// their declarations (and of the comptime blocks that select them) to
// `global_source`, so the global phase parses only that text.
fn collect_global_names(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, mut globals map[string]string, mut public_globals map[string]bool, mut global_paths map[string]string, mut global_source strings.Builder) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut depth := 0
	mut previous_tok := token.Token.unknown
	mut previous_pos := 0
	mut emitted_end := 0
	// A single `__global name = value` declaration runs to the next top-level
	// statement end; its span is closed there.
	mut pending_start := -1
	mut skip_index := 0
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				globals_before := globals.len
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_source := strings.new_builder(64)
					collect_global_names(selected.source, path, header, prefs, []int{}, mut globals, mut public_globals, mut global_paths, mut selected_source)!
				}
				if globals.len > globals_before {
					fastc_append_filtered_source_span(source, emitted_end, comptime_start, comptime_end, mut global_source)
					emitted_end = comptime_end
				}
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if depth == 0 && tok == .key_global {
			is_public := previous_tok == .key_pub
			declaration_start := if is_public { previous_pos } else { scan.pos }
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut at_start := true
				for tok != .rpar && tok != .eof {
					if tok == .semicolon {
						at_start = true
					} else if at_start && tok == .name {
						if scan.lit != 'C' {
							fastc_register_global(header, scan.lit, is_public, path, prefs, mut globals, mut public_globals, mut global_paths)!
						}
						at_start = false
					}
					tok = scan.scan()
				}
				declaration_end := if tok == .rpar { scan.offset } else { scan.pos }
				fastc_append_filtered_source_span(source, emitted_end, declaration_start, declaration_end, mut global_source)
				emitted_end = declaration_end
				continue
			}
			if tok == .name && scan.lit != 'C' {
				fastc_register_global(header, scan.lit, is_public, path, prefs, mut globals, mut public_globals, mut global_paths)!
			}
			pending_start = declaration_start
			continue
		}
		if pending_start >= 0 && depth == 0 && tok == .semicolon {
			fastc_append_filtered_source_span(source, emitted_end, pending_start, scan.pos, mut global_source)
			emitted_end = scan.pos
			pending_start = -1
		}
		if tok == .lcbr && depth == 0 {
			skipped, next_skip := fastc_skip_recorded_body(mut scan, skips, skip_index)
			skip_index = next_skip
			if skipped {
				previous_tok = .rcbr
				previous_pos = scan.pos
				tok = scan.scan()
				continue
			}
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr && depth > 0 {
			depth--
		}
		previous_tok = tok
		previous_pos = scan.pos
		tok = scan.scan()
	}
	if pending_start >= 0 {
		fastc_append_filtered_source_span(source, emitted_end, pending_start, source.len, mut global_source)
	}
}

fn fastc_register_global(header FastcSourceHeader, name string, is_public bool, path string, prefs &pref.Preferences, mut globals map[string]string, mut public_globals map[string]bool, mut global_paths map[string]string) ! {
	if !prefs.enable_globals && !prefs.building_v && header.module_name != 'builtin' && !header.has_globals {
		return error('use `v -enable-globals ...` to enable globals in ${path}')
	}
	key := fastc_global_key(header.module_name, name)
	if key in globals {
		return error('fastc parser does not support duplicate global `${name}` in ${path}')
	}
	globals[key] = fastc_c_global_name(key)
	global_paths[key] = path
	if is_public {
		public_globals[key] = true
	}
}

struct FastcDeclarationPartial {
mut:
	declared_types      map[string]bool
	declared_kinds      map[string]FastcDeclaredTypeKind
	enum_flags          map[string]bool
	params_structs      map[string]bool
	type_source_paths   map[string]bool
	type_sources        map[string]string
	declaration_paths   map[string]string
	declaration_modules map[string]string
	constants           map[string]string
	public_constants    map[string]bool
	constant_paths      map[string]string
	constant_sources    map[string]string
	constant_spans      map[string][]int
	global_sources      map[string]string
	body_spans          map[string][]int
	globals             map[string]string
	public_globals      map[string]bool
	global_paths        map[string]string
	failed              bool
	error_message       string
}

fn fastc_collect_declaration_chunk(sources []FastcSourceFile, prefs &pref.Preferences, start int, end int) FastcDeclarationPartial {
	mut partial := FastcDeclarationPartial{
		declared_types: map[string]bool{}
		declared_kinds: map[string]FastcDeclaredTypeKind{}
		enum_flags: map[string]bool{}
		params_structs: map[string]bool{}
		type_source_paths: map[string]bool{}
		type_sources: map[string]string{}
		declaration_paths: map[string]string{}
		declaration_modules: map[string]string{}
		constants: map[string]string{}
		public_constants: map[string]bool{}
		constant_paths: map[string]string{}
		constant_sources: map[string]string{}
		globals: map[string]string{}
		public_globals: map[string]bool{}
		global_paths: map[string]string{}
	}
	for idx in start .. end {
		source_file := sources[idx]
		mut type_source := strings.new_builder(256)
		mut has_type_declarations := false
		mut body_spans := []int{}
		if source_file.header.has_type_keywords {
			has_type_declarations = collect_declared_types(source_file.source, source_file.path, source_file.header.module_name, prefs, mut partial.declared_types, mut partial.declared_kinds, mut partial.enum_flags, mut partial.params_structs, mut partial.declaration_paths, mut partial.declaration_modules, mut type_source, mut body_spans) or {
				partial.failed = true
				partial.error_message = err.msg()
				return partial
			}
		}
		if has_type_declarations {
			partial.type_source_paths[source_file.path] = true
			partial.type_sources[source_file.path] = type_source.str()
		}
		if source_file.header.has_constants {
			mut constant_source := strings.new_builder(256)
			mut constant_spans := []int{}
			collect_constant_names(source_file.source, source_file.path, source_file.header.module_name, prefs, body_spans, mut partial.constants, mut partial.public_constants, mut partial.constant_paths, mut constant_source, mut constant_spans) or {
				partial.failed = true
				partial.error_message = err.msg()
				return partial
			}
			if constant_source.len > 0 {
				partial.constant_sources[source_file.path] = constant_source.str()
				partial.constant_spans[source_file.path] = constant_spans
			}
		}
		if source_file.header.has_global_declarations {
			mut global_source := strings.new_builder(256)
			collect_global_names(source_file.source, source_file.path, source_file.header, prefs, body_spans, mut partial.globals, mut partial.public_globals, mut partial.global_paths, mut global_source) or {
				partial.failed = true
				partial.error_message = err.msg()
				return partial
			}
			if global_source.len > 0 {
				partial.global_sources[source_file.path] = global_source.str()
			}
		}
		if body_spans.len > 0 {
			partial.body_spans[source_file.path] = body_spans
		}
	}
	return partial
}

fn fastc_merge_declaration_partial(partial FastcDeclarationPartial, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut global_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) ! {
	if partial.failed {
		return error(partial.error_message)
	}
	for key, is_public in partial.declared_types {
		if key in declared_types && !key.starts_with('#Cstruct#') {
			return error('fastc parser does not support duplicate type declaration `${key.all_after_last('.')}` in module `${partial.declaration_modules[key]}` in ${partial.declaration_paths[key]}')
		}
		declared_types[key] = is_public
	}
	for key, kind in partial.declared_kinds {
		declared_kinds[key] = kind
	}
	for key, _ in partial.enum_flags {
		enum_flags[key] = true
	}
	for key, _ in partial.params_structs {
		params_structs[key] = true
	}
	for path, _ in partial.type_source_paths {
		type_source_paths[path] = true
	}
	for path, source in partial.type_sources {
		type_sources[path] = source
	}
	for key, c_name in partial.constants {
		if key in constants {
			return error('fastc parser does not support duplicate constant `${key.all_after_last('.')}` in ${partial.constant_paths[key]}')
		}
		constants[key] = c_name
	}
	for key, _ in partial.public_constants {
		public_constants[key] = true
	}
	for path, source in partial.constant_sources {
		constant_sources[path] = source
	}
	for path, spans in partial.constant_spans {
		constant_spans[path] = spans
	}
	for path, source in partial.global_sources {
		global_sources[path] = source
	}
	for key, c_name in partial.globals {
		if key in globals {
			return error('fastc parser does not support duplicate global `${key.all_after_last('.')}` in ${partial.global_paths[key]}')
		}
		globals[key] = c_name
	}
	for key, _ in partial.public_globals {
		public_globals[key] = true
	}
}

fn fastc_generate_global_declarations(ordered_sources []FastcSourceFile, global_sources map[string]string, prefs &pref.Preferences, header_free bool, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types map[string]string, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, constant_values map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, mut global_types map[string]string) !FastcGlobalDeclarations {
	declared_type_key_by_name := fastc_declared_type_key_by_name(declared_types)
	mut out := strings.new_builder(1024)
	mut module_initializers := map[string]string{}
	mut composite_types := map[string]bool{}
	mut fixed_array_types := map[string]string{}
	helper_has_c_functions := fastc_functions_declare_c(functions)
	for source_file in ordered_sources {
		global_source := global_sources[source_file.path] or { continue }
		mut initializers := strings.new_builder(256)
		file := token.File.unindexed(source_file.path, global_source.len)
		mut gen := Parser{
			prefs: unsafe { prefs }
			unqualified_key_memo: map[string]string{}
			c_function_name_memo: map[string]string{}
			nonlocal_name_type_memo: map[string]string{}
			resolved_name_memo: map[string]string{}
			declared_type_key_memo: map[string]FastcMemoEntry{}
			path: source_file.path
			module_name: source_file.header.module_name
			imports: source_file.header.imports
			declared_types: declared_types
			declared_type_c_names: declared_type_c_names
			declared_type_key_by_name: declared_type_key_by_name
			fastc_prefixed_c_names: fastc_prefixed_c_names
			declaration_initializer_mode: true
			has_c_functions: helper_has_c_functions
			comparison_memo: map[i64]FastcRenderedExpression{}
			type_memo: map[i64]string{}
			method_key_memo: map[string]map[string]string{}
			field_memo: map[string]map[string]FastcStructField{}
			spawn_typedefs: map[string]string{}
			spawn_helpers: map[string]string{}
			thread_value_types: map[string]string{}
			declared_kinds: declared_kinds
			enum_flags: enum_flags
			enum_field_types: enum_field_types
			alias_base_types: alias_base_types
			struct_fields: struct_fields
			struct_field_info: struct_field_info
			constants: constants
			constant_values: constant_values
			public_constants: public_constants
			globals: globals
			public_globals: public_globals
			selfhost: prefs.building_v
			header_free: header_free
			s: scanner.new_scanner(prefs, .normal)
			out: strings.new_builder(0)
			protos: strings.new_builder(0)
			functions: functions
			constant_types: constant_types
			global_types: global_types
			composite_types: map[string]bool{}
			fixed_array_types: map[string]string{}
		}
		gen.s.init(file, global_source)
		gen.next()
		gen.parse_selected_global_declarations(mut out, mut initializers, false)!
		global_types = gen.global_types.move()
		for name, _ in gen.composite_types {
			composite_types[name] = true
		}
		for name, array_type in gen.fixed_array_types {
			fixed_array_types[name] = array_type
		}
		if initializers.len > 0 {
			previous := module_initializers[source_file.header.module_name] or { '' }
			module_initializers[source_file.header.module_name] = previous + initializers.str()
		}
	}
	if out.len > 0 {
		out.writeln('')
	}
	return FastcGlobalDeclarations{
		declarations: out.str()
		module_initializers: module_initializers
		composite_types: composite_types
		fixed_array_types: fixed_array_types
	}
}

fn (mut g Parser) parse_selected_global_declarations(mut out strings.Builder, mut initializers strings.Builder, stop_at_block_end bool) ! {
	for g.tok != .eof {
		g.skip_semicolons()
		if stop_at_block_end && g.tok == .rcbr {
			g.next()
			g.skip_semicolons()
			return
		}
		if g.tok == .eof {
			break
		}
		if g.tok == .key_const {
			// Skip constant declarations wholesale: their initializer may contain a
			// `$d(...)`/`$if` that this pass would otherwise mistake for a top-level
			// comptime global block. Constants are handled by their own pass.
			g.skip_top_level_declaration()!
			continue
		}
		if g.tok == .dollar {
			g.parse_selected_comptime_global_declarations(mut out, mut initializers)!
			continue
		}
		if g.tok == .key_global {
			g.parse_global_declaration(mut out, mut initializers)!
			continue
		}
		if g.tok == .lcbr {
			g.skip_balanced(.lcbr, .rcbr)!
			continue
		}
		g.next()
	}
	if stop_at_block_end {
		return g.unsupported('unfinished top-level compile-time global block')
	}
}

fn (mut g Parser) parse_selected_comptime_global_declarations(mut out strings.Builder, mut initializers strings.Builder) ! {
	g.expect(.dollar)!

	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		g.parse_selected_global_declarations(mut out, mut initializers, true)!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
		} else {
			g.parse_selected_comptime_global_declarations(mut out, mut initializers)!
		}
		return
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
	} else {
		g.parse_selected_global_declarations(mut out, mut initializers, true)!
	}
}

// fastc_prealloc_enabled reports whether the arena allocator is active for this
// build (`-prealloc`, or the driver's building_v default). It gates the
// per-thread arena root so a serial-runtime FastC build is unaffected.
fn fastc_prealloc_enabled(prefs &pref.Preferences) bool {
	return 'prealloc' in prefs.user_defines
}

// fastc_write_prealloc_tls_global emits the arena root `g_memory_block` as
// per-thread storage, so the parallel per-file generator's worker threads each
// bump-allocate from their own chunk instead of racing one shared global. This
// mirrors gen/c's `write_prealloc_tls_global`: bundled TinyCC on macOS has no
// working thread-local storage, so there the identifier is redirected to a
// pthread-key slot; every other target uses `_Thread_local`.
fn fastc_write_prealloc_tls_global(mut out strings.Builder, styp string, c_name string, header_free bool) {
	out.writeln('#if defined(__TINYC__) && defined(__APPLE__)')
	if !header_free {
		// A header-free build declares the pthread API in its prelude.
		out.writeln('#include <pthread.h>')
	}
	out.writeln('static pthread_key_t v_prealloc_tls_key;')
	out.writeln('static pthread_once_t v_prealloc_tls_once = PTHREAD_ONCE_INIT;')
	out.writeln('static void v_prealloc_tls_slot_free(void *slot) { free(slot); }')
	out.writeln('static void v_prealloc_tls_key_init(void) { pthread_key_create(&v_prealloc_tls_key, v_prealloc_tls_slot_free); }')
	out.writeln('static void **v_prealloc_tls_slot(void) {')
	out.writeln('\tpthread_once(&v_prealloc_tls_once, v_prealloc_tls_key_init);')
	out.writeln('\tvoid **slot = (void **)pthread_getspecific(v_prealloc_tls_key);')
	out.writeln('\tif (slot == ((void *)0)) {')
	out.writeln('\t\tslot = (void **)calloc(1, sizeof(void *));')
	out.writeln('\t\tpthread_setspecific(v_prealloc_tls_key, slot);')
	out.writeln('\t}')
	out.writeln('\treturn slot;')
	out.writeln('}')
	out.writeln('#define ${c_name} (*(${styp} *)v_prealloc_tls_slot())')
	out.writeln('#elif defined(__cplusplus)')
	out.writeln('static thread_local ${styp} ${c_name};')
	out.writeln('#else')
	out.writeln('static _Thread_local ${styp} ${c_name};')
	out.writeln('#endif')
}

fn (mut g Parser) parse_global_declaration(mut out strings.Builder, mut initializers strings.Builder) ! {
	g.expect(.key_global)!
	if g.tok == .lpar {
		return g.unsupported('grouped globals')
	}
	if g.tok != .name {
		return g.unsupported('global name')
	}
	if g.lit == 'C' {
		g.skip_top_level_declaration()!
		return
	}
	name := g.lit
	key := fastc_global_key(g.module_name, name)
	c_name := g.globals[key] or { return g.unsupported('unregistered global `${name}`') }
	g.next()
	if g.tok == .assign {
		g.next()
		if g.tok == .lsbr {
			g.next()
			size := g.read_expression([token.Token.rsbr, token.Token.comma])!
			if g.tok == .comma {
				return g.unsupported('global array literal')
			}
			g.expect(.rsbr)!
			element_type := g.parse_type()!
			g.expect(.lcbr)!
			g.skip_open_block()!
			out.writeln('static ${element_type} ${c_name}[${size}];')
			g.global_types[key] = 'FixedArray_${fastc_composite_type_part(element_type)}'
			return
		}
		initializer := g.read_expression([token.Token.semicolon])!
		typ := fastc_normalize_inferred_type(g.last_expression_type)
		if typ == '' {
			return g.unsupported('unverifiable global `${name}` type')
		}
		out.writeln('static ${typ} ${c_name};')
		if !(g.selfhost && key in ['g_main_argc', 'g_main_argv']) {
			initializers.writeln('\t${c_name} = ${initializer};')
		}
		g.global_types[key] = typ
		g.skip_semicolons()
		return
	}
	typ := g.parse_type()!
	if g.selfhost && name == 'g_memory_block' && fastc_prealloc_enabled(g.prefs) {
		// The prealloc arena root must be per-thread; a shared global would be
		// corrupted by the parallel per-file generator's concurrent bump-allocations.
		fastc_write_prealloc_tls_global(mut out, typ, c_name, g.header_free)
	} else {
		out.writeln('static ${typ} ${c_name};')
	}
	g.global_types[key] = typ
	if g.tok == .assign {
		g.next()
		initializer := g.read_expression([token.Token.semicolon])!
		if !(g.selfhost && key in ['g_main_argc', 'g_main_argv']) {
			initializers.writeln('\t${c_name} = ${initializer};')
		}
	}
	g.skip_semicolons()
}

// fastc_map_field_default renders the empty-`map[K]V` value used to seed a map
// struct field's default (matching `map[K]V{}`), or '' when the type is not a
// resolvable map. Kept as a helper so the caller avoids a multi-return option in
// an `if` guard, which the FastC self-host does not accept.
fn fastc_map_field_default(typ string, pointer_bits int, declared_kinds map[string]FastcDeclaredTypeKind) string {
	key_type, value_type := fastc_map_key_value_types(typ) or {
		fastc_declared_map_key_value_types(typ, declared_kinds) or { return '' }
	}
	hash_fn, eq_fn, clone_fn, free_fn := fastc_map_runtime_functions(key_type, pointer_bits)
	return '(builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
}

// FastcFieldDefaultsResult is the struct field table with its defaults
// rendered and its by-name index, or the first error rendering them.
struct FastcFieldDefaultsResult {
	struct_field_info map[string][]FastcStructField
	lookup            map[string]map[string]FastcStructField
	failed            bool
	error_message     string
}

// fastc_run_field_defaults renders the field defaults into a copy of the
// field table (the renderer replaces each type's entry) and indexes it.
fn fastc_run_field_defaults(source_imports map[string]map[string]string, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types map[string]string, enum_field_names map[string][]string, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, global_types map[string]string, sum_types map[string]bool) FastcFieldDefaultsResult {
	mut rendered := struct_field_info.clone()
	fastc_render_struct_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, enum_field_names, alias_base_types, struct_fields, mut rendered, functions, constants, public_constants, constant_types, globals, public_globals, global_types, sum_types) or {
		return FastcFieldDefaultsResult{
			failed: true
			error_message: err.msg()
		}
	}
	return FastcFieldDefaultsResult{
		struct_field_info: rendered
		lookup: fastc_build_struct_field_lookup(rendered)
	}
}

fn fastc_render_struct_field_defaults(source_imports map[string]map[string]string, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types map[string]string, enum_field_names map[string][]string, alias_base_types map[string]string, struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, global_types map[string]string, sum_types map[string]bool) ! {
	declared_type_key_by_name := fastc_declared_type_key_by_name(declared_types)
	helper_has_c_functions := fastc_functions_declare_c(functions)
	mut type_names := struct_field_info.keys()
	type_names.sort()
	// Seed every implicit container default before rendering explicit field defaults. An
	// explicit default can construct another struct (`&UsedFeatures{}` in ast.Table), and
	// that nested literal must already know that its map/array fields need real runtime
	// headers rather than an all-zero C value.
	for type_name in type_names {
		mut fields := struct_field_info[type_name].clone()
		for mut field in fields {
			if field.default_source != '' {
				continue
			}
			field_layout := field.typ.trim_right('*')
			container_type := fastc_underlying_alias_type(field.typ, alias_base_types)
			container_layout := container_type.trim_right('*')
			if prefs.building_v && container_layout.starts_with('Array_') {
				if element_type := fastc_array_element_type(container_type) {
					element_sizeof := if element_type.ends_with('_ptr') {
						'voidptr'
					} else {
						element_type
					}
					field.default_value = '((${field_layout})builtin____new_array(0, 0, sizeof(${element_sizeof})))'
				}
			} else if prefs.building_v && container_layout.starts_with('Map_') {
				map_default := fastc_map_field_default(container_type, prefs.target.pointer_bits, declared_kinds)
				if map_default != '' {
					field.default_value = map_default
				}
			}
		}
		struct_field_info[type_name] = fields
	}
	for type_name in type_names {
		mut fields := struct_field_info[type_name].clone()
		for mut field in fields {
			if field.default_source == '' {
				continue
			}
			default_path := '${field.path}:${field.name}:default'
			file := token.File.unindexed(default_path, field.default_source.len)
			mut gen := Parser{
				prefs: unsafe { prefs }
				unqualified_key_memo: map[string]string{}
				c_function_name_memo: map[string]string{}
				nonlocal_name_type_memo: map[string]string{}
				resolved_name_memo: map[string]string{}
				declared_type_key_memo: map[string]FastcMemoEntry{}
				path: default_path
				module_name: field.module_name
				imports: source_imports[field.path] or {
					map[string]string{}
				}
				declared_types: declared_types
				declared_type_c_names: declared_type_c_names
				declared_type_key_by_name: declared_type_key_by_name
				fastc_prefixed_c_names: fastc_prefixed_c_names
				declaration_initializer_mode: true
				has_c_functions: helper_has_c_functions
				comparison_memo: map[i64]FastcRenderedExpression{}
				type_memo: map[i64]string{}
				method_key_memo: map[string]map[string]string{}
				field_memo: map[string]map[string]FastcStructField{}
				spawn_typedefs: map[string]string{}
				spawn_helpers: map[string]string{}
				thread_value_types: map[string]string{}
				declared_kinds: declared_kinds
				enum_flags: enum_flags
				enum_field_types: enum_field_types
				enum_field_names: enum_field_names
				alias_base_types: alias_base_types
				struct_fields: struct_fields
				struct_field_info: struct_field_info
				sum_types: sum_types
				constants: constants
				public_constants: public_constants
				globals: globals
				public_globals: public_globals
				selfhost: prefs.building_v
				s: scanner.new_scanner(prefs, .normal)
				out: strings.new_builder(0)
				protos: strings.new_builder(0)
				functions: functions
				constant_types: constant_types
				global_types: global_types
				fixed_array_types: map[string]string{}
				composite_types: map[string]bool{}
			}
			gen.s.init(file, field.default_source)
			gen.next()
			gen.expected_expression_type = field.typ
			field.default_value = gen.read_expression([token.Token.semicolon, token.Token.eof])!
			// A flag-enum default mixing a value with a `.member` shorthand (`~Show.zero() ^
			// .name`) can lose the field type mid-stream (the `Show.zero()` call clears it), so
			// resolve any surviving value-position shorthands against the field's enum type here.
			if prefs.building_v && declared_kinds[gen.semantic_type_key(field.typ)] == .enum_ {
				enum_c := field.typ.trim_right('*')
				if members := enum_field_names[enum_c] {
					field.default_value = fastc_resolve_enum_shorthands_in_source(field.default_value, enum_c, members)
				}
				// `show Show = ~Show.zero()`: lower the flag enum's magic zero()/all() statics.
				field.default_value = gen.fastc_resolve_flag_enum_statics(field.default_value, enum_c)
			}
			// A variant literal (`value Primitive = Null{}`) defaulting a sum-type or
			// interface field must be boxed into the field's representation, mirroring
			// the assignment/argument paths; a bare `(Variant){}` is not assignable to a
			// boxed `{_object,_typ,_methods}` field.
			default_actual_type := gen.last_expression_type
			if gen.should_box_variant(field.typ, default_actual_type) {
				field.default_value = gen.interface_value_expression(field.typ, default_actual_type, field.default_value)
			}
			if gen.tok == .semicolon {
				gen.next()
			}
			if gen.tok != .eof {
				return error('fastc parser does not support trailing tokens in default for `${type_name}.${field.name}` in ${field.path}')
			}
			if gen.s.diagnostics.len > 0 {
				diagnostic := gen.s.diagnostics[0]
				return error('fastc scanner error at byte ${diagnostic.offset} in ${field.path}: ${diagnostic.message}')
			}
		}
		struct_field_info[type_name] = fields
	}
}

// FastcConstantGenContext bundles the read-only program tables every
// constant-initializer Parser starts from, so parallel workers share them.
struct FastcConstantGenContext {
	prefs                     &pref.Preferences = unsafe { nil }
	declared_types            map[string]bool
	declared_type_c_names     map[string]string
	declared_type_key_by_name map[string]string
	fastc_prefixed_c_names    []string
	has_c_functions           bool
	declared_kinds            map[string]FastcDeclaredTypeKind
	enum_flags                map[string]bool
	enum_field_types          map[string]string
	alias_base_types          map[string]string
	struct_fields             map[string]map[string]string
	struct_field_info         map[string][]FastcStructField
	sum_types                 map[string]bool
	sum_type_variants         map[string]bool
	functions                 map[string]FastcFunctionSignature
	constants                 map[string]string
	public_constants          map[string]bool
	globals                   map[string]string
	public_globals            map[string]bool
}

// FastcConstantFileResult is one file's parsed constant initializers.
struct FastcConstantFileResult {
mut:
	// used_field_defaults: the parse consulted rendered struct field defaults,
	// which the parallel pre-pass does not have yet.
	used_field_defaults bool
	values              []FastcConstantValue
	constant_types      map[string]string
	composite_types     map[string]bool
	fixed_array_types   map[string]string
	failed              bool
	error_message       string
}

struct FastcIndexedConstantFileResult {
	index  int
	result FastcConstantFileResult
}

// fastc_parse_constant_file parses the filtered constant source of one file
// (`source_file.source`), starting from `constant_types`, the constant types
// known before it.
fn fastc_parse_constant_file(ctx &FastcConstantGenContext, source_file FastcSourceFile, constant_types map[string]string) FastcConstantFileResult {
	constant_source := source_file.source
	file := token.File.unindexed(source_file.path, constant_source.len)
	prefs := ctx.prefs
	mut gen := Parser{
		prefs: unsafe { prefs }
		unqualified_key_memo: map[string]string{}
		c_function_name_memo: map[string]string{}
		nonlocal_name_type_memo: map[string]string{}
		resolved_name_memo: map[string]string{}
		declared_type_key_memo: map[string]FastcMemoEntry{}
		path: source_file.path
		module_name: source_file.header.module_name
		imports: source_file.header.imports
		declared_types: ctx.declared_types
		declared_type_c_names: ctx.declared_type_c_names
		declared_type_key_by_name: ctx.declared_type_key_by_name
		fastc_prefixed_c_names: ctx.fastc_prefixed_c_names
		declaration_initializer_mode: true
		has_c_functions: ctx.has_c_functions
		comparison_memo: map[i64]FastcRenderedExpression{}
		type_memo: map[i64]string{}
		method_key_memo: map[string]map[string]string{}
		field_memo: map[string]map[string]FastcStructField{}
		spawn_typedefs: map[string]string{}
		spawn_helpers: map[string]string{}
		thread_value_types: map[string]string{}
		declared_kinds: ctx.declared_kinds
		enum_flags: ctx.enum_flags
		enum_field_types: ctx.enum_field_types
		alias_base_types: ctx.alias_base_types
		struct_fields: ctx.struct_fields
		struct_field_info: ctx.struct_field_info
		sum_types: ctx.sum_types
		sum_type_variants: ctx.sum_type_variants
		constants: ctx.constants
		public_constants: ctx.public_constants
		globals: ctx.globals
		public_globals: ctx.public_globals
		selfhost: prefs.building_v
		s: scanner.new_scanner(prefs, .normal)
		out: strings.new_builder(256)
		protos: strings.new_builder(0)
		functions: ctx.functions
		constant_types: constant_types
		composite_types: map[string]bool{}
		fixed_array_types: map[string]string{}
	}
	gen.s.init(file, constant_source)
	gen.next()
	mut values := []FastcConstantValue{}
	gen.parse_selected_constant_declarations(mut values, false) or {
		return FastcConstantFileResult{
			failed: true
			error_message: err.msg()
		}
	}
	if gen.s.diagnostics.len > 0 {
		diagnostic := gen.s.diagnostics[0]
		return FastcConstantFileResult{
			failed: true
			error_message: 'fastc scanner error at byte ${diagnostic.offset} in ${source_file.path}: ${diagnostic.message}'
		}
	}
	return FastcConstantFileResult{
		used_field_defaults: gen.used_field_defaults
		values: values
		constant_types: gen.constant_types.move()
		composite_types: gen.composite_types
		fixed_array_types: gen.fixed_array_types
	}
}

// fastc_constant_file_is_independent reports whether every constant that the
// file's initializers depend on is declared in that same file. Type inference
// of an initializer consults only the types of the constants it references, so
// such a file parsed from the phase's initial constant types yields exactly
// what an in-order parse would; any other file is re-parsed in order.
fn fastc_constant_file_is_independent(result FastcConstantFileResult) bool {
	mut declared := map[string]bool{}
	for value in result.values {
		declared[value.key] = true
	}
	for value in result.values {
		for dependency in value.dependencies {
			if dependency !in declared {
				return false
			}
		}
	}
	return true
}

// fastc_constant_split_size is the constant source size from which a file's
// declarations are parsed as separate parallel candidates.
const fastc_constant_split_size = 16 * 1024

fn fastc_constant_candidates(ordered_sources []FastcSourceFile, constant_sources map[string]string, constant_spans map[string][]int) []FastcSourceFile {
	mut candidates := []FastcSourceFile{cap: ordered_sources.len}
	for source_file in ordered_sources {
		constant_source := constant_sources[source_file.path] or { continue }
		spans := constant_spans[source_file.path] or { []int{} }
		if constant_source.len >= fastc_constant_split_size && spans.len > 2 {
			// A large file's declarations parse as separate candidates, so its
			// tables no longer bound the parallel pre-pass; the in-order merge
			// sees them in source order, as it would within one file.
			for i := 0; i + 1 < spans.len; i += 2 {
				candidates << FastcSourceFile{
					path: source_file.path
					source: constant_source[spans[i]..spans[i + 1]]
					header: source_file.header
				}
			}
			continue
		}
		candidates << FastcSourceFile{
			path: source_file.path
			source: constant_source
			header: source_file.header
		}
	}
	return candidates
}

// fastc_seed_constant_types parses constants in source order only to make their
// types available to struct field defaults. It is used conditionally when a
// default references a constant; the authoritative parallel pass still renders
// and emits the declarations after the defaults are available.
fn fastc_seed_constant_types(ctx &FastcConstantGenContext, candidates []FastcSourceFile, mut constant_types map[string]string) {
	for candidate in candidates {
		mut result := fastc_parse_constant_file(ctx, candidate, constant_types)
		if result.failed {
			return
		}
		constant_types = result.constant_types.move()
	}
}

fn fastc_field_defaults_reference_constants(struct_field_info map[string][]FastcStructField, constants map[string]string) bool {
	for fields in struct_field_info.values() {
		for field in fields {
			if field.default_source == '' {
				continue
			}
			for key in constants.keys() {
				name := key.all_after_last('.')
				if fastc_replace_c_identifier(field.default_source, name, '') != field.default_source {
					return true
				}
			}
		}
	}
	return false
}

fn fastc_generate_constant_declarations(ordered_sources []FastcSourceFile, constant_sources map[string]string, constant_spans map[string][]int, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types map[string]string, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, sum_types map[string]bool, sum_type_variants map[string]bool, mut pending_defaults FastcPendingFieldDefaults, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, globals map[string]string, public_globals map[string]bool, mut constant_types map[string]string) !FastcConstantDeclarations {
	mut values := []FastcConstantValue{}
	mut composite_types := map[string]bool{}
	mut fixed_array_types := map[string]string{}
	ctx := FastcConstantGenContext{
		prefs: unsafe { prefs }
		declared_types: declared_types
		declared_type_c_names: declared_type_c_names
		declared_type_key_by_name: fastc_declared_type_key_by_name(declared_types)
		fastc_prefixed_c_names: fastc_prefixed_c_names
		has_c_functions: fastc_functions_declare_c(functions)
		declared_kinds: declared_kinds
		enum_flags: enum_flags
		enum_field_types: enum_field_types
		alias_base_types: alias_base_types
		struct_fields: struct_fields
		struct_field_info: struct_field_info
		sum_types: sum_types
		sum_type_variants: sum_type_variants
		functions: functions
		constants: constants
		public_constants: public_constants
		globals: globals
		public_globals: public_globals
	}
	// Candidates carry their filtered constant source as `source`, in module
	// dependency order, so both the parallel pre-pass and the in-order merge
	// below see the same files.
	candidates := fastc_constant_candidates(ordered_sources, constant_sources, constant_spans)
	sw := time.new_stopwatch()
	parallel_results := fastc_parse_constant_files_parallel(&ctx, candidates, constant_types)
	parallel_us := sw.elapsed().microseconds()
	// The rendered defaults are needed from here on: for the in-order parse of
	// the files that consulted them, and by the phases after this one.
	defaults := fastc_wait_field_defaults(mut pending_defaults)!
	merge_ctx := FastcConstantGenContext{
		prefs: unsafe { prefs }
		declared_types: declared_types
		declared_type_c_names: declared_type_c_names
		declared_type_key_by_name: ctx.declared_type_key_by_name
		fastc_prefixed_c_names: fastc_prefixed_c_names
		has_c_functions: ctx.has_c_functions
		declared_kinds: declared_kinds
		enum_flags: enum_flags
		enum_field_types: enum_field_types
		alias_base_types: alias_base_types
		struct_fields: struct_fields
		struct_field_info: defaults.struct_field_info
		sum_types: sum_types
		sum_type_variants: sum_type_variants
		functions: functions
		constants: constants
		public_constants: public_constants
		globals: globals
		public_globals: public_globals
	}
	mut serial_count := 0
	for index, candidate in candidates {
		if index < parallel_results.len {
			result := parallel_results[index]
			if !result.failed && !result.used_field_defaults
				&& fastc_constant_file_is_independent(result) {
				for value in result.values {
					constant_types[value.key] = value.typ
				}
				values << result.values
				for name, _ in result.composite_types {
					composite_types[name] = true
				}
				for name, array_type in result.fixed_array_types {
					fixed_array_types[name] = array_type
				}
				continue
			}
		}
		serial_count++
		mut serial := fastc_parse_constant_file(&merge_ctx, candidate, constant_types)
		if serial.failed {
			return error(serial.error_message)
		}
		constant_types = serial.constant_types.move()
		values << serial.values
		for name, _ in serial.composite_types {
			composite_types[name] = true
		}
		for name, array_type in serial.fixed_array_types {
			fixed_array_types[name] = array_type
		}
	}
	if os.getenv('FASTC_BENCH_PHASES') != '' {
		eprintln('fastc-phase constants.detail candidates=${candidates.len} parallel_us=${parallel_us} serial=${serial_count} merge_us=${sw.elapsed().microseconds() - parallel_us}')
	}
	mut runtime_constants := map[string]bool{}
	for value in values {
		if value.is_runtime {
			runtime_constants[value.key] = true
		}
	}
	for {
		mut changed := false
		for value in values {
			if value.key in runtime_constants {
				continue
			}
			for dependency in value.dependencies {
				if dependency in runtime_constants {
					runtime_constants[value.key] = true
					changed = true
					break
				}
			}
		}
		if !changed {
			break
		}
	}
	mut macros := strings.new_builder(4096)
	mut declarations := strings.new_builder(1024)
	mut compile_time_values := map[string]string{}
	for value in values {
		if value.key in runtime_constants {
			if value.typ == '' {
				return error('fastc parser does not support unverifiable runtime constant `${value.key}` type')
			}
			declarations.writeln('static ${value.typ} ${value.c_name};')
		} else {
			macros.writeln('#define ${value.c_name} (${value.value})')
			compile_time_values[value.c_name] = value.value
		}
	}
	mut initializer_order := []int{}
	mut visiting := []int{}
	mut visited := []int{}
	for i, value in values {
		if value.key in runtime_constants {
			fastc_append_runtime_constant(i, values, runtime_constants, mut visiting, mut visited, mut initializer_order)!
		}
	}
	mut module_initializers := map[string]string{}
	for index in initializer_order {
		value := values[index]
		previous := module_initializers[value.module_name] or { '' }
		module_initializers[value.module_name] = previous + '\t${value.c_name} = ${value.value};\n'
	}
	if macros.len > 0 {
		macros.writeln('')
	}
	if declarations.len > 0 {
		declarations.writeln('')
	}
	return FastcConstantDeclarations{
		macros: macros.str()
		declarations: declarations.str()
		module_initializers: module_initializers
		compile_time_values: compile_time_values
		composite_types: composite_types
		fixed_array_types: fixed_array_types
		struct_field_info: defaults.struct_field_info
		struct_field_lookup: defaults.lookup
	}
}

fn (mut g Parser) parse_selected_constant_declarations(mut values []FastcConstantValue, stop_at_block_end bool) ! {
	for g.tok != .eof {
		g.skip_semicolons()
		if stop_at_block_end && g.tok == .rcbr {
			g.next()
			g.skip_semicolons()
			return
		}
		if g.tok == .eof {
			break
		}
		if g.tok == .dollar {
			g.parse_selected_comptime_constant_declarations(mut values)!
			continue
		}
		if g.tok == .key_const {
			g.parse_constant_declaration(mut values)!
			continue
		}
		if g.tok == .key_global {
			// A global initializer may contain a `$d(...)`/`$if`; skip the whole
			// declaration so this pass does not mistake it for a comptime block.
			g.skip_top_level_declaration()!
			continue
		}
		if g.tok == .lcbr {
			g.skip_balanced(.lcbr, .rcbr)!
			continue
		}
		g.next()
	}
	if stop_at_block_end {
		return g.unsupported('unfinished top-level compile-time constant block')
	}
}

fn (mut g Parser) parse_selected_comptime_constant_declarations(mut values []FastcConstantValue) ! {
	g.expect(.dollar)!

	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		g.parse_selected_constant_declarations(mut values, true)!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
		} else {
			g.parse_selected_comptime_constant_declarations(mut values)!
		}
		return
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
	} else {
		g.parse_selected_constant_declarations(mut values, true)!
	}
}

fn fastc_append_runtime_constant(index int, values []FastcConstantValue, runtime_constants map[string]bool, mut visiting []int, mut visited []int, mut ordered []int) ! {
	if index in visited {
		return
	}
	if index in visiting {
		return error('fastc parser does not support cyclic runtime constant initialization involving `${values[index].key}`')
	}
	visiting << index
	for dependency in values[index].dependencies {
		if dependency !in runtime_constants {
			continue
		}
		for dependency_index, value in values {
			if value.key == dependency {
				fastc_append_runtime_constant(dependency_index, values, runtime_constants, mut visiting, mut visited, mut ordered)!
				break
			}
		}
	}
	visiting.delete_last()
	visited << index
	ordered << index
}

fn (mut g Parser) parse_constant_declaration(mut values []FastcConstantValue) ! {
	g.expect(.key_const)!
	if g.tok == .lpar {
		g.next()
		g.skip_semicolons()
		for g.tok != .rpar {
			if g.tok == .eof {
				return g.unsupported('unfinished constant group')
			}
			g.parse_one_constant(mut values, [token.Token.semicolon, token.Token.rpar])!
			g.skip_semicolons()
		}
		g.next()
		g.skip_semicolons()
		return
	}
	g.parse_one_constant(mut values, [token.Token.semicolon])!
	g.skip_semicolons()
}

fn (mut g Parser) parse_one_constant(mut values []FastcConstantValue, stops []token.Token) ! {
	if g.tok != .name {
		return g.unsupported('constant name `${g.token_source()}`')
	}
	name := g.lit
	g.next()
	if name == 'C' && g.tok == .dot {
		// `const C.name type` declares an external C constant; the C headers
		// already provide the symbol, so FastC skips the whole declaration.
		for g.tok !in stops && g.tok != .eof {
			g.next()
		}
		return
	}
	if g.tok !in [.assign, .decl_assign] {
		return g.unsupported('constant `${name}` requires `=` or `:=` after its name, got `${g.token_source()}`')
	}
	g.next()
	value := g.read_expression(stops)!
	if value.len == 0 {
		return g.unsupported('empty constant `${name}`')
	}
	key := fastc_constant_key(g.module_name, name)
	c_name := g.constants[key] or { return g.unsupported('unregistered constant `${name}`') }
	typ := fastc_normalize_inferred_type(g.last_expression_type)
	is_runtime := g.constant_expression_requires_runtime_storage(g.last_expression, value)
	if typ == '' && is_runtime {
		return g.unsupported('unverifiable constant `${name}` type')
	}
	values << FastcConstantValue{
		key: key
		c_name: c_name
		module_name: g.module_name
		value: value
		typ: typ
		dependencies: g.constant_expression_dependencies(g.last_expression)
		is_runtime: is_runtime
	}
	g.constant_types[key] = typ
}

fn (g &Parser) constant_expression_dependencies(tokens []FastcExpressionToken) []string {
	mut dependencies := []string{}
	for i, item in tokens {
		if item.tok != .name || (i > 0 && tokens[i - 1].tok == .dot) {
			continue
		}
		if i + 1 < tokens.len && tokens[i + 1].tok == .lpar && fastc_primitive_c_type(item.lit) != none {
			continue
		}
		mut key := ''
		if i + 2 < tokens.len && tokens[i + 1].tok == .dot && tokens[i + 2].tok == .name {
			if imported_module := g.imports[item.lit] {
				key = fastc_constant_key(imported_module, tokens[i + 2].lit)
			}
		} else {
			local_key := fastc_constant_key(g.module_name, item.lit)
			builtin_key := fastc_constant_key('builtin', item.lit)
			if local_key in g.constants {
				key = local_key
			} else if builtin_key in g.constants {
				key = builtin_key
			} else if imported_module := g.imports[fastc_selective_import_key(item.lit)] {
				key = fastc_constant_key(imported_module, item.lit)
			}
		}
		if key != '' && key !in dependencies {
			dependencies << key
		}
	}
	return dependencies
}

fn (g &Parser) constant_expression_requires_runtime_storage(tokens []FastcExpressionToken, rendered string) bool {
	if rendered.contains('({') {
		return true
	}
	for i, item in tokens {
		if item.tok in [.lcbr, .lsbr] {
			return true
		}
		if item.tok != .name || i + 1 >= tokens.len || tokens[i + 1].tok != .lpar {
			continue
		}
		if i > 0 && tokens[i - 1].tok == .dot {
			return true
		}
		if fastc_primitive_c_type(item.lit) != none || g.resolve_declared_type_key(item.lit) != none {
			continue
		}
		return true
	}
	return false
}

// fastc_composite_typedefs renders the typedefs of the composite (`Array_x`,
// `Map_k_v`) types named by signatures and struct fields. It precedes the
// type declarations in the output; it is rendered after the type phase and
// the signature phase have both contributed their names.
fn fastc_composite_typedefs(composite_types map[string]bool) string {
	mut out := strings.new_builder(1024)
	mut composite_names := composite_types.keys()
	composite_names.sort()
	for composite_name in composite_names {
		base := if composite_name.starts_with('Array_') { 'array' } else { 'map' }
		out.writeln('typedef ${base} ${composite_name};')
	}
	out.writeln('')
	return out.str()
}

// FastcTypeDeclarationResult is the type phase's output with the tables it
// fills, so the phase can run on a worker and hand them back.
struct FastcTypeDeclarationResult {
mut:
	output            FastcTypeDeclarations
	struct_fields     map[string]map[string]string
	struct_field_info map[string][]FastcStructField
	composite_types   map[string]bool
	failed            bool
	error_message     string
}

// fastc_run_type_declarations runs the type phase into fresh tables.
fn fastc_run_type_declarations(sources []FastcSourceFile, type_sources map[string]string, prefs &pref.Preferences, type_source_paths map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool) FastcTypeDeclarationResult {
	mut result := FastcTypeDeclarationResult{
		struct_fields: map[string]map[string]string{}
		struct_field_info: map[string][]FastcStructField{}
		composite_types: map[string]bool{}
	}
	result.output = fastc_generate_type_declarations(sources, type_sources, prefs, type_source_paths, declared_types, declared_kinds, enum_flags, constants, public_constants, mut result.struct_fields, mut result.struct_field_info, mut result.composite_types) or {
		result.failed = true
		result.error_message = err.msg()
		return result
	}
	return result
}

fn fastc_generate_type_declarations(sources []FastcSourceFile, type_sources map[string]string, prefs &pref.Preferences, type_source_paths map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool) !FastcTypeDeclarations {
	mut out := strings.new_builder(4096)
	mut bodies := strings.new_builder(4096)
	mut enum_infos := []FastcEnumInfo{}
	mut alias_base_types := map[string]string{}
	mut fn_alias_return_types := map[string]string{}
	mut sum_types := map[string]bool{}
	mut sum_type_variants := map[string]bool{}
	mut keys := declared_kinds.keys()
	keys.sort()
	for type_id, key in keys {
		name := fastc_c_declared_type_name(key)
		out.writeln('#define __v_typeid_${name} ${type_id + 1}')
		match declared_kinds[key] {
			.struct_, .interface_ { out.writeln('typedef struct ${name} ${name};') }
			.union_ { out.writeln('typedef union ${name} ${name};') }
			.enum_ {
				out.writeln('typedef ${if enum_flags[key] { 'u64' } else { 'int' }} ${name};')
			}
			.alias_ {}
		}
	}
	// Primitive scalars can be sum-type variants (`type Any = int | bool | ...`).
	// They are never declared types, so give them stable type ids in a high range
	// that cannot collide with the sequential declared-type ids above. Construction
	// and `match` both reference `__v_typeid_<primitive>`, so the exact value only
	// has to be unique and consistent.
	for offset, primitive in fastc_boxed_primitive_types {
		out.writeln('#define __v_typeid_${primitive} ${u32(0x40000000) + u32(offset)}')
	}
	out.writeln('')
	mut timer := fastc_new_phase_timer()
	for source_file in sources {
		if source_file.path !in type_source_paths {
			continue
		}
		type_source := type_sources[source_file.path] or { continue }
		type_source_file := FastcSourceFile{
			path: source_file.path
			source: type_source
			header: source_file.header
		}
		fastc_emit_source_type_declarations(type_source_file, prefs, declared_types, declared_kinds, constants, public_constants, mut struct_fields, mut struct_field_info, mut composite_types, mut alias_base_types, mut fn_alias_return_types, mut sum_types, mut sum_type_variants, mut enum_infos, mut bodies)!
	}
	timer.mark('type_declarations.emit')
	// The composite typedefs go here in the output; they are rendered by the
	// caller once the signature phase has contributed its names too.
	declarations_head := out.str()
	mut type_bodies := fastc_hoist_c_type_aliases(bodies.str())
	timer.mark('type_declarations.hoist')
	if prefs.building_v {
		// Index the by-value composite C spellings once. The ordering pass below
		// queries this per struct field on every pass; the previous linear scan
		// over declared_kinds re-rendered each candidate's C name (a `.replace`
		// allocation) on every query, which dominated the type-declaration phase.
		mut by_value_composite_names := map[string]bool{}
		for key, kind in declared_kinds {
			if kind in [.struct_, .union_, .interface_] {
				by_value_composite_names[fastc_c_declared_type_name(key)] = true
			}
		}
		type_bodies = fastc_order_c_composite_definitions(type_bodies, struct_fields, by_value_composite_names, alias_base_types)
	}
	timer.mark('type_declarations.order')
	out.write_string(type_bodies)
	// Index each enum field name to its enum's C type, so an inferred map literal
	// with `.field` shorthand keys (e.g. `{ .md_block_hr: 'hr' }`) can recover the
	// key type. First declaration wins on the rare shared field name.
	mut enum_field_types := map[string]string{}
	mut enum_field_names := map[string][]string{}
	for info in enum_infos {
		enum_field_names[info.c_name] = info.fields.clone()
		for field in info.fields {
			if field !in enum_field_types {
				enum_field_types[field] = info.c_name
			}
		}
	}
	enum_helper_names, enum_helper_texts := fastc_generate_enum_string_helpers(enum_infos)
	timer.mark('type_declarations.enum_helpers')
	return FastcTypeDeclarations{
		declarations_head: declarations_head
		declarations: out.str()
		enum_helper_names: enum_helper_names
		enum_helper_texts: enum_helper_texts
		alias_base_types: alias_base_types
		fn_alias_return_types: fn_alias_return_types
		enum_field_types: enum_field_types
		enum_field_names: enum_field_names
		sum_types: sum_types
		sum_type_variants: sum_type_variants
	}
}

fn fastc_hoist_c_type_aliases(source string) string {
	mut defines := strings.new_builder(256)
	mut aliases := strings.new_builder(256)
	mut function_aliases := strings.new_builder(256)
	mut bodies := strings.new_builder(source.len)
	for line in source.split('\n') {
		if line.starts_with('#define ') {
			defines.writeln(line)
		} else if line.starts_with('typedef ') && line.ends_with(';') {
			if line.contains('(*') {
				function_aliases.writeln(line)
			} else {
				aliases.writeln(line)
			}
		} else {
			bodies.writeln(line)
		}
	}
	if defines.len == 0 && aliases.len == 0 && function_aliases.len == 0 {
		return source
	}
	defines.write_string(aliases.str())
	defines.write_string(function_aliases.str())
	defines.writeln('')
	defines.write_string(bodies.str())
	return defines.str()
}

fn fastc_order_c_composite_definitions(source string, struct_fields map[string]map[string]string, by_value_composite_names map[string]bool, alias_base_types map[string]string) string {
	mut ordered := source
	mut changed := true
	mut passes := 0
	// Index every `struct/union NAME {` definition start once. The
	// dependency-already-before-dependent test is the overwhelmingly common
	// case, and a splice only moves one definition block, so the index is
	// updated arithmetically instead of rescanning the text after each move.
	mut positions := fastc_composite_definition_positions(ordered)
	for changed && passes < struct_fields.len {
		changed = false
		passes++
		for dependent, fields in struct_fields {
			for _, field_type in fields {
				dependency := fastc_by_value_composite_type(field_type, by_value_composite_names, alias_base_types)
				if dependency == '' || dependency == dependent {
					continue
				}
				dependency_start := positions[dependency] or { continue }
				dependent_start := positions[dependent] or { continue }
				if dependency_start < dependent_start {
					continue
				}
				block_end := fastc_c_composite_block_end(ordered, dependency_start) or { continue }
				next := fastc_move_c_composite_before(ordered, dependency_start, dependent_start)
				if next != ordered {
					ordered = next
					changed = true
					fastc_shift_composite_positions(mut positions, dependency_start, block_end, dependent_start)
				}
			}
		}
	}
	return ordered
}

// fastc_shift_composite_positions updates definition starts after the block
// [block_start, block_end) was spliced in front of the definition at
// insert_at: the moved block now starts there, and every definition that was
// between the insertion point and the block moved down by the block's length.
fn fastc_shift_composite_positions(mut positions map[string]int, block_start int, block_end int, insert_at int) {
	block_len := block_end - block_start
	for name, start in positions {
		if start == block_start {
			positions[name] = insert_at
		} else if start >= insert_at && start < block_start {
			positions[name] = start + block_len
		}
	}
}

// fastc_c_composite_block_end returns the end of the definition block that
// starts at `start`, including the newlines that follow its `};`.
fn fastc_c_composite_block_end(source string, start int) ?int {
	mut end := fastc_c_composite_definition_end(source, start) or { return none }
	for end < source.len && source[end] == `\n` {
		end++
	}
	return end
}

// fastc_composite_definition_positions maps each C composite name to the start
// offset of the first `struct NAME {` / `union NAME {` definition.
@[direct_array_access]
fn fastc_composite_definition_positions(source string) map[string]int {
	mut positions := map[string]int{}
	mut i := 0
	for i < source.len {
		c := source[i]
		mut keyword_len := 0
		if c == `s` && i + 7 <= source.len && source[i + 1] == `t` && source[i + 2] == `r` && source[i + 3] == `u` && source[i + 4] == `c` && source[i + 5] == `t` && source[i + 6] == ` ` {
			keyword_len = 7
		} else if c == `u` && i + 6 <= source.len && source[i + 1] == `n` && source[i + 2] == `i` && source[i + 3] == `o` && source[i + 4] == `n` && source[i + 5] == ` ` {
			keyword_len = 6
		}
		if keyword_len == 0 {
			i++
			continue
		}
		mut j := i + keyword_len
		name_start := j
		for j < source.len && (source[j].is_alnum() || source[j] == `_`) {
			j++
		}
		if j > name_start && j + 1 < source.len && source[j] == ` ` && source[j + 1] == `{` {
			name := source[name_start..j]
			if name !in positions {
				positions[name] = i
			}
		}
		i = j
	}
	return positions
}

fn fastc_by_value_composite_type(field_type string, by_value_composite_names map[string]bool, alias_base_types map[string]string) string {
	if field_type.ends_with('*') || field_type.starts_with('Array_') || field_type.starts_with('Map_') {
		return ''
	}
	mut candidate := field_type
	if element_type := fastc_fixed_array_element_type(candidate) {
		candidate = element_type
	}
	// Most field types are not aliases; answer those without the cycle guard.
	if candidate !in alias_base_types {
		return if candidate in by_value_composite_names { candidate } else { '' }
	}
	mut seen_aliases := map[string]bool{}
	for {
		if candidate !in alias_base_types || candidate in seen_aliases {
			break
		}
		seen_aliases[candidate] = true
		candidate = alias_base_types[candidate]
		if candidate.ends_with('*') {
			return ''
		}
	}
	if candidate in by_value_composite_names {
		return candidate
	}
	return ''
}

fn fastc_move_c_composite_before(source string, dependency_start int, dependent_start int) string {
	if dependency_start < dependent_start {
		return source
	}
	end := fastc_c_composite_block_end(source, dependency_start) or { return source }
	block := source[dependency_start..end]
	return source[..dependent_start] + block + source[dependent_start..dependency_start] + source[end..]
}

fn fastc_c_composite_definition_end(source string, start int) ?int {
	tail := source[start..]
	if relative_end := tail.index('\n};') {
		return start + relative_end + 3
	}
	if relative_end := tail.index('};') {
		return start + relative_end + 2
	}
	return none
}

fn fastc_emit_source_type_declarations(source_file FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool, mut alias_base_types map[string]string, mut fn_alias_return_types map[string]string, mut sum_types map[string]bool, mut sum_type_variants map[string]bool, mut enum_infos []FastcEnumInfo, mut out strings.Builder) ! {
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut depth := 0
	mut next_enum_is_flag := false
	mut next_type_is_enabled := true
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), source_file.path, prefs)!
				if selected.source != '' {
					selected_source := FastcSourceFile{
						path: source_file.path
						source: selected.source
						header: source_file.header
					}
					fastc_emit_source_type_declarations(selected_source, prefs, declared_types, declared_kinds, constants, public_constants, mut struct_fields, mut struct_field_info, mut composite_types, mut alias_base_types, mut fn_alias_return_types, mut sum_types, mut sum_type_variants, mut enum_infos, mut out)!
				}
				tok = selected.tok
				next_enum_is_flag = false
				continue
			}
		}
		if depth == 0 && tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut scan, source_file.path, prefs)!
			tok = attribute.tok
			next_enum_is_flag = next_enum_is_flag || attribute.is_flag
			next_type_is_enabled = next_type_is_enabled && attribute.is_enabled
			continue
		}
		if depth == 0 && tok in [.key_fn, .key_const, .key_global] {
			next_enum_is_flag = false
			next_type_is_enabled = true
			if tok == .key_fn {
				// A function or method whose name is itself a keyword
				// (`fn (e Expr) type() Type`) tokenizes that name as e.g. `.key_type`.
				// Skip the receiver, name, and parameter lists so the keyword name is
				// never mistaken for a top-level type declaration.
				tok = fastc_skip_function_header(mut scan)!
				continue
			}
		}
		if depth == 0 && !next_type_is_enabled && tok in [.key_struct, .key_union, .key_enum,
			.key_interface, .key_type] {
			tok = fastc_skip_type_declaration(mut scan, tok)!
			next_enum_is_flag = false
			next_type_is_enabled = true
			continue
		}
		if depth == 0 && tok in [.key_struct, .key_union] {
			tok = fastc_emit_struct_declaration(mut scan, tok == .key_union, source_file, declared_types, prefs.building_v, mut struct_fields, mut struct_field_info, mut composite_types, mut out)!
			next_type_is_enabled = true
			continue
		}
		if depth == 0 && tok == .key_enum {
			tok = fastc_emit_enum_declaration(mut scan, source_file, next_enum_is_flag, declared_types, declared_kinds, constants, public_constants, mut enum_infos, mut out)!
			next_enum_is_flag = false
			next_type_is_enabled = true
			continue
		}
		if depth == 0 && tok == .key_interface {
			tok = fastc_emit_interface_declaration(mut scan, source_file, mut out)!
			next_type_is_enabled = true
			continue
		}
		if depth == 0 && tok == .key_type {
			tok = fastc_emit_alias_declaration(mut scan, source_file, declared_types, declared_kinds, prefs.building_v, mut struct_fields, mut struct_field_info, mut alias_base_types, mut fn_alias_return_types, mut sum_types, mut sum_type_variants, mut out)!
			next_type_is_enabled = true
			continue
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr && depth > 0 {
			depth--
		}
		tok = scan.scan()
	}
}

// fastc_skip_function_header consumes a top-level function's receiver, name, and
// generic/value parameter lists starting right after the `fn` keyword. It exists
// so that a method or function whose name is a V keyword (e.g. `type`, `map`) is
// not mistaken for a top-level type declaration by the declaration scanner. It
// returns the first token after the parameter list (the return type or body).
fn fastc_skip_function_header(mut scan scanner.Scanner) !token.Token {
	mut tok := scan.scan()
	if tok == .lpar {
		// Method receiver, e.g. `(e Expr)`.
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lpar, .rpar)!
	}
	// The name may be `C.free`, a plain identifier, an operator overload token, or
	// a keyword used as a method name. Consume exactly the name here.
	if tok == .name && scan.lit == 'C' {
		tok = scan.scan()
		if tok == .dot {
			tok = scan.scan()
			tok = scan.scan()
		}
	} else if tok != .eof && tok != .lpar && tok != .lsbr {
		tok = scan.scan()
	}
	if tok == .lsbr {
		// Generic type parameters, e.g. `[T]`.
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
	}
	if tok == .lpar {
		// Value parameters.
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lpar, .rpar)!
	}
	return tok
}

fn fastc_scan_declaration_attribute(mut scan scanner.Scanner, path string, prefs &pref.Preferences) !FastcDeclarationAttribute {
	mut tok := scan.scan()
	mut depth := 1
	mut is_flag := false
	mut is_params := false
	mut is_typedef := false
	mut is_c_extern := false
	mut is_enabled := true
	mut at_item_start := true
	for depth > 0 {
		if tok == .eof {
			return error('fastc parser does not support unfinished declaration attribute in ${path}')
		}
		if depth == 1 && at_item_start && tok == .key_if {
			condition := fastc_scan_comptime_or(mut scan, scan.scan(), path, prefs)!
			is_enabled = is_enabled && condition.value
			tok = condition.tok
			if tok !in [.semicolon, .rsbr] {
				return error('fastc parser does not support conditional attribute expression in ${path}')
			}
			continue
		}
		if tok == .name && scan.lit == 'flag' {
			is_flag = true
		}
		if at_item_start && tok == .name && scan.lit == 'params' {
			is_params = true
		}
		if tok == .name && scan.lit == 'typedef' {
			is_typedef = true
		}
		if tok == .name && scan.lit == 'c_extern' {
			is_c_extern = true
		}
		if depth == 1 && tok == .semicolon {
			at_item_start = true
		} else if depth == 1 {
			at_item_start = false
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return FastcDeclarationAttribute{
		tok: tok
		is_enabled: is_enabled
		is_flag: is_flag
		is_params: is_params
		is_typedef: is_typedef
		is_c_extern: is_c_extern
	}
}

fn fastc_emit_struct_declaration(mut scan scanner.Scanner, is_union bool, source_file FastcSourceFile, declared_types map[string]bool, allow_short_placeholders bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support struct declaration in ${source_file.path}')
	}
	mut name := scan.lit
	tok = scan.scan()
	mut is_c_struct := false
	if name == 'C' && tok == .dot {
		is_c_struct = true
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support C struct declaration in ${source_file.path}')
		}
		name = scan.lit
		tok = scan.scan()
	}
	mut type_module := source_file.header.module_name
	if is_c_struct {
		type_module = 'C'
	}
	key := fastc_type_key(type_module, name)
	mut c_name := fastc_c_declared_type_name(key)
	if is_c_struct {
		c_name = name
		if '#Cstruct#${name}' in declared_types {
			c_name = 'struct ${name}'
		}
	}
	mut fields_by_name := map[string]string{}
	if c_name in struct_fields {
		fields_by_name = struct_fields[c_name].clone()
	}
	mut field_info := []FastcStructField{}
	if c_name in struct_field_info {
		field_info = struct_field_info[c_name].clone()
	}
	if tok == .lsbr {
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
	}
	if tok == .name && scan.lit == 'implements' {
		// `struct S implements A, B {`: the declared interfaces are advisory, so
		// skip them up to the struct body.
		for tok != .lcbr && tok != .eof {
			tok = scan.scan()
		}
	}
	if tok != .lcbr {
		return error('fastc parser does not support struct `${name}` body in ${source_file.path}')
	}
	if !is_c_struct {
		out.writeln('${if is_union { 'union' } else { 'struct' }} ${c_name} {')
	}
	tok = scan.scan()
	mut fields := 0
	mut embedded_id := 0
	mut fields_are_public := is_c_struct
	mut fields_are_mutable := false
	for tok != .rcbr && tok != .eof {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		if tok == .attribute {
			tok = fastc_skip_attribute(mut scan)!
			continue
		}
		if tok == .key_pub {
			fields_are_public = true
			fields_are_mutable = false
			tok = scan.scan()
			if tok == .key_mut {
				fields_are_mutable = true
				tok = scan.scan()
			}
			if tok == .colon {
				tok = scan.scan()
			}
			continue
		}
		if tok in [.key_mut, .key_global] {
			fields_are_public = false
			fields_are_mutable = true
			tok = scan.scan()
			if tok == .colon {
				tok = scan.scan()
			}
			continue
		}
		if tok != .name && !tok.is_keyword() {
			return error('fastc parser does not support struct `${name}` field token `${tok.str()}` in ${source_file.path}')
		}
		// A field named with a V keyword (`type int` in `struct C.cJSON`): C struct members
		// mirror C headers where such names are ordinary identifiers. `scan.lit` holds the
		// keyword text, and it is a valid C field name (sanitized via fastc_c_identifier).
		mut field_names := [scan.lit]
		tok = scan.scan()
		// A qualified embedded field like `mbedtls.SSLConnectConfig` (or
		// `veb.Context`): a dot right after the first field name means the field is
		// an embedded `module.Type`, not a `name type` pair.
		mut qualified_embed_key := ''
		if tok == .dot && field_names.len == 1 {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support embedded field in ${source_file.path}')
			}
			embed_module := source_file.header.imports[field_names[0]] or { field_names[0] }
			qualified_embed_key = fastc_type_key(embed_module, scan.lit)
			tok = scan.scan()
			if tok == .lsbr {
				// Generic embed like `veb.Middleware[Context]`: skip the type args
				// (the embed still resolves to the non-generic C struct name).
				tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
			}
		}
		for tok == .comma {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support struct `${name}` grouped field in ${source_file.path}')
			}
			field_names << scan.lit
			tok = scan.scan()
		}
		if tok == .semicolon || tok == .rcbr {
			embedded_key := if qualified_embed_key != '' {
				qualified_embed_key
			} else {
				fastc_resolve_declared_type_key(source_file.header.module_name, field_names[0], source_file.header.imports, declared_types) or {
					return error('fastc parser does not support embedded field `${field_names[0]}` in ${source_file.path}')
				}
			}
			if !is_c_struct {
				out.writeln('\t${fastc_c_declared_type_name(embedded_key)} __embedded_${embedded_id};')
			}
			fields_by_name['__embedded_${embedded_id}'] = fastc_c_declared_type_name(embedded_key)
			field_info << FastcStructField{
				name: '__embedded_${embedded_id}'
				typ: fastc_c_declared_type_name(embedded_key)
				is_public: true
				module_name: source_file.header.module_name
				path: source_file.path
				imports: source_file.header.imports.clone()
			}
			embedded_id++
			fields++
			if tok == .semicolon {
				tok = scan.scan()
			}
			continue
		}
		mut is_shared_field := false
		if tok == .key_shared {
			// FastC does not emit mutex operations yet, but shared maps still need reference
			// identity: a copied owner must update the same map header as the original owner.
			is_shared_field = true
			tok = scan.scan()
		}
		mut field_chan_element := ''
		if tok == .name && scan.lit == 'chan' {
			field_chan_element = fastc_peek_chan_element(scan, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)
		}
		mut field_option_value := ''
		if tok == .question {
			field_option_value = fastc_peek_option_element(scan, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)
		}
		field_generic_argument := fastc_peek_generic_type_argument(tok, scan, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)
		mut is_optional_function := false
		mut function_scan := scan
		if tok == .question && function_scan.scan() == .key_fn {
			is_optional_function = true
		}
		is_function_field := tok == .key_fn || is_optional_function
		mut function_type := FastcFunctionTypeInfo{}
		if is_function_field {
			function_type = fastc_peek_function_type(function_scan, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
		}
		field_type, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders) or {
			return error('fastc struct `${name}` field `${field_names[0]}`: ${err.msg()}')
		}
		tok = next_token
		fastc_register_composite_type(field_type, mut composite_types)
		mut is_required := false
		mut is_skip := false
		for tok == .attribute {
			mut attribute_is_required := false
			mut attribute_is_skip := false
			tok, attribute_is_required, attribute_is_skip = fastc_scan_struct_field_attribute(mut scan)!
			is_required = is_required || attribute_is_required
			is_skip = is_skip || attribute_is_skip
		}
		mut default_source := ''
		if tok == .assign {
			first_default_token := scan.scan()
			default_start := scan.pos
			tok = fastc_skip_field_default_from_token(mut scan, first_default_token)!
			default_source = source_file.source[default_start..scan.pos].trim_space().trim_right(';').trim_space()
			if default_source == '' {
				return error('fastc parser does not support empty default for struct `${name}` field `${field_names[0]}` in ${source_file.path}')
			}
		}
		for field_name in field_names {
			c_field_name := fastc_c_identifier(field_name)
			mut emitted_field_type := field_type
			// Imported generic declarations currently use `voidptr` for their type
			// parameters. json2's private linked list has a single concrete payload,
			// `ValueInfo`; retain that layout so non-generic Decoder methods can access
			// `current_node.value` as the declared struct.
			if name == 'Node' && field_name == 'value' && field_type == 'voidptr' && source_file.path.ends_with('/vlib/json2/decode.v') {
				value_info_key := fastc_type_key(source_file.header.module_name, 'ValueInfo')
				if value_info_key in declared_types {
					emitted_field_type = fastc_c_declared_type_name(value_info_key)
				}
			}
			is_shared_pointer := is_shared_field && emitted_field_type.starts_with('Map_')
			if !is_c_struct {
				if is_shared_pointer {
					out.writeln('\t${emitted_field_type}* ${c_field_name};')
				} else if declaration := fastc_fixed_array_field_declaration(emitted_field_type, c_field_name) {
					out.writeln('\t${declaration};')
				} else {
					out.writeln('\t${fastc_output_c_type(emitted_field_type)} ${c_field_name};')
				}
			}
			fields_by_name[field_name] = emitted_field_type
			field_info << FastcStructField{
				name: field_name
				typ: emitted_field_type
				is_public: fields_are_public
				is_mutable: fields_are_mutable
				is_required: is_required
				is_skip: is_skip
				is_function: is_function_field
				is_optional_function: is_optional_function
				is_shared_pointer: is_shared_pointer
				module_name: source_file.header.module_name
				path: source_file.path
				imports: source_file.header.imports.clone()
				default_source: default_source
				chan_element_type: field_chan_element
				option_value_type: field_option_value
				fn_parameter_types: function_type.parameter_types.clone()
				fn_return_type: function_type.return_type
				fn_option_value_type: function_type.option_value_type
				generic_argument_type: field_generic_argument
			}
			fields++
		}
		if tok == .semicolon {
			tok = scan.scan()
		}
	}
	if tok != .rcbr {
		return error('fastc parser does not support unfinished struct `${name}` in ${source_file.path}')
	}
	if fields == 0 && !is_c_struct {
		out.writeln('\tunsigned char __empty;')
	}
	if !is_c_struct && c_name in ['Option', '_option', '_result'] {
		out.writeln('\tvoid *data;')
		fields_by_name['data'] = 'voidptr'
		field_info << FastcStructField{
			name: 'data'
			typ: 'voidptr'
			is_public: true
			module_name: source_file.header.module_name
			path: source_file.path
			imports: source_file.header.imports.clone()
		}
	}
	if !is_c_struct {
		out.writeln('};')
		out.writeln('')
	}
	struct_fields[c_name] = fields_by_name.move()
	struct_field_info[c_name] = field_info.clone()
	return scan.scan()
}

fn fastc_fixed_array_field_declaration(field_type string, field_name string) ?string {
	mut element_type := field_type
	mut dimensions := []string{}
	for {
		length := fastc_fixed_array_length(element_type) or { break }
		dimensions << length
		element_type = fastc_fixed_array_element_type(element_type) or { return none }
	}
	if dimensions.len == 0 {
		return none
	}
	return '${element_type} ${field_name}[${dimensions.join('][')}]'
}

fn fastc_resolve_declared_type_key(module_name string, raw_type string, imports map[string]string, declared_types map[string]bool) ?string {
	local_key := fastc_type_key(module_name, raw_type)
	if local_key in declared_types {
		return local_key
	}
	if imported_module := imports[fastc_selective_import_key(raw_type)] {
		imported_key := fastc_type_key(imported_module, raw_type)
		if imported_key in declared_types && declared_types[imported_key] {
			return imported_key
		}
	}
	if raw_type in declared_types {
		return raw_type
	}
	if raw_type.contains('__') {
		generated_key := raw_type.replace('__', '.')
		if generated_key in declared_types {
			return generated_key
		}
	}
	return none
}

fn (g &Parser) resolve_declared_type_key(raw_type string) ?string {
	if entry := g.declared_type_key_memo[raw_type] {
		if entry.found {
			return entry.value
		}
		return none
	}
	mut w := unsafe { &Parser(g) }
	if key := g.resolve_declared_type_key_uncached(raw_type) {
		w.declared_type_key_memo[raw_type] = FastcMemoEntry{
			found: true
			value: key
		}
		return key
	}
	w.declared_type_key_memo[raw_type] = FastcMemoEntry{}
	return none
}

fn (g &Parser) resolve_declared_type_key_uncached(raw_type string) ?string {
	if g.declared_type_key_by_name.len == 0 {
		return fastc_resolve_declared_type_key(g.module_name, raw_type, g.imports, g.declared_types)
	}
	key := g.declared_type_key_by_name[raw_type] or {
		if raw_type.contains('__') {
			return g.declared_type_c_names[raw_type] or { return none }
		}
		return none
	}
	if key == '' {
		return fastc_resolve_declared_type_key(g.module_name, raw_type, g.imports, g.declared_types)
	}
	if fastc_type_key_matches_module(key, g.module_name, raw_type) {
		return key
	}
	if imported_module := g.imports[fastc_selective_import_key(raw_type)] {
		if g.declared_types[key] && fastc_type_key_matches_module(key, imported_module, raw_type) {
			return key
		}
	}
	if key == raw_type {
		return key
	}
	return none
}

fn fastc_type_key_matches_module(key string, module_name string, name string) bool {
	if module_name in ['', 'main', 'builtin'] {
		return key == name
	}
	return key.len == module_name.len + name.len + 1 && key.starts_with(module_name)
		&& key[module_name.len] == `.`
}

fn fastc_semantic_declared_type_key(c_type string, declared_type_c_names map[string]string) string {
	base := c_type.trim_right('*')
	// Resolve through the precomputed C-spelling index (first key per spelling
	// wins, matching the previous insertion-ordered first-match scan). The old
	// scan re-rendered every declared type's C name per query — an allocation
	// per key on a path hit once per method receiver during signature scanning.
	if key := declared_type_c_names[base] {
		return key
	}
	return base
}

fn fastc_emit_enum_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, is_flag bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool, mut enum_infos []FastcEnumInfo, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support enum declaration in ${source_file.path}')
	}
	name := scan.lit
	key := fastc_type_key(source_file.header.module_name, name)
	c_name := fastc_c_declared_type_name(key)
	tok = scan.scan()
	if tok == .key_as {
		tok = scan.scan()
		// Accept any integer backing type (`as u32`, `as u8`, ...). The C typedef is
		// still `int` (or `u64` for flag enums); values are emitted with an explicit
		// cast, so a narrower/wider spelling does not change the generated C.
		is_backing_int := tok == .name && scan.lit in ['i8', 'i16', 'i32', 'i64', 'int', 'u8', 'u16',
			'u32', 'u64', 'isize', 'usize']
		if !is_backing_int || (is_flag && scan.lit != 'u64') {
			return error('fastc parser does not support enum `${name}` backing type in ${source_file.path}')
		}
		tok = scan.scan()
	}
	if tok != .lcbr {
		return error('fastc parser does not support enum `${name}` body in ${source_file.path}')
	}
	tok = scan.scan()
	mut value := 0
	mut symbolic_value := ''
	mut symbolic_offset := 0
	mut field_names := []string{}
	mut fields := map[string]bool{}
	for tok != .rcbr && tok != .eof {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		if tok == .attribute {
			tok = fastc_skip_attribute(mut scan)!
			continue
		}
		if tok != .name && !tok.is_keyword() {
			return error('fastc parser does not support enum `${name}` field in ${source_file.path}')
		}
		field_name := scan.lit
		if field_name in fields {
			return error('fastc parser does not support duplicate enum field `${name}.${field_name}` in ${source_file.path}')
		}
		field_names << field_name
		tok = scan.scan()
		if tok == .assign {
			if is_flag {
				return error('fastc parser does not support custom value for flag enum field `${name}.${field_name}` in ${source_file.path}')
			}
			value_tokens, next_token := fastc_scan_enum_value_tokens(mut scan, scan.scan())!
			tok = next_token
			if literal_value := fastc_enum_integer_literal(value_tokens) {
				value = literal_value
				symbolic_value = ''
				symbolic_offset = 0
			} else {
				symbolic_value = fastc_render_enum_value_expression(value_tokens, source_file, c_name, fields, declared_types, declared_kinds, constants, public_constants)!
				symbolic_offset = 0
			}
		}
		value_expression := if symbolic_value == '' {
			value.str()
		} else if symbolic_offset == 0 {
			symbolic_value
		} else {
			'(${symbolic_value} + ${symbolic_offset})'
		}
		c_value := if is_flag {
			'(((u64)1) << (${value_expression}))'
		} else {
			value_expression
		}
		out.writeln('#define ${c_name}__${field_name} ((${c_name})${c_value})')
		fields[field_name] = true
		if symbolic_value == '' {
			value++
		} else {
			symbolic_offset++
		}
	}
	if tok != .rcbr {
		return error('fastc parser does not support unfinished enum `${name}` in ${source_file.path}')
	}
	enum_infos << FastcEnumInfo{
		c_name: c_name
		name: name
		fields: field_names.clone()
		is_flag: is_flag
	}
	return scan.scan()
}

fn fastc_scan_enum_value_tokens(mut scan scanner.Scanner, first token.Token) !([]FastcExpressionToken, token.Token) {
	mut tokens := []FastcExpressionToken{}
	mut tok := first
	mut parens := 0
	for tok != .eof {
		if parens == 0 && tok in [.comma, .semicolon, .rcbr] {
			if tokens.len == 0 {
				return error('fastc parser does not support an empty enum discriminant')
			}
			return tokens, tok
		}
		if tok == .lpar {
			parens++
		} else if tok == .rpar {
			parens--
			if parens < 0 {
				return error('fastc parser does not support an unbalanced enum discriminant')
			}
		}
		tokens << FastcExpressionToken{
			tok: tok
			lit: scan.lit
		}
		tok = scan.scan()
	}
	return error('fastc parser does not support an unfinished enum discriminant')
}

fn fastc_enum_integer_literal(tokens []FastcExpressionToken) ?int {
	if tokens.len == 1 && tokens[0].tok == .number {
		return tokens[0].lit.int()
	}
	if tokens.len == 2 && tokens[0].tok in [.plus, .minus] && tokens[1].tok == .number {
		value := tokens[1].lit.int()
		return if tokens[0].tok == .minus { -value } else { value }
	}
	return none
}

fn fastc_render_enum_value_expression(tokens []FastcExpressionToken, source_file FastcSourceFile, enum_c_name string, fields map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool) !string {
	value, next := fastc_render_enum_binary_expression(tokens, 0, 1, source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
	if next != tokens.len {
		return error('fastc parser does not support enum discriminant `${fastc_expression_tokens_debug(tokens)}` in ${source_file.path}')
	}
	return value
}

fn fastc_render_enum_binary_expression(tokens []FastcExpressionToken, start int, minimum_precedence int, source_file FastcSourceFile, enum_c_name string, fields map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool) !(string, int) {
	initial_left, initial_next := fastc_render_enum_unary_expression(tokens, start, source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
	mut left := initial_left
	mut next := initial_next
	for next < tokens.len {
		operator := tokens[next].tok
		precedence := int(operator.left_binding_power())
		if precedence < minimum_precedence || operator !in [.plus, .minus, .mul, .div, .mod, .pipe,
			.xor, .amp, .left_shift, .right_shift] {
			break
		}
		right, after_right := fastc_render_enum_binary_expression(tokens, next + 1, int(operator.right_binding_power()), source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
		left = '((${left}) ${operator.str()} (${right}))'
		next = after_right
	}
	return left, next
}

fn fastc_render_enum_unary_expression(tokens []FastcExpressionToken, start int, source_file FastcSourceFile, enum_c_name string, fields map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool) !(string, int) {
	if start >= tokens.len {
		return error('fastc parser does not support an incomplete enum discriminant in ${source_file.path}')
	}
	if tokens[start].tok in [.plus, .minus, .bit_not] {
		value, next := fastc_render_enum_unary_expression(tokens, start + 1, source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
		return '(${tokens[start].tok.str()}(${value}))', next
	}
	if tokens[start].tok == .lpar {
		value, next := fastc_render_enum_binary_expression(tokens, start + 1, 1, source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
		if next >= tokens.len || tokens[next].tok != .rpar {
			return error('fastc parser does not support an unbalanced enum discriminant in ${source_file.path}')
		}
		return '(${value})', next + 1
	}
	// A primitive cast like `int(http.Status.found)`: enum values are plain C
	// integers, so render `((int)(<inner>))`.
	if tokens[start].tok == .name && start + 1 < tokens.len && tokens[start + 1].tok == .lpar {
		if c_type := fastc_primitive_c_type(tokens[start].lit) {
			value, next := fastc_render_enum_binary_expression(tokens, start + 2, 1, source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
			if next >= tokens.len || tokens[next].tok != .rpar {
				return error('fastc parser does not support an unbalanced enum discriminant in ${source_file.path}')
			}
			return '((${c_type})(${value}))', next + 1
		}
	}
	if tokens[start].tok == .number {
		return fastc_c_number(tokens[start].lit)!, start + 1
	}
	return fastc_render_enum_symbol(tokens, start, source_file, enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)
}

fn fastc_render_enum_symbol(tokens []FastcExpressionToken, start int, source_file FastcSourceFile, enum_c_name string, fields map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool) !(string, int) {
	if tokens[start].tok == .dot && start + 1 < tokens.len && tokens[start + 1].tok == .name && tokens[start + 1].lit in fields {
		return '${enum_c_name}__${tokens[start + 1].lit}', start + 2
	}
	if tokens[start].tok != .name {
		return error('fastc parser does not support enum discriminant token `${tokens[start].tok.str()}` in ${source_file.path}')
	}
	name := tokens[start].lit
	if start + 4 < tokens.len && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name && tokens[start + 3].tok == .dot && tokens[start + 4].tok == .name {
		if imported_module := source_file.header.imports[name] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if declared_types[type_key] && declared_kinds[type_key] == .enum_ {
				return '${fastc_c_declared_type_name(type_key)}__${tokens[start + 4].lit}', start + 5
			}
		}
	}
	if start + 2 < tokens.len && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name {
		member := tokens[start + 2].lit
		if name == 'C' {
			return member, start + 3
		}
		if imported_module := source_file.header.imports[name] {
			constant_key := fastc_constant_key(imported_module, member)
			if c_name := constants[constant_key] {
				if !public_constants[constant_key] {
					return error('fastc parser does not support private imported constant `${name}.${member}` in enum discriminant in ${source_file.path}')
				}
				return c_name, start + 3
			}
		}
		type_key := fastc_resolve_declared_type_key(source_file.header.module_name, name, source_file.header.imports, declared_types) or { '' }
		if type_key != '' && declared_kinds[type_key] == .enum_ {
			return '${fastc_c_declared_type_name(type_key)}__${member}', start + 3
		}
	}
	if name in fields {
		return '${enum_c_name}__${name}', start + 1
	}
	local_key := fastc_constant_key(source_file.header.module_name, name)
	builtin_key := fastc_constant_key('builtin', name)
	if c_name := constants[local_key] {
		return c_name, start + 1
	}
	if c_name := constants[builtin_key] {
		return c_name, start + 1
	}
	if imported_module := source_file.header.imports[fastc_selective_import_key(name)] {
		constant_key := fastc_constant_key(imported_module, name)
		if c_name := constants[constant_key] {
			if public_constants[constant_key] {
				return c_name, start + 1
			}
		}
	}
	return error('fastc parser does not support unresolved enum discriminant name `${name}` in ${source_file.path}')
}

fn fastc_emit_enum_print_function(c_name string, name string, fields []string, is_flag bool, mut out strings.Builder) {
	out.writeln('void v_fastc_print_enum_${c_name}(${c_name} value, bool newline) {')
	if is_flag {
		out.writeln('\tfputs("${name}{", stdout);')
		out.writeln('\tbool written = false;')
		for field in fields {
			out.writeln('\tif ((value & ${c_name}__${field}) == ${c_name}__${field}) {')
			out.writeln('\t\tif (written) fputs(" | ", stdout);')
			out.writeln('\t\tfputs(".${field}", stdout);')
			out.writeln('\t\twritten = true;')
			out.writeln('\t}')
		}
		out.writeln('\tfputc(125, stdout);')
	} else {
		if fields.len == 0 {
			out.writeln('\tfputs("unknown enum value", stdout);')
		} else {
			for i, field in fields {
				out.writeln('\t${if i == 0 { 'if' } else { 'else if' }} (value == ${c_name}__${field}) fputs("${field}", stdout);')
			}
			out.writeln('\telse fputs("unknown enum value", stdout);')
		}
	}
	out.writeln('\tif (newline) fputc(10, stdout);')
	out.writeln('}')
}

// fastc_generate_enum_string_helpers renders the `str` and print helper of
// every enum as separate pieces (parallel name and text lists), so the
// stitch can leave out the helpers nothing reachable calls.
fn fastc_generate_enum_string_helpers(infos []FastcEnumInfo) ([]string, []string) {
	mut names := []string{cap: infos.len * 2}
	mut texts := []string{cap: infos.len * 2}
	for info in infos {
		mut out := strings.new_builder(256)
		out.writeln('string v_fastc_enum_str_${info.c_name}(${info.c_name} value) {')
		if info.is_flag {
			out.writeln('\tstring parts[${info.fields.len * 2 + 2}] = {0};')
			out.writeln('\tint part_count = 0;')
			out.writeln('\tbool written = false;')
			out.writeln('\tparts[part_count++] = _S("${info.name}{");')
			for field in info.fields {
				out.writeln('\tif ((value & ${info.c_name}__${field}) == ${info.c_name}__${field}) {')
				out.writeln('\t\tif (written) parts[part_count++] = _S(" | ");')
				out.writeln('\t\tparts[part_count++] = _S(".${field}");')
				out.writeln('\t\twritten = true;')
				out.writeln('\t}')
			}
			out.writeln('\tparts[part_count++] = _S("}");')
			out.writeln('\treturn builtin__string_plus_many(part_count, parts);')
		} else {
			for field in info.fields {
				out.writeln('\tif (value == ${info.c_name}__${field}) return _S("${field}");')
			}
			out.writeln('\treturn _S("unknown enum value");')
		}
		out.writeln('}')
		out.writeln('')
		names << 'v_fastc_enum_str_${info.c_name}'
		texts << out.str()
		mut print_out := strings.new_builder(256)
		fastc_emit_enum_print_function(info.c_name, info.name, info.fields, info.is_flag, mut print_out)
		print_out.writeln('')
		names << 'v_fastc_print_enum_${info.c_name}'
		texts << print_out.str()
		mut from_out := strings.new_builder(256)
		// `Enum.from_string(s)` maps a field-name string back to its value, returning
		// `?Enum` (none when no field name matches).
		from_out.writeln('Option ${info.c_name}__from_string(string __v_fastc_from) {')
		for field in info.fields {
			from_out.writeln('\tif (builtin__string_eq(__v_fastc_from, _S("${field}"))) { ${info.c_name} __v_fastc_value = ${info.c_name}__${field}; return (Option){.data=v_fastc_interface_box(&__v_fastc_value, sizeof(${info.c_name})), .state=0}; }')
		}
		from_out.writeln('\treturn (Option){.state=2};')
		from_out.writeln('}')
		from_out.writeln('')
		names << '${info.c_name}__from_string'
		texts << from_out.str()
	}
	return names, texts
}

fn fastc_emit_interface_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support interface declaration in ${source_file.path}')
	}
	name := scan.lit
	c_name := fastc_c_declared_type_name(fastc_type_key(source_file.header.module_name, name))
	tok = scan.scan()
	if tok != .lcbr {
		return error('fastc parser does not support interface `${name}` body in ${source_file.path}')
	}
	tok = fastc_skip_balanced_tokens(mut scan, tok, .lcbr, .rcbr)!
	out.writeln('struct ${c_name} { void *_object; u32 _typ; void *_methods; };')
	out.writeln('')
	return tok
}

fn fastc_emit_alias_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, allow_short_placeholders bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut alias_base_types map[string]string, mut fn_alias_return_types map[string]string, mut sum_types map[string]bool, mut sum_type_variants map[string]bool, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support type alias in ${source_file.path}')
	}
	name := scan.lit
	key := fastc_type_key(source_file.header.module_name, name)
	c_name := fastc_c_declared_type_name(key)
	tok = scan.scan()
	if name == 'C' && tok == .dot {
		return fastc_skip_type_declaration(mut scan, tok)
	}
	if tok == .lsbr {
		// Generic type alias like `Foo[T] = ...`: skip the type parameters. The body
		// is emitted with each parameter resolved to a `voidptr` placeholder.
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished generic type `${name}` in ${source_file.path}')
			}
			if tok == .lsbr {
				depth++
			} else if tok == .rsbr {
				depth--
			}
		}
		tok = scan.scan()
	}
	if tok != .assign {
		return error('fastc parser does not support type `${name}` declaration in ${source_file.path}')
	}
	tok = scan.scan()
	if tok == .key_fn {
		return fastc_emit_function_alias(mut scan, source_file, declared_types, allow_short_placeholders, c_name, mut fn_alias_return_types, mut out)
	}
	base, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders) or { return error('fastc type `${name}`: ${err.msg()}') }
	tok = next_token
	if tok == .semicolon {
		tok = scan.scan()
	}
	if tok == .pipe {
		// Record each variant's C type (keyed `<sum type>|<variant>`) so an append can
		// tell push-many (`[]T << []T`) from boxing an array-valued variant of a
		// recursive sum type (`type Value = []Value | int`) as one element. `base` is
		// the first variant, already scanned above; the rest follow each `|`.
		sum_type_variants['${c_name}|${base.trim_right('*')}'] = true
		for tok == .pipe {
			tok = scan.scan()
			variant, variant_next := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders) or { return error('fastc type `${name}`: ${err.msg()}') }
			sum_type_variants['${c_name}|${variant.trim_right('*')}'] = true
			tok = variant_next
			if tok == .semicolon {
				tok = scan.scan()
			}
		}
		// A sum type is lowered to the same boxed representation as an interface, so
		// construction (variant boxing) and `match` (dispatch on `_typ`) reuse the
		// interface machinery. The `_methods` slot is unused but keeps the layout
		// identical so `interface_value_expression` applies unchanged.
		out.writeln('typedef struct { void *_object; u32 _typ; void *_methods; } ${c_name};')
		sum_types[c_name] = true
	} else if fastc_primitive_c_type(name) == none && declared_kinds[key] == .alias_ {
		out.writeln('typedef ${base} ${c_name};')
		alias_base_types[c_name] = base
		mut layout_type := base.trim_right('*')
		if layout_type.starts_with('Array_') {
			layout_type = 'array'
		} else if layout_type.starts_with('Map_') {
			layout_type = 'map'
		}
		mut alias_fields := map[string]string{}
		mut alias_field_info := []FastcStructField{}
		if layout_type in struct_fields {
			alias_fields = struct_fields[layout_type].clone()
		}
		if layout_type in struct_field_info {
			alias_field_info = struct_field_info[layout_type].clone()
		}
		if base.starts_with('Array_') {
			alias_fields['__fastc_element_type'] = base['Array_'.len..]
		}
		if alias_fields.len > 0 {
			struct_fields[c_name] = alias_fields.clone()
		}
		if alias_field_info.len > 0 {
			struct_field_info[c_name] = alias_field_info.clone()
		}
	}
	return tok
}

fn fastc_emit_function_alias(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, allow_short_placeholders bool, c_name string, mut fn_alias_return_types map[string]string, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type `${c_name}` in ${source_file.path}')
	}
	tok = scan.scan()
	mut parameter_types := []string{}
	for tok != .rpar {
		if tok in [.comma, .semicolon] {
			tok = scan.scan()
			continue
		}
		mut parameter_is_mut := false
		if tok in [.key_mut, .key_shared] {
			parameter_is_mut = true
			tok = scan.scan()
		}
		mut has_parameter_name := false
		if tok == .name {
			mut lookahead := scan
			next_token := lookahead.scan()
			has_parameter_name = next_token in [.name, .amp, .and, .mul, .question, .not, .key_fn,
				.lsbr]
		}
		if has_parameter_name {
			tok = scan.scan()
			parameter_type, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
			parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
				parameter_type + '*'
			} else {
				parameter_type
			}
			tok = next_token
		} else {
			parameter_type, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
			parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
				parameter_type + '*'
			} else {
				parameter_type
			}
			tok = next_token
		}
	}
	tok = scan.scan()
	mut return_type := 'void'
	if tok in [.not, .question] {
		// A result/option return (`fn (...) !`, `fn (...) ?Type`) lowers to FastC's
		// fixed `Option` value; consume any concrete value type after it.
		return_type = 'Option'
		tok = scan.scan()
		if tok in [.name, .amp, .and, .mul, .lsbr, .key_fn, .question, .not] {
			_, tok = fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
		}
	} else if tok !in [.semicolon, .eof] {
		return_type, tok = fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
	}
	out.writeln('typedef ${return_type} (*${c_name})(${if parameter_types.len == 0 {
		'void'
	} else {
		parameter_types.join(', ')
	}});')
	// Record the alias's return type so a call through a value of this fn type
	// (`resolver(path)` where `resolver SomeFnAlias`) can infer its result type.
	fn_alias_return_types[c_name] = return_type
	if tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}
