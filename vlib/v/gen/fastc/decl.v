module fastc

import v.flat
import os
import strings
import time
import v.pref
import v.scanner
import v.token

fn collect_declared_types(source string, path string, module_name string, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut declaration_paths map[string]string, mut declaration_modules map[string]string, mut type_source strings.Builder, mut source_origins []FastcSourceOrigin, mut body_spans []int) !bool {
	mut scratch_body_spans := []int{}
	mut scratch_source_origins := []FastcSourceOrigin{}
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
		if brace_depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if brace_depth == 0 && tok in [.key_module, .key_import] {
			tok = fastc_skip_module_or_import(mut scan, tok, path)!
			previous_tok = .unknown
			continue
		}
		if brace_depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_types := strings.new_builder(256)
					selected_has_types := collect_declared_types(selected.source, path, module_name, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut declaration_paths, mut declaration_modules, mut selected_types, mut scratch_source_origins, mut scratch_body_spans)!
					has_type_declarations = has_type_declarations || selected_has_types
					if selected_types.len > 0 {
						start := if pending_start >= 0 { pending_start } else { comptime_start }
						fastc_append_filtered_source_span(source, emitted_end, start, comptime_end, mut type_source, mut source_origins)
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
					if tok == .name {
						key := if next_c_struct_is_typedef {
							'#Ctypedef#${scan.lit}'
						} else {
							'#Cstruct#${scan.lit}'
						}
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
			if brace_depth == 0 && pending_fn && declaration_start < 0 {
				// Only this function body's bounds are needed by the declaration
				// index. The scanner still owns strings, comments and punctuation.
				fn_body_start = scan.pos
				tok = fastc_skip_header_block(mut scan)
				if tok == .rcbr {
					body_spans << fn_body_start
					body_spans << scan.offset
					fn_body_start = -1
					pending_fn = false
					previous_tok = .rcbr
					tok = scan.scan()
				}
				continue
			}
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
				fastc_append_filtered_source_span(source, emitted_end, declaration_start, scan.offset, mut type_source, mut source_origins)
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
			fastc_append_filtered_source_span(source, emitted_end, declaration_start, declaration_end, mut type_source, mut source_origins)
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
		fastc_append_filtered_source_span(source, emitted_end, declaration_start, source.len, mut type_source, mut source_origins)
	}
	return has_type_declarations
}

fn collect_constant_names(source string, path string, module_name string, prefs &pref.Preferences, skips []int, mut constants map[string]string, mut public_constants map[string]bool, mut constant_paths map[string]string, mut constant_source strings.Builder, mut constant_spans []int, mut source_origins []FastcSourceOrigin) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut previous_tok := token.Token.unknown
	mut emitted_end := 0
	mut skip_index := 0
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if brace_depth == 0 && tok in [.key_module, .key_import] {
			tok = fastc_skip_module_or_import(mut scan, tok, path)!
			previous_tok = .unknown
			continue
		}
		if brace_depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_constants := strings.new_builder(256)
					mut selected_spans := []int{}
					mut selected_origins := []FastcSourceOrigin{}
					collect_constant_names(selected.source, path, module_name, prefs, []int{}, mut constants, mut public_constants, mut constant_paths, mut selected_constants, mut selected_spans, mut selected_origins)!
					if selected_constants.len > 0 {
						constant_spans << constant_source.len

						fastc_append_filtered_source_span(source, emitted_end, comptime_start, comptime_end, mut constant_source, mut source_origins)

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

						fastc_append_filtered_source_span(source, emitted_end, declaration_start, scan.offset, mut constant_source, mut source_origins)

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

					fastc_append_filtered_source_span(source, emitted_end, declaration_start, declaration_end, mut constant_source, mut source_origins)

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

				fastc_append_filtered_source_span(source, emitted_end, declaration_start, source.len, mut constant_source, mut source_origins)

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
fn fastc_append_filtered_source_span(source string, previous_end int, start int, end int, mut out strings.Builder, mut source_origins []FastcSourceOrigin) {
	mut last_newline := -1
	for i in previous_end .. start {
		if source[i] == `\n` {
			out.write_u8(`\n`)
			last_newline = i
		}
	}
	column_start := if last_newline >= 0 { last_newline + 1 } else { previous_end }
	out.write_repeated_rune(` `, start - column_start)
	source_origins << FastcSourceOrigin{
		filtered_offset: out.len
		source_offset:   start
	}
	out.write_string(source[start..end])
}

fn fastc_source_origin_index(origins []FastcSourceOrigin, offset int) int {
	mut low := 0
	mut high := origins.len
	for low < high {
		mid := low + (high - low) / 2
		if origins[mid].filtered_offset <= offset {
			low = mid + 1
		} else {
			high = mid
		}
	}
	return low - 1
}

fn fastc_original_source_offset(origins []FastcSourceOrigin, offset int) int {
	index := fastc_source_origin_index(origins, offset)
	if index < 0 {
		return offset
	}
	origin := origins[index]
	return origin.source_offset + offset - origin.filtered_offset
}

// fastc_source_range_origins preserves every mapping transition inside a copied range.
fn fastc_source_range_origins(source_file FastcSourceFile, start int, end int, destination int) []FastcSourceOrigin {
	if start >= end {
		return []FastcSourceOrigin{}
	}
	base := source_file.source_offset + start
	mut mapped := [FastcSourceOrigin{
		filtered_offset: destination
		source_offset:   fastc_original_source_offset(source_file.source_origins, base)
	}]
	for i := fastc_source_origin_index(source_file.source_origins, base) + 1; i < source_file.source_origins.len; i++ {
		origin := source_file.source_origins[i]
		if origin.filtered_offset >= source_file.source_offset + end {
			break
		}
		mapped << FastcSourceOrigin{
			filtered_offset: destination + origin.filtered_offset - base
			source_offset:   origin.source_offset
		}
	}
	return mapped
}

fn fastc_declaration_source(source_file FastcSourceFile, source string, origins []FastcSourceOrigin) FastcDeclarationSource {
	if source_file.source_offset == 0 && source_file.source_origins.len == 0 {
		return FastcDeclarationSource{ source: source, origins: origins }
	}
	mut mapped := []FastcSourceOrigin{cap: origins.len}
	for i, origin in origins {
		end := if i + 1 < origins.len { origins[i + 1].filtered_offset } else { source.len }
		mapped << fastc_source_range_origins(source_file, origin.source_offset,
			origin.source_offset + end - origin.filtered_offset, origin.filtered_offset)
	}
	return FastcDeclarationSource{ source: source, origins: mapped }
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
fn collect_global_names(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, mut globals map[string]string, mut public_globals map[string]bool, mut global_paths map[string]string, mut global_source strings.Builder, mut source_origins []FastcSourceOrigin) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut depth := 0
	mut previous_tok := token.Token.unknown
	mut previous_pos := 0
	mut emitted_end := 0
	mut attribute_start := -1
	mut export_name := ''
	// A single `__global name = value` declaration runs to the next top-level
	// statement end; its span is closed there.
	mut pending_start := -1
	mut skip_index := 0
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && tok == .attribute {
			if attribute_start < 0 {
				attribute_start = scan.pos
			}
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			if attribute.export_name != '' {
				export_name = attribute.export_name
			}
			tok = attribute.tok
			continue
		}
		if depth == 0 && tok in [.key_fn, .key_const, .key_struct, .key_union, .key_enum,
			.key_interface, .key_type] {
			attribute_start = -1
			export_name = ''
		}
		if depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if depth == 0 && tok in [.key_module, .key_import] {
			tok = fastc_skip_module_or_import(mut scan, tok, path)!
			continue
		}
		if depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				globals_before := globals.len
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_source := strings.new_builder(64)
					mut selected_origins := []FastcSourceOrigin{}
					collect_global_names(selected.source, path, header, prefs, []int{}, mut globals, mut public_globals, mut global_paths, mut selected_source, mut selected_origins)!
				}
				if globals.len > globals_before {
					fastc_append_filtered_source_span(source, emitted_end, comptime_start, comptime_end, mut global_source, mut source_origins)
					emitted_end = comptime_end
				}
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if depth == 0 && tok == .key_global {
			is_public := previous_tok == .key_pub
			declaration_start := if attribute_start >= 0 {
				attribute_start
			} else if is_public {
				previous_pos
			} else {
				scan.pos
			}
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut boundaries := []int{}
				for tok != .rpar && tok != .eof {
					if tok in [.semicolon, .key_volatile] {
						tok = scan.scan()
						continue
					}
					if tok == .name {
						if scan.lit != 'C' {
							fastc_register_global(header, scan.lit, is_public, path, prefs, mut globals, mut public_globals, mut global_paths)!
							if export_name != '' {
								globals[fastc_global_key(header.module_name, scan.lit)] = export_name
							}
						}
						tok = fastc_skip_global_declaration(mut scan, path)!
						if tok in [.name, .key_volatile] {
							boundaries << scan.pos
						}
						continue
					}
					tok = scan.scan()
				}
				declaration_end := if tok == .rpar { scan.offset } else { scan.pos }
				fastc_append_grouped_global_source(source, emitted_end, declaration_start,
					declaration_end, boundaries, mut global_source, mut source_origins)
				emitted_end = declaration_end
				attribute_start = -1
				export_name = ''
				continue
			}
			if tok == .key_volatile {
				tok = scan.scan()
			}
			if tok == .name && scan.lit != 'C' {
				fastc_register_global(header, scan.lit, is_public, path, prefs, mut globals, mut public_globals, mut global_paths)!
				if export_name != '' {
					globals[fastc_global_key(header.module_name, scan.lit)] = export_name
				}
			}
			pending_start = declaration_start
			attribute_start = -1
			export_name = ''
			continue
		}
		if pending_start >= 0 && depth == 0 && tok == .semicolon {
			fastc_append_filtered_source_span(source, emitted_end, pending_start, scan.pos, mut global_source, mut source_origins)
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
		fastc_append_filtered_source_span(source, emitted_end, pending_start, source.len, mut global_source, mut source_origins)
	}
}

fn fastc_append_grouped_global_source(source string, previous_end int, start int, end int, boundaries []int, mut out strings.Builder, mut source_origins []FastcSourceOrigin) {
	mut cursor := start
	for boundary in boundaries {
		// Replace separating whitespace to keep pseudo-variable columns intact.
		separator := if boundary > cursor && source[boundary - 1] in [` `, `\t`] {
			boundary - 1
		} else {
			boundary
		}
		if cursor == start {
			fastc_append_filtered_source_span(source, previous_end, start, separator, mut out, mut source_origins)
		} else {
			source_origins << FastcSourceOrigin{
				filtered_offset: out.len
				source_offset:   cursor
			}
			out.write_string(source[cursor..separator])
		}
		out.write_u8(`;`)
		cursor = boundary
	}
	if cursor == start {
		fastc_append_filtered_source_span(source, previous_end, start, end, mut out, mut source_origins)
	} else {
		source_origins << FastcSourceOrigin{
			filtered_offset: out.len
			source_offset:   cursor
		}
		out.write_string(source[cursor..end])
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
	type_sources        map[string]FastcDeclarationSource
	declaration_paths   map[string]string
	declaration_modules map[string]string
	constants           map[string]string
	public_constants    map[string]bool
	constant_paths      map[string]string
	constant_sources    map[string]FastcDeclarationSource
	constant_spans      map[string][]int
	global_sources      map[string]FastcDeclarationSource
	body_spans          map[string][]int
	globals             map[string]string
	public_globals      map[string]bool
	global_paths        map[string]string
	failed              bool
	error_message       string
}

fn fastc_collect_declaration_chunk(sources []FastcSourceFile, prefs &pref.Preferences, start int, end int) FastcDeclarationPartial {
	mut partial := FastcDeclarationPartial{
		declared_types:      map[string]bool{}
		declared_kinds:      map[string]FastcDeclaredTypeKind{}
		enum_flags:          map[string]bool{}
		params_structs:      map[string]bool{}
		type_source_paths:   map[string]bool{}
		type_sources:        map[string]FastcDeclarationSource{}
		declaration_paths:   map[string]string{}
		declaration_modules: map[string]string{}
		constants:           map[string]string{}
		public_constants:    map[string]bool{}
		constant_paths:      map[string]string{}
		constant_sources:    map[string]FastcDeclarationSource{}
		globals:             map[string]string{}
		public_globals:      map[string]bool{}
		global_paths:        map[string]string{}
	}
	for idx in start .. end {
		source_file := sources[idx]
		mut type_source := strings.new_builder(256)
		mut type_origins := []FastcSourceOrigin{}
		mut has_type_declarations := false
		mut body_spans := []int{}
		if source_file.header.has_type_keywords {
			has_type_declarations = collect_declared_types(source_file.source, source_file.path, source_file.header.module_name, prefs, mut partial.declared_types, mut partial.declared_kinds, mut partial.enum_flags, mut partial.params_structs, mut partial.declaration_paths, mut partial.declaration_modules, mut type_source, mut type_origins, mut body_spans) or {
				partial.failed = true
				partial.error_message = err.msg()
				return partial
			}
		}
		if has_type_declarations {
			partial.type_source_paths[source_file.path] = true
			partial.type_sources[source_file.path] = fastc_declaration_source(source_file, type_source.str(), type_origins)
		}
		if source_file.header.has_constants {
			mut constant_source := strings.new_builder(256)
			mut constant_origins := []FastcSourceOrigin{}
			mut constant_spans := []int{}
			collect_constant_names(source_file.source, source_file.path, source_file.header.module_name, prefs, body_spans, mut partial.constants, mut partial.public_constants, mut partial.constant_paths, mut constant_source, mut constant_spans, mut constant_origins) or {
				partial.failed = true
				partial.error_message = err.msg()
				return partial
			}
			if constant_source.len > 0 {
				partial.constant_sources[source_file.path] = fastc_declaration_source(source_file, constant_source.str(), constant_origins)
				partial.constant_spans[source_file.path] = constant_spans
			}
		}
		if source_file.header.has_global_declarations {
			mut global_source := strings.new_builder(256)
			mut global_origins := []FastcSourceOrigin{}
			collect_global_names(source_file.source, source_file.path, source_file.header, prefs, body_spans, mut partial.globals, mut partial.public_globals, mut partial.global_paths, mut global_source, mut global_origins) or {
				partial.failed = true
				partial.error_message = err.msg()
				return partial
			}
			if global_source.len > 0 {
				partial.global_sources[source_file.path] = fastc_declaration_source(source_file, global_source.str(), global_origins)
			}
		}
		if body_spans.len > 0 {
			partial.body_spans[source_file.path] = body_spans
		}
	}
	return partial
}

// fastc_apply_declaration_body_spans preserves the body bounds collected by
// the declaration pass for the signature and interface passes that follow.
fn fastc_apply_declaration_body_spans(mut sources []FastcSourceFile, body_spans map[string][]int, start int, end int) {
	for index in start .. end {
		source_file := sources[index]
		sources[index] = FastcSourceFile{
			...source_file
			header: fastc_header_with_body_spans(source_file.header, body_spans[source_file.path] or { []int{} })
		}
	}
}

fn fastc_merge_declaration_partial(partial FastcDeclarationPartial, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]FastcDeclarationSource, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]FastcDeclarationSource, mut constant_spans map[string][]int, mut global_sources map[string]FastcDeclarationSource, mut globals map[string]string, mut public_globals map[string]bool) ! {
	if partial.failed {
		return error(partial.error_message)
	}
	for key, is_public in partial.declared_types {
		if key in declared_types && !key.starts_with('#Cstruct#') && !key.starts_with('#Ctypedef#') {
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

fn fastc_generate_global_declarations(ordered_sources []FastcSourceFile, global_sources map[string]FastcDeclarationSource, prefs &pref.Preferences, header_free bool, declared_types map[string]bool, declared_type_c_names flat.TypeTextBindings, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types flat.TypeTextBindings, alias_base_types flat.TypeTextBindings, struct_fields map[string]flat.TypeTextBindings, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, function_c_names map[string]string, constants map[string]string, constant_values map[string]string, public_constants map[string]bool, constant_types flat.TypeTextBindings, globals map[string]string, public_globals map[string]bool, mut global_types flat.TypeTextBindings) !FastcGlobalDeclarations {
	declared_type_key_by_name := fastc_declared_type_key_by_name(declared_types)
	mut out := strings.new_builder(1024)
	mut module_initializers := map[string]string{}
	mut uses_pthread_tls := false
	mut composite_types := map[string]bool{}
	mut fixed_array_types := flat.TypeTextBindings{}
	helper_has_c_functions := fastc_functions_declare_c(functions)
	for source_file in ordered_sources {
		global_source := global_sources[source_file.path] or { continue }
		mut initializers := strings.new_builder(256)
		file := token.File.unindexed(source_file.path, global_source.source.len)
		mut gen := Parser{
			prefs:                        unsafe { prefs }
			unqualified_key_memo:         map[string]string{}
			global_key_memo:              map[string]string{}
			c_function_name_memo:         map[string]string{}
			nonlocal_name_type_memo:      flat.TypeTextBindings{}
			resolved_name_memo:           map[string]string{}
			declared_type_key_memo:       map[string]FastcMemoEntry{}
			path:                         source_file.path
			source_origins:               global_source.origins
			translated:                   source_file.header.translated
			module_name:                  source_file.header.module_name
			imports:                      source_file.header.imports
			declared_types:               declared_types
			declared_type_c_names:        declared_type_c_names
			declared_type_key_by_name:    declared_type_key_by_name
			fastc_prefixed_c_names:       fastc_prefixed_c_names
			declaration_initializer_mode: true
			has_c_functions:              helper_has_c_functions
			comparison_memo:              map[i64]FastcRenderedExpression{}
			type_memo:                    map[i64]u32{}
			method_key_memo:              map[u32]map[string]string{}
			field_memo:                   map[u32]map[string]&FastcStructField{}
			spawn_typedefs:               map[string]string{}
			spawn_helpers:                map[string]string{}
			thread_value_types:           flat.TypeTextBindings{}
			declared_kinds:               declared_kinds
			enum_flags:                   enum_flags
			enum_field_types:             enum_field_types
			alias_base_types:             alias_base_types
			struct_fields:                struct_fields
			struct_field_info:            struct_field_info
			constants:                    constants
			constant_values:              constant_values
			public_constants:             public_constants
			globals:                      globals
			public_globals:               public_globals
			selfhost:                     prefs.building_v
			header_free:                  header_free
			s:                            scanner.new_scanner(prefs, .normal)
			out:                          strings.new_builder(0)
			protos:                       strings.new_builder(0)
			functions:                    functions
			function_c_names:             function_c_names
			constant_types:               constant_types
			global_types:                 global_types
			composite_types:              map[string]bool{}
			fixed_array_types:            flat.TypeTextBindings{}
		}
		gen.s.init(file, global_source.source)
		gen.next()
		gen.parse_selected_global_declarations(mut out, mut initializers, false)!
		uses_pthread_tls = uses_pthread_tls || gen.uses_pthread_tls
		global_types = gen.global_types.move()
		for name, _ in gen.composite_types {
			composite_types[name] = true
		}
		for name, array_type_id in gen.fixed_array_types.ids {
			fixed_array_types.set_id(name, array_type_id)
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
		declarations:        out.str()
		uses_pthread_tls:    uses_pthread_tls
		module_initializers: module_initializers
		composite_types:     composite_types
		fixed_array_types:   fixed_array_types
	}
}

fn (mut g Parser) parse_selected_global_declarations(mut out strings.Builder, mut initializers strings.Builder, stop_at_block_end bool) ! {
	mut attributes := FastcDeclarationAttribute{ is_enabled: true }
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
		if g.tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut g.s, g.path, g.prefs)!
			g.tok = attribute.tok
			g.lit = g.s.lit
			attributes = fastc_merge_declaration_attributes(attributes, attribute)
			continue
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
			if attributes.is_enabled {
				g.parse_global_declaration(attributes, mut out, mut initializers)!
			} else {
				g.skip_top_level_declaration()!
			}
			attributes = FastcDeclarationAttribute{ is_enabled: true }
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
// uses a pthread-key slot on macOS and Linux under TinyCC, which lacks usable
// language-level thread-local storage. Other compilers use their native TLS.
fn fastc_write_prealloc_tls_global(mut out strings.Builder, styp string, c_name string, header_free bool) {
	out.writeln('#if defined(__TINYC__) && (defined(__APPLE__) || defined(__linux__))')
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

fn (mut g Parser) parse_global_declaration(attributes FastcDeclarationAttribute, mut out strings.Builder, mut initializers strings.Builder) ! {
	g.expect(.key_global)!
	if g.tok == .lpar {
		// `__global ( a int  b = 1 )` declares each entry as its own `__global`.
		g.next()
		g.skip_semicolons()
		for g.tok != .rpar {
			if g.tok == .eof {
				return g.unsupported('unterminated grouped globals')
			}
			g.parse_one_global_declaration(attributes, mut out, mut initializers)!
			g.skip_semicolons()
		}
		g.next()
		g.skip_semicolons()
		return
	}
	g.parse_one_global_declaration(attributes, mut out, mut initializers)!
}

fn (mut g Parser) parse_one_global_declaration(attributes FastcDeclarationAttribute, mut out strings.Builder, mut initializers strings.Builder) ! {
	is_volatile := g.tok == .key_volatile
	if is_volatile {
		g.next()
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
	storage := if attributes.export_name == '' { 'static ' } else { '' }
	qualifier := if is_volatile { 'volatile ' } else { '' }
	decoration := fastc_global_c_attributes(attributes)
	g.c_global_static_initializer = attributes.is_cinit
	g.next()
	if g.tok == .assign {
		g.next()
		checkpoint := g
		mut initializer := g.read_expression([token.Token.semicolon, .rpar])!
		typ := fastc_normalize_inferred_type(g.last_expression_type)
		if typ == 0 {
			return g.unsupported('unverifiable global `${name}` type')
		}
		mut static_initializer := attributes.is_cinit
		if !static_initializer {
			if value := g.target_static_global_initializer(&checkpoint, initializer) {
				initializer = value
				static_initializer = true
			}
		}
		if declaration := fastc_fixed_array_field_declaration(typ, c_name) {
			if static_initializer {
				out.writeln('${storage}${qualifier}${declaration}${decoration} = ${fastc_static_fixed_array_initializer(initializer)};')
			} else {
				out.writeln('${storage}${qualifier}${declaration}${decoration};')
				initializers.writeln('\tmemcpy(${c_name}, (${initializer}).data, sizeof(${c_name}));')
			}
		} else if static_initializer {
			out.writeln('${storage}${qualifier}${fastc_output_c_type(flat.source_type_name(typ))} ${c_name}${decoration} = ${initializer};')
		} else {
			out.writeln('${storage}${qualifier}${fastc_output_c_type(flat.source_type_name(typ))} ${c_name}${decoration};')
		}
		if !static_initializer && !flat.source_type_name(typ).starts_with('FixedArray_')
			&& !(g.selfhost && key in ['g_main_argc', 'g_main_argv']) {
			initializers.writeln('\t${c_name} = ${initializer};')
		}
		g.global_types.set_id(key, typ)
		g.skip_semicolons()
		return
	}
	typ := g.parse_type()!
	is_function_pointer := g.pending_fn_pointer
	fn_return_type := g.pending_fn_return_type
	fn_parameters := g.pending_fn_parameters
	declaration_start := out.len
	if is_function_pointer {
		out.writeln('${storage}${fastc_output_c_type(flat.source_type_name(fn_return_type))} (*${qualifier}${c_name})(${fn_parameters})${decoration};')
	} else if g.selfhost && name == 'g_memory_block' && fastc_prealloc_enabled(g.prefs) {
		// The prealloc arena root must be per-thread; a shared global would be
		// corrupted by the parallel per-file generator's concurrent bump-allocations.
		fastc_write_prealloc_tls_global(mut out, flat.source_type_name(typ), c_name, g.header_free)
		// TinyCC needs pthreads for the TLS slot even when the program never spawns.
		g.uses_pthread_tls = g.prefs.target.os in ['macos', 'linux']
	} else if declaration := fastc_fixed_array_field_declaration(typ, c_name) {
		// Global fixed arrays use the same raw storage as fixed-array fields.
		out.writeln('${storage}${qualifier}${declaration}${decoration};')
	} else {
		out.writeln('${storage}${qualifier}${fastc_output_c_type(flat.source_type_name(typ))} ${c_name}${decoration};')
	}
	g.global_types.set_id(key, typ)
	if g.tok != .assign && g.selfhost && !attributes.is_cinit {
		// An array or map global starts empty, not as zeroed memory.
		container := g.underlying_alias_type(typ)
		if flat.source_type_name(container).starts_with('Array_') {
			if element_type := g.array_element_type(container) {
				initializers.writeln('\t${c_name} = ((${flat.source_type_name(typ)})builtin____new_array(0, 0, sizeof(${g.collection_storage_type(element_type)})));')
			}
		} else if flat.source_type_name(container).starts_with('Map_') {
			map_default := fastc_map_field_default(container, g.prefs.target.pointer_bits, g.declared_kinds)
			if map_default != '' {
				initializers.writeln('\t${c_name} = ${map_default};')
			}
		}
	}
	if g.tok == .assign {
		g.next()
		checkpoint := g
		mut initializer := g.read_expression([token.Token.semicolon, .rpar])!
		if g.last_expression.len == 3 && g.last_expression[0].lit == 'C'
			&& g.last_expression[1].tok == .dot && g.last_expression[2].tok == .name {
			// Native aggregate initializer macros expand to braces. A cast turns
			// them into compound literals when initialization runs at startup.
			initializer = '(${flat.source_type_name(typ)})${initializer}'
		}
		mut static_initializer := attributes.is_cinit
		if !static_initializer {
			if value := g.target_static_global_initializer(&checkpoint, initializer) {
				initializer = value
				static_initializer = true
			}
		}
		if static_initializer {
			out.go_back(out.len - declaration_start)
			if is_function_pointer {
				out.writeln('${storage}${fastc_output_c_type(flat.source_type_name(fn_return_type))} (*${qualifier}${c_name})(${fn_parameters})${decoration} = ${initializer};')
			} else if declaration := fastc_fixed_array_field_declaration(typ, c_name) {
				out.writeln('${storage}${qualifier}${declaration}${decoration} = ${fastc_static_fixed_array_initializer(initializer)};')
			} else {
				out.writeln('${storage}${qualifier}${fastc_output_c_type(flat.source_type_name(typ))} ${c_name}${decoration} = ${initializer};')
			}
		} else if fastc_fixed_array_field_declaration(typ, c_name) != none {
			initializers.writeln('\tmemcpy(${c_name}, (${initializer}).data, sizeof(${c_name}));')
		} else if !(g.selfhost && key in ['g_main_argc', 'g_main_argv']) {
			initializers.writeln('\t${c_name} = ${initializer};')
		}
	}
	g.skip_semicolons()
}

// target_static_global_initializer keeps C-constant target globals available
// before _vinit, without overwriting storage changed by early native callers.
fn (g &Parser) target_static_global_initializer(checkpoint &Parser, initializer string) ?string {
	if !g.prefs.target_libc_headers || !g.global_initializer_is_constant(initializer, true) {
		return none
	}
	mut probe := *checkpoint
	probe.c_global_static_initializer = true
	value := probe.read_expression([token.Token.semicolon, .rpar]) or { return none }
	return if g.global_initializer_is_constant(value, false) { value } else { none }
}

fn (g &Parser) global_initializer_is_constant(source string, allow_temporaries bool) bool {
	if !allow_temporaries && source.contains('({') {
		return false
	}
	mut i := 0
	mut previous := u8(0)
	for i < source.len {
		ch := source[i]
		if ch in [`\"`, `\'`] {
			quote := ch
			i++
			for i < source.len {
				if source[i] == `\\` {
					i += 2
					continue
				}
				if source[i] == quote {
					i++
					break
				}
				i++
			}
			previous = quote
			continue
		}
		if ch.is_digit() {
			for i < source.len && (source[i].is_alnum() || source[i] in [`_`, `.`]) { i++ }
			previous = `0`
			continue
		}
		if !ch.is_letter() && ch != `_` {
			if !ch.is_space() { previous = ch }
			i++
			continue
		}
		start := i
		for i < source.len && (source[i].is_alnum() || source[i] == `_`) { i++ }
		name := source[start..i]
		mut next := i
		for next < source.len && source[next].is_space() { next++ }
		// The dynamic form of a struct literal copies fixed-array fields with memcpy;
		// the static re-render does not, and is checked again without temporaries.
		is_temporary_copy := allow_temporaries && name == 'memcpy'
		if next < source.len && source[next] == `(` && name !in ['sizeof', '_S']
			&& !is_temporary_copy {
			return false
		}
		if previous == `.` || name in ['NULL', 'true', 'false', 'sizeof', '_S', 'struct', 'union',
			'void', 'signed', 'unsigned', 'long', 'short', 'size_t', 'uintptr_t', 'intptr_t']
			|| fastc_primitive_c_type(name) != none || g.declared_type_c_names.contains(name)
			|| name.starts_with('FixedArray_') || '#Cstruct#${name}' in g.declared_types
			|| '#Ctypedef#${name}' in g.declared_types {
			previous = `a`
			continue
		}
		if is_temporary_copy || (allow_temporaries && (name.starts_with('__vf_')
			|| (name.starts_with('__v') && name.len > 3 && name[3].is_digit()))) {
			previous = `a`
			continue
		}
		mut known := false
		for _, c_name in g.constants {
			if c_name == name {
				if c_name !in g.constant_values { return false }
				known = true
				break
			}
		}
		for key, kind in g.declared_kinds {
			if kind == .enum_ && name.starts_with(fastc_c_declared_type_name(key) + '__') {
				known = true
				break
			}
		}
		// Global and function addresses may name declarations emitted later in
		// the C file. Leave them in _vinit unless explicitly marked cinit.
		if !known { return false }
		previous = `a`
	}
	return true
}

fn fastc_merge_declaration_attributes(left FastcDeclarationAttribute, right FastcDeclarationAttribute) FastcDeclarationAttribute {
	return FastcDeclarationAttribute{
		is_enabled:     left.is_enabled && right.is_enabled
		is_cinit:       left.is_cinit || right.is_cinit
		is_packed:      left.is_packed || right.is_packed
		export_name:    if right.export_name != '' { right.export_name } else { left.export_name }
		linker_section: if right.linker_section != '' {
			right.linker_section
		} else {
			left.linker_section
		}
		alignment:      if right.alignment != '' { right.alignment } else { left.alignment }
	}
}

fn fastc_global_c_attributes(attributes FastcDeclarationAttribute) string {
	mut parts := []string{}
	if attributes.linker_section != '' {
		section := fastc_c_string_value(attributes.linker_section)
		parts << 'used'
		parts << 'section(${section})'
	}
	if attributes.alignment != '' {
		parts << 'aligned(${attributes.alignment})'
	}
	return if parts.len > 0 { ' __attribute__((${parts.join(', ')}))' } else { '' }
}

fn fastc_static_fixed_array_initializer(initializer string) string {
	if marker := initializer.index('.data=') {
		start := marker + '.data='.len
		if start < initializer.len && initializer[start] == `{` {
			end := fastc_veb_balanced_pair(initializer, start, `{`, `}`)
			if end > start {
				return initializer[start..end]
			}
		}
	}
	return if initializer.ends_with('{0})') || initializer.ends_with('{0}')
		|| initializer.ends_with('{}') {
		'{0}'
	} else {
		initializer
	}
}

// fastc_map_field_default renders the empty-`map[K]V` value used to seed a map
// struct field's default (matching `map[K]V{}`), or '' when the type is not a
// resolvable map. Kept as a helper so the caller avoids a multi-return option in
// an `if` guard, which the FastC self-host does not accept.
fn fastc_map_field_default(typ u32, pointer_bits int, declared_kinds map[string]FastcDeclaredTypeKind) string {
	key_type_source_id, value_type_source_id := fastc_map_key_value_types(typ) or {
		fastc_declared_map_key_value_types(typ, declared_kinds) or { return '' }
	}
	key_type := key_type_source_id
	value_type := value_type_source_id
	hash_fn, eq_fn, clone_fn, free_fn := fastc_map_runtime_functions(flat.source_type_id(fastc_output_c_type(flat.source_type_name(key_type))), pointer_bits)
	return '(builtin__new_map(sizeof(${fastc_output_c_type(fastc_runtime_c_type(flat.source_type_name(key_type)))}), sizeof(${fastc_output_c_type(fastc_runtime_c_type(flat.source_type_name(value_type)))}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
}

// FastcFieldDefaultsResult is the struct field table with its defaults
// rendered and its by-name index, or the first error rendering them.
struct FastcFieldDefaultsResult {
	struct_field_info map[string][]FastcStructField
	lookup            map[string]map[string]&FastcStructField
	failed            bool
	error_message     string
}

// fastc_run_field_defaults renders the field defaults into a copy of the
// field table (the renderer replaces each type's entry) and indexes it.
fn fastc_run_field_defaults(source_imports map[string]map[string]string, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names flat.TypeTextBindings, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types flat.TypeTextBindings, enum_field_names map[string][]string, alias_base_types flat.TypeTextBindings, struct_fields map[string]flat.TypeTextBindings, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, function_c_names map[string]string, constants map[string]string, public_constants map[string]bool, constant_types flat.TypeTextBindings, globals map[string]string, public_globals map[string]bool, global_types flat.TypeTextBindings, sum_types map[string]bool) FastcFieldDefaultsResult {
	mut rendered := struct_field_info.clone()
	fastc_render_struct_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, enum_field_names, alias_base_types, struct_fields, mut rendered, functions, function_c_names, constants, public_constants, constant_types, globals, public_globals, global_types, sum_types) or {
		return FastcFieldDefaultsResult{
			failed:        true
			error_message: err.msg()
		}
	}
	return FastcFieldDefaultsResult{
		struct_field_info: rendered
		lookup:            fastc_build_struct_field_lookup(rendered)
	}
}

fn fastc_render_struct_field_defaults(source_imports map[string]map[string]string, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names flat.TypeTextBindings, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types flat.TypeTextBindings, enum_field_names map[string][]string, alias_base_types flat.TypeTextBindings, struct_fields map[string]flat.TypeTextBindings, mut struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, function_c_names map[string]string, constants map[string]string, public_constants map[string]bool, constant_types flat.TypeTextBindings, globals map[string]string, public_globals map[string]bool, global_types flat.TypeTextBindings, sum_types map[string]bool) ! {
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
			field_layout := field.typ_text().trim_right('*')
			container_type := fastc_underlying_alias_type(field.typ, alias_base_types)
			container_layout := flat.source_type_name(container_type).trim_right('*')
			if prefs.building_v && container_layout.starts_with('Array_') {
				if element_type_source_id := fastc_array_element_type(container_type) {
					element_type := element_type_source_id

					element_sizeof := if flat.source_type_name(element_type).ends_with('_ptr') {
						'voidptr'
					} else {
						flat.source_type_name(element_type)
					}
					field.default_value = '((${field_layout})builtin____new_array(0, 0, sizeof(${fastc_output_c_type(element_sizeof)})))'
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
			file := token.File.unindexed(field.path, field.default_source.len)
			mut gen := Parser{
				prefs:                        unsafe { prefs }
				unqualified_key_memo:         map[string]string{}
				global_key_memo:              map[string]string{}
				c_function_name_memo:         map[string]string{}
				nonlocal_name_type_memo:      flat.TypeTextBindings{}
				resolved_name_memo:           map[string]string{}
				declared_type_key_memo:       map[string]FastcMemoEntry{}
				path:                         field.path
				source_offset:                field.default_source_offset
				source_origins:               field.default_source_origins
				translated:                   field.translated
				module_name:                  field.module_name
				imports:                      source_imports[field.path] or {
					map[string]string{}
				}
				declared_types:               declared_types
				declared_type_c_names:        declared_type_c_names
				declared_type_key_by_name:    declared_type_key_by_name
				fastc_prefixed_c_names:       fastc_prefixed_c_names
				declaration_initializer_mode: true
				has_c_functions:              helper_has_c_functions
				comparison_memo:              map[i64]FastcRenderedExpression{}
				type_memo:                    map[i64]u32{}
				method_key_memo:              map[u32]map[string]string{}
				field_memo:                   map[u32]map[string]&FastcStructField{}
				spawn_typedefs:               map[string]string{}
				spawn_helpers:                map[string]string{}
				thread_value_types:           flat.TypeTextBindings{}
				declared_kinds:               declared_kinds
				enum_flags:                   enum_flags
				enum_field_types:             enum_field_types
				enum_field_names:             enum_field_names
				alias_base_types:             alias_base_types
				struct_fields:                struct_fields
				struct_field_info:            struct_field_info
				sum_types:                    sum_types
				constants:                    constants
				public_constants:             public_constants
				globals:                      globals
				public_globals:               public_globals
				selfhost:                     prefs.building_v
				s:                            scanner.new_scanner(prefs, .normal)
				out:                          strings.new_builder(0)
				protos:                       strings.new_builder(0)
				functions:                    functions
				function_c_names:             function_c_names
				constant_types:               constant_types
				global_types:                 global_types
				fixed_array_types:            flat.TypeTextBindings{}
				composite_types:              map[string]bool{}
			}
			gen.s.init(file, field.default_source)
			gen.next()
			gen.expected_expression_type = field.typ
			gen.allows_nil_local_assignment = gen.nil_field_is_allowed(field)
			field.default_value = gen.read_expression([token.Token.semicolon, token.Token.eof])!
			// A flag-enum default mixing a value with a `.member` shorthand (`~Show.zero() ^
			// .name`) can lose the field type mid-stream (the `Show.zero()` call clears it), so
			// resolve any surviving value-position shorthands against the field's enum type here.
			if prefs.building_v && declared_kinds[flat.source_type_name(gen.semantic_type_key(field.typ))] == .enum_ {
				enum_c := field.typ_text().trim_right('*')
				if members := enum_field_names[enum_c] {
					field.default_value = fastc_resolve_enum_shorthands_in_source(field.default_value, enum_c, members)
				}
				// `show Show = ~Show.zero()`: lower the flag enum's magic zero()/all() statics.
				field.default_value = gen.fastc_resolve_flag_enum_statics(field.default_value, enum_c)
			}
			// A variant literal (`value Primitive = Null{}`) defaulting a sum-type or
			// interface field must be boxed into the field's representation, mirroring
			// the assignment/argument paths; a bare `(Variant){}` is not assignable to a
			// boxed `{_object,_typ}` field.
			default_actual_type := gen.last_expression_type
			if field.typ == fastc_type_ierror && gen.last_expression.len == 1
				&& gen.last_expression[0].tok == .key_none {
				// `none` in an error interface is the None__ implementation, not an option.
				field.default_value = gen.interface_value_expression(field.typ, flat.source_type_id('None__'),
					gen.render_empty_struct_initializer(flat.source_type_id('None__')))
			} else if gen.should_box_variant(field.typ, default_actual_type) {
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
				offset := fastc_original_source_offset(gen.source_origins, gen.source_offset + diagnostic.offset)
				return error('fastc scanner error at byte ${offset} in ${field.path}: ${diagnostic.message}')
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
	declared_type_c_names     flat.TypeTextBindings
	declared_type_key_by_name flat.TypeTextBindings
	fastc_prefixed_c_names    []string
	has_c_functions           bool
	declared_kinds            map[string]FastcDeclaredTypeKind
	enum_flags                map[string]bool
	enum_field_types          flat.TypeTextBindings
	alias_base_types          flat.TypeTextBindings
	struct_fields             map[string]flat.TypeTextBindings
	struct_field_info         map[string][]FastcStructField
	sum_types                 map[string]bool
	sum_type_variants         map[string]bool
	functions                 map[string]FastcFunctionSignature
	function_c_names          map[string]string
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
	constant_types      flat.TypeTextBindings
	composite_types     map[string]bool
	fixed_array_types   flat.TypeTextBindings
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
fn fastc_parse_constant_file(ctx &FastcConstantGenContext, source_file FastcSourceFile, constant_types flat.TypeTextBindings) FastcConstantFileResult {
	constant_source := source_file.source
	file := token.File.unindexed(source_file.path, constant_source.len)
	prefs := ctx.prefs
	mut gen := Parser{
		prefs:                        unsafe { prefs }
		unqualified_key_memo:         map[string]string{}
		global_key_memo:              map[string]string{}
		c_function_name_memo:         map[string]string{}
		nonlocal_name_type_memo:      flat.TypeTextBindings{}
		resolved_name_memo:           map[string]string{}
		declared_type_key_memo:       map[string]FastcMemoEntry{}
		path:                         source_file.path
		source_offset:                source_file.source_offset
		source_origins:               source_file.source_origins
		translated:                   source_file.header.translated
		module_name:                  source_file.header.module_name
		imports:                      source_file.header.imports
		declared_types:               ctx.declared_types
		declared_type_c_names:        ctx.declared_type_c_names
		declared_type_key_by_name:    ctx.declared_type_key_by_name
		fastc_prefixed_c_names:       ctx.fastc_prefixed_c_names
		declaration_initializer_mode: true
		has_c_functions:              ctx.has_c_functions
		comparison_memo:              map[i64]FastcRenderedExpression{}
		type_memo:                    map[i64]u32{}
		method_key_memo:              map[u32]map[string]string{}
		field_memo:                   map[u32]map[string]&FastcStructField{}
		spawn_typedefs:               map[string]string{}
		spawn_helpers:                map[string]string{}
		thread_value_types:           flat.TypeTextBindings{}
		declared_kinds:               ctx.declared_kinds
		enum_flags:                   ctx.enum_flags
		enum_field_types:             ctx.enum_field_types
		alias_base_types:             ctx.alias_base_types
		struct_fields:                ctx.struct_fields
		struct_field_info:            ctx.struct_field_info
		sum_types:                    ctx.sum_types
		sum_type_variants:            ctx.sum_type_variants
		constants:                    ctx.constants
		public_constants:             ctx.public_constants
		globals:                      ctx.globals
		public_globals:               ctx.public_globals
		selfhost:                     prefs.building_v
		s:                            scanner.new_scanner(prefs, .normal)
		out:                          strings.new_builder(256)
		protos:                       strings.new_builder(0)
		functions:                    ctx.functions
		function_c_names:             ctx.function_c_names
		constant_types:               constant_types
		composite_types:              map[string]bool{}
		fixed_array_types:            flat.TypeTextBindings{}
	}
	gen.s.init(file, constant_source)
	gen.next()
	mut values := []FastcConstantValue{}
	gen.parse_selected_constant_declarations(mut values, false) or {
		return FastcConstantFileResult{
			failed:        true
			error_message: err.msg()
		}
	}
	if gen.s.diagnostics.len > 0 {
		diagnostic := gen.s.diagnostics[0]
		return FastcConstantFileResult{
			failed:        true
			error_message: 'fastc scanner error at byte ${diagnostic.offset} in ${source_file.path}: ${diagnostic.message}'
		}
	}
	return FastcConstantFileResult{
		used_field_defaults: gen.used_field_defaults
		values:              values
		constant_types:      gen.constant_types.move()
		composite_types:     gen.composite_types
		fixed_array_types:   gen.fixed_array_types
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

fn fastc_constant_candidates(ordered_sources []FastcSourceFile, constant_sources map[string]FastcDeclarationSource, constant_spans map[string][]int) []FastcSourceFile {
	mut candidates := []FastcSourceFile{cap: ordered_sources.len}
	for source_file in ordered_sources {
		constant_source := constant_sources[source_file.path] or { continue }
		spans := constant_spans[source_file.path] or { []int{} }
		if constant_source.source.len >= fastc_constant_split_size && spans.len > 2 {
			// A large file's declarations parse as separate candidates, so its
			// tables no longer bound the parallel pre-pass; the in-order merge
			// sees them in source order, as it would within one file.
			for i := 0; i + 1 < spans.len; i += 2 {
				candidates << FastcSourceFile{
					path:           source_file.path
					source:         constant_source.source[spans[i]..spans[i + 1]]
					source_offset:  spans[i]
					source_origins: constant_source.origins
					header:         source_file.header
				}
			}
			continue
		}
		candidates << FastcSourceFile{
			path:           source_file.path
			source:         constant_source.source
			source_origins: constant_source.origins
			header:         source_file.header
		}
	}
	return candidates
}

// fastc_seed_constant_types parses constants in source order only to make their
// types available to struct field defaults. Independent results from the
// authoritative parallel pass are reused; only order-dependent candidates are
// parsed again before the defaults are rendered.
fn fastc_seed_constant_types(ctx &FastcConstantGenContext, candidates []FastcSourceFile, parallel_results []FastcConstantFileResult, mut constant_types flat.TypeTextBindings) {
	for index, candidate in candidates {
		if index < parallel_results.len {
			result := parallel_results[index]
			if !result.failed && !result.used_field_defaults
				&& fastc_constant_file_is_independent(result) {
				for value in result.values {
					constant_types.set_id(value.key, value.typ)
				}
				continue
			}
		}
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
				if fastc_v_source_names(field.default_source, key.all_after_last('.')) {
					return true
				}
			}
		}
	}
	return false
}

// fastc_v_source_names reports whether the V source `source` may name `name`: as a whole
// identifier outside string text, or anywhere when it holds an interpolation or a comment,
// whose contents are not parsed here.
fn fastc_v_source_names(source string, name string) bool {
	if name == '' || !fastc_contains(source, name) {
		return false
	}
	if fastc_contains(source, '\${') || fastc_contains(source, '//') || fastc_contains(source, '/*') {
		return true
	}
	mut i := 0
	for i < source.len {
		c := source[i]
		if c in [`'`, `"`] {
			i = fastc_v_string_end(source, i, false)
		} else if c == `\`` {
			i = fastc_c_literal_end(source, i)
		} else if c.is_alnum() || c == `_` {
			mut end := i + 1
			for end < source.len && (source[end].is_alnum() || source[end] == `_`) {
				end++
			}
			// `r'...'` and `c'...'` are string literals, not names.
			if end == i + 1 && c in [`r`, `c`] && end < source.len
				&& (source[end] == `'` || source[end] == `"`) {
				i = fastc_v_string_end(source, end, c == `r`)
				continue
			}
			if end - i == name.len && unsafe { vmemcmp(source.str + i, name.str, name.len) } == 0 {
				return true
			}
			i = end
		} else {
			i++
		}
	}
	return false
}

// fastc_v_string_end returns the index just past the V string literal that opens at `quote`.
fn fastc_v_string_end(source string, quote int, raw bool) int {
	mut i := quote + 1
	for i < source.len {
		if source[i] == source[quote] {
			return i + 1
		}
		i += if !raw && source[i] == `\\` { 2 } else { 1 }
	}
	return source.len
}

fn fastc_generate_constant_declarations(candidates []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names flat.TypeTextBindings, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types flat.TypeTextBindings, alias_base_types flat.TypeTextBindings, struct_fields map[string]flat.TypeTextBindings, struct_field_info map[string][]FastcStructField, sum_types map[string]bool, sum_type_variants map[string]bool, mut pending_defaults FastcPendingFieldDefaults, functions map[string]FastcFunctionSignature, function_c_names map[string]string, constants map[string]string, public_constants map[string]bool, globals map[string]string, public_globals map[string]bool, initial_parallel_results []FastcConstantFileResult, mut constant_types flat.TypeTextBindings) !FastcConstantDeclarations {
	mut values := []FastcConstantValue{}
	mut composite_types := map[string]bool{}
	mut fixed_array_types := flat.TypeTextBindings{}
	ctx := FastcConstantGenContext{
		prefs:                     unsafe { prefs }
		declared_types:            declared_types
		declared_type_c_names:     declared_type_c_names
		declared_type_key_by_name: fastc_declared_type_key_by_name(declared_types)
		fastc_prefixed_c_names:    fastc_prefixed_c_names
		has_c_functions:           fastc_functions_declare_c(functions)
		declared_kinds:            declared_kinds
		enum_flags:                enum_flags
		enum_field_types:          enum_field_types
		alias_base_types:          alias_base_types
		struct_fields:             struct_fields
		struct_field_info:         struct_field_info
		sum_types:                 sum_types
		sum_type_variants:         sum_type_variants
		functions:                 functions
		function_c_names:          function_c_names
		constants:                 constants
		public_constants:          public_constants
		globals:                   globals
		public_globals:            public_globals
	}
	sw := time.new_stopwatch()
	parallel_results := if initial_parallel_results.len > 0 {
		initial_parallel_results
	} else {
		fastc_parse_constant_files_parallel(&ctx, candidates, constant_types)
	}
	parallel_us := sw.elapsed().microseconds()
	// The rendered defaults are needed from here on: for the in-order parse of
	// the files that consulted them, and by the phases after this one.
	defaults := fastc_wait_field_defaults(mut pending_defaults)!
	merge_ctx := FastcConstantGenContext{
		prefs:                     unsafe { prefs }
		declared_types:            declared_types
		declared_type_c_names:     declared_type_c_names
		declared_type_key_by_name: ctx.declared_type_key_by_name
		fastc_prefixed_c_names:    fastc_prefixed_c_names
		has_c_functions:           ctx.has_c_functions
		declared_kinds:            declared_kinds
		enum_flags:                enum_flags
		enum_field_types:          enum_field_types
		alias_base_types:          alias_base_types
		struct_fields:             struct_fields
		struct_field_info:         defaults.struct_field_info
		sum_types:                 sum_types
		sum_type_variants:         sum_type_variants
		functions:                 functions
		function_c_names:          function_c_names
		constants:                 constants
		public_constants:          public_constants
		globals:                   globals
		public_globals:            public_globals
	}
	mut serial_count := 0
	for index, candidate in candidates {
		if index < parallel_results.len {
			result := parallel_results[index]
			if !result.failed && !result.used_field_defaults
				&& fastc_constant_file_is_independent(result) {
				for value in result.values {
					constant_types.set_id(value.key, value.typ)
				}
				values << result.values
				for name, _ in result.composite_types {
					composite_types[name] = true
				}
				for name, array_type_id in result.fixed_array_types.ids {
					fixed_array_types.set_id(name, array_type_id)
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
		for name, array_type_id in serial.fixed_array_types.ids {
			fixed_array_types.set_id(name, array_type_id)
		}
	}
	if os.getenv('FASTC_BENCH_PHASES') != '' {
		eprintln('fastc-phase constants.detail candidates=${candidates.len} parallel_us=${parallel_us} serial=${serial_count} merge_us=${sw.elapsed().microseconds() - parallel_us}')
	}
	// Lowering a struct literal inserts field defaults that are absent from the
	// constant's source. Include their references before ordering initializers.
	mut constant_keys := map[string]string{}
	for value in values {
		constant_keys[value.c_name] = value.key
	}
	for index, value in values {
		mut dependencies := value.dependencies.clone()
		fastc_rendered_constant_dependencies(value.value, constant_keys, mut dependencies)
		values[index] = FastcConstantValue{
			...value
			dependencies: dependencies
		}
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
			if value.typ == u32(0) {
				return error('fastc parser does not support unverifiable runtime constant `${value.key}` type')
			}
			declarations.writeln('static ${value.typ_text()} ${value.c_name};')
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
		macros:              macros.str()
		declarations:        declarations.str()
		module_initializers: module_initializers
		compile_time_values: compile_time_values
		composite_types:     composite_types
		fixed_array_types:   fixed_array_types
		struct_field_info:   defaults.struct_field_info
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

// fastc_rendered_constant_dependencies collects references introduced by defaults,
// ignoring literals and member names in the generated C expression.
fn fastc_rendered_constant_dependencies(source string, keys map[string]string, mut dependencies []string) {
	mut offset := 0
	for offset < source.len {
		if source[offset] in [`'`, `"`] {
			offset = fastc_c_literal_end(source, offset)
			continue
		}
		if !fastc_c_identifier_start_byte(source[offset]) {
			offset++
			continue
		}
		start := offset
		offset++
		for offset < source.len && fastc_identifier_byte(source[offset]) {
			offset++
		}
		if start > 0 && (source[start - 1] == `.`
			|| (source[start - 1] == `>` && start > 1 && source[start - 2] == `-`)) {
			continue
		}
		if key := keys[source[start..offset]] {
			if key !in dependencies {
				dependencies << key
			}
		}
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
	expression_start := g.s.pos
	value := g.read_expression(stops)!
	if value.len == 0 {
		return g.unsupported('empty constant `${name}`')
	}
	key := fastc_constant_key(g.module_name, name)
	c_name := g.constants[key] or { return g.unsupported('unregistered constant `${name}`') }
	typ := fastc_normalize_inferred_type(g.last_expression_type)
	is_runtime := g.constant_expression_requires_runtime_storage(g.last_expression, value)
	// An inserted EOF semicolon leaves pos at the final operand's start.
	dependencies := g.constant_source_dependencies(g.s.src[expression_start..g.s.offset])
	if typ == u32(0) && is_runtime {
		return g.unsupported('unverifiable constant `${name}` type')
	}
	values << FastcConstantValue{
		key:          key
		c_name:       c_name
		module_name:  g.module_name
		value:        value
		typ:          typ
		dependencies: dependencies
		is_runtime:   is_runtime
	}
	g.constant_types.set_id(key, typ)
}

// constant_source_dependencies retains references inside lowered expressions,
// including interpolation operands that no longer appear in last_expression.
fn (g &Parser) constant_source_dependencies(source string) []string {
	mut scan := scanner.new_scanner(g.prefs, .normal)
	file := token.File.unindexed(g.path, source.len)
	scan.init(file, source)
	mut tokens := []FastcExpressionToken{}
	mut tok := scan.scan()
	for tok != .eof {
		tokens << FastcExpressionToken{
			tok: tok
			lit: scan.lit
		}
		tok = scan.scan()
	}
	return g.constant_expression_dependencies(tokens)
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
	struct_fields     map[string]flat.TypeTextBindings
	struct_field_info map[string][]FastcStructField
	composite_types   map[string]bool
	failed            bool
	error_message     string
}

// fastc_run_type_declarations runs the type phase into fresh tables.
fn fastc_run_type_declarations(sources []FastcSourceFile, type_sources map[string]FastcDeclarationSource, prefs &pref.Preferences, type_source_paths map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool) FastcTypeDeclarationResult {
	mut result := FastcTypeDeclarationResult{
		struct_fields:     map[string]flat.TypeTextBindings{}
		struct_field_info: map[string][]FastcStructField{}
		composite_types:   map[string]bool{}
	}
	result.output = fastc_generate_type_declarations(sources, type_sources, prefs, type_source_paths, declared_types, declared_kinds, enum_flags, constants, public_constants, mut result.struct_fields, mut result.struct_field_info, mut result.composite_types) or {
		result.failed = true
		result.error_message = err.msg()
		return result
	}
	return result
}

fn fastc_generate_type_declarations(sources []FastcSourceFile, type_sources map[string]FastcDeclarationSource, prefs &pref.Preferences, type_source_paths map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool, mut struct_fields map[string]flat.TypeTextBindings, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool) !FastcTypeDeclarations {
	mut out := strings.new_builder(4096)
	mut bodies := strings.new_builder(4096)
	mut enum_infos := []FastcEnumInfo{}
	mut alias_base_types := flat.TypeTextBindings{}
	mut fn_alias_return_types := flat.TypeTextBindings{}
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
		if primitive == 'usize' {
			// Both spellings box as uint and must also match the same runtime tag.
			out.writeln('#define __v_typeid_usize __v_typeid_uint')
		} else {
			out.writeln('#define __v_typeid_${primitive} ${u32(0x40000000) + u32(offset)}')
		}
	}
	out.writeln('')
	mut timer := fastc_new_phase_timer()
	for source_file in sources {
		if source_file.path !in type_source_paths {
			continue
		}
		type_source := type_sources[source_file.path] or { continue }
		type_source_file := FastcSourceFile{
			path:           source_file.path
			source:         type_source.source
			source_origins: type_source.origins
			header:         source_file.header
		}
		fastc_emit_source_type_declarations(type_source_file, prefs, declared_types, declared_kinds, constants, public_constants, mut struct_fields, mut struct_field_info, mut composite_types, mut alias_base_types, mut fn_alias_return_types, mut sum_types, mut sum_type_variants, mut enum_infos, mut bodies)!
	}
	timer.mark('type_declarations.emit')
	if '__v_result' in struct_fields {
		out.writeln('typedef struct __v_result __v_result;')
	}
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
		if '__v_result' in struct_fields {
			by_value_composite_names['__v_result'] = true
		}
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
	mut enum_field_types := flat.TypeTextBindings{}
	mut enum_field_names := map[string][]string{}
	for info in enum_infos {
		enum_field_names[info.c_name] = info.fields.clone()
		for field in info.fields {
			if !enum_field_types.contains(field) {
				enum_field_types.set(field, info.c_name)
			}
		}
	}
	enum_helper_names, enum_helper_texts := fastc_generate_enum_string_helpers(enum_infos, prefs.target_libc_headers)
	timer.mark('type_declarations.enum_helpers')
	return FastcTypeDeclarations{
		declarations_head:     declarations_head
		declarations:          out.str()
		enum_helper_names:     enum_helper_names
		enum_helper_texts:     enum_helper_texts
		alias_base_types:      alias_base_types
		fn_alias_return_types: fn_alias_return_types
		enum_field_types:      enum_field_types
		enum_field_names:      enum_field_names
		sum_types:             sum_types
		sum_type_variants:     sum_type_variants
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

fn fastc_order_c_composite_definitions(source string, struct_fields map[string]flat.TypeTextBindings, by_value_composite_names map[string]bool, alias_base_types flat.TypeTextBindings) string {
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
			for _, field_type_id in fields.ids {
				dependency := fastc_by_value_composite_type(field_type_id, by_value_composite_names, alias_base_types)
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
	// A packed definition's `#pragma pack(pop)` belongs to its block.
	if source[end..].starts_with(fastc_pack_pop_line) {
		end += fastc_pack_pop_line.len
		for end < source.len && source[end] == `\n` {
			end++
		}
	}
	return end
}

const fastc_pack_push_line = '#pragma pack(push, 1)\n'
const fastc_pack_pop_line = '#pragma pack(pop)\n'

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
		mut j := fastc_c_type_attributes_end(source, i + keyword_len) or {
			i += keyword_len
			continue
		}
		name_start := j
		for j < source.len && (source[j].is_alnum() || source[j] == `_`) {
			j++
		}
		body_start := fastc_c_type_attributes_end(source, j) or { j }
		if j > name_start && body_start < source.len && source[body_start] == `{` {
			name := source[name_start..j]
			if name !in positions {
				// A packed definition's `#pragma pack(push, 1)` line belongs to its block.
				push_start := i - fastc_pack_push_line.len
				positions[name] = if push_start >= 0 && source[push_start..i] == fastc_pack_push_line {
					push_start
				} else {
					i
				}
			}
		}
		i = j
	}
	return positions
}

fn fastc_by_value_composite_type(field_type u32, by_value_composite_names map[string]bool, alias_base_types flat.TypeTextBindings) string {
	field_type_text := flat.source_type_name(field_type)
	if flat.source_type_query(field_type).has(.c_pointer_suffix) || field_type_text.starts_with('Array_') || field_type_text.starts_with('Map_') {
		return ''
	}
	mut candidate := field_type
	if element_type := fastc_fixed_array_element_type(candidate) {
		candidate = element_type
	}
	// Most field types are not aliases; answer those without the cycle guard.
	candidate_text := flat.source_type_name(candidate)
	if !alias_base_types.contains(candidate_text) {
		return if candidate_text in by_value_composite_names { candidate_text } else { '' }
	}
	mut seen_aliases := map[u32]bool{}
	for {
		name := flat.source_type_name(candidate)
		if !alias_base_types.contains(name) || candidate in seen_aliases {
			break
		}
		seen_aliases[candidate] = true
		candidate = alias_base_types.value_id(name)
		if flat.source_type_query(candidate).has(.c_pointer_suffix) {
			return ''
		}
	}
	name := flat.source_type_name(candidate)
	if name in by_value_composite_names {
		return name
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

// fastc_c_composite_definition_end returns the offset just past the `};` that closes the
// definition starting at `start`, matching braces so that a one-line definition
// (`struct I { void *_object; uintptr_t _typ; };`) ends at its own `};`.
@[direct_array_access]
fn fastc_c_composite_definition_end(source string, start int) ?int {
	mut depth := 0
	mut quote := u8(0)
	mut i := start
	for i >= 0 && i < source.len {
		c := source[i]
		if quote != 0 {
			if c == `\\` {
				i += 2
				continue
			}
			if c == quote {
				quote = 0
			}
		} else if c in [`'`, `"`] {
			quote = c
		} else if c == `/` && i + 1 < source.len && source[i + 1] == `*` {
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			i += 2
			continue
		} else if c == `/` && i + 1 < source.len && source[i + 1] == `/` {
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		} else if c == `{` {
			depth++
		} else if c == `}` {
			depth--
			if depth == 0 {
				end := fastc_c_type_attributes_end(source, i + 1) or { return none }
				if end < source.len && source[end] == `;` {
					return end + 1
				}
				return none
			}
		}
		i++
	}
	return none
}

fn fastc_c_type_attributes_end(source string, start int) ?int {
	mut i := start
	for {
		for i < source.len && source[i].is_space() {
			i++
		}
		attribute_end := i + '__attribute__'.len
		if attribute_end > source.len {
			return i
		}
		// This temporary prefix view avoids copying the whole declaration tail.
		if !(unsafe { source[i..attribute_end] }).starts_with('__attribute__') {
			return i
		}
		i += '__attribute__'.len
		for i < source.len && source[i].is_space() {
			i++
		}
		if i >= source.len || source[i] != `(` {
			return none
		}
		i = fastc_veb_balanced_pair(source, i, `(`, `)`)
		if i < 0 {
			return none
		}
	}
	return none
}

fn fastc_emit_source_type_declarations(source_file FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool, mut struct_fields map[string]flat.TypeTextBindings, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool, mut alias_base_types flat.TypeTextBindings, mut fn_alias_return_types flat.TypeTextBindings, mut sum_types map[string]bool, mut sum_type_variants map[string]bool, mut enum_infos []FastcEnumInfo, mut out strings.Builder) ! {
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut depth := 0
	mut next_enum_is_flag := false
	mut next_type_is_enabled := true
	mut struct_attributes := FastcDeclarationAttribute{ is_enabled: true }
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if depth == 0 && tok in [.key_module, .key_import] {
			tok = fastc_skip_module_or_import(mut scan, tok, source_file.path)!
			continue
		}
		if depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), source_file.path, prefs)!
				if selected.source != '' {
					selected_source := FastcSourceFile{
						path:           source_file.path
						source:         selected.source
						source_offset:  source_file.source_offset + selected.offset
						source_origins: source_file.source_origins
						header:         source_file.header
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
			struct_attributes = fastc_merge_declaration_attributes(struct_attributes, attribute)
			continue
		}
		if depth == 0 && tok in [.key_fn, .key_const, .key_global] {
			next_enum_is_flag = false
			next_type_is_enabled = true
			struct_attributes = FastcDeclarationAttribute{ is_enabled: true }
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
			tok = fastc_emit_struct_declaration(mut scan, tok == .key_union, struct_attributes, source_file, declared_types, prefs.building_v, prefs.target_libc_headers, mut struct_fields, mut struct_field_info, mut composite_types, mut out)!
			struct_attributes = FastcDeclarationAttribute{ is_enabled: true }
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
			tok = fastc_emit_alias_declaration(mut scan, source_file, declared_types, declared_kinds, prefs.building_v, mut struct_fields, mut struct_field_info, mut composite_types, mut alias_base_types, mut fn_alias_return_types, mut sum_types, mut sum_type_variants, mut out)!
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
	mut is_markused := false
	mut export_name := ''
	mut linker_section := ''
	mut alignment := ''
	mut is_cinit := false
	mut is_packed := false
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
		if depth == 1 && at_item_start && tok == .name && scan.lit in ['packed', '_packed'] {
			is_packed = true
		}
		if depth == 1 && at_item_start && tok == .name && scan.lit == '_pack' {
			// `@[_pack: N]`: only 1 has an attribute spelling that keeps its place when the
			// definition is reordered; refuse any other rather than lay the struct out wrong.
			mut value_scan := scan
			if value_scan.scan() == .colon {
				value_tok := value_scan.scan()
				value := value_scan.lit.trim('\'"')
				if value_tok !in [.number, .string] || value != '1' {
					return error('fastc parser does not support `@[_pack: ${value}]` in ${path}')
				}
				is_packed = true
			}
		}
		if depth == 1 && at_item_start {
			if scan.lit in ['export', '_linker_section', 'aligned'] {
				name := scan.lit
				tok = scan.scan()
				if tok != .colon {
					return error('fastc parser expects a value for attribute `${name}` in ${path}')
				}
				tok = scan.scan()
				if (name == 'aligned' && tok !in [.number, .name])
					|| (name != 'aligned' && tok != .string) {
					return error('fastc parser does not support attribute `${name}` value in ${path}')
				}
				match name {
					'export' { export_name = fastc_string_literal_content(scan.lit) }
					'_linker_section' { linker_section = fastc_string_literal_content(scan.lit) }
					else { alignment = scan.lit }
				}
				at_item_start = false
				tok = scan.scan()
				continue
			}
			is_cinit = is_cinit || scan.lit == 'cinit'
			is_packed = is_packed || scan.lit == 'packed'
			is_markused = is_markused || scan.lit == 'markused'
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
		tok:            tok
		is_enabled:     is_enabled
		is_flag:        is_flag
		is_params:      is_params
		is_typedef:     is_typedef
		is_c_extern:    is_c_extern
		is_markused:    is_markused
		export_name:    export_name
		linker_section: linker_section
		alignment:      alignment
		is_cinit:       is_cinit
		is_packed:      is_packed
	}
}

fn fastc_emit_struct_declaration(mut scan scanner.Scanner, is_union bool, attributes FastcDeclarationAttribute, source_file FastcSourceFile, declared_types map[string]bool, allow_short_placeholders bool, target_libc_headers bool, mut struct_fields map[string]flat.TypeTextBindings, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool, mut out strings.Builder) !token.Token {
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
	mut fields_by_name := flat.TypeTextBindings{}
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
		if attributes.is_packed {
			// TinyCC ignores `__attribute__((packed))`; it honours `#pragma pack`, as the C
			// backend emits it. The ordering pass moves these lines with the definition.
			out.write_string(fastc_pack_push_line)
		}
		mut decorations := []string{}
		if attributes.is_packed {
			decorations << 'packed'
		}
		if attributes.alignment != '' {
			decorations << 'aligned(${attributes.alignment})'
		}
		decoration := if decorations.len > 0 {
			' __attribute__((${decorations.join(', ')}))'
		} else {
			''
		}
		out.writeln('${if is_union { 'union' } else { 'struct' }}${decoration} ${c_name} {')
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
		// `pub string` names a field `pub`; only `pub:` / `pub mut:` start a section.
		mut pub_lookahead := scan
		pub_starts_section := tok == .key_pub && pub_lookahead.scan() in [.key_mut, .colon]
		if pub_starts_section {
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
		is_volatile_field := tok == .key_volatile
		if is_volatile_field {
			tok = scan.scan()
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
		if tok in [.semicolon, .rcbr, .assign] {
			embedded_key := if qualified_embed_key != '' {
				qualified_embed_key
			} else {
				fastc_resolve_declared_type_key(source_file.header.module_name, field_names[0], source_file.header.imports, declared_types) or {
					return error('fastc parser does not support embedded field `${field_names[0]}` in ${source_file.path}')
				}
			}
			mut default_source := ''
			mut default_source_offset := 0
			if tok == .assign {
				first_default_token := scan.scan()
				default_start := scan.pos
				default_source_offset = source_file.source_offset + default_start
				tok = fastc_skip_field_default_from_token(mut scan, first_default_token)!
				default_source = source_file.source[default_start..scan.pos].trim_space().trim_right(';').trim_space()
				if default_source == '' {
					return error('fastc parser does not support empty default for struct `${name}` embedded field `${field_names[0]}` in ${source_file.path}')
				}
			}
			if !is_c_struct {
				out.writeln('\t${fastc_c_declared_type_name(embedded_key)} __embedded_${embedded_id};')
			}
			fields_by_name.set('__embedded_${embedded_id}', fastc_c_declared_type_name(embedded_key))
			field_info << FastcStructField{
				name:                   '__embedded_${embedded_id}'
				typ:                    flat.source_type_id(fastc_c_declared_type_name(embedded_key))
				is_public:              true
				module_name:            source_file.header.module_name
				path:                   source_file.path
				imports:                source_file.header.imports.clone()
				default_source:         default_source
				default_source_offset:  default_source_offset
				default_source_origins: source_file.source_origins
				translated:             source_file.header.translated
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
		mut field_chan_element := u32(0)
		if tok == .name && scan.lit == 'chan' {
			field_chan_element = fastc_peek_chan_element(scan, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)
		}
		mut field_option_value := u32(0)
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
		is_declared_voidptr := tok == .name && scan.lit == 'voidptr'
		mut function_type := FastcFunctionTypeInfo{}
		if is_function_field {
			function_type = fastc_peek_function_type(function_scan, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
		}
		field_type_source_id, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders) or {
			return error('fastc struct `${name}` field `${field_names[0]}`: ${err.msg()}')
		}
		field_type := field_type_source_id

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
		mut default_source_offset := 0
		if tok == .assign {
			first_default_token := scan.scan()
			default_start := scan.pos
			default_source_offset = source_file.source_offset + default_start
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
			if name == 'Node' && field_name == 'value' && field_type == flat.source_type_voidptr && source_file.path.ends_with('/vlib/json2/decode.v') {
				value_info_key := fastc_type_key(source_file.header.module_name, 'ValueInfo')
				if value_info_key in declared_types {
					emitted_field_type = flat.source_type_id(fastc_c_declared_type_name(value_info_key))
				}
			}
			is_shared_pointer := is_shared_field && flat.source_type_name(emitted_field_type).starts_with('Map_')
			if !is_c_struct {
				if is_shared_pointer {
					out.writeln('\t${if is_volatile_field { 'volatile ' } else { '' }}${flat.source_type_name(emitted_field_type)}* ${c_field_name};')
				} else if declaration := fastc_fixed_array_field_declaration(emitted_field_type, c_field_name) {
					out.writeln('\t${if is_volatile_field { 'volatile ' } else { '' }}${declaration};')
				} else {
					out.writeln('\t${if is_volatile_field { 'volatile ' } else { '' }}${fastc_output_c_type(flat.source_type_name(emitted_field_type))} ${c_field_name};')
				}
			}
			fields_by_name.set_id(field_name, emitted_field_type)
			field_info << FastcStructField{
				name:                   field_name
				typ:                    emitted_field_type
				is_public:              fields_are_public
				is_mutable:             fields_are_mutable
				is_required:            is_required
				is_skip:                is_skip
				is_function:            is_function_field
				is_optional_function:   is_optional_function
				is_declared_voidptr:    is_declared_voidptr
				is_shared_pointer:      is_shared_pointer
				module_name:            source_file.header.module_name
				path:                   source_file.path
				imports:                source_file.header.imports.clone()
				default_source:         default_source
				default_source_offset:  default_source_offset
				default_source_origins: source_file.source_origins
				translated:             source_file.header.translated
				chan_element_type:      field_chan_element
				option_value_type:      field_option_value
				fn_parameter_types:     function_type.parameter_types.clone()
				fn_return_type:         function_type.return_type
				fn_option_value_type:   function_type.option_value_type
				generic_argument_type:  field_generic_argument
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
		if target_libc_headers {
			out.writeln('\tuintptr_t inline_bits;')
			out.writeln('\tu8 is_inline;')
		}
		fields_by_name.set('data', 'voidptr')
		field_info << FastcStructField{
			name:        'data'
			typ:         flat.source_type_voidptr
			is_public:   true
			module_name: source_file.header.module_name
			path:        source_file.path
			imports:     source_file.header.imports.clone()
		}
	}
	if !is_c_struct {
		out.writeln('};')
		if attributes.is_packed {
			out.write_string(fastc_pack_pop_line)
		}
		out.writeln('')
	}
	if c_name == 'Option' {
		inline_fields := if target_libc_headers {
			' uintptr_t inline_bits; u8 is_inline;'
		} else {
			''
		}
		out.writeln('struct __v_result { u8 state; IError err; void *data;${inline_fields} };')
		mut result_fields := flat.TypeTextBindings{}
		result_fields.set('state', 'u8')
		result_fields.set('err', 'IError')
		result_fields.set('data', 'voidptr')
		struct_fields['__v_result'] = result_fields
	}
	struct_fields[c_name] = fields_by_name.move()
	struct_field_info[c_name] = field_info.clone()
	return scan.scan()
}

fn fastc_fixed_array_field_declaration(field_type u32, field_name string) ?string {
	mut element_type := field_type
	mut dimensions := []string{}
	for {
		length := fastc_fixed_array_length(flat.source_type_name(element_type)) or { break }
		dimensions << length
		element_type = fastc_fixed_array_element_type(element_type) or { return none }
	}
	if dimensions.len == 0 {
		return none
	}
	return '${fastc_output_c_type(flat.source_type_name(element_type))} ${field_name}[${dimensions.join('][')}]'
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
			return flat.source_type_name(entry.value)
		}
		return none
	}
	mut w := unsafe { &Parser(g) }
	if key := g.resolve_declared_type_key_uncached(raw_type) {
		w.declared_type_key_memo[raw_type] = FastcMemoEntry{
			found: true
			value: flat.source_type_id(key)
		}
		return key
	}
	w.declared_type_key_memo[raw_type] = FastcMemoEntry{}
	return none
}

fn (g &Parser) resolve_declared_type_key_uncached(raw_type string) ?string {
	if g.declared_type_key_by_name.ids.len == 0 {
		return fastc_resolve_declared_type_key(g.module_name, raw_type, g.imports, g.declared_types)
	}
	key := g.declared_type_key_by_name.get(raw_type) or {
		if raw_type.contains('__') {
			return g.declared_type_c_names.get(raw_type) or { return none }
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

fn fastc_semantic_declared_type_key(c_type string, declared_type_c_names flat.TypeTextBindings) string {
	base := c_type.trim_right('*')
	// Resolve through the precomputed C-spelling index (first key per spelling
	// wins, matching the previous insertion-ordered first-match scan). The old
	// scan re-rendered every declared type's C name per query — an allocation
	// per key on a path hit once per method receiver during signature scanning.
	if key := declared_type_c_names.get(base) {
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
		is_backing_int := tok == .name && scan.lit in ['i8', 'i16', 'i32', 'i64', 'int', 'u8',
			'u16', 'u32', 'u64', 'isize', 'uint', 'usize']
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
			scanned_tokens, next_token := fastc_scan_enum_value_tokens(mut scan, scan.scan())!
			tok = next_token
			// `2 ** 3` has no C operator; the C backend folds it to a constant.
			value_tokens := fastc_fold_integer_powers(scanned_tokens) or { scanned_tokens }
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
		c_name:  c_name
		name:    flat.source_type_id(name)
		fields:  field_names.clone()
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
fn fastc_generate_enum_string_helpers(infos []FastcEnumInfo, target_libc_headers bool) ([]string, []string) {
	mut names := []string{cap: infos.len * 2}
	mut texts := []string{cap: infos.len * 2}
	for info in infos {
		mut out := strings.new_builder(256)
		out.writeln('string v_fastc_enum_str_${info.c_name}(${info.c_name} value) {')
		if info.is_flag {
			out.writeln('\tstring parts[${info.fields.len * 2 + 2}] = {0};')
			out.writeln('\tint part_count = 0;')
			out.writeln('\tbool written = false;')
			out.writeln('\tparts[part_count++] = _S("${info.name_text()}{");')
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
		fastc_emit_enum_print_function(info.c_name, info.name_text(), info.fields, info.is_flag, mut print_out)
		print_out.writeln('')
		names << 'v_fastc_print_enum_${info.c_name}'
		texts << print_out.str()
		mut from_out := strings.new_builder(256)
		// `Enum.from_string(s)` maps a field-name string back to its value, returning
		// `?Enum` (none when no field name matches).
		from_out.writeln('Option ${info.c_name}__from_string(string __vf_from) {')
		for field in info.fields {
			value := if target_libc_headers {
				'V_FASTC_OPTION_SUCCESS(Option, ${info.c_name}, __vf_value)'
			} else {
				'(Option){.data=v_fastc_interface_box(&__vf_value, sizeof(${info.c_name})), .state=0}'
			}
			from_out.writeln('\tif (builtin__string_eq(__vf_from, _S("${field}"))) { ${info.c_name} __vf_value = ${info.c_name}__${field}; return ${value}; }')
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
	out.writeln('struct ${c_name} { void *_object; uintptr_t _typ; };')
	out.writeln('')
	return tok
}

fn fastc_emit_alias_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, allow_short_placeholders bool, mut struct_fields map[string]flat.TypeTextBindings, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool, mut alias_base_types flat.TypeTextBindings, mut fn_alias_return_types flat.TypeTextBindings, mut sum_types map[string]bool, mut sum_type_variants map[string]bool, mut out strings.Builder) !token.Token {
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
	mut base_scan := scan
	mut base_token := tok
	for (base_token in [.amp, .and, .mul]) {
		base_token = base_scan.scan()
	}
	is_c_type := base_token == .name && base_scan.lit == 'C'
	base_source_id, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders) or { return error('fastc type `${name}`: ${err.msg()}') }
	base := base_source_id
	tok = next_token
	if tok == .semicolon {
		tok = scan.scan()
	}
	if tok == .pipe {
		// Record each variant's C type (keyed `<sum type>|<variant>`) so an append can
		// tell push-many (`[]T << []T`) from boxing an array-valued variant of a
		// recursive sum type (`type Value = []Value | int`) as one element. `base` is
		// the first variant, already scanned above; the rest follow each `|`.
		sum_type_variants['${c_name}|${flat.source_type_name(base).trim_right('*')}'] = true
		for tok == .pipe {
			tok = scan.scan()
			variant_source_id, variant_next := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders) or { return error('fastc type `${name}`: ${err.msg()}') }
			variant := variant_source_id

			sum_type_variants['${c_name}|${flat.source_type_name(variant).trim_right('*')}'] = true
			tok = variant_next
			if tok == .semicolon {
				tok = scan.scan()
			}
		}
		// A sum type is lowered to the same boxed representation as an interface, so
		// construction (variant boxing) and `match` (dispatch on `_typ`) reuse the
		// interface machinery.
		out.writeln('typedef struct { void *_object; uintptr_t _typ; } ${c_name};')
		sum_types[c_name] = true
	} else if fastc_primitive_c_type(name) == none && declared_kinds[key] == .alias_ {
		out.writeln('typedef ${if is_c_type {
			flat.source_type_name(base)
		} else {
			fastc_output_c_type(flat.source_type_name(base))
		}} ${c_name};')
		// The typedef above names the aliased `Array_x`/`Map_k_v` type, so that type
		// needs its own typedef even when nothing else uses it.
		fastc_register_composite_type(base, mut composite_types)
		// C.int retains its native width instead of V's platform-width int.
		alias_base_types.set_id(c_name, if is_c_type && fastc_trim_pointer_type(base) == flat.source_type_int {
			flat.source_type_id('i32' + flat.source_type_name(base)['int'.len..])
		} else {
			base
		})
		mut layout_type := flat.source_type_name(base).trim_right('*')
		if layout_type.starts_with('Array_') {
			layout_type = 'array'
		} else if layout_type.starts_with('Map_') {
			layout_type = 'map'
		}
		mut alias_fields := flat.TypeTextBindings{}
		mut alias_field_info := []FastcStructField{}
		if layout_type in struct_fields {
			alias_fields = struct_fields[layout_type].clone()
		}
		if layout_type in struct_field_info {
			alias_field_info = struct_field_info[layout_type].clone()
		}
		if flat.source_type_name(base).starts_with('Array_') {
			alias_fields.set('__fastc_element_type', flat.source_type_name(base)['Array_'.len..])
		}
		if alias_fields.ids.len > 0 {
			struct_fields[c_name] = alias_fields.clone()
		}
		if alias_field_info.len > 0 {
			struct_field_info[c_name] = alias_field_info.clone()
		}
	}
	return tok
}

fn fastc_emit_function_alias(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, allow_short_placeholders bool, c_name string, mut fn_alias_return_types flat.TypeTextBindings, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type `${c_name}` in ${source_file.path}')
	}
	tok = scan.scan()
	mut parameter_types := []u32{}
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
			parameter_type_source_id, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
			parameter_type := parameter_type_source_id

			parameter_types << if parameter_is_mut && !flat.source_type_query(parameter_type).has(.c_pointer_suffix) {
				flat.source_type_id(flat.source_type_name(parameter_type) + '*')
			} else {
				parameter_type
			}
			tok = next_token
		} else {
			parameter_type_source_id_2, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
			parameter_type := parameter_type_source_id_2

			parameter_types << if parameter_is_mut && !flat.source_type_query(parameter_type).has(.c_pointer_suffix) {
				flat.source_type_id(flat.source_type_name(parameter_type) + '*')
			} else {
				parameter_type
			}
			tok = next_token
		}
	}
	tok = scan.scan()
	mut return_type := flat.source_type_void
	if tok in [.not, .question] {
		// A result/option return (`fn (...) !`, `fn (...) ?Type`) lowers to FastC's
		// fixed `Option` value; consume any concrete value type after it.
		return_type = flat.source_type_id(if tok == .not { '__v_result' } else { 'Option' })
		tok = scan.scan()
		if tok in [.name, .amp, .and, .mul, .lsbr, .key_fn, .question, .not] {
			_, tok = fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
		}
	} else if tok !in [.semicolon, .eof] {
		return_type_source_id, tok_scan_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, allow_short_placeholders)!
		return_type = return_type_source_id
		tok = tok_scan_token
	}
	out.writeln('typedef ${fastc_output_c_type(flat.source_type_name(return_type))} (*${c_name})(${fastc_c_prototype_parameters(parameter_types)});')
	// Record the alias's return type so a call through a value of this fn type
	// (`resolver(path)` where `resolver SomeFnAlias`) can infer its result type.
	fn_alias_return_types.set_id(c_name, return_type)
	if tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}
