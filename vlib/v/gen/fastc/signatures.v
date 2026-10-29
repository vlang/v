module fastc

import v.flat
import encoding.hex
import os
import strings
import v.pref
import v.scanner
import v.token

fn fastc_token_can_be_decl_name(tok token.Token) bool {
	return tok == .name || (tok.is_keyword() && tok != .key_volatile)
}

fn fastc_skip_attribute(mut scan scanner.Scanner) !token.Token {
	mut tok := scan.scan()
	mut depth := 1
	for depth > 0 {
		if tok == .eof {
			return error('fastc parser does not support unfinished attribute')
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return tok
}

fn fastc_shared_parameter_is_name(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) bool {
	mut lookahead := scan
	first := lookahead.scan()
	if first == .comma {
		return true
	}
	if first in [.semicolon, .rpar, .eof] {
		return false
	}
	_, boundary := fastc_scan_type(mut lookahead, first, path, module_name, imports, declared_types, allow_short_placeholders) or { return false }
	return boundary in [.comma, .semicolon, .rpar, .eof]
}

fn fastc_scan_struct_field_attribute(mut scan scanner.Scanner) !(token.Token, bool, bool) {
	mut tok := scan.scan()
	mut depth := 1
	mut is_required := false
	mut is_skip := false
	for depth > 0 {
		if tok == .eof {
			return error('fastc parser does not support unfinished struct field attribute')
		}
		if tok == .name && scan.lit == 'required' {
			is_required = true
		}
		if tok == .name && scan.lit == 'skip' {
			is_skip = true
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return tok, is_required, is_skip
}

fn fastc_skip_balanced_tokens(mut scan scanner.Scanner, first token.Token, open token.Token, close token.Token) !token.Token {
	mut tok := first
	mut depth := 0
	for {
		if tok == open {
			depth++
		} else if tok == close {
			depth--
			if depth == 0 {
				return scan.scan()
			}
		} else if tok == .eof {
			return error('fastc parser does not support unfinished `${open.str()}` group')
		}
		tok = scan.scan()
	}
	return tok
}

fn fastc_skip_field_default_from_token(mut scan scanner.Scanner, first token.Token) !token.Token {
	mut tok := first
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for tok != .eof {
		if parens == 0 && brackets == 0 && braces == 0 && tok in [.semicolon, .rcbr] {
			return tok
		}
		match tok {
			.lpar { parens++ }
			.rpar { parens-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			.lcbr { braces++ }
			.rcbr { braces-- }
			else {}
		}
		tok = scan.scan()
	}
	return tok
}

fn fastc_skip_type_declaration(mut scan scanner.Scanner, first token.Token) !token.Token {
	mut tok := first
	for tok != .eof && tok != .lcbr && tok != .semicolon {
		tok = scan.scan()
	}
	if tok == .lcbr {
		return fastc_skip_balanced_tokens(mut scan, tok, .lcbr, .rcbr)
	}
	return if tok == .semicolon { scan.scan() } else { tok }
}

fn fastc_parameter_is_params_struct(parameter_type u32, params_structs map[string]bool) bool {
	return params_structs[flat.source_type_name(fastc_trim_pointer_type(parameter_type))]
}

fn collect_function_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, declared_types map[u32]bool, declared_type_c_names flat.TypeTextBindings, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	collect_function_signatures_at(source, 0, path, header, prefs, skips, declared_types,
		declared_type_c_names, params_structs, mut functions)!
}

// collect_function_signatures_at collects the functions declared in `source`, which starts at
// byte `source_offset` of the file at `path`.
fn collect_function_signatures_at(source string, source_offset int, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, declared_types map[u32]bool, declared_type_c_names flat.TypeTextBindings, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_declaration_is_enabled := true
	mut next_declaration_is_c_extern := false
	mut next_declaration_is_markused := false
	mut next_export_name := ''
	mut previous_tok := token.Token.unknown
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
		if brace_depth == 0 && tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_declaration_is_enabled = next_declaration_is_enabled && attribute.is_enabled
			next_declaration_is_c_extern = next_declaration_is_c_extern || attribute.is_c_extern
			next_declaration_is_markused = next_declaration_is_markused || attribute.is_markused
			if attribute.export_name != '' {
				next_export_name = attribute.export_name
			}
			continue
		}
		if brace_depth == 0 && tok == .key_global {
			tok = fastc_collect_global_function_signatures(mut scan, path, header, prefs,
				declared_types, next_declaration_is_enabled, previous_tok == .key_pub,
				mut functions)!
			next_declaration_is_enabled = true
			next_declaration_is_c_extern = false
			next_declaration_is_markused = false
			next_export_name = ''
			previous_tok = .unknown
			continue
		}
		if tok == .key_type && brace_depth == 0 {
			mut alias_scan := scan
			if alias_scan.scan() == .name {
				alias_name := alias_scan.lit
				if alias_scan.scan() == .assign && alias_scan.scan() == .key_fn {
					alias_key := fastc_c_declared_type_name(fastc_type_key(header.module_name, alias_name))
					signature, next_token := fastc_scan_function_alias_signature(mut alias_scan, path, header, prefs, declared_types)!
					functions[alias_key] = signature
					// Resume after the whole alias, including nested callback types.
					scan = alias_scan
					tok = next_token
					next_declaration_is_enabled = true
					next_declaration_is_c_extern = false
					next_declaration_is_markused = false
					next_export_name = ''
					previous_tok = .unknown
					continue
				}
			}
		}
		if tok == .key_fn && brace_depth == 0 && previous_tok != .assign {
			is_public := previous_tok == .key_pub
			declaration_offset := source_offset + scan.pos
			tok = scan.scan()
			mut parameter_types := []u32{}
			mut parameter_mutability := []bool{}
			mut parameter_constness := []bool{}
			mut parameter_generic_values := []bool{}
			mut receiver_type := u32(0)
			mut receiver_key := ''
			if tok == .lpar {
				tok = scan.scan()
				mut receiver_is_mut := false
				if tok == .key_mut {
					receiver_is_mut = true
					tok = scan.scan()
				}
				if tok != .name {
					return error('fastc parser does not support method receiver in ${path}')
				}
				type_only_scan := scan
				tok = scan.scan()
				if tok == .rpar {
					// type only receiver like `fn (Padding) marker() {}`: rewind, so that the
					// single name is scanned as the receiver type instead of as its binding
					scan = type_only_scan
					tok = .name
				}
				if tok == .name && scan.lit != 'C' {
					receiver_key = fastc_type_key(header.module_name, scan.lit)
				} else if tok == .key_none {
					receiver_key = 'none'
				}
				receiver_type_source_id, tok_scan_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
					return error('fastc method receiver: ${err.msg()}')
				}
				receiver_type = receiver_type_source_id
				tok = tok_scan_token

				if receiver_key == '' {
					receiver_key = flat.source_type_name(fastc_semantic_declared_type_key(receiver_type, declared_type_c_names))
				}
				if receiver_is_mut && !flat.source_type_query(receiver_type).has(.c_pointer_suffix) {
					receiver_type = flat.source_type_composition_two(.concat, receiver_type, flat.source_type_id('*'))
				}
				parameter_types << receiver_type
				parameter_mutability << receiver_is_mut
				parameter_generic_values << false
				if tok != .rpar {
					return error('fastc parser does not support method receiver separator in ${path}')
				}
				tok = scan.scan()
			}
			if tok != .name && !(tok.is_overloadable() || tok.is_keyword()) {
				return error('fastc parser does not support function declaration token `${tok.str()}` `${scan.lit}` in ${path}')
			}
			mut name := if tok == .name || tok.is_keyword() { scan.lit } else { tok.str() }
			tok = scan.scan()
			mut is_c_function := false
			if receiver_type == u32(0) && name == 'C' && tok == .dot {
				is_c_function = true
				tok = scan.scan()
				if tok != .name && !tok.is_keyword() {
					return error('fastc parser does not support C function declaration in ${path}')
				}
				name = scan.lit
				tok = scan.scan()
			} else if receiver_type == u32(0) && tok == .dot {
				type_key := fastc_type_key(header.module_name, name)
				if !fastc_source_type_key_exists(declared_types, type_key) {
					return error('fastc parser does not support static method owner `${name}` in ${path}')
				}
				receiver_type = flat.source_type_id(fastc_c_declared_type_name(type_key))
				receiver_key = type_key
				tok = scan.scan()
				if tok != .name && !tok.is_keyword() {
					return error('fastc parser does not support static method declaration in ${path}')
				}
				name = scan.lit
				tok = scan.scan()
			}
			function_key := if is_c_function {
				'C.${name}'
			} else if receiver_type == u32(0) {
				fastc_function_key(header.module_name, name)
			} else {
				'${receiver_key}.${name}'
			}
			mut generic_type_params := map[string]bool{}
			if tok == .lsbr {
				mut generic_depth := 1
				for generic_depth > 0 {
					tok = scan.scan()
					if tok == .eof {
						return error('fastc parser does not support unfinished generic function in ${path}')
					}
					if tok == .lsbr {
						generic_depth++
					} else if tok == .rsbr {
						generic_depth--
					} else if generic_depth == 1 && tok == .name {
						generic_type_params[scan.lit] = true
					}
				}
				tok = scan.scan()
			}
			if tok != .lpar {
				return error('fastc parser does not support function `${name}` declaration in ${path}')
			}
			tok = scan.scan()
			mut is_variadic := false
			for tok != .rpar {
				if is_c_function && tok == .ellipsis {
					mut look := scan
					if look.scan() == .rpar {
						tok = scan.scan()
					} else {
						_, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
							return error('fastc C variadic parameter: ${err.msg()}')
						}
						tok = fastc_scan_skip_semicolons(mut scan, next_token)
					}
					if tok != .rpar {
						return error('fastc C variadic parameter must be last in ${path}')
					}
					is_variadic = true
					break
				}
				mut parameter_is_mut := false
				if tok == .key_mut || (tok == .key_shared && !fastc_shared_parameter_is_name(scan, path, header.module_name, header.imports, declared_types, prefs.building_v)) {
					parameter_is_mut = true
					tok = scan.scan()
				}
				if is_c_function && tok != .name {
					parameter_type_source_id, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
						return error('fastc C function `${name}` parameter: ${err.msg()}')
					}
					parameter_type := parameter_type_source_id

					parameter_types << parameter_type
					parameter_mutability << false
					parameter_constness << false
					parameter_generic_values << false
					tok = next_token
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if !fastc_token_can_be_decl_name(tok) {
					return error('fastc parser does not support function `${name}` parameter token `${tok.str()}` in ${path}')
				}
				parameter_name_or_type := scan.lit
				tok = scan.scan()
				mut parameter_names := [parameter_name_or_type]
				if is_c_function && tok == .dot {
					tok = scan.scan()
					if parameter_name_or_type != 'C' || tok != .name {
						return error('fastc parser does not support qualified C parameter type in ${path}')
					}
					parameter_types << flat.source_type_id(scan.lit)
					parameter_mutability << false
					parameter_constness << false
					parameter_generic_values << false
					tok = scan.scan()
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if is_c_function && tok in [.comma, .rpar] {
					// An unnamed C parameter is just a type. Resolve declared types
					// (e.g. an enum like `CParameter`) as well as primitives.
					type_key := fastc_type_key(header.module_name, parameter_name_or_type)
					parameter_type := if fastc_source_type_key_exists(declared_types, type_key) {
						fastc_c_declared_type_name(type_key)
					} else if fastc_source_type_key_exists(declared_types, parameter_name_or_type) {
						fastc_c_declared_type_name(parameter_name_or_type)
					} else {
						fastc_primitive_c_type(parameter_name_or_type) or {
							return error('fastc parser does not support undeclared C parameter type `${parameter_name_or_type}` in ${path}')
						}
					}
					parameter_types << flat.source_type_id(parameter_type)
					parameter_mutability << false
					parameter_constness << false
					parameter_generic_values << false
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				for tok == .comma {
					tok = scan.scan()
					if !fastc_token_can_be_decl_name(tok) {
						return error('fastc parser does not support grouped function parameter token `${tok.str()}` in ${path}')
					}
					parameter_names << scan.lit
					tok = scan.scan()
				}
				if tok == .ellipsis {
					is_variadic = true
					if is_c_function {
						_, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
							return error('fastc C variadic parameter: ${err.msg()}')
						}
						tok = fastc_scan_skip_semicolons(mut scan, next_token)
						if tok != .rpar {
							return error('fastc C variadic parameter must be last in ${path}')
						}
						break
					}
				}
				is_generic_value := tok == .name && scan.lit in generic_type_params
				parameter_type_source_id_2, next_token := fastc_scan_parameter_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
					return error('fastc function `${name}` parameter: ${err.msg()}')
				}
				parameter_type := parameter_type_source_id_2

				stored_parameter_type := if parameter_is_mut && !flat.source_type_query(parameter_type).has(.c_pointer_suffix) {
					flat.source_type_composition_two(.concat, parameter_type, flat.source_type_id('*'))
				} else {
					parameter_type
				}
				for parameter_name in parameter_names {
					parameter_types << stored_parameter_type
					parameter_mutability << parameter_is_mut
					parameter_constness << is_c_function && parameter_name.starts_with('const_')
					parameter_generic_values << is_generic_value
				}
				tok = fastc_scan_skip_semicolons(mut scan, next_token)
				if tok == .comma {
					tok = scan.scan()
					continue
				}
				if tok != .rpar {
					return error('fastc parser does not support function parameter separator in ${path}')
				}
			}
			tok = scan.scan()
			mut return_type := flat.source_type_void
			mut return_types := []u32{}
			mut option_type := u32(0)
			mut returned_function := FastcFunctionTypeInfo{}
			if tok == .key_fn {
				returned_function = fastc_peek_function_type(scan, path, header.module_name,
					header.imports, declared_types, prefs.building_v)!
			}
			if tok != .lcbr && tok != .semicolon {
				if tok in [.not, .question] {
					return_type = flat.source_type_id(if tok == .not {
						'__v_result'
					} else {
						'Option'
					})
					tok = scan.scan()
					if tok in [.lcbr, .semicolon] {
						option_type = flat.source_type_void
					} else if tok == .lpar {
						return_types_source_ids, tok_scan_token_2_2_2_2_2_2_2 := fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
						return_types = return_types_source_ids.clone()
						tok = tok_scan_token_2_2_2_2_2_2_2

						option_type = flat.source_type_id('MultiReturn')
					} else {
						option_type_source_id, tok_scan_token_2 := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
						option_type = option_type_source_id
						tok = tok_scan_token_2
					}
				} else if tok == .lpar {
					return_types_source_ids_2, tok_scan_token_2_2_2_2_2_2_2_2 := fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					return_types = return_types_source_ids_2.clone()
					tok = tok_scan_token_2_2_2_2_2_2_2_2

					return_type = flat.source_type_id('MultiReturn')
				} else {
					return_type_source_id, tok_scan_token_2_2 := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
						return error('fastc function `${name}` return: ${err.msg()}')
					}
					return_type = return_type_source_id
					tok = tok_scan_token_2_2
				}
			}
			if tok != .lcbr && tok != .semicolon {
				return error('fastc parser does not support function `${name}` body in ${path}')
			}
			fixed_parameter_count := parameter_types.len - if receiver_type == u32(0) {
				0
			} else {
				1
			}
			signature := FastcFunctionSignature{
				parameter_types:               flat.TypeTextList{ ids: parameter_types.clone() }
				parameter_mutability:          parameter_mutability
				parameter_constness:           parameter_constness
				parameter_generic_values:      parameter_generic_values
				return_type:                   return_type
				return_types:                  flat.TypeTextList{ ids: return_types.clone() }
				option_type:                   option_type
				returned_fn_parameter_types:   returned_function.parameter_types
				returned_fn_return_type:       returned_function.return_type
				returned_fn_option_value_type: returned_function.option_value_type
				is_variadic:                   is_variadic
				last_parameter_is_params:      fixed_parameter_count > 0 && fastc_parameter_is_params_struct(parameter_types.last(), params_structs)
				is_public:                     is_public || is_c_function
				is_disabled:                   !next_declaration_is_enabled
				is_c_extern:                   is_c_function && next_declaration_is_c_extern
				is_markused:                   !is_c_function && next_declaration_is_markused
				export_name:                   if is_c_function { '' } else { next_export_name }
				module_name:                   header.module_name
				path:                          path
				position:                      declaration_offset
			}
			if previous := functions[function_key] {
				if !is_c_function {
					is_c_override := previous.path.ends_with('.c.v') || path.ends_with('.c.v')
					if previous.path == path || !is_c_override || !fastc_type_ids_equal(previous.parameter_types.ids.clone(), signature.parameter_types.ids.clone()) || !fastc_bool_types_equal(previous.parameter_mutability, signature.parameter_mutability) || previous.last_parameter_is_params != signature.last_parameter_is_params || previous.return_type != signature.return_type {
						return error('fastc parser does not support duplicate function `${name}` in ${path}')
					}
					if previous.path.ends_with('.c.v') {
						next_declaration_is_enabled = true
						next_declaration_is_c_extern = false
						next_declaration_is_markused = false
						next_export_name = ''
						continue
					}
				}
			}
			functions[function_key] = signature
			next_declaration_is_enabled = true
			next_declaration_is_c_extern = false
			next_declaration_is_markused = false
			next_export_name = ''
			continue
		}
		if brace_depth == 0 && tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union,
			.key_const, .key_global] {
			next_declaration_is_enabled = true
			next_declaration_is_c_extern = false
			next_declaration_is_markused = false
			next_export_name = ''
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
	if header.has_comptime_if {
		fastc_collect_selected_comptime_function_signatures(source, source_offset, path, header, prefs, skips, declared_types, declared_type_c_names, params_structs, mut functions)!
	}
}

fn fastc_collect_global_function_signatures(mut scan scanner.Scanner, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[u32]bool, enabled bool, is_public bool, mut functions map[string]FastcFunctionSignature) !token.Token {
	mut tok := scan.scan()
	grouped := tok == .lpar
	if grouped {
		tok = scan.scan()
	}
	for tok != .eof {
		if (grouped && tok == .rpar) || (!grouped && tok == .semicolon) {
			return scan.scan()
		}
		if tok in [.semicolon, .key_volatile] {
			tok = scan.scan()
			continue
		}
		if tok == .name {
			name := scan.lit
			position := scan.pos
			mut look := scan
			mut type_tok := look.scan()
			mut index_depth := 0
			for type_tok == .lsbr {
				type_tok = fastc_skip_balanced_tokens(mut look, type_tok, .lsbr, .rsbr)!
				index_depth++
			}
			if type_tok == .key_fn {
				signature, _ := fastc_scan_function_alias_signature(mut look, path, header, prefs,
					declared_types)!
				functions[fastc_function_key(header.module_name, name)] = FastcFunctionSignature{
					...signature
					is_global:            true
					is_public:            is_public
					is_disabled:          !enabled
					position:             position
					callable_index_depth: index_depth
				}
			}
			tok = fastc_skip_global_declaration(mut scan, path)!
			continue
		}
		tok = scan.scan()
	}
	return tok
}

// Global collection precedes complete type metadata, so declaration boundaries
// are scanned syntactically. The normal global parser still validates each type.
fn fastc_skip_global_type(mut scan scanner.Scanner, first token.Token, path string) !token.Token {
	mut tok := first
	for tok == .question || tok == .not || tok == .amp || tok == .and || tok == .mul {
		tok = scan.scan()
		if fastc_is_void_return_boundary(tok) {
			return tok
		}
	}
	if tok == .lsbr {
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
		return fastc_skip_global_type(mut scan, tok, path)
	}
	if tok == .lpar {
		return fastc_skip_balanced_tokens(mut scan, tok, .lpar, .rpar)
	}
	if tok == .key_fn {
		tok = scan.scan()
		if tok != .lpar {
			return error('fastc parser does not support function type in ${path}')
		}
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lpar, .rpar)!
		return if fastc_is_void_return_boundary(tok) {
			tok
		} else {
			fastc_skip_global_type(mut scan, tok, path)!
		}
	}
	if tok != .name {
		return error('fastc parser does not support global type in ${path}')
	}
	name := scan.lit
	tok = scan.scan()
	if name == 'map' && tok == .lsbr {
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
		return fastc_skip_global_type(mut scan, tok, path)
	}
	if name in ['chan', 'thread'] && !fastc_is_void_return_boundary(tok) {
		return fastc_skip_global_type(mut scan, tok, path)
	}
	if tok == .dot {
		if scan.scan() != .name {
			return error('fastc parser does not support qualified global type in ${path}')
		}
		tok = scan.scan()
	}
	if tok == .lsbr {
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
	}
	return tok
}

fn fastc_global_declaration_follows(scan scanner.Scanner, first token.Token, path string) bool {
	mut look := scan
	mut tok := first
	if tok == .key_volatile {
		tok = look.scan()
	}
	if tok != .name {
		return false
	}
	tok = look.scan()
	if tok == .assign {
		return true
	}
	if tok !in [.name, .lsbr, .amp, .and, .mul, .question, .not, .key_fn] {
		return false
	}
	_ := fastc_skip_global_type(mut look, tok, path) or { return false }
	return true
}

fn fastc_skip_global_declaration(mut scan scanner.Scanner, path string) !token.Token {
	name := scan.lit
	mut tok := scan.scan()
	if name == 'C' && tok == .dot {
		_ = scan.scan()
		tok = scan.scan()
	}
	if tok != .assign {
		tok = fastc_skip_global_type(mut scan, tok, path)!
	}
	if tok != .assign {
		return tok
	}
	tok = scan.scan()
	if tok in [.name, .lsbr, .amp, .and] {
		mut initializer_type := scan
		boundary := fastc_skip_global_type(mut initializer_type, tok, path) or { token.Token.unknown }
		if boundary == .lcbr {
			// A nested map or array type is one initializer prefix, not a
			// sequence of same-line global declarations after each `]`.
			scan = initializer_type
			tok = boundary
		}
	}
	mut depth := 0
	mut expression_can_end := false
	for tok != .eof {
		if depth == 0 {
			if tok in [.semicolon, .rpar] {
				return tok
			}
			if expression_can_end && fastc_global_declaration_follows(scan, tok, path) {
				return tok
			}
		}
		if tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tok in [.rpar, .rsbr, .rcbr] && depth > 0 {
			depth--
		}
		// A postfix `!` keeps a complete value complete; unary `!` does not.
		if tok !in [.not, .question] {
			expression_can_end = tok in [.name, .number, .string, .char, .rpar, .rsbr, .rcbr,
				.key_true, .key_false, .key_none]
		}
		tok = scan.scan()
	}
	return tok
}

// fastc_scan_function_alias_signature scans the signature after `type Name = fn`
// and returns the token following it. Function-pointer aliases are
// callable locals, so their return and option payload types must survive the alias
// cast used to initialize those locals.
fn fastc_scan_function_alias_signature(mut scan scanner.Scanner, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[u32]bool) !(FastcFunctionSignature, token.Token) {
	mut tok := scan.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type in ${path}')
	}
	tok = scan.scan()
	mut parameter_types := []u32{}
	mut parameter_mutability := []bool{}
	for tok != .rpar {
		if tok in [.comma, .semicolon] {
			tok = scan.scan()
			continue
		}
		mut parameter_is_mut := false
		// Function aliases may omit parameter names, so `shared T` here is always
		// the shared modifier followed by the type rather than a contextual name.
		if tok in [.key_mut, .key_shared] {
			parameter_is_mut = true
			tok = scan.scan()
		}
		if tok in [.name, .key_shared, .key_module, .key_type] && !fastc_parameter_is_type_only(scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) {
			tok = scan.scan()
		}

		parameter_type_source_id_2_2, next_token := fastc_scan_parameter_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
		parameter_type := parameter_type_source_id_2_2

		parameter_types << if parameter_is_mut && !flat.source_type_query(parameter_type).has(.c_pointer_suffix) {
			flat.source_type_composition_two(.concat, parameter_type, flat.source_type_id('*'))
		} else {
			parameter_type
		}
		parameter_mutability << parameter_is_mut
		tok = fastc_scan_skip_semicolons(mut scan, next_token)
	}
	tok = scan.scan()
	mut return_type := flat.source_type_void
	mut return_types := []u32{}
	mut option_type := u32(0)
	if !fastc_is_void_return_boundary(tok) {
		if tok in [.not, .question] {
			return_type = flat.source_type_id(if tok == .not { '__v_result' } else { 'Option' })
			tok = scan.scan()
			if fastc_is_void_return_boundary(tok) {
				option_type = flat.source_type_void
			} else if tok == .lpar {
				return_types_source_ids_2_2, tok_scan_token_2_2_2_2_2_2_2_2_2 := fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				return_types = return_types_source_ids_2_2.clone()
				tok = tok_scan_token_2_2_2_2_2_2_2_2_2

				option_type = flat.source_type_id('MultiReturn')
			} else {
				option_type_source_id_2, tok_scan_token_2_2_2 := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				option_type = option_type_source_id_2
				tok = tok_scan_token_2_2_2
			}
		} else if tok == .lpar {
			return_types_source_ids_2_2_2, tok_scan_token_2_2_2_2_2_2_2_2_2_2 := fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
			return_types = return_types_source_ids_2_2_2.clone()
			tok = tok_scan_token_2_2_2_2_2_2_2_2_2_2

			return_type = flat.source_type_id('MultiReturn')
		} else {
			return_type_source_id_2, tok_scan_token_2_2_2_2 := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
			return_type = return_type_source_id_2
			tok = tok_scan_token_2_2_2_2
		}
	}
	return FastcFunctionSignature{
		parameter_types:      flat.TypeTextList{ ids: parameter_types.clone() }
		parameter_mutability: parameter_mutability
		return_type:          return_type
		return_types:         flat.TypeTextList{ ids: return_types.clone() }
		option_type:          option_type
		is_public:            true
		module_name:          header.module_name
		path:                 path
	}, tok
}

// fastc_bind_legacy_native_aliases retains V functions named by explicit C bindings.
// Marked native hooks also accept the regular C backend's module basename.
fn fastc_bind_legacy_native_aliases(mut functions map[string]FastcFunctionSignature) {
	fastc_bind_native_aliases(true, mut functions)
}

fn fastc_bind_native_aliases(require_markused bool, mut functions map[string]FastcFunctionSignature) {
	mut candidates := map[string]string{}
	mut ambiguous := map[string]bool{}
	for key, signature in functions {
		name := key.all_after_last('.')
		if key.starts_with('C.') || signature.is_disabled || (require_markused && !signature.is_markused)
			|| signature.is_global || key != fastc_function_key(signature.module_name, name) {
			continue
		}
		qualified := fastc_signature_c_name(key, signature)
		legacy := '${signature.module_name.all_after_last('.')}__${name}'
		c_name := if 'C.${qualified}' in functions {
			qualified
		} else if signature.is_markused || 'C.${legacy}' in functions {
			legacy
		} else {
			qualified
		}
		if c_name in candidates {
			ambiguous[c_name] = true
		} else {
			candidates[c_name] = key
		}
	}
	for c_name, key in candidates {
		if ambiguous[c_name] {
			continue
		}
		binding := functions['C.${c_name}'] or { continue }
		signature := functions[key]
		if binding.is_disabled || binding.is_variadic || signature.is_variadic
			|| signature.export_name != ''
			|| binding.parameter_types.ids.len != signature.parameter_types.ids.len {
			continue
		}
		// The existing export wrapper preserves internal V names and widths while
		// adapting native callers to the explicit C declaration's ABI.
		functions[key] = FastcFunctionSignature{
			...signature
			export_name: c_name
		}
	}
}

fn fastc_function_is_native_root(key string, signature FastcFunctionSignature) bool {
	// FastC supplies its own startup sequence instead of the regular C backend's
	// builtin and GC hooks. An actual call still retains either hook through
	// normal reachability.
	is_builtin_startup := key == fastc_function_key('builtin', 'builtin_init')
		|| key == fastc_function_key('builtin', 'gc_runtime_init')
	return !signature.is_disabled
		&& (signature.export_name != '' || (signature.is_markused && !is_builtin_startup))
}

fn fastc_native_pair_wrapper_name(c_name string) string {
	return 'v_fastc_native_pair_${c_name}'
}

fn (g &Parser) has_native_pair_return(signature FastcFunctionSignature) bool {
	if !g.prefs.target_libc_headers || signature.is_disabled || signature.is_variadic
		|| signature.is_c_extern || signature.is_global
		|| signature.return_type != flat.source_type_id('MultiReturn') || signature.return_types.ids.len != 2 {
		return false
	}
	return g.underlying_alias_type(signature.return_types.ids[0]) == flat.source_type_u64
		&& g.underlying_alias_type(signature.return_types.ids[1]) == flat.source_type_u64
}

// Raw native entry points use the platform's two-register tuple ABI. Typed V
// calls retain MultiReturn, including typed function values.
fn (g &Parser) render_native_function_pointer(tokens []FastcExpressionToken, expected_type u32) ?string {
	if !g.prefs.target_libc_headers || g.underlying_alias_type(expected_type) != flat.source_type_voidptr {
		return none
	}
	operand := fastc_strip_paren_tokens(tokens)
	if _ := g.render_function_value_expression(operand) {
		function_key := g.function_key_for_call(operand, operand.len - 1)
		if function_key.starts_with('C.') {
			return none
		}
		signature := g.functions[function_key] or { return none }
		if g.has_native_pair_return(signature) {
			return '&${fastc_native_pair_wrapper_name(g.c_function_name_for_key(function_key))}'
		}
	}
	return none
}

fn fastc_collect_referenced_function_names(sources []FastcSourceFile, prefs &pref.Preferences, functions map[string]FastcFunctionSignature) map[string]bool {
	mut available_names := map[string]bool{}
	for key in functions.keys() {
		available_names[key.all_after_last('.')] = true
	}
	mut references := map[string]map[string]bool{}
	mut top_level_references := map[string]bool{}
	fastc_collect_reference_partials(sources, prefs, available_names, mut references, mut top_level_references)
	mut used := {
		'main':                     true
		'run':                      true
		// Capturing literals and their scoped cleanup call these from generated C.
		'closure_create_with_data': true
		'closure_try_destroy':      true
		'memdup':                   true
		// `select` is a keyword, so a `sql db { select ... }` block does not surface a
		// `.select(` reference the way `insert`/`update`/... do; seed it so a connection
		// type's `select` method survives reachability pruning for the ORM lowering.
		'select':                   true
		// `arr.sort(a < b)` lowers to a `sort_with_compare` call emitted in generated C,
		// so no source `.sort_with_compare(` reference surfaces; seed it (its body pulls in
		// vqsort transitively) so it survives reachability pruning.
		'sort_with_compare':        true
		// Channel operations are emitted directly by the scanner lowering.
		'new_channel_st':           true
		'pop':                      true
		'closed_error':             true
		'closed':                   true
		'len':                      true
		'channel_select_lang':      true
		'free':                     true
		'try_push':                 true
		'try_pop':                  true
		'panic_result_not_set':     true
		'array_push':               true
		'push':                     true
		'push_many':                true
		'array_slice':              true
		'slice':                    true
		// A gated slice `a#[i..j]` lowers to these, which no source text names.
		'slice_ni':                 true
		'substr_ni':                true
		'at':                       true
		'keys':                     true
		'get':                      true
		'get_check':                true
		'new_map':                  true
		'set':                      true
		'values':                   true
		'map_hash_string':          true
		'map_eq_string':            true
		'map_clone_string':         true
		'map_free_string':          true
		'map_free_nop':             true
		'map_hash_int_1':           true
		'map_hash_int_2':           true
		'map_hash_int_4':           true
		'map_hash_int_8':           true
		'map_hash_int_16':          true
		'map_eq_int_1':             true
		'map_eq_int_2':             true
		'map_eq_int_4':             true
		'map_eq_int_8':             true
		'map_eq_int_16':            true
		'map_clone_int_1':          true
		'map_clone_int_2':          true
		'map_clone_int_4':          true
		'map_clone_int_8':          true
		'map_clone_int_16':         true
		'new_array_from_c_array':   true
		'string_plus_many':         true
		'v_fixed_index':            true
		// `m1 == m2` lowers to a `builtin__map_map_eq` call in generated C, so no source
		// `.map_map_eq(` reference surfaces; seed it so it survives reachability pruning.
		'map_map_eq':               true
	}
	for name in top_level_references.keys() {
		used[name] = true
	}
	// Native callers enter exported V functions without a V call site. Their
	// transitive callees must participate in the same reachability traversal.
	for key, signature in functions {
		if fastc_function_is_native_root(key, signature) {
			used[key.all_after_last('.')] = true
		}
	}
	// A test build calls the test file's tests from the main it synthesizes.
	if prefs.is_test {
		for key, signature in functions {
			name := key.all_after_last('.')
			if fastc_is_test_harness_function(name) && fastc_is_test_source(signature.path, prefs) {
				used[name] = true
			}
		}
	}
	// Operator overload declarations are emitted even when their symbolic names do
	// not appear as ordinary call tokens. Treat those symbols as roots too, so the
	// private helper methods called by an overload body are retained.
	for name in references.keys() {
		if name.len > 0 && !name[0].is_letter() && name[0] != `_` {
			used[name] = true
		}
	}
	// Reachability by worklist BFS. The previous fixpoint re-scanned every
	// discovered name on every pass and cloned each name's reference set per
	// visit; under -prealloc those clones were never reclaimed. Each name is
	// now expanded exactly once and its reference set is iterated in place.
	mut worklist := used.keys()
	for worklist.len > 0 {
		name := worklist.pop()
		if name !in references {
			continue
		}
		for referenced_name, _ in references[name] {
			if referenced_name !in used {
				used[referenced_name] = true
				worklist << referenced_name
			}
		}
	}
	return used
}

fn fastc_collect_type_default_references(mut scan scanner.Scanner, first token.Token, available_names map[string]bool, mut references map[string]bool) token.Token {
	mut tok := first
	for tok !in [.lcbr, .eof] {
		tok = scan.scan()
	}
	if tok == .eof {
		return tok
	}
	mut depth := 1
	mut in_default := false
	tok = scan.scan()
	for depth > 0 && tok != .eof {
		if depth == 1 && tok == .assign {
			in_default = true
		} else if depth == 1 && tok == .semicolon {
			in_default = false
		} else if in_default && tok == .name && scan.lit in available_names {
			references[scan.lit] = true
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
		}
		tok = scan.scan()
	}
	return tok
}

fn collect_interface_method_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, declared_types map[u32]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut interface_field_paths map[string]string, mut embed_embedders []u32, mut embed_embeddeds []u32) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	mut depth := 0
	mut skip_index := 0
	mut next_declaration_is_enabled := true
	for tok != .eof {
		if depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if depth == 0 && tok in [.key_module, .key_import] {
			tok = fastc_skip_module_or_import(mut scan, tok, path)!
			continue
		}
		if depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_interface_method_signatures(selected.source, path, header, prefs, []int{}, declared_types, mut functions, mut interface_methods, mut interface_fields, mut interface_field_paths, mut embed_embedders, mut embed_embeddeds)!
				}
				tok = selected.tok
				continue
			}
		}
		if depth == 0 && tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_declaration_is_enabled = next_declaration_is_enabled && attribute.is_enabled
			continue
		}
		if depth == 0 && tok == .key_interface && !next_declaration_is_enabled {
			tok = fastc_skip_type_declaration(mut scan, tok)!
			next_declaration_is_enabled = true
			continue
		}
		if depth != 0 || tok != .key_interface {
			if depth == 0 && tok in [.key_fn, .key_struct, .key_enum, .key_type, .key_union,
				.key_const, .key_global] {
				next_declaration_is_enabled = true
			}
			if tok == .lcbr && depth == 0 {
				skipped, next_skip := fastc_skip_recorded_body(mut scan, skips, skip_index)
				skip_index = next_skip
				if skipped {
					tok = scan.scan()
					continue
				}
			}
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr && depth > 0 {
				depth--
			}
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support interface declaration in ${path}')
		}
		interface_key := fastc_type_key(header.module_name, scan.lit)
		interface_type := fastc_c_declared_type_name(interface_key)
		tok = scan.scan()
		if tok == .lsbr {
			tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
		}
		if tok != .lcbr {
			return error('fastc parser does not support interface body in ${path}')
		}
		tok = scan.scan()
		mut members_are_mutable := false
		for tok != .rcbr && tok != .eof {
			if tok in [.semicolon, .comma] {
				tok = scan.scan()
				continue
			}
			if tok == .key_mut {
				members_are_mutable = true
				tok = scan.scan()
				if tok == .colon {
					tok = scan.scan()
				}
				continue
			}
			if tok == .key_pub {
				tok = scan.scan()
				if tok == .key_mut {
					members_are_mutable = true
					tok = scan.scan()
				}
				if tok == .colon {
					tok = scan.scan()
				}
				continue
			}
			if tok != .name && !tok.is_keyword() {
				tok = scan.scan()
				continue
			}
			// An interface member may be named with a word that is also a keyword
			// (`select(...)`, `lock`, ...); the scanner still exposes the spelling via
			// `scan.lit`, matching how method definitions accept keyword names.
			mut member_names := [scan.lit]
			tok = scan.scan()
			for tok == .comma {
				tok = scan.scan()
				if tok != .name {
					return error('fastc parser does not support interface field declaration in ${path}')
				}
				member_names << scan.lit
				tok = scan.scan()
			}
			if tok != .lpar {
				if tok in [.semicolon, .rcbr] {
					// A member that is a bare type name with no `(` is an embedded
					// interface (`interface B { A }`). Record it so A's methods can be
					// promoted onto B once every interface has been collected.
					if member_names.len == 1 {
						if embedded_key := fastc_resolve_declared_type_key(header.module_name, member_names[0], header.imports, declared_types) {
							embed_embedders << flat.source_type_id(interface_key)
							embed_embeddeds << embedded_key
						}
					}
					if tok == .semicolon {
						tok = scan.scan()
					}
					continue
				}
				field_type_source_id, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				field_type := field_type_source_id

				for field_name in member_names {
					field_key := '${interface_key}.${field_name}'
					if field_key in interface_fields {
						return error('fastc parser does not support duplicate interface field `${field_name}` in ${path}')
					}
					interface_fields[field_key] = FastcInterfaceField{
						name:       field_name
						typ:        field_type
						is_mutable: members_are_mutable
					}
					interface_field_paths[field_key] = path
				}
				tok = next_token
				if tok == .semicolon {
					tok = scan.scan()
				}
				continue
			}
			if member_names.len != 1 {
				return error('fastc parser does not support grouped interface methods in ${path}')
			}
			method_name := member_names[0]
			mut parameter_types := flat.TypeTextList.from_names([interface_type]).ids
			mut parameter_mutability := [members_are_mutable]
			tok = scan.scan()
			for tok != .rpar {
				mut parameter_is_mut := false
				// Interface parameters may be type-only, so `shared T` is always the
				// shared modifier followed by the type, matching the main parser.
				if tok in [.key_mut, .key_shared] {
					parameter_is_mut = true
					tok = scan.scan()
				}
				// Interface method parameters may be unnamed (just a type), e.g.
				// `handle(Request) Response`. When a leading plain name is followed by
				// another type token it is the parameter name; otherwise the name
				// itself starts the (unnamed) type.
				if tok in [.name, .key_shared, .key_module, .key_type] {
					mut lookahead := scan
					after := lookahead.scan()
					if after !in [.comma, .rpar, .dot] {
						tok = scan.scan()
					}
				}
				parameter_type_source_id_2_2_2, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				parameter_type := parameter_type_source_id_2_2_2

				parameter_types << if parameter_is_mut && !flat.source_type_query(parameter_type).has(.c_pointer_suffix) {
					flat.source_type_composition_two(.concat, parameter_type, flat.source_type_id('*'))
				} else {
					parameter_type
				}
				parameter_mutability << parameter_is_mut
				tok = fastc_scan_skip_semicolons(mut scan, next_token)
				if tok == .comma {
					tok = scan.scan()
				}
			}
			tok = scan.scan()
			mut return_type := flat.source_type_void
			mut return_types := []u32{}
			mut option_type := u32(0)
			if tok !in [.semicolon, .rcbr] {
				if tok in [.not, .question] {
					return_type = flat.source_type_id(if tok == .not {
						'__v_result'
					} else {
						'Option'
					})
					tok = scan.scan()
					if tok in [.semicolon, .rcbr] {
						option_type = flat.source_type_void
					} else if tok == .lpar {
						components, next := fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
						return_types = components.clone()
						option_type = fastc_type_multi_return
						tok = next
					} else {
						option_type_source_id_2_2, tok_scan_token_2_2_2_2_2 := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
						option_type = option_type_source_id_2_2
						tok = tok_scan_token_2_2_2_2_2
					}
				} else if tok == .lpar {
					return_types_source_ids_2_2_2_2, tok_scan_token_2_2_2_2_2_2_2_2_2_2_2 := fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					return_types = return_types_source_ids_2_2_2_2.clone()
					tok = tok_scan_token_2_2_2_2_2_2_2_2_2_2_2

					return_type = flat.source_type_id('MultiReturn')
				} else {
					return_type_source_id_2_2, tok_scan_token_2_2_2_2_2_2 := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					return_type = return_type_source_id_2_2
					tok = tok_scan_token_2_2_2_2_2_2
				}
			}
			interface_method_key := '${interface_key}.${method_name}'
			functions[interface_method_key] = FastcFunctionSignature{
				parameter_types:      flat.TypeTextList{ ids: parameter_types.clone() }
				parameter_mutability: parameter_mutability
				return_type:          return_type
				return_types:         flat.TypeTextList{ ids: return_types.clone() }
				option_type:          option_type
				is_public:            true
				module_name:          header.module_name
				path:                 path
			}
			interface_methods[interface_method_key] = true
		}
		if tok == .rcbr {
			tok = scan.scan()
		}
		next_declaration_is_enabled = true
	}
}

struct FastcSignaturePartial {
mut:
	functions             map[string]FastcFunctionSignature
	interface_methods     map[string]bool
	interface_fields      map[string]FastcInterfaceField
	interface_field_paths map[string]string
	embed_embedders       []u32
	embed_embeddeds       []u32
	failed                bool
	error_message         string
}

fn fastc_collect_signature_chunk(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[u32]bool, declared_type_c_names flat.TypeTextBindings, params_structs map[string]bool, start int, end int) FastcSignaturePartial {
	mut partial := FastcSignaturePartial{
		functions:             map[string]FastcFunctionSignature{}
		interface_methods:     map[string]bool{}
		interface_fields:      map[string]FastcInterfaceField{}
		interface_field_paths: map[string]string{}
		embed_embedders:       []u32{}
		embed_embeddeds:       []u32{}
	}
	for idx in start .. end {
		source_file := sources[idx]
		collect_function_signatures(source_file.source, source_file.path, source_file.header, prefs, source_file.header.body_spans, declared_types, declared_type_c_names, params_structs, mut partial.functions) or {
			partial.failed = true
			partial.error_message = err.msg()
			return partial
		}
		if !source_file.header.has_interfaces {
			continue
		}
		collect_interface_method_signatures(source_file.source, source_file.path, source_file.header, prefs, source_file.header.body_spans, declared_types, mut partial.functions, mut partial.interface_methods, mut partial.interface_fields, mut partial.interface_field_paths, mut partial.embed_embedders, mut partial.embed_embeddeds) or {
			partial.failed = true
			partial.error_message = err.msg()
			return partial
		}
	}
	return partial
}

// fastc_signature_partial_count sums the function signatures of `partials`.
fn fastc_signature_partial_count(partials []FastcSignaturePartial) int {
	mut count := 0
	for partial in partials {
		count += partial.functions.len
	}
	return count
}

fn fastc_merge_signature_partial(partial FastcSignaturePartial, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []u32, mut embed_embeddeds []u32) ! {
	if partial.failed {
		return error(partial.error_message)
	}
	for key, signature in partial.functions {
		// Duplicates are rare, so test membership first and only then copy
		// the previous signature out for the override checks.
		if key !in partial.interface_methods && key in functions {
			previous := functions[key]
			if !key.starts_with('C.') {
				is_c_override := previous.path.ends_with('.c.v') || signature.path.ends_with('.c.v')
				if previous.path == signature.path || !is_c_override || !fastc_type_ids_equal(previous.parameter_types.ids.clone(), signature.parameter_types.ids.clone()) || !fastc_bool_types_equal(previous.parameter_mutability, signature.parameter_mutability) || previous.last_parameter_is_params != signature.last_parameter_is_params || previous.return_type != signature.return_type {
					return error('fastc parser does not support duplicate function `${key.all_after_last('.')}` in ${signature.path}')
				}
				if previous.path.ends_with('.c.v') {
					continue
				}
			}
		}
		functions[key] = signature
	}
	for key, _ in partial.interface_methods {
		interface_methods[key] = true
	}
	for key, field in partial.interface_fields {
		if key in interface_fields {
			return error('fastc parser does not support duplicate interface field `${field.name}` in ${partial.interface_field_paths[key]}')
		}
		interface_fields[key] = field
	}
	for embedder in partial.embed_embedders {
		embed_embedders << embedder
	}
	for embedded in partial.embed_embeddeds {
		embed_embeddeds << embedded
	}
}

fn fastc_scan_multi_return_types(mut scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) !([]u32, token.Token) {
	mut types := []u32{}
	mut tok := scan.scan()
	for tok != .rpar {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		payload := if tok == .question {
			fastc_peek_option_element(scan, path, module_name, imports, declared_types, allow_short_placeholders)
		} else {
			u32(0)
		}
		component_type_source_id, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		component_type := if payload != 0 && component_type_source_id == fastc_type_option {
			flat.source_type_id(fastc_typed_option_type(payload))
		} else {
			component_type_source_id
		}

		types << component_type
		tok = next_token
		if tok == .comma {
			tok = scan.scan()
		}
		if tok == .eof {
			return error('fastc parser does not support unfinished multi-return type in ${path}')
		}
	}
	return types, scan.scan()
}

// fastc_peek_chan_element scans the element type of a `chan Elem` type from a scanner
// positioned just after the `chan` keyword, returning its C type ('' if erased/none).
// Works on a copy, so the real scanner is not advanced.
fn fastc_peek_chan_element(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) u32 {
	mut probe := scan
	elem_tok := probe.scan()
	if elem_tok in [token.Token.comma, .rpar, .lcbr, .semicolon, .assign, .rcbr, .attribute, .eof] {
		return u32(0)
	}
	elem_c_source_id, _ := fastc_scan_type(mut probe, elem_tok, path, module_name, imports, declared_types, allow_short_placeholders) or { return u32(0) }
	elem_c := elem_c_source_id

	return elem_c
}

// fastc_peek_option_element recovers the wrapped C value type of an option field (`f ?T`),
// scanning a copy positioned just after the leading `?`. Returns '' for a bare `?` (no
// following type) or an unresolvable type.
fn fastc_peek_option_element(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) u32 {
	mut probe := scan
	elem_tok := probe.scan()
	if elem_tok in [token.Token.comma, .rpar, .lcbr, .semicolon, .assign, .rcbr, .attribute, .eof] {
		return u32(0)
	}
	elem_c_source_id_2, _ := fastc_scan_type(mut probe, elem_tok, path, module_name, imports, declared_types, allow_short_placeholders) or { return u32(0) }
	elem_c := elem_c_source_id_2

	return elem_c
}

// fastc_peek_generic_type_argument recovers the first concrete argument from a
// generic field type such as `Stack[Item]`. The scanner copy starts immediately
// after `first`, so this does not advance declaration scanning.
fn fastc_peek_generic_type_argument(first token.Token, scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) u32 {
	if first != .name {
		return u32(0)
	}
	mut probe := scan
	mut tok := probe.scan()
	if tok == .dot {
		if probe.scan() != .name {
			return u32(0)
		}
		tok = probe.scan()
	}
	if tok != .lsbr {
		return u32(0)
	}
	arg_tok := probe.scan()
	if arg_tok == .rsbr {
		return u32(0)
	}
	argument_type_source_id, next_token := fastc_scan_type(mut probe, arg_tok, path, module_name, imports, declared_types, allow_short_placeholders) or { return u32(0) }
	argument_type := argument_type_source_id

	if next_token !in [.comma, .rsbr] {
		return u32(0)
	}
	return argument_type
}

struct FastcFunctionTypeInfo {
	parameter_types   flat.TypeTextList
	return_type       u32
	option_value_type u32
}

fn fastc_is_void_return_boundary(tok token.Token) bool {
	return tok in [.semicolon, .comma, .rpar, .lcbr, .assign, .attribute, .rcbr, .eof]
}

// fastc_peek_function_type preserves the signature that fastc_scan_type erases to
// `voidptr`. The scanner copy is positioned immediately after the leading `fn`.
fn fastc_peek_function_type(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) !FastcFunctionTypeInfo {
	mut look := scan
	mut tok := look.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type in ${path}')
	}
	tok = look.scan()
	mut parameter_types := []u32{}
	for tok != .rpar {
		if tok in [.comma, .semicolon] {
			tok = look.scan()
			continue
		}
		mut parameter_is_mut := false
		if tok == .key_mut {
			parameter_is_mut = true
			tok = look.scan()
		}
		if !fastc_parameter_is_type_only(look, tok, path, module_name, imports, declared_types,
			allow_short_placeholders) {
			tok = look.scan()
		}
		parameter_type_source_id_2_2_2_2, next_token := fastc_scan_type(mut look, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		parameter_type := parameter_type_source_id_2_2_2_2

		parameter_types << if parameter_is_mut && !flat.source_type_query(parameter_type).has(.c_pointer_suffix) {
			flat.source_type_composition_two(.concat, parameter_type, flat.source_type_id('*'))
		} else {
			parameter_type
		}
		tok = fastc_scan_skip_semicolons(mut look, next_token)
	}
	tok = look.scan()
	mut return_type := flat.source_type_void
	mut option_value_type := u32(0)
	if tok in [.not, .question] {
		return_type = flat.source_type_id(if tok == .not { '__v_result' } else { 'Option' })
		value_tok := look.scan()
		if fastc_is_void_return_boundary(value_tok) {
			option_value_type = flat.source_type_void
		} else {
			option_value_type, _ = fastc_scan_type(mut look, value_tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
	} else if tok !in [.semicolon, .comma, .rpar, .lcbr, .assign, .attribute, .rcbr, .eof] {
		return_type_source_id_2_2_2, _ := fastc_scan_type(mut look, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		return_type = return_type_source_id_2_2_2
	}
	return FastcFunctionTypeInfo{
		parameter_types:   flat.TypeTextList{ ids: parameter_types.clone() }
		return_type:       return_type
		option_value_type: option_value_type
	}
}

// fastc_parameter_is_type_only recognizes one whole unnamed parameter type ending at a
// separator. Parameters are classified separately because function types can mix names and types.
fn fastc_parameter_is_type_only(scan scanner.Scanner, first token.Token, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) bool {
	mut look := scan
	_, next_token := fastc_scan_type(mut look, first, path, module_name, imports, declared_types,
		allow_short_placeholders) or { return false }
	return next_token in [.comma, .semicolon, .rpar]
}

fn fastc_scan_type(mut scan scanner.Scanner, first token.Token, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) !(u32, token.Token) {
	typ, next_token := fastc_scan_type_bytes(mut scan, first, path, module_name, imports, declared_types, allow_short_placeholders)!
	return flat.source_type_id(typ), next_token
}

fn fastc_scan_type_bytes(mut scan scanner.Scanner, first token.Token, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) !(string, token.Token) {
	mut tok := first
	mut optional := false
	wrapper := if first == .not { '__v_result' } else { 'Option' }
	if tok in [.question, .not] {
		optional = true
		tok = scan.scan()
	}
	mut pointers := 0
	for tok == .amp || tok == .and || tok == .mul {
		pointers += if tok == .and { 2 } else { 1 }
		tok = scan.scan()
	}
	// `&?T` / `&!T`: a pointer to an option or result value.
	if !optional && pointers > 0 && tok in [.question, .not] {
		inner_type, next_token := fastc_scan_type_bytes(mut scan, tok, path, module_name, imports,
			declared_types, allow_short_placeholders)!
		return inner_type + '*'.repeat(pointers), next_token
	}
	if optional && fastc_is_void_return_boundary(tok) {
		return wrapper + '*'.repeat(pointers), tok
	}
	if tok == .ellipsis {
		tok = scan.scan()
		element_type, next_token := fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		tok = next_token
		return fastc_array_c_type(element_type) + '*'.repeat(pointers), tok
	}
	if tok == .lpar {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished multi-return type in ${path}')
			}
			if tok == .lpar {
				depth++
			} else if tok == .rpar {
				depth--
			}
		}
		tok = scan.scan()
		return 'MultiReturn' + '*'.repeat(pointers), tok
	}
	if tok == .lsbr {
		tok = scan.scan()
		mut is_dynamic := false
		mut fixed_length := ''
		if tok == .rsbr {
			is_dynamic = true
			tok = scan.scan()
		} else if tok == .dollar {
			// `[$d('name', 32)]T`: the `-d name=N` value, else the default.
			mut define := ''
			mut default_length := ''
			tok = scan.scan()
			if tok == .name && scan.lit == 'd' {
				tok = scan.scan()
				if tok == .lpar {
					tok = scan.scan()
					if tok == .string {
						define = scan.lit.trim('\'"')
						tok = scan.scan()
					}
					if tok == .comma {
						tok = scan.scan()
						if tok == .number {
							default_length = scan.lit
							tok = scan.scan()
						}
					}
					if tok == .rpar {
						tok = scan.scan()
					}
				}
			}
			if tok == .rsbr && define != '' && default_length != '' {
				fixed_length = scan.compile_value(define) or { default_length }
				tok = scan.scan()
			} else {
				mut depth := 1
				for depth > 0 && tok != .eof {
					if tok == .lsbr {
						depth++
					} else if tok == .rsbr {
						depth--
					}
					tok = scan.scan()
				}
			}
		} else {
			mut dimension := []FastcExpressionToken{}
			mut depth := 0
			for tok != .rsbr || depth != 0 {
				if tok == .eof {
					return error('fastc parser does not support unfinished fixed array dimension in ${path}')
				}
				if tok == .lpar { depth++ }
				if tok == .rpar { depth-- }
				if depth < 0 {
					return error('fastc parser does not support invalid fixed array dimension in ${path}')
				}
				if tok != .semicolon {
					dimension << FastcExpressionToken{ tok: tok, lit: scan.lit }
				}
				tok = scan.scan()
			}
			length_source, end := fastc_fixed_length_expression(dimension, 0, 1, module_name, imports, declared_types)!
			if end != dimension.len {
				return error('fastc parser does not support fixed array dimension in ${path}')
			}
			fixed_length = length_source
			tok = scan.scan()
		}
		if !is_dynamic && fixed_length == '' {
			return error('fastc parser does not support fixed array dimension in ${path}')
		}
		optional_element := tok == .question
		if optional_element { tok = scan.scan() }
		payload, next_token := fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		element_type := if optional_element {
			fastc_typed_option_type(flat.source_type_id(payload))
		} else {
			payload
		}
		tok = next_token
		array_type := if optional {
			wrapper
		} else if is_dynamic {
			fastc_array_c_type(element_type)
		} else if fixed_length != '' {
			fastc_fixed_array_type(fixed_length, element_type)
		} else {
			'array'
		}
		return array_type + '*'.repeat(pointers), tok
	}
	if tok == .key_fn {
		info := fastc_peek_function_type(scan, path, module_name, imports, declared_types, allow_short_placeholders)!
		mut paren_depth := 0
		for {
			tok = scan.scan()
			if tok == .lpar {
				paren_depth++
			} else if tok == .rpar {
				paren_depth--
				if paren_depth == 0 {
					tok = scan.scan()
					break
				}
			} else if tok == .eof {
				return error('fastc parser does not support unfinished function type in ${path}')
			}
		}
		if tok == .name || tok == .amp || tok == .and || tok == .mul || tok == .question || tok == .not || tok == .lsbr || tok == .lpar || tok == .key_fn {
			_, tok = fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
		return if optional { wrapper } else { fastc_callable_type_name(info) } + '*'.repeat(pointers), tok
	}
	if tok == .key_none {
		tok = scan.scan()
		return 'voidptr' + '*'.repeat(pointers), tok
	}
	if tok != .name {
		return error('fastc parser does not support type `${tok.str()}` in ${path}')
	}
	mut raw_type := scan.lit
	tok = scan.scan()
	if raw_type == 'chan' {
		if tok !in [.comma, .rpar, .lcbr, .semicolon, .assign] {
			_, tok = fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
		channel_type := if optional { wrapper } else { 'chan' + '*'.repeat(pointers) }
		return channel_type, tok
	}
	if raw_type == 'thread' {
		// `thread`, `thread T`, `thread !`, `thread ?`: a spawned-thread handle. Its
		mut value_type := ''
		if tok in [.not, .question] {
			value_type = if tok == .not { '__v_result' } else { 'Option' }
			tok = scan.scan()
		} else if tok !in [.comma, .rpar, .lcbr, .semicolon, .assign] {
			value_type, tok = fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
		thread_type := if optional {
			wrapper
		} else {
			fastc_thread_type_name(value_type) + '*'.repeat(pointers)
		}
		return thread_type, tok
	}
	if raw_type == 'map' && tok == .lsbr {
		tok = scan.scan()
		key_type, next_key_token := fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		tok = next_key_token
		if tok != .rsbr {
			return error('fastc parser does not support unfinished map key type in ${path}')
		}
		tok = scan.scan()
		optional_value := tok == .question
		if optional_value { tok = scan.scan() }
		payload, next_value_token := fastc_scan_type_bytes(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		value_type := if optional_value {
			fastc_typed_option_type(flat.source_type_id(payload))
		} else {
			payload
		}
		tok = next_value_token
		base := if optional { wrapper } else { fastc_map_c_type(key_type, value_type) }
		return base + '*'.repeat(pointers), tok
	}
	mut type_module := module_name
	mut is_imported_type := false
	if tok == .dot {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support qualified type `${raw_type}` in ${path}')
		}
		if raw_type == 'C' {
			type_module = 'C'
		} else if raw_type == module_name.all_after_last('.') {
			type_module = module_name
		} else {
			type_module = imports[raw_type] or {
				return error('fastc parser does not support unknown module qualifier `${raw_type}` in ${path}')
			}
			is_imported_type = type_module != module_name
		}
		raw_type = scan.lit
		tok = scan.scan()
	} else if imported_module := imports[fastc_selective_import_key(raw_type)] {
		type_module = imported_module
		is_imported_type = type_module != module_name
	}
	type_key := fastc_type_key(type_module, raw_type)
	if is_imported_type && type_module != 'builtin' && fastc_source_type_key_exists(declared_types, type_key) && !fastc_source_type_key_value(declared_types, type_key) {
		return error('fastc parser does not support private type `${raw_type}` from imported module `${type_module}` in ${path}')
	}
	mut base := ''
	if type_module == 'C' {
		base = if fastc_source_type_key_exists(declared_types, '#Cstruct#${raw_type}') {
			'struct ${raw_type}'
		} else {
			raw_type
		}
	} else if fastc_source_type_key_exists(declared_types, type_key) {
		base = fastc_c_declared_type_name(type_key)
	} else if fastc_source_type_key_exists(declared_types, raw_type) {
		// Builtin declarations use their unqualified spelling as the canonical key.
		base = fastc_c_declared_type_name(raw_type)
	} else if raw_type.contains('__') && fastc_source_type_key_exists(declared_types, raw_type.replace('__', '.')) {
		// Cross-module monomorphization substitutes an already-resolved C spelling
		// (`config__Config`) into the defining module's generic source.
		base = raw_type
	} else if raw_type.starts_with('Array_') || raw_type.starts_with('Map_') || raw_type.starts_with('FixedArray_') || fastc_callable_signature(flat.source_type_id(raw_type)) != none || fastc_typed_option_payload(flat.source_type_id(raw_type)) != none {
		// On-demand monomorphization likewise substitutes FastC's already-resolved
		// composite spelling (`Array_string`, `Map_string_int`, ...). Keep it as the
		// concrete C type when the generated instance is scanned again.
		base = raw_type
	} else {
		base = fastc_primitive_c_type(raw_type) or { '' }
	}
	if base == '' {
		if !fastc_source_type_key_exists(declared_types, type_key) {
			if allow_short_placeholders && raw_type.len <= 3 && raw_type[0].is_capital() {
				base = 'voidptr'
			}
			if base == '' {
				return error('fastc parser does not support undeclared type `${raw_type}` before `${tok.str()}` at byte ${scan.pos} in ${path}')
			}
		} else {
			base = fastc_c_declared_type_name(type_key)
		}
	}
	if tok == .lsbr {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished generic type in ${path}')
			}
			if tok == .lsbr {
				depth++
			} else if tok == .rsbr {
				depth--
			}
		}
		tok = scan.scan()
	}
	if optional {
		return wrapper, tok
	}
	return base + '*'.repeat(pointers), tok
}

fn fastc_composite_type_part(typ string) string {
	return flat.source_type_name(flat.source_type_composition_one(.fastc_composite_part, flat.source_type_id(typ)))
}

fn fastc_fixed_array_type(length string, element_type string) string {
	return flat.source_type_name(fastc_fixed_array_type_id(length, flat.source_type_id(element_type)))
}

fn fastc_fixed_array_type_id(length string, element_type u32) u32 {
	return flat.source_type_composition(.concat, [flat.source_type_id('FixedArray_'),
		flat.source_type_id(fastc_fixed_array_length_part(length)),
		flat.source_type_id('_FASTC_ARRAY_OF_'), fastc_composite_part_id(element_type)])
}

fn fastc_fixed_array_length_part(length string) string {
	return if length.bytes().all(fastc_identifier_byte(it)) {
		length
	} else {
		'EXPRHEX' + hex.encode(length.bytes())
	}
}

fn fastc_fixed_array_length(typ string) ?string {
	return fastc_fixed_array_length_id(flat.source_type_id(typ))
}

fn fastc_fixed_array_length_id(typ_id u32) ?string {
	syntax := flat.source_type_syntax(typ_id)
	if !syntax.fastc_fixed_array_length_valid() {
		return none
	}
	return flat.source_type_name(syntax.fastc_fixed_array_length())
}

fn fastc_fixed_length_expression(tokens []FastcExpressionToken, start int, minimum_precedence int, module_name string, imports map[string]string, declared_types map[u32]bool) !(string, int) {
	mut left, mut index := fastc_fixed_length_atom(tokens, start, module_name, imports, declared_types)!
	for index < tokens.len {
		op := tokens[index].tok
		if op !in [.plus, .minus, .mul, .div, .mod, .left_shift, .right_shift, .amp, .pipe, .xor] {
			break
		}
		precedence := fastc_overloaded_binary_precedence(op)
		if precedence < minimum_precedence { break }
		right, end := fastc_fixed_length_expression(tokens, index + 1, precedence + 1, module_name, imports, declared_types)!
		left = '(${left} ${op.str()} ${right})'
		index = end
	}
	return left, index
}

fn fastc_fixed_length_atom(tokens []FastcExpressionToken, start int, module_name string, imports map[string]string, declared_types map[u32]bool) !(string, int) {
	if start >= tokens.len {
		return error('fastc parser does not support empty fixed array dimension')
	}
	item := tokens[start]
	if item.tok in [.plus, .minus, .bit_not] {
		value, end := fastc_fixed_length_atom(tokens, start + 1, module_name, imports, declared_types)!
		return '(${item.tok.str()}${value})', end
	}
	if item.tok == .lpar {
		value, end := fastc_fixed_length_expression(tokens, start + 1, 1, module_name, imports, declared_types)!
		if end >= tokens.len || tokens[end].tok != .rpar {
			return error('fastc parser does not support unbalanced fixed array dimension')
		}
		return '(${value})', end + 1
	}
	if item.tok == .number && fastc_is_integer_literal_expression_type(fastc_number_expression_type(item.lit)) {
		return item.lit.replace('_', ''), start + 1
	}
	if item.tok != .name {
		return error('fastc parser does not support fixed array dimension token `${item.tok.str()}`')
	}
	if start + 1 < tokens.len && tokens[start + 1].tok == .lpar {
		if item.lit !in ['int', 'u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i32', 'i64', 'uint', 'usize',
			'isize'] {
			return error('fastc parser does not support function call in fixed array dimension')
		}
		value, end := fastc_fixed_length_expression(tokens, start + 2, 1, module_name, imports, declared_types)!
		if end >= tokens.len || tokens[end].tok != .rpar {
			return error('fastc parser does not support unbalanced fixed array dimension cast')
		}
		// Fixed dimensions use V's mathematical constant evaluation, which does
		// not truncate primitive casts as runtime arithmetic does.
		return '(${value})', end + 1
	}
	if start + 2 < tokens.len && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name {
		member := tokens[start + 2].lit
		if item.lit == 'C' { return member, start + 3 }
		if owner := imports[item.lit] {
			if start + 4 < tokens.len && tokens[start + 3].tok == .dot && tokens[start + 4].tok == .name {
				key := fastc_type_key(owner, member)
				if fastc_source_type_key_exists(declared_types, key) {
					return '${fastc_c_declared_type_name(key)}__${tokens[start + 4].lit}', start + 5
				}
				return error('fastc parser does not support qualified fixed array dimension')
			}
			return fastc_c_constant_name(owner, member), start + 3
		}
		key := fastc_type_key(module_name, item.lit)
		if fastc_source_type_key_exists(declared_types, key) {
			return '${fastc_c_declared_type_name(key)}__${member}', start + 3
		}
		return error('fastc parser does not support qualified fixed array dimension')
	}
	return fastc_c_constant_name(module_name, item.lit), start + 1
}

fn fastc_fixed_array_element_type(typ_id u32) ?u32 {
	base := fastc_trim_pointer_type(typ_id)
	syntax := flat.source_type_syntax(base)
	if !flat.source_type_query(base).has(.c_fixed_array_prefix) || !syntax.fastc_array_marker() {
		return none
	}
	return fastc_decode_ptr_element_type_id(syntax.fastc_array_marker_payload())
}

fn fastc_decimal_integer_value(source string) ?int {
	value := source.trim(' \t\r\n()').replace('_', '')
	if value == '' {
		return none
	}
	mut start := 0
	if value[0] in [`+`, `-`] {
		start = 1
	}
	if start == value.len {
		return none
	}
	for digit in value[start..] {
		if !digit.is_digit() {
			return none
		}
	}
	return value.int()
}

fn (g &Parser) fixed_array_length_value(source string) ?int {
	mut value := source
	mut seen := map[string]bool{}
	for {
		if result := fastc_decimal_integer_value(value) {
			return result
		}
		key := value.trim(' \t\r\n()')
		if key in seen {
			return none
		}
		seen[key] = true
		value = g.constant_values[key] or { return g.c_integer_expression_value(key, 0) }
	}
	return none
}

// c_integer_expression_value folds a constant's C spelling such as
// `(mod__a+mod__b*2)`: integers, constant names, `+ - * / %` and parentheses.
fn (g &Parser) c_integer_expression_value(source string, depth int) ?int {
	if depth > 16 {
		return none
	}
	mut terms := []string{}
	mut operators := []u8{}
	mut i := 0
	text := source.trim_space()
	mut current := strings.new_builder(16)
	mut paren := 0
	for i < text.len {
		c := text[i]
		if c == `(` {
			paren++
		} else if c == `)` {
			paren--
		}
		if paren == 0 && c in [`+`, `-`] && current.len > 0 {
			terms << current.str().trim_space()
			operators << c
		} else if c != ` ` || paren > 0 {
			current.write_u8(c)
		}
		i++
	}
	terms << current.str().trim_space()
	mut total := 0
	for k, term in terms {
		value := g.c_integer_term_value(term, depth)?
		total = if k == 0 || operators[k - 1] == `+` { total + value } else { total - value }
	}
	return total
}

fn (g &Parser) c_integer_term_value(term string, depth int) ?int {
	mut factors := []string{}
	mut operators := []u8{}
	mut current := strings.new_builder(16)
	mut paren := 0
	for c in term {
		if c == `(` {
			paren++
		} else if c == `)` {
			paren--
		}
		if paren == 0 && c in [`*`, `/`, `%`] {
			factors << current.str().trim_space()
			operators << c
		} else {
			current.write_u8(c)
		}
	}
	factors << current.str().trim_space()
	mut product := 0
	for k, factor in factors {
		mut value := 0
		if factor.starts_with('(') && factor.ends_with(')') {
			value = g.c_integer_expression_value(factor[1..factor.len - 1], depth + 1)?
		} else if decimal := fastc_decimal_integer_value(factor) {
			value = decimal
		} else {
			constant := g.constant_values[factor] or { return none }
			value = if decimal := fastc_decimal_integer_value(constant) {
				decimal
			} else {
				g.c_integer_expression_value(constant, depth + 1)?
			}
		}
		if k == 0 {
			product = value
		} else if operators[k - 1] == `*` {
			product *= value
		} else if value == 0 {
			return none
		} else if operators[k - 1] == `/` {
			product /= value
		} else {
			product %= value
		}
	}
	return product
}

// fastc_composite_part_id separates a container's own name from an encoded pointer suffix.
fn fastc_composite_part_id(typ u32) u32 {
	query := flat.source_type_query(typ)
	if !query.has(.c_pointer_suffix) && (query.has(.c_array_prefix) || query.has(.c_map_prefix))
		&& flat.source_type_syntax(typ).c_ptr_name_suffix() {
		return flat.source_type_id('__vf_collection_' + flat.source_type_name(typ) + '_end')
	}
	return flat.source_type_composition_one(.fastc_composite_part, typ)
}

fn fastc_array_c_type(element_type string) string {
	return flat.source_type_name(fastc_array_c_type_id(flat.source_type_id(element_type)))
}

fn fastc_array_c_type_id(element_type u32) u32 {
	return flat.source_type_composition_two(.concat, flat.source_type_id('Array_'), fastc_composite_part_id(element_type))
}

fn fastc_map_c_type(key_type string, value_type string) string {
	return flat.source_type_name(fastc_map_c_type_id(flat.source_type_id(key_type), flat.source_type_id(value_type)))
}

fn fastc_map_c_type_id(key_type u32, value_type u32) u32 {
	return flat.source_type_composition(.concat, [flat.source_type_id('Map_'),
		fastc_composite_part_id(key_type), flat.source_type_id('_'),
		fastc_composite_part_id(value_type)])
}

fn fastc_map_key_value_types(typ_id u32) ?(u32, u32) {
	base := fastc_trim_pointer_type(typ_id)
	if !flat.source_type_query(base).has(.c_map_prefix) { return none }
	syntax := flat.source_type_syntax(flat.source_type_syntax(base).c_map_payload())
	// Retain the scalar-key priority and complete encoded composite value.
	for key in ['string', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
		'f32', 'f64', 'char', 'uint', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr', 'bool'] {
		key_id := flat.source_type_id(key)
		for index, prefix in syntax.underscore_prefixes() {
			if prefix == key_id {
				return key_id, fastc_decode_map_value_type_id(syntax.underscore_suffixes()[index])
			}
		}
	}
	return none
}

fn fastc_declared_map_key_value_types(typ_id u32, declared_kinds map[u32]FastcDeclaredTypeKind) ?(u32, u32) {
	base := fastc_trim_pointer_type(typ_id)
	if !flat.source_type_query(base).has(.c_map_prefix) { return none }
	syntax := flat.source_type_syntax(flat.source_type_syntax(base).c_map_payload())
	mut matched_key := u32(0)
	mut matched_index := -1
	for type_key, kind in declared_kinds {
		if kind !in [.enum_, .alias_] { continue }
		c_type := fastc_c_declared_type_name(flat.source_type_name(type_key))
		prefix := flat.source_type_composition_one(.fastc_composite_part, flat.source_type_id(c_type))
		// Later underscore boundaries are longer prefixes, preserving the longest match.
		for index, candidate in syntax.underscore_prefixes() {
			if candidate == prefix && index > matched_index {
				matched_key = flat.source_type_id(c_type)
				matched_index = index
			}
		}
	}
	if matched_key == 0 { return none }
	return matched_key, fastc_decode_map_value_type_id(syntax.underscore_suffixes()[matched_index])
}

fn fastc_decode_map_value_type(encoded string) string {
	return flat.source_type_name(fastc_decode_map_value_type_id(flat.source_type_id(encoded)))
}

fn fastc_decode_map_value_type_id(encoded u32) u32 {
	if flat.source_type_name(encoded).starts_with('__vf_collection_') {
		return fastc_decode_ptr_element_type_id(encoded)
	}
	syntax := flat.source_type_syntax(encoded)
	query := flat.source_type_query(encoded)
	// A nested composite owns the `_ptr` suffix of its encoded element.
	if syntax.c_ptr_name_suffix() && !query.has(.c_array_prefix) && !query.has(.c_map_prefix) && !query.has(.c_fixed_array_prefix) {
		return flat.source_type_composition_two(.concat, syntax.c_ptr_name_base(), flat.source_type_id('*'))
	}
	return encoded
}

fn (g &Parser) map_key_value_types(typ u32) ?(u32, u32) {
	if key_type, value_type := fastc_map_key_value_types(typ) {
		return key_type, value_type
	}
	if key_type, value_type := fastc_declared_map_key_value_types(typ, g.declared_kinds) {
		return key_type, value_type
	}
	// An alias of a map type (`type Counts = map[string]int`) has the aliased map's key and value.
	aliased := g.map_type_through_alias(typ)
	if aliased != typ {
		return g.map_key_value_types(aliased)
	}
	return none
}

// map_type_of returns the map type of `typ`, seen through an alias, or none when it is
// not a map.
fn (g &Parser) map_type_of(typ u32) ?u32 {
	map_type := g.map_type_through_alias(typ)
	if flat.source_type_query(map_type).has(.c_map_prefix) {
		return map_type
	}
	return none
}

// map_type_through_alias returns the map type that an alias of a map stands for
// (`main__Counts` -> `Map_string_int`), keeping a pointer suffix. Any other type is
// returned unchanged. The alias is a C typedef of the map, so the two are interchangeable.
fn (g &Parser) map_type_through_alias(typ u32) u32 {
	if typ == 0 || flat.source_type_query(typ).has(.c_map_prefix) {
		return typ
	}
	aliased := g.underlying_alias_type(typ)
	if aliased != typ && flat.source_type_query(aliased).has(.c_map_prefix) {
		return aliased
	}
	return typ
}

fn (g &Parser) is_map_type(typ u32) bool {
	base := fastc_trim_pointer_type(g.underlying_alias_type(fastc_normalize_inferred_type(typ)))
	return base == fastc_type_map || g.map_key_value_types(base) != none
}

fn fastc_register_composite_type(typ u32, mut composite_types map[string]bool) {
	base := fastc_trim_pointer_type(typ)
	query := flat.source_type_query(base)
	if query.has(.c_fixed_array_prefix) {
		if element := fastc_array_element_type(base) {
			fastc_register_composite_type(element, mut composite_types)
		}
	}
	if query.has(.c_array_prefix) || query.has(.c_map_prefix) || fastc_typed_option_payload(base) != none || fastc_callable_signature(base) != none {
		composite_types[flat.source_type_name(base)] = true
		if element := fastc_array_element_type(base) {
			fastc_register_composite_type(element, mut composite_types)
		}
		if key_type, value_type := fastc_map_key_value_types(base) {
			fastc_register_composite_type(key_type, mut composite_types)
			fastc_register_composite_type(value_type, mut composite_types)
		}
		if payload := fastc_typed_option_payload(base) {
			fastc_register_composite_type(payload, mut composite_types)
		}
		if signature := fastc_callable_signature(base) {
			fastc_register_composite_type(signature.return_type, mut composite_types)
			fastc_register_composite_type(signature.option_type, mut composite_types)
			for parameter in signature.parameter_types.ids {
				fastc_register_composite_type(parameter, mut composite_types)
			}
		}
	}
}

fn fastc_collect_generated_template_references(source string, path string, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]bool) {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	for tok != .eof {
		if (tok == .name || tok.is_keyword()) && scan.lit in available_names {
			references[scan.lit] = true
		}
		tok = scan.scan()
	}
}

fn fastc_collect_active_body_references(source string, source_file FastcSourceFile, function_name string, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]bool) {
	file := token.File.unindexed(source_file.path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), source_file.path, prefs) or {
					tok = scan.scan()
					continue
				}
				if selected.source != '' {
					fastc_collect_active_body_references(selected.source, source_file, function_name, prefs, available_names, mut references)
				}
				tok = selected.tok
				continue
			}
			fastc_collect_veb_template_references(source_file, function_name, scan, prefs, available_names, mut references)
		}
		if (tok == .name || tok.is_keyword()) && scan.lit in available_names {
			references[scan.lit] = true
		}
		tok = scan.scan()
	}
}

fn fastc_referenced_veb_template_path(source_path string, function_name string, explicit_path string) ?string {
	dir := os.dir(os.real_path(source_path))
	mut candidates := if explicit_path == '' {
		[
			os.join_path(dir, 'templates', '${function_name}.html'),
			os.join_path_single(dir, '${function_name}.html'),
		]
	} else {
		[
			os.join_path_single(dir, explicit_path),
			explicit_path,
		]
	}
	vmod_root := fastc_vmod_root_for_file(source_path)
	if vmod_root != '' && vmod_root != dir {
		candidates << if explicit_path == '' {
			os.join_path(vmod_root, 'templates', '${function_name}.html')
		} else {
			os.join_path_single(vmod_root, explicit_path)
		}
	}
	for candidate in candidates {
		if os.exists(candidate) {
			return candidate
		}
	}
	return none
}

// fastc_collect_veb_template_references adds calls produced by a `$veb.html()`
// expansion to the enclosing function's references. The ordinary source scan cannot
// otherwise see methods used only inside the HTML template.
fn fastc_collect_veb_template_references(source_file FastcSourceFile, function_name string, scan_after_dollar scanner.Scanner, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]bool) {
	mut lookahead := scan_after_dollar
	if lookahead.scan() != .name || lookahead.lit != 'veb' || lookahead.scan() != .dot || lookahead.scan() != .name || lookahead.lit != 'html' || lookahead.scan() != .lpar {
		return
	}
	mut explicit_path := ''
	if lookahead.scan() == .string {
		explicit_path = lookahead.lit.trim('\'"')
	}
	template_path := fastc_referenced_veb_template_path(source_file.path, function_name, explicit_path) or { return }
	generated := fastc_veb_compile_template(template_path, '__vf_reachability_template', 'ctx') or {
		return
	}
	fastc_collect_generated_template_references(generated, template_path, prefs, available_names, mut references)
}

// fastc_collect_file_references scans one source file's function bodies,
// generated veb templates, and top-level initializers for collected function names.
fn fastc_collect_file_references(source_file FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut previous := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if tok in [.key_struct, .key_union, .key_interface, .key_enum] {
			tok = fastc_collect_type_default_references(mut scan, tok, available_names, mut top_level_references)
			previous = .rcbr
			continue
		}
		if tok != .key_fn || previous == .assign {
			if (tok == .name || tok.is_keyword()) && scan.lit in available_names {
				top_level_references[scan.lit] = true
			}
			previous = tok
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok == .lpar {
			tok = fastc_skip_balanced_tokens(mut scan, tok, .lpar, .rpar) or { break }
		}
		if tok != .name && !tok.is_overloadable() && !tok.is_keyword() {
			previous = tok
			tok = scan.scan()
			continue
		}
		mut function_name := if tok == .name || tok.is_keyword() { scan.lit } else { tok.str() }
		tok = scan.scan()
		if tok == .dot {
			tok = scan.scan()
			if tok == .name {
				function_name = scan.lit
				tok = scan.scan()
			}
		}
		for tok !in [.lcbr, .semicolon, .eof] {
			tok = scan.scan()
		}
		if tok != .lcbr {
			previous = tok
			tok = scan.scan()
			continue
		}
		mut function_references := map[string]bool{}
		if function_name in references {
			function_references = references[function_name].clone()
		}
		mut depth := 1
		tok = scan.scan()
		for depth > 0 && tok != .eof {
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr {
				depth--
			} else if tok == .dollar {
				mut lookahead := scan
				if lookahead.scan() == .key_if {
					selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), source_file.path, prefs) or {
						tok = scan.scan()
						continue
					}
					if selected.source != '' {
						fastc_collect_active_body_references(selected.source, source_file, function_name, prefs, available_names, mut function_references)
					}
					tok = selected.tok
					continue
				}
				fastc_collect_veb_template_references(source_file, function_name, scan, prefs, available_names, mut function_references)
			} else if (tok == .name || tok.is_keyword()) && scan.lit in available_names {
				function_references[scan.lit] = true
			}
			tok = scan.scan()
		}
		references[function_name] = function_references.clone()
		previous = .rcbr
	}
}

// return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcFunctionTypeInfo) return_type_text() string {
	return flat.source_type_name(value.return_type)
}

// option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcFunctionTypeInfo) option_value_type_text() string {
	return flat.source_type_name(value.option_value_type)
}

// parameter_types_text resolves indexed types at a text integration boundary.
fn (value &FastcFunctionTypeInfo) parameter_types_text() []string {
	return value.parameter_types.names()
}

fn fastc_scan_parameter_type(mut scan scanner.Scanner, first token.Token, path string, module_name string, imports map[string]string, declared_types map[u32]bool, allow_short_placeholders bool) !(u32, token.Token) {
	if first == .question {
		payload, next := fastc_scan_type(mut scan, scan.scan(), path, module_name, imports, declared_types, allow_short_placeholders)!
		return flat.source_type_id(fastc_typed_option_type(payload)), next
	}
	return fastc_scan_type(mut scan, first, path, module_name, imports, declared_types, allow_short_placeholders)
}
