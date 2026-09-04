module fastc

import os
import v3.pref
import v3.scanner
import v3.token

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

fn fastc_shared_parameter_is_name(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) bool {
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

fn fastc_parameter_is_params_struct(parameter_type string, params_structs map[string]bool) bool {
	return params_structs[parameter_type.trim_right('*')]
}

fn collect_function_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_declaration_is_enabled := true
	mut next_declaration_is_c_extern := false
	mut previous_tok := token.Token.unknown
	mut skip_index := 0
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_declaration_is_enabled = next_declaration_is_enabled && attribute.is_enabled
			next_declaration_is_c_extern = next_declaration_is_c_extern || attribute.is_c_extern
			continue
		}
		if tok == .key_type && brace_depth == 0 {
			mut alias_scan := scan
			if alias_scan.scan() == .name {
				alias_name := alias_scan.lit
				if alias_scan.scan() == .assign && alias_scan.scan() == .key_fn {
					alias_key := fastc_c_declared_type_name(fastc_type_key(header.module_name, alias_name))
					functions[alias_key] = fastc_scan_function_alias_signature(mut alias_scan, path, header, prefs, declared_types)!
				}
			}
		}
		if tok == .key_fn && brace_depth == 0 && previous_tok != .assign {
			is_public := previous_tok == .key_pub
			tok = scan.scan()
			mut parameter_types := []string{}
			mut parameter_mutability := []bool{}
			mut receiver_type := ''
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
				tok = scan.scan()
				if tok == .name && scan.lit != 'C' {
					receiver_key = fastc_type_key(header.module_name, scan.lit)
				} else if tok == .key_none {
					receiver_key = 'none'
				}
				receiver_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
					return error('fastc method receiver: ${err.msg()}')
				}
				if receiver_key == '' {
					receiver_key = fastc_semantic_declared_type_key(receiver_type, declared_type_c_names)
				}
				if receiver_is_mut && !receiver_type.ends_with('*') {
					receiver_type += '*'
				}
				parameter_types << receiver_type
				parameter_mutability << receiver_is_mut
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
			if receiver_type == '' && name == 'C' && tok == .dot {
				is_c_function = true
				tok = scan.scan()
				if tok != .name && !tok.is_keyword() {
					return error('fastc parser does not support C function declaration in ${path}')
				}
				name = scan.lit
				tok = scan.scan()
			} else if receiver_type == '' && tok == .dot {
				type_key := fastc_type_key(header.module_name, name)
				if type_key !in declared_types {
					return error('fastc parser does not support static method owner `${name}` in ${path}')
				}
				receiver_type = fastc_c_declared_type_name(type_key)
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
			} else if receiver_type == '' {
				fastc_function_key(header.module_name, name)
			} else {
				'${receiver_key}.${name}'
			}
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
						is_variadic = true
						tok = scan.scan()
						break
					}
				}
				mut parameter_is_mut := false
				if tok == .key_mut || (tok == .key_shared && !fastc_shared_parameter_is_name(scan, path, header.module_name, header.imports, declared_types, prefs.building_v)) {
					parameter_is_mut = true
					tok = scan.scan()
				}
				if is_c_function && tok != .name {
					parameter_type, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
						return error('fastc C function `${name}` parameter: ${err.msg()}')
					}
					parameter_types << parameter_type
					parameter_mutability << false
					tok = next_token
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if tok !in [.name, .key_shared] {
					return error('fastc parser does not support function `${name}` parameter token `${tok.str()}` in ${path}')
				}
				parameter_name_or_type := scan.lit
				tok = scan.scan()
				mut parameter_name_count := 1
				if is_c_function && tok == .dot {
					tok = scan.scan()
					if parameter_name_or_type != 'C' || tok != .name {
						return error('fastc parser does not support qualified C parameter type in ${path}')
					}
					parameter_types << scan.lit
					parameter_mutability << false
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
					parameter_type := if type_key in declared_types {
						fastc_c_declared_type_name(type_key)
					} else if parameter_name_or_type in declared_types {
						fastc_c_declared_type_name(parameter_name_or_type)
					} else {
						fastc_primitive_c_type(parameter_name_or_type) or {
							return error('fastc parser does not support undeclared C parameter type `${parameter_name_or_type}` in ${path}')
						}
					}
					parameter_types << parameter_type
					parameter_mutability << false
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
					parameter_name_count++
					tok = scan.scan()
				}
				if tok == .ellipsis {
					is_variadic = true
				}
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
					return error('fastc function `${name}` parameter: ${err.msg()}')
				}
				stored_parameter_type := if parameter_is_mut && !parameter_type.ends_with('*') {
					parameter_type + '*'
				} else {
					parameter_type
				}
				for _ in 0 .. parameter_name_count {
					parameter_types << stored_parameter_type
					parameter_mutability << parameter_is_mut
				}
				tok = next_token
				if tok == .comma {
					tok = scan.scan()
					continue
				}
				if tok != .rpar {
					return error('fastc parser does not support function parameter separator in ${path}')
				}
			}
			tok = scan.scan()
			mut return_type := 'void'
			mut return_types := []string{}
			mut option_type := ''
			if tok != .lcbr && tok != .semicolon {
				if tok in [.not, .question] {
					tok = scan.scan()
					return_type = 'Option'
					if tok in [.lcbr, .semicolon] {
						option_type = 'void'
					} else if tok == .lpar {
						return_types, tok = fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
						option_type = 'MultiReturn'
					} else {
						option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					}
				} else if tok == .lpar {
					return_types, tok = fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					return_type = 'MultiReturn'
				} else {
					return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v) or {
						return error('fastc function `${name}` return: ${err.msg()}')
					}
				}
			}
			if tok != .lcbr && tok != .semicolon {
				return error('fastc parser does not support function `${name}` body in ${path}')
			}
			fixed_parameter_count := parameter_types.len - if receiver_type == '' { 0 } else { 1 }
			signature := FastcFunctionSignature{
				parameter_types: parameter_types
				parameter_mutability: parameter_mutability
				return_type: return_type
				return_types: return_types
				option_type: option_type
				is_variadic: is_variadic
				last_parameter_is_params: fixed_parameter_count > 0 && fastc_parameter_is_params_struct(parameter_types.last(), params_structs)
				is_public: is_public || is_c_function
				is_disabled: !next_declaration_is_enabled
				is_c_extern: is_c_function && next_declaration_is_c_extern
				module_name: header.module_name
				path: path
			}
			if previous := functions[function_key] {
				if !is_c_function {
					is_c_override := previous.path.ends_with('.c.v') || path.ends_with('.c.v')
					if previous.path == path || !is_c_override || !fastc_string_types_equal(previous.parameter_types, signature.parameter_types) || !fastc_bool_types_equal(previous.parameter_mutability, signature.parameter_mutability) || previous.last_parameter_is_params != signature.last_parameter_is_params || previous.return_type != signature.return_type {
						return error('fastc parser does not support duplicate function `${name}` in ${path}')
					}
					if previous.path.ends_with('.c.v') {
						next_declaration_is_enabled = true
						next_declaration_is_c_extern = false
						continue
					}
				}
			}
			functions[function_key] = signature
			next_declaration_is_enabled = true
			next_declaration_is_c_extern = false
			continue
		}
		if brace_depth == 0 && tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union,
			.key_const, .key_global] {
			next_declaration_is_enabled = true
			next_declaration_is_c_extern = false
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
		fastc_collect_selected_comptime_function_signatures(source, path, header, prefs, declared_types, declared_type_c_names, params_structs, mut functions)!
	}
}

// fastc_scan_function_alias_signature scans the signature after `type Name = fn`
// without consuming the main declaration scanner. Function-pointer aliases are
// callable locals, so their return and option payload types must survive the alias
// cast used to initialize those locals.
fn fastc_scan_function_alias_signature(mut scan scanner.Scanner, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool) !FastcFunctionSignature {
	mut tok := scan.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type in ${path}')
	}
	tok = scan.scan()
	mut parameter_types := []string{}
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
		if tok in [.name, .key_shared] {
			mut lookahead := scan
			next_token := lookahead.scan()
			if next_token in [.name, .amp, .and, .mul, .question, .not, .key_fn, .lsbr] {
				tok = scan.scan()
			}
		}
		parameter_type, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
		parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
			parameter_type + '*'
		} else {
			parameter_type
		}
		parameter_mutability << parameter_is_mut
		tok = next_token
	}
	tok = scan.scan()
	mut return_type := 'void'
	mut return_types := []string{}
	mut option_type := ''
	if tok !in [.semicolon, .eof] {
		if tok in [.not, .question] {
			return_type = 'Option'
			tok = scan.scan()
			if tok in [.semicolon, .eof] {
				option_type = 'void'
			} else if tok == .lpar {
				return_types, tok = fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				option_type = 'MultiReturn'
			} else {
				option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
			}
		} else if tok == .lpar {
			return_types, tok = fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
			return_type = 'MultiReturn'
		} else {
			return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
		}
	}
	return FastcFunctionSignature{
		parameter_types: parameter_types
		parameter_mutability: parameter_mutability
		return_type: return_type
		return_types: return_types
		option_type: option_type
		is_public: true
		module_name: header.module_name
		path: path
	}
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
		'main':                   true
		'run':                    true
		// `select` is a keyword, so a `sql db { select ... }` block does not surface a
		// `.select(` reference the way `insert`/`update`/... do; seed it so a connection
		// type's `select` method survives reachability pruning for the ORM lowering.
		'select':                 true
		// `arr.sort(a < b)` lowers to a `sort_with_compare` call emitted in generated C,
		// so no source `.sort_with_compare(` reference surfaces; seed it (its body pulls in
		// vqsort transitively) so it survives reachability pruning.
		'sort_with_compare':      true
		// Channel send/receive (`ch <- v` / `<-ch`) lower to `builtin__chan_try_push`/
		// `builtin__chan_try_pop` in generated C, so no source `.try_push(`/`.try_pop(`
		// reference surfaces; seed them so the chan stubs survive reachability pruning.
		'try_push':               true
		'try_pop':                true
		'panic_result_not_set':   true
		'array_push':             true
		'push':                   true
		'push_many':              true
		'array_slice':            true
		'slice':                  true
		'at':                     true
		'keys':                   true
		'get':                    true
		'get_check':              true
		'new_map':                true
		'set':                    true
		'values':                 true
		'map_hash_string':        true
		'map_eq_string':          true
		'map_clone_string':       true
		'map_free_string':        true
		'map_free_nop':           true
		'map_hash_int_1':         true
		'map_hash_int_2':         true
		'map_hash_int_4':         true
		'map_hash_int_8':         true
		'map_eq_int_1':           true
		'map_eq_int_2':           true
		'map_eq_int_4':           true
		'map_eq_int_8':           true
		'map_clone_int_1':        true
		'map_clone_int_2':        true
		'map_clone_int_4':        true
		'map_clone_int_8':        true
		'new_array_from_c_array': true
		'string_plus_many':       true
		'v_fixed_index':          true
		// `m1 == m2` lowers to a `builtin__map_map_eq` call in generated C, so no source
		// `.map_map_eq(` reference surfaces; seed it so it survives reachability pruning.
		'map_map_eq':             true
	}
	for name in top_level_references.keys() {
		used[name] = true
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

fn collect_interface_method_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, skips []int, declared_types map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut interface_field_paths map[string]string, mut embed_embedders []string, mut embed_embeddeds []string) ! {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	mut depth := 0
	mut skip_index := 0
	mut next_declaration_is_enabled := true
	for tok != .eof {
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
							embed_embedders << interface_key
							embed_embeddeds << embedded_key
						}
					}
					if tok == .semicolon {
						tok = scan.scan()
					}
					continue
				}
				field_type, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				for field_name in member_names {
					field_key := '${interface_key}.${field_name}'
					if field_key in interface_fields {
						return error('fastc parser does not support duplicate interface field `${field_name}` in ${path}')
					}
					interface_fields[field_key] = FastcInterfaceField{
						name: field_name
						typ: field_type
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
			mut parameter_types := [interface_type]
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
				if tok in [.name, .key_shared] {
					mut lookahead := scan
					after := lookahead.scan()
					if after !in [.comma, .rpar, .dot] {
						tok = scan.scan()
					}
				}
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
					parameter_type + '*'
				} else {
					parameter_type
				}
				parameter_mutability << parameter_is_mut
				tok = next_token
				if tok == .comma {
					tok = scan.scan()
				}
			}
			tok = scan.scan()
			mut return_type := 'void'
			mut return_types := []string{}
			mut option_type := ''
			if tok !in [.semicolon, .rcbr] {
				if tok in [.not, .question] {
					tok = scan.scan()
					return_type = 'Option'
					if tok in [.semicolon, .rcbr] {
						option_type = 'void'
					} else {
						option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					}
				} else if tok == .lpar {
					return_types, tok = fastc_scan_multi_return_types(mut scan, path, header.module_name, header.imports, declared_types, prefs.building_v)!
					return_type = 'MultiReturn'
				} else {
					return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name, header.imports, declared_types, prefs.building_v)!
				}
			}
			interface_method_key := '${interface_key}.${method_name}'
			functions[interface_method_key] = FastcFunctionSignature{
				parameter_types: parameter_types
				parameter_mutability: parameter_mutability
				return_type: return_type
				return_types: return_types
				option_type: option_type
				is_public: true
				module_name: header.module_name
				path: path
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
	embed_embedders       []string
	embed_embeddeds       []string
	failed                bool
	error_message         string
}

fn fastc_collect_signature_chunk(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, start int, end int) FastcSignaturePartial {
	mut partial := FastcSignaturePartial{
		functions: map[string]FastcFunctionSignature{}
		interface_methods: map[string]bool{}
		interface_fields: map[string]FastcInterfaceField{}
		interface_field_paths: map[string]string{}
		embed_embedders: []string{}
		embed_embeddeds: []string{}
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

fn fastc_merge_signature_partial(partial FastcSignaturePartial, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []string, mut embed_embeddeds []string) ! {
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
				if previous.path == signature.path || !is_c_override || !fastc_string_types_equal(previous.parameter_types, signature.parameter_types) || !fastc_bool_types_equal(previous.parameter_mutability, signature.parameter_mutability) || previous.last_parameter_is_params != signature.last_parameter_is_params || previous.return_type != signature.return_type {
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

fn fastc_scan_multi_return_types(mut scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) !([]string, token.Token) {
	mut types := []string{}
	mut tok := scan.scan()
	for tok != .rpar {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		component_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
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
fn fastc_peek_chan_element(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) string {
	mut probe := scan
	elem_tok := probe.scan()
	if elem_tok in [token.Token.comma, .rpar, .lcbr, .semicolon, .assign, .rcbr, .attribute, .eof] {
		return ''
	}
	elem_c, _ := fastc_scan_type(mut probe, elem_tok, path, module_name, imports, declared_types, allow_short_placeholders) or { return '' }
	return elem_c
}

// fastc_peek_option_element recovers the wrapped C value type of an option field (`f ?T`),
// scanning a copy positioned just after the leading `?`. Returns '' for a bare `?` (no
// following type) or an unresolvable type.
fn fastc_peek_option_element(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) string {
	mut probe := scan
	elem_tok := probe.scan()
	if elem_tok in [token.Token.comma, .rpar, .lcbr, .semicolon, .assign, .rcbr, .attribute, .eof] {
		return ''
	}
	elem_c, _ := fastc_scan_type(mut probe, elem_tok, path, module_name, imports, declared_types, allow_short_placeholders) or { return '' }
	return elem_c
}

// fastc_peek_generic_type_argument recovers the first concrete argument from a
// generic field type such as `Stack[Item]`. The scanner copy starts immediately
// after `first`, so this does not advance declaration scanning.
fn fastc_peek_generic_type_argument(first token.Token, scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) string {
	if first != .name {
		return ''
	}
	mut probe := scan
	mut tok := probe.scan()
	if tok == .dot {
		if probe.scan() != .name {
			return ''
		}
		tok = probe.scan()
	}
	if tok != .lsbr {
		return ''
	}
	arg_tok := probe.scan()
	if arg_tok == .rsbr {
		return ''
	}
	argument_type, next_token := fastc_scan_type(mut probe, arg_tok, path, module_name, imports, declared_types, allow_short_placeholders) or { return '' }
	if next_token !in [.comma, .rsbr] {
		return ''
	}
	return argument_type
}

struct FastcFunctionTypeInfo {
	parameter_types   []string
	return_type       string
	option_value_type string
}

// fastc_peek_function_type preserves the signature that fastc_scan_type erases to
// `voidptr`. The scanner copy is positioned immediately after the leading `fn`.
fn fastc_peek_function_type(scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) !FastcFunctionTypeInfo {
	mut look := scan
	mut tok := look.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type in ${path}')
	}
	tok = look.scan()
	mut parameter_types := []string{}
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
		mut has_parameter_name := false
		if tok == .name {
			mut probe := look
			next_token := probe.scan()
			has_parameter_name = next_token in [.name, .amp, .and, .mul, .question, .not, .key_fn,
				.lsbr]
		}
		if has_parameter_name {
			tok = look.scan()
		}
		parameter_type, next_token := fastc_scan_type(mut look, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
			parameter_type + '*'
		} else {
			parameter_type
		}
		tok = next_token
	}
	tok = look.scan()
	mut return_type := 'void'
	mut option_value_type := ''
	if tok in [.not, .question] {
		return_type = 'Option'
		value_tok := look.scan()
		if value_tok !in [.semicolon, .comma, .rpar, .lcbr, .assign, .attribute, .rcbr, .eof] {
			option_value_type, _ = fastc_scan_type(mut look, value_tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
	} else if tok !in [.semicolon, .comma, .rpar, .lcbr, .assign, .attribute, .rcbr, .eof] {
		return_type, _ = fastc_scan_type(mut look, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
	}
	return FastcFunctionTypeInfo{
		parameter_types: parameter_types
		return_type: return_type
		option_value_type: option_value_type
	}
}

fn fastc_scan_type(mut scan scanner.Scanner, first token.Token, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) !(string, token.Token) {
	mut tok := first
	mut optional := false
	if tok in [.question, .not] {
		optional = true
		tok = scan.scan()
	}
	mut pointers := 0
	for tok == .amp || tok == .and || tok == .mul {
		pointers += if tok == .and { 2 } else { 1 }
		tok = scan.scan()
	}
	if optional && tok == .lcbr {
		return 'Option' + '*'.repeat(pointers), tok
	}
	if tok == .ellipsis {
		tok = scan.scan()
		element_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
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
		} else if tok in [.name, .number] {
			fixed_length = scan.lit
			if tok == .name {
				length_owner := fixed_length
				fixed_length = fastc_c_constant_name(module_name, fixed_length)
				tok = scan.scan()
				if tok == .dot {
					tok = scan.scan()
					if tok == .name {
						if imported_module := imports[length_owner] {
							fixed_length = fastc_c_constant_name(imported_module, scan.lit)
						} else {
							fixed_length = ''
						}
						tok = scan.scan()
					} else {
						fixed_length = ''
					}
				}
			} else {
				tok = scan.scan()
			}
			if tok != .rsbr {
				fixed_length = ''
				mut depth := 1
				for depth > 0 && tok != .eof {
					if tok == .lsbr {
						depth++
					} else if tok == .rsbr {
						depth--
					}
					tok = scan.scan()
				}
			} else {
				tok = scan.scan()
			}
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
		element_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		tok = next_token
		array_type := if optional {
			'Option'
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
			_, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
		return 'voidptr' + '*'.repeat(pointers), tok
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
			_, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
		channel_type := if optional { 'Option' } else { 'chan' + '*'.repeat(pointers) }
		return channel_type, tok
	}
	if raw_type == 'thread' {
		// `thread`, `thread T`, `thread !`, `thread ?`: a spawned-thread handle. Its
		// C name must match what `spawn` derives from the callee's return type: void
		// -> '' , a result/option -> 'Option', otherwise the concrete value type.
		mut value_type := ''
		if tok in [.not, .question] {
			value_type = 'Option'
			tok = scan.scan()
		} else if tok !in [.comma, .rpar, .lcbr, .semicolon, .assign] {
			value_type, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		}
		thread_type := if optional {
			'Option'
		} else {
			fastc_thread_type_name(value_type) + '*'.repeat(pointers)
		}
		return thread_type, tok
	}
	if raw_type == 'map' && tok == .lsbr {
		tok = scan.scan()
		key_type, next_key_token := fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		tok = next_key_token
		if tok != .rsbr {
			return error('fastc parser does not support unfinished map key type in ${path}')
		}
		tok = scan.scan()
		value_type, next_value_token := fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types, allow_short_placeholders)!
		tok = next_value_token
		base := if optional { 'Option' } else { fastc_map_c_type(key_type, value_type) }
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
	if is_imported_type && type_module != 'builtin' && type_key in declared_types && !declared_types[type_key] {
		return error('fastc parser does not support private type `${raw_type}` from imported module `${type_module}` in ${path}')
	}
	mut base := ''
	if type_module == 'C' {
		base = if '#Cstruct#${raw_type}' in declared_types {
			'struct ${raw_type}'
		} else {
			raw_type
		}
	} else if type_key in declared_types {
		base = fastc_c_declared_type_name(type_key)
	} else if raw_type in declared_types {
		// Builtin declarations use their unqualified spelling as the canonical key.
		base = fastc_c_declared_type_name(raw_type)
	} else if raw_type.contains('__') && raw_type.replace('__', '.') in declared_types {
		// Cross-module monomorphization substitutes an already-resolved C spelling
		// (`config__Config`) into the defining module's generic source.
		base = raw_type
	} else if raw_type.starts_with('Array_') || raw_type.starts_with('Map_') || raw_type.starts_with('FixedArray_') {
		// On-demand monomorphization likewise substitutes FastC's already-resolved
		// composite spelling (`Array_string`, `Map_string_int`, ...). Keep it as the
		// concrete C type when the generated instance is scanned again.
		base = raw_type
	} else {
		base = fastc_primitive_c_type(raw_type) or { '' }
	}
	if base == '' {
		if type_key !in declared_types {
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
		return 'Option', tok
	}
	return base + '*'.repeat(pointers), tok
}

fn fastc_composite_type_part(typ string) string {
	return typ.replace(' ', '_').replace('*', '_ptr').replace('.', '__')
}

fn fastc_fixed_array_type(length string, element_type string) string {
	return 'FixedArray_${length}_FASTC_ARRAY_OF_${element_type}'
}

fn fastc_fixed_array_length(typ string) ?string {
	if !typ.starts_with('FixedArray_') || !typ.contains('_FASTC_ARRAY_OF_') {
		return none
	}
	details := typ['FixedArray_'.len..]
	return details.all_before('_FASTC_ARRAY_OF_')
}

fn fastc_fixed_array_element_type(typ string) ?string {
	if !typ.starts_with('FixedArray_') || !typ.contains('_FASTC_ARRAY_OF_') {
		return none
	}
	return typ.all_after('_FASTC_ARRAY_OF_')
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
		value = g.constant_values[key] or { return none }
	}
	return none
}

fn fastc_array_c_type(element_type string) string {
	return 'Array_${fastc_composite_type_part(element_type)}'
}

fn fastc_map_c_type(key_type string, value_type string) string {
	return 'Map_${fastc_composite_type_part(key_type)}_${fastc_composite_type_part(value_type)}'
}

fn fastc_map_key_value_types(typ string) ?(string, string) {
	base := typ.trim_right('*')
	if !base.starts_with('Map_') {
		return none
	}
	payload := base['Map_'.len..]
	// Composite C type names can contain underscores, so splitting every
	// underscore loses the value type. Map keys are restricted to scalar V
	// types; use that boundary and retain the complete encoded value type.
	for key_type in ['string', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
		'byte', 'char', 'uint', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr', 'bool'] {
		prefix := '${fastc_composite_type_part(key_type)}_'
		if payload.starts_with(prefix) {
			return key_type, fastc_decode_map_value_type(payload[prefix.len..])
		}
	}
	return none
}

fn fastc_declared_map_key_value_types(typ string, declared_kinds map[string]FastcDeclaredTypeKind) ?(string, string) {
	base := typ.trim_right('*')
	if !base.starts_with('Map_') {
		return none
	}
	payload := base['Map_'.len..]
	mut matched_key := ''
	mut matched_prefix := ''
	for type_key, kind in declared_kinds {
		// Map keys may be enums or aliases of scalar key types. Choose the longest
		// matching C spelling because qualified type names can share prefixes.
		if kind !in [.enum_, .alias_] {
			continue
		}
		c_type := fastc_c_declared_type_name(type_key)
		prefix := '${fastc_composite_type_part(c_type)}_'
		if payload.starts_with(prefix) && prefix.len > matched_prefix.len {
			matched_key = c_type
			matched_prefix = prefix
		}
	}
	if matched_key == '' {
		return none
	}
	return matched_key, fastc_decode_map_value_type(payload[matched_prefix.len..])
}

fn fastc_decode_map_value_type(encoded string) string {
	// A trailing `_ptr` on a composite value can belong to its nested element
	// type (`map[string][]&T` -> `Map_string_Array_T_ptr`), not to the map
	// value itself. Preserve composite names so their own decoder handles it.
	if encoded.ends_with('_ptr') && !encoded.starts_with('Array_') && !encoded.starts_with('Map_') && !encoded.starts_with('FixedArray_') {
		return encoded[..encoded.len - '_ptr'.len] + '*'
	}
	return encoded
}

fn (g &Parser) map_key_value_types(typ string) ?(string, string) {
	if key_type, value_type := fastc_map_key_value_types(typ) {
		return key_type, value_type
	}
	return fastc_declared_map_key_value_types(typ, g.declared_kinds)
}

fn fastc_register_composite_type(typ string, mut composite_types map[string]bool) {
	base := typ.trim_right('*')
	if base.starts_with('Array_') || base.starts_with('Map_') {
		composite_types[base] = true
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
	generated := fastc_veb_compile_template(template_path, '__v_fastc_reachability_template', 'ctx') or {
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
