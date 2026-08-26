module fastc

import v3.pref
import v3.scanner
import v3.token

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

fn fastc_scan_struct_field_attribute(mut scan scanner.Scanner) !(token.Token, bool) {
	mut tok := scan.scan()
	mut depth := 1
	mut is_required := false
	for depth > 0 {
		if tok == .eof {
			return error('fastc parser does not support unfinished struct field attribute')
		}
		if tok == .name && scan.lit == 'required' {
			is_required = true
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return tok, is_required
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

fn collect_function_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_declaration_is_enabled := true
	mut previous_tok := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_declaration_is_enabled = next_declaration_is_enabled && attribute.is_enabled
			continue
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
				receiver_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
					header.imports, declared_types, prefs.building_v) or {
					return error('fastc method receiver: ${err.msg()}')
				}
				if receiver_key == '' {
					receiver_key = fastc_semantic_declared_type_key(receiver_type, declared_types)
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
			if tok != .name && !(receiver_type != '' && (tok.is_overloadable() || tok.is_keyword())) {
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
				if tok != .name {
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
				mut parameter_is_mut := false
				if tok in [.key_mut, .key_shared] {
					parameter_is_mut = true
					tok = scan.scan()
				}
				if is_c_function && tok != .name {
					parameter_type, next_token := fastc_scan_type(mut scan, tok, path,
						header.module_name, header.imports, declared_types, prefs.building_v) or {
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
				if tok != .name {
					return error('fastc parser does not support function `${name}` parameter token `${tok.str()}` in ${path}')
				}
				parameter_name_or_type := scan.lit
				tok = scan.scan()
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
					parameter_type := fastc_primitive_c_type(parameter_name_or_type) or {
						return error('fastc parser does not support undeclared C parameter type `${parameter_name_or_type}` in ${path}')
					}
					parameter_types << parameter_type
					parameter_mutability << false
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if tok == .comma {
					return error('fastc parser does not support grouped parameter names in ${path}')
				}
				if tok == .ellipsis {
					is_variadic = true
				}
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path,
					header.module_name, header.imports, declared_types, prefs.building_v) or {
					return error('fastc function `${name}` parameter: ${err.msg()}')
				}
				parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
					parameter_type + '*'
				} else {
					parameter_type
				}
				parameter_mutability << parameter_is_mut
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
						return_types, tok = fastc_scan_multi_return_types(mut scan, path,
							header.module_name, header.imports, declared_types, prefs.building_v)!
						option_type = 'MultiReturn'
					} else {
						option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
							header.imports, declared_types, prefs.building_v)!
					}
				} else if tok == .lpar {
					return_types, tok = fastc_scan_multi_return_types(mut scan, path,
						header.module_name, header.imports, declared_types, prefs.building_v)!
					return_type = 'MultiReturn'
				} else {
					return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
						header.imports, declared_types, prefs.building_v) or {
						return error('fastc function `${name}` return: ${err.msg()}')
					}
				}
			}
			if tok != .lcbr && tok != .semicolon {
				return error('fastc parser does not support function `${name}` body in ${path}')
			}
			fixed_parameter_count := parameter_types.len - if receiver_type == '' { 0 } else { 1 }
			signature := FastcFunctionSignature{
				parameter_types:          parameter_types
				parameter_mutability:     parameter_mutability
				return_type:              return_type
				return_types:             return_types
				option_type:              option_type
				is_variadic:              is_variadic
				last_parameter_is_params: fixed_parameter_count > 0
					&& fastc_parameter_is_params_struct(parameter_types.last(), params_structs)
				is_public:                is_public || is_c_function
				is_disabled:              !next_declaration_is_enabled
				module_name:              header.module_name
				path:                     path
			}
			if previous := functions[function_key] {
				if !is_c_function {
					is_c_override := previous.path.ends_with('.c.v') || path.ends_with('.c.v')
					if previous.path == path || !is_c_override
						|| !fastc_string_types_equal(previous.parameter_types, signature.parameter_types)
						|| !fastc_bool_types_equal(previous.parameter_mutability, signature.parameter_mutability)
						|| previous.last_parameter_is_params != signature.last_parameter_is_params
						|| previous.return_type != signature.return_type {
						return error('fastc parser does not support duplicate function `${name}` in ${path}')
					}
					if previous.path.ends_with('.c.v') {
						next_declaration_is_enabled = true
						continue
					}
				}
			}
			functions[function_key] = signature
			next_declaration_is_enabled = true
			continue
		}
		if brace_depth == 0
			&& tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union, .key_const, .key_global] {
			next_declaration_is_enabled = true
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		previous_tok = tok
		tok = scan.scan()
	}
	fastc_collect_selected_comptime_function_signatures(source, path, header, prefs,
		declared_types, params_structs, mut functions)!
}

fn fastc_collect_referenced_function_names(sources []FastcSourceFile, prefs &pref.Preferences, functions map[string]FastcFunctionSignature) map[string]bool {
	mut available_names := map[string]bool{}
	for key in functions.keys() {
		available_names[key.all_after_last('.')] = true
	}
	mut references := map[string]map[string]bool{}
	mut top_level_references := map[string]bool{}
	for source_file in sources {
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines_without_digest(source_file.source)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source_file.source)
		mut previous := token.Token.unknown
		mut tok := scan.scan()
		for tok != .eof {
			if tok in [.key_struct, .key_union, .key_interface, .key_enum] {
				tok = fastc_collect_type_default_references(mut scan, tok, available_names, mut
					top_level_references)
				previous = .rcbr
				continue
			}
			if tok != .key_fn || previous == .assign {
				if tok == .name && scan.lit in available_names {
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
				} else if tok == .name && scan.lit in available_names {
					function_references[scan.lit] = true
				}
				tok = scan.scan()
			}
			references[function_name] = function_references.clone()
			previous = .rcbr
		}
	}
	mut used := {
		'main':                   true
		'run':                    true
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
	}
	for name in top_level_references.keys() {
		used[name] = true
	}
	mut changed := true
	for changed {
		changed = false
		for name in used.keys() {
			if name !in references {
				continue
			}
			referenced := references[name].clone()
			for referenced_name in referenced.keys() {
				if referenced_name !in used {
					used[referenced_name] = true
					changed = true
				}
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

fn collect_interface_method_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	mut depth := 0
	mut next_declaration_is_enabled := true
	for tok != .eof {
		if depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_interface_method_signatures(selected.source, path, header, prefs,
						declared_types, mut functions, mut interface_methods, mut interface_fields)!
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
			if depth == 0
				&& tok in [.key_fn, .key_struct, .key_enum, .key_type, .key_union, .key_const, .key_global] {
				next_declaration_is_enabled = true
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
			if tok != .name {
				tok = scan.scan()
				continue
			}
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
					if tok == .semicolon {
						tok = scan.scan()
					}
					continue
				}
				field_type, next_token := fastc_scan_type(mut scan, tok, path, header.module_name,
					header.imports, declared_types, prefs.building_v)!
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
				if tok in [.key_mut, .key_shared] {
					parameter_is_mut = true
					tok = scan.scan()
				}
				if tok != .name {
					return error('fastc parser does not support interface method parameter in ${path}')
				}
				tok = scan.scan()
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path,
					header.module_name, header.imports, declared_types, prefs.building_v)!
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
						option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
							header.imports, declared_types, prefs.building_v)!
					}
				} else if tok == .lpar {
					return_types, tok = fastc_scan_multi_return_types(mut scan, path,
						header.module_name, header.imports, declared_types, prefs.building_v)!
					return_type = 'MultiReturn'
				} else {
					return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
						header.imports, declared_types, prefs.building_v)!
				}
			}
			interface_method_key := '${interface_key}.${method_name}'
			functions[interface_method_key] = FastcFunctionSignature{
				parameter_types:      parameter_types
				parameter_mutability: parameter_mutability
				return_type:          return_type
				return_types:         return_types
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

fn fastc_scan_multi_return_types(mut scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool, allow_short_placeholders bool) !([]string, token.Token) {
	mut types := []string{}
	mut tok := scan.scan()
	for tok != .rpar {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		component_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types, allow_short_placeholders)!
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
		element_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types, allow_short_placeholders)!
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
		element_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types, allow_short_placeholders)!
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
		if tok == .name || tok == .amp || tok == .and || tok == .mul || tok == .question
			|| tok == .not || tok == .lsbr || tok == .lpar || tok == .key_fn {
			_, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types,
				allow_short_placeholders)!
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
			_, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types,
				allow_short_placeholders)!
		}
		channel_type := if optional { 'Option' } else { 'chan' + '*'.repeat(pointers) }
		return channel_type, tok
	}
	if raw_type == 'map' && tok == .lsbr {
		tok = scan.scan()
		key_type, next_key_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types, allow_short_placeholders)!
		tok = next_key_token
		if tok != .rsbr {
			return error('fastc parser does not support unfinished map key type in ${path}')
		}
		tok = scan.scan()
		value_type, next_value_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types, allow_short_placeholders)!
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
	if is_imported_type && type_module != 'builtin' && type_key in declared_types
		&& !declared_types[type_key] {
		return error('fastc parser does not support private type `${raw_type}` from imported module `${type_module}` in ${path}')
	}
	mut base := ''
	if type_module == 'C' {
		base = if '#Cstruct#${raw_type}' in declared_types { 'struct ${raw_type}' } else { raw_type }
	} else if type_key in declared_types {
		base = fastc_c_declared_type_name(type_key)
	} else if raw_type in declared_types {
		// Builtin declarations use their unqualified spelling as the canonical key.
		base = fastc_c_declared_type_name(raw_type)
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
			mut value_type := payload[prefix.len..]
			if value_type.ends_with('_ptr') {
				value_type = value_type[..value_type.len - '_ptr'.len] + '*'
			}
			return key_type, value_type
		}
	}
	return none
}

fn fastc_register_composite_type(typ string, mut composite_types map[string]bool) {
	base := typ.trim_right('*')
	if base.starts_with('Array_') || base.starts_with('Map_') {
		composite_types[base] = true
	}
}
