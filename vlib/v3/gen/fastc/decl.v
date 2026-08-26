module fastc

import strings
import v3.pref
import v3.scanner
import v3.token

fn collect_declared_types(source string, path string, module_name string, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_c_struct_is_typedef := false
	mut next_enum_is_flag := false
	mut next_struct_is_params := false
	mut next_type_is_enabled := true
	mut previous_tok := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_declared_types(selected.source, path, module_name, prefs, mut
						declared_types, mut declared_kinds, mut enum_flags, mut params_structs)!
				}
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if brace_depth == 0 && tok == .attribute {
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_c_struct_is_typedef = next_c_struct_is_typedef || attribute.is_typedef
			next_enum_is_flag = next_enum_is_flag || attribute.is_flag
			next_struct_is_params = next_struct_is_params || attribute.is_params
			next_type_is_enabled = next_type_is_enabled && attribute.is_enabled
			continue
		}
		if brace_depth == 0 && tok in [.key_fn, .key_const, .key_global] {
			next_c_struct_is_typedef = false
			next_enum_is_flag = false
			next_struct_is_params = false
			next_type_is_enabled = true
		}
		if brace_depth == 0
			&& tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union] {
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
						declared_types['#Cstruct#${scan.lit}'] = true
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
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		previous_tok = tok
		tok = scan.scan()
	}
}

fn collect_constant_names(source string, path string, module_name string, prefs &pref.Preferences, mut constants map[string]string, mut public_constants map[string]bool) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut previous_tok := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_constant_names(selected.source, path, module_name, prefs, mut
						constants, mut public_constants)!
				}
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if brace_depth == 0 && tok == .key_const {
			is_public := previous_tok == .key_pub
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut at_declaration_start := true
				mut nested_depth := 0
				for tok != .eof {
					if nested_depth == 0 && tok == .rpar {
						tok = scan.scan()
						break
					}
					if nested_depth == 0 && tok == .semicolon {
						at_declaration_start = true
						tok = scan.scan()
						continue
					}
					if nested_depth == 0 && at_declaration_start && tok == .name {
						fastc_register_constant(module_name, scan.lit, is_public, path, mut
							constants, mut public_constants)!
						at_declaration_start = false
					}
					if tok in [.lpar, .lsbr, .lcbr] {
						nested_depth++
					} else if tok in [.rpar, .rsbr, .rcbr] && nested_depth > 0 {
						nested_depth--
					}
					tok = scan.scan()
				}
				continue
			}
			if tok != .name {
				return error('fastc parser does not support constant declaration in ${path}')
			}
			fastc_register_constant(module_name, scan.lit, is_public, path, mut constants, mut
				public_constants)!
			continue
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

fn fastc_register_constant(module_name string, name string, is_public bool, path string, mut constants map[string]string, mut public_constants map[string]bool) ! {
	key := fastc_constant_key(module_name, name)
	if key in constants {
		return error('fastc parser does not support duplicate constant `${name}` in ${path}')
	}
	constants[key] = fastc_c_constant_name(module_name, name)
	if is_public {
		public_constants[key] = true
	}
}

fn collect_global_names(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, mut globals map[string]string, mut public_globals map[string]bool) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut depth := 0
	mut previous_tok := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_global_names(selected.source, path, header, prefs, mut globals, mut
						public_globals)!
				}
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if depth == 0 && tok == .key_global {
			is_public := previous_tok == .key_pub
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut at_start := true
				for tok != .rpar && tok != .eof {
					if tok == .semicolon {
						at_start = true
					} else if at_start && tok == .name {
						if scan.lit != 'C' {
							fastc_register_global(header, scan.lit, is_public, path, prefs, mut
								globals, mut public_globals)!
						}
						at_start = false
					}
					tok = scan.scan()
				}
				continue
			}
			if tok == .name && scan.lit != 'C' {
				fastc_register_global(header, scan.lit, is_public, path, prefs, mut globals, mut
					public_globals)!
			}
			continue
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr && depth > 0 {
			depth--
		}
		previous_tok = tok
		tok = scan.scan()
	}
}

fn fastc_register_global(header FastcSourceHeader, name string, is_public bool, path string, prefs &pref.Preferences, mut globals map[string]string, mut public_globals map[string]bool) ! {
	if !prefs.enable_globals && !prefs.building_v && header.module_name != 'builtin'
		&& !header.has_globals {
		return error('use `v -enable-globals ...` to enable globals in ${path}')
	}
	key := fastc_global_key(header.module_name, name)
	if key in globals {
		return error('fastc parser does not support duplicate global `${name}` in ${path}')
	}
	globals[key] = fastc_c_global_name(key)
	if is_public {
		public_globals[key] = true
	}
}

fn fastc_generate_global_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, constant_values map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, mut global_types map[string]string) !FastcGlobalDeclarations {
	mut out := strings.new_builder(1024)
	mut module_initializers := map[string]string{}
	helper_has_c_functions := fastc_functions_declare_c(functions)
	ordered_sources := fastc_sources_in_dependency_order(sources)!
	for source_file in ordered_sources {
		mut initializers := strings.new_builder(256)
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines_without_digest(source_file.source)
		mut gen := Parser{
			prefs:                  unsafe { prefs }
			path:                   source_file.path
			module_name:            source_file.header.module_name
			imports:                source_file.header.imports
			declared_types:         declared_types
			declared_type_c_names:  declared_type_c_names
			fastc_prefixed_c_names: fastc_prefixed_c_names
			has_c_functions:        helper_has_c_functions
			comparison_memo:        map[i64]FastcRenderedExpression{}
			declared_kinds:         declared_kinds
			enum_flags:             enum_flags
			alias_base_types:       alias_base_types
			struct_fields:          struct_fields
			struct_field_info:      struct_field_info
			constants:              constants
			constant_values:        constant_values
			public_constants:       public_constants
			globals:                globals
			public_globals:         public_globals
			selfhost:               prefs.building_v
			s:                      scanner.new_scanner(prefs, .normal)
			out:                    strings.new_builder(0)
			protos:                 strings.new_builder(0)
			functions:              functions
			constant_types:         constant_types
			global_types:           global_types
		}
		gen.s.init(file, source_file.source)
		gen.next()
		gen.parse_selected_global_declarations(mut out, mut initializers, false)!
		global_types = gen.global_types.clone()
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
		module_initializers: module_initializers
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
	out.writeln('static ${typ} ${c_name};')
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

fn fastc_render_struct_field_defaults(prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, alias_base_types map[string]string, struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, global_types map[string]string) ! {
	helper_has_c_functions := fastc_functions_declare_c(functions)
	mut type_names := struct_field_info.keys()
	type_names.sort()
	for type_name in type_names {
		mut fields := struct_field_info[type_name].clone()
		for mut field in fields {
			if field.default_source == '' {
				continue
			}
			default_path := '${field.path}:${field.name}:default'
			mut file_set := token.FileSet.new()
			mut file := file_set.add_file(default_path, field.default_source.len)
			file.index_lines_without_digest(field.default_source)
			mut gen := Parser{
				prefs:                  unsafe { prefs }
				path:                   default_path
				module_name:            field.module_name
				imports:                field.imports
				declared_types:         declared_types
				declared_type_c_names:  declared_type_c_names
				fastc_prefixed_c_names: fastc_prefixed_c_names
				has_c_functions:        helper_has_c_functions
				comparison_memo:        map[i64]FastcRenderedExpression{}
				declared_kinds:         declared_kinds
				enum_flags:             enum_flags
				alias_base_types:       alias_base_types
				struct_fields:          struct_fields
				struct_field_info:      struct_field_info
				constants:              constants
				public_constants:       public_constants
				globals:                globals
				public_globals:         public_globals
				selfhost:               prefs.building_v
				s:                      scanner.new_scanner(prefs, .normal)
				out:                    strings.new_builder(0)
				protos:                 strings.new_builder(0)
				functions:              functions
				constant_types:         constant_types
				global_types:           global_types
				fixed_array_types:      map[string]string{}
				composite_types:        map[string]bool{}
			}
			gen.s.init(file, field.default_source)
			gen.next()
			gen.expected_expression_type = field.typ
			field.default_value = gen.read_expression([token.Token.semicolon, token.Token.eof])!
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

fn fastc_generate_constant_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, globals map[string]string, public_globals map[string]bool, mut constant_types map[string]string) !FastcConstantDeclarations {
	mut values := []FastcConstantValue{}
	helper_has_c_functions := fastc_functions_declare_c(functions)
	ordered_sources := fastc_sources_in_dependency_order(sources)!
	for source_file in ordered_sources {
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines_without_digest(source_file.source)
		mut gen := Parser{
			prefs:                  unsafe { prefs }
			path:                   source_file.path
			module_name:            source_file.header.module_name
			imports:                source_file.header.imports
			declared_types:         declared_types
			declared_type_c_names:  declared_type_c_names
			fastc_prefixed_c_names: fastc_prefixed_c_names
			has_c_functions:        helper_has_c_functions
			comparison_memo:        map[i64]FastcRenderedExpression{}
			declared_kinds:         declared_kinds
			enum_flags:             enum_flags
			alias_base_types:       alias_base_types
			struct_fields:          struct_fields
			struct_field_info:      struct_field_info
			constants:              constants
			public_constants:       public_constants
			globals:                globals
			public_globals:         public_globals
			selfhost:               prefs.building_v
			s:                      scanner.new_scanner(prefs, .normal)
			out:                    strings.new_builder(256)
			protos:                 strings.new_builder(0)
			functions:              functions
			constant_types:         constant_types
		}
		gen.s.init(file, source_file.source)
		gen.next()
		gen.parse_selected_constant_declarations(mut values, false)!
		if gen.s.diagnostics.len > 0 {
			diagnostic := gen.s.diagnostics[0]
			return error('fastc scanner error at byte ${diagnostic.offset} in ${source_file.path}: ${diagnostic.message}')
		}
		constant_types = gen.constant_types.clone()
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
			fastc_append_runtime_constant(i, values, runtime_constants, mut visiting, mut visited, mut
				initializer_order)!
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
				fastc_append_runtime_constant(dependency_index, values, runtime_constants, mut
					visiting, mut visited, mut ordered)!
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
		key:          key
		c_name:       c_name
		module_name:  g.module_name
		value:        value
		typ:          typ
		dependencies: g.constant_expression_dependencies(g.last_expression)
		is_runtime:   is_runtime
	}
	g.constant_types[key] = typ
}

fn (g &Parser) constant_expression_dependencies(tokens []FastcExpressionToken) []string {
	mut dependencies := []string{}
	for i, item in tokens {
		if item.tok != .name || (i > 0 && tokens[i - 1].tok == .dot) {
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
		if fastc_primitive_c_type(item.lit) != none
			|| fastc_resolve_declared_type_key(g.module_name, item.lit, g.imports, g.declared_types) != none {
			continue
		}
		return true
	}
	return false
}

fn fastc_generate_type_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool) !FastcTypeDeclarations {
	mut out := strings.new_builder(4096)
	mut bodies := strings.new_builder(4096)
	mut enum_infos := []FastcEnumInfo{}
	mut alias_base_types := map[string]string{}
	mut keys := declared_kinds.keys()
	keys.sort()
	for type_id, key in keys {
		name := fastc_c_declared_type_name(key)
		out.writeln('#define __v_typeid_${name} ${type_id + 1}')
		match declared_kinds[key] {
			.struct_, .interface_ { out.writeln('typedef struct ${name} ${name};') }
			.union_ { out.writeln('typedef union ${name} ${name};') }
			.enum_ { out.writeln('typedef ${if enum_flags[key] { 'u64' } else { 'int' }} ${name};') }
			.alias_ {}
		}
	}
	out.writeln('')
	for source_file in sources {
		fastc_emit_source_type_declarations(source_file, prefs, declared_types, declared_kinds,
			constants, public_constants, mut struct_fields, mut struct_field_info, mut
			composite_types, mut alias_base_types, mut enum_infos, mut bodies)!
	}
	mut composite_names := composite_types.keys()
	composite_names.sort()
	for composite_name in composite_names {
		base := if composite_name.starts_with('Array_') { 'array' } else { 'map' }
		out.writeln('typedef ${base} ${composite_name};')
	}
	out.writeln('')
	mut type_bodies := bodies.str()
	if prefs.building_v {
		type_bodies = fastc_order_c_composite_definitions(type_bodies, struct_fields,
			declared_kinds)
	}
	out.write_string(type_bodies)
	return FastcTypeDeclarations{
		declarations:        out.str()
		enum_string_helpers: fastc_generate_enum_string_helpers(enum_infos)
		alias_base_types:    alias_base_types
	}
}

fn fastc_order_c_composite_definitions(source string, struct_fields map[string]map[string]string, declared_kinds map[string]FastcDeclaredTypeKind) string {
	mut ordered := source
	mut changed := true
	mut passes := 0
	for changed && passes < struct_fields.len {
		changed = false
		passes++
		for dependent, fields in struct_fields {
			for _, field_type in fields {
				dependency := fastc_by_value_composite_type(field_type, declared_kinds)
				if dependency == '' || dependency == dependent {
					continue
				}
				next := fastc_move_c_composite_before(ordered, dependency, dependent)
				if next != ordered {
					ordered = next
					changed = true
				}
			}
		}
	}
	return ordered
}

fn fastc_by_value_composite_type(field_type string, declared_kinds map[string]FastcDeclaredTypeKind) string {
	if field_type.ends_with('*') || field_type.starts_with('Array_')
		|| field_type.starts_with('Map_') {
		return ''
	}
	mut candidate := field_type
	if element_type := fastc_fixed_array_element_type(candidate) {
		candidate = element_type
	}
	for key, kind in declared_kinds {
		if kind in [.struct_, .union_, .interface_] && fastc_c_declared_type_name(key) == candidate {
			return candidate
		}
	}
	return ''
}

fn fastc_move_c_composite_before(source string, dependency string, dependent string) string {
	dependency_start := fastc_c_composite_definition_start(source, dependency) or { return source }
	dependent_start := fastc_c_composite_definition_start(source, dependent) or { return source }
	if dependency_start < dependent_start {
		return source
	}
	dependency_end := fastc_c_composite_definition_end(source, dependency_start) or {
		return source
	}
	mut end := dependency_end
	for end < source.len && source[end] == `\n` {
		end++
	}
	block := source[dependency_start..end]
	without := source[..dependency_start] + source[end..]
	insert_at := fastc_c_composite_definition_start(without, dependent) or { return source }
	return without[..insert_at] + block + without[insert_at..]
}

fn fastc_c_composite_definition_start(source string, name string) ?int {
	if start := source.index('struct ${name} {') {
		return int(start)
	}
	if start := source.index('union ${name} {') {
		return int(start)
	}
	return none
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

fn fastc_emit_source_type_declarations(source_file FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut composite_types map[string]bool, mut alias_base_types map[string]string, mut enum_infos []FastcEnumInfo, mut out strings.Builder) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(source_file.path, source_file.source.len)
	file.index_lines_without_digest(source_file.source)
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
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(),
					source_file.path, prefs)!
				if selected.source != '' {
					selected_source := FastcSourceFile{
						path:   source_file.path
						source: selected.source
						header: source_file.header
					}
					fastc_emit_source_type_declarations(selected_source, prefs, declared_types,
						declared_kinds, constants, public_constants, mut struct_fields, mut
						struct_field_info, mut composite_types, mut alias_base_types, mut
						enum_infos, mut out)!
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
		}
		if depth == 0 && !next_type_is_enabled
			&& tok in [.key_struct, .key_union, .key_enum, .key_interface, .key_type] {
			tok = fastc_skip_type_declaration(mut scan, tok)!
			next_enum_is_flag = false
			next_type_is_enabled = true
			continue
		}
		if depth == 0 && tok in [.key_struct, .key_union] {
			tok = fastc_emit_struct_declaration(mut scan, tok == .key_union, source_file,
				declared_types, prefs.building_v, mut struct_fields, mut struct_field_info, mut
				composite_types, mut out)!
			next_type_is_enabled = true
			continue
		}
		if depth == 0 && tok == .key_enum {
			tok = fastc_emit_enum_declaration(mut scan, source_file, next_enum_is_flag,
				declared_types, declared_kinds, constants, public_constants, mut enum_infos, mut out)!
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
			tok = fastc_emit_alias_declaration(mut scan, source_file, declared_types,
				declared_kinds, prefs.building_v, mut struct_fields, mut struct_field_info, mut
				alias_base_types, mut out)!
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

fn fastc_scan_declaration_attribute(mut scan scanner.Scanner, path string, prefs &pref.Preferences) !FastcDeclarationAttribute {
	mut tok := scan.scan()
	mut depth := 1
	mut is_flag := false
	mut is_params := false
	mut is_typedef := false
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
		tok:        tok
		is_enabled: is_enabled
		is_flag:    is_flag
		is_params:  is_params
		is_typedef: is_typedef
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
		if tok != .name {
			return error('fastc parser does not support struct `${name}` field token `${tok.str()}` in ${source_file.path}')
		}
		mut field_names := [scan.lit]
		tok = scan.scan()
		for tok == .comma {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support struct `${name}` grouped field in ${source_file.path}')
			}
			field_names << scan.lit
			tok = scan.scan()
		}
		if tok == .semicolon || tok == .rcbr {
			embedded_key := fastc_resolve_declared_type_key(source_file.header.module_name,
				field_names[0], source_file.header.imports, declared_types) or {
				return error('fastc parser does not support embedded field `${field_names[0]}` in ${source_file.path}')
			}
			if !is_c_struct {
				out.writeln('\t${fastc_c_declared_type_name(embedded_key)} __embedded_${embedded_id};')
			}
			fields_by_name['__embedded_${embedded_id}'] = fastc_c_declared_type_name(embedded_key)
			field_info << FastcStructField{
				name:        '__embedded_${embedded_id}'
				typ:         fastc_c_declared_type_name(embedded_key)
				is_public:   true
				module_name: source_file.header.module_name
				path:        source_file.path
				imports:     source_file.header.imports.clone()
			}
			embedded_id++
			fields++
			if tok == .semicolon {
				tok = scan.scan()
			}
			continue
		}
		field_type, next_token := fastc_scan_type(mut scan, tok, source_file.path,
			source_file.header.module_name, source_file.header.imports, declared_types,
			allow_short_placeholders) or {
			return error('fastc struct `${name}` field `${field_names[0]}`: ${err.msg()}')
		}
		tok = next_token
		fastc_register_composite_type(field_type, mut composite_types)
		mut is_required := false
		for tok == .attribute {
			mut attribute_is_required := false
			tok, attribute_is_required = fastc_scan_struct_field_attribute(mut scan)!
			is_required = is_required || attribute_is_required
		}
		mut default_source := ''
		if tok == .assign {
			first_default_token := scan.scan()
			default_start := scan.pos
			tok = fastc_skip_field_default_from_token(mut scan, first_default_token)!
			default_source =
				source_file.source[default_start..scan.pos].trim_space().trim_right(';').trim_space()
			if default_source == '' {
				return error('fastc parser does not support empty default for struct `${name}` field `${field_names[0]}` in ${source_file.path}')
			}
		}
		for field_name in field_names {
			c_field_name := fastc_c_identifier(field_name)
			if !is_c_struct {
				if element_type := fastc_fixed_array_element_type(field_type) {
					length := fastc_fixed_array_length(field_type) or {
						return error('invalid fixed array type')
					}
					out.writeln('\t${element_type} ${c_field_name}[${length}];')
				} else {
					out.writeln('\t${field_type} ${c_field_name};')
				}
			}
			fields_by_name[field_name] = field_type
			field_info << FastcStructField{
				name:           field_name
				typ:            field_type
				is_public:      fields_are_public
				is_mutable:     fields_are_mutable
				is_required:    is_required
				module_name:    source_file.header.module_name
				path:           source_file.path
				imports:        source_file.header.imports.clone()
				default_source: default_source
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
			name:        'data'
			typ:         'voidptr'
			is_public:   true
			module_name: source_file.header.module_name
			path:        source_file.path
			imports:     source_file.header.imports.clone()
		}
	}
	if !is_c_struct {
		out.writeln('};')
		out.writeln('')
	}
	struct_fields[c_name] = fields_by_name.clone()
	struct_field_info[c_name] = field_info.clone()
	return scan.scan()
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
	return none
}

fn fastc_semantic_declared_type_key(c_type string, declared_types map[string]bool) string {
	base := c_type.trim_right('*')
	for key in declared_types.keys() {
		if fastc_c_declared_type_name(key) == base {
			return key
		}
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
		if !is_flag || tok != .name || scan.lit != 'u64' {
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
		if tok != .name {
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
				symbolic_value = fastc_render_enum_value_expression(value_tokens, source_file,
					c_name, fields, declared_types, declared_kinds, constants, public_constants)!
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
		name:    name
		fields:  field_names.clone()
		is_flag: is_flag
	}
	fastc_emit_enum_print_function(c_name, name, field_names, is_flag, mut out)
	out.writeln('')
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
	value, next := fastc_render_enum_binary_expression(tokens, 0, 1, source_file, enum_c_name,
		fields, declared_types, declared_kinds, constants, public_constants)!
	if next != tokens.len {
		return error('fastc parser does not support enum discriminant `${fastc_expression_tokens_debug(tokens)}` in ${source_file.path}')
	}
	return value
}

fn fastc_render_enum_binary_expression(tokens []FastcExpressionToken, start int, minimum_precedence int, source_file FastcSourceFile, enum_c_name string, fields map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool) !(string, int) {
	mut left, mut next := fastc_render_enum_unary_expression(tokens, start, source_file,
		enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
	for next < tokens.len {
		operator := tokens[next].tok
		precedence := int(operator.left_binding_power())
		if precedence < minimum_precedence
			|| operator !in [.plus, .minus, .mul, .div, .mod, .pipe, .xor, .amp, .left_shift, .right_shift] {
			break
		}
		right, after_right := fastc_render_enum_binary_expression(tokens, next + 1,
			int(operator.right_binding_power()), source_file, enum_c_name, fields, declared_types,
			declared_kinds, constants, public_constants)!
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
		value, next := fastc_render_enum_unary_expression(tokens, start + 1, source_file,
			enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
		return '(${tokens[start].tok.str()}(${value}))', next
	}
	if tokens[start].tok == .lpar {
		value, next := fastc_render_enum_binary_expression(tokens, start + 1, 1, source_file,
			enum_c_name, fields, declared_types, declared_kinds, constants, public_constants)!
		if next >= tokens.len || tokens[next].tok != .rpar {
			return error('fastc parser does not support an unbalanced enum discriminant in ${source_file.path}')
		}
		return '(${value})', next + 1
	}
	if tokens[start].tok == .number {
		return fastc_c_number(tokens[start].lit)!, start + 1
	}
	return fastc_render_enum_symbol(tokens, start, source_file, enum_c_name, fields,
		declared_types, declared_kinds, constants, public_constants)
}

fn fastc_render_enum_symbol(tokens []FastcExpressionToken, start int, source_file FastcSourceFile, enum_c_name string, fields map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, constants map[string]string, public_constants map[string]bool) !(string, int) {
	if tokens[start].tok == .dot && start + 1 < tokens.len && tokens[start + 1].tok == .name
		&& tokens[start + 1].lit in fields {
		return '${enum_c_name}__${tokens[start + 1].lit}', start + 2
	}
	if tokens[start].tok != .name {
		return error('fastc parser does not support enum discriminant token `${tokens[start].tok.str()}` in ${source_file.path}')
	}
	name := tokens[start].lit
	if start + 4 < tokens.len && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name
		&& tokens[start + 3].tok == .dot && tokens[start + 4].tok == .name {
		if imported_module := source_file.header.imports[name] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if declared_types[type_key] && declared_kinds[type_key] == .enum_ {
				return '${fastc_c_declared_type_name(type_key)}__${tokens[start + 4].lit}', start +
					5
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
		type_key := fastc_resolve_declared_type_key(source_file.header.module_name, name,
			source_file.header.imports, declared_types) or { '' }
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
	out.writeln('static void v_fastc_print_enum_${c_name}(${c_name} value, bool newline) {')
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

fn fastc_generate_enum_string_helpers(infos []FastcEnumInfo) string {
	mut out := strings.new_builder(infos.len * 256)
	for info in infos {
		out.writeln('static string v_fastc_enum_str_${info.c_name}(${info.c_name} value) {')
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
	}
	return out.str()
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

fn fastc_emit_alias_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, allow_short_placeholders bool, mut struct_fields map[string]map[string]string, mut struct_field_info map[string][]FastcStructField, mut alias_base_types map[string]string, mut out strings.Builder) !token.Token {
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
	if tok != .assign {
		return error('fastc parser does not support type `${name}` declaration in ${source_file.path}')
	}
	tok = scan.scan()
	if tok == .key_fn {
		return fastc_emit_function_alias(mut scan, source_file, declared_types,
			allow_short_placeholders, c_name, mut out)
	}
	base, next_token := fastc_scan_type(mut scan, tok, source_file.path,
		source_file.header.module_name, source_file.header.imports, declared_types,
		allow_short_placeholders) or { return error('fastc type `${name}`: ${err.msg()}') }
	tok = next_token
	if tok == .semicolon {
		tok = scan.scan()
	}
	if tok == .pipe {
		for tok != .eof {
			tok = scan.scan()
			if tok == .semicolon {
				tok = scan.scan()
				if tok != .pipe {
					break
				}
			}
		}
		out.writeln('typedef struct { void *_object; u32 _typ; } ${c_name};')
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

fn fastc_emit_function_alias(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, allow_short_placeholders bool, c_name string, mut out strings.Builder) !token.Token {
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
		if tok == .key_mut {
			parameter_is_mut = true
			tok = scan.scan()
		}
		mut has_parameter_name := false
		if !parameter_is_mut && tok == .name {
			mut lookahead := scan
			next_token := lookahead.scan()
			has_parameter_name = next_token in [.name, .amp, .and, .mul, .question, .not, .key_fn]
		}
		if has_parameter_name {
			tok = scan.scan()
			parameter_type, next_token := fastc_scan_type(mut scan, tok, source_file.path,
				source_file.header.module_name, source_file.header.imports, declared_types,
				allow_short_placeholders)!
			parameter_types << parameter_type
			tok = next_token
		} else {
			parameter_type, next_token := fastc_scan_type(mut scan, tok, source_file.path,
				source_file.header.module_name, source_file.header.imports, declared_types,
				allow_short_placeholders)!
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
	if tok !in [.semicolon, .eof] {
		return_type, tok = fastc_scan_type(mut scan, tok, source_file.path,
			source_file.header.module_name, source_file.header.imports, declared_types,
			allow_short_placeholders)!
	}
	out.writeln('typedef ${return_type} (*${c_name})(${if parameter_types.len == 0 {
		'void'
	} else {
		parameter_types.join(', ')
	}});')
	if tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}
