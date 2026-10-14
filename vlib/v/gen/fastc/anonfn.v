module fastc

import os
import v.scanner
import v.token

// parse_anonymous_function hoists a function literal to C. Immutable captures use
// builtin.closure's per-instance context and callable trampoline.
//
// The literal is re-parsed as an ordinary named function via `parse_function`,
// reusing the same machinery the on-demand monomorphization drain relies on. Its
// definition is captured out of `g.out` and stored alongside the mono instances
// so it is emitted at top level; the enclosing expression only keeps the name.
fn (mut g Parser) parse_anonymous_function() !(string, string, bool) {
	anon_start := g.s.pos
	// Advance the main scanner past the whole literal: `fn`, an optional parameter
	// list, the return type, and the balanced body block. `body_end` marks the byte
	// just past the closing `}`.
	g.next() // consume `fn`
	mut capture_names := []string{}
	mut capture_types := []string{}
	mut capture_params := []string{}
	mut capture_values := []string{}
	if g.tok == .lsbr {
		if 'builtin.closure' !in g.imports.values() {
			return g.unsupported('function literals and closures without the real builtin runtime')
		}
		g.next()
		for g.tok != .rsbr {
			if g.tok != .name {
				return g.unsupported('mutable or expression closure captures')
			}
			capture_name := g.lit
			local := g.locals[capture_name] or {
				return g.unsupported('closure capture `${capture_name}` without a local value')
			}
			capture_type := fastc_normalize_inferred_type(local.typ)
			capture_source_type := g.capture_type_source(capture_type) or {
				return g.unsupported('closure capture `${capture_name}` of type `${capture_type}`')
			}
			capture_names << capture_name
			capture_types << capture_type
			capture_params << '${capture_name} ${capture_source_type}'
			capture_value := g.local_c_name(capture_name)
			capture_values << if capture_type.starts_with('Map_') {
				'builtin__map_clone(&(${capture_value}))'
			} else if capture_type.starts_with('Array_') {
				'builtin__array_clone_to_depth(&(${capture_value}), 1)'
			} else {
				capture_value
			}
			g.next()
			if g.tok == .comma {
				g.next()
			} else if g.tok != .rsbr {
				return g.unsupported('closure capture separator')
			}
		}
		g.next()
	}
	params_start := g.s.pos
	if g.tok == .lpar {
		g.skip_balanced(.lpar, .rpar)!
	}
	for g.tok != .lcbr && g.tok != .eof {
		g.next()
	}
	if g.tok != .lcbr {
		return g.unsupported('anonymous function body')
	}
	mut depth := 0
	mut body_end := 0
	for {
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			depth--
			body_end = g.s.offset
			g.next()
			if depth == 0 {
				break
			}
			continue
		} else if g.tok == .eof {
			return g.unsupported('unfinished anonymous function body')
		}
		g.next()
	}
	name := '__vf_anon_${g.module_name.replace('.', '_')}_${os.file_name(g.path).replace('.', '_')}_${anon_start}'
	// Rewrite `fn (...) ...` as `fn <name> (...) ...` so `parse_function` reads it as
	// an ordinary declaration.
	function_source := g.s.src[params_start..body_end]
	plain_synthetic := 'fn ${name} ${function_source}'
	c_name := fastc_c_function_name(g.module_name, name)
	mut signatures := map[string]FastcFunctionSignature{}
	collect_function_signatures(plain_synthetic, g.path, FastcSourceHeader{
		module_name: g.module_name
		imports:     g.imports
	}, g.prefs, []int{}, g.declared_types, g.declared_type_c_names, map[string]bool{},
		mut signatures)!
	if signature := signatures[fastc_function_key(g.module_name, name)] {
		g.mono_functions[c_name] = signature
	}
	synthetic := if capture_names.len == 0 {
		plain_synthetic
	} else {
		'fn ${name} (${capture_params.join(', ')}${if function_source[1..].trim_left(' \t\r\n').starts_with(')') {
			''
		} else {
			', '
		}}${function_source[1..]}'
	}

	// Save the resume position (already advanced past the literal) and every piece of
	// parser state `parse_function` mutates but does not itself restore.
	resume_s := g.s
	resume_tok := g.tok
	resume_lit := g.lit
	saved_locals := g.locals.clone()
	saved_temp_id := g.temp_id
	saved_indent := g.indent
	saved_scope_changes := g.local_scope_changes.clone()
	saved_scope_depth := g.local_scope_depth
	saved_drain := g.in_mono_drain

	start := g.out.len
	g.indent = 0
	// Emit the hoisted function even though its synthetic name is not in the reachable
	// set, exactly as the mono drain does for on-demand instances.
	g.in_mono_drain = true
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(g.path, synthetic.len)
	file.index_lines_without_digest(synthetic)
	g.s = scanner.new_scanner(g.prefs, .normal)
	g.s.init(file, synthetic)
	g.next()
	g.parse_function(true) or {
		// Restore before propagating so the outer parse can report cleanly.
		g.s = resume_s
		g.tok = resume_tok
		g.lit = resume_lit
		g.locals = saved_locals.clone()
		g.temp_id = saved_temp_id
		g.indent = saved_indent
		g.local_scope_changes = saved_scope_changes.clone()
		g.local_scope_depth = saved_scope_depth
		g.in_mono_drain = saved_drain
		g.out.go_back(g.out.len - start)
		return err
	}
	definition := g.out.after(start)
	g.out.go_back(g.out.len - start)
	if definition != '' {
		g.generated_mono[c_name] = true
		g.mono_definitions[c_name] = definition
	}

	// Restore the enclosing parse and resume just past the literal.
	g.s = resume_s
	g.tok = resume_tok
	g.lit = resume_lit
	g.locals = saved_locals.clone()
	g.temp_id = saved_temp_id
	g.indent = saved_indent
	g.local_scope_changes = saved_scope_changes.clone()
	g.local_scope_depth = saved_scope_depth
	g.in_mono_drain = saved_drain
	if capture_names.len == 0 {
		return '(&${c_name})', c_name, false
	}
	signature := g.mono_functions[c_name]
	context_name := '${c_name}_captures'
	wrapper_name := '${c_name}_closure'
	mut fields := []string{}
	mut initializers := []string{}
	mut wrapper_params := []string{}
	mut call_args := []string{}
	for i, capture_name in capture_names {
		field_name := fastc_c_identifier(capture_name)
		fields << '${capture_types[i]} ${field_name};'
		initializers << '.${field_name}=(${capture_values[i]})'
		call_args << '__vf_ctx->${field_name}'
	}
	for i, parameter_type in signature.parameter_types {
		wrapper_params << '${fastc_output_c_type(parameter_type)} __vf_arg_${i}'
		call_args << '__vf_arg_${i}'
	}
	closure_global := g.globals[fastc_global_key('builtin.closure', 'g_closure')]
	create_function := g.c_function_name_for_key(fastc_function_key('builtin.closure', 'closure_create_with_data'))
	return_type := fastc_output_c_type(signature.return_type)
	return_prefix := if return_type == 'void' { '' } else { 'return ' }
	g.spawn_helpers[context_name] = 'typedef struct { ${fields.join(' ')} } ${context_name};\n'
	g.spawn_helpers[wrapper_name] = '${return_type} ${wrapper_name}(${if wrapper_params.len == 0 {
		'void'
	} else {
		wrapper_params.join(', ')
	}}) {\n${context_name} *__vf_ctx = (${context_name} *)((voidptr (*)(void))${closure_global}.closure_get_data)();\n${return_prefix}${c_name}(${call_args.join(', ')});\n}\n'
	return '({ ${context_name} __vf_capture_value = {${initializers.join(', ')}}; (${return_type} (*)(${if signature.parameter_types.len == 0 {
		'void'
	} else {
		signature.parameter_types.map(fastc_output_c_type(it)).join(', ')
	}}))${create_function}((voidptr)${wrapper_name}, builtin__memdup(&__vf_capture_value, sizeof(__vf_capture_value)), true); })', c_name, true
}

// local_closure_has_only_scoped_calls proves a fresh local callback does not
// escape its declaring block. Value uses (including aliases and assignments),
// asynchronous calls, nested functions and function defers keep runtime ownership.
fn (g &Parser) local_closure_has_only_scoped_calls(name string) bool {
	mut scan := g.s
	mut tok := g.tok
	mut lit := g.lit
	mut depth := 0
	for tok != .eof {
		if tok == .rcbr {
			if depth == 0 {
				return true
			}
			depth--
		} else if tok == .lcbr {
			depth++
		} else if tok == .key_fn {
			if !fastc_skip_unrelated_closure(mut scan, name) {
				return false
			}
		} else if tok in [.key_spawn, .key_go, .hash] {
			return false
		} else if tok == .name && lit == name {
			mut next := scan
			if next.scan() != .lpar {
				return false
			}
		}
		tok = scan.scan()
		lit = scan.lit
	}
	return false
}

fn fastc_skip_unrelated_closure(mut scan scanner.Scanner, name string) bool {
	mut tok := scan.scan()
	// `defer(fn)` runs after the declaring block has ended.
	if tok == .rpar {
		return false
	}
	mut depth := 0
	for tok != .eof {
		if tok == .name && scan.lit == name {
			return false
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if depth == 0 {
				return true
			}
		}
		tok = scan.scan()
	}
	return false
}

fn (mut g Parser) add_local_closure_cleanup(name string) {
	destroy := g.c_function_name_for_key(fastc_function_key('builtin.closure', 'closure_try_destroy'))
	line := '${destroy}((voidptr)${g.local_c_name(name)});'
	if name == '_' {
		g.write_line(line)
		return
	}
	g.deferred_block_starts << g.deferred_lines.len
	g.deferred_lines << '\t'.repeat(g.indent) + line
}

fn (g &Parser) capture_type_source(typ string) ?string {
	if typ.ends_with('*') {
		return '&' + g.capture_type_source(typ[..typ.len - 1])?
	}
	if typ.starts_with('Array_') {
		element := g.array_element_type(typ)?
		return '[]' + g.capture_type_source(element)?
	}
	if typ.starts_with('Map_') {
		key, value := g.map_key_value_types(typ)?
		return 'map[' + g.capture_type_source(key)? + ']' + g.capture_type_source(value)?
	}
	if fastc_primitive_c_type(typ) != none {
		return typ
	}
	key := g.semantic_type_key(typ)
	if key !in g.declared_types {
		return none
	}
	module_name := key.all_before_last('.')
	name := key.all_after_last('.')
	if module_name == g.module_name || module_name == 'builtin' {
		return name
	}
	for alias, imported_module in g.imports {
		if imported_module == module_name {
			return '${alias}.${name}'
		}
	}
	return none
}
