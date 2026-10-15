module fastc

import v.flat
import v.token

fn (mut g Parser) parse_for() !bool {
	g.next()
	if g.tok == .lcbr {
		g.next()
		g.write_line('for (;;) {')
		g.indent++
		loop_result := g.parse_loop_block_body()!
		g.indent--
		g.write_line('}')
		return !loop_result.has_reachable_break
	}
	mut item_is_mut := false
	if g.tok == .key_mut {
		item_is_mut = true
		g.next()
	}
	if g.tok == .semicolon {
		g.next()
		condition := g.read_condition_expression([token.Token.semicolon])!
		g.expect(.semicolon)!
		// `for ; cond; {` has no update.
		has_update := g.tok != .lcbr
		update := if has_update { g.read_statement_expression([token.Token.lcbr])! } else { '' }
		if has_update && (update == '' || !g.last_expression_is_statement()) {
			return g.unsupported('C-style for update expression')
		}
		g.expect(.lcbr)!
		g.write_line('for (; ${condition}; ${update}) {')
		g.indent++
		_ = g.parse_loop_block_body()!
		g.indent--
		g.write_line('}')
		return false
	}
	if g.tok == .name || (g.tok == .key_shared && g.shared_token_is_identifier(.unknown)) {
		name := g.lit
		g.next()
		mut value_name := ''
		mut value_is_mut := false
		if g.tok == .comma {
			g.next()
			if g.tok == .key_mut {
				// `for k, mut v in m`: the value binds mutably (the map value is a
				// pointer, so mutations through `v` persist).
				value_is_mut = true
				g.next()
			}
			if g.tok != .name && !(g.tok == .key_shared && g.shared_token_is_identifier(.unknown)) {
				return g.unsupported('for-in value name')
			}
			value_name = g.lit
			g.next()
		}
		if g.tok == .key_in {
			// `_` is the blank identifier: it never binds a local, so nested or
			// sibling `for _ in ...` loops must not be seen as redeclarations.
			if name != '_' && name in g.locals {
				return g.unsupported('redeclaration of `${name}`')
			}
			g.next()
			// A two-value for-in whose collection is an inline map literal
			// (`for k, v in { 'a': 1, ... } { ... }`) opens with `{`; a plain
			// read_expression would stop at that brace and infer an empty
			// collection, so read the map literal directly here.
			start := if value_name != '' && g.tok == .lcbr {
				g.read_inferred_map_literal()!
			} else {
				g.read_expression([token.Token.dotdot, token.Token.lcbr])!
			}
			start_expression_type := g.last_expression_type
			start_expression := g.last_expression
			if g.tok == .dotdot {
				if item_is_mut || value_name != '' {
					return g.unsupported('mutable or two-value range loop')
				}
				g.next()
				end := g.read_expression([token.Token.lcbr])!
				end_expression_type := g.last_expression_type
				end_expression := g.last_expression
				if start_value := fastc_integer_literal_value(start_expression) {
					if end_value := fastc_integer_literal_value(end_expression) {
						if start_value >= end_value {
							return g.unsupported('empty range: `${start_expression[0].lit} .. ${end_expression[0].lit}` will never execute')
						}
					}
				}
				g.expect(.lcbr)!
				start_name := g.temporary_name('range_start')
				end_name := g.temporary_name('range_end')
				// The blank `_` binds no local, so give it a private counter name
				// rather than emitting `_` as a C variable.
				c_name := if name == '_' {
					g.temporary_name('range_index')
				} else {
					fastc_c_identifier(name)
				}
				// V evaluates both range bounds exactly once, from left to right.
				start_type := g.declaration_c_type(start_expression_type, start)
				end_type := g.declaration_c_type(end_expression_type, end)
				g.write_line('${start_type} ${start_name} = (${start});')
				g.write_line('${end_type} ${end_name} = (${end});')
				g.write_line('for (${start_type} ${c_name} = (${start_name}); ${c_name} < (${end_name}); ${c_name}++) {')
				if name != '_' {
					g.locals[name] = FastcLocal{
						typ: fastc_normalize_inferred_type(start_expression_type)
					}
				}
				g.indent++
				_ = g.parse_loop_block_body()!
				g.indent--
				if name != '_' {
					g.locals.delete(name)
				}
				g.write_line('}')
				return false
			}
			if g.tok != .lcbr {
				return g.unsupported('for-in collection')
			}
			collection_type := fastc_normalize_inferred_type(start_expression_type)
			collection_layout_type := g.underlying_alias_type(collection_type)
			if item_is_mut {
				if g.array_element_type(collection_layout_type) == none {
					return g.unsupported('mutable iteration over non-array collection `${start}`')
				}
				if !g.mutable_collection_expression(start_expression) {
					return g.unsupported('mutable iteration over immutable collection `${start}`')
				}
			}
			if flat.source_type_name(collection_layout_type).trim_right('*').starts_with('Map_') {
				key_type_source_id, map_value_type_source_id := g.map_key_value_types(collection_layout_type) or {
					return g.unsupported('map iteration type `${flat.source_type_name(collection_type)}`')
				}
				key_type := key_type_source_id
				map_value_type := map_value_type_source_id

				g.next()
				collection_name := g.temporary_name('map_collection')
				keys_name := g.temporary_name('map_keys')
				values_name := g.temporary_name('map_values')
				index_name := g.temporary_name('map_index')
				collection_c_type := g.declaration_c_type(start_expression_type, start)
				g.write_line('${collection_c_type} ${collection_name} = (${start});')
				map_pointer := if flat.source_type_name(collection_layout_type).ends_with('*') {
					collection_name
				} else {
					'&${collection_name}'
				}
				g.write_line('array ${keys_name} = builtin__map_keys((map *)${map_pointer});')
				if value_name != '' {
					g.write_line('array ${values_name} = builtin__map_values((map *)${map_pointer});')
				}
				g.write_line('for (int ${index_name} = 0; ${index_name} < ${keys_name}.len; ${index_name}++) {')
				g.indent++
				if name != '_' {
					g.write_line('${g.collection_storage_type(key_type)} ${fastc_c_identifier(name)} = ((${g.collection_storage_type(key_type)} *)${keys_name}.data)[${index_name}];')
					g.locals[name] = FastcLocal{
						typ: key_type
					}
				}
				if value_name != '' && value_name != '_' {
					g.write_line('${g.collection_storage_type(map_value_type)} ${fastc_c_identifier(value_name)} = ((${g.collection_storage_type(map_value_type)} *)${values_name}.data)[${index_name}];')
					g.locals[value_name] = FastcLocal{
						is_mut: value_is_mut
						typ:    map_value_type
					}
				}
				_ = g.parse_loop_block_body()!
				g.indent--
				g.locals.delete(name)
				if value_name != '' {
					g.locals.delete(value_name)
				}
				g.write_line('}')
				return false
			}
			if !item_is_mut && flat.source_type_name(collection_layout_type).trim_right('*') != 'string'
				&& g.array_element_type(collection_layout_type) == none {
				if iterator := g.iterator_next_method(collection_layout_type) {
					g.next()
					g.parse_for_in_iterator(iterator, start, start_expression_type, collection_layout_type,
						name, value_name)!
					return false
				}
			}
			element_type := if flat.source_type_name(collection_layout_type).trim_right('*') == 'string' {
				flat.source_type_u8
			} else {
				g.array_element_type(collection_layout_type) or {
					return g.unsupported('for-in collection `${start}` of type `${flat.source_type_name(collection_type)}`')
				}
			}
			element_c_type := fastc_output_c_type(flat.source_type_name(element_type))
			fixed_length := fastc_fixed_array_length(flat.source_type_name(collection_layout_type).trim_right('*')) or {
				''
			}
			is_fixed_array := fixed_length != ''
			is_raw_fixed_array := is_fixed_array && g.fixed_array_uses_raw_storage(fastc_strip_paren_tokens(start_expression))
			g.next()
			collection_name := g.temporary_name('collection')
			is_ordinary_string := !g.selfhost && collection_layout_type == flat.source_type_string
			index_name := if value_name == '' && name != '_' {
				g.temporary_name('index')
			} else if name == '_' {
				g.temporary_name('index')
			} else {
				fastc_c_identifier(name)
			}
			access := if flat.source_type_name(collection_layout_type).ends_with('*') {
				'->'
			} else {
				'.'
			}
			data_field := if flat.source_type_name(collection_layout_type).trim_right('*') == 'string' {
				'str'
			} else {
				'data'
			}
			if is_fixed_array {
				fixed_data := if is_raw_fixed_array {
					'((${start})[0])'
				} else {
					fixed_access := if flat.source_type_name(collection_layout_type).ends_with('*') {
						'->'
					} else {
						'.'
					}
					'((${start})${fixed_access}data[0])'
				}
				g.write_line('${element_c_type} *${collection_name} = &${fixed_data};')
			} else if is_ordinary_string {
				g.write_line('string ${collection_name} = (${start});')
			} else {
				collection_c_type := g.declaration_c_type(start_expression_type, start)
				g.write_line('${collection_c_type} ${collection_name} = (${start});')
			}
			collection_length := if is_ordinary_string {
				'strlen(${collection_name} ? ${collection_name} : "")'
			} else if is_fixed_array {
				fixed_length
			} else {
				'${collection_name}${access}len'
			}
			collection_data := if is_fixed_array {
				collection_name
			} else {
				'${collection_name}${access}${data_field}'
			}
			g.write_line('for (int ${index_name} = 0; ${index_name} < ${collection_length}; ${index_name}++) {')
			g.indent++
			actual_value_name := if value_name == '' { name } else { value_name }
			if actual_value_name != '_' {
				c_value_name := fastc_c_identifier(actual_value_name)
				// `for i, mut x in arr` makes the value variable a mutable reference to the
				// element exactly like `for mut x in arr`; the `mut` is just carried on the
				// second binding (`value_is_mut`) instead of the first.
				if item_is_mut || value_is_mut {
					if flat.source_type_name(element_type).ends_with('*') {
						// Pointer elements are already references to mutable values. Taking the
						// array slot's address here would incorrectly bind `x` as `&&T`.
						g.write_line('${element_c_type} ${c_value_name} = ((${element_c_type} *)${collection_data})[${index_name}];')
						g.locals[actual_value_name] = FastcLocal{
							is_mut: true
							typ:    element_type
						}
					} else {
						g.write_line('${element_c_type} *${c_value_name} = &(((${element_c_type} *)${collection_data})[${index_name}]);')
						g.locals[actual_value_name] = FastcLocal{
							is_mut:       true
							is_reference: true
							typ:          flat.source_type_id(flat.source_type_name(element_type) + '*')
						}
					}
				} else if is_ordinary_string {
					g.write_line('u8 ${c_value_name} = ((const unsigned char *)${collection_name})[${index_name}];')
					g.locals[actual_value_name] = FastcLocal{
						typ: flat.source_type_u8
					}
				} else if !is_fixed_array && flat.source_type_name(collection_layout_type).ends_with('*') && flat.source_type_name(collection_layout_type).trim_right('*') != 'string' {
					g.write_line('${element_c_type} *${c_value_name} = &(((${element_c_type} *)${collection_data})[${index_name}]);')
					g.locals[actual_value_name] = FastcLocal{
						typ: flat.source_type_id(flat.source_type_name(element_type) + '*')
					}
				} else {
					g.write_line('${element_c_type} ${c_value_name} = ((${element_c_type} *)${collection_data})[${index_name}];')
					g.locals[actual_value_name] = FastcLocal{
						typ: element_type
					}
				}
			}
			if value_name != '' && name != '_' {
				g.locals[name] = FastcLocal{
					typ: flat.source_type_int
				}
			}
			_ = g.parse_loop_block_body()!
			g.indent--
			g.locals.delete(name)
			if value_name != '' {
				g.locals.delete(value_name)
			}
			g.write_line('}')
			return false
		}
		if g.tok in [.decl_assign, .assign] {
			is_declaration := g.tok == .decl_assign
			if is_declaration && name in g.locals {
				return g.unsupported('redeclaration of `${name}`')
			}
			if !is_declaration {
				local := g.locals[name] or {
					return g.unsupported('assignment to undeclared loop variable `${name}`')
				}
				if !local.is_mut && g.unsafe_depth == 0 {
					return g.unsupported('assignment to immutable loop variable `${name}`')
				}
			}
			g.next()
			initial := g.read_expression([token.Token.semicolon])!
			initial_type := fastc_normalize_inferred_type(g.last_expression_type)
			g.expect(.semicolon)!
			if is_declaration {
				g.locals[name] = FastcLocal{
					is_mut: true
					typ:    initial_type
				}
			}
			condition := g.read_expression([token.Token.semicolon])!
			g.expect(.semicolon)!
			update := g.read_statement_expression([token.Token.lcbr])!
			g.expect(.lcbr)!
			initializer := if is_declaration {
				'${g.declaration_c_type(initial_type, initial)} ${fastc_c_identifier(name)} = (${initial})'
			} else {
				'${fastc_c_identifier(name)} = (${initial})'
			}
			g.write_line('for (${initializer}; ${condition}; ${update}) {')
			g.indent++
			_ = g.parse_loop_block_body()!
			g.indent--
			if is_declaration {
				g.locals.delete(name)
			}
			g.write_line('}')
			return false
		}
		g.validate_expression_name(name, .unknown)!
		condition := g.read_expression_with_prefix(name, [token.Token.lcbr])!
		condition_tokens := g.last_expression.clone()
		g.expect(.lcbr)!
		if g.write_condition_loop(condition, condition_tokens)! {
			return false
		}
		g.write_line('while (${condition}) {')
		g.indent++
		_ = g.parse_loop_block_body()!
		g.indent--
		g.write_line('}')
		return false
	}
	condition := g.read_expression([token.Token.lcbr])!
	condition_tokens := g.last_expression.clone()
	g.expect(.lcbr)!
	if g.write_condition_loop(condition, condition_tokens)! {
		return false
	}
	g.write_line('while (${condition}) {')
	g.indent++
	_ = g.parse_loop_block_body()!
	g.indent--
	g.write_line('}')
	return false
}

// write_condition_loop emits a `for <cond> { … }` loop whose condition narrows a boxed member
// (`for x.f is T { … x.f.field … }`), keeping the smart-cast live through the body exactly as
// an `if` does. Because the condition is re-checked each iteration (its tag test reads a
// per-iteration temp), the loop is emitted as `while (1) { <boxed temps>; if (!cond) break;
// <member ptrs>; body }`. Returns false (emitting nothing) when the condition has no member
// smart-cast, so the caller falls back to a plain `while (cond)`.
fn (mut g Parser) write_condition_loop(condition string, condition_tokens []FastcExpressionToken) !bool {
	if !g.selfhost {
		return false
	}
	plans, rewritten_condition := g.detect_member_smartcasts(condition_tokens, condition)
	if plans.len == 0 {
		return false
	}
	loop_condition := if rewritten_condition != '' { rewritten_condition } else { condition }
	g.write_line('while (1) {')
	g.indent++
	for plan in plans {
		boxed_zero := if plan.boxed_type_text().ends_with('*') {
			'NULL'
		} else {
			'(${plan.boxed_type_text()}){0}'
		}
		g.write_line('${plan.boxed_type_text()} ${plan.boxed_tmp} = ${boxed_zero};')
	}
	g.write_line('if (!(${loop_condition})) { break; }')
	mut previous_member_smartcasts := map[string]FastcMemberSmartcast{}
	mut had_member_smartcasts := map[string]bool{}
	for plan in plans {
		plan_access := if plan.boxed_type_text().ends_with('*') { '->' } else { '.' }
		g.write_line('${plan.type_c_text()} *${plan.member_tmp} = (${plan.type_c_text()} *)${plan.boxed_tmp}${plan_access}_object;')
		if plan.path !in previous_member_smartcasts {
			had_member_smartcasts[plan.path] = plan.path in g.member_smartcasts
			previous_member_smartcasts[plan.path] = g.member_smartcasts[plan.path] or {
				FastcMemberSmartcast{}
			}
		}
		g.member_smartcasts[plan.path] = FastcMemberSmartcast{
			typ:    flat.source_type_id(plan.type_c_text() + '*')
			source: plan.member_tmp
		}
	}
	_ = g.parse_loop_block_body()!
	for path, present in had_member_smartcasts {
		if present {
			g.member_smartcasts[path] = previous_member_smartcasts[path]
		} else {
			g.member_smartcasts.delete(path)
		}
	}
	g.indent--
	g.write_line('}')
	return true
}

fn (g &Parser) mutable_collection_expression(tokens []FastcExpressionToken) bool {
	for i, item in tokens {
		if item.tok != .name {
			continue
		}
		if local := g.locals[item.lit] {
			// An `unsafe { … }` collection is the programmer's explicit assertion that
			// mutable iteration is intended (`for mut f in unsafe { s.fields }`); the
			// array's data is shared regardless, so the mutation reaches the source.
			return local.is_mut || item.unsafe_depth > 0
		}
		if imported_module := g.imports[item.lit] {
			if i + 2 >= tokens.len || tokens[i + 1].tok != .dot
				|| tokens[i + 2].tok != .name {
				return false
			}
			global_key := fastc_global_key(g.resolve_module_alias(imported_module),
				tokens[i + 2].lit)
			return global_key in g.globals
		}
		// A global is mutable storage, including a member collection rooted
		// in it. Local bindings above retain precedence over global names.
		return g.unqualified_global_key(item.lit) != none
	}
	return false
}

// FastcIteratorNext is the `next() ?T` method that makes a value iterable in a for-in.
struct FastcIteratorNext {
	c_name       string
	element_type u32
	by_pointer   bool
}

// iterator_next_method finds the `next() ?T` method of `collection_type`, if it has one.
fn (g &Parser) iterator_next_method(collection_type_source_id u32) ?FastcIteratorNext {
	collection_type := flat.source_type_name(collection_type_source_id)
	key, path := g.resolve_method(flat.source_type_id(fastc_trim_pointer_suffix(collection_type)), 'next')
	signature := g.functions[key] or { return none }
	if path.len > 0 || signature.parameter_types.ids.len != 1 || signature.return_type_text() != 'Option'
		|| signature.option_type_text() in ['', 'void', 'MultiReturn'] {
		return none
	}
	receiver := signature.parameter_types.at(0)
	return FastcIteratorNext{
		c_name:       g.c_function_name_or(key, fastc_method_c_name(signature.module_name, receiver,
			'next'))
		element_type: signature.option_type
		by_pointer:   receiver.ends_with('*')
	}
}

// parse_for_in_iterator lowers `for x in it {` and `for i, x in it {` over an iterator:
// `next()` is called on a copy of `it` until it returns none.
fn (mut g Parser) parse_for_in_iterator(iterator FastcIteratorNext, start string, start_type_source_id u32, layout_type_source_id u32, name string, value_name string) ! {
	layout_type := flat.source_type_name(layout_type_source_id)
	iterator_name := g.temporary_name('iterator')
	option_name := g.temporary_name('iterator_next')
	index_name := if value_name != '' && name != '_' {
		fastc_c_identifier(name)
	} else {
		g.temporary_name('index')
	}
	is_pointer := layout_type.ends_with('*')
	receiver := if iterator.by_pointer && !is_pointer {
		'&${iterator_name}'
	} else if !iterator.by_pointer && is_pointer {
		'*${iterator_name}'
	} else {
		iterator_name
	}
	g.write_line('${g.declaration_c_type(start_type_source_id, start)} ${iterator_name} = (${start});')
	g.write_line('for (int ${index_name} = 0;; ${index_name}++) {')
	g.indent++
	g.write_line('Option ${option_name} = ${iterator.c_name}(${receiver});')
	g.write_line('if (${option_name}.state != 0) break;')
	value := if value_name == '' { name } else { value_name }
	if value != '_' {
		payload := if g.prefs.target_libc_headers {
			g.option_value_expression(iterator.element_type, option_name)
		} else {
			'*((${flat.source_type_name(iterator.element_type)} *)${option_name}.data)'
		}
		g.write_line('${flat.source_type_name(iterator.element_type)} ${fastc_c_identifier(value)} = ${payload};')
		g.locals[value] = FastcLocal{
			typ: iterator.element_type
		}
	}
	if value_name != '' && name != '_' {
		g.locals[name] = FastcLocal{
			typ: flat.source_type_int
		}
	}
	_ = g.parse_loop_block_body()!
	g.indent--
	g.locals.delete(name)
	if value_name != '' {
		g.locals.delete(value_name)
	}
	g.write_line('}')
}
