module types

import os
import v.flat

// VlsPos is where a declaration is: a file and a byte offset in it.
struct VlsPos {
	file_id int
	offset  int
}

// vls_definition answers a go-to-definition request with where the name under
// the cursor is declared, as V1 printed it: `path:line:column`, the line
// 1-based and the column 0-based. `input` is what the command line checks.
fn (mut tc TypeChecker) vls_definition(target VlsTarget, input string) string {
	at := tc.vls_definition_at(target) or { return '' }
	return tc.vls_position_text(at.file_id, at.offset, input)
}

// vls_position_text writes a declaration's position as V1 did: a file of the
// directory `input` with that directory as the command line wrote it,
// `./main.v` for `.`, the file `input` as written, any other with its full
// path.
fn (tc &TypeChecker) vls_position_text(file_id int, offset int, input string) string {
	file := tc.a.source_files[file_id] or { return '' }
	line, col := file.find_line_and_column(offset)
	mut path := file.name
	if input != '' {
		real_path := os.real_path(path)
		if os.is_dir(input) {
			if os.dir(real_path) == os.real_path(input) {
				path = '${input.trim_right('/')}/${os.file_name(path)}'
			}
		} else if real_path == os.real_path(input) {
			path = input
		}
	}
	return '${path}:${line}:${col - 1}'
}

// vls_definition_at returns the file and byte offset where the name the
// cursor is on is declared.
fn (mut tc TypeChecker) vls_definition_at(target VlsTarget) ?VlsPos {
	id := target.id
	node := tc.a.nodes[int(id)]
	tc.vls_enter_file(target.file_id)
	if call_id := tc.vls_called_by(id) {
		resolved := tc.vls_call_target(call_id, id) or { return tc.vls_local_definition(id) }
		return tc.vls_function_definition(resolved)
	}
	match node.kind {
		.ident {
			if module_name := tc.vls_import_symbol_module(id) {
				return tc.vls_module_member_definition(module_name, node.value)
			}
			if at := tc.vls_local_definition(id) {
				return at
			}
			if node.value == 'err' {
				if at := tc.vls_err_block(id) {
					return at
				}
			}
			if node.value in ['it', 'a', 'b'] {
				if at := tc.vls_implicit_var_at(id, node.value) {
					return at
				}
			}
			if at := tc.vls_const_definition(node.value) {
				return at
			}
			if at := tc.vls_global_definition(node.value) {
				return at
			}
			if at := tc.vls_type_definition(node.value) {
				return at
			}
			// A module name before one of its members stands for that member.
			if member := tc.vls_module_receiver_member(id) {
				if at := tc.vls_module_member_definition(node.value, member) {
					return at
				}
			}
			// A function named without a call: a callback.
			return tc.vls_function_definition(tc.qualify_name(node.value))
		}
		.selector {
			return tc.vls_selector_definition(node)
		}
		.enum_val {
			typ := tc.expr_type(id) or { tc.vls_match_subject_type(id) or { return none } }
			return tc.vls_enum_value_definition(vls_unwrap_type(typ), node.value)
		}
		.cast_expr, .struct_init, .is_expr, .as_expr {
			return tc.vls_type_definition(node.value)
		}
		.param, .enum_field {
			return VlsPos{int(node.pos.id), int(node.pos.offset)}
		}
		.field_init {
			owner := tc.vls_field_init_owner(id)?
			return tc.vls_field_definition(tc.vls_member_owner(owner)?, node.value)
		}
		else {
			return none
		}
	}
}

// vls_local_definition is where a local name is declared.
fn (tc &TypeChecker) vls_local_definition(id flat.NodeId) ?VlsPos {
	decl_id := tc.vls_local_declaration(id)?
	decl := tc.a.node(decl_id)
	if decl.kind == .param {
		if at := tc.vls_receiver_name_at(decl_id, decl) {
			return at
		}
	}
	return VlsPos{int(decl.pos.id), int(decl.pos.offset)}
}

// vls_err_block is where the implicit `err` that `id` names comes from: the
// `{` of the `or {}` block around it, or of the `else {}` of an `if x := f()`.
fn (tc &TypeChecker) vls_err_block(id flat.NodeId) ?VlsPos {
	mut child := id
	mut parent := tc.direct_parent_id(child)
	for tc.valid_node_id(parent) {
		p := tc.a.node(parent)
		if p.kind in [.fn_decl, .fn_literal, .lambda_expr, .file] {
			return none
		}
		if p.kind == .block {
			block_parent_id := tc.direct_parent_id(parent)
			if tc.valid_node_id(block_parent_id) {
				owner := tc.a.node(block_parent_id)
				from_or := owner.kind == .or_expr && owner.children_count > 1
					&& tc.a.child(owner, 1) == parent
				from_else := owner.kind == .if_expr && owner.children_count > 2
					&& tc.a.child(owner, 2) == parent
					&& tc.a.child_node(owner, 0).kind == .decl_assign
				if from_or || from_else {
					return VlsPos{int(p.pos.id), int(p.pos.offset)}
				}
			}
		}
		child = parent
		parent = tc.direct_parent_id(child)
	}
	return none
}

// vls_implicit_var_at is where the implicit `it` of `.map(it.x)`, `.filter()`,
// `.any()`, `.all()` and `.count()`, or the `a` and `b` of `.sort(a < b)` and
// `.sorted()`, come from: the name of that method, as V1 answered.
fn (tc &TypeChecker) vls_implicit_var_at(id flat.NodeId, name string) ?VlsPos {
	methods := if name == 'it' {
		['filter', 'map', 'any', 'all', 'count']
	} else {
		['sort', 'sorted']
	}
	mut child := id
	mut parent := tc.direct_parent_id(child)
	for tc.valid_node_id(parent) {
		p := tc.a.node(parent)
		if p.kind in [.fn_decl, .fn_literal, .lambda_expr, .file] {
			return none
		}
		if p.kind == .call && p.children_count > 1 && tc.a.child(p, 0) != child {
			callee := tc.a.child_node(p, 0)
			if callee.kind == .selector && callee.value in methods {
				return VlsPos{int(callee.pos.id), int(callee.pos.end) - callee.value.len}
			}
		}
		child = parent
		parent = tc.direct_parent_id(child)
	}
	return none
}

// vls_receiver_name_at finds the name of a method's receiver in the source:
// the parser gives the receiver no position of its own.
fn (tc &TypeChecker) vls_receiver_name_at(param_id flat.NodeId, param &flat.Node) ?VlsPos {
	fn_id := tc.direct_parent_id(param_id)
	if !tc.valid_node_id(fn_id) {
		return none
	}
	fn_node := tc.a.node(fn_id)
	if fn_node.kind != .fn_decl || !fn_node.value.contains('.') || fn_node.children_count == 0
		|| tc.a.child(fn_node, 0) != param_id {
		return none
	}
	source := tc.vls_source(int(fn_node.pos.id))
	name_offset := int(fn_node.pos.offset)
	open := source[..name_offset].last_index('(') or { return none }
	close := source[..name_offset].last_index(')') or { return none }
	header := source[open + 1..close]
	mut rel := 0
	for word in header.fields() {
		start := header.index_after(word, rel) or { break }
		rel = start + word.len
		if word == param.value {
			return VlsPos{int(fn_node.pos.id), open + 1 + start}
		}
	}
	return none
}

// vls_function_definition is where the function a call resolved to is
// declared, or the member of an interface that declares a method.
fn (tc &TypeChecker) vls_function_definition(resolved string) ?VlsPos {
	if decl_id := tc.vls_fn_decl_id(resolved) {
		decl := tc.a.node(decl_id)
		return VlsPos{int(decl.pos.id), int(decl.pos.offset)}
	}
	if decl_id := tc.vls_builtin_method_decl(resolved) {
		decl := tc.a.node(decl_id)
		return VlsPos{int(decl.pos.id), int(decl.pos.offset)}
	}
	interface_name := resolved.all_before_last('.')
	method := resolved.all_after_last('.')
	if interface_name == resolved {
		return none
	}
	index := tc.first_type_declaration_ids[interface_name] or { return none }
	decl := tc.a.nodes[index]
	for i in 0 .. decl.children_count {
		member := tc.a.child_node(&decl, i)
		if member.kind == .interface_field && member.value == method {
			return VlsPos{int(member.pos.id), int(member.pos.offset)}
		}
	}
	return none
}

fn (tc &TypeChecker) vls_const_definition(name string) ?VlsPos {
	qualified := if name.contains('.') { name } else { tc.qualify_name(name) }
	expr_id := tc.const_exprs[qualified] or { tc.const_exprs[name] or { return none } }
	field_id := tc.direct_parent_id(expr_id)
	if !tc.valid_node_id(field_id) {
		return none
	}
	field := tc.a.node(field_id)
	return VlsPos{int(field.pos.id), int(field.pos.offset)}
}

fn (tc &TypeChecker) vls_global_definition(name string) ?VlsPos {
	if !tc.vls_is_global(name) {
		return none
	}
	for index in tc.top_level_idx {
		node := tc.a.nodes[index]
		if node.kind != .global_decl {
			continue
		}
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.value == name {
				return VlsPos{int(field.pos.id), int(field.pos.offset)}
			}
		}
	}
	return none
}

// vls_type_definition is where the type `name` is declared: the name after
// `struct`, `interface`, `enum` or `type`.
fn (tc &TypeChecker) vls_type_definition(name string) ?VlsPos {
	short := name.all_after_last('.')
	index := tc.first_type_declaration_ids[tc.qualify_name(name)] or {
		tc.first_type_declaration_ids[name] or { return none }
	}
	decl := tc.a.nodes[index]
	file_id := int(decl.pos.id)
	start := int(decl.pos.offset)
	if decl.kind == .enum_decl {
		return VlsPos{file_id, start}
	}
	source := tc.vls_source(file_id)
	if decl.kind == .type_decl {
		// The node covers what follows `=`; the name comes before it.
		head := source[..start]
		at := head.last_index(short) or { return none }
		return VlsPos{file_id, at}
	}
	at := source.index_after(short, start) or { return none }
	return VlsPos{file_id, at}
}

fn (tc &TypeChecker) vls_selector_definition(node flat.Node) ?VlsPos {
	if node.children_count == 0 {
		return none
	}
	receiver_id := tc.a.child(&node, 0)
	receiver := tc.a.node(receiver_id)
	if enum_name := tc.vls_enum_receiver(receiver) {
		qualified := if enum_name.contains('.') { enum_name } else { tc.qualify_name(enum_name) }
		return tc.vls_enum_value_definition(Type(Enum{
			name: qualified
		}), node.value)
	}
	if receiver.kind == .ident && tc.expr_type(receiver_id) == none {
		if at := tc.vls_module_member_definition(receiver.value, node.value) {
			return at
		}
	}
	receiver_type := tc.vls_expr_type(receiver_id) or { return none }
	type_name := tc.vls_member_owner(receiver_type) or { return none }
	if at := tc.vls_field_definition(type_name, node.value) {
		return at
	}
	// A method named without a call.
	return tc.vls_function_definition('${type_name}.${node.value}')
}

// vls_module_member_definition is where `module.member` is declared: a const,
// a function or a type of that module.
fn (tc &TypeChecker) vls_module_member_definition(module_name string, member string) ?VlsPos {
	qualified := tc.vls_member_name(module_name, member)
	if at := tc.vls_const_definition(qualified) {
		return at
	}
	if at := tc.vls_function_definition(qualified) {
		return at
	}
	return tc.vls_type_definition(qualified)
}

// vls_field_definition is where the struct or interface `type_name` declares
// the field `field`.
fn (tc &TypeChecker) vls_field_definition(type_name string, field string) ?VlsPos {
	index := tc.first_type_declaration_ids[type_name] or { return none }
	decl := tc.a.nodes[index]
	for i in 0 .. decl.children_count {
		member := tc.a.child_node(&decl, i)
		if member.kind in [.field_decl, .interface_field] && member.value == field {
			return VlsPos{int(member.pos.id), int(member.pos.offset)}
		}
	}
	return none
}

fn (tc &TypeChecker) vls_enum_value_definition(typ Type, value string) ?VlsPos {
	if typ !is Enum {
		return none
	}
	name := (typ as Enum).name
	index := tc.first_type_declaration_ids[name] or { return none }
	decl := tc.a.nodes[index]
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind == .enum_field && field.value == value {
			return VlsPos{int(field.pos.id), int(field.pos.offset)}
		}
	}
	return none
}

// vls_source is the text of the file `file_id`, as the checker read it.
fn (tc &TypeChecker) vls_source(file_id int) string {
	file := tc.a.source_files[file_id] or { return '' }
	if text := tc.source_texts_by_file[file.name] {
		return text
	}
	return os.read_file(file.name) or { '' }
}
