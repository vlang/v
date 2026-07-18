module transform

import v3.flat

// transform_field_init_expr transforms transform field init expr data for transform.
fn (mut t Transformer) transform_field_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	val_id := t.a.child(&node, 0)
	val_node := t.a.nodes[int(val_id)]
	new_val := if inferred_sum := t.sum_type_for_field_variant(node.value, val_id, val_node) {
		t.wrap_sum_value(val_id, inferred_sum)
	} else {
		t.transform_expr(val_id)
	}
	start := t.a.children.len
	t.a.children << new_val
	return t.a.add_node(flat.Node{
		kind:           .field_init
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_struct_fields transforms struct initialization fields with enum resolution.
// For each .field_init child, transforms the value expression. If the struct field type
// is a known enum, resolves shorthand enum values (e.g. `.red` -> `Color.red`).
fn (mut t Transformer) transform_struct_fields(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.add_missing_struct_defaults(id, node)
	}
	info := t.lookup_struct_info(node.value) or {
		// Unknown struct: fall back to generic child transform
		return t.transform_struct_children(id, node)
	}
	// Build a field name -> type lookup from the struct definition
	mut field_types := map[string]string{}
	mut field_order := []string{cap: info.fields.len}
	for f in info.fields {
		field_types[f.name] = t.lookup_struct_field_type(node.value, f.name) or { f.typ }
		field_order << f.name
	}
	mut field_ids := []flat.NodeId{}
	mut promoted_fields := map[string][]flat.NodeId{}
	mut promoted_paths := map[string][]FieldInfo{}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			val_node := t.a.nodes[int(val_id)]
			field_name := if child.value.len > 0 {
				child.value
			} else if i < field_order.len {
				field_order[i]
			} else {
				''
			}
			mut target_field_name := field_name
			mut field_type := field_types[field_name] or { '' }
			mut promoted_key := ''
			if field_type.len == 0 {
				// A cross-module embed (`aa.Inner`) is initialized under its
				// short name: `Outer{ Inner: ... }`.
				for f in info.fields {
					if f.name.contains('.') && f.name.all_after_last('.') == field_name {
						target_field_name = f.name
						field_type = field_types[f.name] or { f.typ }
						break
					}
				}
			}
			if field_type.len == 0 {
				if path := t.struct_field_path_for_field(node.value, field_name) {
					if path.len > 0 {
						promoted_key = promoted_field_path_key(path)
						promoted_paths[promoted_key] = path
						embedded_owner := path[path.len - 1].typ
						field_type = t.lookup_struct_field_raw_type(embedded_owner, field_name) or {
							t.checker_struct_field_type_name(node.value, field_name) or { '' }
						}
					}
				}
			}
			if val_node.kind == .array_literal && t.is_fixed_array_type(field_type) {
				t.set_node_typ(int(val_id), field_type)
			}
			value_type := t.node_type(val_id)
			sum_field_type := t.struct_field_sum_type(field_type, info.module)
			enum_field_type := t.enum_type_name_for_expected(field_type, info.module)
			// Check if the value is an enum shorthand and the field type is an enum
			mut new_val := if val_node.kind == .enum_val && enum_field_type.len > 0 {
				t.transform_enum_shorthand(val_id, val_node, enum_field_type)
			} else if field_type.starts_with('[]') && t.is_fixed_array_type(value_type) {
				t.fixed_array_value_to_array(val_id, value_type, field_type)
			} else if sum_field_type.len > 0 {
				t.wrap_sum_value(val_id, sum_field_type)
			} else if inferred_sum := t.sum_type_for_field_variant(field_name, val_id, val_node) {
				t.wrap_sum_value(val_id, inferred_sum)
			} else if field_type.starts_with('&') {
				t.transform_expr_for_type(val_id, field_type)
			} else if field_type.len > 0 {
				t.transform_expr_for_type(val_id, field_type)
			} else {
				t.transform_expr(val_id)
			}
			if sum_field_type.len == 0 && field_type.len > 0 {
				new_val = t.coerce_transformed_expr_to_type(new_val, val_id, field_type)
			}
			t.drain_pending(mut prelude)
			fi_start := t.a.children.len
			t.a.children << new_val
			new_field := t.a.add_node(flat.Node{
				kind:           .field_init
				op:             child.op
				children_start: fi_start
				children_count: 1
				pos:            child.pos
				value:          target_field_name
				typ:            child.typ
			})
			if promoted_key.len > 0 {
				mut promoted := promoted_fields[promoted_key] or { []flat.NodeId{} }
				promoted << new_field
				promoted_fields[promoted_key] = promoted
			} else {
				field_ids << new_field
			}
		} else {
			field_ids << child_id
		}
	}
	for key, promoted in promoted_fields {
		path := promoted_paths[key] or { []FieldInfo{} }
		if path.len == 0 {
			continue
		}
		field_ids << t.make_promoted_struct_field_init(path, promoted)
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
	final_id := t.add_missing_struct_defaults(new_id, t.a.nodes[int(new_id)])
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	return final_id
}

fn promoted_field_path_key(path []FieldInfo) string {
	mut parts := []string{cap: path.len}
	for field in path {
		parts << field.name
	}
	return parts.join('\n')
}

fn (mut t Transformer) make_promoted_struct_field_init(path []FieldInfo, leaf_fields []flat.NodeId) flat.NodeId {
	mut init_start := t.a.children.len
	for fid in leaf_fields {
		t.a.children << fid
	}
	mut cur_type := path[path.len - 1].typ
	mut cur_init := t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: init_start
		children_count: flat.child_count(leaf_fields.len)
		value:          cur_type
		typ:            cur_type
	})
	for rev in 0 .. path.len - 1 {
		idx := path.len - 2 - rev
		parent := path[idx]
		child := path[idx + 1]
		field_start := t.a.children.len
		t.a.children << cur_init
		field := t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: field_start
			children_count: 1
			value:          child.name
			typ:            child.typ
		})
		init_start = t.a.children.len
		t.a.children << field
		cur_type = parent.typ
		cur_init = t.a.add_node(flat.Node{
			kind:           .struct_init
			children_start: init_start
			children_count: 1
			value:          cur_type
			typ:            cur_type
		})
	}
	root := path[0]
	field_start := t.a.children.len
	t.a.children << cur_init
	return t.a.add_node(flat.Node{
		kind:           .field_init
		children_start: field_start
		children_count: 1
		value:          root.name
		typ:            root.typ
	})
}

// transform_struct_children is a fallback for struct inits where the struct type is unknown.
// Transforms all field_init value expressions without enum resolution.
fn (mut t Transformer) transform_struct_children(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut field_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			new_val := t.transform_expr(val_id)
			fi_start := t.a.children.len
			t.a.children << new_val
			field_ids << t.a.add_node(flat.Node{
				kind:           .field_init
				op:             child.op
				children_start: fi_start
				children_count: 1
				pos:            child.pos
				value:          child.value
				typ:            child.typ
			})
		} else {
			field_ids << child_id
		}
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
}

fn (mut t Transformer) infer_bare_generic_struct_init_type(node flat.Node) ?string {
	if node.value.len == 0 || node.value.contains('[') {
		return none
	}
	params := t.generic_struct_param_names_for_base(node.value)
	if params.len == 0 {
		return none
	}
	info := t.lookup_struct_info(node.value) or { return none }
	mut inferred := map[string]string{}
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		mut field_type := ''
		for struct_field in info.fields {
			if struct_field.name == field_name {
				field_type = struct_field.typ
				break
			}
		}
		if field_type.len == 0 {
			continue
		}
		value_type := t.generic_struct_init_value_type(t.a.child(field, 0))
		if value_type.len > 0 {
			infer_generic_type_args(field_type, value_type, mut inferred)
		}
	}
	mut args := []string{cap: params.len}
	for param in params {
		arg := inferred[param] or { return none }
		if arg.len == 0 || t.generic_arg_is_unresolved(arg) {
			return none
		}
		args << arg
	}
	return '${node.value}[${args.join(', ')}]'
}

fn (mut t Transformer) generic_struct_init_value_type(id flat.NodeId) string {
	mut typ := t.generic_call_arg_type_for_inference(id)
	if typ.len == 0 {
		typ = t.node_type(id)
	}
	return match typ {
		'int literal' { 'int' }
		'float literal' { 'f64' }
		else { typ }
	}
}

// add_missing_struct_defaults checks if any fields with default values are missing
// from the struct initialization. This is a hook point for future default-fill logic.
// Currently returns the node unchanged because StructInfo does not yet store default values.
fn (mut t Transformer) add_missing_struct_defaults(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len == 0 {
		return id
	}
	info := t.lookup_struct_info(node.value) or { return id }
	mut provided := map[string]bool{}
	mut field_ids := []flat.NodeId{cap: int(node.children_count) + info.fields.len}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init {
			provided[child.value] = true
		}
		field_ids << child_id
	}
	old_module := t.cur_module
	// Imported defaults must retain their declaration module while resolving consts, globals,
	// and function names. Leave them absent here; cgen's struct-default path emits them with the
	// declaring module/file active. Defaults from the current module still need transform-time
	// lowering for the non-C backends.
	if info.module.len > 0 && info.module !in ['main', 'builtin'] && info.module != old_module {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		return id
	}
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	mut added := false
	for field in info.fields {
		if field.name in provided || int(field.default_expr) < 0 {
			continue
		}
		field_type := t.lookup_struct_field_type(node.value, field.name) or { field.typ }
		default_node := t.a.nodes[int(field.default_expr)]
		enum_field_type := t.enum_type_name_for_expected(field_type, info.module)
		new_val := if default_node.kind == .enum_val && enum_field_type.len > 0 {
			t.transform_enum_shorthand(field.default_expr, default_node, enum_field_type)
		} else if t.is_sum_type_name(field_type) {
			// A sum-type field default (e.g. `typ_expr Expr = EmptyExpr{}`) must be
			// wrapped into the sum, not emitted as the bare variant. wrap_sum_value
			// is a no-op when the value already is the sum type.
			t.wrap_sum_value(field.default_expr, field_type)
		} else {
			t.transform_expr_for_type(field.default_expr, field_type)
		}
		t.drain_pending(mut prelude)
		fi_start := t.a.children.len
		t.a.children << new_val
		field_ids << t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: fi_start
			children_count: 1
			value:          field.name
			typ:            field_type
		})
		provided[field.name] = true
		added = true
	}
	t.cur_module = old_module
	if !added {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		// The node passes through untouched, but downstream consumers (e.g.
		// the sum-wrap decision for `return SNull{}`) need its type text.
		if node.typ.len == 0 && node.value.len > 0 {
			t.set_node_typ(int(id), node.value)
		}
		return id
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	final_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	return final_id
}

// lookup_struct_info resolves lookup struct info information for transform.
fn (t &Transformer) lookup_struct_info(name string) ?StructInfo {
	base, args, has_generic_args := generic_app_parts(name)
	if has_generic_args {
		if base_info := t.lookup_struct_info_direct(base) {
			params := t.generic_struct_param_names_for_base(base)
			if params.len == args.len && params.len > 0 {
				mut fields := []FieldInfo{cap: base_info.fields.len}
				for field in base_info.fields {
					fields << FieldInfo{
						name:         field.name
						typ:          substitute_generic_type_text_with_params(field.typ, args,
							params)
						raw_typ:      substitute_generic_type_text_with_params(field.raw_typ, args,
							params)
						default_expr: field.default_expr
						is_embedded:  field.is_embedded
					}
				}
				return StructInfo{
					name:      name
					module:    base_info.module
					is_params: base_info.is_params
					fields:    fields
				}
			}
		}
	}
	if info := t.lookup_struct_info_preferred(name) {
		return info
	}
	// Resolve a qualified import alias (`m.Config` -> `some.mod.Config`) before the
	// short-name fallback below, so a same-named local type cannot shadow the imported
	// one. `resolve_imported_type_name` only matches dotted alias names, so bare names
	// fall through to the direct lookup unchanged.
	if imported := t.resolve_imported_type_name(name) {
		if info := t.lookup_struct_info_direct(imported) {
			return info
		}
	}
	// Resolve a module-local alias before the bare short-name fallback. A local
	// `Context = C.sgl_context` must not inherit defaults from an imported
	// `gg.Context` that happens to own the global short-name entry.
	normalized := t.normalize_type_alias(name)
	if normalized != name {
		if info := t.lookup_struct_info_direct(normalized) {
			return info
		}
	}
	// A monomorphized generic instantiated from another module references the concrete
	// type by its bare name (`BoolConfig` from main, while transforming the declaring
	// module); the module-qualified lookup above misses those, so fall back to the
	// direct lookup, which tries the bare name too.
	if info := t.lookup_struct_info_direct(name) {
		return info
	}
	// Mark-used runs before transform and can prune a struct declaration whose value is
	// only observed through an interface box. The checker field table deliberately
	// survives that pruning, so use it as the authoritative fallback for structural
	// operations such as semantic equality.
	if info := t.lookup_checker_struct_info(name) {
		return info
	}
	return none
}

fn (t &Transformer) bare_struct_name_is_local_to_current_module(name string) bool {
	if name.len == 0 || name.contains('.') {
		return false
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qname := '${t.cur_module}.${name}'
		return qname in t.structs || (!isnil(t.tc) && qname in t.tc.structs)
	}
	if info := t.structs[name] {
		return info.module.len == 0 || info.module in ['main', 'builtin']
	}
	if isnil(t.tc) || name !in t.tc.structs {
		return false
	}
	module_name := t.tc.struct_modules[name] or { '' }
	return module_name.len == 0 || module_name in ['main', 'builtin']
}

fn (t &Transformer) checker_struct_lookup_name(name string) string {
	if isnil(t.tc) || name.len == 0 {
		return ''
	}
	if name in t.tc.structs {
		return name
	}
	if name.contains('.') {
		short_name := name.all_after_last('.')
		module_name := name.all_before_last('.')
		short_module := t.tc.struct_modules[short_name] or { '' }
		if short_name in t.tc.structs && short_module == module_name {
			return short_name
		}
		return ''
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qualified_name := '${t.cur_module}.${name}'
		if qualified_name in t.tc.structs {
			return qualified_name
		}
	}
	return ''
}

fn (t &Transformer) lookup_checker_struct_info(name string) ?StructInfo {
	lookup_name := t.checker_struct_lookup_name(name)
	if lookup_name.len == 0 {
		return none
	}
	checker_fields := t.tc.structs[lookup_name] or { return none }
	mut fields := []FieldInfo{cap: checker_fields.len}
	for field in checker_fields {
		field_type := field.typ.name()
		fields << FieldInfo{
			name:        field.name
			typ:         field_type
			raw_typ:     field_type
			is_embedded: field.name == field_type
		}
	}
	return StructInfo{
		name:   lookup_name.all_after_last('.')
		module: t.tc.struct_modules[lookup_name] or { '' }
		fields: fields
	}
}

fn (t &Transformer) resolve_imported_type_name(name string) ?string {
	if isnil(t.tc) || !name.contains('.') || name.starts_with('C.') {
		return none
	}
	dot := name.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	alias := name[..dot]
	if mod := t.tc.file_imports[file_import_key(t.cur_file, alias)] {
		if mod != alias {
			return mod + name[dot..]
		}
	}
	if mod := t.tc.imports[alias] {
		if mod != alias {
			return mod + name[dot..]
		}
	}
	return none
}

fn (t &Transformer) lookup_struct_info_preferred(name string) ?StructInfo {
	if name.contains('.') {
		if name in t.structs {
			return t.structs[name]
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.structs {
			return t.structs[qname]
		}
	} else if name in t.structs {
		return t.structs[name]
	}
	return none
}

fn (t &Transformer) lookup_struct_info_direct(name string) ?StructInfo {
	if name.contains('.') {
		if name in t.structs {
			return t.structs[name]
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.structs {
			return t.structs[qname]
		}
	}
	if name in t.structs {
		return t.structs[name]
	}
	if name.contains('.') {
		short_name := name.all_after_last('.')
		if short_name in t.structs {
			return t.structs[short_name]
		}
	} else if t.struct_short_name_index_ready {
		if qualified := t.struct_short_name_index[name] {
			if qualified != struct_short_name_ambiguous {
				return t.structs[qualified]
			}
		}
	} else {
		mut matches := []StructInfo{}
		for qualified, info in t.structs {
			if qualified.contains('.') && qualified.all_after_last('.') == name {
				matches << info
			}
		}
		if matches.len == 1 {
			return matches[0]
		}
	}
	return none
}

// struct_field_type supports struct field type handling for Transformer.
fn (t &Transformer) struct_field_type(info StructInfo, field_name string) ?string {
	for field in info.fields {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// embedded_field_for_promoted_field
// supports helper handling in transform.
fn (t &Transformer) embedded_field_for_promoted_field(info StructInfo, field_name string) ?FieldInfo {
	for field in info.fields {
		if !t.is_embedded_field(field) {
			continue
		}
		embedded_info := t.lookup_struct_info(field.typ) or { continue }
		if _ := t.struct_field_type(embedded_info, field_name) {
			return field
		}
	}
	return none
}

fn (t &Transformer) embedded_field_for_direct_selector(info StructInfo, field_name string) ?FieldInfo {
	for field in info.fields {
		if !t.is_embedded_field(field) {
			continue
		}
		if embedded_selector_matches(field_name, field.name)
			|| embedded_selector_matches(field_name, field.typ)
			|| embedded_selector_matches(field_name, field.raw_typ) {
			return field
		}
	}
	return none
}

fn embedded_selector_matches(field_name string, embedded_name string) bool {
	clean := embedded_name.trim_space().trim_left('&')
	if clean.len == 0 {
		return false
	}
	if clean == field_name || clean.all_after_last('.') == field_name {
		return true
	}
	base, _, is_generic := generic_app_parts(clean)
	return is_generic && (base == field_name || base.all_after_last('.') == field_name)
}

// is_embedded_field reports whether is embedded field applies in transform.
fn (t &Transformer) is_embedded_field(field FieldInfo) bool {
	return field.is_embedded
}

fn field_decl_is_embedded(name string, typ string) bool {
	if name.len == 0 || typ.len == 0 {
		return false
	}
	short_typ := if typ.contains('.') { typ.all_after_last('.') } else { typ }
	short_name := if name.contains('.') { name.all_after_last('.') } else { name }
	return short_name == short_typ
}

// transform_assoc_expr transforms transform assoc expr data for transform.
fn (mut t Transformer) transform_assoc_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .assoc || node.children_count == 0 {
		return id
	}
	base_id := t.a.child(&node, 0)
	base_node := t.a.nodes[int(base_id)]
	mut base_type := ''
	if base_node.kind == .ident {
		base_type = t.raw_var_type(base_node.value)
		if base_type.len == 0 {
			base_type = t.var_type(base_node.value)
		}
	}
	if base_type.len == 0 {
		base_type = t.node_type(base_id)
	}
	mut assoc_type := node.value
	if assoc_type.len == 0 {
		assoc_type = base_type
	}
	if assoc_type.starts_with('&') {
		assoc_type = assoc_type[1..]
	}
	if _ := t.lookup_struct_info(assoc_type) {
		// use the explicit assoc type
	} else {
		mut clean_base_type := base_type
		if clean_base_type.starts_with('&') {
			clean_base_type = clean_base_type[1..]
		}
		if _ := t.lookup_struct_info(clean_base_type) {
			assoc_type = clean_base_type
		}
	}
	mut field_types := map[string]string{}
	mut assoc_module := ''
	if info := t.lookup_struct_info(assoc_type) {
		assoc_module = info.module
		for field in info.fields {
			field_types[field.name] = t.lookup_struct_field_type(assoc_type, field.name) or {
				field.typ
			}
		}
	}

	tmp_name := t.new_temp('assoc')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	transformed_base := t.transform_expr(base_id)
	base := if base_type.starts_with('&') {
		t.make_prefix(.mul, transformed_base)
	} else {
		transformed_base
	}
	t.drain_pending(mut prelude)
	init_base := t.assoc_mapped_base_init(base, base_type, assoc_type, mut prelude) or { base }
	prelude << t.make_decl_assign_typed(tmp_name, init_base, assoc_type)

	for i in 1 .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := t.a.child(&field, 0)
		value_node := t.a.nodes[int(value_id)]
		field_type := field_types[field.value] or { '' }
		if value_node.kind == .array_literal && t.is_fixed_array_type(field_type) {
			t.set_node_typ(int(value_id), field_type)
		}
		value_type := t.node_type(value_id)
		enum_field_type := t.enum_type_name_for_expected(field_type, assoc_module)
		sum_field_type := t.struct_field_sum_type(field_type, assoc_module)
		value := if value_node.kind == .enum_val && enum_field_type.len > 0 {
			t.transform_enum_shorthand(value_id, value_node, enum_field_type)
		} else if field_type.starts_with('[]') && t.is_fixed_array_type(value_type) {
			t.fixed_array_value_to_array(value_id, value_type, field_type)
		} else if sum_field_type.len > 0 {
			t.wrap_sum_value(value_id, sum_field_type)
		} else if field_type.len > 0 {
			t.transform_expr_for_type(value_id, field_type)
		} else {
			t.transform_expr(value_id)
		}
		t.drain_pending(mut prelude)
		prelude << t.make_assign(t.make_selector(t.make_ident(tmp_name), field.value, field_type),
			value)
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), assoc_type)
	return result
}

fn (mut t Transformer) assoc_mapped_base_init(base flat.NodeId, base_type string, assoc_type string, mut prelude []flat.NodeId) ?flat.NodeId {
	source_type := t.trim_pointer_type(base_type)
	if source_type.len == 0 || assoc_type.len == 0
		|| t.normalize_type_alias(source_type) == t.normalize_type_alias(assoc_type) {
		return none
	}
	target_info := t.lookup_struct_info(assoc_type) or { return none }
	source_info := t.lookup_struct_info(source_type) or { return none }
	if target_info.name == source_info.name {
		return none
	}
	mut source_fields := map[string]FieldInfo{}
	for field in source_info.fields {
		source_fields[field.name] = field
	}
	source_name := t.new_temp('assoc_src')
	prelude << t.make_decl_assign_typed(source_name, base, source_type)
	mut field_ids := []flat.NodeId{}
	for field in target_info.fields {
		source_field := source_fields[field.name] or { continue }
		target_type := t.lookup_struct_field_type(assoc_type, field.name) or { field.typ }
		source_field_type := t.lookup_struct_field_type(source_type, source_field.name) or {
			source_field.typ
		}
		if t.normalize_type_alias(target_type) != t.normalize_type_alias(source_field_type) {
			continue
		}
		value := t.make_selector(t.make_ident(source_name), source_field.name, source_field_type)
		field_ids << t.make_named_field_init(field.name, value, target_type)
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value:          assoc_type
		typ:            assoc_type
	})
}

// transform_amp_assoc_expr_for_type supports transform_amp_assoc_expr_for_type handling.
fn (mut t Transformer) transform_amp_assoc_expr_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind != .assoc {
		return none
	}
	value := t.transform_assoc_expr(child_id, child)
	mut value_type := t.node_type(value)
	if value_type.len == 0 {
		value_type = t.node_type(child_id)
	}
	if value_type.starts_with('&') {
		value_type = value_type[1..]
	}
	if value_type.len == 0 {
		return none
	}
	addr := t.make_prefix(.amp, value)
	dup := t.make_memdup_call_for_type(addr, value_type)
	mut ptr_type := target_type
	if ptr_type.len == 0 {
		ptr_type = node.typ
	}
	if !ptr_type.starts_with('&') {
		ptr_type = '&${value_type}'
	}
	cast := t.make_cast(ptr_type, dup, ptr_type)
	t.set_node_typ(int(cast), ptr_type)
	return cast
}

// struct_field_sum_type supports struct field sum type handling for Transformer.
fn (t &Transformer) struct_field_sum_type(field_type string, owner_module string) string {
	if field_type.contains('|') {
		if sum_name := t.sum_type_for_union_text(field_type, owner_module) {
			return sum_name
		}
	}
	if field_type.starts_with('[]') || field_type.starts_with('map[')
		|| t.is_fixed_array_type(field_type) {
		return ''
	}
	if t.is_sum_type_name(field_type) {
		return field_type
	}
	if field_type.len == 0 || field_type.contains('.') || owner_module.len == 0 {
		return ''
	}
	qname := '${owner_module}.${field_type}'
	if qname in t.sum_types {
		return qname
	}
	return ''
}

fn (t &Transformer) sum_type_for_union_text(field_type string, owner_module string) ?string {
	raw_variants := split_sum_union_text(field_type)
	if raw_variants.len < 2 {
		return none
	}
	mut variants := []string{cap: raw_variants.len}
	for raw in raw_variants {
		variants << t.normalize_sum_variant_type(raw, owner_module, [])
	}
	for sum_name, sum_variants in t.sum_types {
		if owner_module.len > 0 && sum_name.contains('.')
			&& sum_name.all_before_last('.') != owner_module {
			continue
		}
		if sum_variants.len != variants.len {
			continue
		}
		mut matched := true
		for variant in variants {
			mut found := false
			for sum_variant in sum_variants {
				if t.union_variant_text_matches(sum_variant, variant) {
					found = true
					break
				}
			}
			if !found {
				matched = false
				break
			}
		}
		if matched {
			return sum_name
		}
	}
	return none
}

fn (t &Transformer) union_variant_text_matches(a string, b string) bool {
	if t.variant_names_match(a, b) {
		return true
	}
	if t.is_fixed_array_type(a) && t.is_fixed_array_type(b) {
		return t.resolved_fixed_array_canonical_type(a) == t.resolved_fixed_array_canonical_type(b)
	}
	return false
}

fn split_sum_union_text(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i in 0 .. s.len {
		match s[i] {
			`[`, `(`, `{` {
				depth++
			}
			`]`, `)`, `}` {
				if depth > 0 {
					depth--
				}
			}
			`|` {
				if depth == 0 {
					part := s[start..i].trim_space()
					if part.len > 0 {
						parts << part
					}
					start = i + 1
				}
			}
			else {}
		}
	}
	part := s[start..].trim_space()
	if part.len > 0 {
		parts << part
	}
	return parts
}

// sum_type_for_field_variant supports sum type for field variant handling for Transformer.
fn (t &Transformer) sum_type_for_field_variant(field_name string, val_id flat.NodeId, val_node flat.Node) ?string {
	if field_name != 'info' {
		return none
	}
	mut variant := if val_node.kind in [.struct_init, .cast_expr, .assoc] && val_node.value.len > 0 {
		val_node.value
	} else if val_node.kind == .assoc && val_node.children_count > 0 {
		t.node_type(t.a.child(&val_node, 0))
	} else {
		t.node_type(val_id)
	}
	if variant.starts_with('&') {
		variant = variant[1..]
	}
	if variant.len == 0 {
		return none
	}
	short_variant := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	for sum_name, variants in t.sum_types {
		if sum_name != 'TypeInfo' && !sum_name.ends_with('.TypeInfo') {
			continue
		}
		for v in variants {
			short_v := if v.contains('.') { v.all_after_last('.') } else { v }
			if v == variant || short_v == short_variant {
				return sum_name
			}
		}
	}
	return none
}

// fixed_array_value_to_array converts fixed array value to array data for transform.
fn (mut t Transformer) fixed_array_value_to_array(value_id flat.NodeId, fixed_type string, array_type string) flat.NodeId {
	return t.fixed_array_data_to_array(t.transform_expr(value_id), fixed_type, array_type)
}

fn (mut t Transformer) fixed_array_data_to_array(data_id flat.NodeId, fixed_type string, array_type string) flat.NodeId {
	elem_type := fixed_array_elem_type(fixed_type)
	len_expr := t.make_fixed_array_len_expr(fixed_type)
	return t.make_call_typed('new_array_from_c_array', [
		len_expr,
		len_expr,
		t.make_sizeof_type(elem_type),
		data_id,
	], array_type)
}

// transform_array_init_expr transforms .array_init nodes (e.g. `[]int{len: n}`).
// Recursively transforms any child expressions (len, cap, init values).
fn (mut t Transformer) transform_array_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if fixed := t.transform_fixed_array_init_expr(node) {
		return fixed
	}
	lowered := t.lower_array_init_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .array_init
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_map_init_expr transforms .map_init nodes.
// Recursively transforms all child key/value expressions.
fn (mut t Transformer) transform_map_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	raw_type := if node.value.len > 0 {
		node.value
	} else if node.typ.len > 0 {
		node.typ
	} else {
		t.node_type(id)
	}
	map_type := t.normalize_type_alias(raw_type)
	if map_type.starts_with('map[') {
		mut map_node := node
		map_node.value = map_type
		map_node.typ = map_type
		return t.lower_map_init_to_runtime(id, map_node)
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .map_init
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}
