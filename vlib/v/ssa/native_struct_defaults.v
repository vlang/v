module ssa

import v.flat

struct NativeFieldDefault {
	module_name string
	expr        flat.NodeId
}

fn (mut b Builder) register_struct_field_defaults() {
	mut module_name := 'main'
	for node in b.a.nodes {
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .struct_decl {
			continue
		}
		typ := b.struct_type_id_for_decl(node.value, module_name)
		b.default_struct_types[typ] = true
		for index in 0 .. node.children_count {
			field := b.a.child_node(&node, index)
			if field.kind == .field_decl && field.children_count > 0 {
				b.struct_field_defaults['${typ}.${field.value}'] = NativeFieldDefault{
					module_name: module_name
					expr:        b.a.child(field, 0)
				}
			}
		}
	}
}

fn (mut b Builder) declared_field_default_value(struct_name string, field_name string) ?ValueID {
	typ, _ := b.struct_literal_type(struct_name)
	initializer := b.struct_field_defaults['${typ}.${field_name}'] or { return none }
	old_module := b.cur_module
	old_vars := b.vars
	old_types := b.var_type_names
	b.cur_module = initializer.module_name
	// Defaults belong to the declaration's scope, regardless of the caller's locals.
	b.vars = b.global_vars.clone()
	b.var_type_names = b.global_type_names.clone()
	prefix := b.cur_module + '.'
	for name, address in b.global_vars {
		if name.starts_with(prefix) {
			short := name[prefix.len..]
			if !short.contains('.') {
				b.vars[short] = address
				b.var_type_names[short] = b.global_type_names[name]
			}
		}
	}
	defer {
		b.cur_module = old_module
		b.vars = old_vars
		b.var_type_names = old_types
	}
	field_type_name := b.field_type_name(struct_name, field_name)
	field_type := b.resolve_type(field_type_name)
	mut value := b.build_field_value(initializer.expr, field_type_name)
	if b.is_option_type(field_type) && b.value_type(value) != field_type {
		is_some := b.a.nodes[int(initializer.expr)].kind != .none_expr
		if is_some {
			value = b.coerce_store_value(value, b.option_value_type(field_type))
		}
		return b.build_option_value(field_type, is_some, value)
	}
	if b.is_int_type(field_type) && b.is_int_type(b.value_type(value)) {
		value = b.coerce_int_value(value, field_type)
	}
	return b.coerce_store_value(value, field_type)
}

fn (b &Builder) struct_field_storage_type(struct_name string, field_name string) ?TypeID {
	typ, _ := b.struct_literal_type(struct_name)
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return none
	}
	structure := b.m.type_store.types[typ]
	index := structure.field_names.index(field_name)
	if index < 0 || index >= structure.fields.len {
		return none
	}
	return structure.fields[index]
}

fn (mut b Builder) nested_struct_default_value(type_name string, storage_type TypeID) ?ValueID {
	if type_name in ['string', 'array', 'map'] || type_name.starts_with('&') || type_name.starts_with('[') || type_name.starts_with('?')
		|| type_name.starts_with('!') || type_name.starts_with('map[') {
		return none
	}
	if storage_type > 0 && storage_type < b.m.type_store.types.len
		&& b.m.type_store.types[storage_type].kind == .struct_t
		&& b.default_struct_types[storage_type]
		&& b.resolve_type(type_name) == storage_type {
		return b.build_struct_init(flat.Node{
			kind:  .struct_init
			value: type_name
			typ:   type_name
		})
	}
	return none
}
