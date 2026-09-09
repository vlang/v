// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast
import v.token

fn (mut c Checker) interface_decl(mut node ast.InterfaceDecl) {
	c.check_valid_pascal_case(node.name, 'interface name', node.pos)
	mut decl_sym := c.table.sym(node.typ)
	is_js := node.language == .js
	if mut decl_sym.info is ast.Interface {
		mut has_generic_types := false
		if node.embeds.len > 0 {
			all_embeds := c.expand_iface_embeds(node, 0, node.embeds)
			// eprintln('> node.name: ${node.name} | node.embeds.len: ${node.embeds.len} | all_embeds: ${all_embeds.len}')
			node.embeds = all_embeds
			mut emnames := map[string]int{}
			mut emnames_ds := map[string]bool{}
			mut emnames_ds_info := map[string]bool{}
			mut efnames := map[string]int{}
			mut efnames_ds_info := map[string]bool{}
			for i, m in node.methods {
				emnames[m.name] = i
				emnames_ds[m.name] = true
				emnames_ds_info[m.name] = true
			}
			for i, f in node.fields {
				efnames[f.name] = i
				efnames_ds_info[f.name] = true
			}
			for embed in all_embeds {
				isym := c.table.sym(embed.typ)
				if embed.typ.has_flag(.generic) {
					has_generic_types = true
				}
				if isym.kind != .interface {
					c.error('interface `${node.name}` tries to embed `${isym.name}`, but `${isym.name}` is not an interface, but `${isym.kind}`',
						embed.pos)
					continue
				}
				// Ensure each generic type of the embed was declared in the interface's definition
				if node.generic_types.len > 0 && embed.typ.has_flag(.generic) {
					embed_generic_names := c.table.generic_type_names(embed.typ)
					node_generic_names := node.generic_types.map(c.table.type_to_str(it))
					for name in embed_generic_names {
						if name !in node_generic_names {
							interface_generic_names := node_generic_names.join(', ')
							c.error('generic type name `${name}` is not mentioned in interface `${node.name}<${interface_generic_names}>`',
								embed.pos)
						}
					}
				}
				isym_info := isym.info as ast.Interface
				for f in isym_info.fields {
					if !efnames_ds_info[f.name] {
						efnames_ds_info[f.name] = true
						decl_sym.info.fields << f
					}
				}
				for m in isym_info.methods {
					if !emnames_ds_info[m.name] {
						emnames_ds_info[m.name] = true
						decl_sym.info.methods << m.new_method_with_receiver_type(node.typ)
					}
				}
				for m in isym.methods {
					if !emnames_ds[m.name] {
						emnames_ds[m.name] = true
						decl_sym.methods << m.new_method_with_receiver_type(node.typ)
					}
				}
				if embed_decl := c.table.interfaces[embed.typ] {
					for f in embed_decl.fields {
						if f.name in efnames {
							// already existing field name, check for conflicts
							ifield := node.fields[efnames[f.name]]
							if field := c.table.find_field_with_embeds(isym, f.name) {
								if ifield.typ != field.typ {
									exp := c.table.type_to_str(ifield.typ)
									got := c.table.type_to_str(field.typ)
									c.error('embedded interface `${embed_decl.name}` conflicts existing field: `${ifield.name}`, expecting type: `${exp}`, got type: `${got}`',
										ifield.pos)
								}
							}
						} else {
							efnames[f.name] = node.fields.len
							node.fields << f
						}
					}
					for m in embed_decl.methods {
						if m.name in emnames {
							// already existing method name, check for conflicts
							imethod := node.methods[emnames[m.name]]
							if em_fn := decl_sym.find_method(imethod.name) {
								if m_fn := isym.find_method(m.name) {
									msg := c.table.is_same_method(m_fn, em_fn)
									if msg.len > 0 {
										em_sig := c.table.fn_signature(em_fn, skip_receiver: true)
										m_sig := c.table.fn_signature(m_fn, skip_receiver: true)
										c.error('embedded interface `${embed_decl.name}` causes conflict: ${msg}, for interface method `${em_sig}` vs `${m_sig}`',
											imethod.pos)
									}
								}
							}
						} else {
							emnames[m.name] = node.methods.len
							mut new_method := m.new_method_with_receiver_type(node.typ)
							new_method.pos = embed.pos
							node.methods << new_method
						}
					}
				}
			}
		}
		for i, method in node.methods {
			if node.language == .v {
				c.check_valid_snake_case(method.name, 'method name', method.pos)
			}
			if !c.ensure_type_exists(method.return_type, method.return_type_pos) {
				continue
			}
			if is_js {
				mrsym := c.table.sym(method.return_type)
				if !mrsym.is_js_compatible() {
					c.error('method ${method.name} returns non JS type', method.pos)
				}
			}
			if method.return_type.has_flag(.generic) {
				has_generic_types = true
				// Ensure each generic type of the method was declared in the interface's definition
				if node.generic_types.len > 0 {
					method_generic_names := c.table.generic_type_names(method.return_type)
					node_generic_names := node.generic_types.map(c.table.type_to_str(it))
					for name in method_generic_names {
						if name !in node_generic_names {
							interface_generic_names := node_generic_names.join(', ')
							c.error('generic type name `${name}` is not mentioned in interface `${node.name}<${interface_generic_names}>`',
								method.return_type_pos)
						}
					}
				}
			} else if !method.return_type.has_option_or_result() {
				ret_sym := c.table.sym(method.return_type)
				if ret_sym.info is ast.ArrayFixed && !ret_sym.info.is_fn_ret {
					c.cast_to_fixed_array_ret(method.return_type, ret_sym)
				} else if ret_sym.info is ast.Alias {
					parent_sym := c.table.sym(ret_sym.info.parent_type)
					if parent_sym.info is ast.ArrayFixed && !parent_sym.info.is_fn_ret {
						c.cast_to_fixed_array_ret(ret_sym.info.parent_type, parent_sym)
					}
				}
			}
			for j, param in method.params {
				if j == 0 && is_js {
					continue // no need to check first param
				}
				if param.typ.has_flag(.generic) {
					has_generic_types = true
				}
				if !c.ensure_type_exists(param.typ, param.pos) {
					continue
				}
				if reserved_type_names_chk.matches(param.name) {
					c.error('invalid use of reserved type `${param.name}` as a parameter name',
						param.pos)
				}
				// Ensure each generic type of the method was declared in the interface's definition
				if node.generic_types.len > 0 && param.typ.has_flag(.generic) {
					method_generic_names := c.table.generic_type_names(param.typ)
					node_generic_names := node.generic_types.map(c.table.type_to_str(it))
					for name in method_generic_names {
						if name !in node_generic_names {
							interface_generic_names := node_generic_names.join(', ')
							c.error('generic type name `${name}` is not mentioned in interface `${node.name}<${interface_generic_names}>`',
								param.type_pos)
						}
					}
				}
				if is_js {
					psym := c.table.sym(param.typ)
					if !psym.is_js_compatible() && !(j == method.params.len - 1
						&& method.is_variadic) {
						c.error('method `${method.name}` accepts non JS type as parameter',
							method.pos)
					}
				}
			}
			for field in node.fields {
				field_sym := c.table.sym(field.typ)
				if field.name == method.name && field_sym.kind == .function {
					c.error('type `${decl_sym.name}` has both field and method named `${method.name}`',
						method.pos)
				}
			}
			for j in 0 .. i {
				if method.name == node.methods[j].name {
					c.error('duplicate method name `${method.name}`', method.pos)
				}
			}
		}
		for i, field in node.fields {
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			if !c.ensure_type_exists(field.typ, field.pos) {
				continue
			}
			if field.typ.has_flag(.generic) {
				has_generic_types = true
			}
			if is_js {
				tsym := c.table.sym(field.typ)
				if !tsym.is_js_compatible() {
					c.error('field `${field.name}` uses non JS type', field.pos)
				}
			}
			if field.typ == node.typ && node.language != .js {
				c.error('recursive interface fields are not allowed because they cannot be initialised',
					field.type_pos)
			}
			for j in 0 .. i {
				if field.name == node.fields[j].name {
					c.error('field name `${field.name}` duplicate', field.pos)
				}
			}
		}
		if node.generic_types.len == 0 && has_generic_types {
			c.error('generic interface `${node.name}` declaration must specify the generic type names, e.g. ${node.name}[T]',
				node.pos)
		}
	}
}

fn (mut c Checker) unwrap_generic_interface(typ ast.Type, interface_type ast.Type, pos token.Pos) ast.Type {
	utyp := c.unwrap_generic(typ)
	resolved_interface_type := c.unwrap_generic(interface_type)
	if resolved_interface_type != interface_type && !resolved_interface_type.has_flag(.generic) {
		mut resolved_sym := c.table.sym(resolved_interface_type)
		if mut resolved_sym.info is ast.Interface {
			if resolved_sym.info.concrete_types.len == 0
				&& resolved_sym.generic_types.len == resolved_sym.info.generic_types.len {
				resolved_sym.info.concrete_types = resolved_sym.generic_types.clone()
			}
		}
		return resolved_interface_type
	}
	typ_sym := c.table.sym(utyp)
	mut inter_sym := c.table.sym(interface_type)

	if mut inter_sym.info is ast.Interface {
		if inter_sym.info.is_generic {
			mut inferred_types := []ast.Type{}
			generic_names := inter_sym.info.generic_types.map(c.table.get_type_name(it))
			mut interface_concrete_types := inter_sym.generic_types.clone()
			if interface_concrete_types.len == 0
				&& inter_sym.info.generic_types.len == inter_sym.info.concrete_types.len {
				interface_concrete_types = inter_sym.info.concrete_types.clone()
			}
			// inferring interface generic types
			for gt_name in generic_names {
				mut inferred_type := ast.void_type
				for ifield in inter_sym.info.fields {
					if field := c.table.find_field_with_embeds(typ_sym, ifield.name) {
						mut ifield_typ := ifield.typ
						if interface_concrete_types.len == generic_names.len {
							if resolved_field_type := c.table.convert_generic_type(ifield.typ,
								generic_names, interface_concrete_types)
							{
								ifield_typ = resolved_field_type
							}
						}
						inferred_field_type := c.infer_composite_generic_type(gt_name, ifield_typ,
							field.typ)
						if inferred_field_type != ast.void_type {
							inferred_type = inferred_field_type
						}
					}
				}
				for imethod in inter_sym.info.methods {
					mut method := typ_sym.find_method_with_generic_parent(imethod.name) or {
						if pos.file_idx != -1 {
							c.error('can not find method `${imethod.name}` on `${typ_sym.name}`, needed for interface: `${inter_sym.name}`', pos)
						}
						return 0
					}
					method = c.resolve_method_for_concrete_type(method, typ_sym)
					mut imethod_return_type := imethod.return_type
					if interface_concrete_types.len == generic_names.len {
						if resolved_return_type := c.table.convert_generic_type(imethod.return_type,
							generic_names, interface_concrete_types)
						{
							imethod_return_type = resolved_return_type
						}
					}
					if imethod_return_type.has_flag(.generic)
						|| c.type_has_unresolved_generic_parts(imethod_return_type) {
						imret_sym := c.table.sym(imethod_return_type)
						mret_sym := c.table.sym(method.return_type)
						if method.return_type == ast.void_type
							&& imethod_return_type != method.return_type {
							if pos.file_idx != -1 {
								c.error('interface method `${imethod.name}` returns `${imret_sym.name}`, but implementation method `${method.name}` returns no value',
									pos)
							}
							return 0
						}
						if imethod_return_type == ast.void_type
							&& imethod_return_type != method.return_type {
							if pos.file_idx != -1 {
								c.error('interface method `${imethod.name}` returns no value, but implementation method `${method.name}` returns `${mret_sym.name}`',
									pos)
							}
							return 0
						}
						inferred_return_type := c.infer_composite_generic_type(gt_name,
							imethod_return_type, method.return_type)
						if inferred_return_type != ast.void_type {
							inferred_type = inferred_return_type
						}
					}
					for i, iparam in imethod.params {
						if i == 0 {
							continue
						}
						param := method.params[i] or { ast.Param{} }
						mut iparam_typ := iparam.typ
						if interface_concrete_types.len == generic_names.len {
							if resolved_param_type := c.table.convert_generic_type(iparam.typ,
								generic_names, interface_concrete_types)
							{
								iparam_typ = resolved_param_type
							}
						}
						mut need_inferred_type := false
						if !iparam.typ.has_flag(.generic) && iparam.typ == param.typ
							&& imethod.return_type == method.return_type
							&& imethod.name == method.name {
							need_inferred_type = true
						}
						if iparam_typ.has_flag(.generic)
							|| c.type_has_unresolved_generic_parts(iparam_typ) || need_inferred_type {
							inferred_param_type := c.infer_composite_generic_type(gt_name,
								iparam_typ, param.typ)
							if inferred_param_type != ast.void_type {
								inferred_type = inferred_param_type
							} else if need_inferred_type && inferred_type == ast.void_type {
								inferred_type = param.typ
							}
						}
					}
				}
				if inferred_type == ast.void_type {
					if interface_concrete_types.any(c.type_has_unresolved_generic_parts(it)) {
						return interface_type
					}
					if pos.file_idx != -1 {
						c.error('could not infer generic type `${gt_name}` in interface `${inter_sym.name}`',
							pos)
					}
					return interface_type
				}
				inferred_types << inferred_type
			}
			// add concrete types to method, but only if at least one
			// inferred type is actually concrete (not still generic).
			// When all inferred types are generic (e.g. T -> T from
			// `implements`), registering them would cause a circular
			// recheck that fails to resolve field types.
			if !inferred_types.all(it.has_flag(.generic)) {
				for imethod in inter_sym.info.methods {
					im_fkey := imethod.fkey()
					if c.table.register_fn_concrete_types(im_fkey, inferred_types) {
						c.need_recheck_generic_fns = true
					}
					if method := typ_sym.find_method_with_generic_parent(imethod.name) {
						method_concrete_types := c.concrete_types_for_type_symbol(typ_sym)
						if method_concrete_types.len > 0
							&& c.table.register_fn_concrete_types(method.fkey(), method_concrete_types) {
							c.need_recheck_generic_fns = true
						}
					}
				}
			}
			result_type := c.table.unwrap_generic_type(interface_type, generic_names,
				inferred_types)
			// Set concrete types on the instantiated interface symbol
			mut result_sym := c.table.sym(result_type)
			if mut result_sym.info is ast.Interface {
				result_sym.info.concrete_types = inferred_types
			}
			return result_type
		}
	}
	return interface_type
}
