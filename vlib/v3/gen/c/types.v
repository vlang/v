module c

import v3.flat
import v3.types

fn (mut g FlatGen) optional_type_name(t types.Type) string {
	mut base_type := types.Type(types.void_)
	if t is types.OptionType {
		base_type = t.base_type
	} else if t is types.ResultType {
		base_type = t.base_type
	} else {
		return g.tc.c_type(t)
	}

	if base_type is types.Void || base_type is types.Primitive || base_type is types.Enum {
		return 'Optional'
	}
	mut inner_ct := g.tc.c_type(base_type)
	if inner_ct.starts_with('fn_ptr:') {
		inner_ct = g.resolve_fn_ptr_type(inner_ct)
	}
	safe_name := inner_ct.replace('*', 'ptr').replace(' ', '_')
	opt_name := 'Optional_${safe_name}'
	g.needed_optional_types[opt_name] = inner_ct
	return opt_name
}

fn (mut g FlatGen) optional_value_ct(t types.Type) (string, types.Type) {
	if t is types.OptionType {
		if t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.tc.c_type(t.base_type), t.base_type
	} else if t is types.ResultType {
		if t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.tc.c_type(t.base_type), t.base_type
	}
	return 'int', types.Type(types.int_)
}

fn (mut g FlatGen) optional_typedefs() {
	for _, ret in g.tc.fn_ret_types {
		if ret is types.OptionType || ret is types.ResultType {
			g.optional_type_name(ret)
		}
	}
	mut wrote := false
	for opt_name, val_type in g.needed_optional_types {
		if g.emit_optional_typedef(opt_name, val_type) {
			wrote = true
		}
	}
	if wrote {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_optional_typedef(opt_name string, val_type string) bool {
	if opt_name in g.emitted_optional_types {
		return false
	}
	err_field := if g.has_ierror_interface() { 'IError err; ' } else { '' }
	g.writeln('typedef struct { bool ok; ${err_field}${val_type} value; } ${opt_name};')
	g.emitted_optional_types[opt_name] = true
	return true
}

fn (mut g FlatGen) enum_decls() {
	mut cur_module := ''
	for node in g.a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				name := if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
					'${cur_module}.${node.value}'
				} else {
					node.value
				}
				cn := c_name(name)
				g.writeln('typedef enum {')
				is_flag := node.typ == 'flag'
				mut val := 0
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.children_count > 0 {
						if enum_val := g.enum_field_expr_value(g.a.child(f, 0)) {
							val = enum_val
						}
					}
					if is_flag {
						g.writeln('\t${cn}__${f.value} = ${1 << val},')
						val++
					} else {
						g.writeln('\t${cn}__${f.value} = ${val},')
						val++
					}
				}
				g.writeln('} ${cn};')
				g.writeln('')
			}
			else {}
		}
	}
}

fn (g &FlatGen) enum_field_expr_value(id flat.NodeId) ?int {
	if int(id) < 0 {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return node.value.int()
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return g.enum_field_expr_value(g.a.child(&node, 0))
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.enum_field_expr_value(g.a.child(&node, 0))?
			return match node.op {
				.plus { value }
				.minus { -value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := g.enum_field_expr_value(g.a.child(&node, 0))?
			right := g.enum_field_expr_value(g.a.child(&node, 1))?
			return match node.op {
				.plus {
					left + right
				}
				.minus {
					left - right
				}
				.mul {
					left * right
				}
				.div {
					if right == 0 {
						none
					} else {
						left / right
					}
				}
				.mod {
					if right == 0 {
						none
					} else {
						left % right
					}
				}
				.amp {
					left & right
				}
				.pipe {
					left | right
				}
				.xor {
					left ^ right
				}
				.left_shift {
					int(u64(left) << right)
				}
				.right_shift, .right_shift_unsigned {
					left >> right
				}
				else {
					none
				}
			}
		}
		else {
			return none
		}
	}
}

fn (mut g FlatGen) type_alias_decls() {
	mut emitted := false
	for name, target in g.tc.type_aliases {
		if target.starts_with('fn_ptr:') || target.starts_with('C.') {
			continue
		}
		if g.has_builtins {
			continue
		}
		ct := g.tc.c_type(g.tc.parse_type(target))
		if ct == 'void' || ct == name {
			continue
		}
		g.writeln('typedef ${ct} ${c_name(name)};')
		emitted = true
	}
	if emitted {
		g.writeln('')
	}
}
