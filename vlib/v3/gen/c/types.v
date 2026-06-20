module c

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
	inner_ct := g.tc.c_type(base_type)
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
	for opt_name, val_type in g.needed_optional_types {
		g.writeln('typedef struct { bool ok; ${val_type} value; } ${opt_name};')
	}
	if g.needed_optional_types.len > 0 {
		g.writeln('')
	}
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
						ev := g.a.child_node(f, 0)
						if ev.kind == .int_literal {
							val = ev.value.int()
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
