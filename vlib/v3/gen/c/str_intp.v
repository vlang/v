module c

import v3.flat
import v3.types

fn (mut g FlatGen) gen_string_interp(node flat.Node) {
	n := node.children_count
	if n == 0 {
		sid := g.intern_string('')
		g.write('_str_${sid}')
		return
	}
	g.write('string_plus_many(${n}, (string[${n}]){')
	for i in 0 .. n {
		if i > 0 {
			g.write(', ')
		}
		child_id := g.a.child(&node, i)
		child := g.a.nodes[int(child_id)]
		if child.kind == .string_literal {
			sid := g.intern_string(child.value)
			g.write('_str_${sid}')
		} else if child.typ == 'string' {
			g.gen_expr(child_id)
		} else {
			typ := g.tc.resolve_type(child_id)
			if typ is types.String {
				g.gen_expr(child_id)
			} else if typ is types.Struct && typ.name == 'IError' {
				g.gen_expr(child_id)
				g.write('.message')
			} else if typ is types.Primitive {
				ct := g.tc.c_type(typ)
				g.write('${ct}_str(')
				g.gen_expr(child_id)
				g.write(')')
			} else if typ is types.ISize || typ is types.USize {
				g.write('${typ.name()}_str(')
				g.gen_expr(child_id)
				g.write(')')
			} else if typ is types.Struct {
				g.write('${c_name(typ.name)}__str(')
				g.gen_expr(child_id)
				g.write(')')
			} else if typ is types.SumType {
				g.write('${c_name(typ.name)}__str(')
				g.gen_expr(child_id)
				g.write(')')
			} else {
				g.write('int_str(')
				g.gen_expr(child_id)
				g.write(')')
			}
		}
	}
	g.write('})')
}

fn (g &FlatGen) is_string_node(id flat.NodeId) bool {
	return g.tc.resolve_type(id) is types.String
}

fn (mut g FlatGen) string_literals() {
	for i, s in g.str_lits {
		escaped := s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t',
			'\\t').replace('\r', '\\r')
		g.writeln('string _str_${i} = {"${escaped}", ${s.len}, 1};')
	}
	if g.str_lits.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) intern_string(s string) int {
	if s in g.str_lit_ids {
		return g.str_lit_ids[s]
	}
	id := g.str_lits.len
	g.str_lits << s
	g.str_lit_ids[s] = id
	return id
}
