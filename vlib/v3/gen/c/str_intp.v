module c

import v3.flat
import v3.types

// gen_string_interp emits string interp output for c.
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
			mut typ := g.tc.resolve_type(child_id)
			// For a bare ident, prefer the live cgen scope binding when present: it reflects
			// locals introduced during generation (e.g. the `err` of an or-body lowered here,
			// or for-loop vars) that resolve_type may stale-cache as `int`.
			if child.kind == .ident {
				if scope_typ := g.tc.cur_scope.lookup(child.value) {
					if scope_typ !is types.Void {
						typ = scope_typ
					}
				}
			}
			typ_name := types.Type(typ).name()
			if typ is types.String {
				g.gen_expr(child_id)
			} else if g.gen_map_str_expr(child_id, typ) {
				// emitted by gen_map_str_expr
			} else if typ_name == 'IError' || typ_name.ends_with('.IError') {
				// IError may resolve as Interface/Alias/Struct depending on context; match by
				// name and interpolate its `.message` (mirrors the transformer's IError path).
				g.gen_expr(child_id)
				g.write('.message')
			} else if typ is types.Primitive {
				prim_name := types.Type(typ).name()
				g.write('${c_name('${prim_name}.str')}(')
				g.gen_expr(child_id)
				g.write(')')
			} else if typ is types.ISize || typ is types.USize {
				g.write('${c_name('${typ.name()}.str')}(')
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
				g.write('int__str(')
				g.gen_expr(child_id)
				g.write(')')
			}
		}
	}
	g.write('})')
}

// is_string_node reports whether is string node applies in c.
fn (g &FlatGen) is_string_node(id flat.NodeId) bool {
	return g.tc.resolve_type(id) is types.String
}

// string_literals supports string literals handling for FlatGen.
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

// intern_string supports intern string handling for FlatGen.
fn (mut g FlatGen) intern_string(s string) int {
	if s in g.str_lit_ids {
		return g.str_lit_ids[s]
	}
	id := g.str_lits.len
	g.str_lits << s
	g.str_lit_ids[s] = id
	return id
}
