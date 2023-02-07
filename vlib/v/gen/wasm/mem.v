module wasm

import v.ast
import binaryen as wa

/* fn (mut g Gen) get_field_offset(typ ast.Type, name string) int {
	ts := g.table.sym(typ)
	field := ts.find_field(name) or { g.w_error('Could not find field `${name}` on init') }
	return g.structs[typ.idx()].offsets[field.i]
} */

type Var = Temporary | Struct | Global

// 
// Var {
//   name    string
//   typ     wa.Type
//   ast_typ ast.Type
// }
//
// Var.set(wa.Expression)
// Var.set_with_type(wa.Expression, Type)
// 

struct Temporary {
	name    string
	typ     wa.Type
	ast_typ ast.Type
	//
	idx int
}

struct Struct {
	name    string
	typ     wa.Type
	ast_typ ast.Type
	//
	address int
	offsets []int
}

struct Global {
	name    string
	typ     wa.Type
	ast_typ ast.Type
}

fn (mut g Gen) set(v Var, expr wa.Expression, typ ast.Type) wa.Expression {
	return match v {
		Temporary {
			wa.localset(g.mod, v.idx, g.cast_t(expr, typ, v.typ))
		}
		Struct {
			assert v.typ == typ
			// For all fields
			ti := g.table.sym(v.ast_typ).info as ast.Struct
			
			for i, f in ti.fields {
				
			}
		}
		Global {
			wa.globalset(g.mod, v.name.str, g.cast_t(expr, typ, v.typ))
		}
	}
}