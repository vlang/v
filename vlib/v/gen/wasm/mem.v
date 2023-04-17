module wasm

import wasm
import v.ast

struct Var {
	name       string
	typ        ast.Type
	idx        wasm.LocalIndex
	is_pointer bool
	is_global  bool
}

fn (mut g Gen) get_var_from_ident(ident ast.Ident) Var {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.w_error('unknown variable ${ident.name}') }
	}

	match mut obj {
		ast.Var {
			if g.local_vars.len == 0 {
				g.w_error('get_var_from_ident: g.local_vars.len == 0')
			}
			mut c := g.local_vars.len
			for {
				c--
				if g.local_vars[c].name == obj.name {
					return g.local_vars[c]
				}
				if c == 0 {
					break
				}
			}
			g.w_error('get_var_from_ident: unreachable, variable not found')
		}
		else {
			g.w_error('unsupported variable type type:${obj} name:${ident.name}')
		}
	}
}

fn (mut g Gen) get_var_from_expr(node ast.Expr) ?Var {
	match node {
		ast.Ident {
			return g.get_var_from_ident(node)
		}
		ast.ParExpr {
			return g.get_var_from_expr(node.expr)
		}
		/* ast.SelectorExpr {
			
		}
		ast.IndexExpr {

		} */
		/* ast.PrefixExpr {
			// *p = expr
			v := g.get_var_from_expr(node.expr) or {
				return none
			}
			v.ref()
			return none
		} */
		else {
			g.w_error('get_var_from_expr: unexpected `${node.type_name()}`')
		}
	}
}

fn (mut g Gen) bp() int {
	if g.bp_idx == -1 {
		g.bp_idx = g.func.new_local(.i32_t)
	}
	return g.bp_idx
}

fn (mut g Gen) new_local(node ast.Ident, typ ast.Type) {
	ts := g.table.sym(typ)

	match ts.info {
		ast.Enum {
			g.new_local(node, ts.info.typ)
			return
		}
		ast.Alias {
			g.new_local(node, ts.info.parent_type)
			return
		}
		else {}
	}
	
	wtyp := g.get_wasm_type(typ)

	idx := g.func.new_local(wtyp)
	v := Var{
		name: node.name
		typ: typ
		idx: idx
		is_pointer: typ.is_ptr() || !g.is_pure_type(typ)
	}
	g.local_vars << v
	
	if !v.is_pointer {
		return
	}

	// allocate memory, then assign to the local
	// 
	match ts.info {
		ast.Struct, ast.ArrayFixed {
			size, align := g.get_type_size_align(typ)
			padding := (align - g.stack_frame % align) % align
			address := g.stack_frame
			g.stack_frame += size + padding

			// lea v, [bp + address]
			// 
			g.func.local_get(g.bp())
			if address != 0 {
				g.func.i32_const(address)
				g.func.add(.i32_t)	
			}
			g.func.local_set(v.idx)
		}
		else {
			g.w_error('new_local: type `${*ts}` (${ts.info.type_name()}) is not a supported local type')
		}
	}
}

// is_pure_type(voidptr) == true
// is_pure_type(&Struct) == false
fn (g Gen) is_pure_type(typ ast.Type) bool {
	if typ.is_pure_int() || typ.is_pure_float() || typ == ast.char_type_idx || typ.is_pointer()
		|| typ.is_bool() {
		return true
	}
	ts := g.table.sym(typ)
	if ts.info is ast.Alias {
		return g.is_pure_type(ts.info.parent_type)
	}
	return false
}

fn log2(size int) int {
	return match size {
		1 { 0 }
		2 { 1 }
		4 { 2 }
		8 { 3 }
		else { panic("unreachable") }
	}
}

fn (mut g Gen) load(typ ast.Type) {
	size, align := g.table.type_size(typ)
	wtyp := g.as_numtype(g.get_wasm_type(typ))

	match size {
		1 { g.func.load8(wtyp, typ.is_signed(), log2(align), 0) }
		2 { g.func.load16(wtyp, typ.is_signed(), log2(align), 0) }
		else { g.func.load(wtyp, log2(align), 0) }
	}
}

fn (mut g Gen) store(typ ast.Type) {
	size, align := g.table.type_size(typ)
	wtyp := g.as_numtype(g.get_wasm_type(typ))

	match size {
		1 { g.func.store8(wtyp, log2(align), 0) }
		2 { g.func.store16(wtyp, log2(align), 0) }
		else { g.func.store(wtyp, log2(align), 0) }
	}
}

fn (mut g Gen) get(v Var) {
	if v.is_global {
		panic('globals unimplemented')
	} else {
		g.func.local_get(v.idx)
	}

	if v.is_pointer && g.is_pure_type(v.typ) {
		g.load(v.typ)
	}
}

// set structures with pointer, memcpy
// set pointers with value, get local, store value
// set value, set local
fn (mut g Gen) set(v Var) {
	if !v.is_pointer {
		if v.is_global {
			panic('globals unimplemented')
		} else {
			g.func.local_set(v.idx)
		}
		return
	}

	if v.is_global {
		panic('globals unimplemented')
	} else {
		g.func.local_get(v.idx)
	}

	if g.is_pure_type(v.typ) {
		g.store(v.typ)
		return
	}

	panic('memcpy unimplemented')
}

fn (mut g Gen) ref(v Var) {
	if !v.is_pointer {
		panic('unreachable')
	}

	if v.is_global {
		panic('globals unimplemented')
	} else {
		g.func.local_get(v.idx)
	}
}

// creates a new pointer variable with the offset `offset` and type `typ`
fn (mut g Gen) offset(v Var, typ ast.Type, offset int) Var {
	if !v.is_pointer {
		panic('unreachable')
	}
	
	nv := Var {
		...v
		typ: typ
		idx: g.func.new_local(.i32_t)
	}

	g.ref(v)
	if offset != 0 {
		g.func.i32_const(offset)
		g.func.add(.i32_t)
	}
	g.func.local_set(nv.idx)

	return nv
}

fn (mut g Gen) zero_fill(v Var, size int) {
	assert size > 0

	// no need for a complex system utilising each different type of store
	// to get full efficient coverage of all sizes, the webassembly optimiser
	// will spot uses of `memory.fill` and do the work for us accordingly.
	// 
	// in the future though, do it!
	// 
	// reference code below:
	// 
	// ```v
	// for size > 0 {
	//     if size - 8 >= 0 {
	//         size -= 8
	//         // i64.store
	//     } else if size - 4 >= 0 {
	//         size -= 4
	//         // i32.store
	//     } else if size - 2 >= 0 {
	//         size -= 2
	//         // i32.store16
	//     } else if size - 1 >= 0 {
	//         size -= 1
	//         // i32.store8
	//     }
	// }
	// ```
	
	g.ref(v)
	g.func.i32_const(0)
	g.func.i32_const(size)
	g.func.memory_fill()
}

fn (mut g Gen) set_with_expr(init ast.Expr, v Var) {
	match init {
		ast.StructInit {
			size, _ := g.get_type_size_align(v.typ)
			ts := g.table.sym(v.typ)
			ts_info := ts.info as ast.Struct
			
			if init.fields.len == 0 && !(ts_info.fields.any(it.has_default_expr)) {
				// Struct definition contains no default initialisers
				// AND struct init contains no set values.
				g.zero_fill(v, size)
				return
			}

			for i, f in ts_info.fields {
				field_to_be_set := init.fields.map(it.name).contains(f.name)

				if !field_to_be_set {
					offset := g.structs[v.typ.idx()].offsets[i]
					offset_var := g.offset(v, f.typ, offset)

					fsize, _ := g.get_type_size_align(f.typ)
					
					if f.has_default_expr {
						g.expr(f.default_expr, f.typ)
						g.set(offset_var)
					} else {
						g.zero_fill(offset_var, fsize)
					}						
				}
			}

			for f in init.fields {
				field := ts.find_field(f.name) or {
					g.w_error('could not find field `${f.name}` on init')
				}
				
				offset := g.structs[v.typ.idx()].offsets[field.i]
				offset_var := g.offset(v, f.expected_type, offset)

				g.expr(f.expr, f.expected_type)
				g.set(offset_var)
			}
		}
		else {
			panic("unimplemented")
		}
	}
}