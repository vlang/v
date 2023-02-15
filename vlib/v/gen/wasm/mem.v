module wasm

import v.ast
import binaryen as wa

type Var = /* Global | */ Stack | Temporary | ast.Ident

//
// Var {
//   name    string
//   typ     wa.Type
//   ast_typ ast.Type
// }
//

struct Temporary {
	name    string
	typ     wa.Type
	ast_typ ast.Type
	//
	idx int
}

struct Stack {
	name    string
	ast_typ ast.Type
	//
	address int
}

/* struct Global {
	name    string
	typ     wa.Type
	ast_typ ast.Type
} */

fn (g Gen) is_pure_type(typ ast.Type) bool {
	return typ.is_pure_int() || typ.is_pure_float() || typ == ast.char_type_idx
		|| typ.is_real_pointer() || typ.is_bool()
}

fn (mut g Gen) get_var_from_ident(ident ast.Ident) Var {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.w_error('unknown variable ${ident.name}') }
	}
	match mut obj {
		ast.Var {
			if obj.name !in g.local_addresses {
				return g.local_temporaries[g.get_local_temporary(obj.name)]
			}
			return g.local_addresses[obj.name]
		}
		else {
			g.w_error('unsupported variable type type:${obj} name:${ident.name}')
		}
	}
}


fn (mut g Gen) get_var_from_expr(node ast.Expr) Var {
	match node {
		ast.Ident {
			return g.get_var_from_ident(node)
		}
		ast.SelectorExpr {
			address := g.path_expr_address(node)
			match address {
				wa.Expression {
					panic("not implemented for complex expressions")
				}
				int {
					return Stack {
						ast_typ: node.typ
						address: address
					}
				}
			}
		}
		ast.IndexExpr {
			// TODO: this would require an unknown offset at compile time
			g.w_error("`ident[expr] = expr` not implemented")
		}
		ast.PrefixExpr {
			g.w_error('`*ident = 10` not implemented')
		}
		else {
			g.w_error('get_var_from_expr: unexpected `${node.type_name()}`')
		}
	}	
}

fn (mut g Gen) get_local_temporary(name string) int {
	if g.local_temporaries.len == 0 {
		g.w_error('get_local: g.local_temporaries.len == 0')
	}
	mut c := g.local_temporaries.len
	for {
		c--
		if g.local_temporaries[c].name == name {
			return c
		}
		if c == 0 {
			break
		}
	}
	g.w_error("get_local: cannot get '${name}'")
}

fn (mut g Gen) get_local_temporary_from_ident(ident ast.Ident) (int, wa.Type) {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.w_error('unknown variable ${ident.name}') }
	}
	match mut obj {
		ast.Var {
			idx := g.get_local_temporary(obj.name)
			return idx, g.local_temporaries[idx].typ
		}
		else {
			g.w_error('unsupported variable type type:${obj} name:${ident.name}')
		}
	}
}

fn (mut g Gen) new_local_temporary_anon(typ ast.Type) int {
	ret := g.local_temporaries.len
	g.local_temporaries << Temporary{
		name: '_'
		typ: g.get_wasm_type(typ)
		ast_typ: typ
		idx: ret
	}
	return ret
}

fn (mut g Gen) new_local_temporary(name string, typ ast.Type) Temporary {
	idx := g.local_temporaries.len
	var := Temporary{
		name: name
		typ: g.get_wasm_type(typ)
		ast_typ: typ
		idx: idx
	}
	g.local_temporaries << var	
	return var
}

fn (mut g Gen) new_local(var ast.Ident, typ ast.Type) {
	if g.is_pure_type(typ) {
		g.new_local_temporary(var.name, typ)
	} else {
		g.allocate_struct(var.name, typ)
	}
}

fn (mut g Gen) deref(expr wa.Expression, expected ast.Type) wa.Expression {
	size, _ := g.table.type_size(expected)

	return wa.load(g.mod, u32(size), g.is_signed(expected), 0, 0, g.get_wasm_type(expected), expr, c'__vmem')
}

fn (mut g Gen) get_field_offset(typ ast.Type, name string) int {
	ts := g.table.sym(typ)
	field := ts.find_field(name) or { g.w_error('could not find field `${name}` on init') }
	if typ !in g.structs {
		g.get_type_size_align(typ.idx())
	}
	return g.structs[typ.idx()].offsets[field.i]
}

fn (mut g Gen) get_type_size_align(typ ast.Type) (int, int) {
	ts := g.table.sym(typ)
	if ts.size != -1 && typ in g.structs {
		return ts.size, ts.align
	}

	ti := ts.info as ast.Struct

	// Code borrowed from native, hope you don't mind!

	mut strc := StructInfo{}
	mut size := 0
	mut align := 1
	for f in ti.fields {
		f_size, f_align := g.table.type_size(f.typ)
		if f_size == 0 {
			strc.offsets << 0
			continue
		}
		padding := (f_align - size % f_align) % f_align
		strc.offsets << size + padding
		size += f_size + padding
		if f_align > align {
			align = f_align
		}
	}
	size = (size + align - 1) / align * align
	g.structs[typ.idx()] = strc

	mut ts_ := g.table.sym(typ)
	ts_.size = size
	ts_.align = align

	return size, align
}

fn (mut g Gen) allocate_struct(name string, typ ast.Type) int {
	// Gen.allocate_struct() must be called with a struct!

	size, align := g.get_type_size_align(typ)
	padding := (align - g.stack_frame % align) % align
	address := g.stack_frame
	g.stack_frame += size + padding
	g.local_addresses[name] = Stack{
		name: name
		ast_typ: typ
		address: address
	}

	return address
}

fn (mut g Gen) get_bp() wa.Expression {
	return wa.localget(g.mod, g.bp_idx, type_i32)
}


/* fn (mut g Gen) get_ident(node ast.Ident, expected ast.Type) (wa.Expression, int) {
	idx, typ := g.get_local_temporary_from_ident(node)
	expr := wa.localget(g.mod, idx, typ)

	return g.cast(expr, typ, g.is_signed(g.local_temporaries[idx].ast_typ), g.get_wasm_type(expected)), idx
} */

fn (mut g Gen) lea_address(address int) wa.Expression {
	return if address != 0 {
		wa.binary(g.mod, wa.addint32(), g.get_bp(), wa.constant(g.mod, wa.literalint32(address)))
	} else {
		g.get_bp()
	}
}

fn (mut g Gen) get_var(var Var) wa.Expression {
	return match var {
		ast.Ident {
			g.get_var(g.get_var_from_ident(var))
		}
		Temporary {
			wa.localget(g.mod, var.idx, var.typ)
		}
		Stack {
			g.lea_address(var.address)
		}
	}
}

// WASM expression and integer relative to base pointer.
type PathAddress = wa.Expression | int

// `node` is expected to not be `ast.Ident` when called externally.
fn (mut g Gen) path_expr_address(node ast.Expr) PathAddress {
	match node {
		ast.SelectorExpr {
			address := g.path_expr_address(node.expr)
			offset := g.get_field_offset(node.expr_type, node.field_name)
			return match address {
				wa.Expression {
					wa.binary(g.mod, wa.addint32(), address, wa.constant(g.mod, wa.literalint32(offset)))
				}
				int {
					address + offset
				}
			}
		}
		ast.Ident {
			var := g.get_var_from_ident(node)
			return match var {
				Temporary {
					wa.localget(g.mod, var.idx, var.typ)
				}
				Stack {
					var.address
				}
				else {
					panic("unreachable")
				}
			}
		}
		// handle a[expr] and *a
		else {
			g.w_error("path_expr: forbidden node `${node.type_name()}`")
		}
	}
}

fn (mut g Gen) path_expr_ptr(node ast.Expr) wa.Expression {
	// deref here
	rel_address := g.path_expr_address(node)
	return match rel_address {
		wa.Expression { rel_address }
		int { wa.constant(g.mod, wa.literalint32(rel_address)) }
	}
}

fn (mut g Gen) path_expr_t(node ast.Expr, expected ast.Type) wa.Expression {
	return g.deref_local(g.path_expr_address(node), expected)
}

fn (mut g Gen) deref_local(address PathAddress, expected ast.Type) wa.Expression {
	size, _ := g.table.type_size(expected)

	match address {
		wa.Expression {
			expr := wa.binary(g.mod, wa.addint32(), g.get_bp(), address)
			return wa.load(g.mod, u32(size), g.is_signed(expected), 0, 0, g.get_wasm_type(expected), expr, c'__vmem')
		}
		int {
			return wa.load(g.mod, u32(size), g.is_signed(expected), u32(address), 0, g.get_wasm_type(expected), g.get_bp(), c'__vmem')
		}
	}
	// return wa.load(g.mod, u32(size), g.is_signed(expected), 0, 0, g.get_wasm_type(expected), expr, c'__vmem')
}

// Will automatcally cast value from `var` to `ast_type`, will ignore if struct value.
// TODO: When supporting base types on the stack, actually cast them.
fn (mut g Gen) get_var_t(var Var, ast_typ ast.Type) wa.Expression {
	return match var {
		ast.Ident {
			g.get_var_t(g.get_var_from_ident(var), ast_typ)
		}
		Temporary {
			expr := wa.localget(g.mod, var.idx, var.typ)
			g.cast_t(expr, var.ast_typ, ast_typ)
		}
		Stack {
			if var.address != 0 {
				wa.binary(g.mod, wa.addint32(), g.get_bp(), wa.constant(g.mod, wa.literalint32(var.address)))
			} else {
				g.get_bp()
			}
		}
	}
}

[params]
struct SetConfig {
	offset  int
	ast_typ ast.Type
}

fn (mut g Gen) set_var(var Var, expr wa.Expression, cfg SetConfig) wa.Expression {
	return match var {
		ast.Ident {
			g.set_var(g.get_var_from_ident(var), expr, cfg)
		}
		Temporary {
			wa.localset(g.mod, var.idx, expr)
		}
		Stack {
			ast_typ := if cfg.ast_typ != 0 {
				cfg.ast_typ
			} else {
				var.ast_typ
			}
			
			ts := g.table.sym(ast_typ)

			if ts.kind == .struct_ {
				// `expr` is pointer
				g.blit_local(expr, ast_typ, var.address + cfg.offset)
			} else {
				size, _ := g.table.type_size(ast_typ)
				// println("address: ${var.address}, offset: ${cfg.offset}")
				wa.store(g.mod, u32(size), u32(var.address + cfg.offset), 0, g.get_bp(), expr, g.get_wasm_type(ast_typ), c'__vmem')
			}
		}
	}
}

// `memcpy` from `ptr` to known local `address` in stack memory.
fn (mut g Gen) blit_local(ptr wa.Expression, ast_typ ast.Type, address int) wa.Expression {
	size, _ := g.get_type_size_align(ast_typ)
	return wa.memorycopy(g.mod, g.lea_address(address), ptr, wa.constant(g.mod, wa.literalint32(size)), c'__vmem', c'__vmem')
}

// `memcpy` from `ptr` to `dest`
fn (mut g Gen) blit(ptr wa.Expression, ast_typ ast.Type, dest wa.Expression) wa.Expression {
	size, _ := g.get_type_size_align(ast_typ)
	return wa.memorycopy(g.mod, dest, ptr, wa.constant(g.mod, wa.literalint32(size)), c'__vmem', c'__vmem')
}

fn (mut g Gen) init_struct(var Var, init ast.StructInit) wa.Expression {
	match var {
		ast.Ident {
			return g.init_struct(g.get_var_from_ident(var), init)
		}
		Stack {
			mut exprs := []wa.Expression{}
			
			ts := g.table.sym(var.ast_typ)
			match ts.info {
				ast.Struct {
					for i, f in ts.info.fields {
						field_to_be_set := init.fields.map(it.name).contains(f.name)
						if !field_to_be_set {
							offset := g.structs[var.ast_typ.idx()].offsets[i]
							initexpr := if f.has_default_expr {
								g.expr(f.default_expr, f.typ) // or `unaliased_typ`?
							} else {
								g.literal("0", f.typ)
							}
							
							exprs << g.set_var(var, initexpr, ast_typ: f.typ, offset: offset)
						}
					}
				}
				else {}
			}

			for f in init.fields {
				field := ts.find_field(f.name) or {
					g.w_error('could not find field `${f.name}` on init')
				}
				offset := g.structs[var.ast_typ.idx()].offsets[field.i]
				initexpr := g.expr(f.expr, f.expected_type)

				exprs << g.set_var(var, initexpr, ast_typ: f.expected_type, offset: offset)
			}

			return g.mknblock("STRUCTINIT", exprs)
		}
		else {}
	}
	panic("unreachable")
}