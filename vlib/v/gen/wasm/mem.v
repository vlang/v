module wasm

import v.ast
import v.gen.wasm.binaryen
import strconv

type Var = Global | Stack | Temporary | ast.Ident

fn (v Var) ast_typ() int {
	if v is Temporary {
		return v.ast_typ
	}
	if v is Stack {
		return v.ast_typ
	}
	if v is Global {
		return v.ast_typ
	}
	panic('unreachable')
}

fn (v Var) address() int {
	return (v as Stack).address
}

struct Temporary {
	name    string
	typ     binaryen.Type
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

struct Global {
	name    string
	ast_typ ast.Type
	//
	abs_address int
}

fn (g Gen) is_pure_type(typ ast.Type) bool {
	if typ.is_pure_int() || typ.is_pure_float() || typ == ast.char_type_idx
		|| typ.is_any_kind_of_pointer() || typ.is_bool() {
		return true
	}
	ts := g.table.sym(typ)
	if ts.info is ast.Alias {
		return g.is_pure_type(ts.info.parent_type)
	}
	return false
}

fn (mut g Gen) new_global_const(obj ast.ScopeObject) {
	obj_expr := match obj {
		ast.ConstField { obj.expr }
		ast.GlobalField { obj.expr }
		else { panic('unreachable') }
	}
	typ := ast.mktyp(obj.typ)

	size, align := g.get_type_size_align(typ)
	padding := (align - g.constant_data_offset % align) % align
	g.globals[obj.name] = GlobalData{
		init: obj_expr
		ast_typ: typ
		abs_address: g.constant_data_offset
	}
	g.constant_data_offset += size + padding
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
		ast.ConstField {
			if obj.name !in g.globals {
				g.new_global_const(obj)
			}
			return g.globals[obj.name].to_var(obj.name)
		}
		ast.GlobalField {
			if obj.name !in g.globals {
				g.new_global_const(obj)
			}
			return g.globals[obj.name].to_var(obj.name)
		}
		else {
			g.w_error('unsupported variable type type:${obj} name:${ident.name}')
		}
	}
}

type LocalOrPointer = Var | binaryen.Expression

fn (mut g Gen) get_or_lea_lop(lp LocalOrPointer, expected ast.Type) binaryen.Expression {
	size, _ := g.table.type_size(expected)

	mut offset := 0
	mut parent_typ := expected
	mut is_expr := false

	expr := match lp {
		binaryen.Expression {
			is_expr = true
			lp
		}
		Var {
			match lp {
				Temporary {
					parent_typ = lp.ast_typ
					binaryen.localget(g.mod, lp.idx, g.get_wasm_type(expected))
				}
				Stack {
					parent_typ = lp.ast_typ
					offset = lp.address
					g.get_bp()
				}
				Global {
					parent_typ = lp.ast_typ
					is_expr = true
					g.literalint(lp.abs_address, ast.int_type)
				}
				else {
					panic('unreachable')
				}
			}
		}
	}

	if (!is_expr && parent_typ == expected) || !g.is_pure_type(expected) {
		return expr
	}

	return binaryen.load(g.mod, u32(size), g.is_signed(expected), u32(offset), 0, g.get_wasm_type(expected),
		expr, c'memory')
}

fn (mut g Gen) lea_lop(lp LocalOrPointer, expected ast.Type) binaryen.Expression {
	expr := match lp {
		binaryen.Expression {
			lp
		}
		Var {
			match lp {
				Temporary {
					binaryen.localget(g.mod, lp.idx, g.get_wasm_type(expected))
				}
				Stack {
					g.get_bp()
				}
				Global {
					g.literalint(lp.abs_address, ast.int_type)
				}
				else {
					panic('unreachable')
				}
			}
		}
	}

	return expr
}

fn (mut g Gen) lea_var_from_expr(node ast.Expr) binaryen.Expression {
	var := g.get_var_from_expr(node)

	return match var {
		binaryen.Expression {
			var
		}
		Var {
			match var {
				Temporary {
					if g.is_pure_type(var.ast_typ) {
						g.w_error('lea_var_from_expr: you cannot take the address of a pure temporary')
					}
					binaryen.localget(g.mod, var.idx, type_i32)
				}
				Stack {
					g.lea_address(var.address)
				}
				Global {
					g.literalint(var.abs_address, ast.int_type)
				}
				else {
					panic('unreachable')
				}
			}
		}
	}
}

fn (mut g Gen) local_or_pointer_add_offset(v Var, offset int) LocalOrPointer {
	return match v {
		Temporary {
			binaryen.binary(g.mod, binaryen.addint32(), binaryen.localget(g.mod, v.idx,
				type_i32), binaryen.constant(g.mod, binaryen.literalint32(offset)))
		}
		Stack {
			Var(Stack{
				...v
				address: v.address + offset
			})
		}
		Global {
			Var(Global{
				...v
				abs_address: v.abs_address + offset
			})
		}
		ast.Ident {
			panic('unreachable')
		}
	}
}

// TODO: return `Var | binaryen.Expression`
fn (mut g Gen) get_var_from_expr(node ast.Expr) LocalOrPointer {
	return match node {
		ast.Ident {
			g.get_var_from_ident(node)
		}
		ast.ParExpr {
			g.get_var_from_expr(node.expr)
		}
		ast.SelectorExpr {
			address := g.get_var_from_expr(node.expr)
			offset := g.get_field_offset(node.expr_type, node.field_name)

			match address {
				binaryen.Expression {
					binaryen.binary(g.mod, binaryen.addint32(), address, binaryen.constant(g.mod,
						binaryen.literalint32(offset)))
				}
				Var {
					g.local_or_pointer_add_offset(address, offset)
				}
			}
		}
		ast.IndexExpr {
			address := g.get_var_from_expr(node.left)

			ts := g.table.sym(node.left_type)
			deref_type := if g.is_pure_type(node.left_type) {
				node.left_type.set_nr_muls(node.left_type.nr_muls() - 1)
			} else if ts.kind == .array_fixed {
				(ts.info as ast.ArrayFixed).elem_type
			} else {
				node.left_type
			}
			size, _ := g.get_type_size_align(deref_type)

			index := g.expr(node.index, ast.int_type)
			mut ptr_address := binaryen.Expression(0)

			if address is binaryen.Expression {
				ptr_address = address
			}
			if address is Var {
				ptr_address = g.get_var_t(address, ast.voidptr_type)
			}

			// ptr + index * size
			binaryen.binary(g.mod, binaryen.addint32(), ptr_address, binaryen.binary(g.mod,
				binaryen.mulint32(), index, g.literalint(size, ast.int_type)))
		}
		ast.PrefixExpr {
			g.lea_lop(g.get_var_from_expr(node.right), ast.voidptr_type)
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

fn (mut g Gen) new_local_temporary_anon_wtyp(w_typ binaryen.Type) int {
	ret := g.local_temporaries.len
	g.local_temporaries << Temporary{
		name: '_'
		typ: w_typ
		idx: ret
	}
	return ret
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
		return
	}

	ts := g.table.sym(typ)
	match ts.info {
		ast.Struct, ast.ArrayFixed {
			g.allocate_local_var(var.name, typ)
		}
		ast.Enum {
			g.new_local_temporary(var.name, ts.info.typ)
		}
		else {
			g.w_error('new_local: type `${*ts}` (${ts.info.type_name()}) is not a supported local type')
		}
	}
}

fn (mut g Gen) deref(expr binaryen.Expression, expected ast.Type) binaryen.Expression {
	size, _ := g.get_type_size_align(expected)
	return binaryen.load(g.mod, u32(size), g.is_signed(expected), 0, 0, g.get_wasm_type(expected),
		expr, c'memory')
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

	if ts.info !is ast.Struct {
		return g.table.type_size(typ)
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

fn (mut g Gen) allocate_local_var(name string, typ ast.Type) int {
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

fn (mut g Gen) get_bp() binaryen.Expression {
	return binaryen.localget(g.mod, g.bp_idx, type_i32)
}

fn (mut g Gen) lea_address(address int) binaryen.Expression {
	return if address != 0 {
		binaryen.binary(g.mod, binaryen.addint32(), g.get_bp(), binaryen.constant(g.mod,
			binaryen.literalint32(address)))
	} else {
		g.get_bp()
	}
}

// Will automatcally cast value from `var` to `ast_type`, will ignore if struct value.
// TODO: When supporting base types on the stack, actually cast them.
fn (mut g Gen) get_var_t(var Var, ast_typ ast.Type) binaryen.Expression {
	return match var {
		ast.Ident {
			g.get_var_t(g.get_var_from_ident(var), ast_typ)
		}
		Temporary {
			expr := binaryen.localget(g.mod, var.idx, var.typ)
			g.cast_t(expr, var.ast_typ, ast_typ)
		}
		Stack {
			address := if var.address != 0 {
				binaryen.binary(g.mod, binaryen.addint32(), g.get_bp(), binaryen.constant(g.mod,
					binaryen.literalint32(var.address)))
			} else {
				g.get_bp()
			}

			if g.is_pure_type(var.ast_typ) {
				g.cast_t(g.deref(address, var.ast_typ), var.ast_typ, ast_typ)
			} else {
				address
			}
		}
		Global {
			address := g.literalint(var.abs_address, ast.int_type)
			if g.is_pure_type(var.ast_typ) {
				g.cast_t(g.deref(address, var.ast_typ), var.ast_typ, ast_typ)
			} else {
				address
			}
		}
	}
}

[params]
struct SetConfig {
	offset  int
	ast_typ ast.Type
}

fn (mut g Gen) set_to_address(address_expr binaryen.Expression, expr binaryen.Expression, ast_typ ast.Type) binaryen.Expression {
	return if !g.is_pure_type(ast_typ) {
		// `expr` is pointer
		g.blit(expr, ast_typ, address_expr)
	} else {
		size, _ := g.table.type_size(ast_typ)
		binaryen.store(g.mod, u32(size), 0, 0, address_expr, expr, g.get_wasm_type(ast_typ),
			c'memory')
	}
}

fn (mut g Gen) set_var_v(address Var, expr binaryen.Expression, cfg SetConfig) binaryen.Expression {
	return g.set_var(address, expr, cfg)
}

fn (mut g Gen) set_var(address LocalOrPointer, expr binaryen.Expression, cfg SetConfig) binaryen.Expression {
	ast_typ := if cfg.ast_typ != 0 {
		cfg.ast_typ
	} else {
		(address as Var).ast_typ()
	}
	match address {
		binaryen.Expression {
			return g.set_to_address(address, expr, ast_typ)
		}
		Var {
			var := address

			return match var {
				ast.Ident {
					g.set_var(g.get_var_from_ident(var), expr, cfg)
				}
				Temporary {
					binaryen.localset(g.mod, var.idx, expr)
				}
				Stack {
					if !g.is_pure_type(ast_typ) {
						// `expr` is pointer
						g.blit_local(expr, ast_typ, var.address + cfg.offset)
					} else {
						size, _ := g.table.type_size(ast_typ)
						// println("address: ${var.address}, offset: ${cfg.offset}")
						binaryen.store(g.mod, u32(size), u32(var.address + cfg.offset),
							0, g.get_bp(), expr, g.get_wasm_type(ast_typ), c'memory')
					}
				}
				Global {
					address_expr := g.literalint(var.abs_address + cfg.offset, ast.int_type)
					g.set_to_address(address_expr, expr, ast_typ)
				}
			}
		}
	}
}

// zero out stack memory in known local `address`.
fn (mut g Gen) zero_fill(ast_typ ast.Type, address int) binaryen.Expression {
	size, _ := g.get_type_size_align(ast_typ)

	if size <= 4 {
		zero := g.literalint(0, ast.int_type)
		return binaryen.store(g.mod, u32(size), u32(address), 0, g.get_bp(), zero, type_i32,
			c'memory')
	} else if size <= 8 {
		zero := g.literalint(0, ast.i64_type)
		return binaryen.store(g.mod, u32(size), u32(address), 0, g.get_bp(), zero, type_i64,
			c'memory')
	}
	return binaryen.memoryfill(g.mod, g.lea_address(address), g.literalint(0, ast.int_type),
		g.literalint(size, ast.int_type), c'memory')
}

// `memcpy` from `ptr` to known local `address` in stack memory.
fn (mut g Gen) blit_local(ptr binaryen.Expression, ast_typ ast.Type, address int) binaryen.Expression {
	size, _ := g.get_type_size_align(ast_typ)
	return binaryen.memorycopy(g.mod, g.lea_address(address), ptr, binaryen.constant(g.mod,
		binaryen.literalint32(size)), c'memory', c'memory')
}

// `memcpy` from `ptr` to `dest`
fn (mut g Gen) blit(ptr binaryen.Expression, ast_typ ast.Type, dest binaryen.Expression) binaryen.Expression {
	size, _ := g.get_type_size_align(ast_typ)
	return binaryen.memorycopy(g.mod, dest, ptr, binaryen.constant(g.mod, binaryen.literalint32(size)),
		c'memory', c'memory')
}

fn (mut g Gen) init_struct(var Var, init ast.StructInit) binaryen.Expression {
	match var {
		ast.Ident {
			return g.init_struct(g.get_var_from_ident(var), init)
		}
		Stack {
			mut exprs := []binaryen.Expression{}

			ts := g.table.sym(var.ast_typ)
			match ts.info {
				ast.Struct {
					if init.fields.len == 0 && !(ts.info.fields.any(it.has_default_expr)) {
						// Struct definition contains no default initialisers
						// AND struct init contains no set values.
						return g.mknblock('STRUCTINIT(ZERO)', [
							g.zero_fill(var.ast_typ, var.address),
						])
					}

					for i, f in ts.info.fields {
						field_to_be_set := init.fields.map(it.name).contains(f.name)
						fts := g.table.sym(f.typ)
						if !field_to_be_set {
							g.get_type_size_align(var.ast_typ)
							offset := g.structs[var.ast_typ.idx()].offsets[i]
							if f.has_default_expr {
								init_expr := g.expr(f.default_expr, f.typ) // or `unaliased_typ`?
								exprs << g.set_var_v(var, init_expr, ast_typ: f.typ, offset: offset)
							} else {
								if fts.info is ast.Struct {
									exprs << g.init_struct(Stack{
										address: var.address + offset
										ast_typ: f.typ
									}, ast.StructInit{})
								} else {
									exprs << g.zero_fill(f.typ, var.address + offset)
								}
							}
							// TODO: replace invocations of `set_var` with `assign_expr_to_var`?						
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

				exprs << g.set_var_v(var, initexpr, ast_typ: f.expected_type, offset: offset)
			}

			return g.mknblock('STRUCTINIT', exprs)
		}
		else {}
	}
	panic('unreachable')
}

// From native, this should be taken out into `StringLiteral.eval_escape_codes()`
fn (mut g Gen) eval_escape_codes(str_lit ast.StringLiteral) string {
	if str_lit.is_raw {
		return str_lit.val
	}

	str := str_lit.val
	mut buffer := []u8{}

	mut i := 0
	for i < str.len {
		if str[i] != `\\` {
			buffer << str[i]
			i++
			continue
		}

		// skip \
		i++
		match str[i] {
			`\\`, `'`, `"` {
				buffer << str[i]
				i++
			}
			`a`, `b`, `f` {
				buffer << str[i] - u8(90)
				i++
			}
			`n` {
				buffer << `\n`
				i++
			}
			`r` {
				buffer << `\r`
				i++
			}
			`t` {
				buffer << `\t`
				i++
			}
			`u` {
				i++
				utf8 := strconv.parse_int(str[i..i + 4], 16, 16) or {
					g.w_error('invalid \\u escape code (${str[i..i + 4]})')
					0
				}
				i += 4
				buffer << u8(utf8)
				buffer << u8(utf8 >> 8)
			}
			`v` {
				buffer << `\v`
				i++
			}
			`x` {
				i++
				c := strconv.parse_int(str[i..i + 2], 16, 8) or {
					g.w_error('invalid \\x escape code (${str[i..i + 2]})')
					0
				}
				i += 2
				buffer << u8(c)
			}
			`0`...`7` {
				c := strconv.parse_int(str[i..i + 3], 8, 8) or {
					g.w_error('invalid escape code \\${str[i..i + 3]}')
					0
				}
				i += 3
				buffer << u8(c)
			}
			else {
				g.w_error('invalid escape code \\${str[i]}')
			}
		}
	}

	return buffer.bytestr()
}

struct ConstantData {
	offset int
	data   []u8
}

fn (mut g Gen) constant_data_intern_offset(data []u8) ?(int, int) {
	for d in g.constant_data {
		if d.data == data {
			return d.offset, d.data.len
		}
	}
	return none
}

// (offset, len)
fn (mut g Gen) allocate_string(node ast.StringLiteral) (int, int) {
	data := g.eval_escape_codes(node).bytes()

	// `-prod` will only intern strings.
	if g.pref.is_prod {
		if offset, len := g.constant_data_intern_offset(data) {
			return offset, len
		}
	}

	offset := g.constant_data_offset
	g.constant_data << ConstantData{
		offset: offset
		data: data
	}

	padding := (8 - offset % 8) % 8
	g.constant_data_offset += data.len + padding

	return offset, data.len
}
