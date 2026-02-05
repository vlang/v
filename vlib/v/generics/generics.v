module generics

// TODO do scopes need to be cloned?
import v.pref
import v.ast
import arrays
import strings

// Stage for solving generics

const result_name = ast.result_name
const option_name = ast.option_name

pub struct Generics {
	pref &pref.Preferences
pub mut:
	table                &ast.Table = unsafe { nil }
	file                 &ast.File  = unsafe { nil }
	styp_cache           map[ast.Type]string
	cur_fn               &ast.FnDecl = unsafe { nil }
	cur_concrete_types   []ast.Type
	inside_struct_init   bool
	cur_struct_init_node &ast.StructInit = unsafe { nil }
	forin_types          map[string]ast.Type // maps the name of the elem variable (`for elem in my_array`) to the solved type
}

pub fn new_generics(pref_ &pref.Preferences) &Generics {
	return &Generics{
		pref: pref_
	}
}

pub fn new_generics_with_table(table &ast.Table, pref_ &pref.Preferences) &Generics {
	mut g := new_generics(pref_)
	g.table = table
	return g
}

pub fn (mut g Generics) solve_files(ast_files []&ast.File) {
	for i in 0 .. ast_files.len {
		mut file := unsafe { ast_files[i] }
		g.solve(mut file)
	}
}

pub fn (mut g Generics) solve(mut ast_file ast.File) {
	g.file = ast_file
	ast_file.stmts = g.stmts(mut ast_file.stmts)
}

pub fn (mut g Generics) stmts(mut nodes []ast.Stmt) []ast.Stmt {
	mut solved_indexes := []int{}
	mut solved_generic_fns := []ast.Stmt{}
	for i, mut stmt in nodes {
		match mut stmt {
			ast.FnDecl {
				old_cur_fn := g.cur_fn
				g.cur_fn = unsafe { &stmt }
				if stmt.generic_names.len > 0 {
					solved_generic_fns << g.generic_fn_decl(mut stmt)
					solved_indexes << i
				} else {
					nodes[i] = g.stmt(mut stmt)
				}
				g.cur_fn = old_cur_fn
			}
			else {
				stmt = g.stmt(mut stmt)
			}
		}
	}
	for i in arrays.reverse_iterator(solved_indexes) {
		nodes.delete(*i)
	}
	nodes << solved_generic_fns
	return nodes
}

pub fn (mut g Generics) stmt(mut node ast.Stmt) ast.Stmt {
	match mut node {
		ast.EmptyStmt {}
		ast.NodeError {}
		ast.AsmStmt {}
		ast.DebuggerStmt {}
		ast.AssertStmt {
			if g.cur_concrete_types.len > 0 {
				return ast.Stmt(ast.AssertStmt{
					...node
					expr: g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.AssignStmt {
			if g.cur_concrete_types.len > 0 {
				mut right := node.right.clone()
				mut left := node.left.clone()
				return ast.Stmt(ast.AssignStmt{
					...node
					right:       g.exprs(mut right)
					left:        g.exprs(mut left)
					left_types:  node.left_types.map(g.unwrap_generic(it))
					right_types: node.right_types.map(g.unwrap_generic(it))
				})
			}
			node.right = g.exprs(mut node.right)
			node.left = g.exprs(mut node.left)
		}
		ast.Block {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				return ast.Stmt(ast.Block{
					...node
					stmts: g.stmts(mut stmts)
				})
			}
			node.stmts = g.stmts(mut node.stmts)
		}
		ast.BranchStmt {}
		ast.ComptimeFor {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				return ast.Stmt(ast.ComptimeFor{
					...node
					typ:   g.unwrap_generic(node.typ)
					stmts: g.stmts(mut stmts)
				})
			}
			node.stmts = g.stmts(mut node.stmts)
		}
		ast.ConstDecl {
			if g.cur_concrete_types.len > 0 {
				mut fields := node.fields.clone()
				for mut field in fields {
					field.typ = g.unwrap_generic(field.typ)
					field.expr = g.expr(mut field.expr)
				}
				return ast.Stmt(ast.ConstDecl{
					...node
					fields: fields
				})
			}
			for mut field in node.fields {
				field.expr = g.expr(mut field.expr)
			}
		}
		ast.DeferStmt {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				return ast.Stmt(ast.DeferStmt{
					...node
					stmts: g.stmts(mut stmts)
				})
			}
			node.stmts = g.stmts(mut node.stmts)
		}
		ast.EnumDecl {}
		ast.ExprStmt {
			if g.cur_concrete_types.len > 0 {
				return ast.Stmt(ast.ExprStmt{
					...node
					expr: g.expr(mut node.expr)
					typ:  g.unwrap_generic(node.typ)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.FnDecl {
			if g.cur_concrete_types.len > 0 {
				old_cur_fn := g.cur_fn
				g.cur_fn = unsafe { &node }
				mut params := node.params.clone()
				for mut param in params {
					param.typ = g.unwrap_generic(param.typ)
				}
				mut stmts := node.stmts.clone()
				stmts = g.stmts(mut stmts)
				mut defer_stmts := node.defer_stmts.clone()
				for mut defer_stmt in defer_stmts {
					defer_stmt = g.stmt(mut defer_stmt) as ast.DeferStmt
				}
				g.cur_fn = old_cur_fn
				return ast.Stmt(ast.FnDecl{
					...node
					receiver:    ast.StructField{
						...node.receiver
						typ: g.unwrap_generic(node.receiver.typ)
					}
					return_type: g.unwrap_generic(node.return_type)
					params:      params
					stmts:       stmts
					defer_stmts: defer_stmts
				})
			}
			old_cur_fn := g.cur_fn
			g.cur_fn = unsafe { &node }
			node.stmts = g.stmts(mut node.stmts)
			g.cur_fn = old_cur_fn
		}
		ast.ForCStmt {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				return ast.Stmt(ast.ForCStmt{
					...node
					cond:  g.expr(mut node.cond)
					init:  g.stmt(mut node.init)
					inc:   g.stmt(mut node.inc)
					stmts: g.stmts(mut stmts)
				})
			}
			if node.has_init && !node.is_multi {
				node.init = g.stmt(mut node.init)
			}

			if node.has_cond {
				node.cond = g.expr(mut node.cond)
			}

			node.stmts = g.stmts(mut node.stmts)

			if node.has_inc && !node.is_multi {
				node.inc = g.stmt(mut node.inc)
			}
		}
		ast.ForInStmt {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				if mut node.cond is ast.Ident && node.cond.ct_expr {
					// Solve the type for elem in `for elem in my_array` when my_array is T
					unwrapped_typ := g.unwrap_generic(node.cond_type)
					node.val_type = g.table.value_type(unwrapped_typ)
					if node.val_is_mut {
						node.val_type = node.val_type.ref()
					}
					g.forin_types[node.val_var] = node.val_type
					defer(fn) {
						g.forin_types.delete(node.val_var)
					}
				}
				mut new_node := ast.ForInStmt{
					...node
					cond:      g.expr(mut node.cond)
					high:      g.expr(mut node.high)
					key_type:  g.unwrap_generic(node.key_type)
					val_type:  g.unwrap_generic(node.val_type)
					cond_type: g.unwrap_generic(node.cond_type)
					high_type: g.unwrap_generic(node.high_type)
					stmts:     g.stmts(mut stmts)
				}
				return ast.Stmt(new_node)
			}
			node.stmts = g.stmts(mut node.stmts)
		}
		ast.ForStmt {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				return ast.Stmt(ast.ForStmt{
					...node
					cond:  g.expr(mut node.cond)
					stmts: g.stmts(mut stmts)
				})
			}
			node.cond = g.expr(mut node.cond)
			node.stmts = g.stmts(mut node.stmts)
		}
		ast.GlobalDecl {
			if g.cur_concrete_types.len > 0 {
				mut fields := node.fields.clone()
				for mut field in fields {
					field.typ = g.unwrap_generic(field.typ)
					field.expr = g.expr(mut field.expr)
				}
				return ast.Stmt(ast.GlobalDecl{
					...node
					fields: fields
				})
			}
			for mut field in node.fields {
				field.expr = g.expr(mut field.expr)
			}
		}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {
			if g.cur_concrete_types.len > 0 {
				mut ct_conds := node.ct_conds.clone()
				return ast.Stmt(ast.HashStmt{
					...node
					ct_conds: g.exprs(mut ct_conds)
				})
			}
			node.ct_conds = g.exprs(mut node.ct_conds)
		}
		ast.Import {}
		ast.InterfaceDecl {}
		ast.Module {}
		ast.Return {
			if g.cur_concrete_types.len > 0 {
				mut exprs := node.exprs.clone()
				return ast.Stmt(ast.Return{
					...node
					exprs: g.exprs(mut exprs)
					types: node.types.map(g.unwrap_generic(it))
				})
			}
			node.exprs = g.exprs(mut node.exprs)
		}
		ast.SemicolonStmt {}
		ast.SqlStmt {}
		ast.StructDecl {
			if g.cur_concrete_types.len > 0 {
				mut fields := node.fields.clone()
				for mut field in fields {
					field.container_typ = g.unwrap_generic(field.container_typ)
					field.default_expr_typ = g.unwrap_generic(field.default_expr_typ)
					field.typ = g.unwrap_generic(field.typ)
					field.unaliased_typ = g.unwrap_generic(field.unaliased_typ)
					field.default_expr = g.expr(mut field.default_expr)
				}
				return ast.Stmt(ast.StructDecl{
					...node
					fields: fields
				})
			}
			for mut field in node.fields {
				field.default_expr = g.expr(mut field.default_expr)
			}
		}
		ast.TypeDecl {}
	}
	return node
}

pub fn (mut g Generics) styp(t ast.Type) string {
	if !t.has_option_or_result() {
		return g.base_type(t)
	} else if t.has_flag(.option) {
		// Register an optional if it's not registered yet
		return g.register_option(t)
	} else {
		return g.register_result(t)
	}
}

// incomplete implementation: TODO
fn (mut g Generics) base_type(_t ast.Type) string {
	t := g.unwrap_generic(_t)
	if styp := g.styp_cache[t] {
		return styp
	}
	if g.pref.nofloat {
		if t == ast.f32_type {
			return 'u32'
		} else if t == ast.f64_type {
			return 'u64'
		}
	}
	share := t.share()
	mut styp := if share == .atomic_t { t.atomic_typename() } else { g.cc_type(t, true) }
	nr_muls := t.nr_muls()
	if nr_muls > 0 {
		styp += strings.repeat(`*`, nr_muls)
	}
	g.styp_cache[t] = styp
	return styp
}

// incomplete implementation: TODO
fn (mut g Generics) register_option(t ast.Type) string {
	styp, _ := g.option_type_name(t)
	return if !t.has_flag(.option_mut_param_t) { styp } else { '${styp}*' }
}

// incomplete implementation: TODO
fn (mut g Generics) register_result(t ast.Type) string {
	styp, _ := g.result_type_name(t)
	return styp
}

fn (mut g Generics) option_type_name(t ast.Type) (string, string) {
	mut base := g.base_type(t)
	mut styp := ''
	sym := g.table.sym(t)
	if sym.info is ast.FnType {
		base = 'anon_fn_${g.table.fn_type_signature(sym.info.func)}'
	}
	if sym.language == .c && sym.kind == .struct {
		styp = '${option_name}_${base.replace(' ', '_')}'
	} else {
		styp = '${option_name}_${base}'
	}
	if t.has_flag(.generic) || t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

fn (mut g Generics) result_type_name(t ast.Type) (string, string) {
	mut base := g.base_type(t)
	if t.has_flag(.option) {
		g.register_option(t)
		base = '_option_' + base
	}
	mut styp := ''
	sym := g.table.sym(t)
	if sym.info is ast.FnType {
		base = 'anon_fn_${g.table.fn_type_signature(sym.info.func)}'
	}
	if sym.language == .c && sym.kind == .struct {
		styp = '${result_name}_${base.replace(' ', '_')}'
	} else {
		styp = '${result_name}_${base}'
	}
	if t.has_flag(.generic) || t.is_ptr() {
		styp = styp.replace('*', '_ptr')
	}
	return styp, base
}

// cc_type whether to prefix 'struct' or not (C__Foo -> struct Foo)
fn (mut g Generics) cc_type(typ ast.Type, is_prefix_struct bool) string {
	sym := g.table.sym(g.unwrap_generic(typ))
	mut styp := sym.scoped_cname()
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.is_generic && sym.generic_types.len == 0 {
				mut sgtyps := '_T'
				for gt in sym.info.generic_types {
					gts := g.table.sym(g.unwrap_generic(gt))
					sgtyps += '_${gts.cname}'
				}
				styp += sgtyps
			}
		}
		else {}
	}
	if is_prefix_struct && sym.language == .c {
		styp = styp[3..]
		if sym.kind == .struct {
			info := sym.info as ast.Struct
			if info.is_anon {
				styp = 'C__' + styp
			} else if !info.is_typedef {
				styp = 'struct ${styp}'
			}
		}
	}
	return styp
}

pub fn (mut g Generics) method_concrete_name(old_name string, concrete_types []ast.Type, receiver_type ast.Type) string {
	mut name := old_name
	if receiver_type != 0 {
		mut info := g.table.sym(g.unwrap_generic(receiver_type)).info
		if mut info is ast.Alias {
			info = g.table.sym(g.table.unaliased_type(g.unwrap_generic(receiver_type))).info
		}
		if mut info is ast.Struct {
			fn_conc_types := concrete_types#[info.generic_types.len..] // concrete types without the generic types of the struct

			if fn_conc_types.len > 0 {
				name += '_T'
			}
			for typ in fn_conc_types {
				name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) +
					g.styp(typ.set_nr_muls(0))
			}
			return name
		} else if mut info is ast.Interface {
			return name
		}
	}
	if concrete_types.len > 0 {
		name += '_T'
	}
	for typ in concrete_types {
		name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) + g.styp(typ.set_nr_muls(0))
	}
	return name
}

pub fn (mut g Generics) concrete_name(old_name string, concrete_types []ast.Type) string {
	mut name := old_name
	if concrete_types.len > 0 {
		name += '_T'
	}
	for typ in concrete_types {
		name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) + g.styp(typ.set_nr_muls(0))
	}
	return name
}

pub fn (mut g Generics) generic_fn_decl(mut node ast.FnDecl) []ast.Stmt {
	mut solved_fns := []ast.Stmt{}
	nkey := node.fkey()
	generic_types_by_fn := g.table.fn_generic_types[nkey]
	for concrete_types in generic_types_by_fn {
		if g.pref.is_verbose {
			syms := concrete_types.map(g.table.sym(it))
			the_type := syms.map(it.name).join(', ')
			println('solve generic fn `${node.name}` for type `${the_type}`')
		}
		g.cur_concrete_types = concrete_types

		mut new_node := ast.FnDecl{
			...node
		}
		new_node = g.stmt(mut new_node) as ast.FnDecl
		new_node = ast.FnDecl{
			...new_node
			name:          if node.is_method {
				g.method_concrete_name(new_node.name, concrete_types, new_node.receiver.typ)
			} else {
				g.concrete_name(new_node.name, concrete_types)
			}
			ninstances:    0
			generic_names: []
		}
		if new_node.is_method {
			mut sym := g.table.sym(new_node.receiver.typ)
			func := ast.Fn{
				is_variadic:                    new_node.is_variadic
				is_c_variadic:                  new_node.is_c_variadic
				language:                       .v
				is_pub:                         new_node.is_pub
				is_deprecated:                  new_node.is_deprecated
				is_noreturn:                    new_node.is_noreturn
				is_unsafe:                      new_node.is_unsafe
				is_must_use:                    new_node.is_must_use
				is_keep_alive:                  new_node.is_keep_alive
				is_method:                      new_node.is_method
				is_static_type_method:          new_node.is_static_type_method
				no_body:                        new_node.no_body
				is_file_translated:             new_node.is_file_translated
				mod:                            new_node.mod
				file:                           new_node.file
				file_mode:                      new_node.file_mode
				pos:                            new_node.pos
				return_type_pos:                new_node.return_type_pos
				return_type:                    new_node.return_type
				receiver_type:                  new_node.receiver.typ
				name:                           new_node.name
				params:                         new_node.params
				generic_names:                  []
				is_conditional:                 new_node.is_conditional
				ctdefine_idx:                   new_node.ctdefine_idx
				is_expand_simple_interpolation: new_node.is_expand_simple_interpolation
			}
			g.table.find_or_register_fn_type(func, false, true)
			sym.register_method(func)
		}
		solved_fns << new_node
	}
	g.cur_concrete_types = []
	return solved_fns
}

pub fn (mut g Generics) exprs(mut nodes []ast.Expr) []ast.Expr {
	for mut e in nodes {
		e = g.expr(mut e)
	}
	return nodes
}

pub fn (mut g Generics) expr(mut node ast.Expr) ast.Expr {
	match mut node {
		ast.AnonFn {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.AnonFn{
					...node
					typ:  g.unwrap_generic(node.typ)
					decl: g.stmt(mut node.decl) as ast.FnDecl
				})
			}
			node.decl = g.stmt(mut node.decl) as ast.FnDecl
		}
		ast.ArrayDecompose {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.ArrayDecompose{
					...node
					expr_type: g.unwrap_generic(node.expr_type)
					arg_type:  g.unwrap_generic(node.arg_type)
					expr:      g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.ArrayInit {
			if g.cur_concrete_types.len > 0 {
				mut exprs := node.exprs.clone()
				return ast.Expr(ast.ArrayInit{
					...node
					exprs:      g.exprs(mut exprs)
					expr_types: node.expr_types.map(g.unwrap_generic(it))
					typ:        g.unwrap_generic(node.typ)
					elem_type:  g.unwrap_generic(node.elem_type)
					alias_type: g.unwrap_generic(node.alias_type)
					len_expr:   g.expr(mut node.len_expr)
					cap_expr:   g.expr(mut node.cap_expr)
					init_type:  g.unwrap_generic(node.init_type)
					init_expr:  g.expr(mut node.init_expr)
				})
			}
			node.exprs = g.exprs(mut node.exprs)
			if node.has_len {
				node.len_expr = g.expr(mut node.len_expr)
			}
			if node.has_cap {
				node.cap_expr = g.expr(mut node.cap_expr)
			}
			if node.has_init {
				node.init_expr = g.expr(mut node.init_expr)
			}
		}
		ast.AsCast {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.AsCast{
					...node
					typ:       g.unwrap_generic(node.typ)
					expr_type: g.unwrap_generic(node.expr_type)
					expr:      g.expr(mut node.expr)
				})
			} else {
				node.expr = g.expr(mut node.expr)
			}
		}
		ast.CallExpr {
			if g.cur_concrete_types.len > 0 {
				mut args := node.args.clone()
				mut all_concrete_types := node.concrete_types.clone()
				for mut ct in all_concrete_types {
					idx := g.cur_fn.generic_names.index(g.table.type_str(ct))
					if idx != -1 {
						ct = g.cur_concrete_types[idx]
					}
				}
				for i, mut arg in args {
					if arg.typ.has_flag(.generic) {
						arg.typ = g.unwrap_generic(arg.typ)
						arg.ct_expr = false
					}
					arg.expr = g.expr(mut arg.expr)
					if mut arg.expr is ast.Ident {
						// Solve concrete_types when the type of one argument was elem in `for elem in my_array` when my_array is T
						forin_type := g.forin_types[arg.expr.name]
						if forin_type != 0 {
							if func := g.table.find_fn(node.name) {
								solved_type_generic_name := g.table.type_str(func.params[i].typ)
								idx := func.generic_names.index(solved_type_generic_name)
								if idx != -1 {
									all_concrete_types[idx] = forin_type
								}
							}
						}
					}
				}
				mut receiver_type := g.unwrap_generic(node.receiver_type)
				if receiver_type.has_flag(.generic) {
					receiver_type = receiver_type.clear_flag(.generic)
				}
				return ast.Expr(ast.CallExpr{
					...node
					name:                if node.is_method {
						g.method_concrete_name(node.name, all_concrete_types, node.receiver_type)
					} else {
						g.concrete_name(node.name, all_concrete_types)
					}
					left_type:           g.unwrap_generic(node.left_type)
					receiver_type:       receiver_type
					return_type:         g.unwrap_generic(node.return_type)
					return_type_generic: ast.no_type
					fn_var_type:         g.unwrap_generic(node.fn_var_type)
					left:                g.expr(mut node.left)
					expected_arg_types:  node.expected_arg_types.map(g.unwrap_generic(it))
					args:                args
					concrete_types:      []
					raw_concrete_types:  node.raw_concrete_types.clone()
					from_embed_types:    node.from_embed_types.clone()
					or_block:            g.expr(mut node.or_block) as ast.OrExpr
				})
			}
			node.left = g.expr(mut node.left)
			for mut arg in node.args {
				arg.expr = g.expr(mut arg.expr)
			}
			node.or_block = g.expr(mut node.or_block) as ast.OrExpr
			if node.is_method && g.table.sym(node.receiver_type).info is ast.Alias {
				// Workaround needed for markused
				unaliased_type := g.table.unaliased_type(g.unwrap_generic(node.receiver_type))
				if g.table.sym(unaliased_type).has_method(node.name) {
					node.receiver_type = unaliased_type
				}
			}
			if node.receiver_type.has_flag(.generic) {
				node.receiver_type = node.receiver_type.clear_flag(.generic)
			}
			if node.concrete_types.len > 0 {
				if func := g.table.find_fn(node.name) {
					node.expected_arg_types = node.expected_arg_types.map(g.table.convert_generic_type(it,
						func.generic_names, node.concrete_types) or { it })
				}
			}
			node.name = if node.is_method {
				g.method_concrete_name(node.name, node.concrete_types, node.receiver_type)
			} else {
				g.concrete_name(node.name, node.concrete_types)
			}
			return ast.Expr(ast.CallExpr{
				...node
				concrete_types:     []
				raw_concrete_types: []
			})
		}
		ast.CastExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.CastExpr{
					...node
					typ:  g.unwrap_generic(node.typ)
					arg:  g.expr(mut node.arg)
					expr: g.expr(mut node.expr)
				})
			}
			node.arg = g.expr(mut node.arg)
			node.expr = g.expr(mut node.expr)
		}
		ast.ChanInit {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.ChanInit{
					...node
					typ:       g.unwrap_generic(node.typ)
					elem_type: g.unwrap_generic(node.typ)
					cap_expr:  g.expr(mut node.cap_expr)
				})
			}
			node.cap_expr = g.expr(mut node.cap_expr)
		}
		ast.ComptimeCall {
			if g.cur_concrete_types.len > 0 {
				mut args := node.args.clone()
				for mut arg in args {
					arg.expr = g.expr(mut arg.expr)
				}
				return ast.Expr(ast.ComptimeCall{
					...node
					left_type:   g.unwrap_generic(node.left_type)
					result_type: g.unwrap_generic(node.result_type)
					args:        args
				})
			}
			for mut arg in node.args {
				arg.expr = g.expr(mut arg.expr)
			}
		}
		ast.ComptimeSelector {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.ComptimeSelector{
					...node
					left_type:  g.unwrap_generic(node.left_type)
					typ:        g.unwrap_generic(node.typ)
					left:       g.expr(mut node.left)
					field_expr: g.expr(mut node.field_expr)
				})
			}
			node.left = g.expr(mut node.left)
			node.field_expr = g.expr(mut node.field_expr)
		}
		ast.ConcatExpr {
			if g.cur_concrete_types.len > 0 {
				mut vals := node.vals.clone()
				for mut val in vals {
					val = g.expr(mut val)
				}
				return ast.Expr(ast.ConcatExpr{
					...node
					return_type: g.unwrap_generic(node.return_type)
					vals:        vals
				})
			}
			for mut val in node.vals {
				val = g.expr(mut val)
			}
		}
		ast.DumpExpr {
			if g.cur_concrete_types.len > 0 {
				name := if mut node.expr is ast.Ident {
					// var
					if node.expr.info is ast.IdentVar && node.expr.language == .v {
						g.styp(g.unwrap_generic(node.expr.info.typ.clear_flags(.shared_f,
							.result))).replace('*', '')
					} else {
						node.cname
					}
				} else if mut node.expr is ast.CallExpr {
					g.styp(g.unwrap_generic(node.expr_type.clear_flags(.shared_f, .result))).replace('*',
						'').replace('.', '__')
				} else {
					node.cname
				}
				return ast.Expr(ast.DumpExpr{
					...node
					cname:     name
					expr_type: g.unwrap_generic(node.expr_type)
					expr:      g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.Ident {
			match mut node.obj {
				ast.Var {
					if g.cur_concrete_types.len > 0 {
						is_ct_type_generic := node.obj.ct_type_var == .generic_param
							|| node.obj.ct_type_var == .generic_var
						is_ct_type_forin_val := node.obj.ct_type_var == .value_var // elem in `for elem in my_array` when my_array is T
						unwrapped_typ := if is_ct_type_forin_val {
							g.forin_types[node.name]
						} else {
							g.unwrap_generic(node.obj.typ)
						}
						info := match node.info {
							ast.IdentVar {
								ast.IdentInfo(ast.IdentVar{
									...node.info as ast.IdentVar
									typ: g.unwrap_generic(node.info.typ)
								})
							}
							ast.IdentFn {
								ast.IdentInfo(ast.IdentFn{
									...node.info as ast.IdentFn
									typ: g.unwrap_generic(node.info.typ)
								})
							}
						}
						return ast.Expr(ast.Ident{
							...node
							obj:     ast.Var{
								...node.obj
								typ:           unwrapped_typ
								smartcasts:    node.obj.smartcasts.map(g.unwrap_generic(it))
								is_auto_deref: node.obj.is_auto_deref
									|| (node.obj.typ.has_flag(.generic)
									&& g.table.sym(unwrapped_typ).kind in [.sum_type, .interface])
								// node.obj.orig_type = g.unwrap_generic(node.obj.orig_type)
								ct_type_unwrapped: is_ct_type_generic || is_ct_type_forin_val
									|| node.obj.ct_type_unwrapped
								ct_type_var:       if is_ct_type_generic || is_ct_type_forin_val {
									.no_comptime
								} else {
									node.obj.ct_type_var
								}
							}
							info:    info
							ct_expr: !is_ct_type_forin_val && node.ct_expr
						})
					}
				}
				ast.EmptyScopeObject {
					if g.cur_concrete_types.len > 0 {
						mut typ := node.obj.typ
						mut name := node.name
						if typ == 0 {
							if g.cur_fn != unsafe { nil } {
								idx := g.cur_fn.generic_names.index(node.name)
								if idx != -1 {
									typ = g.cur_concrete_types[idx]
									name = g.table.type_str(typ)
								}
							}
						} else {
							typ = g.unwrap_generic(typ)
							name = g.table.type_str(typ)
						}
						return ast.Expr(ast.Ident{
							...node
							obj:  ast.EmptyScopeObject{
								...node.obj
								typ:  typ
								name: name
							}
							name: name
						})
					}
				}
				else {}
			}
		}
		ast.IfExpr {
			if g.cur_concrete_types.len > 0 {
				mut branches := node.branches.clone()
				for mut branch in branches {
					branch.stmts = branch.stmts.clone()
					branch.cond = g.expr(mut branch.cond)
					branch.stmts = g.stmts(mut branch.stmts)
				}
				return ast.Expr(ast.IfExpr{
					...node
					typ:      g.unwrap_generic(node.typ)
					branches: branches
					left:     g.expr(mut node.left)
				})
			}
			for mut branch in node.branches {
				branch.cond = g.expr(mut branch.cond)
				branch.stmts = g.stmts(mut branch.stmts)
			}
			node.left = g.expr(mut node.left)
		}
		ast.IfGuardExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.IfGuardExpr{
					...node
					expr_type: g.unwrap_generic(node.expr_type)
					expr:      g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.IndexExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.IndexExpr{
					...node
					left_type: g.unwrap_generic(node.left_type)
					typ:       g.unwrap_generic(node.typ)
					left:      g.expr(mut node.left)
					index:     g.expr(mut node.index)
					or_expr:   g.expr(mut node.or_expr) as ast.OrExpr
				})
			}
			node.left = g.expr(mut node.left)
			node.index = g.expr(mut node.index)
			node.or_expr = g.expr(mut node.or_expr) as ast.OrExpr
		}
		ast.InfixExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.InfixExpr{
					...node
					left_type:     g.unwrap_generic(node.left_type)
					right_type:    g.unwrap_generic(node.right_type)
					promoted_type: g.unwrap_generic(node.promoted_type)
					left:          g.expr(mut node.left)
					right:         g.expr(mut node.right)
				})
			}
			node.left = g.expr(mut node.left)
			node.right = g.expr(mut node.right)
		}
		ast.IsRefType {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.IsRefType{
					...node
					typ:  g.unwrap_generic(node.typ)
					expr: g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.Likely {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.Likely{
					...node
					expr: g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.LockExpr {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				mut lockeds := node.lockeds.clone()
				return ast.Expr(ast.LockExpr{
					...node
					typ:     g.unwrap_generic(node.typ)
					stmts:   g.stmts(mut stmts)
					lockeds: g.exprs(mut lockeds)
				})
			}
			node.stmts = g.stmts(mut node.stmts)
			node.lockeds = g.exprs(mut node.lockeds)
		}
		ast.MapInit {
			if g.cur_concrete_types.len > 0 {
				mut keys := node.keys.clone()
				mut vals := node.vals.clone()
				return ast.Expr(ast.MapInit{
					typ:        g.unwrap_generic(node.typ)
					key_type:   g.unwrap_generic(node.key_type)
					value_type: g.unwrap_generic(node.value_type)
					val_types:  node.val_types.map(g.unwrap_generic(it))
					keys:       g.exprs(mut keys)
					vals:       g.exprs(mut vals)
				})
			}
			node.keys = g.exprs(mut node.keys)
			node.vals = g.exprs(mut node.vals)
		}
		ast.MatchExpr {
			if g.cur_concrete_types.len > 0 {
				mut branches := node.branches.clone()
				for mut branch in branches {
					branch.stmts = branch.stmts.clone()
					branch.exprs = branch.exprs.clone()
					branch.exprs = g.exprs(mut branch.exprs)
					branch.stmts = g.stmts(mut branch.stmts)
				}
				return ast.Expr(ast.MatchExpr{
					...node
					branches:      branches
					return_type:   g.unwrap_generic(node.return_type)
					cond_type:     g.unwrap_generic(node.cond_type)
					cond:          g.expr(mut node.cond)
					expected_type: g.unwrap_generic(node.expected_type)
				})
			}
			node.cond = g.expr(mut node.cond)
			for mut branch in node.branches {
				branch.exprs = g.exprs(mut branch.exprs)
				branch.stmts = g.stmts(mut branch.stmts)
			}
		}
		ast.OrExpr {
			if g.cur_concrete_types.len > 0 {
				mut stmts := node.stmts.clone()
				return ast.Expr(ast.OrExpr{
					...node
					stmts: g.stmts(mut stmts)
				})
			}
			node.stmts = g.stmts(mut node.stmts)
		}
		ast.ParExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.ParExpr{
					...node
					expr: g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.PostfixExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.PostfixExpr{
					...node
					expr: g.expr(mut node.expr)
					typ:  g.unwrap_generic(node.typ)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.PrefixExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.PrefixExpr{
					...node
					right_type: g.unwrap_generic(node.right_type)
					right:      g.expr(mut node.right)
					or_block:   g.expr(mut node.or_block) as ast.OrExpr
				})
			}
			node.right = g.expr(mut node.right)
			node.or_block = g.expr(mut node.or_block) as ast.OrExpr
		}
		ast.RangeExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.RangeExpr{
					...node
					typ:  g.unwrap_generic(node.typ)
					low:  g.expr(mut node.low)
					high: g.expr(mut node.high)
				})
			}
			node.low = g.expr(mut node.low)
			node.high = g.expr(mut node.high)
		}
		ast.SelectExpr {
			if g.cur_concrete_types.len > 0 {
				mut branches := node.branches.clone()
				for mut branch in node.branches {
					branch.stmt = g.stmt(mut branch.stmt)
					branch.stmts = branch.stmts.clone()
					branch.stmts = g.stmts(mut branch.stmts)
				}
				return ast.Expr(ast.SelectExpr{
					...node
					expected_type: g.unwrap_generic(node.expected_type)
					branches:      branches
				})
			}
			for mut branch in node.branches {
				branch.stmt = g.stmt(mut branch.stmt)
				branch.stmts = g.stmts(mut branch.stmts)
			}
		}
		ast.SelectorExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.SelectorExpr{
					...node
					expr:             g.expr(mut node.expr)
					expr_type:        g.unwrap_generic(node.expr_type)
					typ:              g.unwrap_generic(node.typ)
					name_type:        g.unwrap_generic(node.name_type)
					from_embed_types: node.from_embed_types.map(g.unwrap_generic(it))
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.SizeOf {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.SizeOf{
					...node
					expr: g.expr(mut node.expr)
					typ:  g.unwrap_generic(node.typ)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.SqlExpr {
			if g.cur_concrete_types.len > 0 {
				mut fields := node.fields.clone()
				for mut field in fields {
					field.default_expr = g.expr(mut field.default_expr)
				}
				mut sub_structs := node.sub_structs.clone()
				for _, mut sub_struct in sub_structs {
					sub_struct = g.expr(mut sub_struct) as ast.SqlExpr
				}
				return ast.Expr(ast.SqlExpr{
					...node
					typ:         g.unwrap_generic(node.typ)
					db_expr:     g.expr(mut node.db_expr)
					where_expr:  g.expr(mut node.where_expr)
					order_expr:  g.expr(mut node.order_expr)
					limit_expr:  g.expr(mut node.limit_expr)
					offset_expr: g.expr(mut node.offset_expr)
					fields:      fields
					sub_structs: sub_structs
				})
			}
			node.db_expr = g.expr(mut node.db_expr)
			if node.has_where {
				node.where_expr = g.expr(mut node.where_expr)
			}
			if node.has_order {
				node.order_expr = g.expr(mut node.order_expr)
			}
			if node.has_limit {
				node.limit_expr = g.expr(mut node.limit_expr)
			}
			if node.has_offset {
				node.offset_expr = g.expr(mut node.offset_expr)
			}
			for mut field in node.fields {
				field.default_expr = g.expr(mut field.default_expr)
			}
			for _, mut sub_struct in node.sub_structs {
				sub_struct = g.expr(mut sub_struct) as ast.SqlExpr
			}
		}
		ast.StringInterLiteral {
			if g.cur_concrete_types.len > 0 {
				mut exprs := node.exprs.clone()
				return ast.Expr(ast.StringInterLiteral{
					...node
					exprs:      g.exprs(mut exprs)
					expr_types: node.expr_types.map(g.unwrap_generic(it))
				})
			}
			node.exprs = g.exprs(mut node.exprs)
		}
		ast.StructInit {
			if g.cur_concrete_types.len > 0 {
				old_inside_struct_init := g.inside_struct_init
				g.inside_struct_init = true
				old_cur_struct_init_node := g.cur_struct_init_node
				g.cur_struct_init_node = unsafe { &node }

				mut init_fields := node.init_fields.clone()
				for mut init_field in init_fields {
					init_field.expr = g.expr(mut init_field.expr)
					init_field.typ = g.unwrap_generic(init_field.typ)
					init_field.expected_type = g.unwrap_generic(init_field.expected_type)
					init_field.parent_type = g.unwrap_generic(init_field.parent_type)
				}

				out := ast.Expr(ast.StructInit{
					...node
					typ:              g.unwrap_generic(node.typ)
					typ_str:          g.table.type_str(g.unwrap_generic(node.typ))
					generic_types:    node.generic_types.map(g.unwrap_generic(it))
					update_expr:      g.expr(mut node.update_expr)
					update_expr_type: g.unwrap_generic(node.update_expr_type)
					init_fields:      init_fields
				})

				g.cur_struct_init_node = old_cur_struct_init_node
				g.inside_struct_init = old_inside_struct_init
				return out
			}
			old_inside_struct_init := g.inside_struct_init
			g.inside_struct_init = true
			node.update_expr = g.expr(mut node.update_expr)
			for mut init_field in node.init_fields {
				init_field.expr = g.expr(mut init_field.expr)
			}
			g.inside_struct_init = old_inside_struct_init
		}
		ast.TypeNode {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.TypeNode{
					...node
					typ:  g.unwrap_generic(node.typ)
					stmt: g.stmt(mut node.stmt)
				})
			}
			node.stmt = g.stmt(mut node.stmt)
		}
		ast.TypeOf {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.TypeOf{
					...node
					typ:  g.unwrap_generic(node.typ)
					expr: g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		ast.UnsafeExpr {
			if g.cur_concrete_types.len > 0 {
				return ast.Expr(ast.UnsafeExpr{
					...node
					expr: g.expr(mut node.expr)
				})
			}
			node.expr = g.expr(mut node.expr)
		}
		else {}
	}
	return node
}

fn (mut g Generics) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 {
			if t_typ := g.table.convert_generic_type(typ, g.cur_fn.generic_names, g.cur_concrete_types) {
				return t_typ
			}
		}
		if g.inside_struct_init && g.cur_struct_init_node != unsafe { nil } {
			if g.cur_struct_init_node.typ != 0 {
				sym := g.table.sym(g.cur_struct_init_node.typ)
				if sym.info is ast.Struct {
					if sym.info.generic_types.len > 0 {
						generic_names := sym.info.generic_types.map(g.table.sym(it).name)
						if t_typ := g.table.convert_generic_type(typ, generic_names, g.cur_struct_init_node.generic_types.map(g.unwrap_generic(it))) {
							return t_typ
						}
					}
				}
			}
		} else if g.table.sym(typ).kind == .struct {
			// resolve selector `a.foo` where `a` is struct[T] on non generic function
			sym := g.table.sym(typ)
			if sym.info is ast.Struct {
				if sym.info.generic_types.len > 0 {
					generic_names := sym.info.generic_types.map(g.table.sym(it).name)
					if t_typ := g.table.convert_generic_type(typ, generic_names, sym.info.concrete_types) {
						return t_typ
					}

					if t_typ := g.table.convert_generic_type(typ, generic_names, g.cur_concrete_types) {
						return t_typ
					}
				}
			}
		}
	}
	return typ
}
