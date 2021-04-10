// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import v.ast
import v.util
import v.pref

fn (mut g Gen) comptime_selector(node ast.ComptimeSelector) {
	g.expr(node.left)
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	// check for field.name
	if node.field_expr is ast.SelectorExpr {
		if node.field_expr.expr is ast.Ident {
			if node.field_expr.expr.name == g.comp_for_field_var
				&& node.field_expr.field_name == 'name' {
				g.write(g.comp_for_field_value.name)
				return
			}
		}
	}
	g.expr(node.field_expr)
}

fn (mut g Gen) comptime_call(node ast.ComptimeCall) {
	if node.is_embed {
		// $embed_file('/path/to/file')
		g.gen_embed_file_init(node)
		return
	}
	if node.method_name == 'env' {
		// $env('ENV_VAR_NAME')
		val := util.cescaped_path(os.getenv(node.args_var))
		g.write('_SLIT("$val")')
		return
	}
	if node.is_vweb {
		is_html := node.method_name == 'html'
		for stmt in node.vweb_tmpl.stmts {
			if stmt is ast.FnDecl {
				// insert stmts from vweb_tmpl fn
				if stmt.name.starts_with('main.vweb_tmpl') {
					if is_html {
						g.inside_vweb_tmpl = true
					}
					g.stmts(stmt.stmts)
					g.inside_vweb_tmpl = false
					break
				}
			}
		}
		if is_html {
			// return vweb html template
			g.writeln('vweb__Context_html(&app->Context, _tmpl_res_$g.fn_decl.name); strings__Builder_free(&sb); string_free(&_tmpl_res_$g.fn_decl.name);')
		} else {
			// return $tmpl string
			fn_name := g.fn_decl.name.replace('.', '__')
			g.writeln('return _tmpl_res_$fn_name;')
		}
		return
	}
	g.writeln('// \$method call. sym="$node.sym.name"')
	if node.method_name == 'method' {
		// `app.$method()`
		m := node.sym.find_method(g.comp_for_method) or { return }
		/*
		vals := m.attrs[0].split('/')
		args := vals.filter(it.starts_with(':')).map(it[1..])
		println(vals)
		for val in vals {
		}
		*/
		expand_strs := if node.args.len > 0 && m.params.len - 1 >= node.args.len {
			arg := node.args[node.args.len - 1]
			param := m.params[node.args.len]

			arg.expr is ast.Ident && g.table.type_to_str(arg.typ) == '[]string'
				&& g.table.type_to_str(param.typ) != '[]string'
		} else {
			false
		}
		// check argument length and types
		if m.params.len - 1 != node.args.len && !expand_strs {
			// do not generate anything if the argument lengths don't match
			g.writeln('/* skipping ${node.sym.name}.$m.name due to mismatched arguments list */')
			// verror('expected ${m.params.len-1} arguments to method ${node.sym.name}.$m.name, but got $node.args.len')
			return
		}
		// TODO: check argument types
		g.write('${util.no_dots(node.sym.name)}_${g.comp_for_method}(')

		// try to see if we need to pass a pointer
		if node.left is ast.Ident {
			scope := g.file.scope.innermost(node.pos.pos)
			if v := scope.find_var(node.left.name) {
				if m.params[0].typ.is_ptr() && !v.typ.is_ptr() {
					g.write('&')
				}
			}
		}
		g.expr(node.left)
		if m.params.len > 1 {
			g.write(', ')
		}
		for i in 1 .. m.params.len {
			if node.left is ast.Ident {
				if m.params[i].name == node.left.name {
					continue
				}
			}
			if i - 1 < node.args.len - 1 {
				g.expr(node.args[i - 1].expr)
				g.write(', ')
			} else if !expand_strs && i == node.args.len {
				g.expr(node.args[i - 1].expr)
				break
			} else {
				// last argument; try to expand if it's []string
				idx := i - node.args.len
				if m.params[i].typ.is_int() || m.params[i].typ.idx() == ast.bool_type_idx {
					// Gets the type name and cast the string to the type with the string_<type> function
					type_name := g.table.type_symbols[int(m.params[i].typ)].str()
					g.write('string_${type_name}(((string*)${node.args[node.args.len - 1]}.data) [$idx])')
				} else {
					g.write('((string*)${node.args[node.args.len - 1]}.data) [$idx] ')
				}
				if i < m.params.len - 1 {
					g.write(', ')
				}
			}
		}
		g.write(' ); // vweb action call with args')
		return
	}
	mut j := 0
	for method in node.sym.methods {
		// if method.return_type != ast.void_type {
		if method.return_type != node.result_type {
			continue
		}
		if method.params.len != 1 {
			continue
		}
		// receiver := method.args[0]
		// if !p.expr_var.ptr {
		// p.error('`$p.expr_var.name` needs to be a reference')
		// }
		amp := '' // if receiver.is_mut && !p.expr_var.ptr { '&' } else { '' }
		if node.is_vweb {
			if j > 0 {
				g.write(' else ')
			}
			g.write('if (string_eq($node.method_name, _SLIT("$method.name"))) ')
		}
		g.write('${util.no_dots(node.sym.name)}_${method.name}($amp ')
		g.expr(node.left)
		g.writeln(');')
		j++
	}
}

fn cgen_attrs(attrs []ast.Attr) []string {
	mut res := []string{cap: attrs.len}
	for attr in attrs {
		// we currently don't quote 'arg' (otherwise we could just use `s := attr.str()`)
		mut s := attr.name
		if attr.arg.len > 0 {
			s += ': $attr.arg'
		}
		res << '_SLIT("$s")'
	}
	return res
}

fn (mut g Gen) comp_at(node ast.AtExpr) {
	if node.kind == .vmod_file {
		val := cnewlines(node.val.replace('\r', ''))
		g.write('_SLIT("$val")')
	} else {
		val := node.val.replace('\\', '\\\\')
		g.write('_SLIT("$val")')
	}
}

fn (mut g Gen) comp_if(node ast.IfExpr) {
	if !node.is_expr && !node.has_else && node.branches.len == 1 {
		if node.branches[0].stmts.len == 0 {
			// empty ifdef; result of target OS != conditional => skip
			return
		}
		if !g.pref.output_cross_c {
			if node.branches[0].cond is ast.Ident {
				if g.pref.os == (pref.os_from_string(node.branches[0].cond.name) or {
					pref.OS._auto
				}) {
					// Same target OS as the conditional...
					// => skip the #if defined ... #endif wrapper
					// and just generate the branch statements:
					g.indent--
					g.stmts(node.branches[0].stmts)
					g.indent++
					return
				}
			}
		}
	}
	line := if node.is_expr {
		stmt_str := g.go_before_stmt(0)
		g.write(util.tabs(g.indent))
		stmt_str.trim_space()
	} else {
		''
	}
	mut comp_if_stmts_skip := false
	for i, branch in node.branches {
		start_pos := g.out.len
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('#else')
			comp_if_stmts_skip = false
		} else {
			if i == 0 {
				g.write('#if ')
			} else {
				g.write('#elif ')
			}
			comp_if_stmts_skip = !g.comp_if_cond(branch.cond)
			g.writeln('')
		}
		expr_str := g.out.last_n(g.out.len - start_pos).trim_space()
		g.defer_ifdef = expr_str
		if node.is_expr {
			len := branch.stmts.len
			if len > 0 {
				last := branch.stmts[len - 1] as ast.ExprStmt
				if len > 1 {
					tmp := g.new_tmp_var()
					styp := g.typ(last.typ)
					g.indent++
					g.writeln('$styp $tmp;')
					g.writeln('{')
					g.stmts(branch.stmts[0..len - 1])
					g.write('\t$tmp = ')
					g.stmt(last)
					g.writeln('}')
					g.indent--
					g.writeln('$line $tmp;')
				} else {
					g.write('$line ')
					g.stmt(last)
				}
			}
		} else {
			// Only wrap the contents in {} if we're inside a function, not on the top level scope
			should_create_scope := g.fn_decl != 0
			if should_create_scope {
				g.writeln('{')
			}
			if !comp_if_stmts_skip {
				g.stmts(branch.stmts)
			}
			if should_create_scope {
				g.writeln('}')
			}
		}
		g.defer_ifdef = ''
	}
	g.writeln('#endif')
}

fn (mut g Gen) comp_if_cond(cond ast.Expr) bool {
	match cond {
		ast.BoolLiteral {
			g.expr(cond)
			return true
		}
		ast.ParExpr {
			g.write('(')
			is_cond_true := g.comp_if_cond(cond.expr)
			g.write(')')
			return is_cond_true
		}
		ast.PrefixExpr {
			g.write(cond.op.str())
			return g.comp_if_cond(cond.right)
		}
		ast.PostfixExpr {
			ifdef := g.comp_if_to_ifdef((cond.expr as ast.Ident).name, true) or {
				verror(err.msg)
				return false
			}
			g.write('defined($ifdef)')
			return true
		}
		ast.InfixExpr {
			match cond.op {
				.and, .logical_or {
					l := g.comp_if_cond(cond.left)
					g.write(' $cond.op ')
					r := g.comp_if_cond(cond.right)
					return if cond.op == .and { l && r } else { l || r }
				}
				.key_is, .not_is {
					left := cond.left
					mut name := ''
					mut exp_type := ast.Type(0)
					got_type := (cond.right as ast.TypeNode).typ
					if left is ast.SelectorExpr {
						name = '${left.expr}.$left.field_name'
						exp_type = g.comptime_var_type_map[name]
					} else if left is ast.TypeNode {
						name = left.str()
						// this is only allowed for generics currently, otherwise blocked by checker
						exp_type = g.unwrap_generic(left.typ)
					}

					if cond.op == .key_is {
						g.write('$exp_type == $got_type')
						return exp_type == got_type
					} else {
						g.write('$exp_type !=$got_type')
						return exp_type != got_type
					}
				}
				.eq, .ne {
					// TODO Implement `$if method.args.len == 1`
					g.write('1')
					return true
				}
				else {
					return true
				}
			}
		}
		ast.Ident {
			ifdef := g.comp_if_to_ifdef(cond.name, false) or { 'true' } // handled in checker
			g.write('defined($ifdef)')
			return true
		}
		else {
			// should be unreachable, but just in case
			g.write('1')
			return true
		}
	}
}

fn (mut g Gen) comp_for(node ast.CompFor) {
	sym := g.table.get_type_symbol(g.unwrap_generic(node.typ))
	g.writeln('/* \$for $node.val_var in ${sym.name}($node.kind.str()) */ {')
	g.indent++
	// vweb_result_type := ast.new_type(g.table.find_type_idx('vweb.Result'))
	mut i := 0
	// g.writeln('string method = _SLIT("");')
	if node.kind == .methods {
		mut methods := sym.methods.filter(it.attrs.len == 0) // methods without attrs first
		methods_with_attrs := sym.methods.filter(it.attrs.len > 0) // methods with attrs second
		methods << methods_with_attrs
		if methods.len > 0 {
			g.writeln('FunctionData $node.val_var = {0};')
		}
		for method in methods { // sym.methods {
			/*
			if method.return_type != vweb_result_type { // ast.void_type {
				continue
			}
			*/
			g.comp_for_method = method.name
			g.writeln('/* method $i */ {')
			g.writeln('\t${node.val_var}.name = _SLIT("$method.name");')
			if method.attrs.len == 0 {
				g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
			} else {
				attrs := cgen_attrs(method.attrs)
				g.writeln(
					'\t${node.val_var}.attrs = new_array_from_c_array($attrs.len, $attrs.len, sizeof(string), _MOV((string[$attrs.len]){' +
					attrs.join(', ') + '}));')
			}
			if method.params.len < 2 {
				// 0 or 1 (the receiver) args
				g.writeln('\t${node.val_var}.args = __new_array_with_default(0, 0, sizeof(MethodArgs), 0);')
			} else {
				len := method.params.len - 1
				g.write('\t${node.val_var}.args = new_array_from_c_array($len, $len, sizeof(MethodArgs), _MOV((MethodArgs[$len]){')
				// Skip receiver arg
				for j, arg in method.params[1..] {
					typ := arg.typ.idx()
					g.write('{$typ.str(), _SLIT("$arg.name")}')
					if j < len - 1 {
						g.write(', ')
					}
					g.comptime_var_type_map['${node.val_var}.args[$j].typ'] = typ
				}
				g.writeln('}));')
			}
			mut sig := 'anon_fn_'
			// skip the first (receiver) arg
			for j, arg in method.params[1..] {
				// TODO: ignore mut/pts in sig for now
				typ := arg.typ.set_nr_muls(0)
				sig += '$typ'
				if j < method.params.len - 2 {
					sig += '_'
				}
			}
			sig += '_$method.return_type'
			styp := g.table.find_type_idx(sig)
			// println(styp)
			// if styp == 0 { }
			// TODO: type aliases
			ret_typ := method.return_type.idx()
			g.writeln('\t${node.val_var}.typ = $styp;')
			g.writeln('\t${node.val_var}.return_type = $ret_typ;')
			//
			g.comptime_var_type_map['${node.val_var}.return_type'] = ret_typ
			g.comptime_var_type_map['${node.val_var}.typ'] = styp
			g.stmts(node.stmts)
			i++
			g.writeln('}')
			//
			mut delete_keys := []string{}
			for key, _ in g.comptime_var_type_map {
				if key.starts_with(node.val_var) {
					delete_keys << key
				}
			}
			for key in delete_keys {
				g.comptime_var_type_map.delete(key)
			}
		}
	} else if node.kind == .fields {
		// TODO add fields
		if sym.info is ast.Struct {
			mut fields := sym.info.fields.filter(it.attrs.len == 0)
			fields_with_attrs := sym.info.fields.filter(it.attrs.len > 0)
			fields << fields_with_attrs
			if fields.len > 0 {
				g.writeln('\tFieldData $node.val_var = {0};')
			}
			for field in fields {
				g.comp_for_field_var = node.val_var
				g.comp_for_field_value = field
				g.writeln('/* field $i */ {')
				g.writeln('\t${node.val_var}.name = _SLIT("$field.name");')
				if field.attrs.len == 0 {
					g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
				} else {
					attrs := cgen_attrs(field.attrs)
					g.writeln(
						'\t${node.val_var}.attrs = new_array_from_c_array($attrs.len, $attrs.len, sizeof(string), _MOV((string[$attrs.len]){' +
						attrs.join(', ') + '}));')
				}
				// field_sym := g.table.get_type_symbol(field.typ)
				// g.writeln('\t${node.val_var}.typ = _SLIT("$field_sym.name");')
				styp := field.typ
				g.writeln('\t${node.val_var}.typ = $styp;')
				g.writeln('\t${node.val_var}.is_pub = $field.is_pub;')
				g.writeln('\t${node.val_var}.is_mut = $field.is_mut;')
				g.comptime_var_type_map['${node.val_var}.typ'] = styp
				g.stmts(node.stmts)
				i++
				g.writeln('}')
			}
			g.comptime_var_type_map.delete(node.val_var)
		}
	}
	g.indent--
	g.writeln('}// \$for')
}

fn (mut g Gen) comp_if_to_ifdef(name string, is_comptime_optional bool) ?string {
	match name {
		// platforms/os-es:
		'windows' {
			return '_WIN32'
		}
		'ios' {
			return '__TARGET_IOS__'
		}
		'macos' {
			return '__APPLE__'
		}
		'mach' {
			return '__MACH__'
		}
		'darwin' {
			return '__DARWIN__'
		}
		'hpux' {
			return '__HPUX__'
		}
		'gnu' {
			return '__GNU__'
		}
		'qnx' {
			return '__QNX__'
		}
		'linux' {
			return '__linux__'
		}
		'freebsd' {
			return '__FreeBSD__'
		}
		'openbsd' {
			return '__OpenBSD__'
		}
		'netbsd' {
			return '__NetBSD__'
		}
		'bsd' {
			return '__BSD__'
		}
		'dragonfly' {
			return '__DragonFly__'
		}
		'android' {
			return '__ANDROID__'
		}
		'solaris' {
			return '__sun'
		}
		'haiku' {
			return '__haiku__'
		}
		'linux_or_macos' {
			return ''
		}
		//
		'js' {
			return '_VJS'
		}
		// compilers:
		'gcc' {
			return '__V_GCC__'
		}
		'tinyc' {
			return '__TINYC__'
		}
		'clang' {
			return '__clang__'
		}
		'mingw' {
			return '__MINGW32__'
		}
		'msvc' {
			return '_MSC_VER'
		}
		'cplusplus' {
			return '__cplusplus'
		}
		// other:
		'gcboehm' {
			return '_VGCBOEHM'
		}
		'debug' {
			return '_VDEBUG'
		}
		'prod' {
			return '_VPROD'
		}
		'test' {
			return '_VTEST'
		}
		'glibc' {
			return '__GLIBC__'
		}
		'prealloc' {
			return '_VPREALLOC'
		}
		'no_bounds_checking' {
			return 'CUSTOM_DEFINE_no_bounds_checking'
		}
		'freestanding' {
			return '_VFREESTANDING'
		}
		// architectures:
		'amd64' {
			return '__V_amd64'
		}
		'aarch64' {
			return '__V_aarch64'
		}
		// bitness:
		'x64' {
			return 'TARGET_IS_64BIT'
		}
		'x32' {
			return 'TARGET_IS_32BIT'
		}
		// endianness:
		'little_endian' {
			return 'TARGET_ORDER_IS_LITTLE'
		}
		'big_endian' {
			return 'TARGET_ORDER_IS_BIG'
		}
		else {
			if is_comptime_optional
				|| (g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all) {
				return 'CUSTOM_DEFINE_$name'
			}
			return error('bad os ifdef name "$name"') // should never happen, caught in the checker
		}
	}
	return none
}
