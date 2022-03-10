// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.util
import strings

pub fn (node &FnDecl) modname() string {
	if node.mod != '' {
		return node.mod
	}
	mut pamod := node.name.all_before_last('.')
	if pamod == node.name.after('.') {
		pamod = if node.is_builtin { 'builtin' } else { 'main' }
	}
	return pamod
}

// fkey returns a unique name of the function/method.
// it is used in table.used_fns and v.markused.
pub fn (node &FnDecl) fkey() string {
	if node.is_method {
		return '${int(node.receiver.typ)}.$node.name'
	}
	return node.name
}

pub fn (node &Fn) fkey() string {
	if node.is_method {
		return '${int(node.receiver_type)}.$node.name'
	}
	return node.name
}

pub fn (node &CallExpr) fkey() string {
	if node.is_method {
		return '${int(node.receiver_type)}.$node.name'
	}
	return node.name
}

// These methods are used only by vfmt, vdoc, and for debugging.
pub fn (node &AnonFn) stringify(t &Table, cur_mod string, m2a map[string]string) string {
	mut f := strings.new_builder(30)
	f.write_string('fn ')
	if node.inherited_vars.len > 0 {
		f.write_string('[')
		for i, var in node.inherited_vars {
			if i > 0 {
				f.write_string(', ')
			}
			if var.is_mut {
				f.write_string('mut ')
			}
			f.write_string(var.name)
		}
		f.write_string('] ')
	}
	stringify_fn_after_name(node.decl, mut f, t, cur_mod, m2a)
	return f.str()
}

pub fn (node &FnDecl) stringify(t &Table, cur_mod string, m2a map[string]string) string {
	mut f := strings.new_builder(30)
	if node.is_pub {
		f.write_string('pub ')
	}
	f.write_string('fn ')
	if node.is_method {
		f.write_string('(')
		mut styp := util.no_cur_mod(t.type_to_code(node.receiver.typ.clear_flag(.shared_f)),
			cur_mod)
		if node.rec_mut {
			f.write_string(node.receiver.typ.share().str() + ' ')
			styp = styp[1..] // remove &
		}
		f.write_string(node.receiver.name + ' ')
		styp = util.no_cur_mod(styp, cur_mod)
		if node.params[0].is_auto_rec {
			styp = styp.trim('&')
		}
		f.write_string(styp + ') ')
	}
	name := if !node.is_method && node.language == .v {
		node.name.all_after_last('.')
	} else {
		node.name
	}
	f.write_string(name)
	if name in ['+', '-', '*', '/', '%', '<', '>', '==', '!=', '>=', '<='] {
		f.write_string(' ')
	}
	stringify_fn_after_name(node, mut f, t, cur_mod, m2a)
	return f.str()
}

fn stringify_fn_after_name(node &FnDecl, mut f strings.Builder, t &Table, cur_mod string, m2a map[string]string) {
	mut add_para_types := true
	if node.generic_names.len > 0 {
		if node.is_method {
			sym := t.sym(node.params[0].typ)
			if sym.info is Struct {
				generic_names := sym.info.generic_types.map(t.sym(it).name)
				if generic_names == node.generic_names {
					add_para_types = false
				}
			}
		}
		if add_para_types {
			f.write_string('<')
			for i, gname in node.generic_names {
				is_last := i == node.generic_names.len - 1
				f.write_string(gname)
				if !is_last {
					f.write_string(', ')
				}
			}
			f.write_string('>')
		}
	}
	f.write_string('(')
	for i, arg in node.params {
		// skip receiver
		// if (node.is_method || node.is_interface) && i == 0 {
		if node.is_method && i == 0 {
			continue
		}
		if arg.is_hidden {
			continue
		}
		is_last_arg := i == node.params.len - 1
		is_type_only := arg.name == ''
		should_add_type := true // is_last_arg || is_type_only || node.params[i + 1].typ != arg.typ ||
		// (node.is_variadic && i == node.params.len - 2)
		if arg.is_mut {
			f.write_string(arg.typ.share().str() + ' ')
		}
		f.write_string(arg.name)
		mut s := t.type_to_str(arg.typ.clear_flag(.shared_f))
		if arg.is_mut {
			arg_sym := t.sym(arg.typ)
			if s.starts_with('&') && ((!arg_sym.is_number() && arg_sym.kind != .bool)
				|| node.language != .v) {
				s = s[1..]
			}
		}
		s = util.no_cur_mod(s, cur_mod)
		for mod, alias in m2a {
			s = s.replace(mod, alias)
		}
		if should_add_type {
			if !is_type_only {
				f.write_string(' ')
			}
			if node.is_variadic && is_last_arg {
				f.write_string('...')
			}
			f.write_string(s)
		}
		if !is_last_arg {
			f.write_string(', ')
		}
	}
	f.write_string(')')
	if node.return_type != void_type {
		mut rs := util.no_cur_mod(t.type_to_str(node.return_type), cur_mod)
		for mod, alias in m2a {
			rs = rs.replace(mod, alias)
		}
		f.write_string(' ' + rs)
	}
}

// Expressions in string interpolations may have to be put in braces if they
// are non-trivial, if they would interfere with the next character or if a
// format specification is given. In the latter case
// the format specifier must be appended, separated by a colon:
// '$z $z.b $z.c.x ${x[4]} ${z:8.3f} ${a:-20} ${a>b+2}'
// This method creates the format specifier (including the colon) or an empty
// string if none is needed and also returns (as bool) if the expression
// must be enclosed in braces.
pub fn (lit &StringInterLiteral) get_fspec_braces(i int) (string, bool) {
	mut res := []string{}
	needs_fspec := lit.need_fmts[i] || lit.pluss[i]
		|| (lit.fills[i] && lit.fwidths[i] >= 0) || lit.fwidths[i] != 0
		|| lit.precisions[i] != 987698
	mut needs_braces := needs_fspec
	sx := lit.exprs[i].str()
	if sx.contains(r'"') || sx.contains(r"'") {
		needs_braces = true
	}
	if !needs_braces {
		if i + 1 < lit.vals.len && lit.vals[i + 1].len > 0 {
			next_char := lit.vals[i + 1][0]
			if util.is_func_char(next_char) || next_char == `.` || next_char == `(` {
				needs_braces = true
			}
		}
	}
	if !needs_braces {
		mut sub_expr := lit.exprs[i]
		for {
			match mut sub_expr {
				Ident {
					if sub_expr.name[0] == `@` {
						needs_braces = true
					}
					break
				}
				CallExpr {
					if sub_expr.args.len != 0 || sub_expr.concrete_types.len != 0 {
						needs_braces = true
					} else if sub_expr.left is CallExpr {
						sub_expr = sub_expr.left
						continue
					} else if sub_expr.left is CastExpr || sub_expr.left is IndexExpr {
						needs_braces = true
					}
					break
				}
				SelectorExpr {
					if sub_expr.field_name[0] == `@` {
						needs_braces = true
						break
					}
					sub_expr = sub_expr.expr
					continue
				}
				else {
					needs_braces = true
					break
				}
			}
		}
	}
	if needs_fspec {
		res << ':'
		if lit.pluss[i] {
			res << '+'
		}
		if lit.fills[i] && lit.fwidths[i] >= 0 {
			res << '0'
		}
		if lit.fwidths[i] != 0 {
			res << '${lit.fwidths[i]}'
		}
		if lit.precisions[i] != 987698 {
			res << '.${lit.precisions[i]}'
		}
		if lit.need_fmts[i] {
			res << '${lit.fmts[i]:c}'
		}
	}
	return res.join(''), needs_braces
}

// string representation of expr
pub fn (x Expr) str() string {
	match x {
		AnonFn {
			return 'anon_fn'
		}
		ComptimeType {
			return x.str()
		}
		DumpExpr {
			return 'dump($x.expr.str())'
		}
		ArrayInit {
			mut fields := []string{}
			if x.has_len {
				fields << 'len: $x.len_expr.str()'
			}
			if x.has_cap {
				fields << 'cap: $x.cap_expr.str()'
			}
			if x.has_default {
				fields << 'init: $x.default_expr.str()'
			}
			if fields.len > 0 {
				return '[]T{${fields.join(', ')}}'
			} else {
				return x.exprs.str()
			}
		}
		AsCast {
			return '$x.expr.str() as ${global_table.type_to_str(x.typ)}'
		}
		AtExpr {
			return '$x.val'
		}
		CTempVar {
			return x.orig.str()
		}
		BoolLiteral {
			return x.val.str()
		}
		CastExpr {
			return '${x.typname}($x.expr.str())'
		}
		CallExpr {
			sargs := args2str(x.args)
			propagate_suffix := if x.or_block.kind == .propagate { ' ?' } else { '' }
			if x.is_method {
				return '${x.left.str()}.${x.name}($sargs)$propagate_suffix'
			}
			if x.name.starts_with('${x.mod}.') {
				return util.strip_main_name('${x.name}($sargs)$propagate_suffix')
			}
			if x.mod == '' && x.name == '' {
				return x.left.str() + '($sargs)$propagate_suffix'
			}
			if x.name.contains('.') {
				return '${x.name}($sargs)$propagate_suffix'
			}
			return '${x.mod}.${x.name}($sargs)$propagate_suffix'
		}
		CharLiteral {
			return '`$x.val`'
		}
		Comment {
			if x.is_multi {
				lines := x.text.split_into_lines()
				return '/* $lines.len lines comment */'
			} else {
				text := x.text.trim('\x01').trim_space()
				return '´// $text´'
			}
		}
		ComptimeSelector {
			return '${x.left}.$$x.field_expr'
		}
		ConcatExpr {
			return x.vals.map(it.str()).join(',')
		}
		EnumVal {
			return '.$x.val'
		}
		FloatLiteral, IntegerLiteral {
			return x.val
		}
		GoExpr {
			return 'go $x.call_expr'
		}
		Ident {
			return x.name
		}
		IfExpr {
			mut parts := []string{}
			dollar := if x.is_comptime { '$' } else { '' }
			for i, branch in x.branches {
				if i != 0 {
					parts << ' } ${dollar}else '
				}
				if i < x.branches.len - 1 || !x.has_else {
					parts << ' ${dollar}if ' + branch.cond.str() + ' { '
				}
				for stmt in branch.stmts {
					parts << stmt.str()
				}
			}
			parts << ' }'
			return parts.join('')
		}
		IndexExpr {
			return '$x.left.str()[$x.index.str()]'
		}
		InfixExpr {
			return '$x.left.str() $x.op.str() $x.right.str()'
		}
		MapInit {
			mut pairs := []string{}
			for ik, kv in x.keys {
				mv := x.vals[ik].str()
				pairs << '$kv: $mv'
			}
			return 'map{ ${pairs.join(' ')} }'
		}
		ParExpr {
			return '($x.expr)'
		}
		PostfixExpr {
			if x.op == .question {
				return '$x.expr ?'
			}
			return '$x.expr$x.op'
		}
		PrefixExpr {
			return x.op.str() + x.right.str()
		}
		RangeExpr {
			mut s := '..'
			if x.has_low {
				s = '$x.low ' + s
			}
			if x.has_high {
				s = s + ' $x.high'
			}
			return s
		}
		SelectExpr {
			return 'ast.SelectExpr'
		}
		SelectorExpr {
			return '${x.expr.str()}.$x.field_name'
		}
		SizeOf {
			if x.is_type {
				return 'sizeof(${global_table.type_to_str(x.typ)})'
			}
			return 'sizeof($x.expr)'
		}
		OffsetOf {
			return '__offsetof(${global_table.type_to_str(x.struct_type)}, $x.field)'
		}
		StringInterLiteral {
			mut res := strings.new_builder(50)
			res.write_string("'")
			for i, val in x.vals {
				res.write_string(val)
				if i >= x.exprs.len {
					break
				}
				res.write_string('$')
				fspec_str, needs_braces := x.get_fspec_braces(i)
				if needs_braces {
					res.write_string('{')
					res.write_string(x.exprs[i].str())
					res.write_string(fspec_str)
					res.write_string('}')
				} else {
					res.write_string(x.exprs[i].str())
				}
			}
			res.write_string("'")
			return res.str()
		}
		StringLiteral {
			return "'$x.val'"
		}
		TypeNode {
			return 'TypeNode($x.typ)'
		}
		TypeOf {
			return 'typeof($x.expr.str())'
		}
		Likely {
			return '_likely_($x.expr.str())'
		}
		UnsafeExpr {
			return 'unsafe { $x.expr }'
		}
		None {
			return 'none'
		}
		IsRefType {
			return 'isreftype(' + if x.is_type {
				global_table.type_to_str(x.typ)
			} else {
				x.expr.str()
			} + ')'
		}
		IfGuardExpr {
			mut s := ''
			for i, var in x.vars {
				s += var.name
				if i != x.vars.len - 1 {
					s += ', '
				}
			}
			return s + ' := ' + x.expr.str()
		}
		StructInit {
			sname := global_table.sym(x.typ).name
			return '$sname{....}'
		}
		ArrayDecompose {
			return 'ast.ArrayDecompose'
		}
		Assoc {
			return 'ast.Assoc'
		}
		ChanInit {
			return 'ast.ChanInit'
		}
		ComptimeCall {
			return 'ast.ComptimeCall'
		}
		EmptyExpr {
			return 'ast.EmptyExpr'
		}
		LockExpr {
			return 'ast.LockExpr'
		}
		MatchExpr {
			return 'ast.MatchExpr'
		}
		NodeError {
			return 'ast.NodeError'
		}
		OrExpr {
			return 'ast.OrExpr'
		}
		SqlExpr {
			return 'ast.SqlExpr'
		}
	}
	return '[unhandled expr type $x.type_name()]'
}

pub fn (a CallArg) str() string {
	if a.is_mut {
		return 'mut $a.expr.str()'
	}
	return '$a.expr.str()'
}

pub fn args2str(args []CallArg) string {
	mut res := []string{}
	for a in args {
		res << a.str()
	}
	return res.join(', ')
}

pub fn (node &BranchStmt) str() string {
	mut s := '$node.kind'
	if node.label.len > 0 {
		s += ' $node.label'
	}
	return s
}

pub fn (node Stmt) str() string {
	match node {
		AssertStmt {
			return 'assert $node.expr'
		}
		AssignStmt {
			mut out := ''
			for i, left in node.left {
				if left is Ident {
					var_info := left.var_info()
					if var_info.is_mut {
						out += 'mut '
					}
				}
				out += left.str()
				if i < node.left.len - 1 {
					out += ','
				}
			}
			out += ' $node.op.str() '
			for i, val in node.right {
				out += val.str()
				if i < node.right.len - 1 {
					out += ','
				}
			}
			return out
		}
		BranchStmt {
			return node.str()
		}
		ConstDecl {
			fields := node.fields.map(field_to_string)
			return 'const (${fields.join(' ')})'
		}
		ExprStmt {
			return node.expr.str()
		}
		FnDecl {
			return 'fn ${node.name}( $node.params.len params ) { $node.stmts.len stmts }'
		}
		EnumDecl {
			return 'enum $node.name { $node.fields.len fields }'
		}
		Module {
			return 'module $node.name'
		}
		Import {
			mut out := 'import $node.mod'
			if node.alias.len > 0 {
				out += ' as $node.alias'
			}
			return out
		}
		StructDecl {
			return 'struct $node.name { $node.fields.len fields }'
		}
		else {
			return '[unhandled stmt str type: $node.type_name() ]'
		}
	}
}

fn field_to_string(f ConstField) string {
	x := f.name.trim_string_left(f.mod + '.')
	return '$x = $f.expr'
}

pub fn (e ComptimeForKind) str() string {
	match e {
		.methods { return 'methods' }
		.fields { return 'fields' }
		.attributes { return 'attributes' }
	}
}
