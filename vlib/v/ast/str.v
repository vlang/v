// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.table
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

// These methods are used only by vfmt, vdoc, and for debugging.
pub fn (node &FnDecl) stringify(t &table.Table, cur_mod string) string {
	mut f := strings.new_builder(30)
	if node.is_pub {
		f.write('pub ')
	}
	mut receiver := ''
	if node.is_method {
		mut styp := util.no_cur_mod(t.type_to_str(node.receiver.typ), cur_mod)
		m := if node.rec_mut { node.receiver.typ.share().str() + ' ' } else { '' }
		if node.rec_mut {
			styp = styp[1..] // remove &
		}
		styp = util.no_cur_mod(styp, cur_mod)
		receiver = '($m$node.receiver.name $styp) '
		/*
		sym := t.get_type_symbol(node.receiver.typ)
		name := sym.name.after('.')
		mut m := if node.rec_mut { 'mut ' } else { '' }
		if !node.rec_mut && node.receiver.typ.is_ptr() {
			m = '&'
		}
		receiver = '($node.receiver.name $m$name) '
		*/
	}
	mut name := if node.is_anon { '' } else { node.name.after('.') }
	if node.language == .c {
		name = 'C.$name'
	} else if node.language == .js {
		name = 'JS.$name'
	}
	f.write('fn $receiver$name')
	if node.is_generic {
		f.write('<T>')
	}
	f.write('(')
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
			f.write(arg.typ.share().str() + ' ')
		}
		f.write(arg.name)
		mut s := t.type_to_str(arg.typ)
		if arg.is_mut {
			// f.write(' mut')
			if s.starts_with('&') {
				s = s[1..]
			}
		}
		s = util.no_cur_mod(s, cur_mod)
		if should_add_type {
			if !is_type_only {
				f.write(' ')
			}
			if node.is_variadic && is_last_arg {
				f.write('...')
			}
			f.write(s)
		}
		if !is_last_arg {
			f.write(', ')
		}
	}
	f.write(')')
	if node.return_type != table.void_type {
		// typ := t.type_to_str(node.typ)
		// if typ.starts_with('
		f.write(' ' + util.no_cur_mod(t.type_to_str(node.return_type), cur_mod))
	}
	return f.str()
}

pub fn (x &InfixExpr) str() string {
	return '$x.left.str() $x.op.str() $x.right.str()'
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
	needs_fspec := lit.need_fmts[i] || lit.pluss[i] ||
		(lit.fills[i] && lit.fwidths[i] >= 0) || lit.fwidths[i] != 0 || lit.precisions[i] != 987698
	mut needs_braces := needs_fspec
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
					if sub_expr.args.len != 0 {
						needs_braces = true
					}
					break
				}
				SelectorExpr {
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
		CTempVar {
			return x.orig.str()
		}
		BoolLiteral {
			return x.val.str()
		}
		CastExpr {
			return '${x.typname}($x.expr.str())'
		}
		AtExpr {
			return '$x.val'
		}
		CallExpr {
			sargs := args2str(x.args)
			if x.is_method {
				return '${x.left.str()}.${x.name}($sargs)'
			}
			if x.name.starts_with('${x.mod}.') {
				return util.strip_main_name('${x.name}($sargs)')
			}
			return '${x.mod}.${x.name}($sargs)'
		}
		CharLiteral {
			return '`$x.val`'
		}
		EnumVal {
			return '.$x.val'
		}
		FloatLiteral, IntegerLiteral {
			return x.val
		}
		Ident {
			return x.name
		}
		IndexExpr {
			return '$x.left.str()[$x.index.str()]'
		}
		InfixExpr {
			return '$x.left.str() $x.op.str() $x.right.str()'
		}
		ParExpr {
			return '($x.expr)'
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
		SelectorExpr {
			return '${x.expr.str()}.$x.field_name'
		}
		SizeOf {
			return 'sizeof($x.expr)'
		}
		StringInterLiteral {
			mut res := []string{}
			res << "'"
			for i, val in x.vals {
				res << val
				if i >= x.exprs.len {
					break
				}
				res << '$'
				fspec_str, needs_braces := x.get_fspec_braces(i)
				if needs_braces {
					res << '{'
					res << x.exprs[i].str()
					res << fspec_str
					res << '}'
				} else {
					res << x.exprs[i].str()
				}
			}
			res << "'"
			return res.join('')
		}
		StringLiteral {
			return '"$x.val"'
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
		else {}
	}
	return '[unhandled expr type ${typeof(x)}]'
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
		ExprStmt {
			return node.expr.str()
		}
		FnDecl {
			return 'fn ${node.name}() { $node.stmts.len stmts }'
		}
		else {
			return '[unhandled stmt str type: ${typeof(node)} ]'
		}
	}
}

pub fn (e CompForKind) str() string {
	match e {
		.methods { return 'methods' }
		.fields { return 'fields' }
	}
}
