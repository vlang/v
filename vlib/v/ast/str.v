// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// These methods are used only by vfmt, vdoc, and for debugging.
import (
	v.table
	strings
)

pub fn (node &FnDecl) str(t &table.Table) string {
	mut f := strings.new_builder(30)
	if node.is_pub {
		f.write('pub ')
	}
	mut receiver := ''
	if node.is_method {
		mut styp := t.type_to_str(node.receiver.typ)
		mut m := if node.rec_mut { 'mut ' } else { '' }
		if node.rec_mut {
			styp = styp[1..] // remove &
		}
		receiver = '($node.receiver.name $m$styp) '
		/*
		sym := t.get_type_symbol(node.receiver.typ)
		name := sym.name.after('.')
		mut m := if node.rec_mut { 'mut ' } else { '' }
		if !node.rec_mut && table.type_is_ptr(node.receiver.typ) {
			m = '&'
		}
		receiver = '($node.receiver.name $m$name) '
*/
	}
	name := node.name.after('.')
	if node.is_c {
		name = 'C.$name'
	}
	f.write('fn ${receiver}${name}(')
	for i, arg in node.args {
		// skip receiver
		if node.is_method && i == 0 {
			continue
		}
		is_last_arg := i == node.args.len - 1
		should_add_type := is_last_arg || node.args[i + 1].typ != arg.typ || (node.is_variadic &&
			i == node.args.len - 2)
		f.write(arg.name)
		mut s := t.type_to_str(arg.typ)
		if arg.is_mut {
			f.write(' mut')
			if s.starts_with('&') {
				s = s[1..]
			}
		}
		if should_add_type {
			if node.is_variadic && is_last_arg {
				f.write(' ...' + s)
			} else {
				f.write(' ' + s)
			}
		}
		if !is_last_arg {
			f.write(', ')
		}
	}
	f.write(')')
	if node.return_type != table.void_type {
		// typ := t.type_to_str(node.typ)
		// if typ.starts_with('
		f.write(' ' + t.type_to_str(node.return_type))
	}
	return f.str()
}

// string representaiton of expr
pub fn (x Expr) str() string {
	match x {
		Ident {
			return it.name
		}
		InfixExpr {
			return '${it.left.str()} $it.op.str() ${it.right.str()}'
		}
		PrefixExpr {
			return it.op.str() + it.right.str()
		}
		IntegerLiteral {
			return it.val
		}
		FloatLiteral {
			return it.val
		}
		StringLiteral {
			return '"$it.val"'
		}
		StringInterLiteral {
			res := []string
			res << "'"
			for i, val in it.vals {
				res << val
				if i >= it.exprs.len {
					continue
				}
				res << '$'
				if it.expr_fmts[i].len > 0 {
					res << '{'
					res << it.exprs[i].str()
					res << it.expr_fmts[i]
					res << '}'
				} else {
					res << it.exprs[i].str()
				}
			}
			res << "'"
			return res.join('')
		}
		BoolLiteral {
			return it.val.str()
		}
		ParExpr {
			return it.expr.str()
		}
		IndexExpr {
			return '${it.left.str()}[${it.index.str()}]'
		}
		CastExpr {
			return '${it.typname}(${it.expr.str()})'
		}
		CallExpr {
			sargs := args2str(it.args)
			if it.is_method {
				return '${it.left.str()}.${it.name}($sargs)'
			}
			return '${it.mod}.${it.name}($sargs)'
		}
		else {
			return '[unhandled expr type ${typeof(x)}]'
		}
	}
}

pub fn (a CallArg) str() string {
	if a.is_mut {
		return 'mut ${a.expr.str()}'
	}
	return '${a.expr.str()}'
}

pub fn args2str(args []CallArg) string {
	mut res := []string
	for a in args {
		res << a.str()
	}
	return res.join(', ')
}

pub fn (node Stmt) str() string {
	match node {
		AssignStmt {
			mut out := ''
			for i, ident in it.left {
				var_info := ident.var_info()
				if var_info.is_mut {
					out += 'mut '
				}
				out += ident.name
				if i < it.left.len - 1 {
					out += ','
				}
			}
			out += ' $it.op.str() '
			for i, val in it.right {
				out += val.str()
				if i < it.right.len - 1 {
					out += ','
				}
			}
			return out
		}
		ExprStmt {
			return it.expr.str()
		}
		FnDecl {
			return 'fn ${it.name}() { $it.stmts.len stmts }'
		}
		else {
			return '[unhandled stmt str type: ${typeof(node)} ]'
		}
	}
}
