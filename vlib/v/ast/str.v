// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// These methods are used only by vfmt, vdoc, and for debugging.
import v.table
import strings

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
	}
	else if node.language == .js {
		name = 'JS.$name'
	}
	f.write('fn ${receiver}${name}')
	if node.is_generic {
		f.write('<T>')
	}
	f.write('(')
	for i, arg in node.args {
		// skip receiver
		// if (node.is_method || node.is_interface) && i == 0 {
		if node.is_method && i == 0 {
			continue
		}
		if arg.is_hidden {
			continue
		}
		is_last_arg := i == node.args.len - 1
		should_add_type := is_last_arg || node.args[i + 1].typ != arg.typ || (node.is_variadic &&
			i == node.args.len - 2)
		if arg.is_mut {
			f.write('mut ')
		}
		f.write(arg.name)
		mut s := t.type_to_str(arg.typ)
		if arg.is_mut {
			// f.write(' mut')
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
		BoolLiteral {
			return it.val.str()
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
		CharLiteral {
			return '`$it.val`'
		}
		EnumVal {
			return '.${it.val}'
		}
		FloatLiteral {
			return it.val
		}
		Ident {
			return it.name
		}
		IndexExpr {
			return '${it.left.str()}[${it.index.str()}]'
		}
		IntegerLiteral {
			return it.val
		}
		InfixExpr {
			return '${it.left.str()} $it.op.str() ${it.right.str()}'
		}
		ParExpr {
			return it.expr.str()
		}
		PrefixExpr {
			return it.op.str() + it.right.str()
		}
		SelectorExpr {
			return '${it.expr.str()}.${it.field_name}'
		}
		StringInterLiteral {
			mut res := []string{}
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
		StringLiteral {
			return '"$it.val"'
		}
		TypeOf {
			return 'typeof(${it.expr.str()})'
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
	mut res := []string{}
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
