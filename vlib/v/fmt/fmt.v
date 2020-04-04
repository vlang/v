// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import (
	v.ast
	v.table
	strings
)

const (
	tabs = ['', '\t', '\t\t', '\t\t\t', '\t\t\t\t', '\t\t\t\t\t', '\t\t\t\t\t\t', '\t\t\t\t\t\t\t']
	// tabs = ['', '  ', '    ', '      ', '        ']
	max_len = 80
)

struct Fmt {
	out            strings.Builder
	table          &table.Table
mut:
	indent         int
	empty_line     bool
	line_len       int
	single_line_if bool
	cur_mod        string
}

pub fn fmt(file ast.File, table &table.Table) string {
	mut f := Fmt{
		out: strings.new_builder(1000)
		table: table
		indent: 0
	}
	f.mod(file.mod)
	f.imports(file.imports)
	for stmt in file.stmts {
		f.stmt(stmt)
	}
	return f.out.str().trim_space() + '\n'
}

pub fn (f mut Fmt) write(s string) {
	if f.indent > 0 && f.empty_line {
		f.out.write(tabs[f.indent])
		f.line_len += f.indent * 4
	}
	f.out.write(s)
	f.line_len += s.len
	f.empty_line = false
}

pub fn (f mut Fmt) writeln(s string) {
	if f.indent > 0 && f.empty_line {
		// println(f.indent.str() + s)
		f.out.write(tabs[f.indent])
	}
	f.out.writeln(s)
	f.empty_line = true
	f.line_len = 0
}

fn (f mut Fmt) mod(mod ast.Module) {
	if mod.name != 'main' {
		f.writeln('module ${mod.name}\n')
	}
	f.cur_mod = mod.name
}

fn (f mut Fmt) imports(imports []ast.Import) {
	if imports.len == 1 {
		imp_stmt_str := f.imp_stmt_str(imports[0])
		f.writeln('import ${imp_stmt_str}\n')
	}
	else if imports.len > 1 {
		f.writeln('import (')
		f.indent++
		for imp in imports {
			f.writeln(f.imp_stmt_str(imp))
		}
		f.indent--
		f.writeln(')\n')
	}
}

fn (f Fmt) imp_stmt_str(imp ast.Import) string {
	is_diff := imp.alias != imp.mod && !imp.mod.ends_with('.' + imp.alias)
	imp_alias_suffix := if is_diff { ' as ${imp.alias}' } else { '' }
	return '${imp.mod}${imp_alias_suffix}'
}

fn (f mut Fmt) stmts(stmts []ast.Stmt) {
	f.indent++
	for stmt in stmts {
		f.stmt(stmt)
	}
	f.indent--
}

fn (f mut Fmt) stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			for i, ident in it.left {
				var_info := ident.var_info()
				if var_info.is_mut {
					f.write('mut ')
				}
				f.expr(ident)
				if i < it.left.len - 1 {
					f.write(', ')
				}
			}
			f.write(' $it.op.str() ')
			for i, val in it.right {
				f.expr(val)
				if i < it.right.len - 1 {
					f.write(', ')
				}
			}
			f.writeln('')
		}
		ast.Attr {
			f.writeln('[$it.name]')
		}
		ast.BranchStmt {
			match it.tok.kind {
				.key_break {
					f.writeln('break')
				}
				.key_continue {
					f.writeln('continue')
				}
				else {}
	}
		}
		ast.ConstDecl {
			if it.is_pub {
				f.write('pub ')
			}
			f.writeln('const (')
			f.indent++
			for i, field in it.fields {
				name := field.name.after('.')
				f.write('$name = ')
				f.expr(field.expr)
				f.writeln('')
			}
			f.indent--
			f.writeln(')\n')
		}
		ast.DeferStmt {
			f.writeln('defer {')
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.EnumDecl {
			if it.is_pub {
				f.write('pub ')
			}
			f.writeln('enum $it.name {')
			for val in it.vals {
				f.writeln('\t' + val)
			}
			f.writeln('}\n')
		}
		ast.ExprStmt {
			f.expr(it.expr)
			if !f.single_line_if {
				f.writeln('')
			}
		}
		ast.FnDecl {
			s := it.str(f.table)
			// f.write(it.str(f.table))
			f.write(s.replace(f.cur_mod + '.', '')) // `Expr` instead of `ast.Expr` in mod ast
			f.writeln(' {')
			f.stmts(it.stmts)
			f.writeln('}\n')
		}
		ast.ForInStmt {
			f.write('for ')
			if it.key_var != '' {
				f.write(it.key_var)
			}
			if it.val_var != '' {
				if it.key_var != '' {
					f.write(', ')
				}
				f.write(it.val_var)
			}
			f.write(' in ')
			f.expr(it.cond)
			if it.is_range {
				f.write(' .. ')
				f.expr(it.high)
			}
			f.writeln(' {')
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.ForStmt {
			f.write('for ')
			f.expr(it.cond)
			f.writeln(' {')
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.GotoLabel {
			f.writeln('$it.name:')
		}
		ast.GotoStmt {
			f.writeln('goto $it.name')
		}
		ast.LineComment {
			f.writeln('// $it.text')
		}
		ast.MultiLineComment {
			f.writeln('/*')
			f.writeln(it.text)
			f.writeln('*/')
		}
		ast.Return {
			f.write('return')
			// multiple returns
			if it.exprs.len > 1 {
				f.write(' ')
				for i, expr in it.exprs {
					f.expr(expr)
					if i < it.exprs.len - 1 {
						f.write(', ')
					}
				}
			}
			// normal return
			else if it.exprs.len == 1 {
				f.write(' ')
				f.expr(it.exprs[0])
			}
			f.writeln('')
		}
		ast.StructDecl {
			f.struct_decl(it)
		}
		ast.UnsafeStmt {
			f.writeln('unsafe {')
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.Import {}
		// already handled in f.imports
		ast.TypeDecl {
			f.type_decl(it)
		}
		ast.AssertStmt {
			f.write('assert ')
			f.expr(it.expr)
			f.writeln('')
		}
		ast.CompIf {
			inversion := if it.is_not { '!' } else { '' }
			f.writeln('\$if ${inversion}${it.val} {')
			f.stmts(it.stmts)
			if it.has_else {
				f.writeln('} \$else {')
				f.stmts(it.else_stmts)
			}
			f.writeln('}')
		}
		else {
			eprintln('fmt stmt: unknown node: ' + typeof(node))
			// exit(1)
		}
	}
}

fn (f mut Fmt) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl {
			if it.is_pub {
				f.write('pub ')
			}
			ptype := f.table.type_to_str(it.parent_type)
			f.write('type $it.name $ptype')
		}
		ast.SumTypeDecl {
			if it.is_pub {
				f.write('pub ')
			}
			f.write('type $it.name = ')
			mut sum_type_names := []string
			for t in it.sub_types {
				sum_type_names << f.table.type_to_str(t)
			}
			f.write(sum_type_names.join(' | '))
		}
		else {
			eprintln('fmt type_decl: unknown ' + typeof(node))
		}
	}
	f.writeln('\n')
}

fn (f mut Fmt) struct_decl(node ast.StructDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	f.writeln('struct $node.name {')
	mut max := 0
	for field in node.fields {
		if field.name.len > max {
			max = field.name.len
		}
	}
	for i, field in node.fields {
		if i == node.mut_pos {
			f.writeln('mut:')
		}
		else if i == node.pub_pos {
			f.writeln('pub:')
		}
		else if i == node.pub_mut_pos {
			f.writeln('pub mut:')
		}
		f.write('\t$field.name ')
		f.write(strings.repeat(` `, max - field.name.len))
		f.writeln(f.type_to_str(field.typ))
	}
	f.writeln('}\n')
}

fn (f &Fmt) type_to_str(t table.Type) string {
	res := f.table.type_to_str(t)
	return res.replace(f.cur_mod + '.', '')
}

fn (f mut Fmt) expr(node ast.Expr) {
	match node {
		ast.ArrayInit {
			// `x := []string`
			if it.exprs.len == 0 && it.typ != 0 {
				f.write(f.table.type_to_str(it.typ))
			}
			// `[1,2,3]`
			else {
				// type_sym := f.table.get_type_symbol(it.typ)
				f.write('[')
				for i, expr in it.exprs {
					if i > 0 && it.exprs.len > 1 {
						f.wrap_long_line()
					}
					f.expr(expr)
					if i < it.exprs.len - 1 {
						f.write(', ')
					}
				}
				f.write(']')
			}
		}
		ast.AsCast {
			type_str := f.table.type_to_str(it.typ)
			f.expr(it.expr)
			f.write(' as $type_str')
		}
		ast.AssignExpr {
			f.expr(it.left)
			f.write(' $it.op.str() ')
			f.expr(it.val)
		}
		ast.Assoc {
			f.writeln('{')
			// f.indent++
			f.writeln('\t$it.var_name |')
			// TODO StructInit copy pasta
			for i, field in it.fields {
				f.write('\t$field: ')
				f.expr(it.exprs[i])
				f.writeln('')
			}
			// f.indent--
			f.write('}')
		}
		ast.BoolLiteral {
			f.write(it.val.str())
		}
		ast.CastExpr {
			f.write(f.table.type_to_str(it.typ) + '(')
			f.expr(it.expr)
			f.write(')')
		}
		ast.CallExpr {
			if it.is_method {
				f.expr(it.left)
				f.write('.' + it.name + '(')
				f.call_args(it.args)
				f.write(')')
				f.or_expr(it.or_block)
			}
			else {
				f.write('${it.name}(')
				f.call_args(it.args)
				f.write(')')
				f.or_expr(it.or_block)
			}
		}
		ast.CharLiteral {
			f.write('`$it.val`')
		}
		ast.EnumVal {
			f.write(it.enum_name + '.' + it.val)
		}
		ast.FloatLiteral {
			f.write(it.val)
		}
		ast.IfExpr {
			single_line := it.branches.len == 2 && it.has_else //
			&& it.branches[0].stmts.len == 1 && it.branches[1].stmts.len == 1
			f.single_line_if = single_line
			for i, branch in it.branches {
				if i == 0 {
					f.write('if ')
					f.expr(branch.cond)
					f.write(' {')
				}
				else if i < it.branches.len - 1 || !it.has_else {
					f.write('} else if ')
					f.expr(branch.cond)
					f.write(' {')
				}
				else if i == it.branches.len - 1 && it.has_else {
					f.write('} else {')
				}
				if single_line {
					f.write(' ')
				}
				else {
					f.writeln('')
				}
				f.stmts(branch.stmts)
				if single_line {
					f.write(' ')
				}
			}
			f.write('}')
			f.single_line_if = false
		}
		ast.Ident {
			if it.kind == .blank_ident {
				f.write('_')
			}
			else {
				f.write('$it.name')
			}
		}
		ast.InfixExpr {
			f.expr(it.left)
			f.write(' $it.op.str() ')
			f.wrap_long_line()
			f.expr(it.right)
		}
		ast.IndexExpr {
			f.expr(it.left)
			f.write('[')
			f.expr(it.index)
			f.write(']')
		}
		ast.IntegerLiteral {
			f.write(it.val)
		}
		ast.MapInit {
			f.writeln('{')
			f.indent++
			/*
			mut max := 0
			for i, key in it.keys {
				if key.len > max {
					max = key.len
				}
			}
				*/

			for i, key in it.keys {
				f.expr(key)
				// f.write(strings.repeat(` `, max - field.name.len))
				f.write(': ')
				f.expr(it.vals[i])
				f.writeln('')
			}
			f.indent--
			f.write('}')
		}
		ast.MatchExpr {
			f.write('match ')
			f.expr(it.cond)
			f.writeln(' {')
			f.indent++
			for i, branch in it.branches {
				// normal branch
				if i < it.branches.len - 1 {
					for j, expr in branch.exprs {
						f.expr(expr)
						if j < branch.exprs.len - 1 {
							f.write(', ')
						}
					}
				}
				// else branch
				else {
					f.write('else')
				}
				if branch.stmts.len == 0 {
					f.writeln(' {}')
				}
				else {
					f.writeln(' {')
					f.stmts(branch.stmts)
					f.writeln('}')
				}
			}
			f.indent--
			f.write('}')
		}
		ast.None {
			f.write('none')
		}
		ast.IfGuardExpr {
			f.write(it.var_name + ' := ')
			f.expr(it.expr)
		}
		ast.ParExpr {
			f.write('(')
			f.expr(it.expr)
			f.write(')')
		}
		ast.PostfixExpr {
			f.expr(it.expr)
			f.write(it.op.str())
		}
		ast.PrefixExpr {
			f.write(it.op.str())
			f.expr(it.right)
		}
		ast.RangeExpr {
			f.expr(it.low)
			f.write('..')
			f.expr(it.high)
		}
		ast.SelectorExpr {
			f.expr(it.expr)
			f.write('.')
			f.write(it.field)
		}
		ast.StringLiteral {
			if it.val.contains("'") {
				f.write('"$it.val"')
			}
			else {
				f.write("'$it.val'")
			}
		}
		ast.StringInterLiteral {
			f.write("'")
			for i, val in it.vals {
				f.write(val)
				if i>=it.exprs.len {
					continue
				}
				f.write('$')
				if it.expr_fmts[i].len > 0 {
					f.write('{')
					f.expr(it.exprs[i])
					f.write(it.expr_fmts[i])
					f.write('}')
				}else{
					f.expr(it.exprs[i])
				}
			}
			f.write("'")
		}
		ast.StructInit {
			type_sym := f.table.get_type_symbol(it.typ)
			// `Foo{}` on one line if there are no fields
			if it.fields.len == 0 {
				f.write('$type_sym.name{}')
			}
			else {
				f.writeln('$type_sym.name{')
				for i, field in it.fields {
					f.write('\t$field: ')
					f.expr(it.exprs[i])
					f.writeln('')
				}
				f.write('}')
			}
		}
		ast.TypeOf {
			f.write('typeof(')
			f.expr(it.expr)
			f.write(')')
		}
		else {
			eprintln('fmt expr: unhandled node ' + typeof(node))
		}
	}
}

fn (f mut Fmt) wrap_long_line() {
	if f.line_len > max_len {
		f.write('\n' + tabs[f.indent + 1])
		f.line_len = 0
	}
}

fn (f mut Fmt) call_args(args []ast.CallArg) {
	for i, arg in args {
		if arg.is_mut {
			f.write('mut ')
		}
		if i > 0 {
			f.wrap_long_line()
		}
		f.expr(arg.expr)
		if i < args.len - 1 {
			f.write(', ')
		}
	}
}

fn (f mut Fmt) or_expr(or_block ast.OrExpr) {
	if or_block.stmts.len > 0 {
		f.writeln(' or {')
		f.stmts(or_block.stmts)
		f.write('}')
	}
}
