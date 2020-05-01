// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.ast
import v.table
import strings

const (
	level_indent = '\t'
	max_len = 90
)

struct Fmt {
	out            strings.Builder
	out_imports    strings.Builder
	table          &table.Table
mut:
	indent         int
	empty_line     bool
	line_len       int
	single_line_if bool
	cur_mod        string
	file           ast.File
	did_imports    bool
	is_assign      bool
	auto_imports   []string // automatically inserted imports that the user forgot to specify
	import_pos     int // position of the imports in the resulting string for later autoimports insertion
	used_imports   []string // to remove unused imports
}

pub fn fmt(file ast.File, table &table.Table) string {
	mut f := Fmt{
		out: strings.new_builder(1000)
		out_imports: strings.new_builder(200)
		table: table
		indent: 0
		file: file
	}
	f.cur_mod = 'main'
	for stmt in file.stmts {
		if stmt is ast.Import {
			// Just remember the position of the imports for now
			f.import_pos = f.out.len
			// f.imports(f.file.imports)
		}
		f.stmt(stmt)
	}
	// for comment in file.comments { println('$comment.line_nr $comment.text')	}
	f.imports(f.file.imports) // now that we have all autoimports, handle them
	res := f.out.str().trim_space() + '\n'
	return res[..f.import_pos] + f.out_imports.str() + res[f.import_pos..] // + '\n'
}

/*
fn (f mut Fmt) find_comment(line_nr int) {
	for comment in f.file.comments {
		if comment.line_nr == line_nr {
			f.writeln('// FFF $comment.line_nr $comment.text')
			return
		}
	}
}
*/
pub fn (mut f Fmt) write(s string) {
	if f.indent > 0 && f.empty_line {
		f.out.write(level_indent.repeat(f.indent))
		f.line_len += f.indent * 4
	}
	f.out.write(s)
	f.line_len += s.len
	f.empty_line = false
}

pub fn (mut f Fmt) writeln(s string) {
	if f.indent > 0 && f.empty_line {
		f.out.write(level_indent.repeat(f.indent))
	}
	f.out.writeln(s)
	f.empty_line = true
	f.line_len = 0
}

fn (mut f Fmt) mod(mod ast.Module) {
	f.cur_mod = mod.name
	if mod.is_skipped {
		return
	}
	f.writeln('module $mod.name\n')
}

fn (mut f Fmt) imports(imports []ast.Import) {
	if f.did_imports || imports.len == 0 {
		return
	}
	// f.import_pos = f.out.len
	f.did_imports = true
	/*
	if imports.len == 1 {
		imp_stmt_str := f.imp_stmt_str(imports[0])
		f.out_imports.writeln('import ${imp_stmt_str}\n')
	} else if imports.len > 1 {
	*/
	// f.out_imports.writeln('import (')
	for imp in imports {
		if imp.mod !in f.used_imports {
			// TODO bring back once only unused imports are removed
			// continue
		}
		// f.out_imports.write('\t')
		// f.out_imports.writeln(f.imp_stmt_str(imp))
		f.out_imports.write('import ')
		f.out_imports.writeln(f.imp_stmt_str(imp))
	}
	f.out_imports.writeln('')
	// f.out_imports.writeln(')\n')
	// }
}

fn (f Fmt) imp_stmt_str(imp ast.Import) string {
	is_diff := imp.alias != imp.mod && !imp.mod.ends_with('.' + imp.alias)
	imp_alias_suffix := if is_diff { ' as ${imp.alias}' } else { '' }
	return '${imp.mod}${imp_alias_suffix}'
}

fn (mut f Fmt) stmts(stmts []ast.Stmt) {
	f.indent++
	for stmt in stmts {
		f.stmt(stmt)
	}
	f.indent--
}

fn (mut f Fmt) stmt(node ast.Stmt) {
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
			f.is_assign = true
			f.write(' $it.op.str() ')
			for i, val in it.right {
				f.expr(val)
				if i < it.right.len - 1 {
					f.write(', ')
				}
			}
			if !f.single_line_if {
				f.writeln('')
			}
			f.is_assign = false
		}
		ast.AssertStmt {
			f.write('assert ')
			f.expr(it.expr)
			f.writeln('')
		}
		ast.Attr {
			f.writeln('[$it.name]')
		}
		ast.Block {
			f.writeln('{')
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.BranchStmt {
			match it.tok.kind {
				.key_break { f.writeln('break') }
				.key_continue { f.writeln('continue') }
				else {}
			}
		}
		ast.Comment {
			f.comment(it)
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
		ast.ConstDecl {
			if it.is_pub {
				f.write('pub ')
			}
			f.writeln('const (')
			mut max := 0
			for field in it.fields {
				if field.name.len > max {
					max = field.name.len
				}
			}
			f.indent++
			for field in it.fields {
				name := field.name.after('.')
				f.write('$name ')
				f.write(strings.repeat(` `, max - field.name.len))
				f.write('= ')
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
			name := it.name.after('.')
			f.writeln('enum $name {')
			for field in it.fields {
				f.write('\t$field.name')
				if field.has_expr {
					f.write(' = ')
					f.expr(field.expr)
				}
				f.writeln('')
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
			f.fn_decl(it)
		}
		ast.ForCStmt {
			f.write('for ')
			if it.has_init {
				f.single_line_if = true // to keep all for ;; exprs on the same line
				f.stmt(it.init)
				f.single_line_if = false
			}
			f.write('; ')
			f.expr(it.cond)
			f.write('; ')
			f.expr(it.inc)
			f.writeln(' {')
			f.stmts(it.stmts)
			f.writeln('}')
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
			if it.is_inf {
				f.writeln('{')
			} else {
				f.writeln(' {')
			}
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.GlobalDecl {
			f.write('__global $it.name ')
			f.write(f.type_to_str(it.typ))
			if it.has_expr {
				f.write(' = ')
				f.expr(it.expr)
			}
		}
		ast.GoStmt {
			f.write('go ')
			f.expr(it.call_expr)
			f.writeln('')
		}
		ast.GotoLabel {
			f.writeln('$it.name:')
		}
		ast.GotoStmt {
			f.writeln('goto $it.name')
		}
		ast.HashStmt {
			f.writeln('#$it.val')
		}
		ast.Import {
			// Imports are handled after the file is formatted, to automatically add necessary modules
			// f.imports(f.file.imports)
		}
		ast.InterfaceDecl {
			f.writeln('interface $it.name {')
			for method in it.methods {
				f.write('\t')
				f.writeln(method.str(f.table).after('fn '))
			}
			f.writeln('}\n')
		}
		ast.Module {
			f.mod(it)
		}
		ast.Return {
			f.write('return')
			if it.exprs.len > 1 {
				// multiple returns
				f.write(' ')
				for i, expr in it.exprs {
					f.expr(expr)
					if i < it.exprs.len - 1 {
						f.write(', ')
					}
				}
			} else if it.exprs.len == 1 {
				// normal return
				f.write(' ')
				f.expr(it.exprs[0])
			}
			f.writeln('')
		}
		ast.StructDecl {
			f.struct_decl(it)
		}
		ast.TypeDecl {
			// already handled in f.imports
			f.type_decl(it)
		}
		ast.UnsafeStmt {
			f.writeln('unsafe {')
			f.stmts(it.stmts)
			f.writeln('}')
		}
	}
}

fn (mut f Fmt) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl {
			if it.is_pub {
				f.write('pub ')
			}
			ptype := f.type_to_str(it.parent_type)
			f.write('type $it.name $ptype')
		}
		ast.FnTypeDecl {
			if it.is_pub {
				f.write('pub ')
			}
			typ_sym := f.table.get_type_symbol(it.typ)
			fn_typ_info := typ_sym.info as table.FnType
			fn_info := fn_typ_info.func
			fn_name := it.name.replace(f.cur_mod + '.', '')
			f.write('type $fn_name = fn (')
			for i, arg in fn_info.args {
				f.write(arg.name)
				mut s := f.table.type_to_str(arg.typ)
				if arg.is_mut {
					f.write('mut ')
					if s.starts_with('&') {
						s = s[1..]
					}
				}
				is_last_arg := i == fn_info.args.len - 1
				should_add_type := is_last_arg || fn_info.args[i + 1].typ != arg.typ || (fn_info.is_variadic &&
					i == fn_info.args.len - 2)
				if should_add_type {
					if fn_info.is_variadic && is_last_arg {
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
			if fn_info.return_type.idx() != table.void_type_idx {
				ret_str := f.table.type_to_str(fn_info.return_type)
				f.write(' ' + ret_str)
			}
		}
		ast.SumTypeDecl {
			if it.is_pub {
				f.write('pub ')
			}
			f.write('type $it.name = ')
			mut sum_type_names := []string{}
			for t in it.sub_types {
				sum_type_names << f.type_to_str(t)
			}
			sum_type_names.sort()
			f.write(sum_type_names.join(' | '))
		}
	}
	f.writeln('\n')
}

fn (mut f Fmt) struct_decl(node ast.StructDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	name := node.name.after('.')
	f.writeln('struct $name {')
	mut max := 0
	for field in node.fields {
		if field.name.len > max {
			max = field.name.len
		}
	}
	for i, field in node.fields {
		if i == node.mut_pos {
			f.writeln('mut:')
		} else if i == node.pub_pos {
			f.writeln('pub:')
		} else if i == node.pub_mut_pos {
			f.writeln('pub mut:')
		}
		if field.comment.text != '' && field.comment.pos.line_nr < field.pos.line_nr {
			// Comment on the previous line
			f.write('\t')
			f.comment(field.comment)
		}
		f.write('\t$field.name ')
		f.write(strings.repeat(` `, max - field.name.len))
		f.write(f.type_to_str(field.typ))
		if field.has_default_expr {
			f.write(' = ${field.default_expr.str()}')
		}
		// f.write('// $field.pos.line_nr')
		if field.comment.text != '' && field.comment.pos.line_nr == field.pos.line_nr {
			// Same line comment
			f.write('  ')
			f.comment(field.comment)
		} else {
			// if field.comment.text != '' {
			// f.write (' // com linenr=$field.comment.pos.line_nr')
			// }
			f.writeln('')
		}
	}
	f.writeln('}\n')
}

fn (f &Fmt) type_to_str(t table.Type) string {
	mut res := f.table.type_to_str(t)
	// type_ptr => &type
	if res.ends_with('_ptr') {
		res = res[0..res.len - 4]
		start_pos := 2 * res.count('[]')
		res = res[0..start_pos] + '&' + res[start_pos..res.len]
	}
	return res.replace(f.cur_mod + '.', '')
}

fn (mut f Fmt) expr(node ast.Expr) {
	match node {
		ast.AnonFn {
			f.fn_decl(it.decl)
		}
		ast.ArrayInit {
			f.array_init(it)
		}
		ast.AsCast {
			type_str := f.type_to_str(it.typ)
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
			f.write(f.type_to_str(it.typ) + '(')
			f.expr(it.expr)
			f.write(')')
		}
		ast.CallExpr {
			f.call_expr(it)
		}
		ast.CharLiteral {
			f.write('`$it.val`')
		}
		ast.ConcatExpr {
			for i, val in it.vals {
				if i != 0 {
					f.write(' + ')
				}
				f.expr(val)
			}
		}
		ast.EnumVal {
			name := short_module(it.enum_name)
			f.write(name + '.' + it.val)
		}
		ast.FloatLiteral {
			f.write(it.val)
		}
		ast.IfExpr {
			f.if_expr(it)
		}
		ast.Ident {
			if it.kind == .blank_ident {
				f.write('_')
			} else {
				name := short_module(it.name)
				// f.write('<$it.name => $name>')
				f.write(name)
				if name.contains('.') {
					f.mark_module_as_used(name)
				}
			}
		}
		ast.IfGuardExpr {
			f.write(it.var_name + ' := ')
			f.expr(it.expr)
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
			if it.keys.len == 0 {
				if it.value_type == 0 {
					f.write('map[string]int') // TODO
					return
				}
				f.write('map[string]')
				f.write(f.type_to_str(it.value_type))
				return
			}
			f.writeln('{')
			f.indent++
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
			f.match_expr(it)
		}
		ast.None {
			f.write('none')
		}
		ast.OrExpr {
			// shouldn't happen, an or expression
			// is always linked to a call expr
			panic('fmt: OrExpr should to linked to CallExpr')
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
		ast.SizeOf {
			f.write('sizeof(')
			if it.type_name != '' {
				f.write(it.type_name)
			} else {
				f.write(f.type_to_str(it.typ))
			}
			f.write(')')
		}
		ast.StringLiteral {
			if it.val.contains("'") && !it.val.contains('"') {
				f.write('"$it.val"')
			} else {
				f.write("'$it.val'")
			}
		}
		ast.StringInterLiteral {
			f.write("'")
			for i, val in it.vals {
				f.write(val)
				if i >= it.exprs.len {
					continue
				}
				f.write('$')
				if it.expr_fmts[i].len > 0 {
					f.write('{')
					f.expr(it.exprs[i])
					f.write(it.expr_fmts[i])
					f.write('}')
				} else {
					f.expr(it.exprs[i])
				}
			}
			f.write("'")
		}
		ast.StructInit {
			type_sym := f.table.get_type_symbol(it.typ)
			// f.write('<old name: $type_sym.name>')
			mut name := short_module(type_sym.name).replace(f.cur_mod + '.', '') // TODO f.type_to_str?
			if name == 'void' {
				name = ''
			}
			if it.fields.len == 0 {
				// `Foo{}` on one line if there are no fields
				f.write('$name{}')
			} else if it.fields.len == 0 {
				// `Foo{1,2,3}` (short syntax )
				f.write('$name{')
				for i, field in it.fields {
					f.expr(field.expr)
					if i < it.fields.len - 1 {
						f.write(', ')
					}
				}
				f.write('}')
			} else {
				f.writeln('$name{')
				f.indent++
				for field in it.fields {
					f.write('$field.name: ')
					f.expr(field.expr)
					f.writeln('')
				}
				f.indent--
				f.write('}')
			}
		}
		ast.Type {
			f.write(f.type_to_str(it.typ))
		}
		ast.TypeOf {
			f.write('typeof(')
			f.expr(it.expr)
			f.write(')')
		}
	}
}

fn (mut f Fmt) wrap_long_line() bool {
	if f.line_len <= max_len {
		return false
	}
	if f.out.buf[f.out.buf.len - 1] == ` ` {
		f.out.go_back(1)
	}
	f.write('\n' + level_indent.repeat(f.indent + 1))
	f.line_len = 0
	return true
}

fn (mut f Fmt) call_args(args []ast.CallArg) {
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

fn (mut f Fmt) or_expr(or_block ast.OrExpr) {
	if or_block.is_used {
		f.writeln(' or {')
		f.stmts(or_block.stmts)
		f.write('}')
	}
}

fn (mut f Fmt) comment(node ast.Comment) {
	if !node.text.contains('\n') {
		is_separate_line := node.text.starts_with('|')
		mut s := if is_separate_line { node.text[1..] } else { node.text }
		if s == '' {
			s = '//'
		} else {
			s = '// ' + s
		}
		if !is_separate_line {
			f.remove_new_line() // delete the generated \n
			f.write(' ')
		}
		f.writeln(s)
		return
	}
	lines := node.text.split_into_lines()
	f.writeln('/*')
	for line in lines {
		f.writeln(line)
		f.empty_line = false
	}
	f.empty_line = true
	f.writeln('*/')
}

fn (mut f Fmt) fn_decl(node ast.FnDecl) {
	// println('$it.name find_comment($it.pos.line_nr)')
	// f.find_comment(it.pos.line_nr)
	s := node.str(f.table)
	f.write(s.replace(f.cur_mod + '.', '')) // `Expr` instead of `ast.Expr` in mod ast
	if !node.is_c && !node.is_js {
		f.writeln(' {')
		f.stmts(node.stmts)
		f.write('}')
		if !node.is_anon {
			f.writeln('\n')
		}
	} else {
		f.writeln('\n')
	}
	// Mark all function's used type so that they are not removed from imports
	for arg in node.args {
		f.mark_types_module_as_used(arg.typ)
	}
	f.mark_types_module_as_used(node.return_type)
}

// foo.bar.fn() => bar.fn()
fn short_module(name string) string {
	if !name.contains('.') {
		return name
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	return vals[vals.len - 2] + '.' + vals[vals.len - 1]
}

fn (mut f Fmt) if_expr(it ast.IfExpr) {
	single_line := it.branches.len == 2 && it.has_else && it.branches[0].stmts.len == 1 &&
		it.branches[1].stmts.len == 1 && (it.is_expr || f.is_assign)
	f.single_line_if = single_line
	for i, branch in it.branches {
		if branch.comment.text != '' {
			f.comment(branch.comment)
		}
		if i == 0 {
			f.write('if ')
			f.expr(branch.cond)
			f.write(' {')
		} else if i < it.branches.len - 1 || !it.has_else {
			f.write('} else if ')
			f.expr(branch.cond)
			f.write(' {')
		} else if i == it.branches.len - 1 && it.has_else {
			f.write('} else {')
		}
		if single_line {
			f.write(' ')
		} else {
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

fn (mut f Fmt) call_expr(node ast.CallExpr) {
	if node.is_method {
		if node.left is ast.Ident {
			it := node.left as ast.Ident
			// `time.now()` without `time imported` is processed as a method call with `time` being
			// a `node.left` expression. Import `time` automatically.
			// TODO fetch all available modules
			if it.name in ['time', 'os', 'strings', 'math', 'json', 'base64'] {
				if it.name !in f.auto_imports {
					f.auto_imports << it.name
					f.file.imports << ast.Import{
						mod: it.name
						alias: it.name
					}
				}
				// for imp in f.file.imports {
				// println(imp.mod)
				// }
			}
		}
		f.expr(node.left)
		f.write('.' + node.name + '(')
		f.call_args(node.args)
		f.write(')')
		f.or_expr(node.or_block)
	} else {
		name := short_module(node.name)
		f.mark_module_as_used(name)
		f.write('${name}(')
		f.call_args(node.args)
		f.write(')')
		f.or_expr(node.or_block)
	}
}

fn (mut f Fmt) match_expr(it ast.MatchExpr) {
	f.write('match ')
	if it.is_mut {
		f.write('mut ')
	}
	f.expr(it.cond)
	f.writeln(' {')
	f.indent++
	mut single_line := true
	for branch in it.branches {
		if branch.stmts.len > 1 {
			single_line = false
			break
		}
		if branch.stmts.len == 0 {
			continue
		}
		stmt := branch.stmts[0]
		if stmt is ast.ExprStmt {
			// If expressions inside match branches can't be one a single line
			expr_stmt := stmt as ast.ExprStmt
			if !expr_is_single_line(expr_stmt.expr) {
				single_line = false
				break
			}
		} else if stmt is ast.Comment {
			single_line = false
			break
		}
	}
	for branch in it.branches {
		if branch.comment.text != '' {
			f.comment(branch.comment)
		}
		if !branch.is_else {
			// normal branch
			for j, expr in branch.exprs {
				f.expr(expr)
				if j < branch.exprs.len - 1 {
					f.write(', ')
				}
			}
		} else {
			// else branch
			f.write('else')
		}
		if branch.stmts.len == 0 {
			f.writeln(' {}')
		} else {
			if single_line {
				f.write(' { ')
			} else {
				f.writeln(' {')
			}
			f.stmts(branch.stmts)
			if single_line {
				f.remove_new_line()
				f.writeln(' }')
			} else {
				f.writeln('}')
			}
		}
	}
	f.indent--
	f.write('}')
}

fn (mut f Fmt) remove_new_line() {
	mut i := 0
	for i = f.out.len - 1; i >= 0; i-- {
		if !f.out.buf[i].is_space() { // != `\n` {
			break
		}
	}
	f.out.go_back(f.out.len - i - 1)
	f.empty_line = false
	// f.writeln('sdf')
}

fn (mut f Fmt) mark_types_module_as_used(typ table.Type) {
	sym := f.table.get_type_symbol(typ)
	f.mark_module_as_used(sym.name)
}

// `name` is a function (`foo.bar()`) or type (`foo.Bar{}`)
fn (mut f Fmt) mark_module_as_used(name string) {
	if !name.contains('.') {
		return
	}
	pos := name.last_index('.') or {
		0
	}
	mod := name[..pos]
	if mod in f.used_imports {
		return
	}
	f.used_imports << mod
	// println('marking module $mod as used')
}

fn expr_is_single_line(expr ast.Expr) bool {
	match expr {
		ast.IfExpr { return false }
		else {}
	}
	return true
}

fn (mut f Fmt) array_init(it ast.ArrayInit) {
	if it.exprs.len == 0 && it.typ != 0 && it.typ != table.void_type {
		// `x := []string`
		f.write(f.type_to_str(it.typ))
		f.write('{')
		if it.has_cap {
			f.write('cap: ')
			f.expr(it.cap_expr)
		}
		f.write('}')
		return
	}
	// `[1,2,3]`
	// type_sym := f.table.get_type_symbol(it.typ)
	f.write('[')
	mut inc_indent := false
	mut last_line_nr := it.pos.line_nr // to have the same newlines between array elements
	for i, expr in it.exprs {
		line_nr := expr.position().line_nr
		if last_line_nr < line_nr {
			if !inc_indent {
				f.indent++
				inc_indent = true
			}
			f.writeln('')
		}
		is_new_line := last_line_nr < line_nr || f.wrap_long_line()
		if !is_new_line && i > 0 {
			f.write(' ')
		}
		f.expr(expr)
		if i == it.exprs.len - 1 {
			if is_new_line {
				f.writeln('')
			}
		} else {
			f.write(',')
		}
		last_line_nr = line_nr
	}
	if inc_indent {
		f.indent--
	}
	f.write(']')
}
