module c

import strings
import v3.ast

const c_reserved_words = ['auto', 'break', 'case', 'char', 'const', 'continue', 'copy', 'default',
	'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
	'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch',
	'typedef', 'union', 'unsigned', 'void', 'volatile', 'while']

fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	n := name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('.', '__')
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}

pub struct Gen {
mut:
	sb           strings.Builder
	indent       int
	cur_fn       string
	had_main     bool
	fn_decls     []ast.FnDecl
	str_lits     []string
	str_lit_id   int
	fn_ret_types map[string]string
	var_types    map[string]string
}

pub fn Gen.new() Gen {
	return Gen{
		sb: strings.new_builder(4096)
	}
}

pub fn (mut g Gen) gen(files []ast.File) string {
	g.collect(files)
	// Pass 1: generate function bodies to discover string literals
	orig_sb := g.sb
	g.sb = strings.new_builder(4096)
	g.gen_fns()
	fn_code := g.sb.str()
	// Pass 2: write final output with string literals declared before use
	g.sb = orig_sb
	g.preamble()
	g.forward_decls()
	g.string_literals()
	g.sb.write_string(fn_code)
	return g.sb.str()
}

fn (mut g Gen) collect(files []ast.File) {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				g.fn_decls << stmt
				if stmt.name == 'main' && !stmt.is_method {
					g.had_main = true
				}
				ret_type := g.c_return_type(stmt.typ)
				g.fn_ret_types[stmt.name] = ret_type
				for p in stmt.typ.params {
					g.var_types[p.name] = g.c_type_name(p.typ)
				}
			}
		}
	}
}

fn (mut g Gen) preamble() {
	g.writeln('#include <stdio.h>')
	g.writeln('#include <stdlib.h>')
	g.writeln('#include <string.h>')
	g.writeln('')
	g.writeln('typedef struct {')
	g.writeln('\tchar* str;')
	g.writeln('\tint len;')
	g.writeln('\tint is_lit;')
	g.writeln('} string;')
	g.writeln('')
	g.writeln('typedef signed char i8;')
	g.writeln('typedef short i16;')
	g.writeln('typedef int i32;')
	g.writeln('typedef long long i64;')
	g.writeln('typedef unsigned char u8;')
	g.writeln('typedef unsigned char byte;')
	g.writeln('typedef unsigned short u16;')
	g.writeln('typedef unsigned int u32;')
	g.writeln('typedef unsigned long long u64;')
	g.writeln('typedef int bool;')
	g.writeln('#define true 1')
	g.writeln('#define false 0')
	g.writeln('')
	g.writeln('void println(string s) {')
	g.writeln('\tfwrite(s.str, 1, s.len, stdout);')
	g.writeln('\tputchar(10);')
	g.writeln('}')
	g.writeln('')
	g.writeln('void print(string s) {')
	g.writeln('\tfwrite(s.str, 1, s.len, stdout);')
	g.writeln('}')
	g.writeln('')
	g.writeln('string int_str(int n) {')
	g.writeln('\tstatic char buf[20];')
	g.writeln('\tint len = snprintf(buf, sizeof(buf), "%d", n);')
	g.writeln('\treturn (string){buf, len, 1};')
	g.writeln('}')
	g.writeln('')
	g.writeln('string string__plus(string a, string b) {')
	g.writeln('\tint len = a.len + b.len;')
	g.writeln('\tchar* s = malloc(len + 1);')
	g.writeln('\tmemcpy(s, a.str, a.len);')
	g.writeln('\tmemcpy(s + a.len, b.str, b.len);')
	g.writeln('\ts[len] = 0;')
	g.writeln('\treturn (string){s, len, 0};')
	g.writeln('}')
	g.writeln('')
}

fn (mut g Gen) forward_decls() {
	for fn_decl in g.fn_decls {
		if fn_decl.name == 'main' {
			continue
		}
		g.write(g.c_return_type(fn_decl.typ))
		g.write(' ')
		g.write(c_name(fn_decl.name))
		g.write('(')
		g.fn_params(fn_decl.typ.params)
		g.writeln(');')
	}
	g.writeln('')
}

fn (mut g Gen) string_literals() {
	for i, s in g.str_lits {
		g.writeln("string _str_${i} = {\"${c_escape(s)}\", ${s.len}, 1};")
	}
	if g.str_lits.len > 0 {
		g.writeln('')
	}
}

fn (mut g Gen) gen_fns() {
	for fn_decl in g.fn_decls {
		g.cur_fn = fn_decl.name
		g.var_types = map[string]string{}
		for param in fn_decl.typ.params {
			if param.name.len > 0 {
				g.var_types[param.name] = g.c_type_name(param.typ)
			}
		}
		if fn_decl.name == 'main' {
			g.writeln('int main(int argc, char** argv) {')
		} else {
			g.write(g.c_return_type(fn_decl.typ))
			g.write(' ')
			g.write(c_name(fn_decl.name))
			g.write('(')
			g.fn_params(fn_decl.typ.params)
			g.writeln(') {')
		}
		g.indent++
		g.stmts(fn_decl.stmts)
		if fn_decl.name == 'main' {
			g.writeln('return 0;')
		}
		g.indent--
		g.writeln('}')
		g.writeln('')
	}
}

fn (mut g Gen) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.stmt(stmt)
	}
}

fn (mut g Gen) stmt(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt {
			g.expr(stmt.expr)
			g.writeln(';')
		}
		ast.AssignStmt {
			g.assign_stmt(stmt)
		}
		ast.ReturnStmt {
			g.write('return')
			if stmt.exprs.len > 0 {
				g.write(' ')
				g.expr(stmt.exprs[0])
			}
			g.writeln(';')
		}
		ast.FnDecl {
			// nested function declarations not supported in C
		}
		ast.ForStmt {
			g.for_stmt(stmt)
		}
		ast.FlowControlStmt {
			match stmt.op {
				.key_break { g.writeln('break;') }
				.key_continue { g.writeln('continue;') }
				else {}
			}
		}
		ast.BlockStmt {
			g.writeln('{')
			g.indent++
			g.stmts(stmt.stmts)
			g.indent--
			g.writeln('}')
		}
		ast.ImportStmt, ast.ModuleStmt, []ast.Attribute {}
		else {}
	}
}

fn (mut g Gen) for_stmt(stmt ast.ForStmt) {
	if stmt.init is ast.EmptyStmt && stmt.cond is ast.EmptyExpr && stmt.post is ast.EmptyStmt {
		g.writeln('for (;;) {')
	} else if stmt.init is ast.EmptyStmt && stmt.post is ast.EmptyStmt {
		g.write('while (')
		g.expr(stmt.cond)
		g.writeln(') {')
	} else {
		g.write('for (')
		if stmt.init !is ast.EmptyStmt {
			g.stmt_inline(stmt.init)
		}
		g.write('; ')
		if stmt.cond !is ast.EmptyExpr {
			g.expr(stmt.cond)
		}
		g.write('; ')
		if stmt.post !is ast.EmptyStmt {
			g.stmt_inline(stmt.post)
		}
		g.writeln(') {')
	}
	g.indent++
	g.stmts(stmt.stmts)
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) stmt_inline(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt {
			g.expr(stmt.expr)
		}
		ast.AssignStmt {
			lhs := g.unwrap_modifier(stmt.lhs[0])
			if stmt.op == .decl_assign {
				typ := g.infer_type(stmt.rhs[0])
				g.write('${typ} ')
			}
			g.expr(lhs)
			if stmt.op == .decl_assign {
				g.write(' = ')
			} else {
				g.write(' ${stmt.op.str()} ')
			}
			g.expr(stmt.rhs[0])
		}
		else {}
	}
}

fn (mut g Gen) assign_stmt(stmt ast.AssignStmt) {
	if stmt.op == .decl_assign {
		for i in 0 .. stmt.lhs.len {
			typ := g.infer_type(stmt.rhs[i])
			lhs := g.unwrap_modifier(stmt.lhs[i])
			g.write('${typ} ')
			g.expr(lhs)
			g.write(' = ')
			g.expr(stmt.rhs[i])
			g.writeln(';')
			if lhs is ast.Ident {
				g.var_types[lhs.name] = typ
			}
		}
	} else {
		for i in 0 .. stmt.lhs.len {
			g.expr(stmt.lhs[i])
			g.write(' ${stmt.op.str()} ')
			g.expr(stmt.rhs[i])
			g.writeln(';')
		}
	}
}

fn (g &Gen) unwrap_modifier(expr ast.Expr) ast.Expr {
	if expr is ast.ModifierExpr {
		return expr.expr
	}
	return expr
}

fn (g &Gen) infer_type(expr ast.Expr) string {
	match expr {
		ast.BasicLiteral {
			return match expr.kind {
				.number { 'int' }
				.key_true, .key_false { 'bool' }
				.char { 'u8' }
				else { 'int' }
			}
		}
		ast.StringLiteral {
			return 'string'
		}
		ast.StringInterLiteral {
			return 'string'
		}
		ast.CallExpr {
			fn_name := expr.lhs.name()
			if ret := g.fn_ret_types[fn_name] {
				return ret
			}
			return 'int'
		}
		ast.CallOrCastExpr {
			fn_name := expr.lhs.name()
			if ret := g.fn_ret_types[fn_name] {
				return ret
			}
			return 'int'
		}
		ast.Ident {
			if typ := g.var_types[expr.name] {
				return typ
			}
			return 'int'
		}
		ast.InfixExpr {
			lhs_type := g.infer_type(expr.lhs)
			if lhs_type == 'string' {
				return 'string'
			}
			rhs_type := g.infer_type(expr.rhs)
			if rhs_type == 'string' {
				return 'string'
			}
			return lhs_type
		}
		ast.PrefixExpr {
			return g.infer_type(expr.expr)
		}
		ast.ParenExpr {
			return g.infer_type(expr.expr)
		}
		else {
			return 'int'
		}
	}
}

fn (mut g Gen) expr(expr ast.Expr) {
	match expr {
		ast.BasicLiteral {
			match expr.kind {
				.key_true { g.write('true') }
				.key_false { g.write('false') }
				else { g.write(expr.value) }
			}
		}
		ast.StringLiteral {
			id := g.intern_string(expr.value)
			g.write('_str_${id}')
		}
		ast.StringInterLiteral {
			g.string_inter(expr)
		}
		ast.Ident {
			g.write(c_name(expr.name))
		}
		ast.CallExpr {
			g.call_expr(expr)
		}
		ast.CallOrCastExpr {
			fn_name := expr.lhs.name()
			if fn_name == 'println' || fn_name == 'print' {
				g.write(fn_name)
				g.write('(')
				arg_type := g.infer_type(expr.expr)
				if arg_type == 'string' {
					g.expr(expr.expr)
				} else {
					g.write('int_str(')
					g.expr(expr.expr)
					g.write(')')
				}
				g.write(')')
			} else {
				g.expr(expr.lhs)
				g.write('(')
				g.expr(expr.expr)
				g.write(')')
			}
		}
		ast.SelectorExpr {
			g.expr(expr.lhs)
			g.write('.')
			g.write(expr.rhs.name)
		}
		ast.InfixExpr {
			if expr.op == .plus {
				if g.is_string_expr(expr.lhs) || g.is_string_expr(expr.rhs) {
					g.write('string__plus(')
					g.expr(expr.lhs)
					g.write(', ')
					g.expr(expr.rhs)
					g.write(')')
					return
				}
			}
			g.expr(expr.lhs)
			g.write(' ${expr.op.str()} ')
			g.expr(expr.rhs)
		}
		ast.PrefixExpr {
			g.write(expr.op.str())
			g.expr(expr.expr)
		}
		ast.PostfixExpr {
			g.expr(expr.expr)
			g.write(expr.op.str())
		}
		ast.ParenExpr {
			g.write('(')
			g.expr(expr.expr)
			g.write(')')
		}
		ast.IfExpr {
			g.if_expr(expr)
		}
		ast.IndexExpr {
			g.expr(expr.lhs)
			g.write('[')
			g.expr(expr.expr)
			g.write(']')
		}
		ast.ArrayInitExpr {
			g.write('{0}')
		}
		ast.MapInitExpr {
			g.write('{0}')
		}
		ast.EmptyExpr {}
		else {}
	}
}

fn (mut g Gen) call_expr(expr ast.CallExpr) {
	fn_name := expr.lhs.name()
	match fn_name {
		'println', 'print' {
			g.write(fn_name)
			g.write('(')
			if expr.args.len > 0 {
				arg := expr.args[0]
				arg_type := g.infer_type(arg)
				if arg_type == 'string' {
					g.expr(arg)
				} else {
					g.write('int_str(')
					g.expr(arg)
					g.write(')')
				}
			}
			g.write(')')
		}
		else {
			g.expr(expr.lhs)
			g.write('(')
			for i, arg in expr.args {
				g.expr(arg)
				if i < expr.args.len - 1 {
					g.write(', ')
				}
			}
			g.write(')')
		}
	}
}

fn (g &Gen) is_string_expr(expr ast.Expr) bool {
	return g.infer_type(expr) == 'string'
}

fn (mut g Gen) string_inter(expr ast.StringInterLiteral) {
	mut parts := []string{}
	for i, val in expr.values {
		if val.len > 0 {
			parts << 'str_${val.len}'
		}
		if i < expr.inters.len {
			parts << 'inter_${i}'
		}
	}
	mut first := true
	for i, val in expr.values {
		if val.len > 0 {
			id := g.intern_string(val)
			if first {
				g.write('_str_${id}')
				first = false
			} else {
				prev := g.sb.str()
				g.sb = strings.new_builder(4096)
				g.sb.write_string(prev[..prev.len])
			}
		}
		if i < expr.inters.len {
			inter := expr.inters[i]
			if !first {
				g.write(', ')
			}
			g.write('int_str(')
			g.expr(inter.expr)
			g.write(')')
			first = false
		}
	}
}

fn (mut g Gen) if_expr(expr ast.IfExpr) {
	if expr.cond !is ast.EmptyExpr {
		g.write('if (')
		g.expr(expr.cond)
		g.writeln(') {')
	} else {
		g.writeln('{')
	}
	g.indent++
	g.stmts(expr.stmts)
	g.indent--
	if expr.else_expr is ast.IfExpr {
		g.write('} else ')
		g.if_expr(expr.else_expr)
	} else if expr.else_expr !is ast.EmptyExpr {
		g.writeln('} else {')
		g.indent++
		if expr.else_expr is ast.IfExpr {
			g.if_expr(expr.else_expr)
		}
		g.indent--
		g.writeln('}')
	} else {
		g.writeln('}')
	}
}

fn (mut g Gen) fn_params(params []ast.Parameter) {
	if params.len == 0 {
		g.write('void')
		return
	}
	for i, param in params {
		g.write(g.c_type_name(param.typ))
		if param.name.len > 0 {
			g.write(' ')
			g.write(c_name(param.name))
		}
		if i < params.len - 1 {
			g.write(', ')
		}
	}
}

fn (g &Gen) c_return_type(typ ast.FnType) string {
	if typ.return_type is ast.EmptyExpr {
		return 'void'
	}
	return g.c_type_name(typ.return_type)
}

fn (g &Gen) c_type_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return match expr.name {
				'int' { 'int' }
				'i8' { 'i8' }
				'i16' { 'i16' }
				'i32' { 'i32' }
				'i64' { 'i64' }
				'u8', 'byte' { 'u8' }
				'u16' { 'u16' }
				'u32' { 'u32' }
				'u64' { 'u64' }
				'f32' { 'float' }
				'f64' { 'double' }
				'bool' { 'bool' }
				'string' { 'string' }
				'voidptr' { 'void*' }
				else { expr.name }
			}
		}
		ast.Type {
			match expr {
				ast.PointerType { return g.c_type_name(expr.base_type) + '*' }
				ast.ArrayType { return 'void*' }
				ast.OptionType { return g.c_type_name(expr.base_type) }
				ast.ResultType { return g.c_type_name(expr.base_type) }
				else { return 'int' }
			}
		}
		else {
			return 'int'
		}
	}
}

fn (mut g Gen) intern_string(s string) int {
	stripped := strip_quotes(s)
	for i, existing in g.str_lits {
		if existing == stripped {
			return i
		}
	}
	id := g.str_lits.len
	g.str_lits << stripped
	return id
}

fn strip_quotes(s string) string {
	if s.len >= 2 && ((s[0] == `'` && s[s.len - 1] == `'`) || (s[0] == `"` && s[s.len - 1] == `"`)) {
		return s[1..s.len - 1]
	}
	return s
}

fn (mut g Gen) write(s string) {
	if g.sb.len == 0 || g.sb.last_n(1) == '\n' {
		g.write_indent()
	}
	g.sb.write_string(s)
}

fn (mut g Gen) writeln(s string) {
	if s.len > 0 {
		if g.sb.len == 0 || g.sb.last_n(1) == '\n' {
			g.write_indent()
		}
		g.sb.write_string(s)
	}
	g.sb.write_string('\n')
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}

fn c_escape(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t').replace('\r',
		'\\r')
}
