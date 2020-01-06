// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	v.scanner
	v.ast
	v.token
	v.table
	v.types
	term
	os
)

struct Parser {
	scanner   &scanner.Scanner
	file_name string
mut:
	tok       token.Token
	peek_tok  token.Token
	// vars []string
	table     &table.Table
	return_ti types.TypeIdent
	is_c      bool
}

pub fn parse_stmt(text string, table &table.Table) ast.Stmt {
	s := scanner.new_scanner(text)
	mut p := Parser{
		scanner: s
		table: table
	}
	p.read_first_token()
	return p.stmt()
}

pub fn parse_file(path string, table &table.Table) ast.File {
	println('parse file "$path"')
	text := os.read_file(path) or {
		panic(err)
	}
	mut stmts := []ast.Stmt
	mut p := Parser{
		scanner: scanner.new_scanner(text)
		table: table
		file_name: path
	}
	p.read_first_token()
	for {
		// res := s.scan()
		if p.tok.kind == .eof {
			break
		}
		// println('expr at ' + p.tok.str())
		s := p.stmt()
		// println(s)
		stmts << s // p.stmt()
	}
	p.check_fn_calls()
	// println('nr stmts = $stmts.len')
	// println(stmts[0])
	return ast.File{
		stmts: stmts
	}
}

pub fn parse_files(paths []string, table &table.Table) []ast.File {
	mut files := []ast.File
	for path in paths {
		files << parse_file(path, table)
	}
	return files
}

pub fn (p mut Parser) read_first_token() {
	// need to call next() twice to get peek token and current token
	p.next()
	p.next()
}

pub fn (p mut Parser) parse_block() []ast.Stmt {
	p.check(.lcbr)
	mut stmts := []ast.Stmt
	for {
		// res := s.scan()
		if p.tok.kind in [.eof, .rcbr] {
			break
		}
		// println('expr at ' + p.tok.str())
		stmts << p.stmt()
	}
	p.check(.rcbr)
	// println('nr exprs in block = $exprs.len')
	return stmts
}

fn (p mut Parser) next() {
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
	// println(p.tok.str())
}

fn (p mut Parser) check(expected token.Kind) {
	if p.tok.kind != expected {
		s := 'syntax error: unexpected `${p.tok.kind.str()}`, expecting `${expected.str()}`'
		p.error(s)
	}
	p.next()
}

fn (p mut Parser) check_name() string {
	name := p.tok.lit
	p.check(.name)
	return name
}

pub fn (p mut Parser) stmt() ast.Stmt {
	// println('stmt at ' + p.tok.str())
	// `x := ...`
	if p.tok.kind == .name {
		if p.peek_tok.kind == .decl_assign {
			return p.var_decl()
		}
		else if p.peek_tok.is_assign() {
			return p.assign_stmt()
		}
	}
	match p.tok.kind {
		.key_module {
			return p.module_decl()
		}
		.key_import {
			return p.import_stmt()
		}
		.key_pub {
			match p.peek_tok.kind {
				.key_fn {
					return p.fn_decl()
				}
				.key_struct, .key_union, .key_interface {
					return p.struct_decl()
				}
				else {
					p.error('wrong pub keyword usage')
					return ast.Stmt{}
				}
		}
			// .key_const {
				// return p.const_decl()
			// }
			// .key_enum {
				// return p.enum_decl()
			// }
			// .key_type {
				// return p.type_decl()
			// }
		}
		.key_fn {
			return p.fn_decl()
		}
		.key_struct {
			return p.struct_decl()
		}
		.key_return {
			return p.return_stmt()
		}
		.key_mut {
			return p.var_decl()
		}
		.key_for {
			return p.for_statement()
		}
		else {
			expr,ti := p.expr(0)
			return ast.ExprStmt{
				expr: expr
				ti: ti
			}
		}
	}
}

pub fn (p mut Parser) assign_stmt() ast.AssignStmt {
	name := p.tok.lit
	// println('looking for $name')
	var := p.table.find_var(name) or {
		p.error('unknown variable `$name`')
		exit(1)
	}
	if !var.is_mut {
		p.error('`$var.name` is immutable, declare it with `mut $var.name := ...`')
	}
	left_expr,left_type := p.expr(0)
	op := p.tok.kind
	// println('assignn_stmt() ' + op.str())
	p.next()
	right_expr,right_type := p.expr(0)
	if !types.check(left_type, right_type) {
		p.error('oops')
	}
	return ast.AssignStmt{
		left: left_expr
		right: right_expr
		op: op
	}
}

pub fn (p &Parser) error(s string) {
	println(term.bold(term.red('$p.file_name:$p.tok.line_nr: $s')))
	exit(1)
}

pub fn (p &Parser) error_at_line(s string, line_nr int) {
	println(term.bold(term.red('$p.file_name:$line_nr: $s')))
	exit(1)
}

pub fn (p &Parser) warn(s string) {
	println(term.blue('$p.file_name:$p.tok.line_nr: $s'))
}

// Implementation of Pratt Precedence
pub fn (p mut Parser) expr(rbp int) (ast.Expr,types.TypeIdent) {
	// println('expr at ' + p.tok.str())
	// null denotation (prefix)
	mut node := ast.Expr{}
	// mut typ := types.void_type
	mut ti := types.new_base_ti(._void, 0)
	match p.tok.kind {
		.name {
			/*
			sym := p.table.find_symbol(p.tok.lit)
			if sym.cat == .function {
				return
			}
			*/
			if p.tok.lit == 'C' {
				p.is_c = true
				println('is c')
				p.next()
				p.check(.dot)
			}
			// fn call
			if p.peek_tok.kind == .lpar {
				x,ti2 := p.call_expr() // TODO `node,typ :=` should work
				node = x
				ti = ti2
			}
			// struct init
			else if p.peek_tok.kind == .lcbr {
				ti = p.parse_ti()
				// println('sturct init typ=$typ.name')
				p.check(.lcbr)
				mut field_names := []string
				mut exprs := []ast.Expr
				for p.tok.kind != .rcbr {
					field_name := p.check_name()
					field_names << field_name
					p.check(.colon)
					// expr,field_type := p.expr(0)
					expr,_ := p.expr(0)
					// if !types.check(   ,field_type
					exprs << expr
				}
				node = ast.StructInit{
					ti: ti
					exprs: exprs
					fields: field_names
				}
				p.check(.rcbr)
			}
			else {
				// name expr
				node = ast.Ident{
					name: p.tok.lit
				}
				var := p.table.find_var(p.tok.lit) or {
					p.error('unknown variable `$p.tok.lit`')
					exit(0)
				}
				ti = var.ti
				// ///typ = types.int_type
				p.next()
			}
		}
		.lsbr {
			node,ti = p.array_init()
		}
		.key_true, .key_false {
			node = ast.BoolLiteral{
				val: p.tok.kind == .key_true
			}
			ti = types.new_base_ti(._bool, 0)
			p.next()
		}
		.str {
			node,ti = p.parse_string_literal()
		}
		.number {
			node,ti = p.parse_number_literal()
		}
		.key_if {
			node,ti = p.if_expr()
		}
		.lpar {
			p.check(.lpar)
			p.next()
			node,ti = p.expr(token.lowest_prec)
			p.check(.rpar)
		}
		else {
			if p.tok.is_unary() {
				pt := p.tok
				p.next()
				expr,ti2 := p.expr(token.lowest_prec)
				node = ast.UnaryExpr{
					left: expr
					op: pt.kind
				}
				ti = ti2
			}
			else {
				p.error('expr(): unknown token ' + p.tok.str() + ' kind=$p.tok.kind')
			}
		}
	}
	// left binding power
	for rbp < p.tok.precedence() {
		prev_tok := p.tok
		p.next()
		// mut t2 := types.Type{}
		mut ti2 := types.new_base_ti(._void, 0)
		// left denotation (infix / postfix)
		if prev_tok.is_right_assoc() && !p.tok.kind in [.plus, .minus] && // think of better way to handle this
		!p.peek_tok.kind in [.number, .name] {
			// supposed to be only unary, additive handled in left asssoc
			mut expr := ast.Expr{}
			expr,ti2 = p.expr(prev_tok.precedence() - 1)
			node = ast.BinaryExpr{
				left: node
				op: prev_tok.kind
				right: expr
			}
			// println(t2.name + 'OOO')
			if !types.check(&ti, &ti2) {
				println('tok: $prev_tok.str()')
				p.error('cannot convert `$ti2.type_name` to `$ti.type_name`')
			}
		}
		else if prev_tok.is_left_assoc() {
			// postfix `.`
			if prev_tok.kind == .dot {
				p.warn('dot prev_tok = $prev_tok.str() typ=$ti.type_name')
				// p.next()
				field := p.check_name()
				if !ti.type_kind in [._placeholder, ._struct] {
					println('kind: $ti.str()')
					p.error('cannot access field, `$ti.type_name` is not a struct')
				}
				typ := p.table.types[ti.type_idx] as types.Struct
				mut ok := false
				for f in typ.fields {
					if f.name == field {
						ok = true
					}
				}
				if !ok {
					p.error('unknown field `${typ.name}.$field`')
				}
				node = ast.SelectorExpr{
					expr: node
					field: field
				}
				// return node,typ
			}
			// postfix (`++` | `--`)
			else if prev_tok.kind in [.inc, .dec] {
				node = ast.UnaryExpr{
					left: node
					op: prev_tok.kind
				}
			}
			else {
				mut expr := ast.Expr{}
				expr,ti2 = p.expr(prev_tok.precedence() - 1)
				if prev_tok.is_relational() {
					// typ = types.bool_type
					ti = types.new_base_ti(._bool, 0)
				}
				else {
					ti = ti2
				}
				// println(ti2.type_name + '222')
				node = ast.BinaryExpr{
					left: node
					op: prev_tok.kind
					right: expr
				}
			}
		}
	}
	return node,ti
}

[inline]
fn (p &Parser) is_addative() bool {
	return p.tok.kind in [.plus, .minus] && p.peek_tok.kind in [.number, .name]
}

fn (p mut Parser) for_statement() ast.ForStmt {
	p.check(.key_for)
	// `for i in start .. end`
	if p.peek_tok.kind == .key_in {
		var := p.check_name()
		p.check(.key_in)
		start := p.tok.lit.int()
		p.check(.number)
		p.check(.dotdot)
		end := p.tok.lit.int()
		// println('for start=$start $end')
		p.check(.number)
		stmts := p.parse_block()
		// println('nr stmts=$stmts.len')
		return ast.ForStmt{
			stmts: stmts
			is_in: true
		}
	}
	// `for cond {`
	cond,ti := p.expr(0)
	// if !types.check(types.bool_type, ti) {
	if ti.type_kind != ._bool {
		p.error('non-bool used as for condition')
	}
	stmts := p.parse_block()
	return ast.ForStmt{
		cond: cond
		stmts: stmts
	}
}

fn (p mut Parser) if_expr() (ast.Expr,types.TypeIdent) {
	mut node := ast.Expr{}
	p.check(.key_if)
	cond,cond_type := p.expr(0)
	// if !types.check(types.bool_type, cond_type) {
	if cond_type.type_kind != ._bool {
		p.error('non-bool used as if condition')
	}
	stmts := p.parse_block()
	mut else_stmts := []ast.Stmt
	if p.tok.kind == .key_else {
		// println('GOT ELSE')
		p.check(.key_else)
		else_stmts = p.parse_block()
	}
	// mut typ := types.void_type
	mut ti := types.new_base_ti(._void, 0)
	// mut left := ast.Expr{}
	// If the last statement is an expression, return its type
	match stmts[stmts.len - 1] {
		ast.ExprStmt {
			p.warn('if expr ret $it.ti.type_name')
			ti = it.ti
			// return node,it.typ
			// left =
		}
		else {}
	}
	node = ast.IfExpr{
		cond: cond
		stmts: stmts
		else_stmts: else_stmts
		ti: ti
		// left: left
		
	}
	return node,ti
}

fn (p mut Parser) parse_string_literal() (ast.Expr,types.TypeIdent) {
	mut node := ast.Expr{}
	node = ast.StringLiteral{
		val: p.tok.lit
	}
	p.next()
	// return node,types.string_type
	return node,types.new_base_ti(._string, 0)
}

fn (p mut Parser) array_init() (ast.Expr,types.TypeIdent) {
	p.check(.lsbr)
	// mut val_type := types.void_type
	mut val_ti := types.new_base_ti(._void, 0)
	mut exprs := []ast.Expr
	mut i := 0
	for p.tok.kind != .rsbr {
		expr,ti := p.expr(0)
		// The first element's type
		if i == 0 {
			val_ti = ti
		}
		else if !types.check(val_ti, ti) {
			p.error('expected array element with type `$val_ti.type_name`')
		}
		exprs << expr
		i++
		if p.tok.kind == .comma {
			p.check(.comma)
		}
	}
	mut node := ast.Expr{}
	node = ast.ArrayInit{
		ti: val_ti
		exprs: exprs
	}
	p.check(.rsbr)
	return node,val_ti
}

fn (p mut Parser) parse_number_literal() (ast.Expr,types.TypeIdent) {
	lit := p.tok.lit
	mut node := ast.Expr{}
	mut ti := types.new_base_ti(._int, 0)
	if lit.contains('.') {
		node = ast.FloatLiteral{
			// val: lit.f64()
			val: lit
		}
		ti = types.new_base_ti(._f64, 0)
	}
	else {
		node = ast.IntegerLiteral{
			val: lit.int()
		}
		// ti = types.new_base_ti(._int, 0)
	}
	p.next()
	return node,ti
}

fn (p mut Parser) module_decl() ast.Module {
	p.check(.key_module)
	p.next()
	return ast.Module{}
}

fn (p mut Parser) import_stmt() ast.Import {
	p.check(.key_import)
	name := p.check_name()
	mut alias := name
	if p.tok.kind == .key_as {
		p.check(.key_as)
		alias = p.check_name()
	}
	mut mods := map[string]string
	mods[alias] = name
	return ast.Import{
		mods: mods
	}
}

fn (p mut Parser) struct_decl() ast.StructDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_struct)
	name := p.check_name()
	p.check(.lcbr)
	mut ast_fields := []ast.Field
	mut fields := []types.Field
	for p.tok.kind != .rcbr {
		if p.tok.kind == .key_pub {
			p.check(.key_pub)
			p.check(.colon)
		}
		field_name := p.check_name()
		ti := p.parse_ti()
		ast_fields << ast.Field{
			name: field_name
			ti: ti
		}
		fields << types.Field{
			name: field_name
			type_idx: ti.type_idx
		}
	}
	p.check(.rcbr)
	p.table.register_struct(types.Struct{
		name: name
		fields: fields
	})
	return ast.StructDecl{
		name: name
		is_pub: is_pub
		fields: ast_fields
	}
}

fn (p mut Parser) return_stmt() ast.Return {
	p.next()
	expr,t := p.expr(0)
	if !types.check(p.return_ti, t) {
		p.error('cannot use `$t.type_name` as type `$p.return_ti.type_name` in return argument')
	}
	return ast.Return{
		expr: expr
	}
}

fn (p mut Parser) var_decl() ast.VarDecl {
	is_mut := p.tok.kind == .key_mut // || p.prev_tok == .key_for
	// is_static := p.tok.kind == .key_static
	if p.tok.kind == .key_mut {
		p.check(.key_mut)
		// p.fspace()
	}
	if p.tok.kind == .key_static {
		p.check(.key_static)
		// p.fspace()
	}
	name := p.tok.lit
	p.read_first_token()
	expr,t := p.expr(token.lowest_prec)
	if _ := p.table.find_var(name) {
		p.error('redefinition of `$name`')
	}
	p.table.register_var(table.Var{
		name: name
		ti: t
		is_mut: is_mut
	})
	// println(p.table.names)
	// println('added $name')
	return ast.VarDecl{
		name: name
		expr: expr // p.expr(token.lowest_prec)
		
		ti: t
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}
