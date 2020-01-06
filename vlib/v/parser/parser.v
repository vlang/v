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

type PrefixParseFn fn()ast.Expr

type InfixParseFn fn(e ast.Expr)ast.Expr

type PostfixParseFn fn()ast.Expr

struct Parser {
	scanner          &scanner.Scanner
	file_name        string
mut:
	tok              token.Token
	peek_tok         token.Token
	// vars []string
	table            &table.Table
	return_type      types.Type
	is_c             bool
	//
	prefix_parse_fns []PrefixParseFn
}

pub fn parse_stmt(text string, table &table.Table) ast.Stmt {
	s := scanner.new_scanner(text)
	mut p := Parser{
		scanner: s
		table: table
	}
	p.init_parse_fns()
	p.read_first_token()
	return p.stmt()
}

pub fn parse_file(path string, table &table.Table) ast.File {
	println('parse_file("$path")')
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
			println('EOF, breaking')
			break
		}
		// println('stmt at ' + p.tok.str())
		stmts << p.top_stmt()
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

pub fn (p mut Parser) init_parse_fns() {
	p.prefix_parse_fns = make(100, 100, sizeof(PrefixParseFn))
	// p.prefix_parse_fns[token.Kind.name] = parse_name
}
// former get_type()
pub fn (p mut Parser) parse_type() types.Type {
	typ := p.table.find_type(p.tok.lit) or {
		// typ := p.table.types[p.tok.lit]
		// if isnil(typ.name.str) || typ.name == '' {
		p.error('undefined type `$p.tok.lit`')
		exit(0)
	}
	p.next()
	return typ
}

pub fn (p mut Parser) read_first_token() {
	// need to call next() twice to get peek token and current token
	p.next()
	p.next()
}

pub fn (p mut Parser) parse_block() []ast.Stmt {
	p.check(.lcbr)
	mut stmts := []ast.Stmt
	if p.tok.kind != .rcbr {
		for {
			stmts << p.stmt()
			// p.warn('after stmt(): tok=$p.tok.str()')
			if p.tok.kind in [.eof, .rcbr] {
				break
			}
		}
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

pub fn (p mut Parser) top_stmt() ast.Stmt {
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
		else {
			p.error('bad top level statement')
			return ast.Module{} // silence C warning
			// exit(0)
		}
	}
}

pub fn (p mut Parser) stmt() ast.Stmt {
	match p.tok.kind {
		.key_mut {
			return p.var_decl()
		}
		.key_for {
			return p.for_statement()
		}
		.key_return {
			return p.return_stmt()
		}
		else {
			// `x := ...`
			if p.tok.kind == .name && p.peek_tok.kind == .decl_assign {
				return p.var_decl()
			}
			expr,typ := p.expr(0)
			return ast.ExprStmt{
				expr: expr
				typ: typ
			}
		}
	}
}

pub fn (p mut Parser) assign_expr(left ast.Expr) ast.AssignExpr {
	op := p.tok.kind
	p.next()
	val,_ := p.expr(0)
	node := ast.AssignExpr{
		left: left
		op: op
		val: val
	}
	return node
}

/*
pub fn (p mut Parser) assign_stmt() ast.AssignStmt {
	name := p.tok.lit
	// println('looking for $name')
	var := p.table.find_var(name) or {
		p.error('assign unknown variable `$name`')
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
*/


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

pub fn (p mut Parser) name_expr() (ast.Expr,types.Type) {
	mut node := ast.Expr{}
	mut typ := types.void_type
	// fn call
	if p.peek_tok.kind == .lpar {
		x,typ2 := p.call_expr() // TODO `node,typ :=` should work
		node = x
		typ = typ2
	}
	// struct init
	else if p.peek_tok.kind == .lcbr {
		typ = p.parse_type()
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
			typ: typ
			exprs: exprs
			fields: field_names
		}
		p.check(.rcbr)
	}
	else {
		// p.warn('name ')
		// left := p.parse_ident()
		node = ast.Ident{
			name: p.tok.lit
		}
		var := p.table.find_var(p.tok.lit) or {
			p.error('name expr unknown variable `$p.tok.lit`')
			exit(0)
		}
		typ = var.typ
		p.next()
	}
	return node,typ
}

pub fn (p mut Parser) expr(precedence int) (ast.Expr,types.Type) {
	mut typ := types.void_type
	mut node := ast.Expr{}
	// Prefix
	match p.tok.kind {
		.name {
			node,typ = p.name_expr()
		}
		.str {
			node,typ = p.parse_string_literal()
		}
		// -1, -a etc
		.minus {
			node,typ = p.prefix_expr()
		}
		.key_true, .key_false {
			node = ast.BoolLiteral{
				val: p.tok.kind == .key_true
			}
			typ = types.bool_type
			p.next()
		}
		.number {
			node,typ = p.parse_number_literal()
		}
		.lpar {
			p.check(.lpar)
			p.next()
			node,typ = p.expr(0)
			p.check(.rpar)
		}
		.key_if {
			node,typ = p.if_expr()
		}
		.lsbr {
			node,typ = p.array_init()
		}
		else {
			p.error('expr(): bad token `$p.tok.str()`')
		}
	}
	// Infix
	for precedence < p.tok.precedence() {
		if p.tok.kind.is_assign() {
			node = p.assign_expr(node)
		}
		else if p.tok.kind == .dot {
			node,typ = p.dot_expr(node)
		}
		else if p.tok.kind.is_infix() {
			node,typ = p.infix_expr(node)
		}
		else if p.tok.kind in [.inc, .dec] {
			node = ast.PostfixExpr{
				op: p.tok.kind
				expr: node
			}
			p.next()
			return node,typ
		}
		else {
			return node,typ
		}
	}
	return node,typ
}

fn (p mut Parser) prefix_expr() (ast.Expr,types.Type) {
	op := p.tok.kind
	p.next()
	right,typ := p.expr(1)
	mut expr := ast.Expr{}
	expr = ast.PrefixExpr{
		op: op
		right: right
	}
	return expr,typ
}

fn (p mut Parser) dot_expr(left ast.Expr) (ast.Expr,types.Type) {
	p.next()
	field_name := p.check_name()
	/*
				// p.next()
				field := p.check_name()
				mut ok := false
				for f in typ.fields {
					if f.name == field {
						ok = true
					}
				}
				if !ok {
					p.error('unknown field `${typ.name}.$field`')
				}
				*/

	mut node := ast.Expr{}
	node = ast.SelectorExpr{
		expr: left
		field: field_name
	}
	return node,types.int_type
}

fn (p mut Parser) infix_expr(left ast.Expr) (ast.Expr,types.Type) {
	op := p.tok.kind
	// mut typ := p.
	// println('infix op=$op.str()')
	precedence := p.tok.precedence()
	p.next()
	right,mut typ := p.expr(precedence)
	if op.is_relational() {
		typ = types.bool_type
	}
	mut expr := ast.Expr{}
	expr = ast.BinaryExpr{
		op: op
		left: left
		right: right
	}
	return expr,typ
}

// Implementation of Pratt Precedence
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
	cond,typ := p.expr(0)
	if !types.check(types.bool_type, typ) {
		p.error('non-bool used as for condition')
	}
	stmts := p.parse_block()
	return ast.ForStmt{
		cond: cond
		stmts: stmts
	}
}

fn (p mut Parser) if_expr() (ast.Expr,types.Type) {
	mut node := ast.Expr{}
	p.check(.key_if)
	cond,cond_type := p.expr(0)
	if !types.check(types.bool_type, cond_type) {
		p.error('non-bool used as if condition')
	}
	stmts := p.parse_block()
	mut else_stmts := []ast.Stmt
	if p.tok.kind == .key_else {
		p.check(.key_else)
		else_stmts = p.parse_block()
	}
	mut typ := types.void_type
	// mut left := ast.Expr{}
	// If the last statement is an expression, return its type
	match stmts[stmts.len - 1] {
		ast.ExprStmt {
			p.warn('if expr ret $it.typ.name')
			typ = it.typ
			// return node,it.typ
			// left =
		}
		else {}
	}
	node = ast.IfExpr{
		cond: cond
		stmts: stmts
		else_stmts: else_stmts
		typ: typ
		// left: left
		
	}
	return node,typ
}

fn (p mut Parser) parse_string_literal() (ast.Expr,types.Type) {
	mut node := ast.Expr{}
	node = ast.StringLiteral{
		val: p.tok.lit
	}
	p.next()
	return node,types.string_type
}

fn (p mut Parser) array_init() (ast.Expr,types.Type) {
	p.check(.lsbr)
	mut val_type := types.void_type
	mut exprs := []ast.Expr
	mut i := 0
	for p.tok.kind != .rsbr {
		expr,typ := p.expr(0)
		// The first element's type
		if i == 0 {
			val_type = typ
		}
		else if !types.check(val_type, typ) {
			p.error('expected array element with type `$val_type.name`')
		}
		exprs << expr
		i++
		if p.tok.kind == .comma {
			p.check(.comma)
		}
	}
	mut node := ast.Expr{}
	node = ast.ArrayInit{
		typ: val_type
		exprs: exprs
	}
	p.check(.rsbr)
	return node,val_type
}

fn (p mut Parser) parse_number_literal() (ast.Expr,types.Type) {
	lit := p.tok.lit
	mut node := ast.Expr{}
	mut typ := types.int_type
	if lit.contains('.') {
		node = ast.FloatLiteral{
			// val: lit.f64()
			val: lit
		}
		typ = types.f64_type
	}
	else {
		node = ast.IntegerLiteral{
			val: lit.int()
		}
		typ = types.int_type
	}
	p.next()
	return node,typ
}

fn (p mut Parser) module_decl() ast.Module {
	p.check(.key_module)
	p.next()
	return ast.Module{}
}

fn (p mut Parser) import_stmt() ast.Import {
	p.check(.key_import)
	name := p.check_name()
	return ast.Import{
		mods: [name]
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
		typ := p.parse_type()
		ast_fields << ast.Field{
			name: field_name
			typ: typ
		}
		fields << types.Field{
			name: field_name
			type_idx: typ.idx
		}
	}
	p.check(.rcbr)
	p.table.register_type(types.Type{
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
	if !types.check(p.return_type, t) {
		p.error('cannot use `$t.name` as type `$p.return_type.name` in return argument')
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
		typ: t
		is_mut: is_mut
	})
	// println(p.table.names)
	// println('added var `$name` with type $t.name')
	return ast.VarDecl{
		name: name
		expr: expr // p.expr(token.lowest_prec)
		
		typ: t
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}
