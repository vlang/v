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
	scanner   &scanner.Scanner
	file_name string
mut:
	tok       token.Token
	peek_tok  token.Token
	// vars []string
	table     &table.Table
	return_ti types.TypeIdent
	is_c      bool
	//
	// prefix_parse_fns []PrefixParseFn
	inside_if bool
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

pub fn (p &Parser) init_parse_fns() {
	// p.prefix_parse_fns = make(100, 100, sizeof(PrefixParseFn))
	// p.prefix_parse_fns[token.Kind.name] = parse_name
	println('')
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
			expr,ti := p.expr(0)
			return ast.ExprStmt{
				expr: expr
				ti: ti
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
	print_backtrace()
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

pub fn (p mut Parser) name_expr() (ast.Expr,types.TypeIdent) {
	mut node := ast.Expr{}
	mut ti := types.void_ti
	if p.tok.lit == 'C' {
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
	else if p.peek_tok.kind == .lcbr && !p.inside_if {
		ti = p.parse_ti()
		// println('sturct init ti=$ti.name')
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
		// p.warn('name ')
		// left := p.parse_ident()
		node = ast.Ident{
			name: p.tok.lit
		}
		var := p.table.find_var(p.tok.lit) or {
			p.error('name expr unknown variable `$p.tok.lit`')
			exit(0)
		}
		ti = var.ti
		p.next()
	}
	return node,ti
}

pub fn (p mut Parser) expr(precedence int) (ast.Expr,types.TypeIdent) {
	mut ti := types.void_ti
	mut node := ast.Expr{}
	// Prefix
	match p.tok.kind {
		.name {
			node,ti = p.name_expr()
		}
		.str {
			node,ti = p.string_expr()
		}
		// -1, -a etc
		.minus {
			node,ti = p.prefix_expr()
		}
		.amp {
			p.next()
		}
		.key_true, .key_false {
			node = ast.BoolLiteral{
				val: p.tok.kind == .key_true
			}
			ti = types.bool_ti
			p.next()
		}
		.number {
			node,ti = p.parse_number_literal()
		}
		.lpar {
			p.check(.lpar)
			node,ti = p.expr(0)
			p.check(.rpar)
		}
		.key_if {
			node,ti = p.if_expr()
		}
		.lsbr {
			node,ti = p.array_init()
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
			node,ti = p.dot_expr(node)
		}
		else if p.tok.kind.is_infix() {
			node,ti = p.infix_expr(node)
		}
		else if p.tok.kind in [.inc, .dec] {
			node = ast.PostfixExpr{
				op: p.tok.kind
				expr: node
			}
			p.next()
			return node,ti
		}
		else {
			return node,ti
		}
	}
	return node,ti
}

fn (p mut Parser) prefix_expr() (ast.Expr,types.TypeIdent) {
	op := p.tok.kind
	p.next()
	right,ti := p.expr(1)
	mut expr := ast.Expr{}
	expr = ast.PrefixExpr{
		op: op
		right: right
	}
	return expr,ti
}

fn (p mut Parser) dot_expr(left ast.Expr) (ast.Expr,types.TypeIdent) {
	p.next()
	field_name := p.check_name()
	// Method call
	if p.tok.kind == .lpar {
		p.next()
		args := p.call_args()
		println('method call $field_name')
		mut node := ast.Expr{}
		node = ast.MethodCallExpr{
			expr: left
			name: field_name
			args: args
		}
		return node,types.int_ti
	}
	/*
				// p.next()
				field := p.check_name()
				if !ti.type_kind in  [._placeholder, ._struct] {
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
				*/

	mut node := ast.Expr{}
	node = ast.SelectorExpr{
		expr: left
		field: field_name
	}
	return node,types.int_ti
}

fn (p mut Parser) infix_expr(left ast.Expr) (ast.Expr,types.TypeIdent) {
	op := p.tok.kind
	// mut typ := p.
	// println('infix op=$op.str()')
	precedence := p.tok.precedence()
	p.next()
	right,mut ti := p.expr(precedence)
	if op.is_relational() {
		ti = types.bool_ti
	}
	mut expr := ast.Expr{}
	expr = ast.BinaryExpr{
		op: op
		left: left
		right: right
	}
	return expr,ti
}

// Implementation of Pratt Precedence
[inline]
fn (p &Parser) is_addative() bool {
	return p.tok.kind in [.plus, .minus] && p.peek_tok.kind in [.number, .name]
}

fn (p mut Parser) for_statement() ast.Stmt {
	p.check(.key_for)
	// Infinite loop
	if p.tok.kind == .lcbr {
		stmts := p.parse_block()
		return ast.ForStmt{
			stmts: stmts
		}
	}
	else if p.tok.kind == .key_mut {
		p.error('`mut` is not required in for loops')
	}
	// for i := 0; i < 10; i++ {
	else if p.peek_tok.kind in [.decl_assign, .assign, .semicolon] {
		mut init := ast.Stmt{}
		mut cond := ast.Expr{}
		mut inc := ast.Stmt{}
		if p.peek_tok.kind == .decl_assign {
			init = p.var_decl()
		}
		else if p.tok.kind != .semicolon {
			// allow `for ;; i++ {`
			// Allow `for i = 0; i < ...`
			/*
			cond, typ = p.expr(0)
			if typ.kind != _bool {
				p.error('non-bool used as for condition')
			}
			*/
			println(1)
		}
		p.check(.semicolon)
		if p.tok.kind != .semicolon {
			mut typ := types.TypeIdent{}
			cond,typ = p.expr(0)
			if typ.kind != ._bool {
				p.error('non-bool used as for condition')
			}
		}
		p.check(.semicolon)
		if p.tok.kind != .lcbr {
			inc = p.stmt()
		}
		stmts := p.parse_block()
		return ast.ForCStmt{
			stmts: stmts
			init: init
			cond: cond
			inc: inc
		}
	}
	// `for i in start .. end`
	else if p.peek_tok.kind == .key_in {
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
		}
	}
	// `for cond {`
	cond,ti := p.expr(0)
	if !types.check(types.bool_ti, ti) {
		p.error('non-bool used as for condition')
	}
	stmts := p.parse_block()
	return ast.ForStmt{
		cond: cond
		stmts: stmts
	}
}

fn (p mut Parser) if_expr() (ast.Expr,types.TypeIdent) {
	p.inside_if = true
	// defer {
	// }
	mut node := ast.Expr{}
	p.check(.key_if)
	cond,cond_ti := p.expr(0)
	// if !types.check(types.bool_ti, cond_ti) {
	if cond_ti.kind != ._bool {
		p.error('non-bool used as if condition')
	}
	stmts := p.parse_block()
	mut else_stmts := []ast.Stmt
	if p.tok.kind == .key_else {
		p.check(.key_else)
		else_stmts = p.parse_block()
	}
	mut ti := types.void_ti
	// mut left := ast.Expr{}
	// If the last statement is an expression, return its type
	match stmts[stmts.len - 1] {
		ast.ExprStmt {
			p.warn('if expr ret $it.ti.name')
			ti = it.ti
			// return node,it.ti
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
	p.inside_if = false
	return node,ti
}

fn (p mut Parser) string_expr() (ast.Expr,types.TypeIdent) {
	mut node := ast.Expr{}
	node = ast.StringLiteral{
		val: p.tok.lit
	}
	if p.peek_tok.kind != .str_dollar {
		p.next()
		return node,types.string_ti
	}
	// Handle $ interpolation
	for p.tok.kind == .str {
		p.next()
		if p.tok.kind != .str_dollar {
			continue
		}
		p.check(.str_dollar)
		p.expr(0)
	}
	return node,types.string_ti
}

fn (p mut Parser) array_init() (ast.Expr,types.TypeIdent) {
	p.check(.lsbr)
	mut val_ti := types.void_ti
	mut exprs := []ast.Expr
	mut i := 0
	for p.tok.kind != .rsbr {
		expr,ti := p.expr(0)
		// The first element's type
		if i == 0 {
			val_ti = ti
		}
		else if !types.check(val_ti, ti) {
			p.error('expected array element with type `$val_ti.name`')
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
			type_idx: ti.idx
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
		p.warn('cannot use `$t.name` as type `$p.return_ti.name` in return argument')
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
	expr,ti := p.expr(token.lowest_prec)
	if _ := p.table.find_var(name) {
		p.error('redefinition of `$name`')
	}
	p.table.register_var(table.Var{
		name: name
		ti: ti
		is_mut: is_mut
	})
	// println(p.table.names)
	// println('added var `$name` with type $t.name')
	return ast.VarDecl{
		name: name
		expr: expr // p.expr(token.lowest_prec)
		
		ti: ti
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}
