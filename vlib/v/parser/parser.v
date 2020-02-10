// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	v.scanner
	v.ast
	v.token
	v.table
	v.pref
	term
	os
)

const (
	colored_output = term.can_show_color_on_stderr()
)
/*
type PrefixParseFn fn()ast.Expr

type InfixParseFn fn(e ast.Expr)ast.Expr

type PostfixParseFn fn()ast.Expr
*/


struct Parser {
	scanner           &scanner.Scanner
	file_name         string
mut:
	tok               token.Token
	peek_tok          token.Token
	// vars []string
	table             &table.Table
	return_type       table.Type // current function's return type
	// scope_level int
	// var_idx     int
	is_c              bool
	//
	// prefix_parse_fns []PrefixParseFn
	inside_if         bool
	pref              &pref.Preferences // Preferences shared from V struct
	builtin_mod       bool
	mod               string
	unresolved        []ast.Expr
	unresolved_offset int
	expected_type     table.Type
}

// for tests
pub fn parse_stmt(text string, table &table.Table) ast.Stmt {
	s := scanner.new_scanner(text)
	mut p := Parser{
		scanner: s
		table: table
		pref: &pref.Preferences{}
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
		pref: &pref.Preferences{}
		unresolved_offset: table.unresolved_idxs.size
	}
	p.read_first_token()
	// module decl
	module_decl := if p.tok.kind == .key_module { p.module_decl() } else { ast.Module{name: 'main'
	} }
	p.mod = module_decl.name
	p.builtin_mod = p.mod == 'builtin'
	// imports
	mut imports := []ast.Import
	for p.tok.kind == .key_import {
		imports << p.import_stmt()
	}
	// TODO: import only mode
	for {
		// res := s.scan()
		if p.tok.kind == .eof {
			println('EOF, breaking')
			break
		}
		// println('stmt at ' + p.tok.str())
		stmts << p.top_stmt()
	}
	// println('nr stmts = $stmts.len')
	// println(stmts[0])
	return ast.File{
		path: path
		mod: module_decl
		imports: imports
		stmts: stmts
		unresolved: p.unresolved
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
	p.table.open_scope()
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
	p.table.close_scope()
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
		.key_pub {
			match p.peek_tok.kind {
				.key_const {
					return p.const_decl()
				}
				.key_fn {
					return p.fn_decl()
				}
				.key_struct, .key_union, .key_interface {
					return p.struct_decl()
				}
				.key_enum {
					return p.enum_decl()
				}
				.key_type {
					return p.type_decl()
				}
				else {
					p.error('wrong pub keyword usage')
					return ast.Stmt{}
				}
	}
		}
		.lsbr {
			return p.attr()
		}
		.key_global {
			return p.global_decl()
		}
		.key_const {
			return p.const_decl()
		}
		.key_fn {
			return p.fn_decl()
		}
		.key_struct {
			return p.struct_decl()
		}
		.dollar {
			return p.comp_if()
		}
		.hash {
			return p.hash()
		}
		else {
			p.error('parser: bad top level statement')
			return ast.Stmt{}
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
		.dollar {
			return p.comp_if()
		}
		.key_continue, .key_break {
			tok := p.tok
			p.next()
			return ast.BranchStmt{
				tok: p.tok
			}
		}
		.key_unsafe {
			p.next()
			p.parse_block()
			return ast.Stmt{}
		}
		else {
			// `x := ...`
			// if p.tok.kind == .name && p.peek_tok.kind in [.decl_assign, .comma] {
			if p.tok.kind == .name && p.peek_tok.kind in [.decl_assign] {
				return p.var_decl()
			}
			if p.tok.kind == .name && p.peek_tok.kind in [.comma] {
				return p.assign_stmt()
			}
			expr,typ := p.expr(0)
			return ast.ExprStmt{
				expr: expr
				typ: typ
			}
		}
	}
}

pub fn (p mut Parser) comp_if() ast.CompIf {
	p.next()
	p.check(.key_if)
	if p.tok.kind == .not {
		p.next()
	}
	p.check_name()
	p.parse_block()
	if p.tok.kind == .dollar && p.peek_tok.kind == .key_else {
		p.next()
		p.check(.key_else)
		p.parse_block()
	}
	return ast.CompIf{}
}

pub fn (p mut Parser) assign_expr(left ast.Expr) ast.AssignExpr {
	op := p.tok.kind
	p.next()
	val,_ := p.expr(0)
	node := ast.AssignExpr{
		left: left
		val: val
		op: op
		pos: p.tok.position()
	}
	return node
}

fn (p mut Parser) attr() ast.Attr {
	p.check(.lsbr)
	name := p.check_name()
	p.check(.rsbr)
	return ast.Attr{
		name: name
	}
}

/*

fn (p mut Parser) range_expr(low ast.Expr) ast.Expr {
	// ,table.Type) {
	if p.tok.kind != .dotdot {
		p.next()
	}
	p.check(.dotdot)
	mut high := ast.Expr{}
	if p.tok.kind != .rsbr {
		high,_ = p.expr(0)
		// if typ.typ.kind != .int {
		// p.error('non-integer index `$typ.typ.name`')
		// }
	}
	node := ast.RangeExpr{
		low: low
		high: high
	}
	return node
}
*/

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
	if !p.table.check(left_type, right_type) {
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
	mut path := p.file_name
	// Get relative path
	workdir := os.getwd() + os.path_separator
	if path.starts_with(workdir) {
		path = path.replace(workdir, '')
	}
	final_msg_line := 'xxx$path:$p.tok.line_nr: error: $s'
	if colored_output {
		eprintln(term.bold(term.red(final_msg_line)))
	}
	else {
		eprintln(final_msg_line)
	}
	exit(1)
}

pub fn (p &Parser) error_at_line(s string, line_nr int) {
	final_msg_line := '$p.file_name:$line_nr: error: $s'
	if colored_output {
		eprintln(term.bold(term.red(final_msg_line)))
	}
	else {
		eprintln(final_msg_line)
	}
	exit(1)
}

pub fn (p &Parser) warn(s string) {
	final_msg_line := '$p.file_name:$p.tok.line_nr: warning: $s'
	if colored_output {
		eprintln(term.bold(term.blue(final_msg_line)))
	}
	else {
		eprintln(final_msg_line)
	}
}

pub fn (p mut Parser) parse_ident(is_c bool) (ast.Ident,table.Type) {
	mut node := ast.Ident{}
	mut typ := table.void_type
	// p.warn('name ')
	// left := p.parse_ident()
	name := p.check_name()
	mut ident := ast.Ident{
		name: name
		is_c: is_c
	}
	mut known_var := false
	if var := p.table.find_var(name) {
		known_var = true
		typ = var.typ
	}
	// variable
	if known_var || p.tok.kind in [.comma, .decl_assign, .assign] {
		// println('#### IDENT: $var.name: $var.typ.typ.name - $var.typ.idx')
		ident.kind = .variable
		ident.info = ast.IdentVar{
			typ: typ
			// name: ident.name
			// expr: p.expr(0)// var.expr
			
		}
		return ident,typ
	}
	else {
		if is_c {
			typ = table.int_type
			ident.info = ast.IdentVar{
				typ: typ
				// name: ident.name
				
			}
			return ident,typ
		}
		// const
		if c := p.table.find_const(name) {
			typ = c.typ
			ident.kind = .constant
			ident.info = ast.IdentVar{
				typ: typ
				// name: ident.name
				
			}
			node = ident
		}else{
			// Function object (not a call), e.g. `onclick(my_click)`
			p.table.find_fn(name) or {
				// ident.info = ast.IdentVar
				node = ast.Ident{
					kind: .blank_ident
					name: name
				}
				return node,typ
				// p.error('parse_ident: unknown identifier `$name`')
			}
			// p.next()
		}
	}
	return node,typ
}

pub fn (p mut Parser) name_expr() (ast.Expr,table.Type) {
	mut node := ast.Expr{}
	mut typ := table.void_type
	// mut typ := table.unresolved_type
	is_c := p.tok.lit == 'C' && p.peek_tok.kind == .dot
	if is_c {
		p.next()
		p.check(.dot)
	}
	else if p.tok.lit in ['strings', 'strconv'] {
		p.next()
		p.check(.dot)
	}
	if p.tok.lit == 'map' && p.peek_tok.kind == .lsbr {
		map_type := p.parse_map_type(0)
		return node,typ
	}
	// fn call or type cast
	if p.peek_tok.kind == .lpar {
		name := p.tok.lit
		// type cast. TODO: finish
		// if name in table.builtin_type_names {
		if name in p.table.type_idxs {
			to_typ := p.parse_type()
			p.check(.lpar)
			mut expr := ast.Expr{}
			expr,_ = p.expr(0)
			// TODO, string(b, len)
			if table.type_idx(to_typ) == table.string_type_idx && p.tok.kind == .comma {
				p.check(.comma)
				p.expr(0) // len
			}
			p.check(.rpar)
			node = ast.CastExpr{
				typ: to_typ
				expr: expr
			}
			return node,to_typ
		}
		// fn call
		else {
			println('calling $p.tok.lit')
			x,ti2 := p.call_expr() // TODO `node,typ :=` should work
			node = x
			typ = ti2
		}
	}
	// struct init
	else if p.peek_tok.kind == .lcbr && (p.tok.lit[0].is_capital() || is_c || p.tok.lit in ['array', 'string', 'ustring', 'mapnode', 'map']) && !p.tok.lit[p.tok.lit.len - 1].is_capital() {
		// || p.table.known_type(p.tok.lit)) {
		typ = p.parse_type()
		// p.warn('struct init typ=$typ.name')
		p.check(.lcbr)
		mut field_names := []string
		mut exprs := []ast.Expr
		for p.tok.kind != .rcbr {
			field_name := p.check_name()
			field_names << field_name
			p.check(.colon)
			expr,_ := p.expr(0)
			exprs << expr
		}
		node = ast.StructInit{
			typ: typ
			exprs: exprs
			fields: field_names
			pos: p.tok.position()
		}
		p.check(.rcbr)
	}
	else {
		mut ident := ast.Ident{}
		ident,typ = p.parse_ident(is_c)
		node = ident
	}
	return node,typ
}

pub fn (p mut Parser) expr(precedence int) (ast.Expr,table.Type) {
	// println('\n\nparser.expr()')
	mut typ := table.void_type
	mut node := ast.Expr{}
	// Prefix
	match p.tok.kind {
		.name {
			node,typ = p.name_expr()
		}
		.str {
			node,typ = p.string_expr()
		}
		.dot {
			// .enum_val
			node,typ = p.enum_val()
		}
		.chartoken {
			typ = table.byte_type
			node = ast.CharLiteral{
				val: p.tok.lit
			}
			p.next()
		}
		// -1, -a, !x, &x, ~x
		.minus, .amp, .mul, .not, .bit_not {
			node,typ = p.prefix_expr()
		}
		// .amp {
		// p.next()
		// }
		.key_true, .key_false {
			node = ast.BoolLiteral{
				val: p.tok.kind == .key_true
			}
			typ = table.bool_type
			p.next()
		}
		.key_match {
			node,typ = p.match_expr()
		}
		.number {
			node,typ = p.parse_number_literal()
		}
		.lpar {
			p.check(.lpar)
			node,typ = p.expr(0)
			p.check(.rpar)
		}
		.key_if {
			node,typ = p.if_expr()
		}
		.lsbr {
			node,typ = p.array_init()
		}
		.key_none {
			p.next()
		}
		.key_sizeof {
			p.next()
			p.check(.lpar)
			p.next()
			p.check(.rpar)
			typ = table.int_type
		}
		// Map or `{ x | foo:bar, a:10 }`
		.lcbr {
			p.next()
			p.check_name()
			p.check(.pipe)
			for {
				p.check_name()
				p.check(.colon)
				p.expr(0)
				if p.tok.kind == .comma {
					p.check(.comma)
				}
				if p.tok.kind == .rcbr {
					break
				}
			}
			p.check(.rcbr)
		}
		else {
			p.error('pexpr(): bad token `$p.tok.str()`')
		}
	}
	// Infix
	for precedence < p.tok.precedence() {
		if p.tok.kind.is_assign() {
			node = p.assign_expr(node)
		}
		else if p.tok.kind == .dot {
			node,typ = p.dot_expr(node, typ)
		}
		else if p.tok.kind == .lsbr {
			node = p.index_expr(node) // , typ)
		}
		else if p.tok.kind == .key_as {
			p.next()
			typ = p.parse_type()
		}
		else if p.tok.kind.is_infix() {
			node,typ = p.infix_expr(node)
		}
		// Postfix
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

fn (p mut Parser) prefix_expr() (ast.Expr,table.Type) {
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

fn (p mut Parser) index_expr(left ast.Expr) ast.IndexExpr {
	// left == `a` in `a[0]`
	p.next() // [
	if p.tok.kind == .dotdot {
		// [..end]
		p.next()
		high,_ := p.expr(0)
		p.check(.rsbr)
		return ast.IndexExpr{
			left: left
			pos: p.tok.position()
			index: ast.RangeExpr{
				low: ast.Expr{}
				high: high
			}
		}
	}
	expr,_ := p.expr(0) // `[expr]` or  `[expr..]`
	if p.tok.kind == .dotdot {
		// [start..end] or [start..]
		p.check(.dotdot)
		mut high := ast.Expr{}
		if p.tok.kind != .rsbr {
			high,_ = p.expr(0)
		}
		p.check(.rsbr)
		return ast.IndexExpr{
			left: left
			pos: p.tok.position()
			index: ast.RangeExpr{
				low: expr
				high: high
			}
		}
	}
	// [expr]
	p.check(.rsbr)
	return ast.IndexExpr{
		left: left
		index: expr
		pos: p.tok.position()
	}
}

fn (p mut Parser) filter(typ table.Type) {
	p.table.register_var(table.Var{
		name: 'it'
		typ: typ
	})
}

fn (p mut Parser) dot_expr(left ast.Expr, left_type table.Type) (ast.Expr,table.Type) {
	p.next()
	field_name := p.check_name()
	if field_name == 'filter' {
		p.table.open_scope()
		p.filter(left_type)
	}
	// Method call
	if p.tok.kind == .lpar {
		p.next()
		args := p.call_args()
		if p.tok.kind == .key_orelse {
			p.next()
			p.parse_block()
		}
		mcall_expr := ast.MethodCallExpr{
			expr: left
			name: field_name
			args: args
			pos: p.tok.position()
		}
		mut node := ast.Expr{}
		node = mcall_expr
		// typ := p.add_unresolved('${left_type.typ.name}.${field_name}()', mcall_expr)
		typ := p.add_unresolved('${table.type_idx(left_type)}.${field_name}()', mcall_expr)
		return node,typ
	}
	sel_expr := ast.SelectorExpr{
		expr: left
		field: field_name
		pos: p.tok.position()
	}
	// typ := p.add_unresolved('${left_type.typ.name}.$field_name', sel_expr)
	typ := p.add_unresolved('${table.type_idx(left_type)}.$field_name', sel_expr)
	mut node := ast.Expr{}
	node = sel_expr
	if field_name == 'filter' {
		p.table.close_scope()
	}
	return node,typ
}

fn (p mut Parser) infix_expr(left ast.Expr) (ast.Expr,table.Type) {
	op := p.tok.kind
	// mut typ := p.
	// println('infix op=$op.str()')
	precedence := p.tok.precedence()
	p.next()
	right,mut typ := p.expr(precedence)
	if op.is_relational() {
		typ = table.bool_type
	}
	mut expr := ast.Expr{}
	expr = ast.InfixExpr{
		left: left
		right: right
		right_type: typ
		op: op
		pos: p.tok.position()
	}
	return expr,typ
}

// Implementation of Pratt Precedence
/*
[inline]
fn (p &Parser) is_addative() bool {
	return p.tok.kind in [.plus, .minus] && p.peek_tok.kind in [.number, .name]
}
*/
// `.green`
fn (p mut Parser) enum_val() (ast.Expr,table.Type) {
	p.check(.dot)
	name := p.check_name()
	mut node := ast.Expr{}
	node = ast.EnumVal{
		name: name
	}
	return node,table.bool_type
}

fn (p mut Parser) for_statement() ast.Stmt {
	p.check(.key_for)
	p.table.open_scope()
	// defer { p.table.close_scope() }
	// Infinite loop
	if p.tok.kind == .lcbr {
		stmts := p.parse_block()
		p.table.close_scope()
		return ast.ForStmt{
			stmts: stmts
			pos: p.tok.position()
		}
	}
	else if p.tok.kind == .key_mut {
		p.error('`mut` is not required in for loops')
	}
	// for i := 0; i < 10; i++ {
	else if p.peek_tok.kind in [.decl_assign, .assign, .semicolon] || p.tok.kind == .semicolon {
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
			mut typ := table.void_type
			cond,typ = p.expr(0)
		}
		p.check(.semicolon)
		if p.tok.kind != .lcbr {
			inc = p.stmt()
		}
		stmts := p.parse_block()
		p.table.close_scope()
		return ast.ForCStmt{
			stmts: stmts
			init: init
			cond: cond
			inc: inc
		}
	}
	// `for i in vals`, `for i in start .. end`
	else if p.peek_tok.kind == .key_in || p.peek_tok.kind == .comma {
		var_name := p.check_name()
		if p.tok.kind == .comma {
			p.check(.comma)
			val_name := p.check_name()
			p.table.register_var(table.Var{
				name: val_name
				typ: table.int_type
			})
		}
		p.check(.key_in)
		mut elem_type := table.void_type
		// arr_expr
		_,arr_typ := p.expr(0)
		// array / map
		arr_typ_sym := p.table.get_type_symbol(arr_typ)
		match arr_typ_sym.info {
			table.Array {
				elem_type = it.elem_type
			}
			table.Map {
				elem_type = it.value_type
			}
			else {
				println(1)
				// elem_type_sym := p.table.get_type_symbol(elem_type)
				// p.error('cannot loop over type: $elem_type_sym.name')
			}
	}
		// 0 .. 10
		// start := p.tok.lit.int()
		if p.tok.kind == .dotdot {
			p.check(.dotdot)
			p.expr(0)
		}
		p.table.register_var(table.Var{
			name: var_name
			typ: elem_type
		})
		stmts := p.parse_block()
		// println('nr stmts=$stmts.len')
		p.table.close_scope()
		return ast.ForStmt{
			stmts: stmts
			pos: p.tok.position()
		}
	}
	// `for cond {`
	cond,_ := p.expr(0)
	stmts := p.parse_block()
	p.table.close_scope()
	return ast.ForStmt{
		cond: cond
		stmts: stmts
		pos: p.tok.position()
	}
}

fn (p mut Parser) if_expr() (ast.Expr,table.Type) {
	p.inside_if = true
	// defer {
	// }
	mut node := ast.Expr{}
	p.check(.key_if)
	cond,_ := p.expr(0)
	p.inside_if = false
	stmts := p.parse_block()
	mut else_stmts := []ast.Stmt
	if p.tok.kind == .key_else {
		p.check(.key_else)
		if p.tok.kind == .key_if {
			p.if_expr()
		}
		else {
			else_stmts = p.parse_block()
		}
	}
	mut typ := table.void_type
	// mut left := ast.Expr{}
	// If the last statement is an expression, return its type
	if stmts.len > 0 {
		match stmts[stmts.len - 1] {
			ast.ExprStmt {
				type_sym := p.table.get_type_symbol(it.typ)
				p.warn('if expr ret $type_sym.name')
				typ = it.typ
				// return node,it.ti
				// left =
			}
			else {}
	}
	}
	node = ast.IfExpr{
		cond: cond
		stmts: stmts
		else_stmts: else_stmts
		typ: typ
		pos: p.tok.position()
		// left: left
		
	}
	return node,typ
}

fn (p mut Parser) string_expr() (ast.Expr,table.Type) {
	mut node := ast.Expr{}
	node = ast.StringLiteral{
		val: p.tok.lit
	}
	if p.peek_tok.kind != .str_dollar {
		p.next()
		return node,table.string_type
	}
	// Handle $ interpolation
	for p.tok.kind == .str {
		p.next()
		if p.tok.kind != .str_dollar {
			continue
		}
		p.check(.str_dollar)
		p.expr(0)
		if p.tok.kind == .colon {
			p.next()
		}
	}
	return node,table.string_type
}

fn (p mut Parser) array_init() (ast.Expr,table.Type) {
	mut node := ast.Expr{}
	p.check(.lsbr)
	mut val_type := table.void_type
	mut exprs := []ast.Expr
	mut is_fixed := false
	mut fixed_size := 0
	if p.tok.kind == .rsbr {
		p.check(.rsbr)
		// []string
		if p.tok.kind == .name {
			val_type = p.parse_type()
		}
		// []
		else {
			// TODO ?
			println(0)
		}
	}
	else {
		// [1,2,3]
		for i := 0; p.tok.kind != .rsbr; i++ {
			expr,typ := p.expr(0)
			exprs << expr
			if i == 0 {
				val_type = typ
			}
			if p.tok.kind == .comma {
				p.check(.comma)
			}
		}
		line_nr := p.tok.line_nr
		p.check(.rsbr)
		// Fixed size array? (`[100]byte`)
		if exprs.len == 1 && p.tok.kind == .name && p.tok.line_nr == line_nr {
			is_fixed = true
			val_type = p.parse_type()
			match exprs[0] {
				ast.IntegerLiteral {
					fixed_size = it.val
				}
				else {}
	}
			p.warn('fixed size array')
		}
	}
	idx := if is_fixed { p.table.find_or_register_array_fixed(val_type, fixed_size, 1) } else { p.table.find_or_register_array(val_type, 1) }
	array_type := table.new_type(idx)
	node = ast.ArrayInit{
		typ: array_type
		exprs: exprs
		pos: p.tok.position()
	}
	return node,array_type
}

fn (p mut Parser) parse_number_literal() (ast.Expr,table.Type) {
	lit := p.tok.lit
	mut node := ast.Expr{}
	mut ti := table.int_type
	if lit.contains('.') {
		node = ast.FloatLiteral{
			// val: lit.f64()
			val: lit
		}
		ti = table.f64_type
	}
	else {
		node = ast.IntegerLiteral{
			val: lit.int()
		}
		// ti = table.int_ti
	}
	p.next()
	return node,ti
}

fn (p mut Parser) module_decl() ast.Module {
	p.check(.key_module)
	name := p.check_name()
	return ast.Module{
		name: name
	}
}

fn (p mut Parser) parse_import() ast.Import {
	mod_name := p.check_name()
	mut mod_alias := mod_name
	if p.tok.kind == .key_as {
		p.check(.key_as)
		mod_alias = p.check_name()
	}
	return ast.Import{
		mod: mod_name
		alias: mod_alias
		pos: p.tok.position()
	}
}

fn (p mut Parser) import_stmt() []ast.Import {
	p.check(.key_import)
	mut imports := []ast.Import
	if p.tok.kind == .lpar {
		p.check(.lpar)
		for p.tok.kind != .rpar {
			imports << p.parse_import()
		}
		p.check(.rpar)
	}
	else {
		imports << p.parse_import()
	}
	return imports
}

fn (p mut Parser) const_decl() ast.ConstDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_const)
	p.check(.lpar)
	mut fields := []ast.Field
	mut exprs := []ast.Expr
	for p.tok.kind != .rpar {
		name := p.check_name()
		println('const: $name')
		p.check(.assign)
		expr,typ := p.expr(0)
		fields << ast.Field{
			name: name
			typ: typ
		}
		exprs << expr
		p.table.register_const(table.Var{
			name: name
			typ: typ
		})
	}
	p.check(.rpar)
	return ast.ConstDecl{
		fields: fields
		exprs: exprs
	}
}

fn (p mut Parser) struct_decl() ast.StructDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_struct)
	is_c := p.tok.lit == 'C' && p.peek_tok.kind == .dot
	if is_c {
		p.next() // C
		p.next() // .
	}
	name := p.check_name()
	p.check(.lcbr)
	mut ast_fields := []ast.Field
	mut fields := []table.Field
	for p.tok.kind != .rcbr {
		if p.tok.kind == .key_pub {
			p.check(.key_pub)
			if p.tok.kind == .key_mut {
				p.check(.key_mut)
			}
			p.check(.colon)
		}
		else if p.tok.kind == .key_mut {
			p.check(.key_mut)
			p.check(.colon)
		}
		field_name := p.check_name()
		// p.warn('field $field_name')
		typ := p.parse_type()
		// Default value
		if p.tok.kind == .assign {
			p.next()
			p.expr(0)
		}
		ast_fields << ast.Field{
			name: field_name
			typ: typ
		}
		fields << table.Field{
			name: field_name
			typ: typ
		}
		// println('struct field $ti.name $field_name')
	}
	p.check(.rcbr)
	t := table.TypeSymbol{
		parent: 0
		kind: .struct_
		name: name
		info: table.Struct{
			fields: fields
		}
	}
	mut ret := 0
	if p.builtin_mod && t.name in table.builtin_type_names {
		// this allows overiding the builtins type
		// with the real struct type info parsed from builtin
		ret = p.table.register_builtin_type_symbol(t)
	}
	else {
		ret = p.table.register_type_symbol(t)
	}
	if ret == -1 {
		p.error('cannot register type `$name`, another type with this name exists')
	}
	return ast.StructDecl{
		name: name
		is_pub: is_pub
		fields: ast_fields
		pos: p.tok.position()
	}
}

fn (p mut Parser) return_stmt() ast.Return {
	p.next()
	// return expressions
	mut exprs := []ast.Expr
	// return type idents
	// mut got_tis := []table.Type
	if table.type_idx(p.return_type) == table.void_type_idx {
		return ast.Return{
			pos: p.tok.position()
		}
	}
	for {
		// expr,ti := p.expr(0)
		expr,_ := p.expr(0)
		exprs << expr
		// got_tis << ti
		if p.tok.kind == .comma {
			p.check(.comma)
		}
		else {
			break
		}
	}
	stmt := ast.Return{
		expected_type: p.return_type
		exprs: exprs
		pos: p.tok.position()
	}
	return stmt
}

pub fn (p mut Parser) assign_stmt() ast.AssignStmt {
	// TODO: multiple return & multiple assign
	mut idents := []ast.Ident
	for {
		ident,_ := p.parse_ident(false)
		idents << ident
		if p.tok.kind == .comma {
			p.check(.comma)
		}
		else {
			break
		}
	}
	p.next() // :=, =
	expr,_ := p.expr(0)
	return ast.AssignStmt{
		left: idents
		right: [expr]
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
	name := p.check_name()
	p.next()
	expr,typ := p.expr(0)
	if _ := p.table.find_var(name) {
		p.error('redefinition of `$name`')
	}
	p.table.register_var(table.Var{
		name: name
		is_mut: is_mut
		typ: typ
	})
	typ_sym := p.table.get_type_symbol(typ)
	p.warn('var decl name=$name typ=$typ_sym.name')
	// println(p.table.names)
	node := ast.VarDecl{
		name: name
		expr: expr // p.expr(token.lowest_prec)
		
		is_mut: is_mut
		typ: typ
		pos: p.tok.position()
	}
	return node
}

fn (p mut Parser) hash() ast.HashStmt {
	p.next()
	return ast.HashStmt{
		name: p.tok.lit
	}
}

fn (p mut Parser) global_decl() ast.GlobalDecl {
	if !p.pref.translated && !p.pref.is_live && !p.builtin_mod && !p.pref.building_v && p.mod != 'ui' && p.mod != 'gg2' && p.mod != 'uiold' && !os.getwd().contains('/volt') && !p.pref.enable_globals {
		p.error('use `v --enable-globals ...` to enable globals')
	}
	p.next()
	name := p.check_name()
	println(name)
	typ := p.parse_type()
	if p.tok.kind == .assign {
		p.next()
		p.expr(0)
	}
	p.table.register_global(name, typ)
	// p.genln(p.table.cgen_name_type_pair(name, typ))
	/*
		mut g := p.table.cgen_name_type_pair(name, typ)
		if p.tok == .assign {
			p.next()
			g += ' = '
			_,expr := p.tmp_expr()
			g += expr
		}
		// p.genln('; // global')
		g += '; // global'
		if !p.cgen.nogen {
			p.cgen.consts << g
		}
		*/

	return ast.GlobalDecl{
		name: name
	}
}

fn (p mut Parser) match_expr() (ast.Expr,table.Type) {
	p.check(.key_match)
	cond,typ := p.expr(0)
	p.check(.lcbr)
	mut blocks := []ast.StmtBlock
	mut match_exprs := []ast.Expr
	mut return_type := table.void_type
	for {
		// Sum type match
		if p.tok.kind == .name && p.tok.lit[0].is_capital() {
			p.check_name()
		}
		else {
			// Expression match
			match_expr,_ := p.expr(0)
			match_exprs << match_expr
		}
		p.warn('match block')
		stmts := p.parse_block()
		blocks << ast.StmtBlock{
			stmts: stmts
		}
		if p.tok.kind == .key_else {
			p.next()
			blocks << ast.StmtBlock{
				stmts: p.parse_block()
			}
		}
		// If the last statement is an expression, return its type
		if stmts.len > 0 {
			match stmts[stmts.len - 1] {
				ast.ExprStmt {
					type_sym := p.table.get_type_symbol(it.typ)
					p.warn('match expr ret $type_sym.name')
					return_type = it.typ
				}
				else {}
	}
		}
		if p.tok.kind == .rcbr {
			break
		}
	}
	p.check(.rcbr)
	mut node := ast.Expr{}
	node = ast.MatchExpr{
		blocks: blocks
		match_exprs: match_exprs
		typ: typ
		cond: cond
	}
	return node,return_type
}

fn (p mut Parser) enum_decl() ast.EnumDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_enum)
	name := p.check_name()
	p.check(.lcbr)
	mut vals := []string
	for p.tok.kind != .eof && p.tok.kind != .rcbr {
		val := p.check_name()
		vals << val
		p.warn('enum val $val')
	}
	p.check(.rcbr)
	return ast.EnumDecl{
		name: name
		is_pub: is_pub
		vals: vals
	}
}

fn (p mut Parser) type_decl() ast.TypeDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_type)
	name := p.check_name()
	// type SumType = A | B | c
	if p.tok.kind == .assign {
		p.next()
		for {
			p.check_name()
			if p.tok.kind != .pipe {
				break
			}
			p.check(.pipe)
		}
	}
	return ast.TypeDecl{
		name: name
	}
}

fn (p mut Parser) add_unresolved(key string, expr ast.Expr) table.Type {
	mut idx := p.unresolved_offset + p.unresolved.len
	if key in p.table.unresolved_idxs {
		idx = p.table.unresolved_idxs[key]
	}
	else {
		p.table.unresolved_idxs[key] = idx
		p.unresolved << expr
	}
	return table.new_type((-idx) - 1)
}

fn verror(s string) {
	println(s)
	exit(1)
}
