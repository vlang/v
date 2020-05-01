// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.scanner
import v.ast
import v.token
import v.table
import v.pref
import v.util
import term
import os

struct Parser {
	scanner           &scanner.Scanner
	file_name         string // "/home/user/hello.v"
	file_name_dir     string // "/home/user"
mut:
	tok               token.Token
	prev_tok          token.Token
	peek_tok          token.Token
	peek_tok2         token.Token
	table             &table.Table
	is_c              bool
	is_js             bool
	inside_if         bool
	inside_for        bool
	inside_fn         bool
	pref              &pref.Preferences
	builtin_mod       bool // are we in the `builtin` module?
	mod               string // current module name
	attr              string
	attr_ctdefine     string
	expr_mod          string
	scope             &ast.Scope
	global_scope      &ast.Scope
	imports           map[string]string
	ast_imports       []ast.Import
	is_amp            bool
	returns           bool
	inside_match      bool // to separate `match A { }` from `Struct{}`
	inside_match_case bool // to separate `match_expr { }` from `Struct{}`
	is_stmt_ident     bool // true while the beginning of a statement is an ident/selector
	inside_is         bool // `is Type`, expecting type
}

// for tests
pub fn parse_stmt(text string, table &table.Table, scope &ast.Scope) ast.Stmt {
	s := scanner.new_scanner(text, .skip_comments)
	mut p := Parser{
		scanner: s
		table: table
		pref: &pref.Preferences{}
		scope: scope
		global_scope: &ast.Scope{
			start_pos: 0
			parent: 0
		}
	}
	p.init_parse_fns()
	p.read_first_token()
	return p.stmt()
}

pub fn parse_file(path string, b_table &table.Table, comments_mode scanner.CommentsMode, pref &pref.Preferences, global_scope &ast.Scope) ast.File {
	// println('parse_file("$path")')
	// text := os.read_file(path) or {
	// panic(err)
	// }
	mut stmts := []ast.Stmt{}
	mut p := Parser{
		scanner: scanner.new_scanner_file(path, comments_mode)
		table: b_table
		file_name: path
		file_name_dir: os.dir(path)
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: 0
		}
		global_scope: global_scope
	}
	// comments_mode: comments_mode
	p.read_first_token()
	for p.tok.kind == .comment {
		mut stmt := ast.Stmt{} // TODO sum type << bug
		com := p.comment()
		stmt = com
		stmts << stmt
	}
	mut mstmt := ast.Stmt{}
	module_decl := p.module_decl()
	mstmt = module_decl
	stmts << mstmt
	// imports
	/*
	mut imports := []ast.Import{}
	for p.tok.kind == .key_import {
		imports << p.import_stmt()
	}
	*/
	// TODO: import only mode
	for {
		if p.tok.kind == .eof {
			if p.pref.is_script && !p.pref.is_test && p.mod == 'main' && !have_fn_main(stmts) {
				stmts << ast.FnDecl {
					name: 'main'
					return_type: table.void_type
				}
			}
			break
		}
		// println('stmt at ' + p.tok.str())
		stmts << p.top_stmt()
	}
	// println('nr stmts = $stmts.len')
	// println(stmts[0])
	p.scope.end_pos = p.tok.pos
	return ast.File{
		path: path
		mod: module_decl
		imports: p.ast_imports
		stmts: stmts
		scope: p.scope
		global_scope: p.global_scope
	}
}

/*
struct Queue {
mut:
	idx              int
	mu               sync.Mutex
	paths            []string
	table            &table.Table
	parsed_ast_files []ast.File
}

fn (q mut Queue) run() {
	q.mu.lock()
	idx := q.idx
	if idx >= q.paths.len {
		q.mu.unlock()
		return
	}
	q.idx++
	q.mu.unlock()
	path := q.paths[idx]
	file := parse_file(path, q.table, .skip_comments)
	q.mu.lock()
	q.parsed_ast_files << file
	q.mu.unlock()
}
*/
pub fn parse_files(paths []string, table &table.Table, pref &pref.Preferences, global_scope &ast.Scope) []ast.File {
	/*
	println('\n\n\nparse_files()')
	println(paths)
	nr_cpus := runtime.nr_cpus()
	println('nr_cpus= $nr_cpus')
	mut q := &Queue{
		paths: paths
		table: table
	}
	for i in 0 .. nr_cpus {
		go q.run()
	}
	time.sleep_ms(100)
	return q.parsed_ast_files
	*/
	// ///////////////
	mut files := []ast.File{}
	for path in paths {
		// println('parse_files $path')
		files << parse_file(path, table, .skip_comments, pref, global_scope)
	}
	return files
}

pub fn (p &Parser) init_parse_fns() {
	// p.prefix_parse_fns = make(100, 100, sizeof(PrefixParseFn))
	// p.prefix_parse_fns[token.Kind.name] = parse_name
	println('')
}

pub fn (mut p Parser) read_first_token() {
	// need to call next() three times to get peek token 1 & 2 and current token
	p.next()
	p.next()
	p.next()
}

pub fn (mut p Parser) open_scope() {
	p.scope = &ast.Scope{
		parent: p.scope
		start_pos: p.tok.pos
	}
}

pub fn (mut p Parser) close_scope() {
	if !p.pref.is_repl && !p.scanner.is_fmt {
		for _, obj in p.scope.objects {
			match obj {
				ast.Var {
					if !it.is_used && !it.name.starts_with('__') {
						if p.pref.is_prod {
							p.error_with_pos('unused variable: `$it.name`', it.pos)
						} else {
							p.warn_with_pos('unused variable: `$it.name`', it.pos)
						}
					}
				}
				else {}
			}
		}
	}
	p.scope.end_pos = p.tok.pos
	p.scope.parent.children << p.scope
	p.scope = p.scope.parent
}

pub fn (mut p Parser) parse_block() []ast.Stmt {
	p.open_scope()
	// println('parse block')
	stmts := p.parse_block_no_scope()
	p.close_scope()
	// println('nr exprs in block = $exprs.len')
	return stmts
}

pub fn (mut p Parser) parse_block_no_scope() []ast.Stmt {
	p.check(.lcbr)
	mut stmts := []ast.Stmt{}
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
	return stmts
}

/*
fn (mut p Parser) next_with_comment() {
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
}
*/
fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	p.peek_tok = p.peek_tok2
	p.peek_tok2 = p.scanner.scan()
	/*
	if p.tok.kind==.comment {
		p.comments << ast.Comment{text:p.tok.lit, line_nr:p.tok.line_nr}
		p.next()
	}
	*/
}

fn (mut p Parser) check(expected token.Kind) {
	// for p.tok.kind in [.line_comment, .mline_comment] {
	// p.next()
	// }
	if p.tok.kind != expected {
		p.error('unexpected `${p.tok.kind.str()}`, expecting `${expected.str()}`')
	}
	p.next()
}

fn (mut p Parser) check_name() string {
	name := p.tok.lit
	p.check(.name)
	return name
}

pub fn (mut p Parser) top_stmt() ast.Stmt {
	match p.tok.kind {
		.key_pub {
			match p.peek_tok.kind {
				.key_const {
					return p.const_decl()
				}
				.key_fn {
					return p.fn_decl()
				}
				.key_struct, .key_union {
					return p.struct_decl()
				}
				.key_interface {
					return p.interface_decl()
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
			return p.attribute()
		}
		.key_interface {
			return p.interface_decl()
		}
		.key_import {
			node := p.import_stmt()
			if node.len == 0 {
				return p.top_stmt()
			}
			p.ast_imports << node
			return node[0]
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
		.key_type {
			return p.type_decl()
		}
		.key_enum {
			return p.enum_decl()
		}
		.key_union {
			return p.struct_decl()
		}
		.comment {
			return p.comment()
		}
		else {
			if p.pref.is_script && !p.pref.is_test {
				p.scanner.add_fn_main_and_rescan(p.tok.pos-1)
				p.read_first_token()
				return p.top_stmt()
			} else {
				p.error('bad top level statement ' + p.tok.str())
				return ast.Stmt{}
			}
		}
	}
}

// TODO [if vfmt]
pub fn (mut p Parser) check_comment() ast.Comment {
	if p.tok.kind == .comment {
		return p.comment()
	}
	return ast.Comment{}
}

pub fn (mut p Parser) comment() ast.Comment {
	pos := p.tok.position()
	text := p.tok.lit
	p.next()
	// p.next_with_comment()
	return ast.Comment{
		text: text
		pos: pos
	}
}

pub fn (mut p Parser) stmt() ast.Stmt {
	p.is_stmt_ident = p.tok.kind == .name
	match p.tok.kind {
		.lcbr {
			stmts := p.parse_block()
			return ast.Block{
				stmts: stmts
			}
		}
		.key_assert {
			p.next()
			assert_pos := p.tok.position()
			expr := p.expr(0)
			return ast.AssertStmt{
				expr: expr
				pos: assert_pos
			}
		}
		.key_mut, .key_static {
			return p.assign_stmt()
		}
		.key_for {
			return p.for_stmt()
		}
		.comment {
			return p.comment()
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
				tok: tok
			}
		}
		.key_unsafe {
			p.next()
			stmts := p.parse_block()
			return ast.UnsafeStmt{
				stmts: stmts
			}
		}
		.hash {
			return p.hash()
		}
		.key_defer {
			p.next()
			stmts := p.parse_block()
			return ast.DeferStmt{
				stmts: stmts
			}
		}
		.key_go {
			p.next()
			expr := p.expr(0)
			// mut call_expr := &ast.CallExpr(0) // TODO
			// { call_expr = it }
			match expr {
				ast.CallExpr {}
				else {}
			}
			return ast.GoStmt{
				call_expr: expr
			}
		}
		.key_goto {
			p.next()
			name := p.check_name()
			return ast.GotoStmt{
				name: name
			}
		}
		else {
			// `x := ...`
			if p.tok.kind == .name && p.peek_tok.kind in [.decl_assign, .comma] {
				return p.assign_stmt()
			} else if p.tok.kind == .name && p.peek_tok.kind == .colon {
				// `label:`
				name := p.check_name()
				p.check(.colon)
				return ast.GotoLabel{
					name: name
				}
			} else if p.tok.kind == .name && p.peek_tok.kind == .name {
				p.error_with_pos('unexpected name `$p.peek_tok.lit`', p.peek_tok.position())
			}
			epos := p.tok.position()
			expr := p.expr(0)
			return ast.ExprStmt{
				expr: expr
				pos: epos
			}
		}
	}
}

fn (mut p Parser) attribute() ast.Attr {
	p.check(.lsbr)
	mut is_if_attr := false
	if p.tok.kind == .key_if {
		p.next()
		is_if_attr = true
	}
	mut name := p.check_name()
	if p.tok.kind == .colon {
		p.next()
		if p.tok.kind == .name {
			name += p.check_name()
		} else if p.tok.kind == .string {
			name += p.tok.lit
			p.next()
		}
	}
	p.check(.rsbr)
	p.attr = name
	if is_if_attr {
		p.attr_ctdefine = name
	}
	return ast.Attr{
		name: name
	}
}

/*
fn (mut p Parser) range_expr(low ast.Expr) ast.Expr {
	// ,table.Type) {
	if p.tok.kind != .dotdot {
		p.next()
	}
	p.check(.dotdot)
	mut high := ast.Expr{}
	if p.tok.kind != .rsbr {
		high = p.expr(0)
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
pub fn (p &Parser) error(s string) {
	p.error_with_pos(s, p.tok.position())
}

pub fn (p &Parser) warn(s string) {
	p.warn_with_pos(s, p.tok.position())
}

pub fn (p &Parser) error_with_pos(s string, pos token.Position) {
	mut kind := 'error:'
	if p.pref.is_verbose {
		print_backtrace()
		kind = 'parser error:'
	}
	ferror := util.formatted_error(kind, s, p.file_name, pos)
	eprintln(ferror)
	exit(1)
}

pub fn (p &Parser) warn_with_pos(s string, pos token.Position) {
	ferror := util.formatted_error('warning:', s, p.file_name, pos)
	eprintln(ferror)
}

pub fn (mut p Parser) parse_ident(is_c, is_js bool) ast.Ident {
	// p.warn('name ')
	pos := p.tok.position()
	mut name := p.check_name()
	if name == '_' {
		return ast.Ident{
			name: '_'
			kind: .blank_ident
			pos: pos
		}
	}
	if p.expr_mod.len > 0 {
		name = '${p.expr_mod}.$name'
	}
	return ast.Ident{
		kind: .unresolved
		name: name
		is_c: is_c
		is_js: is_js
		mod: p.mod
		pos: pos
	}
}

pub fn (mut p Parser) name_expr() ast.Expr {
	mut node := ast.Expr{}
	if p.inside_is {
		p.inside_is = false
		// get type position before moving to next
		type_pos := p.tok.position()
		return ast.Type{
			typ: p.parse_type()
			pos: type_pos
		}
	}
	is_c := p.tok.lit == 'C'
	is_js := p.tok.lit == 'JS'
	mut mod := ''
	// p.warn('resetting')
	p.expr_mod = ''
	// `map[string]int` initialization
	if p.tok.lit == 'map' && p.peek_tok.kind == .lsbr {
		map_type := p.parse_map_type()
		return ast.MapInit{
			typ: map_type
		}
	}
	// Raw string (`s := r'hello \n ')
	if p.tok.lit in ['r', 'c', 'js'] && p.peek_tok.kind == .string && p.prev_tok.kind != .str_dollar {
		return p.string_expr()
	}
	mut known_var := false
	if obj := p.scope.find(p.tok.lit) {
		match mut obj {
			ast.Var {
				known_var = true
				it.is_used = true
			}
			else {}
		}
	}
	if p.peek_tok.kind == .dot && !known_var && (is_c || is_js || p.known_import(p.tok.lit) ||
		p.mod.all_after('.') == p.tok.lit) {
		if is_c {
			mod = 'C'
		} else if is_js {
			mod = 'JS'
		} else {
			// prepend the full import
			mod = p.imports[p.tok.lit]
		}
		p.next()
		p.check(.dot)
		p.expr_mod = mod
	}
	// p.warn('name expr  $p.tok.lit $p.peek_tok.str()')
	// fn call or type cast
	if p.peek_tok.kind == .lpar {
		mut name := p.tok.lit
		if mod.len > 0 {
			name = '${mod}.$name'
		}
		name_w_mod := p.prepend_mod(name)
		// type cast. TODO: finish
		// if name in table.builtin_type_names {
		if !known_var && (name in p.table.type_idxs || name_w_mod in p.table.type_idxs) &&
			name !in ['C.stat', 'C.sigaction'] {
			// TODO handle C.stat()
			mut to_typ := p.parse_type()
			if p.is_amp {
				// Handle `&Foo(0)`
				to_typ = to_typ.to_ptr()
			}
			p.check(.lpar)
			mut expr := ast.Expr{}
			mut arg := ast.Expr{}
			mut has_arg := false
			expr = p.expr(0)
			// TODO, string(b, len)
			if p.tok.kind == .comma && to_typ.idx() == table.string_type_idx {
				p.check(.comma)
				arg = p.expr(0) // len
				has_arg = true
			}
			p.check(.rpar)
			node = ast.CastExpr{
				typ: to_typ
				expr: expr
				arg: arg
				has_arg: has_arg
			}
			p.expr_mod = ''
			return node
		} else {
			// fn call
			// println('calling $p.tok.lit')
			x := p.call_expr(is_c, is_js, mod) // TODO `node,typ :=` should work
			node = x
		}
	} else if p.peek_tok.kind == .lcbr && !p.inside_match && !p.inside_match_case && !p.inside_if &&
		!p.inside_for {
		return p.struct_init(false) // short_syntax: false
	} else if p.peek_tok.kind == .dot && (p.tok.lit[0].is_capital() && !known_var) {
		// `Color.green`
		mut enum_name := p.check_name()
		if mod != '' {
			enum_name = mod + '.' + enum_name
		} else {
			enum_name = p.prepend_mod(enum_name)
		}
		// p.warn('Color.green $enum_name ' + p.prepend_mod(enum_name) + 'mod=$mod')
		p.check(.dot)
		val := p.check_name()
		// println('enum val $enum_name . $val')
		p.expr_mod = ''
		return ast.EnumVal{
			enum_name: enum_name
			val: val
			pos: p.tok.position()
			mod: mod
		}
	} else {
		node = p.parse_ident(is_c, is_js)
	}
	p.expr_mod = ''
	return node
}

fn (mut p Parser) index_expr(left ast.Expr) ast.IndexExpr {
	// left == `a` in `a[0]`
	p.next() // [
	mut has_low := true
	if p.tok.kind == .dotdot {
		has_low = false
		// [..end]
		p.next()
		high := p.expr(0)
		p.check(.rsbr)
		return ast.IndexExpr{
			left: left
			pos: p.tok.position()
			index: ast.RangeExpr{
				low: ast.Expr{}
				high: high
				has_high: true
			}
		}
	}
	expr := p.expr(0) // `[expr]` or  `[expr..]`
	mut has_high := false
	if p.tok.kind == .dotdot {
		// [start..end] or [start..]
		p.check(.dotdot)
		mut high := ast.Expr{}
		if p.tok.kind != .rsbr {
			has_high = true
			high = p.expr(0)
		}
		p.check(.rsbr)
		return ast.IndexExpr{
			left: left
			pos: p.tok.position()
			index: ast.RangeExpr{
				low: expr
				high: high
				has_high: has_high
				has_low: has_low
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

fn (mut p Parser) filter() {
	p.scope.register('it', ast.Var{
		name: 'it'
		pos: p.tok.position()
		is_used: true
	})
}

fn (mut p Parser) dot_expr(left ast.Expr) ast.Expr {
	p.next()
	mut name_pos := p.tok.position()
	field_name := p.check_name()
	is_filter := field_name in ['filter', 'map']
	if is_filter {
		p.open_scope()
		name_pos = p.tok.position()
		p.filter()
		// wrong tok position when using defer
		// defer {
		// p.close_scope()
		// }
	}
	// Method call
	if p.tok.kind == .lpar {
		p.next()
		args := p.call_args()
		p.check(.rpar)
		mut or_stmts := []ast.Stmt{}
		mut is_or_block_used := false
		if p.tok.kind == .key_orelse {
			p.next()
			p.open_scope()
			p.scope.register('errcode', ast.Var{
				name: 'errcode'
				typ: table.int_type
				pos: p.tok.position()
				is_used: true
			})
			p.scope.register('err', ast.Var{
				name: 'err'
				typ: table.string_type
				pos: p.tok.position()
				is_used: true
			})
			is_or_block_used = true
			or_stmts = p.parse_block_no_scope()
			p.close_scope()
		}
		end_pos := p.tok.position()
		pos := token.Position{
			line_nr: name_pos.line_nr
			pos: name_pos.pos
			len: end_pos.pos - name_pos.pos
		}
		mcall_expr := ast.CallExpr{
			left: left
			name: field_name
			args: args
			pos: pos
			is_method: true
			or_block: ast.OrExpr{
				stmts: or_stmts
				is_used: is_or_block_used
			}
		}
		mut node := ast.Expr{}
		node = mcall_expr
		if is_filter {
			p.close_scope()
		}
		return node
	}
	sel_expr := ast.SelectorExpr{
		expr: left
		field: field_name
		pos: name_pos
	}
	mut node := ast.Expr{}
	node = sel_expr
	if is_filter {
		p.close_scope()
	}
	return node
}

// `.green`
// `pref.BuildMode.default_mode`
fn (mut p Parser) enum_val() ast.EnumVal {
	p.check(.dot)
	val := p.check_name()
	return ast.EnumVal{
		val: val
		pos: p.tok.position()
	}
}

fn (mut p Parser) string_expr() ast.Expr {
	is_raw := p.tok.kind == .name && p.tok.lit == 'r'
	is_cstr := p.tok.kind == .name && p.tok.lit == 'c'
	if is_raw || is_cstr {
		p.next()
	}
	mut node := ast.Expr{}
	val := p.tok.lit
	pos := p.tok.position()
	if p.peek_tok.kind != .str_dollar {
		p.next()
		node = ast.StringLiteral{
			val: val
			is_raw: is_raw
			is_c: is_cstr
			pos: pos
		}
		return node
	}
	mut exprs := []ast.Expr{}
	mut vals := []string{}
	mut efmts := []string{}
	// Handle $ interpolation
	for p.tok.kind == .string {
		vals << p.tok.lit
		p.next()
		if p.tok.kind != .str_dollar {
			continue
		}
		p.check(.str_dollar)
		exprs << p.expr(0)
		mut efmt := []string{}
		if p.tok.kind == .colon {
			efmt << ':'
			p.next()
			// ${num:-2d}
			if p.tok.kind == .minus {
				efmt << '-'
				p.next()
			}
			// ${num:2d}
			if p.tok.kind == .number {
				efmt << p.tok.lit
				p.next()
			}
			if p.tok.lit.len == 1 {
				efmt << p.tok.lit
				p.next()
			}
		}
		efmts << efmt.join('')
	}
	node = ast.StringInterLiteral{
		vals: vals
		exprs: exprs
		expr_fmts: efmts
		pos: pos
	}
	return node
}

fn (mut p Parser) parse_number_literal() ast.Expr {
	lit := p.tok.lit
	pos := p.tok.position()
	mut node := ast.Expr{}
	if lit.index_any('.eE') >= 0 && lit[..2] !in ['0x', '0X', '0o', '0O', '0b', '0B'] {
		node = ast.FloatLiteral{
			val: lit
			pos: pos
		}
	} else {
		node = ast.IntegerLiteral{
			val: lit
			pos: pos
		}
	}
	p.next()
	return node
}

fn (mut p Parser) module_decl() ast.Module {
	mut name := 'main'
	is_skipped := p.tok.kind != .key_module
	if !is_skipped {
		p.check(.key_module)
		name = p.check_name()
	}
	full_mod := p.table.qualify_module(name, p.file_name)
	p.mod = full_mod
	p.builtin_mod = p.mod == 'builtin'
	return ast.Module{
		name: full_mod
		is_skipped: is_skipped
	}
}

fn (mut p Parser) parse_import() ast.Import {
	pos := p.tok.position()
	mut mod_name := p.check_name()
	mut mod_alias := mod_name
	for p.tok.kind == .dot {
		p.check(.dot)
		submod_name := p.check_name()
		mod_name += '.' + submod_name
		mod_alias = submod_name
	}
	if p.tok.kind == .key_as {
		p.check(.key_as)
		mod_alias = p.check_name()
	}
	p.imports[mod_alias] = mod_name
	p.table.imports << mod_name
	return ast.Import{
		mod: mod_name
		alias: mod_alias
		pos: pos
	}
}

fn (mut p Parser) import_stmt() []ast.Import {
	p.check(.key_import)
	mut imports := []ast.Import{}
	if p.tok.kind == .lpar {
		p.warn('`import()` has been deprecated, use `import x` instead. run `v fmt` to handle the transition')
		p.check(.lpar)
		for p.tok.kind != .rpar {
			imports << p.parse_import()
			if p.tok.kind == .comment {
				p.comment()
			}
		}
		p.check(.rpar)
	} else {
		// p.warn('`import module` has been deprecated, use `import ( module )` instead')
		imports << p.parse_import()
	}
	return imports
}

fn (mut p Parser) const_decl() ast.ConstDecl {
	start_pos := p.tok.position()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	end_pos := p.tok.position()
	p.check(.key_const)
	if p.tok.kind != .lpar {
		p.error('consts must be grouped, e.g.\nconst (\n\ta = 1\n)')
	}
	p.next() // (
	mut fields := []ast.ConstField{}
	for p.tok.kind != .rpar {
		if p.tok.kind == .comment {
			p.comment()
		}
		pos := p.tok.position()
		name := p.prepend_mod(p.check_name())
		// name := p.check_name()
		// println('!!const: $name')
		p.check(.assign)
		expr := p.expr(0)
		field := ast.ConstField{
			name: name
			expr: expr
			pos: pos
		}
		fields << field
		p.global_scope.register(field.name, field)
	}
	p.check(.rpar)
	return ast.ConstDecl{
		pos: start_pos.extend(end_pos)
		fields: fields
		is_pub: is_pub
	}
}

fn (mut p Parser) return_stmt() ast.Return {
	first_pos := p.tok.position()
	p.next()
	// return expressions
	mut exprs := []ast.Expr{}
	if p.tok.kind == .rcbr {
		return ast.Return{
			pos: first_pos
		}
	}
	for {
		expr := p.expr(0)
		exprs << expr
		if p.tok.kind == .comma {
			p.check(.comma)
		} else {
			break
		}
	}
	end_pos := exprs.last().position()
	return ast.Return{
		exprs: exprs
		pos: first_pos.extend(end_pos)
	}
}

// left hand side of `=` or `:=` in `a,b,c := 1,2,3`
fn (mut p Parser) global_decl() ast.GlobalDecl {
	if !p.pref.translated && !p.pref.is_livemain && !p.builtin_mod && !p.pref.building_v && p.mod !=
		'ui' && p.mod != 'gg2' && p.mod != 'uiold' && !os.getwd().contains('/volt') && !p.pref.enable_globals {
		p.error('use `v --enable-globals ...` to enable globals')
	}
	p.next()
	name := p.check_name()
	// println(name)
	typ := p.parse_type()
	mut expr := ast.Expr{}
	has_expr := p.tok.kind == .assign
	if has_expr {
		p.next()
		expr = p.expr(0)
	}
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
	glob := ast.GlobalDecl{
		name: name
		typ: typ
		has_expr: has_expr
		expr: expr
	}
	p.global_scope.register(name, glob)
	return glob
}

fn (mut p Parser) enum_decl() ast.EnumDecl {
	is_pub := p.tok.kind == .key_pub
	start_pos := p.tok.position()
	if is_pub {
		p.next()
	}
	p.check(.key_enum)
	end_pos := p.tok.position()
	enum_name := p.check_name()
	if enum_name.len > 0 && !enum_name[0].is_capital() {
		verror('enum name `$enum_name` must begin with a capital letter')
	}
	name := p.prepend_mod(enum_name)
	p.check(.lcbr)
	mut vals := []string{}
	// mut default_exprs := []ast.Expr{}
	mut fields := []ast.EnumField{}
	for p.tok.kind != .eof && p.tok.kind != .rcbr {
		pos := p.tok.position()
		val := p.check_name()
		vals << val
		mut expr := ast.Expr{}
		mut has_expr := false
		// p.warn('enum val $val')
		if p.tok.kind == .assign {
			p.next()
			expr = p.expr(0)
			has_expr = true
		}
		fields << ast.EnumField{
			name: val
			pos: pos
			expr: expr
			has_expr: has_expr
		}
	}
	p.check(.rcbr)
	p.table.register_type_symbol(table.TypeSymbol{
		kind: .enum_
		name: name
		info: table.Enum{
			vals: vals
		}
	})
	return ast.EnumDecl{
		name: name
		is_pub: is_pub
		fields: fields
		pos: start_pos.extend(end_pos)
	}
}

fn (mut p Parser) type_decl() ast.TypeDecl {
	start_pos := p.tok.position()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_type)
	end_pos := p.tok.position()
	decl_pos := start_pos.extend(end_pos)
	name := p.check_name()
	mut sum_variants := []table.Type{}
	if p.tok.kind == .assign {
		p.next() // TODO require `=`
	}
	if p.tok.kind == .key_fn {
		// function type: `type mycallback fn(string, int)`
		fn_name := p.prepend_mod(name)
		fn_type := p.parse_fn_type(fn_name)
		return ast.FnTypeDecl{
			name: fn_name
			is_pub: is_pub
			typ: fn_type
			pos: decl_pos
		}
	}
	first_type := p.parse_type() // need to parse the first type before we can check if it's `type A = X | Y`
	if p.tok.kind == .pipe {
		p.check(.pipe)
		sum_variants << first_type
		// type SumType = A | B | c
		for {
			variant_type := p.parse_type()
			sum_variants << variant_type
			if p.tok.kind != .pipe {
				break
			}
			p.check(.pipe)
		}
		p.table.register_type_symbol(table.TypeSymbol{
			kind: .sum_type
			name: p.prepend_mod(name)
			info: table.SumType{
				variants: sum_variants
			}
		})
		return ast.SumTypeDecl{
			name: name
			is_pub: is_pub
			sub_types: sum_variants
			pos: decl_pos
		}
	}
	// type MyType int
	parent_type := first_type
	pid := parent_type.idx()
	p.table.register_type_symbol(table.TypeSymbol{
		kind: .alias
		name: p.prepend_mod(name)
		parent_idx: pid
		info: table.Alias{
			foo: ''
		}
	})
	return ast.AliasTypeDecl{
		name: name
		is_pub: is_pub
		parent_type: parent_type
		pos: decl_pos
	}
}

fn (mut p Parser) assoc() ast.Assoc {
	var_name := p.check_name()
	pos := p.tok.position()
	mut v := p.scope.find_var(var_name) or {
		p.error('unknown variable `$var_name`')
		return ast.Assoc{}
	}
	v.is_used = true
	// println('assoc var $name typ=$var.typ')
	mut fields := []string{}
	mut vals := []ast.Expr{}
	p.check(.pipe)
	for {
		fields << p.check_name()
		p.check(.colon)
		expr := p.expr(0)
		vals << expr
		if p.tok.kind == .comma {
			p.check(.comma)
		}
		if p.tok.kind == .rcbr {
			break
		}
	}
	return ast.Assoc{
		var_name: var_name
		fields: fields
		exprs: vals
		pos: pos
	}
}

fn (p &Parser) new_true_expr() ast.Expr {
	return ast.BoolLiteral{
		val: true
		pos: p.tok.position()
	}
}

fn verror(s string) {
	util.verror('parser error', s)
}
