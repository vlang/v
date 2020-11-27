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
import v.errors
import os
import runtime
import time

pub struct Parser {
	file_base         string // "hello.v"
	file_name         string // "/home/user/hello.v"
	file_name_dir     string // "/home/user"
	pref              &pref.Preferences
mut:
	scanner           &scanner.Scanner
	comments_mode     scanner.CommentsMode = .skip_comments
	// see comment in parse_file
	tok               token.Token
	prev_tok          token.Token
	peek_tok          token.Token
	peek_tok2         token.Token
	peek_tok3         token.Token
	table             &table.Table
	language          table.Language
	inside_if         bool
	inside_if_expr    bool
	inside_ct_if_expr bool
	inside_or_expr    bool
	inside_for        bool
	inside_fn         bool
	inside_str_interp bool
	builtin_mod       bool // are we in the `builtin` module?
	mod               string // current module name
	attrs             []table.Attr // attributes before next decl stmt
	expr_mod          string // for constructing full type names in parse_type()
	scope             &ast.Scope
	global_scope      &ast.Scope
	imports           map[string]string // alias => mod_name
	ast_imports       []ast.Import // mod_names
	used_imports      []string // alias
	is_amp            bool // for generating the right code for `&Foo{}`
	returns           bool
	inside_match      bool // to separate `match A { }` from `Struct{}`
	inside_select     bool // to allow `ch <- Struct{} {` inside `select`
	inside_match_case bool // to separate `match_expr { }` from `Struct{}`
	inside_match_body bool // to fix eval not used TODO
	inside_unsafe     bool
	is_stmt_ident     bool // true while the beginning of a statement is an ident/selector
	expecting_type    bool // `is Type`, expecting type
	errors            []errors.Error
	warnings          []errors.Warning
	vet_errors        []string
	cur_fn_name       string
	in_generic_params bool // indicates if parsing between `<` and `>` of a method/function
}

// for tests
pub fn parse_stmt(text string, table &table.Table, scope &ast.Scope) ast.Stmt {
	pref := &pref.Preferences{}
	s := scanner.new_scanner(text, .skip_comments, pref)
	mut p := Parser{
		scanner: s
		table: table
		pref: pref
		scope: scope
		global_scope: &ast.Scope{
			start_pos: 0
			parent: 0
		}
	}
	p.init_parse_fns()
	p.read_first_token()
	return p.stmt(false)
}

pub fn parse_text(text string, b_table &table.Table, pref &pref.Preferences, scope &ast.Scope, global_scope &ast.Scope) ast.File {
	s := scanner.new_scanner(text, .skip_comments, pref)
	mut p := Parser{
		scanner: s
		table: b_table
		pref: pref
		scope: scope
		errors: []errors.Error{}
		warnings: []errors.Warning{}
		global_scope: global_scope
	}
	return p.parse()
}

pub fn parse_file(path string, b_table &table.Table, comments_mode scanner.CommentsMode, pref &pref.Preferences, global_scope &ast.Scope) ast.File {
	// NB: when comments_mode == .toplevel_comments,
	// the parser gives feedback to the scanner about toplevel statements, so that the scanner can skip
	// all the tricky inner comments. This is needed because we do not have a good general solution
	// for handling them, and should be removed when we do (the general solution is also needed for vfmt)
	// println('parse_file("$path")')
	// text := os.read_file(path) or {
	// panic(err)
	// }
	mut p := Parser{
		scanner: scanner.new_scanner_file(path, comments_mode, pref)
		comments_mode: comments_mode
		table: b_table
		file_name: path
		file_base: os.base(path)
		file_name_dir: os.dir(path)
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: global_scope
		}
		errors: []errors.Error{}
		warnings: []errors.Warning{}
		global_scope: global_scope
	}
	return p.parse()
}

pub fn parse_vet_file(path string, table_ &table.Table, pref &pref.Preferences) (ast.File, []string) {
	global_scope := &ast.Scope{
		parent: 0
	}
	mut p := Parser{
		scanner: scanner.new_vet_scanner_file(path, .parse_comments, pref)
		comments_mode: .parse_comments
		table: table_
		file_name: path
		file_base: os.base(path)
		file_name_dir: os.dir(path)
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: global_scope
		}
		errors: []errors.Error{}
		warnings: []errors.Warning{}
		global_scope: global_scope
	}
	if p.scanner.text.contains('\n  ') {
		source_lines := os.read_lines(path) or {
			[]string{}
		}
		for lnumber, line in source_lines {
			if line.starts_with('  ') {
				p.vet_error('Looks like you are using spaces for indentation.', lnumber)
			}
		}
	}
	file := p.parse()
	p.vet_errors << p.scanner.vet_errors
	return file, p.vet_errors
}

pub fn (mut p Parser) parse() ast.File {
	// comments_mode: comments_mode
	p.init_parse_fns()
	p.read_first_token()
	mut stmts := []ast.Stmt{}
	for p.tok.kind == .comment {
		stmts << p.comment_stmt()
	}
	// module
	module_decl := p.module_decl()
	stmts << module_decl
	// imports
	for {
		if p.tok.kind == .key_import {
			stmts << p.import_stmt()
			continue
		}
		if p.tok.kind == .comment {
			stmts << p.comment_stmt()
			continue
		}
		break
	}
	for {
		if p.tok.kind == .eof {
			p.check_unused_imports()
			break
		}
		// println('stmt at ' + p.tok.str())
		stmts << p.top_stmt()
		// clear the attributes after each statement
		p.attrs = []
	}
	// println('nr stmts = $stmts.len')
	// println(stmts[0])
	p.scope.end_pos = p.tok.pos
	//
	return ast.File{
		path: p.file_name
		mod: module_decl
		imports: p.ast_imports
		stmts: stmts
		scope: p.scope
		global_scope: p.global_scope
		errors: p.errors
		warnings: p.warnings
	}
}

/*
struct Queue {
mut:
	idx              int
	mu               &sync.Mutex
	mu2              &sync.Mutex
	paths            []string
	table            &table.Table
	parsed_ast_files []ast.File
	pref             &pref.Preferences
	global_scope     &ast.Scope
}

fn (mut q Queue) run() {
	for {
		q.mu.lock()
		idx := q.idx
		if idx >= q.paths.len {
			q.mu.unlock()
			return
		}
		q.idx++
		q.mu.unlock()
		println('run(idx=$idx)')
		path := q.paths[idx]
		file := parse_file(path, q.table, .skip_comments, q.pref, q.global_scope)
		q.mu2.lock()
		q.parsed_ast_files << file
		q.mu2.unlock()
		println('run done(idx=$idx)')
	}
}
*/
pub fn parse_files(paths []string, table &table.Table, pref &pref.Preferences, global_scope &ast.Scope) []ast.File {
	// println('nr_cpus= $nr_cpus')
	$if macos {
		/*
		if pref.is_parallel && paths[0].contains('/array.v') {
			println('\n\n\nparse_files() nr_files=$paths.len')
			println(paths)
			nr_cpus := runtime.nr_cpus()
			mut q := &Queue{
				paths: paths
				table: table
				pref: pref
				global_scope: global_scope
				mu: sync.new_mutex()
				mu2: sync.new_mutex()
			}
			for _ in 0 .. nr_cpus - 1 {
				go q.run()
			}
			time.sleep_ms(1000)
			println('all done')
			return q.parsed_ast_files
		}
		*/
	}
	if false {
		// TODO: remove this; it just prevents warnings about unused time and runtime
		time.sleep_ms(1)
		println(runtime.nr_cpus())
	}
	// ///////////////
	mut files := []ast.File{}
	for path in paths {
		// println('parse_files $path')
		files << parse_file(path, table, .skip_comments, pref, global_scope)
	}
	return files
}

pub fn (mut p Parser) init_parse_fns() {
	if p.comments_mode == .toplevel_comments {
		p.scanner.scan_all_tokens_in_buffer()
	}
	// p.prefix_parse_fns = make(100, 100, sizeof(PrefixParseFn))
	// p.prefix_parse_fns[token.Kind.name] = parse_name
}

pub fn (mut p Parser) read_first_token() {
	// need to call next() 4 times to get peek token 1,2,3 and current token
	p.next()
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
	// p.scope.end_pos = p.tok.pos
	// NOTE: since this is usually called after `p.parse_block()`
	// ie. when `prev_tok` is rcbr `}` we most likely want `prev_tok`
	// we could do the following, but probably not needed in 99% of cases:
	// `end_pos = if p.prev_tok.kind == .rcbr { p.prev_tok.pos } else { p.tok.pos }`
	p.scope.end_pos = p.prev_tok.pos
	p.scope.parent.children << p.scope
	p.scope = p.scope.parent
}

pub fn (mut p Parser) parse_block() []ast.Stmt {
	p.open_scope()
	// println('parse block')
	stmts := p.parse_block_no_scope(false)
	p.close_scope()
	// println('nr exprs in block = $exprs.len')
	return stmts
}

pub fn (mut p Parser) parse_block_no_scope(is_top_level bool) []ast.Stmt {
	p.check(.lcbr)
	mut stmts := []ast.Stmt{}
	if p.tok.kind != .rcbr {
		mut c := 0
		for {
			stmts << p.stmt(is_top_level)
			// p.warn('after stmt(): tok=$p.tok.str()')
			if p.tok.kind in [.eof, .rcbr] {
				break
			}
			c++
			if c % 100000 == 0 {
				eprintln('parsed $c statements so far from fn $p.cur_fn_name ...')
			}
			if c > 1000000 {
				p.error_with_pos('parsed over $c statements from fn $p.cur_fn_name, the parser is probably stuck',
					p.tok.position())
			}
		}
	}
	if is_top_level {
		p.top_level_statement_end()
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
	p.peek_tok2 = p.peek_tok3
	p.peek_tok3 = p.scanner.scan()
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
		if p.tok.kind == .name {
			p.error('unexpected name `$p.tok.lit`, expecting `$expected.str()`')
		} else {
			p.error('unexpected `$p.tok.kind.str()`, expecting `$expected.str()`')
		}
	}
	p.next()
}

// JS functions can have multiple dots in their name:
// JS.foo.bar.and.a.lot.more.dots()
fn (mut p Parser) check_js_name() string {
	mut name := ''
	for p.peek_tok.kind == .dot {
		name += '${p.tok.lit}.'
		p.next() // .name
		p.next() // .dot
	}
	// last .name
	name += p.tok.lit
	p.next()
	return name
}

fn (mut p Parser) check_name() string {
	name := p.tok.lit
	if p.peek_tok.kind == .dot && name in p.imports {
		p.register_used_import(name)
	}
	p.check(.name)
	return name
}

pub fn (mut p Parser) top_stmt() ast.Stmt {
	$if trace_parser ? {
		tok_pos := p.tok.position()
		eprintln('parsing file: ${p.file_name:-30} | tok.kind: ${p.tok.kind:-10} | tok.lit: ${p.tok.lit:-10} | tok_pos: ${tok_pos.str():-45} | top_stmt')
	}
	for {
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
				// attrs are stored in `p.attrs`
				p.attributes()
				continue
			}
			.key_interface {
				return p.interface_decl()
			}
			.key_import {
				p.error_with_pos('`import x` can only be declared at the beginning of the file',
					p.tok.position())
				return p.import_stmt()
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
				return ast.ExprStmt{
					expr: p.if_expr(true)
				}
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
				return p.comment_stmt()
			}
			else {
				if p.pref.is_script && !p.pref.is_test {
					mut stmts := []ast.Stmt{}
					for p.tok.kind != .eof {
						stmts << p.stmt(false)
					}
					return ast.FnDecl{
						name: 'main.main'
						mod: 'main'
						stmts: stmts
						file: p.file_name
						return_type: table.void_type
					}
				} else if p.pref.is_fmt {
					return p.stmt(false)
				} else {
					p.error('bad top level statement ' + p.tok.str())
					return ast.Stmt{}
				}
			}
		}
	}
	// TODO remove dummy return statement
	// the compiler complains if it's not there
	return ast.Stmt{}
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
		is_multi: text.contains('\n')
		text: text
		pos: pos
	}
}

pub fn (mut p Parser) comment_stmt() ast.ExprStmt {
	comment := p.comment()
	return ast.ExprStmt{
		expr: comment
		pos: comment.pos
	}
}

pub fn (mut p Parser) eat_comments() []ast.Comment {
	mut comments := []ast.Comment{}
	for {
		if p.tok.kind != .comment {
			break
		}
		comments << p.comment()
	}
	return comments
}

pub fn (mut p Parser) eat_lineend_comments() []ast.Comment {
	mut comments := []ast.Comment{}
	for {
		if p.tok.kind != .comment || p.tok.line_nr != p.prev_tok.line_nr {
			break
		}
		comments << p.comment()
	}
	return comments
}

pub fn (mut p Parser) stmt(is_top_level bool) ast.Stmt {
	$if trace_parser ? {
		tok_pos := p.tok.position()
		eprintln('parsing file: ${p.file_name:-30} | tok.kind: ${p.tok.kind:-10} | tok.lit: ${p.tok.lit:-10} | tok_pos: ${tok_pos.str():-45} | stmt($is_top_level)')
	}
	p.is_stmt_ident = p.tok.kind == .name
	match p.tok.kind {
		.lcbr {
			pos := p.tok.position()
			stmts := p.parse_block()
			return ast.Block{
				stmts: stmts
				pos: pos
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
		.key_for {
			return p.for_stmt()
		}
		.name, .key_mut, .key_shared, .key_atomic, .key_static, .mul {
			if p.tok.kind == .name {
				if p.tok.lit == 'sql' {
					return p.sql_stmt()
				}
				if p.peek_tok.kind == .colon {
					// `label:`
					spos := p.tok.position()
					name := p.check_name()
					p.next()
					if p.tok.kind == .key_for {
						mut stmt := p.stmt(is_top_level)
						match mut stmt {
							ast.ForStmt {
								stmt.label = name
								return stmt
							}
							ast.ForInStmt {
								stmt.label = name
								return stmt
							}
							ast.ForCStmt {
								stmt.label = name
								return stmt
							}
							else {
								assert false
							}
						}
					}
					return ast.GotoLabel{
						name: name
						pos: spos.extend(p.tok.position())
					}
				} else if p.peek_tok.kind == .name {
					p.error_with_pos('unexpected name `$p.peek_tok.lit`', p.peek_tok.position())
				} else if !p.inside_if_expr && !p.inside_match_body && !p.inside_or_expr &&
					p.peek_tok.kind in [.rcbr, .eof] {
					p.error_with_pos('`$p.tok.lit` evaluated but not used', p.tok.position())
				}
			}
			return p.parse_multi_expr(is_top_level)
		}
		.comment {
			return p.comment_stmt()
		}
		.key_return {
			return p.return_stmt()
		}
		.dollar {
			if p.peek_tok.kind == .key_if {
				return ast.ExprStmt{
					expr: p.if_expr(true)
				}
			} else if p.peek_tok.kind == .key_for {
				return p.comp_for()
			} else if p.peek_tok.kind == .name {
				return ast.ExprStmt{
					expr: p.vweb()
				}
			}
			p.error_with_pos('unexpected \$', p.tok.position())
			return ast.Stmt{}
		}
		.key_continue, .key_break {
			tok := p.tok
			line := p.tok.line_nr
			p.next()
			mut label := ''
			if p.tok.line_nr == line && p.tok.kind == .name {
				label = p.check_name()
			}
			return ast.BranchStmt{
				kind: tok.kind
				label: label
				pos: tok.position()
			}
		}
		.key_unsafe {
			return p.unsafe_stmt()
		}
		.hash {
			return p.hash()
		}
		.key_defer {
			p.next()
			spos := p.tok.position()
			stmts := p.parse_block()
			return ast.DeferStmt{
				stmts: stmts
				pos: spos.extend(p.tok.position())
			}
		}
		.key_go {
			p.next()
			spos := p.tok.position()
			expr := p.expr(0)
			// mut call_expr := &ast.CallExpr(0) // TODO
			// { call_expr = it }
			return ast.GoStmt{
				call_expr: expr
				pos: spos.extend(p.tok.position())
			}
		}
		.key_goto {
			p.next()
			spos := p.tok.position()
			name := p.check_name()
			return ast.GotoStmt{
				name: name
				pos: spos
			}
		}
		.key_const {
			p.error_with_pos('const can only be defined at the top level (outside of functions)',
				p.tok.position())
			return ast.Stmt{}
		}
		// literals, 'if', etc. in here
		else {
			return p.parse_multi_expr(is_top_level)
		}
	}
}

fn (mut p Parser) expr_list() ([]ast.Expr, []ast.Comment) {
	mut exprs := []ast.Expr{}
	mut comments := []ast.Comment{}
	for {
		expr := p.expr(0)
		if expr is ast.Comment {
			comments << expr
		} else {
			exprs << expr
			if p.tok.kind != .comma {
				break
			}
			p.next()
		}
	}
	return exprs, comments
}

// when is_top_stmt is true attrs are added to p.attrs
fn (mut p Parser) attributes() {
	p.check(.lsbr)
	mut has_ctdefine := false
	for p.tok.kind != .rsbr {
		start_pos := p.tok.position()
		attr := p.parse_attr()
		if p.attrs.contains(attr.name) {
			p.error_with_pos('duplicate attribute `$attr.name`', start_pos.extend(p.prev_tok.position()))
		}
		if attr.is_ctdefine {
			if has_ctdefine {
				p.error_with_pos('only one `[if flag]` may be applied at a time `$attr.name`',
					start_pos.extend(p.prev_tok.position()))
			} else {
				has_ctdefine = true
			}
		}
		p.attrs << attr
		if p.tok.kind != .semicolon {
			if p.tok.kind == .rsbr {
				p.next()
				break
			}
			p.error('unexpected `$p.tok.kind.str()`, expecting `;`')
		}
		p.next()
	}
	if p.attrs.len == 0 {
		p.error_with_pos('attributes cannot be empty', p.prev_tok.position().extend(p.tok.position()))
	}
}

fn (mut p Parser) parse_attr() table.Attr {
	if p.tok.kind == .key_unsafe {
		p.next()
		return table.Attr{
			name: 'unsafe'
		}
	}
	mut is_ctdefine := false
	if p.tok.kind == .key_if {
		p.next()
		is_ctdefine = true
	}
	mut name := ''
	mut arg := ''
	is_string := p.tok.kind == .string
	mut is_string_arg := false
	if is_string {
		name = p.tok.lit
		p.next()
	} else {
		name = p.check_name()
		if name == 'unsafe_fn' {
			p.error_with_pos('please use `[unsafe]` instead', p.tok.position())
		} else if name == 'trusted_fn' {
			p.error_with_pos('please use `[trusted]` instead', p.tok.position())
		}
		if p.tok.kind == .colon {
			p.next()
			// `name: arg`
			if p.tok.kind == .name {
				arg = p.check_name()
			} else if p.tok.kind == .string { // `name: 'arg'`
				arg = p.tok.lit
				is_string_arg = true
				p.next()
			}
		}
	}
	return table.Attr{
		name: name
		is_string: is_string
		is_ctdefine: is_ctdefine
		arg: arg
		is_string_arg: is_string_arg
	}
}

pub fn (mut p Parser) error(s string) {
	p.error_with_pos(s, p.tok.position())
}

pub fn (mut p Parser) warn(s string) {
	p.warn_with_pos(s, p.tok.position())
}

pub fn (mut p Parser) error_with_pos(s string, pos token.Position) {
	mut kind := 'error:'
	if p.pref.output_mode == .stdout {
		if p.pref.is_verbose {
			print_backtrace()
			kind = 'parser error:'
		}
		ferror := util.formatted_error(kind, s, p.file_name, pos)
		eprintln(ferror)
		exit(1)
	} else {
		p.errors << errors.Error{
			file_path: p.file_name
			pos: pos
			reporter: .parser
			message: s
		}
	}
	if p.pref.output_mode == .silent {
		// Normally, parser errors mean that the parser exits immediately, so there can be only 1 parser error.
		// In the silent mode however, the parser continues to run, even though it would have stopped. Some
		// of the parser logic does not expect that, and may loop forever.
		// The p.next() here is needed, so the parser is more robust, and *always* advances, even in the -silent mode.
		p.next()
	}
}

pub fn (mut p Parser) warn_with_pos(s string, pos token.Position) {
	if p.pref.warns_are_errors {
		p.error_with_pos(s, pos)
		return
	}
	if p.pref.skip_warnings {
		return
	}
	if p.pref.output_mode == .stdout {
		ferror := util.formatted_error('warning:', s, p.file_name, pos)
		eprintln(ferror)
	} else {
		p.warnings << errors.Warning{
			file_path: p.file_name
			pos: pos
			reporter: .parser
			message: s
		}
	}
}

pub fn (mut p Parser) vet_error(s string, line int) {
	p.vet_errors << '$p.scanner.file_path:${line + 1}: $s'
}

fn (mut p Parser) parse_multi_expr(is_top_level bool) ast.Stmt {
	// in here might be 1) multi-expr 2) multi-assign
	// 1, a, c ... }       // multi-expression
	// a, mut b ... :=/=   // multi-assign
	// collect things upto hard boundaries
	tok := p.tok
	left, left_comments := p.expr_list()
	left0 := left[0]
	if tok.kind == .key_mut && p.tok.kind != .decl_assign {
		p.error('expecting `:=` (e.g. `mut x :=`)')
	}
	if p.tok.kind in [.assign, .decl_assign] || p.tok.kind.is_assign() {
		return p.partial_assign_stmt(left, left_comments)
	} else if tok.kind !in [.key_if, .key_match, .key_lock, .key_rlock, .key_select] &&
		left0 !is ast.CallExpr && (is_top_level || p.tok.kind != .rcbr) && left0 !is ast.PostfixExpr &&
		!(left0 is ast.InfixExpr && (left0 as ast.InfixExpr).op in [.left_shift, .arrow]) && left0 !is
		ast.ComptimeCall {
		p.error_with_pos('expression evaluated but not used', left0.position())
	}
	if left.len == 1 {
		return ast.ExprStmt{
			expr: left0
			pos: tok.position()
			comments: left_comments
			is_expr: p.inside_for
		}
	}
	return ast.ExprStmt{
		expr: ast.ConcatExpr{
			vals: left
			pos: tok.position()
		}
		pos: tok.position()
		comments: left_comments
	}
}

pub fn (mut p Parser) parse_ident(language table.Language) ast.Ident {
	// p.warn('name ')
	is_shared := p.tok.kind == .key_shared
	is_atomic := p.tok.kind == .key_atomic
	mut_pos := p.tok.position()
	is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
	if is_mut {
		p.next()
	}
	is_static := p.tok.kind == .key_static
	if is_static {
		p.next()
	}
	if p.tok.kind == .name {
		pos := p.tok.position()
		mut name := p.check_name()
		if name == '_' {
			return ast.Ident{
				tok_kind: p.tok.kind
				name: '_'
				kind: .blank_ident
				pos: pos
				info: ast.IdentVar{
					is_mut: false
					is_static: false
				}
			}
		}
		if p.inside_match_body && name == 'it' {
			// p.warn('it')
		}
		if p.expr_mod.len > 0 {
			name = '${p.expr_mod}.$name'
		}
		return ast.Ident{
			tok_kind: p.tok.kind
			kind: .unresolved
			name: name
			language: language
			mod: p.mod
			pos: pos
			is_mut: is_mut
			mut_pos: mut_pos
			info: ast.IdentVar{
				is_mut: is_mut
				is_static: is_static
				share: table.sharetype_from_flags(is_shared, is_atomic)
			}
		}
	} else {
		p.error('unexpected token `$p.tok.lit`')
	}
	return ast.Ident{}
}

pub fn (mut p Parser) name_expr() ast.Expr {
	prev_tok_kind := p.prev_tok.kind
	mut node := ast.Expr{}
	if p.expecting_type {
		p.expecting_type = false
		// get type position before moving to next
		type_pos := p.tok.position()
		typ := p.parse_type()
		return ast.Type{
			typ: typ
			pos: type_pos
		}
	}
	language := if p.tok.lit == 'C' {
		table.Language.c
	} else if p.tok.lit == 'JS' {
		table.Language.js
	} else {
		table.Language.v
	}
	mut mod := ''
	// p.warn('resetting')
	p.expr_mod = ''
	// `map[string]int` initialization
	if p.tok.lit == 'map' && p.peek_tok.kind == .lsbr {
		map_type := p.parse_map_type()
		if p.tok.kind == .lcbr && p.peek_tok.kind == .rcbr {
			p.next()
			p.next()
		}
		return ast.MapInit{
			typ: map_type
			pos: p.tok.position()
		}
	}
	// `chan typ{...}`
	if p.tok.lit == 'chan' {
		first_pos := p.tok.position()
		mut last_pos := first_pos
		chan_type := p.parse_chan_type()
		mut has_cap := false
		mut cap_expr := ast.Expr{}
		p.check(.lcbr)
		if p.tok.kind == .rcbr {
			last_pos = p.tok.position()
			p.next()
		} else {
			key := p.check_name()
			p.check(.colon)
			match key {
				'cap' {
					has_cap = true
					cap_expr = p.expr(0)
				}
				'len', 'init' {
					p.error('`$key` cannot be initialized for `chan`. Did you mean `cap`?')
				}
				else {
					p.error('wrong field `$key`, expecting `cap`')
				}
			}
			last_pos = p.tok.position()
			p.check(.rcbr)
		}
		return ast.ChanInit{
			pos: first_pos.extend(last_pos)
			has_cap: has_cap
			cap_expr: cap_expr
			typ: chan_type
		}
	}
	// Raw string (`s := r'hello \n ')
	if p.tok.lit in ['r', 'c', 'js'] && p.peek_tok.kind == .string && !p.inside_str_interp {
		return p.string_expr()
	}
	// don't allow r`byte` and c`byte`
	if p.tok.lit in ['r', 'c'] && p.peek_tok.kind == .chartoken {
		opt := if p.tok.lit == 'r' { '`r` (raw string)' } else { '`c` (c string)' }
		p.error('cannot use $opt with `byte` and `rune`')
	}
	known_var := p.mark_var_as_used(p.tok.lit)
	mut is_mod_cast := false
	if p.peek_tok.kind == .dot && !known_var &&
		(language != .v || p.known_import(p.tok.lit) || p.mod.all_after_last('.') == p.tok.lit) {
		// p.tok.lit has been recognized as a module
		if language == .c {
			mod = 'C'
		} else if language == .js {
			mod = 'JS'
		} else {
			if p.tok.lit in p.imports {
				// mark the imported module as used
				p.register_used_import(p.tok.lit)
				if p.peek_tok.kind == .dot && p.peek_tok2.lit[0].is_capital() {
					is_mod_cast = true
				}
			}
			// prepend the full import
			mod = p.imports[p.tok.lit]
		}
		p.next()
		p.check(.dot)
		p.expr_mod = mod
	}
	lit0_is_capital := p.tok.lit[0].is_capital()
	// use heuristics to detect `func<T>()` from `var < expr`
	is_generic_call := !lit0_is_capital && p.peek_tok.kind == .lt && (match p.peek_tok2.kind {
		.name {
			// maybe `f<int>`, `f<map[`
			(p.peek_tok2.kind == .name &&
				p.peek_tok3.kind == .gt) ||
				(p.peek_tok2.lit == 'map' && p.peek_tok3.kind == .lsbr)
		}
		.lsbr {
			// maybe `f<[]T>`, assume `var < []` is invalid
			p.peek_tok3.kind == .rsbr
		}
		else {
			false
		}
	})
	// p.warn('name expr  $p.tok.lit $p.peek_tok.str()')
	same_line := p.tok.line_nr == p.peek_tok.line_nr
	// `(` must be on same line as name token otherwise it's a ParExpr
	if !same_line && p.peek_tok.kind == .lpar {
		node = p.parse_ident(language)
	} else if p.peek_tok.kind == .lpar || is_generic_call {
		// foo(), foo<int>() or type() cast
		mut name := p.tok.lit
		if mod.len > 0 {
			name = '${mod}.$name'
		}
		name_w_mod := p.prepend_mod(name)
		// type cast. TODO: finish
		// if name in table.builtin_type_names {
		if (!known_var && (name in p.table.type_idxs ||
			name_w_mod in p.table.type_idxs) && name !in ['C.stat', 'C.sigaction']) ||
			is_mod_cast || (language == .v && name[0].is_capital()) {
			// MainLetter(x) is *always* a cast, as long as it is not `C.`
			// TODO handle C.stat()
			start_pos := p.tok.position()
			mut to_typ := p.parse_type()
			if p.is_amp {
				// Handle `&Foo(0)`
				to_typ = to_typ.to_ptr()
			}
			// this prevents inner casts to also have an `&`
			// example: &Foo(malloc(int(num)))
			// without the next line int would result in int*
			p.is_amp = false
			p.check(.lpar)
			mut expr := ast.Expr{}
			mut arg := ast.Expr{}
			mut has_arg := false
			expr = p.expr(0)
			// TODO, string(b, len)
			if p.tok.kind == .comma && to_typ.idx() == table.string_type_idx {
				p.next()
				arg = p.expr(0) // len
				has_arg = true
			}
			end_pos := p.tok.position()
			p.check(.rpar)
			node = ast.CastExpr{
				typ: to_typ
				expr: expr
				arg: arg
				has_arg: has_arg
				pos: start_pos.extend(end_pos)
			}
			p.expr_mod = ''
			return node
		} else {
			// fn call
			// println('calling $p.tok.lit')
			node = p.call_expr(language, mod)
		}
	} else if (p.peek_tok.kind == .lcbr ||
		(p.peek_tok.kind == .lt && lit0_is_capital)) &&
		(!p.inside_match || (p.inside_select && prev_tok_kind == .arrow && lit0_is_capital)) && !p.inside_match_case &&
		(!p.inside_if || p.inside_select) &&
		(!p.inside_for || p.inside_select) { // && (p.tok.lit[0].is_capital() || p.builtin_mod) {
		return p.struct_init(false) // short_syntax: false
	} else if p.peek_tok.kind == .dot && (lit0_is_capital && !known_var && language == .v) {
		// T.name
		if p.tok.lit == 'T' {
			pos := p.tok.position()
			name := p.check_name()
			p.check(.dot)
			field := p.check_name()
			pos.extend(p.tok.position())
			return ast.SelectorExpr{
				expr: ast.Ident{
					name: name
				}
				field_name: field
				pos: pos
			}
		}
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
	} else if p.peek_tok.kind == .colon && p.prev_tok.kind != .str_dollar {
		// `foo(key:val, key2:val2)`
		return p.struct_init(true) // short_syntax:true
		// JS. function call with more than 1 dot
	} else if language == .js && p.peek_tok.kind == .dot && p.peek_tok2.kind == .name {
		node = p.call_expr(language, mod)
	} else {
		node = p.parse_ident(language)
	}
	p.expr_mod = ''
	return node
}

fn (mut p Parser) index_expr(left ast.Expr) ast.IndexExpr {
	// left == `a` in `a[0]`
	start_pos := p.tok.position()
	p.next() // [
	mut has_low := true
	if p.tok.kind == .dotdot {
		has_low = false
		// [..end]
		p.next()
		high := p.expr(0)
		pos := start_pos.extend(p.tok.position())
		p.check(.rsbr)
		return ast.IndexExpr{
			left: left
			pos: pos
			index: ast.RangeExpr{
				low: ast.Expr{}
				high: high
				has_high: true
				pos: pos
			}
		}
	}
	expr := p.expr(0) // `[expr]` or  `[expr..`
	mut has_high := false
	if p.tok.kind == .dotdot {
		// [start..end] or [start..]
		p.next()
		mut high := ast.Expr{}
		if p.tok.kind != .rsbr {
			has_high = true
			high = p.expr(0)
		}
		pos := start_pos.extend(p.tok.position())
		p.check(.rsbr)
		return ast.IndexExpr{
			left: left
			pos: pos
			index: ast.RangeExpr{
				low: expr
				high: high
				has_high: has_high
				has_low: has_low
				pos: pos
			}
		}
	}
	// [expr]
	pos := start_pos.extend(p.tok.position())
	p.check(.rsbr)
	return ast.IndexExpr{
		left: left
		index: expr
		pos: pos
	}
}

fn (mut p Parser) scope_register_it() {
	p.scope.register('it', ast.Var{
		name: 'it'
		pos: p.tok.position()
		is_used: true
	})
}

fn (mut p Parser) scope_register_ab() {
	p.scope.register('a', ast.Var{
		name: 'a'
		pos: p.tok.position()
		is_used: true
	})
	p.scope.register('b', ast.Var{
		name: 'b'
		pos: p.tok.position()
		is_used: true
	})
}

fn (mut p Parser) dot_expr(left ast.Expr) ast.Expr {
	p.next()
	if p.tok.kind == .dollar {
		return p.comptime_method_call(left)
	}
	name_pos := p.tok.position()
	field_name := p.check_name()
	is_filter := field_name in ['filter', 'map']
	if is_filter {
		p.open_scope()
	} else if field_name == 'sort' {
		p.open_scope()
	}
	// ! in mutable methods
	if p.tok.kind == .not && p.peek_tok.kind == .lpar {
		p.next()
	}
	// Method call
	// TODO move to fn.v call_expr()
	if p.tok.kind == .lpar {
		p.next()
		args := p.call_args()
		if is_filter && args.len != 1 {
			p.error('needs exactly 1 argument')
		}
		p.check(.rpar)
		mut or_stmts := []ast.Stmt{}
		mut or_kind := ast.OrKind.absent
		mut or_pos := p.tok.position()
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
			or_kind = .block
			or_stmts = p.parse_block_no_scope(false)
			or_pos = or_pos.extend(p.prev_tok.position())
			p.close_scope()
		}
		// `foo()?`
		if p.tok.kind == .question {
			p.next()
			or_kind = .propagate
		}
		//
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
				kind: or_kind
				pos: or_pos
			}
		}
		if is_filter || field_name == 'sort' {
			p.close_scope()
		}
		return mcall_expr
	}
	mut is_mut := false
	mut mut_pos := token.Position{}
	if p.inside_match || p.inside_if_expr {
		match left {
			ast.Ident, ast.SelectorExpr {
				is_mut = left.is_mut
				mut_pos = left.mut_pos
			}
			else {}
		}
	}
	sel_expr := ast.SelectorExpr{
		expr: left
		field_name: field_name
		pos: name_pos
		is_mut: is_mut
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
			language: if is_cstr { table.Language.c } else { table.Language.v }
			pos: pos
		}
		return node
	}
	mut exprs := []ast.Expr{}
	mut vals := []string{}
	mut has_fmts := []bool{}
	mut fwidths := []int{}
	mut precisions := []int{}
	mut visible_pluss := []bool{}
	mut fills := []bool{}
	mut fmts := []byte{}
	mut fposs := []token.Position{}
	// Handle $ interpolation
	p.inside_str_interp = true
	for p.tok.kind == .string {
		vals << p.tok.lit
		p.next()
		if p.tok.kind != .str_dollar {
			break
		}
		p.next()
		exprs << p.expr(0)
		mut has_fmt := false
		mut fwidth := 0
		mut fwidthneg := false
		// 987698 is a magic default value, unlikely to be present in user input. NB: 0 is valid precision
		mut precision := 987698
		mut visible_plus := false
		mut fill := false
		mut fmt := `_` // placeholder
		if p.tok.kind == .colon {
			p.next()
			// ${num:-2d}
			if p.tok.kind == .minus {
				fwidthneg = true
				p.next()
			} else if p.tok.kind == .plus {
				visible_plus = true
				p.next()
			}
			// ${num:2d}
			if p.tok.kind == .number {
				fields := p.tok.lit.split('.')
				if fields[0].len > 0 && fields[0][0] == `0` {
					fill = true
				}
				fwidth = fields[0].int()
				if fwidthneg {
					fwidth = -fwidth
				}
				if fields.len > 1 {
					precision = fields[1].int()
				}
				p.next()
			}
			if p.tok.kind == .name {
				if p.tok.lit.len == 1 {
					fmt = p.tok.lit[0]
					has_fmt = true
					p.next()
				} else {
					p.error('format specifier may only be one letter')
				}
			}
		}
		fwidths << fwidth
		has_fmts << has_fmt
		precisions << precision
		visible_pluss << visible_plus
		fmts << fmt
		fills << fill
		fposs << p.prev_tok.position()
	}
	node = ast.StringInterLiteral{
		vals: vals
		exprs: exprs
		need_fmts: has_fmts
		fwidths: fwidths
		precisions: precisions
		pluss: visible_pluss
		fills: fills
		fmts: fmts
		fmt_poss: fposs
		pos: pos
	}
	// need_fmts: prelimery - until checker finds out if really needed
	p.inside_str_interp = false
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
	mut module_pos := token.Position{}
	if !is_skipped {
		module_pos = p.tok.position()
		p.next()
		mut pos := p.tok.position()
		name = p.check_name()
		if module_pos.line_nr != pos.line_nr {
			p.error_with_pos('`module` and `$name` must be at same line', pos)
		}
		pos = p.tok.position()
		if module_pos.line_nr == pos.line_nr && p.tok.kind != .comment {
			if p.tok.kind != .name {
				p.error_with_pos('`module x` syntax error', pos)
			} else {
				p.error_with_pos('`module x` can only declare one module', pos)
			}
		}
		module_pos = module_pos.extend(pos)
	}
	mut full_mod := p.table.qualify_module(name, p.file_name)
	if p.pref.build_mode == .build_module && !full_mod.contains('.') {
		// A hack to make building vlib modules work
		// `v build-module v.gen` will result in `full_mod = "gen"`, not "v.gen",
		// because the module being built
		// is not imported.
		// So here we fetch the name of the module by looking at the path that's being built.
		word := p.pref.path.after('/')
		if full_mod == word {
			full_mod = p.pref.path.after('vlib/').replace('/', '.')
			// println('new full mod =$full_mod')
		}
		// println('file_name=$p.file_name path=$p.pref.path')
	}
	p.mod = full_mod
	p.builtin_mod = p.mod == 'builtin'
	return ast.Module{
		name: full_mod
		is_skipped: is_skipped
		pos: module_pos
	}
}

fn (mut p Parser) import_stmt() ast.Import {
	import_pos := p.tok.position()
	p.check(.key_import)
	pos := p.tok.position()
	if p.tok.kind == .lpar {
		p.error_with_pos('`import()` has been deprecated, use `import x` instead', pos)
	}
	mut mod_name := p.check_name()
	if import_pos.line_nr != pos.line_nr {
		p.error_with_pos('`import` statements must be a single line', pos)
	}
	mut mod_alias := mod_name
	for p.tok.kind == .dot {
		p.next()
		pos_t := p.tok.position()
		if p.tok.kind != .name {
			p.error_with_pos('module syntax error, please use `x.y.z`', pos)
		}
		if import_pos.line_nr != pos_t.line_nr {
			p.error_with_pos('`import` and `submodule` must be at same line', pos)
		}
		submod_name := p.check_name()
		mod_name += '.' + submod_name
		mod_alias = submod_name
	}
	if p.tok.kind == .key_as {
		p.next()
		mod_alias = p.check_name()
		if mod_alias == mod_name.split('.').last() {
			p.error_with_pos('import alias `$mod_name as $mod_alias` is redundant', p.prev_tok.position())
		}
	}
	mut node := ast.Import{
		pos: pos
		mod: mod_name
		alias: mod_alias
	}
	if p.tok.kind == .lcbr { // import module { fn1, Type2 } syntax
		p.import_syms(mut node)
		p.register_used_import(mod_name) // no `unused import` msg for parent
	}
	pos_t := p.tok.position()
	if import_pos.line_nr == pos_t.line_nr {
		if p.tok.kind !in [.lcbr, .eof] {
			p.error_with_pos('cannot import multiple modules at a time', pos_t)
		}
	}
	p.imports[mod_alias] = mod_name
	// if mod_name !in p.table.imports {
	p.table.imports << mod_name
	p.ast_imports << node
	// }
	return node
}

// import_syms parses the inner part of `import module { submod1, submod2 }`
fn (mut p Parser) import_syms(mut parent ast.Import) {
	p.next()
	pos_t := p.tok.position()
	if p.tok.kind == .rcbr { // closed too early
		p.error_with_pos('empty `$parent.mod` import set, remove `{}`', pos_t)
	}
	if p.tok.kind != .name { // not a valid inner name
		p.error_with_pos('import syntax error, please specify a valid fn or type name',
			pos_t)
	}
	for p.tok.kind == .name {
		pos := p.tok.position()
		alias := p.check_name()
		name := '${parent.mod}.$alias'
		if alias[0].is_capital() {
			idx := p.table.add_placeholder_type(name, .v)
			typ := table.new_type(idx)
			prepend_mod_name := p.prepend_mod(alias)
			p.table.register_type_symbol(table.TypeSymbol{
				kind: .alias
				name: prepend_mod_name
				source_name: prepend_mod_name
				mod: p.mod
				parent_idx: idx
				info: table.Alias{
					parent_type: typ
					language: table.Language.v
					is_import: true
				}
				is_public: false
			})
			// so we can work with the fully declared type in fmt+checker
			parent.syms << ast.ImportSymbol{
				pos: pos
				name: alias
				kind: .type_
			}
		} else {
			if !p.table.known_fn(name) {
				p.table.fns[alias] = table.Fn{
					is_placeholder: true
					mod: parent.mod
					name: name
				}
			}
			// so we can work with this in fmt+checker
			parent.syms << ast.ImportSymbol{
				pos: pos
				name: alias
				kind: .fn_
			}
		}
		if p.tok.kind == .comma { // go again if more than one
			p.next()
			continue
		}
		if p.tok.kind == .rcbr { // finish if closing `}` is seen
			break
		}
	}
	if p.tok.kind != .rcbr {
		p.error_with_pos('import syntax error, no closing `}`', p.tok.position())
	}
	p.next()
}

fn (mut p Parser) const_decl() ast.ConstDecl {
	p.top_level_statement_start()
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
	mut comments := []ast.Comment{}
	for {
		comments = p.eat_comments()
		if p.tok.kind == .rpar {
			break
		}
		pos := p.tok.position()
		name := p.check_name()
		if util.contains_capital(name) {
			p.warn_with_pos('const names cannot contain uppercase letters, use snake_case instead',
				pos)
		}
		full_name := p.prepend_mod(name)
		// name := p.check_name()
		// println('!!const: $name')
		p.check(.assign)
		if p.tok.kind == .key_fn {
			p.error('const initializer fn literal is not a constant')
		}
		expr := p.expr(0)
		field := ast.ConstField{
			name: full_name
			mod: p.mod
			expr: expr
			pos: pos
			comments: comments
		}
		fields << field
		p.global_scope.register(field.name, field)
		comments = []
	}
	p.top_level_statement_end()
	p.check(.rpar)
	return ast.ConstDecl{
		pos: start_pos.extend(end_pos)
		fields: fields
		is_pub: is_pub
		end_comments: comments
	}
}

fn (mut p Parser) return_stmt() ast.Return {
	first_pos := p.tok.position()
	p.next()
	// no return
	mut comments := p.eat_comments()
	if p.tok.kind == .rcbr {
		return ast.Return{
			comments: comments
			pos: first_pos
		}
	}
	// return exprs
	exprs, comments2 := p.expr_list()
	comments << comments2
	end_pos := exprs.last().position()
	return ast.Return{
		exprs: exprs
		comments: comments
		pos: first_pos.extend(end_pos)
	}
}

const (
	// modules which allow globals by default
	global_enabled_mods = ['rand', 'sokol.sapp']
)

// left hand side of `=` or `:=` in `a,b,c := 1,2,3`
fn (mut p Parser) global_decl() ast.GlobalDecl {
	if !p.pref.translated && !p.pref.is_livemain && !p.builtin_mod && !p.pref.building_v &&
		p.mod != 'ui' && p.mod != 'gg2' && p.mod != 'uiold' && !p.pref.enable_globals && !p.pref.is_fmt &&
		p.mod !in global_enabled_mods {
		p.error('use `v --enable-globals ...` to enable globals')
	}
	start_pos := p.tok.position()
	end_pos := p.tok.position()
	p.check(.key_global)
	if p.tok.kind != .lpar {
		p.error('globals must be grouped, e.g. `__global ( a = int(1) )`')
	}
	p.next() // (
	mut fields := []ast.GlobalField{}
	mut comments := []ast.Comment{}
	for {
		comments = p.eat_comments()
		if p.tok.kind == .rpar {
			break
		}
		pos := p.tok.position()
		name := p.check_name()
		has_expr := p.tok.kind == .assign
		if has_expr {
			p.next() // =
		}
		typ := p.parse_type()
		if p.tok.kind == .assign {
			p.error('global assign must have the type around the value, use `__global ( name = type(value) )`')
		}
		mut expr := ast.Expr{}
		if has_expr {
			if p.tok.kind != .lpar {
				p.error('global assign must have a type and value, use `__global ( name = type(value) )` or `__global ( name type )`')
			}
			p.next() // (
			expr = p.expr(0)
			p.check(.rpar)
		}
		field := ast.GlobalField{
			name: name
			has_expr: has_expr
			expr: expr
			pos: pos
			typ: typ
			comments: comments
		}
		fields << field
		p.global_scope.register(field.name, field)
		comments = []
	}
	p.check(.rpar)
	return ast.GlobalDecl{
		pos: start_pos.extend(end_pos)
		fields: fields
		end_comments: comments
	}
}

fn (mut p Parser) enum_decl() ast.EnumDecl {
	p.top_level_statement_start()
	is_pub := p.tok.kind == .key_pub
	start_pos := p.tok.position()
	if is_pub {
		p.next()
	}
	p.check(.key_enum)
	end_pos := p.tok.position()
	enum_name := p.check_name()
	if enum_name.len == 1 {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			end_pos)
	}
	name := p.prepend_mod(enum_name)
	p.check(.lcbr)
	enum_decl_comments := p.eat_comments()
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
			comments: p.eat_comments()
		}
	}
	p.top_level_statement_end()
	p.check(.rcbr)
	is_flag := p.attrs.contains('flag')
	is_multi_allowed := p.attrs.contains('_allow_multiple_values')
	if is_flag {
		if fields.len > 32 {
			p.error('when an enum is used as bit field, it must have a max of 32 fields')
		}
		for f in fields {
			if f.has_expr {
				p.error_with_pos('when an enum is used as a bit field, you can not assign custom values',
					f.pos)
			}
		}
		pubfn := if p.mod == 'main' { 'fn' } else { 'pub fn' }
		p.scanner.codegen('
//
$pubfn (    e &$enum_name) has(flag $enum_name) bool { return      (int(*e) &  (int(flag))) != 0 }
$pubfn (mut e  $enum_name) set(flag $enum_name)      { unsafe{ *e = int(*e) |  (int(flag)) } }
$pubfn (mut e  $enum_name) clear(flag $enum_name)    { unsafe{ *e = int(*e) & ~(int(flag)) } }
$pubfn (mut e  $enum_name) toggle(flag $enum_name)   { unsafe{ *e = int(*e) ^  (int(flag)) } }
//
')
	}
	p.table.register_type_symbol(table.TypeSymbol{
		kind: .enum_
		name: name
		source_name: name
		mod: p.mod
		info: table.Enum{
			vals: vals
			is_flag: is_flag
			is_multi_allowed: is_multi_allowed
		}
	})
	return ast.EnumDecl{
		name: name
		is_pub: is_pub
		is_flag: is_flag
		is_multi_allowed: is_multi_allowed
		fields: fields
		pos: start_pos.extend(end_pos)
		attrs: p.attrs
		comments: enum_decl_comments
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
	if name.len == 1 && name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			decl_pos)
	}
	mut sum_variants := []table.Type{}
	p.check(.assign)
	mut comments := []ast.Comment{}
	if p.tok.kind == .key_fn {
		// function type: `type mycallback fn(string, int)`
		fn_name := p.prepend_mod(name)
		fn_type := p.parse_fn_type(fn_name)
		comments = p.eat_lineend_comments()
		return ast.FnTypeDecl{
			name: fn_name
			is_pub: is_pub
			typ: fn_type
			pos: decl_pos
			comments: comments
		}
	}
	first_type := p.parse_type() // need to parse the first type before we can check if it's `type A = X | Y`
	if p.tok.kind == .pipe {
		p.next()
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
		prepend_mod_name := p.prepend_mod(name)
		p.table.register_type_symbol(table.TypeSymbol{
			kind: .sum_type
			name: prepend_mod_name
			source_name: prepend_mod_name
			mod: p.mod
			info: table.SumType{
				variants: sum_variants
			}
			is_public: is_pub
		})
		comments = p.eat_lineend_comments()
		return ast.SumTypeDecl{
			name: name
			is_pub: is_pub
			sub_types: sum_variants
			pos: decl_pos
			comments: comments
		}
	}
	// type MyType int
	parent_type := first_type
	parent_name := p.table.get_type_symbol(parent_type).name
	pid := parent_type.idx()
	language := if parent_name.len > 2 && parent_name.starts_with('C.') {
		table.Language.c
	} else if parent_name.len > 2 && parent_name.starts_with('JS.') {
		table.Language.js
	} else {
		table.Language.v
	}
	prepend_mod_name := p.prepend_mod(name)
	p.table.register_type_symbol(table.TypeSymbol{
		kind: .alias
		name: prepend_mod_name
		source_name: prepend_mod_name
		mod: p.mod
		parent_idx: pid
		info: table.Alias{
			parent_type: parent_type
			language: language
		}
		is_public: is_pub
	})
	comments = p.eat_lineend_comments()
	return ast.AliasTypeDecl{
		name: name
		is_pub: is_pub
		parent_type: parent_type
		pos: decl_pos
		comments: comments
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
			p.next()
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

fn (mut p Parser) top_level_statement_start() {
	if p.comments_mode == .toplevel_comments {
		p.scanner.set_is_inside_toplevel_statement(true)
		p.rewind_scanner_to_current_token_in_new_mode()
		$if debugscanner ? {
			eprintln('>> p.top_level_statement_start | tidx:${p.tok.tidx:-5} | p.tok.kind: ${p.tok.kind:-10} | p.tok.lit: $p.tok.lit $p.peek_tok.lit $p.peek_tok2.lit $p.peek_tok3.lit ...')
		}
	}
}

fn (mut p Parser) top_level_statement_end() {
	if p.comments_mode == .toplevel_comments {
		p.scanner.set_is_inside_toplevel_statement(false)
		p.rewind_scanner_to_current_token_in_new_mode()
		$if debugscanner ? {
			eprintln('>> p.top_level_statement_end   | tidx:${p.tok.tidx:-5} | p.tok.kind: ${p.tok.kind:-10} | p.tok.lit: $p.tok.lit $p.peek_tok.lit $p.peek_tok2.lit $p.peek_tok3.lit ...')
		}
	}
}

fn (mut p Parser) rewind_scanner_to_current_token_in_new_mode() {
	// Go back and rescan some tokens, ensuring that the parser's
	// lookahead buffer p.peek_tok .. p.peek_tok3, will now contain
	// the correct tokens (possible comments), for the new mode
	// This refilling of the lookahead buffer is needed for the
	// .toplevel_comments parsing mode.
	tidx := p.tok.tidx
	p.scanner.set_current_tidx(tidx - 5)
	no_token := token.Token{}
	p.prev_tok = no_token
	p.tok = no_token
	p.peek_tok = no_token
	p.peek_tok2 = no_token
	p.peek_tok3 = no_token
	for {
		p.next()
		// eprintln('rewinding to ${p.tok.tidx:5} | goal: ${tidx:5}')
		if tidx == p.tok.tidx {
			break
		}
	}
}

pub fn (mut p Parser) mark_var_as_used(varname string) bool {
	if obj := p.scope.find(varname) {
		match mut obj {
			ast.Var {
				obj.is_used = true
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut p Parser) unsafe_stmt() ast.Stmt {
	pos := p.tok.position()
	p.next()
	if p.tok.kind != .lcbr {
		p.error_with_pos('please use `unsafe {`', p.tok.position())
	}
	p.next()
	if p.inside_unsafe {
		p.error_with_pos('already inside `unsafe` block', pos)
	}
	if p.tok.kind == .rcbr {
		// `unsafe {}`
		p.next()
		return ast.Block{
			is_unsafe: true
			pos: pos
		}
	}
	p.inside_unsafe = true
	p.open_scope() // needed in case of `unsafe {stmt}`
	defer {
		p.inside_unsafe = false
		p.close_scope()
	}
	stmt := p.stmt(false)
	if p.tok.kind == .rcbr {
		if stmt is ast.ExprStmt {
			// `unsafe {expr}`
			if stmt.expr.is_expr() {
				p.next()
				ue := ast.UnsafeExpr{
					expr: stmt.expr
					pos: pos
				}
				// parse e.g. `unsafe {expr}.foo()`
				expr := p.expr_with_left(ue, 0, p.is_stmt_ident)
				return ast.ExprStmt{
					expr: expr
					pos: pos
				}
			}
		}
	}
	// unsafe {stmts}
	mut stmts := [stmt]
	for p.tok.kind != .rcbr {
		stmts << p.stmt(false)
	}
	p.next()
	return ast.Block{
		stmts: stmts
		is_unsafe: true
		pos: pos
	}
}

fn (mut p Parser) trace(fbase string, message string) {
	if p.file_base == fbase {
		println('> p.trace | ${fbase:-10s} | $message')
	}
}
