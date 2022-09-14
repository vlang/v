// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module parser

import v.scanner
import v.ast
import v.token
import v.pref
import v.util
import v.vet
import v.errors
import os
import hash.fnv1a

[minify]
pub struct Parser {
	pref &pref.Preferences = unsafe { nil }
mut:
	file_base         string       // "hello.v"
	file_name         string       // "/home/user/hello.v"
	file_name_dir     string       // "/home/user"
	unique_prefix     string       // a hash of p.file_name, used for making anon fn generation unique
	file_backend_mode ast.Language // .c for .c.v|.c.vv|.c.vsh files; .js for .js.v files, .amd64/.rv32/other arches for .amd64.v/.rv32.v/etc. files, .v otherwise.
	comments_mode     scanner.CommentsMode = .skip_comments
	// see comment in parse_file
	tok                       token.Token
	prev_tok                  token.Token
	peek_tok                  token.Token
	table                     &ast.Table = unsafe { nil }
	language                  ast.Language
	fn_language               ast.Language // .c for `fn C.abcd()` declarations
	expr_level                int  // prevent too deep recursions for pathological programs
	inside_vlib_file          bool // true for all vlib/ files
	inside_test_file          bool // when inside _test.v or _test.vv file
	inside_if                 bool
	inside_comptime_if        bool
	inside_if_expr            bool
	inside_if_cond            bool
	inside_ct_if_expr         bool
	inside_or_expr            bool
	inside_for                bool
	inside_fn                 bool // true even with implicit main
	inside_fn_return          bool
	inside_unsafe_fn          bool
	inside_str_interp         bool
	inside_array_lit          bool
	inside_in_array           bool
	inside_match              bool // to separate `match A { }` from `Struct{}`
	inside_select             bool // to allow `ch <- Struct{} {` inside `select`
	inside_match_case         bool // to separate `match_expr { }` from `Struct{}`
	inside_match_body         bool // to fix eval not used TODO
	inside_unsafe             bool
	inside_sum_type           bool // to prevent parsing inline sum type again
	inside_asm_template       bool
	inside_asm                bool
	inside_defer              bool
	inside_generic_params     bool // indicates if parsing between `<` and `>` of a method/function
	inside_receiver_param     bool // indicates if parsing the receiver parameter inside the first `(` and `)` of a method
	inside_struct_field_decl  bool
	or_is_handled             bool       // ignore `or` in this expression
	builtin_mod               bool       // are we in the `builtin` module?
	mod                       string     // current module name
	is_manualfree             bool       // true when `[manualfree] module abc`, makes *all* fns in the current .v file, opt out of autofree
	has_globals               bool       // `[has_globals] module abc` - allow globals declarations, even without -enable-globals, in that single .v file __only__
	is_generated              bool       // `[generated] module abc` - turn off compiler notices for that single .v file __only__.
	is_translated             bool       // `[translated] module abc` - mark a file as translated, to relax some compiler checks for translated code.
	attrs                     []ast.Attr // attributes before next decl stmt
	expr_mod                  string     // for constructing full type names in parse_type()
	scope                     &ast.Scope = unsafe { nil }
	imports                   map[string]string // alias => mod_name
	ast_imports               []ast.Import      // mod_names
	used_imports              []string // alias
	auto_imports              []string // imports, the user does not need to specify
	imported_symbols          map[string]string
	is_amp                    bool // for generating the right code for `&Foo{}`
	returns                   bool
	is_stmt_ident             bool // true while the beginning of a statement is an ident/selector
	expecting_type            bool // `is Type`, expecting type
	cur_fn_name               string
	label_names               []string
	name_error                bool // indicates if the token is not a name or the name is on another line
	n_asm                     int  // controls assembly labels
	global_labels             []string
	comptime_if_cond          bool
	defer_vars                []ast.Ident
	should_abort              bool // when too many errors/warnings/notices are accumulated, should_abort becomes true, and the parser should stop
	codegen_text              string
	struct_init_generic_types []ast.Type
	if_cond_comments          []ast.Comment
	script_mode               bool
	script_mode_start_token   token.Token
pub mut:
	scanner    &scanner.Scanner = unsafe { nil }
	errors     []errors.Error
	warnings   []errors.Warning
	notices    []errors.Notice
	vet_errors []vet.Error
}

__global codegen_files = unsafe { []&ast.File{} }

// for tests
pub fn parse_stmt(text string, table &ast.Table, scope &ast.Scope) ast.Stmt {
	$if trace_parse_stmt ? {
		eprintln('> ${@MOD}.${@FN} text: $text')
	}
	mut p := Parser{
		scanner: scanner.new_scanner(text, .skip_comments, &pref.Preferences{})
		inside_test_file: true
		table: table
		pref: &pref.Preferences{}
		scope: scope
	}
	p.init_parse_fns()
	util.timing_start('PARSE stmt')
	defer {
		util.timing_measure_cumulative('PARSE stmt')
	}
	p.read_first_token()
	return p.stmt(false)
}

pub fn parse_comptime(tmpl_path string, text string, table &ast.Table, pref &pref.Preferences, scope &ast.Scope) &ast.File {
	$if trace_parse_comptime ? {
		eprintln('> ${@MOD}.${@FN} text: $text')
	}
	mut p := Parser{
		file_name: tmpl_path
		scanner: scanner.new_scanner(text, .skip_comments, pref)
		table: table
		pref: pref
		scope: scope
		errors: []errors.Error{}
		warnings: []errors.Warning{}
	}
	res := p.parse()
	unsafe { p.free_scanner() }
	return res
}

pub fn parse_text(text string, path string, table &ast.Table, comments_mode scanner.CommentsMode, pref &pref.Preferences) &ast.File {
	$if trace_parse_text ? {
		eprintln('> ${@MOD}.${@FN} comments_mode: ${comments_mode:-20} | path: ${path:-20} | text: $text')
	}
	mut p := Parser{
		scanner: scanner.new_scanner(text, comments_mode, pref)
		comments_mode: comments_mode
		table: table
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: table.global_scope
		}
		errors: []errors.Error{}
		warnings: []errors.Warning{}
	}
	p.set_path(path)
	res := p.parse()
	unsafe { p.free_scanner() }
	return res
}

[unsafe]
pub fn (mut p Parser) free() {
	unsafe { p.free_scanner() }
}

[unsafe]
pub fn (mut p Parser) free_scanner() {
	unsafe {
		if p.scanner != 0 {
			p.scanner.free()
			p.scanner = &scanner.Scanner(0)
		}
	}
}

pub fn (mut p Parser) set_path(path string) {
	p.file_name = path
	p.file_base = os.base(path)
	p.file_name_dir = os.dir(path)
	p.inside_vlib_file = p.file_name_dir.contains('vlib')
	p.inside_test_file = p.file_base.ends_with('_test.v') || p.file_base.ends_with('_test.vv')
		|| p.file_base.all_before_last('.v').all_before_last('.').ends_with('_test')

	hash := fnv1a.sum64_string(path)
	p.unique_prefix = hash.hex_full()

	p.file_backend_mode = .v
	before_dot_v := path.all_before_last('.v') // also works for .vv and .vsh
	language := before_dot_v.all_after_last('.')
	language_with_underscore := before_dot_v.all_after_last('_')
	if language == before_dot_v && language_with_underscore == before_dot_v {
		return
	}
	actual_language := if language == before_dot_v { language_with_underscore } else { language }
	match actual_language {
		'c' {
			p.file_backend_mode = .c
		}
		'js' {
			p.file_backend_mode = .js
		}
		else {
			arch := pref.arch_from_string(actual_language) or { pref.Arch._auto }
			p.file_backend_mode = ast.pref_arch_to_table_language(arch)
			if arch == ._auto {
				p.file_backend_mode = .v
			}
		}
	}
}

pub fn parse_file(path string, table &ast.Table, comments_mode scanner.CommentsMode, pref &pref.Preferences) &ast.File {
	// Note: when comments_mode == .toplevel_comments,
	// the parser gives feedback to the scanner about toplevel statements, so that the scanner can skip
	// all the tricky inner comments. This is needed because we do not have a good general solution
	// for handling them, and should be removed when we do (the general solution is also needed for vfmt)
	$if trace_parse_file ? {
		eprintln('> ${@MOD}.${@FN} comments_mode: ${comments_mode:-20} | path: $path')
	}
	mut p := Parser{
		scanner: scanner.new_scanner_file(path, comments_mode, pref) or { panic(err) }
		comments_mode: comments_mode
		table: table
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: table.global_scope
		}
		errors: []errors.Error{}
		warnings: []errors.Warning{}
	}
	p.set_path(path)
	res := p.parse()
	unsafe { p.free_scanner() }
	return res
}

pub fn parse_vet_file(path string, table_ &ast.Table, pref &pref.Preferences) (&ast.File, []vet.Error) {
	$if trace_parse_vet_file ? {
		eprintln('> ${@MOD}.${@FN} path: $path')
	}
	global_scope := &ast.Scope{
		parent: 0
	}
	mut p := Parser{
		scanner: scanner.new_scanner_file(path, .parse_comments, pref) or { panic(err) }
		comments_mode: .parse_comments
		table: table_
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: global_scope
		}
		errors: []errors.Error{}
		warnings: []errors.Warning{}
	}
	p.set_path(path)
	if p.scanner.text.contains_any_substr(['\n  ', ' \n']) {
		source_lines := os.read_lines(path) or { []string{} }
		for lnumber, line in source_lines {
			if line.starts_with('  ') {
				p.vet_error('Looks like you are using spaces for indentation.', lnumber,
					.vfmt, .space_indent)
			}
			if line.ends_with(' ') {
				p.vet_error('Looks like you have trailing whitespace.', lnumber, .unknown,
					.trailing_space)
			}
		}
	}
	p.vet_errors << p.scanner.vet_errors
	file := p.parse()
	unsafe { p.free_scanner() }
	return file, p.vet_errors
}

pub fn (mut p Parser) parse() &ast.File {
	util.timing_start('PARSE')
	defer {
		util.timing_measure_cumulative('PARSE')
	}
	// comments_mode: comments_mode
	p.init_parse_fns()
	p.read_first_token()
	mut stmts := []ast.Stmt{}
	for p.tok.kind == .comment {
		stmts << p.comment_stmt()
	}
	// module
	module_decl := p.module_decl()
	if module_decl.is_skipped {
		stmts.insert(0, ast.Stmt(module_decl))
	} else {
		stmts << module_decl
	}
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
		stmt := p.top_stmt()
		// clear the attributes after each statement
		if !(stmt is ast.ExprStmt && (stmt as ast.ExprStmt).expr is ast.Comment) {
			p.attrs = []
		}
		stmts << stmt
		if p.should_abort {
			break
		}
	}
	p.scope.end_pos = p.tok.pos

	mut errors := p.errors.clone()
	mut warnings := p.warnings.clone()
	mut notices := p.notices.clone()

	if p.pref.check_only {
		errors << p.scanner.errors
		warnings << p.scanner.warnings
		notices << p.scanner.notices
	}

	// codegen
	if p.codegen_text.len > 0 && !p.pref.is_fmt {
		ptext := 'module ' + p.mod.all_after_last('.') + p.codegen_text
		codegen_files << parse_text(ptext, p.file_name, p.table, p.comments_mode, p.pref)
	}

	return &ast.File{
		path: p.file_name
		path_base: p.file_base
		is_test: p.inside_test_file
		is_generated: p.is_generated
		is_translated: p.is_translated
		nr_lines: p.scanner.line_nr
		nr_bytes: p.scanner.text.len
		mod: module_decl
		imports: p.ast_imports
		imported_symbols: p.imported_symbols
		auto_imports: p.auto_imports
		stmts: stmts
		scope: p.scope
		global_scope: p.table.global_scope
		errors: errors
		warnings: warnings
		notices: notices
		global_labels: p.global_labels
	}
}

/*
struct Queue {
mut:
	idx              int
	mu               &sync.Mutex
	mu2              &sync.Mutex
	paths            []string
	table            &ast.Table = unsafe { nil }
	parsed_ast_files []&ast.File
	pref             &pref.Preferences = unsafe { nil }
	global_scope     &ast.Scope = unsafe { nil }
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
pub fn parse_files(paths []string, table &ast.Table, pref &pref.Preferences) []&ast.File {
	mut timers := util.new_timers(should_print: false, label: 'parse_files: $paths')
	$if time_parsing ? {
		timers.should_print = true
	}
	$if macos {
		/*
		if !pref.no_parallel && paths[0].contains('/array.v') {
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
			time.sleep(time.second)
			println('all done')
			return q.parsed_ast_files
		}
		*/
	}
	unsafe {
		mut files := []&ast.File{cap: paths.len}
		for path in paths {
			timers.start('parse_file $path')
			files << parse_file(path, table, .skip_comments, pref)
			timers.show('parse_file $path')
		}
		if codegen_files.len > 0 {
			files << codegen_files
			codegen_files.clear()
		}
		return files
	}
}

// codegen allows you to generate V code, so that it can be parsed,
// checked, markused, cgen-ed etc further, just like user's V code.
pub fn (mut p Parser) codegen(code string) {
	$if debug_codegen ? {
		eprintln('parser.codegen:\n $code')
	}
	p.codegen_text += '\n' + code
}

pub fn (mut p Parser) init_parse_fns() {
	// p.prefix_parse_fns = make(100, 100, sizeof(PrefixParseFn))
	// p.prefix_parse_fns[token.Kind.name] = parse_name
}

pub fn (mut p Parser) read_first_token() {
	// need to call next() 2 times to get peek token and current token
	p.next()
	p.next()
}

[inline]
pub fn (p &Parser) peek_token(n int) token.Token {
	return p.scanner.peek_token(n - 2)
}

// peek token in if guard `if x,y := opt()` after var_list `x,y`
fn (p &Parser) peek_token_after_var_list() token.Token {
	mut n := 0
	mut tok := p.tok
	for {
		if tok.kind == .key_mut {
			n += 2
		} else {
			n++
		}
		tok = p.scanner.peek_token(n - 2)
		if tok.kind != .comma {
			break
		} else {
			n++
			tok = p.scanner.peek_token(n - 2)
		}
	}
	return tok
}

// peek token `type Fn = fn () int`
fn (p &Parser) is_fn_type_decl() bool {
	mut n := 1
	mut tok := p.tok
	mut prev_tok := p.tok
	cur_ln := p.tok.line_nr
	for {
		tok = p.scanner.peek_token(n)
		if tok.kind in [.lpar, .rpar] {
			n++
			prev_tok = tok
			continue
		}
		if tok.kind == .pipe {
			if tok.pos - prev_tok.pos > prev_tok.len {
				return false
			}
		}
		if tok.line_nr > cur_ln {
			break
		}
		prev_tok = tok
		n++
	}
	return true
}

fn (p &Parser) is_array_type() bool {
	mut i := 1
	mut tok := p.tok
	line_nr := p.tok.line_nr

	for {
		tok = p.peek_token(i)
		if tok.line_nr != line_nr {
			return false
		}
		if tok.kind in [.name, .amp] {
			return true
		}
		i++
		if tok.kind == .lsbr || tok.kind != .rsbr {
			continue
		}
	}
	return false
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
	stmts := p.parse_block_no_scope(false)
	p.close_scope()
	return stmts
}

pub fn (mut p Parser) parse_block_no_scope(is_top_level bool) []ast.Stmt {
	p.check(.lcbr)
	mut stmts := []ast.Stmt{cap: 20}
	if p.tok.kind != .rcbr {
		mut count := 0
		for p.tok.kind !in [.eof, .rcbr] {
			stmts << p.stmt(is_top_level)
			count++
			if count % 100000 == 0 {
				eprintln('parsed $count statements so far from fn $p.cur_fn_name ...')
			}
			if count > 1000000 {
				p.error_with_pos('parsed over $count statements from fn $p.cur_fn_name, the parser is probably stuck',
					p.tok.pos())
				return []
			}
		}
	}
	if is_top_level {
		p.top_level_statement_end()
	}
	p.check(.rcbr)
	return stmts
}

fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
}

fn (mut p Parser) check(expected token.Kind) {
	p.name_error = false
	if _likely_(p.tok.kind == expected) {
		p.next()
	} else {
		if expected == .name {
			p.name_error = true
		}
		mut s := expected.str()
		// quote keywords, punctuation, operators
		if token.is_key(s) || (s.len > 0 && !s[0].is_letter()) {
			s = '`$s`'
		}
		p.unexpected(expecting: s)
	}
}

[params]
struct ParamsForUnexpected {
	got            string
	expecting      string
	prepend_msg    string
	additional_msg string
}

fn (mut p Parser) unexpected(params ParamsForUnexpected) ast.NodeError {
	return p.unexpected_with_pos(p.tok.pos(), params)
}

fn (mut p Parser) unexpected_with_pos(pos token.Pos, params ParamsForUnexpected) ast.NodeError {
	mut msg := if params.got != '' {
		'unexpected $params.got'
	} else {
		'unexpected $p.tok'
	}
	if params.expecting != '' {
		msg += ', expecting $params.expecting'
	}
	if params.prepend_msg != '' {
		msg = '$params.prepend_msg ' + msg
	}
	if params.additional_msg != '' {
		msg += ', $params.additional_msg'
	}
	return p.error_with_pos(msg, pos)
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

[if trace_parser ?]
fn (p &Parser) trace_parser(label string) {
	eprintln('parsing: ${p.file_name:-30}|tok.pos: ${p.tok.pos().line_str():-39}|tok.kind: ${p.tok.kind:-10}|tok.lit: ${p.tok.lit:-10}|$label')
}

pub fn (mut p Parser) top_stmt() ast.Stmt {
	p.trace_parser('top_stmt')
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
						return p.struct_decl(false)
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
						return p.error('wrong pub keyword usage')
					}
				}
			}
			.lsbr {
				// attrs are stored in `p.attrs`
				p.attributes()
				continue
			}
			.key_asm {
				return p.asm_stmt(true)
			}
			.key_interface {
				return p.interface_decl()
			}
			.key_import {
				p.error_with_pos('`import x` can only be declared at the beginning of the file',
					p.tok.pos())
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
				return p.struct_decl(false)
			}
			.dollar {
				if p.peek_tok.kind == .eof {
					return p.unexpected(got: 'eof')
				}
				if_expr := p.if_expr(true)
				cur_stmt := ast.ExprStmt{
					expr: if_expr
					pos: if_expr.pos
				}
				if comptime_if_expr_contains_top_stmt(if_expr) {
					return cur_stmt
				} else {
					return p.other_stmts(cur_stmt)
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
				return p.struct_decl(false)
			}
			.comment {
				return p.comment_stmt()
			}
			else {
				return p.other_stmts(ast.empty_stmt)
			}
		}
		if p.should_abort {
			break
		}
	}
	// TODO remove dummy return statement
	// the compiler complains if it's not there
	return ast.empty_stmt
}

fn comptime_if_expr_contains_top_stmt(if_expr ast.IfExpr) bool {
	for branch in if_expr.branches {
		for stmt in branch.stmts {
			if stmt is ast.ExprStmt {
				if stmt.expr is ast.IfExpr {
					if !comptime_if_expr_contains_top_stmt(stmt.expr) {
						return false
					}
				} else if stmt.expr is ast.CallExpr {
					return false
				}
			} else if stmt is ast.AssignStmt {
				return false
			} else if stmt is ast.HashStmt {
				return true
			}
		}
	}
	return true
}

fn (mut p Parser) other_stmts(cur_stmt ast.Stmt) ast.Stmt {
	p.inside_fn = true
	if p.pref.is_script && !p.pref.is_test {
		p.script_mode = true
		p.script_mode_start_token = p.tok

		if p.table.known_fn('main.main') {
			p.error('function `main` is already defined, put your script statements inside it')
		}

		p.open_scope()
		mut stmts := []ast.Stmt{}
		if cur_stmt != ast.empty_stmt {
			stmts << cur_stmt
		}
		for p.tok.kind != .eof {
			stmts << p.stmt(false)
		}
		p.close_scope()

		p.script_mode = false
		return ast.FnDecl{
			name: 'main.main'
			short_name: 'main'
			mod: 'main'
			is_main: true
			stmts: stmts
			file: p.file_name
			return_type: ast.void_type
			scope: p.scope
			label_names: p.label_names
		}
	} else if p.pref.is_fmt {
		return p.stmt(false)
	} else {
		return p.error('bad top level statement ' + p.tok.str())
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
	mut pos := p.tok.pos()
	text := p.tok.lit
	num_newlines := text.count('\n')
	is_multi := num_newlines > 0
	is_inline := text.len + 4 == p.tok.len // 4: `/` `*` `*` `/`
	pos.last_line = pos.line_nr + num_newlines
	p.next()
	// Filter out false positive space indent vet errors inside comments
	if p.vet_errors.len > 0 && is_multi {
		p.vet_errors = p.vet_errors.filter(it.typ != .space_indent
			|| it.pos.line_nr - 1 > pos.last_line || it.pos.line_nr - 1 <= pos.line_nr)
	}
	return ast.Comment{
		text: text
		is_multi: is_multi
		is_inline: is_inline
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

[params]
struct EatCommentsConfig {
	same_line bool // Only eat comments on the same line as the previous token
	follow_up bool // Comments directly below the previous token as long as there is no empty line
}

pub fn (mut p Parser) eat_comments(cfg EatCommentsConfig) []ast.Comment {
	mut line := p.prev_tok.line_nr
	mut comments := []ast.Comment{}
	for {
		if p.tok.kind != .comment || (cfg.same_line && p.tok.line_nr > line)
			|| (cfg.follow_up && (p.tok.line_nr > line + 1 || p.tok.lit.contains('\n'))) {
			break
		}
		comments << p.comment()
		if cfg.follow_up {
			line = p.prev_tok.line_nr
		}
	}
	return comments
}

pub fn (mut p Parser) stmt(is_top_level bool) ast.Stmt {
	p.trace_parser('stmt($is_top_level)')
	p.is_stmt_ident = p.tok.kind == .name
	match p.tok.kind {
		.lcbr {
			mut pos := p.tok.pos()
			stmts := p.parse_block()
			pos.last_line = p.prev_tok.line_nr
			return ast.Block{
				stmts: stmts
				pos: pos
			}
		}
		.key_assert {
			p.next()
			mut pos := p.tok.pos()
			expr := p.expr(0)
			pos.update_last_line(p.prev_tok.line_nr)
			mut extra := ast.empty_expr
			mut extra_pos := p.tok.pos()
			if p.tok.kind == .comma {
				p.next()
				extra_pos = p.tok.pos()
				extra = p.expr(0)
				// dump(extra)
				extra_pos = extra_pos.extend(p.tok.pos())
			}
			return ast.AssertStmt{
				expr: expr
				extra: extra
				extra_pos: extra_pos
				pos: pos.extend(p.tok.pos())
				is_used: p.inside_test_file || !p.pref.is_prod
			}
		}
		.key_for {
			return p.for_stmt()
		}
		.name {
			if p.tok.lit == 'sql' && p.peek_tok.kind == .name {
				return p.sql_stmt()
			}
			if p.peek_tok.kind == .colon {
				// `label:`
				spos := p.tok.pos()
				name := p.check_name()
				if name in p.label_names {
					return p.error_with_pos('duplicate label `$name`', spos)
				}
				p.label_names << name
				p.next()
				if p.tok.kind == .key_for {
					for_pos := p.tok.pos()
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
							return p.error_with_pos('unknown kind of For statement', for_pos)
						}
					}
				}
				return ast.GotoLabel{
					name: name
					pos: spos.extend(p.tok.pos())
				}
			} else if p.peek_tok.kind == .name {
				return p.unexpected(got: 'name `$p.tok.lit`')
			} else if !p.inside_if_expr && !p.inside_match_body && !p.inside_or_expr
				&& p.peek_tok.kind in [.rcbr, .eof] && !p.mark_var_as_used(p.tok.lit) {
				return p.error_with_pos('`$p.tok.lit` evaluated but not used', p.tok.pos())
			}
			return p.parse_multi_expr(is_top_level)
		}
		.comment {
			return p.comment_stmt()
		}
		.key_return {
			if p.inside_defer {
				return p.error_with_pos('`return` not allowed inside `defer` block', p.tok.pos())
			} else {
				return p.return_stmt()
			}
		}
		.dollar {
			match p.peek_tok.kind {
				.key_if {
					mut pos := p.tok.pos()
					expr := p.if_expr(true)
					pos.update_last_line(p.prev_tok.line_nr)
					return ast.ExprStmt{
						expr: expr
						pos: pos
					}
				}
				.key_for {
					return p.comptime_for()
				}
				.name {
					mut pos := p.tok.pos()
					expr := p.comptime_call()
					pos.update_last_line(p.prev_tok.line_nr)
					return ast.ExprStmt{
						expr: expr
						pos: pos
					}
				}
				else {
					return p.unexpected(got: '\$')
				}
			}
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
				pos: tok.pos()
			}
		}
		.key_unsafe {
			return p.unsafe_stmt()
		}
		.hash {
			return p.hash()
		}
		.key_defer {
			if p.inside_defer {
				return p.error_with_pos('`defer` blocks cannot be nested', p.tok.pos())
			} else {
				p.next()
				spos := p.tok.pos()
				p.inside_defer = true
				p.defer_vars = []ast.Ident{}
				stmts := p.parse_block()
				p.inside_defer = false
				return ast.DeferStmt{
					stmts: stmts
					defer_vars: p.defer_vars.clone()
					pos: spos.extend_with_last_line(p.tok.pos(), p.prev_tok.line_nr)
				}
			}
		}
		.key_go {
			go_expr := p.go_expr()
			return ast.ExprStmt{
				expr: go_expr
				pos: go_expr.pos
			}
		}
		.key_goto {
			p.next()
			spos := p.tok.pos()
			name := p.check_name()
			return ast.GotoStmt{
				name: name
				pos: spos
			}
		}
		.key_const {
			return p.error_with_pos('const can only be defined at the top level (outside of functions)',
				p.tok.pos())
		}
		.key_asm {
			return p.asm_stmt(false)
		}
		// literals, 'if', etc. in here
		else {
			return p.parse_multi_expr(is_top_level)
		}
	}
}

fn (mut p Parser) asm_stmt(is_top_level bool) ast.AsmStmt {
	p.inside_asm = true
	p.inside_asm_template = true
	defer {
		p.inside_asm = false
		p.inside_asm_template = false
	}
	p.n_asm = 0
	if is_top_level {
		p.top_level_statement_start()
	}
	mut backup_scope := p.scope

	pos := p.tok.pos()

	p.check(.key_asm)
	mut arch := pref.arch_from_string(p.tok.lit) or { pref.Arch._auto }
	mut is_volatile := false
	mut is_goto := false
	if p.tok.kind == .key_volatile {
		arch = pref.arch_from_string(p.peek_tok.lit) or { pref.Arch._auto }
		is_volatile = true
		p.next()
	} else if p.tok.kind == .key_goto {
		arch = pref.arch_from_string(p.peek_tok.lit) or { pref.Arch._auto }
		is_goto = true
		p.next()
	}
	if arch == ._auto && !p.pref.is_fmt {
		if p.tok.lit == '' {
			p.error('missing assembly architecture. Try i386, amd64 or arm64.')
		}
		p.error('unknown assembly architecture')
	}
	if p.tok.kind != .name {
		p.error('must specify assembly architecture')
	} else {
		p.next()
	}

	p.check_for_impure_v(ast.pref_arch_to_table_language(arch), p.prev_tok.pos())

	p.check(.lcbr)
	p.scope = &ast.Scope{
		parent: 0 // you shouldn't be able to reference other variables in assembly blocks
		detached_from_parent: true
		start_pos: p.tok.pos
		objects: ast.all_registers(mut p.table, arch) //
	}

	mut local_labels := []string{}
	// riscv: https://github.com/jameslzhu/riscv-card/blob/master/riscv-card.pdf
	// x86: https://www.felixcloutier.com/x86/
	// arm: https://developer.arm.com/documentation/dui0068/b/arm-instruction-reference
	mut templates := []ast.AsmTemplate{}
	for p.tok.kind !in [.semicolon, .rcbr] {
		template_pos := p.tok.pos()
		mut name := ''
		if p.tok.kind == .name && arch == .amd64 && p.tok.lit in ['rex', 'vex', 'xop'] {
			name += p.tok.lit
			p.next()
			for p.tok.kind == .dot {
				p.next()
				name += '.' + p.tok.lit
				p.check(.name)
			}
			name += ' '
		}
		is_directive := p.tok.kind == .dot
		if is_directive {
			p.next()
		}
		if p.tok.kind in [.key_in, .key_lock, .key_orelse] { // `in`, `lock`, `or` are v keywords that are also x86/arm/riscv instructions.
			name += p.tok.kind.str()
			p.next()
		} else if p.tok.kind == .number {
			name += p.tok.lit
			p.next()
		} else {
			name += p.tok.lit
			p.check(.name)
		}
		// dots are part of instructions for some riscv extensions
		if arch in [.rv32, .rv64] {
			for p.tok.kind == .dot {
				name += '.'
				p.next()
				name += p.tok.lit
				p.check(.name)
			}
		}
		mut is_label := false

		mut args := []ast.AsmArg{}
		if p.tok.line_nr == p.prev_tok.line_nr {
			args_loop: for {
				if p.prev_tok.pos().line_nr < p.tok.pos().line_nr {
					break
				}
				mut segment := ''
				if p.tok.kind == .name && p.peek_tok.kind == .colon {
					segment = p.tok.lit
					p.next()
					p.next()
				}
				match p.tok.kind {
					.name {
						args << p.reg_or_alias()
					}
					.number {
						number_lit := p.parse_number_literal()
						match number_lit {
							ast.FloatLiteral {
								args << ast.FloatLiteral{
									...number_lit
								}
							}
							ast.IntegerLiteral {
								if is_directive {
									args << ast.AsmDisp{
										val: number_lit.val
										pos: number_lit.pos
									}
								} else {
									args << ast.IntegerLiteral{
										...number_lit
									}
								}
							}
							else {
								verror('p.parse_number_literal() invalid output: `$number_lit`')
							}
						}
					}
					.chartoken {
						args << ast.CharLiteral{
							val: p.tok.lit
							pos: p.tok.pos()
						}
						p.next()
					}
					.colon {
						is_label = true
						p.next()
						local_labels << name
						break
					}
					.lsbr {
						mut addressing := p.asm_addressing()
						addressing.segment = segment
						args << addressing
					}
					.rcbr {
						break
					}
					.semicolon {
						break
					}
					else {
						p.error('invalid token in assembly block')
					}
				}
				if p.tok.kind == .comma {
					p.next()
				} else {
					break
				}
			}
			// if p.prev_tok.pos().line_nr < p.tok.pos().line_nr {
			// 	break
			// }
		}
		mut comments := []ast.Comment{}
		for p.tok.kind == .comment {
			comments << p.comment()
		}
		if is_directive && name in ['globl', 'global'] {
			for arg in args {
				p.global_labels << (arg as ast.AsmAlias).name
			}
		}
		templates << ast.AsmTemplate{
			name: name
			args: args
			comments: comments
			is_label: is_label
			is_directive: is_directive
			pos: template_pos.extend(p.tok.pos())
		}
	}
	mut scope := p.scope
	p.scope = backup_scope
	p.inside_asm_template = false
	mut output, mut input, mut clobbered, mut global_labels := []ast.AsmIO{}, []ast.AsmIO{}, []ast.AsmClobbered{}, []string{}
	if !is_top_level {
		if p.tok.kind == .semicolon {
			output = p.asm_ios(true)
			if p.tok.kind == .semicolon {
				input = p.asm_ios(false)
			}
			if p.tok.kind == .semicolon {
				// because p.reg_or_alias() requires the scope with registers to recognize registers.
				backup_scope = p.scope
				p.scope = scope
				p.next()
				for p.tok.kind == .name {
					reg := ast.AsmRegister{
						name: p.tok.lit
						typ: 0
						size: -1
					}
					p.next()

					mut comments := []ast.Comment{}
					for p.tok.kind == .comment {
						comments << p.comment()
					}
					clobbered << ast.AsmClobbered{
						reg: reg
						comments: comments
					}

					if p.tok.kind in [.rcbr, .semicolon] {
						break
					}
				}

				if is_goto && p.tok.kind == .semicolon {
					p.next()
					for p.tok.kind == .name {
						global_labels << p.tok.lit
						p.next()
					}
				}
			}
		}
	} else if p.tok.kind == .semicolon {
		p.error('extended assembly is not allowed as a top level statement')
	}
	p.scope = backup_scope
	p.check(.rcbr)
	if is_top_level {
		p.top_level_statement_end()
	}
	scope.end_pos = p.prev_tok.pos

	return ast.AsmStmt{
		arch: arch
		is_goto: is_goto
		is_volatile: is_volatile
		templates: templates
		output: output
		input: input
		clobbered: clobbered
		pos: pos.extend(p.prev_tok.pos())
		is_basic: is_top_level || output.len + input.len + clobbered.len == 0
		scope: scope
		global_labels: global_labels
		local_labels: local_labels
	}
}

fn (mut p Parser) reg_or_alias() ast.AsmArg {
	p.check(.name)
	if p.prev_tok.lit in p.scope.objects {
		x := unsafe { p.scope.objects[p.prev_tok.lit] }
		if x is ast.AsmRegister {
			return ast.AsmArg(x as ast.AsmRegister)
		} else {
			verror('non-register ast.ScopeObject found in scope')
			return ast.AsmDisp{} // should not be reached
		}
	} else if p.prev_tok.len >= 2 && p.prev_tok.lit[0] in [`b`, `f`]
		&& p.prev_tok.lit[1..].bytes().all(it.is_digit()) {
		return ast.AsmDisp{
			val: p.prev_tok.lit[1..] + p.prev_tok.lit[0].ascii_str()
		}
	} else {
		return ast.AsmAlias{
			name: p.prev_tok.lit
			pos: p.prev_tok.pos()
		}
	}
}

// fn (mut p Parser) asm_addressing() ast.AsmAddressing {
// 	pos := p.tok.pos()
// 	p.check(.lsbr)
// 	unknown_addressing_mode := 'unknown addressing mode. supported ones are [displacement],	[base], [base + displacement] [index ∗ scale + displacement], [base + index ∗ scale + displacement], [base + index + displacement] [rip + displacement]'
// 	mut mode := ast.AddressingMode.invalid
// 	if p.peek_tok.kind == .rsbr {
// 		if p.tok.kind == .name {
// 			mode = .base
// 		} else if p.tok.kind == .number {
// 			mode = .displacement
// 		} else {
// 			p.error(unknown_addressing_mode)
// 		}
// 	} else if p.peek_tok.kind == .mul {
// 		mode = .index_times_scale_plus_displacement
// 	} else if p.tok.lit == 'rip' {
// 		mode = .rip_plus_displacement
// 	} else if p.peek_tok3.kind == .mul {
// 		mode = .base_plus_index_times_scale_plus_displacement
// 	} else if p.peek_tok.kind == .plus && p.peek_tok3.kind == .rsbr {
// 		mode = .base_plus_displacement
// 	} else if p.peek_tok.kind == .plus && p.peek_tok3.kind == .plus {
// 		mode = .base_plus_index_plus_displacement
// 	} else {
// 		p.error(unknown_addressing_mode)
// 	}
// 	mut displacement, mut base, mut index, mut scale := u32(0), ast.AsmArg{}, ast.AsmArg{}, -1

// 	match mode {
// 		.base {
// 			base = p.reg_or_alias()
// 		}
// 		.displacement {
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.base_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.index_times_scale_plus_displacement {
// 			index = p.reg_or_alias()
// 			p.check(.mul)
// 			scale = p.tok.lit.int()
// 			p.check(.number)
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.base_plus_index_times_scale_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			index = p.reg_or_alias()
// 			p.check(.mul)
// 			scale = p.tok.lit.int()
// 			p.check(.number)
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.rip_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.base_plus_index_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			index = p.reg_or_alias()
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.invalid {} // there was already an error above
// 	}

// 	p.check(.rsbr)
// 	return ast.AsmAddressing{
// 		base: base
// 		displacement: displacement
// 		index: index
// 		scale: scale
// 		mode: mode
// 		pos: pos.extend(p.prev_tok.pos())
// 	}
// }

fn (mut p Parser) asm_addressing() ast.AsmAddressing {
	pos := p.tok.pos()
	p.check(.lsbr)
	unknown_addressing_mode := 'unknown addressing mode. supported ones are [displacement],	[base], [base + displacement], [index ∗ scale + displacement], [base + index ∗ scale + displacement], [base + index + displacement], [rip + displacement]'
	// this mess used to look much cleaner before the removal of peek_tok2/3, see above code for cleaner version
	if p.peek_tok.kind == .rsbr { // [displacement] or [base]
		if p.tok.kind == .name {
			base := p.reg_or_alias()
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode: .base
				base: base
				pos: pos.extend(p.prev_tok.pos())
			}
		} else if p.tok.kind == .number {
			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode: .displacement
				displacement: displacement
				pos: pos.extend(p.prev_tok.pos())
			}
		} else {
			p.error(unknown_addressing_mode)
		}
	}
	if p.peek_tok.kind == .plus && p.tok.kind == .name { // [base + displacement], [base + index ∗ scale + displacement], [base + index + displacement] or [rip + displacement]
		if p.tok.lit == 'rip' {
			rip := p.reg_or_alias()
			p.next()

			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode: .rip_plus_displacement
				base: rip
				displacement: displacement
				pos: pos.extend(p.prev_tok.pos())
			}
		}
		base := p.reg_or_alias()
		p.next()
		if p.peek_tok.kind == .rsbr {
			if p.tok.kind == .number {
				displacement := if p.tok.kind == .name {
					p.reg_or_alias()
				} else {
					x := ast.AsmArg(ast.AsmDisp{
						val: p.tok.lit
						pos: p.tok.pos()
					})
					p.check(.number)
					x
				}
				p.check(.rsbr)
				return ast.AsmAddressing{
					mode: .base_plus_displacement
					base: base
					displacement: displacement
					pos: pos.extend(p.prev_tok.pos())
				}
			} else {
				p.error(unknown_addressing_mode)
			}
		}
		index := p.reg_or_alias()
		if p.tok.kind == .mul {
			p.next()
			scale := p.tok.lit.int()
			p.check(.number)
			p.check(.plus)
			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode: .base_plus_index_times_scale_plus_displacement
				base: base
				index: index
				scale: scale
				displacement: displacement
				pos: pos.extend(p.prev_tok.pos())
			}
		} else if p.tok.kind == .plus {
			p.next()
			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode: .base_plus_index_plus_displacement
				base: base
				index: index
				displacement: displacement
				pos: pos.extend(p.prev_tok.pos())
			}
		}
	}
	if p.peek_tok.kind == .mul { // [index ∗ scale + displacement]
		index := p.reg_or_alias()
		p.next()
		scale := p.tok.lit.int()
		p.check(.number)
		p.check(.plus)
		displacement := if p.tok.kind == .name {
			p.reg_or_alias()
		} else {
			x := ast.AsmArg(ast.AsmDisp{
				val: p.tok.lit
				pos: p.tok.pos()
			})
			p.check(.number)
			x
		}
		p.check(.rsbr)
		return ast.AsmAddressing{
			mode: .index_times_scale_plus_displacement
			index: index
			scale: scale
			displacement: displacement
			pos: pos.extend(p.prev_tok.pos())
		}
	}
	p.error(unknown_addressing_mode)
	return ast.AsmAddressing{}
}

fn (mut p Parser) asm_ios(output bool) []ast.AsmIO {
	mut res := []ast.AsmIO{}
	p.check(.semicolon)
	if p.tok.kind in [.rcbr, .semicolon] {
		return []
	}
	for {
		pos := p.tok.pos()

		mut constraint := ''
		if p.tok.kind == .lpar {
			constraint = if output { '+r' } else { 'r' } // default constraint, though vfmt fmts to `+r` and `r`
		} else {
			constraint += match p.tok.kind {
				.assign {
					'='
				}
				.plus {
					'+'
				}
				.mod {
					'%'
				}
				.amp {
					'&'
				}
				else {
					''
				}
			}
			if constraint != '' {
				p.next()
			}
			constraint += p.tok.lit
			if p.tok.kind == .at {
				p.next()
			} else {
				p.check(.name)
			}
		}
		mut expr := p.expr(0)
		if mut expr is ast.ParExpr {
			expr = expr.expr
		} else {
			p.error('asm in/output must be enclosed in brackets')
		}
		mut alias := ''
		if p.tok.kind == .key_as {
			p.next()
			alias = p.tok.lit
			p.check(.name)
		} else if mut expr is ast.Ident {
			alias = expr.name
		}
		// for constraints like `a`, no alias is needed, it is reffered to as rcx
		mut comments := []ast.Comment{}
		for p.tok.kind == .comment {
			comments << p.comment()
		}

		res << ast.AsmIO{
			alias: alias
			constraint: constraint
			expr: expr
			comments: comments
			pos: pos.extend(p.prev_tok.pos())
		}
		p.n_asm++
		if p.tok.kind in [.semicolon, .rcbr] {
			break
		}
	}
	return res
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

fn (mut p Parser) is_attributes() bool {
	if p.tok.kind != .lsbr {
		return false
	}
	mut i := 0
	for {
		tok := p.peek_token(i)
		if tok.kind == .eof || tok.line_nr != p.tok.line_nr {
			return false
		}
		if tok.kind == .rsbr {
			break
		}
		i++
	}
	peek_rsbr_tok := p.peek_token(i + 1)
	if peek_rsbr_tok.line_nr == p.tok.line_nr && peek_rsbr_tok.kind != .rcbr {
		return false
	}
	return true
}

// when is_top_stmt is true attrs are added to p.attrs
fn (mut p Parser) attributes() {
	p.check(.lsbr)
	mut has_ctdefine := false
	for p.tok.kind != .rsbr {
		start_pos := p.tok.pos()
		attr := p.parse_attr()
		if p.attrs.contains(attr.name) && attr.name != 'wasm_export' {
			p.error_with_pos('duplicate attribute `$attr.name`', start_pos.extend(p.prev_tok.pos()))
			return
		}
		if attr.kind == .comptime_define {
			if has_ctdefine {
				p.error_with_pos('only one `[if flag]` may be applied at a time `$attr.name`',
					start_pos.extend(p.prev_tok.pos()))
				return
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
			p.unexpected(expecting: '`;`')
			return
		}
		p.next()
	}
	if p.attrs.len == 0 {
		p.error_with_pos('attributes cannot be empty', p.prev_tok.pos().extend(p.tok.pos()))
		return
	}
}

fn (mut p Parser) parse_attr() ast.Attr {
	mut kind := ast.AttrKind.plain
	apos := p.prev_tok.pos()
	if p.tok.kind == .key_unsafe {
		p.next()
		return ast.Attr{
			name: 'unsafe'
			kind: kind
			pos: apos.extend(p.tok.pos())
		}
	}
	mut name := ''
	mut has_arg := false
	mut arg := ''
	mut comptime_cond := ast.empty_expr
	mut comptime_cond_opt := false
	if p.tok.kind == .key_if {
		kind = .comptime_define
		p.next()
		p.comptime_if_cond = true
		p.inside_if_expr = true
		p.inside_ct_if_expr = true
		comptime_cond = p.expr(0)
		p.comptime_if_cond = false
		p.inside_if_expr = false
		p.inside_ct_if_expr = false
		if comptime_cond is ast.PostfixExpr {
			comptime_cond_opt = true
		}
		name = comptime_cond.str()
	} else if p.tok.kind == .string {
		name = p.tok.lit
		kind = .string
		p.next()
	} else {
		name = p.check_name()
		// support dot prefix `module.name: arg`
		if p.tok.kind == .dot {
			p.next()
			name += '.'
			name += p.check_name()
		}
		if p.tok.kind == .colon {
			has_arg = true
			p.next()
			if p.tok.kind == .name { // `name: arg`
				kind = .plain
				arg = p.check_name()
			} else if p.tok.kind == .number { // `name: 123`
				kind = .number
				arg = p.tok.lit
				p.next()
			} else if p.tok.kind == .string { // `name: 'arg'`
				kind = .string
				arg = p.tok.lit
				p.next()
			} else if p.tok.kind == .key_true || p.tok.kind == .key_false { // `name: true`
				kind = .bool
				arg = p.tok.kind.str()
				p.next()
			} else {
				p.unexpected(additional_msg: 'an argument is expected after `:`')
			}
		}
	}
	return ast.Attr{
		name: name
		has_arg: has_arg
		arg: arg
		kind: kind
		ct_expr: comptime_cond
		ct_opt: comptime_cond_opt
		pos: apos.extend(p.tok.pos())
	}
}

pub fn (mut p Parser) language_not_allowed_error(language ast.Language, pos token.Pos) {
	upcase_language := language.str().to_upper()
	p.error_with_pos('$upcase_language code is not allowed in .${p.file_backend_mode}.v files, please move it to a .${language}.v file',
		pos)
}

pub fn (mut p Parser) language_not_allowed_warning(language ast.Language, pos token.Pos) {
	upcase_language := language.str().to_upper()
	p.warn_with_pos('$upcase_language code will not be allowed in pure .v files, please move it to a .${language}.v file instead',
		pos)
}

pub fn (mut p Parser) check_for_impure_v(language ast.Language, pos token.Pos) {
	if language == .v {
		// pure V code is always allowed everywhere
		return
	} else {
		match p.file_backend_mode {
			.c {
				if language != .c {
					p.language_not_allowed_error(language, pos)
					return
				}
			}
			.js {
				if language != .js {
					p.language_not_allowed_error(language, pos)
					return
				}
			}
			else {}
		}
	}
	if !p.pref.warn_impure_v {
		// the stricter mode is not ON yet => allow everything for now
		return
	}
	if p.file_backend_mode != language {
		if p.file_backend_mode == .v {
			p.language_not_allowed_warning(language, pos)
			return
		}
	}
}

pub fn (mut p Parser) error(s string) ast.NodeError {
	return p.error_with_pos(s, p.tok.pos())
}

pub fn (mut p Parser) warn(s string) {
	p.warn_with_pos(s, p.tok.pos())
}

pub fn (mut p Parser) note(s string) {
	p.note_with_pos(s, p.tok.pos())
}

pub fn (mut p Parser) error_with_pos(s string, pos token.Pos) ast.NodeError {
	mut kind := 'error:'
	if p.pref.fatal_errors {
		util.show_compiler_message(kind, pos: pos, file_path: p.file_name, message: s)
		exit(1)
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		if p.pref.is_verbose {
			print_backtrace()
			kind = 'parser error:'
		}
		util.show_compiler_message(kind, pos: pos, file_path: p.file_name, message: s)
		exit(1)
	} else {
		p.errors << errors.Error{
			file_path: p.file_name
			pos: pos
			reporter: .parser
			message: s
		}

		// To avoid getting stuck after an error, the parser
		// will proceed to the next token.
		if p.pref.check_only {
			p.next()
		}
	}
	if p.pref.output_mode == .silent {
		// Normally, parser errors mean that the parser exits immediately, so there can be only 1 parser error.
		// In the silent mode however, the parser continues to run, even though it would have stopped. Some
		// of the parser logic does not expect that, and may loop forever.
		// The p.next() here is needed, so the parser is more robust, and *always* advances, even in the -silent mode.
		p.next()
	}
	return ast.NodeError{
		idx: p.errors.len - 1
		pos: pos
	}
}

pub fn (mut p Parser) error_with_error(error errors.Error) {
	mut kind := 'error:'
	if p.pref.fatal_errors {
		util.show_compiler_message(kind, error.CompilerMessage)
		exit(1)
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		if p.pref.is_verbose {
			print_backtrace()
			kind = 'parser error:'
		}
		util.show_compiler_message(kind, error.CompilerMessage)
		exit(1)
	} else {
		if p.pref.message_limit >= 0 && p.errors.len >= p.pref.message_limit {
			p.should_abort = true
			return
		}
		p.errors << error
	}
	if p.pref.output_mode == .silent {
		// Normally, parser errors mean that the parser exits immediately, so there can be only 1 parser error.
		// In the silent mode however, the parser continues to run, even though it would have stopped. Some
		// of the parser logic does not expect that, and may loop forever.
		// The p.next() here is needed, so the parser is more robust, and *always* advances, even in the -silent mode.
		p.next()
	}
}

pub fn (mut p Parser) warn_with_pos(s string, pos token.Pos) {
	if p.pref.warns_are_errors {
		p.error_with_pos(s, pos)
		return
	}
	if p.pref.skip_warnings {
		return
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		util.show_compiler_message('warning:', pos: pos, file_path: p.file_name, message: s)
	} else {
		if p.pref.message_limit >= 0 && p.warnings.len >= p.pref.message_limit {
			p.should_abort = true
			return
		}
		p.warnings << errors.Warning{
			file_path: p.file_name
			pos: pos
			reporter: .parser
			message: s
		}
	}
}

pub fn (mut p Parser) note_with_pos(s string, pos token.Pos) {
	if p.pref.skip_warnings {
		return
	}
	if p.is_generated {
		return
	}
	if p.pref.output_mode == .stdout && !p.pref.check_only {
		util.show_compiler_message('notice:', pos: pos, file_path: p.file_name, message: s)
	} else {
		p.notices << errors.Notice{
			file_path: p.file_name
			pos: pos
			reporter: .parser
			message: s
		}
	}
}

pub fn (mut p Parser) vet_error(msg string, line int, fix vet.FixKind, typ vet.ErrorType) {
	pos := token.Pos{
		line_nr: line + 1
	}
	p.vet_errors << vet.Error{
		message: msg
		file_path: p.scanner.file_path
		pos: pos
		kind: .error
		fix: fix
		typ: typ
	}
}

fn (mut p Parser) parse_multi_expr(is_top_level bool) ast.Stmt {
	// in here might be 1) multi-expr 2) multi-assign
	// 1, a, c ... }       // multi-expression
	// a, mut b ... :=/=   // multi-assign
	// collect things upto hard boundaries
	tok := p.tok
	mut pos := tok.pos()

	mut defer_vars := p.defer_vars.clone()
	p.defer_vars = []ast.Ident{}

	left, left_comments := p.expr_list()

	if !(p.inside_defer && p.tok.kind == .decl_assign) {
		defer_vars << p.defer_vars
	}

	p.defer_vars = defer_vars

	left0 := left[0]
	if tok.kind == .key_mut && p.tok.kind != .decl_assign {
		return p.error('expecting `:=` (e.g. `mut x :=`)')
	}
	// TODO remove translated
	if p.tok.kind in [.assign, .decl_assign] || p.tok.kind.is_assign() {
		return p.partial_assign_stmt(left, left_comments)
	} else if !p.pref.translated && !p.is_translated && !p.pref.is_fmt && !p.pref.is_vet
		&& tok.kind !in [.key_if, .key_match, .key_lock, .key_rlock, .key_select] {
		for node in left {
			if (is_top_level || p.tok.kind != .rcbr) && node !is ast.CallExpr
				&& node !is ast.PostfixExpr && node !is ast.ComptimeCall
				&& node !is ast.SelectorExpr && node !is ast.DumpExpr {
				is_complex_infix_expr := node is ast.InfixExpr
					&& (node as ast.InfixExpr).op in [.left_shift, .right_shift, .unsigned_right_shift, .arrow]
				if !is_complex_infix_expr {
					return p.error_with_pos('expression evaluated but not used', node.pos())
				}
			}
		}
	}
	pos.update_last_line(p.prev_tok.line_nr)
	if left.len == 1 {
		return ast.ExprStmt{
			expr: left0
			pos: left0.pos()
			comments: left_comments
			is_expr: p.inside_for
		}
	}
	return ast.ExprStmt{
		expr: ast.ConcatExpr{
			vals: left
			pos: tok.pos()
		}
		pos: pos
		comments: left_comments
	}
}

pub fn (mut p Parser) parse_ident(language ast.Language) ast.Ident {
	// p.warn('name ')
	is_shared := p.tok.kind == .key_shared
	is_atomic := p.tok.kind == .key_atomic
	if is_shared {
		p.register_auto_import('sync')
	}
	mut_pos := p.tok.pos()
	modifier_kind := p.tok.kind
	is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
	if is_mut {
		p.next()
	}
	is_static := p.tok.kind == .key_static
	if is_static {
		p.next()
	}
	is_volatile := p.tok.kind == .key_volatile
	if is_volatile {
		p.next()
	}
	if p.tok.kind != .name {
		if is_mut || is_static || is_volatile {
			p.error_with_pos('the `$modifier_kind` keyword is invalid here', mut_pos)
		} else {
			p.unexpected(got: 'token `$p.tok.lit`')
		}
		return ast.Ident{
			scope: p.scope
		}
	}
	pos := p.tok.pos()
	mut name := p.check_name()
	if name == '_' {
		return ast.Ident{
			tok_kind: p.tok.kind
			name: '_'
			comptime: p.comptime_if_cond
			kind: .blank_ident
			pos: pos
			info: ast.IdentVar{
				is_mut: false
				is_static: false
				is_volatile: false
			}
			scope: p.scope
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
		comptime: p.comptime_if_cond
		language: language
		mod: p.mod
		pos: pos
		is_mut: is_mut
		mut_pos: mut_pos
		info: ast.IdentVar{
			is_mut: is_mut
			is_static: is_static
			is_volatile: is_volatile
			share: ast.sharetype_from_flags(is_shared, is_atomic)
		}
		scope: p.scope
	}
}

fn (p &Parser) is_typename(t token.Token) bool {
	return t.kind == .name && (t.lit[0].is_capital() || p.table.known_type(t.lit))
}

// heuristics to detect `func<T>()` from `var < expr`
// 1. `f<[]` is generic(e.g. `f<[]int>`) because `var < []` is invalid
// 2. `f<map[` is generic(e.g. `f<map[string]string>)
// 3. `f<foo>` is generic because `v1 < foo > v2` is invalid syntax
// 4. `f<foo<bar` is generic when bar is not generic T (f<foo<T>(), in contrast, is not generic!)
// 5. `f<Foo,` is generic when Foo is typename.
//	   otherwise it is not generic because it may be multi-value (e.g. `return f < foo, 0`).
// 6. `f<mod.Foo>` is same as case 3
// 7. `f<mod.Foo,` is same as case 5
// 8. if there is a &, ignore the & and see if it is a type
// 9. otherwise, it's not generic
// see also test_generic_detection in vlib/v/tests/generics_test.v
fn (p &Parser) is_generic_call() bool {
	lit0_is_capital := p.tok.kind != .eof && p.tok.lit.len > 0 && p.tok.lit[0].is_capital()
	if lit0_is_capital || p.peek_tok.kind != .lt {
		return false
	}
	mut tok2 := p.peek_token(2)
	mut tok3 := p.peek_token(3)
	mut tok4 := p.peek_token(4)
	mut tok5 := p.peek_token(5)
	mut kind2, mut kind3, mut kind4, mut kind5 := tok2.kind, tok3.kind, tok4.kind, tok5.kind
	if kind2 == .amp { // if there is a & in front, shift everything left
		tok2 = tok3
		kind2 = kind3
		tok3 = tok4
		kind3 = kind4
		tok4 = tok5
		kind4 = kind5
		tok5 = p.peek_token(6)
		kind5 = tok5.kind
	}

	if kind2 == .lsbr {
		// case 1
		return tok3.kind == .rsbr
	}

	if kind2 == .name {
		if tok2.lit == 'map' && kind3 == .lsbr {
			// case 2
			return true
		}
		return match kind3 {
			.gt { true } // case 3
			.lt { !(tok4.lit.len == 1 && tok4.lit[0].is_capital()) } // case 4
			.comma { p.is_typename(tok2) } // case 5
			// case 6 and 7
			.dot { kind4 == .name && (kind5 == .gt || (kind5 == .comma && p.is_typename(tok4))) }
			else { false }
		}
	}
	return false
}

const valid_tokens_inside_types = [token.Kind.lsbr, .rsbr, .name, .dot, .comma, .key_fn, .lt]

fn (mut p Parser) is_generic_cast() bool {
	if !ast.type_can_start_with_token(p.tok) {
		return false
	}
	mut i := 0
	mut level := 0
	mut lt_count := 0
	for {
		i++
		tok := p.peek_token(i)

		if tok.kind == .lt {
			lt_count++
			level++
		} else if tok.kind == .gt {
			level--
		}
		if lt_count > 0 && level == 0 {
			break
		}

		if i > 20 || tok.kind !in parser.valid_tokens_inside_types {
			return false
		}
	}
	next_tok := p.peek_token(i + 1)
	// `next_tok` is the token following the closing `>` of the generic type: MyType<int>{
	//                                                                                   ^
	// if `next_tok` is a left paren, then the full expression looks something like
	// `Foo<string>(` or `Foo<mod.Type>(`, which are valid type casts - return true
	if next_tok.kind == .lpar {
		return true
	}
	// any other token is not a valid generic cast, however
	return false
}

pub fn (mut p Parser) name_expr() ast.Expr {
	prev_tok_kind := p.prev_tok.kind
	mut node := ast.empty_expr
	if p.expecting_type {
		if p.tok.kind == .dollar {
			node = p.parse_comptime_type()
			p.expecting_type = false
			return node
		}
		p.expecting_type = false
		// get type position before moving to next
		type_pos := p.tok.pos()
		typ := p.parse_type()
		return ast.TypeNode{
			typ: typ
			pos: type_pos
		}
	}
	mut language := ast.Language.v
	if p.tok.lit == 'C' {
		language = ast.Language.c
		p.check_for_impure_v(language, p.tok.pos())
	} else if p.tok.lit == 'JS' {
		language = ast.Language.js
		p.check_for_impure_v(language, p.tok.pos())
	}
	mut mod := ''
	// p.warn('resetting')
	p.expr_mod = ''
	// `map[string]int` initialization
	if p.tok.lit == 'map' && p.peek_tok.kind == .lsbr {
		mut pos := p.tok.pos()
		map_type := p.parse_map_type()
		if p.tok.kind == .lcbr {
			p.next()
			if p.tok.kind == .rcbr {
				pos = pos.extend(p.tok.pos())
				p.next()
			} else {
				if p.pref.is_fmt {
					map_init := p.map_init()
					p.check(.rcbr)
					return map_init
				}
				p.error('`}` expected; explicit `map` initialization does not support parameters')
			}
		}
		return ast.MapInit{
			typ: map_type
			pos: pos
		}
	}
	// `chan typ{...}`
	if p.tok.lit == 'chan' {
		first_pos := p.tok.pos()
		mut last_pos := first_pos
		mut elem_type_pos := p.peek_tok.pos()
		chan_type := p.parse_chan_type()
		elem_type_pos = elem_type_pos.extend(p.prev_tok.pos())
		mut has_cap := false
		mut cap_expr := ast.empty_expr
		p.check(.lcbr)
		if p.tok.kind == .rcbr {
			last_pos = p.tok.pos()
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
					return p.error('`$key` cannot be initialized for `chan`. Did you mean `cap`?')
				}
				else {
					return p.error('wrong field `$key`, expecting `cap`')
				}
			}
			last_pos = p.tok.pos()
			p.check(.rcbr)
		}
		return ast.ChanInit{
			pos: first_pos.extend(last_pos)
			elem_type_pos: elem_type_pos
			has_cap: has_cap
			cap_expr: cap_expr
			typ: chan_type
		}
	}
	// Raw string (`s := r'hello \n ')
	if p.peek_tok.kind == .string && !p.inside_str_interp && p.peek_token(2).kind != .colon {
		if p.tok.kind == .name && p.tok.lit in ['r', 'c', 'js'] {
			return p.string_expr()
		} else {
			// don't allow any other string prefix except `r`, `js` and `c`
			return p.error('only `c`, `r`, `js` are recognized string prefixes, but you tried to use `$p.tok.lit`')
		}
	}
	// don't allow r`byte` and c`byte`
	if p.peek_tok.kind == .chartoken && p.tok.lit.len == 1 && p.tok.lit[0] in [`r`, `c`] {
		opt := if p.tok.lit == 'r' { '`r` (raw string)' } else { '`c` (c string)' }
		return p.error('cannot use $opt with `byte` and `rune`')
	}
	// Make sure that the var is not marked as used in assignments: `x = 1`, `x += 2` etc
	// but only when it's actually used (e.g. `println(x)`)
	known_var := if p.peek_tok.kind.is_assign() {
		p.scope.known_var(p.tok.lit)
	} else {
		p.mark_var_as_used(p.tok.lit)
	}
	// Handle modules
	mut is_mod_cast := false
	if p.peek_tok.kind == .dot && !known_var && (language != .v || p.known_import(p.tok.lit)
		|| p.mod.all_after_last('.') == p.tok.lit) {
		// p.tok.lit has been recognized as a module
		if language == .c {
			mod = 'C'
		} else if language == .js {
			mod = 'JS'
		} else {
			if p.tok.lit in p.imports {
				// mark the imported module as used
				p.register_used_import(p.tok.lit)
				if p.peek_tok.kind == .dot && p.peek_token(2).kind != .eof
					&& p.peek_token(2).lit.len > 0 && p.peek_token(2).lit[0].is_capital() {
					is_mod_cast = true
				} else if p.peek_tok.kind == .dot && p.peek_token(2).kind != .eof
					&& p.peek_token(2).lit.len == 0 {
					// incomplete module selector must be handled by dot_expr instead
					ident := p.parse_ident(language)
					node = ident
					if p.inside_defer {
						if !p.defer_vars.any(it.name == ident.name && it.mod == ident.mod)
							&& ident.name != 'err' {
							p.defer_vars << ident
						}
					}
					return node
				}
			}
			// prepend the full import
			mod = p.imports[p.tok.lit]
		}
		p.next()
		p.check(.dot)
		p.expr_mod = mod
	}
	lit0_is_capital := if p.tok.kind != .eof && p.tok.lit.len > 0 {
		p.tok.lit[0].is_capital()
	} else {
		false
	}
	is_optional := p.tok.kind == .question
	is_generic_call := p.is_generic_call()
	is_generic_cast := p.is_generic_cast()
	// p.warn('name expr  $p.tok.lit $p.peek_tok.str()')
	same_line := p.tok.line_nr == p.peek_tok.line_nr
	// `(` must be on same line as name token otherwise it's a ParExpr
	if !same_line && p.peek_tok.kind == .lpar {
		ident := p.parse_ident(language)
		node = ident
		if p.inside_defer {
			if !p.defer_vars.any(it.name == ident.name && it.mod == ident.mod)
				&& ident.name != 'err' {
				p.defer_vars << ident
			}
		}
	} else if p.peek_tok.kind == .lpar || is_generic_call || is_generic_cast
		|| (is_optional && p.peek_token(2).kind == .lpar) {
		// foo(), foo<int>() or type() cast
		mut name := if is_optional { p.peek_tok.lit } else { p.tok.lit }
		if mod.len > 0 {
			name = '${mod}.$name'
		}
		name_w_mod := p.prepend_mod(name)
		// type cast. TODO: finish
		// if name in ast.builtin_type_names_to_idx {
		if (!known_var && (name in p.table.type_idxs || name_w_mod in p.table.type_idxs)
			&& name !in ['C.stat', 'C.sigaction']) || is_mod_cast || is_generic_cast
			|| (language == .v && name.len > 0 && name[0].is_capital()) {
			// MainLetter(x) is *always* a cast, as long as it is not `C.`
			// TODO handle C.stat()
			start_pos := p.tok.pos()
			mut to_typ := p.parse_type()
			// this prevents inner casts to also have an `&`
			// example: &Foo(malloc(int(num)))
			// without the next line int would result in int*
			p.is_amp = false
			p.check(.lpar)
			mut expr := ast.empty_expr
			mut arg := ast.empty_expr
			mut has_arg := false
			expr = p.expr(0)
			// TODO, string(b, len)
			if p.tok.kind == .comma && to_typ.idx() == ast.string_type_idx {
				p.next()
				arg = p.expr(0) // len
				has_arg = true
			}
			if p.tok.kind == .comma && p.peek_tok.kind == .rpar {
				p.next()
			}
			end_pos := p.tok.pos()
			p.check(.rpar)
			node = ast.CastExpr{
				typ: to_typ
				typname: p.table.sym(to_typ).name
				expr: expr
				arg: arg
				has_arg: has_arg
				pos: start_pos.extend(end_pos)
			}
			p.expr_mod = ''
			return node
		} else {
			// fn call
			if is_optional {
				p.unexpected_with_pos(p.prev_tok.pos(),
					got: '$p.prev_tok'
				)
			}
			node = p.call_expr(language, mod)
			if p.tok.kind == .lpar && p.prev_tok.line_nr == p.tok.line_nr {
				p.next()
				pos := p.tok.pos()
				args := p.call_args()
				p.check(.rpar)
				node = ast.CallExpr{
					left: node
					args: args
					pos: pos
					scope: p.scope
				}
			}
		}
	} else if (p.peek_tok.kind == .lcbr || (p.peek_tok.kind == .lt && lit0_is_capital))
		&& (!p.inside_match || (p.inside_select && prev_tok_kind == .arrow && lit0_is_capital))
		&& !p.inside_match_case && (!p.inside_if || p.inside_select)
		&& (!p.inside_for || p.inside_select) && !known_var {
		return p.struct_init(p.mod + '.' + p.tok.lit, .normal) // short_syntax: false
	} else if p.peek_tok.kind == .lcbr
		&& ((p.inside_if && lit0_is_capital && p.tok.lit.len > 1 && !known_var && language == .v)
		|| (p.inside_match_case && p.tok.kind == .name && p.peek_tok.pos - p.tok.pos == p.tok.len)) {
		// `if a == Foo{} {...}` or `match foo { Foo{} {...} }`
		return p.struct_init(p.mod + '.' + p.tok.lit, .normal)
	} else if p.peek_tok.kind == .dot && (lit0_is_capital && !known_var && language == .v) {
		// T.name
		if p.is_generic_name() {
			pos := p.tok.pos()
			name := p.check_name()
			p.check(.dot)
			field := p.check_name()
			fkind := match field {
				'name' { ast.GenericKindField.name }
				'typ' { ast.GenericKindField.typ }
				else { ast.GenericKindField.unknown }
			}
			pos.extend(p.tok.pos())
			return ast.SelectorExpr{
				expr: ast.Ident{
					name: name
					scope: p.scope
				}
				field_name: field
				gkind_field: fkind
				pos: pos
				scope: p.scope
			}
		}
		if p.peek_token(2).kind == .name && p.peek_token(3).kind == .lpar && !known_var {
			p.error_with_pos('the receiver of the method call must be an instantiated object, e.g. `foo.bar()`',
				p.tok.pos())
		}
		// `Color.green`
		mut enum_name := p.check_name()
		enum_name_pos := p.prev_tok.pos()
		if mod != '' {
			enum_name = mod + '.' + enum_name
		} else {
			enum_name = p.imported_symbols[enum_name] or { p.prepend_mod(enum_name) }
		}
		p.check(.dot)
		val := p.check_name()
		p.expr_mod = ''
		return ast.EnumVal{
			enum_name: enum_name
			val: val
			pos: enum_name_pos.extend(p.prev_tok.pos())
			mod: mod
		}
	} else if language == .js && p.peek_tok.kind == .dot && p.peek_token(2).kind == .name {
		// JS. function call with more than 1 dot
		node = p.call_expr(language, mod)
	} else {
		if p.inside_in_array && ((lit0_is_capital && !known_var && language == .v)
			|| (p.peek_tok.kind == .dot && p.peek_token(2).lit.len > 0
			&& p.peek_token(2).lit[0].is_capital())
			|| p.table.find_type_idx(p.mod + '.' + p.tok.lit) > 0) {
			type_pos := p.tok.pos()
			typ := p.parse_type()
			return ast.TypeNode{
				typ: typ
				pos: type_pos
			}
		} else if !known_var && language == .v && (p.table.known_type(p.tok.lit) || lit0_is_capital)
			&& p.peek_tok.kind == .pipe {
			start_pos := p.tok.pos()
			mut to_typ := p.parse_type()
			p.check(.lpar)
			expr := p.expr(0)
			end_pos := p.tok.pos()
			p.check(.rpar)
			node = ast.CastExpr{
				typ: to_typ
				typname: p.table.sym(to_typ).name
				expr: expr
				arg: ast.empty_expr
				has_arg: false
				pos: start_pos.extend(end_pos)
			}
			p.expr_mod = ''
			return node
		}

		ident := p.parse_ident(language)
		node = ident
		if p.inside_defer {
			if !p.defer_vars.any(it.name == ident.name && it.mod == ident.mod)
				&& ident.name != 'err' {
				p.defer_vars << ident
			}
		}
	}
	p.expr_mod = ''
	return node
}

enum OrBlockErrVarMode {
	no_err_var
	with_err_var
}

fn (mut p Parser) or_block(err_var_mode OrBlockErrVarMode) ([]ast.Stmt, token.Pos) {
	was_inside_or_expr := p.inside_or_expr
	defer {
		p.inside_or_expr = was_inside_or_expr
	}
	p.inside_or_expr = true

	mut pos := p.tok.pos()
	p.next()
	p.open_scope()
	defer {
		p.close_scope()
	}

	if err_var_mode == .with_err_var {
		p.scope.register(ast.Var{
			name: 'err'
			typ: ast.error_type
			pos: p.tok.pos()
			is_used: true
			is_stack_obj: true
		})
	}

	stmts := p.parse_block_no_scope(false)
	pos = pos.extend(p.prev_tok.pos())
	return stmts, pos
}

fn (mut p Parser) index_expr(left ast.Expr, is_gated bool) ast.IndexExpr {
	// left == `a` in `a[0]`
	start_pos := p.tok.pos()
	p.next() // [
	mut has_low := true
	if p.tok.kind == .dotdot {
		has_low = false
		// [..end]
		p.next()
		mut high := ast.empty_expr
		mut has_high := false
		if p.tok.kind != .rsbr {
			high = p.expr(0)
			has_high = true
		}

		pos_high := start_pos.extend(p.tok.pos())
		p.check(.rsbr)
		mut or_kind_high := ast.OrKind.absent
		mut or_stmts_high := []ast.Stmt{}
		mut or_pos_high := token.Pos{}

		if !p.or_is_handled {
			// a[..end] or {...}
			if p.tok.kind == .key_orelse {
				or_stmts_high, or_pos_high = p.or_block(.no_err_var)
				return ast.IndexExpr{
					left: left
					pos: pos_high
					index: ast.RangeExpr{
						low: ast.empty_expr
						high: high
						has_high: has_high
						pos: pos_high
						is_gated: is_gated
					}
					or_expr: ast.OrExpr{
						kind: .block
						stmts: or_stmts_high
						pos: or_pos_high
					}
					is_gated: is_gated
				}
			}
			// `a[start..end] ?`
			if p.tok.kind == .question {
				or_pos_high = p.tok.pos()
				or_kind_high = .propagate_option
				p.next()
			}
		}

		return ast.IndexExpr{
			left: left
			pos: pos_high
			index: ast.RangeExpr{
				low: ast.empty_expr
				high: high
				has_high: has_high
				pos: pos_high
				is_gated: is_gated
			}
			or_expr: ast.OrExpr{
				kind: or_kind_high
				stmts: or_stmts_high
				pos: or_pos_high
			}
			is_gated: is_gated
		}
	}
	expr := p.expr(0) // `[expr]` or  `[expr..`
	mut has_high := false

	if p.tok.kind == .dotdot {
		// either [start..end] or [start..]
		p.next()
		mut high := ast.empty_expr
		if p.tok.kind != .rsbr {
			has_high = true
			high = p.expr(0)
		}
		pos_low := start_pos.extend(p.tok.pos())
		p.check(.rsbr)
		mut or_kind_low := ast.OrKind.absent
		mut or_stmts_low := []ast.Stmt{}
		mut or_pos_low := token.Pos{}

		if !p.or_is_handled {
			// a[start..end] or {...}
			if p.tok.kind == .key_orelse {
				or_stmts_low, or_pos_low = p.or_block(.no_err_var)
				return ast.IndexExpr{
					left: left
					pos: pos_low
					index: ast.RangeExpr{
						low: expr
						high: high
						has_high: has_high
						has_low: has_low
						pos: pos_low
						is_gated: is_gated
					}
					or_expr: ast.OrExpr{
						kind: .block
						stmts: or_stmts_low
						pos: or_pos_low
					}
					is_gated: is_gated
				}
			}
			// `a[start..end] ?`
			if p.tok.kind == .question {
				or_pos_low = p.tok.pos()
				or_kind_low = .propagate_option
				p.next()
			}
		}

		return ast.IndexExpr{
			left: left
			pos: pos_low
			index: ast.RangeExpr{
				low: expr
				high: high
				has_high: has_high
				has_low: has_low
				pos: pos_low
				is_gated: is_gated
			}
			or_expr: ast.OrExpr{
				kind: or_kind_low
				stmts: or_stmts_low
				pos: or_pos_low
			}
			is_gated: is_gated
		}
	}
	// [expr]
	pos := start_pos.extend(p.tok.pos())
	p.check(.rsbr)
	mut or_kind := ast.OrKind.absent
	mut or_stmts := []ast.Stmt{}
	mut or_pos := token.Pos{}
	if !p.or_is_handled {
		// a[i] or { ... }
		if p.tok.kind == .key_orelse {
			or_stmts, or_pos = p.or_block(.no_err_var)
			return ast.IndexExpr{
				left: left
				index: expr
				pos: pos
				or_expr: ast.OrExpr{
					kind: .block
					stmts: or_stmts
					pos: or_pos
				}
				is_gated: is_gated
			}
		}
		// `a[i] ?`
		if p.tok.kind == .question {
			or_pos = p.tok.pos()
			or_kind = .propagate_option
			p.next()
		}
	}
	return ast.IndexExpr{
		left: left
		index: expr
		pos: pos
		or_expr: ast.OrExpr{
			kind: or_kind
			stmts: or_stmts
			pos: or_pos
		}
		is_gated: is_gated
	}
}

fn (mut p Parser) scope_register_it() {
	p.scope.register(ast.Var{
		name: 'it'
		pos: p.tok.pos()
		is_used: true
	})
}

fn (mut p Parser) scope_register_ab() {
	p.scope.register(ast.Var{
		name: 'a'
		pos: p.tok.pos()
		is_used: true
	})
	p.scope.register(ast.Var{
		name: 'b'
		pos: p.tok.pos()
		is_used: true
	})
}

fn (mut p Parser) dot_expr(left ast.Expr) ast.Expr {
	p.next()
	if p.tok.kind == .dollar {
		return p.comptime_selector(left)
	}
	is_generic_call := p.is_generic_call()
	name_pos := p.tok.pos()
	mut field_name := ''
	// check if the name is on the same line as the dot
	if (p.prev_tok.pos().line_nr == name_pos.line_nr) || p.tok.kind != .name {
		field_name = p.check_name()
	} else {
		p.name_error = true
	}
	is_filter := field_name in ['filter', 'map', 'any', 'all']
	if is_filter || field_name == 'sort' {
		p.open_scope()
	}
	// ! in mutable methods
	if p.tok.kind == .not && p.peek_tok.kind == .lpar {
		p.next()
	}
	// Method call
	// TODO move to fn.v call_expr()
	mut concrete_types := []ast.Type{}
	mut concrete_list_pos := p.tok.pos()
	if is_generic_call {
		// `g.foo<int>(10)`
		concrete_types = p.parse_concrete_types()
		concrete_list_pos = concrete_list_pos.extend(p.prev_tok.pos())
		// In case of `foo<T>()`
		// T is unwrapped and registered in the checker.
		has_generic := concrete_types.any(it.has_flag(.generic))
		if !has_generic {
			// will be added in checker
			p.table.register_fn_concrete_types(field_name, concrete_types)
		}
	}
	if p.tok.kind == .lpar {
		p.next()
		args := p.call_args()
		p.check(.rpar)
		mut or_stmts := []ast.Stmt{}
		mut or_kind := ast.OrKind.absent
		mut or_pos := p.tok.pos()
		if p.tok.kind == .key_orelse {
			or_kind = .block
			or_stmts, or_pos = p.or_block(.with_err_var)
		}
		// `foo()?`
		if p.tok.kind in [.question, .not] {
			is_not := p.tok.kind == .not
			p.next()
			if p.inside_defer {
				p.error_with_pos('error propagation not allowed inside `defer` blocks',
					p.prev_tok.pos())
			}
			or_kind = if is_not { .propagate_result } else { .propagate_option }
		}
		end_pos := p.prev_tok.pos()
		pos := name_pos.extend(end_pos)
		comments := p.eat_comments(same_line: true)
		mcall_expr := ast.CallExpr{
			left: left
			name: field_name
			args: args
			name_pos: name_pos
			pos: pos
			is_method: true
			concrete_types: concrete_types
			concrete_list_pos: concrete_list_pos
			or_block: ast.OrExpr{
				stmts: or_stmts
				kind: or_kind
				pos: or_pos
			}
			scope: p.scope
			comments: comments
		}
		if is_filter || field_name == 'sort' {
			p.close_scope()
		}
		return mcall_expr
	}
	mut is_mut := false
	mut mut_pos := token.Pos{}
	if p.inside_match || p.inside_if_expr || p.inside_for {
		match left {
			ast.Ident, ast.SelectorExpr {
				is_mut = left.is_mut
				mut_pos = left.mut_pos
			}
			else {}
		}
	}
	pos := if p.name_error { left.pos().extend(name_pos) } else { name_pos }
	sel_expr := ast.SelectorExpr{
		expr: left
		field_name: field_name
		pos: pos
		is_mut: is_mut
		mut_pos: mut_pos
		scope: p.scope
		next_token: p.tok.kind
	}
	if is_filter {
		p.close_scope()
	}
	return sel_expr
}

fn (mut p Parser) parse_generic_types() ([]ast.Type, []string) {
	mut types := []ast.Type{}
	mut param_names := []string{}
	if p.tok.kind != .lt {
		return types, param_names
	}
	p.check(.lt)
	mut first_done := false
	mut count := 0
	for p.tok.kind !in [.gt, .eof] {
		if first_done {
			p.check(.comma)
		}
		name := p.tok.lit
		if name.len > 0 && !name[0].is_capital() {
			p.error('generic parameter needs to be uppercase')
		}
		if name.len > 1 {
			p.error('generic parameter name needs to be exactly one char')
		}
		if !util.is_generic_type_name(p.tok.lit) {
			p.error('`$p.tok.lit` is a reserved name and cannot be used for generics')
		}
		if name in param_names {
			p.error('duplicated generic parameter `$name`')
		}
		if count > 8 {
			p.error('cannot have more than 9 generic parameters')
		}
		p.check(.name)
		param_names << name

		mut idx := p.table.find_type_idx(name)
		if idx == 0 {
			idx = p.table.register_sym(ast.TypeSymbol{
				name: name
				cname: util.no_dots(name)
				mod: p.mod
				kind: .any
				is_pub: true
			})
		}
		types << ast.new_type(idx).set_flag(.generic)
		first_done = true
		count++
	}
	p.check(.gt)
	return types, param_names
}

fn (mut p Parser) parse_concrete_types() []ast.Type {
	mut types := []ast.Type{}
	if p.tok.kind != .lt {
		return types
	}
	p.next() // `<`
	mut first_done := false
	for p.tok.kind !in [.eof, .gt] {
		if first_done {
			p.check(.comma)
		}
		types << p.parse_type()
		first_done = true
	}
	p.check(.gt) // `>`
	return types
}

// is_generic_name returns true if the current token is a generic name.
fn (p Parser) is_generic_name() bool {
	return p.tok.kind == .name && util.is_generic_type_name(p.tok.lit)
}

// `.green`
// `pref.BuildMode.default_mode`
fn (mut p Parser) enum_val() ast.EnumVal {
	start_pos := p.tok.pos()
	p.check(.dot)
	val := p.check_name()
	return ast.EnumVal{
		val: val
		pos: start_pos.extend(p.prev_tok.pos())
	}
}

fn (mut p Parser) filter_string_vet_errors(pos token.Pos) {
	if p.vet_errors.len == 0 {
		return
	}
	p.vet_errors = p.vet_errors.filter(
		(it.typ == .trailing_space && it.pos.line_nr - 1 >= pos.last_line)
		|| (it.typ != .trailing_space && it.pos.line_nr - 1 > pos.last_line)
		|| (it.typ == .space_indent && it.pos.line_nr - 1 <= pos.line_nr)
		|| (it.typ != .space_indent && it.pos.line_nr - 1 < pos.line_nr))
}

fn (mut p Parser) string_expr() ast.Expr {
	is_raw := p.tok.kind == .name && p.tok.lit == 'r'
	is_cstr := p.tok.kind == .name && p.tok.lit == 'c'
	if is_raw || is_cstr {
		p.next()
	}
	mut node := ast.empty_expr
	val := p.tok.lit
	mut pos := p.tok.pos()
	pos.last_line = pos.line_nr + val.count('\n')
	if p.peek_tok.kind != .str_dollar {
		p.next()
		p.filter_string_vet_errors(pos)
		node = ast.StringLiteral{
			val: val
			is_raw: is_raw
			language: if is_cstr { ast.Language.c } else { ast.Language.v }
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
	mut fmts := []u8{}
	mut fposs := []token.Pos{}
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
		// 987698 is a magic default value, unlikely to be present in user input. Note: 0 is valid precision
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
					return p.error('format specifier may only be one letter')
				}
			}
		}
		fwidths << fwidth
		has_fmts << has_fmt
		precisions << precision
		visible_pluss << visible_plus
		fmts << fmt
		fills << fill
		fposs << p.prev_tok.pos()
	}
	pos = pos.extend(p.prev_tok.pos())
	p.filter_string_vet_errors(pos)
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
	mut pos := p.tok.pos()
	is_neg := p.tok.kind == .minus
	if is_neg {
		p.next()
		pos = pos.extend(p.tok.pos())
	}
	lit := p.tok.lit
	full_lit := if is_neg { '-' + lit } else { lit }
	mut node := ast.empty_expr
	if lit.index_any('.eE') >= 0 && lit[..2] !in ['0x', '0X', '0o', '0O', '0b', '0B'] {
		node = ast.FloatLiteral{
			val: full_lit
			pos: pos
		}
	} else {
		node = ast.IntegerLiteral{
			val: full_lit
			pos: pos
		}
	}
	p.next()
	return node
}

fn (mut p Parser) module_decl() ast.Module {
	mut module_attrs := []ast.Attr{}
	mut attrs_pos := p.tok.pos()
	for p.tok.kind == .lsbr {
		p.attributes()
	}
	module_attrs << p.attrs
	mut name := 'main'
	mut module_pos := token.Pos{}
	mut name_pos := token.Pos{}
	mut mod_node := ast.Module{}
	is_skipped := p.tok.kind != .key_module
	if is_skipped {
		// the attributes were for something else != module, like a struct/fn/type etc.
		module_attrs = []
	} else {
		p.attrs = []
		module_pos = p.tok.pos()
		p.next()
		name_pos = p.tok.pos()
		name = p.check_name()
		mod_node = ast.Module{
			pos: module_pos
		}
		if module_pos.line_nr != name_pos.line_nr {
			p.error_with_pos('`module` and `$name` must be at same line', name_pos)
			return mod_node
		}
		// Note: this shouldn't be reassigned into name_pos
		// as it creates a wrong position when extended
		// to module_pos
		n_pos := p.tok.pos()
		if module_pos.line_nr == n_pos.line_nr && p.tok.kind != .comment && p.tok.kind != .eof {
			if p.tok.kind == .name {
				p.unexpected_with_pos(n_pos,
					prepend_msg: '`module $name`, you can only declare one module,'
					got: '`$p.tok.lit`'
				)
				return mod_node
			} else {
				p.unexpected_with_pos(n_pos,
					prepend_msg: '`module $name`,'
					got: '`$p.tok.kind` after module name'
				)
				return mod_node
			}
		}
		module_pos = attrs_pos.extend(name_pos)
	}
	full_name := util.qualify_module(p.pref, name, p.file_name)
	p.mod = full_name
	p.builtin_mod = p.mod == 'builtin'
	mod_node = ast.Module{
		name: full_name
		short_name: name
		attrs: module_attrs
		is_skipped: is_skipped
		pos: module_pos
		name_pos: name_pos
	}
	if !is_skipped {
		p.table.module_attrs[p.mod] = module_attrs
		for ma in module_attrs {
			match ma.name {
				'deprecated', 'deprecated_after' {
					p.table.module_deprecated[p.mod] = true
				}
				'manualfree' {
					p.is_manualfree = true
				}
				'generated' {
					p.is_generated = true
				}
				'has_globals' {
					if p.inside_vlib_file {
						p.has_globals = true
					} else {
						p.error_with_pos('[has_globals] is allowed only in .v files of `vlib` modules',
							ma.pos)
					}
				}
				'translated' {
					p.is_translated = true
				}
				else {
					p.error_with_pos('unknown module attribute `[$ma.name]`', ma.pos)
					return mod_node
				}
			}
		}
	}
	return mod_node
}

fn (mut p Parser) import_stmt() ast.Import {
	import_pos := p.tok.pos()
	p.check(.key_import)
	mut pos := p.tok.pos()
	mut import_node := ast.Import{
		pos: import_pos.extend(pos)
	}
	if p.tok.kind == .lpar {
		p.error_with_pos('`import()` has been deprecated, use `import x` instead', pos)
		return import_node
	}
	mut mod_name_arr := []string{}
	mod_name_arr << p.check_name()
	if import_pos.line_nr != pos.line_nr {
		p.error_with_pos('`import` statements must be a single line', pos)
		return import_node
	}
	mut mod_alias := mod_name_arr[0]
	import_node = ast.Import{
		pos: import_pos.extend(pos)
		mod_pos: pos
		alias_pos: pos
	}
	for p.tok.kind == .dot {
		p.next()
		submod_pos := p.tok.pos()
		if p.tok.kind != .name {
			p.error_with_pos('module syntax error, please use `x.y.z`', submod_pos)
			return import_node
		}
		if import_pos.line_nr != submod_pos.line_nr {
			p.error_with_pos('`import` and `submodule` must be at same line', submod_pos)
			return import_node
		}
		submod_name := p.check_name()
		mod_name_arr << submod_name
		mod_alias = submod_name
		pos = pos.extend(submod_pos)
		import_node = ast.Import{
			pos: import_pos.extend(pos)
			mod_pos: pos
			alias_pos: submod_pos
			mod: util.qualify_import(p.pref, mod_name_arr.join('.'), p.file_name)
			alias: mod_alias
		}
	}
	if mod_name_arr.len == 1 {
		import_node = ast.Import{
			pos: import_node.pos
			mod_pos: import_node.mod_pos
			alias_pos: import_node.alias_pos
			mod: util.qualify_import(p.pref, mod_name_arr[0], p.file_name)
			alias: mod_alias
		}
	}
	mod_name := import_node.mod
	if p.tok.kind == .key_as {
		p.next()
		alias_pos := p.tok.pos()
		mod_alias = p.check_name()
		if mod_alias == mod_name_arr.last() {
			p.error_with_pos('import alias `$mod_name as $mod_alias` is redundant', p.prev_tok.pos())
			return import_node
		}
		import_node = ast.Import{
			pos: import_node.pos.extend(alias_pos)
			mod_pos: import_node.mod_pos
			alias_pos: alias_pos
			mod: import_node.mod
			alias: mod_alias
		}
	}
	if p.tok.kind == .lcbr { // import module { fn1, Type2 } syntax
		mut initial_syms_pos := p.tok.pos()
		p.import_syms(mut import_node)
		initial_syms_pos = initial_syms_pos.extend(p.tok.pos())
		import_node = ast.Import{
			...import_node
			syms_pos: initial_syms_pos
			pos: import_node.pos.extend(initial_syms_pos)
		}
		p.register_used_import(mod_alias) // no `unused import` msg for parent
	}
	pos_t := p.tok.pos()
	if import_pos.line_nr == pos_t.line_nr {
		if p.tok.kind !in [.lcbr, .eof, .comment] {
			p.error_with_pos('cannot import multiple modules at a time', pos_t)
			return import_node
		}
	}
	import_node.comments = p.eat_comments(same_line: true)
	import_node.next_comments = p.eat_comments(follow_up: true)
	p.imports[mod_alias] = mod_name
	// if mod_name !in p.table.imports {
	p.table.imports << mod_name
	p.ast_imports << import_node
	// }
	return import_node
}

// import_syms parses the inner part of `import module { submod1, submod2 }`
fn (mut p Parser) import_syms(mut parent ast.Import) {
	p.next()
	pos_t := p.tok.pos()
	if p.tok.kind == .rcbr { // closed too early
		p.error_with_pos('empty `$parent.mod` import set, remove `{}`', pos_t)
		return
	}
	if p.tok.kind != .name { // not a valid inner name
		p.error_with_pos('import syntax error, please specify a valid fn or type name',
			pos_t)
		return
	}
	for p.tok.kind == .name {
		pos := p.tok.pos()
		alias := p.check_name()
		p.imported_symbols[alias] = parent.mod + '.' + alias
		// so we can work with this in fmt+checker
		parent.syms << ast.ImportSymbol{
			pos: pos
			name: alias
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
		p.error_with_pos('import syntax error, no closing `}`', p.tok.pos())
		return
	}
	p.next()
}

fn (mut p Parser) const_decl() ast.ConstDecl {
	p.top_level_statement_start()
	mut attrs := []ast.Attr{}
	if p.attrs.len > 0 {
		attrs = p.attrs.clone()
		p.attrs = []
	}
	mut is_markused := false
	for ga in attrs {
		match ga.name {
			'markused' { is_markused = true }
			else {}
		}
	}
	start_pos := p.tok.pos()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	const_pos := p.tok.pos()
	if p.disallow_declarations_in_script_mode() {
		return ast.ConstDecl{}
	}
	p.check(.key_const)
	is_block := p.tok.kind == .lpar
	if is_block {
		p.next() // (
	}
	mut fields := []ast.ConstField{}
	mut comments := []ast.Comment{}
	mut end_comments := []ast.Comment{}
	for {
		comments = p.eat_comments()
		if is_block && p.tok.kind == .eof {
			p.unexpected(got: 'eof', expecting: '´)´')
			return ast.ConstDecl{}
		}
		if p.tok.kind == .rpar {
			break
		}
		pos := p.tok.pos()
		name := p.check_name()
		end_comments << p.eat_comments()
		if util.contains_capital(name) {
			p.warn_with_pos('const names cannot contain uppercase letters, use snake_case instead',
				pos)
		}
		full_name := p.prepend_mod(name)
		if p.tok.kind == .comma {
			p.error_with_pos('const declaration do not support multiple assign yet', p.tok.pos())
		}
		p.check(.assign)
		end_comments << p.eat_comments()
		if p.tok.kind == .key_fn {
			p.error('const initializer fn literal is not a constant')
			return ast.ConstDecl{}
		}
		if p.tok.kind == .eof {
			p.unexpected(got: 'eof', expecting: 'an expression')
			return ast.ConstDecl{}
		}
		expr := p.expr(0)
		field := ast.ConstField{
			name: full_name
			mod: p.mod
			is_pub: is_pub
			expr: expr
			pos: pos.extend(expr.pos())
			comments: comments
			end_comments: end_comments
			is_markused: is_markused
		}
		fields << field
		p.table.global_scope.register(field)
		comments = []
		if !is_block {
			break
		}
	}
	p.top_level_statement_end()
	if is_block {
		p.check(.rpar)
	} else {
		comments << p.eat_comments(same_line: true)
	}
	return ast.ConstDecl{
		pos: start_pos.extend_with_last_line(const_pos, p.prev_tok.line_nr)
		fields: fields
		is_pub: is_pub
		end_comments: comments
		is_block: is_block
		attrs: attrs
	}
}

fn (mut p Parser) return_stmt() ast.Return {
	first_pos := p.tok.pos()
	p.next()
	// no return
	mut comments := p.eat_comments()
	if p.tok.kind == .rcbr || (p.tok.kind == .name && p.peek_tok.kind == .colon) {
		return ast.Return{
			comments: comments
			pos: first_pos
		}
	}
	// return exprs
	exprs, comments2 := p.expr_list()
	comments << comments2
	end_pos := exprs.last().pos()
	return ast.Return{
		exprs: exprs
		comments: comments
		pos: first_pos.extend(end_pos)
	}
}

// left hand side of `=` or `:=` in `a,b,c := 1,2,3`
fn (mut p Parser) global_decl() ast.GlobalDecl {
	mut attrs := []ast.Attr{}
	if p.attrs.len > 0 {
		attrs = p.attrs.clone()
		p.attrs = []
	}

	mut is_markused := false
	for ga in attrs {
		match ga.name {
			'markused' { is_markused = true }
			else {}
		}
	}

	if !p.has_globals && !p.pref.enable_globals && !p.pref.is_fmt && !p.pref.translated
		&& !p.is_translated && !p.pref.is_livemain && !p.pref.building_v && !p.builtin_mod {
		p.error('use `v -enable-globals ...` to enable globals')
		return ast.GlobalDecl{}
	}
	start_pos := p.tok.pos()
	p.check(.key_global)
	if p.disallow_declarations_in_script_mode() {
		return ast.GlobalDecl{}
	}
	is_block := p.tok.kind == .lpar
	if is_block {
		p.next() // (
	}
	mut fields := []ast.GlobalField{}
	mut comments := []ast.Comment{}
	for {
		comments = p.eat_comments()
		is_volatile := p.tok.kind == .key_volatile
		if is_volatile {
			p.next()
		}
		if is_block && p.tok.kind == .eof {
			p.unexpected(got: 'eof', expecting: '`)`')
			return ast.GlobalDecl{}
		}
		if p.tok.kind == .rpar {
			break
		}
		pos := p.tok.pos()
		name := p.check_name()
		has_expr := p.tok.kind == .assign
		mut expr := ast.empty_expr
		mut typ := ast.void_type
		mut typ_pos := token.Pos{}
		if has_expr {
			p.next() // =
			expr = p.expr(0)
			match mut expr {
				ast.CastExpr, ast.StructInit, ast.ArrayInit, ast.ChanInit {
					typ = expr.typ
				}
				ast.BoolLiteral, ast.IsRefType {
					typ = ast.bool_type
				}
				ast.CharLiteral {
					typ = ast.char_type
				}
				ast.FloatLiteral {
					typ = ast.f64_type
				}
				ast.IntegerLiteral, ast.SizeOf {
					typ = ast.int_type
				}
				ast.StringLiteral, ast.StringInterLiteral {
					typ = ast.string_type
				}
				else {
					// type will be deduced by checker
				}
			}
		} else {
			typ_pos = p.tok.pos()
			typ = p.parse_type()
		}
		field := ast.GlobalField{
			name: name
			has_expr: has_expr
			expr: expr
			pos: pos
			typ_pos: typ_pos
			typ: typ
			comments: comments
			is_markused: is_markused
			is_volatile: is_volatile
		}
		fields << field
		p.table.global_scope.register(field)
		comments = []
		if !is_block {
			break
		}
	}
	if is_block {
		p.check(.rpar)
	}
	return ast.GlobalDecl{
		pos: start_pos.extend(p.prev_tok.pos())
		mod: p.mod
		fields: fields
		end_comments: comments
		is_block: is_block
		attrs: attrs
	}
}

fn (mut p Parser) enum_decl() ast.EnumDecl {
	p.top_level_statement_start()
	is_pub := p.tok.kind == .key_pub
	start_pos := p.tok.pos()
	if is_pub {
		p.next()
	}
	p.check(.key_enum)
	end_pos := p.tok.pos()
	if p.disallow_declarations_in_script_mode() {
		return ast.EnumDecl{}
	}
	enum_name := p.check_name()
	if enum_name.len == 1 {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			end_pos)
		return ast.EnumDecl{}
	}
	if enum_name in p.imported_symbols {
		p.error_with_pos('cannot register enum `$enum_name`, this type was already imported',
			end_pos)
		return ast.EnumDecl{}
	}
	name := p.prepend_mod(enum_name)
	p.check(.lcbr)
	enum_decl_comments := p.eat_comments()
	mut vals := []string{}
	// mut default_exprs := []ast.Expr{}
	mut fields := []ast.EnumField{}
	mut uses_exprs := false
	for p.tok.kind != .eof && p.tok.kind != .rcbr {
		pos := p.tok.pos()
		val := p.check_name()
		vals << val
		mut expr := ast.empty_expr
		mut has_expr := false
		// p.warn('enum val $val')
		if p.tok.kind == .assign {
			p.next()
			expr = p.expr(0)
			has_expr = true
			uses_exprs = true
		}
		fields << ast.EnumField{
			name: val
			pos: pos
			expr: expr
			has_expr: has_expr
			comments: p.eat_comments(same_line: true)
			next_comments: p.eat_comments()
		}
	}
	p.top_level_statement_end()
	p.check(.rcbr)
	is_flag := p.attrs.contains('flag')
	is_multi_allowed := p.attrs.contains('_allow_multiple_values')
	if is_flag {
		if fields.len > 32 {
			p.error('when an enum is used as bit field, it must have a max of 32 fields')
			return ast.EnumDecl{}
		}
		for f in fields {
			if f.has_expr {
				p.error_with_pos('when an enum is used as a bit field, you can not assign custom values',
					f.pos)
				return ast.EnumDecl{}
			}
		}
		pubfn := if p.mod == 'main' { 'fn' } else { 'pub fn' }
		p.codegen('
//
[inline] $pubfn (    e &$enum_name) is_empty() bool           { return  int(*e) == 0 }
[inline] $pubfn (    e &$enum_name) has(flag $enum_name) bool { return  (int(*e) &  (int(flag))) != 0 }
[inline] $pubfn (    e &$enum_name) all(flag $enum_name) bool { return  (int(*e) &  (int(flag))) == int(flag) }
[inline] $pubfn (mut e  $enum_name) set(flag $enum_name)      { unsafe{ *e = ${enum_name}(int(*e) |  (int(flag))) } }
[inline] $pubfn (mut e  $enum_name) clear(flag $enum_name)    { unsafe{ *e = ${enum_name}(int(*e) & ~(int(flag))) } }
[inline] $pubfn (mut e  $enum_name) toggle(flag $enum_name)   { unsafe{ *e = ${enum_name}(int(*e) ^  (int(flag))) } }
//
')
	}
	idx := p.table.register_sym(ast.TypeSymbol{
		kind: .enum_
		name: name
		cname: util.no_dots(name)
		mod: p.mod
		info: ast.Enum{
			vals: vals
			is_flag: is_flag
			is_multi_allowed: is_multi_allowed
			uses_exprs: uses_exprs
		}
		is_pub: is_pub
	})
	if idx == -1 {
		p.error_with_pos('cannot register enum `$name`, another type with this name exists',
			end_pos)
	}

	enum_decl := ast.EnumDecl{
		name: name
		is_pub: is_pub
		is_flag: is_flag
		is_multi_allowed: is_multi_allowed
		fields: fields
		pos: start_pos.extend_with_last_line(end_pos, p.prev_tok.line_nr)
		attrs: p.attrs
		comments: enum_decl_comments
	}

	p.table.register_enum_decl(enum_decl)

	return enum_decl
}

fn (mut p Parser) type_decl() ast.TypeDecl {
	start_pos := p.tok.pos()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_type)
	end_pos := p.tok.pos()
	decl_pos := start_pos.extend(end_pos)
	name_pos := p.tok.pos()
	if p.disallow_declarations_in_script_mode() {
		return ast.SumTypeDecl{}
	}
	name := p.check_name()
	if name.len == 1 && name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types',
			name_pos)
		return ast.FnTypeDecl{}
	}
	if name in p.imported_symbols {
		p.error_with_pos('cannot register alias `$name`, this type was already imported',
			end_pos)
		return ast.AliasTypeDecl{}
	}
	mut sum_variants := []ast.TypeNode{}
	generic_types, _ := p.parse_generic_types()
	decl_pos_with_generics := decl_pos.extend(p.prev_tok.pos())
	p.check(.assign)
	mut type_pos := p.tok.pos()
	mut comments := []ast.Comment{}
	if p.tok.kind == .key_fn && p.is_fn_type_decl() {
		// function type: `type mycallback = fn(string, int)`
		fn_name := p.prepend_mod(name)
		fn_type := p.parse_fn_type(fn_name)
		p.table.sym(fn_type).is_pub = is_pub
		type_pos = type_pos.extend(p.tok.pos())
		comments = p.eat_comments(same_line: true)
		attrs := p.attrs
		p.attrs = []
		return ast.FnTypeDecl{
			name: fn_name
			is_pub: is_pub
			typ: fn_type
			pos: decl_pos
			type_pos: type_pos
			comments: comments
			attrs: attrs
		}
	}
	sum_variants << p.parse_sum_type_variants()
	// type SumType = Aaa | Bbb | Ccc
	if sum_variants.len > 1 {
		for variant in sum_variants {
			variant_sym := p.table.sym(variant.typ)
			// TODO: implement this check for error too
			if variant_sym.kind == .none_ {
				p.error_with_pos('named sum type cannot have none as its variant', variant.pos)
				return ast.AliasTypeDecl{}
			}
		}
		variant_types := sum_variants.map(it.typ)
		prepend_mod_name := p.prepend_mod(name)
		typ := p.table.register_sym(ast.TypeSymbol{
			kind: .sum_type
			name: prepend_mod_name
			cname: util.no_dots(prepend_mod_name)
			mod: p.mod
			info: ast.SumType{
				variants: variant_types
				is_generic: generic_types.len > 0
				generic_types: generic_types
			}
			is_pub: is_pub
		})
		if typ == ast.invalid_type_idx {
			p.error_with_pos('cannot register sum type `$name`, another type with this name exists',
				name_pos)
			return ast.SumTypeDecl{}
		}
		comments = p.eat_comments(same_line: true)
		return ast.SumTypeDecl{
			name: name
			typ: typ
			is_pub: is_pub
			variants: sum_variants
			generic_types: generic_types
			attrs: p.attrs
			pos: decl_pos
			name_pos: name_pos
			comments: comments
		}
	}
	// type MyType = int
	if generic_types.len > 0 {
		p.error_with_pos('generic type aliases are not yet implemented', decl_pos_with_generics)
		return ast.AliasTypeDecl{}
	}
	// sum_variants will have only one element
	parent_type := sum_variants[0].typ
	parent_sym := p.table.sym(parent_type)
	pidx := parent_type.idx()
	p.check_for_impure_v(parent_sym.language, decl_pos)
	prepend_mod_name := p.prepend_mod(name)
	idx := p.table.register_sym(ast.TypeSymbol{
		kind: .alias
		name: prepend_mod_name
		cname: util.no_dots(prepend_mod_name)
		mod: p.mod
		parent_idx: pidx
		info: ast.Alias{
			parent_type: parent_type
			language: parent_sym.language
		}
		is_pub: is_pub
	})
	type_end_pos := p.prev_tok.pos()
	if idx == ast.invalid_type_idx {
		p.error_with_pos('cannot register alias `$name`, another type with this name exists',
			name_pos)
		return ast.AliasTypeDecl{}
	}
	if idx == pidx {
		type_alias_pos := sum_variants[0].pos
		p.error_with_pos('a type alias can not refer to itself: $name', decl_pos.extend(type_alias_pos))
		return ast.AliasTypeDecl{}
	}
	comments = p.eat_comments(same_line: true)
	return ast.AliasTypeDecl{
		name: name
		is_pub: is_pub
		parent_type: parent_type
		type_pos: type_pos.extend(type_end_pos)
		pos: decl_pos
		comments: comments
	}
}

fn (mut p Parser) assoc() ast.Assoc {
	var_name := p.check_name()
	pos := p.tok.pos()
	mut v := p.scope.find_var(var_name) or {
		p.error('unknown variable `$var_name`')
		return ast.Assoc{
			scope: 0
		}
	}
	v.is_used = true
	mut fields := []string{}
	mut vals := []ast.Expr{}
	p.check(.pipe)
	for p.tok.kind != .eof {
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
		scope: p.scope
	}
}

fn (p &Parser) new_true_expr() ast.Expr {
	return ast.BoolLiteral{
		val: true
		pos: p.tok.pos()
	}
}

[noreturn]
fn verror(s string) {
	util.verror('parser error', s)
}

fn (mut p Parser) top_level_statement_start() {
	if p.comments_mode == .toplevel_comments {
		p.scanner.set_is_inside_toplevel_statement(true)
		p.rewind_scanner_to_current_token_in_new_mode()
		$if debugscanner ? {
			eprintln('>> p.top_level_statement_start | tidx:${p.tok.tidx:-5} | p.tok.kind: ${p.tok.kind:-10} | p.tok.lit: $p.tok.lit $p.peek_tok.lit ${p.peek_token(2).lit} ${p.peek_token(3).lit} ...')
		}
	}
}

fn (mut p Parser) top_level_statement_end() {
	if p.comments_mode == .toplevel_comments {
		p.scanner.set_is_inside_toplevel_statement(false)
		p.rewind_scanner_to_current_token_in_new_mode()
		$if debugscanner ? {
			eprintln('>> p.top_level_statement_end   | tidx:${p.tok.tidx:-5} | p.tok.kind: ${p.tok.kind:-10} | p.tok.lit: $p.tok.lit $p.peek_tok.lit ${p.peek_token(2).lit} ${p.peek_token(3).lit} ...')
		}
	}
}

fn (mut p Parser) rewind_scanner_to_current_token_in_new_mode() {
	// Go back and rescan some tokens, ensuring that the parser's
	// lookahead buffer p.peek_tok .. p.peek_token(3), will now contain
	// the correct tokens (possible comments), for the new mode
	// This refilling of the lookahead buffer is needed for the
	// .toplevel_comments parsing mode.
	tidx := p.tok.tidx
	p.scanner.set_current_tidx(tidx - 5)
	no_token := token.Token{}
	p.prev_tok = no_token
	p.tok = no_token
	p.peek_tok = no_token // requires 2 calls p.next() or check p.tok.kind != token.Kind.unknown
	p.next()
	for {
		p.next()
		// eprintln('rewinding to ${p.tok.tidx:5} | goal: ${tidx:5}')
		if tidx == p.tok.tidx {
			break
		}
	}
}

// returns true if `varname` is known
pub fn (mut p Parser) mark_var_as_used(varname string) bool {
	if mut obj := p.scope.find(varname) {
		match mut obj {
			ast.Var {
				obj.is_used = true
				return true
			}
			ast.GlobalField {
				// obj.is_used = true
				return true
			}
			// ast.ConstField {
			// return true
			//}
			else {}
		}
	}
	return false
}

fn (mut p Parser) unsafe_stmt() ast.Stmt {
	mut pos := p.tok.pos()
	p.next()
	if p.tok.kind != .lcbr {
		return p.error_with_pos('please use `unsafe {`', p.tok.pos())
	}
	p.next()
	if p.inside_unsafe {
		return p.error_with_pos('already inside `unsafe` block', pos)
	}
	if p.tok.kind == .rcbr {
		// `unsafe {}`
		pos.update_last_line(p.tok.line_nr)
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
				pos.update_last_line(p.prev_tok.line_nr)
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
	pos.update_last_line(p.tok.line_nr)
	return ast.Block{
		stmts: stmts
		is_unsafe: true
		pos: pos
	}
}

fn (mut p Parser) disallow_declarations_in_script_mode() bool {
	if p.script_mode {
		p.note_with_pos('script mode started here', p.script_mode_start_token.pos())
		p.error_with_pos('all definitions must occur before code in script mode', p.tok.pos())
		return true
	}
	return false
}

fn (mut p Parser) trace(fbase string, message string) {
	if p.file_base == fbase {
		println('> p.trace | ${fbase:-10s} | $message')
	}
}
