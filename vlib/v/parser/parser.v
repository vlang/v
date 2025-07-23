// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.scanner
import v.ast
import v.token
import v.pref
import v.util
import v.errors
import os
import hash.fnv1a

@[minify]
pub struct Parser {
pub:
	pref &pref.Preferences = unsafe { nil }
mut:
	file_base         string       // "hello.v"
	file_path         string       // "/home/user/hello.v"
	file_display_path string       // just "hello.v", when your current folder for the compilation is "/home/user/", otherwise the full path "/home/user/hello.v"
	unique_prefix     string       // a hash of p.file_path, used for making anon fn generation unique
	file_backend_mode ast.Language // .c for .c.v|.c.vv|.c.vsh files; .js for .js.v files, .amd64/.rv32/other arches for .amd64.v/.rv32.v/etc. files, .v otherwise.
	// see comment in parse_file
	tok                      token.Token
	prev_tok                 token.Token
	peek_tok                 token.Token
	language                 ast.Language
	fn_language              ast.Language // .c for `fn C.abcd()` declarations
	expr_level               int          // prevent too deep recursions for pathological programs
	inside_vlib_file         bool         // true for all vlib/ files
	inside_test_file         bool         // when inside _test.v or _test.vv file
	inside_if                bool
	inside_comptime_if       bool
	inside_if_expr           bool
	inside_if_cond           bool
	inside_ct_if_expr        bool
	inside_or_expr           bool
	inside_for               bool
	inside_for_expr          bool
	inside_fn                bool // true even with implicit main
	inside_fn_return         bool
	inside_fn_concrete_type  bool // parsing fn_name[concrete_type]() call expr
	inside_call_args         bool // true inside f(  ....  )
	inside_unsafe_fn         bool
	inside_str_interp        bool
	inside_array_lit         bool
	inside_in_array          bool
	inside_infix             bool
	inside_assign_rhs        bool // rhs assignment
	inside_match             bool // to separate `match A { }` from `Struct{}`
	inside_select            bool // to allow `ch <- Struct{} {` inside `select`
	inside_match_case        bool // to separate `match_expr { }` from `Struct{}`
	inside_match_body        bool // to fix eval not used TODO
	inside_unsafe            bool
	inside_sum_type          bool // to prevent parsing inline sum type again
	inside_asm_template      bool
	inside_asm               bool
	inside_defer             bool
	inside_generic_params    bool // indicates if parsing between `<` and `>` of a method/function
	inside_receiver_param    bool // indicates if parsing the receiver parameter inside the first `(` and `)` of a method
	inside_struct_field_decl bool
	inside_struct_attr_decl  bool
	inside_map_init          bool
	inside_orm               bool
	inside_chan_decl         bool
	inside_attr_decl         bool
	array_dim                int               // array dim parsing level
	fixed_array_dim          int               // fixed array dim parsing level
	or_is_handled            bool              // ignore `or` in this expression
	builtin_mod              bool              // are we in the `builtin` module?
	mod                      string            // current module name
	is_manualfree            bool              // true when `@[manualfree] module abc`, makes *all* fns in the current .v file, opt out of autofree
	has_globals              bool              // `@[has_globals] module abc` - allow globals declarations, even without -enable-globals, in that single .v file __only__
	is_generated             bool              // `@[generated] module abc` - turn off compiler notices for that single .v file __only__.
	is_translated            bool              // `@[translated] module abc` - mark a file as translated, to relax some compiler checks for translated code.
	attrs                    []ast.Attr        // attributes before next decl stmt
	expr_mod                 string            // for constructing full type names in parse_type()
	last_enum_name           string            // saves the last enum name on an array initialization
	last_enum_mod            string            // saves the last enum mod name on an array initialization
	imports                  map[string]string // alias => mod_name
	ast_imports              []ast.Import      // mod_names
	used_imports             []string          // alias
	auto_imports             []string          // imports, the user does not need to specify
	imported_symbols         map[string]string
	is_amp                   bool // for generating the right code for `&Foo{}`
	returns                  bool
	is_stmt_ident            bool // true while the beginning of a statement is an ident/selector
	expecting_type           bool // `is Type`, expecting type
	expecting_value          bool = true // true where a node value will be used
	cur_fn_name              string
	cur_fn_scope             &ast.Scope = unsafe { nil }
	label_names              []string
	name_error               bool // indicates if the token is not a name or the name is on another line
	n_asm                    int  // controls assembly labels
	global_labels            []string
	comptime_if_cond         bool
	defer_vars               []ast.Ident
	should_abort             bool // when too many errors/warnings/notices are accumulated, should_abort becomes true, and the parser should stop
	codegen_text             string
	anon_struct_decl         ast.StructDecl
	init_generic_types       []ast.Type
	if_cond_comments         []ast.Comment
	left_comments            []ast.Comment
	script_mode              bool
	script_mode_start_token  token.Token
	generic_type_level       int  // to avoid infinite recursion segfaults due to compiler bugs in ensure_type_exists
	main_already_defined     bool // TODO move to checker
	is_vls                   bool
pub mut:
	scanner &scanner.Scanner = unsafe { nil }
	table   &ast.Table       = unsafe { nil }
	scope   &ast.Scope       = unsafe { nil }

	opened_scopes     int
	max_opened_scopes int = 100 // values above 300 risk stack overflow

	errors         []errors.Error
	warnings       []errors.Warning
	notices        []errors.Notice
	template_paths []string // record all compiled $tmpl files; needed for `v watch run webserver.v`
}

// for tests
pub fn parse_stmt(text string, mut table ast.Table, mut scope ast.Scope) ast.Stmt {
	$if trace_parse_stmt ? {
		eprintln('> ${@MOD}.${@FN} text: ${text}')
	}
	mut p := Parser{
		scanner:          scanner.new_scanner(text, .skip_comments, &pref.Preferences{})
		inside_test_file: true
		table:            table
		pref:             &pref.Preferences{}
		scope:            scope
	}
	p.init_parse_fns()
	util.timing_start('PARSE stmt')
	defer {
		util.timing_measure_cumulative('PARSE stmt')
	}
	p.read_first_token()
	return p.stmt(false)
}

pub fn parse_comptime(tmpl_path string, text string, mut table ast.Table, pref_ &pref.Preferences, mut scope ast.Scope) &ast.File {
	$if trace_parse_comptime ? {
		eprintln('> ${@MOD}.${@FN} text: ${text}')
	}
	mut p := Parser{
		file_path: tmpl_path
		scanner:   scanner.new_scanner(text, .skip_comments, pref_)
		table:     table
		pref:      pref_
		scope:     scope
		errors:    []errors.Error{}
		warnings:  []errors.Warning{}
	}
	mut res := p.parse()
	unsafe { p.free_scanner() }
	res.is_template_text = true
	return res
}

pub fn parse_text(text string, path string, mut table ast.Table, comments_mode scanner.CommentsMode, pref_ &pref.Preferences) &ast.File {
	$if trace_parse_text ? {
		eprintln('> ${@MOD}.${@FN} comments_mode: ${comments_mode:-20} | path: ${path:-20} | text: ${text}')
	}
	mut p := Parser{
		scanner:  scanner.new_scanner(text, comments_mode, pref_)
		table:    table
		pref:     pref_
		is_vls:   pref_.is_vls
		scope:    &ast.Scope{
			start_pos: 0
			parent:    table.global_scope
		}
		errors:   []errors.Error{}
		warnings: []errors.Warning{}
	}
	p.set_path(path)
	mut res := p.parse()
	unsafe { p.free_scanner() }
	res.is_parse_text = true
	return res
}

@[unsafe]
pub fn (mut p Parser) free() {
	unsafe { p.free_scanner() }
}

@[unsafe]
fn (mut p Parser) free_scanner() {
	unsafe {
		if p.scanner != 0 {
			p.scanner.free()
			p.scanner = &scanner.Scanner(nil)
		}
	}
}

const normalised_working_folder = (os.real_path(os.getwd()) + os.path_separator).replace('\\',
	'/')

pub fn (mut p Parser) set_path(path string) {
	p.file_path = path
	p.file_base = os.base(path)
	p.file_display_path = os.real_path(p.file_path).replace_once(normalised_working_folder,
		'').replace('\\', '/')
	p.inside_vlib_file = os.dir(path).contains('vlib')
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

pub fn parse_file(path string, mut table ast.Table, comments_mode scanner.CommentsMode, pref_ &pref.Preferences) &ast.File {
	// Note: when comments_mode == .toplevel_comments,
	// the parser gives feedback to the scanner about toplevel statements, so that the scanner can skip
	// all the tricky inner comments. This is needed because we do not have a good general solution
	// for handling them, and should be removed when we do (the general solution is also needed for vfmt)
	$if trace_parse_file ? {
		eprintln('> ${@MOD}.${@FN} comments_mode: ${comments_mode:-20} | path: ${path}')
	}
	mut p := Parser{
		scanner:  scanner.new_scanner_file(path, comments_mode, pref_) or { panic(err) }
		table:    table
		pref:     pref_
		scope:    &ast.Scope{
			start_pos: 0
			parent:    table.global_scope
		}
		errors:   []errors.Error{}
		warnings: []errors.Warning{}
	}
	p.set_path(path)
	res := p.parse()
	unsafe { p.free_scanner() }
	return res
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
		if !(stmt is ast.ExprStmt && stmt.expr is ast.Comment) {
			p.attrs = []
		}
		stmts << stmt
		if p.should_abort {
			break
		}
	}
	p.scope.end_pos = p.tok.pos

	mut errors_ := p.errors.clone()
	mut warnings := p.warnings.clone()
	mut notices := p.notices.clone()

	if p.pref.check_only {
		errors_ << p.scanner.errors
		warnings << p.scanner.warnings
		notices << p.scanner.notices
	}

	p.handle_codegen_for_file()

	ast_file := &ast.File{
		path:             p.file_path
		path_base:        p.file_base
		is_test:          p.inside_test_file
		is_generated:     p.is_generated
		is_translated:    p.is_translated
		language:         p.file_backend_mode
		nr_lines:         p.scanner.line_nr
		nr_bytes:         p.scanner.text.len
		nr_tokens:        p.scanner.all_tokens.len
		mod:              module_decl
		imports:          p.ast_imports
		imported_symbols: p.imported_symbols
		auto_imports:     p.auto_imports
		stmts:            stmts
		scope:            p.scope
		global_scope:     p.table.global_scope
		errors:           errors_
		warnings:         warnings
		notices:          notices
		global_labels:    p.global_labels
		template_paths:   p.template_paths
		unique_prefix:    p.unique_prefix
	}
	$if trace_parse_file_path_and_mod ? {
		eprintln('>> ast.File, tokens: ${ast_file.nr_tokens:5}, mname: ${ast_file.mod.name:20}, sname: ${ast_file.mod.short_name:11}, path: ${p.file_display_path}')
	}
	return ast_file
}

pub fn parse_files(paths []string, mut table ast.Table, pref_ &pref.Preferences) []&ast.File {
	mut timers := util.new_timers(should_print: false, label: 'parse_files: ${paths}')
	$if time_parsing ? {
		timers.should_print = true
	}
	unsafe {
		mut files := []&ast.File{cap: paths.len}
		for path in paths {
			timers.start('parse_file ${path}')
			files << parse_file(path, mut table, .skip_comments, pref_)
			timers.show('parse_file ${path}')
		}
		handle_codegen_for_multiple_files(mut files)
		return files
	}
}

fn (mut p Parser) init_parse_fns() {
	// p.prefix_parse_fns = make(100, 100, sizeof(PrefixParseFn))
	// p.prefix_parse_fns[token.Kind.name] = parse_name
}

fn (mut p Parser) read_first_token() {
	// need to call next() 2 times to get peek token and current token
	p.next()
	p.next()
}

@[inline]
fn (p &Parser) peek_token(n int) token.Token {
	return p.scanner.peek_token(n - 2)
}

// peek token in if guard `if x,y := opt()` after var_list `x,y`
fn (p &Parser) peek_token_after_var_list() token.Token {
	mut n := 0
	mut tok := p.tok
	for tok.kind != .eof {
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

fn (mut p Parser) open_scope() {
	if p.opened_scopes > p.max_opened_scopes {
		p.should_abort = true
		p.error('nested opened scopes limit reached: ${p.max_opened_scopes}')
		return
	}
	p.scope = &ast.Scope{
		parent:    p.scope
		start_pos: p.tok.pos
	}
	p.opened_scopes++
}

fn (mut p Parser) close_scope() {
	// p.scope.end_pos = p.tok.pos
	// NOTE: since this is usually called after `p.parse_block()`
	// ie. when `prev_tok` is rcbr `}` we most likely want `prev_tok`
	// we could do the following, but probably not needed in 99% of cases:
	// `end_pos = if p.prev_tok.kind == .rcbr { p.prev_tok.pos } else { p.tok.pos }`
	p.scope.end_pos = p.prev_tok.pos
	p.scope.parent.children << p.scope
	p.scope = p.scope.parent
	p.opened_scopes--
}

fn (mut p Parser) parse_block() []ast.Stmt {
	p.open_scope()
	stmts := p.parse_block_no_scope(false)
	p.close_scope()
	return stmts
}

fn (mut p Parser) parse_block_no_scope(is_top_level bool) []ast.Stmt {
	p.check(.lcbr)
	mut stmts := []ast.Stmt{cap: 20}
	old_assign_rhs := p.inside_assign_rhs
	p.inside_assign_rhs = false
	if p.tok.kind != .rcbr {
		mut count := 0
		for p.tok.kind !in [.eof, .rcbr] {
			stmts << p.stmt(is_top_level)
			count++
			if count % 100000 == 0 {
				if p.is_vls {
					// Stuck in VLS mode, exit
					return []
				}
				eprintln('parsed ${count} statements so far from fn ${p.cur_fn_name} ...')
			}
			if count > 1000000 {
				p.error_with_pos('parsed over ${count} statements from fn ${p.cur_fn_name}, the parser is probably stuck',
					p.tok.pos())
				return []
			}
		}
	}
	p.inside_assign_rhs = old_assign_rhs
	if is_top_level {
		p.top_level_statement_end()
	}
	p.check(.rcbr)
	// on assignment the last callexpr must be marked as return used recursively
	if p.inside_assign_rhs && stmts.len > 0 {
		mut last_stmt := stmts.last()
		p.mark_last_call_return_as_used(mut last_stmt)
	}
	return stmts
}

fn (mut p Parser) mark_last_call_return_as_used(mut last_stmt ast.Stmt) {
	match mut last_stmt {
		ast.ExprStmt {
			match mut last_stmt.expr {
				ast.CallExpr {
					// last stmt on block is CallExpr
					last_stmt.expr.is_return_used = true
				}
				ast.IfExpr {
					// last stmt on block is: if .. { foo() } else { bar() }
					for mut branch in last_stmt.expr.branches {
						if branch.stmts.len > 0 {
							mut last_if_stmt := branch.stmts.last()
							p.mark_last_call_return_as_used(mut last_if_stmt)
						}
					}
				}
				ast.ConcatExpr {
					// last stmt on block is: a, b, c := ret1(), ret2(), ret3()
					for mut expr in last_stmt.expr.vals {
						if mut expr is ast.CallExpr {
							expr.is_return_used = true
						}
					}
				}
				ast.InfixExpr {
					// last stmt has infix expr with CallExpr: foo()? + 'a'
					mut left_expr := last_stmt.expr.left
					for {
						if mut left_expr is ast.InfixExpr {
							left_expr = left_expr.left
							continue
						}
						if mut left_expr is ast.CallExpr {
							left_expr.is_return_used = true
						}
						break
					}
				}
				else {}
			}
		}
		else {}
	}
}

@[inline]
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
			s = '`${s}`'
		}
		p.unexpected(expecting: s)
	}
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

@[direct_array_access]
fn is_ident_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if !util.name_char_table[name[0]] {
		return false
	}
	for i in 1 .. name.len {
		if !util.func_char_table[name[i]] {
			return false
		}
	}
	return true
}

fn (mut p Parser) check_name() string {
	pos := p.tok.pos()
	name := p.tok.lit
	if p.tok.kind != .name && p.peek_tok.kind == .dot && name in p.imports {
		p.register_used_import(name)
	} else if p.tok.kind == .name && p.peek_tok.kind == .dot && name in p.imported_symbols {
		// symbols like Enum.field_name
		p.register_used_import_for_symbol_name(p.imported_symbols[name])
	}
	if !is_ident_name(name) {
		p.check(.name)
	} else {
		p.next()
	}
	if !p.inside_orm && !p.inside_attr_decl && name == 'sql' {
		p.error_with_pos('unexpected keyword `sql`, expecting name', pos)
	}
	return name
}

@[if trace_parser ?]
fn (p &Parser) trace_parser(label string) {
	eprintln('parsing: ${p.file_path:-30}|tok.pos: ${p.tok.pos().line_str():-39}|tok.kind: ${p.tok.kind:-10}|tok.lit: ${p.tok.lit:-10}|${label}')
}

fn (mut p Parser) top_stmt() ast.Stmt {
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
			.at {
				if p.peek_tok.kind == .lsbr {
					p.attributes()
					continue
				} else {
					return p.error('@[attr] expected')
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
				if p.peek_tok.kind == .key_for {
					comptime_for_stmt := p.comptime_for()
					return p.other_stmts(comptime_for_stmt)
				} else if p.peek_tok.kind == .key_if {
					if_expr := p.if_expr(true, false)
					cur_stmt := ast.ExprStmt{
						expr: if_expr
						pos:  if_expr.pos
					}
					if p.pref.is_fmt || comptime_if_expr_contains_top_stmt(if_expr) {
						return cur_stmt
					} else {
						return p.other_stmts(cur_stmt)
					}
				} else {
					return p.unexpected()
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
			.semicolon {
				return p.semicolon_stmt()
			}
			.key_asm {
				return p.asm_stmt(true)
			}
			else {
				return p.other_stmts(ast.empty_stmt)
			}
		}
		if p.should_abort {
			break
		}
	}
	// TODO: remove dummy return statement
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

		if p.main_already_defined {
			p.error('function `main` is already defined, put your script statements inside it')
		}

		p.open_scope()
		p.cur_fn_name = 'main.main'
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
			name:        'main.main'
			short_name:  'main'
			mod:         'main'
			is_main:     true
			stmts:       stmts
			file:        p.file_path
			return_type: ast.void_type
			scope:       p.scope
			label_names: p.label_names
		}
	} else if p.pref.is_fmt || p.pref.is_vet {
		return p.stmt(false)
	} else {
		return p.error('bad top level statement ' + p.tok.str())
	}
}

// TODO: [if vfmt]
fn (mut p Parser) check_comment() ast.Comment {
	if p.tok.kind == .comment {
		return p.comment()
	}
	return ast.Comment{}
}

fn (mut p Parser) comment() ast.Comment {
	mut pos := p.tok.pos()
	text := p.tok.lit
	num_newlines := text.count('\n')
	is_multi := num_newlines > 0
	pos.last_line = pos.line_nr + num_newlines
	p.next()
	return ast.Comment{
		text:     text
		is_multi: is_multi
		pos:      pos
	}
}

fn (mut p Parser) comment_stmt() ast.ExprStmt {
	comment := p.comment()
	return ast.ExprStmt{
		expr: comment
		pos:  comment.pos
	}
}

@[params]
struct EatCommentsConfig {
pub:
	same_line bool // Only eat comments on the same line as the previous token
	follow_up bool // Comments directly below the previous token as long as there is no empty line
}

fn (mut p Parser) eat_comments(cfg EatCommentsConfig) []ast.Comment {
	mut line := p.prev_tok.line_nr + p.prev_tok.lit.count('\n')
	mut comments := []ast.Comment{}
	for {
		if p.tok.kind != .comment || (cfg.same_line && p.tok.line_nr > line)
			|| (cfg.follow_up && p.tok.line_nr > line + 1) {
			break
		}
		comments << p.comment()
		if cfg.follow_up {
			line = p.prev_tok.line_nr + p.prev_tok.lit.count('\n')
		}
	}
	return comments
}

fn (mut p Parser) goto_eof() {
	for p.tok.kind != .eof {
		p.next()
	}
}

fn (mut p Parser) stmt(is_top_level bool) ast.Stmt {
	// ensure that possible parser aborts, are handled as early as possible (on the *next* processed statement):
	if p.should_abort {
		abort_pos := p.tok.pos()
		p.goto_eof()
		return ast.NodeError{
			idx: 0
			pos: abort_pos
		}
	}

	p.trace_parser('stmt(${is_top_level})')
	p.is_stmt_ident = p.tok.kind == .name
	match p.tok.kind {
		.lcbr {
			mut pos := p.tok.pos()
			if p.peek_token(2).kind == .colon {
				expr := p.expr(0)
				// `{ 'abc' : 22 }`
				return ast.ExprStmt{
					expr: expr
					pos:  pos
				}
			} else {
				stmts := p.parse_block()
				pos.update_last_line(p.prev_tok.line_nr)
				return ast.Block{
					stmts: stmts
					pos:   pos
				}
			}
		}
		.name {
			if p.peek_tok.kind == .name && p.tok.lit == 'sql' {
				return p.sql_stmt()
			}
			if p.peek_tok.kind == .colon {
				// `label:`
				spos := p.tok.pos()
				name := p.check_name()
				if name in p.label_names {
					return p.error_with_pos('duplicate label `${name}`', spos)
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
					pos:  spos.extend(p.tok.pos())
				}
			} else if p.peek_tok.kind == .name {
				if p.is_vls {
					// So that a line with a simple `var_name` works
					p.next()
					return ast.ExprStmt{
						expr: p.ident(.v)
					}
				}
				return p.unexpected(got: 'name `${p.tok.lit}`')
			} else if !p.inside_if_expr && !p.inside_match_body && !p.inside_or_expr
				&& p.peek_tok.kind in [.rcbr, .eof] && !p.scope.mark_var_as_used(p.tok.lit) {
				return p.error_with_pos('`${p.tok.lit}` evaluated but not used', p.tok.pos())
			}
			return p.parse_multi_expr(is_top_level)
		}
		.key_for {
			return p.for_stmt()
		}
		.comment {
			return p.comment_stmt()
		}
		.key_return {
			if !p.inside_defer {
				return p.return_stmt()
			} else {
				return p.error_with_pos('`return` not allowed inside `defer` block', p.tok.pos())
			}
		}
		.dollar {
			match p.peek_tok.kind {
				.key_if {
					mut pos := p.tok.pos()
					expr := p.if_expr(true, false)
					pos.update_last_line(p.prev_tok.line_nr)
					return ast.ExprStmt{
						expr: expr
						pos:  pos
					}
				}
				.key_for {
					return p.comptime_for()
				}
				.name {
					// handles $dbg directly without registering token
					if p.peek_tok.lit == 'dbg' {
						return p.dbg_stmt()
					} else {
						mut pos := p.tok.pos()
						expr := p.expr(0)
						pos.update_last_line(p.prev_tok.line_nr)
						return ast.ExprStmt{
							expr: expr
							pos:  pos
						}
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
				kind:  tok.kind
				label: label
				pos:   tok.pos()
			}
		}
		.key_unsafe {
			return p.unsafe_stmt()
		}
		.hash {
			return p.hash()
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
				expr:      expr
				extra:     extra
				extra_pos: extra_pos
				pos:       pos.extend(p.tok.pos())
				is_used:   p.inside_test_file || !p.pref.is_prod
			}
		}
		.key_defer {
			if !p.inside_defer {
				p.next()
				spos := p.tok.pos()
				p.inside_defer = true
				p.defer_vars = []ast.Ident{}
				stmts := p.parse_block()
				p.inside_defer = false
				return ast.DeferStmt{
					stmts:      stmts
					defer_vars: p.defer_vars.clone()
					pos:        spos.extend_with_last_line(p.tok.pos(), p.prev_tok.line_nr)
				}
			} else {
				return p.error_with_pos('`defer` blocks cannot be nested', p.tok.pos())
			}
		}
		.key_go, .key_spawn {
			if (p.pref.use_coroutines || p.pref.is_fmt) && p.tok.kind == .key_go {
				go_expr := p.go_expr()
				return ast.ExprStmt{
					expr: go_expr
					pos:  go_expr.pos
				}
			} else {
				spawn_expr := p.spawn_expr()
				return ast.ExprStmt{
					expr: spawn_expr
					pos:  spawn_expr.pos
				}
			}
		}
		.key_goto {
			p.next()
			spos := p.tok.pos()
			name := p.check_name()
			return ast.GotoStmt{
				name: name
				pos:  spos
			}
		}
		.key_const {
			return p.error_with_pos('const can only be defined at the top level (outside of functions)',
				p.tok.pos())
		}
		.key_asm {
			return p.asm_stmt(false)
		}
		.semicolon {
			return p.semicolon_stmt()
		}
		// Allow struct definitions inside functions
		.key_struct, .key_union {
			return p.struct_decl(false)
		}
		// literals, 'if', etc. in here
		else {
			return p.parse_multi_expr(is_top_level)
		}
	}
}

fn (mut p Parser) dbg_stmt() ast.DebuggerStmt {
	pos := p.tok.pos()
	p.check(.dollar)
	p.check(.name)
	p.register_auto_import('v.debug')
	return ast.DebuggerStmt{
		pos: pos
	}
}

fn (mut p Parser) semicolon_stmt() ast.SemicolonStmt {
	pos := p.tok.pos()
	p.check(.semicolon)
	return ast.SemicolonStmt{
		pos: pos
	}
}

fn (mut p Parser) expr_list(expect_value bool) []ast.Expr {
	mut exprs := []ast.Expr{}
	for {
		expr := if expect_value { p.expr(0) } else { p.expr_no_value(0) }
		if expr !is ast.Comment {
			exprs << expr
			if p.tok.kind != .comma {
				break
			}
			p.next()
		}
	}
	return exprs
}

@[direct_array_access]
fn (mut p Parser) parse_multi_expr(is_top_level bool) ast.Stmt {
	// in here might be 1) multi-expr 2) multi-assign
	// 1, a, c ... }       // multi-expression
	// a, mut b ... :=/=   // multi-assign
	// collect things upto hard boundaries
	tok := p.tok
	mut pos := tok.pos()

	mut defer_vars := p.defer_vars.clone()
	p.defer_vars = []ast.Ident{}

	left := p.expr_list(p.inside_assign_rhs)

	if !(p.inside_defer && p.tok.kind == .decl_assign) {
		defer_vars << p.defer_vars
	}

	p.defer_vars = defer_vars

	left0 := left[0]
	if tok.kind in [.key_mut, .key_shared, .key_atomic] && left0.is_blank_ident() {
		return p.error_with_pos('cannot use `${tok.kind}` on `_`', tok.pos())
	}

	if tok.kind == .key_mut && p.tok.kind != .decl_assign {
		return p.error('expecting `:=` (e.g. `mut x :=`)')
	}
	// TODO: remove translated
	if p.tok.kind.is_assign() {
		return p.partial_assign_stmt(left)
	} else if !p.pref.translated && !p.is_translated && !p.pref.is_fmt && !p.pref.is_vet
		&& tok.kind !in [.key_if, .key_match, .key_lock, .key_rlock, .key_select] {
		for node in left {
			if (is_top_level || p.tok.kind !in [.comment, .rcbr])
				&& node !in [ast.CallExpr, ast.PostfixExpr, ast.ComptimeCall, ast.SelectorExpr, ast.DumpExpr] {
				is_complex_infix_expr := node is ast.InfixExpr
					&& node.op in [.left_shift, .right_shift, .unsigned_right_shift, .arrow]
				if !is_complex_infix_expr && !p.is_vls {
					return p.error_with_pos('expression evaluated but not used', node.pos())
				}
			}
		}
	}
	pos.update_last_line(p.prev_tok.line_nr)
	if left.len == 1 {
		return ast.ExprStmt{
			expr:    left0
			pos:     left0.pos()
			is_expr: p.inside_for
		}
	}
	return ast.ExprStmt{
		expr: ast.ConcatExpr{
			vals: left
			pos:  tok.pos()
		}
		pos:  pos
	}
}

fn (mut p Parser) ident(language ast.Language) ast.Ident {
	is_option := p.tok.kind == .question && p.peek_tok.kind == .lsbr
	if is_option {
		p.next()
	}
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
	if p.tok.kind !in [.name, .key_type] {
		if is_mut || is_static || is_volatile {
			p.error_with_pos('the `${modifier_kind}` keyword is invalid here', mut_pos)
		} else {
			p.unexpected(got: 'token `${p.tok.lit}`')
		}
		return ast.Ident{
			scope: p.scope
		}
	}
	in_select := p.prev_tok.kind == .arrow
	pos := p.tok.pos()
	mut name := p.check_name()
	if name == '_' {
		return ast.Ident{
			tok_kind: p.tok.kind
			name:     '_'
			comptime: p.comptime_if_cond
			kind:     .blank_ident
			pos:      pos
			info:     ast.IdentVar{
				is_option: is_option
			}
			scope:    p.scope
		}
	}
	is_following_concrete_types := p.is_following_concrete_types()
	mut concrete_types := []ast.Type{}
	if p.expr_mod.len > 0 {
		name = '${p.expr_mod}.${name}'
	}

	// parsers ident like var?, except on '<- var' '$if ident ?', '[if define ?]'
	allowed_cases := !in_select && !p.inside_comptime_if && !p.inside_ct_if_expr
	mut or_kind := ast.OrKind.absent
	mut or_stmts := []ast.Stmt{}
	mut or_pos := token.Pos{}

	if allowed_cases && p.tok.kind == .question && p.peek_tok.kind != .lpar { // var?, not var?(
		or_kind = ast.OrKind.propagate_option
		p.check(.question)
	} else if allowed_cases && p.tok.kind == .key_orelse {
		or_kind = ast.OrKind.block
		or_stmts, or_pos = p.or_block(.no_err_var)
	} else if is_following_concrete_types {
		// `generic_fn[int]`
		concrete_types = p.parse_concrete_types()
	}
	typ := match p.peek_tok.kind {
		.string {
			ast.string_type_idx
		}
		.lsbr {
			ast.array_type_idx
		}
		else {
			if p.tok.kind == .dot {
				if var := p.scope.find_var(name) { var.typ } else { 0 }
			} else {
				0
			}
		}
	}
	return ast.Ident{
		tok_kind:       p.tok.kind
		kind:           .unresolved
		name:           name
		comptime:       p.comptime_if_cond
		language:       language
		mod:            p.mod
		pos:            pos
		is_mut:         is_mut
		mut_pos:        mut_pos
		info:           ast.IdentVar{
			typ:         typ
			is_mut:      is_mut
			is_static:   is_static
			is_volatile: is_volatile
			is_option:   or_kind != ast.OrKind.absent
			share:       ast.sharetype_from_flags(is_shared, is_atomic)
		}
		scope:          p.scope
		or_expr:        ast.OrExpr{
			kind:  or_kind
			stmts: or_stmts
			pos:   or_pos
		}
		concrete_types: concrete_types
	}
}

fn (mut p Parser) alias_array_type() ast.Type {
	full_name := p.prepend_mod(p.tok.lit)

	if idx := p.table.type_idxs[full_name] {
		if idx == 0 {
			return ast.void_type
		}
		sym := p.table.sym(ast.idx_to_type(idx))
		if sym.info is ast.Alias {
			if sym.info.parent_type == 0 {
				return ast.void_type
			}
			if p.table.sym(sym.info.parent_type).kind == .array {
				return idx
			}
		}
	}
	return ast.void_type
}

@[direct_array_access]
fn (mut p Parser) name_expr() ast.Expr {
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
		is_known_var := p.scope.known_var(p.tok.lit)
		if is_known_var {
			p.scope.mark_var_as_used(p.tok.lit)
			return p.ident(.v)
		} else {
			type_pos := p.tok.pos()
			typ := p.parse_type()
			return ast.TypeNode{
				typ: typ
				pos: type_pos
			}
		}
	}
	language := match p.tok.lit {
		'C' { ast.Language.c }
		'JS' { ast.Language.js }
		'WASM' { ast.Language.wasm }
		else { ast.Language.v }
	}
	if language != .v {
		p.check_for_impure_v(language, p.tok.pos())
	}
	is_option := p.tok.kind == .question
	if is_option {
		if p.peek_tok.kind in [.name, .lsbr] {
			p.check(.question)
		}
	}
	is_array := p.tok.kind == .lsbr
	is_fixed_array := is_array && p.peek_tok.kind == .number
	mut mod := ''
	// p.warn('resetting')
	p.expr_mod = ''
	// `map[string]int` initialization
	if p.peek_tok.kind == .lsbr && p.tok.lit == 'map' {
		mut pos := p.tok.pos()
		mut map_type := p.parse_map_type()
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
		if is_option {
			map_type = map_type.set_flag(.option)
		}
		node = ast.MapInit{
			typ: map_type
			pos: pos
		}
		if p.tok.kind == .lpar {
			// ?map[int]int(none) cast expr
			p.check(.lpar)
			expr := p.expr(0)
			p.check(.rpar)
			return ast.CastExpr{
				typ:     map_type
				typname: p.table.sym(map_type).name
				expr:    expr
				pos:     pos.extend(p.tok.pos())
			}
		}
		return node
	}
	// `chan typ{...}`
	if p.tok.lit == 'chan' {
		first_pos := p.tok.pos()
		mut last_pos := first_pos
		mut elem_type_pos := p.peek_tok.pos()
		if p.peek_tok.kind == .not {
			return p.error_with_pos('cannot use chan with Result type', p.peek_tok.pos())
		}
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
					return p.error('`${key}` cannot be initialized for `chan`. Did you mean `cap`?')
				}
				else {
					return p.error('wrong field `${key}`, expecting `cap`')
				}
			}
			last_pos = p.tok.pos()
			p.check(.rcbr)
		}
		if chan_type == ast.chan_type {
			p.error_with_pos('`chan` has no type specified. Use `chan Type{}` instead of `chan{}`',
				first_pos.extend(last_pos))
		}
		return ast.ChanInit{
			pos:           first_pos.extend(last_pos)
			elem_type_pos: elem_type_pos
			has_cap:       has_cap
			cap_expr:      cap_expr
			typ:           chan_type
		}
	}
	// Raw string (`s := r'hello \n ')
	if p.peek_tok.kind == .string && p.tok.line_nr == p.peek_tok.line_nr && !p.inside_str_interp
		&& p.peek_token(2).kind != .colon {
		if p.tok.kind == .name && p.tok.lit in ['r', 'c', 'js'] {
			return p.string_expr()
		} else {
			// don't allow any other string prefix except `r`, `js` and `c`
			return p.error('only `c`, `r`, `js` are recognized string prefixes, but you tried to use `${p.tok.lit}`')
		}
	}
	// don't allow r`byte` and c`byte`
	if p.peek_tok.kind == .chartoken && p.tok.lit.len == 1 && p.tok.lit[0] in [`r`, `c`] {
		opt := if p.tok.lit == 'r' { '`r` (raw string)' } else { '`c` (c string)' }
		return p.error('cannot use ${opt} with `byte` and `rune`')
	}
	// Make sure that the var is not marked as used in assignments: `x = 1`, `x += 2` etc
	// but only when it's actually used (e.g. `println(x)`)
	known_var := if p.peek_tok.kind.is_assign() {
		p.scope.known_var(p.tok.lit)
	} else {
		p.scope.mark_var_as_used(p.tok.lit)
	}
	// Handle modules
	mut is_mod_cast := false
	if p.peek_tok.kind == .dot && !known_var && (language != .v || p.known_import(p.tok.lit)
		|| p.mod.all_after_last('.') == p.tok.lit) {
		// p.tok.lit has been recognized as a module
		if language in [.c, .js, .wasm] {
			mod = language.str().to_upper_ascii()
		} else {
			if p.tok.lit in p.imports {
				// mark the imported module as used
				p.register_used_import(p.tok.lit)
				tk2 := p.peek_token(2)
				if p.peek_tok.kind == .dot && tk2.kind != .eof && tk2.lit.len > 0
					&& tk2.lit[0].is_capital() {
					is_mod_cast = true
				} else if p.peek_tok.kind == .dot && tk2.kind != .eof && tk2.lit.len == 0 {
					// incomplete module selector must be handled by dot_expr instead
					ident := p.ident(language)
					node = ident
					p.add_defer_var(ident)
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

	is_generic_call := p.is_generic_call()
	is_generic_cast := p.is_generic_cast()
	is_generic_struct_init := p.is_generic_struct_init()
	if p.peek_tok.kind == .lpar && p.tok.line_nr != p.peek_tok.line_nr
		&& p.peek_token(2).is_next_to(p.peek_tok) {
		// `(` must be on same line as name token otherwise it's a ParExpr
		ident := p.ident(language)
		node = ident
		p.add_defer_var(ident)
	} else if p.peek_tok.kind == .lpar || is_generic_call || is_generic_cast
		|| (p.tok.kind == .lsbr && p.peek_tok.kind == .rsbr && (p.peek_token(3).kind == .lpar
		|| p.peek_token(5).kind == .lpar)) || (p.tok.kind == .lsbr && p.peek_tok.kind == .number
		&& p.peek_token(2).kind == .rsbr && (p.peek_token(4).kind == .lpar
		|| p.peek_token(6).kind == .lpar)) {
		// ?[]foo(), ?[1]foo, foo(), foo<int>() or type() cast
		mut original_name := if is_array {
			p.peek_token(if is_fixed_array { 3 } else { 2 }).lit
		} else {
			p.tok.lit
		}
		if is_fixed_array && p.peek_token(4).kind == .dot {
			mod = original_name
			original_name = p.peek_token(5).lit
		} else if is_array && p.peek_token(3).kind == .dot {
			mod = original_name
			original_name = p.peek_token(4).lit
		}
		name := if mod != '' { '${mod}.${original_name}' } else { original_name }
		name_w_mod := p.prepend_mod(name)
		is_c_pointer_cast := language == .c && prev_tok_kind == .amp // `&C.abc(x)` is *always* a cast
		is_c_type_cast := language == .c && (original_name in ['intptr_t', 'uintptr_t']
			|| (name in p.table.type_idxs && original_name[0].is_capital()))
		is_js_cast := language == .js && name.all_after_last('.')[0].is_capital()
		// type cast. TODO: finish
		// if name in ast.builtin_type_names_to_idx {
		// handle the easy cases first, then check for an already known V typename, not shadowed by a local variable
		if (is_option || p.peek_tok.kind in [.lsbr, .lt, .lpar]) && (is_mod_cast
			|| is_c_pointer_cast || is_c_type_cast || is_js_cast || is_generic_cast
			|| (language == .v && name != '' && (name[0].is_capital() || (!known_var
			&& (name in p.table.type_idxs || name_w_mod in p.table.type_idxs))
			|| name.all_after_last('.')[0].is_capital()))) {
			// MainLetter(x) is *always* a cast, as long as it is not `C.`
			// TODO: handle C.stat()
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
			if is_option {
				to_typ = to_typ.set_flag(.option)
			}
			node = ast.CastExpr{
				typ:     to_typ
				typname: if to_typ != 0 { p.table.sym(to_typ).name } else { 'unknown typename' }
				expr:    expr
				arg:     arg
				has_arg: has_arg
				pos:     start_pos.extend(end_pos)
			}
			p.expr_mod = ''
			return node
		} else {
			// fn_call
			if is_option {
				p.unexpected_with_pos(p.prev_tok.pos(),
					got: '${p.prev_tok}'
				)
			}
			// mod.Enum.val
			if p.peek_tok.kind == .dot && p.peek_token(3).kind in [.comma, .rpar] {
				node = p.enum_val_expr(mod)
			} else {
				node = p.call_expr(language, mod)
				if p.tok.kind == .lpar && p.prev_tok.line_nr == p.tok.line_nr {
					p.next()
					pos := p.tok.pos()
					args := p.call_args()
					p.check(.rpar)
					or_block := p.gen_or_block()
					node = ast.CallExpr{
						left:           node
						args:           args
						pos:            pos
						scope:          p.scope
						or_block:       or_block
						is_return_used: p.expecting_value
					}
				}
			}
		}
	} else if !known_var && (p.peek_tok.kind == .lcbr || is_generic_struct_init)
		&& (!p.inside_match || (p.inside_select && prev_tok_kind == .arrow && lit0_is_capital))
		&& !p.inside_match_case && (!p.inside_if || p.inside_select)
		&& (!p.inside_for || p.inside_select) {
		alias_array_type := p.alias_array_type()
		if alias_array_type != ast.void_type {
			return p.array_init(is_option, alias_array_type)
		} else {
			// `if a == Foo{} {...}` or `match foo { Foo{} {...} }`
			return p.struct_init(p.mod + '.' + p.tok.lit, .normal, is_option)
		}
	} else if p.peek_tok.kind == .lcbr
		&& ((p.inside_if && lit0_is_capital && p.tok.lit.len > 1 && !known_var && language == .v)
		|| (p.inside_match_case && lit0_is_capital && p.tok.kind == .name
		&& p.peek_tok.is_next_to(p.tok))) {
		// XTODO check iscap
		//|| (p.inside_match_case && p.tok.kind == .name && p.peek_tok.is_next_to(p.tok))) {
		// `if a == Foo{} {...}` or `match foo { Foo{} {...} }`
		return p.struct_init(p.mod + '.' + p.tok.lit, .normal, is_option)
	} else if p.peek_tok.kind == .dot && lit0_is_capital && !known_var && language == .v {
		// T.name selector
		if p.is_generic_name() && p.peek_token(3).kind != .lpar {
			pos := p.tok.pos()
			name := p.check_name()
			p.check(.dot)
			field := p.check_name()
			fkind := match field {
				'name' { ast.GenericKindField.name }
				'typ' { ast.GenericKindField.typ }
				'unaliased_typ' { ast.GenericKindField.unaliased_typ }
				'indirections' { ast.GenericKindField.indirections }
				else { ast.GenericKindField.unknown }
			}
			pos.extend(p.tok.pos())
			return ast.SelectorExpr{
				expr:        ast.Ident{
					name:  name
					scope: p.scope
				}
				field_name:  field
				gkind_field: fkind
				pos:         pos
				scope:       p.scope
			}
		}
		if !known_var && p.peek_token(2).kind == .name && p.peek_token(3).kind == .lpar {
			if lit0_is_capital && p.peek_tok.kind == .dot && language == .v {
				// New static method call
				p.expr_mod = ''
				return p.call_expr(language, mod)
			} else {
				p.error_with_pos('${lit0_is_capital} the receiver of the method call must be an instantiated object, e.g. `foo.bar()`',
					p.tok.pos())
			}
		}
		// `anon_fn := Foo.bar` assign static method
		if !known_var && lit0_is_capital && p.peek_tok.kind == .dot && language == .v
			&& p.peek_token(2).kind == .name {
			if func := p.table.find_fn(p.prepend_mod(p.tok.lit) + '__static__' + p.peek_token(2).lit) {
				fn_type := ast.new_type(p.table.find_or_register_fn_type(func, false,
					true))
				pos := p.tok.pos()
				typ_name := p.check_name()
				p.check(.dot)
				field_name := p.check_name()
				pos.extend(p.tok.pos())
				return ast.Ident{
					name:  p.prepend_mod(typ_name) + '__static__' + field_name
					mod:   p.mod
					kind:  .function
					info:  ast.IdentFn{
						typ: fn_type
					}
					pos:   pos
					scope: p.scope
				}
			}
		}
		return p.enum_val_expr(mod)
	} else if language == .js && p.peek_tok.kind == .dot && p.peek_token(2).kind == .name {
		// JS. function call with more than 1 dot
		node = p.call_expr(language, mod)
	} else {
		if p.inside_in_array && ((lit0_is_capital && !known_var && language == .v)
			|| (p.peek_tok.kind == .dot && p.peek_token(2).lit.len > 0
			&& p.peek_token(2).lit[0].is_capital())
			|| p.table.find_type_idx(p.mod + '.' + p.tok.lit) > 0
			|| p.inside_comptime_if) {
			type_pos := p.tok.pos()
			mut typ := p.parse_type()
			if is_option {
				typ = typ.set_flag(.option)
			}
			return ast.TypeNode{
				typ: typ
				pos: type_pos
			}
		} else if !known_var && language == .v && (lit0_is_capital || p.table.known_type(p.tok.lit))
			&& p.peek_tok.kind == .pipe {
			start_pos := p.tok.pos()
			mut to_typ := p.parse_type()
			p.check(.lpar)
			expr := p.expr(0)
			end_pos := p.tok.pos()
			p.check(.rpar)
			node = ast.CastExpr{
				typ:     to_typ
				typname: if to_typ != 0 { p.table.sym(to_typ).name } else { 'unknown type name' }
				expr:    expr
				arg:     ast.empty_expr
				has_arg: false
				pos:     start_pos.extend(end_pos)
			}
			p.expr_mod = ''
			return node
		} else if is_option && p.tok.kind == .lsbr {
			return p.array_init(is_option, ast.void_type)
		} else if !known_var && language == .v && p.peek_tok.kind == .dot && !p.pref.is_fmt {
			peek_tok2 := p.peek_token(2)
			peek_tok3 := p.peek_token(3)
			mod = p.tok.lit
			mut n := -1
			for p.peek_token(n).kind == .dot && p.peek_token(n - 1).kind == .name {
				mod = p.peek_token(n - 1).lit + '.' + mod
				n -= 2
			}
			if peek_tok2.kind == .name && peek_tok2.lit.len > 0 && peek_tok2.lit[0].is_capital()
				&& peek_tok3.kind == .lcbr
				&& (mod.len > p.tok.lit.len || !p.known_import(p.tok.lit)) {
				mut msg := 'unknown module `${mod}`'
				if mod.len > p.tok.lit.len && p.known_import(p.tok.lit) {
					msg += '; did you mean `${p.tok.lit}`?'
				}
				p.error_with_pos(msg, p.tok.pos())
			}
		}

		ident := p.ident(language)
		node = ident
		p.add_defer_var(ident)
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
			name:         'err'
			typ:          ast.error_type
			pos:          p.tok.pos()
			is_used:      true
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
					left:     left
					pos:      pos_high
					index:    ast.RangeExpr{
						low:      ast.empty_expr
						high:     high
						has_high: has_high
						pos:      pos_high
						is_gated: is_gated
					}
					or_expr:  ast.OrExpr{
						kind:  .block
						stmts: or_stmts_high
						pos:   or_pos_high
					}
					is_gated: is_gated
				}
			}
			// `a[start..end]!`
			if p.tok.kind == .not {
				or_pos_high = p.tok.pos()
				or_kind_high = .propagate_result
				p.next()
			} else if p.tok.kind == .question {
				p.error_with_pos('`?` for propagating errors from index expressions is no longer supported, use `!` instead of `?`',
					p.tok.pos())
			}
		}

		return ast.IndexExpr{
			left:     left
			pos:      pos_high
			index:    ast.RangeExpr{
				low:      ast.empty_expr
				high:     high
				has_high: has_high
				pos:      pos_high
				is_gated: is_gated
			}
			or_expr:  ast.OrExpr{
				kind:  or_kind_high
				stmts: or_stmts_high
				pos:   or_pos_high
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
					left:     left
					pos:      pos_low
					index:    ast.RangeExpr{
						low:      expr
						high:     high
						has_high: has_high
						has_low:  has_low
						pos:      pos_low
						is_gated: is_gated
					}
					or_expr:  ast.OrExpr{
						kind:  .block
						stmts: or_stmts_low
						pos:   or_pos_low
					}
					is_gated: is_gated
				}
			}
			// `a[start..end]!`
			if p.tok.kind == .not {
				or_pos_low = p.tok.pos()
				or_kind_low = .propagate_result
				p.next()
			} else if p.tok.kind == .question {
				p.error_with_pos('`?` for propagating errors from index expressions is no longer supported, use `!` instead of `?`',
					p.tok.pos())
			}
		}

		return ast.IndexExpr{
			left:     left
			pos:      pos_low
			index:    ast.RangeExpr{
				low:      expr
				high:     high
				has_high: has_high
				has_low:  has_low
				pos:      pos_low
				is_gated: is_gated
			}
			or_expr:  ast.OrExpr{
				kind:  or_kind_low
				stmts: or_stmts_low
				pos:   or_pos_low
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
				left:     left
				index:    expr
				pos:      pos
				or_expr:  ast.OrExpr{
					kind:  .block
					stmts: or_stmts
					pos:   or_pos
				}
				is_gated: is_gated
			}
		}
		// `a[i]!`
		if p.tok.kind == .not {
			or_pos = p.tok.pos()
			or_kind = .propagate_result
			p.next()
		} else if p.tok.kind == .question {
			p.error_with_pos('`?` for propagating errors from index expressions is no longer supported, use `!` instead of `?`',
				p.tok.pos())
		}
	}
	return ast.IndexExpr{
		left:     left
		index:    expr
		pos:      pos
		or_expr:  ast.OrExpr{
			kind:  or_kind
			stmts: or_stmts
			pos:   or_pos
		}
		is_gated: is_gated
	}
}

fn (mut p Parser) dot_expr(left ast.Expr) ast.Expr {
	prev_line := p.prev_tok.pos().line_nr
	p.next()
	if p.tok.kind == .dollar {
		return p.comptime_selector(left)
	}
	is_generic_call := p.is_generic_call()
	name_pos := p.tok.pos()
	// array initialization with enum shortcut [Enum.foo .bar]
	if !is_generic_call && p.tok.kind == .name && p.inside_array_lit && p.last_enum_name != ''
		&& prev_line != name_pos.line_nr {
		p.name_error = true
		return ast.EnumVal{
			enum_name: p.last_enum_name
			val:       p.check_name()
			pos:       p.tok.pos()
			mod:       p.last_enum_mod
		}
	}
	mut field_name := ''
	// check if the name is on the same line as the dot
	if p.prev_tok.pos().line_nr == name_pos.line_nr || p.tok.kind != .name {
		if p.is_vls {
			if p.tok.kind in [.rpar, .rcbr] {
				// Simplify the dot expression for VLS, so that the parser doesn't error
				// `println(x.)` => `println(x)`
				// `x. }` => `x }` etc
				return left
			} else if name_pos.line_nr != p.tok.line_nr {
				return left
			}
		}
		field_name = p.check_name()
	} else {
		p.name_error = true
	}
	is_filter := field_name in ['filter', 'map', 'any', 'all', 'count']
	if is_filter || field_name == 'sort' || field_name == 'sorted' {
		p.open_scope()
		defer {
			p.close_scope()
		}
	}
	// ! in mutable methods
	if p.tok.kind == .not && p.peek_tok.kind == .lpar {
		p.next()
	}
	// Method call
	// TODO: move to fn.v call_expr()
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
		or_block := p.gen_or_block()
		end_pos := p.prev_tok.pos()
		pos := name_pos.extend(end_pos)
		comments := p.eat_comments(same_line: true)
		mut left_node := unsafe { left }
		if mut left_node is ast.CallExpr {
			left_node.is_return_used = true
		}
		mcall_expr := ast.CallExpr{
			left:              left
			name:              field_name
			args:              args
			name_pos:          name_pos
			pos:               pos
			is_method:         true
			concrete_types:    concrete_types
			concrete_list_pos: concrete_list_pos
			or_block:          or_block
			scope:             p.scope
			comments:          comments
			is_return_used:    p.expecting_value
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

	mut or_kind := ast.OrKind.absent
	mut or_stmts := []ast.Stmt{}
	mut or_pos := token.Pos{}
	if p.tok.kind == .key_orelse {
		or_kind = .block
		or_stmts, or_pos = p.or_block(.with_err_var)
	} else if p.tok.kind == .not {
		or_kind = .propagate_result
		or_pos = p.tok.pos()
		p.next()
	} else if p.tok.kind == .question {
		or_kind = .propagate_option
		or_pos = p.tok.pos()
		p.next()
	}
	sel_expr := ast.SelectorExpr{
		expr:       left
		field_name: field_name
		pos:        pos
		is_mut:     is_mut
		mut_pos:    mut_pos
		or_block:   ast.OrExpr{
			kind:  or_kind
			stmts: or_stmts
			pos:   or_pos
		}
		scope:      p.scope
		next_token: p.tok.kind
	}
	mut left_node := unsafe { left }
	if mut left_node is ast.CallExpr {
		left_node.is_return_used = true
	}
	return sel_expr
}

fn (mut p Parser) parse_generic_types() ([]ast.Type, []string) {
	mut types := []ast.Type{}
	mut param_names := []string{}
	if p.tok.kind == .lt {
		p.error('The generic symbol `<>` is obsolete, please replace it with `[]`')
	}
	if p.tok.kind != .lsbr {
		return types, param_names
	}
	end_kind := token.Kind.rsbr
	p.next()
	mut first_done := false
	mut count := 0
	for p.tok.kind !in [end_kind, .eof] {
		if first_done {
			p.check(.comma)
		}
		name := p.tok.lit
		if name != '' && !name[0].is_capital() {
			p.error('generic parameter needs to be uppercase')
		}
		if name.len > 1 {
			p.error('generic parameter name needs to be exactly one char')
		}
		if !util.is_generic_type_name(p.tok.lit) {
			p.error('`${p.tok.lit}` is a reserved name and cannot be used for generics')
		}
		if name in param_names {
			p.error('duplicated generic parameter `${name}`')
		}
		if count > 8 {
			p.error('cannot have more than 9 generic parameters')
		}
		p.check(.name)
		param_names << name

		mut idx := p.table.find_type_idx(name)
		if idx == 0 {
			idx = p.table.register_sym(ast.TypeSymbol{
				name:   name
				cname:  util.no_dots(name)
				mod:    p.mod
				kind:   .any
				is_pub: true
			})
		}
		types << ast.new_type(idx).set_flag(.generic)
		first_done = true
		count++
	}
	p.check(end_kind)
	return types, param_names
}

fn (mut p Parser) parse_concrete_types() []ast.Type {
	mut types := []ast.Type{}
	if p.tok.kind == .lt {
		p.error('The generic symbol `<>` is obsolete, please replace it with `[]`')
	}
	if p.tok.kind != .lsbr {
		return types
	}
	p.inside_fn_concrete_type = true
	defer {
		p.inside_fn_concrete_type = false
	}
	end_kind := token.Kind.rsbr
	p.next() // `[`
	mut first_done := false
	for p.tok.kind !in [.eof, end_kind] {
		if first_done {
			p.check(.comma)
		}
		types << p.parse_type()
		first_done = true
	}
	p.check(end_kind) // `]`
	return types
}

fn (mut p Parser) string_expr() ast.Expr {
	is_raw := p.tok.kind == .name && p.tok.lit == 'r'
	is_cstr := p.tok.kind == .name && p.tok.lit == 'c'
	is_js_str := p.tok.kind == .name && p.tok.lit == 'js'
	if is_raw || is_cstr || is_js_str {
		p.next()
	}
	mut node := ast.empty_expr
	val := p.tok.lit
	mut pos := p.tok.pos()
	pos.last_line = pos.line_nr + val.count('\n')
	if p.peek_tok.kind != .str_dollar {
		p.next()
		node = ast.StringLiteral{
			val:      val
			is_raw:   is_raw
			language: match true {
				is_cstr { ast.Language.c }
				is_js_str { ast.Language.js }
				else { ast.Language.v }
			}
			pos:      pos
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
	node = ast.StringInterLiteral{
		vals:       vals
		exprs:      exprs
		need_fmts:  has_fmts
		fwidths:    fwidths
		precisions: precisions
		pluss:      visible_pluss
		fills:      fills
		fmts:       fmts
		fmt_poss:   fposs
		pos:        pos
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
		mut name := p.check_name()
		end_comments << p.eat_comments()
		// Handle `const C.MY_CONST u16`
		mut is_virtual_c_const := false
		mut typ := ast.void_type
		if name == 'C' && p.tok.kind == .dot {
			p.next()
			name += '.' + p.check_name()
			typ = p.parse_type()
			is_virtual_c_const = true
		}
		if !p.pref.translated && !p.is_translated && util.contains_capital(name)
			&& !is_virtual_c_const {
			p.error_with_pos('const names cannot contain uppercase letters, use snake_case instead',
				pos)
		}
		full_name := if is_virtual_c_const { name } else { p.prepend_mod(name) }
		if p.tok.kind == .comma {
			p.error_with_pos('const declaration do not support multiple assign yet', p.tok.pos())
		}
		// Allow for `const x := 123`, and for `const x = 123` too.
		// Supporting `const x := 123` in addition to `const x = 123`, makes extracting local variables to constants
		// much less annoying, while prototyping:
		if p.tok.kind == .decl_assign {
			p.check(.decl_assign)
		} else {
			if !is_virtual_c_const {
				p.check(.assign)
			}
		}
		end_comments << p.eat_comments()
		if p.tok.kind == .key_fn && !is_virtual_c_const {
			p.error('const initializer fn literal is not a constant')
			return ast.ConstDecl{}
		}
		if p.tok.kind == .eof {
			p.unexpected(got: 'eof', expecting: 'an expression')
			return ast.ConstDecl{}
		}
		mut expr := ast.Expr{}
		if !is_virtual_c_const {
			expr = p.expr(0)
		}
		if is_block {
			end_comments << p.eat_comments(same_line: true)
		}
		mut field := ast.ConstField{
			name:         full_name
			mod:          p.mod
			is_pub:       is_pub
			expr:         expr
			pos:          pos.extend(expr.pos())
			attrs:        attrs
			comments:     comments
			end_comments: end_comments
			is_markused:  is_markused
			is_virtual_c: is_virtual_c_const
		}
		if is_virtual_c_const {
			field.typ = typ
		}
		fields << field
		p.table.global_scope.register(field)
		comments = []
		if is_block {
			end_comments = []
		}
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
		pos:          start_pos.extend_with_last_line(const_pos, p.prev_tok.line_nr)
		fields:       fields
		is_pub:       is_pub
		end_comments: comments
		is_block:     is_block
		attrs:        attrs
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
			pos:      first_pos
		}
	}
	// return exprs
	old_assign_rhs := p.inside_assign_rhs
	p.inside_assign_rhs = true
	exprs := p.expr_list(true)
	p.inside_assign_rhs = old_assign_rhs
	end_pos := exprs.last().pos()
	return ast.Return{
		exprs:    exprs
		comments: comments
		pos:      first_pos.extend(end_pos)
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
	mut is_exported := false
	for ga in attrs {
		match ga.name {
			'export' { is_exported = true }
			'markused' { is_markused = true }
			else {}
		}
	}

	if !p.has_globals && !p.pref.enable_globals && !p.pref.is_fmt && !p.pref.is_vet
		&& !p.pref.translated && !p.is_translated && !p.pref.is_livemain && !p.pref.building_v
		&& !p.builtin_mod {
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
			name:        name
			has_expr:    has_expr
			expr:        expr
			pos:         pos
			typ_pos:     typ_pos
			typ:         typ
			comments:    comments
			is_markused: is_markused
			is_volatile: is_volatile
			is_exported: is_exported
		}
		fields << field
		if name !in ast.global_reserved_type_names {
			p.table.global_scope.register(field)
		}
		comments = []
		if !is_block {
			break
		}
	}
	if is_block {
		p.check(.rpar)
	}
	return ast.GlobalDecl{
		pos:          start_pos.extend(p.prev_tok.pos())
		mod:          p.mod
		fields:       fields
		end_comments: comments
		is_block:     is_block
		attrs:        attrs
	}
}

fn source_name(name string) string {
	if token.is_key(name) {
		return '@${name}'
	}
	return name
}

fn (mut p Parser) type_decl() ast.TypeDecl {
	attrs := p.attrs
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
	mut name := p.check_name()
	mut language := ast.Language.v
	if name.len == 1 && name[0].is_capital() {
		if name == 'C' && p.tok.kind == .dot {
			p.next() // .
			name = 'C.' + p.check_name()
			language = .c
		} else {
			p.error_with_pos('single letter capital names are reserved for generic template types',
				name_pos)
			return ast.FnTypeDecl{}
		}
	}
	if name in p.imported_symbols {
		p.error_with_pos('cannot register alias `${name}`, this type was already imported',
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
		fn_type := p.parse_fn_type(fn_name, generic_types)
		p.table.sym(fn_type).is_pub = is_pub
		type_pos = type_pos.extend(p.tok.pos())
		comments = p.eat_comments(same_line: true)
		p.attrs = []
		return ast.FnTypeDecl{
			name:          fn_name
			is_pub:        is_pub
			typ:           fn_type
			pos:           decl_pos
			type_pos:      type_pos
			comments:      comments
			generic_types: generic_types
			attrs:         attrs
			is_markused:   attrs.contains('markused')
		}
	}
	sum_variants << p.parse_sum_type_variants()
	// type SumType = Aaa | Bbb | Ccc
	if sum_variants.len > 1 {
		for variant in sum_variants {
			if variant.typ == 0 {
				// the type symbol is probably coming from another .v file
				continue
			}
			variant_sym := p.table.sym(variant.typ)
			// TODO: implement this check for error too
			if variant_sym.kind == .none {
				p.error_with_pos('named sum type cannot have none as its variant', variant.pos)
				return ast.AliasTypeDecl{}
			}
		}
		variant_types := sum_variants.map(it.typ)
		prepend_mod_name := p.prepend_mod(name)
		typ := p.table.register_sym(ast.TypeSymbol{
			kind:   .sum_type
			name:   prepend_mod_name
			cname:  util.no_dots(prepend_mod_name)
			mod:    p.mod
			info:   ast.SumType{
				variants:      variant_types
				is_generic:    generic_types.len > 0
				generic_types: generic_types
			}
			is_pub: is_pub
		})
		if typ in [ast.string_type_idx, ast.rune_type_idx, ast.array_type_idx, ast.map_type_idx] {
			p.error_with_pos('cannot register sum type `${name}`, another type with this name exists',
				name_pos)
			return ast.SumTypeDecl{}
		}
		node := ast.SumTypeDecl{
			name:          name
			typ:           typ
			is_pub:        is_pub
			variants:      sum_variants
			generic_types: generic_types
			attrs:         p.attrs
			pos:           decl_pos
			name_pos:      name_pos
			is_markused:   attrs.contains('markused')
		}
		p.table.register_sumtype(node)
		return node
	}
	// type MyType = int
	if generic_types.len > 0 {
		p.error_with_pos('generic type aliases are not yet implemented', decl_pos_with_generics)
		return ast.AliasTypeDecl{}
	}
	// sum_variants will have only one element
	parent_type := sum_variants[0].typ
	pidx := parent_type.idx()
	mut parent_language := ast.Language.v
	if parent_type != 0 {
		parent_sym := p.table.sym(parent_type)
		parent_language = parent_sym.language
		p.check_for_impure_v(parent_sym.language, decl_pos)
	}
	prepend_mod_name := if language == .v { p.prepend_mod(name) } else { name } // `C.time_t`, not `time.C.time_t`
	idx := p.table.register_sym(ast.TypeSymbol{
		kind:       .alias
		name:       prepend_mod_name
		cname:      util.no_dots(prepend_mod_name)
		mod:        p.mod
		parent_idx: pidx
		info:       ast.Alias{
			parent_type: parent_type
			language:    parent_language
		}
		is_pub:     is_pub
	})
	type_end_pos := p.prev_tok.pos()
	if idx in [ast.string_type_idx, ast.rune_type_idx, ast.array_type_idx, ast.map_type_idx] {
		p.error_with_pos('cannot register alias `${name}`, another type with this name exists',
			name_pos)
		return ast.AliasTypeDecl{}
	}
	if idx == pidx {
		type_alias_pos := sum_variants[0].pos
		p.error_with_pos('a type alias can not refer to itself: ${name}', decl_pos.extend(type_alias_pos))
		return ast.AliasTypeDecl{}
	}
	comments = sum_variants[0].end_comments.clone()
	return ast.AliasTypeDecl{
		name:        name
		is_pub:      is_pub
		typ:         idx
		parent_type: parent_type
		type_pos:    type_pos.extend(type_end_pos)
		pos:         decl_pos
		comments:    comments
		is_markused: attrs.contains('markused')
	}
}

fn (mut p Parser) assoc() ast.Assoc {
	var_name := p.check_name()
	pos := p.tok.pos()
	mut v := p.scope.find_var(var_name) or {
		p.error('unknown variable `${var_name}`')
		return ast.Assoc{
			scope: unsafe { nil }
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
		fields:   fields
		exprs:    vals
		pos:      pos
		scope:    p.scope
	}
}

fn (p &Parser) new_true_expr() ast.Expr {
	return ast.BoolLiteral{
		val: true
		pos: p.tok.pos()
	}
}

fn (mut p Parser) top_level_statement_start() {
	if p.scanner.comments_mode == .toplevel_comments {
		p.scanner.set_is_inside_toplevel_statement(true)
		p.rewind_scanner_to_current_token_in_new_mode()
		$if trace_scanner ? {
			eprintln('>> p.top_level_statement_start | tidx:${p.tok.tidx:-5} | p.tok.kind: ${p.tok.kind:-10} | p.tok.lit: ${p.tok.lit} ${p.peek_tok.lit} ${p.peek_token(2).lit} ${p.peek_token(3).lit} ...')
		}
	}
}

fn (mut p Parser) top_level_statement_end() {
	if p.scanner.comments_mode == .toplevel_comments {
		p.scanner.set_is_inside_toplevel_statement(false)
		p.rewind_scanner_to_current_token_in_new_mode()
		$if trace_scanner ? {
			eprintln('>> p.top_level_statement_end   | tidx:${p.tok.tidx:-5} | p.tok.kind: ${p.tok.kind:-10} | p.tok.lit: ${p.tok.lit} ${p.peek_tok.lit} ${p.peek_token(2).lit} ${p.peek_token(3).lit} ...')
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
			pos:       pos
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
					pos:  pos
				}
				// parse e.g. `unsafe {expr}.foo()`
				expr := p.expr_with_left(ue, 0, p.is_stmt_ident)
				return ast.ExprStmt{
					expr: expr
					pos:  pos
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
		stmts:     stmts
		is_unsafe: true
		pos:       pos
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

fn (mut p Parser) trace[T](fbase string, x &T) {
	if p.file_base == fbase {
		println('> p.trace | ${fbase:-10s} | ${voidptr(x):16} | ${x}')
	}
}

@[params]
struct ParserShowParams {
pub:
	msg   string
	reach int = 3
}

fn (mut p Parser) show(params ParserShowParams) {
	mut context := []string{}
	for i in -params.reach .. params.reach + 1 {
		x := p.peek_token(i).str()
		if i == 0 {
			context << '    ${x:-30s}  '
			continue
		}
		context << x
	}
	location := '${p.file_display_path}:${p.tok.line_nr}:'
	println('>> ${location:-40s} ${params.msg} ${context.join('  ')}')
}

fn (mut p Parser) add_defer_var(ident ast.Ident) {
	if p.inside_defer {
		if !p.defer_vars.any(it.name == ident.name && it.mod == ident.mod)
			&& ident.name !in ['err', 'it'] {
			p.defer_vars << ident
		}
	}
}
