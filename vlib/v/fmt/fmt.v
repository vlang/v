// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import os
import strings
import v.ast
import v.util
import v.pref

const break_points = [0, 35, 60, 85, 93, 100]! // when to break a line depending on the penalty
const max_len = break_points[break_points.len - 1]
const bs = '\\'

@[minify]
pub struct Fmt {
pub:
	pref &pref.Preferences = unsafe { nil }
pub mut:
	file               ast.File
	table              &ast.Table = unsafe { nil }
	is_debug           bool
	out                strings.Builder
	out_imports        strings.Builder
	indent             int
	empty_line         bool
	line_len           int    // the current line length, Note: it counts \t as 4 spaces, and starts at 0 after f.writeln
	buffering          bool   // disables line wrapping for exprs that will be analyzed later
	par_level          int    // how many parentheses are put around the current expression
	array_init_break   []bool // line breaks after elements in hierarchy level of multi dimensional array
	array_init_depth   int    // current level of hierarchy in array init
	single_line_if     bool
	cur_mod            string
	did_imports        bool
	import_pos         int               // position of the imports in the resulting string
	auto_imports       map[string]bool   // potentially hidden imports(`sync` when using channels) and preludes(when embedding files)
	used_imports       map[string]bool   // to remove unused imports
	import_syms_used   map[string]bool   // to remove unused import symbols
	mod2alias          map[string]string // for `import time as t`, will contain: 'time'=>'t'
	mod2syms           map[string]string // import time { now } 'time.now'=>'now'
	use_short_fn_args  bool
	single_line_fields bool // should struct fields be on a single line
	in_lambda_depth    int
	inside_const       bool
	inside_unsafe      bool
	inside_comptime_if bool
	is_assign          bool
	is_index_expr      bool
	is_mbranch_expr    bool // match a { x...y { } }
	is_struct_init     bool
	fn_scope           &ast.Scope = unsafe { nil }
	wsinfix_depth      int
	format_state       FormatState
	source_text        string // can be set by `echo "println('hi')" | v fmt`, i.e. when processing source not from a file, but from stdin. In this case, it will contain the entire input text. You can use f.file.path otherwise, and read from that file.
}

@[params]
pub struct FmtOptions {
pub:
	source_text string
}

pub fn fmt(file ast.File, mut table ast.Table, pref_ &pref.Preferences, is_debug bool, options FmtOptions) string {
	mut f := Fmt{
		file:        file
		table:       table
		pref:        pref_
		is_debug:    is_debug
		out:         strings.new_builder(1000)
		out_imports: strings.new_builder(200)
	}
	f.source_text = options.source_text
	f.process_file_imports(file)
	// Compensate for indent increase of toplevel stmts done in `f.stmts()`.
	f.indent--
	f.stmts(file.stmts)
	f.indent++
	// Format after file import symbols are processed.
	f.imports(f.file.imports)
	res := f.out.str().trim_space() + '\n'
	if res.len == 1 {
		return f.out_imports.str().trim_space() + '\n'
	}
	if res.len <= f.import_pos {
		imp_str := f.out_imports.str().trim_space()
		if imp_str.len > 0 {
			return res + '\n' + imp_str + '\n'
		}
		return res
	}
	mut import_start_pos := f.import_pos
	if f.import_pos == 0 && file.stmts.len > 1 {
		// Check shebang.
		stmt := file.stmts[1]
		if stmt is ast.ExprStmt && stmt.expr is ast.Comment
			&& (stmt.expr as ast.Comment).text.starts_with('#!') {
			import_start_pos = stmt.pos.len
		}
	}
	return res[..import_start_pos] + f.out_imports.str() + res[import_start_pos..]
}

/*
// vfmt has a special type_to_str which calls Table.type_to_str, but does extra work.
// Having it here and not in Table saves cpu cycles when not running the compiler in vfmt mode.
pub fn (f &Fmt) type_to_str_using_aliases(typ ast.Type, import_aliases map[string]string) string {
	mut s := f.table.type_to_str_using_aliases(typ, import_aliases)
	if s.contains('Result') {
		println('${s}')
	}
	if s.starts_with('x.vweb') {
		s = s.replace_once('x.vweb.', 'veb.')
	}
	return s
}

pub fn (f &Fmt) type_to_str(typ ast.Type) string {
	return f.table.type_to_str(typ)
}
*/

pub fn (mut f Fmt) process_file_imports(file &ast.File) {
	for imp in file.imports {
		f.mod2alias[imp.mod] = imp.alias
		f.mod2alias[imp.mod.all_after('${file.mod.name}.')] = imp.alias
		for sym in imp.syms {
			f.mod2alias['${imp.mod}.${sym.name}'] = sym.name
			f.mod2alias['${imp.mod.all_after_last('.')}.${sym.name}'] = sym.name
			f.mod2alias[sym.name] = sym.name
			f.mod2syms['${imp.mod}.${sym.name}'] = sym.name
			f.mod2syms['${imp.mod.all_after_last('.')}.${sym.name}'] = sym.name
			f.mod2syms[sym.name] = sym.name
			f.import_syms_used[sym.name] = false
		}
	}
	for mod in f.file.auto_imports {
		f.auto_imports[mod] = true
	}
}

//=== Basic buffer write operations ===//

pub fn (mut f Fmt) write(s string) {
	if f.indent > 0 && f.empty_line {
		f.write_indent()
	}
	f.out.write_string(s)
	f.line_len += s.len
	f.empty_line = false
}

pub fn (mut f Fmt) writeln(s string) {
	if f.indent > 0 && f.empty_line && s != '' {
		f.write_indent()
	}
	f.out.writeln(s)
	f.empty_line = true
	f.line_len = 0
}

fn (mut f Fmt) write_indent() {
	f.out.write_string(util.tabs(f.indent))
	f.line_len += f.indent * 4
}

pub fn (mut f Fmt) wrap_long_line(penalty_idx int, add_indent bool) bool {
	if f.buffering {
		return false
	}
	if penalty_idx > 0 && f.line_len <= break_points[penalty_idx] {
		return false
	}
	if f.out.last() == ` ` {
		f.out.go_back(1)
	}
	f.write('\n')
	f.line_len = 0
	if add_indent {
		f.indent++
	}
	f.write_indent()
	if add_indent {
		f.indent--
	}
	return true
}

@[params]
pub struct RemoveNewLineConfig {
pub:
	imports_buffer bool // Work on f.out_imports instead of f.out
}

// When the removal action actually occurs, the string of the last line after the removal is returned
pub fn (mut f Fmt) remove_new_line(cfg RemoveNewLineConfig) string {
	mut buffer := if cfg.imports_buffer { unsafe { &f.out_imports } } else { unsafe { &f.out } }
	mut i := 0
	for i = buffer.len - 1; i >= 0; i-- {
		if !buffer.byte_at(i).is_space() { // != `\n` {
			break
		}
	}
	if i == buffer.len - 1 {
		return ''
	}
	buffer.go_back(buffer.len - i - 1)
	f.empty_line = false
	mut line_len := 0
	mut last_line_str := []u8{}
	for i = buffer.len - 1; i >= 0; i-- {
		ch := buffer.byte_at(i)
		if ch == `\n` {
			break
		}
		line_len += if ch == `\t` { 4 } else { 1 }
		last_line_str << ch
	}
	f.line_len = line_len
	return last_line_str.reverse().bytestr()
}

//=== Specialized write methods ===//

fn (mut f Fmt) write_language_prefix(lang ast.Language) {
	match lang {
		.c { f.write('C.') }
		.js { f.write('JS.') }
		.wasm { f.write('WASM.') }
		else {}
	}
}

fn (mut f Fmt) write_generic_types(gtypes []ast.Type) {
	if gtypes.len > 0 {
		f.write('[')
		gtypes_string := gtypes.map(f.table.type_to_str(it)).join(', ')
		f.write(gtypes_string)
		f.write(']')
	}
}

//=== Module handling helper methods ===//

pub fn (mut f Fmt) set_current_module_name(cmodname string) {
	f.cur_mod = cmodname
	f.table.cmod_prefix = cmodname + '.'
}

fn (f &Fmt) get_modname_prefix(mname string) (string, string) {
	// ./tests/proto_module_importing_vproto_keep.vv to know, why here is checked for ']' and '&'
	if !mname.contains(']') && !mname.contains('&') {
		return mname, ''
	}
	after_rbc := mname.all_after_last(']')
	after_ref := mname.all_after_last('&')
	modname := if after_rbc.len < after_ref.len { after_rbc } else { after_ref }
	return modname, mname.trim_string_right(modname)
}

fn (mut f Fmt) is_external_name(name string) bool {
	if name.len > 2 && name[0] == `C` && name[1] == `.` {
		return true
	}
	if name.len > 3 && name[0] == `J` && name[1] == `S` && name[2] == `.` {
		return true
	}
	return false
}

pub fn (mut f Fmt) no_cur_mod(typename string) string {
	return util.no_cur_mod(typename, f.cur_mod)
}

// foo.bar.fn() => bar.fn()
pub fn (mut f Fmt) short_module(name string) string {
	if !name.contains('.') || name.starts_with('JS.') {
		return name
	}
	if name in f.mod2syms {
		return f.mod2syms[name]
	}
	if name.ends_with(']') {
		generic_levels := name.trim_string_right(']').split('[')
		mut res := '${f.short_module(generic_levels[0])}'
		for i in 1 .. generic_levels.len {
			genshorts := generic_levels[i].split(', ').map(f.short_module(it)).join(', ')
			res += '[${genshorts}'
		}
		res += ']'
		return res
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	idx := vals.len - 1
	mname, tprefix := f.get_modname_prefix(vals[..idx].join('.'))
	symname := vals.last()
	mut aname := f.mod2alias[mname]
	if aname == '' {
		for _, v in f.mod2alias {
			if v == mname {
				aname = mname
				break
			}
		}
	}
	if aname == '' {
		return '${tprefix}${symname}'
	}
	return '${tprefix}${aname}.${symname}'
}

//=== Import-related methods ===//

pub fn (mut f Fmt) mark_types_import_as_used(typ ast.Type) {
	sym := f.table.sym(typ)
	match sym.info {
		ast.Map {
			map_info := sym.map_info()
			f.mark_types_import_as_used(map_info.key_type)
			f.mark_types_import_as_used(map_info.value_type)
			return
		}
		ast.Array, ast.ArrayFixed {
			f.mark_types_import_as_used(sym.info.elem_type)
			return
		}
		ast.GenericInst {
			for concrete_typ in sym.info.concrete_types {
				f.mark_types_import_as_used(concrete_typ)
			}
		}
		else {}
	}
	// `Type[T]` -> `Type` || `[]thread Type` -> `Type`.
	name := sym.name.all_before('[').all_after(' ')
	f.mark_import_as_used(name)
}

pub fn (mut f Fmt) mark_import_as_used(name string) {
	parts := name.split('.')
	sym := parts.last()
	if sym in f.import_syms_used {
		f.import_syms_used[sym] = true
	}
	if parts.len == 1 {
		return
	}
	mod := parts[..parts.len - 1].join('.')
	f.used_imports[mod] = true
}

pub fn (mut f Fmt) imports(imports []ast.Import) {
	if f.did_imports || imports.len == 0 {
		return
	}
	f.did_imports = true
	mut processed_imports := map[string]bool{}
	for imp in imports {
		if imp.mod in f.auto_imports && imp.mod !in f.used_imports {
			// Skip hidden imports like preludes.
			continue
		}
		imp_stmt := f.imp_stmt_str(imp)
		if imp_stmt in processed_imports {
			// Skip duplicates.
			f.import_comments(imp.next_comments)
			continue
		}
		processed_imports[imp_stmt] = true
		if !f.format_state.is_vfmt_on {
			original_imp_line := f.get_source_lines()#[imp.pos.line_nr..imp.pos.last_line + 1].join('\n')
			// Same line comments(`imp.comments`) are included in the `original_imp_line`.
			f.out_imports.writeln(original_imp_line)
			f.import_comments(imp.next_comments)
		} else {
			f.out_imports.writeln('import ${imp_stmt}')
			f.import_comments(imp.comments, same_line: true)
			f.import_comments(imp.next_comments)
		}
	}
	if processed_imports.len > 0 {
		f.out_imports.writeln('')
	}
}

pub fn (f &Fmt) imp_stmt_str(imp ast.Import) string {
	// Format / remove unused selective import symbols
	// E.g.: `import foo { Foo }` || `import foo as f { Foo }`
	has_alias := imp.alias != imp.source_name.all_after_last('.')
	mut suffix := if has_alias { ' as ${imp.alias}' } else { '' }
	mut syms := imp.syms.map(it.name).filter(f.import_syms_used[it])
	syms.sort()
	if syms.len > 0 {
		suffix += if imp.syms[0].pos.line_nr == imp.pos.line_nr {
			' { ' + syms.join(', ') + ' }'
		} else {
			' {\n\t' + syms.join(',\n\t') + ',\n}'
		}
	}
	return '${imp.source_name}${suffix}'
}

//=== Node helpers ===//

fn (f &Fmt) should_insert_newline_before_node(node ast.Node, prev_node ast.Node) bool {
	// No need to insert a newline if there is already one
	if f.out.last_n(2) == '\n\n' {
		return false
	}
	prev_line_nr := prev_node.pos().last_line
	// The nodes are Stmts
	if node is ast.Stmt && prev_node is ast.Stmt {
		match prev_node {
			// Force a newline after a block of HashStmts
			ast.HashStmt {
				if node !in [ast.HashStmt, ast.ExprStmt] {
					return true
				}
			}
			// Force a newline after function declarations
			// The only exception is inside a block of no_body functions
			ast.FnDecl {
				if node !is ast.FnDecl || !prev_node.no_body {
					return true
				}
			}
			ast.SemicolonStmt {
				return false
			}
			// Force a newline after struct declarations
			ast.StructDecl {
				return true
			}
			// Empty line after a block of type declarations
			ast.TypeDecl {
				if node !is ast.TypeDecl {
					return true
				}
			}
			// Imports are handled special hence they are ignored here
			ast.Import {
				return false
			}
			ast.ConstDecl {
				if node !is ast.ConstDecl && !(node is ast.ExprStmt && node.expr is ast.Comment) {
					return true
				}
			}
			else {}
		}
		match node {
			// Attributes are not respected in the stmts position, so this requires manual checking
			ast.StructDecl, ast.EnumDecl, ast.FnDecl {
				if node.attrs.len > 0 && node.attrs[0].pos.line_nr - prev_line_nr <= 1 {
					return false
				}
			}
			ast.Import {
				return false
			}
			else {}
		}
	}
	// The node shouldn't have a newline before
	if node.pos().line_nr - prev_line_nr <= 1 {
		return false
	}
	return true
}

pub fn (mut f Fmt) node_str(node ast.Node) string {
	was_empty_line := f.empty_line
	prev_line_len := f.line_len
	pos := f.out.len
	match node {
		ast.Stmt { f.stmt(node) }
		ast.Expr { f.expr(node) }
		else { panic('´f.node_str()´ is not implemented for ${node}.') }
	}
	str := f.out.after(pos)
	f.out.go_back_to(pos)
	f.empty_line = was_empty_line
	f.line_len = prev_line_len
	return str
}

//=== General Stmt-related methods and helpers ===//

pub fn (mut f Fmt) stmts(stmts []ast.Stmt) {
	mut prev_stmt := ast.empty_stmt
	f.indent++
	for i, stmt in stmts {
		if i > 0 && f.should_insert_newline_before_node(stmt, prev_stmt) {
			f.out.writeln('')
		}
		f.stmt(stmt)
		prev_stmt = stmt
	}
	f.indent--
}

pub fn (mut f Fmt) stmt(node ast.Stmt) {
	if f.is_debug {
		eprintln('stmt ${node.type_name():-20} | pos: ${node.pos.line_str()}')
	}
	match node {
		ast.EmptyStmt, ast.NodeError {}
		ast.AsmStmt {
			f.asm_stmt(node)
		}
		ast.AssertStmt {
			f.assert_stmt(node)
		}
		ast.AssignStmt {
			f.assign_stmt(node)
		}
		ast.Block {
			if node.is_unsafe {
				f.inside_unsafe = true
				f.block(node)
				f.inside_unsafe = false
			} else {
				f.block(node)
			}
		}
		ast.BranchStmt {
			f.branch_stmt(node)
		}
		ast.ComptimeFor {
			f.comptime_for(node)
		}
		ast.ConstDecl {
			f.const_decl(node)
		}
		ast.DebuggerStmt {
			f.debugger_stmt(node)
		}
		ast.DeferStmt {
			f.defer_stmt(node)
		}
		ast.EnumDecl {
			f.enum_decl(node)
		}
		ast.ExprStmt {
			f.expr_stmt(node)
		}
		ast.FnDecl {
			f.fn_decl(node)
		}
		ast.ForCStmt {
			f.for_c_stmt(node)
		}
		ast.ForInStmt {
			f.for_in_stmt(node)
		}
		ast.ForStmt {
			f.for_stmt(node)
		}
		ast.GlobalDecl {
			f.global_decl(node)
		}
		ast.GotoLabel {
			f.goto_label(node)
		}
		ast.GotoStmt {
			f.goto_stmt(node)
		}
		ast.HashStmt {
			f.hash_stmt(node)
		}
		ast.Import {
			// Imports are handled after the file is formatted, to automatically add necessary modules
			// Just remember the position of the imports for now
			f.import_pos = f.out.len
		}
		ast.InterfaceDecl {
			f.interface_decl(node)
		}
		ast.Module {
			f.module_stmt(node)
		}
		ast.Return {
			f.return_stmt(node)
		}
		ast.SemicolonStmt {}
		ast.SqlStmt {
			f.sql_stmt(node)
		}
		ast.StructDecl {
			f.struct_decl(node, false)
		}
		ast.TypeDecl {
			f.type_decl(node)
		}
	}
}

fn stmt_is_single_line(stmt ast.Stmt) bool {
	return match stmt {
		ast.ExprStmt, ast.AssertStmt { expr_is_single_line(stmt.expr) }
		ast.Return, ast.AssignStmt, ast.BranchStmt { true }
		ast.SemicolonStmt { true }
		else { false }
	}
}

//=== General Expr-related methods and helpers ===//

pub fn (mut f Fmt) expr(node_ ast.Expr) {
	mut node := unsafe { node_ }
	if f.is_debug {
		eprintln('expr ${node.type_name():-20} | pos: ${node.pos().line_str()} | ${node.str()}')
	}
	match mut node {
		ast.NodeError {}
		ast.EmptyExpr {}
		ast.AnonFn {
			f.anon_fn(node)
		}
		ast.ArrayDecompose {
			f.array_decompose(node)
		}
		ast.ArrayInit {
			f.array_init(node)
		}
		ast.AsCast {
			f.as_cast(node)
		}
		ast.Assoc {
			f.assoc(node)
		}
		ast.AtExpr {
			f.at_expr(node)
		}
		ast.BoolLiteral {
			f.write(node.val.str())
		}
		ast.CallExpr {
			f.call_expr(node)
		}
		ast.CastExpr {
			f.cast_expr(node)
		}
		ast.ChanInit {
			f.chan_init(mut node)
		}
		ast.CharLiteral {
			f.char_literal(node)
		}
		ast.Comment {
			f.comment(node, same_line: true)
		}
		ast.ComptimeCall {
			f.comptime_call(node)
		}
		ast.ComptimeSelector {
			f.comptime_selector(node)
		}
		ast.ConcatExpr {
			f.concat_expr(node)
		}
		ast.CTempVar {
			eprintln('ast.CTempVar of ${node.orig.str()} should be generated/used only in cgen')
		}
		ast.DumpExpr {
			f.dump_expr(node)
		}
		ast.EnumVal {
			f.enum_val(node)
		}
		ast.FloatLiteral {
			f.write(node.val)
			if node.val.ends_with('.') {
				f.write('0')
			}
		}
		ast.GoExpr {
			f.go_expr(node)
		}
		ast.SpawnExpr {
			f.spawn_expr(node)
		}
		ast.Ident {
			f.ident(node)
		}
		ast.IfExpr {
			f.if_expr(node)
		}
		ast.IfGuardExpr {
			f.if_guard_expr(node)
		}
		ast.IndexExpr {
			f.index_expr(node)
		}
		ast.InfixExpr {
			f.infix_expr(node)
		}
		ast.IntegerLiteral {
			f.write(node.val)
		}
		ast.LambdaExpr {
			f.write('|')
			for i, x in node.params {
				f.expr(x)
				if i < node.params.len - 1 {
					f.write(', ')
				}
			}
			f.write('| ')
			f.expr(node.expr)
		}
		ast.Likely {
			f.likely(node)
		}
		ast.LockExpr {
			f.lock_expr(node)
		}
		ast.MapInit {
			f.map_init(node)
		}
		ast.MatchExpr {
			f.match_expr(node)
		}
		ast.None {
			f.write('none')
		}
		ast.Nil {
			f.write('nil')
		}
		ast.OffsetOf {
			f.offset_of(node)
		}
		ast.OrExpr {
			// shouldn't happen, an or expression is always linked to a call expr or index expr
			panic('fmt: OrExpr should be linked to ast.CallExpr or ast.IndexExpr')
		}
		ast.ParExpr {
			f.par_expr(node)
		}
		ast.PostfixExpr {
			f.postfix_expr(node)
		}
		ast.PrefixExpr {
			f.prefix_expr(node)
		}
		ast.RangeExpr {
			f.range_expr(node)
		}
		ast.SelectExpr {
			f.select_expr(node)
		}
		ast.SelectorExpr {
			f.selector_expr(node)
		}
		ast.SizeOf {
			f.size_of(node)
		}
		ast.IsRefType {
			f.is_ref_type(node)
		}
		ast.SqlExpr {
			f.sql_expr(node)
		}
		ast.StringLiteral {
			f.string_literal(node)
		}
		ast.StringInterLiteral {
			f.string_inter_literal(node)
		}
		ast.StructInit {
			f.struct_init(node)
		}
		ast.TypeNode {
			f.type_expr(node)
		}
		ast.TypeOf {
			f.type_of(node)
		}
		ast.UnsafeExpr {
			f.inside_unsafe = true
			f.unsafe_expr(node)
			f.inside_unsafe = false
		}
		ast.ComptimeType {
			match node.kind {
				.unknown { f.write('\$unknown') }
				.array { f.write('\$array') }
				.array_dynamic { f.write('\$array_dynamic') }
				.array_fixed { f.write('\$array_fixed') }
				.struct { f.write('\$struct') }
				.iface { f.write('\$interface') }
				.map { f.write('\$map') }
				.int { f.write('\$int') }
				.float { f.write('\$float') }
				.sum_type { f.write('\$sumtype') }
				.enum { f.write('\$enum') }
				.alias { f.write('\$alias') }
				.function { f.write('\$function') }
				.option { f.write('\$option') }
				.string { f.write('\$string') }
				.pointer { f.write('\$pointer') }
				.voidptr { f.write('\$voidptr') }
			}
		}
	}
}

fn expr_is_single_line(expr ast.Expr) bool {
	match expr {
		ast.Comment, ast.IfExpr, ast.MapInit, ast.MatchExpr {
			return false
		}
		ast.AnonFn {
			if !expr.decl.no_body {
				return false
			}
		}
		ast.StructInit {
			if !expr.no_keys && (expr.init_fields.len > 0 || expr.pre_comments.len > 0) {
				return false
			}
		}
		ast.CallExpr {
			if expr.or_block.stmts.len > 1 || expr.args.any(it.expr is ast.CallExpr
				&& it.expr.or_block.stmts.len > 1) {
				return false
			}
		}
		ast.ArrayInit {
			for e in expr.exprs {
				if !expr_is_single_line(e) {
					return false
				}
			}
		}
		ast.ConcatExpr {
			for e in expr.vals {
				if !expr_is_single_line(e) {
					return false
				}
			}
		}
		ast.StringLiteral {
			return expr.pos.line_nr == expr.pos.last_line
		}
		else {}
	}
	return true
}

//=== Specific Stmt methods ===//

pub fn (mut f Fmt) assert_stmt(node ast.AssertStmt) {
	f.write('assert ')
	mut expr := node.expr
	expr = expr.remove_par()
	f.expr(expr)
	if node.extra !is ast.EmptyExpr {
		f.write(', ')
		f.expr(node.extra)
	}
	f.writeln('')
}

pub fn (mut f Fmt) assign_stmt(node ast.AssignStmt) {
	for i, left in node.left {
		f.expr(left)
		if i < node.left.len - 1 {
			f.write(', ')
		}
	}
	f.is_assign = true
	f.write(' ${node.op.str()} ')
	for i, val in node.right {
		f.expr(val)
		if i < node.right.len - 1 {
			f.write(', ')
		}
	}
	if node.attr.name != '' {
		f.write(' @[${node.attr.name}]')
	}
	f.comments(node.end_comments, has_nl: false, same_line: true, level: .keep)
	if !f.single_line_if {
		f.writeln('')
	}
	f.is_assign = false
}

pub fn (mut f Fmt) block(node ast.Block) {
	if node.is_unsafe {
		f.write('unsafe ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) debugger_stmt(node ast.DebuggerStmt) {
	f.writeln('\$dbg;')
}

pub fn (mut f Fmt) branch_stmt(node ast.BranchStmt) {
	f.writeln(node.str())
}

pub fn (mut f Fmt) comptime_for(node ast.ComptimeFor) {
	typ := if node.typ != ast.void_type {
		f.no_cur_mod(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	} else {
		(node.expr as ast.Ident).name
	}
	f.write('\$for ${node.val_var} in ${typ}.${node.kind.str()} {')
	if node.typ != ast.void_type {
		f.mark_types_import_as_used(node.typ)
	}
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) const_decl(node ast.ConstDecl) {
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		// remove "const()"
		return
	}

	f.attrs(node.attrs)
	if !node.is_block {
		if node.is_pub {
			f.write('pub ')
		}
	}
	f.inside_const = true
	defer { f.inside_const = false }
	if !node.is_block {
		f.write('const ')
	}
	mut prev_field := if node.fields.len > 0 {
		ast.Node(node.fields[0])
	} else {
		ast.Node(ast.NodeError{})
	}
	for fidx, field in node.fields {
		if field.comments.len > 0 {
			if f.should_insert_newline_before_node(ast.Expr(field.comments[0]), prev_field) {
				f.writeln('')
			}
			f.comments(field.comments, same_line: true)
			prev_field = ast.Expr(field.comments.last())
		}
		if node.is_block && f.should_insert_newline_before_node(field, prev_field) {
			f.writeln('')
		}
		name := field.name.after('.')
		if node.is_block {
			// const() blocks are deprecated, prepend "const" before each value
			if node.is_pub {
				f.write('pub ')
			}
			f.write('const ')
		}
		if field.is_virtual_c {
			f.write('C.')
		}
		f.write('${name} ')
		if field.is_virtual_c {
			// f.typ(field.typ)
			f.write(f.table.type_to_str(field.typ))
		} else {
			f.write('= ')
			f.expr(field.expr)
		}
		f.comments(field.end_comments, same_line: true)
		if node.is_block && fidx < node.fields.len - 1 && node.fields.len > 1 {
			// old style grouped consts, converted to the new style ungrouped const
			f.writeln('')
		} else if node.end_comments.len > 0 {
			// Write out single line comments after const expr if present
			// E.g.: `const x = 1 // <comment>`
			if node.end_comments[0].text.contains('\n') {
				f.writeln('\n')
			}
			f.comments(node.end_comments, same_line: true, has_nl: false)
		}
		prev_field = field
	}

	f.writeln('')
}

fn (mut f Fmt) defer_stmt(node ast.DeferStmt) {
	f.write('defer ')
	if node.stmts.len == 0 {
		f.writeln('{}')
	} else if node.stmts.len == 1 && node.pos.line_nr == node.pos.last_line
		&& stmt_is_single_line(node.stmts[0]) {
		f.write('{ ')
		// the control stmts (return/break/continue...) print a newline inside them,
		// so, since this'll all be on one line, trim any possible whitespace
		str := f.node_str(node.stmts[0]).trim_space()
		// single_line := ' defer { ${str} }'
		// if single_line.len + f.line_len <= fmt.max_len {
		// f.write(single_line)
		// return
		//}
		f.write(str)

		// f.stmt(node.stmts[0])
		f.writeln(' }')
	} else {
		f.writeln('{')
		f.stmts(node.stmts)
		f.writeln('}')
	}
}

pub fn (mut f Fmt) expr_stmt(node ast.ExprStmt) {
	f.comments(node.comments)
	f.expr(node.expr)
	if !f.single_line_if {
		f.writeln('')
	}
}

pub fn (mut f Fmt) enum_decl(node ast.EnumDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	mut name := node.name.after('.')
	if node.typ != ast.int_type && node.typ != ast.invalid_type {
		senum_type := f.table.type_to_str_using_aliases(node.typ, f.mod2alias)
		name += ' as ${senum_type}'
	}
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln('enum ${name} {}\n')
		return
	}
	f.writeln('enum ${name} {')
	f.comments(node.comments, same_line: true, level: .indent)

	mut value_align := new_field_align(use_break_line: true)
	mut attr_align := new_field_align(use_threshold: true)
	mut comment_align := new_field_align(use_threshold: true)
	for field in node.fields {
		if field.has_expr {
			value_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
		}
		attrs_len := inline_attrs_len(field.attrs)
		if field.attrs.len > 0 {
			if field.has_expr {
				attr_align.add_info(field.expr.str().len + 2, field.pos.line_nr, field.has_break_line)
			} else {
				attr_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
			}
		}
		if field.comments.len > 0 {
			if field.attrs.len > 0 {
				comment_align.add_info(attrs_len, field.pos.line_nr, field.has_break_line)
			} else if field.has_expr {
				comment_align.add_info(field.expr.str().len + 2, field.pos.line_nr, field.has_break_line)
			} else {
				comment_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
			}
		}
	}

	for i, field in node.fields {
		if i > 0 && field.has_prev_newline {
			f.writeln('')
		}
		if field.pre_comments.len > 0 {
			f.comments(field.pre_comments, has_nl: true, level: .indent)
		}
		f.write('\t${field.name}')
		if field.has_expr {
			f.write(' '.repeat(value_align.max_len(field.pos.line_nr) - field.name.len))
			f.write(' = ')
			f.expr(field.expr)
		}
		attrs_len := inline_attrs_len(field.attrs)
		if field.attrs.len > 0 {
			if field.has_expr {
				f.write(' '.repeat(attr_align.max_len(field.pos.line_nr) - field.expr.str().len - 1))
			} else {
				f.write(' '.repeat(attr_align.max_len(field.pos.line_nr) - field.name.len + 1))
			}
			f.single_line_attrs(field.attrs, same_line: true)
		}
		// f.comments(field.comments, same_line: true, has_nl: false, level: .indent)
		if field.comments.len > 0 {
			if field.attrs.len > 0 {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - attrs_len + 1))
			} else if field.has_expr {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - field.expr.str().len - 1))
			} else {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - field.name.len + 1))
			}
			f.comments(field.comments, same_line: true, has_nl: false)
		}
		f.writeln('')
		f.comments(field.next_comments, has_nl: true, level: .indent)
	}
	f.writeln('}\n')
}

pub fn (mut f Fmt) fn_decl(node ast.FnDecl) {
	f.attrs(node.attrs)
	f.write(f.table.stringify_fn_decl(&node, f.cur_mod, f.mod2alias, true))
	// Handle trailing comments after fn header declarations
	if node.no_body && node.end_comments.len > 0 {
		first_comment := node.end_comments[0]
		if first_comment.text.contains('\n') {
			f.writeln('\n')
		} else {
			f.write(' ')
		}
		f.comment(first_comment)
		if node.end_comments.len > 1 {
			f.writeln('\n')
			comments := node.end_comments[1..]
			for i, comment in comments {
				f.comment(comment)
				if i != comments.len - 1 {
					f.writeln('\n')
				}
			}
		}
	}
	f.fn_body(node)
}

pub fn (mut f Fmt) anon_fn(node ast.AnonFn) {
	f.write(f.table.stringify_anon_decl(&node, f.cur_mod, f.mod2alias)) // `Expr` instead of `ast.Expr` in mod ast
	f.fn_body(node.decl)
}

fn (mut f Fmt) fn_body(node ast.FnDecl) {
	prev_fn_scope := f.fn_scope
	f.fn_scope = node.scope
	defer { f.fn_scope = prev_fn_scope }
	if node.language == .v || (node.is_method && node.language == .js) {
		if !node.no_body {
			f.write(' {')
			pre_comments := node.comments.filter(it.pos.pos < node.name_pos.pos)
			body_comments := node.comments[pre_comments.len..]
			f.comments(body_comments, same_line: true)
			if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
				if body_comments.len == 0 {
					f.writeln('')
				}
				f.stmts(node.stmts)
			}
			f.write('}')
			if node.end_comments.len > 0 {
				first_comment := node.end_comments[0]
				if first_comment.text.contains('\n') {
					f.writeln('\n')
				} else {
					f.write(' ')
				}
				f.comment(first_comment)
				if node.end_comments.len > 1 {
					f.writeln('\n')
					comments := node.end_comments[1..]
					for i, comment in comments {
						f.comment(comment)
						if i != comments.len - 1 {
							f.writeln('\n')
						}
					}
				}
			}
		}
		if !node.is_anon {
			f.writeln('')
		}
	} else {
		f.writeln('')
	}
	// Mark all function's used type so that they are not removed from imports
	for arg in node.params {
		f.mark_types_import_as_used(arg.typ)
	}
	f.mark_types_import_as_used(node.return_type)
}

pub fn (mut f Fmt) for_c_stmt(node ast.ForCStmt) {
	if node.label.len > 0 {
		f.write('${node.label}: ')
	}
	init_comments := node.comments.filter(it.pos.pos < node.init.pos.pos)
	cond_comments := node.comments[init_comments.len..].filter(it.pos.pos < node.cond.pos().pos)
	inc_comments := node.comments[(init_comments.len + cond_comments.len)..].filter(it.pos.pos < node.inc.pos.pos)
	after_inc_comments := node.comments[(init_comments.len + cond_comments.len + inc_comments.len)..]
	f.write('for ')
	if node.has_init {
		if init_comments.len > 0 {
			f.comments(init_comments)
			f.write(' ')
		}
		f.single_line_if = true // to keep all for ;; exprs on the same line
		f.stmt(node.init)
		f.single_line_if = false
	}
	f.write('; ')
	if cond_comments.len > 0 {
		f.comments(cond_comments)
		f.write(' ')
	}
	f.expr(node.cond)
	f.write('; ')
	if inc_comments.len > 0 {
		f.comments(inc_comments)
		f.write(' ')
	}
	f.stmt(node.inc)
	f.remove_new_line()
	if after_inc_comments.len > 0 {
		f.comments(after_inc_comments)
	}
	if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) for_in_stmt(node ast.ForInStmt) {
	if node.label.len > 0 {
		f.write('${node.label}: ')
	}
	kv_comments := node.comments.filter(it.pos.pos < node.kv_pos.pos)
	cond_comments := node.comments[kv_comments.len..].filter(it.pos.pos < node.cond.pos().pos)
	after_comments := node.comments[(kv_comments.len + cond_comments.len)..]
	f.write('for ')
	if kv_comments.len > 0 {
		f.comments(kv_comments)
		f.write(' ')
	}
	if node.key_var != '' {
		f.write(node.key_var)
	}
	if node.val_var != '' {
		if node.key_var != '' {
			f.write(', ')
		}
		if node.val_is_mut {
			f.write('mut ')
		}
		f.write(node.val_var)
	}
	f.write(' in ')
	if cond_comments.len > 0 {
		f.comments(cond_comments)
		f.write(' ')
	}
	f.expr(node.cond)
	if node.is_range {
		f.write(' .. ')
		f.expr(node.high)
	}
	if after_comments.len > 0 {
		f.comments(after_comments)
	}
	if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) for_stmt(node ast.ForStmt) {
	if node.label.len > 0 {
		f.write('${node.label}: ')
	}
	f.write('for ')
	if node.comments.len > 0 {
		f.comments(node.comments)
	}
	f.expr(node.cond)
	if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) global_decl(node ast.GlobalDecl) {
	f.attrs(node.attrs)
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		// remove "__global()"
		return
	}
	f.write('__global ')
	mut max := 0
	if node.is_block {
		f.writeln('(')
		f.indent++
		for field in node.fields {
			if field.name.len > max {
				max = field.name.len
			}
		}
	}
	for field in node.fields {
		f.comments(field.comments, same_line: true)
		if field.is_volatile {
			f.write('volatile ')
		}
		f.write('${field.name} ')
		f.write(' '.repeat(max - field.name.len))
		if field.has_expr {
			f.write('= ')
			f.expr(field.expr)
		} else {
			f.write('${f.table.type_to_str_using_aliases(field.typ, f.mod2alias)}')
		}
		if node.is_block {
			f.writeln('')
		}
		f.mark_types_import_as_used(field.typ)
	}
	f.comments_after_last_field(node.end_comments)
	if node.is_block {
		f.indent--
		f.writeln(')')
	} else {
		f.writeln('')
	}
}

pub fn (mut f Fmt) spawn_expr(node ast.SpawnExpr) {
	f.write('spawn ')
	f.call_expr(node.call_expr)
}

pub fn (mut f Fmt) go_expr(node ast.GoExpr) {
	f.write('go ')
	f.call_expr(node.call_expr)
}

pub fn (mut f Fmt) goto_label(node ast.GotoLabel) {
	f.writeln('${node.name}:')
}

pub fn (mut f Fmt) goto_stmt(node ast.GotoStmt) {
	f.writeln('goto ${node.name}')
}

pub fn (mut f Fmt) hash_stmt(node ast.HashStmt) {
	f.attrs(node.attrs)
	f.writeln('#${node.val}')
}

pub fn (mut f Fmt) interface_decl(node ast.InterfaceDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	f.write('interface ')
	f.write_language_prefix(node.language)
	name := node.name.after('.') // strip prepended module
	f.write(name)
	f.write_generic_types(node.generic_types)
	f.write(' {')
	if node.fields.len > 0 || node.methods.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
	}
	f.comments_before_field(node.pre_comments)
	for embed in node.embeds {
		f.write('\t${embed.name}')
		f.comments(embed.comments, same_line: true, has_nl: false, level: .indent)
		f.writeln('')
	}
	immut_fields := if node.mut_pos < 0 { node.fields } else { node.fields[..node.mut_pos] }
	mut_fields := if node.mut_pos < 0 { []ast.StructField{} } else { node.fields[node.mut_pos..] }

	mut immut_methods := node.methods.clone()
	mut mut_methods := []ast.FnDecl{}
	for i, method in node.methods {
		if method.params[0].is_mut {
			immut_methods = node.methods[..i].clone()
			mut_methods = node.methods[i..].clone()
			break
		}
	}

	mut type_align := new_field_align(use_break_line: true)
	mut comment_align := new_field_align(use_threshold: true)
	mut default_expr_align := new_field_align(use_threshold: true)
	mut attr_align := new_field_align(use_threshold: true)
	mut field_types := []string{cap: node.fields.len}

	// Calculate the alignments first
	f.calculate_alignment(node.fields, mut type_align, mut comment_align, mut default_expr_align, mut
		attr_align, mut field_types)

	mut method_comment_align := new_field_align(use_threshold: true)
	for method in node.methods {
		end_comments := method.comments.filter(it.pos.pos > method.pos.pos)
		if end_comments.len > 0 {
			method_str := f.table.stringify_fn_decl(&method, f.cur_mod, f.mod2alias, false).all_after_first('fn ')
			method_comment_align.add_info(method_str.len, method.pos.line_nr, method.has_break_line)
		}
	}

	// TODO: alignment, comments, etc.
	for field in immut_fields {
		if field.has_prev_newline {
			f.writeln('')
		}
		f.interface_field(field, mut type_align, mut comment_align)
	}
	for method in immut_methods {
		if method.has_prev_newline {
			f.writeln('')
		}
		f.interface_method(method, mut method_comment_align)
	}
	if mut_fields.len + mut_methods.len > 0 {
		f.writeln('mut:')
		for field in mut_fields {
			if field.has_prev_newline {
				f.writeln('')
			}
			f.interface_field(field, mut type_align, mut comment_align)
		}
		for method in mut_methods {
			if method.has_prev_newline {
				f.writeln('')
			}
			f.interface_method(method, mut method_comment_align)
		}
	}
	f.writeln('}\n')
}

enum AlignState {
	plain
	has_attributes
	has_default_expression
	has_everything
}

pub fn (mut f Fmt) calculate_alignment(fields []ast.StructField, mut type_align FieldAlign, mut comment_align FieldAlign,
	mut default_expr_align FieldAlign, mut attr_align FieldAlign, mut field_types []string) {
	// Calculate the alignments first
	mut prev_state := AlignState.plain
	for field in fields {
		ft := f.no_cur_mod(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
		// Handle anon structs recursively
		field_types << ft
		attrs_len := inline_attrs_len(field.attrs)
		end_pos := field.pos.pos + field.pos.len
		type_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
		if field.has_default_expr {
			default_expr_align.add_info(ft.len, field.pos.line_nr, field.has_break_line)
		}
		if field.attrs.len > 0 {
			attr_align.add_info(ft.len, field.pos.line_nr, field.has_break_line)
		}
		for comment in field.comments {
			if comment.pos.pos >= end_pos {
				if comment.pos.line_nr == field.pos.line_nr {
					if field.attrs.len > 0 {
						if prev_state != AlignState.has_attributes {
							comment_align.add_new_info(attrs_len, comment.pos.line_nr)
						} else {
							comment_align.add_info(attrs_len, comment.pos.line_nr, field.has_break_line)
						}
						prev_state = AlignState.has_attributes
					} else if field.has_default_expr {
						if prev_state != AlignState.has_default_expression {
							comment_align.add_new_info(field.default_expr.str().len + 2,
								comment.pos.line_nr)
						} else {
							comment_align.add_info(field.default_expr.str().len + 2, comment.pos.line_nr,
								field.has_break_line)
						}
						prev_state = AlignState.has_default_expression
					} else {
						if prev_state != AlignState.has_everything {
							comment_align.add_new_info(ft.len, comment.pos.line_nr)
						} else {
							comment_align.add_info(ft.len, comment.pos.line_nr, field.has_break_line)
						}
						prev_state = AlignState.has_everything
					}
				}
				continue
			}
		}
	}
}

pub fn (mut f Fmt) interface_field(field ast.StructField, mut type_align FieldAlign, mut comment_align FieldAlign) {
	ft := f.no_cur_mod(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
	mut pre_cmts, mut end_cmts, mut next_line_cmts := []ast.Comment{}, []ast.Comment{}, []ast.Comment{}
	for cmt in field.comments {
		match true {
			cmt.pos.pos < field.pos.pos { pre_cmts << cmt }
			cmt.pos.line_nr > field.pos.last_line { next_line_cmts << cmt }
			else { end_cmts << cmt }
		}
	}
	if pre_cmts.len > 0 {
		f.comments(pre_cmts, level: .indent)
	}

	sym := f.table.sym(field.typ)
	if sym.info is ast.Struct {
		if sym.info.is_anon {
			f.write('\t${field.name} ')
			f.write_anon_struct_field_decl(field.typ, ast.StructDecl{ fields: sym.info.fields })
		} else {
			f.write('\t${field.name} ')
		}
	} else {
		f.write('\t${field.name} ')
	}
	if !(sym.info is ast.Struct && sym.info.is_anon) {
		f.write(' '.repeat(type_align.max_len(field.pos.line_nr) - field.name.len))
		f.write(ft)
	}
	if end_cmts.len > 0 {
		f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - ft.len + 1))
		f.comments(end_cmts, level: .indent)
	} else {
		f.writeln('')
	}
	if next_line_cmts.len > 0 {
		f.comments(next_line_cmts, level: .indent)
	}
	f.mark_types_import_as_used(field.typ)
}

pub fn (mut f Fmt) interface_method(method ast.FnDecl, mut comment_align FieldAlign) {
	before_comments := method.comments.filter(it.pos.pos < method.pos.pos)
	end_comments := method.comments.filter(it.pos.pos > method.pos.pos)
	if before_comments.len > 0 {
		f.comments(before_comments, level: .indent)
	}
	f.write('\t')
	method_str := f.table.stringify_fn_decl(&method, f.cur_mod, f.mod2alias, false).all_after_first('fn ')
	f.write(method_str)
	if end_comments.len > 0 {
		f.write(' '.repeat(comment_align.max_len(method.pos.line_nr) - method_str.len + 1))
		f.comments(end_comments, level: .indent)
	} else {
		f.writeln('')
	}
	f.comments(method.next_comments, level: .indent)
	for param in method.params {
		f.mark_types_import_as_used(param.typ)
	}
	f.mark_types_import_as_used(method.return_type)
}

pub fn (mut f Fmt) module_stmt(mod ast.Module) {
	f.set_current_module_name(mod.name)
	if mod.is_skipped {
		return
	}
	f.attrs(mod.attrs)
	f.writeln('module ${mod.short_name}\n')
	if f.import_pos == 0 {
		f.import_pos = f.out.len
	}
}

pub fn (mut f Fmt) return_stmt(node ast.Return) {
	f.write('return')
	if node.exprs.len > 0 {
		f.write(' ')
		mut sum_len := 0
		// Loop over all return values. In normal returns this will only run once.
		for i, expr in node.exprs {
			pre_comments := node.comments[sum_len..].filter(it.pos.pos < expr.pos().pos)
			sum_len += pre_comments.len
			if pre_comments.len > 0 {
				f.comments(pre_comments)
				f.write(' ')
			}
			if expr is ast.ParExpr && expr.comments.len == 0 {
				f.expr(expr.expr)
			} else {
				f.expr(expr)
			}
			if i < node.exprs.len - 1 {
				f.write(', ')
			}
		}
	}
	if !f.single_line_if {
		f.writeln('')
	}
}

pub fn (mut f Fmt) sql_stmt(node ast.SqlStmt) {
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')

	for line in node.lines {
		f.comments(line.pre_comments, level: .indent)
		f.sql_stmt_line(line)
		f.comments(line.end_comments, level: .indent)
	}
	f.write('}')
	f.or_expr(node.or_expr)
	f.writeln('')
}

pub fn (mut f Fmt) sql_stmt_line(node ast.SqlStmtLine) {
	sym := f.table.sym(node.table_expr.typ)
	mut table_name := sym.name
	if !table_name.starts_with('C.') && !table_name.starts_with('JS.') {
		table_name = f.no_cur_mod(f.short_module(sym.name)) // TODO: f.type_to_str?
	}

	f.mark_types_import_as_used(node.table_expr.typ)
	f.write('\t')
	match node.kind {
		.insert {
			f.writeln('insert ${node.object_var} into ${table_name}')
		}
		.update {
			f.write('update ${table_name} set ')
			for i, col in node.updated_columns {
				f.write('${col} = ')
				f.expr(node.update_exprs[i])
				if i < node.updated_columns.len - 1 {
					f.write(', ')
				} else {
					f.write(' ')
				}
				f.wrap_long_line(3, true)
			}
			f.write('where ')
			f.expr(node.where_expr)
			f.writeln('')
		}
		.delete {
			f.write('delete from ${table_name} where ')
			f.expr(node.where_expr)
			f.writeln('')
		}
		.create {
			f.writeln('create table ${table_name}')
		}
		.drop {
			f.writeln('drop table ${table_name}')
		}
	}
}

pub fn (mut f Fmt) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl { f.alias_type_decl(node) }
		ast.FnTypeDecl { f.fn_type_decl(node) }
		ast.SumTypeDecl { f.sum_type_decl(node) }
	}
	f.writeln('')
}

pub fn (mut f Fmt) alias_type_decl(node ast.AliasTypeDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	// aliases of anon struct: `type Foo = struct {}`
	sym := f.table.sym(node.parent_type)
	if sym.info is ast.Struct {
		if sym.info.is_anon {
			f.write('type ${node.name} = ')
			f.struct_decl(ast.StructDecl{ fields: sym.info.fields }, true)
			f.comments(node.comments, has_nl: false)
			f.mark_types_import_as_used(node.parent_type)
			return
		}
	}
	ptype := f.table.type_to_str_using_aliases(node.parent_type, f.mod2alias)
	f.write('type ${node.name} = ${ptype}')

	f.comments(node.comments, has_nl: false)
	f.mark_types_import_as_used(node.parent_type)
}

pub fn (mut f Fmt) fn_type_decl(node ast.FnTypeDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	typ_sym := f.table.sym(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	fn_name := f.no_cur_mod(node.name)
	mut generic_types_str := ''
	if node.generic_types.len > 0 {
		generic_names := node.generic_types.map(f.table.sym(it).name)
		generic_types_str = '[${generic_names.join(', ')}]'
	}
	f.write('type ${fn_name}${generic_types_str} = fn (')
	for i, arg in fn_info.params {
		if arg.is_mut {
			f.write(arg.typ.share().str() + ' ')
		}
		f.write(arg.name)
		f.mark_types_import_as_used(arg.typ)
		mut s := f.no_cur_mod(f.table.type_to_str_using_aliases(arg.typ, f.mod2alias))
		if arg.is_mut {
			if s.starts_with('&') {
				s = s[1..]
			}
			s = s.trim_left('shared ')
		}
		is_last_arg := i == fn_info.params.len - 1
		should_add_type := true || is_last_arg
			|| fn_info.params[i + 1].typ != arg.typ
			|| (fn_info.is_variadic && i == fn_info.params.len - 2)
		if should_add_type {
			ns := if arg.name == '' { '' } else { ' ' }
			if fn_info.is_variadic && is_last_arg {
				f.write(ns + '...' + s)
			} else {
				f.write(ns + s)
			}
		}
		if !is_last_arg {
			f.write(', ')
		}
	}
	f.write(')')
	if fn_info.return_type.idx() != ast.void_type_idx {
		f.mark_types_import_as_used(fn_info.return_type)
		ret_str := f.no_cur_mod(f.table.type_to_str_using_aliases(fn_info.return_type,
			f.mod2alias))
		f.write(' ${ret_str}')
	} else if fn_info.return_type.has_flag(.option) {
		f.write(' ?')
	} else if fn_info.return_type.has_flag(.result) {
		f.write(' !')
	}

	f.comments(node.comments, has_nl: false)
	f.writeln('')
}

struct Variant {
	name string
	id   int
}

pub fn (mut f Fmt) sum_type_decl(node ast.SumTypeDecl) {
	f.attrs(node.attrs)
	start_pos := f.out.len
	if node.is_pub {
		f.write('pub ')
	}
	f.write('type ${node.name}')
	f.write_generic_types(node.generic_types)
	f.write(' = ')

	mut variants := []Variant{cap: node.variants.len}
	for i, variant in node.variants {
		variants << Variant{f.table.type_to_str_using_aliases(variant.typ, f.mod2alias), i}
		f.mark_types_import_as_used(variant.typ)
	}
	// The first variant is now used as the default variant when doing `a:= Sumtype{}`, i.e. a change in semantics.
	// Sorting is disabled, because it is no longer a cosmetic change - it can change the default variant.
	// variants.sort(a.name < b.name)

	mut separator := ' | '
	mut is_multiline := false
	// if line length is too long, put each type on its own line
	mut line_length := f.out.len - start_pos
	for variant in variants {
		// 3 = length of ' = ' or ' | '
		line_length += 3 + variant.name.len
		if line_length > max_len || (variant.id != node.variants.len - 1
			&& node.variants[variant.id].end_comments.len > 0) {
			separator = '\n\t| '
			is_multiline = true
			break
		}
	}

	for i, variant in variants {
		if i > 0 {
			f.write(separator)
		}
		f.write(variant.name)
		if node.variants[variant.id].end_comments.len > 0 && is_multiline {
			f.comments(node.variants[variant.id].end_comments, has_nl: false)
		}
	}
	if !is_multiline {
		f.comments(node.variants.last().end_comments,
			has_nl: false
		)
	}
}

//=== Specific Expr methods ===//

pub fn (mut f Fmt) array_decompose(node ast.ArrayDecompose) {
	f.write('...')
	f.expr(node.expr)
}

pub fn (mut f Fmt) array_init(node ast.ArrayInit) {
	if node.is_fixed && node.is_option {
		f.write('?')
	}
	if node.exprs.len == 0 && node.typ != 0 && node.typ != ast.void_type {
		// `x := []string{}`
		f.mark_types_import_as_used(node.typ)
		if node.alias_type != ast.void_type {
			f.write(f.table.type_to_str_using_aliases(node.alias_type, f.mod2alias))
		} else {
			f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		}
		f.write('{')
		if node.has_len {
			f.write('len: ')
			f.expr(node.len_expr)
			if node.has_cap || node.has_init {
				f.write(', ')
			}
		}
		if node.has_cap {
			f.write('cap: ')
			f.expr(node.cap_expr)
			if node.has_init {
				f.write(', ')
			}
		}
		if node.has_init {
			f.write('init: ')
			f.expr(node.init_expr)
		}
		f.write('}')
		return
	}
	// `[1,2,3]`
	f.write('[')
	mut inc_indent := false
	mut last_line_nr := node.pos.line_nr // to have the same newlines between array elements
	f.array_init_depth++
	if node.pre_cmnts.len > 0 {
		if node.pre_cmnts[0].pos.line_nr > last_line_nr {
			f.writeln('')
		}
	}
	for i, c in node.pre_cmnts {
		if i < node.pre_cmnts.len - 1 {
			if c.pos.last_line < node.pre_cmnts[i + 1].pos.line_nr {
				f.comment(c, level: .indent)
				f.writeln('')
			} else {
				f.comment(c, level: .indent)
				f.write(' ')
			}
		} else {
			next_line := if node.exprs.len > 0 {
				node.exprs[0].pos().line_nr
			} else {
				node.pos.last_line
			}
			if c.pos.last_line < next_line {
				f.comment(c, level: .indent)
				if node.exprs.len == 0 {
					f.writeln('')
				}
			} else {
				f.comment(c, level: .indent)
				if node.exprs.len > 0 {
					f.write(' ')
				}
			}
		}
		last_line_nr = c.pos.last_line
	}
	mut set_comma := false
	for i, expr in node.exprs {
		pos := expr.pos()
		if i == 0 {
			if f.array_init_depth > f.array_init_break.len {
				f.array_init_break << pos.line_nr > last_line_nr
					|| f.line_len + expr.pos().len > break_points[3]
			}
		}
		mut line_break := f.array_init_break[f.array_init_depth - 1]
		mut penalty := if line_break { 0 } else { 4 }
		if penalty > 0 {
			if i == 0
				|| node.exprs[i - 1] in [ast.ArrayInit, ast.StructInit, ast.MapInit, ast.CallExpr] {
				penalty--
			}
			if expr in [ast.ArrayInit, ast.StructInit, ast.MapInit, ast.CallExpr] {
				penalty--
			}
		}
		mut is_new_line := f.wrap_long_line(penalty, !inc_indent)
		if is_new_line && !inc_indent {
			f.indent++
			inc_indent = true
		}
		single_line_expr := expr_is_single_line(expr)
		if single_line_expr {
			mut estr := ''
			if !is_new_line && !f.buffering && f.line_len + expr.pos().len > max_len {
				if inc_indent {
					estr = f.node_str(expr)
				}
				f.writeln('')
				is_new_line = true
				if !inc_indent {
					f.indent++
					inc_indent = true
					f.write_indent()
					f.empty_line = false
					estr = f.node_str(expr)
				}
				if i == 0 {
					f.array_init_break[f.array_init_depth - 1] = true
					line_break = true
				}
			} else {
				estr = f.node_str(expr)
			}
			if !is_new_line && i > 0 {
				f.write(' ')
			}
			f.write(estr)
		} else {
			if !is_new_line && i > 0 {
				f.write(' ')
			}
			f.expr(expr)
		}
		mut last_comment_was_inline := false
		mut has_comments := node.ecmnts[i].len > 0
		if i < node.ecmnts.len && has_comments {
			expr_pos := expr.pos()
			for icmt, cmt in node.ecmnts[i] {
				if !set_comma && cmt.pos.pos > expr_pos.pos + expr_pos.len + 2 {
					if icmt > 0 {
						if last_comment_was_inline {
							f.write(',')
							set_comma = true
						}
					} else {
						f.write(',') // first comment needs a comma
						set_comma = true
					}
				}
				if cmt.pos.line_nr > expr_pos.last_line {
					f.writeln('')
					f.comment(cmt)
				} else {
					if !set_comma {
						f.write(',')
						set_comma = true
					}
					f.write(' ')
					f.comment(cmt)
					if !line_break {
						f.writeln('')
					}
				}
			}
		} else if i == node.exprs.len - 1 && !line_break {
			is_new_line = false
		}

		mut put_comma := !set_comma
		if has_comments && !last_comment_was_inline {
			put_comma = false
		}
		if i == node.exprs.len - 1 {
			if is_new_line {
				if put_comma {
					f.write(',')
				}
				f.writeln('')
			}
		} else if put_comma {
			f.write(',')
		}
		last_line_nr = pos.last_line
		set_comma = false
	}
	f.array_init_depth--
	if f.array_init_depth == 0 {
		f.array_init_break = []
	}
	if inc_indent {
		f.indent--
	}
	f.write(']')
	// `[100]u8`
	if node.is_fixed {
		if node.has_val {
			f.write('!')
			return
		}
		f.write(f.table.type_to_str_using_aliases(node.elem_type, f.mod2alias))
		if node.has_init {
			f.write('{init: ')
			f.expr(node.init_expr)
			f.write('}')
		} else {
			f.write('{}')
		}
	}
}

pub fn (mut f Fmt) as_cast(node ast.AsCast) {
	f.mark_types_import_as_used(node.typ)
	type_str := f.table.type_to_str_using_aliases(node.typ, f.mod2alias)
	f.expr(node.expr)
	f.write(' as ${type_str}')
}

pub fn (mut f Fmt) assoc(node ast.Assoc) {
	f.writeln('{')
	f.indent++
	f.writeln('...${node.var_name}')
	for i, field in node.fields {
		f.write('${field}: ')
		f.expr(node.exprs[i])
		f.writeln('')
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) at_expr(node ast.AtExpr) {
	f.write(node.name)
}

fn (mut f Fmt) write_static_method(name string, short_name string) {
	f.mark_import_as_used(name.split('__static__')[0])
	if short_name.contains('.') {
		indx := short_name.index('.') or { -1 } + 1
		f.write(short_name[0..indx] + short_name[indx..].replace('__static__', '.').capitalize())
	} else {
		f.write(short_name.replace('__static__', '.').capitalize())
	}
}

pub fn (mut f Fmt) call_expr(node ast.CallExpr) {
	mut is_method_newline := false
	if node.is_method {
		if node.name in ['map', 'filter', 'all', 'any', 'count'] {
			f.in_lambda_depth++
			defer { f.in_lambda_depth-- }
		}
		if node.left is ast.Ident {
			// `time.now()` without `time imported` is processed as a method call with `time` being
			// a `node.left` expression. Import `time` automatically.
			// TODO: fetch all available modules
			if node.left.name in ['time', 'os', 'strings', 'math', 'json', 'base64']
				&& !node.left.scope.known_var(node.left.name) {
				f.file.imports << ast.Import{
					source_name: node.left.name
					mod:         node.left.name
					alias:       node.left.name
				}
				f.used_imports[node.left.name] = true
			}
		}
		f.expr(node.left)
		is_method_newline = node.left.pos().last_line != node.name_pos.line_nr
		if is_method_newline {
			f.indent++
			f.writeln('')
		}
		f.write('.' + node.name)
	} else {
		f.write_language_prefix(node.language)
		if node.left is ast.AnonFn {
			f.anon_fn(node.left)
		} else if node.language != .v {
			f.write('${node.name.after_char(`.`)}')
		} else {
			name := f.short_module(node.name)
			if node.is_static_method {
				f.write_static_method(node.name, name)
			} else {
				f.mark_import_as_used(name)
				f.write(name)
			}
		}
	}
	if node.mod == '' && node.name == '' {
		if node.left is ast.CallExpr {
			f.expr(node.left)
		} else {
			f.write(node.left.str())
		}
	}
	f.write_generic_call_if_require(node)
	f.write('(')
	f.call_args(node.args)
	f.write(')')
	f.or_expr(node.or_block)
	f.comments(node.comments, has_nl: false)
	if is_method_newline {
		f.indent--
	}
}

fn (mut f Fmt) write_generic_call_if_require(node ast.CallExpr) {
	if node.concrete_types.len > 0 {
		f.write('[')
		for i, concrete_type in node.concrete_types {
			mut name := f.table.type_to_str_using_aliases(concrete_type, f.mod2alias)
			tsym := f.table.sym(concrete_type)
			if tsym.language != .js && !tsym.name.starts_with('JS.') {
				name = f.short_module(name)
			} else if tsym.language == .js && !tsym.name.starts_with('JS.') {
				name = 'JS.' + name
			}
			if tsym.language == .c {
				name = 'C.' + name
			}
			f.write(name)
			f.mark_types_import_as_used(concrete_type)
			if i != node.concrete_types.len - 1 {
				f.write(', ')
			}
		}
		f.write(']')
	}
}

pub fn (mut f Fmt) call_args(args []ast.CallArg) {
	old_single_line_fields_state := f.single_line_fields
	old_short_arg_state := f.use_short_fn_args
	f.single_line_fields = true
	f.use_short_fn_args = false
	defer {
		f.single_line_fields = old_single_line_fields_state
		f.use_short_fn_args = old_short_arg_state
	}
	for i, arg in args {
		pre_comments := arg.comments.filter(it.pos.pos < arg.expr.pos().pos)
		post_comments := arg.comments[pre_comments.len..]
		if pre_comments.len > 0 {
			f.comments(pre_comments)
			f.write(' ')
		}
		if i == args.len - 1 && arg.expr is ast.StructInit {
			if arg.expr.typ == ast.void_type {
				f.use_short_fn_args = true
			}
		}
		if arg.is_mut {
			f.write(arg.share.str() + ' ')
		}
		if i > 0 && !f.single_line_if && !f.use_short_fn_args {
			f.wrap_long_line(3, true)
		}
		f.expr(arg.expr)
		if post_comments.len > 0 {
			f.comments(post_comments)
			f.write(' ')
		}
		if i < args.len - 1 {
			f.write(', ')
		}
	}
}

pub fn (mut f Fmt) cast_expr(node ast.CastExpr) {
	typ := f.table.type_to_str_using_aliases(node.typ, f.mod2alias)
	if typ == 'voidptr' {
		// `voidptr(0)` => `nil`
		if node.expr is ast.IntegerLiteral {
			if node.expr.val == '0' {
				if f.inside_unsafe {
					f.write('nil')
				} else {
					f.write('unsafe { nil }')
				}
				return
			}
		}
	}
	f.write('${typ}(')
	f.mark_types_import_as_used(node.typ)
	f.expr(node.expr)
	if node.has_arg {
		f.write(', ')
		f.expr(node.arg)
	}
	f.write(')')
}

pub fn (mut f Fmt) chan_init(mut node ast.ChanInit) {
	info := f.table.sym(node.typ).chan_info()
	if node.elem_type == 0 && node.typ > 0 {
		node.elem_type = info.elem_type
	}
	is_mut := info.is_mut
	el_typ := if is_mut {
		node.elem_type.set_nr_muls(node.elem_type.nr_muls() - 1)
	} else {
		node.elem_type
	}
	f.write('chan ')
	if is_mut {
		f.write('mut ')
	}
	f.write(f.table.type_to_str_using_aliases(el_typ, f.mod2alias))
	f.write('{')
	if node.has_cap {
		f.write('cap: ')
		f.expr(node.cap_expr)
	}
	f.write('}')
}

pub fn (mut f Fmt) comptime_call(node ast.ComptimeCall) {
	if node.is_vweb {
		if node.method_name == 'html' {
			if node.args.len == 1 && node.args[0].expr is ast.StringLiteral {
				if node.is_veb {
					f.write('\$veb.html(')
				} else {
					f.write('\$vweb.html(')
				}
				f.expr(node.args[0].expr)
				f.write(')')
			} else {
				if node.is_veb {
					f.write('\$veb.html()')
				} else {
					f.write('\$vweb.html()')
				}
			}
		} else {
			f.write('\$tmpl(')
			f.expr(node.args[0].expr)
			f.write(')')
		}
	} else {
		match true {
			node.is_embed {
				f.write('\$embed_file(')
				f.expr(node.args[0].expr)
				if node.embed_file.compression_type != 'none' {
					f.write(', .${node.embed_file.compression_type}')
				}
				f.write(')')
			}
			node.is_env {
				f.write("\$env('${node.args_var}')")
			}
			node.is_pkgconfig {
				f.write("\$pkgconfig('${node.args_var}')")
			}
			node.method_name in ['compile_error', 'compile_warn'] {
				if node.args_var.contains("'") {
					f.write('\$${node.method_name}("${node.args_var}")')
				} else {
					f.write("\$${node.method_name}('${node.args_var}')")
				}
			}
			node.method_name == 'd' {
				f.write("\$d('${node.args_var}', ")
				f.expr(node.args[0].expr)
				f.write(')')
			}
			node.method_name == 'res' {
				if node.args_var != '' {
					f.write('\$res(${node.args_var})')
				} else {
					f.write('\$res()')
				}
			}
			else {
				inner_args := if node.args_var != '' {
					node.args_var
				} else {
					node.args.map(if it.expr is ast.ArrayDecompose {
						'...${it.expr.expr.str()}'
					} else {
						it.str()
					}).join(', ')
				}
				method_expr := if node.has_parens {
					'(${node.method_name}(${inner_args}))'
				} else {
					'${node.method_name}(${inner_args})'
				}
				f.expr(node.left)
				f.write('.$${method_expr}')
				f.or_expr(node.or_block)
			}
		}
	}
}

pub fn (mut f Fmt) comptime_selector(node ast.ComptimeSelector) {
	f.expr(node.left)
	f.write('.\$(${node.field_expr})')
}

pub fn (mut f Fmt) concat_expr(node ast.ConcatExpr) {
	for i, val in node.vals {
		if i != 0 {
			f.write(', ')
		}
		f.expr(val)
	}
}

pub fn (mut f Fmt) dump_expr(node ast.DumpExpr) {
	f.write('dump(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Fmt) enum_val(node ast.EnumVal) {
	name := f.short_module(node.enum_name)
	f.write(name + '.' + node.val)
	f.mark_import_as_used(name)
}

pub fn (mut f Fmt) ident(node ast.Ident) {
	if node.info is ast.IdentVar {
		if node.comptime && node.name in ast.valid_comptime_not_user_defined {
			f.write(node.name)
			return
		}
		if node.info.is_mut {
			f.write(node.info.share.str() + ' ')
		}
		var_info := node.var_info()
		if var_info.is_static {
			f.write('static ')
		}
		if var_info.is_volatile {
			f.write('volatile ')
		}
	}
	f.write_language_prefix(node.language)
	if node.kind == .blank_ident {
		f.write('_')
	} else {
		mut is_local := false
		if f.fn_scope != unsafe { nil } {
			if _ := f.fn_scope.find_var(node.name) {
				is_local = true
			}
		}
		if !is_local && !node.name.contains('.') && !f.inside_const {
			if _ := f.file.global_scope.find_const('${f.cur_mod}.${node.name}') {
				const_name := node.name.all_after_last('.')
				f.write(const_name)
				if node.or_expr.kind == .block {
					f.or_expr(node.or_expr)
				}
				return
			}
		}
		name := f.short_module(node.name)
		if node.name.contains('__static__') {
			f.write_static_method(node.name, name)
		} else {
			f.mark_import_as_used(name)
			f.write(name)
		}
		if node.concrete_types.len > 0 {
			f.write('[')
			for i, concrete_type in node.concrete_types {
				typ_name := f.table.type_to_str_using_aliases(concrete_type, f.mod2alias)
				f.write(typ_name)
				if i != node.concrete_types.len - 1 {
					f.write(', ')
				}
			}
			f.write(']')
		}
		if node.or_expr.kind == .propagate_option {
			f.write('?')
		} else if node.or_expr.kind == .block {
			f.or_expr(node.or_expr)
		}
		f.mark_import_as_used(name)
	}
}

pub fn (mut f Fmt) if_expr(node ast.IfExpr) {
	dollar := if node.is_comptime { '$' } else { '' }
	f.inside_comptime_if = node.is_comptime
	mut is_ternary := node.branches.len == 2 && node.has_else
		&& branch_is_single_line(node.branches[0]) && branch_is_single_line(node.branches[1])
		&& (node.is_expr || f.is_assign || f.inside_const || f.is_struct_init
		|| f.single_line_fields)
	f.single_line_if = is_ternary
	start_pos := f.out.len
	start_len := f.line_len
	for {
		for i, branch in node.branches {
			mut sum_len := 0
			if i > 0 {
				// `else`, close previous branch
				if branch.comments.len > 0 {
					f.writeln('}')
					pre_comments := branch.comments.filter(it.pos.pos < branch.pos.pos)
					sum_len += pre_comments.len
					if pre_comments.len > 0 {
						f.comments(pre_comments)
					}
				} else {
					f.write('} ')
				}
				f.write('${dollar}else ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				f.write('${dollar}if ')
				cur_pos := f.out.len
				pre_comments := branch.comments[sum_len..].filter(it.pos.pos < branch.cond.pos().pos)
				sum_len += pre_comments.len
				post_comments := branch.comments[sum_len..]
				if pre_comments.len > 0 {
					f.comments(pre_comments)
					f.write(' ')
				}
				f.expr(branch.cond)
				if post_comments.len > 0 {
					f.comments(post_comments)
					f.write(' ')
				}
				cond_len := f.out.len - cur_pos
				is_cond_wrapped := cond_len > 0 && branch.cond in [ast.IfGuardExpr, ast.CallExpr]
					&& f.out.last_n(cond_len).contains('\n')
				if is_cond_wrapped {
					f.writeln('')
				} else {
					f.write(' ')
				}
			}
			f.write('{')
			if is_ternary {
				f.write(' ')
			} else {
				f.writeln('')
			}
			f.stmts(branch.stmts)
			if is_ternary {
				f.write(' ')
			}
		}
		// When a single line if is really long, write it again as multiline,
		// except it is part of an InfixExpr.
		if is_ternary && f.line_len > max_len && !f.buffering {
			is_ternary = false
			f.single_line_if = false
			f.out.go_back_to(start_pos)
			f.line_len = start_len
			f.empty_line = start_len == 0
			continue
		}
		break
	}
	f.write('}')
	f.single_line_if = false
	f.inside_comptime_if = false
	if node.post_comments.len > 0 {
		f.writeln('')
		f.comments(node.post_comments,
			has_nl:    false
			prev_line: node.branches.last().body_pos.last_line
		)
	}
}

fn branch_is_single_line(b ast.IfBranch) bool {
	if b.stmts.len == 1 && b.comments.len == 0 && stmt_is_single_line(b.stmts[0])
		&& b.pos.line_nr == b.stmts[0].pos.line_nr {
		return true
	}
	return false
}

pub fn (mut f Fmt) if_guard_expr(node ast.IfGuardExpr) {
	for i, var in node.vars {
		if var.is_mut {
			f.write('mut ')
		}
		f.write(var.name)
		if i != node.vars.len - 1 {
			f.write(', ')
		}
	}
	f.write(' := ')
	f.expr(node.expr)
}

pub fn (mut f Fmt) index_expr(node ast.IndexExpr) {
	f.expr(node.left)
	if node.index is ast.RangeExpr {
		if node.index.is_gated {
			f.write('#')
		}
	}
	last_index_expr_state := f.is_index_expr
	f.is_index_expr = true
	f.write('[')
	f.expr(node.index)
	f.write(']')
	f.is_index_expr = last_index_expr_state
	if node.or_expr.kind != .absent {
		f.or_expr(node.or_expr)
	}
}

pub fn (mut f Fmt) infix_expr(node ast.InfixExpr) {
	buffering_save := f.buffering
	if !f.buffering && node.op in [.logical_or, .and, .plus] {
		f.buffering = true
	}
	is_assign_save := f.is_assign
	if node.op == .left_shift {
		f.is_assign = true // To write ternary if on a single line
	}
	start_pos := f.out.len
	start_len := f.line_len
	mut redundant_par := false
	if node.left is ast.ParExpr && node.op in [.and, .logical_or] {
		if node.left.expr is ast.InfixExpr {
			if node.left.expr.op !in [.and, .logical_or] {
				redundant_par = true
				f.expr(node.left.expr)
			}
		}
	}
	if !redundant_par {
		f.expr(node.left)
	}
	if node.before_op_comments.len > 0 {
		f.comments(node.before_op_comments)
	}
	is_one_val_array_init := node.op in [.key_in, .not_in] && node.right is ast.ArrayInit
		&& node.right.exprs.len == 1
	is_and := node.op == .amp && f.node_str(node.right).starts_with('&')
	if is_one_val_array_init && !f.inside_comptime_if {
		// `var in [val]` => `var == val`
		op := if node.op == .key_in { ' == ' } else { ' != ' }
		f.write(op)
	} else if is_and {
		f.write(' && ')
	} else {
		f.write(' ${node.op.str()} ')
	}
	if node.after_op_comments.len > 0 {
		f.comments(node.after_op_comments)
		f.write(' ')
	}
	if is_one_val_array_init && !f.inside_comptime_if {
		// `var in [val]` => `var == val`
		f.expr((node.right as ast.ArrayInit).exprs[0])
	} else if is_and {
		f.write(f.node_str(node.right).trim_string_left('&'))
	} else {
		redundant_par = false
		if node.right is ast.ParExpr && node.op in [.and, .logical_or] {
			if node.right.expr is ast.InfixExpr {
				if node.right.expr.op !in [.and, .logical_or] {
					redundant_par = true
					f.expr(node.right.expr)
				}
			}
		}
		if !redundant_par {
			f.expr(node.right)
		}
	}
	if !buffering_save && f.buffering {
		f.buffering = false
		if !f.single_line_if && f.line_len > max_len {
			is_cond := node.op in [.and, .logical_or]
			f.wrap_infix(start_pos, start_len, is_cond)
		}
	}
	f.is_assign = is_assign_save
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) wrap_infix(start_pos int, start_len int, is_cond bool) {
	cut_span := f.out.len - start_pos
	infix_str := f.out.cut_last(cut_span)
	if !infix_str.contains_any_substr(['&&', '||', '+']) {
		f.write(infix_str)
		return
	}
	f.line_len = start_len
	if start_len == 0 {
		f.empty_line = true
	}
	conditions, penalties := split_up_infix(infix_str, false, is_cond)
	f.write_splitted_infix(conditions, penalties, false, is_cond)
}

fn split_up_infix(infix_str string, ignore_paren bool, is_cond_infix bool) ([]string, []int) {
	mut conditions := ['']
	mut penalties := [5]
	or_pen := if infix_str.contains('&&') { 3 } else { 5 }
	parts := infix_str.split(' ')
	mut inside_paren := false
	mut ind := 0
	for p in parts {
		if is_cond_infix && p in ['&&', '||'] {
			if inside_paren {
				conditions[ind] += '${p} '
			} else {
				pen := if p == '||' { or_pen } else { 5 }
				penalties << pen
				conditions << '${p} '
				ind++
			}
		} else if !is_cond_infix && p == '+' {
			penalties << 5
			conditions[ind] += '${p} '
			conditions << ''
			ind++
		} else {
			conditions[ind] += '${p} '
			if ignore_paren {
				continue
			}
			if p.starts_with('(') {
				inside_paren = true
			} else if p.ends_with(')') {
				inside_paren = false
			}
		}
	}
	return conditions, penalties
}

const wsinfix_depth_max = 10

fn (mut f Fmt) write_splitted_infix(conditions []string, penalties []int, ignore_paren bool, is_cond bool) {
	f.wsinfix_depth++
	defer { f.wsinfix_depth-- }
	for i, cnd in conditions {
		c := cnd.trim_space()
		if f.line_len + c.len < break_points[penalties[i]] {
			if (i > 0 && i < conditions.len) || (ignore_paren && i == 0 && c.len > 5 && c[3] == `(`) {
				f.write(' ')
			}
			f.write(c)
		} else {
			is_paren_expr := (c[0] == `(` || (c.len > 5 && c[3] == `(`)) && c.ends_with(')')
			final_len := ((f.indent + 1) * 4) + c.len
			if f.wsinfix_depth > wsinfix_depth_max {
				// limit indefinite recursion, by just giving up splitting:
				f.write(c)
				continue
			}
			if final_len > max_len && is_paren_expr {
				conds, pens := split_up_infix(c, true, is_cond)
				f.write_splitted_infix(conds, pens, true, is_cond)
				continue
			}
			mut is_wrap_needed := true
			if i == 0 {
				last_line_str := f.remove_new_line().trim_space()
				if last_line_str in ['if', 'for'] {
					f.write(' ')
					is_wrap_needed = false
				}
			}
			if is_wrap_needed {
				f.writeln('')
			}
			f.indent++
			f.write(c)
			f.indent--
		}
	}
}

pub fn (mut f Fmt) likely(node ast.Likely) {
	if node.is_likely {
		f.write('_likely_')
	} else {
		f.write('_unlikely_')
	}
	f.write('(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Fmt) lock_expr(node ast.LockExpr) {
	mut num_locked := 0
	mut num_rlocked := 0
	for is_rlock in node.is_rlock {
		if is_rlock {
			num_rlocked++
		} else {
			num_locked++
		}
	}
	if num_locked > 0 || num_rlocked == 0 {
		f.write('lock')
		if num_locked > 0 {
			f.write(' ')
		}
		mut n := 0
		for i, v in node.lockeds {
			if !node.is_rlock[i] {
				if n > 0 {
					f.write(', ')
				}
				f.expr(v)
				n++
			}
		}
	}
	if num_rlocked > 0 {
		if num_locked > 0 {
			f.write('; ')
		}
		f.write('rlock ')
		mut n := 0
		for i, v in node.lockeds {
			if node.is_rlock[i] {
				if n > 0 {
					f.write(', ')
				}
				f.expr(v)
				n++
			}
		}
	}
	f.writeln(' {')
	f.stmts(node.stmts)
	f.write('}')
}

pub fn (mut f Fmt) map_init(node ast.MapInit) {
	if node.keys.len == 0 && !node.has_update_expr {
		if node.typ > ast.void_type {
			sym := f.table.sym(node.typ)
			info := sym.info as ast.Map
			f.mark_types_import_as_used(info.key_type)
			f.mark_types_import_as_used(info.value_type)
			f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		}
		if node.pos.line_nr == node.pos.last_line {
			f.write('{}')
		} else {
			f.writeln('{')
			f.comments(node.pre_cmnts, level: .indent)
			f.write('}')
		}
		return
	}
	f.writeln('{')
	f.indent++
	f.comments(node.pre_cmnts)
	if node.has_update_expr {
		f.write('...')
		f.expr(node.update_expr)
		f.comments(node.update_expr_comments,
			prev_line: node.update_expr_pos.last_line
			has_nl:    false
		)
		f.writeln('')
	}
	mut max_field_len := 0
	mut skeys := []string{}
	for key in node.keys {
		skey := f.node_str(key).trim_space()
		skeys << skey
		skey_len := utf8_str_visible_length(skey)
		if skey_len > max_field_len {
			max_field_len = skey_len
		}
	}
	for i, _ in node.keys {
		skey := skeys[i]
		f.write(skey)
		f.write(': ')
		skey_len := utf8_str_visible_length(skey)
		f.write(' '.repeat(max_field_len - skey_len))
		f.expr(node.vals[i])
		f.comments(node.comments[i], prev_line: node.vals[i].pos().last_line, has_nl: false)
		f.writeln('')
	}
	f.indent--
	f.write('}')
}

fn (mut f Fmt) match_branch(branch ast.MatchBranch, single_line bool) {
	if !branch.is_else {
		// normal branch
		f.is_mbranch_expr = true
		for j, expr in branch.exprs {
			estr := f.node_str(expr).trim_space()
			if f.line_len + estr.len + 2 > max_len {
				f.remove_new_line()
				f.writeln('')
			}
			f.write(estr)
			if j < branch.exprs.len - 1 {
				f.write(', ')
			}
			if j < branch.ecmnts.len && branch.ecmnts[j].len > 0 {
				f.write(' ')
				f.comments(branch.ecmnts[j])
			}
		}
		f.is_mbranch_expr = false
	} else {
		// else branch
		f.write('else')
	}
	if branch.stmts.len == 0 {
		f.writeln(' {}')
	} else {
		if single_line {
			f.write(' { ')
		} else if branch.ecmnts.len > 0 && branch.ecmnts.last().len > 0 {
			f.writeln('{')
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
	f.comments(branch.post_comments, same_line: true)
}

pub fn (mut f Fmt) match_expr(node ast.MatchExpr) {
	f.write('match ')
	f.expr(node.cond)
	f.writeln(' {')
	f.indent++
	f.comments(node.comments)
	mut single_line := true
	for branch in node.branches {
		if branch.stmts.len > 1 || branch.pos.line_nr < branch.pos.last_line {
			single_line = false
			break
		}
		if branch.stmts.len == 0 {
			continue
		}
		if !stmt_is_single_line(branch.stmts[0]) {
			single_line = false
			break
		}
	}
	mut else_idx := -1
	for i, branch in node.branches {
		if branch.is_else {
			else_idx = i
			continue
		}
		f.match_branch(branch, single_line)
	}
	if else_idx >= 0 {
		f.match_branch(node.branches[else_idx], single_line)
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) offset_of(node ast.OffsetOf) {
	f.write('__offsetof(${f.table.type_to_str_using_aliases(node.struct_type, f.mod2alias)}, ${node.field})')
	f.mark_types_import_as_used(node.struct_type)
}

pub fn (mut f Fmt) or_expr(node ast.OrExpr) {
	match node.kind {
		.absent {}
		.block {
			if node.stmts.len == 0 {
				f.write(' or {')
				if node.pos.line_nr != node.pos.last_line {
					f.writeln('')
				}
				f.write('}')
				return
			} else if node.stmts.len == 1 && stmt_is_single_line(node.stmts[0]) {
				// the control stmts (return/break/continue...) print a newline inside them,
				// so, since this'll all be on one line, trim any possible whitespace
				str := f.node_str(node.stmts[0]).trim_space()
				single_line := ' or { ${str} }'
				if single_line.len + f.line_len <= max_len {
					f.write(single_line)
					return
				}
			}
			// Make it multiline if the blocks has at least two stmts
			// or a single line would be too long
			f.writeln(' or {')
			f.stmts(node.stmts)
			f.write('}')
		}
		.propagate_option {
			f.write('?')
		}
		.propagate_result {
			f.write('!')
		}
	}
}

pub fn (mut f Fmt) par_expr(node ast.ParExpr) {
	mut expr := node.expr
	expr = expr.remove_par()
	requires_paren := expr !is ast.Ident || node.comments.len > 0
	if requires_paren {
		f.par_level++
		f.write('(')
	}
	pre_comments := node.comments.filter(it.pos.pos < expr.pos().pos)
	post_comments := node.comments[pre_comments.len..]
	if pre_comments.len > 0 {
		f.comments(pre_comments)
		f.write(' ')
	}
	f.expr(expr)
	if post_comments.len > 0 {
		f.comments(post_comments)
		f.write(' ')
	}
	if requires_paren {
		f.par_level--
		f.write(')')
	}
}

pub fn (mut f Fmt) postfix_expr(node ast.PostfixExpr) {
	f.expr(node.expr)
	// `$if foo ?`
	if node.op == .question {
		f.write(' ?')
	} else {
		f.write('${node.op}')
	}
	if node.is_c2v_prefix {
		f.write('$')
	}
}

pub fn (mut f Fmt) prefix_expr(node ast.PrefixExpr) {
	// !(a in b) => a !in b, !(a is b) => a !is b
	if node.op == .not && node.right is ast.ParExpr {
		if node.right.expr is ast.InfixExpr {
			if node.right.expr.op in [.key_in, .not_in, .key_is, .not_is]
				&& node.right.expr.right !is ast.InfixExpr {
				f.expr(node.right.expr.left)
				match node.right.expr.op {
					.key_in { f.write(' !in ') }
					.not_in { f.write(' in ') }
					.key_is { f.write(' !is ') }
					.not_is { f.write(' is ') }
					else {}
				}
				f.expr(node.right.expr.right)
				return
			}
		}
	}
	f.write(node.op.str())
	f.expr(node.right)
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) range_expr(node ast.RangeExpr) {
	f.expr(node.low)
	if f.is_mbranch_expr && !f.is_index_expr {
		f.write('...')
	} else {
		f.write('..')
	}
	f.expr(node.high)
}

pub fn (mut f Fmt) select_expr(node ast.SelectExpr) {
	f.writeln('select {')
	f.indent++
	for branch in node.branches {
		if branch.comment.text != '' {
			f.comment(branch.comment, same_line: true)
			f.writeln('')
		}
		if branch.is_else {
			f.write('else {')
		} else {
			f.single_line_if = true
			match branch.stmt {
				ast.ExprStmt { f.expr(branch.stmt.expr) }
				else { f.stmt(branch.stmt) }
			}
			f.single_line_if = false
			f.write(' {')
		}
		if branch.stmts.len > 0 {
			f.writeln('')
			f.stmts(branch.stmts)
		}
		f.writeln('}')
		if branch.post_comments.len > 0 {
			f.comments(branch.post_comments, same_line: true)
		}
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) selector_expr(node ast.SelectorExpr) {
	// TODO(StunxFS): Even though we ignored the JS backend, the `v/gen/js/tests/js.v`
	// file was still formatted/transformed, so it is specifically ignored here. Fix this.
	if f.file.language != .js && node.expr is ast.StringLiteral && node.field_name == 'str'
		&& !f.pref.backend.is_js()
		&& !f.file.path.ends_with(os.join_path('v', 'gen', 'js', 'tests', 'js.v')) {
		f.write('c')
		f.expr(node.expr)
		return
	}
	f.expr(node.expr)
	f.write('.')
	f.write(node.field_name)
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) size_of(node ast.SizeOf) {
	f.write('sizeof')
	if node.is_type && !node.guessed_type {
		// the new form was explicitly written in the source code; keep it:
		f.write('[')
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(']()')
		return
	}
	if node.is_type {
		f.write('(')
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(')')
	} else {
		f.write('(')
		f.expr(node.expr)
		f.write(')')
	}
}

pub fn (mut f Fmt) is_ref_type(node ast.IsRefType) {
	f.write('isreftype')
	if node.is_type && !node.guessed_type {
		// the new form was explicitly written in the source code; keep it:
		f.write('[')
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(']()')
		return
	}
	if node.is_type {
		f.write('(')
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(')')
	} else {
		f.write('(')
		f.expr(node.expr)
		f.write(')')
	}
}

pub fn (mut f Fmt) sql_expr(node ast.SqlExpr) {
	// sql app.db { select from Contributor where repo == id && user == 0 }
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')
	if node.is_insert {
		f.write('\tinsert ')
	} else {
		f.write('\tselect ')
	}
	sym := f.table.sym(node.table_expr.typ)
	mut table_name := sym.name
	if !table_name.starts_with('C.') && !table_name.starts_with('JS.') {
		table_name = f.no_cur_mod(f.short_module(sym.name)) // TODO: f.type_to_str?
	}
	if node.is_count {
		f.write('count ')
	} else {
		for i, fd in node.fields {
			f.write(fd.name)
			if i < node.fields.len - 1 {
				f.write(', ')
			}
		}
	}
	if node.is_insert {
		f.write('${node.inserted_var} into ${table_name}')
	} else {
		f.write('from ${table_name}')
	}
	if node.has_where {
		f.write(' where ')
		f.expr(node.where_expr)
	}
	if node.has_order {
		f.write(' order by ')
		f.expr(node.order_expr)
		if node.has_desc {
			f.write(' desc')
		}
	}
	if node.has_limit {
		f.write(' limit ')
		f.expr(node.limit_expr)
	}
	if node.has_offset {
		f.write(' offset ')
		f.expr(node.offset_expr)
	}
	f.writeln('')
	f.write('}')
	f.or_expr(node.or_expr)
}

pub fn (mut f Fmt) char_literal(node ast.CharLiteral) {
	if node.val == r"\'" {
		f.write("`'`")
		return
	}
	if node.val.len == 1 {
		clit := node.val[0]
		if clit < 32 || clit > 127 || clit == 92 || clit == 96 {
			f.write('`\\x${clit.hex()}`')
			return
		}
	}
	f.write('`${node.val}`')
}

pub fn (mut f Fmt) string_literal(node ast.StringLiteral) {
	quote := if node.val.contains("'") && !node.val.contains('"') { '"' } else { "'" }
	if node.is_raw {
		f.write('r')
	} else if node.language == ast.Language.c {
		f.write('c')
	} else if node.language == ast.Language.js {
		f.write('js')
	}
	if node.is_raw {
		f.write('${quote}${node.val}${quote}')
	} else {
		unescaped_val := node.val.replace('${bs}${bs}', '\x01').replace_each([
			"${bs}'",
			"'",
			'${bs}"',
			'"',
		])
		s := unescaped_val.replace_each(['\x01', '${bs}${bs}', quote, '${bs}${quote}'])
		f.write('${quote}${s}${quote}')
	}
}

pub fn (mut f Fmt) string_inter_literal(node ast.StringInterLiteral) {
	mut quote := "'"
	for val in node.vals {
		if val.contains('\\"') {
			quote = '"'
			break
		}
		if val.contains("\\'") {
			quote = "'"
			break
		}
		if val.contains('"') {
			quote = "'"
		}
		if val.contains("'") {
			quote = '"'
		}
	}
	// TODO: this code is very similar to ast.Expr.str()
	// serkonda7: it can not fully be replaced tho as ´f.expr()´ and `ast.Expr.str()`
	//	work too different for the various exprs that are interpolated
	f.write(quote)
	for i, val in node.vals {
		unescaped_val := val.replace('${bs}${bs}', '\x01').replace_each([
			"${bs}'",
			"'",
			'${bs}"',
			'"',
		])
		s := unescaped_val.replace_each(['\x01', '${bs}${bs}', quote, '${bs}${quote}'])
		f.write('${s}')
		if i >= node.exprs.len {
			break
		}
		f.write('$')
		fspec_str, needs_braces := node.get_fspec_braces(i)
		if needs_braces {
			f.write('{')
			f.expr(node.exprs[i])
			f.write(fspec_str)
			f.write('}')
		} else {
			f.write('{')
			f.expr(node.exprs[i])
			f.write('}')
		}
	}
	f.write(quote)
}

pub fn (mut f Fmt) type_expr(node ast.TypeNode) {
	if node.stmt == ast.empty_stmt {
		f.mark_types_import_as_used(node.typ)
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	} else {
		f.struct_decl(ast.StructDecl{ fields: (node.stmt as ast.StructDecl).fields },
			true)
	}
}

pub fn (mut f Fmt) type_of(node ast.TypeOf) {
	f.write('typeof')
	if node.is_type {
		f.write('[')
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(']()')
	} else {
		f.write('(')
		f.expr(node.expr)
		f.write(')')
	}
}

pub fn (mut f Fmt) unsafe_expr(node ast.UnsafeExpr) {
	single_line := node.pos.line_nr >= node.pos.last_line
	f.write('unsafe {')
	if single_line {
		f.write(' ')
	} else {
		f.writeln('')
		f.indent++
		f.empty_line = true
	}
	f.expr(node.expr)
	if single_line {
		f.write(' ')
	} else {
		f.writeln('')
		f.indent--
	}
	f.write('}')
}

fn (mut f Fmt) trace[T](fbase string, x &T) {
	if f.file.path_base == fbase {
		println('> f.trace | ${fbase:-10s} | ${voidptr(x):16} | ${x}')
	}
}
