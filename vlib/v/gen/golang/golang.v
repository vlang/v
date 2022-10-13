// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module golang

import strings
import v.ast
import v.util
import v.pref
import os

const (
	bs      = '\\'
	// when to break a line dependant on penalty
	max_len = [0, 35, 60, 85, 93, 100]
)

pub struct Gen {
pub mut:
	table &ast.Table        = unsafe { nil }
	pref  &pref.Preferences = unsafe { nil }
	// is_debug           bool
	out                strings.Builder
	out_imports        strings.Builder
	indent             int
	empty_line         bool
	line_len           int  // the current line length, Note: it counts \t as 4 spaces, and starts at 0 after f.writeln
	buffering          bool // disables line wrapping for exprs that will be analyzed later
	par_level          int  // how many parentheses are put around the current expression
	single_line_if     bool
	cur_mod            string
	did_imports        bool
	is_assign          bool
	is_struct_init     bool
	auto_imports       []string          // automatically inserted imports that the user forgot to specify
	import_pos         int               // position of the imports in the resulting string for later autoimports insertion
	used_imports       []string          // to remove unused imports
	import_syms_used   map[string]bool   // to remove unused import symbols.
	mod2alias          map[string]string // for `import time as t`, will contain: 'time'=>'t'
	mod2syms           map[string]string // import time { now } 'time.now'=>'now'
	use_short_fn_args  bool
	single_line_fields bool   // should struct fields be on a single line
	it_name            string // the name to replace `it` with
	in_lambda_depth    int
	inside_const       bool
	is_mbranch_expr    bool // match a { x...y { } }
	fn_scope           &ast.Scope = unsafe { nil }
	wsinfix_depth      int
	nlines             int
}

pub fn gen(files []&ast.File, table &ast.Table, out_file string, pref &pref.Preferences) (int, int) {
	mut g := Gen{
		table: table
		pref: pref
		// is_debug: is_debug
		out: strings.new_builder(1000)
		out_imports: strings.new_builder(200)
	}
	for file in files {
		if file.errors.len > 0 {
			g.error(file.errors[0].str())
		}
		g.stmts(file.stmts)
	}
	os.write_file(out_file, g.out.str()) or { panic(err) }
	os.system('go fmt "$out_file"')
	return g.nlines, 0 // g.buf.len
}

pub fn (mut f Gen) process_file_imports(file &ast.File) {
	for imp in file.imports {
		f.mod2alias[imp.mod] = imp.alias
		for sym in imp.syms {
			f.mod2alias['${imp.mod}.$sym.name'] = sym.name
			f.mod2alias['${imp.mod.all_after_last('.')}.$sym.name'] = sym.name
			f.mod2alias[sym.name] = sym.name
			f.mod2syms['${imp.mod}.$sym.name'] = sym.name
			f.mod2syms['${imp.mod.all_after_last('.')}.$sym.name'] = sym.name
			f.mod2syms[sym.name] = sym.name
			f.import_syms_used[sym.name] = false
		}
	}
	f.auto_imports = file.auto_imports
}

//=== Basic buffer write operations ===//

pub fn (mut f Gen) write(s string) {
	if f.indent > 0 && f.empty_line {
		f.write_indent()
	}
	f.out.write_string(s)
	f.line_len += s.len
	f.empty_line = false
}

pub fn (mut f Gen) writeln(s string) {
	if f.indent > 0 && f.empty_line && s.len > 0 {
		f.write_indent()
	}
	f.out.writeln(s)
	f.empty_line = true
	f.line_len = 0
}

fn (mut f Gen) write_indent() {
	f.out.write_string(util.tabs(f.indent))
	f.line_len += f.indent * 4
}

pub fn (mut f Gen) wrap_long_line(penalty_idx int, add_indent bool) bool {
	if f.buffering {
		return false
	}
	if f.out[f.out.len - 1] == ` ` {
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

[params]
pub struct RemoveNewLineConfig {
	imports_buffer bool // Work on f.out_imports instead of f.out
}

pub fn (mut f Gen) remove_new_line(cfg RemoveNewLineConfig) {
	mut buffer := if cfg.imports_buffer { unsafe { &f.out_imports } } else { unsafe { &f.out } }
	mut i := 0
	for i = buffer.len - 1; i >= 0; i-- {
		if !buffer.byte_at(i).is_space() { // != `\n` {
			break
		}
	}
	buffer.go_back(buffer.len - i - 1)
	f.empty_line = false
}

//=== Specialized write methods ===//

fn (mut f Gen) write_language_prefix(lang ast.Language) {
	match lang {
		.c { f.write('C.') }
		.js { f.write('JS.') }
		else {}
	}
}

fn (mut f Gen) write_generic_types(gtypes []ast.Type) {
	if gtypes.len > 0 {
		f.write('<')
		gtypes_string := gtypes.map(f.table.type_to_str(it)).join(', ')
		f.write(gtypes_string)
		f.write('>')
	}
}

//=== Module handling helper methods ===//

pub fn (mut f Gen) set_current_module_name(cmodname string) {
	f.cur_mod = cmodname
	f.table.cmod_prefix = cmodname + '.'
}

fn (f Gen) get_modname_prefix(mname string) (string, string) {
	// ./tests/proto_module_importing_vproto_keep.vv to know, why here is checked for ']' and '&'
	if !mname.contains(']') && !mname.contains('&') {
		return mname, ''
	}
	after_rbc := mname.all_after_last(']')
	after_ref := mname.all_after_last('&')
	modname := if after_rbc.len < after_ref.len { after_rbc } else { after_ref }
	return modname, mname.trim_string_right(modname)
}

fn (mut f Gen) is_external_name(name string) bool {
	if name.len > 2 && name[0] == `C` && name[1] == `.` {
		return true
	}
	if name.len > 3 && name[0] == `J` && name[1] == `S` && name[2] == `.` {
		return true
	}
	return false
}

pub fn (mut f Gen) no_cur_mod(typename string) string {
	return util.no_cur_mod(typename, f.cur_mod)
}

// foo.bar.fn() => bar.fn()
pub fn (mut f Gen) short_module(name string) string {
	if !name.contains('.') || name.starts_with('JS.') {
		return name
	}
	if name in f.mod2syms {
		return f.mod2syms[name]
	}
	if name.ends_with('>') {
		generic_levels := name.trim_string_right('>').split('<')
		mut res := '${f.short_module(generic_levels[0])}'
		for i in 1 .. generic_levels.len {
			genshorts := generic_levels[i].split(', ').map(f.short_module(it)).join(', ')
			res += '<$genshorts'
		}
		res += '>'
		return res
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	idx := vals.len - 1
	mname, tprefix := f.get_modname_prefix(vals[..idx].join('.'))
	symname := vals[vals.len - 1]
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
		return '$tprefix$symname'
	}
	return '$tprefix${aname}.$symname'
}

//=== Import-related methods ===//

pub fn (mut f Gen) mark_types_import_as_used(typ ast.Type) {
	sym := f.table.sym(typ)
	if sym.info is ast.Map {
		map_info := sym.map_info()
		f.mark_types_import_as_used(map_info.key_type)
		f.mark_types_import_as_used(map_info.value_type)
		return
	}
	if sym.info is ast.GenericInst {
		for concrete_typ in sym.info.concrete_types {
			f.mark_types_import_as_used(concrete_typ)
		}
	}
	name := sym.name.split('<')[0] // take `Type` from `Type<T>`
	f.mark_import_as_used(name)
}

// `name` is a function (`foo.bar()`) or type (`foo.Bar{}`)
pub fn (mut f Gen) mark_import_as_used(name string) {
	parts := name.split('.')
	last := parts.last()
	if last in f.import_syms_used {
		f.import_syms_used[last] = true
	}
	if parts.len == 1 {
		return
	}
	mod := parts[0..parts.len - 1].join('.')
	if mod in f.used_imports {
		return
	}
	f.used_imports << mod
}

pub fn (mut f Gen) imports(imports []ast.Import) {
	if f.did_imports || imports.len == 0 {
		return
	}
	f.did_imports = true
	mut num_imports := 0
	mut already_imported := map[string]bool{}

	for imp in imports {
		if imp.mod !in f.used_imports {
			// TODO bring back once only unused imports are removed
			// continue
		}
		if imp.mod in f.auto_imports && imp.mod !in f.used_imports {
			continue
		}
		import_text := 'import ${f.imp_stmt_str(imp)}'
		if already_imported[import_text] {
			continue
		}
		already_imported[import_text] = true
		f.out_imports.writeln(import_text)
		num_imports++
	}
	if num_imports > 0 {
		f.out_imports.writeln('')
	}
}

pub fn (f Gen) imp_stmt_str(imp ast.Import) string {
	mod := if imp.mod.len == 0 { imp.alias } else { imp.mod }
	is_diff := imp.alias != mod && !mod.ends_with('.' + imp.alias)
	mut imp_alias_suffix := if is_diff { ' as $imp.alias' } else { '' }
	mut syms := imp.syms.map(it.name).filter(f.import_syms_used[it])
	syms.sort()
	if syms.len > 0 {
		imp_alias_suffix += if imp.syms[0].pos.line_nr == imp.pos.line_nr {
			' { ' + syms.join(', ') + ' }'
		} else {
			' {\n\t' + syms.join(',\n\t') + ',\n}'
		}
	}
	return '$mod$imp_alias_suffix'
}

//=== Node helpers ===//

fn (f Gen) should_insert_newline_before_node(node ast.Node, prev_node ast.Node) bool {
	// No need to insert a newline if there is already one
	if f.out.last_n(2) == '\n\n' {
		return false
	}
	prev_line_nr := prev_node.pos().last_line
	// The nodes are Stmts
	if node is ast.Stmt && prev_node is ast.Stmt {
		stmt := node
		prev_stmt := prev_node
		// Force a newline after a block of HashStmts
		if prev_stmt is ast.HashStmt && stmt !is ast.HashStmt && stmt !is ast.ExprStmt {
			return true
		}
		// Force a newline after function declarations
		// The only exception is inside an block of no_body functions
		if prev_stmt is ast.FnDecl {
			if stmt !is ast.FnDecl || !prev_stmt.no_body {
				return true
			}
		}
		// Force a newline after struct declarations
		if prev_stmt is ast.StructDecl {
			return true
		}
		// Empty line after an block of type declarations
		if prev_stmt is ast.TypeDecl && stmt !is ast.TypeDecl {
			return true
		}
		// Imports are handled special hence they are ignored here
		if stmt is ast.Import || prev_stmt is ast.Import {
			return false
		}
		// Attributes are not respected in the stmts position, so this requires manual checking
		if stmt is ast.StructDecl {
			if stmt.attrs.len > 0 && stmt.attrs[0].pos.line_nr - prev_line_nr <= 1 {
				return false
			}
		}
		if stmt is ast.FnDecl {
			if stmt.attrs.len > 0 && stmt.attrs[0].pos.line_nr - prev_line_nr <= 1 {
				return false
			}
		}
	}
	// The node shouldn't have a newline before
	if node.pos().line_nr - prev_line_nr <= 1 {
		return false
	}
	return true
}

pub fn (mut f Gen) node_str(node ast.Node) string {
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

pub fn (mut f Gen) stmts(stmts []ast.Stmt) {
	mut prev_stmt := if stmts.len > 0 { stmts[0] } else { ast.empty_stmt }
	f.indent++
	for stmt in stmts {
		if !f.pref.building_v && f.should_insert_newline_before_node(stmt, prev_stmt) {
			f.out.writeln('')
		}
		f.stmt(stmt)
		prev_stmt = stmt
	}
	f.indent--
}

pub fn (mut f Gen) stmt(node ast.Stmt) {
	// if f.is_debug {
	// eprintln('stmt: ${node.pos:-42} | node: ${node.type_name():-20}')
	//}
	match node {
		ast.EmptyStmt, ast.NodeError {}
		ast.AsmStmt {
			// f.asm_stmt(node)
		}
		ast.AssertStmt {
			f.assert_stmt(node)
		}
		ast.AssignStmt {
			f.assign_stmt(node)
		}
		ast.Block {
			f.block(node)
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
		ast.SqlStmt {
			f.sql_stmt(node)
		}
		ast.StructDecl {
			f.struct_decl(node)
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
		else { false }
	}
}

//=== General Expr-related methods and helpers ===//

pub fn (mut f Gen) expr(node_ ast.Expr) {
	mut node := unsafe { node_ }
	// if f.is_debug {
	// eprintln('expr: ${node.pos():-42} | node: ${node.type_name():-20} | $node.str()')
	//}
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
		ast.Comment {}
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
			eprintln('ast.CTempVar of $node.orig.str() should be generated/used only in cgen')
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
		ast.Nil {}
		ast.None {
			f.write('none')
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
			f.unsafe_expr(node)
		}
		ast.ComptimeType {
			match node.kind {
				.array { f.write('\$Array') }
				.struct_ { f.write('\$Struct') }
				.iface { f.write('\$Interface') }
				.map_ { f.write('\$Map') }
				.int { f.write('\$Int') }
				.float { f.write('\$Float') }
				.sum_type { f.write('\$Sumtype') }
				.enum_ { f.write('\$Enum') }
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
			if !expr.no_keys && (expr.fields.len > 0 || expr.pre_comments.len > 0) {
				return false
			}
		}
		ast.CallExpr {
			if expr.or_block.stmts.len > 1 {
				return false
			}
		}
		ast.ArrayInit {
			if expr.exprs.len > 0 {
				return expr_is_single_line(expr.exprs[0])
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

pub fn (mut f Gen) assert_stmt(node ast.AssertStmt) {
	f.write('assert ')
	mut expr := node.expr
	for expr is ast.ParExpr {
		expr = (expr as ast.ParExpr).expr
	}
	f.expr(expr)
	f.writeln('')
}

pub fn (mut f Gen) assign_stmt(node ast.AssignStmt) {
	for i, left in node.left {
		f.expr(left)
		if i < node.left.len - 1 {
			f.write(', ')
		}
	}
	f.is_assign = true
	f.write(' $node.op.str() ')
	for i, val in node.right {
		f.expr(val)
		if i < node.right.len - 1 {
			f.write(', ')
		}
	}
	if !f.single_line_if {
		f.writeln('')
	}
	f.is_assign = false
}

pub fn (mut f Gen) block(node ast.Block) {
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

pub fn (mut f Gen) branch_stmt(node ast.BranchStmt) {
	f.writeln(node.str())
}

pub fn (mut f Gen) comptime_for(node ast.ComptimeFor) {
	typ := f.no_cur_mod(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	f.write('\$for $node.val_var in ${typ}.$node.kind.str() {')
	f.mark_types_import_as_used(node.typ)
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

struct ConstAlignInfo {
mut:
	max      int
	last_idx int
}

pub fn (mut f Gen) const_decl(node ast.ConstDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln('const ()\n')
		return
	}
	f.inside_const = true
	defer {
		f.inside_const = false
	}
	f.write('const ')
	mut align_infos := []ConstAlignInfo{}
	if node.is_block {
		f.writeln('(')
		mut info := ConstAlignInfo{}
		for i, field in node.fields {
			if field.name.len > info.max {
				info.max = field.name.len
			}
			if !expr_is_single_line(field.expr) {
				info.last_idx = i
				align_infos << info
				info = ConstAlignInfo{}
			}
		}
		info.last_idx = node.fields.len
		align_infos << info
		f.indent++
	} else {
		align_infos << ConstAlignInfo{0, 1}
	}
	mut prev_field := if node.fields.len > 0 {
		ast.Node(node.fields[0])
	} else {
		ast.Node(ast.NodeError{})
	}
	mut align_idx := 0
	for i, field in node.fields {
		if i > align_infos[align_idx].last_idx {
			align_idx++
		}
		if node.is_block && f.should_insert_newline_before_node(field, prev_field) {
			f.writeln('')
		}
		name := field.name.after('.')
		f.write('$name ')
		f.write(strings.repeat(` `, align_infos[align_idx].max - field.name.len))
		f.write('= ')
		f.expr(field.expr)
		if node.is_block && field.end_comments.len == 0 {
			f.writeln('')
		} else {
			// Write out single line comments after const expr if present
			// E.g.: `const x = 1 // <comment>`
			if node.end_comments.len > 0 && node.end_comments[0].text.contains('\n') {
				f.writeln('\n')
			}
		}
		prev_field = field
	}

	if node.is_block {
	} else if node.end_comments.len == 0 {
		// If no single line comments after the const expr is present
		f.writeln('')
	}
	if node.is_block {
		f.indent--
		f.writeln(')\n')
	} else {
		f.writeln('')
	}
}

pub fn (mut f Gen) defer_stmt(node ast.DeferStmt) {
	f.write('defer {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Gen) expr_stmt(node ast.ExprStmt) {
	f.expr(node.expr)
	if !f.single_line_if {
		f.writeln('')
	}
}

pub fn (mut f Gen) enum_decl(node ast.EnumDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	name := node.name.after('.')
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln('enum $name {}\n')
		return
	}
	f.writeln('enum $name {')
	for field in node.fields {
		f.write('\t$field.name')
		if field.has_expr {
			f.write(' = ')
			f.expr(field.expr)
		}
		f.writeln('')
	}
	f.writeln('}\n')
}

pub fn (mut f Gen) fn_decl(node ast.FnDecl) {
	f.attrs(node.attrs)
	f.write(node.stringify(f.table, f.cur_mod, f.mod2alias).replace('fn ', 'func '))
	f.fn_body(node)
}

pub fn (mut f Gen) anon_fn(node ast.AnonFn) {
	f.write(node.stringify(f.table, f.cur_mod, f.mod2alias)) // `Expr` instead of `ast.Expr` in mod ast
	f.fn_body(node.decl)
}

fn (mut f Gen) fn_body(node ast.FnDecl) {
	prev_fn_scope := f.fn_scope
	f.fn_scope = node.scope
	defer {
		f.fn_scope = prev_fn_scope
	}
	if node.language == .v {
		if !node.no_body {
			f.write(' {')
			if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
				if node.comments.len == 0 {
					f.writeln('')
				}
				f.stmts(node.stmts)
			}
			f.write('}')
		}
		if !node.is_anon {
			f.writeln('')
		}
	} else {
		f.writeln('')
	}
}

pub fn (mut f Gen) for_c_stmt(node ast.ForCStmt) {
	if node.label.len > 0 {
		f.write('$node.label: ')
	}
	f.write('for ')
	if node.has_init {
		f.single_line_if = true // to keep all for ;; exprs on the same line
		f.stmt(node.init)
		f.single_line_if = false
	}
	f.write('; ')
	f.expr(node.cond)
	f.write('; ')
	f.stmt(node.inc)
	f.remove_new_line()
	f.write(' {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Gen) for_in_stmt(node ast.ForInStmt) {
	if node.label.len > 0 {
		f.write('$node.label: ')
	}
	f.write('for ')
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
	f.write(' := range ')
	f.expr(node.cond)
	if node.is_range {
		f.write(' .. ')
		f.expr(node.high)
	}
	f.write(' {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Gen) for_stmt(node ast.ForStmt) {
	if node.label.len > 0 {
		f.write('$node.label: ')
	}
	f.write('for ')
	f.expr(node.cond)
	if !node.is_inf {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Gen) global_decl(node ast.GlobalDecl) {
	f.attrs(node.attrs)
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln('__global ()')
		return
	}
	f.write('__global ')
	mut max := 0
	// mut has_assign := false
	if node.is_block {
		f.writeln('(')
		f.indent++
		for field in node.fields {
			if field.name.len > max {
				max = field.name.len
			}
			// if field.has_expr {
			// has_assign = true
			//}
		}
	}
	for field in node.fields {
		f.write('$field.name ')
		f.write(strings.repeat(` `, max - field.name.len))
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
	if node.is_block {
		f.indent--
		f.writeln(')')
	} else {
		f.writeln('')
	}
}

pub fn (mut f Gen) go_expr(node ast.GoExpr) {
	f.write('go ')
	f.call_expr(node.call_expr)
}

pub fn (mut f Gen) goto_label(node ast.GotoLabel) {
	f.writeln('$node.name:')
}

pub fn (mut f Gen) goto_stmt(node ast.GotoStmt) {
	f.writeln('goto $node.name')
}

pub fn (mut f Gen) hash_stmt(node ast.HashStmt) {
	f.writeln(node.val)
}

pub fn (mut f Gen) interface_decl(node ast.InterfaceDecl) {
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
	for embed in node.embeds {
		f.write('\t$embed.name')
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

	// TODO: alignment, comments, etc.
	for field in immut_fields {
		f.interface_field(field)
	}
	for method in immut_methods {
		f.interface_method(method)
	}
	if mut_fields.len + mut_methods.len > 0 {
		f.writeln('mut:')
		for field in mut_fields {
			f.interface_field(field)
		}
		for method in mut_methods {
			f.interface_method(method)
		}
	}
	f.writeln('}\n')
}

pub fn (mut f Gen) interface_field(field ast.StructField) {
	mut ft := f.no_cur_mod(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
	// end_pos := field.pos.pos + field.pos.len
	f.write('\t$field.name $ft')
	f.writeln('')
	f.mark_types_import_as_used(field.typ)
}

pub fn (mut f Gen) interface_method(method ast.FnDecl) {
	f.write('\t')
	f.write(method.stringify(f.table, f.cur_mod, f.mod2alias).after('fn '))
	f.writeln('')
	for param in method.params {
		f.mark_types_import_as_used(param.typ)
	}
	f.mark_types_import_as_used(method.return_type)
}

pub fn (mut f Gen) module_stmt(mod ast.Module) {
	f.set_current_module_name(mod.name)
	if mod.is_skipped {
		return
	}
	f.attrs(mod.attrs)
	f.writeln('package $mod.short_name\n')
	if f.import_pos == 0 {
		f.import_pos = f.out.len
	}
}

pub fn (mut f Gen) return_stmt(node ast.Return) {
	f.write('return')
	if node.exprs.len > 0 {
		f.write(' ')
		// Loop over all return values. In normal returns this will only run once.
		for i, expr in node.exprs {
			if expr is ast.ParExpr {
				f.expr(expr.expr)
			} else {
				f.expr(expr)
			}
			if i < node.exprs.len - 1 {
				f.write(', ')
			}
		}
	}
	f.writeln('')
}

pub fn (mut f Gen) sql_stmt(node ast.SqlStmt) {
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')

	for line in node.lines {
		f.sql_stmt_line(line)
	}

	f.writeln('}')
}

pub fn (mut f Gen) sql_stmt_line(node ast.SqlStmtLine) {
	table_name := util.strip_mod_name(f.table.sym(node.table_expr.typ).name)
	f.mark_types_import_as_used(node.table_expr.typ)
	f.write('\t')
	match node.kind {
		.insert {
			f.writeln('insert $node.object_var_name into $table_name')
		}
		.update {
			f.write('update $table_name set ')
			for i, col in node.updated_columns {
				f.write('$col = ')
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
			f.write('delete from $table_name where ')
			f.expr(node.where_expr)
			f.writeln('')
		}
		.create {
			f.writeln('create table $table_name')
		}
		.drop {
			f.writeln('drop table $table_name')
		}
	}
}

pub fn (mut f Gen) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl { f.alias_type_decl(node) }
		ast.FnTypeDecl { f.fn_type_decl(node) }
		ast.SumTypeDecl { f.sum_type_decl(node) }
	}
	f.writeln('')
}

pub fn (mut f Gen) alias_type_decl(node ast.AliasTypeDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	ptype := f.table.type_to_str_using_aliases(node.parent_type, f.mod2alias)
	f.write('type $node.name = $ptype')

	f.mark_types_import_as_used(node.parent_type)
}

pub fn (mut f Gen) fn_type_decl(node ast.FnTypeDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	typ_sym := f.table.sym(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	fn_name := f.no_cur_mod(node.name)
	f.write('type $fn_name = fn (')
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
		f.write(' $ret_str')
	} else if fn_info.return_type.has_flag(.optional) {
		f.write(' ?')
	} else if fn_info.return_type.has_flag(.result) {
		f.write(' !')
	}

	f.writeln('')
}

pub fn (mut f Gen) sum_type_decl(node ast.SumTypeDecl) {
	f.attrs(node.attrs)
	// start_pos := f.out.len
	if node.is_pub {
		f.write('pub ')
	}
	f.write('type $node.name')
	f.write_generic_types(node.generic_types)
	f.write(' = ')

	mut sum_type_names := []string{cap: node.variants.len}
	for variant in node.variants {
		sum_type_names << f.table.type_to_str_using_aliases(variant.typ, f.mod2alias)
		f.mark_types_import_as_used(variant.typ)
	}
	sum_type_names.sort()

	separator := ' | '
	for i, name in sum_type_names {
		if i > 0 {
			f.write(separator)
		}
		f.write(name)
	}
}

//=== Specific Expr methods ===//

pub fn (mut f Gen) array_decompose(node ast.ArrayDecompose) {
	f.write('...')
	f.expr(node.expr)
}

pub fn (mut f Gen) array_init(node ast.ArrayInit) {
	if node.exprs.len == 0 && node.typ != 0 && node.typ != ast.void_type {
		// `x := []string{}`
		f.mark_types_import_as_used(node.typ)
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write('{')
		if node.has_len {
			f.write('len: ')
			f.expr(node.len_expr)
			if node.has_cap || node.has_default {
				f.write(', ')
			}
		}
		if node.has_cap {
			f.write('cap: ')
			f.expr(node.cap_expr)
			if node.has_default {
				f.write(', ')
			}
		}
		if node.has_default {
			f.write('init: ')
			f.expr(node.default_expr)
		}
		f.write('}')
		return
	}
	// `[1,2,3]`
	sym := f.table.sym(node.typ)
	f.write('$sym.name{')
	// mut inc_indent := false
	mut last_line_nr := node.pos.line_nr // to have the same newlines between array elements
	if node.pre_cmnts.len > 0 {
		if node.pre_cmnts[0].pos.line_nr > last_line_nr {
			f.writeln('')
		}
	}
	// mut set_comma := false
	for expr in node.exprs {
		// pos := expr.pos()
		f.expr(expr)
		f.write(',')
	}
	f.write('}')
	// `[100]u8`
	if node.is_fixed {
		if node.has_val {
			f.write('!')
			return
		}
		f.write(f.table.type_to_str_using_aliases(node.elem_type, f.mod2alias))
		if node.has_default {
			f.write('{init: ')
			f.expr(node.default_expr)
			f.write('}')
		} else {
			f.write('{}')
		}
	}
}

pub fn (mut f Gen) as_cast(node ast.AsCast) {
	f.mark_types_import_as_used(node.typ)
	type_str := f.table.type_to_str_using_aliases(node.typ, f.mod2alias)
	f.expr(node.expr)
	f.write(' as $type_str')
}

pub fn (mut f Gen) assoc(node ast.Assoc) {
	f.writeln('{')
	f.indent++
	f.writeln('...$node.var_name')
	for i, field in node.fields {
		f.write('$field: ')
		f.expr(node.exprs[i])
		f.writeln('')
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Gen) at_expr(node ast.AtExpr) {
	f.write(node.name)
}

pub fn (mut f Gen) call_expr(node ast.CallExpr) {
	// for arg in node.args {}
	mut is_method_newline := false
	if node.is_method {
		if node.name in ['map', 'filter', 'all', 'any'] {
			f.in_lambda_depth++
			defer {
				f.in_lambda_depth--
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
			mut name := f.short_module(node.name)
			f.mark_import_as_used(name)
			f.write('$name')
		}
	}
	if node.mod == '' && node.name == '' {
		f.write(node.left.str())
	}
	f.write_generic_call_if_require(node)
	f.write('(')
	f.call_args(node.args)
	f.write(')')
	f.or_expr(node.or_block)
	if is_method_newline {
		f.indent--
	}
}

fn (mut f Gen) write_generic_call_if_require(node ast.CallExpr) {
	if node.concrete_types.len > 0 {
		f.write('<')
		for i, concrete_type in node.concrete_types {
			mut name := f.table.type_to_str_using_aliases(concrete_type, f.mod2alias)
			tsym := f.table.sym(concrete_type)
			if tsym.language != .js && !tsym.name.starts_with('JS.') {
				name = f.short_module(name)
			} else if tsym.language == .js && !tsym.name.starts_with('JS.') {
				name = 'JS.' + name
			}
			f.write(name)
			f.mark_import_as_used(name)
			if i != node.concrete_types.len - 1 {
				f.write(', ')
			}
		}
		f.write('>')
	}
}

pub fn (mut f Gen) call_args(args []ast.CallArg) {
	f.single_line_fields = true
	old_short_arg_state := f.use_short_fn_args
	f.use_short_fn_args = false
	defer {
		f.single_line_fields = false
		f.use_short_fn_args = old_short_arg_state
	}
	for i, arg in args {
		if i == args.len - 1 && arg.expr is ast.StructInit {
			if arg.expr.typ == ast.void_type {
				f.use_short_fn_args = true
			}
		}
		if arg.is_mut {
			f.write(arg.share.str() + ' ')
		}
		if i > 0 && !f.single_line_if {
			f.wrap_long_line(3, true)
		}
		f.expr(arg.expr)
		if i < args.len - 1 {
			f.write(', ')
		}
	}
}

pub fn (mut f Gen) cast_expr(node ast.CastExpr) {
	f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias) + '(')
	f.mark_types_import_as_used(node.typ)
	f.expr(node.expr)
	if node.has_arg {
		f.write(', ')
		f.expr(node.arg)
	}
	f.write(')')
}

pub fn (mut f Gen) chan_init(mut node ast.ChanInit) {
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

pub fn (mut f Gen) comptime_call(node ast.ComptimeCall) {
	if node.is_vweb {
		if node.method_name == 'html' {
			f.write('\$vweb.html()')
		} else {
			f.write("\$tmpl('$node.args_var')")
		}
	} else {
		if node.is_embed {
			if node.embed_file.compression_type == 'none' {
				f.write("\$embed_file('$node.embed_file.rpath')")
			} else {
				f.write("\$embed_file('$node.embed_file.rpath', .$node.embed_file.compression_type)")
			}
		} else if node.is_env {
			f.write("\$env('$node.args_var')")
		} else if node.is_pkgconfig {
			f.write("\$pkgconfig('$node.args_var')")
		} else {
			inner_args := if node.args_var != '' {
				node.args_var
			} else {
				node.args.map(it.str()).join(', ')
			}
			method_expr := if node.has_parens {
				'(${node.method_name}($inner_args))'
			} else {
				'${node.method_name}($inner_args)'
			}
			f.write('${node.left}.$$method_expr')
		}
	}
}

pub fn (mut f Gen) comptime_selector(node ast.ComptimeSelector) {
	f.write('${node.left}.\$($node.field_expr)')
}

pub fn (mut f Gen) concat_expr(node ast.ConcatExpr) {
	for i, val in node.vals {
		if i != 0 {
			f.write(', ')
		}
		f.expr(val)
	}
}

pub fn (mut f Gen) dump_expr(node ast.DumpExpr) {
	f.write('dump(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Gen) enum_val(node ast.EnumVal) {
	name := f.short_module(node.enum_name)
	f.write(name + '.' + node.val)
	f.mark_import_as_used(name)
}

pub fn (mut f Gen) ident(node ast.Ident) {
	// if node.info is ast.IdentVar {
	// 	if node.info.is_mut {
	// 		f.write(node.info.share.str() + ' ')
	// 	}
	// 	var_info := node.var_info()
	// 	if var_info.is_static {
	// 		f.write('static ')
	// 	}
	// 	if var_info.is_volatile {
	// 		f.write('volatile ')
	// 	}
	// }
	f.write_language_prefix(node.language)
	if node.name == 'it' && f.it_name != '' && f.in_lambda_depth == 0 { // allow `it` in lambdas
		f.write(f.it_name)
	} else if node.kind == .blank_ident {
		f.write('_')
	} else {
		// mut is_local := false
		if !isnil(f.fn_scope) {
			if _ := f.fn_scope.find_var(node.name) {
				// is_local = true
			}
		}
		name := f.short_module(node.name)
		f.write(name)
		f.mark_import_as_used(name)
	}
}

pub fn (mut f Gen) if_expr(node ast.IfExpr) {
	dollar := if node.is_comptime { '$' } else { '' }
	mut is_ternary := node.branches.len == 2 && node.has_else
		&& branch_is_single_line(node.branches[0]) && branch_is_single_line(node.branches[1])
		&& (node.is_expr || f.is_assign || f.is_struct_init || f.single_line_fields)
	f.single_line_if = is_ternary
	// start_pos := f.out.len
	// start_len := f.line_len
	for {
		for i, branch in node.branches {
			if i == 0 {
				// first `if`
			} else {
				// `else`, close previous branch
				f.write('} ')
				f.write('${dollar}else ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				f.write('${dollar}if ')
				cur_pos := f.out.len
				f.expr(branch.cond)
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
		break
	}
	f.write('}')
	f.single_line_if = false
	if node.post_comments.len > 0 {
		f.writeln('')
	}
}

fn branch_is_single_line(b ast.IfBranch) bool {
	if b.stmts.len == 1 && stmt_is_single_line(b.stmts[0])
		&& b.pos.line_nr == b.stmts[0].pos.line_nr {
		return true
	}
	return false
}

pub fn (mut f Gen) if_guard_expr(node ast.IfGuardExpr) {
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

pub fn (mut f Gen) index_expr(node ast.IndexExpr) {
	f.expr(node.left)
	if node.index is ast.RangeExpr {
		if node.index.is_gated {
			f.write('#')
		}
	}
	f.write('[')
	f.expr(node.index)
	f.write(']')
	if node.or_expr.kind != .absent {
		f.or_expr(node.or_expr)
	}
}

pub fn (mut f Gen) infix_expr(node ast.InfixExpr) {
	buffering_save := f.buffering
	if !f.buffering && node.op in [.logical_or, .and, .plus] {
		f.buffering = true
	}
	is_assign_save := f.is_assign
	if node.op == .left_shift {
		f.is_assign = true // To write ternary if on a single line
	}
	// start_pos := f.out.len
	// start_len := f.line_len
	f.expr(node.left)
	left_type_sym := f.table.final_sym(node.left_type)
	is_array_push := left_type_sym.kind == .array && node.op == .left_shift
	is_one_val_array_init := node.op in [.key_in, .not_in] && node.right is ast.ArrayInit
		&& (node.right as ast.ArrayInit).exprs.len == 1
	if is_one_val_array_init {
		// `var in [val]` => `var == val`
		op := if node.op == .key_in { ' == ' } else { ' != ' }
		f.write(op)
	} else if is_array_push {
		f.write(' = ')
	} else {
		f.write(' $node.op.str() ')
	}
	if is_one_val_array_init {
		// `var in [val]` => `var == val`
		f.expr((node.right as ast.ArrayInit).exprs[0])
	} else if is_array_push {
		f.write('append(')
		f.expr(node.left)
		f.write(', ')
		f.expr(node.right)
		right_type_sym := f.table.final_sym(node.right_type)
		if right_type_sym.kind == .array {
			f.write('...')
		}
		f.write(')')
	} else {
		f.expr(node.right)
	}
	if !buffering_save && f.buffering {
		f.buffering = false
	}
	f.is_assign = is_assign_save
	f.or_expr(node.or_block)
}

pub fn (mut f Gen) wrap_infix(start_pos int, start_len int, is_cond bool) {
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
				conditions[ind] += '$p '
			} else {
				pen := if p == '||' { or_pen } else { 5 }
				penalties << pen
				conditions << '$p '
				ind++
			}
		} else if !is_cond_infix && p == '+' {
			penalties << 5
			conditions[ind] += '$p '
			conditions << ''
			ind++
		} else {
			conditions[ind] += '$p '
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

fn (mut f Gen) write_splitted_infix(conditions []string, penalties []int, ignore_paren bool, is_cond bool) {
	f.wsinfix_depth++
	defer {
		f.wsinfix_depth--
	}
	for i, cnd in conditions {
		c := cnd.trim_space()
		// is_paren_expr := (c[0] == `(` || (c.len > 5 && c[3] == `(`)) && c.ends_with(')')
		// final_len := ((f.indent + 1) * 4) + c.len
		if i == 0 {
			f.remove_new_line()
		}
		f.writeln('')
		f.indent++
		f.write(c)
		f.indent--
	}
}

pub fn (mut f Gen) likely(node ast.Likely) {
	if node.is_likely {
		f.write('_likely_')
	} else {
		f.write('_unlikely_')
	}
	f.write('(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Gen) lock_expr(node ast.LockExpr) {
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
		f.write('lock ')
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

pub fn (mut f Gen) map_init(node ast.MapInit) {
	if node.keys.len == 0 {
		if node.typ > ast.void_type {
			sym := f.table.sym(node.typ)
			info := sym.info as ast.Map
			f.mark_types_import_as_used(info.key_type)
			f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		}
		if node.pos.line_nr == node.pos.last_line {
			f.write('{}')
		} else {
			f.writeln('{')
			f.write('}')
		}
		return
	}
	f.writeln('{')
	f.indent++
	mut max_field_len := 0
	mut skeys := []string{}
	for key in node.keys {
		skey := f.node_str(key).trim_space()
		skeys << skey
		if skey.len > max_field_len {
			max_field_len = skey.len
		}
	}
	for i, key in node.keys {
		skey := skeys[i]
		f.write(skey)
		f.write(': ')
		f.write(strings.repeat(` `, max_field_len - skey.len))
		f.expr(node.vals[i])
		if key is ast.EnumVal && skey.starts_with('.') {
			// enforce the use of `,` for maps with short enum keys, otherwise there is ambiguity
			// when the values are struct values, and the code will no longer parse properly
			f.write(',')
		}
		f.writeln('')
	}
	f.indent--
	f.write('}')
}

fn (mut f Gen) match_branch(branch ast.MatchBranch, single_line bool) {
	if !branch.is_else {
		// normal branch
		f.is_mbranch_expr = true
		for j, expr in branch.exprs {
			estr := f.node_str(expr).trim_space()
			f.write(estr)
			if j < branch.ecmnts.len && branch.ecmnts[j].len > 0 {
				f.write(' ')
			}
			if j < branch.exprs.len - 1 {
				f.write(', ')
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
}

pub fn (mut f Gen) match_expr(node ast.MatchExpr) {
	f.write('match ')
	f.expr(node.cond)
	if node.cond is ast.Ident {
		f.it_name = node.cond.name
	}
	f.writeln(' {')
	f.indent++
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
	f.it_name = ''
}

pub fn (mut f Gen) offset_of(node ast.OffsetOf) {
	f.write('__offsetof(${f.table.type_to_str_using_aliases(node.struct_type, f.mod2alias)}, $node.field)')
	f.mark_types_import_as_used(node.struct_type)
}

pub fn (mut f Gen) or_expr(node ast.OrExpr) {
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
				// str := f.node_str(node.stmts[0]).trim_space()
				// single_line := ' or { $str }'
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

pub fn (mut f Gen) par_expr(node ast.ParExpr) {
	requires_paren := node.expr !is ast.Ident
	if requires_paren {
		f.par_level++
		f.write('(')
	}
	mut expr := node.expr
	for mut expr is ast.ParExpr {
		expr = expr.expr
	}
	f.expr(expr)
	if requires_paren {
		f.par_level--
		f.write(')')
	}
}

pub fn (mut f Gen) postfix_expr(node ast.PostfixExpr) {
	f.expr(node.expr)
	// `$if foo ?`
	if node.op == .question {
		f.write(' ?')
	} else {
		f.write('$node.op')
	}
	if node.is_c2v_prefix {
		f.write('$')
	}
}

pub fn (mut f Gen) prefix_expr(node ast.PrefixExpr) {
	// !(a in b) => a !in b, !(a is b) => a !is b
	if node.op == .not && node.right is ast.ParExpr {
		if node.right.expr is ast.InfixExpr {
			if node.right.expr.op in [.key_in, .not_in, .key_is, .not_is]
				&& node.right.expr.right !is ast.InfixExpr {
				f.expr(node.right.expr.left)
				if node.right.expr.op == .key_in {
					f.write(' !in ')
				} else if node.right.expr.op == .not_in {
					f.write(' in ')
				} else if node.right.expr.op == .key_is {
					f.write(' !is ')
				} else if node.right.expr.op == .not_is {
					f.write(' is ')
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

pub fn (mut f Gen) range_expr(node ast.RangeExpr) {
	f.expr(node.low)
	if f.is_mbranch_expr {
		f.write('...')
	} else {
		f.write('..')
	}
	f.expr(node.high)
}

pub fn (mut f Gen) select_expr(node ast.SelectExpr) {
	f.writeln('select {')
	f.indent++
	for branch in node.branches {
		if branch.comment.text != '' {
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
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Gen) selector_expr(node ast.SelectorExpr) {
	f.expr(node.expr)
	f.write('.')
	f.write(node.field_name)
}

pub fn (mut f Gen) size_of(node ast.SizeOf) {
	f.write('sizeof(')
	if node.is_type {
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	} else {
		f.expr(node.expr)
	}
	f.write(')')
}

pub fn (mut f Gen) is_ref_type(node ast.IsRefType) {
	f.write('isreftype(')
	if node.is_type {
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	} else {
		f.expr(node.expr)
	}
	f.write(')')
}

pub fn (mut f Gen) sql_expr(node ast.SqlExpr) {
	// sql app.db { select from Contributor where repo == id && user == 0 }
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')
	f.write('\tselect ')
	table_name := util.strip_mod_name(f.table.sym(node.table_expr.typ).name)
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
	f.write('from $table_name')
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
}

pub fn (mut f Gen) char_literal(node ast.CharLiteral) {
	if node.val == r"\'" {
		f.write("`'`")
		return
	}
	if node.val.len == 1 {
		clit := node.val[0]
		if clit < 32 || clit > 127 || clit == 92 || clit == 96 {
			f.write('`\\x$clit.hex()`')
			return
		}
	}
	f.write('`$node.val`')
}

pub fn (mut f Gen) string_literal(node ast.StringLiteral) {
	if node.is_raw {
		f.write('`$node.val`')
	} else {
		unescaped_val := node.val.replace('$golang.bs$golang.bs', '\x01').replace_each([
			"$golang.bs'",
			"'",
			'$golang.bs"',
			'"',
		])
		s := unescaped_val.replace_each(['\x01', '$golang.bs$golang.bs', '"', '$golang.bs"'])
		f.write('"$s"')
	}
}

pub fn (mut f Gen) string_inter_literal(node ast.StringInterLiteral) {
	mut quote := '"'
	// TODO: this code is very similar to ast.Expr.str()
	// serkonda7: it can not fully be replaced tho as ´f.expr()´ and `ast.Expr.str()`
	//	work too different for the various exprs that are interpolated
	f.write(quote)
	for i, val in node.vals {
		f.write(val.replace("$golang.bs'", "'"))
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
			f.expr(node.exprs[i])
		}
	}
	f.write(quote)
}

pub fn (mut f Gen) type_expr(node ast.TypeNode) {
	f.mark_types_import_as_used(node.typ)
	f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
}

pub fn (mut f Gen) type_of(node ast.TypeOf) {
	f.write('typeof(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Gen) unsafe_expr(node ast.UnsafeExpr) {
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

fn (mut f Gen) trace(fbase string, message string) {
	// if f.file.path_base == fbase {
	// println('> f.trace | ${fbase:-10s} | $message')
	//}
}

pub fn (mut g Gen) error(s string) {
	util.verror('golang backend error', s)
}
