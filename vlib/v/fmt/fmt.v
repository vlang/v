// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import math.mathutil as mu
import v.ast
import strings
import v.util
import v.pref

const (
	bs      = '\\'
	// when to break a line dependant on penalty
	max_len = [0, 35, 60, 85, 93, 100]
)

pub struct Fmt {
pub mut:
	table              &ast.Table
	out_imports        strings.Builder
	out                strings.Builder
	indent             int
	empty_line         bool
	line_len           int    // the current line length, NB: it counts \t as 4 spaces, and starts at 0 after f.writeln
	buffering          bool   // disables line wrapping for exprs that will be analyzed later
	par_level          int    // how many parentheses are put around the current expression
	array_init_break   []bool // line breaks after elements in hierarchy level of multi dimensional array
	array_init_depth   int    // current level of hierarchie in array init
	single_line_if     bool
	cur_mod            string
	file               ast.File
	did_imports        bool
	is_assign          bool
	is_struct_init     bool
	auto_imports       []string        // automatically inserted imports that the user forgot to specify
	import_pos         int             // position of the imports in the resulting string for later autoimports insertion
	used_imports       []string        // to remove unused imports
	import_syms_used   map[string]bool // to remove unused import symbols.
	is_debug           bool
	mod2alias          map[string]string // for `import time as t`, will contain: 'time'=>'t'
	use_short_fn_args  bool
	single_line_fields bool   // should struct fields be on a single line
	it_name            string // the name to replace `it` with
	inside_lambda      bool
	inside_const       bool
	is_mbranch_expr    bool // match a { x...y { } }
	pref               &pref.Preferences
}

pub fn fmt(file ast.File, table &ast.Table, pref &pref.Preferences, is_debug bool) string {
	mut f := Fmt{
		out: strings.new_builder(1000)
		out_imports: strings.new_builder(200)
		table: table
		indent: 0
		file: file
		is_debug: is_debug
		pref: pref
	}
	f.process_file_imports(file)
	f.set_current_module_name('main')
	// As these are toplevel stmts, the indent increase done in f.stmts() has to be compensated
	f.indent--
	f.stmts(file.stmts)
	f.indent++
	f.imports(f.file.imports) // now that we have all autoimports, handle them
	res := f.out.str().trim_space() + '\n'
	if res.len == 1 {
		return f.out_imports.str().trim_space() + '\n'
	}
	bounded_import_pos := mu.min(res.len, f.import_pos)
	return res[..bounded_import_pos] + f.out_imports.str() + res[bounded_import_pos..]
}

pub fn (mut f Fmt) process_file_imports(file &ast.File) {
	for imp in file.imports {
		mod := imp.mod.all_after_last('.')
		f.mod2alias[mod] = imp.alias
		for sym in imp.syms {
			f.mod2alias['${imp.mod}.$sym.name'] = sym.name
			f.mod2alias['${mod}.$sym.name'] = sym.name
			f.mod2alias[sym.name] = sym.name
			f.import_syms_used[sym.name] = false
		}
	}
	f.auto_imports = file.auto_imports
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
	if f.indent > 0 && f.empty_line && s.len > 0 {
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

fn (mut f Fmt) write_language_prefix(lang ast.Language) {
	match lang {
		.c { f.write('C.') }
		.js { f.write('JS.') }
		else {}
	}
}

pub fn (mut f Fmt) wrap_long_line(penalty_idx int, add_indent bool) bool {
	if f.buffering {
		return false
	}
	if penalty_idx > 0 && f.line_len <= fmt.max_len[penalty_idx] {
		return false
	}
	if f.out.buf[f.out.buf.len - 1] == ` ` {
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

pub struct RemoveNewLineConfig {
	imports_buffer bool // Work on f.out_imports instead of f.out
}

pub fn (mut f Fmt) remove_new_line(cfg RemoveNewLineConfig) {
	mut buffer := if cfg.imports_buffer { &f.out_imports } else { &f.out }
	mut i := 0
	for i = buffer.len - 1; i >= 0; i-- {
		if !buffer.buf[i].is_space() { // != `\n` {
			break
		}
	}
	buffer.go_back(buffer.len - i - 1)
	f.empty_line = false
}

//=== Module handling helper methods ===//

pub fn (mut f Fmt) set_current_module_name(cmodname string) {
	f.cur_mod = cmodname
	f.table.cmod_prefix = cmodname + '.'
}

fn (f Fmt) get_modname_prefix(mname string) (string, string) {
	// ./tests/proto_module_importing_vproto_keep.vv to know, why here is checked for ']' and '&'
	if !mname.contains(']') && !mname.contains('&') {
		return mname, ''
	}
	after_rbc := mname.all_after_last(']')
	after_ref := mname.all_after_last('&')
	modname := if after_rbc.len < after_ref.len { after_rbc } else { after_ref }
	return modname, mname.trim_suffix(modname)
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
	if !name.contains('.') {
		return name
	}
	if name in f.mod2alias {
		return f.mod2alias[name]
	}
	if name.ends_with('>') {
		x := name.trim_suffix('>').split('<')
		if x.len == 2 {
			main := f.short_module(x[0])
			genlist := x[1].split(',')
			genshorts := genlist.map(f.short_module(it)).join(',')
			return '$main<$genshorts>'
		}
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	mname, tprefix := f.get_modname_prefix(vals[vals.len - 2])
	symname := vals[vals.len - 1]
	aname := f.mod2alias[mname]
	if aname == '' {
		return symname
	}
	return '$tprefix${aname}.$symname'
}

//=== Import-related methods ===//

pub fn (mut f Fmt) mark_types_import_as_used(typ ast.Type) {
	sym := f.table.get_type_symbol(typ)
	f.mark_import_as_used(sym.name)
}

// `name` is a function (`foo.bar()`) or type (`foo.Bar{}`)
pub fn (mut f Fmt) mark_import_as_used(name string) {
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

pub fn (mut f Fmt) imports(imports []ast.Import) {
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
		f.import_comments(imp.comments, inline: true)
		f.import_comments(imp.next_comments, {})
		num_imports++
	}
	if num_imports > 0 {
		f.out_imports.writeln('')
	}
}

pub fn (f Fmt) imp_stmt_str(imp ast.Import) string {
	is_diff := imp.alias != imp.mod && !imp.mod.ends_with('.' + imp.alias)
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
	return '$imp.mod$imp_alias_suffix'
}

//=== Node helpers ===//

fn (f Fmt) should_insert_newline_before_node(node ast.Node, prev_node ast.Node) bool {
	// No need to insert a newline if there is already one
	if f.out.last_n(2) == '\n\n' {
		return false
	}
	prev_line_nr := prev_node.position().last_line
	// The nodes are Stmts
	if node is ast.Stmt && prev_node is ast.Stmt {
		stmt := node as ast.Stmt
		prev_stmt := prev_node as ast.Stmt
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
	if node.position().line_nr - prev_line_nr <= 1 {
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
	str := f.out.after(pos).trim_space()
	f.out.go_back_to(pos)
	f.empty_line = was_empty_line
	f.line_len = prev_line_len
	return str
}

//=== General Stmt-related methods and helpers ===//

pub fn (mut f Fmt) stmts(stmts []ast.Stmt) {
	mut prev_stmt := if stmts.len > 0 { stmts[0] } else { ast.empty_stmt() }
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

pub fn (mut f Fmt) stmt(node ast.Stmt) {
	if f.is_debug {
		eprintln('stmt: ${node.pos:-42} | node: ${node.type_name():-20}')
	}
	match node {
		ast.NodeError {}
		ast.EmptyStmt {}
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
			f.block(node)
		}
		ast.BranchStmt {
			f.branch_stmt(node)
		}
		ast.CompFor {
			f.comp_for(node)
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
			f.mod(node)
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

pub fn (mut f Fmt) expr(node ast.Expr) {
	if f.is_debug {
		eprintln('expr: ${node.position():-42} | node: ${node.type_name():-20} | $node.str()')
	}
	match mut node {
		ast.NodeError {}
		ast.EmptyExpr {}
		ast.AnonFn {
			f.fn_decl(node.decl)
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
			f.write('`$node.val`')
		}
		ast.Comment {
			f.comment(node, inline: true)
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
			if !expr.is_short && (expr.fields.len > 0 || expr.pre_comments.len > 0) {
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
		else {}
	}
	return true
}

//=== Specific Stmt methods ===//

fn (mut f Fmt) asm_stmt(stmt ast.AsmStmt) {
	f.write('asm ')
	if stmt.is_volatile {
		f.write('volatile ')
	} else if stmt.is_goto {
		f.write('goto ')
	}
	f.writeln('$stmt.arch {')
	f.indent++
	for template in stmt.templates {
		if template.is_directive {
			f.write('.')
		}
		f.write('$template.name')
		if template.is_label {
			f.write(':')
		} else {
			if template.args.len > 0 {
				f.write(' ')
			}
		}
		for i, arg in template.args {
			f.asm_arg(arg)
			if i + 1 < template.args.len {
				f.write(', ')
			}
		}
		if template.comments.len == 0 {
			f.writeln('')
		} else {
			f.comments(template.comments, inline: false)
		}
	}
	if stmt.output.len != 0 || stmt.input.len != 0 || stmt.clobbered.len != 0 {
		f.write('; ')
	}
	f.asm_ios(stmt.output)

	if stmt.input.len != 0 || stmt.clobbered.len != 0 {
		f.write('; ')
	}
	f.asm_ios(stmt.input)

	if stmt.clobbered.len != 0 {
		f.write('; ')
	}
	for i, clob in stmt.clobbered {
		if i != 0 {
			f.write('  ')
		}
		f.write(clob.reg.name)

		if clob.comments.len == 0 {
			f.writeln('')
		} else {
			f.comments(clob.comments, inline: false)
		}
	}
	f.indent--
	f.writeln('}')
}

fn (mut f Fmt) asm_arg(arg ast.AsmArg) {
	match arg {
		ast.AsmRegister {
			f.asm_reg(arg)
		}
		ast.AsmAlias {
			f.write('$arg.name')
		}
		ast.IntegerLiteral, ast.FloatLiteral, ast.CharLiteral {
			f.write(arg.val)
		}
		ast.BoolLiteral {
			f.write(arg.val.str())
		}
		string {
			f.write(arg)
		}
		ast.AsmAddressing {
			f.write('[')
			base := arg.base
			index := arg.index
			displacement := arg.displacement
			scale := arg.scale
			match arg.mode {
				.base {
					f.asm_arg(base)
				}
				.displacement {
					f.asm_arg(displacement)
				}
				.base_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(displacement)
				}
				.index_times_scale_plus_displacement {
					f.asm_arg(index)
					f.write(' * $scale + ')
					f.asm_arg(displacement)
				}
				.base_plus_index_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(index)
					f.write(' + ')
					f.asm_arg(displacement)
				}
				.base_plus_index_times_scale_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(index)
					f.write(' * $scale + ')
					f.asm_arg(displacement)
				}
				.rip_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(displacement)
				}
				.invalid {
					panic('fmt: invalid addressing mode')
				}
			}
			f.write(']')
		}
		ast.AsmDisp {
			f.write(arg.val)
		}
	}
}

fn (mut f Fmt) asm_reg(reg ast.AsmRegister) {
	f.write(reg.name)
}

fn (mut f Fmt) asm_ios(ios []ast.AsmIO) {
	for i, io in ios {
		if i != 0 {
			f.write('  ')
		}

		f.write('$io.constraint ($io.expr)')
		mut as_block := true
		if io.expr is ast.Ident {
			if io.expr.name == io.alias {
				as_block = false
			}
		}
		if as_block && io.alias != '' {
			f.write(' as $io.alias')
		}
		if io.comments.len == 0 {
			f.writeln('')
		} else {
			f.comments(io.comments, inline: false)
		}
	}
}

pub fn (mut f Fmt) assert_stmt(node ast.AssertStmt) {
	f.write('assert ')
	if node.expr is ast.ParExpr {
		if node.expr.expr is ast.InfixExpr {
			infix := node.expr.expr
			f.expr(infix)
			f.writeln('')
			return
		}
	}
	f.expr(node.expr)
	f.writeln('')
}

pub fn (mut f Fmt) assign_stmt(node ast.AssignStmt) {
	f.comments(node.comments, {})
	for i, left in node.left {
		f.expr(left)
		if i < node.left.len - 1 {
			f.write(', ')
		}
	}
	f.is_assign = true
	f.write(' $node.op.str() ')
	for i, val in node.right {
		f.prefix_expr_cast_expr(val)
		if i < node.right.len - 1 {
			f.write(', ')
		}
	}
	f.comments(node.end_comments, has_nl: false, inline: true, level: .keep)
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

pub fn (mut f Fmt) branch_stmt(node ast.BranchStmt) {
	f.writeln(node.str())
}

pub fn (mut f Fmt) comp_for(node ast.CompFor) {
	typ := f.no_cur_mod(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	f.write('\$for $node.val_var in ${typ}.$node.kind.str() {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) const_decl(node ast.ConstDecl) {
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
	mut max := 0
	if node.is_block {
		f.writeln('(')
		for field in node.fields {
			if field.name.len > max {
				max = field.name.len
			}
		}
		f.indent++
	}
	mut prev_field := if node.fields.len > 0 {
		ast.Node(node.fields[0])
	} else {
		ast.Node(ast.NodeError{})
	}
	for field in node.fields {
		if field.comments.len > 0 {
			if f.should_insert_newline_before_node(ast.Expr(field.comments[0]), prev_field) {
				f.writeln('')
			}
			f.comments(field.comments, inline: true)
			prev_field = ast.Expr(field.comments.last())
		}
		if node.is_block && f.should_insert_newline_before_node(field, prev_field) {
			f.writeln('')
		}
		name := field.name.after('.')
		f.write('$name ')
		f.write(strings.repeat(` `, max - field.name.len))
		f.write('= ')
		f.expr(field.expr)
		f.writeln('')
		prev_field = field
	}
	f.comments_after_last_field(node.end_comments)
	if node.is_block {
		f.indent--
		f.writeln(')\n')
	} else {
		f.writeln('')
	}
}

pub fn (mut f Fmt) defer_stmt(node ast.DeferStmt) {
	f.write('defer {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) expr_stmt(node ast.ExprStmt) {
	f.comments(node.comments, {})
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
	name := node.name.after('.')
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln('enum $name {}\n')
		return
	}
	f.writeln('enum $name {')
	f.comments(node.comments, inline: true, level: .indent)
	for field in node.fields {
		f.write('\t$field.name')
		if field.has_expr {
			f.write(' = ')
			f.expr(field.expr)
		}
		f.comments(field.comments, inline: true, has_nl: false, level: .indent)
		f.writeln('')
		f.comments(field.next_comments, inline: false, has_nl: true, level: .indent)
	}
	f.writeln('}\n')
}

pub fn (mut f Fmt) fn_decl(node ast.FnDecl) {
	f.attrs(node.attrs)
	f.write(node.stringify(f.table, f.cur_mod, f.mod2alias)) // `Expr` instead of `ast.Expr` in mod ast
	if node.language == .v {
		if !node.no_body {
			f.write(' {')
			if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
				f.writeln('')
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
	// Mark all function's used type so that they are not removed from imports
	for arg in node.params {
		f.mark_types_import_as_used(arg.typ)
	}
	f.mark_types_import_as_used(node.return_type)
}

pub fn (mut f Fmt) for_c_stmt(node ast.ForCStmt) {
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
	f.remove_new_line({})
	f.write(' {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) for_in_stmt(node ast.ForInStmt) {
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
	f.write(' in ')
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

pub fn (mut f Fmt) for_stmt(node ast.ForStmt) {
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

pub fn (mut f Fmt) global_decl(node ast.GlobalDecl) {
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
		f.comments(field.comments, inline: true)
		f.write('$field.name ')
		f.write(strings.repeat(` `, max - field.name.len))
		if field.has_expr {
			f.write('= ')
			f.write(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
			f.write('(')
			f.expr(field.expr)
			f.write(')')
		} else {
			f.write('${f.table.type_to_str_using_aliases(field.typ, f.mod2alias)}')
		}
		if node.is_block {
			f.writeln('')
		}
	}
	f.comments_after_last_field(node.end_comments)
	if node.is_block {
		f.indent--
		f.writeln(')')
	} else {
		f.writeln('')
	}
}

pub fn (mut f Fmt) go_expr(node ast.GoExpr) {
	f.write('go ')
	f.expr(node.call_expr)
}

pub fn (mut f Fmt) goto_label(node ast.GotoLabel) {
	f.writeln('$node.name:')
}

pub fn (mut f Fmt) goto_stmt(node ast.GotoStmt) {
	f.writeln('goto $node.name')
}

pub fn (mut f Fmt) hash_stmt(node ast.HashStmt) {
	f.writeln('#$node.val')
}

pub fn (mut f Fmt) interface_decl(node ast.InterfaceDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	name := node.name.after('.')
	f.write('interface $name {')
	if node.fields.len > 0 || node.methods.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
	}
	f.comments_after_last_field(node.pre_comments)
	for i, field in node.fields {
		if i == node.mut_pos {
			f.writeln('mut:')
		}
		// TODO: alignment, comments, etc.
		mut ft := f.no_cur_mod(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
		if !ft.contains('C.') && !ft.contains('JS.') && !ft.contains('fn (') {
			ft = f.short_module(ft)
		}
		f.writeln('\t$field.name $ft')
		f.mark_types_import_as_used(field.typ)
	}
	for method in node.methods {
		f.write('\t')
		f.write(method.stringify(f.table, f.cur_mod, f.mod2alias).after('fn '))
		f.comments(method.comments, inline: true, has_nl: false, level: .indent)
		f.writeln('')
		f.comments(method.next_comments, inline: false, has_nl: true, level: .indent)
		for param in method.params {
			f.mark_types_import_as_used(param.typ)
		}
		f.mark_types_import_as_used(method.return_type)
	}
	f.writeln('}\n')
}

pub fn (mut f Fmt) mod(mod ast.Module) {
	f.set_current_module_name(mod.name)
	if mod.is_skipped {
		return
	}
	f.attrs(mod.attrs)
	f.writeln('module $mod.short_name\n')
	if f.import_pos == 0 {
		f.import_pos = f.out.len
	}
}

pub fn (mut f Fmt) return_stmt(node ast.Return) {
	f.comments(node.comments, {})
	f.write('return')
	if node.exprs.len > 0 {
		f.write(' ')
		// Loop over all return values. In normal returns this will only run once.
		for i, expr in node.exprs {
			f.expr(expr)
			if i < node.exprs.len - 1 {
				f.write(', ')
			}
		}
	}
	f.writeln('')
}

pub fn (mut f Fmt) sql_stmt(node ast.SqlStmt) {
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')
	table_name := util.strip_mod_name(f.table.get_type_symbol(node.table_expr.typ).name)
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
	f.writeln('}')
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
	ptype := f.table.type_to_str_using_aliases(node.parent_type, f.mod2alias)
	f.write('type $node.name = $ptype')

	f.comments(node.comments, has_nl: false)
}

pub fn (mut f Fmt) fn_type_decl(node ast.FnTypeDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	typ_sym := f.table.get_type_symbol(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	fn_name := f.no_cur_mod(node.name)
	f.write('type $fn_name = fn (')
	for i, arg in fn_info.params {
		if arg.is_mut {
			f.write(arg.typ.share().str() + ' ')
		}
		f.write(arg.name)
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
		ret_str := f.no_cur_mod(f.table.type_to_str_using_aliases(fn_info.return_type,
			f.mod2alias))
		f.write(' $ret_str')
	} else if fn_info.return_type.has_flag(.optional) {
		f.write(' ?')
	}

	f.comments(node.comments, has_nl: false)
	f.writeln('')
}

pub fn (mut f Fmt) sum_type_decl(node ast.SumTypeDecl) {
	if node.is_pub {
		f.write('pub ')
	}
	f.write('type $node.name = ')
	mut sum_type_names := []string{}
	for t in node.variants {
		sum_type_names << f.table.type_to_str_using_aliases(t.typ, f.mod2alias)
	}
	sum_type_names.sort()
	for i, name in sum_type_names {
		f.write(name)
		if i < sum_type_names.len - 1 {
			f.write(' | ')
		}
		if i < sum_type_names.len - 1 {
			f.wrap_long_line(3, true)
		}
	}

	f.comments(node.comments, has_nl: false)
}

//=== Specific Expr methods ===//

pub fn (mut f Fmt) array_decompose(node ast.ArrayDecompose) {
	f.write('...')
	f.expr(node.expr)
}

pub fn (mut f Fmt) array_init(node ast.ArrayInit) {
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
				f.comment(c, level: .indent, iembed: true)
				f.write(' ')
			}
		} else {
			if c.pos.last_line < node.pos.last_line {
				f.comment(c, level: .indent)
				if node.exprs.len == 0 {
					f.writeln('')
				}
			} else {
				f.comment(c, level: .indent, iembed: true)
				if node.exprs.len > 0 {
					f.write(' ')
				}
			}
		}
		last_line_nr = c.pos.last_line
	}
	mut set_comma := false
	for i, expr in node.exprs {
		pos := expr.position()
		if i == 0 {
			if f.array_init_depth > f.array_init_break.len {
				f.array_init_break << pos.line_nr > last_line_nr
			}
		}
		line_break := f.array_init_break[f.array_init_depth - 1]
		mut penalty := if line_break { 0 } else { 4 }
		if penalty > 0 {
			if i == 0 || should_decrease_arr_penalty(node.exprs[i - 1]) {
				penalty--
			}
			if should_decrease_arr_penalty(expr) {
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
			estr := f.node_str(expr)
			if !is_new_line && !f.buffering && f.line_len + estr.len > fmt.max_len.last() {
				f.writeln('')
				is_new_line = true
				if !inc_indent {
					f.indent++
					inc_indent = true
				}
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
		if i < node.ecmnts.len && node.ecmnts[i].len > 0 {
			expr_pos := expr.position()
			for cmt in node.ecmnts[i] {
				if !set_comma && cmt.pos.pos > expr_pos.pos + expr_pos.len + 2 {
					f.write(',')
					set_comma = true
				}
				if cmt.pos.line_nr > expr_pos.last_line {
					embed := i + 1 < node.exprs.len
						&& node.exprs[i + 1].position().line_nr == cmt.pos.last_line
					f.writeln('')
					f.comment(cmt, iembed: embed)
				} else {
					f.write(' ')
					f.comment(cmt, iembed: true)
				}
			}
		}
		if i == node.exprs.len - 1 {
			if is_new_line {
				if !set_comma {
					f.write(',')
				}
				f.writeln('')
			}
		} else if !set_comma {
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
	// `[100]byte`
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

fn should_decrease_arr_penalty(e ast.Expr) bool {
	if e is ast.ArrayInit || e is ast.StructInit || e is ast.MapInit || e is ast.CallExpr {
		return true
	}
	return false
}

pub fn (mut f Fmt) as_cast(node ast.AsCast) {
	type_str := f.table.type_to_str_using_aliases(node.typ, f.mod2alias)
	f.expr(node.expr)
	f.write(' as $type_str')
}

pub fn (mut f Fmt) assoc(node ast.Assoc) {
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

pub fn (mut f Fmt) at_expr(node ast.AtExpr) {
	f.write(node.name)
}

pub fn (mut f Fmt) call_expr(node ast.CallExpr) {
	old_short_arg_state := f.use_short_fn_args
	f.use_short_fn_args = false
	if node.args.len > 0 && node.args.last().expr is ast.StructInit {
		struct_expr := node.args.last().expr as ast.StructInit
		if struct_expr.typ == ast.void_type {
			f.use_short_fn_args = true
		}
	}
	for arg in node.args {
		f.comments(arg.comments, {})
	}
	if node.is_method {
		if node.name in ['map', 'filter'] {
			f.inside_lambda = true
			defer {
				f.inside_lambda = false
			}
		}
		if node.left is ast.Ident {
			// `time.now()` without `time imported` is processed as a method call with `time` being
			// a `node.left` expression. Import `time` automatically.
			// TODO fetch all available modules
			if node.left.name in ['time', 'os', 'strings', 'math', 'json', 'base64'] {
				f.file.imports << ast.Import{
					mod: node.left.name
					alias: node.left.name
				}
			}
		}
		f.expr(node.left)
		f.write('.' + node.name)
	} else {
		f.write_language_prefix(node.language)
		if node.left is ast.AnonFn {
			f.fn_decl(node.left.decl)
		} else if node.language != .v {
			f.write('${node.name.after_char(`.`)}')
		} else {
			mut name := f.short_module(node.name)
			f.mark_import_as_used(name)
			if node.name in f.mod2alias {
				name = f.mod2alias[node.name]
			}
			f.write('$name')
		}
	}
	if node.mod == '' && node.name == '' {
		f.write(node.left.str())
	}
	f.write_generic_if_require(node)
	f.write('(')
	f.call_args(node.args)
	f.write(')')
	f.or_expr(node.or_block)
	f.comments(node.comments, has_nl: false)
	f.use_short_fn_args = old_short_arg_state
}

fn (mut f Fmt) write_generic_if_require(node ast.CallExpr) {
	if node.concrete_types.len > 0 {
		f.write('<')
		for i, concrete_type in node.concrete_types {
			is_last := i == node.concrete_types.len - 1
			f.write(f.table.type_to_str(concrete_type))
			if !is_last {
				f.write(', ')
			}
		}
		f.write('>')
	}
}

pub fn (mut f Fmt) call_args(args []ast.CallArg) {
	f.single_line_fields = true
	defer {
		f.single_line_fields = false
	}
	for i, arg in args {
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

pub fn (mut f Fmt) cast_expr(node ast.CastExpr) {
	f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias) + '(')
	f.expr(node.expr)
	if node.has_arg {
		f.write(', ')
		f.expr(node.arg)
	}
	f.write(')')
}

pub fn (mut f Fmt) chan_init(mut node ast.ChanInit) {
	info := f.table.get_type_symbol(node.typ).chan_info()
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
			f.write('\$vweb.html()')
		} else {
			f.write("\$tmpl('$node.args_var')")
		}
	} else {
		if node.is_embed {
			f.write("\$embed_file('$node.embed_file.rpath')")
		} else if node.is_env {
			f.write("\$env('$node.args_var')")
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

pub fn (mut f Fmt) comptime_selector(node ast.ComptimeSelector) {
	f.write('${node.left}.\$($node.field_expr)')
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
}

pub fn (mut f Fmt) ident(node ast.Ident) {
	if mut node.info is ast.IdentVar {
		if node.info.is_mut {
			f.write(node.info.share.str() + ' ')
		}
		var_info := node.var_info()
		if var_info.is_static {
			f.write('static ')
		}
	}
	f.write_language_prefix(node.language)
	if node.name == 'it' && f.it_name != '' && !f.inside_lambda { // allow `it` in lambdas
		f.write(f.it_name)
	} else if node.kind == .blank_ident {
		f.write('_')
	} else {
		// Force usage of full path to const in the same module:
		// `println(minute)` => `println(time.minute)`
		// This makes it clear that a module const is being used
		// (since V's conts are no longer ALL_CAP).
		// ^^^ except for `main`, where consts are allowed to not have a `main.` prefix.
		if !node.name.contains('.') && !f.inside_const {
			mod := f.cur_mod
			full_name := mod + '.' + node.name
			if obj := f.file.global_scope.find(full_name) {
				if obj is ast.ConstField {
					// "v.fmt.foo" => "fmt.foo"
					vals := full_name.split('.')
					mod_prefix := vals[vals.len - 2]
					const_name := vals[vals.len - 1]
					if mod_prefix == 'main' {
						f.write(const_name)
						return
					} else {
						short := mod_prefix + '.' + const_name
						f.write(short)
						f.mark_import_as_used(short)
						return
					}
				}
			}
		}
		name := f.short_module(node.name)
		f.write(name)
		f.mark_import_as_used(name)
	}
}

pub fn (mut f Fmt) if_expr(node ast.IfExpr) {
	dollar := if node.is_comptime { '$' } else { '' }
	mut is_ternary := node.branches.len == 2 && node.has_else
		&& branch_is_single_line(node.branches[0]) && branch_is_single_line(node.branches[1])
		&& (node.is_expr || f.is_assign || f.is_struct_init || f.single_line_fields)
	f.single_line_if = is_ternary
	start_pos := f.out.len
	start_len := f.line_len
	for {
		for i, branch in node.branches {
			if i == 0 {
				// first `if`
				f.comments(branch.comments, {})
			} else {
				// `else`, close previous branch
				if branch.comments.len > 0 {
					f.writeln('}')
					f.comments(branch.comments, {})
				} else {
					f.write('} ')
				}
				f.write('${dollar}else ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				f.write('${dollar}if ')
				cur_pos := f.out.len
				f.expr(branch.cond)
				cond_len := f.out.len - cur_pos
				is_cond_wrapped := cond_len > 0
					&& (branch.cond is ast.IfGuardExpr || branch.cond is ast.CallExpr)
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
		if is_ternary && f.line_len > fmt.max_len.last() && !f.buffering {
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
	if node.post_comments.len > 0 {
		f.writeln('')
		f.comments(node.post_comments,
			has_nl: false
			prev_line: node.branches.last().body_pos.last_line
		)
	}
}

fn branch_is_single_line(b ast.IfBranch) bool {
	if b.stmts.len == 1 && b.comments.len == 0 && stmt_is_single_line(b.stmts[0]) {
		return true
	}
	return false
}

pub fn (mut f Fmt) if_guard_expr(node ast.IfGuardExpr) {
	f.write(node.var_name + ' := ')
	f.expr(node.expr)
}

pub fn (mut f Fmt) index_expr(node ast.IndexExpr) {
	f.expr(node.left)
	f.write('[')
	f.expr(node.index)
	f.write(']')
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
	f.expr(node.left)
	is_one_val_array_init := node.op in [.key_in, .not_in] && node.right is ast.ArrayInit
		&& (node.right as ast.ArrayInit).exprs.len == 1
	if is_one_val_array_init {
		// `var in [val]` => `var == val`
		op := if node.op == .key_in { ' == ' } else { ' != ' }
		f.write(op)
	} else {
		f.write(' $node.op.str() ')
	}
	if is_one_val_array_init {
		// `var in [val]` => `var == val`
		f.expr((node.right as ast.ArrayInit).exprs[0])
	} else {
		f.expr(node.right)
	}
	if !buffering_save && f.buffering {
		f.buffering = false
		if !f.single_line_if && f.line_len > fmt.max_len.last() {
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

fn (mut f Fmt) write_splitted_infix(conditions []string, penalties []int, ignore_paren bool, is_cond bool) {
	for i, cnd in conditions {
		c := cnd.trim_space()
		if f.line_len + c.len < fmt.max_len[penalties[i]] {
			if (i > 0 && i < conditions.len) || (ignore_paren && i == 0 && c.len > 5 && c[3] == `(`) {
				f.write(' ')
			}
			f.write(c)
		} else {
			is_paren_expr := (c[0] == `(` || (c.len > 5 && c[3] == `(`)) && c.ends_with(')')
			final_len := ((f.indent + 1) * 4) + c.len
			if final_len > fmt.max_len.last() && is_paren_expr {
				conds, pens := split_up_infix(c, true, is_cond)
				f.write_splitted_infix(conds, pens, true, is_cond)
				continue
			}
			if i == 0 {
				f.remove_new_line({})
			}
			f.writeln('')
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

pub fn (mut f Fmt) map_init(node ast.MapInit) {
	if node.keys.len == 0 {
		if node.typ > ast.void_type {
			f.mark_types_import_as_used(node.typ)
			f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
		} else {
			// m = map{}
			f.write('map')
		}
		f.write('{}')
		return
	}
	f.writeln('map{')
	f.indent++
	f.comments(node.pre_cmnts, {})
	mut max_field_len := 0
	for key in node.keys {
		if key.str().len > max_field_len {
			max_field_len = key.str().len
		}
	}
	for i, key in node.keys {
		f.expr(key)
		f.write(': ')
		f.write(strings.repeat(` `, max_field_len - key.str().len))
		f.expr(node.vals[i])
		f.comments(node.comments[i], prev_line: node.vals[i].position().last_line, has_nl: false)
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
			estr := f.node_str(expr)
			if f.line_len + estr.len + 2 > fmt.max_len[5] {
				f.remove_new_line({})
				f.writeln('')
			}
			f.write(estr)
			if j < branch.ecmnts.len && branch.ecmnts[j].len > 0 {
				f.write(' ')
				f.comments(branch.ecmnts[j], iembed: true)
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
			f.remove_new_line({})
			f.writeln(' }')
		} else {
			f.writeln('}')
		}
	}
	f.comments(branch.post_comments, inline: true)
}

pub fn (mut f Fmt) match_expr(node ast.MatchExpr) {
	f.write('match ')
	f.expr(node.cond)
	if node.cond is ast.Ident {
		f.it_name = node.cond.name
	}
	f.writeln(' {')
	f.indent++
	f.comments(node.comments, {})
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

pub fn (mut f Fmt) offset_of(node ast.OffsetOf) {
	f.write('__offsetof(${f.table.type_to_str(node.struct_type)}, $node.field)')
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
				single_line := ' or { $str }'
				if single_line.len + f.line_len <= fmt.max_len.last() {
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
		.propagate {
			f.write(' ?')
		}
	}
}

pub fn (mut f Fmt) par_expr(node ast.ParExpr) {
	requires_paren := node.expr !is ast.Ident
	if requires_paren {
		f.par_level++
		f.write('(')
	}
	f.expr(node.expr)
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
		f.write('$node.op')
	}
}

pub fn (mut f Fmt) prefix_expr(node ast.PrefixExpr) {
	f.write(node.op.str())
	f.prefix_expr_cast_expr(node.right)
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) range_expr(node ast.RangeExpr) {
	f.expr(node.low)
	if f.is_mbranch_expr {
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
			f.comment(branch.comment, inline: true)
			f.writeln('')
		}
		if branch.is_else {
			f.write('else {')
		} else {
			if branch.is_timeout {
				f.write('> ')
			}
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
			f.comments(branch.post_comments, inline: true)
		}
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) selector_expr(node ast.SelectorExpr) {
	f.expr(node.expr)
	f.write('.')
	f.write(node.field_name)
}

pub fn (mut f Fmt) size_of(node ast.SizeOf) {
	f.write('sizeof(')
	if node.is_type {
		f.write(f.table.type_to_str_using_aliases(node.typ, f.mod2alias))
	} else {
		f.expr(node.expr)
	}
	f.write(')')
}

pub fn (mut f Fmt) sql_expr(node ast.SqlExpr) {
	// sql app.db { select from Contributor where repo == id && user == 0 }
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')
	f.write('\tselect ')
	table_name := util.strip_mod_name(f.table.get_type_symbol(node.table_expr.typ).name)
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

pub fn (mut f Fmt) string_literal(node ast.StringLiteral) {
	use_double_quote := node.val.contains("'") && !node.val.contains('"')
	if node.is_raw {
		f.write('r')
	} else if node.language == ast.Language.c {
		f.write('c')
	}
	if node.is_raw {
		if use_double_quote {
			f.write('"$node.val"')
		} else {
			f.write("'$node.val'")
		}
	} else {
		unescaped_val := node.val.replace('$fmt.bs$fmt.bs', '\x01').replace_each(["$fmt.bs'", "'",
			'$fmt.bs"', '"'])
		if use_double_quote {
			s := unescaped_val.replace_each(['\x01', '$fmt.bs$fmt.bs', '"', '$fmt.bs"'])
			f.write('"$s"')
		} else {
			s := unescaped_val.replace_each(['\x01', '$fmt.bs$fmt.bs', "'", "$fmt.bs'"])
			f.write("'$s'")
		}
	}
}

pub fn (mut f Fmt) string_inter_literal(node ast.StringInterLiteral) {
	// TODO: this code is very similar to ast.Expr.str()
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
	f.write(quote)
	for i, val in node.vals {
		f.write(val)
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

pub fn (mut f Fmt) type_expr(node ast.TypeNode) {
	f.write(f.table.type_to_str(node.typ))
}

pub fn (mut f Fmt) type_of(node ast.TypeOf) {
	f.write('typeof(')
	f.expr(node.expr)
	f.write(')')
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

pub fn (mut f Fmt) prefix_expr_cast_expr(node ast.Expr) {
	mut is_pe_amp_ce := false
	if node is ast.PrefixExpr {
		if node.right is ast.CastExpr && node.op == .amp {
			mut ce := node.right as ast.CastExpr
			ce.typname = f.table.get_type_symbol(ce.typ).name
			is_pe_amp_ce = true
			f.expr(ce)
		}
	} else if node is ast.CastExpr {
		last := f.out.cut_last(1)
		if last != '&' {
			f.out.write_string(last)
		}
	}
	if !is_pe_amp_ce {
		f.expr(node)
	}
}

fn (mut f Fmt) trace(fbase string, message string) {
	if f.file.path_base == fbase {
		println('> f.trace | ${fbase:-10s} | $message')
	}
}
