module js

import strings
import v.ast
import v.table
import v.pref
import v.util
import v.depgraph

const (
	// https://ecma-international.org/ecma-262/#sec-reserved-words
	js_reserved = ['await', 'break', 'case', 'catch', 'class', 'const', 'continue', 'debugger',
		'default', 'delete', 'do', 'else', 'enum', 'export', 'extends', 'finally', 'for', 'function', 'if',
		'implements', 'import', 'in', 'instanceof', 'interface', 'let', 'new', 'package', 'private', 'protected',
		'public', 'return', 'static', 'super', 'switch', 'this', 'throw', 'try', 'typeof', 'var', 'void',
		'while', 'with', 'yield']
	tabs        = ['', '\t', '\t\t', '\t\t\t', '\t\t\t\t', '\t\t\t\t\t', '\t\t\t\t\t\t', '\t\t\t\t\t\t\t',
		'\t\t\t\t\t\t\t\t', '\t\t\t\t\t\t\t\t\t', '\t\t\t\t\t\t\t\t\t', '\t\t\t\t\t\t\t\t\t']
)

struct JsGen {
	table             &table.Table
	pref              &pref.Preferences
mut:
	definitions       strings.Builder
	out               strings.Builder
	namespaces        map[string]strings.Builder
	namespaces_pub    map[string][]string
	namespace_imports map[string]map[string]string
	namespace         string
	doc               &JsDoc
	enable_doc        bool
	file              ast.File
	tmp_count         int
	inside_ternary    bool
	inside_loop       bool
	inside_map_set    bool // map.set(key, value)
	is_test           bool
	indents           map[string]int // indentations mapped to namespaces
	stmt_start_pos    int
	defer_stmts       []ast.DeferStmt
	fn_decl           &ast.FnDecl // pointer to the FnDecl we are currently inside otherwise 0
	str_types         []string // types that need automatic str() generation
	method_fn_decls   map[string][]ast.FnDecl
	builtin_fns       []string // Functions defined in `builtin`
	empty_line        bool
}

pub fn gen(files []ast.File, table &table.Table, pref &pref.Preferences) string {
	mut g := &JsGen{
		out: strings.new_builder(100)
		definitions: strings.new_builder(100)
		table: table
		pref: pref
		fn_decl: 0
		empty_line: true
		doc: 0
		enable_doc: true
	}
	g.doc = new_jsdoc(g)
	// TODO: Add '[-no]-jsdoc' flag
	if pref.is_prod {
		g.enable_doc = false
	}
	g.init()
	mut graph := depgraph.new_dep_graph()
	// Get class methods
	for file in files {
		g.file = file
		g.enter_namespace(g.file.mod.name)
		g.is_test = g.file.path.ends_with('_test.v')
		g.find_class_methods(file.stmts)
		g.escape_namespace()
	}
	for file in files {
		g.file = file
		g.enter_namespace(g.file.mod.name)
		g.is_test = g.file.path.ends_with('_test.v')
		// store imports
		mut imports := []string{}
		for imp in g.file.imports {
			imports << imp.mod
		}
		graph.add(g.file.mod.name, imports)
		g.stmts(file.stmts)
		// store the current namespace
		g.escape_namespace()
	}
	// resolve imports
	deps_resolved := graph.resolve()
	nodes := deps_resolved.nodes
	mut out := g.hashes() + g.definitions.str()
	for node in nodes {
		name := g.js_name(node.name).replace('.', '_')
		if g.enable_doc {
			out += '/** @namespace $name */\n'
		}
		out += 'const $name = (function ('
		imports := g.namespace_imports[node.name]
		for i, key in imports.keys() {
			if i > 0 {
				out += ', '
			}
			out += imports[key]
		}
		out += ') {\n\t'
		// private scope
		out += g.namespaces[node.name].str().trim_space()
		// public scope
		out += '\n'
		if g.enable_doc {
			out += '\n\t/* module exports */'
		}
		out += '\n\treturn {'
		for i, pub_var in g.namespaces_pub[node.name] {
			out += '\n\t\t$pub_var'
			if i < g.namespaces_pub[node.name].len - 1 {
				out += ','
			}
		}
		if g.namespaces_pub[node.name].len > 0 {
			out += '\n\t'
		}
		out += '};'
		out += '\n})('
		for i, key in imports.keys() {
			if i > 0 {
				out += ', '
			}
			out += key.replace('.', '_')
		}
		out += ');\n\n'
	}
	if pref.is_shared {
		// Export, through CommonJS, the module of the entry file if `-shared` was passed
		export := nodes[nodes.len - 1].name
		out += 'if (typeof module === "object" && module.exports) module.exports = $export;'
	}
	return out
}

pub fn (mut g JsGen) enter_namespace(n string) {
	g.namespace = n
	if g.namespaces[g.namespace].len == 0 {
		// create a new namespace
		g.out = strings.new_builder(100)
		g.indents[g.namespace] = 0
	} else {
		g.out = g.namespaces[g.namespace]
	}
}

pub fn (mut g JsGen) escape_namespace() {
	g.namespaces[g.namespace] = g.out
	g.namespace = ''
}

pub fn (mut g JsGen) push_pub_var(s string) {
	mut arr := g.namespaces_pub[g.namespace]
	arr << g.js_name(s)
	g.namespaces_pub[g.namespace] = arr
}

pub fn (mut g JsGen) find_class_methods(stmts []ast.Stmt) {
	for stmt in stmts {
		match stmt {
			ast.FnDecl {
				if stmt.is_method {
					// Found struct method, store it to be generated along with the class.
					class_name := g.table.get_type_name(stmt.receiver.typ)
					// Workaround until `map[key] << val` works.
					mut arr := g.method_fn_decls[class_name]
					arr << stmt
					g.method_fn_decls[class_name] = arr
				}
			}
			else {}
		}
	}
}

pub fn (mut g JsGen) init() {
	g.definitions.writeln('// Generated by the V compiler\n')
	g.definitions.writeln('"use strict";')
	g.definitions.writeln('')
}

pub fn (g JsGen) hashes() string {
	mut res := '// V_COMMIT_HASH $util.vhash()\n'
	res += '// V_CURRENT_COMMIT_HASH ${util.githash(g.pref.building_v)}\n'
	return res
}

// V type to JS type
pub fn (mut g JsGen) typ(t table.Type) string {
	sym := g.table.get_type_symbol(t)
	mut styp := ''
	match sym.kind {
		.placeholder {
			// This should never happen: means checker bug
			styp = 'any'
		}
		.void {
			styp = 'void'
		}
		.voidptr {
			styp = 'any'
		}
		.byteptr, .charptr {
			styp = 'string'
		}
		.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .f32, .f64, .any_int, .any_float, .size_t {
			// TODO: Should u64 and i64 use BigInt rather than number?
			styp = 'number'
		}
		.bool {
			styp = 'boolean'
		}
		.none_ {
			styp = 'undefined'
		}
		.string, .ustring, .char {
			styp = 'string'
		}
		// 'array_array_int' => 'number[][]'
		.array {
			info := sym.info as table.Array
			styp = g.typ(info.elem_type) + '[]'
		}
		.array_fixed {
			info := sym.info as table.ArrayFixed
			styp = g.typ(info.elem_type) + '[]'
		}
		.chan {
			styp = 'chan'
		}
		// 'map[string]int' => 'Map<string, number>'
		.map {
			info := sym.info as table.Map
			key := g.typ(info.key_type)
			val := g.typ(info.value_type)
			styp = 'Map<$key, $val>'
		}
		.any {
			styp = 'any'
		}
		// ns.Foo => alias["Foo"]["prototype"]
		.struct_ {
			styp = g.struct_typ(sym.name)
		}
		.generic_struct_inst {}
		// 'multi_return_int_int' => '[number, number]'
		.multi_return {
			info := sym.info as table.MultiReturn
			types := info.types.map(g.typ(it))
			joined := types.join(', ')
			styp = '[$joined]'
		}
		.sum_type {
			// TODO: Implement sumtypes
			styp = 'union_sym_type'
		}
		.alias {
			// TODO: Implement aliases
			styp = 'alias'
		}
		.enum_ {
			// NB: We could declare them as TypeScript enums but TS doesn't like
			// our namespacing so these break if declared in a different module.
			// Until this is fixed, We need to use the type of an enum's members
			// rather than the enum itself, and this can only be 'number' for now
			styp = 'number'
		}
		// 'anon_fn_7_7_1' => '(a number, b number) => void'
		.function {
			info := sym.info as table.FnType
			styp = g.fn_typ(info.func.params, info.func.return_type)
		}
		.interface_ {
			styp = g.js_name(sym.name)
		}
		.rune {
			styp = 'any'
		}
		.aggregate {
			panic('TODO: unhandled aggregate in JS')
		}
	}
	/*
	else {
			println('jsgen.typ: Unhandled type $t')
			styp = sym.name
		}
	*/
	if styp.starts_with('JS.') {
		return styp[3..]
	}
	return styp
}

fn (mut g JsGen) fn_typ(args []table.Param, return_type table.Type) string {
	mut res := '('
	for i, arg in args {
		res += '$arg.name: ${g.typ(arg.typ)}'
		if i < args.len - 1 {
			res += ', '
		}
	}
	return res + ') => ' + g.typ(return_type)
}

fn (mut g JsGen) struct_typ(s string) string {
	ns := get_ns(s)
	mut name := if ns == g.namespace { s.split('.').last() } else { g.get_alias(s) }
	mut styp := ''
	for i, v in name.split('.') {
		if i == 0 {
			styp = v
		} else {
			styp += '["$v"]'
		}
	}
	if ns in ['', g.namespace] {
		return styp
	}
	return styp + '["prototype"]'
}

fn (mut g JsGen) to_js_typ_val(t table.Type) string {
	sym := g.table.get_type_symbol(t)
	mut styp := ''
	match sym.kind {
		.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .f32, .f64, .any_int, .any_float, .size_t {
			styp = '0'
		}
		.bool {
			styp = 'false'
		}
		.string {
			styp = '""'
		}
		.map {
			styp = 'new Map()'
		}
		.array {
			styp = '[]'
		}
		.struct_ {
			styp = 'new ${g.js_name(sym.name)}({})'
		}
		else {
			// TODO
			styp = 'undefined'
		}
	}
	return styp
}

pub fn (mut g JsGen) gen_indent() {
	if g.indents[g.namespace] > 0 && g.empty_line {
		g.out.write(tabs[g.indents[g.namespace]])
	}
	g.empty_line = false
}

pub fn (mut g JsGen) inc_indent() {
	g.indents[g.namespace]++
}

pub fn (mut g JsGen) dec_indent() {
	g.indents[g.namespace]--
}

pub fn (mut g JsGen) write(s string) {
	g.gen_indent()
	g.out.write(s)
}

pub fn (mut g JsGen) writeln(s string) {
	g.gen_indent()
	g.out.writeln(s)
	g.empty_line = true
}

pub fn (mut g JsGen) new_tmp_var() string {
	g.tmp_count++
	return '_tmp$g.tmp_count'
}

// 'mod1.mod2.fn' => 'mod1.mod2'
// 'fn' => ''
[inline]
fn get_ns(s string) string {
	idx := s.last_index('.') or {
		return ''
	}
	return s.substr(0, idx)
}

fn (mut g JsGen) get_alias(name string) string {
	ns := get_ns(name)
	if ns == '' {
		return name
	}
	imports := g.namespace_imports[g.namespace]
	alias := imports[ns]
	if alias == '' {
		return name
	}
	return alias + '.' + name.split('.').last()
}

fn (mut g JsGen) js_name(name_ string) string {
	ns := get_ns(name_)
	mut name := if ns == g.namespace { name_.split('.').last() } else { g.get_alias(name_) }
	mut parts := name.split('.')
	for i, p in parts {
		if p in js_reserved {
			parts[i] = 'v_$p'
		}
	}
	return parts.join('.')
}

fn (mut g JsGen) stmts(stmts []ast.Stmt) {
	g.inc_indent()
	for stmt in stmts {
		g.stmt(stmt)
	}
	g.dec_indent()
}

fn (mut g JsGen) stmt(node ast.Stmt) {
	g.stmt_start_pos = g.out.len
	match node {
		ast.AssertStmt {
			g.gen_assert_stmt(node)
		}
		ast.AssignStmt {
			g.gen_assign_stmt(node)
		}
		ast.Block {
			g.gen_block(node)
			g.writeln('')
		}
		ast.BranchStmt {
			g.gen_branch_stmt(node)
		}
		ast.CompFor {}
		ast.ConstDecl {
			g.gen_const_decl(node)
		}
		ast.DeferStmt {
			g.defer_stmts << node
		}
		ast.EnumDecl {
			g.gen_enum_decl(node)
			g.writeln('')
		}
		ast.ExprStmt {
			g.gen_expr_stmt(node)
		}
		ast.FnDecl {
			g.fn_decl = &node
			g.gen_fn_decl(node)
		}
		ast.ForCStmt {
			g.gen_for_c_stmt(node)
			g.writeln('')
		}
		ast.ForInStmt {
			g.gen_for_in_stmt(node)
			g.writeln('')
		}
		ast.ForStmt {
			g.gen_for_stmt(node)
			g.writeln('')
		}
		ast.GlobalDecl {
			// TODO
		}
		ast.GoStmt {
			g.gen_go_stmt(node)
			g.writeln('')
		}
		ast.GotoLabel {
			g.writeln('${g.js_name(node.name)}:')
		}
		ast.GotoStmt {
			// skip: JS has no goto
		}
		ast.HashStmt {
			g.gen_hash_stmt(node)
		}
		ast.Import {
			g.gen_import_stmt(node)
		}
		ast.InterfaceDecl {
			g.gen_interface_decl(node)
		}
		ast.Module {
			// skip: namespacing implemented externally
		}
		ast.Return {
			if g.defer_stmts.len > 0 {
				g.gen_defer_stmts()
			}
			g.gen_return_stmt(node)
		}
		ast.SqlStmt {}
		ast.StructDecl {
			g.gen_struct_decl(node)
		}
		ast.TypeDecl {
			// skip JS has no typedecl
		}
	}
}

fn (mut g JsGen) expr(node ast.Expr) {
	match node {
		ast.CTempVar {
			g.write('/* ast.CTempVar: node.name */')
		}
		ast.AnonFn {
			g.gen_fn_decl(node.decl)
		}
		ast.ArrayInit {
			g.gen_array_init_expr(node)
		}
		ast.AsCast {
			// skip: JS has no types, so no need to cast
			// TODO: Is jsdoc needed here for TS support?
		}
		ast.Assoc {
			// TODO
		}
		ast.BoolLiteral {
			if node.val == true {
				g.write('true')
			} else {
				g.write('false')
			}
		}
		ast.CallExpr {
			g.gen_call_expr(node)
		}
		ast.ChanInit {
			// TODO
		}
		ast.CastExpr {
			// JS has no types, so no need to cast
			// Just write the expression inside
			g.expr(node.expr)
		}
		ast.CharLiteral {
			g.write("'$node.val'")
		}
		ast.Comment {}
		ast.ConcatExpr {
			// TODO
		}
		ast.EnumVal {
			sym := g.table.get_type_symbol(node.typ)
			styp := g.js_name(sym.name)
			g.write('${styp}.$node.val')
		}
		ast.FloatLiteral {
			g.write(node.val)
		}
		ast.Ident {
			g.gen_ident(node)
		}
		ast.IfExpr {
			g.gen_if_expr(node)
		}
		ast.IfGuardExpr {
			// TODO no optionals yet
		}
		ast.IndexExpr {
			g.gen_index_expr(node)
		}
		ast.InfixExpr {
			g.gen_infix_expr(node)
		}
		ast.IntegerLiteral {
			g.write(node.val)
		}
		ast.LockExpr {
			g.gen_lock_expr(node)
		}
		ast.MapInit {
			g.gen_map_init_expr(node)
		}
		ast.MatchExpr {
			// TODO
		}
		ast.None {
			// TODO
		}
		ast.OrExpr {
			// TODO
		}
		ast.ParExpr {
			g.write('(')
			g.expr(node.expr)
			g.write(')')
		}
		ast.PostfixExpr {
			g.expr(node.expr)
			g.write(node.op.str())
		}
		ast.PrefixExpr {
			if node.op in [.amp, .mul] {
				// C pointers/references: ignore them
			} else {
				g.write(node.op.str())
			}
			g.expr(node.right)
		}
		ast.RangeExpr {
			// Only used in IndexExpr, requires index type info
		}
		ast.SelectExpr {
			// TODO: to be implemented
		}
		ast.SelectorExpr {
			g.gen_selector_expr(node)
		}
		ast.SizeOf {
			// TODO
		}
		ast.SqlExpr {
			// TODO
		}
		ast.StringInterLiteral {
			g.gen_string_inter_literal(node)
		}
		ast.StringLiteral {
			text := node.val.replace('`', '\\`')
			g.write('`$text`')
		}
		ast.StructInit {
			// `user := User{name: 'Bob'}`
			g.gen_struct_init(node)
		}
		ast.Type {
			// skip: JS has no types
			// TODO maybe?
		}
		ast.Likely {
			g.write('(')
			g.expr(node.expr)
			g.write(')')
		}
		ast.TypeOf {
			g.gen_typeof_expr(node)
			// TODO: Should this print the V type or the JS type?
		}
		ast.AtExpr {
			g.write('"$node.val"')
		}
		ast.ComptimeCall {
			// TODO
		}
		ast.UnsafeExpr {
			g.expr(node.expr)
		}
	}
}

// TODO
fn (mut g JsGen) gen_assert_stmt(a ast.AssertStmt) {
	g.writeln('// assert')
	g.write('if( ')
	g.expr(a.expr)
	g.write(' ) {')
	s_assertion := a.expr.str().replace('"', "'")
	mut mod_path := g.file.path.replace('\\', '\\\\')
	if g.is_test {
		g.writeln('	g_test_oks++;')
		g.writeln('	cb_assertion_ok("$mod_path", ${a.pos.line_nr + 1}, "assert $s_assertion", "${g.fn_decl.name}()" );')
		g.writeln('} else {')
		g.writeln('	g_test_fails++;')
		g.writeln('	cb_assertion_failed("$mod_path", ${a.pos.line_nr + 1}, "assert $s_assertion", "${g.fn_decl.name}()" );')
		g.writeln('	exit(1);')
		g.writeln('}')
		return
	}
	g.writeln('} else {')
	g.inc_indent()
	g.writeln('builtin.eprintln("$mod_path:${a.pos.line_nr + 1}: FAIL: fn ${g.fn_decl.name}(): assert $s_assertion");')
	g.writeln('builtin.exit(1);')
	g.dec_indent()
	g.writeln('}')
}

fn (mut g JsGen) gen_assign_stmt(stmt ast.AssignStmt) {
	if stmt.left.len > stmt.right.len {
		// multi return
		g.write('const [')
		for i, left in stmt.left {
			if !left.is_blank_ident() {
				g.expr(left)
			}
			if i < stmt.left.len - 1 {
				g.write(', ')
			}
		}
		g.write('] = ')
		g.expr(stmt.right[0])
		g.writeln(';')
	} else {
		// `a := 1` | `a,b := 1,2`
		for i, left in stmt.left {
			mut op := stmt.op
			if stmt.op == .decl_assign {
				op = .assign
			}
			val := stmt.right[i]
			mut is_mut := false
			if left is ast.Ident {
				is_mut = left.is_mut
				if left.kind == .blank_ident || left.name in ['', '_'] {
					tmp_var := g.new_tmp_var()
					// TODO: Can the tmp_var declaration be omitted?
					g.write('const $tmp_var = ')
					g.expr(val)
					g.writeln(';')
					continue
				}
			}
			mut styp := g.typ(stmt.left_types[i])
			if !g.inside_loop && styp.len > 0 {
				g.doc.gen_typ(styp)
			}
			if stmt.op == .decl_assign {
				if g.inside_loop || is_mut {
					g.write('let ')
				} else {
					g.write('const ')
				}
			}
			g.expr(left)
			if g.inside_map_set && op == .assign {
				g.inside_map_set = false
				g.write(', ')
				g.expr(val)
				g.write(')')
			} else {
				g.write(' $op ')
				g.expr(val)
			}
			if g.inside_loop {
				g.write('; ')
			} else {
				g.writeln(';')
			}
		}
	}
}

fn (mut g JsGen) gen_attrs(attrs []table.Attr) {
	for attr in attrs {
		g.writeln('/* [$attr.name] */')
	}
}

fn (mut g JsGen) gen_block(it ast.Block) {
	g.writeln('{')
	g.stmts(it.stmts)
	g.writeln('}')
}

fn (mut g JsGen) gen_branch_stmt(it ast.BranchStmt) {
	// continue or break
	g.write(it.kind.str())
	g.writeln(';')
}

fn (mut g JsGen) gen_const_decl(it ast.ConstDecl) {
	for field in it.fields {
		g.doc.gen_const(g.typ(field.typ))
		if field.is_pub {
			g.push_pub_var(field.name)
		}
		g.write('const ${g.js_name(field.name)} = ')
		g.expr(field.expr)
		g.writeln(';')
	}
	g.writeln('')
}

fn (mut g JsGen) gen_defer_stmts() {
	g.writeln('(function defer() {')
	for defer_stmt in g.defer_stmts {
		g.stmts(defer_stmt.stmts)
	}
	g.defer_stmts = []
	g.writeln('})();')
}

fn (mut g JsGen) gen_enum_decl(it ast.EnumDecl) {
	g.doc.gen_enum()
	g.writeln('const ${g.js_name(it.name)} = {')
	g.inc_indent()
	mut i := 0
	for field in it.fields {
		g.write('$field.name: ')
		if field.has_expr && field.expr is ast.IntegerLiteral {
			e := field.expr as ast.IntegerLiteral
			i = e.val.int()
		}
		g.writeln('${i++},')
	}
	g.dec_indent()
	g.writeln('};')
	if it.is_pub {
		g.push_pub_var(it.name)
	}
}

fn (mut g JsGen) gen_expr_stmt(it ast.ExprStmt) {
	g.expr(it.expr)
	if !it.is_expr && it.expr !is ast.IfExpr && !g.inside_ternary {
		g.writeln(';')
	}
}

fn (mut g JsGen) gen_fn_decl(it ast.FnDecl) {
	if it.no_body || it.is_method {
		// Struct methods are handled by class generation code.
		return
	}
	if g.namespace == 'builtin' {
		g.builtin_fns << it.name
	}
	g.gen_method_decl(it)
}

fn fn_has_go(it ast.FnDecl) bool {
	mut has_go := false
	for stmt in it.stmts {
		if stmt is ast.GoStmt {
			has_go = true
		}
	}
	return has_go
}

fn (mut g JsGen) gen_method_decl(it ast.FnDecl) {
	g.fn_decl = &it
	has_go := fn_has_go(it)
	is_main := it.name == 'main.main'
	g.gen_attrs(it.attrs)
	if is_main {
		// there is no concept of main in JS but we do have iife
		g.writeln('/* program entry point */')
		g.write('(')
		if has_go {
			g.write('async ')
		}
		g.write('function(')
	} else if it.is_anon {
		g.write('function (')
	} else {
		mut name := g.js_name(it.name)
		c := name[0]
		if c in [`+`, `-`, `*`, `/`] {
			name = util.replace_op(name)
		}
		// type_name := g.typ(it.return_type)
		// generate jsdoc for the function
		g.doc.gen_fn(it)
		if has_go {
			g.write('async ')
		}
		if !it.is_method {
			g.write('function ')
		}
		g.write('${name}(')
		if it.is_pub && !it.is_method {
			g.push_pub_var(name)
		}
	}
	mut args := it.params
	if it.is_method {
		args = args[1..]
	}
	g.fn_args(args, it.is_variadic)
	g.writeln(') {')
	if it.is_method {
		g.inc_indent()
		g.writeln('const ${it.params[0].name} = this;')
		g.dec_indent()
	}
	g.stmts(it.stmts)
	g.write('}')
	if is_main {
		g.write(')();')
	}
	if !it.is_anon && !it.is_method {
		g.writeln('\n')
	}
	g.fn_decl = voidptr(0)
}

fn (mut g JsGen) fn_args(args []table.Param, is_variadic bool) {
	for i, arg in args {
		name := g.js_name(arg.name)
		is_varg := i == args.len - 1 && is_variadic
		if is_varg {
			g.write('...$name')
		} else {
			g.write(name)
		}
		// if its not the last argument
		if i < args.len - 1 {
			g.write(', ')
		}
	}
}

fn (mut g JsGen) gen_for_c_stmt(it ast.ForCStmt) {
	g.inside_loop = true
	g.write('for (')
	if it.has_init {
		g.stmt(it.init)
	} else {
		g.write('; ')
	}
	if it.has_cond {
		g.expr(it.cond)
	}
	g.write('; ')
	if it.has_inc {
		g.stmt(it.inc)
	}
	g.writeln(') {')
	g.stmts(it.stmts)
	g.writeln('}')
	g.inside_loop = false
}

fn (mut g JsGen) gen_for_in_stmt(it ast.ForInStmt) {
	if it.is_range {
		// `for x in 1..10 {`
		mut i := it.val_var
		if i in ['', '_'] {
			i = g.new_tmp_var()
		}
		g.inside_loop = true
		g.write('for (let $i = ')
		g.expr(it.cond)
		g.write('; $i < ')
		g.expr(it.high)
		g.writeln('; ++$i) {')
		g.inside_loop = false
		g.stmts(it.stmts)
		g.writeln('}')
	} else if it.kind in [.array, .string] || it.cond_type.has_flag(.variadic) {
		// `for num in nums {`
		i := if it.key_var in ['', '_'] { g.new_tmp_var() } else { it.key_var }
		val := if it.val_var in ['', '_'] { '' } else { it.val_var }
		// styp := g.typ(it.val_type)
		g.inside_loop = true
		g.write('for (let $i = 0; $i < ')
		g.expr(it.cond)
		g.writeln('.length; ++$i) {')
		g.inside_loop = false
		if val !in ['', '_'] {
			g.write('\tconst $val = ')
			g.expr(it.cond)
			g.writeln('[$i];')
		}
		g.stmts(it.stmts)
		g.writeln('}')
	} else if it.kind == .map {
		// `for key, val in map[string]int {`
		// key_styp := g.typ(it.key_type)
		// val_styp := g.typ(it.val_type)
		key := if it.key_var in ['', '_'] { '' } else { it.key_var }
		val := if it.val_var in ['', '_'] { '' } else { it.val_var }
		g.write('for (let [$key, $val] of ')
		g.expr(it.cond)
		g.writeln(') {')
		g.stmts(it.stmts)
		g.writeln('}')
	}
}

fn (mut g JsGen) gen_for_stmt(it ast.ForStmt) {
	g.write('while (')
	if it.is_inf {
		g.write('true')
	} else {
		g.expr(it.cond)
	}
	g.writeln(') {')
	g.stmts(it.stmts)
	g.writeln('}')
}

fn (mut g JsGen) gen_go_stmt(node ast.GoStmt) {
	// x := node.call_expr as ast.CallEpxr // TODO
	match node.call_expr {
		ast.CallExpr {
			mut name := node.call_expr.name
			if node.call_expr.is_method {
				receiver_sym := g.table.get_type_symbol(node.call_expr.receiver_type)
				name = receiver_sym.name + '.' + name
			}
			// todo: please add a name feild without the mod name for ast.CallExpr
			if name.starts_with('${node.call_expr.mod}.') {
				name = name[node.call_expr.mod.len + 1..]
			}
			g.writeln('await new Promise(function(resolve){')
			g.inc_indent()
			g.write('${name}(')
			for i, arg in node.call_expr.args {
				g.expr(arg.expr)
				if i < node.call_expr.args.len - 1 {
					g.write(', ')
				}
			}
			g.writeln(');')
			g.writeln('resolve();')
			g.dec_indent()
			g.writeln('});')
		}
		else {}
	}
}

fn (mut g JsGen) gen_import_stmt(it ast.Import) {
	mut imports := g.namespace_imports[g.namespace]
	imports[it.mod] = it.alias
	g.namespace_imports[g.namespace] = imports
}

fn (mut g JsGen) gen_interface_decl(it ast.InterfaceDecl) {
	// JS is dynamically typed, so we don't need any codegen at all
	// We just need the JSDoc so TypeScript type checking works
	g.doc.gen_interface(it)
	// This is a hack to make the interface's type accessible outside its namespace
	// TODO: interfaces are always `pub`?
	name := g.js_name(it.name)
	g.push_pub_var('/** @type $name */\n\t\t$name: undefined')
}

fn (mut g JsGen) gen_return_stmt(it ast.Return) {
	if it.exprs.len == 0 {
		// Returns nothing
		g.writeln('return;')
		return
	}
	g.write('return ')
	if it.exprs.len == 1 {
		g.expr(it.exprs[0])
	} else { // Multi return
		g.gen_array_init_values(it.exprs)
	}
	g.writeln(';')
}

fn (mut g JsGen) gen_hash_stmt(it ast.HashStmt) {
	g.writeln(it.val)
}

fn (mut g JsGen) gen_struct_decl(node ast.StructDecl) {
	if node.name.starts_with('JS.') {
		return
	}
	g.gen_attrs(node.attrs)
	g.doc.gen_fac_fn(node.fields)
	g.write('function ${g.js_name(node.name)}({ ')
	for i, field in node.fields {
		g.write('$field.name = ')
		if field.has_default_expr {
			g.expr(field.default_expr)
		} else {
			g.write('${g.to_js_typ_val(field.typ)}')
		}
		if i < node.fields.len - 1 {
			g.write(', ')
		}
	}
	g.writeln(' }) {')
	g.inc_indent()
	for field in node.fields {
		g.writeln('this.$field.name = $field.name')
	}
	g.dec_indent()
	g.writeln('};')
	g.writeln('${g.js_name(node.name)}.prototype = {')
	g.inc_indent()
	fns := g.method_fn_decls[node.name]
	for i, field in node.fields {
		typ := g.typ(field.typ)
		g.doc.gen_typ(typ)
		g.write('$field.name: ${g.to_js_typ_val(field.typ)}')
		if i < node.fields.len - 1 || fns.len > 0 {
			g.writeln(',')
		} else {
			g.writeln('')
		}
	}
	for i, cfn in fns {
		g.gen_method_decl(cfn)
		if i < fns.len - 1 {
			g.writeln(',')
		} else {
			g.writeln('')
		}
	}
	g.dec_indent()
	g.writeln('};\n')
	if node.is_pub {
		g.push_pub_var(node.name)
	}
}

fn (mut g JsGen) gen_array_init_expr(it ast.ArrayInit) {
	// NB: Fixed arrays and regular arrays are handled the same, since fixed arrays:
	// 1)  Are only available for number types
	// 2)  Give the code unnecessary complexity
	// 3)  Have several limitations like missing most `Array.prototype` methods
	// 4)  Modern engines can optimize regular arrays into typed arrays anyways,
	// offering similar performance
	if it.has_len {
		t1 := g.new_tmp_var()
		t2 := g.new_tmp_var()
		g.writeln('(function() {')
		g.inc_indent()
		g.writeln('const $t1 = [];')
		g.write('for (let $t2 = 0; $t2 < ')
		g.expr(it.len_expr)
		g.writeln('; $t2++) {')
		g.inc_indent()
		g.write('${t1}.push(')
		if it.has_default {
			g.expr(it.default_expr)
		} else {
			// Fill the array with the default values for its type
			t := g.to_js_typ_val(it.elem_type)
			g.write(t)
		}
		g.writeln(');')
		g.dec_indent()
		g.writeln('};')
		g.writeln('return $t1;')
		g.dec_indent()
		g.write('})()')
	} else {
		g.gen_array_init_values(it.exprs)
	}
}

fn (mut g JsGen) gen_array_init_values(exprs []ast.Expr) {
	g.write('[')
	for i, expr in exprs {
		g.expr(expr)
		if i < exprs.len - 1 {
			g.write(', ')
		}
	}
	g.write(']')
}

fn (mut g JsGen) gen_call_expr(it ast.CallExpr) {
	mut name := ''
	if it.name.starts_with('JS.') {
		name = it.name[3..]
	} else {
		name = g.js_name(it.name)
	}
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return builtin.unwrap(')
	}
	g.expr(it.left)
	if it.is_method { // foo.bar.baz()
		sym := g.table.get_type_symbol(it.receiver_type)
		g.write('.')
		if sym.kind == .array && it.name in ['map', 'filter'] {
			// Prevent 'it' from getting shadowed inside the match
			node := it
			g.write(it.name)
			g.write('(')
			expr := node.args[0].expr
			match expr {
				ast.AnonFn {
					g.gen_fn_decl(expr.decl)
					g.write(')')
					return
				}
				ast.Ident {
					if expr.kind == .function {
						g.write(g.js_name(expr.name))
						g.write(')')
						return
					} else if expr.kind == .variable {
						v_sym := g.table.get_type_symbol(expr.var_info().typ)
						if v_sym.kind == .function {
							g.write(g.js_name(expr.name))
							g.write(')')
							return
						}
					}
				}
				else {}
			}
			g.write('it => ')
			g.expr(node.args[0].expr)
			g.write(')')
			return
		}
	} else {
		if name in g.builtin_fns {
			g.write('builtin.')
		}
	}
	g.write('${g.js_name(name)}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end method call
	g.write(')')
	if call_return_is_optional {
		// end unwrap
		g.writeln(')')
		g.dec_indent()
		// begin catch block
		g.writeln('} catch(err) {')
		g.inc_indent()
		// gen or block contents
		match it.or_block.kind {
			.block {
				if it.or_block.stmts.len > 1 {
					g.stmts(it.or_block.stmts[..it.or_block.stmts.len - 1])
				}
				g.write('return ')
				g.stmt(it.or_block.stmts.last())
			}
			.propagate {
				panicstr := '`optional not set (\${err})`'
				if g.file.mod.name == 'main' && g.fn_decl.name == 'main.main' {
					g.writeln('return builtin.panic($panicstr)')
				} else {
					g.writeln('builtin.js_throw(err)')
				}
			}
			else {}
		}
		// end catch
		g.dec_indent()
		g.writeln('}')
		// end anon fn
		g.dec_indent()
		g.write('})()')
	}
}

fn (mut g JsGen) gen_ident(node ast.Ident) {
	mut name := g.js_name(node.name)
	if node.kind == .blank_ident || name in ['', '_'] {
		name = g.new_tmp_var()
	}
	// TODO `is`
	// TODO handle optionals
	g.write(name)
}

fn (mut g JsGen) gen_lock_expr(node ast.LockExpr) {
	// TODO: implement this
}

fn (mut g JsGen) gen_if_expr(node ast.IfExpr) {
	type_sym := g.table.get_type_symbol(node.typ)
	// one line ?:
	if node.is_expr && node.branches.len >= 2 && node.has_else && type_sym.kind != .void {
		// `x := if a > b {  } else if { } else { }`
		g.write('(')
		g.inside_ternary = true
		for i, branch in node.branches {
			if i > 0 {
				g.write(' : ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				g.expr(branch.cond)
				g.write(' ? ')
			}
			g.stmts(branch.stmts)
		}
		g.inside_ternary = false
		g.write(')')
	} else {
		// mut is_guard = false
		for i, branch in node.branches {
			if i == 0 {
				match branch.cond {
					ast.IfGuardExpr {
						// TODO optionals
					}
					else {
						g.write('if (')
						if '$branch.cond' == 'js' {
							g.write('true')
						} else {
							g.expr(branch.cond)
						}
						g.writeln(') {')
					}
				}
			} else if i < node.branches.len - 1 || !node.has_else {
				g.write('} else if (')
				g.expr(branch.cond)
				g.writeln(') {')
			} else if i == node.branches.len - 1 && node.has_else {
				/*
				if is_guard {
					//g.writeln('} if (!$guard_ok) { /* else */')
				} else {
				*/
				g.writeln('} else {')
				// }
			}
			g.stmts(branch.stmts)
		}
		/*
		if is_guard {
			g.write('}')
		}
		*/
		g.writeln('}')
		g.writeln('')
	}
}

fn (mut g JsGen) gen_index_expr(expr ast.IndexExpr) {
	left_typ := g.table.get_type_symbol(expr.left_type)
	// TODO: Handle splice setting if it's implemented
	if expr.index is ast.RangeExpr {
		g.expr(expr.left)
		g.write('.slice(')
		if expr.index.has_low {
			g.expr(expr.index.low)
		} else {
			g.write('0')
		}
		g.write(', ')
		if expr.index.has_high {
			g.expr(expr.index.high)
		} else {
			g.expr(expr.left)
			g.write('.length')
		}
		g.write(')')
	} else if left_typ.kind == .map {
		g.expr(expr.left)
		if expr.is_setter {
			g.inside_map_set = true
			g.write('.set(')
		} else {
			g.write('.get(')
		}
		g.expr(expr.index)
		if !expr.is_setter {
			g.write(')')
		}
	} else if left_typ.kind == .string {
		if expr.is_setter {
			// TODO: What's the best way to do this?
			// 'string'[3] = `o`
		} else {
			g.expr(expr.left)
			g.write('.charCodeAt(')
			g.expr(expr.index)
			g.write(')')
		}
	} else {
		// TODO Does this cover all cases?
		g.expr(expr.left)
		g.write('[')
		g.expr(expr.index)
		g.write(']')
	}
}

fn (mut g JsGen) gen_infix_expr(it ast.InfixExpr) {
	l_sym := g.table.get_type_symbol(it.left_type)
	r_sym := g.table.get_type_symbol(it.right_type)
	if l_sym.kind == .array && it.op == .left_shift { // arr << 1
		g.expr(it.left)
		g.write('.push(')
		if r_sym.kind == .array {
			g.write('...')
		}
		// arr << [1, 2]
		g.expr(it.right)
		g.write(')')
	} else if r_sym.kind in [.array, .map, .string] && it.op in [.key_in, .not_in] {
		if it.op == .not_in {
			g.write('!(')
		}
		g.expr(it.right)
		g.write(if r_sym.kind == .map {
			'.has('
		} else {
			'.includes('
		})
		g.expr(it.left)
		g.write(')')
		if it.op == .not_in {
			g.write(')')
		}
	} else if it.op in [.key_is, .not_is] { // foo is Foo
		if it.op == .not_is {
			g.write('!(')
		}
		g.expr(it.left)
		g.write(' instanceof ')
		g.write(g.typ(it.right_type))
		if it.op == .not_is {
			g.write(')')
		}
	} else {
		both_are_int := int(it.left_type) in table.integer_type_idxs &&
			int(it.right_type) in table.integer_type_idxs
		if it.op == .div && both_are_int {
			g.write('parseInt(')
		}
		g.expr(it.left)
		// in js == is non-strict & === is strict, always do strict
		if it.op == .eq {
			g.write(' === ')
		} else if it.op == .ne {
			g.write(' !== ')
		} else {
			g.write(' $it.op ')
		}
		g.expr(it.right)
		// Int division: 2.5 -> 2 by prepending |0
		if it.op == .div && both_are_int {
			g.write(',10)')
		}
	}
}

fn (mut g JsGen) gen_map_init_expr(it ast.MapInit) {
	// key_typ_sym := g.table.get_type_symbol(it.key_type)
	// value_typ_sym := g.table.get_type_symbol(it.value_type)
	// key_typ_str := util.no_dots(key_typ_sym.name)
	// value_typ_str := util.no_dots(value_typ_sym.name)
	if it.vals.len > 0 {
		g.writeln('new Map([')
		g.inc_indent()
		for i, key in it.keys {
			val := it.vals[i]
			g.write('[')
			g.expr(key)
			g.write(', ')
			g.expr(val)
			g.write(']')
			if i < it.keys.len - 1 {
				g.write(',')
			}
			g.writeln('')
		}
		g.dec_indent()
		g.write('])')
	} else {
		g.write('new Map()')
	}
}

fn (mut g JsGen) gen_selector_expr(it ast.SelectorExpr) {
	g.expr(it.expr)
	g.write('.$it.field_name')
}

fn (mut g JsGen) gen_string_inter_literal(it ast.StringInterLiteral) {
	g.write('`')
	for i, val in it.vals {
		escaped_val := val.replace('`', '\\`')
		g.write(escaped_val)
		if i >= it.exprs.len {
			continue
		}
		expr := it.exprs[i]
		fmt := it.fmts[i]
		fwidth := it.fwidths[i]
		precision := it.precisions[i]
		g.write('\${')
		if fmt != `_` || fwidth != 0 || precision != 987698 {
			// TODO: Handle formatting
			g.expr(expr)
		} else {
			sym := g.table.get_type_symbol(it.expr_types[i])
			g.expr(expr)
			if sym.kind == .struct_ && sym.has_method('str') {
				g.write('.str()')
			}
		}
		g.write('}')
	}
	g.write('`')
}

fn (mut g JsGen) gen_struct_init(it ast.StructInit) {
	type_sym := g.table.get_type_symbol(it.typ)
	name := type_sym.name
	if it.fields.len == 0 {
		g.write('new ${g.js_name(name)}({})')
	} else {
		g.writeln('new ${g.js_name(name)}({')
		g.inc_indent()
		for i, field in it.fields {
			g.write('$field.name: ')
			g.expr(field.expr)
			if i < it.fields.len - 1 {
				g.write(',')
			}
			g.writeln('')
		}
		g.dec_indent()
		g.write('})')
	}
}

fn (mut g JsGen) gen_typeof_expr(it ast.TypeOf) {
	sym := g.table.get_type_symbol(it.expr_type)
	if sym.kind == .sum_type {
		// TODO: JS sumtypes not implemented yet
	} else if sym.kind == .array_fixed {
		fixed_info := sym.info as table.ArrayFixed
		typ_name := g.table.get_type_name(fixed_info.elem_type)
		g.write('"[$fixed_info.size]$typ_name"')
	} else if sym.kind == .function {
		info := sym.info as table.FnType
		fn_info := info.func
		mut repr := 'fn ('
		for i, arg in fn_info.params {
			if i > 0 {
				repr += ', '
			}
			repr += g.table.get_type_name(arg.typ)
		}
		repr += ')'
		if fn_info.return_type != table.void_type {
			repr += ' ${g.table.get_type_name(fn_info.return_type)}'
		}
		g.write('"$repr"')
	} else {
		g.write('"$sym.name"')
	}
}
