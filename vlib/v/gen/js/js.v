module js

import strings
import v.ast
import v.token
import v.pref
import v.util
import v.depgraph

const (
	// https://ecma-international.org/ecma-262/#sec-reserved-words
	js_reserved        = ['await', 'break', 'case', 'catch', 'class', 'const', 'continue', 'debugger',
		'default', 'delete', 'do', 'else', 'enum', 'export', 'extends', 'finally', 'for', 'function',
		'if', 'implements', 'import', 'in', 'instanceof', 'interface', 'let', 'new', 'package',
		'private', 'protected', 'public', 'return', 'static', 'super', 'switch', 'this', 'throw',
		'try', 'typeof', 'var', 'void', 'while', 'with', 'yield', 'Number', 'String', 'Boolean',
		'Array', 'Map']
	// used to generate type structs
	v_types            = ['i8', 'i16', 'int', 'i64', 'byte', 'u16', 'u32', 'u64', 'f32', 'f64',
		'int_literal', 'float_literal', 'size_t', 'bool', 'string', 'map', 'array']
	shallow_equatables = [ast.Kind.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .f32, .f64,
		.int_literal, .float_literal, .size_t, .bool, .string]
)

struct Namespace {
	name string
mut:
	out      strings.Builder = strings.new_builder(128)
	pub_vars []string
	imports  map[string]string
	indent   int
	methods  map[string][]ast.FnDecl
}

struct JsGen {
	table &ast.Table
	pref  &pref.Preferences
mut:
	definitions         strings.Builder
	ns                  &Namespace
	namespaces          map[string]&Namespace
	doc                 &JsDoc
	enable_doc          bool
	file                ast.File
	tmp_count           int
	inside_ternary      bool
	inside_loop         bool
	inside_map_set      bool // map.set(key, value)
	inside_builtin      bool
	generated_builtin   bool
	inside_def_typ_decl bool
	is_test             bool
	stmt_start_pos      int
	defer_stmts         []ast.DeferStmt
	fn_decl             &ast.FnDecl // pointer to the FnDecl we are currently inside otherwise 0
	str_types           []string    // types that need automatic str() generation
	method_fn_decls     map[string][]ast.FnDecl
	builtin_fns         []string // Functions defined in `builtin`
	empty_line          bool
	cast_stack          []ast.Type
	call_stack          []ast.CallExpr
}

pub fn gen(files []ast.File, table &ast.Table, pref &pref.Preferences) string {
	mut g := &JsGen{
		definitions: strings.new_builder(100)
		table: table
		pref: pref
		fn_decl: 0
		empty_line: true
		doc: 0
		ns: 0
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
		g.is_test = g.pref.is_test
		g.find_class_methods(file.stmts)
		g.escape_namespace()
	}
	for file in files {
		g.file = file
		g.enter_namespace(g.file.mod.name)
		g.is_test = g.pref.is_test
		// store imports
		mut imports := []string{}
		for imp in g.file.imports {
			imports << imp.mod
		}
		graph.add(g.file.mod.name, imports)
		// builtin types
		if g.file.mod.name == 'builtin' && !g.generated_builtin {
			g.gen_builtin_type_defs()
			g.generated_builtin = true
		}
		g.stmts(file.stmts)
		// store the current namespace
		g.escape_namespace()
	}
	// resolve imports
	deps_resolved := graph.resolve()
	nodes := deps_resolved.nodes
	mut out := g.hashes() + g.definitions.str()
	// equality check for js objects
	// TODO: Fix msvc bug that's preventing $embed_file('fast_deep_equal.js')
	// unsafe {
	//	mut eq_fn := $embed_file('fast_deep_equal.js')
	//	out += eq_fn.data().vstring()
	//}
	out += fast_deep_eq_fn
	for node in nodes {
		name := g.js_name(node.name).replace('.', '_')
		if g.enable_doc {
			out += '/** @namespace $name */\n'
		}
		out += 'const $name = (function ('
		mut namespace := g.namespaces[node.name]
		mut first := true
		for _, val in namespace.imports {
			if !first {
				out += ', '
			}
			first = false
			out += val
		}
		out += ') {\n\t'
		// private scope
		out += namespace.out.str().trim_space()
		// public scope
		out += '\n'
		if g.enable_doc {
			out += '\n\t/* module exports */'
		}
		out += '\n\treturn {'
		// export builtin types
		if name == 'builtin' {
			for typ in js.v_types {
				out += '\n\t\t$typ,'
			}
		}
		for i, pub_var in namespace.pub_vars {
			out += '\n\t\t$pub_var'
			if i < namespace.pub_vars.len - 1 {
				out += ','
			}
		}
		if namespace.pub_vars.len > 0 {
			out += '\n\t'
		}
		out += '};'
		out += '\n})('
		first = true
		for key, _ in namespace.imports {
			if !first {
				out += ', '
			}
			first = false
			out += key.replace('.', '_')
		}
		out += ');\n'
		// generate builtin basic type casts
		if name == 'builtin' {
			out += '// builtin type casts\n'
			out += 'const ['
			for i, typ in js.v_types {
				if i > 0 {
					out += ', '
				}
				out += '$typ'
			}
			out += '] = ['
			for i, typ in js.v_types {
				if i > 0 {
					out += ','
				}
				out += '\n\tfunction(val) { return new builtin.${typ}(val) }'
			}
			out += '\n]\n'
		}
	}
	if pref.is_shared {
		// Export, through CommonJS, the module of the entry file if `-shared` was passed
		export := nodes[nodes.len - 1].name
		out += 'if (typeof module === "object" && module.exports) module.exports = $export;\n'
	}
	out += '\n'
	return out
}

pub fn (mut g JsGen) enter_namespace(name string) {
	if g.namespaces[name] == 0 {
		// create a new namespace
		ns := &Namespace{
			name: name
		}
		g.namespaces[name] = ns
		g.ns = ns
	} else {
		g.ns = g.namespaces[name]
	}
	g.inside_builtin = name == 'builtin'
}

pub fn (mut g JsGen) escape_namespace() {
	g.ns = &Namespace(0)
	g.inside_builtin = false
}

pub fn (mut g JsGen) push_pub_var(s string) {
	g.ns.pub_vars << g.js_name(s)
}

pub fn (mut g JsGen) find_class_methods(stmts []ast.Stmt) {
	for stmt in stmts {
		match stmt {
			ast.FnDecl {
				if stmt.is_method {
					// Found struct method, store it to be generated along with the class.
					mut class_name := g.table.get_type_name(stmt.receiver.typ)
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

[inline]
fn verror(msg string) {
	eprintln('jsgen error: $msg')
	exit(1)
}

[inline]
pub fn (mut g JsGen) gen_indent() {
	if g.ns.indent > 0 && g.empty_line {
		g.ns.out.write_string(util.tabs(g.ns.indent))
	}
	g.empty_line = false
}

[inline]
pub fn (mut g JsGen) inc_indent() {
	g.ns.indent++
}

[inline]
pub fn (mut g JsGen) dec_indent() {
	g.ns.indent--
}

[inline]
pub fn (mut g JsGen) write(s string) {
	if g.ns == 0 {
		verror('g.write: not in a namespace')
	}
	g.gen_indent()
	g.ns.out.write_string(s)
}

[inline]
pub fn (mut g JsGen) writeln(s string) {
	if g.ns == 0 {
		verror('g.writeln: not in a namespace')
	}
	g.gen_indent()
	g.ns.out.writeln(s)
	g.empty_line = true
}

[inline]
pub fn (mut g JsGen) new_tmp_var() string {
	g.tmp_count++
	return '_tmp$g.tmp_count'
}

// 'mod1.mod2.fn' => 'mod1.mod2'
// 'fn' => ''
[inline]
fn get_ns(s string) string {
	idx := s.last_index('.') or { return '' }
	return s.substr(0, idx)
}

fn (mut g JsGen) get_alias(name string) string {
	ns := get_ns(name)
	if ns == '' {
		return name
	}
	alias := g.ns.imports[ns]
	if alias == '' {
		return name
	}
	return alias + '.' + name.split('.').last()
}

fn (mut g JsGen) js_name(name_ string) string {
	mut is_js := false
	mut name := name_
	if name.starts_with('JS.') {
		name = name[3..]
		is_js = true
	}
	ns := get_ns(name)
	name = if g.ns == 0 {
		name
	} else if ns == g.ns.name {
		name.split('.').last()
	} else {
		g.get_alias(name)
	}
	mut parts := name.split('.')
	if !is_js {
		for i, p in parts {
			if p in js.js_reserved {
				parts[i] = 'v_$p'
			}
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
	g.stmt_start_pos = g.ns.out.len
	match node {
		ast.EmptyStmt {}
		ast.AsmStmt {
			panic('inline asm is not supported by js')
		}
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
			g.ns.imports[node.mod] = node.alias
		}
		ast.InterfaceDecl {
			g.gen_interface_decl(node)
		}
		ast.Module {
			// skip: namespacing implemented externally
		}
		ast.NodeError {}
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
		ast.NodeError {}
		ast.EmptyExpr {}
		ast.CTempVar {
			g.write('/* ast.CTempVar: node.name */')
		}
		ast.DumpExpr {
			g.write('/* ast.DumpExpr: $node.expr */')
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
			g.gen_type_cast_expr(node)
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
			g.gen_float_literal_expr(node)
		}
		ast.GoExpr {
			// TODO
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
			g.gen_integer_literal_expr(node)
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
		ast.OffsetOf {
			// TODO
		}
		ast.SqlExpr {
			// TODO
		}
		ast.StringInterLiteral {
			g.gen_string_inter_literal(node)
		}
		ast.StringLiteral {
			g.gen_string_literal(node)
		}
		ast.StructInit {
			// TODO: once generic fns/unwrap_generic is implemented
			// if node.unresolved {
			// 	g.expr(ast.resolve_init(node, g.unwrap_generic(node.typ), g.table))
			// } else {
			// 	// `user := User{name: 'Bob'}`
			// 	g.gen_struct_init(node)
			// }
			// `user := User{name: 'Bob'}`
			g.gen_struct_init(node)
		}
		ast.TypeNode {
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
		ast.ComptimeSelector {
			// TODO
		}
		ast.UnsafeExpr {
			g.expr(node.expr)
		}
		ast.ArrayDecompose {}
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
				// TODO: Multiple types??
				should_cast := 
					(g.table.type_kind(stmt.left_types.first()) in js.shallow_equatables)
					&& (g.cast_stack.len <= 0 || stmt.left_types.first() != g.cast_stack.last())

				if should_cast {
					g.cast_stack << stmt.left_types.first()
					if g.file.mod.name == 'builtin' {
						g.write('new ')
					}
					g.write('${g.typ(stmt.left_types.first())}(')
				}
				g.expr(val)
				if should_cast {
					g.write(')')
					g.cast_stack.delete_last()
				}
			}
			if g.inside_loop {
				g.write('; ')
			} else {
				g.writeln(';')
			}
		}
	}
}

fn (mut g JsGen) gen_attrs(attrs []ast.Attr) {
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
	if g.inside_builtin {
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
		} else {
			if it.attrs.contains('js_getter') {
				g.write('get ')
			} else if it.attrs.contains('js_setter') {
				g.write('set ')
			}
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
	if it.is_method {
		if args.len > 0 {
			g.write(', ')
		}
		g.write('${it.params[0].name} = this')
	}
	g.writeln(') {')
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

fn (mut g JsGen) fn_args(args []ast.Param, is_variadic bool) {
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
		val := if it.val_var in ['', '_'] { '_' } else { it.val_var }
		// styp := g.typ(it.val_type)
		if it.key_var.len > 0 {
			g.write('for (const [$it.key_var, $val] of ')
			if it.kind == .string {
				g.write('Array.from(')
				g.expr(it.cond)
				g.write(".str.split(\'\').entries(), ([$it.key_var, $val]) => [$it.key_var, ")
				if g.ns.name == 'builtin' {
					g.write('new ')
				}
				g.write('byte($val)])')
			} else {
				g.expr(it.cond)
				g.write('.entries()')
			}
		} else {
			g.write('for (const $val of ')
			g.expr(it.cond)
			if it.kind == .string {
				g.write(".str.split('')")
			}
			// cast characters to bytes
			if val !in ['', '_'] && it.kind == .string {
				g.write('.map(c => ')
				if g.ns.name == 'builtin' {
					g.write('new ')
				}
				g.write('byte(c))')
			}
		}
		g.writeln(') {')
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

fn (mut g JsGen) gen_import_stmt(it ast.Import) {
	g.ns.imports[it.mod] = it.alias
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
	mut name := node.name
	if name.starts_with('JS.') {
		return
	}
	if name in js.v_types && g.ns.name == 'builtin' {
		return
	}
	js_name := g.js_name(name)
	g.gen_attrs(node.attrs)
	g.doc.gen_fac_fn(node.fields)
	g.write('function ${js_name}({ ')
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
	g.writeln('${js_name}.prototype = {')
	g.inc_indent()
	fns := g.method_fn_decls[name]
	for field in node.fields {
		typ := g.typ(field.typ)
		g.doc.gen_typ(typ)
		g.write('$field.name: ${g.to_js_typ_val(field.typ)}')
		g.writeln(',')
	}
	for cfn in fns {
		g.gen_method_decl(cfn)
		g.writeln(',')
	}
	// gen toString method
	fn_names := fns.map(it.name)
	if 'toString' !in fn_names {
		g.writeln('toString() {')
		g.inc_indent()
		g.write('return `$js_name {')
		for i, field in node.fields {
			if i == 0 {
				g.write(' ')
			} else {
				g.write(', ')
			}
			match g.typ(field.typ).split('.').last() {
				'string' { g.write('$field.name: "\${this["$field.name"].toString()}"') }
				else { g.write('$field.name: \${this["$field.name"].toString()} ') }
			}
		}
		g.writeln('}`')
		g.dec_indent()
		g.writeln('}')
	}
	g.dec_indent()
	g.writeln('};\n')
	if node.is_pub {
		g.push_pub_var(name)
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
	g.call_stack << it
	mut name := g.js_name(it.name)
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
	g.write('${name}(')
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
	g.call_stack.delete_last()
}

fn (mut g JsGen) gen_ident(node ast.Ident) {
	mut name := g.js_name(node.name)
	if node.kind == .blank_ident || name in ['', '_'] {
		name = g.new_tmp_var()
	}
	// TODO `is`
	// TODO handle optionals
	g.write(name)
	// TODO: Generate .val for basic types
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
		g.write('.toString()')
		if !expr.is_setter {
			g.write(')')
		}
	} else if left_typ.kind == .string {
		if expr.is_setter {
			// TODO: What's the best way to do this?
			// 'string'[3] = `o`
		} else {
			g.expr(expr.left)
			g.write('.str.charCodeAt(')
			g.expr(expr.index)
			g.write(')')
		}
	} else {
		// TODO Does this cover all cases?
		g.expr(expr.left)
		g.write('[')
		g.cast_stack << ast.int_type_idx
		g.expr(expr.index)
		g.cast_stack.delete_last()
		g.write(']')
	}
}

fn (mut g JsGen) gen_infix_expr(it ast.InfixExpr) {
	l_sym := g.table.get_type_symbol(it.left_type)
	r_sym := g.table.get_type_symbol(it.right_type)

	is_not := it.op in [.not_in, .not_is, .ne]
	if is_not {
		g.write('!(')
	}

	if it.op == .eq || it.op == .ne {
		// Shallow equatables
		if l_sym.kind in js.shallow_equatables && r_sym.kind in js.shallow_equatables {
			g.expr(it.left)
			g.write('.eq(')
			g.cast_stack << int(l_sym.kind)
			g.expr(it.right)
			g.cast_stack.delete_last()
			g.write(')')
		} else {
			g.write('vEq(')
			g.expr(it.left)
			g.write(', ')
			g.expr(it.right)
			g.write(')')
		}
	} else if l_sym.kind == .array && it.op == .left_shift { // arr << 1
		g.expr(it.left)
		g.write('.push(')
		// arr << [1, 2]
		if r_sym.kind == .array {
			g.write('...')
		}
		g.expr(it.right)
		g.write(')')
	} else if r_sym.kind in [.array, .map, .string] && it.op in [.key_in, .not_in] {
		g.expr(it.right)
		if r_sym.kind == .map {
			g.write('.has(')
		} else if r_sym.kind == .string {
			g.write('.str.includes(')
		} else {
			g.write('.includes(')
		}
		g.expr(it.left)
		if l_sym.kind == .string {
			g.write('.str')
		}
		g.write(')')
	} else if it.op in [.key_is, .not_is] { // foo is Foo
		g.expr(it.left)
		g.write(' instanceof ')
		g.write(g.typ(it.right_type))
	} else {
		is_arithmetic := it.op in [token.Kind.plus, .minus, .mul, .div, .mod]
		mut needs_cast := is_arithmetic && it.left_type != it.right_type
		mut greater_typ := 0
		if needs_cast {
			greater_typ = g.greater_typ(it.left_type, it.right_type)
			if g.cast_stack.len > 0 {
				needs_cast = g.cast_stack.last() != greater_typ
			}
		}

		if needs_cast {
			if g.ns.name == 'builtin' {
				g.write('new ')
			}
			g.write('${g.typ(greater_typ)}(')
			g.cast_stack << greater_typ
		}
		g.expr(it.left)
		g.write(' $it.op ')
		g.expr(it.right)

		if needs_cast {
			g.cast_stack.delete_last()
			g.write(')')
		}
	}

	if is_not {
		g.write(')')
	}
}

fn (mut g JsGen) greater_typ(left ast.Type, right ast.Type) ast.Type {
	l := int(left)
	r := int(right)
	lr := [l, r]
	if ast.string_type_idx in lr {
		return ast.Type(ast.string_type_idx)
	}
	should_float := (l in ast.integer_type_idxs && r in ast.float_type_idxs)
		|| (r in ast.integer_type_idxs && l in ast.float_type_idxs)
	if should_float {
		if ast.f64_type_idx in lr {
			return ast.Type(ast.f64_type_idx)
		}
		if ast.f32_type_idx in lr {
			return ast.Type(ast.f32_type_idx)
		}
		return ast.Type(ast.float_literal_type)
	}
	should_int := (l in ast.integer_type_idxs && r in ast.integer_type_idxs)
	if should_int {
		// cant add to u64 - if (ast.u64_type_idx in lr) { return ast.Type(ast.u64_type_idx) }
		// just guessing this order
		if ast.i64_type_idx in lr {
			return ast.Type(ast.i64_type_idx)
		}
		if ast.u32_type_idx in lr {
			return ast.Type(ast.u32_type_idx)
		}
		if ast.int_type_idx in lr {
			return ast.Type(ast.int_type_idx)
		}
		if ast.u16_type_idx in lr {
			return ast.Type(ast.u16_type_idx)
		}
		if ast.i16_type_idx in lr {
			return ast.Type(ast.i16_type_idx)
		}
		if ast.byte_type_idx in lr {
			return ast.Type(ast.byte_type_idx)
		}
		if ast.i8_type_idx in lr {
			return ast.Type(ast.i8_type_idx)
		}
		return ast.Type(ast.int_literal_type_idx)
	}
	return ast.Type(l)
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
	should_cast := !(g.cast_stack.len > 0 && g.cast_stack.last() == ast.string_type_idx)
	if should_cast {
		if g.file.mod.name == 'builtin' {
			g.write('new ')
		}
		g.write('string(')
	}
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
	if should_cast {
		g.write(')')
	}
}

fn (mut g JsGen) gen_string_literal(it ast.StringLiteral) {
	text := it.val.replace("'", "\\'")
	should_cast := !(g.cast_stack.len > 0 && g.cast_stack.last() == ast.string_type_idx)
	if should_cast {
		if g.file.mod.name == 'builtin' {
			g.write('new ')
		}
		g.write('string(')
	}
	g.write("'$text'")
	if should_cast {
		g.write(')')
	}
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
		fixed_info := sym.info as ast.ArrayFixed
		typ_name := g.table.get_type_name(fixed_info.elem_type)
		g.write('"[$fixed_info.size]$typ_name"')
	} else if sym.kind == .function {
		info := sym.info as ast.FnType
		fn_info := info.func
		mut repr := 'fn ('
		for i, arg in fn_info.params {
			if i > 0 {
				repr += ', '
			}
			repr += g.table.get_type_name(arg.typ)
		}
		repr += ')'
		if fn_info.return_type != ast.void_type {
			repr += ' ${g.table.get_type_name(fn_info.return_type)}'
		}
		g.write('"$repr"')
	} else {
		g.write('"$sym.name"')
	}
}

fn (mut g JsGen) gen_type_cast_expr(it ast.CastExpr) {
	is_literal := ((it.expr is ast.IntegerLiteral && it.typ in ast.integer_type_idxs)
		|| (it.expr is ast.FloatLiteral && it.typ in ast.float_type_idxs))
	// Skip cast if type is the same as the parrent caster
	if g.cast_stack.len > 0 && is_literal {
		if it.typ == g.cast_stack[g.cast_stack.len - 1] {
			return
		}
	}
	g.cast_stack << it.typ
	typ := g.typ(it.typ)
	if !is_literal {
		if typ !in js.v_types || g.ns.name == 'builtin' {
			g.write('new ')
		}
		g.write('${typ}(')
	}
	g.expr(it.expr)
	if typ == 'string' && it.expr !is ast.StringLiteral {
		g.write('.toString()')
	}
	if !is_literal {
		g.write(')')
	}
	g.cast_stack.delete_last()
}

fn (mut g JsGen) gen_integer_literal_expr(it ast.IntegerLiteral) {
	typ := ast.Type(ast.int_type)

	// Don't wrap integers for use in JS.foo functions.
	// TODO: call.language always seems to be "v", parser bug?
	if g.call_stack.len > 0 {
		call := g.call_stack[g.call_stack.len - 1]
		// if call.language == .js {
		for t in call.args {
			if t.expr is ast.IntegerLiteral {
				if t.expr == it {
					g.write(it.val)
					return
				}
			}
		}
		//}
	}

	// Skip cast if type is the same as the parrent caster
	if g.cast_stack.len > 0 {
		if g.cast_stack[g.cast_stack.len - 1] in ast.integer_type_idxs {
			g.write('$it.val')
			return
		}
	}

	if g.ns.name == 'builtin' {
		g.write('new ')
	}

	g.write('${g.typ(typ)}($it.val)')
}

fn (mut g JsGen) gen_float_literal_expr(it ast.FloatLiteral) {
	typ := ast.Type(ast.f32_type)

	// Don't wrap integers for use in JS.foo functions.
	// TODO: call.language always seems to be "v", parser bug?
	if g.call_stack.len > 0 {
		call := g.call_stack[g.call_stack.len - 1]
		// if call.language == .js {
		for i, t in call.args {
			if t.expr is ast.FloatLiteral {
				if t.expr == it {
					if call.expected_arg_types[i] in ast.integer_type_idxs {
						g.write(int(it.val.f64()).str())
					} else {
						g.write(it.val)
					}
					return
				}
			}
		}
		//}
	}

	// Skip cast if type is the same as the parrent caster
	if g.cast_stack.len > 0 {
		if g.cast_stack[g.cast_stack.len - 1] in ast.float_type_idxs {
			g.write('$it.val')
			return
		} else if g.cast_stack[g.cast_stack.len - 1] in ast.integer_type_idxs {
			g.write(int(it.val.f64()).str())
			return
		}
	}

	if g.ns.name == 'builtin' {
		g.write('new ')
	}

	g.write('${g.typ(typ)}($it.val)')
}
