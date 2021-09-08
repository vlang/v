module js

import strings
import v.ast
import v.token
import v.pref
import v.util
import v.util.version
import v.depgraph
import encoding.base64
import v.gen.js.sourcemap

struct MutArg {
	tmp_var string
	expr    ast.Expr = ast.empty_expr()
}

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
		'int_literal', 'float_literal', 'size_t', 'bool', 'string', 'map', 'array', 'any']
	shallow_equatables = [ast.Kind.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .f32, .f64,
		.int_literal, .float_literal, .size_t, .bool, .string]
)

struct SourcemapHelper {
	src_path string
	src_line u32
	ns_pos   u32
}

struct Namespace {
	name string
mut:
	pub_vars         []string
	imports          map[string]string
	indent           int
	methods          map[string][]ast.FnDecl
	sourcemap_helper []SourcemapHelper
}

[heap]
struct JsGen {
	pref &pref.Preferences
mut:
	table                 &ast.Table
	definitions           strings.Builder
	ns                    &Namespace
	namespaces            map[string]&Namespace
	doc                   &JsDoc
	enable_doc            bool
	file                  &ast.File
	tmp_count             int
	inside_ternary        bool
	inside_loop           bool
	inside_map_set        bool // map.set(key, value)
	inside_builtin        bool
	inside_if_optional    bool
	generated_builtin     bool
	inside_def_typ_decl   bool
	is_test               bool
	stmt_start_pos        int
	defer_stmts           []ast.DeferStmt
	fn_decl               &ast.FnDecl // pointer to the FnDecl we are currently inside otherwise 0
	str_types             []string    // types that need automatic str() generation
	method_fn_decls       map[string][]ast.FnDecl
	builtin_fns           []string // Functions defined in `builtin`
	empty_line            bool
	cast_stack            []ast.Type
	call_stack            []ast.CallExpr
	is_vlines_enabled     bool // is it safe to generate #line directives when -g is passed
	sourcemap             sourcemap.SourceMap // maps lines in generated javascrip file to original source files and line
	comptime_var_type_map map[string]ast.Type
	defer_ifdef           string
	out                   strings.Builder = strings.new_builder(128)
}

fn (mut g JsGen) write_tests_definitions() {
	g.definitions.writeln('globalThis.g_test_oks = 0;')
	g.definitions.writeln('globalThis.g_test_fails = 0;')
}

pub fn gen(files []&ast.File, table &ast.Table, pref &pref.Preferences) string {
	mut g := &JsGen{
		definitions: strings.new_builder(100)
		table: table
		pref: pref
		fn_decl: 0
		empty_line: true
		doc: 0
		ns: 0
		enable_doc: true
		file: 0
	}
	g.doc = new_jsdoc(g)
	// TODO: Add '[-no]-jsdoc' flag
	if pref.is_prod {
		g.enable_doc = false
		g.is_vlines_enabled = false
	}
	g.init()
	mut graph := depgraph.new_dep_graph()
	if g.pref.sourcemap {
		mut sg := sourcemap.generate_empty_map()
		g.sourcemap = sg.add_map('', '', g.pref.sourcemap_src_included, 0, 0)
	}
	mut tests_inited := false

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
		if g.enable_doc {
			g.writeln('/** @namespace $file.mod.name */')
		}
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
			g.writeln('Object.defineProperty(array.prototype,"len", { get: function() {return new int(this.arr.length);}, set: function(l) { this.arr.length = l.valueOf(); } }); ')
			g.writeln('Object.defineProperty(string.prototype,"len", { get: function() {return new int(this.str.length);}, set: function(l) {/* ignore */ } }); ')
			g.writeln('Object.defineProperty(map.prototype,"len", { get: function() {return new int(this.map.length);}, set: function(l) { this.map.length = l.valueOf(); } }); ')
			g.writeln('Object.defineProperty(array.prototype,"length", { get: function() {return new int(this.arr.length);}, set: function(l) { this.arr.length = l.valueOf(); } }); ')
			g.generated_builtin = true
		}
		if g.is_test && !tests_inited {
			g.write_tests_definitions()
			tests_inited = true
		}
		g.stmts(file.stmts)
		// store the current namespace
		g.escape_namespace()
	}
	if g.pref.is_test {
		g.gen_js_main_for_tests()
	}
	// resolve imports
	deps_resolved := graph.resolve()
	nodes := deps_resolved.nodes

	mut out := g.definitions.str() + g.hashes()
	// equality check for js objects
	// TODO: Fix msvc bug that's preventing $embed_file('fast_deep_equal.js')
	// unsafe {
	//	mut eq_fn := $embed_file('fast_deep_equal.js')
	//	out += eq_fn.data().vstring()
	//}
	out += fast_deep_eq_fn

	if pref.is_shared {
		// Export, through CommonJS, the module of the entry file if `-shared` was passed
		export := nodes[nodes.len - 1].name
		out += 'if (typeof module === "object" && module.exports) module.exports = $export;\n'
	}
	out += '\n'
	out += g.out.str()

	/*
	for node in nodes {
		name := g.js_name(node.name).replace('.', '_')
		if g.enable_doc {
			out += '/** @namespace $name */\n'
		}
		// out += 'const $name = (function ('
		mut namespace := g.namespaces[node.name]

		
		if g.pref.sourcemap {
			// calculate current output start line
			mut current_line := u32(out.count('\n') + 1)
			mut sm_pos := u32(0)
			for sourcemap_ns_entry in namespace.sourcemap_helper {
				// calculate final generated location in output based on position
				current_segment := g.out.substr(int(sm_pos), int(sourcemap_ns_entry.ns_pos))
				current_line += u32(current_segment.count('\n'))
				current_column := if last_nl_pos := current_segment.last_index('\n') {
					u32(current_segment.len - last_nl_pos - 1)
				} else {
					u32(0)
				}
				g.sourcemap.add_mapping(sourcemap_ns_entry.src_path, sourcemap.SourcePosition{
					source_line: sourcemap_ns_entry.src_line
					source_column: 0 // sourcemap_ns_entry.src_column
				}, current_line, current_column, '')
				sm_pos = sourcemap_ns_entry.ns_pos
			}
		}
		

		// public scope
		out += '\n'
	}*/
	if g.pref.sourcemap {
		out += g.create_sourcemap()
	}
	return out
}

fn (g JsGen) create_sourcemap() string {
	mut sm := g.sourcemap
	mut out := '\n//# sourceMappingURL=data:application/json;base64,'
	out += base64.encode(sm.to_json().str().bytes())
	out += '\n'

	return out
}

pub fn (mut g JsGen) gen_js_main_for_tests() {
	g.enter_namespace('main')
	g.writeln('(function() {  ')
	g.inc_indent()
	all_tfuncs := g.get_all_test_function_names()

	g.writeln('')
	g.writeln('globalThis.VTEST=1')
	if g.pref.is_stats {
		g.writeln('let bt = start_testing($all_tfuncs.len, "$g.pref.path")')
	}
	for tname in all_tfuncs {
		tcname := g.js_name(tname)

		if g.pref.is_stats {
			g.writeln('bt.testing_step_start("$tcname")')
		}

		g.writeln('try { ${tcname}(); } catch (_e) {} ')
		if g.pref.is_stats {
			g.writeln('bt.testing_step_end();')
		}
	}

	g.writeln('')
	if g.pref.is_stats {
		g.writeln('bt.end_testing();')
	}
	g.dec_indent()
	g.writeln('})();')
	g.escape_namespace()
}

fn (g &JsGen) get_all_test_function_names() []string {
	mut tfuncs := []string{}
	mut tsuite_begin := ''
	mut tsuite_end := ''
	for _, f in g.table.fns {
		if f.name.ends_with('.testsuite_begin') {
			tsuite_begin = f.name
			continue
		}
		if f.name.contains('.test_') {
			tfuncs << f.name
			continue
		}
		if f.name.ends_with('.testsuite_end') {
			tsuite_end = f.name
			continue
		}
	}
	mut all_tfuncs := []string{}
	if tsuite_begin.len > 0 {
		all_tfuncs << tsuite_begin
	}
	all_tfuncs << tfuncs
	if tsuite_end.len > 0 {
		all_tfuncs << tsuite_end
	}
	return all_tfuncs
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
	// g.definitions.writeln('"use strict";')
	g.definitions.writeln('')
	g.definitions.writeln('var \$global = (new Function("return this"))();')
	g.definitions.writeln('function \$ref(value) { if (value instanceof \$ref) { return value; } this.val = value; } ')
	g.definitions.writeln('\$ref.prototype.valueOf = function() { return this.val; } ')
	if g.pref.backend != .js_node {
		g.definitions.writeln('const \$process = {')
		g.definitions.writeln('  arch: "js",')
		if g.pref.backend == .js_freestanding {
			g.definitions.writeln('  platform: "freestanding"')
		} else {
			g.definitions.writeln('  platform: "browser"')
		}
		g.definitions.writeln('}')

		g.definitions.writeln('const \$os = {')
		g.definitions.writeln('  endianess: "LE",')

		g.definitions.writeln('}')
	} else {
		g.definitions.writeln('const \$os = require("os");')
		g.definitions.writeln('const \$process = process;')
	}
	g.definitions.writeln('function alias(value) { return value; } ')

	g.definitions.writeln('function \$v_fmt(value) { let res = "";
		if (Object.getPrototypeOf(s).hasOwnProperty("str") && typeof s.str == "function") res = s.str().str
		else res = s.toString()
		return res
  } ')
}

pub fn (g JsGen) hashes() string {
	mut res := '// V_COMMIT_HASH $version.vhash()\n'
	res += '// V_CURRENT_COMMIT_HASH ${version.githash(g.pref.building_v)}\n'
	return res
}

[noreturn]
fn verror(msg string) {
	eprintln('jsgen error: $msg')
	exit(1)
}

[inline]
pub fn (mut g JsGen) gen_indent() {
	if g.ns.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.ns.indent))
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
	g.out.write_string(s)
}

[inline]
pub fn (mut g JsGen) writeln(s string) {
	if g.ns == 0 {
		verror('g.writeln: not in a namespace')
	}
	g.gen_indent()
	g.out.writeln(s)
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
	/*
	mut is_js := false
	is_overload := ['+', '-', '*', '/', '==', '<', '>']
	mut name := name_
	if name.starts_with('JS.') {
		name = name[3..]
		is_js = true
	}
	ns := get_ns(name)
	name = if name in is_overload {
		match name {
			'+' {
				'\$add'
			}
			'-' {
				'\$sub'
			}
			'/' {
				'\$div'
			}
			'*' {
				'\$mul'
			}
			'%' {
				'\$mod'
			}
			'==' {
				'eq'
			}
			'>' {
				'\$gt'
			}
			'<' {
				'\$lt'
			}
			else {
				''
			}
		}
	} else if g.ns == 0 {
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
	return parts.join('.')*/

	mut name := name_
	if name.starts_with('JS.') {
		name = name[3..]
		return name
	}
	name = name_.replace('.', '__')
	if name in js.js_reserved {
		return '_v_$name'
	}
	return name
}

fn (mut g JsGen) stmts(stmts []ast.Stmt) {
	g.inc_indent()
	for stmt in stmts {
		g.stmt(stmt)
	}
	g.dec_indent()
}

[inline]
fn (mut g JsGen) write_v_source_line_info(pos token.Position) {
	// g.inside_ternary == 0 &&
	if g.pref.sourcemap {
		g.ns.sourcemap_helper << SourcemapHelper{
			src_path: util.vlines_escape_path(g.file.path, g.pref.ccompiler)
			src_line: u32(pos.line_nr + 1)
			ns_pos: u32(g.out.len)
		}
	}
	if g.pref.is_vlines && g.is_vlines_enabled {
		g.write(' /* ${pos.line_nr + 1} $g.out.len */ ')
	}
}

fn (mut g JsGen) gen_global_decl(node ast.GlobalDecl) {
	mod := if g.pref.build_mode == .build_module { 'enumerable: false' } else { 'enumerable: true' }
	for field in node.fields {
		if field.has_expr {
			tmp_var := g.new_tmp_var()
			g.write('const $tmp_var = ')
			g.expr(field.expr)
			g.writeln(';')
			g.writeln('Object.defineProperty(\$global,"$field.name", {
				configurable: false,
				$mod ,
				writable: true,
				value: $tmp_var
				}
			); // global')
		} else {
			// TODO(playXE): Initialize with default value of type

			if field.typ.is_ptr() {
				g.writeln('Object.defineProperty(\$global,"$field.name", {
					configurable: false,
					$mod ,
					writable: true,
					value: new \$ref({})
					}
				); // global')
			} else {
				g.writeln('Object.defineProperty(\$global,"$field.name", {
					configurable: false,
					$mod ,
					writable: true,
					value: {}
					}
				); // global')
			}
		}
	}
}

fn (mut g JsGen) stmt_no_semi(node ast.Stmt) {
	g.stmt_start_pos = g.out.len
	match node {
		ast.EmptyStmt {}
		ast.AsmStmt {
			panic('inline asm is not supported by js')
		}
		ast.AssertStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_assert_stmt(node)
		}
		ast.AssignStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_assign_stmt(node, false)
		}
		ast.Block {
			g.write_v_source_line_info(node.pos)
			g.gen_block(node)
			g.writeln('')
		}
		ast.BranchStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_branch_stmt(node)
		}
		ast.CompFor {}
		ast.ConstDecl {
			g.write_v_source_line_info(node.pos)
			g.gen_const_decl(node)
		}
		ast.DeferStmt {
			g.defer_stmts << node
		}
		ast.EnumDecl {
			g.write_v_source_line_info(node.pos)
			g.gen_enum_decl(node)
			g.writeln('')
		}
		ast.ExprStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_expr_stmt_no_semi(node)
		}
		ast.FnDecl {
			g.write_v_source_line_info(node.pos)
			g.fn_decl = unsafe { &node }
			g.gen_fn_decl(node)
		}
		ast.ForCStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_for_c_stmt(node)
			g.writeln('')
		}
		ast.ForInStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_for_in_stmt(node)
			g.writeln('')
		}
		ast.ForStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_for_stmt(node)
			g.writeln('')
		}
		ast.GlobalDecl {
			g.write_v_source_line_info(node.pos)
			g.gen_global_decl(node)
			g.writeln('')
		}
		ast.GotoLabel {
			g.write_v_source_line_info(node.pos)
			g.writeln('${g.js_name(node.name)}:')
		}
		ast.GotoStmt {
			// skip: JS has no goto
		}
		ast.HashStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_hash_stmt(node)
		}
		ast.Import {
			g.ns.imports[node.mod] = node.alias
		}
		ast.InterfaceDecl {
			g.write_v_source_line_info(node.pos)
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
			g.write_v_source_line_info(node.pos)
			g.gen_struct_decl(node)
		}
		ast.TypeDecl {}
	}
}

fn (mut g JsGen) stmt(node ast.Stmt) {
	g.stmt_start_pos = g.out.len
	match node {
		ast.EmptyStmt {}
		ast.AsmStmt {
			panic('inline asm is not supported by js')
		}
		ast.AssertStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_assert_stmt(node)
		}
		ast.AssignStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_assign_stmt(node, true)
		}
		ast.Block {
			g.write_v_source_line_info(node.pos)
			g.gen_block(node)
			g.writeln('')
		}
		ast.BranchStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_branch_stmt(node)
		}
		ast.CompFor {}
		ast.ConstDecl {
			g.write_v_source_line_info(node.pos)
			g.gen_const_decl(node)
		}
		ast.DeferStmt {
			g.defer_stmts << node
		}
		ast.EnumDecl {
			g.write_v_source_line_info(node.pos)
			g.gen_enum_decl(node)
			g.writeln('')
		}
		ast.ExprStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_expr_stmt(node)
		}
		ast.FnDecl {
			g.write_v_source_line_info(node.pos)
			g.fn_decl = unsafe { &node }
			g.gen_fn_decl(node)
		}
		ast.ForCStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_for_c_stmt(node)
			g.writeln('')
		}
		ast.ForInStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_for_in_stmt(node)
			g.writeln('')
		}
		ast.ForStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_for_stmt(node)
			g.writeln('')
		}
		ast.GlobalDecl {
			g.write_v_source_line_info(node.pos)
			g.gen_global_decl(node)
			g.writeln('')
		}
		ast.GotoLabel {
			g.write_v_source_line_info(node.pos)
			g.writeln('${g.js_name(node.name)}:')
		}
		ast.GotoStmt {
			// skip: JS has no goto
		}
		ast.HashStmt {
			g.write_v_source_line_info(node.pos)
			g.gen_hash_stmt(node)
		}
		ast.Import {
			g.ns.imports[node.mod] = node.alias
		}
		ast.InterfaceDecl {
			g.write_v_source_line_info(node.pos)
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
			g.write_v_source_line_info(node.pos)
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
			g.write('new bool(')
			if node.val == true {
				g.write('true')
			} else {
				g.write('false')
			}
			g.write(')')
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
			g.write("new byte('$node.val')")
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
			g.gen_go_expr(node)
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
			g.infix_expr(node)
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
		ast.None {
			g.write('none__')
		}
		ast.MatchExpr {
			g.match_expr(node)
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
			if node.op in [.inc, .dec] {
				g.write('.val $node.op')
			} else {
				g.write(node.op.str())
			}
		}
		ast.PrefixExpr {
			if node.op in [.amp, .mul] {
				if node.op == .amp {
					// if !node.right_type.is_pointer() {
					// kind of weird way to handle references but it allows us to access type methods easily.
					g.write('(function(x) {')
					g.write(' return { val: x, __proto__: Object.getPrototypeOf(x), valueOf: function() { return this.val; } }})(  ')
					g.expr(node.right)
					g.write(')')
					//} else {
					//		g.expr(node.right)
					//	}
				} else {
					g.write('(')
					g.expr(node.right)
					g.write(').valueOf()')
				}
			} else {
				g.write(node.op.str())

				if node.op in [.inc, .dec] {
					g.expr(node.right)
					g.write('.val ')
				} else {
					g.write('(')
					g.expr(node.right)
					g.write('.valueOf()')
					g.write(')')
				}
			}
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
		ast.SizeOf, ast.IsRefType {
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
			typ := g.unwrap_generic(node.typ)
			sym := g.table.get_type_symbol(typ)

			g.write('${g.js_name(sym.name)}')
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

fn (mut g JsGen) gen_assert_metainfo(node ast.AssertStmt) string {
	mod_path := g.file.path
	fn_name := g.fn_decl.name
	line_nr := node.pos.line_nr
	src := node.expr.str()
	metaname := 'v_assert_meta_info_$g.new_tmp_var()'
	g.writeln('let $metaname = {}')
	g.writeln('${metaname}.fpath = new string("$mod_path");')
	g.writeln('${metaname}.line_nr = new int("$line_nr")')
	g.writeln('${metaname}.fn_name = new string("$fn_name")')
	metasrc := src
	g.writeln('${metaname}.src = "$metasrc"')

	match mut node.expr {
		ast.InfixExpr {
			expr_op_str := node.expr.op.str()
			expr_left_str := node.expr.left.str()
			expr_right_str := node.expr.right.str()
			g.writeln('\t${metaname}.op = new string("$expr_op_str");')
			g.writeln('\t${metaname}.llabel = new string("$expr_left_str");')
			g.writeln('\t${metaname}.rlabel = new string("$expr_right_str");')
			g.write('\t${metaname}.lvalue = new string("')
			g.gen_assert_single_expr(node.expr.left, node.expr.left_type)
			g.writeln('");')
			g.write('\t${metaname}.rvalue = new string("')
			g.gen_assert_single_expr(node.expr.right, node.expr.right_type)
			g.writeln('");')
		}
		ast.CallExpr {
			g.writeln('\t${metaname}.op = new string("call");')
		}
		else {}
	}
	return metaname
}

fn (mut g JsGen) gen_assert_single_expr(expr ast.Expr, typ ast.Type) {
	// eprintln('> gen_assert_single_expr typ: $typ | expr: $expr | typeof(expr): ${typeof(expr)}')
	unknown_value := '*unknown value*'
	match expr {
		ast.CastExpr, ast.IfExpr, ast.IndexExpr, ast.MatchExpr {
			g.write(unknown_value)
		}
		ast.PrefixExpr {
			g.write(unknown_value)
		}
		ast.TypeNode {
			sym := g.table.get_type_symbol(g.unwrap_generic(typ))
			g.write('$sym.name')
		}
		else {
			g.writeln(unknown_value)
		}
	}
	g.write(' /* typeof: ' + expr.type_name() + ' type: ' + typ.str() + ' */ ')
}

// TODO
fn (mut g JsGen) gen_assert_stmt(a ast.AssertStmt) {
	if !a.is_used {
		return
	}
	g.writeln('// assert')
	g.write('if( ')
	g.expr(a.expr)
	g.write('.valueOf() ) {')
	s_assertion := a.expr.str().replace('"', "'")
	mut mod_path := g.file.path.replace('\\', '\\\\')
	if g.is_test {
		metaname_ok := g.gen_assert_metainfo(a)
		g.writeln('	g_test_oks++;')
		g.writeln('	main__cb_assertion_ok($metaname_ok);')
		g.writeln('} else {')
		metaname_fail := g.gen_assert_metainfo(a)
		g.writeln('	g_test_fails++;')
		g.writeln('	main__cb_assertion_failed($metaname_fail);')
		g.writeln('	exit(1);')
		g.writeln('}')
		return
	}
	g.writeln('} else {')
	g.inc_indent()
	g.writeln('eprintln(new string("$mod_path:${a.pos.line_nr + 1}: FAIL: fn ${g.fn_decl.name}(): assert $s_assertion"));')
	g.writeln('exit(1);')
	g.dec_indent()
	g.writeln('}')
}

fn (mut g JsGen) gen_assign_stmt(stmt ast.AssignStmt, semicolon bool) {
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
		if semicolon {
			g.writeln(';')
		}
	} else {
		// `a := 1` | `a,b := 1,2`
		for i, left in stmt.left {
			mut op := stmt.op
			if stmt.op == .decl_assign {
				op = .assign
			}
			is_assign := stmt.op in [.plus_assign, .minus_assign, .mult_assign, .div_assign,
				.xor_assign, .mod_assign, .or_assign, .and_assign, .right_shift_assign,
				.left_shift_assign,
			]

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
			l_sym := g.table.get_type_symbol(stmt.left_types[i])
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
			mut is_ptr := false
			if stmt.op == .assign && stmt.left_types[i].is_ptr() {
				is_ptr = true
				g.write('.val')
			}
			if g.inside_map_set && op == .assign {
				g.inside_map_set = false
				g.write(', ')
				g.expr(val)
				if is_ptr {
					g.write('.val')
				}
				g.write(')')
			} else {
				if is_assign {
					if l_sym.kind == .string {
						g.write('.str')
					} else {
						g.write('.val')
					}
					g.write(' = ')
					g.expr(left)

					match op {
						.plus_assign {
							g.write(' + ')
						}
						.minus_assign {
							g.write(' - ')
						}
						.mult_assign {
							g.write(' * ')
						}
						.div_assign {
							g.write(' / ')
						}
						.mod_assign {
							g.write(' % ')
						}
						.xor_assign {
							g.write(' ^ ')
						}
						.and_assign {
							g.write(' & ')
						}
						.right_shift_assign {
							g.write(' >> ')
						}
						.left_shift_assign {
							g.write(' << ')
						}
						.or_assign {
							g.write(' | ')
						}
						else {
							panic('unexpected op $op')
						}
					}
				} else {
					g.write(' $op ')
				}
				// TODO: Multiple types??
				should_cast :=
					(g.table.type_kind(stmt.left_types.first()) in js.shallow_equatables)
					&& (g.cast_stack.len <= 0 || stmt.left_types.first() != g.cast_stack.last())

				if should_cast {
					g.cast_stack << stmt.left_types.first()
					g.write('new ')
					g.write('${g.typ(stmt.left_types.first())}(')
				}
				g.expr(val)
				if is_ptr {
					g.write('.val')
				}
				if should_cast {
					g.write(')')
					g.cast_stack.delete_last()
				}
			}
			if semicolon {
				if g.inside_loop {
					g.write('; ')
				} else {
					g.writeln(';')
				}
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
			i = field.expr.val.int()
		}
		g.writeln('$i,')
		i++
	}
	g.dec_indent()
	g.writeln('};')
	if it.is_pub {
		g.push_pub_var(it.name)
	}
}

fn (mut g JsGen) gen_expr_stmt(it ast.ExprStmt) {
	g.expr(it.expr)
	if !it.is_expr && it.expr !is ast.IfExpr && !g.inside_ternary && !g.inside_if_optional {
		g.writeln(';')
	}
}

fn (mut g JsGen) gen_expr_stmt_no_semi(it ast.ExprStmt) {
	g.expr(it.expr)
}

enum FnGenType {
	function
	struct_method
	alias_method
	iface_method
}

fn (g &JsGen) fn_gen_type(it &ast.FnDecl) FnGenType {
	if it.is_method && g.table.get_type_symbol(it.params[0].typ).kind == .alias {
		return .alias_method
	} else if it.is_method && g.table.get_type_symbol(it.params[0].typ).kind == .interface_ {
		return .iface_method
	} else if it.is_method || it.no_body {
		return .struct_method
	} else {
		return .function
	}
}

fn (mut g JsGen) gen_fn_decl(it ast.FnDecl) {
	res := g.fn_gen_type(it)
	if it.language == .js {
		return
	}
	/*
	if res == .struct_method {
		// Struct methods are handled by class generation code.
		return
	}*/
	if g.inside_builtin {
		g.builtin_fns << it.name
	}
	cur_fn_decl := g.fn_decl
	g.gen_method_decl(it, res)
	g.fn_decl = cur_fn_decl
}

fn fn_has_go(node ast.FnDecl) bool {
	mut has_go := false
	for stmt in node.stmts {
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.GoExpr {
				has_go = true
				break
			}
		}
	}
	return has_go
}

// cc_type whether to prefix 'struct' or not (C__Foo -> struct Foo)
fn (mut g JsGen) cc_type(typ ast.Type, is_prefix_struct bool) string {
	sym := g.table.get_type_symbol(g.unwrap_generic(typ))
	mut styp := sym.cname
	match mut sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.is_generic {
				mut sgtyps := '_T'
				for gt in sym.info.generic_types {
					gts := g.table.get_type_symbol(g.unwrap_generic(gt))
					sgtyps += '_$gts.cname'
				}
				styp += sgtyps
			}
		}
		else {}
	}
	return styp
}

fn (mut g JsGen) generic_fn_name(types []ast.Type, before string, is_decl bool) string {
	if types.len == 0 {
		return before
	}

	mut name := before + '_T'
	for typ in types {
		name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) + g.typ(typ.set_nr_muls(0))
	}
	return name
}

fn (mut g JsGen) gen_method_decl(it ast.FnDecl, typ FnGenType) {
	unsafe {
		g.fn_decl = &it
	}
	cur_fn_save := g.table.cur_fn
	defer {
		g.table.cur_fn = cur_fn_save
	}
	unsafe {
		g.table.cur_fn = &it
	}
	node := it
	mut name := it.name
	if name in ['+', '-', '*', '/', '%', '<', '=='] {
		name = util.replace_op(name)
	}

	if node.is_method {
		unwrapped_rec_sym := g.table.get_type_symbol(g.unwrap_generic(node.receiver.typ))
		if unwrapped_rec_sym.kind == .placeholder {
			return
		}
		name = g.cc_type(node.receiver.typ, false) + '_' + name
	}

	name = g.js_name(name)

	name = g.generic_fn_name(g.table.cur_concrete_types, name, true)

	has_go := fn_has_go(it)
	if it.is_pub && !it.is_method {
		g.push_pub_var(name)
	}
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

		g.write('function ')

		g.write('${name}(')
		if it.is_pub && !it.is_method {
			g.push_pub_var(name)
		}
	}
	mut args := it.params

	g.fn_args(args, it.is_variadic)
	g.write(') {')
	for i, arg in args {
		is_varg := i == args.len - 1 && it.is_variadic
		arg_name := g.js_name(arg.name)
		if is_varg {
			g.writeln('$arg_name = new array($arg_name);')
		} else {
			if arg.typ.is_ptr() || arg.is_mut {
				g.writeln('$arg_name = new \$ref($arg_name)')
			}
		}
	}
	g.stmts(it.stmts)
	g.writeln('}')

	if is_main {
		g.write(')();')
	}
	g.writeln('')
	/*
	if typ == .alias_method || typ == .iface_method {
		sym := g.table.get_final_type_symbol(it.params[0].typ.set_nr_muls(0))
		name := g.js_name(sym.name)
		if name in js.v_types {
			g.writeln('')
		}
		g.writeln('${name}.prototype.$it.name = function ')
	}
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
		if it.params[0].is_mut || it.params[0].typ.is_ptr() {
			g.write('${it.params[0].name} = new \$ref(this)')
		} else {
			g.write('${it.params[0].name} = this')
		}
	}
	g.writeln(') {')
	for i, arg in args {
		is_varg := i == args.len - 1 && it.is_variadic
		name := g.js_name(arg.name)
		if is_varg {
			g.writeln('$name = new array($name);')
		} else {
			if arg.typ.is_ptr() || arg.is_mut {
				g.writeln('$name = new \$ref($name)')
			}
		}
	}

	g.stmts(it.stmts)
	g.writeln('}')
	for attr in it.attrs {
		match attr.name {
			'export' {
				g.writeln('globalThis.$attr.arg = ${g.js_name(it.name)};')
			}
			else {}
		}
	}
	if is_main {
		g.write(')();')
	} else if typ != .struct_method {
		// g.write(';')
	}
	if typ == .struct_method || typ == .alias_method || typ == .iface_method {
		g.writeln('\n')
	}
	*/
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
		g.write('+') // convert to number or boolean
		g.expr(it.cond)
	}
	g.write('; ')
	if it.has_inc {
		g.stmt_no_semi(it.inc)
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
		g.writeln('; $i = new int($i + 1)) {')
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
				if it.cond_type.is_ptr() {
					g.write('.valueOf()')
				}
				g.write('.str.split(\'\').entries(), ([$it.key_var, $val]) => [$it.key_var, ')

				g.write('new ')

				g.write('byte($val)])')
			} else {
				g.expr(it.cond)
				if it.cond_type.is_ptr() {
					g.write('.valueOf()')
				}
				g.write('.entries()')
			}
		} else {
			g.write('for (const $val of ')
			g.expr(it.cond)
			if it.cond_type.is_ptr() {
				g.write('.valueOf()')
			}
			if it.kind == .string {
				g.write(".str.split('')")
			}
			// cast characters to bytes
			if val !in ['', '_'] && it.kind == .string {
				g.write('.map(c => ')

				g.write('new ')

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
		if it.cond_type.is_ptr() {
			g.write('.valueOf()')
		}
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
		g.write('+') // convert expr to number or boolean
		g.expr(it.cond)
	}
	g.writeln(') {')
	g.stmts(it.stmts)
	g.writeln('}')
}

fn (mut g JsGen) gen_go_expr(node ast.GoExpr) {
	// TODO Handle joinable expressions
	// node.is_expr
	mut name := g.js_name(node.call_expr.name)
	if node.call_expr.is_method {
		receiver_sym := g.table.get_type_symbol(node.call_expr.receiver_type)
		name = receiver_sym.name + '.' + name
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
	g.push_pub_var('/** @type $name */\n\t\t$name')
	g.writeln('function ${g.js_name(it.name)} (arg) { return arg; }')
}

fn (mut g JsGen) gen_optional_error(expr ast.Expr) {
	g.write('new Option({ state:  new byte(2),err: ')
	g.expr(expr)
	g.write('})')
}

fn (mut g JsGen) gen_return_stmt(it ast.Return) {
	node := it
	// sym := g.table.get_type_symbol(g.fn_decl.return_type)
	fn_return_is_optional := g.fn_decl.return_type.has_flag(.optional)
	if node.exprs.len == 0 {
		if fn_return_is_optional {
			g.writeln('return {}')
		} else {
			g.writeln('return;')
		}
		return
	}

	if fn_return_is_optional {
		optional_none := node.exprs[0] is ast.None
		ftyp := g.typ(node.types[0])
		mut is_regular_option := ftyp == 'Option'
		if optional_none || is_regular_option || node.types[0] == ast.error_type_idx {
			if !isnil(g.fn_decl) && g.fn_decl.is_test {
				test_error_var := g.new_tmp_var()
				g.writeln('let $test_error_var = "TODO";')
				g.writeln('return $test_error_var;')
				return
			}
			g.write('return ')
			g.gen_optional_error(it.exprs[0])
			g.writeln(';')
			return
		}
	}
	if fn_return_is_optional {
		tmp := g.new_tmp_var()
		g.write('const $tmp = new ')

		g.writeln('Option({});')
		g.write('${tmp}.data = ')
		if it.exprs.len == 1 {
			g.expr(it.exprs[0])
		} else { // Multi return
			g.gen_array_init_values(it.exprs)
		}
		g.writeln('')
		g.write('return $tmp;')
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
	for embed in node.embeds {
		etyp := g.typ(embed.typ)
		g.writeln('...${etyp}.prototype,')
	}
	fns := g.method_fn_decls[name]
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
		g.writeln('},')
	}
	for field in node.fields {
		typ := g.typ(field.typ)
		g.doc.gen_typ(typ)
		g.write('$field.name: ${g.to_js_typ_val(field.typ)}')
		g.writeln(',')
	}
	g.writeln('\$toJS() { return this; }')

	g.writeln('};\n')
	g.dec_indent()

	/*
	for cfn in fns {
		g.gen_method_decl(cfn, .struct_method)
	}*/

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
	g.write('new array(')
	g.inc_indent()

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
	} else if it.is_fixed && it.exprs.len == 1 {
		// [100]byte codegen
		t1 := g.new_tmp_var()
		t2 := g.new_tmp_var()
		g.writeln('(function() {')
		g.inc_indent()
		g.writeln('const $t1 = [];')
		g.write('for (let $t2 = 0; $t2 < ')
		g.expr(it.exprs[0])
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
	g.dec_indent()
	g.write(')')
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

fn (mut g JsGen) need_tmp_var_in_match(node ast.MatchExpr) bool {
	if node.is_expr && node.return_type != ast.void_type && node.return_type != 0 {
		sym := g.table.get_type_symbol(node.return_type)
		if sym.kind == .multi_return {
			return false
		}
		for branch in node.branches {
			if branch.stmts.len > 1 {
				return true
			}
			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if stmt.expr is ast.CallExpr || stmt.expr is ast.IfExpr
						|| stmt.expr is ast.MatchExpr || (stmt.expr is ast.IndexExpr
						&& (stmt.expr as ast.IndexExpr).or_expr.kind != .absent) {
						return true
					}
				}
			}
		}
	}
	return false
}

fn (mut g JsGen) match_expr_classic(node ast.MatchExpr, is_expr bool, cond_var MatchCond, tmp_var string) {
	type_sym := g.table.get_type_symbol(node.cond_type)
	for j, branch in node.branches {
		is_last := j == node.branches.len - 1
		if branch.is_else || (node.is_expr && is_last && tmp_var.len == 0) {
			if node.branches.len > 1 {
				if is_expr && tmp_var.len == 0 {
					// TODO too many branches. maybe separate ?: matches
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.writeln('else {')
				}
			}
		} else {
			if j > 0 {
				if is_expr && tmp_var.len == 0 {
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.write('else ')
				}
			}
			if is_expr && tmp_var.len == 0 {
				g.write('(')
			} else {
				if j == 0 {
					g.writeln('')
				}
				g.write_v_source_line_info(branch.pos)
				g.write('if (')
			}
			for i, expr in branch.exprs {
				if i > 0 {
					g.write(' || ')
				}
				match type_sym.kind {
					.array {
						g.write('vEq(')
						g.match_cond(cond_var)
						g.write(',')
						g.expr(expr)
						g.write(')')
					}
					.array_fixed {
						g.write('vEq(')
						g.match_cond(cond_var)
						g.write(',')
						g.expr(expr)
						g.write(')')
					}
					.map {
						g.write('vEq(')
						g.match_cond(cond_var)
						g.write(',')
						g.expr(expr)
						g.write(')')
					}
					.string {
						g.write('vEq(')
						g.match_cond(cond_var)
						g.write(',')
						g.expr(expr)
						g.write(')')
					}
					.struct_ {
						g.write('vEq(')
						g.match_cond(cond_var)
						g.write(',')
						g.expr(expr)
						g.write(')')
					}
					else {
						if expr is ast.RangeExpr {
							// if type is unsigned and low is 0, check is unneeded
							mut skip_low := false
							if expr.low is ast.IntegerLiteral {
								if node.cond_type in [ast.u16_type, ast.u32_type, ast.u64_type]
									&& expr.low.val == '0' {
									skip_low = true
								}
							}
							g.write('(')
							if !skip_low {
								g.match_cond(cond_var)
								g.write(' >= ')
								g.expr(expr.low)
								g.write(' && ')
							}
							g.match_cond(cond_var)
							g.write(' <= ')
							g.expr(expr.high)
							g.write(')')
						} else {
							g.write('vEq(')
							g.match_cond(cond_var)
							g.write(',')
							g.expr(expr)
							g.write(')')
						}
					}
				}
			}
			if is_expr && tmp_var.len == 0 {
				g.write(') ? ')
			} else {
				g.writeln(') {')
			}
		}
		g.stmts_with_tmp_var(branch.stmts, tmp_var)
		if !g.inside_ternary && node.branches.len >= 1 {
			g.write('}')
		}
	}
}

type MatchCond = CondExpr | CondString

struct CondString {
	s string
}

struct CondExpr {
	expr ast.Expr
}

fn (mut g JsGen) match_cond(cond MatchCond) {
	match cond {
		CondString {
			g.writeln(cond.s)
		}
		CondExpr {
			g.expr(cond.expr)
		}
	}
}

fn (mut g JsGen) match_expr(node ast.MatchExpr) {
	if node.cond_type == 0 {
		g.writeln('// match 0')
		return
	}
	prev := g.inside_ternary
	need_tmp_var := g.need_tmp_var_in_match(node)
	is_expr := (node.is_expr && node.return_type != ast.void_type) || g.inside_ternary
	mut cond_var := MatchCond(CondString{''})
	mut tmp_var := ''
	if is_expr && !need_tmp_var {
		g.inside_ternary = true
	}

	if node.cond is ast.Ident || node.cond is ast.SelectorExpr || node.cond is ast.IntegerLiteral
		|| node.cond is ast.StringLiteral || node.cond is ast.FloatLiteral
		|| node.cond is ast.CallExpr {
		cond_var = CondExpr{node.cond}
	} else {
		s := g.new_tmp_var()
		cond_var = CondString{s}
		g.write('let $s = ')
		g.expr(node.cond)
		g.writeln(';')
	}
	if need_tmp_var {
		tmp_var = g.new_tmp_var()
		g.writeln('let $tmp_var = undefined;')
	}
	if is_expr && !need_tmp_var {
		g.write('(')
	}
	if node.is_sum_type {
		g.match_expr_sumtype(node, is_expr, cond_var, tmp_var)
	} else {
		g.match_expr_classic(node, is_expr, cond_var, tmp_var)
	}
	if need_tmp_var {
		g.write('$tmp_var')
	}
	if is_expr && !need_tmp_var {
		g.write(')')
		g.inside_ternary = prev
	}
}

fn (mut g JsGen) stmts_with_tmp_var(stmts []ast.Stmt, tmp_var string) {
	g.inc_indent()
	if g.inside_ternary {
		g.write('(')
	}
	for i, stmt in stmts {
		if i == stmts.len - 1 && tmp_var != '' {
			if g.inside_if_optional {
				if stmt is ast.ExprStmt {
					if stmt.typ == ast.error_type_idx || stmt.expr is ast.None {
						g.writeln('${tmp_var}.state = 2;')
						g.write('${tmp_var}.err = ')
						g.expr(stmt.expr)
						g.writeln(';')
					} else {
						g.write('opt_ok(')
						g.stmt(stmt)
						g.writeln(', $tmp_var);')
					}
				}
			} else {
				g.write('$tmp_var = ')
				g.stmt(stmt)
				g.writeln('')
			}
		} else {
			g.stmt(stmt)
			if g.inside_if_optional && stmt is ast.ExprStmt {
				g.writeln(';')
			}
		}
		if g.inside_ternary && i < stmts.len - 1 {
			g.write(',')
		}
	}
	g.dec_indent()
	if g.inside_ternary {
		g.write(')')
	}
}

fn (mut g JsGen) match_expr_sumtype(node ast.MatchExpr, is_expr bool, cond_var MatchCond, tmp_var string) {
	for j, branch in node.branches {
		mut sumtype_index := 0
		for {
			is_last := j == node.branches.len - 1
			sym := g.table.get_type_symbol(node.cond_type)
			if branch.is_else || (node.is_expr && is_last && tmp_var.len == 0) {
				if is_expr && tmp_var.len == 0 {
					g.write(' : ')
				} else {
					g.writeln('')
					g.writeln('else {')
				}
			} else {
				if j > 0 || sumtype_index > 0 {
					if is_expr && tmp_var.len == 0 {
						g.write(' : ')
					} else {
						g.write('else ')
					}
				}

				if is_expr && tmp_var.len == 0 {
					g.write('(')
				} else {
					g.write('if (')
				}
				g.match_cond(cond_var)
				if sym.kind == .sum_type {
					g.write(' instanceof ')
					g.expr(branch.exprs[sumtype_index])
				} else if sym.kind == .interface_ {
					if branch.exprs[sumtype_index] is ast.TypeNode {
						g.write(' instanceof ')
						g.expr(branch.exprs[sumtype_index])
					} else {
						g.write(' instanceof ')

						g.write('None__')
					}
				}
				if is_expr && tmp_var.len == 0 {
					g.write(') ? ')
				} else {
					g.writeln(') {')
				}
			}
			g.stmts_with_tmp_var(branch.stmts, tmp_var)
			if !g.inside_ternary {
				g.writeln('}')
			}
			sumtype_index++
			if branch.exprs.len == 0 || sumtype_index == branch.exprs.len {
				break
			}
		}
	}
}

fn (mut g JsGen) need_tmp_var_in_if(node ast.IfExpr) bool {
	if node.is_expr && g.inside_ternary {
		if node.typ.has_flag(.optional) {
			return true
		}

		for branch in node.branches {
			if branch.cond is ast.IfGuardExpr || branch.stmts.len > 1 {
				return true
			}

			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if stmt.expr is ast.CallExpr {
						if stmt.expr.is_method {
							left_sym := g.table.get_type_symbol(stmt.expr.receiver_type)
							if left_sym.kind in [.array, .array_fixed, .map] {
								return true
							}
						}
					}
				}
			}
		}
	}
	return false
}

fn (mut g JsGen) gen_if_expr(node ast.IfExpr) {
	if node.is_comptime {
		g.comp_if(node)
		return
	}
	// For simpe if expressions we can use C's `?:`
	// `if x > 0 { 1 } else { 2 }` => `(x > 0) ? (1) : (2)`
	// For if expressions with multiple statements or another if expression inside, it's much
	// easier to use a temp var, than do C tricks with commas, introduce special vars etc
	// (as it used to be done).
	// Always use this in -autofree, since ?: can have tmp expressions that have to be freed.
	needs_tmp_var := g.need_tmp_var_in_if(node)
	tmp := if needs_tmp_var { g.new_tmp_var() } else { '' }

	if needs_tmp_var {
		if node.typ.has_flag(.optional) {
			g.inside_if_optional = true
		}

		g.writeln('let $tmp; /* if prepend */')
	} else if node.is_expr || g.inside_ternary {
		g.write('(')
		prev := g.inside_ternary
		g.inside_ternary = true
		for i, branch in node.branches {
			if i > 0 {
				g.write(' : ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				g.write('(')
				g.expr(branch.cond)
				g.write(').valueOf()')
				g.write(' ? ')
			}
			g.stmts(branch.stmts)
		}
		g.inside_ternary = prev
		g.write(')')
		return
	}

	mut is_guard := false
	mut guard_idx := 0
	mut guard_vars := []string{}

	for i, branch in node.branches {
		cond := branch.cond
		if cond is ast.IfGuardExpr {
			if !is_guard {
				is_guard = true
				guard_idx = i
				guard_vars = []string{len: node.branches.len}
			}
			if cond.expr !is ast.IndexExpr && cond.expr !is ast.PrefixExpr {
				var_name := g.new_tmp_var()
				guard_vars[i] = var_name
				g.writeln('let $var_name;')
			} else {
				guard_vars[i] = ''
			}
		}
	}

	for i, branch in node.branches {
		if i > 0 {
			g.write('} else ')
		}
		// if last branch is `else {`
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('{')
			// define `err` only for simple `if val := opt {...} else {`
			if is_guard && guard_idx == i - 1 {
				cvar_name := guard_vars[guard_idx]
				g.writeln('\tlet err = ${cvar_name}.err;')
			}
		} else {
			match branch.cond {
				ast.IfGuardExpr {
					mut var_name := guard_vars[i]
					mut short_opt := false
					if var_name == '' {
						short_opt = true // we don't need a further tmp, so use the one we'll get later
						var_name = g.new_tmp_var()
						guard_vars[i] = var_name // for `else`
						g.tmp_count--
						g.writeln('if (${var_name}.state == 0) {')
					} else {
						g.write('if ($var_name = ')
						g.expr(branch.cond.expr)
						g.writeln(', ${var_name}.state == 0) {')
					}
					if short_opt || branch.cond.var_name != '_' {
						if short_opt {
							cond_var_name := if branch.cond.var_name == '_' {
								'_dummy_${g.tmp_count + 1}'
							} else {
								branch.cond.var_name
							}
							g.write('\tlet $cond_var_name = ')
							g.expr(branch.cond.expr)
							g.writeln(';')
						} else {
							g.writeln('\tlet $branch.cond.var_name = ${var_name}.data;')
						}
					}
				}
				else {
					g.write('if ((')
					g.expr(branch.cond)
					g.writeln(').valueOf()) {')
				}
			}
		}
		if needs_tmp_var {
			g.stmts_with_tmp_var(branch.stmts, tmp)
		} else {
			g.stmts(branch.stmts)
		}
	}
	g.writeln('}')
	if needs_tmp_var {
		g.write('$tmp')
	}
	if node.typ.has_flag(.optional) {
		g.inside_if_optional = false
	}
}

fn (mut g JsGen) gen_index_expr(expr ast.IndexExpr) {
	left_typ := g.table.get_type_symbol(expr.left_type)
	// TODO: Handle splice setting if it's implemented
	if expr.index is ast.RangeExpr {
		if left_typ.kind == .array {
			g.write('array_slice(')
		} else {
			g.write('string_slice(')
		}
		g.expr(expr.left)
		if expr.left_type.is_ptr() {
			g.write('.valueOf()')
		}
		g.write(',')
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
			if expr.left_type.is_ptr() {
				g.write('.valueOf()')
			}
			g.write('.len')
		}
		g.write(')')
	} else if left_typ.kind == .map {
		g.expr(expr.left)
		if expr.is_setter {
			g.inside_map_set = true
			g.write('.map.set(')
		} else {
			g.write('.map.get(')
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
			// TODO: Maybe use u16 there? JS String returns values up to 2^16-1
			g.write('new byte(')
			g.expr(expr.left)
			if expr.left_type.is_ptr() {
				g.write('.valueOf()')
			}
			g.write('.str.charCodeAt(')
			g.expr(expr.index)
			g.write('))')
		}
	} else {
		// TODO Does this cover all cases?
		g.expr(expr.left)
		if expr.left_type.is_ptr() {
			g.write('.valueOf()')
		}
		g.write('.arr')
		g.write('[+')
		g.cast_stack << ast.int_type_idx
		g.expr(expr.index)
		g.cast_stack.delete_last()
		g.write(']')
	}
}

fn (mut g JsGen) gen_deref_ptr(ty ast.Type) {
	mut t := ty
	for t.is_ptr() {
		g.write('.val')
		t = t.deref()
	}
}

fn (mut g JsGen) gen_infix_expr(it ast.InfixExpr) {
	l_sym := g.table.get_final_type_symbol(it.left_type)
	r_sym := g.table.get_final_type_symbol(it.right_type)

	is_not := it.op in [.not_in, .not_is, .ne]
	if is_not {
		g.write('!(')
	}
	is_arithmetic := it.op in [token.Kind.plus, .minus, .mul, .div, .mod, .right_shift, .left_shift,
		.amp, .pipe, .xor]

	if is_arithmetic && ((l_sym.kind == .i64 || l_sym.kind == .u64)
		|| (r_sym.kind == .i64 || r_sym.kind == .u64)) {
		// if left or right is i64 or u64 we convert them to bigint to perform operation.
		greater_typ := if l_sym.kind == .i64 || l_sym.kind == .u64 {
			it.left_type
		} else {
			it.right_type
		} // g.greater_typ(it.left_type, it.right_type)
		g.write('new ')

		g.write('${g.typ(greater_typ)}(')
		g.cast_stack << greater_typ
		g.write('BigInt((')
		g.expr(it.left)
		g.gen_deref_ptr(it.left_type)
		g.write(').\$toJS())')
		g.write(' $it.op ')
		g.write('BigInt((')
		g.expr(it.right)
		g.gen_deref_ptr(it.right_type)
		g.write(').\$toJS())')
		g.cast_stack.delete_last()
		g.write(')')
		if is_not {
			g.write(')')
		}
		return
	}
	if it.op == .logical_or || it.op == .and {
		g.write('new bool(')
		g.expr(it.left)
		g.write('.valueOf()')
		g.write(it.op.str())
		g.expr(it.right)
		g.write('.valueOf()')
		g.write(')')
	} else if it.op == .eq || it.op == .ne {
		/*
		has_operator_overloading := g.table.type_has_method(l_sym, '==')
		if has_operator_overloading {
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			g.write('.eq(')
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.write(')')
			// Shallow equatables
		} else if l_sym.kind in js.shallow_equatables && r_sym.kind in js.shallow_equatables {
			// wrap left expr in parens so binary operations will work correctly.
			g.write('(')
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			g.write(')')
			g.write('.eq(')
			g.cast_stack << int(l_sym.kind)
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.cast_stack.delete_last()
			g.write(')')
		} else {
			g.write('vEq(')
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			g.write(', ')
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.write(')')
		}*/
		node := it
		left := g.unwrap(node.left_type)
		right := g.unwrap(node.right_type)
		has_operator_overloading := g.table.type_has_method(left.sym, '==')
		if has_operator_overloading
			|| (l_sym.kind in js.shallow_equatables && r_sym.kind in js.shallow_equatables) {
			if node.op == .ne {
				g.write('!')
			}
			g.write(g.typ(left.unaliased.set_nr_muls(0)))
			g.write('__eq(')
			g.expr(node.left)
			g.gen_deref_ptr(left.typ)
			g.write(',')
			g.expr(node.right)
			g.gen_deref_ptr(right.typ)
			g.write(')')
		} else {
			g.write('vEq(')
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			g.write(', ')
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.write(')')
		}
	} else if l_sym.kind == .array && it.op == .left_shift { // arr << 1
		g.write('Array.prototype.push.call(')
		g.expr(it.left)
		mut ltyp := it.left_type
		for ltyp.is_ptr() {
			g.write('.val')
			ltyp = ltyp.deref()
		}
		g.write('.arr,')
		array_info := l_sym.info as ast.Array
		// arr << [1, 2]
		if r_sym.kind == .array && array_info.elem_type != it.right_type {
			g.write('...')
		}
		g.expr(it.right)
		g.write(')')
	} else if r_sym.kind in [.array, .map, .string] && it.op in [.key_in, .not_in] {
		g.expr(it.right)

		mut ltyp := it.right_type
		for ltyp.is_ptr() {
			g.write('.val')
			ltyp = ltyp.deref()
		}
		if r_sym.kind == .map {
			g.write('.map.has(')
		} else if r_sym.kind == .string {
			g.write('.str.includes(')
		} else {
			g.write('.\$includes(')
		}
		g.expr(it.left)
		if l_sym.kind == .string {
			g.write('.str')
		}
		g.write(')')
	} else if it.op in [.key_is, .not_is] { // foo is Foo
		g.expr(it.left)
		g.gen_deref_ptr(it.left_type)
		g.write(' instanceof ')
		g.write(g.typ(it.right_type))
	} else if it.op in [.lt, .gt, .ge, .le] && g.table.type_has_method(l_sym, '<')
		&& l_sym.kind == r_sym.kind {
		if it.op in [.le, .ge] {
			g.write('!')
		}
		if it.op in [.lt, .ge] {
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			g.write('.\$lt (')
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.write(')')
		} else {
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.write('.\$lt (')
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			g.write(')')
		}
	} else {
		has_operator_overloading := g.table.type_has_method(l_sym, it.op.str())
		if has_operator_overloading {
			g.expr(it.left)
			g.gen_deref_ptr(it.left_type)
			name := match it.op.str() {
				'+' {
					'\$add'
				}
				'-' {
					'\$sub'
				}
				'/' {
					'\$div'
				}
				'*' {
					'\$mul'
				}
				'%' {
					'\$mod'
				}
				else {
					panic('unreachable')
					''
				}
			}
			g.write('.$name (')
			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			g.write(')')
		} else {
			mut greater_typ := 0
			// todo(playX): looks like this cast is always required to perform .eq operation on types.
			if is_arithmetic {
				greater_typ = g.greater_typ(it.left_type, it.right_type)
				if g.cast_stack.len > 0 {
					// needs_cast = g.cast_stack.last() != greater_typ
				}
			}

			if is_arithmetic {
				g.write('new ')

				g.write('${g.typ(greater_typ)}(')
				g.cast_stack << greater_typ
			}

			g.expr(it.left)

			g.gen_deref_ptr(it.left_type)
			// g.write('.val')
			g.write(' $it.op ')

			g.expr(it.right)
			g.gen_deref_ptr(it.right_type)
			// g.write('.val')

			if is_arithmetic {
				g.cast_stack.delete_last()
				g.write(')')
			}
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
		if ast.u64_type_idx in lr {
			return ast.Type(ast.u64_type_idx)
		}
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
	g.writeln('new map(')
	g.inc_indent()
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
	g.dec_indent()
	g.write(')')
}

fn (mut g JsGen) type_name(raw_type ast.Type) {
	typ := raw_type
	sym := g.table.get_type_symbol(typ)
	mut s := ''
	if sym.kind == .function {
		// todo: properly print function signatures
		if typ.is_ptr() {
			s = '&function'
		} else {
			s = 'function'
		}
	} else {
		s = g.table.type_to_str(g.unwrap_generic(typ))
	}
	g.write('new string("$s")')
}

fn (mut g JsGen) gen_selector_expr(it ast.SelectorExpr) {
	if it.name_type > 0 {
		node := it
		match node.gkind_field {
			.name {
				g.type_name(it.name_type)
				return
			}
			.typ {
				g.write('new int(')

				g.write('${int(g.unwrap_generic(it.name_type))}')
				g.write(')')
				g.write(')')
				return
			}
			.unknown {
				if node.field_name == 'name' {
					g.type_name(it.name_type)
					return
				} else if node.field_name == 'idx' {
					g.write('new int(')
					g.write('${int(g.unwrap_generic(it.name_type))}')
					g.write(')')
					return
				}
				panic('unknown generic field $it.pos')
			}
		}
	}
	g.expr(it.expr)
	mut ltyp := it.expr_type
	for ltyp.is_ptr() {
		g.write('.val')
		ltyp = ltyp.deref()
	}
	g.write('.$it.field_name')
}

fn (mut g JsGen) gen_string_inter_literal(it ast.StringInterLiteral) {
	should_cast := !(g.cast_stack.len > 0 && g.cast_stack.last() == ast.string_type_idx)
	if should_cast {
		g.write('new ')

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
	mut text := it.val.replace("'", "'")
	text = text.replace('"', '\\"')
	should_cast := !(g.cast_stack.len > 0 && g.cast_stack.last() == ast.string_type_idx)
	if true || should_cast {
		g.write('new ')

		g.write('string(')
	}
	if it.is_raw {
		g.writeln('(function() { let s = String(); ')
		for x in text {
			g.writeln('s += String.fromCharCode($x);')
		}
		g.writeln('return s; })()')
	} else {
		g.write("\"$text\"")
	}
	if true || should_cast {
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
	tsym := g.table.get_final_type_symbol(it.typ)
	if it.expr is ast.IntegerLiteral && (tsym.kind == .i64 || tsym.kind == .u64) {
		g.write('new ')

		g.write('$tsym.kind.str()')
		g.write('(BigInt(')
		g.write(it.expr.val)
		g.write('n))')
		return
	}
	if g.cast_stack.len > 0 && is_literal {
		if it.typ == g.cast_stack[g.cast_stack.len - 1] {
			g.expr(it.expr)
			return
		}
	}
	g.cast_stack << it.typ
	typ := g.typ(it.typ)
	if !is_literal {
		if it.typ.is_ptr() {
			g.write('new \$ref(')
		}

		g.write('new ')

		g.write('${typ}(')
	}
	g.expr(it.expr)
	if typ == 'string' && it.expr !is ast.StringLiteral {
		g.write('.toString()')
	}
	if !is_literal {
		g.write(')')
		if it.typ.is_ptr() {
			g.write(')')
		}
	}
	g.cast_stack.delete_last()
}

fn (mut g JsGen) gen_integer_literal_expr(it ast.IntegerLiteral) {
	typ := ast.Type(ast.int_type)

	// Don't wrap integers for use in JS.foo functions.
	// TODO: call.language always seems to be "v", parser bug?
	if g.call_stack.len > 0 {
		call := g.call_stack[g.call_stack.len - 1]
		if call.language == .js {
			for t in call.args {
				if t.expr is ast.IntegerLiteral {
					if t.expr == it {
						g.write(it.val)
						return
					}
				}
			}
		}
	}

	// Skip cast if type is the same as the parrent caster
	if g.cast_stack.len > 0 {
		if g.cast_stack[g.cast_stack.len - 1] in ast.integer_type_idxs {
			g.write('new ')

			g.write('int($it.val)')
			return
		}
	}
	g.write('new ')

	g.write('${g.typ(typ)}($it.val)')
}

fn (mut g JsGen) gen_float_literal_expr(it ast.FloatLiteral) {
	typ := ast.Type(ast.f32_type)

	// Don't wrap integers for use in JS.foo functions.
	// TODO: call.language always seems to be "v", parser bug?
	if g.call_stack.len > 0 {
		call := g.call_stack[g.call_stack.len - 1]
		if call.language == .js {
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
		}
	}

	// Skip cast if type is the same as the parrent caster
	if g.cast_stack.len > 0 {
		if g.cast_stack[g.cast_stack.len - 1] in ast.float_type_idxs {
			g.write('new f32($it.val)')
			return
		} else if g.cast_stack[g.cast_stack.len - 1] in ast.integer_type_idxs {
			g.write(int(it.val.f64()).str())
			return
		}
	}
	g.write('new ')

	g.write('${g.typ(typ)}($it.val)')
}

fn (mut g JsGen) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if t_typ := g.table.resolve_generic_to_concrete(typ, g.table.cur_fn.generic_names,
			g.table.cur_concrete_types)
		{
			return t_typ
		}
	}
	return typ
}

fn replace_op(s string) string {
	return match s {
		'+' { '_plus' }
		'-' { '_minus' }
		'*' { '_mult' }
		'/' { '_div' }
		'%' { '_mod' }
		'<' { '_lt' }
		'>' { '_gt' }
		'==' { '_eq' }
		else { '' }
	}
}
