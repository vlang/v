module main

import os
import time
import flag
import v.token
import v.parser
import v.ast
import v.pref
import v.errors
import strings

struct Context {
mut:
	is_watch   bool
	is_compile bool
	is_print   bool
}

struct HideFields {
mut:
	names map[string]bool
}

const hide_fields = &HideFields{}

fn main() {
	if os.args.len < 2 {
		eprintln('not enough parameters')
		exit(1)
	}
	mut ctx := Context{}
	mut fp := flag.new_flag_parser(os.args[2..])
	fp.application('v ast')
	fp.usage_example('demo.v       generate demo.json file.')
	fp.usage_example('-w demo.v    generate demo.json file, and watch for changes.')
	fp.usage_example('-c demo.v    generate demo.json *and* a demo.c file, and watch for changes.')
	fp.usage_example('-p demo.v    print the json output to stdout.')
	fp.description('Dump a JSON representation of the V AST for a given .v or .vsh file.')
	fp.description('By default, `v ast` will save the JSON to a .json file, named after the .v file.')
	fp.description('Pass -p to see it instead.')
	ctx.is_watch = fp.bool('watch', `w`, false, 'watch a .v file for changes, rewrite the .json file, when a change is detected')
	ctx.is_print = fp.bool('print', `p`, false, 'print the AST to stdout')
	ctx.is_compile = fp.bool('compile', `c`, false, 'watch the .v file for changes, rewrite the .json file, *AND* generate a .c file too on any change')
	hfields := fp.string_multi('hide', 0, 'hide the specified fields. You can give several, by separating them with `,`').join(',')
	mut mhf := unsafe { hide_fields }
	for hf in hfields.split(',') {
		mhf.names[hf] = true
	}
	fp.limit_free_args_to_at_least(1) ?
	rest_of_args := fp.remaining_parameters()
	for vfile in rest_of_args {
		file := get_abs_path(vfile)
		check_file(file)
		ctx.write_file_or_print(file)
		if ctx.is_watch || ctx.is_compile {
			ctx.watch_for_changes(file)
		}
	}
}

fn (ctx Context) write_file_or_print(file string) {
	if ctx.is_print {
		println(json(file))
	} else {
		println('$time.now(): AST written to: ' + json_file(file))
	}
}

// generate ast json file and c source code file
fn (ctx Context) watch_for_changes(file string) {
	println('start watching...')
	mut timestamp := i64(0)
	for {
		new_timestamp := os.file_last_mod_unix(file)
		if timestamp != new_timestamp {
			ctx.write_file_or_print(file)
			if ctx.is_compile {
				file_name := file[0..(file.len - os.file_ext(file).len)]
				os.system('v -o ${file_name}.c $file')
			}
		}
		timestamp = new_timestamp
		time.sleep(500 * time.millisecond)
	}
}

// get absolute path for file
fn get_abs_path(path string) string {
	if os.is_abs_path(path) {
		return path
	} else if path.starts_with('./') {
		return os.join_path(os.getwd(), path[2..])
	} else {
		return os.join_path(os.getwd(), path)
	}
}

// check file is v file and exists
fn check_file(file string) {
	if os.file_ext(file) !in ['.v', '.vv', '.vsh'] {
		eprintln('the file `$file` must be a v file or vsh file')
		exit(1)
	}
	if !os.exists(file) {
		eprintln('the v file `$file` does not exist')
		exit(1)
	}
}

// generate json file with the same file name
fn json_file(file string) string {
	ast_json := json(file)
	// support .v and .vsh file
	file_name := file[0..(file.len - os.file_ext(file).len)]
	json_file := file_name + '.json'
	os.write_file(json_file, ast_json) or { panic(err) }
	return json_file
}

// generate json string
fn json(file string) string {
	// use as permissive preferences as possible, so that `v ast`
	// can print the AST of arbitrary V files, even .vsh or ones
	// that require globals:
	mut pref := &pref.Preferences{}
	pref.fill_with_defaults()
	pref.enable_globals = true
	pref.is_fmt = true
	//
	mut t := Tree{
		root: new_object()
		table: ast.new_table()
		pref: pref
	}
	// parse file with comment
	ast_file := parser.parse_file(file, t.table, .parse_comments, t.pref)
	t.root = t.ast_file(ast_file)
	// generate the ast string
	s := json_print(t.root)
	return s
}

// the ast tree
struct Tree {
	table &ast.Table
	pref  &pref.Preferences
mut:
	root Node // the root of tree
}

// tree node
pub type Node = C.cJSON

// create an object node
[inline]
fn new_object() &Node {
	return C.cJSON_CreateObject()
}

// add item to object node
[inline]
fn (node &Node) add(key string, child &Node) {
	if hide_fields.names.len > 0 && key in hide_fields.names {
		return
	}
	add_item_to_object(node, key, child)
}

// create an array node
[inline]
fn new_array() &Node {
	return C.cJSON_CreateArray()
}

// add item to array node
[inline]
fn (node &Node) add_item(child &Node) {
	add_item_to_array(node, child)
}

// string type node
fn (t Tree) string_node(val string) &Node {
	return create_string(val)
}

// number type node
fn (t Tree) number_node(val int) &Node {
	return create_number(val)
}

// bool type node
fn (t Tree) bool_node(val bool) &Node {
	if val {
		return create_true()
	} else {
		return create_false()
	}
}

// null type node
fn (t Tree) null_node() &Node {
	return create_null()
}

// type node
fn (t Tree) type_node(typ ast.Type) &Node {
	if typ == 0 {
		return create_null()
	} else {
		type_name := t.table.get_type_name(typ)
		return create_string(strings.repeat(`&`, typ.nr_muls()) + type_name)
	}
}

// token type node
fn (t Tree) token_node(tok_kind token.Kind) &Node {
	return t.string_node('token:${int(tok_kind)}($tok_kind.str())')
}

// enum type node
fn (t Tree) enum_node<T>(value T) &Node {
	return t.string_node('enum:${int(value)}($value)')
}

// for [][]comment
fn (t Tree) two_dimension_comment(node [][]ast.Comment) &Node {
	mut comments := new_array()
	for n in node {
		mut comment_array := new_array()
		for c in n {
			comment_array.add_item(t.comment(c))
		}
		comments.add_item(comment_array)
	}
	return comments
}

// ast file root node
fn (t Tree) ast_file(node ast.File) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ast.File'))
	obj.add('path', t.string_node(node.path))
	obj.add('path_base', t.string_node(node.path_base))
	obj.add('nr_lines', t.number_node(node.nr_lines))
	obj.add('nr_bytes', t.number_node(node.nr_bytes))
	obj.add('mod', t.mod(node.mod))
	obj.add('imports', t.imports(node.imports))
	obj.add('global_scope', t.scope(node.global_scope))
	obj.add('scope', t.scope(node.scope))
	obj.add('errors', t.errors(node.errors))
	obj.add('warnings', t.warnings(node.warnings))
	obj.add('notices', t.notices(node.notices))
	obj.add('auto_imports', t.array_node_string(node.auto_imports))
	symbol_obj := new_object()
	for key, val in node.imported_symbols {
		symbol_obj.add(key, t.string_node(val))
	}
	obj.add('imported_symbols', symbol_obj)
	obj.add('generic_fns', t.array_node_generic_fns(node.generic_fns))
	obj.add('embedded_files', t.array_node_embed_file(node.embedded_files))
	obj.add('global_labels', t.array_node_string(node.global_labels))
	obj.add('is_test', t.bool_node(node.is_test))
	obj.add('stmts', t.stmts(node.stmts))
	return obj
}

// embed files
fn (t Tree) embed_file(node ast.EmbeddedFile) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('EmbeddedFile'))
	obj.add('rpath', t.string_node(node.rpath))
	obj.add('apath', t.string_node(node.apath))
	return obj
}

// ast module node
fn (t Tree) mod(node ast.Module) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Module'))
	obj.add('name', t.string_node(node.name))
	obj.add('short_name', t.string_node(node.short_name))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	obj.add('is_skipped', t.bool_node(node.is_skipped))
	return obj
}

fn (t Tree) scope(scope ast.Scope) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Scope'))
	obj.add('parent', t.string_node(ptr_str(scope.parent)))
	children_arr := new_array()
	for s in scope.children {
		mut children_obj := new_object()
		children_obj.add('parent', t.string_node(ptr_str(s.parent)))
		children_obj.add('start_pos', t.number_node(s.start_pos))
		children_obj.add('end_pos', t.number_node(s.end_pos))
		children_arr.add_item(children_obj)
	}
	obj.add('children', children_arr)
	obj.add('start_pos', t.number_node(scope.start_pos))
	obj.add('end_pos', t.number_node(scope.end_pos))
	obj.add('objects', t.objects(scope.objects))
	obj.add('struct_fields', t.array_node_scope_struct_field(scope.struct_fields))
	return obj
}

fn (t Tree) scope_struct_field(node ast.ScopeStructField) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ScopeStructField'))
	obj.add('struct_type', t.type_node(node.struct_type))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('orig_type', t.type_node(node.orig_type))
	obj.add('pos', t.position(node.pos))
	obj.add('smartcasts', t.array_node_type(node.smartcasts))
	return obj
}

fn (t Tree) objects(so map[string]ast.ScopeObject) &Node {
	mut obj := new_object()
	for key, val in so {
		obj.add(key, t.scope_object(val))
	}
	return obj
}

fn (t Tree) scope_object(node ast.ScopeObject) &Node {
	mut obj := new_object()
	match node {
		ast.ConstField { t.const_field(node) }
		ast.GlobalField { t.global_field(node) }
		ast.Var { t.var(node) }
		ast.AsmRegister { t.asm_register(node) }
	}
	return obj
}

fn (t Tree) imports(nodes []ast.Import) &Node {
	mut import_array := new_array()
	for node in nodes {
		import_array.add_item(t.import_module(node))
	}
	return import_array
}

fn (t Tree) errors(errors []errors.Error) &Node {
	mut errs := new_array()
	for e in errors {
		obj := new_object()
		obj.add('message', t.string_node(e.message))
		obj.add('file_path', t.string_node(e.file_path))
		obj.add('pos', t.position(e.pos))
		obj.add('backtrace', t.string_node(e.backtrace))
		obj.add('reporter', t.enum_node(e.reporter))
		errs.add_item(obj)
	}
	return errs
}

fn (t Tree) warnings(warnings []errors.Warning) &Node {
	mut warns := new_array()
	for w in warnings {
		mut obj := new_object()
		obj.add('message', t.string_node(w.message))
		obj.add('file_path', t.string_node(w.file_path))
		obj.add('pos', t.position(w.pos))
		obj.add('reporter', t.enum_node(w.reporter))
		warns.add_item(obj)
	}
	return warns
}

fn (t Tree) notices(notices []errors.Notice) &Node {
	mut notice_array := new_array()
	for n in notices {
		mut obj := new_object()
		obj.add('message', t.string_node(n.message))
		obj.add('file_path', t.string_node(n.file_path))
		obj.add('pos', t.position(n.pos))
		obj.add('reporter', t.enum_node(n.reporter))
		notice_array.add_item(obj)
	}
	return notice_array
}

// stmt array node
fn (t Tree) stmts(stmts []ast.Stmt) &Node {
	mut stmt_array := new_array()
	for s in stmts {
		stmt_array.add_item(t.stmt(s))
	}
	return stmt_array
}

fn (t Tree) stmt(node ast.Stmt) &Node {
	match node {
		ast.Module { return t.mod(node) }
		ast.Import { return t.import_module(node) }
		ast.ConstDecl { return t.const_decl(node) }
		ast.FnDecl { return t.fn_decl(node) }
		ast.StructDecl { return t.struct_decl(node) }
		ast.EnumDecl { return t.enum_decl(node) }
		ast.InterfaceDecl { return t.interface_decl(node) }
		ast.HashStmt { return t.hash_stmt(node) }
		ast.ComptimeFor { return t.comptime_for(node) }
		ast.GlobalDecl { return t.global_decl(node) }
		ast.DeferStmt { return t.defer_stmt(node) }
		ast.TypeDecl { return t.type_decl(node) }
		ast.GotoLabel { return t.goto_label(node) }
		ast.GotoStmt { return t.goto_stmt(node) }
		ast.AssignStmt { return t.assign_stmt(node) }
		ast.Return { return t.return_(node) }
		ast.ForCStmt { return t.for_c_stmt(node) }
		ast.ForStmt { return t.for_stmt(node) }
		ast.ForInStmt { return t.for_in_stmt(node) }
		ast.BranchStmt { return t.branch_stmt(node) }
		ast.AssertStmt { return t.assert_stmt(node) }
		ast.ExprStmt { return t.expr_stmt(node) }
		ast.Block { return t.block(node) }
		ast.SqlStmt { return t.sql_stmt(node) }
		ast.AsmStmt { return t.asm_stmt(node) }
		ast.NodeError { return t.node_error(node) }
		ast.EmptyStmt { return t.empty_stmt(node) }
	}
	return t.null_node()
}

fn (t Tree) import_module(node ast.Import) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Import'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('alias', t.string_node(node.alias))
	obj.add('syms', t.array_node_import_symbol(node.syms))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('pos', t.position(node.pos))
	obj.add('mod_pos', t.position(node.mod_pos))
	obj.add('alias_pos', t.position(node.alias_pos))
	obj.add('syms_pos', t.position(node.syms_pos))
	return obj
}

fn (t Tree) import_symbol(node ast.ImportSymbol) &Node {
	mut obj := new_object()
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) position(p token.Position) &Node {
	mut obj := new_object()
	obj.add('line_nr', t.number_node(p.line_nr))
	obj.add('last_line', t.number_node(p.last_line))
	obj.add('pos', t.number_node(p.pos))
	obj.add('len', t.number_node(p.len))
	return obj
}

fn (t Tree) comment(node ast.Comment) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Comment'))
	obj.add('text', t.string_node(node.text))
	obj.add('is_multi', t.bool_node(node.is_multi))
	obj.add('is_inline', t.bool_node(node.is_inline))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_decl(node ast.ConstDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ConstDecl'))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_block', t.bool_node(node.is_block))
	obj.add('fields', t.array_node_const_field(node.fields))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) const_field(node ast.ConstField) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ConstField'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('typ', t.type_node(node.typ))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('comptime_expr_value', t.comptime_expr_value(node.comptime_expr_value))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) comptime_expr_value(node ast.ComptTimeConstValue) &Node {
	match node {
		ast.EmptyExpr {
			return t.empty_expr(node)
		}
		string {
			return t.string_node(node)
		}
		else {
			return t.string_node(node.str())
		}
	}
}

// function declaration
fn (t Tree) fn_decl(node ast.FnDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('FnDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('mod', t.string_node(node.mod))
	obj.add('is_deprecated', t.bool_node(node.is_deprecated))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_variadic', t.bool_node(node.is_variadic))
	obj.add('is_anon', t.bool_node(node.is_anon))
	obj.add('is_noreturn', t.bool_node(node.is_noreturn))
	obj.add('is_manualfree', t.bool_node(node.is_manualfree))
	obj.add('is_main', t.bool_node(node.is_main))
	obj.add('is_test', t.bool_node(node.is_test))
	obj.add('is_conditional', t.bool_node(node.is_conditional))
	obj.add('is_exported', t.bool_node(node.is_exported))
	obj.add('is_keep_alive', t.bool_node(node.is_keep_alive))
	obj.add('is_unsafe', t.bool_node(node.is_unsafe))
	obj.add('receiver', t.struct_field(node.receiver))
	obj.add('receiver_pos', t.position(node.receiver_pos))
	obj.add('is_method', t.bool_node(node.is_method))
	obj.add('method_type_pos', t.position(node.method_type_pos))
	obj.add('method_idx', t.number_node(node.method_idx))
	obj.add('rec_mut', t.bool_node(node.rec_mut))
	obj.add('rec_share', t.enum_node(node.rec_share))
	obj.add('language', t.enum_node(node.language))
	obj.add('file_mode', t.enum_node(node.file_mode))
	obj.add('no_body', t.bool_node(node.no_body))
	obj.add('is_builtin', t.bool_node(node.is_builtin))
	obj.add('is_direct_arr', t.bool_node(node.is_direct_arr))
	obj.add('ctdefine_idx', t.number_node(node.ctdefine_idx))
	obj.add('pos', t.position(node.pos))
	obj.add('body_pos', t.position(node.body_pos))
	obj.add('return_type_pos', t.position(node.return_type_pos))
	obj.add('file', t.string_node(node.file))
	obj.add('has_return', t.bool_node(node.has_return))
	obj.add('should_be_skipped', t.bool_node(node.should_be_skipped))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('source_file', t.number_node(int(node.source_file)))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('params', t.array_node_arg(node.params))
	obj.add('generic_names', t.array_node_string(node.generic_names))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('label_names', t.array_node_string(node.label_names))
	obj.add('defer_stmts', t.array_node_defer_stmt(node.defer_stmts))
	return obj
}

fn (t Tree) anon_fn(node ast.AnonFn) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AnonFn'))
	obj.add('decl', t.fn_decl(node.decl))
	obj.add('inherited_vars', t.array_node_arg(node.inherited_vars))
	obj.add('typ', t.type_node(node.typ))
	obj.add('has_gen', t.bool_node(node.has_gen))
	return obj
}

fn (t Tree) struct_decl(node ast.StructDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StructDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('pub_pos', t.number_node(node.pub_pos))
	obj.add('mut_pos', t.number_node(node.mut_pos))
	obj.add('pub_mut_pos', t.number_node(node.pub_mut_pos))
	obj.add('global_pos', t.number_node(node.global_pos))
	obj.add('module_pos', t.number_node(node.module_pos))
	obj.add('language', t.enum_node(node.language))
	obj.add('is_union', t.bool_node(node.is_union))
	obj.add('pos', t.position(node.pos))
	obj.add('fields', t.array_node_struct_field(node.fields))
	obj.add('generic_types', t.array_node_type(node.generic_types))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	obj.add('embeds', t.array_node_embed(node.embeds))
	return obj
}

fn (t Tree) struct_field(node ast.StructField) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StructField'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('type_pos', t.position(node.type_pos))
	obj.add('has_default_expr', t.bool_node(node.has_default_expr))
	obj.add('default_expr_typ', t.type_node(node.default_expr_typ))
	obj.add('default_expr', t.expr(node.default_expr))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('is_global', t.bool_node(node.is_global))
	obj.add('is_volatile', t.bool_node(node.is_volatile))
	obj.add('attrs', t.array_node_attr(node.attrs))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) embed(node ast.Embed) &Node {
	mut obj := new_object()
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) enum_decl(node ast.EnumDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('EnumDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('is_flag', t.bool_node(node.is_flag))
	obj.add('is_multi_allowed', t.bool_node(node.is_multi_allowed))
	obj.add('pos', t.position(node.pos))
	obj.add('fields', t.array_node_enum_field(node.fields))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('attrs', t.array_node_attr(node.attrs))
	return obj
}

fn (t Tree) enum_field(node ast.EnumField) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('EnumField'))
	obj.add('name', t.string_node(node.name))
	obj.add('has_expr', t.bool_node(node.has_expr))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	return obj
}

fn (t Tree) interface_decl(node ast.InterfaceDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('InterfaceDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('mut_pos', t.number_node(node.mut_pos))
	obj.add('field_names', t.array_node_string(node.field_names))
	obj.add('methods', t.array_node_fn_decl(node.methods))
	obj.add('fields', t.array_node_struct_field(node.fields))
	obj.add('pre_comments', t.array_node_comment(node.pre_comments))
	obj.add('name_pos', t.position(node.name_pos))
	obj.add('language', t.enum_node(node.language))
	obj.add('pos', t.position(node.pos))
	obj.add('are_ifaces_expanded', t.bool_node(node.are_ifaces_expanded))
	obj.add('ifaces', t.array_node_interface_embedding(node.ifaces))
	return obj
}

fn (t Tree) interface_embedding(node ast.InterfaceEmbedding) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('InterfaceEmbedding'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) attr(node ast.Attr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Attr'))
	obj.add('name', t.string_node(node.name))
	obj.add('has_arg', t.bool_node(node.has_arg))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('ct_expr', t.expr(node.ct_expr))
	obj.add('ct_opt', t.bool_node(node.ct_opt))
	obj.add('ct_evaled', t.bool_node(node.ct_evaled))
	obj.add('ct_skip', t.bool_node(node.ct_skip))
	obj.add('arg', t.string_node(node.arg))
	return obj
}

fn (t Tree) hash_stmt(node ast.HashStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('HashStmt'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('val', t.string_node(node.val))
	obj.add('kind', t.string_node(node.kind))
	obj.add('main', t.string_node(node.main))
	obj.add('msg', t.string_node(node.msg))
	obj.add('ct_conds', t.array_node_expr(node.ct_conds))
	obj.add('source_file', t.string_node(node.source_file))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) comptime_for(node ast.ComptimeFor) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ComptimeFor'))
	obj.add('val_var', t.string_node(node.val_var))
	obj.add('typ', t.type_node(node.typ))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('pos', t.position(node.pos))
	obj.add('typ_pos', t.position(node.pos))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) global_decl(node ast.GlobalDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('GlobalDecl'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('pos', t.position(node.pos))
	obj.add('is_block', t.bool_node(node.is_block))
	obj.add('fields', t.array_node_global_field(node.fields))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	return obj
}

fn (t Tree) global_field(node ast.GlobalField) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('GlobalField'))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('has_expr', t.bool_node(node.has_expr))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	obj.add('typ_pos', t.position(node.typ_pos))
	return obj
}

fn (t Tree) defer_stmt(node ast.DeferStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('DeferStmt'))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('defer_vars', t.array_node_ident(node.defer_vars))
	obj.add('ifdef', t.string_node(node.ifdef))
	obj.add('idx_in_fn', t.number_node(node.idx_in_fn))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_decl(node ast.TypeDecl) &Node {
	match node {
		ast.AliasTypeDecl { return t.alias_type_decl(node) }
		ast.FnTypeDecl { return t.fn_type_decl(node) }
		ast.SumTypeDecl { return t.sum_type_decl(node) }
	}
}

fn (t Tree) alias_type_decl(node ast.AliasTypeDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AliasTypeDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('parent_type', t.type_node(node.parent_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) sum_type_decl(node ast.SumTypeDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SumTypeDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('pos', t.position(node.pos))
	obj.add('typ', t.type_node(node.typ))
	obj.add('generic_types', t.array_node_type(node.generic_types))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('variants', t.array_node_type_expr(node.variants))
	return obj
}

fn (t Tree) fn_type_decl(node ast.FnTypeDecl) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('FnTypeDecl'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_pub', t.bool_node(node.is_pub))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) arg(node ast.Param) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Param'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_mut', t.bool_node(node.is_mut))
	return obj
}

fn (t Tree) goto_label(node ast.GotoLabel) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('GotoLabel'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) goto_stmt(node ast.GotoStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('GotoStmt'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) assign_stmt(node ast.AssignStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AssignStmt'))
	obj.add('left', t.array_node_expr(node.left))
	obj.add('left_types', t.array_node_type(node.left_types))
	obj.add('right', t.array_node_expr(node.right))
	obj.add('right_types', t.array_node_type(node.left_types))
	obj.add('op', t.token_node(node.op))
	obj.add('is_static', t.bool_node(node.is_static))
	obj.add('is_volatile', t.bool_node(node.is_volatile))
	obj.add('is_simple', t.bool_node(node.is_simple))
	obj.add('has_cross_var', t.bool_node(node.has_cross_var))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('end_comments', t.array_node_comment(node.end_comments))
	return obj
}

fn (t Tree) var(node ast.Var) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Var'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('orig_type', t.type_node(node.orig_type))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_arg', t.bool_node(node.is_arg))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('is_used', t.bool_node(node.is_used))
	obj.add('is_changed', t.bool_node(node.is_changed))
	obj.add('is_or', t.bool_node(node.is_or))
	obj.add('is_tmp', t.bool_node(node.is_tmp))
	obj.add('is_autofree_tmp', t.bool_node(node.is_autofree_tmp))
	obj.add('is_auto_deref', t.bool_node(node.is_auto_deref))
	obj.add('is_inherited', t.bool_node(node.is_inherited))
	obj.add('is_auto_heap', t.bool_node(node.is_auto_heap))
	obj.add('is_stack_obj', t.bool_node(node.is_stack_obj))
	obj.add('share', t.enum_node(node.share))
	obj.add('pos', t.position(node.pos))
	obj.add('smartcasts', t.array_node_type(node.smartcasts))
	return obj
}

fn (t Tree) return_(node ast.Return) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Return'))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('types', t.array_node_type(node.types))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) for_c_stmt(node ast.ForCStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ForCStmt'))
	obj.add('init', t.stmt(node.init))
	obj.add('has_init', t.bool_node(node.has_init))
	obj.add('cond', t.expr(node.cond))
	obj.add('has_cond', t.bool_node(node.has_cond))
	obj.add('inc', t.stmt(node.inc))
	obj.add('has_inc', t.bool_node(node.has_inc))
	obj.add('is_multi', t.bool_node(node.is_multi))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) for_stmt(node ast.ForStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ForStmt'))
	obj.add('cond', t.expr(node.cond))
	obj.add('is_inf', t.bool_node(node.is_inf))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) for_in_stmt(node ast.ForInStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ForInStmt'))
	obj.add('key_var', t.string_node(node.key_var))
	obj.add('val_var', t.string_node(node.val_var))
	obj.add('cond', t.expr(node.cond))
	obj.add('is_range', t.bool_node(node.is_range))
	obj.add('high', t.expr(node.high))
	obj.add('key_type', t.type_node(node.key_type))
	obj.add('val_type', t.type_node(node.val_type))
	obj.add('cond_type', t.type_node(node.cond_type))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('val_is_mut', t.bool_node(node.val_is_mut))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	return obj
}

fn (t Tree) branch_stmt(node ast.BranchStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('BranchStmt'))
	obj.add('kind', t.token_node(node.kind))
	obj.add('label', t.string_node(node.label))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) assert_stmt(node ast.AssertStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AssertStmt'))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_used', t.bool_node(node.is_used))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) block(node ast.Block) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Block'))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('is_unsafe', t.bool_node(node.is_unsafe))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) comptime_call(node ast.ComptimeCall) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ComptimeCall'))
	obj.add('method_name', t.string_node(node.method_name))
	obj.add('left', t.expr(node.left))
	obj.add('is_vweb', t.bool_node(node.is_vweb))
	obj.add('vweb_tmpl', t.string_node(node.vweb_tmpl.path))
	obj.add('args_var', t.string_node(node.args_var))
	obj.add('sym', t.string_node(node.sym.name))
	obj.add('has_parens', t.bool_node(node.has_parens))
	obj.add('is_embed', t.bool_node(node.is_embed))
	obj.add('embed_file', t.embed_file(node.embed_file))
	obj.add('method_pos', t.position(node.method_pos))
	obj.add('result_type', t.type_node(node.result_type))
	obj.add('scope', t.scope(node.scope))
	obj.add('env_value', t.string_node(node.env_value))
	obj.add('pos', t.position(node.pos))
	obj.add('args', t.array_node_call_arg(node.args))
	return obj
}

fn (t Tree) comptime_selector(node ast.ComptimeSelector) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ComptimeSelector'))
	obj.add('has_parens', t.bool_node(node.has_parens))
	obj.add('left', t.expr(node.left))
	obj.add('field_expr', t.expr(node.field_expr))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) expr_stmt(node ast.ExprStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ExprStmt'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

// expr
fn (t Tree) expr(expr ast.Expr) &Node {
	match expr {
		ast.IntegerLiteral {
			return t.integer_literal(expr)
		}
		ast.FloatLiteral {
			return t.float_literal(expr)
		}
		ast.StringLiteral {
			return t.string_literal(expr)
		}
		ast.CharLiteral {
			return t.char_literal(expr)
		}
		ast.BoolLiteral {
			return t.bool_literal(expr)
		}
		ast.StringInterLiteral {
			return t.string_inter_literal(expr)
		}
		ast.EnumVal {
			return t.enum_val(expr)
		}
		ast.Assoc {
			return t.assoc(expr)
		}
		ast.AtExpr {
			return t.at_expr(expr)
		}
		ast.CastExpr {
			return t.cast_expr(expr)
		}
		ast.AsCast {
			return t.as_cast(expr)
		}
		ast.TypeNode {
			return t.type_expr(expr)
		}
		ast.SizeOf {
			return t.size_of(expr)
		}
		ast.IsRefType {
			return t.is_ref_type(expr)
		}
		ast.PrefixExpr {
			return t.prefix_expr(expr)
		}
		ast.InfixExpr {
			return t.infix_expr(expr)
		}
		ast.IndexExpr {
			return t.index_expr(expr)
		}
		ast.PostfixExpr {
			return t.postfix_expr(expr)
		}
		ast.SelectorExpr {
			return t.selector_expr(expr)
		}
		ast.RangeExpr {
			return t.range_expr(expr)
		}
		ast.IfExpr {
			return t.if_expr(expr)
		}
		ast.Ident {
			return t.ident(expr)
		}
		ast.CallExpr {
			return t.call_expr(expr)
		}
		ast.OrExpr {
			return t.or_expr(expr)
		}
		ast.StructInit {
			return t.struct_init(expr)
		}
		ast.ArrayInit {
			return t.array_init(expr)
		}
		ast.MapInit {
			return t.map_init(expr)
		}
		ast.None {
			return t.none_expr(expr)
		}
		ast.ParExpr {
			return t.par_expr(expr)
		}
		ast.IfGuardExpr {
			return t.if_guard_expr(expr)
		}
		ast.MatchExpr {
			return t.match_expr(expr)
		}
		ast.ConcatExpr {
			return t.concat_expr(expr)
		}
		ast.TypeOf {
			return t.type_of(expr)
		}
		ast.Likely {
			return t.likely(expr)
		}
		ast.SqlExpr {
			return t.sql_expr(expr)
		}
		ast.ComptimeCall {
			return t.comptime_call(expr)
		}
		ast.ComptimeSelector {
			return t.comptime_selector(expr)
		}
		ast.LockExpr {
			return t.lock_expr(expr)
		}
		ast.UnsafeExpr {
			return t.unsafe_expr(expr)
		}
		ast.ChanInit {
			return t.chan_init(expr)
		}
		ast.SelectExpr {
			return t.select_expr(expr)
		}
		ast.Comment {
			return t.comment(expr)
		}
		ast.AnonFn {
			return t.anon_fn(expr)
		}
		ast.ArrayDecompose {
			return t.array_decompose(expr)
		}
		ast.GoExpr {
			return t.go_expr(expr)
		}
		ast.OffsetOf {
			return t.offset_of(expr)
		}
		ast.DumpExpr {
			return t.dump_expr(expr)
		}
		ast.NodeError {
			return t.node_error(expr)
		}
		ast.EmptyExpr {
			return t.empty_expr(expr)
		}
		else {
			// println('unknown expr')
			return t.null_node()
		}
	}
}

fn (t Tree) integer_literal(node ast.IntegerLiteral) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IntegerLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) float_literal(node ast.FloatLiteral) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('FloatLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) string_literal(node ast.StringLiteral) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StringLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('is_raw', t.bool_node(node.is_raw))
	obj.add('language', t.enum_node(node.language))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) char_literal(node ast.CharLiteral) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('CharLiteral'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) bool_literal(node ast.BoolLiteral) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('BoolLiteral'))
	obj.add('val', t.bool_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) string_inter_literal(node ast.StringInterLiteral) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StringInterLiteral'))
	obj.add('vals', t.array_node_string(node.vals))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('expr_types', t.array_node_type(node.expr_types))
	obj.add('fwidths', t.array_node_int(node.fwidths))
	obj.add('precisions', t.array_node_int(node.precisions))
	obj.add('pluss', t.array_node_bool(node.pluss))
	obj.add('fills', t.array_node_bool(node.fills))
	obj.add('fmt_poss', t.array_node_position(node.fmt_poss))
	obj.add('fmts', t.array_node_byte(node.fmts))
	obj.add('need_fmts', t.array_node_bool(node.need_fmts))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) enum_val(node ast.EnumVal) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('EnumVal'))
	obj.add('enum_name', t.string_node(node.enum_name))
	obj.add('mod', t.string_node(node.mod))
	obj.add('val', t.string_node(node.val))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) assoc(node ast.Assoc) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Assoc'))
	obj.add('var_name', t.string_node(node.var_name))
	obj.add('fields', t.array_node_string(node.fields))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) at_expr(node ast.AtExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AtExpr'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('val', t.string_node(node.val))
	return obj
}

fn (t Tree) cast_expr(node ast.CastExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('CastExpr'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('ityp', t.number_node(int(node.typ)))
	obj.add('typname', t.string_node(node.typname))
	obj.add('has_arg', t.bool_node(node.has_arg))
	obj.add('arg', t.expr(node.arg))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) as_cast(node ast.AsCast) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsCast'))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_expr(node ast.TypeNode) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('TypeNode'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) size_of(node ast.SizeOf) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SizeOf'))
	obj.add('is_type', t.bool_node(node.is_type))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) is_ref_type(node ast.IsRefType) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IsRefType'))
	obj.add('is_type', t.bool_node(node.is_type))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) prefix_expr(node ast.PrefixExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('PrefixExpr'))
	obj.add('op', t.token_node(node.op))
	obj.add('right', t.expr(node.right))
	obj.add('right_type', t.type_node(node.right_type))
	obj.add('or_block', t.or_expr(node.or_block))
	obj.add('is_option', t.bool_node(node.is_option))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) infix_expr(node ast.InfixExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('InfixExpr'))
	obj.add('op', t.token_node(node.op))
	obj.add('left', t.expr(node.left))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('right', t.expr(node.right))
	obj.add('right_type', t.type_node(node.right_type))
	obj.add('auto_locked', t.string_node(node.auto_locked))
	obj.add('or_block', t.or_expr(node.or_block))
	obj.add('is_stmt', t.bool_node(node.is_stmt))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) index_expr(node ast.IndexExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IndexExpr'))
	obj.add('left', t.expr(node.left))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('index', t.expr(node.index))
	obj.add('is_setter', t.bool_node(node.is_setter))
	obj.add('or_expr', t.or_expr(node.or_expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) postfix_expr(node ast.PostfixExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('PostfixExpr'))
	obj.add('op', t.token_node(node.op))
	obj.add('expr', t.expr(node.expr))
	obj.add('auto_locked', t.string_node(node.auto_locked))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) selector_expr(node ast.SelectorExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SelectorExpr'))
	obj.add('expr', t.expr(node.expr))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('field_name', t.string_node(node.field_name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('name_type', t.type_node(node.name_type))
	obj.add('gkind_field', t.enum_node(node.gkind_field))
	obj.add('from_embed_types', t.array_node_type(node.from_embed_types))
	obj.add('next_token', t.token_node(node.next_token))
	obj.add('pos', t.position(node.pos))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) range_expr(node ast.RangeExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('RangeExpr'))
	obj.add('low', t.expr(node.low))
	obj.add('high', t.expr(node.high))
	obj.add('has_high', t.bool_node(node.has_high))
	obj.add('has_low', t.bool_node(node.has_low))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) if_expr(node ast.IfExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IfExpr'))
	obj.add('is_comptime', t.bool_node(node.is_comptime))
	obj.add('tok_kind', t.token_node(node.tok_kind))
	obj.add('branches', t.array_node_if_branch(node.branches))
	obj.add('left', t.expr(node.left))
	obj.add('typ', t.type_node(node.typ))
	obj.add('has_else', t.bool_node(node.has_else))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('pos', t.position(node.pos))
	obj.add('post_comments', t.array_node_comment(node.post_comments))
	return obj
}

fn (t Tree) if_branch(node ast.IfBranch) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IfBranch'))
	obj.add('cond', t.expr(node.cond))
	obj.add('pos', t.position(node.pos))
	obj.add('body_pos', t.position(node.body_pos))
	obj.add('pkg_exist', t.bool_node(node.pkg_exist))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) ident(node ast.Ident) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Ident'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('name', t.string_node(node.name))
	obj.add('language', t.enum_node(node.language))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('comptime', t.bool_node(node.comptime))
	obj.add('tok_kind', t.token_node(node.tok_kind))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('info', t.ident_info(node.info))
	obj.add('pos', t.position(node.pos))
	obj.add('mut_pos', t.position(node.mut_pos))
	obj.add('obj', t.scope_object(node.obj))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) ident_info(node ast.IdentInfo) &Node {
	match node {
		ast.IdentVar { return t.ident_var(node) }
		ast.IdentFn { return t.ident_fn(node) }
	}
}

fn (t Tree) ident_var(node ast.IdentVar) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IdentVar'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('is_static', t.bool_node(node.is_static))
	obj.add('is_volatile', t.bool_node(node.is_volatile))
	obj.add('is_optional', t.bool_node(node.is_optional))
	obj.add('share', t.enum_node(node.share))
	return obj
}

fn (t Tree) ident_fn(node ast.IdentFn) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IdentFn'))
	obj.add('typ', t.type_node(node.typ))
	return obj
}

fn (t Tree) call_expr(node ast.CallExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('CallExpr'))
	obj.add('mod', t.string_node(node.mod))
	obj.add('name', t.string_node(node.name))
	obj.add('language', t.enum_node(node.language))
	obj.add('left_type', t.type_node(node.left_type))
	obj.add('receiver_type', t.type_node(node.receiver_type))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('left', t.expr(node.left))
	obj.add('is_method', t.bool_node(node.is_method))
	obj.add('is_keep_alive', t.bool_node(node.is_keep_alive))
	obj.add('is_noreturn', t.bool_node(node.is_noreturn))
	obj.add('should_be_skipped', t.bool_node(node.should_be_skipped))
	obj.add('free_receiver', t.bool_node(node.free_receiver))
	obj.add('scope', t.number_node(int(node.scope)))
	obj.add('args', t.array_node_call_arg(node.args))
	obj.add('expected_arg_types', t.array_node_type(node.expected_arg_types))
	obj.add('concrete_types', t.array_node_type(node.concrete_types))
	obj.add('or_block', t.or_expr(node.or_block))
	obj.add('concrete_list_pos', t.position(node.concrete_list_pos))
	obj.add('from_embed_type', t.type_node(node.from_embed_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	return obj
}

fn (t Tree) call_arg(node ast.CallArg) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('CallArg'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_mut', t.bool_node(node.is_mut))
	obj.add('share', t.enum_node(node.share))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_tmp_autofree', t.bool_node(node.is_tmp_autofree))
	obj.add('pos', t.position(node.pos))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) or_expr(node ast.OrExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('OrExpr'))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) struct_init(node ast.StructInit) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StructInit'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('is_short', t.bool_node(node.is_short))
	obj.add('unresolved', t.bool_node(node.unresolved))
	obj.add('has_update_expr', t.bool_node(node.has_update_expr))
	obj.add('update_expr', t.expr(node.update_expr))
	obj.add('update_expr_type', t.type_node(node.update_expr_type))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	obj.add('update_expr_comments', t.array_node_comment(node.update_expr_comments))
	obj.add('fields', t.array_node_struct_init_field(node.fields))
	obj.add('embeds', t.array_node_struct_init_embed(node.embeds))
	obj.add('pre_comments', t.array_node_comment(node.pre_comments))
	return obj
}

fn (t Tree) struct_init_field(node ast.StructInitField) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StructInitField'))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expected_type', t.type_node(node.expected_type))
	obj.add('parent_type', t.type_node(node.parent_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('pos', t.position(node.pos))
	obj.add('name_pos', t.position(node.name_pos))
	return obj
}

fn (t Tree) struct_init_embed(node ast.StructInitEmbed) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('StructInitEmbed'))
	obj.add('name', t.string_node(node.name))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('expected_type', t.type_node(node.expected_type))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('next_comments', t.array_node_comment(node.next_comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) array_init(node ast.ArrayInit) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ArrayInit'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('elem_type', t.type_node(node.elem_type))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('ecmnts', t.two_dimension_comment(node.ecmnts))
	obj.add('pre_cmnts', t.array_node_comment(node.pre_cmnts))
	obj.add('elem_type_pos', t.position(node.elem_type_pos))
	obj.add('is_fixed', t.bool_node(node.is_fixed))
	obj.add('has_val', t.bool_node(node.has_val))
	obj.add('mod', t.string_node(node.mod))
	obj.add('len_expr', t.expr(node.len_expr))
	obj.add('cap_expr', t.expr(node.cap_expr))
	obj.add('default_expr', t.expr(node.default_expr))
	obj.add('has_len', t.bool_node(node.has_len))
	obj.add('has_cap', t.bool_node(node.has_cap))
	obj.add('has_default', t.bool_node(node.has_default))
	obj.add('has_it', t.bool_node(node.has_it))
	obj.add('expr_types', t.array_node_type(node.expr_types))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) map_init(node ast.MapInit) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('MapInit'))
	obj.add('typ', t.type_node(node.typ))
	obj.add('key_type', t.type_node(node.key_type))
	obj.add('value_type', t.type_node(node.value_type))
	obj.add('keys', t.array_node_expr(node.keys))
	obj.add('vals', t.array_node_expr(node.vals))
	obj.add('comments', t.two_dimension_comment(node.comments))
	obj.add('pre_cmnts', t.array_node_comment(node.pre_cmnts))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) none_expr(node ast.None) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('None'))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) par_expr(node ast.ParExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ParExpr'))
	obj.add('expr', t.expr(node.expr))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) if_guard_expr(node ast.IfGuardExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('IfGuardExpr'))
	obj.add('var_name', t.string_node(node.var_name))
	obj.add('expr', t.expr(node.expr))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) match_expr(node ast.MatchExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('MatchExpr'))
	obj.add('tok_kind', t.token_node(node.tok_kind))
	obj.add('cond', t.expr(node.cond))
	obj.add('cond_type', t.type_node(node.cond_type))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('expected_type', t.type_node(node.expected_type))
	obj.add('is_sum_type', t.bool_node(node.is_sum_type))
	obj.add('is_expr', t.bool_node(node.is_expr))
	obj.add('pos', t.position(node.pos))
	obj.add('branches', t.array_node_match_branch(node.branches))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) match_branch(node ast.MatchBranch) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('MatchBranch'))
	obj.add('ecmnts', t.two_dimension_comment(node.ecmnts))
	obj.add('stmts', t.array_node_stmt(node.stmts))
	obj.add('is_else', t.bool_node(node.is_else))
	obj.add('pos', t.position(node.pos))
	obj.add('post_comments', t.array_node_comment(node.post_comments))
	obj.add('branch_pos', t.position(node.branch_pos))
	obj.add('exprs', t.array_node_expr(node.exprs))
	obj.add('scope', t.number_node(int(node.scope)))
	return obj
}

fn (t Tree) concat_expr(node ast.ConcatExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ConcatExpr'))
	obj.add('vals', t.array_node_expr(node.vals))
	obj.add('return_type', t.type_node(node.return_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) type_of(node ast.TypeOf) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('TypeOf'))
	obj.add('expr', t.expr(node.expr))
	obj.add('expr_type', t.type_node(node.expr_type))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) likely(node ast.Likely) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('Likely'))
	obj.add('expr', t.expr(node.expr))
	obj.add('is_likely', t.bool_node(node.is_likely))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) sql_expr(node ast.SqlExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SqlExpr'))
	obj.add('type', t.type_node(node.typ))
	obj.add('is_count', t.bool_node(node.is_count))
	obj.add('db_expr', t.expr(node.db_expr))
	obj.add('table_expr', t.type_expr(node.table_expr))
	obj.add('has_where', t.bool_node(node.has_where))
	obj.add('where_expr', t.expr(node.where_expr))
	obj.add('has_order', t.bool_node(node.has_order))
	obj.add('order_expr', t.expr(node.order_expr))
	obj.add('has_desc', t.bool_node(node.has_desc))
	obj.add('is_array', t.bool_node(node.is_array))
	obj.add('pos', t.position(node.pos))
	obj.add('has_limit', t.bool_node(node.has_limit))
	obj.add('limit_expr', t.expr(node.limit_expr))
	obj.add('has_offset', t.bool_node(node.has_offset))
	obj.add('offset_expr', t.expr(node.offset_expr))
	obj.add('fields', t.array_node_struct_field(node.fields))
	sub_struct_map := new_object()
	for key, val in node.sub_structs {
		sub_struct_map.add(key.str(), t.sql_expr(val))
	}
	obj.add('sub_structs', sub_struct_map)
	return obj
}

fn (t Tree) sql_stmt(node ast.SqlStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SqlStmt'))
	obj.add('db_expr', t.expr(node.db_expr))
	obj.add('pos', t.position(node.pos))
	obj.add('lines', t.array_node_sql_stmt_line(node.lines))
	return obj
}

fn (t Tree) sql_stmt_line(node ast.SqlStmtLine) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SqlStmtLine'))
	obj.add('kind', t.enum_node(node.kind))
	obj.add('table_expr', t.type_expr(node.table_expr))
	obj.add('object_var_name', t.string_node(node.object_var_name))
	obj.add('where_expr', t.expr(node.where_expr))
	obj.add('fields', t.array_node_struct_field(node.fields))
	obj.add('updated_columns', t.array_node_string(node.updated_columns))
	obj.add('update_exprs', t.array_node_expr(node.update_exprs))
	obj.add('pos', t.position(node.pos))

	sub_struct_map := new_object()
	for key, val in node.sub_structs {
		sub_struct_map.add(key.str(), t.sql_stmt_line(val))
	}
	obj.add('sub_structs', sub_struct_map)
	return obj
}

fn (t Tree) lock_expr(expr ast.LockExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('LockExpr'))
	obj.add('is_expr', t.bool_node(expr.is_expr))
	obj.add('typ', t.type_node(expr.typ))
	obj.add('pos', t.position(expr.pos))
	obj.add('stmts', t.array_node_stmt(expr.stmts))
	obj.add('lockeds', t.array_node_expr(expr.lockeds))
	obj.add('r_lock', t.array_node_bool(expr.is_rlock))
	return obj
}

fn (t Tree) unsafe_expr(expr ast.UnsafeExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('UnsafeExpr'))
	obj.add('expr', t.expr(expr.expr))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) chan_init(expr ast.ChanInit) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ChanInit'))
	obj.add('has_cap', t.bool_node(expr.has_cap))
	obj.add('cap_expr', t.expr(expr.cap_expr))
	obj.add('typ', t.type_node(expr.typ))
	obj.add('elem_type', t.type_node(expr.elem_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) select_expr(expr ast.SelectExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SelectExpr'))
	obj.add('branches', t.array_node_select_branch(expr.branches))
	obj.add('is_expr', t.bool_node(expr.is_expr))
	obj.add('has_exception', t.bool_node(expr.has_exception))
	obj.add('expected_type', t.type_node(expr.expected_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) select_branch(expr ast.SelectBranch) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('SelectBranch'))
	obj.add('stmt', t.stmt(expr.stmt))
	obj.add('stmts', t.array_node_stmt(expr.stmts))
	obj.add('pos', t.position(expr.pos))
	obj.add('comment', t.comment(expr.comment))
	obj.add('is_else', t.bool_node(expr.is_else))
	obj.add('is_timeout', t.bool_node(expr.is_timeout))
	obj.add('post_comments', t.array_node_comment(expr.post_comments))
	return obj
}

fn (t Tree) array_decompose(expr ast.ArrayDecompose) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('ArrayDecompose'))
	obj.add('expr', t.expr(expr.expr))
	obj.add('expr_type', t.type_node(expr.expr_type))
	obj.add('arg_type', t.type_node(expr.arg_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) go_expr(expr ast.GoExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('GoExpr'))
	obj.add('call_expr', t.call_expr(expr.call_expr))
	obj.add('is_expr', t.bool_node(expr.is_expr))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) offset_of(expr ast.OffsetOf) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('OffsetOf'))
	obj.add('struct_type', t.type_node(expr.struct_type))
	obj.add('field', t.string_node('field'))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) dump_expr(expr ast.DumpExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('DumpExpr'))
	obj.add('expr', t.expr(expr.expr))
	obj.add('expr_type', t.type_node(expr.expr_type))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) node_error(expr ast.NodeError) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('NodeError'))
	obj.add('idx', t.number_node(expr.idx))
	obj.add('pos', t.position(expr.pos))
	return obj
}

fn (t Tree) empty_expr(expr ast.EmptyExpr) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('EmptyExpr'))
	// obj.add('x', t.number_node(expr.x))
	return obj
}

fn (t Tree) empty_stmt(node ast.EmptyStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('EmptyStmt'))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_stmt(node ast.AsmStmt) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmStmt'))
	obj.add('arch', t.enum_node(node.arch))
	obj.add('is_basic', t.bool_node(node.is_basic))
	obj.add('is_volatile', t.bool_node(node.is_volatile))
	obj.add('is_goto', t.bool_node(node.is_goto))
	obj.add('scope', t.scope(node.scope))
	// obj.add('scope', t.number_node(int(node.scope)))
	obj.add('pos', t.position(node.pos))
	obj.add('clobbered', t.array_node_asm_clobbered(node.clobbered))
	obj.add('templates', t.array_node_asm_template(node.templates))
	obj.add('output', t.array_node_asm_io(node.output))
	obj.add('input', t.array_node_asm_io(node.input))
	obj.add('global_labels', t.array_node_string(node.global_labels))
	obj.add('local_labels', t.array_node_string(node.local_labels))
	return obj
}

fn (t Tree) asm_register(node ast.AsmRegister) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmRegister'))
	obj.add('name', t.string_node(node.name))
	obj.add('typ', t.type_node(node.typ))
	obj.add('size', t.number_node(node.size))
	return obj
}

fn (t Tree) asm_template(node ast.AsmTemplate) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmTemplate'))
	obj.add('name', t.string_node(node.name))
	obj.add('is_label', t.bool_node(node.is_label))
	obj.add('is_directive', t.bool_node(node.is_directive))
	obj.add('args', t.array_node_asm_arg(node.args))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_addressing(node ast.AsmAddressing) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmAddressing'))
	obj.add('scale', t.number_node(node.scale))
	obj.add('mode', t.enum_node(node.mode))
	obj.add('segment', t.string_node(node.segment))
	obj.add('displacement', t.asm_arg(node.displacement))
	obj.add('base', t.asm_arg(node.base))
	obj.add('index', t.asm_arg(node.index))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_arg(node ast.AsmArg) &Node {
	match node {
		ast.AsmAddressing {
			return t.asm_addressing(node)
		}
		ast.AsmAlias {
			return t.asm_alias(node)
		}
		ast.AsmDisp {
			return t.asm_disp(node)
		}
		ast.AsmRegister {
			return t.asm_register(node)
		}
		ast.BoolLiteral {
			return t.bool_literal(node)
		}
		ast.CharLiteral {
			return t.char_literal(node)
		}
		ast.FloatLiteral {
			return t.float_literal(node)
		}
		ast.IntegerLiteral {
			return t.integer_literal(node)
		}
		string {
			return t.string_node(node)
		}
	}
}

fn (t Tree) asm_alias(node ast.AsmAlias) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmAlias'))
	obj.add('name', t.string_node(node.name))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_disp(node ast.AsmDisp) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmDisp'))
	obj.add('val', t.string_node(node.val))
	obj.add('pos', t.position(node.pos))
	return obj
}

fn (t Tree) asm_clobbered(node ast.AsmClobbered) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmClobbered'))
	obj.add('reg', t.asm_register(node.reg))
	obj.add('comments', t.array_node_comment(node.comments))
	return obj
}

fn (t Tree) asm_io(node ast.AsmIO) &Node {
	mut obj := new_object()
	obj.add('ast_type', t.string_node('AsmIO'))
	obj.add('alias', t.string_node(node.alias))
	obj.add('constraint', t.string_node(node.constraint))
	obj.add('expr', t.expr(node.expr))
	obj.add('typ', t.type_node(node.typ))
	obj.add('comments', t.array_node_comment(node.comments))
	obj.add('pos', t.position(node.pos))
	return obj
}

// do not support yet by vlang
// fn (t Tree) array_node1<T>(nodes []T, method_name string) &Node {
// 	mut arr := new_array()

// 	// call method dynamically, V do not support yet
// 	// error: todo: not a string literal

// 	// for node in nodes {
// 	// 	arr.add_item(t.$method_name(node))
// 	// }

// 	// temp
// 	$for method in Tree.methods {
// 		if method.name == method_name {
// 			for node in nodes {
// 				res := t.$method(node)
// 				arr.add_item(res) // TODO,waiting for bug fixed
// 			}
// 		}
// 	}
// 	return arr
// }

// do not support yet by vlang
// fn (t Tree) array_node2<T>(nodes []T) &Node {
// 	mut arr := new_array()

// 	for node in nodes {
// 		match node {
// 			string {
// 				arr.add_item(t.string_node(node))
// 			}
// 			ast.Comment {
// 				arr.add_item(t.comment(node))
// 			}
// 			ast.ConstField {
// 				arr.add_item(t.const_field(node))
// 			}
// 			else {
// 				panic('unknown array type')
// 			}
// 		}
// 	}

// 	return arr
// }

// list all the different type of array node,temporarily
fn (t Tree) array_node_string(nodes []string) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.string_node(node))
	}
	return arr
}

fn (t Tree) array_node_position(nodes []token.Position) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.position(node))
	}
	return arr
}

fn (t Tree) array_node_if_branch(nodes []ast.IfBranch) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.if_branch(node))
	}
	return arr
}

fn (t Tree) array_node_fn_decl(nodes []ast.FnDecl) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.fn_decl(node))
	}
	return arr
}

fn (t Tree) array_node_generic_fns(nodes []&ast.FnDecl) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.fn_decl(node))
	}
	return arr
}

fn (t Tree) array_node_embed_file(nodes []ast.EmbeddedFile) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.embed_file(node))
	}
	return arr
}

fn (t Tree) array_node_attr(nodes []ast.Attr) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.attr(node))
	}
	return arr
}

fn (t Tree) array_node_scope_struct_field(nodes map[string]ast.ScopeStructField) &Node {
	mut arr := new_array()
	for _, node in nodes {
		arr.add_item(t.scope_struct_field(node))
	}
	return arr
}

fn (t Tree) array_node_type(nodes []ast.Type) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.type_node(node))
	}
	return arr
}

fn (t Tree) array_node_type_expr(nodes []ast.TypeNode) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.type_expr(node))
	}
	return arr
}

fn (t Tree) array_node_import_symbol(nodes []ast.ImportSymbol) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.import_symbol(node))
	}
	return arr
}

fn (t Tree) array_node_comment(nodes []ast.Comment) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.comment(node))
	}
	return arr
}

fn (t Tree) array_node_const_field(nodes []ast.ConstField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.const_field(node))
	}
	return arr
}

fn (t Tree) array_node_arg(nodes []ast.Param) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.arg(node))
	}
	return arr
}

fn (t Tree) array_node_stmt(nodes []ast.Stmt) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.stmt(node))
	}
	return arr
}

fn (t Tree) array_node_defer_stmt(nodes []ast.DeferStmt) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.defer_stmt(node))
	}
	return arr
}

fn (t Tree) array_node_struct_field(nodes []ast.StructField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.struct_field(node))
	}
	return arr
}

fn (t Tree) array_node_embed(nodes []ast.Embed) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.embed(node))
	}
	return arr
}

fn (t Tree) array_node_enum_field(nodes []ast.EnumField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.enum_field(node))
	}
	return arr
}

fn (t Tree) array_node_global_field(nodes []ast.GlobalField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.global_field(node))
	}
	return arr
}

fn (t Tree) array_node_expr(nodes []ast.Expr) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.expr(node))
	}
	return arr
}

fn (t Tree) array_node_call_arg(nodes []ast.CallArg) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.call_arg(node))
	}
	return arr
}

fn (t Tree) array_node_int(nodes []int) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.number_node(node))
	}
	return arr
}

fn (t Tree) array_node_byte(nodes []byte) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.number_node(node))
	}
	return arr
}

fn (t Tree) array_node_bool(nodes []bool) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.bool_node(node))
	}
	return arr
}

fn (t Tree) array_node_struct_init_field(nodes []ast.StructInitField) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.struct_init_field(node))
	}
	return arr
}

fn (t Tree) array_node_struct_init_embed(nodes []ast.StructInitEmbed) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.struct_init_embed(node))
	}
	return arr
}

fn (t Tree) array_node_match_branch(nodes []ast.MatchBranch) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.match_branch(node))
	}
	return arr
}

fn (t Tree) array_node_ident(nodes []ast.Ident) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.ident(node))
	}
	return arr
}

fn (t Tree) array_node_select_branch(nodes []ast.SelectBranch) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.select_branch(node))
	}
	return arr
}

fn (t Tree) array_node_asm_clobbered(nodes []ast.AsmClobbered) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_clobbered(node))
	}
	return arr
}

fn (t Tree) array_node_asm_template(nodes []ast.AsmTemplate) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_template(node))
	}
	return arr
}

fn (t Tree) array_node_asm_io(nodes []ast.AsmIO) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_io(node))
	}
	return arr
}

fn (t Tree) array_node_asm_arg(nodes []ast.AsmArg) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.asm_arg(node))
	}
	return arr
}

fn (t Tree) array_node_sql_stmt_line(nodes []ast.SqlStmtLine) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.sql_stmt_line(node))
	}
	return arr
}

fn (t Tree) array_node_interface_embedding(nodes []ast.InterfaceEmbedding) &Node {
	mut arr := new_array()
	for node in nodes {
		arr.add_item(t.interface_embedding(node))
	}
	return arr
}
