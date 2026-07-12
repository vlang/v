module eval

import os
import strconv
import time
import v3.flat
import v3.parser
import v3.pref

pub type Value = ArrayValue
	| EnumValue
	| FnValue
	| MapValue
	| ModuleValue
	| RangeValue
	| StructValue
	| SumValue
	| TupleValue
	| TypeValue
	| VoidValue
	| bool
	| f64
	| i64
	| string

pub struct ArrayValue {
pub mut:
	elem_type_name string
	values         []Value
}

pub struct EnumValue {
pub:
	type_name string
	value     i64
}

pub struct FnValue {
	node          flat.NodeId
	module_name   string
	file_name     string
	captures      map[string]Value
	capture_types map[string]string
}

pub struct MapEntry {
pub:
	key Value
pub mut:
	value Value
}

pub struct MapValue {
pub mut:
	key_type_name   string
	value_type_name string
	default_value   Value
	entries         []MapEntry
}

pub struct ModuleValue {
pub:
	name string
}

pub struct RangeValue {
pub:
	start     i64
	end       i64
	inclusive bool
}

pub struct StructValue {
pub:
	type_name string
pub mut:
	fields map[string]Value
}

pub struct SumValue {
pub:
	type_name    string
	variant_name string
pub mut:
	payload Value
}

pub struct TupleValue {
pub:
	values []Value
}

pub struct TypeValue {
pub:
	name string
}

pub struct VoidValue {}

struct FunctionDef {
	node        flat.NodeId
	name        string
	module_name string
	file_name   string
}

struct ConstEntry {
	node        flat.NodeId
	module_name string
	file_name   string
mut:
	cached     bool
	evaluating bool
	value      Value
}

struct GlobalEntry {
	name         string
	typ          string
	default_node flat.NodeId
	module_name  string
	file_name    string
}

struct EnumInitEntry {
	node        flat.NodeId
	module_name string
	file_name   string
}

struct TopLevelStmt {
	node        flat.NodeId
	module_name string
	file_name   string
}

struct StructInfo {
	module_name string
	name        string
mut:
	fields []FieldInfo
}

struct TypeAliasInfo {
	module_name string
	target      string
}

struct FieldInfo {
	name         string
	typ          string
	default_node flat.NodeId
	module_name  string
	file_name    string
}

struct ScopeFrame {
mut:
	vars        map[string]Value
	types       map[string]string
	defer_stmts []flat.NodeId
}

struct CallFrame {
	module_name string
	file_name   string
	fn_name     string
	return_type string
	scope_idx   int = -1
}

struct MaybeValue {
	found bool
	value Value
}

struct SmartcastBinding {
	name      string
	value     Value
	type_name string
}

struct CallResult {
	values           []Value
	mutated_args     map[int]Value
	fn_value_changed bool
	fn_value         FnValue
}

struct MethodCallResult {
	value            Value
	receiver_changed bool
	receiver         Value
	mutated_args     map[int]Value
}

struct LvalueStep {
	kind       flat.NodeKind
	container  Value
	index      Value
	field_name string
}

struct ResolvedLvalue {
mut:
	signal  FlowSignal
	root_id flat.NodeId
	value   Value
	steps   []LvalueStep
}

struct InfixOperatorCallInfo {
	target  FunctionDef
	reverse bool
	negate  bool
}

enum FlowKind {
	normal
	break_
	continue_
	return_
}

struct FlowSignal {
	kind        FlowKind
	label       string
	values      []Value
	mut_lvalues map[int]ResolvedLvalue
}

// Eval interprets v3 flat AST nodes directly for a practical subset of V.
pub struct Eval {
pub mut:
	capture_output bool
	prefs          pref.Preferences
mut:
	a                 &flat.FlatAst = unsafe { nil }
	stdout_data       string
	stderr_data       string
	functions         map[string]map[string]FunctionDef
	consts            map[string]map[string]ConstEntry
	globals           map[string]map[string]Value
	global_types      map[string]map[string]string
	global_inits      []GlobalEntry
	enum_inits        []EnumInitEntry
	implicit_main     []TopLevelStmt
	structs           map[string]StructInfo
	enum_fields       map[string][]string
	sum_types         map[string][]string
	type_aliases      map[string]TypeAliasInfo
	type_names        map[string]map[string]bool
	module_imports    map[string][]string
	module_order      []string
	file_import_alias map[string]map[string]string
	modules           map[string]bool
	scopes            []ScopeFrame
	call_stack        []CallFrame
}

// new returns a new evaluator configured for direct execution.
pub fn new(prefs_ &pref.Preferences) Eval {
	return Eval{
		prefs: *prefs_
	}
}

// create returns a capturing evaluator convenient for tests.
pub fn create() Eval {
	return Eval{
		capture_output: true
		prefs:          pref.new_preferences()
	}
}

// stdout returns the captured stdout stream.
pub fn (e &Eval) stdout() string {
	return e.stdout_data
}

// stderr returns the captured stderr stream.
pub fn (e &Eval) stderr() string {
	return e.stderr_data
}

// run_text parses and executes a single V source string.
pub fn (mut e Eval) run_text(code string) ![]Value {
	tmp_file := os.join_path(os.temp_dir(), 'v3_eval_${os.getpid()}_${time.now().unix_micro()}.v')
	os.write_file(tmp_file, code)!
	defer {
		os.rm(tmp_file) or {}
	}
	mut p := parser.Parser.new(&e.prefs)
	p.parse_into(tmp_file)
	return e.run_files(p.a)
}

// run_files executes parsed flat AST files and invokes `main.main`.
pub fn (mut e Eval) run_files(a &flat.FlatAst) ![]Value {
	e.reset(a)
	e.register_files()!
	e.run_inits()!
	if e.has_main_function() {
		return e.call_function('main', 'main', []Value{})!.values
	}
	return e.run_implicit_main()
}

fn void_value() Value {
	return VoidValue{}
}

fn normal_flow() FlowSignal {
	return FlowSignal{
		kind: .normal
	}
}

fn value_flow(value Value) FlowSignal {
	return FlowSignal{
		values: [value]
	}
}

fn (mut e Eval) reset(a &flat.FlatAst) {
	e.a = unsafe { a }
	e.stdout_data = ''
	e.stderr_data = ''
	e.functions = map[string]map[string]FunctionDef{}
	e.consts = map[string]map[string]ConstEntry{}
	e.globals = map[string]map[string]Value{}
	e.global_types = map[string]map[string]string{}
	e.global_inits = []GlobalEntry{}
	e.enum_inits = []EnumInitEntry{}
	e.implicit_main = []TopLevelStmt{}
	e.structs = map[string]StructInfo{}
	e.enum_fields = map[string][]string{}
	e.sum_types = map[string][]string{}
	e.type_aliases = map[string]TypeAliasInfo{}
	e.type_names = map[string]map[string]bool{}
	e.module_imports = map[string][]string{}
	e.module_order = []string{}
	e.file_import_alias = map[string]map[string]string{}
	e.modules = map[string]bool{}
	e.scopes = []ScopeFrame{}
	e.call_stack = []CallFrame{}
}

fn (e &Eval) node(id flat.NodeId) &flat.Node {
	return e.a.node(id)
}

fn (e &Eval) child(node &flat.Node, index int) flat.NodeId {
	return e.a.child(node, index)
}

fn (e &Eval) child_node(node &flat.Node, index int) &flat.Node {
	return e.a.child_node(node, index)
}

fn (e &Eval) children(node &flat.Node) []flat.NodeId {
	return e.a.children_of(node)
}

fn (mut e Eval) ensure_module_maps(module_name string) {
	if module_name !in e.functions {
		e.functions[module_name] = map[string]FunctionDef{}
	}
	if module_name !in e.consts {
		e.consts[module_name] = map[string]ConstEntry{}
	}
	if module_name !in e.globals {
		e.globals[module_name] = map[string]Value{}
	}
	if module_name !in e.global_types {
		e.global_types[module_name] = map[string]string{}
	}
	if module_name !in e.type_names {
		e.type_names[module_name] = map[string]bool{}
	}
}

fn (e &Eval) top_level_registration_children(node &flat.Node) []flat.NodeId {
	mut ids := []flat.NodeId{}
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind == .block {
			ids << e.top_level_registration_children(child)
			continue
		}
		ids << child_id
	}
	return ids
}

fn (e &Eval) has_main_function() bool {
	return 'main' in e.functions && 'main' in e.functions['main']
}

fn is_top_level_declaration_kind(kind flat.NodeKind) bool {
	match kind {
		.empty, .module_decl, .import_decl, .directive, .fn_decl, .c_fn_decl, .struct_decl,
		.field_decl, .global_decl, .const_decl, .const_field, .enum_decl, .enum_field, .type_decl,
		.interface_decl, .interface_field, .param, .comptime_if, .comptime_for {
			return true
		}
		else {
			return false
		}
	}
}

fn (mut e Eval) register_implicit_main_stmt(module_name string, file_name string, id flat.NodeId, node &flat.Node) {
	if module_name != 'main' || is_top_level_declaration_kind(node.kind) {
		return
	}
	e.implicit_main << TopLevelStmt{
		node:        id
		module_name: module_name
		file_name:   file_name
	}
}

fn (mut e Eval) register_files() ! {
	for file_id, file_node in e.a.nodes {
		if file_node.kind != .file || file_node.children_count == 0 {
			continue
		}
		file_name := file_node.value
		top_level_children := e.top_level_registration_children(&file_node)
		mut module_name := 'main'
		for child_id in top_level_children {
			child := e.node(child_id)
			if child.kind == .module_decl {
				module_name = child.value
				break
			}
		}
		if module_name !in e.modules {
			e.module_order << module_name
		}
		e.modules[module_name] = true
		e.ensure_module_maps(module_name)
		if module_name !in e.module_imports {
			e.module_imports[module_name] = []string{}
		}
		mut import_aliases := map[string]string{}
		for child_id in top_level_children {
			child := e.node(child_id)
			if child.kind != .import_decl {
				continue
			}
			imported_module := child.value.all_after_last('.')
			if imported_module.len > 0 && imported_module != module_name
				&& imported_module !in e.module_imports[module_name] {
				e.module_imports[module_name] << imported_module
			}
			import_aliases[child.typ] = imported_module
			import_aliases[imported_module] = imported_module
		}
		e.file_import_alias[file_name] = import_aliases.clone()
		for child_id in top_level_children {
			child := e.node(child_id)
			match child.kind {
				.import_decl {}
				.const_decl {
					for field_id in e.children(child) {
						field := e.node(field_id)
						if field.kind != .const_field {
							continue
						}
						e.consts[module_name][field.value] = ConstEntry{
							node:        field_id
							module_name: module_name
							file_name:   file_name
							value:       void_value()
						}
					}
				}
				.global_decl {
					for field_id in e.children(child) {
						field := e.node(field_id)
						if field.kind != .field_decl {
							continue
						}
						e.global_inits << GlobalEntry{
							name:         field.value
							typ:          field.typ
							default_node: if field.children_count > 0 {
								e.child(field, 0)
							} else {
								flat.empty_node
							}
							module_name:  module_name
							file_name:    file_name
						}
						e.global_types[module_name][field.value] = e.qualify_nested_type_name(module_name,
							field.typ)
					}
				}
				.enum_decl {
					e.type_names[module_name][child.value] = true
					enum_name := e.qualify_type_name(module_name, child.value)
					mut enum_fields := []string{}
					for field_id in e.children(child) {
						field := e.node(field_id)
						if field.kind != .enum_field {
							continue
						}
						enum_fields << field.value
					}
					e.enum_fields[enum_name] = enum_fields
					if module_name in ['main', 'builtin'] {
						e.enum_fields[child.value] = enum_fields
					}
					e.enum_inits << EnumInitEntry{
						node:        child_id
						module_name: module_name
						file_name:   file_name
					}
				}
				.fn_decl {
					e.register_function(module_name, file_name, child_id, child)
				}
				.struct_decl {
					e.type_names[module_name][child.value] = true
					mut fields := []FieldInfo{}
					for field_id in e.children(child) {
						field := e.node(field_id)
						if field.kind == .field_decl {
							fields << FieldInfo{
								name:         field.value
								typ:          field.typ
								default_node: if field.children_count > 0 {
									e.child(field, 0)
								} else {
									flat.empty_node
								}
								module_name:  module_name
								file_name:    file_name
							}
						}
					}
					info := StructInfo{
						module_name: module_name
						name:        child.value
						fields:      fields
					}
					qualified_name := e.qualify_type_name(module_name, child.value)
					e.structs[qualified_name] = info
					if module_name in ['main', 'builtin'] {
						e.structs[child.value] = info
					}
				}
				.type_decl {
					e.type_names[module_name][child.value] = true
					if child.children_count > 0 {
						mut variants := []string{}
						for variant_id in e.children(child) {
							variant := e.node(variant_id)
							variants << e.qualify_type_name(module_name, variant.value)
						}
						qualified_name := e.qualify_type_name(module_name, child.value)
						e.sum_types[qualified_name] = variants
						if module_name in ['main', 'builtin'] {
							e.sum_types[child.value] = variants
						}
					} else if child.typ.len > 0 {
						info := TypeAliasInfo{
							module_name: module_name
							target:      child.typ
						}
						qualified_name := e.qualify_type_name(module_name, child.value)
						e.type_aliases[qualified_name] = info
						if module_name in ['main', 'builtin'] {
							e.type_aliases[child.value] = info
						}
					}
				}
				else {
					e.register_implicit_main_stmt(module_name, file_name, child_id, child)
				}
			}
		}
		_ = file_id
	}
	e.evaluate_enum_inits()!
	e.evaluate_global_inits()!
	if !e.has_main_function() && e.implicit_main.len == 0 {
		return error('v3.eval: missing main.main entry point')
	}
}

fn (mut e Eval) evaluate_enum_inits() ! {
	for entry in e.enum_inits {
		node := e.node(entry.node)
		mut next_value := if node.typ == 'flag' { i64(1) } else { i64(0) }
		for field_id in e.children(node) {
			field := e.node(field_id)
			if field.kind != .enum_field {
				continue
			}
			value := if field.children_count > 0 {
				e.value_as_int(e.eval_expr_in_module(e.child(field, 0), entry.module_name,
					entry.file_name, '<enum ${node.value}.${field.value}>')!)!
			} else {
				next_value
			}
			e.consts[entry.module_name][field.value] = ConstEntry{
				node:        field_id
				module_name: entry.module_name
				file_name:   entry.file_name
				cached:      true
				value:       Value(value)
			}
			e.consts[entry.module_name]['${node.value}.${field.value}'] = ConstEntry{
				node:        field_id
				module_name: entry.module_name
				file_name:   entry.file_name
				cached:      true
				value:       Value(value)
			}
			next_value = if node.typ == 'flag' {
				if value <= 0 { i64(1) } else { value * 2 }
			} else {
				value + 1
			}
		}
	}
}

fn (mut e Eval) run_implicit_main() ![]Value {
	e.open_scope()
	scope_idx := e.scopes.len - 1
	mut frame := CallFrame{
		module_name: 'main'
		file_name:   if e.implicit_main.len > 0 { e.implicit_main[0].file_name } else { '' }
		fn_name:     'main'
		return_type: 'void'
		scope_idx:   scope_idx
	}
	e.call_stack << frame
	mut values := [void_value()]
	mut escaped := normal_flow()
	for stmt in e.implicit_main {
		frame = CallFrame{
			module_name: stmt.module_name
			file_name:   stmt.file_name
			fn_name:     'main'
			return_type: 'void'
			scope_idx:   scope_idx
		}
		e.call_stack[e.call_stack.len - 1] = frame
		signal := e.exec_stmt(stmt.node)!
		if signal.kind == .return_ {
			values = flow_values_or_void(signal.values)
			break
		}
		if signal.kind != .normal {
			escaped = signal
			break
		}
	}
	e.run_deferred_stmts()!
	e.close_scope()!
	e.call_stack.delete(e.call_stack.len - 1)
	if escaped.kind != .normal {
		return error('v3.eval: unexpected `${escaped.kind}` escaped implicit main')
	}
	return values
}

fn (mut e Eval) evaluate_global_inits() ! {
	mut entries_by_module := map[string][]GlobalEntry{}
	for entry in e.global_inits {
		mut entries := entries_by_module[entry.module_name]
		entries << entry
		entries_by_module[entry.module_name] = entries
	}
	mut visited := map[string]bool{}
	mut visiting := map[string]bool{}
	e.evaluate_module_global_inits('main', entries_by_module, mut visited, mut visiting)!
}

fn (mut e Eval) evaluate_module_global_inits(module_name string, entries_by_module map[string][]GlobalEntry, mut visited map[string]bool, mut visiting map[string]bool) ! {
	if module_name in visited {
		return
	}
	if module_name in visiting {
		return
	}
	visiting[module_name] = true
	if module_name in e.module_imports {
		for imported_module in e.module_imports[module_name] {
			e.evaluate_module_global_inits(imported_module, entries_by_module, mut visited, mut
				visiting)!
		}
	}
	visiting.delete(module_name)
	visited[module_name] = true
	if module_name !in entries_by_module {
		return
	}
	for entry in entries_by_module[module_name] {
		e.evaluate_global_init(entry)!
	}
}

fn (mut e Eval) evaluate_global_init(entry GlobalEntry) ! {
	e.call_stack << CallFrame{
		module_name: entry.module_name
		file_name:   entry.file_name
		fn_name:     '<global ${entry.name}>'
	}
	value := if int(entry.default_node) >= 0 {
		e.eval_expr(entry.default_node)!
	} else {
		e.zero_value_for_type_name_in_module(entry.typ, entry.module_name)
	}
	e.call_stack.delete(e.call_stack.len - 1)
	e.globals[entry.module_name][entry.name] = e.adapt_value_to_type_name(value, entry.typ)
}

fn (mut e Eval) eval_expr_in_module(id flat.NodeId, module_name string, file_name string, label string) !Value {
	e.call_stack << CallFrame{
		module_name: module_name
		file_name:   file_name
		fn_name:     label
	}
	defer {
		e.call_stack.delete(e.call_stack.len - 1)
	}
	return e.eval_expr(id)
}

fn (mut e Eval) register_function(module_name string, file_name string, id flat.NodeId, node &flat.Node) {
	def := FunctionDef{
		node:        id
		name:        node.value
		module_name: module_name
		file_name:   file_name
	}
	e.functions[module_name][node.value] = def
	if module_name != 'main' && module_name != 'builtin' {
		e.functions[module_name]['${module_name}.${node.value}'] = def
	}
	if node.value.contains('.') {
		short := node.value.all_after_last('.')
		receiver := node.value.all_before_last('.').all_after_last('.')
		e.functions[module_name]['${receiver}.${short}'] = def
	}
}

fn (mut e Eval) run_inits() ! {
	mut visited := map[string]bool{}
	mut visiting := map[string]bool{}
	e.run_module_init('main', mut visited, mut visiting)!
}

fn (mut e Eval) run_module_init(module_name string, mut visited map[string]bool, mut visiting map[string]bool) ! {
	if module_name in visited {
		return
	}
	if module_name in visiting {
		return
	}
	visiting[module_name] = true
	if module_name in e.module_imports {
		for imported_module in e.module_imports[module_name] {
			e.run_module_init(imported_module, mut visited, mut visiting)!
		}
	}
	visiting.delete(module_name)
	visited[module_name] = true
	if module_name in e.functions && 'init' in e.functions[module_name] {
		e.call_function(module_name, 'init', []Value{})!
	}
}

fn (mut e Eval) open_scope() {
	e.scopes << ScopeFrame{
		vars:        map[string]Value{}
		types:       map[string]string{}
		defer_stmts: []flat.NodeId{}
	}
}

fn (mut e Eval) close_scope() ! {
	if e.scopes.len > 0 {
		e.run_deferred_stmts()!
		e.scopes.delete(e.scopes.len - 1)
	}
}

fn (mut e Eval) declare_var(name string, value Value) {
	e.declare_var_typed(name, value, '')
}

fn (mut e Eval) declare_var_typed(name string, value Value, type_name string) {
	if name == '_' {
		return
	}
	if e.scopes.len == 0 {
		e.open_scope()
	}
	e.scopes[e.scopes.len - 1].vars[name] = value
	normalized := e.normalize_type_name(type_name)
	if normalized.len > 0 {
		e.scopes[e.scopes.len - 1].types[name] = normalized
	}
}

fn (mut e Eval) set_var(name string, value Value) ! {
	if name == '_' {
		return
	}
	for i := e.scopes.len - 1; i >= 0; i-- {
		if name in e.scopes[i].vars {
			e.scopes[i].vars[name] = value
			return
		}
	}
	module_name := e.current_module_name()
	if module_name in e.globals && name in e.globals[module_name] {
		e.globals[module_name][name] = value
		return
	}
	return e.unknown_variable_error(name)
}

fn (mut e Eval) set_var_type(name string, type_name string) {
	normalized := e.normalize_type_name(type_name)
	if normalized.len == 0 {
		return
	}
	for i := e.scopes.len - 1; i >= 0; i-- {
		if name in e.scopes[i].vars {
			e.scopes[i].types[name] = normalized
			return
		}
	}
}

fn (e &Eval) lookup_var(name string) MaybeValue {
	for i := e.scopes.len - 1; i >= 0; i-- {
		if name in e.scopes[i].vars {
			return MaybeValue{
				found: true
				value: e.scopes[i].vars[name] or { void_value() }
			}
		}
	}
	module_name := e.current_module_name()
	if module_name in e.globals && name in e.globals[module_name] {
		return MaybeValue{
			found: true
			value: e.globals[module_name][name] or { void_value() }
		}
	}
	return MaybeValue{}
}

fn (e &Eval) lookup_var_type(name string) ?string {
	for i := e.scopes.len - 1; i >= 0; i-- {
		if name in e.scopes[i].types {
			return e.scopes[i].types[name]
		}
	}
	module_name := e.current_module_name()
	if module_name in e.global_types && name in e.global_types[module_name] {
		return e.global_types[module_name][name]
	}
	return none
}

fn (e &Eval) current_module_name() string {
	if e.call_stack.len == 0 {
		return 'main'
	}
	return e.call_stack[e.call_stack.len - 1].module_name
}

fn (e &Eval) current_file_name() string {
	if e.call_stack.len == 0 {
		return ''
	}
	return e.call_stack[e.call_stack.len - 1].file_name
}

fn (e &Eval) current_function_label() string {
	if e.call_stack.len == 0 {
		return '<top-level>'
	}
	frame := e.call_stack[e.call_stack.len - 1]
	return '${frame.module_name}.${frame.fn_name}'
}

fn (e &Eval) call_stack_trace() string {
	return e.call_stack.map('${it.module_name}.${it.fn_name}').join(' -> ')
}

fn (e &Eval) unknown_variable_error(name string) IError {
	return error('v3.eval: unknown variable `${name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
}

fn (mut e Eval) call_function(module_name string, fn_name string, args []Value) !CallResult {
	builtin := e.maybe_call_builtin(module_name, fn_name, args)!
	if builtin.found {
		return CallResult{
			values: [builtin.value]
		}
	}
	target_module := if module_name == '' { e.current_module_name() } else { module_name }
	if target_module !in e.functions {
		return error('v3.eval: unknown module `${target_module}`')
	}
	if fn_name !in e.functions[target_module] {
		return error('v3.eval: unknown function `${target_module}.${fn_name}`')
	}
	def := e.functions[target_module][fn_name]
	node := e.node(def.node)
	mut params := []flat.NodeId{}
	mut body_start := 0
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind == .param {
			params << child_id
			body_start++
			continue
		}
		break
	}
	min_args := e.minimum_arg_count(params)
	if args.len < min_args {
		return error('v3.eval: `${target_module}.${fn_name}` expected ${min_args} arguments, got ${args.len}')
	}
	e.open_scope()
	e.call_stack << CallFrame{
		module_name: target_module
		file_name:   def.file_name
		fn_name:     fn_name
		return_type: node.typ
		scope_idx:   e.scopes.len - 1
	}
	e.bind_call_params(params, args)!
	mut body_ids := e.children(node)
	if body_start > 0 {
		body_ids = body_ids[body_start..].clone()
	}
	signal := e.exec_stmts(body_ids)!
	e.run_deferred_stmts()!
	mutated_args := e.collect_mutated_param_args(params, args)
	adapted_values := if signal.kind == .return_ {
		e.adapt_return_values(signal.values, node.typ)
	} else {
		[]Value{}
	}
	e.close_scope()!
	e.call_stack.delete(e.call_stack.len - 1)
	if signal.kind == .return_ {
		return CallResult{
			values:       adapted_values
			mutated_args: mutated_args
		}
	}
	if signal.kind != .normal {
		return error('v3.eval: unexpected `${signal.kind}` escaped `${target_module}.${fn_name}`')
	}
	return CallResult{
		values:       [void_value()]
		mutated_args: mutated_args
	}
}

fn (mut e Eval) exec_stmts(stmts []flat.NodeId) !FlowSignal {
	mut i := 0
	for i < stmts.len {
		stmt_id := stmts[i]
		if int(stmt_id) >= 0 {
			node := e.node(stmt_id)
			if node.kind == .label_stmt && i + 1 < stmts.len {
				next_id := stmts[i + 1]
				if int(next_id) >= 0 {
					next_node := e.node(next_id)
					if next_node.kind == .for_stmt {
						signal := e.exec_for(next_node, node.value)!
						if signal.kind != .normal {
							return signal
						}
						i += 2
						continue
					}
					if next_node.kind == .for_in_stmt {
						signal := e.exec_for_in(next_node, node.value)!
						if signal.kind != .normal {
							return signal
						}
						i += 2
						continue
					}
					if e.is_multi_init_for_block(next_node) {
						signal := e.exec_labeled_multi_init_for(next_node, node.value)!
						if signal.kind != .normal {
							return signal
						}
						i += 2
						continue
					}
				}
			}
		}
		signal := e.exec_stmt(stmt_id)!
		if signal.kind != .normal {
			return signal
		}
		i++
	}
	return normal_flow()
}

fn (e &Eval) is_multi_init_for_block(node &flat.Node) bool {
	if node.kind != .block || node.children_count != 2 {
		return false
	}
	if node.value != 'for_c_style_multi' {
		return false
	}
	init_id := e.child(node, 0)
	loop_id := e.child(node, 1)
	if int(init_id) < 0 || int(loop_id) < 0 {
		return false
	}
	init_node := e.node(init_id)
	loop_node := e.node(loop_id)
	if init_node.kind !in [.assign, .decl_assign] || init_node.children_count < 3 {
		return false
	}
	if loop_node.kind != .for_stmt || loop_node.children_count < 3 {
		return false
	}
	loop_init_id := e.child(loop_node, 0)
	if int(loop_init_id) < 0 {
		return false
	}
	return e.node(loop_init_id).kind == .empty
}

fn (mut e Eval) exec_labeled_multi_init_for(node &flat.Node, loop_label string) !FlowSignal {
	init_id := e.child(node, 0)
	loop_id := e.child(node, 1)
	e.open_scope()
	if int(init_id) >= 0 {
		init_signal := e.exec_stmt(init_id)!
		if init_signal.kind != .normal {
			e.close_scope()!
			return init_signal
		}
	}
	signal := e.exec_for(e.node(loop_id), loop_label)!
	e.close_scope()!
	return signal
}

fn (mut e Eval) exec_block(node &flat.Node) !FlowSignal {
	e.open_scope()
	signal := e.exec_stmts(e.children(node))!
	e.close_scope()!
	return signal
}

fn (mut e Eval) exec_stmt(id flat.NodeId) !FlowSignal {
	if int(id) < 0 {
		return normal_flow()
	}
	node := e.node(id)
	match node.kind {
		.block {
			return e.exec_block(node)
		}
		.expr_stmt {
			if node.children_count > 0 {
				expr_id := e.child(node, 0)
				expr := e.node(expr_id)
				if expr.kind == .infix && expr.op == .left_shift {
					return e.exec_array_append(expr)
				} else {
					signal := e.eval_expr_flow(expr_id)!
					if signal.kind != .normal {
						return signal
					}
				}
			}
			return normal_flow()
		}
		.decl_assign {
			return e.exec_assign(node, true)
		}
		.assign, .selector_assign, .index_assign {
			return e.exec_assign(node, false)
		}
		.return_stmt {
			mut values := []Value{}
			return_type := e.current_return_type()
			return_types := split_multi_return_types(return_type)
			child_count := node.children_count
			mut child_index := 0
			for child_id in e.children(node) {
				expected_type := return_child_expected_type(return_type, return_types, child_index,
					child_count)
				signal := e.eval_expr_flow_expected(child_id, expected_type)!
				if signal.kind != .normal {
					return signal
				}
				expr_values := flow_values_or_void(signal.values)
				if expr_values.len > 1 {
					values << expr_values
					continue
				}
				value := expr_values[0]
				if value is TupleValue {
					values << value.values
				} else {
					values << value
				}
				child_index++
			}
			if values.len == 0 {
				values << void_value()
			}
			return FlowSignal{
				kind:   .return_
				values: values
			}
		}
		.if_expr {
			return e.exec_if(node)
		}
		.match_stmt {
			return e.exec_match(node)
		}
		.for_stmt {
			return e.exec_for(node, '')
		}
		.for_in_stmt {
			return e.exec_for_in(node, '')
		}
		.break_stmt {
			return FlowSignal{
				kind:  .break_
				label: node.value
			}
		}
		.continue_stmt {
			return FlowSignal{
				kind:  .continue_
				label: node.value
			}
		}
		.assert_stmt {
			cond_signal := e.eval_expr_flow(e.child(node, 0))!
			if cond_signal.kind != .normal {
				return cond_signal
			}
			cond := flow_value(cond_signal)
			if !e.value_as_bool(cond)! {
				mut msg := 'assertion failed'
				if node.children_count > 1 {
					msg = e.display_string(e.eval_expr(e.child(node, 1))!)!
				}
				return error('v3.eval: ${msg}')
			}
			return normal_flow()
		}
		.defer_stmt {
			e.register_defer_stmt(id)
			return normal_flow()
		}
		.asm_stmt, .empty, .label_stmt {
			return normal_flow()
		}
		else {
			_ = e.eval_expr(id)!
			return normal_flow()
		}
	}
}

fn (mut e Eval) register_defer_stmt(id flat.NodeId) {
	if e.scopes.len == 0 {
		e.open_scope()
	}
	node := e.node(id)
	scope_idx := if node.value == 'function' && e.call_stack.len > 0
		&& e.call_stack[e.call_stack.len - 1].scope_idx >= 0 {
		e.call_stack[e.call_stack.len - 1].scope_idx
	} else {
		e.scopes.len - 1
	}
	e.scopes[scope_idx].defer_stmts << id
}

fn (mut e Eval) run_deferred_stmts() ! {
	if e.scopes.len == 0 {
		return
	}
	scope_idx := e.scopes.len - 1
	defer_stmts := e.scopes[scope_idx].defer_stmts.clone()
	e.scopes[scope_idx].defer_stmts = []flat.NodeId{}
	for i := defer_stmts.len; i > 0; i-- {
		defer_id := defer_stmts[i - 1]
		defer_node := e.node(defer_id)
		for child_id in e.children(defer_node) {
			signal := e.exec_stmt(child_id)!
			if signal.kind != .normal {
				return error('v3.eval: unexpected `${signal.kind}` escaped defer')
			}
		}
	}
}

fn (mut e Eval) exec_assign(node &flat.Node, declare bool) !FlowSignal {
	children := e.children(node)
	if children.len == 0 {
		return normal_flow()
	}
	if children.len == 2 {
		lhs_id := children[0]
		rhs_id := children[1]
		if node.op != .assign && node.op != .none && e.target_needs_single_eval(lhs_id) {
			return e.exec_compound_assignment_pair_flow(node.op, lhs_id, rhs_id, declare)
		}
		target_type := e.assignment_target_type_name(lhs_id, declare)
		signal := e.assignment_value_flow(node.op, lhs_id, rhs_id, target_type)!
		if signal.kind != .normal {
			return signal
		}
		assign_signal := e.assign_target_typed_flow(lhs_id, flow_value(signal), declare, if target_type.len > 0 {
			target_type
		} else {
			e.infer_expr_type_name(rhs_id)
		})!
		if assign_signal.kind != .normal {
			return assign_signal
		}
		return normal_flow()
	}
	if children.len > 2 {
		first_target_type := e.assignment_target_type_name(children[0], declare)
		first_signal := e.assignment_value_flow(node.op, children[0], children[1],
			first_target_type)!
		if first_signal.kind != .normal {
			return first_signal
		}
		value := flow_value(first_signal)
		mut lhs_ids := [children[0]]
		for i := 2; i < children.len; i++ {
			lhs_ids << children[i]
		}
		if value is TupleValue {
			for i, lhs_id in lhs_ids {
				item := if i < value.values.len { value.values[i] } else { void_value() }
				assign_signal := e.assign_target_flow(lhs_id, item, declare)!
				if assign_signal.kind != .normal {
					return assign_signal
				}
			}
			return normal_flow()
		}
		if children.len % 2 == 1 {
			for i, lhs_id in lhs_ids {
				item := if i == 0 { value } else { void_value() }
				assign_signal := e.assign_target_flow(lhs_id, item, declare)!
				if assign_signal.kind != .normal {
					return assign_signal
				}
			}
			return normal_flow()
		}
		mut assign_lhs_ids := [children[0]]
		mut values := [value]
		mut i := 2
		for i < children.len {
			lhs_id := children[i]
			rhs_id := if i + 1 < children.len { children[i + 1] } else { flat.empty_node }
			item := if int(rhs_id) >= 0 {
				target_type := e.assignment_target_type_name(lhs_id, declare)
				signal := e.assignment_value_flow(node.op, lhs_id, rhs_id, target_type)!
				if signal.kind != .normal {
					return signal
				}
				flow_value(signal)
			} else {
				void_value()
			}
			assign_lhs_ids << lhs_id
			values << item
			i += 2
		}
		for idx, lhs_id in assign_lhs_ids {
			assign_signal := e.assign_target_flow(lhs_id, values[idx], declare)!
			if assign_signal.kind != .normal {
				return assign_signal
			}
		}
		return normal_flow()
	}
	mut i := 0
	for i < children.len {
		lhs_id := children[i]
		rhs_id := if i + 1 < children.len { children[i + 1] } else { flat.empty_node }
		value := if int(rhs_id) >= 0 {
			target_type := e.assignment_target_type_name(lhs_id, declare)
			signal := e.assignment_value_flow(node.op, lhs_id, rhs_id, target_type)!
			if signal.kind != .normal {
				return signal
			}
			flow_value(signal)
		} else {
			void_value()
		}
		assign_signal := e.assign_target_flow(lhs_id, value, declare)!
		if assign_signal.kind != .normal {
			return assign_signal
		}
		i += 2
	}
	return normal_flow()
}

fn (e &Eval) target_needs_single_eval(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	return e.node(id).kind in [.index, .selector]
}

fn (mut e Eval) exec_compound_assignment_pair_flow(op flat.Op, lhs_id flat.NodeId, rhs_id flat.NodeId, declare bool) !FlowSignal {
	target_type := e.assignment_target_type_name(lhs_id, declare)
	rhs_signal := e.eval_expr_flow_expected(rhs_id, target_type)!
	if rhs_signal.kind != .normal {
		return rhs_signal
	}
	rhs := flow_value(rhs_signal)
	return e.apply_compound_target_flow(op, lhs_id, rhs, declare, if target_type.len > 0 {
		target_type
	} else {
		e.infer_expr_type_name(rhs_id)
	})
}

fn (mut e Eval) apply_compound_target_flow(op flat.Op, lhs_id flat.NodeId, rhs Value, declare bool, type_name string) !FlowSignal {
	node := e.node(lhs_id)
	match node.kind {
		.index {
			return e.update_resolved_lvalue_with_op_flow(lhs_id, op, rhs)
		}
		.selector {
			return e.update_resolved_lvalue_with_op_flow(lhs_id, op, rhs)
		}
		else {
			left_signal := e.eval_expr_flow(lhs_id)!
			if left_signal.kind != .normal {
				return left_signal
			}
			value := e.apply_assignment_op(op, flow_value(left_signal), rhs)!
			return e.assign_target_typed_flow(lhs_id, value, declare, type_name)
		}
	}
}

fn (mut e Eval) assignment_value(op flat.Op, lhs_id flat.NodeId, rhs_id flat.NodeId) !Value {
	rhs := e.eval_expr(rhs_id)!
	if op == .assign || op == .none {
		return rhs
	}
	left := e.eval_expr(lhs_id) or { e.zero_value_like(rhs) }
	return match op {
		.plus_assign { e.apply_infix(.plus, left, rhs)! }
		.minus_assign { e.apply_infix(.minus, left, rhs)! }
		.mul_assign { e.apply_infix(.mul, left, rhs)! }
		.div_assign { e.apply_infix(.div, left, rhs)! }
		.mod_assign { e.apply_infix(.mod, left, rhs)! }
		.amp_assign { e.apply_infix(.amp, left, rhs)! }
		.pipe_assign { e.apply_infix(.pipe, left, rhs)! }
		.xor_assign { e.apply_infix(.xor, left, rhs)! }
		.left_shift_assign { e.apply_infix(.left_shift, left, rhs)! }
		.right_shift_assign { e.apply_infix(.right_shift, left, rhs)! }
		.right_shift_unsigned_assign { e.apply_infix(.right_shift_unsigned, left, rhs)! }
		else { rhs }
	}
}

fn (mut e Eval) assignment_value_flow(op flat.Op, lhs_id flat.NodeId, rhs_id flat.NodeId, expected_type string) !FlowSignal {
	rhs_signal := e.eval_expr_flow_expected(rhs_id, expected_type)!
	if rhs_signal.kind != .normal {
		return rhs_signal
	}
	rhs := flow_value(rhs_signal)
	if op == .assign || op == .none {
		return FlowSignal{
			values: [rhs]
		}
	}
	left_signal := e.eval_expr_flow(lhs_id)!
	if left_signal.kind != .normal {
		return left_signal
	}
	left := flow_value(left_signal)
	return FlowSignal{
		values: [e.apply_assignment_op(op, left, rhs)!]
	}
}

fn (mut e Eval) apply_assignment_op(op flat.Op, left Value, rhs Value) !Value {
	return match op {
		.plus_assign { e.apply_infix(.plus, left, rhs)! }
		.minus_assign { e.apply_infix(.minus, left, rhs)! }
		.mul_assign { e.apply_infix(.mul, left, rhs)! }
		.div_assign { e.apply_infix(.div, left, rhs)! }
		.mod_assign { e.apply_infix(.mod, left, rhs)! }
		.amp_assign { e.apply_infix(.amp, left, rhs)! }
		.pipe_assign { e.apply_infix(.pipe, left, rhs)! }
		.xor_assign { e.apply_infix(.xor, left, rhs)! }
		.left_shift_assign { e.apply_infix(.left_shift, left, rhs)! }
		.right_shift_assign { e.apply_infix(.right_shift, left, rhs)! }
		.right_shift_unsigned_assign { e.apply_infix(.right_shift_unsigned, left, rhs)! }
		else { rhs }
	}
}

fn (mut e Eval) assignment_target_type_name(lhs_id flat.NodeId, declare bool) string {
	if int(lhs_id) < 0 {
		return ''
	}
	node := e.node(lhs_id)
	match node.kind {
		.ident {
			if !declare {
				if typ := e.lookup_var_type(node.value) {
					return typ
				}
			}
			return node.typ
		}
		.selector {
			if node.children_count > 0 {
				base_type := e.infer_expr_type_name(e.child(node, 0))
				if base_type.len > 0 {
					return e.struct_field_type_name_by_type(base_type, node.value)
				}
			}
		}
		.index {
			if node.children_count > 0 {
				container_type := e.infer_expr_type_name(e.child(node, 0))
				if container_type.starts_with('[]') {
					return container_type[2..]
				}
				if is_fixed_array_type_name(container_type) {
					return fixed_array_elem_type_name(container_type)
				}
				if container_type.starts_with('map[') {
					_, value_type := split_map_type(container_type)
					return value_type
				}
			}
		}
		else {}
	}

	return ''
}

fn flow_value(signal FlowSignal) Value {
	if signal.values.len > 0 {
		return signal.values[0]
	}
	return void_value()
}

fn flow_values_value(values []Value) Value {
	if values.len == 0 {
		return void_value()
	}
	if values.len == 1 {
		return values[0]
	}
	return TupleValue{
		values: values.clone()
	}
}

fn flow_values_or_void(values []Value) []Value {
	if values.len > 0 {
		return values.clone()
	}
	return [void_value()]
}

fn (mut e Eval) assign_target(id flat.NodeId, value Value, declare bool) ! {
	signal := e.assign_target_typed_flow(id, value, declare, '')!
	if signal.kind != .normal {
		return error('v3.eval: unexpected `${signal.kind}` escaped assignment target')
	}
}

fn (mut e Eval) assign_target_typed(id flat.NodeId, value Value, declare bool, type_name string) ! {
	signal := e.assign_target_typed_flow(id, value, declare, type_name)!
	if signal.kind != .normal {
		return error('v3.eval: unexpected `${signal.kind}` escaped assignment target')
	}
}

fn (mut e Eval) assign_target_flow(id flat.NodeId, value Value, declare bool) !FlowSignal {
	return e.assign_target_typed_flow(id, value, declare, '')
}

fn (mut e Eval) assign_target_typed_flow(id flat.NodeId, value Value, declare bool, type_name string) !FlowSignal {
	node := e.node(id)
	match node.kind {
		.ident {
			if declare {
				decl_type := if type_name.len > 0 { type_name } else { node.typ }
				e.declare_var_typed(node.value, e.adapt_value_to_type_name(value, decl_type),
					decl_type)
			} else {
				target_type := if existing := e.lookup_var_type(node.value) {
					existing
				} else {
					if type_name.len > 0 { type_name } else { node.typ }
				}
				e.set_var(node.value, e.adapt_value_to_type_name(value, target_type))!
				e.set_var_type(node.value, target_type)
			}
		}
		.selector {
			return e.update_resolved_lvalue_flow(id, value)
		}
		.index {
			return e.update_resolved_lvalue_flow(id, value)
		}
		else {
			return error('v3.eval: unsupported assignment target `${node.kind}`')
		}
	}

	return normal_flow()
}

fn (e &Eval) infer_expr_type_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := e.node(id)
	if node.kind in [.struct_init, .cast_expr, .as_expr] {
		return e.normalize_type_name(node.value)
	}
	if node.typ.len > 0 {
		return e.normalize_type_name(node.typ)
	}
	match node.kind {
		.int_literal {
			return 'int'
		}
		.float_literal {
			return 'f64'
		}
		.bool_literal {
			return 'bool'
		}
		.char_literal {
			return 'char'
		}
		.string_literal, .string_interp {
			return 'string'
		}
		.ident {
			if typ := e.lookup_var_type(node.value) {
				return typ
			}
		}
		.selector {
			if node.children_count > 0 {
				if typ := e.type_value_name_from_expr(e.child(node, 0)) {
					return typ
				}
				base_type := e.infer_expr_type_name(e.child(node, 0))
				if base_type.len > 0 {
					return e.struct_field_type_name_by_type(base_type, node.value)
				}
			}
		}
		.index {
			if node.children_count > 0 {
				container_type := e.infer_expr_type_name(e.child(node, 0))
				if container_type.starts_with('[]') {
					return container_type[2..]
				}
				if is_fixed_array_type_name(container_type) {
					return fixed_array_elem_type_name(container_type)
				}
				if container_type.starts_with('map[') {
					_, value_type := split_map_type(container_type)
					return value_type
				}
			}
		}
		.array_literal {
			elem_type_name := e.array_literal_elem_type_name(node)
			if elem_type_name.len > 0 {
				return '[]${elem_type_name}'
			}
		}
		else {}
	}

	return ''
}

fn (e &Eval) array_literal_elem_type_name(node &flat.Node) string {
	if node.typ.len > 0 {
		return array_elem_type_name_from_type(node.typ)
	}
	if node.value.len > 0 {
		return array_elem_type_name_from_type(node.value)
	}
	if node.children_count > 0 {
		return e.infer_expr_type_name(e.child(node, 0))
	}
	return ''
}

fn array_elem_type_name_from_type(type_name string) string {
	if type_name.starts_with('[]') {
		return type_name[2..]
	}
	if is_fixed_array_type_name(type_name) {
		return fixed_array_elem_type_name(type_name)
	}
	return ''
}

fn (mut e Eval) update_target(id flat.NodeId, value Value) ! {
	signal := e.update_target_flow(id, value)!
	if signal.kind != .normal {
		return error('v3.eval: unexpected `${signal.kind}` escaped assignment target')
	}
}

fn (mut e Eval) update_target_flow(id flat.NodeId, value Value) !FlowSignal {
	return e.assign_target_flow(id, value, false)
}

fn (mut e Eval) update_resolved_lvalue_flow(id flat.NodeId, value Value) !FlowSignal {
	resolved := e.resolve_lvalue_flow(id)!
	if resolved.signal.kind != .normal {
		return resolved.signal
	}
	return e.write_resolved_lvalue_flow(resolved, value)
}

fn (mut e Eval) update_resolved_lvalue_with_op_flow(id flat.NodeId, op flat.Op, rhs Value) !FlowSignal {
	resolved := e.resolve_lvalue_flow(id)!
	if resolved.signal.kind != .normal {
		return resolved.signal
	}
	value := e.apply_assignment_op(op, resolved.value, rhs)!
	return e.write_resolved_lvalue_flow(resolved, value)
}

fn (mut e Eval) update_resolved_lvalue_postfix_flow(id flat.NodeId, op flat.Op) !FlowSignal {
	resolved := e.resolve_lvalue_flow(id)!
	if resolved.signal.kind != .normal {
		return resolved.signal
	}
	value := e.apply_postfix_op(op, resolved.value)!
	update_signal := e.write_resolved_lvalue_flow(resolved, value)!
	if update_signal.kind != .normal {
		return update_signal
	}
	return value_flow(resolved.value)
}

fn (mut e Eval) resolve_lvalue_flow(id flat.NodeId) !ResolvedLvalue {
	node := e.node(id)
	match node.kind {
		.selector {
			mut base := e.resolve_lvalue_flow(e.child(node, 0))!
			if base.signal.kind != .normal {
				return base
			}
			value := e.eval_selector_value(base.value, node.value)!
			base.steps << LvalueStep{
				kind:       .selector
				container:  base.value
				index:      void_value()
				field_name: node.value
			}
			base.value = value
			return base
		}
		.index {
			mut base := e.resolve_lvalue_flow(e.child(node, 0))!
			if base.signal.kind != .normal {
				return base
			}
			index_signal := if base.value is MapValue {
				e.eval_expr_flow_expected(e.child(node, 1), base.value.key_type_name)!
			} else {
				e.eval_expr_flow(e.child(node, 1))!
			}
			if index_signal.kind != .normal {
				return ResolvedLvalue{
					signal: index_signal
				}
			}
			index := flow_value(index_signal)
			value := e.index_value(base.value, index)!
			base.steps << LvalueStep{
				kind:      .index
				container: base.value
				index:     index
			}
			base.value = value
			return base
		}
		else {
			signal := e.eval_expr_flow(id)!
			if signal.kind != .normal {
				return ResolvedLvalue{
					signal: signal
				}
			}
			return ResolvedLvalue{
				root_id: id
				value:   flow_value(signal)
			}
		}
	}
}

fn (mut e Eval) write_resolved_lvalue_flow(resolved ResolvedLvalue, value Value) !FlowSignal {
	mut new_value := value
	for i := resolved.steps.len - 1; i >= 0; i-- {
		step := resolved.steps[i]
		match step.kind {
			.index {
				new_value = e.set_index_value(step.container, step.index, new_value)!
			}
			.selector {
				new_value = e.set_selector_value(step.container, step.field_name, new_value)!
			}
			else {}
		}
	}
	return e.update_target_flow(resolved.root_id, new_value)
}

fn (mut e Eval) set_index_value(container Value, index Value, value Value) !Value {
	match container {
		ArrayValue {
			mut arr := ArrayValue{
				elem_type_name: container.elem_type_name
				values:         container.values.clone()
			}
			idx := int(e.value_as_int(index)!)
			if idx < 0 || idx >= arr.values.len {
				return error('v3.eval: array index out of bounds')
			}
			arr.values[idx] = e.adapt_value_to_type_name(value, arr.elem_type_name)
			return arr
		}
		MapValue {
			return e.map_set_value(container, index, value)
		}
		else {
			return error('v3.eval: indexed assignment expects array or map')
		}
	}
}

fn (mut e Eval) set_selector_value(container Value, field_name string, value Value) !Value {
	match container {
		StructValue {
			mut st := container
			st.fields[field_name] = e.adapt_value_to_type_name(value, e.struct_field_type_name(container,
				field_name))
			return st
		}
		SumValue {
			mut sv := container
			sv.payload = e.set_selector_value(sv.payload, field_name, value)!
			return sv
		}
		else {
			return error('v3.eval: selector assignment expects struct, got `${e.runtime_type_name(container)}`')
		}
	}
}

fn (mut e Eval) exec_array_append(node &flat.Node) !FlowSignal {
	lhs_id := e.child(node, 0)
	rhs_id := e.child(node, 1)
	resolved := e.resolve_lvalue_flow(lhs_id)!
	if resolved.signal.kind != .normal {
		return resolved.signal
	}
	current := resolved.value
	if current !is ArrayValue {
		return error('v3.eval: `<<` is only supported for arrays')
	}
	mut arr := current as ArrayValue
	rhs_type_name := e.infer_expr_type_name(rhs_id)
	rhs_expected_type := if rhs_type_name.len > 0
		&& e.array_type_spreads_into_elem(rhs_type_name, arr.elem_type_name) {
		rhs_type_name
	} else {
		arr.elem_type_name
	}
	rhs_signal := e.eval_expr_flow_expected(rhs_id, rhs_expected_type)!
	if rhs_signal.kind != .normal {
		return rhs_signal
	}
	rhs := flow_value(rhs_signal)
	if rhs is ArrayValue && e.array_append_rhs_spreads(rhs_id, rhs, arr.elem_type_name) {
		for item in rhs.values {
			arr.values << e.adapt_value_to_type_name(item, arr.elem_type_name)
		}
	} else {
		arr.values << e.adapt_value_to_type_name(rhs, arr.elem_type_name)
	}
	return e.write_resolved_lvalue_flow(resolved, arr)
}

fn (e &Eval) array_append_rhs_spreads(rhs_id flat.NodeId, rhs ArrayValue, elem_type_name string) bool {
	rhs_type_name := e.infer_expr_type_name(rhs_id)
	if rhs_type_name.len > 0 {
		return e.array_type_spreads_into_elem(rhs_type_name, elem_type_name)
	}
	if rhs.elem_type_name.len > 0 {
		return e.type_name_matches(rhs.elem_type_name, elem_type_name)
	}
	return false
}

fn (e &Eval) array_type_spreads_into_elem(array_type_name string, elem_type_name string) bool {
	rhs_elem_type_name := array_elem_type_name_from_type(array_type_name)
	if rhs_elem_type_name.len == 0 {
		return false
	}
	return e.type_name_matches(rhs_elem_type_name, elem_type_name)
}

fn (mut e Eval) exec_if(node &flat.Node) !FlowSignal {
	if node.children_count < 2 {
		return normal_flow()
	}
	cond_id := e.child(node, 0)
	cond_is_guard := e.node(cond_id).kind == .decl_assign
	if cond_is_guard {
		e.open_scope()
	}
	cond_signal := e.eval_condition_flow(cond_id)!
	if cond_signal.kind != .normal {
		if cond_is_guard {
			e.close_scope()!
		}
		return cond_signal
	}
	if e.value_as_bool(flow_value(cond_signal))! {
		mut smartcast_scope := false
		if !cond_is_guard {
			if binding := e.smartcast_binding_from_condition(cond_id) {
				e.open_scope()
				e.declare_var_typed(binding.name, binding.value, binding.type_name)
				smartcast_scope = true
			}
		}
		signal := e.exec_stmt(e.child(node, 1))!
		if smartcast_scope {
			e.close_scope()!
		}
		if cond_is_guard {
			e.close_scope()!
		}
		return signal
	}
	if cond_is_guard {
		e.close_scope()!
	}
	if node.children_count > 2 {
		return e.exec_stmt(e.child(node, 2))
	}
	return normal_flow()
}

fn (mut e Eval) smartcast_binding_from_condition(cond_id flat.NodeId) ?SmartcastBinding {
	if int(cond_id) < 0 {
		return none
	}
	node := e.node(cond_id)
	if node.kind == .paren {
		return e.smartcast_binding_from_condition(e.child(node, 0))
	}
	if node.kind == .infix && node.op == .logical_and {
		return e.smartcast_binding_from_condition(e.child(node, 0))
	}
	if node.kind != .is_expr || node.children_count == 0 {
		return none
	}
	left_id := e.child(node, 0)
	left := e.node(left_id)
	if left.kind != .ident {
		return none
	}
	found := e.lookup_var(left.value)
	if !found.found {
		return none
	}
	if value := e.smartcast_value(found.value, node.value) {
		return SmartcastBinding{
			name:      left.value
			value:     value
			type_name: e.normalize_type_name(node.value)
		}
	}
	return none
}

fn (e &Eval) smartcast_value(value Value, target_type string) ?Value {
	if value is SumValue {
		if e.type_name_matches(value.variant_name, target_type)
			|| e.value_matches_type_name(value.payload, target_type) {
			return value.payload
		}
	}
	return none
}

fn (mut e Eval) eval_condition(id flat.NodeId) !bool {
	signal := e.eval_condition_flow(id)!
	return e.value_as_bool(flow_value(signal))
}

fn (mut e Eval) eval_condition_flow(id flat.NodeId) !FlowSignal {
	node := e.node(id)
	if node.kind == .decl_assign {
		rhs_id := e.child(node, 1)
		rhs_node := e.node(rhs_id)
		mut ok := false
		mut bind_value := Value(void_value())
		mut bind_type := ''
		if rhs_node.kind == .index && rhs_node.value != 'range' && rhs_node.children_count > 1 {
			container_signal := e.eval_expr_flow(e.child(rhs_node, 0))!
			if container_signal.kind != .normal {
				return container_signal
			}
			container := flow_value(container_signal)
			index_signal := if container is MapValue {
				e.eval_expr_flow_expected(e.child(rhs_node, 1), container.key_type_name)!
			} else {
				e.eval_expr_flow(e.child(rhs_node, 1))!
			}
			if index_signal.kind != .normal {
				return index_signal
			}
			if container is MapValue {
				value, found := e.map_lookup(container, flow_value(index_signal))
				ok = found
				bind_value = value
				bind_type = container.value_type_name
			} else {
				value := e.index_value(container, flow_value(index_signal))!
				ok = e.value_is_truthy(value)
				bind_value = e.unwrap_option_like(value)
				bind_type = e.guard_binding_type_name(rhs_id, value)
			}
		} else {
			rhs_signal := e.eval_expr_flow(rhs_id)!
			if rhs_signal.kind != .normal {
				return rhs_signal
			}
			value := flow_value(rhs_signal)
			ok = e.value_is_truthy(value)
			bind_value = e.unwrap_option_like(value)
			bind_type = e.guard_binding_type_name(rhs_id, value)
		}
		if ok {
			e.assign_target_typed(e.child(node, 0), bind_value, true, bind_type)!
		}
		return value_flow(Value(ok))
	}
	signal := e.eval_expr_flow(id)!
	if signal.kind != .normal {
		return signal
	}
	return value_flow(Value(e.value_as_bool(flow_value(signal))!))
}

fn (e &Eval) guard_binding_type_name(rhs_id flat.NodeId, value Value) string {
	rhs_type := e.infer_expr_type_name(rhs_id)
	name := rhs_type.trim_left('&')
	if name.starts_with('?') || name.starts_with('!') {
		return name[1..]
	}
	if name.len > 0 {
		return name
	}
	return e.runtime_type_name(e.unwrap_option_like(value))
}

fn (e &Eval) unwrap_option_like(value Value) Value {
	if value is StructValue {
		if 'data' in value.fields {
			return value.fields['data'] or { void_value() }
		}
	}
	return value
}

fn (e &Eval) value_is_truthy(value Value) bool {
	match value {
		bool {
			return value
		}
		VoidValue {
			return false
		}
		StructValue {
			state := value.fields['state'] or { void_value() }
			if state is i64 {
				return state == 0
			}
			is_error := value.fields['is_error'] or { void_value() }
			if is_error is bool {
				return !is_error
			}
			return true
		}
		else {
			return true
		}
	}
}

fn (mut e Eval) exec_for(node &flat.Node, loop_label string) !FlowSignal {
	if node.children_count < 3 {
		return normal_flow()
	}
	init_id := e.child(node, 0)
	cond_id := e.child(node, 1)
	post_id := e.child(node, 2)
	e.open_scope()
	if int(init_id) >= 0 && e.node(init_id).kind != .empty {
		init_signal := e.exec_stmt(init_id)!
		if init_signal.kind != .normal {
			e.close_scope()!
			return init_signal
		}
	}
	body := e.children(node)
	for {
		if int(cond_id) >= 0 && e.node(cond_id).kind != .empty {
			cond_signal := e.eval_condition_flow(cond_id)!
			if cond_signal.kind != .normal {
				e.close_scope()!
				return cond_signal
			}
			if !e.value_as_bool(flow_value(cond_signal))! {
				break
			}
		}
		e.open_scope()
		signal := e.exec_stmts(body[3..])!
		e.close_scope()!
		if signal.kind == .break_ {
			if e.flow_targets_loop(signal, loop_label) {
				e.close_scope()!
				return normal_flow()
			}
			e.close_scope()!
			return signal
		}
		if signal.kind == .continue_ {
			if !e.flow_targets_loop(signal, loop_label) {
				e.close_scope()!
				return signal
			}
		}
		if signal.kind == .return_ {
			e.close_scope()!
			return signal
		}
		if int(post_id) >= 0 && e.node(post_id).kind != .empty {
			post_signal := e.exec_stmt(post_id)!
			if post_signal.kind != .normal {
				e.close_scope()!
				return post_signal
			}
		}
	}
	e.close_scope()!
	return normal_flow()
}

fn (mut e Eval) exec_for_in(node &flat.Node, loop_label string) !FlowSignal {
	header_count := if node.value.int() > 0 { node.value.int() } else { 3 }
	key_id := e.child(node, 0)
	val_id := e.child(node, 1)
	container_id := e.child(node, 2)
	is_mut_loop := node.op == .amp
	mut resolved_container := ResolvedLvalue{}
	mut has_resolved_container := false
	mut container := Value(void_value())
	if is_mut_loop && e.is_assignable_target(container_id) {
		resolved_container = e.resolve_lvalue_flow(container_id)!
		if resolved_container.signal.kind != .normal {
			return resolved_container.signal
		}
		container = resolved_container.value
		has_resolved_container = true
	} else {
		container_signal := e.eval_expr_flow(container_id)!
		if container_signal.kind != .normal {
			return container_signal
		}
		container = flow_value(container_signal)
	}
	body := e.children(node)[header_count..]
	if header_count == 4 {
		start := e.value_as_int(container)!
		end_signal := e.eval_expr_flow(e.child(node, 3))!
		if end_signal.kind != .normal {
			return end_signal
		}
		end := e.value_as_int(flow_value(end_signal))!
		for i := start; i < end; i++ {
			e.open_scope()
			e.assign_loop_vars(key_id, val_id, Value(i), void_value(), false)
			signal := e.exec_stmts(body)!
			e.close_scope()!
			if signal.kind == .break_ {
				if e.flow_targets_loop(signal, loop_label) {
					return normal_flow()
				}
				return signal
			}
			if signal.kind == .continue_ {
				if e.flow_targets_loop(signal, loop_label) {
					continue
				}
				return signal
			}
			if signal.kind == .return_ {
				return signal
			}
		}
		return normal_flow()
	}
	match container {
		RangeValue {
			last := if container.inclusive { container.end + 1 } else { container.end }
			for i := container.start; i < last; i++ {
				e.open_scope()
				e.assign_loop_vars(key_id, val_id, Value(i), void_value(), false)
				signal := e.exec_stmts(body)!
				e.close_scope()!
				if signal.kind == .break_ {
					if e.flow_targets_loop(signal, loop_label) {
						return normal_flow()
					}
					return signal
				}
				if signal.kind == .continue_ {
					if e.flow_targets_loop(signal, loop_label) {
						continue
					}
					return signal
				}
				if signal.kind == .return_ {
					return signal
				}
			}
		}
		ArrayValue {
			mut arr := ArrayValue{
				elem_type_name: container.elem_type_name
				values:         container.values.clone()
			}
			for i, item in arr.values {
				e.open_scope()
				e.assign_loop_vars(key_id, val_id, Value(i64(i)), item, true)
				signal := e.exec_stmts(body)!
				if is_mut_loop {
					if value := e.loop_mut_value(key_id, val_id, true) {
						arr.values[i] = e.adapt_value_to_type_name(value, arr.elem_type_name)
						mut write_signal := FlowSignal{}
						if has_resolved_container {
							write_signal = e.write_resolved_lvalue_flow(resolved_container, arr) or {
								e.close_scope()!
								return err
							}
						} else {
							write_signal = e.update_target_flow(container_id, arr) or {
								e.close_scope()!
								return err
							}
						}
						if write_signal.kind != .normal {
							e.close_scope()!
							return write_signal
						}
					}
				}
				e.close_scope()!
				if signal.kind == .break_ {
					if e.flow_targets_loop(signal, loop_label) {
						return normal_flow()
					}
					return signal
				}
				if signal.kind == .continue_ {
					if e.flow_targets_loop(signal, loop_label) {
						continue
					}
					return signal
				}
				if signal.kind == .return_ {
					return signal
				}
			}
		}
		MapValue {
			mut m := MapValue{
				key_type_name:   container.key_type_name
				value_type_name: container.value_type_name
				default_value:   container.default_value
				entries:         container.entries.clone()
			}
			for i, entry in m.entries {
				e.open_scope()
				e.assign_loop_vars(key_id, val_id, entry.key, entry.value, true)
				signal := e.exec_stmts(body)!
				if is_mut_loop {
					if value := e.loop_mut_value(key_id, val_id, true) {
						m.entries[i].value = e.adapt_value_to_type_name(value, m.value_type_name)
						mut write_signal := FlowSignal{}
						if has_resolved_container {
							write_signal = e.write_resolved_lvalue_flow(resolved_container, m) or {
								e.close_scope()!
								return err
							}
						} else {
							write_signal = e.update_target_flow(container_id, m) or {
								e.close_scope()!
								return err
							}
						}
						if write_signal.kind != .normal {
							e.close_scope()!
							return write_signal
						}
					}
				}
				e.close_scope()!
				if signal.kind == .break_ {
					if e.flow_targets_loop(signal, loop_label) {
						return normal_flow()
					}
					return signal
				}
				if signal.kind == .continue_ {
					if e.flow_targets_loop(signal, loop_label) {
						continue
					}
					return signal
				}
				if signal.kind == .return_ {
					return signal
				}
			}
		}
		string {
			for i, ch in container {
				e.open_scope()
				e.assign_loop_vars(key_id, val_id, Value(i64(i)), Value(i64(ch)), true)
				signal := e.exec_stmts(body)!
				e.close_scope()!
				if signal.kind == .break_ {
					if e.flow_targets_loop(signal, loop_label) {
						return normal_flow()
					}
					return signal
				}
				if signal.kind == .continue_ {
					if e.flow_targets_loop(signal, loop_label) {
						continue
					}
					return signal
				}
				if signal.kind == .return_ {
					return signal
				}
			}
		}
		else {
			return error('v3.eval: unsupported for-in iterable `${e.runtime_type_name(container)}`')
		}
	}

	return normal_flow()
}

fn (e &Eval) flow_targets_loop(signal FlowSignal, loop_label string) bool {
	return signal.label.len == 0 || signal.label == loop_label
}

fn (mut e Eval) assign_loop_vars(key_id flat.NodeId, val_id flat.NodeId, key Value, value Value, has_value bool) {
	if int(val_id) >= 0 && e.node(val_id).kind != .empty {
		e.assign_target(key_id, key, true) or {}
		e.assign_target(val_id, value, true) or {}
	} else if has_value {
		e.assign_target(key_id, value, true) or {}
	} else {
		e.assign_target(key_id, key, true) or {}
	}
}

fn (e &Eval) loop_mut_value(key_id flat.NodeId, val_id flat.NodeId, has_value bool) ?Value {
	mut value_id := key_id
	if int(val_id) >= 0 && e.node(val_id).kind != .empty {
		value_id = val_id
	} else if !has_value {
		return none
	}
	value_node := e.node(value_id)
	if value_node.kind != .ident {
		return none
	}
	found := e.lookup_var(value_node.value)
	if !found.found {
		return none
	}
	return found.value
}

fn (mut e Eval) eval_expr(id flat.NodeId) !Value {
	if int(id) < 0 {
		return void_value()
	}
	node := e.node(id)
	match node.kind {
		.empty {
			return void_value()
		}
		.int_literal {
			return Value(strconv.parse_int(clean_number_literal(node.value), 0, 64) or { i64(0) })
		}
		.float_literal {
			return Value(strconv.atof64(clean_number_literal(node.value)) or { 0.0 })
		}
		.bool_literal {
			return Value(node.value == 'true')
		}
		.char_literal {
			return Value(e.char_literal_value(node.value))
		}
		.string_literal {
			return Value(node.value)
		}
		.string_interp {
			return flow_value(e.eval_string_interp_flow(node)!)
		}
		.ident {
			return e.eval_ident(node.value)
		}
		.enum_val {
			return e.lookup_const(e.current_module_name(), node.value) or { Value(i64(0)) }
		}
		.paren {
			return e.eval_expr(e.child(node, 0))
		}
		.prefix {
			return e.eval_prefix(node)
		}
		.postfix {
			return e.eval_postfix(node)
		}
		.infix {
			if node.op == .logical_and {
				left := e.eval_expr(e.child(node, 0))!
				if !e.value_as_bool(left)! {
					return Value(false)
				}
				return Value(e.value_as_bool(e.eval_expr(e.child(node, 1))!)!)
			}
			if node.op == .logical_or {
				left := e.eval_expr(e.child(node, 0))!
				if e.value_as_bool(left)! {
					return Value(true)
				}
				return Value(e.value_as_bool(e.eval_expr(e.child(node, 1))!)!)
			}
			return e.apply_infix(node.op, e.eval_expr(e.child(node, 0))!, e.eval_expr(e.child(node,
				1))!)
		}
		.call {
			return flow_value(e.eval_call_flow(id, node)!)
		}
		.selector {
			return e.eval_selector(node)
		}
		.index {
			return e.eval_index(node)
		}
		.if_expr {
			return e.eval_if_value(node)
		}
		.block {
			return e.eval_block_value(node)
		}
		.array_literal {
			mut values := []Value{}
			elem_type_name := e.array_literal_elem_type_name(node)
			for child_id in e.children(node) {
				values << e.adapt_value_to_type_name(e.eval_expr_expected(child_id, elem_type_name)!,
					elem_type_name)
			}
			return ArrayValue{
				elem_type_name: elem_type_name
				values:         values
			}
		}
		.array_init {
			return e.eval_array_init(node)
		}
		.map_init {
			return flow_value(e.eval_map_init_flow(node)!)
		}
		.struct_init {
			return e.eval_struct_init(node)
		}
		.assoc {
			return e.eval_assoc(node)
		}
		.range {
			return RangeValue{
				start: e.value_as_int(e.eval_expr(e.child(node, 0))!)!
				end:   e.value_as_int(e.eval_expr(e.child(node, 1))!)!
			}
		}
		.cast_expr, .as_expr {
			value := e.eval_expr(e.child(node, 0))!
			return e.cast_value(value, node.value)
		}
		.is_expr {
			value := e.eval_expr(e.child(node, 0))!
			return Value(e.value_matches_type_name(value, node.value))
		}
		.in_expr {
			left := e.eval_expr(e.child(node, 0))!
			right := e.eval_expr(e.child(node, 1))!
			return Value(e.value_in(left, right))
		}
		.or_expr {
			return e.eval_or_expr(node)
		}
		.match_stmt {
			return e.eval_match_value(node)
		}
		.none_expr, .nil_literal {
			return void_value()
		}
		.sizeof_expr {
			return Value(e.sizeof_type_name(node.value))
		}
		.typeof_expr {
			value := e.eval_expr(e.child(node, 0))!
			return TypeValue{
				name: e.runtime_type_name(value)
			}
		}
		.fn_literal {
			return Value(e.eval_fn_literal(id, node)!)
		}
		else {
			return error('v3.eval: unsupported expression `${node.kind}`')
		}
	}
}

fn (mut e Eval) eval_fn_literal(id flat.NodeId, node &flat.Node) !FnValue {
	mut captures := map[string]Value{}
	mut capture_types := map[string]string{}
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind != .ident {
			break
		}
		found := e.lookup_var(child.value)
		if !found.found {
			return error('v3.eval: unknown captured variable `${child.value}`')
		}
		captures[child.value] = found.value
		if typ := e.lookup_var_type(child.value) {
			capture_types[child.value] = typ
		}
	}
	return FnValue{
		node:          id
		module_name:   e.current_module_name()
		file_name:     e.current_file_name()
		captures:      captures
		capture_types: capture_types
	}
}

fn (mut e Eval) eval_expr_flow(id flat.NodeId) !FlowSignal {
	if int(id) < 0 {
		return FlowSignal{
			values: [void_value()]
		}
	}
	node := e.node(id)
	match node.kind {
		.or_expr {
			return e.eval_or_expr_flow(node)
		}
		.call {
			return e.eval_call_flow(id, node)
		}
		.index {
			return e.eval_index_flow(node)
		}
		.if_expr {
			return e.eval_if_value_flow(node)
		}
		.match_stmt {
			return e.eval_match_value_flow(node)
		}
		.selector {
			left_signal := e.eval_expr_flow(e.child(node, 0))!
			if left_signal.kind != .normal {
				return left_signal
			}
			return value_flow(e.eval_selector_value(flow_value(left_signal), node.value)!)
		}
		.paren {
			return e.eval_expr_flow(e.child(node, 0))
		}
		.prefix {
			value_signal := e.eval_expr_flow(e.child(node, 0))!
			if value_signal.kind != .normal {
				return value_signal
			}
			return value_flow(e.apply_prefix(node.op, flow_value(value_signal))!)
		}
		.postfix {
			return e.eval_postfix_flow(node)
		}
		.infix {
			return e.eval_infix_flow(node)
		}
		.range {
			start_signal := e.eval_expr_flow(e.child(node, 0))!
			if start_signal.kind != .normal {
				return start_signal
			}
			end_signal := e.eval_expr_flow(e.child(node, 1))!
			if end_signal.kind != .normal {
				return end_signal
			}
			return value_flow(RangeValue{
				start: e.value_as_int(flow_value(start_signal))!
				end:   e.value_as_int(flow_value(end_signal))!
			})
		}
		.cast_expr, .as_expr {
			value_signal := e.eval_expr_flow(e.child(node, 0))!
			if value_signal.kind != .normal {
				return value_signal
			}
			return value_flow(e.cast_value(flow_value(value_signal), node.value)!)
		}
		.is_expr {
			value_signal := e.eval_expr_flow(e.child(node, 0))!
			if value_signal.kind != .normal {
				return value_signal
			}
			return value_flow(Value(e.value_matches_type_name(flow_value(value_signal), node.value)))
		}
		.in_expr {
			left_signal := e.eval_expr_flow(e.child(node, 0))!
			if left_signal.kind != .normal {
				return left_signal
			}
			right_signal := e.eval_expr_flow(e.child(node, 1))!
			if right_signal.kind != .normal {
				return right_signal
			}
			return value_flow(Value(e.value_in(flow_value(left_signal), flow_value(right_signal))))
		}
		.array_literal {
			mut values := []Value{}
			elem_type_name := e.array_literal_elem_type_name(node)
			for child_id in e.children(node) {
				child_signal := e.eval_expr_flow_expected(child_id, elem_type_name)!
				if child_signal.kind != .normal {
					return child_signal
				}
				values << e.adapt_value_to_type_name(flow_value(child_signal), elem_type_name)
			}
			return value_flow(ArrayValue{
				elem_type_name: elem_type_name
				values:         values
			})
		}
		.array_init {
			return e.eval_array_init_flow(node)
		}
		.struct_init {
			return e.eval_struct_init_flow(node)
		}
		.map_init {
			return e.eval_map_init_flow(node)
		}
		.assoc {
			return e.eval_assoc_flow(node)
		}
		.string_interp {
			return e.eval_string_interp_flow(node)
		}
		else {
			return FlowSignal{
				values: [e.eval_expr(id)!]
			}
		}
	}
}

fn (mut e Eval) eval_expr_expected(id flat.NodeId, expected_type string) !Value {
	signal := e.eval_expr_flow_expected(id, expected_type)!
	if signal.kind != .normal {
		return flow_value(signal)
	}
	return flow_value(signal)
}

fn (mut e Eval) eval_expr_flow_expected(id flat.NodeId, expected_type string) !FlowSignal {
	if int(id) >= 0 {
		node := e.node(id)
		if node.kind == .enum_val && expected_type.len > 0 {
			if value := e.lookup_enum_value(expected_type, node.value) {
				return value_flow(value)
			}
		}
		if expected_type.len > 0 {
			match node.kind {
				.if_expr {
					return e.eval_if_value_flow_expected(node, expected_type)
				}
				.match_stmt {
					return e.eval_match_value_flow_expected(node, expected_type)
				}
				.block {
					return e.eval_block_value_flow_expected(node, expected_type)
				}
				.paren {
					return e.eval_expr_flow_expected(e.child(node, 0), expected_type)
				}
				else {}
			}
		}
	}
	return e.eval_expr_flow(id)
}

fn (e &Eval) current_return_type() string {
	if e.call_stack.len == 0 {
		return ''
	}
	return e.call_stack[e.call_stack.len - 1].return_type
}

fn return_child_expected_type(return_type string, return_types []string, child_index int, child_count int) string {
	if return_types.len > 0 {
		if child_count == return_types.len && child_index < return_types.len {
			return return_types[child_index]
		}
		return ''
	}
	if child_count == 1 {
		return return_type
	}
	return ''
}

fn (mut e Eval) eval_infix_flow(node &flat.Node) !FlowSignal {
	left_signal := e.eval_expr_flow(e.child(node, 0))!
	if left_signal.kind != .normal {
		return left_signal
	}
	left := flow_value(left_signal)
	if node.op == .logical_and {
		if !e.value_as_bool(left)! {
			return value_flow(Value(false))
		}
		mut smartcast_scope := false
		if binding := e.smartcast_binding_from_condition(e.child(node, 0)) {
			e.open_scope()
			e.declare_var_typed(binding.name, binding.value, binding.type_name)
			smartcast_scope = true
		}
		right_signal := e.eval_expr_flow(e.child(node, 1))!
		if smartcast_scope {
			e.close_scope()!
		}
		if right_signal.kind != .normal {
			return right_signal
		}
		return value_flow(Value(e.value_as_bool(flow_value(right_signal))!))
	}
	if node.op == .logical_or {
		if e.value_as_bool(left)! {
			return value_flow(Value(true))
		}
		right_signal := e.eval_expr_flow(e.child(node, 1))!
		if right_signal.kind != .normal {
			return right_signal
		}
		return value_flow(Value(e.value_as_bool(flow_value(right_signal))!))
	}
	right_signal := e.eval_expr_flow(e.child(node, 1))!
	if right_signal.kind != .normal {
		return right_signal
	}
	return value_flow(e.apply_infix(node.op, left, flow_value(right_signal))!)
}

fn clean_number_literal(value string) string {
	return value.replace('_', '')
}

fn (e &Eval) char_literal_value(raw string) i64 {
	mut s := raw
	if s.len >= 2 && ((s[0] == `'` && s[s.len - 1] == `'`)
		|| (s[0] == `\`` && s[s.len - 1] == `\``)) {
		s = s[1..s.len - 1]
	}
	if s.len == 0 {
		return 0
	}
	if s.len >= 2 && s[0] == `\\` {
		return match s[1] {
			`n` { i64(`\n`) }
			`t` { i64(`\t`) }
			`r` { i64(`\r`) }
			`\\` { i64(`\\`) }
			`'` { i64(`'`) }
			`"` { i64(`"`) }
			`$` { i64(`$`) }
			`0` { i64(0) }
			`a` { i64(7) }
			`b` { i64(8) }
			`f` { i64(12) }
			`v` { i64(11) }
			else { i64(s[0]) }
		}
	}
	return i64(s[0])
}

fn (mut e Eval) eval_ident(name string) !Value {
	if name == '_' {
		return void_value()
	}
	found := e.lookup_var(name)
	if found.found {
		return found.value
	}
	if value := e.lookup_const(e.current_module_name(), name) {
		return value
	}
	mod_name := e.resolve_module_name(name)
	if mod_name in e.modules || name in ['os', 'time', 'strings'] {
		return ModuleValue{
			name: mod_name
		}
	}
	if e.has_type_name(e.current_module_name(), name) {
		return TypeValue{
			name: e.qualify_type_name(e.current_module_name(), name)
		}
	}
	return e.unknown_variable_error(name)
}

fn (e &Eval) resolve_module_name(name string) string {
	file_name := e.current_file_name()
	if file_name in e.file_import_alias {
		aliases := e.file_import_alias[file_name].clone()
		if name in aliases {
			return aliases[name]
		}
	}
	return name
}

fn (e &Eval) has_type_name(module_name string, name string) bool {
	if module_name in e.type_names && name in e.type_names[module_name] {
		return true
	}
	if name.contains('.') {
		mod := name.all_before_last('.')
		short := name.all_after_last('.')
		return mod in e.type_names && short in e.type_names[mod]
	}
	return name in e.structs || name in e.sum_types
}

fn (mut e Eval) lookup_const(module_name string, name string) ?Value {
	if module_name in e.consts && name in e.consts[module_name] {
		mut entry := e.consts[module_name][name]
		if entry.cached {
			return entry.value
		}
		if entry.evaluating {
			return none
		}
		entry.evaluating = true
		e.consts[module_name][name] = entry
		mut value := Value(void_value())
		if e.node(entry.node).children_count > 0 {
			e.call_stack << CallFrame{
				module_name: entry.module_name
				file_name:   entry.file_name
				fn_name:     '<const ${name}>'
			}
			value = e.eval_expr(e.child(e.node(entry.node), 0)) or {
				e.call_stack.delete(e.call_stack.len - 1)
				entry.evaluating = false
				e.consts[module_name][name] = entry
				return none
			}
			e.call_stack.delete(e.call_stack.len - 1)
		}
		entry.cached = true
		entry.evaluating = false
		entry.value = value
		e.consts[module_name][name] = entry
		return value
	}
	for mod_name in e.consts.keys() {
		if name in e.consts[mod_name] {
			return e.lookup_const(mod_name, name)
		}
	}
	return none
}

fn (mut e Eval) eval_call_flow(id flat.NodeId, node &flat.Node) !FlowSignal {
	if node.children_count == 0 {
		return FlowSignal{
			values: [void_value()]
		}
	}
	callee_id := e.child(node, 0)
	callee := e.node(callee_id)
	if value := e.disabled_call_value(node, callee) {
		return FlowSignal{
			values: [value]
		}
	}
	if callee.kind == .ident {
		fn_name := callee.value
		found := e.lookup_var(fn_name)
		if found.found && found.value is FnValue {
			fv := found.value as FnValue
			args_signal := e.eval_call_arg_values(node, e.fn_value_param_type_names(fv))!
			if args_signal.kind != .normal {
				return args_signal
			}
			result := e.call_fn_value(fv, args_signal.values)!
			e.write_back_fn_value(callee_id, result)!
			e.write_back_mutated_arg_values(node, result.mutated_args, args_signal.mut_lvalues)!
			return FlowSignal{
				values: [e.call_result_value(result)]
			}
		}
		expected_types := if target := e.function_def(e.current_module_name(), fn_name) {
			e.function_param_type_names(target, 0)
		} else {
			[]string{}
		}
		args_signal := e.eval_call_arg_values(node, expected_types)!
		if args_signal.kind != .normal {
			return args_signal
		}
		result := e.call_function(e.current_module_name(), fn_name, args_signal.values)!
		e.write_back_mutated_args(node, result, args_signal.mut_lvalues)!
		return FlowSignal{
			values: [e.call_result_value(result)]
		}
	}
	if callee.kind == .selector {
		receiver_id := e.child(callee, 0)
		receiver_resolved := e.resolve_lvalue_flow(receiver_id)!
		if receiver_resolved.signal.kind != .normal {
			return receiver_resolved.signal
		}
		left := receiver_resolved.value
		if left is ModuleValue {
			expected_types := if target := e.function_def(left.name, callee.value) {
				e.function_param_type_names(target, 0)
			} else {
				[]string{}
			}
			args_signal := e.eval_call_arg_values(node, expected_types)!
			if args_signal.kind != .normal {
				return args_signal
			}
			result := e.call_function(left.name, callee.value, args_signal.values)!
			e.write_back_mutated_args(node, result, args_signal.mut_lvalues)!
			return FlowSignal{
				values: [e.call_result_value(result)]
			}
		}
		if left is TypeValue {
			static_name := '${left.name.all_after_last('.')}.${callee.value}'
			static_module := e.type_value_module_name(left)
			expected_types := if target := e.function_def(static_module, static_name) {
				e.function_param_type_names(target, 0)
			} else {
				[]string{}
			}
			args_signal := e.eval_call_arg_values(node, expected_types)!
			if args_signal.kind != .normal {
				return args_signal
			}
			result := e.call_function(static_module, static_name, args_signal.values)!
			e.write_back_mutated_args(node, result, args_signal.mut_lvalues)!
			return FlowSignal{
				values: [e.call_result_value(result)]
			}
		}
		selector_value := e.eval_selector_value(left, callee.value) or { void_value() }
		if selector_value is FnValue {
			args_signal :=
				e.eval_call_arg_values(node, e.fn_value_param_type_names(selector_value))!
			if args_signal.kind != .normal {
				return args_signal
			}
			result := e.call_fn_value(selector_value, args_signal.values)!
			e.write_back_fn_value(callee_id, result)!
			e.write_back_mutated_arg_values(node, result.mutated_args, args_signal.mut_lvalues)!
			return FlowSignal{
				values: [e.call_result_value(result)]
			}
		}
		receiver_type_name := e.infer_expr_type_name(receiver_id)
		expected_types := e.method_call_param_type_names(left, receiver_type_name, callee.value)
		args_signal := e.eval_call_arg_values(node, expected_types)!
		if args_signal.kind != .normal {
			return args_signal
		}
		result := e.call_value_method(left, receiver_type_name, callee.value, args_signal.values)!
		if result.receiver_changed {
			if e.is_assignable_target(receiver_id) {
				write_signal := e.write_resolved_lvalue_flow(receiver_resolved, result.receiver)!
				if write_signal.kind != .normal {
					return write_signal
				}
			}
		}
		e.write_back_mutated_arg_values(node, result.mutated_args, args_signal.mut_lvalues)!
		return FlowSignal{
			values: [result.value]
		}
	}
	if callee.kind == .fn_literal {
		fv := e.eval_fn_literal(callee_id, callee)!
		args_signal := e.eval_call_arg_values(node, e.fn_literal_param_type_names(callee))!
		if args_signal.kind != .normal {
			return args_signal
		}
		result := e.call_fn_value(fv, args_signal.values)!
		e.write_back_mutated_arg_values(node, result.mutated_args, args_signal.mut_lvalues)!
		return FlowSignal{
			values: [e.call_result_value(result)]
		}
	}
	value := e.eval_expr(callee_id)!
	if value is FnValue {
		args_signal := e.eval_call_arg_values(node, e.fn_value_param_type_names(value))!
		if args_signal.kind != .normal {
			return args_signal
		}
		result := e.call_fn_value(value, args_signal.values)!
		e.write_back_fn_value(callee_id, result)!
		e.write_back_mutated_arg_values(node, result.mutated_args, args_signal.mut_lvalues)!
		return FlowSignal{
			values: [e.call_result_value(result)]
		}
	}
	_ = id
	return error('v3.eval: unsupported call target `${callee.kind}`')
}

fn (mut e Eval) disabled_call_value(call_node &flat.Node, callee &flat.Node) ?Value {
	match callee.kind {
		.ident {
			found := e.lookup_var(callee.value)
			if found.found && found.value is FnValue {
				return none
			}
			if value := e.disabled_function_zero_value(e.current_module_name(), callee.value,
				call_node)
			{
				return value
			}
		}
		.selector {
			if callee.children_count == 0 {
				return none
			}
			base_id := e.child(callee, 0)
			base := e.node(base_id)
			receiver_type := e.infer_expr_type_name(base_id)
			if receiver_type.len > 0 {
				if target := e.resolve_method_target(receiver_type, callee.value) {
					if e.is_disabled_fn_name_in_module(target.module_name, target.name) {
						return e.zero_value_for_disabled_function(call_node, target)
					}
				}
			}
			if base.kind == .ident {
				full_name := '${base.value}.${callee.value}'
				if value := e.disabled_function_zero_value(e.current_module_name(), full_name,
					call_node)
				{
					return value
				}
				found := e.lookup_var(base.value)
				if !found.found {
					module_name := e.resolve_module_name(base.value)
					if value := e.disabled_function_zero_value(module_name, callee.value, call_node) {
						return value
					}
				}
			}
		}
		else {}
	}

	return none
}

fn (e &Eval) function_def(module_name string, fn_name string) ?FunctionDef {
	target_module := if module_name == '' { e.current_module_name() } else { module_name }
	if target_module !in e.functions {
		return none
	}
	if fn_name in e.functions[target_module] {
		return e.functions[target_module][fn_name]
	}
	if target_module !in ['main', 'builtin'] && !fn_name.contains('.') {
		qualified := '${target_module}.${fn_name}'
		if qualified in e.functions[target_module] {
			return e.functions[target_module][qualified]
		}
	}
	return none
}

fn (e &Eval) function_param_type_names(def FunctionDef, skip int) []string {
	node := e.node(def.node)
	mut types := []string{}
	mut param_index := 0
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind != .param {
			break
		}
		if param_index >= skip {
			types << e.qualify_expected_type_name(def.module_name, child.typ)
		}
		param_index++
	}
	return types
}

fn (e &Eval) qualify_expected_type_name(module_name string, type_name string) string {
	name := type_name.trim_space()
	if name == '' {
		return ''
	}
	if name.starts_with('&') {
		return '&${e.qualify_expected_type_name(module_name, name[1..])}'
	}
	if name.starts_with('...') {
		return '...${e.qualify_expected_type_name(module_name, name[3..])}'
	}
	if name.starts_with('mut ') {
		return '&${e.qualify_expected_type_name(module_name, name[4..])}'
	}
	return e.qualify_nested_type_name(module_name, name)
}

fn (e &Eval) fn_value_param_type_names(fv FnValue) []string {
	return e.fn_literal_param_type_names_in_module(e.node(fv.node), fv.module_name)
}

fn (e &Eval) fn_literal_param_type_names(node &flat.Node) []string {
	return e.fn_literal_param_type_names_in_module(node, e.current_module_name())
}

fn (e &Eval) fn_literal_param_type_names_in_module(node &flat.Node, module_name string) []string {
	children := e.children(node)
	mut i := 0
	for i < children.len {
		child := e.node(children[i])
		if child.kind != .ident {
			break
		}
		i++
	}
	mut types := []string{}
	for i < children.len {
		child := e.node(children[i])
		if child.kind != .param {
			break
		}
		types << e.qualify_expected_type_name(module_name, child.typ)
		i++
	}
	return types
}

fn (e &Eval) method_call_param_type_names(receiver Value, receiver_type_name string, method_name string) []string {
	static_type_name := e.normalize_type_name(receiver_type_name)
	if static_type_name.len > 0 {
		if target := e.resolve_method_target(static_type_name, method_name) {
			return e.function_param_type_names(target, 1)
		}
	}
	if receiver is StructValue {
		if target := e.resolve_method_target(receiver.type_name, method_name) {
			return e.function_param_type_names(target, 1)
		}
	}
	if receiver is ArrayValue {
		return e.array_method_param_type_names(receiver, method_name)
	}
	if receiver is MapValue {
		return e.map_method_param_type_names(receiver, method_name)
	}
	if receiver is SumValue {
		return e.method_call_param_type_names(receiver.payload, receiver.variant_name, method_name)
	}
	return []string{}
}

fn (e &Eval) array_method_param_type_names(receiver ArrayValue, method_name string) []string {
	return match method_name {
		'contains', 'index' { [receiver.elem_type_name] }
		else { []string{} }
	}
}

fn (e &Eval) map_method_param_type_names(receiver MapValue, method_name string) []string {
	return match method_name {
		'delete' { [receiver.key_type_name] }
		else { []string{} }
	}
}

fn (mut e Eval) eval_call_arg_values(node &flat.Node, expected_types []string) !FlowSignal {
	mut args := []Value{}
	mut mut_lvalues := map[int]ResolvedLvalue{}
	for child_index in 1 .. node.children_count {
		arg_index := child_index - 1
		expected_type := call_arg_expected_type(expected_types, arg_index)
		value_expected_type := call_arg_value_expected_type(expected_type)
		arg_id := e.child(node, child_index)
		if expected_type.starts_with('&') && e.is_assignable_target(arg_id) {
			resolved := e.resolve_lvalue_flow(arg_id)!
			if resolved.signal.kind != .normal {
				return resolved.signal
			}
			args << resolved.value
			mut_lvalues[arg_index] = resolved
			continue
		}
		signal := e.eval_expr_flow_expected(arg_id, value_expected_type)!
		if signal.kind != .normal {
			return signal
		}
		args << flow_value(signal)
	}
	return FlowSignal{
		values:      args
		mut_lvalues: mut_lvalues
	}
}

fn call_arg_expected_type(expected_types []string, index int) string {
	if index < expected_types.len {
		typ := expected_types[index]
		if typ.starts_with('...') {
			return typ[3..]
		}
		return typ
	}
	if expected_types.len > 0 {
		last := expected_types[expected_types.len - 1]
		if last.starts_with('...') {
			return last[3..]
		}
	}
	return ''
}

fn call_arg_value_expected_type(type_name string) string {
	name := type_name.trim_space()
	if name.starts_with('&') {
		return call_arg_value_expected_type(name[1..])
	}
	if name.starts_with('mut ') {
		return call_arg_value_expected_type(name[4..])
	}
	if name.starts_with('...') {
		return call_arg_value_expected_type(name[3..])
	}
	return name
}

fn (mut e Eval) disabled_function_zero_value(module_name string, fn_name string, call_node &flat.Node) ?Value {
	target_module := if module_name == '' { e.current_module_name() } else { module_name }
	mut candidates := []string{cap: 3}
	candidates << fn_name
	if target_module.len > 0 && target_module !in ['main', 'builtin'] && !fn_name.contains('.') {
		candidates << '${target_module}.${fn_name}'
	}
	if target_module in e.functions {
		for candidate in candidates {
			if candidate in e.functions[target_module] {
				target := e.functions[target_module][candidate]
				if e.is_disabled_fn_name_in_module(target.module_name, target.name)
					|| e.is_disabled_fn_name_in_module(target_module, candidate) {
					return e.zero_value_for_disabled_function(call_node, target)
				}
			}
		}
	}
	for candidate in candidates {
		if e.is_disabled_fn_name_in_module(target_module, candidate) {
			return e.zero_value_for_disabled_return_type(call_node.typ, target_module)
		}
	}
	return none
}

fn (mut e Eval) zero_value_for_disabled_function(call_node &flat.Node, target FunctionDef) Value {
	target_node := e.node(target.node)
	return e.zero_value_for_disabled_return_type(if call_node.typ.len > 0 {
		call_node.typ
	} else {
		target_node.typ
	}, target.module_name)
}

fn (mut e Eval) zero_value_for_disabled_return_type(return_type string, module_name string) Value {
	if return_type.len == 0 || return_type == 'void' {
		return void_value()
	}
	return e.zero_value_for_type_name_in_module(return_type, module_name)
}

fn (e &Eval) is_disabled_fn_name_in_module(module_name string, fn_name string) bool {
	if fn_name in e.a.disabled_fns {
		return true
	}
	if !fn_name.contains('.') && module_name.len > 0 && module_name !in ['main', 'builtin'] {
		return '${module_name}.${fn_name}' in e.a.disabled_fns
	}
	return false
}

fn (e &Eval) call_result_value(result CallResult) Value {
	if result.values.len == 1 {
		return result.values[0]
	}
	return TupleValue{
		values: result.values
	}
}

fn (mut e Eval) write_back_mutated_args(node &flat.Node, result CallResult, mut_lvalues map[int]ResolvedLvalue) ! {
	e.write_back_mutated_arg_values(node, result.mutated_args, mut_lvalues)!
}

fn (mut e Eval) adapt_return_values(values []Value, return_type string) []Value {
	return_types := split_multi_return_types(return_type)
	if return_types.len > 0 {
		mut adapted := []Value{cap: values.len}
		for i, value in values {
			adapted << if i < return_types.len {
				e.adapt_value_to_type_name(value, return_types[i])
			} else {
				value
			}
		}
		return adapted
	}
	if values.len == 1 && return_type.len > 0 {
		return [e.adapt_value_to_type_name(values[0], return_type)]
	}
	return values.clone()
}

fn split_multi_return_types(return_type string) []string {
	if !return_type.starts_with('(') || !return_type.ends_with(')') {
		return []string{}
	}
	inner := return_type[1..return_type.len - 1]
	mut types := []string{}
	mut start := 0
	mut paren_depth := 0
	mut bracket_depth := 0
	for i := 0; i < inner.len; i++ {
		ch := inner[i]
		match ch {
			`(` {
				paren_depth++
			}
			`)` {
				if paren_depth > 0 {
					paren_depth--
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			`,` {
				if paren_depth == 0 && bracket_depth == 0 {
					types << inner[start..i].trim_space()
					start = i + 1
				}
			}
			else {}
		}
	}
	last := inner[start..].trim_space()
	if last.len > 0 {
		types << last
	}
	return if types.len > 1 { types } else { []string{} }
}

fn (e &Eval) minimum_arg_count(params []flat.NodeId) int {
	if params.len == 0 {
		return 0
	}
	last_param := e.node(params[params.len - 1])
	if last_param.typ.starts_with('...') {
		return params.len - 1
	}
	return params.len
}

fn (mut e Eval) bind_call_params(params []flat.NodeId, args []Value) ! {
	for i, param_id in params {
		param := e.node(param_id)
		if param.typ.starts_with('...') {
			elem_type := param.typ[3..]
			mut values := []Value{cap: if args.len > i { args.len - i } else { 0 }}
			for j := i; j < args.len; j++ {
				values << e.adapt_value_to_type_name(args[j], elem_type)
			}
			if param.value.len > 0 {
				e.declare_var_typed(param.value, ArrayValue{
					elem_type_name: elem_type
					values:         values
				}, '[]${elem_type}')
			}
			return
		}
		if i < args.len {
			e.declare_var_typed(param.value, e.adapt_value_to_type_name(args[i], param.typ),
				param.typ)
		}
	}
}

fn (mut e Eval) collect_mutated_param_args(params []flat.NodeId, args []Value) map[int]Value {
	mut mutated_args := map[int]Value{}
	for i, param_id in params {
		param := e.node(param_id)
		if param.value.len == 0 {
			continue
		}
		if param.typ.starts_with('...') {
			continue
		}
		found := e.lookup_var(param.value)
		if !found.found {
			continue
		}
		if param.typ.starts_with('&') {
			mutated_args[i] = found.value
			continue
		}
		if i < args.len && args[i] is FnValue && found.value is FnValue {
			fn_value := found.value as FnValue
			if fn_value.captures.len > 0 {
				mutated_args[i] = found.value
			}
		}
	}
	return mutated_args
}

fn (mut e Eval) write_back_fn_value(callee_id flat.NodeId, result CallResult) ! {
	if result.fn_value_changed && e.is_assignable_target(callee_id) {
		e.update_target(callee_id, Value(result.fn_value))!
	}
}

fn (mut e Eval) write_back_mutated_arg_values(node &flat.Node, mutated_args map[int]Value, mut_lvalues map[int]ResolvedLvalue) ! {
	for child_index := 1; child_index < node.children_count; child_index++ {
		arg_index := child_index - 1
		if arg_index !in mutated_args {
			continue
		}
		value := mutated_args[arg_index] or { continue }
		if resolved := mut_lvalues[arg_index] {
			signal := e.write_resolved_lvalue_flow(resolved, value)!
			if signal.kind != .normal {
				return error('v3.eval: unexpected `${signal.kind}` escaped mut argument writeback')
			}
			continue
		}
		arg_id := e.child(node, child_index)
		if e.is_assignable_target(arg_id) {
			e.update_target(arg_id, value)!
		}
	}
}

fn (e &Eval) is_assignable_target(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := e.node(id)
	return node.kind in [.ident, .selector, .index]
}

fn (mut e Eval) call_fn_value(fv FnValue, args []Value) !CallResult {
	node := e.node(fv.node)
	mut params := []flat.NodeId{}
	mut body_start := 0
	children := e.children(node)
	for body_start < children.len {
		child := e.node(children[body_start])
		if child.kind != .ident {
			break
		}
		body_start++
	}
	for body_start < children.len {
		child_id := children[body_start]
		child := e.node(child_id)
		if child.kind == .param {
			params << child_id
			body_start++
			continue
		}
		break
	}
	e.open_scope()
	e.call_stack << CallFrame{
		module_name: fv.module_name
		file_name:   fv.file_name
		fn_name:     '<anonymous>'
		return_type: node.typ
		scope_idx:   e.scopes.len - 1
	}
	for name, value in fv.captures {
		e.declare_var_typed(name, value, fv.capture_types[name] or { '' })
	}
	e.bind_call_params(params, args)!
	signal := e.exec_stmts(children[body_start..])!
	e.run_deferred_stmts()!
	mutated_args := e.collect_mutated_param_args(params, args)
	mut updated_fn_value := fv
	if fv.captures.len > 0 {
		mut captures := fv.captures.clone()
		for name, _ in fv.captures {
			found := e.lookup_var(name)
			if found.found {
				captures[name] = found.value
			}
		}
		updated_fn_value = FnValue{
			node:          fv.node
			module_name:   fv.module_name
			file_name:     fv.file_name
			captures:      captures
			capture_types: fv.capture_types.clone()
		}
	}
	adapted_values := if signal.kind == .return_ {
		e.adapt_return_values(signal.values, node.typ)
	} else {
		[]Value{}
	}
	e.close_scope()!
	e.call_stack.delete(e.call_stack.len - 1)
	if signal.kind == .return_ {
		return CallResult{
			values:           adapted_values
			mutated_args:     mutated_args
			fn_value_changed: fv.captures.len > 0
			fn_value:         updated_fn_value
		}
	}
	if signal.kind != .normal {
		return error('v3.eval: unexpected `${signal.kind}` escaped anonymous function')
	}
	return CallResult{
		values:           [void_value()]
		mutated_args:     mutated_args
		fn_value_changed: fv.captures.len > 0
		fn_value:         updated_fn_value
	}
}

fn (mut e Eval) eval_selector(node &flat.Node) !Value {
	left := e.eval_expr(e.child(node, 0))!
	return e.eval_selector_value(left, node.value)
}

fn (mut e Eval) eval_selector_value(left Value, field string) !Value {
	match left {
		ModuleValue {
			if value := e.lookup_const(left.name, field) {
				return value
			}
			return TypeValue{
				name: '${left.name}.${field}'
			}
		}
		TypeValue {
			if field == 'name' {
				return Value(left.name)
			}
			mod := e.type_value_module_name(left)
			enum_key := '${left.name.all_after_last('.')}.${field}'
			if value := e.lookup_const(mod, enum_key) {
				return e.enum_value(left.name, value)
			}
			if value := e.lookup_const(mod, field) {
				return e.enum_value(left.name, value)
			}
			return TypeValue{
				name: '${left.name}.${field}'
			}
		}
		ArrayValue {
			if field == 'len' {
				return Value(i64(left.values.len))
			}
			if field == 'cap' {
				return Value(i64(left.values.cap))
			}
		}
		MapValue {
			if field == 'len' {
				return Value(i64(left.entries.len))
			}
		}
		string {
			if field == 'len' {
				return Value(i64(left.len))
			}
		}
		StructValue {
			if field in left.fields {
				return left.fields[field] or { void_value() }
			}
		}
		SumValue {
			return e.eval_sum_selector(left, field)
		}
		else {}
	}

	return error('v3.eval: unsupported selector `${field}` on `${e.runtime_type_name(left)}`')
}

fn (e &Eval) eval_sum_selector(value SumValue, field string) !Value {
	if field == '_typ' {
		return Value(i64(e.sum_variant_index(value.type_name, value.variant_name)))
	}
	if value.payload is StructValue {
		payload := value.payload as StructValue
		if field in payload.fields {
			return payload.fields[field] or { void_value() }
		}
	}
	return error('v3.eval: unknown sum type field `${field}` on `${value.type_name}`')
}

fn (e &Eval) sum_variant_index(type_name string, variant_name string) int {
	if type_name in e.sum_types {
		variants := e.sum_types[type_name]
		sum_module := if type_name.contains('.') {
			type_name.all_before_last('.')
		} else {
			e.current_module_name()
		}
		expected_name := e.qualify_type_name(sum_module, variant_name)
		for i, variant in variants {
			if variant == expected_name || variant == variant_name {
				return i
			}
		}
		mut short_match_index := -1
		for i, variant in variants {
			if variant.all_after_last('.') == variant_name.all_after_last('.') {
				if short_match_index >= 0 {
					return 0
				}
				short_match_index = i
			}
		}
		if short_match_index >= 0 {
			return short_match_index
		}
	}
	return 0
}

fn (mut e Eval) eval_index(node &flat.Node) !Value {
	container := e.eval_expr(e.child(node, 0))!
	if node.value == 'range' {
		start := if node.children_count > 1 && e.node(e.child(node, 1)).kind != .empty {
			int(e.value_as_int(e.eval_expr(e.child(node, 1))!)!)
		} else {
			0
		}
		mut end := 0
		match container {
			ArrayValue { end = container.values.len }
			string { end = container.len }
			else {}
		}

		if node.children_count > 2 {
			end = int(e.value_as_int(e.eval_expr(e.child(node, 2))!)!)
		}
		return e.slice_value(container, start, end)!
	}
	index := if container is MapValue {
		e.eval_expr_expected(e.child(node, 1), container.key_type_name)!
	} else {
		e.eval_expr(e.child(node, 1))!
	}
	return e.index_value(container, index)
}

fn (mut e Eval) eval_index_flow(node &flat.Node) !FlowSignal {
	container_signal := e.eval_expr_flow(e.child(node, 0))!
	if container_signal.kind != .normal {
		return container_signal
	}
	container := flow_value(container_signal)
	if node.value == 'range' {
		mut start := 0
		if node.children_count > 1 && e.node(e.child(node, 1)).kind != .empty {
			start_signal := e.eval_expr_flow(e.child(node, 1))!
			if start_signal.kind != .normal {
				return start_signal
			}
			start = int(e.value_as_int(flow_value(start_signal))!)
		}
		mut end := 0
		match container {
			ArrayValue { end = container.values.len }
			string { end = container.len }
			else {}
		}

		if node.children_count > 2 {
			end_signal := e.eval_expr_flow(e.child(node, 2))!
			if end_signal.kind != .normal {
				return end_signal
			}
			end = int(e.value_as_int(flow_value(end_signal))!)
		}
		return value_flow(e.slice_value(container, start, end)!)
	}
	index_signal := if container is MapValue {
		e.eval_expr_flow_expected(e.child(node, 1), container.key_type_name)!
	} else {
		e.eval_expr_flow(e.child(node, 1))!
	}
	if index_signal.kind != .normal {
		return index_signal
	}
	return value_flow(e.index_value(container, flow_value(index_signal))!)
}

fn (mut e Eval) index_value(container Value, index Value) !Value {
	match container {
		ArrayValue {
			idx := int(e.value_as_int(index)!)
			if idx < 0 || idx >= container.values.len {
				return error('v3.eval: array index out of bounds')
			}
			return container.values[idx]
		}
		MapValue {
			value, ok := e.map_lookup(container, e.adapt_value_to_type_name(index,
				container.key_type_name))
			if ok {
				return value
			}
			return container.default_value
		}
		string {
			idx := int(e.value_as_int(index)!)
			if idx < 0 || idx >= container.len {
				return error('v3.eval: string index out of bounds')
			}
			return Value(i64(container[idx]))
		}
		else {
			return error('v3.eval: unsupported index target `${e.runtime_type_name(container)}`')
		}
	}
}

fn (e &Eval) slice_value(container Value, start int, end int) !Value {
	match container {
		ArrayValue {
			mut arr := container
			arr.values = arr.values[start..end].clone()
			return arr
		}
		string {
			sliced := container[start..end]
			return Value(sliced)
		}
		else {
			return error('v3.eval: slicing is only supported for arrays and strings')
		}
	}
}

fn (mut e Eval) eval_if_value(node &flat.Node) !Value {
	signal := e.eval_if_value_flow(node)!
	return flow_values_value(signal.values)
}

fn (mut e Eval) eval_if_value_flow(node &flat.Node) !FlowSignal {
	return e.eval_if_value_flow_expected(node, '')
}

fn (mut e Eval) eval_if_value_flow_expected(node &flat.Node, expected_type string) !FlowSignal {
	if node.children_count < 2 {
		return value_flow(void_value())
	}
	cond_id := e.child(node, 0)
	cond_is_guard := e.node(cond_id).kind == .decl_assign
	if cond_is_guard {
		e.open_scope()
	}
	cond_signal := e.eval_condition_flow(cond_id)!
	if cond_signal.kind != .normal {
		if cond_is_guard {
			e.close_scope()!
		}
		return cond_signal
	}
	if e.value_as_bool(flow_value(cond_signal))! {
		mut smartcast_scope := false
		if !cond_is_guard {
			if binding := e.smartcast_binding_from_condition(cond_id) {
				e.open_scope()
				e.declare_var_typed(binding.name, binding.value, binding.type_name)
				smartcast_scope = true
			}
		}
		signal := e.eval_value_expr_flow_expected(e.child(node, 1), expected_type)!
		if smartcast_scope {
			e.close_scope()!
		}
		if cond_is_guard {
			e.close_scope()!
		}
		return signal
	}
	if cond_is_guard {
		e.close_scope()!
	}
	if node.children_count > 2 {
		return e.eval_value_expr_flow_expected(e.child(node, 2), expected_type)
	}
	return value_flow(void_value())
}

fn (mut e Eval) eval_value_expr_flow(id flat.NodeId) !FlowSignal {
	return e.eval_value_expr_flow_expected(id, '')
}

fn (mut e Eval) eval_value_expr_flow_expected(id flat.NodeId, expected_type string) !FlowSignal {
	if int(id) >= 0 {
		node := e.node(id)
		if node.kind == .block {
			return e.eval_block_value_flow_expected(node, expected_type)
		}
	}
	return e.eval_expr_flow_expected(id, expected_type)
}

fn (mut e Eval) eval_block_value(node &flat.Node) !Value {
	signal := e.eval_block_value_flow(node)!
	if signal.kind == .return_ && signal.values.len > 0 {
		return flow_values_value(signal.values)
	}
	return flow_values_value(signal.values)
}

fn (mut e Eval) eval_block_value_flow(node &flat.Node) !FlowSignal {
	return e.eval_block_value_flow_expected(node, '')
}

fn (mut e Eval) eval_block_value_flow_expected(node &flat.Node, expected_type string) !FlowSignal {
	return e.eval_block_value_flow_collect(node, false, expected_type)
}

fn (mut e Eval) eval_block_value_flow_collect(node &flat.Node, collect_expr_values bool, expected_type string) !FlowSignal {
	e.open_scope()
	mut values := []Value{}
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind == .expr_stmt && child.children_count > 0 {
			expr_id := e.child(child, 0)
			expr := e.node(expr_id)
			if expr.kind == .infix && expr.op == .left_shift {
				signal := e.exec_stmt(child_id)!
				if signal.kind != .normal {
					e.close_scope()!
					return signal
				}
				continue
			}
			signal := e.eval_expr_flow_expected(expr_id, expected_type)!
			if signal.kind != .normal {
				e.close_scope()!
				return signal
			}
			expr_values := flow_values_or_void(signal.values)
			if collect_expr_values {
				for expr_value in expr_values {
					if expr_value !is VoidValue {
						values << expr_value
					}
				}
			} else {
				values = expr_values.clone()
			}
			continue
		}
		if child.kind == .block {
			collect_child_expr_values := node.children_count == 1
				&& e.block_has_only_expr_stmts(child) && child.children_count > 1
			signal := e.eval_block_value_flow_collect(child, collect_child_expr_values,
				expected_type)!
			if signal.kind != .normal {
				e.close_scope()!
				return signal
			}
			values = flow_values_or_void(signal.values)
			continue
		}
		signal := e.exec_stmt(child_id)!
		if signal.kind != .normal {
			e.close_scope()!
			return signal
		}
	}
	e.close_scope()!
	return FlowSignal{
		values: flow_values_or_void(values)
	}
}

fn (e &Eval) block_has_only_expr_stmts(node &flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	for child_id in e.children(node) {
		if e.node(child_id).kind != .expr_stmt {
			return false
		}
	}
	return true
}

fn (mut e Eval) eval_array_init(node &flat.Node) !Value {
	return flow_value(e.eval_array_init_flow(node)!)
}

fn (mut e Eval) eval_array_init_flow(node &flat.Node) !FlowSignal {
	array_type_name := if node.typ.len > 0 { node.typ } else { node.value }
	is_fixed_array := is_fixed_array_type_name(array_type_name)
	elem_type_name := if is_fixed_array {
		fixed_array_elem_type_name(array_type_name)
	} else {
		node.value
	}
	mut len := 0
	mut cap := 0
	mut has_cap := false
	mut init_id := flat.empty_node
	mut has_init := false
	mut values := []Value{}
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind == .field_init {
			if child.value == 'len' {
				signal := e.eval_expr_flow(e.child(child, 0))!
				if signal.kind != .normal {
					return signal
				}
				len = int(e.value_as_int(flow_value(signal))!)
			} else if child.value == 'cap' {
				signal := e.eval_expr_flow(e.child(child, 0))!
				if signal.kind != .normal {
					return signal
				}
				cap = int(e.value_as_int(flow_value(signal))!)
				has_cap = true
			} else if child.value == 'init' {
				init_id = e.child(child, 0)
				has_init = true
			}
		} else {
			signal := e.eval_expr_flow_expected(child_id, elem_type_name)!
			if signal.kind != .normal {
				return signal
			}
			values << e.adapt_value_to_type_name(flow_value(signal), elem_type_name)
		}
	}
	if len > 0 || has_cap {
		array_cap := if has_cap { cap } else { len }
		return e.eval_array_init_values(len, array_cap, has_init, init_id, elem_type_name)
	}
	if is_fixed_array && values.len == 0 {
		fixed_len := e.fixed_array_len(array_type_name)!
		return e.eval_array_init_values(fixed_len, fixed_len, has_init, init_id, elem_type_name)
	}
	return value_flow(ArrayValue{
		elem_type_name: elem_type_name
		values:         values
	})
}

fn (mut e Eval) eval_array_init_values(len int, cap int, has_init bool, init_id flat.NodeId, elem_type_name string) !FlowSignal {
	mut values := []Value{cap: cap}
	for i in 0 .. len {
		if has_init {
			e.open_scope()
			e.declare_var_typed('index', Value(i64(i)), 'int')
			signal := e.eval_expr_flow_expected(init_id, elem_type_name) or {
				e.close_scope()!
				return err
			}
			e.close_scope()!
			if signal.kind != .normal {
				return signal
			}
			values << e.adapt_value_to_type_name(flow_value(signal), elem_type_name)
		} else {
			values << e.zero_value_for_type_name(elem_type_name)
		}
	}
	return value_flow(ArrayValue{
		elem_type_name: elem_type_name
		values:         values
	})
}

fn is_fixed_array_type_name(type_name string) bool {
	if type_name.starts_with('[]') || type_name.starts_with('map[') {
		return false
	}
	return type_name.contains('[') && (type_name.ends_with(']') || type_name.starts_with('['))
}

fn (mut e Eval) fixed_array_len(type_name string) !int {
	return e.fixed_array_len_in_module(type_name, e.current_module_name())
}

fn (mut e Eval) fixed_array_len_in_module(type_name string, module_name string) !int {
	text := fixed_array_len_text(type_name)
	if len := fixed_array_len_literal(text) {
		return len
	}
	if value := e.lookup_const(module_name, text) {
		return int(e.value_as_int(value)!)
	}
	if text.contains('.') {
		mod_name := text.all_before_last('.')
		const_name := text.all_after_last('.')
		if value := e.lookup_const(mod_name, const_name) {
			return int(e.value_as_int(value)!)
		}
	}
	return 0
}

fn fixed_array_len_literal(text string) ?int {
	normalized := text.replace('_', '')
	if normalized.len == 0 {
		return none
	}
	mut len := 0
	for ch in normalized {
		if ch < `0` || ch > `9` {
			return none
		}
		len = len * 10 + int(ch - `0`)
	}
	return len
}

fn fixed_array_len_text(type_name string) string {
	return type_name.all_after('[').all_before(']').trim_space()
}

fn fixed_array_elem_type_name(type_name string) string {
	if type_name.starts_with('[') {
		return type_name.all_after(']')
	}
	return type_name.all_before('[')
}

fn (mut e Eval) eval_map_init(node &flat.Node) !Value {
	return flow_value(e.eval_map_init_flow(node)!)
}

fn (mut e Eval) eval_map_init_flow(node &flat.Node) !FlowSignal {
	mut key_type, mut value_type := split_map_type(node.value)
	children := e.children(node)
	mut keys := []Value{}
	mut values := []Value{}
	mut i := 0
	for i + 1 < children.len {
		key_signal := e.eval_expr_flow_expected(children[i], key_type)!
		if key_signal.kind != .normal {
			return key_signal
		}
		key := flow_value(key_signal)
		if key_type.len == 0 {
			key_type = e.infer_expr_type_name(children[i])
			if key_type.len == 0 {
				key_type = e.runtime_type_name(key)
			}
		}
		typed_key := e.adapt_value_to_type_name(key, key_type)
		value_signal := e.eval_expr_flow_expected(children[i + 1], value_type)!
		if value_signal.kind != .normal {
			return value_signal
		}
		value := flow_value(value_signal)
		if value_type.len == 0 {
			value_type = e.infer_expr_type_name(children[i + 1])
			if value_type.len == 0 {
				value_type = e.runtime_type_name(value)
			}
		}
		keys << typed_key
		values << value
		i += 2
	}
	mut m := MapValue{
		key_type_name:   key_type
		value_type_name: value_type
		default_value:   e.zero_value_for_type_name(value_type)
	}
	for j, key in keys {
		m = e.map_set_value(m, key, values[j])
	}
	return value_flow(m)
}

fn (mut e Eval) eval_string_interp_flow(node &flat.Node) !FlowSignal {
	mut out := ''
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind == .string_literal {
			out += child.value
			continue
		}
		signal := e.eval_expr_flow(child_id)!
		if signal.kind != .normal {
			return signal
		}
		out += e.display_string(flow_value(signal))!
	}
	return value_flow(Value(out))
}

fn split_map_type(type_name string) (string, string) {
	if !type_name.starts_with('map[') {
		return '', ''
	}
	inner := type_name[4..]
	if idx := inner.index(']') {
		return inner[..idx], inner[idx + 1..]
	}
	return '', ''
}

fn (mut e Eval) eval_struct_init(node &flat.Node) !Value {
	return flow_value(e.eval_struct_init_flow(node)!)
}

fn (mut e Eval) eval_struct_init_flow(node &flat.Node) !FlowSignal {
	type_name := e.normalize_type_name(node.value)
	mut st := e.zero_struct_value(type_name)
	mut positional := 0
	for child_id in e.children(node) {
		child := e.node(child_id)
		if child.kind != .field_init {
			continue
		}
		if child.value.len > 0 {
			field_type := e.struct_field_type_name(st, child.value)
			value_signal := e.eval_expr_flow_expected(e.child(child, 0), field_type)!
			if value_signal.kind != .normal {
				return value_signal
			}
			value := flow_value(value_signal)
			st.fields[child.value] = e.adapt_value_to_type_name(value, e.struct_field_type_name(st,
				child.value))
		} else {
			fields := e.struct_fields(type_name)
			if positional < fields.len {
				field := fields[positional]
				field_type := e.qualify_nested_type_name(field.module_name, field.typ)
				value_signal := e.eval_expr_flow_expected(e.child(child, 0), field_type)!
				if value_signal.kind != .normal {
					return value_signal
				}
				value := flow_value(value_signal)
				st.fields[field.name] = e.adapt_value_to_type_name(value, field_type)
			}
			positional++
		}
	}
	return value_flow(st)
}

fn (mut e Eval) eval_assoc(node &flat.Node) !Value {
	return flow_value(e.eval_assoc_flow(node)!)
}

fn (mut e Eval) eval_assoc_flow(node &flat.Node) !FlowSignal {
	if node.children_count == 0 {
		return value_flow(e.zero_struct_value(node.value))
	}
	base_signal := e.eval_expr_flow(e.child(node, 0))!
	if base_signal.kind != .normal {
		return base_signal
	}
	base := flow_value(base_signal)
	if base !is StructValue {
		return error('v3.eval: assoc base must be struct')
	}
	mut st := base as StructValue
	for i in 1 .. node.children_count {
		field := e.child_node(node, i)
		if field.kind == .field_init {
			field_type := e.struct_field_type_name(st, field.value)
			value_signal := e.eval_expr_flow_expected(e.child(field, 0), field_type)!
			if value_signal.kind != .normal {
				return value_signal
			}
			st.fields[field.value] = e.adapt_value_to_type_name(flow_value(value_signal),
				field_type)
		}
	}
	return value_flow(st)
}

fn (mut e Eval) eval_prefix(node &flat.Node) !Value {
	value := e.eval_expr(e.child(node, 0))!
	return e.apply_prefix(node.op, value)
}

fn (e &Eval) apply_prefix(op flat.Op, value Value) !Value {
	match op {
		.minus {
			if value is f64 {
				negated := -value
				return Value(negated)
			}
			return Value(-e.value_as_int(value)!)
		}
		.not {
			return Value(!e.value_as_bool(value)!)
		}
		.bit_not {
			return Value(~e.value_as_int(value)!)
		}
		.amp, .mul {
			return value
		}
		else {
			return value
		}
	}
}

fn (mut e Eval) eval_postfix(node &flat.Node) !Value {
	return flow_value(e.eval_postfix_flow(node)!)
}

fn (mut e Eval) eval_postfix_flow(node &flat.Node) !FlowSignal {
	target_id := e.child(node, 0)
	return e.apply_postfix_target_flow(target_id, node.op)
}

fn (mut e Eval) apply_postfix_target_flow(target_id flat.NodeId, op flat.Op) !FlowSignal {
	target := e.node(target_id)
	match target.kind {
		.index, .selector {
			return e.update_resolved_lvalue_postfix_flow(target_id, op)
		}
		else {
			old_signal := e.eval_expr_flow(target_id)!
			if old_signal.kind != .normal {
				return old_signal
			}
			old := flow_value(old_signal)
			value := e.apply_postfix_op(op, old)!
			update_signal := e.update_target_flow(target_id, value)!
			if update_signal.kind != .normal {
				return update_signal
			}
			return value_flow(old)
		}
	}
}

fn (mut e Eval) apply_postfix_op(op flat.Op, old Value) !Value {
	if op == .inc {
		return e.apply_infix(.plus, old, Value(i64(1)))
	}
	if op == .dec {
		return e.apply_infix(.minus, old, Value(i64(1)))
	}
	return old
}

fn (mut e Eval) eval_or_expr(node &flat.Node) !Value {
	signal := e.eval_or_expr_flow(node)!
	if signal.kind == .return_ && signal.values.len > 0 {
		return signal.values[0]
	}
	return flow_value(signal)
}

fn (mut e Eval) eval_or_expr_flow(node &flat.Node) !FlowSignal {
	left_id := e.child(node, 0)
	left_node := e.node(left_id)
	if left_node.kind == .index {
		container_signal := e.eval_expr_flow(e.child(left_node, 0))!
		if container_signal.kind != .normal {
			return container_signal
		}
		container := flow_value(container_signal)
		if container is MapValue {
			index_signal := e.eval_expr_flow_expected(e.child(left_node, 1),
				container.key_type_name)!
			if index_signal.kind != .normal {
				return index_signal
			}
			value, ok := e.map_lookup(container, flow_value(index_signal))
			if ok {
				return value_flow(e.unwrap_option_like(value))
			}
			return e.eval_or_failure(node, container.default_value)
		}
	}
	left_signal := e.eval_expr_flow(left_id)!
	if left_signal.kind != .normal {
		return left_signal
	}
	left := flow_value(left_signal)
	if e.value_is_truthy(left) {
		return FlowSignal{
			values: [e.unwrap_option_like(left)]
		}
	}
	return e.eval_or_failure(node, left)
}

fn (mut e Eval) eval_or_failure(node &flat.Node, value Value) !FlowSignal {
	if node.children_count > 1 && e.node(e.child(node, 1)).kind != .empty {
		or_id := e.child(node, 1)
		or_node := e.node(or_id)
		if or_node.kind == .block {
			e.open_scope()
			e.declare_var('err', e.or_block_err_value(value))
			signal := e.eval_block_value_flow(or_node) or {
				e.close_scope() or {}
				return err
			}
			e.close_scope()!
			return signal
		}
		return FlowSignal{
			values: [e.eval_expr(or_id)!]
		}
	}
	if node.value == '?' || node.value == '!' {
		return FlowSignal{
			kind:   .return_
			values: [value]
		}
	}
	return FlowSignal{
		values: [void_value()]
	}
}

fn (e &Eval) or_block_err_value(value Value) Value {
	if value is VoidValue {
		return Value('')
	}
	return value
}

fn (mut e Eval) exec_match(node &flat.Node) !FlowSignal {
	target_id := e.child(node, 0)
	target_signal := e.eval_expr_flow(target_id)!
	if target_signal.kind != .normal {
		return target_signal
	}
	target := flow_value(target_signal)
	for i in 1 .. node.children_count {
		branch := e.child_node(node, i)
		if branch.kind != .match_branch {
			continue
		}
		cond_count := if branch.value == 'else' { 0 } else { branch.value.int() }
		mut matched := branch.value == 'else'
		mut matched_cond := Value(void_value())
		for j in 0 .. cond_count {
			cond_signal := e.eval_match_condition_flow(e.child(branch, j), target_id)!
			if cond_signal.kind != .normal {
				return cond_signal
			}
			cond := flow_value(cond_signal)
			if e.match_condition_matches(target, cond) {
				matched = true
				matched_cond = cond
				break
			}
		}
		if matched {
			e.open_scope()
			if binding := e.smartcast_binding_from_match(target_id, target, matched_cond) {
				e.declare_var_typed(binding.name, binding.value, binding.type_name)
			}
			signal := e.exec_stmts(e.children(branch)[cond_count..])!
			e.close_scope()!
			if signal.kind != .normal {
				return signal
			}
			return normal_flow()
		}
	}
	return normal_flow()
}

fn (mut e Eval) eval_match_value(node &flat.Node) !Value {
	signal := e.eval_match_value_flow(node)!
	if signal.kind == .return_ && signal.values.len > 0 {
		return flow_values_value(signal.values)
	}
	return flow_values_value(signal.values)
}

fn (mut e Eval) eval_match_value_flow(node &flat.Node) !FlowSignal {
	return e.eval_match_value_flow_expected(node, '')
}

fn (mut e Eval) eval_match_value_flow_expected(node &flat.Node, expected_type string) !FlowSignal {
	target_id := e.child(node, 0)
	target_signal := e.eval_expr_flow(target_id)!
	if target_signal.kind != .normal {
		return target_signal
	}
	target := flow_value(target_signal)
	for i in 1 .. node.children_count {
		branch := e.child_node(node, i)
		if branch.kind != .match_branch {
			continue
		}
		cond_count := if branch.value == 'else' { 0 } else { branch.value.int() }
		mut matched := branch.value == 'else'
		mut matched_cond := Value(void_value())
		for j in 0 .. cond_count {
			cond_signal := e.eval_match_condition_flow(e.child(branch, j), target_id)!
			if cond_signal.kind != .normal {
				return cond_signal
			}
			cond := flow_value(cond_signal)
			if e.match_condition_matches(target, cond) {
				matched = true
				matched_cond = cond
				break
			}
		}
		if matched {
			e.open_scope()
			if binding := e.smartcast_binding_from_match(target_id, target, matched_cond) {
				e.declare_var_typed(binding.name, binding.value, binding.type_name)
			}
			mut values := []Value{}
			for j in cond_count .. branch.children_count {
				stmt_id := e.child(branch, j)
				stmt := e.node(stmt_id)
				if stmt.kind == .expr_stmt && stmt.children_count > 0 {
					expr_id := e.child(stmt, 0)
					expr := e.node(expr_id)
					if expr.kind == .infix && expr.op == .left_shift {
						signal := e.exec_stmt(stmt_id)!
						if signal.kind != .normal {
							e.close_scope()!
							return signal
						}
						continue
					}
					signal := e.eval_expr_flow_expected(expr_id, expected_type)!
					if signal.kind != .normal {
						e.close_scope()!
						return signal
					}
					values = flow_values_or_void(signal.values)
				} else {
					signal := e.exec_stmt(stmt_id)!
					if signal.kind != .normal {
						e.close_scope()!
						return signal
					}
				}
			}
			e.close_scope()!
			return FlowSignal{
				values: flow_values_or_void(values)
			}
		}
	}
	return value_flow(void_value())
}

fn (e &Eval) match_condition_matches(target Value, cond Value) bool {
	if cond is RangeValue {
		return e.value_in(target, cond)
	}
	if cond is TypeValue {
		return e.value_matches_type_name(target, cond.name)
	}
	return e.value_eq(target, cond)
}

fn (e &Eval) smartcast_binding_from_match(target_id flat.NodeId, target Value, cond Value) ?SmartcastBinding {
	if cond !is TypeValue {
		return none
	}
	type_cond := cond as TypeValue
	target_node := e.node(target_id)
	if target_node.kind != .ident {
		return none
	}
	if value := e.smartcast_value(target, type_cond.name) {
		return SmartcastBinding{
			name:      target_node.value
			value:     value
			type_name: e.normalize_type_name(type_cond.name)
		}
	}
	return none
}

fn (mut e Eval) eval_match_condition(cond_id flat.NodeId, target_id flat.NodeId) !Value {
	return flow_value(e.eval_match_condition_flow(cond_id, target_id)!)
}

fn (mut e Eval) eval_match_condition_flow(cond_id flat.NodeId, target_id flat.NodeId) !FlowSignal {
	cond := e.node(cond_id)
	if cond.kind == .enum_val {
		if enum_type := e.match_target_enum_type_name(target_id) {
			if value := e.lookup_enum_value(enum_type, cond.value) {
				return value_flow(value)
			}
		}
	}
	if cond.kind == .ident && is_builtin_type_name(cond.value) {
		return value_flow(TypeValue{
			name: cond.value
		})
	}
	signal := e.eval_expr_flow(cond_id)!
	if signal.kind != .normal {
		return signal
	}
	value := flow_value(signal)
	if value is RangeValue {
		return value_flow(RangeValue{
			start:     value.start
			end:       value.end
			inclusive: true
		})
	}
	return value_flow(value)
}

fn (e &Eval) match_target_enum_type_name(target_id flat.NodeId) ?string {
	target := e.node(target_id)
	match target.kind {
		.ident {
			if typ := e.lookup_var_type(target.value) {
				return typ
			}
			if target.typ.len > 0 {
				return e.normalize_type_name(target.typ)
			}
		}
		.selector {
			if target.children_count > 0 {
				return e.type_value_name_from_expr(e.child(target, 0))
			}
		}
		else {}
	}

	return none
}

fn (e &Eval) type_value_name_from_expr(id flat.NodeId) ?string {
	node := e.node(id)
	match node.kind {
		.ident {
			if e.has_type_name(e.current_module_name(), node.value) {
				return e.qualify_type_name(e.current_module_name(), node.value)
			}
		}
		.selector {
			if node.children_count > 0 {
				left := e.node(e.child(node, 0))
				if left.kind == .ident {
					mod := e.resolve_module_name(left.value)
					if e.has_type_name(mod, node.value) {
						return e.qualify_type_name(mod, node.value)
					}
				}
			}
		}
		else {}
	}

	return none
}

fn (mut e Eval) lookup_enum_value(enum_type_name string, field string) ?Value {
	mod := if enum_type_name.contains('.') {
		enum_type_name.all_before_last('.')
	} else {
		e.current_module_name()
	}
	enum_key := '${enum_type_name.all_after_last('.')}.${field}'
	if value := e.lookup_const(mod, enum_key) {
		return e.enum_value(e.qualify_type_name(mod, enum_type_name), value)
	}
	return none
}

fn (e &Eval) enum_value(type_name string, value Value) Value {
	return EnumValue{
		type_name: e.normalize_type_name(type_name)
		value:     e.value_as_int(value) or { i64(0) }
	}
}

fn (mut e Eval) maybe_call_builtin(module_name string, fn_name string, args []Value) !MaybeValue {
	if module_name in ['', 'main', e.current_module_name()] {
		match fn_name {
			'print' {
				if args.len > 0 {
					e.write_stdout(e.display_string(args[0])!)
				}
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'println' {
				if args.len > 0 {
					e.write_stdout(e.display_string(args[0])! + '\n')
				} else {
					e.write_stdout('\n')
				}
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'eprint' {
				if args.len > 0 {
					e.write_stderr(e.display_string(args[0])!)
				}
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'eprintln' {
				if args.len > 0 {
					e.write_stderr(e.display_string(args[0])! + '\n')
				} else {
					e.write_stderr('\n')
				}
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'int_str', 'i64_str', 'u64_str' {
				return MaybeValue{
					found: true
					value: Value(e.value_as_int(args[0] or { Value(i64(0)) })!.str())
				}
			}
			'str' {
				return MaybeValue{
					found: true
					value: Value(e.display_string(args[0] or { void_value() })!)
				}
			}
			'panic' {
				return error(e.display_string(args[0] or { Value('panic') })!)
			}
			else {}
		}
	}
	if module_name == 'os' {
		return e.maybe_call_os_builtin(fn_name, args)
	}
	return MaybeValue{}
}

fn (mut e Eval) maybe_call_os_builtin(fn_name string, args []Value) !MaybeValue {
	match fn_name {
		'execute' {
			cmd := e.expect_string_arg(args, 0)!
			return MaybeValue{
				found: true
				value: os_result_value(os.execute(cmd))
			}
		}
		'user_os' {
			return MaybeValue{
				found: true
				value: Value(os.user_os())
			}
		}
		'getenv' {
			return MaybeValue{
				found: true
				value: Value(os.getenv(e.expect_string_arg(args, 0)!))
			}
		}
		'temp_dir' {
			return MaybeValue{
				found: true
				value: Value(os.temp_dir())
			}
		}
		'getpid' {
			return MaybeValue{
				found: true
				value: Value(i64(os.getpid()))
			}
		}
		'join_path' {
			parts := args.map(e.value_string(it))
			if parts.len == 0 {
				return MaybeValue{
					found: true
					value: Value('')
				}
			}
			return MaybeValue{
				found: true
				value: Value(os.join_path(parts[0], ...parts[1..]))
			}
		}
		'join_path_single' {
			a := e.expect_string_arg(args, 0)!
			b := e.expect_string_arg(args, 1)!
			return MaybeValue{
				found: true
				value: Value(os.join_path_single(a, b))
			}
		}
		'dir' {
			return MaybeValue{
				found: true
				value: Value(os.dir(e.expect_string_arg(args, 0)!))
			}
		}
		'is_dir' {
			return MaybeValue{
				found: true
				value: Value(os.is_dir(e.expect_string_arg(args, 0)!))
			}
		}
		'exists' {
			return MaybeValue{
				found: true
				value: Value(os.exists(e.expect_string_arg(args, 0)!))
			}
		}
		'quoted_path' {
			return MaybeValue{
				found: true
				value: Value(os.quoted_path(e.expect_string_arg(args, 0)!))
			}
		}
		else {
			return MaybeValue{}
		}
	}
}

fn os_result_value(result os.Result) Value {
	return StructValue{
		type_name: 'os.Result'
		fields:    {
			'exit_code': Value(i64(result.exit_code))
			'output':    Value(result.output)
		}
	}
}

fn (mut e Eval) call_value_method(receiver Value, receiver_type_name string, method_name string, args []Value) !MethodCallResult {
	static_type_name := e.normalize_type_name(receiver_type_name)
	if value := e.direct_builtin_str_method_value(receiver, static_type_name, method_name, args) {
		return MethodCallResult{
			value:    value
			receiver: receiver
		}
	}
	if static_type_name.len > 0 {
		if target := e.resolve_method_target(static_type_name, method_name) {
			return e.call_method_target(receiver, target, args)!
		}
	}
	if value := e.direct_str_method_value(receiver, method_name, args) {
		return MethodCallResult{
			value:    value
			receiver: receiver
		}
	}
	if receiver is string {
		value := e.call_string_method(receiver, method_name, args)!
		return MethodCallResult{
			value:    value
			receiver: receiver
		}
	}
	if receiver is ArrayValue {
		value := e.call_array_method(receiver, method_name, args)!
		return MethodCallResult{
			value:    value
			receiver: receiver
		}
	}
	if receiver is MapValue {
		value := e.call_map_method(receiver, method_name, args)!
		return MethodCallResult{
			value:            value
			receiver_changed: method_name in ['clear', 'delete']
			receiver:         value
		}
	}
	if receiver is StructValue {
		if target := e.resolve_method_target(receiver.type_name, method_name) {
			return e.call_method_target(receiver, target, args)!
		}
	}
	if receiver is SumValue {
		result := e.call_value_method(receiver.payload, receiver.variant_name, method_name, args)!
		if result.receiver_changed {
			return MethodCallResult{
				value:            result.value
				receiver_changed: true
				receiver:         SumValue{
					type_name:    receiver.type_name
					variant_name: receiver.variant_name
					payload:      result.receiver
				}
				mutated_args:     result.mutated_args
			}
		}
		return result
	}
	return error('v3.eval: unsupported method `${method_name}` on `${e.runtime_type_name(receiver)}`')
}

fn (mut e Eval) direct_builtin_str_method_value(receiver Value, static_type_name string, method_name string, args []Value) ?Value {
	if !is_builtin_str_receiver_type_name(static_type_name) {
		return none
	}
	return e.direct_str_method_value(receiver, method_name, args)
}

fn (mut e Eval) direct_str_method_value(receiver Value, method_name string, args []Value) ?Value {
	if method_name != 'str' || args.len != 0 {
		return none
	}
	match receiver {
		bool, i64, f64, string, EnumValue {
			return Value(e.value_string(receiver))
		}
		StructValue {
			if _ := e.resolve_method_target(receiver.type_name, 'str') {
				return none
			}
			return Value(e.default_struct_string(receiver))
		}
		else {}
	}

	return none
}

fn is_builtin_str_receiver_type_name(name string) bool {
	return name in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'byte', 'u16', 'u32',
		'u64', 'usize', 'f32', 'f64', 'rune', 'char', 'string']
}

fn (mut e Eval) call_method_target(receiver Value, target FunctionDef, args []Value) !MethodCallResult {
	mut call_args := []Value{}
	call_args << receiver
	call_args << args
	result := e.call_function(target.module_name, target.name, call_args)!
	mut value := Value(void_value())
	if result.values.len == 1 {
		value = result.values[0]
	} else {
		value = TupleValue{
			values: result.values
		}
	}
	mut updated_receiver := receiver
	mut receiver_changed := false
	if 0 in result.mutated_args {
		receiver_changed = true
		updated_receiver = result.mutated_args[0] or { receiver }
	}
	mut mutated_args := map[int]Value{}
	for arg_index, mutated_value in result.mutated_args {
		if arg_index > 0 {
			mutated_args[arg_index - 1] = mutated_value
		}
	}
	return MethodCallResult{
		value:            value
		receiver_changed: receiver_changed
		receiver:         updated_receiver
		mutated_args:     mutated_args
	}
}

fn (e &Eval) resolve_method_target(type_name string, method_name string) ?FunctionDef {
	short_type := type_name.all_after_last('.')
	module_name := if type_name.contains('.') {
		type_name.all_before_last('.')
	} else {
		e.current_module_name()
	}
	names := ['${type_name}.${method_name}', '${short_type}.${method_name}']
	if module_name in e.functions {
		for name in names {
			if name in e.functions[module_name] {
				return e.functions[module_name][name]
			}
		}
	}
	for mod in e.functions.keys() {
		for name in names {
			if name in e.functions[mod] {
				return e.functions[mod][name]
			}
		}
	}
	return none
}

fn (e &Eval) infix_operator_call_info(left Value, op flat.Op) ?InfixOperatorCallInfo {
	type_name := e.infix_operator_receiver_type_name(left)
	if type_name.len == 0 {
		return none
	}
	if op_name := infix_operator_symbol(op) {
		if target := e.resolve_method_target(type_name, op_name) {
			return InfixOperatorCallInfo{
				target: target
			}
		}
	}
	match op {
		.gt {
			if target := e.resolve_method_target(type_name, '<') {
				return InfixOperatorCallInfo{
					target:  target
					reverse: true
				}
			}
		}
		.ge {
			if target := e.resolve_method_target(type_name, '<') {
				return InfixOperatorCallInfo{
					target: target
					negate: true
				}
			}
		}
		.le {
			if target := e.resolve_method_target(type_name, '<') {
				return InfixOperatorCallInfo{
					target:  target
					reverse: true
					negate:  true
				}
			}
		}
		.ne {
			if target := e.resolve_method_target(type_name, '==') {
				return InfixOperatorCallInfo{
					target: target
					negate: true
				}
			}
		}
		else {}
	}

	return none
}

fn (e &Eval) infix_operator_receiver_type_name(value Value) string {
	return match value {
		StructValue { value.type_name }
		else { '' }
	}
}

fn infix_operator_symbol(op flat.Op) ?string {
	return match op {
		.plus { '+' }
		.minus { '-' }
		.mul { '*' }
		.div { '/' }
		.mod { '%' }
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		else { none }
	}
}

fn (mut e Eval) apply_infix_operator_overload(op flat.Op, left Value, right Value) !MaybeValue {
	if call_info := e.infix_operator_call_info(left, op) {
		mut receiver := left
		mut arg := right
		if call_info.reverse {
			receiver = right
			arg = left
		}
		result := e.call_method_target(receiver, call_info.target, [arg])!
		mut value := result.value
		if call_info.negate {
			value = Value(!e.value_as_bool(value)!)
		}
		return MaybeValue{
			found: true
			value: value
		}
	}
	return MaybeValue{}
}

fn (mut e Eval) call_string_method(receiver string, method_name string, args []Value) !Value {
	match method_name {
		'int' {
			return Value(strconv.parse_int(receiver, 10, 64) or { i64(0) })
		}
		'i64' {
			return Value(strconv.parse_int(receiver, 10, 64) or { i64(0) })
		}
		'str' {
			return Value(receiver)
		}
		'contains' {
			return Value(receiver.contains(e.expect_string_arg(args, 0)!))
		}
		'starts_with' {
			return Value(receiver.starts_with(e.expect_string_arg(args, 0)!))
		}
		'ends_with' {
			return Value(receiver.ends_with(e.expect_string_arg(args, 0)!))
		}
		'all_after_last' {
			return Value(receiver.all_after_last(e.expect_string_arg(args, 0)!))
		}
		'all_before_last' {
			return Value(receiver.all_before_last(e.expect_string_arg(args, 0)!))
		}
		'trim_space' {
			return Value(receiver.trim_space())
		}
		else {
			return error('v3.eval: unsupported string method `${method_name}`')
		}
	}
}

fn (mut e Eval) call_array_method(receiver ArrayValue, method_name string, args []Value) !Value {
	match method_name {
		'clone', 'move' {
			return ArrayValue{
				elem_type_name: receiver.elem_type_name
				values:         receiver.values.clone()
			}
		}
		'first' {
			if receiver.values.len == 0 {
				return error('v3.eval: array.first on empty array')
			}
			return receiver.values[0]
		}
		'last' {
			if receiver.values.len == 0 {
				return error('v3.eval: array.last on empty array')
			}
			return receiver.values[receiver.values.len - 1]
		}
		'contains' {
			return Value(receiver.values.any(e.value_eq(it, args[0] or { void_value() })))
		}
		'index' {
			needle := args[0] or { void_value() }
			for i, item in receiver.values {
				if e.value_eq(item, needle) {
					return Value(i64(i))
				}
			}
			return Value(i64(-1))
		}
		'join' {
			sep := e.expect_string_arg(args, 0) or { '' }
			return Value(receiver.values.map(e.value_string(it)).join(sep))
		}
		else {
			return error('v3.eval: unsupported array method `${method_name}`')
		}
	}
}

fn (mut e Eval) call_map_method(receiver MapValue, method_name string, args []Value) !Value {
	match method_name {
		'keys' {
			return e.map_keys(receiver)
		}
		'values' {
			return e.map_values(receiver)
		}
		'clone', 'move' {
			return e.map_clone(receiver)
		}
		'clear' {
			return MapValue{
				key_type_name:   receiver.key_type_name
				value_type_name: receiver.value_type_name
				default_value:   receiver.default_value
			}
		}
		'delete' {
			if args.len == 0 {
				return receiver
			}
			return e.map_delete_value(receiver, args[0])
		}
		else {
			return error('v3.eval: unsupported map method `${method_name}`')
		}
	}
}

fn (mut e Eval) apply_infix(op flat.Op, left Value, right Value) !Value {
	overloaded := e.apply_infix_operator_overload(op, left, right)!
	if overloaded.found {
		return overloaded.value
	}
	match op {
		.plus {
			if left is string || right is string {
				return Value(e.value_string(left) + e.value_string(right))
			}
			if left is f64 || right is f64 {
				return Value(e.value_as_f64(left)! + e.value_as_f64(right)!)
			}
			return Value(e.value_as_int(left)! + e.value_as_int(right)!)
		}
		.minus {
			if left is f64 || right is f64 {
				return Value(e.value_as_f64(left)! - e.value_as_f64(right)!)
			}
			return Value(e.value_as_int(left)! - e.value_as_int(right)!)
		}
		.mul {
			if left is f64 || right is f64 {
				return Value(e.value_as_f64(left)! * e.value_as_f64(right)!)
			}
			return Value(e.value_as_int(left)! * e.value_as_int(right)!)
		}
		.div {
			if left is f64 || right is f64 {
				return Value(e.value_as_f64(left)! / e.value_as_f64(right)!)
			}
			return Value(e.value_as_int(left)! / e.value_as_int(right)!)
		}
		.mod {
			return Value(e.value_as_int(left)! % e.value_as_int(right)!)
		}
		.eq {
			return Value(e.value_eq(left, right))
		}
		.ne {
			return Value(!e.value_eq(left, right))
		}
		.lt {
			return Value(e.compare_values(left, right) < 0)
		}
		.gt {
			return Value(e.compare_values(left, right) > 0)
		}
		.le {
			return Value(e.compare_values(left, right) <= 0)
		}
		.ge {
			return Value(e.compare_values(left, right) >= 0)
		}
		.amp {
			return Value(e.value_as_int(left)! & e.value_as_int(right)!)
		}
		.pipe {
			return Value(e.value_as_int(left)! | e.value_as_int(right)!)
		}
		.xor {
			return Value(e.value_as_int(left)! ^ e.value_as_int(right)!)
		}
		.left_shift {
			mut result := e.value_as_int(left)!
			for _ in 0 .. int(e.value_as_int(right)!) {
				result *= 2
			}
			return Value(result)
		}
		.right_shift {
			return Value(e.value_as_int(left)! >> e.value_as_int(right)!)
		}
		.right_shift_unsigned {
			return Value(i64(u64(e.value_as_int(left)!) >>> e.value_as_int(right)!))
		}
		else {
			return error('v3.eval: unsupported infix operator `${op}`')
		}
	}
}

fn (e &Eval) value_in(left Value, right Value) bool {
	match right {
		ArrayValue {
			return right.values.any(e.value_eq(it, left))
		}
		MapValue {
			return e.map_contains_key(right, left)
		}
		string {
			return right.contains(e.value_string(left))
		}
		RangeValue {
			v := e.value_as_int(left) or { return false }
			if right.inclusive {
				return v >= right.start && v <= right.end
			}
			return v >= right.start && v < right.end
		}
		else {
			return false
		}
	}
}

fn (e &Eval) compare_values(left Value, right Value) int {
	if left is string && right is string {
		return left.compare(right)
	}
	lf := e.value_as_f64(left) or { 0.0 }
	rf := e.value_as_f64(right) or { 0.0 }
	if lf < rf {
		return -1
	}
	if lf > rf {
		return 1
	}
	return 0
}

fn (e &Eval) value_eq(left Value, right Value) bool {
	match left {
		bool {
			return right is bool && left == right
		}
		i64 {
			if right is f64 {
				return f64(left) == right
			}
			return e.value_as_int(right) or { return false } == left
		}
		EnumValue {
			if right is EnumValue {
				return e.type_name_matches(left.type_name, right.type_name)
					&& left.value == right.value
			}
			return e.value_as_int(right) or { return false } == left.value
		}
		f64 {
			return e.value_as_f64(right) or { return false } == left
		}
		string {
			return right is string && left == right
		}
		VoidValue {
			return right is VoidValue
		}
		TypeValue {
			return right is TypeValue && left.name == right.name
		}
		ModuleValue {
			return right is ModuleValue && left.name == right.name
		}
		ArrayValue {
			if right !is ArrayValue {
				return false
			}
			right_arr := right as ArrayValue
			if left.values.len != right_arr.values.len {
				return false
			}
			for i, item in left.values {
				if !e.value_eq(item, right_arr.values[i]) {
					return false
				}
			}
			return true
		}
		StructValue {
			if right !is StructValue {
				return false
			}
			right_struct := right as StructValue
			if left.type_name != right_struct.type_name
				|| left.fields.len != right_struct.fields.len {
				return false
			}
			for name, value in left.fields {
				right_value := right_struct.fields[name] or { return false }
				if !e.value_eq(value, right_value) {
					return false
				}
			}
			return true
		}
		SumValue {
			return right is SumValue && left.type_name == right.type_name
				&& left.variant_name == right.variant_name
				&& e.value_eq(left.payload, right.payload)
		}
		else {
			return false
		}
	}
}

fn (e &Eval) map_lookup(receiver MapValue, key Value) (Value, bool) {
	typed_key := e.adapt_value_to_type_name(key, receiver.key_type_name)
	for entry in receiver.entries {
		if e.value_eq(entry.key, typed_key) {
			return entry.value, true
		}
	}
	return receiver.default_value, false
}

fn (e &Eval) map_contains_key(receiver MapValue, key Value) bool {
	_, ok := e.map_lookup(receiver, key)
	return ok
}

fn (e &Eval) map_set_value(receiver MapValue, key Value, value Value) MapValue {
	entry_key := e.adapt_value_to_type_name(key, receiver.key_type_name)
	entry_value := e.adapt_value_to_type_name(value, receiver.value_type_name)
	mut m := MapValue{
		key_type_name:   receiver.key_type_name
		value_type_name: receiver.value_type_name
		default_value:   receiver.default_value
		entries:         receiver.entries.clone()
	}
	for i, entry in m.entries {
		if e.value_eq(entry.key, entry_key) {
			m.entries[i].value = entry_value
			return m
		}
	}
	m.entries << MapEntry{
		key:   entry_key
		value: entry_value
	}
	return m
}

fn (e &Eval) map_delete_value(receiver MapValue, key Value) MapValue {
	mut m := receiver
	typed_key := e.adapt_value_to_type_name(key, receiver.key_type_name)
	for i, entry in m.entries {
		if e.value_eq(entry.key, typed_key) {
			m.entries.delete(i)
			break
		}
	}
	return m
}

fn (e &Eval) map_keys(receiver MapValue) ArrayValue {
	return ArrayValue{
		elem_type_name: receiver.key_type_name
		values:         receiver.entries.map(it.key)
	}
}

fn (e &Eval) map_values(receiver MapValue) ArrayValue {
	return ArrayValue{
		elem_type_name: receiver.value_type_name
		values:         receiver.entries.map(it.value)
	}
}

fn (e &Eval) map_clone(receiver MapValue) MapValue {
	return MapValue{
		key_type_name:   receiver.key_type_name
		value_type_name: receiver.value_type_name
		default_value:   receiver.default_value
		entries:         receiver.entries.clone()
	}
}

fn (mut e Eval) zero_value_like(value Value) Value {
	match value {
		bool {
			return Value(false)
		}
		i64 {
			return Value(i64(0))
		}
		f64 {
			return Value(0.0)
		}
		EnumValue {
			return EnumValue{
				type_name: value.type_name
			}
		}
		string {
			return Value('')
		}
		ArrayValue {
			return ArrayValue{
				elem_type_name: value.elem_type_name
			}
		}
		MapValue {
			return MapValue{
				key_type_name:   value.key_type_name
				value_type_name: value.value_type_name
				default_value:   value.default_value
			}
		}
		StructValue {
			return e.zero_struct_value(value.type_name)
		}
		SumValue {
			return SumValue{
				type_name:    value.type_name
				variant_name: value.variant_name
				payload:      e.zero_value_like(value.payload)
			}
		}
		else {
			return void_value()
		}
	}
}

fn (mut e Eval) zero_value_for_type_name(type_name string) Value {
	return e.zero_value_for_type_name_in_module(type_name, e.current_module_name())
}

fn (mut e Eval) zero_value_for_type_name_in_module(type_name string, module_name string) Value {
	name := type_name.trim_left('&')
	if name == '' || name == 'void' {
		return void_value()
	}
	if alias := e.type_alias_info_in_module(name, module_name) {
		return e.zero_value_for_type_name_in_module(alias.target, alias.module_name)
	}
	if name == 'bool' {
		return Value(false)
	}
	if name in ['f32', 'f64'] {
		return Value(0.0)
	}
	if name == 'string' {
		return Value('')
	}
	if name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'isize', 'usize',
		'rune', 'char'] {
		return Value(i64(0))
	}
	if name.starts_with('[]') {
		elem_type := e.qualify_nested_type_name(module_name, name[2..])
		return ArrayValue{
			elem_type_name: elem_type
		}
	}
	if is_fixed_array_type_name(name) {
		elem_type := e.qualify_nested_type_name(module_name, fixed_array_elem_type_name(name))
		len := e.fixed_array_len_in_module(name, module_name) or { 0 }
		return ArrayValue{
			elem_type_name: elem_type
			values:         []Value{len: len, cap: len, init: e.zero_value_for_type_name_in_module(elem_type,
				module_name)}
		}
	}
	if name.starts_with('map[') {
		key_type, value_type := split_map_type(name)
		qualified_key_type := e.qualify_nested_type_name(module_name, key_type)
		qualified_value_type := e.qualify_nested_type_name(module_name, value_type)
		return MapValue{
			key_type_name:   qualified_key_type
			value_type_name: qualified_value_type
			default_value:   e.zero_value_for_type_name_in_module(qualified_value_type, module_name)
		}
	}
	enum_name := e.qualify_type_name(module_name, name)
	if fields := e.enum_fields[enum_name] {
		if fields.len > 0 {
			if value := e.lookup_enum_value(enum_name, fields[0]) {
				return value
			}
		}
		return EnumValue{
			type_name: enum_name
		}
	}
	sum_name := e.qualify_type_name(module_name, name)
	if sum_name in e.sum_types {
		return SumValue{
			type_name: sum_name
			payload:   void_value()
		}
	}
	if name in e.sum_types {
		return SumValue{
			type_name: name
			payload:   void_value()
		}
	}
	struct_name := e.resolve_struct_type_name_in_module(name, module_name)
	if struct_name in e.structs {
		return e.zero_struct_value(struct_name)
	}
	return void_value()
}

fn (e &Eval) qualify_nested_type_name(module_name string, type_name string) string {
	name := type_name.trim_space()
	if name == '' {
		return ''
	}
	if name.starts_with('[]') {
		return '[]${e.qualify_nested_type_name(module_name, name[2..])}'
	}
	if name.starts_with('map[') {
		key_type, value_type := split_map_type(name)
		return 'map[${e.qualify_nested_type_name(module_name, key_type)}]${e.qualify_nested_type_name(module_name,
			value_type)}'
	}
	if name.starts_with('?') || name.starts_with('!') {
		return '${name[..1]}${e.qualify_nested_type_name(module_name, name[1..])}'
	}
	return e.qualify_type_name(module_name, name)
}

fn (mut e Eval) zero_struct_value(type_name string) StructValue {
	name := e.resolve_struct_type_name(type_name)
	mut fields := map[string]Value{}
	info := e.struct_info(name)
	for field in info.fields {
		fields[field.name] = if int(field.default_node) >= 0 {
			e.eval_struct_field_default(field)
		} else {
			e.zero_value_for_type_name_in_module(field.typ, info.module_name)
		}
	}
	return StructValue{
		type_name: type_name
		fields:    fields
	}
}

fn (mut e Eval) eval_struct_field_default(field FieldInfo) Value {
	e.call_stack << CallFrame{
		module_name: field.module_name
		file_name:   field.file_name
		fn_name:     '<field ${field.name}>'
	}
	value := e.eval_expr_expected(field.default_node, field.typ) or {
		e.call_stack.delete(e.call_stack.len - 1)
		return e.zero_value_for_type_name_in_module(field.typ, field.module_name)
	}
	adapted := e.adapt_value_to_type_name(value, field.typ)
	e.call_stack.delete(e.call_stack.len - 1)
	return adapted
}

fn (e &Eval) struct_fields(type_name string) []FieldInfo {
	return e.struct_info(type_name).fields
}

fn (e &Eval) struct_field_type_name(value StructValue, field_name string) string {
	return e.struct_field_type_name_by_type(value.type_name, field_name)
}

fn (e &Eval) struct_field_type_name_by_type(type_name string, field_name string) string {
	info := e.struct_info(type_name)
	for field in info.fields {
		if field.name == field_name {
			return e.qualify_nested_type_name(field.module_name, field.typ)
		}
	}
	return ''
}

fn (e &Eval) struct_info(type_name string) StructInfo {
	name := e.resolve_struct_type_name(type_name)
	if name in e.structs {
		return e.structs[name]
	}
	return StructInfo{}
}

fn (e &Eval) resolve_struct_type_name(type_name string) string {
	return e.resolve_struct_type_name_in_module(type_name, e.current_module_name())
}

fn (e &Eval) resolve_struct_type_name_in_module(type_name string, module_name string) string {
	if type_name.contains('.') {
		if type_name in e.structs {
			return type_name
		}
		return type_name
	}
	qualified := e.qualify_type_name(module_name, type_name)
	if qualified in e.structs {
		return qualified
	}
	if type_name in e.structs {
		return type_name
	}
	short := type_name.all_after_last('.')
	if short in e.structs {
		return short
	}
	return type_name
}

fn (e &Eval) adapt_value_to_type_name(value Value, type_name string) Value {
	if alias := e.type_alias_info_in_module(type_name, e.current_module_name()) {
		return e.adapt_value_to_type_name(value, e.qualify_type_name(alias.module_name,
			alias.target))
	}
	if type_name.starts_with('?') {
		if value is VoidValue || e.is_option_like_value(value) {
			return value
		}
		inner_type := type_name[1..]
		data := if inner_type.len > 0 {
			e.adapt_value_to_type_name(value, inner_type)
		} else {
			value
		}
		return StructValue{
			type_name: 'Option'
			fields:    {
				'state': Value(i64(0))
				'err':   Value('')
				'data':  data
			}
		}
	}
	if type_name.starts_with('!') {
		if e.is_option_like_value(value) {
			return value
		}
		inner_type := type_name[1..]
		data := if inner_type.len > 0 {
			e.adapt_value_to_type_name(value, inner_type)
		} else {
			value
		}
		return StructValue{
			type_name: 'Result'
			fields:    {
				'is_error': Value(false)
				'err':      Value('')
				'data':     data
			}
		}
	}
	if type_name.starts_with('[]') && value is ArrayValue {
		elem_type_name := e.qualify_nested_type_name(e.current_module_name(), type_name[2..])
		mut arr := value
		arr.elem_type_name = elem_type_name
		mut values := []Value{cap: arr.values.cap}
		for item in arr.values {
			values << e.adapt_value_to_type_name(item, elem_type_name)
		}
		arr.values = values
		return arr
	}
	if type_name.starts_with('map[') && value is MapValue {
		key_type_name, value_type_name := split_map_type(type_name)
		qualified_key_type_name := e.qualify_nested_type_name(e.current_module_name(),
			key_type_name)
		qualified_value_type_name := e.qualify_nested_type_name(e.current_module_name(),
			value_type_name)
		mut m := MapValue{
			key_type_name:   qualified_key_type_name
			value_type_name: qualified_value_type_name
			default_value:   e.adapt_value_to_type_name(value.default_value,
				qualified_value_type_name)
		}
		for entry in value.entries {
			m = e.map_set_value(m, entry.key, entry.value)
		}
		return m
	}
	target_type_name := e.normalize_type_name(type_name)
	if target_type_name in e.enum_fields {
		return e.enum_value(target_type_name, value)
	}
	if type_name in e.enum_fields {
		return e.enum_value(type_name, value)
	}
	if target_type_name in e.sum_types && !e.value_matches_type_name(value, target_type_name) {
		return e.wrap_sum_value(target_type_name, value)
	}
	if type_name in e.sum_types && !e.value_matches_type_name(value, type_name) {
		return e.wrap_sum_value(type_name, value)
	}
	return value
}

fn (e &Eval) is_option_like_value(value Value) bool {
	if value is StructValue {
		return 'data' in value.fields && ('state' in value.fields || 'is_error' in value.fields)
	}
	return false
}

fn (e &Eval) wrap_sum_value(type_name string, value Value) Value {
	return SumValue{
		type_name:    type_name
		variant_name: e.runtime_type_name(value)
		payload:      value
	}
}

fn (e &Eval) unwrap_sum_cast_value(value Value, type_name string) Value {
	if value is SumValue {
		if !e.type_name_matches(value.type_name, type_name)
			&& (e.type_name_matches(value.variant_name, type_name)
			|| e.value_matches_type_name(value.payload, type_name)) {
			return value.payload
		}
	}
	return value
}

fn (e &Eval) cast_value(value Value, type_name string) !Value {
	return e.cast_value_in_module(value, type_name, e.current_module_name())
}

fn (e &Eval) cast_value_in_module(value Value, type_name string, module_name string) !Value {
	name := type_name.trim_left('&')
	if name == '' {
		return value
	}
	if alias := e.type_alias_info_in_module(name, module_name) {
		return e.cast_value_in_module(value, alias.target, alias.module_name)
	}
	target_name := e.qualify_type_name(module_name, name)
	source := e.unwrap_sum_cast_value(value, target_name)
	if name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'isize', 'usize',
		'rune', 'char'] {
		return Value(e.value_as_int(source)!)
	}
	if name in ['f32', 'f64'] {
		return Value(e.value_as_f64(source)!)
	}
	if name == 'bool' {
		return Value(e.value_as_bool(source)!)
	}
	if name == 'string' {
		return Value(e.value_string(source))
	}
	if target_name in e.enum_fields {
		return e.enum_value(target_name, source)
	}
	if name in e.enum_fields {
		return e.enum_value(name, source)
	}
	if name.starts_with('?') {
		inner_type := name[1..]
		data := if inner_type.len > 0 {
			e.adapt_value_to_type_name(source, inner_type)
		} else {
			source
		}
		return StructValue{
			type_name: 'Option'
			fields:    {
				'state': Value(i64(0))
				'err':   Value('')
				'data':  data
			}
		}
	}
	if target_name in e.sum_types || name in e.sum_types {
		sum_name := if target_name in e.sum_types { target_name } else { name }
		if e.value_matches_type_name(source, sum_name) {
			return source
		}
		return e.wrap_sum_value(sum_name, source)
	}
	return source
}

fn (e &Eval) type_alias_info_in_module(type_name string, module_name string) ?TypeAliasInfo {
	name := type_name.trim_left('&')
	if name == '' {
		return none
	}
	qualified := e.qualify_type_name(module_name, name)
	if alias := e.type_aliases[qualified] {
		return alias
	}
	if alias := e.type_aliases[name] {
		return alias
	}
	return none
}

fn (e &Eval) value_matches_type_name(value Value, target_name string) bool {
	name := e.normalize_type_name(target_name)
	match value {
		bool {
			return name == 'bool'
		}
		i64 {
			return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64',
				'isize', 'usize', 'rune', 'char']
		}
		EnumValue {
			return e.type_name_matches(value.type_name, name)
		}
		f64 {
			return name in ['f32', 'f64']
		}
		string {
			return name == 'string'
		}
		ArrayValue {
			if name == 'array' {
				return true
			}
			if !name.starts_with('[]') {
				return false
			}
			return e.type_name_matches(value.elem_type_name, name[2..])
		}
		MapValue {
			if !name.starts_with('map[') {
				return false
			}
			key_type, value_type := split_map_type(name)
			return e.type_name_matches(value.key_type_name, key_type)
				&& e.type_name_matches(value.value_type_name, value_type)
		}
		StructValue {
			return e.type_name_matches(value.type_name, name)
		}
		SumValue {
			return e.type_name_matches(value.type_name, name)
				|| e.type_name_matches(value.variant_name, name)
				|| e.value_matches_type_name(value.payload, name)
		}
		TypeValue {
			return name == 'Type' || name == 'types.Type'
		}
		else {
			return false
		}
	}
}

fn (e &Eval) type_name_matches(actual string, expected string) bool {
	return e.normalize_type_name(actual) == e.normalize_type_name(expected)
}

fn (e &Eval) type_value_module_name(value TypeValue) string {
	if value.name.contains('.') {
		return value.name.all_before_last('.')
	}
	return e.current_module_name()
}

fn is_builtin_type_name(name string) bool {
	return name in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'byte', 'u16', 'u32',
		'u64', 'usize', 'f32', 'f64', 'rune', 'char', 'string', 'void', 'voidptr', 'charptr',
		'byteptr', 'array']
}

fn (e &Eval) qualify_type_name(module_name string, type_name string) string {
	name := e.resolve_import_alias_type_name(type_name)
	if name == '' || name.contains('.') || name.starts_with('[]') || name.starts_with('map[')
		|| is_builtin_type_name(name) {
		return name
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${name}'
	}
	return name
}

fn (e &Eval) resolve_import_alias_type_name(type_name string) string {
	if !type_name.contains('.') {
		return type_name
	}
	mod_alias := type_name.all_before('.')
	real_module := e.resolve_module_name(mod_alias)
	if real_module == mod_alias {
		return type_name
	}
	return '${real_module}.${type_name.all_after('.')}'
}

fn (e &Eval) normalize_type_name(type_name string) string {
	name := type_name.trim_left('&')
	if name == '' {
		return ''
	}
	return e.qualify_type_name(e.current_module_name(), name)
}

fn (e &Eval) sizeof_type_name(name string) i64 {
	return match name {
		'bool', 'i8', 'u8', 'byte', 'char' { i64(1) }
		'i16', 'u16' { i64(2) }
		'int', 'i32', 'u32', 'rune', 'f32' { i64(4) }
		'i64', 'u64', 'isize', 'usize', 'f64' { i64(8) }
		else { i64(8) }
	}
}

fn (e &Eval) expect_string_arg(args []Value, index int) !string {
	if index >= args.len {
		return error('v3.eval: missing argument ${index}')
	}
	return e.value_string(args[index])
}

fn (e &Eval) value_as_bool(value Value) !bool {
	match value {
		bool { return value }
		i64 { return value != 0 }
		EnumValue { return value.value != 0 }
		VoidValue { return false }
		else { return error('v3.eval: `${e.runtime_type_name(value)}` can not be used as bool') }
	}
}

fn (e &Eval) value_as_int(value Value) !i64 {
	match value {
		i64 {
			return value
		}
		EnumValue {
			return value.value
		}
		f64 {
			return i64(value)
		}
		bool {
			return if value { i64(1) } else { i64(0) }
		}
		string {
			return strconv.parse_int(value, 10, 64) or { i64(0) }
		}
		StructValue {
			if 'value' in value.fields {
				return e.value_as_int(value.fields['value'] or { void_value() })
			}
		}
		else {}
	}

	return error('v3.eval: `${e.runtime_type_name(value)}` can not be used as int')
}

fn (e &Eval) value_as_f64(value Value) !f64 {
	match value {
		f64 { return value }
		i64 { return f64(value) }
		EnumValue { return f64(value.value) }
		bool { return if value { 1.0 } else { 0.0 } }
		string { return strconv.atof64(value) or { 0.0 } }
		else { return error('v3.eval: `${e.runtime_type_name(value)}` can not be used as float') }
	}
}

fn (e &Eval) value_string(value Value) string {
	match value {
		bool {
			return value.str()
		}
		i64 {
			return value.str()
		}
		EnumValue {
			return e.enum_value_string(value)
		}
		f64 {
			return value.str()
		}
		string {
			return value
		}
		VoidValue {
			return ''
		}
		ArrayValue {
			return '[' + value.values.map(e.value_string(it)).join(', ') + ']'
		}
		MapValue {
			return '{' +
				value.entries.map('${e.value_string(it.key)}: ${e.value_string(it.value)}').join(', ') +
				'}'
		}
		ModuleValue {
			return value.name
		}
		RangeValue {
			return '${value.start}..${value.end}'
		}
		StructValue {
			if value.type_name == 'os.Result' && 'output' in value.fields {
				return e.value_string(value.fields['output'] or { void_value() })
			}
			return e.default_struct_string(value)
		}
		SumValue {
			return e.value_string(value.payload)
		}
		TupleValue {
			return value.values.map(e.value_string(it)).join(', ')
		}
		TypeValue {
			return value.name
		}
		FnValue {
			return '<fn>'
		}
	}
}

fn (mut e Eval) display_string(value Value) !string {
	match value {
		ArrayValue {
			mut parts := []string{cap: value.values.len}
			for item in value.values {
				parts << e.display_string(item)!
			}
			return '[' + parts.join(', ') + ']'
		}
		MapValue {
			mut parts := []string{cap: value.entries.len}
			for entry in value.entries {
				key := e.display_string(entry.key)!
				map_value := e.display_string(entry.value)!
				parts << '${key}: ${map_value}'
			}
			return '{' + parts.join(', ') + '}'
		}
		StructValue {
			if value.type_name == 'os.Result' && 'output' in value.fields {
				return e.display_string(value.fields['output'] or { void_value() })
			}
			if target := e.resolve_method_target(value.type_name, 'str') {
				result := e.call_method_target(value, target, [])!
				return e.value_string(result.value)
			}
			return e.default_struct_string(value)
		}
		SumValue {
			return e.display_string(value.payload)
		}
		TupleValue {
			mut parts := []string{cap: value.values.len}
			for item in value.values {
				parts << e.display_string(item)!
			}
			return parts.join(', ')
		}
		else {
			return e.value_string(value)
		}
	}
}

fn (e &Eval) default_struct_string(value StructValue) string {
	if value.fields.len == 0 {
		return '${value.type_name}{}'
	}
	mut names := []string{}
	fields := e.struct_fields(value.type_name)
	for field in fields {
		if field.name in value.fields {
			names << field.name
		}
	}
	if names.len == 0 {
		names = value.fields.keys()
		names.sort()
	}
	mut out := '${value.type_name}{\n'
	for name in names {
		field_value := value.fields[name] or { void_value() }
		out += '    ${name}: ${e.struct_field_value_string(field_value)}\n'
	}
	out += '}'
	return out
}

fn (e &Eval) struct_field_value_string(value Value) string {
	match value {
		string {
			return "'${value}'"
		}
		else {
			return e.value_string(value)
		}
	}
}

fn (e &Eval) enum_value_string(value EnumValue) string {
	enum_name := e.normalize_type_name(value.type_name)
	if fields := e.enum_fields[enum_name] {
		module_name := if enum_name.contains('.') {
			enum_name.all_before_last('.')
		} else {
			e.current_module_name()
		}
		short_name := enum_name.all_after_last('.')
		if module_name in e.consts {
			for field in fields {
				if entry := e.consts[module_name]['${short_name}.${field}'] {
					if e.value_as_int(entry.value) or { i64(-1) } == value.value {
						return field
					}
				}
			}
		}
		index := int(value.value)
		if index >= 0 && index < fields.len {
			return fields[index]
		}
	}
	return value.value.str()
}

fn (e &Eval) runtime_type_name(value Value) string {
	match value {
		bool { return 'bool' }
		i64 { return 'int' }
		EnumValue { return value.type_name }
		f64 { return 'f64' }
		string { return 'string' }
		VoidValue { return 'void' }
		ArrayValue { return '[]${value.elem_type_name}' }
		MapValue { return 'map[${value.key_type_name}]${value.value_type_name}' }
		ModuleValue { return 'module' }
		RangeValue { return 'range' }
		StructValue { return value.type_name }
		SumValue { return value.type_name }
		TupleValue { return 'tuple' }
		TypeValue { return value.name }
		FnValue { return 'fn' }
	}
}

fn (mut e Eval) write_stdout(s string) {
	if e.capture_output {
		e.stdout_data += s
	} else {
		print(s)
	}
}

fn (mut e Eval) write_stderr(s string) {
	if e.capture_output {
		e.stderr_data += s
	} else {
		eprint(s)
	}
}
