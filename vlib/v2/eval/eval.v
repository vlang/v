module eval

import os
import os.cmdline as host_cmdline
import strconv
import time
import v2.ast
import v2.parser
import v2.pref
import v2.token

type Value = ArrayValue
	| FlagsValue
	| MapValue
	| ModuleValue
	| RangeValue
	| StructValue
	| TupleValue
	| TypeValue
	| VoidValue
	| bool
	| f64
	| i64
	| string

struct ArrayValue {
mut:
	elem_type_name string
	values         []Value
}

struct FlagsValue {}

struct MapEntry {
	key   Value
	value Value
}

struct MapValue {
	default_value Value
mut:
	entries []MapEntry
}

struct ModuleValue {
	name string
}

struct RangeValue {
	start     i64
	end       i64
	inclusive bool
}

struct StructValue {
	type_name string
mut:
	fields map[string]Value
}

struct TupleValue {
	values []Value
}

struct TypeValue {
	name string
}

struct VoidValue {}

struct FunctionDef {
	decl      ast.FnDecl
	file_name string
}

struct ConstEntry {
	expr      ast.Expr
	file_name string
mut:
	cached     bool
	evaluating bool
	value      Value
}

struct SumTypeInfo {
	module_name string
	name        string
mut:
	variant_tags      map[string]int
	tag_field_aliases map[int][]string
}

struct ScopeFrame {
mut:
	vars   map[string]Value
	defers [][]ast.Stmt
}

struct CallFrame {
	module_name string
	file_name   string
	fn_name     string
}

struct MaybeValue {
	found bool
	value Value
}

struct MaybeFunctionTarget {
	found       bool
	module_name string
	fn_name     string
}

struct CallResult {
	values []Value
mut:
	mut_args map[int]Value
}

enum FlowKind {
	normal
	break_
	continue_
	goto_
	return_
}

struct FlowSignal {
	kind   FlowKind
	label  string
	values []Value
}

fn void_value() Value {
	return VoidValue{}
}

fn wrap_result_ok(value Value) Value {
	return StructValue{
		type_name: 'Result'
		fields:    {
			'is_error': Value(false)
			'err':      Value('')
			'data':     value
		}
	}
}

fn wrap_result_err(message string) Value {
	return StructValue{
		type_name: 'Result'
		fields:    {
			'is_error': Value(true)
			'err':      Value(message)
			'data':     void_value()
		}
	}
}

fn wrap_option_ok(value Value) Value {
	return StructValue{
		type_name: 'Option'
		fields:    {
			'state': Value(i64(0))
			'err':   Value('')
			'data':  value
		}
	}
}

fn wrap_option_none() Value {
	return StructValue{
		type_name: 'Option'
		fields:    {
			'state': Value(i64(1))
			'err':   Value('')
			'data':  void_value()
		}
	}
}

// Eval interprets the v2 AST directly for a limited subset of V.
pub struct Eval {
pub mut:
	capture_output bool
	prefs          pref.Preferences
mut:
	stdout_data        string
	stderr_data        string
	functions          map[string]map[string]FunctionDef
	consts             map[string]map[string]ConstEntry
	sum_types          map[string]SumTypeInfo
	struct_field_types map[string]map[string]map[string]ast.Expr
	type_names         map[string]map[string]bool
	file_import_alias  map[string]map[string]string
	modules            map[string]bool
	scopes             []ScopeFrame
	call_stack         []CallFrame
	next_token_pos_id  i64
}

// new returns a new evaluator configured for direct execution.
pub fn new(prefs_ &pref.Preferences) Eval {
	return Eval{
		capture_output: false
		prefs:          *prefs_
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
	tmp_file := os.join_path(os.temp_dir(), 'v2_eval_${os.getpid()}_${time.now().unix_micro()}.v')
	os.write_file(tmp_file, code)!
	defer {
		os.rm(tmp_file) or {}
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&e.prefs)
	files := par.parse_files([tmp_file], mut file_set)
	return e.run_files(files)
}

// run_files executes parsed AST files and invokes `main.main`.
pub fn (mut e Eval) run_files(files []ast.File) ![]Value {
	e.reset()
	e.register_files(files)!
	return e.call_function('main', 'main', []Value{})!.values
}

fn (mut e Eval) reset() {
	e.stdout_data = ''
	e.stderr_data = ''
	e.functions = map[string]map[string]FunctionDef{}
	e.consts = map[string]map[string]ConstEntry{}
	e.sum_types = map[string]SumTypeInfo{}
	e.struct_field_types = map[string]map[string]map[string]ast.Expr{}
	e.type_names = map[string]map[string]bool{}
	e.file_import_alias = map[string]map[string]string{}
	e.modules = map[string]bool{}
	e.scopes = []ScopeFrame{}
	e.call_stack = []CallFrame{}
	e.next_token_pos_id = 0
}

fn (mut e Eval) register_files(files []ast.File) ! {
	for file in files {
		e.modules[file.mod] = true
		mut import_aliases := map[string]string{}
		for imp in file.imports {
			import_aliases[imp.alias] = imp.name.all_after_last('.')
		}
		e.file_import_alias[file.name] = import_aliases.clone()
		if file.mod !in e.functions {
			e.functions[file.mod] = map[string]FunctionDef{}
		}
		if file.mod !in e.consts {
			e.consts[file.mod] = map[string]ConstEntry{}
		}
		if file.mod !in e.type_names {
			e.type_names[file.mod] = map[string]bool{}
		}
		if file.mod !in e.struct_field_types {
			e.struct_field_types[file.mod] = map[string]map[string]ast.Expr{}
		}
		for stmt in file.stmts {
			match stmt {
				ast.ConstDecl {
					for field in stmt.fields {
						key := if field.name.contains('.') {
							field.name.all_after_last('.')
						} else {
							field.name
						}
						e.consts[file.mod][key] = ConstEntry{
							expr:      field.value
							file_name: file.name
							value:     void_value()
						}
					}
				}
				ast.EnumDecl {
					e.type_names[file.mod][stmt.name] = true
					mut next_value := i64(0)
					for field in stmt.fields {
						key := field.name
						value := if field.value is ast.EmptyExpr {
							Value(next_value)
						} else if field.value is ast.BasicLiteral && field.value.kind == .number {
							Value(strconv.parse_int(field.value.value, 0, 64)!)
						} else {
							Value(next_value)
						}
						int_value := e.value_as_int(value)!
						e.consts[file.mod][key] = ConstEntry{
							expr:      ast.empty_expr
							file_name: file.name
							cached:    true
							value:     int_value
						}
						next_value = int_value + 1
					}
				}
				ast.InterfaceDecl {
					e.type_names[file.mod][stmt.name] = true
				}
				ast.FnDecl {
					e.functions[file.mod][stmt.name] = FunctionDef{
						decl:      stmt
						file_name: file.name
					}
					if stmt.is_method {
						receiver_type_name := stmt.receiver.typ.name()
						if receiver_type_name != '' && receiver_type_name != 'EmptyExpr' {
							e.functions[file.mod]['${receiver_type_name}__${stmt.name}'] = FunctionDef{
								decl:      stmt
								file_name: file.name
							}
							short_receiver_name := receiver_type_name.all_after_last('.')
							if short_receiver_name != receiver_type_name {
								e.functions[file.mod]['${short_receiver_name}__${stmt.name}'] = FunctionDef{
									decl:      stmt
									file_name: file.name
								}
							}
							normalized_receiver_name := receiver_type_name.trim_left('&').trim_right('*')
							if normalized_receiver_name != ''
								&& normalized_receiver_name != receiver_type_name {
								e.functions[file.mod]['${normalized_receiver_name}__${stmt.name}'] = FunctionDef{
									decl:      stmt
									file_name: file.name
								}
								short_normalized_name := normalized_receiver_name.all_after_last('.')
								if short_normalized_name != normalized_receiver_name {
									e.functions[file.mod]['${short_normalized_name}__${stmt.name}'] = FunctionDef{
										decl:      stmt
										file_name: file.name
									}
								}
							}
						}
					}
				}
				ast.StructDecl {
					e.type_names[file.mod][stmt.name] = true
					mut field_types := map[string]ast.Expr{}
					for field in stmt.fields {
						field_types[field.name] = field.typ
					}
					e.struct_field_types[file.mod][stmt.name] = field_types.clone()
				}
				ast.TypeDecl {
					e.type_names[file.mod][stmt.name] = true
					if stmt.variants.len > 0 {
						e.register_sum_type(file.mod, stmt)
					}
				}
				else {}
			}
		}
	}
	if 'main' !in e.functions || 'main' !in e.functions['main'] {
		return error('v2.eval: missing main.main entry point')
	}
}

fn add_type_name_alias(mut aliases []string, alias string) {
	if alias != '' && alias !in aliases {
		aliases << alias
	}
}

fn (e &Eval) type_name_aliases(module_name string, type_name string) []string {
	mut aliases := []string{}
	if type_name == '' {
		return aliases
	}
	add_type_name_alias(mut aliases, type_name)
	if type_name.starts_with('[]') {
		for inner_alias in e.type_name_aliases(module_name, type_name[2..]) {
			add_type_name_alias(mut aliases, '[]${inner_alias}')
		}
		return aliases
	}
	if type_name.starts_with('map[') {
		inner := type_name[4..]
		if bracket_idx := inner.index(']') {
			key_name := inner[..bracket_idx]
			value_name := inner[bracket_idx + 1..]
			key_aliases := e.type_name_aliases(module_name, key_name)
			value_aliases := e.type_name_aliases(module_name, value_name)
			for key_alias in key_aliases {
				for value_alias in value_aliases {
					add_type_name_alias(mut aliases, 'map[${key_alias}]${value_alias}')
				}
			}
		}
		return aliases
	}
	if type_name.contains('__') {
		add_type_name_alias(mut aliases, type_name.replace('__', '.'))
		add_type_name_alias(mut aliases, type_name.all_after_last('__'))
		return aliases
	}
	if type_name.contains('.') {
		add_type_name_alias(mut aliases, type_name.replace('.', '__'))
		add_type_name_alias(mut aliases, type_name.all_after_last('.'))
		return aliases
	}
	if module_name != '' {
		add_type_name_alias(mut aliases, '${module_name}.${type_name}')
		add_type_name_alias(mut aliases, '${e.mangled_module_name(module_name)}__${type_name}')
	}
	return aliases
}

fn (e &Eval) sumtype_variant_field_name(variant_name string) string {
	if variant_name.starts_with('[]') {
		return 'Array_${variant_name[2..].replace('.', '__')}'
	}
	if variant_name.starts_with('map[') {
		inner := variant_name[4..]
		if bracket_idx := inner.index(']') {
			key := inner[..bracket_idx].replace('.', '__')
			val := inner[bracket_idx + 1..].replace('.', '__')
			return 'Map_${key}_${val}'
		}
	}
	if variant_name.contains('__') {
		return variant_name.all_after_last('__')
	}
	if variant_name.contains('.') {
		return variant_name.all_after_last('.')
	}
	return variant_name
}

fn (mut e Eval) register_sum_type(module_name string, decl ast.TypeDecl) {
	mut info := SumTypeInfo{
		module_name:       module_name
		name:              decl.name
		variant_tags:      map[string]int{}
		tag_field_aliases: map[int][]string{}
	}
	for tag, variant in decl.variants {
		variant_name := e.type_expr_name(variant)
		for alias in e.type_name_aliases(module_name, variant_name) {
			info.variant_tags[alias] = tag
			field_alias := '_' + e.sumtype_variant_field_name(alias)
			mut field_aliases := []string{}
			if tag in info.tag_field_aliases {
				field_aliases = info.tag_field_aliases[tag]
			}
			if field_alias !in field_aliases {
				field_aliases << field_alias
			}
			info.tag_field_aliases[tag] = field_aliases
		}
	}
	for alias in e.type_name_aliases(module_name, decl.name) {
		e.sum_types[alias] = info
	}
}

fn (e &Eval) sum_type_info(type_name string) ?SumTypeInfo {
	trimmed := type_name.trim_left('&').trim_right('*')
	if trimmed in e.sum_types {
		return e.sum_types[trimmed]
	}
	if trimmed.contains('.') {
		short_name := trimmed.all_after_last('.')
		if short_name in e.sum_types {
			return e.sum_types[short_name]
		}
	}
	if trimmed.contains('__') {
		short_name := trimmed.all_after_last('__')
		if short_name in e.sum_types {
			return e.sum_types[short_name]
		}
		dotted := trimmed.replace('__', '.')
		if dotted in e.sum_types {
			return e.sum_types[dotted]
		}
	}
	return none
}

fn (e &Eval) is_sum_type_name(type_name string) bool {
	_ = e.sum_type_info(type_name) or { return false }
	return true
}

fn (e &Eval) lookup_sumtype_variant_tag(info SumTypeInfo, variant_name string) ?int {
	for alias in e.type_name_aliases(info.module_name, variant_name) {
		if alias in info.variant_tags {
			return info.variant_tags[alias]
		}
	}
	return none
}

fn (e &Eval) sumtype_tag_from_value(info SumTypeInfo, value Value) ?int {
	match value {
		StructValue {
			for alias in e.type_name_aliases(info.module_name, value.type_name) {
				if alias in info.variant_tags {
					return info.variant_tags[alias]
				}
			}
		}
		string {
			return e.lookup_sumtype_variant_tag(info, 'string')
		}
		bool {
			return e.lookup_sumtype_variant_tag(info, 'bool')
		}
		i64 {
			for alias in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte',
				'char', 'rune'] {
				if tag := e.lookup_sumtype_variant_tag(info, alias) {
					return tag
				}
			}
		}
		f64 {
			for alias in ['f64', 'f32'] {
				if tag := e.lookup_sumtype_variant_tag(info, alias) {
					return tag
				}
			}
		}
		ArrayValue {
			mut tags := []int{}
			for alias, tag in info.variant_tags {
				if alias.starts_with('[]') && tag !in tags {
					tags << tag
				}
			}
			if tags.len == 1 {
				return tags[0]
			}
		}
		MapValue {
			mut tags := []int{}
			for alias, tag in info.variant_tags {
				if alias.starts_with('map[') && tag !in tags {
					tags << tag
				}
			}
			if tags.len == 1 {
				return tags[0]
			}
		}
		TypeValue {
			return e.lookup_sumtype_variant_tag(info, 'Type')
		}
		else {}
	}
	return none
}

fn (e &Eval) lookup_const_expr(module_name string, name string) ?ast.Expr {
	if module_name !in e.consts {
		return none
	}
	entry := e.consts[module_name][name] or { return none }
	if entry.expr is ast.EmptyExpr {
		return none
	}
	return entry.expr
}

fn (e &Eval) sumtype_variant_name_from_expr(expr ast.Expr) ?string {
	return e.sumtype_variant_name_from_expr_with_depth(expr, 0)
}

fn (e &Eval) sumtype_variant_name_from_expr_with_depth(expr ast.Expr, depth int) ?string {
	if depth > 8 {
		return none
	}
	return match expr {
		ast.ArrayInitExpr {
			if expr.typ is ast.EmptyExpr {
				none
			} else {
				e.type_expr_name(expr.typ)
			}
		}
		ast.BasicLiteral {
			match expr.kind {
				.key_false, .key_true {
					'bool'
				}
				.char {
					'char'
				}
				.number {
					if expr.value.contains('.') || expr.value.contains('e')
						|| expr.value.contains('E') {
						'f64'
					} else {
						'int'
					}
				}
				else {
					none
				}
			}
		}
		ast.CallOrCastExpr {
			if e.is_type_expr(expr.lhs) {
				e.type_expr_name(expr.lhs)
			} else {
				none
			}
		}
		ast.CastExpr {
			e.type_expr_name(expr.typ)
		}
		ast.InitExpr {
			e.type_expr_name(expr.typ)
		}
		ast.Ident {
			cur_module := e.current_module_name()
			if const_expr := e.lookup_const_expr(cur_module, expr.name) {
				e.sumtype_variant_name_from_expr_with_depth(const_expr, depth + 1)
			} else {
				if const_expr2 := e.lookup_const_expr('builtin', expr.name) {
					e.sumtype_variant_name_from_expr_with_depth(const_expr2, depth + 1)
				} else {
					none
				}
			}
		}
		ast.MapInitExpr {
			if expr.typ is ast.EmptyExpr {
				none
			} else {
				e.type_expr_name(expr.typ)
			}
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				module_name := e.resolve_module_name(expr.lhs.name)
				if module_name != '' {
					if const_expr := e.lookup_const_expr(module_name, expr.rhs.name) {
						e.sumtype_variant_name_from_expr_with_depth(const_expr, depth + 1)
					} else {
						none
					}
				} else {
					none
				}
			} else {
				none
			}
		}
		ast.StringInterLiteral, ast.StringLiteral {
			'string'
		}
		else {
			none
		}
	}
}

fn (e &Eval) build_sumtype_wrapper(type_name string, info SumTypeInfo, tag int, payload Value) StructValue {
	mut data_fields := map[string]Value{}
	mut field_aliases := []string{}
	if tag in info.tag_field_aliases {
		field_aliases = info.tag_field_aliases[tag]
	}
	for field_alias in field_aliases {
		data_fields[field_alias] = payload
	}
	return StructValue{
		type_name: type_name
		fields:    {
			'_tag':  Value(i64(tag))
			'_data': Value(StructValue{
				type_name: '${type_name}._data'
				fields:    data_fields
			})
		}
	}
}

fn (e &Eval) wrap_sumtype_value(sum_type_name string, value Value, expr ast.Expr) !Value {
	if value is StructValue && value.type_name == sum_type_name && '_tag' in value.fields
		&& '_data' in value.fields {
		return value
	}
	info := e.sum_type_info(sum_type_name) or {
		return error('v2.eval: unknown sum type `${sum_type_name}`')
	}
	if variant_name := e.sumtype_variant_name_from_expr(expr) {
		if tag := e.lookup_sumtype_variant_tag(info, variant_name) {
			return e.build_sumtype_wrapper(sum_type_name, info, tag, value)
		}
	}
	if tag := e.sumtype_tag_from_value(info, value) {
		return e.build_sumtype_wrapper(sum_type_name, info, tag, value)
	}
	return error('v2.eval: can not wrap `${e.runtime_type_name(value)}` into `${sum_type_name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
}

fn (e &Eval) unwrap_sumtype_value(sum_value StructValue, target_type string) ?Value {
	info := e.sum_type_info(sum_value.type_name) or { return none }
	tag_value := sum_value.fields['_tag'] or { return none }
	tag := int(e.value_as_int(tag_value) or { return none })
	target_tag := e.lookup_sumtype_variant_tag(info, target_type) or { return none }
	if tag != target_tag {
		return none
	}
	data_value := sum_value.fields['_data'] or { return none }
	if data_value !is StructValue {
		return none
	}
	data_struct := data_value as StructValue
	mut field_aliases := []string{}
	if tag in info.tag_field_aliases {
		field_aliases = info.tag_field_aliases[tag]
	}
	for field_alias in field_aliases {
		for key, payload in data_struct.fields {
			if key == field_alias {
				return payload
			}
		}
	}
	for _, payload in data_struct.fields {
		return payload
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
		return 'main.main'
	}
	frame := e.call_stack[e.call_stack.len - 1]
	return '${frame.module_name}.${frame.fn_name}'
}

fn (e &Eval) call_stack_trace() string {
	if e.call_stack.len == 0 {
		return 'main.main'
	}
	return e.call_stack.map('${it.module_name}.${it.fn_name}').join(' <- ')
}

fn (e &Eval) unknown_variable_error(name string) IError {
	return error('v2.eval: unknown variable `${name}` in `${e.current_function_label()}`')
}

fn call_result_value(result CallResult) Value {
	return if result.values.len == 0 {
		void_value()
	} else if result.values.len == 1 {
		result.values[0]
	} else {
		TupleValue{
			values: result.values
		}
	}
}

fn (mut e Eval) writeback_mut_args(arg_exprs []ast.Expr, result CallResult) ! {
	for idx, value in result.mut_args {
		if idx < 0 || idx >= arg_exprs.len {
			continue
		}
		if can_update_target(arg_exprs[idx]) {
			e.update_target(arg_exprs[idx], value)!
		}
	}
}

fn (mut e Eval) call_function(module_name string, fn_name string, args []Value) !CallResult {
	builtin_result := e.maybe_call_builtin_function(module_name, fn_name, args)
	if builtin_result.found {
		return CallResult{
			values:   [builtin_result.value]
			mut_args: map[int]Value{}
		}
	}
	if target := e.resolve_mangled_function_target(fn_name) {
		return e.call_function(target.module_name, target.fn_name, args)
	}
	if module_name !in e.functions {
		return error('v2.eval: unknown module `${module_name}`')
	}
	def := e.functions[module_name][fn_name] or {
		return error('v2.eval: unknown function `${module_name}.${fn_name}`')
	}
	e.call_stack << CallFrame{
		module_name: module_name
		file_name:   def.file_name
		fn_name:     fn_name
	}
	defer {
		e.call_stack.pop()
	}
	scope_start := e.scopes.len
	e.open_scope()
	defer {
		for e.scopes.len > scope_start {
			e.close_scope() or {}
		}
	}
	has_receiver_arg := def.decl.is_method && !def.decl.is_static
	expected_args := def.decl.typ.params.len + if has_receiver_arg { 1 } else { 0 }
	if expected_args != args.len {
		return error('v2.eval: `${module_name}.${fn_name}` expected ${expected_args} arguments, got ${args.len}')
	}
	arg_offset := if has_receiver_arg { 1 } else { 0 }
	if has_receiver_arg && def.decl.receiver.name != '' && def.decl.receiver.name != '_' {
		receiver_value := if def.decl.receiver.typ is ast.EmptyExpr {
			args[0]
		} else {
			e.adapt_value_to_type(args[0], def.decl.receiver.typ)
		}
		e.declare_var(def.decl.receiver.name, receiver_value)
	}
	for i, param in def.decl.typ.params {
		param_value := if param.typ is ast.EmptyExpr {
			args[i + arg_offset]
		} else {
			e.adapt_value_to_type(args[i + arg_offset], param.typ)
		}
		e.declare_var(param.name, param_value)
	}
	signal := e.exec_stmts(def.decl.stmts)!
	mut returned_values := []Value{}
	if signal.kind == .return_ {
		returned_values = signal.values.clone()
	} else if signal.kind == .goto_ {
		return error('v2.eval: unknown goto label `${signal.label}`')
	} else if signal.kind != .normal {
		return error('v2.eval: unexpected `${signal.kind}` escaped `${module_name}.${fn_name}`')
	}
	if returned_values.len == 1 && def.decl.typ.return_type !is ast.EmptyExpr {
		returned_values[0] = e.adapt_value_to_type(returned_values[0], def.decl.typ.return_type)
	}
	if def.decl.typ.return_type is ast.Type {
		match def.decl.typ.return_type {
			ast.OptionType {
				payload := if returned_values.len == 0 {
					void_value()
				} else if returned_values.len == 1 {
					returned_values[0]
				} else {
					Value(TupleValue{
						values: returned_values
					})
				}
				return CallResult{
					values:   [wrap_option_ok(payload)]
					mut_args: map[int]Value{}
				}
			}
			ast.ResultType {
				payload := if returned_values.len == 0 {
					void_value()
				} else if returned_values.len == 1 {
					returned_values[0]
				} else {
					Value(TupleValue{
						values: returned_values
					})
				}
				return CallResult{
					values:   [wrap_result_ok(payload)]
					mut_args: map[int]Value{}
				}
			}
			else {}
		}
	}
	mut result := CallResult{
		values:   returned_values
		mut_args: map[int]Value{}
	}
	if has_receiver_arg && def.decl.receiver.is_mut && def.decl.receiver.name != ''
		&& def.decl.receiver.name != '_' {
		receiver_value := e.lookup_var(def.decl.receiver.name)
		if receiver_value.found {
			result.mut_args[0] = receiver_value.value
		}
	}
	for i, param in def.decl.typ.params {
		if !param.is_mut || param.name == '' || param.name == '_' {
			continue
		}
		param_value := e.lookup_var(param.name)
		if param_value.found {
			result.mut_args[i + arg_offset] = param_value.value
		}
	}
	return result
}

fn safe_arg(args []Value, idx int) Value {
	if idx < args.len {
		return args[idx]
	}
	return void_value()
}

fn builtin_method_name(fn_name string) string {
	if !fn_name.contains('__') {
		return fn_name
	}
	return fn_name.all_after_last('__')
}

fn builtin_receiver_name(fn_name string) string {
	if !fn_name.contains('__') {
		return ''
	}
	mut prefix := fn_name.all_before_last('__')
	if prefix.contains('__') {
		prefix = prefix.all_after_last('__')
	}
	return prefix
}

fn (e &Eval) expect_token_arg(args []Value, index int) !token.Token {
	raw := e.value_as_int(safe_arg(args, index))!
	// Enum values are stored as ints in eval, so host-side token helpers need a narrow cast here.
	return unsafe { token.Token(int(raw)) }
}

fn (e &Eval) expect_byte_arg(args []Value, index int) !u8 {
	raw := e.value_as_int(safe_arg(args, index))!
	return u8(raw)
}

fn (e &Eval) infer_array_elem_type(values []Value) string {
	if values.len == 0 {
		return ''
	}
	return e.runtime_type_name(values[0])
}

fn (e &Eval) array_elem_type_name(expr ast.Expr) string {
	return match expr {
		ast.Type {
			match expr {
				ast.ArrayType, ast.ArrayFixedType {
					e.type_expr_name(expr.elem_type)
				}
				else {
					''
				}
			}
		}
		else {
			type_name := e.type_expr_name(expr)
			if type_name.starts_with('[]') {
				type_name[2..]
			} else if type_name.starts_with('[') && type_name.contains(']') {
				type_name.all_after(']')
			} else {
				''
			}
		}
	}
}

fn (e &Eval) annotate_value_for_type(value Value, typ ast.Expr) Value {
	match value {
		ArrayValue {
			elem_type_name := if value.elem_type_name != '' {
				value.elem_type_name
			} else {
				e.array_elem_type_name(typ)
			}
			return ArrayValue{
				elem_type_name: elem_type_name
				values:         value.values
			}
		}
		else {
			return value
		}
	}
}

fn (e &Eval) adapt_value_to_type(value Value, typ ast.Expr) Value {
	adapted := e.annotate_value_for_type(value, typ)
	target_name := e.type_expr_name(typ)
	if target_name != '' && e.is_sum_type_name(target_name) {
		return e.cast_value(adapted, target_name) or { adapted }
	}
	return adapted
}

fn (e &Eval) should_append_many(container ArrayValue, value ArrayValue) bool {
	if container.elem_type_name != '' && value.elem_type_name != '' {
		return container.elem_type_name == value.elem_type_name
	}
	if container.elem_type_name != '' && value.values.len > 0 {
		return container.elem_type_name == e.runtime_type_name(value.values[0])
	}
	if container.values.len > 0 && value.values.len > 0 {
		return e.runtime_type_name(container.values[0]) == e.runtime_type_name(value.values[0])
	}
	return false
}

fn (e &Eval) inferred_sumtype_tag(value Value) ?int {
	mut matches := map[string]int{}
	for _, info in e.sum_types {
		if tag := e.sumtype_tag_from_value(info, value) {
			key := '${info.module_name}.${info.name}:${tag}'
			matches[key] = tag
		}
	}
	if matches.len != 1 {
		return none
	}
	for _, tag in matches {
		return tag
	}
	return none
}

fn eval_token_method_value(tok token.Token, method_name string) ?Value {
	return match method_name {
		'str' { Value(tok.str()) }
		'left_binding_power' { Value(i64(int(tok.left_binding_power()))) }
		'right_binding_power' { Value(i64(int(tok.right_binding_power()))) }
		'is_keyword' { Value(tok.is_keyword()) }
		'is_prefix' { Value(tok.is_prefix()) }
		'is_infix' { Value(tok.is_infix()) }
		'is_postfix' { Value(tok.is_postfix()) }
		'is_assignment' { Value(tok.is_assignment()) }
		'is_overloadable' { Value(tok.is_overloadable()) }
		'is_comparison' { Value(tok.is_comparison()) }
		else { none }
	}
}

fn eval_byte_method_value(c u8, method_name string) ?Value {
	return match method_name {
		'is_space' {
			Value(c == 32 || (c > 8 && c < 14) || c == 0x85 || c == 0xa0)
		}
		'is_digit' {
			Value(c >= `0` && c <= `9`)
		}
		'is_hex_digit' {
			Value((c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`))
		}
		'is_oct_digit' {
			Value(c >= `0` && c <= `7`)
		}
		'is_bin_digit' {
			Value(c == `0` || c == `1`)
		}
		'is_letter' {
			Value((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`))
		}
		'is_alnum' {
			Value((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`))
		}
		'is_capital' {
			Value(c >= `A` && c <= `Z`)
		}
		else {
			none
		}
	}
}

fn (e &Eval) maybe_call_token_builtin(fn_name string, args []Value) MaybeValue {
	method_name := builtin_method_name(fn_name)
	receiver_name := builtin_receiver_name(fn_name)
	if receiver_name == 'Token' {
		tok := e.expect_token_arg(args, 0) or { return MaybeValue{} }
		if value := eval_token_method_value(tok, method_name) {
			return MaybeValue{
				found: true
				value: value
			}
		}
		return MaybeValue{}
	}
	if method_name == 'from_string_tinyv' && receiver_name in ['', 'Token'] {
		name := e.expect_string_arg(args, args.len - 1) or { return MaybeValue{} }
		return MaybeValue{
			found: true
			value: i64(int(token.Token.from_string_tinyv(name)))
		}
	}
	return MaybeValue{}
}

fn (e &Eval) maybe_call_ast_builtin(fn_name string, args []Value) MaybeValue {
	method_name := builtin_method_name(fn_name)
	receiver_name := builtin_receiver_name(fn_name)
	if receiver_name in ['', 'StringLiteralKind'] {
		match method_name {
			'str' {
				raw := e.value_as_int(safe_arg(args, 0)) or { return MaybeValue{} }
				// StringLiteralKind is stored as an int in eval and only used with known enum values here.
				kind := unsafe { ast.StringLiteralKind(int(raw)) }
				return MaybeValue{
					found: true
					value: kind.str()
				}
			}
			'from_string_tinyv' {
				name := e.expect_string_arg(args, args.len - 1) or { return MaybeValue{} }
				return MaybeValue{
					found: true
					value: i64(int(ast.StringLiteralKind.from_string_tinyv(name)))
				}
			}
			else {}
		}
	}
	return MaybeValue{}
}

fn (e &Eval) maybe_call_byte_builtin(module_name string, fn_name string, args []Value) MaybeValue {
	if module_name !in ['', 'builtin'] {
		return MaybeValue{}
	}
	receiver_name := builtin_receiver_name(fn_name)
	if receiver_name !in ['u8', 'byte'] {
		return MaybeValue{}
	}
	c := e.expect_byte_arg(args, 0) or { return MaybeValue{} }
	if value := eval_byte_method_value(c, builtin_method_name(fn_name)) {
		return MaybeValue{
			found: true
			value: value
		}
	}
	return MaybeValue{}
}

fn (mut e Eval) maybe_call_builtin_function(module_name string, fn_name string, args []Value) MaybeValue {
	if fn_name == 'free' || fn_name.ends_with('__free') {
		return MaybeValue{
			found: true
			value: void_value()
		}
	}
	if fn_name == 'map__get_check' || fn_name.ends_with('map__get_check')
		|| (fn_name.starts_with('__Map_') && fn_name.ends_with('_get_check')) {
		if args.len >= 2 && args[0] is MapValue {
			map_value := args[0] as MapValue
			value, found := e.map_lookup(map_value, args[1])
			return MaybeValue{
				found: true
				value: if found { value } else { void_value() }
			}
		}
	}
	if fn_name in ['sync__RwMutex_lock', 'sync__RwMutex_unlock', 'sync__RwMutex_rlock', 'sync__RwMutex_runlock']
		|| fn_name.ends_with('RwMutex_lock') || fn_name.ends_with('RwMutex_unlock')
		|| fn_name.ends_with('RwMutex_rlock') || fn_name.ends_with('RwMutex_runlock') {
		return MaybeValue{
			found: true
			value: void_value()
		}
	}
	if module_name == 'token' {
		token_result := e.maybe_call_token_builtin(fn_name, args)
		if token_result.found {
			return token_result
		}
	}
	if module_name == 'ast' {
		ast_result := e.maybe_call_ast_builtin(fn_name, args)
		if ast_result.found {
			return ast_result
		}
	}
	if fn_name.ends_with('__str') && args.len == 1 {
		return MaybeValue{
			found: true
			value: e.value_string(args[0])
		}
	}
	if fn_name.ends_with('__eq') && args.len == 2 {
		return MaybeValue{
			found: true
			value: e.value_eq(args[0], args[1])
		}
	}
	if fn_name.starts_with('string__') && args.len >= 1 {
		receiver := e.value_string(args[0])
		method_name := fn_name.all_after('string__')
		if result := e.call_string_method(receiver, method_name, args[1..]) {
			return MaybeValue{
				found: true
				value: result
			}
		}
	}
	if array_helper := e.maybe_call_generated_array_helper(fn_name, args) {
		return array_helper
	}
	byte_result := e.maybe_call_byte_builtin(module_name, fn_name, args)
	if byte_result.found {
		return byte_result
	}
	if fn_name in ['builtin__new_array_from_c_array_noscan', 'builtin__new_array_from_c_array', 'new_array_from_c_array_noscan', 'new_array_from_c_array']
		&& args.len >= 4 {
		mut values := []Value{}
		if args[3] is ArrayValue {
			array_arg := args[3] as ArrayValue
			values = array_arg.values.clone()
		} else if args[3] is string {
			string_arg := args[3] as string
			for b in string_arg.bytes() {
				values << i64(b)
			}
		}
		target_len := int(e.value_as_int(args[0]) or { values.len })
		if target_len >= 0 && target_len < values.len {
			values = values[..target_len].clone()
		}
		return MaybeValue{
			found: true
			value: ArrayValue{
				values: values
			}
		}
	}
	if fn_name == '__new_array_with_default_noscan' && args.len >= 4 {
		target_len := int(e.value_as_int(args[0]) or { 0 })
		init_value := args[3]
		mut values := []Value{cap: target_len}
		for _ in 0 .. target_len {
			values << init_value
		}
		return MaybeValue{
			found: true
			value: ArrayValue{
				values: values
			}
		}
	}
	if module_name in ['', 'builtin'] {
		match fn_name {
			'arguments' {
				return MaybeValue{
					found: true
					value: ArrayValue{
						values: os.args.map(Value(it))
					}
				}
			}
			'array__slice' {
				if args.len < 3 || args[0] !is ArrayValue {
					return MaybeValue{}
				}
				arr := args[0] as ArrayValue
				start := int(e.value_as_int(args[1]) or { return MaybeValue{} })
				end := int(e.value_as_int(args[2]) or { return MaybeValue{} })
				return MaybeValue{
					found: true
					value: ArrayValue{
						values: arr.values[start..end]
					}
				}
			}
			'print' {
				e.write_stdout(e.value_string(safe_arg(args, 0)))
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'println' {
				e.write_stdout(e.value_string(safe_arg(args, 0)) + '\n')
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'eprint' {
				e.write_stderr(e.value_string(safe_arg(args, 0)))
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'eprintln' {
				e.write_stderr(e.value_string(safe_arg(args, 0)) + '\n')
				return MaybeValue{
					found: true
					value: void_value()
				}
			}
			'panic' {
				panic(e.value_string(safe_arg(args, 0)))
			}
			else {}
		}
	}
	if module_name == 'token' && fn_name.ends_with('File__pos') && args.len >= 2 {
		offset := e.value_as_int(args[1]) or { return MaybeValue{} }
		mut base := i64(0)
		mut size := i64(0)
		if args[0] is StructValue {
			file := args[0] as StructValue
			if base_value := file.fields['base'] {
				base = e.value_as_int(base_value) or { 0 }
			}
			if size_value := file.fields['size'] {
				size = e.value_as_int(size_value) or { 0 }
			}
		}
		if offset > size {
			return MaybeValue{}
		}
		e.next_token_pos_id++
		return MaybeValue{
			found: true
			value: StructValue{
				type_name: 'token__Pos'
				fields:    {
					'offset': Value(base + offset)
					'id':     Value(e.next_token_pos_id)
				}
			}
		}
	}
	if module_name == 'os' {
		match fn_name {
			'arguments' {
				return MaybeValue{
					found: true
					value: ArrayValue{
						values: os.args.map(Value(it))
					}
				}
			}
			'dir' {
				return MaybeValue{
					found: true
					value: os.dir(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'file_name' {
				return MaybeValue{
					found: true
					value: os.file_name(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'join_path' {
				mut parts := e.array_args_to_strings(args)
				if parts.len == 0 {
					return MaybeValue{
						found: true
						value: ''
					}
				}
				mut joined := parts[0]
				for part in parts[1..] {
					joined = os.join_path(joined, part)
				}
				return MaybeValue{
					found: true
					value: joined
				}
			}
			'exists' {
				return MaybeValue{
					found: true
					value: os.exists(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'is_dir' {
				return MaybeValue{
					found: true
					value: os.is_dir(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'getwd' {
				return MaybeValue{
					found: true
					value: os.getwd()
				}
			}
			'temp_dir' {
				return MaybeValue{
					found: true
					value: os.temp_dir()
				}
			}
			'getenv' {
				return MaybeValue{
					found: true
					value: os.getenv(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'is_abs_path' {
				return MaybeValue{
					found: true
					value: os.is_abs_path(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'norm_path' {
				return MaybeValue{
					found: true
					value: os.norm_path(e.expect_string_arg(args, 0) or { return MaybeValue{} })
				}
			}
			'ls' {
				path := e.expect_string_arg(args, 0) or { return MaybeValue{} }
				if path == '' {
					return MaybeValue{
						found: true
						value: wrap_result_err('os.ls empty path in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
					}
				}
				items := os.ls(path) or {
					return MaybeValue{
						found: true
						value: wrap_result_err(err.msg())
					}
				}
				return MaybeValue{
					found: true
					value: wrap_result_ok(ArrayValue{
						values: items.map(Value(it))
					})
				}
			}
			'read_file' {
				path := e.expect_string_arg(args, 0) or { return MaybeValue{} }
				if path == '' {
					return MaybeValue{
						found: true
						value: wrap_result_err('os.read_file empty path in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
					}
				}
				content := os.read_file(path) or {
					return MaybeValue{
						found: true
						value: wrap_result_err(err.msg())
					}
				}
				return MaybeValue{
					found: true
					value: wrap_result_ok(content)
				}
			}
			'write_file' {
				os.write_file(e.expect_string_arg(args, 0) or { return MaybeValue{} },
					e.expect_string_arg(args, 1) or { return MaybeValue{} }) or {
					return MaybeValue{
						found: true
						value: wrap_result_err(err.msg())
					}
				}
				return MaybeValue{
					found: true
					value: wrap_result_ok(void_value())
				}
			}
			'rm' {
				os.rm(e.expect_string_arg(args, 0) or { return MaybeValue{} }) or {
					return MaybeValue{
						found: true
						value: wrap_result_err(err.msg())
					}
				}
				return MaybeValue{
					found: true
					value: wrap_result_ok(void_value())
				}
			}
			'cp' {
				os.cp(e.expect_string_arg(args, 0) or { return MaybeValue{} }, e.expect_string_arg(args,
					1) or { return MaybeValue{} }) or {
					return MaybeValue{
						found: true
						value: wrap_result_err(err.msg())
					}
				}
				return MaybeValue{
					found: true
					value: wrap_result_ok(void_value())
				}
			}
			else {}
		}
	}
	if module_name == 'time' {
		match fn_name {
			'sys_mono_now', 'sys_mono_now_darwin', 'vpc_now', 'vpc_now_darwin' {
				return MaybeValue{
					found: true
					value: i64(time.now().unix_micro() * 1000)
				}
			}
			else {}
		}
	}
	if module_name == 'token' {
		match fn_name {
			'File__line_start', 'line_start' {
				if args.len < 2 || args[0] !is StructValue {
					return MaybeValue{}
				}
				file_value := args[0] as StructValue
				line := int(e.value_as_int(args[1]) or { return MaybeValue{} })
				if line <= 0 {
					return MaybeValue{
						found: true
						value: i64(0)
					}
				}
				line_offsets := file_value.fields['line_offsets'] or { return MaybeValue{} }
				if line_offsets !is ArrayValue {
					return MaybeValue{}
				}
				offsets := line_offsets as ArrayValue
				idx := line - 1
				if idx < 0 || idx >= offsets.values.len {
					return MaybeValue{
						found: true
						value: i64(0)
					}
				}
				return MaybeValue{
					found: true
					value: offsets.values[idx]
				}
			}
			else {}
		}
	}
	if module_name == 'cmdline' {
		match fn_name {
			'option' {
				if args.len < 1 {
					return MaybeValue{}
				}
				str_arr := e.value_to_string_array(args[0]) or { return MaybeValue{} }
				opt_arg1 := e.expect_string_arg(args, 1) or { return MaybeValue{} }
				opt_arg2 := e.expect_string_arg(args, 2) or { return MaybeValue{} }
				return MaybeValue{
					found: true
					value: host_cmdline.option(str_arr, opt_arg1, opt_arg2)
				}
			}
			'options' {
				if args.len < 1 {
					return MaybeValue{}
				}
				str_arr := e.value_to_string_array(args[0]) or { return MaybeValue{} }
				opt_arg1 := e.expect_string_arg(args, 1) or { return MaybeValue{} }
				return MaybeValue{
					found: true
					value: ArrayValue{
						values: host_cmdline.options(str_arr, opt_arg1).map(Value(it))
					}
				}
			}
			'only_options' {
				if args.len < 1 {
					return MaybeValue{}
				}
				str_arr := e.value_to_string_array(args[0]) or { return MaybeValue{} }
				return MaybeValue{
					found: true
					value: ArrayValue{
						values: host_cmdline.only_options(str_arr).map(Value(it))
					}
				}
			}
			else {}
		}
	}
	return MaybeValue{}
}

fn (e &Eval) clone_array_item_to_depth(value Value, depth int) Value {
	if depth < 0 {
		return value
	}
	return match value {
		ArrayValue {
			e.clone_array_to_depth(value, depth)
		}
		string {
			value.clone()
		}
		else {
			value
		}
	}
}

fn (e &Eval) clone_array_to_depth(array_value ArrayValue, depth int) ArrayValue {
	if depth <= 0 {
		return ArrayValue{
			elem_type_name: array_value.elem_type_name
			values:         array_value.values.clone()
		}
	}
	mut values := []Value{cap: array_value.values.len}
	for item in array_value.values {
		values << e.clone_array_item_to_depth(item, depth - 1)
	}
	return ArrayValue{
		elem_type_name: array_value.elem_type_name
		values:         values
	}
}

fn (mut e Eval) maybe_call_generated_array_helper(fn_name string, args []Value) ?MaybeValue {
	if args.len > 0 && args[0] is ArrayValue {
		array_value := args[0] as ArrayValue
		if fn_name == 'array__clone' || fn_name.ends_with('array__clone') {
			return MaybeValue{
				found: true
				value: e.clone_array_to_depth(array_value, 0)
			}
		}
		if (fn_name == 'array__clone_to_depth' || fn_name.ends_with('array__clone_to_depth'))
			&& args.len >= 2 {
			depth := int(e.value_as_int(args[1]) or { return none })
			return MaybeValue{
				found: true
				value: e.clone_array_to_depth(array_value, depth)
			}
		}
	}
	if !fn_name.starts_with('Array_') || args.len == 0 || args[0] !is ArrayValue {
		return none
	}
	array_value := args[0] as ArrayValue
	if fn_name.ends_with('_contains') && args.len >= 2 {
		return MaybeValue{
			found: true
			value: e.array_contains(array_value, args[1])
		}
	}
	if fn_name.ends_with('_has') && args.len >= 2 {
		return MaybeValue{
			found: true
			value: e.array_has(array_value, args[1])
		}
	}
	if fn_name.ends_with('_index') && !fn_name.ends_with('_last_index') && args.len >= 2 {
		return MaybeValue{
			found: true
			value: e.array_index(array_value, args[1], false)
		}
	}
	if fn_name.ends_with('_last_index') && args.len >= 2 {
		return MaybeValue{
			found: true
			value: e.array_index(array_value, args[1], true)
		}
	}
	if fn_name.ends_with('_join') && args.len >= 2 {
		return MaybeValue{
			found: true
			value: array_value.values.map(e.value_string(it)).join(e.expect_string_arg(args,
				1) or { return none })
		}
	}
	if fn_name.ends_with('_str') {
		return MaybeValue{
			found: true
			value: e.value_string(array_value)
		}
	}
	return none
}

fn (mut e Eval) open_scope() {
	e.scopes << ScopeFrame{
		vars: map[string]Value{}
	}
}

fn (mut e Eval) close_scope() ! {
	if e.scopes.len == 0 {
		return
	}
	scope_idx := e.scopes.len - 1
	defers := e.scopes[scope_idx].defers.clone()
	for i := defers.len - 1; i >= 0; i-- {
		signal := e.exec_stmts(defers[i])!
		if signal.kind != .normal {
			return error('v2.eval: control flow inside defer is not supported')
		}
	}
	e.scopes.pop()
}

fn (mut e Eval) declare_var(name string, value Value) {
	if name == '' || name == '_' {
		return
	}
	if e.scopes.len == 0 {
		e.open_scope()
	}
	e.scopes[e.scopes.len - 1].vars[name] = value
}

fn (mut e Eval) set_var(name string, value Value) ! {
	for i := e.scopes.len - 1; i >= 0; i-- {
		if name in e.scopes[i].vars {
			e.scopes[i].vars[name] = value
			return
		}
	}
	return e.unknown_variable_error(name)
}

fn (e &Eval) lookup_var(name string) MaybeValue {
	for i := e.scopes.len - 1; i >= 0; i-- {
		if value := e.scopes[i].vars[name] {
			return MaybeValue{
				found: true
				value: value
			}
		}
	}
	return MaybeValue{}
}

fn (mut e Eval) exec_stmts(stmts []ast.Stmt) !FlowSignal {
	mut label_positions := map[string]int{}
	for i, stmt in stmts {
		if stmt is ast.LabelStmt {
			label_positions[stmt.name] = i
		}
	}
	mut i := 0
	for i < stmts.len {
		stmt := stmts[i]
		signal := e.exec_stmt(stmt)!
		if signal.kind == .goto_ {
			if signal.label in label_positions {
				i = label_positions[signal.label]
				continue
			}
			return signal
		}
		if signal.kind != .normal {
			return signal
		}
		i++
	}
	return FlowSignal{
		kind: .normal
	}
}

fn (mut e Eval) exec_block(stmts []ast.Stmt) !FlowSignal {
	e.open_scope()
	signal := e.exec_stmts(stmts)!
	e.close_scope()!
	return signal
}

fn (mut e Eval) exec_stmt(stmt ast.Stmt) !FlowSignal {
	match stmt {
		ast.AssertStmt {
			passed := e.value_as_bool(e.eval_expr(stmt.expr)!)!
			if !passed {
				message := if stmt.extra is ast.EmptyExpr {
					'assertion failed'
				} else {
					e.value_string(e.eval_expr(stmt.extra)!)
				}
				return error('v2.eval: ${message}')
			}
		}
		ast.AssignStmt {
			return e.exec_assign_stmt(stmt)
		}
		ast.BlockStmt {
			return e.exec_block(stmt.stmts)
		}
		ast.ComptimeStmt {
			return e.exec_stmt(stmt.stmt)
		}
		ast.DeferStmt {
			if e.scopes.len == 0 {
				return error('v2.eval: defer used outside of a scope')
			}
			e.scopes[e.scopes.len - 1].defers << stmt.stmts
		}
		ast.ExprStmt {
			return e.exec_expr_stmt(stmt.expr)
		}
		ast.FlowControlStmt {
			return match stmt.op {
				.key_break {
					FlowSignal{
						kind: .break_
					}
				}
				.key_continue {
					FlowSignal{
						kind: .continue_
					}
				}
				.key_goto {
					FlowSignal{
						kind:  .goto_
						label: stmt.label
					}
				}
				else {
					return error('v2.eval: unsupported flow control `${stmt.op}`')
				}
			}
		}
		ast.ForStmt {
			return e.exec_for_stmt(stmt)
		}
		ast.ReturnStmt {
			mut values := []Value{cap: stmt.exprs.len}
			for expr in stmt.exprs {
				values << e.eval_expr(expr)!
			}
			return FlowSignal{
				kind:   .return_
				values: values
			}
		}
		ast.ConstDecl, ast.Directive, ast.EnumDecl, ast.FnDecl, ast.GlobalDecl, ast.ImportStmt,
		ast.InterfaceDecl, ast.ModuleStmt, ast.StructDecl, ast.TypeDecl, []ast.Attribute {
			return FlowSignal{
				kind: .normal
			}
		}
		ast.LabelStmt {
			if stmt.stmt is ast.EmptyStmt {
				return FlowSignal{
					kind: .normal
				}
			}
			return e.exec_stmt(stmt.stmt)
		}
		else {
			return error('v2.eval: unsupported statement `${stmt.type_name()}`')
		}
	}
	return FlowSignal{
		kind: .normal
	}
}

fn (mut e Eval) exec_expr_stmt(expr ast.Expr) !FlowSignal {
	match expr {
		ast.ComptimeExpr {
			if expr.expr is ast.IfExpr {
				return e.exec_if_stmt(expr.expr)
			}
			_ = e.eval_expr(expr.expr)!
		}
		ast.IfExpr {
			return e.exec_if_stmt(expr)
		}
		ast.InfixExpr {
			if expr.op == .left_shift {
				e.exec_array_append(expr)!
			} else {
				_ = e.eval_expr(expr)!
			}
		}
		ast.PostfixExpr {
			_ = e.eval_postfix_expr(expr)!
		}
		ast.UnsafeExpr {
			return e.exec_block(expr.stmts)
		}
		else {
			_ = e.eval_expr(expr)!
		}
	}
	return FlowSignal{
		kind: .normal
	}
}

fn (mut e Eval) exec_assign_stmt(stmt ast.AssignStmt) !FlowSignal {
	mut values := []Value{}
	for rhs in stmt.rhs {
		values << e.eval_expr(rhs)!
	}
	if stmt.lhs.len > 1 && values.len == 1 && values[0] is TupleValue {
		tuple_value := values[0] as TupleValue
		values = tuple_value.values.clone()
	}
	if stmt.lhs.len != values.len {
		return error('v2.eval: assignment arity mismatch')
	}
	for i, lhs in stmt.lhs {
		match stmt.op {
			.decl_assign {
				e.assign_target(lhs, values[i], true)!
			}
			.assign {
				e.assign_target(lhs, values[i], false)!
			}
			.plus_assign, .minus_assign, .mul_assign, .div_assign, .mod_assign, .and_assign,
			.or_assign, .xor_assign, .left_shift_assign, .right_shift_assign,
			.right_shift_unsigned_assign {
				current := e.eval_expr(lhs)!
				op := assign_to_infix_token(stmt.op)
				updated := e.apply_infix(op, current, values[i])!
				e.assign_target(lhs, updated, false)!
			}
			else {
				return error('v2.eval: unsupported assignment operator `${stmt.op}`')
			}
		}
	}
	return FlowSignal{
		kind: .normal
	}
}

fn assign_to_infix_token(op token.Token) token.Token {
	return match op {
		.plus_assign { .plus }
		.minus_assign { .minus }
		.mul_assign { .mul }
		.div_assign { .div }
		.mod_assign { .mod }
		.and_assign { .amp }
		.or_assign { .pipe }
		.xor_assign { .xor }
		.left_shift_assign { .left_shift }
		.right_shift_assign { .right_shift }
		.right_shift_unsigned_assign { .right_shift_unsigned }
		else { .unknown }
	}
}

fn (mut e Eval) assign_target(lhs ast.Expr, value Value, declare bool) ! {
	if name := assignable_ident_name(lhs) {
		if name == '_' {
			return
		}
		if declare {
			e.declare_var(name, value)
		} else {
			e.update_target(lhs, value)!
		}
		return
	}
	if declare {
		return error('v2.eval: unsupported declaration target `${lhs.type_name()}`')
	}
	return e.update_target(lhs, value)
}

fn can_update_target(expr ast.Expr) bool {
	if assignable_ident_name(expr) != none || expr is ast.IndexExpr || expr is ast.SelectorExpr {
		return true
	}
	match expr {
		ast.CastExpr, ast.ModifierExpr, ast.ParenExpr {
			return can_update_target(expr.expr)
		}
		else {}
	}
	if expr is ast.PrefixExpr && expr.op in [.mul, .amp] {
		return can_update_target(expr.expr)
	}
	return false
}

fn (mut e Eval) update_target(expr ast.Expr, value Value) ! {
	if name := assignable_ident_name(expr) {
		var_result := e.lookup_var(name)
		if var_result.found {
			e.set_var(name, value)!
			return
		}
		if e.set_runtime_const(e.current_module_name(), name, value) {
			return
		}
		return e.unknown_variable_error(name)
	}
	if expr is ast.IndexExpr {
		return e.update_index_target(expr, value)
	}
	if expr is ast.SelectorExpr {
		return e.update_selector_target(expr, value)
	}
	match expr {
		ast.CastExpr, ast.ModifierExpr, ast.ParenExpr {
			return e.update_target(expr.expr, value)
		}
		else {}
	}
	if expr is ast.PrefixExpr && expr.op in [.mul, .amp] {
		return e.update_target(expr.expr, value)
	}
	return error('v2.eval: unsupported assignment target `${expr.type_name()}`')
}

fn assignable_ident_name(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.ModifierExpr {
			if expr.expr is ast.Ident {
				return expr.expr.name
			}
		}
		else {}
	}
	return none
}

fn (mut e Eval) update_index_target(expr ast.IndexExpr, value Value) ! {
	container := e.eval_expr(expr.lhs)!
	index_value := e.eval_expr(expr.expr)!
	match container {
		ArrayValue {
			index := int(e.value_as_int(index_value)!)
			if index < 0 || index >= container.values.len {
				return error('v2.eval: array index out of bounds')
			}
			mut updated := container
			updated.values[index] = value
			e.update_target(expr.lhs, updated)!
			return
		}
		MapValue {
			e.update_target(expr.lhs, e.map_set_value(container, index_value, value))!
			return
		}
		else {
			return error('v2.eval: indexed assignment expects an array or map')
		}
	}
}

fn (mut e Eval) update_selector_target(expr ast.SelectorExpr, value Value) ! {
	container := e.eval_expr(expr.lhs)!
	match container {
		ArrayValue {
			if expr.rhs.name == 'flags' {
				return
			}
		}
		StructValue {
			mut updated := container
			updated.fields[expr.rhs.name] = value
			e.update_target(expr.lhs, updated)!
			return
		}
		else {
			return error('v2.eval: selector assignment expects a struct, got `${e.runtime_type_name(container)}` in `${e.current_function_label()}`')
		}
	}
}

fn (mut e Eval) exec_array_append(expr ast.InfixExpr) ! {
	container := e.eval_expr(expr.lhs)!
	value := e.eval_expr(expr.rhs)!
	match container {
		ArrayValue {
			mut updated := container
			if value is ArrayValue && e.should_append_many(container, value) {
				updated.values << value.values
			} else {
				updated.values << value
			}
			if updated.elem_type_name == '' {
				updated.elem_type_name = e.infer_array_elem_type(updated.values)
			}
			e.update_target(expr.lhs, updated)!
			return
		}
		else {
			return error('v2.eval: `<<` is only supported for arrays')
		}
	}
}

fn (mut e Eval) exec_for_stmt(stmt ast.ForStmt) !FlowSignal {
	e.open_scope()
	defer {
		e.close_scope() or {}
	}
	if stmt.init is ast.ForInStmt {
		return e.exec_for_in(stmt.init, stmt.stmts)
	}
	if stmt.init !is ast.EmptyStmt {
		init_signal := e.exec_stmt(stmt.init)!
		if init_signal.kind != .normal {
			return init_signal
		}
	}
	for {
		if stmt.cond !is ast.EmptyExpr {
			if !e.value_as_bool(e.eval_expr(stmt.cond)!)! {
				break
			}
		}
		body_signal := e.exec_block(stmt.stmts)!
		if body_signal.kind == .return_ {
			return body_signal
		}
		if body_signal.kind == .break_ {
			break
		}
		if stmt.post !is ast.EmptyStmt {
			post_signal := e.exec_stmt(stmt.post)!
			if post_signal.kind == .return_ {
				return post_signal
			}
			if post_signal.kind == .break_ {
				break
			}
		}
	}
	return FlowSignal{
		kind: .normal
	}
}

fn (mut e Eval) exec_for_in(for_in ast.ForInStmt, body []ast.Stmt) !FlowSignal {
	mut key_name := assignable_ident_name(for_in.key) or { '' }
	mut value_name := assignable_ident_name(for_in.value) or { '' }
	if value_name == '' {
		value_name = key_name
		key_name = ''
	}
	iterable := e.eval_expr(for_in.expr)!
	match iterable {
		RangeValue {
			mut i := iterable.start
			limit := iterable.end
			for {
				if iterable.inclusive {
					if i > limit {
						break
					}
				} else if i >= limit {
					break
				}
				e.open_scope()
				if value_name != '' && value_name != '_' {
					e.declare_var(value_name, i)
				}
				body_signal := e.exec_stmts(body)!
				e.close_scope()!
				if body_signal.kind == .return_ {
					return body_signal
				}
				if body_signal.kind == .break_ {
					break
				}
				i++
			}
		}
		ArrayValue {
			for idx, item in iterable.values {
				e.open_scope()
				if key_name != '' && key_name != '_' {
					e.declare_var(key_name, i64(idx))
				}
				if value_name != '' && value_name != '_' {
					e.declare_var(value_name, item)
				}
				body_signal := e.exec_stmts(body)!
				e.close_scope()!
				if body_signal.kind == .return_ {
					return body_signal
				}
				if body_signal.kind == .break_ {
					break
				}
			}
		}
		MapValue {
			for entry in iterable.entries {
				e.open_scope()
				if key_name != '' && key_name != '_' {
					e.declare_var(key_name, entry.key)
				}
				if value_name != '' && value_name != '_' {
					e.declare_var(value_name, entry.value)
				}
				body_signal := e.exec_stmts(body)!
				e.close_scope()!
				if body_signal.kind == .return_ {
					return body_signal
				}
				if body_signal.kind == .break_ {
					break
				}
			}
		}
		string {
			for idx, ch in iterable.runes() {
				e.open_scope()
				if key_name != '' && key_name != '_' {
					e.declare_var(key_name, i64(idx))
				}
				if value_name != '' && value_name != '_' {
					e.declare_var(value_name, ch.str())
				}
				body_signal := e.exec_stmts(body)!
				e.close_scope()!
				if body_signal.kind == .return_ {
					return body_signal
				}
				if body_signal.kind == .break_ {
					break
				}
			}
		}
		else {
			return error('v2.eval: unsupported for-in iterable `${e.runtime_type_name(iterable)}`')
		}
	}
	return FlowSignal{
		kind: .normal
	}
}

fn (mut e Eval) exec_if_stmt(expr ast.IfExpr) !FlowSignal {
	should_run := if expr.cond is ast.EmptyExpr {
		true
	} else {
		e.value_as_bool(e.eval_expr(expr.cond)!)!
	}
	if should_run {
		return e.exec_block(expr.stmts)
	}
	if expr.else_expr is ast.IfExpr {
		return e.exec_if_stmt(expr.else_expr)
	}
	if expr.else_expr is ast.EmptyExpr {
		return FlowSignal{
			kind: .normal
		}
	}
	_ = e.eval_expr(expr.else_expr)!
	return FlowSignal{
		kind: .normal
	}
}

fn (mut e Eval) eval_expr(expr ast.Expr) !Value {
	match expr {
		ast.ArrayInitExpr {
			mut elem_type_name := if expr.typ is ast.EmptyExpr {
				''
			} else {
				e.array_elem_type_name(expr.typ)
			}
			if expr.len !is ast.EmptyExpr && expr.exprs.len == 0 {
				size := int(e.value_as_int(e.eval_expr(expr.len)!)!)
				init_value := if expr.init is ast.EmptyExpr {
					void_value()
				} else {
					e.eval_expr(expr.init)!
				}
				mut values := []Value{len: size, init: init_value}
				if elem_type_name == '' {
					elem_type_name = e.infer_array_elem_type(values)
				}
				return ArrayValue{
					elem_type_name: elem_type_name
					values:         values
				}
			}
			mut values := []Value{cap: expr.exprs.len}
			for item in expr.exprs {
				values << e.eval_expr(item)!
			}
			if elem_type_name == '' {
				elem_type_name = e.infer_array_elem_type(values)
			}
			return ArrayValue{
				elem_type_name: elem_type_name
				values:         values
			}
		}
		ast.InitExpr {
			mut fields := map[string]Value{}
			mut data_fields := map[string]Value{}
			type_name := expr.typ.name()
			for field in expr.fields {
				mut value := e.eval_expr(field.value)!
				if field.name != '' && !field.name.starts_with('_data.') {
					if field_type := e.lookup_struct_field_type(type_name, field.name) {
						value = e.adapt_value_to_type(value, field_type)
					}
				}
				if field.name.starts_with('_data.') {
					data_fields[field.name.all_after('_data.')] = value
					continue
				}
				fields[field.name] = value
			}
			if data_fields.len > 0 {
				mut data_struct := StructValue{
					type_name: type_name + '._data'
					fields:    map[string]Value{}
				}
				if existing_data := fields['_data'] {
					if existing_data is StructValue {
						data_struct = existing_data
					}
				}
				mut merged_data_fields := data_struct.fields.clone()
				for name, value in data_fields {
					merged_data_fields[name] = value
				}
				fields['_data'] = StructValue{
					type_name: data_struct.type_name
					fields:    merged_data_fields
				}
			}
			return StructValue{
				type_name: type_name
				fields:    fields
			}
		}
		ast.AsCastExpr {
			return e.eval_as_cast_expr(expr)
		}
		ast.BasicLiteral {
			return e.eval_basic_literal(expr)
		}
		ast.CallExpr {
			return e.eval_call_expr(expr)
		}
		ast.CallOrCastExpr {
			if e.is_type_expr(expr.lhs) {
				value := e.eval_expr(expr.expr)!
				target_name := expr.lhs.name()
				if e.is_sum_type_name(target_name) {
					return e.wrap_sumtype_value(target_name, value, expr.expr) or {
						e.cast_value(value, target_name)!
					}
				}
				return e.cast_value(value, target_name)
			}
			return e.eval_call_expr(ast.CallExpr{
				lhs:  expr.lhs
				args: [expr.expr]
				pos:  expr.pos
			})
		}
		ast.CastExpr {
			value := e.eval_expr(expr.expr)!
			target_name := expr.typ.name()
			if e.is_sum_type_name(target_name) {
				return e.wrap_sumtype_value(target_name, value, expr.expr) or {
					e.cast_value(value, target_name)!
				}
			}
			return e.cast_value(value, target_name)
		}
		ast.ComptimeExpr {
			return e.eval_expr(expr.expr)
		}
		ast.GenericArgs {
			if e.is_type_expr(expr) {
				return TypeValue{
					name: generic_args_name(expr)
				}
			}
			return error('v2.eval: unsupported generic expression `${generic_args_name(expr)}`')
		}
		ast.Ident {
			return e.eval_ident(expr.name)
		}
		ast.IfExpr {
			return e.eval_if_value(expr)
		}
		ast.IndexExpr {
			return e.eval_index_expr(expr)
		}
		ast.InfixExpr {
			return e.eval_infix_expr(expr)
		}
		ast.KeywordOperator {
			return e.eval_keyword_operator(expr)
		}
		ast.MapInitExpr {
			mut entries := []MapEntry{cap: expr.keys.len}
			for i, key_expr in expr.keys {
				key := e.eval_expr(key_expr)!
				val := if i < expr.vals.len {
					e.eval_expr(expr.vals[i])!
				} else {
					void_value()
				}
				entries << MapEntry{
					key:   key
					value: val
				}
			}
			default_value := if expr.typ is ast.Type && expr.typ is ast.MapType {
				e.zero_value_for_type_expr((expr.typ as ast.MapType).value_type)
			} else if entries.len > 0 {
				e.zero_value_like(entries[0].value)
			} else {
				void_value()
			}
			return MapValue{
				default_value: default_value
				entries:       entries
			}
		}
		ast.ModifierExpr {
			return e.eval_expr(expr.expr)
		}
		ast.ParenExpr {
			return e.eval_expr(expr.expr)
		}
		ast.PostfixExpr {
			return e.eval_postfix_expr(expr)
		}
		ast.PrefixExpr {
			return e.eval_prefix_expr(expr)
		}
		ast.RangeExpr {
			return RangeValue{
				start:     e.value_as_int(e.eval_expr(expr.start)!)!
				end:       e.value_as_int(e.eval_expr(expr.end)!)!
				inclusive: expr.op == .ellipsis
			}
		}
		ast.SelectorExpr {
			return e.eval_selector_expr(expr)
		}
		ast.StringInterLiteral {
			return e.eval_string_inter(expr)
		}
		ast.StringLiteral {
			return decode_string_literal(expr.value)
		}
		ast.Tuple {
			mut values := []Value{cap: expr.exprs.len}
			for item in expr.exprs {
				values << e.eval_expr(item)!
			}
			return TupleValue{
				values: values
			}
		}
		ast.Type {
			return TypeValue{
				name: e.type_node_name(expr)
			}
		}
		ast.UnsafeExpr {
			return e.eval_unsafe_expr(expr)
		}
		ast.EmptyExpr {
			return void_value()
		}
		else {
			return error('v2.eval: unsupported expression `${expr.type_name()}`')
		}
	}
}

fn (mut e Eval) eval_as_cast_expr(expr ast.AsCastExpr) !Value {
	value := e.eval_expr(expr.expr)!
	target_name := e.type_expr_name(expr.typ)
	if value is StructValue {
		if payload := e.unwrap_sumtype_value(value, target_name) {
			return payload
		}
		if e.value_matches_type_name(value, target_name) {
			return value
		}
		return error('v2.eval: `${value.type_name}` is not `${target_name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
	}
	if e.value_matches_type_name(value, target_name) {
		return e.cast_value(value, target_name)
	}
	return error('v2.eval: `${e.runtime_type_name(value)}` is not `${target_name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
}

fn (mut e Eval) eval_basic_literal(expr ast.BasicLiteral) !Value {
	if expr.kind == .key_true {
		v := Value(true)
		return v
	} else if expr.kind == .key_false {
		v := Value(false)
		return v
	} else if expr.kind == .number {
		if expr.value.contains('.') || expr.value.contains('e') || expr.value.contains('E') {
			f := strconv.atof64(expr.value)!
			v := Value(f)
			return v
		} else {
			n := strconv.parse_int(expr.value, 0, 64)!
			v := Value(n)
			return v
		}
	} else if expr.kind == .char {
		if expr.value.len == 0 {
			v := Value(i64(0))
			return v
		} else {
			decoded := decode_string_literal(expr.value)
			if decoded.len == 0 {
				v := Value(i64(0))
				return v
			}
			v := Value(i64(decoded.runes()[0]))
			return v
		}
	} else {
		return error('v2.eval: unsupported literal `${expr.kind}`')
	}
}

fn (mut e Eval) eval_ident(name string) !Value {
	value_result := e.lookup_var(name)
	if value_result.found {
		return value_result.value
	}
	module_name := e.resolve_module_name(name)
	if module_name != '' {
		return ModuleValue{
			name: module_name
		}
	}
	if name == 'nil' || name == 'none' {
		return void_value()
	}
	if e.is_builtin_type_name(name) {
		return TypeValue{
			name: name
		}
	}
	cur_module := e.current_module_name()
	if e.has_type_name(cur_module, name) {
		return TypeValue{
			name: '${cur_module}.${name}'
		}
	}
	if type_value := e.resolve_mangled_type_value(name) {
		return type_value
	}
	const_result := e.lookup_const(cur_module, name)
	if const_result.found {
		return const_result.value
	}
	builtin_const := e.lookup_const('builtin', name)
	if builtin_const.found {
		return builtin_const.value
	}
	if mangled_const := e.resolve_mangled_const(name) {
		return mangled_const
	}
	return error('v2.eval: unknown identifier `${name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
}

fn (e &Eval) resolve_module_name(name string) string {
	file_name := e.current_file_name()
	if file_name != '' {
		if aliases := e.file_import_alias[file_name] {
			if name in aliases {
				return aliases[name]
			}
		}
	}
	if name in e.modules {
		return name
	}
	return ''
}

fn (e &Eval) has_type_name(module_name string, type_name string) bool {
	if module_name !in e.type_names {
		return false
	}
	return type_name in e.type_names[module_name]
}

fn (mut e Eval) resolve_mangled_const(name string) ?Value {
	for module_name in e.consts.keys() {
		prefix := e.mangled_module_name(module_name) + '__'
		if !name.starts_with(prefix) {
			continue
		}
		key := name[prefix.len..].all_after_last('__')
		const_result := e.lookup_const(module_name, key)
		if const_result.found {
			return const_result.value
		}
	}
	if name.contains('__') {
		type_name := name.all_before_last('__')
		field_name := name.all_after_last('__')
		for module_name, module_types in e.type_names {
			if type_name !in module_types {
				continue
			}
			const_result := e.lookup_const(module_name, field_name)
			if const_result.found {
				return const_result.value
			}
		}
	}
	return none
}

fn (e &Eval) resolve_mangled_type_value(name string) ?TypeValue {
	for module_name, module_types in e.type_names {
		prefix := e.mangled_module_name(module_name) + '__'
		if !name.starts_with(prefix) {
			continue
		}
		type_name := name[prefix.len..]
		if type_name in module_types {
			return TypeValue{
				name: '${module_name}.${type_name}'
			}
		}
	}
	return none
}

fn (e &Eval) mangled_module_name(name string) string {
	if name == '' {
		return ''
	}
	return name.replace('.', '__')
}

fn (mut e Eval) lookup_const(module_name string, name string) MaybeValue {
	if module_name !in e.consts {
		return MaybeValue{}
	}
	mut entry := e.consts[module_name][name] or { return MaybeValue{} }
	if entry.cached {
		return MaybeValue{
			found: true
			value: entry.value
		}
	}
	if entry.evaluating {
		return MaybeValue{}
	}
	entry.evaluating = true
	e.consts[module_name][name] = entry
	e.call_stack << CallFrame{
		module_name: module_name
		file_name:   entry.file_name
		fn_name:     ''
	}
	value := e.eval_expr(entry.expr) or {
		entry.evaluating = false
		e.consts[module_name][name] = entry
		e.call_stack.pop()
		return MaybeValue{}
	}
	e.call_stack.pop()
	entry.cached = true
	entry.evaluating = false
	entry.value = value
	e.consts[module_name][name] = entry
	return MaybeValue{
		found: true
		value: value
	}
}

fn (mut e Eval) set_runtime_const(module_name string, name string, value Value) bool {
	if module_name !in e.consts {
		return false
	}
	mut entry := e.consts[module_name][name] or { return false }
	entry.cached = true
	entry.evaluating = false
	entry.value = value
	e.consts[module_name][name] = entry
	return true
}

fn (mut e Eval) eval_call_expr(expr ast.CallExpr) !Value {
	if expr.lhs is ast.SelectorExpr {
		selector := expr.lhs as ast.SelectorExpr
		if builder_result := e.maybe_call_builder_wrapper(selector.rhs.name, expr.args) {
			return builder_result
		}
		if selector.lhs is ast.SelectorExpr {
			inner := selector.lhs as ast.SelectorExpr
			if inner.rhs.name == 'flags' {
				_ = e.eval_expr(inner.lhs)!
				for arg in expr.args {
					_ = e.eval_expr(arg)!
				}
				return match selector.rhs.name {
					'set', 'clear' { void_value() }
					'has' { Value(false) }
					else { return error('v2.eval: unsupported array flags method `${selector.rhs.name}`') }
				}
			}
		}
		if selector.lhs is ast.Ident {
			module_name := e.resolve_module_name(selector.lhs.name)
			if module_name != '' {
				mut args := []Value{cap: expr.args.len}
				for arg in expr.args {
					args << e.eval_expr(arg)!
				}
				result := e.call_function(module_name, selector.rhs.name, args)!
				e.writeback_mut_args(expr.args, result)!
				return call_result_value(result)
			}
		}
		receiver := e.eval_expr(selector.lhs)!
		mut args := []Value{cap: expr.args.len}
		for arg in expr.args {
			args << e.eval_expr(arg)!
		}
		return e.call_value_method(receiver, selector.rhs.name, args)
	}
	if expr.lhs is ast.Ident {
		if builder_result := e.maybe_call_builder_wrapper(expr.lhs.name, expr.args) {
			return builder_result
		}
		mut args := []Value{cap: expr.args.len}
		for arg in expr.args {
			args << e.eval_expr(arg)!
		}
		if target := e.resolve_mangled_function_target(expr.lhs.name) {
			result := e.call_function(target.module_name, target.fn_name, args)!
			e.writeback_mut_args(expr.args, result)!
			return call_result_value(result)
		}
		result := e.call_function(e.current_module_name(), expr.lhs.name, args) or {
			if result := e.call_function('builtin', expr.lhs.name, args) {
				e.writeback_mut_args(expr.args, result)!
				return call_result_value(result)
			}
			return error(err.msg())
		}
		e.writeback_mut_args(expr.args, result)!
		return call_result_value(result)
	}
	return error('v2.eval: unsupported call target `${expr.lhs.type_name()}`')
}

fn (mut e Eval) maybe_call_builder_wrapper(name string, args []ast.Expr) ?Value {
	if !name.contains('Builder__') || args.len == 0 {
		return none
	}
	method_name := name.all_after('Builder__')
	target_name := builder_target_name(args[0]) or { return none }
	target_result := e.lookup_var(target_name)
	if !target_result.found || target_result.value !is ArrayValue {
		return none
	}
	mut receiver := target_result.value as ArrayValue
	match method_name {
		'cut_last' {
			if args.len < 2 {
				return none
			}
			count_value := e.eval_expr(args[1]) or { return none }
			n := int(e.value_as_int(count_value) or { return none })
			mut items := receiver.values.clone()
			if n <= 0 || n > items.len {
				return ''
			}
			cut := items[items.len - n..].clone()
			items = items[..items.len - n].clone()
			e.set_var(target_name, ArrayValue{
				values: items
			}) or { return none }
			return e.builder_array_to_string(ArrayValue{
				values: cut
			})
		}
		'go_back' {
			if args.len < 2 {
				return none
			}
			count_value := e.eval_expr(args[1]) or { return none }
			n := int(e.value_as_int(count_value) or { return none })
			if n <= 0 {
				return void_value()
			}
			keep_len := if n > receiver.values.len { 0 } else { receiver.values.len - n }
			e.set_var(target_name, ArrayValue{
				values: receiver.values[..keep_len].clone()
			}) or { return none }
			return void_value()
		}
		'go_back_to' {
			if args.len < 2 {
				return none
			}
			pos_value := e.eval_expr(args[1]) or { return none }
			pos := int(e.value_as_int(pos_value) or { return none })
			keep_len := if pos < 0 {
				0
			} else if pos > receiver.values.len {
				receiver.values.len
			} else {
				pos
			}
			e.set_var(target_name, ArrayValue{
				values: receiver.values[..keep_len].clone()
			}) or { return none }
			return void_value()
		}
		'last_n' {
			if args.len < 2 {
				return none
			}
			count_value := e.eval_expr(args[1]) or { return none }
			n := int(e.value_as_int(count_value) or { return none })
			return e.builder_tail_string(receiver, n)
		}
		'after' {
			if args.len < 2 {
				return none
			}
			start_value := e.eval_expr(args[1]) or { return none }
			start := int(e.value_as_int(start_value) or { return none })
			return e.builder_slice_string(receiver, start, receiver.values.len - start)
		}
		'spart' {
			if args.len < 3 {
				return none
			}
			start_value := e.eval_expr(args[1]) or { return none }
			len_value := e.eval_expr(args[2]) or { return none }
			start := int(e.value_as_int(start_value) or { return none })
			n := int(e.value_as_int(len_value) or { return none })
			return e.builder_slice_string(receiver, start, n)
		}
		'str' {
			result := e.builder_array_to_string(receiver)
			e.set_var(target_name, ArrayValue{
				values: []Value{}
			}) or { return none }
			return result
		}
		'write_ptr' {
			return void_value()
		}
		'write_byte', 'write_rune', 'write_repeated_rune', 'write_string', 'write_string2',
		'write_u8', 'writeln', 'writeln2' {
			mut items := receiver.values.clone()
			match method_name {
				'write_string' {
					if args.len < 2 {
						return none
					}
					value := e.eval_expr(args[1]) or { return none }
					e.append_builder_string(mut items, e.value_string(value))
				}
				'write_string2' {
					if args.len < 3 {
						return none
					}
					first_value := e.eval_expr(args[1]) or { return none }
					second_value := e.eval_expr(args[2]) or { return none }
					e.append_builder_string(mut items, e.value_string(first_value))
					e.append_builder_string(mut items, e.value_string(second_value))
				}
				'write_rune' {
					if args.len < 2 {
						return none
					}
					rune_value := e.eval_expr(args[1]) or { return none }
					e.append_builder_rune(mut items, e.value_as_int(rune_value) or { return none })
				}
				'write_repeated_rune' {
					if args.len < 3 {
						return none
					}
					rune_value := e.eval_expr(args[1]) or { return none }
					count_value := e.eval_expr(args[2]) or { return none }
					r := e.value_as_int(rune_value) or { return none }
					count := int(e.value_as_int(count_value) or { return none })
					for _ in 0 .. count {
						e.append_builder_rune(mut items, r)
					}
				}
				'write_byte', 'write_u8' {
					if args.len < 2 {
						return none
					}
					byte_value := e.eval_expr(args[1]) or { return none }
					items << (e.value_as_int(byte_value) or { return none })
				}
				'writeln' {
					if args.len < 2 {
						return none
					}
					value := e.eval_expr(args[1]) or { return none }
					e.append_builder_string(mut items, e.value_string(value))
					items << i64(`\n`)
				}
				'writeln2' {
					if args.len < 3 {
						return none
					}
					first_value := e.eval_expr(args[1]) or { return none }
					second_value := e.eval_expr(args[2]) or { return none }
					e.append_builder_string(mut items, e.value_string(first_value))
					items << i64(`\n`)
					e.append_builder_string(mut items, e.value_string(second_value))
					items << i64(`\n`)
				}
				else {}
			}
			e.set_var(target_name, ArrayValue{
				values: items
			}) or { return none }
			return void_value()
		}
		else {
			return none
		}
	}
}

fn builder_target_name(expr ast.Expr) ?string {
	match expr {
		ast.CastExpr {
			return builder_target_name(expr.expr)
		}
		ast.Ident {
			return expr.name
		}
		ast.ModifierExpr {
			return builder_target_name(expr.expr)
		}
		ast.ParenExpr {
			return builder_target_name(expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return builder_target_name(expr.expr)
			}
		}
		else {}
	}
	return none
}

fn (e &Eval) append_builder_string(mut items []Value, s string) {
	for b in s.bytes() {
		items << i64(b)
	}
}

fn (e &Eval) append_builder_rune(mut items []Value, r i64) {
	e.append_builder_string(mut items, rune(r).str())
}

fn (e &Eval) builder_array_to_string(receiver ArrayValue) string {
	mut bytes := []u8{cap: receiver.values.len}
	for item in receiver.values {
		bytes << u8(e.value_as_int(item) or { 0 })
	}
	return bytes.bytestr()
}

fn (e &Eval) builder_slice_string(receiver ArrayValue, start_pos int, n int) string {
	if start_pos < 0 || n <= 0 || start_pos >= receiver.values.len {
		return ''
	}
	end := if start_pos + n > receiver.values.len {
		receiver.values.len
	} else {
		start_pos + n
	}
	return e.builder_array_to_string(ArrayValue{
		values: receiver.values[start_pos..end].clone()
	})
}

fn (e &Eval) builder_tail_string(receiver ArrayValue, n int) string {
	if n <= 0 || n > receiver.values.len {
		return ''
	}
	return e.builder_array_to_string(ArrayValue{
		values: receiver.values[receiver.values.len - n..].clone()
	})
}

fn (mut e Eval) eval_if_value(expr ast.IfExpr) !Value {
	should_run := if expr.cond is ast.EmptyExpr {
		true
	} else {
		e.value_as_bool(e.eval_expr(expr.cond)!)!
	}
	if should_run {
		return e.eval_block_value(expr.stmts)
	}
	if expr.else_expr is ast.IfExpr {
		return e.eval_if_value(expr.else_expr)
	}
	if expr.else_expr is ast.EmptyExpr {
		return void_value()
	}
	return e.eval_expr(expr.else_expr)
}

fn (e &Eval) resolve_mangled_function_target(name string) ?MaybeFunctionTarget {
	for module_name, functions in e.functions {
		prefix := e.mangled_module_name(module_name) + '__'
		if !name.starts_with(prefix) {
			continue
		}
		fn_name := name[prefix.len..]
		if fn_name in functions {
			return MaybeFunctionTarget{
				found:       true
				module_name: module_name
				fn_name:     fn_name
			}
		}
	}
	return none
}

fn (mut e Eval) eval_block_value(stmts []ast.Stmt) !Value {
	e.open_scope()
	defer {
		e.close_scope() or {}
	}
	if stmts.len == 0 {
		return void_value()
	}
	for i, stmt in stmts {
		is_last := i == stmts.len - 1
		if is_last && stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				return e.eval_if_value(stmt.expr)
			}
			return e.eval_expr(stmt.expr)
		}
		signal := e.exec_stmt(stmt)!
		if signal.kind != .normal {
			return error('v2.eval: control flow inside expression block is not supported yet')
		}
	}
	return void_value()
}

fn (mut e Eval) eval_index_expr(expr ast.IndexExpr) !Value {
	container := e.eval_expr(expr.lhs)!
	if expr.expr is ast.RangeExpr {
		range_value := e.eval_expr(expr.expr)! as RangeValue
		start := int(range_value.start)
		end := int(range_value.end) + if range_value.inclusive { 1 } else { 0 }
		match container {
			ArrayValue {
				return ArrayValue{
					values: container.values[start..end]
				}
			}
			string {
				s := container[start..end]
				return s
			}
			else {
				return error('v2.eval: slicing is only supported for arrays and strings')
			}
		}
	}
	index_value := e.eval_expr(expr.expr)!
	match container {
		ArrayValue {
			index := int(e.value_as_int(index_value)!)
			if index < 0 || index >= container.values.len {
				return error('v2.eval: array index out of bounds')
			}
			return container.values[index]
		}
		MapValue {
			return e.map_get_or_default(container, index_value)
		}
		string {
			index := int(e.value_as_int(index_value)!)
			if index < 0 || index >= container.len {
				return error('v2.eval: string index out of bounds')
			}
			return i64(container[index])
		}
		else {
			return error('v2.eval: unsupported index target `${e.runtime_type_name(container)}`')
		}
	}
}

fn (mut e Eval) eval_infix_expr(expr ast.InfixExpr) !Value {
	if expr.op == .and {
		left := e.value_as_bool(e.eval_expr(expr.lhs)!)!
		if !left {
			return Value(false)
		}
		return e.value_as_bool(e.eval_expr(expr.rhs)!)!
	}
	if expr.op == .logical_or {
		left := e.value_as_bool(e.eval_expr(expr.lhs)!)!
		if left {
			return Value(true)
		}
		return e.value_as_bool(e.eval_expr(expr.rhs)!)!
	}
	left := e.eval_expr(expr.lhs)!
	right := e.eval_expr(expr.rhs)!
	return e.apply_infix(expr.op, left, right)
}

fn (mut e Eval) apply_infix(op token.Token, left Value, right Value) !Value {
	match op {
		.plus {
			if left is string || right is string {
				return e.value_string(left) + e.value_string(right)
			}
			if left is f64 || right is f64 {
				return e.value_as_f64(left)! + e.value_as_f64(right)!
			}
			return e.value_as_int(left)! + e.value_as_int(right)!
		}
		.minus {
			if left is f64 || right is f64 {
				return e.value_as_f64(left)! - e.value_as_f64(right)!
			}
			return e.value_as_int(left)! - e.value_as_int(right)!
		}
		.mul {
			if left is f64 || right is f64 {
				return e.value_as_f64(left)! * e.value_as_f64(right)!
			}
			return e.value_as_int(left)! * e.value_as_int(right)!
		}
		.div {
			if left is f64 || right is f64 {
				return e.value_as_f64(left)! / e.value_as_f64(right)!
			}
			return e.value_as_int(left)! / e.value_as_int(right)!
		}
		.mod {
			return e.value_as_int(left)! % e.value_as_int(right)!
		}
		.eq {
			return e.value_eq(left, right)
		}
		.ne {
			return !e.value_eq(left, right)
		}
		.lt {
			return e.compare_values(left, right) < 0
		}
		.le {
			return e.compare_values(left, right) <= 0
		}
		.gt {
			return e.compare_values(left, right) > 0
		}
		.ge {
			return e.compare_values(left, right) >= 0
		}
		.key_is {
			return e.value_is_type(left, right)
		}
		.not_is {
			return !e.value_is_type(left, right)
		}
		.amp {
			return e.value_as_int(left)! & e.value_as_int(right)!
		}
		.pipe {
			return e.value_as_int(left)! | e.value_as_int(right)!
		}
		.xor {
			return e.value_as_int(left)! ^ e.value_as_int(right)!
		}
		.left_shift {
			return i64(u64(e.value_as_int(left)!) << u64(e.value_as_int(right)!))
		}
		.right_shift {
			return e.value_as_int(left)! >> e.value_as_int(right)!
		}
		.right_shift_unsigned {
			return i64(u64(e.value_as_int(left)!) >> u64(e.value_as_int(right)!))
		}
		.key_in {
			match right {
				ArrayValue {
					for item in right.values {
						if e.value_eq(left, item) {
							return true
						}
					}
					return false
				}
				string {
					return right.contains(e.value_string(left))
				}
				MapValue {
					return e.map_contains_key(right, left)
				}
				else {
					return error('v2.eval: unsupported `in` rhs `${e.runtime_type_name(right)}`')
				}
			}
		}
		.not_in {
			in_result := e.apply_infix(.key_in, left, right)!
			return !e.value_as_bool(in_result)!
		}
		else {
			return error('v2.eval: unsupported infix operator `${op}`')
		}
	}
}

fn (e &Eval) value_is_type(value Value, rhs Value) bool {
	target_name := match rhs {
		TypeValue { rhs.name }
		string { rhs }
		else { e.runtime_type_name(rhs) }
	}
	if value is StructValue && e.unwrap_sumtype_value(value, target_name) != none {
		return true
	}
	return e.value_matches_type_name(value, target_name)
}

fn (e &Eval) compare_values(left Value, right Value) int {
	if left is string || right is string {
		ls := e.value_string(left)
		rs := e.value_string(right)
		if ls < rs {
			return -1
		}
		if ls > rs {
			return 1
		}
		return 0
	}
	if left is f64 || right is f64 {
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
	li := e.value_as_int(left) or { 0 }
	ri := e.value_as_int(right) or { 0 }
	if li < ri {
		return -1
	}
	if li > ri {
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
			if right is i64 {
				return left == right
			}
			if right is f64 {
				return f64(left) == right
			}
			return false
		}
		f64 {
			if right is f64 {
				return left == right
			}
			if right is i64 {
				return left == f64(right)
			}
			return false
		}
		string {
			return right is string && left == right
		}
		ArrayValue {
			if right !is ArrayValue {
				return false
			}
			right_array := right as ArrayValue
			if left.values.len != right_array.values.len {
				return false
			}
			for i, item in left.values {
				if !e.value_eq(item, right_array.values[i]) {
					return false
				}
			}
			return true
		}
		MapValue {
			if right !is MapValue {
				return false
			}
			right_map := right as MapValue
			if left.entries.len != right_map.entries.len {
				return false
			}
			for entry in left.entries {
				right_value, found := e.map_lookup(right_map, entry.key)
				if !found || !e.value_eq(entry.value, right_value) {
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
			for key, value in left.fields {
				right_value := right_struct.fields[key] or { return false }
				if !e.value_eq(value, right_value) {
					return false
				}
			}
			return true
		}
		FlagsValue {
			return right is FlagsValue
		}
		ModuleValue {
			return right is ModuleValue && left.name == right.name
		}
		TypeValue {
			return right is TypeValue && left.name == right.name
		}
		VoidValue {
			return right is VoidValue
		}
		TupleValue {
			if right !is TupleValue {
				return false
			}
			right_tuple := right as TupleValue
			if left.values.len != right_tuple.values.len {
				return false
			}
			for i, item in left.values {
				if !e.value_eq(item, right_tuple.values[i]) {
					return false
				}
			}
			return true
		}
		RangeValue {
			return right is RangeValue && left.start == right.start && left.end == right.end
				&& left.inclusive == right.inclusive
		}
	}
}

fn (mut e Eval) eval_keyword_operator(expr ast.KeywordOperator) !Value {
	match expr.op {
		.key_isreftype {
			if expr.exprs.len == 0 {
				return error('v2.eval: isreftype expects one argument')
			}
			if e.is_type_expr(expr.exprs[0]) {
				return e.is_ref_type_name(e.type_expr_name(expr.exprs[0]))
			}
			value := e.eval_expr(expr.exprs[0])!
			return e.is_ref_type_name(e.runtime_type_name(value))
		}
		.key_sizeof {
			if expr.exprs.len == 0 {
				return error('v2.eval: sizeof expects one argument')
			}
			if e.is_type_expr(expr.exprs[0]) {
				return e.sizeof_type_name(e.type_expr_name(expr.exprs[0]))
			}
			value := e.eval_expr(expr.exprs[0])!
			return e.sizeof_type_name(e.runtime_type_name(value))
		}
		.key_typeof {
			if expr.exprs.len == 0 {
				return error('v2.eval: typeof expects one argument')
			}
			value := e.eval_expr(expr.exprs[0])!
			return TypeValue{
				name: e.runtime_type_name(value)
			}
		}
		else {
			return error('v2.eval: unsupported keyword operator `${expr.op}`')
		}
	}
}

fn (mut e Eval) eval_postfix_expr(expr ast.PostfixExpr) !Value {
	if !can_update_target(expr.expr) {
		return error('v2.eval: postfix update expects an assignable target')
	}
	current := e.eval_expr(expr.expr)!
	current_i64 := e.value_as_int(current)!
	updated := if expr.op == .inc { current_i64 + 1 } else { current_i64 - 1 }
	e.update_target(expr.expr, updated)!
	return updated
}

fn (mut e Eval) eval_prefix_expr(expr ast.PrefixExpr) !Value {
	value := e.eval_expr(expr.expr)!
	match expr.op {
		.not {
			return !e.value_as_bool(value)!
		}
		.amp, .mul {
			return value
		}
		.minus {
			if value is f64 {
				f := -value
				return f
			}
			return -e.value_as_int(value)!
		}
		.plus {
			return value
		}
		else {
			return error('v2.eval: unsupported prefix operator `${expr.op}`')
		}
	}
}

fn (mut e Eval) eval_selector_expr(expr ast.SelectorExpr) !Value {
	if expr.lhs is ast.EmptyExpr {
		const_result := e.lookup_const(e.current_module_name(), expr.rhs.name)
		if const_result.found {
			return const_result.value
		}
		return error('v2.eval: enum shorthand `${expr.rhs.name}` is not supported yet')
	}
	left := e.eval_expr(expr.lhs)!
	match left {
		ModuleValue {
			prop := e.module_property(left.name, expr.rhs.name)
			if prop.found {
				return prop.value
			}
			if e.has_type_name(left.name, expr.rhs.name) {
				return TypeValue{
					name: '${left.name}.${expr.rhs.name}'
				}
			}
			if expr.rhs.name.contains('__') {
				short_name := expr.rhs.name.all_after_last('__')
				if short_name != '' {
					if e.has_type_name(left.name, short_name) {
						return TypeValue{
							name: '${left.name}.${short_name}'
						}
					}
					const_result := e.lookup_const(left.name, short_name)
					if const_result.found {
						return const_result.value
					}
				}
				module_prefix := e.mangled_module_name(left.name) + '__'
				if expr.rhs.name.starts_with(module_prefix) {
					normalized_name := expr.rhs.name[module_prefix.len..]
					if e.has_type_name(left.name, normalized_name) {
						return TypeValue{
							name: '${left.name}.${normalized_name}'
						}
					}
				}
			}
			const_result := e.lookup_const(left.name, expr.rhs.name)
			if const_result.found {
				return const_result.value
			}
			return error('v2.eval: unknown module selector `${left.name}.${expr.rhs.name}`')
		}
		ArrayValue {
			if expr.rhs.name == 'len' {
				return i64(left.values.len)
			}
			if expr.rhs.name == 'flags' {
				return FlagsValue{}
			}
			if expr.rhs.name == 'str' {
				return e.value_string(left)
			}
		}
		MapValue {
			if expr.rhs.name == 'len' {
				return i64(left.entries.len)
			}
			if expr.rhs.name == 'str' {
				return e.value_string(left)
			}
		}
		StructValue {
			if expr.rhs.name == 'mtx' {
				return StructValue{
					type_name: 'sync__RwMutex'
					fields:    map[string]Value{}
				}
			}
			if expr.rhs.name == '_tag' {
				if tag := e.inferred_sumtype_tag(left) {
					return i64(tag)
				}
			}
			if expr.rhs.name == '_data' {
				if _ := e.inferred_sumtype_tag(left) {
					return left
				}
			}
			if expr.rhs.name in left.fields {
				return left.fields[expr.rhs.name] or {
					return error('v2.eval: unknown struct field `${expr.rhs.name}` on `${left.type_name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
				}
			}
			if field_type := e.lookup_struct_field_type(left.type_name, expr.rhs.name) {
				return e.zero_value_for_type_expr(field_type)
			}
			return error('v2.eval: unknown struct field `${expr.rhs.name}` on `${left.type_name}` in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
		}
		string {
			if expr.rhs.name == 'len' {
				return i64(left.len)
			}
			if expr.rhs.name == 'str' {
				return left
			}
		}
		i64 {
			if expr.rhs.name == 'str' {
				return e.value_string(left)
			}
			if expr.rhs.name == '_tag' || expr.rhs.name == '_data' {
				return Value(left)
			}
		}
		f64 {
			if expr.rhs.name == 'str' {
				return e.value_string(left)
			}
		}
		bool {
			if expr.rhs.name == 'str' {
				return e.value_string(left)
			}
		}
		TypeValue {
			if expr.rhs.name == '_tag' {
				if tag := e.inferred_sumtype_tag(left) {
					return i64(tag)
				}
			}
			if expr.rhs.name == '_data' {
				if _ := e.inferred_sumtype_tag(left) {
					return left
				}
			}
			module_name := e.type_value_module_name(left)
			if module_name != '' {
				const_result := e.lookup_const(module_name, expr.rhs.name)
				if const_result.found {
					return const_result.value
				}
			}
			if expr.rhs.name == 'name' {
				s := left.name
				v := Value(s)
				return v
			}
		}
		else {}
	}
	return error('v2.eval: unsupported selector `${expr.rhs.name}` on `${e.runtime_type_name(left)}` in `${e.current_function_label()}`')
}

fn (e &Eval) type_value_module_name(value TypeValue) string {
	if !value.name.contains('.') {
		return ''
	}
	return value.name.all_before_last('.')
}

fn struct_field_lookup_candidates(type_name string) []string {
	mut candidates := []string{}
	mut trimmed := type_name.trim_left('&').trim_right('*')
	if trimmed == '' {
		return candidates
	}
	add_type_name_alias(mut candidates, trimmed)
	if trimmed.contains('__') {
		add_type_name_alias(mut candidates, trimmed.replace('__', '.'))
		add_type_name_alias(mut candidates, trimmed.all_after_last('__'))
	}
	if trimmed.contains('.') {
		add_type_name_alias(mut candidates, trimmed.all_after_last('.'))
	}
	return candidates
}

fn (e &Eval) lookup_struct_field_type(type_name string, field_name string) ?ast.Expr {
	for candidate in struct_field_lookup_candidates(type_name) {
		mut module_name := ''
		mut short_type_name := candidate
		if candidate.contains('.') {
			module_name = candidate.all_before_last('.')
			short_type_name = candidate.all_after_last('.')
		}
		if module_name != '' {
			if module_fields := e.struct_field_types[module_name] {
				if type_fields := module_fields[short_type_name] {
					if field_name in type_fields {
						return type_fields[field_name] or { return none }
					}
				}
			}
			continue
		}
		current_module := e.current_module_name()
		if module_fields := e.struct_field_types[current_module] {
			if type_fields := module_fields[short_type_name] {
				if field_name in type_fields {
					return type_fields[field_name] or { return none }
				}
			}
		}
		for module_fields in e.struct_field_types.values() {
			if type_fields := module_fields[short_type_name] {
				if field_name in type_fields {
					return type_fields[field_name] or { return none }
				}
			}
		}
	}
	return none
}

fn (e &Eval) zero_struct_value(type_name string) StructValue {
	if info := e.sum_type_info(type_name) {
		return e.build_sumtype_wrapper(type_name, info, 0, void_value())
	}
	mut fields := map[string]Value{}
	mut module_name := ''
	mut short_type_name := type_name
	if type_name.contains('.') {
		module_name = type_name.all_before_last('.')
		short_type_name = type_name.all_after_last('.')
	}
	if module_name != '' {
		if module_fields := e.struct_field_types[module_name] {
			if type_fields := module_fields[short_type_name] {
				for field_name, field_type in type_fields {
					fields[field_name] = e.zero_value_for_type_expr(field_type)
				}
			}
		}
	} else if module_fields := e.struct_field_types[e.current_module_name()] {
		if type_fields := module_fields[short_type_name] {
			for field_name, field_type in type_fields {
				fields[field_name] = e.zero_value_for_type_expr(field_type)
			}
		}
	}
	return StructValue{
		type_name: type_name
		fields:    fields
	}
}

fn (mut e Eval) eval_string_inter(expr ast.StringInterLiteral) !Value {
	mut out := ''
	for i, part in expr.values {
		mut decoded := decode_string_literal(part)
		if part in ['"', "'"] && (i == 0 || i == expr.values.len - 1) {
			decoded = ''
		}
		out += decoded
		if i < expr.inters.len {
			inter := expr.inters[i]
			out += e.value_string(e.eval_expr(inter.expr)!)
		}
	}
	return out
}

fn decode_string_literal(raw string) string {
	mut s := raw
	if s.len >= 2 && ((s[0] == `"` && s[s.len - 1] == `"`) || (s[0] == `'` && s[s.len - 1] == `'`)) {
		s = s[1..s.len - 1]
	}
	return s.replace('\\n', '\n').replace('\\r', '\r').replace('\\t', '\t').replace('\\0',
		'\0').replace('\\"', '"').replace("\\'", "'").replace('\\`', '`').replace('\\\\',
		'\\').replace('\\$', '$')
}

fn (mut e Eval) eval_unsafe_expr(expr ast.UnsafeExpr) !Value {
	if expr.stmts.len == 0 {
		return void_value()
	}
	e.open_scope()
	defer {
		e.close_scope() or {}
	}
	for i, stmt in expr.stmts {
		is_last := i == expr.stmts.len - 1
		if is_last && stmt is ast.ExprStmt {
			return e.eval_expr(stmt.expr)
		}
		signal := e.exec_stmt(stmt)!
		if signal.kind != .normal {
			return error('v2.eval: control flow inside unsafe expression is not supported')
		}
	}
	return void_value()
}

fn (mut e Eval) call_value_method(receiver Value, method_name string, args []Value) !Value {
	if method_name == 'str' && args.len == 0 {
		return e.value_string(receiver)
	}
	match receiver {
		string {
			return e.call_string_method(receiver, method_name, args)
		}
		ArrayValue {
			return e.call_array_method(receiver, method_name, args)
		}
		FlagsValue {
			return e.call_flags_method(method_name, args)
		}
		MapValue {
			if method_name == 'str' && args.len == 0 {
				return e.value_string(receiver)
			}
		}
		i64 {
			if args.len == 0 && receiver >= 0 && receiver <= 255 {
				if byte_value := eval_byte_method_value(u8(receiver), method_name) {
					return byte_value
				}
			}
			if args.len == 0 && method_name != 'str' {
				// Token enum values are stored as ints in eval, so enum helpers need a narrow cast here.
				if token_value := eval_token_method_value(unsafe { token.Token(int(receiver)) },
					method_name)
				{
					return token_value
				}
			}
			if method_name == 'str' && args.len == 0 {
				return e.value_string(receiver)
			}
		}
		f64, bool {
			if method_name == 'str' && args.len == 0 {
				return e.value_string(receiver)
			}
		}
		else {}
	}
	return error('v2.eval: unsupported method `${method_name}` on `${e.runtime_type_name(receiver)}`')
}

fn (mut e Eval) call_string_method(receiver string, method_name string, args []Value) !Value {
	match method_name {
		'all_after' {
			return receiver.all_after(e.expect_string_arg(args, 0)!)
		}
		'all_after_last' {
			return receiver.all_after_last(e.expect_string_arg(args, 0)!)
		}
		'all_before' {
			return receiver.all_before(e.expect_string_arg(args, 0)!)
		}
		'all_before_last' {
			return receiver.all_before_last(e.expect_string_arg(args, 0)!)
		}
		'contains' {
			return receiver.contains(e.expect_string_arg(args, 0)!)
		}
		'bytes', 'vbytes' {
			mut values := []Value{cap: receiver.len}
			for b in receiver.bytes() {
				values << i64(b)
			}
			return ArrayValue{
				values: values
			}
		}
		'clone' {
			return receiver.clone()
		}
		'ends_with' {
			return receiver.ends_with(e.expect_string_arg(args, 0)!)
		}
		'ge' {
			return receiver >= e.expect_string_arg(args, 0)!
		}
		'gt' {
			return receiver > e.expect_string_arg(args, 0)!
		}
		'int' {
			return i64(receiver.int())
		}
		'index' {
			idx := receiver.index(e.expect_string_arg(args, 0)!) or { return wrap_option_none() }
			return wrap_option_ok(i64(idx))
		}
		'i64' {
			return receiver.i64()
		}
		'last_index' {
			idx := receiver.last_index(e.expect_string_arg(args, 0)!) or {
				return wrap_option_none()
			}
			return wrap_option_ok(i64(idx))
		}
		'le' {
			return receiver <= e.expect_string_arg(args, 0)!
		}
		'lt' {
			return receiver < e.expect_string_arg(args, 0)!
		}
		'ne' {
			return receiver != e.expect_string_arg(args, 0)!
		}
		'plus' {
			return receiver + e.expect_string_arg(args, 0)!
		}
		'plus_two' {
			return receiver + e.expect_string_arg(args, 0)! + e.expect_string_arg(args, 1)!
		}
		'repeat' {
			return receiver.repeat(int(e.value_as_int(args[0])!))
		}
		'replace' {
			return receiver.replace(e.expect_string_arg(args, 0)!, e.expect_string_arg(args,
				1)!)
		}
		'split' {
			return ArrayValue{
				values: receiver.split(e.expect_string_arg(args, 0)!).map(Value(it))
			}
		}
		'starts_with' {
			return receiver.starts_with(e.expect_string_arg(args, 0)!)
		}
		'substr', 'substr_unsafe' {
			start := int(e.value_as_int(args[0])!)
			end := int(e.value_as_int(args[1])!)
			if start < 0 || end < start || end > receiver.len {
				return ''
			}
			substr := receiver[start..end]
			return substr
		}
		'substr_with_check' {
			start := int(e.value_as_int(args[0])!)
			end := int(e.value_as_int(args[1])!)
			if start < 0 || end < start || end > receiver.len {
				return wrap_result_err('substring out of bounds')
			}
			substr := receiver[start..end]
			return wrap_result_ok(substr)
		}
		'to_lower' {
			return receiver.to_lower()
		}
		'to_upper' {
			return receiver.to_upper()
		}
		'trim' {
			return receiver.trim(e.expect_string_arg(args, 0)!)
		}
		'trim_right' {
			return receiver.trim_right(e.expect_string_arg(args, 0)!)
		}
		'trim_space' {
			return receiver.trim_space()
		}
		else {
			return error('v2.eval: unsupported string method `${method_name}`')
		}
	}
}

fn (e &Eval) call_flags_method(method_name string, args []Value) !Value {
	_ = args
	return match method_name {
		'set', 'clear' { void_value() }
		'has' { Value(false) }
		else { error('v2.eval: unsupported flags method `${method_name}`') }
	}
}

fn (mut e Eval) call_array_method(receiver ArrayValue, method_name string, args []Value) !Value {
	match method_name {
		'has' {
			return e.array_has(receiver, safe_arg(args, 0))
		}
		'contains' {
			return e.array_contains(receiver, safe_arg(args, 0))
		}
		'index' {
			return e.array_index(receiver, safe_arg(args, 0), false)
		}
		'join' {
			sep := e.expect_string_arg(args, 0)!
			return receiver.values.map(e.value_string(it)).join(sep)
		}
		'last_index' {
			return e.array_index(receiver, safe_arg(args, 0), true)
		}
		'str' {
			return e.value_string(receiver)
		}
		else {
			return error('v2.eval: unsupported array method `${method_name}`')
		}
	}
}

fn (e &Eval) map_lookup(receiver MapValue, key Value) (Value, bool) {
	for entry in receiver.entries {
		if e.value_eq(entry.key, key) {
			return entry.value, true
		}
	}
	return receiver.default_value, false
}

fn (e &Eval) map_get_or_default(receiver MapValue, key Value) Value {
	value, found := e.map_lookup(receiver, key)
	if found {
		return value
	}
	return receiver.default_value
}

fn (e &Eval) map_contains_key(receiver MapValue, key Value) bool {
	_, found := e.map_lookup(receiver, key)
	return found
}

fn (e &Eval) map_set_value(receiver MapValue, key Value, value Value) MapValue {
	mut entries := receiver.entries.clone()
	for i, entry in entries {
		if e.value_eq(entry.key, key) {
			entries[i] = MapEntry{
				key:   key
				value: value
			}
			return MapValue{
				default_value: receiver.default_value
				entries:       entries
			}
		}
	}
	entries << MapEntry{
		key:   key
		value: value
	}
	return MapValue{
		default_value: receiver.default_value
		entries:       entries
	}
}

fn (e &Eval) zero_value_like(value Value) Value {
	return match value {
		ArrayValue {
			ArrayValue{}
		}
		FlagsValue {
			FlagsValue{}
		}
		MapValue {
			MapValue{
				default_value: e.zero_value_like(value.default_value)
			}
		}
		StructValue {
			e.zero_struct_value(value.type_name)
		}
		TupleValue {
			TupleValue{}
		}
		VoidValue {
			void_value()
		}
		bool {
			false
		}
		f64 {
			f64(0.0)
		}
		i64 {
			i64(0)
		}
		string {
			''
		}
		else {
			void_value()
		}
	}
}

fn (e &Eval) zero_value_for_type_expr(expr ast.Expr) Value {
	match expr {
		ast.GenericArgs {
			if expr.lhs.name() == 'map' && expr.args.len == 2 {
				return MapValue{
					default_value: e.zero_value_for_type_expr(expr.args[1])
				}
			}
			return StructValue{
				type_name: generic_args_name(expr)
				fields:    map[string]Value{}
			}
		}
		ast.Ident {
			if expr.name == 'bool' {
				return false
			}
			if expr.name in ['byte', 'char', 'i8', 'i16', 'i32', 'int', 'i64', 'isize', 'rune',
				'u8', 'u16', 'u32', 'u64', 'usize'] {
				return i64(0)
			}
			if expr.name in ['f32', 'f64'] {
				return f64(0.0)
			}
			if expr.name == 'string' {
				return ''
			}
			type_name := if e.has_type_name(e.current_module_name(), expr.name) {
				'${e.current_module_name()}.${expr.name}'
			} else {
				expr.name
			}
			return e.zero_struct_value(type_name)
		}
		ast.SelectorExpr {
			return e.zero_struct_value(expr.name())
		}
		ast.Type {
			return match expr {
				ast.ArrayFixedType, ast.ArrayType {
					ArrayValue{}
				}
				ast.MapType {
					MapValue{
						default_value: e.zero_value_for_type_expr(expr.value_type)
					}
				}
				ast.OptionType, ast.ResultType, ast.NilType, ast.NoneType {
					void_value()
				}
				ast.TupleType {
					TupleValue{
						values: expr.types.map(e.zero_value_for_type_expr(it))
					}
				}
				else {
					e.zero_struct_value(e.type_node_name(expr))
				}
			}
		}
		else {
			return void_value()
		}
	}
}

fn (e &Eval) array_contains(receiver ArrayValue, needle Value) bool {
	for item in receiver.values {
		if e.value_eq(item, needle) {
			return true
		}
	}
	return false
}

fn (e &Eval) array_has(receiver ArrayValue, needle Value) bool {
	if e.array_contains(receiver, needle) {
		return true
	}
	if needle is string {
		for item in receiver.values {
			if item is StructValue {
				if name_value := item.fields['name'] {
					if e.value_string(name_value) == needle {
						return true
					}
				}
			}
		}
	}
	return false
}

fn (e &Eval) array_index(receiver ArrayValue, needle Value, reverse bool) i64 {
	if reverse {
		for i := receiver.values.len - 1; i >= 0; i-- {
			if e.value_eq(receiver.values[i], needle) {
				return i64(i)
			}
		}
		return -1
	}
	for i, item in receiver.values {
		if e.value_eq(item, needle) {
			return i64(i)
		}
	}
	return -1
}

fn (e &Eval) module_property(module_name string, field_name string) MaybeValue {
	if module_name == 'os' && field_name == 'args' {
		return MaybeValue{
			found: true
			value: ArrayValue{
				values: os.args.map(Value(it))
			}
		}
	}
	return MaybeValue{}
}

fn (e &Eval) is_type_expr(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return e.is_builtin_type_name(expr.name)
				|| e.has_type_name(e.current_module_name(), expr.name)
				|| e.resolve_mangled_type_value(expr.name) != none || e.is_sum_type_name(expr.name)
		}
		ast.Type {
			return true
		}
		ast.GenericArgs {
			return true
		}
		else {
			return false
		}
	}
}

fn generic_args_name(expr ast.GenericArgs) string {
	mut parts := []string{cap: expr.args.len}
	for arg in expr.args {
		parts << arg.name()
	}
	return '${expr.lhs.name()}[${parts.join(',')}]'
}

fn (e &Eval) type_expr_name(expr ast.Expr) string {
	return match expr {
		ast.GenericArgs {
			generic_args_name(expr)
		}
		ast.Ident {
			expr.name
		}
		ast.ModifierExpr {
			e.type_expr_name(expr.expr)
		}
		ast.ParenExpr {
			e.type_expr_name(expr.expr)
		}
		ast.SelectorExpr {
			expr.name()
		}
		ast.Type {
			e.type_node_name(expr)
		}
		else {
			expr.name()
		}
	}
}

fn (e &Eval) type_node_name(typ ast.Type) string {
	return match typ {
		ast.AnonStructType {
			'struct'
		}
		ast.ArrayFixedType {
			'[${typ.len.name()}]${e.type_expr_name(typ.elem_type)}'
		}
		ast.ArrayType {
			'[]${e.type_expr_name(typ.elem_type)}'
		}
		ast.ChannelType {
			'chan ${e.type_expr_name(typ.elem_type)}'
		}
		ast.FnType {
			'fn'
		}
		ast.GenericType {
			'${typ.name.name()}[${typ.params.name_list()}]'
		}
		ast.MapType {
			'map[${e.type_expr_name(typ.key_type)}]${e.type_expr_name(typ.value_type)}'
		}
		ast.NilType {
			'nil'
		}
		ast.NoneType {
			'none'
		}
		ast.OptionType {
			'?${e.type_expr_name(typ.base_type)}'
		}
		ast.ResultType {
			'!${e.type_expr_name(typ.base_type)}'
		}
		ast.ThreadType {
			if typ.elem_type is ast.EmptyExpr {
				'thread'
			} else {
				'thread ${e.type_expr_name(typ.elem_type)}'
			}
		}
		ast.TupleType {
			'(' + typ.types.map(e.type_expr_name(it)).join(',') + ')'
		}
	}
}

fn (e &Eval) is_builtin_type_name(name string) bool {
	return name in ['bool', 'byte', 'char', 'f32', 'f64', 'i8', 'i16', 'i32', 'int', 'i64', 'isize',
		'rune', 'string', 'u8', 'u16', 'u32', 'u64', 'usize', 'voidptr', 'byteptr']
}

fn (e &Eval) is_ref_type_name(name string) bool {
	trimmed := name.trim_space()
	if trimmed == '' {
		return false
	}
	if trimmed.starts_with('&') || trimmed.starts_with('[]') || trimmed.starts_with('map[')
		|| trimmed.starts_with('fn') || trimmed.starts_with('thread ')
		|| trimmed in ['array', 'map', 'module', 'range', 'tuple', 'type', 'void'] {
		return true
	}
	if trimmed.starts_with('?') || trimmed.starts_with('!') {
		return e.is_ref_type_name(trimmed[1..])
	}
	if e.is_builtin_type_name(trimmed) {
		return false
	}
	return trimmed.contains('.')
}

fn (e &Eval) sizeof_type_name(name string) i64 {
	trimmed := name.trim_space()
	if trimmed == '' {
		return 0
	}
	if trimmed.starts_with('?') || trimmed.starts_with('!') {
		return e.sizeof_type_name(trimmed[1..])
	}
	if trimmed.starts_with('&') || trimmed.ends_with('*') || trimmed == 'voidptr'
		|| trimmed == 'byteptr' || trimmed.starts_with('fn') {
		return 8
	}
	if trimmed.starts_with('[]') || trimmed == 'array' || trimmed.ends_with('Builder')
		|| trimmed.ends_with('.Builder') {
		return 24
	}
	if trimmed.starts_with('map[') || trimmed == 'map' {
		return 32
	}
	if trimmed.starts_with('chan ') || trimmed.starts_with('thread ') {
		return 8
	}
	return match trimmed {
		'bool', 'byte', 'char', 'i8', 'u8' {
			1
		}
		'i16', 'u16' {
			2
		}
		'f32', 'i32', 'int', 'rune', 'u32' {
			4
		}
		'f64', 'i64', 'isize', 'u64', 'usize' {
			8
		}
		'string' {
			16
		}
		'flags' {
			4
		}
		'module', 'range', 'tuple', 'type', 'void' {
			8
		}
		else {
			if trimmed.contains('.') || trimmed == 'struct' {
				8
			} else {
				0
			}
		}
	}
}

fn (e &Eval) cast_value(value Value, type_name string) !Value {
	if info := e.sum_type_info(type_name) {
		if value is StructValue && value.type_name == type_name && '_tag' in value.fields
			&& '_data' in value.fields {
			return value
		}
		if tag := e.sumtype_tag_from_value(info, value) {
			return e.build_sumtype_wrapper(type_name, info, tag, value)
		}
	}
	match type_name {
		'bool' {
			return e.value_as_bool(value)!
		}
		'byte', 'char', 'i8', 'i16', 'int', 'i64', 'rune', 'u8', 'u16', 'u32', 'u64' {
			return e.value_as_int(value)!
		}
		'f32', 'f64' {
			return e.value_as_f64(value)!
		}
		'string' {
			return e.value_string(value)
		}
		else {
			if value is StructValue {
				if payload := e.unwrap_sumtype_value(value, type_name) {
					return payload
				}
				return StructValue{
					type_name: type_name
					fields:    value.fields.clone()
				}
			}
			return value
		}
	}
}

fn (e &Eval) type_name_matches(actual string, expected string) bool {
	if actual == expected {
		return true
	}
	mut actual_module := ''
	if actual.contains('.') {
		actual_module = actual.all_before_last('.')
	}
	for alias in e.type_name_aliases(actual_module, actual) {
		if alias == expected {
			return true
		}
	}
	mut expected_module := ''
	if expected.contains('.') {
		expected_module = expected.all_before_last('.')
	}
	for alias in e.type_name_aliases(expected_module, expected) {
		if alias == actual {
			return true
		}
	}
	return false
}

fn (e &Eval) value_matches_type_name(value Value, target_name string) bool {
	match value {
		ArrayValue {
			return target_name == 'array' || target_name.starts_with('[]')
		}
		FlagsValue {
			return target_name == 'flags'
		}
		MapValue {
			return target_name == 'map' || target_name.starts_with('map[')
		}
		ModuleValue {
			return target_name == 'module'
		}
		RangeValue {
			return target_name == 'range'
		}
		StructValue {
			return e.type_name_matches(value.type_name, target_name)
		}
		TupleValue {
			return target_name == 'tuple'
		}
		TypeValue {
			return target_name == 'type' || value.name == target_name
		}
		VoidValue {
			return target_name in ['none', 'nil', 'void']
		}
		bool {
			return target_name == 'bool'
		}
		f64 {
			return target_name in ['f32', 'f64']
		}
		i64 {
			return target_name in ['byte', 'char', 'i8', 'i16', 'i32', 'int', 'i64', 'isize', 'rune',
				'u8', 'u16', 'u32', 'u64', 'usize']
		}
		string {
			return target_name == 'string'
		}
	}
}

fn (e &Eval) expect_string_arg(args []Value, index int) !string {
	if index >= args.len {
		return error('v2.eval: missing argument ${index}')
	}
	value := args[index]
	if value is string {
		return value
	}
	return e.value_string(value)
}

fn (e &Eval) value_to_string_array(value Value) ![]string {
	if value is ArrayValue {
		vals := value.values
		return vals.map(e.value_string(it))
	}
	return error('v2.eval: expected []string-compatible value')
}

fn (e &Eval) array_args_to_strings(args []Value) []string {
	mut out := []string{}
	for arg in args {
		if arg is ArrayValue {
			for item in arg.values {
				out << e.value_string(item)
			}
			continue
		}
		out << e.value_string(arg)
	}
	return out
}

fn (e &Eval) value_as_bool(value Value) !bool {
	return match value {
		bool { value }
		i64 { value != 0 }
		f64 { value != 0.0 }
		string { value.len > 0 }
		ArrayValue { value.values.len > 0 }
		FlagsValue { false }
		MapValue { value.entries.len > 0 }
		StructValue { value.fields.len > 0 }
		VoidValue { false }
		else { return error('v2.eval: `${e.runtime_type_name(value)}` can not be used as bool') }
	}
}

fn (e &Eval) value_as_int(value Value) !i64 {
	return match value {
		i64 {
			value
		}
		f64 {
			i64(value)
		}
		bool {
			if value {
				i64(1)
			} else {
				i64(0)
			}
		}
		string {
			strconv.parse_int(value, 0, 64)!
		}
		FlagsValue {
			i64(0)
		}
		else {
			return error('v2.eval: `${e.runtime_type_name(value)}` can not be used as int in `${e.current_function_label()}` stack `${e.call_stack_trace()}`')
		}
	}
}

fn (e &Eval) value_as_f64(value Value) !f64 {
	return match value {
		f64 {
			value
		}
		i64 {
			f64(value)
		}
		bool {
			if value {
				1.0
			} else {
				0.0
			}
		}
		string {
			strconv.atof64(value)!
		}
		else {
			return error('v2.eval: `${e.runtime_type_name(value)}` can not be used as float')
		}
	}
}

fn (e &Eval) map_entry_string(entry MapEntry) string {
	key_text := if entry.key is string {
		"'${entry.key}'"
	} else {
		e.value_string(entry.key)
	}
	return '${key_text}: ${e.value_string(entry.value)}'
}

fn (e &Eval) value_string(value Value) string {
	return match value {
		ArrayValue {
			vals := value.values
			'[' + vals.map(e.value_string(it)).join(', ') + ']'
		}
		FlagsValue {
			'flags'
		}
		MapValue {
			'{' + value.entries.map(e.map_entry_string(it)).join(', ') + '}'
		}
		ModuleValue {
			value.name
		}
		RangeValue {
			rs := value.start
			re := value.end
			sep := if value.inclusive { '...' } else { '..' }
			'${rs}${sep}${re}'
		}
		StructValue {
			keys := value.fields.keys()

			'${value.type_name}{' + keys.map('${it}: ${e.value_string(value.fields[it] or {
				void_value()
			})}').join(', ') + '}'
		}
		TupleValue {
			vals := value.values
			'(' + vals.map(e.value_string(it)).join(', ') + ')'
		}
		TypeValue {
			value.name
		}
		VoidValue {
			''
		}
		bool, f64, i64, string {
			value.str()
		}
	}
}

fn (e &Eval) runtime_type_name(value Value) string {
	return match value {
		ArrayValue { 'array' }
		FlagsValue { 'flags' }
		MapValue { 'map' }
		ModuleValue { 'module' }
		RangeValue { 'range' }
		StructValue { value.type_name }
		TupleValue { 'tuple' }
		TypeValue { 'type' }
		VoidValue { 'void' }
		bool { 'bool' }
		f64 { 'f64' }
		i64 { 'int' }
		string { 'string' }
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
