module types

import v.flat

// ComptimeConstValue is a typed value with no references to runtime storage.
// Aggregate updates copy their containers, preserving V value semantics.
pub struct ComptimeConstValue {
pub mut:
	typ      Type = builtin_void_type
	integer  i64
	text     string
	elements []ComptimeConstValue
	fields   map[string]ComptimeConstValue
}

struct ConstEval {
mut:
	tc             &TypeChecker
	scopes         []map[string]ComptimeConstValue
	active         map[string]bool
	expr_types     map[int]Type
	staged_indices map[int]int
	return_type    Type = builtin_void_type
	steps          int
	storage        int
	depth          int
}

struct ConstEvalFlow {
	kind  flat.NodeKind = .empty
	value ComptimeConstValue
}

struct ConstEvalShiftCount {
	known bool
	value int
}

struct ConstEvalAccessStep {
	kind  flat.NodeKind
	index i64
	field string
}

struct ConstEvalLvalue {
	scope int
	name  string
	path  []ConstEvalAccessStep
}

struct ConstEvalOrdering {
	branch      bool
	independent bool
}

// Integer lookup creates an evaluator only when it encounters required evaluation.
// Symbolic array bounds borrow the current evaluator, including its guards.
struct ConstIntContext {
mut:
	evaluator        &ConstEval = unsafe { nil }
	parent_evaluator &ConstEval = unsafe { nil }
	failure          string
	check_only       bool
}

// requires_comptime_const reports an explicit opt-in in a constant initializer.
pub fn (tc &TypeChecker) requires_comptime_const(id flat.NodeId) bool {
	if !tc.has_comptime_functions || !tc.valid_node_id(id) { return false }
	node := tc.a.node(id)
	// Constructing a callback does not execute its deferred body.
	if node.kind in [.fn_literal, .lambda_expr] { return false }
	if node.kind == .call {
		if decl := tc.comptime_static_enum_helper_decl(id, tc.cur_module) {
			if tc.declaration_has_attribute(flat.NodeId(decl.idx), 'comptime') { return true }
		}
	}
	for i in 0 .. node.children_count {
		if tc.requires_comptime_const(tc.a.child(node, i)) { return true }
	}
	return false
}

// eval_comptime_const evaluates one expression without running module initializers.
// The evaluator shares the enum evaluator's integer conversions and promotions.
pub fn (tc &TypeChecker) eval_comptime_const(id flat.NodeId) !ComptimeConstValue {
	mut e := tc.new_comptime_const_eval(id)!
	return e.expr(id)
}

fn (tc &TypeChecker) new_comptime_const_eval(id flat.NodeId) !&ConstEval {
	if tc.cross_target_prefs != unsafe { nil } {
		return error('required evaluation needs a concrete target; portable C is not supported')
	}
	file := if source := tc.a.source_files[tc.a.node(id).pos.id] {
		source.name
	} else {
		tc.cur_file
	}
	view := if file == tc.cur_file { tc } else { tc.fork_type_parse_view(file, tc.cur_module) }
	view.check_comptime_const_context()!
	return &ConstEval{ tc: view, scopes: [map[string]ComptimeConstValue{}] }
}

fn (tc &TypeChecker) check_comptime_const_context() ! {
	if tc.translated_files[tc.cur_file] {
		return error('required evaluation of translated code is not supported')
	}
}

fn (mut context ConstIntContext) start(tc &TypeChecker, id flat.NodeId) bool {
	if context.failure.len > 0 { return false }
	if context.check_only {
		context.failure = 'required evaluation during conditional type inference is not supported'
		return false
	}
	if context.evaluator == unsafe { nil } {
		if context.parent_evaluator != unsafe { nil } {
			context.evaluator = context.parent_evaluator
		} else {
			context.evaluator = tc.new_comptime_const_eval(id) or {
				context.failure = err.msg()
				return false
			}
		}
	}
	return true
}

fn (mut context ConstIntContext) expression(tc &TypeChecker, id flat.NodeId, module_name string) ?int {
	file := if source := tc.a.source_files[tc.a.node(id).pos.id] {
		source.name
	} else {
		tc.cur_file
	}
	view := tc.fork_type_parse_view(file, module_name)
	if !context.start(view, id) { return none }
	old_tc := context.evaluator.tc
	context.evaluator.tc = view
	defer { context.evaluator.tc = old_tc }
	value := context.evaluator.expr(id) or {
		context.failure = err.msg()
		return none
	}
	if !unalias_type(value.typ).is_integer() { return none }
	return int(value.integer)
}

fn (mut e ConstEval) tick() ! {
	e.steps++
	if e.steps > 1_000_000 { return error('evaluation exceeded 1000000 steps') }
}

fn (mut e ConstEval) reserve(count int) ! {
	if count < 0 || count > 100_000 || e.storage > 1_000_000 - count {
		return error('evaluation exceeded storage limit')
	}
	e.storage += count
}

fn (e &ConstEval) local(name string) ?ComptimeConstValue {
	for i := e.scopes.len - 1; i >= 0; i-- {
		if value := e.scopes[i][name] { return value }
	}
	return none
}

fn (mut e ConstEval) bind(name string, value ComptimeConstValue) {
	if name != '_' { e.scopes[e.scopes.len - 1][name] = value }
}

fn const_eval_integer_width(typ Type) int {
	base := unalias_type(typ)
	if base is Primitive && base.size > 0 { return int(base.size) }
	if base is Rune { return 32 }
	return platform_int_bits()
}

fn const_eval_integer_cast(value i64, typ Type) i64 {
	base := unalias_type(typ)
	if base is Rune { return i64(u32(value)) }
	if platform_int_bits() == 32 {
		if base is USize { return i64(u32(value)) }
		if base is ISize { return i64(i32(value)) }
	}
	return comptime_enum_integer_cast(value, base.source_type_id())
}

fn const_eval_integer_unsigned(typ Type) bool {
	base := unalias_type(typ)
	if base is Primitive { return base.props.has(.unsigned) }
	return base is USize || base is Rune
}

fn const_eval_c_integer_promotion(left Type, right Type) Type {
	left_width := int_max(32, const_eval_integer_width(left))
	right_width := int_max(32, const_eval_integer_width(right))
	left_unsigned := const_eval_integer_width(left) >= 32 && const_eval_integer_unsigned(left)
	right_unsigned := const_eval_integer_width(right) >= 32 && const_eval_integer_unsigned(right)
	unsigned := if left_unsigned == right_unsigned {
		left_unsigned
	} else if left_unsigned {
		left_width >= right_width
	} else {
		right_width >= left_width
	}
	if int_max(left_width, right_width) == 32 {
		return if unsigned { Type(u32_) } else { Type(i32_) }
	}
	return if unsigned { Type(u64_) } else { Type(i64_) }
}

fn (mut e ConstEval) convert(value ComptimeConstValue, destination Type) !ComptimeConstValue {
	typ := e.resolved_storage_type(destination)!
	base := unalias_type(typ)
	if base.is_integer() {
		if base is Primitive && base.size > 64 {
			return error('128-bit integers are not supported')
		}
		if !unalias_type(value.typ).is_integer() { return error('unsupported integer conversion') }
		return ComptimeConstValue{ typ: typ, integer: const_eval_integer_cast(value.integer, typ) }
	}
	if base is ArrayFixed {
		e.depth++
		defer { e.depth-- }
		if e.depth > 128 { return error('evaluation exceeded recursion limit') }
		if !e.fixed_array_value_matches(value, base)! {
			return error('unsupported conversion to `${typ.name()}`')
		}
		actual := unalias_type(value.typ)
		if actual is ArrayFixed && actual.elem_type.source_type_id() == base.elem_type.source_type_id() {
			return ComptimeConstValue{ ...value, typ: typ }
		}
		e.reserve(value.elements.len)!
		mut elements := []ComptimeConstValue{cap: value.elements.len}
		for element in value.elements {
			e.tick()!
			elements << e.convert(element, base.elem_type)!
		}
		return ComptimeConstValue{ ...value, typ: typ, elements: elements }
	}
	if (base is Primitive && base.props.has(.boolean)) || base is String || base is Struct {
		if unalias_type(value.typ).source_type_id() != base.source_type_id() {
			return error('unsupported conversion to `${typ.name()}`')
		}
		return ComptimeConstValue{ ...value, typ: typ }
	}
	return error('unsupported constant type `${typ.name()}`')
}

fn (mut e ConstEval) fixed_array_length(array ArrayFixed) !int {
	if array.len > 0 { return array.len }
	mut context := ConstIntContext{ evaluator: &e }
	return e.tc.const_int_value_with_context(array.len_expr, e.tc.cur_module, []string{}, mut context) or {
		return error(if context.failure.len > 0 {
			context.failure
		} else {
			'unknown fixed-array length'
		})
	}
}

// Array values carry concrete dimensions after leaving their declaration scope.
// Keep nominal aliases while replacing only their evaluator-owned array shapes.
fn (mut e ConstEval) resolved_storage_type(typ Type) !Type {
	if unalias_type(typ) !is ArrayFixed { return typ }
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	if typ is Alias {
		name_id := if typ.name_id in e.tc.type_alias_modules {
			typ.name_id
		} else {
			flat.source_type_syntax(typ.name_id).generic_base()
		}
		module_name := e.tc.type_alias_modules[name_id] or { e.tc.cur_module }
		decl := e.tc.first_type_declaration_ids[name_id] or { -1 }
		file := if decl >= 0 {
			if source := e.tc.a.source_files[e.tc.a.node(flat.NodeId(decl)).pos.id] {
				source.name
			} else {
				e.tc.cur_file
			}
		} else {
			e.tc.cur_file
		}
		old_tc := e.tc
		e.tc = e.tc.fork_type_parse_view(file, module_name)
		defer { e.tc = old_tc }
		e.tc.check_comptime_const_context()!
		base := e.resolved_storage_type(typ.base_type)!
		return Type(Alias{ name_id: typ.name_id, base_type: e.tc.intern_type_reference(base) })
	}
	if typ is ArrayFixed {
		length := e.fixed_array_length(typ)!
		element := e.resolved_storage_type(typ.elem_type)!
		return Type(ArrayFixed{
			elem_type: e.tc.intern_type_reference(element)
			len:       length
			len_expr:  if length == 0 { '0' } else { '' }
		})
	}
	return typ
}

fn (mut e ConstEval) array_element_types_match(actual Type, expected Type) !bool {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	left := unalias_type(actual)
	right := unalias_type(expected)
	if right is ArrayFixed {
		if left !is ArrayFixed { return false }
		length := e.fixed_array_length(right)!
		if left.len != length { return false }
		return e.array_element_types_match(left.elem_type, right.elem_type)
	}
	return left.source_type_id() == right.source_type_id()
}

fn (mut e ConstEval) fixed_array_value_matches(value ComptimeConstValue, expected ArrayFixed) !bool {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	actual := unalias_type(value.typ)
	if actual !is ArrayFixed { return false }
	if value.elements.len != e.fixed_array_length(expected)! { return false }
	if !e.array_element_types_match(actual.elem_type, expected.elem_type)! { return false }
	// Source bounds can belong to the caller's file. Their evaluated shapes are
	// already present in the values; only destination bounds use the current scope.
	element_type := unalias_type(expected.elem_type)
	if element_type is ArrayFixed {
		for element in value.elements {
			if !e.fixed_array_value_matches(element, element_type)! { return false }
		}
	}
	return true
}

fn (mut e ConstEval) zero(destination Type) !ComptimeConstValue {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	typ := e.resolved_storage_type(destination)!
	base := unalias_type(typ)
	if base is Primitive && base.size > 64 { return error('128-bit integers are not supported') }
	if base.is_integer() || (base is Primitive && base.props.has(.boolean)) || base is String {
		return ComptimeConstValue{ typ: typ }
	}
	if base is ArrayFixed {
		n := e.fixed_array_length(base)!
		e.reserve(n)!
		mut elements := []ComptimeConstValue{cap: n}
		for _ in 0 .. n { elements << e.zero(base.elem_type)! }
		return ComptimeConstValue{ typ: typ, elements: elements }
	}
	if base is Struct {
		// Specializations can lack concrete fields and declaration context here.
		// Reject them consistently, even after another pass materializes fields.
		if flat.source_type_syntax(base.name_id).application_valid() {
			return error('generic structs are not supported')
		}
		if base.name_id !in e.tc.structs {
			return error('struct field metadata is unavailable for `${base.name()}`')
		}
		if e.tc.unions[base.name_id] { return error('unions are not supported') }
		$if ownership ? {
			if e.tc.ownership_type_has_explicit_drop(base.name_id) {
				return error('types with implicit cleanup are not supported (`${base.name()}`)')
			}
		}
		file := e.tc.struct_files[base.name_id] or { e.tc.cur_file }
		module_name := e.tc.struct_modules[base.name_id] or { e.tc.cur_module }
		old_tc := e.tc
		e.tc = if file == e.tc.cur_file && module_name == e.tc.cur_module {
			e.tc
		} else {
			e.tc.fork_type_parse_view(file, module_name)
		}
		defer { e.tc = old_tc }
		e.tc.check_comptime_const_context()!
		mut fields := map[string]ComptimeConstValue{}
		for field in e.tc.structs[base.name_id] {
			if field.has_default || field.is_embed {
				return error('struct defaults and embedded fields are not supported')
			}
			fields[field.name] = e.zero(field.typ)!
		}
		return ComptimeConstValue{ typ: typ, fields: fields }
	}
	return error('unsupported constant type `${typ.name()}`')
}

fn (mut e ConstEval) constant(name string) !ComptimeConstValue {
	resolved := e.tc.resolve_selective_import_symbol(name) or { name }
	key := e.tc.const_key_for_name(resolved) or {
		e.tc.const_main_qualified_key(resolved) or { return error('cannot read runtime state `${name}`') }
	}
	return e.constant_value(key)
}

fn (mut e ConstEval) constant_value(key string) !ComptimeConstValue {
	if e.active[key] { return error('cycle in constant `${key}`') }
	e.active[key] = true
	old_tc := e.tc
	old_scopes := e.scopes
	e.tc = e.tc.fork_type_parse_view(e.tc.const_files[key] or { e.tc.cur_file }, e.tc.const_modules[key] or { e.tc.cur_module })
	e.scopes = [map[string]ComptimeConstValue{}]
	defer {
		e.tc = old_tc
		e.scopes = old_scopes
		e.active.delete(key)
	}
	e.tc.check_comptime_const_context()!
	// Completed values remain authoritative after transform has lowered their
	// initializers and helper bodies. Their storage types can still have bounds
	// that must resolve in the declaration's scope under the active cycle guard.
	if value := e.tc.comptime_const_values[key] {
		return e.convert(value, e.tc.const_types[key] or { value.typ })
	}
	id := e.tc.const_exprs[key] or { return error('constant `${key}` has no initializer') }
	value := e.expr_with_expected_type(id, e.tc.const_types[key] or { builtin_void_type })!
	return e.convert(value, e.tc.const_types[key] or { value.typ })
}

// Transform qualifies main constants, whose semantic table keys remain bare.
fn (tc &TypeChecker) const_main_qualified_key(name string) ?string {
	if name.starts_with('main.') {
		key := name[5..]
		if key in tc.const_exprs && is_main_module_name(tc.const_modules[key] or { '' }) {
			return key
		}
	}
	return none
}

fn (mut e ConstEval) expr(id flat.NodeId) !ComptimeConstValue {
	return e.expr_with_expected_type(id, builtin_void_type)
}

fn (mut e ConstEval) expr_with_expected_type(id flat.NodeId, expected Type) !ComptimeConstValue {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	if !e.tc.valid_node_id(id) { return error('missing constant expression') }
	e.tc.check_comptime_const_context()!
	value := e.expr_value(id, expected)!
	e.expr_types[int(id)] = value.typ
	if e.staged_indices[int(id)] > 0 { e.check_staged_integer(id, value)! }
	return value
}

fn (mut e ConstEval) expr_value(id flat.NodeId, expected Type) !ComptimeConstValue {
	node := e.tc.a.node(id)
	match node.kind {
		.int_literal {
			value := comptime_enum_integer_literal(node.value) or { return error('invalid integer literal') }
			return ComptimeConstValue{ typ: e.tc.expr_type(id) or { builtin_int_type }, integer: value }
		}
		.bool_literal {
			return ComptimeConstValue{ typ: bool_, integer: if node.value == 'true' { 1 } else { 0 } }
		}
		.string_literal {
			e.reserve(node.value.len)!
			return ComptimeConstValue{ typ: string_, text: node.value }
		}
		.ident { return e.local(node.value) or { e.constant(node.value)! } }
		.paren, .expr_stmt { return e.expr_with_expected_type(e.tc.a.child(node, 0), expected) }
		.cast_expr {
			typ := e.tc.parse_type(node.source_value_id())
			return e.convert(e.expr_with_expected_type(e.tc.a.child(node, 0), typ)!, typ)
		}
		.call { return e.call(id) }
		.prefix {
			v := e.expr(e.tc.a.child(node, 0))!
			if node.op == .not && unalias_type(v.typ).source_type_id() == flat.source_type_bool {
				return ComptimeConstValue{ typ: bool_, integer: if v.integer == 0 { 1 } else { 0 } }
			}
			if !unalias_type(v.typ).is_integer() { return error('unsupported unary operation') }
			value := match node.op {
				.plus { v.integer }
				.minus { i64(u64(0) - u64(v.integer)) }
				.bit_not { ~v.integer }
				else { return error('unsupported unary operation') }
			}
			// Narrow operands are promoted by C unary arithmetic.
			base := unalias_type(v.typ)
			typ := if base is Primitive && base.size > 0 && base.size < 32 {
				builtin_int_type
			} else {
				v.typ
			}
			c_type := e.integer_c_expression_type(id, typ)!
			return ComptimeConstValue{ typ: v.typ, integer: const_eval_integer_cast(value, c_type) }
		}
		.infix {
			left_id := e.tc.a.child(node, 0)
			right_id := e.tc.a.child(node, 1)
			left := e.expr(left_id)!
			if node.op == .logical_and && left.integer == 0 {
				return ComptimeConstValue{ typ: bool_ }
			}
			if node.op == .logical_or && left.integer != 0 {
				return ComptimeConstValue{ typ: bool_, integer: 1 }
			}
			right := e.expr(right_id)!
			if node.op !in [.logical_and, .logical_or]
				&& (e.hoists_value_branch(left_id)! || e.hoists_value_branch(right_id)!) {
				e.check_staged_integer(left_id, left)!
				e.check_staged_integer(right_id, right)!
			}
			return e.binary(id, node.op, left, right)
		}
		.if_expr, .comptime_if {
			branch := e.branch(node)!
			if branch < 0 { return error('conditional has no value') }
			branch_node := e.tc.a.node(branch)
			value := if branch_node.kind in [.if_expr, .comptime_if] {
				e.expr_with_expected_type(branch, expected)!
			} else {
				flow := e.scope_with_expected_type(branch_node, 0, expected)!
				if flow.kind != .empty {
					return error('control flow in a value expression is not supported')
				}
				flow.value
			}
			if node.kind == .if_expr {
				typ := if unalias_type(expected) is ArrayFixed {
					expected
				} else {
					e.expr_types[int(id)] or { e.if_result_type(id)! }
				}
				converted := e.convert(value, typ)!
				// Return lowering can use a branch directly, while a stored
				// conditional converts it to the inferred type. Do not fold a
				// value whose narrowing depends on that surrounding context.
				if unalias_type(value.typ).is_integer() && converted.integer != value.integer {
					return error('narrowing conditional integer values is not supported')
				}
				return converted
			}
			return value
		}
		.postfix {
			child := e.tc.a.child_node(node, 0)
			if node.op != .not || child.kind != .array_literal {
				return error('unsupported postfix expression')
			}
			context_type := e.fixed_literal_expected_type(child, expected)!
			if unalias_type(context_type) is ArrayFixed {
				return e.array_value(child, context_type, none)
			}
			if typ := e.tc.expr_type(id) { return e.array_value(child, typ, none) }
			if child.children_count == 0 { return error('unknown fixed-array type') }
			first := e.expr(e.tc.a.child(child, 0))!
			elem_type := e.tc.intern_type_reference(first.typ)
			typ := Type(ArrayFixed{ elem_type: elem_type, len: child.children_count })
			return e.array_value(child, typ, first)
		}
		.array_init, .array_literal {
			if node.kind == .array_literal {
				context_type := e.fixed_literal_expected_type(node, expected)!
				if unalias_type(context_type) is ArrayFixed {
					return e.array_value(node, context_type, none)
				}
			}
			typ := e.tc.expr_type(id) or {
				*e.tc.parse_type(if node.typ != 0 {
					node.typ
				} else {
					node.source_value_id()
				})
			}
			return e.array_value(node, typ, none)
		}
		.struct_init {
			typ := e.tc.expr_type(id) or { *e.tc.parse_type(node.source_value_id()) }
			base := unalias_type(typ)
			if base !is Struct { return error('only value structs are supported') }
			mut value := e.zero(typ)!
			fields := e.tc.structs[base.name_id]
			last_branch := e.last_value_branch(node, 0)!
			for i in 0 .. node.children_count {
				field := e.tc.a.child_node(node, i)
				if field.kind != .field_init || field.children_count != 1 {
					return error('unsupported struct initializer')
				}
				name := if field.value.len > 0 {
					field.value
				} else if i < fields.len {
					fields[i].name
				} else {
					return error('unknown struct field')
				}
				old := value.fields[name] or { return error('unknown struct field `${name}`') }
				value_id := e.tc.a.child(field, 0)
				field_value := e.expr_with_expected_type(value_id, old.typ)!
				if i < last_branch { e.check_staged_integer(value_id, field_value)! }
				value.fields[name] = e.convert(field_value, old.typ)!
			}
			return value
		}
		.index {
			root, path := e.access_path(id)!
			e.check_access_order(root, path, flat.empty_node)!
			staged := e.staged_access_indices(path)!
			for index in staged { e.staged_indices[int(index)] = e.staged_indices[int(index)] + 1 }
			defer {
				for index in staged {
					if e.staged_indices[int(index)] == 1 {
						e.staged_indices.delete(int(index))
					} else {
						e.staged_indices[int(index)] = e.staged_indices[int(index)] - 1
					}
				}
			}
			root_node := e.tc.a.node(root)
			if root_node.kind == .ident && e.local(root_node.value) != none {
				target := e.resolve_lvalue(root, path)!
				return e.lvalue_value(target)
			}
			container := e.expr(e.tc.a.child(node, 0))!
			e.check_builtin_index(container.typ)!
			index := e.expr(e.tc.a.child(node, 1))!
			if unalias_type(container.typ) !is ArrayFixed {
				return error('only fixed-array indexing is supported')
			}
			if index.integer < 0 || index.integer >= container.elements.len {
				return error('array index out of bounds')
			}
			return container.elements[int(index.integer)]
		}
		.selector {
			base_node := e.tc.a.child_node(node, 0)
			// A cached header combines sibling imports. Executing locals still
			// take precedence, even though the checker view has no function scope.
			if base_node.kind == .ident && e.local(base_node.value) == none
				&& e.tc.is_namespace_selector(node, base_node) {
				if module_name := e.tc.resolve_import_alias(base_node.value) {
					return e.constant('${module_name}.${node.value}')
				}
				if base_node.value == e.tc.cur_module || base_node.value == short_name_view(e.tc.cur_module)
					|| (base_node.value == 'main' && is_main_module_name(e.tc.cur_module)) {
					// Current-module lookup also handles main/builtin's bare keys.
					return e.constant(node.value)
				}
			}
			container := e.expr(e.tc.a.child(node, 0))!
			base := unalias_type(container.typ)
			if node.value == 'len' && (base is ArrayFixed || base is String) {
				return ComptimeConstValue{
					typ:     int_
					integer: if base is String {
						container.text.len
					} else {
						container.elements.len
					}
				}
			}
			return container.fields[node.value] or { return error('unsupported selector `${node.value}`') }
		}
		.sizeof_expr {
			typ := unalias_type(e.tc.parse_type(node.source_value_id()))
			if typ is Primitive {
				bits := if typ.size == 0 { platform_int_bits() } else { int(typ.size) }
				return ComptimeConstValue{
					typ:     u32_
					integer: if typ.props.has(.boolean) {
						1
					} else {
						bits / 8
					}
				}
			}
			return error('unsupported target-dependent sizeof operation')
		}
		else { return error('unsupported operation `${node.kind}`') }
	}
}

// Context types untyped literals, without widening conversions of existing array
// values or changing the length/type written on an explicitly typed literal.
fn (mut e ConstEval) fixed_literal_expected_type(node flat.Node, expected Type) !Type {
	if node.typ != 0 || unalias_type(expected) !is ArrayFixed { return builtin_void_type }
	typ := e.resolved_storage_type(expected)!
	base := unalias_type(typ)
	if base !is ArrayFixed { return builtin_void_type }
	if node.children_count != base.len {
		return builtin_void_type
	}
	return typ
}

fn (mut e ConstEval) array_value(node flat.Node, typ Type, first ?ComptimeConstValue) !ComptimeConstValue {
	base := unalias_type(typ)
	if base !is ArrayFixed { return error('only fixed arrays are supported') }
	mut value := e.zero(typ)!
	if node.children_count > value.elements.len { return error('too many array elements') }
	for i in 0 .. node.children_count {
		child := e.tc.a.child(node, i)
		if e.tc.a.node(child).kind == .field_init {
			return error('array init fields are not supported')
		}
		// The inferred postfix path already evaluated element zero for its type.
		element_type := value.elements[i].typ
		item := if i == 0 {
			first or { e.expr_with_expected_type(child, element_type)! }
		} else {
			e.expr_with_expected_type(child, element_type)!
		}
		value.elements[i] = e.convert(item, value.elements[i].typ)!
	}
	return value
}

fn (e &ConstEval) comparison_operand(id flat.NodeId, value ComptimeConstValue) i64 {
	mut node := e.tc.a.node(id)
	for node.kind == .paren && node.children_count > 0 {
		node = e.tc.a.child_node(node, 0)
	}
	if node.kind != .infix || node.op !in [.plus, .minus, .mul, .left_shift] {
		return value.integer
	}
	// The C backend truncates narrow arithmetic operands before comparisons.
	base := unalias_type(value.typ)
	if base is Primitive && base.size in [u8(8), 16] {
		return const_eval_integer_cast(value.integer, value.typ)
	}
	left := unalias_type(e.expr_types[int(e.tc.a.child(node, 0))] or { builtin_int_type })
	right := unalias_type(e.expr_types[int(e.tc.a.child(node, 1))] or { builtin_int_type })
	if left is Primitive && left.size in [u8(8), 16] && left.source_type_id() == right.source_type_id() {
		return const_eval_integer_cast(value.integer, left)
	}
	return value.integer
}

fn (e &ConstEval) shift_count_const_key(node flat.Node) ?string {
	mut name := node.value
	if node.kind == .selector {
		if node.children_count == 0 { return none }
		base := e.tc.a.child_node(node, 0)
		if base.kind != .ident { return none }
		if _ := e.local(base.value) { return none }
		if e.tc.global_names[e.tc.qualify_name(base.value)] { return none }
		file := if source := e.tc.a.source_files[node.pos.id] { source.name } else { e.tc.cur_file }
		view := if file == e.tc.cur_file {
			e.tc
		} else {
			e.tc.fork_type_parse_view(file, e.tc.cur_module)
		}
		module_name := view.resolve_import_alias(base.value) or { base.value }
		name = '${module_name}.${node.value}'
	} else if node.kind != .ident {
		return none
	} else {
		// Cgen consults global constants for an identifier even if a local
		// shadows it; the evaluated shift still uses the local's actual value.
		name = e.tc.resolve_selective_import_symbol(name) or { name }
	}
	return e.tc.const_key_for_name(name) or {
		e.tc.const_main_qualified_key(name) or {
			if !name.contains('.') { return e.tc.const_key_for_suffix(name) }
			return none
		}
	}
}

// Match Cgen's syntactic count lookup, including its legacy folding of ordinary
// constants. If a named constant needs evaluation, borrow this evaluator's guards.
fn (mut e ConstEval) shift_count_const_value(id flat.NodeId) !ConstEvalShiftCount {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	if !e.tc.valid_node_id(id) { return ConstEvalShiftCount{} }
	node := e.tc.a.node(id)
	match node.kind {
		.int_literal, .ident, .selector {
			name := if node.kind == .int_literal {
				node.value
			} else {
				e.shift_count_const_key(node) or { return ConstEvalShiftCount{} }
			}
			module_name := e.tc.const_modules[name] or { e.tc.cur_module }
			mut context := ConstIntContext{ parent_evaluator: &e }
			value := e.tc.const_int_value_with_context(name, module_name, [], mut context) or {
				if context.failure.len > 0 { return error(context.failure) }
				return ConstEvalShiftCount{}
			}
			return ConstEvalShiftCount{ known: true, value: value }
		}
		.paren {
			if node.children_count > 0 { return e.shift_count_const_value(e.tc.a.child(node, 0)) }
		}
		.prefix {
			if node.children_count == 0 || node.op !in [.plus, .minus, .bit_not] {
				return ConstEvalShiftCount{}
			}
			value := e.shift_count_const_value(e.tc.a.child(node, 0))!
			if !value.known { return value }
			return ConstEvalShiftCount{
				known: true
				value: match node.op {
					.plus { value.value }
					.minus { -value.value }
					else { ~value.value }
				}
			}
		}
		.infix {
			if node.children_count < 2 || node.op !in [.plus, .minus, .mul, .div, .mod, .pipe,
				.xor, .amp] {
				return ConstEvalShiftCount{}
			}
			left := e.shift_count_const_value(e.tc.a.child(node, 0))!
			if !left.known { return left }
			right := e.shift_count_const_value(e.tc.a.child(node, 1))!
			if !right.known { return right }
			if node.op in [.div, .mod] && right.value == 0 { return ConstEvalShiftCount{} }
			return ConstEvalShiftCount{
				known: true
				value: match node.op {
					.plus { left.value + right.value }
					.minus { left.value - right.value }
					.mul { left.value * right.value }
					.div { left.value / right.value }
					.mod { left.value % right.value }
					.pipe { left.value | right.value }
					.xor { left.value ^ right.value }
					else { left.value & right.value }
				}
			}
		}
		else {}
	}
	return ConstEvalShiftCount{}
}

fn (mut e ConstEval) shift_needs_64bit_widening(node flat.Node) !bool {
	if node.children_count < 2 { return false }
	lhs := e.tc.a.child_node(node, 0)
	if lhs.kind != .int_literal { return false }
	if source := e.tc.a.source_files[lhs.pos.id] {
		if e.tc.translated_files[source.name] { return false }
	}
	count := e.shift_count_const_value(e.tc.a.child(node, 1))!
	return count.known && count.value >= 31 && count.value < 64
}

fn (mut e ConstEval) integer_c_expression_type(id flat.NodeId, typ Type) !Type {
	node := e.tc.a.node(id)
	if node.kind == .sizeof_expr { return builtin_usize_type }
	if node.kind == .paren && node.children_count == 1 {
		return e.integer_c_expression_type(e.tc.a.child(node, 0), typ)
	}
	if node.kind == .prefix && node.op in [.plus, .minus, .bit_not] {
		child_id := e.tc.a.child(node, 0)
		mut child := e.tc.a.node(child_id)
		for child.kind == .paren && child.children_count == 1 {
			child = e.tc.a.child_node(child, 0)
		}
		if node.op == .minus && child.kind == .int_literal {
			literal := comptime_enum_integer_literal(child.value) or { return typ }
			if literal > 2147483647 { return i64_ }
		}
		operand := e.integer_c_expression_type(child_id, typ)!
		return if const_eval_integer_width(operand) < 32 { Type(i32_) } else { operand }
	}
	if node.kind == .infix && node.op == .left_shift && e.shift_needs_64bit_widening(node)! {
		return builtin_u64_type
	}
	if node.kind == .infix && node.op in [.plus, .minus, .mul, .amp, .pipe, .xor] {
		left_id := e.tc.a.child(node, 0)
		right_id := e.tc.a.child(node, 1)
		mut left := e.integer_c_expression_type(left_id, e.expr_types[int(left_id)] or { typ })!
		right := e.integer_c_expression_type(right_id, e.expr_types[int(right_id)] or { typ })!
		if node.op in [.plus, .minus, .mul] && e.integer_literal_arithmetic(left_id)
			&& e.integer_literal_arithmetic(right_id) {
			left = i64_
		}
		return const_eval_c_integer_promotion(left, right)
	}
	if node.kind != .int_literal { return e.expr_types[int(id)] or { typ } }
	// Literal comparisons bypass the C backend's mixed-sign helper.
	literal := comptime_enum_integer_literal(node.value) or { return typ }
	if literal < 0 { return u64_ }
	if literal <= 2147483647 { return i32_ }
	if node.value.starts_with('0x') || node.value.starts_with('0o') || node.value.starts_with('0b') {
		if literal <= 4294967295 { return u32_ }
	}
	return i64_
}

fn (e &ConstEval) integer_literal_arithmetic(id flat.NodeId) bool {
	if e.tc.translated_files[e.tc.cur_file] { return false }
	node := e.tc.a.node(id)
	return match node.kind {
		.int_literal { true }
		.paren { node.children_count == 1 && e.integer_literal_arithmetic(e.tc.a.child(node, 0)) }
		.prefix {
			node.op == .minus && node.children_count == 1 && e.integer_literal_arithmetic(e.tc.a.child(node, 0))
		}
		.infix {
			node.op in [.plus, .minus, .mul] && e.integer_literal_arithmetic(e.tc.a.child(node, 0)) && e.integer_literal_arithmetic(e.tc.a.child(node, 1))
		}
		else { false }
	}
}

fn (mut e ConstEval) references_local(id flat.NodeId) !bool {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	node := e.tc.a.node(id)
	if node.kind == .ident && e.local(node.value) != none {
		// Runtime checking may still consult a same-named global constant.
		// Its promotion differs between stored and directly returned values;
		// reject that ambiguity without evaluating a fresh helper invocation.
		mut context := ConstIntContext{ check_only: true }
		if e.tc.const_int_expr_with_context(id, e.tc.cur_module, [], mut context) != none
			|| context.failure.len > 0 {
			return error('integer promotion of a local that shadows a constant is not supported')
		}
		return true
	}
	for i in 0 .. node.children_count {
		if e.references_local(e.tc.a.child(node, i))! { return true }
	}
	return false
}

fn (mut e ConstEval) binary_result_type(id flat.NodeId, op flat.Op, left Type, right Type) !Type {
	node := e.tc.a.node(id)
	// A compound assignment uses the stored lvalue type, not a pre-check query
	// of its index/selector. Ordinary expressions use their evaluated operands.
	if node.kind != .infix { return left }
	if op == .right_shift_unsigned {
		if e.tc.integer_literal_source(e.tc.a.child(node, 0)) != none { return builtin_u32_type }
		return unsigned_shift_result_type(left)
	}
	if op in [.left_shift, .right_shift] { return left }
	left_id := e.tc.a.child(node, 0)
	right_id := e.tc.a.child(node, 1)
	// A helper-local int is stored data, even when its current value is known.
	// Checker constant queries have no evaluator scopes and can accidentally
	// restart evaluation of a shadowed global with fresh cycle and depth guards.
	for side in 0 .. 2 {
		lit_id := if side == 0 { left_id } else { right_id }
		other_id := if side == 0 { right_id } else { left_id }
		lit_type := if side == 0 { left } else { right }
		other_type := unalias_type(if side == 0 { right } else { left })
		if lit_type.source_type_id() != flat.source_type_int
			|| !other_type.is_integer() || other_type.source_type_id() == flat.source_type_int
			|| e.tc.int_literal_value(other_id) != none || e.references_local(lit_id)! {
			continue
		}
		mut context := ConstIntContext{ parent_evaluator: &e }
		value := e.tc.const_int_expr_with_context(lit_id, e.tc.cur_module, [], mut context) or {
			if context.failure.len > 0 { return error(context.failure) }
			continue
		}
		if other_type is Primitive && other_type.size in [u8(8), 16] {
			return if small_integer_type_accepts_int_literal(other_type, value) {
				Type(other_type)
			} else {
				builtin_int_type
			}
		}
		return *other_type
	}
	return infix_integer_promotion_type(left, right) or { left }
}

fn (e &ConstEval) check_builtin_operator(op flat.Op, left Type, right Type) ! {
	method_op := match op {
		.ne { flat.Op.eq }
		.gt, .ge, .le { flat.Op.lt }
		else { op }
	}
	if (left is Alias || left is Struct)
		&& e.tc.type_has_infix_operator_method(left, method_op)
		&& e.tc.infix_operator_return_type(method_op, left, right) != none {
		return error('overloaded operator `${op}` is not supported')
	}
}

// An empty node denotes an operand already converted to its stored type.
fn (mut e ConstEval) integer_comparison(op flat.Op, left_id flat.NodeId, right_id flat.NodeId, left ComptimeConstValue, right ComptimeConstValue) !ComptimeConstValue {
	typ := infix_integer_promotion_type(left.typ, right.typ) or { left.typ }
	type_id := unalias_type(typ).source_type_id()
	mut l := e.comparison_operand(left_id, left)
	mut r := e.comparison_operand(right_id, right)
	mut unsigned := type_id in [flat.source_type_u64, flat.source_type_uint]
	left_base := unalias_type(left.typ)
	right_base := unalias_type(right.typ)
	left_unsigned := const_eval_integer_unsigned(left.typ)
	right_unsigned := const_eval_integer_unsigned(right.typ)
	left_c_type := e.integer_c_expression_type(left_id, left.typ)!
	right_c_type := e.integer_c_expression_type(right_id, right.typ)!
	mixed_sign := left_base is Primitive && right_base is Primitive
		&& left_unsigned != right_unsigned
		&& !(!left_unsigned && e.tc.a.node(left_id).kind == .int_literal)
		&& !(!right_unsigned && e.tc.a.node(right_id).kind == .int_literal)
	if mixed_sign {
		// Match the C backend's mathematical signed/unsigned comparison.
		l = const_eval_integer_cast(l, left.typ)
		r = const_eval_integer_cast(r, right.typ)
		if left_base is Primitive && !left_unsigned && l < 0 {
			return ComptimeConstValue{
				typ:     bool_
				integer: if op in [.ne, .lt, .le] {
					1
				} else {
					0
				}
			}
		}
		if right_base is Primitive && !right_unsigned && r < 0 {
			return ComptimeConstValue{
				typ:     bool_
				integer: if op in [.ne, .gt, .ge] {
					1
				} else {
					0
				}
			}
		}
		unsigned = true
	} else if left_base is ISize || left_base is USize || left_base is Rune
		|| right_base is ISize || right_base is USize || right_base is Rune
		|| left_c_type.source_type_id() != left.typ.source_type_id()
		|| right_c_type.source_type_id() != right.typ.source_type_id() {
		// These C integer types use the usual arithmetic conversions rather
		// than the primitive mixed-sign helper emitted by the C backend.
		common := const_eval_c_integer_promotion(left_c_type, right_c_type)
		unsigned = const_eval_integer_unsigned(common)
		if const_eval_integer_width(common) == 32 {
			l = if unsigned { i64(u32(l)) } else { i64(i32(l)) }
			r = if unsigned { i64(u32(r)) } else { i64(i32(r)) }
		}
	} else if const_eval_integer_width(typ) >= 32 {
		l = const_eval_integer_cast(l, typ)
		r = const_eval_integer_cast(r, typ)
	}
	ok := match op {
		.eq { l == r }
		.ne { l != r }
		.lt {
			if unsigned { u64(l) < u64(r) } else { l < r }
		}
		.gt {
			if unsigned { u64(l) > u64(r) } else { l > r }
		}
		.le {
			if unsigned { u64(l) <= u64(r) } else { l <= r }
		}
		else {
			if unsigned { u64(l) >= u64(r) } else { l >= r }
		}
	}
	return ComptimeConstValue{ typ: bool_, integer: if ok { 1 } else { 0 } }
}

fn (mut e ConstEval) binary(id flat.NodeId, op flat.Op, left ComptimeConstValue, right ComptimeConstValue) !ComptimeConstValue {
	e.check_builtin_operator(op, left.typ, right.typ)!
	if unalias_type(left.typ) is String && unalias_type(right.typ) is String {
		if op == .plus {
			e.reserve(left.text.len + right.text.len)!
			// Match the checker's builtin fallback without erasing a nominal
			// alias before a later operator-dispatch check.
			typ := if right.typ is String { Type(right.typ) } else { left.typ }
			return ComptimeConstValue{ typ: typ, text: left.text + right.text }
		}
		if op in [.eq, .ne] {
			return ComptimeConstValue{
				typ:     bool_
				integer: if (left.text == right.text) == (op == .eq) {
					1
				} else {
					0
				}
			}
		}
		return error('unsupported string operation')
	}
	if op in [.logical_and, .logical_or] && unalias_type(left.typ).source_type_id() == flat.source_type_bool && unalias_type(right.typ).source_type_id() == flat.source_type_bool {
		return ComptimeConstValue{ typ: bool_, integer: if right.integer != 0 { 1 } else { 0 } }
	}
	if op in [.eq, .ne] && unalias_type(left.typ).source_type_id() == flat.source_type_bool && unalias_type(right.typ).source_type_id() == flat.source_type_bool {
		return ComptimeConstValue{
			typ:     bool_
			integer: if (left.integer == right.integer) == (op == .eq) {
				1
			} else {
				0
			}
		}
	}
	if !unalias_type(left.typ).is_integer() || !unalias_type(right.typ).is_integer() {
		return error('unsupported binary operands')
	}
	// Reuse integer/literal promotions with the evaluated operand types; helper
	// bodies need not have been checked or annotated yet.
	if op in [.eq, .ne, .lt, .gt, .le, .ge] {
		node := e.tc.a.node(id)
		return e.integer_comparison(op, e.tc.a.child(node, 0), e.tc.a.child(node, 1), left, right)
	}
	typ := e.binary_result_type(id, op, left.typ, right.typ)!
	type_id := unalias_type(typ).source_type_id()
	mut l := left.integer
	mut r := right.integer
	if op in [.div, .mod] {
		l = const_eval_integer_cast(l, typ)
		r = const_eval_integer_cast(r, typ)
		if r == 0 { return error('division by zero') }
		if type_id !in [flat.source_type_u64, flat.source_type_uint] && l == i64(-9223372036854775807) - 1 && r == -1 {
			return error('signed division overflow')
		}
	}
	if op in [.left_shift, .right_shift, .right_shift_unsigned] {
		shift_type := if op == .left_shift {
			e.integer_c_expression_type(id, typ)!
		} else if op == .right_shift_unsigned {
			typ
		} else {
			left.typ
		}
		if r < 0 || r >= const_eval_integer_width(shift_type) {
			return ComptimeConstValue{ typ: typ }
		}
		l = const_eval_integer_cast(l, shift_type)
	}

	result := match op {
		.plus { i64(u64(l) + u64(r)) }
		.minus { i64(u64(l) - u64(r)) }
		.mul { i64(u64(l) * u64(r)) }
		.div { comptime_enum_integer_division(l, r, type_id, false) }
		.mod { comptime_enum_integer_division(l, r, type_id, true) }
		.amp { l & r }
		.pipe { l | r }
		.xor { l ^ r }
		.left_shift { i64(u64(l) << r) }
		.right_shift {
			if unalias_type(left.typ).source_type_id() in [flat.source_type_u64, flat.source_type_uint] {
				i64(u64(l) >> r)
			} else {
				l >> r
			}
		}
		.right_shift_unsigned {
			shift_type := if const_eval_integer_width(typ) == 32 {
				flat.source_type_u32
			} else {
				unalias_type(typ).source_type_id()
			}
			comptime_enum_unsigned_right_shift(l, r, shift_type)
		}
		else { return error('unsupported integer operation `${op}`') }
	}
	// Keep the C expression's intermediate width until a V storage boundary
	// converts it to its semantic type.
	return ComptimeConstValue{
		typ:     typ
		integer: const_eval_integer_cast(result, e.integer_c_expression_type(id, typ)!)
	}
}

fn (mut e ConstEval) call(id flat.NodeId) !ComptimeConstValue {
	node := e.tc.a.node(id)
	decl := e.tc.comptime_static_enum_helper_decl(id, e.tc.cur_module) or { return error('unsupported call') }
	fn_id := flat.NodeId(decl.idx)
	if !e.tc.declaration_has_attribute(fn_id, 'comptime') {
		return error('call to `${e.tc.a.node(fn_id).value}` requires @[comptime]')
	}
	f := e.tc.a.node(fn_id)
	if f.generic_params().len > 0 { return error('generic calls are not supported') }
	old_tc := e.tc
	old_scopes := e.scopes
	old_return_type := e.return_type
	file := if source := e.tc.a.source_files[f.pos.id] { source.name } else { e.tc.cur_file }
	callee := if e.tc.cur_file == file && e.tc.cur_module == decl.mod {
		e.tc
	} else {
		e.tc.fork_type_parse_view(file, decl.mod)
	}
	defer {
		e.tc = old_tc
		e.scopes = old_scopes
		e.return_type = old_return_type
	}
	// Resolve formal storage shapes in their declaration's view, then evaluate
	// each argument once in the caller's view with that literal context.
	e.tc = callee
	e.scopes = [map[string]ComptimeConstValue{}]
	e.tc.check_comptime_const_context()!
	mut param_types := []Type{}
	for i in 0 .. f.children_count {
		param := e.tc.a.child_node(f, i)
		if param.kind == .param {
			param_types << e.resolved_storage_type(e.tc.parse_type(param.typ))!
		}
	}
	return_type := e.resolved_storage_type(e.tc.parse_type(f.typ))!
	e.tc = old_tc
	e.scopes = old_scopes
	mut args := []ComptimeConstValue{}
	last_branch := e.last_value_branch(node, 1)!
	for i in 1 .. node.children_count {
		arg_id := e.tc.a.child(node, i)
		expected := if i - 1 < param_types.len { param_types[i - 1] } else { builtin_void_type }
		arg := e.expr_with_expected_type(arg_id, expected)!
		if i < last_branch { e.check_staged_integer(arg_id, arg)! }
		args << arg
	}
	e.tc = callee
	e.scopes = [map[string]ComptimeConstValue{}]
	e.return_type = return_type
	mut param_index := 0
	for i in 0 .. f.children_count {
		param := e.tc.a.child_node(f, i)
		if param.kind != .param { continue }
		if param_index >= args.len || param.is_mut {
			return error('unsupported arguments to `${f.value}`')
		}
		e.bind(param.value, e.convert(args[param_index], param_types[param_index])!)
		param_index++
	}
	if param_index != args.len { return error('incorrect argument count') }
	flow := e.sequence(f, 0) or { return error('in `${f.value}`: ${err}') }
	if flow.kind != .return_stmt { return error('`${f.value}` did not return a value') }
	return e.convert(flow.value, return_type)
}

// Runtime lowering can store earlier operands before a value branch's prelude.
// Compile-time branches are pruned before their inactive bodies can stage values.
fn (mut e ConstEval) hoists_value_branch(id flat.NodeId) !bool {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	if !e.tc.valid_node_id(id) { return false }
	node := e.tc.a.node(id)
	if node.kind in [.if_expr, .match_stmt, .or_expr] { return true }
	if node.kind in [.fn_literal, .lambda_expr, .spawn_expr] { return false }
	if node.kind == .comptime_if { return e.hoists_value_branch(e.branch(node)!) }
	for i in 0 .. node.children_count {
		if e.hoists_value_branch(e.tc.a.child(node, i))! { return true }
	}
	return false
}

fn (mut e ConstEval) last_value_branch(node flat.Node, start int) !int {
	mut last := -1
	for i in start .. node.children_count {
		if e.hoists_value_branch(e.tc.a.child(node, i))! { last = i }
	}
	return last
}

fn (mut e ConstEval) staging_operand_is_pure(id flat.NodeId) !bool {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	node := e.tc.a.node(id)
	match node.kind {
		.int_literal, .bool_literal, .string_literal, .sizeof_expr { return true }
		.paren, .cast_expr {
			return node.children_count == 1 && e.staging_operand_is_pure(e.tc.a.child(node, 0))!
		}
		.prefix {
			return node.op in [.plus, .minus, .bit_not, .not] && node.children_count == 1
				&& e.staging_operand_is_pure(e.tc.a.child(node, 0))!
		}
		.infix { return e.integer_literal_arithmetic(id) }
		else { return false }
	}
}

// Reject staging whose storage conversion changes either the value or effective
// C integer type. Equal values alone do not preserve widened unsigned comparisons.
fn (mut e ConstEval) check_staged_integer(id flat.NodeId, value ComptimeConstValue) ! {
	if !unalias_type(value.typ).is_integer() || e.staging_operand_is_pure(id)! { return }
	original := const_eval_c_integer_promotion(e.integer_c_expression_type(id, value.typ)!, i32_)
	stored := const_eval_c_integer_promotion(value.typ, i32_)
	if const_eval_integer_cast(value.integer, value.typ) != value.integer
		|| const_eval_integer_width(original) != const_eval_integer_width(stored)
		|| const_eval_integer_unsigned(original) != const_eval_integer_unsigned(stored) {
		return error('integer operand staging that changes its value or C type is not supported')
	}
}

fn (mut e ConstEval) staged_access_indices(path []flat.NodeId) ![]flat.NodeId {
	mut last_branch := -1
	for i, id in path {
		node := e.tc.a.node(id)
		if node.kind == .index && e.hoists_value_branch(e.tc.a.child(node, 1))! { last_branch = i }
	}
	mut staged := []flat.NodeId{}
	for i, id in path {
		node := e.tc.a.node(id)
		if i < last_branch && node.kind == .index { staged << e.tc.a.child(node, 1) }
	}
	return staged
}

fn (mut e ConstEval) if_result_type(id flat.NodeId) !Type {
	e.check_conditional_type_queries(id)!
	mut locals := map[string]Type{}
	for scope in e.scopes {
		for name, value in scope { locals[name] = value.typ }
	}
	mut bindings := []LocalBinding{cap: locals.len}
	for name, typ in locals { bindings << LocalBinding{ name: name, typ: typ } }
	mut typing := e.tc.fork_type_parse_view(e.tc.cur_file, e.tc.cur_module)
	return e.if_result_type_in_context(mut typing, id, bindings)
}

// The general checker infers both branch types and may query integer constants
// for promotions. Probe those lookups without executing another evaluator: its
// fresh scopes and guards cannot safely describe this active helper invocation.
fn (mut e ConstEval) check_conditional_type_queries(id flat.NodeId) ! {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	node := e.tc.a.node(id)
	if node.kind == .infix && node.op !in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or,
		.left_shift, .right_shift, .right_shift_unsigned] {
		for i in 0 .. node.children_count {
			child := e.tc.a.child(node, i)
			mut context := ConstIntContext{ check_only: true }
			if e.tc.requires_comptime_const(child) {
				return error('required evaluation during conditional type inference is not supported')
			}
			_ = e.tc.const_int_expr_with_context(child, e.tc.cur_module, [], mut context)
			if context.failure.len > 0 { return error(context.failure) }
		}
	}
	for i in 0 .. node.children_count {
		e.check_conditional_type_queries(e.tc.a.child(node, i))!
	}
}

fn (e &ConstEval) if_result_type_in_context(mut typing TypeChecker, id flat.NodeId, bindings []LocalBinding) !Type {
	node := e.tc.a.node(id)
	mut result := builtin_void_type
	for i in 1 .. node.children_count {
		branch_id := e.tc.a.child(node, i)
		mut typ := builtin_void_type
		if e.tc.a.node(branch_id).kind == .if_expr {
			typ = e.if_result_type_in_context(mut typing, branch_id, bindings)!
		} else if !typing.branch_tail_never_returns(branch_id) {
			tail := typing.branch_tail_expr_id(branch_id)
			actual, untyped := typing.resolve_branch_tail_expr(branch_id, tail, builtin_void_type, bindings)
			if untyped || actual is Unknown {
				return error('unsupported conditional result type')
			}
			typ = actual
		}
		result = typing.choose_translated_if_tail_type(id, result, typ)
	}
	return result
}

fn (mut e ConstEval) branch(node flat.Node) !flat.NodeId {
	take_then := if node.kind == .comptime_if {
		e.tc.comptime_type_condition_value(node.value) or { return error('unsupported target condition `${node.value}`') }
	} else {
		condition := e.expr(e.tc.a.child(node, 0))!
		if unalias_type(condition.typ).source_type_id() != flat.source_type_bool {
			return error('expected boolean condition')
		}
		condition.integer != 0
	}
	index := if node.kind == .comptime_if {
		if take_then { 0 } else { 1 }
	} else {
		if take_then { 1 } else { 2 }
	}
	return if index < node.children_count { e.tc.a.child(node, index) } else { flat.empty_node }
}

fn (mut e ConstEval) scope(node flat.Node, start int) !ConstEvalFlow {
	return e.scope_with_expected_type(node, start, builtin_void_type)
}

fn (mut e ConstEval) scope_with_expected_type(node flat.Node, start int, expected Type) !ConstEvalFlow {
	e.scopes << map[string]ComptimeConstValue{}
	defer { e.scopes.delete_last() }
	return e.sequence_with_expected_type(node, start, expected)
}

fn (mut e ConstEval) sequence(node flat.Node, start int) !ConstEvalFlow {
	return e.sequence_with_expected_type(node, start, builtin_void_type)
}

fn (mut e ConstEval) sequence_with_expected_type(node flat.Node, start int, expected Type) !ConstEvalFlow {
	mut result := ConstEvalFlow{}
	for i in start .. node.children_count {
		typ := if i + 1 == node.children_count { expected } else { builtin_void_type }
		result = e.stmt_with_expected_type(e.tc.a.child(node, i), typ)!
		if result.kind != .empty { return result }
	}
	return result
}

fn (mut e ConstEval) stmt(id flat.NodeId) !ConstEvalFlow {
	return e.stmt_with_expected_type(id, builtin_void_type)
}

fn (mut e ConstEval) stmt_with_expected_type(id flat.NodeId, expected Type) !ConstEvalFlow {
	e.tick()!
	node := e.tc.a.node(id)
	match node.kind {
		.empty, .param { return ConstEvalFlow{} }
		.block { return e.scope_with_expected_type(node, 0, expected) }
		.return_stmt {
			if node.children_count != 1 { return error('only single value returns are supported') }
			return ConstEvalFlow{
				kind:  .return_stmt
				value: e.expr_with_expected_type(e.tc.a.child(node, 0), e.return_type)!
			}
		}
		.if_expr, .comptime_if {
			branch := e.branch(node)!
			return if branch < 0 {
				ConstEvalFlow{}
			} else {
				e.stmt_with_expected_type(branch, expected)!
			}
		}
		.decl_assign, .assign, .index_assign, .selector_assign {
			if node.children_count != 2 { return error('only single assignments are supported') }
			lhs := e.tc.a.child(node, 0)
			rhs := e.tc.a.child(node, 1)
			if node.kind == .decl_assign {
				value := e.expr(rhs)!
				name := e.tc.a.node(lhs)
				if name.kind != .ident { return error('unsupported local declaration') }
				e.bind(name.value, e.convert(value, value.typ)!)
			} else {
				root, path := e.access_path(lhs)!
				e.check_access_order(root, path, rhs)!
				mut value := e.expr_with_expected_type(rhs, e.lvalue_type(root, path))!
				target := e.resolve_lvalue(root, path)!
				if node.op != .assign {
					op := match node.op {
						.plus_assign { flat.Op.plus }
						.minus_assign { flat.Op.minus }
						.mul_assign { flat.Op.mul }
						.div_assign { flat.Op.div }
						.mod_assign { flat.Op.mod }
						.amp_assign { flat.Op.amp }
						.pipe_assign { flat.Op.pipe }
						.xor_assign { flat.Op.xor }
						.left_shift_assign { flat.Op.left_shift }
						.right_shift_assign { flat.Op.right_shift }
						else { return error('unsupported compound assignment') }
					}
					value = e.binary(lhs, op, e.lvalue_value(target)!, value)!
				}
				e.assign(target, value)!
			}
			return ConstEvalFlow{}
		}
		.postfix {
			lhs := e.tc.a.child(node, 0)
			root, path := e.access_path(lhs)!
			e.check_access_order(root, path, flat.empty_node)!
			target := e.resolve_lvalue(root, path)!
			old := e.lvalue_value(target)!
			if !unalias_type(old.typ).is_integer() || node.op !in [.inc, .dec] {
				return error('unsupported postfix operation')
			}
			e.assign(target, ComptimeConstValue{
				typ:     old.typ
				integer: if node.op == .inc {
					i64(u64(old.integer) + 1)
				} else {
					i64(u64(old.integer) - 1)
				}
			})!
			return ConstEvalFlow{}
		}
		.for_in_stmt {
			header := node.value.int()
			if header !in [3, 4] || node.op == .amp {
				return error('only integer range loops are supported')
			}
			container := e.tc.a.child_node(node, 2)
			if header == 3 && container.kind != .range {
				return error('only integer range loops are supported')
			}
			start_id := if header == 4 { e.tc.a.child(node, 2) } else { e.tc.a.child(container, 0) }
			end_id := if header == 4 { e.tc.a.child(node, 3) } else { e.tc.a.child(container, 1) }
			start := e.expr(start_id)!
			end := e.expr(end_id)!
			if !unalias_type(start.typ).is_integer() || !unalias_type(end.typ).is_integer() {
				return error('expected integer range')
			}
			range_type := if e.tc.range_endpoint_is_literal(start_id) && !e.tc.range_endpoint_is_literal(end_id) {
				end.typ
			} else {
				start.typ
			}
			live_bound := e.range_bound_is_live(end_id)!
			// The loop index is stored in range_type, while a materialized upper
			// bound has its own storage conversion and a live bound stays an expression.
			mut limit := if live_bound { end } else { e.convert(end, end.typ)! }
			limit_id := if live_bound { end_id } else { flat.empty_node }
			e.check_builtin_operator(.lt, range_type, limit.typ)!
			for index := const_eval_integer_cast(start.integer, range_type); e.integer_comparison(.lt,
				flat.empty_node, limit_id, ComptimeConstValue{ typ: range_type, integer: index }, limit)!.integer != 0; index = const_eval_integer_cast(i64(u64(index) + 1), range_type) {
				e.tick()!
				e.scopes << map[string]ComptimeConstValue{}
				name := e.tc.a.child_node(node, 0).value
				e.bind(name, ComptimeConstValue{ typ: range_type, integer: index })
				flow := e.sequence(node, header)!
				e.scopes.delete_last()
				if flow.kind == .return_stmt { return flow }
				if flow.kind == .break_stmt { break }
				if live_bound {
					limit = e.expr(end_id)!
				}
			}
			return ConstEvalFlow{}
		}
		.for_stmt {
			e.scopes << map[string]ComptimeConstValue{}
			defer { e.scopes.delete_last() }
			_ = e.stmt(e.tc.a.child(node, 0))!
			for {
				e.tick()!
				cond := e.tc.a.child(node, 1)
				if e.tc.a.node(cond).kind != .empty && e.expr(cond)!.integer == 0 { break }
				flow := e.scope(node, 3)!
				if flow.kind == .return_stmt { return flow }
				if flow.kind == .break_stmt { break }
				if e.unsigned_inclusive_loop_post_overflows(cond, e.tc.a.child(node, 2))! { break }
				_ = e.stmt(e.tc.a.child(node, 2))!
			}
			return ConstEvalFlow{}
		}
		.break_stmt, .continue_stmt {
			if node.value.len > 0 { return error('labeled control flow is not supported') }
			return ConstEvalFlow{ kind: node.kind }
		}
		.expr_stmt {
			child := e.tc.a.child(node, 0)
			if e.tc.a.node(child).kind == .postfix && e.tc.a.node(child).op in [.inc, .dec] {
				return e.stmt(child)
			}
			return ConstEvalFlow{ value: e.expr_with_expected_type(child, expected)! }
		}
		else { return error('unsupported statement `${node.kind}`') }
	}
}

// Match transform's unsigned_inclusive_for_post_body at the post boundary,
// including continue paths and mutations made by the loop body.
fn (e &ConstEval) unsigned_inclusive_loop_post_overflows(cond_id flat.NodeId, post_id flat.NodeId) !bool {
	if !e.tc.valid_node_id(cond_id) || !e.tc.valid_node_id(post_id) { return false }
	cond := e.tc.a.node(cond_id)
	post := e.tc.a.node(post_id)
	if cond.kind != .infix || cond.children_count < 2
		|| post.kind != .expr_stmt || post.children_count == 0 {
		return false
	}
	post_expr := e.tc.a.child_node(post, 0)
	if post_expr.kind != .postfix || post_expr.children_count == 0
		|| post_expr.op !in [.inc, .dec] {
		return false
	}
	if (post_expr.op == .inc && cond.op != .le)
		|| (post_expr.op == .dec && cond.op != .ge) {
		return false
	}
	index := e.tc.a.child_node(post_expr, 0)
	lhs := e.tc.a.child_node(cond, 0)
	if index.kind != .ident || lhs.kind != .ident || lhs.value != index.value { return false }
	value := e.local(index.value) or { return false }
	if unalias_type(value.typ).source_type_id() !in [flat.source_type_u8, flat.source_type_u16,
		flat.source_type_u32, flat.source_type_u64, flat.source_type_uint] {
		return false
	}
	limit := if post_expr.op == .inc { const_eval_integer_cast(-1, value.typ) } else { i64(0) }
	if value.integer != limit { return false }
	// Runtime storage can retain or erase a nominal alias depending on the
	// declaration. Reject ambiguous boundary guards instead of guessing that type.
	if value.typ is Alias {
		return error('unsigned inclusive loop boundary guards for alias counters are not supported')
	}
	return true
}

// Match transform's stable_expr_for_reuse for supported range bounds. Other
// expressions are materialized before the loop, including parenthesized bounds.
fn (mut e ConstEval) range_bound_is_live(id flat.NodeId) !bool {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	node := e.tc.a.node(id)
	match node.kind {
		.ident, .int_literal, .bool_literal, .string_literal, .sizeof_expr { return true }
		.selector {
			return node.children_count > 0 && e.range_bound_is_live(e.tc.a.child(node, 0))!
		}
		.cast_expr, .field_init {
			return node.children_count == 0 || e.range_bound_is_live(e.tc.a.child(node, 0))!
		}
		.index {
			return node.children_count >= 2 && e.range_bound_is_live(e.tc.a.child(node, 0))!
				&& e.range_bound_is_live(e.tc.a.child(node, 1))!
		}
		.struct_init {
			typ := e.tc.expr_type(id) or { *e.tc.parse_type(node.source_value_id()) }
			base := unalias_type(typ)
			if base is Struct {
				for field in e.tc.structs[base.name_id] {
					if unalias_type(field.typ) is ArrayFixed { return false }
				}
			}
			for i in 0 .. node.children_count {
				if !e.range_bound_is_live(e.tc.a.child(node, i))! { return false }
			}
			return true
		}
		else { return false }
	}
}

fn (mut e ConstEval) access_path(id flat.NodeId) !(flat.NodeId, []flat.NodeId) {
	mut current := id
	mut path := []flat.NodeId{}
	mut depth := e.depth
	for {
		e.tick()!
		depth++
		if depth > 128 { return error('evaluation exceeded recursion limit') }
		node := e.tc.a.node(current)
		if node.kind == .paren {
			current = e.tc.a.child(node, 0)
		} else if node.kind in [.index, .selector] {
			if node.kind == .index && node.op == .gated_index {
				return error('gated indexing is not supported')
			}
			path << current
			current = e.tc.a.child(node, 0)
		} else {
			return current, path.reverse()
		}
	}
	return current, path
}

// Classify ordering without executing a branch or reading a local's value.
fn (mut e ConstEval) expression_ordering(id flat.NodeId) !ConstEvalOrdering {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	node := e.tc.a.node(id)
	if node.kind in [.if_expr, .comptime_if] { return ConstEvalOrdering{ branch: true } }
	mut independent := node.kind !in [.call, .decl_assign, .assign, .index_assign, .selector_assign]
		&& !(node.kind == .ident && e.local(node.value) != none)
		&& !(node.kind == .postfix && node.op in [.inc, .dec])
	mut branch := false
	for i in 0 .. node.children_count {
		child := e.expression_ordering(e.tc.a.child(node, i))!
		branch = branch || child.branch
		independent = independent && child.independent
	}
	return ConstEvalOrdering{ branch: branch, independent: independent }
}

fn (mut e ConstEval) check_access_order(root flat.NodeId, path []flat.NodeId, rhs flat.NodeId) ! {
	mut has_branch := false
	mut independent_indices := true
	for id in path {
		node := e.tc.a.node(id)
		if node.kind != .index { continue }
		index := e.tc.a.child(node, 1)
		ordering := e.expression_ordering(index)!
		has_branch = has_branch || ordering.branch
		mut whole := index
		for e.tc.a.node(whole).kind == .paren { whole = e.tc.a.child(e.tc.a.node(whole), 0) }
		independent_indices = independent_indices && (ordering.independent
			|| e.tc.a.node(whole).kind == .if_expr)
	}
	if !has_branch { return }
	// Transform materializes whole value conditionals, but leaves ordinary
	// indices live. Mixed dependent expressions require staging outside this
	// evaluator's milestone rather than an approximation of runtime ordering.
	root_node := e.tc.a.node(root)
	local_root := root_node.kind == .ident && e.local(root_node.value) != none
	if !independent_indices || (!local_root && !e.expression_ordering(root)!.independent)
		|| (rhs >= 0 && !e.expression_ordering(rhs)!.independent) {
		return error('conditional index ordering with dependent expressions is not supported')
	}
}

// Read only storage types here: evaluating an index before the RHS would change
// assignment ordering. Array values have homogeneous element/field types, and an
// empty array cannot contain a valid indexed destination.
fn (e &ConstEval) lvalue_type(root flat.NodeId, path []flat.NodeId) Type {
	node := e.tc.a.node(root)
	if node.kind != .ident { return builtin_void_type }
	mut value := e.local(node.value) or { return builtin_void_type }
	for id in path {
		step := e.tc.a.node(id)
		if step.kind == .index {
			if value.elements.len == 0 { return builtin_void_type }
			value = value.elements[0]
		} else {
			value = value.fields[step.value] or { return builtin_void_type }
		}
	}
	return value.typ
}

fn (mut e ConstEval) resolve_lvalue(root flat.NodeId, path []flat.NodeId) !ConstEvalLvalue {
	node := e.tc.a.node(root)
	if node.kind != .ident { return error('unsupported assignment target') }
	mut scope := -1
	for i := e.scopes.len - 1; i >= 0; i-- {
		if node.value in e.scopes[i] {
			scope = i
			break
		}
	}
	if scope < 0 { return error('cannot mutate runtime state `${node.value}`') }
	mut steps := []ConstEvalAccessStep{cap: path.len}
	for id in path {
		e.tick()!
		step := e.tc.a.node(id)
		steps << ConstEvalAccessStep{
			kind:  step.kind
			index: if step.kind == .index { e.expr(e.tc.a.child(step, 1))!.integer } else { 0 }
			field: step.value
		}
	}
	return ConstEvalLvalue{ scope: scope, name: node.value, path: steps }
}

fn (e &ConstEval) check_builtin_index(typ Type) ! {
	if e.tc.index_overload_call_info(typ, false) != none
		|| e.tc.index_overload_call_info(typ, true) != none {
		return error('overloaded indexing is not supported')
	}
}

fn (e &ConstEval) access_value(base ComptimeConstValue, step ConstEvalAccessStep) !ComptimeConstValue {
	if step.kind == .index {
		e.check_builtin_index(base.typ)!
		if unalias_type(base.typ) !is ArrayFixed {
			return error('only fixed-array indexing is supported')
		}
		if step.index < 0 || step.index >= base.elements.len {
			return error('array index out of bounds')
		}
		return base.elements[int(step.index)]
	}
	return base.fields[step.field] or { return error('unknown struct field') }
}

fn (mut e ConstEval) lvalue_value(target ConstEvalLvalue) !ComptimeConstValue {
	mut value := e.scopes[target.scope][target.name] or { return error('missing local value') }
	for step in target.path {
		e.tick()!
		value = e.access_value(value, step)!
	}
	return value
}

fn (mut e ConstEval) assign(target ConstEvalLvalue, value ComptimeConstValue) ! {
	// Index expressions may update other fields in this root. Read its current
	// value only after resolving the path, then copy containers on writeback.
	mut old := e.scopes[target.scope][target.name] or { return error('missing local value') }
	mut parents := []ComptimeConstValue{cap: target.path.len}
	for step in target.path {
		e.tick()!
		parents << old
		old = e.access_value(old, step)!
	}
	mut updated := e.convert(value, old.typ)!
	for i := target.path.len - 1; i >= 0; i-- {
		e.tick()!
		step := target.path[i]
		mut base := parents[i]
		if step.kind == .index {
			e.reserve(base.elements.len)!
			base = ComptimeConstValue{ ...base, elements: base.elements.clone() }
			base.elements[int(step.index)] = updated
		} else {
			e.reserve(base.fields.len)!
			base = ComptimeConstValue{ ...base, fields: base.fields.clone() }
			base.fields[step.field] = updated
		}
		updated = base
	}
	e.scopes[target.scope][target.name] = updated
}
