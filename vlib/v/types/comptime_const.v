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
	tc         &TypeChecker
	scopes     []map[string]ComptimeConstValue
	active     map[string]bool
	expr_types map[int]Type
	steps      int
	storage    int
	depth      int
}

struct ConstEvalFlow {
	kind  flat.NodeKind = .empty
	value ComptimeConstValue
}

// requires_comptime_const reports an explicit opt-in in a constant initializer.
pub fn (tc &TypeChecker) requires_comptime_const(id flat.NodeId) bool {
	if !tc.has_comptime_functions || !tc.valid_node_id(id) { return false }
	node := tc.a.node(id)
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
	if tc.cross_target_prefs != unsafe { nil } {
		return error('required evaluation needs a concrete target; portable C is not supported')
	}
	file := if source := tc.a.source_files[tc.a.node(id).pos.id] {
		source.name
	} else {
		tc.cur_file
	}
	view := if file == tc.cur_file { tc } else { tc.fork_type_parse_view(file, tc.cur_module) }
	mut e := ConstEval{ tc: view, scopes: [map[string]ComptimeConstValue{}] }
	return e.expr(id)
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

fn const_eval_convert(value ComptimeConstValue, typ Type) !ComptimeConstValue {
	base := unalias_type(typ)
	if base.is_integer() {
		if base is Primitive && base.size > 64 {
			return error('128-bit integers are not supported')
		}
		if !unalias_type(value.typ).is_integer() { return error('unsupported integer conversion') }
		return ComptimeConstValue{ typ: typ, integer: const_eval_integer_cast(value.integer, typ) }
	}
	if (base is Primitive && base.props.has(.boolean)) || base is String || base is ArrayFixed || base is Struct {
		if unalias_type(value.typ).source_type_id() != base.source_type_id() {
			return error('unsupported conversion to `${typ.name()}`')
		}
		return ComptimeConstValue{ ...value, typ: typ }
	}
	return error('unsupported constant type `${typ.name()}`')
}

fn (mut e ConstEval) zero(typ Type) !ComptimeConstValue {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	base := unalias_type(typ)
	if base is Primitive && base.size > 64 { return error('128-bit integers are not supported') }
	if base.is_integer() || (base is Primitive && base.props.has(.boolean)) || base is String {
		return ComptimeConstValue{ typ: typ }
	}
	if base is ArrayFixed {
		n := e.tc.fixed_array_len_value(base) or { return error('unknown fixed-array length') }
		e.reserve(n)!
		mut elements := []ComptimeConstValue{cap: n}
		for _ in 0 .. n { elements << e.zero(base.elem_type)! }
		return ComptimeConstValue{ typ: typ, elements: elements }
	}
	if base is Struct {
		if e.tc.unions[base.name_id] { return error('unions are not supported') }
		$if ownership ? {
			if e.tc.ownership_type_has_explicit_drop(base.name_id) {
				return error('types with implicit cleanup are not supported (`${base.name()}`)')
			}
		}
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
	key := e.tc.const_key_for_name(resolved) or { return error('cannot read runtime state `${name}`') }
	if e.active[key] { return error('cycle in constant `${key}`') }
	id := e.tc.const_exprs[key] or { return error('constant `${key}` has no initializer') }
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
	return e.expr(id)
}

fn (mut e ConstEval) expr(id flat.NodeId) !ComptimeConstValue {
	e.tick()!
	e.depth++
	defer { e.depth-- }
	if e.depth > 128 { return error('evaluation exceeded recursion limit') }
	if !e.tc.valid_node_id(id) { return error('missing constant expression') }
	value := e.expr_value(id)!
	e.expr_types[int(id)] = value.typ
	return value
}

fn (mut e ConstEval) expr_value(id flat.NodeId) !ComptimeConstValue {
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
		.paren, .expr_stmt { return e.expr(e.tc.a.child(node, 0)) }
		.cast_expr {
			return const_eval_convert(e.expr(e.tc.a.child(node, 0))!, e.tc.parse_type(node.source_value_id()))
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
			c_type := e.integer_c_expression_type(id, typ)
			return ComptimeConstValue{ typ: v.typ, integer: const_eval_integer_cast(value, c_type) }
		}
		.infix {
			left := e.expr(e.tc.a.child(node, 0))!
			if node.op == .logical_and && left.integer == 0 {
				return ComptimeConstValue{ typ: bool_ }
			}
			if node.op == .logical_or && left.integer != 0 {
				return ComptimeConstValue{ typ: bool_, integer: 1 }
			}
			right := e.expr(e.tc.a.child(node, 1))!
			return e.binary(id, node.op, left, right)
		}
		.if_expr, .comptime_if {
			branch := e.branch(node)!
			if branch < 0 { return error('conditional has no value') }
			flow := e.scope(e.tc.a.node(branch), 0)!
			if flow.kind != .empty {
				return error('control flow in a value expression is not supported')
			}
			if node.kind == .if_expr {
				typ := e.expr_types[int(id)] or { e.if_result_type(id)! }
				return const_eval_convert(flow.value, typ)
			}
			return flow.value
		}
		.postfix {
			child := e.tc.a.child_node(node, 0)
			if node.op != .not || child.kind != .array_literal {
				return error('unsupported postfix expression')
			}
			if typ := e.tc.expr_type(id) { return e.array_value(child, typ) }
			if child.children_count == 0 { return error('unknown fixed-array type') }
			first := e.expr(e.tc.a.child(child, 0))!
			elem_type := e.tc.intern_type_reference(first.typ)
			typ := Type(ArrayFixed{ elem_type: elem_type, len: child.children_count })
			return e.array_value(child, typ)
		}
		.array_init, .array_literal {
			typ := e.tc.expr_type(id) or {
				*e.tc.parse_type(if node.typ != 0 {
					node.typ
				} else {
					node.source_value_id()
				})
			}
			return e.array_value(node, typ)
		}
		.struct_init {
			typ := e.tc.expr_type(id) or { *e.tc.parse_type(node.source_value_id()) }
			base := unalias_type(typ)
			if base !is Struct { return error('only value structs are supported') }
			mut value := e.zero(typ)!
			fields := e.tc.structs[base.name_id]
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
				value.fields[name] = const_eval_convert(e.expr(e.tc.a.child(field, 0))!, old.typ)!
			}
			return value
		}
		.index {
			container := e.expr(e.tc.a.child(node, 0))!
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
			// Imported constants use the source file's import bindings.
			if e.tc.a.child_node(node, 0).kind == .ident && e.tc.resolve_import_alias(e.tc.a.child_node(node, 0).value) != none {
				alias := e.tc.a.child_node(node, 0).value
				module_name := e.tc.resolve_import_alias(alias) or { return error('unknown module') }
				return e.constant('${module_name}.${node.value}')
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
					typ:     int_
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

fn (mut e ConstEval) array_value(node flat.Node, typ Type) !ComptimeConstValue {
	base := unalias_type(typ)
	if base !is ArrayFixed { return error('only fixed arrays are supported') }
	mut value := e.zero(typ)!
	if node.children_count > value.elements.len { return error('too many array elements') }
	for i in 0 .. node.children_count {
		child := e.tc.a.child(node, i)
		if e.tc.a.node(child).kind == .field_init {
			return error('array init fields are not supported')
		}
		value.elements[i] = const_eval_convert(e.expr(child)!, base.elem_type)!
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

fn (e &ConstEval) integer_c_expression_type(id flat.NodeId, typ Type) Type {
	node := e.tc.a.node(id)
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
		operand := e.integer_c_expression_type(child_id, typ)
		return if const_eval_integer_width(operand) < 32 { Type(i32_) } else { operand }
	}
	if node.kind == .infix && node.op in [.plus, .minus, .mul, .amp, .pipe, .xor] {
		left_id := e.tc.a.child(node, 0)
		right_id := e.tc.a.child(node, 1)
		mut left := e.integer_c_expression_type(left_id, e.expr_types[int(left_id)] or { typ })
		right := e.integer_c_expression_type(right_id, e.expr_types[int(right_id)] or { typ })
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

fn (e &ConstEval) binary_result_type(id flat.NodeId, op flat.Op, left Type, right Type) Type {
	node := e.tc.a.node(id)
	// A compound assignment uses the stored lvalue type, not a pre-check query
	// of its index/selector. Ordinary expressions use their evaluated operands.
	if node.kind != .infix { return left }
	if op == .right_shift_unsigned { return unsigned_shift_result_type(left) }
	if op in [.left_shift, .right_shift] { return left }
	left_id := e.tc.a.child(node, 0)
	right_id := e.tc.a.child(node, 1)
	if promoted := e.tc.int_literal_promoted_infix_type(left_id, right_id, left, right) {
		return promoted
	}
	if promoted := e.tc.int_literal_promoted_infix_type(right_id, left_id, right, left) {
		return promoted
	}
	return infix_integer_promotion_type(left, right) or { left }
}

fn (mut e ConstEval) binary(id flat.NodeId, op flat.Op, left ComptimeConstValue, right ComptimeConstValue) !ComptimeConstValue {
	if unalias_type(left.typ) is String && unalias_type(right.typ) is String {
		if op == .plus {
			e.reserve(left.text.len + right.text.len)!
			return ComptimeConstValue{ typ: string_, text: left.text + right.text }
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
	typ := if op in [.eq, .ne, .lt, .gt, .le, .ge] {
		infix_integer_promotion_type(left.typ, right.typ) or { left.typ }
	} else {
		e.binary_result_type(id, op, left.typ, right.typ)
	}
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
		if r < 0 || r >= const_eval_integer_width(left.typ) {
			return ComptimeConstValue{ typ: typ }
		}
		l = const_eval_integer_cast(l, left.typ)
	}
	if op in [.eq, .ne, .lt, .gt, .le, .ge] {
		left_id := e.tc.a.child(e.tc.a.node(id), 0)
		right_id := e.tc.a.child(e.tc.a.node(id), 1)
		l = e.comparison_operand(left_id, left)
		r = e.comparison_operand(right_id, right)
		mut unsigned := type_id in [flat.source_type_u64, flat.source_type_uint]
		left_base := unalias_type(left.typ)
		right_base := unalias_type(right.typ)
		left_unsigned := const_eval_integer_unsigned(left.typ)
		right_unsigned := const_eval_integer_unsigned(right.typ)
		left_c_type := e.integer_c_expression_type(left_id, left.typ)
		right_c_type := e.integer_c_expression_type(right_id, right.typ)
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
			shift_type := if const_eval_integer_width(left.typ) == 32 {
				flat.source_type_u32
			} else {
				unalias_type(left.typ).source_type_id()
			}
			comptime_enum_unsigned_right_shift(l, r, shift_type)
		}
		else { return error('unsupported integer operation `${op}`') }
	}
	// Keep the C expression's intermediate width until a V storage boundary
	// converts it to its semantic type.
	return ComptimeConstValue{
		typ:     typ
		integer: const_eval_integer_cast(result, e.integer_c_expression_type(id, typ))
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
	mut args := []ComptimeConstValue{}
	for i in 1 .. node.children_count { args << e.expr(e.tc.a.child(node, i))! }
	old_tc := e.tc
	old_scopes := e.scopes
	file := if source := e.tc.a.source_files[f.pos.id] { source.name } else { e.tc.cur_file }
	e.tc = if e.tc.cur_file == file && e.tc.cur_module == decl.mod {
		e.tc
	} else {
		e.tc.fork_type_parse_view(file, decl.mod)
	}
	e.scopes = [map[string]ComptimeConstValue{}]
	defer {
		e.tc = old_tc
		e.scopes = old_scopes
	}
	mut param_index := 0
	for i in 0 .. f.children_count {
		param := e.tc.a.child_node(f, i)
		if param.kind != .param { continue }
		if param_index >= args.len || param.is_mut {
			return error('unsupported arguments to `${f.value}`')
		}
		e.bind(param.value, const_eval_convert(args[param_index], e.tc.parse_type(param.typ))!)
		param_index++
	}
	if param_index != args.len { return error('incorrect argument count') }
	flow := e.sequence(f, 0) or { return error('in `${f.value}`: ${err}') }
	if flow.kind != .return_stmt { return error('`${f.value}` did not return a value') }
	return const_eval_convert(flow.value, e.tc.parse_type(f.typ))
}

fn (e &ConstEval) if_result_type(id flat.NodeId) !Type {
	mut locals := map[string]Type{}
	for scope in e.scopes {
		for name, value in scope { locals[name] = value.typ }
	}
	mut bindings := []LocalBinding{cap: locals.len}
	for name, typ in locals { bindings << LocalBinding{ name: name, typ: typ } }
	mut typing := e.tc.fork_type_parse_view(e.tc.cur_file, e.tc.cur_module)
	return e.if_result_type_in_context(mut typing, id, bindings)
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
	e.scopes << map[string]ComptimeConstValue{}
	defer { e.scopes.delete_last() }
	return e.sequence(node, start)
}

fn (mut e ConstEval) sequence(node flat.Node, start int) !ConstEvalFlow {
	mut result := ConstEvalFlow{}
	for i in start .. node.children_count {
		result = e.stmt(e.tc.a.child(node, i))!
		if result.kind != .empty { return result }
	}
	return result
}

fn (mut e ConstEval) stmt(id flat.NodeId) !ConstEvalFlow {
	e.tick()!
	node := e.tc.a.node(id)
	match node.kind {
		.empty, .param { return ConstEvalFlow{} }
		.block { return e.scope(node, 0) }
		.return_stmt {
			if node.children_count != 1 { return error('only single value returns are supported') }
			return ConstEvalFlow{ kind: .return_stmt, value: e.expr(e.tc.a.child(node, 0))! }
		}
		.if_expr, .comptime_if {
			branch := e.branch(node)!
			return if branch < 0 { ConstEvalFlow{} } else { e.stmt(branch)! }
		}
		.decl_assign, .assign, .index_assign, .selector_assign {
			if node.children_count != 2 { return error('only single assignments are supported') }
			lhs := e.tc.a.child(node, 0)
			mut value := e.expr(e.tc.a.child(node, 1))!
			if node.kind == .decl_assign {
				name := e.tc.a.node(lhs)
				if name.kind != .ident { return error('unsupported local declaration') }
				e.bind(name.value, const_eval_convert(value, value.typ)!)
			} else {
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
					value = e.binary(lhs, op, e.expr(lhs)!, value)!
				}
				e.assign(lhs, value)!
			}
			return ConstEvalFlow{}
		}
		.postfix {
			lhs := e.tc.a.child(node, 0)
			old := e.expr(lhs)!
			if !unalias_type(old.typ).is_integer() || node.op !in [.inc, .dec] {
				return error('unsupported postfix operation')
			}
			e.assign(lhs, ComptimeConstValue{
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
			unsigned := unalias_type(range_type).source_type_id() in [
				flat.source_type_u64,
				flat.source_type_uint,
			]
			limit := const_eval_integer_cast(end.integer, range_type)
			for index := const_eval_integer_cast(start.integer, range_type); if unsigned {
				u64(index) < u64(limit)
			} else {
				index < limit
			}; index = const_eval_integer_cast(i64(u64(index) + 1), range_type) {
				e.tick()!
				e.scopes << map[string]ComptimeConstValue{}
				name := e.tc.a.child_node(node, 0).value
				e.bind(name, ComptimeConstValue{ typ: range_type, integer: index })
				flow := e.sequence(node, header)!
				e.scopes.delete_last()
				if flow.kind == .return_stmt { return flow }
				if flow.kind == .break_stmt { break }
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
			return ConstEvalFlow{ value: e.expr(child)! }
		}
		else { return error('unsupported statement `${node.kind}`') }
	}
}

fn (mut e ConstEval) assign(id flat.NodeId, value ComptimeConstValue) ! {
	node := e.tc.a.node(id)
	if node.kind == .ident {
		for i := e.scopes.len - 1; i >= 0; i-- {
			if old := e.scopes[i][node.value] {
				e.scopes[i][node.value] = const_eval_convert(value, old.typ)!
				return
			}
		}
		return error('cannot mutate runtime state `${node.value}`')
	}
	if node.kind in [.index, .selector] {
		base_id := e.tc.a.child(node, 0)
		mut base := e.expr(base_id)!
		if node.kind == .index {
			index := e.expr(e.tc.a.child(node, 1))!.integer
			if index < 0 || index >= base.elements.len { return error('array index out of bounds') }
			e.reserve(base.elements.len)!
			base = ComptimeConstValue{ ...base, elements: base.elements.clone() }
			base.elements[int(index)] = const_eval_convert(value, base.elements[int(index)].typ)!
		} else {
			old := base.fields[node.value] or { return error('unknown struct field') }
			e.reserve(base.fields.len)!
			base = ComptimeConstValue{ ...base, fields: base.fields.clone() }
			base.fields[node.value] = const_eval_convert(value, old.typ)!
		}
		e.assign(base_id, base)!
		return
	}
	return error('unsupported assignment target')
}
