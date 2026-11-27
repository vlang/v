module types

import v.flat

// ShiftCountValue keeps unsigned values above max_i64 distinct from negative counts.
struct ShiftCountValue {
	bits     u128
	negative bool
}

fn (tc &TypeChecker) shift_count_value(id flat.NodeId, depth int, initializer string) ?ShiftCountValue {
	if depth > 32 || !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	match node.kind {
		.int_literal {
			return ShiftCountValue{ bits: shift_count_literal_magnitude(node.value)? }
		}
		.char_literal {
			return ShiftCountValue{ bits: u128(match_char_literal_value(node.value)?) }
		}
		.ident {
			mut key := node.value
			if initializer != '' {
				mod := tc.const_modules[initializer] or { tc.cur_module }
				if mod !in ['', 'main', 'builtin'] {
					qualified := '${mod}.${node.value}'
					if qualified in tc.const_exprs {
						key = qualified
					} else if !tc.bare_const_key_is_visible_in_module(key, mod) {
						return none
					}
				}
			} else {
				if owner := tc.cur_scope.lookup_owner_id(node.name_id()) {
					if !tc.binding_owner_is_file_scope(owner) {
						return none
					}
				}
				key = tc.const_key_for_name(node.value)?
			}
			return tc.named_shift_count_value(key, depth + 1)
		}
		.selector {
			if unalias_type(tc.resolve_type(id)) is Enum {
				return tc.enum_shift_count_value(id, depth + 1)
			}
			if node.children_count != 1 {
				return none
			}
			base := tc.a.child_node(node, 0)
			if base.kind != .ident {
				return none
			}
			file := if initializer == '' {
				tc.cur_file
			} else {
				tc.const_files[initializer] or { tc.cur_file }
			}
			mod := tc.file_imports[file_import_key(file, base.value)] or { return none }
			return tc.named_shift_count_value('${mod}.${node.value}', depth + 1)
		}
		.enum_val {
			return tc.enum_shift_count_value(id, depth + 1)
		}
		.if_expr {
			return tc.shift_count_value(tc.constant_if_selected_tail(node)?, depth + 1, initializer)
		}
		.comptime_if {
			take_then := tc.comptime_type_condition_value(node.value)?
			index := if take_then { 0 } else { 1 }
			if index >= node.children_count { return none }
			return tc.shift_count_value(tc.branch_tail_expr_id(tc.a.child(node, index)), depth + 1, initializer)
		}
		.paren, .cast_expr, .prefix {
			if node.children_count != 1 {
				return none
			}
			value := tc.shift_count_value(tc.a.child(node, 0), depth + 1, initializer)?
			if node.kind == .paren {
				return value
			}
			if node.kind == .prefix && node.op == .minus && tc.a.child_node(node, 0).kind == .int_literal {
				return ShiftCountValue{
					bits:     if value.bits == 0 {
						u128(0)
					} else {
						~value.bits + 1
					}
					negative: value.bits != 0
				}
			}
			range := shift_count_type_range(tc.resolve_type(id))?
			if range.bits <= 0 || range.bits > 128 {
				return none
			}
			if node.kind == .cast_expr || node.op == .plus {
				return shift_count_from_bits(value.bits, range)
			}
			if node.op == .minus {
				bits := if value.bits == 0 { u128(0) } else { ~value.bits + 1 }
				return shift_count_from_bits(bits, range)
			}
			if node.op == .bit_not {
				return shift_count_from_bits(~value.bits, range)
			}
			return none
		}
		.infix {
			if node.children_count != 2 {
				return none
			}
			left := tc.shift_count_value(tc.a.child(node, 0), depth + 1, initializer)?
			right := tc.shift_count_value(tc.a.child(node, 1), depth + 1, initializer)?
			range := shift_count_type_range(tc.resolve_type(id))?
			if range.bits <= 0 || range.bits > 128 {
				return none
			}
			bits := shift_count_infix_bits(node.op, left, right, range)?
			return shift_count_from_bits(bits, range)
		}
		else {
			return none
		}
	}
}

fn (tc &TypeChecker) named_shift_count_value(key string, depth int) ?ShiftCountValue {
	value := tc.shift_count_value(tc.const_exprs[key] or { return none }, depth, key)?
	typ := tc.const_types[key] or { return none }
	range := shift_count_type_range(tc.const_type_from_initializer(key, typ))?
	if range.bits <= 0 || range.bits > 128 {
		return none
	}
	return shift_count_from_bits(value.bits, range)
}

fn (tc &TypeChecker) enum_shift_count_value(id flat.NodeId, depth int) ?ShiftCountValue {
	typ := unalias_type(tc.resolve_type(id))
	if typ !is Enum {
		return none
	}
	node := tc.a.node(id)
	text := if node.kind == .enum_val {
		'${typ.name()}.${node.value}'
	} else {
		tc.source_text_for_node(id)
	}
	range := shift_count_type_range(tc.inline_asm_enum_backing_type(typ.source_type_id()))?
	// The older enum evaluator returns int and cannot retain 128-bit fields.
	if range.bits == 128 {
		return tc.enum_shift_count_value_wide(typ.source_type_id(), node.value, range, depth)
	}
	value := tc.const_int_enum_selector_value(text)?
	return shift_count_from_bits(u128(value), range)
}

fn (tc &TypeChecker) enum_shift_count_value_wide(enum_type u32, field_name string, range IntegerTypeRange, depth int) ?ShiftCountValue {
	if depth > 32 { return none }
	enum_name := tc.resolve_enum_name(enum_type)?
	indexes := tc.top_level_check_decl_indexes()
	if isnil(indexes) || !indexes.enum_decls_complete { return none }
	decl := indexes.enum_decls[enum_name] or { return none }
	node := tc.a.nodes[decl.idx]
	file := if source := tc.a.source_files[node.pos.id] { source.name } else { tc.cur_file }
	view := if tc.cur_file == file && tc.cur_module == decl.module_name {
		tc
	} else {
		tc.fork_type_parse_view(file, decl.module_name)
	}
	mut value := ShiftCountValue{}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .enum_field { continue }
		if field.children_count > 0 {
			value = view.shift_count_value(tc.a.child(field, 0), depth + 1, '')?
		}
		if field.value == field_name {
			return shift_count_from_bits(value.bits, range)
		}
		value = shift_count_from_bits(shift_count_infix_bits(.plus, value, ShiftCountValue{ bits: 1 }, range)?, range)
	}
	return none
}

// shift_count_type_range is `integer_type_range` with `isize` and `usize` at the
// target's width, so a count computed in them wraps where the program wraps.
fn shift_count_type_range(typ Type) ?IntegerTypeRange {
	range := integer_type_range(typ)?
	clean := unalias_type(typ)
	if clean is ISize || clean is USize {
		return IntegerTypeRange{
			...range
			bits: platform_int_bits()
		}
	}
	return range
}

fn shift_count_from_bits(bits u128, range IntegerTypeRange) ShiftCountValue {
	mask := if range.bits >= 128 { (~u128(0)) } else { (u128(1) << range.bits) - 1 }
	low := bits & mask
	negative := !range.is_unsigned && low >> (range.bits - 1) != 0
	return ShiftCountValue{
		bits:     if negative { low | ~mask } else { low }
		negative: negative
	}
}

fn (value ShiftCountValue) magnitude() u128 {
	return if value.negative { ~value.bits + 1 } else { value.bits }
}

// Constant arithmetic follows V's wrapping semantics before the shift is checked.
@[ignore_overflow]
fn shift_count_infix_bits(op flat.Op, left ShiftCountValue, right ShiftCountValue, range IntegerTypeRange) ?u128 {
	match op {
		.plus { return left.bits + right.bits }
		.minus { return left.bits - right.bits }
		.mul { return left.bits * right.bits }
		.div, .mod {
			divisor := right.magnitude()
			if divisor == 0 {
				return none
			}
			magnitude := if op == .div {
				left.magnitude() / divisor
			} else {
				left.magnitude() % divisor
			}
			negative := if op == .div { left.negative != right.negative } else { left.negative }
			return if negative && magnitude != 0 { ~magnitude + 1 } else { magnitude }
		}
		.power {
			if right.negative {
				return none
			}
			mut result := u128(1)
			mut base := left.bits
			mut exponent := right.bits
			for exponent != 0 {
				if exponent & 1 != 0 {
					result *= base
				}
				exponent >>= 1
				if exponent != 0 {
					base *= base
				}
			}
			return result
		}
		.amp { return left.bits & right.bits }
		.pipe { return left.bits | right.bits }
		.xor { return left.bits ^ right.bits }
		.left_shift, .right_shift, .right_shift_unsigned {
			if right.negative || right.bits >= u128(range.bits) {
				return none
			}
			if op == .left_shift {
				return left.bits << right.bits
			}
			if op == .right_shift && left.negative {
				magnitude := ((left.magnitude() - 1) >> right.bits) + 1
				return ~magnitude + 1
			}
			mask := if range.bits >= 128 { (~u128(0)) } else { (u128(1) << range.bits) - 1 }
			return (left.bits & mask) >> right.bits
		}
		else { return none }
	}
}

fn shift_count_literal_magnitude(text string) ?u128 {
	if value := comptime_enum_integer_literal(text) {
		return u128(u64(value))
	}
	mut clean := text.to_lower().replace('_', '')
	for suffix in ['u128', 'i128', 'u64', 'i64', 'u32', 'i32', 'u16', 'i16', 'u8', 'i8'] {
		if clean.ends_with(suffix) {
			clean = clean[..clean.len - suffix.len]
			break
		}
	}
	mut base := u128(10)
	mut start := 0
	if clean.starts_with('0x') {
		base = 16
		start = 2
	} else if clean.starts_with('0o') {
		base = 8
		start = 2
	} else if clean.starts_with('0b') {
		base = 2
		start = 2
	}
	if clean.len == start {
		return none
	}
	mut result := u128(0)
	for ch in clean[start..] {
		digit := if ch >= `0` && ch <= `9` {
			u128(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			u128(ch - `a` + 10)
		} else {
			return none
		}
		if digit >= base || result > ((~u128(0)) - digit) / base {
			return none
		}
		result = result * base + digit
	}
	return result
}

fn (mut tc TypeChecker) check_instantiated_const_shift_count(instantiation GenericCompileErrorInstantiation) {
	// A concrete body probe owned by another check must not start a second walk.
	// Its ordinary call site will schedule this check separately.
	if tc.valid_diagnostic_fast
		|| (tc.type_param_texts.len > 0 && !tc.const_shift_generic_active)
		|| !tc.generic_args_are_concrete(instantiation.concrete_args.ids) {
		return
	}
	instance_key := '${int(instantiation.decl_id)}:${instantiation.concrete_args.ids.map(it.str()).join(',')}'
	if instance_key in tc.const_shift_generic_checked
		|| !tc.generic_body_has_const_shift(instantiation.decl_id) {
		return
	}
	// An instance is either pending or complete. Nested calls enqueue work in the
	// body checker; its owner drains that work after the fork returns.
	tc.const_shift_generic_checked[instance_key] = true
	tc.const_shift_generic_pending << instantiation
	if tc.const_shift_generic_active {
		return
	}
	tc.const_shift_generic_active = true
	defer { tc.const_shift_generic_active = false }
	for tc.const_shift_generic_pending.len > 0 {
		next := tc.const_shift_generic_pending.pop()
		tc.check_const_shift_generic_instance(next)
	}
}

fn (mut tc TypeChecker) check_const_shift_generic_instance(instantiation GenericCompileErrorInstantiation) {
	mut argument_view := tc.fork_type_parse_view('', 'main')
	concrete_args := instantiation.concrete_args.ids.map(argument_view.explicit_generic_concrete_arg_text(it))
	mut texts := map[u32]u32{}
	for i, name in instantiation.generic_params {
		if i >= concrete_args.len {
			return
		}
		texts[name] = concrete_args[i]
	}
	mut w := tc.fork_for_parallel_check()
	w.valid_diagnostic_fast = false
	w.valid_resolution_fast = false
	w.cur_file = instantiation.decl_file
	w.cur_module = tc.file_modules[instantiation.decl_file] or { tc.cur_module }

	if w.diagnostic_files.len > 0 {
		w.diagnostic_files = w.diagnostic_files.clone()
		w.diagnostic_files[instantiation.decl_file] = true
	}
	checked := w.checked_generic_fn_body(tc.a.node(instantiation.decl_id), int(instantiation.decl_id),
		texts, false)
	// The fork owns only newly discovered work. Its inherited instance set also
	// includes the owner's pending work, so a diamond cannot enqueue it twice.
	tc.const_shift_generic_pending << checked.const_shift_generic_pending
	for key, _ in checked.const_shift_generic_checked {
		tc.const_shift_generic_checked[key] = true
	}
	for key, found in checked.const_shift_generic_bodies {
		tc.const_shift_generic_bodies[key] = found
	}

	for diagnostic in checked.errors {
		if diagnostic.msg.starts_with('shift count for type ') || diagnostic.msg == 'invalid negative shift count' {
			tc.record_error_at(.assignment_mismatch, diagnostic.msg, diagnostic.node, diagnostic.pos)
		}
	}
}

// prepare_const_shift_generic_callees indexes the declarations that calls can reach,
// including methods whose type parameters come from a generic receiver.
fn (mut tc TypeChecker) prepare_const_shift_generic_callees() {
	if tc.const_shift_generic_index_len == tc.top_level_idx.len {
		return
	}
	// Build a new snapshot. Forks only read the published name index.
	mut callees := map[string][]flat.NodeId{}
	for idx in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(idx))
		if node.kind != .fn_decl
			|| (node.generic_params().len == 0 && !tc.decl_receiver_may_carry_generic_params(node)) {
			continue
		}
		callees[node.value.all_after_last('.')] << flat.NodeId(idx)
		if node.is_static_type_method() {
			if _, method := flat.decode_static_type_method_name(node.value) {
				callees[method] << flat.NodeId(idx)
			}
		}
	}
	tc.const_shift_generic_names = callees
	tc.const_shift_generic_index_len = tc.top_level_idx.len
	tc.const_shift_generic_bodies = map[int]bool{}
}

// generic_body_has_const_shift follows named generic calls to a shift or compound
// assignment whose count the constant evaluator can follow.
fn (mut tc TypeChecker) generic_body_has_const_shift(root flat.NodeId) bool {
	tc.prepare_const_shift_generic_callees()
	if found := tc.const_shift_generic_bodies[int(root)] {
		return found
	}
	mut seen := map[int]bool{}
	mut callers := map[int][]int{}
	mut pending := [root]
	mut relevant := []int{}
	for pending.len > 0 {
		decl := pending.pop()
		if seen[int(decl)] {
			continue
		}
		seen[int(decl)] = true
		if found := tc.const_shift_generic_bodies[int(decl)] {
			if found { relevant << int(decl) }
			continue
		}
		mut stack := [decl]
		for stack.len > 0 {
			id := stack.pop()
			if !tc.valid_node_id(id) {
				continue
			}
			node := tc.a.node(id)
			if tc.const_shift_candidate(node) {
				relevant << int(decl)
				break
			}
			if node.kind == .call {
				mut callees := []flat.NodeId{}
				tc.const_shift_generic_callees(node, mut callees)
				for callee in callees {
					callers[int(callee)] << int(decl)
					pending << callee
				}
			}
			for i in 0 .. node.children_count {
				stack << tc.a.child(node, i)
			}
		}
	}
	// Propagate relevance back through the visited graph, including cycles, before
	// caching negative answers. Each declaration is scanned once per snapshot.
	mut propagated := map[int]bool{}
	for relevant.len > 0 {
		decl := relevant.pop()
		if propagated[decl] { continue }
		propagated[decl] = true
		tc.const_shift_generic_bodies[decl] = true
		relevant << callers[decl]
	}
	for decl, _ in seen {
		if decl !in tc.const_shift_generic_bodies {
			tc.const_shift_generic_bodies[decl] = false
		}
	}
	return tc.const_shift_generic_bodies[int(root)]
}

fn (tc &TypeChecker) const_shift_candidate(node flat.Node) bool {
	if node.kind == .infix && node.op in [.left_shift, .right_shift, .right_shift_unsigned]
		&& node.children_count == 2 {
		return tc.shift_count_operand_shape(tc.a.child(node, 1), 0)
	}
	if node.kind == .assign
		&& node.op in [.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
		for i := 1; i < node.children_count; i += 2 {
			if tc.shift_count_operand_shape(tc.a.child(node, i), 0) {
				return true
			}
		}
	}
	return false
}

// shift_count_operand_shape reports whether a shift count could be known at
// compile time: it uses only literals, casts, enum values, names of constants and
// conditions, with parentheses, signs and arithmetic.
fn (tc &TypeChecker) shift_count_operand_shape(id flat.NodeId, depth int) bool {
	if depth > 32 || !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	return match node.kind {
		.int_literal, .char_literal, .enum_val, .selector, .if_expr, .comptime_if {
			true
		}
		.ident {
			tc.const_expr_short_names_count != tc.const_exprs.len
				|| node.value in tc.const_expr_short_names
		}
		.paren, .prefix, .cast_expr, .infix {
			if node.children_count == 0 {
				return false
			}
			for i in 0 .. node.children_count {
				if !tc.shift_count_operand_shape(tc.a.child(node, i), depth + 1) {
					return false
				}
			}
			true
		}
		else {
			false
		}
	}
}

// const_shift_generic_callees adds the declarations of the generic functions that
// a call may reach by its name.
fn (tc &TypeChecker) const_shift_generic_callees(node flat.Node, mut pending []flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	mut callee := tc.a.child_node(node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind !in [.ident, .selector] || callee.value == '' {
		return
	}
	pending << tc.const_shift_generic_names[callee.value.all_after_last('.')]
}
