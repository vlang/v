module types

import os
import v.flat

const const_int_max_work = 48

// const_int_max_names is how many constants deep a value is followed through initializers.
const const_int_max_names = 4

// ConstIntBudget is the number of nodes an evaluation may still visit, counted through the
// initializers of the constants it names, and the first operation of the expression itself that
// it found to overflow, with the width it is checked at.
struct ConstIntBudget {
mut:
	left  int
	names int // constants being followed, one inside another's initializer
	found bool
	site  flat.NodeId
	bits  int
	known map[string]ConstIntVal // by the name as written; `bits` 0 when it has no sure value
}

// ConstIntVal is a constant operand's value with the width and signedness of its type. An
// integer literal that no cast or operation has typed yet is `literal`; a value cast to a type
// that is not built in is `aliased`, and only such a type can declare an operator.
struct ConstIntVal {
	value    i128
	bits     int
	unsigned bool
	literal  bool
	aliased  bool
}

// const_int_val is `value` at a type, or none when the type is not 8 to 64 bits wide or the
// value does not fit it: a cast or a narrow operation would have converted it.
fn const_int_val(value i128, bits int, unsigned bool, literal bool) ?ConstIntVal {
	if bits < 8 || bits > 64 {
		return none
	}
	low := if unsigned { i128(0) } else { -(i128(1) << (bits - 1)) }
	high := if unsigned { i128(1) << bits } else { i128(1) << (bits - 1) }
	if value < low || value >= high {
		return none
	}
	return ConstIntVal{
		value:    value
		bits:     bits
		unsigned: unsigned
		literal:  literal
	}
}

// check_const_int_overflow notes where `-check-overflow` would panic on constant operands.
// The code generator checks `+`, `-` and `*` on built-in integers at the left operand's type
// (`gen_checked_integer_infix` in vlib/v/gen/c/cleanc.v). This reports the same operations on
// signed integers, outside vlib, when both operands are compile-time constants and the
// statement always evaluates the expression. It stays silent whenever it cannot be sure of an
// operand's value, and in a constant's own initializer, which the run-time check does not reach.
fn (mut tc TypeChecker) check_const_int_overflow(id flat.NodeId, node flat.Node) {
	if node.kind != .infix || node.op !in [.plus, .minus, .mul] || node.children_count != 2 {
		return
	}
	// This runs for every `+`, `-` and `*`, so the cheapest tests come first, and the answer
	// for a file is kept. The standard library and its tests rely on wrapping arithmetic.
	if tc.cur_file != tc.const_overflow_file {
		tc.const_overflow_file = tc.cur_file
		path := os.real_path(tc.cur_file).replace('\\', '/')
		vlib := os.real_path(os.join_path(tc.compiler_vroot, 'vlib')).replace('\\', '/') + '/'
		in_vlib := if tc.compiler_vroot == '' {
			path.contains('/vlib/')
		} else {
			path.starts_with(vlib)
		}
		tc.const_overflow_skip = in_vlib
			&& (tc.cur_module != 'main' || tc.cur_file.ends_with('_test.v'))
	}
	if tc.const_overflow_skip {
		return
	}
	// An operand's operators were here just before this one, so the answer for the last of
	// them is kept and a long expression is not walked again at each of its operators.
	left := tc.const_int_shape(id, const_int_max_work)
	if left > 0 {
		// Short, and only names of constants and forms that can be constant. The outermost
		// of a run of constant arithmetic is evaluated, once: an enclosing `+`, `-` or `*`
		// either is that one or evaluates this operand when it comes to it.
		if !tc.const_int_inside_arithmetic(id) {
			tc.note_const_int_parts(id)
		}
	} else {
		for i in 0 .. 2 {
			operand := tc.const_int_arithmetic_operand(tc.a.child(node, i))
			if tc.valid_node_id(operand) && tc.const_int_shape(operand, const_int_max_work) > 0 {
				tc.note_const_int_parts(operand)
			}
		}
	}
	tc.const_overflow_last = int(id)
	tc.const_overflow_last_size = if left > 0 { const_int_max_work - left } else { 0 }
}

// const_int_inside_arithmetic is true when the expression is an operand of `+`, `-` or `*`,
// directly or through parentheses, a unary operator or a cast.
fn (tc &TypeChecker) const_int_inside_arithmetic(id flat.NodeId) bool {
	mut inner := id
	mut above := tc.direct_parent_id(id)
	for tc.valid_node_id(above) {
		outer := tc.a.node(above)
		if outer.children_count == 0 || tc.a.child(outer, 0) != inner {
			return outer.kind == .infix && outer.op in [.plus, .minus, .mul]
				&& outer.children_count == 2 && tc.a.child(outer, 1) == inner
		}
		if outer.kind !in [.paren, .prefix, .cast_expr] {
			return outer.kind == .infix && outer.op in [.plus, .minus, .mul]
				&& outer.children_count == 2
		}
		inner = above
		above = tc.direct_parent_id(above)
	}
	return false
}

// const_int_arithmetic_operand is the `+`, `-` or `*` that an operand is, directly or inside
// parentheses, a unary operator or a cast: the way `const_int_inside_arithmetic` climbs.
fn (tc &TypeChecker) const_int_arithmetic_operand(id flat.NodeId) flat.NodeId {
	mut inner := id
	for _ in 0 .. const_int_max_work {
		if !tc.valid_node_id(inner) {
			break
		}
		node := tc.a.node(inner)
		if node.kind == .infix && node.op in [.plus, .minus, .mul] && node.children_count == 2 {
			return inner
		}
		if node.kind !in [.paren, .prefix, .cast_expr] || node.children_count == 0 {
			break
		}
		inner = tc.a.child(node, 0)
	}
	return flat.NodeId(-1)
}

// note_const_int_parts evaluates a `+`, `-` or `*` of constant shape, and where it cannot be
// sure of the whole (an operand it refuses, a local named like a constant), each operand that
// is such an operation, so that one operand does not hide an overflow in the other.
fn (mut tc TypeChecker) note_const_int_parts(id flat.NodeId) {
	if tc.note_const_int_overflow(id) {
		return
	}
	node := tc.a.node(id)
	for i in 0 .. node.children_count {
		operand := tc.const_int_arithmetic_operand(tc.a.child(node, i))
		if tc.valid_node_id(operand) {
			tc.note_const_int_parts(operand)
		}
	}
}

// note_const_int_overflow evaluates an expression of constants and notes its first operation
// that overflows, when its statement always evaluates it. It is false when it could not be
// sure of the expression as a whole.
fn (mut tc TypeChecker) note_const_int_overflow(id flat.NodeId) bool {
	if !tc.should_diagnose_notice(id) || tc.node_is_in_translated_file(id)
		|| tc.fn_context.generic_params.len > 0 || tc.type_param_texts.len > 0 {
		return true
	}
	mut budget := ConstIntBudget{
		left: const_int_max_work
	}
	mut sure := true
	tc.const_int_sure(id, true, tc.cur_file, true, mut budget) or { sure = false }
	if !budget.found {
		return sure
	}
	if !tc.const_int_certainly_evaluated(budget.site) {
		return true
	}
	// The checker's type of the left operand has to be the one the evaluation arrived at.
	site := tc.a.node(budget.site)
	lhs_type := tc.resolve_type(tc.a.child(site, 0))
	range := integer_type_range(lhs_type) or { return true }
	if range.is_unsigned || range.bits != budget.bits || unalias_type(lhs_type) is Rune {
		return true
	}
	// Last, because it costs more the more locals a function has: no name in the expression
	// is a local that shadows a constant.
	if tc.expr_names_visible_local(id) {
		return false
	}
	tc.record_notice_at(.assignment_mismatch, 'constant integer expression overflows `${range.name_text()}`',
		budget.site, site.pos)
	return true
}

// const_int_certainly_evaluated is true when every node between the expression and its
// function is one that evaluates its operands each time it runs. An arm of `if`,
// `match`, `or`, `&&` or `||`, a loop body, a call argument (the call may be compiled
// out, or run its argument as a loop body), `typeof`, `sizeof`, a struct field default, a
// constant's initializer and anything else may never be evaluated, or is not arithmetic at run
// time. The other operands and values on the way have to be plain (`const_int_plain`), since
// one that calls, or that the run-time check stops, leaves first. A block has to be a statement
// of its own: one used as a value (`x := 1 + unsafe { f()!; … }`, a `$if` branch) may be left
// by an earlier statement of its own.
// Generic functions and functions tagged `@[ignore_overflow]` are not checked at run time.
fn (tc &TypeChecker) const_int_certainly_evaluated(id flat.NodeId) bool {
	mut plain := ConstIntBudget{
		left: const_int_max_work
	}
	mut child := id
	mut parent := tc.direct_parent_id(id)
	mut in_block := false
	for tc.valid_node_id(parent) {
		decl := tc.a.node(parent)
		if in_block && decl.kind !in [.block, .expr_stmt, .fn_decl] {
			return false
		}
		match decl.kind {
			.fn_decl {
				return decl.generic_params().len == 0
					&& tc.enclosing_generic_params_by_node[int(parent)].len == 0
					&& !tc.a.specialized_fn_nodes[int(parent)]
					&& !tc.declaration_has_attribute(parent, 'ignore_overflow')
			}
			.infix, .decl_assign, .assign, .return_stmt {
				// `&&` and `||` may skip an operand. A `static` local is initialized once, in
				// a C static initializer. Another operand or value runs first and may leave
				// (`f()! + …`, `a, b := f()!, …`).
				if decl.op in [.logical_and, .logical_or] || decl.value == 'static' {
					return false
				}
				for i in 0 .. decl.children_count {
					other := tc.a.child(decl, i)
					if other != child && !tc.const_int_plain(other, mut plain) {
						return false
					}
					// With several values, one whose target is `_` is dropped unevaluated.
					if decl.children_count > 2 && tc.a.node(other).kind == .ident
						&& tc.a.node(other).value == '_' {
						return false
					}
				}
			}
			.paren, .prefix, .cast_expr, .expr_stmt {}
			.block {
				// Only a written block: the parser also groups `a, b` and the parts of a
				// `for` header in blocks, where an earlier part may leave.
				if decl.value !in ['', 'unsafe'] {
					return false
				}
				in_block = true
			}
			else {
				return false
			}
		}
		child = parent
		parent = tc.direct_parent_id(parent)
	}
	return false
}

// const_int_sure is the value of an operand when this function can be sure of it. It evaluates
// only integer literals, parentheses, unary minus, integer casts, `+`, `-` and `*` on signed
// values, and, with `names`, a constant named directly or through an imported module, by its
// initializer. Types come from what is written (a literal, a cast's type, a constant's declared
// type): the checker's type of a node in a constant's initializer would be read in the scope of
// the code that uses the constant. In the expression itself (`direct`) a cast may name any
// integer type; in an initializer only a built-in one, since an import or an alias may give a
// name another meaning there. Every part has to fit its type, and an operation follows the
// run-time rule: a literal on the left is an `int`, the right operand fits the left one's type,
// and so does the result. The first operation of the expression itself whose result does not
// is recorded in `budget`. A global, a field of a constant struct, `sizeof`, `~`, shifts,
// `rune`, `isize` and `usize` (their width follows the target), a literal with a leading zero,
// a constant of an alias type or of a translated file and everything else are refused.
// A constant of another module may hold no names: they would be read here.
fn (tc &TypeChecker) const_int_sure(id flat.NodeId, names bool, file string, direct bool, mut budget ConstIntBudget) ?ConstIntVal {
	budget.left--
	if budget.left < 0 || !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	match node.kind {
		.int_literal {
			if node.value.len > 1 && node.value[0] == `0`
				&& (node.value[1].is_digit() || node.value[1] == `_`) {
				return none
			}
			return ConstIntVal{
				value:   i128(v_int_literal_value(node.value)?)
				bits:    64
				literal: true
			}
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return tc.const_int_sure(tc.a.child(node, 0), names, file, direct, mut budget)
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			inner := tc.const_int_sure(tc.a.child(node, 0), names, file, direct, mut budget)?
			name := node.value_text()
			if name in ['i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64'] {
				return const_int_val(inner.value, name[1..].int(), name[0] == `u`, false)
			}
			if name == 'int' {
				return const_int_val(inner.value, platform_int_bits(), false, false)
			}
			if !direct {
				return none
			}
			typ := tc.resolve_type(id)
			clean := unalias_type(typ)
			if clean is Rune || clean is ISize || clean is USize {
				return none
			}
			range := integer_type_range(typ)?
			return ConstIntVal{
				...const_int_val(inner.value, range.bits, range.is_unsigned, false)?
				aliased: true
			}
		}
		.prefix {
			if node.op != .minus || node.children_count == 0 {
				return none
			}
			inner := tc.const_int_sure(tc.a.child(node, 0), names, file, direct, mut budget)?
			return ConstIntVal{
				...const_int_val(-inner.value, inner.bits, inner.unsigned, inner.literal)?
				aliased: inner.aliased
			}
		}
		.infix {
			if node.op !in [.plus, .minus, .mul] || node.children_count != 2 {
				return none
			}
			left := tc.const_int_sure(tc.a.child(node, 0), names, file, direct, mut budget)?
			right := tc.const_int_sure(tc.a.child(node, 1), names, file, direct, mut budget)?
			if left.unsigned || right.unsigned {
				return none
			}
			if left.aliased {
				left_type := tc.resolve_type(tc.a.child(node, 0))
				if tc.type_has_infix_operator_method(&left_type, node.op) {
					return none
				}
			}
			bits := if left.literal { platform_int_bits() } else { left.bits }
			const_int_val(left.value, bits, false, false)?
			const_int_val(right.value, bits, false, false)?
			value := match node.op {
				.plus { left.value + right.value }
				.minus { left.value - right.value }
				else { left.value * right.value }
			}
			const_int_val(value, bits, false, false) or {
				if direct && !budget.found {
					budget.found = true
					budget.site = id
					budget.bits = bits
				}
				return none
			}
			// The result has the wider type, or the typed operand's when the other is a
			// literal; two literals stay a literal.
			if left.literal && right.literal {
				return ConstIntVal{
					value:   value
					bits:    64
					literal: true
				}
			}
			typed := if left.literal { right } else { left }
			wider := if left.literal != right.literal || left.bits > right.bits {
				typed.bits
			} else {
				right.bits
			}
			return ConstIntVal{
				...const_int_val(value, wider, false, false)?
				aliased: left.aliased || right.aliased
			}
		}
		.ident, .selector {
			if !names {
				return none
			}
			// One expression often names a constant many times: the first answer is kept.
			name := if !direct {
				''
			} else if node.kind == .ident {
				node.value
			} else {
				tc.source_text_for_node(id)
			}
			if name != '' {
				if known := budget.known[name] {
					return if known.bits == 0 { none } else { known }
				}
			}
			value := tc.const_int_named(id, file, mut budget) or { ConstIntVal{} }
			if name != '' {
				budget.known[name] = value
			}
			return if value.bits == 0 { none } else { value }
		}
		else {
			return none
		}
	}
}

// const_int_named is the value of a constant named directly or through an imported module, by
// its initializer: see `const_int_sure`.
fn (tc &TypeChecker) const_int_named(id flat.NodeId, file string, mut budget ConstIntBudget) ?ConstIntVal {
	if budget.names >= const_int_max_names {
		return none
	}
	node := tc.a.node(id)
	mut key := ''
	if node.kind == .ident {
		if node.value in tc.global_names && !tc.bare_name_is_const_owned_over_global(node.value) {
			return none
		}
		key = tc.const_key_for_name(node.value)?
		// A constant of this module comes before one of `builtin` with the same name.
		own := tc.qualify_name(node.value)
		if own != key && own in tc.const_types {
			return none
		}
	} else {
		base := tc.a.child_node(node, 0)
		if node.children_count != 1 || base.kind != .ident || base.value in tc.global_names
			|| base.value in tc.const_exprs || tc.qualify_name(base.value) in tc.const_exprs {
			return none
		}
		owner := tc.file_imports[file_import_key(file, base.value)] or { return none }
		key = '${owner}.${node.value}'
	}
	initializer := tc.const_exprs[key] or { return none }
	typ := tc.const_types[key] or { return none }
	clean := unalias_type(typ)
	if *clean != typ || clean is Rune || clean is ISize || clean is USize
		|| tc.node_is_in_translated_file(initializer) {
		// An alias may declare the operator; a translated file follows C's types.
		return none
	}
	range := integer_type_range(typ)?
	// Names in the initializer are read in the file that declares the constant, and only when
	// it is of this module.
	same_module := node.kind == .ident && (tc.const_modules[key] or { '' }) == tc.cur_module
	budget.names++
	inner := tc.const_int_sure(initializer, same_module, tc.const_files[key] or { file }, false, mut
		budget)
	budget.names--
	return const_int_val(inner?.value, range.bits, range.is_unsigned, false)
}

// const_int_plain is true for an operand or value that cannot leave its statement before the
// others are evaluated: a name, a field of a name or a literal, in parentheses or a cast, or
// constant arithmetic that is itself sure not to overflow. Anything else may: a call, `!`, `?`,
// `or`, a declared operator (also behind a pointer, or reached by `==` on an array of structs),
// a channel receive, a division by zero, or arithmetic that the run-time check stops first.
fn (tc &TypeChecker) const_int_plain(id flat.NodeId, mut budget ConstIntBudget) bool {
	if tc.const_int_leaf(id) {
		return true
	}
	mut sure := ConstIntBudget{
		left: budget.left
	}
	tc.const_int_sure(id, true, tc.cur_file, true, mut sure) or { return false }
	budget.left = sure.left
	return !sure.found && !tc.expr_names_visible_local(id)
}

// const_int_leaf is true for a name, a field of a name or a literal, in parentheses or a cast.
fn (tc &TypeChecker) const_int_leaf(id flat.NodeId) bool {
	mut inner := id
	for _ in 0 .. const_int_max_work {
		if !tc.valid_node_id(inner) {
			return false
		}
		node := tc.a.node(inner)
		match node.kind {
			.ident, .int_literal, .float_literal, .bool_literal, .char_literal {
				return true
			}
			.selector {
				return node.children_count == 1 && tc.a.child_node(node, 0).kind == .ident
			}
			.paren, .cast_expr {
				if node.children_count == 0 {
					return false
				}
				inner = tc.a.child(node, 0)
			}
			else {
				return false
			}
		}
	}
	return false
}

// const_int_shape is what is left of `budget` after the nodes of an expression, or 0 when it
// holds a local, a form that `const_int_sure` refuses, or too many nodes. It builds no strings.
fn (tc &TypeChecker) const_int_shape(id flat.NodeId, budget int) int {
	if budget <= 0 || !tc.valid_node_id(id) {
		return 0
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		// Most arithmetic is on locals: a name that no constant has ends the scan there.
		if tc.const_expr_short_names_count == tc.const_exprs.len
			&& node.value !in tc.const_expr_short_names {
			return 0
		}
		return budget - 1
	}
	if node.kind == .selector {
		// `m.c` of an imported module only: a field of a local ends the scan with one lookup.
		base := tc.a.child_node(node, 0)
		if node.children_count != 1 || base.kind != .ident {
			return 0
		}
		if owner := tc.cur_scope.lookup_owner_id(base.name_id()) {
			if !tc.binding_owner_is_file_scope(owner) {
				return 0
			}
		}
		return budget - 1
	}
	if node.kind == .int_literal {
		return budget - 1
	}
	if node.kind !in [.paren, .prefix, .cast_expr, .infix] {
		return 0
	}
	if int(id) == tc.const_overflow_last {
		left := budget - tc.const_overflow_last_size
		return if tc.const_overflow_last_size > 0 && left > 0 { left } else { 0 }
	}
	mut left := budget - 1
	for i in 0 .. node.children_count {
		left = tc.const_int_shape(tc.a.child(node, i), left)
		if left <= 0 {
			return 0
		}
	}
	return left
}
