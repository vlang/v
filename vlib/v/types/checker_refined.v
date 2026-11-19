module types

import v.flat
import v.token

// RefinedRange describes the integer interval a `@[range: ...]` refinement
// type accepts. Bounds are decimal magnitudes so the full `u64` span fits.
pub struct RefinedRange {
pub mut:
	raw_text     string // range attribute text, repeated in diagnostics
	lo_mag       string // decimal magnitude without leading zeros ('' when unbounded)
	hi_mag       string
	lo_negative  bool
	hi_negative  bool
	has_lo       bool
	has_hi       bool
	lo_exclusive bool
	hi_exclusive bool
	exclude_zero bool
}

// parse_refined_range parses a `range` attribute value: `lo..hi` (inclusive),
// `lo..<hi` (exclusive upper), `lo<..hi` (exclusive lower), open ends such as
// `1..` or `..10`, or `nonzero`.
pub fn parse_refined_range(text string) ?RefinedRange {
	raw := text.trim_space()
	if raw == 'nonzero' {
		return RefinedRange{
			raw_text:     text
			exclude_zero: true
		}
	}
	sep := raw.index('..') or { return none }
	left := raw[..sep].trim_space()
	right := raw[sep + 2..].trim_space()
	if left.contains('..') || right.contains('..') {
		return none
	}
	mut bound := RefinedRange{
		raw_text: text
		has_lo:   true
		has_hi:   true
	}
	mut lo := left
	mut hi := right
	if lo.ends_with('<') {
		bound.lo_exclusive = true
		lo = lo[..lo.len - 1].trim_space()
	}
	if hi.starts_with('<') {
		bound.hi_exclusive = true
		hi = hi[1..].trim_space()
	}
	if lo == '' {
		bound.has_lo = false
	} else {
		neg, mag := parse_refined_bound(lo) or { return none }
		bound.lo_negative = neg
		bound.lo_mag = mag
	}
	if hi == '' {
		bound.has_hi = false
	} else {
		neg, mag := parse_refined_bound(hi) or { return none }
		bound.hi_negative = neg
		bound.hi_mag = mag
	}
	if !bound.has_lo && !bound.has_hi {
		return none
	}
	// The interval must not be empty: `10..1` and exclusive single points
	// such as `5<..5` or `5..<5` are rejected.
	if bound.has_lo && bound.has_hi {
		order := compare_refined_magnitudes(bound.lo_negative, bound.lo_mag, bound.hi_negative,
			bound.hi_mag)
		if order > 0 {
			return none
		}
		if order == 0 && (bound.lo_exclusive || bound.hi_exclusive) {
			return none
		}
	}
	return bound
}

// parse_refined_bound parses one decimal bound with an optional sign into its
// sign and its magnitude without leading zeros.
fn parse_refined_bound(text string) ?(bool, string) {
	mut rest := text.trim_space()
	if rest == '' {
		return none
	}
	mut negative := false
	if rest[0] == `-` {
		negative = true
		rest = rest[1..]
	} else if rest[0] == `+` {
		rest = rest[1..]
	}
	if rest == '' {
		return none
	}
	mut digits := []u8{cap: rest.len}
	for ch in rest {
		if ch == `_` {
			continue
		}
		if ch < `0` || ch > `9` {
			return none
		}
		digits << ch
	}
	if digits.len == 0 {
		return none
	}
	mut start := 0
	for start < digits.len - 1 && digits[start] == `0` {
		start++
	}
	return negative, digits[start..].bytestr()
}

// parse_refined_literal parses an integer literal (any radix, either sign)
// into its sign and decimal magnitude.
fn parse_refined_literal(text string) ?(bool, string) {
	mut rest := text.replace('_', '').trim_space()
	if rest == '' {
		return none
	}
	mut negative := false
	if rest[0] == `-` {
		negative = true
		rest = rest[1..]
	} else if rest[0] == `+` {
		rest = rest[1..]
	}
	if rest == '' {
		return none
	}
	if rest.len >= 2 && rest[0] == `0`
		&& rest[1] in [`x`, `X`, `o`, `O`, `b`, `B`, `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`,
			`9`] {
		radix := if rest[1] in [`x`, `X`] {
			16
		} else if rest[1] in [`o`, `O`] {
			8
		} else if rest[1] in [`b`, `B`] {
			2
		} else {
			10
		}
		digits := if radix == 10 { rest } else { rest[2..] }
		if radix != 10 {
			decimal := decimal_magnitude_of_base(digits, radix) or { return none }
			return parse_refined_bound(if negative { '-' + decimal } else { decimal })
		}
		mut start := 0
		for start < digits.len - 1 && digits[start] == `0` {
			start++
		}
		for ch in digits {
			if ch < `0` || ch > `9` {
				return none
			}
		}
		return negative, digits[start..]
	}
	return parse_refined_bound(if negative { '-' + rest } else { rest })
}

// compare_refined_magnitudes orders two signed magnitudes: -1 when the first
// is smaller, 0 when equal, 1 when larger.
fn compare_refined_magnitudes(first_negative bool, first_mag string, second_negative bool, second_mag string) int {
	if first_negative != second_negative {
		return if first_negative { -1 } else { 1 }
	}
	if first_mag.len != second_mag.len {
		longer_wins := first_mag.len > second_mag.len
		if first_negative {
			return if longer_wins { -1 } else { 1 }
		}
		return if longer_wins { 1 } else { -1 }
	}
	if first_mag == second_mag {
		return 0
	}
	greater := first_mag > second_mag
	if first_negative {
		return if greater { -1 } else { 1 }
	}
	return if greater { 1 } else { -1 }
}

// contains_value reports whether the signed `magnitude` lies in the range.
pub fn (r RefinedRange) contains_value(negative bool, magnitude string) bool {
	mut mag := magnitude.trim_space().replace('_', '')
	mut start := 0
	for start < mag.len - 1 && mag[start] == `0` {
		start++
	}
	mag = mag[start..]
	if mag == '' {
		return false
	}
	if r.exclude_zero && !negative && mag == '0' {
		return false
	}
	if r.has_lo {
		order := compare_refined_magnitudes(negative, mag, r.lo_negative, r.lo_mag)
		if order < 0 || (order == 0 && r.lo_exclusive) {
			return false
		}
	}
	if r.has_hi {
		order := compare_refined_magnitudes(negative, mag, r.hi_negative, r.hi_mag)
		if order > 0 || (order == 0 && r.hi_exclusive) {
			return false
		}
	}
	return true
}

// RefinedAliasDecl records the bound and the declaration site of one refined
// alias. Both the qualified and the short name spellings map here.
struct RefinedAliasDecl {
	range_  RefinedRange
	decl_id int // top-level node index of the type declaration
}

// RefinedAliasInfo identifies the refinement bound governing a type.
struct RefinedAliasInfo {
	name_id u32
	range_  RefinedRange
	decl_id int
}

// register_refined_alias_range records a `range` refinement bound for the alias
// declared by `node` when the declaration carries a `range` attribute.
fn (mut tc TypeChecker) register_refined_alias_range(node flat.Node, decl_id int, qname_id u32, short_id u32) {
	range_text := tc.declaration_attribute_value(flat.NodeId(decl_id), 'range') or { return }
	decl_name := flat.source_type_name(qname_id)
	if node.children_count > 0 {
		tc.record_error_at(.unknown_type, 'refinement `range` on type `${decl_name}` is only supported on plain aliases', flat.NodeId(decl_id), node.pos)
		return
	}
	if node.generic_params().len > 0 {
		tc.record_error_at(.unknown_type, 'refinement `range` on type `${decl_name}` is not supported on generic aliases', flat.NodeId(decl_id), node.pos)
		return
	}
	bound := parse_refined_range(range_text) or {
		tc.record_error_at(.unknown_type, 'invalid range `${range_text}` on type `${decl_name}`: expected `lo..hi`, `lo..<hi`, `lo<..hi`, or `nonzero`', flat.NodeId(decl_id), node.pos)
		return
	}
	tc.refined_aliases[qname_id] = RefinedAliasDecl{
		range_:  bound
		decl_id: decl_id
	}
	if tc.cur_module in ['', 'main', 'builtin'] && short_id != qname_id
		&& short_id !in tc.refined_aliases {
		tc.refined_aliases[short_id] = RefinedAliasDecl{
			range_:  bound
			decl_id: decl_id
		}
	}
}

// refined_names_match reports whether two refined alias spellings name the same
// declaration, tolerating short and qualified spellings of one name.
fn (tc &TypeChecker) refined_names_match(first u32, second u32) bool {
	if first == second {
		return true
	}
	if tc.qualify_type_name(first) == second || first == tc.qualify_type_name(second) {
		return true
	}
	return tc.qualify_type_name(first) == tc.qualify_type_name(second)
}

// refined_alias_info resolves `t` through alias chains to its refinement bound.
fn (tc &TypeChecker) refined_alias_info(t &Type) ?RefinedAliasInfo {
	mut current := t
	for _ in 0 .. 16 {
		if current is Alias {
			if decl := tc.refined_aliases[current.name_id] {
				return RefinedAliasInfo{
					name_id: current.name_id
					range_:  decl.range_
					decl_id: decl.decl_id
				}
			}
			qualified := tc.qualify_type_name(current.name_id)
			if decl := tc.refined_aliases[qualified] {
				return RefinedAliasInfo{
					name_id: qualified
					range_:  decl.range_
					decl_id: decl.decl_id
				}
			}
			current = current.base_type
		} else {
			return none
		}
	}
	return none
}

// refined_static_int_value evaluates `value_id` when it is an integer literal
// or a named constant that `const_int_value` can evaluate.
fn (tc &TypeChecker) refined_static_int_value(value_id flat.NodeId) ?(bool, string) {
	if !tc.valid_node_id(value_id) {
		return none
	}
	if literal := tc.integer_literal_source(value_id) {
		return parse_refined_literal(literal)
	}
	node := tc.a.node(value_id)
	if node.kind == .ident && node.value.len > 0 {
		if value := tc.const_int_value(node.value, []string{}) {
			if value < 0 {
				return true, (-i64(value)).str()
			}
			return false, value.str()
		}
	}
	return none
}

// expr_is_refined_constructor reports whether `value_id` is checked inside the
// `from` constructor of the refined alias, where self-construction is allowed.
fn (tc &TypeChecker) expr_is_refined_constructor(refined_name u32) bool {
	fn_id := tc.fn_context.node_id
	if fn_id < 0 || !tc.valid_node_id(flat.NodeId(fn_id)) {
		return false
	}
	fn_node := tc.a.node(flat.NodeId(fn_id))
	if fn_node.kind != .fn_decl {
		return false
	}
	if receiver, method := flat.decode_static_type_method_name(fn_node.value) {
		if method == 'from' {
			receiver_id := flat.source_type_id(receiver)
			return tc.refined_names_match(receiver_id, refined_name)
		}
		return false
	}
	// Static constructors may also spell the receiver with a dot.
	if fn_node.value.contains('.') && fn_node.value.ends_with('.from') {
		receiver_id := flat.source_type_id(fn_node.value[..fn_node.value.len - '.from'.len])
		return tc.refined_names_match(receiver_id, refined_name)
	}
	return false
}

// refined_value_matches reports whether a value may flow into the refined
// `expected`: the same refinement, an in-range static, or an explicit `unsafe`
// or constructor context. Anything else needs the caller's diagnostic.
fn (tc &TypeChecker) refined_value_matches(value_id flat.NodeId, actual &Type, expected &Type) bool {
	info := tc.refined_alias_info(expected) or { return false }
	if actual_info := tc.refined_alias_info(actual) {
		return tc.refined_names_match(actual_info.name_id, info.name_id)
	}
	if tc.unsafe_depth > 0 || tc.expr_is_inside_unsafe_block(value_id) {
		return true
	}
	if tc.expr_is_refined_constructor(info.name_id) {
		return true
	}
	negative, magnitude := tc.refined_static_int_value(value_id) or { return false }
	return info.range_.contains_value(negative, magnitude)
}

// check_refined_expected_mismatch reports the refinement diagnostic when
// `expected` is refined and the value was already found incompatible (the
// caller consulted `refined_value_matches` through the compatibility chain).
// It returns true when it reported and the caller must skip its generic
// diagnostic.
fn (mut tc TypeChecker) check_refined_expected_mismatch(expected &Type, value_id flat.NodeId, actual &Type, kind TypeErrorKind, diag_id flat.NodeId, pos token.Pos) bool {
	info := tc.refined_alias_info(expected) or { return false }
	if negative, magnitude := tc.refined_static_int_value(value_id) {
		if info.range_.contains_value(negative, magnitude) {
			return false
		}
		display := if negative { '-' + magnitude } else { magnitude }
		type_name := flat.source_type_name(info.name_id)
		tc.record_error_at(kind, 'value `${display}` is out of range for type `${type_name}` (allowed range: ${info.range_.raw_text})', diag_id, pos)
		return true
	}
	if actual_info := tc.refined_alias_info(actual) {
		if tc.refined_names_match(actual_info.name_id, info.name_id) {
			return false
		}
	}
	type_name := flat.source_type_name(info.name_id)
	tc.record_error_at(kind, 'cannot use `${actual.name()}` as `${type_name}`; use `${type_name}.from(value)!` to check the value at runtime', diag_id, pos)
	return true
}

// check_refined_cast enforces the refinement bound on a `Target(expr)` cast.
// In-range statics, same-refinement values and explicit `unsafe`/constructor
// contexts pass through; anything else is reported here. It returns true when
// it reported and the caller must not run the remaining cast checks.
fn (mut tc TypeChecker) check_refined_cast(id flat.NodeId, child_id flat.NodeId, actual &Type, target &Type) bool {
	info := tc.refined_alias_info(target) or { return false }
	if tc.refined_value_matches(child_id, actual, target) {
		return false
	}
	node := tc.a.node(id)
	if negative, magnitude := tc.refined_static_int_value(child_id) {
		if info.range_.contains_value(negative, magnitude) {
			return false
		}
		display := if negative { '-' + magnitude } else { magnitude }
		type_name := flat.source_type_name(info.name_id)
		tc.record_error_at(.assignment_mismatch, 'value `${display}` is out of range for type `${type_name}` (allowed range: ${info.range_.raw_text})', id, node.pos)
		return true
	}
	type_name := flat.source_type_name(info.name_id)
	tc.record_error_at(.assignment_mismatch, 'cannot cast `${actual.name()}` to `${type_name}`; use `${type_name}.from(value)!` to check the value at runtime', id, node.pos)
	return true
}

// check_refined_alias_requirements verifies every refined alias once the full
// signature tables are known: the base must be a plain integer type and a
// `from` constructor with the exact signature must exist.
fn (mut tc TypeChecker) check_refined_alias_requirements() {
	mut verified := map[int]bool{}
	for name_id, decl in tc.refined_aliases {
		if decl.decl_id in verified {
			continue
		}
		verified[decl.decl_id] = true
		decl_node := tc.a.node(flat.NodeId(decl.decl_id))
		type_name := flat.source_type_name(name_id)
		target_spelling := tc.type_aliases.type_id(name_id) or {
			tc.type_aliases.type_id(tc.qualify_type_name(name_id)) or { u32(0) }
		}
		if target_spelling == 0 {
			continue
		}
		base := tc.parse_type(target_spelling)
		clean := unalias_type(base)
		if clean is Primitive && clean.props.has(.integer) && !clean.props.has(.boolean) {
			tc.check_refined_from_method(name_id, decl, base)
			continue
		}
		tc.record_error_at(.unknown_type, 'refinement `range` on type `${type_name}` requires an integer base type, not `${base.name()}`', flat.NodeId(decl.decl_id), decl_node.pos)
	}
}

// check_refined_from_method verifies the `from` constructor of one refined
// alias: `fn Type.from(base) !Type` with the base as its only parameter.
fn (mut tc TypeChecker) check_refined_from_method(name_id u32, decl RefinedAliasDecl, base &Type) {
	type_name := flat.source_type_name(name_id)
	decl_node := tc.a.node(flat.NodeId(decl.decl_id))
	clean_base := unalias_type(base)
	from_key := tc.static_assoc_fn_key_for_base(name_id, 'from') or {
		tc.static_assoc_fn_key_for_base(tc.qualify_type_name(name_id), 'from') or {
			tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
			return
		}
	}
	params := tc.fn_param_types[from_key] or {
		tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
		return
	}
	if params.len != 1 {
		tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
		return
	}
	param_type := params[0]
	if _ := tc.refined_alias_info(&param_type) {
		tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
		return
	}
	param_base := unalias_type(&param_type)
	want_base := unalias_type(base)
	if param_base.source_type_id() != want_base.source_type_id() {
		tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
		return
	}
	ret := tc.fn_ret_types[from_key] or {
		tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
		return
	}
	if ret is ResultType {
		if payload := tc.refined_alias_info(ret.base_type) {
			if tc.refined_names_match(payload.name_id, name_id) {
				return
			}
		}
	}
	tc.record_error_at(.unknown_fn, 'refined type `${type_name}` must declare `from` as `fn ${type_name}.from(${clean_base.name()}) !${type_name}`', flat.NodeId(decl.decl_id), decl_node.pos)
}
