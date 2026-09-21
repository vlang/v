fn (tc &TypeChecker) assignment_types_compatible(rhs_id flat.NodeId, rhs_type Type, expected_type Type, op flat.Op) bool {
	if op == .assign && tc.fn_storage_voidptr_mismatch(rhs_id, rhs_type, expected_type) {
		return false
	}
	if op == .assign && tc.translated_files[tc.cur_file] && rhs_type is ArrayFixed
		&& expected_type is Pointer && tc.a.node(rhs_id).kind == .ident {
		return tc.type_compatible(rhs_type.elem_type, expected_type.base_type)
	}
	if op == .assign
		&& tc.fixed_array_address_to_byte_pointer_compatible(rhs_id, rhs_type, expected_type) {
		return true
	}
	if fn_param_unalias_type(expected_type).is_integer() && tc.c_scalar_byte_literal_arg(rhs_id) {
		return true
	}
	if tc.expr_tail_is_nil(rhs_id) {
		if _ := fn_type_from_type(expected_type) {
			return true
		}
	}
	if base := tc.mut_param_expr_base(rhs_id, rhs_type) {
		if tc.type_compatible(base, expected_type)
			|| tc.pointer_value_compatible(base, expected_type) {
			return true
		}
	}
	clean_rhs := unalias_type(rhs_type)
	clean_expected := unalias_type(expected_type)
	if op == .assign && clean_expected is OptionType && clean_expected.base_type is Pointer
		&& tc.type_compatible(clean_rhs, clean_expected.base_type.base_type) {
		return true
	}
	if op == .assign && clean_rhs.name() == 'int' && clean_expected.name() == 'f64' {
		return true
	}
	if op == .assign && clean_rhs.is_integer() && clean_expected.is_float()
		&& tc.a.node(rhs_id).kind != .int_literal {
		return false
	}
	if op == .assign && clean_expected is FnType
		&& tc.fn_types_match_ignoring_module_qualification(clean_expected, clean_rhs) {
		return true
	}
	if clean_expected is SumType {
		return tc.direct_sum_assignment_variant_matches(rhs_type, clean_expected)
	}
	return tc.expr_compatible(rhs_id, rhs_type, expected_type)
		|| tc.pointer_value_compatible(rhs_type, expected_type)
		|| tc.pointer_arithmetic_assign_compatible(op, rhs_type, expected_type)
}

fn (tc &TypeChecker) fn_storage_voidptr_mismatch(expr_id flat.NodeId, actual Type, expected Type) bool {
	return is_fn_pointer_type(expected) && fn_param_is_voidptr_type(actual) && tc.unsafe_depth == 0
		&& !tc.current_fn_declared_unsafe() && !tc.expr_is_unsafe_nil(expr_id)
}
