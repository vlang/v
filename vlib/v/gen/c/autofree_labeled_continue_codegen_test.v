import os

const vroot = os.dir(@VEXE)
const test_vexe = os.quoted_path(@VEXE)
const autofree_labeled_continue_testdata = os.join_path(vroot,
	'vlib/v/tests/autofree_labeled_continue_scope_test.v')

fn function_window(source string, signature string) string {
	start := source.index(signature) or { return '' }
	mut depth := 0
	for i := start; i < source.len; i++ {
		if source[i] == `{` {
			depth++
		} else if source[i] == `}` {
			depth--
			if depth == 0 {
				return source[start..i + 1]
			}
		}
	}
	return source[start..]
}

fn test_labeled_loop_cleanup_codegen_order() {
	cmd := '${test_vexe} -autofree -o - ${os.quoted_path(autofree_labeled_continue_testdata)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	generated := res.output

	fallthrough_fn := function_window(generated,
		'void main__labeled_fallthrough_cleanup_order(void) {')
	inner_free_pos := fallthrough_fn.index('main__Tracked_free(&inner);') or {
		assert false, fallthrough_fn
		return
	}
	after_inner := fallthrough_fn[inner_free_pos..]
	defer_pos := after_inner.index('main__push_event(7 + target.id - 1);') or {
		assert false, fallthrough_fn
		return
	}
	free_pos := after_inner.index('main__Tracked_free(&target);') or {
		assert false, fallthrough_fn
		return
	}
	assert defer_pos < free_pos

	labeled_break_fn := function_window(generated, 'void main__labeled_break_cleanup_order(void) {')
	labeled_break_goto_pos := labeled_break_fn.index('goto break_outer__break;') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_inner_defer_pos := labeled_break_fn.index('main__push_event(9);') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_inner_free_pos := labeled_break_fn.index('main__Tracked_free(&inner);') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_middle_defer_pos := labeled_break_fn.index('main__push_event(8);') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_middle_free_pos := labeled_break_fn.index('main__Tracked_free(&middle);') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_target_defer_pos := labeled_break_fn.index('main__push_event(7);') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_target_free_pos := labeled_break_fn.index('main__Tracked_free(&target);') or {
		assert false, labeled_break_fn
		return
	}
	assert labeled_break_inner_defer_pos < labeled_break_inner_free_pos
	assert labeled_break_inner_free_pos < labeled_break_middle_defer_pos
	assert labeled_break_middle_defer_pos < labeled_break_middle_free_pos
	assert labeled_break_middle_free_pos < labeled_break_target_defer_pos
	assert labeled_break_target_defer_pos < labeled_break_target_free_pos
	assert labeled_break_target_free_pos < labeled_break_goto_pos

	break_fn := function_window(generated, 'void main__for_c_labeled_break(void) {')
	break_label_pos := break_fn.index('outer__break: {}') or {
		assert false, break_fn
		return
	}
	init_free_pos := break_fn.index('main__Tracked_free(&init);') or {
		assert false, break_fn
		return
	}
	assert break_label_pos < init_free_pos

	nested_continue_fn := function_window(generated,
		'void main__for_c_nested_continue_outer_init_cleanup(void) {')
	nested_continue_body_free_pos := nested_continue_fn.index('main__Tracked_free(&body);') or {
		assert false, nested_continue_fn
		return
	}
	nested_continue_inner_free_pos := nested_continue_fn.index('main__Tracked_free(&inner_init);') or {
		assert false, nested_continue_fn
		return
	}
	nested_continue_goto_pos := nested_continue_fn.index('goto outer__continue_entry;') or {
		assert false, nested_continue_fn
		return
	}
	nested_continue_after_goto := nested_continue_fn[nested_continue_goto_pos..]
	nested_continue_before_goto := nested_continue_fn[..nested_continue_goto_pos]
	nested_continue_outer_free_after_goto_pos := nested_continue_after_goto.index('main__Tracked_free(&outer_init);') or {
		assert false, nested_continue_fn
		return
	}
	assert nested_continue_body_free_pos < nested_continue_inner_free_pos
	assert nested_continue_inner_free_pos < nested_continue_goto_pos
	assert !nested_continue_before_goto.contains('main__Tracked_free(&outer_init);')
	assert nested_continue_outer_free_after_goto_pos > 0

	nested_break_fn := function_window(generated,
		'void main__for_c_nested_break_outer_init_cleanup(void) {')
	nested_break_body_free_pos := nested_break_fn.index('main__Tracked_free(&body);') or {
		assert false, nested_break_fn
		return
	}
	nested_break_inner_free_pos := nested_break_fn.index('main__Tracked_free(&inner_init);') or {
		assert false, nested_break_fn
		return
	}
	nested_break_goto_pos := nested_break_fn.index('goto break_outer__break;') or {
		assert false, nested_break_fn
		return
	}
	nested_break_after_goto := nested_break_fn[nested_break_goto_pos..]
	nested_break_before_goto := nested_break_fn[..nested_break_goto_pos]
	nested_break_outer_free_after_goto_pos := nested_break_after_goto.index('main__Tracked_free(&outer_init);') or {
		assert false, nested_break_fn
		return
	}
	assert nested_break_body_free_pos < nested_break_inner_free_pos
	assert nested_break_inner_free_pos < nested_break_goto_pos
	assert !nested_break_before_goto.contains('main__Tracked_free(&outer_init);')
	assert nested_break_outer_free_after_goto_pos > 0

	return_second_fn := function_window(generated,
		'main__Tracked main__for_c_multi_init_return_second(void) {')
	return_pos := return_second_fn.index('return ') or {
		assert false, return_second_fn
		return
	}
	body_free_pos := return_second_fn.index('main__Tracked_free(&body);') or {
		assert false, return_second_fn
		return
	}
	first_free_pos := return_second_fn.index('main__Tracked_free(&first);') or {
		assert false, return_second_fn
		return
	}
	assert body_free_pos < return_pos
	assert first_free_pos < return_pos
	if second_free_pos := return_second_fn.index('main__Tracked_free(&second);') {
		assert return_pos < second_free_pos
	}

	return_field_fn := function_window(generated,
		'string main__for_c_return_init_field_string(void) {')
	return_field_return_pos := return_field_fn.index('return ') or {
		assert false, return_field_fn
		return
	}
	return_field_return_path := return_field_fn[..return_field_return_pos]
	assert !return_field_return_path.contains('main__Holder_free(&init);')
	assert !return_field_return_path.contains('builtin__string_free(&(init.label));')
	assert !return_field_fn.contains('builtin__string_free(&(init.other));')

	return_alias_field_fn := function_window(generated,
		'main__Label main__for_c_return_init_field_alias_string(void) {')
	return_alias_field_return_pos := return_alias_field_fn.index('return ') or {
		assert false, return_alias_field_fn
		return
	}
	return_alias_field_return_path := return_alias_field_fn[..return_alias_field_return_pos]
	assert !return_alias_field_return_path.contains('main__AliasHolder_free(&init);')
	assert !return_alias_field_return_path.contains('builtin__string_free(&(init.label));')
	assert !return_alias_field_fn.contains('builtin__string_free(&(init.other));')

	return_id_fn := function_window(generated, 'int main__for_c_return_init_field_id(void) {')
	return_id_holder_free_pos := return_id_fn.index('main__Holder_free(&init);') or {
		assert false, return_id_fn
		return
	}
	return_id_return_pos := return_id_fn.index('return ') or {
		assert false, return_id_fn
		return
	}
	assert return_id_holder_free_pos < return_id_return_pos

	branch_body_fn := function_window(generated,
		'main__Tracked main__for_c_branch_return_body_after_init_path(bool cond) {')
	body_decl_pos := branch_body_fn.index('main__Tracked body = main__tracked(5);') or {
		assert false, branch_body_fn
		return
	}
	body_return_path := branch_body_fn[body_decl_pos..]
	branch_body_return_pos := body_return_path.index('return ') or {
		assert false, branch_body_fn
		return
	}
	branch_init_free_pos := body_return_path.index('main__Tracked_free(&init);') or {
		assert false, branch_body_fn
		return
	}
	assert branch_init_free_pos < branch_body_return_pos
	if branch_body_free_pos := body_return_path.index('main__Tracked_free(&body);') {
		assert branch_body_return_pos < branch_body_free_pos
	}
}
