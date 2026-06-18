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
