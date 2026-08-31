#insert "@VMODROOT/anon.h"
#insert "@VMODROOT/cstruct_goto_label_hostile.h"

@[typedef]
struct C.outer {
	inner struct {
		x int
	}
}

struct Outer {
	inner struct {
		val int
	}
}

fn test_main() {
	_ = Outer{}
	_ = C.outer{}
}

fn generic_labeled_loop_with_selected_goto[T]() int {
	mut reached := 0
	$if T is int {
		unsafe {
			goto outer
		}
	}
	reached = 7
	outer: for {
		reached++
		break outer
	}
	return reached
}

fn ordinary_labels_with_c_name_collisions_and_hostile_macro() int {
	mut reached := 0
	unsafe {
		goto class
	}
	class:
	reached++
	unsafe {
		goto __v_class
	}
	__v_class:
	reached += 2
	unsafe {
		goto macro_target
	}
	macro_target:
	reached += 4
	return reached
}

fn loop_head_labels_with_c_name_collisions() int {
	mut reached := 0
	mut class_visits := 0
	unsafe {
		goto class
	}
	class: for {
		class_visits++
		reached++
		if class_visits == 1 {
			continue class
		}
		break class
	}
	mut v_class_visits := 0
	unsafe {
		goto __v_class
	}
	__v_class: for {
		v_class_visits++
		reached += 2
		if v_class_visits == 1 {
			continue __v_class
		}
		break __v_class
	}
	mut long_visits := 0
	unsafe {
		goto long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax
	}
	long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax: for {
		long_visits++
		reached += 4
		if long_visits == 1 {
			continue long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax
		}
		break long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax
	}
	return reached
}

fn long_labels_and_source_generated_name_collision() int {
	mut reached := 0
	unsafe {
		goto long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax
	}
	long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax:
	reached++
	mut right_visits := 0
	long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay:
	right_visits++
	reached += 2
	if right_visits == 1 {
		unsafe {
			goto long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay
		}
	}
	unsafe {
		goto __v_user_goto_0
	}
	__v_user_goto_0:
	reached += 4
	return reached
}

fn labels_reset_in_first_function() int {
	unsafe {
		goto shared_label
	}
	shared_label:
	return 1
}

fn labels_reset_in_second_function() int {
	shared_label:
	if arguments().len == 0 {
		unsafe {
			goto shared_label
		}
	}
	return 2
}

fn labels_restore_across_nested_closures() int {
	mut reached := 0
	unsafe {
		goto outer_before
	}
	outer_before:
	reached++
	callback := fn () int {
		mut inner := 0
		unsafe {
			goto closure_before
		}
		closure_before:
		inner++
		nested := fn () int {
			mut value := 0
			unsafe {
				goto nested_label
			}
			nested_label:
			value += 1001
			return value
		}
		inner += nested()
		unsafe {
			goto closure_after
		}
		closure_after:
		inner += 101
		return inner
	}
	reached += callback()
	unsafe {
		goto outer_after
	}
	outer_after:
	reached += 10001
	return reached
}

fn test_labeled_loop_name_matching_c_typedef() {
	mut iterations := 0
	outer: for {
		iterations++
		for {
			if iterations == 1 {
				continue outer
			}
			break outer
		}
	}
	assert iterations == 2
	assert generic_labeled_loop_with_selected_goto[string]() == 8
	assert generic_labeled_loop_with_selected_goto[int]() == 1
	assert ordinary_labels_with_c_name_collisions_and_hostile_macro() == 7
	assert loop_head_labels_with_c_name_collisions() == 14
	assert long_labels_and_source_generated_name_collision() == 9
	assert labels_reset_in_first_function() == 1
	assert labels_reset_in_second_function() == 2
	assert labels_restore_across_nested_closures() == 11105
}
