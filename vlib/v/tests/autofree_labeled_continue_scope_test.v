// vtest build: !sanitize-address-gcc && !sanitize-address-clang
// vtest vflags: -autofree
@[has_globals]
module main

__global (
	event_code int
)

type Label = string

struct Tracked {
	id int
}

struct Holder {
	label string
	other string
	id    int
}

struct AliasHolder {
	label Label
	other string
	id    int
}

fn (t &Tracked) free() {
	unsafe {
		event_code = event_code * 10 + t.id
	}
}

fn (h &Holder) free() {
	push_event(h.id)
	unsafe {
		h.label.free()
		h.other.free()
	}
}

fn (h &AliasHolder) free() {
	push_event(h.id)
	unsafe {
		h.label.free()
		h.other.free()
	}
}

fn tracked(id int) Tracked {
	return Tracked{
		id: id
	}
}

fn holder(id int) Holder {
	return Holder{
		label: 'x'.repeat(id)
		other: 'y'.repeat(id + 1)
		id:    id
	}
}

fn alias_holder(id int) AliasHolder {
	return AliasHolder{
		label: Label('x'.repeat(id))
		other: 'y'.repeat(id + 1)
		id:    id
	}
}

fn push_event(id int) {
	unsafe {
		event_code = event_code * 10 + id
	}
}

fn reset_events() {
	unsafe {
		event_code = 0
	}
}

fn events() int {
	return unsafe { event_code }
}

fn labeled_continue_cleanup_order() {
	outer: for _ in 0 .. 1 {
		target := tracked(1)
		if target.id == -1 {
			push_event(9)
		}
		defer {
			push_event(7 + target.id - 1)
		}
		{
			inner := tracked(2)
			if inner.id == -1 {
				push_event(9)
			}
			continue outer
		}
		after := tracked(3)
		defer {
			push_event(8)
		}
		if after.id == -1 {
			push_event(9)
		}
	}
}

fn labeled_fallthrough_cleanup_order() {
	outer: for _ in 0 .. 1 {
		target := tracked(1)
		if target.id == -1 {
			continue outer
		}
		defer {
			push_event(7 + target.id - 1)
		}
		{
			inner := tracked(2)
			if inner.id == -1 {
				continue outer
			}
		}
	}
}

fn labeled_break_cleanup_order() {
	break_outer: for _ in 0 .. 1 {
		target := tracked(1)
		defer {
			push_event(7)
		}
		{
			middle := tracked(2)
			defer {
				push_event(8)
			}
			{
				inner := tracked(3)
				defer {
					push_event(9)
				}
				if inner.id == 3 {
					break break_outer
				}
			}
		}
	}
}

fn for_c_all_continue() {
	mut i := 0
	outer: for init := tracked(4); i < 2; i++ {
		if init.id == -1 {
			push_event(9)
		}
		body := tracked(5 + i)
		if body.id >= 5 {
			continue outer
		}
	}
}

fn for_c_mixed_fallthrough_continue() {
	mut i := 0
	outer: for init := tracked(4); i < 3; i++ {
		if init.id == -1 {
			push_event(9)
		}
		body := tracked(5 + i)
		if i == 1 {
			continue outer
		}
		if body.id == -1 {
			push_event(9)
		}
	}
}

fn for_c_nested_continue_outer_init_cleanup() {
	mut i := 0
	outer: for outer_init := tracked(4); i < 2; i++ {
		if outer_init.id == -1 {
			push_event(9)
		}
		mut j := 0
		for inner_init := tracked(5); j < 1; j++ {
			if inner_init.id == -1 {
				push_event(9)
			}
			body := tracked(6)
			if body.id == 6 {
				continue outer
			}
		}
	}
}

fn for_c_nested_break_outer_init_cleanup() {
	mut i := 0
	break_outer: for outer_init := tracked(4); i < 1; i++ {
		if outer_init.id == -1 {
			push_event(9)
		}
		mut j := 0
		for inner_init := tracked(5); j < 1; j++ {
			if inner_init.id == -1 {
				push_event(9)
			}
			body := tracked(6)
			if body.id == 6 {
				break break_outer
			}
		}
	}
}

fn for_c_labeled_break() {
	mut i := 0
	outer: for init := tracked(4); i < 1; i++ {
		if init.id == -1 {
			push_event(9)
		}
		body := tracked(5)
		if body.id == 5 {
			break outer
		}
	}
}

fn for_c_return() int {
	mut i := 0
	for init := tracked(4); i < 1; i++ {
		if init.id == -1 {
			push_event(9)
		}
		body := tracked(5)
		if body.id == 5 {
			return body.id
		}
	}
	return 0
}

fn for_c_multi_init_return() int {
	mut i := 0
	for first, second := tracked(4), tracked(3); i < 1; i++ {
		if first.id + second.id == -1 {
			push_event(9)
		}
		body := tracked(5)
		if body.id == 5 {
			return body.id
		}
	}
	return 0
}

fn for_c_return_init_string() string {
	mut i := 0
	for init := 'ab'.repeat(3); i < 1; i++ {
		body := tracked(5)
		if body.id == 5 {
			return init
		}
	}
	return ''
}

fn for_c_multi_init_return_second() Tracked {
	mut i := 0
	for first, second := tracked(4), tracked(3); i < 1; i++ {
		if first.id + second.id == -1 {
			push_event(9)
		}
		body := tracked(5)
		if body.id == 5 {
			return second
		}
	}
	return tracked(0)
}

fn for_c_multi_init_return_second_string() string {
	mut i := 0
	for first, second := 'x'.repeat(4), 'y'.repeat(3); i < 1; i++ {
		assert first.len == 4
		body := tracked(5)
		if body.id == 5 {
			return second
		}
	}
	return ''
}

fn for_c_return_init_field_string() string {
	mut i := 0
	for init := holder(4); i < 1; i++ {
		body := tracked(5)
		if body.id == 5 {
			return init.label
		}
	}
	return ''
}

fn for_c_return_init_field_alias_string() Label {
	mut i := 0
	for init := alias_holder(4); i < 1; i++ {
		body := tracked(5)
		if body.id == 5 {
			return init.label
		}
	}
	return Label('')
}

fn for_c_return_init_field_id() int {
	mut i := 0
	for init := holder(4); i < 1; i++ {
		body := tracked(5)
		if body.id == 5 {
			return init.id
		}
	}
	return 0
}

fn for_c_branch_return_body_after_init_path(cond bool) Tracked {
	mut i := 0
	for init := tracked(4); i < 1; i++ {
		if cond {
			return init
		}
		body := tracked(5)
		return body
	}
	return tracked(0)
}

fn for_c_multi_init_branch_return_body_after_second_path(cond bool) Tracked {
	mut i := 0
	for first, second := tracked(4), tracked(3); i < 1; i++ {
		if cond {
			return second
		}
		body := tracked(5)
		return body
	}
	return tracked(0)
}

fn for_c_branch_return_body_after_init_string_path(cond bool) string {
	mut i := 0
	for init := 'z'.repeat(4); i < 1; i++ {
		if cond {
			return init
		}
		body := tracked(5)
		if body.id == 5 {
			return 'body'
		}
	}
	return ''
}

fn test_labeled_continue_runs_reached_target_defer_before_target_autofree() {
	reset_events()
	labeled_continue_cleanup_order()
	assert events() == 271
}

fn test_labeled_fallthrough_runs_target_defer_before_target_autofree() {
	reset_events()
	labeled_fallthrough_cleanup_order()
	assert events() == 271
}

fn test_labeled_break_runs_all_exited_defers_before_jump() {
	reset_events()
	labeled_break_cleanup_order()
	assert events() == 938271
}

fn test_for_c_all_continue_frees_init_once_after_loop() {
	reset_events()
	for_c_all_continue()
	assert events() == 564
}

fn test_for_c_mixed_fallthrough_continue_keeps_init_until_loop_exit() {
	reset_events()
	for_c_mixed_fallthrough_continue()
	assert events() == 5674
}

fn test_for_c_nested_continue_outer_frees_inner_init_before_jump() {
	reset_events()
	for_c_nested_continue_outer_init_cleanup()
	assert events() == 65654
}

fn test_for_c_nested_break_outer_frees_inner_init_before_jump() {
	reset_events()
	for_c_nested_break_outer_init_cleanup()
	assert events() == 654
}

fn test_for_c_labeled_break_frees_init_once_after_body() {
	reset_events()
	for_c_labeled_break()
	assert events() == 54
}

fn test_for_c_return_frees_init_once_after_body() {
	reset_events()
	assert for_c_return() == 5
	assert events() == 54
}

fn test_for_c_multi_init_return_frees_all_init_vars_after_body() {
	reset_events()
	assert for_c_multi_init_return() == 5
	assert events() == 534
}

fn test_for_c_returned_init_string_is_not_freed_before_return() {
	reset_events()
	value := for_c_return_init_string()
	assert value == 'ababab'
	assert events() == 5
}

fn test_for_c_multi_init_returned_second_is_not_freed_before_return() {
	reset_events()
	value := for_c_multi_init_return_second()
	assert value.id == 3
	assert events() == 54
}

fn test_for_c_multi_init_returned_second_string_is_not_freed_before_return() {
	reset_events()
	value := for_c_multi_init_return_second_string()
	assert value == 'yyy'
	assert events() == 5
}

fn test_for_c_returned_init_string_field_preserves_owner() {
	reset_events()
	value := for_c_return_init_field_string()
	assert value == 'xxxx'
	assert events() == 5
}

fn test_for_c_returned_init_alias_string_field_preserves_owner() {
	reset_events()
	value := for_c_return_init_field_alias_string()
	assert string(value) == 'xxxx'
	assert events() == 5
}

fn test_for_c_returned_init_scalar_field_still_frees_owner() {
	reset_events()
	value := for_c_return_init_field_id()
	assert value == 4
	assert events() == 54
}

fn test_for_c_branch_return_body_still_frees_init() {
	reset_events()
	value := for_c_branch_return_body_after_init_path(false)
	assert value.id == 5
	assert events() == 4
}

fn test_for_c_multi_init_branch_return_body_still_frees_all_init_vars() {
	reset_events()
	value := for_c_multi_init_branch_return_body_after_second_path(false)
	assert value.id == 5
	assert events() == 34
}

fn test_for_c_branch_return_body_string_path_still_frees_init() {
	reset_events()
	value := for_c_branch_return_body_after_init_string_path(false)
	assert value == 'body'
	assert events() == 5
}
