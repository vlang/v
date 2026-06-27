import os

const test_vexe = os.quoted_path(@VEXE)

fn bounded_window(text string, marker string, before int, after int) string {
	pos := text.index(marker) or {
		assert false, 'missing marker: ${marker}'
		return ''
	}
	start := if pos > before { pos - before } else { 0 }
	end_limit := pos + marker.len + after
	end := if end_limit < text.len { end_limit } else { text.len }
	return text[start..end]
}

fn function_window(text string, marker string) string {
	pos := text.index(marker) or {
		assert false, 'missing marker: ${marker}'
		return ''
	}
	rest := text[pos + marker.len..]
	end_offset := rest.index('\nVV_LOC ') or { rest.len }
	return text[pos..pos + marker.len + end_offset]
}

fn function_window_containing(text string, marker string) string {
	pos := text.index(marker) or {
		assert false, 'missing marker: ${marker}'
		return ''
	}
	prefix := text[..pos]
	start := prefix.last_index('\nVV_LOC ') or { 0 }
	rest := text[start + 1..]
	end_offset := rest.index('\nVV_LOC ') or { rest.len }
	return rest[..end_offset]
}

fn test_closure_context_codegen_uses_collectable_memdup_and_ownership() {
	workdir := os.join_path(os.vtmp_dir(), 'closure_context_codegen_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	source_path := os.join_path(workdir, 'main.v')
	exe_path := os.join_path(workdir, 'main')
	os.write_file(source_path, 'module main

struct Receiver {
mut:
	value int
}

@[heap]
struct PointerReceiver {
mut:
	value int
}

type IntCallback = fn (int) int

type ZeroCallback = fn () int

type ResultRequestHandler = fn (int) !

__global global_cb = fn (x int) int {
	return x
}

struct CallbackBox {
mut:
	cb IntCallback = unsafe { nil }
}

fn make_closure(seed int) fn () int {
	return fn [seed] () int {
		return seed + 1
	}
}

fn (r Receiver) read() int {
	return r.value
}

fn (mut r PointerReceiver) bump() int {
	r.value++
	return r.value
}

fn local_direct_closure() {
	big := []int{len: 200, init: index}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	_ = h(11)
}

fn local_escaped_closure(mut callbacks []IntCallback) {
	big := []int{len: 200, init: index}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	callbacks << h
}

fn local_return_fn_value(n int) IntCallback {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	return h
}

fn local_copy_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	k := h
	return k(n % 200)
}

fn takes_cb(cb IntCallback, n int) int {
	return cb(n % 200)
}

fn local_arg_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	return takes_cb(h, n)
}

fn local_struct_init_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	box := CallbackBox{
		cb: h
	}
	return box.cb(n % 200)
}

fn local_struct_assign_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	mut box := CallbackBox{}
	box.cb = h
	return box.cb(n % 200)
}

fn local_global_assign_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	global_cb = h
	return global_cb(n % 200)
}

fn local_defer_closure() {
	big := []int{len: 200, init: index}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	defer {
		_ = h(0)
	}
}

fn local_return_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	if n >= 0 {
		return h(n % 200)
	}
	return 0
}

fn local_call_then_return_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	_ = h(n % 200)
	return 0
}

fn maybe_int(n int) ?int {
	if n < 0 {
		return none
	}
	return n
}

fn result_int(n int) !int {
	if n < 0 {
		return error("negative")
	}
	return n
}

fn maybe_request(request int) ! {
	if request < 0 {
		return error("negative request")
	}
}

fn local_option_propagation_cleanup(n int) ?int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	value := maybe_int(n)?
	return h(value % 200)
}

fn local_result_propagation_cleanup(n int) !int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	value := result_int(n)!
	return h(value % 200)
}

fn local_result_handler_cast_closure_boundary() {
	offset := 1
	handle_request := fn [offset] (request int) ! {
		maybe_request(request)!
		_ = offset
	}
	_ = ResultRequestHandler(handle_request)
}

fn local_for_c_init_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	for i := takes_cb(h, n); i < 1; i++ {
		return i
	}
	return 0
}

fn local_for_c_cond_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	mut result := 0
	for i := 0; i < 1 && takes_cb(h, n) >= 0; i++ {
		result += i
	}
	return result
}

fn local_for_c_inc_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	mut result := 0
	for i := 0; i < 1; i += takes_cb(h, n) + 1 {
		result += i
	}
	return result
}

fn local_for_c_init_closure_body_tail(n int) int {
	big := []int{len: 200, init: index + n}
	mut i := 0
	mut result := 0
	for h := fn [big] (x int) int {
		return big[x % big.len]
	}; i < 2; i++ {
		result += h(i)
	}
	return result
}

fn local_for_c_init_closure_continue(n int) int {
	big := []int{len: 200, init: index + n}
	mut i := 0
	mut result := 0
	for h := fn [big] (x int) int {
		return big[x % big.len]
	}; i < 2; i++ {
		if i == 0 {
			continue
		}
		result += h(i)
	}
	return result
}

fn local_multi_for_c_init_closure_body_tail(n int) int {
	big := []int{len: 200, init: index + n}
	mut result := 0
	for h, i := fn [big] (x int) int {
		return big[x % big.len]
	}, 0; i < 2; i++ {
		result += h(i)
	}
	return result
}

fn local_for_c_body_closure_continue_cleanup(n int) int {
	mut i := 0
	for ; i < 1; i++ {
		big := []int{len: 200, init: index + n}
		h := fn [big] (x int) int {
			return big[x % big.len]
		}
		_ = h(n % 200)
		continue
	}
	return 0
}

fn local_for_c_init_closure_return_cleanup(n int) int {
	big := []int{len: 200, init: index + n}
	mut i := 0
	for h := fn [big] (x int) int {
		return big[x % big.len]
	}; i < 1; i++ {
		return h(n % 200)
	}
	return -1
}

fn local_for_c_init_closure_labeled_break_cleanup(n int) int {
	big := []int{len: 200, init: index + n}
	mut i := 0
	mut result := 0
	closure_init_break: for h := fn [big] (x int) int {
		return big[x % big.len]
	}; i < 1; i++ {
		result += h(n % 200)
		break closure_init_break
	}
	return result
}

fn local_for_c_init_escaped_closure(mut callbacks []ZeroCallback, n int) {
	big := []int{len: 200, init: index + n}
	mut i := 0
	for h := fn [big, n] () int {
		return big[n % big.len]
	}; i < 1; i++ {
		callbacks << h
	}
}

fn local_nested_for_c_init_closure_continue_outer(n int) int {
	outer_big := []int{len: 200, init: index + n}
	mut i := 0
	mut result := 0
	nested_continue_outer: for outer_h := fn [outer_big] (x int) int {
		return outer_big[x % outer_big.len]
	}; i < 2; i++ {
		inner_big := []int{len: 200, init: index + n + 10}
		mut j := 0
		for inner_h := fn [inner_big] (x int) int {
			return inner_big[x % inner_big.len]
		}; j < 1; j++ {
			result += outer_h(i) + inner_h(j)
			continue nested_continue_outer
		}
	}
	return result
}

fn local_nested_for_c_init_closure_break_outer(n int) int {
	outer_big := []int{len: 200, init: index + n}
	mut i := 0
	mut result := 0
	nested_break_outer: for outer_h := fn [outer_big] (x int) int {
		return outer_big[x % outer_big.len]
	}; i < 2; i++ {
		inner_big := []int{len: 200, init: index + n + 20}
		mut j := 0
		for inner_h := fn [inner_big] (x int) int {
			return inner_big[x % inner_big.len]
		}; j < 1; j++ {
			result += outer_h(i) + inner_h(j)
			break nested_break_outer
		}
	}
	return result
}

fn local_for_cond_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	mut result := 0
	for result < 1 && takes_cb(h, n) >= 0 {
		break
	}
	return result
}

fn local_for_in_cond_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	for value in []int{len: 1, init: takes_cb(h, n)} {
		return value
	}
	return 0
}

fn local_for_in_high_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	mut result := 0
	for i in 0 .. takes_cb(h, n) {
		result += i
	}
	return result
}

fn local_array_init_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	values := []int{len: 1, init: takes_cb(h, n)}
	return values[0]
}

fn local_if_else_condition_closure(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	if n < 0 {
		return n
	} else if takes_cb(h, n) >= 0 {
		return 0
	}
	return n
}

fn local_nested_returning_closure(n int) ZeroCallback {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	nested := fn [h, n] () int {
		return h(n % 200)
	}
	return nested
}

fn local_nested_stored_closure(mut callbacks []ZeroCallback, n int) {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	nested := fn [h, n] () int {
		return h(n % 200)
	}
	callbacks << nested
}

fn local_keyword_closure() {
	big := []int{len: 200, init: index}
	free := fn [big] (x int) int {
		return big[x % big.len]
	}
}

fn local_labeled_break_closure(n int) int {
	mut value := 0
	closure_break: for {
		big := []int{len: 200, init: index + n}
		h := fn [big] (x int) int {
			return big[x % big.len]
		}
		value = h(n % 200)
		break closure_break
	}
	return value
}

fn local_labeled_continue_closure(n int) int {
	mut value := 0
	closure_continue: for _ in 0 .. 1 {
		big := []int{len: 200, init: index + n}
		h := fn [big] (x int) int {
			return big[x % big.len]
		}
		value = h(n % 200)
		continue closure_continue
	}
	return value
}

fn local_spawn_closure() {
	big := []int{len: 200, init: index}
	h := fn [big] () {
		_ = big[0]
	}
	t := spawn h()
	t.wait()
}

fn local_go_closure() {
	big := []int{len: 200, init: index}
	h := fn [big] () {
		_ = big[0]
	}
	t := go h()
	t.wait()
}

fn main() {
	receiver := Receiver{
		value: 10
	}
	mut pointer_receiver := &PointerReceiver{
		value: 10
	}
	anon_cb := make_closure(5)
	value_cb := receiver.read
	pointer_cb := pointer_receiver.bump
	local_direct_closure()
	mut callbacks := []IntCallback{}
	local_escaped_closure(mut callbacks)
	local_spawn_closure()
	local_go_closure()
	local_defer_closure()
	local_result_handler_cast_closure_boundary()
	opt_cleanup := local_option_propagation_cleanup(-1) or { 0 }
	res_cleanup := local_result_propagation_cleanup(-1) or { 0 }
	returned_cb := local_return_fn_value(0)
	returned_nested := local_nested_returning_closure(0)
	mut zero_callbacks := []ZeroCallback{}
	local_nested_stored_closure(mut zero_callbacks, 0)
	local_keyword_closure()
	mut escaped_for_c_callbacks := []ZeroCallback{}
	local_for_c_init_escaped_closure(mut escaped_for_c_callbacks, 0)
	mut total := anon_cb() + value_cb() + pointer_cb() + local_return_closure(0)
	total += local_call_then_return_closure(0) + opt_cleanup + res_cleanup
	total += local_for_c_init_closure(0) + local_for_c_cond_closure(0) + local_for_c_inc_closure(0)
	total += local_for_c_init_closure_body_tail(0) + local_for_c_init_closure_continue(0)
	total += local_multi_for_c_init_closure_body_tail(0) + local_for_c_body_closure_continue_cleanup(0)
	total += local_for_c_init_closure_return_cleanup(0) + local_for_c_init_closure_labeled_break_cleanup(0)
	total += escaped_for_c_callbacks[0]()
	total += local_nested_for_c_init_closure_continue_outer(0) + local_nested_for_c_init_closure_break_outer(0)
	total += local_for_cond_closure(0) + local_for_in_cond_closure(0) + local_for_in_high_closure(0)
	total += local_array_init_closure(0) + local_if_else_condition_closure(0) + returned_nested()
	total += zero_callbacks[0]() + local_labeled_break_closure(0) + local_labeled_continue_closure(0)
	total += returned_cb(0) + local_copy_closure(0) + local_arg_closure(0)
	total += local_struct_init_closure(0) + local_struct_assign_closure(0) + local_global_assign_closure(0)
	println(total)
}
')!

	c_cmd := '${test_vexe} -enable-globals -gc boehm -skip-unused -o - ${os.quoted_path(source_path)}'
	c_res := os.execute(c_cmd)
	assert c_res.exit_code == 0, '${c_cmd}\n${c_res.output}'
	generated := c_res.output.replace('\r\n', '\n')
	assert !generated.contains('builtin__memdup_uncollectable')
	assert generated.contains('sizeof(builtin__closure__ClosureLiveInfo)')
	assert !generated.contains('new_map_noscan_value(sizeof(voidptr), sizeof(builtin__closure__ClosureLiveInfo)')
	assert !generated.contains('new_map_noscan_key_value(sizeof(voidptr), sizeof(builtin__closure__ClosureLiveInfo)')

	anon_window := bounded_window(generated, 'VV_LOC anon_fn___int main__make_closure(int seed) {',
		0, 900)
	assert anon_window.contains('builtin__closure__closure_create_with_data(')
	assert anon_window.contains('builtin__memdup(&(')
	assert anon_window.contains(', true)')

	value_method_call := bounded_window(generated,
		'builtin__closure__closure_create_with_data(_V_closure_main__Receiver_read_', 0, 300)
	assert value_method_call.contains('builtin__memdup(')
	assert value_method_call.contains(', true)')

	pointer_method_call := bounded_window(generated,
		'builtin__closure__closure_create_with_data(_V_closure_main__PointerReceiver_bump_', 0, 300)
	assert !pointer_method_call.contains('builtin__memdup(')
	assert pointer_method_call.contains(', false)')

	direct_fn := function_window(generated, 'void main__local_direct_closure(void) {')
	assert direct_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	escaped_fn := function_window(generated, 'void main__local_escaped_closure')
	assert !escaped_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	return_value_fn := function_window(generated, 'main__IntCallback main__local_return_fn_value')
	assert !return_value_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	copy_fn := function_window(generated, 'int main__local_copy_closure(int n) {')
	assert !copy_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	arg_fn := function_window(generated, 'int main__local_arg_closure(int n) {')
	assert !arg_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	struct_init_fn := function_window(generated, 'int main__local_struct_init_closure(int n) {')
	assert !struct_init_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	struct_assign_fn := function_window(generated, 'int main__local_struct_assign_closure(int n) {')
	assert !struct_assign_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	global_assign_fn := function_window(generated, 'int main__local_global_assign_closure(int n) {')
	assert !global_assign_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	defer_fn := function_window(generated, 'void main__local_defer_closure(void) {')
	assert !defer_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	return_fn := function_window(generated, 'int main__local_return_closure(int n) {')
	return_cleanup_pos := return_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, return_fn
		return
	}
	return_tmp_pos := return_fn.index(' = h(') or {
		assert false, return_fn
		return
	}
	assert return_tmp_pos < return_cleanup_pos
	assert return_fn[return_cleanup_pos..].contains('return _')

	call_return_fn := function_window(generated,
		'int main__local_call_then_return_closure(int n) {')
	assert call_return_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	option_propagation_fn := function_window(generated,
		'main__local_option_propagation_cleanup(int n) {')
	option_call_pos := option_propagation_fn.index('main__maybe_int(n)') or {
		assert false, option_propagation_fn
		return
	}
	option_after_call := option_propagation_fn[option_call_pos..]
	option_value_pos := option_after_call.index('int value = ') or {
		assert false, option_propagation_fn
		return
	}
	option_fail_branch := option_after_call[..option_value_pos]
	option_cleanup_pos := option_fail_branch.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, option_propagation_fn
		return
	}
	option_hidden_return_pos := option_fail_branch.index('\treturn ') or {
		assert false, option_propagation_fn
		return
	}
	assert option_cleanup_pos < option_hidden_return_pos

	result_propagation_fn := function_window(generated,
		'main__local_result_propagation_cleanup(int n) {')
	result_call_pos := result_propagation_fn.index('main__result_int(n)') or {
		assert false, result_propagation_fn
		return
	}
	result_after_call := result_propagation_fn[result_call_pos..]
	result_value_pos := result_after_call.index('int value = ') or {
		assert false, result_propagation_fn
		return
	}
	result_fail_branch := result_after_call[..result_value_pos]
	result_cleanup_pos := result_fail_branch.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, result_propagation_fn
		return
	}
	result_hidden_return_pos := result_fail_branch.index('\treturn ') or {
		assert false, result_propagation_fn
		return
	}
	assert result_cleanup_pos < result_hidden_return_pos

	result_handler_anon_fn := function_window_containing(generated, 'main__maybe_request(request)')
	assert !result_handler_anon_fn.contains('builtin__closure__closure_try_destroy((voidptr)handle_request);')

	for_c_init_fn := function_window(generated, 'int main__local_for_c_init_closure(int n) {')
	assert !for_c_init_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	for_c_cond_fn := function_window(generated, 'int main__local_for_c_cond_closure(int n) {')
	assert !for_c_cond_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	for_c_inc_fn := function_window(generated, 'int main__local_for_c_inc_closure(int n) {')
	assert !for_c_inc_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	for_c_init_body_tail_fn := function_window(generated,
		'int main__local_for_c_init_closure_body_tail(int n) {')
	assert for_c_init_body_tail_fn.contains('for (;')
	for_c_init_body_tail_call_pos := for_c_init_body_tail_fn.index('result += h(') or {
		assert false, for_c_init_body_tail_fn
		return
	}
	for_c_init_body_tail_cleanup_pos := for_c_init_body_tail_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, for_c_init_body_tail_fn
		return
	}
	for_c_init_body_tail_return_pos := for_c_init_body_tail_fn.index('return result;') or {
		assert false, for_c_init_body_tail_fn
		return
	}
	assert for_c_init_body_tail_call_pos < for_c_init_body_tail_cleanup_pos
	assert for_c_init_body_tail_cleanup_pos < for_c_init_body_tail_return_pos

	for_c_init_continue_fn := function_window(generated,
		'int main__local_for_c_init_closure_continue(int n) {')
	assert for_c_init_continue_fn.contains('for (;')
	for_c_init_continue_pos := for_c_init_continue_fn.index('continue;') or {
		assert false, for_c_init_continue_fn
		return
	}
	for_c_init_continue_cleanup_pos := for_c_init_continue_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, for_c_init_continue_fn
		return
	}
	for_c_init_continue_return_pos := for_c_init_continue_fn.index('return result;') or {
		assert false, for_c_init_continue_fn
		return
	}
	assert for_c_init_continue_pos < for_c_init_continue_cleanup_pos
	assert for_c_init_continue_cleanup_pos < for_c_init_continue_return_pos

	multi_for_c_init_body_tail_fn := function_window(generated,
		'int main__local_multi_for_c_init_closure_body_tail(int n) {')
	assert multi_for_c_init_body_tail_fn.contains('while (true)')
	assert multi_for_c_init_body_tail_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	for_c_body_continue_fn := function_window(generated,
		'int main__local_for_c_body_closure_continue_cleanup(int n) {')
	for_c_body_continue_cleanup_pos := for_c_body_continue_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, for_c_body_continue_fn
		return
	}
	for_c_body_continue_pos := for_c_body_continue_fn.index('continue;') or {
		assert false, for_c_body_continue_fn
		return
	}
	assert for_c_body_continue_cleanup_pos < for_c_body_continue_pos

	for_c_init_return_fn := function_window(generated,
		'int main__local_for_c_init_closure_return_cleanup(int n) {')
	for_c_init_return_call_pos := for_c_init_return_fn.index(' = h(') or {
		assert false, for_c_init_return_fn
		return
	}
	for_c_init_return_cleanup_pos := for_c_init_return_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, for_c_init_return_fn
		return
	}
	for_c_init_return_pos := for_c_init_return_fn.index('\treturn ') or {
		assert false, for_c_init_return_fn
		return
	}
	assert for_c_init_return_call_pos < for_c_init_return_cleanup_pos
	assert for_c_init_return_cleanup_pos < for_c_init_return_pos

	for_c_init_labeled_break_fn := function_window(generated,
		'int main__local_for_c_init_closure_labeled_break_cleanup(int n) {')
	for_c_init_labeled_break_goto_pos := for_c_init_labeled_break_fn.index('goto closure_init_break__break;') or {
		assert false, for_c_init_labeled_break_fn
		return
	}
	for_c_init_labeled_break_label_pos := for_c_init_labeled_break_fn.index('closure_init_break__break: {}') or {
		assert false, for_c_init_labeled_break_fn
		return
	}
	for_c_init_labeled_break_cleanup_pos := for_c_init_labeled_break_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, for_c_init_labeled_break_fn
		return
	}
	assert for_c_init_labeled_break_goto_pos < for_c_init_labeled_break_label_pos
	assert for_c_init_labeled_break_label_pos < for_c_init_labeled_break_cleanup_pos

	for_c_init_escaped_fn := function_window(generated,
		'void main__local_for_c_init_escaped_closure')
	assert !for_c_init_escaped_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	nested_continue_fn := function_window(generated,
		'int main__local_nested_for_c_init_closure_continue_outer(int n) {')
	nested_continue_inner_cleanup_pos := nested_continue_fn.index('builtin__closure__closure_try_destroy((voidptr)inner_h);') or {
		assert false, nested_continue_fn
		return
	}
	nested_continue_goto_pos := nested_continue_fn.index('goto nested_continue_outer__continue_entry;') or {
		assert false, nested_continue_fn
		return
	}
	nested_continue_outer_cleanup_pos := nested_continue_fn.index('builtin__closure__closure_try_destroy((voidptr)outer_h);') or {
		assert false, nested_continue_fn
		return
	}
	assert nested_continue_inner_cleanup_pos < nested_continue_goto_pos
	assert nested_continue_goto_pos < nested_continue_outer_cleanup_pos

	nested_break_fn := function_window(generated,
		'int main__local_nested_for_c_init_closure_break_outer(int n) {')
	nested_break_inner_cleanup_pos := nested_break_fn.index('builtin__closure__closure_try_destroy((voidptr)inner_h);') or {
		assert false, nested_break_fn
		return
	}
	nested_break_goto_pos := nested_break_fn.index('goto nested_break_outer__break;') or {
		assert false, nested_break_fn
		return
	}
	nested_break_label_pos := nested_break_fn.index('nested_break_outer__break: {}') or {
		assert false, nested_break_fn
		return
	}
	nested_break_outer_cleanup_pos := nested_break_fn.index('builtin__closure__closure_try_destroy((voidptr)outer_h);') or {
		assert false, nested_break_fn
		return
	}
	assert nested_break_inner_cleanup_pos < nested_break_goto_pos
	assert nested_break_goto_pos < nested_break_label_pos
	assert nested_break_label_pos < nested_break_outer_cleanup_pos

	for_cond_fn := function_window(generated, 'int main__local_for_cond_closure(int n) {')
	assert !for_cond_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	for_in_cond_fn := function_window(generated, 'int main__local_for_in_cond_closure(int n) {')
	assert !for_in_cond_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	for_in_high_fn := function_window(generated, 'int main__local_for_in_high_closure(int n) {')
	assert !for_in_high_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	array_init_fn := function_window(generated, 'int main__local_array_init_closure(int n) {')
	assert !array_init_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	if_condition_fn := function_window(generated,
		'int main__local_if_else_condition_closure(int n) {')
	assert !if_condition_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	nested_returning_fn := function_window(generated,
		'main__ZeroCallback main__local_nested_returning_closure(int n) {')
	assert !nested_returning_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')
	assert !nested_returning_fn.contains('builtin__closure__closure_try_destroy((voidptr)nested);')

	nested_stored_fn := function_window(generated, 'void main__local_nested_stored_closure')
	assert !nested_stored_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')
	assert !nested_stored_fn.contains('builtin__closure__closure_try_destroy((voidptr)nested);')

	keyword_fn := function_window(generated, 'void main__local_keyword_closure(void) {')
	assert keyword_fn.contains('(*_v_free)')
	assert keyword_fn.contains('builtin__closure__closure_try_destroy((voidptr)_v_free);')
	assert !keyword_fn.contains('builtin__closure__closure_try_destroy((voidptr)__v_free);')
	assert !keyword_fn.contains('builtin__closure__closure_try_destroy((voidptr)free);')

	labeled_break_fn := function_window(generated, 'int main__local_labeled_break_closure(int n) {')
	labeled_break_cleanup_pos := labeled_break_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, labeled_break_fn
		return
	}
	labeled_break_goto_pos := labeled_break_fn.index('goto closure_break__break;') or {
		assert false, labeled_break_fn
		return
	}
	assert labeled_break_cleanup_pos < labeled_break_goto_pos

	labeled_continue_fn := function_window(generated,
		'int main__local_labeled_continue_closure(int n) {')
	labeled_continue_cleanup_pos := labeled_continue_fn.index('builtin__closure__closure_try_destroy((voidptr)h);') or {
		assert false, labeled_continue_fn
		return
	}
	labeled_continue_goto_pos := labeled_continue_fn.index('goto closure_continue__continue_entry;') or {
		assert false, labeled_continue_fn
		return
	}
	assert labeled_continue_cleanup_pos < labeled_continue_goto_pos

	spawn_fn := function_window(generated, 'void main__local_spawn_closure(void) {')
	assert spawn_fn.contains('/*spawn (thread) */')
	assert !spawn_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	go_fn := function_window(generated, 'void main__local_go_closure(void) {')
	assert go_fn.contains('/*spawn (thread) */') || go_fn.contains('/*go (coroutine) */')
	assert !go_fn.contains('builtin__closure__closure_try_destroy((voidptr)h);')

	compile_cmd := '${test_vexe} -enable-globals -gc none -skip-unused -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	compile_res := os.execute(compile_cmd)
	assert compile_res.exit_code == 0, '${compile_cmd}\n${compile_res.output}'
	run_res := os.execute(os.quoted_path(exe_path))
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.trim_space() == '71'
}
