import sync

fn test_go_anon_fn() {
	mut wg := sync.new_waitgroup()
	wg.add(1)
	spawn fn (mut wg sync.WaitGroup) {
		wg.done()
	}(mut wg)
	wg.wait()
}

struct AnonFnWrapper {
mut:
	fn_ fn () bool
}

fn test_anon_assign_struct() {
	mut w := AnonFnWrapper{}
	w.fn_ = fn () bool {
		return true
	}
	assert w.fn_()
}

//

fn fnormal(mut acc []string, e int) []string {
	acc << e.str()
	return acc
}

fn test_anon_fn_returning_a_mut_parameter_should_act_the_same_as_normal_fn_returning_a_mut_parameter() {
	fanon := fn (mut acc []string, e int) []string {
		acc << e.str()
		return acc
	}
	assert '${fanon}' == '${fnormal}'
	mut a := ['a', 'b', 'c']
	mut b := a.clone()
	x := fanon(mut a, 123)
	y := fnormal(mut b, 123)
	assert a == b
}

// for issue 20163
struct Struct {}

fn test_spawn_anon_fn_with_closure_parameters_and_mut_ref_parameters() {
	mut s := &Struct{}
	a := 1

	t := spawn fn [a] (mut s Struct) int {
		return a
	}(mut s)
	assert t.wait() == 1
}

struct CallbackEvent {}

fn call_event_handler(cb fn (CallbackEvent) bool) bool {
	return cb(CallbackEvent{})
}

fn call_two_arg_handler(cb fn (CallbackEvent, int) int) int {
	return cb(CallbackEvent{}, 7)
}

fn test_anon_fn_can_omit_all_unused_callback_params() {
	assert call_event_handler(fn () bool {
		return true
	})
}

fn test_anon_fn_can_omit_trailing_callback_params() {
	assert call_two_arg_handler(fn (_ CallbackEvent) int {
		return 42
	}) == 42
}
