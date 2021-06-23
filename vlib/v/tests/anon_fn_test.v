import sync

fn test_go_anon_fn() {
	mut wg := sync.new_waitgroup()
	wg.add(1)
	go fn (mut wg sync.WaitGroup) {
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
