struct Window {
mut:
	calls   int
	on_init fn (mut w Window)        = unsafe { nil }
	cb      fn (mut w Window, x int) = unsafe { nil }
}

fn (mut w Window) run() {
	w.on_init(mut w)
	w.cb(mut w, w.calls)
}

fn (mut w Window) method_with_mut_receiver() {
	w.calls++
}

fn test_mut_lambda_expr() {
	mut window := Window{
		on_init: |mut w| w.method_with_mut_receiver()
		cb:      |mut w, mut y| unsafe {
			if true {
				assert y == 1
				w.calls++
			}
		}
	}
	window.run()
	assert window.calls == 2
}
