// Regression test for https://github.com/vlang/v/issues/26558
// Multi-return match assignment inside closure should work
// when the closure is a field of a @[heap] struct and the
// struct init is inside a return expression.

@[heap]
struct Cfg {
	handler fn (string) = unsafe { nil }
}

fn call_it(cfg Cfg) {
	if cfg.handler != unsafe { nil } {
		cfg.handler('s')
	}
}

fn make_cfg(ch string) Cfg {
	return Cfg{
		handler: fn [ch] (s string) {
			x, y, z := match ch {
				'h' { f32(1), f32(2), f32(3) }
				's' { f32(4), f32(5), f32(6) }
				else { f32(7), f32(8), f32(9) }
			}
			println('${x} ${y} ${z}')
		}
	}
}

fn test_multi_return_match_in_closure_heap_struct() {
	// This test verifies that the compiler does not panic with:
	// "as cast: cannot cast `v.ast.UnknownTypeInfo` to `v.ast.MultiReturn`"
	call_it(make_cfg('h'))
	call_it(make_cfg('s'))
	call_it(make_cfg('x'))
}
