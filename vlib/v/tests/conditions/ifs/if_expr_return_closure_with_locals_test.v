module main

import time

struct Event {
mut:
	value int
}

@[heap]
struct Cfg {
	wrap     bool
	delay    time.Duration
	callback fn (&int, mut Event) = unsafe { nil }
}

fn resolve(cfg Cfg) fn (&int, mut Event) {
	user_cb := cfg.callback
	result := if cfg.wrap && cfg.delay > 0 {
		tag := 'tag'
		dur := cfg.delay
		fn [user_cb, tag, dur] (x &int, mut e Event) {
			_ = dur
			if user_cb != unsafe { nil } {
				user_cb(x, mut e)
			}
			e.value += 1
			println('wrapped(${tag}): ${e.value}')
		}
	} else {
		cfg.callback
	}
	return result
}

fn test_if_expr_can_return_closure_after_local_decls() {
	cb := resolve(Cfg{
		wrap:     true
		delay:    2 * time.second
		callback: fn (x &int, mut e Event) {
			e.value = *x
		}
	})
	n := 41
	mut e := Event{}
	cb(&n, mut e)
	assert e.value == 42
}
