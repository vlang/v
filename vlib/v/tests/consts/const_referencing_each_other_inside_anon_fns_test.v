// Regression test for https://github.com/vlang/v/issues/27087
// Constants that reference each other only inside anonymous function bodies
// are not a real initialisation-time cycle (the bodies run at runtime).

type FnPtr = fn (voidptr)

fn FnPtr.nop(_ voidptr) {}

struct AppViewFn {
mut:
	on_event  FnPtr = FnPtr.nop
	on_update FnPtr = FnPtr.nop
	on_draw   FnPtr = FnPtr.nop
}

struct AppView {
mut:
	on_fn AppViewFn
}

const fn_idle = AppViewFn{
	on_event: fn (mut view AppView) {
		view.on_fn = fn_track
	}
}

const fn_track = AppViewFn{
	on_event: fn (mut view AppView) {
		view.on_fn = fn_idle
	}
}

fn test_mutual_const_references_inside_anon_fn_bodies_compile() {
	v := AppView{
		on_fn: fn_idle
	}
	assert voidptr(v.on_fn.on_event) != unsafe { nil }
}
