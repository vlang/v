type GuardedFn = fn (arg Bar) bool

type GuardedResultFn = fn (arg Bar) !bool

struct GuardedFoo {
	f ?GuardedFn
}

struct GuardedResultFoo {
	f ?GuardedResultFn
}

@[params]
struct Bar {}

fn (foo GuardedFoo) call(arg Bar) bool {
	if foo.f != none {
		// vfmt off
		return foo.f or { panic(err) }(arg)
		// vfmt on
	}
	return false
}

fn (foo GuardedResultFoo) call(arg Bar) bool {
	if foo.f != none {
		// vfmt off
		return foo.f or { panic(err) }(arg) or { return false }
		// vfmt on
	}
	return false
}

fn test_optional_fn_selector_call_in_if_guard() {
	foo := GuardedFoo{
		f: fn (arg Bar) bool {
			_ = arg
			return true
		}
	}
	assert foo.call(Bar{})
}

fn test_optional_fn_selector_call_with_nested_or_blocks() {
	ok := GuardedResultFoo{
		f: fn (arg Bar) !bool {
			_ = arg
			return true
		}
	}
	failing := GuardedResultFoo{
		f: fn (arg Bar) !bool {
			_ = arg
			return error('boom')
		}
	}
	assert ok.call(Bar{})
	assert !failing.call(Bar{})
}
