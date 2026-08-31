// Regression test for https://github.com/vlang/v/issues/28079
// `spawn obj.method(none)` / `spawn obj.method(value)` where the method takes an
// `?T` parameter used to generate a thread-argument wrapper whose field was typed
// `none` instead of the option type, producing invalid C that failed to compile.

struct StartParams {
	x int
}

struct App {
	name string
}

fn (a &App) run(params ?StartParams) int {
	if p := params {
		return p.x
	}
	return -1
}

fn test_spawn_method_with_none_option_arg() {
	a := &App{
		name: 'app'
	}
	t := spawn a.run(none)
	assert t.wait() == -1
}

fn test_spawn_method_with_value_option_arg() {
	a := &App{
		name: 'app'
	}
	t := spawn a.run(StartParams{ x: 42 })
	assert t.wait() == 42
}

fn plain(params ?StartParams) int {
	return if p := params { p.x } else { -7 }
}

fn test_spawn_plain_fn_with_option_arg() {
	t1 := spawn plain(none)
	assert t1.wait() == -7
	t2 := spawn plain(StartParams{ x: 5 })
	assert t2.wait() == 5
}
