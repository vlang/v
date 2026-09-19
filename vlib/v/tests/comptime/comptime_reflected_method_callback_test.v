// A method selected by reflection can become reachable only during transform.
// Its bound callbacks (and their dependencies) must not be stripped.
struct ReflectedCallbackWindow {
	increment int
}

fn (w &ReflectedCallbackWindow) run_worker(callback fn (&ReflectedCallbackWindow)) {
	callback(w)
}

struct ReflectedCallbackModel {
mut:
	calls int
}

@[run]
fn (mut m ReflectedCallbackModel) run_file(w &ReflectedCallbackWindow) {
	w.run_worker(m.poll_status)
}

fn (mut m ReflectedCallbackModel) poll_status(w &ReflectedCallbackWindow) {
	m.calls += w.increment
}

struct ChainedReflectedCallbackModel {
mut:
	calls int
}

@[run]
fn (mut m ChainedReflectedCallbackModel) run_file(w &ReflectedCallbackWindow) {
	w.run_worker(m.poll_status)
}

fn (mut m ChainedReflectedCallbackModel) poll_status(w &ReflectedCallbackWindow) {
	w.run_worker(m.record_status)
}

fn (mut m ChainedReflectedCallbackModel) record_status(w &ReflectedCallbackWindow) {
	m.calls += reflected_callback_increment(w.increment)
}

fn reflected_callback_increment(value int) int {
	return value + 1
}

fn invoke_reflected_callbacks[T](mut model T, window &ReflectedCallbackWindow) {
	$for method in T.methods {
		if 'run' in method.attrs {
			model.$method(window)
		}
	}
}

fn test_reflected_method_retains_bound_callback() {
	mut model := ReflectedCallbackModel{}
	window := ReflectedCallbackWindow{increment: 3}
	invoke_reflected_callbacks(mut model, &window)
	assert model.calls == 3
}

fn test_reflected_method_retains_transitive_bound_callback() {
	mut model := ChainedReflectedCallbackModel{}
	window := ReflectedCallbackWindow{increment: 3}
	invoke_reflected_callbacks(mut model, &window)
	assert model.calls == 4
}
