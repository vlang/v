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

// Interface dispatch targets discovered in late-used bodies must be retained
// too, including a second interface callback reached through an implementation.
fn (w &ReflectedCallbackWindow) run_value_worker(callback fn (&ReflectedCallbackWindow) int) int {
	return callback(w)
}

interface ReflectedCallbackPoller {
	poll(w &ReflectedCallbackWindow) int
}

interface ReflectedCallbackFinisher {
	finish(w &ReflectedCallbackWindow) int
}

struct ReflectedInterfaceCallbackModel {
	poller ReflectedCallbackPoller
mut:
	calls int
}

@[run]
fn (mut m ReflectedInterfaceCallbackModel) run_file(w &ReflectedCallbackWindow) {
	m.calls = w.run_value_worker(m.poller.poll)
}

struct ReflectedCallbackPollerImpl {
	offset int
}

fn (p ReflectedCallbackPollerImpl) poll(w &ReflectedCallbackWindow) int {
	return w.increment + p.offset
}

struct ChainedReflectedCallbackPoller {
	finisher ReflectedCallbackFinisher
}

fn (p ChainedReflectedCallbackPoller) poll(w &ReflectedCallbackWindow) int {
	return w.run_value_worker(p.finisher.finish)
}

struct ReflectedCallbackFinisherImpl {
	offset int
}

fn (f ReflectedCallbackFinisherImpl) finish(w &ReflectedCallbackWindow) int {
	return reflected_interface_callback_increment(w.increment + f.offset)
}

fn reflected_interface_callback_increment(value int) int {
	return value + 1
}

fn test_reflected_method_retains_interface_callback_implementations() {
	window := ReflectedCallbackWindow{increment: 3}
	pollers := [
		ReflectedCallbackPoller(ReflectedCallbackPollerImpl{offset: 2}),
		ReflectedCallbackPoller(ChainedReflectedCallbackPoller{
			finisher: ReflectedCallbackFinisherImpl{offset: 5}
		}),
	]
	expected := [5, 9]
	for i, poller in pollers {
		mut model := ReflectedInterfaceCallbackModel{poller: poller}
		invoke_reflected_callbacks(mut model, &window)
		assert model.calls == expected[i]
	}
}
