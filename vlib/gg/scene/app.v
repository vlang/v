module scene

// System is one composable addon with optional lifecycle hooks. Defaults are
// no-ops, so a system can implement only the stages it needs.
pub struct System {
pub:
	name    string
	init    fn (mut App)      = app_noop
	frame   fn (mut App, f32) = app_frame_noop
	cleanup fn (mut App)      = app_noop
}

fn app_noop(mut a App) {
	_ = a
}

fn app_frame_noop(mut a App, dt f32) {
	_ = a
	_ = dt
}

// App runs a list of systems in registration order, like a tiny Bevy-style
// plugin container. It holds no globals, so several can coexist.
pub struct App {
pub mut:
	systems []System
	running bool
	elapsed f32
}

// new_app returns an empty app.
pub fn new_app() App {
	return App{}
}

// add registers a system and returns its index.
pub fn (mut a App) add(system System) int {
	a.systems << system
	return a.systems.len - 1
}

// start initializes every system and marks the app running.
pub fn (mut a App) start() {
	for s in a.systems {
		s.init(mut a)
	}
	a.running = true
}

// update advances every system by dt when running.
pub fn (mut a App) update(dt f32) {
	if !a.running {
		return
	}
	a.elapsed += dt
	for s in a.systems {
		s.frame(mut a, dt)
	}
}

// stop cleans up every system in reverse order.
pub fn (mut a App) stop() {
	if !a.running {
		return
	}
	for i := a.systems.len - 1; i >= 0; i-- {
		a.systems[i].cleanup(mut a)
	}
	a.running = false
}
