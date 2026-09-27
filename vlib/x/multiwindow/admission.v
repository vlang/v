module multiwindow

fn (app &App) accepted_wrapper_is_current(app_instance u64, admission_epoch u64) bool {
	app.state_mutex.lock()
	current := app.instance_id == app_instance && app.admission_open && !app.stopping
		&& app.status == .running && app.admission_epoch == admission_epoch
	app.state_mutex.unlock()
	return current
}

fn (mut app App) close_admission_locked() {
	if !app.admission_open {
		return
	}
	app.admission_open = false
	if app.admission_epoch != u64(0xffffffffffffffff) {
		app.admission_epoch++
	}
}

fn (mut app App) begin_owner_callback() {
	app.state_mutex.lock()
	app.owner_callback_depth++
	app.state_mutex.unlock()
}

fn (mut app App) end_owner_callback() {
	app.state_mutex.lock()
	if app.owner_callback_depth > 0 {
		app.owner_callback_depth--
	}
	app.state_mutex.unlock()
}

fn (mut app App) drain_cancelled_wrappers() []string {
	mut errors := []string{}
	for {
		drained := app.owner.drain_pending(app.config.queue_size) or {
			errors << err.msg()
			break
		}
		if drained == 0 {
			break
		}
	}
	return errors
}
