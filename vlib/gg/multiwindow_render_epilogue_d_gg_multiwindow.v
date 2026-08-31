module gg

struct MultiWindowRenderEpilogue {
	window          WindowId
	lease_epoch     u64
	pass_epoch      u64
	finish_frame    bool
	target_attached bool
	sgl_flushed     bool
}

// finish_render_epilogue is the sole pure-state unwind for managed render
// operations. It never short-circuits: pass, frame lease and target attachment
// are each closed or forcibly invalidated before aggregated errors return.
fn (mut app App) finish_render_epilogue(epilogue MultiWindowRenderEpilogue, prior_errors []IError) ! {
	mut errors := prior_errors.clone()
	if epilogue.pass_epoch != 0 {
		app.render_runtime.finish_pass(epilogue.window, epilogue.lease_epoch, epilogue.pass_epoch,
			epilogue.sgl_flushed) or {
			errors << err
			app.render_runtime.abort_pass_state(epilogue.window, epilogue.lease_epoch,
				epilogue.pass_epoch)
		}
	}
	if epilogue.finish_frame && epilogue.lease_epoch != 0 {
		app.render_runtime.finish_frame_lease(epilogue.window, epilogue.lease_epoch) or {
			errors << err
			app.render_runtime.abort_frame_lease_state(epilogue.window, epilogue.lease_epoch)
		}
	}
	if epilogue.target_attached {
		app.render_runtime.detach_render_target(epilogue.window)
	}
	if errors.len > 0 {
		return aggregate_managed_ierror(err_multiwindow_render_callback_failed, errors)
	}
}

fn aggregate_managed_ierror(prefix string, errors []IError) IError {
	if errors.len == 1 {
		return errors[0]
	}
	mut messages := []string{cap: errors.len}
	for item in errors {
		messages << item.msg()
	}
	return error('${prefix}: ${unique_managed_errors(messages).join('; ')}')
}

fn unique_managed_errors(errors []string) []string {
	mut unique := []string{}
	for message in errors {
		if message != '' && message !in unique {
			unique << message
		}
	}
	return unique
}
