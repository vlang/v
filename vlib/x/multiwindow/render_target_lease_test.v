module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

fn test_render_target_lease_validates_app_batch_target_window_epoch_generation_and_release() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		mut app := new_app()!
		window := app.create_window(title: 'target lease')!
		lease := RenderTargetLease{
			app_instance: app.instance_id()
			batch_epoch:  7
			target_epoch: 11
			window_epoch: 13
			window:       window
		}
		mut state := &RenderBackendState{
			started:      true
			batch_active: true
			batch_epoch:  7
			targets:      [
				BackendTargetSlot{
					epoch:  11
					lease:  lease
					status: .acquired
				},
			]
		}
		assert state.targets[0].epoch != 0
		assert state.targets[0].epoch == lease.target_epoch
		assert lease.window_epoch != 0
		assert state.targets[0].lease.window_epoch == lease.window_epoch
		assert validate_target_lease(state, app.instance_id(), lease)! == 0

		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			app_instance: app.instance_id() + 1
		}, err_render_target_stale)
		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			batch_epoch: 8
		}, err_render_target_stale)
		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			target_epoch: 0
		}, err_render_target_stale)
		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			target_epoch: 12
		}, err_render_target_stale)
		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			window_epoch: 0
		}, err_render_target_stale)
		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			window_epoch: lease.window_epoch + 1
		}, err_render_target_stale)
		state.targets[0].lease = RenderTargetLease{
			...lease
			window_epoch: 0
		}
		assert_target_lease_error(state, app.instance_id(), lease, err_render_target_stale)
		state.targets[0].lease = lease
		assert_target_lease_error(state, app.instance_id(), RenderTargetLease{
			...lease
			window: WindowId{
				app_instance: app.instance_id()
				slot:         window.slot
				generation:   window.generation + 1
			}
		}, err_render_target_stale)

		copied_before_rotation := lease
		rotated := RenderTargetLease{
			...lease
			window_epoch: lease.window_epoch + 1
		}
		state.targets[0].lease = rotated
		assert state.targets[0].lease.window_epoch != 0
		assert state.targets[0].lease.window_epoch == rotated.window_epoch
		assert_target_lease_error(state, app.instance_id(), copied_before_rotation,
			err_render_target_stale)
		assert validate_target_lease(state, app.instance_id(), rotated)! == 0

		copied_after_rotation := rotated
		state.targets.clear()
		assert_target_lease_error(state, app.instance_id(), copied_after_rotation,
			err_render_target_stale)
		app.stop()!
	} $else {
		return
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn test_render_target_pass_rejects_foreign_thread_before_renderer_or_lease_access() {
		mut app := new_app()!
		result := chan string{cap: 1}
		render_worker := spawn fn [mut app, result] () {
			app.with_render_target_pass(RenderTargetLease{}, gfx.PassAction{}, fn () ! {}) or {
				result <- err.msg()
				return
			}
			result <- 'accepted'
		}()
		assert <-result == err_owner_thread_required
		render_worker.wait()
		app.stop()!
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn assert_target_lease_error(state &RenderBackendState, app_instance u64, lease RenderTargetLease, expected string) {
		mut actual := ''
		validate_target_lease(state, app_instance, lease) or { actual = err.msg() }
		assert actual == expected, 'target lease error was `${actual}`, expected `${expected}`'
	}
}
