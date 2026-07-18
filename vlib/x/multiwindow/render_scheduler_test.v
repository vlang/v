module multiwindow

struct RenderSchedulerWrapperSeen {
mut:
	ran bool
}

fn test_render_scheduler_foreign_redraw_coalesces_one_owner_wrapper() {
	mut app := new_app(queue_size: 2)!
	window := app.create_window(title: 'redraw coalescing')!
	results := chan string{cap: 2}

	redraw_worker := spawn fn [mut app, window, results] () {
		for _ in 0 .. 2 {
			app.request_redraw(window) or {
				results <- err.msg()
				continue
			}
			results <- 'ok'
		}
	}()
	assert <-results == 'ok'
	assert <-results == 'ok'
	redraw_worker.wait()
	assert app.drain_pending(2)! == 1
	assert app.drain_pending(1)! == 0
	app.stop()!
}

fn test_render_scheduler_queue_full_rolls_back_foreign_redraw_admission() {
	mut app := new_app(queue_size: 1)!
	window := app.create_window(title: 'redraw queue rollback')!
	app.try_post(fn (mut queued_app App) ! {
		_ = queued_app.status()
	})!
	first_result := chan string{cap: 1}
	first := spawn fn [mut app, window, first_result] () {
		app.request_redraw(window) or {
			first_result <- err.msg()
			return
		}
		first_result <- 'ok'
	}()
	assert <-first_result == err_owner_queue_full
	first.wait()
	assert app.drain_pending(1)! == 1

	second_result := chan string{cap: 1}
	second := spawn fn [mut app, window, second_result] () {
		app.request_redraw(window) or {
			second_result <- err.msg()
			return
		}
		second_result <- 'ok'
	}()
	assert <-second_result == 'ok'
	second.wait()
	assert app.drain_pending(1)! == 1
	app.stop()!
}

fn test_render_scheduler_foreign_redraw_rejects_stale_and_stopped_apps_without_queueing() {
	mut app := new_app(queue_size: 2)!
	stale := app.create_window(title: 'stale redraw')!
	app.destroy_window(stale)!
	replacement := app.create_window(title: 'replacement redraw')!
	stale_result := chan string{cap: 1}
	stale_thread := spawn fn [mut app, stale, stale_result] () {
		app.request_redraw(stale) or {
			stale_result <- err.msg()
			return
		}
		stale_result <- 'ok'
	}()
	assert <-stale_result == err_stale_window
	stale_thread.wait()
	assert app.drain_pending(1)! == 0
	assert app.window_exists(replacement)
	app.stop()!

	stopped_result := chan string{cap: 1}
	stopped_thread := spawn fn [mut app, replacement, stopped_result] () {
		app.request_redraw(replacement) or {
			stopped_result <- err.msg()
			return
		}
		stopped_result <- 'ok'
	}()
	assert <-stopped_result == err_app_stopped
	stopped_thread.wait()
	app.drain_pending(1) or {
		assert err.msg() == err_app_stopped
		return
	}
	assert false, 'stopped app drained redraw work'
}

fn test_render_scheduler_owner_redraw_rejects_destroyed_window_without_mutation() {
	mut app := new_app(queue_size: 2)!
	stale := app.create_window(title: 'owner stale redraw')!
	app.state_mutex.lock()
	app.render_runtime.windows[stale.slot].consumed_dirty_epoch = app.render_runtime.windows[stale.slot].dirty_epoch
	app.state_mutex.unlock()
	app.destroy_window(stale)!

	app.state_mutex.lock()
	before_dirty_epoch := app.render_runtime.windows[stale.slot].dirty_epoch
	before_pending_admission := app.render_runtime.windows[stale.slot].pending_admission
	before_pending_admission_id := app.render_runtime.windows[stale.slot].pending_admission_id
	before_pending_admission_epoch := app.render_runtime.windows[stale.slot].pending_admission_epoch
	before_next_epoch := app.render_runtime.next_epoch
	before_next_admission_id := app.render_runtime.next_admission_id
	app.state_mutex.unlock()

	mut redraw_error := ''
	app.request_redraw(stale) or { redraw_error = err.msg() }
	assert redraw_error == err_stale_window

	app.state_mutex.lock()
	after := app.render_runtime.windows[stale.slot]
	after_next_epoch := app.render_runtime.next_epoch
	after_next_admission_id := app.render_runtime.next_admission_id
	app.state_mutex.unlock()
	assert after.status == .destroyed
	assert after.dirty_epoch == before_dirty_epoch
	assert after.pending_admission == before_pending_admission
	assert after.pending_admission_id == before_pending_admission_id
	assert after.pending_admission_epoch == before_pending_admission_epoch
	assert after_next_epoch == before_next_epoch
	assert after_next_admission_id == before_next_admission_id
	assert app.drain_pending(1)! == 0
	app.stop()!
}

fn test_render_scheduler_stop_drains_accepted_wrapper_as_cancelled_without_running_user_code() {
	mut app := new_app(queue_size: 1)!
	mut seen := &RenderSchedulerWrapperSeen{}
	app.try_post(fn [mut seen] (mut queued_app App) ! {
		_ = queued_app.status()
		seen.ran = true
	})!
	ticket := app.prepare_stop()!
	assert !seen.ran, 'accepted executor wrapper invoked user code while draining after stop'
	assert app.owner.drain_pending(1)! == 0
	app.finish_stop(ticket, [])!
	app.drain_pending(1) or {
		assert err.msg() == err_app_stopped
		return
	}
	assert false, 'stopped app drained an accepted executor wrapper'
}

fn test_render_scheduler_frame_serial_is_distinct_from_submitted_frame() {
	mut app := new_app(queue_size: 4)!
	window := app.create_window(title: 'frame accounting', render_workload: true)!
	publish_ready_credit_for_test(mut app, window, 1, 320, 200)

	first_batch := begin_scheduler_batch_for_test(mut app)!
	first_index, first_target, first_candidate := claim_render_target_for_test(mut app, window,
		first_batch)!
	assert first_candidate.frame_serial == 1
	assert first_candidate.submitted_frame == 0
	assert app.render_window_snapshot(window)!.frame_serial == 0
	first := complete_scheduler_acquisition_for_test(mut app, first_index, first_batch,
		first_target, first_candidate)!
	assert first.frame_serial == 1
	assert first.submitted_frame == 0
	app.complete_render_submission(window, first_batch.epoch, false)
	app.finish_scheduler_batch(first_batch.epoch)
	failed := app.render_window_snapshot(window)!
	assert failed.frame_serial == 1
	assert failed.submitted_frame == 0
	assert failed.consumed_epoch == 0
	assert !app.render_window_eligible(window)!, 'consumed ready credit was reused after failed finalization'

	publish_ready_credit_for_test(mut app, window, 3, 320, 200)
	assert app.render_window_eligible(window)!

	second_batch := begin_scheduler_batch_for_test(mut app)!
	second_index, second_target, second_candidate := claim_render_target_for_test(mut app, window,
		second_batch)!
	second := complete_scheduler_acquisition_for_test(mut app, second_index, second_batch,
		second_target, second_candidate)!
	assert second.frame_serial == 2
	assert second.submitted_frame == 0
	app.complete_render_submission(window, second_batch.epoch, true)
	app.finish_scheduler_batch(second_batch.epoch)
	submitted := app.render_window_snapshot(window)!
	assert submitted.frame_serial == 2
	assert submitted.submitted_frame == 1
	assert submitted.consumed_epoch == second_candidate.dirty_epoch
	assert !app.render_window_eligible(window)!
	app.stop()!
}

fn test_render_scheduler_redraw_during_frame_survives_that_frame_submission() {
	mut app := new_app(queue_size: 4)!
	window := app.create_window(title: 'redraw during frame', render_workload: true)!
	publish_ready_credit_for_test(mut app, window, 1, 400, 240)
	first_batch := begin_scheduler_batch_for_test(mut app)!
	first_index, first_target, first_candidate := claim_render_target_for_test(mut app, window,
		first_batch)!

	app.request_redraw(window)!
	during := app.render_window_snapshot(window)!
	assert during.dirty_epoch > first_candidate.dirty_epoch
	_ = complete_scheduler_acquisition_for_test(mut app, first_index, first_batch, first_target,
		first_candidate)!
	app.complete_render_submission(window, first_batch.epoch, true)
	app.finish_scheduler_batch(first_batch.epoch)
	assert !app.render_window_eligible(window)!, 'a redraw cannot bypass backend readiness'

	publish_ready_credit_for_test(mut app, window, 3, 400, 240)
	assert app.render_window_eligible(window)!
	second_batch := begin_scheduler_batch_for_test(mut app)!
	second_index, second_target, second_candidate := claim_render_target_for_test(mut app, window,
		second_batch)!
	second := complete_scheduler_acquisition_for_test(mut app, second_index, second_batch,
		second_target, second_candidate)!
	assert second.frame_serial == 2
	assert second.submitted_frame == 1
	assert second_candidate.dirty_epoch == during.dirty_epoch
	app.complete_render_submission(window, second_batch.epoch, true)
	app.finish_scheduler_batch(second_batch.epoch)
	after := app.render_window_snapshot(window)!
	assert after.submitted_frame == 2
	assert after.consumed_epoch == during.dirty_epoch
	assert !app.render_window_eligible(window)!
	app.stop()!
}

fn test_render_scheduler_rejects_copied_batch_lease_after_transaction_finishes() {
	mut app := new_app()!
	window := app.create_window(title: 'copied batch lease', render_workload: true)!
	publish_ready_credit_for_test(mut app, window, 1, 300, 180)
	lease := begin_scheduler_batch_for_test(mut app)!
	copied := lease
	app.finish_scheduler_batch(lease.epoch)

	claim_render_target_for_test(mut app, window, copied) or {
		assert err.msg() == err_render_batch_stale
		app.stop()!
		return
	}
	assert false, 'copied render batch lease remained active after transaction completion'
}

fn test_render_scheduler_window_lease_epoch_is_nonzero_expires_and_rotates() {
	mut app := new_app()!
	window := app.create_window(title: 'window lease epoch', render_workload: true)!
	publish_ready_credit_for_test(mut app, window, 1, 320, 180)
	first_batch := begin_scheduler_batch_for_test(mut app)!
	first_index, first_target, first_candidate := claim_render_target_for_test(mut app, window,
		first_batch)!
	_ = complete_scheduler_acquisition_for_test(mut app, first_index, first_batch, first_target,
		first_candidate)!
	first_epoch := app.render_window_lease_epoch(window, first_batch)!
	assert first_epoch != 0
	assert app.complete_render_submission(window, first_batch.epoch, true) == ''
	app.render_window_lease_epoch(window, first_batch) or {
		assert err.msg() == err_render_attempt_stale
		app.finish_scheduler_batch(first_batch.epoch)

		app.request_redraw(window)!
		publish_ready_credit_for_test(mut app, window, 3, 320, 180)
		second_batch := begin_scheduler_batch_for_test(mut app)!
		second_index, second_target, second_candidate := claim_render_target_for_test(mut app,
			window, second_batch)!
		_ = complete_scheduler_acquisition_for_test(mut app, second_index, second_batch,
			second_target, second_candidate)!
		second_epoch := app.render_window_lease_epoch(window, second_batch)!
		assert second_epoch != 0
		assert second_epoch != first_epoch
		assert app.complete_render_submission(window, second_batch.epoch, true) == ''
		app.finish_scheduler_batch(second_batch.epoch)
		app.stop()!
		return
	}
	assert false, 'completed target retained its expired window lease epoch'
}

fn test_render_scheduler_window_state_is_generation_scoped_after_slot_reuse() {
	mut app := new_app()!
	stale := app.create_window(title: 'first generation', render_workload: true)!
	publish_ready_credit_for_test(mut app, stale, 1, 200, 120)
	app.destroy_window(stale)!
	replacement := app.create_window(title: 'second generation', render_workload: true)!
	assert replacement.slot == stale.slot
	assert replacement.generation == stale.generation + 1

	app.render_window_snapshot(stale) or {
		assert err.msg() == err_stale_window
		replacement_snapshot := app.render_window_snapshot(replacement)!
		assert replacement_snapshot.frame_serial == 0
		assert replacement_snapshot.submitted_frame == 0
		assert replacement_snapshot.metrics.metrics_sequence == 0
		assert !replacement_snapshot.metrics.metrics_available
		app.stop()!
		return
	}
	assert false, 'stale generation retained render scheduler access'
}

fn test_render_scheduler_tracks_two_window_metrics_and_frames_independently() {
	mut app := new_app()!
	main_window := app.create_window(
		title:           'main'
		width:           800
		height:          600
		render_workload: true
	)!
	tool_window := app.create_window(
		title:           'tool'
		width:           320
		height:          240
		redraw_mode:     .continuous
		render_workload: true
	)!
	publish_render_update_for_test(mut app, BackendRenderUpdate{
		window:       main_window
		sequence:     1
		ready_credit: true
		block_reason: .none
		metrics:      render_metrics_for_test(400, 300, 800, 600, 2, 1)
		target:       render_target_for_test(101)
	})
	publish_render_update_for_test(mut app, BackendRenderUpdate{
		window:       tool_window
		sequence:     1
		ready_credit: true
		block_reason: .none
		metrics:      render_metrics_for_test(320, 240, 320, 240, 1, 1)
		target:       render_target_for_test(202)
	})

	main_batch := begin_scheduler_batch_for_test(mut app)!
	main_index, main_target, main_candidate := claim_render_target_for_test(mut app, main_window,
		main_batch)!
	_ = complete_scheduler_acquisition_for_test(mut app, main_index, main_batch, main_target,
		main_candidate)!
	app.complete_render_submission(main_window, main_batch.epoch, true)
	app.finish_scheduler_batch(main_batch.epoch)

	publish_ready_credit_for_test(mut app, tool_window, 2, 320, 240)
	tool_first_batch := begin_scheduler_batch_for_test(mut app)!
	tool_first_index, tool_first_target, tool_first_candidate := claim_render_target_for_test(mut app,
		tool_window, tool_first_batch)!
	_ = complete_scheduler_acquisition_for_test(mut app, tool_first_index, tool_first_batch,
		tool_first_target, tool_first_candidate)!
	app.complete_render_submission(tool_window, tool_first_batch.epoch, true)
	app.finish_scheduler_batch(tool_first_batch.epoch)

	publish_ready_credit_for_test(mut app, tool_window, 4, 320, 240)
	tool_second_batch := begin_scheduler_batch_for_test(mut app)!
	tool_second_index, tool_second_target, tool_second_candidate := claim_render_target_for_test(mut app,
		tool_window, tool_second_batch)!
	_ = complete_scheduler_acquisition_for_test(mut app, tool_second_index, tool_second_batch,
		tool_second_target, tool_second_candidate)!
	app.complete_render_submission(tool_window, tool_second_batch.epoch, true)
	app.finish_scheduler_batch(tool_second_batch.epoch)

	main_snapshot := app.render_window_snapshot(main_window)!
	tool_snapshot := app.render_window_snapshot(tool_window)!
	assert main_snapshot.metrics.logical_width == 400
	assert main_snapshot.metrics.logical_height == 300
	assert main_snapshot.metrics.framebuffer_width == 800
	assert main_snapshot.metrics.framebuffer_height == 600
	assert main_snapshot.metrics.dpi_scale == 2
	assert main_snapshot.metrics.metrics_sequence == 1
	assert main_snapshot.submitted_frame == 1
	assert main_snapshot.target.target_identity == 101
	assert tool_snapshot.metrics.logical_width == 320
	assert tool_snapshot.metrics.logical_height == 240
	assert tool_snapshot.metrics.dpi_scale == 1
	assert tool_snapshot.metrics.metrics_sequence == 1
	assert tool_snapshot.submitted_frame == 2
	assert tool_snapshot.target.target_identity == 202
	assert !app.render_window_eligible(tool_window)!, 'continuous mode still requires a fresh backend credit'
	publish_ready_credit_for_test(mut app, tool_window, 6, 320, 240)
	assert app.render_window_eligible(tool_window)!
	app.stop()!
}

fn test_render_scheduler_eligibility_requires_workload_and_one_shot_backend_credit() {
	mut app := new_app()!
	window := app.create_window(title: 'eligibility')!
	publish_ready_credit_for_test(mut app, window, 1, 640, 360)
	assert !app.render_window_eligible(window)!, 'ready credit fabricated a workload'
	app.set_render_workload(window, true)!
	assert app.render_window_eligible(window)!

	batch := begin_scheduler_batch_for_test(mut app)!
	index, _, _ := claim_render_target_for_test(mut app, window, batch)!
	app.cancel_render_target_claim(index, batch, .drawable_unavailable)
	app.finish_scheduler_batch(batch.epoch)
	snapshot := app.render_window_snapshot(window)!
	assert snapshot.frame_serial == 0, 'transient acquisition consumed a frame serial'
	assert snapshot.submitted_frame == 0
	assert snapshot.block_reason == .drawable_unavailable
	assert !app.render_window_eligible(window)!, 'one-shot ready credit was reused after acquisition'

	mut sequence := u64(2)
	for reason in [RenderBlockReason.not_configured, .frame_callback_pending, .hidden, .minimized,
		.occluded, .unmapped, .not_viewable, .zero_sized, .resize_pending, .drawable_unavailable,
		.backend_unavailable, .renderer_failed] {
		publish_render_update_for_test(mut app, BackendRenderUpdate{
			window:       window
			sequence:     sequence
			ready_credit: false
			block_reason: reason
			metrics:      render_metrics_for_test(640, 360, 1280, 720, 2, sequence)
			target:       render_target_for_test(77)
		})
		assert !app.render_window_eligible(window)!, '${reason} target became eligible'
		assert app.render_window_snapshot(window)!.block_reason == reason
		sequence++
	}
	publish_ready_credit_for_test(mut app, window, sequence, 640, 360)
	assert app.render_window_eligible(window)!
	app.stop()!
}

fn test_render_scheduler_ignores_stale_backend_snapshots_and_never_fabricates_metrics() {
	mut app := new_app()!
	window := app.create_window(title: 'authoritative metrics', render_workload: true)!
	initial := app.render_window_snapshot(window)!
	assert !initial.metrics.metrics_available
	assert !initial.metrics.conversion_available
	assert initial.metrics.dpi_scale == 0

	publish_render_update_for_test(mut app, BackendRenderUpdate{
		window:       window
		sequence:     10
		ready_credit: true
		block_reason: .none
		metrics:      render_metrics_for_test(500, 250, 1000, 500, 2, 10)
		target:       render_target_for_test(900)
	})
	publish_render_update_for_test(mut app, BackendRenderUpdate{
		window:       window
		sequence:     9
		ready_credit: false
		block_reason: .occluded
		metrics:      render_metrics_for_test(1, 1, 1, 1, 1, 9)
		target:       render_target_for_test(1)
	})
	snapshot := app.render_window_snapshot(window)!
	assert snapshot.eligibility_sequence == 10
	assert snapshot.block_reason == .none
	assert snapshot.metrics.metrics_sequence == 10
	assert snapshot.metrics.logical_width == 500
	assert snapshot.metrics.framebuffer_width == 1000
	assert snapshot.metrics.conversion_available
	assert snapshot.target.target_identity == 900
	assert app.render_window_eligible(window)!
	app.stop()!
}

fn test_render_scheduler_batch_lease_rejects_foreign_app_owner_thread_and_generation() {
	mut owner := new_app()!
	owner_window := owner.create_window(title: 'lease owner', render_workload: true)!
	publish_ready_credit_for_test(mut owner, owner_window, 1, 200, 100)
	lease := begin_scheduler_batch_for_test(mut owner)!

	mut foreign := new_app()!
	foreign_window := foreign.create_window(title: 'foreign app', render_workload: true)!
	publish_ready_credit_for_test(mut foreign, foreign_window, 1, 200, 100)
	claim_render_target_for_test(mut foreign, foreign_window, lease) or {
		assert err.msg() == err_app_identity_mismatch
		owner.finish_scheduler_batch(lease.epoch)
		owner.destroy_window(owner_window)!
		replacement := owner.create_window(title: 'new generation', render_workload: true)!
		assert replacement.slot == owner_window.slot
		generation_lease := begin_scheduler_batch_for_test(mut owner)!
		claim_render_target_for_test(mut owner, owner_window, generation_lease) or {
			assert err.msg() == err_stale_window
			owner.finish_scheduler_batch(generation_lease.epoch)
			owner.stop()!
			foreign.stop()!
			return
		}
		assert false, 'stale window generation acquired a render target'
	}
	assert false, 'foreign App accepted a render batch lease'
}

fn test_render_scheduler_batch_entry_rejects_foreign_thread_before_backend_work() {
	mut app := new_app()!
	result := chan string{cap: 1}
	batch_worker := spawn fn [mut app, result] () {
		app.with_scheduled_render_batch(fn (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {}) or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	assert <-result == err_owner_thread_required
	batch_worker.wait()
	app.stop()!
}

fn begin_scheduler_batch_for_test(mut app App) !RenderBatchLease {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	plan := app.plan_render_batch_locked(false, false)!
	app.commit_render_batch_plan_locked(plan)
	return RenderBatchLease{
		app_instance: app.instance_id
		epoch:        plan.epoch
	}
}

fn claim_render_target_for_test(mut app App, id WindowId, batch RenderBatchLease) !(int, RenderTargetLease, RenderWindowSnapshot) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	next_target_epoch := app.render_runtime.next_lease_epoch
	plan := app.plan_render_target_claim_locked(id, batch, next_target_epoch)!
	app.commit_render_target_claim_locked(plan)
	return plan.index, RenderTargetLease{
		app_instance: app.instance_id
		batch_epoch:  batch.epoch
		target_epoch: plan.target_epoch
		window_epoch: plan.window_epoch
		window:       id
	}, plan.candidate
}

fn complete_scheduler_acquisition_for_test(mut app App, index int, batch RenderBatchLease, target RenderTargetLease, candidate RenderWindowSnapshot) !RenderWindowSnapshot {
	expected := app.complete_render_target_preparation(index, batch, candidate)!
	return app.linearize_render_target_acquisition(target, expected, candidate.metrics,
		candidate.target)
}

fn publish_ready_credit_for_test(mut app App, window WindowId, sequence u64, width int, height int) {
	current := app.render_window_snapshot(window) or { RenderWindowSnapshot{} }
	metrics := if current.metrics.metrics_available {
		current.metrics
	} else {
		render_metrics_for_test(f32(width), f32(height), width, height, 1, sequence)
	}
	target := if current.target.target_identity != 0 {
		current.target
	} else {
		render_target_for_test(u64(window.slot + 1))
	}
	publish_render_update_for_test(mut app, BackendRenderUpdate{
		window:       window
		sequence:     sequence
		ready_credit: true
		block_reason: .none
		metrics:      metrics
		target:       target
	})
}

fn publish_render_update_for_test(mut app App, update BackendRenderUpdate) {
	app.state_mutex.lock()
	app.apply_backend_render_update_locked(update)
	app.state_mutex.unlock()
}

fn render_metrics_for_test(logical_width f32, logical_height f32, framebuffer_width int, framebuffer_height int, dpi_scale f32, sequence u64) RenderMetricsSnapshot {
	return RenderMetricsSnapshot{
		logical_width:        logical_width
		logical_height:       logical_height
		framebuffer_width:    framebuffer_width
		framebuffer_height:   framebuffer_height
		dpi_scale:            dpi_scale
		metrics_sequence:     sequence
		metrics_available:    true
		conversion_available: true
	}
}

fn render_target_for_test(identity u64) RenderTargetSnapshot {
	return RenderTargetSnapshot{
		target_identity: identity
		color_format:    23
		depth_format:    42
		sample_count:    1
	}
}
