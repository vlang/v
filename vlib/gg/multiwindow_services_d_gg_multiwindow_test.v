// vtest build: gg_multiwindow?
module gg

import x.multiwindow

fn test_multiwindow_service_window_state_preserves_observed_empty_monitor_membership() {
	unknown := window_state_from_core(multiwindow.ServiceWindowState{})
	assert unknown.monitor_ids.len == 0
	assert !unknown.monitor_membership_observed
	observed_empty := window_state_from_core(multiwindow.ServiceWindowState{
		monitor_membership_observed: true
	})
	assert observed_empty.monitor_ids.len == 0
	assert observed_empty.monitor_membership_observed
}

fn test_multiwindow_service_config_converts_identity_and_owner() {
	mut app := new_app(backend: .mock, app_id: 'org.vlang.multiwindow.test')!
	owner := app.create_window(title: 'owner')!
	child_config := WindowConfig{
		title: 'child'
		owner: owner
		modal: true
	}
	core := child_config.to_core()
	assert core.modal
	core_owner := core.owner or { panic('core owner missing') }
	assert core_owner == owner.core
	assert app.config.to_core().app_id == 'org.vlang.multiwindow.test'
	app.stop()!
}

fn test_multiwindow_service_complete_cursor_conversion_matches_core() {
	assert window_cursor_shape_to_core(.text) == multiwindow.CursorShape.text
	assert window_cursor_shape_to_core(.crosshair) == multiwindow.CursorShape.crosshair
	assert window_cursor_shape_to_core(.not_allowed) == multiwindow.CursorShape.not_allowed
	assert window_cursor_shape_to_core(.resize_all) == multiwindow.CursorShape.resize_all
}

struct PortalReadyAfterTeardownProof {
mut:
	callback_hits  int
	stale_hits     int
	destroyed_hits int
}

fn test_mock_portal_ready_destroy_before_drain_acks_stale_lease_once() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'portal ready before teardown')!
	assert app.drain_window_queued_events()!.len == 1
	request := app.request_portal_parent(window)!
	app.destroy_window(window)!
	mut proof := &PortalReadyAfterTeardownProof{}
	app.run(
		event_fn:          fn [mut proof] (event WindowEvent, mut app App) ! {
			if event.kind == .window_destroyed {
				proof.destroyed_hits++
				app.stop()!
			}
		}
		window_service_fn: fn [request, mut proof] (event WindowServiceEvent, mut app App) ! {
			if event.kind != .portal_parent || event.portal_parent.id != request {
				return
			}
			proof.callback_hits++
			assert event.portal_parent.status == .ready
			assert event.portal_parent.identifier.starts_with('mock:')
			app.release_portal_parent(event.portal_parent.lease) or {
				assert err.msg().contains('service request is stale')
				proof.stale_hits++
			}
		}
	)!
	assert proof.callback_hits == 1
	assert proof.stale_hits == 1
	assert proof.destroyed_hits == 1
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
	assert proof.callback_hits == 1
	assert app.core.drain_queued_events()!.len == 0
}

struct ManagedOwnerTree {
	owner      WindowId
	child      WindowId
	grandchild WindowId
	sibling    WindowId
}

struct ManagedOwnerCascadeProof {
mut:
	cleanup_order   []WindowId
	cleanup_reasons []WindowCleanupReason
	destroyed_order []WindowId
	callback_order  []string
	all_live_inside bool
	deferred_inside []WindowId
	fail_cleanup    WindowId
	failure_message string
}

fn managed_owner_cleanup(mut context WindowCleanupContext, mut proof ManagedOwnerCascadeProof) ! {
	proof.cleanup_order << context.window_id()
	proof.cleanup_reasons << context.reason()
	if context.window_id() == proof.fail_cleanup && proof.failure_message != '' {
		return error(proof.failure_message)
	}
}

fn create_managed_owner_tree(mut app App, mut proof ManagedOwnerCascadeProof) !ManagedOwnerTree {
	owner := app.create_window(
		title:      'managed owner'
		cleanup_fn: fn [mut proof] (mut context WindowCleanupContext) ! {
			managed_owner_cleanup(mut context, mut proof)!
		}
	)!
	child := app.create_window(
		title:      'managed child'
		owner:      owner
		modal:      true
		cleanup_fn: fn [mut proof] (mut context WindowCleanupContext) ! {
			managed_owner_cleanup(mut context, mut proof)!
		}
	)!
	grandchild := app.create_window(
		title:      'managed grandchild'
		owner:      child
		cleanup_fn: fn [mut proof] (mut context WindowCleanupContext) ! {
			managed_owner_cleanup(mut context, mut proof)!
		}
	)!
	sibling := app.create_window(
		title:      'managed sibling'
		owner:      owner
		cleanup_fn: fn [mut proof] (mut context WindowCleanupContext) ! {
			managed_owner_cleanup(mut context, mut proof)!
		}
	)!
	return ManagedOwnerTree{
		owner:      owner
		child:      child
		grandchild: grandchild
		sibling:    sibling
	}
}

fn managed_owner_tree_order(tree ManagedOwnerTree) []WindowId {
	return [tree.grandchild, tree.child, tree.sibling, tree.owner]
}

fn assert_managed_owner_events_exactly_once(events []multiwindow.QueuedEvent, order []WindowId, readbacks []multiwindow.ServiceReadbackId) {
	assert events.len == order.len * 2
	for index in 1 .. events.len {
		assert events[index - 1].sequence < events[index].sequence
	}
	for index, window in order {
		readback_event := events[index * 2]
		destroyed_event := events[index * 2 + 1]
		assert readback_event.kind == .readback
		assert readback_event.readback.id == readbacks[index]
		assert readback_event.readback.window == window.core
		assert readback_event.readback.status == .cancelled
		assert destroyed_event.kind == .lifecycle
		assert destroyed_event.lifecycle.kind == .window_destroyed
		assert destroyed_event.lifecycle.window_id == window.core
	}
}

fn with_mock_native_window_borrow_for_managed_test(mut app App, id WindowId, f NativeWindowBorrowFn) ! {
	app_ptr := unsafe { voidptr(&app) }
	callback := fn [app_ptr, f] (borrow multiwindow.NativeWindowBorrow) ! {
		mut facade := unsafe { &App(app_ptr) }
		mut lease := NativeWindowLease{
			app:          facade
			app_instance: borrow.app_instance_for_gg()
			window:       window_id_from_core(borrow.window_for_gg())
			lease_epoch:  borrow.epoch_for_gg()
			backend:      native_backend_from_core(borrow.backend_for_gg())
			primary:      borrow.primary_for_gg()
			secondary:    borrow.secondary_for_gg()
		}
		facade.render_runtime.begin_user_callback()
		mut callback_error := IError(none)
		f(mut lease) or { callback_error = err }
		facade.render_runtime.end_user_callback()
		if callback_error !is none {
			return callback_error
		}
	}
	mut borrow_error := IError(none)
	app.core.with_mock_native_window_borrow_for_gg_test(id.core, callback) or { borrow_error = err }
	app.flush_deferred_transitions()!
	if borrow_error !is none {
		return borrow_error
	}
}

fn test_managed_owner_destroy_is_child_first_and_exactly_once() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	mut readbacks := []multiwindow.ServiceReadbackId{cap: order.len}
	for index, window in order {
		readbacks << seed_managed_window_capture_for_test(mut app, window, u64(20 + index))!
	}

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .requested}
	assert app.window_ids()!.len == 0
	assert app.pending_window_captures.len == 0
	assert app.render_runtime.windows.all(it.status == .destroyed)
	events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(events, order, readbacks)

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
	assert app.core.drain_queued_events()!.len == 0
}

fn test_managed_owner_destroy_purges_descendant_image_readbacks_exactly_once() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	mut readbacks := []multiwindow.ServiceReadbackId{cap: order.len}
	for index, window in order {
		readbacks << seed_managed_image_readback_for_test(mut app, window, u64(100 + index))!
	}
	assert app.pending_window_captures.len == 0
	assert app.pending_image_readbacks.len == order.len

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert app.pending_window_captures.len == 0
	assert app.pending_image_readbacks.len == 0
	events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(events, order, readbacks)

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
}

fn test_managed_owner_destroy_continues_after_terminal_child_cleanup_error() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	proof.fail_cleanup = tree.grandchild
	proof.failure_message = 'managed owner grandchild cleanup failed'
	mut readbacks := []multiwindow.ServiceReadbackId{cap: order.len}
	for index, window in order {
		readbacks << seed_managed_window_capture_for_test(mut app, window, u64(60 + index))!
	}

	mut destroy_error := ''
	app.destroy_window(tree.owner) or { destroy_error = err.msg() }
	assert destroy_error.contains(proof.failure_message)
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .requested}
	assert app.window_ids()!.len == 0
	assert app.pending_window_captures.len == 0
	assert app.render_runtime.windows.all(it.status == .destroyed)
	events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(events, order, readbacks)

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
}

fn test_managed_owner_destroy_defers_complete_order_until_callback_returns() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	app.core.enqueue_mock_close_requested_for_test(tree.owner.core)!
	app.run(
		event_fn: fn [tree, order, mut proof] (event WindowEvent, mut app App) ! {
			match event.kind {
				.window_close_requested {
					proof.callback_order << 'before-destroy'
					app.destroy_window(tree.sibling)!
					assert app.render_runtime.deferred_windows == [tree.sibling]
					app.destroy_window(tree.owner)!
					app.destroy_window(tree.owner)!
					proof.all_live_inside = order.all(app.window_exists(it))
					proof.deferred_inside = app.render_runtime.deferred_windows.clone()
					assert proof.cleanup_order.len == 0
					proof.callback_order << 'after-destroy'
				}
				.window_destroyed {
					proof.destroyed_order << event.window
					if proof.destroyed_order.len == order.len {
						app.stop()!
					}
				}
				else {}
			}
		}
	)!
	assert proof.callback_order == ['before-destroy', 'after-destroy']
	assert proof.all_live_inside
	assert proof.deferred_inside == order
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .requested}
	assert proof.destroyed_order == order
	assert app.core.status() == .stopped
}

fn test_managed_owner_destroy_during_descendant_native_borrow_waits_for_release() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	app_ptr := unsafe { voidptr(app) }
	callback := fn [app_ptr, tree, order, mut proof] (mut lease NativeWindowLease) ! {
		mut owner := unsafe { &App(app_ptr) }
		assert lease.window == tree.grandchild
		assert lease.backend == .mock
		owner.destroy_window(tree.owner)!
		proof.all_live_inside = order.all(owner.window_exists(it))
		proof.deferred_inside = owner.render_runtime.deferred_windows.clone()
		assert proof.cleanup_order.len == 0
	}

	with_mock_native_window_borrow_for_managed_test(mut app, tree.grandchild, callback)!
	assert proof.all_live_inside
	assert proof.deferred_inside == order
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .requested}
	assert app.window_ids()!.len == 0
	events := app.core.drain_queued_events()!
	assert events.len == order.len
	assert events.map(it.kind) == []multiwindow.QueuedEventKind{len: order.len, init: .lifecycle}
	assert events.map(window_id_from_core(it.lifecycle.window_id)) == order

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
}

fn test_deferred_owner_destroy_continues_after_terminal_child_cleanup_error() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	proof.fail_cleanup = tree.grandchild
	proof.failure_message = 'deferred grandchild cleanup failed'
	mut readbacks := []multiwindow.ServiceReadbackId{cap: order.len}
	for index, window in order {
		readbacks << seed_managed_window_capture_for_test(mut app, window, u64(80 + index))!
	}
	app.core.enqueue_mock_close_requested_for_test(tree.owner.core)!
	mut run_error := ''
	app.run(
		event_fn: fn [tree, order, mut proof] (event WindowEvent, mut app App) ! {
			if event.kind == .window_close_requested {
				app.destroy_window(tree.owner)!
				proof.all_live_inside = order.all(app.window_exists(it))
				assert proof.cleanup_order.len == 0
			}
		}
	) or { run_error = err.msg() }
	assert run_error.contains(proof.failure_message)
	assert proof.all_live_inside
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .requested}
	assert app.window_ids()!.len == 0
	assert app.pending_window_captures.len == 0
	assert app.render_runtime.deferred_windows.len == 0
	events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(events, order, readbacks)
	mut stop_error := ''
	app.stop() or { stop_error = err.msg() }
	assert stop_error.contains(proof.failure_message)
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
}

fn test_deferred_owner_destroy_restores_reversible_failure_suffix_for_retry() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	mut readbacks := []multiwindow.ServiceReadbackId{cap: order.len}
	for index, window in order {
		readbacks << seed_managed_window_capture_for_test(mut app, window, u64(120 + index))!
	}
	failure := 'managed owner reversible teardown prepare failure'
	app.render_runtime.set_internal_fault(.teardown_prepare, 1, failure)!
	app.render_runtime.begin_user_callback()
	app.destroy_window(tree.owner)!
	app.render_runtime.end_user_callback()
	assert app.render_runtime.deferred_windows == order

	mut first_error := ''
	app.flush_deferred_transitions() or { first_error = err.msg() }
	assert first_error.contains(failure)
	assert !app.window_exists(tree.grandchild)
	assert order[1..].all(app.window_exists(it))
	assert proof.cleanup_order == [tree.grandchild]
	assert proof.cleanup_reasons == [.requested]
	assert app.pending_window_captures.len == order.len - 1
	assert app.render_runtime.deferred_windows == order[1..]
	prefix_events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(prefix_events, order[..1], readbacks[..1])

	app.flush_deferred_transitions()!
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .requested}
	assert app.window_ids()!.len == 0
	assert app.pending_window_captures.len == 0
	assert app.render_runtime.deferred_windows.len == 0
	suffix_events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(suffix_events, order[1..], readbacks[1..])
	assert app.core.drain_queued_events()!.len == 0

	app.destroy_window(tree.owner)!
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
}

fn test_managed_owner_destroy_admission_rejects_atomically() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	_ = app.drain_window_queued_events()!
	app.render_runtime.mutex.lock()
	child_index := app.render_runtime.window_index_locked(tree.child)!
	app.render_runtime.windows[child_index].status = .destroyed
	app.render_runtime.mutex.unlock()
	app.render_runtime.begin_user_callback()
	mut rejected := false
	app.destroy_window(tree.owner) or {
		assert err.msg() == err_multiwindow_window_not_found
		rejected = true
	}
	app.render_runtime.end_user_callback()
	assert rejected
	app.render_runtime.mutex.lock()
	owner_index := app.render_runtime.window_index_locked(tree.owner)!
	assert app.render_runtime.windows[owner_index].status == .registered
	assert !app.render_runtime.windows[owner_index].cleanup_reason_set
	assert app.render_runtime.deferred_windows.len == 0
	app.render_runtime.windows[child_index].status = .registered
	app.render_runtime.mutex.unlock()
	assert app.window_ids()!.len == 4
	app.stop()!
}

fn test_managed_owner_stop_is_child_first_and_exactly_once() {
	mut app := new_app(backend: .mock)!
	mut proof := &ManagedOwnerCascadeProof{}
	tree := create_managed_owner_tree(mut app, mut proof)!
	order := managed_owner_tree_order(tree)
	assert app.drain_window_queued_events()!.len == 4
	mut readbacks := []multiwindow.ServiceReadbackId{cap: order.len}
	for index, window in order {
		readbacks << seed_managed_window_capture_for_test(mut app, window, u64(40 + index))!
	}

	app.stop()!
	assert proof.cleanup_order == order
	assert proof.cleanup_reasons == []WindowCleanupReason{len: order.len, init: .app_stop}
	assert app.window_ids()!.len == 0
	assert app.pending_window_captures.len == 0
	assert app.render_runtime.windows.all(it.status == .destroyed)
	events := app.core.drain_queued_events()!
	assert_managed_owner_events_exactly_once(events, order, readbacks)

	app.stop()!
	assert proof.cleanup_order == order
	assert app.core.drain_queued_events()!.len == 0
}

fn test_readback_rect_fits_exact_edge_and_rejects_overflow_without_admission() {
	assert readback_rect_fits(WindowPixelRect{
		x:      639
		y:      479
		width:  1
		height: 1
	}, 640, 480)
	assert !readback_rect_fits(WindowPixelRect{
		x:      0x7fffffff
		y:      0
		width:  1
		height: 1
	}, 640, 480)
	assert !readback_rect_fits(WindowPixelRect{
		x:      1
		y:      1
		width:  0x7fffffff
		height: 1
	}, 640, 480)

	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'readback-overflow', width: 640, height: 480)!
	_ = app.drain_window_queued_events()!
	assert app.pending_window_captures.len == 0
	app.request_window_capture(window, WindowReadbackConfig{
		rect: WindowPixelRect{
			x:      0x7fffffff
			y:      0
			width:  1
			height: 1
		}
	}) or {
		assert err.msg() == err_multiwindow_render_readback_unsupported
		assert app.pending_window_captures.len == 0
		assert app.drain_window_queued_events()!.len == 0
		app.stop()!
		return
	}
	assert false, 'overflowing readback rectangle entered the pending/event pipeline'
}

fn capture_producer_snapshot_for_test(reason multiwindow.RenderBlockReason, metrics_available bool, width int, height int) multiwindow.RenderWindowSnapshot {
	return multiwindow.RenderWindowSnapshot{
		metrics:      multiwindow.RenderMetricsSnapshot{
			framebuffer_width:  width
			framebuffer_height: height
			metrics_available:  metrics_available
		}
		target:       multiwindow.RenderTargetSnapshot{
			sample_count: 1
		}
		block_reason: reason
	}
}

fn test_managed_window_capture_requires_a_real_viable_frame_producer() {
	ready := capture_producer_snapshot_for_test(.none, true, 64, 48)
	frame_fn := MultiWindowCaptureProducer{
		frame_fn_configured: true
	}
	assert managed_window_capture_has_producer(frame_fn, ready)
	assert !managed_window_capture_has_producer(MultiWindowCaptureProducer{}, ready)
	assert !managed_window_capture_has_producer(frame_fn, capture_producer_snapshot_for_test(.none,
		false, 64, 48))
	assert !managed_window_capture_has_producer(frame_fn, capture_producer_snapshot_for_test(.zero_sized,
		true, 0, 0))

	for reason in [multiwindow.RenderBlockReason.no_workload, .not_configured, .hidden, .minimized,
		.occluded, .unmapped, .not_viewable, .zero_sized, .backend_unavailable, .renderer_failed] {
		blocked := capture_producer_snapshot_for_test(reason, true, 64, 48)
		assert !managed_window_capture_has_producer(frame_fn, blocked)
		assert managed_window_capture_has_producer(MultiWindowCaptureProducer{
			frame_active: true
		}, blocked)
	}
	for reason in [multiwindow.RenderBlockReason.frame_callback_pending, .resize_pending,
		.drawable_unavailable] {
		assert managed_window_capture_has_producer(frame_fn, capture_producer_snapshot_for_test(reason,
			true, 64, 48))
	}
}

fn test_managed_window_capture_producer_distinguishes_init_and_resource_only_windows() {
	mut app := new_app(backend: .mock)!
	init_only := app.create_window(title: 'capture init only')!
	resource_only := app.create_window(title: 'capture resource only')!
	frame_window := app.create_window(title: 'capture frame producer')!
	app.render_runtime.mutex.lock()
	init_index := app.render_runtime.window_index_locked(init_only)!
	frame_index := app.render_runtime.window_index_locked(frame_window)!
	app.render_runtime.windows[init_index].init_fn = fn (mut context WindowInitContext) ! {
		_ = context
	}
	app.render_runtime.windows[frame_index].frame_fn = fn (mut context WindowContext) ! {
		_ = context
	}
	app.render_runtime.mutex.unlock()
	assert !app.render_runtime.window_capture_producer(init_only)!.frame_fn_configured
	assert !app.render_runtime.window_capture_producer(resource_only)!.frame_fn_configured
	assert app.render_runtime.window_capture_producer(frame_window)!.frame_fn_configured

	app.render_runtime.mutex.lock()
	active_index := app.render_runtime.window_index_locked(resource_only)!
	app.render_runtime.windows[active_index].active_lease_epoch = 1
	app.render_runtime.windows[active_index].active_phase = .frame
	app.render_runtime.mutex.unlock()
	assert app.render_runtime.window_capture_producer(resource_only)!.frame_active
	app.render_runtime.mutex.lock()
	app.render_runtime.windows[active_index].active_lease_epoch = 0
	app.render_runtime.windows[active_index].active_phase = .invalid
	app.render_runtime.mutex.unlock()
	app.stop()!
}

fn configure_managed_window_capture_producer_for_test(mut app App, window WindowId) ! {
	app.render_runtime.mutex.lock()
	index := app.render_runtime.window_index_locked(window) or {
		app.render_runtime.mutex.unlock()
		return err
	}
	app.render_runtime.windows[index].frame_fn = fn (mut context WindowContext) ! {
		_ = context
	}
	app.render_runtime.mutex.unlock()
}

fn seed_unbound_managed_window_capture_for_test(mut app App, window WindowId) !multiwindow.ServiceReadbackId {
	readback := app.core.service_begin_window_readback(window.core)!
	app.pending_window_captures << MultiWindowPendingWindowCapture{
		id:                     readback
		window:                 window
		rect:                   WindowPixelRect{
			width:  2
			height: 2
		}
		target_submitted_frame: 1
	}
	return readback
}

fn set_managed_window_capture_snapshot_for_test(mut app App, window WindowId, reason multiwindow.RenderBlockReason, submitted_frame u64) ! {
	app.core.set_render_window_snapshot_for_gg_test(window.core, reason, true, 2, 2, 1,
		submitted_frame)!
}

fn test_unbound_managed_window_capture_permanent_blockers_terminalize_exactly_once() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture permanent blockers')!
	_ = app.drain_window_queued_events()!
	configure_managed_window_capture_producer_for_test(mut app, window)!

	for reason in [multiwindow.RenderBlockReason.no_workload, .not_configured, .hidden, .minimized,
		.occluded, .unmapped, .not_viewable, .zero_sized, .backend_unavailable, .renderer_failed] {
		set_managed_window_capture_snapshot_for_test(mut app, window, reason, 0)!
		readback := seed_unbound_managed_window_capture_for_test(mut app, window)!
		assert app.poll_events()! == 0
		results := app.core.drain_readback_events()!
		assert results.len == 1
		assert results[0].id == readback
		assert results[0].status == .failed
		assert results[0].pixels_rgba8.len == 0
		assert results[0].error == err_multiwindow_render_capture_cancelled
		assert app.pending_window_captures.len == 0
		assert app.poll_events()! == 0
		assert app.core.drain_readback_events()!.len == 0
	}
	app.stop()!
}

fn test_unbound_managed_window_capture_preserves_transient_render_blockers() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture transient blockers')!
	_ = app.drain_window_queued_events()!
	configure_managed_window_capture_producer_for_test(mut app, window)!

	for reason in [multiwindow.RenderBlockReason.frame_callback_pending, .resize_pending,
		.drawable_unavailable] {
		set_managed_window_capture_snapshot_for_test(mut app, window, reason, 0)!
		readback := seed_unbound_managed_window_capture_for_test(mut app, window)!
		assert app.poll_events()! == 0
		assert app.pending_window_captures.len == 1
		assert app.pending_window_captures[0].id == readback
		assert app.core.drain_readback_events()!.len == 0

		set_managed_window_capture_snapshot_for_test(mut app, window, .hidden, 0)!
		assert app.poll_events()! == 0
		results := app.core.drain_readback_events()!
		assert results.len == 1
		assert results[0].id == readback
		assert results[0].status == .failed
		assert app.pending_window_captures.len == 0
	}
	app.stop()!
}

fn test_unbound_managed_window_capture_hide_retries_terminal_admission_and_does_not_poison_next_capture() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture hide reconciliation')!
	_ = app.drain_window_queued_events()!
	configure_managed_window_capture_producer_for_test(mut app, window)!
	set_managed_window_capture_snapshot_for_test(mut app, window, .none, 0)!
	readback := seed_unbound_managed_window_capture_for_test(mut app, window)!

	app.hide_window(window)!
	_ = app.drain_window_queued_events()!
	set_managed_window_capture_snapshot_for_test(mut app, window, .hidden, 0)!
	saved_token := app.core.swap_event_delivery_token_for_gg_test(0)
	mut failed := false
	app.poll_events() or {
		assert err.msg() == 'multiwindow: event delivery sequence exhausted'
		failed = true
	}
	assert failed
	assert app.pending_window_captures.len == 1
	assert app.pending_window_captures[0].id == readback
	assert app.core.drain_readback_events()!.len == 0
	_ = app.core.swap_event_delivery_token_for_gg_test(saved_token)

	assert app.poll_events()! == 0
	failed_results := app.core.drain_readback_events()!
	assert failed_results.len == 1
	assert failed_results[0].id == readback
	assert failed_results[0].status == .failed
	assert app.pending_window_captures.len == 0

	app.show_window(window)!
	_ = app.drain_window_queued_events()!
	set_managed_window_capture_snapshot_for_test(mut app, window, .none, 1)!
	retry := seed_managed_window_capture_for_test(mut app, window, 9)!
	app.finish_managed_window_captures(multiwindow.RenderBatchOutcome{
		batch_epoch: 9
		committed:   true
	})!
	ready := app.core.drain_readback_events()!
	assert ready.len == 1
	assert ready[0].id == retry
	assert ready[0].status == .ready
	assert ready[0].pixels_rgba8.len == 16
	assert app.pending_window_captures.len == 0
	app.stop()!
}

fn test_managed_window_capture_redraw_failure_precedes_pending_admission() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture redraw failure')!
	_ = app.drain_window_queued_events()!
	before := app.core.render_window_snapshot(window.core)!
	failure := 'injected capture redraw admission failure'
	app.render_runtime.set_internal_fault(.capture_request_redraw, 0, failure)!
	mut rejected := false
	app.request_window_capture_redraw(window) or {
		assert err.msg() == failure
		rejected = true
	}
	assert rejected
	after := app.core.render_window_snapshot(window.core)!
	assert after.dirty_epoch == before.dirty_epoch
	assert app.pending_window_captures.len == 0
	assert app.pending_image_readbacks.len == 0
	assert app.core.drain_readback_events()!.len == 0
	app.stop()!
}

struct Package2RunSeen {
mut:
	order                []string
	stop_was_deferred    bool
	service_callback_hit int
}

fn test_multiwindow_run_config_dispatches_interleaved_service_and_readback_in_canonical_order() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'package2 run order')!
	app.show_window(window)!
	_ = app.request_window_capture(window, WindowReadbackConfig{})!
	mut seen := &Package2RunSeen{}
	app.run(
		event_fn:          fn [mut seen] (event WindowEvent, mut app App) ! {
			_ = app
			assert event.kind == .window_created
			seen.order << 'lifecycle'
		}
		window_service_fn: fn [mut seen] (event WindowServiceEvent, mut app App) ! {
			_ = app
			assert event.kind == .state
			seen.service_callback_hit++
			seen.order << 'service'
		}
		readback_fn:       fn [mut seen] (result WindowReadbackResult, mut app App) ! {
			assert result.status == .ready
			seen.order << 'readback'
			app.stop()!
			seen.stop_was_deferred = app.core.status() == multiwindow.AppStatus.running
		}
	)!
	assert seen.order == ['lifecycle', 'service', 'readback']
	assert seen.service_callback_hit == 1
	assert seen.stop_was_deferred
	assert app.core.status() == multiwindow.AppStatus.stopped
}

fn test_multiwindow_run_config_callback_error_replays_failed_event_and_exact_suffix() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'package2 replay')!
	assert app.drain_events()!.len == 1
	_ = app.request_clipboard_text(window)!
	_ = app.request_window_capture(window, WindowReadbackConfig{})!
	app.show_window(window)!
	mut seen := &Package2RunSeen{}
	mut run_error := ''
	app.run(
		window_service_fn: fn [mut seen] (event WindowServiceEvent, mut app App) ! {
			_ = event
			_ = app
			seen.service_callback_hit++
			return error('package2 injected callback failure')
		}
		readback_fn:       fn (result WindowReadbackResult, mut app App) ! {
			_ = result
			_ = app
		}
	) or { run_error = err.msg() }
	assert run_error.contains('package2 injected callback failure')
	assert seen.service_callback_hit == 1
	replayed := app.drain_window_queued_events()!
	assert replayed.len == 4
	assert replayed.map(it.kind) == [.service, .readback, .service, .lifecycle]
	assert replayed[0].service.kind == .clipboard
	assert replayed[1].readback.status == .ready
	assert replayed[2].service.kind == .state
	assert replayed[3].lifecycle.kind == .window_destroyed
}

fn seed_managed_window_capture_for_test(mut app App, window WindowId, batch_epoch u64) !multiwindow.ServiceReadbackId {
	readback := app.core.service_begin_window_readback(window.core)!
	app.pending_window_captures << MultiWindowPendingWindowCapture{
		id:                     readback
		window:                 window
		rect:                   WindowPixelRect{
			width:  2
			height: 2
		}
		target_submitted_frame: 1
		attempt_batch_epoch:    batch_epoch
		staged_batch_epoch:     batch_epoch
		staged_pixels:          []u8{len: 16, init: 0xff}
	}
	return readback
}

fn seed_managed_image_readback_for_test(mut app App, window WindowId, batch_epoch u64) !multiwindow.ServiceReadbackId {
	readback := app.core.service_begin_window_readback(window.core)!
	app.pending_image_readbacks << MultiWindowPendingImageReadback{
		id:                     readback
		window:                 window
		batch_epoch:            batch_epoch
		target_submitted_frame: 1
		width:                  2
		height:                 2
		stride:                 8
		pixels:                 []u8{len: 16, init: 0xff}
	}
	return readback
}

fn test_managed_window_capture_submit_failure_is_terminal_exactly_once() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture submit failure')!
	_ = app.drain_events()!
	readback := seed_managed_window_capture_for_test(mut app, window, 7)!

	app.finish_managed_window_captures(multiwindow.RenderBatchOutcome{
		batch_epoch: 7
		error:       'injected submit failure'
	})!
	results := app.core.drain_readback_events()!
	assert results.len == 1
	assert results[0].id == readback
	assert results[0].status == .failed
	assert results[0].submitted_frame == 0
	assert results[0].pixels_rgba8.len == 0
	assert results[0].error == 'injected submit failure'
	assert app.pending_window_captures.len == 0

	app.finish_managed_window_captures(multiwindow.RenderBatchOutcome{
		batch_epoch: 7
		error:       'injected submit failure'
	})!
	assert app.core.drain_readback_events()!.len == 0
	app.stop()!
}

fn test_linux_gl_image_readback_submit_failure_is_terminal_exactly_once() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'image readback submit failure')!
	_ = app.drain_events()!
	readback := seed_managed_image_readback_for_test(mut app, window, 17)!

	app.finish_linux_gl_image_readbacks(multiwindow.RenderBatchOutcome{
		batch_epoch: 17
		error:       'injected image submit failure'
	})!
	results := app.core.drain_readback_events()!
	assert results.len == 1
	assert results[0].id == readback
	assert results[0].status == .failed
	assert results[0].submitted_frame == 0
	assert results[0].pixels_rgba8.len == 0
	assert results[0].error == 'injected image submit failure'
	assert app.pending_image_readbacks.len == 0

	app.finish_linux_gl_image_readbacks(multiwindow.RenderBatchOutcome{
		batch_epoch: 17
		error:       'injected image submit failure'
	})!
	assert app.core.drain_readback_events()!.len == 0
	app.stop()!
}

fn test_managed_window_capture_destroy_is_cancelled_exactly_once() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture destroy cancellation')!
	_ = app.drain_events()!
	readback := seed_managed_window_capture_for_test(mut app, window, 8)!

	app.destroy_window(window)!
	events := app.core.drain_queued_events()!
	readbacks := events.filter(it.kind == .readback)
	assert readbacks.len == 1
	assert readbacks[0].readback.id == readback
	assert readbacks[0].readback.status == .cancelled
	assert readbacks[0].readback.submitted_frame == 0
	assert readbacks[0].readback.pixels_rgba8.len == 0
	assert app.pending_window_captures.len == 0

	app.destroy_window(window)!
	assert app.core.drain_queued_events()!.len == 0
	app.stop()!
}

fn test_managed_window_capture_stop_is_cancelled_exactly_once() {
	mut app := new_app(backend: .mock)!
	window := app.create_window(title: 'capture stop cancellation')!
	_ = app.drain_events()!
	readback := seed_managed_window_capture_for_test(mut app, window, 9)!

	app.stop()!
	events := app.core.drain_queued_events()!
	readbacks := events.filter(it.kind == .readback)
	assert readbacks.len == 1
	assert readbacks[0].readback.id == readback
	assert readbacks[0].readback.status == .cancelled
	assert readbacks[0].readback.submitted_frame == 0
	assert readbacks[0].readback.pixels_rgba8.len == 0
	assert app.pending_window_captures.len == 0

	app.stop()!
	assert app.core.drain_queued_events()!.len == 0
}
