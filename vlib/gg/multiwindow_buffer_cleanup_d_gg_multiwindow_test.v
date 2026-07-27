// vtest build: gg_multiwindow?
module gg

import gg.testdata.multiwindow_sokol_trace
import gg.testdata.multiwindow_probe_gate
import os
import sokol.gfx
import time
import x.multiwindow

#flag -DSOKOL_TRACE_HOOKS

struct MultiWindowBufferSlotProof {
	status              MultiWindowResourceStatus
	usage               gfx.Usage
	capacity            usize
	last_mutation_batch u64
	mutation_mode       MultiWindowBufferMutationMode
	native_id           u32
	native_state        gfx.ResourceState
}

struct MultiWindowBufferRegistryProof {
mut:
	slots                int
	alive                int
	retiring             int
	invalid              int
	exhausted            int
	deferred             int
	dependency_edges     int
	dependent_edges      int
	slot_native_ids      int
	deferred_native_ids  int
	sgl_materializations int
}

@[heap]
struct MultiWindowBufferDescriptorProof {
mut:
	trace_installed           bool
	invalid_rejections        int
	init_trace                []string
	cleanup_trace             []string
	cleanup_calls             int
	success_slots             []MultiWindowBufferSlotProof
	saved_buffers             []gfx.Buffer
	cleanup_states            []gfx.ResourceState
	validation_fault_error    string
	immutable_update_error    string
	immutable_append_error    string
	immutable_before_mutation MultiWindowBufferSlotProof
	immutable_after_mutation  MultiWindowBufferSlotProof
}

@[heap]
struct MultiWindowBufferAppendProof {
mut:
	trace_installed            bool
	frame_calls                int
	cleanup_calls              int
	main                       WindowBufferId
	update_then_append         WindowBufferId
	append_then_update         WindowBufferId
	saved_buffers              []gfx.Buffer
	first_offsets              []int
	reset_offset               int
	overflow_error             string
	update_then_append_error   string
	append_then_update_error   string
	overflow_trace_before      []string
	overflow_trace_after       []string
	update_append_trace_before []string
	update_append_trace_after  []string
	append_update_trace_before []string
	append_update_trace_after  []string
	reset_trace_before         []string
	reset_trace_after          []string
	overflow_slot_before       MultiWindowBufferSlotProof
	overflow_slot_after        MultiWindowBufferSlotProof
	update_append_slot_before  MultiWindowBufferSlotProof
	update_append_slot_after   MultiWindowBufferSlotProof
	append_update_slot_before  MultiWindowBufferSlotProof
	append_update_slot_after   MultiWindowBufferSlotProof
	reset_slot                 MultiWindowBufferSlotProof
	cleanup_trace              []string
	cleanup_states             []gfx.ResourceState
}

struct MultiWindowCleanupCauseObservation {
	window             WindowId
	reason             WindowCleanupReason
	graphics_available bool
}

@[heap]
struct MultiWindowCleanupCauseProof {
mut:
	observations                            []MultiWindowCleanupCauseObservation
	frame_calls                             int
	completed_passes                        int
	event_calls                             int
	stop_requests                           int
	victim_frame_calls                      int
	victim_completed_passes                 int
	victim_recoverable_error_observed       bool
	victim_recoverable_error_message        string
	victim_recoverable_error_consumed       bool
	survivor_frame_calls                    int
	survivor_callbacks_before_friend_finish int
	survivor_completed_passes               int
	survivor_exists_after_pass              bool
	survivor_renderer_usable_after_pass     bool
	victim                                  WindowId
	survivor                                WindowId
	native_ticket                           multiwindow.GgEglNativeWindowProofTicket
	native_ticket_armed                     bool
	native_friend_finished                  bool
}

struct MultiWindowCleanupRuntimeProof {
	window_slots            int
	next_lease_epoch        u64
	next_pass_epoch         u64
	active_batch_epoch      u64
	last_completed_batch    u64
	batch_active            bool
	callback_depth          int
	deferred_windows        int
	deferred_stop           bool
	stopping                bool
	stopped                 bool
	app_lease_epoch         u64
	app_phase               MultiWindowRenderPhase
	app_resource_active     bool
	app_init_started        bool
	app_init_completed      bool
	app_init_terminal       string
	app_cleanup_started     bool
	app_cleanup_finished    bool
	app_cleanup_lease_epoch u64
	app_resources_retired   bool
mut:
	window_invalid           int
	window_registered        int
	window_initialized       int
	window_closing           int
	window_destroyed         int
	cleanup_started          int
	cleanup_finished         int
	cleanup_reason_set       int
	native_closed_reasons    int
	app_stop_reasons         int
	targets_available        int
	target_identities        int
	active_window_leases     int
	active_resource_sections int
	active_passes            int
	active_sgl               int
}

struct MultiWindowCleanupTerminalProof {
	core_status               multiwindow.AppStatus
	core_window_count         int
	gfx_started               bool
	sgl_initialized           bool
	render_owner_claimed      bool
	renderer_usable           bool
	renderer_device_available bool
	renderer_terminal_failure bool
	terminal_error            string
	sgl_contexts              int
	sgl_context_targets       int
	window_sgl_targets        int
	deferred_sgl_targets      int
	active_render_snapshots   int
	active_drawn_windows      int
	active_batch_epoch        u64
	app_frame_active          bool
	runtime                   MultiWindowCleanupRuntimeProof
	resources                 MultiWindowBufferRegistryProof
}

fn test_multiwindow_buffer_descriptor_boundaries_drive_real_make_operations() {
	if !multiwindow_buffer_contract_runtime_requested() {
		return
	}
	mut app := multiwindow_buffer_contract_new_app()!
	mut proof := &MultiWindowBufferDescriptorProof{}
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut proof] (mut resources AppResourceContext) ! {
			multiwindow_run_buffer_descriptor_init(mut resources, mut proof)!
		}
		app_resource_cleanup_fn: fn [mut proof] (mut resources AppResourceContext) ! {
			_ = resources
			proof.cleanup_calls++
			proof.cleanup_trace = multiwindow_sokol_trace.snapshot()
			for buffer in proof.saved_buffers {
				proof.cleanup_states << gfx.query_buffer_state(buffer)
			}
			multiwindow_sokol_trace.uninstall()
			proof.trace_installed = false
		}
	) or { run_error = err.msg() }
	mut emergency_trace := []string{}
	if proof.trace_installed {
		emergency_trace = multiwindow_sokol_trace.snapshot()
		multiwindow_sokol_trace.uninstall()
		proof.trace_installed = false
	}
	mut forced_stop_error := ''
	if app.core.status() != .stopped {
		app.stop() or { forced_stop_error = err.msg() }
	}
	mut replay_stop_error := ''
	app.stop() or { replay_stop_error = err.msg() }
	terminal_registry := multiwindow_buffer_registry_proof(app.render_runtime)

	assert run_error == ''
	assert emergency_trace.len == 0
	assert forced_stop_error == ''
	assert replay_stop_error == ''
	assert proof.invalid_rejections == 5
	assert proof.validation_fault_error == 'fault:resource_make_buffer:descriptor-boundary'
	assert proof.success_slots.len == 4
	assert proof.success_slots[0].usage == .dynamic
	assert proof.success_slots[0].capacity == 16
	assert proof.success_slots[1].usage == .stream
	assert proof.success_slots[1].capacity == 20
	assert proof.success_slots[2].usage == .immutable
	assert proof.success_slots[2].capacity == 4
	assert proof.success_slots[3].usage == .immutable
	assert proof.success_slots[3].capacity == 4
	for slot in proof.success_slots {
		assert slot.status == .alive
		assert slot.native_id != 0
		assert slot.native_state == .valid
	}
	assert proof.immutable_update_error == err_multiwindow_render_mutation_mode
	assert proof.immutable_append_error == err_multiwindow_render_mutation_mode
	assert multiwindow_buffer_slot_proofs_equal(proof.immutable_before_mutation,
		proof.immutable_after_mutation)
	assert proof.immutable_before_mutation.last_mutation_batch == 0
	assert proof.immutable_before_mutation.mutation_mode == .none
	assert proof.init_trace == ['make_buffer', 'make_buffer', 'make_buffer', 'make_buffer']
	assert proof.cleanup_calls == 1
	assert proof.cleanup_states == [.invalid, .invalid, .invalid, .invalid]
	assert proof.cleanup_trace == ['make_buffer', 'make_buffer', 'make_buffer', 'make_buffer',
		'begin_swapchain_pass', 'end_pass', 'commit', 'destroy_buffer', 'destroy_buffer',
		'destroy_buffer', 'destroy_buffer']
	assert terminal_registry.slots == 4
	assert terminal_registry.alive == 0
	assert terminal_registry.retiring == 0
	assert terminal_registry.invalid == 4
	assert terminal_registry.deferred == 0
	assert terminal_registry.slot_native_ids == 0
	assert terminal_registry.deferred_native_ids == 0
	assert terminal_registry.dependency_edges == 0
	assert terminal_registry.dependent_edges == 0
	assert app.core.status() == .stopped
	assert !app.gfx_started
}

fn multiwindow_run_buffer_descriptor_init(mut resources AppResourceContext, mut proof MultiWindowBufferDescriptorProof) ! {
	multiwindow_sokol_trace.install()!
	proof.trace_installed = true
	mut runtime := resources.app.render_runtime
	fault_message := 'fault:resource_make_buffer:descriptor-boundary'
	runtime.set_internal_fault(.resource_make_buffer, 0, fault_message)!
	bytes := [u8(1), 2, 3, 4]
	data := gfx.Range{
		ptr:  bytes.data
		size: usize(bytes.len)
	}

	for desc in [
		gfx.BufferDesc{
			size:  4
			usage: .immutable
		},
		gfx.BufferDesc{
			size:  8
			usage: .immutable
			data:  data
		},
		gfx.BufferDesc{
			size:  4
			usage: .dynamic
			data:  data
		},
		gfx.BufferDesc{
			usage: .stream
		},
		gfx.BufferDesc{
			size:  usize(0x80000000)
			usage: .dynamic
		},
	] {
		multiwindow_expect_invalid_buffer_before_fault(mut resources, &desc, fault_message)!
		proof.invalid_rejections++
	}
	fault_trace_before := multiwindow_sokol_trace.snapshot()
	fault_registry_before := multiwindow_buffer_registry_proof(runtime)
	mut faulted := false
	unexpected := resources.make_buffer(&gfx.BufferDesc{
		size:  16
		usage: .dynamic
	}) or {
		proof.validation_fault_error = err.msg()
		faulted = true
		WindowBufferId{}
	}
	if !faulted {
		resources.retire_buffer(unexpected) or {
			return error('valid buffer descriptor bypassed its armed make fault; cleanup failed: ${err.msg()}')
		}
		return error('valid buffer descriptor bypassed its armed make fault')
	}
	if proof.validation_fault_error != fault_message {
		return error('valid buffer descriptor returned `${proof.validation_fault_error}` instead of its armed make fault')
	}
	if multiwindow_sokol_trace.snapshot() != fault_trace_before {
		return error('make-buffer fault reached Sokol creation')
	}
	if !multiwindow_buffer_registry_proofs_equal(fault_registry_before,
		multiwindow_buffer_registry_proof(runtime)) {
		return error('make-buffer fault left registry residue')
	}
	consumed_plan := multiwindow_buffer_fault_plan(runtime)
	if consumed_plan.stage != .none || consumed_plan.message != '' {
		return error('valid buffer operation did not consume its one-shot make fault')
	}
	runtime.clear_internal_fault()

	dynamic := resources.make_buffer(&gfx.BufferDesc{
		size:  16
		usage: .dynamic
	})!
	stream := resources.make_buffer(&gfx.BufferDesc{
		size:  20
		usage: .stream
	})!
	immutable_inferred := resources.make_buffer(&gfx.BufferDesc{
		usage: .immutable
		data:  data
	})!
	immutable_exact := resources.make_buffer(&gfx.BufferDesc{
		size:  4
		usage: .immutable
		data:  data
	})!
	for id in [dynamic, stream, immutable_inferred, immutable_exact] {
		slot := multiwindow_buffer_slot_proof(runtime, id)!
		proof.success_slots << slot
		proof.saved_buffers << multiwindow_buffer_native_handle(runtime, id)!
	}
	proof.init_trace = multiwindow_sokol_trace.snapshot()
	proof.immutable_before_mutation = multiwindow_buffer_slot_proof(runtime, immutable_inferred)!
	mutation_trace := multiwindow_sokol_trace.snapshot()
	resources.update_buffer(immutable_inferred, &data) or {
		proof.immutable_update_error = err.msg()
	}
	_ := resources.append_buffer(immutable_inferred, &data) or {
		proof.immutable_append_error = err.msg()
		-1
	}
	if multiwindow_sokol_trace.snapshot() != mutation_trace {
		return error('immutable mutation reached a Sokol update or append operation')
	}
	proof.immutable_after_mutation = multiwindow_buffer_slot_proof(runtime, immutable_inferred)!
	for id in [dynamic, stream, immutable_inferred, immutable_exact] {
		resources.retire_buffer(id)!
	}
}

fn multiwindow_expect_invalid_buffer_before_fault(mut resources AppResourceContext, desc &gfx.BufferDesc, fault_message string) ! {
	mut runtime := resources.app.render_runtime
	before_plan := multiwindow_buffer_fault_plan(runtime)
	before_trace := multiwindow_sokol_trace.snapshot()
	before_registry := multiwindow_buffer_registry_proof(runtime)
	mut actual := ''
	_ := resources.make_buffer(desc) or {
		actual = err.msg()
		WindowBufferId{}
	}
	if actual != err_multiwindow_render_invalid_descriptor {
		return error('invalid buffer descriptor returned `${actual}`')
	}
	after_plan := multiwindow_buffer_fault_plan(runtime)
	if !multiwindow_buffer_fault_plans_equal(before_plan, after_plan)
		|| after_plan.stage != .resource_make_buffer || after_plan.message != fault_message {
		return error('invalid buffer descriptor consumed or changed the make fault plan')
	}
	if multiwindow_sokol_trace.snapshot() != before_trace {
		return error('invalid buffer descriptor reached Sokol make_buffer')
	}
	after_registry := multiwindow_buffer_registry_proof(runtime)
	if !multiwindow_buffer_registry_proofs_equal(before_registry, after_registry) {
		return error('invalid buffer descriptor changed the managed resource registry')
	}
}

fn test_multiwindow_append_uses_sokol_alignment_reset_and_preflight() {
	if !multiwindow_buffer_contract_runtime_requested() {
		return
	}
	mut app := multiwindow_buffer_contract_new_app()!
	mut proof := &MultiWindowBufferAppendProof{}
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut proof] (mut resources AppResourceContext) ! {
			multiwindow_run_buffer_append_init(mut resources, mut proof)!
		}
		app_resource_frame_fn:   fn [mut proof] (mut resources AppResourceContext) ! {
			multiwindow_run_buffer_append_frame(mut resources, mut proof)!
		}
		app_resource_cleanup_fn: fn [mut proof] (mut resources AppResourceContext) ! {
			_ = resources
			proof.cleanup_calls++
			proof.cleanup_trace = multiwindow_sokol_trace.snapshot()
			for buffer in proof.saved_buffers {
				proof.cleanup_states << gfx.query_buffer_state(buffer)
			}
			multiwindow_sokol_trace.uninstall()
			proof.trace_installed = false
		}
	) or { run_error = err.msg() }
	mut emergency_trace := []string{}
	if proof.trace_installed {
		emergency_trace = multiwindow_sokol_trace.snapshot()
		multiwindow_sokol_trace.uninstall()
		proof.trace_installed = false
	}
	mut forced_stop_error := ''
	if app.core.status() != .stopped {
		app.stop() or { forced_stop_error = err.msg() }
	}
	mut replay_stop_error := ''
	app.stop() or { replay_stop_error = err.msg() }
	terminal_registry := multiwindow_buffer_registry_proof(app.render_runtime)

	assert run_error == ''
	assert emergency_trace.len == 0
	assert forced_stop_error == ''
	assert replay_stop_error == ''
	assert proof.frame_calls == 2
	assert proof.first_offsets == [0, 4, 8]
	assert proof.reset_offset == 0
	assert proof.overflow_error == err_multiwindow_render_buffer_overflow
	assert proof.update_then_append_error == err_multiwindow_render_mutation_mode
	assert proof.append_then_update_error == err_multiwindow_render_mutation_mode
	assert proof.overflow_trace_after == proof.overflow_trace_before
	assert proof.update_append_trace_after == proof.update_append_trace_before
	assert proof.append_update_trace_after == proof.append_update_trace_before
	assert multiwindow_buffer_slot_proofs_equal(proof.overflow_slot_before,
		proof.overflow_slot_after)
	assert multiwindow_buffer_slot_proofs_equal(proof.update_append_slot_before,
		proof.update_append_slot_after)
	assert multiwindow_buffer_slot_proofs_equal(proof.append_update_slot_before,
		proof.append_update_slot_after)
	assert proof.overflow_slot_before.mutation_mode == .append
	assert proof.update_append_slot_before.mutation_mode == .update
	assert proof.append_update_slot_before.mutation_mode == .append
	assert proof.overflow_slot_before.last_mutation_batch != 0
	assert proof.reset_slot.last_mutation_batch != 0
	assert proof.reset_slot.last_mutation_batch != proof.overflow_slot_before.last_mutation_batch
	assert proof.reset_slot.mutation_mode == .append
	assert proof.reset_trace_before.len > 0
	assert proof.reset_trace_before.last() == 'commit'
	assert proof.reset_trace_after.len == proof.reset_trace_before.len + 1
	assert proof.reset_trace_after.last() == 'append_buffer'
	assert proof.cleanup_calls == 1
	assert proof.cleanup_states == [.invalid, .invalid, .invalid]
	assert proof.cleanup_trace == ['make_buffer', 'make_buffer', 'make_buffer', 'append_buffer',
		'append_buffer', 'append_buffer', 'update_buffer', 'append_buffer', 'begin_swapchain_pass',
		'end_pass', 'commit', 'append_buffer', 'begin_swapchain_pass', 'end_pass', 'commit',
		'destroy_buffer', 'destroy_buffer', 'destroy_buffer']
	assert terminal_registry.slots == 3
	assert terminal_registry.alive == 0
	assert terminal_registry.retiring == 0
	assert terminal_registry.invalid == 3
	assert terminal_registry.deferred == 0
	assert terminal_registry.slot_native_ids == 0
	assert terminal_registry.deferred_native_ids == 0
	assert app.core.status() == .stopped
	assert !app.gfx_started
}

fn test_multiwindow_renderer_initialization_failure_reports_renderer_lost_once() {
	if !multiwindow_buffer_contract_runtime_requested() {
		return
	}
	mut app := multiwindow_buffer_contract_new_app()!
	mut proof := &MultiWindowCleanupCauseProof{}
	first := multiwindow_create_cleanup_cause_window(mut app, 'renderer-init-failure-first', mut
		proof)!
	second := multiwindow_create_cleanup_cause_window(mut app, 'renderer-init-failure-second', mut
		proof)!
	message := 'fault:renderer_sgl_setup:cleanup-cause'
	app.render_runtime.set_internal_fault(.renderer_sgl_setup, 0, message)!
	mut init_error := ''
	app.ensure_render_initialized() or { init_error = err.msg() }
	assert init_error.contains(message)
	assert app.renderer_terminal_failure
	assert !app.gfx_started
	assert !app.core.renderer_is_usable()

	app.stop()!
	multiwindow_assert_cleanup_cause_observations(proof.observations, [first, second],
		.renderer_lost, false)
	first_stop := proof.observations.clone()
	app.stop()!
	assert proof.observations == first_stop
	assert app.core.status() == .stopped
	assert !app.gfx_started
	assert !app.core.renderer_is_usable()
}

fn test_multiwindow_runtime_callback_error_after_completed_pass_reports_app_stop_without_renderer_loss() {
	if !multiwindow_buffer_contract_runtime_requested() {
		return
	}
	mut app := multiwindow_buffer_contract_new_app()!
	mut proof := &MultiWindowCleanupCauseProof{}
	message := 'fault:completed-pass-terminal-runtime'
	first := multiwindow_create_terminal_frame_window(mut app, 'renderer-runtime-failure-first',
		message, mut proof)!
	second := multiwindow_create_terminal_frame_window(mut app, 'renderer-runtime-failure-second',
		message, mut proof)!
	mut run_error := ''
	app.run() or { run_error = err.msg() }
	assert run_error.contains(message)
	assert proof.frame_calls == 1
	assert proof.completed_passes == 1
	assert !app.renderer_terminal_failure
	multiwindow_assert_cleanup_cause_observations(proof.observations, [first, second], .app_stop,
		true)
	assert app.core.status() == .stopped
	assert !app.gfx_started
	assert !app.core.renderer_is_usable()

	first_stop := proof.observations.clone()
	mut replay_stop_error := ''
	app.stop() or { replay_stop_error = err.msg() }
	assert replay_stop_error == run_error
	assert proof.observations == first_stop
}

fn test_multiwindow_callback_deferred_healthy_stop_reports_app_stop_once() {
	if !multiwindow_buffer_contract_runtime_requested() {
		return
	}
	mut app := multiwindow_buffer_contract_new_app()!
	mut proof := &MultiWindowCleanupCauseProof{}
	first := multiwindow_create_cleanup_cause_window(mut app, 'healthy-stop-first', mut proof)!
	second := multiwindow_create_cleanup_cause_window(mut app, 'healthy-stop-second', mut proof)!
	app.ensure_render_initialized()!
	assert app.gfx_started
	assert app.core.renderer_is_usable()

	app.run(
		event_fn: fn [mut proof] (event WindowEvent, mut app App) ! {
			proof.event_calls++
			if proof.stop_requests == 0 && event.kind == .window_created {
				proof.stop_requests++
				app.stop()!
			}
		}
	)!
	assert proof.event_calls >= 1
	assert proof.stop_requests == 1
	multiwindow_assert_cleanup_cause_observations(proof.observations, [first, second], .app_stop,
		true)
	first_stop := proof.observations.clone()
	app.stop()!
	assert proof.observations == first_stop
	assert app.core.status() == .stopped
	assert !app.gfx_started
	assert !app.core.renderer_is_usable()
}

fn test_multiwindow_real_egl_bad_native_window_reports_native_closed_once_and_replays_stop() {
	if !multiwindow_buffer_contract_runtime_requested() {
		return
	}
	selected_backend := os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND')
	if selected_backend !in ['x11', 'wayland'] {
		if selected_backend !in ['appkit', 'win32'] {
			panic('VGG_MULTIWINDOW_RUNTIME_BACKEND must select x11, wayland, appkit, or win32')
		}
		return
	}
	multiwindow_require_native_cleanup_parent_gate()!
	mut app := multiwindow_buffer_contract_new_app()!
	mut proof := &MultiWindowCleanupCauseProof{}
	proof.victim = multiwindow_create_native_close_victim(mut app, mut proof)!
	proof.survivor = multiwindow_create_native_close_survivor(mut app, mut proof)!
	app.core.set_render_workload(proof.survivor.core, false)!
	app.enable_multiwindow_lifecycle_trace()

	mut run_error := ''
	app.run() or { run_error = err.msg() }
	assert run_error == ''
	assert !proof.native_ticket_armed
	assert proof.native_friend_finished
	assert proof.victim_frame_calls == 1
	assert proof.victim_recoverable_error_observed
	assert proof.victim_recoverable_error_message == 'multiwindow: native render window was lost'
	assert proof.victim_completed_passes == 0
	assert proof.victim_recoverable_error_consumed
	assert proof.survivor_callbacks_before_friend_finish == 0
	assert proof.survivor_frame_calls == 1
	assert proof.survivor_completed_passes == 1
	assert proof.survivor_exists_after_pass
	assert proof.survivor_renderer_usable_after_pass
	assert proof.observations.len == 2
	assert proof.observations[0] == MultiWindowCleanupCauseObservation{
		window:             proof.victim
		reason:             .native_closed
		graphics_available: true
	}
	assert proof.observations[1] == MultiWindowCleanupCauseObservation{
		window:             proof.survivor
		reason:             .app_stop
		graphics_available: true
	}
	mut victim_cleanup_calls := 0
	for observation in proof.observations {
		if observation.window == proof.victim {
			victim_cleanup_calls++
			assert observation.reason == .native_closed
		}
	}
	assert victim_cleanup_calls == 1

	first_observations := proof.observations.clone()
	first_lifecycle := app.multiwindow_lifecycle_trace_snapshot()
	first_terminal := multiwindow_cleanup_terminal_proof(app)!
	mut replay_stop_error := ''
	app.stop() or { replay_stop_error = err.msg() }
	second_lifecycle := app.multiwindow_lifecycle_trace_snapshot()
	second_terminal := multiwindow_cleanup_terminal_proof(app)!
	assert replay_stop_error == ''
	assert proof.observations == first_observations
	multiwindow_assert_cleanup_lifecycle_snapshots_equal(first_lifecycle, second_lifecycle)
	assert second_terminal == first_terminal
	multiwindow_assert_native_cleanup_terminal(first_terminal)
	assert !proof.native_ticket_armed
	assert proof.native_friend_finished
	app.disarm_multiwindow_lifecycle_trace()
}

fn multiwindow_create_native_close_victim(mut app App, mut proof MultiWindowCleanupCauseProof) !WindowId {
	return app.create_window(
		title:       'native-close-victim'
		width:       96
		height:      72
		visible:     true
		redraw_mode: .continuous
		frame_fn:    fn [mut proof] (mut context WindowContext) ! {
			proof.victim_frame_calls++
			if proof.victim_frame_calls != 1 || proof.native_ticket_armed {
				return error('native BAD_NATIVE_WINDOW victim reached an unexpected replay frame')
			}
			real_core_lease := context.app.render_runtime.target_lease(context.info.window,
				context.lease_epoch)!
			proof.native_ticket =
				context.app.core.arm_gg_egl_bad_native_window_for_test(real_core_lease)!
			proof.native_ticket_armed = true
			mut swapchain_error := IError(none)
			context.with_swapchain(gfx.PassAction{}, fn [mut proof] (mut pass WindowPassContext) ! {
				_ = pass
				proof.victim_completed_passes++
			}) or { swapchain_error = err }
			if swapchain_error is none {
				return error('native BAD_NATIVE_WINDOW swapchain boundary returned normally')
			}
			proof.victim_recoverable_error_message = swapchain_error.msg()
			if proof.victim_recoverable_error_message != 'multiwindow: native render window was lost' {
				return error('native BAD_NATIVE_WINDOW returned `${proof.victim_recoverable_error_message}`')
			}
			proof.victim_recoverable_error_observed = true
			proof.victim_recoverable_error_consumed = true
			mut facade := context.app
			facade.core.finish_gg_egl_bad_native_window_proof_for_test(proof.native_ticket)!
			proof.native_ticket_armed = false
			proof.native_friend_finished = true
		}
		cleanup_fn:  fn [mut proof] (mut context WindowCleanupContext) ! {
			multiwindow_record_cleanup_cause(mut context, mut proof)
			if context.reason() != .native_closed {
				return error('native BAD_NATIVE_WINDOW victim cleanup used the wrong reason')
			}
			if !proof.victim_recoverable_error_observed || !proof.victim_recoverable_error_consumed
				|| proof.native_ticket_armed || !proof.native_friend_finished {
				return error('native BAD_NATIVE_WINDOW cleanup preceded friend verification and disarm')
			}
			mut facade := context.app
			facade.core.set_render_workload(proof.survivor.core, true)!
		}
	)
}

fn multiwindow_create_native_close_survivor(mut app App, mut proof MultiWindowCleanupCauseProof) !WindowId {
	return app.create_window(
		title:       'native-close-survivor'
		width:       96
		height:      72
		visible:     true
		redraw_mode: .on_demand
		frame_fn:    fn [mut proof] (mut context WindowContext) ! {
			proof.survivor_frame_calls++
			if !proof.native_friend_finished || proof.native_ticket_armed {
				proof.survivor_callbacks_before_friend_finish++
				return error('native BAD_NATIVE_WINDOW survivor ran before friend completion')
			}
			if proof.observations.len != 1 || proof.observations[0].window != proof.victim
				|| proof.observations[0].reason != .native_closed {
				return
			}
			if proof.survivor_completed_passes != 0 {
				return error('native BAD_NATIVE_WINDOW survivor reached an unexpected replay frame')
			}
			context.with_swapchain(gfx.PassAction{}, fn [mut proof] (mut pass WindowPassContext) ! {
				_ = pass
				proof.survivor_completed_passes++
			})!
			proof.survivor_exists_after_pass = context.exists()
			proof.survivor_renderer_usable_after_pass = context.app.core.renderer_is_usable()
			mut facade := context.app
			facade.stop()!
		}
		cleanup_fn:  fn [mut proof] (mut context WindowCleanupContext) ! {
			multiwindow_record_cleanup_cause(mut context, mut proof)
		}
	)
}

fn multiwindow_require_native_cleanup_parent_gate() ! {
	if os.getenv(multiwindow_probe_gate.environment_name) == '' {
		return error('native cleanup proof requires the parent process-tree watchdog start gate')
	}
	multiwindow_probe_gate.await_parent_release(2 * time.second)!
}

fn multiwindow_cleanup_runtime_proof(runtime &MultiWindowRenderRuntime) MultiWindowCleanupRuntimeProof {
	runtime.mutex.lock()
	mut proof := MultiWindowCleanupRuntimeProof{
		window_slots:            runtime.windows.len
		next_lease_epoch:        runtime.next_lease_epoch
		next_pass_epoch:         runtime.next_pass_epoch
		active_batch_epoch:      runtime.active_batch_epoch
		last_completed_batch:    runtime.last_completed_batch
		batch_active:            runtime.batch_active
		callback_depth:          runtime.callback_depth
		deferred_windows:        runtime.deferred_windows.len
		deferred_stop:           runtime.deferred_stop
		stopping:                runtime.stopping
		stopped:                 runtime.stopped
		app_lease_epoch:         runtime.app_lease_epoch
		app_phase:               runtime.app_phase
		app_resource_active:     runtime.app_resource_active
		app_init_started:        runtime.app_init_started
		app_init_completed:      runtime.app_init_completed
		app_init_terminal:       runtime.app_init_terminal
		app_cleanup_started:     runtime.app_cleanup_started
		app_cleanup_finished:    runtime.app_cleanup_finished
		app_cleanup_lease_epoch: runtime.app_cleanup_lease_epoch
		app_resources_retired:   runtime.app_resources_retired
	}
	for window in runtime.windows {
		match window.status {
			.invalid { proof.window_invalid++ }
			.registered { proof.window_registered++ }
			.initialized { proof.window_initialized++ }
			.closing { proof.window_closing++ }
			.destroyed { proof.window_destroyed++ }
		}

		if window.cleanup_started {
			proof.cleanup_started++
		}
		if window.cleanup_finished {
			proof.cleanup_finished++
		}
		if window.cleanup_reason_set {
			proof.cleanup_reason_set++
			match window.cleanup_reason {
				.native_closed { proof.native_closed_reasons++ }
				.app_stop { proof.app_stop_reasons++ }
				else {}
			}
		}
		if window.target_available {
			proof.targets_available++
		}
		if window.target_identity != 0 {
			proof.target_identities++
		}
		if window.active_lease_epoch != 0 {
			proof.active_window_leases++
		}
		if window.resource_section_active {
			proof.active_resource_sections++
		}
		if window.pass_active || window.pass_epoch != 0 {
			proof.active_passes++
		}
		if window.sgl_active {
			proof.active_sgl++
		}
	}
	runtime.mutex.unlock()
	return proof
}

fn multiwindow_cleanup_terminal_proof(app &App) !MultiWindowCleanupTerminalProof {
	return MultiWindowCleanupTerminalProof{
		core_status:               app.core.status()
		core_window_count:         app.window_ids()!.len
		gfx_started:               app.gfx_started
		sgl_initialized:           app.sgl_initialized
		render_owner_claimed:      app.render_owner_claimed
		renderer_usable:           app.core.renderer_is_usable()
		renderer_device_available: app.core.renderer_device_available_for_gg()
		renderer_terminal_failure: app.renderer_terminal_failure
		terminal_error:            app.terminal_error
		sgl_contexts:              app.sgl_contexts.len
		sgl_context_targets:       app.sgl_context_targets.len
		window_sgl_targets:        app.window_sgl_targets.len
		deferred_sgl_targets:      app.deferred_sgl_targets.len
		active_render_snapshots:   app.active_render_snapshots.len
		active_drawn_windows:      app.active_drawn_windows.len
		active_batch_epoch:        app.active_batch_epoch
		app_frame_active:          app.app_frame_active
		runtime:                   multiwindow_cleanup_runtime_proof(app.render_runtime)
		resources:                 multiwindow_buffer_registry_proof(app.render_runtime)
	}
}

fn multiwindow_assert_cleanup_lifecycle_snapshots_equal(first MultiWindowLifecycleTraceSnapshot, second MultiWindowLifecycleTraceSnapshot) {
	assert first.trace_enabled
	assert !first.overflow
	assert !first.mismatch
	assert first.len > 0
	assert second.trace_enabled == first.trace_enabled
	assert second.len == first.len
	assert second.overflow == first.overflow
	assert second.mismatch == first.mismatch
	for index in 0 .. multiwindow_lifecycle_trace_capacity {
		assert second.milestones[index] == first.milestones[index]
		if index >= first.len {
			assert first.milestones[index] == .invalid
		}
	}
}

fn multiwindow_assert_native_cleanup_terminal(proof MultiWindowCleanupTerminalProof) {
	assert proof.core_status == .stopped
	assert proof.core_window_count == 0
	assert !proof.gfx_started
	assert !proof.sgl_initialized
	assert !proof.render_owner_claimed
	assert !proof.renderer_usable
	assert !proof.renderer_device_available
	assert !proof.renderer_terminal_failure
	assert proof.terminal_error == ''
	assert proof.sgl_contexts == 0
	assert proof.sgl_context_targets == 0
	assert proof.window_sgl_targets == 0
	assert proof.deferred_sgl_targets == 0
	assert proof.active_render_snapshots == 0
	assert proof.active_drawn_windows == 0
	assert proof.active_batch_epoch == 0
	assert !proof.app_frame_active
	assert proof.runtime.window_slots == 2
	assert proof.runtime.window_invalid == 0
	assert proof.runtime.window_registered == 0
	assert proof.runtime.window_initialized == 0
	assert proof.runtime.window_closing == 0
	assert proof.runtime.window_destroyed == 2
	assert proof.runtime.cleanup_started == 2
	assert proof.runtime.cleanup_finished == 2
	assert proof.runtime.cleanup_reason_set == 2
	assert proof.runtime.native_closed_reasons == 1
	assert proof.runtime.app_stop_reasons == 1
	assert proof.runtime.targets_available == 0
	assert proof.runtime.target_identities == 0
	assert proof.runtime.active_window_leases == 0
	assert proof.runtime.active_resource_sections == 0
	assert proof.runtime.active_passes == 0
	assert proof.runtime.active_sgl == 0
	assert proof.runtime.last_completed_batch != 0
	assert proof.runtime.active_batch_epoch == 0
	assert !proof.runtime.batch_active
	assert proof.runtime.callback_depth == 0
	assert proof.runtime.deferred_windows == 0
	assert !proof.runtime.deferred_stop
	assert !proof.runtime.stopping
	assert proof.runtime.stopped
	assert proof.runtime.app_lease_epoch == 0
	assert proof.runtime.app_phase == .invalid
	assert !proof.runtime.app_resource_active
	assert proof.runtime.app_init_terminal == ''
	assert proof.runtime.app_cleanup_lease_epoch == 0
	assert proof.runtime.app_resources_retired
	assert proof.resources.slots == 0
	assert proof.resources.alive == 0
	assert proof.resources.retiring == 0
	assert proof.resources.invalid == 0
	assert proof.resources.exhausted == 0
	assert proof.resources.deferred == 0
	assert proof.resources.dependency_edges == 0
	assert proof.resources.dependent_edges == 0
	assert proof.resources.slot_native_ids == 0
	assert proof.resources.deferred_native_ids == 0
	assert proof.resources.sgl_materializations == 0
}

fn multiwindow_create_cleanup_cause_window(mut app App, title string, mut proof MultiWindowCleanupCauseProof) !WindowId {
	return app.create_window(
		title:       title
		width:       96
		height:      72
		visible:     app.capabilities().backend == .wayland
		redraw_mode: .on_demand
		cleanup_fn:  fn [mut proof] (mut context WindowCleanupContext) ! {
			multiwindow_record_cleanup_cause(mut context, mut proof)
		}
	)
}

fn multiwindow_create_terminal_frame_window(mut app App, title string, message string, mut proof MultiWindowCleanupCauseProof) !WindowId {
	return app.create_window(
		title:       title
		width:       96
		height:      72
		visible:     true
		redraw_mode: .continuous
		frame_fn:    fn [message, mut proof] (mut context WindowContext) ! {
			proof.frame_calls++
			context.with_swapchain(gfx.PassAction{}, fn [mut proof] (mut pass WindowPassContext) ! {
				_ = pass
				proof.completed_passes++
			})!
			return error(message)
		}
		cleanup_fn:  fn [mut proof] (mut context WindowCleanupContext) ! {
			multiwindow_record_cleanup_cause(mut context, mut proof)
		}
	)
}

fn multiwindow_record_cleanup_cause(mut context WindowCleanupContext, mut proof MultiWindowCleanupCauseProof) {
	proof.observations << MultiWindowCleanupCauseObservation{
		window:             context.window_id()
		reason:             context.reason()
		graphics_available: context.graphics_available()
	}
}

fn multiwindow_assert_cleanup_cause_observations(actual []MultiWindowCleanupCauseObservation, windows []WindowId, reason WindowCleanupReason, graphics_available bool) {
	assert actual.len == windows.len
	for index, window in windows {
		assert actual[index].window == window
		assert actual[index].reason == reason
		assert actual[index].graphics_available == graphics_available
	}
}

fn multiwindow_run_buffer_append_init(mut resources AppResourceContext, mut proof MultiWindowBufferAppendProof) ! {
	multiwindow_sokol_trace.install()!
	proof.trace_installed = true
	proof.main = resources.make_buffer(&gfx.BufferDesc{
		size:  16
		usage: .stream
	})!
	proof.update_then_append = resources.make_buffer(&gfx.BufferDesc{
		size:  16
		usage: .dynamic
	})!
	proof.append_then_update = resources.make_buffer(&gfx.BufferDesc{
		size:  16
		usage: .stream
	})!
	mut runtime := resources.app.render_runtime
	for id in [proof.main, proof.update_then_append, proof.append_then_update] {
		proof.saved_buffers << multiwindow_buffer_native_handle(runtime, id)!
	}
}

fn multiwindow_run_buffer_append_frame(mut resources AppResourceContext, mut proof MultiWindowBufferAppendProof) ! {
	proof.frame_calls++
	mut runtime := resources.app.render_runtime
	bytes := [u8(1), 2, 3, 4, 5, 6, 7, 8]
	three := gfx.Range{
		ptr:  bytes.data
		size: 3
	}
	one := gfx.Range{
		ptr:  bytes.data
		size: 1
	}
	eight := gfx.Range{
		ptr:  bytes.data
		size: 8
	}
	four := gfx.Range{
		ptr:  bytes.data
		size: 4
	}
	if proof.frame_calls == 1 {
		proof.first_offsets << resources.append_buffer(proof.main, &three)!
		proof.first_offsets << resources.append_buffer(proof.main, &one)!
		proof.first_offsets << resources.append_buffer(proof.main, &eight)!
		proof.overflow_trace_before = multiwindow_sokol_trace.snapshot()
		proof.overflow_slot_before = multiwindow_buffer_slot_proof(runtime, proof.main)!
		_ := resources.append_buffer(proof.main, &one) or {
			proof.overflow_error = err.msg()
			-1
		}
		proof.overflow_trace_after = multiwindow_sokol_trace.snapshot()
		proof.overflow_slot_after = multiwindow_buffer_slot_proof(runtime, proof.main)!

		resources.update_buffer(proof.update_then_append, &four)!
		proof.update_append_trace_before = multiwindow_sokol_trace.snapshot()
		proof.update_append_slot_before = multiwindow_buffer_slot_proof(runtime,
			proof.update_then_append)!
		_ := resources.append_buffer(proof.update_then_append, &four) or {
			proof.update_then_append_error = err.msg()
			-1
		}
		proof.update_append_trace_after = multiwindow_sokol_trace.snapshot()
		proof.update_append_slot_after = multiwindow_buffer_slot_proof(runtime,
			proof.update_then_append)!

		_ = resources.append_buffer(proof.append_then_update, &four)!
		proof.append_update_trace_before = multiwindow_sokol_trace.snapshot()
		proof.append_update_slot_before = multiwindow_buffer_slot_proof(runtime,
			proof.append_then_update)!
		resources.update_buffer(proof.append_then_update, &four) or {
			proof.append_then_update_error = err.msg()
		}
		proof.append_update_trace_after = multiwindow_sokol_trace.snapshot()
		proof.append_update_slot_after = multiwindow_buffer_slot_proof(runtime,
			proof.append_then_update)!
		return
	}
	if proof.frame_calls == 2 {
		proof.reset_trace_before = multiwindow_sokol_trace.snapshot()
		proof.reset_offset = resources.append_buffer(proof.main, &four)!
		proof.reset_trace_after = multiwindow_sokol_trace.snapshot()
		proof.reset_slot = multiwindow_buffer_slot_proof(runtime, proof.main)!
		for id in [proof.main, proof.update_then_append, proof.append_then_update] {
			resources.retire_buffer(id)!
		}
		mut app := resources.app
		app.stop()!
		return
	}
	return error('app resource frame callback ran after its deferred stop')
}

fn multiwindow_buffer_contract_runtime_requested() bool {
	return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
}

fn multiwindow_buffer_contract_new_app() !&App {
	backend := match os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') {
		'x11' { MultiWindowBackend.x11 }
		'wayland' { .wayland }
		'appkit' { .appkit }
		'win32' { .win32 }
		else { return error('VGG_MULTIWINDOW_RUNTIME_BACKEND must select x11, wayland, appkit, or win32') }
	}

	mut app := new_app(
		backend:          backend
		queue_size:       8
		require_renderer: true
	)!
	caps := app.capabilities()
	backend_matches := match backend {
		.x11 { caps.x11 && caps.gl }
		.wayland { caps.wayland && caps.gl }
		.appkit { caps.metal }
		.win32 { caps.win32 && caps.d3d11 }
		else { false }
	}

	if caps.backend != backend || !caps.native || !caps.multi_window || !caps.explicit_swapchain
		|| !backend_matches {
		message := 'selected buffer contract backend `${backend}` is unavailable: ${caps}'
		app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
		return error(message)
	}
	return app
}

fn multiwindow_buffer_fault_plan(runtime &MultiWindowRenderRuntime) MultiWindowInternalFaultPlan {
	runtime.mutex.lock()
	plan := runtime.internal_fault
	runtime.mutex.unlock()
	return plan
}

fn multiwindow_buffer_fault_plans_equal(first MultiWindowInternalFaultPlan, second MultiWindowInternalFaultPlan) bool {
	return first.stage == second.stage && first.hits_before_failure == second.hits_before_failure
		&& first.message == second.message
}

fn multiwindow_buffer_registry_proof(runtime &MultiWindowRenderRuntime) MultiWindowBufferRegistryProof {
	runtime.mutex.lock()
	mut proof := MultiWindowBufferRegistryProof{
		slots:    runtime.resources.slots.len
		deferred: runtime.resources.deferred.len
	}
	for slot in runtime.resources.slots {
		match slot.status {
			.alive { proof.alive++ }
			.retiring { proof.retiring++ }
			.invalid { proof.invalid++ }
			.exhausted { proof.exhausted++ }
		}

		proof.dependency_edges += slot.dependencies.len
		proof.dependent_edges += slot.dependents.len
		proof.sgl_materializations += slot.materialized_sgl.len
		proof.slot_native_ids += multiwindow_buffer_slot_native_id_count(slot)
	}
	for retired in runtime.resources.deferred {
		proof.deferred_native_ids += multiwindow_buffer_retired_native_id_count(retired)
		proof.sgl_materializations += retired.materialized_sgl.len
	}
	runtime.mutex.unlock()
	return proof
}

fn multiwindow_buffer_slot_native_id_count(slot MultiWindowResourceSlot) int {
	mut count := 0
	if slot.buffer.id != 0 {
		count++
	}
	if slot.image.id != 0 {
		count++
	}
	if slot.sampler.id != 0 {
		count++
	}
	if slot.shader.id != 0 {
		count++
	}
	if slot.pipeline.id != 0 {
		count++
	}
	if slot.attachments.id != 0 {
		count++
	}
	return count
}

fn multiwindow_buffer_retired_native_id_count(resource MultiWindowRetiredResource) int {
	mut count := 0
	if resource.buffer.id != 0 {
		count++
	}
	if resource.image.id != 0 {
		count++
	}
	if resource.sampler.id != 0 {
		count++
	}
	if resource.shader.id != 0 {
		count++
	}
	if resource.pipeline.id != 0 {
		count++
	}
	if resource.attachments.id != 0 {
		count++
	}
	return count
}

fn multiwindow_buffer_registry_proofs_equal(first MultiWindowBufferRegistryProof, second MultiWindowBufferRegistryProof) bool {
	return first.slots == second.slots && first.alive == second.alive
		&& first.retiring == second.retiring && first.invalid == second.invalid
		&& first.exhausted == second.exhausted && first.deferred == second.deferred
		&& first.dependency_edges == second.dependency_edges
		&& first.dependent_edges == second.dependent_edges
		&& first.slot_native_ids == second.slot_native_ids
		&& first.deferred_native_ids == second.deferred_native_ids
		&& first.sgl_materializations == second.sgl_materializations
}

fn multiwindow_buffer_slot_proof(runtime &MultiWindowRenderRuntime, id WindowBufferId) !MultiWindowBufferSlotProof {
	key := buffer_resource_key(id)
	runtime.mutex.lock()
	if key.slot < 0 || key.slot >= runtime.resources.slots.len {
		runtime.mutex.unlock()
		return error('buffer proof has no registry slot')
	}
	slot := runtime.resources.slots[key.slot]
	if slot.app_instance != key.app_instance || slot.generation != key.generation
		|| slot.window != key.window || slot.kind != .buffer {
		runtime.mutex.unlock()
		return error('buffer proof key does not identify its registry slot')
	}
	buffer := slot.buffer
	proof := MultiWindowBufferSlotProof{
		status:              slot.status
		usage:               slot.buffer_usage
		capacity:            slot.buffer_capacity
		last_mutation_batch: slot.last_mutation_batch
		mutation_mode:       slot.buffer_mutation_mode
		native_id:           buffer.id
	}
	runtime.mutex.unlock()
	return MultiWindowBufferSlotProof{
		...proof
		native_state: gfx.query_buffer_state(buffer)
	}
}

fn multiwindow_buffer_native_handle(runtime &MultiWindowRenderRuntime, id WindowBufferId) !gfx.Buffer {
	key := buffer_resource_key(id)
	runtime.mutex.lock()
	if key.slot < 0 || key.slot >= runtime.resources.slots.len {
		runtime.mutex.unlock()
		return error('buffer handle proof has no registry slot')
	}
	slot := runtime.resources.slots[key.slot]
	if slot.status != .alive || slot.kind != .buffer || slot.app_instance != key.app_instance
		|| slot.generation != key.generation || slot.window != key.window || slot.buffer.id == 0 {
		runtime.mutex.unlock()
		return error('buffer handle proof does not identify a live native buffer')
	}
	buffer := slot.buffer
	runtime.mutex.unlock()
	return buffer
}

fn multiwindow_buffer_slot_proofs_equal(first MultiWindowBufferSlotProof, second MultiWindowBufferSlotProof) bool {
	return first.status == second.status && first.usage == second.usage
		&& first.capacity == second.capacity
		&& first.last_mutation_batch == second.last_mutation_batch
		&& first.mutation_mode == second.mutation_mode && first.native_id == second.native_id
		&& first.native_state == second.native_state
}
