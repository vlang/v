// vtest build: gg_multiwindow?
module gg

import gg.testdata.multiwindow_sokol_trace
import gg.testdata.multiwindow_probe_gate
import os
import sokol.gfx
import time
import x.multiwindow

#flag -DSOKOL_TRACE_HOOKS

struct MultiWindowMatrixPlanState {
	stage               MultiWindowInternalFaultStage
	hits_before_failure int
	message             string
}

struct MultiWindowSamplerDescSnapshot {
	start_canary        u32
	min_filter          gfx.Filter
	mag_filter          gfx.Filter
	mipmap_filter       gfx.Filter
	wrap_u              gfx.Wrap
	wrap_v              gfx.Wrap
	wrap_w              gfx.Wrap
	min_lod             f32
	max_lod             f32
	border_color        gfx.BorderColor
	compare             gfx.CompareFunc
	max_anisotropy      u32
	label_is_nil        bool
	gl_sampler          u32
	mtl_sampler_is_nil  bool
	d3d_sampler_is_nil  bool
	wgpu_sampler_is_nil bool
	end_canary          u32
}

struct MultiWindowMatrixSlotState {
	key                  string
	kind                 MultiWindowResourceKind
	status               MultiWindowResourceStatus
	app_scoped           bool
	label                string
	dependencies         []string
	dependents           []string
	last_mutation_batch  u64
	buffer_mutation_mode MultiWindowBufferMutationMode
	buffer_capacity      usize
	buffer_usage         gfx.Usage
	buffer_desc          []u8
	image_desc           []u8
	sampler_desc         MultiWindowSamplerDescSnapshot
	shader_desc          []u8
	pipeline_desc        []u8
	sgl_recipe           []u8
	buffer_id            u32
	image_id             u32
	sampler_id           u32
	shader_id            u32
	pipeline_id          u32
	attachments_id       u32
	image_width          int
	image_height         int
	image_sample_count   int
	image_pixel_format   gfx.PixelFormat
	image_usage          gfx.Usage
	image_render_target  bool
	target_identity      u64
	attachment_colors    []string
	attachment_resolves  []string
	attachment_depth     string
	sgl_shader           string
	sgl_materializations []string
}

struct MultiWindowMatrixDeferredState {
	key                  string
	kind                 MultiWindowResourceKind
	retire_batch         u64
	buffer_id            u32
	image_id             u32
	sampler_id           u32
	shader_id            u32
	pipeline_id          u32
	attachments_id       u32
	sgl_materializations []string
}

struct MultiWindowMatrixRegistryState {
	app_instance u64
	slots        []MultiWindowMatrixSlotState
	deferred     []MultiWindowMatrixDeferredState
}

struct MultiWindowMatrixCheckpoint {
	plan     MultiWindowMatrixPlanState
	registry MultiWindowMatrixRegistryState
	trace    multiwindow_sokol_trace.TypedSnapshot
}

struct MultiWindowMatrixDestroyExpectation {
	operation multiwindow_sokol_trace.Operation
	identity  u32
}

@[heap]
struct MultiWindowRealOperationProof {
mut:
	trace_generation                   u64
	trace_owned                        bool
	sgl_load_generation                u64
	init_calls                         int
	frame_calls                        int
	cleanup_calls                      int
	window_cleanup_calls               int
	cleanup_observations               []MultiWindowCleanupObservation
	window_frames                      int
	windows                            []WindowId
	update_buffer                      WindowBufferId
	append_buffer                      WindowBufferId
	update_image                       WindowImageId
	replace_image                      WindowImageId
	rebuild_image                      WindowImageId
	sampler                            WindowSamplerId
	shader                             WindowShaderId
	pipeline                           WindowPipelineId
	replace_attachments                WindowAttachmentsId
	replace_attachments_second         WindowAttachmentsId
	rebuild_attachments                WindowAttachmentsId
	rebuild_attachments_second         WindowAttachmentsId
	sgl_default                        WindowSglPipelineId
	sgl_shader                         WindowSglPipelineId
	default_target_keys                []string
	shader_target_key                  string
	default_sgl_native                 [][]u32
	shader_sgl_native                  []u32
	materialized_default               int
	materialized_shader                bool
	retired                            bool
	retirement_verified                bool
	retirement_before                  multiwindow_sokol_trace.TypedSnapshot
	retirement_after                   multiwindow_sokol_trace.TypedSnapshot
	destroy_expectations               []MultiWindowMatrixDestroyExpectation
	replacement_post_retry             multiwindow_sokol_trace.TypedSnapshot
	replacement_old_destroys           []MultiWindowMatrixDestroyExpectation
	replacement_old_images             []gfx.Image
	replacement_old_attachments_native []gfx.Attachments
	replacement_postcommit_verified    bool
	retry_labels                       []string
	retry_registry                     []MultiWindowMatrixRegistryState
}

struct MultiWindowCleanupObservation {
	window             WindowId
	reason             WindowCleanupReason
	graphics_available bool
}

struct MultiWindowCleanupProofState {
	trace_generation     u64
	trace_owned          bool
	init_calls           int
	window_frames        int
	window_cleanup_calls int
	app_cleanup_calls    int
	window               WindowId
	app_buffer           WindowBufferId
	window_buffer        WindowBufferId
	app_buffer_native    u32
	window_buffer_native u32
	framebuffer_width    int
	framebuffer_height   int
	observation          MultiWindowCleanupObservation
}

struct MultiWindowHealthyCleanupProofState {
	app_cleanup_calls    int
	window_cleanup_calls int
	observations         []MultiWindowCleanupObservation
}

@[heap]
struct MultiWindowCleanupErrorProof {
mut:
	trace_generation     u64
	trace_owned          bool
	init_calls           int
	window_frames        int
	window_cleanup_calls int
	app_cleanup_calls    int
	window               WindowId
	app_buffer           WindowBufferId
	window_buffer        WindowBufferId
	app_buffer_native    u32
	window_buffer_native u32
	framebuffer_width    int
	framebuffer_height   int
	observation          MultiWindowCleanupObservation
}

struct MultiWindowMatrixRuntimeState {
	window_count            int
	window_invalid          int
	window_destroyed        int
	cleanup_started         int
	cleanup_finished        int
	app_stop_reasons        int
	active_batch_epoch      u64
	last_completed_batch    u64
	batch_active            bool
	callback_depth          int
	deferred_windows        int
	deferred_stop           bool
	stopping                bool
	stopped                 bool
	app_resource_active     bool
	app_init_started        bool
	app_init_completed      bool
	app_init_terminal       string
	app_cleanup_started     bool
	app_cleanup_finished    bool
	app_resources_retired   bool
	next_lease_epoch        u64
	next_pass_epoch         u64
	app_lease_epoch         u64
	app_phase               MultiWindowRenderPhase
	app_cleanup_lease_epoch u64
	deferred_window_ids     []string
	windows_raw             [][]u8
	registry                MultiWindowMatrixRegistryState
}

struct MultiWindowMatrixTerminalState {
	app_instance              u64
	core_status               multiwindow.AppStatus
	gfx_started               bool
	sgl_initialized           bool
	render_owner_claimed      bool
	legacy_render_mode        bool
	app_frame_active          bool
	active_batch_epoch        u64
	sgl_contexts              int
	sgl_context_targets       int
	window_sgl_targets        int
	deferred_sgl_targets      int
	active_render_snapshots   int
	active_drawn_windows      int
	failed_init_windows       []string
	renderer_terminal_failure bool
	terminal_error            string
	runtime                   MultiWindowMatrixRuntimeState
}

fn test_multiwindow_render_internal_fault_plan_rejects_invalid_configuration() {
	mut runtime := new_multiwindow_render_runtime(42)
	for config in [
		MultiWindowInternalFaultPlan{
			stage:               .none
			hits_before_failure: 0
			message:             'none'
		},
		MultiWindowInternalFaultPlan{
			stage:               .resource_make_image
			hits_before_failure: -1
			message:             'negative'
		},
		MultiWindowInternalFaultPlan{
			stage:               .resource_make_image
			hits_before_failure: 0
			message:             ''
		},
	] {
		mut actual := ''
		runtime.set_internal_fault(config.stage, config.hits_before_failure, config.message) or {
			actual = err.msg()
		}
		assert actual == err_multiwindow_render_fault_config_invalid
	}
}

fn test_multiwindow_real_public_operation_fault_matrix_and_retirement() {
	if !multiwindow_fault_matrix_runtime_requested() {
		return
	}
	mut app := multiwindow_fault_matrix_new_app()!
	mut proof := &MultiWindowRealOperationProof{}
	for window_index, title in ['fault-matrix-default-sgl', 'fault-matrix-shader-sgl'] {
		proof.windows << app.create_window(
			title:       title
			width:       96
			height:      72
			visible:     true
			redraw_mode: .continuous
			frame_fn:    fn [window_index, mut proof] (mut context WindowContext) ! {
				multiwindow_fault_matrix_window_frame(window_index, mut context, mut proof)!
			}
			cleanup_fn:  fn [mut proof] (mut context WindowCleanupContext) ! {
				proof.window_cleanup_calls++
				proof.cleanup_observations << MultiWindowCleanupObservation{
					window:             context.window_id()
					reason:             context.reason()
					graphics_available: context.graphics_available()
				}
			}
		)!
	}
	app.enable_multiwindow_lifecycle_trace()
	proof.sgl_load_generation = multiwindow_sgl_load_proof_arm_for_test()!
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut proof] (mut resources AppResourceContext) ! {
			multiwindow_fault_matrix_initialize(mut resources, mut proof)!
		}
		app_resource_frame_fn:   fn [mut proof] (mut resources AppResourceContext) ! {
			multiwindow_fault_matrix_retire(mut resources, mut proof)!
		}
		app_resource_cleanup_fn: fn [mut proof] (mut resources AppResourceContext) ! {
			proof.cleanup_calls++
			_ = resources
		}
	) or { run_error = err.msg() }
	assert run_error == ''
	assert proof.trace_owned
	assert proof.init_calls == 1
	assert proof.cleanup_calls == 1
	assert proof.window_cleanup_calls == proof.windows.len
	multiwindow_assert_healthy_cleanup_observations(proof.cleanup_observations, proof.windows)
	assert proof.window_frames == 2
	assert proof.materialized_default == 2
	assert proof.materialized_shader
	assert proof.default_target_keys.len == 2
	assert proof.default_target_keys[0] != ''
	assert proof.default_target_keys[1] != ''
	assert proof.shader_target_key != ''
	assert proof.default_target_keys[0] != proof.default_target_keys[1]
	assert proof.default_sgl_native.len == 2
	assert proof.default_sgl_native[0].len == 5
	assert proof.default_sgl_native[1].len == 5
	assert proof.shader_sgl_native.len == 5
	sgl_loads := multiwindow_sgl_load_proof_snapshot_for_test(proof.sgl_load_generation)!
	assert sgl_loads.armed
	assert !sgl_loads.overflow
	assert sgl_loads.records.len == 3
	assert sgl_loads.records[0].target_key == proof.default_target_keys[0]
	assert sgl_loads.records[1].target_key == proof.shader_target_key
	assert sgl_loads.records[2].target_key == proof.default_target_keys[1]
	assert proof.retry_labels == [
		'make_buffer',
		'make_image',
		'make_sampler',
		'make_shader',
		'make_pipeline',
		'make_attachments',
		'make_sgl_pipeline',
		'make_sgl_pipeline_with_shader',
		'update_buffer',
		'append_buffer',
		'update_image',
		'replace_image',
		'rebuild_attachment',
		'materialize_default-target-a',
		'materialize_shader-target-a',
		'materialize_default-target-b',
	]
	assert proof.retry_registry.len == proof.retry_labels.len
	multiwindow_assert_final_retry_registry(proof)
	assert proof.retired
	assert proof.retirement_verified
	assert proof.replacement_postcommit_verified
	multiwindow_assert_retirement_trace(proof.retirement_before, proof.retirement_after,
		proof.destroy_expectations)
	shutdown_trace := multiwindow_sokol_trace.typed_snapshot()
	multiwindow_assert_shutdown_destruction_tail(proof.retirement_after, shutdown_trace)
	multiwindow_sokol_trace.logical_release_generation_after_shutdown(proof.trace_generation)!
	proof.trace_owned = false
	released_trace := multiwindow_sokol_trace.typed_snapshot()
	multiwindow_assert_trace_retained_after_logical_release(shutdown_trace, released_trace)
	lifecycle := app.multiwindow_lifecycle_trace_snapshot()
	multiwindow_assert_healthy_sgl_shutdown_order(lifecycle)
	terminal := multiwindow_fault_matrix_terminal_state(app)
	multiwindow_assert_terminal_registry_empty(terminal.runtime.registry)
	cleanup_state := multiwindow_healthy_cleanup_proof_state(proof)
	mut replay_error := ''
	app.stop() or { replay_error = err.msg() }
	assert replay_error == ''
	assert multiwindow_fault_matrix_terminal_state(app) == terminal
	assert multiwindow_healthy_cleanup_proof_state(proof) == cleanup_state
	assert multiwindow_sokol_trace.typed_snapshot() == released_trace
	multiwindow_assert_lifecycle_trace_equal(lifecycle, app.multiwindow_lifecycle_trace_snapshot())
	multiwindow_sgl_load_proof_disarm_for_test(proof.sgl_load_generation)!
	app.disarm_multiwindow_lifecycle_trace()
}

fn test_multiwindow_sgl_setup_fault_is_terminal_through_public_run_without_fake_shutdown() {
	if !multiwindow_fault_matrix_runtime_requested() {
		return
	}
	mut app := multiwindow_fault_matrix_new_app()!
	message := 'fault:renderer_sgl_setup:public-run'
	app.render_runtime.set_internal_fault(.renderer_sgl_setup, 0, message)!
	app.enable_multiwindow_lifecycle_trace()
	mut init_calls := 0
	mut run_error := ''
	app.run(
		app_resource_init_fn: fn [mut init_calls] (mut resources AppResourceContext) ! {
			init_calls++
			_ = resources
		}
	) or { run_error = err.msg() }
	assert init_calls == 0
	assert run_error.contains(message)
	assert app.terminal_error == run_error
	assert app.renderer_terminal_failure
	assert app.core.status() == .stopped
	assert !app.gfx_started
	assert !app.sgl_initialized
	lifecycle := app.multiwindow_lifecycle_trace_snapshot()
	for index in 0 .. lifecycle.len {
		assert lifecycle.milestones[index] != .sgl_shutdown_enter
		assert lifecycle.milestones[index] != .sgl_shutdown_complete
	}
	terminal := multiwindow_fault_matrix_terminal_state(app)
	mut replay_error := ''
	app.stop() or { replay_error = err.msg() }
	assert replay_error == run_error
	assert multiwindow_fault_matrix_terminal_state(app) == terminal
	multiwindow_assert_lifecycle_trace_equal(lifecycle, app.multiwindow_lifecycle_trace_snapshot())
	app.disarm_multiwindow_lifecycle_trace()
}

fn test_multiwindow_cleanup_callback_errors_retire_totally_and_replay_exactly() {
	if !multiwindow_fault_matrix_runtime_requested() {
		return
	}
	mut app := multiwindow_fault_matrix_new_app()!
	mut proof := &MultiWindowCleanupErrorProof{}
	window_message := 'fault-matrix-window-cleanup-error'
	app_message := 'fault-matrix-app-cleanup-error'
	proof.window = app.create_window(
		title:       'fault-matrix-cleanup-errors'
		width:       96
		height:      72
		visible:     true
		redraw_mode: .continuous
		frame_fn:    fn [mut proof] (mut context WindowContext) ! {
			if proof.window_frames != 0 {
				return
			}
			framebuffer_size := context.framebuffer_size()
			proof.framebuffer_width = framebuffer_size.width
			proof.framebuffer_height = framebuffer_size.height
			proof.window_frames++
			context.with_resources(fn [mut proof] (mut window_resources WindowResourceContext) ! {
				proof.window_buffer = window_resources.make_buffer(&gfx.BufferDesc{
					size:  32
					usage: .dynamic
				})!
				proof.window_buffer_native = multiwindow_fault_matrix_buffer_native(window_resources.app.render_runtime,
					proof.window_buffer)!
			})!
			mut app_facade := context.app
			app_facade.stop()!
		}
		cleanup_fn:  fn [window_message, mut proof] (mut context WindowCleanupContext) ! {
			proof.window_cleanup_calls++
			proof.observation = MultiWindowCleanupObservation{
				window:             context.window_id()
				reason:             context.reason()
				graphics_available: context.graphics_available()
			}
			return error(window_message)
		}
	)!
	app.enable_multiwindow_lifecycle_trace()
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut proof] (mut resources AppResourceContext) ! {
			proof.init_calls++
			proof.trace_generation = multiwindow_sokol_trace.install_generation()!
			proof.trace_owned = true
			proof.app_buffer = resources.make_buffer(&gfx.BufferDesc{
				size:  32
				usage: .dynamic
			})!
			proof.app_buffer_native = multiwindow_fault_matrix_buffer_native(resources.app.render_runtime,
				proof.app_buffer)!
		}
		app_resource_cleanup_fn: fn [app_message, mut proof] (mut resources AppResourceContext) ! {
			proof.app_cleanup_calls++
			_ = resources
			return error(app_message)
		}
	) or { run_error = err.msg() }
	assert proof.trace_owned
	assert proof.init_calls == 1
	assert proof.window_frames == 1
	assert proof.framebuffer_width > 0
	assert proof.framebuffer_height > 0
	assert proof.window_cleanup_calls == 1
	assert proof.app_cleanup_calls == 1
	assert proof.observation.window == proof.window
	assert proof.observation.reason == .app_stop
	assert proof.observation.graphics_available
	shutdown_trace := multiwindow_sokol_trace.typed_snapshot()
	multiwindow_assert_cleanup_error_trace(shutdown_trace, proof.app_buffer_native,
		proof.window_buffer_native, proof.framebuffer_width, proof.framebuffer_height)
	multiwindow_sokol_trace.logical_release_generation_after_shutdown(proof.trace_generation)!
	proof.trace_owned = false
	released_trace := multiwindow_sokol_trace.typed_snapshot()
	multiwindow_assert_trace_retained_after_logical_release(shutdown_trace, released_trace)
	expected_error := multiwindow_cleanup_error_expected(window_message, app_message)
	assert run_error == expected_error
	assert run_error == app.terminal_error
	terminal := multiwindow_fault_matrix_terminal_state(app)
	multiwindow_assert_terminal_registry_empty(terminal.runtime.registry)
	assert terminal.runtime.app_resources_retired
	assert terminal.runtime.cleanup_started == 1
	assert terminal.runtime.cleanup_finished == 1
	assert terminal.runtime.app_stop_reasons == 1
	assert terminal.runtime.app_cleanup_started
	assert terminal.runtime.app_cleanup_finished
	lifecycle := app.multiwindow_lifecycle_trace_snapshot()
	multiwindow_assert_cleanup_error_lifecycle(lifecycle)
	cleanup_proof_state := multiwindow_cleanup_proof_state(proof)
	mut replay_error := ''
	app.stop() or { replay_error = err.msg() }
	assert replay_error == run_error
	assert multiwindow_fault_matrix_terminal_state(app) == terminal
	assert multiwindow_cleanup_proof_state(proof) == cleanup_proof_state
	assert multiwindow_sokol_trace.typed_snapshot() == released_trace
	multiwindow_assert_lifecycle_trace_equal(lifecycle, app.multiwindow_lifecycle_trace_snapshot())
	app.disarm_multiwindow_lifecycle_trace()
}

fn multiwindow_fault_matrix_runtime_requested() bool {
	return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
}

fn multiwindow_cleanup_error_expected(window_message string, app_message string) string {
	terminal_prefix := 'multiwindow: terminal lifecycle failed'
	window_callback := '${err_multiwindow_render_cleanup_failed}: ${window_message}'
	window_batch_callback := '${err_multiwindow_render_cleanup_failed}: ${window_callback}'
	window_batch_outcome := '${terminal_prefix}: ${window_batch_callback}'
	window_teardown := '${err_multiwindow_render_cleanup_failed}: ${window_batch_outcome}'
	window_core_finish := '${terminal_prefix}: ${window_teardown}'
	window_total := '${err_multiwindow_render_cleanup_failed}: ${window_teardown}; ${window_core_finish}'
	app_batch_callback := '${err_multiwindow_render_cleanup_failed}: ${app_message}'
	app_batch_outcome := '${terminal_prefix}: ${app_batch_callback}'
	app_total := '${err_multiwindow_render_cleanup_failed}: ${app_batch_outcome}'
	core_stop := '${terminal_prefix}: ${window_total}; ${app_total}'
	stop_total := '${err_multiwindow_render_cleanup_failed}: ${window_total}; ${app_total}; ${core_stop}'
	accepted := '${err_multiwindow_render_callback_failed}: ${stop_total}'
	return '${err_multiwindow_render_callback_failed}: ${accepted}; ${stop_total}'
}

fn multiwindow_cleanup_proof_state(proof &MultiWindowCleanupErrorProof) MultiWindowCleanupProofState {
	return MultiWindowCleanupProofState{
		trace_generation:     proof.trace_generation
		trace_owned:          proof.trace_owned
		init_calls:           proof.init_calls
		window_frames:        proof.window_frames
		window_cleanup_calls: proof.window_cleanup_calls
		app_cleanup_calls:    proof.app_cleanup_calls
		window:               proof.window
		app_buffer:           proof.app_buffer
		window_buffer:        proof.window_buffer
		app_buffer_native:    proof.app_buffer_native
		window_buffer_native: proof.window_buffer_native
		framebuffer_width:    proof.framebuffer_width
		framebuffer_height:   proof.framebuffer_height
		observation:          proof.observation
	}
}

fn multiwindow_healthy_cleanup_proof_state(proof &MultiWindowRealOperationProof) MultiWindowHealthyCleanupProofState {
	return MultiWindowHealthyCleanupProofState{
		app_cleanup_calls:    proof.cleanup_calls
		window_cleanup_calls: proof.window_cleanup_calls
		observations:         proof.cleanup_observations.clone()
	}
}

fn multiwindow_assert_healthy_cleanup_observations(actual []MultiWindowCleanupObservation, windows []WindowId) {
	assert actual.len == windows.len
	mut seen := []WindowId{}
	for observation in actual {
		assert observation.window in windows
		assert observation.window !in seen
		assert observation.reason == .app_stop
		assert observation.graphics_available
		seen << observation.window
	}
}

fn multiwindow_fault_matrix_new_app() !&App {
	multiwindow_fault_matrix_require_parent_gate()!
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
		message := 'selected fault-matrix backend `${backend}` is unavailable: ${caps}'
		app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
		return error(message)
	}
	return app
}

fn multiwindow_fault_matrix_require_parent_gate() ! {
	if os.getenv(multiwindow_probe_gate.environment_name) == '' {
		return error('fault-matrix runtime proof requires the parent process-tree watchdog start gate')
	}
	multiwindow_probe_gate.await_parent_release(2 * time.second)!
}

fn multiwindow_fault_matrix_initialize(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	proof.init_calls++
	proof.trace_generation = multiwindow_sokol_trace.install_generation()!
	proof.trace_owned = true
	multiwindow_fault_matrix_make_buffer(mut resources, mut proof)!
	multiwindow_fault_matrix_make_image(mut resources, mut proof)!
	multiwindow_fault_matrix_make_sampler(mut resources, mut proof)!
	multiwindow_fault_matrix_make_shader(mut resources, mut proof)!
	multiwindow_fault_matrix_make_pipeline(mut resources, mut proof)!
	multiwindow_fault_matrix_make_attachments(mut resources, mut proof)!
	multiwindow_fault_matrix_make_sgl_recipes(mut resources, mut proof)!
	multiwindow_fault_matrix_update_buffer(mut resources, mut proof)!
	multiwindow_fault_matrix_append_buffer(mut resources, mut proof)!
	multiwindow_fault_matrix_update_image(mut resources, mut proof)!
	multiwindow_fault_matrix_replace_image(mut resources, mut proof)!
	multiwindow_fault_matrix_rebuild_attachment(mut resources, mut proof)!
}

fn multiwindow_fault_matrix_make_buffer(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_make_buffer:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_buffer, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	_ := resources.make_buffer(&gfx.BufferDesc{
		usage: .dynamic
	}) or {
		invalid_error = err.msg()
		WindowBufferId{}
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	mut fault_error := ''
	_ := resources.make_buffer(&gfx.BufferDesc{
		size:  64
		usage: .dynamic
	}) or {
		fault_error = err.msg()
		WindowBufferId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.update_buffer = resources.make_buffer(&gfx.BufferDesc{
		size:  64
		usage: .dynamic
	})!
	native := multiwindow_fault_matrix_buffer_native(resources.app.render_runtime,
		proof.update_buffer)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.make_buffer, native, 0)
	retry_registry := multiwindow_record_retry_registry('make_buffer', armed.registry, [
		multiwindow_fault_matrix_key(buffer_resource_key(proof.update_buffer)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_buffer_retry_slot(retry_registry, proof.update_buffer, native, 64, .dynamic,
		.none, 0)
	proof.append_buffer = resources.make_buffer(&gfx.BufferDesc{
		size:  64
		usage: .stream
	})!
}

fn multiwindow_fault_matrix_make_image(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_make_image:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_image, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	invalid_pixels := [u8(1), 2, 3, 4]
	mut invalid_desc := gfx.ImageDesc{
		width:        1
		height:       1
		usage:        .dynamic
		pixel_format: .rgba8
	}
	invalid_desc.data.subimage[0][0] = gfx.Range{
		ptr:  invalid_pixels.data
		size: usize(invalid_pixels.len)
	}
	mut invalid_error := ''
	_ := resources.make_image(&invalid_desc) or {
		invalid_error = err.msg()
		WindowImageId{}
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	valid := multiwindow_fault_matrix_dynamic_image_desc()
	mut fault_error := ''
	_ := resources.make_image(&valid) or {
		fault_error = err.msg()
		WindowImageId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.update_image = resources.make_image(&valid)!
	native := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.update_image)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.make_image, native, 0)
	retry_registry := multiwindow_record_retry_registry('make_image', armed.registry, [
		multiwindow_fault_matrix_key(image_resource_key(proof.update_image)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_image_retry_slot(retry_registry, proof.update_image, native,
		multiwindow_fault_matrix_dynamic_image_desc(), 0)
	render_target := multiwindow_fault_matrix_render_target_desc()
	proof.replace_image = resources.make_image(&render_target)!
	proof.rebuild_image = resources.make_image(&render_target)!
}

fn multiwindow_fault_matrix_make_sampler(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_make_sampler:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_sampler, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	invalid := gfx.SamplerDesc{
		gl_sampler: 1
	}
	mut invalid_error := ''
	_ := resources.make_sampler(&invalid) or {
		invalid_error = err.msg()
		WindowSamplerId{}
	}
	assert invalid_error == err_multiwindow_render_native_injection
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	valid := gfx.SamplerDesc{
		min_filter: .nearest
		mag_filter: .nearest
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	}
	mut fault_error := ''
	_ := resources.make_sampler(&valid) or {
		fault_error = err.msg()
		WindowSamplerId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.sampler = resources.make_sampler(&valid)!
	native := multiwindow_fault_matrix_sampler_native(resources.app.render_runtime, proof.sampler)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.make_sampler, native, 0)
	retry_registry := multiwindow_record_retry_registry('make_sampler', armed.registry, [
		multiwindow_fault_matrix_key(sampler_resource_key(proof.sampler)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_sampler_retry_slot(retry_registry, proof.sampler, native, valid)
}

fn multiwindow_fault_matrix_make_shader(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_make_shader:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_shader, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid := gfx.ShaderDesc{}
	invalid.vs.bytecode.size = 1
	mut invalid_error := ''
	_ := resources.make_shader(&invalid) or {
		invalid_error = err.msg()
		WindowShaderId{}
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	valid := multiwindow_sokol_trace.shader_descriptor()!
	mut fault_error := ''
	_ := resources.make_shader(&valid) or {
		fault_error = err.msg()
		WindowShaderId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.shader = resources.make_shader(&valid)!
	native := multiwindow_fault_matrix_shader_native(resources.app.render_runtime, proof.shader)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.make_shader, native, 0)
	retry_registry := multiwindow_record_retry_registry('make_shader', armed.registry, [
		multiwindow_fault_matrix_key(shader_resource_key(proof.shader)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_shader_retry_slot(retry_registry, proof.shader, native, valid)
}

fn multiwindow_fault_matrix_make_pipeline(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_make_pipeline:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_pipeline, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	valid := multiwindow_fault_matrix_pipeline_desc()
	_ := resources.make_pipeline(&valid, WindowShaderId{}) or {
		invalid_error = err.msg()
		WindowPipelineId{}
	}
	assert invalid_error == err_multiwindow_render_resource_scope
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	mut fault_error := ''
	_ := resources.make_pipeline(&valid, proof.shader) or {
		fault_error = err.msg()
		WindowPipelineId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.pipeline = resources.make_pipeline(&valid, proof.shader)!
	native :=
		multiwindow_fault_matrix_pipeline_native(resources.app.render_runtime, proof.pipeline)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.make_pipeline, native, 0)
	retry_registry := multiwindow_record_retry_registry('make_pipeline', armed.registry, [
		multiwindow_fault_matrix_key(pipeline_resource_key(proof.pipeline)),
		multiwindow_fault_matrix_key(shader_resource_key(proof.shader)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_pipeline_retry_slot(armed.registry, retry_registry, proof.pipeline, native,
		proof.shader, valid)
	multiwindow_assert_query_defaults_compatibility(resources.app.render_runtime, proof)
}

fn multiwindow_assert_query_defaults_compatibility(runtime &MultiWindowRenderRuntime, proof &MultiWindowRealOperationProof) {
	runtime.mutex.lock()
	buffer := runtime.resources.slots[proof.update_buffer.slot].buffer
	image := runtime.resources.slots[proof.update_image.slot].image
	shader := runtime.resources.slots[proof.shader.slot].shader
	pipeline := runtime.resources.slots[proof.pipeline.slot].pipeline
	runtime.mutex.unlock()

	buffer_desc := gfx.query_buffer_desc(buffer)
	expected_buffer := gfx.query_buffer_desc_defaults(&buffer_desc)
	actual_buffer := gfx.query_buffer_defaults(&buffer)
	assert actual_buffer.size == expected_buffer.size
	assert actual_buffer.type == expected_buffer.type
	assert actual_buffer.usage == expected_buffer.usage
	assert actual_buffer.data.size == expected_buffer.data.size
	assert actual_buffer.size == 64
	assert actual_buffer.type == .vertexbuffer
	assert actual_buffer.usage == .dynamic
	assert actual_buffer.data.size == 64

	image_desc := gfx.query_image_desc(image)
	expected_image := gfx.query_image_desc_defaults(&image_desc)
	actual_image := gfx.query_image_defaults(&image)
	assert actual_image.type == expected_image.type
	assert actual_image.width == expected_image.width
	assert actual_image.height == expected_image.height
	assert actual_image.num_slices == expected_image.num_slices
	assert actual_image.num_mipmaps == expected_image.num_mipmaps
	assert actual_image.usage == expected_image.usage
	assert actual_image.pixel_format == expected_image.pixel_format
	assert actual_image.sample_count == expected_image.sample_count
	assert actual_image.type == ._2d
	assert actual_image.width == 4
	assert actual_image.height == 4
	assert actual_image.num_slices == 1
	assert actual_image.num_mipmaps == 1
	assert actual_image.usage == .dynamic
	assert actual_image.pixel_format == .rgba8
	assert actual_image.sample_count == 1

	shader_desc := gfx.query_shader_desc(shader)
	expected_shader := gfx.query_shader_desc_defaults(&shader_desc)
	actual_shader := gfx.query_shader_defaults(&shader)
	assert actual_shader.vs.uniform_blocks[0].size == expected_shader.vs.uniform_blocks[0].size
	assert actual_shader.vs.uniform_blocks[0].layout == expected_shader.vs.uniform_blocks[0].layout
	assert actual_shader.fs.uniform_blocks[0].size == expected_shader.fs.uniform_blocks[0].size
	assert actual_shader.fs.uniform_blocks[0].layout == expected_shader.fs.uniform_blocks[0].layout
	backend := gfx.query_backend()
	expected_entry := if backend in [.metal_ios, .metal_macos, .metal_simulator] {
		'_main'
	} else {
		'main'
	}
	assert unsafe { cstring_to_vstring(actual_shader.vs.entry) } == expected_entry
	assert unsafe { cstring_to_vstring(actual_shader.fs.entry) } == expected_entry

	pipeline_desc := gfx.query_pipeline_desc(pipeline)
	expected_pipeline := gfx.query_pipeline_desc_defaults(&pipeline_desc)
	actual_pipeline := gfx.query_pipeline_defaults(&pipeline)
	assert actual_pipeline.shader == expected_pipeline.shader
	assert actual_pipeline.primitive_type == expected_pipeline.primitive_type
	assert actual_pipeline.index_type == expected_pipeline.index_type
	assert actual_pipeline.cull_mode == expected_pipeline.cull_mode
	assert actual_pipeline.face_winding == expected_pipeline.face_winding
	assert actual_pipeline.sample_count == expected_pipeline.sample_count
	assert actual_pipeline.depth.compare == expected_pipeline.depth.compare
	assert actual_pipeline.color_count == expected_pipeline.color_count
	assert actual_pipeline.primitive_type == .triangles
	assert actual_pipeline.index_type == .none
	assert actual_pipeline.cull_mode == .none
	assert actual_pipeline.face_winding == .cw
	assert actual_pipeline.sample_count == 1
	assert actual_pipeline.depth.compare == .always
	assert actual_pipeline.color_count == 1
}

fn multiwindow_fault_matrix_make_attachments(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_make_attachments:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_attachments, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	_ := resources.make_attachments(WindowAttachmentsConfig{}) or {
		invalid_error = err.msg()
		WindowAttachmentsId{}
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	valid := WindowAttachmentsConfig{
		colors: [proof.replace_image]
	}
	mut fault_error := ''
	_ := resources.make_attachments(valid) or {
		fault_error = err.msg()
		WindowAttachmentsId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.replace_attachments = resources.make_attachments(valid)!
	native := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.replace_attachments)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.make_attachments, native, 0)
	retry_registry := multiwindow_record_retry_registry('make_attachments', armed.registry, [
		multiwindow_fault_matrix_key(attachments_resource_key(proof.replace_attachments)),
		multiwindow_fault_matrix_key(image_resource_key(proof.replace_image)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_attachments_retry_slot(armed.registry, retry_registry,
		proof.replace_attachments, native, proof.replace_image, 1)
	proof.rebuild_attachments = resources.make_attachments(WindowAttachmentsConfig{
		colors: [proof.rebuild_image]
	})!
	proof.replace_attachments_second = resources.make_attachments(WindowAttachmentsConfig{
		colors: [proof.replace_image]
	})!
	proof.rebuild_attachments_second = resources.make_attachments(WindowAttachmentsConfig{
		colors: [proof.rebuild_image]
	})!
}

fn multiwindow_fault_matrix_make_sgl_recipes(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	valid := multiwindow_fault_matrix_pipeline_desc()
	message_default := 'fault:resource_make_sgl_recipe:default-api'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_make_sgl_recipe, 0, message_default)!
	armed_default := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_recipe := gfx.PipelineDesc{}
	invalid_recipe.shader = gfx.Shader{
		id: 1
	}
	mut invalid_default := ''
	_ := resources.make_sgl_pipeline(&invalid_recipe) or {
		invalid_default = err.msg()
		WindowSglPipelineId{}
	}
	assert invalid_default == err_multiwindow_render_native_injection
	multiwindow_assert_matrix_checkpoint_equal(armed_default,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	mut fault_default := ''
	_ := resources.make_sgl_pipeline(&valid) or {
		fault_default = err.msg()
		WindowSglPipelineId{}
	}
	assert fault_default == message_default
	multiwindow_assert_matrix_fault_consumed(armed_default,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	trace_before_default_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.sgl_default = resources.make_sgl_pipeline(&valid)!
	multiwindow_assert_typed_trace_equal(trace_before_default_retry,
		multiwindow_sokol_trace.typed_snapshot())
	default_registry := multiwindow_record_retry_registry('make_sgl_pipeline',
		armed_default.registry, [
		multiwindow_fault_matrix_key(sgl_pipeline_resource_key(proof.sgl_default)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_sgl_recipe_retry_slot(armed_default.registry, default_registry,
		proof.sgl_default, WindowShaderId{}, valid)

	message_shader := 'fault:resource_make_sgl_recipe:shader-api'
	app.render_runtime.set_internal_fault(.resource_make_sgl_recipe, 0, message_shader)!
	armed_shader := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_shader := ''
	_ := resources.make_sgl_pipeline_with_shader(&valid, WindowShaderId{}) or {
		invalid_shader = err.msg()
		WindowSglPipelineId{}
	}
	assert invalid_shader == err_multiwindow_render_resource_scope
	multiwindow_assert_matrix_checkpoint_equal(armed_shader,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	mut fault_shader := ''
	_ := resources.make_sgl_pipeline_with_shader(&valid, proof.shader) or {
		fault_shader = err.msg()
		WindowSglPipelineId{}
	}
	assert fault_shader == message_shader
	multiwindow_assert_matrix_fault_consumed(armed_shader,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	trace_before_shader_retry := multiwindow_sokol_trace.typed_snapshot()
	proof.sgl_shader = resources.make_sgl_pipeline_with_shader(&valid, proof.shader)!
	multiwindow_assert_typed_trace_equal(trace_before_shader_retry,
		multiwindow_sokol_trace.typed_snapshot())
	shader_registry := multiwindow_record_retry_registry('make_sgl_pipeline_with_shader',
		armed_shader.registry, [
		multiwindow_fault_matrix_key(sgl_pipeline_resource_key(proof.sgl_shader)),
		multiwindow_fault_matrix_key(shader_resource_key(proof.shader)),
	], 1, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_sgl_recipe_retry_slot(armed_shader.registry, shader_registry,
		proof.sgl_shader, proof.shader, valid)
}

fn multiwindow_fault_matrix_update_buffer(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_update_buffer:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_update_buffer, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	resources.update_buffer(proof.update_buffer, &gfx.Range{}) or { invalid_error = err.msg() }
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	bytes := [u8(1), 2, 3, 4]
	data := gfx.Range{
		ptr:  bytes.data
		size: usize(bytes.len)
	}
	mut fault_error := ''
	resources.update_buffer(proof.update_buffer, &data) or { fault_error = err.msg() }
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	resources.update_buffer(proof.update_buffer, &data)!
	native := multiwindow_fault_matrix_buffer_native(resources.app.render_runtime,
		proof.update_buffer)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.update_buffer, native, i64(bytes.len))
	updated_slot := multiwindow_fault_matrix_slot(resources.app.render_runtime,
		buffer_resource_key(proof.update_buffer), .buffer)!
	assert updated_slot.last_mutation_batch == resources.batch_epoch
	assert updated_slot.buffer_mutation_mode == .update
	retry_registry := multiwindow_record_retry_registry('update_buffer', armed.registry, [
		multiwindow_fault_matrix_key(buffer_resource_key(proof.update_buffer)),
	], 0, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_buffer_retry_slot(retry_registry, proof.update_buffer, native, 64, .dynamic,
		.update, resources.batch_epoch)
}

fn multiwindow_fault_matrix_append_buffer(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_append_buffer:real-operation'
	prime_bytes := []u8{len: 48, init: u8(index)}
	prime_data := gfx.Range{
		ptr:  prime_bytes.data
		size: usize(prime_bytes.len)
	}
	prime_offset := resources.append_buffer(proof.append_buffer, &prime_data)!
	assert prime_offset == 0
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_append_buffer, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	_ := resources.append_buffer(proof.append_buffer, &gfx.Range{}) or {
		invalid_error = err.msg()
		-1
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	overflow_bytes := []u8{len: 20, init: u8(index)}
	overflow_data := gfx.Range{
		ptr:  overflow_bytes.data
		size: usize(overflow_bytes.len)
	}
	mut overflow_error := ''
	_ := resources.append_buffer(proof.append_buffer, &overflow_data) or {
		overflow_error = err.msg()
		-1
	}
	assert overflow_error == err_multiwindow_render_buffer_overflow
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	bytes := [u8(5), 6, 7, 8]
	data := gfx.Range{
		ptr:  bytes.data
		size: usize(bytes.len)
	}
	mut fault_error := ''
	_ := resources.append_buffer(proof.append_buffer, &data) or {
		fault_error = err.msg()
		-1
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	offset := resources.append_buffer(proof.append_buffer, &data)!
	assert offset == prime_bytes.len
	native := multiwindow_fault_matrix_buffer_native(resources.app.render_runtime,
		proof.append_buffer)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.append_buffer, native, i64(offset))
	appended_slot := multiwindow_fault_matrix_slot(resources.app.render_runtime,
		buffer_resource_key(proof.append_buffer), .buffer)!
	assert appended_slot.last_mutation_batch == resources.batch_epoch
	assert appended_slot.buffer_mutation_mode == .append
	retry_registry := multiwindow_record_retry_registry('append_buffer', armed.registry, [
		multiwindow_fault_matrix_key(buffer_resource_key(proof.append_buffer)),
	], 0, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_buffer_retry_slot(retry_registry, proof.append_buffer, native, 64, .stream,
		.append, resources.batch_epoch)
}

fn multiwindow_fault_matrix_update_image(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_update_image:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_update_image, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	resources.update_image(proof.update_image, &gfx.ImageData{}) or { invalid_error = err.msg() }
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	pixels := []u8{len: 64, init: u8(index)}
	mut data := gfx.ImageData{}
	data.subimage[0][0] = gfx.Range{
		ptr:  pixels.data
		size: usize(pixels.len)
	}
	mut fault_error := ''
	resources.update_image(proof.update_image, &data) or { fault_error = err.msg() }
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	resources.update_image(proof.update_image, &data)!
	native := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.update_image)!
	multiwindow_assert_single_trace_delta(before_retry, multiwindow_sokol_trace.typed_snapshot(),
		.update_image, native, 0)
	updated_slot := multiwindow_fault_matrix_slot(resources.app.render_runtime,
		image_resource_key(proof.update_image), .image)!
	assert updated_slot.last_mutation_batch == resources.batch_epoch
	retry_registry := multiwindow_record_retry_registry('update_image', armed.registry, [
		multiwindow_fault_matrix_key(image_resource_key(proof.update_image)),
	], 0, 0, resources.app.render_runtime, mut proof)
	multiwindow_assert_image_retry_slot(retry_registry, proof.update_image, native,
		multiwindow_fault_matrix_dynamic_image_desc(), resources.batch_epoch)
}

fn multiwindow_fault_matrix_replace_image(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_replace_image:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_replace_image, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	invalid := multiwindow_fault_matrix_incompatible_render_target_desc()
	_ := resources.replace_image(proof.replace_image, &invalid) or {
		invalid_error = err.msg()
		WindowImageId{}
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	valid := multiwindow_fault_matrix_render_target_desc()
	old_image := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.replace_image)!
	old_attachments := [
		multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
			proof.replace_attachments)!,
		multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
			proof.replace_attachments_second)!,
	]
	mut fault_error := ''
	_ := resources.replace_image(proof.replace_image, &valid) or {
		fault_error = err.msg()
		WindowImageId{}
	}
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	retried := resources.replace_image(proof.replace_image, &valid)!
	assert retried == proof.replace_image
	after_retry := multiwindow_sokol_trace.typed_snapshot()
	new_image := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.replace_image)!
	new_attachments := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.replace_attachments)!
	new_attachments_second := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.replace_attachments_second)!
	assert new_image != old_image
	assert new_attachments != old_attachments[0]
	assert new_attachments_second != old_attachments[1]
	multiwindow_assert_trace_delta(before_retry, after_retry, [
		MultiWindowMatrixDestroyExpectation{ operation: .make_image, identity: new_image },
		MultiWindowMatrixDestroyExpectation{ operation: .make_attachments, identity: new_attachments },
		MultiWindowMatrixDestroyExpectation{
			operation: .make_attachments
			identity:  new_attachments_second
		},
	])
	retry_registry := multiwindow_record_retry_registry('replace_image', armed.registry, [
		multiwindow_fault_matrix_key(image_resource_key(proof.replace_image)),
		multiwindow_fault_matrix_key(attachments_resource_key(proof.replace_attachments)),
		multiwindow_fault_matrix_key(attachments_resource_key(proof.replace_attachments_second)),
	], 0, 3, resources.app.render_runtime, mut proof)
	multiwindow_assert_replacement_registry(armed.registry, retry_registry, proof.replace_image, [
		proof.replace_attachments,
		proof.replace_attachments_second,
	], old_image, old_attachments, new_image, [new_attachments, new_attachments_second], valid,
		resources.batch_epoch)
	old_destroys := [
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  old_attachments[0]
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  old_attachments[1]
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_image
			identity:  old_image
		},
	]
	old_images, old_attachment_handles := multiwindow_assert_replacement_live_before_commit(resources.app.render_runtime,
		after_retry, old_destroys)
	proof.replacement_old_destroys << old_destroys
	proof.replacement_old_images << old_images
	proof.replacement_old_attachments_native << old_attachment_handles
}

fn multiwindow_fault_matrix_rebuild_attachment(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_rebuild_attachment:real-operation'
	mut app := resources.app
	app.render_runtime.set_internal_fault(.resource_rebuild_attachment, 1, message)!
	armed := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut invalid_error := ''
	invalid := multiwindow_fault_matrix_incompatible_render_target_desc()
	_ := resources.replace_image(proof.rebuild_image, &invalid) or {
		invalid_error = err.msg()
		WindowImageId{}
	}
	assert invalid_error == err_multiwindow_render_invalid_descriptor
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	valid := multiwindow_fault_matrix_render_target_desc()
	old_image := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.rebuild_image)!
	old_attachments := [
		multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
			proof.rebuild_attachments)!,
		multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
			proof.rebuild_attachments_second)!,
	]
	trace_before_fault := multiwindow_sokol_trace.typed_snapshot()
	mut fault_error := ''
	_ := resources.replace_image(proof.rebuild_image, &valid) or {
		fault_error = err.msg()
		WindowImageId{}
	}
	assert fault_error == message
	after_fault := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	assert after_fault.plan == MultiWindowMatrixPlanState{}
	assert after_fault.registry == armed.registry
	assert !trace_before_fault.overflow
	assert !after_fault.trace.overflow
	assert trace_before_fault.install_generation != 0
	assert after_fault.trace.install_generation == trace_before_fault.install_generation
	assert after_fault.trace.count == trace_before_fault.count + 4
	created_image := after_fault.trace.records[int(trace_before_fault.count)]
	created_attachment := after_fault.trace.records[int(trace_before_fault.count) + 1]
	destroyed_attachment := after_fault.trace.records[int(trace_before_fault.count) + 2]
	destroyed_image := after_fault.trace.records[int(trace_before_fault.count) + 3]
	assert created_image.sequence == multiwindow_fault_matrix_next_trace_sequence(trace_before_fault)
	assert created_attachment.sequence == created_image.sequence + 1
	assert destroyed_attachment.sequence == created_attachment.sequence + 1
	assert destroyed_image.sequence == destroyed_attachment.sequence + 1
	assert created_image.operation == .make_image
	assert created_image.identity != 0
	assert created_attachment.operation == .make_attachments
	assert created_attachment.identity != 0
	assert destroyed_attachment.operation == .destroy_attachments
	assert destroyed_attachment.identity == created_attachment.identity
	assert destroyed_image.operation == .destroy_image
	assert destroyed_image.identity == created_image.identity
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	retried := resources.replace_image(proof.rebuild_image, &valid)!
	assert retried == proof.rebuild_image
	after_retry := multiwindow_sokol_trace.typed_snapshot()
	new_image := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.rebuild_image)!
	new_attachments := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.rebuild_attachments)!
	new_attachments_second := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.rebuild_attachments_second)!
	assert new_image != old_image
	assert new_attachments != old_attachments[0]
	assert new_attachments_second != old_attachments[1]
	multiwindow_assert_trace_delta(before_retry, after_retry, [
		MultiWindowMatrixDestroyExpectation{ operation: .make_image, identity: new_image },
		MultiWindowMatrixDestroyExpectation{ operation: .make_attachments, identity: new_attachments },
		MultiWindowMatrixDestroyExpectation{
			operation: .make_attachments
			identity:  new_attachments_second
		},
	])
	retry_registry := multiwindow_record_retry_registry('rebuild_attachment', armed.registry, [
		multiwindow_fault_matrix_key(image_resource_key(proof.rebuild_image)),
		multiwindow_fault_matrix_key(attachments_resource_key(proof.rebuild_attachments)),
		multiwindow_fault_matrix_key(attachments_resource_key(proof.rebuild_attachments_second)),
	], 0, 3, resources.app.render_runtime, mut proof)
	multiwindow_assert_replacement_registry(armed.registry, retry_registry, proof.rebuild_image, [
		proof.rebuild_attachments,
		proof.rebuild_attachments_second,
	], old_image, old_attachments, new_image, [new_attachments, new_attachments_second], valid,
		resources.batch_epoch)
	old_destroys := [
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  old_attachments[0]
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  old_attachments[1]
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_image
			identity:  old_image
		},
	]
	old_images, old_attachment_handles := multiwindow_assert_replacement_live_before_commit(resources.app.render_runtime,
		after_retry, old_destroys)
	proof.replacement_old_destroys << old_destroys
	proof.replacement_old_images << old_images
	proof.replacement_old_attachments_native << old_attachment_handles
	proof.replacement_post_retry = after_retry
}

fn multiwindow_fault_matrix_window_frame(window_index int, mut context WindowContext, mut proof MultiWindowRealOperationProof) ! {
	if proof.retired {
		return
	}
	match window_index {
		0 {
			if proof.materialized_default != 0 || proof.materialized_shader {
				return
			}
			context.with_swapchain_sgl(gfx.PassAction{}, fn [mut proof] (mut drawing WindowSglContext) ! {
				multiwindow_fault_matrix_materialize_recipe(mut drawing, proof.sgl_default,
					'default-target-a', false, mut proof)!
				multiwindow_fault_matrix_materialize_recipe(mut drawing, proof.sgl_shader,
					'shader-target-a', true, mut proof)!
			})!
			proof.window_frames++
		}
		1 {
			if proof.materialized_default != 1 || !proof.materialized_shader {
				return
			}
			context.with_swapchain_sgl(gfx.PassAction{}, fn [mut proof] (mut drawing WindowSglContext) ! {
				multiwindow_fault_matrix_materialize_recipe(mut drawing, proof.sgl_default,
					'default-target-b', false, mut proof)!
			})!
			mut app := context.app
			app.core.set_render_workload(proof.windows[0].core, false)!
			app.core.set_render_workload(proof.windows[1].core, false)!
			proof.window_frames++
		}
		else {
			return error('fault matrix received an unexpected window frame index')
		}
	}
}

fn multiwindow_fault_matrix_materialize_recipe(mut drawing WindowSglContext, id WindowSglPipelineId, label string, shader_backed bool, mut proof MultiWindowRealOperationProof) ! {
	message := 'fault:resource_sgl_materialization:${label}'
	mut app := drawing.app
	app.render_runtime.set_internal_fault(.resource_sgl_materialization, 0, message)!
	armed := multiwindow_fault_matrix_checkpoint(drawing.app.render_runtime)
	load_armed := multiwindow_sgl_load_proof_snapshot_for_test(proof.sgl_load_generation)!
	mut invalid_error := ''
	drawing.load_pipeline(WindowSglPipelineId{}) or { invalid_error = err.msg() }
	assert invalid_error == err_multiwindow_render_resource_scope
	multiwindow_assert_matrix_checkpoint_equal(armed,
		multiwindow_fault_matrix_checkpoint(drawing.app.render_runtime))
	load_after_invalid := multiwindow_sgl_load_proof_snapshot_for_test(proof.sgl_load_generation)!
	assert load_after_invalid == load_armed
	mut fault_error := ''
	drawing.load_pipeline(id) or { fault_error = err.msg() }
	assert fault_error == message
	multiwindow_assert_matrix_fault_consumed(armed,
		multiwindow_fault_matrix_checkpoint(drawing.app.render_runtime))
	load_after_fault := multiwindow_sgl_load_proof_snapshot_for_test(proof.sgl_load_generation)!
	assert load_after_fault == load_armed
	before_retry := multiwindow_sokol_trace.typed_snapshot()
	drawing.load_pipeline(id)!
	after_retry := multiwindow_sokol_trace.typed_snapshot()
	assert !before_retry.overflow
	assert !after_retry.overflow
	assert before_retry.install_generation != 0
	assert after_retry.install_generation == before_retry.install_generation
	assert after_retry.count == before_retry.count + 5
	mut identities := []u32{cap: 5}
	for index in 0 .. 5 {
		record := after_retry.records[int(before_retry.count) + index]
		assert record.sequence == multiwindow_fault_matrix_next_trace_sequence(before_retry) +
			u64(index)
		assert record.operation == .make_pipeline
		assert record.identity != 0
		assert record.identity !in identities
		identities << record.identity
	}
	load_after := multiwindow_sgl_load_proof_snapshot_for_test(proof.sgl_load_generation)!
	observed_load := multiwindow_assert_sgl_load_proof_delta(load_armed, load_after,
		drawing.window, drawing.target_key)
	assert multiwindow_sokol_trace.typed_snapshot() == after_retry
	retry_registry := multiwindow_record_retry_registry('materialize_${label}', armed.registry, [
		multiwindow_fault_matrix_key(sgl_pipeline_resource_key(id)),
	], 0, 0, drawing.app.render_runtime, mut proof)
	multiwindow_assert_sgl_materialization_retry_slot(armed.registry, retry_registry, id,
		drawing.target_key, observed_load.pipeline_id, identities)
	if shader_backed {
		proof.materialized_shader = true
		proof.shader_target_key = drawing.target_key
		proof.shader_sgl_native = identities
	} else {
		proof.materialized_default++
		proof.default_target_keys << drawing.target_key
		proof.default_sgl_native << identities
	}
}

fn multiwindow_assert_sgl_load_proof_delta(before MultiWindowSglLoadProofSnapshot, after MultiWindowSglLoadProofSnapshot, window WindowId, target_key string) MultiWindowSglLoadProofRecord {
	assert before.generation != 0
	assert after.generation == before.generation
	assert before.armed
	assert after.armed
	assert !before.overflow
	assert !after.overflow
	assert target_key != ''
	assert after.records.len == before.records.len + 1
	for index in 0 .. before.records.len {
		assert after.records[index] == before.records[index]
	}
	record := after.records[before.records.len]
	assert record.generation == before.generation
	assert record.sequence == u64(before.records.len + 1)
	assert record.pipeline_id != 0
	assert record.window == window
	assert record.context_id != 0
	assert record.target_key == target_key
	return record
}

fn multiwindow_fault_matrix_retire(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	proof.frame_calls++
	if proof.materialized_default != 2 || !proof.materialized_shader {
		return
	}
	if !proof.retired {
		multiwindow_fault_matrix_retire_all(mut resources, mut proof)!
		proof.retired = true
		return
	}
	if proof.retirement_verified {
		return error('fault matrix app resource frame ran after deferred stop')
	}
	proof.retirement_after = multiwindow_sokol_trace.typed_snapshot()
	multiwindow_assert_retirement_trace(proof.retirement_before, proof.retirement_after,
		proof.destroy_expectations)
	proof.retirement_verified = true
	mut facade := resources.app
	facade.stop()!
}

fn multiwindow_fault_matrix_retire_all(mut resources AppResourceContext, mut proof MultiWindowRealOperationProof) ! {
	postcommit_trace := multiwindow_sokol_trace.typed_snapshot()
	multiwindow_assert_postcommit_replacement_destruction(proof.replacement_post_retry,
		postcommit_trace, proof.replacement_old_destroys, proof.replacement_old_images,
		proof.replacement_old_attachments_native, proof.default_sgl_native, proof.shader_sgl_native)
	proof.replacement_postcommit_verified = true
	dependency_before := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut image_dependency_error := ''
	resources.retire_image(proof.replace_image) or { image_dependency_error = err.msg() }
	assert image_dependency_error == err_multiwindow_render_resource_has_dependents
	multiwindow_assert_matrix_checkpoint_equal(dependency_before,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	mut shader_dependency_error := ''
	resources.retire_shader(proof.shader) or { shader_dependency_error = err.msg() }
	assert shader_dependency_error == err_multiwindow_render_resource_has_dependents
	multiwindow_assert_matrix_checkpoint_equal(dependency_before,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))

	attachments_replace_native := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.replace_attachments)!
	attachments_rebuild_native := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.rebuild_attachments)!
	attachments_replace_second_native := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.replace_attachments_second)!
	attachments_rebuild_second_native := multiwindow_fault_matrix_attachments_native(resources.app.render_runtime,
		proof.rebuild_attachments_second)!
	pipeline_native := multiwindow_fault_matrix_pipeline_native(resources.app.render_runtime,
		proof.pipeline)!
	shader_native := multiwindow_fault_matrix_shader_native(resources.app.render_runtime,
		proof.shader)!
	update_buffer_native := multiwindow_fault_matrix_buffer_native(resources.app.render_runtime,
		proof.update_buffer)!
	append_buffer_native := multiwindow_fault_matrix_buffer_native(resources.app.render_runtime,
		proof.append_buffer)!
	sampler_native := multiwindow_fault_matrix_sampler_native(resources.app.render_runtime,
		proof.sampler)!
	update_image_native := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.update_image)!
	replace_image_native := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.replace_image)!
	rebuild_image_native := multiwindow_fault_matrix_image_native(resources.app.render_runtime,
		proof.rebuild_image)!
	proof.retirement_before = multiwindow_sokol_trace.typed_snapshot()

	resources.retire_attachments(proof.replace_attachments)!
	resources.retire_attachments(proof.replace_attachments_second)!
	resources.retire_attachments(proof.rebuild_attachments)!
	resources.retire_attachments(proof.rebuild_attachments_second)!
	resources.retire_pipeline(proof.pipeline)!
	resources.retire_sgl_pipeline(proof.sgl_default)!
	resources.retire_sgl_pipeline(proof.sgl_shader)!
	resources.retire_shader(proof.shader)!
	resources.retire_buffer(proof.update_buffer)!
	stale_before := multiwindow_fault_matrix_checkpoint(resources.app.render_runtime)
	mut stale_error := ''
	resources.retire_buffer(proof.update_buffer) or { stale_error = err.msg() }
	assert stale_error == err_multiwindow_render_stale_resource
	multiwindow_assert_matrix_checkpoint_equal(stale_before,
		multiwindow_fault_matrix_checkpoint(resources.app.render_runtime))
	resources.retire_buffer(proof.append_buffer)!
	resources.retire_sampler(proof.sampler)!
	resources.retire_image(proof.update_image)!
	resources.retire_image(proof.replace_image)!
	resources.retire_image(proof.rebuild_image)!
	multiwindow_assert_typed_trace_equal(proof.retirement_before,
		multiwindow_sokol_trace.typed_snapshot())
	mut destroy_expectations := [
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  attachments_replace_native
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  attachments_replace_second_native
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  attachments_rebuild_native
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_attachments
			identity:  attachments_rebuild_second_native
		},
		MultiWindowMatrixDestroyExpectation{
			operation: .destroy_pipeline
			identity:  pipeline_native
		},
	]
	for materialization in proof.default_sgl_native {
		for identity in materialization {
			destroy_expectations << MultiWindowMatrixDestroyExpectation{
				operation: .destroy_pipeline
				identity:  identity
			}
		}
	}
	for identity in proof.shader_sgl_native {
		destroy_expectations << MultiWindowMatrixDestroyExpectation{
			operation: .destroy_pipeline
			identity:  identity
		}
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_shader
		identity:  shader_native
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_buffer
		identity:  update_buffer_native
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_buffer
		identity:  append_buffer_native
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_sampler
		identity:  sampler_native
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_image
		identity:  update_image_native
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_image
		identity:  replace_image_native
	}
	destroy_expectations << MultiWindowMatrixDestroyExpectation{
		operation: .destroy_image
		identity:  rebuild_image_native
	}
	proof.destroy_expectations = destroy_expectations
}

fn multiwindow_assert_replacement_live_before_commit(runtime &MultiWindowRenderRuntime, trace multiwindow_sokol_trace.TypedSnapshot, expected []MultiWindowMatrixDestroyExpectation) ([]gfx.Image, []gfx.Attachments) {
	assert expected.len == 3
	assert !trace.overflow
	for record in trace.records {
		for item in expected {
			assert record.operation != item.operation || record.identity != item.identity
		}
	}
	mut images := []gfx.Image{}
	mut attachments := []gfx.Attachments{}
	runtime.mutex.lock()
	for item in expected {
		mut matches := 0
		for retired in runtime.resources.deferred {
			match item.operation {
				.destroy_image {
					if retired.image.id == item.identity {
						images << retired.image
						matches++
					}
				}
				.destroy_attachments {
					if retired.attachments.id == item.identity {
						attachments << retired.attachments
						matches++
					}
				}
				else {
					assert false, 'replacement live proof received a non-replacement destroy kind'
				}
			}
		}
		assert matches == 1
	}
	runtime.mutex.unlock()
	assert images.len == 1
	assert attachments.len == 2
	for image in images {
		assert gfx.query_image_state(image) == .valid
	}
	for item in attachments {
		assert gfx.query_attachments_state(item) == .valid
	}
	return images, attachments
}

fn multiwindow_assert_postcommit_replacement_destruction(before multiwindow_sokol_trace.TypedSnapshot, after multiwindow_sokol_trace.TypedSnapshot, expected []MultiWindowMatrixDestroyExpectation, old_images []gfx.Image, old_attachments []gfx.Attachments, default_recipes [][]u32, shader_recipe []u32) {
	assert expected.len == 6
	assert old_images.len == 2
	assert old_attachments.len == 4
	assert default_recipes.len == 2
	assert default_recipes[0].len == 5
	assert shader_recipe.len == 5
	assert default_recipes[1].len == 5
	recipe_bundles := [default_recipes[0], shader_recipe, default_recipes[1]]
	assert !before.overflow
	assert !after.overflow
	assert after.install_generation == before.install_generation
	assert after.count >= before.count + usize(expected.len + 1)
	for record in before.records {
		for item in expected {
			assert record.operation != item.operation || record.identity != item.identity
		}
	}
	mut commit_index := -1
	for index in int(before.count) .. int(after.count) {
		if after.records[index].operation == .commit {
			commit_index = index
			break
		}
	}
	assert commit_index >= int(before.count)
	commit := after.records[commit_index]
	for index, item in expected {
		mut matches := 0
		mut matched_index := -1
		for record_index, record in after.records {
			if record.operation == item.operation && record.identity == item.identity {
				matches++
				matched_index = record_index
			}
		}
		assert matches == 1
		assert matched_index == commit_index + index + 1
		record := after.records[matched_index]
		assert record.operation == item.operation
		assert record.identity == item.identity
		assert record.sequence == commit.sequence + u64(index + 1)
	}
	mut recipe_index := 0
	mut context_buffers := []u32{cap: 2}
	recipe_index, context_buffers = multiwindow_assert_postcommit_submission_batches(after,
		int(before.count), commit_index + 1, recipe_bundles, recipe_index, context_buffers)
	recipe_index, context_buffers = multiwindow_assert_postcommit_submission_batches(after,

		commit_index + expected.len + 1, int(after.count), recipe_bundles, recipe_index,
		context_buffers)
	assert recipe_index == recipe_bundles.len
	assert context_buffers.len == 2
	for image in old_images {
		assert gfx.query_image_state(image) == .invalid
	}
	for item in old_attachments {
		assert gfx.query_attachments_state(item) == .invalid
	}
}

fn multiwindow_assert_postcommit_submission_batches(snapshot multiwindow_sokol_trace.TypedSnapshot, start int, end int, recipe_bundles [][]u32, first_recipe int, previous_context_buffers []u32) (int, []u32) {
	assert start >= 0
	assert start <= end
	assert end <= int(snapshot.count)
	assert first_recipe != 1
	assert previous_context_buffers.len <= 2
	for index, identity in previous_context_buffers {
		assert identity != 0
		for previous_index in 0 .. index {
			assert identity != previous_context_buffers[previous_index]
		}
	}
	mut context_buffers := previous_context_buffers.clone()
	mut record_index := start
	mut recipe_index := first_recipe
	for record_index < end {
		batch_recipe_start := recipe_index
		mut pass_count := 0
		mut committed := false
		for !committed {
			for record_index < end {
				operation := snapshot.records[record_index].operation
				if operation == .make_buffer {
					context_buffer := snapshot.records[record_index].identity
					assert context_buffer != 0
					assert context_buffer !in context_buffers
					assert context_buffers.len < 2
					record_index = multiwindow_assert_postcommit_context_bundle(snapshot,
						record_index, end)
					context_buffers << context_buffer
					continue
				}
				if operation == .make_pipeline {
					assert recipe_index < recipe_bundles.len
					record_index = multiwindow_assert_postcommit_recipe_bundle(snapshot,
						record_index, end, recipe_bundles[recipe_index])
					recipe_index++
					if recipe_index == 1 {
						assert record_index < end
						assert snapshot.records[record_index].operation == .make_pipeline
						record_index = multiwindow_assert_postcommit_recipe_bundle(snapshot,
							record_index, end, recipe_bundles[recipe_index])
						recipe_index++
					}
					break
				}
				break
			}
			mut segment_pass_count := 0
			for record_index < end
				&& snapshot.records[record_index].operation == .begin_swapchain_pass {
				assert record_index + 1 < end
				end_pass := snapshot.records[record_index + 1]
				assert end_pass.operation == .end_pass
				multiwindow_assert_postcommit_sequence(snapshot, record_index)
				multiwindow_assert_postcommit_sequence(snapshot, record_index + 1)
				record_index += 2
				segment_pass_count++
			}
			assert segment_pass_count > 0
			pass_count += segment_pass_count
			assert record_index < end
			next_operation := snapshot.records[record_index].operation
			if next_operation == .commit {
				multiwindow_assert_postcommit_sequence(snapshot, record_index)
				record_index++
				committed = true
				continue
			}
			assert next_operation in [.make_buffer, .make_pipeline]
		}
		batch_recipe_count := recipe_index - batch_recipe_start
		if batch_recipe_count == 0 {
			assert pass_count == 1
		} else if batch_recipe_start == 0 {
			assert batch_recipe_count >= 2
		} else {
			assert batch_recipe_start == 2
			assert batch_recipe_count == 1
		}
	}
	assert record_index == end
	return recipe_index, context_buffers
}

fn multiwindow_assert_postcommit_context_bundle(snapshot multiwindow_sokol_trace.TypedSnapshot, start int, end int) int {
	assert start + 6 <= end
	buffer := snapshot.records[start]
	assert buffer.operation == .make_buffer
	assert buffer.identity != 0
	multiwindow_assert_postcommit_sequence(snapshot, start)
	for offset in 1 .. 6 {
		pipeline := snapshot.records[start + offset]
		assert pipeline.operation == .make_pipeline
		assert pipeline.identity != 0
		multiwindow_assert_postcommit_sequence(snapshot, start + offset)
		for previous_offset in 1 .. offset {
			assert pipeline.identity != snapshot.records[start + previous_offset].identity
		}
	}
	return start + 6
}

fn multiwindow_assert_postcommit_recipe_bundle(snapshot multiwindow_sokol_trace.TypedSnapshot, start int, end int, expected []u32) int {
	assert expected.len == 5
	assert start + expected.len <= end
	for offset, identity in expected {
		assert identity != 0
		for previous_offset in 0 .. offset {
			assert identity != expected[previous_offset]
		}
		record := snapshot.records[start + offset]
		assert record.operation == .make_pipeline
		assert record.identity == identity
		multiwindow_assert_postcommit_sequence(snapshot, start + offset)
	}
	return start + expected.len
}

fn multiwindow_assert_postcommit_sequence(snapshot multiwindow_sokol_trace.TypedSnapshot, index int) {
	assert index > 0
	assert index < int(snapshot.count)
	assert snapshot.records[index].sequence == snapshot.records[index - 1].sequence + 1
}

fn multiwindow_assert_retirement_trace(before multiwindow_sokol_trace.TypedSnapshot, after multiwindow_sokol_trace.TypedSnapshot, expected []MultiWindowMatrixDestroyExpectation) {
	assert !before.overflow
	assert !after.overflow
	assert before.install_generation != 0
	assert after.install_generation == before.install_generation
	assert after.count == before.count + usize(expected.len + 3)
	anchor_begin := after.records[int(before.count)]
	anchor_end := after.records[int(before.count) + 1]
	commit := after.records[int(before.count) + 2]
	assert anchor_begin.operation == .begin_swapchain_pass
	assert anchor_begin.width > 0
	assert anchor_begin.height > 0
	assert anchor_end.operation == .end_pass
	assert commit.operation == .commit
	assert commit.identity == 0
	assert anchor_begin.sequence == multiwindow_fault_matrix_next_trace_sequence(before)
	assert anchor_end.sequence == anchor_begin.sequence + 1
	assert commit.sequence == anchor_end.sequence + 1
	for index, item in expected {
		record := after.records[int(before.count) + index + 3]
		assert record.operation == item.operation
		assert record.identity == item.identity
		assert record.identity != 0
		assert record.sequence == commit.sequence + u64(index + 1)
	}
}

fn multiwindow_fault_matrix_checkpoint(runtime &MultiWindowRenderRuntime) MultiWindowMatrixCheckpoint {
	runtime.mutex.lock()
	plan := MultiWindowMatrixPlanState{
		stage:               runtime.internal_fault.stage
		hits_before_failure: runtime.internal_fault.hits_before_failure
		message:             runtime.internal_fault.message
	}
	registry := multiwindow_fault_matrix_registry_locked(runtime.resources)
	runtime.mutex.unlock()
	return MultiWindowMatrixCheckpoint{
		plan:     plan
		registry: registry
		trace:    multiwindow_sokol_trace.typed_snapshot()
	}
}

fn multiwindow_record_retry_registry(label string, before MultiWindowMatrixRegistryState, changed_keys []string, new_slots int, deferred_added int, runtime &MultiWindowRenderRuntime, mut proof MultiWindowRealOperationProof) MultiWindowMatrixRegistryState {
	checkpoint := multiwindow_fault_matrix_checkpoint(runtime)
	assert checkpoint.plan == MultiWindowMatrixPlanState{}
	multiwindow_assert_exact_registry_delta(before, checkpoint.registry, changed_keys, new_slots,
		deferred_added)
	multiwindow_assert_registry_graph(checkpoint.registry)
	proof.retry_labels << label
	proof.retry_registry << checkpoint.registry
	return checkpoint.registry
}

fn multiwindow_assert_exact_registry_delta(before MultiWindowMatrixRegistryState, after MultiWindowMatrixRegistryState, changed_keys []string, new_slots int, deferred_added int) {
	assert before.app_instance == after.app_instance
	assert after.slots.len == before.slots.len + new_slots
	assert after.deferred.len == before.deferred.len + deferred_added
	mut unique_changed := []string{}
	for key in changed_keys {
		assert key != ''
		assert key !in unique_changed
		unique_changed << key
	}
	for before_slot in before.slots {
		after_slot := multiwindow_fault_matrix_registry_slot(after, before_slot.key)
		if before_slot.key !in changed_keys {
			assert after_slot == before_slot
		}
	}
	mut actual_new := 0
	for after_slot in after.slots {
		mut existed := false
		for before_slot in before.slots {
			if before_slot.key == after_slot.key {
				existed = true
				break
			}
		}
		if !existed {
			actual_new++
			assert after_slot.key in changed_keys
		}
	}
	assert actual_new == new_slots
	for index in 0 .. before.deferred.len {
		assert after.deferred[index] == before.deferred[index]
	}
}

fn multiwindow_assert_buffer_retry_slot(registry MultiWindowMatrixRegistryState, id WindowBufferId, native u32, capacity usize, usage gfx.Usage, mutation MultiWindowBufferMutationMode, mutation_batch u64) {
	slot := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(buffer_resource_key(id)))
	mut desc := gfx.BufferDesc{
		size:  capacity
		usage: usage
	}
	effective := gfx.query_buffer_desc_defaults(&desc)
	expected_desc := pointer_free_buffer_desc(effective)
	assert slot.kind == .buffer
	assert slot.status == .alive
	assert slot.app_scoped
	assert slot.dependencies.len == 0
	assert slot.dependents.len == 0
	assert slot.buffer_id == native
	assert slot.buffer_capacity == effective.size
	assert slot.buffer_usage == effective.usage
	multiwindow_assert_desc_semantically_equal(slot.buffer_desc, &expected_desc)
	assert slot.label == ''
	assert slot.last_mutation_batch == mutation_batch
	assert slot.buffer_mutation_mode == mutation
	assert slot.image_id == 0 && slot.sampler_id == 0 && slot.shader_id == 0
	assert slot.pipeline_id == 0 && slot.attachments_id == 0
}

fn multiwindow_assert_image_retry_slot(registry MultiWindowMatrixRegistryState, id WindowImageId, native u32, desc gfx.ImageDesc, mutation_batch u64) {
	slot := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(image_resource_key(id)))
	effective := gfx.query_image_desc_defaults(&desc)
	expected_desc := pointer_free_image_desc(effective)
	assert slot.kind == .image
	assert slot.status == .alive
	assert slot.app_scoped
	assert slot.dependencies.len == 0
	assert slot.dependents.len == 0
	assert slot.image_id == native
	multiwindow_assert_desc_semantically_equal(slot.image_desc, &expected_desc)
	assert slot.image_width == effective.width
	assert slot.image_height == effective.height
	assert slot.image_sample_count == effective.sample_count
	assert slot.image_pixel_format == effective.pixel_format
	assert slot.image_usage == effective.usage
	assert slot.image_render_target == effective.render_target
	assert slot.last_mutation_batch == mutation_batch
	assert slot.buffer_id == 0 && slot.sampler_id == 0 && slot.shader_id == 0
	assert slot.pipeline_id == 0 && slot.attachments_id == 0
}

fn multiwindow_assert_sampler_retry_slot(registry MultiWindowMatrixRegistryState, id WindowSamplerId, native u32, desc gfx.SamplerDesc) {
	slot := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(sampler_resource_key(id)))
	effective := gfx.query_sampler_defaults(&desc)
	expected_desc := pointer_free_sampler_desc(effective)
	expected_snapshot := multiwindow_fault_matrix_sampler_desc_snapshot(expected_desc)
	assert slot.kind == .sampler
	assert slot.status == .alive
	assert slot.app_scoped
	assert slot.dependencies.len == 0
	assert slot.dependents.len == 0
	assert slot.sampler_id == native
	assert slot.sampler_desc == expected_snapshot
	multiwindow_assert_sampler_desc_snapshot_pointer_free(slot.sampler_desc)
	assert slot.buffer_id == 0 && slot.image_id == 0 && slot.shader_id == 0
	assert slot.pipeline_id == 0 && slot.attachments_id == 0
}

fn multiwindow_assert_shader_retry_slot(registry MultiWindowMatrixRegistryState, id WindowShaderId, native u32, desc gfx.ShaderDesc) {
	slot := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(shader_resource_key(id)))
	effective := gfx.query_shader_desc_defaults(&desc)
	expected_desc := pointer_free_shader_desc(effective)
	assert slot.kind == .shader
	assert slot.status == .alive
	assert slot.app_scoped
	assert slot.dependencies.len == 0
	assert slot.dependents.len == 0
	assert slot.shader_id == native
	multiwindow_assert_desc_semantically_equal(slot.shader_desc, &expected_desc)
	assert slot.buffer_id == 0 && slot.image_id == 0 && slot.sampler_id == 0
	assert slot.pipeline_id == 0 && slot.attachments_id == 0
}

fn multiwindow_assert_pipeline_retry_slot(before MultiWindowMatrixRegistryState, registry MultiWindowMatrixRegistryState, id WindowPipelineId, native u32, shader WindowShaderId, desc gfx.PipelineDesc) {
	pipeline_key := multiwindow_fault_matrix_key(pipeline_resource_key(id))
	shader_key := multiwindow_fault_matrix_key(shader_resource_key(shader))
	slot := multiwindow_fault_matrix_registry_slot(registry, pipeline_key)
	shader_slot := multiwindow_fault_matrix_registry_slot(registry, shader_key)
	before_shader := multiwindow_fault_matrix_registry_slot(before, shader_key)
	mut input := desc
	input.shader = gfx.Shader{
		id: shader_slot.shader_id
	}
	effective := gfx.query_pipeline_desc_defaults(&input)
	expected_desc := pointer_free_pipeline_desc(effective)
	assert slot.kind == .pipeline
	assert slot.status == .alive
	assert slot.app_scoped
	assert slot.dependencies == [shader_key]
	assert slot.dependents.len == 0
	assert slot.pipeline_id == native
	multiwindow_assert_desc_semantically_equal(slot.pipeline_desc, &expected_desc)
	mut expected_dependents := before_shader.dependents.clone()
	expected_dependents << pipeline_key
	expected_shader := MultiWindowMatrixSlotState{
		...before_shader
		dependents: expected_dependents
	}
	assert shader_slot == expected_shader
	assert slot.buffer_id == 0 && slot.image_id == 0 && slot.sampler_id == 0
	assert slot.shader_id == 0 && slot.attachments_id == 0
}

fn multiwindow_assert_attachments_retry_slot(before MultiWindowMatrixRegistryState, registry MultiWindowMatrixRegistryState, id WindowAttachmentsId, native u32, image WindowImageId, target_identity u64) {
	attachments_key := multiwindow_fault_matrix_key(attachments_resource_key(id))
	image_key := multiwindow_fault_matrix_key(image_resource_key(image))
	slot := multiwindow_fault_matrix_registry_slot(registry, attachments_key)
	image_slot := multiwindow_fault_matrix_registry_slot(registry, image_key)
	before_image := multiwindow_fault_matrix_registry_slot(before, image_key)
	assert slot.kind == .attachments
	assert slot.status == .alive
	assert slot.app_scoped
	assert slot.dependencies == [image_key]
	assert slot.dependents.len == 0
	assert slot.attachments_id == native
	assert slot.target_identity == target_identity
	assert slot.attachment_colors == [image_key]
	assert slot.attachment_resolves.len == 0
	assert slot.attachment_depth == ''
	mut expected_dependents := before_image.dependents.clone()
	expected_dependents << attachments_key
	expected_image := MultiWindowMatrixSlotState{
		...before_image
		dependents: expected_dependents
	}
	assert image_slot == expected_image
	assert slot.buffer_id == 0 && slot.image_id == 0 && slot.sampler_id == 0
	assert slot.shader_id == 0 && slot.pipeline_id == 0
}

fn multiwindow_assert_sgl_recipe_retry_slot(before MultiWindowMatrixRegistryState, registry MultiWindowMatrixRegistryState, id WindowSglPipelineId, shader WindowShaderId, desc gfx.PipelineDesc) {
	recipe_key := multiwindow_fault_matrix_key(sgl_pipeline_resource_key(id))
	slot := multiwindow_fault_matrix_registry_slot(registry, recipe_key)
	assert slot.kind == .sgl_pipeline
	assert slot.status == .alive
	assert slot.app_scoped
	multiwindow_assert_desc_semantically_equal(slot.sgl_recipe, &desc)
	assert slot.sgl_materializations.len == 0
	assert slot.pipeline_id == 0
	if shader.app_instance == 0 {
		assert slot.dependencies.len == 0
		assert slot.sgl_shader == ''
	} else {
		shader_key := multiwindow_fault_matrix_key(shader_resource_key(shader))
		shader_slot := multiwindow_fault_matrix_registry_slot(registry, shader_key)
		before_shader := multiwindow_fault_matrix_registry_slot(before, shader_key)
		assert slot.dependencies == [shader_key]
		assert slot.sgl_shader == shader_key
		assert recipe_key !in before_shader.dependents
		mut expected_dependents := before_shader.dependents.clone()
		expected_dependents << recipe_key
		expected_shader := MultiWindowMatrixSlotState{
			...before_shader
			dependents: expected_dependents
		}
		assert shader_slot == expected_shader
	}
}

fn multiwindow_assert_sgl_materialization_retry_slot(before MultiWindowMatrixRegistryState, after MultiWindowMatrixRegistryState, id WindowSglPipelineId, target_key string, expected_pipeline u32, native_pipelines []u32) {
	assert target_key != ''
	assert expected_pipeline != 0
	assert native_pipelines.len == 5
	key := multiwindow_fault_matrix_key(sgl_pipeline_resource_key(id))
	before_slot := multiwindow_fault_matrix_registry_slot(before, key)
	slot := multiwindow_fault_matrix_registry_slot(after, key)
	assert slot.kind == .sgl_pipeline
	assert slot.sgl_materializations.len == before_slot.sgl_materializations.len + 1
	assert slot.key == before_slot.key
	assert slot.status == before_slot.status
	assert slot.app_scoped == before_slot.app_scoped
	assert slot.label == before_slot.label
	assert slot.dependencies == before_slot.dependencies
	assert slot.dependents == before_slot.dependents
	assert slot.last_mutation_batch == before_slot.last_mutation_batch
	assert slot.buffer_mutation_mode == before_slot.buffer_mutation_mode
	assert slot.buffer_capacity == before_slot.buffer_capacity
	assert slot.buffer_usage == before_slot.buffer_usage
	assert slot.buffer_desc == before_slot.buffer_desc
	assert slot.image_desc == before_slot.image_desc
	assert slot.sampler_desc == before_slot.sampler_desc
	assert slot.shader_desc == before_slot.shader_desc
	assert slot.pipeline_desc == before_slot.pipeline_desc
	assert slot.sgl_recipe == before_slot.sgl_recipe
	assert slot.buffer_id == before_slot.buffer_id
	assert slot.image_id == before_slot.image_id
	assert slot.sampler_id == before_slot.sampler_id
	assert slot.shader_id == before_slot.shader_id
	assert slot.pipeline_id == before_slot.pipeline_id
	assert slot.attachments_id == before_slot.attachments_id
	assert slot.image_width == before_slot.image_width
	assert slot.image_height == before_slot.image_height
	assert slot.image_sample_count == before_slot.image_sample_count
	assert slot.image_pixel_format == before_slot.image_pixel_format
	assert slot.image_usage == before_slot.image_usage
	assert slot.image_render_target == before_slot.image_render_target
	assert slot.target_identity == before_slot.target_identity
	assert slot.attachment_colors == before_slot.attachment_colors
	assert slot.attachment_resolves == before_slot.attachment_resolves
	assert slot.attachment_depth == before_slot.attachment_depth
	assert slot.sgl_shader == before_slot.sgl_shader
	expected_entry := '${target_key}:${expected_pipeline}:${target_key}'
	assert expected_entry !in before_slot.sgl_materializations
	mut expected_materializations := before_slot.sgl_materializations.clone()
	expected_materializations << expected_entry
	expected_materializations.sort()
	assert slot.sgl_materializations == expected_materializations
	mut unique_native := []u32{}
	for identity in native_pipelines {
		assert identity != 0
		assert identity !in unique_native
		unique_native << identity
	}
	assert unique_native == native_pipelines
}

fn multiwindow_assert_registry_graph(registry MultiWindowMatrixRegistryState) {
	assert registry.app_instance != 0
	mut alive_keys := []string{}
	for slot in registry.slots {
		if slot.status != .alive {
			continue
		}
		assert slot.key != ''
		assert slot.key !in alive_keys
		alive_keys << slot.key
		assert slot.app_scoped
		match slot.kind {
			.buffer {
				assert slot.buffer_id != 0
				assert slot.buffer_capacity > 0
				assert slot.buffer_desc.len > 0
			}
			.image {
				assert slot.image_id != 0
				assert slot.image_width > 0
				assert slot.image_height > 0
				assert slot.image_desc.len > 0
			}
			.sampler {
				assert slot.sampler_id != 0
				multiwindow_assert_sampler_desc_snapshot_pointer_free(slot.sampler_desc)
			}
			.shader {
				assert slot.shader_id != 0
				assert slot.shader_desc.len > 0
			}
			.pipeline {
				assert slot.pipeline_id != 0
				assert slot.pipeline_desc.len > 0
			}
			.attachments {
				assert slot.attachments_id != 0
				assert slot.target_identity != 0
				assert slot.attachment_colors.len > 0 || slot.attachment_depth != ''
			}
			.sgl_pipeline {
				assert slot.sgl_recipe.len > 0
				assert slot.pipeline_id == 0
				mut materializations := []string{}
				for materialization in slot.sgl_materializations {
					assert materialization != ''
					assert materialization !in materializations
					materializations << materialization
				}
			}
		}
	}
	for slot in registry.slots {
		if slot.status != .alive {
			continue
		}
		for dependency in slot.dependencies {
			dependency_slot := multiwindow_fault_matrix_registry_slot(registry, dependency)
			assert dependency_slot.status == .alive
			assert slot.key in dependency_slot.dependents
		}
		for dependent in slot.dependents {
			dependent_slot := multiwindow_fault_matrix_registry_slot(registry, dependent)
			assert dependent_slot.status == .alive
			assert slot.key in dependent_slot.dependencies
		}
	}
	for retired in registry.deferred {
		assert retired.retire_batch != 0
		match retired.kind {
			.image {
				assert retired.image_id != 0
			}
			.attachments {
				assert retired.attachments_id != 0
			}
			.buffer {
				assert retired.buffer_id != 0
			}
			.sampler {
				assert retired.sampler_id != 0
			}
			.shader {
				assert retired.shader_id != 0
			}
			.pipeline, .sgl_pipeline {
				assert retired.pipeline_id != 0 || retired.sgl_materializations.len > 0
			}
		}
	}
}

fn multiwindow_assert_replacement_registry(before MultiWindowMatrixRegistryState, after MultiWindowMatrixRegistryState, image WindowImageId, attachments []WindowAttachmentsId, old_image u32, old_attachments []u32, new_image u32, new_attachments []u32, desc gfx.ImageDesc, mutation_batch u64) {
	assert attachments.len == 2
	assert old_attachments.len == 2
	assert new_attachments.len == 2
	image_key := multiwindow_fault_matrix_key(image_resource_key(image))
	before_image := multiwindow_fault_matrix_registry_slot(before, image_key)
	after_image := multiwindow_fault_matrix_registry_slot(after, image_key)
	assert before_image.status == .alive
	assert after_image.status == .alive
	assert before_image.image_id == old_image
	assert after_image.image_id == new_image
	effective := gfx.query_image_desc_defaults(&desc)
	expected_image_desc := pointer_free_image_desc(effective)
	multiwindow_assert_desc_semantically_equal(after_image.image_desc, &expected_image_desc)
	actual_image_without_desc := MultiWindowMatrixSlotState{
		...after_image
		image_desc: []u8{}
	}
	expected_image_without_desc := MultiWindowMatrixSlotState{
		...before_image
		image_id:            new_image
		image_desc:          []u8{}
		image_width:         effective.width
		image_height:        effective.height
		image_sample_count:  effective.sample_count
		image_pixel_format:  effective.pixel_format
		image_usage:         effective.usage
		image_render_target: effective.render_target
		last_mutation_batch: mutation_batch
	}
	assert actual_image_without_desc == expected_image_without_desc
	for index, id in attachments {
		key := multiwindow_fault_matrix_key(attachments_resource_key(id))
		before_attachment := multiwindow_fault_matrix_registry_slot(before, key)
		after_attachment := multiwindow_fault_matrix_registry_slot(after, key)
		assert before_attachment.attachments_id == old_attachments[index]
		assert after_attachment.attachments_id == new_attachments[index]
		expected_attachment := MultiWindowMatrixSlotState{
			...before_attachment
			attachments_id:  new_attachments[index]
			target_identity: before_attachment.target_identity + 1
		}
		assert after_attachment == expected_attachment
	}
	assert after.deferred.len == before.deferred.len + 3
	deferred_start := before.deferred.len
	assert after.deferred[deferred_start] == MultiWindowMatrixDeferredState{
		kind:           .attachments
		retire_batch:   mutation_batch
		attachments_id: old_attachments[0]
	}
	assert after.deferred[deferred_start + 1] == MultiWindowMatrixDeferredState{
		kind:           .attachments
		retire_batch:   mutation_batch
		attachments_id: old_attachments[1]
	}
	assert after.deferred[deferred_start + 2] == MultiWindowMatrixDeferredState{
		kind:         .image
		retire_batch: mutation_batch
		image_id:     old_image
	}
}

fn multiwindow_assert_final_retry_registry(proof &MultiWindowRealOperationProof) {
	assert proof.retry_registry.len > 0
	registry := proof.retry_registry.last()
	update_buffer := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(buffer_resource_key(proof.update_buffer)))
	append_buffer := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(buffer_resource_key(proof.append_buffer)))
	update_image := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(image_resource_key(proof.update_image)))
	assert update_buffer.buffer_mutation_mode == .update
	assert append_buffer.buffer_mutation_mode == .append
	assert update_buffer.last_mutation_batch != 0
	assert append_buffer.last_mutation_batch == update_buffer.last_mutation_batch
	assert update_image.last_mutation_batch == update_buffer.last_mutation_batch
	for image in [proof.replace_image, proof.rebuild_image] {
		slot := multiwindow_fault_matrix_registry_slot(registry,
			multiwindow_fault_matrix_key(image_resource_key(image)))
		assert slot.dependents.len == 2
		assert slot.last_mutation_batch == update_buffer.last_mutation_batch
	}
	for attachments in [proof.replace_attachments, proof.replace_attachments_second,
		proof.rebuild_attachments, proof.rebuild_attachments_second] {
		slot := multiwindow_fault_matrix_registry_slot(registry,
			multiwindow_fault_matrix_key(attachments_resource_key(attachments)))
		assert slot.dependencies.len == 1
		assert slot.target_identity == 2
	}
	default_recipe := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(sgl_pipeline_resource_key(proof.sgl_default)))
	shader_recipe := multiwindow_fault_matrix_registry_slot(registry,
		multiwindow_fault_matrix_key(sgl_pipeline_resource_key(proof.sgl_shader)))
	assert default_recipe.sgl_materializations.len == 2
	assert shader_recipe.sgl_materializations.len == 1
	assert default_recipe.dependencies.len == 0
	assert shader_recipe.dependencies == [
		multiwindow_fault_matrix_key(shader_resource_key(proof.shader)),
	]
	for target_key in proof.default_target_keys {
		assert default_recipe.sgl_materializations.any(it.starts_with('${target_key}:'))
	}
	assert shader_recipe.sgl_materializations[0].starts_with('${proof.shader_target_key}:')
}

fn multiwindow_fault_matrix_registry_slot(registry MultiWindowMatrixRegistryState, key string) MultiWindowMatrixSlotState {
	for slot in registry.slots {
		if slot.key == key {
			return slot
		}
	}
	assert false, 'retry snapshot dependency `${key}` has no registry slot'
	return MultiWindowMatrixSlotState{}
}

fn multiwindow_fault_matrix_registry_locked(registry MultiWindowResourceRegistry) MultiWindowMatrixRegistryState {
	mut slots := []MultiWindowMatrixSlotState{cap: registry.slots.len}
	mut deferred := []MultiWindowMatrixDeferredState{cap: registry.deferred.len}
	for index, slot in registry.slots {
		mut dependencies := []string{cap: slot.dependencies.len}
		for key in slot.dependencies {
			dependencies << multiwindow_fault_matrix_key(key)
		}
		mut dependents := []string{cap: slot.dependents.len}
		for key in slot.dependents {
			dependents << multiwindow_fault_matrix_key(key)
		}
		mut colors := []string{cap: slot.attachments_recipe.colors.len}
		for key in slot.attachments_recipe.colors {
			colors << multiwindow_fault_matrix_key(key)
		}
		mut resolves := []string{cap: slot.attachments_recipe.resolves.len}
		for key in slot.attachments_recipe.resolves {
			resolves << multiwindow_fault_matrix_key(key)
		}
		mut depth := ''
		if key := slot.attachments_recipe.depth_stencil {
			depth = multiwindow_fault_matrix_key(key)
		}
		mut shader := ''
		if key := slot.sgl_shader {
			shader = multiwindow_fault_matrix_key(key)
		}
		slots << MultiWindowMatrixSlotState{
			key:                  '${slot.app_instance}:${index}:${slot.generation}:${slot.window.str()}'
			kind:                 slot.kind
			status:               slot.status
			app_scoped:           slot.app_scoped
			label:                slot.label
			dependencies:         dependencies
			dependents:           dependents
			last_mutation_batch:  slot.last_mutation_batch
			buffer_mutation_mode: slot.buffer_mutation_mode
			buffer_capacity:      slot.buffer_capacity
			buffer_usage:         slot.buffer_usage
			buffer_desc:          multiwindow_fault_matrix_raw_bytes(&slot.buffer_desc)
			image_desc:           multiwindow_fault_matrix_raw_bytes(&slot.image_desc)
			sampler_desc:         multiwindow_fault_matrix_sampler_desc_snapshot(slot.sampler_desc)
			shader_desc:          multiwindow_fault_matrix_raw_bytes(&slot.shader_desc)
			pipeline_desc:        multiwindow_fault_matrix_raw_bytes(&slot.pipeline_desc)
			sgl_recipe:           multiwindow_fault_matrix_raw_bytes(&slot.sgl_recipe)
			buffer_id:            slot.buffer.id
			image_id:             slot.image.id
			sampler_id:           slot.sampler.id
			shader_id:            slot.shader.id
			pipeline_id:          slot.pipeline.id
			attachments_id:       slot.attachments.id
			image_width:          slot.image_desc.width
			image_height:         slot.image_desc.height
			image_sample_count:   slot.image_desc.sample_count
			image_pixel_format:   slot.image_desc.pixel_format
			image_usage:          slot.image_desc.usage
			image_render_target:  slot.image_desc.render_target
			target_identity:      slot.target_identity
			attachment_colors:    colors
			attachment_resolves:  resolves
			attachment_depth:     depth
			sgl_shader:           shader
			sgl_materializations: multiwindow_fault_matrix_materializations(slot.materialized_sgl)
		}
	}
	for retired in registry.deferred {
		mut key := ''
		if actual := retired.key {
			key = multiwindow_fault_matrix_key(actual)
		}
		deferred << MultiWindowMatrixDeferredState{
			key:                  key
			kind:                 retired.kind
			retire_batch:         retired.retire_batch
			buffer_id:            retired.buffer.id
			image_id:             retired.image.id
			sampler_id:           retired.sampler.id
			shader_id:            retired.shader.id
			pipeline_id:          retired.pipeline.id
			attachments_id:       retired.attachments.id
			sgl_materializations: multiwindow_fault_matrix_materializations(retired.materialized_sgl)
		}
	}
	return MultiWindowMatrixRegistryState{
		app_instance: registry.app_instance
		slots:        slots
		deferred:     deferred
	}
}

fn multiwindow_fault_matrix_materializations(values map[string]MultiWindowSglMaterialization) []string {
	mut keys := values.keys()
	keys.sort()
	mut result := []string{cap: keys.len}
	for key in keys {
		value := values[key]
		result << '${key}:${value.pipeline.id}:${value.context_key}'
	}
	return result
}

fn multiwindow_fault_matrix_raw_bytes[T](value &T) []u8 {
	mut result := []u8{len: int(sizeof(T))}
	unsafe {
		C.memcpy(result.data, value, usize(result.len))
	}
	return result
}

fn multiwindow_assert_desc_semantically_equal[T](actual_bytes []u8, expected &T) {
	assert actual_bytes.len == int(sizeof(T))
	mut actual := T{}
	unsafe {
		C.memcpy(&actual, actual_bytes.data, usize(actual_bytes.len))
	}
	assert actual == *expected
}

fn multiwindow_fault_matrix_sampler_desc_snapshot(desc gfx.SamplerDesc) MultiWindowSamplerDescSnapshot {
	return MultiWindowSamplerDescSnapshot{
		start_canary:        desc._start_canary
		min_filter:          desc.min_filter
		mag_filter:          desc.mag_filter
		mipmap_filter:       desc.mipmap_filter
		wrap_u:              desc.wrap_u
		wrap_v:              desc.wrap_v
		wrap_w:              desc.wrap_w
		min_lod:             desc.min_lod
		max_lod:             desc.max_lod
		border_color:        desc.border_color
		compare:             desc.compare
		max_anisotropy:      desc.max_anisotropy
		label_is_nil:        desc.label == unsafe { nil }
		gl_sampler:          desc.gl_sampler
		mtl_sampler_is_nil:  desc.mtl_sampler == unsafe { nil }
		d3d_sampler_is_nil:  desc.d3d11_sampler == unsafe { nil }
		wgpu_sampler_is_nil: desc.wgpu_sampler == unsafe { nil }
		end_canary:          desc._end_canary
	}
}

fn multiwindow_assert_sampler_desc_snapshot_pointer_free(snapshot MultiWindowSamplerDescSnapshot) {
	assert snapshot.start_canary == 0
	assert snapshot.end_canary == 0
	assert snapshot.label_is_nil
	assert snapshot.gl_sampler == 0
	assert snapshot.mtl_sampler_is_nil
	assert snapshot.d3d_sampler_is_nil
	assert snapshot.wgpu_sampler_is_nil
}

fn multiwindow_fault_matrix_key(key MultiWindowResourceKey) string {
	return '${key.app_instance}:${key.slot}:${key.generation}:${key.window.str()}'
}

fn multiwindow_assert_matrix_checkpoint_equal(expected MultiWindowMatrixCheckpoint, actual MultiWindowMatrixCheckpoint) {
	assert actual.plan == expected.plan
	assert actual.registry == expected.registry
	multiwindow_assert_typed_trace_equal(expected.trace, actual.trace)
}

fn multiwindow_assert_matrix_fault_consumed(armed MultiWindowMatrixCheckpoint, actual MultiWindowMatrixCheckpoint) {
	assert actual.plan == MultiWindowMatrixPlanState{}
	assert actual.registry == armed.registry
	multiwindow_assert_typed_trace_equal(armed.trace, actual.trace)
}

fn multiwindow_assert_typed_trace_equal(expected multiwindow_sokol_trace.TypedSnapshot, actual multiwindow_sokol_trace.TypedSnapshot) {
	assert actual.count == expected.count
	assert actual.overflow == expected.overflow
	assert actual.install_generation == expected.install_generation
	assert actual.records.len == expected.records.len
	for index, record in expected.records {
		candidate := actual.records[index]
		assert candidate.operation == record.operation
		assert candidate.identity == record.identity
		assert candidate.value == record.value
		assert candidate.sequence == record.sequence
		assert candidate.swapchain_identity == record.swapchain_identity
		assert candidate.width == record.width
		assert candidate.height == record.height
		assert candidate.sample_count == record.sample_count
		assert candidate.color_format == record.color_format
		assert candidate.depth_format == record.depth_format
	}
}

fn multiwindow_assert_trace_retained_after_logical_release(before multiwindow_sokol_trace.TypedSnapshot, after multiwindow_sokol_trace.TypedSnapshot) {
	assert before.install_generation != 0
	assert after.install_generation == 0
	assert after.count == before.count
	assert after.overflow == before.overflow
	assert after.records == before.records
}

fn multiwindow_assert_cleanup_error_swapchain_pass(snapshot multiwindow_sokol_trace.TypedSnapshot, cursor int, width int, height int) int {
	assert cursor >= 0
	assert cursor + 2 < snapshot.records.len
	begin_pass := snapshot.records[cursor]
	assert begin_pass.operation == .begin_swapchain_pass
	assert begin_pass.width == width
	assert begin_pass.height == height
	assert snapshot.records[cursor + 1].operation == .end_pass
	assert snapshot.records[cursor + 2].operation == .commit
	return cursor + 3
}

fn multiwindow_assert_cleanup_error_trace(snapshot multiwindow_sokol_trace.TypedSnapshot, app_buffer u32, window_buffer u32, framebuffer_width int, framebuffer_height int) {
	assert app_buffer != 0
	assert window_buffer != 0
	assert app_buffer != window_buffer
	assert snapshot.install_generation != 0
	assert !snapshot.overflow
	assert snapshot.count == usize(snapshot.records.len)
	assert snapshot.records.len > 0
	assert snapshot.records[0].sequence != 0
	for index in 1 .. snapshot.records.len {
		assert snapshot.records[index].sequence == snapshot.records[index - 1].sequence + 1
	}

	mut cursor := 0
	app_create := snapshot.records[cursor]
	assert app_create.operation == .make_buffer
	assert app_create.identity == app_buffer
	cursor++
	if cursor < snapshot.records.len && snapshot.records[cursor].operation == .begin_swapchain_pass {
		cursor = multiwindow_assert_cleanup_error_swapchain_pass(snapshot, cursor, 1, 1)
	}

	context_create := snapshot.records[cursor]
	assert context_create.operation == .make_buffer
	assert context_create.identity != 0
	assert context_create.identity != app_buffer
	assert context_create.identity != window_buffer
	context_buffer := context_create.identity
	cursor++
	mut context_pipelines := []u32{cap: 5}
	for _ in 0 .. 5 {
		pipeline := snapshot.records[cursor]
		assert pipeline.operation == .make_pipeline
		assert pipeline.identity != 0
		assert pipeline.identity !in context_pipelines
		context_pipelines << pipeline.identity
		cursor++
	}

	window_create := snapshot.records[cursor]
	assert window_create.operation == .make_buffer
	assert window_create.identity == window_buffer
	cursor++

	cursor = multiwindow_assert_cleanup_error_swapchain_pass(snapshot, cursor, framebuffer_width,
		framebuffer_height)
	cursor = multiwindow_assert_cleanup_error_swapchain_pass(snapshot, cursor, 1, 1)

	window_destroy := snapshot.records[cursor]
	assert window_destroy.operation == .destroy_buffer
	assert window_destroy.identity == window_buffer
	cursor++
	context_destroy := snapshot.records[cursor]
	assert context_destroy.operation == .destroy_buffer
	assert context_destroy.identity == context_buffer
	cursor++
	for identity in context_pipelines {
		pipeline_destroy := snapshot.records[cursor]
		assert pipeline_destroy.operation == .destroy_pipeline
		assert pipeline_destroy.identity == identity
		cursor++
	}
	cursor = multiwindow_assert_cleanup_error_swapchain_pass(snapshot, cursor, 1, 1)
	app_destroy := snapshot.records[cursor]
	assert app_destroy.operation == .destroy_buffer
	assert app_destroy.identity == app_buffer
	cursor++

	shutdown_expected := [
		multiwindow_sokol_trace.Operation.destroy_buffer,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_image,
		.destroy_sampler,
		.destroy_shader,
	]
	for offset, operation in shutdown_expected {
		record := snapshot.records[cursor + offset]
		assert record.operation == operation
		assert record.identity != 0
	}
	cursor += shutdown_expected.len
	assert cursor == snapshot.records.len
}

fn multiwindow_assert_shutdown_destruction_tail(before multiwindow_sokol_trace.TypedSnapshot, after multiwindow_sokol_trace.TypedSnapshot) {
	assert before.install_generation != 0
	assert after.install_generation == before.install_generation
	assert !before.overflow
	assert !after.overflow
	context_shutdown := [
		multiwindow_sokol_trace.Operation.begin_swapchain_pass,
		.end_pass,
		.commit,
		.destroy_buffer,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
	]
	global_shutdown := [
		multiwindow_sokol_trace.Operation.destroy_buffer,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_pipeline,
		.destroy_image,
		.destroy_sampler,
		.destroy_shader,
	]
	expected_tail := context_shutdown.len * 2 + global_shutdown.len
	assert after.count == before.count + usize(expected_tail)
	assert after.records.len == int(after.count)
	for index, record in before.records {
		assert after.records[index] == record
	}
	mut offset := 0
	for _ in 0 .. 2 {
		for operation in context_shutdown {
			record := after.records[int(before.count) + offset]
			assert record.operation == operation
			assert record.sequence == after.records[int(before.count) + offset - 1].sequence + 1
			if operation == .begin_swapchain_pass {
				assert record.width > 0
				assert record.height > 0
			} else if operation in [.destroy_buffer, .destroy_pipeline] {
				assert record.identity != 0
			}
			offset++
		}
	}
	for operation in global_shutdown {
		record := after.records[int(before.count) + offset]
		assert record.operation == operation
		assert record.identity != 0
		assert record.sequence == after.records[int(before.count) + offset - 1].sequence + 1
		offset++
	}
	assert offset == expected_tail
}

fn multiwindow_assert_single_trace_delta(before multiwindow_sokol_trace.TypedSnapshot, after multiwindow_sokol_trace.TypedSnapshot, operation multiwindow_sokol_trace.Operation, identity u32, value i64) {
	assert !before.overflow
	assert !after.overflow
	assert before.install_generation != 0
	assert after.install_generation == before.install_generation
	assert after.count == before.count + 1
	record := after.records[int(before.count)]
	assert record.operation == operation
	assert record.identity == identity
	assert record.identity != 0
	assert record.value == value
	assert record.sequence == multiwindow_fault_matrix_next_trace_sequence(before)
}

fn multiwindow_assert_trace_delta(before multiwindow_sokol_trace.TypedSnapshot, after multiwindow_sokol_trace.TypedSnapshot, expected []MultiWindowMatrixDestroyExpectation) {
	assert !before.overflow
	assert !after.overflow
	assert before.install_generation != 0
	assert after.install_generation == before.install_generation
	assert after.count == before.count + usize(expected.len)
	for index, item in expected {
		record := after.records[int(before.count) + index]
		assert record.operation == item.operation
		assert record.identity == item.identity
		assert record.identity != 0
		assert record.sequence == multiwindow_fault_matrix_next_trace_sequence(before) + u64(index)
	}
}

fn multiwindow_fault_matrix_next_trace_sequence(snapshot multiwindow_sokol_trace.TypedSnapshot) u64 {
	if snapshot.count == 0 {
		return 1
	}
	return snapshot.records[int(snapshot.count) - 1].sequence + 1
}

fn multiwindow_fault_matrix_dynamic_image_desc() gfx.ImageDesc {
	return gfx.ImageDesc{
		width:        4
		height:       4
		usage:        .dynamic
		pixel_format: .rgba8
	}
}

fn multiwindow_fault_matrix_render_target_desc() gfx.ImageDesc {
	return gfx.ImageDesc{
		render_target: true
		width:         8
		height:        8
		sample_count:  1
		pixel_format:  .rgba8
	}
}

fn multiwindow_fault_matrix_incompatible_render_target_desc() gfx.ImageDesc {
	return gfx.ImageDesc{
		render_target: true
		width:         -1
		height:        8
		sample_count:  1
		pixel_format:  .rgba8
	}
}

fn multiwindow_fault_matrix_pipeline_desc() gfx.PipelineDesc {
	mut desc := gfx.PipelineDesc{}
	desc.layout.attrs[0].format = .float3
	desc.layout.attrs[1].format = .float2
	desc.layout.attrs[2].format = .ubyte4n
	desc.layout.attrs[3].format = .float
	return desc
}

fn multiwindow_fault_matrix_slot(runtime &MultiWindowRenderRuntime, key MultiWindowResourceKey, kind MultiWindowResourceKind) !MultiWindowResourceSlot {
	runtime.mutex.lock()
	if key.slot < 0 || key.slot >= runtime.resources.slots.len {
		runtime.mutex.unlock()
		return error('fault-matrix resource key has no registry slot')
	}
	slot := runtime.resources.slots[key.slot]
	if slot.app_instance != key.app_instance || slot.generation != key.generation
		|| slot.window != key.window || slot.kind != kind || slot.status != .alive {
		runtime.mutex.unlock()
		return error('fault-matrix resource key does not identify a live exact slot')
	}
	runtime.mutex.unlock()
	return slot
}

fn multiwindow_fault_matrix_buffer_native(runtime &MultiWindowRenderRuntime, id WindowBufferId) !u32 {
	slot := multiwindow_fault_matrix_slot(runtime, buffer_resource_key(id), .buffer)!
	if slot.buffer.id == 0 {
		return error('fault-matrix buffer has no Sokol identity')
	}
	return slot.buffer.id
}

fn multiwindow_fault_matrix_image_native(runtime &MultiWindowRenderRuntime, id WindowImageId) !u32 {
	slot := multiwindow_fault_matrix_slot(runtime, image_resource_key(id), .image)!
	if slot.image.id == 0 {
		return error('fault-matrix image has no Sokol identity')
	}
	return slot.image.id
}

fn multiwindow_fault_matrix_sampler_native(runtime &MultiWindowRenderRuntime, id WindowSamplerId) !u32 {
	slot := multiwindow_fault_matrix_slot(runtime, sampler_resource_key(id), .sampler)!
	if slot.sampler.id == 0 {
		return error('fault-matrix sampler has no Sokol identity')
	}
	return slot.sampler.id
}

fn multiwindow_fault_matrix_shader_native(runtime &MultiWindowRenderRuntime, id WindowShaderId) !u32 {
	slot := multiwindow_fault_matrix_slot(runtime, shader_resource_key(id), .shader)!
	if slot.shader.id == 0 {
		return error('fault-matrix shader has no Sokol identity')
	}
	return slot.shader.id
}

fn multiwindow_fault_matrix_pipeline_native(runtime &MultiWindowRenderRuntime, id WindowPipelineId) !u32 {
	slot := multiwindow_fault_matrix_slot(runtime, pipeline_resource_key(id), .pipeline)!
	if slot.pipeline.id == 0 {
		return error('fault-matrix pipeline has no Sokol identity')
	}
	return slot.pipeline.id
}

fn multiwindow_fault_matrix_attachments_native(runtime &MultiWindowRenderRuntime, id WindowAttachmentsId) !u32 {
	slot := multiwindow_fault_matrix_slot(runtime, attachments_resource_key(id), .attachments)!
	if slot.attachments.id == 0 {
		return error('fault-matrix attachments have no Sokol identity')
	}
	return slot.attachments.id
}

fn multiwindow_fault_matrix_terminal_state(app &App) MultiWindowMatrixTerminalState {
	mut failed_init_windows := []string{cap: app.failed_init_windows.len}
	for id in app.failed_init_windows {
		failed_init_windows << id.str()
	}
	return MultiWindowMatrixTerminalState{
		app_instance:              app.app_instance
		core_status:               app.core.status()
		gfx_started:               app.gfx_started
		sgl_initialized:           app.sgl_initialized
		render_owner_claimed:      app.render_owner_claimed
		legacy_render_mode:        app.legacy_render_mode
		app_frame_active:          app.app_frame_active
		active_batch_epoch:        app.active_batch_epoch
		sgl_contexts:              app.sgl_contexts.len
		sgl_context_targets:       app.sgl_context_targets.len
		window_sgl_targets:        app.window_sgl_targets.len
		deferred_sgl_targets:      app.deferred_sgl_targets.len
		active_render_snapshots:   app.active_render_snapshots.len
		active_drawn_windows:      app.active_drawn_windows.len
		failed_init_windows:       failed_init_windows
		renderer_terminal_failure: app.renderer_terminal_failure
		terminal_error:            app.terminal_error
		runtime:                   multiwindow_fault_matrix_runtime_state(app.render_runtime)
	}
}

fn multiwindow_fault_matrix_runtime_state(runtime &MultiWindowRenderRuntime) MultiWindowMatrixRuntimeState {
	runtime.mutex.lock()
	mut deferred_window_ids := []string{cap: runtime.deferred_windows.len}
	for id in runtime.deferred_windows {
		deferred_window_ids << id.str()
	}
	mut windows_raw := [][]u8{cap: runtime.windows.len}
	for window in runtime.windows {
		windows_raw << multiwindow_fault_matrix_raw_bytes(&window)
	}
	mut window_invalid := 0
	mut window_destroyed := 0
	mut cleanup_started := 0
	mut cleanup_finished := 0
	mut app_stop_reasons := 0
	for window in runtime.windows {
		if window.status == .invalid {
			window_invalid++
		}
		if window.status == .destroyed {
			window_destroyed++
		}
		if window.cleanup_started {
			cleanup_started++
		}
		if window.cleanup_finished {
			cleanup_finished++
		}
		if window.cleanup_reason_set && window.cleanup_reason == .app_stop {
			app_stop_reasons++
		}
	}
	state := MultiWindowMatrixRuntimeState{
		window_count:            runtime.windows.len
		window_invalid:          window_invalid
		window_destroyed:        window_destroyed
		cleanup_started:         cleanup_started
		cleanup_finished:        cleanup_finished
		app_stop_reasons:        app_stop_reasons
		active_batch_epoch:      runtime.active_batch_epoch
		last_completed_batch:    runtime.last_completed_batch
		batch_active:            runtime.batch_active
		callback_depth:          runtime.callback_depth
		deferred_windows:        runtime.deferred_windows.len
		deferred_stop:           runtime.deferred_stop
		stopping:                runtime.stopping
		stopped:                 runtime.stopped
		app_resource_active:     runtime.app_resource_active
		app_init_started:        runtime.app_init_started
		app_init_completed:      runtime.app_init_completed
		app_init_terminal:       runtime.app_init_terminal
		app_cleanup_started:     runtime.app_cleanup_started
		app_cleanup_finished:    runtime.app_cleanup_finished
		app_resources_retired:   runtime.app_resources_retired
		next_lease_epoch:        runtime.next_lease_epoch
		next_pass_epoch:         runtime.next_pass_epoch
		app_lease_epoch:         runtime.app_lease_epoch
		app_phase:               runtime.app_phase
		app_cleanup_lease_epoch: runtime.app_cleanup_lease_epoch
		deferred_window_ids:     deferred_window_ids
		windows_raw:             windows_raw
		registry:                multiwindow_fault_matrix_registry_locked(runtime.resources)
	}
	runtime.mutex.unlock()
	return state
}

fn multiwindow_assert_terminal_registry_empty(registry MultiWindowMatrixRegistryState) {
	assert registry.deferred.len == 0
	for slot in registry.slots {
		assert slot.status in [.invalid, .exhausted]
		assert slot.buffer_id == 0
		assert slot.image_id == 0
		assert slot.sampler_id == 0
		assert slot.shader_id == 0
		assert slot.pipeline_id == 0
		assert slot.attachments_id == 0
		assert slot.dependencies.len == 0
		assert slot.dependents.len == 0
		assert slot.sgl_materializations.len == 0
	}
}

fn multiwindow_assert_healthy_sgl_shutdown_order(snapshot MultiWindowLifecycleTraceSnapshot) {
	assert snapshot.trace_enabled
	assert !snapshot.overflow
	assert !snapshot.mismatch
	expected := [
		MultiWindowLifecycleMilestone.resource_cleanup_enter,
		.resource_cleanup_batch_complete,
		.runtime_finish_stop_enter,
		.runtime_finish_stop_complete,
		.prepare_enter,
		.prepare_complete,
		.sgl_shutdown_enter,
		.sgl_shutdown_complete,
		.core_renderer_shutdown_enter,
		.core_renderer_shutdown_complete,
		.core_finish_stop_enter,
		.core_finish_stop_complete,
	]
	assert snapshot.len == expected.len
	for index, milestone in expected {
		assert snapshot.milestones[index] == milestone
	}
}

fn multiwindow_assert_cleanup_error_lifecycle(snapshot MultiWindowLifecycleTraceSnapshot) {
	assert snapshot.trace_enabled
	assert !snapshot.overflow
	assert !snapshot.mismatch
	expected := [
		MultiWindowLifecycleMilestone.resource_cleanup_enter,
		.resource_cleanup_failed,
		.runtime_finish_stop_enter,
		.runtime_finish_stop_complete,
		.prepare_enter,
		.prepare_complete,
		.sgl_shutdown_enter,
		.sgl_shutdown_complete,
		.core_renderer_shutdown_enter,
		.core_renderer_shutdown_complete,
		.core_finish_stop_enter,
		.core_finish_stop_failed,
	]
	assert snapshot.len == expected.len
	for index, milestone in expected {
		assert snapshot.milestones[index] == milestone
	}
}

fn multiwindow_assert_lifecycle_trace_equal(expected MultiWindowLifecycleTraceSnapshot, actual MultiWindowLifecycleTraceSnapshot) {
	assert actual.trace_enabled == expected.trace_enabled
	assert actual.len == expected.len
	assert actual.overflow == expected.overflow
	assert actual.mismatch == expected.mismatch
	for index in 0 .. multiwindow_lifecycle_trace_capacity {
		assert actual.milestones[index] == expected.milestones[index]
	}
}
