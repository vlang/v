module main

import gg
import sokol.gfx
import time
import gg.testdata.multiwindow_probe_backend
import gg.testdata.multiwindow_probe_gate
import gg.testdata.multiwindow_sokol_trace

#flag -DSOKOL_TRACE_HOOKS

const dynamic_texture_probe_watchdog = 5 * time.second
const dynamic_texture_duplicate_update_error = 'gg.multiwindow: render resource can be updated only once per frame batch'
const dynamic_texture_resource_scope_error = 'gg.multiwindow: render resource belongs to a different window or app'
const dynamic_texture_partial_init_injection = 'dynamic texture partial app init injection'

struct DynamicTextureProbeFrame {
	window     string
	serial     u64
	generation u64
}

struct DynamicTextureProbeState {
mut:
	app                            &gg.App = unsafe { nil }
	main_window                    gg.WindowId
	tool_window                    gg.WindowId
	texture                        gg.WindowImageId
	sampler                        gg.WindowSamplerId
	render_target                  gg.WindowImageId
	attachments                    gg.WindowAttachmentsId
	app_initialized                bool
	app_cleaned                    bool
	texture_updates                u64
	resource_batches               u64
	resource_batch_armed           bool
	replacements                   u64
	dependency_rejections          u64
	invalid_replacement_rejections u64
	duplicate_update_rejections    u64
	frames                         map[string]u64
	rendered_frames                map[string]u64
	armed_generation               map[string]u64
	accepted_generation            map[string]u64
	phase_generation               u64
	snapshot_armed_generation      u64
	snapshot_posted_generation     u64
	cleanup_submitted              map[string]u64
	recovery_requested             bool
	trace_installed                bool
	trace_uninstalled              bool
}

struct AppResourceOnlyProbeState {
mut:
	current_image     gg.WindowImageId
	init_calls        u64
	frame_calls       u64
	cleanup_calls     u64
	identity_rejected bool
}

struct CleanupOnlyProbeState {
mut:
	cleanup_calls u64
}

struct PartialInitProbeState {
mut:
	partial_image gg.WindowImageId
	init_calls    u64
	cleanup_calls u64
}

fn main() {
	run_dynamic_texture_probe() or { panic(err) }
}

fn run_dynamic_texture_probe() ! {
	multiwindow_probe_gate.await_parent_release(dynamic_texture_probe_watchdog)!
	frames := chan DynamicTextureProbeFrame{cap: 8}
	driver_result := chan string{cap: 1}
	trace_ack := chan string{cap: 1}
	trace_snapshot := chan []string{cap: 1}
	mut state := &DynamicTextureProbeState{}
	backend := multiwindow_probe_backend.selected()!
	mut app := gg.new_app(backend: backend, queue_size: 16, require_renderer: true)!
	multiwindow_probe_backend.validate(app.capabilities(), backend)!
	state.app = app
	main_window := app.create_window(
		title:       'dynamic texture consumer: main'
		width:       480
		height:      320
		redraw_mode: .on_demand
		frame_fn:    fn [frames, trace_snapshot, mut state] (mut context gg.WindowContext) ! {
			draw_dynamic_texture_probe(mut context, frames, trace_snapshot, mut state)!
		}
		cleanup_fn:  fn [mut state] (mut context gg.WindowCleanupContext) ! {
			state.cleanup_submitted[context.window_id().str()] = context.metrics().submitted_frame
		}
	)!
	state.main_window = main_window
	tool_window := app.create_window(
		title:       'dynamic texture consumer: inspector'
		width:       300
		height:      220
		redraw_mode: .on_demand
		frame_fn:    fn [frames, trace_snapshot, mut state] (mut context gg.WindowContext) ! {
			draw_dynamic_texture_probe(mut context, frames, trace_snapshot, mut state)!
		}
		cleanup_fn:  fn [mut state] (mut context gg.WindowCleanupContext) ! {
			state.cleanup_submitted[context.window_id().str()] = context.metrics().submitted_frame
		}
	)!
	state.tool_window = tool_window
	initial_generation := arm_dynamic_texture_initial_frames(mut state, [main_window, tool_window])
	state.resource_batch_armed = true

	driver := spawn drive_dynamic_texture_probe(mut app, main_window, tool_window, frames, mut
		state, trace_ack, trace_snapshot, initial_generation, driver_result)
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut state] (mut resources gg.AppResourceContext) ! {
			multiwindow_sokol_trace.install()!
			state.trace_installed = true
			image_desc := gfx.ImageDesc{
				width:        4
				height:       4
				usage:        .dynamic
				pixel_format: .rgba8
			}
			sampler_desc := gfx.SamplerDesc{
				min_filter: .nearest
				mag_filter: .nearest
				wrap_u:     .clamp_to_edge
				wrap_v:     .clamp_to_edge
			}
			state.texture = resources.make_image(&image_desc)!
			state.sampler = resources.make_sampler(&sampler_desc)!
			render_target_desc := dynamic_texture_render_target_desc(64, 64)
			state.render_target = resources.make_image(&render_target_desc)!
			state.attachments = resources.make_attachments(gg.WindowAttachmentsConfig{
				colors: [state.render_target]
			})!
			state.app_initialized = true
		}
		app_resource_frame_fn:   fn [mut state] (mut resources gg.AppResourceContext) ! {
			if !state.resource_batch_armed {
				return
			}
			state.resource_batch_armed = false
			update_dynamic_texture(mut resources, mut state)!
			state.resource_batches++
			if state.resource_batches == 2 {
				update_dynamic_texture(mut resources, mut state) or {
					if err.msg() != dynamic_texture_duplicate_update_error {
						return err
					}
					state.duplicate_update_rejections++
				}
				if state.duplicate_update_rejections != 1 {
					return error('second update in one global batch unexpectedly succeeded')
				}
				resources.retire_image(state.render_target) or { state.dependency_rejections++ }
				if state.dependency_rejections != 1 {
					return error('live attachment dependency did not reject image retirement')
				}
				invalid_desc := dynamic_texture_render_target_desc(0, 64)
				_ := resources.replace_image(state.render_target, &invalid_desc) or {
					state.invalid_replacement_rejections++
					state.render_target
				}
				if state.invalid_replacement_rejections != 1 {
					return error('invalid replacement unexpectedly succeeded')
				}
			}
			if state.resource_batches == 3 {
				replacement_desc := dynamic_texture_render_target_desc(96, 64)
				state.render_target = resources.replace_image(state.render_target,
					&replacement_desc)!
				state.replacements++
			}
			if state.recovery_requested {
				return error('dynamic texture recovery injection')
			}
		}
		app_resource_cleanup_fn: fn [mut state] (mut resources gg.AppResourceContext) ! {
			resources.retire_attachments(state.attachments)!
			resources.retire_image(state.render_target)!
			resources.retire_sampler(state.sampler)!
			resources.retire_image(state.texture)!
			multiwindow_sokol_trace.uninstall()
			state.trace_uninstalled = true
			state.app_cleaned = true
		}
		event_fn:                fn (event gg.WindowEvent, mut app gg.App) ! {
			if event.kind == .window_close_requested && app.window_exists(event.window) {
				app.destroy_window(event.window)!
			}
		}
	) or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	recovery_trace := multiwindow_sokol_trace.snapshot()
	mut first_stop_error := ''
	app.stop() or { first_stop_error = err.msg() }
	mut second_stop_error := ''
	app.stop() or { second_stop_error = err.msg() }

	mut failures := []string{}
	if !run_error.contains('dynamic texture recovery injection') {
		failures << 'owner loop did not return the injected recovery error: ${run_error}'
	}
	if driver_error != '' {
		failures << 'driver: ${driver_error}'
	}
	if !first_stop_error.contains('dynamic texture recovery injection') {
		failures << 'terminal error was not replayed by first stop: ${first_stop_error}'
	}
	if second_stop_error != first_stop_error {
		failures << 'terminal error changed between repeated stops: first=`${first_stop_error}` second=`${second_stop_error}`'
	}
	recovery_trace_error := validate_dynamic_texture_recovery_trace(recovery_trace)
	if recovery_trace_error != '' {
		failures << recovery_trace_error
	}
	if failures.len > 0 {
		return error(failures.join('; '))
	}
	assert state.app_initialized
	assert state.app_cleaned
	assert state.trace_installed
	assert state.trace_uninstalled
	assert state.texture_updates == 4
	assert state.resource_batches == 4
	assert state.replacements == 1
	assert state.dependency_rejections == 1
	assert state.invalid_replacement_rejections == 1
	assert state.duplicate_update_rejections == 1
	for window in [main_window, tool_window] {
		key := window.str()
		assert state.frames[key] == 3
		assert state.cleanup_submitted[key] == state.rendered_frames[key]
	}
	run_app_resource_only_identity_probe(backend, state.texture)!
	run_cleanup_only_probe(backend)!
	run_partial_app_init_probe(backend)!
	println('{"probe":"dynamic_texture_consumer","status":"PASS","cleanup":"complete"}')
}

fn run_app_resource_only_identity_probe(backend gg.MultiWindowBackend, stale_image gg.WindowImageId) ! {
	frame_ready := chan bool{cap: 1}
	driver_result := chan string{cap: 1}
	mut state := &AppResourceOnlyProbeState{}
	mut app := gg.new_app(backend: backend, queue_size: 4, require_renderer: true)!
	driver := spawn drive_app_resource_only_probe(mut app, frame_ready, driver_result)
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut state] (mut resources gg.AppResourceContext) ! {
			desc := gfx.ImageDesc{
				width:        2
				height:       2
				usage:        .dynamic
				pixel_format: .rgba8
			}
			state.current_image = resources.make_image(&desc)!
			state.init_calls++
		}
		app_resource_frame_fn:   fn [stale_image, frame_ready, mut state] (mut resources gg.AppResourceContext) ! {
			if state.frame_calls != 0 {
				return error('app-resource-only callback ran again before its accepted stop')
			}
			pixels := [u8(1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
			mut data := gfx.ImageData{}
			data.subimage[0][0] = gfx.Range{
				ptr:  pixels.data
				size: usize(pixels.len)
			}
			mut rejected := false
			resources.update_image(stale_image, &data) or {
				if err.msg() != dynamic_texture_resource_scope_error {
					return err
				}
				rejected = true
			}
			if !rejected {
				return error('resource id from a stopped App resolved in a new App')
			}
			state.identity_rejected = true
			state.frame_calls++
			frame_ready <- true
		}
		app_resource_cleanup_fn: fn [mut state] (mut resources gg.AppResourceContext) ! {
			resources.retire_image(state.current_image)!
			state.cleanup_calls++
		}
		max_pending_jobs:        1
	) or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	mut stop_error := ''
	app.stop() or { stop_error = err.msg() }
	if run_error != '' || driver_error != '' || stop_error != '' {
		return error('app-resource-only phase: run=`${run_error}` driver=`${driver_error}` stop=`${stop_error}`')
	}
	assert state.init_calls == 1
	assert state.frame_calls == 1
	assert state.cleanup_calls == 1
	assert state.identity_rejected
}

fn drive_app_resource_only_probe(mut app gg.App, frame_ready chan bool, result chan string) {
	select {
		_ := <-frame_ready {}
		dynamic_texture_probe_watchdog {
			result <- 'watchdog expired waiting for app-resource-only frame'
			return
		}
	}
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	}) or {
		result <- 'app-resource-only stop admission failed: ${err.msg()}'
		return
	}
	result <- ''
}

fn run_cleanup_only_probe(backend gg.MultiWindowBackend) ! {
	owner_barrier := chan bool{cap: 1}
	driver_result := chan string{cap: 1}
	mut state := &CleanupOnlyProbeState{}
	mut app := gg.new_app(backend: backend, queue_size: 4, require_renderer: true)!
	driver := spawn drive_cleanup_only_probe(mut app, owner_barrier, driver_result)
	mut run_error := ''
	app.run(
		event_fn:                fn (event gg.WindowEvent, mut app gg.App) ! {
			_ = event
			_ = app.capabilities()
		}
		app_resource_cleanup_fn: fn [mut state] (mut resources gg.AppResourceContext) ! {
			_ = resources
			state.cleanup_calls++
		}
		max_pending_jobs:        1
	) or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	mut stop_error := ''
	app.stop() or { stop_error = err.msg() }
	if run_error != '' || driver_error != '' || stop_error != '' {
		return error('cleanup-only phase: run=`${run_error}` driver=`${driver_error}` stop=`${stop_error}`')
	}
	assert state.cleanup_calls == 1
}

fn drive_cleanup_only_probe(mut app gg.App, owner_barrier chan bool, result chan string) {
	app.post(fn [owner_barrier] (mut app gg.App) ! {
		_ = app.capabilities()
		owner_barrier <- true
	}) or {
		result <- 'cleanup-only barrier admission failed: ${err.msg()}'
		return
	}
	select {
		_ := <-owner_barrier {}
		dynamic_texture_probe_watchdog {
			result <- 'watchdog expired waiting for cleanup-only owner barrier'
			return
		}
	}
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	}) or {
		result <- 'cleanup-only stop admission failed: ${err.msg()}'
		return
	}
	result <- ''
}

fn run_partial_app_init_probe(backend gg.MultiWindowBackend) ! {
	mut state := &PartialInitProbeState{}
	mut app := gg.new_app(backend: backend, queue_size: 4, require_renderer: true)!
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut state] (mut resources gg.AppResourceContext) ! {
			desc := gfx.ImageDesc{
				width:        2
				height:       2
				usage:        .dynamic
				pixel_format: .rgba8
			}
			state.partial_image = resources.make_image(&desc)!
			state.init_calls++
			return error(dynamic_texture_partial_init_injection)
		}
		app_resource_cleanup_fn: fn [mut state] (mut resources gg.AppResourceContext) ! {
			resources.retire_image(state.partial_image)!
			state.cleanup_calls++
		}
	) or { run_error = err.msg() }
	assert run_error.contains(dynamic_texture_partial_init_injection), 'partial app init did not return its injected failure: ${run_error}'

	mut first_stop_error := ''
	app.stop() or { first_stop_error = err.msg() }
	mut second_stop_error := ''
	app.stop() or { second_stop_error = err.msg() }
	assert first_stop_error == second_stop_error, 'partial-init terminal result changed: first=`${first_stop_error}` second=`${second_stop_error}`'

	assert first_stop_error.contains(dynamic_texture_partial_init_injection), 'partial-init terminal failure was not replayed by stop: ${first_stop_error}'

	assert state.init_calls == 1
	assert state.cleanup_calls == 1
}

fn update_dynamic_texture(mut resources gg.AppResourceContext, mut state DynamicTextureProbeState) ! {
	pixels := [
		u8(255),
		0,
		0,
		255,
		0,
		255,
		0,
		255,
		0,
		0,
		255,
		255,
		255,
		255,
		255,
		255,
		u8(255),
		255,
		255,
		255,
		0,
		0,
		255,
		255,
		0,
		255,
		0,
		255,
		255,
		0,
		0,
		255,
		u8(255),
		0,
		0,
		255,
		0,
		255,
		0,
		255,
		0,
		0,
		255,
		255,
		255,
		255,
		255,
		255,
		u8(255),
		255,
		255,
		255,
		0,
		0,
		255,
		255,
		0,
		255,
		0,
		255,
		255,
		0,
		0,
		255,
	]
	mut data := gfx.ImageData{}
	data.subimage[0][0] = gfx.Range{
		ptr:  pixels.data
		size: usize(pixels.len)
	}
	resources.update_image(state.texture, &data)!
	state.texture_updates++
}

fn dynamic_texture_render_target_desc(width int, height int) gfx.ImageDesc {
	return gfx.ImageDesc{
		render_target: true
		width:         width
		height:        height
		pixel_format:  .rgba8
		sample_count:  1
	}
}

fn arm_dynamic_texture_initial_frames(mut state DynamicTextureProbeState, windows []gg.WindowId) u64 {
	state.phase_generation++
	generation := state.phase_generation
	for window in windows {
		state.armed_generation[window.str()] = generation
	}
	return generation
}

fn arm_dynamic_texture_trace_phase(mut app gg.App, mut state DynamicTextureProbeState, windows []gg.WindowId) !u64 {
	multiwindow_sokol_trace.reset()
	state.phase_generation++
	generation := state.phase_generation
	for window in windows {
		state.armed_generation[window.str()] = generation
	}
	state.snapshot_armed_generation = generation
	state.resource_batch_armed = true
	for window in windows {
		app.request_redraw(window)!
	}
	return generation
}

fn draw_dynamic_texture_probe(mut context gg.WindowContext, frames chan DynamicTextureProbeFrame, trace_snapshot chan []string, mut state DynamicTextureProbeState) ! {
	assert state.app_initialized
	info := context.frame_info()
	key := info.window.str()
	generation := state.phase_generation
	window_armed := generation != 0 && state.armed_generation[key] == generation
	accept_frame := window_armed && state.accepted_generation[key] != generation
	if info.window == state.main_window {
		context.with_offscreen(gg.WindowOffscreenPassConfig{
			attachments: state.attachments
			action:      gfx.create_clear_pass_action(0, 0, 0, 0)
		}, fn (mut pass gg.WindowPassContext) ! {
			_ = pass
		})!
	}
	image := state.texture
	sampler := state.sampler
	action := gfx.create_clear_pass_action(0.08, 0.1, 0.12, 1)
	context.with_swapchain_sgl(action, fn [info, image, sampler] (mut drawing gg.WindowSglContext) ! {
		width := info.metrics.logical_size.width
		height := info.metrics.logical_size.height
		drawing.defaults()
		drawing.matrix_mode_projection()
		drawing.load_identity()
		drawing.ortho(0, width, height, 0, -1, 1)
		drawing.enable_texture()
		drawing.texture(image, sampler)!
		drawing.begin_quads()
		drawing.v2f_t2f_c4b(width * 0.1, height * 0.15, 0, 0, 255, 255, 255, 255)
		drawing.v2f_t2f_c4b(width * 0.9, height * 0.15, 1, 0, 255, 255, 255, 255)
		drawing.v2f_t2f_c4b(width * 0.9, height * 0.85, 1, 1, 255, 255, 255, 255)
		drawing.v2f_t2f_c4b(width * 0.1, height * 0.85, 0, 1, 255, 255, 255, 255)
		drawing.end()
		drawing.disable_texture()
	})!
	state.rendered_frames[key]++
	if !accept_frame {
		return
	}
	state.accepted_generation[key] = generation
	state.frames[key]++
	main_accepted := state.accepted_generation[state.main_window.str()] == generation
	tool_accepted := state.accepted_generation[state.tool_window.str()] == generation
	if generation == state.snapshot_armed_generation
		&& state.snapshot_posted_generation != generation && main_accepted && tool_accepted {
		state.app.post(fn [trace_snapshot] (mut app gg.App) ! {
			_ = app
			trace_snapshot <- multiwindow_sokol_trace.snapshot()
		})!
		state.snapshot_posted_generation = generation
	}
	frames <- DynamicTextureProbeFrame{
		window:     key
		serial:     info.frame_serial
		generation: generation
	}
}

fn drive_dynamic_texture_probe(mut app gg.App, main_window gg.WindowId, tool_window gg.WindowId, frames chan DynamicTextureProbeFrame, mut state DynamicTextureProbeState, trace_ack chan string, trace_snapshot chan []string, initial_generation u64, result chan string) {
	windows := [main_window.str(), tool_window.str()]
	wait_for_dynamic_texture_probe_barrier(frames, windows, initial_generation) or {
		fail_dynamic_texture_probe(mut app, 'first frame barrier: ${err.msg()}', result)
		return
	}
	preserved_graph_generation := initial_generation + 1
	app.post(fn [main_window, tool_window, trace_ack, mut state] (mut app gg.App) ! {
		generation := arm_dynamic_texture_trace_phase(mut app, mut state, [
			main_window,
			tool_window,
		])!
		trace_ack <- 'phase_2_armed:${generation}'
	}) or {
		fail_dynamic_texture_probe(mut app, 'phase 2 admission: ${err.msg()}', result)
		return
	}
	wait_for_dynamic_texture_trace_ack(trace_ack, 'phase_2_armed:${preserved_graph_generation}') or {
		fail_dynamic_texture_probe(mut app, 'phase 2 acknowledgement: ${err.msg()}', result)
		return
	}
	wait_for_dynamic_texture_probe_barrier(frames, windows, preserved_graph_generation) or {
		fail_dynamic_texture_probe(mut app, 'second frame barrier: ${err.msg()}', result)
		return
	}
	invalid_trace := wait_for_dynamic_texture_trace_snapshot(trace_snapshot) or {
		fail_dynamic_texture_probe(mut app, 'invalid replacement trace snapshot: ${err.msg()}',
			result)
		return
	}
	trace_error := validate_dynamic_texture_preserved_graph_trace(invalid_trace)
	if trace_error != '' {
		fail_dynamic_texture_probe(mut app, trace_error, result)
		return
	}
	replacement_generation := preserved_graph_generation + 1
	app.post(fn [main_window, tool_window, trace_ack, mut state] (mut app gg.App) ! {
		generation := arm_dynamic_texture_trace_phase(mut app, mut state, [
			main_window,
			tool_window,
		])!
		trace_ack <- 'phase_3_armed:${generation}'
	}) or {
		fail_dynamic_texture_probe(mut app, 'phase 3 admission: ${err.msg()}', result)
		return
	}
	wait_for_dynamic_texture_trace_ack(trace_ack, 'phase_3_armed:${replacement_generation}') or {
		fail_dynamic_texture_probe(mut app, 'phase 3 acknowledgement: ${err.msg()}', result)
		return
	}
	wait_for_dynamic_texture_probe_barrier(frames, windows, replacement_generation) or {
		fail_dynamic_texture_probe(mut app, 'replacement frame barrier: ${err.msg()}', result)
		return
	}
	replacement_trace := wait_for_dynamic_texture_trace_snapshot(trace_snapshot) or {
		fail_dynamic_texture_probe(mut app, 'replacement trace snapshot: ${err.msg()}', result)
		return
	}
	replacement_error := validate_dynamic_texture_replacement_trace(replacement_trace)
	if replacement_error != '' {
		fail_dynamic_texture_probe(mut app, replacement_error, result)
		return
	}
	app.post(fn [main_window, tool_window, mut state] (mut app gg.App) ! {
		app.destroy_window(tool_window)!
		app.request_redraw(main_window)!
		state.resource_batch_armed = true
		multiwindow_sokol_trace.reset()
		state.recovery_requested = true
	}) or {
		fail_dynamic_texture_probe(mut app, 'recovery batch admission: ${err.msg()}', result)
		return
	}
	result <- ''
}

fn validate_dynamic_texture_recovery_trace(trace []string) string {
	if trace.count(it == 'update_image') != 1 {
		return 'recovery batch expected exactly one texture update: ${trace}'
	}
	for forbidden in ['make_image', 'make_attachments'] {
		if forbidden in trace {
			return 'recovery batch unexpectedly created a resource via `${forbidden}`: ${trace}'
		}
	}
	if trace.count(it == 'commit') < 1 {
		return 'recovery batch did not commit its texture update: ${trace}'
	}
	update_index := trace.index('update_image')
	mut commit_after_update := false
	for index, operation in trace {
		if operation == 'commit' && index > update_index {
			commit_after_update = true
			break
		}
	}
	if update_index < 0 || !commit_after_update {
		return 'recovery texture update was not followed by a commit: ${trace}'
	}
	return ''
}

fn validate_dynamic_texture_preserved_graph_trace(trace []string) string {
	if trace.count(it == 'update_image') != 1 {
		return 'invalid replacement batch expected exactly one upload: ${trace}'
	}
	if !dynamic_texture_updates_have_later_commit(trace) {
		return 'invalid replacement batch upload was not followed by a commit: ${trace}'
	}
	for forbidden in ['make_image', 'make_attachments', 'destroy_attachments', 'destroy_image'] {
		if forbidden in trace {
			return 'invalid replacement changed the live graph via `${forbidden}`: ${trace}'
		}
	}
	structure_error := validate_dynamic_texture_render_trace_structure(trace, false)
	if structure_error != '' {
		return 'invalid replacement batch ${structure_error}'
	}
	if trace.count(it == 'begin_offscreen_pass') < 1
		|| trace.count(it == 'begin_swapchain_pass') < 2 {
		return 'old attachment graph expected at least one offscreen and at least two swapchain passes after invalid replacement: ${trace}'
	}
	return ''
}

fn validate_dynamic_texture_replacement_trace(trace []string) string {
	if trace.count(it == 'update_image') != 1 {
		return 'replacement batch expected one app-scoped texture upload, received ${trace}'
	}
	if !dynamic_texture_updates_have_later_commit(trace) {
		return 'replacement batch upload was not followed by a commit: ${trace}'
	}
	if trace.count(it == 'make_image') != 1 || trace.count(it == 'make_attachments') != 1
		|| trace.count(it == 'destroy_attachments') != 1 || trace.count(it == 'destroy_image') != 1 {
		return 'replacement batch expected one graph create/retire operation each: ${trace}'
	}
	structure_error := validate_dynamic_texture_render_trace_structure(trace, true)
	if structure_error != '' {
		return 'replacement batch ${structure_error}'
	}
	make_image := int(trace.index('make_image'))
	make_attachments := int(trace.index('make_attachments'))
	first_commit := int(trace.index('commit'))
	destroy_attachments := int(trace.index('destroy_attachments'))
	destroy_image := int(trace.index('destroy_image'))
	if make_image < 0 || make_attachments < 0 || first_commit < 0 || destroy_attachments < 0
		|| destroy_image < 0 {
		return 'replacement batch omitted a required graph operation: ${trace}'
	}
	if !(make_image < make_attachments && make_attachments < first_commit
		&& first_commit < destroy_attachments && destroy_attachments < destroy_image) {
		return 'replacement graph order is invalid: ${trace}'
	}
	if trace.count(it == 'begin_offscreen_pass') < 1
		|| trace.count(it == 'begin_swapchain_pass') < 2 {
		return 'replacement batch expected at least one offscreen and at least two swapchain passes: ${trace}'
	}
	return ''
}

fn dynamic_texture_updates_have_later_commit(trace []string) bool {
	mut update_pending := false
	for operation in trace {
		if operation == 'update_image' {
			update_pending = true
		} else if operation == 'commit' {
			update_pending = false
		}
	}
	return !update_pending
}

fn validate_dynamic_texture_render_trace_structure(trace []string, allow_graph_changes bool) string {
	mut open_pass := ''
	mut completed_since_commit := 0
	mut commit_count := 0
	for operation in trace {
		match operation {
			'update_image' {}
			'make_image', 'make_attachments', 'destroy_attachments', 'destroy_image' {
				if !allow_graph_changes {
					return 'contains forbidden graph operation `${operation}`: ${trace}'
				}
			}
			'begin_offscreen_pass', 'begin_swapchain_pass' {
				if open_pass != '' {
					return 'opens `${operation}` before closing `${open_pass}`: ${trace}'
				}
				open_pass = operation
			}
			'update_buffer' {
				if open_pass == '' {
					return 'contains update_buffer outside a render pass: ${trace}'
				}
			}
			'end_pass' {
				if open_pass == '' {
					return 'contains end_pass without an open render pass: ${trace}'
				}
				open_pass = ''
				completed_since_commit++
			}
			'commit' {
				if open_pass != '' {
					return 'commits while `${open_pass}` remains open: ${trace}'
				}
				if completed_since_commit == 0 {
					return 'commits without a newly completed render pass: ${trace}'
				}
				commit_count++
				completed_since_commit = 0
			}
			else {
				return 'contains unsupported operation `${operation}`: ${trace}'
			}
		}
	}
	if open_pass != '' {
		return 'leaves `${open_pass}` without end_pass: ${trace}'
	}
	if commit_count == 0 {
		return 'does not contain a commit: ${trace}'
	}
	if completed_since_commit != 0 {
		return 'leaves completed render passes without a commit: ${trace}'
	}
	return ''
}

fn wait_for_dynamic_texture_trace_ack(trace_ack chan string, expected string) ! {
	select {
		value := <-trace_ack {
			if value != expected {
				return error('expected `${expected}`, received `${value}`')
			}
		}
		dynamic_texture_probe_watchdog {
			return error('watchdog expired waiting for owner trace acknowledgement')
		}
	}
}

fn wait_for_dynamic_texture_trace_snapshot(trace_snapshot chan []string) ![]string {
	mut snapshot := []string{}
	select {
		value := <-trace_snapshot {
			snapshot = value.clone()
		}
		dynamic_texture_probe_watchdog {
			return error('watchdog expired waiting for owner trace snapshot')
		}
	}
	return snapshot
}

fn wait_for_dynamic_texture_probe_barrier(frames chan DynamicTextureProbeFrame, windows []string, generation u64) ! {
	if generation == 0 {
		return error('dynamic texture frame generation was not armed')
	}
	mut seen := map[string]bool{}
	for seen.len < windows.len {
		select {
			signal := <-frames {
				if signal.window !in windows || signal.generation != generation {
					continue
				}
				seen[signal.window] = true
			}
			dynamic_texture_probe_watchdog {
				return error('watchdog expired with ${seen.len}/${windows.len} windows ready for generation ${generation}')
			}
		}
	}
}

fn fail_dynamic_texture_probe(mut app gg.App, message string, result chan string) {
	post_dynamic_texture_probe_stop(mut app) or {
		result <- '${message}; owner stop admission failed: ${err.msg()}'
		return
	}
	result <- message
}

fn post_dynamic_texture_probe_stop(mut app gg.App) ! {
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	})!
}
