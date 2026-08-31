module main

import gg
import sokol.gfx
import time
import gg.testdata.multiwindow_probe_backend
import gg.testdata.multiwindow_probe_gate
import gg.testdata.multiwindow_sokol_trace

#flag -DSOKOL_TRACE_HOOKS

const retained_ui_probe_watchdog = 5 * time.second
const retained_ui_stale_lease_error = 'gg.multiwindow: render callback lease has expired'
const retained_ui_owner_thread_error = 'gg.multiwindow: render operation requires the app owner thread'
const retained_ui_resource_scope_error = 'gg.multiwindow: render resource belongs to a different window or app'
const retained_ui_stale_resource_error = 'gg.multiwindow: render resource handle is stale'
const retained_ui_app_cleanup_injection = 'retained UI app cleanup injection'
const retained_ui_readback_unsupported_error = 'gg.multiwindow: requested readback is not supported'
const retained_ui_wrong_phase_error = 'gg.multiwindow: operation is not valid in this render phase'
const retained_ui_resource_after_sgl_error = 'gg.multiwindow: resources cannot be mutated after the first SGL flush in a frame'

enum RetainedUiProbePhase {
	initial
	trace
	tool_teardown
	main_continuation
}

struct RetainedUiProbeFrame {
	window     string
	serial     u64
	generation u64
}

struct RetainedUiProbePhaseAck {
	phase      RetainedUiProbePhase
	generation u64
}

struct RetainedUiProbeState {
mut:
	app                            &gg.App = unsafe { nil }
	main_window                    gg.WindowId
	tool_window                    gg.WindowId
	accepted_frames                map[string]u64
	rendered_frames                map[string]u64
	armed_generation               map[string]u64
	accepted_generation            map[string]u64
	phase                          RetainedUiProbePhase
	phase_generation               u64
	cleanup_submitted              map[string]u64
	cleanup_order                  []string
	trace_installed                bool
	trace_uninstalled              bool
	trace_snapshot_captured        bool
	init_lease                     gg.WindowInitContext
	frame_lease                    gg.WindowContext
	resource_lease                 gg.WindowResourceContext
	pass_lease                     gg.WindowPassContext
	sgl_lease                      gg.WindowSglContext
	cleanup_lease                  gg.WindowCleanupContext
	app_init_lease                 gg.AppResourceContext
	app_frame_lease                gg.AppResourceContext
	app_frame_lease_captured       bool
	app_cleanup_lease              gg.AppResourceContext
	stale_primary_validation_armed bool
	main_buffer                    gg.WindowBufferId
	main_readback_image            gg.WindowImageId
	tool_buffer                    gg.WindowBufferId
	cross_window_rejections        u64
	destroyed_resource_rejections  u64
	app_cleanup_calls              u64
	offscreen_readback_checks      u64
	window_capture_checks          u64
	phase_rejections               u64
}

fn main() {
	run_retained_ui_probe() or { panic(err) }
}

fn run_retained_ui_probe() ! {
	multiwindow_probe_gate.await_parent_release(retained_ui_probe_watchdog)!
	frames := chan RetainedUiProbeFrame{cap: 12}
	driver_result := chan string{cap: 1}
	trace_ack := chan string{cap: 1}
	trace_snapshot := chan []string{cap: 1}
	phase_ack := chan RetainedUiProbePhaseAck{cap: 1}
	mut state := &RetainedUiProbeState{}
	backend := multiwindow_probe_backend.selected()!
	mut app := gg.new_app(backend: backend, queue_size: 16, require_renderer: true)!
	multiwindow_probe_backend.validate(app.capabilities(), backend)!
	state.app = app
	main_window := app.create_window(
		title:       'retained UI consumer: main'
		width:       560
		height:      360
		redraw_mode: .on_demand
		init_fn:     fn [mut state] (mut context gg.WindowInitContext) ! {
			state.init_lease = context
			context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
				desc := gfx.BufferDesc{
					size:  16
					usage: .dynamic
				}
				state.main_buffer = resources.make_buffer(&desc)!
				readback_desc := gfx.ImageDesc{
					render_target: true
					width:         1
					height:        1
					pixel_format:  .rgba8
					sample_count:  1
				}
				state.main_readback_image = resources.make_image(&readback_desc)!
			})!
		}
		frame_fn:    fn [frames, trace_snapshot, mut state] (mut context gg.WindowContext) ! {
			draw_retained_ui_probe(mut context, gg.rgb(40, 145, 185), frames, trace_snapshot, mut
				state)!
		}
		cleanup_fn:  fn [mut state] (mut context gg.WindowCleanupContext) ! {
			key := context.window_id().str()
			context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
				resources.retire_image(state.main_readback_image)!
				resources.retire_buffer(state.main_buffer)!
			})!
			state.cleanup_submitted[key] = context.metrics().submitted_frame
			state.cleanup_order << key
		}
	)!
	state.main_window = main_window
	tool_window := app.create_window(
		title:       'retained UI consumer: tool'
		width:       320
		height:      240
		redraw_mode: .on_demand
		init_fn:     fn [mut state] (mut context gg.WindowInitContext) ! {
			context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
				desc := gfx.BufferDesc{
					size:  16
					usage: .dynamic
				}
				state.tool_buffer = resources.make_buffer(&desc)!
			})!
		}
		frame_fn:    fn [frames, trace_snapshot, mut state] (mut context gg.WindowContext) ! {
			draw_retained_ui_probe(mut context, gg.rgb(210, 92, 52), frames, trace_snapshot, mut
				state)!
		}
		cleanup_fn:  fn [mut state] (mut context gg.WindowCleanupContext) ! {
			key := context.window_id().str()
			context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
				desc := gfx.BufferDesc{
					size:  16
					usage: .dynamic
				}
				mut rejected := false
				_ := resources.make_buffer(&desc) or {
					if err.msg() != retained_ui_wrong_phase_error {
						return err
					}
					rejected = true
					gg.WindowBufferId{}
				}
				if !rejected {
					return error('window cleanup created a new resource')
				}
				state.phase_rejections++
				resources.retire_buffer(state.tool_buffer)!
			})!
			state.cleanup_submitted[key] = context.metrics().submitted_frame
			state.cleanup_order << key
			state.cleanup_lease = context
		}
	)!
	state.tool_window = tool_window
	initial_generation := arm_retained_ui_initial_phase(mut state, [main_window, tool_window])

	driver := spawn drive_retained_ui_probe(mut app, main_window, tool_window, frames, mut state,
		trace_ack, trace_snapshot, phase_ack, initial_generation, driver_result)
	mut run_error := ''
	app.run(
		app_resource_init_fn:    fn [mut state] (mut resources gg.AppResourceContext) ! {
			state.app_init_lease = resources
			multiwindow_sokol_trace.install()!
			state.trace_installed = true
		}
		app_resource_frame_fn:   fn [trace_ack, mut state] (mut resources gg.AppResourceContext) ! {
			mut validation_ack := ''
			if state.stale_primary_validation_armed {
				state.stale_primary_validation_armed = false
				validation_ack = validate_retained_ui_stale_primary_leases(mut state)
			}
			if !state.app_frame_lease_captured {
				state.app_frame_lease = resources
				state.app_frame_lease_captured = true
			}
			if validation_ack != '' {
				trace_ack <- validation_ack
			}
		}
		app_resource_cleanup_fn: fn [mut state] (mut resources gg.AppResourceContext) ! {
			state.app_cleanup_lease = resources
			state.cleanup_order << 'app'
			desc := gfx.ImageDesc{
				width:  1
				height: 1
			}
			mut rejected := false
			_ := resources.make_image(&desc) or {
				if err.msg() != retained_ui_wrong_phase_error {
					return err
				}
				rejected = true
				gg.WindowImageId{}
			}
			if !rejected {
				return error('app cleanup created a new resource')
			}
			state.phase_rejections++
			multiwindow_sokol_trace.uninstall()
			state.trace_uninstalled = true
			state.app_cleanup_calls++
			return error(retained_ui_app_cleanup_injection)
		}
		event_fn:                fn (event gg.WindowEvent, mut app gg.App) ! {
			if event.kind == .window_close_requested && app.window_exists(event.window) {
				app.destroy_window(event.window)!
			}
		}
	) or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	mut stop_error := ''
	app.stop() or { stop_error = err.msg() }

	mut failures := []string{}
	if !run_error.contains(retained_ui_app_cleanup_injection) {
		failures << 'owner loop did not return the app cleanup injection: ${run_error}'
	}
	if driver_error != '' {
		failures << 'driver: ${driver_error}'
	}
	if stop_error != run_error {
		failures << 'repeated stop did not replay terminal cleanup outcome: run=`${run_error}` stop=`${stop_error}`'
	}
	if failures.len > 0 {
		return error(failures.join('; '))
	}
	main_key := main_window.str()
	tool_key := tool_window.str()
	assert state.accepted_frames[main_key] == 3
	assert state.accepted_frames[tool_key] == 3
	assert state.cleanup_submitted[tool_key] == state.rendered_frames[tool_key]
	assert state.cleanup_submitted[main_key] == state.rendered_frames[main_key]
	assert state.cleanup_order == [tool_key, main_key, 'app']
	assert state.trace_installed
	assert state.trace_uninstalled
	assert state.cross_window_rejections == 1
	assert state.destroyed_resource_rejections == 1
	assert state.app_cleanup_calls == 1
	assert state.offscreen_readback_checks == 1
	assert state.window_capture_checks == 1
	assert state.phase_rejections == 3
	assert validate_retained_ui_stale_app_cleanup_lease(mut state) == ''
	foreign_cleanup_result := chan string{cap: 1}
	foreign_cleanup := spawn fn [mut state, foreign_cleanup_result] () {
		mut lease := state.app_cleanup_lease
		lease.retire_buffer(gg.WindowBufferId{}) or {
			foreign_cleanup_result <- err.msg()
			return
		}
		foreign_cleanup_result <- 'accepted'
	}()
	assert <-foreign_cleanup_result == retained_ui_owner_thread_error
	foreign_cleanup.wait()
	println('{"probe":"retained_ui_consumer","status":"PASS","cleanup":"complete"}')
}

fn arm_retained_ui_initial_phase(mut state RetainedUiProbeState, windows []gg.WindowId) u64 {
	state.phase_generation++
	state.phase = .initial
	generation := state.phase_generation
	for window in windows {
		state.armed_generation[window.str()] = generation
	}
	return generation
}

fn arm_retained_ui_probe_phase(mut app gg.App, mut state RetainedUiProbeState, phase RetainedUiProbePhase, windows []gg.WindowId) !u64 {
	if phase == .initial {
		return error('initial retained UI phase cannot be rearmed')
	}
	if phase == .trace {
		multiwindow_sokol_trace.reset()
		state.trace_snapshot_captured = false
	}
	state.phase_generation++
	state.phase = phase
	generation := state.phase_generation
	for window in windows {
		state.armed_generation[window.str()] = generation
	}
	if phase == .trace {
		state.stale_primary_validation_armed = true
	}
	for window in windows {
		app.request_redraw(window)!
	}
	return generation
}

fn draw_retained_ui_probe(mut context gg.WindowContext, accent gg.Color, frames chan RetainedUiProbeFrame, trace_snapshot chan []string, mut state RetainedUiProbeState) ! {
	info := context.frame_info()
	key := info.window.str()
	generation := state.phase_generation
	phase := state.phase
	accept_frame := generation != 0 && state.armed_generation[key] == generation
		&& state.accepted_generation[key] != generation
	if accept_frame && phase == .initial && info.window == state.main_window {
		state.frame_lease = context
		context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
			state.resource_lease = resources
		})!
		capabilities := state.app.window_readback_capabilities(info.window)!
		if capabilities.offscreen_image {
			context.request_image_readback(state.main_readback_image, gg.WindowReadbackConfig{})!
		} else {
			if _ := context.request_image_readback(state.main_readback_image, gg.WindowReadbackConfig{}) {
				return error('offscreen readback unexpectedly succeeded without capability')
			} else {
				if err.msg() != retained_ui_readback_unsupported_error {
					return err
				}
			}
		}
		state.offscreen_readback_checks++
	}
	if accept_frame && phase == .initial && info.window == state.tool_window {
		context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
			data_bytes := [u8(1), 2, 3, 4]
			data := gfx.Range{
				ptr:  data_bytes.data
				size: usize(data_bytes.len)
			}
			mut rejected := false
			resources.update_buffer(state.main_buffer, &data) or {
				if err.msg() != retained_ui_resource_scope_error {
					return err
				}
				rejected = true
			}
			if !rejected {
				return error('tool window updated a main-window resource')
			}
			state.cross_window_rejections++
		})!
	}
	if accept_frame && phase == .main_continuation && info.window == state.main_window {
		context.with_resources(fn [mut state] (mut resources gg.WindowResourceContext) ! {
			mut rejected := false
			resources.retire_buffer(state.tool_buffer) or {
				if err.msg() != retained_ui_stale_resource_error {
					return err
				}
				rejected = true
			}
			if !rejected {
				return error('destroyed tool-window resource remained live')
			}
			state.destroyed_resource_rejections++
		})!
	}
	action := gfx.create_clear_pass_action(0.12, 0.13, 0.14, 1)
	if info.window == state.main_window {
		context.with_swapchain(action, fn [accept_frame, phase, mut state] (mut pass gg.WindowPassContext) ! {
			if accept_frame && phase == .initial {
				state.pass_lease = pass
			}
		})!
	} else {
		context.with_swapchain_sgl(action, fn [info, accent, accept_frame, phase, mut state] (mut drawing gg.WindowSglContext) ! {
			if accept_frame && phase == .initial {
				state.sgl_lease = drawing
			}
			width := info.metrics.logical_size.width
			height := info.metrics.logical_size.height
			drawing.defaults()
			drawing.matrix_mode_projection()
			drawing.load_identity()
			drawing.ortho(0, width, height, 0, -1, 1)
			drawing.c4b(accent.r, accent.g, accent.b, accent.a)
			drawing.begin_quads()
			drawing.v2f(width * 0.1, height * 0.1)
			drawing.v2f(width * 0.9, height * 0.1)
			drawing.v2f(width * 0.9, height * 0.9)
			drawing.v2f(width * 0.1, height * 0.9)
			drawing.end()
		})!
		if accept_frame && phase == .initial {
			mut rejected := false
			context.with_resources(fn (mut resources gg.WindowResourceContext) ! {
				_ = resources
			}) or {
				if err.msg() != retained_ui_resource_after_sgl_error {
					return err
				}
				rejected = true
			}
			if !rejected {
				return error('resource section opened after an SGL flush')
			}
			state.phase_rejections++
		}
	}
	state.rendered_frames[key]++
	if !accept_frame {
		return
	}
	state.accepted_generation[key] = generation
	state.accepted_frames[key]++
	main_accepted := state.accepted_generation[state.main_window.str()] == generation
	tool_accepted := state.accepted_generation[state.tool_window.str()] == generation
	if phase == .trace && !state.trace_snapshot_captured && main_accepted && tool_accepted {
		state.app.post(fn [trace_snapshot] (mut app gg.App) ! {
			_ = app
			trace_snapshot <- multiwindow_sokol_trace.snapshot()
		})!
		state.trace_snapshot_captured = true
	}
	frames <- RetainedUiProbeFrame{
		window:     key
		serial:     info.frame_serial
		generation: generation
	}
	match phase {
		.tool_teardown {
			if info.window == state.tool_window {
				state.app.destroy_window(info.window)!
			}
		}
		.main_continuation {
			if info.window == state.main_window {
				state.app.stop()!
			}
		}
		else {}
	}
}

fn drive_retained_ui_probe(mut app gg.App, main_window gg.WindowId, tool_window gg.WindowId, frames chan RetainedUiProbeFrame, mut state RetainedUiProbeState, trace_ack chan string, trace_snapshot chan []string, phase_ack chan RetainedUiProbePhaseAck, initial_generation u64, result chan string) {
	windows := [main_window.str(), tool_window.str()]
	wait_for_retained_ui_phase_barrier(frames, windows, initial_generation) or {
		fail_retained_ui_probe(mut app, 'first frame barrier: ${err.msg()}', result)
		return
	}
	foreign_primary := validate_retained_ui_foreign_primary_leases(mut state)
	if foreign_primary != 'foreign-ok' {
		fail_retained_ui_probe(mut app, foreign_primary, result)
		return
	}
	app.post(fn [main_window, trace_ack, mut state] (mut app gg.App) ! {
		capabilities := app.window_readback_capabilities(main_window)!
		if capabilities.window_capture {
			app.request_window_capture(main_window, gg.WindowReadbackConfig{}) or {
				trace_ack <- err.msg()
				return
			}
		} else {
			mut rejected := false
			app.request_window_capture(main_window, gg.WindowReadbackConfig{}) or {
				if err.msg() != retained_ui_readback_unsupported_error {
					trace_ack <- err.msg()
					return
				}
				rejected = true
			}
			if !rejected {
				trace_ack <- 'window capture unexpectedly succeeded without capability'
				return
			}
		}
		state.window_capture_checks++
		trace_ack <- 'readback-ok'
	}) or {
		fail_retained_ui_probe(mut app, 'readback validation admission: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_trace_ack(trace_ack, 'readback-ok') or {
		fail_retained_ui_probe(mut app, 'readback validation: ${err.msg()}', result)
		return
	}
	trace_generation := initial_generation + 1
	app.post(fn [main_window, tool_window, phase_ack, mut state] (mut app gg.App) ! {
		generation := arm_retained_ui_probe_phase(mut app, mut state, .trace, [
			main_window,
			tool_window,
		])!
		phase_ack <- RetainedUiProbePhaseAck{
			phase:      .trace
			generation: generation
		}
	}) or {
		fail_retained_ui_probe(mut app, 'trace phase admission: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_phase_ack(phase_ack, .trace, trace_generation) or {
		fail_retained_ui_probe(mut app, 'trace phase acknowledgement: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_trace_ack(trace_ack, 'leases-ok') or {
		fail_retained_ui_probe(mut app, 'stale lease validation: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_phase_barrier(frames, windows, trace_generation) or {
		fail_retained_ui_probe(mut app, 'trace frame barrier: ${err.msg()}', result)
		return
	}
	trace := wait_for_retained_ui_trace_snapshot(trace_snapshot) or {
		fail_retained_ui_probe(mut app, 'trace snapshot handshake: ${err.msg()}', result)
		return
	}
	trace_validation := validate_retained_ui_two_window_trace(trace)
	if trace_validation != '' {
		fail_retained_ui_probe(mut app, trace_validation, result)
		return
	}
	tool_teardown_generation := trace_generation + 1
	app.post(fn [tool_window, phase_ack, mut state] (mut app gg.App) ! {
		generation := arm_retained_ui_probe_phase(mut app, mut state, .tool_teardown, [
			tool_window,
		])!
		phase_ack <- RetainedUiProbePhaseAck{
			phase:      .tool_teardown
			generation: generation
		}
	}) or {
		fail_retained_ui_probe(mut app, 'tool teardown phase admission: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_phase_ack(phase_ack, .tool_teardown, tool_teardown_generation) or {
		fail_retained_ui_probe(mut app, 'tool teardown phase acknowledgement: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_phase_barrier(frames, [tool_window.str()], tool_teardown_generation) or {
		fail_retained_ui_probe(mut app, 'tool teardown frame barrier: ${err.msg()}', result)
		return
	}
	app.post(fn [trace_ack] (mut app gg.App) ! {
		_ = app.capabilities()
		trace_ack <- 'tool-destroyed'
	}) or {
		fail_retained_ui_probe(mut app, 'tool teardown completion admission: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_trace_ack(trace_ack, 'tool-destroyed') or {
		fail_retained_ui_probe(mut app, 'tool teardown completion: ${err.msg()}', result)
		return
	}
	foreign_cleanup := validate_retained_ui_foreign_cleanup_lease(mut state)
	if foreign_cleanup != 'foreign-cleanup-ok' {
		fail_retained_ui_probe(mut app, foreign_cleanup, result)
		return
	}
	app.post(fn [trace_ack, mut state] (mut app gg.App) ! {
		_ = app
		trace_ack <- validate_retained_ui_stale_cleanup_lease(mut state)
	}) or {
		fail_retained_ui_probe(mut app, 'cleanup lease validation admission: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_trace_ack(trace_ack, 'cleanup-ok') or {
		fail_retained_ui_probe(mut app, 'cleanup lease validation: ${err.msg()}', result)
		return
	}
	main_continuation_generation := tool_teardown_generation + 1
	app.post(fn [main_window, phase_ack, mut state] (mut app gg.App) ! {
		generation := arm_retained_ui_probe_phase(mut app, mut state, .main_continuation, [
			main_window,
		])!
		phase_ack <- RetainedUiProbePhaseAck{
			phase:      .main_continuation
			generation: generation
		}
	}) or {
		fail_retained_ui_probe(mut app, 'main continuation phase admission: ${err.msg()}', result)
		return
	}
	wait_for_retained_ui_phase_ack(phase_ack, .main_continuation, main_continuation_generation) or {
		fail_retained_ui_probe(mut app, 'main continuation phase acknowledgement: ${err.msg()}',
			result)
		return
	}
	wait_for_retained_ui_phase_barrier(frames, [main_window.str()], main_continuation_generation) or {
		fail_retained_ui_probe(mut app, 'main continuation barrier: ${err.msg()}', result)
		return
	}
	result <- ''
}

fn validate_retained_ui_stale_primary_leases(mut state RetainedUiProbeState) string {
	mut errors := []string{}
	mut init_lease := state.init_lease
	init_lease.with_resources(fn (mut resources gg.WindowResourceContext) ! {
		_ = resources
	}) or { errors << err.msg() }
	mut frame_lease := state.frame_lease
	frame_lease.with_resources(fn (mut resources gg.WindowResourceContext) ! {
		_ = resources
	}) or { errors << err.msg() }
	mut resource_lease := state.resource_lease
	resource_lease.retire_buffer(gg.WindowBufferId{}) or { errors << err.msg() }
	mut pass_lease := state.pass_lease
	pass_lease.draw(0, 0, 0) or { errors << err.msg() }
	mut sgl_lease := state.sgl_lease
	sgl_lease.load_pipeline(gg.WindowSglPipelineId{}) or { errors << err.msg() }
	mut app_init_lease := state.app_init_lease
	app_init_lease.retire_buffer(gg.WindowBufferId{}) or { errors << err.msg() }
	mut app_frame_lease := state.app_frame_lease
	app_frame_lease.retire_buffer(gg.WindowBufferId{}) or { errors << err.msg() }
	if errors.len != 7 || errors.any(it != retained_ui_stale_lease_error) {
		return 'expected seven expired leases, received ${errors}'
	}
	return 'leases-ok'
}

fn validate_retained_ui_foreign_primary_leases(mut state RetainedUiProbeState) string {
	mut errors := []string{}
	mut init_lease := state.init_lease
	init_lease.with_resources(fn (mut resources gg.WindowResourceContext) ! {
		_ = resources
	}) or { errors << err.msg() }
	mut frame_lease := state.frame_lease
	frame_lease.with_resources(fn (mut resources gg.WindowResourceContext) ! {
		_ = resources
	}) or { errors << err.msg() }
	mut resource_lease := state.resource_lease
	resource_lease.retire_buffer(gg.WindowBufferId{}) or { errors << err.msg() }
	mut pass_lease := state.pass_lease
	pass_lease.draw(0, 0, 0) or { errors << err.msg() }
	mut sgl_lease := state.sgl_lease
	sgl_lease.load_pipeline(gg.WindowSglPipelineId{}) or { errors << err.msg() }
	mut app_init_lease := state.app_init_lease
	app_init_lease.retire_buffer(gg.WindowBufferId{}) or { errors << err.msg() }
	mut app_frame_lease := state.app_frame_lease
	app_frame_lease.retire_buffer(gg.WindowBufferId{}) or { errors << err.msg() }
	if errors.len != 7 || errors.any(it != retained_ui_owner_thread_error) {
		return 'expected seven foreign-thread lease rejections, received ${errors}'
	}
	return 'foreign-ok'
}

fn validate_retained_ui_stale_cleanup_lease(mut state RetainedUiProbeState) string {
	mut cleanup_lease := state.cleanup_lease
	cleanup_lease.with_native_window(fn (mut lease gg.NativeWindowLease) ! {
		_ = lease
	}) or {
		if err.msg() == retained_ui_stale_lease_error {
			return 'cleanup-ok'
		}
		return err.msg()
	}
	return 'cleanup lease unexpectedly remained active'
}

fn validate_retained_ui_foreign_cleanup_lease(mut state RetainedUiProbeState) string {
	mut cleanup_lease := state.cleanup_lease
	cleanup_lease.with_native_window(fn (mut lease gg.NativeWindowLease) ! {
		_ = lease
	}) or {
		if err.msg() == retained_ui_owner_thread_error {
			return 'foreign-cleanup-ok'
		}
		return err.msg()
	}
	return 'foreign cleanup lease unexpectedly remained usable'
}

fn validate_retained_ui_stale_app_cleanup_lease(mut state RetainedUiProbeState) string {
	mut cleanup_lease := state.app_cleanup_lease
	cleanup_lease.retire_buffer(gg.WindowBufferId{}) or {
		if err.msg() == retained_ui_stale_lease_error {
			return ''
		}
		return err.msg()
	}
	return 'app cleanup lease unexpectedly remained active'
}

fn validate_retained_ui_two_window_trace(trace []string) string {
	main_then_tool_single_commit := [
		'begin_swapchain_pass',
		'end_pass',
		'begin_swapchain_pass',
		'update_buffer',
		'end_pass',
		'commit',
	]
	tool_then_main_single_commit := [
		'begin_swapchain_pass',
		'update_buffer',
		'end_pass',
		'begin_swapchain_pass',
		'end_pass',
		'commit',
	]
	main_then_tool_split_commits := [
		'begin_swapchain_pass',
		'end_pass',
		'commit',
		'begin_swapchain_pass',
		'update_buffer',
		'end_pass',
		'commit',
	]
	tool_then_main_split_commits := [
		'begin_swapchain_pass',
		'update_buffer',
		'end_pass',
		'commit',
		'begin_swapchain_pass',
		'end_pass',
		'commit',
	]
	if trace == main_then_tool_single_commit || trace == tool_then_main_single_commit
		|| trace == main_then_tool_split_commits || trace == tool_then_main_split_commits {
		return ''
	}
	return 'two-window trace mismatch: expected ${main_then_tool_single_commit}, ${tool_then_main_single_commit}, ${main_then_tool_split_commits}, or ${tool_then_main_split_commits}; received ${trace}'
}

fn wait_for_retained_ui_trace_ack(trace_ack chan string, expected string) ! {
	select {
		value := <-trace_ack {
			if value != expected {
				return error('expected `${expected}`, received `${value}`')
			}
		}
		retained_ui_probe_watchdog {
			return error('watchdog expired waiting for owner trace acknowledgement')
		}
	}
}

fn wait_for_retained_ui_trace_snapshot(trace_snapshot chan []string) ![]string {
	mut snapshot := []string{}
	select {
		value := <-trace_snapshot {
			snapshot = value.clone()
		}
		retained_ui_probe_watchdog {
			return error('watchdog expired waiting for owner trace snapshot')
		}
	}
	return snapshot
}

fn wait_for_retained_ui_phase_ack(phase_ack chan RetainedUiProbePhaseAck, expected_phase RetainedUiProbePhase, expected_generation u64) ! {
	mut received := RetainedUiProbePhaseAck{}
	select {
		value := <-phase_ack {
			received = value
		}
		retained_ui_probe_watchdog {
			return error('watchdog expired waiting for ${expected_phase} generation ${expected_generation} acknowledgement')
		}
	}
	if received.phase != expected_phase {
		return error('expected phase ${expected_phase}, received ${received.phase}')
	}
	if received.generation != expected_generation {
		return error('expected generation ${expected_generation}, received ${received.generation}')
	}
}

fn wait_for_retained_ui_phase_barrier(frames chan RetainedUiProbeFrame, windows []string, generation u64) ! {
	if generation == 0 {
		return error('retained UI phase generation was not armed')
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
			retained_ui_probe_watchdog {
				return error('watchdog expired with ${seen.len}/${windows.len} windows ready for generation ${generation}')
			}
		}
	}
}

fn fail_retained_ui_probe(mut app gg.App, message string, result chan string) {
	post_retained_ui_probe_stop(mut app) or {
		result <- '${message}; owner stop admission failed: ${err.msg()}'
		return
	}
	result <- message
}

fn post_retained_ui_probe_stop(mut app gg.App) ! {
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	})!
}
