module main

import gg
import sokol.gfx
import time
import gg.testdata.multiwindow_probe_backend
import gg.testdata.multiwindow_probe_gate

const render_probe_watchdog = 5 * time.second

struct RenderProbeFrameSignal {
	window string
	serial u64
}

struct RenderProbeState {
mut:
	frames            map[string]u64
	cleanup_submitted map[string]u64
}

fn main() {
	run_render_probe() or { panic(err) }
}

fn run_render_probe() ! {
	multiwindow_probe_gate.await_parent_release(render_probe_watchdog)!
	frames := chan RenderProbeFrameSignal{cap: 8}
	driver_result := chan string{cap: 1}
	mut state := &RenderProbeState{}
	backend := multiwindow_probe_backend.selected()!
	mut app := gg.new_app(
		backend:          backend
		queue_size:       16
		require_renderer: true
	)!
	multiwindow_probe_backend.validate(app.capabilities(), backend)!

	main_window := app.create_window(
		title:       'gg runtime probe: main'
		width:       640
		height:      400
		clear_color: gg.rgb(18, 28, 38)
		redraw_mode: .on_demand
		frame_fn:    fn [frames, mut state] (mut context gg.WindowContext) ! {
			draw_probe_window(mut context, gg.rgb(65, 170, 210), frames, mut state)!
		}
		cleanup_fn:  fn [mut state] (mut context gg.WindowCleanupContext) ! {
			state.cleanup_submitted[context.window_id().str()] = context.metrics().submitted_frame
		}
	)!
	tool_window := app.create_window(
		title:       'gg runtime probe: tool'
		width:       360
		height:      260
		clear_color: gg.rgb(236, 224, 196)
		redraw_mode: .on_demand
		frame_fn:    fn [frames, mut state] (mut context gg.WindowContext) ! {
			draw_probe_window(mut context, gg.rgb(205, 82, 55), frames, mut state)!
		}
		cleanup_fn:  fn [mut state] (mut context gg.WindowCleanupContext) ! {
			state.cleanup_submitted[context.window_id().str()] = context.metrics().submitted_frame
		}
	)!

	driver := spawn drive_render_probe(mut app, main_window, tool_window, frames, driver_result)
	mut run_error := ''
	app.run(
		event_fn: fn (event gg.WindowEvent, mut app gg.App) ! {
			if event.kind != .window_close_requested || !app.window_exists(event.window) {
				return
			}
			app.destroy_window(event.window)!
			if app.window_ids()!.len == 0 {
				app.stop()!
			}
		}
	) or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	mut stop_error := ''
	app.stop() or { stop_error = err.msg() }

	mut failures := []string{}
	if run_error != '' {
		failures << 'owner loop: ${run_error}'
	}
	if driver_error != '' {
		failures << 'driver: ${driver_error}'
	}
	if stop_error != '' {
		failures << 'final stop: ${stop_error}'
	}
	if failures.len > 0 {
		return error(failures.join('; '))
	}
	for window in [main_window, tool_window] {
		key := window.str()
		assert state.frames[key] >= 2, '${key} did not cross both frame barriers'
		assert state.cleanup_submitted[key] == state.frames[key], '${key} cleanup observed ${state.cleanup_submitted[key]} submitted frames after ${state.frames[key]} successful callbacks'
	}
	println('{"example":"multiwindow_render_runtime","status":"PASS","cleanup":"complete"}')
}

fn drive_render_probe(mut app gg.App, main_window gg.WindowId, tool_window gg.WindowId, frames chan RenderProbeFrameSignal, result chan string) {
	windows := [main_window.str(), tool_window.str()]
	first := wait_for_render_probe_barrier(frames, windows, map[string]u64{}) or {
		fail_render_probe(mut app, 'first frame barrier: ${err.msg()}', result)
		return
	}
	app.request_redraw(main_window) or {
		fail_render_probe(mut app, 'main redraw admission: ${err.msg()}', result)
		return
	}
	app.request_redraw(main_window) or {
		fail_render_probe(mut app, 'coalesced main redraw admission: ${err.msg()}', result)
		return
	}
	app.request_redraw(tool_window) or {
		fail_render_probe(mut app, 'tool redraw admission: ${err.msg()}', result)
		return
	}
	wait_for_render_probe_barrier(frames, windows, first) or {
		fail_render_probe(mut app, 'second frame barrier: ${err.msg()}', result)
		return
	}
	post_render_probe_stop(mut app) or {
		result <- 'owner stop admission: ${err.msg()}'
		return
	}
	result <- ''
}

fn wait_for_render_probe_barrier(frames chan RenderProbeFrameSignal, windows []string, after map[string]u64) !map[string]u64 {
	mut seen := map[string]u64{}
	for seen.len < windows.len {
		select {
			signal := <-frames {
				if signal.window !in windows || signal.serial <= after[signal.window] {
					continue
				}
				seen[signal.window] = signal.serial
			}
			render_probe_watchdog {
				return error('watchdog expired with ${seen.len}/${windows.len} windows ready')
			}
		}
	}
	return seen
}

fn fail_render_probe(mut app gg.App, message string, result chan string) {
	post_render_probe_stop(mut app) or {
		result <- '${message}; owner stop admission failed: ${err.msg()}'
		return
	}
	result <- message
}

fn post_render_probe_stop(mut app gg.App) ! {
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	})!
}

fn draw_probe_window(mut context gg.WindowContext, accent gg.Color, frames chan RenderProbeFrameSignal, mut state RenderProbeState) ! {
	info := context.frame_info()
	assert info.frame_serial == info.submitted_frame + 1
	assert info.metrics.submitted_frame == info.submitted_frame
	assert info.metrics.logical_size.width > 0
	assert info.metrics.logical_size.height > 0
	assert info.metrics.framebuffer_size.width > 0
	assert info.metrics.framebuffer_size.height > 0
	action := gfx.create_clear_pass_action(0, 0, 0, 0)
	context.with_swapchain_sgl(action, fn [info, accent] (mut drawing gg.WindowSglContext) ! {
		width := info.metrics.logical_size.width
		height := info.metrics.logical_size.height
		drawing.defaults()
		drawing.matrix_mode_projection()
		drawing.load_identity()
		drawing.ortho(0, width, height, 0, -1, 1)
		drawing.c4b(accent.r, accent.g, accent.b, accent.a)
		drawing.begin_quads()
		drawing.v2f(width * 0.15, height * 0.2)
		drawing.v2f(width * 0.85, height * 0.2)
		drawing.v2f(width * 0.85, height * 0.8)
		drawing.v2f(width * 0.15, height * 0.8)
		drawing.end()
	})!
	state.frames[info.window.str()]++
	frames <- RenderProbeFrameSignal{
		window: info.window.str()
		serial: info.frame_serial
	}
}
