// vtest build: gg_multiwindow? && !musl? && !self_ubuntu_musl_ci?
module gg

import os
import sokol.gfx

fn wayland_gg_runtime_probe_available() bool {
	if os.getenv('WAYLAND_DISPLAY') != '' {
		return true
	}
	eprintln('SKIP gg Wayland Package2 probe: WAYLAND_DISPLAY is not set')
	assert os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') != '1', 'gg Wayland runtime probes were required, but no compositor is available'
	return false
}

struct WaylandWindowCaptureTestState {
mut:
	frames                   int
	readback                 WindowReadbackId
	no_producer              WindowId
	expected_submitted_frame u64
}

struct WaylandOffscreenReadbackTestState {
mut:
	image                    WindowImageId
	attachments              WindowAttachmentsId
	readback                 WindowReadbackId
	requested                bool
	expected_submitted_frame u64
	premature_results        int
}

struct WaylandOffscreenReadbackFailureTestState {
mut:
	image       WindowImageId
	attachments WindowAttachmentsId
	readback    WindowReadbackId
	requested   bool
}

fn test_multiwindow_wayland_public_window_capture_delivers_submitted_frame() {
	$if linux && sokol_wayland ? {
		if !wayland_gg_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: true)!
		results := chan WindowReadbackResult{cap: 1}
		mut state := &WaylandWindowCaptureTestState{}
		window := app.create_window(
			title:       'V gg.App Wayland public window capture'
			width:       64
			height:      48
			high_dpi:    false
			redraw_mode: .continuous
			frame_fn:    fn [mut state] (mut context WindowContext) ! {
				state.frames++
				if state.frames == 2 {
					mut app := context.app
					assert app.window_operation_capability(context.window_id(), .window_capture)!.support == .available
					assert app.window_readback_capabilities(context.window_id())!.window_capture
					assert app.window_operation_capability(state.no_producer, .window_capture)!.support == .unsupported
					assert !app.window_readback_capabilities(state.no_producer)!.window_capture
					pending_before_rejection := app.pending_window_captures.len
					mut rejected_without_producer := false
					_ = app.request_window_capture(state.no_producer, WindowReadbackConfig{}) or {
						rejected_without_producer = true
						assert err.msg() == err_multiwindow_render_readback_unsupported
						WindowReadbackId{}
					}
					assert rejected_without_producer
					assert app.pending_window_captures.len == pending_before_rejection
					metrics := app.window_metrics(context.window_id())!
					state.expected_submitted_frame = metrics.submitted_frame + 1
					state.readback = app.request_window_capture(context.window_id(), WindowReadbackConfig{
						rect: WindowPixelRect{
							x:      8
							y:      6
							width:  4
							height: 4
						}
					})!
				}
				context.with_swapchain_sgl(gfx.create_clear_pass_action(0, 0, 0, 1), fn (mut drawing WindowSglContext) ! {
					drawing.defaults()
					drawing.matrix_mode_projection()
					drawing.load_identity()
					drawing.ortho(0, 64, 48, 0, -1, 1)
					drawing.begin_quads()
					drawing.v2f_c4b(8, 6, 255, 0, 0, 255)
					drawing.v2f_c4b(10, 6, 255, 0, 0, 255)
					drawing.v2f_c4b(10, 8, 255, 0, 0, 255)
					drawing.v2f_c4b(8, 8, 255, 0, 0, 255)
					drawing.v2f_c4b(10, 6, 0, 255, 0, 255)
					drawing.v2f_c4b(12, 6, 0, 255, 0, 255)
					drawing.v2f_c4b(12, 8, 0, 255, 0, 255)
					drawing.v2f_c4b(10, 8, 0, 255, 0, 255)
					drawing.v2f_c4b(8, 8, 0, 0, 255, 255)
					drawing.v2f_c4b(10, 8, 0, 0, 255, 255)
					drawing.v2f_c4b(10, 10, 0, 0, 255, 255)
					drawing.v2f_c4b(8, 10, 0, 0, 255, 255)
					drawing.v2f_c4b(10, 8, 255, 255, 0, 255)
					drawing.v2f_c4b(12, 8, 255, 255, 0, 255)
					drawing.v2f_c4b(12, 10, 255, 255, 0, 255)
					drawing.v2f_c4b(10, 10, 255, 255, 0, 255)
					drawing.end()
				})!
			}
		)!
		no_producer := app.create_window(
			title:    'V gg.App Wayland capture without a frame producer'
			width:    64
			height:   48
			high_dpi: false
		)!
		state.no_producer = no_producer
		assert app.window_operation_capability(no_producer, .window_capture)!.support == .unsupported
		assert !app.window_readback_capabilities(no_producer)!.window_capture
		assert app.window_operation_capability(window, .window_capture)!.support == .unsupported
		assert !app.window_readback_capabilities(window)!.window_capture
		app.run(
			readback_fn: fn [results] (result WindowReadbackResult, mut app App) ! {
				results <- result
				app.stop()!
			}
		)!

		result := <-results
		assert result.id == state.readback
		assert result.window == window
		assert result.status == .ready
		assert result.submitted_frame == state.expected_submitted_frame
		assert result.width == 4
		assert result.height == 4
		assert result.stride == 16
		assert result.pixels_rgba8.len == 64
		for sample in [
			[0, 0, 255, 0, 0],
			[3, 0, 0, 255, 0],
			[0, 3, 0, 0, 255],
			[3, 3, 255, 255, 0],
		] {
			offset := sample[1] * result.stride + sample[0] * 4
			assert result.pixels_rgba8[offset] == sample[2]
			assert result.pixels_rgba8[offset + 1] == sample[3]
			assert result.pixels_rgba8[offset + 2] == sample[4]
			assert result.pixels_rgba8[offset + 3] == 255
		}
	}
}

fn test_multiwindow_wayland_public_managed_offscreen_readback_delivers_exact_region() {
	$if linux && sokol_wayland ? {
		if !wayland_gg_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: true)!
		results := chan WindowReadbackResult{cap: 1}
		mut state := &WaylandOffscreenReadbackTestState{}
		window := app.create_window(
			title:       'V gg.App Wayland offscreen readback'
			width:       48
			height:      48
			redraw_mode: .continuous
			init_fn:     fn [mut state] (mut context WindowInitContext) ! {
				context.with_resources(fn [mut state] (mut resources WindowResourceContext) ! {
					state.image = resources.make_image(&gfx.ImageDesc{
						render_target: true
						width:         4
						height:        4
						pixel_format:  .rgba8
						sample_count:  1
					})!
					state.attachments = resources.make_attachments(WindowAttachmentsConfig{
						colors: [state.image]
					})!
				})!
			}
			frame_fn:    fn [mut state] (mut context WindowContext) ! {
				if state.requested {
					return
				}
				state.requested = true
				state.expected_submitted_frame = context.info.submitted_frame + 1
				context.with_offscreen_sgl(WindowOffscreenPassConfig{
					attachments: state.attachments
					action:      gfx.create_clear_pass_action(0, 0, 0, 1)
				}, fn (mut drawing WindowSglContext) ! {
					drawing.defaults()
					drawing.matrix_mode_projection()
					drawing.load_identity()
					drawing.ortho(0, 4, 4, 0, -1, 1)
					drawing.begin_quads()
					drawing.v2f_c4b(0, 0, 255, 0, 0, 255)
					drawing.v2f_c4b(4, 0, 255, 0, 0, 255)
					drawing.v2f_c4b(4, 2, 255, 0, 0, 255)
					drawing.v2f_c4b(0, 2, 255, 0, 0, 255)
					drawing.v2f_c4b(0, 2, 0, 0, 255, 255)
					drawing.v2f_c4b(4, 2, 0, 0, 255, 255)
					drawing.v2f_c4b(4, 4, 0, 0, 255, 255)
					drawing.v2f_c4b(0, 4, 0, 0, 255, 255)
					drawing.end()
				})!
				state.readback = context.request_image_readback(state.image, WindowReadbackConfig{
					rect: WindowPixelRect{
						x:      1
						y:      1
						width:  2
						height: 2
					}
				})!
				mut app := context.app
				premature := app.core.drain_readback_events()!
				state.premature_results = premature.len
				assert premature.len == 0
			}
			cleanup_fn:  fn [mut state] (mut context WindowCleanupContext) ! {
				context.with_resources(fn [mut state] (mut resources WindowResourceContext) ! {
					resources.retire_attachments(state.attachments)!
					resources.retire_image(state.image)!
				})!
			}
		)!
		capabilities := app.window_readback_capabilities(window)!
		assert capabilities.offscreen_image
		app.run(
			readback_fn: fn [results, mut state] (result WindowReadbackResult, mut app App) ! {
				metrics := app.window_metrics(result.window)!
				assert result.status == .ready
				assert result.submitted_frame == state.expected_submitted_frame
				assert metrics.submitted_frame >= result.submitted_frame
				results <- result
				app.stop()!
			}
		)!
		result := <-results
		assert state.premature_results == 0
		assert result.id == state.readback
		assert result.window == window
		assert result.status == .ready
		assert result.submitted_frame == state.expected_submitted_frame
		assert result.width == 2
		assert result.height == 2
		assert result.stride == 8
		assert result.pixels_rgba8.len == 16
		for offset in [0, 4] {
			assert result.pixels_rgba8[offset] == 255
			assert result.pixels_rgba8[offset + 1] == 0
			assert result.pixels_rgba8[offset + 2] == 0
			assert result.pixels_rgba8[offset + 3] == 255
		}
		for offset in [8, 12] {
			assert result.pixels_rgba8[offset] == 0
			assert result.pixels_rgba8[offset + 1] == 0
			assert result.pixels_rgba8[offset + 2] == 255
			assert result.pixels_rgba8[offset + 3] == 255
		}
	}
}

fn test_multiwindow_wayland_offscreen_readback_failure_is_terminal_non_ready() {
	$if linux && sokol_wayland ? {
		if !wayland_gg_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: true)!
		mut state := &WaylandOffscreenReadbackFailureTestState{}
		window := app.create_window(
			title:       'V gg.App Wayland failed offscreen readback'
			width:       48
			height:      48
			redraw_mode: .continuous
			init_fn:     fn [mut state] (mut context WindowInitContext) ! {
				context.with_resources(fn [mut state] (mut resources WindowResourceContext) ! {
					state.image = resources.make_image(&gfx.ImageDesc{
						render_target: true
						width:         4
						height:        4
						pixel_format:  .rgba8
						sample_count:  1
					})!
					state.attachments = resources.make_attachments(WindowAttachmentsConfig{
						colors: [state.image]
					})!
				})!
			}
			frame_fn:    fn [mut state] (mut context WindowContext) ! {
				if state.requested {
					return
				}
				state.requested = true
				context.with_offscreen_sgl(WindowOffscreenPassConfig{
					attachments: state.attachments
					action:      gfx.create_clear_pass_action(1, 0, 0, 1)
				}, fn (mut drawing WindowSglContext) ! {
					drawing.defaults()
				})!
				state.readback =
					context.request_image_readback(state.image, WindowReadbackConfig{})!
				return error('injected failure before Wayland offscreen submission')
			}
			cleanup_fn:  fn [mut state] (mut context WindowCleanupContext) ! {
				context.with_resources(fn [mut state] (mut resources WindowResourceContext) ! {
					resources.retire_attachments(state.attachments)!
					resources.retire_image(state.image)!
				})!
			}
		)!
		mut run_error := ''
		app.run() or { run_error = err.msg() }
		assert run_error.contains('injected failure before Wayland offscreen submission')
		core_results := app.core.drain_readback_events()!
		assert core_results.len == 1
		result := window_readback_result_from_core(core_results[0])
		assert result.id == state.readback
		assert result.window == window
		assert result.status == .failed
		assert result.submitted_frame == 0
		assert result.pixels_rgba8.len == 0
		app.stop() or {
			assert err.msg().contains('injected failure before Wayland offscreen submission')
		}
	}
}
