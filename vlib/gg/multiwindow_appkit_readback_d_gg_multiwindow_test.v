// vtest build: gg_multiwindow?
module gg

import os
import sokol.gfx

struct AppKitPublicReadbackState {
mut:
	image                    WindowImageId
	attachments              WindowAttachmentsId
	window_readback          WindowReadbackId
	pre_pass_image_readback  WindowReadbackId
	post_pass_image_readback WindowReadbackId
	expected_submitted_frame u64
	requested                bool
}

struct AppKitPublicReadbackCompletion {
mut:
	count int
}

fn appkit_public_readback_runtime_requested() bool {
	if os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
		&& os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') == 'appkit' {
		return true
	}
	eprintln('SKIP gg AppKit public readback probe: runtime probe is not requested')
	return false
}

fn appkit_public_readback_init(mut context WindowInitContext, mut state AppKitPublicReadbackState) ! {
	context.with_resources(fn [mut state] (mut resources WindowResourceContext) ! {
		state.image = resources.make_image(&gfx.ImageDesc{
			render_target: true
			width:         5
			height:        3
			pixel_format:  .rgba8
			sample_count:  1
		})!
		state.attachments = resources.make_attachments(WindowAttachmentsConfig{
			colors: [state.image]
		})!
	})!
}

fn appkit_public_readback_frame(mut context WindowContext, mut state AppKitPublicReadbackState) ! {
	if state.requested {
		return
	}
	mut app := context.app
	assert app.window_operation_capability(context.window_id(), .window_capture)!.support == .available
	assert app.window_readback_capabilities(context.window_id())!.window_capture
	state.requested = true
	state.expected_submitted_frame = context.info.submitted_frame + 1
	context.with_offscreen(WindowOffscreenPassConfig{
		attachments: state.attachments
		action:      gfx.create_clear_pass_action(0, 0, 1, 1)
	}, fn (mut pass WindowPassContext) ! {
		_ = pass
	})!
	state.pre_pass_image_readback = context.request_image_readback(state.image, WindowReadbackConfig{
		rect: WindowPixelRect{
			x:      1
			y:      0
			width:  3
			height: 3
		}
	})!
	context.with_offscreen(WindowOffscreenPassConfig{
		attachments: state.attachments
		action:      gfx.create_clear_pass_action(1, 0, 0, 1)
	}, fn (mut pass WindowPassContext) ! {
		_ = pass
	})!
	state.post_pass_image_readback = context.request_image_readback(state.image, WindowReadbackConfig{
		rect: WindowPixelRect{
			x:      1
			y:      0
			width:  3
			height: 3
		}
	})!
	state.window_readback = app.request_window_capture(context.window_id(), WindowReadbackConfig{
		rect: WindowPixelRect{
			x:      8
			y:      6
			width:  3
			height: 2
		}
	})!
	premature := app.core.drain_readback_events()!
	if premature.len != 0 {
		return error('AppKit readback became observable before its producing submission')
	}
	context.with_swapchain_sgl(gfx.create_clear_pass_action(0, 0, 0, 1), fn (mut drawing WindowSglContext) ! {
		drawing.defaults()
		drawing.matrix_mode_projection()
		drawing.load_identity()
		drawing.ortho(0, 48, 32, 0, -1, 1)
		drawing.begin_quads()
		drawing.v2f_c4b(8, 6, 0, 255, 0, 255)
		drawing.v2f_c4b(11, 6, 0, 255, 0, 255)
		drawing.v2f_c4b(11, 7, 0, 255, 0, 255)
		drawing.v2f_c4b(8, 7, 0, 255, 0, 255)
		drawing.v2f_c4b(8, 7, 255, 255, 0, 255)
		drawing.v2f_c4b(11, 7, 255, 255, 0, 255)
		drawing.v2f_c4b(11, 8, 255, 255, 0, 255)
		drawing.v2f_c4b(8, 8, 255, 255, 0, 255)
		drawing.end()
	})!
}

fn appkit_public_readback_cleanup(mut context WindowCleanupContext, mut state AppKitPublicReadbackState) ! {
	context.with_resources(fn [mut state] (mut resources WindowResourceContext) ! {
		resources.retire_attachments(state.attachments)!
		resources.retire_image(state.image)!
	})!
}

fn appkit_assert_public_image_result(result WindowReadbackResult, state AppKitPublicReadbackState, expected []u8) {
	assert result.status == .ready
	assert result.submitted_frame == state.expected_submitted_frame
	assert result.width == 3
	assert result.height == 3
	assert result.stride == 12
	assert result.pixels_rgba8.len == 36
	assert expected.len == 4
	for y in 0 .. result.height {
		for x in 0 .. result.width {
			offset := y * result.stride + x * 4
			assert result.pixels_rgba8[offset..offset + 4] == expected
		}
	}
}

fn appkit_assert_public_window_result(result WindowReadbackResult, state AppKitPublicReadbackState) {
	assert result.status == .ready
	assert result.submitted_frame == state.expected_submitted_frame
	assert result.width == 3
	assert result.height == 2
	assert result.stride == 12
	assert result.pixels_rgba8.len == 24
	for x in 0 .. 3 {
		top := x * 4
		bottom := result.stride + x * 4
		assert result.pixels_rgba8[top..top + 4] == [u8(0), 255, 0, 255]
		assert result.pixels_rgba8[bottom..bottom + 4] == [u8(255), 255, 0, 255]
	}
}

fn appkit_assert_public_results_for_window(results []WindowReadbackResult, state AppKitPublicReadbackState, window WindowId) {
	mut image_found := false
	mut post_image_found := false
	mut window_found := false
	for result in results {
		if result.window != window {
			continue
		}
		if result.id == state.pre_pass_image_readback {
			appkit_assert_public_image_result(result, state, [u8(0), 0, 255, 255])
			image_found = true
		} else if result.id == state.post_pass_image_readback {
			appkit_assert_public_image_result(result, state, [u8(255), 0, 0, 255])
			post_image_found = true
		} else if result.id == state.window_readback {
			appkit_assert_public_window_result(result, state)
			window_found = true
		}
	}
	assert image_found
	assert post_image_found
	assert window_found
}

fn test_multiwindow_appkit_public_readback_requires_live_metal_renderer() {
	$if darwin && sokol_metal ? {
		mut app := new_app(backend: .appkit, require_renderer: false)!
		window :=
			app.create_window(title: 'AppKit rendererless readback capability', visible: false)!
		assert !app.capabilities().readback
		capabilities := app.window_readback_capabilities(window)!
		assert !capabilities.offscreen_image
		assert !capabilities.window_capture
		assert app.window_operation_capability(window, .window_capture)!.support == .unsupported
		app.stop()!
	}
}

fn test_multiwindow_appkit_public_window_capture_requires_frame_producer() {
	$if darwin && sokol_metal ? {
		if !appkit_public_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, require_renderer: true)!
		window := app.create_window(
			title:    'AppKit capture without a frame producer'
			width:    48
			height:   32
			high_dpi: false
			init_fn:  fn (mut context WindowInitContext) ! {
				_ = context
			}
		)!
		assert app.core.renderer_device_available_for_gg()
		assert gfx.query_backend() == .metal_macos
		assert app.core.service_operation_capability(window.core, .window_capture)!.support == .available
		app.core.set_render_window_snapshot_for_gg_test(window.core, .none, true, 48, 32, 1, 0)!
		snapshot := app.core.render_window_snapshot(window.core)!
		producer := app.render_runtime.window_capture_producer(window)!
		assert snapshot.metrics.metrics_available
		assert snapshot.metrics.framebuffer_width == 48
		assert snapshot.metrics.framebuffer_height == 32
		assert snapshot.target.sample_count == 1
		assert snapshot.block_reason == .none
		assert !producer.frame_active
		assert !producer.frame_fn_configured
		assert managed_window_capture_has_producer(MultiWindowCaptureProducer{
			frame_fn_configured: true
		}, snapshot)
		assert !managed_window_capture_has_producer(producer, snapshot)
		assert app.window_operation_capability(window, .window_capture)!.support == .unsupported
		assert !app.window_readback_capabilities(window)!.window_capture

		dirty_before := snapshot.dirty_epoch
		pending_before := app.pending_window_captures.len
		mut rejected := false
		_ = app.request_window_capture(window, WindowReadbackConfig{}) or {
			rejected = true
			assert err.msg() == err_multiwindow_render_readback_unsupported
			WindowReadbackId{}
		}
		assert rejected
		assert app.core.render_window_snapshot(window.core)!.dirty_epoch == dirty_before
		assert app.pending_window_captures.len == pending_before
		assert app.core.drain_readback_events()!.len == 0
		app.stop()!
	}
}

fn test_multiwindow_appkit_public_readbacks_are_per_window_and_post_submit() {
	$if darwin && sokol_metal ? {
		if !appkit_public_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, require_renderer: true)!
		assert app.capabilities().readback
		results := chan WindowReadbackResult{cap: 6}
		mut first := &AppKitPublicReadbackState{}
		mut second := &AppKitPublicReadbackState{}
		first_window := app.create_window(
			title:       'AppKit public readback A'
			width:       48
			height:      32
			high_dpi:    false
			redraw_mode: .continuous
			init_fn:     fn [mut first] (mut context WindowInitContext) ! {
				appkit_public_readback_init(mut context, mut first)!
			}
			frame_fn:    fn [mut first] (mut context WindowContext) ! {
				appkit_public_readback_frame(mut context, mut first)!
			}
			cleanup_fn:  fn [mut first] (mut context WindowCleanupContext) ! {
				appkit_public_readback_cleanup(mut context, mut first)!
			}
		)!
		second_window := app.create_window(
			title:       'AppKit public readback B'
			width:       48
			height:      32
			high_dpi:    false
			redraw_mode: .continuous
			init_fn:     fn [mut second] (mut context WindowInitContext) ! {
				appkit_public_readback_init(mut context, mut second)!
			}
			frame_fn:    fn [mut second] (mut context WindowContext) ! {
				appkit_public_readback_frame(mut context, mut second)!
			}
			cleanup_fn:  fn [mut second] (mut context WindowCleanupContext) ! {
				appkit_public_readback_cleanup(mut context, mut second)!
			}
		)!
		assert app.window_readback_capabilities(first_window)!.offscreen_image
		assert !app.window_readback_capabilities(first_window)!.window_capture
		assert app.window_operation_capability(first_window, .window_capture)!.support == .unsupported
		assert app.window_readback_capabilities(second_window)!.offscreen_image
		assert !app.window_readback_capabilities(second_window)!.window_capture
		assert app.window_operation_capability(second_window, .window_capture)!.support == .unsupported
		mut completion := &AppKitPublicReadbackCompletion{}
		app.run(
			readback_fn: fn [results, mut completion] (result WindowReadbackResult, mut app App) ! {
				results <- result
				completion.count++
				if completion.count == 6 {
					app.stop()!
				}
			}
		)!
		mut received := []WindowReadbackResult{cap: 6}
		for _ in 0 .. 6 {
			received << <-results
		}
		appkit_assert_public_results_for_window(received, *first, first_window)
		appkit_assert_public_results_for_window(received, *second, second_window)
	}
}
