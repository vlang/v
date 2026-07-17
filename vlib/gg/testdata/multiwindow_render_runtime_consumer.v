module main

import gg
import sokol.gfx

fn consume_window_resources(mut resources gg.WindowResourceContext) ! {
	buffer_desc := gfx.BufferDesc{}
	image_desc := gfx.ImageDesc{}
	sampler_desc := gfx.SamplerDesc{}
	shader_desc := gfx.ShaderDesc{}
	pipeline_desc := gfx.PipelineDesc{}
	data := gfx.Range{}
	image_data := gfx.ImageData{}

	buffer := resources.make_buffer(&buffer_desc)!
	image := resources.make_image(&image_desc)!
	sampler := resources.make_sampler(&sampler_desc)!
	shader := resources.make_shader(&shader_desc)!
	pipeline := resources.make_pipeline(&pipeline_desc, shader)!
	attachments := resources.make_attachments(gg.WindowAttachmentsConfig{
		colors:        [image]
		resolves:      [image]
		depth_stencil: image
	})!
	sgl_pipeline := resources.make_sgl_pipeline(&pipeline_desc)!
	custom_sgl_pipeline := resources.make_sgl_pipeline_with_shader(&pipeline_desc, shader)!
	resources.update_buffer(buffer, &data)!
	_ = resources.append_buffer(buffer, &data)!
	resources.update_image(image, &image_data)!
	_ = resources.replace_image(image, &image_desc)!
	resources.retire_attachments(attachments)!
	resources.retire_pipeline(pipeline)!
	resources.retire_shader(shader)!
	resources.retire_sampler(sampler)!
	resources.retire_sgl_pipeline(sgl_pipeline)!
	resources.retire_sgl_pipeline(custom_sgl_pipeline)!
	resources.retire_image(image)!
	resources.retire_buffer(buffer)!
}

fn consume_window_pass(mut pass gg.WindowPassContext) ! {
	pass.apply_pipeline(gg.WindowPipelineId{})!
	pass.apply_bindings(gg.WindowBindings{
		vertex_buffers: [gg.WindowBufferBinding{}]
		index_buffer:   gg.WindowBufferBinding{}
		vs:             gg.WindowStageBindings{
			images:          [gg.WindowImageBinding{}]
			samplers:        [gg.WindowSamplerBinding{}]
			storage_buffers: [gg.WindowBufferBinding{}]
		}
		fs:             gg.WindowStageBindings{
			images:          [gg.WindowImageBinding{}]
			samplers:        [gg.WindowSamplerBinding{}]
			storage_buffers: [gg.WindowBufferBinding{}]
		}
	})!
	data := gfx.Range{}
	pass.apply_uniforms(.vs, 0, &data)!
	pass.apply_viewport(gg.WindowPixelRect{})!
	pass.apply_scissor(gg.WindowPixelRect{})!
	pass.draw(0, 3, 1)!
}

fn consume_window_sgl(mut sgl_context gg.WindowSglContext) ! {
	matrix := []f32{len: 16}
	sgl_context.defaults()
	sgl_context.viewport(0, 0, 1, 1, true)
	sgl_context.scissor_rect(0, 0, 1, 1, true)
	sgl_context.scissor_rectf(0, 0, 1, 1, true)
	sgl_context.enable_texture()
	sgl_context.texture(gg.WindowImageId{}, gg.WindowSamplerId{})!
	sgl_context.disable_texture()
	sgl_context.load_default_pipeline()
	sgl_context.default_pipeline()
	sgl_context.load_pipeline(gg.WindowSglPipelineId{})!
	sgl_context.push_pipeline()
	sgl_context.pop_pipeline()
	sgl_context.matrix_mode_modelview()
	sgl_context.matrix_mode_projection()
	sgl_context.matrix_mode_texture()
	sgl_context.load_identity()
	sgl_context.load_matrix(matrix)
	sgl_context.load_transpose_matrix(matrix)
	sgl_context.mult_matrix(matrix)
	sgl_context.mult_transpose_matrix(matrix)
	sgl_context.rotate(0, 0, 0, 1)
	sgl_context.scale(1, 1, 1)
	sgl_context.translate(0, 0, 0)
	sgl_context.frustum(-1, 1, -1, 1, 0.1, 100)
	sgl_context.ortho(0, 1, 1, 0, -1, 1)
	sgl_context.perspective(1, 1, 0.1, 100)
	sgl_context.lookat(0, 0, 1, 0, 0, 0, 0, 1, 0)
	sgl_context.push_matrix()
	sgl_context.pop_matrix()
	sgl_context.t2f(0, 0)
	sgl_context.c3f(1, 1, 1)
	sgl_context.c4f(1, 1, 1, 1)
	sgl_context.c3b(255, 255, 255)
	sgl_context.c4b(255, 255, 255, 255)
	sgl_context.c1i(0xffffffff)
	sgl_context.point_size(1)
	sgl_context.begin_points()
	sgl_context.end()
	sgl_context.begin_lines()
	sgl_context.end()
	sgl_context.begin_line_strip()
	sgl_context.end()
	sgl_context.begin_triangles()
	sgl_context.v2f(0, 0)
	sgl_context.v3f(0, 0, 0)
	sgl_context.v2f_t2f(0, 0, 0, 0)
	sgl_context.v3f_t2f(0, 0, 0, 0, 0)
	sgl_context.v2f_c3f(0, 0, 1, 1, 1)
	sgl_context.v2f_c3b(0, 0, 255, 255, 255)
	sgl_context.v2f_c4f(0, 0, 1, 1, 1, 1)
	sgl_context.v2f_c4b(0, 0, 255, 255, 255, 255)
	sgl_context.v2f_c1i(0, 0, 0xffffffff)
	sgl_context.v3f_c3f(0, 0, 0, 1, 1, 1)
	sgl_context.v3f_c3b(0, 0, 0, 255, 255, 255)
	sgl_context.v3f_c4f(0, 0, 0, 1, 1, 1, 1)
	sgl_context.v3f_c4b(0, 0, 0, 255, 255, 255, 255)
	sgl_context.v3f_c1i(0, 0, 0, 0xffffffff)
	sgl_context.v2f_t2f_c3f(0, 0, 0, 0, 1, 1, 1)
	sgl_context.v2f_t2f_c3b(0, 0, 0, 0, 255, 255, 255)
	sgl_context.v2f_t2f_c4f(0, 0, 0, 0, 1, 1, 1, 1)
	sgl_context.v2f_t2f_c4b(0, 0, 0, 0, 255, 255, 255, 255)
	sgl_context.v2f_t2f_c1i(0, 0, 0, 0, 0xffffffff)
	sgl_context.v3f_t2f_c3f(0, 0, 0, 0, 0, 1, 1, 1)
	sgl_context.v3f_t2f_c3b(0, 0, 0, 0, 0, 255, 255, 255)
	sgl_context.v3f_t2f_c4f(0, 0, 0, 0, 0, 1, 1, 1, 1)
	sgl_context.v3f_t2f_c4b(0, 0, 0, 0, 0, 255, 255, 255, 255)
	sgl_context.v3f_t2f_c1i(0, 0, 0, 0, 0, 0xffffffff)
	sgl_context.end()
	sgl_context.begin_triangle_strip()
	sgl_context.end()
	sgl_context.begin_quads()
	sgl_context.v2f(0, 0)
	sgl_context.v2f(1, 0)
	sgl_context.v2f(1, 1)
	sgl_context.v2f(0, 1)
	sgl_context.end()
}

fn consume_native_window(mut lease gg.NativeWindowLease) ! {
	_ = lease
}

fn on_window_init(mut context gg.WindowInitContext) ! {
	_ = context.window_id()
	_ = context.metrics()
	_ = context.render_target_info()
	context.with_resources(consume_window_resources)!
}

fn on_window_frame(mut context gg.WindowContext) ! {
	info := context.frame_info()
	_ = info.frame_serial
	_ = info.submitted_frame
	_ = info.metrics.logical_size.width
	_ = info.metrics.logical_size.height
	_ = info.metrics.framebuffer_size.width
	_ = info.metrics.framebuffer_size.height
	_ = info.metrics.dpi_scale
	_ = info.metrics.metrics_sequence
	_ = info.metrics.submitted_frame
	_ = info.target.color_format
	_ = info.target.depth_format
	_ = info.target.sample_count
	_ = context.window_id()
	_ = context.exists()
	_ = context.capabilities()
	_ = context.framebuffer_size()
	_ = context.size()
	_ = context.logical_size()
	_ = context.logical_bounds()
	_ = context.pixel_bounds()
	context.draw_rect_filled(0, 0, 1, 1, gg.white)
	context.draw_rect_empty(0, 0, 1, 1, gg.black)
	_ = context.logical_to_pixel_rect(gg.WindowLogicalRect{})
	_ = context.pixel_to_logical_rect(gg.WindowPixelRect{})
	context.with_resources(consume_window_resources)!
	context.with_offscreen(gg.WindowOffscreenPassConfig{}, consume_window_pass)!
	context.with_swapchain(gfx.PassAction{}, consume_window_pass)!
	context.with_offscreen_sgl(gg.WindowOffscreenPassConfig{}, consume_window_sgl)!
	context.with_swapchain_sgl(gfx.PassAction{}, consume_window_sgl)!
	_ = context.request_image_readback(gg.WindowImageId{}, gg.WindowReadbackConfig{})!
	_ = info.window
}

fn on_window_cleanup(mut context gg.WindowCleanupContext) ! {
	_ = context.window_id()
	_ = context.metrics()
	_ = context.render_target_info()
	_ = context.reason()
	_ = context.graphics_available()
	context.with_resources(fn (mut resources gg.WindowResourceContext) ! {
		resources.retire_buffer(gg.WindowBufferId{})!
	})!
	context.with_native_window(consume_native_window)!
}

fn on_app_resource_init(mut context gg.AppResourceContext) ! {
	consume_window_resources(mut context)!
}

fn on_app_resource_cleanup(mut context gg.AppResourceContext) ! {
	context.retire_buffer(gg.WindowBufferId{})!
}

fn on_app_resource_frame(mut context gg.AppResourceContext) ! {
	data := gfx.Range{}
	context.update_buffer(gg.WindowBufferId{}, &data)!
}

fn on_readback(result gg.WindowReadbackResult, mut app gg.App) ! {
	_ = result.id
	_ = result.window
	_ = result.status
	_ = result.submitted_frame
	_ = result.width
	_ = result.height
	_ = result.stride
	_ = result.pixels_rgba8
	_ = result.error
	_ = app.capabilities()
}

fn on_app_job(mut app gg.App) ! {
	_ = app.capabilities()
}

fn on_legacy_app_frame(mut app gg.App) ! {
	_ = app.capabilities()
}

fn on_app_event(event gg.WindowEvent, mut app gg.App) ! {
	_ = event.kind
	_ = event.window
	_ = event.width
	_ = event.height
	_ = app.capabilities()
}

fn on_app_input(event gg.WindowInputEvent, mut app gg.App) ! {
	_ = event.window
	_ = event.event
	_ = event.dropped_files
	_ = app.capabilities()
}

fn consume_app_surface(mut app gg.App, window gg.WindowId) ! {
	_ = window.str()
	app.set_window_title(window, 'compile consumer')!
	app.resize_window(window, 2, 2)!
	app.set_window_cursor(window, .pointer)!
	app.begin_window_move(window)!
	app.begin_window_resize(window, .bottom_right)!
	_ = app.window_info(window)!
	_ = app.window_ids()!
	_ = app.window_infos()!
	_ = app.window_exists(window)
	_ = app.capabilities()
	_ = app.drain_events()!
	_ = app.drain_input_events()!
	_ = app.poll_events()!
	app.post(on_app_job)!
	app.try_post(on_app_job)!
	_ = app.drain_pending(1)!
	app.request_redraw(window)!
	_ = app.window_metrics(window)!
	_ = app.window_render_target_info(window)!
	app.set_window_clear_color(window, gg.Color{})!
	_ = app.window_readback_capabilities(window)!
	_ = app.request_window_capture(window, gg.WindowReadbackConfig{})!
	app.draw_window(window, on_window_frame)!
	app.run(gg.RunConfig{
		frame_fn:                on_legacy_app_frame
		event_fn:                on_app_event
		input_fn:                on_app_input
		app_resource_init_fn:    on_app_resource_init
		app_resource_frame_fn:   on_app_resource_frame
		app_resource_cleanup_fn: on_app_resource_cleanup
		readback_fn:             on_readback
		max_pending_jobs:        1
	})!
	app.destroy_window(window)!
	app.stop()!
}

fn consume_facade_constructors() ! {
	_ = gg.capabilities_for_backend(.mock)!
	_ = gg.capabilities_for_backend_with_renderer(.auto)!
	mut app := gg.new_app(gg.AppConfig{
		backend:          .mock
		queue_size:       1
		require_renderer: true
	})!
	window := app.create_window(gg.WindowConfig{
		title:        'complete facade compile consumer'
		width:        2
		height:       2
		min_width:    1
		min_height:   1
		resizable:    true
		visible:      true
		high_dpi:     true
		borderless:   false
		fullscreen:   false
		clear_color:  gg.Color{}
		sample_count: 1
		redraw_mode:  .on_demand
		init_fn:      on_window_init
		frame_fn:     on_window_frame
		cleanup_fn:   on_window_cleanup
	})!
	consume_app_surface(mut app, window)!
}

fn main() {
	_ = gg.MultiWindowBackend.auto
	_ = gg.MultiWindowBackend.mock
	_ = gg.MultiWindowBackend.x11
	_ = gg.MultiWindowBackend.wayland
	_ = gg.MultiWindowBackend.appkit
	_ = gg.MultiWindowBackend.win32
	_ = gg.WindowEventKind.window_created
	_ = gg.WindowEventKind.window_destroyed
	_ = gg.WindowEventKind.window_close_requested
	_ = gg.WindowEventKind.window_resized
	_ = gg.WindowResizeEdge.top
	_ = gg.WindowResizeEdge.bottom
	_ = gg.WindowResizeEdge.left
	_ = gg.WindowResizeEdge.right
	_ = gg.WindowResizeEdge.top_left
	_ = gg.WindowResizeEdge.top_right
	_ = gg.WindowResizeEdge.bottom_left
	_ = gg.WindowResizeEdge.bottom_right
	_ = gg.WindowCursorShape.default
	_ = gg.WindowCursorShape.pointer
	_ = gg.WindowCursorShape.move
	_ = gg.WindowCursorShape.n_resize
	_ = gg.WindowCursorShape.s_resize
	_ = gg.WindowCursorShape.e_resize
	_ = gg.WindowCursorShape.w_resize
	_ = gg.WindowCursorShape.ne_resize
	_ = gg.WindowCursorShape.nw_resize
	_ = gg.WindowCursorShape.se_resize
	_ = gg.WindowCursorShape.sw_resize
	_ = gg.WindowCursorShape.ew_resize
	_ = gg.WindowCursorShape.ns_resize
	_ = gg.WindowCursorShape.nesw_resize
	_ = gg.WindowCursorShape.nwse_resize
	_ = gg.WindowCursorShape.grab
	_ = gg.WindowCursorShape.grabbing
	_ = gg.WindowRedrawMode.on_demand
	_ = gg.WindowRedrawMode.continuous
	_ = gg.WindowCleanupReason.requested
	_ = gg.WindowCleanupReason.native_closed
	_ = gg.WindowCleanupReason.init_failed
	_ = gg.WindowCleanupReason.app_stop
	_ = gg.WindowCleanupReason.renderer_lost
	_ = gg.WindowReadbackStatus.ready
	_ = gg.WindowReadbackStatus.cancelled
	_ = gg.WindowReadbackStatus.failed
	_ = gg.AppConfig{
		backend:          .mock
		queue_size:       1
		require_renderer: true
	}
	window_config := gg.WindowConfig{
		title:        'surface fields'
		width:        2
		height:       2
		min_width:    1
		min_height:   1
		resizable:    true
		visible:      true
		high_dpi:     true
		borderless:   false
		fullscreen:   false
		clear_color:  gg.Color{}
		sample_count: 1
		redraw_mode:  .on_demand
		init_fn:      on_window_init
		frame_fn:     on_window_frame
		cleanup_fn:   on_window_cleanup
	}
	_ = window_config
	_ = gg.RunConfig{
		frame_fn:                on_legacy_app_frame
		event_fn:                on_app_event
		input_fn:                on_app_input
		app_resource_init_fn:    on_app_resource_init
		app_resource_frame_fn:   on_app_resource_frame
		app_resource_cleanup_fn: on_app_resource_cleanup
		readback_fn:             on_readback
		max_pending_jobs:        1
	}
	window_id := gg.WindowId{}
	window_info := gg.WindowInfo{
		id:                 window_id
		title:              'snapshot'
		width:              2
		height:             2
		min_width:          1
		min_height:         1
		resizable:          true
		visible:            true
		high_dpi:           true
		borderless:         false
		fullscreen:         false
		native_decorations: true
	}
	_ = window_info.id
	_ = window_info.title
	_ = window_info.width
	_ = window_info.height
	_ = window_info.min_width
	_ = window_info.min_height
	_ = window_info.resizable
	_ = window_info.visible
	_ = window_info.high_dpi
	_ = window_info.borderless
	_ = window_info.fullscreen
	_ = window_info.native_decorations
	capabilities := gg.Capabilities{
		backend:                 .mock
		mock:                    true
		native:                  true
		multi_window:            true
		owner_queue:             true
		explicit_swapchain:      true
		readback:                true
		d3d11:                   true
		metal:                   true
		x11:                     true
		wayland:                 true
		win32:                   true
		gl:                      true
		input_events:            true
		mouse_events:            true
		keyboard_events:         true
		text_events:             true
		focus_events:            true
		drop_events:             true
		touch_events:            true
		cursor_shapes:           true
		interactive_move_resize: true
		native_decorations:      true
	}
	_ = capabilities
	_ = gg.WindowEvent{
		kind:   .window_created
		window: window_id
		width:  2
		height: 2
	}
	_ = gg.WindowInputEvent{
		window:        window_id
		event:         gg.Event{}
		dropped_files: ['file']
	}
	_ = gg.WindowReadbackCapabilities{
		offscreen_image: true
		window_capture:  true
	}
	logical_size := gg.WindowLogicalSize{
		width:  1
		height: 1
	}
	pixel_size := gg.WindowPixelSize{
		width:  1
		height: 1
	}
	logical_rect := gg.WindowLogicalRect{
		x:      0
		y:      0
		width:  1
		height: 1
	}
	pixel_rect := gg.WindowPixelRect{
		x:      0
		y:      0
		width:  1
		height: 1
	}
	metrics := gg.WindowMetrics{
		logical_size:     logical_size
		framebuffer_size: pixel_size
		dpi_scale:        1
		metrics_sequence: 1
		submitted_frame:  1
	}
	target := gg.WindowRenderTargetInfo{
		color_format: .rgba8
		depth_format: .depth
		sample_count: 1
	}
	_ = gg.WindowFrameInfo{
		window:          gg.WindowId{}
		frame_serial:    2
		submitted_frame: 1
		metrics:         metrics
		target:          target
	}
	_ = gg.WindowBufferId{}
	_ = gg.WindowImageId{}
	_ = gg.WindowSamplerId{}
	_ = gg.WindowShaderId{}
	_ = gg.WindowPipelineId{}
	_ = gg.WindowAttachmentsId{}
	_ = gg.WindowSglPipelineId{}
	_ = gg.WindowReadbackId{}
	_ = gg.WindowOffscreenPassConfig{
		attachments: gg.WindowAttachmentsId{}
		action:      gfx.PassAction{}
	}
	_ = gg.WindowAttachmentsConfig{
		colors:        [gg.WindowImageId{}]
		resolves:      [gg.WindowImageId{}]
		depth_stencil: gg.WindowImageId{}
	}
	buffer_binding := gg.WindowBufferBinding{
		slot:   0
		buffer: gg.WindowBufferId{}
		offset: 0
	}
	image_binding := gg.WindowImageBinding{
		slot:  0
		image: gg.WindowImageId{}
	}
	sampler_binding := gg.WindowSamplerBinding{
		slot:    0
		sampler: gg.WindowSamplerId{}
	}
	stage_bindings := gg.WindowStageBindings{
		images:          [image_binding]
		samplers:        [sampler_binding]
		storage_buffers: [buffer_binding]
	}
	_ = gg.WindowBindings{
		vertex_buffers: [buffer_binding]
		index_buffer:   buffer_binding
		vs:             stage_bindings
		fs:             stage_bindings
	}
	_ = gg.WindowReadbackConfig{
		rect: pixel_rect
	}
	_ = gg.WindowReadbackResult{
		id:              gg.WindowReadbackId{}
		window:          gg.WindowId{}
		status:          .ready
		submitted_frame: 1
		width:           1
		height:          1
		stride:          4
		pixels_rgba8:    [u8(0), 0, 0, 0]
		error:           ''
	}
	_ = gg.NativeWindowLease{}
	_ = gg.AppJobFn(on_app_job)
	_ = gg.AppFrameFn(on_legacy_app_frame)
	_ = gg.AppEventFn(on_app_event)
	_ = gg.AppInputFn(on_app_input)
	_ = gg.WindowDrawFn(on_window_frame)
	_ = gg.WindowInitFn(on_window_init)
	_ = gg.WindowFrameFn(on_window_frame)
	_ = gg.WindowCleanupFn(on_window_cleanup)
	_ = gg.AppResourceInitFn(on_app_resource_init)
	_ = gg.AppResourceFrameFn(on_app_resource_frame)
	_ = gg.AppResourceCleanupFn(on_app_resource_cleanup)
	_ = gg.WindowReadbackFn(on_readback)
	_ = gg.WindowResourceFn(consume_window_resources)
	_ = gg.WindowPassFn(consume_window_pass)
	_ = gg.WindowSglFn(consume_window_sgl)
	_ = gg.NativeWindowBorrowFn(consume_native_window)
	_ = logical_rect
	_ = consume_app_surface
	_ = consume_facade_constructors
}
