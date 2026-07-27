module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend Win32Backend) create_renderer_anchor() ! {
		$if windows && sokol_d3d11 ? {
			if backend.anchor_committed {
				if backend.renderer_ready() && native_identity(backend.anchor_color_texture) != 0
					&& backend.anchor_color_texture_ticket != 0
					&& native_identity(backend.anchor_render_view) != 0
					&& backend.anchor_render_view_ticket != 0
					&& native_identity(backend.anchor_depth_texture) != 0
					&& backend.anchor_depth_texture_ticket != 0
					&& native_identity(backend.anchor_depth_stencil_view) != 0
					&& backend.anchor_depth_stencil_view_ticket != 0 {
					return
				}
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			if backend.has_anchor_ownership() {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			backend.anchor_committed = false
			if !backend.renderer_ready() {
				return error(err_render_native_renderer_unavailable)
			}
			seed := NativeOperationSeed{
				call_site: .anchor_create
				scope:     .anchor
			}
			mut ordinals := backend.native_operations.reserve_ordinals(12) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			mut cleanup := ordinals.split_tail(4) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			color_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			view_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
				backend.native_operations.burn_lifetime_ticket(color_ticket)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			depth_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
				backend.native_operations.burn_lifetime_ticket(view_ticket)
				backend.native_operations.burn_lifetime_ticket(color_ticket)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			depth_view_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
				backend.native_operations.burn_lifetime_ticket(depth_ticket)
				backend.native_operations.burn_lifetime_ticket(view_ticket)
				backend.native_operations.burn_lifetime_ticket(color_ticket)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			color_seed := seed.with_target_identity(native_identity(backend.device))
			color_context := ordinals.materialize(backend.native_operations, .dxgi,
				.color_texture_create, color_seed) or {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				backend.native_operations.burn_lifetime_ticket(depth_ticket)
				backend.native_operations.burn_lifetime_ticket(view_ticket)
				backend.native_operations.burn_lifetime_ticket(color_ticket)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_win32_d3d11_create_texture(native_identity(backend.device), 1, 1, 0,
				&raw)
			color_texture := native_pointer(raw.handle)
			backend.bind_or_burn_com_lifetime_ticket(color_ticket, color_texture)
			backend.anchor_color_texture = color_texture
			backend.anchor_color_texture_ticket = if native_identity(color_texture) == 0 {
				u64(0)
			} else {
				color_ticket
			}
			color_result := backend.accept_dxgi_result(color_context, mut ordinals, color_seed,
				native_identity(backend.device), raw, .none)
			if !color_result.succeeded() {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				backend.native_operations.burn_lifetime_ticket(depth_ticket)
				backend.native_operations.burn_lifetime_ticket(view_ticket)
				if !backend.release_anchor_ownership(seed) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				}
				return native_render_error(color_result)
			}

			view_seed := seed.with_target_identity(native_identity(color_texture))
			view_context := ordinals.materialize(backend.native_operations, .dxgi,
				.render_view_create, view_seed) or {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				backend.native_operations.burn_lifetime_ticket(depth_ticket)
				backend.native_operations.burn_lifetime_ticket(view_ticket)
				_ = backend.release_anchor_ownership(seed)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			raw = C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_win32_d3d11_create_render_view(native_identity(backend.device),
				native_identity(color_texture), &raw)
			render_view := native_pointer(raw.handle)
			backend.bind_or_burn_com_lifetime_ticket(view_ticket, render_view)
			backend.anchor_render_view = render_view
			backend.anchor_render_view_ticket = if native_identity(render_view) == 0 {
				u64(0)
			} else {
				view_ticket
			}
			view_result := backend.accept_dxgi_result(view_context, mut ordinals, view_seed,
				native_identity(backend.device), raw, .none)
			if !view_result.succeeded() {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				backend.native_operations.burn_lifetime_ticket(depth_ticket)
				if !backend.release_anchor_ownership(seed) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				}
				return native_render_error(view_result)
			}

			depth_seed := seed.with_target_identity(native_identity(backend.device))
			depth_context := ordinals.materialize(backend.native_operations, .dxgi,
				.depth_texture_create, depth_seed) or {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				backend.native_operations.burn_lifetime_ticket(depth_ticket)
				_ = backend.release_anchor_ownership(seed)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			raw = C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_win32_d3d11_create_texture(native_identity(backend.device), 1, 1, 1,
				&raw)
			depth_texture := native_pointer(raw.handle)
			backend.bind_or_burn_com_lifetime_ticket(depth_ticket, depth_texture)
			backend.anchor_depth_texture = depth_texture
			backend.anchor_depth_texture_ticket = if native_identity(depth_texture) == 0 {
				u64(0)
			} else {
				depth_ticket
			}
			depth_result := backend.accept_dxgi_result(depth_context, mut ordinals, depth_seed,
				native_identity(backend.device), raw, .none)
			if !depth_result.succeeded() {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				if !backend.release_anchor_ownership(seed) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				}
				return native_render_error(depth_result)
			}

			depth_view_seed := seed.with_target_identity(native_identity(depth_texture))
			depth_view_context := ordinals.materialize(backend.native_operations, .dxgi,
				.depth_view_create, depth_view_seed) or {
				backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
				_ = backend.release_anchor_ownership(seed)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			raw = C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_win32_d3d11_create_depth_view(native_identity(backend.device),
				native_identity(depth_texture), &raw)
			depth_view := native_pointer(raw.handle)
			backend.bind_or_burn_com_lifetime_ticket(depth_view_ticket, depth_view)
			backend.anchor_depth_stencil_view = depth_view
			backend.anchor_depth_stencil_view_ticket = if native_identity(depth_view) == 0 {
				u64(0)
			} else {
				depth_view_ticket
			}
			depth_view_result := backend.accept_dxgi_result(depth_view_context, mut ordinals,
				depth_view_seed, native_identity(backend.device), raw, .none)
			if !depth_view_result.succeeded() {
				if !backend.release_anchor_ownership(seed) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				}
				return native_render_error(depth_view_result)
			}
			if !backend.renderer_ready() {
				_ = backend.release_anchor_ownership(seed)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			backend.anchor_committed = true
			return
		} $else {
			return error(err_renderer_unsupported)
		}
	}

	fn (mut backend Win32Backend) destroy_renderer_anchor() ! {
		$if windows && sokol_d3d11 ? {
			seed := NativeOperationSeed{
				call_site: .shutdown_release
				scope:     .anchor
			}
			if !backend.release_anchor_ownership(seed) {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			return
		} $else {
			_ = backend
			return
		}
	}

	fn (mut backend Win32Backend) renderer_environment() !gfx.Environment {
		$if windows && sokol_d3d11 ? {
			if !backend.anchor_committed || backend.anchor_render_view == unsafe { nil }
				|| !backend.renderer_ready() {
				return error(err_render_anchor_failed)
			}
			return gfx.Environment{
				defaults: gfx.EnvironmentDefaults{
					color_format: .bgra8
					depth_format: .depth_stencil
					sample_count: 1
				}
				d3d11:    gfx.D3d11Environment{
					device:         backend.device
					device_context: backend.device_context
				}
			}
		} $else {
			return error(err_renderer_unsupported)
		}
	}

	fn (mut backend Win32Backend) begin_renderer_anchor() !RenderFrame {
		$if windows && sokol_d3d11 ? {
			if !backend.anchor_committed || backend.render_health.blocks_graphics()
				|| backend.anchor_render_view == unsafe { nil }
				|| backend.anchor_depth_stencil_view == unsafe { nil }
				|| backend.anchor_render_view_ticket == 0
				|| backend.anchor_depth_stencil_view_ticket == 0 {
				return error(err_render_anchor_failed)
			}
			return RenderFrame{
				swapchain: gfx.Swapchain{
					width:        1
					height:       1
					sample_count: 1
					color_format: .bgra8
					depth_format: .depth_stencil
					d3d11:        gfx.D3d11Swapchain{
						render_view:        backend.anchor_render_view
						depth_stencil_view: backend.anchor_depth_stencil_view
					}
				}
			}
		} $else {
			return error(err_renderer_unsupported)
		}
	}

	fn (mut backend Win32Backend) prepare_renderer_anchor_context() ! {
		if !backend.anchor_committed || backend.render_health.blocks_graphics()
			|| backend.anchor_render_view == unsafe { nil }
			|| backend.anchor_depth_stencil_view == unsafe { nil } {
			return error(err_render_anchor_failed)
		}
	}

	fn (mut backend Win32Backend) end_renderer_anchor(frame RenderFrame) ! {
		if !backend.anchor_committed || backend.anchor_render_view == unsafe { nil }
			|| frame.swapchain.d3d11.render_view != backend.anchor_render_view {
			return error(err_render_anchor_failed)
		}
	}

	fn (mut backend Win32Backend) abort_renderer_anchor(frame RenderFrame) ! {
		backend.end_renderer_anchor(frame)!
	}

	fn (mut backend Win32Backend) activate_render_frame(frame RenderFrame) BackendFrameAttempt {
		$if windows && sokol_d3d11 ? {
			index := backend.window_record_index(frame.window_id) or {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .backbuffer_acquire, .window_target,
						.operation_failed, 0, 0, err_window_not_found)
				}
			}
			record := backend.windows[index]
			if backend.render_health.blocks_graphics() {
				disposition := if backend.render_health == .lost {
					NativeRenderDisposition.renderer_lost
				} else {
					NativeRenderDisposition.renderer_unavailable
				}
				return BackendFrameAttempt{
					outcome: native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						disposition, 0, 0, err_render_native_renderer_lost)
				}
			}
			if frame.acquired || record.swapchain == unsafe { nil }
				|| record.render_view == unsafe { nil }
				|| record.depth_stencil_view == unsafe { nil } || record.swapchain_ticket == 0
				|| record.render_view_ticket == 0 || record.depth_stencil_view_ticket == 0
				|| frame.swapchain.d3d11.render_view != record.render_view
				|| frame.target.target_identity != record.render_target_generation
				|| frame.metrics.framebuffer_width != record.framebuffer_width
				|| frame.metrics.framebuffer_height != record.framebuffer_height {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .backbuffer_acquire, .window_target,
						.target_lost, 0, 0, err_render_target_stale)
				}
			}
			return BackendFrameAttempt{
				frame:   RenderFrame{
					...frame
					acquired:  true
					swapchain: backend.swapchain_for_record(record)
				}
				outcome: native_render_ok(.dxgi, .backbuffer_acquire, .window_target)
			}
		} $else {
			_ = frame
			return BackendFrameAttempt{
				outcome: native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
					.renderer_unavailable, 0, 0, err_renderer_unsupported)
			}
		}
	}

	fn (mut backend Win32Backend) abort_render_frame(frame RenderFrame) ! {
		backend.window_record_index(frame.window_id) or { return error(err_window_not_found) }
	}

	fn (mut backend Win32Backend) prove_renderer_shutdown_anchor() RendererShutdownProof {
		$if windows && sokol_d3d11 ? {
			if !backend.anchor_committed || !backend.renderer_ready()
				|| backend.anchor_render_view == unsafe { nil }
				|| backend.anchor_depth_stencil_view == unsafe { nil } {
				disposition := if backend.render_health == .lost {
					NativeRenderDisposition.renderer_lost
				} else {
					NativeRenderDisposition.renderer_unavailable
				}
				return RendererShutdownProof{
					path:    .logical_abandon
					outcome: native_render_outcome(.dxgi, .device_status, .anchor, disposition, 0,
						0, err_render_anchor_failed)
				}
			}
			seed := NativeOperationSeed{
				presence_mask:   native_context_has_target_identity
				call_site:       .shutdown_anchor
				scope:           .renderer
				target_identity: native_identity(backend.device)
			}
			mut ordinals := backend.native_operations.reserve_ordinals(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return RendererShutdownProof{
					path:    .logical_abandon
					outcome: native_render_outcome(.dxgi, .device_status, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
			}
			context := ordinals.materialize(backend.native_operations, .dxgi, .device_status, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return RendererShutdownProof{
					path:    .logical_abandon
					outcome: native_render_outcome(.dxgi, .device_status, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_win32_d3d11_device_status(native_identity(backend.device), &raw)
			capture := backend.native_operations.capture_call(context, raw)
			outcome := backend.native_operations.accept_dxgi(context, capture, .none)
			backend.render_health = renderer_health_after_result(backend.render_health, outcome)
			backend.native_operations.record_health_latch(outcome.context, backend.render_health)
			return RendererShutdownProof{
				path:    if outcome.succeeded() { .orderly_anchor } else { .logical_abandon }
				outcome: outcome
			}
		} $else {
			return RendererShutdownProof{
				path:    .logical_abandon
				outcome: native_render_outcome(.dxgi, .device_status, .renderer,
					.renderer_unavailable, 0, 0, err_renderer_unsupported)
			}
		}
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		struct Win32RenderObservation {
			available            bool
			visible              bool
			minimized            bool
			logical_width        f32
			logical_height       f32
			framebuffer_width    int
			framebuffer_height   int
			dpi_scale            f32
			conversion_available bool
		}

		fn win32_render_observation(available bool, visible int, minimized int, framebuffer_width int, framebuffer_height int, dpi_scale f32, conversion_available int) Win32RenderObservation {
			return Win32RenderObservation{
				available:            available
				visible:              visible != 0
				minimized:            minimized != 0
				logical_width:        if dpi_scale > 0 {
					f32(framebuffer_width) / dpi_scale
				} else {
					f32(0)
				}
				logical_height:       if dpi_scale > 0 {
					f32(framebuffer_height) / dpi_scale
				} else {
					f32(0)
				}
				framebuffer_width:    framebuffer_width
				framebuffer_height:   framebuffer_height
				dpi_scale:            dpi_scale
				conversion_available: conversion_available != 0
			}
		}

		fn win32_render_update(window WindowId, sequence u64, health NativeRendererHealth, target_generation u64, resize_pending bool, observation Win32RenderObservation) BackendRenderUpdate {
			metrics_available := observation.available && observation.logical_width > 0
				&& observation.logical_height > 0 && observation.framebuffer_width > 0
				&& observation.framebuffer_height > 0 && observation.dpi_scale > 0
			ready := health == .ready && target_generation != 0 && observation.visible
				&& !observation.minimized && !resize_pending && metrics_available
			block := if health.blocks_graphics() || target_generation == 0 {
				RenderBlockReason.renderer_failed
			} else if !observation.available {
				RenderBlockReason.backend_unavailable
			} else if !observation.visible {
				RenderBlockReason.hidden
			} else if observation.minimized {
				RenderBlockReason.minimized
			} else if observation.logical_width <= 0 || observation.logical_height <= 0
				|| observation.framebuffer_width <= 0 || observation.framebuffer_height <= 0 {
				RenderBlockReason.zero_sized
			} else if observation.dpi_scale <= 0 {
				RenderBlockReason.backend_unavailable
			} else if resize_pending {
				RenderBlockReason.resize_pending
			} else {
				RenderBlockReason.none
			}
			return BackendRenderUpdate{
				window:       window
				sequence:     sequence
				ready_credit: ready
				block_reason: block
				metrics:      RenderMetricsSnapshot{
					logical_width:        observation.logical_width
					logical_height:       observation.logical_height
					framebuffer_width:    observation.framebuffer_width
					framebuffer_height:   observation.framebuffer_height
					dpi_scale:            observation.dpi_scale
					metrics_sequence:     sequence
					metrics_available:    metrics_available
					conversion_available: metrics_available && observation.conversion_available
				}
				target:       RenderTargetSnapshot{
					target_identity: target_generation
					color_format:    int(gfx.PixelFormat.bgra8)
					depth_format:    int(gfx.PixelFormat.depth_stencil)
					sample_count:    1
				}
			}
		}
	}
}

fn (mut backend Win32Backend) collect_render_updates() ![]BackendRenderUpdate {
	mut updates := []BackendRenderUpdate{}
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if windows && sokol_d3d11 ? {
			for i in 0 .. backend.windows.len {
				mut record := backend.windows[i]
				if record.destroyed || record.hwnd == unsafe { nil } {
					continue
				}
				mut visible := 0
				mut minimized := 0
				mut width := 0
				mut height := 0
				mut framebuffer_width := 0
				mut framebuffer_height := 0
				mut scale := f32(0)
				mut conversion_available := 0
				available := if backend.render_health.blocks_graphics() {
					false
				} else {
					C.v_multiwindow_win32_render_snapshot(record.hwnd, &visible, &minimized,
						&width, &height, &framebuffer_width, &framebuffer_height, &scale,
						&conversion_available) != 0
				}
				if available {
					if !record.render_resize_pending && record.framebuffer_width > 0
						&& record.framebuffer_height > 0 && framebuffer_width > 0
						&& framebuffer_height > 0 && (record.framebuffer_width != framebuffer_width
						|| record.framebuffer_height != framebuffer_height) {
						record.render_target_generation =
							exhaust_backend_target_generation(record.render_target_generation)
						record.render_resize_pending = true
					}
					record.width = width
					record.height = height
					record.framebuffer_width = framebuffer_width
					record.framebuffer_height = framebuffer_height
				}
				was_resize_pending := record.render_resize_pending
				if available && visible != 0 && minimized == 0 && width > 0 && height > 0
					&& framebuffer_width > 0 && framebuffer_height > 0 && was_resize_pending {
					resize := backend.ensure_window_render_target(i, NativeOperationSeed{
						presence_mask:     native_context_has_window | native_context_has_target_generation
						call_site:         .display_transport
						scope:             .window_target
						window:            record.id
						target_generation: record.render_target_generation
					})
					if !resize.succeeded() {
						return native_render_error(resize)
					}
				}
				backend.render_sequence = next_backend_render_sequence(backend.render_sequence)!
				observation := win32_render_observation(available, visible, minimized,
					framebuffer_width, framebuffer_height, scale, conversion_available)
				updates << win32_render_update(record.id, backend.render_sequence,
					backend.render_health, record.render_target_generation, was_resize_pending,
					observation)
			}
		}
	}
	return updates
}
