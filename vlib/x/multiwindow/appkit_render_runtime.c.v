module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend AppKitBackend) create_renderer_anchor() ! {
		$if darwin {
			if backend.anchor_state != unsafe { nil } || backend.anchor_state_ticket != 0
				|| backend.anchor_native_destroyed {
				if backend.anchor_state != unsafe { nil } && backend.anchor_state_ticket != 0
					&& !backend.anchor_native_destroyed {
					return
				}
				return error(err_render_anchor_failed)
			}
			if !backend.renderer_ready() {
				return error(err_renderer_unsupported)
			}
			seed := NativeOperationSeed{
				presence_mask:   native_context_has_target_identity
				call_site:       .anchor_create
				scope:           .anchor
				target_identity: native_identity(backend.device)
			}
			mut ordinals := backend.native_operations.reserve_ordinals(3) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			mut cleanup := ordinals.split_tail(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			mut state_ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup, .appkit_state, seed) or {
				return error(err_render_anchor_failed)
			}
			context := ordinals.materialize(backend.native_operations, .metal,
				.anchor_surface_create, seed) or {
				if !backend.native_operations.burn_lifetime_ticket(state_ticket) {
					backend.anchor_state = unsafe { nil }
					backend.anchor_state_ticket = state_ticket
					backend.anchor_native_destroyed = false
					_ = backend.release_anchor_state_lifetime(seed)
				}
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			raw := C.v_multiwindow_appkit_create_renderer_anchor(backend.device)
			state := native_pointer(raw.handle)
			state_identity := raw.handle
			if state == unsafe { nil } {
				if backend.native_operations.burn_lifetime_ticket(state_ticket) {
					state_ticket = 0
				}
			} else {
				backend.native_operations.bind_lifetime_ticket(state_ticket, state_identity, 0)
			}
			backend.anchor_state = state
			backend.anchor_state_ticket = state_ticket
			backend.anchor_native_destroyed = false
			result := backend.accept_metal_result(context, raw, .none, err_render_anchor_failed)
			if !result.succeeded() {
				if backend.anchor_state != unsafe { nil } {
					cleanup_seed := seed.with_target_identity(state_identity)
					if backend.render_health.blocks_graphics() {
						ordinals.skip(1) or {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						backend.anchor_native_destroyed = true
					} else {
						cleanup_context := ordinals.materialize(backend.native_operations, .metal,
							.surface_destroy, cleanup_seed) or {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
							backend.anchor_native_destroyed = true
							NativeOperationContext{}
						}
						if cleanup_context.ordinal != 0 {
							cleanup_raw :=
								C.v_multiwindow_appkit_destroy_renderer_anchor(backend.anchor_state)
							backend.anchor_native_destroyed = true
							backend.accept_metal_result(cleanup_context, cleanup_raw,
								.void_completion, err_render_anchor_failed)
						}
					}
				} else {
					ordinals.skip(1) or {}
				}
				anchor_lifetime_released := backend.release_anchor_state_lifetime(seed)
				if !anchor_lifetime_released {
					return error(err_render_anchor_failed)
				}
				return error(err_render_anchor_failed)
			}
			ordinals.skip(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.anchor_native_destroyed = true
				_ = backend.release_anchor_state_lifetime(seed)
				return error(err_render_anchor_failed)
			}
			return
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) destroy_renderer_anchor() ! {
		$if darwin {
			if backend.render_health.blocks_graphics() {
				if !backend.release_renderer_lifetime() {
					return error(err_render_anchor_failed)
				}
				return error(err_render_native_renderer_unavailable)
			}
			if appkit_lifetime_pair_is_empty(backend.anchor_state, backend.anchor_state_ticket) {
				backend.anchor_native_destroyed = false
				return
			}
			if backend.batch_autorelease_pool != unsafe { nil }
				|| backend.batch_autorelease_pool_ticket != 0 {
				return error(err_render_batch_active)
			}
			if !backend.release_anchor_drawable_lifetime(err_render_anchor_failed) {
				return error(err_render_anchor_failed)
			}
			if !backend.release_anchor_state_lifetime(NativeOperationSeed{
				call_site: .shutdown_release
				scope:     .anchor
			}) {
				return error(err_render_anchor_failed)
			}
			return
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) renderer_environment() !gfx.Environment {
		$if darwin {
			if backend.anchor_state == unsafe { nil } || backend.anchor_state_ticket == 0
				|| backend.anchor_native_destroyed || !backend.renderer_ready() {
				return error(err_render_anchor_failed)
			}
			return gfx.Environment{
				defaults: gfx.EnvironmentDefaults{
					color_format: .bgra8
					depth_format: .depth_stencil
					sample_count: 1
				}
				metal:    gfx.MetalEnvironment{
					device: backend.device
				}
			}
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) begin_renderer_anchor() !RenderFrame {
		$if darwin {
			if backend.render_health.blocks_graphics() || backend.anchor_state == unsafe { nil }
				|| backend.anchor_state_ticket == 0 || backend.anchor_native_destroyed
				|| backend.batch_autorelease_pool == unsafe { nil }
				|| backend.batch_autorelease_pool_ticket == 0 || backend.active_anchor_lease != 0
				|| backend.active_anchor_drawable != unsafe { nil }
				|| backend.active_anchor_drawable_ticket != 0 {
				return error(err_render_anchor_failed)
			}
			lease, next_anchor_lease := plan_nonwrapping_counter(backend.next_anchor_lease) or {
				return error(err_render_renderer_failed)
			}
			backend.next_anchor_lease = next_anchor_lease
			seed := NativeOperationSeed{
				presence_mask:   native_context_has_target_identity
				call_site:       .anchor_prepare
				scope:           .anchor
				target_identity: native_identity(backend.anchor_state)
			}
			mut ordinals := backend.native_operations.reserve_ordinals(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			mut cleanup := ordinals.split_tail(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			mut drawable_ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup,
				.metal_drawable, seed) or { return error(err_render_anchor_failed) }
			context := ordinals.materialize(backend.native_operations, .metal, .drawable_acquire, seed) or {
				if !backend.native_operations.burn_lifetime_ticket(drawable_ticket) {
					backend.active_anchor_lease = lease
					backend.active_anchor_drawable = unsafe { nil }
					backend.active_anchor_drawable_ticket = drawable_ticket
					_ = backend.release_anchor_drawable_lifetime(err_render_anchor_failed)
				}
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			raw := C.v_multiwindow_appkit_begin_frame(backend.anchor_state, backend.device)
			drawable := native_pointer(raw.handle)
			if drawable == unsafe { nil } {
				if backend.native_operations.burn_lifetime_ticket(drawable_ticket) {
					drawable_ticket = 0
				}
			} else {
				backend.native_operations.bind_lifetime_ticket(drawable_ticket,
					native_identity(drawable), native_identity(backend.anchor_state))
			}
			backend.active_anchor_lease = lease
			backend.active_anchor_drawable = drawable
			backend.active_anchor_drawable_ticket = drawable_ticket
			result := backend.accept_metal_result(context, raw, .none, err_render_anchor_failed)
			if !result.succeeded() {
				_ = backend.release_anchor_drawable_lifetime(err_render_anchor_failed)
				return error(err_render_anchor_failed)
			}
			depth_texture := native_pointer(raw.object_identity_0)
			width := int(raw.observed_count)
			height := int(raw.selected_value)
			return RenderFrame{
				backend_lease: lease
				swapchain:     gfx.Swapchain{
					width:        width
					height:       height
					sample_count: 1
					color_format: .bgra8
					depth_format: .depth_stencil
					metal:        gfx.MetalSwapchain{
						current_drawable:      drawable
						depth_stencil_texture: depth_texture
					}
				}
			}
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) prepare_renderer_anchor_context() ! {
		proof := backend.prove_renderer_shutdown_anchor()
		if proof.path != .orderly_anchor {
			return native_render_error(proof.outcome)
		}
	}

	fn (mut backend AppKitBackend) end_renderer_anchor(frame RenderFrame) ! {
		$if darwin {
			drawable := frame.swapchain.metal.current_drawable
			if backend.render_health.blocks_graphics() {
				_ = backend.release_active_frames_lifetime()
				return error(err_render_anchor_failed)
			}
			if backend.anchor_state == unsafe { nil } || frame.backend_lease == 0
				|| backend.active_anchor_lease != frame.backend_lease
				|| backend.active_anchor_drawable != drawable {
				return error(err_render_anchor_failed)
			}
			seed := NativeOperationSeed{
				presence_mask:   native_context_has_target_identity
				call_site:       .window_finalize
				scope:           .anchor
				target_identity: native_identity(drawable)
			}
			mut ordinals := backend.native_operations.reserve_ordinals(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			context := ordinals.materialize(backend.native_operations, .metal, .present, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			claim := backend.native_operations.claim_lifetime_release(backend.active_anchor_drawable_ticket,
				.metal_drawable, native_identity(drawable), native_identity(backend.anchor_state)) or {
				return error(err_render_anchor_failed)
			}
			raw := C.v_multiwindow_appkit_end_frame(backend.anchor_state, drawable)
			released_drawable := backend.active_anchor_drawable
			released_ticket := backend.active_anchor_drawable_ticket
			backend.active_anchor_drawable = unsafe { nil }
			backend.active_anchor_lease = 0
			released, result := backend.complete_appkit_drawable_release(claim, context, raw,
				released_drawable, released_ticket, err_render_anchor_failed)
			backend.active_anchor_drawable = released.value
			backend.active_anchor_drawable_ticket = released.ticket_id
			if !result.succeeded() || !released.ticket_retired {
				return native_render_error(result)
			}
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) abort_renderer_anchor(frame RenderFrame) ! {
		$if darwin {
			drawable := frame.swapchain.metal.current_drawable
			if backend.render_health.blocks_graphics() {
				_ = backend.release_active_frames_lifetime()
				return error(err_render_anchor_failed)
			}
			if backend.anchor_state == unsafe { nil } || frame.backend_lease == 0
				|| backend.active_anchor_lease != frame.backend_lease
				|| backend.active_anchor_drawable != drawable {
				return error(err_render_anchor_failed)
			}
			seed := NativeOperationSeed{
				presence_mask:   native_context_has_target_identity
				call_site:       .window_finalize
				scope:           .anchor
				target_identity: native_identity(drawable)
			}
			mut ordinals := backend.native_operations.reserve_ordinals(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			context := ordinals.materialize(backend.native_operations, .metal, .clear_state, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_anchor_failed)
			}
			claim := backend.native_operations.claim_lifetime_release(backend.active_anchor_drawable_ticket,
				.metal_drawable, native_identity(drawable), native_identity(backend.anchor_state)) or {
				return error(err_render_anchor_failed)
			}
			raw := C.v_multiwindow_appkit_abort_frame(backend.anchor_state, drawable)
			released_drawable := backend.active_anchor_drawable
			released_ticket := backend.active_anchor_drawable_ticket
			backend.active_anchor_drawable = unsafe { nil }
			backend.active_anchor_lease = 0
			released, result := backend.complete_appkit_drawable_release(claim, context, raw,
				released_drawable, released_ticket, err_render_anchor_failed)
			backend.active_anchor_drawable = released.value
			backend.active_anchor_drawable_ticket = released.ticket_id
			if !result.succeeded() || !released.ticket_retired {
				return native_render_error(result)
			}
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) activate_render_frame(frame RenderFrame) BackendFrameAttempt {
		$if darwin {
			if backend.render_health != .ready || backend.batch_autorelease_pool == unsafe { nil }
				|| backend.batch_autorelease_pool_ticket == 0 {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			index := backend.window_record_index(frame.window_id) or {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .window_target, .operation_failed, 0, 0,
						err_window_not_found))
				}
			}
			mut record := &backend.windows[index]
			if frame.acquired || !record.frame_active || frame.backend_lease == 0
				|| record.active_frame_lease != frame.backend_lease
				|| record.state == unsafe { nil } || record.state_ticket == 0
				|| record.active_drawable != unsafe { nil } || record.active_drawable_ticket != 0
				|| frame.target.target_identity != record.render_target_generation {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .window_target, .target_lost, 0, 0,
						err_render_target_stale))
				}
			}
			seed :=
				native_seed_for_frame(frame, .window_activate).with_target_identity(native_identity(record.state))
			mut ordinals := backend.native_operations.reserve_ordinals(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			mut cleanup := ordinals.split_tail(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			mut drawable_ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup,
				.metal_drawable, seed) or {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			context := ordinals.materialize(backend.native_operations, .metal, .drawable_acquire, seed) or {
				if backend.native_operations.burn_lifetime_ticket(drawable_ticket) {
					drawable_ticket = 0
				} else {
					record.active_drawable = unsafe { nil }
					record.active_drawable_ticket = drawable_ticket
				}
				_ = backend.release_window_drawable_lifetime(mut record, .close_frame,
					err_render_target_stale)
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			raw := C.v_multiwindow_appkit_begin_frame(record.state, backend.device)
			drawable := native_pointer(raw.handle)
			if drawable == unsafe { nil } {
				if backend.native_operations.burn_lifetime_ticket(drawable_ticket) {
					drawable_ticket = 0
				}
			} else {
				backend.native_operations.bind_lifetime_ticket(drawable_ticket,
					native_identity(drawable), native_identity(record.state))
			}
			record.active_drawable = drawable
			record.active_drawable_ticket = drawable_ticket
			result := backend.accept_metal_result(context, raw, .none,
				err_appkit_metal_drawable_failed)
			if !result.succeeded() {
				mode := if backend.render_health.blocks_graphics() {
					AppKitWindowDrawableReleaseMode.close_frame
				} else {
					AppKitWindowDrawableReleaseMode.preserve_for_abort
				}
				_ = backend.release_window_drawable_lifetime(mut record, mode,
					err_render_target_stale)
				return BackendFrameAttempt{
					outcome: result
				}
			}
			depth_texture := native_pointer(raw.object_identity_0)
			framebuffer_width := int(raw.observed_count)
			framebuffer_height := int(raw.selected_value)
			if framebuffer_width != frame.metrics.framebuffer_width
				|| framebuffer_height != frame.metrics.framebuffer_height
				|| frame.target.color_format != int(gfx.PixelFormat.bgra8)
				|| frame.target.depth_format != int(gfx.PixelFormat.depth_stencil)
				|| frame.target.sample_count != 1 {
				released := backend.release_window_drawable_lifetime(mut record,
					.preserve_for_abort, err_render_target_stale)
				record.framebuffer_width = framebuffer_width
				record.framebuffer_height = framebuffer_height
				record.render_target_generation =
					exhaust_backend_target_generation(record.render_target_generation)
				if !released {
					return BackendFrameAttempt{
						outcome: backend.record_metal_result(native_render_outcome(.metal,
							.clear_state, .window_target, .target_lost, 0, 0,
							err_render_target_stale))
					}
				}
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.drawable_acquire, .window_target, .target_lost, 0, 0,
						err_render_target_stale))
				}
			}
			return BackendFrameAttempt{
				frame:   RenderFrame{
					...frame
					acquired:  true
					swapchain: gfx.Swapchain{
						width:        framebuffer_width
						height:       framebuffer_height
						sample_count: 1
						color_format: .bgra8
						depth_format: .depth_stencil
						metal:        gfx.MetalSwapchain{
							current_drawable:      drawable
							depth_stencil_texture: depth_texture
						}
					}
				}
				outcome: result
			}
		} $else {
			_ = frame
			return BackendFrameAttempt{
				outcome: native_render_outcome(.metal, .drawable_acquire, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend AppKitBackend) prove_renderer_shutdown_anchor() RendererShutdownProof {
		$if darwin {
			if backend.renderer_ready() && backend.anchor_state != unsafe { nil }
				&& !backend.anchor_native_destroyed && C.v_multiwindow_appkit_is_main_thread() != 0 {
				return RendererShutdownProof{
					path:    .orderly_anchor
					outcome: backend.record_metal_result(native_render_ok(.metal, .device_status,
						.anchor))
				}
			}
			disposition := if backend.render_health == .lost {
				NativeRenderDisposition.renderer_lost
			} else {
				NativeRenderDisposition.renderer_unavailable
			}
			return RendererShutdownProof{
				path:    .logical_abandon
				outcome: backend.record_metal_result(native_render_outcome(.metal, .device_status,
					.anchor, disposition, 0, 0, err_render_native_renderer_unavailable))
			}
		} $else {
			return RendererShutdownProof{
				path:    .logical_abandon
				outcome: native_render_outcome(.metal, .device_status, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}
	}
}

fn (mut backend AppKitBackend) collect_render_updates() ![]BackendRenderUpdate {
	mut updates := []BackendRenderUpdate{}
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			for i in 0 .. backend.windows.len {
				mut record := &backend.windows[i]
				if record.native_destroyed {
					continue
				}
				mut visible := 0
				mut miniaturized := 0
				mut occluded := 0
				mut width := 0
				mut height := 0
				mut framebuffer_width := 0
				mut framebuffer_height := 0
				mut scale := f32(0)
				available := if backend.render_health.blocks_graphics() {
					false
				} else {
					C.v_multiwindow_appkit_render_snapshot(record.state, &visible, &miniaturized,
						&occluded, &width, &height, &framebuffer_width, &framebuffer_height, &scale) != 0
				}
				backend.render_sequence = next_backend_render_sequence(backend.render_sequence)!
				if available {
					if record.framebuffer_width != framebuffer_width
						|| record.framebuffer_height != framebuffer_height {
						record.render_target_generation =
							exhaust_backend_target_generation(record.render_target_generation)
					}
					record.width = width
					record.height = height
					record.framebuffer_width = framebuffer_width
					record.framebuffer_height = framebuffer_height
				}
				ready := backend.renderer_ready() && record.render_target_generation != 0
					&& available && visible != 0 && miniaturized == 0 && occluded == 0 && width > 0
					&& height > 0 && framebuffer_width > 0 && framebuffer_height > 0 && scale > 0
				block := if backend.render_health.blocks_graphics()
					|| record.render_target_generation == 0 {
					RenderBlockReason.renderer_failed
				} else if !available {
					RenderBlockReason.backend_unavailable
				} else if visible == 0 {
					RenderBlockReason.hidden
				} else if miniaturized != 0 {
					RenderBlockReason.minimized
				} else if occluded != 0 {
					RenderBlockReason.occluded
				} else if width <= 0 || height <= 0 || framebuffer_width <= 0
					|| framebuffer_height <= 0 {
					RenderBlockReason.zero_sized
				} else {
					RenderBlockReason.none
				}
				updates << BackendRenderUpdate{
					window:       record.id
					sequence:     backend.render_sequence
					ready_credit: ready
					block_reason: block
					metrics:      RenderMetricsSnapshot{
						logical_width:        f32(width)
						logical_height:       f32(height)
						framebuffer_width:    framebuffer_width
						framebuffer_height:   framebuffer_height
						dpi_scale:            scale
						metrics_sequence:     backend.render_sequence
						metrics_available:    available && scale > 0
						conversion_available: available && scale > 0
					}
					target:       RenderTargetSnapshot{
						target_identity: record.render_target_generation
						color_format:    int(gfx.PixelFormat.bgra8)
						depth_format:    int(gfx.PixelFormat.depth_stencil)
						sample_count:    1
					}
				}
			}
		}
	}
	return updates
}
