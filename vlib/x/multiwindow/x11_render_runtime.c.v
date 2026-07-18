module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if linux && x_multiwindow_x11 ? {
		fn C.v_multiwindow_x11_render_snapshot(display &C.Display, window X11NativeWindow, out_width &int, out_height &int, out_viewable &int) int
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend X11Backend) create_renderer_anchor() ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.anchor_surface != unsafe { nil } {
				return
			}
			if backend.anchor_generation == 0 {
				outcome := backend.record_egl_result(native_render_outcome(.egl,
					.anchor_surface_create, .anchor, .renderer_unavailable, 0, 0,
					err_render_native_renderer_unavailable))
				return native_render_error(outcome)
			}
			if !backend.renderer_ready() {
				return error(err_renderer_unsupported)
			}
			seed := NativeOperationSeed{
				presence_mask:     native_context_has_target_generation
				call_site:         .anchor_create
				scope:             .anchor
				target_generation: backend.anchor_generation
			}
			mut ordinals := backend.native_operations.reserve_ordinals(3) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			mut cleanup_ordinals := ordinals.split_tail(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			cleanup_ticket := backend.native_operations.reserve_linux_egl_lifetime_ticket(mut cleanup_ordinals,
				.egl_surface, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			create_seed := seed.with_target_identity(native_identity(backend.egl_config))
			context := ordinals.materialize(backend.native_operations, .egl,
				.anchor_surface_create, create_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				return error(err_render_native_renderer_unavailable)
			}
			C.v_multiwindow_linux_egl_create_pbuffer_surface(native_identity(backend.egl_display),
				native_identity(backend.egl_config), 1, 1, &raw)
			actual_surface := native_pointer(raw.handle)
			if actual_surface != unsafe { nil } {
				backend.anchor_surface = actual_surface
				backend.anchor_surface_ticket = cleanup_ticket
				backend.native_operations.bind_lifetime_ticket(cleanup_ticket,
					native_identity(actual_surface), native_identity(backend.egl_display))
			}
			outcome := backend.accept_egl_result(context, mut ordinals, create_seed, raw, .none)
			if actual_surface == unsafe { nil } {
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
			}
			if !outcome.succeeded() {
				release := backend.release_egl_surface_ticket(cleanup_ticket, actual_surface)
				if release.terminal {
					backend.anchor_surface = unsafe { nil }
					backend.anchor_surface_ticket = 0
				}
				return native_render_error(outcome)
			}
			binding := backend.make_renderer_anchor_current(.anchor_prepare)
			if !backend.anchor_binding_proven(binding) {
				if !backend.render_health.blocks_graphics() {
					backend.invalidate_egl_anchor()
				}
				return native_render_error(binding)
			}
			return
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) destroy_renderer_anchor() ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.anchor_surface == unsafe { nil } {
				return
			}
			if backend.render_health.blocks_graphics() {
				return error(err_render_native_renderer_lost)
			}
			if backend.egl_binding.kind == .anchor {
				seed := NativeOperationSeed{
					presence_mask:     native_context_has_target_generation | native_context_has_target_identity
					call_site:         .shutdown_anchor
					scope:             .anchor
					target_generation: backend.anchor_generation
					target_identity:   native_identity(backend.anchor_surface)
				}
				mut ordinals := backend.native_operations.reserve_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				context := ordinals.materialize(backend.native_operations, .egl, .make_current, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_linux_egl_make_current(native_identity(backend.egl_display), 0, 0,
					0, &raw)
				backend.accept_egl_result(context, mut ordinals, seed, raw, .none)
				backend.egl_binding = EglBindingIdentity{}
			}
			backend.invalidate_egl_anchor()
			return
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) renderer_environment() !gfx.Environment {
		$if linux && x_multiwindow_x11 ? {
			outcome := backend.make_renderer_anchor_current(.anchor_prepare)
			if !backend.anchor_binding_proven(outcome) {
				return native_render_error(outcome)
			}
			return gfx.Environment{
				defaults: gfx.EnvironmentDefaults{
					color_format: .rgba8
					depth_format: .depth_stencil
					sample_count: 1
				}
			}
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) begin_renderer_anchor() !RenderFrame {
		$if linux && x_multiwindow_x11 ? {
			outcome := backend.make_renderer_anchor_current(.anchor_prepare)
			if !backend.anchor_binding_proven(outcome) {
				return native_render_error(outcome)
			}
			return RenderFrame{
				swapchain: gfx.Swapchain{
					width:        1
					height:       1
					sample_count: 1
					color_format: .rgba8
					depth_format: .depth_stencil
					gl:           gfx.GlSwapchain{
						framebuffer: 0
					}
				}
			}
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) prepare_renderer_anchor_context() ! {
		outcome := backend.make_renderer_anchor_current(.anchor_prepare)
		if !backend.anchor_binding_proven(outcome) {
			return native_render_error(outcome)
		}
	}

	fn (mut backend X11Backend) end_renderer_anchor(frame RenderFrame) ! {
		_ = frame
		outcome := backend.make_renderer_anchor_current(.anchor_prepare)
		if !backend.anchor_binding_proven(outcome) {
			return native_render_error(outcome)
		}
	}

	fn (mut backend X11Backend) abort_renderer_anchor(frame RenderFrame) ! {
		backend.end_renderer_anchor(frame)!
	}

	fn (mut backend X11Backend) activate_render_frame(frame RenderFrame) BackendFrameAttempt {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(frame.window_id) or {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .make_current, .window_target,
						.operation_failed, 0, 0, err_window_not_found)
				}
			}
			record := backend.windows[index]
			if frame.acquired || frame.target.target_identity != record.render_target_generation
				|| frame.metrics.framebuffer_width != record.width
				|| frame.metrics.framebuffer_height != record.height {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .make_current, .window_target,
						.target_lost, 0, 0, err_render_target_stale)
				}
			}
			seed :=
				native_seed_for_frame(frame, .window_activate).with_target_identity(native_identity(record.egl_surface))
			outcome := backend.make_current(index, .window_target, seed)
			if !outcome.succeeded() {
				return BackendFrameAttempt{
					outcome: outcome
				}
			}
			return BackendFrameAttempt{
				frame:   RenderFrame{
					...frame
					acquired:  true
					swapchain: backend.swapchain_for_record(backend.windows[index])
				}
				outcome: outcome
			}
		} $else {
			_ = frame
			return BackendFrameAttempt{
				outcome: native_render_outcome(.none, .make_current, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend X11Backend) abort_render_frame(frame RenderFrame) ! {
		backend.window_record_index(frame.window_id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() {
			return
		}
		outcome := backend.make_renderer_anchor_current(.anchor_prepare)
		if !backend.anchor_binding_proven(outcome) {
			return native_render_error(outcome)
		}
	}

	fn (mut backend X11Backend) make_renderer_anchor_current(call_site NativeRenderCallSite) NativeRenderResult {
		$if linux && x_multiwindow_x11 ? {
			if backend.render_health.blocks_graphics() || backend.anchor_surface == unsafe { nil }
				|| backend.anchor_generation == 0 || backend.egl_display == unsafe { nil }
				|| backend.egl_context == unsafe { nil } {
				return backend.record_egl_result(native_render_outcome(.egl, .make_current,
					.anchor, .renderer_unavailable, 0, 0, err_render_native_renderer_unavailable))
			}
			desired := egl_anchor_binding(backend.anchor_generation, backend.anchor_surface)
			seed := NativeOperationSeed{
				presence_mask:     native_context_has_target_generation | native_context_has_target_identity
				call_site:         call_site
				scope:             .anchor
				target_generation: backend.anchor_generation
				target_identity:   native_identity(backend.anchor_surface)
			}
			return backend.bind_egl_identity(desired, seed)
		} $else {
			return native_render_outcome(.egl, .make_current, .renderer, .renderer_unavailable, 0,
				0, err_backend_unsupported)
		}
	}

	$if linux && x_multiwindow_x11 ? {
		fn (mut backend X11Backend) query_egl_binding(bind NativeRenderResult, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, expected EglBindingIdentity, verify_expected bool) NativeRenderResult {
			mut raw := C.VMultiwindowNativePrimitive{}
			query_seed := seed.without_target_identity()
			draw_context := ordinals.materialize(backend.native_operations, .egl,
				.current_draw_query, query_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(bind))
			}
			C.v_multiwindow_linux_egl_get_current_draw_surface(&raw)
			draw := backend.accept_egl_query(draw_context, raw, .none)
			if !draw.succeeded() {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(draw))
			}
			read_context := ordinals.materialize(backend.native_operations, .egl,
				.current_read_query, query_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(bind))
			}
			C.v_multiwindow_linux_egl_get_current_read_surface(&raw)
			read := backend.accept_egl_query(read_context, raw, .none)
			if !read.succeeded() {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(read))
			}
			context_context := ordinals.materialize(backend.native_operations, .egl,
				.current_context_query, query_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(bind))
			}
			C.v_multiwindow_linux_egl_get_current_context(&raw)
			context_result := if verify_expected {
				backend.accept_egl_binding_query(context_context, raw, draw, read, expected)
			} else {
				backend.accept_egl_query(context_context, raw, .none)
			}
			if !context_result.succeeded() {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(context_result))
			}
			return egl_verified_binding_result(bind, draw, read, context_result)
		}

		fn (mut backend X11Backend) bind_egl_identity(desired EglBindingIdentity, seed NativeOperationSeed) NativeRenderResult {
			mut ordinals := backend.native_operations.reserve_ordinals(5) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(native_render_outcome(.egl, .make_current,
					.renderer, .renderer_unavailable, 0, 0, err_render_native_renderer_unavailable))
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			bind_seed := seed.with_target_identity(native_identity(desired.surface))
			bind_context := ordinals.materialize(backend.native_operations, .egl, .make_current,
				bind_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(native_render_outcome(.egl, .make_current,
					.renderer, .renderer_unavailable, 0, 0, err_render_native_renderer_unavailable))
			}
			C.v_multiwindow_linux_egl_make_current(native_identity(backend.egl_display),
				native_identity(desired.surface), native_identity(desired.surface),
				native_identity(backend.egl_context), &raw)
			bind := backend.accept_egl_result(bind_context, mut ordinals, bind_seed, raw, .none)
			backend.egl_binding = EglBindingIdentity{}
			if !bind.succeeded() {
				if bind.native_code == i64(0x3007) {
					return backend.recover_bad_current_surface(desired, bind, mut ordinals, seed)
				}
				return bind
			}
			verified := backend.query_egl_binding(bind, mut ordinals, seed, desired, true)
			if !verified.succeeded() {
				return verified
			}
			actual := backend.egl_actual_binding(verified)
			if !egl_binding_is_exact(actual, desired) {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(verified))
			}
			backend.egl_binding = actual
			return verified
		}
	}

	fn (backend &X11Backend) anchor_binding_proven(outcome NativeRenderResult) bool {
		expected := egl_anchor_binding(backend.anchor_generation, backend.anchor_surface)
		recovered := !outcome.succeeded() && outcome.disposition == .target_lost
			&& outcome.native_code == i64(0x3007) && backend.egl_bad_current_recovery_used
		if backend.anchor_generation == 0 || (!outcome.succeeded() && !recovered) {
			return false
		}
		actual := backend.egl_actual_binding(outcome)
		return egl_binding_is_exact(actual, expected)
			&& egl_binding_is_exact(backend.egl_binding, expected)
	}

	fn (mut backend X11Backend) prove_renderer_shutdown_anchor() RendererShutdownProof {
		outcome := backend.make_renderer_anchor_current(.shutdown_anchor)
		return RendererShutdownProof{
			path:    if backend.anchor_binding_proven(outcome) {
				RendererShutdownPath.orderly_anchor
			} else {
				RendererShutdownPath.logical_abandon
			}
			outcome: outcome
		}
	}

	fn (backend &X11Backend) egl_binding_is_live(binding EglBindingIdentity) bool {
		return match binding.kind {
			.anchor {
				backend.anchor_generation != 0
					&& egl_binding_is_exact(binding, egl_anchor_binding(backend.anchor_generation, backend.anchor_surface))
			}
			.window {
				index := backend.window_record_index(binding.window) or { return false }
				record := backend.windows[index]

				record.render_target_generation != 0
					&& egl_binding_is_exact(binding, egl_window_binding(record.id, record.render_target_generation, record.egl_surface))
			}
			else {
				false
			}
		}
	}

	fn (backend &X11Backend) egl_actual_binding(result NativeRenderResult) EglBindingIdentity {
		if !egl_result_has_current_identity(result, backend.egl_context) {
			return EglBindingIdentity{}
		}
		surface := result.current_draw_surface
		if backend.anchor_generation != 0 && surface == backend.anchor_surface {
			return egl_anchor_binding(backend.anchor_generation, surface)
		}
		for record in backend.windows {
			if record.render_target_generation != 0 && surface == record.egl_surface {
				return egl_window_binding(record.id, record.render_target_generation, surface)
			}
		}
		return EglBindingIdentity{}
	}

	fn (mut backend X11Backend) recover_bad_current_surface(desired EglBindingIdentity, failure NativeRenderResult, mut query_ordinals NativeOrdinalRange, boundary_seed NativeOperationSeed) NativeRenderResult {
		$if linux && x_multiwindow_x11 ? {
			if backend.egl_bad_current_recovery_used || desired.kind == .none
				|| desired.target_generation == 0 || backend.anchor_generation == 0 {
				backend.egl_bad_current_recovery_used = true
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			backend.egl_bad_current_recovery_used = true
			observed := backend.query_egl_binding(failure, mut query_ordinals, boundary_seed, EglBindingIdentity{},
				false)
			if observed.blocks_graphics() {
				return observed
			}
			actual := backend.egl_actual_binding(observed)
			if !backend.egl_binding_is_live(actual) {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			backend.invalidate_egl_binding(actual)
			if backend.render_health.blocks_graphics() || backend.egl_display == unsafe { nil }
				|| backend.egl_context == unsafe { nil } || backend.egl_display_ticket == 0
				|| backend.egl_context_ticket == 0 || backend.anchor_generation == 0 {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			need_anchor := backend.anchor_surface == unsafe { nil }
			seed := NativeOperationSeed{
				presence_mask:      native_context_has_target_generation | native_context_has_batch_epoch | native_context_has_window_lease_epoch | native_context_has_target_lease_epoch
				call_site:          .anchor_prepare
				scope:              .anchor
				target_generation:  backend.anchor_generation
				batch_epoch:        failure.context.batch_epoch
				window_lease_epoch: failure.context.window_lease_epoch
				target_lease_epoch: failure.context.target_lease_epoch
			}
			mut ordinals := backend.native_operations.reserve_ordinals(if need_anchor {
				u64(8)
			} else {
				u64(5)
			}) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			if need_anchor {
				mut cleanup_ordinals := ordinals.split_tail(1) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
				}
				cleanup_ticket := backend.native_operations.reserve_linux_egl_lifetime_ticket(mut cleanup_ordinals,
					.egl_surface, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
				}
				create_seed := seed.with_target_identity(native_identity(backend.egl_config))
				create_context := ordinals.materialize(backend.native_operations, .egl,
					.anchor_surface_create, create_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
				}
				C.v_multiwindow_linux_egl_create_pbuffer_surface(native_identity(backend.egl_display),
					native_identity(backend.egl_config), 1, 1, &raw)
				actual_surface := native_pointer(raw.handle)
				if actual_surface != unsafe { nil } {
					backend.anchor_surface = actual_surface
					backend.anchor_surface_ticket = cleanup_ticket
					backend.native_operations.bind_lifetime_ticket(cleanup_ticket,
						native_identity(actual_surface), native_identity(backend.egl_display))
				}
				created := backend.accept_egl_result(create_context, mut ordinals, create_seed,
					raw, .none)
				if actual_surface == unsafe { nil } {
					backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				}
				if !created.succeeded() {
					release := backend.release_egl_surface_ticket(cleanup_ticket, actual_surface)
					if release.terminal {
						backend.anchor_surface = unsafe { nil }
						backend.anchor_surface_ticket = 0
					}
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(created))
				}
			}
			expected := egl_anchor_binding(backend.anchor_generation, backend.anchor_surface)
			bind_seed := seed.with_target_identity(native_identity(backend.anchor_surface))
			bind_context := ordinals.materialize(backend.native_operations, .egl, .make_current,
				bind_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			C.v_multiwindow_linux_egl_make_current(native_identity(backend.egl_display),
				native_identity(backend.anchor_surface), native_identity(backend.anchor_surface),
				native_identity(backend.egl_context), &raw)
			rebound := backend.accept_egl_result(bind_context, mut ordinals, bind_seed, raw, .none)
			if !rebound.succeeded() {
				backend.egl_binding = EglBindingIdentity{}
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(rebound))
			}
			verified := backend.query_egl_binding(rebound, mut ordinals, seed, expected, true)
			if !verified.succeeded() {
				backend.egl_binding = EglBindingIdentity{}
				return verified
			}
			backend.egl_binding = backend.egl_actual_binding(verified)
			if desired.kind == .window {
				index := backend.window_record_index(desired.window) or {
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
				}
				if desired.surface != unsafe { nil } {
					backend.invalidate_egl_binding(desired)
				} else if backend.windows[index].render_target_generation == desired.target_generation {
					backend.windows[index].render_target_generation =
						exhaust_backend_target_generation(backend.windows[index].render_target_generation)
				}
				if backend.windows[index].render_target_generation == 0 {
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
				}
			}
			recovery := NativeRenderResult{
				...verified
				scope:       egl_binding_scope(desired)
				disposition: .target_lost
				native_code: failure.native_code
				error_text:  err_render_target_lost
			}
			return backend.record_egl_result(recovery)
		} $else {
			_ = desired
			_ = query_ordinals
			_ = boundary_seed
			return egl_unavailable_after_failed_recovery(failure)
		}
	}

	$if linux && x_multiwindow_x11 ? {
		fn (mut backend X11Backend) invalidate_egl_binding(binding EglBindingIdentity) {
			if binding.kind == .anchor {
				if binding.surface == backend.anchor_surface
					&& binding.target_generation == backend.anchor_generation {
					backend.invalidate_egl_anchor()
				}
			} else if binding.kind == .window {
				index := backend.window_record_index(binding.window) or { return }
				mut record := &backend.windows[index]
				if record.egl_surface == binding.surface
					&& record.render_target_generation == binding.target_generation {
					release := backend.release_egl_surface_ticket(record.egl_surface_ticket,
						record.egl_surface)
					if release.terminal {
						record.egl_surface = unsafe { nil }
						record.egl_surface_ticket = 0
						record.render_target_generation =
							exhaust_backend_target_generation(record.render_target_generation)
					}
				}
			}
			if egl_binding_is_exact(backend.egl_binding, binding) {
				backend.egl_binding = EglBindingIdentity{}
			}
		}
	}

	fn (mut backend X11Backend) invalidate_egl_anchor() {
		$if linux && x_multiwindow_x11 ? {
			if backend.anchor_surface != unsafe { nil } {
				old_surface := backend.anchor_surface
				release := backend.release_egl_surface_ticket(backend.anchor_surface_ticket,
					old_surface)
				if release.terminal {
					backend.anchor_surface = unsafe { nil }
					backend.anchor_surface_ticket = 0
					backend.anchor_generation =
						exhaust_backend_target_generation(backend.anchor_generation)
					if backend.egl_binding.kind == .anchor
						&& backend.egl_binding.surface == old_surface {
						backend.egl_binding = EglBindingIdentity{}
					}
				}
			}
		}
	}
}

fn (mut backend X11Backend) collect_render_updates() ![]BackendRenderUpdate {
	mut updates := []BackendRenderUpdate{}
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if linux && x_multiwindow_x11 ? {
			for i in 0 .. backend.windows.len {
				mut record := &backend.windows[i]
				if record.native_destroyed {
					continue
				}
				mut width := 0
				mut height := 0
				mut viewable := 0
				available := C.v_multiwindow_x11_render_snapshot(backend.display, record.window,
					&width, &height, &viewable) != 0
				backend.render_sequence = next_backend_render_sequence(backend.render_sequence)!
				if available && (record.width != width || record.height != height) {
					record.width = width
					record.height = height
					record.render_target_generation =
						exhaust_backend_target_generation(record.render_target_generation)
				}
				ready := backend.render_health == .ready && record.render_target_generation != 0
					&& available && viewable != 0 && width > 0 && height > 0
				block := if backend.render_health.blocks_graphics()
					|| record.render_target_generation == 0 {
					RenderBlockReason.renderer_failed
				} else if !available {
					RenderBlockReason.backend_unavailable
				} else if viewable == 0 {
					RenderBlockReason.not_viewable
				} else if width <= 0 || height <= 0 {
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
						framebuffer_width:    width
						framebuffer_height:   height
						dpi_scale:            1
						metrics_sequence:     backend.render_sequence
						metrics_available:    available && width > 0 && height > 0
						conversion_available: available && width > 0 && height > 0
					}
					target:       RenderTargetSnapshot{
						target_identity: record.render_target_generation
						color_format:    int(gfx.PixelFormat.rgba8)
						depth_format:    int(gfx.PixelFormat.depth_stencil)
						sample_count:    1
					}
				}
			}
		}
	}
	return updates
}
