module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if linux && sokol_wayland ? {
		fn (backend &WaylandBackend) renderer_anchor_lifetime_absent() bool {
			return backend.anchor_surface == unsafe { nil }
				&& backend.anchor_wl_egl_window == unsafe { nil }
				&& backend.anchor_wl_surface == unsafe { nil } && backend.anchor_surface_ticket == 0
				&& backend.anchor_wl_egl_window_ticket == 0 && backend.anchor_wl_surface_ticket == 0
		}

		fn (backend &WaylandBackend) renderer_anchor_lifetime_complete() bool {
			return backend.anchor_surface != unsafe { nil }
				&& backend.anchor_wl_egl_window != unsafe { nil }
				&& backend.anchor_wl_surface != unsafe { nil } && backend.anchor_surface_ticket != 0
				&& backend.anchor_wl_egl_window_ticket != 0 && backend.anchor_wl_surface_ticket != 0
		}

		fn (mut backend WaylandBackend) create_private_renderer_anchor(mut ordinals NativeOrdinalRange, mut cleanup_ordinals NativeOrdinalRange, seed NativeOperationSeed) NativeRenderResult {
			if !backend.transport_can_marshal() || backend.compositor == unsafe { nil }
				|| backend.display == unsafe { nil } {
				return backend.record_wayland_result(native_wayland_logical_result(.anchor_surface_create,
					.anchor, .renderer_unavailable, backend.wayland_display_error,
					err_wayland_create_surface_failed))
			}
			if !backend.renderer_anchor_lifetime_absent() {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			egl_surface_ticket := backend.native_operations.reserve_linux_egl_lifetime_ticket(mut cleanup_ordinals,
				.egl_surface, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			backend.anchor_surface_ticket = egl_surface_ticket
			wl_egl_window_ticket := backend.reserve_wayland_lifetime_ticket(mut cleanup_ordinals,
				.wayland_egl_window, seed) or {
				backend.native_operations.burn_lifetime_ticket(egl_surface_ticket)
				backend.anchor_surface_ticket = 0
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			backend.anchor_wl_egl_window_ticket = wl_egl_window_ticket
			wl_surface_ticket := backend.reserve_wayland_lifetime_ticket(mut cleanup_ordinals,
				.wayland_surface, seed) or {
				backend.native_operations.burn_lifetime_ticket(wl_egl_window_ticket)
				backend.native_operations.burn_lifetime_ticket(egl_surface_ticket)
				backend.anchor_wl_egl_window_ticket = 0
				backend.anchor_surface_ticket = 0
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			backend.anchor_wl_surface_ticket = wl_surface_ticket

			display := unsafe { &C.wl_display(backend.display) }
			mut raw := C.VMultiwindowNativePrimitive{}
			surface_seed := seed.with_target_identity(native_identity(backend.compositor))
			surface_context := ordinals.materialize(backend.native_operations, .wayland,
				.anchor_surface_create, surface_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				_ = backend.release_renderer_anchor_lifetime()
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			C.v_multiwindow_wayland_create_anchor_surface(unsafe {
				&C.wl_compositor(backend.compositor)
			}, &raw)
			actual_wl_surface := native_pointer(raw.handle)
			if actual_wl_surface != unsafe { nil } {
				backend.anchor_wl_surface = actual_wl_surface
				backend.native_operations.bind_lifetime_ticket(wl_surface_ticket,
					native_identity(actual_wl_surface), native_identity(backend.compositor))
			}
			surface_validation := if actual_wl_surface == unsafe { nil } {
				NativeLocalValidation.null_output
			} else {
				NativeLocalValidation.none
			}
			surface_result := backend.accept_wayland_result(surface_context, mut ordinals, display,
				raw, surface_validation, err_wayland_create_surface_failed)
			if actual_wl_surface == unsafe { nil } {
				backend.native_operations.burn_lifetime_ticket(wl_surface_ticket)
				backend.anchor_wl_surface_ticket = 0
			}
			if !surface_result.succeeded() {
				_ = backend.release_renderer_anchor_lifetime()
				return surface_result
			}

			window_seed := seed.with_target_identity(native_identity(actual_wl_surface))
			window_context := ordinals.materialize(backend.native_operations, .wayland,
				.anchor_surface_create, window_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				_ = backend.release_renderer_anchor_lifetime()
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			C.v_multiwindow_wayland_egl_create_window(unsafe { &C.wl_surface(actual_wl_surface) },
				1, 1, &raw)
			actual_wl_egl_window := native_pointer(raw.handle)
			if actual_wl_egl_window != unsafe { nil } {
				backend.anchor_wl_egl_window = actual_wl_egl_window
				backend.native_operations.bind_lifetime_ticket(wl_egl_window_ticket,
					native_identity(actual_wl_egl_window), native_identity(actual_wl_surface))
			}
			window_validation := if actual_wl_egl_window == unsafe { nil } {
				NativeLocalValidation.null_output
			} else {
				NativeLocalValidation.none
			}
			window_result := backend.accept_wayland_result(window_context, mut ordinals, display,
				raw, window_validation, err_wayland_egl_surface_failed)
			if actual_wl_egl_window == unsafe { nil } {
				backend.native_operations.burn_lifetime_ticket(wl_egl_window_ticket)
				backend.anchor_wl_egl_window_ticket = 0
			}
			if !window_result.succeeded() {
				_ = backend.release_renderer_anchor_lifetime()
				return window_result
			}

			egl_seed := seed.with_target_identity(native_identity(actual_wl_egl_window))
			egl_context := ordinals.materialize(backend.native_operations, .egl,
				.anchor_surface_create, egl_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				_ = backend.release_renderer_anchor_lifetime()
				return backend.blocked_renderer_result(.anchor_surface_create)
			}
			C.v_multiwindow_linux_egl_create_window_surface(native_identity(backend.egl_display),
				native_identity(backend.egl_config), native_identity(actual_wl_egl_window), &raw)
			actual_egl_surface := native_pointer(raw.handle)
			if actual_egl_surface != unsafe { nil } {
				backend.anchor_surface = actual_egl_surface
				backend.native_operations.bind_lifetime_ticket(egl_surface_ticket,
					native_identity(actual_egl_surface), native_identity(backend.egl_display))
			}
			egl_validation := if actual_egl_surface == unsafe { nil } {
				NativeLocalValidation.null_output
			} else {
				NativeLocalValidation.none
			}
			egl_result := backend.accept_egl_result(egl_context, mut ordinals, egl_seed, raw,
				egl_validation)
			if actual_egl_surface == unsafe { nil } {
				backend.native_operations.burn_lifetime_ticket(egl_surface_ticket)
				backend.anchor_surface_ticket = 0
			}
			if !egl_result.succeeded() {
				_ = backend.release_renderer_anchor_lifetime()
				return egl_result
			}
			return egl_result
		}
	}

	fn (mut backend WaylandBackend) create_renderer_anchor() ! {
		$if linux && sokol_wayland ? {
			if backend.renderer_anchor_lifetime_complete() {
				return
			}
			if !backend.renderer_anchor_lifetime_absent() {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				_ = backend.release_renderer_anchor_lifetime()
				return error(err_render_native_renderer_unavailable)
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
			mut ordinals := backend.native_operations.reserve_ordinals(9) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			mut cleanup_ordinals := ordinals.split_tail(3) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_render_native_renderer_unavailable)
			}
			outcome :=
				backend.create_private_renderer_anchor(mut ordinals, mut cleanup_ordinals, seed)
			if !outcome.succeeded() {
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

	fn (mut backend WaylandBackend) destroy_renderer_anchor() ! {
		$if linux && sokol_wayland ? {
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

	fn (mut backend WaylandBackend) renderer_environment() !gfx.Environment {
		$if linux && sokol_wayland ? {
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

	fn (mut backend WaylandBackend) begin_renderer_anchor() !RenderFrame {
		$if linux && sokol_wayland ? {
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

	fn (mut backend WaylandBackend) prepare_renderer_anchor_context() ! {
		outcome := backend.make_renderer_anchor_current(.anchor_prepare)
		if !backend.anchor_binding_proven(outcome) {
			return native_render_error(outcome)
		}
	}

	fn (mut backend WaylandBackend) end_renderer_anchor(frame RenderFrame) ! {
		_ = frame
		outcome := backend.make_renderer_anchor_current(.anchor_prepare)
		if !backend.anchor_binding_proven(outcome) {
			return native_render_error(outcome)
		}
	}

	fn (mut backend WaylandBackend) abort_renderer_anchor(frame RenderFrame) ! {
		backend.end_renderer_anchor(frame)!
	}

	fn (mut backend WaylandBackend) activate_render_frame(frame RenderFrame) BackendFrameAttempt {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(frame.window_id) or {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .make_current, .window_target,
						.operation_failed, 0, 0, err_window_not_found)
				}
			}
			mut record := backend.windows[index]
			if frame.acquired || frame.target.target_identity != record.render_target_generation
				|| frame.metrics.framebuffer_width != record.width
				|| frame.metrics.framebuffer_height != record.height || !record.frame_ready {
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
			record.frame_ready = false
			return BackendFrameAttempt{
				frame:   RenderFrame{
					...frame
					acquired:  true
					swapchain: backend.swapchain_for_record(record)
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

	fn (mut backend WaylandBackend) abort_render_frame(frame RenderFrame) ! {
		index := backend.window_record_index(frame.window_id) or {
			return error(err_window_not_found)
		}
		backend.windows[index].frame_ready = true
		if backend.render_health.blocks_graphics() {
			return
		}
		outcome := backend.make_renderer_anchor_current(.anchor_prepare)
		if !backend.anchor_binding_proven(outcome) {
			return native_render_error(outcome)
		}
	}

	fn (mut backend WaylandBackend) make_renderer_anchor_current(call_site NativeRenderCallSite) NativeRenderResult {
		$if linux && sokol_wayland ? {
			if backend.render_health.blocks_graphics() {
				return backend.blocked_renderer_result(.make_current)
			}
			if backend.anchor_surface == unsafe { nil } || backend.anchor_generation == 0
				|| backend.egl_display == unsafe { nil } || backend.egl_context == unsafe { nil } {
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

	$if linux && sokol_wayland ? {
		fn (mut backend WaylandBackend) query_egl_binding(bind NativeRenderResult, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, expected EglBindingIdentity, verify_expected bool) NativeRenderResult {
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

		fn (mut backend WaylandBackend) bind_egl_identity(desired EglBindingIdentity, seed NativeOperationSeed) NativeRenderResult {
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

	fn (backend &WaylandBackend) anchor_binding_proven(outcome NativeRenderResult) bool {
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

	fn (mut backend WaylandBackend) prove_renderer_shutdown_anchor() RendererShutdownProof {
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

	fn (backend &WaylandBackend) egl_binding_is_live(binding EglBindingIdentity) bool {
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

	fn (backend &WaylandBackend) egl_actual_binding(result NativeRenderResult) EglBindingIdentity {
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

	fn (mut backend WaylandBackend) recover_bad_current_surface(desired EglBindingIdentity, failure NativeRenderResult, mut query_ordinals NativeOrdinalRange, boundary_seed NativeOperationSeed) NativeRenderResult {
		$if linux && sokol_wayland ? {
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
			if !backend.renderer_anchor_lifetime_absent()
				&& !backend.renderer_anchor_lifetime_complete() {
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			need_anchor := backend.renderer_anchor_lifetime_absent()
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
				u64(14)
			} else {
				u64(5)
			}) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			if need_anchor {
				mut cleanup_ordinals := ordinals.split_tail(3) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.record_egl_result(egl_unavailable_after_failed_recovery(failure))
				}
				created := backend.create_private_renderer_anchor(mut ordinals, mut
					cleanup_ordinals, seed)
				if !created.succeeded() {
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
				backend.invalidate_window_egl_target(index, desired.surface,
					desired.target_generation, false)
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

	$if linux && sokol_wayland ? {
		fn (mut backend WaylandBackend) invalidate_egl_binding(binding EglBindingIdentity) {
			if binding.kind == .anchor {
				if binding.surface == backend.anchor_surface
					&& binding.target_generation == backend.anchor_generation {
					backend.invalidate_egl_anchor()
				}
			} else if binding.kind == .window {
				index := backend.window_record_index(binding.window) or { return }
				backend.invalidate_window_egl_target(index, binding.surface,
					binding.target_generation, false)
			}
			if egl_binding_is_exact(backend.egl_binding, binding) {
				backend.egl_binding = EglBindingIdentity{}
			}
		}
	}

	fn (mut backend WaylandBackend) invalidate_egl_anchor() {
		$if linux && sokol_wayland ? {
			if backend.anchor_surface != unsafe { nil }
				|| backend.anchor_wl_egl_window != unsafe { nil }
				|| backend.anchor_wl_surface != unsafe { nil } || backend.anchor_surface_ticket != 0
				|| backend.anchor_wl_egl_window_ticket != 0 || backend.anchor_wl_surface_ticket != 0 {
				old_surface := backend.anchor_surface
				if backend.release_renderer_anchor_lifetime() {
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

fn (mut backend WaylandBackend) collect_render_updates() ![]BackendRenderUpdate {
	mut updates := []BackendRenderUpdate{}
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if linux && sokol_wayland ? {
			for record in backend.windows {
				if record.native_destroyed {
					continue
				}
				backend.render_sequence = next_backend_render_sequence(backend.render_sequence)!
				ready := backend.render_health == .ready && record.render_target_generation != 0
					&& record.requested_visible && record.configured && record.frame_ready
					&& record.width > 0 && record.height > 0
				block := if backend.render_health.blocks_graphics()
					|| record.render_target_generation == 0 {
					RenderBlockReason.renderer_failed
				} else if !record.requested_visible || !record.configured {
					RenderBlockReason.not_configured
				} else if record.width <= 0 || record.height <= 0 {
					RenderBlockReason.zero_sized
				} else if !record.frame_ready {
					RenderBlockReason.frame_callback_pending
				} else {
					RenderBlockReason.none
				}
				updates << BackendRenderUpdate{
					window:       record.id
					sequence:     backend.render_sequence
					ready_credit: ready
					block_reason: block
					metrics:      RenderMetricsSnapshot{
						...record.service_metrics_snapshot()
						metrics_sequence: backend.render_sequence
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
