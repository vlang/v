module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	// RenderFrame never crosses the x.multiwindow module boundary.
	struct RenderFrame {
		window_id          WindowId
		backend_lease      u64
		batch_epoch        u64
		window_lease_epoch u64
		target_lease_epoch u64
		metrics            RenderMetricsSnapshot
		target             RenderTargetSnapshot
		acquired           bool
		swapchain          gfx.Swapchain
	}

	struct NativeTargetAttempt {
		batch_epoch        u64
		window_lease_epoch u64
		target_lease_epoch u64
	}

	enum RenderFinalizeStatus {
		submitted
		not_presented
	}

	struct BackendFrameAttempt {
		frame   RenderFrame
		outcome NativeRenderResult
	}

	struct BackendFinalizeAttempt {
		status  RenderFinalizeStatus
		outcome NativeRenderResult
	}

	fn (backend &Backend) preflight_renderer_start() ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11, .wayland, .appkit, .win32 {}
		}
	}

	fn (mut backend Backend) begin_render_batch(seed NativeOperationSeed) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.appkit { backend.appkit.begin_render_batch(seed)! }
			else {}
		}
	}

	fn (mut backend Backend) end_render_batch(seed NativeOperationSeed) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.appkit { backend.appkit.end_render_batch(seed)! }
			else {}
		}
	}

	fn (mut backend Backend) renderer_environment() !gfx.Environment {
		return match backend.kind {
			.auto { error(err_backend_unsupported) }
			.mock { error(err_renderer_unsupported) }
			.x11 { backend.x11.renderer_environment()! }
			.wayland { backend.wayland.renderer_environment()! }
			.appkit { backend.appkit.renderer_environment()! }
			.win32 { backend.win32.renderer_environment()! }
		}
	}

	fn (mut backend Backend) ensure_renderer_started() ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.init_renderer()! }
			.wayland { backend.wayland.init_renderer()! }
			.appkit { backend.appkit.init_renderer()! }
			.win32 { backend.win32.init_renderer()! }
		}
	}

	fn (mut backend Backend) create_renderer_anchor() ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.create_renderer_anchor()! }
			.wayland { backend.wayland.create_renderer_anchor()! }
			.appkit { backend.appkit.create_renderer_anchor()! }
			.win32 { backend.win32.create_renderer_anchor()! }
		}
	}

	fn (mut backend Backend) destroy_renderer_anchor() ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock {}
			.x11 { backend.x11.destroy_renderer_anchor()! }
			.wayland { backend.wayland.destroy_renderer_anchor()! }
			.appkit { backend.appkit.destroy_renderer_anchor()! }
			.win32 { backend.win32.destroy_renderer_anchor()! }
		}
	}

	fn (mut backend Backend) begin_render(id WindowId, candidate RenderWindowSnapshot, native_attempt NativeTargetAttempt) BackendFrameAttempt {
		return match backend.kind {
			.auto, .mock {
				BackendFrameAttempt{
					outcome: native_render_outcome(.none, .window_surface_create, .window_target,
						.renderer_unavailable, 0, 0, err_renderer_unsupported)
				}
			}
			.x11 {
				backend.x11.begin_render(id, candidate, native_attempt)
			}
			.wayland {
				backend.wayland.begin_render(id, candidate, native_attempt)
			}
			.appkit {
				backend.appkit.begin_render(id, candidate, native_attempt)
			}
			.win32 {
				backend.win32.begin_render(id, candidate, native_attempt)
			}
		}
	}

	fn (mut backend Backend) end_render(frame RenderFrame) BackendFinalizeAttempt {
		return match backend.kind {
			.auto, .mock {
				BackendFinalizeAttempt{
					status:  .not_presented
					outcome: native_render_outcome(.none, .present, .window_target,
						.renderer_unavailable, 0, 0, err_renderer_unsupported)
				}
			}
			.x11 {
				backend.x11.end_render(frame)
			}
			.wayland {
				backend.wayland.end_render(frame)
			}
			.appkit {
				backend.appkit.end_render(frame)
			}
			.win32 {
				backend.win32.end_render(frame)
			}
		}
	}

	fn (mut backend Backend) abort_render(frame RenderFrame) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.abort_render_frame(frame)! }
			.wayland { backend.wayland.abort_render_frame(frame)! }
			.appkit { backend.appkit.abort_render(frame)! }
			.win32 { backend.win32.abort_render_frame(frame)! }
		}
	}

	fn (mut backend Backend) activate_render_frame(frame RenderFrame) BackendFrameAttempt {
		return match backend.kind {
			.auto, .mock {
				BackendFrameAttempt{
					outcome: native_render_outcome(.none, .window_surface_create, .window_target,
						.renderer_unavailable, 0, 0, err_renderer_unsupported)
				}
			}
			.x11 {
				backend.x11.activate_render_frame(frame)
			}
			.wayland {
				backend.wayland.activate_render_frame(frame)
			}
			.appkit {
				backend.appkit.activate_render_frame(frame)
			}
			.win32 {
				backend.win32.activate_render_frame(frame)
			}
		}
	}

	fn (mut backend Backend) begin_renderer_anchor() !RenderFrame {
		return match backend.kind {
			.auto { error(err_backend_unsupported) }
			.mock { error(err_renderer_unsupported) }
			.x11 { backend.x11.begin_renderer_anchor()! }
			.wayland { backend.wayland.begin_renderer_anchor()! }
			.appkit { backend.appkit.begin_renderer_anchor()! }
			.win32 { backend.win32.begin_renderer_anchor()! }
		}
	}

	fn (mut backend Backend) prepare_renderer_anchor_context() ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.prepare_renderer_anchor_context()! }
			.wayland { backend.wayland.prepare_renderer_anchor_context()! }
			.appkit { backend.appkit.prepare_renderer_anchor_context()! }
			.win32 { backend.win32.prepare_renderer_anchor_context()! }
		}
	}

	fn (mut backend Backend) end_renderer_anchor(frame RenderFrame) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.end_renderer_anchor(frame)! }
			.wayland { backend.wayland.end_renderer_anchor(frame)! }
			.appkit { backend.appkit.end_renderer_anchor(frame)! }
			.win32 { backend.win32.end_renderer_anchor(frame)! }
		}
	}

	fn (mut backend Backend) abort_renderer_anchor(frame RenderFrame) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.abort_renderer_anchor(frame)! }
			.wayland { backend.wayland.abort_renderer_anchor(frame)! }
			.appkit { backend.appkit.abort_renderer_anchor(frame)! }
			.win32 { backend.win32.abort_renderer_anchor(frame)! }
		}
	}

	fn (backend &Backend) renderer_health() NativeRendererHealth {
		return match backend.kind {
			.auto, .mock { NativeRendererHealth.unavailable }
			.x11 { backend.x11.render_health }
			.wayland { backend.wayland.render_health }
			.appkit { backend.appkit.render_health }
			.win32 { backend.win32.render_health }
		}
	}

	fn (mut backend Backend) abandon_renderer_ownership() {
		match backend.kind {
			.x11 { backend.x11.abandon_renderer_ownership() }
			.wayland { backend.wayland.abandon_renderer_ownership() }
			.appkit { backend.appkit.abandon_renderer_ownership() }
			.win32 { backend.win32.abandon_renderer_ownership() }
			else {}
		}
	}

	fn (mut backend Backend) promote_native_render_window_loss(id WindowId, outcome NativeRenderResult) {
		if outcome.disposition != .native_window_lost {
			return
		}
		match backend.kind {
			.x11 { backend.x11.accept_native_render_window_loss(id) }
			.wayland { backend.wayland.accept_native_render_window_loss(id) }
			.appkit { backend.appkit.accept_native_render_window_loss(id) }
			.win32 { backend.win32.accept_native_render_window_loss(id) }
			else { return }
		}

		for notice in backend.teardown_notices {
			if notice.window == id {
				return
			}
		}
		backend.teardown_notices << BackendTeardownNotice{
			window: id
		}
	}

	fn (mut backend Backend) prove_renderer_shutdown_anchor() RendererShutdownProof {
		return match backend.kind {
			.auto, .mock {
				RendererShutdownProof{
					path:    .logical_abandon
					outcome: native_render_outcome(.none, .device_status, .renderer,
						.renderer_unavailable, 0, 0, err_renderer_unsupported)
				}
			}
			.x11 {
				backend.x11.prove_renderer_shutdown_anchor()
			}
			.wayland {
				backend.wayland.prove_renderer_shutdown_anchor()
			}
			.appkit {
				backend.appkit.prove_renderer_shutdown_anchor()
			}
			.win32 {
				backend.win32.prove_renderer_shutdown_anchor()
			}
		}
	}
}

fn (mut backend Backend) render_updates() ![]BackendRenderUpdate {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		return match backend.kind {
			.auto { error(err_backend_unsupported) }
			.mock { []BackendRenderUpdate{} }
			.x11 { backend.x11.collect_render_updates()! }
			.wayland { backend.wayland.collect_render_updates()! }
			.appkit { backend.appkit.collect_render_updates()! }
			.win32 { backend.win32.collect_render_updates()! }
		}
	} $else {
		_ = backend
		return []BackendRenderUpdate{}
	}
}
