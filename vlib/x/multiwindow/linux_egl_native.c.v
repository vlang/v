module multiwindow

$if linux {
	$if x_multiwindow_x11 ? || sokol_wayland ? {
		#insert "@VMODROOT/vlib/x/multiwindow/linux_egl_native_helpers.h"
	}
}

struct LinuxEglRendererLifetimeTickets {
	display u64
	context u64
	thread  u64
}

fn C.v_multiwindow_linux_egl_get_display(native_display u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_get_error(result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_initialize(display u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_query_extensions(display u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_bind_opengl_api(result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_choose_config(display u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_choose_wayland_config(display u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_get_native_visual(display u64, config u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_create_context(display u64, config u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_create_window_surface(display u64, config u64, native_window u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_create_pbuffer_surface(display u64, config u64, width i64, height i64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_make_current(display u64, draw u64, read u64, context u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_get_current_draw_surface(result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_get_current_read_surface(result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_get_current_context(result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_swap_buffers(display u64, surface u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_destroy_surface(display u64, surface u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_destroy_context(display u64, context u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_terminate(display u64, result &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_linux_egl_release_thread(result &C.VMultiwindowNativePrimitive)

fn (mut authority NativeOperationAuthority) capture_egl_call(context NativeOperationContext, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, raw C.VMultiwindowNativePrimitive) !NativePrimitiveCapture {
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			primary := authority.capture_call(context, raw)
			if !egl_capture_requires_error(context, primary) {
				ordinals.skip(1)!
				return primary
			}
			evidence_context := ordinals.materialize(authority, .egl, .egl_error_query,
				seed.without_target_identity())!
			mut evidence_raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_linux_egl_get_error(&evidence_raw)
			evidence := authority.capture_evidence(evidence_context, evidence_raw)
			return native_capture_with_egl_error(primary, evidence)
		} $else {
			return error(err_render_native_renderer_unavailable)
		}
	} $else {
		return error(err_render_native_renderer_unavailable)
	}
}

fn (mut authority NativeOperationAuthority) reserve_linux_egl_lifetime_ticket(mut cleanup NativeOrdinalRange, release_kind NativeLifetimeReleaseKind, seed NativeOperationSeed) !u64 {
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			if release_kind !in [.egl_surface, .egl_context, .egl_display, .egl_thread] {
				return error(err_render_native_renderer_unavailable)
			}
			context := cleanup.materialize(authority, .egl,
				native_lifetime_release_operation(release_kind), seed.without_target_identity())!
			return authority.reserve_lifetime_ticket(context, release_kind,
				seed.without_target_identity())
		} $else {
			return error(err_render_native_renderer_unavailable)
		}
	} $else {
		return error(err_render_native_renderer_unavailable)
	}
}

fn (mut authority NativeOperationAuthority) reserve_linux_egl_renderer_lifetime_tickets(mut cleanup NativeOrdinalRange, seed NativeOperationSeed) !LinuxEglRendererLifetimeTickets {
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			display_ticket := authority.reserve_linux_egl_lifetime_ticket(mut cleanup,
				.egl_display, seed)!
			context_ticket := authority.reserve_linux_egl_lifetime_ticket(mut cleanup,
				.egl_context, seed) or {
				authority.burn_lifetime_ticket(display_ticket)
				return err
			}
			thread_ticket := authority.reserve_linux_egl_lifetime_ticket(mut cleanup, .egl_thread, seed) or {
				authority.burn_lifetime_ticket(context_ticket)
				authority.burn_lifetime_ticket(display_ticket)
				return err
			}
			return LinuxEglRendererLifetimeTickets{
				display: display_ticket
				context: context_ticket
				thread:  thread_ticket
			}
		} $else {
			return error(err_render_native_renderer_unavailable)
		}
	} $else {
		return error(err_render_native_renderer_unavailable)
	}
}

fn (mut authority NativeOperationAuthority) bind_linux_egl_thread_lifetime_ticket(ticket_id u64) {
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			authority.bind_lifetime_ticket(ticket_id, authority.owner_thread_identity, 0)
		} $else {
			return
		}
	} $else {
		return
	}
}

fn (mut authority NativeOperationAuthority) release_linux_egl_thread_lifetime_ticket(ticket_id u64, health NativeRendererHealth) NativeLifetimeReleaseAttempt {
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			if ticket_id == 0 || !authority.owner_thread_is_current() {
				return NativeLifetimeReleaseAttempt{}
			}
			if authority.burn_lifetime_ticket(ticket_id) {
				return NativeLifetimeReleaseAttempt{
					terminal: true
				}
			}
			return authority.release_linux_egl_lifetime_ticket(ticket_id, .egl_thread,
				authority.owner_thread_identity, 0, health)
		} $else {
			return NativeLifetimeReleaseAttempt{}
		}
	} $else {
		return NativeLifetimeReleaseAttempt{}
	}
}

fn (mut authority NativeOperationAuthority) release_linux_egl_lifetime_ticket(ticket_id u64, release_kind NativeLifetimeReleaseKind, native_object_identity u64, display_identity u64, health NativeRendererHealth) NativeLifetimeReleaseAttempt {
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			if !authority.owner_thread_is_current()
				|| release_kind !in [.egl_surface, .egl_context, .egl_display, .egl_thread]
				|| native_object_identity == 0
				|| (release_kind == .egl_display && display_identity != native_object_identity)
				|| (release_kind == .egl_thread && (display_identity != 0
				|| native_object_identity != authority.owner_thread_identity)) {
				return NativeLifetimeReleaseAttempt{}
			}
			parent_identity := if release_kind in [.egl_surface, .egl_context] {
				display_identity
			} else {
				u64(0)
			}
			claim := authority.claim_lifetime_release(ticket_id, release_kind,
				native_object_identity, parent_identity) or {
				return NativeLifetimeReleaseAttempt{
					terminal: authority.acknowledge_abandoned_lifetime_ticket(ticket_id,
						release_kind, native_object_identity, parent_identity)
				}
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			match release_kind {
				.egl_surface {
					C.v_multiwindow_linux_egl_destroy_surface(display_identity,
						native_object_identity, &raw)
				}
				.egl_context {
					C.v_multiwindow_linux_egl_destroy_context(display_identity,
						native_object_identity, &raw)
				}
				.egl_display {
					C.v_multiwindow_linux_egl_terminate(native_object_identity, &raw)
				}
				.egl_thread {
					C.v_multiwindow_linux_egl_release_thread(&raw)
				}
				else {}
			}

			result := authority.complete_lifetime_release(claim, raw, .none, health, '')
			return NativeLifetimeReleaseAttempt{
				claimed:  true
				terminal: true
				result:   result
			}
		} $else {
			return NativeLifetimeReleaseAttempt{}
		}
	} $else {
		return NativeLifetimeReleaseAttempt{}
	}
}
