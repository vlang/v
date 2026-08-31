module multiwindow

fn egl_core_context_attributes_supported(initialize NativeRenderResult, evidence NativePrimitiveEvidence) bool {
	major := initialize.primitive.observed_count
	minor := initialize.primitive.selected_value
	if major > 1 || (major == 1 && minor >= 5) {
		return true
	}
	return evidence.has(native_valid_observed_flags) && evidence.observed_flags & u64(1) != 0
}

fn egl_anchor_binding(generation u64, surface voidptr) EglBindingIdentity {
	return EglBindingIdentity{
		kind:              .anchor
		target_generation: generation
		surface:           surface
	}
}

fn egl_window_binding(id WindowId, generation u64, surface voidptr) EglBindingIdentity {
	return EglBindingIdentity{
		kind:              .window
		window:            id
		target_generation: generation
		surface:           surface
	}
}

fn egl_binding_is_exact(actual EglBindingIdentity, expected EglBindingIdentity) bool {
	return actual.kind == expected.kind && actual.window == expected.window
		&& actual.target_generation == expected.target_generation
		&& actual.surface == expected.surface && expected.surface != unsafe { nil }
}

fn egl_binding_scope(binding EglBindingIdentity) NativeRenderScope {
	return if binding.kind == .window { .window_target } else { .anchor }
}

fn egl_result_has_current_identity(result NativeRenderResult, context voidptr) bool {
	return context != unsafe { nil } && result.current_context == context
		&& result.current_draw_surface != unsafe { nil }
		&& result.current_draw_surface == result.current_read_surface
}

fn egl_verified_binding_result(bind NativeRenderResult, draw NativeRenderResult, read NativeRenderResult, context NativeRenderResult) NativeRenderResult {
	return NativeRenderResult{
		...bind
		current_draw_surface: native_pointer(draw.actual_primitive.handle)
		current_read_surface: native_pointer(read.actual_primitive.handle)
		current_context:      native_pointer(context.actual_primitive.handle)
	}
}

fn egl_unavailable_after_failed_recovery(result NativeRenderResult) NativeRenderResult {
	if result.disposition == .renderer_lost {
		return result
	}
	return NativeRenderResult{
		...result
		domain:      .egl
		scope:       .renderer
		disposition: .renderer_unavailable
		error_text:  err_render_native_renderer_unavailable
	}
}
