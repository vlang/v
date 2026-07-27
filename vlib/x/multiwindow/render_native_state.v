module multiwindow

#flag -I @VMODROOT/vlib/x/multiwindow
#insert "@VMODROOT/vlib/x/multiwindow/native_render_result.h"

const err_render_target_transient_unavailable = 'multiwindow: render target is transiently unavailable'
const err_render_target_lost = 'multiwindow: native render target was lost'
const err_render_native_window_lost = 'multiwindow: native render window was lost'
const err_render_native_batch_failed = 'multiwindow: native render batch failed'
const err_render_native_renderer_unavailable = 'multiwindow: native renderer is unavailable'
const err_render_native_renderer_lost = 'multiwindow: native renderer was lost'

enum NativeRendererHealth {
	uninitialized
	ready
	unavailable
	lost
	abandoned
}

enum NativeRenderDomain {
	none
	egl
	dxgi
	wayland
	metal
}

enum NativeRenderOperation {
	none
	display_acquire
	display_initialize
	api_bind
	config_choose
	config_visual
	display_query
	context_create
	window_surface_create
	anchor_surface_create
	make_current
	current_draw_query
	current_read_query
	current_context_query
	swap_buffers
	surface_destroy
	context_destroy
	display_terminate
	release_thread
	device_create
	device_query
	adapter_acquire
	factory_acquire
	swapchain_create
	window_association
	backbuffer_acquire
	color_texture_create
	render_view_create
	depth_texture_create
	depth_view_create
	resize_buffers
	clear_state
	present
	window_configure
	display_dispatch
	display_prepare
	display_cancel
	display_read
	display_poll
	display_fd_query
	display_roundtrip
	frame_callback
	display_flush
	drawable_acquire
	render_batch_begin
	render_batch_end
	device_status
	egl_error_query
	dxgi_removal_query
	wayland_display_error_query
	object_release
}

enum NativeRenderScope {
	none
	renderer
	anchor
	batch
	window_target
}

enum NativeRenderCallSite {
	none
	app_start
	renderer_start
	anchor_create
	anchor_prepare
	window_prepare
	window_activate
	window_finalize
	display_transport
	shutdown_anchor
	shutdown_release
}

enum NativeOperationAuthorityScope {
	none
	app_lifetime
	renderer_attempt
}

enum NativeLocalValidation {
	none
	null_input
	null_output
	zero_count
	missing_visual
	bad_revents
	binding_mismatch
	context_incompatible
	sequence_exhausted
	void_completion
}

struct NativeOperationContext {
	authority_scope        NativeOperationAuthorityScope
	authority_token        u64
	renderer_attempt_token u64
	app_identity           u64
	presence_mask          u64
	domain                 NativeRenderDomain
	operation              NativeRenderOperation
	call_site              NativeRenderCallSite
	scope                  NativeRenderScope
	window                 WindowId
	target_generation      u64
	target_identity        u64
	batch_epoch            u64
	window_lease_epoch     u64
	target_lease_epoch     u64
	ordinal                u64
}

struct NativeOperationSeed {
	presence_mask      u64
	call_site          NativeRenderCallSite
	scope              NativeRenderScope
	window             WindowId
	target_generation  u64
	target_identity    u64
	batch_epoch        u64
	window_lease_epoch u64
	target_lease_epoch u64
}

const native_context_has_window = u64(1)
const native_context_has_target_generation = u64(2)
const native_context_has_target_identity = u64(4)
const native_context_has_batch_epoch = u64(8)
const native_context_has_window_lease_epoch = u64(16)
const native_context_has_target_lease_epoch = u64(32)

const native_context_window_target_fields = native_context_has_window | native_context_has_target_generation | native_context_has_batch_epoch | native_context_has_window_lease_epoch | native_context_has_target_lease_epoch

fn (seed NativeOperationSeed) with_target_identity(identity u64) NativeOperationSeed {
	return NativeOperationSeed{
		...seed
		presence_mask:   seed.presence_mask | native_context_has_target_identity
		target_identity: identity
	}
}

fn (seed NativeOperationSeed) without_target_identity() NativeOperationSeed {
	return NativeOperationSeed{
		...seed
		presence_mask:   seed.presence_mask & ~native_context_has_target_identity
		target_identity: 0
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn native_seed_for_frame(frame RenderFrame, call_site NativeRenderCallSite) NativeOperationSeed {
		return NativeOperationSeed{
			presence_mask:      native_context_window_target_fields & ~native_context_has_target_identity
			call_site:          call_site
			scope:              .window_target
			window:             frame.window_id
			target_generation:  frame.target.target_identity
			batch_epoch:        frame.batch_epoch
			window_lease_epoch: frame.window_lease_epoch
			target_lease_epoch: frame.target_lease_epoch
		}
	}
}

struct NativePrimitiveEvidence {
	valid_mask            u64
	return_value          i64
	handle                u64
	egl_error             i64
	native_errno          i64
	wayland_display_error i64
	dxgi_removal_reason   i64
	observed_count        u64
	observed_flags        u64
	selected_value        i64
	object_identity_0     u64
	object_identity_1     u64
	object_identity_2     u64
}

struct NativePrimitiveCapture {
	actual    NativePrimitiveEvidence
	effective NativePrimitiveEvidence
}

enum NativeOperationTraceMilestone {
	real_call
	actual_primitive
	effective_primitive
	acceptance
	health_latched
	authority_release
}

enum NativeRenderDisposition {
	none
	ok
	not_presented
	transient
	target_lost
	native_window_lost
	batch_terminal
	renderer_unavailable
	renderer_lost
	operation_failed
}

enum RendererShutdownPath {
	unprepared
	orderly_anchor
	logical_abandon
}

enum NativeLifetimeReleaseKind {
	none
	egl_surface
	egl_context
	egl_display
	egl_thread
	wayland_egl_window
	wayland_surface
	wayland_frame_callback
	com_reference
	metal_device
	appkit_state
	appkit_autorelease_pool
	metal_drawable
}

enum NativeLifetimeTicketState {
	reserved
	acquiring
	provisional_bound
	bound
	releasing
	native_released
	released
	burned
	abandoned
}

struct NativeLifetimeAcquisitionTransaction {
	ticket_id           u64
	app_identity        u64
	authority_scope     NativeOperationAuthorityScope
	authority_token     u64
	release_kind        NativeLifetimeReleaseKind
	acquisition_context NativeOperationContext
	release_context     NativeOperationContext
	native_identity     u64
}

struct NativeLifetimeAcquisitionClaim {
	ticket_id       u64
	app_identity    u64
	authority_scope NativeOperationAuthorityScope
	authority_token u64
	release_kind    NativeLifetimeReleaseKind
	context         NativeOperationContext
	native_identity u64
}

struct NativeLifetimeReleaseClaim {
	ticket_id                u64
	registry_index           int
	authority_scope          NativeOperationAuthorityScope
	authority_token          u64
	release_kind             NativeLifetimeReleaseKind
	context                  NativeOperationContext
	native_identity          u64
	required_parent_identity u64
}

struct NativeLifetimeReleaseAttempt {
	claimed  bool
	terminal bool
	result   NativeRenderResult
}

struct NativeLifetimeReleaseResult {
	value           voidptr
	ticket_id       u64
	native_released bool
	ticket_retired  bool
}

enum EglBindingKind {
	none
	anchor
	window
}

struct EglBindingIdentity {
	kind              EglBindingKind
	window            WindowId
	target_generation u64
	surface           voidptr
}

struct NativeRenderResult {
	domain               NativeRenderDomain
	operation            NativeRenderOperation
	scope                NativeRenderScope
	disposition          NativeRenderDisposition
	native_code          i64
	removal_reason       i64
	native_status        i64
	display_error        i64
	current_draw_surface voidptr
	current_read_surface voidptr
	current_context      voidptr
	error_text           string
	context              NativeOperationContext
	actual_primitive     NativePrimitiveEvidence
	primitive            NativePrimitiveEvidence
	local_validation     NativeLocalValidation
}

struct RendererShutdownProof {
	path    RendererShutdownPath
	outcome NativeRenderResult
}

fn (result NativeRenderResult) succeeded() bool {
	return result.disposition == .ok
}

fn (result NativeRenderResult) is_recoverable_target() bool {
	return result.disposition == .transient || result.disposition == .target_lost
		|| result.disposition == .not_presented
}

fn (result NativeRenderResult) blocks_graphics() bool {
	return result.disposition == .renderer_unavailable || result.disposition == .renderer_lost
}

fn (health NativeRendererHealth) blocks_graphics() bool {
	return health == .unavailable || health == .lost || health == .abandoned
}

fn renderer_health_latch_unavailable(current NativeRendererHealth) NativeRendererHealth {
	if current.blocks_graphics() {
		return current
	}
	return .unavailable
}

fn renderer_health_after_result(current NativeRendererHealth, result NativeRenderResult) NativeRendererHealth {
	if current == .abandoned || current == .lost {
		return current
	}
	return match result.disposition {
		.renderer_lost { .lost }
		.renderer_unavailable { .unavailable }
		else { current }
	}
}

fn native_render_error(result NativeRenderResult) IError {
	if result.error_text != '' {
		return error(result.error_text)
	}
	return match result.disposition {
		.transient, .not_presented { error(err_render_target_transient_unavailable) }
		.target_lost { error(err_render_target_lost) }
		.native_window_lost { error(err_render_native_window_lost) }
		.batch_terminal, .operation_failed { error(err_render_native_batch_failed) }
		.renderer_unavailable { error(err_render_native_renderer_unavailable) }
		.renderer_lost { error(err_render_native_renderer_lost) }
		else { error(err_render_native_batch_failed) }
	}
}

fn native_render_error_text(disposition NativeRenderDisposition) string {
	return match disposition {
		.transient, .not_presented { err_render_target_transient_unavailable }
		.target_lost { err_render_target_lost }
		.native_window_lost { err_render_native_window_lost }
		.batch_terminal, .operation_failed { err_render_native_batch_failed }
		.renderer_unavailable { err_render_native_renderer_unavailable }
		.renderer_lost { err_render_native_renderer_lost }
		else { '' }
	}
}

fn native_render_outcome(domain NativeRenderDomain, operation NativeRenderOperation, scope NativeRenderScope, disposition NativeRenderDisposition, native_code i64, removal_reason i64, error_text string) NativeRenderResult {
	return NativeRenderResult{
		domain:         domain
		operation:      operation
		scope:          scope
		disposition:    disposition
		native_code:    native_code
		removal_reason: removal_reason
		error_text:     if error_text == '' {
			native_render_error_text(disposition)
		} else {
			error_text
		}
	}
}

fn native_render_ok(domain NativeRenderDomain, operation NativeRenderOperation, scope NativeRenderScope) NativeRenderResult {
	return native_render_outcome(domain, operation, scope, .ok, 0, 0, '')
}

@[typedef]
struct C.VMultiwindowNativePrimitive {
mut:
	valid_mask            u64
	return_value          i64
	handle                u64
	egl_error             i64
	native_errno          i64
	wayland_display_error i64
	dxgi_removal_reason   i64
	observed_count        u64
	observed_flags        u64
	selected_value        i64
	object_identity_0     u64
	object_identity_1     u64
	object_identity_2     u64
}

fn native_identity(value voidptr) u64 {
	return u64(usize(value))
}

fn native_pointer(identity u64) voidptr {
	return voidptr(usize(identity))
}

const native_valid_return_value = u64(1)
const native_valid_handle = u64(2)
const native_valid_egl_error = u64(4)
const native_valid_errno = u64(8)
const native_valid_wayland_display_error = u64(16)
const native_valid_dxgi_removal_reason = u64(32)
const native_valid_observed_count = u64(64)
const native_valid_observed_flags = u64(128)
const native_valid_selected_value = u64(256)
const native_valid_object_identity_0 = u64(512)
const native_valid_object_identity_1 = u64(1024)
const native_valid_object_identity_2 = u64(2048)

fn (evidence NativePrimitiveEvidence) has(field u64) bool {
	return evidence.valid_mask & field != 0
}

fn native_local_validation_disposition(validation NativeLocalValidation, scope NativeRenderScope) NativeRenderDisposition {
	return match validation {
		.none {
			.none
		}
		.void_completion {
			.ok
		}
		.sequence_exhausted, .bad_revents, .binding_mismatch, .context_incompatible, .zero_count,
		.missing_visual {
			.renderer_unavailable
		}
		.null_input, .null_output {
			if scope == .renderer || scope == .anchor {
				NativeRenderDisposition.renderer_unavailable
			} else {
				NativeRenderDisposition.operation_failed
			}
		}
	}
}

fn native_result_from_egl(context NativeOperationContext, capture NativePrimitiveCapture, validation NativeLocalValidation) NativeRenderResult {
	evidence := capture.effective
	actual_fatal := native_fatal_egl_disposition(context, capture.actual)
	effective_fatal := native_fatal_egl_disposition(context, evidence)
	mut disposition := native_local_validation_disposition(validation, context.scope)
	if actual_fatal != .none {
		disposition = actual_fatal
	} else if effective_fatal != .none {
		disposition = effective_fatal
	}
	if disposition == .none {
		disposition = native_egl_evidence_disposition(context, evidence)
	}
	return NativeRenderResult{
		domain:           .egl
		operation:        context.operation
		scope:            context.scope
		disposition:      disposition
		native_code:      if actual_fatal != .none {
			if capture.actual.has(native_valid_egl_error) {
				capture.actual.egl_error
			} else {
				capture.actual.return_value
			}
		} else if evidence.has(native_valid_egl_error) {
			evidence.egl_error
		} else {
			evidence.return_value
		}
		native_status:    evidence.return_value
		error_text:       native_render_error_text(disposition)
		context:          context
		actual_primitive: capture.actual
		primitive:        evidence
		local_validation: validation
	}
}

fn native_fatal_egl_disposition(context NativeOperationContext, evidence NativePrimitiveEvidence) NativeRenderDisposition {
	disposition := native_egl_evidence_disposition(context, evidence)
	if disposition in [.renderer_unavailable, .renderer_lost] {
		return disposition
	}
	return .none
}

fn native_egl_evidence_disposition(context NativeOperationContext, evidence NativePrimitiveEvidence) NativeRenderDisposition {
	if evidence.has(native_valid_egl_error) {
		error_disposition := native_egl_error_disposition(evidence.egl_error)
		if error_disposition in [.renderer_unavailable, .renderer_lost] {
			return error_disposition
		}
	}
	succeeded :=
		(context.operation in [.current_draw_query, .current_read_query, .current_context_query]
		&& evidence.has(native_valid_handle))
		|| (evidence.has(native_valid_return_value) && evidence.return_value == 1)
		|| (evidence.has(native_valid_handle) && evidence.handle != 0)
	if succeeded {
		return .ok
	}
	mut disposition := if evidence.has(native_valid_egl_error) {
		native_egl_error_disposition(evidence.egl_error)
	} else {
		NativeRenderDisposition.operation_failed
	}
	if disposition == .operation_failed && (context.scope == .renderer || context.scope == .anchor) {
		disposition = .renderer_unavailable
	}
	return disposition
}

fn native_egl_error_disposition(error_code i64) NativeRenderDisposition {
	return match int(error_code) {
		0x300e { NativeRenderDisposition.renderer_lost }
		0x3001, 0x3003, 0x3006, 0x3008 { NativeRenderDisposition.renderer_unavailable }
		0x300d, 0x3007 { NativeRenderDisposition.target_lost }
		0x300b { NativeRenderDisposition.native_window_lost }
		else { NativeRenderDisposition.operation_failed }
	}
}

fn dxgi_hresult_failed(value i64) bool {
	return u32(value) & u32(0x80000000) != 0
}

fn dxgi_hresult_is_direct_loss(value i64) bool {
	code := u32(value)
	return code == u32(0x887a0005) || code == u32(0x887a0007) || code == u32(0x887a0006)
		|| code == u32(0x887a0020)
}

fn dxgi_removal_reason_is_loss(value i64) bool {
	return dxgi_hresult_is_direct_loss(value) || u32(value) == u32(0x887a0001)
}

fn native_result_from_dxgi(context NativeOperationContext, capture NativePrimitiveCapture, validation NativeLocalValidation) NativeRenderResult {
	evidence := capture.effective
	actual_direct_loss := capture.actual.has(native_valid_return_value)
		&& dxgi_hresult_is_direct_loss(capture.actual.return_value)
	actual_fatal := actual_direct_loss
		|| (capture.actual.has(native_valid_dxgi_removal_reason)
		&& dxgi_removal_reason_is_loss(capture.actual.dxgi_removal_reason))
	effective_fatal := dxgi_hresult_is_direct_loss(evidence.return_value)
		|| (evidence.has(native_valid_dxgi_removal_reason)
		&& dxgi_removal_reason_is_loss(evidence.dxgi_removal_reason))
	mut disposition := native_local_validation_disposition(validation, context.scope)
	if actual_fatal {
		disposition = .renderer_lost
	} else if effective_fatal {
		disposition = .renderer_lost
	}
	if disposition == .none {
		if u32(evidence.return_value) == u32(0x087a0001) {
			disposition = .not_presented
		} else if context.operation == .device_status
			&& evidence.has(native_valid_dxgi_removal_reason)
			&& !dxgi_hresult_failed(evidence.dxgi_removal_reason) {
			disposition = .ok
		} else if evidence.has(native_valid_return_value)
			&& !dxgi_hresult_failed(evidence.return_value) {
			disposition = .ok
		} else {
			disposition = .operation_failed
		}
	}
	return NativeRenderResult{
		domain:           .dxgi
		operation:        context.operation
		scope:            context.scope
		disposition:      disposition
		native_code:      if actual_fatal {
			capture.actual.return_value
		} else {
			evidence.return_value
		}
		removal_reason:   if actual_fatal {
			capture.actual.dxgi_removal_reason
		} else {
			evidence.dxgi_removal_reason
		}
		native_status:    i64(evidence.observed_flags)
		error_text:       native_render_error_text(disposition)
		context:          context
		actual_primitive: capture.actual
		primitive:        evidence
		local_validation: validation
	}
}

fn native_result_from_wayland(context NativeOperationContext, capture NativePrimitiveCapture, validation NativeLocalValidation, error_text string) NativeRenderResult {
	evidence := capture.effective
	mut result_scope := context.scope
	actual_display_fatal := capture.actual.has(native_valid_wayland_display_error)
		&& capture.actual.wayland_display_error != 0
	effective_display_fatal := evidence.has(native_valid_wayland_display_error)
		&& evidence.wayland_display_error != 0
	mut disposition := native_local_validation_disposition(validation, context.scope)
	if actual_display_fatal {
		disposition = .renderer_unavailable
		result_scope = .renderer
	} else if effective_display_fatal {
		disposition = .renderer_unavailable
		result_scope = .renderer
	}
	if disposition == .none {
		if context.operation in [.window_surface_create, .anchor_surface_create, .frame_callback]
			&& evidence.has(native_valid_handle) && evidence.handle != 0 {
			disposition = .ok
		} else if !evidence.has(native_valid_return_value) {
			disposition = .operation_failed
		} else if context.operation == .frame_callback && evidence.return_value != 0 {
			disposition = .operation_failed
		} else if context.operation == .display_prepare && evidence.return_value < 0 {
			disposition = .transient
		} else if evidence.return_value >= 0 {
			disposition = .ok
		} else if context.operation == .display_flush && evidence.has(native_valid_errno)
			&& evidence.native_errno == 11 {
			disposition = .ok
		} else if context.operation == .display_poll && evidence.has(native_valid_errno)
			&& (evidence.native_errno == 11 || evidence.native_errno == 4) {
			disposition = .transient
		} else {
			disposition = .operation_failed
		}
	}
	return NativeRenderResult{
		domain:           .wayland
		operation:        context.operation
		scope:            result_scope
		disposition:      disposition
		native_code:      evidence.native_errno
		native_status:    evidence.return_value
		display_error:    if actual_display_fatal {
			capture.actual.wayland_display_error
		} else {
			evidence.wayland_display_error
		}
		error_text:       if disposition == .ok { '' } else { error_text }
		context:          context
		actual_primitive: capture.actual
		primitive:        evidence
		local_validation: validation
	}
}

fn native_wayland_logical_result(operation NativeRenderOperation, scope NativeRenderScope, disposition NativeRenderDisposition, native_code i64, error_text string) NativeRenderResult {
	return native_render_outcome(.wayland, operation, scope, disposition, native_code, 0,
		error_text)
}

fn native_result_from_metal(context NativeOperationContext, capture NativePrimitiveCapture, validation NativeLocalValidation, error_text string) NativeRenderResult {
	evidence := capture.effective
	mut disposition := native_local_validation_disposition(validation, context.scope)
	if context.operation == .drawable_acquire && validation == .null_output {
		disposition = .transient
	}
	if validation == .binding_mismatch && context.scope == .window_target {
		disposition = .target_lost
	}
	if disposition == .operation_failed && context.operation == .drawable_acquire {
		disposition = .transient
	}
	if disposition == .none {
		if context.operation in [.device_create, .window_surface_create, .anchor_surface_create, .render_batch_begin]
			&& evidence.has(native_valid_handle) && evidence.handle != 0 {
			disposition = .ok
		} else if context.operation == .drawable_acquire && evidence.has(native_valid_handle)
			&& evidence.handle != 0 && evidence.object_identity_0 != 0
			&& evidence.observed_count != 0 && evidence.selected_value > 0 {
			disposition = .ok
		} else if evidence.has(native_valid_return_value) && evidence.return_value != 0 {
			disposition = .ok
		} else if context.operation == .drawable_acquire {
			disposition = .transient
		} else if context.operation == .present && context.scope == .window_target {
			disposition = .target_lost
		} else if context.scope == .renderer || context.scope == .anchor {
			disposition = .renderer_unavailable
		} else {
			disposition = .operation_failed
		}
	}
	return NativeRenderResult{
		domain:           .metal
		operation:        context.operation
		scope:            context.scope
		disposition:      disposition
		native_code:      evidence.return_value
		native_status:    evidence.return_value
		error_text:       if disposition == .ok { '' } else { error_text }
		context:          context
		actual_primitive: capture.actual
		primitive:        evidence
		local_validation: validation
	}
}
