module multiwindow

import os

$if gg_multiwindow ? || x_multiwindow_render ? {
	import gg.testdata.multiwindow_probe_gate
	import gg.testdata.multiwindow_sokol_trace
	import sokol.gfx
	import time
}

const native_verified_egl_binding_ordinal_span_for_test = u64(5)
const native_wayland_frame_callback_setup_ordinal_span_for_test = u64(5)
const native_wayland_transport_boundary_ordinal_span_for_test = u64(2)
const native_wayland_transport_compound_trace_span_for_test = 8
const native_wayland_listener_registration_ordinal_offset_for_test = u64(2)
const native_wayland_frame_callback_cleanup_ordinal_offset_from_listener_for_test =
	native_wayland_frame_callback_setup_ordinal_span_for_test -
	native_wayland_listener_registration_ordinal_offset_for_test - 1
const native_wayland_quiescent_read_predecessor_count_for_test = u64(5)
const native_wayland_quiescent_read_ordinal_offset_for_test = native_wayland_transport_boundary_ordinal_span_for_test * native_wayland_quiescent_read_predecessor_count_for_test
const native_wayland_poll_predecessor_count_for_test = u64(4)
const native_wayland_poll_ordinal_offset_for_test = native_wayland_transport_boundary_ordinal_span_for_test * native_wayland_poll_predecessor_count_for_test

@[heap]
struct NativeWaylandSyncState {
mut:
	done              bool
	callback_identity u64
}

@[heap]
struct NativeWaylandListenerFailureState {
mut:
	lease                    RenderTargetLease
	pass_snapshot            RenderWindowSnapshot
	selector                 NativeOperationContext
	callback_parent_identity u64
}

struct NativeWaylandSyncDispatchProof {
	outcome      NativeRenderResult
	read_context NativeOperationContext
	read_count   int
	snapshot     NativeAuthorityProofSnapshot
}

#flag -DSOKOL_TRACE_HOOKS
#preinclude windows "@VMODROOT/vlib/x/multiwindow/testdata/native_win32_lifetime_oracle_helpers.h"

$if gg_multiwindow ? || x_multiwindow_render ? {
	#flag darwin -DV_MULTIWINDOW_NATIVE_PROOF_TEST
	$if windows {
		$if sokol_d3d11 ? {
			#flag windows -DV_MULTIWINDOW_NATIVE_PROOF_TEST
		}
	}
}

#insert "@VMODROOT/vlib/x/multiwindow/native_render_result.h"
#insert "@VMODROOT/vlib/x/multiwindow/testdata/native_primitive_proof_helpers.h"
#insert "@VMODROOT/vlib/x/multiwindow/testdata/native_primitive_abi_consumer.c"

$if linux {
	#include <errno.h>
	#include <signal.h>
	#include <sys/wait.h>

	$if x_multiwindow_x11 ? || sokol_wayland ? {
		#flag -include @VMODROOT/vlib/x/multiwindow/testdata/native_egl_release_oracle_helpers.h
		#insert "@VMODROOT/vlib/x/multiwindow/testdata/native_egl_release_oracle_helpers.h"

		fn C.v_multiwindow_test_egl_release_oracle_reset()
		fn C.v_multiwindow_test_release_oracle_reset_sequence()
		fn C.v_multiwindow_test_egl_release_oracle_count() u64
		fn C.v_multiwindow_test_egl_release_oracle_overflow() int
		fn C.v_multiwindow_test_egl_release_oracle_sequence(index u64) u64
		fn C.v_multiwindow_test_egl_release_oracle_kind(index u64) u64
		fn C.v_multiwindow_test_egl_release_oracle_identity(index u64) u64
		fn C.v_multiwindow_test_egl_release_oracle_parent(index u64) u64
	}
	$if sokol_wayland ? {
		#include <poll.h>
		#include <wayland-client.h>

		#flag -include @VEXEROOT/thirdparty/sokol/xdg-shell-client-protocol.h
		#flag -include @VEXEROOT/thirdparty/sokol/xdg-decoration-unstable-v1-client-protocol.h
		#flag -include @VMODROOT/vlib/x/multiwindow/testdata/native_wayland_release_oracle_helpers.h
		#insert "@VMODROOT/vlib/x/multiwindow/testdata/native_wayland_release_oracle_helpers.h"

		fn C.v_multiwindow_test_wayland_release_oracle_reset()
		fn C.v_multiwindow_test_wayland_release_oracle_count() u64
		fn C.v_multiwindow_test_wayland_release_oracle_overflow() int
		fn C.v_multiwindow_test_wayland_release_oracle_sequence(index u64) u64
		fn C.v_multiwindow_test_wayland_release_oracle_kind(index u64) u64
		fn C.v_multiwindow_test_wayland_release_oracle_identity(index u64) u64
		fn C.v_multiwindow_test_wayland_release_oracle_mode(index u64) u64
		fn C.v_multiwindow_test_wayland_invoke_frame_done_trampoline(data voidptr, callback voidptr, time u32)

		struct C.wl_callback {}

		struct C.wl_callback_listener {
			done fn (voidptr, &C.wl_callback, u32) = unsafe { nil }
		}

		fn C.wl_display_dispatch(display &C.wl_display) int
		fn C.wl_display_flush(display &C.wl_display) int
		fn C.wl_display_get_fd(display &C.wl_display) int
		fn C.wl_display_sync(display &C.wl_display) &C.wl_callback
		fn C.wl_callback_add_listener(callback &C.wl_callback, listener &C.wl_callback_listener, data voidptr) int
		fn C.wl_callback_destroy(callback &C.wl_callback)

		@[markused]
		fn native_wayland_sync_done_for_test(data voidptr, callback &C.wl_callback, callback_data u32) {
			_ = callback_data
			if data != unsafe { nil } {
				mut state := unsafe { &NativeWaylandSyncState(data) }
				state.done = true
				state.callback_identity = native_identity(callback)
			}
			C.wl_callback_destroy(callback)
		}
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows {
		$if sokol_d3d11 ? {
			@[typedef]
			struct C.VMultiwindowTestWin32OracleRecord {
				sequence           u64
				kind               u64
				identity           u64
				parent_identity    u64
				output_identity    u64
				auxiliary_identity u64
				observed_count     u64
				valid_mask         u64
				return_value       i64
				thread_identity    u64
			}

			fn C.v_multiwindow_test_win32_oracle_reset()
			fn C.v_multiwindow_test_win32_oracle_count_get() u64
			fn C.v_multiwindow_test_win32_oracle_overflow_get() int
			fn C.v_multiwindow_test_win32_oracle_record_get(index u64) C.VMultiwindowTestWin32OracleRecord
			fn C.v_multiwindow_test_win32_resize_invalid_call_arm() int
			fn C.v_multiwindow_test_win32_resize_invalid_call_reset()
			fn C.v_multiwindow_test_win32_resize_invalid_call_consumed_once() int
			fn C.v_multiwindow_test_win32_resize_invalid_call_code() i64
			fn C.v_multiwindow_test_win32_release_kind() u64
			fn C.v_multiwindow_test_win32_resize_buffers_kind() u64
			fn C.v_multiwindow_test_win32_fake_unknown_reset(references u64)
			fn C.v_multiwindow_test_win32_fake_unknown_identity() u64
			fn C.v_multiwindow_test_win32_fake_unknown_references() u64
			fn C.v_multiwindow_test_win32_fake_d3d_reset()
			fn C.v_multiwindow_test_win32_fake_d3d_device_identity() u64
			fn C.v_multiwindow_test_win32_fake_d3d_context_identity() u64
			fn C.v_multiwindow_test_win32_fake_d3d_count() u64
			fn C.v_multiwindow_test_win32_fake_d3d_kind(index u64) u64
			fn C.v_multiwindow_test_win32_fake_d3d_identity(index u64) u64
			fn C.v_multiwindow_test_win32_fake_d3d_remaining(index u64) u64
			fn C.v_multiwindow_test_win32_fake_d3d_overflow() int
		}
	}

	$if darwin {
		#insert "@VMODROOT/vlib/x/multiwindow/testdata/native_appkit_lifetime_oracle_helpers.h"

		@[typedef]
		struct C.VMultiwindowTestAppKitOracleRecord {
			sequence             u64
			kind                 u64
			identity             u64
			parent_identity      u64
			output_identity      u64
			auxiliary_identity   u64
			auxiliary_identity_1 u64
			auxiliary_identity_2 u64
			valid_mask           u64
			thread_identity      u64
		}

		@[typedef]
		struct C.VMultiwindowAppKitSideEffectRecord {
			generation         u64
			sequence           u64
			kind               u64
			subject            u64
			identity           u64
			parent_identity    u64
			before_identity    u64
			after_identity     u64
			auxiliary_identity u64
			thread_identity    u64
			main_thread        u64
		}

		fn C.v_multiwindow_test_appkit_oracle_reset()
		fn C.v_multiwindow_test_appkit_oracle_count_get() u64
		fn C.v_multiwindow_test_appkit_oracle_overflow_get() int
		fn C.v_multiwindow_test_appkit_oracle_record_get(index u64) C.VMultiwindowTestAppKitOracleRecord
		fn C.v_multiwindow_appkit_side_effect_reset() u64
		fn C.v_multiwindow_appkit_side_effect_generation() u64
		fn C.v_multiwindow_appkit_side_effect_count() u64
		fn C.v_multiwindow_appkit_side_effect_overflow() int
		fn C.v_multiwindow_appkit_side_effect_record(index u64, out_record &C.VMultiwindowAppKitSideEffectRecord) int
		fn C.v_multiwindow_appkit_side_effect_create_release_probe(subject u64) voidptr
		fn C.v_multiwindow_appkit_side_effect_probe_generation(probe voidptr) u64
		fn C.v_multiwindow_appkit_side_effect_probe_subject(probe voidptr) u64
		fn C.v_multiwindow_appkit_native_proof_install_physical_nil_drawable(state voidptr, layer voidptr, device voidptr, owner_thread u64) voidptr
		fn C.v_multiwindow_appkit_native_proof_restore_physical_nil_drawable(lease voidptr, owner_thread u64) int
		fn C.v_multiwindow_test_appkit_admit_window(state voidptr) int
	}
}

fn C.v_multiwindow_test_native_primitive_reset()
fn C.v_multiwindow_test_native_primitive_call_count() u64
fn C.v_multiwindow_test_native_primitive(valid_mask u64, return_value i64, handle u64, native_code i64, observed_count u64, observed_flags u64, out &C.VMultiwindowNativePrimitive)
fn C.v_multiwindow_test_native_primitive_abi_size() usize
fn C.v_multiwindow_test_native_primitive_abi_last_offset() usize
fn C.v_multiwindow_test_common_native_primitive_size() usize
fn C.v_multiwindow_test_common_native_primitive_mask() u64

struct NativeContextMismatchCase {
	label   string
	context NativeOperationContext
}

enum NativeReleaseOracleDomain {
	egl
	x11
	wayland
}

struct NativeReleaseOracleRecord {
	sequence        u64
	domain          NativeReleaseOracleDomain
	kind            u64
	identity        u64
	parent_identity u64
	mode            u64
}

struct NativeReleaseOracleSnapshot {
	records              []NativeReleaseOracleRecord
	callback_completions []NativeReleaseOracleRecord
	unticketed_releases  []NativeReleaseOracleRecord
	overflow             bool
}

struct NativeIdentityReleaseOrder {
	child  u64
	parent u64
}

struct NativeWaylandLocalReleaseProof {
	identities      []u64
	orders          []NativeIdentityReleaseOrder
	registry        u64
	display         u64
	pending_alias   u64
	pending_drop_fd int
}

struct NativeAuthorityProofSnapshot {
	app_identity           u64
	app_lifetime_token     u64
	renderer_attempt_token u64
	owner_thread_identity  u64
	next_ordinal           u64
	next_proof_generation  u64
	sequence_exhausted     bool
	terminal_cause         NativeLocalValidation
	proof_armed            bool
	proof_generation       u64
	proof_ordinal_floor    u64
	proof_accepting_plans  bool
	plan                   [native_primitive_plan_capacity]NativePrimitivePlanEntry
	trace                  [native_operation_trace_capacity]NativeOperationTraceEntry
	trace_len              int
	trace_overflow         bool
	registry               NativeLifetimeRegistryProofSnapshot
}

struct NativeLifetimeTicketProofSnapshot {
	ticket_id                u64
	app_identity             u64
	authority_scope          NativeOperationAuthorityScope
	authority_token          u64
	domain                   NativeRenderDomain
	release_kind             NativeLifetimeReleaseKind
	owner_seed               NativeOperationSeed
	proof_generation         u64
	context                  NativeOperationContext
	native_identity          u64
	required_parent_identity u64
	parent_authority_scope   NativeOperationAuthorityScope
	parent_authority_token   u64
	state                    NativeLifetimeTicketState
}

struct NativeLifetimeRegistryProofSnapshot {
	app_identity           u64
	app_lifetime_token     u64
	renderer_attempt_token u64
	owner_thread_identity  u64
	tickets                []NativeLifetimeTicketProofSnapshot
}

struct NativePhaseAQueuedEventSnapshot {
	sequence u64
	event    QueuedEvent
}

struct NativePhaseATouchSnapshot {
	active    bool
	id        int
	window_id WindowId
	x         f32
	y         f32
}

struct NativePhaseAWindowOwnershipSnapshot {
	id                             WindowId
	native_operations_identity     u64
	owner_identity                 u64
	native_window_identity         u64
	native_aux_identity_0          u64
	native_aux_identity_1          u64
	native_aux_identity_2          u64
	resizable                      bool
	config                         WindowConfig
	cursor_shape                   CursorShape
	width                          int
	height                         int
	min_width                      int
	min_height                     int
	pending_toplevel_width         int
	pending_toplevel_height        int
	configured                     bool
	pending_egl_resize             bool
	pending_events                 []NativePhaseAQueuedEventSnapshot
	mouse_x                        f32
	mouse_y                        f32
	mouse_dx                       f32
	mouse_dy                       f32
	mouse_pos_valid                bool
	mouse_buttons                  u8
	key_repeat                     [256]bool
	window_state                   int
	render_target_generation       u64
	native_destroyed               bool
	egl_surface                    u64
	egl_surface_ticket             u64
	wl_egl_window                  u64
	wl_egl_window_ticket           u64
	frame_callback                 u64
	frame_callback_ticket          u64
	frame_ready                    bool
	fallback_buffer_count          int
	fallback_buffers               [3]u64
	fallback_current_buffer        u64
	fallback_buffer_width          int
	fallback_buffer_height         int
	toplevel_decoration_configured bool
	toplevel_decoration_mode       u32
	user_action_serial             u32
	user_action_poll               u64
	user_action_serial_valid       bool
}

struct NativePhaseABackendOwnershipSnapshot {
	kind                          BackendKind
	app_status                    AppStatus
	stop_terminal                 string
	started                       bool
	render_health                 NativeRendererHealth
	render_sequence               u64
	native_display                u64
	egl_display                   u64
	egl_config                    u64
	egl_context                   u64
	anchor_surface                u64
	anchor_wl_egl_window          u64
	anchor_wl_surface             u64
	wayland_compositor            u64
	egl_display_ticket            u64
	egl_context_ticket            u64
	egl_thread_ticket             u64
	anchor_surface_ticket         u64
	anchor_wl_egl_window_ticket   u64
	anchor_wl_surface_ticket      u64
	anchor_generation             u64
	binding_kind                  EglBindingKind
	binding_window                WindowId
	binding_target_generation     u64
	binding_surface               u64
	egl_bad_current_recovery_used bool
	native_visual_id              int
	poll_generation               u64
	poll_error                    string
	event_sequence_terminal       string
	wayland_display_unavailable   bool
	wayland_display_error         i64
	native_operations_identity    u64
	x11_screen                    int
	x11_xdnd_version              i64
	x11_keycodes                  [256]int
	data_offer_has_uri_list       bool
	data_offer_source_actions     u32
	data_offer_selected_action    u32
	data_offer_action_received    bool
	data_offer_window             WindowId
	data_offer_window_valid       bool
	pending_drop_fd               int
	pending_drop_window           WindowId
	pending_drop_window_valid     bool
	pending_drop_source_actions   u32
	pending_drop_selected_action  u32
	pending_drop_action_received  bool
	pending_drop_poll_cycles      int
	pending_drop_buffer           []u8
	pointer_focus                 WindowId
	pointer_focused               bool
	pointer_enter_serial          u32
	pointer_enter_serial_valid    bool
	keyboard_focus                WindowId
	keyboard_focused              bool
	keyboard_repeat_rate          int
	keyboard_repeat_delay         int
	keyboard_repeat_active        bool
	keyboard_repeat_raw_key       u32
	keyboard_repeat_key_code      int
	keyboard_repeat_window        WindowId
	keyboard_repeat_next_ns       u64
	keyboard_repeat_interval_ns   u64
	pointer_buttons               u32
	modifiers                     u32
	keys_down                     [512]bool
	touches                       []NativePhaseATouchSnapshot
	backend_owned_identities      []u64
	backend_state_words           []u64
	windows                       []NativePhaseAWindowOwnershipSnapshot
	registry                      NativeLifetimeRegistryProofSnapshot
	authority                     NativeAuthorityProofSnapshot
}

const native_egl_bad_display_child_marker = 'V_MULTIWINDOW_NATIVE_PROOF_EGL_BAD_DISPLAY_CHILD'
const native_appkit_ordinal_exhaustion_child_marker = 'V_MULTIWINDOW_NATIVE_PROOF_APPKIT_ORDINAL_EXHAUSTION_CHILD'

$if gg_multiwindow ? || x_multiwindow_render ? {
	@[markused]
	fn native_terminal_child_marker_for_test() string {
		if os.getenv(native_egl_bad_display_child_marker) == '1' {
			return native_egl_bad_display_child_marker
		}
		$if darwin {
			if os.getenv(native_appkit_ordinal_exhaustion_child_marker) == '1' {
				return native_appkit_ordinal_exhaustion_child_marker
			}
		}
		return ''
	}

	@[markused]
	fn native_run_terminal_child_for_test(marker string) ! {
		if marker == native_egl_bad_display_child_marker {
			native_egl_exercise_terminal_failure(i64(0x3008))!
			return
		}
		$if darwin {
			if marker == native_appkit_ordinal_exhaustion_child_marker {
				native_appkit_exercise_phase_b_ordinal_exhaustion_for_test()!
				return
			}
		}
		return error('unsupported native terminal child marker `${marker}`')
	}
}

fn before_each() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		marker := native_terminal_child_marker_for_test()
		if marker == '' {
			return
		}
		native_run_terminal_child_for_test(marker) or {
			assert false, err.msg()
			return
		}
		exit(0)
	}
}

fn after_each() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if native_terminal_child_marker_for_test() != '' {
			exit(86)
		}
	}
}

fn test_native_primitive_abi_is_portable_and_included_from_production_header() {
	expected_mask := native_valid_return_value | native_valid_handle | native_valid_egl_error | native_valid_errno | native_valid_wayland_display_error | native_valid_dxgi_removal_reason | native_valid_observed_count | native_valid_observed_flags | native_valid_selected_value | native_valid_object_identity_0 | u64(1024) | u64(2048)
	assert C.v_multiwindow_test_native_primitive_abi_size() == usize(104)
	assert C.v_multiwindow_test_common_native_primitive_size() == usize(104)
	assert C.v_multiwindow_test_native_primitive_abi_last_offset() == usize(96)
	assert C.v_multiwindow_test_common_native_primitive_mask() == expected_mask
}

fn test_native_primitive_darwin_generated_c_has_no_linux_egl_boundary() {
	fixture := os.join_path(@VMODROOT, 'vlib', 'x', 'multiwindow', 'testdata',
		'native_primitive_darwin_consumer.v')
	output := os.join_path(os.temp_dir(), 'v_multiwindow_native_primitive_darwin_${os.getpid()}.c')
	defer {
		os.rm(output) or {}
	}
	command := '${os.quoted_path(@VEXE)} -os macos -b c -d x_multiwindow_render -o ${os.quoted_path(output)} ${os.quoted_path(fixture)}'
	result := os.execute(command)
	assert result.exit_code == 0, 'Darwin native primitive generated-C gate failed\ncommand: ${command}\noutput:\n${result.output}'
	generated := os.read_file(output) or { panic(err) }
	for marker in ['<EGL/', 'EGLDisplay', 'EGLSurface', 'EGLContext', 'EGLConfig', 'EGLBoolean',
		'EGLint', 'EGLNative', 'v_multiwindow_linux_egl_', 'eglGetDisplay', 'eglGetError',
		'eglInitialize', 'eglBindAPI', 'eglChooseConfig', 'eglCreate', 'eglMakeCurrent',
		'eglSwapBuffers', 'eglDestroy', 'eglTerminate', 'eglReleaseThread'] {
		assert !generated.contains(marker), 'Darwin native primitive generated C leaked `${marker}`'
	}
	assert generated.contains('VMultiwindowNativePrimitive')
}

fn test_native_proof_authority_has_exact_bounds_and_explicit_lifecycle() {
	assert native_primitive_plan_capacity == 8
	assert native_operation_trace_capacity == $if linux && sokol_wayland ? {
		320
	} $else {
		256
	}
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 41, 73)!
	assert authority.proof == unsafe { nil }
	assert !authority.arm(native_proof_baseline_context(), NativePrimitiveEvidence{})

	C.v_multiwindow_test_native_primitive_reset()
	raw := native_proof_call_primitive(native_valid_return_value, 1, 0, 0)
	capture := authority.capture_call(native_proof_baseline_context(), raw)
	assert C.v_multiwindow_test_native_primitive_call_count() == 1
	assert capture.actual.return_value == 1
	assert capture.effective.return_value == 1
	assert authority.proof == unsafe { nil }

	assert authority.arm_proof()
	assert authority.proof != unsafe { nil }
	assert authority.proof.trace_len == 0
	assert authority.arm(native_proof_baseline_context(), NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	})
	authority.record_entry(NativeOperationTraceEntry{
		milestone: .real_call
		context:   native_proof_baseline_context()
	})
	assert authority.proof.trace_len == 1
	_ = authority.inject(native_proof_baseline_context(), NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	})
	assert authority.reset_proof()
	assert authority.proof != unsafe { nil }
	assert authority.proof.trace_len == 0
	assert !authority.proof.trace_overflow
	for entry in authority.proof.plan {
		assert !entry.armed
	}
	assert authority.disarm_proof()
	assert authority.proof == unsafe { nil }
	authority.record_entry(NativeOperationTraceEntry{
		milestone: .real_call
	})
	assert authority.proof == unsafe { nil }
}

fn test_native_lifetime_registry_transitions_release_children_before_parent_and_gates_rebind() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 701, 709)!
	assert authority.arm_proof()
	first_generation := authority.proof.generation
	display_ticket, _ := native_reserve_lifetime_ticket_for_test(mut authority, .egl_display, NativeOperationSeed{
		call_site: .renderer_start
		scope:     .renderer
	})!
	native_assert_live_ticket_for_test(&authority, display_ticket, .egl_display, .reserved, 0, 0)
	display_reserved := native_lifetime_ticket_snapshot_for_test(&authority, display_ticket)!
	assert display_reserved.proof_generation == first_generation
	authority.bind_lifetime_ticket(display_ticket, 0xd150, 0)
	native_assert_live_ticket_for_test(&authority, display_ticket, .egl_display, .bound, 0xd150, 0)
	surface_ticket, _ := native_reserve_lifetime_ticket_for_test(mut authority, .egl_surface, NativeOperationSeed{
		presence_mask:     native_context_has_window | native_context_has_target_generation
		call_site:         .window_prepare
		scope:             .window_target
		window:            WindowId{
			app_instance: 701
			slot:         3
			generation:   5
		}
		target_generation: 11
	})!
	native_assert_live_ticket_for_test(&authority, surface_ticket, .egl_surface, .reserved, 0, 0)
	surface_reserved := native_lifetime_ticket_snapshot_for_test(&authority, surface_ticket)!
	assert surface_reserved.proof_generation == first_generation
	authority.bind_lifetime_ticket(surface_ticket, 0x5a50, 0xd150)
	native_assert_live_ticket_for_test(&authority, surface_ticket, .egl_surface, .bound, 0x5a50,
		0xd150)
	plan_context := NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        authority.renderer_attempt_token
		renderer_attempt_token: authority.renderer_attempt_token
		app_identity:           authority.app_identity
		domain:                 .egl
		operation:              .make_current
		call_site:              .window_activate
		scope:                  .window_target
		ordinal:                authority.next_ordinal
	}
	assert authority.arm(plan_context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	})
	assert native_proof_evidence_equal(authority.inject(plan_context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	}), NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	})
	assert !authority.has_pending_native_plans()
	assert authority.proof.trace_len == 0

	app_tickets_before_advance := native_lifetime_registry_snapshot(&authority)
	authority.advance_renderer_attempt(701, 719)!
	assert authority.app_lifetime_token == 708
	assert authority.renderer_attempt_token == 719
	app_tickets_after_advance := native_lifetime_registry_snapshot(&authority)
	assert app_tickets_after_advance.renderer_attempt_token == 719
	assert app_tickets_after_advance.tickets.len == app_tickets_before_advance.tickets.len
	for index in 0 .. app_tickets_before_advance.tickets.len {
		native_lifetime_ticket_assert_equal(app_tickets_before_advance.tickets[index],
			app_tickets_after_advance.tickets[index])
		assert app_tickets_after_advance.tickets[index].authority_scope == .app_lifetime
		assert app_tickets_after_advance.tickets[index].authority_token == 708
	}

	outgoing_plan := NativeOperationContext{
		...plan_context
		authority_token:        authority.renderer_attempt_token
		renderer_attempt_token: authority.renderer_attempt_token
		ordinal:                authority.next_ordinal
	}
	assert authority.arm(outgoing_plan, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -2
	})
	before_plan_block := native_proof_snapshot(&authority)
	mut rebind_error := ''
	authority.advance_renderer_attempt(701, 729) or { rebind_error = err.msg() }
	assert rebind_error == err_render_native_renderer_unavailable
	native_proof_assert_snapshots_equal(before_plan_block, native_proof_snapshot(&authority))
	_ = authority.inject(outgoing_plan, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	})

	attempt_ticket, _ := native_reserve_lifetime_ticket_for_scope_for_test(mut authority,
		.renderer_attempt, .wayland_frame_callback, NativeOperationSeed{
		presence_mask:     native_context_has_window | native_context_has_target_generation
		call_site:         .window_finalize
		scope:             .window_target
		window:            WindowId{
			app_instance: 701
			slot:         7
			generation:   9
		}
		target_generation: 13
	})!
	authority.bind_lifetime_ticket(attempt_ticket, 0xca11, 0x5fce)
	attempt_snapshot := native_lifetime_ticket_snapshot_for_test(&authority, attempt_ticket)!
	assert attempt_snapshot.authority_scope == .renderer_attempt
	assert attempt_snapshot.authority_token == 719
	assert attempt_snapshot.parent_authority_scope == .none
	assert attempt_snapshot.parent_authority_token == 0
	before_ticket_block := native_proof_snapshot(&authority)
	rebind_error = ''
	authority.advance_renderer_attempt(701, 729) or { rebind_error = err.msg() }
	assert rebind_error == err_render_native_renderer_unavailable
	native_proof_assert_snapshots_equal(before_ticket_block, native_proof_snapshot(&authority))
	attempt_claim := authority.claim_lifetime_release(attempt_ticket, .wayland_frame_callback,
		0xca11, 0x5fce) or { panic('attempt-scoped callback claim was rejected') }
	attempt_result := authority.complete_lifetime_release(attempt_claim, C.VMultiwindowNativePrimitive{},
		.void_completion, .ready, '')
	assert attempt_result.succeeded()
	assert native_lifetime_ticket_index_for_test(&authority, attempt_ticket) == -1
	native_assert_lifetime_release_sequence_for_test(&authority, attempt_claim.context, 0)
	assert !authority.reset_proof()
	assert !authority.proof.accepting_plans
	assert authority.proof.generation == first_generation

	stale_before := native_lifetime_registry_snapshot(&authority)
	assert !native_lifetime_claim_succeeds_for_test(mut authority, surface_ticket, .egl_surface,
		0x5a51, 0xd150)
	assert !native_lifetime_claim_succeeds_for_test(mut authority, surface_ticket, .egl_context,
		0x5a50, 0xd150)
	native_lifetime_registry_assert_snapshots_equal(stale_before,
		native_lifetime_registry_snapshot(&authority))
	surface_claim := authority.claim_lifetime_release(surface_ticket, .egl_surface, 0x5a50, 0xd150) or {
		panic('exact surface lifetime claim was rejected')
	}
	native_assert_live_ticket_for_test(&authority, surface_ticket, .egl_surface, .releasing,
		0x5a50, 0xd150)
	duplicate_before := native_lifetime_registry_snapshot(&authority)
	assert !native_lifetime_claim_succeeds_for_test(mut authority, surface_ticket, .egl_surface,
		0x5a50, 0xd150)
	native_lifetime_registry_assert_snapshots_equal(duplicate_before,
		native_lifetime_registry_snapshot(&authority))
	surface_raw := native_proof_call_primitive(native_valid_return_value, 1, 0, 0)
	surface_result := authority.complete_lifetime_release(surface_claim, surface_raw, .none,
		.ready, '')
	assert surface_result.succeeded()
	assert native_lifetime_ticket_index_for_test(&authority, surface_ticket) == -1
	retired_before := native_lifetime_registry_snapshot(&authority)
	assert !native_lifetime_claim_succeeds_for_test(mut authority, surface_ticket, .egl_surface,
		0x5a50, 0xd150)
	native_lifetime_registry_assert_snapshots_equal(retired_before,
		native_lifetime_registry_snapshot(&authority))

	display_claim := authority.claim_lifetime_release(display_ticket, .egl_display, 0xd150, 0) or {
		panic('exact display lifetime claim was rejected')
	}
	native_assert_live_ticket_for_test(&authority, display_ticket, .egl_display, .releasing,
		0xd150, 0)
	display_raw := native_proof_call_primitive(native_valid_return_value, 1, 0, 0)
	display_result := authority.complete_lifetime_release(display_claim, display_raw, .none,
		.ready, '')
	assert display_result.succeeded()
	assert authority.lifetime_tickets.len == 0
	native_assert_lifetime_release_sequence_for_test(&authority, surface_claim.context, 0)
	surface_release := native_lifetime_release_start_for_test(&authority, surface_claim.context)
	display_release := native_lifetime_release_start_for_test(&authority, display_claim.context)
	assert surface_release < display_release

	authority.advance_renderer_attempt(701, 729)!
	assert authority.renderer_attempt_token == 729
	assert authority.lifetime_tickets.len == 0
	assert authority.reset_proof()
	assert authority.proof.generation != first_generation
	assert authority.proof.accepting_plans
	assert authority.disarm_proof()
}

fn test_native_lifetime_null_burn_and_owner_ack_abandonment_are_terminal() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 811, 821)!
	assert authority.arm_proof()
	null_ticket, _ := native_reserve_lifetime_ticket_for_test(mut authority, .egl_surface, NativeOperationSeed{
		call_site: .window_prepare
		scope:     .window_target
	})!
	native_assert_live_ticket_for_test(&authority, null_ticket, .egl_surface, .reserved, 0, 0)
	trace_before_burn := authority.proof.trace_len
	assert authority.burn_lifetime_ticket(null_ticket)
	assert native_lifetime_ticket_index_for_test(&authority, null_ticket) == -1
	assert authority.proof.trace_len == trace_before_burn
	after_burn := native_lifetime_registry_snapshot(&authority)
	assert !authority.burn_lifetime_ticket(null_ticket)
	native_lifetime_registry_assert_snapshots_equal(after_burn,
		native_lifetime_registry_snapshot(&authority))

	abandoned_ticket, _ := native_reserve_lifetime_ticket_for_test(mut authority, .egl_surface, NativeOperationSeed{
		call_site: .window_prepare
		scope:     .window_target
	})!
	authority.bind_lifetime_ticket(abandoned_ticket, 0xa550, 0xdead)
	native_assert_live_ticket_for_test(&authority, abandoned_ticket, .egl_surface, .abandoned,
		0xa550, 0xdead)
	abandoned_before := native_lifetime_registry_snapshot(&authority)
	assert !authority.acknowledge_abandoned_lifetime_ticket(abandoned_ticket, .egl_surface, 0xa551,
		0xdead)
	assert !native_lifetime_claim_succeeds_for_test(mut authority, abandoned_ticket, .egl_surface,
		0xa550, 0xdead)
	native_lifetime_registry_assert_snapshots_equal(abandoned_before,
		native_lifetime_registry_snapshot(&authority))
	assert authority.acknowledge_abandoned_lifetime_ticket(abandoned_ticket, .egl_surface, 0xa550,
		0xdead)
	assert authority.lifetime_tickets.len == 0
	assert authority.proof.trace_len == trace_before_burn
	assert authority.disarm_proof()
}

fn test_native_lifetime_retirement_survives_trace_overflow_and_preserves_prefix() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 907, 911)!
	assert authority.arm_proof()
	proof_generation := authority.proof.generation
	ticket, _ := native_reserve_lifetime_ticket_for_test(mut authority, .egl_thread, NativeOperationSeed{
		call_site: .shutdown_release
		scope:     .renderer
	})!
	authority.bind_lifetime_ticket(ticket, authority.owner_thread_identity, 0)
	native_assert_live_ticket_for_test(&authority, ticket, .egl_thread, .bound,
		authority.owner_thread_identity, 0)
	overflow_ticket := native_lifetime_ticket_snapshot_for_test(&authority, ticket)!
	assert overflow_ticket.proof_generation == proof_generation
	assert overflow_ticket.authority_scope == .app_lifetime
	assert overflow_ticket.authority_token == 910
	for index in 0 .. (native_operation_trace_capacity + 1) {
		authority.record_entry(NativeOperationTraceEntry{
			milestone: .real_call
			context:   NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        911
				renderer_attempt_token: 911
				app_identity:           907
				ordinal:                u64(index + 1000)
			}
		})
	}
	prefix := authority.proof.trace
	assert !authority.reset_proof()
	assert authority.proof.generation == proof_generation
	assert !authority.proof.accepting_plans
	after_rejected_reset := native_proof_snapshot(&authority)
	assert !authority.disarm_proof()
	native_proof_assert_snapshots_equal(after_rejected_reset, native_proof_snapshot(&authority))
	authority.advance_renderer_attempt(907, 919)!
	assert authority.renderer_attempt_token == 919
	rebound_ticket := native_lifetime_ticket_snapshot_for_test(&authority, ticket)!
	native_lifetime_ticket_assert_equal(overflow_ticket, rebound_ticket)
	assert rebound_ticket.authority_scope == .app_lifetime
	assert rebound_ticket.authority_token == 910
	assert authority.proof.trace_len == native_operation_trace_capacity
	assert authority.proof.trace_overflow
	native_proof_assert_trace_equal(prefix, authority.proof.trace)
	claim := authority.claim_lifetime_release(ticket, .egl_thread, authority.owner_thread_identity, 0) or {
		panic('trace-overflow lifetime claim was rejected')
	}
	native_assert_live_ticket_for_test(&authority, ticket, .egl_thread, .releasing,
		authority.owner_thread_identity, 0)
	raw := native_proof_call_primitive(native_valid_return_value, 1, 0, 0)
	result := authority.complete_lifetime_release(claim, raw, .none, .ready, '')
	assert result.succeeded()
	assert authority.lifetime_tickets.len == 0
	assert authority.proof.trace_len == native_operation_trace_capacity
	assert authority.proof.trace_overflow
	native_proof_assert_trace_equal(prefix, authority.proof.trace)
	assert native_lifetime_release_start_for_test(&authority, claim.context) == -1
	assert authority.disarm_proof()
}

fn test_native_proof_generation_exhaustion_is_sticky_and_never_wraps() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 929, 937)!
	authority.next_proof_generation = ~u64(0)
	assert authority.arm_proof()
	assert authority.proof.generation == ~u64(0)
	assert authority.next_proof_generation == 0
	assert !authority.reset_proof()
	assert authority.proof.generation == ~u64(0)
	assert authority.next_proof_generation == 0
	assert !authority.proof.accepting_plans
	assert authority.disarm_proof()
	assert authority.proof == unsafe { nil }
	assert !authority.arm_proof()
	assert authority.proof == unsafe { nil }
	assert authority.next_proof_generation == 0
}

fn test_native_plan_matches_every_context_field_exactly() {
	baseline := native_proof_baseline_context()
	cases := native_proof_context_mismatches(baseline)
	assert cases.len == 18
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
		baseline.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	injected := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value | native_valid_handle
		return_value: -37
		handle:       0xabc
	}
	actual := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value | native_valid_handle
		return_value: 1
		handle:       0xdef
	}
	for item in cases {
		assert authority.reset_proof()
		assert authority.arm(baseline, injected), item.label
		before_mismatch := native_proof_snapshot(&authority)
		mismatch_result := authority.inject(item.context, actual)
		assert native_proof_evidence_equal(mismatch_result, actual), item.label
		after_mismatch := native_proof_snapshot(&authority)
		native_proof_assert_snapshots_equal(before_mismatch, after_mismatch)
		exact_result := authority.inject(baseline, actual)
		assert native_proof_evidence_equal(exact_result, injected), item.label
		assert !authority.has_pending_native_plans(), item.label
		consumed := native_proof_snapshot(&authority)
		replay_result := authority.inject(baseline, actual)
		assert native_proof_evidence_equal(replay_result, actual), item.label
		native_proof_assert_snapshots_equal(consumed, native_proof_snapshot(&authority))
	}
}

fn test_native_plan_rejects_duplicate_and_capacity_overflow_without_mutation() {
	baseline := native_proof_baseline_context()
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
		baseline.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	evidence := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	}
	assert authority.arm(baseline, evidence)
	before_duplicate := native_proof_snapshot(&authority)
	assert !authority.arm(baseline, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -2
	})
	after_duplicate := native_proof_snapshot(&authority)
	native_proof_assert_snapshots_equal(before_duplicate, after_duplicate)

	_ = authority.inject(baseline, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	})
	assert authority.reset_proof()
	for index in 0 .. native_primitive_plan_capacity {
		assert authority.arm(NativeOperationContext{
			...baseline
			ordinal: u64(index + 1)
		}, evidence)
	}
	before_overflow := native_proof_snapshot(&authority)
	assert !authority.arm(NativeOperationContext{
		...baseline
		ordinal: u64(native_primitive_plan_capacity + 1)
	}, evidence)
	after_overflow := native_proof_snapshot(&authority)
	native_proof_assert_snapshots_equal(before_overflow, after_overflow)
	for index in 0 .. native_primitive_plan_capacity {
		_ = authority.inject(NativeOperationContext{
			...baseline
			ordinal: u64(index + 1)
		}, NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value
			return_value: 1
		})
	}
}

fn test_gg_native_plan_pair_rolls_back_duplicate_secondary_and_disarms() {
	$if gg_multiwindow ? {
		baseline := native_proof_baseline_context()
		primary_evidence := NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value
			return_value: 0
		}
		secondary_evidence := NativePrimitiveEvidence{
			valid_mask: native_valid_egl_error
			egl_error:  0x300b
		}
		mut authority := NativeOperationAuthority{}
		native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
			baseline.renderer_attempt_token)!
		before := native_proof_snapshot(&authority)
		assert !before.proof_armed
		assert before.registry.tickets.len == 0

		outcome := authority.arm_gg_native_plan_pair(baseline, primary_evidence, baseline,
			secondary_evidence)
		assert outcome == .secondary_rejected_rolled_back_disarmed
		after := native_proof_snapshot(&authority)
		assert authority.proof == unsafe { nil }
		assert !authority.has_pending_native_plans()
		assert authority.lifetime_tickets.len == 0
		assert after.app_identity == before.app_identity
		assert after.app_lifetime_token == before.app_lifetime_token
		assert after.renderer_attempt_token == before.renderer_attempt_token
		assert after.owner_thread_identity == before.owner_thread_identity
		assert after.next_ordinal == before.next_ordinal
		assert after.next_proof_generation == before.next_proof_generation + 1
		assert after.sequence_exhausted == before.sequence_exhausted
		assert after.terminal_cause == before.terminal_cause
		assert !after.proof_armed
		assert after.proof_generation == 0
		assert after.proof_ordinal_floor == 0
		assert !after.proof_accepting_plans
		assert after.trace_len == 0
		assert !after.trace_overflow
		native_lifetime_registry_assert_snapshots_equal(before.registry, after.registry)

		fresh_generation := authority.next_proof_generation
		assert authority.arm_proof()
		assert authority.proof != unsafe { nil }
		assert authority.proof.generation == fresh_generation
		assert authority.proof.ordinal_floor == authority.next_ordinal
		assert authority.proof.accepting_plans
		assert !authority.has_pending_native_plans()
		assert authority.lifetime_tickets.len == 0
		fresh := native_proof_snapshot(&authority)
		assert fresh.trace_len == 0
		assert !fresh.trace_overflow
		for entry in fresh.plan {
			assert !entry.armed
			assert !entry.listener_registration_pending
			assert entry.proof_generation == 0
			assert native_operation_contexts_identical(entry.context, NativeOperationContext{})
			assert native_proof_evidence_equal(entry.evidence, NativePrimitiveEvidence{})
		}
		native_proof_assert_invalid_trace_tail(fresh)
		assert authority.disarm_proof()
		final := native_proof_snapshot(&authority)
		assert authority.proof == unsafe { nil }
		assert !authority.has_pending_native_plans()
		assert authority.lifetime_tickets.len == 0
		assert final.app_identity == before.app_identity
		assert final.app_lifetime_token == before.app_lifetime_token
		assert final.renderer_attempt_token == before.renderer_attempt_token
		assert final.owner_thread_identity == before.owner_thread_identity
		assert final.next_ordinal == before.next_ordinal
		assert final.next_proof_generation == before.next_proof_generation + 2
		assert final.sequence_exhausted == before.sequence_exhausted
		assert final.terminal_cause == before.terminal_cause
		assert !final.proof_armed
		native_lifetime_registry_assert_snapshots_equal(before.registry, final.registry)
	}
}

fn test_native_listener_selector_and_exact_plan_collisions_are_symmetric_and_nonmutating() {
	selector := NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        73
		renderer_attempt_token: 73
		app_identity:           41
		presence_mask:          native_context_window_target_fields
		domain:                 .wayland
		operation:              .frame_callback
		call_site:              .window_finalize
		scope:                  .window_target
		window:                 WindowId{
			app_instance: 41
			slot:         3
			generation:   5
		}
		target_generation:      7
		batch_epoch:            11
		window_lease_epoch:     13
		target_lease_epoch:     17
		ordinal:                19
	}
	exact := NativeOperationContext{
		...selector
		presence_mask:   selector.presence_mask | native_context_has_target_identity
		target_identity: 0x1234
	}
	non_overlapping_selector := NativeOperationContext{
		...selector
		ordinal: selector.ordinal + 1
	}
	non_overlapping_exact := NativeOperationContext{
		...non_overlapping_selector
		presence_mask:   non_overlapping_selector.presence_mask | native_context_has_target_identity
		target_identity: 0x5678
	}
	failure := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value | native_valid_errno
		return_value: -1
		native_errno: 11
	}
	actual := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 0
	}
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, selector.app_identity,
		selector.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}

	// Pending selector first: the overlapping exact entry must not alter any slot.
	assert authority.queue_listener_registration_injection(selector, failure)
	before_exact_overlap := native_proof_snapshot(&authority)
	assert !authority.arm(exact, NativePrimitiveEvidence{
		...failure
		return_value: -2
	})
	after_exact_overlap := native_proof_snapshot(&authority)
	native_proof_assert_snapshots_equal(before_exact_overlap, after_exact_overlap)
	mut pending_count := 0
	mut armed_count := 0
	for entry in authority.proof.plan {
		if entry.listener_registration_pending {
			pending_count++
		}
		if entry.armed {
			armed_count++
		}
	}
	assert pending_count == 1
	assert armed_count == 0
	assert authority.proof.trace_len == 0
	assert !authority.proof.trace_overflow
	assert authority.arm(non_overlapping_exact, failure)
	assert authority.arm_listener_registration(exact)
	assert native_proof_evidence_equal(authority.inject(exact, actual), failure)
	assert native_proof_evidence_equal(authority.inject(non_overlapping_exact, actual), failure)
	for entry in authority.proof.plan {
		assert !entry.armed
		assert !entry.listener_registration_pending
	}
	assert authority.proof.trace_len == 0
	native_proof_assert_invalid_trace_tail(native_proof_snapshot(&authority))

	// Exact entry first: the overlapping pending selector has the same invariant.
	assert authority.arm(exact, failure)
	before_pending_overlap := native_proof_snapshot(&authority)
	assert !authority.queue_listener_registration_injection(selector, NativePrimitiveEvidence{
		...failure
		return_value: -3
	})
	after_pending_overlap := native_proof_snapshot(&authority)
	native_proof_assert_snapshots_equal(before_pending_overlap, after_pending_overlap)
	pending_count = 0
	armed_count = 0
	for entry in authority.proof.plan {
		if entry.listener_registration_pending {
			pending_count++
		}
		if entry.armed {
			armed_count++
		}
	}
	assert pending_count == 0
	assert armed_count == 1
	assert authority.proof.trace_len == 0
	assert !authority.proof.trace_overflow
	assert authority.queue_listener_registration_injection(non_overlapping_selector, failure)
	assert authority.arm_listener_registration(non_overlapping_exact)
	assert native_proof_evidence_equal(authority.inject(exact, actual), failure)
	assert native_proof_evidence_equal(authority.inject(non_overlapping_exact, actual), failure)
	for entry in authority.proof.plan {
		assert !entry.armed
		assert !entry.listener_registration_pending
	}
	assert authority.proof.trace_len == 0
	assert !authority.proof.trace_overflow
	native_proof_assert_invalid_trace_tail(native_proof_snapshot(&authority))
}

fn test_native_plan_is_app_window_generation_and_epoch_local() {
	baseline := native_proof_baseline_context()
	mut first := NativeOperationAuthority{}
	mut second := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut first, baseline.app_identity,
		baseline.renderer_attempt_token)!
	native_bind_renderer_authority_for_test(mut second, baseline.app_identity + 1,

		baseline.renderer_attempt_token + 1)!
	first.arm_proof()
	second.arm_proof()
	defer {
		first.disarm_proof()
		second.disarm_proof()
	}
	evidence := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -91
	}
	actual := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	}
	assert first.arm(baseline, evidence)
	foreign_app := NativeOperationContext{
		...baseline
		authority_token:        second.renderer_attempt_token
		app_identity:           second.app_identity
		renderer_attempt_token: second.renderer_attempt_token
	}
	assert native_proof_evidence_equal(first.inject(foreign_app, actual), actual)
	assert first.proof.plan[0].armed
	assert first.proof.trace_len == 0
	foreign_window := NativeOperationContext{
		...baseline
		window: WindowId{
			app_instance: baseline.window.app_instance
			slot:         baseline.window.slot + 1
			generation:   baseline.window.generation
		}
	}
	assert native_proof_evidence_equal(first.inject(foreign_window, actual), actual)
	assert first.proof.plan[0].armed
	assert first.proof.trace_len == 0
	for collision in [
		NativeOperationContext{
			...baseline
			window: WindowId{
				app_instance: baseline.window.app_instance
				slot:         baseline.window.slot
				generation:   baseline.window.generation + 1
			}
		},
		NativeOperationContext{
			...baseline
			target_generation: baseline.target_generation + 1
		},
		NativeOperationContext{
			...baseline
			batch_epoch: baseline.batch_epoch + 1
		},
		NativeOperationContext{
			...baseline
			window_lease_epoch: baseline.window_lease_epoch + 1
		},
		NativeOperationContext{
			...baseline
			target_lease_epoch: baseline.target_lease_epoch + 1
		},
	] {
		assert native_proof_evidence_equal(first.inject(collision, actual), actual)
		assert first.proof.plan[0].armed
		assert first.proof.trace_len == 0
	}
	assert native_proof_evidence_equal(first.inject(baseline, actual), evidence)
	assert !first.proof.plan[0].armed
	assert second.proof.trace_len == 0
	for entry in second.proof.plan {
		assert !entry.armed
	}
}

fn test_native_plan_does_not_consume_a_stale_renderer_attempt() {
	baseline := native_proof_baseline_context()
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
		baseline.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	assert authority.arm(baseline, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -41
	})
	before_rebind := native_proof_snapshot(&authority)
	mut rebind_error := ''
	authority.advance_renderer_attempt(baseline.app_identity, baseline.renderer_attempt_token + 1) or {
		rebind_error = err.msg()
	}
	assert rebind_error == err_render_native_renderer_unavailable
	native_proof_assert_snapshots_equal(before_rebind, native_proof_snapshot(&authority))
	current := NativeOperationContext{
		...baseline
		authority_token:        baseline.renderer_attempt_token + 1
		renderer_attempt_token: baseline.renderer_attempt_token + 1
	}
	actual := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	}
	assert native_proof_evidence_equal(authority.inject(current, actual), actual)
	assert authority.proof.plan[0].armed
	assert authority.proof.trace_len == 0
	assert native_proof_evidence_equal(authority.inject(baseline, actual), NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -41
	})
	authority.advance_renderer_attempt(baseline.app_identity, baseline.renderer_attempt_token + 1)!
	assert authority.reset_proof()
	for entry in authority.proof.plan {
		assert !entry.armed
	}
}

fn test_native_plan_isolated_between_real_apps_and_windows() {
	mut first := new_app(backend: .mock, queue_size: 2)!
	first_window := first.create_window(title: 'first native authority app')!
	mut second := new_app(backend: .mock, queue_size: 2)!
	second_window := second.create_window(title: 'second native authority app')!
	assert first.instance_id != second.instance_id
	assert first_window != second_window
	first.backend.native_operations.arm_proof()
	context := NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        first.backend.native_operations.renderer_attempt_token
		renderer_attempt_token: first.backend.native_operations.renderer_attempt_token
		app_identity:           first.instance_id
		presence_mask:          native_context_has_window | native_context_has_target_generation | native_context_has_target_identity | native_context_has_batch_epoch | native_context_has_window_lease_epoch | native_context_has_target_lease_epoch
		domain:                 .egl
		operation:              .make_current
		call_site:              .window_activate
		scope:                  .window_target
		window:                 first_window
		target_generation:      3
		target_identity:        5
		batch_epoch:            7
		window_lease_epoch:     11
		target_lease_epoch:     13
		ordinal:                17
	}
	assert first.backend.native_operations.arm(context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	})
	foreign := NativeOperationContext{
		...context
		authority_token:        second.backend.native_operations.renderer_attempt_token
		renderer_attempt_token: second.backend.native_operations.renderer_attempt_token
		app_identity:           second.instance_id
		window:                 second_window
	}
	actual := NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	}
	before := native_proof_snapshot(&first.backend.native_operations)
	assert native_proof_evidence_equal(first.backend.native_operations.inject(foreign, actual),
		actual)
	after := native_proof_snapshot(&first.backend.native_operations)
	native_proof_assert_snapshots_equal(before, after)
	_ = first.backend.native_operations.inject(context, actual)
	assert first.backend.native_operations.reset_proof()
	assert first.backend.native_operations.disarm_proof()
	first.stop()!
	second.stop()!
}

fn test_native_compound_ordinal_reservation_exhausts_without_call_or_consumption() {
	baseline := native_proof_baseline_context()
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
		baseline.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	authority.next_ordinal = ~u64(0) - 3
	mut first := authority.reserve_ordinals(2)!
	assert authority.next_ordinal == ~u64(0) - 1
	first_primary := first.materialize(&authority, .egl, .make_current, NativeOperationSeed{
		call_site: .window_activate
		scope:     .window_target
	})!
	first_ancillary := first.materialize(&authority, .egl, .egl_error_query, NativeOperationSeed{})!
	assert first_primary.ordinal == ~u64(0) - 3
	assert first_ancillary.ordinal == ~u64(0) - 2
	mut last := authority.reserve_ordinals(2)!
	assert authority.next_ordinal == 0
	last_primary := last.materialize(&authority, .egl, .make_current, NativeOperationSeed{})!
	last_ancillary := last.materialize(&authority, .egl, .egl_error_query, NativeOperationSeed{})!
	assert last_primary.ordinal == ~u64(0) - 1
	assert last_ancillary.ordinal == ~u64(0)
	assert last_primary.ordinal != 0
	assert last_ancillary.ordinal != 0
	assert last_primary.ordinal != first_primary.ordinal
	assert last_ancillary.ordinal != first_ancillary.ordinal

	unconsumed := NativeOperationContext{
		...baseline
		ordinal: 17
	}
	assert authority.arm(unconsumed, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	})
	C.v_multiwindow_test_native_primitive_reset()
	before := native_proof_snapshot(&authority)
	mut first_error := ''
	_ = authority.reserve_ordinals(1) or {
		first_error = err.msg()
		NativeOrdinalRange{}
	}
	assert first_error == err_render_native_renderer_unavailable
	assert authority.sequence_exhausted
	assert authority.terminal_cause == .sequence_exhausted
	assert authority.next_ordinal == 0
	assert authority.proof.plan[0].armed
	assert authority.proof.trace_len == 0
	assert C.v_multiwindow_test_native_primitive_call_count() == 0
	exhausted := native_proof_snapshot(&authority)
	mut replay_error := ''
	_ = authority.reserve_ordinals(1) or {
		replay_error = err.msg()
		NativeOrdinalRange{}
	}
	assert replay_error == first_error
	after := native_proof_snapshot(&authority)
	assert after.sequence_exhausted
	assert after.terminal_cause == .sequence_exhausted
	assert after.next_ordinal == 0
	assert after.proof_armed
	assert after.plan[0].armed
	assert after.trace_len == 0
	assert !after.trace_overflow
	assert C.v_multiwindow_test_native_primitive_call_count() == 0
	native_proof_assert_snapshots_equal(exhausted, after)
	native_proof_assert_plan_equal(before.plan, after.plan)
	native_proof_assert_trace_equal(before.trace, after.trace)
}

fn test_native_trace_separates_real_actual_effective_acceptance_health_and_release() {
	baseline := native_proof_baseline_context()
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
		baseline.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	mut ordinals := authority.reserve_ordinals(2)!
	primary_context := ordinals.materialize(&authority, .dxgi, .present, NativeOperationSeed{
		presence_mask:      baseline.presence_mask
		call_site:          baseline.call_site
		scope:              baseline.scope
		window:             baseline.window
		target_generation:  baseline.target_generation
		target_identity:    baseline.target_identity
		batch_epoch:        baseline.batch_epoch
		window_lease_epoch: baseline.window_lease_epoch
		target_lease_epoch: baseline.target_lease_epoch
	})!
	query_context := ordinals.materialize(&authority, .dxgi, .dxgi_removal_query, NativeOperationSeed{
		presence_mask:   native_context_has_target_identity
		call_site:       .window_finalize
		scope:           .window_target
		target_identity: 0x4444
	})!
	assert authority.arm(primary_context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: i64(u32(0x887a0001))
	})
	assert authority.arm(query_context, NativePrimitiveEvidence{
		valid_mask:          native_valid_dxgi_removal_reason
		dxgi_removal_reason: i64(u32(0x887a0007))
	})
	C.v_multiwindow_test_native_primitive_reset()
	primary_raw := native_proof_call_primitive(native_valid_return_value, 0, 0x1111, 0)
	primary := authority.capture_call(primary_context, primary_raw)
	query_raw := native_proof_call_primitive(native_valid_dxgi_removal_reason, 0, 0x2222, 0)
	query := authority.capture_evidence(query_context, query_raw)
	combined := native_capture_with_dxgi_removal(primary, query)
	result := authority.accept_dxgi(primary_context, combined, .none)
	authority.record_health_latch(primary_context, .lost)
	authority.record_release(primary_context, combined, result)
	assert C.v_multiwindow_test_native_primitive_call_count() == 2
	assert authority.proof.trace_len == 9
	assert !authority.proof.trace_overflow
	expected := [
		NativeOperationTraceMilestone.real_call,
		.actual_primitive,
		.effective_primitive,
		.real_call,
		.actual_primitive,
		.effective_primitive,
		.acceptance,
		.health_latched,
		.authority_release,
	]
	for index, milestone in expected {
		assert authority.proof.trace[index].milestone == milestone
		if index < 3 {
			assert native_operation_contexts_identical(authority.proof.trace[index].context,
				primary_context)
		} else if index < 6 {
			assert native_operation_contexts_identical(authority.proof.trace[index].context,
				query_context)
		} else {
			assert native_operation_contexts_identical(authority.proof.trace[index].context,
				primary_context)
		}
	}
	assert authority.proof.trace[1].actual.return_value == 0
	assert u32(authority.proof.trace[2].effective.return_value) == u32(0x887a0001)
	assert authority.proof.trace[4].actual.dxgi_removal_reason == 0
	assert u32(authority.proof.trace[5].effective.dxgi_removal_reason) == u32(0x887a0007)
	assert result.disposition == .renderer_lost
	assert authority.proof.trace[7].health == .lost
	assert !authority.proof.plan[0].armed
	assert !authority.proof.plan[1].armed
}

fn test_native_plan_consumption_alone_records_no_operation_milestone() {
	baseline := native_proof_baseline_context()
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, baseline.app_identity,
		baseline.renderer_attempt_token)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	assert authority.arm(baseline, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: -1
	})
	_ = authority.inject(baseline, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 1
	})
	assert !authority.proof.plan[0].armed
	assert authority.proof.trace_len == 0
	assert !authority.proof.trace_overflow
}

fn test_native_actual_fatal_evidence_cannot_be_downgraded() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 91, 101)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	for index, error_code in [i64(0x3003), i64(0x3006), i64(0x300e)] {
		authority.reset_proof()
		context := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        101
			renderer_attempt_token: 101
			app_identity:           91
			domain:                 .egl
			operation:              .make_current
			call_site:              .window_activate
			scope:                  .window_target
			ordinal:                u64(index + 1)
		}
		assert authority.arm(context, NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value | native_valid_egl_error
			return_value: 1
			egl_error:    0x3000
		})
		raw := native_proof_call_primitive(native_valid_return_value | native_valid_egl_error, 0,
			0, error_code)
		capture := authority.capture_call(context, raw)
		result := authority.accept_egl(context, capture, .null_output)
		expected := if error_code == 0x300e {
			NativeRenderDisposition.renderer_lost
		} else {
			NativeRenderDisposition.renderer_unavailable
		}
		assert result.disposition == expected
		assert result.native_code == error_code
		assert result.actual_primitive.egl_error == error_code
		assert result.primitive.return_value == 1
		assert result.local_validation == .null_output
	}
}

fn test_native_actual_dxgi_and_wayland_fatal_evidence_cannot_be_downgraded() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 191, 211)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	dxgi_context := NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        211
		renderer_attempt_token: 211
		app_identity:           191
		domain:                 .dxgi
		operation:              .present
		call_site:              .window_finalize
		scope:                  .window_target
		ordinal:                1
	}
	assert authority.arm(dxgi_context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 0
	})
	dxgi_raw := native_proof_call_primitive(native_valid_return_value, i64(u32(0x887a0005)), 0, 0)
	dxgi_capture := authority.capture_call(dxgi_context, dxgi_raw)
	dxgi_result := authority.accept_dxgi(dxgi_context, dxgi_capture, .none)
	assert dxgi_result.disposition == .renderer_lost
	assert u32(dxgi_result.native_code) == u32(0x887a0005)
	assert dxgi_result.primitive.return_value == 0

	authority.reset_proof()
	dxgi_operation_context := NativeOperationContext{
		...dxgi_context
		ordinal: 2
	}
	dxgi_query_context := NativeOperationContext{
		...dxgi_operation_context
		presence_mask:   native_context_has_target_identity
		operation:       .dxgi_removal_query
		target_identity: 0x4444
		ordinal:         3
	}
	assert authority.arm(dxgi_operation_context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value
		return_value: 0
	})
	assert authority.arm(dxgi_query_context, NativePrimitiveEvidence{
		valid_mask:          native_valid_dxgi_removal_reason
		dxgi_removal_reason: 0
	})
	dxgi_operation_raw := native_proof_call_primitive(native_valid_return_value,
		i64(u32(0x80004005)), 0, 0)
	dxgi_query_raw := native_proof_call_primitive(native_valid_dxgi_removal_reason, 0, 0,
		i64(u32(0x887a0007)))
	dxgi_operation_capture := authority.capture_call(dxgi_operation_context, dxgi_operation_raw)
	dxgi_query_capture := authority.capture_evidence(dxgi_query_context, dxgi_query_raw)
	dxgi_combined := native_capture_with_dxgi_removal(dxgi_operation_capture, dxgi_query_capture)
	dxgi_removal_result := authority.accept_dxgi(dxgi_operation_context, dxgi_combined, .none)
	assert dxgi_removal_result.disposition == .renderer_lost
	assert u32(dxgi_removal_result.removal_reason) == u32(0x887a0007)
	assert dxgi_removal_result.primitive.return_value == 0
	assert dxgi_removal_result.primitive.dxgi_removal_reason == 0

	authority.reset_proof()
	wayland_context := NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        211
		renderer_attempt_token: 211
		app_identity:           191
		domain:                 .wayland
		operation:              .window_surface_create
		call_site:              .window_prepare
		scope:                  .window_target
		ordinal:                4
	}
	assert authority.arm(wayland_context, NativePrimitiveEvidence{
		valid_mask:            native_valid_handle | native_valid_wayland_display_error
		handle:                1
		wayland_display_error: 0
	})
	wayland_raw := native_proof_call_primitive(native_valid_handle | native_valid_wayland_display_error,
		0, 0, 71)
	wayland_capture := authority.capture_call(wayland_context, wayland_raw)
	wayland_result := authority.accept_wayland(wayland_context, wayland_capture, .null_output,
		'wayland failure')
	assert wayland_result.disposition == .renderer_unavailable
	assert wayland_result.display_error == 71
	assert wayland_result.actual_primitive.wayland_display_error == 71
	assert wayland_result.primitive.handle == 1
}

fn test_wayland_window_surface_handle_classification() {
	context := NativeOperationContext{
		domain:    .wayland
		operation: .window_surface_create
		call_site: .window_prepare
		scope:     .window_target
	}
	valid_handle := NativePrimitiveEvidence{
		valid_mask: native_valid_handle
		handle:     0x1234
	}
	accepted := native_result_from_wayland(context, NativePrimitiveCapture{
		actual:    valid_handle
		effective: valid_handle
	}, .none, 'wayland failure')
	assert accepted.disposition == .ok

	null_handle := NativePrimitiveEvidence{
		valid_mask: native_valid_handle
	}
	rejected := native_result_from_wayland(context, NativePrimitiveCapture{
		actual:    null_handle
		effective: null_handle
	}, .none, 'wayland failure')
	assert rejected.disposition == .operation_failed

	display_failure := NativePrimitiveEvidence{
		valid_mask:            native_valid_handle | native_valid_wayland_display_error
		handle:                0x1234
		wayland_display_error: 71
	}
	fatal := native_result_from_wayland(context, NativePrimitiveCapture{
		actual:    display_failure
		effective: display_failure
	}, .void_completion, 'wayland failure')
	assert fatal.disposition == .renderer_unavailable
	assert fatal.scope == .renderer
	assert fatal.display_error == 71
}

fn test_native_trace_bound_reports_overflow_and_preserves_prefix() {
	mut authority := NativeOperationAuthority{}
	native_bind_renderer_authority_for_test(mut authority, 7, 9)!
	authority.arm_proof()
	defer {
		authority.disarm_proof()
	}
	for index in 0 .. native_operation_trace_capacity {
		authority.record_entry(NativeOperationTraceEntry{
			milestone: .real_call
			context:   NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        9
				renderer_attempt_token: 9
				app_identity:           7
				ordinal:                u64(index + 1)
			}
		})
	}
	assert authority.proof.trace_len == native_operation_trace_capacity
	assert !authority.proof.trace_overflow
	prefix := authority.proof.trace
	authority.record_entry(NativeOperationTraceEntry{
		milestone: .authority_release
		context:   NativeOperationContext{
			ordinal: 999
		}
	})
	assert authority.proof.trace_len == native_operation_trace_capacity
	assert authority.proof.trace_overflow
	native_proof_assert_trace_equal(prefix, authority.proof.trace)
}

fn test_native_proof_state_is_identical_after_replayed_public_stop() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	app.backend.native_operations.arm_proof()
	context := NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        app.backend.native_operations.renderer_attempt_token
		renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
		app_identity:           app.instance_id
		domain:                 .wayland
		operation:              .display_flush
		call_site:              .display_transport
		scope:                  .renderer
		ordinal:                app.backend.native_operations.next_ordinal
	}
	assert app.backend.native_operations.arm(context, NativePrimitiveEvidence{
		valid_mask:   native_valid_return_value | native_valid_errno
		return_value: -1
		native_errno: 11
	})
	app.stop()!
	first := native_proof_snapshot(&app.backend.native_operations)
	first_status := app.status()
	first_terminal := app.stop_terminal
	assert first.proof_armed
	assert first.plan[0].armed
	assert !first.trace_overflow
	app.stop()!
	second := native_proof_snapshot(&app.backend.native_operations)
	assert first_status == .stopped
	assert app.status() == .stopped
	assert app.stop_terminal == first_terminal
	native_proof_assert_invalid_trace_tail(first)
	native_proof_assert_snapshots_equal(first, second)
	app.backend.native_operations.disarm_proof()
}

fn native_proof_baseline_context() NativeOperationContext {
	return NativeOperationContext{
		authority_scope:        .renderer_attempt
		authority_token:        73
		renderer_attempt_token: 73
		app_identity:           41
		presence_mask:          native_context_has_window | native_context_has_target_generation | native_context_has_target_identity | native_context_has_batch_epoch | native_context_has_window_lease_epoch | native_context_has_target_lease_epoch
		domain:                 .egl
		operation:              .make_current
		call_site:              .window_activate
		scope:                  .window_target
		window:                 WindowId{
			app_instance: 41
			slot:         3
			generation:   5
		}
		target_generation:      7
		target_identity:        0x1234
		batch_epoch:            11
		window_lease_epoch:     13
		target_lease_epoch:     17
		ordinal:                19
	}
}

fn native_proof_context_mismatches(baseline NativeOperationContext) []NativeContextMismatchCase {
	return [
		NativeContextMismatchCase{
			label:   'authority_scope'
			context: NativeOperationContext{
				...baseline
				authority_scope: .app_lifetime
			}
		},
		NativeContextMismatchCase{
			label:   'authority_token'
			context: NativeOperationContext{
				...baseline
				authority_token: baseline.authority_token + 1
			}
		},
		NativeContextMismatchCase{
			label:   'presence_mask'
			context: NativeOperationContext{
				...baseline
				presence_mask: baseline.presence_mask & ~native_context_has_target_identity
			}
		},
		NativeContextMismatchCase{
			label:   'app_identity'
			context: NativeOperationContext{
				...baseline
				app_identity: baseline.app_identity + 1
			}
		},
		NativeContextMismatchCase{
			label:   'renderer_attempt_token'
			context: NativeOperationContext{
				...baseline
				renderer_attempt_token: baseline.renderer_attempt_token + 1
			}
		},
		NativeContextMismatchCase{
			label:   'domain'
			context: NativeOperationContext{
				...baseline
				domain: .dxgi
			}
		},
		NativeContextMismatchCase{
			label:   'operation'
			context: NativeOperationContext{
				...baseline
				operation: .swap_buffers
			}
		},
		NativeContextMismatchCase{
			label:   'call_site'
			context: NativeOperationContext{
				...baseline
				call_site: .window_finalize
			}
		},
		NativeContextMismatchCase{
			label:   'scope'
			context: NativeOperationContext{
				...baseline
				scope: .anchor
			}
		},
		NativeContextMismatchCase{
			label:   'window.app_instance'
			context: NativeOperationContext{
				...baseline
				window: WindowId{
					...baseline.window
					app_instance: baseline.window.app_instance + 1
				}
			}
		},
		NativeContextMismatchCase{
			label:   'window.slot'
			context: NativeOperationContext{
				...baseline
				window: WindowId{
					...baseline.window
					slot: baseline.window.slot + 1
				}
			}
		},
		NativeContextMismatchCase{
			label:   'window.generation'
			context: NativeOperationContext{
				...baseline
				window: WindowId{
					...baseline.window
					generation: baseline.window.generation + 1
				}
			}
		},
		NativeContextMismatchCase{
			label:   'target_generation'
			context: NativeOperationContext{
				...baseline
				target_generation: baseline.target_generation + 1
			}
		},
		NativeContextMismatchCase{
			label:   'target_identity'
			context: NativeOperationContext{
				...baseline
				target_identity: baseline.target_identity + 1
			}
		},
		NativeContextMismatchCase{
			label:   'batch_epoch'
			context: NativeOperationContext{
				...baseline
				batch_epoch: baseline.batch_epoch + 1
			}
		},
		NativeContextMismatchCase{
			label:   'window_lease_epoch'
			context: NativeOperationContext{
				...baseline
				window_lease_epoch: baseline.window_lease_epoch + 1
			}
		},
		NativeContextMismatchCase{
			label:   'target_lease_epoch'
			context: NativeOperationContext{
				...baseline
				target_lease_epoch: baseline.target_lease_epoch + 1
			}
		},
		NativeContextMismatchCase{
			label:   'ordinal'
			context: NativeOperationContext{
				...baseline
				ordinal: baseline.ordinal + 1
			}
		},
	]
}

fn native_reserve_lifetime_ticket_for_test(mut authority NativeOperationAuthority, release_kind NativeLifetimeReleaseKind, seed NativeOperationSeed) !(u64, NativeOperationContext) {
	return native_reserve_lifetime_ticket_for_scope_for_test(mut authority, .app_lifetime,
		release_kind, seed)
}

fn native_reserve_lifetime_ticket_for_scope_for_test(mut authority NativeOperationAuthority, authority_scope NativeOperationAuthorityScope, release_kind NativeLifetimeReleaseKind, seed NativeOperationSeed) !(u64, NativeOperationContext) {
	mut cleanup := authority.reserve_ordinals_for(authority_scope, 1)!
	context := cleanup.materialize(&authority, native_lifetime_release_domain(release_kind),
		native_lifetime_release_operation(release_kind), seed.without_target_identity())!
	ticket_id := authority.reserve_lifetime_ticket(context, release_kind,
		seed.without_target_identity())!
	return ticket_id, context
}

fn native_bind_renderer_authority_for_test(mut authority NativeOperationAuthority, app_identity u64, renderer_attempt_token u64) ! {
	if renderer_attempt_token <= 1 {
		return error('renderer attempt token must leave room for an app-lifetime token')
	}
	authority.bind_app_lifetime(app_identity, renderer_attempt_token - 1)!
	authority.advance_renderer_attempt(app_identity, renderer_attempt_token)!
}

fn native_lifetime_ticket_index_for_test(authority &NativeOperationAuthority, ticket_id u64) int {
	for index, ticket in authority.lifetime_tickets {
		if ticket.ticket_id == ticket_id {
			return index
		}
	}
	return -1
}

fn native_lifetime_ticket_snapshot_for_test(authority &NativeOperationAuthority, ticket_id u64) !NativeLifetimeTicketProofSnapshot {
	index := native_lifetime_ticket_index_for_test(authority, ticket_id)
	if index < 0 {
		return error('native lifetime ticket `${ticket_id}` is absent')
	}
	ticket := authority.lifetime_tickets[index]
	return NativeLifetimeTicketProofSnapshot{
		ticket_id:                ticket.ticket_id
		app_identity:             ticket.app_identity
		authority_scope:          ticket.authority_scope
		authority_token:          ticket.authority_token
		domain:                   ticket.domain
		release_kind:             ticket.release_kind
		owner_seed:               ticket.owner_seed
		proof_generation:         ticket.proof_generation
		context:                  ticket.context
		native_identity:          ticket.native_identity
		required_parent_identity: ticket.required_parent_identity
		parent_authority_scope:   ticket.parent_authority_scope
		parent_authority_token:   ticket.parent_authority_token
		state:                    ticket.state
	}
}

fn native_assert_bound_ticket_snapshot_for_test(ticket NativeLifetimeTicketProofSnapshot, release_kind NativeLifetimeReleaseKind, identity u64, parent_identity u64) {
	assert ticket.ticket_id != 0
	assert ticket.release_kind == release_kind
	assert ticket.domain == native_lifetime_release_domain(release_kind)
	assert ticket.native_identity == identity
	assert ticket.required_parent_identity == parent_identity
	assert ticket.state == .bound
	if release_kind in [.egl_surface, .egl_context] {
		assert parent_identity != 0
		assert ticket.parent_authority_scope in [.app_lifetime, .renderer_attempt]
		assert ticket.parent_authority_token != 0
	} else {
		assert ticket.parent_authority_scope == .none
		assert ticket.parent_authority_token == 0
	}
	assert ticket.context.ordinal == ticket.ticket_id
	assert ticket.context.authority_scope == ticket.authority_scope
	assert ticket.context.authority_token == ticket.authority_token
	assert ticket.context.renderer_attempt_token == if ticket.authority_scope == .renderer_attempt {
		ticket.authority_token
	} else {
		u64(0)
	}
	assert ticket.context.target_identity == identity
	assert (ticket.context.presence_mask & native_context_has_target_identity) != 0
}

fn native_assert_live_ticket_for_test(authority &NativeOperationAuthority, ticket_id u64, release_kind NativeLifetimeReleaseKind, state NativeLifetimeTicketState, identity u64, parent_identity u64) {
	index := native_lifetime_ticket_index_for_test(authority, ticket_id)
	assert index >= 0
	ticket := authority.lifetime_tickets[index]
	assert ticket.ticket_id == ticket_id
	assert ticket.app_identity == authority.app_identity
	assert authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
	assert ticket.domain == native_lifetime_release_domain(release_kind)
	assert ticket.release_kind == release_kind
	assert ticket.native_identity == identity
	assert ticket.required_parent_identity == parent_identity
	assert ticket.state == state
	assert ticket.context.ordinal == ticket_id
	assert ticket.context.app_identity == authority.app_identity
	assert ticket.context.authority_scope == ticket.authority_scope
	assert ticket.context.authority_token == ticket.authority_token
	assert ticket.context.renderer_attempt_token == if ticket.authority_scope == .renderer_attempt {
		ticket.authority_token
	} else {
		u64(0)
	}
	assert ticket.context.operation == native_lifetime_release_operation(release_kind)
	assert ticket.context.domain == native_lifetime_release_domain(release_kind)
	assert ticket.context.target_identity == identity
	assert (ticket.context.presence_mask & native_context_has_target_identity != 0) == (identity != 0)
}

fn native_lifetime_claim_succeeds_for_test(mut authority NativeOperationAuthority, ticket_id u64, release_kind NativeLifetimeReleaseKind, identity u64, parent_identity u64) bool {
	claim := authority.claim_lifetime_release(ticket_id, release_kind, identity, parent_identity) or {
		return false
	}
	_ = claim
	return true
}

fn native_lifetime_release_start_for_test(authority &NativeOperationAuthority, ticket_context NativeOperationContext) int {
	if authority.proof == unsafe { nil } {
		return -1
	}
	for index in 0 .. authority.proof.trace_len {
		entry := authority.proof.trace[index]
		if entry.milestone == .real_call
			&& native_operation_contexts_identical(entry.context, ticket_context) {
			return index
		}
	}
	return -1
}

fn native_assert_lifetime_release_sequence_for_test(authority &NativeOperationAuthority, ticket_context NativeOperationContext, after_index int) int {
	return native_assert_lifetime_release_sequence_in_snapshot_for_test(native_proof_snapshot(authority),
		ticket_context, after_index)
}

fn native_lifetime_release_start_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, ticket_context NativeOperationContext) int {
	for index in 0 .. snapshot.trace_len {
		entry := snapshot.trace[index]
		if entry.milestone == .real_call
			&& native_operation_contexts_identical(entry.context, ticket_context) {
			return index
		}
	}
	return -1
}

fn native_assert_lifetime_release_sequence_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, ticket_context NativeOperationContext, after_index int) int {
	start := native_lifetime_release_start_in_snapshot_for_test(snapshot, ticket_context)
	assert start >= after_index
	assert start + 5 < snapshot.trace_len
	expected := [
		NativeOperationTraceMilestone.real_call,
		.actual_primitive,
		.effective_primitive,
		.acceptance,
		.health_latched,
		.authority_release,
	]
	context := snapshot.trace[start].context
	assert context.ordinal == ticket_context.ordinal
	assert context.target_identity != 0
	for offset, milestone in expected {
		entry := snapshot.trace[start + offset]
		assert entry.milestone == milestone
		assert native_operation_contexts_identical(entry.context, context)
	}
	assert native_proof_evidence_equal(snapshot.trace[start + 1].actual,
		snapshot.trace[start + 2].effective)
	accepted := snapshot.trace[start + 3]
	expected_validation := if context.domain == .wayland {
		NativeLocalValidation.void_completion
	} else {
		NativeLocalValidation.none
	}
	assert accepted.local_validation == expected_validation
	assert accepted.result.succeeded()
	assert native_operation_contexts_identical(accepted.result.context, context)
	assert snapshot.trace[start + 4].health != .uninitialized
	assert native_proof_result_equal(snapshot.trace[start + 5].result, accepted.result)
	return start
}

fn native_lifetime_trace_entry_count_for_test(snapshot NativeAuthorityProofSnapshot, ticket_context NativeOperationContext) int {
	mut count := 0
	for index in 0 .. snapshot.trace_len {
		entry := snapshot.trace[index]
		if native_operation_contexts_identical(entry.context, ticket_context) {
			count++
		}
	}
	return count
}

fn native_proof_call_primitive(valid_mask u64, return_value i64, handle u64, native_code i64) C.VMultiwindowNativePrimitive {
	mut raw := C.VMultiwindowNativePrimitive{}
	C.v_multiwindow_test_native_primitive(valid_mask, return_value, handle, native_code, 3, 5, &raw)
	return raw
}

fn native_proof_snapshot(authority &NativeOperationAuthority) NativeAuthorityProofSnapshot {
	if authority.proof == unsafe { nil } {
		return NativeAuthorityProofSnapshot{
			app_identity:           authority.app_identity
			app_lifetime_token:     authority.app_lifetime_token
			renderer_attempt_token: authority.renderer_attempt_token
			owner_thread_identity:  authority.owner_thread_identity
			next_ordinal:           authority.next_ordinal
			next_proof_generation:  authority.next_proof_generation
			sequence_exhausted:     authority.sequence_exhausted
			terminal_cause:         authority.terminal_cause
			registry:               native_lifetime_registry_snapshot(authority)
		}
	}
	return NativeAuthorityProofSnapshot{
		app_identity:           authority.app_identity
		app_lifetime_token:     authority.app_lifetime_token
		renderer_attempt_token: authority.renderer_attempt_token
		owner_thread_identity:  authority.owner_thread_identity
		next_ordinal:           authority.next_ordinal
		next_proof_generation:  authority.next_proof_generation
		sequence_exhausted:     authority.sequence_exhausted
		terminal_cause:         authority.terminal_cause
		proof_armed:            true
		proof_generation:       authority.proof.generation
		proof_ordinal_floor:    authority.proof.ordinal_floor
		proof_accepting_plans:  authority.proof.accepting_plans
		plan:                   authority.proof.plan
		trace:                  authority.proof.trace
		trace_len:              authority.proof.trace_len
		trace_overflow:         authority.proof.trace_overflow
		registry:               native_lifetime_registry_snapshot(authority)
	}
}

fn native_lifetime_registry_snapshot(authority &NativeOperationAuthority) NativeLifetimeRegistryProofSnapshot {
	mut tickets := []NativeLifetimeTicketProofSnapshot{cap: authority.lifetime_tickets.len}
	for ticket in authority.lifetime_tickets {
		tickets << NativeLifetimeTicketProofSnapshot{
			ticket_id:                ticket.ticket_id
			app_identity:             ticket.app_identity
			authority_scope:          ticket.authority_scope
			authority_token:          ticket.authority_token
			domain:                   ticket.domain
			release_kind:             ticket.release_kind
			owner_seed:               ticket.owner_seed
			proof_generation:         ticket.proof_generation
			context:                  ticket.context
			native_identity:          ticket.native_identity
			required_parent_identity: ticket.required_parent_identity
			parent_authority_scope:   ticket.parent_authority_scope
			parent_authority_token:   ticket.parent_authority_token
			state:                    ticket.state
		}
	}
	return NativeLifetimeRegistryProofSnapshot{
		app_identity:           authority.app_identity
		app_lifetime_token:     authority.app_lifetime_token
		renderer_attempt_token: authority.renderer_attempt_token
		owner_thread_identity:  authority.owner_thread_identity
		tickets:                tickets
	}
}

fn native_proof_assert_snapshots_equal(expected NativeAuthorityProofSnapshot, actual NativeAuthorityProofSnapshot) {
	assert actual.app_identity == expected.app_identity
	assert actual.app_lifetime_token == expected.app_lifetime_token
	assert actual.renderer_attempt_token == expected.renderer_attempt_token
	assert actual.owner_thread_identity == expected.owner_thread_identity
	assert actual.next_ordinal == expected.next_ordinal
	assert actual.next_proof_generation == expected.next_proof_generation
	assert actual.sequence_exhausted == expected.sequence_exhausted
	assert actual.terminal_cause == expected.terminal_cause
	assert actual.proof_armed == expected.proof_armed
	assert actual.proof_generation == expected.proof_generation
	assert actual.proof_ordinal_floor == expected.proof_ordinal_floor
	assert actual.proof_accepting_plans == expected.proof_accepting_plans
	assert actual.trace_len == expected.trace_len
	assert actual.trace_overflow == expected.trace_overflow
	native_proof_assert_plan_equal(expected.plan, actual.plan)
	native_proof_assert_trace_equal(expected.trace, actual.trace)
	native_lifetime_registry_assert_snapshots_equal(expected.registry, actual.registry)
}

fn native_proof_assert_plan_equal(expected [native_primitive_plan_capacity]NativePrimitivePlanEntry, actual [native_primitive_plan_capacity]NativePrimitivePlanEntry) {
	for index in 0 .. native_primitive_plan_capacity {
		assert actual[index].armed == expected[index].armed
		assert actual[index].listener_registration_pending == expected[index].listener_registration_pending
		assert actual[index].proof_generation == expected[index].proof_generation
		assert native_operation_contexts_identical(actual[index].context, expected[index].context)
		assert native_proof_evidence_equal(actual[index].evidence, expected[index].evidence)
	}
}

fn native_lifetime_registry_assert_snapshots_equal(expected NativeLifetimeRegistryProofSnapshot, actual NativeLifetimeRegistryProofSnapshot) {
	assert actual.app_identity == expected.app_identity
	assert actual.app_lifetime_token == expected.app_lifetime_token
	assert actual.renderer_attempt_token == expected.renderer_attempt_token
	assert actual.owner_thread_identity == expected.owner_thread_identity
	assert actual.tickets.len == expected.tickets.len
	for index in 0 .. expected.tickets.len {
		native_lifetime_ticket_assert_equal(expected.tickets[index], actual.tickets[index])
	}
}

fn native_lifetime_ticket_assert_equal(expected NativeLifetimeTicketProofSnapshot, actual NativeLifetimeTicketProofSnapshot) {
	assert actual.ticket_id == expected.ticket_id
	assert actual.app_identity == expected.app_identity
	assert actual.authority_scope == expected.authority_scope
	assert actual.authority_token == expected.authority_token
	assert actual.domain == expected.domain
	assert actual.release_kind == expected.release_kind
	assert native_operation_seeds_identical(actual.owner_seed, expected.owner_seed)
	assert actual.proof_generation == expected.proof_generation
	assert native_operation_contexts_identical(actual.context, expected.context)
	assert actual.native_identity == expected.native_identity
	assert actual.required_parent_identity == expected.required_parent_identity
	assert actual.parent_authority_scope == expected.parent_authority_scope
	assert actual.parent_authority_token == expected.parent_authority_token
	assert actual.state == expected.state
}

fn native_operation_seeds_identical(left NativeOperationSeed, right NativeOperationSeed) bool {
	return left.presence_mask == right.presence_mask && left.call_site == right.call_site
		&& left.scope == right.scope && left.window == right.window
		&& left.target_generation == right.target_generation
		&& left.target_identity == right.target_identity && left.batch_epoch == right.batch_epoch
		&& left.window_lease_epoch == right.window_lease_epoch
		&& left.target_lease_epoch == right.target_lease_epoch
}

fn native_proof_assert_trace_equal(expected [native_operation_trace_capacity]NativeOperationTraceEntry, actual [native_operation_trace_capacity]NativeOperationTraceEntry) {
	for index in 0 .. native_operation_trace_capacity {
		assert actual[index].milestone == expected[index].milestone
		assert native_operation_contexts_identical(actual[index].context, expected[index].context)
		assert native_proof_evidence_equal(actual[index].actual, expected[index].actual)
		assert native_proof_evidence_equal(actual[index].effective, expected[index].effective)
		assert actual[index].local_validation == expected[index].local_validation
		assert native_proof_result_equal(actual[index].result, expected[index].result)
		assert actual[index].health == expected[index].health
	}
}

fn native_proof_assert_invalid_trace_tail(snapshot NativeAuthorityProofSnapshot) {
	assert snapshot.trace_len >= 0
	assert snapshot.trace_len <= native_operation_trace_capacity
	for index in snapshot.trace_len .. native_operation_trace_capacity {
		entry := snapshot.trace[index]
		assert entry.milestone == .real_call
		assert native_operation_contexts_identical(entry.context, NativeOperationContext{})
		assert native_proof_evidence_equal(entry.actual, NativePrimitiveEvidence{})
		assert native_proof_evidence_equal(entry.effective, NativePrimitiveEvidence{})
		assert entry.local_validation == .none
		assert native_proof_result_equal(entry.result, NativeRenderResult{})
		assert entry.health == .uninitialized
	}
}

fn native_proof_result_equal(actual NativeRenderResult, expected NativeRenderResult) bool {
	return actual.domain == expected.domain && actual.operation == expected.operation
		&& actual.scope == expected.scope && actual.disposition == expected.disposition
		&& actual.native_code == expected.native_code
		&& actual.removal_reason == expected.removal_reason
		&& actual.native_status == expected.native_status
		&& actual.display_error == expected.display_error
		&& actual.current_draw_surface == expected.current_draw_surface
		&& actual.current_read_surface == expected.current_read_surface
		&& actual.current_context == expected.current_context
		&& actual.error_text == expected.error_text
		&& native_operation_contexts_identical(actual.context, expected.context)
		&& native_proof_evidence_equal(actual.actual_primitive, expected.actual_primitive)
		&& native_proof_evidence_equal(actual.primitive, expected.primitive)
		&& actual.local_validation == expected.local_validation
}

fn native_proof_evidence_equal(actual NativePrimitiveEvidence, expected NativePrimitiveEvidence) bool {
	return actual.valid_mask == expected.valid_mask && actual.return_value == expected.return_value
		&& actual.handle == expected.handle && actual.egl_error == expected.egl_error
		&& actual.native_errno == expected.native_errno
		&& actual.wayland_display_error == expected.wayland_display_error
		&& actual.dxgi_removal_reason == expected.dxgi_removal_reason
		&& actual.observed_count == expected.observed_count
		&& actual.observed_flags == expected.observed_flags
		&& actual.selected_value == expected.selected_value
		&& actual.object_identity_0 == expected.object_identity_0
		&& actual.object_identity_1 == expected.object_identity_1
		&& actual.object_identity_2 == expected.object_identity_2
}

fn test_native_egl_prepass_bad_surface_keeps_serials_dirty_and_replaces_target() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_egl_exercise_bad_surface(false)!
	}
}

fn test_native_egl_postcommit_bad_surface_consumes_only_frame_and_replaces_target() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_egl_exercise_bad_surface(true)!
	}
}

fn test_native_egl_bad_native_window_is_window_local_and_clears_target() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_egl_exercise_bad_native_window()!
	}
}

fn test_native_egl_context_lost_forbids_later_operations_and_uses_lifetime_releases() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_egl_exercise_terminal_failure(0x300e)!
	}
}

fn test_native_egl_bad_display_logically_abandons_without_native_release_calls() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_egl_bad_display_subprocess_for_test()!
	}
}

fn test_native_egl_bad_current_surface_queries_previous_binding_and_recovers_once() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_egl_exercise_bad_current_surface()!
	}
}

fn test_native_wayland_flush_and_poll_use_fresh_errno_and_display_evidence() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_transport_evidence()!
	}
}

fn test_native_wayland_fatal_transport_uses_local_proxy_anchor_release() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_fatal_transport_cleanup()!
	}
}

fn test_native_linux_egl_failed_start_closes_all_native_ownership_and_replays() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_linux_egl_exercise_failed_start_cleanup()!
	}
}

fn test_native_wayland_prepare_and_read_failures_are_balanced_and_retry_once() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_prepare_failure_and_retry()!
		native_wayland_exercise_read_failure_and_retry()!
	}
}

fn test_native_wayland_bad_poll_revents_latch_loss_after_balanced_cancel() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_bad_poll_revents()!
	}
}

fn test_native_wayland_listener_failure_arms_after_exact_callback_identity_exists() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_listener_failure()!
	}
}

fn test_native_wayland_normal_frame_callback_retires_only_after_post_destroy_completion() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_normal_frame_callback_retirement()!
	}
}

fn test_native_wayland_frame_callback_registry_completion_follows_native_destroy() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_frame_callback_trampoline_post_destroy_completion()!
	}
}

fn test_native_wayland_stop_releases_callback_before_egl_and_wayland_children() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_pending_callback_child_first_stop()!
	}
}

fn test_native_wayland_display_error_outranks_null_egl_window_and_forbids_fallback() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_null_egl_window_global_loss()!
	}
}

fn test_native_wayland_private_anchor_acquisitions_rollback_and_retry() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_wayland_runtime_requested_for_test() {
			return
		}
		native_wayland_exercise_private_anchor_acquisition_boundaries()!
	}
}

fn test_native_dxgi_null_success_and_direct_losses_forbid_fallback_and_release_outputs() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_dxgi_runtime_requested_for_test() {
			return
		}
		native_dxgi_exercise_startup_failures()!
	}
}

fn test_native_dxgi_present_captures_operation_and_removal_before_release() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_dxgi_runtime_requested_for_test() {
			return
		}
		native_dxgi_exercise_present_failures()!
	}
}

fn test_native_dxgi_resize_physically_calls_after_view_release_and_propagates_failure() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_dxgi_runtime_requested_for_test() {
			return
		}
		native_dxgi_exercise_physical_resize_failure_for_test()!
	}
}

fn test_native_sokol_swapchain_attachment_matches_backend_authority() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_runtime_proofs_requested_for_test() {
			return
		}
		native_exercise_sokol_swapchain_identity_for_test()!
	}
}

fn test_native_ordinal_exhaustion_at_real_boundary_is_terminal_and_replay_safe() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_runtime_proofs_requested_for_test() {
			return
		}
		native_exercise_real_boundary_ordinal_exhaustion()!
	}
}

fn test_native_phase_a_trace_overflow_does_not_block_lifetime_cleanup_or_replay() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !native_egl_runtime_requested_for_test() {
			return
		}
		native_exercise_phase_a_trace_overflow_cleanup()!
	}
}

fn test_native_win32_phase_b_com_lifetimes_are_bijective_and_child_first() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if windows {
			$if sokol_d3d11 ? {
				if !native_dxgi_runtime_requested_for_test() {
					return
				}
				native_win32_exercise_phase_b_lifetimes()!
			}
		}
	}
}

fn test_native_win32_phase_b_alias_null_burn_rejection_and_wrong_thread() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if windows {
			$if sokol_d3d11 ? {
				native_win32_exercise_phase_b_registry_edges()!
			}
		}
	}
}

fn test_native_win32_phase_b_exhaustion_overflow_and_stop_replay() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if windows {
			$if sokol_d3d11 ? {
				if !native_dxgi_runtime_requested_for_test() {
					return
				}
				native_win32_exercise_phase_b_exhaustion_and_overflow()!
			}
		}
	}
}

fn test_native_appkit_phase_b_lifetimes_are_bijective_and_child_first() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			if !native_appkit_runtime_requested_for_test() {
				return
			}
			native_appkit_exercise_phase_b_lifetimes()!
		}
	}
}

fn test_native_appkit_phase_b_null_burn_rejection_and_wrong_thread() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			if !native_appkit_runtime_requested_for_test() {
				return
			}
			native_appkit_exercise_phase_b_registry_edges()!
		}
	}
}

fn test_native_appkit_phase_b_exhaustion_overflow_and_stop_replay() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			if !native_appkit_runtime_requested_for_test() {
				return
			}
			native_appkit_exercise_phase_b_exhaustion_and_overflow()!
		}
	}
}

fn test_native_appkit_physical_nil_window_drawable_is_transient_and_retryable() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			if !native_appkit_runtime_requested_for_test() {
				return
			}
			native_appkit_exercise_physical_nil_window_for_test()!
		}
	}
}

fn test_native_appkit_physical_nil_anchor_drawable_is_transient_and_retryable() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			if !native_appkit_runtime_requested_for_test() {
				return
			}
			native_appkit_exercise_physical_nil_anchor_for_test()!
		}
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	@[heap]
	struct NativeEglFrameProof {
	mut:
		lease                    RenderTargetLease
		retry_lease              RenderTargetLease
		pass_snapshot            RenderWindowSnapshot
		retry_snapshot           RenderWindowSnapshot
		failure_context          NativeOperationContext
		failed_target            NativeEglTargetProof
		pre_failure_tickets      []NativeLifetimeTicketProofSnapshot
		replacement              NativeEglTargetProof
		failed_ticket            NativeLifetimeTicketProofSnapshot
		replacement_ticket       NativeLifetimeTicketProofSnapshot
		platform_ticket          NativeLifetimeTicketProofSnapshot
		platform_ticket_id       u64
		renderer_token           u64
		activation_start         u64
		activation_end           u64
		finalize_end             u64
		callback_setup_end       u64
		platform_target_identity u64
		platform_parent_identity u64
	}

	struct NativeEglTargetProof {
		generation       u64
		identity         u64
		ticket_id        u64
		native_destroyed bool
		frame_ready      bool
	}

	@[heap]
	struct NativeEglHealthyTargetState {
	mut:
		target NativeEglTargetProof
		ticket NativeLifetimeTicketProofSnapshot
	}

	@[heap]
	struct NativeEglHealthyBatchState {
	mut:
		lease    RenderTargetLease
		snapshot RenderWindowSnapshot
	}

	@[heap]
	struct NativeEglSokolPhaseState {
	mut:
		candidate_present  bool
		acquisition_status RenderAcquireStatus
	}

	@[heap]
	struct NativeOrdinalExhaustionState {
	mut:
		armed_context            NativeOperationContext
		boundary_release_oracle  NativeReleaseOracleSnapshot
		boundary_ownership       NativePhaseABackendOwnershipSnapshot
		pre_window               NativePhaseAWindowOwnershipSnapshot
		post_window              NativePhaseAWindowOwnershipSnapshot
		post_acquisition_tickets []NativeLifetimeTicketProofSnapshot
		setup_prefix             NativeAuthorityProofSnapshot
		lease                    RenderTargetLease
		target                   NativeEglTargetProof
	}

	struct NativeEglBindingProof {
		binding       EglBindingIdentity
		anchor        NativeEglTargetProof
		context       u64
		recovery_used bool
		health        NativeRendererHealth
	}

	struct NativeDxgiFrameProof {
	mut:
		context            NativeOperationContext
		query              NativeOperationContext
		lease              RenderTargetLease
		pass_snapshot      RenderWindowSnapshot
		identities         []u64
		view_identities    []u64
		swapchain_identity u64
	}

	struct NativeWaylandErrnoCase {
		native_errno i64
		disposition  NativeRenderDisposition
	}

	struct NativeWaylandBufferProof {
		buffers               []u64
		current               u64
		egl_window            u64
		egl_window_ticket     u64
		frame_callback        u64
		frame_callback_ticket u64
	}

	fn native_phase_a_backend_ownership_snapshot(app &App) NativePhaseABackendOwnershipSnapshot {
		registry := native_lifetime_registry_snapshot(&app.backend.native_operations)
		match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					mut windows := []NativePhaseAWindowOwnershipSnapshot{cap: app.backend.x11.windows.len}
					for record in app.backend.x11.windows {
						windows << NativePhaseAWindowOwnershipSnapshot{
							id:                       record.id
							native_window_identity:   u64(record.window)
							native_aux_identity_0:    u64(record.colormap)
							native_aux_identity_1:    native_identity(record.xic)
							native_aux_identity_2:    u64(record.cursor)
							config:                   record.config
							cursor_shape:             record.cursor_shape
							width:                    record.width
							height:                   record.height
							mouse_x:                  record.mouse_x
							mouse_y:                  record.mouse_y
							mouse_dx:                 record.mouse_dx
							mouse_dy:                 record.mouse_dy
							mouse_pos_valid:          record.mouse_pos_valid
							mouse_buttons:            record.mouse_buttons
							key_repeat:               record.key_repeat
							window_state:             record.window_state
							render_target_generation: record.render_target_generation
							native_destroyed:         record.native_destroyed
							egl_surface:              native_identity(record.egl_surface)
							egl_surface_ticket:       record.egl_surface_ticket
						}
					}
					binding := app.backend.x11.egl_binding
					return NativePhaseABackendOwnershipSnapshot{
						kind:                          .x11
						app_status:                    app.status()
						stop_terminal:                 app.stop_terminal
						started:                       app.backend.x11.started
						render_health:                 app.backend.x11.render_health
						render_sequence:               app.backend.x11.render_sequence
						native_display:                u64(usize(app.backend.x11.display))
						egl_display:                   native_identity(app.backend.x11.egl_display)
						egl_config:                    native_identity(app.backend.x11.egl_config)
						egl_context:                   native_identity(app.backend.x11.egl_context)
						anchor_surface:                native_identity(app.backend.x11.anchor_surface)
						egl_display_ticket:            app.backend.x11.egl_display_ticket
						egl_context_ticket:            app.backend.x11.egl_context_ticket
						egl_thread_ticket:             app.backend.x11.egl_thread_ticket
						anchor_surface_ticket:         app.backend.x11.anchor_surface_ticket
						anchor_generation:             app.backend.x11.anchor_generation
						binding_kind:                  binding.kind
						binding_window:                binding.window
						binding_target_generation:     binding.target_generation
						binding_surface:               native_identity(binding.surface)
						egl_bad_current_recovery_used: app.backend.x11.egl_bad_current_recovery_used
						native_visual_id:              app.backend.x11.native_visual_id
						native_operations_identity:    u64(usize(app.backend.x11.native_operations))
						x11_screen:                    app.backend.x11.screen
						x11_xdnd_version:              i64(app.backend.x11.xdnd_version)
						x11_keycodes:                  app.backend.x11.keycodes
						backend_owned_identities:      [
							u64(usize(app.backend.x11.display)),
							u64(app.backend.x11.root),
							u64(app.backend.x11.wm_protocols),
							u64(app.backend.x11.wm_delete_window),
							u64(app.backend.x11.wm_state),
							u64(app.backend.x11.xdnd_aware),
							u64(app.backend.x11.xdnd_enter),
							u64(app.backend.x11.xdnd_position),
							u64(app.backend.x11.xdnd_status),
							u64(app.backend.x11.xdnd_action_copy),
							u64(app.backend.x11.xdnd_drop),
							u64(app.backend.x11.xdnd_leave),
							u64(app.backend.x11.xdnd_finished),
							u64(app.backend.x11.xdnd_selection),
							u64(app.backend.x11.xdnd_type_list),
							u64(app.backend.x11.text_uri_list),
							u64(app.backend.x11.xdnd_source),
							u64(app.backend.x11.xdnd_target),
							u64(app.backend.x11.xdnd_format),
							native_identity(app.backend.x11.xim),
							native_identity(app.backend.x11.egl_display),
							native_identity(app.backend.x11.egl_config),
							native_identity(app.backend.x11.egl_context),
							native_identity(app.backend.x11.anchor_surface),
						]
						backend_state_words:           [u64(app.backend.x11.screen),
							u64(app.backend.x11.xdnd_version), app.backend.x11.anchor_generation,
							app.backend.x11.render_sequence, u64(app.backend.x11.native_visual_id),
							u64(app.backend.x11.windows.len)]
						windows:                       windows
						registry:                      registry
						authority:                     native_proof_snapshot(&app.backend.native_operations)
					}
				} $else {
					return NativePhaseABackendOwnershipSnapshot{
						kind:          .x11
						app_status:    app.status()
						stop_terminal: app.stop_terminal
						registry:      registry
						authority:     native_proof_snapshot(&app.backend.native_operations)
					}
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					mut windows := []NativePhaseAWindowOwnershipSnapshot{cap: app.backend.wayland.windows.len}
					for record in app.backend.wayland.windows {
						mut fallback_buffers := [3]u64{}
						for index, buffer in record.fallback_buffers {
							if index < fallback_buffers.len {
								fallback_buffers[index] = native_identity(buffer)
							}
						}
						mut pending_events := []NativePhaseAQueuedEventSnapshot{cap: record.pending_events.len}
						for pending in record.pending_events {
							pending_events << NativePhaseAQueuedEventSnapshot{
								sequence: pending.sequence
								event:    pending.event
							}
						}
						windows << NativePhaseAWindowOwnershipSnapshot{
							id:                             record.id
							native_operations_identity:     u64(usize(record.native_operations))
							owner_identity:                 u64(usize(record.owner))
							native_window_identity:         native_identity(record.surface)
							native_aux_identity_0:          native_identity(record.xdg_surface)
							native_aux_identity_1:          native_identity(record.xdg_toplevel)
							native_aux_identity_2:          native_identity(record.toplevel_decoration)
							resizable:                      record.resizable
							width:                          record.width
							height:                         record.height
							min_width:                      record.min_width
							min_height:                     record.min_height
							pending_toplevel_width:         record.pending_toplevel_width
							pending_toplevel_height:        record.pending_toplevel_height
							configured:                     record.configured
							pending_egl_resize:             record.pending_egl_resize
							pending_events:                 pending_events
							mouse_x:                        record.mouse_x
							mouse_y:                        record.mouse_y
							mouse_dx:                       record.mouse_dx
							mouse_dy:                       record.mouse_dy
							mouse_pos_valid:                record.mouse_pos_valid
							render_target_generation:       record.render_target_generation
							native_destroyed:               record.native_destroyed
							egl_surface:                    native_identity(record.egl_surface)
							egl_surface_ticket:             record.egl_surface_ticket
							wl_egl_window:                  native_identity(record.wl_egl_window)
							wl_egl_window_ticket:           record.wl_egl_window_ticket
							frame_callback:                 native_identity(record.frame_callback)
							frame_callback_ticket:          record.frame_callback_ticket
							frame_ready:                    record.frame_ready
							fallback_buffer_count:          record.fallback_buffers.len
							fallback_buffers:               fallback_buffers
							fallback_current_buffer:        native_identity(record.fallback_current_buffer)
							fallback_buffer_width:          record.fallback_buffer_width
							fallback_buffer_height:         record.fallback_buffer_height
							toplevel_decoration_configured: record.toplevel_decoration_configured
							toplevel_decoration_mode:       record.toplevel_decoration_mode
							user_action_serial:             record.user_action_serial
							user_action_poll:               record.user_action_poll
							user_action_serial_valid:       record.user_action_serial_valid
						}
					}
					mut touches := []NativePhaseATouchSnapshot{cap: app.backend.wayland.touches.len}
					for touch in app.backend.wayland.touches {
						touches << NativePhaseATouchSnapshot{
							active:    touch.active
							id:        touch.id
							window_id: touch.window_id
							x:         touch.x
							y:         touch.y
						}
					}
					binding := app.backend.wayland.egl_binding
					return NativePhaseABackendOwnershipSnapshot{
						kind:                          .wayland
						app_status:                    app.status()
						stop_terminal:                 app.stop_terminal
						started:                       app.backend.wayland.started
						render_health:                 app.backend.wayland.render_health
						render_sequence:               app.backend.wayland.render_sequence
						native_display:                native_identity(app.backend.wayland.display)
						egl_display:                   native_identity(app.backend.wayland.egl_display)
						egl_config:                    native_identity(app.backend.wayland.egl_config)
						egl_context:                   native_identity(app.backend.wayland.egl_context)
						anchor_surface:                native_identity(app.backend.wayland.anchor_surface)
						anchor_wl_egl_window:          native_identity(app.backend.wayland.anchor_wl_egl_window)
						anchor_wl_surface:             native_identity(app.backend.wayland.anchor_wl_surface)
						wayland_compositor:            native_identity(app.backend.wayland.compositor)
						egl_display_ticket:            app.backend.wayland.egl_display_ticket
						egl_context_ticket:            app.backend.wayland.egl_context_ticket
						egl_thread_ticket:             app.backend.wayland.egl_thread_ticket
						anchor_surface_ticket:         app.backend.wayland.anchor_surface_ticket
						anchor_wl_egl_window_ticket:   app.backend.wayland.anchor_wl_egl_window_ticket
						anchor_wl_surface_ticket:      app.backend.wayland.anchor_wl_surface_ticket
						anchor_generation:             app.backend.wayland.anchor_generation
						binding_kind:                  binding.kind
						binding_window:                binding.window
						binding_target_generation:     binding.target_generation
						binding_surface:               native_identity(binding.surface)
						egl_bad_current_recovery_used: app.backend.wayland.egl_bad_current_recovery_used
						poll_generation:               app.backend.wayland.poll_generation
						poll_error:                    app.backend.wayland.poll_error
						event_sequence_terminal:       app.backend.wayland.event_sequence_terminal
						wayland_display_unavailable:   app.backend.wayland.wayland_display_unavailable
						wayland_display_error:         app.backend.wayland.wayland_display_error
						native_operations_identity:    u64(usize(app.backend.wayland.native_operations))
						data_offer_has_uri_list:       app.backend.wayland.data_offer_has_uri_list
						data_offer_source_actions:     app.backend.wayland.data_offer_source_actions
						data_offer_selected_action:    app.backend.wayland.data_offer_selected_action
						data_offer_action_received:    app.backend.wayland.data_offer_action_received
						data_offer_window:             app.backend.wayland.data_offer_window
						data_offer_window_valid:       app.backend.wayland.data_offer_window_valid
						pending_drop_fd:               app.backend.wayland.pending_drop_fd
						pending_drop_window:           app.backend.wayland.pending_drop_window
						pending_drop_window_valid:     app.backend.wayland.pending_drop_window_valid
						pending_drop_source_actions:   app.backend.wayland.pending_drop_source_actions
						pending_drop_selected_action:  app.backend.wayland.pending_drop_selected_action
						pending_drop_action_received:  app.backend.wayland.pending_drop_action_received
						pending_drop_poll_cycles:      app.backend.wayland.pending_drop_poll_cycles
						pending_drop_buffer:           app.backend.wayland.pending_drop_buffer.clone()
						pointer_focus:                 app.backend.wayland.pointer_focus
						pointer_focused:               app.backend.wayland.pointer_focused
						pointer_enter_serial:          app.backend.wayland.pointer_enter_serial
						pointer_enter_serial_valid:    app.backend.wayland.pointer_enter_serial_valid
						keyboard_focus:                app.backend.wayland.keyboard_focus
						keyboard_focused:              app.backend.wayland.keyboard_focused
						keyboard_repeat_rate:          app.backend.wayland.keyboard_repeat_rate
						keyboard_repeat_delay:         app.backend.wayland.keyboard_repeat_delay
						keyboard_repeat_active:        app.backend.wayland.keyboard_repeat_active
						keyboard_repeat_raw_key:       app.backend.wayland.keyboard_repeat_raw_key
						keyboard_repeat_key_code:      app.backend.wayland.keyboard_repeat_key_code
						keyboard_repeat_window:        app.backend.wayland.keyboard_repeat_window
						keyboard_repeat_next_ns:       app.backend.wayland.keyboard_repeat_next_ns
						keyboard_repeat_interval_ns:   app.backend.wayland.keyboard_repeat_interval_ns
						pointer_buttons:               app.backend.wayland.pointer_buttons
						modifiers:                     app.backend.wayland.modifiers
						keys_down:                     app.backend.wayland.keys_down
						touches:                       touches
						backend_owned_identities:      [
							native_identity(app.backend.wayland.display),
							native_identity(app.backend.wayland.registry),
							native_identity(app.backend.wayland.compositor),
							native_identity(app.backend.wayland.seat),
							native_identity(app.backend.wayland.pointer),
							native_identity(app.backend.wayland.cursor_shape_manager),
							native_identity(app.backend.wayland.cursor_shape_device),
							native_identity(app.backend.wayland.keyboard),
							native_identity(app.backend.wayland.touch),
							native_identity(app.backend.wayland.data_device_manager),
							native_identity(app.backend.wayland.data_device),
							native_identity(app.backend.wayland.shm),
							native_identity(app.backend.wayland.data_offer),
							native_identity(app.backend.wayland.pending_drop_offer),
							native_identity(app.backend.wayland.wm_base),
							native_identity(app.backend.wayland.decoration_manager),
							native_identity(app.backend.wayland.xkb_context),
							native_identity(app.backend.wayland.xkb_keymap),
							native_identity(app.backend.wayland.xkb_state),
							native_identity(app.backend.wayland.egl_display),
							native_identity(app.backend.wayland.egl_config),
							native_identity(app.backend.wayland.egl_context),
							native_identity(app.backend.wayland.anchor_surface),
							native_identity(app.backend.wayland.anchor_wl_egl_window),
							native_identity(app.backend.wayland.anchor_wl_surface),
						]
						backend_state_words:           [
							u64(app.backend.wayland.compositor_name),
							u64(app.backend.wayland.compositor_version),
							u64(app.backend.wayland.seat_name),
							u64(app.backend.wayland.cursor_shape_manager_name),
							u64(app.backend.wayland.data_device_manager_name),
							u64(app.backend.wayland.shm_name),
							u64(app.backend.wayland.wm_base_name),
							u64(app.backend.wayland.decoration_manager_name),
							app.backend.wayland.anchor_generation,
							app.backend.wayland.render_sequence,
							app.backend.wayland.poll_generation,
							u64(app.backend.wayland.windows.len),
						]
						windows:                       windows
						registry:                      registry
						authority:                     native_proof_snapshot(&app.backend.native_operations)
					}
				} $else {
					return NativePhaseABackendOwnershipSnapshot{
						kind:          .wayland
						app_status:    app.status()
						stop_terminal: app.stop_terminal
						registry:      registry
						authority:     native_proof_snapshot(&app.backend.native_operations)
					}
				}
			}
			else {
				return NativePhaseABackendOwnershipSnapshot{
					kind:          app.backend.kind
					app_status:    app.status()
					stop_terminal: app.stop_terminal
					registry:      registry
					authority:     native_proof_snapshot(&app.backend.native_operations)
				}
			}
		}
	}

	fn native_phase_a_backend_assert_durable_ownership_equal(expected NativePhaseABackendOwnershipSnapshot, actual NativePhaseABackendOwnershipSnapshot) {
		assert actual.kind == expected.kind
		assert actual.started == expected.started
		assert actual.render_sequence == expected.render_sequence
		assert actual.native_display == expected.native_display
		assert actual.egl_display == expected.egl_display
		assert actual.egl_config == expected.egl_config
		assert actual.egl_context == expected.egl_context
		assert actual.anchor_surface == expected.anchor_surface
		assert actual.anchor_wl_egl_window == expected.anchor_wl_egl_window
		assert actual.anchor_wl_surface == expected.anchor_wl_surface
		assert actual.wayland_compositor == expected.wayland_compositor
		assert actual.egl_display_ticket == expected.egl_display_ticket
		assert actual.egl_context_ticket == expected.egl_context_ticket
		assert actual.egl_thread_ticket == expected.egl_thread_ticket
		assert actual.anchor_surface_ticket == expected.anchor_surface_ticket
		assert actual.anchor_wl_egl_window_ticket == expected.anchor_wl_egl_window_ticket
		assert actual.anchor_wl_surface_ticket == expected.anchor_wl_surface_ticket
		assert actual.anchor_generation == expected.anchor_generation
		assert actual.binding_kind == expected.binding_kind
		assert actual.binding_window == expected.binding_window
		assert actual.binding_target_generation == expected.binding_target_generation
		assert actual.binding_surface == expected.binding_surface
		assert actual.egl_bad_current_recovery_used == expected.egl_bad_current_recovery_used
		assert actual.native_visual_id == expected.native_visual_id
		assert actual.poll_generation == expected.poll_generation
		assert actual.poll_error == expected.poll_error
		assert actual.event_sequence_terminal == expected.event_sequence_terminal
		assert actual.wayland_display_unavailable == expected.wayland_display_unavailable
		assert actual.wayland_display_error == expected.wayland_display_error
		assert actual.native_operations_identity == expected.native_operations_identity
		assert actual.x11_screen == expected.x11_screen
		assert actual.x11_xdnd_version == expected.x11_xdnd_version
		assert actual.x11_keycodes == expected.x11_keycodes
		assert actual.data_offer_has_uri_list == expected.data_offer_has_uri_list
		assert actual.data_offer_source_actions == expected.data_offer_source_actions
		assert actual.data_offer_selected_action == expected.data_offer_selected_action
		assert actual.data_offer_action_received == expected.data_offer_action_received
		assert actual.data_offer_window == expected.data_offer_window
		assert actual.data_offer_window_valid == expected.data_offer_window_valid
		assert actual.pending_drop_fd == expected.pending_drop_fd
		assert actual.pending_drop_window == expected.pending_drop_window
		assert actual.pending_drop_window_valid == expected.pending_drop_window_valid
		assert actual.pending_drop_source_actions == expected.pending_drop_source_actions
		assert actual.pending_drop_selected_action == expected.pending_drop_selected_action
		assert actual.pending_drop_action_received == expected.pending_drop_action_received
		assert actual.pending_drop_poll_cycles == expected.pending_drop_poll_cycles
		assert actual.pending_drop_buffer == expected.pending_drop_buffer
		assert actual.pointer_focus == expected.pointer_focus
		assert actual.pointer_focused == expected.pointer_focused
		assert actual.pointer_enter_serial == expected.pointer_enter_serial
		assert actual.pointer_enter_serial_valid == expected.pointer_enter_serial_valid
		assert actual.keyboard_focus == expected.keyboard_focus
		assert actual.keyboard_focused == expected.keyboard_focused
		assert actual.keyboard_repeat_rate == expected.keyboard_repeat_rate
		assert actual.keyboard_repeat_delay == expected.keyboard_repeat_delay
		assert actual.keyboard_repeat_active == expected.keyboard_repeat_active
		assert actual.keyboard_repeat_raw_key == expected.keyboard_repeat_raw_key
		assert actual.keyboard_repeat_key_code == expected.keyboard_repeat_key_code
		assert actual.keyboard_repeat_window == expected.keyboard_repeat_window
		assert actual.keyboard_repeat_next_ns == expected.keyboard_repeat_next_ns
		assert actual.keyboard_repeat_interval_ns == expected.keyboard_repeat_interval_ns
		assert actual.pointer_buttons == expected.pointer_buttons
		assert actual.modifiers == expected.modifiers
		assert actual.keys_down == expected.keys_down
		assert actual.touches == expected.touches
		assert actual.backend_owned_identities == expected.backend_owned_identities
		assert actual.backend_state_words == expected.backend_state_words
		assert actual.windows.len == expected.windows.len
		for index in 0 .. expected.windows.len {
			assert actual.windows[index] == expected.windows[index]
		}
		native_lifetime_registry_assert_snapshots_equal(expected.registry, actual.registry)
	}

	fn native_phase_a_backend_assert_snapshots_equal(expected NativePhaseABackendOwnershipSnapshot, actual NativePhaseABackendOwnershipSnapshot) {
		assert actual.app_status == expected.app_status
		assert actual.stop_terminal == expected.stop_terminal
		assert actual.render_health == expected.render_health
		native_phase_a_backend_assert_durable_ownership_equal(expected, actual)
		native_proof_assert_snapshots_equal(expected.authority, actual.authority)
	}

	fn native_phase_a_assert_canonical_ticket_slots_for_test(app &App) []NativeLifetimeTicketProofSnapshot {
		return native_phase_a_assert_ticket_slots_for_test(app, false)
	}

	fn native_phase_a_assert_display_loss_ticket_slots_for_test(app &App) []NativeLifetimeTicketProofSnapshot {
		return native_phase_a_assert_ticket_slots_for_test(app, true)
	}

	fn native_phase_a_assert_ticket_slots_for_test(app &App, display_loss bool) []NativeLifetimeTicketProofSnapshot {
		snapshot := native_phase_a_backend_ownership_snapshot(app)
		assert snapshot.egl_display != 0
		assert snapshot.egl_context != 0
		assert snapshot.anchor_surface != 0
		if snapshot.kind == .wayland {
			assert snapshot.anchor_wl_egl_window != 0
			assert snapshot.anchor_wl_surface != 0
			assert snapshot.wayland_compositor != 0
		}
		native_phase_a_assert_ticket_slot_for_test(snapshot, snapshot.egl_display_ticket,
			.egl_display, snapshot.egl_display, 0, .app_lifetime, .none, display_loss)
		native_phase_a_assert_ticket_slot_for_test(snapshot, snapshot.egl_context_ticket,
			.egl_context, snapshot.egl_context, snapshot.egl_display, .app_lifetime, .app_lifetime,
			display_loss)
		native_phase_a_assert_ticket_slot_for_test(snapshot, snapshot.egl_thread_ticket,
			.egl_thread, snapshot.registry.owner_thread_identity, 0, .app_lifetime, .none, false)
		native_phase_a_assert_ticket_slot_for_test(snapshot, snapshot.anchor_surface_ticket,
			.egl_surface, snapshot.anchor_surface, snapshot.egl_display, .renderer_attempt,
			.app_lifetime, display_loss)
		if snapshot.kind == .wayland {
			native_phase_a_assert_ticket_slot_for_test(snapshot,
				snapshot.anchor_wl_egl_window_ticket, .wayland_egl_window,
				snapshot.anchor_wl_egl_window, snapshot.anchor_wl_surface, .renderer_attempt,
				.renderer_attempt, false)
			native_phase_a_assert_ticket_slot_for_test(snapshot, snapshot.anchor_wl_surface_ticket,
				.wayland_surface, snapshot.anchor_wl_surface, snapshot.wayland_compositor,
				.renderer_attempt, .none, false)
			native_assert_private_wayland_anchor_trace_for_test(snapshot.authority)
		}
		for window in snapshot.windows {
			assert (window.egl_surface_ticket == 0) == (window.egl_surface == 0)
			if window.egl_surface_ticket != 0 {
				native_phase_a_assert_ticket_slot_for_test(snapshot, window.egl_surface_ticket,
					.egl_surface, window.egl_surface, snapshot.egl_display, .app_lifetime,
					.app_lifetime, display_loss)
			}
			assert (window.wl_egl_window_ticket == 0) == (window.wl_egl_window == 0)
			if window.wl_egl_window_ticket != 0 {
				native_phase_a_assert_ticket_slot_for_test(snapshot, window.wl_egl_window_ticket,
					.wayland_egl_window, window.wl_egl_window, window.native_window_identity,
					.app_lifetime, .none, false)
			}
			assert (window.frame_callback_ticket == 0) == (window.frame_callback == 0)
			if window.frame_callback_ticket != 0 {
				native_phase_a_assert_ticket_slot_for_test(snapshot, window.frame_callback_ticket,
					.wayland_frame_callback, window.frame_callback, window.native_window_identity,
					.renderer_attempt, .none, false)
			}
		}
		mut slot_ids := []u64{}
		for ticket_id in [snapshot.egl_display_ticket, snapshot.egl_context_ticket, snapshot.egl_thread_ticket,
			snapshot.anchor_surface_ticket, snapshot.anchor_wl_egl_window_ticket,
			snapshot.anchor_wl_surface_ticket] {
			if ticket_id != 0 {
				assert ticket_id !in slot_ids
				slot_ids << ticket_id
			}
		}
		for window in snapshot.windows {
			for ticket_id in [window.egl_surface_ticket, window.wl_egl_window_ticket,
				window.frame_callback_ticket] {
				if ticket_id != 0 {
					assert ticket_id !in slot_ids
					slot_ids << ticket_id
				}
			}
		}
		assert snapshot.registry.tickets.len == slot_ids.len
		for ticket in snapshot.registry.tickets {
			assert ticket.ticket_id in slot_ids
			assert ticket.app_identity == app.instance_id
			assert ticket.authority_token == app.backend.native_operations.authority_token(ticket.authority_scope)
		}
		return snapshot.registry.tickets.clone()
	}

	fn native_assert_private_wayland_anchor_trace_for_test(snapshot NativeAuthorityProofSnapshot) {
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.context.scope != .anchor {
				continue
			}
			assert entry.context.window == WindowId{}
			assert entry.context.presence_mask & native_context_has_window == 0
			assert entry.context.operation !in [.swap_buffers, .frame_callback, .display_flush,
				.window_configure]
		}
	}

	enum NativeWaylandAnchorBoundary {
		wl_surface
		wl_egl_window
		egl_surface
	}

	fn native_wayland_anchor_app_for_test() !&App {
		mut app := native_phase_b_new_app_without_renderer_for_test(.wayland)!
		$if linux && sokol_wayland ? {
			app.backend.wayland.init_renderer()!
			assert app.backend.wayland.renderer_ready()
			assert app.backend.wayland.renderer_anchor_lifetime_absent()
			assert app.backend.wayland.anchor_generation != 0
			return app
		} $else {
			app.stop() or {}
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_anchor_selector_for_test(app &App, boundary NativeWaylandAnchorBoundary, base_ordinal u64) NativeOperationContext {
		operation_offset := match boundary {
			.wl_surface { u64(0) }
			.wl_egl_window { u64(2) }
			.egl_surface { u64(4) }
		}

		domain := if boundary == .egl_surface {
			NativeRenderDomain.egl
		} else {
			NativeRenderDomain.wayland
		}
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          native_context_has_target_generation
			domain:                 domain
			operation:              .anchor_surface_create
			call_site:              .anchor_create
			scope:                  .anchor
			target_generation:      app.backend.wayland.anchor_generation
			ordinal:                base_ordinal + operation_offset
		}
	}

	fn native_wayland_arm_anchor_boundary_for_test(mut app App, boundary NativeWaylandAnchorBoundary, base_ordinal u64) !NativeOperationContext {
		selector := native_wayland_anchor_selector_for_test(app, boundary, base_ordinal)
		failure := NativePrimitiveEvidence{
			valid_mask: native_valid_handle
			handle:     0
		}
		if boundary == .wl_surface {
			exact := NativeOperationContext{
				...selector
				presence_mask:   selector.presence_mask | native_context_has_target_identity
				target_identity: native_identity(app.backend.wayland.compositor)
			}
			if !app.backend.native_operations.arm(exact, failure) {
				return error('could not arm the exact Wayland wl_surface anchor acquisition')
			}
			return exact
		}
		if !app.backend.native_operations.arm_anchor_acquisition_for_test(selector, failure) {
			return error('could not arm the dynamic Wayland anchor acquisition `${boundary}`')
		}
		return selector
	}

	fn native_assert_wayland_anchor_primary_for_test(snapshot NativeAuthorityProofSnapshot, start int, selector NativeOperationContext, display_identity u64, injected bool) u64 {
		assert start >= 0
		assert start + 7 < snapshot.trace_len
		primary := snapshot.trace[start].context
		if selector.target_identity == 0 {
			assert native_anchor_acquisition_selector_matches(selector, primary)
		} else {
			assert native_operation_contexts_identical(selector, primary)
		}
		for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
			.effective_primitive] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, primary)
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		assert actual.has(native_valid_handle)
		assert actual.handle != 0
		assert effective.has(native_valid_handle)
		assert effective.handle == if injected {
			u64(0)
		} else {
			actual.handle
		}
		query := native_wayland_display_error_context_for_test_from_context(primary,
			display_identity)
		for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
			.effective_primitive] {
			entry := snapshot.trace[start + 3 + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, query)
		}
		for evidence in [snapshot.trace[start + 4].actual, snapshot.trace[start + 5].effective] {
			assert evidence.has(native_valid_wayland_display_error)
			assert evidence.wayland_display_error == 0
		}
		acceptance := snapshot.trace[start + 6]
		assert acceptance.milestone == .acceptance
		assert native_operation_contexts_identical(acceptance.context, primary)
		assert acceptance.local_validation == if injected {
			NativeLocalValidation.null_output
		} else {
			NativeLocalValidation.none
		}
		assert acceptance.result.disposition == if injected {
			NativeRenderDisposition.renderer_unavailable
		} else {
			NativeRenderDisposition.ok
		}
		health := snapshot.trace[start + 7]
		assert health.milestone == .health_latched
		assert native_operation_contexts_identical(health.context, primary)
		assert health.health == if injected {
			NativeRendererHealth.unavailable
		} else {
			NativeRendererHealth.ready
		}
		return actual.handle
	}

	fn native_wayland_display_error_context_for_test_from_context(primary NativeOperationContext, display_identity u64) NativeOperationContext {
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        primary.authority_token
			renderer_attempt_token: primary.renderer_attempt_token
			app_identity:           primary.app_identity
			presence_mask:          native_context_has_target_identity
			domain:                 .wayland
			operation:              .wayland_display_error_query
			call_site:              .display_transport
			scope:                  .renderer
			target_identity:        display_identity
			ordinal:                primary.ordinal + 1
		}
	}

	fn native_assert_egl_anchor_primary_for_test(snapshot NativeAuthorityProofSnapshot, start int, selector NativeOperationContext, injected bool) u64 {
		required_len := if injected { 8 } else { 5 }
		assert start >= 0
		assert start + required_len <= snapshot.trace_len
		primary := snapshot.trace[start].context
		if selector.target_identity == 0 {
			assert native_anchor_acquisition_selector_matches(selector, primary)
		} else {
			assert native_operation_contexts_identical(selector, primary)
		}
		for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
			.effective_primitive] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, primary)
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		assert actual.has(native_valid_handle)
		assert actual.handle != 0
		assert effective.has(native_valid_handle)
		assert effective.handle == if injected {
			u64(0)
		} else {
			actual.handle
		}
		acceptance_index := if injected { start + 6 } else { start + 3 }
		if injected {
			query := native_egl_error_context_for_test(primary)
			for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
				.effective_primitive] {
				entry := snapshot.trace[start + 3 + offset]
				assert entry.milestone == milestone
				assert native_operation_contexts_identical(entry.context, query)
			}
			for evidence in [snapshot.trace[start + 4].actual, snapshot.trace[start + 5].effective] {
				assert evidence.has(native_valid_egl_error)
				assert evidence.egl_error == 0x3000
			}
		}
		acceptance := snapshot.trace[acceptance_index]
		assert acceptance.milestone == .acceptance
		assert native_operation_contexts_identical(acceptance.context, primary)
		assert acceptance.local_validation == .none
		assert acceptance.result.disposition == if injected {
			NativeRenderDisposition.renderer_unavailable
		} else {
			NativeRenderDisposition.ok
		}
		health := snapshot.trace[acceptance_index + 1]
		assert health.milestone == .health_latched
		assert native_operation_contexts_identical(health.context, primary)
		assert health.health == if injected {
			NativeRendererHealth.unavailable
		} else {
			NativeRendererHealth.ready
		}
		return actual.handle
	}

	fn native_assert_anchor_rollback_release_for_test(snapshot NativeAuthorityProofSnapshot, start int, domain NativeRenderDomain, ticket_id u64, target_generation u64, identity u64, expected_mode u64) int {
		assert start >= 0
		assert start + 5 < snapshot.trace_len
		context := snapshot.trace[start].context
		assert context.authority_scope == .renderer_attempt
		assert context.renderer_attempt_token == context.authority_token
		assert context.app_identity == snapshot.app_identity
		assert context.presence_mask == (native_context_has_target_generation | native_context_has_target_identity)
		assert context.domain == domain
		assert context.operation == .surface_destroy
		assert context.call_site == .anchor_create
		assert context.scope == .anchor
		assert context.window == WindowId{}
		assert context.target_generation == target_generation
		assert context.target_identity == identity
		assert context.ordinal == ticket_id
		expected := [NativeOperationTraceMilestone.real_call, .actual_primitive, .effective_primitive,
			.acceptance, .health_latched, .authority_release]
		for offset, milestone in expected {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		actual := snapshot.trace[start + 1].actual
		assert native_proof_evidence_equal(actual, snapshot.trace[start + 2].effective)
		if expected_mode == 0 {
			assert !actual.has(native_valid_observed_flags)
		} else {
			assert actual.has(native_valid_observed_flags)
			assert actual.observed_flags == expected_mode
		}
		assert snapshot.trace[start + 3].result.disposition == .ok
		assert snapshot.trace[start + 4].health == .unavailable
		assert snapshot.trace[start + 5].actual == actual
		return start + 6
	}

	fn native_assert_wayland_anchor_failure_oracle_for_test(snapshot NativeReleaseOracleSnapshot, boundary NativeWaylandAnchorBoundary, egl_surface u64, wl_egl_window u64, wl_surface u64, egl_display u64) {
		assert !snapshot.overflow
		assert snapshot.callback_completions.len == 0
		assert snapshot.unticketed_releases.len == 0
		expected_count := match boundary {
			.wl_surface { 1 }
			.wl_egl_window { 2 }
			.egl_surface { 3 }
		}

		assert snapshot.records.len == expected_count
		mut index := 0
		if boundary == .egl_surface {
			record := snapshot.records[index]
			assert record.sequence == u64(index + 1)
			assert record.domain == .egl
			assert record.kind == 1
			assert record.identity == egl_surface
			assert record.parent_identity == egl_display
			assert record.mode == 0
			index++
		}
		if boundary in [.wl_egl_window, .egl_surface] {
			record := snapshot.records[index]
			assert record.sequence == u64(index + 1)
			assert record.domain == .wayland
			assert record.kind == 1
			assert record.identity == wl_egl_window
			assert record.parent_identity == 0
			assert record.mode == 0
			index++
		}
		record := snapshot.records[index]
		assert record.sequence == u64(index + 1)
		assert record.domain == .wayland
		assert record.kind == 6
		assert record.identity == wl_surface
		assert record.parent_identity == 0
		assert record.mode == wayland_anchor_release_protocol_destroy
		index++
		assert index == snapshot.records.len
	}

	fn native_assert_wayland_anchor_failure_for_test(app &App, boundary NativeWaylandAnchorBoundary, selector NativeOperationContext, base_ordinal u64, baseline NativeLifetimeRegistryProofSnapshot, release_oracle NativeReleaseOracleSnapshot) {
		snapshot := native_proof_snapshot(&app.backend.native_operations)
		assert !snapshot.trace_overflow
		assert !app.backend.native_operations.has_pending_native_plans()
		assert app.backend.wayland.render_health == .unavailable
		$if linux && sokol_wayland ? {
			assert app.backend.wayland.renderer_anchor_lifetime_absent()
		}
		native_lifetime_registry_assert_snapshots_equal(baseline, snapshot.registry)
		display_identity := native_identity(app.backend.wayland.display)
		egl_display_identity := native_identity(app.backend.wayland.egl_display)
		generation := app.backend.wayland.anchor_generation
		wl_surface := native_assert_wayland_anchor_primary_for_test(snapshot, 0, native_wayland_anchor_selector_for_test(app,
			.wl_surface, base_ordinal), display_identity, boundary == .wl_surface)
		mut wl_egl_window := u64(0)
		mut egl_surface := u64(0)
		mut release_start := 8
		if boundary != .wl_surface {
			wl_egl_window = native_assert_wayland_anchor_primary_for_test(snapshot, 8, native_wayland_anchor_selector_for_test(app,
				.wl_egl_window, base_ordinal), display_identity, boundary == .wl_egl_window)
			release_start = 16
		}
		if boundary == .egl_surface {
			egl_surface = native_assert_egl_anchor_primary_for_test(snapshot, 16, native_wayland_anchor_selector_for_test(app,
				.egl_surface, base_ordinal), true)
			release_start = 24
		}
		if selector.target_identity == 0 {
			injection_index := if boundary == .wl_egl_window { 8 } else { 16 }
			assert native_anchor_acquisition_selector_matches(selector,
				snapshot.trace[injection_index].context)
		}
		mut index := release_start
		if boundary == .egl_surface {
			index = native_assert_anchor_rollback_release_for_test(snapshot, index, .egl,

				base_ordinal + 6, generation, egl_surface, 0)
		}
		if boundary in [.wl_egl_window, .egl_surface] {
			index = native_assert_anchor_rollback_release_for_test(snapshot, index, .wayland,

				base_ordinal + 7, generation, wl_egl_window, 0)
		}
		index = native_assert_anchor_rollback_release_for_test(snapshot, index, .wayland,

			base_ordinal + 8, generation, wl_surface, wayland_anchor_release_protocol_destroy)
		assert index == snapshot.trace_len
		native_assert_wayland_anchor_failure_oracle_for_test(release_oracle, boundary, egl_surface,
			wl_egl_window, wl_surface, egl_display_identity)
		native_assert_complete_native_trace_for_test(snapshot)
	}

	fn native_assert_wayland_anchor_ticket_for_test(ticket NativeLifetimeTicketProofSnapshot, app &App, ticket_id u64, kind NativeLifetimeReleaseKind, identity u64, parent_identity u64, parent_scope NativeOperationAuthorityScope, generation u64) {
		assert ticket.ticket_id == ticket_id
		assert ticket.app_identity == app.instance_id
		assert ticket.authority_scope == .renderer_attempt
		assert ticket.authority_token == app.backend.native_operations.renderer_attempt_token
		assert ticket.release_kind == kind
		assert ticket.domain == native_lifetime_release_domain(kind)
		assert ticket.native_identity == identity
		assert ticket.required_parent_identity == parent_identity
		assert ticket.parent_authority_scope == parent_scope
		assert ticket.parent_authority_token == if parent_scope == .app_lifetime {
			app.backend.native_operations.app_lifetime_token
		} else if parent_scope == .renderer_attempt {
			app.backend.native_operations.renderer_attempt_token
		} else {
			u64(0)
		}
		assert ticket.owner_seed.presence_mask == native_context_has_target_generation
		assert ticket.owner_seed.call_site == .anchor_create
		assert ticket.owner_seed.scope == .anchor
		assert ticket.owner_seed.target_generation == generation
		assert ticket.owner_seed.target_identity == 0
		assert ticket.context.target_identity == identity
		assert ticket.context.target_generation == generation
		assert ticket.state == .bound
	}

	fn native_assert_egl_anchor_binding_suffix_for_test(snapshot NativeAuthorityProofSnapshot, start int, expected_generation u64, expected_surface u64, expected_context u64) int {
		assert expected_generation != 0
		assert expected_surface != 0
		assert expected_context != 0
		bind := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot, start, .egl,
			.make_current, .anchor_prepare, .anchor)
		expected_bind := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        snapshot.renderer_attempt_token
			renderer_attempt_token: snapshot.renderer_attempt_token
			app_identity:           snapshot.app_identity
			presence_mask:          native_context_has_target_generation | native_context_has_target_identity
			domain:                 .egl
			operation:              .make_current
			call_site:              .anchor_prepare
			scope:                  .anchor
			target_generation:      expected_generation
			target_identity:        expected_surface
			ordinal:                bind.ordinal
		}
		assert native_operation_contexts_identical(bind, expected_bind)
		assert snapshot.trace[start + 1].actual.has(native_valid_return_value)
		assert snapshot.trace[start + 1].actual.return_value == 1
		operations := [NativeRenderOperation.current_draw_query, .current_read_query,
			.current_context_query]
		expected_handles := [expected_surface, expected_surface, expected_context]
		for operation_index, operation in operations {
			query_start := start + 5 + operation_index * 5
			query := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot,
				query_start, .egl, operation, .anchor_prepare, .anchor)
			expected_query := NativeOperationContext{
				...expected_bind
				presence_mask:   native_context_has_target_generation
				operation:       operation
				target_identity: 0
				ordinal:         bind.ordinal + u64(operation_index + 2)
			}
			assert native_operation_contexts_identical(query, expected_query)
			actual := snapshot.trace[query_start + 1].actual
			assert actual.has(native_valid_handle)
			assert actual.handle == expected_handles[operation_index]
		}
		return start + 20
	}

	fn native_wayland_exercise_private_anchor_acquisition_boundaries() ! {
		for boundary in [NativeWaylandAnchorBoundary.wl_surface, .wl_egl_window, .egl_surface] {
			mut app := native_wayland_anchor_app_for_test()!
			assert app.backend.native_operations.arm_proof()
			native_release_oracle_reset_for_test(.wayland)
			baseline := native_lifetime_registry_snapshot(&app.backend.native_operations)
			base_ordinal := app.backend.native_operations.next_ordinal
			selector :=
				native_wayland_arm_anchor_boundary_for_test(mut app, boundary, base_ordinal)!
			mut failure := ''
			app.backend.wayland.create_renderer_anchor() or { failure = err.msg() }
			expected_error := match boundary {
				.wl_surface { err_wayland_create_surface_failed }
				.wl_egl_window { err_wayland_egl_surface_failed }
				.egl_surface { err_render_native_renderer_unavailable }
			}

			assert failure == expected_error
			release_oracle := native_release_oracle_snapshot_for_test(.wayland)
			native_assert_wayland_anchor_failure_for_test(app, boundary, selector, base_ordinal,
				baseline, release_oracle)
			assert app.backend.native_operations.disarm_proof()
			app.stop() or {}
			assert app.status() == .stopped
			assert !app.backend.wayland.retains_native_ownership()
		}

		mut healthy := native_wayland_anchor_app_for_test()!
		assert healthy.backend.native_operations.arm_proof()
		native_release_oracle_reset_for_test(.wayland)
		before_release_oracle := native_release_oracle_snapshot_for_test(.wayland)
		base_ordinal := healthy.backend.native_operations.next_ordinal
		healthy.backend.wayland.create_renderer_anchor()!
		created := native_proof_snapshot(&healthy.backend.native_operations)
		assert !created.trace_overflow
		assert created.trace_len >= 21
		display_identity := native_identity(healthy.backend.wayland.display)
		egl_display_identity := native_identity(healthy.backend.wayland.egl_display)
		wl_surface := native_assert_wayland_anchor_primary_for_test(created, 0, native_wayland_anchor_selector_for_test(healthy,
			.wl_surface, base_ordinal), display_identity, false)
		wl_egl_window := native_assert_wayland_anchor_primary_for_test(created, 8, native_wayland_anchor_selector_for_test(healthy,
			.wl_egl_window, base_ordinal), display_identity, false)
		egl_surface := native_assert_egl_anchor_primary_for_test(created, 16, native_wayland_anchor_selector_for_test(healthy,
			.egl_surface, base_ordinal), false)
		assert native_identity(healthy.backend.wayland.anchor_wl_surface) == wl_surface
		assert native_identity(healthy.backend.wayland.anchor_wl_egl_window) == wl_egl_window
		assert native_identity(healthy.backend.wayland.anchor_surface) == egl_surface
		generation := healthy.backend.wayland.anchor_generation
		egl_ticket := native_lifetime_ticket_snapshot_for_test(&healthy.backend.native_operations,

			base_ordinal + 6)!
		wl_egl_ticket := native_lifetime_ticket_snapshot_for_test(&healthy.backend.native_operations,

			base_ordinal + 7)!
		wl_surface_ticket := native_lifetime_ticket_snapshot_for_test(&healthy.backend.native_operations,

			base_ordinal + 8)!
		context_ticket := native_lifetime_ticket_snapshot_for_test(&healthy.backend.native_operations,
			healthy.backend.wayland.egl_context_ticket)!
		display_ticket := native_lifetime_ticket_snapshot_for_test(&healthy.backend.native_operations,
			healthy.backend.wayland.egl_display_ticket)!
		thread_ticket := native_lifetime_ticket_snapshot_for_test(&healthy.backend.native_operations,
			healthy.backend.wayland.egl_thread_ticket)!
		native_assert_wayland_anchor_ticket_for_test(egl_ticket, healthy, base_ordinal + 6,
			.egl_surface, egl_surface, egl_display_identity, .app_lifetime, generation)
		native_assert_wayland_anchor_ticket_for_test(wl_egl_ticket, healthy, base_ordinal + 7,
			.wayland_egl_window, wl_egl_window, wl_surface, .renderer_attempt, generation)
		native_assert_wayland_anchor_ticket_for_test(wl_surface_ticket, healthy, base_ordinal + 8,
			.wayland_surface, wl_surface, native_identity(healthy.backend.wayland.compositor),
			.none, generation)
		binding_end := native_assert_egl_anchor_binding_suffix_for_test(created, 21, generation,
			egl_surface, native_identity(healthy.backend.wayland.egl_context))
		assert binding_end == created.trace_len
		cleanup_tickets := native_lifetime_registry_snapshot(&healthy.backend.native_operations).tickets
		proof_generation := healthy.backend.native_operations.proof.generation
		native_reset_or_clear_proof_for_test(mut healthy.backend.native_operations)
		assert healthy.backend.native_operations.proof.generation == proof_generation
		assert healthy.backend.native_operations.proof.trace_len == 0
		assert !healthy.backend.native_operations.proof.trace_overflow
		assert !healthy.backend.native_operations.has_pending_native_plans()
		healthy.backend.wayland.shutdown_renderer()
		stopped := native_proof_snapshot(&healthy.backend.native_operations)
		after_release_oracle := native_release_oracle_snapshot_for_test(.wayland)
		mut release_cursor := 0
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, egl_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, wl_egl_ticket,
			release_cursor)
		native_assert_wayland_anchor_surface_release_mode_at_for_test(stopped, wl_surface_ticket,
			release_cursor, wayland_anchor_release_protocol_destroy)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, wl_surface_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, context_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, display_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, thread_ticket,
			release_cursor)
		assert release_cursor == stopped.trace_len
		ordered_tickets := [egl_ticket, wl_egl_ticket, wl_surface_ticket, context_ticket,
			display_ticket, thread_ticket]
		assert cleanup_tickets.len == ordered_tickets.len
		mut oracle_cursor := before_release_oracle.records.len
		for ticket in ordered_tickets {
			assert oracle_cursor < after_release_oracle.records.len
			record := after_release_oracle.records[oracle_cursor]
			assert native_release_oracle_record_matches_ticket_for_test(record, ticket)
			if ticket.release_kind == .wayland_surface {
				assert record.mode == wayland_anchor_release_protocol_destroy
			}
			oracle_cursor++
		}
		assert oracle_cursor == after_release_oracle.records.len
		native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
			after_release_oracle, cleanup_tickets)
		$if linux && sokol_wayland ? {
			assert healthy.backend.wayland.renderer_anchor_lifetime_absent()
		}
		assert healthy.backend.native_operations.lifetime_tickets.len == 0
		assert !healthy.backend.native_operations.has_pending_native_plans()
		native_assert_complete_native_trace_for_test(stopped)
		assert healthy.backend.native_operations.disarm_proof()
		healthy.stop() or {}
		assert healthy.status() == .stopped
	}

	fn native_phase_a_assert_ticket_slot_for_test(snapshot NativePhaseABackendOwnershipSnapshot, ticket_id u64, release_kind NativeLifetimeReleaseKind, identity u64, parent_identity u64, authority_scope NativeOperationAuthorityScope, parent_authority_scope NativeOperationAuthorityScope, abandoned bool) {
		assert ticket_id != 0
		assert identity != 0
		mut found := false
		for ticket in snapshot.registry.tickets {
			if ticket.ticket_id != ticket_id {
				continue
			}
			assert !found
			found = true
			assert ticket.app_identity == snapshot.registry.app_identity
			assert ticket.authority_scope == authority_scope
			assert ticket.authority_token == if authority_scope == .app_lifetime {
				snapshot.registry.app_lifetime_token
			} else {
				snapshot.registry.renderer_attempt_token
			}
			assert ticket.domain == native_lifetime_release_domain(release_kind)
			assert ticket.release_kind == release_kind
			assert ticket.native_identity == identity
			assert ticket.required_parent_identity == parent_identity
			assert ticket.parent_authority_scope == parent_authority_scope
			assert ticket.parent_authority_token == match parent_authority_scope {
				.app_lifetime { snapshot.registry.app_lifetime_token }
				.renderer_attempt { snapshot.registry.renderer_attempt_token }
				.none { u64(0) }
			}

			assert ticket.state == if abandoned {
				.abandoned
			} else {
				.bound
			}
			assert ticket.context.app_identity == ticket.app_identity
			assert ticket.context.authority_scope == ticket.authority_scope
			assert ticket.context.authority_token == ticket.authority_token
			assert ticket.context.renderer_attempt_token == if ticket.authority_scope == .renderer_attempt {
				ticket.authority_token
			} else {
				u64(0)
			}
			assert ticket.context.domain == ticket.domain
			assert ticket.context.operation == native_lifetime_release_operation(release_kind)
			assert ticket.context.ordinal == ticket.ticket_id
			assert ticket.context.target_identity == ticket.native_identity
			assert ticket.context.presence_mask == (ticket.owner_seed.presence_mask | native_context_has_target_identity)
			assert ticket.context.call_site == ticket.owner_seed.call_site
			assert ticket.context.scope == ticket.owner_seed.scope
			assert ticket.context.window == ticket.owner_seed.window
			assert ticket.context.target_generation == ticket.owner_seed.target_generation
			assert ticket.context.batch_epoch == ticket.owner_seed.batch_epoch
			assert ticket.context.window_lease_epoch == ticket.owner_seed.window_lease_epoch
			assert ticket.context.target_lease_epoch == ticket.owner_seed.target_lease_epoch
		}
		assert found
	}

	fn native_ticket_snapshot_by_id_for_test(tickets []NativeLifetimeTicketProofSnapshot, ticket_id u64) !NativeLifetimeTicketProofSnapshot {
		for ticket in tickets {
			if ticket.ticket_id == ticket_id {
				return ticket
			}
		}
		return error('native lifetime ticket `${ticket_id}` is absent from the snapshot')
	}

	fn native_assert_terminal_failure_acquired_ticket_for_test(ticket NativeLifetimeTicketProofSnapshot, ticket_id u64, release_kind NativeLifetimeReleaseKind, lease RenderTargetLease, target_generation u64, identity u64, parent_identity u64, parent_authority_scope NativeOperationAuthorityScope, app_identity u64, app_lifetime_token u64, proof_generation u64) {
		expected_seed := NativeOperationSeed{
			presence_mask:      native_context_window_target_fields
			call_site:          .window_prepare
			scope:              .window_target
			window:             lease.window
			target_generation:  target_generation
			batch_epoch:        lease.batch_epoch
			window_lease_epoch: lease.window_epoch
			target_lease_epoch: lease.target_epoch
		}
		expected_parent_token := if parent_authority_scope == .app_lifetime {
			app_lifetime_token
		} else {
			u64(0)
		}
		expected_context := NativeOperationContext{
			authority_scope:        .app_lifetime
			authority_token:        app_lifetime_token
			renderer_attempt_token: 0
			app_identity:           app_identity
			presence_mask:          native_context_window_target_fields | native_context_has_target_identity
			domain:                 native_lifetime_release_domain(release_kind)
			operation:              native_lifetime_release_operation(release_kind)
			call_site:              .window_prepare
			scope:                  .window_target
			window:                 lease.window
			target_generation:      target_generation
			target_identity:        identity
			batch_epoch:            lease.batch_epoch
			window_lease_epoch:     lease.window_epoch
			target_lease_epoch:     lease.target_epoch
			ordinal:                ticket_id
		}
		assert ticket.ticket_id == ticket_id
		assert ticket.app_identity == app_identity
		assert ticket.authority_scope == .app_lifetime
		assert ticket.authority_token == app_lifetime_token
		assert ticket.domain == native_lifetime_release_domain(release_kind)
		assert ticket.release_kind == release_kind
		assert native_operation_seeds_identical(ticket.owner_seed, expected_seed)
		assert ticket.proof_generation == proof_generation
		assert native_operation_contexts_identical(ticket.context, expected_context)
		assert ticket.native_identity == identity
		assert ticket.required_parent_identity == parent_identity
		assert ticket.parent_authority_scope == parent_authority_scope
		assert ticket.parent_authority_token == expected_parent_token
		assert ticket.state == .bound
	}

	fn native_assert_terminal_failure_acquisition_delta_for_test(backend BackendKind, baseline_tickets []NativeLifetimeTicketProofSnapshot, acquired_tickets []NativeLifetimeTicketProofSnapshot, proof &NativeEglFrameProof, app_identity u64, app_lifetime_token u64, proof_generation u64, egl_display_identity u64) ! {
		mut expected_ticket_ids := []u64{cap: acquired_tickets.len}
		for baseline_ticket in baseline_tickets {
			assert baseline_ticket.ticket_id !in expected_ticket_ids
			expected_ticket_ids << baseline_ticket.ticket_id
			acquired_ticket := native_ticket_snapshot_by_id_for_test(acquired_tickets,
				baseline_ticket.ticket_id)!
			native_lifetime_ticket_assert_equal(baseline_ticket, acquired_ticket)
		}
		assert proof.failed_target.ticket_id != 0
		assert proof.failed_target.identity != 0
		assert proof.failed_target.generation != 0
		assert !proof.failed_target.native_destroyed
		assert proof.failed_target.frame_ready
		assert proof.failed_target.ticket_id !in expected_ticket_ids
		expected_ticket_ids << proof.failed_target.ticket_id
		failed_ticket := native_ticket_snapshot_by_id_for_test(acquired_tickets,
			proof.failed_target.ticket_id)!
		native_assert_terminal_failure_acquired_ticket_for_test(failed_ticket,
			proof.failed_target.ticket_id, .egl_surface, proof.lease,
			proof.failed_target.generation, proof.failed_target.identity, egl_display_identity,
			.app_lifetime, app_identity, app_lifetime_token, proof_generation)
		match backend {
			.x11 {
				assert proof.platform_ticket_id == 0
				assert proof.platform_target_identity == 0
				assert proof.platform_parent_identity == 0
			}
			.wayland {
				assert proof.platform_ticket_id != 0
				assert proof.platform_target_identity != 0
				assert proof.platform_parent_identity != 0
				assert proof.platform_ticket_id !in expected_ticket_ids
				expected_ticket_ids << proof.platform_ticket_id
				platform_ticket := native_ticket_snapshot_by_id_for_test(acquired_tickets,
					proof.platform_ticket_id)!
				native_assert_terminal_failure_acquired_ticket_for_test(platform_ticket,
					proof.platform_ticket_id, .wayland_egl_window, proof.lease,
					proof.failed_target.generation, proof.platform_target_identity,
					proof.platform_parent_identity, .none, app_identity, app_lifetime_token,
					proof_generation)
			}
			else {
				assert false, 'terminal EGL acquisition proof selected unsupported backend'
			}
		}

		assert acquired_tickets.len == expected_ticket_ids.len
		for expected_ticket_id in expected_ticket_ids {
			mut matches := 0
			for ticket in acquired_tickets {
				if ticket.ticket_id == expected_ticket_id {
					matches++
				}
			}
			assert matches == 1
		}
		for ticket in acquired_tickets {
			assert ticket.ticket_id in expected_ticket_ids
		}
	}

	fn native_assert_terminal_failure_ticket_transition_for_test(before_tickets []NativeLifetimeTicketProofSnapshot, after_tickets []NativeLifetimeTicketProofSnapshot, egl_error i64) ! {
		mut expected_ticket_ids := []u64{cap: before_tickets.len}
		for before_ticket in before_tickets {
			assert before_ticket.ticket_id !in expected_ticket_ids
			expected_ticket_ids << before_ticket.ticket_id
			assert before_ticket.state == .bound
			after_ticket := native_ticket_snapshot_by_id_for_test(after_tickets,
				before_ticket.ticket_id)!
			expected_state := if egl_error == 0x3008 && before_ticket.domain == .egl
				&& before_ticket.release_kind != .egl_thread {
				NativeLifetimeTicketState.abandoned
			} else {
				NativeLifetimeTicketState.bound
			}
			native_lifetime_ticket_assert_equal(NativeLifetimeTicketProofSnapshot{
				...before_ticket
				state: expected_state
			}, after_ticket)
		}
		assert after_tickets.len == expected_ticket_ids.len
		for after_ticket in after_tickets {
			assert after_ticket.ticket_id in expected_ticket_ids
		}
	}

	fn native_assert_phase_a_ticket_retirement_for_test(snapshot NativeAuthorityProofSnapshot, tickets []NativeLifetimeTicketProofSnapshot, after_index int) {
		mut release_starts := map[u64]int{}
		for ticket in tickets {
			if ticket.state == .abandoned {
				assert ticket.domain == .egl
				assert ticket.release_kind != .egl_thread
				assert native_lifetime_release_start_in_snapshot_for_test(snapshot, ticket.context) == -1
				continue
			}
			assert ticket.state == .bound
			release_starts[ticket.ticket_id] = native_assert_lifetime_release_sequence_in_snapshot_for_test(snapshot,
				ticket.context, after_index)
			assert native_lifetime_trace_entry_count_for_test(snapshot, ticket.context) == 6
		}
		for child in tickets {
			if child.state != .bound || child.required_parent_identity == 0 {
				continue
			}
			for parent in tickets {
				if parent.state == .bound
					&& parent.native_identity == child.required_parent_identity
					&& parent.authority_scope == child.parent_authority_scope
					&& parent.authority_token == child.parent_authority_token
					&& parent.ticket_id in release_starts {
					assert release_starts[child.ticket_id] < release_starts[parent.ticket_id]
				}
			}
		}
		for local in tickets {
			if local.release_kind != .wayland_egl_window || local.state != .bound {
				continue
			}
			for surface in tickets {
				if surface.release_kind == .egl_surface && surface.state == .bound
					&& surface.owner_seed.window == local.owner_seed.window {
					assert release_starts[surface.ticket_id] < release_starts[local.ticket_id]
				}
			}
		}
		for callback in tickets {
			if callback.release_kind != .wayland_frame_callback || callback.state != .bound {
				continue
			}
			for child in tickets {
				if child.state == .bound && child.owner_seed.window == callback.owner_seed.window
					&& child.release_kind in [.egl_surface, .wayland_egl_window] {
					assert release_starts[callback.ticket_id] < release_starts[child.ticket_id]
				}
			}
		}
		mut display_ticket := u64(0)
		mut thread_ticket := u64(0)
		for ticket in tickets {
			if ticket.release_kind == .egl_display && ticket.state == .bound {
				display_ticket = ticket.ticket_id
			}
			if ticket.release_kind == .egl_thread && ticket.state == .bound {
				thread_ticket = ticket.ticket_id
			}
		}
		if display_ticket != 0 && thread_ticket != 0 {
			assert release_starts[display_ticket] < release_starts[thread_ticket]
		}
	}

	fn native_assert_only_phase_a_ticket_releases_for_test(snapshot NativeAuthorityProofSnapshot, tickets []NativeLifetimeTicketProofSnapshot, start_index int) {
		assert start_index >= 0
		mut expected_entries := 0
		for ticket in tickets {
			if ticket.state == .bound {
				expected_entries += 6
				native_assert_lifetime_release_sequence_in_snapshot_for_test(snapshot,
					ticket.context, start_index)
				assert native_lifetime_trace_entry_count_for_test(snapshot, ticket.context) == 6
			} else {
				assert ticket.state == .abandoned
				assert ticket.domain == .egl
				assert ticket.release_kind != .egl_thread
				assert native_lifetime_trace_entry_count_for_test(snapshot, ticket.context) == 0
			}
		}
		assert snapshot.trace_len == start_index + expected_entries
		for index in start_index .. snapshot.trace_len {
			entry := snapshot.trace[index]
			mut preauthorized := false
			for ticket in tickets {
				if ticket.state == .bound
					&& native_operation_contexts_identical(entry.context, ticket.context) {
					preauthorized = true
					break
				}
			}
			assert preauthorized
		}
	}

	fn native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, start int, domain NativeRenderDomain, operation NativeRenderOperation, call_site NativeRenderCallSite, scope NativeRenderScope) NativeOperationContext {
		assert start >= 0
		assert start + 5 <= snapshot.trace_len
		context := snapshot.trace[start].context
		assert context.domain == domain
		assert context.operation == operation
		assert context.call_site == call_site
		assert context.scope == scope
		assert context.ordinal != 0
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		assert actual.valid_mask != 0
		assert native_proof_evidence_equal(actual, effective)
		accepted := snapshot.trace[start + 3]
		assert native_proof_evidence_equal(accepted.actual, actual)
		assert native_proof_evidence_equal(accepted.effective, effective)
		assert accepted.local_validation == .none
		assert accepted.result.succeeded()
		assert native_operation_contexts_identical(accepted.result.context, context)
		assert snapshot.trace[start + 4].health == .ready
		return context
	}

	fn native_phase_a_window_ownership_for_test(snapshot NativePhaseABackendOwnershipSnapshot, id WindowId) NativePhaseAWindowOwnershipSnapshot {
		mut found := NativePhaseAWindowOwnershipSnapshot{}
		mut matches := 0
		for window in snapshot.windows {
			if window.id == id {
				found = window
				matches++
			}
		}
		assert matches == 1
		return found
	}

	fn native_assert_egl_anchor_prepare_prefix_for_test(backend BackendKind, snapshot NativeAuthorityProofSnapshot, expected_generation u64, expected_surface u64, expected_context u64) int {
		assert backend in [.x11, .wayland]
		assert expected_generation != 0
		assert expected_surface != 0
		assert expected_context != 0
		bind := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot, 0, .egl,
			.make_current, .anchor_prepare, .anchor)
		expected_bind := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        snapshot.renderer_attempt_token
			renderer_attempt_token: snapshot.renderer_attempt_token
			app_identity:           snapshot.app_identity
			presence_mask:          native_context_has_target_generation | native_context_has_target_identity
			domain:                 .egl
			operation:              .make_current
			call_site:              .anchor_prepare
			scope:                  .anchor
			target_generation:      expected_generation
			target_identity:        expected_surface
			ordinal:                bind.ordinal
		}
		assert native_operation_contexts_identical(bind, expected_bind)
		bind_actual_index := 1
		assert snapshot.trace[bind_actual_index].actual.has(native_valid_return_value)
		assert snapshot.trace[bind_actual_index].actual.return_value == 1

		operations := [NativeRenderOperation.current_draw_query, .current_read_query,
			.current_context_query]
		starts := [5, 10, 15]
		expected_handles := [expected_surface, expected_surface, expected_context]
		for index, operation in operations {
			query := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot,
				starts[index], .egl, operation, .anchor_prepare, .anchor)
			expected_query := NativeOperationContext{
				...expected_bind
				presence_mask:   native_context_has_target_generation
				operation:       operation
				target_identity: 0
				ordinal:         bind.ordinal + u64(index + 2)
			}
			assert native_operation_contexts_identical(query, expected_query)
			actual := snapshot.trace[starts[index] + 1].actual
			assert actual.has(native_valid_handle)
			assert actual.handle == expected_handles[index]
		}
		return 20
	}

	fn native_assert_successful_wayland_boundary_chain_for_test(snapshot NativeAuthorityProofSnapshot, start int, operation NativeRenderOperation, expected_validation NativeLocalValidation, expected_display u64) NativeOperationContext {
		assert expected_display != 0
		assert start >= 0
		assert start + 8 <= snapshot.trace_len
		context := snapshot.trace[start].context
		assert context.domain == .wayland
		assert context.operation == operation
		assert context.call_site == .window_prepare
		assert context.scope == .window_target
		for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
			.effective_primitive] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		query := snapshot.trace[start + 3].context
		expected_query := NativeOperationContext{
			authority_scope:        context.authority_scope
			authority_token:        context.authority_token
			renderer_attempt_token: context.renderer_attempt_token
			app_identity:           context.app_identity
			presence_mask:          native_context_has_target_identity
			domain:                 .wayland
			operation:              .wayland_display_error_query
			call_site:              .display_transport
			scope:                  .renderer
			target_identity:        expected_display
			ordinal:                context.ordinal + 1
		}
		assert native_operation_contexts_identical(query, expected_query)
		for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
			.effective_primitive] {
			entry := snapshot.trace[start + 3 + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, query)
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		query_actual := snapshot.trace[start + 4].actual
		query_effective := snapshot.trace[start + 5].effective
		assert actual.valid_mask != 0
		assert native_proof_evidence_equal(actual, effective)
		assert query_actual.has(native_valid_wayland_display_error)
		assert native_proof_evidence_equal(query_actual, query_effective)
		accepted := snapshot.trace[start + 6]
		assert accepted.milestone == .acceptance
		assert native_operation_contexts_identical(accepted.context, context)
		assert accepted.local_validation == expected_validation
		assert accepted.result.succeeded()
		assert native_operation_contexts_identical(accepted.result.context, context)
		latched := snapshot.trace[start + 7]
		assert latched.milestone == .health_latched
		assert native_operation_contexts_identical(latched.context, context)
		assert latched.health == .ready
		return context
	}

	fn native_assert_window_prepare_context_for_test(snapshot NativeAuthorityProofSnapshot, context NativeOperationContext, lease RenderTargetLease, domain NativeRenderDomain, operation NativeRenderOperation, target_generation u64, target_identity u64, ordinal u64) {
		expected := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        snapshot.renderer_attempt_token
			renderer_attempt_token: snapshot.renderer_attempt_token
			app_identity:           snapshot.app_identity
			presence_mask:          native_context_window_target_fields | native_context_has_target_identity
			domain:                 domain
			operation:              operation
			call_site:              .window_prepare
			scope:                  .window_target
			window:                 lease.window
			target_generation:      target_generation
			target_identity:        target_identity
			batch_epoch:            lease.batch_epoch
			window_lease_epoch:     lease.window_epoch
			target_lease_epoch:     lease.target_epoch
			ordinal:                ordinal
		}
		assert native_operation_contexts_identical(context, expected)
	}

	fn native_assert_ordinal_setup_ticket_delta_for_test(backend BackendKind, before_tickets []NativeLifetimeTicketProofSnapshot, after_tickets []NativeLifetimeTicketProofSnapshot, before_window NativePhaseAWindowOwnershipSnapshot, after_window NativePhaseAWindowOwnershipSnapshot, lease RenderTargetLease, snapshot NativeAuthorityProofSnapshot, egl_display_identity u64) ! {
		mut expected_ticket_ids := []u64{cap: after_tickets.len}
		for before_ticket in before_tickets {
			assert before_ticket.ticket_id !in expected_ticket_ids
			expected_ticket_ids << before_ticket.ticket_id
			after_ticket := native_ticket_snapshot_by_id_for_test(after_tickets,
				before_ticket.ticket_id)!
			native_lifetime_ticket_assert_equal(before_ticket, after_ticket)
		}
		if backend == .wayland {
			if before_window.wl_egl_window == 0 {
				assert after_window.wl_egl_window != 0
				assert after_window.wl_egl_window_ticket != 0
				assert after_window.wl_egl_window_ticket !in expected_ticket_ids
				expected_ticket_ids << after_window.wl_egl_window_ticket
				platform_ticket := native_ticket_snapshot_by_id_for_test(after_tickets,
					after_window.wl_egl_window_ticket)!
				native_assert_terminal_failure_acquired_ticket_for_test(platform_ticket,
					after_window.wl_egl_window_ticket, .wayland_egl_window, lease,
					after_window.render_target_generation, after_window.wl_egl_window,
					after_window.native_window_identity, .none, snapshot.app_identity,
					snapshot.app_lifetime_token, snapshot.proof_generation)
			} else {
				assert after_window.wl_egl_window == before_window.wl_egl_window
				assert after_window.wl_egl_window_ticket == before_window.wl_egl_window_ticket
			}
		} else {
			assert backend == .x11
			assert before_window.wl_egl_window == 0
			assert after_window.wl_egl_window == 0
		}
		assert before_window.egl_surface == 0
		assert before_window.egl_surface_ticket == 0
		assert after_window.egl_surface != 0
		assert after_window.egl_surface_ticket != 0
		assert after_window.egl_surface_ticket !in expected_ticket_ids
		expected_ticket_ids << after_window.egl_surface_ticket
		egl_ticket := native_ticket_snapshot_by_id_for_test(after_tickets,
			after_window.egl_surface_ticket)!
		native_assert_terminal_failure_acquired_ticket_for_test(egl_ticket,
			after_window.egl_surface_ticket, .egl_surface, lease,
			after_window.render_target_generation, after_window.egl_surface, egl_display_identity,
			.app_lifetime, snapshot.app_identity, snapshot.app_lifetime_token,
			snapshot.proof_generation)
		assert after_tickets.len == expected_ticket_ids.len
		for after_ticket in after_tickets {
			assert after_ticket.ticket_id in expected_ticket_ids
		}
	}

	fn native_assert_ordinal_setup_prefix_for_test(backend BackendKind, snapshot NativeAuthorityProofSnapshot, before_window NativePhaseAWindowOwnershipSnapshot, after_window NativePhaseAWindowOwnershipSnapshot, ownership NativePhaseABackendOwnershipSnapshot, lease RenderTargetLease, target NativeEglTargetProof) int {
		mut cursor := native_assert_egl_anchor_prepare_prefix_for_test(backend, snapshot,
			ownership.anchor_generation, ownership.anchor_surface, ownership.egl_context)
		anchor_bind_index := 0
		mut expected_ordinal := snapshot.trace[anchor_bind_index].context.ordinal + 5
		assert after_window.id == before_window.id
		assert after_window.id == lease.window
		assert after_window.native_window_identity == before_window.native_window_identity
		assert after_window.render_target_generation == before_window.render_target_generation
		assert before_window.egl_surface == 0
		assert before_window.egl_surface_ticket == 0
		assert target.generation == after_window.render_target_generation
		assert target.identity == after_window.egl_surface
		assert target.ticket_id == after_window.egl_surface_ticket
		match backend {
			.x11 {
				assert cursor == 20
				context := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot,
					cursor, .egl, .window_surface_create, .window_prepare, .window_target)
				native_assert_window_prepare_context_for_test(snapshot, context, lease, .egl,
					.window_surface_create, after_window.render_target_generation,
					before_window.native_window_identity, expected_ordinal)
				assert snapshot.trace[cursor + 1].actual.has(native_valid_handle)
				assert snapshot.trace[cursor + 1].actual.handle == after_window.egl_surface
				assert after_window.egl_surface_ticket == expected_ordinal + 2
				cursor += 5
				expected_ordinal += 3
				assert cursor == 25
			}
			.wayland {
				assert cursor == 20
				resize_selected := before_window.pending_egl_resize
					&& before_window.wl_egl_window != 0
				create_companion_selected := before_window.wl_egl_window == 0
				assert !(resize_selected && create_companion_selected)
				if resize_selected {
					context := native_assert_successful_wayland_boundary_chain_for_test(snapshot,
						cursor, .window_configure, .void_completion, ownership.native_display)
					native_assert_window_prepare_context_for_test(snapshot, context, lease,
						.wayland, .window_configure, after_window.render_target_generation,
						before_window.wl_egl_window, expected_ordinal)
					cursor += 8
					expected_ordinal += 2
				}
				if create_companion_selected {
					context := native_assert_successful_wayland_boundary_chain_for_test(snapshot,
						cursor, .window_surface_create, .none, ownership.native_display)
					native_assert_window_prepare_context_for_test(snapshot, context, lease,
						.wayland, .window_surface_create, after_window.render_target_generation,
						before_window.native_window_identity, expected_ordinal)
					assert snapshot.trace[cursor + 1].actual.has(native_valid_handle)
					assert snapshot.trace[cursor + 1].actual.handle == after_window.wl_egl_window
					assert after_window.wl_egl_window_ticket == expected_ordinal + 2
					cursor += 8
					expected_ordinal += 3
				}
				context := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot,
					cursor, .egl, .window_surface_create, .window_prepare, .window_target)
				native_assert_window_prepare_context_for_test(snapshot, context, lease, .egl,
					.window_surface_create, after_window.render_target_generation,
					after_window.wl_egl_window, expected_ordinal)
				assert snapshot.trace[cursor + 1].actual.has(native_valid_handle)
				assert snapshot.trace[cursor + 1].actual.handle == after_window.egl_surface
				assert after_window.egl_surface_ticket == expected_ordinal + 2
				cursor += 5
				expected_ordinal += 3
				assert !after_window.pending_egl_resize
			}
			else {
				assert false, 'ordinal setup selected unsupported backend'
			}
		}

		assert cursor == snapshot.trace_len
		assert snapshot.next_ordinal == expected_ordinal
		return cursor
	}

	fn native_assert_successful_wayland_shutdown_flush_for_test(snapshot NativeAuthorityProofSnapshot, start int, expected_display u64, expected_window WindowId, expected_target_generation u64, expected_ordinal u64) {
		assert expected_display != 0
		assert expected_window != WindowId{}
		assert expected_target_generation != 0
		assert start >= 0
		assert start + 8 <= snapshot.trace_len
		context := snapshot.trace[start].context
		assert context.domain == .wayland
		assert context.operation == .display_flush
		assert context.call_site == .shutdown_release
		assert context.scope == .window_target
		assert context.presence_mask == native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
		assert context.window == expected_window
		assert context.target_generation == expected_target_generation
		assert context.target_identity == expected_display
		assert context.ordinal == expected_ordinal
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		query := snapshot.trace[start + 3].context
		expected_query := NativeOperationContext{
			...context
			presence_mask:     native_context_has_target_identity
			operation:         .wayland_display_error_query
			call_site:         .display_transport
			scope:             .renderer
			window:            WindowId{}
			target_generation: 0
			ordinal:           context.ordinal + 1
		}
		assert native_operation_contexts_identical(query, expected_query)
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		] {
			entry := snapshot.trace[start + 3 + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, query)
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		query_actual := snapshot.trace[start + 4].actual
		query_effective := snapshot.trace[start + 5].effective
		assert actual.valid_mask != 0
		assert native_proof_evidence_equal(actual, effective)
		assert query_actual.valid_mask & native_valid_wayland_display_error != 0
		assert native_proof_evidence_equal(query_actual, query_effective)
		accepted := snapshot.trace[start + 6]
		assert accepted.milestone == .acceptance
		assert native_operation_contexts_identical(accepted.context, context)
		assert accepted.local_validation == .none
		assert accepted.result.succeeded()
		assert native_operation_contexts_identical(accepted.result.context, context)
		latched := snapshot.trace[start + 7]
		assert latched.milestone == .health_latched
		assert native_operation_contexts_identical(latched.context, context)
		assert latched.health == .ready
	}

	fn native_assert_lifetime_release_at_for_test(snapshot NativeAuthorityProofSnapshot, ticket NativeLifetimeTicketProofSnapshot, start int) int {
		assert ticket.state == .bound
		assert start >= 0
		assert start + 6 <= snapshot.trace_len
		context := ticket.context
		assert context.ordinal == ticket.ticket_id
		assert context.target_identity == ticket.native_identity
		assert context.target_identity != 0
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
			.authority_release,
		] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		assert native_proof_evidence_equal(snapshot.trace[start + 1].actual, snapshot.trace[start +
			2].effective)
		accepted := snapshot.trace[start + 3]
		expected_validation := if context.domain == .wayland {
			NativeLocalValidation.void_completion
		} else {
			NativeLocalValidation.none
		}
		assert accepted.local_validation == expected_validation
		assert accepted.result.succeeded()
		assert native_operation_contexts_identical(accepted.result.context, context)
		assert snapshot.trace[start + 4].health != .uninitialized
		assert native_proof_result_equal(snapshot.trace[start + 5].result, accepted.result)
		return start + 6
	}

	fn native_assert_wayland_anchor_surface_release_mode_at_for_test(snapshot NativeAuthorityProofSnapshot, ticket NativeLifetimeTicketProofSnapshot, start int, expected_mode u64) {
		assert ticket.release_kind == .wayland_surface
		assert ticket.owner_seed.call_site == .anchor_create
		assert ticket.owner_seed.scope == .anchor
		assert expected_mode in [wayland_anchor_release_protocol_destroy,
			wayland_anchor_release_local_proxy_destroy]
		assert start >= 0
		assert start + 5 < snapshot.trace_len
		assert native_operation_contexts_identical(snapshot.trace[start].context, ticket.context)
		actual := snapshot.trace[start + 1].actual
		assert actual.has(native_valid_observed_flags)
		assert actual.observed_flags == expected_mode
		assert snapshot.trace[start + 5].milestone == .authority_release
		assert snapshot.trace[start + 5].actual == actual
	}

	fn native_assert_normal_egl_shutdown_for_test(backend BackendKind, snapshot NativeAuthorityProofSnapshot, tickets []NativeLifetimeTicketProofSnapshot, wayland_display_identity u64, live_window WindowId) {
		native_assert_complete_native_trace_for_test(snapshot)
		bind_start := 0
		bind := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot, bind_start,
			.egl, .make_current, .shutdown_anchor, .anchor)
		assert bind.presence_mask == native_context_has_target_generation | native_context_has_target_identity
		assert bind.target_generation != 0
		assert bind.target_identity != 0
		assert snapshot.trace[bind_start + 1].actual.has(native_valid_return_value)
		assert snapshot.trace[bind_start + 1].actual.return_value == 1

		draw_start := bind_start + 5
		draw := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot, draw_start,
			.egl, .current_draw_query, .shutdown_anchor, .anchor)
		read_start := draw_start + 5
		read := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot, read_start,
			.egl, .current_read_query, .shutdown_anchor, .anchor)
		current_start := read_start + 5
		current := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot,
			current_start, .egl, .current_context_query, .shutdown_anchor, .anchor)
		assert draw.ordinal == bind.ordinal + 2
		assert read.ordinal == bind.ordinal + 3
		assert current.ordinal == bind.ordinal + 4
		for query in [draw, read, current] {
			expected := NativeOperationContext{
				...bind
				presence_mask:   bind.presence_mask & ~native_context_has_target_identity
				operation:       query.operation
				target_identity: 0
				ordinal:         query.ordinal
			}
			assert native_operation_contexts_identical(query, expected)
		}
		assert snapshot.trace[draw_start + 1].actual.has(native_valid_handle)
		assert snapshot.trace[draw_start + 1].actual.handle == bind.target_identity
		assert snapshot.trace[read_start + 1].actual.has(native_valid_handle)
		assert snapshot.trace[read_start + 1].actual.handle == bind.target_identity
		assert snapshot.trace[current_start + 1].actual.has(native_valid_handle)
		assert snapshot.trace[current_start + 1].actual.handle != 0

		unbind_start := current_start + 5
		unbind := native_assert_successful_boundary_chain_in_snapshot_for_test(snapshot,
			unbind_start, .egl, .make_current, .shutdown_anchor, .anchor)
		expected_unbind := NativeOperationContext{
			...bind
			ordinal: bind.ordinal + 5
		}
		assert native_operation_contexts_identical(unbind, expected_unbind)
		assert snapshot.trace[unbind_start + 1].actual.has(native_valid_return_value)
		assert snapshot.trace[unbind_start + 1].actual.return_value == 1
		release_start := unbind_start + 5

		match backend {
			.x11 {
				assert live_window != WindowId{}
				mut anchor_ticket := NativeLifetimeTicketProofSnapshot{}
				mut anchor_matches := 0
				mut window_surface_ticket := NativeLifetimeTicketProofSnapshot{}
				mut window_surface_matches := 0
				mut context_ticket := NativeLifetimeTicketProofSnapshot{}
				mut context_matches := 0
				mut display_ticket := NativeLifetimeTicketProofSnapshot{}
				mut display_matches := 0
				mut thread_ticket := NativeLifetimeTicketProofSnapshot{}
				mut thread_matches := 0
				for ticket in tickets {
					assert ticket.state == .bound
					if ticket.owner_seed.window == live_window {
						assert ticket.release_kind == .egl_surface
						assert ticket.authority_scope == .app_lifetime
						assert ticket.owner_seed.call_site == .window_prepare
						assert ticket.owner_seed.scope == .window_target
						window_surface_matches++
						window_surface_ticket = ticket
					} else if ticket.owner_seed.window != WindowId{} {
						assert false, 'X11 shutdown proof found a ticket for another live window'
					} else if ticket.release_kind == .egl_surface
						&& ticket.owner_seed.call_site == .anchor_create
						&& ticket.owner_seed.scope == .anchor {
						assert ticket.authority_scope == .renderer_attempt
						anchor_matches++
						anchor_ticket = ticket
					} else {
						assert ticket.authority_scope == .app_lifetime
						assert ticket.owner_seed.call_site == .renderer_start
						assert ticket.owner_seed.scope == .renderer
						match ticket.release_kind {
							.egl_context {
								context_matches++
								context_ticket = ticket
							}
							.egl_display {
								display_matches++
								display_ticket = ticket
							}
							.egl_thread {
								thread_matches++
								thread_ticket = ticket
							}
							else {
								assert false, 'X11 shutdown proof found an unexpected backend ticket'
							}
						}
					}
				}
				assert anchor_matches == 1
				assert window_surface_matches == 1
				assert context_matches == 1
				assert display_matches == 1
				assert thread_matches == 1
				assert tickets.len == 5
				assert anchor_ticket.native_identity == bind.target_identity
				assert anchor_ticket.owner_seed.target_generation == bind.target_generation
				assert anchor_ticket.required_parent_identity != 0
				assert window_surface_ticket.owner_seed.target_generation != 0
				mut index := native_assert_lifetime_release_at_for_test(snapshot, anchor_ticket,
					release_start)
				index = native_assert_lifetime_release_at_for_test(snapshot, window_surface_ticket,
					index)
				index = native_assert_lifetime_release_at_for_test(snapshot, context_ticket, index)
				index = native_assert_lifetime_release_at_for_test(snapshot, display_ticket, index)
				index = native_assert_lifetime_release_at_for_test(snapshot, thread_ticket, index)
				assert index == snapshot.trace_len
			}
			.wayland {
				assert wayland_display_identity != 0
				assert live_window != WindowId{}
				mut anchor_ticket := NativeLifetimeTicketProofSnapshot{}
				mut anchor_matches := 0
				mut anchor_wl_egl_ticket := NativeLifetimeTicketProofSnapshot{}
				mut anchor_wl_egl_matches := 0
				mut anchor_wl_surface_ticket := NativeLifetimeTicketProofSnapshot{}
				mut anchor_wl_surface_matches := 0
				mut frame_ticket := NativeLifetimeTicketProofSnapshot{}
				mut frame_matches := 0
				mut window_surface_ticket := NativeLifetimeTicketProofSnapshot{}
				mut window_surface_matches := 0
				mut window_egl_ticket := NativeLifetimeTicketProofSnapshot{}
				mut window_egl_matches := 0
				mut context_ticket := NativeLifetimeTicketProofSnapshot{}
				mut context_matches := 0
				mut display_ticket := NativeLifetimeTicketProofSnapshot{}
				mut display_matches := 0
				mut thread_ticket := NativeLifetimeTicketProofSnapshot{}
				mut thread_matches := 0
				for ticket in tickets {
					assert ticket.state == .bound
					if ticket.owner_seed.window == live_window {
						assert ticket.owner_seed.scope == .window_target
						match ticket.release_kind {
							.wayland_frame_callback {
								assert ticket.authority_scope == .renderer_attempt
								assert ticket.owner_seed.call_site == .window_finalize
								frame_matches++
								frame_ticket = ticket
							}
							.egl_surface {
								assert ticket.authority_scope == .app_lifetime
								assert ticket.owner_seed.call_site == .window_prepare
								window_surface_matches++
								window_surface_ticket = ticket
							}
							.wayland_egl_window {
								assert ticket.authority_scope == .app_lifetime
								assert ticket.owner_seed.call_site == .window_prepare
								window_egl_matches++
								window_egl_ticket = ticket
							}
							else {
								assert false, 'Wayland shutdown proof found an unexpected live-window ticket'
							}
						}
					} else if ticket.owner_seed.window != WindowId{} {
						assert false, 'Wayland shutdown proof found a ticket for another live window'
					} else if ticket.owner_seed.call_site == .anchor_create
						&& ticket.owner_seed.scope == .anchor {
						assert ticket.authority_scope == .renderer_attempt
						match ticket.release_kind {
							.egl_surface {
								anchor_matches++
								anchor_ticket = ticket
							}
							.wayland_egl_window {
								anchor_wl_egl_matches++
								anchor_wl_egl_ticket = ticket
							}
							.wayland_surface {
								anchor_wl_surface_matches++
								anchor_wl_surface_ticket = ticket
							}
							else {
								assert false, 'Wayland shutdown proof found an unexpected anchor ticket'
							}
						}
					} else {
						assert ticket.authority_scope == .app_lifetime
						assert ticket.owner_seed.call_site == .renderer_start
						assert ticket.owner_seed.scope == .renderer
						match ticket.release_kind {
							.egl_context {
								context_matches++
								context_ticket = ticket
							}
							.egl_display {
								display_matches++
								display_ticket = ticket
							}
							.egl_thread {
								thread_matches++
								thread_ticket = ticket
							}
							else {
								assert false, 'Wayland shutdown proof found an unexpected backend ticket'
							}
						}
					}
				}
				assert anchor_matches == 1
				assert anchor_wl_egl_matches == 1
				assert anchor_wl_surface_matches == 1
				assert frame_matches <= 1
				assert window_surface_matches == 1
				assert window_egl_matches == 1
				assert context_matches == 1
				assert display_matches == 1
				assert thread_matches == 1
				expected_ticket_count := 8 + frame_matches
				assert tickets.len == expected_ticket_count
				assert anchor_ticket.native_identity == bind.target_identity
				assert anchor_ticket.owner_seed.target_generation == bind.target_generation
				assert anchor_ticket.required_parent_identity != 0
				assert anchor_wl_egl_ticket.required_parent_identity == anchor_wl_surface_ticket.native_identity
				assert anchor_wl_surface_ticket.required_parent_identity != 0
				window_target_generation := window_surface_ticket.owner_seed.target_generation
				assert window_target_generation != 0
				assert window_target_generation == window_egl_ticket.owner_seed.target_generation
				assert frame_matches == 0
					|| frame_ticket.owner_seed.target_generation == window_target_generation
				assert frame_matches == 0
					|| frame_ticket.required_parent_identity == window_egl_ticket.required_parent_identity
				mut index := native_assert_lifetime_release_at_for_test(snapshot, anchor_ticket,
					release_start)
				index = native_assert_lifetime_release_at_for_test(snapshot, anchor_wl_egl_ticket,
					index)
				native_assert_wayland_anchor_surface_release_mode_at_for_test(snapshot,
					anchor_wl_surface_ticket, index, wayland_anchor_release_protocol_destroy)
				index = native_assert_lifetime_release_at_for_test(snapshot,
					anchor_wl_surface_ticket, index)
				if frame_matches == 1 {
					index = native_assert_lifetime_release_at_for_test(snapshot, frame_ticket,
						index)
				}
				index = native_assert_lifetime_release_at_for_test(snapshot, window_surface_ticket,
					index)
				index = native_assert_lifetime_release_at_for_test(snapshot, window_egl_ticket,
					index)
				native_assert_successful_wayland_shutdown_flush_for_test(snapshot, index,
					wayland_display_identity, live_window, window_target_generation,

					unbind.ordinal + 2)
				index += 8
				index = native_assert_lifetime_release_at_for_test(snapshot, context_ticket, index)
				index = native_assert_lifetime_release_at_for_test(snapshot, display_ticket, index)
				index = native_assert_lifetime_release_at_for_test(snapshot, thread_ticket, index)
				assert index == snapshot.trace_len
			}
			else {
				assert false, 'normal EGL shutdown proof selected unsupported backend'
			}
		}
	}

	fn native_assert_phase_a_ticket_release_health_for_test(snapshot NativeAuthorityProofSnapshot, tickets []NativeLifetimeTicketProofSnapshot, expected NativeRendererHealth) {
		for ticket in tickets {
			if ticket.state != .bound {
				continue
			}
			start := native_lifetime_release_start_in_snapshot_for_test(snapshot, ticket.context)
			assert start >= 0
			assert start + 4 < snapshot.trace_len
			assert snapshot.trace[start + 4].milestone == .health_latched
			assert snapshot.trace[start + 4].health == expected
		}
	}

	fn native_runtime_proofs_requested_for_test() bool {
		return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
	}

	fn native_runtime_backend_for_test() !BackendKind {
		return match os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') {
			'x11' { .x11 }
			'wayland' { .wayland }
			'appkit' { .appkit }
			'win32' { .win32 }
			else { error('VGG_MULTIWINDOW_RUNTIME_BACKEND must select x11, wayland, appkit, or win32') }
		}
	}

	fn native_release_oracle_reset_for_test(backend BackendKind) {
		$if linux {
			$if x_multiwindow_x11 ? || sokol_wayland ? {
				if backend in [.x11, .wayland] {
					C.v_multiwindow_test_release_oracle_reset_sequence()
					C.v_multiwindow_test_egl_release_oracle_reset()
				}
			}
			$if sokol_wayland ? {
				if backend == .wayland {
					C.v_multiwindow_test_wayland_release_oracle_reset()
				}
			}
		}
	}

	fn native_release_oracle_snapshot_for_test(backend BackendKind) NativeReleaseOracleSnapshot {
		mut records := []NativeReleaseOracleRecord{}
		mut callback_completions := []NativeReleaseOracleRecord{}
		mut unticketed_releases := []NativeReleaseOracleRecord{}
		mut overflow := false
		$if linux {
			$if x_multiwindow_x11 ? || sokol_wayland ? {
				if backend in [.x11, .wayland] {
					count := C.v_multiwindow_test_egl_release_oracle_count()
					for raw_index in 0 .. int(count) {
						index := u64(raw_index)
						record := NativeReleaseOracleRecord{
							sequence:        C.v_multiwindow_test_egl_release_oracle_sequence(index)
							domain:          .egl
							kind:            C.v_multiwindow_test_egl_release_oracle_kind(index)
							identity:        C.v_multiwindow_test_egl_release_oracle_identity(index)
							parent_identity: C.v_multiwindow_test_egl_release_oracle_parent(index)
						}
						if record.kind in [u64(5), u64(6)] {
							unticketed_releases << NativeReleaseOracleRecord{
								...record
								domain: .x11
							}
						} else {
							assert record.kind in [u64(1), u64(2), u64(3), u64(4)]
							records << record
						}
					}
					overflow = C.v_multiwindow_test_egl_release_oracle_overflow() != 0
				}
			}
			$if sokol_wayland ? {
				if backend == .wayland {
					count := C.v_multiwindow_test_wayland_release_oracle_count()
					for raw_index in 0 .. int(count) {
						index := u64(raw_index)
						record := NativeReleaseOracleRecord{
							sequence: C.v_multiwindow_test_wayland_release_oracle_sequence(index)
							domain:   .wayland
							kind:     C.v_multiwindow_test_wayland_release_oracle_kind(index)
							identity: C.v_multiwindow_test_wayland_release_oracle_identity(index)
							mode:     C.v_multiwindow_test_wayland_release_oracle_mode(index)
						}
						if record.kind == 3 {
							callback_completions << record
						} else if record.kind in [u64(4), u64(5)] {
							unticketed_releases << record
						} else {
							assert record.kind in [u64(1), u64(2), u64(6)]
							records << record
						}
					}
					overflow = overflow
						|| C.v_multiwindow_test_wayland_release_oracle_overflow() != 0
				}
			}
		}
		for index in 1 .. records.len {
			mut cursor := index
			for cursor > 0 && records[cursor - 1].sequence > records[cursor].sequence {
				previous := records[cursor - 1]
				records[cursor - 1] = records[cursor]
				records[cursor] = previous
				cursor--
			}
		}
		for index in 1 .. callback_completions.len {
			mut cursor := index
			for cursor > 0
				&& callback_completions[cursor - 1].sequence > callback_completions[cursor].sequence {
				previous := callback_completions[cursor - 1]
				callback_completions[cursor - 1] = callback_completions[cursor]
				callback_completions[cursor] = previous
				cursor--
			}
		}
		for index in 1 .. unticketed_releases.len {
			mut cursor := index
			for cursor > 0
				&& unticketed_releases[cursor - 1].sequence > unticketed_releases[cursor].sequence {
				previous := unticketed_releases[cursor - 1]
				unticketed_releases[cursor - 1] = unticketed_releases[cursor]
				unticketed_releases[cursor] = previous
				cursor--
			}
		}
		mut sequences := []u64{cap: records.len + callback_completions.len + unticketed_releases.len}
		for record in records {
			sequences << record.sequence
		}
		for completion in callback_completions {
			sequences << completion.sequence
		}
		for release in unticketed_releases {
			sequences << release.sequence
		}
		for index in 1 .. sequences.len {
			mut cursor := index
			for cursor > 0 && sequences[cursor - 1] > sequences[cursor] {
				previous := sequences[cursor - 1]
				sequences[cursor - 1] = sequences[cursor]
				sequences[cursor] = previous
				cursor--
			}
		}
		for index, sequence in sequences {
			assert sequence == u64(index + 1)
		}
		return NativeReleaseOracleSnapshot{
			records:              records
			callback_completions: callback_completions
			unticketed_releases:  unticketed_releases
			overflow:             overflow
		}
	}

	fn native_release_oracle_assert_equal_for_test(expected NativeReleaseOracleSnapshot, actual NativeReleaseOracleSnapshot) {
		assert actual.overflow == expected.overflow
		assert actual.records.len == expected.records.len
		for index in 0 .. expected.records.len {
			assert actual.records[index] == expected.records[index]
		}
		assert actual.callback_completions.len == expected.callback_completions.len
		for index in 0 .. expected.callback_completions.len {
			assert actual.callback_completions[index] == expected.callback_completions[index]
		}
		assert actual.unticketed_releases.len == expected.unticketed_releases.len
		for index in 0 .. expected.unticketed_releases.len {
			assert actual.unticketed_releases[index] == expected.unticketed_releases[index]
		}
	}

	fn native_release_oracle_record_count_for_test(records []NativeReleaseOracleRecord, domain NativeReleaseOracleDomain, kind u64, identity u64) int {
		mut count := 0
		for record in records {
			if record.domain == domain && record.kind == kind && record.identity == identity {
				count++
			}
		}
		return count
	}

	fn native_release_oracle_record_sequence_for_test(records []NativeReleaseOracleRecord, domain NativeReleaseOracleDomain, kind u64, identity u64) u64 {
		mut sequence := u64(0)
		mut count := 0
		for record in records {
			if record.domain == domain && record.kind == kind && record.identity == identity {
				sequence = record.sequence
				count++
			}
		}
		assert count == 1
		assert sequence != 0
		return sequence
	}

	fn native_release_oracle_last_sequence_for_test(snapshot NativeReleaseOracleSnapshot) u64 {
		mut sequence := u64(0)
		for record in snapshot.records {
			if record.sequence > sequence {
				sequence = record.sequence
			}
		}
		for record in snapshot.callback_completions {
			if record.sequence > sequence {
				sequence = record.sequence
			}
		}
		for record in snapshot.unticketed_releases {
			if record.sequence > sequence {
				sequence = record.sequence
			}
		}
		return sequence
	}

	fn native_append_unique_identity_for_test(mut identities []u64, identity u64) {
		if identity != 0 && identity !in identities {
			identities << identity
		}
	}

	fn native_append_release_order_for_test(mut orders []NativeIdentityReleaseOrder, child u64, parent u64) {
		if child != 0 && parent != 0 && child != parent {
			orders << NativeIdentityReleaseOrder{
				child:  child
				parent: parent
			}
		}
	}

	fn native_wayland_local_release_proof_for_test(backend &WaylandBackend) NativeWaylandLocalReleaseProof {
		$if linux && sokol_wayland ? {
			mut identities := []u64{}
			mut orders := []NativeIdentityReleaseOrder{}
			registry := native_identity(backend.registry)
			display := native_identity(backend.display)
			compositor := native_identity(backend.compositor)
			for record in backend.windows {
				surface := native_identity(record.surface)
				xdg_surface := native_identity(record.xdg_surface)
				xdg_toplevel := native_identity(record.xdg_toplevel)
				decoration := native_identity(record.toplevel_decoration)
				callback := native_identity(record.frame_callback)
				native_append_unique_identity_for_test(mut identities, callback)
				for buffer in record.fallback_buffers {
					buffer_identity := native_identity(buffer)
					native_append_unique_identity_for_test(mut identities, buffer_identity)
					native_append_release_order_for_test(mut orders, buffer_identity, surface)
				}
				for identity in [decoration, xdg_toplevel, xdg_surface, surface] {
					native_append_unique_identity_for_test(mut identities, identity)
				}
				native_append_release_order_for_test(mut orders, callback, surface)
				native_append_release_order_for_test(mut orders, decoration, xdg_toplevel)
				native_append_release_order_for_test(mut orders, xdg_toplevel, xdg_surface)
				native_append_release_order_for_test(mut orders, xdg_surface, surface)
				native_append_release_order_for_test(mut orders, surface, compositor)
			}
			pending_alias := native_identity(backend.pending_drop_offer)
			for identity in [native_identity(backend.data_offer), pending_alias,
				native_identity(backend.cursor_shape_device),
				native_identity(backend.pointer), native_identity(backend.keyboard),
				native_identity(backend.touch), native_identity(backend.data_device),
				native_identity(backend.data_device_manager),
				native_identity(backend.cursor_shape_manager),
				native_identity(backend.decoration_manager), native_identity(backend.shm),
				native_identity(backend.wm_base), native_identity(backend.seat), compositor, registry] {
				native_append_unique_identity_for_test(mut identities, identity)
			}
			data_device := native_identity(backend.data_device)
			data_manager := native_identity(backend.data_device_manager)
			cursor_device := native_identity(backend.cursor_shape_device)
			cursor_manager := native_identity(backend.cursor_shape_manager)
			seat := native_identity(backend.seat)
			for offer in [native_identity(backend.data_offer), pending_alias] {
				native_append_release_order_for_test(mut orders, offer, data_device)
			}
			native_append_release_order_for_test(mut orders, data_device, data_manager)
			native_append_release_order_for_test(mut orders, cursor_device, cursor_manager)
			for child in [native_identity(backend.pointer), native_identity(backend.keyboard),
				native_identity(backend.touch)] {
				native_append_release_order_for_test(mut orders, child, seat)
			}
			for identity in identities {
				native_append_release_order_for_test(mut orders, identity, registry)
			}
			return NativeWaylandLocalReleaseProof{
				identities:      identities
				orders:          orders
				registry:        registry
				display:         display
				pending_alias:   pending_alias
				pending_drop_fd: backend.pending_drop_fd
			}
		} $else {
			_ = backend
			return NativeWaylandLocalReleaseProof{}
		}
	}

	fn native_assert_wayland_local_release_oracle_for_test(snapshot NativeReleaseOracleSnapshot, proof NativeWaylandLocalReleaseProof) {
		assert !snapshot.overflow
		assert proof.registry != 0
		assert proof.display != 0
		mut local_count := 0
		for record in snapshot.unticketed_releases {
			if record.domain == .wayland && record.kind == 4 {
				local_count++
			}
		}
		assert local_count == proof.identities.len
		for identity in proof.identities {
			assert native_release_oracle_record_count_for_test(snapshot.unticketed_releases,
				.wayland, 4, identity) == 1
		}
		assert native_release_oracle_record_count_for_test(snapshot.unticketed_releases, .wayland,
			5, proof.display) == 1
		disconnect_sequence := native_release_oracle_record_sequence_for_test(snapshot.unticketed_releases,
			.wayland, 5, proof.display)
		registry_sequence := native_release_oracle_record_sequence_for_test(snapshot.unticketed_releases,
			.wayland, 4, proof.registry)
		assert registry_sequence < disconnect_sequence
		assert disconnect_sequence == native_release_oracle_last_sequence_for_test(snapshot)
		for order in proof.orders {
			child_sequence := native_release_oracle_record_sequence_for_test(snapshot.unticketed_releases,
				.wayland, 4, order.child)
			parent_sequence := native_release_oracle_record_sequence_for_test(snapshot.unticketed_releases,
				.wayland, 4, order.parent)
			assert child_sequence < parent_sequence
		}
	}

	fn native_release_oracle_record_matches_ticket_for_test(record NativeReleaseOracleRecord, ticket NativeLifetimeTicketProofSnapshot) bool {
		mut domain := NativeReleaseOracleDomain.egl
		mut kind := u64(0)
		match ticket.release_kind {
			.egl_surface {
				kind = 1
			}
			.egl_context {
				kind = 2
			}
			.egl_display {
				kind = 3
			}
			.egl_thread {
				kind = 4
			}
			.wayland_egl_window {
				domain = .wayland
				kind = 1
			}
			.wayland_surface {
				domain = .wayland
				kind = 6
			}
			.wayland_frame_callback {
				domain = .wayland
				kind = 2
			}
			else {
				return false
			}
		}

		expected_parent := if domain == .egl { ticket.required_parent_identity } else { u64(0) }
		mode_matches := if ticket.release_kind == .wayland_surface {
			record.mode in [wayland_anchor_release_protocol_destroy,
				wayland_anchor_release_local_proxy_destroy]
		} else {
			record.mode == 0
		}
		return record.domain == domain && record.kind == kind
			&& record.identity == ticket.native_identity
			&& record.parent_identity == expected_parent && mode_matches
	}

	fn native_release_oracle_assert_cleanup_bijection_for_test(before NativeReleaseOracleSnapshot, after NativeReleaseOracleSnapshot, tickets []NativeLifetimeTicketProofSnapshot) {
		assert !before.overflow
		assert !after.overflow
		assert after.records.len >= before.records.len
		assert after.callback_completions.len >= before.callback_completions.len
		assert after.unticketed_releases.len >= before.unticketed_releases.len
		for index in 0 .. before.records.len {
			assert after.records[index] == before.records[index]
		}
		for index in 0 .. before.callback_completions.len {
			assert after.callback_completions[index] == before.callback_completions[index]
		}
		for index in 0 .. before.unticketed_releases.len {
			assert after.unticketed_releases[index] == before.unticketed_releases[index]
		}
		mut consumed := []bool{len: after.records.len - before.records.len}
		mut release_indices := map[u64]int{}
		for ticket in tickets {
			if ticket.state == .abandoned {
				assert ticket.domain == .egl
				assert ticket.release_kind != .egl_thread
				continue
			}
			assert ticket.state == .bound
			mut matched := -1
			for delta_index in 0 .. consumed.len {
				if consumed[delta_index] {
					continue
				}
				index := before.records.len + delta_index
				if native_release_oracle_record_matches_ticket_for_test(after.records[index],
					ticket)
				{
					assert matched == -1
					matched = delta_index
				}
			}
			assert matched >= 0
			consumed[matched] = true
			release_indices[ticket.ticket_id] = before.records.len + matched
		}
		for was_consumed in consumed {
			assert was_consumed
		}
		mut completed_ticket_ids := map[u64]bool{}
		mut completed_identities := map[u64]bool{}
		for completion_index in before.callback_completions.len .. after.callback_completions.len {
			completion := after.callback_completions[completion_index]
			assert completion.domain == .wayland
			assert completion.kind == 3
			assert completion.mode == 0
			assert completion.identity != 0
			mut matched_ticket := NativeLifetimeTicketProofSnapshot{}
			mut identity_matches := 0
			for ticket in tickets {
				if ticket.native_identity == completion.identity {
					identity_matches++
					matched_ticket = ticket
				}
			}
			assert identity_matches == 1
			assert matched_ticket.ticket_id != 0
			assert matched_ticket.state == .bound
			assert matched_ticket.domain == .wayland
			assert matched_ticket.release_kind == .wayland_frame_callback
			mut ticket_id_matches := 0
			for ticket in tickets {
				if ticket.ticket_id == matched_ticket.ticket_id {
					ticket_id_matches++
				}
			}
			assert ticket_id_matches == 1
			assert matched_ticket.ticket_id !in completed_ticket_ids
			assert completion.identity !in completed_identities
			assert matched_ticket.ticket_id in release_indices
			destroy_index := release_indices[matched_ticket.ticket_id]
			assert destroy_index >= before.records.len
			assert destroy_index < after.records.len
			destroy := after.records[destroy_index]
			assert destroy.domain == .wayland
			assert destroy.kind == 2
			assert destroy.mode == 0
			assert destroy.identity == completion.identity
			assert consumed[destroy_index - before.records.len]
			mut destroy_matches := 0
			for record_index in before.records.len .. after.records.len {
				record := after.records[record_index]
				if record.domain == .wayland && record.kind == 2
					&& record.identity == completion.identity {
					destroy_matches++
					assert record_index == destroy_index
				}
			}
			assert destroy_matches == 1
			assert destroy.sequence + 1 == completion.sequence
			completed_ticket_ids[matched_ticket.ticket_id] = true
			completed_identities[completion.identity] = true
		}
		for child in tickets {
			if child.state != .bound || child.required_parent_identity == 0 {
				continue
			}
			for parent in tickets {
				if parent.state == .bound
					&& parent.native_identity == child.required_parent_identity
					&& child.parent_authority_scope == parent.authority_scope
					&& child.parent_authority_token == parent.authority_token {
					assert release_indices[child.ticket_id] < release_indices[parent.ticket_id]
				}
			}
		}
		for callback in tickets {
			if callback.state != .bound || callback.release_kind != .wayland_frame_callback {
				continue
			}
			for child in tickets {
				if child.state == .bound && child.owner_seed.window == callback.owner_seed.window
					&& child.release_kind in [.egl_surface, .wayland_egl_window] {
					assert release_indices[callback.ticket_id] < release_indices[child.ticket_id]
				}
			}
		}
		for surface in tickets {
			if surface.state != .bound || surface.release_kind != .egl_surface
				|| surface.owner_seed.scope != .anchor {
				continue
			}
			for egl_window in tickets {
				if egl_window.state == .bound && egl_window.release_kind == .wayland_egl_window
					&& egl_window.owner_seed.scope == .anchor {
					assert release_indices[surface.ticket_id] < release_indices[egl_window.ticket_id]
				}
			}
		}
	}

	fn native_egl_runtime_requested_for_test() bool {
		if !native_runtime_proofs_requested_for_test() {
			return false
		}
		backend := native_runtime_backend_for_test() or { panic(err) }
		return backend in [.x11, .wayland]
	}

	fn native_wayland_runtime_requested_for_test() bool {
		if !native_runtime_proofs_requested_for_test() {
			return false
		}
		backend := native_runtime_backend_for_test() or { panic(err) }
		return backend == .wayland
	}

	fn native_dxgi_runtime_requested_for_test() bool {
		if !native_runtime_proofs_requested_for_test() {
			return false
		}
		backend := native_runtime_backend_for_test() or { panic(err) }
		if backend == .win32 {
			$if multiwindow_d3d11_warp ? || gg_multiwindow_d3d11_warp ? {
				return true
			} $else {
				panic('Win32 native primitive proofs require the forced-WARP compile define')
			}
		}
		return false
	}

	fn native_runtime_new_app_for_test(expected BackendKind) !&App {
		selected := native_runtime_backend_for_test()!
		if selected != expected {
			return error('native proof selected `${selected}`, expected `${expected}`')
		}
		native_require_parent_watchdog_gate_for_test()!
		native_release_oracle_reset_for_test(selected)
		mut app := new_app(
			backend:          selected
			queue_size:       16
			require_renderer: true
		)!
		caps := app.capabilities()
		backend_ready := match selected {
			.x11 { caps.x11 && caps.gl }
			.wayland { caps.wayland && caps.gl }
			.appkit { caps.metal }
			.win32 { caps.win32 && caps.d3d11 }
			else { false }
		}

		if caps.backend != selected || !caps.native || !caps.multi_window
			|| !caps.explicit_swapchain || !backend_ready {
			message := 'selected native proof backend `${selected}` is unavailable: ${caps}'
			app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
			return error(message)
		}
		app.start_renderer(RendererConfig{}) or {
			start_error := err.msg()
			app.stop() or { return error('${start_error}; cleanup failed: ${err.msg()}') }
			return error(start_error)
		}
		if !app.renderer_is_usable() {
			app.stop() or {
				return error('native renderer is unusable; cleanup failed: ${err.msg()}')
			}
			return error('native renderer is unusable after successful start')
		}
		return app
	}

	fn native_require_parent_watchdog_gate_for_test() ! {
		if os.getenv(multiwindow_probe_gate.environment_name) == '' {
			return error('native proof requires the parent process-tree watchdog start gate')
		}
		multiwindow_probe_gate.await_parent_release(2 * time.second)!
	}

	fn native_appkit_admit_window_for_test(app &App, window WindowId) ! {
		$if darwin {
			app.assert_owner_thread()!
			index := native_appkit_window_record_index_for_test(app, window)!
			state := app.backend.appkit.windows[index].state
			if state == unsafe { nil } {
				return error('AppKit native proof requires owner main thread and a live, visible, non-miniaturized native window')
			}
			result := C.v_multiwindow_test_appkit_admit_window(state)
			if result != 1 && result != 2 {
				return error('AppKit native proof requires owner main thread and a live, visible, non-miniaturized native window')
			}
			return
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_runtime_new_window_for_test(mut app App, title string) !WindowId {
		window := app.create_window(
			title:           title
			width:           192
			height:          128
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		if app.backend.kind == .appkit {
			native_appkit_admit_window_for_test(app, window)!
			for _ in 0 .. 128 {
				app.poll_events()!
				if app.render_window_eligible(window)! {
					return window
				}
			}
			assert app.render_window_eligible(window)!, 'AppKit window did not become render eligible after bounded owner polling'
			return window
		}
		deadline := time.now().add(2 * time.second)
		for {
			app.poll_events() or {
				if err.msg() != err_app_stopped {
					return err
				}
				return error('native app stopped before window became render eligible')
			}
			if app.render_window_eligible(window)! {
				return window
			}
			if time.now() >= deadline {
				break
			}
		}
		return error('selected native backend did not make window `${title}` render eligible')
	}

	fn native_egl_target_for_test(app &App, id WindowId) !NativeEglTargetProof {
		match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					index := app.backend.x11.window_record_index(id) or {
						return error(err_window_not_found)
					}
					record := app.backend.x11.windows[index]
					return NativeEglTargetProof{
						generation:       record.render_target_generation
						identity:         native_identity(record.egl_surface)
						ticket_id:        record.egl_surface_ticket
						native_destroyed: record.native_destroyed
						frame_ready:      true
					}
				} $else {
					return error(err_backend_unsupported)
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					index := app.backend.wayland.window_record_index(id) or {
						return error(err_window_not_found)
					}
					record := app.backend.wayland.windows[index]
					return NativeEglTargetProof{
						generation:       record.render_target_generation
						identity:         native_identity(record.egl_surface)
						ticket_id:        record.egl_surface_ticket
						native_destroyed: record.native_destroyed
						frame_ready:      record.frame_ready
					}
				} $else {
					return error(err_backend_unsupported)
				}
			}
			else {
				return error('backend `${app.backend.kind}` has no EGL window target')
			}
		}
	}

	fn native_egl_binding_for_test(app &App) !NativeEglBindingProof {
		match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					return NativeEglBindingProof{
						binding:       app.backend.x11.egl_binding
						anchor:        NativeEglTargetProof{
							generation: app.backend.x11.anchor_generation
							identity:   native_identity(app.backend.x11.anchor_surface)
							ticket_id:  app.backend.x11.anchor_surface_ticket
						}
						context:       native_identity(app.backend.x11.egl_context)
						recovery_used: app.backend.x11.egl_bad_current_recovery_used
						health:        app.backend.x11.render_health
					}
				} $else {
					return error(err_backend_unsupported)
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					return NativeEglBindingProof{
						binding:       app.backend.wayland.egl_binding
						anchor:        NativeEglTargetProof{
							generation: app.backend.wayland.anchor_generation
							identity:   native_identity(app.backend.wayland.anchor_surface)
							ticket_id:  app.backend.wayland.anchor_surface_ticket
						}
						context:       native_identity(app.backend.wayland.egl_context)
						recovery_used: app.backend.wayland.egl_bad_current_recovery_used
						health:        app.backend.wayland.render_health
					}
				} $else {
					return error(err_backend_unsupported)
				}
			}
			else {
				return error('backend `${app.backend.kind}` has no EGL binding')
			}
		}
	}

	fn native_egl_display_identity_for_test(app &App) u64 {
		return match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					native_identity(app.backend.x11.egl_display)
				} $else {
					u64(0)
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					native_identity(app.backend.wayland.egl_display)
				} $else {
					u64(0)
				}
			}
			else {
				u64(0)
			}
		}
	}

	fn native_wayland_egl_window_identity_for_test(app &App, id WindowId) u64 {
		$if linux && sokol_wayland ? {
			if app.backend.kind == .wayland {
				index := app.backend.wayland.window_record_index(id) or { return 0 }
				return native_identity(app.backend.wayland.windows[index].wl_egl_window)
			}
		}
		return 0
	}

	fn native_wayland_egl_window_ticket_for_test(app &App, id WindowId) u64 {
		$if linux && sokol_wayland ? {
			if app.backend.kind == .wayland {
				index := app.backend.wayland.window_record_index(id) or { return 0 }
				return app.backend.wayland.windows[index].wl_egl_window_ticket
			}
		}
		return 0
	}

	fn native_wayland_surface_identity_for_test(app &App, id WindowId) u64 {
		$if linux && sokol_wayland ? {
			if app.backend.kind == .wayland {
				index := app.backend.wayland.window_record_index(id) or { return 0 }
				return native_identity(app.backend.wayland.windows[index].surface)
			}
		}
		return 0
	}

	fn native_egl_frame_context_for_test(app &App, lease RenderTargetLease, call_site NativeRenderCallSite, operation NativeRenderOperation, target NativeEglTargetProof, ordinal u64) NativeOperationContext {
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          native_context_window_target_fields | native_context_has_target_identity
			domain:                 .egl
			operation:              operation
			call_site:              call_site
			scope:                  .window_target
			window:                 lease.window
			target_generation:      target.generation
			target_identity:        target.identity
			batch_epoch:            lease.batch_epoch
			window_lease_epoch:     lease.window_epoch
			target_lease_epoch:     lease.target_epoch
			ordinal:                ordinal
		}
	}

	fn native_egl_error_context_for_test(primary NativeOperationContext) NativeOperationContext {
		return NativeOperationContext{
			...primary
			presence_mask:   primary.presence_mask & ~native_context_has_target_identity
			operation:       .egl_error_query
			target_identity: 0
			ordinal:         primary.ordinal + 1
		}
	}

	fn native_wayland_display_error_context_for_test(app &App, ordinal u64) NativeOperationContext {
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          native_context_has_target_identity
			domain:                 .wayland
			operation:              .wayland_display_error_query
			call_site:              .display_transport
			scope:                  .renderer
			target_identity:        native_wayland_display_identity_for_test(app)
			ordinal:                ordinal
		}
	}

	fn native_arm_wayland_primitive_for_test(mut app App, context NativeOperationContext, evidence NativePrimitiveEvidence, display_error i64) ! {
		if !app.backend.native_operations.arm(context, evidence) {
			return error('could not arm Wayland primary context `${context}`')
		}
		query := native_wayland_display_error_context_for_test(app, context.ordinal + 1)
		if !app.backend.native_operations.arm(query, NativePrimitiveEvidence{
			valid_mask:            native_valid_wayland_display_error
			wayland_display_error: display_error
		}) {
			return error('could not arm Wayland display-error context `${query}`')
		}
	}

	fn native_arm_egl_failure_for_test(mut authority NativeOperationAuthority, context NativeOperationContext, egl_error i64) ! {
		if !authority.arm(context, NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value
			return_value: 0
		}) {
			return error('could not arm EGL primary context `${context}`')
		}
		query := native_egl_error_context_for_test(context)
		if !authority.arm(query, NativePrimitiveEvidence{
			valid_mask: native_valid_egl_error
			egl_error:  egl_error
		}) {
			return error('could not arm EGL error context `${query}`')
		}
	}

	fn native_assert_sokol_sequence_for_test(snapshot multiwindow_sokol_trace.TypedSnapshot, expected []multiwindow_sokol_trace.Operation) {
		assert !snapshot.overflow
		assert snapshot.install_generation != 0
		assert snapshot.count == usize(snapshot.records.len)
		assert snapshot.records.len == expected.len
		for index, operation in expected {
			record := snapshot.records[index]
			assert record.operation == operation
			assert record.sequence == u64(index + 1)
			if operation == .begin_swapchain_pass {
				assert record.width > 0
				assert record.height > 0
				assert record.sample_count > 0
			}
		}
	}

	fn native_install_sokol_trace_for_test() !u64 {
		first := multiwindow_sokol_trace.install_generation()!
		if first == 0 {
			message := 'Sokol trace install returned generation 0'
			_ = multiwindow_sokol_trace.try_uninstall_generation(first)
			return error(message)
		}
		first_active := multiwindow_sokol_trace.active_generation()
		if first_active != first {
			message := 'Sokol trace active generation `${first_active}` does not match installed generation `${first}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(first)
			return error(message)
		}
		multiwindow_sokol_trace.uninstall_generation(first) or {
			_ = multiwindow_sokol_trace.try_uninstall_generation(first)
			return err
		}
		after_first_uninstall := multiwindow_sokol_trace.active_generation()
		if after_first_uninstall != 0 {
			message := 'Sokol trace first uninstall left active generation `${after_first_uninstall}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(first)
			return error(message)
		}
		second := multiwindow_sokol_trace.install_generation()!
		if second <= first {
			message := 'Sokol trace generation did not advance: first `${first}`, second `${second}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(second)
			return error(message)
		}
		second_active := multiwindow_sokol_trace.active_generation()
		if second_active != second {
			message := 'Sokol trace active generation `${second_active}` does not match installed generation `${second}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(second)
			return error(message)
		}
		if multiwindow_sokol_trace.try_uninstall_generation(first) {
			message := 'Sokol trace stale generation `${first}` unexpectedly uninstalled active generation `${second}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(second)
			return error(message)
		}
		after_stale_rejection := multiwindow_sokol_trace.active_generation()
		if after_stale_rejection != second {
			message := 'Sokol trace stale-generation rejection changed active generation from `${second}` to `${after_stale_rejection}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(second)
			return error(message)
		}
		multiwindow_sokol_trace.reset()
		return second
	}

	fn native_uninstall_sokol_trace_for_test(generation u64) ! {
		if generation == 0 {
			return error('cannot uninstall Sokol trace generation 0')
		}
		active := multiwindow_sokol_trace.active_generation()
		if active != generation {
			message := 'Sokol trace active generation `${active}` does not match owned generation `${generation}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(generation)
			return error(message)
		}
		multiwindow_sokol_trace.uninstall_generation(generation) or {
			_ = multiwindow_sokol_trace.try_uninstall_generation(generation)
			return err
		}
		after_uninstall := multiwindow_sokol_trace.active_generation()
		if after_uninstall != 0 {
			message := 'Sokol trace uninstall left active generation `${after_uninstall}`'
			_ = multiwindow_sokol_trace.try_uninstall_generation(generation)
			return error(message)
		}
	}

	fn native_sokol_swapchain_record_for_test(snapshot multiwindow_sokol_trace.TypedSnapshot) multiwindow_sokol_trace.Record {
		assert !snapshot.overflow
		assert snapshot.install_generation != 0
		assert snapshot.count == usize(snapshot.records.len)
		mut result := multiwindow_sokol_trace.Record{}
		mut matches := 0
		for record in snapshot.records {
			if record.operation != .begin_swapchain_pass {
				continue
			}
			matches++
			result = record
		}
		assert matches == 1
		return result
	}

	fn native_assert_sokol_lease_for_test(app &App, lease RenderTargetLease, target RenderWindowSnapshot, record multiwindow_sokol_trace.Record) {
		assert lease.app_instance == app.instance_id
		assert lease.app_instance != 0
		assert lease.window == target.window
		assert lease.window.app_instance == app.instance_id
		assert lease.window.generation != 0
		assert lease.batch_epoch != 0
		assert lease.batch_epoch == target.batch_epoch
		assert lease.window_epoch != 0
		assert lease.target_epoch != 0
		assert target.target.target_identity != 0
		assert record.width == target.metrics.framebuffer_width
		assert record.height == target.metrics.framebuffer_height
		assert record.sample_count == target.target.sample_count
		assert record.color_format == target.target.color_format
		assert record.depth_format == target.target.depth_format
	}

	fn native_context_matches_sokol_lease_for_test(context NativeOperationContext, app &App, lease RenderTargetLease, target RenderWindowSnapshot, domain NativeRenderDomain, operation NativeRenderOperation, has_target_identity bool) bool {
		expected_presence := if has_target_identity {
			native_context_window_target_fields | native_context_has_target_identity
		} else {
			native_context_window_target_fields
		}
		return
			context.renderer_attempt_token == app.backend.native_operations.renderer_attempt_token
			&& context.renderer_attempt_token != 0 && context.app_identity == app.instance_id
			&& context.presence_mask == expected_presence && context.domain == domain
			&& context.operation == operation && context.call_site == .window_activate
			&& context.scope == .window_target && context.window == lease.window
			&& context.window == target.window
			&& context.target_generation == target.target.target_identity
			&& (context.target_identity != 0) == has_target_identity
			&& context.batch_epoch == lease.batch_epoch
			&& context.window_lease_epoch == lease.window_epoch
			&& context.target_lease_epoch == lease.target_epoch && context.ordinal != 0
	}

	fn native_find_sokol_boundary_context_for_test(app &App, lease RenderTargetLease, target RenderWindowSnapshot, domain NativeRenderDomain, operation NativeRenderOperation, has_target_identity bool) NativeOperationContext {
		assert app.backend.native_operations.proof != unsafe { nil }
		assert !app.backend.native_operations.proof.trace_overflow
		assert app.backend.native_operations.proof.trace_len <= native_operation_trace_capacity
		mut result := NativeOperationContext{}
		mut matches := 0
		for index in 0 .. app.backend.native_operations.proof.trace_len {
			entry := app.backend.native_operations.proof.trace[index]
			if entry.milestone != .real_call
				|| !native_context_matches_sokol_lease_for_test(entry.context, app, lease, target, domain, operation, has_target_identity) {
				continue
			}
			matches++
			result = entry.context
		}
		assert matches == 1
		return result
	}

	fn native_assert_successful_boundary_trace_for_test(authority &NativeOperationAuthority, context NativeOperationContext) NativePrimitiveCapture {
		assert authority.proof != unsafe { nil }
		assert !authority.proof.trace_overflow
		assert authority.proof.trace_len <= native_operation_trace_capacity
		mut start := -1
		mut matches := 0
		for index in 0 .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if entry.milestone == .real_call
				&& native_operation_contexts_identical(entry.context, context) {
				matches++
				start = index
			}
		}
		assert matches == 1
		assert start >= 0
		assert start + 4 < authority.proof.trace_len
		expected := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for offset, milestone in expected {
			entry := authority.proof.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		actual := authority.proof.trace[start + 1].actual
		effective := authority.proof.trace[start + 2].effective
		assert native_proof_evidence_equal(actual, effective)
		accepted := authority.proof.trace[start + 3]
		assert accepted.local_validation == .none
		assert accepted.result.succeeded()
		assert authority.proof.trace[start + 4].health == .ready
		return NativePrimitiveCapture{
			actual:    actual
			effective: effective
		}
	}

	fn native_assert_egl_sokol_binding_for_test(app &App, record multiwindow_sokol_trace.Record, lease RenderTargetLease, target RenderWindowSnapshot) ! {
		primary := native_find_sokol_boundary_context_for_test(app, lease, target, .egl,
			.make_current, true)
		primary_capture := native_assert_successful_boundary_trace_for_test(&app.backend.native_operations,
			primary)
		assert primary_capture.actual.has(native_valid_return_value)
		assert primary_capture.actual.return_value == 1
		assert primary.target_identity != 0
		mut context_identity := u64(0)
		for operation in [NativeRenderOperation.current_draw_query, .current_read_query,
			.current_context_query] {
			query := native_find_sokol_boundary_context_for_test(app, lease, target, .egl,
				operation, false)
			capture := native_assert_successful_boundary_trace_for_test(&app.backend.native_operations,
				query)
			assert capture.actual.has(native_valid_handle)
			assert capture.actual.handle != 0
			if operation in [.current_draw_query, .current_read_query] {
				assert capture.actual.handle == primary.target_identity
			} else {
				context_identity = capture.actual.handle
			}
		}
		backend_binding := native_egl_binding_for_test(app)!
		assert context_identity == backend_binding.context
		assert context_identity != 0
		// GL swapchain passes use framebuffer zero; the exact EGL surface authority
		// is the successful make-current plus draw/read/context verification above.
		assert record.swapchain_identity == 0
	}

	fn native_assert_metal_sokol_attachment_for_test(app &App, record multiwindow_sokol_trace.Record, lease RenderTargetLease, target RenderWindowSnapshot) ! {
		$if darwin {
			state := app.render_backend_state()!
			target_index := validate_target_lease(state, app.instance_id, lease)!
			target_slot := state.targets[target_index]
			index := app.backend.appkit.window_record_index(lease.window) or {
				return error(err_window_not_found)
			}
			window_record := app.backend.appkit.windows[index]
			context := native_find_sokol_boundary_context_for_test(app, lease, target, .metal,
				.drawable_acquire, true)
			capture := native_assert_successful_boundary_trace_for_test(&app.backend.native_operations,
				context)
			assert window_record.id == lease.window
			assert window_record.render_target_generation == target.target.target_identity
			assert context.target_identity == native_identity(window_record.state)
			assert context.target_identity != 0
			assert target_slot.status == .acquired
			assert target_slot.lease == lease
			assert target_slot.frame.window_id == lease.window
			assert target_slot.frame.batch_epoch == lease.batch_epoch
			assert target_slot.frame.window_lease_epoch == lease.window_epoch
			assert target_slot.frame.target_lease_epoch == lease.target_epoch
			assert target_slot.frame.target.target_identity == target.target.target_identity
			assert capture.actual.has(native_valid_handle)
			assert capture.actual.handle != 0
			assert capture.actual.object_identity_0 != 0
			assert capture.actual.observed_count == u64(target.metrics.framebuffer_width)
			assert capture.actual.selected_value == i64(target.metrics.framebuffer_height)
			assert native_identity(window_record.active_drawable) == capture.actual.handle
			assert native_identity(target_slot.frame.swapchain.metal.current_drawable) == capture.actual.handle
			assert record.swapchain_identity == capture.actual.handle
			return
		} $else {
			_ = app
			_ = record
			_ = lease
			_ = target
			return error(err_backend_unsupported)
		}
	}

	fn native_appkit_window_record_index_for_test(app &App, window WindowId) !int {
		$if darwin {
			return app.backend.appkit.window_record_index(window) or {
				return error(err_window_not_found)
			}
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_assert_d3d11_sokol_attachment_for_test(app &App, record multiwindow_sokol_trace.Record, lease RenderTargetLease, target RenderWindowSnapshot) ! {
		$if windows && sokol_d3d11 ? {
			state := app.render_backend_state()!
			target_index := validate_target_lease(state, app.instance_id, lease)!
			target_slot := state.targets[target_index]
			index := app.backend.win32.window_record_index(lease.window) or {
				return error(err_window_not_found)
			}
			window_record := app.backend.win32.windows[index]
			assert window_record.id == lease.window
			assert window_record.render_target_generation == target.target.target_identity
			assert target_slot.status == .acquired
			assert target_slot.lease == lease
			assert target_slot.frame.window_id == lease.window
			assert target_slot.frame.batch_epoch == lease.batch_epoch
			assert target_slot.frame.window_lease_epoch == lease.window_epoch
			assert target_slot.frame.target_lease_epoch == lease.target_epoch
			assert target_slot.frame.target.target_identity == target.target.target_identity
			assert window_record.render_view != unsafe { nil }
			assert target_slot.frame.swapchain.d3d11.render_view == window_record.render_view
			assert record.swapchain_identity == native_identity(window_record.render_view)
			return
		} $else {
			_ = app
			_ = record
			_ = lease
			_ = target
			return error(err_backend_unsupported)
		}
	}

	fn native_assert_authoritative_sokol_swapchain_for_test(app &App, snapshot multiwindow_sokol_trace.TypedSnapshot, lease RenderTargetLease, target RenderWindowSnapshot) ! {
		record := native_sokol_swapchain_record_for_test(snapshot)
		native_assert_sokol_lease_for_test(app, lease, target, record)
		match app.backend.kind {
			.x11, .wayland {
				native_assert_egl_sokol_binding_for_test(app, record, lease, target)!
			}
			.appkit {
				native_assert_metal_sokol_attachment_for_test(app, record, lease, target)!
			}
			.win32 {
				native_assert_d3d11_sokol_attachment_for_test(app, record, lease, target)!
			}
			else {
				return error('backend `${app.backend.kind}` has no authoritative swapchain proof')
			}
		}
	}

	fn native_exercise_sokol_swapchain_identity_for_test() ! {
		backend := native_runtime_backend_for_test()!
		match backend {
			.appkit { native_appkit_lifetime_oracle_reset_for_test() }
			else {}
		}

		mut app := native_runtime_new_app_for_test(backend)!
		window := native_runtime_new_window_for_test(mut app,
			'native authoritative swapchain identity')!
		assert app.backend.native_operations.arm_proof()
		if backend == .win32 {
			native_win32_lifetime_oracle_reset_for_test()
		}
		sokol_generation := native_install_sokol_trace_for_test()!
		mut lease := RenderTargetLease{}
		mut pass_snapshot := RenderWindowSnapshot{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut lease, mut pass_snapshot] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			lease = acquisition.lease
			pass_snapshot = acquisition.snapshot
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut app, lease, pass_snapshot] () ! {
				native_assert_authoritative_sokol_swapchain_for_test(app,
					multiwindow_sokol_trace.typed_snapshot(), lease, pass_snapshot)!
			})!
		})!
		assert outcome.error == ''
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 1
		sokol := multiwindow_sokol_trace.typed_snapshot()
		native_assert_sokol_sequence_for_test(sokol, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		native_uninstall_sokol_trace_for_test(sokol_generation)!
		cleanup_tickets := native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
		before_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		app.stop()!
		after_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		stopped_proof := native_proof_snapshot(&app.backend.native_operations)
		match app.backend.kind {
			.x11, .wayland {
				native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
					after_release_oracle, cleanup_tickets)
			}
			.appkit {
				native_appkit_assert_release_bijection_for_test(stopped_proof,
					native_appkit_lifetime_oracle_snapshot_for_test())
			}
			.win32 {
				native_win32_assert_release_bijection_for_test(stopped_proof,
					native_win32_lifetime_oracle_snapshot_for_test())
			}
			else {}
		}

		assert app.backend.native_operations.disarm_proof()
	}

	fn native_assert_proof_snapshot_drained_for_test(snapshot NativeAuthorityProofSnapshot) {
		assert !snapshot.trace_overflow
		for entry in snapshot.plan {
			assert !entry.armed
			assert !entry.listener_registration_pending
		}
		native_proof_assert_invalid_trace_tail(snapshot)
	}

	fn native_assert_proof_drained_for_test(authority &NativeOperationAuthority) {
		assert authority.proof != unsafe { nil }
		native_assert_proof_snapshot_drained_for_test(native_proof_snapshot(authority))
	}

	fn native_assert_dxgi_object_release_staged_trace_for_test(snapshot NativeAuthorityProofSnapshot, acceptance_index int) {
		assert acceptance_index >= 3
		assert acceptance_index + 2 < snapshot.trace_len
		start := acceptance_index - 3
		expected := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.authority_release,
			.health_latched,
		]
		context := snapshot.trace[acceptance_index].context
		assert context.domain == .dxgi
		assert context.operation == .object_release
		for offset, milestone in expected {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		actual := snapshot.trace[start + 1]
		effective := snapshot.trace[start + 2]
		acceptance := snapshot.trace[acceptance_index]
		release := snapshot.trace[acceptance_index + 1]
		health := snapshot.trace[acceptance_index + 2]
		assert native_proof_evidence_equal(actual.actual, acceptance.actual)
		assert native_proof_evidence_equal(actual.actual, acceptance.result.actual_primitive)
		assert native_proof_evidence_equal(actual.effective, NativePrimitiveEvidence{})
		assert native_proof_evidence_equal(effective.actual, NativePrimitiveEvidence{})
		assert native_proof_evidence_equal(effective.effective, acceptance.effective)
		assert native_proof_evidence_equal(effective.effective, acceptance.result.primitive)
		assert native_proof_evidence_equal(release.actual, acceptance.actual)
		assert native_proof_evidence_equal(release.effective, acceptance.effective)
		assert release.local_validation == acceptance.local_validation
		assert native_proof_result_equal(release.result, acceptance.result)
		assert health.health != .uninitialized
	}

	fn native_assert_complete_native_trace_for_test(snapshot NativeAuthorityProofSnapshot) {
		assert !snapshot.trace_overflow
		mut secondary_health_sequences := 0
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			match entry.milestone {
				.real_call {
					assert index + 2 < snapshot.trace_len
					assert snapshot.trace[index + 1].milestone == .actual_primitive
					assert snapshot.trace[index + 2].milestone == .effective_primitive
					assert native_operation_contexts_identical(snapshot.trace[index + 1].context,
						entry.context)
					assert native_operation_contexts_identical(snapshot.trace[index + 2].context,
						entry.context)
				}
				.actual_primitive {
					assert index > 0 && index + 1 < snapshot.trace_len
					assert snapshot.trace[index - 1].milestone == .real_call
					assert snapshot.trace[index + 1].milestone == .effective_primitive
					assert native_operation_contexts_identical(snapshot.trace[index - 1].context,
						entry.context)
					assert native_operation_contexts_identical(snapshot.trace[index + 1].context,
						entry.context)
				}
				.effective_primitive {
					assert index >= 2
					assert snapshot.trace[index - 2].milestone == .real_call
					assert snapshot.trace[index - 1].milestone == .actual_primitive
					assert native_operation_contexts_identical(snapshot.trace[index - 2].context,
						entry.context)
					assert native_operation_contexts_identical(snapshot.trace[index - 1].context,
						entry.context)
				}
				.acceptance {
					if entry.context.domain == .dxgi && entry.context.operation == .object_release {
						native_assert_dxgi_object_release_staged_trace_for_test(snapshot, index)
					} else {
						assert index + 1 < snapshot.trace_len
						assert snapshot.trace[index + 1].milestone == .health_latched
						assert native_operation_contexts_identical(snapshot.trace[index + 1].context,
							entry.context)
					}
					mut effective_count := 0
					for prior in 0 .. index {
						candidate := snapshot.trace[prior]
						if candidate.milestone == .effective_primitive
							&& native_operation_contexts_identical(candidate.context, entry.context) {
							effective_count++
						}
					}
					assert effective_count == 1
				}
				.health_latched {
					if entry.context.domain == .dxgi && entry.context.operation == .object_release {
						assert index >= 2
						native_assert_dxgi_object_release_staged_trace_for_test(snapshot, index - 2)
					} else {
						assert index > 0
						previous := snapshot.trace[index - 1]
						if previous.milestone == .acceptance {
							assert native_operation_contexts_identical(previous.context,
								entry.context)
							if index + 1 < snapshot.trace_len
								&& snapshot.trace[index + 1].milestone == .health_latched {
								next := snapshot.trace[index + 1]
								assert native_operation_contexts_identical(next.context,
									entry.context)
								assert previous.context.domain == .egl
								assert previous.context.operation == .make_current
								assert previous.result.native_code == 0x3007
								assert previous.result.disposition == .target_lost
								assert entry.health == .ready
								assert next.health == .unavailable
								mut health_count := 0
								for candidate_index in 0 .. snapshot.trace_len {
									candidate := snapshot.trace[candidate_index]
									if candidate.milestone == .health_latched
										&& native_operation_contexts_identical(candidate.context, entry.context) {
										health_count++
									}
								}
								assert health_count == 2
								secondary_health_sequences++
								assert secondary_health_sequences == 1
							}
						} else {
							assert previous.milestone == .health_latched
							assert index >= 2
							accepted := snapshot.trace[index - 2]
							assert accepted.milestone == .acceptance
							assert native_operation_contexts_identical(accepted.context,
								previous.context)
							assert native_operation_contexts_identical(previous.context,
								entry.context)
							assert accepted.context.domain == .egl
							assert accepted.context.operation == .make_current
							assert accepted.result.native_code == 0x3007
							assert accepted.result.disposition == .target_lost
							assert previous.health == .ready
							assert entry.health == .unavailable
							mut health_count := 0
							for candidate_index in 0 .. snapshot.trace_len {
								candidate := snapshot.trace[candidate_index]
								if candidate.milestone == .health_latched
									&& native_operation_contexts_identical(candidate.context, entry.context) {
									health_count++
								}
							}
							assert health_count == 2
							assert secondary_health_sequences == 1
						}
					}
				}
				.authority_release {
					if entry.context.domain == .dxgi && entry.context.operation == .object_release {
						assert index > 0
						native_assert_dxgi_object_release_staged_trace_for_test(snapshot, index - 1)
					} else {
						assert index >= 2
						assert snapshot.trace[index - 2].milestone == .acceptance
						assert snapshot.trace[index - 1].milestone == .health_latched
						assert native_operation_contexts_identical(snapshot.trace[index - 2].context,
							snapshot.trace[index - 1].context)
						assert native_operation_contexts_identical(snapshot.trace[index - 1].context,
							entry.context)
					}
				}
			}
		}
		native_proof_assert_invalid_trace_tail(snapshot)
	}

	fn native_assert_egl_failure_capture_for_test(authority &NativeOperationAuthority, primary NativeOperationContext, egl_error i64) {
		assert authority.proof != unsafe { nil }
		native_assert_egl_failure_capture_in_snapshot_for_test(native_proof_snapshot(authority),
			primary, egl_error)
	}

	fn native_assert_egl_failure_capture_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, primary NativeOperationContext, egl_error i64) {
		query := native_egl_error_context_for_test(primary)
		mut start := -1
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .real_call
				&& native_operation_contexts_identical(entry.context, primary) {
				start = index
				break
			}
		}
		assert start >= 0
		assert start + 7 < snapshot.trace_len
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for offset, milestone in expected_milestones {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			if offset < 3 || offset >= 6 {
				assert native_operation_contexts_identical(entry.context, primary)
			} else {
				assert native_operation_contexts_identical(entry.context, query)
			}
		}
		assert snapshot.trace[start + 1].actual.return_value == 1
		assert snapshot.trace[start + 2].effective.return_value == 0
		assert snapshot.trace[start + 4].actual.egl_error == 0x3000
		assert snapshot.trace[start + 5].effective.egl_error == egl_error
		accepted := snapshot.trace[start + 6].result
		expected_disposition := if egl_error == 0x300b {
			NativeRenderDisposition.native_window_lost
		} else if egl_error == 0x300e {
			NativeRenderDisposition.renderer_lost
		} else if egl_error == 0x3008 {
			NativeRenderDisposition.renderer_unavailable
		} else {
			NativeRenderDisposition.target_lost
		}
		assert accepted.native_code == egl_error
		assert accepted.disposition == expected_disposition
	}

	fn native_stop_twice_with_exact_proof_for_test(mut app App) ! {
		cleanup_tickets := native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
		before_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		app.stop() or { return err }
		first_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		first := native_proof_snapshot(&app.backend.native_operations)
		first_ownership := native_phase_a_backend_ownership_snapshot(app)
		first_status := app.status()
		first_reason := app.stop_terminal
		app.stop() or {
			if err.msg() != first_reason {
				return error('replayed stop returned `${err.msg()}`, expected `${first_reason}`')
			}
		}
		second := native_proof_snapshot(&app.backend.native_operations)
		second_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		second_ownership := native_phase_a_backend_ownership_snapshot(app)
		assert first_status == .stopped
		assert app.status() == .stopped
		assert app.stop_terminal == first_reason
		assert !first.trace_overflow
		for entry in first.plan {
			assert !entry.armed
		}
		native_assert_complete_native_trace_for_test(first)
		native_proof_assert_snapshots_equal(first, second)
		native_phase_a_backend_assert_snapshots_equal(first_ownership, second_ownership)
		native_release_oracle_assert_equal_for_test(first_release_oracle, second_release_oracle)
		if app.backend.kind in [.x11, .wayland] {
			native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
				first_release_oracle, cleanup_tickets)
		}
		assert first.registry.tickets.len == 0
		assert app.backend.native_operations.disarm_proof()
	}

	fn native_assert_terminal_causes_once_for_test(message string) {
		prefix := '${err_render_terminal_aggregate}: '
		assert message.starts_with(prefix)
		causes := message.all_after(prefix).split('; ')
		assert causes.len > 0
		for cause in causes {
			assert cause != ''
			assert causes.count(it == cause) == 1
		}
	}

	fn native_stop_twice_with_any_proof_for_test(mut app App) !(string, NativeAuthorityProofSnapshot) {
		cleanup_tickets := native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
		before_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		mut first_error := ''
		app.stop() or { first_error = err.msg() }
		first_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		first := native_proof_snapshot(&app.backend.native_operations)
		first_ownership := native_phase_a_backend_ownership_snapshot(app)
		first_reason := app.stop_terminal
		mut replay_error := ''
		app.stop() or { replay_error = err.msg() }
		second := native_proof_snapshot(&app.backend.native_operations)
		second_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		second_ownership := native_phase_a_backend_ownership_snapshot(app)
		assert app.status() == .stopped
		assert app.stop_terminal == first_reason
		assert replay_error == first_error
		assert !first.trace_overflow
		for entry in first.plan {
			assert !entry.armed
		}
		native_assert_complete_native_trace_for_test(first)
		native_proof_assert_snapshots_equal(first, second)
		native_phase_a_backend_assert_snapshots_equal(first_ownership, second_ownership)
		native_release_oracle_assert_equal_for_test(first_release_oracle, second_release_oracle)
		if app.backend.kind in [.x11, .wayland] {
			native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
				first_release_oracle, cleanup_tickets)
		}
		assert first.registry.tickets.len == 0
		assert app.backend.native_operations.disarm_proof()
		return first_error, first
	}

	fn native_egl_lifetime_is_cleared_for_test(app &App) bool {
		return match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					app.backend.x11.egl_display == unsafe { nil }
						&& app.backend.x11.egl_context == unsafe { nil }
						&& app.backend.x11.anchor_surface == unsafe { nil }
						&& app.backend.x11.egl_display_ticket == 0
						&& app.backend.x11.egl_context_ticket == 0
						&& app.backend.x11.egl_thread_ticket == 0
						&& app.backend.x11.anchor_surface_ticket == 0
						&& app.backend.x11.windows.all(it.egl_surface_ticket == 0)
						&& app.backend.native_operations.lifetime_tickets.len == 0
				} $else {
					false
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					app.backend.wayland.egl_display == unsafe { nil }
						&& app.backend.wayland.egl_context == unsafe { nil }
						&& app.backend.wayland.anchor_surface == unsafe { nil }
						&& app.backend.wayland.anchor_wl_egl_window == unsafe { nil }
						&& app.backend.wayland.anchor_wl_surface == unsafe { nil }
						&& app.backend.wayland.egl_display_ticket == 0
						&& app.backend.wayland.egl_context_ticket == 0
						&& app.backend.wayland.egl_thread_ticket == 0
						&& app.backend.wayland.anchor_surface_ticket == 0
						&& app.backend.wayland.anchor_wl_egl_window_ticket == 0
						&& app.backend.wayland.anchor_wl_surface_ticket == 0
						&& app.backend.wayland.windows.all(it.egl_surface_ticket == 0
						&& it.wl_egl_window_ticket == 0 && it.frame_callback_ticket == 0)
						&& app.backend.native_operations.lifetime_tickets.len == 0
				} $else {
					false
				}
			}
			else {
				false
			}
		}
	}

	fn native_assert_only_egl_lifetime_calls_after_for_test(snapshot NativeAuthorityProofSnapshot, start int) {
		mut release_count := 0
		for index in start .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone != .real_call {
				continue
			}
			assert entry.context.call_site == .shutdown_release
			assert entry.context.operation in [.surface_destroy, .context_destroy, .display_terminate,
				.release_thread]
			assert index + 5 < snapshot.trace_len
			expected := [
				NativeOperationTraceMilestone.real_call,
				.actual_primitive,
				.effective_primitive,
				.acceptance,
				.health_latched,
				.authority_release,
			]
			for offset, milestone in expected {
				part := snapshot.trace[index + offset]
				assert part.milestone == milestone
				assert native_operation_contexts_identical(part.context, entry.context)
			}
			release_count++
		}
		assert release_count > 0
	}

	fn native_assert_identity_released_once_for_test(snapshot NativeAuthorityProofSnapshot, domain NativeRenderDomain, operation NativeRenderOperation, identity u64) int {
		assert identity != 0
		mut starts := []int{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .real_call && entry.context.domain == domain
				&& entry.context.operation == operation && entry.context.target_identity == identity {
				starts << index
			}
		}
		assert starts.len == 1
		start := starts[0]
		has_ancillary := domain == .wayland && start + 3 < snapshot.trace_len
			&& snapshot.trace[start + 3].milestone == .real_call
			&& snapshot.trace[start + 3].context.operation == .wayland_display_error_query
		release_offset := if has_ancillary { 8 } else { 5 }
		assert start + release_offset < snapshot.trace_len
		expected_prefix := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		]
		for offset, milestone in expected_prefix {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, snapshot.trace[start].context)
		}
		if has_ancillary {
			assert snapshot.trace[start + 3].milestone == .real_call
			assert snapshot.trace[start + 3].context.operation == .wayland_display_error_query
			assert snapshot.trace[start + 4].milestone == .actual_primitive
			assert snapshot.trace[start + 5].milestone == .effective_primitive
			assert snapshot.trace[start + 6].milestone == .acceptance
			assert snapshot.trace[start + 7].milestone == .health_latched
			assert snapshot.trace[start + 8].milestone == .authority_release
			assert native_operation_contexts_identical(snapshot.trace[start + 6].context,
				snapshot.trace[start].context)
			assert native_operation_contexts_identical(snapshot.trace[start + 7].context,
				snapshot.trace[start].context)
			assert native_operation_contexts_identical(snapshot.trace[start + 8].context,
				snapshot.trace[start].context)
		} else {
			assert snapshot.trace[start + 3].milestone == .acceptance
			assert snapshot.trace[start + 4].milestone == .health_latched
			assert snapshot.trace[start + 5].milestone == .authority_release
			for offset in 3 .. 6 {
				assert native_operation_contexts_identical(snapshot.trace[start + offset].context,
					snapshot.trace[start].context)
			}
		}
		return start
	}

	fn native_assert_no_real_calls_after_for_test(snapshot NativeAuthorityProofSnapshot, start int) {
		for index in start .. snapshot.trace_len {
			assert snapshot.trace[index].milestone != .real_call
		}
	}

	fn native_acceptance_index_for_test(authority &NativeOperationAuthority, context NativeOperationContext) int {
		assert authority.proof != unsafe { nil }
		return native_acceptance_index_in_snapshot_for_test(native_proof_snapshot(authority),
			context)
	}

	fn native_acceptance_index_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, context NativeOperationContext) int {
		mut matches := []int{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .acceptance
				&& native_operation_contexts_identical(entry.context, context) {
				matches << index
			}
		}
		assert matches.len == 1
		return matches[0]
	}

	fn native_egl_actual_binding_queries_for_test(authority &NativeOperationAuthority) []NativeOperationTraceEntry {
		mut result := []NativeOperationTraceEntry{}
		if authority.proof == unsafe { nil } {
			return result
		}
		for index in 0 .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if entry.milestone == .actual_primitive
				&& entry.context.operation in [.current_draw_query, .current_read_query, .current_context_query] {
				result << entry
			}
		}
		return result
	}

	fn native_find_real_operation_context_for_test(authority &NativeOperationAuthority, operation NativeRenderOperation) !NativeOperationContext {
		if authority.proof == unsafe { nil } {
			return error('native proof is not armed')
		}
		for index in 0 .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if entry.milestone == .real_call && entry.context.operation == operation {
				return entry.context
			}
		}
		return error('native trace contains no real `${operation}` operation')
	}

	fn native_assert_wayland_compound_capture_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, primary NativeOperationContext, expected NativeRenderDisposition, expected_errno i64, expected_display_error i64) {
		query := native_wayland_display_error_context_from_snapshot_for_test(snapshot,

			primary.ordinal + 1)
		mut start := -1
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .real_call
				&& native_operation_contexts_identical(entry.context, primary) {
				start = index
				break
			}
		}
		assert start >= 0
		assert start + 7 < snapshot.trace_len
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for offset, milestone in expected_milestones {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			if offset < 3 || offset >= 6 {
				assert native_operation_contexts_identical(entry.context, primary)
			} else {
				assert native_operation_contexts_identical(entry.context, query)
			}
		}
		primary_actual := snapshot.trace[start + 1].actual
		query_actual := snapshot.trace[start + 4].actual
		assert primary_actual.valid_mask != 0
		assert (query_actual.valid_mask & native_valid_wayland_display_error) != 0
		assert snapshot.trace[start + 2].effective.native_errno == expected_errno
		assert snapshot.trace[start + 5].effective.wayland_display_error == expected_display_error
		accepted := snapshot.trace[start + 6].result
		assert accepted.disposition == expected
		assert accepted.native_code == expected_errno
		assert accepted.display_error == expected_display_error
	}

	fn native_assert_wayland_compound_capture_for_test(authority &NativeOperationAuthority, primary NativeOperationContext, expected NativeRenderDisposition, expected_errno i64, expected_display_error i64) {
		assert authority.proof != unsafe { nil }
		native_assert_wayland_compound_capture_in_snapshot_for_test(native_proof_snapshot(authority),
			primary, expected, expected_errno, expected_display_error)
	}

	fn native_wayland_trace_start_for_test(authority &NativeOperationAuthority, context NativeOperationContext) !int {
		if authority.proof == unsafe { nil } {
			return error('native proof is not armed')
		}
		for index in 0 .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if entry.milestone == .real_call
				&& native_operation_contexts_identical(entry.context, context) {
				return index
			}
		}
		return error('native trace contains no exact `${context.operation}` context')
	}

	fn native_assert_wayland_plan_consumed_for_test(authority &NativeOperationAuthority, primary NativeOperationContext, injected NativePrimitiveEvidence, display_error i64) {
		assert authority.proof != unsafe { nil }
		query := native_wayland_display_error_context_from_authority_for_test(authority,

			primary.ordinal + 1)
		mut primary_matches := 0
		mut query_matches := 0
		for entry in authority.proof.plan {
			if native_operation_contexts_identical(entry.context, primary) {
				primary_matches++
				assert !entry.armed
				assert !entry.listener_registration_pending
				assert native_proof_evidence_equal(entry.evidence, injected)
			}
			if native_operation_contexts_identical(entry.context, query) {
				query_matches++
				assert !entry.armed
				assert !entry.listener_registration_pending
				assert native_proof_evidence_equal(entry.evidence, NativePrimitiveEvidence{
					valid_mask:            native_valid_wayland_display_error
					wayland_display_error: display_error
				})
			}
		}
		assert primary_matches == 1
		assert query_matches == 1
	}

	fn native_assert_wayland_plan_still_armed_for_test(authority &NativeOperationAuthority, primary NativeOperationContext) {
		assert authority.proof != unsafe { nil }
		query := native_wayland_display_error_context_from_authority_for_test(authority,

			primary.ordinal + 1)
		mut primary_matches := 0
		mut query_matches := 0
		for entry in authority.proof.plan {
			if native_operation_contexts_identical(entry.context, primary) {
				primary_matches++
				assert entry.armed
			}
			if native_operation_contexts_identical(entry.context, query) {
				query_matches++
				assert entry.armed
			}
		}
		assert primary_matches == 1
		assert query_matches == 1
	}

	fn native_assert_wayland_injected_failure_for_test(authority &NativeOperationAuthority, primary NativeOperationContext, injected NativePrimitiveEvidence, expected NativeRenderDisposition) {
		native_assert_wayland_compound_capture_for_test(authority, primary, expected,
			injected.native_errno, 0)
		start := native_wayland_trace_start_for_test(authority, primary) or { panic(err) }
		primary_actual := authority.proof.trace[start + 1].actual
		primary_effective := authority.proof.trace[start + 2].effective
		query_actual := authority.proof.trace[start + 4].actual
		query_effective := authority.proof.trace[start + 5].effective
		assert (primary_actual.valid_mask & native_valid_return_value) != 0
		assert primary_actual.return_value == 0
		assert (primary_actual.valid_mask & native_valid_errno) == 0
		assert native_proof_evidence_equal(primary_effective, injected)
		assert (query_actual.valid_mask & native_valid_wayland_display_error) != 0
		assert query_actual.wayland_display_error == 0
		assert query_effective.valid_mask == native_valid_wayland_display_error
		assert query_effective.wayland_display_error == 0
		native_assert_wayland_plan_consumed_for_test(authority, primary, injected, 0)
	}

	fn native_assert_wayland_uninjected_success_after_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, operation NativeRenderOperation, start_index int) !NativeOperationContext {
		for index in start_index .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone != .real_call || entry.context.operation != operation {
				continue
			}
			assert index + 7 < snapshot.trace_len
			assert snapshot.trace[index + 1].milestone == .actual_primitive
			assert snapshot.trace[index + 2].milestone == .effective_primitive
			assert snapshot.trace[index + 3].milestone == .real_call
			assert snapshot.trace[index + 3].context.operation == .wayland_display_error_query
			assert snapshot.trace[index + 4].milestone == .actual_primitive
			assert snapshot.trace[index + 5].milestone == .effective_primitive
			assert snapshot.trace[index + 6].milestone == .acceptance
			assert snapshot.trace[index + 7].milestone == .health_latched
			assert native_proof_evidence_equal(snapshot.trace[index + 1].actual, snapshot.trace[
				index + 2].effective)
			assert native_proof_evidence_equal(snapshot.trace[index + 4].actual, snapshot.trace[
				index + 5].effective)
			assert (snapshot.trace[index + 4].actual.valid_mask & native_valid_wayland_display_error) != 0
			assert snapshot.trace[index + 4].actual.wayland_display_error == 0
			if snapshot.trace[index + 6].result.disposition == .ok {
				return entry.context
			}
		}
		return error('native retry did not execute a successful uninjected `${operation}` boundary')
	}

	fn native_assert_wayland_uninjected_success_after_for_test(authority &NativeOperationAuthority, operation NativeRenderOperation, start_index int) !NativeOperationContext {
		assert authority.proof != unsafe { nil }
		return native_assert_wayland_uninjected_success_after_in_snapshot_for_test(native_proof_snapshot(authority),
			operation, start_index)
	}

	fn native_wayland_has_successful_acceptance_after_for_test(authority &NativeOperationAuthority, operation NativeRenderOperation, start_index int) bool {
		if authority.proof == unsafe { nil } {
			return false
		}
		for index in start_index .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if entry.milestone == .acceptance && entry.context.operation == operation
				&& entry.result.disposition == .ok {
				return true
			}
		}
		return false
	}

	fn native_clear_trace_preserving_consumed_plan_for_test(mut authority NativeOperationAuthority) [native_primitive_plan_capacity]NativePrimitivePlanEntry {
		assert authority.proof != unsafe { nil }
		expected_plan := authority.proof.plan
		for entry in expected_plan {
			assert !entry.armed
			assert !entry.listener_registration_pending
		}
		for index in 0 .. native_operation_trace_capacity {
			authority.proof.trace[index] = NativeOperationTraceEntry{}
		}
		authority.proof.trace_len = 0
		authority.proof.trace_overflow = false
		native_proof_assert_plan_equal(expected_plan, authority.proof.plan)
		return expected_plan
	}

	fn native_segment_consumed_trace_before_stop_for_test(mut app App, segment NativeAuthorityProofSnapshot, backend BackendKind, expected_oracle NativeReleaseOracleSnapshot) NativeAuthorityProofSnapshot {
		assert !segment.trace_overflow
		native_assert_complete_native_trace_for_test(segment)
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		assert !expected_oracle.overflow
		current_oracle := native_release_oracle_snapshot_for_test(backend)
		native_release_oracle_assert_equal_for_test(expected_oracle, current_oracle)
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(segment.plan, consumed_plan)
		cleared := native_proof_snapshot(&app.backend.native_operations)
		assert cleared.trace_len == 0
		assert !cleared.trace_overflow
		assert cleared.app_identity == segment.app_identity
		assert cleared.app_lifetime_token == segment.app_lifetime_token
		assert cleared.renderer_attempt_token == segment.renderer_attempt_token
		assert cleared.owner_thread_identity == segment.owner_thread_identity
		assert cleared.next_ordinal == segment.next_ordinal
		assert cleared.next_proof_generation == segment.next_proof_generation
		assert cleared.sequence_exhausted == segment.sequence_exhausted
		assert cleared.terminal_cause == segment.terminal_cause
		assert cleared.proof_armed == segment.proof_armed
		assert cleared.proof_generation == segment.proof_generation
		assert cleared.proof_ordinal_floor == segment.proof_ordinal_floor
		assert cleared.proof_accepting_plans == segment.proof_accepting_plans
		native_proof_assert_plan_equal(consumed_plan, cleared.plan)
		native_lifetime_registry_assert_snapshots_equal(segment.registry, cleared.registry)
		cleared_oracle := native_release_oracle_snapshot_for_test(backend)
		native_release_oracle_assert_equal_for_test(expected_oracle, cleared_oracle)
		return cleared
	}

	fn native_reset_or_clear_proof_for_test(mut authority NativeOperationAuthority) {
		assert authority.proof != unsafe { nil }
		if authority.has_live_tickets_for_proof_generation(authority.proof.generation) {
			assert !authority.has_pending_native_plans()
			_ = native_clear_trace_preserving_consumed_plan_for_test(mut authority)
			assert authority.proof.accepting_plans
			return
		}
		assert authority.reset_proof()
		assert authority.proof.accepting_plans
	}

	fn native_wayland_display_error_context_from_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, ordinal u64) NativeOperationContext {
		mut identity := u64(0)
		for entry in snapshot.plan {
			if entry.context.operation == .wayland_display_error_query
				&& entry.context.ordinal == ordinal {
				identity = entry.context.target_identity
				break
			}
		}
		if identity == 0 {
			for index in 0 .. snapshot.trace_len {
				entry := snapshot.trace[index]
				if entry.context.operation == .wayland_display_error_query
					&& entry.context.ordinal == ordinal {
					identity = entry.context.target_identity
					break
				}
			}
		}
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        snapshot.renderer_attempt_token
			renderer_attempt_token: snapshot.renderer_attempt_token
			app_identity:           snapshot.app_identity
			presence_mask:          native_context_has_target_identity
			domain:                 .wayland
			operation:              .wayland_display_error_query
			call_site:              .display_transport
			scope:                  .renderer
			target_identity:        identity
			ordinal:                ordinal
		}
	}

	fn native_wayland_display_error_context_from_authority_for_test(authority &NativeOperationAuthority, ordinal u64) NativeOperationContext {
		assert authority.proof != unsafe { nil }
		return native_wayland_display_error_context_from_snapshot_for_test(native_proof_snapshot(authority),
			ordinal)
	}

	fn native_assert_wayland_prepare_balance_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot) {
		mut armed_prepare := false
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone != .actual_primitive {
				continue
			}
			match entry.context.operation {
				.display_prepare {
					if entry.actual.return_value == 0 {
						assert !armed_prepare
						armed_prepare = true
					}
				}
				.display_read, .display_cancel {
					if armed_prepare {
						armed_prepare = false
					}
				}
				else {}
			}
		}
		assert !armed_prepare
	}

	fn native_assert_wayland_prepare_balance_for_test(authority &NativeOperationAuthority) {
		assert authority.proof != unsafe { nil }
		native_assert_wayland_prepare_balance_in_snapshot_for_test(native_proof_snapshot(authority))
	}

	fn native_real_call_count_in_snapshot_for_test(snapshot NativeAuthorityProofSnapshot, operation NativeRenderOperation) int {
		mut count := 0
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .real_call && entry.context.operation == operation {
				count++
			}
		}
		return count
	}

	fn native_real_call_count_for_test(authority &NativeOperationAuthority, operation NativeRenderOperation) int {
		if authority.proof == unsafe { nil } {
			return 0
		}
		return native_real_call_count_in_snapshot_for_test(native_proof_snapshot(authority),
			operation)
	}

	fn native_dxgi_query_context_for_test(primary NativeOperationContext, device_identity u64) NativeOperationContext {
		return NativeOperationContext{
			...primary
			presence_mask:   primary.presence_mask | native_context_has_target_identity
			operation:       .dxgi_removal_query
			target_identity: device_identity
			ordinal:         primary.ordinal + 1
		}
	}

	fn native_arm_dxgi_primitive_for_test(mut authority NativeOperationAuthority, primary NativeOperationContext, query NativeOperationContext, operation_hresult i64, removal_hresult i64) ! {
		if !authority.arm(primary, NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value
			return_value: operation_hresult
		}) {
			return error('could not arm DXGI primary context `${primary}`')
		}
		if !authority.arm(query, NativePrimitiveEvidence{
			valid_mask:          native_valid_dxgi_removal_reason
			dxgi_removal_reason: removal_hresult
		}) {
			return error('could not arm DXGI removal context `${query}`')
		}
	}

	fn native_assert_dxgi_compound_capture_for_test(authority &NativeOperationAuthority, primary NativeOperationContext, query NativeOperationContext, expected NativeRenderDisposition, operation_hresult i64, removal_hresult i64) int {
		assert authority.proof != unsafe { nil }
		mut start := -1
		for index in 0 .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if entry.milestone == .real_call
				&& native_operation_contexts_identical(entry.context, primary) {
				start = index
				break
			}
		}
		assert start >= 0
		assert start + 7 < authority.proof.trace_len
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for offset, milestone in expected_milestones {
			entry := authority.proof.trace[start + offset]
			assert entry.milestone == milestone
			if offset < 3 || offset >= 6 {
				assert native_operation_contexts_identical(entry.context, primary)
			} else {
				assert native_operation_contexts_identical(entry.context, query)
			}
		}
		assert authority.proof.trace[start + 1].actual.return_value == 0
		assert authority.proof.trace[start + 2].effective.return_value == operation_hresult
		assert authority.proof.trace[start + 4].actual.dxgi_removal_reason == 0
		assert authority.proof.trace[start + 5].effective.dxgi_removal_reason == removal_hresult
		accepted := authority.proof.trace[start + 6].result
		assert accepted.disposition == expected
		assert u32(accepted.native_code) == u32(operation_hresult)
		assert u32(accepted.removal_reason) == u32(removal_hresult)
		return start + 6
	}

	fn native_dxgi_release_identities_for_test(app &App, window WindowId) ![]u64 {
		$if windows && sokol_d3d11 ? {
			index := app.backend.win32.window_record_index(window) or {
				return error(err_window_not_found)
			}
			record := app.backend.win32.windows[index]
			mut identities := []u64{}
			for value in [app.backend.win32.device, app.backend.win32.device_context, app.backend.win32.factory,
				app.backend.win32.anchor_color_texture, app.backend.win32.anchor_render_view,
				app.backend.win32.anchor_depth_texture, app.backend.win32.anchor_depth_stencil_view,
				record.swapchain, record.render_view, record.depth_texture, record.depth_stencil_view] {
				identity := native_identity(value)
				if identity != 0 && identity !in identities {
					identities << identity
				}
			}
			return identities
		} $else {
			_ = app
			_ = window
			return error(err_backend_unsupported)
		}
	}

	fn native_dxgi_release_sequence_start_for_test(snapshot NativeAuthorityProofSnapshot, identity u64) int {
		mut indexes := []int{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.context.domain == .dxgi && entry.context.operation == .object_release
				&& entry.context.target_identity == identity {
				indexes << index
			}
		}
		assert identity != 0
		assert indexes.len == 6
		start := indexes[0]
		expected := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.authority_release,
			.health_latched,
		]
		context := snapshot.trace[start].context
		assert (context.presence_mask & native_context_has_target_identity) != 0
		for offset, milestone in expected {
			assert indexes[offset] == start + offset
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
		}
		actual := snapshot.trace[start + 1]
		effective := snapshot.trace[start + 2]
		acceptance := snapshot.trace[start + 3]
		release := snapshot.trace[start + 4]
		health := snapshot.trace[start + 5]
		accepted := acceptance.result
		assert accepted.operation == .object_release
		assert accepted.disposition == .ok
		assert accepted.local_validation == .void_completion
		assert native_proof_evidence_equal(actual.actual, acceptance.actual)
		assert native_proof_evidence_equal(actual.actual, accepted.actual_primitive)
		assert native_proof_evidence_equal(actual.effective, NativePrimitiveEvidence{})
		assert native_proof_evidence_equal(effective.actual, NativePrimitiveEvidence{})
		assert native_proof_evidence_equal(effective.effective, acceptance.effective)
		assert native_proof_evidence_equal(effective.effective, accepted.primitive)
		assert native_proof_evidence_equal(release.actual, acceptance.actual)
		assert native_proof_evidence_equal(release.effective, acceptance.effective)
		assert release.local_validation == acceptance.local_validation
		assert native_proof_result_equal(release.result, accepted)
		assert health.health != .uninitialized
		return start
	}

	fn native_dxgi_assert_released_after_for_test(snapshot NativeAuthorityProofSnapshot, identities []u64, acceptance_index int) {
		for identity in identities {
			start := native_dxgi_release_sequence_start_for_test(snapshot, identity)
			assert start > acceptance_index
		}
	}

	fn native_dxgi_assert_durable_slots_cleared_for_test(app &App) {
		$if windows && sokol_d3d11 ? {
			assert app.backend.win32.device == unsafe { nil }
			assert app.backend.win32.device_context == unsafe { nil }
			assert app.backend.win32.factory == unsafe { nil }
			assert app.backend.win32.anchor_color_texture == unsafe { nil }
			assert app.backend.win32.anchor_render_view == unsafe { nil }
			assert app.backend.win32.anchor_depth_texture == unsafe { nil }
			assert app.backend.win32.anchor_depth_stencil_view == unsafe { nil }
			assert app.backend.win32.windows.len == 0
		} $else {
			_ = app
		}
	}

	fn native_wayland_next_transport_ordinal_for_test(ordinal u64) u64 {
		assert ordinal <= ~u64(0) - native_wayland_transport_boundary_ordinal_span_for_test
		return ordinal + native_wayland_transport_boundary_ordinal_span_for_test
	}

	fn native_assert_wayland_sync_transport_compound_at_for_test(app &App, snapshot NativeAuthorityProofSnapshot, start int, operation NativeRenderOperation, target_identity u64, display_identity u64, expected_ordinal u64) (int, NativeOperationContext, NativePrimitiveEvidence) {
		end := start + native_wayland_transport_compound_trace_span_for_test
		assert start >= 0
		assert end <= snapshot.trace_len
		expected_context := native_wayland_primary_context_for_test(app, operation, NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: target_identity
		}, expected_ordinal)
		context := snapshot.trace[start].context
		assert native_operation_contexts_identical(context, expected_context)
		evidence_context := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          native_context_has_target_identity
			domain:                 .wayland
			operation:              .wayland_display_error_query
			call_site:              .display_transport
			scope:                  .renderer
			target_identity:        display_identity
			ordinal:                context.ordinal + 1
		}
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for offset, milestone in expected_milestones {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			if offset < 3 || offset >= 6 {
				assert native_operation_contexts_identical(entry.context, context)
			} else {
				assert native_operation_contexts_identical(entry.context, evidence_context)
			}
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		query_actual := snapshot.trace[start + 4].actual
		query_effective := snapshot.trace[start + 5].effective
		assert native_proof_evidence_equal(actual, effective)
		assert native_proof_evidence_equal(query_actual, query_effective)
		assert query_actual.has(native_valid_wayland_display_error)
		assert query_actual.wayland_display_error == 0
		expected_errno := if effective.has(native_valid_errno) {
			effective.native_errno
		} else {
			i64(0)
		}
		native_assert_wayland_compound_capture_for_test(&app.backend.native_operations, context,
			.ok, expected_errno, 0)
		assert snapshot.trace[start + 7].health == .ready
		return end, context, actual
	}

	fn native_assert_wayland_sync_cancel_at_for_test(app &App, snapshot NativeAuthorityProofSnapshot, start int, display_identity u64, expected_ordinal u64) (int, NativeOperationContext) {
		end := start + 9
		assert start >= 0
		assert end <= snapshot.trace_len
		context := native_wayland_primary_context_for_test(app, .display_cancel, NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: display_identity
		}, expected_ordinal)
		query_context := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          native_context_has_target_identity
			domain:                 .wayland
			operation:              .wayland_display_error_query
			call_site:              .display_transport
			scope:                  .renderer
			target_identity:        display_identity
			ordinal:                context.ordinal + 1
		}
		empty := NativePrimitiveEvidence{}
		display_error := NativePrimitiveEvidence{
			valid_mask: native_valid_wayland_display_error
		}
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		] {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
			assert native_proof_evidence_equal(entry.actual, empty)
			assert native_proof_evidence_equal(entry.effective, empty)
			assert entry.local_validation == .none
			assert native_proof_result_equal(entry.result, NativeRenderResult{})
			assert entry.health == .uninitialized
		}
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		] {
			entry := snapshot.trace[start + 3 + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, query_context)
			assert native_proof_evidence_equal(entry.actual, if offset == 1 {
				display_error
			} else {
				empty
			})
			assert native_proof_evidence_equal(entry.effective, if offset == 2 {
				display_error
			} else {
				empty
			})
			assert entry.local_validation == .none
			assert native_proof_result_equal(entry.result, NativeRenderResult{})
			assert entry.health == .uninitialized
		}
		accepted := snapshot.trace[start + 6]
		assert accepted.milestone == .acceptance
		assert native_operation_contexts_identical(accepted.context, context)
		assert native_proof_evidence_equal(accepted.actual, display_error)
		assert native_proof_evidence_equal(accepted.effective, display_error)
		assert accepted.local_validation == .void_completion
		assert accepted.result.domain == .wayland
		assert accepted.result.operation == .display_cancel
		assert accepted.result.scope == .batch
		assert accepted.result.disposition == .ok
		assert accepted.result.native_code == 0
		assert accepted.result.native_status == 0
		assert accepted.result.display_error == 0
		assert accepted.result.error_text == ''
		assert native_operation_contexts_identical(accepted.result.context, context)
		assert native_proof_evidence_equal(accepted.result.actual_primitive, display_error)
		assert native_proof_evidence_equal(accepted.result.primitive, display_error)
		assert accepted.result.local_validation == .void_completion
		health := snapshot.trace[start + 7]
		assert health.milestone == .health_latched
		assert native_operation_contexts_identical(health.context, context)
		assert native_proof_evidence_equal(health.actual, empty)
		assert native_proof_evidence_equal(health.effective, empty)
		assert health.local_validation == .none
		assert native_proof_result_equal(health.result, NativeRenderResult{})
		assert health.health == .ready
		release := snapshot.trace[start + 8]
		assert release.milestone == .authority_release
		assert native_operation_contexts_identical(release.context, context)
		assert native_proof_evidence_equal(release.actual, display_error)
		assert native_proof_evidence_equal(release.effective, display_error)
		assert release.local_validation == .void_completion
		assert native_proof_result_equal(release.result, accepted.result)
		assert release.health == .uninitialized
		assert snapshot.next_ordinal == native_wayland_next_transport_ordinal_for_test(context.ordinal)
		return end, context
	}

	fn native_assert_wayland_sync_callback_release_prefixes_for_test(before NativeAuthorityProofSnapshot, after NativeAuthorityProofSnapshot, start int, mut released_tickets []NativeLifetimeTicketProofSnapshot) int {
		mut cursor := start
		for cursor < after.trace_len {
			first := after.trace[cursor]
			if first.milestone != .real_call || first.context.domain != .wayland
				|| first.context.operation != .surface_destroy {
				break
			}
			mut ticket := NativeLifetimeTicketProofSnapshot{}
			mut ticket_matches := 0
			for candidate in before.registry.tickets {
				if native_operation_contexts_identical(candidate.context, first.context) {
					ticket_matches++
					ticket = candidate
				}
			}
			assert ticket_matches == 1
			assert ticket.release_kind == .wayland_frame_callback
			assert ticket.state == .bound
			assert ticket.app_identity == before.app_identity
			assert ticket.authority_scope == .renderer_attempt
			assert ticket.authority_token == before.renderer_attempt_token
			if ticket.context.ordinal < before.proof_ordinal_floor {
				assert ticket.proof_generation == 0
			} else {
				assert ticket.proof_generation == before.proof_generation
			}
			assert ticket.owner_seed.presence_mask == native_context_window_target_fields
			assert ticket.owner_seed.call_site == .window_finalize
			assert ticket.owner_seed.scope == .window_target
			assert ticket.owner_seed.window != WindowId{}
			assert ticket.owner_seed.target_generation != 0
			assert ticket.owner_seed.target_identity == 0
			assert ticket.context.presence_mask == ticket.owner_seed.presence_mask | native_context_has_target_identity
			assert ticket.context.call_site == ticket.owner_seed.call_site
			assert ticket.context.scope == ticket.owner_seed.scope
			assert ticket.context.window == ticket.owner_seed.window
			assert ticket.context.target_generation == ticket.owner_seed.target_generation
			assert ticket.context.batch_epoch == ticket.owner_seed.batch_epoch
			assert ticket.context.window_lease_epoch == ticket.owner_seed.window_lease_epoch
			assert ticket.context.target_lease_epoch == ticket.owner_seed.target_lease_epoch
			assert ticket.context.renderer_attempt_token == before.renderer_attempt_token
			native_assert_bound_ticket_snapshot_for_test(ticket, .wayland_frame_callback,
				ticket.native_identity, ticket.required_parent_identity)
			assert ticket.required_parent_identity != 0
			mut parent_matches := 0
			for parent in before.registry.tickets {
				if parent.release_kind == .wayland_egl_window
					&& parent.owner_seed.window == ticket.owner_seed.window
					&& parent.owner_seed.target_generation == ticket.owner_seed.target_generation {
					parent_matches++
					assert parent.state == .bound
					assert parent.required_parent_identity == ticket.required_parent_identity
				}
			}
			assert parent_matches == 1
			for released in released_tickets {
				assert released.ticket_id != ticket.ticket_id
				assert released.native_identity != ticket.native_identity
			}
			mut final_ticket_matches := 0
			for candidate in after.registry.tickets {
				if candidate.ticket_id == ticket.ticket_id
					|| (candidate.release_kind == .wayland_frame_callback
					&& candidate.native_identity == ticket.native_identity) {
					final_ticket_matches++
				}
			}
			assert final_ticket_matches == 0
			next := native_assert_lifetime_release_at_for_test(after, ticket, cursor)
			assert next == cursor + 6
			assert native_proof_evidence_equal(after.trace[cursor + 1].actual, NativePrimitiveEvidence{})
			assert native_proof_evidence_equal(after.trace[cursor + 2].effective, NativePrimitiveEvidence{})
			assert after.trace[cursor + 4].health == .ready
			released_tickets << ticket
			cursor = next
		}
		return cursor
	}

	fn native_assert_wayland_sync_callback_release_delta_for_test(before NativeAuthorityProofSnapshot, after NativeAuthorityProofSnapshot, before_oracle NativeReleaseOracleSnapshot, after_oracle NativeReleaseOracleSnapshot, released_tickets []NativeLifetimeTicketProofSnapshot) {
		assert after.registry.app_identity == before.registry.app_identity
		assert after.registry.app_lifetime_token == before.registry.app_lifetime_token
		assert after.registry.renderer_attempt_token == before.registry.renderer_attempt_token
		assert after.registry.owner_thread_identity == before.registry.owner_thread_identity
		assert before.registry.tickets.len == after.registry.tickets.len + released_tickets.len
		for initial in before.registry.tickets {
			mut released_matches := 0
			for released in released_tickets {
				if released.ticket_id == initial.ticket_id {
					released_matches++
					native_lifetime_ticket_assert_equal(initial, released)
				}
			}
			mut final_matches := 0
			for final in after.registry.tickets {
				if final.ticket_id == initial.ticket_id {
					final_matches++
					native_lifetime_ticket_assert_equal(initial, final)
				}
			}
			assert released_matches + final_matches == 1
		}
		for final in after.registry.tickets {
			mut initial_matches := 0
			for initial in before.registry.tickets {
				if initial.ticket_id == final.ticket_id {
					initial_matches++
					native_lifetime_ticket_assert_equal(initial, final)
				}
			}
			assert initial_matches == 1
		}
		assert !before_oracle.overflow
		assert !after_oracle.overflow
		assert after_oracle.records.len == before_oracle.records.len + released_tickets.len
		assert after_oracle.callback_completions.len == before_oracle.callback_completions.len +
			released_tickets.len
		assert after_oracle.unticketed_releases.len == before_oracle.unticketed_releases.len
		for index in 0 .. before_oracle.records.len {
			assert after_oracle.records[index] == before_oracle.records[index]
		}
		for index in 0 .. before_oracle.callback_completions.len {
			assert after_oracle.callback_completions[index] == before_oracle.callback_completions[index]
		}
		for index in 0 .. before_oracle.unticketed_releases.len {
			assert after_oracle.unticketed_releases[index] == before_oracle.unticketed_releases[index]
		}
		for index in before_oracle.records.len .. after_oracle.records.len {
			record := after_oracle.records[index]
			assert record.domain == .wayland
			assert record.kind == 2
			mut ticket_matches := 0
			for ticket in released_tickets {
				if ticket.native_identity == record.identity {
					ticket_matches++
				}
			}
			assert ticket_matches == 1
		}
		for index in before_oracle.callback_completions.len .. after_oracle.callback_completions.len {
			completion := after_oracle.callback_completions[index]
			assert completion.domain == .wayland
			assert completion.kind == 3
			mut ticket_matches := 0
			for ticket in released_tickets {
				if ticket.native_identity == completion.identity {
					ticket_matches++
				}
			}
			assert ticket_matches == 1
		}
		for ticket in released_tickets {
			mut destroy_count := 0
			mut completion_count := 0
			mut destroy_sequence := u64(0)
			mut completion_sequence := u64(0)
			for index in before_oracle.records.len .. after_oracle.records.len {
				record := after_oracle.records[index]
				if record.domain == .wayland && record.kind == 2
					&& record.identity == ticket.native_identity {
					destroy_count++
					destroy_sequence = record.sequence
				}
			}
			for index in before_oracle.callback_completions.len .. after_oracle.callback_completions.len {
				completion := after_oracle.callback_completions[index]
				if completion.domain == .wayland && completion.kind == 3
					&& completion.identity == ticket.native_identity {
					completion_count++
					completion_sequence = completion.sequence
				}
			}
			assert destroy_count == 1
			assert completion_count == 1
			assert destroy_sequence != 0
			assert completion_sequence > destroy_sequence
		}
	}

	fn native_assert_wayland_sync_transport_chain_for_test(app &App, before NativeAuthorityProofSnapshot, after NativeAuthorityProofSnapshot, before_oracle NativeReleaseOracleSnapshot, after_oracle NativeReleaseOracleSnapshot, outcome NativeRenderResult, display_identity u64) (NativeOperationContext, int) {
		assert display_identity != 0
		assert !after.trace_overflow
		mut cursor := before.trace_len
		mut expected_ordinal := before.next_ordinal
		mut context := NativeOperationContext{}
		mut actual := NativePrimitiveEvidence{}
		cursor, context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
			after, cursor, .display_flush, display_identity, display_identity, expected_ordinal)
		assert actual.has(native_valid_return_value)
		assert (actual.return_value >= 0 && !actual.has(native_valid_errno))
			|| (actual.return_value == -1 && actual.has(native_valid_errno)
			&& actual.native_errno == 11)
		expected_ordinal = native_wayland_next_transport_ordinal_for_test(context.ordinal)

		mut released_callback_tickets := []NativeLifetimeTicketProofSnapshot{}
		mut initial_dispatch_count := 0
		mut initial_dispatch_last_return := i64(-1)
		for {
			cursor = native_assert_wayland_sync_callback_release_prefixes_for_test(before, after,
				cursor, mut released_callback_tickets)
			assert cursor < after.trace_len
			assert after.trace[cursor].milestone == .real_call
			assert after.trace[cursor].context.operation == .display_dispatch
			cursor, context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
				after, cursor, .display_dispatch, display_identity, display_identity,
				expected_ordinal)
			assert actual.has(native_valid_return_value)
			assert actual.return_value >= 0
			initial_dispatch_count++
			initial_dispatch_last_return = actual.return_value
			expected_ordinal = native_wayland_next_transport_ordinal_for_test(context.ordinal)
			if actual.return_value == 0 {
				break
			}
		}
		assert initial_dispatch_count > 0
		assert initial_dispatch_last_return == 0
		poll_failure_mask := u64(wayland_poll_err | wayland_poll_hup | i16(0x020))
		mut first_read_context := NativeOperationContext{}
		mut parsed_read_count := 0
		mut cancel_context := NativeOperationContext{}
		for {
			cursor, context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
				after, cursor, .display_prepare, display_identity, display_identity,
				expected_ordinal)
			assert actual.has(native_valid_return_value)
			assert actual.return_value == 0
			expected_ordinal = native_wayland_next_transport_ordinal_for_test(context.ordinal)

			cursor, context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
				after, cursor, .display_fd_query, display_identity, display_identity,
				expected_ordinal)
			assert actual.has(native_valid_return_value)
			assert actual.return_value >= 0
			fd_identity := u64(actual.return_value)
			expected_ordinal = native_wayland_next_transport_ordinal_for_test(context.ordinal)

			poll_start := cursor
			mut poll_context := NativeOperationContext{}
			cursor, poll_context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
				after, cursor, .display_poll, fd_identity, display_identity, expected_ordinal)
			assert actual.has(native_valid_return_value)
			assert !actual.has(native_valid_errno)
			assert actual.has(native_valid_observed_flags)
			assert actual.return_value in [i64(0), i64(1)]
			assert (actual.observed_flags & poll_failure_mask) == 0
			if actual.return_value == 0 {
				assert actual.observed_flags == 0
				terminal_poll_result := after.trace[poll_start + 6].result
				assert native_operation_contexts_identical(outcome.context, poll_context)
				assert native_proof_result_equal(outcome, terminal_poll_result)
				reserved_before_cancel :=
					native_wayland_next_transport_ordinal_for_test(poll_context.ordinal)
				expected_ordinal =
					native_wayland_next_transport_ordinal_for_test(reserved_before_cancel)
				cursor, cancel_context = native_assert_wayland_sync_cancel_at_for_test(app, after,
					cursor, display_identity, expected_ordinal)
				break
			}

			assert actual.return_value == 1
			assert (actual.observed_flags & u64(wayland_poll_in)) != 0
			expected_ordinal = native_wayland_next_transport_ordinal_for_test(poll_context.ordinal)
			mut read_context := NativeOperationContext{}
			cursor, read_context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
				after, cursor, .display_read, display_identity, display_identity, expected_ordinal)
			assert read_context.ordinal == native_wayland_next_transport_ordinal_for_test(poll_context.ordinal)
			assert actual.has(native_valid_return_value)
			assert actual.return_value >= 0
			if parsed_read_count == 0 {
				first_read_context = read_context
			}
			parsed_read_count++
			reserved_after_read :=
				native_wayland_next_transport_ordinal_for_test(read_context.ordinal)
			expected_ordinal = native_wayland_next_transport_ordinal_for_test(reserved_after_read)

			mut post_read_dispatch_count := 0
			mut post_read_dispatch_last_return := i64(-1)
			for {
				cursor = native_assert_wayland_sync_callback_release_prefixes_for_test(before,
					after, cursor, mut released_callback_tickets)
				assert cursor < after.trace_len
				assert after.trace[cursor].milestone == .real_call
				assert after.trace[cursor].context.operation == .display_dispatch
				cursor, context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
					after, cursor, .display_dispatch, display_identity, display_identity,
					expected_ordinal)
				assert actual.has(native_valid_return_value)
				assert actual.return_value >= 0
				post_read_dispatch_count++
				post_read_dispatch_last_return = actual.return_value
				expected_ordinal = native_wayland_next_transport_ordinal_for_test(context.ordinal)
				if actual.return_value == 0 {
					break
				}
			}
			assert post_read_dispatch_count > 0
			assert post_read_dispatch_last_return == 0
		}
		assert parsed_read_count > 0
		assert first_read_context.ordinal != 0
		assert cursor == after.trace_len
		assert after.next_ordinal == native_wayland_next_transport_ordinal_for_test(cancel_context.ordinal)
		mut production_read_count := 0
		mut production_cancel_count := 0
		for index in before.trace_len .. after.trace_len {
			entry := after.trace[index]
			if entry.milestone != .real_call {
				continue
			}
			if entry.context.operation == .display_read {
				production_read_count++
			}
			if entry.context.operation == .display_cancel {
				production_cancel_count++
			}
		}
		assert production_read_count == parsed_read_count
		assert production_cancel_count == 1
		native_assert_wayland_sync_callback_release_delta_for_test(before, after, before_oracle,
			after_oracle, released_callback_tickets)
		return first_read_context, parsed_read_count
	}

	fn native_assert_wayland_sync_roundtrip_barrier_for_test(app &App, before NativeAuthorityProofSnapshot, after NativeAuthorityProofSnapshot, before_oracle NativeReleaseOracleSnapshot, after_oracle NativeReleaseOracleSnapshot, outcome NativeRenderResult, display_identity u64) {
		assert display_identity != 0
		assert before.trace_len == 0
		assert !before.trace_overflow
		assert !after.trace_overflow
		mut released_callback_tickets := []NativeLifetimeTicketProofSnapshot{}
		mut cursor := native_assert_wayland_sync_callback_release_prefixes_for_test(before, after,
			0, mut released_callback_tickets)
		roundtrip_start := cursor
		mut context := NativeOperationContext{}
		mut actual := NativePrimitiveEvidence{}
		cursor, context, actual = native_assert_wayland_sync_transport_compound_at_for_test(app,
			after, cursor, .display_roundtrip, display_identity, display_identity,
			before.next_ordinal)
		assert actual.has(native_valid_return_value)
		assert actual.return_value >= 0
		assert outcome.succeeded()
		assert native_operation_contexts_identical(outcome.context, context)
		assert native_proof_result_equal(outcome, after.trace[roundtrip_start + 6].result)
		assert cursor == after.trace_len
		assert after.next_ordinal == native_wayland_next_transport_ordinal_for_test(context.ordinal)
		native_proof_assert_plan_equal(before.plan, after.plan)
		native_assert_complete_native_trace_for_test(after)
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		native_assert_wayland_sync_callback_release_delta_for_test(before, after, before_oracle,
			after_oracle, released_callback_tickets)
	}

	fn native_wayland_dispatch_injected_read_at_sync_boundary_for_test(mut app App, injected NativePrimitiveEvidence) !(NativeRenderResult, NativeOperationContext) {
		$if linux && sokol_wayland ? {
			if !app.backend.native_operations.owner_thread_is_current() {
				return error('Wayland injected read requires the native owner thread')
			}
			if app.backend.wayland.display == unsafe { nil } {
				return error('Wayland injected read requires a live display')
			}
			display_identity := native_identity(app.backend.wayland.display)
			if display_identity == 0 {
				return error('Wayland injected read requires a display identity')
			}
			display := unsafe { &C.wl_display(app.backend.wayland.display) }
			sync_callback := C.wl_display_sync(display)
			if sync_callback == unsafe { nil } {
				return error('Wayland injected read could not create an asynchronous callback')
			}
			mut sync_state := &NativeWaylandSyncState{}
			mut callback_owned := true
			defer {
				if callback_owned && !sync_state.done {
					C.wl_callback_destroy(sync_callback)
				}
			}
			sync_callback_identity := native_identity(sync_callback)
			if sync_callback_identity == 0 {
				return error('Wayland injected read created a callback without identity')
			}
			listener := C.wl_callback_listener{
				done: native_wayland_sync_done_for_test
			}
			if C.wl_callback_add_listener(sync_callback, &listener, voidptr(sync_state)) != 0 {
				return error('Wayland injected read could not install the callback listener')
			}
			if C.wl_display_flush(display) < 0 {
				return error('Wayland injected read could not flush the callback request')
			}
			fd := C.wl_display_get_fd(display)
			if fd < 0 {
				return error('Wayland injected read could not obtain the display fd')
			}
			mut poll_fd := C.pollfd{
				fd:      fd
				events:  wayland_poll_in
				revents: i16(0)
			}
			poll_result := C.poll(&poll_fd, u64(1), 5000)
			poll_failure_mask := wayland_poll_err | wayland_poll_hup | i16(0x020)
			if poll_result != 1 || (poll_fd.revents & wayland_poll_in) == 0
				|| (poll_fd.revents & poll_failure_mask) != 0 {
				return error('Wayland injected read poll returned `${poll_result}` with revents `${poll_fd.revents}`')
			}
			seed := native_wayland_transport_seed_for_test(app)
			if seed.target_identity != display_identity {
				return error('Wayland injected read transport seed does not identify the live display')
			}
			failure_context := native_wayland_primary_context_for_test(app, .display_read, seed,
				app.backend.native_operations.next_ordinal +
				native_wayland_quiescent_read_ordinal_offset_for_test)
			native_arm_wayland_primitive_for_test(mut app, failure_context, injected, 0)!
			failure := app.backend.wayland.dispatch_pending_nonblocking()
			if sync_state.done || sync_state.callback_identity != 0 {
				return error('Wayland injected read unexpectedly dispatched callback `${sync_callback_identity}`')
			}
			C.wl_callback_destroy(sync_callback)
			callback_owned = false
			native_assert_wayland_plan_consumed_for_test(&app.backend.native_operations,
				failure_context, injected, 0)
			if native_real_call_count_for_test(&app.backend.native_operations, .display_read) != 1 {
				return error('Wayland injected read did not execute exactly one display_read boundary')
			}
			if native_real_call_count_for_test(&app.backend.native_operations, .display_cancel) != 0 {
				return error('Wayland injected read unexpectedly executed display_cancel')
			}
			if failure.disposition != .operation_failed
				|| !native_operation_contexts_identical(failure.context, failure_context) {
				return error('Wayland injected read did not return the exact injected failure')
			}
			return failure, failure_context
		} $else {
			_ = app
			_ = injected
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_dispatch_sync_for_test(mut app App) !NativeWaylandSyncDispatchProof {
		$if linux && sokol_wayland ? {
			if !app.backend.native_operations.owner_thread_is_current() {
				return error('Wayland sync dispatch requires the native owner thread')
			}
			if app.backend.wayland.display == unsafe { nil } {
				return error('Wayland sync dispatch requires a live display')
			}
			display_identity := native_identity(app.backend.wayland.display)
			before := native_proof_snapshot(&app.backend.native_operations)
			before_oracle := native_release_oracle_snapshot_for_test(.wayland)
			display := unsafe { &C.wl_display(app.backend.wayland.display) }
			sync_callback := C.wl_display_sync(display)
			if sync_callback == unsafe { nil } {
				return error('Wayland sync dispatch could not create an asynchronous callback')
			}
			mut sync_state := &NativeWaylandSyncState{}
			mut callback_owned := true
			defer {
				if callback_owned && !sync_state.done {
					C.wl_callback_destroy(sync_callback)
				}
			}
			sync_callback_identity := native_identity(sync_callback)
			if sync_callback_identity == 0 {
				return error('Wayland sync dispatch created a callback without identity')
			}
			listener := C.wl_callback_listener{
				done: native_wayland_sync_done_for_test
			}
			if C.wl_callback_add_listener(sync_callback, &listener, voidptr(sync_state)) != 0 {
				return error('Wayland sync dispatch could not install the callback listener')
			}
			if C.wl_display_flush(display) < 0 {
				return error('Wayland sync dispatch could not flush the callback request')
			}
			fd := C.wl_display_get_fd(display)
			if fd < 0 {
				return error('Wayland sync dispatch could not obtain the display fd')
			}
			mut poll_fd := C.pollfd{
				fd:      fd
				events:  wayland_poll_in
				revents: i16(0)
			}
			poll_result := C.poll(&poll_fd, u64(1), 5000)
			poll_failure_mask := wayland_poll_err | wayland_poll_hup | i16(0x020)
			if poll_result != 1 || (poll_fd.revents & wayland_poll_in) == 0
				|| (poll_fd.revents & poll_failure_mask) != 0 {
				return error('Wayland sync dispatch poll returned `${poll_result}` with revents `${poll_fd.revents}`')
			}
			outcome := app.backend.wayland.dispatch_pending_nonblocking()
			after := native_proof_snapshot(&app.backend.native_operations)
			after_oracle := native_release_oracle_snapshot_for_test(.wayland)
			read_context, read_count := native_assert_wayland_sync_transport_chain_for_test(app,
				before, after, before_oracle, after_oracle, outcome, display_identity)
			if !sync_state.done {
				barrier_before := native_segment_consumed_trace_before_stop_for_test(mut app,
					after, .wayland, after_oracle)
				barrier_before_oracle := native_release_oracle_snapshot_for_test(.wayland)
				native_release_oracle_assert_equal_for_test(after_oracle, barrier_before_oracle)
				roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
					call_site: .display_transport
					scope:     .batch
				})
				barrier_after := native_proof_snapshot(&app.backend.native_operations)
				barrier_after_oracle := native_release_oracle_snapshot_for_test(.wayland)
				native_assert_wayland_sync_roundtrip_barrier_for_test(app, barrier_before,
					barrier_after, barrier_before_oracle, barrier_after_oracle, roundtrip,
					display_identity)
			}
			if !sync_state.done {
				return error('Wayland sync dispatch did not dispatch the exact callback')
			}
			if sync_state.callback_identity != sync_callback_identity {
				return error('Wayland sync dispatch observed callback `${sync_state.callback_identity}`, expected `${sync_callback_identity}`')
			}
			callback_owned = false
			return NativeWaylandSyncDispatchProof{
				outcome:      outcome
				read_context: read_context
				read_count:   read_count
				snapshot:     after
			}
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_require_real_read_for_test(mut app App) ! {
		window := native_runtime_new_window_for_test(mut app, 'native Wayland read balance')!
		_ = native_egl_render_healthy_target_for_test(mut app, window)!
		before_sync := native_proof_snapshot(&app.backend.native_operations)
		assert !before_sync.trace_overflow
		native_assert_complete_native_trace_for_test(before_sync)
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(before_sync.plan, consumed_plan)
		sync_start := native_proof_snapshot(&app.backend.native_operations)
		assert sync_start.trace_len == 0
		assert !sync_start.trace_overflow
		assert sync_start.proof_generation == before_sync.proof_generation
		assert sync_start.next_proof_generation == before_sync.next_proof_generation
		assert sync_start.proof_ordinal_floor == before_sync.proof_ordinal_floor
		assert sync_start.next_ordinal == before_sync.next_ordinal
		native_proof_assert_plan_equal(consumed_plan, sync_start.plan)
		native_lifetime_registry_assert_snapshots_equal(before_sync.registry, sync_start.registry)
		display_identity := native_wayland_display_identity_for_test(app)
		assert display_identity != 0
		sync := native_wayland_dispatch_sync_for_test(mut app)!
		assert sync.outcome.succeeded()
		assert !sync.snapshot.trace_overflow
		native_assert_complete_native_trace_for_test(sync.snapshot)
		native_assert_proof_snapshot_drained_for_test(sync.snapshot)
		assert native_real_call_count_in_snapshot_for_test(sync.snapshot, .display_read) == sync.read_count
		assert native_real_call_count_in_snapshot_for_test(sync.snapshot, .display_cancel) == 1
		context := native_assert_wayland_uninjected_success_after_in_snapshot_for_test(sync.snapshot,
			.display_read, 0)!
		assert native_operation_contexts_identical(context, sync.read_context)
		expected_context := native_wayland_primary_context_for_test(app, .display_read, NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: display_identity
		}, sync.read_context.ordinal)
		assert native_operation_contexts_identical(context, expected_context)
		native_assert_wayland_compound_capture_in_snapshot_for_test(sync.snapshot, context, .ok, 0,
			0)
		native_assert_wayland_prepare_balance_in_snapshot_for_test(sync.snapshot)
		native_proof_assert_plan_equal(consumed_plan, sync.snapshot.plan)
		native_proof_assert_plan_equal(consumed_plan, app.backend.native_operations.proof.plan)
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		after_sync := native_proof_snapshot(&app.backend.native_operations)
		assert !after_sync.trace_overflow
		native_assert_complete_native_trace_for_test(after_sync)
		after_sync_oracle := native_release_oracle_snapshot_for_test(.wayland)
		after_sync_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(after_sync.plan, after_sync_plan)
		after_sync_cleared := native_proof_snapshot(&app.backend.native_operations)
		assert after_sync_cleared.trace_len == 0
		assert !after_sync_cleared.trace_overflow
		assert after_sync_cleared.proof_generation == after_sync.proof_generation
		assert after_sync_cleared.next_proof_generation == after_sync.next_proof_generation
		assert after_sync_cleared.proof_ordinal_floor == after_sync.proof_ordinal_floor
		assert after_sync_cleared.next_ordinal == after_sync.next_ordinal
		native_proof_assert_plan_equal(after_sync_plan, after_sync_cleared.plan)
		native_lifetime_registry_assert_snapshots_equal(after_sync.registry,
			after_sync_cleared.registry)
		after_sync_cleared_oracle := native_release_oracle_snapshot_for_test(.wayland)
		native_release_oracle_assert_equal_for_test(after_sync_oracle, after_sync_cleared_oracle)
	}

	fn native_poll_until_replacement_for_test(mut app App, id WindowId, failed NativeEglTargetProof) !NativeEglTargetProof {
		for _ in 0 .. 128 {
			app.poll_events()!
			current := native_egl_target_for_test(app, id)!
			if current.generation != 0 && current.generation != failed.generation
				&& app.render_window_eligible(id)! {
				return current
			}
		}
		return error('EGL target loss did not produce an eligible distinct replacement')
	}

	fn native_egl_exercise_bad_surface(post_commit bool) ! {
		backend := native_runtime_backend_for_test()!
		mut app := native_runtime_new_app_for_test(backend)!
		mut failure_sokol_generation := u64(0)
		defer {
			if app.status() != .stopped {
				app.stop() or {}
			}
			if app.backend.native_operations.proof != unsafe { nil } {
				_ = app.backend.native_operations.disarm_proof()
			}
			if failure_sokol_generation != 0
				&& multiwindow_sokol_trace.active_generation() == failure_sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(failure_sokol_generation)
			}
		}
		window := native_runtime_new_window_for_test(mut app, if post_commit {
			'native post-commit EGL target loss'
		} else {
			'native pre-pass EGL target loss'
		})!
		mut initial_state := &NativeEglHealthyTargetState{}
		if backend == .wayland {
			mut initial_phase := &NativeEglSokolPhaseState{}
			initial_outcome := app.with_scheduled_render_batch(fn [mut app, window, mut initial_phase, mut initial_state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
				initial_phase.candidate_present = candidates.any(it.window == window)
				acquisition := app.acquire_render_target(batch, window)!
				initial_phase.acquisition_status = acquisition.status
				initial_state.target = native_egl_target_for_test(app, window)!
				initial_state.ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
					initial_state.target.ticket_id)!
			})!
			assert initial_outcome.error == ''
			assert !initial_outcome.committed
			assert !initial_outcome.had_gpu_work
			assert initial_outcome.completed_user_passes == 0
			assert initial_outcome.finalized_submissions == 0
			assert initial_phase.candidate_present
			assert initial_phase.acquisition_status == .ready
			assert initial_state.target.generation != 0
			assert initial_state.target.identity != 0
			assert initial_state.target.ticket_id != 0
			app.poll_events()!
			assert app.render_window_eligible(window)!, 'EGL redraw did not become render eligible after bounded owner polling'
		} else {
			initial_state.target = native_egl_render_healthy_target_for_test(mut app, window)!
			app.request_redraw(window)!
			redraw_deadline := time.now().add(2 * time.second)
			for {
				app.poll_events() or {
					if err.msg() != err_app_stopped {
						return err
					}
					return error('native app stopped before window became render eligible')
				}
				if app.render_window_eligible(window)! {
					break
				}
				if time.now() >= redraw_deadline {
					break
				}
			}
			assert app.render_window_eligible(window)!, 'EGL redraw did not become render eligible after bounded owner polling'
			initial_state.ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				initial_state.target.ticket_id)!
		}
		initial_target := initial_state.target
		initial_ticket := initial_state.ticket
		initial := app.render_window_snapshot(window)!
		assert initial_target.generation != 0
		assert initial_target.identity != 0
		assert initial_target.ticket_id != 0
		display_identity := native_egl_display_identity_for_test(app)
		native_assert_bound_ticket_snapshot_for_test(initial_ticket, .egl_surface,
			initial_target.identity, display_identity)
		assert initial.dirty_epoch > initial.consumed_epoch
		assert app.backend.native_operations.arm_proof()
		failure_sokol_generation = native_install_sokol_trace_for_test()!
		mut proof := &NativeEglFrameProof{}
		mut failure_phase := &NativeEglSokolPhaseState{}
		mut failure_phase_error := ''
		failure := app.with_scheduled_render_batch(fn [mut app, window, post_commit, mut proof, mut failure_phase] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			failure_phase.candidate_present = candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			failure_phase.acquisition_status = acquisition.status
			proof.lease = acquisition.lease
			proof.pass_snapshot = acquisition.snapshot
			proof.renderer_token = app.backend.native_operations.renderer_attempt_token
			proof.failed_target = native_egl_target_for_test(app, window)!
			proof.failed_ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				proof.failed_target.ticket_id)!
			if post_commit {
				proof.activation_start = app.backend.native_operations.next_ordinal
				app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut app, mut proof] () ! {
					proof.activation_end = app.backend.native_operations.next_ordinal
					expected_activation_end := proof.activation_start +
						native_verified_egl_binding_ordinal_span_for_test
					if proof.activation_end != expected_activation_end {
						return error('verified EGL activation binding consumed an unexpected ordinal span')
					}
					proof.finalize_end = proof.activation_end +
						native_verified_egl_binding_ordinal_span_for_test
					proof.callback_setup_end = proof.finalize_end
					if app.backend.kind == .wayland {
						proof.callback_setup_end += native_wayland_frame_callback_setup_ordinal_span_for_test
					}
					context := native_egl_frame_context_for_test(app, proof.lease,
						.window_finalize, .swap_buffers, proof.failed_target,
						proof.callback_setup_end)
					native_arm_egl_failure_for_test(mut app.backend.native_operations, context,
						0x300d)!
					proof.failure_context = context
				})!
			} else {
				context := native_egl_frame_context_for_test(app, acquisition.lease,
					.window_activate, .make_current, proof.failed_target,
					app.backend.native_operations.next_ordinal)
				native_arm_egl_failure_for_test(mut app.backend.native_operations, context, 0x300d)!
				proof.failure_context = context
				app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
			}
		}) or {
			failure_phase_error = err.msg()
			RenderBatchOutcome{}
		}
		failure_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(failure_sokol_generation)!
		failure_sokol_generation = 0
		assert failure_phase_error == ''
		assert failure_phase.candidate_present
		assert failure_phase.acquisition_status == .ready
		assert proof.renderer_token != 0
		assert proof.failed_target.generation != 0
		assert proof.failed_target.identity != 0
		assert proof.failed_target.ticket_id != 0
		if post_commit {
			assert proof.activation_start != 0
			assert proof.activation_end == proof.activation_start +
				native_verified_egl_binding_ordinal_span_for_test
			assert proof.finalize_end == proof.activation_end +
				native_verified_egl_binding_ordinal_span_for_test
			expected_callback_setup_end := if backend == .wayland {
				proof.finalize_end + native_wayland_frame_callback_setup_ordinal_span_for_test
			} else {
				proof.finalize_end
			}
			assert proof.callback_setup_end == expected_callback_setup_end
			assert proof.failure_context.ordinal == proof.callback_setup_end
		}
		if post_commit && backend == .x11 {
			native_assert_proof_drained_for_test(&app.backend.native_operations)
		}
		after_failure := app.render_window_snapshot(window)!
		invalidated := native_egl_target_for_test(app, window)!
		assert proof.failure_context.ordinal != 0
		assert failure.error == ''
		assert failure.finalized_submissions == 0
		assert invalidated.generation != 0
		assert invalidated.generation != proof.failed_target.generation
		assert invalidated.identity == 0
		assert invalidated.ticket_id == 0
		native_lifetime_ticket_assert_equal(initial_ticket, proof.failed_ticket)
		native_assert_bound_ticket_snapshot_for_test(proof.failed_ticket, .egl_surface,
			proof.failed_target.identity, display_identity)
		assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
			proof.failed_target.ticket_id) == -1
		assert after_failure.dirty_epoch > after_failure.consumed_epoch
		native_assert_egl_failure_capture_for_test(&app.backend.native_operations,
			proof.failure_context, 0x300d)
		failure_acceptance := native_acceptance_index_for_test(&app.backend.native_operations,
			proof.failure_context)
		release_start := native_assert_lifetime_release_sequence_for_test(&app.backend.native_operations,
			proof.failed_ticket.context, failure_acceptance + 1)
		assert release_start > failure_acceptance
		if post_commit {
			native_assert_sokol_sequence_for_test(failure_sokol, [
				multiwindow_sokol_trace.Operation.begin_swapchain_pass,
				.end_pass,
				.commit,
			])
			native_assert_authoritative_sokol_swapchain_for_test(app, failure_sokol, proof.lease,
				proof.pass_snapshot)!
			assert failure.committed
			assert failure.completed_user_passes == 1
			assert after_failure.frame_serial == initial.frame_serial + 1
			assert after_failure.submitted_frame == initial.submitted_frame
		} else {
			native_assert_sokol_sequence_for_test(failure_sokol, [])
			assert !failure.committed
			assert failure.completed_user_passes == 0
			assert after_failure.frame_serial == initial.frame_serial
			assert after_failure.submitted_frame == initial.submitted_frame
		}
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		before_failure_reset := native_proof_snapshot(&app.backend.native_operations)
		assert native_lifetime_trace_entry_count_for_test(before_failure_reset,
			proof.failed_ticket.context) == 6
		assert app.backend.native_operations.reset_proof()
		published_replacement := native_poll_until_replacement_for_test(mut app, window,
			proof.failed_target)!
		assert published_replacement.generation == invalidated.generation
		assert published_replacement.identity == 0
		assert app.backend.native_operations.reset_proof()
		retry_sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == retry_sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(retry_sokol_generation)
			}
		}
		mut retry_phase := &NativeEglSokolPhaseState{}
		mut retry_phase_error := ''
		retry := app.with_scheduled_render_batch(fn [mut app, window, mut proof, mut retry_phase] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			retry_phase.candidate_present = candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			retry_phase.acquisition_status = acquisition.status
			proof.retry_lease = acquisition.lease
			proof.retry_snapshot = acquisition.snapshot
			proof.replacement = native_egl_target_for_test(app, window)!
			proof.replacement_ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				proof.replacement.ticket_id)!
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		}) or {
			retry_phase_error = err.msg()
			RenderBatchOutcome{}
		}
		retry_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(retry_sokol_generation)!
		assert retry_phase_error == ''
		assert retry_phase.candidate_present
		assert retry_phase.acquisition_status == .ready
		after_retry := app.render_window_snapshot(window)!
		native_assert_sokol_sequence_for_test(retry_sokol, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		native_assert_authoritative_sokol_swapchain_for_test(app, retry_sokol, proof.retry_lease,
			proof.retry_snapshot)!
		assert retry.error == ''
		assert retry.committed
		assert retry.completed_user_passes == 1
		assert retry.finalized_submissions == 1
		assert proof.replacement.generation == published_replacement.generation
		assert proof.replacement.generation != proof.failed_target.generation
		assert proof.replacement.identity != 0
		assert proof.replacement.ticket_id != 0
		assert proof.replacement.ticket_id != proof.failed_target.ticket_id
		native_assert_bound_ticket_snapshot_for_test(proof.replacement_ticket, .egl_surface,
			proof.replacement.identity, display_identity)
		assert app.backend.native_operations.renderer_attempt_token == proof.renderer_token
		assert proof.failure_context.renderer_attempt_token == proof.renderer_token
		assert proof.retry_lease.batch_epoch != 0
		assert proof.retry_lease.window_epoch != 0
		assert proof.retry_lease.target_epoch != 0
		assert proof.retry_lease.batch_epoch != proof.lease.batch_epoch
		assert proof.retry_lease.window_epoch != proof.lease.window_epoch
		assert proof.retry_lease.target_epoch != proof.lease.target_epoch
		expected_retry_serial := initial.frame_serial + (if post_commit { u64(2) } else { u64(1) })
		assert after_retry.frame_serial == expected_retry_serial
		assert after_retry.submitted_frame == initial.submitted_frame + 1
		assert after_retry.consumed_epoch == after_retry.dirty_epoch
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		if !post_commit {
			pre_public_destroy := native_proof_snapshot(&app.backend.native_operations)
			assert !pre_public_destroy.trace_overflow
			native_assert_complete_native_trace_for_test(pre_public_destroy)
			pre_destroy_plan :=
				native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
			native_proof_assert_plan_equal(pre_destroy_plan, pre_public_destroy.plan)
			before_public_destroy := native_proof_snapshot(&app.backend.native_operations)
			assert before_public_destroy.trace_len == 0
			assert !before_public_destroy.trace_overflow
			assert before_public_destroy.next_ordinal == pre_public_destroy.next_ordinal
			assert before_public_destroy.proof_generation == pre_public_destroy.proof_generation
			assert before_public_destroy.next_proof_generation == pre_public_destroy.next_proof_generation
			assert before_public_destroy.proof_ordinal_floor == pre_public_destroy.proof_ordinal_floor
			native_proof_assert_plan_equal(pre_destroy_plan, before_public_destroy.plan)
			native_lifetime_registry_assert_snapshots_equal(pre_public_destroy.registry,
				before_public_destroy.registry)
			all_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
			mut recovered_window_tickets := []NativeLifetimeTicketProofSnapshot{}
			mut found_replacement_ticket := false
			for ticket in all_tickets {
				if ticket.owner_seed.window != window {
					continue
				}
				recovered_window_tickets << ticket
				if ticket.ticket_id == proof.replacement_ticket.ticket_id {
					native_lifetime_ticket_assert_equal(proof.replacement_ticket, ticket)
					found_replacement_ticket = true
				}
			}
			assert found_replacement_ticket
			before_public_destroy_oracle :=
				native_release_oracle_snapshot_for_test(app.backend.kind)
			app.destroy_window(window)!
			after_public_destroy := native_proof_snapshot(&app.backend.native_operations)
			assert !after_public_destroy.trace_overflow
			native_assert_complete_native_trace_for_test(after_public_destroy)
			assert after_public_destroy.trace_len > 0
			for ticket in recovered_window_tickets {
				assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
					ticket.ticket_id) == -1
			}
			native_assert_phase_a_ticket_retirement_for_test(after_public_destroy,
				recovered_window_tickets, 0)
			after_public_destroy_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
			native_release_oracle_assert_cleanup_bijection_for_test(before_public_destroy_oracle,
				after_public_destroy_oracle, recovered_window_tickets)
			assert !app.window_exists(window)
			_ = native_phase_a_assert_canonical_ticket_slots_for_test(app)
			_ =
				native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
			assert app.backend.native_operations.proof.trace_len == 0
			assert !app.backend.native_operations.proof.trace_overflow
			native_stop_twice_with_exact_proof_for_test(mut app)!
			return
		}
		before_stop := native_proof_snapshot(&app.backend.native_operations)
		assert !before_stop.trace_overflow
		native_assert_complete_native_trace_for_test(before_stop)
		assert native_lifetime_trace_entry_count_for_test(before_stop, proof.failed_ticket.context) == 0
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		assert app.backend.native_operations.proof.trace_len == 0
		assert !app.backend.native_operations.proof.trace_overflow
		native_proof_assert_plan_equal(consumed_plan, app.backend.native_operations.proof.plan)
		native_wayland_retire_pending_frame_callback_for_test(mut app, window)!
		callback_retirement := native_proof_snapshot(&app.backend.native_operations)
		assert !callback_retirement.trace_overflow
		native_assert_complete_native_trace_for_test(callback_retirement)
		native_proof_assert_plan_equal(consumed_plan, callback_retirement.plan)
		callback_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(consumed_plan, callback_plan)
		callback_cleared := native_proof_snapshot(&app.backend.native_operations)
		assert callback_cleared.trace_len == 0
		assert !callback_cleared.trace_overflow
		native_proof_assert_plan_equal(consumed_plan, callback_cleared.plan)
		native_lifetime_registry_assert_snapshots_equal(callback_retirement.registry,
			callback_cleared.registry)
		remaining_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		native_proof_assert_plan_equal(consumed_plan, stopped.plan)
		assert native_lifetime_trace_entry_count_for_test(stopped, proof.failed_ticket.context) == 0
		native_assert_phase_a_ticket_retirement_for_test(stopped, remaining_tickets, 0)
	}

	fn native_egl_exercise_bad_native_window() ! {
		backend := native_runtime_backend_for_test()!
		mut app := native_runtime_new_app_for_test(backend)!
		first_window := native_runtime_new_window_for_test(mut app, 'native EGL unaffected window')!
		first_target := native_egl_render_healthy_target_for_test(mut app, first_window)!
		first_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			first_target.ticket_id)!
		second_window := native_runtime_new_window_for_test(mut app, 'native EGL closed window')!
		_ = app.drain_events()!
		binding := native_egl_binding_for_test(app)!
		display_identity := native_egl_display_identity_for_test(app)
		wayland_display_identity := if backend == .wayland {
			native_wayland_display_identity_for_test(app)
		} else {
			u64(0)
		}
		assert display_identity != 0
		assert backend != .wayland || wayland_display_identity != 0
		assert binding.anchor.identity != 0
		assert binding.context != 0
		native_assert_bound_ticket_snapshot_for_test(first_ticket, .egl_surface,
			first_target.identity, display_identity)

		assert app.backend.native_operations.arm_proof()
		failure_sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if app.status() != .stopped {
				app.stop() or {}
			}
			if app.backend.native_operations.proof != unsafe { nil } {
				_ = app.backend.native_operations.disarm_proof()
			}
			if multiwindow_sokol_trace.active_generation() == failure_sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(failure_sokol_generation)
			}
		}
		mut proof := &NativeEglFrameProof{}
		mut failure_phase := &NativeEglSokolPhaseState{}
		mut failure_phase_error := ''
		outcome := app.with_scheduled_render_batch(fn [mut app, second_window, mut proof, mut failure_phase] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			failure_phase.candidate_present = candidates.any(it.window == second_window)
			acquisition := app.acquire_render_target(batch, second_window)!
			failure_phase.acquisition_status = acquisition.status
			proof.lease = acquisition.lease
			proof.failed_target = native_egl_target_for_test(app, second_window)!
			proof.failed_ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				proof.failed_target.ticket_id)!
			proof.platform_target_identity = native_wayland_egl_window_identity_for_test(app,
				second_window)
			proof.platform_ticket_id = native_wayland_egl_window_ticket_for_test(app, second_window)
			if proof.platform_ticket_id != 0 {
				proof.platform_ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
					proof.platform_ticket_id)!
			}
			context := native_egl_frame_context_for_test(app, acquisition.lease, .window_activate,
				.make_current, proof.failed_target, app.backend.native_operations.next_ordinal)
			native_arm_egl_failure_for_test(mut app.backend.native_operations, context, 0x300b)!
			proof.failure_context = context
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		}) or {
			failure_phase_error = err.msg()
			RenderBatchOutcome{}
		}
		failure_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(failure_sokol_generation)!
		failure_release := native_proof_snapshot(&app.backend.native_operations)
		assert failure_phase_error == ''
		assert failure_phase.candidate_present
		assert failure_phase.acquisition_status == .ready
		assert proof.failed_target.ticket_id != 0
		if backend == .wayland {
			assert proof.platform_target_identity != 0
			assert proof.platform_ticket_id != 0
			assert proof.platform_ticket.required_parent_identity != 0
			native_assert_bound_ticket_snapshot_for_test(proof.platform_ticket,
				.wayland_egl_window, proof.platform_target_identity,
				proof.platform_ticket.required_parent_identity)
		} else {
			assert proof.platform_target_identity == 0
			assert proof.platform_ticket_id == 0
		}
		assert outcome.error == ''
		assert !outcome.committed
		assert outcome.completed_user_passes == 0
		assert outcome.finalized_submissions == 0
		native_assert_sokol_sequence_for_test(failure_sokol, [])
		native_assert_egl_failure_capture_in_snapshot_for_test(failure_release,
			proof.failure_context, 0x300b)
		native_assert_bound_ticket_snapshot_for_test(proof.failed_ticket, .egl_surface,
			proof.failed_target.identity, display_identity)
		closed_target := native_egl_target_for_test(app, second_window)!
		assert closed_target.native_destroyed
		assert closed_target.identity == 0
		assert closed_target.ticket_id == 0
		assert closed_target.generation != proof.failed_target.generation
		assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
			proof.failed_target.ticket_id) == -1
		failure_acceptance := native_acceptance_index_in_snapshot_for_test(failure_release,
			proof.failure_context)
		failure_release_start := native_assert_lifetime_release_sequence_in_snapshot_for_test(failure_release,
			proof.failed_ticket.context, failure_acceptance + 1)
		assert failure_release_start == failure_acceptance + 2
		failure_release_end := failure_release_start + 6
		anchor_binding_end := native_assert_egl_anchor_binding_suffix_for_test(failure_release,
			failure_release_end, binding.anchor.generation, binding.anchor.identity,
			binding.context)
		assert anchor_binding_end == failure_release.trace_len
		assert native_lifetime_trace_entry_count_for_test(failure_release,
			proof.failed_ticket.context) == 6
		assert !failure_release.trace_overflow
		native_assert_complete_native_trace_for_test(failure_release)
		if backend == .wayland {
			live_platform_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				proof.platform_ticket.ticket_id)!
			native_lifetime_ticket_assert_equal(proof.platform_ticket, live_platform_ticket)
			assert native_lifetime_release_start_in_snapshot_for_test(failure_release,
				proof.platform_ticket.context) == -1
		}
		assert app.backend.renderer_health() == .ready
		assert app.renderer_is_usable()
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(consumed_plan, failure_release.plan)
		failure_cleared := native_proof_snapshot(&app.backend.native_operations)
		assert failure_cleared.trace_len == 0
		assert !failure_cleared.trace_overflow
		assert failure_cleared.proof_generation == failure_release.proof_generation
		assert failure_cleared.next_proof_generation == failure_release.next_proof_generation
		assert failure_cleared.proof_ordinal_floor == failure_release.proof_ordinal_floor
		assert failure_cleared.next_ordinal == failure_release.next_ordinal
		assert failure_cleared.proof_armed == failure_release.proof_armed
		assert failure_cleared.proof_accepting_plans == failure_release.proof_accepting_plans
		assert failure_cleared.sequence_exhausted == failure_release.sequence_exhausted
		assert failure_cleared.terminal_cause == failure_release.terminal_cause
		native_proof_assert_plan_equal(consumed_plan, failure_cleared.plan)
		native_lifetime_registry_assert_snapshots_equal(failure_release.registry,
			failure_cleared.registry)
		if backend == .wayland {
			live_platform_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				proof.platform_ticket.ticket_id)!
			native_lifetime_ticket_assert_equal(proof.platform_ticket, live_platform_ticket)
		}

		assert app.poll_events()! >= 1
		notices := app.drain_render_teardown_notices()!
		assert notices.len == 1
		assert notices[0].window == second_window
		delivery := native_proof_snapshot(&app.backend.native_operations)
		assert !delivery.trace_overflow
		native_assert_complete_native_trace_for_test(delivery)
		native_proof_assert_plan_equal(consumed_plan, delivery.plan)
		assert native_lifetime_trace_entry_count_for_test(delivery, proof.failed_ticket.context) == 0
		if backend == .wayland {
			assert native_lifetime_trace_entry_count_for_test(delivery,
				proof.platform_ticket.context) == 0
		}
		delivery_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(consumed_plan, delivery_plan)
		delivery_cleared := native_proof_snapshot(&app.backend.native_operations)
		assert delivery_cleared.trace_len == 0
		assert !delivery_cleared.trace_overflow
		assert delivery_cleared.proof_generation == delivery.proof_generation
		assert delivery_cleared.next_proof_generation == delivery.next_proof_generation
		assert delivery_cleared.proof_ordinal_floor == delivery.proof_ordinal_floor
		assert delivery_cleared.next_ordinal == delivery.next_ordinal
		native_proof_assert_plan_equal(consumed_plan, delivery_cleared.plan)
		native_lifetime_registry_assert_snapshots_equal(delivery.registry,
			delivery_cleared.registry)
		terminal_target := native_egl_target_for_test(app, second_window)!
		assert terminal_target.native_destroyed
		assert terminal_target.identity == 0
		assert terminal_target.ticket_id == 0
		assert terminal_target.generation == closed_target.generation
		app.finish_window_destroy(notices[0].ticket, [])!
		finish_release := native_proof_snapshot(&app.backend.native_operations)
		assert !finish_release.trace_overflow
		native_assert_complete_native_trace_for_test(finish_release)
		native_proof_assert_plan_equal(consumed_plan, finish_release.plan)
		assert native_lifetime_trace_entry_count_for_test(finish_release,
			proof.failed_ticket.context) == 0
		if backend == .wayland {
			assert proof.platform_ticket.owner_seed.target_generation != 0
			platform_release_end := native_assert_lifetime_release_at_for_test(finish_release,
				proof.platform_ticket, 0)
			assert platform_release_end == 6
			assert native_lifetime_trace_entry_count_for_test(finish_release,
				proof.platform_ticket.context) == 6
			native_assert_successful_wayland_shutdown_flush_for_test(finish_release,
				platform_release_end, wayland_display_identity, second_window,
				terminal_target.generation, delivery_cleared.next_ordinal)
			assert finish_release.trace_len == platform_release_end + 8
			assert finish_release.next_ordinal == delivery_cleared.next_ordinal + 2
			assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
				proof.platform_ticket.ticket_id) == -1
		}
		mut public_closed := 0
		for event in app.drain_events()! {
			if event.kind == .window_destroyed && event.window_id == second_window {
				public_closed++
			}
		}
		assert public_closed == 1
		assert !app.window_exists(second_window)
		assert app.window_exists(first_window)
		finish_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(consumed_plan, finish_plan)
		finish_cleared := native_proof_snapshot(&app.backend.native_operations)
		assert finish_cleared.trace_len == 0
		assert !finish_cleared.trace_overflow
		assert finish_cleared.proof_generation == finish_release.proof_generation
		assert finish_cleared.next_proof_generation == finish_release.next_proof_generation
		assert finish_cleared.proof_ordinal_floor == finish_release.proof_ordinal_floor
		assert finish_cleared.next_ordinal == finish_release.next_ordinal
		native_proof_assert_plan_equal(consumed_plan, finish_cleared.plan)
		native_lifetime_registry_assert_snapshots_equal(finish_release.registry,
			finish_cleared.registry)
		if backend == .wayland {
			$if linux && sokol_wayland ? {
				first_index := app.backend.wayland.window_record_index(first_window) or {
					return error(err_window_not_found)
				}
				first_record := app.backend.wayland.windows[first_index]
				if first_record.frame_callback != unsafe { nil }
					&& first_record.frame_callback_ticket != 0 {
					native_wayland_retire_pending_frame_callback_for_test(mut app, first_window)!
				} else {
					assert first_record.frame_callback == unsafe { nil }
					assert first_record.frame_callback_ticket == 0
					assert first_record.frame_ready
				}
				retained_target := native_egl_target_for_test(app, first_window)!
				assert retained_target.identity == first_target.identity
				assert retained_target.generation == first_target.generation
				assert retained_target.ticket_id == first_target.ticket_id
				readiness_segment := native_proof_snapshot(&app.backend.native_operations)
				assert !readiness_segment.trace_overflow
				native_assert_complete_native_trace_for_test(readiness_segment)
				readiness_plan :=
					native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
				native_proof_assert_plan_equal(readiness_segment.plan, readiness_plan)
				readiness_cleared := native_proof_snapshot(&app.backend.native_operations)
				assert readiness_cleared.trace_len == 0
				assert !readiness_cleared.trace_overflow
				assert readiness_cleared.proof_generation == readiness_segment.proof_generation
				assert readiness_cleared.next_proof_generation == readiness_segment.next_proof_generation
				assert readiness_cleared.proof_ordinal_floor == readiness_segment.proof_ordinal_floor
				assert readiness_cleared.next_ordinal == readiness_segment.next_ordinal
				native_proof_assert_plan_equal(readiness_plan, readiness_cleared.plan)
				_ = app.poll_events()!
				post_poll := native_proof_snapshot(&app.backend.native_operations)
				assert !post_poll.trace_overflow
				native_assert_complete_native_trace_for_test(post_poll)
				native_assert_proof_drained_for_test(&app.backend.native_operations)
				post_poll_plan :=
					native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
				native_proof_assert_plan_equal(post_poll.plan, post_poll_plan)
				post_poll_cleared := native_proof_snapshot(&app.backend.native_operations)
				assert post_poll_cleared.trace_len == 0
				assert !post_poll_cleared.trace_overflow
				assert post_poll_cleared.proof_generation == post_poll.proof_generation
				assert post_poll_cleared.next_proof_generation == post_poll.next_proof_generation
				assert post_poll_cleared.proof_ordinal_floor == post_poll.proof_ordinal_floor
				assert post_poll_cleared.next_ordinal == post_poll.next_ordinal
				native_proof_assert_plan_equal(post_poll_plan, post_poll_cleared.plan)
				native_lifetime_registry_assert_snapshots_equal(post_poll.registry,
					post_poll_cleared.registry)
				app.request_redraw(first_window)!
				assert app.render_window_eligible(first_window)!, 'unaffected Wayland EGL window did not become render eligible after deterministic callback retirement'
			} $else {
				return error(err_backend_unsupported)
			}
		} else {
			app.request_redraw(first_window)!
			for _ in 0 .. 128 {
				if app.render_window_eligible(first_window)! {
					break
				}
				app.poll_events()!
			}
			assert app.render_window_eligible(first_window)!, 'unaffected X11 EGL window did not become render eligible after bounded owner polling'
		}

		recovery_sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == recovery_sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(recovery_sokol_generation)
			}
		}
		mut healthy_state := &NativeEglHealthyBatchState{}
		mut recovery_phase := &NativeEglSokolPhaseState{}
		mut recovery_phase_error := ''
		healthy := app.with_scheduled_render_batch(fn [mut app, first_window, mut healthy_state, mut recovery_phase] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			recovery_phase.candidate_present = candidates.any(it.window == first_window)
			acquisition := app.acquire_render_target(batch, first_window)!
			recovery_phase.acquisition_status = acquisition.status
			healthy_state.lease = acquisition.lease
			healthy_state.snapshot = acquisition.snapshot
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		}) or {
			recovery_phase_error = err.msg()
			RenderBatchOutcome{}
		}
		healthy_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(recovery_sokol_generation)!
		assert recovery_phase_error == ''
		assert recovery_phase.candidate_present
		assert recovery_phase.acquisition_status == .ready
		assert healthy.error == ''
		assert healthy.committed
		assert healthy.finalized_submissions == 1
		native_assert_sokol_sequence_for_test(healthy_sokol, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		native_assert_authoritative_sokol_swapchain_for_test(app, healthy_sokol,
			healthy_state.lease, healthy_state.snapshot)!
		remaining_target := native_egl_target_for_test(app, first_window)!
		assert remaining_target.generation == first_target.generation
		assert remaining_target.identity == first_target.identity
		assert remaining_target.ticket_id == first_target.ticket_id
		native_assert_bound_ticket_snapshot_for_test(native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			remaining_target.ticket_id)!, .egl_surface, remaining_target.identity, display_identity)
		native_wayland_retire_pending_frame_callback_for_test(mut app, first_window)!
		_ = app.poll_events()!
		for event in app.drain_events()! {
			assert event.kind != .window_destroyed || event.window_id != second_window
		}
		remaining_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		recovery_render := native_proof_snapshot(&app.backend.native_operations)
		assert !recovery_render.trace_overflow
		native_assert_complete_native_trace_for_test(recovery_render)
		native_proof_assert_plan_equal(consumed_plan, recovery_render.plan)
		assert native_lifetime_trace_entry_count_for_test(recovery_render,
			proof.failed_ticket.context) == 0
		if backend == .wayland {
			assert native_lifetime_trace_entry_count_for_test(recovery_render,
				proof.platform_ticket.context) == 0
		}
		for ticket in remaining_tickets {
			assert native_lifetime_trace_entry_count_for_test(recovery_render, ticket.context) == 0
		}
		stop_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(consumed_plan, stop_plan)
		assert app.backend.native_operations.proof.trace_len == 0
		assert !app.backend.native_operations.proof.trace_overflow
		native_proof_assert_plan_equal(consumed_plan, app.backend.native_operations.proof.plan)
		app.shutdown_renderer()!
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		assert !stopped.trace_overflow
		native_proof_assert_plan_equal(consumed_plan, stopped.plan)
		assert native_lifetime_trace_entry_count_for_test(stopped, proof.failed_ticket.context) == 0
		if backend == .wayland {
			assert native_lifetime_trace_entry_count_for_test(stopped,
				proof.platform_ticket.context) == 0
		}
	}

	struct NativeTerminalChildOutput {
	mut:
		stdout string
		stderr string
	}

	fn native_terminal_drain_child_for_test(mut child os.Process, mut output NativeTerminalChildOutput) {
		for child.is_pending(.stdout) {
			chunk := child.stdout_read()
			if chunk == '' {
				break
			}
			output.stdout += chunk
		}
		for child.is_pending(.stderr) {
			chunk := child.stderr_read()
			if chunk == '' {
				break
			}
			output.stderr += chunk
		}
	}

	$if linux {
		@[markused]
		fn native_terminal_force_kill_reap_for_test(mut child os.Process, pid int) string {
			kill_result := C.kill(pid, C.SIGKILL)
			if kill_result != 0 {
				kill_errno := int(C.errno)
				if kill_errno != int(C.ESRCH) {
					child.code = -1
					child.status = .aborted
					return 'kill(${pid}, SIGKILL) failed, errno=${kill_errno}'
				}
			}
			mut wait_status := 0
			mut waited_pid := -1
			mut wait_errno := 0
			for {
				waited_pid = int(C.waitpid(pid, &wait_status, 0))
				if waited_pid == -1 {
					wait_errno = int(C.errno)
					if wait_errno == int(C.EINTR) {
						continue
					}
				}
				break
			}
			if waited_pid != pid {
				child.code = -1
				child.status = .aborted
				return 'waitpid(${pid}) returned ${waited_pid}, errno=${wait_errno}'
			}
			if C.WIFEXITED(wait_status) {
				child.code = int(C.WEXITSTATUS(wait_status))
				child.status = .exited
				return ''
			}
			if C.WIFSIGNALED(wait_status) {
				signal := int(C.WTERMSIG(wait_status))
				child.code = 128 + signal
				child.status = .aborted
				return ''
			}
			child.code = -1
			child.status = .aborted
			return 'waitpid(${pid}) returned undecodable status ${wait_status}'
		}
	}

	fn native_terminal_subprocess_for_test(marker string, label string) ! {
		mut environment := os.environ()
		environment.delete(native_egl_bad_display_child_marker)
		environment.delete(native_appkit_ordinal_exhaustion_child_marker)
		environment[marker] = '1'
		mut child := os.new_process(os.executable())
		child.set_environment(environment)
		child.set_redirect_stdio()
		child.run()
		started := child.status == .running && child.pid > 0
		mut output := NativeTerminalChildOutput{}
		mut timed_out := false
		mut force_killed := false
		mut reap_error := ''
		if started {
			started_at := time.now()
			execution_deadline := started_at.add(25 * time.second)
			escalation_deadline := started_at.add(29 * time.second)
			for child.is_alive() {
				native_terminal_drain_child_for_test(mut child, mut output)
				if time.now() >= execution_deadline {
					timed_out = true
					child.signal_term()
					break
				}
				time.sleep(5 * time.millisecond)
			}
			mut alive_at_escalation := false
			if timed_out {
				for child.is_alive() {
					native_terminal_drain_child_for_test(mut child, mut output)
					if time.now() >= escalation_deadline {
						alive_at_escalation = true
						break
					}
					time.sleep(5 * time.millisecond)
				}
			}
			$if linux {
				native_terminal_drain_child_for_test(mut child, mut output)
				if alive_at_escalation || child.status == .running {
					pid := child.pid
					force_killed = true
					reap_error = native_terminal_force_kill_reap_for_test(mut child, pid)
				}
				native_terminal_drain_child_for_test(mut child, mut output)
				output.stdout += child.stdout_slurp()
				output.stderr += child.stderr_slurp()
			} $else $if windows {
				if alive_at_escalation {
					child.signal_term()
				}
				native_terminal_drain_child_for_test(mut child, mut output)
				if child.status == .running {
					child.wait()
				}
				native_terminal_drain_child_for_test(mut child, mut output)
				output.stdout += child.stdout_slurp()
				output.stderr += child.stderr_slurp()
			} $else {
				if alive_at_escalation {
					child.signal_term()
				}
				native_terminal_drain_child_for_test(mut child, mut output)
				if child.status == .running {
					child.wait()
				}
				native_terminal_drain_child_for_test(mut child, mut output)
				output.stdout += child.stdout_slurp()
				output.stderr += child.stderr_slurp()
			}
		}
		exit_code := child.code
		child_status := child.status
		process_error := child.err
		child.close()
		diagnostics := 'status=${child_status} exit_code=${exit_code} force_killed=${force_killed} process_error=`${process_error}` reap_error=`${reap_error}`\nstdout:\n${output.stdout}\nstderr:\n${output.stderr}'
		if !started {
			return error('${label} child did not start\n${diagnostics}')
		}
		if timed_out {
			return error('${label} child timed out\n${diagnostics}')
		}
		if exit_code != 0 {
			return error('${label} child failed\n${diagnostics}')
		}
	}

	fn native_egl_bad_display_subprocess_for_test() ! {
		native_terminal_subprocess_for_test(native_egl_bad_display_child_marker,
			'synthetic EGL_BAD_DISPLAY')!
	}

	fn native_egl_exercise_terminal_failure(egl_error i64) ! {
		backend := native_runtime_backend_for_test()!
		mut app := native_runtime_new_app_for_test(backend)!
		title := if egl_error == 0x300b {
			'native EGL window loss'
		} else if egl_error == 0x3008 {
			'native EGL display loss'
		} else {
			'native EGL context loss'
		}
		window := native_runtime_new_window_for_test(mut app, title)!
		initial := app.render_window_snapshot(window)!
		baseline_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		assert baseline_tickets.len > 0
		assert app.backend.native_operations.arm_proof()
		app_identity := app.instance_id
		app_lifetime_token := app.backend.native_operations.app_lifetime_token
		native_proof_generation := app.backend.native_operations.proof.generation
		egl_display_identity := native_egl_display_identity_for_test(app)
		sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == sokol_generation {
				assert multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
			}
		}
		mut proof := &NativeEglFrameProof{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, egl_error, mut proof] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			proof.lease = acquisition.lease
			proof.failed_target = native_egl_target_for_test(app, window)!
			proof.pre_failure_tickets = native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
			if app.backend.kind == .wayland {
				proof.platform_ticket_id = native_wayland_egl_window_ticket_for_test(app, window)
				proof.platform_target_identity = native_wayland_egl_window_identity_for_test(app,
					window)
				proof.platform_parent_identity = native_wayland_surface_identity_for_test(app,
					window)
			}
			context := native_egl_frame_context_for_test(app, acquisition.lease, .window_activate,
				.make_current, proof.failed_target, app.backend.native_operations.next_ordinal)
			native_arm_egl_failure_for_test(mut app.backend.native_operations, context, egl_error)!
			proof.failure_context = context
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		failed_sokol := multiwindow_sokol_trace.typed_snapshot()
		failed := app.render_window_snapshot(window)!
		observed_health := app.backend.renderer_health()
		renderer_usable := app.renderer_is_usable()
		app.state_mutex.lock()
		captured_renderer_terminal := app.render_runtime.renderer_terminal
		app.state_mutex.unlock()
		after_loss_tickets := native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
		trace_after_loss := app.backend.native_operations.proof.trace_len
		mut blocked_error := ''
		app.with_scheduled_render_batch(fn (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {}) or {
			blocked_error = err.msg()
		}
		trace_after_blocked := app.backend.native_operations.proof.trace_len
		before_release_oracle := native_release_oracle_snapshot_for_test(backend)
		multiwindow_sokol_trace.reset()
		mut first_error := ''
		app.stop() or { first_error = err.msg() }
		first_release_oracle := native_release_oracle_snapshot_for_test(backend)
		stopped := native_proof_snapshot(&app.backend.native_operations)
		first_ownership := native_phase_a_backend_ownership_snapshot(app)
		first_status := app.status()
		first_reason := app.stop_terminal
		mut replay_error := ''
		app.stop() or { replay_error = err.msg() }
		replayed := native_proof_snapshot(&app.backend.native_operations)
		second_release_oracle := native_release_oracle_snapshot_for_test(backend)
		second_ownership := native_phase_a_backend_ownership_snapshot(app)
		second_status := app.status()
		second_reason := app.stop_terminal
		stopped_sokol := multiwindow_sokol_trace.typed_snapshot()
		lifetime_cleared := native_egl_lifetime_is_cleared_for_test(app)
		proof_disarmed := app.backend.native_operations.disarm_proof()
		sokol_uninstalled := multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
		sokol_generation_after_cleanup := multiwindow_sokol_trace.active_generation()

		assert egl_error in [i64(0x300e), i64(0x3008)]
		assert first_status == .stopped
		assert second_status == .stopped
		expected_stop_terminal := match backend {
			.x11 {
				''
			}
			.wayland {
				'${err_render_terminal_aggregate}: ${err_render_terminal_aggregate}: ${err_wayland_flush_failed}'
			}
			else {
				'native EGL terminal failure selected unsupported backend'
			}
		}

		assert first_error == expected_stop_terminal
		assert first_reason == expected_stop_terminal
		assert replay_error == expected_stop_terminal
		assert second_reason == expected_stop_terminal
		assert !stopped.trace_overflow
		for entry in stopped.plan {
			assert !entry.armed
		}
		native_assert_complete_native_trace_for_test(stopped)
		native_proof_assert_snapshots_equal(stopped, replayed)
		native_phase_a_backend_assert_snapshots_equal(first_ownership, second_ownership)
		native_release_oracle_assert_equal_for_test(first_release_oracle, second_release_oracle)
		native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
			first_release_oracle, after_loss_tickets)
		assert stopped.registry.tickets.len == 0
		assert proof_disarmed
		assert sokol_uninstalled
		assert sokol_generation_after_cleanup == 0
		assert lifetime_cleared
		native_assert_sokol_sequence_for_test(failed_sokol, [])
		native_assert_sokol_sequence_for_test(stopped_sokol, [])
		native_assert_terminal_failure_acquisition_delta_for_test(backend, baseline_tickets,
			proof.pre_failure_tickets, proof, app_identity, app_lifetime_token,
			native_proof_generation, egl_display_identity)!
		native_assert_terminal_failure_ticket_transition_for_test(proof.pre_failure_tickets,
			after_loss_tickets, egl_error)!
		native_assert_egl_failure_capture_in_snapshot_for_test(stopped, proof.failure_context,
			egl_error)
		acceptance_index := native_acceptance_index_in_snapshot_for_test(stopped,
			proof.failure_context)
		assert failed.frame_serial == initial.frame_serial
		assert failed.submitted_frame == initial.submitted_frame
		assert failed.dirty_epoch > failed.consumed_epoch
		expected_native_error := if egl_error == 0x300e {
			err_render_native_renderer_lost
		} else {
			err_render_native_renderer_unavailable
		}
		expected_terminal := '${err_render_terminal_aggregate}: ${expected_native_error}'
		assert outcome.error == expected_terminal
		assert captured_renderer_terminal == expected_terminal
		native_assert_terminal_causes_once_for_test(outcome.error)
		assert !outcome.committed
		assert outcome.finalized_submissions == 0
		expected_health := if egl_error == 0x300e {
			NativeRendererHealth.lost
		} else {
			NativeRendererHealth.unavailable
		}
		assert observed_health == expected_health
		assert !renderer_usable
		assert blocked_error == '${err_render_renderer_failed}: ${outcome.error}'
		assert trace_after_blocked == trace_after_loss
		native_assert_phase_a_ticket_retirement_for_test(stopped, after_loss_tickets,

			acceptance_index + 1)
		native_assert_only_phase_a_ticket_releases_for_test(stopped, after_loss_tickets,
			trace_after_loss)
		native_assert_phase_a_ticket_release_health_for_test(stopped, after_loss_tickets,
			expected_health)
	}

	fn native_egl_render_healthy_target_for_test(mut app App, window WindowId) !NativeEglTargetProof {
		mut state := &NativeEglHealthyTargetState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			state.target = native_egl_target_for_test(app, window)!
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		assert outcome.error == ''
		assert outcome.committed
		assert outcome.finalized_submissions == 1
		assert state.target.generation != 0
		assert state.target.identity != 0
		assert state.target.ticket_id != 0
		return state.target
	}

	fn native_egl_exercise_bad_current_surface() ! {
		backend := native_runtime_backend_for_test()!
		mut app := native_runtime_new_app_for_test(backend)!
		first_window := native_runtime_new_window_for_test(mut app, 'native EGL prior binding')!
		first_target := native_egl_render_healthy_target_for_test(mut app, first_window)!
		first_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			first_target.ticket_id)!
		before := native_egl_binding_for_test(app)!
		assert before.binding.kind == .window
		assert before.binding.window == first_window
		assert before.binding.target_generation == first_target.generation
		assert native_identity(before.binding.surface) == first_target.identity
		assert !before.recovery_used

		second_window := native_runtime_new_window_for_test(mut app, 'native EGL failed binding')!
		baseline_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		app_identity := app.instance_id
		app_lifetime_token := app.backend.native_operations.app_lifetime_token
		display_identity := native_egl_display_identity_for_test(app)
		assert app.backend.native_operations.arm_proof()
		generation_g := app.backend.native_operations.proof.generation
		assert generation_g != 0
		sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
			}
		}
		mut first_failure := &NativeEglFrameProof{}
		first_outcome := app.with_scheduled_render_batch(fn [mut app, second_window, mut first_failure, generation_g] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == second_window)
			acquisition := app.acquire_render_target(batch, second_window)!
			assert acquisition.status == .ready
			first_failure.lease = acquisition.lease
			first_failure.failed_target = native_egl_target_for_test(app, second_window)!
			first_failure.pre_failure_tickets = native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
			first_failure.failed_ticket = native_ticket_snapshot_by_id_for_test(first_failure.pre_failure_tickets,
				first_failure.failed_target.ticket_id)!
			if app.backend.kind == .wayland {
				first_failure.platform_ticket_id = native_wayland_egl_window_ticket_for_test(app,
					second_window)
				first_failure.platform_target_identity = native_wayland_egl_window_identity_for_test(app,
					second_window)
				first_failure.platform_parent_identity = native_wayland_surface_identity_for_test(app,
					second_window)
				first_failure.platform_ticket = native_ticket_snapshot_by_id_for_test(first_failure.pre_failure_tickets,
					first_failure.platform_ticket_id)!
			}
			post_acquisition_ordinal := app.backend.native_operations.next_ordinal
			assert app.backend.native_operations.has_live_tickets_for_proof_generation(generation_g)
			native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
			assert app.backend.native_operations.proof != unsafe { nil }
			assert app.backend.native_operations.proof.generation == generation_g
			assert app.backend.native_operations.proof.trace_len == 0
			assert !app.backend.native_operations.proof.trace_overflow
			assert app.backend.native_operations.proof.accepting_plans
			assert !app.backend.native_operations.has_pending_native_plans()
			assert app.backend.native_operations.has_live_tickets_for_proof_generation(generation_g)
			assert app.backend.native_operations.next_ordinal == post_acquisition_ordinal
			context := native_egl_frame_context_for_test(app, acquisition.lease, .window_activate,
				.make_current, first_failure.failed_target, post_acquisition_ordinal)
			native_arm_egl_failure_for_test(mut app.backend.native_operations, context, 0x3007)!
			first_failure.failure_context = context
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		assert first_outcome.error == ''
		assert !first_outcome.committed
		native_assert_terminal_failure_acquisition_delta_for_test(backend, baseline_tickets,
			first_failure.pre_failure_tickets, first_failure, app_identity, app_lifetime_token,
			generation_g, display_identity)!
		native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
		native_assert_egl_failure_capture_for_test(&app.backend.native_operations,
			first_failure.failure_context, 0x3007)
		queries := native_egl_actual_binding_queries_for_test(&app.backend.native_operations)
		assert queries.len == 9
		assert queries[0].context.operation == .current_draw_query
		assert queries[0].actual.handle == first_failure.failed_target.identity
		assert queries[1].context.operation == .current_read_query
		assert queries[1].actual.handle == first_failure.failed_target.identity
		assert queries[2].context.operation == .current_context_query
		assert queries[2].actual.handle == before.context
		recovered := native_egl_binding_for_test(app)!
		assert recovered.recovery_used
		assert recovered.health == .ready
		assert recovered.anchor.generation != 0
		assert recovered.anchor.identity != 0
		assert queries[3].actual.handle == recovered.anchor.identity
		assert queries[4].actual.handle == recovered.anchor.identity
		assert queries[5].actual.handle == recovered.context
		assert queries[6].actual.handle == recovered.anchor.identity
		assert queries[7].actual.handle == recovered.anchor.identity
		assert queries[8].actual.handle == recovered.context
		assert recovered.binding.kind == .anchor
		assert recovered.binding.target_generation == recovered.anchor.generation
		assert native_identity(recovered.binding.surface) == recovered.anchor.identity
		retained_previous := native_egl_target_for_test(app, first_window)!
		invalidated_failed := native_egl_target_for_test(app, second_window)!
		assert retained_previous.identity == first_target.identity
		assert retained_previous.generation == first_target.generation
		assert retained_previous.ticket_id == first_target.ticket_id
		assert invalidated_failed.identity == 0
		assert invalidated_failed.ticket_id == 0
		assert invalidated_failed.generation != first_failure.failed_target.generation
		native_assert_bound_ticket_snapshot_for_test(first_failure.failed_ticket, .egl_surface,
			first_failure.failed_target.identity, display_identity)
		if backend == .wayland {
			live_platform_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				first_failure.platform_ticket_id)!
			native_lifetime_ticket_assert_equal(first_failure.platform_ticket, live_platform_ticket)
			assert native_lifetime_release_start_for_test(&app.backend.native_operations,
				first_failure.platform_ticket.context) == -1
		}
		first_phase := native_proof_snapshot(&app.backend.native_operations)
		first_acceptance_index := native_acceptance_index_in_snapshot_for_test(first_phase,
			first_failure.failure_context)
		first_release_start := native_assert_lifetime_release_sequence_in_snapshot_for_test(first_phase,
			first_failure.failed_ticket.context, first_acceptance_index + 1)
		assert first_release_start > first_acceptance_index
		assert native_lifetime_trace_entry_count_for_test(first_phase,
			first_failure.failed_ticket.context) == 6
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		generation_before_first_clear := app.backend.native_operations.proof.generation
		generation_g_live_before_first_clear :=
			app.backend.native_operations.has_live_tickets_for_proof_generation(generation_g)
		native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
		assert app.backend.native_operations.proof != unsafe { nil }
		assert app.backend.native_operations.proof.trace_len == 0
		assert !app.backend.native_operations.proof.trace_overflow
		assert app.backend.native_operations.proof.accepting_plans
		assert !app.backend.native_operations.has_pending_native_plans()
		assert generation_before_first_clear == generation_g
		match backend {
			.x11 {
				assert !generation_g_live_before_first_clear
				assert app.backend.native_operations.proof.generation == generation_g + 1
			}
			.wayland {
				assert generation_g_live_before_first_clear
				assert app.backend.native_operations.proof.generation == generation_g
				assert app.backend.native_operations.has_live_tickets_for_proof_generation(generation_g)
			}
			else {
				assert false, 'BAD_CURRENT_SURFACE selected unsupported backend'
			}
		}

		_ = native_poll_until_replacement_for_test(mut app, second_window,
			first_failure.failed_target)!
		if backend == .wayland {
			$if linux && sokol_wayland ? {
				first_index := app.backend.wayland.window_record_index(first_window) or {
					return error(err_window_not_found)
				}
				first_record := app.backend.wayland.windows[first_index]
				if first_record.frame_callback != unsafe { nil }
					&& first_record.frame_callback_ticket != 0 {
					native_wayland_retire_pending_frame_callback_for_test(mut app, first_window)!
				} else {
					assert first_record.frame_callback == unsafe { nil }
					assert first_record.frame_callback_ticket == 0
					assert first_record.frame_ready
				}
				retained_target := native_egl_target_for_test(app, first_window)!
				assert retained_target.identity == first_target.identity
				assert retained_target.generation == first_target.generation
				assert retained_target.ticket_id == first_target.ticket_id
				readiness_segment := native_proof_snapshot(&app.backend.native_operations)
				assert !readiness_segment.trace_overflow
				native_assert_complete_native_trace_for_test(readiness_segment)
				readiness_plan :=
					native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
				native_proof_assert_plan_equal(readiness_segment.plan, readiness_plan)
				readiness_cleared := native_proof_snapshot(&app.backend.native_operations)
				assert readiness_cleared.trace_len == 0
				assert !readiness_cleared.trace_overflow
				assert readiness_cleared.proof_generation == readiness_segment.proof_generation
				assert readiness_cleared.next_proof_generation == readiness_segment.next_proof_generation
				assert readiness_cleared.proof_ordinal_floor == readiness_segment.proof_ordinal_floor
				assert readiness_cleared.next_ordinal == readiness_segment.next_ordinal
				native_proof_assert_plan_equal(readiness_plan, readiness_cleared.plan)
				_ = app.poll_events()!
				post_poll := native_proof_snapshot(&app.backend.native_operations)
				assert !post_poll.trace_overflow
				native_assert_complete_native_trace_for_test(post_poll)
				native_assert_proof_drained_for_test(&app.backend.native_operations)
				post_poll_plan :=
					native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
				native_proof_assert_plan_equal(post_poll.plan, post_poll_plan)
				post_poll_cleared := native_proof_snapshot(&app.backend.native_operations)
				assert post_poll_cleared.trace_len == 0
				assert !post_poll_cleared.trace_overflow
				assert post_poll_cleared.proof_generation == post_poll.proof_generation
				assert post_poll_cleared.next_proof_generation == post_poll.next_proof_generation
				assert post_poll_cleared.proof_ordinal_floor == post_poll.proof_ordinal_floor
				assert post_poll_cleared.next_ordinal == post_poll.next_ordinal
				native_proof_assert_plan_equal(post_poll_plan, post_poll_cleared.plan)
				native_lifetime_registry_assert_snapshots_equal(post_poll.registry,
					post_poll_cleared.registry)
				app.request_redraw(first_window)!
				assert app.render_window_eligible(first_window)!, 'retained Wayland EGL window did not become render eligible after deterministic callback retirement'
			} $else {
				return error(err_backend_unsupported)
			}
		} else {
			app.request_redraw(first_window)!
			for _ in 0 .. 128 {
				if app.render_window_eligible(first_window)! {
					break
				}
				app.poll_events()!
			}
			assert app.render_window_eligible(first_window)!, 'retained EGL window did not become render eligible after bounded owner polling'
		}
		multiwindow_sokol_trace.reset()
		mut retained_state := &NativeEglHealthyBatchState{}
		retained_outcome := app.with_scheduled_render_batch(fn [mut app, first_window, mut retained_state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == first_window)
			acquisition := app.acquire_render_target(batch, first_window)!
			assert acquisition.status == .ready
			retained_state.lease = acquisition.lease
			retained_state.snapshot = acquisition.snapshot
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		assert retained_outcome.error == ''
		assert retained_outcome.committed
		assert retained_outcome.finalized_submissions == 1
		retained_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_assert_sokol_sequence_for_test(retained_sokol, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		native_assert_authoritative_sokol_swapchain_for_test(app, retained_sokol,
			retained_state.lease, retained_state.snapshot)!
		still_usable := native_egl_target_for_test(app, first_window)!
		assert still_usable.identity == first_target.identity
		assert still_usable.generation == first_target.generation
		assert still_usable.ticket_id == first_target.ticket_id
		native_assert_bound_ticket_snapshot_for_test(native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			still_usable.ticket_id)!, .egl_surface, still_usable.identity, display_identity)
		native_wayland_retire_pending_frame_callback_for_test(mut app, first_window)!
		generation_before_second_clear := app.backend.native_operations.proof.generation
		current_generation_live_before_second_clear :=
			app.backend.native_operations.has_live_tickets_for_proof_generation(generation_before_second_clear)
		native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
		assert app.backend.native_operations.proof != unsafe { nil }
		assert app.backend.native_operations.proof.trace_len == 0
		assert !app.backend.native_operations.proof.trace_overflow
		assert app.backend.native_operations.proof.accepting_plans
		assert !app.backend.native_operations.has_pending_native_plans()
		match backend {
			.x11 {
				assert generation_before_second_clear == generation_g + 1
				assert !current_generation_live_before_second_clear
				assert app.backend.native_operations.proof.generation ==
					generation_before_second_clear + 1
			}
			.wayland {
				assert generation_before_second_clear == generation_g
				assert current_generation_live_before_second_clear
				assert app.backend.native_operations.proof.generation == generation_g
				assert app.backend.native_operations.has_live_tickets_for_proof_generation(generation_g)
			}
			else {
				assert false, 'BAD_CURRENT_SURFACE selected unsupported backend'
			}
		}

		multiwindow_sokol_trace.reset()
		mut second_failure := &NativeEglFrameProof{}
		second_outcome := app.with_scheduled_render_batch(fn [mut app, second_window, mut second_failure] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == second_window)
			acquisition := app.acquire_render_target(batch, second_window)!
			assert acquisition.status == .ready
			second_failure.lease = acquisition.lease
			second_failure.failed_target = native_egl_target_for_test(app, second_window)!
			second_failure.failed_ticket = native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				second_failure.failed_target.ticket_id)!
			post_acquisition_ordinal := app.backend.native_operations.next_ordinal
			context := native_egl_frame_context_for_test(app, acquisition.lease, .window_activate,
				.make_current, second_failure.failed_target, post_acquisition_ordinal)
			native_arm_egl_failure_for_test(mut app.backend.native_operations, context, 0x3007)!
			second_failure.failure_context = context
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		expected_renderer_terminal := '${err_render_terminal_aggregate}: ${err_render_native_renderer_unavailable}'
		assert second_outcome.error == expected_renderer_terminal
		assert app.render_runtime.renderer_terminal == expected_renderer_terminal
		native_assert_terminal_causes_once_for_test(second_outcome.error)
		assert !second_outcome.committed
		second_acceptance_index := native_acceptance_index_for_test(&app.backend.native_operations,
			second_failure.failure_context)
		second_trace := native_proof_snapshot(&app.backend.native_operations)
		anchor_cursor := native_assert_egl_anchor_prepare_prefix_for_test(backend, second_trace,
			recovered.anchor.generation, recovered.anchor.identity, recovered.context)
		assert anchor_cursor == 20
		second_queries := native_egl_actual_binding_queries_for_test(&app.backend.native_operations)
		assert second_queries.len == 3
		anchor_bind_index := 0
		for index, operation in [NativeRenderOperation.current_draw_query, .current_read_query,
			.current_context_query] {
			query := second_queries[index]
			assert query.context.operation == operation
			assert query.context.call_site == .anchor_prepare
			assert query.context.scope == .anchor
			assert query.context.target_generation == recovered.anchor.generation
			assert query.context.target_identity == 0
			assert query.context.ordinal == second_trace.trace[anchor_bind_index].context.ordinal +
				u64(index + 2)
			assert query.context.ordinal < second_failure.failure_context.ordinal
		}
		assert second_acceptance_index >= anchor_cursor
		after_second_binding := native_egl_binding_for_test(app)!
		assert after_second_binding.recovery_used
		assert after_second_binding.anchor.generation == recovered.anchor.generation
		assert after_second_binding.anchor.identity == recovered.anchor.identity
		assert after_second_binding.context == recovered.context
		assert app.backend.renderer_health() == .unavailable
		assert !app.renderer_is_usable()
		native_assert_bound_ticket_snapshot_for_test(second_failure.failed_ticket, .egl_surface,
			second_failure.failed_target.identity, display_identity)
		native_assert_bound_ticket_snapshot_for_test(native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			second_failure.failed_target.ticket_id)!, .egl_surface,
			second_failure.failed_target.identity, display_identity)
		assert native_lifetime_release_start_for_test(&app.backend.native_operations,
			second_failure.failed_ticket.context) == -1
		remaining_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		if backend == .wayland {
			remaining_platform_ticket := native_ticket_snapshot_by_id_for_test(remaining_tickets,
				first_failure.platform_ticket_id)!
			native_lifetime_ticket_assert_equal(first_failure.platform_ticket,
				remaining_platform_ticket)
			assert native_lifetime_release_start_for_test(&app.backend.native_operations,
				first_failure.platform_ticket.context) == -1
		}
		trace_after_second_loss := app.backend.native_operations.proof.trace_len
		second_failure_sokol := multiwindow_sokol_trace.typed_snapshot()
		first_error, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		expected_stop_terminal := match backend {
			.x11 {
				''
			}
			.wayland {
				'${err_render_terminal_aggregate}: ${err_render_terminal_aggregate}: ${err_wayland_flush_failed}'
			}
			else {
				'BAD_CURRENT_SURFACE selected unsupported backend'
			}
		}

		assert first_error == expected_stop_terminal
		assert app.stop_terminal == expected_stop_terminal
		stopped_sokol := multiwindow_sokol_trace.typed_snapshot()
		sokol_uninstalled := multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
		sokol_generation_after_cleanup := multiwindow_sokol_trace.active_generation()
		assert sokol_uninstalled
		assert sokol_generation_after_cleanup == 0
		native_assert_sokol_sequence_for_test(second_failure_sokol, [])
		native_assert_sokol_sequence_for_test(stopped_sokol, [])
		assert native_egl_lifetime_is_cleared_for_test(app)
		native_assert_phase_a_ticket_retirement_for_test(stopped, remaining_tickets,

			second_acceptance_index + 1)
		native_assert_only_phase_a_ticket_releases_for_test(stopped, remaining_tickets,
			trace_after_second_loss)
		native_assert_phase_a_ticket_release_health_for_test(stopped, remaining_tickets,
			.unavailable)
		native_assert_lifetime_release_sequence_in_snapshot_for_test(stopped,
			second_failure.failed_ticket.context, second_acceptance_index + 1)
		native_assert_lifetime_release_sequence_in_snapshot_for_test(stopped, first_ticket.context,

			second_acceptance_index + 1)
		assert native_lifetime_trace_entry_count_for_test(stopped,
			first_failure.failed_ticket.context) == 0
		if backend == .wayland {
			native_assert_lifetime_release_sequence_in_snapshot_for_test(stopped,
				first_failure.platform_ticket.context, trace_after_second_loss)
			assert native_lifetime_trace_entry_count_for_test(stopped,
				first_failure.platform_ticket.context) == 6
		}
	}

	fn native_wayland_display_identity_for_test(app &App) u64 {
		$if linux && sokol_wayland ? {
			return native_identity(app.backend.wayland.display)
		} $else {
			_ = app
			return 0
		}
	}

	fn native_wayland_primary_context_for_test(app &App, operation NativeRenderOperation, seed NativeOperationSeed, ordinal u64) NativeOperationContext {
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          seed.presence_mask
			domain:                 .wayland
			operation:              operation
			call_site:              seed.call_site
			scope:                  seed.scope
			window:                 seed.window
			target_generation:      seed.target_generation
			target_identity:        seed.target_identity
			batch_epoch:            seed.batch_epoch
			window_lease_epoch:     seed.window_lease_epoch
			target_lease_epoch:     seed.target_lease_epoch
			ordinal:                ordinal
		}
	}

	fn native_wayland_quiescent_poll_template_for_test(mut app App) !NativeOperationContext {
		for _ in 0 .. 8 {
			native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
			outcome := app.backend.wayland.dispatch_pending_nonblocking()
			if outcome.blocks_graphics() {
				return native_render_error(outcome)
			}
			if context := native_find_real_operation_context_for_test(&app.backend.native_operations,
				.display_poll)
			{
				assert !app.backend.native_operations.proof.trace_overflow
				native_assert_wayland_prepare_balance_for_test(&app.backend.native_operations)
				return context
			}
		}
		return error('Wayland transport did not reach a real display_poll operation')
	}

	fn native_wayland_transport_seed_for_test(app &App) NativeOperationSeed {
		return NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: native_wayland_display_identity_for_test(app)
		}
	}

	fn native_wayland_exercise_prepare_failure_and_retry() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		app.backend.native_operations.arm_proof()
		_ = native_wayland_quiescent_poll_template_for_test(mut app)!
		native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
		seed := native_wayland_transport_seed_for_test(app)
		assert seed.target_identity != 0
		injected := NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value | native_valid_errno
			return_value: -1
			native_errno: 11
		}
		failure_context := native_wayland_primary_context_for_test(app, .display_prepare, seed,

			app.backend.native_operations.next_ordinal + 4)
		native_arm_wayland_primitive_for_test(mut app, failure_context, injected, 0)!
		failure := app.backend.wayland.dispatch_pending_nonblocking()
		assert failure.disposition == .transient
		assert native_operation_contexts_identical(failure.context, failure_context)
		assert app.backend.renderer_health() == .ready
		native_assert_wayland_injected_failure_for_test(&app.backend.native_operations,
			failure_context, injected, .transient)
		native_assert_wayland_prepare_balance_for_test(&app.backend.native_operations)
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_cancel) == 1
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_read) == 0
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)

		mut retry_context := NativeOperationContext{}
		for _ in 0 .. 8 {
			retry := app.backend.wayland.dispatch_pending_nonblocking()
			assert retry.succeeded()
			assert app.backend.renderer_health() == .ready
			native_assert_wayland_prepare_balance_for_test(&app.backend.native_operations)
			native_proof_assert_plan_equal(consumed_plan, app.backend.native_operations.proof.plan)
			if native_wayland_has_successful_acceptance_after_for_test(&app.backend.native_operations,
				.display_prepare, 0)
			{
				retry_context = native_assert_wayland_uninjected_success_after_for_test(&app.backend.native_operations,
					.display_prepare, 0)!
				break
			}
			_ =
				native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
			time.sleep(time.millisecond)
		}
		if retry_context.ordinal == 0 {
			return error('Wayland display_prepare retry did not reach a successful native boundary')
		}
		assert retry_context.ordinal != failure_context.ordinal
		native_proof_assert_plan_equal(consumed_plan, app.backend.native_operations.proof.plan)
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
		native_stop_twice_with_exact_proof_for_test(mut app)!
	}

	fn native_wayland_finish_read_retry_for_test(mut app App, failure NativeRenderResult, failure_context NativeOperationContext, injected NativePrimitiveEvidence) ! {
		assert failure.disposition == .operation_failed
		assert native_operation_contexts_identical(failure.context, failure_context)
		assert app.backend.renderer_health() == .ready
		native_assert_wayland_injected_failure_for_test(&app.backend.native_operations,
			failure_context, injected, .operation_failed)
		native_assert_wayland_prepare_balance_for_test(&app.backend.native_operations)
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_read) == 1
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_cancel) == 0
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_read) == 0
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_cancel) == 0

		$if linux && sokol_wayland ? {
			display_identity := native_identity(app.backend.wayland.display)
			assert display_identity != 0
			assert failure_context.target_identity == display_identity
			recovery := native_wayland_dispatch_sync_for_test(mut app)!
			assert recovery.outcome.succeeded()
			assert !recovery.snapshot.trace_overflow
			native_assert_complete_native_trace_for_test(recovery.snapshot)
			native_assert_proof_snapshot_drained_for_test(recovery.snapshot)
			assert native_real_call_count_in_snapshot_for_test(recovery.snapshot, .display_read) == recovery.read_count
			assert native_real_call_count_in_snapshot_for_test(recovery.snapshot, .display_cancel) == 1
			retry_context := native_assert_wayland_uninjected_success_after_in_snapshot_for_test(recovery.snapshot,
				.display_read, 0)!
			assert native_operation_contexts_identical(retry_context, recovery.read_context)
			expected_retry_context := native_wayland_primary_context_for_test(app, .display_read, NativeOperationSeed{
				presence_mask:   native_context_has_target_identity
				call_site:       .display_transport
				scope:           .batch
				target_identity: display_identity
			}, recovery.read_context.ordinal)
			assert native_operation_contexts_identical(retry_context, expected_retry_context)
			native_assert_wayland_compound_capture_in_snapshot_for_test(recovery.snapshot,
				retry_context, .ok, 0, 0)
			assert retry_context.ordinal != failure_context.ordinal
			native_assert_wayland_prepare_balance_in_snapshot_for_test(recovery.snapshot)
			native_proof_assert_plan_equal(consumed_plan, recovery.snapshot.plan)
			native_proof_assert_plan_equal(consumed_plan, app.backend.native_operations.proof.plan)
			native_assert_proof_drained_for_test(&app.backend.native_operations)
			recovery_plan :=
				native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
			native_proof_assert_plan_equal(consumed_plan, recovery_plan)
			native_stop_twice_with_exact_proof_for_test(mut app)!
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_exercise_read_failure_and_retry() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window :=
			native_runtime_new_window_for_test(mut app, 'native Wayland display_read failure')!
		_ = native_egl_render_healthy_target_for_test(mut app, window)!
		app.backend.native_operations.arm_proof()
		injected := NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value | native_valid_errno
			return_value: -1
			native_errno: 5
		}
		_ = native_wayland_quiescent_poll_template_for_test(mut app)!
		native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
		failure, failure_context := native_wayland_dispatch_injected_read_at_sync_boundary_for_test(mut app,
			injected)!
		native_wayland_finish_read_retry_for_test(mut app, failure, failure_context, injected)!
	}

	fn native_wayland_exercise_transport_evidence() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		app.backend.native_operations.arm_proof()
		display_identity := native_wayland_display_identity_for_test(app)
		assert display_identity != 0
		flush_seed := NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: display_identity
		}
		for item in [
			NativeWaylandErrnoCase{
				native_errno: 11
				disposition:  .ok
			},
			NativeWaylandErrnoCase{
				native_errno: 4
				disposition:  .operation_failed
			},
			NativeWaylandErrnoCase{
				native_errno: 32
				disposition:  .operation_failed
			},
		] {
			native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
			context := native_wayland_primary_context_for_test(app, .display_flush, flush_seed,
				app.backend.native_operations.next_ordinal)
			native_arm_wayland_primitive_for_test(mut app, context, NativePrimitiveEvidence{
				valid_mask:   native_valid_return_value | native_valid_errno
				return_value: -1
				native_errno: item.native_errno
			}, 0)!
			outcome := app.backend.wayland.attempt_wayland_flush(NativeOperationSeed{
				call_site: .display_transport
				scope:     .batch
			})
			assert outcome.disposition == item.disposition
			native_assert_wayland_compound_capture_for_test(&app.backend.native_operations,
				context, item.disposition, item.native_errno, 0)
			native_assert_proof_drained_for_test(&app.backend.native_operations)
		}
		assert app.backend.renderer_health() == .ready

		poll_template := native_wayland_quiescent_poll_template_for_test(mut app)!
		for native_errno in [i64(11), i64(4), i64(32)] {
			native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
			base := app.backend.native_operations.next_ordinal
			poll_context := NativeOperationContext{
				...poll_template
				renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
				app_identity:           app.instance_id
				ordinal:                base + native_wayland_poll_ordinal_offset_for_test
			}
			native_arm_wayland_primitive_for_test(mut app, poll_context, NativePrimitiveEvidence{
				valid_mask:     native_valid_return_value | native_valid_errno | native_valid_observed_count | native_valid_observed_flags
				return_value:   -1
				native_errno:   native_errno
				observed_count: 1
				observed_flags: 0
			}, 0)!
			outcome := app.backend.wayland.dispatch_pending_nonblocking()
			expected := if native_errno in [i64(11), i64(4)] {
				NativeRenderDisposition.transient
			} else {
				NativeRenderDisposition.operation_failed
			}
			assert outcome.disposition == expected
			native_assert_wayland_compound_capture_for_test(&app.backend.native_operations,
				poll_context, expected, native_errno, 0)
			native_assert_wayland_prepare_balance_for_test(&app.backend.native_operations)
			cancel_context := native_find_real_operation_context_for_test(&app.backend.native_operations,
				.display_cancel)!
			assert cancel_context.ordinal > poll_context.ordinal
			mut saw_poll_effective := false
			for index in 0 .. app.backend.native_operations.proof.trace_len {
				entry := app.backend.native_operations.proof.trace[index]
				if entry.milestone == .effective_primitive
					&& native_operation_contexts_identical(entry.context, poll_context) {
					saw_poll_effective = true
					assert entry.effective.observed_count == 1
					assert entry.effective.observed_flags == 0
				}
			}
			assert saw_poll_effective
			for index in 0 .. app.backend.native_operations.proof.trace_len {
				entry := app.backend.native_operations.proof.trace[index]
				if entry.milestone == .real_call {
					assert entry.context.operation != .display_read
				}
			}
			native_assert_proof_drained_for_test(&app.backend.native_operations)
		}
		native_wayland_require_real_read_for_test(mut app)!
		native_stop_twice_with_exact_proof_for_test(mut app)!
	}

	fn native_wayland_exercise_fatal_transport_cleanup() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window := native_runtime_new_window_for_test(mut app,
			'native Wayland fatal transport cleanup')!
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			assert app.backend.wayland.windows[index].frame_callback == unsafe { nil }
			assert app.backend.wayland.data_offer == unsafe { nil }
			assert app.backend.wayland.pending_drop_offer == unsafe { nil }
			assert app.backend.wayland.pending_drop_fd < 0
			pending_alias := app.backend.wayland.wm_base
			assert pending_alias != unsafe { nil }
			mut fds := [2]int{}
			assert C.pipe(&fds[0]) == 0
			assert fds[0] >= 0
			assert fds[1] >= 0
			assert C.close(fds[1]) == 0
			app.backend.wayland.data_offer = pending_alias
			app.backend.wayland.pending_drop_offer = pending_alias
			app.backend.wayland.pending_drop_fd = fds[0]
			app.backend.wayland.wm_base = unsafe { nil }
			app.backend.wayland.wm_base_name = 0
		}
		assert app.backend.native_operations.arm_proof()
		native_release_oracle_reset_for_test(.wayland)
		local_release_proof := native_wayland_local_release_proof_for_test(&app.backend.wayland)
		assert local_release_proof.identities.len > 0
		assert local_release_proof.pending_alias != 0
		assert local_release_proof.pending_drop_fd >= 0
		anchor_surface_identity := native_identity(app.backend.wayland.anchor_wl_surface)
		anchor_surface_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			app.backend.wayland.anchor_wl_surface_ticket)!
		window_index := app.backend.wayland.window_record_index(window) or {
			return error(err_window_not_found)
		}
		window_record := app.backend.wayland.windows[window_index]
		assert window_record.egl_surface == unsafe { nil }
		assert window_record.egl_surface_ticket == 0
		assert window_record.wl_egl_window == unsafe { nil }
		assert window_record.wl_egl_window_ticket == 0
		anchor_egl_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			app.backend.wayland.anchor_surface_ticket)!
		anchor_wl_egl_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			app.backend.wayland.anchor_wl_egl_window_ticket)!
		context_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			app.backend.wayland.egl_context_ticket)!
		display_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			app.backend.wayland.egl_display_ticket)!
		thread_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			app.backend.wayland.egl_thread_ticket)!
		assert anchor_surface_identity != 0
		assert anchor_surface_ticket.native_identity == anchor_surface_identity
		assert anchor_surface_ticket.release_kind == .wayland_surface
		flush_seed := NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: local_release_proof.display
		}
		context := native_wayland_primary_context_for_test(app, .display_flush, flush_seed,
			app.backend.native_operations.next_ordinal)
		native_arm_wayland_primitive_for_test(mut app, context, NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value | native_valid_errno
			return_value: -1
			native_errno: 32
		}, 71)!
		outcome := app.backend.wayland.attempt_wayland_flush(NativeOperationSeed{
			call_site: .display_transport
			scope:     .batch
		})
		assert outcome.disposition == .renderer_unavailable
		assert outcome.display_error == 71
		assert outcome.error_text == err_wayland_flush_failed
		native_assert_wayland_compound_capture_for_test(&app.backend.native_operations, context,
			.renderer_unavailable, 32, 71)
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		assert app.backend.wayland.render_health == .unavailable
		assert app.backend.wayland.wayland_display_unavailable
		assert app.backend.wayland.wayland_display_error == 71
		release_trace_cursor := app.backend.native_operations.proof.trace_len
		before_stop_oracle := native_release_oracle_snapshot_for_test(.wayland)
		assert !before_stop_oracle.overflow
		assert before_stop_oracle.records.len == 0
		assert before_stop_oracle.callback_completions.len == 0
		assert before_stop_oracle.unticketed_releases.len == 0
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		assert stopped.registry.tickets.len == 0
		assert app.status() == .stopped
		assert app.backend.wayland.start_attempt_closed()
		assert !app.backend.wayland.retains_native_ownership()
		ownership := native_phase_a_backend_ownership_snapshot(app)
		assert !ownership.started
		assert ownership.native_display == 0
		assert ownership.egl_display == 0
		assert ownership.egl_config == 0
		assert ownership.egl_context == 0
		assert ownership.anchor_surface == 0
		assert ownership.anchor_wl_egl_window == 0
		assert ownership.anchor_wl_surface == 0
		assert ownership.egl_display_ticket == 0
		assert ownership.egl_context_ticket == 0
		assert ownership.egl_thread_ticket == 0
		assert ownership.anchor_surface_ticket == 0
		assert ownership.anchor_wl_egl_window_ticket == 0
		assert ownership.anchor_wl_surface_ticket == 0
		assert ownership.binding_kind == .none
		assert ownership.binding_surface == 0
		assert ownership.pending_drop_fd == -1
		assert !ownership.pending_drop_window_valid
		assert ownership.pending_drop_buffer.len == 0
		assert ownership.poll_error == ''
		assert !ownership.wayland_display_unavailable
		assert ownership.wayland_display_error == 0
		assert !ownership.data_offer_has_uri_list
		assert !ownership.data_offer_window_valid
		assert ownership.windows.len == 0
		assert ownership.registry.tickets.len == 0
		for identity in ownership.backend_owned_identities {
			assert identity == 0
		}
		after_stop_oracle := native_release_oracle_snapshot_for_test(.wayland)
		mut release_cursor := release_trace_cursor
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, anchor_egl_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, anchor_wl_egl_ticket,
			release_cursor)
		native_assert_wayland_anchor_surface_release_mode_at_for_test(stopped,
			anchor_surface_ticket, release_cursor, wayland_anchor_release_local_proxy_destroy)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, anchor_surface_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, context_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, display_ticket,
			release_cursor)
		release_cursor = native_assert_lifetime_release_at_for_test(stopped, thread_ticket,
			release_cursor)
		assert release_cursor == stopped.trace_len
		ordered_tickets := [anchor_egl_ticket, anchor_wl_egl_ticket, anchor_surface_ticket,
			context_ticket, display_ticket, thread_ticket]
		mut oracle_cursor := before_stop_oracle.records.len
		for ticket in ordered_tickets {
			assert oracle_cursor < after_stop_oracle.records.len
			record := after_stop_oracle.records[oracle_cursor]
			assert native_release_oracle_record_matches_ticket_for_test(record, ticket)
			if ticket.release_kind == .wayland_surface {
				assert record.mode == wayland_anchor_release_local_proxy_destroy
			}
			oracle_cursor++
		}
		assert oracle_cursor == after_stop_oracle.records.len
		native_assert_wayland_local_release_oracle_for_test(after_stop_oracle, local_release_proof)
		assert native_release_oracle_record_count_for_test(after_stop_oracle.unticketed_releases,
			.wayland, 4, local_release_proof.pending_alias) == 1
		assert C.close(local_release_proof.pending_drop_fd) == -1
	}

	fn native_linux_egl_failed_start_context_for_test(backend &Backend, native_display u64) NativeOperationContext {
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        backend.native_operations.renderer_attempt_token
			renderer_attempt_token: backend.native_operations.renderer_attempt_token
			app_identity:           backend.native_operations.app_identity
			presence_mask:          native_context_has_target_identity
			domain:                 .egl
			operation:              .display_acquire
			call_site:              .renderer_start
			scope:                  .renderer
			target_identity:        native_display
			ordinal:                backend.native_operations.next_ordinal
		}
	}

	fn native_arm_egl_display_acquire_failure_for_test(mut authority NativeOperationAuthority, context NativeOperationContext) ! {
		if !authority.arm(context, NativePrimitiveEvidence{
			valid_mask: native_valid_handle
			handle:     0
		}) {
			return error('could not arm EGL display-acquire context `${context}`')
		}
		query := native_egl_error_context_for_test(context)
		if !authority.arm(query, NativePrimitiveEvidence{
			valid_mask: native_valid_egl_error
			egl_error:  0x3000
		}) {
			return error('could not arm EGL display-acquire evidence `${query}`')
		}
	}

	fn native_assert_egl_display_acquire_rejected_after_real_call_for_test(authority &NativeOperationAuthority, context NativeOperationContext) {
		assert authority.proof != unsafe { nil }
		mut real_call_count := 0
		mut actual_count := 0
		mut effective_count := 0
		mut acceptance_count := 0
		mut health_count := 0
		for index in 0 .. authority.proof.trace_len {
			entry := authority.proof.trace[index]
			if !native_operation_contexts_identical(entry.context, context) {
				continue
			}
			match entry.milestone {
				.real_call {
					real_call_count++
				}
				.actual_primitive {
					actual_count++
					assert entry.actual.handle != 0
				}
				.effective_primitive {
					effective_count++
					assert entry.effective.handle == 0
				}
				.acceptance {
					acceptance_count++
					assert entry.result.disposition == .renderer_unavailable
					assert entry.result.local_validation == .null_output
				}
				.health_latched {
					health_count++
					assert entry.health == .unavailable
				}
				else {}
			}
		}
		assert real_call_count == 1
		assert actual_count == 1
		assert effective_count == 1
		assert acceptance_count == 1
		assert health_count == 1
	}

	fn native_assert_linux_failed_start_all_clean_for_test(backend &Backend) {
		assert backend.teardown_notices.len == 0
		assert backend.pending_delivery.len == 0
		assert !backend.pending_delivery_active
		assert !backend.native_operations.has_live_lifetime_tickets()
		assert backend.native_operations.lifetime_tickets.len == 0
		assert !backend.native_operations.has_pending_native_plans()
		assert backend.start_attempt_closed()
		match backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					assert !backend.x11.started
					assert backend.x11.display == unsafe { nil }
					assert backend.x11.xim == unsafe { nil }
					assert backend.x11.windows.len == 0
					assert backend.x11.egl_display == unsafe { nil }
					assert backend.x11.egl_config == unsafe { nil }
					assert backend.x11.egl_context == unsafe { nil }
					assert backend.x11.anchor_surface == unsafe { nil }
					assert backend.x11.egl_display_ticket == 0
					assert backend.x11.egl_context_ticket == 0
					assert backend.x11.egl_thread_ticket == 0
					assert backend.x11.anchor_surface_ticket == 0
					assert backend.x11.egl_binding.kind == .none
					assert backend.x11.egl_binding.surface == unsafe { nil }
					assert !backend.x11.retains_native_ownership()
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					assert !backend.wayland.started
					for identity in [native_identity(backend.wayland.display),
						native_identity(backend.wayland.registry),
						native_identity(backend.wayland.compositor),
						native_identity(backend.wayland.seat),
						native_identity(backend.wayland.pointer),
						native_identity(backend.wayland.keyboard),
						native_identity(backend.wayland.touch),
						native_identity(backend.wayland.cursor_shape_manager),
						native_identity(backend.wayland.cursor_shape_device),
						native_identity(backend.wayland.data_device_manager),
						native_identity(backend.wayland.data_device),
						native_identity(backend.wayland.data_offer),
						native_identity(backend.wayland.pending_drop_offer),
						native_identity(backend.wayland.shm),
						native_identity(backend.wayland.wm_base),
						native_identity(backend.wayland.decoration_manager),
						native_identity(backend.wayland.xkb_context),
						native_identity(backend.wayland.xkb_keymap),
						native_identity(backend.wayland.xkb_state),
						native_identity(backend.wayland.egl_display),
						native_identity(backend.wayland.egl_config),
						native_identity(backend.wayland.egl_context),
						native_identity(backend.wayland.anchor_surface),
						native_identity(backend.wayland.anchor_wl_egl_window),
						native_identity(backend.wayland.anchor_wl_surface)] {
						assert identity == 0
					}
					assert backend.wayland.windows.len == 0
					assert backend.wayland.pending_drop_fd == -1
					assert backend.wayland.pending_drop_buffer.len == 0
					assert !backend.wayland.pending_drop_window_valid
					assert backend.wayland.poll_error == ''
					assert !backend.wayland.wayland_display_unavailable
					assert backend.wayland.wayland_display_error == 0
					assert !backend.wayland.data_offer_has_uri_list
					assert !backend.wayland.data_offer_window_valid
					assert backend.wayland.compositor_name == 0
					assert backend.wayland.seat_name == 0
					assert backend.wayland.cursor_shape_manager_name == 0
					assert backend.wayland.data_device_manager_name == 0
					assert backend.wayland.shm_name == 0
					assert backend.wayland.wm_base_name == 0
					assert backend.wayland.decoration_manager_name == 0
					assert backend.wayland.egl_display_ticket == 0
					assert backend.wayland.egl_context_ticket == 0
					assert backend.wayland.egl_thread_ticket == 0
					assert backend.wayland.anchor_surface_ticket == 0
					assert backend.wayland.anchor_wl_egl_window_ticket == 0
					assert backend.wayland.anchor_wl_surface_ticket == 0
					assert backend.wayland.egl_binding.kind == .none
					assert backend.wayland.egl_binding.surface == unsafe { nil }
					assert !backend.wayland.retains_native_ownership()
				}
			}
			else {
				assert false
			}
		}
	}

	fn native_linux_egl_exercise_failed_start_cleanup() ! {
		selected := native_runtime_backend_for_test()!
		assert selected in [.x11, .wayland]
		native_require_parent_watchdog_gate_for_test()!
		mut backend := new_backend(selected, true)!
		app_identity := if selected == .x11 { u64(0x7311) } else { u64(0x7321) }
		backend.bind_app_native_operations(app_identity, app_identity + 1, app_identity + 2)!
		backend.start(false)!
		mut native_display := u64(0)
		mut xim_identity := u64(0)
		mut wayland_start_proxies := NativeWaylandLocalReleaseProof{}
		match selected {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					native_display = u64(usize(backend.x11.display))
					xim_identity = native_identity(backend.x11.xim)
					assert native_display != 0
					assert xim_identity != 0
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					native_display = native_identity(backend.wayland.display)
					wayland_start_proxies =
						native_wayland_local_release_proof_for_test(&backend.wayland)
					assert native_display != 0
					assert wayland_start_proxies.identities.len > 0
					assert backend.wayland.pending_drop_fd == -1
				}
			}
			else {
				return error(err_backend_unsupported)
			}
		}

		assert backend.native_operations.arm_proof()
		native_release_oracle_reset_for_test(selected)
		context := native_linux_egl_failed_start_context_for_test(&backend, native_display)
		native_arm_egl_display_acquire_failure_for_test(mut backend.native_operations, context)!
		mut failure := ''
		mut first_close_error := ''
		match selected {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					backend.x11.init_renderer() or {
						failure = err.msg()
						first_close_error = backend.close_start_attempt()
					}
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					backend.wayland.init_renderer() or {
						failure = err.msg()
						first_close_error = backend.close_start_attempt()
					}
				}
			}
			else {}
		}

		expected_error := if selected == .x11 {
			err_x11_egl_display_failed
		} else {
			err_wayland_egl_display_failed
		}
		assert failure == expected_error
		assert failure.count(expected_error) == 1
		native_assert_egl_display_acquire_rejected_after_real_call_for_test(&backend.native_operations,
			context)
		native_assert_proof_drained_for_test(&backend.native_operations)
		assert first_close_error == ''
		native_assert_linux_failed_start_all_clean_for_test(&backend)
		first_proof := native_proof_snapshot(&backend.native_operations)
		first_oracle := native_release_oracle_snapshot_for_test(selected)
		assert !first_oracle.overflow
		assert !first_proof.trace_overflow
		native_assert_complete_native_trace_for_test(first_proof)
		assert first_oracle.records.len == 1
		assert native_release_oracle_record_count_for_test(first_oracle.records, .egl, 4,
			backend.native_operations.owner_thread_identity) == 1
		if selected == .x11 {
			assert first_oracle.unticketed_releases.len == 2
			assert native_release_oracle_record_count_for_test(first_oracle.unticketed_releases,
				.x11, 5, xim_identity) == 1
			assert native_release_oracle_record_count_for_test(first_oracle.unticketed_releases,
				.x11, 6, native_display) == 1
			xim_sequence := native_release_oracle_record_sequence_for_test(first_oracle.unticketed_releases,
				.x11, 5, xim_identity)
			display_sequence := native_release_oracle_record_sequence_for_test(first_oracle.unticketed_releases,
				.x11, 6, native_display)
			assert xim_sequence < display_sequence
			assert display_sequence == native_release_oracle_last_sequence_for_test(first_oracle)
		} else {
			assert wayland_start_proxies.display == native_display
			assert first_oracle.unticketed_releases.len == 1
			for identity in wayland_start_proxies.identities {
				assert native_release_oracle_record_count_for_test(first_oracle.unticketed_releases,
					.wayland, 4, identity) == 0
			}
			assert native_release_oracle_record_count_for_test(first_oracle.unticketed_releases,
				.wayland, 5, native_display) == 1
			disconnect_sequence := native_release_oracle_record_sequence_for_test(first_oracle.unticketed_releases,
				.wayland, 5, native_display)
			assert disconnect_sequence == native_release_oracle_last_sequence_for_test(first_oracle)
		}
		second_close_error := backend.close_start_attempt()
		assert second_close_error == ''
		backend.stop()!
		native_assert_linux_failed_start_all_clean_for_test(&backend)
		second_proof := native_proof_snapshot(&backend.native_operations)
		second_oracle := native_release_oracle_snapshot_for_test(selected)
		native_proof_assert_snapshots_equal(first_proof, second_proof)
		native_release_oracle_assert_equal_for_test(first_oracle, second_oracle)
		assert backend.native_operations.disarm_proof()
	}

	fn native_wayland_exercise_bad_poll_revents() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		app.backend.native_operations.arm_proof()
		sokol_generation := native_install_sokol_trace_for_test()!
		poll_template := native_wayland_quiescent_poll_template_for_test(mut app)!
		native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
		poll_context := NativeOperationContext{
			...poll_template
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			ordinal:                app.backend.native_operations.next_ordinal +
				native_wayland_poll_ordinal_offset_for_test
		}
		native_arm_wayland_primitive_for_test(mut app, poll_context, NativePrimitiveEvidence{
			valid_mask:     native_valid_return_value | native_valid_errno | native_valid_observed_count | native_valid_observed_flags
			return_value:   1
			native_errno:   0
			observed_count: 1
			observed_flags: 0x8
		}, 0)!
		outcome := app.backend.wayland.dispatch_pending_nonblocking()
		assert outcome.disposition == .renderer_unavailable
		assert outcome.local_validation == .bad_revents
		native_assert_wayland_compound_capture_for_test(&app.backend.native_operations,
			poll_context, .renderer_unavailable, 0, 0)
		acceptance_index := native_acceptance_index_for_test(&app.backend.native_operations,
			poll_context)
		native_assert_wayland_prepare_balance_for_test(&app.backend.native_operations)
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_cancel) == 1
		assert native_real_call_count_for_test(&app.backend.native_operations, .display_read) == 0
		assert app.backend.renderer_health() == .unavailable
		trace_after_loss := app.backend.native_operations.proof.trace_len
		flush := app.backend.wayland.attempt_wayland_flush(NativeOperationSeed{
			call_site: .display_transport
			scope:     .renderer
		})
		assert flush.blocks_graphics()
		roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
			call_site: .display_transport
			scope:     .renderer
		})
		assert roundtrip.blocks_graphics()
		assert app.backend.native_operations.proof.trace_len == trace_after_loss
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		failure_segment := native_proof_snapshot(&app.backend.native_operations)
		assert !failure_segment.trace_overflow
		native_assert_complete_native_trace_for_test(failure_segment)
		assert acceptance_index >= 0
		assert acceptance_index < failure_segment.trace_len
		assert failure_segment.trace[acceptance_index].milestone == .acceptance
		assert native_operation_contexts_identical(failure_segment.trace[acceptance_index].context,
			poll_context)
		cleanup_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		failure_oracle := native_release_oracle_snapshot_for_test(.wayland)
		assert !failure_oracle.overflow
		consumed_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(failure_segment.plan, consumed_plan)
		stop_baseline := native_proof_snapshot(&app.backend.native_operations)
		assert stop_baseline.trace_len == 0
		assert !stop_baseline.trace_overflow
		assert stop_baseline.app_identity == failure_segment.app_identity
		assert stop_baseline.app_lifetime_token == failure_segment.app_lifetime_token
		assert stop_baseline.renderer_attempt_token == failure_segment.renderer_attempt_token
		assert stop_baseline.owner_thread_identity == failure_segment.owner_thread_identity
		assert stop_baseline.next_ordinal == failure_segment.next_ordinal
		assert stop_baseline.next_proof_generation == failure_segment.next_proof_generation
		assert stop_baseline.sequence_exhausted == failure_segment.sequence_exhausted
		assert stop_baseline.terminal_cause == failure_segment.terminal_cause
		assert stop_baseline.proof_armed == failure_segment.proof_armed
		assert stop_baseline.proof_generation == failure_segment.proof_generation
		assert stop_baseline.proof_ordinal_floor == failure_segment.proof_ordinal_floor
		assert stop_baseline.proof_accepting_plans == failure_segment.proof_accepting_plans
		native_proof_assert_plan_equal(consumed_plan, stop_baseline.plan)
		native_lifetime_registry_assert_snapshots_equal(failure_segment.registry,
			stop_baseline.registry)
		stop_baseline_oracle := native_release_oracle_snapshot_for_test(.wayland)
		native_release_oracle_assert_equal_for_test(failure_oracle, stop_baseline_oracle)
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
		native_uninstall_sokol_trace_for_test(sokol_generation)!
		native_assert_phase_a_ticket_retirement_for_test(stopped, cleanup_tickets, 0)
		native_assert_only_phase_a_ticket_releases_for_test(stopped, cleanup_tickets, 0)
		native_assert_phase_a_ticket_release_health_for_test(stopped, cleanup_tickets, .unavailable)
	}

	fn native_assert_wayland_listener_callback_released_once_for_test(snapshot NativeAuthorityProofSnapshot, listener_context NativeOperationContext, callback_identity u64) {
		assert callback_identity != 0
		assert listener_context.domain == .wayland
		assert listener_context.operation == .frame_callback
		assert listener_context.call_site == .window_finalize
		assert listener_context.scope == .window_target
		assert listener_context.target_identity == callback_identity
		assert listener_context.ordinal > 0
		assert listener_context.ordinal <= ~u64(0) - native_wayland_frame_callback_cleanup_ordinal_offset_from_listener_for_test
		cleanup_context := NativeOperationContext{
			...listener_context
			operation: .surface_destroy
			ordinal:   listener_context.ordinal +
				native_wayland_frame_callback_cleanup_ordinal_offset_from_listener_for_test
		}
		mut real_calls := 0
		mut releases := 0
		mut matching_entries := 0
		mut start := -1
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.context.domain != .wayland || entry.context.operation != .surface_destroy
				|| entry.context.target_identity != callback_identity {
				continue
			}
			matching_entries++
			assert native_operation_contexts_identical(entry.context, cleanup_context)
			if entry.milestone == .real_call {
				real_calls++
				start = index
			}
			if entry.milestone == .authority_release {
				releases++
			}
		}
		assert real_calls == 1
		assert releases == 1
		assert matching_entries == 6
		assert start >= 0
		assert start + 5 < snapshot.trace_len
		expected := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
			.authority_release,
		]
		for offset, milestone in expected {
			entry := snapshot.trace[start + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, cleanup_context)
		}
		actual := snapshot.trace[start + 1].actual
		effective := snapshot.trace[start + 2].effective
		assert native_proof_evidence_equal(actual, effective)
		accepted := snapshot.trace[start + 3]
		assert accepted.local_validation == .void_completion
		assert accepted.result.succeeded()
		assert snapshot.trace[start + 4].health == .ready
		assert native_proof_result_equal(snapshot.trace[start + 5].result, accepted.result)
	}

	fn native_wayland_exercise_normal_frame_callback_retirement() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window := native_runtime_new_window_for_test(mut app,
			'native Wayland normal callback retirement')!
		assert app.backend.native_operations.arm_proof()
		outcome := app.with_scheduled_render_batch(fn [mut app, window] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		assert outcome.error == ''
		assert outcome.committed
		assert outcome.finalized_submissions == 1
		mut callback_identity := u64(0)
		mut callback_ticket_id := u64(0)
		mut parent_identity := u64(0)
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			record := app.backend.wayland.windows[index]
			callback_identity = native_identity(record.frame_callback)
			callback_ticket_id = record.frame_callback_ticket
			parent_identity = native_identity(record.surface)
			assert callback_identity != 0
			assert callback_ticket_id != 0
		}
		callback_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
			callback_ticket_id)!
		assert callback_ticket.proof_generation == app.backend.native_operations.proof.generation
		native_assert_bound_ticket_snapshot_for_test(callback_ticket, .wayland_frame_callback,
			callback_identity, parent_identity)
		assert callback_ticket.owner_seed.window == window
		assert callback_ticket.context.call_site == .window_finalize

		$if linux && sokol_wayland ? {
			assert app.backend.native_operations.owner_thread_is_current()
			assert app.backend.wayland.display != unsafe { nil }
			display := unsafe { &C.wl_display(app.backend.wayland.display) }
			for {
				dispatch_result := C.wl_display_dispatch(display)
				assert dispatch_result >= 0
				index := app.backend.wayland.window_record_index(window) or {
					return error(err_window_not_found)
				}
				record := app.backend.wayland.windows[index]
				if record.frame_callback == unsafe { nil } {
					assert record.frame_callback_ticket == 0
					assert record.frame_ready
					break
				}
				assert native_identity(record.frame_callback) == callback_identity
				assert record.frame_callback_ticket == callback_ticket_id
				assert native_identity(record.surface) == parent_identity
			}
		} $else {
			return error(err_backend_unsupported)
		}
		$if linux && sokol_wayland ? {
			settled_index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			settled_record := app.backend.wayland.windows[settled_index]
			assert settled_record.frame_callback == unsafe { nil }
			assert settled_record.frame_callback_ticket == 0
			assert settled_record.frame_ready
		}
		assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
			callback_ticket_id) == -1
		callback_segment := native_proof_snapshot(&app.backend.native_operations)
		assert !callback_segment.trace_overflow
		native_assert_complete_native_trace_for_test(callback_segment)
		_ = native_assert_lifetime_release_sequence_in_snapshot_for_test(callback_segment,
			callback_ticket.context, 0)
		assert native_lifetime_trace_entry_count_for_test(callback_segment, callback_ticket.context) == 6
		callback_oracle := native_release_oracle_snapshot_for_test(.wayland)
		assert !callback_oracle.overflow
		assert callback_oracle.records.len == 1
		assert callback_oracle.records[0].domain == .wayland
		assert callback_oracle.records[0].kind == 2
		assert callback_oracle.records[0].identity == callback_ticket.native_identity
		assert callback_oracle.callback_completions.len == 1
		assert callback_oracle.callback_completions[0].domain == .wayland
		assert callback_oracle.callback_completions[0].kind == 3
		assert callback_oracle.callback_completions[0].identity == callback_ticket.native_identity
		assert callback_oracle.records[0].sequence < callback_oracle.callback_completions[0].sequence
		for entry in callback_segment.plan {
			assert !entry.armed
			assert !entry.listener_registration_pending
		}
		callback_plan :=
			native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
		native_proof_assert_plan_equal(callback_segment.plan, callback_plan)
		callback_cleared := native_proof_snapshot(&app.backend.native_operations)
		assert callback_cleared.trace_len == 0
		assert !callback_cleared.trace_overflow
		assert callback_cleared.app_identity == callback_segment.app_identity
		assert callback_cleared.app_lifetime_token == callback_segment.app_lifetime_token
		assert callback_cleared.renderer_attempt_token == callback_segment.renderer_attempt_token
		assert callback_cleared.owner_thread_identity == callback_segment.owner_thread_identity
		assert callback_cleared.next_ordinal == callback_segment.next_ordinal
		assert callback_cleared.next_proof_generation == callback_segment.next_proof_generation
		assert callback_cleared.sequence_exhausted == callback_segment.sequence_exhausted
		assert callback_cleared.terminal_cause == callback_segment.terminal_cause
		assert callback_cleared.proof_armed == callback_segment.proof_armed
		assert callback_cleared.proof_generation == callback_segment.proof_generation
		assert callback_cleared.proof_ordinal_floor == callback_segment.proof_ordinal_floor
		assert callback_cleared.proof_accepting_plans == callback_segment.proof_accepting_plans
		native_proof_assert_plan_equal(callback_plan, callback_cleared.plan)
		for entry in callback_cleared.plan {
			assert !entry.armed
			assert !entry.listener_registration_pending
		}
		native_lifetime_registry_assert_snapshots_equal(callback_segment.registry,
			callback_cleared.registry)
		callback_cleared_oracle := native_release_oracle_snapshot_for_test(.wayland)
		native_release_oracle_assert_equal_for_test(callback_oracle, callback_cleared_oracle)
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		assert native_lifetime_trace_entry_count_for_test(stopped, callback_ticket.context) == 0
		stopped_oracle := native_release_oracle_snapshot_for_test(.wayland)
		assert !stopped_oracle.overflow
		assert native_release_oracle_record_count_for_test(stopped_oracle.records, .wayland, 2,
			callback_ticket.native_identity) == 1
		assert native_release_oracle_record_count_for_test(stopped_oracle.callback_completions,
			.wayland, 3, callback_ticket.native_identity) == 1
		assert native_release_oracle_record_sequence_for_test(stopped_oracle.records, .wayland, 2,
			callback_ticket.native_identity) == callback_oracle.records[0].sequence
		assert native_release_oracle_record_sequence_for_test(stopped_oracle.callback_completions,
			.wayland, 3, callback_ticket.native_identity) == callback_oracle.callback_completions[0].sequence
	}

	fn native_wayland_exercise_frame_callback_trampoline_post_destroy_completion() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window := native_runtime_new_window_for_test(mut app,
			'native Wayland trampoline callback retirement')!
		assert app.backend.native_operations.arm_proof()
		outcome := app.with_scheduled_render_batch(fn [mut app, window] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		assert outcome.error == ''
		assert outcome.committed
		assert outcome.finalized_submissions == 1
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			mut record := &app.backend.wayland.windows[index]
			callback := record.frame_callback
			ticket_id := record.frame_callback_ticket
			parent_identity := native_identity(record.surface)
			assert callback != unsafe { nil }
			assert ticket_id != 0
			assert parent_identity != 0
			ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				ticket_id)!
			assert ticket.proof_generation == app.backend.native_operations.proof.generation
			native_assert_bound_ticket_snapshot_for_test(ticket, .wayland_frame_callback,
				native_identity(callback), parent_identity)
			assert ticket.authority_scope == .renderer_attempt
			assert ticket.authority_token == app.backend.native_operations.renderer_attempt_token
			assert ticket.parent_authority_scope == .none
			assert ticket.parent_authority_token == 0
			listener_data := record.listener_data()
			native_release_oracle_reset_for_test(.wayland)
			C.v_multiwindow_test_wayland_invoke_frame_done_trampoline(listener_data, callback, 0)
			assert record.frame_callback == unsafe { nil }
			assert record.frame_callback_ticket == 0
			assert record.frame_ready
			assert native_lifetime_ticket_index_for_test(&app.backend.native_operations, ticket_id) == -1
			native_assert_lifetime_release_sequence_for_test(&app.backend.native_operations,
				ticket.context, 0)
			trampoline_oracle := native_release_oracle_snapshot_for_test(.wayland)
			assert !trampoline_oracle.overflow
			assert trampoline_oracle.records.len == 1
			assert trampoline_oracle.callback_completions.len == 1
			assert trampoline_oracle.records[0] == NativeReleaseOracleRecord{
				sequence: 1
				domain:   .wayland
				kind:     2
				identity: ticket.native_identity
			}
			assert trampoline_oracle.callback_completions[0] == NativeReleaseOracleRecord{
				sequence: 2
				domain:   .wayland
				kind:     3
				identity: ticket.native_identity
			}
			before_stop := native_proof_snapshot(&app.backend.native_operations)
			assert native_lifetime_trace_entry_count_for_test(before_stop, ticket.context) == 6
			_ = native_segment_consumed_trace_before_stop_for_test(mut app, before_stop, .wayland,
				trampoline_oracle)
			_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
			assert native_lifetime_trace_entry_count_for_test(stopped, ticket.context) == 0
			final_oracle := native_release_oracle_snapshot_for_test(.wayland)
			assert !final_oracle.overflow
			mut callback_destroy_count := 0
			mut callback_completion_count := 0
			for record_entry in final_oracle.records {
				if record_entry.domain == .wayland
					&& record_entry.identity == ticket.native_identity {
					if record_entry.kind == 2 {
						callback_destroy_count++
					}
				}
			}
			for completion in final_oracle.callback_completions {
				if completion.domain == .wayland && completion.kind == 3
					&& completion.identity == ticket.native_identity {
					callback_completion_count++
				}
			}
			assert callback_destroy_count == 1
			assert callback_completion_count == 1
			assert native_release_oracle_record_sequence_for_test(final_oracle.records, .wayland,
				2, ticket.native_identity) == trampoline_oracle.records[0].sequence
			assert native_release_oracle_record_sequence_for_test(final_oracle.callback_completions,
				.wayland, 3, ticket.native_identity) == trampoline_oracle.callback_completions[0].sequence
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_exercise_pending_callback_child_first_stop() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window := native_runtime_new_window_for_test(mut app,
			'native Wayland callback child-first stop')!
		assert app.backend.native_operations.arm_proof()
		_ = native_egl_render_healthy_target_for_test(mut app, window)!
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			record := app.backend.wayland.windows[index]
			assert record.frame_callback != unsafe { nil }
			assert record.frame_callback_ticket != 0
			assert record.egl_surface != unsafe { nil }
			assert record.egl_surface_ticket != 0
			assert record.wl_egl_window != unsafe { nil }
			assert record.wl_egl_window_ticket != 0
		} $else {
			return error(err_backend_unsupported)
		}
		tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		mut callback_count := 0
		for ticket in tickets {
			if ticket.release_kind == .wayland_frame_callback {
				callback_count++
				assert ticket.authority_scope == .renderer_attempt
				assert ticket.parent_authority_scope == .none
			}
		}
		assert callback_count == 1
		before_stop := native_proof_snapshot(&app.backend.native_operations)
		before_stop_oracle := native_release_oracle_snapshot_for_test(.wayland)
		_ = native_segment_consumed_trace_before_stop_for_test(mut app, before_stop, .wayland,
			before_stop_oracle)
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		native_assert_phase_a_ticket_retirement_for_test(stopped, tickets, 0)
	}

	fn native_wayland_retire_pending_frame_callback_for_test(mut app App, window WindowId) ! {
		if app.backend.kind != .wayland {
			return
		}
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			mut record := app.backend.wayland.windows[index]
			if record.frame_callback == unsafe { nil } || record.frame_callback_ticket == 0 {
				return error('Wayland frame callback retirement requires a pending callback and ticket')
			}
			callback_identity := native_identity(record.frame_callback)
			parent_identity := native_identity(record.surface)
			target_generation := record.render_target_generation
			assert callback_identity != 0
			assert parent_identity != 0
			assert target_generation != 0
			ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				record.frame_callback_ticket)!
			native_assert_bound_ticket_snapshot_for_test(ticket, .wayland_frame_callback,
				callback_identity, parent_identity)
			assert ticket.owner_seed.target_generation == target_generation
			before_release := native_proof_snapshot(&app.backend.native_operations)
			trace_start := before_release.trace_len
			next_ordinal := before_release.next_ordinal
			released := app.backend.wayland.destroy_frame_callback_lifetime(mut record)
			assert released.succeeded()
			assert record.frame_callback == unsafe { nil }
			assert record.frame_callback_ticket == 0
			record.frame_ready = true
			assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
				ticket.ticket_id) == -1
			after_release := native_proof_snapshot(&app.backend.native_operations)
			assert !after_release.trace_overflow
			release_end := native_assert_lifetime_release_at_for_test(after_release, ticket,
				trace_start)
			assert release_end == trace_start + 6
			assert after_release.trace_len == release_end
			assert after_release.next_ordinal == next_ordinal
			native_assert_complete_native_trace_for_test(after_release)
			display_identity := native_wayland_display_identity_for_test(app)
			assert display_identity != 0
			roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
				call_site: .display_transport
				scope:     .renderer
			})
			assert roundtrip.succeeded()
			expected_roundtrip_context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
				app_identity:           app.instance_id
				presence_mask:          native_context_has_target_identity
				domain:                 .wayland
				operation:              .display_roundtrip
				call_site:              .display_transport
				scope:                  .renderer
				target_identity:        display_identity
				ordinal:                next_ordinal
			}
			assert native_operation_contexts_identical(roundtrip.context,
				expected_roundtrip_context)
			native_assert_wayland_compound_capture_for_test(&app.backend.native_operations,
				roundtrip.context, .ok, 0, 0)
			after_roundtrip := native_proof_snapshot(&app.backend.native_operations)
			assert !after_roundtrip.trace_overflow
			assert after_roundtrip.trace_len == release_end + 8
			assert after_roundtrip.next_ordinal == next_ordinal + 2
			native_assert_complete_native_trace_for_test(after_roundtrip)
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_exercise_listener_failure() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window := native_runtime_new_window_for_test(mut app, 'native Wayland listener identity')!
		initial := app.render_window_snapshot(window)!
		app.backend.native_operations.arm_proof()
		sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
			}
		}
		mut state := &NativeWaylandListenerFailureState{}
		listener_failure := NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value | native_valid_errno
			return_value: -1
			native_errno: 11
		}
		mut phase_error := ''
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut state, listener_failure] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			if !candidates.any(it.window == window) {
				return error('Wayland listener failure batch did not include the target window candidate')
			}
			acquisition := app.acquire_render_target(batch, window)!
			if acquisition.status != .ready {
				return error('Wayland listener failure target acquisition was `${acquisition.status}`, expected ready')
			}
			state.lease = acquisition.lease
			state.pass_snapshot = acquisition.snapshot
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut app, window, mut state, listener_failure] () ! {
				$if linux && sokol_wayland ? {
					index := app.backend.wayland.window_record_index(window) or {
						return error(err_window_not_found)
					}
					record := app.backend.wayland.windows[index]
					state.callback_parent_identity = native_identity(record.surface)
					if state.callback_parent_identity == 0 {
						return error('Wayland listener failure frame callback parent surface has no identity')
					}
					state.selector = NativeOperationContext{
						authority_scope:        .renderer_attempt
						authority_token:        app.backend.native_operations.renderer_attempt_token
						renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
						app_identity:           app.instance_id
						presence_mask:          native_context_window_target_fields
						domain:                 .wayland
						operation:              .frame_callback
						call_site:              .window_finalize
						scope:                  .window_target
						window:                 window
						target_generation:      record.render_target_generation
						batch_epoch:            state.lease.batch_epoch
						window_lease_epoch:     state.lease.window_epoch
						target_lease_epoch:     state.lease.target_epoch
						ordinal:                app.backend.native_operations.next_ordinal +
							native_verified_egl_binding_ordinal_span_for_test +
							native_wayland_listener_registration_ordinal_offset_for_test
					}
					if !app.backend.native_operations.queue_listener_registration_injection(state.selector,
						listener_failure) {
						return error('Wayland listener failure could not queue the exact listener registration injection')
					}
					mut pending := 0
					for entry in app.backend.native_operations.proof.plan {
						if entry.armed {
							return error('Wayland listener failure plan contained an armed entry before listener registration')
						}
						if entry.listener_registration_pending {
							pending++
							if entry.proof_generation != app.backend.native_operations.proof.generation {
								return error('Wayland listener failure pending registration used an unexpected proof generation')
							}
							if !native_operation_contexts_identical(entry.context, state.selector) {
								return error('Wayland listener failure pending registration context did not match its selector')
							}
							if !native_proof_evidence_equal(entry.evidence, listener_failure) {
								return error('Wayland listener failure pending registration evidence did not match the injected failure')
							}
						}
					}
					if pending != 1 {
						return error('Wayland listener failure expected exactly one pending registration, found `${pending}`')
					}
				}
			})!
		}) or {
			phase_error = err.msg()
			RenderBatchOutcome{}
		}
		sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(sokol_generation)!
		assert phase_error == ''
		lease := state.lease
		pass_snapshot := state.pass_snapshot
		selector := state.selector
		callback_parent_identity := state.callback_parent_identity
		assert outcome.error == '${err_render_terminal_aggregate}: ${err_wayland_egl_swap_buffers_failed}'
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 0
		assert app.backend.renderer_health() == .ready
		assert app.render_runtime.renderer_terminal == outcome.error
		assert !app.renderer_is_usable()
		assert app.renderer_device_available_for_gg()
		native_assert_sokol_sequence_for_test(sokol, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		native_assert_authoritative_sokol_swapchain_for_test(app, sokol, lease, pass_snapshot)!
		mut created_callback := u64(0)
		mut created_callback_count := 0
		mut listener_context := NativeOperationContext{}
		for index in 0 .. app.backend.native_operations.proof.trace_len {
			entry := app.backend.native_operations.proof.trace[index]
			if entry.context.domain != .wayland || entry.context.operation != .frame_callback {
				continue
			}
			if entry.milestone == .actual_primitive && entry.actual.handle != 0 {
				created_callback_count++
				created_callback = entry.actual.handle
			}
			if entry.milestone == .acceptance && entry.result.native_code == 11 {
				listener_context = entry.context
				assert entry.result.disposition == .operation_failed
			}
		}
		assert created_callback != 0
		assert created_callback_count == 1
		assert listener_context.ordinal != 0
		assert listener_registration_selector_matches(selector, listener_context)
		assert listener_context.target_identity == created_callback
		assert (listener_context.presence_mask & native_context_has_target_identity) != 0
		assert listener_context.batch_epoch == lease.batch_epoch
		assert listener_context.window_lease_epoch == lease.window_epoch
		assert listener_context.target_lease_epoch == lease.target_epoch
		mut consumed_listener_plan := 0
		for entry in app.backend.native_operations.proof.plan {
			assert !entry.armed
			assert !entry.listener_registration_pending
			if listener_registration_selector_matches(selector, entry.context) {
				consumed_listener_plan++
				assert entry.proof_generation == app.backend.native_operations.proof.generation
				assert native_operation_contexts_identical(entry.context, listener_context)
				assert native_proof_evidence_equal(entry.evidence, listener_failure)
			}
		}
		assert consumed_listener_plan == 1
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			assert native_identity(app.backend.wayland.windows[index].surface) == callback_parent_identity
		}
		cleanup_ticket_id := listener_context.ordinal +
			native_wayland_frame_callback_cleanup_ordinal_offset_from_listener_for_test
		assert cleanup_ticket_id != 0
		assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
			cleanup_ticket_id) == -1
		after := app.render_window_snapshot(window)!
		assert after.frame_serial == initial.frame_serial + 1
		assert after.submitted_frame == initial.submitted_frame
		assert after.dirty_epoch > after.consumed_epoch
		native_assert_proof_drained_for_test(&app.backend.native_operations)
		before_stop := native_proof_snapshot(&app.backend.native_operations)
		native_assert_wayland_listener_callback_released_once_for_test(before_stop,
			listener_context, created_callback)
		before_stop_oracle := native_release_oracle_snapshot_for_test(.wayland)
		mut callback_destroy_count := 0
		mut callback_completion_count := 0
		for record in before_stop_oracle.records {
			if record.domain == .wayland && record.identity == created_callback {
				if record.kind == 2 {
					callback_destroy_count++
				}
			}
		}
		for completion in before_stop_oracle.callback_completions {
			if completion.domain == .wayland && completion.kind == 3
				&& completion.identity == created_callback {
				callback_completion_count++
			}
		}
		assert callback_destroy_count == 1
		assert callback_completion_count == 0
		cleanup_context := NativeOperationContext{
			...listener_context
			operation: .surface_destroy
			ordinal:   cleanup_ticket_id
		}
		assert native_lifetime_trace_entry_count_for_test(before_stop, cleanup_context) == 6
		callback_destroy_sequence := native_release_oracle_record_sequence_for_test(before_stop_oracle.records,
			.wayland, 2, created_callback)
		_ = native_segment_consumed_trace_before_stop_for_test(mut app, before_stop, .wayland,
			before_stop_oracle)
		stop_error, terminal_snapshot := native_stop_twice_with_any_proof_for_test(mut app)!
		assert stop_error == ''
		assert app.stop_terminal == ''
		assert !terminal_snapshot.trace_overflow
		assert native_lifetime_trace_entry_count_for_test(terminal_snapshot, cleanup_context) == 0
		assert terminal_snapshot.registry.tickets.len == 0
		terminal_oracle := native_release_oracle_snapshot_for_test(.wayland)
		assert !terminal_oracle.overflow
		callback_destroy_count = 0
		callback_completion_count = 0
		for record in terminal_oracle.records {
			if record.domain == .wayland && record.identity == created_callback {
				if record.kind == 2 {
					callback_destroy_count++
				}
			}
		}
		for completion in terminal_oracle.callback_completions {
			if completion.domain == .wayland && completion.kind == 3
				&& completion.identity == created_callback {
				callback_completion_count++
			}
		}
		assert callback_destroy_count == 1
		assert callback_completion_count == 0
		assert native_release_oracle_record_sequence_for_test(terminal_oracle.records, .wayland, 2,
			created_callback) == callback_destroy_sequence
	}

	fn native_wayland_window_counts_for_test(app &App, window WindowId) !(int, int, int) {
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			record := app.backend.wayland.windows[index]
			current := if record.fallback_current_buffer == unsafe { nil } {
				0
			} else {
				1
			}
			egl_window := if record.wl_egl_window == unsafe { nil } { 0 } else { 1 }
			return record.fallback_buffers.len, current, egl_window
		} $else {
			_ = app
			_ = window
			return error(err_backend_unsupported)
		}
	}

	fn native_wayland_buffer_proof_for_test(app &App, window WindowId) !NativeWaylandBufferProof {
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return error(err_window_not_found)
			}
			record := app.backend.wayland.windows[index]
			mut buffers := []u64{cap: record.fallback_buffers.len}
			for buffer in record.fallback_buffers {
				buffers << native_identity(buffer)
			}
			return NativeWaylandBufferProof{
				buffers:               buffers
				current:               native_identity(record.fallback_current_buffer)
				egl_window:            native_identity(record.wl_egl_window)
				egl_window_ticket:     record.wl_egl_window_ticket
				frame_callback:        native_identity(record.frame_callback)
				frame_callback_ticket: record.frame_callback_ticket
			}
		} $else {
			_ = app
			_ = window
			return error(err_backend_unsupported)
		}
	}

	fn native_assert_wayland_null_egl_window_release_for_test(snapshot NativeAuthorityProofSnapshot, acceptance_index int, primary_context NativeOperationContext, actual_object u64, parent_surface_identity u64, expected_proof_generation u64, before_oracle NativeReleaseOracleSnapshot, after_oracle NativeReleaseOracleSnapshot) NativeOperationContext {
		assert acceptance_index >= 0
		assert acceptance_index + 8 <= snapshot.trace_len
		assert actual_object != 0
		assert parent_surface_identity != 0
		assert expected_proof_generation != 0
		assert snapshot.proof_generation == expected_proof_generation
		primary_acceptance := snapshot.trace[acceptance_index]
		assert primary_acceptance.milestone == .acceptance
		assert native_operation_contexts_identical(primary_acceptance.context, primary_context)
		primary_health := snapshot.trace[acceptance_index + 1]
		assert primary_health.milestone == .health_latched
		assert native_operation_contexts_identical(primary_health.context, primary_context)
		assert primary_health.health == .unavailable

		release_start := acceptance_index + 2
		assert snapshot.trace[release_start].milestone == .real_call
		release_context := snapshot.trace[release_start].context
		assert release_context.ordinal != 0
		assert release_context.ordinal >= snapshot.proof_ordinal_floor
		owner_seed := NativeOperationSeed{
			presence_mask:      native_context_window_target_fields
			call_site:          .window_prepare
			scope:              .window_target
			window:             primary_context.window
			target_generation:  primary_context.target_generation
			batch_epoch:        primary_context.batch_epoch
			window_lease_epoch: primary_context.window_lease_epoch
			target_lease_epoch: primary_context.target_lease_epoch
		}
		expected_context := NativeOperationContext{
			authority_scope:        .app_lifetime
			authority_token:        snapshot.app_lifetime_token
			renderer_attempt_token: 0
			app_identity:           snapshot.app_identity
			presence_mask:          owner_seed.presence_mask | native_context_has_target_identity
			domain:                 .wayland
			operation:              .surface_destroy
			call_site:              owner_seed.call_site
			scope:                  owner_seed.scope
			window:                 owner_seed.window
			target_generation:      owner_seed.target_generation
			target_identity:        actual_object
			batch_epoch:            owner_seed.batch_epoch
			window_lease_epoch:     owner_seed.window_lease_epoch
			target_lease_epoch:     owner_seed.target_lease_epoch
			ordinal:                release_context.ordinal
		}
		assert native_operation_contexts_identical(release_context, expected_context)
		ticket := NativeLifetimeTicketProofSnapshot{
			ticket_id:                release_context.ordinal
			app_identity:             snapshot.app_identity
			authority_scope:          .app_lifetime
			authority_token:          snapshot.app_lifetime_token
			domain:                   .wayland
			release_kind:             .wayland_egl_window
			owner_seed:               owner_seed
			proof_generation:         expected_proof_generation
			context:                  release_context
			native_identity:          actual_object
			required_parent_identity: parent_surface_identity
			parent_authority_scope:   .none
			parent_authority_token:   0
			state:                    .bound
		}
		native_assert_bound_ticket_snapshot_for_test(ticket, .wayland_egl_window, actual_object,
			parent_surface_identity)
		release_end := native_assert_lifetime_release_at_for_test(snapshot, ticket, release_start)
		assert release_end == release_start + 6
		release_acceptance := snapshot.trace[release_start + 3]
		assert release_acceptance.local_validation == .void_completion
		assert release_acceptance.result.domain == .wayland
		assert release_acceptance.result.operation == .surface_destroy
		assert release_acceptance.result.scope == .window_target
		assert release_acceptance.result.disposition == .ok
		assert release_acceptance.result.native_code == 0
		assert release_acceptance.result.native_status == 0
		assert release_acceptance.result.display_error == 0
		assert release_acceptance.result.error_text == ''
		assert snapshot.trace[release_start + 4].health == .unavailable
		assert native_lifetime_trace_entry_count_for_test(snapshot, release_context) == 6
		mut matching_entries := 0
		mut matching_real_calls := 0
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.context.domain != .wayland || entry.context.operation != .surface_destroy
				|| entry.context.target_identity != actual_object {
				continue
			}
			matching_entries++
			assert native_operation_contexts_identical(entry.context, release_context)
			if entry.milestone == .real_call {
				matching_real_calls++
			}
		}
		assert matching_entries == 6
		assert matching_real_calls == 1
		mut ticket_id_matches := 0
		mut identity_kind_matches := 0
		for remaining in snapshot.registry.tickets {
			if remaining.ticket_id == release_context.ordinal {
				ticket_id_matches++
			}
			if remaining.release_kind == .wayland_egl_window
				&& remaining.native_identity == actual_object {
				identity_kind_matches++
			}
		}
		assert ticket_id_matches == 0
		assert identity_kind_matches == 0
		assert !before_oracle.overflow
		assert !after_oracle.overflow
		assert after_oracle.records.len == before_oracle.records.len + 1
		assert after_oracle.callback_completions.len == before_oracle.callback_completions.len
		assert after_oracle.unticketed_releases.len == before_oracle.unticketed_releases.len
		native_release_oracle_assert_cleanup_bijection_for_test(before_oracle, after_oracle, [
			ticket,
		])
		return release_context
	}

	fn native_wayland_exercise_null_egl_window_global_loss() ! {
		mut app := native_runtime_new_app_for_test(.wayland)!
		window := native_runtime_new_window_for_test(mut app,
			'native Wayland EGL-window display loss')!
		initial_fallback, initial_current, initial_egl_window := native_wayland_window_counts_for_test(app,
			window)!
		assert initial_fallback == 0
		assert initial_current == 0
		assert initial_egl_window == 0
		app.backend.native_operations.arm_proof()
		expected_proof_generation := app.backend.native_operations.proof.generation
		assert expected_proof_generation != 0
		parent_surface_before := native_wayland_surface_identity_for_test(app, window)
		assert parent_surface_before != 0
		temporary_release_oracle_before := native_release_oracle_snapshot_for_test(.wayland)
		assert !temporary_release_oracle_before.overflow
		phase_sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == phase_sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(phase_sokol_generation)
			}
		}
		mut proof := &NativeEglFrameProof{}
		mut phase_error := ''
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut proof] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			candidate := candidates.filter(it.window == window)
			if candidate.len != 1 {
				return error('Wayland null EGL-window batch expected one target candidate, found `${candidate.len}`')
			}
			$if linux && sokol_wayland ? {
				index := app.backend.wayland.window_record_index(window) or {
					return error(err_window_not_found)
				}
				record := app.backend.wayland.windows[index]
				state := app.render_backend_state()!
				seed := NativeOperationSeed{
					presence_mask:      native_context_window_target_fields | native_context_has_target_identity
					call_site:          .window_prepare
					scope:              .window_target
					window:             window
					target_generation:  record.render_target_generation
					target_identity:    native_identity(record.surface)
					batch_epoch:        batch.epoch
					window_lease_epoch: app.render_runtime.next_lease_epoch
					target_lease_epoch: state.next_target_epoch
				}
				context := native_wayland_primary_context_for_test(app, .window_surface_create,
					seed, app.backend.native_operations.next_ordinal)
				native_arm_wayland_primitive_for_test(mut app, context, NativePrimitiveEvidence{
					valid_mask: native_valid_handle
					handle:     0
				}, 77)!
				proof.failure_context = context
			}
			app.acquire_render_target(batch, window)!
		}) or {
			phase_error = err.msg()
			RenderBatchOutcome{}
		}
		trace_after_loss := app.backend.native_operations.proof.trace_len
		mut post_loss_counts_error := ''
		mut fallback := 0
		mut current := 0
		mut egl_window := 0
		fallback, current, egl_window = native_wayland_window_counts_for_test(app, window) or {
			post_loss_counts_error = err.msg()
			0, 0, 0
		}
		mut probe_index := -1
		mut probe_index_error := ''
		mut before_shm := NativeWaylandBufferProof{}
		mut before_shm_error := ''
		mut shm_error := ''
		mut after_shm := NativeWaylandBufferProof{}
		mut after_shm_error := ''
		mut flush := NativeRenderResult{}
		mut roundtrip := NativeRenderResult{}
		$if linux && sokol_wayland ? {
			probe_index = app.backend.wayland.window_record_index(window) or {
				probe_index_error = err.msg()
				-1
			}
			if probe_index >= 0 {
				before_shm = native_wayland_buffer_proof_for_test(app, window) or {
					before_shm_error = err.msg()
					NativeWaylandBufferProof{}
				}
				app.backend.wayland.ensure_lifecycle_buffer(probe_index) or {
					shm_error = err.msg()
				}
				after_shm = native_wayland_buffer_proof_for_test(app, window) or {
					after_shm_error = err.msg()
					NativeWaylandBufferProof{}
				}
			}
			flush = app.backend.wayland.attempt_wayland_flush(NativeOperationSeed{
				call_site: .display_transport
				scope:     .renderer
			})
			roundtrip = app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
				call_site: .display_transport
				scope:     .renderer
			})
		}
		mut blocked_error := ''
		app.with_scheduled_render_batch(fn (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {}) or {
			blocked_error = err.msg()
		}
		trace_after_probes := app.backend.native_operations.proof.trace_len
		parent_surface_after := native_wayland_surface_identity_for_test(app, window)
		phase_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(phase_sokol_generation)!
		temporary_release_oracle_after := native_release_oracle_snapshot_for_test(.wayland)
		phase_snapshot := native_proof_snapshot(&app.backend.native_operations)

		assert phase_error == ''
		assert outcome.error != ''
		assert !outcome.committed
		native_assert_sokol_sequence_for_test(phase_sokol, [])
		mut accepted := NativeRenderResult{}
		mut actual_object := u64(0)
		mut acceptance_index := -1
		for index in 0 .. app.backend.native_operations.proof.trace_len {
			entry := app.backend.native_operations.proof.trace[index]
			if !native_operation_contexts_identical(entry.context, proof.failure_context) {
				continue
			}
			if entry.milestone == .actual_primitive {
				actual_object = entry.actual.handle
				assert actual_object != 0
			}
			if entry.milestone == .effective_primitive {
				assert entry.effective.handle == 0
			}
			if entry.milestone == .acceptance {
				acceptance_index = index
				accepted = entry.result
			}
		}
		assert actual_object != 0
		assert acceptance_index >= 0
		assert accepted.local_validation == .null_output
		assert accepted.disposition == .renderer_unavailable
		assert accepted.scope == .renderer
		assert accepted.display_error == 77
		actual_ticket_context := native_assert_wayland_null_egl_window_release_for_test(phase_snapshot,
			acceptance_index, proof.failure_context, actual_object, parent_surface_before,
			expected_proof_generation, temporary_release_oracle_before,
			temporary_release_oracle_after)
		assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
			actual_ticket_context.ordinal) == -1
		native_assert_wayland_compound_capture_for_test(&app.backend.native_operations,
			proof.failure_context, .renderer_unavailable, 0, 77)
		assert app.backend.renderer_health() == .unavailable
		assert app.backend.wayland.wayland_display_unavailable
		assert post_loss_counts_error == ''
		assert fallback == initial_fallback
		assert current == initial_current
		assert egl_window == 0
		assert parent_surface_after == parent_surface_before
		$if linux && sokol_wayland ? {
			assert probe_index_error == ''
			assert probe_index >= 0
			assert before_shm_error == ''
			assert shm_error == err_render_native_renderer_unavailable
			assert after_shm_error == ''
			assert after_shm == before_shm
			assert after_shm.egl_window == 0
			assert after_shm.egl_window_ticket == 0
			assert flush.blocks_graphics()
			assert roundtrip.blocks_graphics()
		}
		assert blocked_error != ''
		assert trace_after_probes == trace_after_loss
		before_stop := phase_snapshot
		assert native_lifetime_trace_entry_count_for_test(before_stop, actual_ticket_context) == 6
		before_stop_oracle := temporary_release_oracle_after
		_ = native_segment_consumed_trace_before_stop_for_test(mut app, before_stop, .wayland,
			before_stop_oracle)
		remaining_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		cleanup_tickets := native_lifetime_registry_snapshot(&app.backend.native_operations).tickets
		before_release_oracle := native_release_oracle_snapshot_for_test(.wayland)
		temporary_physical_release_count := native_release_oracle_record_count_for_test(before_release_oracle.records,
			.wayland, 1, actual_object)
		assert temporary_physical_release_count ==
			native_release_oracle_record_count_for_test(temporary_release_oracle_before.records, .wayland, 1, actual_object) +
			1
		stop_sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == stop_sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(stop_sokol_generation)
			}
		}
		mut first_error := ''
		app.stop() or { first_error = err.msg() }
		first_reason := app.stop_terminal
		first_status := app.status()
		first := native_proof_snapshot(&app.backend.native_operations)
		first_ownership := native_phase_a_backend_ownership_snapshot(app)
		first_sokol := multiwindow_sokol_trace.typed_snapshot()
		native_uninstall_sokol_trace_for_test(stop_sokol_generation)!
		first_release_oracle := native_release_oracle_snapshot_for_test(.wayland)

		native_assert_sokol_sequence_for_test(first_sokol, [])
		expected_stop_terminal := '${err_render_terminal_aggregate}: ${err_render_terminal_aggregate}: ${err_wayland_flush_failed}'
		assert first_error == expected_stop_terminal
		assert first_reason == expected_stop_terminal
		assert first_status == .stopped
		native_assert_terminal_causes_once_for_test(first_reason)
		assert !first.trace_overflow
		for entry in first.plan {
			assert !entry.armed
		}
		native_assert_complete_native_trace_for_test(first)
		native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
			first_release_oracle, cleanup_tickets)
		assert native_release_oracle_record_count_for_test(first_release_oracle.records, .wayland,
			1, actual_object) == temporary_physical_release_count
		assert first.registry.tickets.len == 0
		assert native_lifetime_trace_entry_count_for_test(first, actual_ticket_context) == 0
		native_assert_phase_a_ticket_retirement_for_test(first, remaining_tickets, 0)
		native_assert_only_phase_a_ticket_releases_for_test(first, remaining_tickets, 0)
		native_assert_phase_a_ticket_release_health_for_test(first, remaining_tickets, .unavailable)

		mut second_error := ''
		app.stop() or { second_error = err.msg() }
		second_reason := app.stop_terminal
		second_status := app.status()
		second := native_proof_snapshot(&app.backend.native_operations)
		second_ownership := native_phase_a_backend_ownership_snapshot(app)
		second_release_oracle := native_release_oracle_snapshot_for_test(.wayland)
		assert second_error == first_error
		assert second_reason == first_reason
		assert second_status == first_status
		native_proof_assert_snapshots_equal(first, second)
		native_phase_a_backend_assert_snapshots_equal(first_ownership, second_ownership)
		native_release_oracle_assert_equal_for_test(first_release_oracle, second_release_oracle)
		assert app.backend.native_operations.disarm_proof()
	}

	fn native_dxgi_new_uninitialized_app_for_test() !&App {
		native_require_parent_watchdog_gate_for_test()!
		mut app := new_app(
			backend:          .win32
			queue_size:       8
			require_renderer: false
		)!
		caps := app.capabilities()
		if caps.backend != .win32 || !caps.native || !caps.multi_window || !caps.win32 {
			message := 'selected Win32 native proof backend is unavailable: ${caps}'
			app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
			return error(message)
		}
		return app
	}

	fn native_exercise_phase_a_trace_overflow_cleanup() ! {
		backend := native_runtime_backend_for_test()!
		if backend !in [.x11, .wayland] {
			return
		}
		mut app := native_runtime_new_app_for_test(backend)!
		_ = native_runtime_new_window_for_test(mut app,
			'native lifetime cleanup after trace overflow')!
		preauthorized_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		assert preauthorized_tickets.len > 0
		assert app.backend.native_operations.arm_proof()
		for index in 0 .. native_operation_trace_capacity {
			app.backend.native_operations.record_entry(NativeOperationTraceEntry{
				milestone: .real_call
				context:   NativeOperationContext{
					authority_scope:        .renderer_attempt
					authority_token:        app.backend.native_operations.renderer_attempt_token
					renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
					app_identity:           app.instance_id
					ordinal:                u64(index + 1000)
				}
			})
		}
		prefix := app.backend.native_operations.proof.trace
		assert app.backend.native_operations.proof.trace_len == native_operation_trace_capacity
		assert !app.backend.native_operations.proof.trace_overflow
		before_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		app.stop()!
		first := native_proof_snapshot(&app.backend.native_operations)
		first_ownership := native_phase_a_backend_ownership_snapshot(app)
		first_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		assert first.trace_len == native_operation_trace_capacity
		assert first.trace_overflow
		assert first.registry.tickets.len == 0
		native_proof_assert_trace_equal(prefix, first.trace)
		assert native_egl_lifetime_is_cleared_for_test(app)
		for ticket in preauthorized_tickets {
			assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
				ticket.ticket_id) == -1
		}
		native_release_oracle_assert_cleanup_bijection_for_test(before_release_oracle,
			first_release_oracle, preauthorized_tickets)
		app.stop()!
		second := native_proof_snapshot(&app.backend.native_operations)
		second_ownership := native_phase_a_backend_ownership_snapshot(app)
		second_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		native_proof_assert_snapshots_equal(first, second)
		native_phase_a_backend_assert_snapshots_equal(first_ownership, second_ownership)
		native_release_oracle_assert_equal_for_test(first_release_oracle, second_release_oracle)
		assert app.backend.native_operations.disarm_proof()
	}

	fn native_exercise_real_boundary_ordinal_exhaustion() ! {
		backend := native_runtime_backend_for_test()!
		if backend !in [.x11, .wayland] {
			return
		}
		mut app := native_runtime_new_app_for_test(backend)!
		window := native_runtime_new_window_for_test(mut app, 'native ordinal exhaustion boundary')!
		preauthorized_tickets := native_phase_a_assert_canonical_ticket_slots_for_test(app)
		assert preauthorized_tickets.len > 0
		assert app.backend.native_operations.arm_proof()
		sokol_generation := native_install_sokol_trace_for_test()!
		defer {
			if multiwindow_sokol_trace.active_generation() == sokol_generation {
				_ = multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
			}
		}
		mut state := &NativeOrdinalExhaustionState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, preauthorized_tickets, backend, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			before_acquisition := native_phase_a_backend_ownership_snapshot(app)
			state.pre_window = native_phase_a_window_ownership_for_test(before_acquisition, window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			state.lease = acquisition.lease
			state.target = native_egl_target_for_test(app, window)!
			assert state.target.identity != 0
			assert state.target.ticket_id != 0
			state.boundary_ownership = native_phase_a_backend_ownership_snapshot(app)
			state.post_window = native_phase_a_window_ownership_for_test(state.boundary_ownership,
				window)
			state.post_acquisition_tickets =
				native_phase_a_assert_canonical_ticket_slots_for_test(app)
			state.setup_prefix = native_proof_snapshot(&app.backend.native_operations)
			setup_end := native_assert_ordinal_setup_prefix_for_test(backend, state.setup_prefix,
				state.pre_window, state.post_window, state.boundary_ownership, acquisition.lease,
				state.target)
			assert setup_end == state.setup_prefix.trace_len
			native_assert_ordinal_setup_ticket_delta_for_test(backend, preauthorized_tickets,
				state.post_acquisition_tickets, state.pre_window, state.post_window,
				acquisition.lease, state.setup_prefix, state.boundary_ownership.egl_display)!

			preserved_plan :=
				native_clear_trace_preserving_consumed_plan_for_test(mut app.backend.native_operations)
			native_proof_assert_plan_equal(state.setup_prefix.plan, preserved_plan)
			cleared := native_proof_snapshot(&app.backend.native_operations)
			assert cleared.trace_len == 0
			assert !cleared.trace_overflow
			assert cleared.proof_generation == state.setup_prefix.proof_generation
			assert cleared.next_proof_generation == state.setup_prefix.next_proof_generation
			assert cleared.proof_ordinal_floor == state.setup_prefix.proof_ordinal_floor
			assert cleared.proof_accepting_plans == state.setup_prefix.proof_accepting_plans
			assert cleared.next_ordinal == state.setup_prefix.next_ordinal
			assert cleared.sequence_exhausted == state.setup_prefix.sequence_exhausted
			assert cleared.terminal_cause == state.setup_prefix.terminal_cause
			native_proof_assert_plan_equal(state.setup_prefix.plan, cleared.plan)
			native_lifetime_registry_assert_snapshots_equal(state.setup_prefix.registry,
				cleared.registry)

			state.armed_context = native_egl_frame_context_for_test(app, acquisition.lease,
				.window_activate, .make_current, state.target, ~u64(0))
			assert state.armed_context.renderer_attempt_token == app.backend.native_operations.renderer_attempt_token
			assert state.armed_context.app_identity == app.instance_id
			assert state.armed_context.presence_mask == (native_context_window_target_fields | native_context_has_target_identity)
			assert state.armed_context.domain == .egl
			assert state.armed_context.operation == .make_current
			assert state.armed_context.call_site == .window_activate
			assert state.armed_context.scope == .window_target
			assert state.armed_context.window == window
			assert state.armed_context.target_generation == state.target.generation
			assert state.armed_context.target_identity == state.target.identity
			assert state.armed_context.batch_epoch == acquisition.lease.batch_epoch
			assert state.armed_context.window_lease_epoch == acquisition.lease.window_epoch
			assert state.armed_context.target_lease_epoch == acquisition.lease.target_epoch
			assert state.armed_context.ordinal == ~u64(0)
			assert app.backend.native_operations.arm(state.armed_context, NativePrimitiveEvidence{
				valid_mask:   native_valid_return_value
				return_value: -1
			})
			native_release_oracle_reset_for_test(app.backend.kind)
			state.boundary_release_oracle =
				native_release_oracle_snapshot_for_test(app.backend.kind)
			assert !state.boundary_release_oracle.overflow
			assert state.boundary_release_oracle.records.len == 0
			assert state.boundary_release_oracle.callback_completions.len == 0
			state.boundary_ownership = native_phase_a_backend_ownership_snapshot(app)
			app.backend.native_operations.next_ordinal = ~u64(0)
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		terminal := native_proof_snapshot(&app.backend.native_operations)
		expected_renderer_terminal := '${err_render_terminal_aggregate}: ${err_render_native_renderer_unavailable}'
		assert outcome.error == expected_renderer_terminal
		assert app.render_runtime.renderer_terminal == expected_renderer_terminal
		native_assert_terminal_causes_once_for_test(outcome.error)
		assert terminal.sequence_exhausted
		assert terminal.terminal_cause == .sequence_exhausted
		assert terminal.next_ordinal == 0
		assert terminal.trace_len == 0
		assert !terminal.trace_overflow
		assert terminal.proof_generation == state.setup_prefix.proof_generation
		assert terminal.proof_ordinal_floor == state.setup_prefix.proof_ordinal_floor
		native_lifetime_registry_assert_snapshots_equal(state.setup_prefix.registry,
			terminal.registry)
		mut terminal_armed := 0
		for entry in terminal.plan {
			assert !entry.listener_registration_pending
			if entry.armed {
				terminal_armed++
				assert entry.proof_generation == terminal.proof_generation
				assert native_operation_contexts_identical(entry.context, state.armed_context)
				assert native_proof_evidence_equal(entry.evidence, NativePrimitiveEvidence{
					valid_mask:   native_valid_return_value
					return_value: -1
				})
			}
		}
		assert terminal_armed == 1
		native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
		after_boundary_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		native_release_oracle_assert_equal_for_test(state.boundary_release_oracle,
			after_boundary_oracle)
		after_boundary_ownership := native_phase_a_backend_ownership_snapshot(app)
		native_phase_a_backend_assert_durable_ownership_equal(state.boundary_ownership,
			after_boundary_ownership)
		assert state.boundary_ownership.render_health == .ready
		assert after_boundary_ownership.render_health == .unavailable
		trace_before_replay := native_proof_snapshot(&app.backend.native_operations)
		ownership_before_replay := native_phase_a_backend_ownership_snapshot(app)
		mut blocked_error := ''
		app.with_scheduled_render_batch(fn (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {}) or {
			blocked_error = err.msg()
		}
		assert blocked_error == '${err_render_renderer_failed}: ${expected_renderer_terminal}'
		native_proof_assert_snapshots_equal(trace_before_replay,
			native_proof_snapshot(&app.backend.native_operations))
		native_phase_a_backend_assert_snapshots_equal(ownership_before_replay,
			native_phase_a_backend_ownership_snapshot(app))
		before_stop_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		native_release_oracle_assert_equal_for_test(after_boundary_oracle, before_stop_oracle)
		mut first_error := ''
		app.stop() or { first_error = err.msg() }
		first := native_proof_snapshot(&app.backend.native_operations)
		first_ownership := native_phase_a_backend_ownership_snapshot(app)
		first_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		first_reason := app.stop_terminal
		mut second_error := ''
		app.stop() or { second_error = err.msg() }
		second := native_proof_snapshot(&app.backend.native_operations)
		second_ownership := native_phase_a_backend_ownership_snapshot(app)
		second_release_oracle := native_release_oracle_snapshot_for_test(app.backend.kind)
		sokol := multiwindow_sokol_trace.typed_snapshot()
		sokol_uninstalled := multiwindow_sokol_trace.try_uninstall_generation(sokol_generation)
		sokol_generation_after_cleanup := multiwindow_sokol_trace.active_generation()
		assert app.status() == .stopped
		expected_stop_terminal := match backend {
			.x11 {
				''
			}
			.wayland {
				'${err_render_terminal_aggregate}: ${err_render_terminal_aggregate}: ${err_wayland_flush_failed}'
			}
			else {
				'ordinal exhaustion selected unsupported backend'
			}
		}

		assert first_reason == expected_stop_terminal
		assert app.stop_terminal == expected_stop_terminal
		assert first_error == expected_stop_terminal
		assert second_error == expected_stop_terminal
		mut retained_armed := 0
		for entry in first.plan {
			if entry.proof_generation != first.proof_generation {
				continue
			}
			assert !entry.listener_registration_pending
			if entry.armed {
				retained_armed++
				assert entry.proof_generation == trace_before_replay.proof_generation
				assert native_operation_contexts_identical(entry.context, state.armed_context)
				assert native_proof_evidence_equal(entry.evidence, NativePrimitiveEvidence{
					valid_mask:   native_valid_return_value
					return_value: -1
				})
			}
		}
		assert retained_armed == 1
		assert first.trace_len > 0
		assert !first.trace_overflow
		assert first.registry.tickets.len == 0
		native_assert_phase_a_ticket_retirement_for_test(first, state.post_acquisition_tickets, 0)
		native_assert_only_phase_a_ticket_releases_for_test(first, state.post_acquisition_tickets,
			0)
		native_assert_phase_a_ticket_release_health_for_test(first, state.post_acquisition_tickets,
			.unavailable)
		native_release_oracle_assert_cleanup_bijection_for_test(before_stop_oracle,
			first_release_oracle, state.post_acquisition_tickets)
		native_proof_assert_snapshots_equal(first, second)
		native_phase_a_backend_assert_snapshots_equal(first_ownership, second_ownership)
		native_release_oracle_assert_equal_for_test(first_release_oracle, second_release_oracle)
		assert !sokol.overflow
		native_assert_sokol_sequence_for_test(sokol, [])
		assert sokol_uninstalled
		assert sokol_generation_after_cleanup == 0
		_ = app.backend.native_operations.inject(state.armed_context, NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value
			return_value: 1
		})
		assert app.backend.native_operations.disarm_proof()
	}

	fn native_dxgi_startup_context_for_test(app &App) NativeOperationContext {
		return NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        app.backend.native_operations.renderer_attempt_token
			renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
			app_identity:           app.instance_id
			domain:                 .dxgi
			operation:              .device_create
			call_site:              .renderer_start
			scope:                  .renderer
			ordinal:                app.backend.native_operations.next_ordinal
		}
	}

	fn native_dxgi_exercise_startup_failure(operation_hresult i64, expect_null bool) ! {
		mut app := native_dxgi_new_uninitialized_app_for_test()!
		assert app.backend.native_operations.arm_proof()
		context := native_dxgi_startup_context_for_test(app)
		evidence := if expect_null {
			NativePrimitiveEvidence{
				valid_mask:        native_valid_return_value | native_valid_handle | native_valid_object_identity_0
				return_value:      0
				handle:            0
				object_identity_0: 0
			}
		} else {
			NativePrimitiveEvidence{
				valid_mask:   native_valid_return_value
				return_value: operation_hresult
			}
		}
		assert app.backend.native_operations.arm(context, evidence)
		mut start_error := ''
		app.backend.ensure_renderer_started() or { start_error = err.msg() }
		assert start_error != ''
		assert app.render_bridge == unsafe { nil }
		assert !app.renderer_is_usable()
		assert native_real_call_count_for_test(&app.backend.native_operations, .device_create) == 1
		mut actual_device := u64(0)
		mut actual_context := u64(0)
		mut acceptance_index := -1
		mut accepted := NativeRenderResult{}
		mut query_context := NativeOperationContext{}
		for index in 0 .. app.backend.native_operations.proof.trace_len {
			entry := app.backend.native_operations.proof.trace[index]
			if native_operation_contexts_identical(entry.context, context) {
				if entry.milestone == .actual_primitive {
					actual_device = entry.actual.handle
					actual_context = entry.actual.object_identity_0
				}
				if entry.milestone == .acceptance {
					acceptance_index = index
					accepted = entry.result
				}
			}
			if entry.milestone == .real_call && entry.context.operation == .dxgi_removal_query {
				query_context = entry.context
			}
		}
		assert actual_device != 0
		assert actual_context != 0
		assert query_context.ordinal == context.ordinal + 1
		assert query_context.target_identity == actual_device
		assert acceptance_index >= 0
		if expect_null {
			assert accepted.local_validation == .null_output
			assert accepted.disposition == .renderer_unavailable
			assert accepted.native_code == 0
		} else {
			assert accepted.local_validation == .none
			assert accepted.disposition == .renderer_lost
			assert u32(accepted.native_code) == u32(operation_hresult)
		}
		capture_acceptance_index := native_assert_dxgi_compound_capture_for_test(&app.backend.native_operations,
			context, query_context, accepted.disposition, operation_hresult, 0)
		assert capture_acceptance_index == acceptance_index
		assert !app.backend.native_operations.proof.plan[0].armed
		assert app.backend.win32.device == unsafe { nil }
		assert app.backend.win32.device_context == unsafe { nil }
		assert app.backend.win32.factory == unsafe { nil }
		_, stopped := native_stop_twice_with_any_proof_for_test(mut app)!
		for index in acceptance_index + 1 .. stopped.trace_len {
			entry := stopped.trace[index]
			if entry.milestone == .real_call {
				assert entry.context.domain == .dxgi
				assert entry.context.operation == .object_release
			}
		}
		native_dxgi_assert_released_after_for_test(stopped, [actual_device, actual_context],
			acceptance_index)
		native_dxgi_assert_durable_slots_cleared_for_test(app)
	}

	fn native_dxgi_exercise_startup_failures() ! {
		native_dxgi_exercise_startup_failure(0, true)!
		for code in [u32(0x887a0005), u32(0x887a0007), u32(0x887a0006), u32(0x887a0020)] {
			native_dxgi_exercise_startup_failure(i64(code), false)!
		}
	}

	fn native_dxgi_frame_context_for_test(app &App, lease RenderTargetLease, operation NativeRenderOperation, ordinal u64) !(NativeOperationContext, NativeOperationContext) {
		$if windows && sokol_d3d11 ? {
			index := app.backend.win32.window_record_index(lease.window) or {
				return error(err_window_not_found)
			}
			record := app.backend.win32.windows[index]
			primary := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
				app_identity:           app.instance_id
				presence_mask:          native_context_window_target_fields | native_context_has_target_identity
				domain:                 .dxgi
				operation:              operation
				call_site:              if operation == .present {
					.window_finalize
				} else {
					.window_prepare
				}
				scope:                  .window_target
				window:                 lease.window
				target_generation:      record.render_target_generation
				target_identity:        native_identity(record.swapchain)
				batch_epoch:            lease.batch_epoch
				window_lease_epoch:     lease.window_epoch
				target_lease_epoch:     lease.target_epoch
				ordinal:                ordinal
			}
			return primary, native_dxgi_query_context_for_test(primary,
				native_identity(app.backend.win32.device))
		} $else {
			_ = app
			_ = lease
			_ = operation
			_ = ordinal
			return error(err_backend_unsupported)
		}
	}

	fn native_dxgi_exercise_present_failure(operation_hresult i64, removal_hresult i64, expected NativeRenderDisposition) ! {
		mut app := native_runtime_new_app_for_test(.win32)!
		$if windows && sokol_d3d11 ? {
			assert app.backend.win32.using_warp
		}
		window := native_runtime_new_window_for_test(mut app, 'native DXGI present evidence')!
		initial := app.render_window_snapshot(window)!
		assert app.backend.native_operations.arm_proof()
		sokol_generation := native_install_sokol_trace_for_test()!
		mut proof := &NativeDxgiFrameProof{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, operation_hresult, removal_hresult, mut proof] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			proof.lease = acquisition.lease
			proof.pass_snapshot = acquisition.snapshot
			proof.identities = native_dxgi_release_identities_for_test(app, window)!
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut app, operation_hresult, removal_hresult, mut proof] () ! {
				native_assert_authoritative_sokol_swapchain_for_test(app,
					multiwindow_sokol_trace.typed_snapshot(), proof.lease, proof.pass_snapshot)!
				native_reset_or_clear_proof_for_test(mut app.backend.native_operations)
				primary, query := native_dxgi_frame_context_for_test(app, proof.lease, .present,
					app.backend.native_operations.next_ordinal)!
				native_arm_dxgi_primitive_for_test(mut app.backend.native_operations, primary,
					query, operation_hresult, removal_hresult)!
				proof.context = primary
				proof.query = query
			})!
		})!
		sokol := multiwindow_sokol_trace.typed_snapshot()
		native_assert_sokol_sequence_for_test(sokol, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		assert outcome.error != ''
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 0
		after := app.render_window_snapshot(window)!
		assert after.frame_serial == initial.frame_serial + 1
		assert after.submitted_frame == initial.submitted_frame
		assert after.dirty_epoch > after.consumed_epoch
		acceptance_index := native_assert_dxgi_compound_capture_for_test(&app.backend.native_operations,
			proof.context, proof.query, expected, operation_hresult, removal_hresult)
		if expected == .renderer_lost {
			assert app.backend.renderer_health() == .lost
			multiwindow_sokol_trace.reset()
			trace_after_loss := app.backend.native_operations.proof.trace_len
			mut blocked_error := ''
			app.with_scheduled_render_batch(fn (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {}) or {
				blocked_error = err.msg()
			}
			assert blocked_error != ''
			assert app.backend.native_operations.proof.trace_len == trace_after_loss
		} else {
			assert expected == .operation_failed
			assert app.backend.renderer_health() == .ready
			native_uninstall_sokol_trace_for_test(sokol_generation)!
		}
		assert native_real_call_count_for_test(&app.backend.native_operations, .device_create) == 0
		assert proof.identities.len == 11
		release_identities := proof.identities.reverse()
		before_stop_registry := native_lifetime_registry_snapshot(&app.backend.native_operations)
		assert before_stop_registry.tickets.len == release_identities.len
		mut release_tickets := []NativeLifetimeTicketProofSnapshot{cap: release_identities.len}
		for index, identity in release_identities {
			mut matches := 0
			mut ticket_id := u64(0)
			for candidate in before_stop_registry.tickets {
				if candidate.native_identity == identity {
					matches++
					ticket_id = candidate.ticket_id
				}
			}
			assert matches == 1
			expected_authority_scope := if index >= 4 && index < 8 {
				NativeOperationAuthorityScope.renderer_attempt
			} else {
				NativeOperationAuthorityScope.app_lifetime
			}
			ticket := native_phase_b_assert_bound_ticket_for_test(&app.backend.native_operations,
				ticket_id, .com_reference, identity, 0, expected_authority_scope)
			expected_call_site := if index < 4 {
				NativeRenderCallSite.window_prepare
			} else if index < 8 {
				NativeRenderCallSite.anchor_create
			} else {
				NativeRenderCallSite.renderer_start
			}
			expected_scope := if index < 4 {
				NativeRenderScope.window_target
			} else if index < 8 {
				NativeRenderScope.anchor
			} else {
				NativeRenderScope.renderer
			}
			assert ticket.owner_seed.call_site == expected_call_site
			assert ticket.owner_seed.scope == expected_scope
			assert ticket.context.call_site == expected_call_site
			assert ticket.context.scope == expected_scope
			for prior in release_tickets {
				assert ticket.native_identity != prior.native_identity
				assert !native_operation_contexts_identical(ticket.context, prior.context)
			}
			release_tickets << ticket
		}
		assert release_tickets.len == 11
		mut stopped := NativeAuthorityProofSnapshot{}
		if expected == .operation_failed {
			assert app.render_runtime.renderer_terminal == outcome.error
			stop_error, operation_failed_stopped :=
				native_stop_twice_with_any_proof_for_test(mut app)!
			assert stop_error == ''
			assert app.stop_terminal == ''
			assert app.render_runtime.renderer_terminal == outcome.error
			stopped = operation_failed_stopped
		} else {
			assert expected == .renderer_lost
			native_assert_terminal_causes_once_for_test(outcome.error)
			assert app.render_runtime.renderer_terminal == outcome.error
			stop_error, renderer_lost_stopped := native_stop_twice_with_any_proof_for_test(mut app)!
			assert stop_error == ''
			assert app.stop_terminal == ''
			assert app.render_runtime.renderer_terminal == outcome.error
			stopped = renderer_lost_stopped
		}
		if expected == .renderer_lost {
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
			native_uninstall_sokol_trace_for_test(sokol_generation)!
		}
		mut shutdown_status_calls := 0
		mut shutdown_release_calls := 0
		mut prior_release_start := acceptance_index
		for index in acceptance_index + 1 .. stopped.trace_len {
			entry := stopped.trace[index]
			if entry.milestone == .real_call {
				if entry.context.operation == .device_status {
					shutdown_status_calls++
					assert expected == .operation_failed
					assert entry.context.call_site == .shutdown_anchor
				} else {
					assert shutdown_release_calls < release_tickets.len
					ticket := release_tickets[shutdown_release_calls]
					assert entry.context.operation == .object_release
					assert entry.context.domain == .dxgi
					assert native_operation_contexts_identical(entry.context, ticket.context)
					release_start := native_dxgi_release_sequence_start_for_test(stopped,
						ticket.native_identity)
					assert release_start == index
					assert release_start > prior_release_start
					prior_release_start = release_start
					shutdown_release_calls++
					if entry.context.scope == .window_target {
						assert entry.context.call_site == .window_prepare
					} else if entry.context.scope == .anchor {
						assert entry.context.call_site == .anchor_create
					} else {
						assert entry.context.scope == .renderer
						assert entry.context.call_site == .renderer_start
					}
				}
			}
		}
		expected_shutdown_status_calls := if expected == .operation_failed { 1 } else { 0 }
		assert shutdown_status_calls == expected_shutdown_status_calls
		assert shutdown_release_calls == release_tickets.len
		native_dxgi_assert_released_after_for_test(stopped, proof.identities, acceptance_index)
		native_dxgi_assert_durable_slots_cleared_for_test(app)
	}

	fn native_dxgi_exercise_present_failures() ! {
		native_dxgi_exercise_present_failure(i64(u32(0x887a0001)), 0, .operation_failed)!
		native_dxgi_exercise_present_failure(i64(u32(0x887a0005)), 0, .renderer_lost)!
		for reason in [u32(0x887a0007), u32(0x887a0006), u32(0x887a0020)] {
			native_dxgi_exercise_present_failure(i64(u32(0x80004005)), i64(reason), .renderer_lost)!
		}
	}

	fn native_dxgi_render_healthy_target_for_test(mut app App, window WindowId) !NativeEglTargetProof {
		mut state := &NativeEglHealthyTargetState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			$if windows && sokol_d3d11 ? {
				index := app.backend.win32.window_record_index(window) or {
					return error(err_window_not_found)
				}
				record := app.backend.win32.windows[index]
				state.target = NativeEglTargetProof{
					generation: record.render_target_generation
					identity:   native_identity(record.swapchain)
				}
			}
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn () ! {})!
		})!
		assert outcome.error == ''
		assert outcome.finalized_submissions == 1
		assert state.target.generation != 0
		assert state.target.identity != 0
		return state.target
	}

	struct NativeWin32LifetimeOracleRecord {
		sequence           u64
		kind               u64
		identity           u64
		parent_identity    u64
		output_identity    u64
		auxiliary_identity u64
		observed_count     u64
		valid_mask         u64
		return_value       i64
		thread_identity    u64
	}

	struct NativeAppKitLifetimeOracleRecord {
		sequence             u64
		kind                 u64
		identity             u64
		parent_identity      u64
		output_identity      u64
		auxiliary_identity   u64
		auxiliary_identity_1 u64
		auxiliary_identity_2 u64
		valid_mask           u64
		thread_identity      u64
	}

	const native_appkit_side_effect_capacity = 256
	const native_appkit_process_commit_oracle_kind = u64(15)
	const native_win32_process_commit_oracle_kind = u64(17)

	enum NativeAppKitSideEffectKind {
		invalid
		bridge_retain
		bridge_release
		release_probe_dealloc
		pool_push
		pool_probe_create
		pool_probe_dealloc
		pool_pop
		next_drawable
		current_drawable_clear
		layer_device_set_read
	}

	enum NativeAppKitSideEffectSubject {
		none
		device_root
		window_root
		anchor_root
		pool
		drawable
		layer
	}

	struct NativeAppKitSideEffectRecord {
		generation         u64
		sequence           u64
		kind               NativeAppKitSideEffectKind
		subject            NativeAppKitSideEffectSubject
		identity           u64
		parent_identity    u64
		before_identity    u64
		after_identity     u64
		auxiliary_identity u64
		thread_identity    u64
		main_thread        bool
	}

	struct NativeAppKitSideEffectSnapshot {
		generation u64
		enabled    bool
		overflow   bool
		records    []NativeAppKitSideEffectRecord
	}

	struct NativeAppKitExpectedReleaseProbe {
		generation                    u64
		identity                      u64
		subject                       NativeAppKitSideEffectSubject
		created_after_operation_count int
	}

	struct NativeAppKitRegistryMainThreadState {
	mut:
		registry_side_generation u64
		rejection_context        NativeOperationContext
		wrong_thread_ticket      u64
		window_probe_ticket      u64
		anchor_probe_ticket      u64
		device_release_context   NativeOperationContext
		window_release_context   NativeOperationContext
		probes                   []NativeAppKitExpectedReleaseProbe
		registry_side            NativeAppKitSideEffectSnapshot
	}

	struct NativeWin32WindowReplaySnapshot {
		id                        WindowId
		hwnd                      u64
		config                    WindowConfig
		width                     int
		height                    int
		framebuffer_width         int
		framebuffer_height        int
		destroyed                 bool
		render_resize_pending     bool
		suppress_resize_event     bool
		queued_events             []Win32NativeQueuedEvent
		mouse_x                   f32
		mouse_y                   f32
		mouse_dx                  f32
		mouse_dy                  f32
		mouse_pos_valid           bool
		iconified                 bool
		pending_dropped_files     []string
		pending_drop_modifiers    u32
		pending_high_surrogate    u32
		suppress_control_char     u32
		swapchain                 u64
		swapchain_ticket          u64
		pending_backbuffer        u64
		pending_backbuffer_ticket u64
		render_view               u64
		render_view_ticket        u64
		depth_texture             u64
		depth_texture_ticket      u64
		depth_stencil_view        u64
		depth_stencil_view_ticket u64
		render_target_generation  u64
	}

	struct NativeWin32BackendReplaySnapshot {
		app_status                       AppStatus
		stop_terminal                    string
		started                          bool
		device                           u64
		device_ticket                    u64
		device_context                   u64
		device_context_ticket            u64
		factory                          u64
		factory_ticket                   u64
		pending_init_dxgi_device         u64
		pending_init_dxgi_device_ticket  u64
		pending_init_adapter             u64
		pending_init_adapter_ticket      u64
		using_warp                       bool
		anchor_color_texture             u64
		anchor_color_texture_ticket      u64
		anchor_render_view               u64
		anchor_render_view_ticket        u64
		anchor_depth_texture             u64
		anchor_depth_texture_ticket      u64
		anchor_depth_stencil_view        u64
		anchor_depth_stencil_view_ticket u64
		anchor_committed                 bool
		render_sequence                  u64
		render_health                    NativeRendererHealth
		poll_error                       string
		event_sequence_terminal          string
		windows                          []NativeWin32WindowReplaySnapshot
		authority                        NativeAuthorityProofSnapshot
	}

	@[heap]
	struct NativeWin32PhaseBExhaustionState {
	mut:
		armed_context      NativeOperationContext
		boundary_oracle    []NativeWin32LifetimeOracleRecord
		boundary_ownership NativeWin32BackendReplaySnapshot
	}

	struct NativeAppKitWindowReplaySnapshot {
		id                       WindowId
		state                    u64
		state_ticket             u64
		width                    int
		height                   int
		framebuffer_width        int
		framebuffer_height       int
		native_destroyed         bool
		render_target_generation u64
		next_frame_lease         u64
		active_frame_lease       u64
		active_drawable          u64
		active_drawable_ticket   u64
		frame_active             bool
	}

	struct NativeAppKitBackendReplaySnapshot {
		app_status                    AppStatus
		stop_terminal                 string
		started                       bool
		device                        u64
		device_ticket                 u64
		anchor_state                  u64
		anchor_state_ticket           u64
		batch_autorelease_pool        u64
		batch_autorelease_pool_ticket u64
		next_anchor_lease             u64
		active_anchor_lease           u64
		active_anchor_drawable        u64
		active_anchor_drawable_ticket u64
		render_sequence               u64
		poll_error                    string
		event_sequence_terminal       string
		render_health                 NativeRendererHealth
		windows                       []NativeAppKitWindowReplaySnapshot
		authority                     NativeAuthorityProofSnapshot
	}

	@[heap]
	struct NativeAppKitPhaseBExhaustionState {
	mut:
		live_pool_ticket     NativeLifetimeTicketProofSnapshot
		live_drawable_ticket NativeLifetimeTicketProofSnapshot
		live_depth_identity  u64
		boundary_oracle      []NativeAppKitLifetimeOracleRecord
		boundary_ownership   NativeAppKitBackendReplaySnapshot
	}

	@[heap]
	struct NativeAppKitEffectiveRejectionState {
	mut:
		drawable_context    NativeOperationContext
		drawable_pass_error string
	}

	@[heap]
	struct NativeAppKitObservedDrawableState {
	mut:
		observed_drawable u64
		observed_depth    u64
		observed_ticket   u64
		observed_parent   u64
	}

	struct NativeWrongThreadReleaseResult {
		identity        u64
		ticket          u64
		native_released bool
		ticket_retired  bool
	}

	fn native_win32_lifetime_oracle_reset_for_test() {
		$if windows {
			$if sokol_d3d11 ? {
				C.v_multiwindow_test_win32_oracle_reset()
			}
		}
	}

	fn native_win32_lifetime_oracle_snapshot_for_test() []NativeWin32LifetimeOracleRecord {
		mut records := []NativeWin32LifetimeOracleRecord{}
		$if windows {
			$if sokol_d3d11 ? {
				assert C.v_multiwindow_test_win32_oracle_overflow_get() == 0
				count := C.v_multiwindow_test_win32_oracle_count_get()
				for index in u64(0) .. count {
					raw := C.v_multiwindow_test_win32_oracle_record_get(index)
					records << NativeWin32LifetimeOracleRecord{
						sequence:           raw.sequence
						kind:               raw.kind
						identity:           raw.identity
						parent_identity:    raw.parent_identity
						output_identity:    raw.output_identity
						auxiliary_identity: raw.auxiliary_identity
						observed_count:     raw.observed_count
						valid_mask:         raw.valid_mask
						return_value:       raw.return_value
						thread_identity:    raw.thread_identity
					}
				}
			}
		}
		return records
	}

	fn native_dxgi_exercise_physical_resize_failure_for_test() ! {
		$if windows && sokol_d3d11 ? {
			mut app := native_runtime_new_app_for_test(.win32)!
			C.v_multiwindow_test_win32_resize_invalid_call_reset()
			defer {
				C.v_multiwindow_test_win32_resize_invalid_call_reset()
				if app.backend.native_operations.proof != unsafe { nil } {
					_ = app.backend.native_operations.disarm_proof()
				}
			}
			window := native_runtime_new_window_for_test(mut app, 'physical DXGI resize failure')!
			_ = native_dxgi_render_healthy_target_for_test(mut app, window)!
			app.resize_window(window, 208, 144)!
			for _ in 0 .. 64 {
				app.poll_events()!
				if app.render_window_eligible(window)! {
					break
				}
			}
			assert app.render_window_eligible(window)!
			index := app.backend.win32.window_record_index(window) or {
				return error(err_window_not_found)
			}
			record := app.backend.win32.windows[index]
			view_ticket_ids := [record.depth_stencil_view_ticket, record.depth_texture_ticket,
				record.render_view_ticket]
			mut retired_ticket_ids := view_ticket_ids.clone()
			retired_ticket_ids << record.swapchain_ticket
			for ticket_id in retired_ticket_ids {
				assert ticket_id != 0
			}
			app.backend.win32.windows[index].render_resize_pending = true
			assert app.backend.native_operations.arm_proof()
			before_oracle := native_win32_lifetime_oracle_snapshot_for_test()
			resize_kind := C.v_multiwindow_test_win32_resize_buffers_kind()
			release_kind := C.v_multiwindow_test_win32_release_kind()
			assert C.v_multiwindow_test_win32_resize_invalid_call_arm() == 1
			outcome := app.with_scheduled_render_batch(fn [mut app, window] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
				assert candidates.any(it.window == window)
				_ = app.acquire_render_target(batch, window)!
			})!
			assert outcome.error != ''
			assert !outcome.committed
			assert app.backend.renderer_health() == .ready
			assert app.render_runtime.renderer_terminal == outcome.error
			assert C.v_multiwindow_test_win32_resize_invalid_call_consumed_once() == 1
			resize_results := app.backend.native_operations.proof.trace[..app.backend.native_operations.proof.trace_len].filter(
				it.milestone == .acceptance && it.context.operation == .resize_buffers).map(it.result)
			assert resize_results.len == 1
			assert resize_results[0].disposition == .operation_failed
			assert u32(resize_results[0].native_code) == u32(C.v_multiwindow_test_win32_resize_invalid_call_code())
			after_oracle := native_win32_lifetime_oracle_snapshot_for_test()
			segment := after_oracle[before_oracle.len..]
			assert native_win32_oracle_kind_count_for_test(segment, resize_kind) == 1
			resize_index := native_win32_oracle_kind_index_for_test(segment, resize_kind)
			assert segment[resize_index].valid_mask & native_valid_return_value != 0
			assert segment[resize_index].return_value == 0
			assert native_win32_oracle_kind_count_for_test(segment[..resize_index], release_kind) == view_ticket_ids.len
			assert native_win32_oracle_kind_count_for_test(segment[resize_index + 1..],
				release_kind) == 1
			failed := app.backend.win32.windows[index]
			assert failed.render_resize_pending
			assert failed.swapchain == unsafe { nil } && failed.swapchain_ticket == 0
			assert failed.render_view == unsafe { nil } && failed.render_view_ticket == 0
			assert failed.depth_texture == unsafe { nil } && failed.depth_texture_ticket == 0
			assert failed.depth_stencil_view == unsafe { nil }
				&& failed.depth_stencil_view_ticket == 0
			failed_registry := native_lifetime_registry_snapshot(&app.backend.native_operations)
			for ticket_id in retired_ticket_ids {
				assert failed_registry.tickets.all(it.ticket_id != ticket_id)
			}
			_ = app.prepare_stop()!
			prepared := app.backend.win32.windows[index]
			prepared_ticket_ids := [prepared.swapchain_ticket, prepared.render_view_ticket,
				prepared.depth_texture_ticket, prepared.depth_stencil_view_ticket]
			assert !prepared.render_resize_pending
			assert prepared.swapchain != unsafe { nil } && prepared.render_view != unsafe { nil }
			assert prepared.depth_texture != unsafe { nil }
				&& prepared.depth_stencil_view != unsafe { nil }
			prepared_registry := native_lifetime_registry_snapshot(&app.backend.native_operations)
			for ticket_id in prepared_ticket_ids {
				assert ticket_id != 0
				assert prepared_registry.tickets.count(it.ticket_id == ticket_id
					&& it.state == .bound) == 1
			}
			prepare_event_count := C.v_multiwindow_test_win32_oracle_count_get()
			_ = app.prepare_stop()!
			assert C.v_multiwindow_test_win32_oracle_count_get() == prepare_event_count
			replayed := app.backend.win32.windows[index]
			assert [replayed.swapchain_ticket, replayed.render_view_ticket, replayed.depth_texture_ticket,
				replayed.depth_stencil_view_ticket] == prepared_ticket_ids
			assert !replayed.render_resize_pending
			app.stop()!
			assert app.status() == .stopped
			native_dxgi_assert_durable_slots_cleared_for_test(app)
			assert native_lifetime_registry_snapshot(&app.backend.native_operations).tickets.len == 0
			first_stop_oracle := native_win32_lifetime_oracle_snapshot_for_test()
			assert native_win32_oracle_kind_count_for_test(first_stop_oracle[after_oracle.len..],
				resize_kind) == 0
			app.stop()!
			second_stop_oracle := native_win32_lifetime_oracle_snapshot_for_test()
			assert second_stop_oracle.len == first_stop_oracle.len
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn native_appkit_lifetime_oracle_reset_for_test() {
		$if darwin {
			C.v_multiwindow_test_appkit_oracle_reset()
		}
	}

	fn native_appkit_lifetime_oracle_snapshot_for_test() []NativeAppKitLifetimeOracleRecord {
		mut records := []NativeAppKitLifetimeOracleRecord{}
		$if darwin {
			assert C.v_multiwindow_test_appkit_oracle_overflow_get() == 0
			count := C.v_multiwindow_test_appkit_oracle_count_get()
			for index in u64(0) .. count {
				raw := C.v_multiwindow_test_appkit_oracle_record_get(index)
				records << NativeAppKitLifetimeOracleRecord{
					sequence:             raw.sequence
					kind:                 raw.kind
					identity:             raw.identity
					parent_identity:      raw.parent_identity
					output_identity:      raw.output_identity
					auxiliary_identity:   raw.auxiliary_identity
					auxiliary_identity_1: raw.auxiliary_identity_1
					auxiliary_identity_2: raw.auxiliary_identity_2
					valid_mask:           raw.valid_mask
					thread_identity:      raw.thread_identity
				}
			}
		}
		return records
	}

	fn native_win32_oracle_kind_count_for_test(records []NativeWin32LifetimeOracleRecord, kind u64) int {
		mut count := 0
		for record in records {
			if record.kind == kind {
				count++
			}
		}
		return count
	}

	fn native_win32_oracle_kind_index_for_test(records []NativeWin32LifetimeOracleRecord, kind u64) int {
		for index, record in records {
			if record.kind == kind {
				return index
			}
		}
		return -1
	}

	fn native_win32_oracle_output_index_for_test(records []NativeWin32LifetimeOracleRecord, kind u64, output_identity u64) int {
		for index, record in records {
			if record.kind == kind && record.output_identity == output_identity {
				return index
			}
		}
		return -1
	}

	fn native_appkit_oracle_kind_count_for_test(records []NativeAppKitLifetimeOracleRecord, kind u64) int {
		mut count := 0
		for record in records {
			if record.kind == kind {
				count++
			}
		}
		return count
	}

	fn native_appkit_oracle_kind_index_for_test(records []NativeAppKitLifetimeOracleRecord, kind u64) int {
		for index, record in records {
			if record.kind == kind {
				return index
			}
		}
		return -1
	}

	fn native_win32_oracle_assert_equal_for_test(expected []NativeWin32LifetimeOracleRecord, actual []NativeWin32LifetimeOracleRecord) {
		assert actual.len == expected.len
		for index in 0 .. expected.len {
			assert actual[index] == expected[index]
		}
	}

	fn native_appkit_oracle_assert_equal_for_test(expected []NativeAppKitLifetimeOracleRecord, actual []NativeAppKitLifetimeOracleRecord) {
		assert actual.len == expected.len
		for index in 0 .. expected.len {
			assert actual[index] == expected[index]
		}
	}

	fn native_appkit_side_effect_reset_for_test() u64 {
		$if darwin {
			generation := C.v_multiwindow_appkit_side_effect_reset()
			assert generation != 0
			assert C.v_multiwindow_appkit_side_effect_generation() == generation
			assert C.v_multiwindow_appkit_side_effect_count() == 0
			assert C.v_multiwindow_appkit_side_effect_overflow() == 0
			return generation
		}
		return 0
	}

	fn native_appkit_side_effect_snapshot_for_test(expected_generation u64) NativeAppKitSideEffectSnapshot {
		mut records := []NativeAppKitSideEffectRecord{}
		$if darwin {
			assert expected_generation != 0
			assert C.v_multiwindow_appkit_side_effect_generation() == expected_generation
			assert C.v_multiwindow_appkit_side_effect_overflow() == 0
			count := C.v_multiwindow_appkit_side_effect_count()
			assert count <= native_appkit_side_effect_capacity
			for index in u64(0) .. count {
				mut raw := C.VMultiwindowAppKitSideEffectRecord{}
				assert C.v_multiwindow_appkit_side_effect_record(index, &raw) == 1
				assert raw.kind > 0
				assert raw.kind <= u64(int(NativeAppKitSideEffectKind.layer_device_set_read))
				assert raw.subject > 0
				assert raw.subject <= u64(int(NativeAppKitSideEffectSubject.layer))
				records << NativeAppKitSideEffectRecord{
					generation:         raw.generation
					sequence:           raw.sequence
					kind:               unsafe { NativeAppKitSideEffectKind(int(raw.kind)) }
					subject:            unsafe { NativeAppKitSideEffectSubject(int(raw.subject)) }
					identity:           raw.identity
					parent_identity:    raw.parent_identity
					before_identity:    raw.before_identity
					after_identity:     raw.after_identity
					auxiliary_identity: raw.auxiliary_identity
					thread_identity:    raw.thread_identity
					main_thread:        raw.main_thread == 1
				}
			}
			return NativeAppKitSideEffectSnapshot{
				generation: expected_generation
				enabled:    true
				overflow:   false
				records:    records
			}
		}
		return NativeAppKitSideEffectSnapshot{}
	}

	fn native_appkit_assert_side_effect_bijection_for_test(authority NativeAuthorityProofSnapshot, operations []NativeAppKitLifetimeOracleRecord, expected_generation u64, expected []NativeAppKitSideEffectRecord, side_effects NativeAppKitSideEffectSnapshot) {
		assert side_effects.enabled
		assert !side_effects.overflow
		assert expected_generation != 0
		assert side_effects.generation == expected_generation
		assert side_effects.records.len <= native_appkit_side_effect_capacity
		for index, record in side_effects.records {
			assert record.generation == expected_generation
			assert record.sequence == u64(index + 1)
			assert record.kind != .invalid
			assert record.subject != .none
			if record.kind != .next_drawable {
				assert record.identity != 0
			}
			assert record.thread_identity == authority.owner_thread_identity
			assert record.main_thread
		}
		assert side_effects.records.len == expected.len
		mut consumed := []bool{len: side_effects.records.len}
		for item in expected {
			assert item.generation == expected_generation
			assert item.sequence != 0
			assert item.kind != .invalid
			assert item.subject != .none
			assert item.thread_identity == authority.owner_thread_identity
			assert item.main_thread
			mut matched := -1
			for index, actual in side_effects.records {
				if consumed[index] || actual.generation != item.generation
					|| actual.sequence != item.sequence || actual.kind != item.kind
					|| actual.subject != item.subject || actual.identity != item.identity
					|| actual.parent_identity != item.parent_identity
					|| actual.before_identity != item.before_identity
					|| actual.after_identity != item.after_identity
					|| actual.auxiliary_identity != item.auxiliary_identity
					|| actual.thread_identity != item.thread_identity
					|| actual.main_thread != item.main_thread {
					continue
				}
				matched = index
				break
			}
			assert matched >= 0
			consumed[matched] = true
		}
		for matched in consumed {
			assert matched
		}
		native_appkit_assert_release_bijection_for_test(authority, operations)
	}

	fn native_appkit_create_release_probe_for_test(generation u64, subject NativeAppKitSideEffectSubject, created_after_operation_count int) !NativeAppKitExpectedReleaseProbe {
		$if darwin {
			assert generation != 0
			assert subject in [.device_root, .window_root, .anchor_root]
			assert created_after_operation_count >= 0
			probe := C.v_multiwindow_appkit_side_effect_create_release_probe(u64(int(subject)))
			if probe == unsafe { nil } {
				return error('AppKit side-effect release probe creation failed')
			}
			identity := native_identity(probe)
			assert identity != 0
			// Probe metadata is valid only while the retained probe is still alive.
			assert C.v_multiwindow_appkit_side_effect_probe_generation(probe) == generation
			assert C.v_multiwindow_appkit_side_effect_probe_subject(probe) == u64(int(subject))
			return NativeAppKitExpectedReleaseProbe{
				generation:                    generation
				identity:                      identity
				subject:                       subject
				created_after_operation_count: created_after_operation_count
			}
		}
		return error('AppKit side-effect release probes require Darwin')
	}

	fn native_appkit_assert_side_effect_snapshots_equal_for_test(expected NativeAppKitSideEffectSnapshot, actual NativeAppKitSideEffectSnapshot) {
		assert actual.generation == expected.generation
		assert actual.enabled == expected.enabled
		assert actual.overflow == expected.overflow
		assert actual.records == expected.records
	}

	fn native_appkit_expect_side_effect_for_test(records []NativeAppKitSideEffectRecord, index int, generation u64, kind NativeAppKitSideEffectKind, subject NativeAppKitSideEffectSubject, identity u64, parent_identity u64, before_identity u64, after_identity u64, auxiliary_identity u64) int {
		assert index >= 0
		assert index < records.len
		record := records[index]
		assert record.generation == generation
		assert record.sequence == u64(index + 1)
		assert record.kind == kind
		assert record.subject == subject
		assert record.identity == identity
		assert record.parent_identity == parent_identity
		assert record.before_identity == before_identity
		assert record.after_identity == after_identity
		assert record.auxiliary_identity == auxiliary_identity
		return index + 1
	}

	fn native_appkit_assert_side_effect_operation_bijection_for_test(authority NativeAuthorityProofSnapshot, operations []NativeAppKitLifetimeOracleRecord, generation u64, probes []NativeAppKitExpectedReleaseProbe, require_authority_trace bool) NativeAppKitSideEffectSnapshot {
		side_effects := native_appkit_side_effect_snapshot_for_test(generation)
		assert side_effects.enabled
		assert !side_effects.overflow
		assert side_effects.generation == generation
		for index, record in side_effects.records {
			assert record.generation == generation
			assert record.sequence == u64(index + 1)
			assert record.kind != .invalid
			assert record.subject != .none
			if record.kind != .next_drawable {
				assert record.identity != 0
			}
			assert record.thread_identity == authority.owner_thread_identity
			assert record.main_thread
		}
		mut roots := map[u64]NativeAppKitSideEffectSubject{}
		mut layers := map[u64]u64{}
		mut pool_probes := map[u64]u64{}
		mut released_probes := map[u64]bool{}
		mut side_index := 0
		for operation_index := 0; operation_index <= operations.len; operation_index++ {
			for probe in probes {
				if probe.created_after_operation_count != operation_index {
					continue
				}
				assert probe.generation == generation
				assert probe.identity != 0
				assert probe.identity !in roots
				side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
					side_index, generation, .bridge_retain, probe.subject, probe.identity, 0, 0,
					probe.identity, 1)
				roots[probe.identity] = probe.subject
			}
			if operation_index == operations.len {
				break
			}
			operation := operations[operation_index]
			assert operation.sequence == u64(operation_index + 1)
			assert operation.thread_identity == authority.owner_thread_identity
			match operation.kind {
				1 {
					if operation.output_identity != 0 {
						identity := operation.output_identity
						assert identity !in roots
						side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
							side_index, generation, .bridge_retain, .device_root, identity, 0, 0,
							identity, 0)
						roots[identity] = .device_root
					}
				}
				2, 6 {
					identity := operation.identity
					assert identity != 0
					assert identity in roots
					subject := roots[identity]
					mut is_probe := false
					for probe in probes {
						if probe.identity == identity {
							assert probe.subject == subject
							assert identity !in released_probes
							side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
								side_index, generation, .release_probe_dealloc, subject, identity,
								0, identity, 0, 0)
							released_probes[identity] = true
							is_probe = true
							break
						}
					}
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .bridge_release, subject, identity, 0, identity, 0, 0)
					roots.delete(identity)
					if is_probe {
						assert released_probes[identity]
					}
				}
				3, 7 {
					identity := operation.output_identity
					if identity == 0 {
						continue
					}
					subject := if operation.kind == 3 {
						NativeAppKitSideEffectSubject.window_root
					} else {
						NativeAppKitSideEffectSubject.anchor_root
					}
					if operation.identity != 0 {
						assert side_index < side_effects.records.len
						layer := side_effects.records[side_index]
						assert layer.kind == .layer_device_set_read
						assert layer.subject == .layer
						assert layer.identity != 0
						assert layer.parent_identity == identity
						assert layer.before_identity == 0
						assert layer.after_identity == operation.identity
						assert layer.auxiliary_identity == operation.identity
						assert identity !in layers
						layers[identity] = layer.identity
						side_index++
					}
					assert identity !in roots
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .bridge_retain, subject, identity, 0, 0, identity, 0)
					roots[identity] = subject
				}
				4 {
					state_identity := operation.identity
					device_identity := operation.parent_identity
					layer_identity := operation.auxiliary_identity_2
					assert state_identity != 0
					assert device_identity != 0
					assert layer_identity != 0
					expected_before := if state_identity in layers {
						device_identity
					} else {
						u64(0)
					}
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .layer_device_set_read, .layer, layer_identity,
						state_identity, expected_before, device_identity, device_identity)
					layers[state_identity] = layer_identity
				}
				9 {
					state_identity := operation.identity
					assert state_identity != 0
					assert state_identity in layers
					drawable_identity := operation.output_identity
					mut observed_layer := layers[state_identity]
					if drawable_identity == 0 {
						assert operation.valid_mask & native_valid_handle != 0
						assert side_index < side_effects.records.len
						physical := side_effects.records[side_index]
						assert physical.kind == .next_drawable
						assert physical.subject == .layer
						assert physical.identity != 0
						assert physical.identity != observed_layer
						assert physical.parent_identity == state_identity
						assert physical.before_identity == 0
						assert physical.after_identity == 0
						assert physical.auxiliary_identity == physical.identity
						observed_layer = physical.identity
						side_index++
					}
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .next_drawable, .drawable, drawable_identity,
						state_identity, 0, drawable_identity, observed_layer)
				}
				10, 11, 12 {
					if operation.valid_mask & native_valid_object_identity_0 != 0 {
						assert operation.identity != 0
						assert operation.parent_identity != 0
						side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
							side_index, generation, .current_drawable_clear, .drawable,
							operation.identity, operation.parent_identity, operation.identity, 0, 0)
					}
				}
				13 {
					pool_identity := operation.output_identity
					assert pool_identity != 0
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .pool_push, .pool, pool_identity, 0, 0,
						pool_identity, 0)
					assert side_index < side_effects.records.len
					probe := side_effects.records[side_index]
					assert probe.kind == .pool_probe_create
					assert probe.subject == .pool
					assert probe.identity != 0
					assert probe.parent_identity == pool_identity
					assert probe.before_identity == 0
					assert probe.after_identity == probe.identity
					assert probe.auxiliary_identity == 0
					assert pool_identity !in pool_probes
					pool_probes[pool_identity] = probe.identity
					side_index++
				}
				14 {
					pool_identity := operation.identity
					assert pool_identity != 0
					assert pool_identity in pool_probes
					probe_identity := pool_probes[pool_identity]
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .pool_probe_dealloc, .pool, probe_identity,
						pool_identity, probe_identity, 0, 0)
					side_index = native_appkit_expect_side_effect_for_test(side_effects.records,
						side_index, generation, .pool_pop, .pool, pool_identity, 0, pool_identity,
						0, 0)
					pool_probes.delete(pool_identity)
				}
				else {}
			}
		}
		assert side_index == side_effects.records.len
		assert pool_probes.len == 0
		assert roots.len == 0
		assert released_probes.len == probes.len
		for probe in probes {
			assert released_probes[probe.identity]
		}
		if require_authority_trace {
			assert !authority.trace_overflow
			native_appkit_assert_release_bijection_for_test(authority, operations)
		} else {
			assert authority.trace_overflow
		}
		return side_effects
	}

	fn native_appkit_layer_identity_for_state_for_test(generation u64, state_identity u64, device_identity u64) u64 {
		assert generation != 0
		assert state_identity != 0
		assert device_identity != 0
		snapshot := native_appkit_side_effect_snapshot_for_test(generation)
		mut layer_identity := u64(0)
		mut matches := 0
		for record in snapshot.records {
			if record.kind != .layer_device_set_read || record.subject != .layer
				|| record.parent_identity != state_identity {
				continue
			}
			matches++
			assert record.identity != 0
			assert record.before_identity == 0
			assert record.after_identity == device_identity
			assert record.auxiliary_identity == device_identity
			layer_identity = record.identity
		}
		assert matches == 1
		return layer_identity
	}

	fn native_appkit_assert_physical_nil_side_effect_delta_for_test(before NativeAppKitSideEffectSnapshot, after NativeAppKitSideEffectSnapshot, generation u64, state_identity u64, original_layer_identity u64, owner_thread u64) u64 {
		assert before.enabled
		assert after.enabled
		assert !before.overflow
		assert !after.overflow
		assert before.generation == generation
		assert after.generation == generation
		assert after.records.len == before.records.len + 2
		for index in 0 .. before.records.len {
			assert after.records[index] == before.records[index]
		}
		physical := after.records[before.records.len]
		result := after.records[before.records.len + 1]
		assert physical.sequence == u64(before.records.len + 1)
		assert physical.kind == .next_drawable
		assert physical.subject == .layer
		assert physical.identity != 0
		assert physical.identity != original_layer_identity
		assert physical.parent_identity == state_identity
		assert physical.before_identity == 0
		assert physical.after_identity == 0
		assert physical.auxiliary_identity == physical.identity
		assert physical.thread_identity == owner_thread
		assert physical.main_thread
		assert result.sequence == u64(before.records.len + 2)
		assert result.kind == .next_drawable
		assert result.subject == .drawable
		assert result.identity == 0
		assert result.parent_identity == state_identity
		assert result.before_identity == 0
		assert result.after_identity == 0
		assert result.auxiliary_identity == physical.identity
		assert result.thread_identity == owner_thread
		assert result.main_thread
		return physical.identity
	}

	fn native_appkit_assert_physical_nil_oracle_delta_for_test(before []NativeAppKitLifetimeOracleRecord, after []NativeAppKitLifetimeOracleRecord, state_identity u64, device_identity u64, owner_thread u64) {
		assert after.len == before.len + 1
		for index in 0 .. before.len {
			assert after[index] == before[index]
		}
		record := after[before.len]
		assert record.sequence == u64(before.len + 1)
		assert record.kind == 9
		assert record.identity == state_identity
		assert record.parent_identity == device_identity
		assert record.output_identity == 0
		assert record.auxiliary_identity == 0
		assert record.auxiliary_identity_1 == 0
		assert record.auxiliary_identity_2 == 0
		assert record.valid_mask == (native_valid_handle | native_valid_object_identity_0 | native_valid_observed_count | native_valid_selected_value)
		assert record.thread_identity == owner_thread
	}

	fn native_appkit_assert_restored_retry_oracle_delta_for_test(before []NativeAppKitLifetimeOracleRecord, after []NativeAppKitLifetimeOracleRecord, state_identity u64, device_identity u64, drawable_identity u64, owner_thread u64) {
		assert drawable_identity != 0
		assert after.len == before.len + 2
		for index in 0 .. before.len {
			assert after[index] == before[index]
		}
		acquire := after[before.len]
		release := after[before.len + 1]
		assert acquire.sequence == u64(before.len + 1)
		assert acquire.kind == 9
		assert acquire.identity == state_identity
		assert acquire.parent_identity == device_identity
		assert acquire.output_identity == drawable_identity
		assert acquire.auxiliary_identity != 0
		assert acquire.valid_mask == (native_valid_handle | native_valid_object_identity_0 | native_valid_observed_count | native_valid_selected_value)
		assert acquire.thread_identity == owner_thread
		assert release.sequence == u64(before.len + 2)
		assert release.kind == 11
		assert release.identity == drawable_identity
		assert release.parent_identity == state_identity
		assert release.output_identity == 0
		assert release.auxiliary_identity == drawable_identity
		assert release.valid_mask == native_valid_object_identity_0
		assert release.thread_identity == owner_thread
	}

	fn native_appkit_assert_trace_prefix_equal_for_test(before NativeAuthorityProofSnapshot, after NativeAuthorityProofSnapshot) {
		assert after.trace_len >= before.trace_len
		for index in 0 .. before.trace_len {
			expected := before.trace[index]
			actual := after.trace[index]
			assert actual.milestone == expected.milestone
			assert native_operation_contexts_identical(actual.context, expected.context)
			assert native_proof_evidence_equal(actual.actual, expected.actual)
			assert native_proof_evidence_equal(actual.effective, expected.effective)
			assert actual.local_validation == expected.local_validation
			assert native_proof_result_equal(actual.result, expected.result)
			assert actual.health == expected.health
		}
	}

	fn native_appkit_assert_physical_nil_authority_delta_for_test(before NativeAuthorityProofSnapshot, after NativeAuthorityProofSnapshot, context NativeOperationContext) {
		assert before.proof_armed
		assert after.proof_armed
		assert context.ordinal == before.next_ordinal
		assert before.next_ordinal < (~u64(0) - 1)
		assert after.next_ordinal == before.next_ordinal + 2
		assert after.app_identity == before.app_identity
		assert after.app_lifetime_token == before.app_lifetime_token
		assert after.renderer_attempt_token == before.renderer_attempt_token
		assert after.owner_thread_identity == before.owner_thread_identity
		assert after.next_proof_generation == before.next_proof_generation
		assert after.sequence_exhausted == before.sequence_exhausted
		assert after.terminal_cause == before.terminal_cause
		assert after.proof_generation == before.proof_generation
		assert after.proof_ordinal_floor == before.proof_ordinal_floor
		assert after.proof_accepting_plans == before.proof_accepting_plans
		assert !after.trace_overflow
		native_proof_assert_plan_equal(before.plan, after.plan)
		native_lifetime_registry_assert_snapshots_equal(before.registry, after.registry)
		native_appkit_assert_trace_prefix_equal_for_test(before, after)
		assert after.trace_len == before.trace_len + 5
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for offset, milestone in expected_milestones {
			entry := after.trace[before.trace_len + offset]
			assert entry.milestone == milestone
			assert native_operation_contexts_identical(entry.context, context)
			assert entry.context.ordinal != context.ordinal + 1
		}
		actual := after.trace[before.trace_len + 1]
		effective := after.trace[before.trace_len + 2]
		assert actual.actual.valid_mask == (native_valid_handle | native_valid_object_identity_0 | native_valid_observed_count | native_valid_selected_value)
		assert actual.actual.handle == 0
		assert actual.actual.object_identity_0 == 0
		assert actual.actual.observed_count == 0
		assert actual.actual.selected_value == 0
		assert native_proof_evidence_equal(actual.actual, effective.effective)
		accepted := after.trace[before.trace_len + 3]
		assert accepted.local_validation == .null_output
		assert accepted.result.domain == .metal
		assert accepted.result.operation == .drawable_acquire
		assert accepted.result.scope == context.scope
		assert accepted.result.disposition == .transient
		assert native_operation_contexts_identical(accepted.result.context, context)
		assert native_proof_evidence_equal(accepted.result.actual_primitive, actual.actual)
		assert native_proof_evidence_equal(accepted.result.primitive, actual.actual)
		latched := after.trace[before.trace_len + 4]
		assert latched.health == .ready
		for index in 0 .. after.trace_len {
			entry := after.trace[index]
			assert entry.context.ordinal != context.ordinal + 1
			if index >= before.trace_len {
				assert entry.milestone != .authority_release
			}
		}
		native_proof_assert_invalid_trace_tail(after)
	}

	fn native_appkit_assert_restored_layer_retry_for_test(before NativeAppKitSideEffectSnapshot, after NativeAppKitSideEffectSnapshot, generation u64, state_identity u64, original_layer_identity u64, owner_thread u64) u64 {
		assert before.generation == generation
		assert after.generation == generation
		assert after.records.len > before.records.len
		for index in 0 .. before.records.len {
			assert after.records[index] == before.records[index]
		}
		mut drawable_identity := u64(0)
		mut drawable_index := -1
		mut clear_index := -1
		mut physical_calls := 0
		for index in before.records.len .. after.records.len {
			record := after.records[index]
			assert record.thread_identity == owner_thread
			assert record.main_thread
			if record.kind == .next_drawable && record.subject == .layer
				&& record.parent_identity == state_identity {
				physical_calls++
			}
			if record.kind == .next_drawable && record.subject == .drawable
				&& record.parent_identity == state_identity {
				assert drawable_identity == 0
				assert record.identity != 0
				assert record.before_identity == 0
				assert record.after_identity == record.identity
				assert record.auxiliary_identity == original_layer_identity
				drawable_identity = record.identity
				drawable_index = index
			}
			if record.kind == .current_drawable_clear && record.subject == .drawable
				&& record.parent_identity == state_identity {
				assert clear_index < 0
				assert drawable_identity != 0
				assert record.identity == drawable_identity
				assert record.before_identity == drawable_identity
				assert record.after_identity == 0
				assert record.auxiliary_identity == 0
				clear_index = index
			}
		}
		assert physical_calls == 0
		assert drawable_identity != 0
		assert drawable_index >= 0
		assert clear_index > drawable_index
		return drawable_identity
	}

	fn native_win32_backend_replay_snapshot_for_test(app &App) NativeWin32BackendReplaySnapshot {
		mut windows := []NativeWin32WindowReplaySnapshot{}
		$if windows {
			backend := &app.backend.win32
			for record_pointer in backend.windows {
				record := record_pointer
				windows << NativeWin32WindowReplaySnapshot{
					id:                        record.id
					hwnd:                      native_identity(record.hwnd)
					config:                    record.config
					width:                     record.width
					height:                    record.height
					framebuffer_width:         record.framebuffer_width
					framebuffer_height:        record.framebuffer_height
					destroyed:                 record.destroyed
					render_resize_pending:     record.render_resize_pending
					suppress_resize_event:     record.suppress_resize_event
					queued_events:             record.queued_events.clone()
					mouse_x:                   record.mouse_x
					mouse_y:                   record.mouse_y
					mouse_dx:                  record.mouse_dx
					mouse_dy:                  record.mouse_dy
					mouse_pos_valid:           record.mouse_pos_valid
					iconified:                 record.iconified
					pending_dropped_files:     record.pending_dropped_files.clone()
					pending_drop_modifiers:    record.pending_drop_modifiers
					pending_high_surrogate:    record.pending_high_surrogate
					suppress_control_char:     record.suppress_control_char
					swapchain:                 native_identity(record.swapchain)
					swapchain_ticket:          record.swapchain_ticket
					pending_backbuffer:        native_identity(record.pending_backbuffer)
					pending_backbuffer_ticket: record.pending_backbuffer_ticket
					render_view:               native_identity(record.render_view)
					render_view_ticket:        record.render_view_ticket
					depth_texture:             native_identity(record.depth_texture)
					depth_texture_ticket:      record.depth_texture_ticket
					depth_stencil_view:        native_identity(record.depth_stencil_view)
					depth_stencil_view_ticket: record.depth_stencil_view_ticket
					render_target_generation:  record.render_target_generation
				}
			}
			return NativeWin32BackendReplaySnapshot{
				app_status:                       app.status()
				stop_terminal:                    app.stop_terminal
				started:                          backend.started
				device:                           native_identity(backend.device)
				device_ticket:                    backend.device_ticket
				device_context:                   native_identity(backend.device_context)
				device_context_ticket:            backend.device_context_ticket
				factory:                          native_identity(backend.factory)
				factory_ticket:                   backend.factory_ticket
				pending_init_dxgi_device:         native_identity(backend.pending_init_dxgi_device)
				pending_init_dxgi_device_ticket:  backend.pending_init_dxgi_device_ticket
				pending_init_adapter:             native_identity(backend.pending_init_adapter)
				pending_init_adapter_ticket:      backend.pending_init_adapter_ticket
				using_warp:                       backend.using_warp
				anchor_color_texture:             native_identity(backend.anchor_color_texture)
				anchor_color_texture_ticket:      backend.anchor_color_texture_ticket
				anchor_render_view:               native_identity(backend.anchor_render_view)
				anchor_render_view_ticket:        backend.anchor_render_view_ticket
				anchor_depth_texture:             native_identity(backend.anchor_depth_texture)
				anchor_depth_texture_ticket:      backend.anchor_depth_texture_ticket
				anchor_depth_stencil_view:        native_identity(backend.anchor_depth_stencil_view)
				anchor_depth_stencil_view_ticket: backend.anchor_depth_stencil_view_ticket
				anchor_committed:                 backend.anchor_committed
				render_sequence:                  backend.render_sequence
				render_health:                    backend.render_health
				poll_error:                       backend.poll_error
				event_sequence_terminal:          backend.event_sequence_terminal
				windows:                          windows
				authority:                        native_proof_snapshot(&app.backend.native_operations)
			}
		}
		return NativeWin32BackendReplaySnapshot{}
	}

	fn native_appkit_backend_replay_snapshot_for_test(app &App) NativeAppKitBackendReplaySnapshot {
		mut windows := []NativeAppKitWindowReplaySnapshot{}
		$if darwin {
			backend := &app.backend.appkit
			for record in backend.windows {
				windows << NativeAppKitWindowReplaySnapshot{
					id:                       record.id
					state:                    native_identity(record.state)
					state_ticket:             record.state_ticket
					width:                    record.width
					height:                   record.height
					framebuffer_width:        record.framebuffer_width
					framebuffer_height:       record.framebuffer_height
					native_destroyed:         record.native_destroyed
					render_target_generation: record.render_target_generation
					next_frame_lease:         record.next_frame_lease
					active_frame_lease:       record.active_frame_lease
					active_drawable:          native_identity(record.active_drawable)
					active_drawable_ticket:   record.active_drawable_ticket
					frame_active:             record.frame_active
				}
			}
			return NativeAppKitBackendReplaySnapshot{
				app_status:                    app.status()
				stop_terminal:                 app.stop_terminal
				started:                       backend.started
				device:                        native_identity(backend.device)
				device_ticket:                 backend.device_ticket
				anchor_state:                  native_identity(backend.anchor_state)
				anchor_state_ticket:           backend.anchor_state_ticket
				batch_autorelease_pool:        native_identity(backend.batch_autorelease_pool)
				batch_autorelease_pool_ticket: backend.batch_autorelease_pool_ticket
				next_anchor_lease:             backend.next_anchor_lease
				active_anchor_lease:           backend.active_anchor_lease
				active_anchor_drawable:        native_identity(backend.active_anchor_drawable)
				active_anchor_drawable_ticket: backend.active_anchor_drawable_ticket
				render_sequence:               backend.render_sequence
				poll_error:                    backend.poll_error
				event_sequence_terminal:       backend.event_sequence_terminal
				render_health:                 backend.render_health
				windows:                       windows
				authority:                     native_proof_snapshot(&app.backend.native_operations)
			}
		}
		return NativeAppKitBackendReplaySnapshot{}
	}

	fn native_win32_backend_assert_replay_equal_for_test(expected NativeWin32BackendReplaySnapshot, actual NativeWin32BackendReplaySnapshot) {
		assert actual.app_status == expected.app_status
		assert actual.stop_terminal == expected.stop_terminal
		assert actual.started == expected.started
		assert actual.device == expected.device
		assert actual.device_ticket == expected.device_ticket
		assert actual.device_context == expected.device_context
		assert actual.device_context_ticket == expected.device_context_ticket
		assert actual.factory == expected.factory
		assert actual.factory_ticket == expected.factory_ticket
		assert actual.pending_init_dxgi_device == expected.pending_init_dxgi_device
		assert actual.pending_init_dxgi_device_ticket == expected.pending_init_dxgi_device_ticket
		assert actual.pending_init_adapter == expected.pending_init_adapter
		assert actual.pending_init_adapter_ticket == expected.pending_init_adapter_ticket
		assert actual.using_warp == expected.using_warp
		assert actual.anchor_color_texture == expected.anchor_color_texture
		assert actual.anchor_color_texture_ticket == expected.anchor_color_texture_ticket
		assert actual.anchor_render_view == expected.anchor_render_view
		assert actual.anchor_render_view_ticket == expected.anchor_render_view_ticket
		assert actual.anchor_depth_texture == expected.anchor_depth_texture
		assert actual.anchor_depth_texture_ticket == expected.anchor_depth_texture_ticket
		assert actual.anchor_depth_stencil_view == expected.anchor_depth_stencil_view
		assert actual.anchor_depth_stencil_view_ticket == expected.anchor_depth_stencil_view_ticket
		assert actual.anchor_committed == expected.anchor_committed
		assert actual.render_sequence == expected.render_sequence
		assert actual.render_health == expected.render_health
		assert actual.poll_error == expected.poll_error
		assert actual.event_sequence_terminal == expected.event_sequence_terminal
		assert actual.windows == expected.windows
		native_proof_assert_snapshots_equal(expected.authority, actual.authority)
	}

	fn native_appkit_backend_assert_replay_equal_for_test(expected NativeAppKitBackendReplaySnapshot, actual NativeAppKitBackendReplaySnapshot) {
		assert actual.app_status == expected.app_status
		assert actual.stop_terminal == expected.stop_terminal
		assert actual.started == expected.started
		assert actual.device == expected.device
		assert actual.device_ticket == expected.device_ticket
		assert actual.anchor_state == expected.anchor_state
		assert actual.anchor_state_ticket == expected.anchor_state_ticket
		assert actual.batch_autorelease_pool == expected.batch_autorelease_pool
		assert actual.batch_autorelease_pool_ticket == expected.batch_autorelease_pool_ticket
		assert actual.next_anchor_lease == expected.next_anchor_lease
		assert actual.active_anchor_lease == expected.active_anchor_lease
		assert actual.active_anchor_drawable == expected.active_anchor_drawable
		assert actual.active_anchor_drawable_ticket == expected.active_anchor_drawable_ticket
		assert actual.render_sequence == expected.render_sequence
		assert actual.poll_error == expected.poll_error
		assert actual.event_sequence_terminal == expected.event_sequence_terminal
		assert actual.render_health == expected.render_health
		assert actual.windows == expected.windows
		native_proof_assert_snapshots_equal(expected.authority, actual.authority)
	}

	fn native_phase_b_assert_bound_ticket_for_test(authority &NativeOperationAuthority, ticket_id u64, release_kind NativeLifetimeReleaseKind, identity u64, parent_identity u64, expected_scope NativeOperationAuthorityScope) NativeLifetimeTicketProofSnapshot {
		ticket := native_lifetime_ticket_snapshot_for_test(authority, ticket_id) or { panic(err) }
		assert ticket.ticket_id == ticket.context.ordinal
		assert ticket.app_identity == authority.app_identity
		assert ticket.release_kind == release_kind
		assert ticket.domain == native_lifetime_release_domain(release_kind)
		assert ticket.context.domain == ticket.domain
		assert ticket.context.operation == native_lifetime_release_operation(release_kind)
		assert ticket.context.app_identity == authority.app_identity
		assert ticket.authority_scope == expected_scope
		assert ticket.context.authority_scope == expected_scope
		assert ticket.authority_token == ticket.context.authority_token
		assert ticket.authority_token == if expected_scope == .app_lifetime {
			authority.app_lifetime_token
		} else {
			authority.renderer_attempt_token
		}
		assert ticket.context.renderer_attempt_token == if expected_scope == .renderer_attempt {
			authority.renderer_attempt_token
		} else {
			u64(0)
		}
		assert ticket.native_identity == identity
		assert ticket.context.target_identity == identity
		assert ticket.context.presence_mask == (ticket.owner_seed.presence_mask | native_context_has_target_identity)
		assert ticket.context.call_site == ticket.owner_seed.call_site
		assert ticket.context.scope == ticket.owner_seed.scope
		assert ticket.context.window == ticket.owner_seed.window
		assert ticket.context.target_generation == ticket.owner_seed.target_generation
		assert ticket.context.batch_epoch == ticket.owner_seed.batch_epoch
		assert ticket.context.window_lease_epoch == ticket.owner_seed.window_lease_epoch
		assert ticket.context.target_lease_epoch == ticket.owner_seed.target_lease_epoch
		assert ticket.required_parent_identity == parent_identity
		assert ticket.state == .bound
		if parent_identity == 0 {
			assert ticket.parent_authority_scope == .none
			assert ticket.parent_authority_token == 0
		} else {
			mut parent_matches := 0
			for parent in authority.lifetime_tickets {
				if parent.native_identity != parent_identity || parent.state != .bound {
					continue
				}
				parent_matches++
				assert ticket.parent_authority_scope == parent.authority_scope
				assert ticket.parent_authority_token == parent.authority_token
				assert parent.app_identity == ticket.app_identity
			}
			assert parent_matches == 1
		}
		return ticket
	}

	fn native_appkit_assert_pool_ticket_context_for_test(authority &NativeOperationAuthority, ticket_id u64, pool_identity u64, batch RenderBatchLease) NativeLifetimeTicketProofSnapshot {
		ticket := native_phase_b_assert_bound_ticket_for_test(authority, ticket_id,
			.appkit_autorelease_pool, pool_identity, 0, .renderer_attempt)
		assert batch.app_instance == authority.app_identity
		assert batch.epoch != 0
		assert ticket.owner_seed.presence_mask == native_context_has_batch_epoch
		assert ticket.owner_seed.call_site == .anchor_prepare
		assert ticket.owner_seed.scope == .batch
		assert ticket.owner_seed.window == WindowId{}
		assert ticket.owner_seed.target_generation == 0
		assert ticket.owner_seed.target_identity == 0
		assert ticket.owner_seed.batch_epoch == batch.epoch
		assert ticket.owner_seed.window_lease_epoch == 0
		assert ticket.owner_seed.target_lease_epoch == 0
		expected := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        authority.renderer_attempt_token
			renderer_attempt_token: authority.renderer_attempt_token
			app_identity:           authority.app_identity
			presence_mask:          native_context_has_batch_epoch | native_context_has_target_identity
			domain:                 .metal
			operation:              .render_batch_end
			call_site:              .anchor_prepare
			scope:                  .batch
			target_identity:        pool_identity
			batch_epoch:            batch.epoch
			ordinal:                ticket.ticket_id
		}
		assert native_operation_contexts_identical(ticket.context, expected)
		return ticket
	}

	fn native_appkit_assert_window_drawable_ticket_context_for_test(authority &NativeOperationAuthority, ticket_id u64, drawable_identity u64, parent_state_identity u64, target_generation u64, batch RenderBatchLease, target RenderTargetLease) NativeLifetimeTicketProofSnapshot {
		ticket := native_phase_b_assert_bound_ticket_for_test(authority, ticket_id,
			.metal_drawable, drawable_identity, parent_state_identity, .renderer_attempt)
		assert batch.app_instance == authority.app_identity
		assert target.app_instance == authority.app_identity
		assert target.batch_epoch == batch.epoch
		assert target.window == ticket.owner_seed.window
		assert target.window_epoch != 0
		assert target.target_epoch != 0
		assert target_generation != 0
		assert ticket.owner_seed.presence_mask == native_context_window_target_fields
		assert ticket.owner_seed.call_site == .window_activate
		assert ticket.owner_seed.scope == .window_target
		assert ticket.owner_seed.target_generation == target_generation
		assert ticket.owner_seed.target_identity == 0
		assert ticket.owner_seed.batch_epoch == batch.epoch
		assert ticket.owner_seed.window_lease_epoch == target.window_epoch
		assert ticket.owner_seed.target_lease_epoch == target.target_epoch
		expected := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        authority.renderer_attempt_token
			renderer_attempt_token: authority.renderer_attempt_token
			app_identity:           authority.app_identity
			presence_mask:          native_context_window_target_fields | native_context_has_target_identity
			domain:                 .metal
			operation:              .clear_state
			call_site:              .window_activate
			scope:                  .window_target
			window:                 target.window
			target_generation:      target_generation
			target_identity:        drawable_identity
			batch_epoch:            batch.epoch
			window_lease_epoch:     target.window_epoch
			target_lease_epoch:     target.target_epoch
			ordinal:                ticket.ticket_id
		}
		assert native_operation_contexts_identical(ticket.context, expected)
		return ticket
	}

	fn native_appkit_assert_anchor_drawable_ticket_context_for_test(authority &NativeOperationAuthority, ticket NativeLifetimeTicketProofSnapshot, drawable_identity u64, anchor_state_identity u64) {
		observed := native_phase_b_assert_bound_ticket_for_test(authority, ticket.ticket_id,
			.metal_drawable, drawable_identity, anchor_state_identity, .renderer_attempt)
		native_lifetime_ticket_assert_equal(ticket, observed)
		assert ticket.authority_scope == .renderer_attempt
		assert ticket.authority_token == authority.renderer_attempt_token
		assert ticket.parent_authority_scope == .renderer_attempt
		assert ticket.parent_authority_token == authority.renderer_attempt_token
		assert ticket.owner_seed.presence_mask == 0
		assert ticket.owner_seed.call_site == .anchor_prepare
		assert ticket.owner_seed.scope == .anchor
		assert ticket.owner_seed.window == WindowId{}
		assert ticket.owner_seed.target_generation == 0
		assert ticket.owner_seed.target_identity == 0
		assert ticket.owner_seed.batch_epoch == 0
		assert ticket.owner_seed.window_lease_epoch == 0
		assert ticket.owner_seed.target_lease_epoch == 0
		expected := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        authority.renderer_attempt_token
			renderer_attempt_token: authority.renderer_attempt_token
			app_identity:           authority.app_identity
			presence_mask:          native_context_has_target_identity
			domain:                 .metal
			operation:              .clear_state
			call_site:              .anchor_prepare
			scope:                  .anchor
			target_identity:        drawable_identity
			ordinal:                ticket.ticket_id
		}
		assert native_operation_contexts_identical(ticket.context, expected)
	}

	fn native_win32_release_oracle_indices_for_identity(records []NativeWin32LifetimeOracleRecord, identity u64) []int {
		mut indices := []int{}
		for index, record in records {
			if record.kind == 1 && record.identity == identity {
				indices << index
			}
		}
		return indices
	}

	fn native_win32_assert_release_bijection_for_test(snapshot NativeAuthorityProofSnapshot, records []NativeWin32LifetimeOracleRecord) {
		mut release_trace := []NativeOperationTraceEntry{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .authority_release && entry.context.domain == .dxgi
				&& entry.context.operation == .object_release {
				release_trace << entry
			}
		}
		mut release_records := []NativeWin32LifetimeOracleRecord{}
		for record in records {
			if record.kind == 1 {
				assert record.identity != 0
				assert record.valid_mask & native_valid_observed_count != 0
				assert record.thread_identity != 0
				release_records << record
			}
		}
		assert release_records.len == release_trace.len
		for index, entry in release_trace {
			assert entry.context.authority_scope in [.app_lifetime, .renderer_attempt]
			assert entry.context.authority_token != 0
			assert entry.context.app_identity == snapshot.app_identity
			assert entry.context.target_identity != 0
			assert entry.actual.valid_mask & native_valid_observed_count != 0
			record := release_records[index]
			assert record.identity == entry.context.target_identity
			assert record.observed_count == entry.actual.observed_count
			assert record.valid_mask & native_valid_observed_count != 0
			assert record.thread_identity == snapshot.owner_thread_identity
			assert native_lifetime_trace_entry_count_for_test(snapshot, entry.context) == 6
		}
	}

	fn native_appkit_release_kind_for_oracle(kind u64) bool {
		return kind in [u64(2), 6, 10, 11, 12, 14]
	}

	fn native_appkit_authority_release_kind_for_oracle(kind u64) bool {
		return native_appkit_release_kind_for_oracle(kind) || kind in [u64(5), 8]
	}

	fn native_appkit_assert_physical_capture_for_test(snapshot NativeAuthorityProofSnapshot, context NativeOperationContext, identity u64) {
		mut entries := []NativeOperationTraceEntry{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if native_operation_contexts_identical(entry.context, context) {
				entries << entry
			}
		}
		assert entries.len == 5
		expected := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for index, milestone in expected {
			assert entries[index].milestone == milestone
		}
		assert entries[1].actual.valid_mask & native_valid_object_identity_0 != 0
		assert entries[1].actual.object_identity_0 == identity
		assert entries[2].effective.valid_mask & native_valid_object_identity_0 != 0
		assert entries[2].effective.object_identity_0 == identity
		assert entries[3].result.succeeded()
		assert entries[3].local_validation == .void_completion
		assert native_operation_contexts_identical(entries[3].result.context, context)
	}

	fn native_appkit_assert_release_bijection_for_test(snapshot NativeAuthorityProofSnapshot, records []NativeAppKitLifetimeOracleRecord) {
		mut release_trace := []NativeOperationTraceEntry{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .authority_release && entry.context.domain == .metal {
				release_trace << entry
			}
		}
		mut release_records := []NativeAppKitLifetimeOracleRecord{}
		for record in records {
			if native_appkit_authority_release_kind_for_oracle(record.kind) {
				assert record.identity != 0
				assert record.thread_identity != 0
				if record.kind in [u64(5), 8] {
					assert record.parent_identity == 0
					assert record.output_identity == 0
					assert record.auxiliary_identity == 0
					assert record.auxiliary_identity_1 == 0
					assert record.auxiliary_identity_2 == 0
					assert record.valid_mask == 0
				} else {
					assert record.auxiliary_identity == record.identity
					assert record.valid_mask & native_valid_object_identity_0 != 0
				}
				release_records << record
			}
		}
		assert release_records.len == release_trace.len
		for index, entry in release_trace {
			assert entry.context.authority_scope in [.app_lifetime, .renderer_attempt]
			assert entry.context.authority_token != 0
			assert entry.context.app_identity == snapshot.app_identity
			assert entry.context.target_identity != 0
			record := release_records[index]
			assert record.identity == entry.context.target_identity
			assert record.thread_identity == snapshot.owner_thread_identity
			if record.kind in [u64(5), 8] {
				assert entry.context.operation == .surface_destroy
				assert entry.actual.valid_mask == 0
				assert entry.effective.valid_mask == 0
				assert native_operation_contexts_identical(entry.result.context, entry.context)
				assert native_lifetime_trace_entry_count_for_test(snapshot, entry.context) == 6
			} else if record.kind in [u64(10), 11] {
				assert record.auxiliary_identity == entry.context.target_identity
				assert record.valid_mask & native_valid_object_identity_0 != 0
				physical_context := entry.result.context
				assert physical_context.domain == .metal
				assert physical_context.operation == if record.kind == 10 {
					NativeRenderOperation.present
				} else {
					NativeRenderOperation.clear_state
				}
				assert physical_context.target_identity == record.identity
				assert physical_context.ordinal != entry.context.ordinal
				assert entry.context.operation == .clear_state
				assert native_lifetime_trace_entry_count_for_test(snapshot, entry.context) == 1
				native_appkit_assert_physical_capture_for_test(snapshot, physical_context,
					record.identity)
			} else {
				assert record.auxiliary_identity == entry.context.target_identity
				assert record.valid_mask & native_valid_object_identity_0 != 0
				assert native_operation_contexts_identical(entry.result.context, entry.context)
				assert native_lifetime_trace_entry_count_for_test(snapshot, entry.context) == 6
			}
		}
	}

	fn native_phase_b_assert_trace_tail_and_release_only_for_test(snapshot NativeAuthorityProofSnapshot, start int, domain NativeRenderDomain) {
		for index in start .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .real_call {
				assert entry.context.domain == domain
				assert entry.context.operation in [.object_release, .render_batch_end, .clear_state,
					.surface_destroy]
			}
		}
		native_proof_assert_invalid_trace_tail(snapshot)
	}

	fn native_appkit_runtime_requested_for_test() bool {
		if !native_runtime_proofs_requested_for_test() {
			return false
		}
		backend := native_runtime_backend_for_test() or { panic(err) }
		return backend == .appkit
	}

	fn native_phase_b_new_app_without_renderer_for_test(kind BackendKind) !&App {
		selected := native_runtime_backend_for_test()!
		if selected != kind {
			return error('native phase-B proof selected `${selected}`, expected `${kind}`')
		}
		native_require_parent_watchdog_gate_for_test()!
		mut app := new_app(
			backend:          kind
			queue_size:       16
			require_renderer: false
		)!
		caps := app.capabilities()
		if caps.backend != kind || !caps.native || !caps.multi_window {
			message := 'selected phase-B backend `${kind}` is unavailable: ${caps}'
			app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
			return error(message)
		}
		return app
	}

	fn native_win32_acquired_reference_identities_for_test(records []NativeWin32LifetimeOracleRecord) []u64 {
		mut identities := []u64{}
		for record in records {
			match record.kind {
				2 {
					if record.output_identity != 0 {
						identities << record.output_identity
					}
					if record.auxiliary_identity != 0 {
						identities << record.auxiliary_identity
					}
				}
				3, 4, 5, 6, 8, 9, 10, 11 {
					if record.output_identity != 0 {
						identities << record.output_identity
					}
				}
				else {}
			}
		}
		return identities
	}

	fn native_win32_assert_every_acquired_reference_released_for_test(records []NativeWin32LifetimeOracleRecord) {
		mut acquired := map[u64]int{}
		for identity in native_win32_acquired_reference_identities_for_test(records) {
			acquired[identity]++
		}
		mut released := map[u64]int{}
		for record in records {
			if record.kind == 1 {
				assert record.identity != 0
				assert record.valid_mask & native_valid_observed_count != 0
				released[record.identity]++
			}
		}
		assert acquired.len > 0
		assert released.len == acquired.len
		for identity, count in acquired {
			assert released[identity] == count
		}
	}

	fn native_win32_assert_transient_reference_ticket_for_test(snapshot NativeAuthorityProofSnapshot, records []NativeWin32LifetimeOracleRecord, acquire_kind u64, operation NativeRenderOperation, call_site NativeRenderCallSite, scope NativeRenderScope, expected_window WindowId, require_window_context bool) u64 {
		mut acquisitions := []NativeWin32LifetimeOracleRecord{}
		for record in records {
			if record.kind == acquire_kind && record.output_identity != 0 {
				acquisitions << record
			}
		}
		assert acquisitions.len == 1
		acquired := acquisitions[0]
		mut native_releases := 0
		for record in records {
			if record.kind == 1 && record.identity == acquired.output_identity {
				native_releases++
				assert record.valid_mask & native_valid_observed_count != 0
			}
		}
		assert native_releases == 1
		mut operation_calls := 0
		mut authority_releases := 0
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone == .real_call && entry.context.domain == .dxgi
				&& entry.context.operation == operation
				&& entry.context.target_identity == acquired.identity {
				operation_calls++
			}
			if entry.milestone != .authority_release || entry.context.domain != .dxgi
				|| entry.context.operation != .object_release
				|| entry.context.target_identity != acquired.output_identity {
				continue
			}
			authority_releases++
			assert entry.context.authority_scope == .renderer_attempt
			assert entry.context.authority_token == snapshot.renderer_attempt_token
			assert entry.context.renderer_attempt_token == snapshot.renderer_attempt_token
			assert entry.context.app_identity == snapshot.app_identity
			assert entry.context.call_site == call_site
			assert entry.context.scope == scope
			if require_window_context {
				assert entry.context.presence_mask & native_context_window_target_fields == native_context_window_target_fields
				assert entry.context.window == expected_window
				assert entry.context.target_generation != 0
				assert entry.context.batch_epoch != 0
				assert entry.context.window_lease_epoch != 0
				assert entry.context.target_lease_epoch != 0
			} else {
				assert entry.context.presence_mask & native_context_has_window == 0
				assert entry.context.window == WindowId{}
			}
			assert native_lifetime_trace_entry_count_for_test(snapshot, entry.context) == 6
		}
		assert operation_calls == 1
		assert authority_releases == 1
		return acquired.output_identity
	}

	fn native_win32_first_output_for_kind_for_test(records []NativeWin32LifetimeOracleRecord, kind u64, ordinal int) u64 {
		mut seen := 0
		for record in records {
			if record.kind == kind && record.output_identity != 0 {
				if seen == ordinal {
					return record.output_identity
				}
				seen++
			}
		}
		return 0
	}

	fn native_win32_release_index_for_test(records []NativeWin32LifetimeOracleRecord, identity u64, ordinal int) int {
		mut seen := 0
		for index, record in records {
			if record.kind == 1 && record.identity == identity {
				if seen == ordinal {
					return index
				}
				seen++
			}
		}
		return -1
	}

	fn native_win32_assert_only_release_after_for_test(records []NativeWin32LifetimeOracleRecord, start int) {
		for index in start .. records.len {
			assert records[index].kind == 1
		}
	}

	fn native_win32_exercise_phase_b_lifetimes() ! {
		mut startup := native_phase_b_new_app_without_renderer_for_test(.win32)!
		$if windows {
			$if sokol_d3d11 ? {
				mut transient_app := native_phase_b_new_app_without_renderer_for_test(.win32)!
				assert transient_app.backend.native_operations.arm_proof()
				native_win32_lifetime_oracle_reset_for_test()
				transient_app.start_renderer(RendererConfig{})!
				assert transient_app.backend.win32.using_warp
				transient_trace := native_proof_snapshot(&transient_app.backend.native_operations)
				transient_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert !transient_trace.trace_overflow
				_ = native_win32_assert_transient_reference_ticket_for_test(transient_trace,
					transient_oracle, 3, .device_query, .renderer_start, .renderer, WindowId{},
					false)
				_ = native_win32_assert_transient_reference_ticket_for_test(transient_trace,
					transient_oracle, 4, .adapter_acquire, .renderer_start, .renderer, WindowId{},
					false)
				transient_app.stop()!
				transient_first := native_win32_backend_replay_snapshot_for_test(transient_app)
				transient_first_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert transient_first.app_status == .stopped
				assert transient_first.authority.registry.tickets.len == 0
				native_win32_assert_every_acquired_reference_released_for_test(transient_first_oracle)
				transient_app.stop()!
				transient_second := native_win32_backend_replay_snapshot_for_test(transient_app)
				transient_second_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				native_win32_backend_assert_replay_equal_for_test(transient_first, transient_second)
				native_win32_oracle_assert_equal_for_test(transient_first_oracle,
					transient_second_oracle)
				assert transient_app.backend.native_operations.disarm_proof()

				native_win32_lifetime_oracle_reset_for_test()
				startup.start_renderer(RendererConfig{})!
				assert startup.backend.win32.using_warp
				live_window := native_runtime_new_window_for_test(mut startup,
					'phase-B Win32 live window COM lifetime')!
				_ = native_dxgi_render_healthy_target_for_test(mut startup, live_window)!
				authority := &startup.backend.native_operations
				backend := &startup.backend.win32
				live_window_index := startup.backend.win32.window_record_index(live_window) or {
					return error(err_window_not_found)
				}
				live_window_record := startup.backend.win32.windows[live_window_index]
				assert authority.lifetime_tickets.len == 11
				window_swapchain_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					live_window_record.swapchain_ticket, .com_reference,
					native_identity(live_window_record.swapchain), 0, .app_lifetime)
				window_view_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					live_window_record.render_view_ticket, .com_reference,
					native_identity(live_window_record.render_view), 0, .app_lifetime)
				window_depth_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					live_window_record.depth_texture_ticket, .com_reference,
					native_identity(live_window_record.depth_texture), 0, .app_lifetime)
				window_depth_view_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					live_window_record.depth_stencil_view_ticket, .com_reference,
					native_identity(live_window_record.depth_stencil_view), 0, .app_lifetime)
				device_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.device_ticket, .com_reference, native_identity(backend.device), 0,
					.app_lifetime)
				context_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.device_context_ticket, .com_reference,
					native_identity(backend.device_context), 0, .app_lifetime)
				factory_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.factory_ticket, .com_reference, native_identity(backend.factory), 0,
					.app_lifetime)
				anchor_color_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.anchor_color_texture_ticket, .com_reference,
					native_identity(backend.anchor_color_texture), 0, .renderer_attempt)
				anchor_view_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.anchor_render_view_ticket, .com_reference,
					native_identity(backend.anchor_render_view), 0, .renderer_attempt)
				anchor_depth_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.anchor_depth_texture_ticket, .com_reference,
					native_identity(backend.anchor_depth_texture), 0, .renderer_attempt)
				anchor_depth_view_lifetime_ticket := native_phase_b_assert_bound_ticket_for_test(authority,
					backend.anchor_depth_stencil_view_ticket, .com_reference,
					native_identity(backend.anchor_depth_stencil_view), 0, .renderer_attempt)
				anchor_depth_view_identity := native_identity(backend.anchor_depth_stencil_view)
				anchor_depth_identity := native_identity(backend.anchor_depth_texture)
				anchor_view_identity := native_identity(backend.anchor_render_view)
				anchor_color_identity := native_identity(backend.anchor_color_texture)
				factory_identity := native_identity(backend.factory)
				context_identity := native_identity(backend.device_context)
				device_identity := native_identity(backend.device)
				release_tickets := [window_depth_view_lifetime_ticket, window_depth_lifetime_ticket,
					window_view_lifetime_ticket, window_swapchain_lifetime_ticket,
					anchor_depth_view_lifetime_ticket, anchor_depth_lifetime_ticket,
					anchor_view_lifetime_ticket, anchor_color_lifetime_ticket,
					factory_lifetime_ticket, context_lifetime_ticket, device_lifetime_ticket]
				release_identities := [
					native_identity(live_window_record.depth_stencil_view),
					native_identity(live_window_record.depth_texture),
					native_identity(live_window_record.render_view),
					native_identity(live_window_record.swapchain),
					anchor_depth_view_identity,
					anchor_depth_identity,
					anchor_view_identity,
					anchor_color_identity,
					factory_identity,
					context_identity,
					device_identity,
				]
				assert release_tickets.len == 11
				assert release_identities.len == release_tickets.len
				for index, ticket in release_tickets {
					expected_call_site := if index < 4 {
						NativeRenderCallSite.window_prepare
					} else if index < 8 {
						NativeRenderCallSite.anchor_create
					} else {
						NativeRenderCallSite.renderer_start
					}
					expected_scope := if index < 4 {
						NativeRenderScope.window_target
					} else if index < 8 {
						NativeRenderScope.anchor
					} else {
						NativeRenderScope.renderer
					}
					assert ticket.context.call_site == expected_call_site
					assert ticket.context.scope == expected_scope
					assert ticket.native_identity == release_identities[index]
					assert ticket.context.target_identity == release_identities[index]
					assert ticket.context.ordinal == ticket.ticket_id
					for prior_index in 0 .. index {
						assert ticket.native_identity != release_tickets[prior_index].native_identity
						assert !native_operation_contexts_identical(ticket.context,
							release_tickets[prior_index].context)
					}
				}
				before_stop_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert startup.backend.native_operations.arm_proof()
				before_stop_trace := native_proof_snapshot(authority)
				startup.stop()!
				first := native_win32_backend_replay_snapshot_for_test(startup)
				first_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert startup.status() == .stopped
				assert !first.authority.trace_overflow
				assert first.authority.registry.tickets.len == 0
				assert first.windows.len == 0
				assert first.device == 0
				assert first.device_context == 0
				assert first.factory == 0
				assert first.anchor_color_texture == 0
				assert first.anchor_render_view == 0
				assert first.anchor_depth_texture == 0
				assert first.anchor_depth_stencil_view == 0
				stop_oracle := first_oracle[before_stop_oracle.len..]
				native_win32_assert_release_bijection_for_test(first.authority, stop_oracle)
				native_win32_assert_every_acquired_reference_released_for_test(first_oracle)
				for record in first_oracle {
					assert record.kind !in [u64(12), 16]
					assert record.thread_identity == first.authority.owner_thread_identity
				}
				assert stop_oracle.len == 12
				for index, ticket in release_tickets {
					release_indices := native_win32_release_oracle_indices_for_identity(stop_oracle,
						ticket.native_identity)
					assert release_indices.len == 1
					expected_release_index := if index < 4 { index } else { index + 1 }
					assert release_indices[0] == expected_release_index
					assert stop_oracle[expected_release_index].kind == 1
					assert stop_oracle[expected_release_index].identity == ticket.native_identity
				}
				status_oracle := stop_oracle[4]
				assert status_oracle.sequence == u64(before_stop_oracle.len + 5)
				assert status_oracle.kind == 15
				assert status_oracle.identity == device_identity
				assert status_oracle.parent_identity == 0
				assert status_oracle.output_identity == 0
				assert status_oracle.auxiliary_identity == 0
				assert status_oracle.observed_count == 0
				assert status_oracle.valid_mask == native_valid_dxgi_removal_reason
				assert status_oracle.return_value == 0
				assert status_oracle.thread_identity == first.authority.owner_thread_identity
				assert first.authority.trace_len == before_stop_trace.trace_len + 71
				mut release_cursor := before_stop_trace.trace_len
				for ticket in release_tickets[..4] {
					release_start := native_dxgi_release_sequence_start_for_test(first.authority,
						ticket.native_identity)
					assert release_start == release_cursor
					assert native_operation_contexts_identical(first.authority.trace[release_start].context,
						ticket.context)
					release_cursor += 6
				}
				status_start := release_cursor
				expected_status_context := NativeOperationContext{
					authority_scope:        .renderer_attempt
					authority_token:        before_stop_trace.renderer_attempt_token
					renderer_attempt_token: before_stop_trace.renderer_attempt_token
					app_identity:           before_stop_trace.app_identity
					presence_mask:          native_context_has_target_identity
					domain:                 .dxgi
					operation:              .device_status
					call_site:              .shutdown_anchor
					scope:                  .renderer
					target_identity:        device_identity
					ordinal:                before_stop_trace.next_ordinal
				}
				expected_status_evidence := NativePrimitiveEvidence{
					valid_mask: native_valid_dxgi_removal_reason
				}
				expected_status_result := NativeRenderResult{
					domain:           .dxgi
					operation:        .device_status
					scope:            .renderer
					disposition:      .ok
					context:          expected_status_context
					actual_primitive: expected_status_evidence
					primitive:        expected_status_evidence
				}
				expected_status_milestones := [
					NativeOperationTraceMilestone.real_call,
					.actual_primitive,
					.effective_primitive,
					.acceptance,
					.health_latched,
				]
				expected_status_actual := [NativePrimitiveEvidence{}, expected_status_evidence,
					NativePrimitiveEvidence{}, expected_status_evidence, NativePrimitiveEvidence{}]
				expected_status_effective := [NativePrimitiveEvidence{},
					NativePrimitiveEvidence{}, expected_status_evidence, expected_status_evidence,
					NativePrimitiveEvidence{}]
				for offset, milestone in expected_status_milestones {
					entry := first.authority.trace[status_start + offset]
					assert entry.milestone == milestone
					assert native_operation_contexts_identical(entry.context,
						expected_status_context)
					assert native_proof_evidence_equal(entry.actual, expected_status_actual[offset])
					assert native_proof_evidence_equal(entry.effective,
						expected_status_effective[offset])
					assert entry.local_validation == .none
					expected_result := if milestone == .acceptance {
						expected_status_result
					} else {
						NativeRenderResult{}
					}
					assert native_proof_result_equal(entry.result, expected_result)
					expected_health := if milestone == .health_latched {
						NativeRendererHealth.ready
					} else {
						NativeRendererHealth.uninitialized
					}
					assert entry.health == expected_health
				}
				release_cursor += 5
				for ticket in release_tickets[4..] {
					release_start := native_dxgi_release_sequence_start_for_test(first.authority,
						ticket.native_identity)
					assert release_start == release_cursor
					assert native_operation_contexts_identical(first.authority.trace[release_start].context,
						ticket.context)
					release_cursor += 6
				}
				assert release_cursor == first.authority.trace_len
				startup.stop()!
				second := native_win32_backend_replay_snapshot_for_test(startup)
				second_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				native_win32_backend_assert_replay_equal_for_test(first, second)
				native_win32_oracle_assert_equal_for_test(first_oracle, second_oracle)
				assert startup.backend.native_operations.disarm_proof()

				mut window_app := native_runtime_new_app_for_test(.win32)!
				window := native_runtime_new_window_for_test(mut window_app,
					'phase-B Win32 window COM lifetime')!
				assert window_app.backend.native_operations.arm_proof()
				native_win32_lifetime_oracle_reset_for_test()
				_ = native_dxgi_render_healthy_target_for_test(mut window_app, window)!
				window_index := window_app.backend.win32.window_record_index(window) or {
					return error(err_window_not_found)
				}
				window_record := window_app.backend.win32.windows[window_index]
				window_tickets := [window_record.swapchain_ticket, window_record.render_view_ticket,
					window_record.depth_texture_ticket, window_record.depth_stencil_view_ticket]
				window_identities := [native_identity(window_record.swapchain),
					native_identity(window_record.render_view),
					native_identity(window_record.depth_texture),
					native_identity(window_record.depth_stencil_view)]
				mut window_ticket_snapshots := []NativeLifetimeTicketProofSnapshot{cap: window_tickets.len}
				for index, ticket_id in window_tickets {
					ticket := native_phase_b_assert_bound_ticket_for_test(&window_app.backend.native_operations,
						ticket_id, .com_reference, window_identities[index], 0, .app_lifetime)
					assert ticket.context.call_site == .window_prepare
					assert ticket.context.scope == .window_target
					for prior in window_ticket_snapshots {
						assert ticket.native_identity != prior.native_identity
						assert !native_operation_contexts_identical(ticket.context, prior.context)
					}
					window_ticket_snapshots << ticket
				}
				assert window_ticket_snapshots.len == 4
				window_release_tickets := [window_ticket_snapshots[3], window_ticket_snapshots[2],
					window_ticket_snapshots[1], window_ticket_snapshots[0]]
				before_destroy_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				before_destroy_trace := native_proof_snapshot(&window_app.backend.native_operations)
				window_app.destroy_window(window)!
				window_trace := native_proof_snapshot(&window_app.backend.native_operations)
				window_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert !window_trace.trace_overflow
				backbuffer_identity := native_win32_assert_transient_reference_ticket_for_test(window_trace,
					window_oracle, 8, .backbuffer_acquire, .window_prepare, .window_target, window, true)
				assert backbuffer_identity !in window_identities
				for ticket in window_app.backend.native_operations.lifetime_tickets {
					assert ticket.native_identity != backbuffer_identity
				}
				for ticket_id in window_tickets {
					assert native_lifetime_ticket_index_for_test(&window_app.backend.native_operations,
						ticket_id) == -1
				}
				for index in before_destroy_oracle.len .. window_oracle.len {
					assert window_oracle[index].kind == 1
				}
				native_win32_assert_release_bijection_for_test(window_trace, window_oracle)
				native_win32_assert_every_acquired_reference_released_for_test(window_oracle)
				mut window_release_cursor := native_lifetime_release_start_in_snapshot_for_test(window_trace,
					window_release_tickets[0].context)
				assert window_release_cursor >= before_destroy_trace.trace_len
				for ticket in window_release_tickets {
					release_start := native_dxgi_release_sequence_start_for_test(window_trace,
						ticket.native_identity)
					assert release_start == window_release_cursor
					assert native_operation_contexts_identical(window_trace.trace[release_start].context,
						ticket.context)
					window_release_cursor += 6
				}
				assert window_release_cursor == window_trace.trace_len
				assert native_win32_release_index_for_test(window_oracle, window_identities[3], 0) < native_win32_release_index_for_test(window_oracle,
					window_identities[2], 0)
				assert native_win32_release_index_for_test(window_oracle, window_identities[2], 0) < native_win32_release_index_for_test(window_oracle,
					window_identities[1], 0)
				assert native_win32_release_index_for_test(window_oracle, window_identities[1], 0) < native_win32_release_index_for_test(window_oracle,
					window_identities[0], 0)
				assert window_app.backend.native_operations.disarm_proof()
				window_app.stop()!
			}
		}
	}

	fn native_win32_wrong_thread_release_for_test(backend_pointer voidptr, identity u64, ticket u64) NativeWrongThreadReleaseResult {
		mut released := NativeLifetimeReleaseResult{
			value:     native_pointer(identity)
			ticket_id: ticket
		}
		unsafe {
			mut backend := &Win32Backend(backend_pointer)
			released = backend.release_identity(released.value, released.ticket_id, NativeOperationSeed{
				call_site: .shutdown_release
				scope:     .renderer
			})
		}
		return NativeWrongThreadReleaseResult{
			identity:        native_identity(released.value)
			ticket:          released.ticket_id
			native_released: released.native_released
			ticket_retired:  released.ticket_retired
		}
	}

	fn native_win32_assert_backend_start_all_clean_for_test(backend &Backend) {
		$if windows {
			assert backend.kind == .win32
			assert !backend.win32.started
			assert backend.win32.device == unsafe { nil }
			assert backend.win32.device_ticket == 0
			assert backend.win32.device_context == unsafe { nil }
			assert backend.win32.device_context_ticket == 0
			assert backend.win32.factory == unsafe { nil }
			assert backend.win32.factory_ticket == 0
			assert backend.win32.pending_init_dxgi_device == unsafe { nil }
			assert backend.win32.pending_init_dxgi_device_ticket == 0
			assert backend.win32.pending_init_adapter == unsafe { nil }
			assert backend.win32.pending_init_adapter_ticket == 0
			assert !backend.win32.using_warp
			assert backend.win32.anchor_color_texture == unsafe { nil }
			assert backend.win32.anchor_color_texture_ticket == 0
			assert backend.win32.anchor_render_view == unsafe { nil }
			assert backend.win32.anchor_render_view_ticket == 0
			assert backend.win32.anchor_depth_texture == unsafe { nil }
			assert backend.win32.anchor_depth_texture_ticket == 0
			assert backend.win32.anchor_depth_stencil_view == unsafe { nil }
			assert backend.win32.anchor_depth_stencil_view_ticket == 0
			assert !backend.win32.anchor_committed
			assert backend.win32.windows.len == 0
			assert !backend.win32.retains_native_ownership()
			assert !backend.native_operations.has_live_lifetime_tickets()
			assert backend.native_operations.lifetime_tickets.len == 0
		}
	}

	fn native_win32_exercise_renderer_probe_and_process_commit_for_test() ! {
		$if windows {
			$if sokol_d3d11 ? {
				mut probe_backend := new_backend(.win32, true)!
				probe_app_identity := u64(0x7119)
				probe_backend.bind_app_native_operations(probe_app_identity,
					probe_app_identity + 1, probe_app_identity + 2)!
				assert probe_backend.native_operations.arm_proof()
				native_win32_lifetime_oracle_reset_for_test()
				probe_caps := probe_backend.probe_renderer_capabilities()!
				assert probe_caps.backend == .win32
				assert probe_caps.d3d11
				native_win32_assert_backend_start_all_clean_for_test(&probe_backend)
				probe_proof := native_proof_snapshot(&probe_backend.native_operations)
				probe_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert native_win32_oracle_kind_count_for_test(probe_oracle,
					native_win32_process_commit_oracle_kind) == 0
				assert !probe_proof.trace_overflow
				assert !probe_backend.native_operations.has_pending_native_plans()
				native_win32_assert_release_bijection_for_test(probe_proof, probe_oracle)
				native_win32_assert_every_acquired_reference_released_for_test(probe_oracle)
				assert probe_backend.native_operations.disarm_proof()

				mut start_backend := new_backend(.win32, true)!
				start_app_identity := u64(0x7121)
				start_backend.bind_app_native_operations(start_app_identity,
					start_app_identity + 1, start_app_identity + 2)!
				assert start_backend.native_operations.arm_proof()
				native_win32_lifetime_oracle_reset_for_test()
				start_backend.start(true)!
				assert start_backend.win32.started
				assert start_backend.win32.device != unsafe { nil }
				assert start_backend.win32.device_ticket != 0
				assert start_backend.win32.device_context != unsafe { nil }
				assert start_backend.win32.device_context_ticket != 0
				start_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert native_win32_oracle_kind_count_for_test(start_oracle,
					native_win32_process_commit_oracle_kind) == 1
				device_create_index := native_win32_oracle_output_index_for_test(start_oracle, 2,
					native_identity(start_backend.win32.device))
				process_commit_index := native_win32_oracle_kind_index_for_test(start_oracle,
					native_win32_process_commit_oracle_kind)
				assert device_create_index >= 0
				assert process_commit_index >= 0
				assert device_create_index < process_commit_index
				assert process_commit_index == start_oracle.len - 1
				process_commit := start_oracle[process_commit_index]
				assert process_commit.identity != 0
				assert process_commit.thread_identity == start_backend.native_operations.owner_thread_identity
				assert !start_backend.native_operations.has_pending_native_plans()
				start_backend.stop()!
				native_win32_assert_backend_start_all_clean_for_test(&start_backend)
				start_proof := native_proof_snapshot(&start_backend.native_operations)
				closed_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert native_win32_oracle_kind_count_for_test(closed_oracle,
					native_win32_process_commit_oracle_kind) == 1
				assert !start_proof.trace_overflow
				assert !start_backend.native_operations.has_pending_native_plans()
				native_win32_assert_release_bijection_for_test(start_proof, closed_oracle)
				native_win32_assert_every_acquired_reference_released_for_test(closed_oracle)
				assert start_backend.native_operations.disarm_proof()
			}
		}
	}

	fn native_win32_exercise_backend_start_rejection_for_test(use_probe bool) !string {
		$if windows {
			$if sokol_d3d11 ? {
				mut backend := new_backend(.win32, true)!
				app_identity := if use_probe { u64(0x7111) } else { u64(0x7109) }
				backend.bind_app_native_operations(app_identity, app_identity + 1, app_identity + 2)!
				assert backend.native_operations.arm_proof()
				context := NativeOperationContext{
					authority_scope:        .renderer_attempt
					authority_token:        backend.native_operations.renderer_attempt_token
					renderer_attempt_token: backend.native_operations.renderer_attempt_token
					app_identity:           app_identity
					domain:                 .dxgi
					operation:              .device_create
					call_site:              .renderer_start
					scope:                  .renderer
					ordinal:                backend.native_operations.next_ordinal
				}
				assert backend.native_operations.arm(context, NativePrimitiveEvidence{
					valid_mask:   native_valid_return_value
					return_value: i64(u32(0x887a0005))
				})
				native_win32_lifetime_oracle_reset_for_test()
				mut failure := ''
				if use_probe {
					_ = backend.probe_renderer_capabilities() or {
						failure = err.msg()
						Capabilities{}
					}
				} else {
					backend.start(true) or { failure = err.msg() }
				}
				assert failure == err_win32_d3d_device_failed
				assert failure.count(err_win32_d3d_device_failed) == 1
				native_win32_assert_backend_start_all_clean_for_test(&backend)
				proof := native_proof_snapshot(&backend.native_operations)
				oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert native_win32_oracle_kind_count_for_test(oracle,
					native_win32_process_commit_oracle_kind) == 0
				assert !proof.trace_overflow
				assert !backend.native_operations.has_pending_native_plans()
				mut saw_actual_acquisition := false
				mut saw_effective_rejection := false
				for index in 0 .. proof.trace_len {
					entry := proof.trace[index]
					if !native_operation_contexts_identical(entry.context, context) {
						continue
					}
					if entry.milestone == .actual_primitive {
						assert entry.actual.handle != 0
						assert entry.actual.object_identity_0 != 0
						saw_actual_acquisition = true
					}
					if entry.milestone == .effective_primitive {
						assert entry.effective.return_value == i64(u32(0x887a0005))
						saw_effective_rejection = true
					}
				}
				assert saw_actual_acquisition
				assert saw_effective_rejection
				native_win32_assert_release_bijection_for_test(proof, oracle)
				native_win32_assert_every_acquired_reference_released_for_test(oracle)
				assert backend.native_operations.disarm_proof()
				return failure
			}
		}
		return error(err_backend_unsupported)
	}

	fn native_win32_exercise_unavailable_shutdown_with_fake_vtables_for_test() ! {
		$if windows {
			$if sokol_d3d11 ? {
				mut app := native_phase_b_new_app_without_renderer_for_test(.win32)!
				assert app.backend.native_operations.arm_proof()
				mut cleanup := app.backend.native_operations.reserve_app_lifetime_ordinals(2)!
				seed := NativeOperationSeed{
					call_site: .shutdown_release
					scope:     .renderer
				}
				device_ticket := app.backend.win32.reserve_com_lifetime_ticket(mut cleanup, seed)!
				context_ticket := app.backend.win32.reserve_com_lifetime_ticket(mut cleanup, seed)!
				C.v_multiwindow_test_win32_fake_d3d_reset()
				native_win32_lifetime_oracle_reset_for_test()
				device_identity := C.v_multiwindow_test_win32_fake_d3d_device_identity()
				context_identity := C.v_multiwindow_test_win32_fake_d3d_context_identity()
				assert device_identity != 0
				assert context_identity != 0
				assert device_identity != context_identity
				app.backend.native_operations.bind_lifetime_ticket(device_ticket, device_identity,
					0)
				app.backend.native_operations.bind_lifetime_ticket(context_ticket,
					context_identity, 0)
				app.backend.win32.device = native_pointer(device_identity)
				app.backend.win32.device_ticket = device_ticket
				app.backend.win32.device_context = native_pointer(context_identity)
				app.backend.win32.device_context_ticket = context_ticket
				app.backend.win32.render_health = .unavailable
				_ = native_phase_b_assert_bound_ticket_for_test(&app.backend.native_operations,
					device_ticket, .com_reference, device_identity, 0, .app_lifetime)
				context_bound := native_phase_b_assert_bound_ticket_for_test(&app.backend.native_operations,
					context_ticket, .com_reference, context_identity, 0, .app_lifetime)
				assert app.backend.native_operations.arm(context_bound.context, NativePrimitiveEvidence{
					valid_mask:   native_valid_return_value
					return_value: i64(u32(0x80004005))
				})
				mut first_stop_error := ''
				app.stop() or { first_stop_error = err.msg() }
				expected_stop_error := '${err_render_terminal_aggregate}: ${err_render_native_renderer_unavailable}'
				assert first_stop_error == expected_stop_error
				assert first_stop_error.count(err_render_native_renderer_unavailable) == 1
				first := native_win32_backend_replay_snapshot_for_test(app)
				first_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert first.app_status == .stopped
				assert first.stop_terminal == first_stop_error
				assert first.stop_terminal.count(err_render_native_renderer_unavailable) == 1
				assert !first.started
				assert first.device == 0
				assert first.device_ticket == 0
				assert first.device_context == 0
				assert first.device_context_ticket == 0
				assert first.factory == 0
				assert first.factory_ticket == 0
				assert first.pending_init_dxgi_device == 0
				assert first.pending_init_dxgi_device_ticket == 0
				assert first.pending_init_adapter == 0
				assert first.pending_init_adapter_ticket == 0
				assert first.anchor_color_texture == 0
				assert first.anchor_color_texture_ticket == 0
				assert first.anchor_render_view == 0
				assert first.anchor_render_view_ticket == 0
				assert first.anchor_depth_texture == 0
				assert first.anchor_depth_texture_ticket == 0
				assert first.anchor_depth_stencil_view == 0
				assert first.anchor_depth_stencil_view_ticket == 0
				assert !first.anchor_committed
				assert first.windows.len == 0
				assert first.authority.registry.tickets.len == 0
				assert !app.backend.win32.retains_native_ownership()
				assert !app.backend.native_operations.has_live_lifetime_tickets()
				assert C.v_multiwindow_test_win32_fake_d3d_overflow() == 0
				assert C.v_multiwindow_test_win32_fake_d3d_count() == 2
				assert C.v_multiwindow_test_win32_fake_d3d_kind(0) == 2
				assert C.v_multiwindow_test_win32_fake_d3d_identity(0) == context_identity
				assert C.v_multiwindow_test_win32_fake_d3d_remaining(0) == 0
				assert C.v_multiwindow_test_win32_fake_d3d_kind(1) == 1
				assert C.v_multiwindow_test_win32_fake_d3d_identity(1) == device_identity
				assert C.v_multiwindow_test_win32_fake_d3d_remaining(1) == 0
				assert first_oracle.len == 2
				assert first_oracle[0].kind == 1
				assert first_oracle[0].identity == context_identity
				assert first_oracle[0].observed_count == 0
				assert first_oracle[1].kind == 1
				assert first_oracle[1].identity == device_identity
				assert first_oracle[1].observed_count == 0
				native_win32_assert_release_bijection_for_test(first.authority, first_oracle)
				mut replay_stop_error := ''
				app.stop() or { replay_stop_error = err.msg() }
				assert replay_stop_error == expected_stop_error
				assert replay_stop_error == first_stop_error
				assert replay_stop_error.count(err_render_native_renderer_unavailable) == 1
				second := native_win32_backend_replay_snapshot_for_test(app)
				second_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				native_win32_backend_assert_replay_equal_for_test(first, second)
				native_win32_oracle_assert_equal_for_test(first_oracle, second_oracle)
				assert C.v_multiwindow_test_win32_fake_d3d_count() == 2
				assert app.backend.native_operations.disarm_proof()
			}
		}
	}

	fn native_win32_exercise_phase_b_registry_edges() ! {
		$if windows {
			$if sokol_d3d11 ? {
				mut authority := NativeOperationAuthority{}
				native_bind_renderer_authority_for_test(mut authority, 0x7101, 0x7103)!
				assert authority.arm_proof()
				mut backend := new_win32_backend()
				unsafe {
					backend.native_operations = &authority
				}
				backend.render_health = .ready
				native_win32_lifetime_oracle_reset_for_test()
				C.v_multiwindow_test_win32_fake_unknown_reset(1)
				identity := C.v_multiwindow_test_win32_fake_unknown_identity()
				assert identity != 0
				seed := NativeOperationSeed{
					call_site: .renderer_start
					scope:     .renderer
				}
				mut cleanup := authority.reserve_app_lifetime_ordinals(3)!
				original_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed)!
				alias_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed)!
				null_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed)!
				backend.bind_or_burn_com_lifetime_ticket(original_ticket, native_pointer(identity))
				original := native_phase_b_assert_bound_ticket_for_test(&authority,
					original_ticket, .com_reference, identity, 0, .app_lifetime)
				null_value := voidptr(unsafe { nil })
				backend.bind_or_burn_com_lifetime_ticket(null_ticket, null_value)
				assert native_lifetime_ticket_index_for_test(&authority, null_ticket) == -1
				mut operation := authority.reserve_renderer_attempt_ordinals(1)!
				operation_context := operation.materialize(&authority, .dxgi, .device_query,
					seed.with_target_identity(identity))!
				assert authority.arm(operation_context, NativePrimitiveEvidence{
					valid_mask:   native_valid_return_value
					return_value: i64(u32(0x80004005))
				})
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_query_dxgi_device(identity, &raw)
				assert raw.handle == identity
				assert C.v_multiwindow_test_win32_fake_unknown_references() == 2
				backend.bind_or_burn_com_lifetime_ticket(alias_ticket, native_pointer(raw.handle))
				alias := native_phase_b_assert_bound_ticket_for_test(&authority, alias_ticket,
					.com_reference, identity, 0, .app_lifetime)
				assert alias.ticket_id != original.ticket_id
				assert alias.native_identity == original.native_identity
				capture := authority.capture_call(operation_context, raw)
				result := authority.accept_dxgi(operation_context, capture, .none)
				authority.record_health_latch(operation_context, backend.render_health)
				assert !result.succeeded()
				assert result.actual_primitive.handle == identity
				assert result.primitive.return_value == i64(u32(0x80004005))
				assert authority.arm(alias.context, NativePrimitiveEvidence{
					valid_mask:   native_valid_return_value
					return_value: i64(u32(0x887a0005))
				})
				before_alias_release := native_proof_snapshot(&authority)
				before_alias_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				alias_release :=
					backend.release_identity(native_pointer(identity), alias_ticket, seed)
				assert alias_release.native_released
				assert alias_release.ticket_retired
				assert alias_release.value == unsafe { nil }
				assert alias_release.ticket_id == 0
				assert native_lifetime_ticket_index_for_test(&authority, alias_ticket) == -1
				assert native_lifetime_ticket_index_for_test(&authority, original_ticket) >= 0
				assert C.v_multiwindow_test_win32_fake_unknown_references() == 1
				after_alias_release := native_proof_snapshot(&authority)
				alias_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert alias_oracle.len == before_alias_oracle.len + 1
				assert after_alias_release.next_ordinal == before_alias_release.next_ordinal
				assert after_alias_release.trace_len == before_alias_release.trace_len + 6
				expected_release_milestones := [
					NativeOperationTraceMilestone.real_call,
					.actual_primitive,
					.effective_primitive,
					.acceptance,
					.authority_release,
					.health_latched,
				]
				for offset, milestone in expected_release_milestones {
					entry := after_alias_release.trace[before_alias_release.trace_len + offset]
					assert entry.milestone == milestone
					assert native_operation_contexts_identical(entry.context, alias.context)
				}
				assert after_alias_release.trace[before_alias_release.trace_len + 2].effective.return_value == i64(u32(0x887a0005))
				assert after_alias_release.trace[before_alias_release.trace_len + 3].result.disposition == .renderer_lost
				assert after_alias_release.trace[before_alias_release.trace_len + 5].health == .lost
				assert !authority.has_pending_native_plans()
				before_wrong_thread := native_proof_snapshot(&authority)
				before_wrong_thread_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				release_thread := spawn native_win32_wrong_thread_release_for_test(voidptr(&backend),
					identity, original_ticket)
				wrong_thread := release_thread.wait()
				assert !wrong_thread.native_released
				assert !wrong_thread.ticket_retired
				assert wrong_thread.identity == identity
				assert wrong_thread.ticket == original_ticket
				native_proof_assert_snapshots_equal(before_wrong_thread,
					native_proof_snapshot(&authority))
				native_win32_oracle_assert_equal_for_test(before_wrong_thread_oracle,
					native_win32_lifetime_oracle_snapshot_for_test())
				assert C.v_multiwindow_test_win32_fake_unknown_references() == 1
				original_release := backend.release_identity(native_pointer(identity),
					original_ticket, seed)
				assert original_release.native_released
				assert original_release.ticket_retired
				assert original_release.value == unsafe { nil }
				assert original_release.ticket_id == 0
				assert C.v_multiwindow_test_win32_fake_unknown_references() == 0
				assert authority.lifetime_tickets.len == 0
				assert !backend.retains_native_ownership()
				records := native_win32_lifetime_oracle_snapshot_for_test()
				release_indices := native_win32_release_oracle_indices_for_identity(records,
					identity)
				assert release_indices.len == 2
				assert records[release_indices[0]].observed_count == 1
				assert records[release_indices[1]].observed_count == 0
				assert records[release_indices[0]].thread_identity == authority.owner_thread_identity
				assert records[release_indices[1]].thread_identity == authority.owner_thread_identity
				proof := native_proof_snapshot(&authority)
				native_win32_assert_release_bijection_for_test(proof, records)
				assert !proof.trace_overflow
				assert !authority.has_pending_native_plans()
				assert authority.disarm_proof()
				if native_dxgi_runtime_requested_for_test() {
					direct_start_error :=
						native_win32_exercise_backend_start_rejection_for_test(false)!
					probe_error := native_win32_exercise_backend_start_rejection_for_test(true)!
					assert probe_error == direct_start_error
					native_win32_exercise_renderer_probe_and_process_commit_for_test()!
					native_win32_exercise_unavailable_shutdown_with_fake_vtables_for_test()!
				}
			}
		}
	}

	fn native_win32_assert_owned_slots_equal_for_test(expected NativeWin32BackendReplaySnapshot, actual NativeWin32BackendReplaySnapshot) {
		assert actual.app_status == expected.app_status
		assert actual.started == expected.started
		assert actual.device == expected.device
		assert actual.device_ticket == expected.device_ticket
		assert actual.device_context == expected.device_context
		assert actual.device_context_ticket == expected.device_context_ticket
		assert actual.factory == expected.factory
		assert actual.factory_ticket == expected.factory_ticket
		assert actual.pending_init_dxgi_device == expected.pending_init_dxgi_device
		assert actual.pending_init_dxgi_device_ticket == expected.pending_init_dxgi_device_ticket
		assert actual.pending_init_adapter == expected.pending_init_adapter
		assert actual.pending_init_adapter_ticket == expected.pending_init_adapter_ticket
		assert actual.using_warp == expected.using_warp
		assert actual.anchor_color_texture == expected.anchor_color_texture
		assert actual.anchor_color_texture_ticket == expected.anchor_color_texture_ticket
		assert actual.anchor_render_view == expected.anchor_render_view
		assert actual.anchor_render_view_ticket == expected.anchor_render_view_ticket
		assert actual.anchor_depth_texture == expected.anchor_depth_texture
		assert actual.anchor_depth_texture_ticket == expected.anchor_depth_texture_ticket
		assert actual.anchor_depth_stencil_view == expected.anchor_depth_stencil_view
		assert actual.anchor_depth_stencil_view_ticket == expected.anchor_depth_stencil_view_ticket
		assert actual.anchor_committed == expected.anchor_committed
		assert actual.windows == expected.windows
		native_lifetime_registry_assert_snapshots_equal(expected.authority.registry,
			actual.authority.registry)
	}

	fn native_win32_assert_ticket_release_delta_for_test(tickets []NativeLifetimeTicketProofSnapshot, records []NativeWin32LifetimeOracleRecord) {
		mut release_records := []NativeWin32LifetimeOracleRecord{}
		for record in records {
			if record.kind == 1 {
				assert record.identity != 0
				assert record.valid_mask & native_valid_observed_count != 0
				assert record.thread_identity != 0
				release_records << record
			}
		}
		mut expected := []NativeLifetimeTicketProofSnapshot{}
		for ticket in tickets {
			if ticket.state == .bound && ticket.release_kind == .com_reference {
				expected << ticket
			}
		}
		assert release_records.len == expected.len
		mut consumed := []bool{len: release_records.len}
		for ticket in expected {
			mut matched := -1
			for index, record in release_records {
				if !consumed[index] && record.identity == ticket.native_identity {
					matched = index
					break
				}
			}
			assert matched >= 0
			consumed[matched] = true
		}
		for matched in consumed {
			assert matched
		}
	}

	fn native_win32_assert_cleanup_order_for_test(ownership NativeWin32BackendReplaySnapshot, records []NativeWin32LifetimeOracleRecord) {
		mut expected := []u64{}
		for window in ownership.windows {
			for identity in [window.depth_stencil_view, window.depth_texture, window.render_view,
				window.swapchain] {
				if identity != 0 {
					expected << identity
				}
			}
		}
		for identity in [ownership.anchor_depth_stencil_view, ownership.anchor_depth_texture,
			ownership.anchor_render_view, ownership.anchor_color_texture, ownership.factory,
			ownership.device_context, ownership.device] {
			if identity != 0 {
				expected << identity
			}
		}
		mut actual := []u64{}
		for record in records {
			if record.kind == 1 {
				actual << record.identity
			}
		}
		assert actual == expected
	}

	fn native_win32_exercise_phase_b_exhaustion_and_overflow() ! {
		$if windows {
			$if sokol_d3d11 ? {
				mut exhausted_app := native_runtime_new_app_for_test(.win32)!
				window := native_runtime_new_window_for_test(mut exhausted_app,
					'phase-B Win32 ordinal exhaustion')!
				_ = native_dxgi_render_healthy_target_for_test(mut exhausted_app, window)!
				exhausted_app.resize_window(window, 208, 144)!
				for _ in 0 .. 64 {
					exhausted_app.poll_events()!
					if exhausted_app.render_window_eligible(window)! {
						break
					}
				}
				assert exhausted_app.render_window_eligible(window)!
				assert exhausted_app.backend.native_operations.arm_proof()
				sokol_generation := native_install_sokol_trace_for_test()!
				mut exhaustion_state := &NativeWin32PhaseBExhaustionState{}
				outcome := exhausted_app.with_scheduled_render_batch(fn [mut exhausted_app, window, mut exhaustion_state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
					assert candidates.any(it.window == window)
					state := exhausted_app.render_backend_state()!
					index := exhausted_app.backend.win32.window_record_index(window) or {
						return error(err_window_not_found)
					}
					target_generation := exhausted_app.backend.win32.windows[index].render_target_generation
					assert !exhausted_app.backend.win32.windows[index].render_resize_pending
					exhausted_app.backend.win32.windows[index].render_resize_pending = true
					assert exhausted_app.backend.win32.windows[index].render_resize_pending
					lease := RenderTargetLease{
						app_instance: exhausted_app.instance_id
						batch_epoch:  batch.epoch
						target_epoch: state.next_target_epoch
						window_epoch: exhausted_app.render_runtime.next_lease_epoch
						window:       window
					}
					exhausted_app.backend.native_operations.next_ordinal = ~u64(0)
					mut last :=
						exhausted_app.backend.native_operations.reserve_renderer_attempt_ordinals(1)!
					assert last.first == ~u64(0)
					assert last.count == 1
					assert last.authority_scope == .renderer_attempt
					assert exhausted_app.backend.native_operations.next_ordinal == 0
					assert !exhausted_app.backend.native_operations.sequence_exhausted
					clear_seed := NativeOperationSeed{
						presence_mask:      native_context_window_target_fields | native_context_has_target_identity
						call_site:          .window_prepare
						scope:              .window_target
						window:             window
						target_generation:  target_generation
						target_identity:    native_identity(exhausted_app.backend.win32.device_context)
						batch_epoch:        batch.epoch
						window_lease_epoch: lease.window_epoch
						target_lease_epoch: lease.target_epoch
					}
					exhaustion_state.armed_context = last.materialize(exhausted_app.backend.native_operations,
						.dxgi, .clear_state, clear_seed)!
					assert exhaustion_state.armed_context.ordinal == ~u64(0)
					assert exhausted_app.backend.native_operations.arm(exhaustion_state.armed_context, NativePrimitiveEvidence{
						valid_mask: native_valid_return_value
					})
					native_win32_lifetime_oracle_reset_for_test()
					exhaustion_state.boundary_oracle =
						native_win32_lifetime_oracle_snapshot_for_test()
					exhaustion_state.boundary_ownership =
						native_win32_backend_replay_snapshot_for_test(exhausted_app)
					exhausted_app.acquire_render_target(batch, window)!
				})!
				expected_exhaustion_error := '${err_render_terminal_aggregate}: ${err_render_native_renderer_unavailable}'
				assert outcome.error == expected_exhaustion_error
				assert outcome.error.count(err_render_native_renderer_unavailable) == 1
				assert !outcome.committed
				assert !outcome.had_gpu_work
				assert outcome.completed_user_passes == 0
				assert outcome.finalized_submissions == 0
				assert exhausted_app.render_runtime.renderer_terminal == expected_exhaustion_error
				assert exhausted_app.backend.native_operations.sequence_exhausted
				assert exhausted_app.backend.native_operations.terminal_cause == .sequence_exhausted
				assert exhausted_app.backend.native_operations.next_ordinal == 0
				assert exhausted_app.backend.native_operations.proof.plan[0].armed
				assert native_operation_contexts_identical(exhausted_app.backend.native_operations.proof.plan[0].context,
					exhaustion_state.armed_context)
				after_boundary_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				native_win32_oracle_assert_equal_for_test(exhaustion_state.boundary_oracle,
					after_boundary_oracle)
				after_boundary_ownership :=
					native_win32_backend_replay_snapshot_for_test(exhausted_app)
				native_win32_assert_owned_slots_equal_for_test(exhaustion_state.boundary_ownership,
					after_boundary_ownership)
				assert after_boundary_ownership.authority.trace_len == exhaustion_state.boundary_ownership.authority.trace_len
				assert after_boundary_ownership.authority.trace_overflow == exhaustion_state.boundary_ownership.authority.trace_overflow
				native_proof_assert_plan_equal(exhaustion_state.boundary_ownership.authority.plan,
					after_boundary_ownership.authority.plan)
				native_proof_assert_trace_equal(exhaustion_state.boundary_ownership.authority.trace,
					after_boundary_ownership.authority.trace)
				assert exhaustion_state.boundary_ownership.render_health == .ready
				assert after_boundary_ownership.render_health == .unavailable
				native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
				native_uninstall_sokol_trace_for_test(sokol_generation)!
				cleanup_tickets := after_boundary_ownership.authority.registry.tickets
				mut first_stop_error := ''
				exhausted_app.stop() or { first_stop_error = err.msg() }
				assert exhausted_app.render_runtime.renderer_terminal == expected_exhaustion_error
				first := native_win32_backend_replay_snapshot_for_test(exhausted_app)
				first_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert first_stop_error == ''
				assert first.app_status == .stopped
				assert first.stop_terminal == ''
				assert first.authority.registry.tickets.len == 0
				native_win32_assert_ticket_release_delta_for_test(cleanup_tickets,
					first_oracle[after_boundary_oracle.len..])
				native_win32_assert_cleanup_order_for_test(after_boundary_ownership,
					first_oracle[after_boundary_oracle.len..])
				for record in first_oracle[after_boundary_oracle.len..] {
					assert record.kind == 1
				}
				mut replay_stop_error := ''
				exhausted_app.stop() or { replay_stop_error = err.msg() }
				assert exhausted_app.render_runtime.renderer_terminal == expected_exhaustion_error
				second := native_win32_backend_replay_snapshot_for_test(exhausted_app)
				second_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				native_win32_backend_assert_replay_equal_for_test(first, second)
				native_win32_oracle_assert_equal_for_test(first_oracle, second_oracle)
				assert replay_stop_error == ''
				assert replay_stop_error == first_stop_error
				_ =
					exhausted_app.backend.native_operations.inject(exhaustion_state.armed_context, NativePrimitiveEvidence{})
				assert exhausted_app.backend.native_operations.disarm_proof()

				mut overflow_app := native_runtime_new_app_for_test(.win32)!
				assert overflow_app.backend.native_operations.arm_proof()
				overflow_ownership := native_win32_backend_replay_snapshot_for_test(overflow_app)
				overflow_tickets := native_lifetime_registry_snapshot(&overflow_app.backend.native_operations).tickets
				assert overflow_tickets.len == 7
				for index in 0 .. native_operation_trace_capacity {
					overflow_app.backend.native_operations.record(.real_call, NativeOperationContext{
						authority_scope:        .renderer_attempt
						authority_token:        overflow_app.backend.native_operations.renderer_attempt_token
						renderer_attempt_token: overflow_app.backend.native_operations.renderer_attempt_token
						app_identity:           overflow_app.instance_id
						ordinal:                u64(index + 1)
					}, NativePrimitiveEvidence{}, NativePrimitiveEvidence{}, .none, NativeRenderResult{})
				}
				prefix := overflow_app.backend.native_operations.proof.trace
				native_win32_lifetime_oracle_reset_for_test()
				overflow_app.stop()!
				overflow_first := native_win32_backend_replay_snapshot_for_test(overflow_app)
				overflow_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				assert overflow_first.authority.trace_len == native_operation_trace_capacity
				assert overflow_first.authority.trace_overflow
				native_proof_assert_trace_equal(prefix, overflow_first.authority.trace)
				assert overflow_first.authority.registry.tickets.len == 0
				native_win32_assert_ticket_release_delta_for_test(overflow_tickets, overflow_oracle)
				native_win32_assert_cleanup_order_for_test(overflow_ownership, overflow_oracle)
				overflow_app.stop()!
				overflow_second := native_win32_backend_replay_snapshot_for_test(overflow_app)
				overflow_second_oracle := native_win32_lifetime_oracle_snapshot_for_test()
				native_win32_backend_assert_replay_equal_for_test(overflow_first, overflow_second)
				native_win32_oracle_assert_equal_for_test(overflow_oracle, overflow_second_oracle)
				assert overflow_app.backend.native_operations.disarm_proof()
			}
		}
	}

	fn native_appkit_wrong_thread_release_for_test(backend_pointer voidptr, identity u64, ticket u64, release_kind NativeLifetimeReleaseKind) NativeWrongThreadReleaseResult {
		mut released := NativeLifetimeReleaseResult{
			value:     native_pointer(identity)
			ticket_id: ticket
		}
		unsafe {
			mut backend := &AppKitBackend(backend_pointer)
			released = backend.release_appkit_lifetime_identity(released.value, released.ticket_id,
				release_kind, 0, err_appkit_metal_device_failed)
		}
		return NativeWrongThreadReleaseResult{
			identity:        native_identity(released.value)
			ticket:          released.ticket_id
			native_released: released.native_released
			ticket_retired:  released.ticket_retired
		}
	}

	fn native_appkit_nonmain_owner_release_for_test(identity u64) NativeWrongThreadReleaseResult {
		mut authority := NativeOperationAuthority{}
		native_bind_renderer_authority_for_test(mut authority, 0x7211, 0x7213) or { panic(err) }
		mut backend := new_appkit_backend()
		unsafe {
			backend.native_operations = &authority
		}
		backend.render_health = .ready
		mut cleanup := authority.reserve_app_lifetime_ordinals(1) or { panic(err) }
		ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup, .metal_device, NativeOperationSeed{
			call_site: .renderer_start
			scope:     .renderer
		}) or { panic(err) }
		authority.bind_lifetime_ticket(ticket, identity, 0)
		before := native_proof_snapshot(&authority)
		released := backend.release_appkit_lifetime_identity(native_pointer(identity), ticket,
			.metal_device, 0, err_appkit_metal_device_failed)
		assert !released.native_released
		assert !released.ticket_retired
		assert native_identity(released.value) == identity
		assert released.ticket_id == ticket
		native_proof_assert_snapshots_equal(before, native_proof_snapshot(&authority))
		assert authority.lifetime_tickets.len == 1
		assert authority.lifetime_tickets[0].state == .bound
		assert authority.lifetime_tickets[0].native_identity == identity
		return NativeWrongThreadReleaseResult{
			identity:        native_identity(released.value)
			ticket:          released.ticket_id
			native_released: released.native_released
			ticket_retired:  released.ticket_retired
		}
	}

	fn native_appkit_assert_backend_start_all_clean_for_test(backend &Backend) {
		$if darwin {
			assert backend.kind == .appkit
			assert !backend.appkit.started
			assert backend.appkit.device == unsafe { nil }
			assert backend.appkit.device_ticket == 0
			assert backend.appkit.anchor_state == unsafe { nil }
			assert backend.appkit.anchor_state_ticket == 0
			assert backend.appkit.batch_autorelease_pool == unsafe { nil }
			assert backend.appkit.batch_autorelease_pool_ticket == 0
			assert backend.appkit.active_anchor_lease == 0
			assert backend.appkit.active_anchor_drawable == unsafe { nil }
			assert backend.appkit.active_anchor_drawable_ticket == 0
			assert backend.appkit.pending_window_state.state == unsafe { nil }
			assert backend.appkit.pending_window_state.state_ticket == 0
			assert backend.appkit.windows.len == 0
			assert !backend.appkit.retains_native_ownership()
			assert !backend.native_operations.has_live_lifetime_tickets()
			assert backend.native_operations.lifetime_tickets.len == 0
		}
	}

	fn native_appkit_exercise_renderer_probe_and_process_commit_for_test() ! {
		$if darwin {
			mut probe_backend := new_backend(.appkit, true)!
			probe_app_identity := u64(0x7239)
			probe_backend.bind_app_native_operations(probe_app_identity, probe_app_identity + 1,

				probe_app_identity + 2)!
			assert probe_backend.native_operations.arm_proof()
			native_appkit_lifetime_oracle_reset_for_test()
			probe_side_generation := native_appkit_side_effect_reset_for_test()
			probe_caps := probe_backend.probe_renderer_capabilities()!
			assert probe_caps.backend == .appkit
			assert probe_caps.metal
			native_appkit_assert_backend_start_all_clean_for_test(&probe_backend)
			probe_proof := native_proof_snapshot(&probe_backend.native_operations)
			probe_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert native_appkit_oracle_kind_count_for_test(probe_oracle,
				native_appkit_process_commit_oracle_kind) == 0
			assert !probe_proof.trace_overflow
			assert !probe_backend.native_operations.has_pending_native_plans()
			native_appkit_assert_release_bijection_for_test(probe_proof, probe_oracle)
			_ = native_appkit_assert_side_effect_operation_bijection_for_test(probe_proof,
				probe_oracle, probe_side_generation, [], true)
			assert probe_backend.native_operations.disarm_proof()

			mut start_backend := new_backend(.appkit, true)!
			start_app_identity := u64(0x7241)
			start_backend.bind_app_native_operations(start_app_identity, start_app_identity + 1,

				start_app_identity + 2)!
			assert start_backend.native_operations.arm_proof()
			native_appkit_lifetime_oracle_reset_for_test()
			start_side_generation := native_appkit_side_effect_reset_for_test()
			start_backend.start(true)!
			assert start_backend.appkit.started
			assert start_backend.appkit.device != unsafe { nil }
			assert start_backend.appkit.device_ticket != 0
			start_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert native_appkit_oracle_kind_count_for_test(start_oracle,
				native_appkit_process_commit_oracle_kind) == 1
			device_create_index := native_appkit_oracle_kind_index_for_test(start_oracle, 1)
			process_commit_index := native_appkit_oracle_kind_index_for_test(start_oracle,
				native_appkit_process_commit_oracle_kind)
			assert device_create_index >= 0
			assert process_commit_index >= 0
			assert start_oracle[device_create_index].output_identity == native_identity(start_backend.appkit.device)
			assert device_create_index < process_commit_index
			assert process_commit_index == start_oracle.len - 1
			process_commit := start_oracle[process_commit_index]
			assert process_commit.identity != 0
			assert process_commit.thread_identity == start_backend.native_operations.owner_thread_identity
			assert !start_backend.native_operations.has_pending_native_plans()
			start_backend.stop()!
			native_appkit_assert_backend_start_all_clean_for_test(&start_backend)
			start_proof := native_proof_snapshot(&start_backend.native_operations)
			closed_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert native_appkit_oracle_kind_count_for_test(closed_oracle,
				native_appkit_process_commit_oracle_kind) == 1
			assert !start_proof.trace_overflow
			assert !start_backend.native_operations.has_pending_native_plans()
			native_appkit_assert_release_bijection_for_test(start_proof, closed_oracle)
			_ = native_appkit_assert_side_effect_operation_bijection_for_test(start_proof,
				closed_oracle, start_side_generation, [], true)
			assert start_backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_backend_start_rejection_for_test(use_probe bool) !string {
		$if darwin {
			mut backend := new_backend(.appkit, true)!
			app_identity := if use_probe { u64(0x7231) } else { u64(0x7229) }
			backend.bind_app_native_operations(app_identity, app_identity + 1, app_identity + 2)!
			assert backend.native_operations.arm_proof()
			context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        backend.native_operations.renderer_attempt_token
				renderer_attempt_token: backend.native_operations.renderer_attempt_token
				app_identity:           app_identity
				domain:                 .metal
				operation:              .device_create
				call_site:              .renderer_start
				scope:                  .renderer
				ordinal:                backend.native_operations.next_ordinal
			}
			assert backend.native_operations.arm(context, NativePrimitiveEvidence{
				valid_mask: native_valid_handle
				handle:     0
			})
			native_appkit_lifetime_oracle_reset_for_test()
			side_generation := native_appkit_side_effect_reset_for_test()
			mut failure := ''
			if use_probe {
				_ = backend.probe_renderer_capabilities() or {
					failure = err.msg()
					Capabilities{}
				}
			} else {
				backend.start(true) or { failure = err.msg() }
			}
			assert failure == err_appkit_metal_device_failed
			assert failure.count(err_appkit_metal_device_failed) == 1
			native_appkit_assert_backend_start_all_clean_for_test(&backend)
			proof := native_proof_snapshot(&backend.native_operations)
			oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert native_appkit_oracle_kind_count_for_test(oracle,
				native_appkit_process_commit_oracle_kind) == 0
			assert !proof.trace_overflow
			assert !backend.native_operations.has_pending_native_plans()
			actual_device := native_appkit_assert_effective_rejected_nonnull_for_test(proof,
				context, .renderer_unavailable, .unavailable)
			assert actual_device != 0
			native_appkit_assert_release_bijection_for_test(proof, oracle)
			_ = native_appkit_assert_side_effect_operation_bijection_for_test(proof, oracle,
				side_generation, [], true)
			assert backend.native_operations.disarm_proof()
			return failure
		}
		return error(err_backend_unsupported)
	}

	fn native_appkit_assert_effective_rejected_nonnull_for_test(snapshot NativeAuthorityProofSnapshot, context NativeOperationContext, expected_disposition NativeRenderDisposition, expected_health NativeRendererHealth) u64 {
		mut entries := []NativeOperationTraceEntry{}
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if native_operation_contexts_identical(entry.context, context) {
				entries << entry
			}
		}
		assert entries.len == 5
		expected := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		for index, milestone in expected {
			assert entries[index].milestone == milestone
		}
		assert entries[1].actual.valid_mask & native_valid_handle != 0
		assert entries[1].actual.handle != 0
		assert entries[2].effective.valid_mask & native_valid_handle != 0
		assert entries[2].effective.handle == 0
		assert !entries[3].result.succeeded()
		assert entries[3].result.disposition == expected_disposition
		assert entries[4].health == expected_health
		return entries[1].actual.handle
	}

	fn native_appkit_exercise_effective_anchor_rejection_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			anchor_side_generation := native_appkit_side_effect_reset_for_test()
			mut anchor_app := native_phase_b_new_app_without_renderer_for_test(.appkit)!
			anchor_app.backend.appkit.init_renderer()!
			assert anchor_app.backend.appkit.device != unsafe { nil }
			assert anchor_app.backend.native_operations.arm_proof()
			anchor_operation_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			anchor_context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        anchor_app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: anchor_app.backend.native_operations.renderer_attempt_token
				app_identity:           anchor_app.instance_id
				presence_mask:          native_context_has_target_identity
				domain:                 .metal
				operation:              .anchor_surface_create
				call_site:              .anchor_create
				scope:                  .anchor
				target_identity:        native_identity(anchor_app.backend.appkit.device)
				ordinal:                anchor_app.backend.native_operations.next_ordinal
			}
			assert anchor_app.backend.native_operations.arm(anchor_context, NativePrimitiveEvidence{
				valid_mask: native_valid_handle
				handle:     0
			})
			mut anchor_error := ''
			anchor_app.backend.appkit.create_renderer_anchor() or { anchor_error = err.msg() }
			assert anchor_error == err_render_anchor_failed
			anchor_failed_proof := native_proof_snapshot(&anchor_app.backend.native_operations)
			anchor_identity := native_appkit_assert_effective_rejected_nonnull_for_test(anchor_failed_proof,
				anchor_context, .renderer_unavailable, .unavailable)
			assert anchor_app.backend.appkit.anchor_state == unsafe { nil }
			assert anchor_app.backend.appkit.anchor_state_ticket == 0
			anchor_oracle_before_stop := native_appkit_lifetime_oracle_snapshot_for_test()
			assert anchor_oracle_before_stop.len == anchor_operation_start + 2
			assert anchor_oracle_before_stop[anchor_operation_start].kind == 7
			assert anchor_oracle_before_stop[anchor_operation_start].output_identity == anchor_identity
			assert anchor_oracle_before_stop[anchor_operation_start + 1].kind == 6
			assert anchor_oracle_before_stop[anchor_operation_start + 1].identity == anchor_identity
			assert anchor_oracle_before_stop[anchor_operation_start + 1].auxiliary_identity == anchor_identity
			assert anchor_app.backend.native_operations.lifetime_tickets.len == 1
			mut anchor_stop_error := ''
			anchor_app.stop() or { anchor_stop_error = err.msg() }
			anchor_first := native_appkit_backend_replay_snapshot_for_test(anchor_app)
			anchor_first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_assert_release_bijection_for_test(anchor_first.authority,
				anchor_first_oracle)
			anchor_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(anchor_first.authority,
				anchor_first_oracle, anchor_side_generation, [], true)
			mut anchor_replay_error := ''
			anchor_app.stop() or { anchor_replay_error = err.msg() }
			anchor_second := native_appkit_backend_replay_snapshot_for_test(anchor_app)
			anchor_second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(anchor_first, anchor_second)
			native_appkit_oracle_assert_equal_for_test(anchor_first_oracle, anchor_second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(anchor_first_side,
				native_appkit_side_effect_snapshot_for_test(anchor_side_generation))
			assert anchor_replay_error == anchor_stop_error
			assert anchor_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_effective_pool_rejection_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			pool_side_generation := native_appkit_side_effect_reset_for_test()
			mut pool_app := native_runtime_new_app_for_test(.appkit)!
			assert pool_app.backend.native_operations.arm_proof()
			pool_operation_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			pool_batch_epoch := pool_app.render_runtime.next_batch_epoch
			pool_context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        pool_app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: pool_app.backend.native_operations.renderer_attempt_token
				app_identity:           pool_app.instance_id
				presence_mask:          native_context_has_batch_epoch
				domain:                 .metal
				operation:              .render_batch_begin
				call_site:              .anchor_prepare
				scope:                  .batch
				batch_epoch:            pool_batch_epoch
				ordinal:                pool_app.backend.native_operations.next_ordinal
			}
			assert pool_app.backend.native_operations.arm(pool_context, NativePrimitiveEvidence{
				valid_mask: native_valid_handle
				handle:     0
			})
			mut pool_callback_calls := 0
			mut pool_error := ''
			pool_app.with_scheduled_render_batch(fn [mut pool_callback_calls] (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {
				pool_callback_calls++
			}) or { pool_error = err.msg() }
			assert pool_error == err_appkit_application_failed
			assert pool_callback_calls == 0
			pool_proof := native_proof_snapshot(&pool_app.backend.native_operations)
			pool_identity := native_appkit_assert_effective_rejected_nonnull_for_test(pool_proof,
				pool_context, .operation_failed, .ready)
			pool_oracle_before_stop := native_appkit_lifetime_oracle_snapshot_for_test()
			assert pool_oracle_before_stop.len == pool_operation_start + 2
			assert pool_oracle_before_stop[pool_operation_start].kind == 13
			assert pool_oracle_before_stop[pool_operation_start].output_identity == pool_identity
			assert pool_oracle_before_stop[pool_operation_start + 1].kind == 14
			assert pool_oracle_before_stop[pool_operation_start + 1].identity == pool_identity
			assert pool_app.backend.appkit.batch_autorelease_pool == unsafe { nil }
			assert pool_app.backend.appkit.batch_autorelease_pool_ticket == 0
			mut pool_stop_error := ''
			pool_app.stop() or { pool_stop_error = err.msg() }
			pool_first := native_appkit_backend_replay_snapshot_for_test(pool_app)
			pool_first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_assert_release_bijection_for_test(pool_first.authority, pool_first_oracle)
			pool_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(pool_first.authority,
				pool_first_oracle, pool_side_generation, [], true)
			mut pool_replay_error := ''
			pool_app.stop() or { pool_replay_error = err.msg() }
			pool_second := native_appkit_backend_replay_snapshot_for_test(pool_app)
			pool_second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(pool_first, pool_second)
			native_appkit_oracle_assert_equal_for_test(pool_first_oracle, pool_second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(pool_first_side,
				native_appkit_side_effect_snapshot_for_test(pool_side_generation))
			assert pool_replay_error == pool_stop_error
			assert pool_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_effective_drawable_rejection_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			drawable_side_generation := native_appkit_side_effect_reset_for_test()
			mut drawable_app := native_runtime_new_app_for_test(.appkit)!
			drawable_window := native_runtime_new_window_for_test(mut drawable_app,
				'phase-B effective-rejected AppKit drawable') or { return err }
			assert drawable_app.backend.native_operations.arm_proof()
			drawable_operation_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			mut state := &NativeAppKitEffectiveRejectionState{}
			drawable_outcome := drawable_app.with_scheduled_render_batch(fn [mut drawable_app, drawable_window, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
				assert candidates.any(it.window == drawable_window)
				_ = native_appkit_assert_pool_ticket_context_for_test(&drawable_app.backend.native_operations,
					drawable_app.backend.appkit.batch_autorelease_pool_ticket,
					native_identity(drawable_app.backend.appkit.batch_autorelease_pool), batch)
				acquisition := drawable_app.acquire_render_target(batch, drawable_window)!
				assert acquisition.status == .ready
				index := native_appkit_window_record_index_for_test(drawable_app, drawable_window)!
				record := drawable_app.backend.appkit.windows[index]
				state.drawable_context = NativeOperationContext{
					authority_scope:        .renderer_attempt
					authority_token:        drawable_app.backend.native_operations.renderer_attempt_token
					renderer_attempt_token: drawable_app.backend.native_operations.renderer_attempt_token
					app_identity:           drawable_app.instance_id
					presence_mask:          native_context_window_target_fields | native_context_has_target_identity
					domain:                 .metal
					operation:              .drawable_acquire
					call_site:              .window_activate
					scope:                  .window_target
					window:                 drawable_window
					target_generation:      acquisition.snapshot.target.target_identity
					target_identity:        native_identity(record.state)
					batch_epoch:            batch.epoch
					window_lease_epoch:     acquisition.lease.window_epoch
					target_lease_epoch:     acquisition.lease.target_epoch
					ordinal:                drawable_app.backend.native_operations.next_ordinal
				}
				assert drawable_app.backend.native_operations.arm(state.drawable_context, NativePrimitiveEvidence{
					valid_mask: native_valid_handle
					handle:     0
				})
				drawable_app.with_render_target_pass(acquisition.lease, gfx.PassAction{},
					fn () ! {}) or { state.drawable_pass_error = err.msg() }
				assert state.drawable_pass_error != ''
			})!
			assert state.drawable_pass_error == err_appkit_metal_drawable_failed
			assert drawable_outcome.error == ''
			assert !drawable_outcome.committed
			assert drawable_outcome.completed_user_passes == 0
			assert drawable_outcome.finalized_submissions == 0
			drawable_proof := native_proof_snapshot(&drawable_app.backend.native_operations)
			drawable_identity := native_appkit_assert_effective_rejected_nonnull_for_test(drawable_proof,
				state.drawable_context, .transient, .ready)
			drawable_oracle_before_stop := native_appkit_lifetime_oracle_snapshot_for_test()
			assert drawable_oracle_before_stop.len == drawable_operation_start + 4
			assert drawable_oracle_before_stop[drawable_operation_start].kind == 13
			assert drawable_oracle_before_stop[drawable_operation_start + 1].kind == 9
			assert drawable_oracle_before_stop[drawable_operation_start + 1].output_identity == drawable_identity
			assert drawable_oracle_before_stop[drawable_operation_start + 1].auxiliary_identity != 0
			assert drawable_oracle_before_stop[drawable_operation_start + 2].kind == 12
			assert drawable_oracle_before_stop[drawable_operation_start + 2].identity == drawable_identity
			assert drawable_oracle_before_stop[drawable_operation_start + 3].kind == 14
			assert drawable_app.backend.appkit.batch_autorelease_pool == unsafe { nil }
			assert drawable_app.backend.appkit.batch_autorelease_pool_ticket == 0
			assert drawable_app.backend.appkit.render_health == .ready
			assert drawable_app.renderer_is_usable()
			mut drawable_stop_error := ''
			drawable_app.stop() or { drawable_stop_error = err.msg() }
			drawable_first := native_appkit_backend_replay_snapshot_for_test(drawable_app)
			drawable_first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_assert_release_bijection_for_test(drawable_first.authority,
				drawable_first_oracle)
			drawable_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(drawable_first.authority,
				drawable_first_oracle, drawable_side_generation, [], true)
			mut drawable_replay_error := ''
			drawable_app.stop() or { drawable_replay_error = err.msg() }
			drawable_second := native_appkit_backend_replay_snapshot_for_test(drawable_app)
			drawable_second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(drawable_first, drawable_second)
			native_appkit_oracle_assert_equal_for_test(drawable_first_oracle,
				drawable_second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(drawable_first_side,
				native_appkit_side_effect_snapshot_for_test(drawable_side_generation))
			assert drawable_replay_error == drawable_stop_error
			assert drawable_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_effective_rejections_for_test() ! {
		$if darwin {
			native_appkit_exercise_effective_anchor_rejection_for_test()!
			native_appkit_exercise_effective_pool_rejection_for_test()!
			native_appkit_exercise_effective_drawable_rejection_for_test()!
		}
	}

	fn native_appkit_exercise_phase_b_registry_edges() ! {
		$if darwin {
			native_require_parent_watchdog_gate_for_test()!
			assert C.v_multiwindow_appkit_is_main_thread() != 0
			direct_start_error := native_appkit_exercise_backend_start_rejection_for_test(false)!
			probe_error := native_appkit_exercise_backend_start_rejection_for_test(true)!
			assert probe_error == direct_start_error
			native_appkit_exercise_renderer_probe_and_process_commit_for_test()!
			native_appkit_exercise_effective_rejections_for_test()!
			native_appkit_exercise_phase_b_registry_lifetime_edges_for_test()!
		}
	}

	fn native_appkit_exercise_phase_b_registry_lifetime_edges_for_test() ! {
		$if darwin {
			native_appkit_exercise_phase_b_registry_main_thread_releases_for_test()!
			native_appkit_exercise_phase_b_registry_nonmain_release_for_test()!
			native_appkit_exercise_phase_b_registry_window_rollback_for_test()!
		}
	}

	fn native_appkit_exercise_phase_b_registry_main_thread_releases_for_test() ! {
		$if darwin {
			mut authority := NativeOperationAuthority{}
			native_bind_renderer_authority_for_test(mut authority, 0x7201, 0x7203)!
			assert authority.arm_proof()
			mut backend := new_appkit_backend()
			unsafe {
				backend.native_operations = &authority
			}
			backend.render_health = .ready
			backend.start(false)!
			assert backend.started
			backend_ptr := voidptr(&backend)
			mut state := NativeAppKitRegistryMainThreadState{}
			native_appkit_prepare_phase_b_registry_main_thread_for_test(mut authority, mut backend,
				backend_ptr, mut state)!
			native_appkit_release_phase_b_registry_device_for_test(mut authority, mut backend,
				state)!
			native_appkit_release_phase_b_registry_window_for_test(mut authority, mut backend,
				state)!
			native_appkit_release_phase_b_registry_anchor_for_test(mut authority, mut backend, mut
				state)!
			native_appkit_stop_phase_b_registry_main_thread_for_test(mut authority, mut backend,
				state)!
		}
	}

	fn native_appkit_prepare_phase_b_registry_main_thread_for_test(mut authority NativeOperationAuthority, mut backend AppKitBackend, backend_ptr voidptr, mut state NativeAppKitRegistryMainThreadState) ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			state.registry_side_generation = native_appkit_side_effect_reset_for_test()
			seed := NativeOperationSeed{
				call_site: .renderer_start
				scope:     .renderer
			}
			state.rejection_context = NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        authority.renderer_attempt_token
				renderer_attempt_token: authority.renderer_attempt_token
				app_identity:           authority.app_identity
				domain:                 .metal
				operation:              .device_create
				call_site:              .renderer_start
				scope:                  .renderer
				ordinal:                authority.next_ordinal
			}
			assert authority.arm(state.rejection_context, NativePrimitiveEvidence{
				valid_mask: native_valid_handle
				handle:     0
			})
			mut rejection_error := ''
			backend.init_renderer() or { rejection_error = err.msg() }
			assert rejection_error == err_appkit_metal_device_failed
			assert backend.device == unsafe { nil }
			assert backend.device_ticket == 0
			assert authority.lifetime_tickets.len == 0
			assert !backend.retains_native_ownership()
			assert !authority.proof.plan[0].armed
			mut cleanup := authority.reserve_app_lifetime_ordinals(4)!
			null_ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup, .metal_device, seed)!
			state.wrong_thread_ticket = backend.reserve_appkit_lifetime_ticket(mut cleanup,
				.metal_device, seed)!
			state.window_probe_ticket = backend.reserve_appkit_lifetime_ticket(mut cleanup,
				.appkit_state, seed)!
			state.anchor_probe_ticket = backend.reserve_appkit_lifetime_ticket(mut cleanup,
				.appkit_state, seed)!
			null_release := backend.release_appkit_lifetime_identity(unsafe { nil }, null_ticket,
				.metal_device, 0, err_appkit_metal_device_failed)
			assert null_release.native_released
			assert null_release.ticket_retired
			assert null_release.value == unsafe { nil }
			assert null_release.ticket_id == 0
			assert native_lifetime_ticket_index_for_test(&authority, null_ticket) == -1
			probe_operation_count := native_appkit_lifetime_oracle_snapshot_for_test().len
			device_probe := native_appkit_create_release_probe_for_test(state.registry_side_generation,
				.device_root, probe_operation_count)!
			window_probe := native_appkit_create_release_probe_for_test(state.registry_side_generation,
				.window_root, probe_operation_count)!
			anchor_probe := native_appkit_create_release_probe_for_test(state.registry_side_generation,
				.anchor_root, probe_operation_count)!
			state.probes = [device_probe, window_probe, anchor_probe]
			authority.bind_lifetime_ticket(state.wrong_thread_ticket, device_probe.identity, 0)
			authority.bind_lifetime_ticket(state.window_probe_ticket, window_probe.identity, 0)
			authority.bind_lifetime_ticket(state.anchor_probe_ticket, anchor_probe.identity, 0)
			device_bound := native_phase_b_assert_bound_ticket_for_test(&authority,
				state.wrong_thread_ticket, .metal_device, device_probe.identity, 0, .app_lifetime)
			window_bound := native_phase_b_assert_bound_ticket_for_test(&authority,
				state.window_probe_ticket, .appkit_state, window_probe.identity, 0, .app_lifetime)
			_ = native_phase_b_assert_bound_ticket_for_test(&authority, state.anchor_probe_ticket,
				.appkit_state, anchor_probe.identity, 0, .app_lifetime)
			state.device_release_context = device_bound.context
			state.window_release_context = window_bound.context
			before_wrong_thread := native_proof_snapshot(&authority)
			before_wrong_thread_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			release_thread := spawn native_appkit_wrong_thread_release_for_test(backend_ptr,
				device_probe.identity, state.wrong_thread_ticket,
				NativeLifetimeReleaseKind.metal_device)
			wrong_thread := release_thread.wait()
			assert !wrong_thread.native_released
			assert !wrong_thread.ticket_retired
			assert wrong_thread.identity == device_probe.identity
			assert wrong_thread.ticket == state.wrong_thread_ticket
			native_proof_assert_snapshots_equal(before_wrong_thread,
				native_proof_snapshot(&authority))
			native_appkit_oracle_assert_equal_for_test(before_wrong_thread_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
		}
	}

	fn native_appkit_release_phase_b_registry_device_for_test(mut authority NativeOperationAuthority, mut backend AppKitBackend, state NativeAppKitRegistryMainThreadState) ! {
		$if darwin {
			device_probe := state.probes[0]
			assert authority.arm(state.device_release_context, NativePrimitiveEvidence{
				valid_mask:        native_valid_object_identity_0
				object_identity_0: 0
			})
			before_device_release := native_proof_snapshot(&authority)
			before_device_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			device_release := backend.release_appkit_lifetime_identity(native_pointer(device_probe.identity),
				state.wrong_thread_ticket, .metal_device, 0, err_appkit_metal_device_failed)
			assert device_release.native_released
			assert device_release.ticket_retired
			assert device_release.value == unsafe { nil }
			assert device_release.ticket_id == 0
			assert native_lifetime_ticket_index_for_test(&authority, state.wrong_thread_ticket) == -1
			after_device_release := native_proof_snapshot(&authority)
			device_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert device_oracle.len == before_device_oracle.len + 1
			assert after_device_release.next_ordinal == before_device_release.next_ordinal
			assert after_device_release.trace_len == before_device_release.trace_len + 6
			for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
				.effective_primitive, .acceptance, .health_latched, .authority_release] {
				entry := after_device_release.trace[before_device_release.trace_len + offset]
				assert entry.milestone == milestone
				assert native_operation_contexts_identical(entry.context,
					state.device_release_context)
			}
			assert after_device_release.trace[before_device_release.trace_len + 1].actual.object_identity_0 == device_probe.identity
			assert after_device_release.trace[before_device_release.trace_len + 2].effective.object_identity_0 == 0
			assert after_device_release.trace[before_device_release.trace_len + 3].result.disposition == .renderer_unavailable
			assert after_device_release.trace[before_device_release.trace_len + 4].health == .unavailable
			assert native_proof_result_equal(after_device_release.trace[
				before_device_release.trace_len + 5].result, after_device_release.trace[
				before_device_release.trace_len + 3].result)
			assert !authority.has_pending_native_plans()
			assert backend.poll_error.count(err_appkit_metal_device_failed) == 1
		}
	}

	fn native_appkit_release_phase_b_registry_window_for_test(mut authority NativeOperationAuthority, mut backend AppKitBackend, state NativeAppKitRegistryMainThreadState) ! {
		$if darwin {
			window_probe := state.probes[1]
			assert authority.arm(state.window_release_context, NativePrimitiveEvidence{
				valid_mask:        native_valid_object_identity_0
				object_identity_0: 0
			})
			before_window_release := native_proof_snapshot(&authority)
			before_window_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			window_release := backend.release_appkit_lifetime_identity(native_pointer(window_probe.identity),
				state.window_probe_ticket, .appkit_state, 0, err_appkit_destroy_window_failed)
			assert window_release.native_released
			assert window_release.ticket_retired
			assert window_release.value == unsafe { nil }
			assert window_release.ticket_id == 0
			assert native_lifetime_ticket_index_for_test(&authority, state.window_probe_ticket) == -1
			after_window_release := native_proof_snapshot(&authority)
			window_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert window_oracle.len == before_window_oracle.len + 1
			assert after_window_release.next_ordinal == before_window_release.next_ordinal
			assert after_window_release.trace_len == before_window_release.trace_len + 6
			for offset, milestone in [NativeOperationTraceMilestone.real_call, .actual_primitive,
				.effective_primitive, .acceptance, .health_latched, .authority_release] {
				entry := after_window_release.trace[before_window_release.trace_len + offset]
				assert entry.milestone == milestone
				assert native_operation_contexts_identical(entry.context,
					state.window_release_context)
			}
			assert after_window_release.trace[before_window_release.trace_len + 1].actual.object_identity_0 == window_probe.identity
			assert after_window_release.trace[before_window_release.trace_len + 2].effective.object_identity_0 == 0
			assert after_window_release.trace[before_window_release.trace_len + 3].result.disposition == .renderer_unavailable
			assert after_window_release.trace[before_window_release.trace_len + 4].health == .unavailable
			assert native_proof_result_equal(after_window_release.trace[
				before_window_release.trace_len + 5].result, after_window_release.trace[
				before_window_release.trace_len + 3].result)
			assert !authority.has_pending_native_plans()
			assert backend.poll_error.count(err_appkit_metal_device_failed) == 1
			assert backend.poll_error.count(err_appkit_destroy_window_failed) == 1
			assert backend.poll_error == '${err_appkit_metal_device_failed}; ${err_appkit_destroy_window_failed}'
			assert authority.lifetime_tickets.len == 1
		}
	}

	fn native_appkit_release_phase_b_registry_anchor_for_test(mut authority NativeOperationAuthority, mut backend AppKitBackend, mut state NativeAppKitRegistryMainThreadState) ! {
		$if darwin {
			anchor_probe := state.probes[2]
			anchor_release := backend.release_appkit_lifetime_identity(native_pointer(anchor_probe.identity),
				state.anchor_probe_ticket, .appkit_state, 0, err_appkit_destroy_window_failed)
			assert anchor_release.native_released
			assert anchor_release.ticket_retired
			assert anchor_release.value == unsafe { nil }
			assert anchor_release.ticket_id == 0
			assert authority.lifetime_tickets.len == 0
			records := native_appkit_lifetime_oracle_snapshot_for_test()
			mut creates := 0
			mut releases := 0
			for record in records {
				assert record.thread_identity == authority.owner_thread_identity
				if record.kind == 1 {
					creates++
				}
				if record.kind == 2 {
					assert record.identity == record.auxiliary_identity
					releases++
				}
			}
			assert creates == 1
			assert releases == 2
			proof := native_proof_snapshot(&authority)
			mut saw_rejected_actual := false
			mut saw_rejected_effective := false
			for index in 0 .. proof.trace_len {
				entry := proof.trace[index]
				if !native_operation_contexts_identical(entry.context, state.rejection_context) {
					continue
				}
				if entry.milestone == .actual_primitive {
					assert entry.actual.handle != 0
					saw_rejected_actual = true
				}
				if entry.milestone == .effective_primitive {
					assert entry.effective.handle == 0
					saw_rejected_effective = true
				}
			}
			assert saw_rejected_actual
			assert saw_rejected_effective
			native_appkit_assert_release_bijection_for_test(proof, records)
			state.registry_side = native_appkit_assert_side_effect_operation_bijection_for_test(proof,
				records, state.registry_side_generation, state.probes, true)
			assert !proof.trace_overflow
			assert !authority.has_pending_native_plans()
			assert !backend.retains_native_ownership()
		}
	}

	fn native_appkit_stop_phase_b_registry_main_thread_for_test(mut authority NativeOperationAuthority, mut backend AppKitBackend, state NativeAppKitRegistryMainThreadState) ! {
		$if darwin {
			first_diagnostics := backend.poll_error
			mut first_stop_error := ''
			backend.stop() or { first_stop_error = err.msg() }
			assert first_stop_error == first_diagnostics
			assert backend.poll_error == ''
			assert !backend.started
			assert !backend.retains_native_ownership()
			first_stop_proof := native_proof_snapshot(&authority)
			first_stop_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			first_stop_side :=
				native_appkit_side_effect_snapshot_for_test(state.registry_side_generation)
			mut replay_stop_error := ''
			backend.stop() or { replay_stop_error = err.msg() }
			assert replay_stop_error == ''
			assert backend.poll_error == ''
			native_proof_assert_snapshots_equal(first_stop_proof, native_proof_snapshot(&authority))
			native_appkit_oracle_assert_equal_for_test(first_stop_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_assert_side_effect_snapshots_equal_for_test(first_stop_side,
				native_appkit_side_effect_snapshot_for_test(state.registry_side_generation))
			assert authority.disarm_proof()
			native_appkit_assert_side_effect_snapshots_equal_for_test(state.registry_side,
				native_appkit_side_effect_snapshot_for_test(state.registry_side_generation))
		}
	}

	fn native_appkit_exercise_phase_b_registry_nonmain_release_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			nonmain_side_generation := native_appkit_side_effect_reset_for_test()
			nonmain_probe := native_appkit_create_release_probe_for_test(nonmain_side_generation,
				.device_root, 0)!
			nonmain_thread := spawn native_appkit_nonmain_owner_release_for_test(nonmain_probe.identity)
			nonmain_result := nonmain_thread.wait()
			assert !nonmain_result.native_released
			assert !nonmain_result.ticket_retired
			assert nonmain_result.identity == nonmain_probe.identity
			assert nonmain_result.ticket != 0
			assert native_appkit_lifetime_oracle_snapshot_for_test().len == 0
			mut nonmain_cleanup_authority := NativeOperationAuthority{}
			native_bind_renderer_authority_for_test(mut nonmain_cleanup_authority, 0x7221, 0x7223)!
			assert nonmain_cleanup_authority.arm_proof()
			mut nonmain_cleanup_backend := new_appkit_backend()
			unsafe {
				nonmain_cleanup_backend.native_operations = &nonmain_cleanup_authority
			}
			nonmain_cleanup_backend.render_health = .ready
			seed := NativeOperationSeed{
				call_site: .renderer_start
				scope:     .renderer
			}
			mut nonmain_cleanup_range := nonmain_cleanup_authority.reserve_app_lifetime_ordinals(1)!
			nonmain_cleanup_ticket := nonmain_cleanup_backend.reserve_appkit_lifetime_ticket(mut nonmain_cleanup_range,
				.metal_device, seed)!
			nonmain_cleanup_authority.bind_lifetime_ticket(nonmain_cleanup_ticket,
				nonmain_probe.identity, 0)
			nonmain_cleanup_release := nonmain_cleanup_backend.release_appkit_lifetime_identity(native_pointer(nonmain_probe.identity),
				nonmain_cleanup_ticket, .metal_device, 0, err_appkit_metal_device_failed)
			assert nonmain_cleanup_release.native_released
			assert nonmain_cleanup_release.ticket_retired
			assert nonmain_cleanup_release.value == unsafe { nil }
			assert nonmain_cleanup_release.ticket_id == 0
			nonmain_cleanup_proof := native_proof_snapshot(&nonmain_cleanup_authority)
			nonmain_cleanup_operations := native_appkit_lifetime_oracle_snapshot_for_test()
			_ = native_appkit_assert_side_effect_operation_bijection_for_test(nonmain_cleanup_proof,
				nonmain_cleanup_operations, nonmain_side_generation, [nonmain_probe], true)
			assert nonmain_cleanup_authority.disarm_proof()
		}
	}

	fn native_appkit_exercise_phase_b_registry_window_rollback_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			rejected_window_side_generation := native_appkit_side_effect_reset_for_test()
			mut rejected_window_app := native_phase_b_new_app_without_renderer_for_test(.appkit)!
			assert rejected_window_app.backend.native_operations.arm_proof()
			expected_window := WindowId{
				app_instance: rejected_window_app.instance_id
				slot:         0
				generation:   1
			}
			window_context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        rejected_window_app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: rejected_window_app.backend.native_operations.renderer_attempt_token
				app_identity:           rejected_window_app.instance_id
				presence_mask:          native_context_has_window | native_context_has_target_generation
				domain:                 .metal
				operation:              .window_surface_create
				call_site:              .window_prepare
				scope:                  .window_target
				window:                 expected_window
				target_generation:      1
				ordinal:                rejected_window_app.backend.native_operations.next_ordinal
			}
			assert rejected_window_app.backend.native_operations.arm(window_context, NativePrimitiveEvidence{
				valid_mask: native_valid_handle
				handle:     0
			})
			mut rejected_window_error := ''
			rejected_window_app.create_window(
				title:   'phase-B rejected retained AppKit state'
				width:   160
				height:  96
				visible: false
			) or { rejected_window_error = err.msg() }
			assert rejected_window_error == err_appkit_create_window_failed
			assert rejected_window_app.window_ids()!.len == 0
			assert rejected_window_app.backend.appkit.windows.len == 0
			assert rejected_window_app.backend.native_operations.lifetime_tickets.len == 0
			rejected_window_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			mut created_state_identity := u64(0)
			mut state_release_count := 0
			for record in rejected_window_oracle {
				if record.kind == 3 {
					assert created_state_identity == 0
					created_state_identity = record.output_identity
					assert created_state_identity != 0
				}
				if record.kind == 6 {
					state_release_count++
					assert record.identity == created_state_identity
					assert record.auxiliary_identity == created_state_identity
				}
			}
			assert created_state_identity != 0
			assert state_release_count == 1
			window_proof := native_proof_snapshot(&rejected_window_app.backend.native_operations)
			mut saw_actual_state := false
			mut saw_effective_null := false
			for index in 0 .. window_proof.trace_len {
				entry := window_proof.trace[index]
				if !native_operation_contexts_identical(entry.context, window_context) {
					continue
				}
				if entry.milestone == .actual_primitive {
					assert entry.actual.handle == created_state_identity
					saw_actual_state = true
				}
				if entry.milestone == .effective_primitive {
					assert entry.effective.handle == 0
					saw_effective_null = true
				}
			}
			assert saw_actual_state
			assert saw_effective_null
			native_appkit_assert_release_bijection_for_test(window_proof, rejected_window_oracle)
			assert !rejected_window_app.backend.native_operations.has_pending_native_plans()
			mut rejected_window_stop_error := ''
			rejected_window_app.stop() or { rejected_window_stop_error = err.msg() }
			window_first := native_appkit_backend_replay_snapshot_for_test(rejected_window_app)
			window_first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			window_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(window_first.authority,
				window_first_oracle, rejected_window_side_generation, [], true)
			mut rejected_window_replay_error := ''
			rejected_window_app.stop() or { rejected_window_replay_error = err.msg() }
			window_second := native_appkit_backend_replay_snapshot_for_test(rejected_window_app)
			window_second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(window_first, window_second)
			native_appkit_oracle_assert_equal_for_test(window_first_oracle, window_second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(window_first_side,
				native_appkit_side_effect_snapshot_for_test(rejected_window_side_generation))
			assert rejected_window_replay_error == rejected_window_stop_error
			assert rejected_window_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_oracle_index_for_test(records []NativeAppKitLifetimeOracleRecord, kind u64, identity u64, start int) int {
		for index in start .. records.len {
			if records[index].kind == kind && (identity == 0 || records[index].identity == identity) {
				return index
			}
		}
		return -1
	}

	fn native_appkit_assert_drawable_and_pool_chains_for_test(records []NativeAppKitLifetimeOracleRecord, require_drawable bool) {
		mut successful_drawable_count := 0
		mut open_drawables := map[u64]u64{}
		for record in records {
			if record.kind == 9 {
				if record.output_identity == 0 {
					assert record.valid_mask & native_valid_handle != 0
					continue
				}
				successful_drawable_count++
				assert record.output_identity != 0
				assert record.auxiliary_identity != 0
				assert record.output_identity !in open_drawables
				open_drawables[record.output_identity] = record.identity
				continue
			}
			if record.kind !in [u64(10), 11, 12] {
				continue
			}
			assert record.identity != 0
			assert record.identity in open_drawables
			assert record.parent_identity == open_drawables[record.identity]
			assert record.auxiliary_identity == record.identity
			assert record.valid_mask & native_valid_object_identity_0 != 0
			open_drawables.delete(record.identity)
		}
		assert open_drawables.len == 0
		if require_drawable {
			assert successful_drawable_count > 0
		}
		mut pool_count := 0
		for push_index, push in records {
			if push.kind != 13 {
				continue
			}
			pool_count++
			assert push.output_identity != 0
			pop_index := native_appkit_oracle_index_for_test(records, 14, push.output_identity,

				push_index + 1)
			assert pop_index > push_index
			assert records[pop_index].auxiliary_identity == push.output_identity
			assert records[pop_index].valid_mask & native_valid_object_identity_0 != 0
		}
		assert pool_count > 0
	}

	fn native_appkit_assert_ticket_release_delta_for_test(tickets []NativeLifetimeTicketProofSnapshot, records []NativeAppKitLifetimeOracleRecord) {
		mut expected := []NativeLifetimeTicketProofSnapshot{}
		for ticket in tickets {
			if ticket.state == .bound
				&& ticket.release_kind in [.metal_device, .appkit_state, .appkit_autorelease_pool, .metal_drawable] {
				expected << ticket
			}
		}
		mut release_records := []NativeAppKitLifetimeOracleRecord{}
		for record in records {
			if native_appkit_release_kind_for_oracle(record.kind) {
				assert record.identity != 0
				assert record.auxiliary_identity == record.identity
				assert record.valid_mask & native_valid_object_identity_0 != 0
				assert record.thread_identity != 0
				release_records << record
			}
		}
		assert records.len == release_records.len
		assert release_records.len == expected.len
		mut consumed := []bool{len: release_records.len}
		for ticket in expected {
			expected_kind := match ticket.release_kind {
				.metal_device { u64(2) }
				.appkit_state { u64(6) }
				.appkit_autorelease_pool { u64(14) }
				.metal_drawable { u64(12) }
				else { u64(0) }
			}

			assert expected_kind != 0
			mut matched := -1
			for index, record in release_records {
				if !consumed[index] && record.kind == expected_kind
					&& record.identity == ticket.native_identity
					&& record.auxiliary_identity == ticket.native_identity {
					matched = index
					break
				}
			}
			assert matched >= 0
			consumed[matched] = true
		}
		for matched in consumed {
			assert matched
		}
	}

	fn native_appkit_assert_one_window_cleanup_oracle_for_test(ownership NativeAppKitBackendReplaySnapshot, records []NativeAppKitLifetimeOracleRecord, owner_thread u64) {
		assert owner_thread != 0
		assert ownership.windows.len == 1
		window_identity := ownership.windows[0].state
		anchor_identity := ownership.anchor_state
		device_identity := ownership.device
		assert window_identity != 0
		assert anchor_identity != 0
		assert device_identity != 0
		assert window_identity != anchor_identity
		assert window_identity != device_identity
		assert anchor_identity != device_identity
		assert ownership.batch_autorelease_pool == 0
		assert records.len == 7
		for index, record in records {
			assert record.thread_identity == owner_thread
			if index > 0 {
				assert record.sequence == records[index - 1].sequence + 1
			}
		}

		window_destroy := records[0]
		assert window_destroy.kind == 5
		assert window_destroy.identity == window_identity
		assert window_destroy.parent_identity == 0
		assert window_destroy.output_identity == 0
		assert window_destroy.auxiliary_identity == 0
		assert window_destroy.auxiliary_identity_1 == 0
		assert window_destroy.auxiliary_identity_2 == 0
		assert window_destroy.valid_mask == 0

		window_release := records[1]
		assert window_release.kind == 6
		assert window_release.identity == window_identity
		assert window_release.parent_identity == 0
		assert window_release.output_identity == 0
		assert window_release.auxiliary_identity == window_identity
		assert window_release.auxiliary_identity_1 == 0
		assert window_release.auxiliary_identity_2 == 0
		assert window_release.valid_mask == native_valid_object_identity_0

		push := records[2]
		pop := records[3]
		assert push.kind == 13
		assert push.identity == 0
		assert push.parent_identity == 0
		assert push.output_identity != 0
		assert push.auxiliary_identity == 0
		assert push.auxiliary_identity_1 == 0
		assert push.auxiliary_identity_2 == 0
		assert push.valid_mask == native_valid_handle
		assert pop.sequence == push.sequence + 1
		assert pop.kind == 14
		assert pop.identity == push.output_identity
		assert pop.parent_identity == 0
		assert pop.output_identity == 0
		assert pop.auxiliary_identity == push.output_identity
		assert pop.auxiliary_identity_1 == 0
		assert pop.auxiliary_identity_2 == 0
		assert pop.valid_mask == native_valid_object_identity_0

		anchor_destroy := records[4]
		assert anchor_destroy.kind == 8
		assert anchor_destroy.identity == anchor_identity
		assert anchor_destroy.parent_identity == 0
		assert anchor_destroy.output_identity == 0
		assert anchor_destroy.auxiliary_identity == 0
		assert anchor_destroy.auxiliary_identity_1 == 0
		assert anchor_destroy.auxiliary_identity_2 == 0
		assert anchor_destroy.valid_mask == 0

		anchor_release := records[5]
		assert anchor_release.kind == 6
		assert anchor_release.identity == anchor_identity
		assert anchor_release.parent_identity == 0
		assert anchor_release.output_identity == 0
		assert anchor_release.auxiliary_identity == anchor_identity
		assert anchor_release.auxiliary_identity_1 == 0
		assert anchor_release.auxiliary_identity_2 == 0
		assert anchor_release.valid_mask == native_valid_object_identity_0

		device_release := records[6]
		assert device_release.kind == 2
		assert device_release.identity == device_identity
		assert device_release.parent_identity == 0
		assert device_release.output_identity == 0
		assert device_release.auxiliary_identity == device_identity
		assert device_release.auxiliary_identity_1 == 0
		assert device_release.auxiliary_identity_2 == 0
		assert device_release.valid_mask == native_valid_object_identity_0
	}

	fn native_appkit_assert_anchor_only_cleanup_oracle_for_test(ownership NativeAppKitBackendReplaySnapshot, records []NativeAppKitLifetimeOracleRecord, owner_thread u64) {
		assert owner_thread != 0
		assert ownership.windows.len == 0
		anchor_identity := ownership.anchor_state
		device_identity := ownership.device
		assert anchor_identity != 0
		assert device_identity != 0
		assert anchor_identity != device_identity
		assert ownership.batch_autorelease_pool == 0
		assert records.len == 5
		for index, record in records {
			assert record.thread_identity == owner_thread
			if index > 0 {
				assert record.sequence == records[index - 1].sequence + 1
			}
		}

		push := records[0]
		pop := records[1]
		assert push.kind == 13
		assert push.identity == 0
		assert push.parent_identity == 0
		assert push.output_identity != 0
		assert push.auxiliary_identity == 0
		assert push.auxiliary_identity_1 == 0
		assert push.auxiliary_identity_2 == 0
		assert push.valid_mask == native_valid_handle
		assert pop.sequence == push.sequence + 1
		assert pop.kind == 14
		assert pop.identity == push.output_identity
		assert pop.parent_identity == 0
		assert pop.output_identity == 0
		assert pop.auxiliary_identity == push.output_identity
		assert pop.auxiliary_identity_1 == 0
		assert pop.auxiliary_identity_2 == 0
		assert pop.valid_mask == native_valid_object_identity_0

		anchor_destroy := records[2]
		assert anchor_destroy.kind == 8
		assert anchor_destroy.identity == anchor_identity
		assert anchor_destroy.parent_identity == 0
		assert anchor_destroy.output_identity == 0
		assert anchor_destroy.auxiliary_identity == 0
		assert anchor_destroy.auxiliary_identity_1 == 0
		assert anchor_destroy.auxiliary_identity_2 == 0
		assert anchor_destroy.valid_mask == 0

		anchor_release := records[3]
		assert anchor_release.kind == 6
		assert anchor_release.identity == anchor_identity
		assert anchor_release.parent_identity == 0
		assert anchor_release.output_identity == 0
		assert anchor_release.auxiliary_identity == anchor_identity
		assert anchor_release.auxiliary_identity_1 == 0
		assert anchor_release.auxiliary_identity_2 == 0
		assert anchor_release.valid_mask == native_valid_object_identity_0

		device_release := records[4]
		assert device_release.kind == 2
		assert device_release.identity == device_identity
		assert device_release.parent_identity == 0
		assert device_release.output_identity == 0
		assert device_release.auxiliary_identity == device_identity
		assert device_release.auxiliary_identity_1 == 0
		assert device_release.auxiliary_identity_2 == 0
		assert device_release.valid_mask == native_valid_object_identity_0
	}

	fn native_appkit_assert_ticket_release_delta_with_temporary_pool_for_test(tickets []NativeLifetimeTicketProofSnapshot, records []NativeAppKitLifetimeOracleRecord, ownership NativeAppKitBackendReplaySnapshot, owner_thread u64) {
		native_appkit_assert_one_window_cleanup_oracle_for_test(ownership, records, owner_thread)
		assert tickets.len == 3
		native_appkit_assert_ticket_release_delta_for_test(tickets, [
			records[1],
			records[5],
			records[6],
		])
	}

	fn native_appkit_assert_cleanup_order_for_test(ownership NativeAppKitBackendReplaySnapshot, records []NativeAppKitLifetimeOracleRecord) {
		mut transient_pool_identity := u64(0)
		mut transient_pool_push_index := -1
		mut transient_pool_pop_index := -1
		for index, record in records {
			if record.kind == 13 {
				assert transient_pool_push_index == -1
				assert ownership.batch_autorelease_pool == 0
				assert record.identity == 0
				assert record.output_identity != 0
				assert record.valid_mask & native_valid_handle != 0
				transient_pool_identity = record.output_identity
				transient_pool_push_index = index
				continue
			}
			if record.kind == 14 && transient_pool_identity != 0
				&& record.identity == transient_pool_identity {
				assert transient_pool_pop_index == -1
				assert index > transient_pool_push_index
				assert record.parent_identity == 0
				assert record.output_identity == 0
				assert record.auxiliary_identity == transient_pool_identity
				assert record.valid_mask & native_valid_object_identity_0 != 0
				transient_pool_pop_index = index
			}
		}
		if transient_pool_push_index >= 0 {
			assert transient_pool_pop_index > transient_pool_push_index
			for index, record in records {
				if record.kind in [u64(2), 5, 6, 8] {
					assert index > transient_pool_pop_index
				}
			}
		} else {
			assert transient_pool_identity == 0
			assert transient_pool_pop_index == -1
		}
		mut expected := []u64{}
		for window in ownership.windows {
			if window.active_drawable != 0 {
				expected << window.active_drawable
			}
		}
		if ownership.active_anchor_drawable != 0 {
			expected << ownership.active_anchor_drawable
		}
		if ownership.batch_autorelease_pool != 0 {
			expected << ownership.batch_autorelease_pool
		}
		if transient_pool_identity != 0 {
			expected << transient_pool_identity
		}
		for window in ownership.windows {
			if window.state != 0 {
				expected << window.state
			}
		}
		if ownership.anchor_state != 0 {
			expected << ownership.anchor_state
		}
		if ownership.device != 0 {
			expected << ownership.device
		}
		mut actual := []u64{}
		for record in records {
			if native_appkit_release_kind_for_oracle(record.kind) {
				actual << record.identity
			}
		}
		assert actual == expected
	}

	fn native_appkit_assert_anchor_drawable_release_context_for_test(snapshot NativeAuthorityProofSnapshot, drawable_identity u64, anchor_state_identity u64) {
		mut matches := 0
		for index in 0 .. snapshot.trace_len {
			entry := snapshot.trace[index]
			if entry.milestone != .authority_release || entry.context.domain != .metal
				|| entry.context.operation != .clear_state
				|| entry.context.target_identity != drawable_identity {
				continue
			}
			matches++
			assert entry.context.authority_scope == .renderer_attempt
			assert entry.context.authority_token == snapshot.renderer_attempt_token
			assert entry.context.renderer_attempt_token == snapshot.renderer_attempt_token
			assert entry.context.app_identity == snapshot.app_identity
			assert entry.context.presence_mask == native_context_has_target_identity
			assert entry.context.call_site == .anchor_prepare
			assert entry.context.scope == .anchor
			assert entry.context.window == WindowId{}
			assert entry.context.target_generation == 0
			assert entry.context.batch_epoch == 0
			assert entry.context.window_lease_epoch == 0
			assert entry.context.target_lease_epoch == 0
			assert entry.result.context.target_identity == drawable_identity
		}
		assert matches == 1
		assert anchor_state_identity != 0
	}

	fn native_appkit_finish_physical_nil_replay_for_test(mut app App, side_generation u64, has_window bool) ! {
		before_stop := native_appkit_backend_replay_snapshot_for_test(app)
		before_stop_oracle_len := native_appkit_lifetime_oracle_snapshot_for_test().len
		app.stop()!
		first := native_appkit_backend_replay_snapshot_for_test(app)
		first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
		assert first.authority.registry.tickets.len == 0
		assert !first.authority.trace_overflow
		native_appkit_assert_drawable_and_pool_chains_for_test(first_oracle, true)
		native_appkit_assert_release_bijection_for_test(first.authority, first_oracle)
		if has_window {
			native_appkit_assert_one_window_cleanup_oracle_for_test(before_stop,
				first_oracle[before_stop_oracle_len..], first.authority.owner_thread_identity)
		} else {
			native_appkit_assert_anchor_only_cleanup_oracle_for_test(before_stop,
				first_oracle[before_stop_oracle_len..], first.authority.owner_thread_identity)
		}
		first_side := native_appkit_assert_side_effect_operation_bijection_for_test(first.authority,
			first_oracle, side_generation, [], true)
		app.stop()!
		second := native_appkit_backend_replay_snapshot_for_test(app)
		second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
		native_appkit_backend_assert_replay_equal_for_test(first, second)
		native_appkit_oracle_assert_equal_for_test(first_oracle, second_oracle)
		native_appkit_assert_side_effect_snapshots_equal_for_test(first_side,
			native_appkit_side_effect_snapshot_for_test(side_generation))
		assert app.backend.native_operations.disarm_proof()
	}

	fn native_appkit_exercise_physical_nil_window_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			side_generation := native_appkit_side_effect_reset_for_test()
			mut app := native_runtime_new_app_for_test(.appkit)!
			window := native_runtime_new_window_for_test(mut app,
				'AppKit physical nil window drawable')!
			assert app.backend.native_operations.arm_proof()
			owner_thread := app.backend.native_operations.owner_thread_identity
			device_identity := native_identity(app.backend.appkit.device)
			index := native_appkit_window_record_index_for_test(app, window)!
			state_identity := native_identity(app.backend.appkit.windows[index].state)
			original_layer_identity := native_appkit_layer_identity_for_state_for_test(side_generation,
				state_identity, device_identity)
			assert owner_thread != 0
			assert device_identity != 0
			assert state_identity != 0
			assert original_layer_identity != 0

			sokol_generation := native_install_sokol_trace_for_test()!
			batch := RenderBatchLease{
				app_instance: app.instance_id
				epoch:        0x7601
			}
			batch_seed := NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .anchor_prepare
				scope:         .batch
				batch_epoch:   batch.epoch
			}
			app.backend.appkit.begin_render_batch(batch_seed)!
			_ = native_appkit_assert_pool_ticket_context_for_test(&app.backend.native_operations,
				app.backend.appkit.batch_autorelease_pool_ticket,
				native_identity(app.backend.appkit.batch_autorelease_pool), batch)

			install_authority := native_proof_snapshot(&app.backend.native_operations)
			install_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			install_side := native_appkit_side_effect_snapshot_for_test(side_generation)
			layer_lease := C.v_multiwindow_appkit_native_proof_install_physical_nil_drawable(app.backend.appkit.windows[index].state,
				native_pointer(original_layer_identity), app.backend.appkit.device, owner_thread)
			assert layer_lease != unsafe { nil }
			native_proof_assert_snapshots_equal(install_authority,
				native_proof_snapshot(&app.backend.native_operations))
			native_appkit_oracle_assert_equal_for_test(install_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_assert_side_effect_snapshots_equal_for_test(install_side,
				native_appkit_side_effect_snapshot_for_test(side_generation))

			candidate := app.render_window_snapshot(window)!
			attempt := NativeTargetAttempt{
				batch_epoch:        batch.epoch
				window_lease_epoch: 0x7602
				target_lease_epoch: 0x7603
			}
			prepared := app.backend.appkit.begin_render(window, candidate, attempt)
			assert prepared.outcome.succeeded()
			assert prepared.frame.backend_lease != 0
			physical_authority_before := native_proof_snapshot(&app.backend.native_operations)
			physical_oracle_before := native_appkit_lifetime_oracle_snapshot_for_test()
			physical_side_before := native_appkit_side_effect_snapshot_for_test(side_generation)
			physical_context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
				app_identity:           app.instance_id
				presence_mask:          native_context_window_target_fields | native_context_has_target_identity
				domain:                 .metal
				operation:              .drawable_acquire
				call_site:              .window_activate
				scope:                  .window_target
				window:                 window
				target_generation:      candidate.target.target_identity
				target_identity:        state_identity
				batch_epoch:            attempt.batch_epoch
				window_lease_epoch:     attempt.window_lease_epoch
				target_lease_epoch:     attempt.target_lease_epoch
				ordinal:                physical_authority_before.next_ordinal
			}
			failed := app.backend.appkit.activate_render_frame(prepared.frame)
			assert !failed.outcome.succeeded()
			assert failed.outcome.disposition == .transient
			assert failed.outcome.local_validation == .null_output
			assert app.backend.appkit.render_health == .ready
			failed_record := app.backend.appkit.windows[index]
			assert failed_record.frame_active
			assert failed_record.active_drawable == unsafe { nil }
			assert failed_record.active_drawable_ticket == 0
			physical_authority_after := native_proof_snapshot(&app.backend.native_operations)
			physical_oracle_after := native_appkit_lifetime_oracle_snapshot_for_test()
			physical_side_after := native_appkit_side_effect_snapshot_for_test(side_generation)
			physical_layer_identity := native_appkit_assert_physical_nil_side_effect_delta_for_test(physical_side_before,
				physical_side_after, side_generation, state_identity, original_layer_identity,
				owner_thread)
			assert physical_layer_identity != original_layer_identity
			native_appkit_assert_physical_nil_oracle_delta_for_test(physical_oracle_before,
				physical_oracle_after, state_identity, device_identity, owner_thread)
			native_appkit_assert_physical_nil_authority_delta_for_test(physical_authority_before,
				physical_authority_after, physical_context)
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])

			abort_authority := native_proof_snapshot(&app.backend.native_operations)
			abort_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			abort_side := native_appkit_side_effect_snapshot_for_test(side_generation)
			app.backend.appkit.abort_render(prepared.frame)!
			assert !app.backend.appkit.windows[index].frame_active
			native_proof_assert_snapshots_equal(abort_authority,
				native_proof_snapshot(&app.backend.native_operations))
			native_appkit_oracle_assert_equal_for_test(abort_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_assert_side_effect_snapshots_equal_for_test(abort_side,
				native_appkit_side_effect_snapshot_for_test(side_generation))

			restore_authority := native_proof_snapshot(&app.backend.native_operations)
			restore_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			restore_side := native_appkit_side_effect_snapshot_for_test(side_generation)
			assert C.v_multiwindow_appkit_native_proof_restore_physical_nil_drawable(layer_lease,
				owner_thread) == 1
			native_proof_assert_snapshots_equal(restore_authority,
				native_proof_snapshot(&app.backend.native_operations))
			native_appkit_oracle_assert_equal_for_test(restore_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_assert_side_effect_snapshots_equal_for_test(restore_side,
				native_appkit_side_effect_snapshot_for_test(side_generation))

			retry_oracle_before := native_appkit_lifetime_oracle_snapshot_for_test()
			retry_side_before := native_appkit_side_effect_snapshot_for_test(side_generation)
			retry_prepared := app.backend.appkit.begin_render(window, candidate, attempt)
			assert retry_prepared.outcome.succeeded()
			retry := app.backend.appkit.activate_render_frame(retry_prepared.frame)
			assert retry.outcome.succeeded()
			assert retry.frame.acquired
			retry_drawable := native_identity(retry.frame.swapchain.metal.current_drawable)
			assert retry_drawable != 0
			retry_record := app.backend.appkit.windows[index]
			retry_target := RenderTargetLease{
				app_instance: app.instance_id
				batch_epoch:  attempt.batch_epoch
				window_epoch: attempt.window_lease_epoch
				target_epoch: attempt.target_lease_epoch
				window:       window
			}
			retry_ticket := native_appkit_assert_window_drawable_ticket_context_for_test(&app.backend.native_operations,
				retry_record.active_drawable_ticket, retry_drawable, state_identity,
				candidate.target.target_identity, batch, retry_target)
			app.backend.appkit.abort_render(retry.frame)!
			assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
				retry_ticket.ticket_id) == -1
			assert !app.backend.appkit.windows[index].frame_active
			retry_oracle_after := native_appkit_lifetime_oracle_snapshot_for_test()
			retry_side_after := native_appkit_side_effect_snapshot_for_test(side_generation)
			observed_retry_drawable := native_appkit_assert_restored_layer_retry_for_test(retry_side_before,
				retry_side_after, side_generation, state_identity, original_layer_identity,
				owner_thread)
			assert observed_retry_drawable == retry_drawable
			native_appkit_assert_restored_retry_oracle_delta_for_test(retry_oracle_before,
				retry_oracle_after, state_identity, device_identity, retry_drawable, owner_thread)

			app.backend.appkit.end_render_batch(NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .window_finalize
				scope:         .batch
				batch_epoch:   batch.epoch
			})!
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
			native_uninstall_sokol_trace_for_test(sokol_generation)!
			native_appkit_finish_physical_nil_replay_for_test(mut app, side_generation, true)!
		}
	}

	fn native_appkit_exercise_physical_nil_anchor_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			side_generation := native_appkit_side_effect_reset_for_test()
			mut app := native_runtime_new_app_for_test(.appkit)!
			assert app.backend.native_operations.arm_proof()
			owner_thread := app.backend.native_operations.owner_thread_identity
			device_identity := native_identity(app.backend.appkit.device)
			state_identity := native_identity(app.backend.appkit.anchor_state)
			original_layer_identity := native_appkit_layer_identity_for_state_for_test(side_generation,
				state_identity, device_identity)
			assert owner_thread != 0
			assert device_identity != 0
			assert state_identity != 0
			assert original_layer_identity != 0

			sokol_generation := native_install_sokol_trace_for_test()!
			batch := RenderBatchLease{
				app_instance: app.instance_id
				epoch:        0x7701
			}
			batch_seed := NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .anchor_prepare
				scope:         .batch
				batch_epoch:   batch.epoch
			}
			app.backend.appkit.begin_render_batch(batch_seed)!
			_ = native_appkit_assert_pool_ticket_context_for_test(&app.backend.native_operations,
				app.backend.appkit.batch_autorelease_pool_ticket,
				native_identity(app.backend.appkit.batch_autorelease_pool), batch)

			install_authority := native_proof_snapshot(&app.backend.native_operations)
			install_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			install_side := native_appkit_side_effect_snapshot_for_test(side_generation)
			layer_lease := C.v_multiwindow_appkit_native_proof_install_physical_nil_drawable(app.backend.appkit.anchor_state,
				native_pointer(original_layer_identity), app.backend.appkit.device, owner_thread)
			assert layer_lease != unsafe { nil }
			native_proof_assert_snapshots_equal(install_authority,
				native_proof_snapshot(&app.backend.native_operations))
			native_appkit_oracle_assert_equal_for_test(install_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_assert_side_effect_snapshots_equal_for_test(install_side,
				native_appkit_side_effect_snapshot_for_test(side_generation))

			physical_authority_before := native_proof_snapshot(&app.backend.native_operations)
			physical_oracle_before := native_appkit_lifetime_oracle_snapshot_for_test()
			physical_side_before := native_appkit_side_effect_snapshot_for_test(side_generation)
			physical_context := NativeOperationContext{
				authority_scope:        .renderer_attempt
				authority_token:        app.backend.native_operations.renderer_attempt_token
				renderer_attempt_token: app.backend.native_operations.renderer_attempt_token
				app_identity:           app.instance_id
				presence_mask:          native_context_has_target_identity
				domain:                 .metal
				operation:              .drawable_acquire
				call_site:              .anchor_prepare
				scope:                  .anchor
				target_identity:        state_identity
				ordinal:                physical_authority_before.next_ordinal
			}
			mut failed_error := ''
			_ = app.backend.appkit.begin_renderer_anchor() or {
				failed_error = err.msg()
				RenderFrame{}
			}
			assert failed_error == err_render_anchor_failed
			assert app.backend.appkit.render_health == .ready
			assert app.backend.appkit.active_anchor_lease == 0
			assert app.backend.appkit.active_anchor_drawable == unsafe { nil }
			assert app.backend.appkit.active_anchor_drawable_ticket == 0
			physical_authority_after := native_proof_snapshot(&app.backend.native_operations)
			physical_oracle_after := native_appkit_lifetime_oracle_snapshot_for_test()
			physical_side_after := native_appkit_side_effect_snapshot_for_test(side_generation)
			physical_layer_identity := native_appkit_assert_physical_nil_side_effect_delta_for_test(physical_side_before,
				physical_side_after, side_generation, state_identity, original_layer_identity,
				owner_thread)
			assert physical_layer_identity != original_layer_identity
			native_appkit_assert_physical_nil_oracle_delta_for_test(physical_oracle_before,
				physical_oracle_after, state_identity, device_identity, owner_thread)
			native_appkit_assert_physical_nil_authority_delta_for_test(physical_authority_before,
				physical_authority_after, physical_context)
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])

			restore_authority := native_proof_snapshot(&app.backend.native_operations)
			restore_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			restore_side := native_appkit_side_effect_snapshot_for_test(side_generation)
			assert C.v_multiwindow_appkit_native_proof_restore_physical_nil_drawable(layer_lease,
				owner_thread) == 1
			native_proof_assert_snapshots_equal(restore_authority,
				native_proof_snapshot(&app.backend.native_operations))
			native_appkit_oracle_assert_equal_for_test(restore_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_assert_side_effect_snapshots_equal_for_test(restore_side,
				native_appkit_side_effect_snapshot_for_test(side_generation))

			retry_oracle_before := native_appkit_lifetime_oracle_snapshot_for_test()
			retry_side_before := native_appkit_side_effect_snapshot_for_test(side_generation)
			retry := app.backend.appkit.begin_renderer_anchor()!
			retry_drawable := native_identity(retry.swapchain.metal.current_drawable)
			assert retry_drawable != 0
			retry_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				app.backend.appkit.active_anchor_drawable_ticket)!
			native_appkit_assert_anchor_drawable_ticket_context_for_test(&app.backend.native_operations,
				retry_ticket, retry_drawable, state_identity)
			app.backend.appkit.abort_renderer_anchor(retry)!
			assert native_lifetime_ticket_index_for_test(&app.backend.native_operations,
				retry_ticket.ticket_id) == -1
			assert app.backend.appkit.active_anchor_lease == 0
			assert app.backend.appkit.active_anchor_drawable == unsafe { nil }
			assert app.backend.appkit.active_anchor_drawable_ticket == 0
			retry_oracle_after := native_appkit_lifetime_oracle_snapshot_for_test()
			retry_side_after := native_appkit_side_effect_snapshot_for_test(side_generation)
			observed_retry_drawable := native_appkit_assert_restored_layer_retry_for_test(retry_side_before,
				retry_side_after, side_generation, state_identity, original_layer_identity,
				owner_thread)
			assert observed_retry_drawable == retry_drawable
			native_appkit_assert_restored_retry_oracle_delta_for_test(retry_oracle_before,
				retry_oracle_after, state_identity, device_identity, retry_drawable, owner_thread)

			app.backend.appkit.end_render_batch(NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .window_finalize
				scope:         .batch
				batch_epoch:   batch.epoch
			})!
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [])
			native_uninstall_sokol_trace_for_test(sokol_generation)!
			native_appkit_finish_physical_nil_replay_for_test(mut app, side_generation, false)!
		}
	}

	fn native_appkit_exercise_anchor_drawable_chain_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			side_generation := native_appkit_side_effect_reset_for_test()
			mut app := native_runtime_new_app_for_test(.appkit)!
			assert app.backend.native_operations.arm_proof()
			direct_oracle_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			anchor_state_identity := native_identity(app.backend.appkit.anchor_state)
			assert anchor_state_identity != 0
			_ = native_phase_b_assert_bound_ticket_for_test(&app.backend.native_operations,
				app.backend.appkit.anchor_state_ticket, .appkit_state, anchor_state_identity, 0,
				.renderer_attempt)
			batch := RenderBatchLease{
				app_instance: app.instance_id
				epoch:        0x7401
			}
			batch_seed := NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .anchor_prepare
				scope:         .batch
				batch_epoch:   batch.epoch
			}
			app.backend.appkit.begin_render_batch(batch_seed)!
			_ = native_appkit_assert_pool_ticket_context_for_test(&app.backend.native_operations,
				app.backend.appkit.batch_autorelease_pool_ticket,
				native_identity(app.backend.appkit.batch_autorelease_pool), batch)
			frame := app.backend.appkit.begin_renderer_anchor()!
			drawable_identity := native_identity(frame.swapchain.metal.current_drawable)
			depth_identity := native_identity(frame.swapchain.metal.depth_stencil_texture)
			assert drawable_identity != 0
			assert depth_identity != 0
			anchor_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				app.backend.appkit.active_anchor_drawable_ticket)!
			native_appkit_assert_anchor_drawable_ticket_context_for_test(&app.backend.native_operations,
				anchor_ticket, drawable_identity, anchor_state_identity)
			for ticket in app.backend.native_operations.lifetime_tickets {
				assert ticket.native_identity != depth_identity
			}
			app.backend.appkit.end_renderer_anchor(frame)!
			assert app.backend.appkit.active_anchor_lease == 0
			assert app.backend.appkit.active_anchor_drawable == unsafe { nil }
			assert app.backend.appkit.active_anchor_drawable_ticket == 0
			app.backend.appkit.end_render_batch(NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .window_finalize
				scope:         .batch
				batch_epoch:   batch.epoch
			})!
			direct_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert direct_oracle.len == direct_oracle_start + 4
			assert direct_oracle[direct_oracle_start].kind == 13
			assert direct_oracle[direct_oracle_start + 1].kind == 9
			assert direct_oracle[direct_oracle_start + 1].identity == anchor_state_identity
			assert direct_oracle[direct_oracle_start + 1].output_identity == drawable_identity
			assert direct_oracle[direct_oracle_start + 1].auxiliary_identity == depth_identity
			assert direct_oracle[direct_oracle_start + 2].kind == 10
			assert direct_oracle[direct_oracle_start + 2].identity == drawable_identity
			assert direct_oracle[direct_oracle_start + 2].parent_identity == anchor_state_identity
			assert direct_oracle[direct_oracle_start + 3].kind == 14
			direct_proof := native_proof_snapshot(&app.backend.native_operations)
			native_appkit_assert_anchor_drawable_release_context_for_test(direct_proof,
				drawable_identity, anchor_state_identity)
			native_appkit_assert_drawable_and_pool_chains_for_test(direct_oracle, true)
			native_appkit_assert_release_bijection_for_test(direct_proof, direct_oracle)

			abort_oracle_start := direct_oracle.len
			abort_batch := RenderBatchLease{
				app_instance: app.instance_id
				epoch:        0x7402
			}
			abort_batch_seed := NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .anchor_prepare
				scope:         .batch
				batch_epoch:   abort_batch.epoch
			}
			app.backend.appkit.begin_render_batch(abort_batch_seed)!
			_ = native_appkit_assert_pool_ticket_context_for_test(&app.backend.native_operations,
				app.backend.appkit.batch_autorelease_pool_ticket,
				native_identity(app.backend.appkit.batch_autorelease_pool), abort_batch)
			abort_frame := app.backend.appkit.begin_renderer_anchor()!
			abort_drawable := native_identity(abort_frame.swapchain.metal.current_drawable)
			abort_depth := native_identity(abort_frame.swapchain.metal.depth_stencil_texture)
			assert abort_drawable != 0
			assert abort_depth != 0
			abort_ticket := native_lifetime_ticket_snapshot_for_test(&app.backend.native_operations,
				app.backend.appkit.active_anchor_drawable_ticket)!
			native_appkit_assert_anchor_drawable_ticket_context_for_test(&app.backend.native_operations,
				abort_ticket, abort_drawable, anchor_state_identity)
			for ticket in app.backend.native_operations.lifetime_tickets {
				assert ticket.native_identity != abort_depth
			}
			app.backend.appkit.abort_renderer_anchor(abort_frame)!
			app.backend.appkit.end_render_batch(NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .window_finalize
				scope:         .batch
				batch_epoch:   abort_batch.epoch
			})!
			abort_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert abort_oracle.len == abort_oracle_start + 4
			assert abort_oracle[abort_oracle_start].kind == 13
			assert abort_oracle[abort_oracle_start + 1].kind == 9
			assert abort_oracle[abort_oracle_start + 1].output_identity == abort_drawable
			assert abort_oracle[abort_oracle_start + 1].auxiliary_identity == abort_depth
			assert abort_oracle[abort_oracle_start + 2].kind == 11
			assert abort_oracle[abort_oracle_start + 2].identity == abort_drawable
			assert abort_oracle[abort_oracle_start + 3].kind == 14

			public_oracle_start := abort_oracle.len
			sokol_generation := native_install_sokol_trace_for_test()!
			public_outcome := app.with_scheduled_render_batch(fn [mut app] (public_batch RenderBatchLease, _ []RenderWindowSnapshot) ! {
				_ = native_appkit_assert_pool_ticket_context_for_test(&app.backend.native_operations,
					app.backend.appkit.batch_autorelease_pool_ticket,
					native_identity(app.backend.appkit.batch_autorelease_pool), public_batch)
				app.note_render_gpu_work(public_batch)!
			})!
			assert public_outcome.error == ''
			assert public_outcome.committed
			assert public_outcome.had_gpu_work
			assert public_outcome.completed_user_passes == 0
			assert public_outcome.finalized_submissions == 0
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [
				multiwindow_sokol_trace.Operation.begin_swapchain_pass,
				.end_pass,
				.commit,
			])
			native_uninstall_sokol_trace_for_test(sokol_generation)!
			all_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert all_oracle.len == public_oracle_start + 4
			public_drawable_identity := all_oracle[public_oracle_start + 1].output_identity
			public_depth_identity := all_oracle[public_oracle_start + 1].auxiliary_identity
			assert all_oracle[public_oracle_start].kind == 13
			assert all_oracle[public_oracle_start + 1].kind == 9
			assert all_oracle[public_oracle_start + 1].identity == anchor_state_identity
			assert public_drawable_identity != 0
			assert public_depth_identity != 0
			assert all_oracle[public_oracle_start + 2].kind == 10
			assert all_oracle[public_oracle_start + 2].identity == public_drawable_identity
			assert all_oracle[public_oracle_start + 3].kind == 14
			for ticket in app.backend.native_operations.lifetime_tickets {
				assert ticket.native_identity != public_depth_identity
			}
			before_stop := native_appkit_backend_replay_snapshot_for_test(app)
			app.stop()!
			first := native_appkit_backend_replay_snapshot_for_test(app)
			first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert !first.authority.trace_overflow
			assert first.authority.registry.tickets.len == 0
			native_appkit_assert_drawable_and_pool_chains_for_test(first_oracle, true)
			native_appkit_assert_release_bijection_for_test(first.authority, first_oracle)
			native_appkit_assert_cleanup_order_for_test(before_stop, first_oracle[all_oracle.len..])
			first_side := native_appkit_assert_side_effect_operation_bijection_for_test(first.authority,
				first_oracle, side_generation, [], true)
			app.stop()!
			second := native_appkit_backend_replay_snapshot_for_test(app)
			second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(first, second)
			native_appkit_oracle_assert_equal_for_test(first_oracle, second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(first_side,
				native_appkit_side_effect_snapshot_for_test(side_generation))
			assert app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_phase_b_lifetimes() ! {
		$if darwin {
			native_appkit_exercise_anchor_drawable_chain_for_test()!
			native_appkit_lifetime_oracle_reset_for_test()
			startup_side_generation := native_appkit_side_effect_reset_for_test()
			mut startup := native_phase_b_new_app_without_renderer_for_test(.appkit)!
			window := startup.create_window(
				title:           'phase-B pre-existing AppKit window'
				width:           192
				height:          128
				visible:         true
				redraw_mode:     .on_demand
				render_workload: true
			)!
			window_index := native_appkit_window_record_index_for_test(startup, window)!
			preexisting_state := native_identity(startup.backend.appkit.windows[window_index].state)
			preexisting_ticket := startup.backend.appkit.windows[window_index].state_ticket
			preexisting_ticket_snapshot := native_phase_b_assert_bound_ticket_for_test(&startup.backend.native_operations,
				preexisting_ticket, .appkit_state, preexisting_state, 0, .app_lifetime)
			initial_attempt := startup.backend.native_operations.renderer_attempt_token
			assert startup.backend.native_operations.arm_proof()
			startup.start_renderer(RendererConfig{})!
			assert startup.renderer_is_usable()
			assert startup.backend.native_operations.renderer_attempt_token != initial_attempt
			stable_state_ticket := native_phase_b_assert_bound_ticket_for_test(&startup.backend.native_operations,
				preexisting_ticket, .appkit_state, preexisting_state, 0, .app_lifetime)
			assert stable_state_ticket.authority_token == preexisting_ticket_snapshot.authority_token
			assert stable_state_ticket.proof_generation == preexisting_ticket_snapshot.proof_generation
			backend := &startup.backend.appkit
			_ = native_phase_b_assert_bound_ticket_for_test(&startup.backend.native_operations,
				backend.device_ticket, .metal_device, native_identity(backend.device), 0,
				.app_lifetime)
			_ = native_phase_b_assert_bound_ticket_for_test(&startup.backend.native_operations,
				backend.anchor_state_ticket, .appkit_state, native_identity(backend.anchor_state),
				0, .renderer_attempt)
			assert startup.backend.native_operations.lifetime_tickets.len == 3
			startup_oracle_before_stop := native_appkit_lifetime_oracle_snapshot_for_test()
			mut configure_count := 0
			for record in startup_oracle_before_stop {
				assert record.thread_identity == startup.backend.native_operations.owner_thread_identity
				if record.kind == 4 {
					configure_count++
					assert record.identity == preexisting_state
					assert record.parent_identity == native_identity(backend.device)
					assert record.auxiliary_identity == preexisting_state
					assert record.auxiliary_identity_1 == native_identity(backend.device)
					assert record.auxiliary_identity_2 != 0
					assert record.valid_mask & native_valid_object_identity_0 != 0
					assert record.valid_mask & native_valid_object_identity_1 != 0
					assert record.valid_mask & native_valid_object_identity_2 != 0
					for ticket in startup.backend.native_operations.lifetime_tickets {
						assert ticket.native_identity != record.auxiliary_identity_2
					}
				}
			}
			assert configure_count == 1
			startup_ownership_before_stop := native_appkit_backend_replay_snapshot_for_test(startup)
			startup.stop()!
			first := native_appkit_backend_replay_snapshot_for_test(startup)
			first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert first.app_status == .stopped
			assert !first.authority.trace_overflow
			assert first.authority.registry.tickets.len == 0
			assert first.device == 0
			assert first.anchor_state == 0
			assert first.batch_autorelease_pool == 0
			assert first.active_anchor_drawable == 0
			assert first.windows.len == 0
			native_appkit_assert_drawable_and_pool_chains_for_test(first_oracle, false)
			native_appkit_assert_release_bijection_for_test(first.authority, first_oracle)
			native_appkit_assert_one_window_cleanup_oracle_for_test(startup_ownership_before_stop,
				first_oracle[startup_oracle_before_stop.len..],
				first.authority.owner_thread_identity)
			startup_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(first.authority,
				first_oracle, startup_side_generation, [], true)
			window_state_identity := startup_ownership_before_stop.windows[0].state
			anchor_state_identity := startup_ownership_before_stop.anchor_state
			device_identity := startup_ownership_before_stop.device
			mut window_state_release := -1
			mut anchor_state_release := -1
			mut device_release := -1
			mut shutdown_pool_pop := -1
			for index, record in first_oracle {
				assert record.thread_identity == first.authority.owner_thread_identity
				if record.kind == 14 {
					shutdown_pool_pop = index
				}
				if record.kind == 6 && record.identity == window_state_identity {
					assert window_state_release < 0
					window_state_release = index
				}
				if record.kind == 6 && record.identity == anchor_state_identity {
					assert anchor_state_release < 0
					anchor_state_release = index
				}
				if record.kind == 2 && record.identity == device_identity {
					assert device_release < 0
					device_release = index
				}
			}
			assert window_state_release >= 0
			assert shutdown_pool_pop >= 0
			assert anchor_state_release >= 0
			assert device_release >= 0
			assert window_state_release < shutdown_pool_pop
			assert shutdown_pool_pop < anchor_state_release
			assert anchor_state_release < device_release
			startup.stop()!
			second := native_appkit_backend_replay_snapshot_for_test(startup)
			second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(first, second)
			native_appkit_oracle_assert_equal_for_test(first_oracle, second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(startup_first_side,
				native_appkit_side_effect_snapshot_for_test(startup_side_generation))
			assert startup.backend.native_operations.disarm_proof()

			native_appkit_lifetime_oracle_reset_for_test()
			draw_side_generation := native_appkit_side_effect_reset_for_test()
			mut draw_app := native_runtime_new_app_for_test(.appkit)!
			draw_window := native_runtime_new_window_for_test(mut draw_app,
				'phase-B AppKit drawable lifetime')!
			assert draw_app.backend.native_operations.arm_proof()
			mut state := &NativeAppKitObservedDrawableState{}
			outcome := draw_app.with_scheduled_render_batch(fn [mut draw_app, draw_window, draw_side_generation, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
				assert candidates.any(it.window == draw_window)
				_ = native_appkit_assert_pool_ticket_context_for_test(&draw_app.backend.native_operations,
					draw_app.backend.appkit.batch_autorelease_pool_ticket,
					native_identity(draw_app.backend.appkit.batch_autorelease_pool), batch)
				acquisition := draw_app.acquire_render_target(batch, draw_window)!
				assert acquisition.status == .ready
				target_generation := acquisition.snapshot.target.target_identity
				draw_app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut draw_app, draw_window, draw_side_generation, batch, acquisition, target_generation, mut state] () ! {
					index := native_appkit_window_record_index_for_test(draw_app, draw_window)!
					record := draw_app.backend.appkit.windows[index]
					state.observed_parent = native_identity(record.state)
					assert state.observed_parent != 0
					state.observed_drawable = native_identity(record.active_drawable)
					state.observed_ticket = record.active_drawable_ticket
					assert state.observed_drawable != 0
					assert state.observed_ticket != 0
					oracle := native_appkit_lifetime_oracle_snapshot_for_test()
					for oracle_index := oracle.len - 1; oracle_index >= 0; oracle_index-- {
						if oracle[oracle_index].kind == 9
							&& oracle[oracle_index].output_identity == state.observed_drawable {
							state.observed_depth = oracle[oracle_index].auxiliary_identity
							break
						}
					}
					assert state.observed_depth != 0
					drawable_ticket := native_appkit_assert_window_drawable_ticket_context_for_test(&draw_app.backend.native_operations,
						state.observed_ticket, state.observed_drawable,
						native_identity(record.state), target_generation, batch, acquisition.lease)
					state_ticket := native_lifetime_ticket_snapshot_for_test(&draw_app.backend.native_operations,
						record.state_ticket)!
					assert drawable_ticket.parent_authority_scope == .app_lifetime
					assert drawable_ticket.parent_authority_token == state_ticket.authority_token
					before_parent_release :=
						native_proof_snapshot(&draw_app.backend.native_operations)
					before_parent_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
					before_parent_side :=
						native_appkit_side_effect_snapshot_for_test(draw_side_generation)
					blocked_parent := draw_app.backend.appkit.release_appkit_lifetime_identity(record.state,
						record.state_ticket, .appkit_state, 0, err_appkit_destroy_window_failed)
					assert !blocked_parent.native_released
					assert !blocked_parent.ticket_retired
					assert native_identity(blocked_parent.value) == state.observed_parent
					assert blocked_parent.ticket_id == record.state_ticket
					native_proof_assert_snapshots_equal(before_parent_release,
						native_proof_snapshot(&draw_app.backend.native_operations))
					native_appkit_oracle_assert_equal_for_test(before_parent_oracle,
						native_appkit_lifetime_oracle_snapshot_for_test())
					native_appkit_assert_side_effect_snapshots_equal_for_test(before_parent_side,
						native_appkit_side_effect_snapshot_for_test(draw_side_generation))
					assert native_identity(draw_app.backend.appkit.windows[index].state) == state.observed_parent
					assert draw_app.backend.appkit.windows[index].state_ticket == record.state_ticket
					_ = native_phase_b_assert_bound_ticket_for_test(&draw_app.backend.native_operations,
						record.state_ticket, .appkit_state, state.observed_parent, 0, .app_lifetime)
					for ticket in draw_app.backend.native_operations.lifetime_tickets {
						assert ticket.native_identity != state.observed_depth
					}
				})!
			})!
			assert outcome.error == ''
			assert outcome.finalized_submissions == 1
			assert state.observed_drawable != 0
			assert state.observed_depth != 0
			assert state.observed_parent != 0
			assert native_lifetime_ticket_index_for_test(&draw_app.backend.native_operations,
				state.observed_ticket) == -1
			draw_index := native_appkit_window_record_index_for_test(draw_app, draw_window)!
			assert native_identity(draw_app.backend.appkit.windows[draw_index].state) == state.observed_parent
			assert draw_app.backend.appkit.windows[draw_index].state_ticket != 0
			assert draw_app.backend.appkit.windows[draw_index].active_drawable == unsafe { nil }
			assert draw_app.backend.appkit.windows[draw_index].active_drawable_ticket == 0
			assert !draw_app.backend.appkit.windows[draw_index].frame_active
			private_batch_seed := NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .anchor_prepare
				scope:         .batch
				batch_epoch:   0x7301
			}
			typed_batch := RenderBatchLease{
				app_instance: draw_app.instance_id
				epoch:        0x7301
			}
			draw_app.backend.appkit.begin_render_batch(private_batch_seed)!
			_ = native_appkit_assert_pool_ticket_context_for_test(&draw_app.backend.native_operations,
				draw_app.backend.appkit.batch_autorelease_pool_ticket,
				native_identity(draw_app.backend.appkit.batch_autorelease_pool), typed_batch)
			candidate := draw_app.render_window_snapshot(draw_window)!
			prepared := draw_app.backend.appkit.begin_render(draw_window, candidate, NativeTargetAttempt{
				batch_epoch:        0x7301
				window_lease_epoch: 0x7302
				target_lease_epoch: 0x7303
			})
			assert prepared.outcome.succeeded()
			active := draw_app.backend.appkit.activate_render_frame(prepared.frame)
			assert active.outcome.succeeded()
			assert active.frame.acquired
			typed_drawable := native_identity(active.frame.swapchain.metal.current_drawable)
			assert typed_drawable != 0
			typed_index := native_appkit_window_record_index_for_test(draw_app, draw_window)!
			typed_record := draw_app.backend.appkit.windows[typed_index]
			typed_target := RenderTargetLease{
				app_instance: draw_app.instance_id
				batch_epoch:  0x7301
				target_epoch: 0x7303
				window_epoch: 0x7302
				window:       draw_window
			}
			typed_ticket := native_appkit_assert_window_drawable_ticket_context_for_test(&draw_app.backend.native_operations,
				typed_record.active_drawable_ticket, typed_drawable,
				native_identity(typed_record.state), candidate.target.target_identity, typed_batch,
				typed_target)
			typed_depth := native_identity(active.frame.swapchain.metal.depth_stencil_texture)
			assert typed_depth != 0
			for ticket in draw_app.backend.native_operations.lifetime_tickets {
				assert ticket.native_identity != typed_depth
			}
			typed_oracle_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			typed_trace_start := draw_app.backend.native_operations.proof.trace_len
			draw_app.backend.appkit.render_health = .unavailable
			draw_app.backend.appkit.release_active_frames_lifetime()
			assert native_lifetime_ticket_index_for_test(&draw_app.backend.native_operations,
				typed_ticket.ticket_id) == -1
			assert draw_app.backend.appkit.windows[typed_index].active_drawable == unsafe { nil }
			assert draw_app.backend.appkit.windows[typed_index].active_drawable_ticket == 0
			assert !draw_app.backend.appkit.windows[typed_index].frame_active
			draw_app.backend.appkit.release_batch_pool_lifetime(private_batch_seed)
			typed_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert typed_oracle.len == typed_oracle_start + 2
			assert typed_oracle[typed_oracle_start].kind == 12
			assert typed_oracle[typed_oracle_start].identity == typed_drawable
			assert typed_oracle[typed_oracle_start].parent_identity == native_identity(typed_record.state)
			assert typed_oracle[typed_oracle_start + 1].kind == 14
			for index in typed_trace_start .. draw_app.backend.native_operations.proof.trace_len {
				entry := draw_app.backend.native_operations.proof.trace[index]
				if entry.milestone == .real_call {
					assert entry.context.operation in [.clear_state, .render_batch_end]
				}
			}
			draw_ownership_before_stop := native_appkit_backend_replay_snapshot_for_test(draw_app)
			mut draw_stop_error := ''
			draw_app.stop() or { draw_stop_error = err.msg() }
			draw_first := native_appkit_backend_replay_snapshot_for_test(draw_app)
			draw_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert !draw_first.authority.trace_overflow
			assert draw_first.authority.registry.tickets.len == 0
			assert draw_first.app_status == .stopped
			assert draw_first.render_health == .abandoned
			assert draw_first.stop_terminal == ''
			assert draw_stop_error == ''
			mut typed_first_state_release := -1
			mut typed_last_state_release := -1
			mut typed_device_release := -1
			for index in typed_oracle_start .. draw_oracle.len {
				record := draw_oracle[index]
				assert record.kind in [u64(12), 14, 6, 2]
				if record.kind == 6 && typed_first_state_release < 0 {
					typed_first_state_release = index
				}
				if record.kind == 6 {
					typed_last_state_release = index
				}
				if record.kind == 2 {
					typed_device_release = index
				}
			}
			assert typed_oracle_start + 1 < typed_first_state_release
			assert typed_first_state_release <= typed_last_state_release
			assert typed_last_state_release < typed_device_release
			mut observed_child_terminal := -1
			mut observed_child_terminal_count := 0
			mut observed_parent_release := -1
			mut observed_parent_release_count := 0
			for index, record in draw_oracle {
				if record.kind in [u64(10), 11, 12] && record.identity == state.observed_drawable {
					observed_child_terminal = index
					observed_child_terminal_count++
				}
				if record.kind == 6 && record.identity == state.observed_parent {
					observed_parent_release = index
					observed_parent_release_count++
				}
			}
			assert observed_child_terminal_count == 1
			assert observed_parent_release_count == 1
			assert observed_child_terminal < observed_parent_release
			native_appkit_assert_drawable_and_pool_chains_for_test(draw_oracle, true)
			native_appkit_assert_release_bijection_for_test(draw_first.authority, draw_oracle)
			native_appkit_assert_cleanup_order_for_test(draw_ownership_before_stop,
				draw_oracle[typed_oracle.len..])
			draw_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(draw_first.authority,
				draw_oracle, draw_side_generation, [], true)
			mut draw_replay_error := ''
			draw_app.stop() or { draw_replay_error = err.msg() }
			draw_second := native_appkit_backend_replay_snapshot_for_test(draw_app)
			draw_second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(draw_first, draw_second)
			native_appkit_oracle_assert_equal_for_test(draw_oracle, draw_second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(draw_first_side,
				native_appkit_side_effect_snapshot_for_test(draw_side_generation))
			assert draw_replay_error == draw_stop_error
			assert draw_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_assert_owned_slots_equal_for_test(expected NativeAppKitBackendReplaySnapshot, actual NativeAppKitBackendReplaySnapshot) {
		assert actual.app_status == expected.app_status
		assert actual.stop_terminal == expected.stop_terminal
		assert actual.started == expected.started
		assert actual.device == expected.device
		assert actual.device_ticket == expected.device_ticket
		assert actual.anchor_state == expected.anchor_state
		assert actual.anchor_state_ticket == expected.anchor_state_ticket
		assert actual.batch_autorelease_pool == expected.batch_autorelease_pool
		assert actual.batch_autorelease_pool_ticket == expected.batch_autorelease_pool_ticket
		assert actual.next_anchor_lease == expected.next_anchor_lease
		assert actual.active_anchor_lease == expected.active_anchor_lease
		assert actual.active_anchor_drawable == expected.active_anchor_drawable
		assert actual.active_anchor_drawable_ticket == expected.active_anchor_drawable_ticket
		assert actual.render_sequence == expected.render_sequence
		assert actual.poll_error == expected.poll_error
		assert actual.event_sequence_terminal == expected.event_sequence_terminal
		assert actual.windows == expected.windows
		assert actual.authority.app_identity == expected.authority.app_identity
		assert actual.authority.app_lifetime_token == expected.authority.app_lifetime_token
		assert actual.authority.renderer_attempt_token == expected.authority.renderer_attempt_token
		assert actual.authority.owner_thread_identity == expected.authority.owner_thread_identity
		assert actual.authority.next_ordinal == expected.authority.next_ordinal
		assert actual.authority.next_proof_generation == expected.authority.next_proof_generation
		assert actual.authority.proof_armed == expected.authority.proof_armed
		assert actual.authority.proof_generation == expected.authority.proof_generation
		assert actual.authority.proof_ordinal_floor == expected.authority.proof_ordinal_floor
		assert actual.authority.proof_accepting_plans == expected.authority.proof_accepting_plans
		native_proof_assert_plan_equal(expected.authority.plan, actual.authority.plan)
		native_proof_assert_trace_equal(expected.authority.trace, actual.authority.trace)
		assert actual.authority.trace_len == expected.authority.trace_len
		assert actual.authority.trace_overflow == expected.authority.trace_overflow
		native_lifetime_registry_assert_snapshots_equal(expected.authority.registry,
			actual.authority.registry)
	}

	fn native_appkit_exercise_phase_b_ordinal_exhaustion_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			exhausted_side_generation := native_appkit_side_effect_reset_for_test()
			mut exhausted_app := native_runtime_new_app_for_test(.appkit)!
			exhausted_window := native_runtime_new_window_for_test(mut exhausted_app,
				'phase-B AppKit ordinal exhaustion')!
			assert exhausted_app.backend.native_operations.arm_proof()
			exhaustion_operation_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			sokol_generation := native_install_sokol_trace_for_test()!
			mut state := &NativeAppKitPhaseBExhaustionState{}
			outcome := exhausted_app.with_scheduled_render_batch(fn [mut exhausted_app, exhausted_window, exhaustion_operation_start, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
				assert candidates.any(it.window == exhausted_window)
				state.live_pool_ticket = native_appkit_assert_pool_ticket_context_for_test(&exhausted_app.backend.native_operations,
					exhausted_app.backend.appkit.batch_autorelease_pool_ticket,
					native_identity(exhausted_app.backend.appkit.batch_autorelease_pool), batch)
				acquisition := exhausted_app.acquire_render_target(batch, exhausted_window)!
				assert acquisition.status == .ready
				target_generation := acquisition.snapshot.target.target_identity
				exhausted_app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut exhausted_app, batch, acquisition, target_generation, exhaustion_operation_start, mut state] () ! {
					index := native_appkit_window_record_index_for_test(exhausted_app,
						acquisition.lease.window)!
					record := exhausted_app.backend.appkit.windows[index]
					drawable_identity := native_identity(record.active_drawable)
					assert drawable_identity != 0
					state.live_drawable_ticket = native_appkit_assert_window_drawable_ticket_context_for_test(&exhausted_app.backend.native_operations,
						record.active_drawable_ticket, drawable_identity,
						native_identity(record.state), target_generation, batch, acquisition.lease)
					oracle := native_appkit_lifetime_oracle_snapshot_for_test()
					assert oracle.len == exhaustion_operation_start + 2
					assert oracle[exhaustion_operation_start].kind == 13
					assert oracle[exhaustion_operation_start + 1].kind == 9
					assert oracle[exhaustion_operation_start + 1].output_identity == drawable_identity
					state.live_depth_identity = oracle[exhaustion_operation_start + 1].auxiliary_identity
					assert state.live_depth_identity != 0
					for ticket in exhausted_app.backend.native_operations.lifetime_tickets {
						assert ticket.native_identity != state.live_depth_identity
					}
					exhausted_app.backend.native_operations.next_ordinal = ~u64(0)
					last :=
						exhausted_app.backend.native_operations.reserve_renderer_attempt_ordinals(1)!
					assert last.first == ~u64(0)
					assert last.count == 1
					assert last.authority_scope == .renderer_attempt
					assert exhausted_app.backend.native_operations.next_ordinal == 0
					assert !exhausted_app.backend.native_operations.sequence_exhausted
					state.boundary_oracle = native_appkit_lifetime_oracle_snapshot_for_test()
					state.boundary_ownership =
						native_appkit_backend_replay_snapshot_for_test(exhausted_app)
				})!
			})!
			assert outcome.error != ''
			assert outcome.committed
			assert outcome.completed_user_passes == 1
			assert outcome.finalized_submissions == 0
			assert exhausted_app.backend.native_operations.sequence_exhausted
			assert exhausted_app.backend.native_operations.terminal_cause == .sequence_exhausted
			assert exhausted_app.backend.native_operations.next_ordinal == 0
			assert state.live_pool_ticket.ticket_id != 0
			assert state.live_drawable_ticket.ticket_id != 0
			assert state.live_depth_identity != 0
			after_boundary_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_oracle_assert_equal_for_test(state.boundary_oracle, after_boundary_oracle)
			after_boundary_ownership :=
				native_appkit_backend_replay_snapshot_for_test(exhausted_app)
			native_appkit_assert_owned_slots_equal_for_test(state.boundary_ownership,
				after_boundary_ownership)
			assert state.boundary_ownership.render_health == .ready
			assert after_boundary_ownership.render_health == .unavailable
			assert !state.boundary_ownership.authority.sequence_exhausted
			assert after_boundary_ownership.authority.sequence_exhausted
			assert after_boundary_ownership.authority.terminal_cause == .sequence_exhausted
			native_assert_sokol_sequence_for_test(multiwindow_sokol_trace.typed_snapshot(), [
				multiwindow_sokol_trace.Operation.begin_swapchain_pass,
				.end_pass,
				.commit,
			])
			native_uninstall_sokol_trace_for_test(sokol_generation)!
			before_rejected_schedule :=
				native_appkit_backend_replay_snapshot_for_test(exhausted_app)
			before_rejected_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			mut rejected_callback_calls := 0
			mut rejected_schedule_error := ''
			exhausted_app.with_scheduled_render_batch(fn [mut rejected_callback_calls] (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {
				rejected_callback_calls++
			}) or { rejected_schedule_error = err.msg() }
			assert rejected_schedule_error != ''
			assert rejected_callback_calls == 0
			native_appkit_oracle_assert_equal_for_test(before_rejected_oracle,
				native_appkit_lifetime_oracle_snapshot_for_test())
			native_appkit_backend_assert_replay_equal_for_test(before_rejected_schedule,
				native_appkit_backend_replay_snapshot_for_test(exhausted_app))
			cleanup_tickets := after_boundary_ownership.authority.registry.tickets
			mut first_stop_error := ''
			exhausted_app.stop() or { first_stop_error = err.msg() }
			first := native_appkit_backend_replay_snapshot_for_test(exhausted_app)
			first_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			assert first.app_status == .stopped
			assert first.authority.registry.tickets.len == 0
			native_appkit_assert_ticket_release_delta_for_test(cleanup_tickets,
				first_oracle[after_boundary_oracle.len..])
			native_appkit_assert_cleanup_order_for_test(after_boundary_ownership,
				first_oracle[after_boundary_oracle.len..])
			for record in first_oracle[after_boundary_oracle.len..] {
				assert native_appkit_release_kind_for_oracle(record.kind)
			}
			exhausted_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(first.authority,
				first_oracle, exhausted_side_generation, [], true)
			mut replay_stop_error := ''
			exhausted_app.stop() or { replay_stop_error = err.msg() }
			second := native_appkit_backend_replay_snapshot_for_test(exhausted_app)
			second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(first, second)
			native_appkit_oracle_assert_equal_for_test(first_oracle, second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(exhausted_first_side,
				native_appkit_side_effect_snapshot_for_test(exhausted_side_generation))
			assert replay_stop_error == first_stop_error
			assert exhausted_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_phase_b_trace_overflow_for_test() ! {
		$if darwin {
			native_appkit_lifetime_oracle_reset_for_test()
			overflow_side_generation := native_appkit_side_effect_reset_for_test()
			mut overflow_app := native_runtime_new_app_for_test(.appkit)!
			_ = native_runtime_new_window_for_test(mut overflow_app,
				'phase-B AppKit trace overflow')!
			assert overflow_app.backend.native_operations.arm_proof()
			overflow_ownership := native_appkit_backend_replay_snapshot_for_test(overflow_app)
			overflow_tickets := native_lifetime_registry_snapshot(&overflow_app.backend.native_operations).tickets
			assert overflow_tickets.len == 3
			for index in 0 .. native_operation_trace_capacity {
				overflow_app.backend.native_operations.record(.real_call, NativeOperationContext{
					authority_scope:        .renderer_attempt
					authority_token:        overflow_app.backend.native_operations.renderer_attempt_token
					renderer_attempt_token: overflow_app.backend.native_operations.renderer_attempt_token
					app_identity:           overflow_app.instance_id
					ordinal:                u64(index + 1)
				}, NativePrimitiveEvidence{}, NativePrimitiveEvidence{}, .none, NativeRenderResult{})
			}
			prefix := overflow_app.backend.native_operations.proof.trace
			overflow_oracle_start := native_appkit_lifetime_oracle_snapshot_for_test().len
			overflow_app.stop()!
			overflow_first := native_appkit_backend_replay_snapshot_for_test(overflow_app)
			overflow_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			overflow_cleanup_oracle := overflow_oracle[overflow_oracle_start..]
			assert overflow_first.authority.trace_len == native_operation_trace_capacity
			assert overflow_first.authority.trace_overflow
			native_proof_assert_trace_equal(prefix, overflow_first.authority.trace)
			assert overflow_first.authority.registry.tickets.len == 0
			native_appkit_assert_ticket_release_delta_with_temporary_pool_for_test(overflow_tickets,
				overflow_cleanup_oracle, overflow_ownership,
				overflow_first.authority.owner_thread_identity)
			overflow_first_side := native_appkit_assert_side_effect_operation_bijection_for_test(overflow_first.authority,
				overflow_oracle, overflow_side_generation, [], false)
			overflow_app.stop()!
			overflow_second := native_appkit_backend_replay_snapshot_for_test(overflow_app)
			overflow_second_oracle := native_appkit_lifetime_oracle_snapshot_for_test()
			native_appkit_backend_assert_replay_equal_for_test(overflow_first, overflow_second)
			native_appkit_oracle_assert_equal_for_test(overflow_oracle, overflow_second_oracle)
			native_appkit_assert_side_effect_snapshots_equal_for_test(overflow_first_side,
				native_appkit_side_effect_snapshot_for_test(overflow_side_generation))
			assert overflow_app.backend.native_operations.disarm_proof()
		}
	}

	fn native_appkit_exercise_phase_b_exhaustion_and_overflow() ! {
		$if darwin {
			native_terminal_subprocess_for_test(native_appkit_ordinal_exhaustion_child_marker,
				'phase-B AppKit ordinal exhaustion')!
			native_appkit_exercise_phase_b_trace_overflow_for_test()!
		}
	}
}
