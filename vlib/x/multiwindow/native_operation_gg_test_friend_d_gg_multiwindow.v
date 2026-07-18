module multiwindow

$if test {
	pub struct GgEglNativeWindowProofTicket {
		app_identity           u64
		renderer_attempt_token u64
		backend_kind           BackendKind
		primary_context        NativeOperationContext
		error_context          NativeOperationContext
	}

	struct GgEglNativeWindowProofTarget {
		generation u64
		identity   u64
	}

	enum GgNativePlanPairArmOutcome {
		armed
		proof_rejected
		primary_rejected_disarmed
		primary_rejected_disarm_failed
		secondary_rejected_rolled_back_disarmed
		secondary_rollback_failed
		secondary_rejected_disarm_failed
	}

	pub fn (mut app App) arm_gg_egl_bad_native_window_for_test(lease RenderTargetLease) !GgEglNativeWindowProofTicket {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		app.ensure_running_locked() or {
			app.state_mutex.unlock()
			return err
		}
		app.ensure_event_admission_open_locked() or {
			app.state_mutex.unlock()
			return err
		}
		app.live_window_index(lease.window) or {
			app.state_mutex.unlock()
			return err
		}
		app.state_mutex.unlock()

		if app.backend.kind !in [.x11, .wayland] {
			return error(err_backend_unsupported)
		}
		state := app.render_backend_state()!
		if !state.batch_active || state.batch_epoch == 0 || state.batch_epoch != lease.batch_epoch
			|| state.terminal || state.device_lost || state.native_health.blocks_graphics() {
			return error(err_render_target_stale)
		}
		target_index := validate_target_lease(state, app.instance_id, lease)!
		target_slot := state.targets[target_index]
		frame := target_slot.frame
		if target_slot.status != .prepared || target_slot.epoch != lease.target_epoch
			|| target_slot.lease != lease || frame.acquired || frame.window_id != lease.window
			|| frame.batch_epoch != lease.batch_epoch
			|| frame.window_lease_epoch != lease.window_epoch
			|| frame.target_lease_epoch != lease.target_epoch || frame.target.target_identity == 0
			|| target_slot.snapshot.window != lease.window
			|| target_slot.snapshot.batch_epoch != lease.batch_epoch
			|| target_slot.snapshot.target.target_identity != frame.target.target_identity {
			return error(err_render_target_stale)
		}

		mut authority := &app.backend.native_operations
		if authority.proof != unsafe { nil } || authority.app_identity != app.instance_id
			|| authority.renderer_attempt_token == 0
			|| authority.renderer_attempt_token != state.fault_trace_token
			|| authority.sequence_exhausted || authority.next_ordinal == 0
			|| authority.next_ordinal > ~u64(0) - u64(4) {
			return error(err_render_attempt_stale)
		}
		target := gg_egl_native_window_proof_target(app, lease, frame)!
		if target.generation != frame.target.target_identity || target.identity == 0 {
			return error(err_render_target_stale)
		}
		primary_context := NativeOperationContext{
			authority_scope:        .renderer_attempt
			authority_token:        authority.renderer_attempt_token
			renderer_attempt_token: authority.renderer_attempt_token
			app_identity:           app.instance_id
			presence_mask:          native_context_window_target_fields | native_context_has_target_identity
			domain:                 .egl
			operation:              .make_current
			call_site:              .window_activate
			scope:                  .window_target
			window:                 lease.window
			target_generation:      target.generation
			target_identity:        target.identity
			batch_epoch:            lease.batch_epoch
			window_lease_epoch:     lease.window_epoch
			target_lease_epoch:     lease.target_epoch
			ordinal:                authority.next_ordinal
		}
		error_context := gg_egl_native_window_error_context(primary_context)
		primary_evidence := gg_egl_native_window_primary_evidence()
		error_evidence := gg_egl_native_window_error_evidence()

		arm_outcome := authority.arm_gg_native_plan_pair(primary_context, primary_evidence,
			error_context, error_evidence)
		match arm_outcome {
			.armed {}
			.proof_rejected {
				return error('multiwindow: could not arm gg EGL native-window proof')
			}
			.primary_rejected_disarmed {
				return error('multiwindow: could not arm gg EGL make-current proof')
			}
			.primary_rejected_disarm_failed, .secondary_rejected_disarm_failed {
				return error('multiwindow: could not disarm rejected gg EGL native-window proof')
			}
			.secondary_rollback_failed {
				return error('multiwindow: could not roll back gg EGL make-current proof')
			}
			.secondary_rejected_rolled_back_disarmed {
				return error('multiwindow: could not arm gg EGL error proof')
			}
		}

		return GgEglNativeWindowProofTicket{
			app_identity:           app.instance_id
			renderer_attempt_token: authority.renderer_attempt_token
			backend_kind:           app.backend.kind
			primary_context:        primary_context
			error_context:          error_context
		}
	}

	pub fn (mut app App) finish_gg_egl_bad_native_window_proof_for_test(ticket GgEglNativeWindowProofTicket) ! {
		app.assert_owner_thread()!
		mut authority := &app.backend.native_operations
		if ticket.app_identity == 0 || ticket.app_identity != app.instance_id
			|| !authority.owner_thread_is_current() || ticket.renderer_attempt_token == 0
			|| ticket.renderer_attempt_token != authority.renderer_attempt_token
			|| ticket.backend_kind != app.backend.kind || ticket.backend_kind !in [.x11, .wayland]
			|| authority.app_identity != ticket.app_identity || authority.proof == unsafe { nil }
			|| ticket.primary_context.authority_scope != .renderer_attempt
			|| ticket.primary_context.authority_token != ticket.renderer_attempt_token
			|| ticket.primary_context.renderer_attempt_token != ticket.renderer_attempt_token
			|| ticket.primary_context.app_identity != ticket.app_identity
			|| ticket.primary_context.presence_mask != (native_context_window_target_fields | native_context_has_target_identity)
			|| ticket.primary_context.domain != .egl
			|| ticket.primary_context.operation != .make_current
			|| ticket.primary_context.call_site != .window_activate
			|| ticket.primary_context.scope != .window_target
			|| ticket.primary_context.window.app_instance != ticket.app_identity
			|| ticket.primary_context.target_generation == 0
			|| ticket.primary_context.target_identity == 0
			|| ticket.primary_context.batch_epoch == 0
			|| ticket.primary_context.window_lease_epoch == 0
			|| ticket.primary_context.target_lease_epoch == 0 || ticket.primary_context.ordinal == 0
			|| ticket.primary_context.ordinal == ~u64(0)
			|| !native_operation_contexts_identical(ticket.error_context, gg_egl_native_window_error_context(ticket.primary_context)) {
			return error('multiwindow: stale gg EGL native-window proof ticket')
		}
		proof := authority.proof
		if !gg_egl_native_window_proof_owned_by_ticket(proof, ticket) {
			return error('multiwindow: stale gg EGL native-window proof ticket')
		}
		mut verification_error := ''
		if proof.trace_overflow {
			verification_error = 'multiwindow: gg EGL native-window proof trace overflowed'
		} else {
			gg_egl_verify_native_window_plan(proof, ticket) or { verification_error = err.msg() }
			if verification_error.len == 0 {
				gg_egl_verify_native_window_trace(proof, ticket) or {
					verification_error = err.msg()
				}
			}
		}
		for index in 0 .. native_primitive_plan_capacity {
			entry := proof.plan[index]
			if native_operation_contexts_identical(entry.context, ticket.primary_context)
				|| native_operation_contexts_identical(entry.context, ticket.error_context) {
				authority.proof.plan[index] = NativePrimitivePlanEntry{}
			}
		}
		if !authority.disarm_proof() {
			return error('multiwindow: could not disarm gg EGL native-window proof')
		}
		if verification_error.len != 0 {
			return error(verification_error)
		}
	}

	fn (mut authority NativeOperationAuthority) arm_gg_native_plan_pair(primary_context NativeOperationContext, primary_evidence NativePrimitiveEvidence, secondary_context NativeOperationContext, secondary_evidence NativePrimitiveEvidence) GgNativePlanPairArmOutcome {
		if !authority.arm_proof() {
			return .proof_rejected
		}
		if !authority.arm(primary_context, primary_evidence) {
			if !authority.disarm_proof() {
				return .primary_rejected_disarm_failed
			}
			return .primary_rejected_disarmed
		}
		if !authority.arm(secondary_context, secondary_evidence) {
			if !authority.rollback_gg_egl_native_window_plan_entry(primary_context,
				primary_evidence) {
				return .secondary_rollback_failed
			}
			if !authority.disarm_proof() {
				return .secondary_rejected_disarm_failed
			}
			return .secondary_rejected_rolled_back_disarmed
		}
		return .armed
	}

	fn (mut authority NativeOperationAuthority) rollback_gg_egl_native_window_plan_entry(context NativeOperationContext, evidence NativePrimitiveEvidence) bool {
		if !authority.owner_thread_is_current() || authority.proof == unsafe { nil } {
			return false
		}
		mut matched_index := -1
		for index in 0 .. native_primitive_plan_capacity {
			entry := authority.proof.plan[index]
			if entry.proof_generation != authority.proof.generation
				|| !native_operation_contexts_identical(entry.context, context) {
				continue
			}
			if matched_index >= 0 || !entry.armed || entry.listener_registration_pending
				|| !native_primitive_evidence_identical(entry.evidence, evidence) {
				return false
			}
			matched_index = index
		}
		if matched_index < 0 {
			return false
		}
		authority.proof.plan[matched_index] = NativePrimitivePlanEntry{}
		return true
	}

	fn gg_egl_native_window_proof_target(app &App, lease RenderTargetLease, frame RenderFrame) !GgEglNativeWindowProofTarget {
		match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					backend := &app.backend.x11
					index := backend.window_record_index(lease.window) or {
						return error(err_render_target_stale)
					}
					record := backend.windows[index]
					if !backend.renderer_ready() || backend.native_operations == unsafe { nil }
						|| backend.native_operations.app_identity != app.instance_id
						|| backend.native_operations.renderer_attempt_token != app.backend.native_operations.renderer_attempt_token
						|| record.native_destroyed || record.egl_surface == unsafe { nil }
						|| record.render_target_generation == 0
						|| record.render_target_generation != frame.target.target_identity
						|| frame.metrics.framebuffer_width != record.width
						|| frame.metrics.framebuffer_height != record.height {
						return error(err_render_target_stale)
					}
					return GgEglNativeWindowProofTarget{
						generation: record.render_target_generation
						identity:   native_identity(record.egl_surface)
					}
				} $else {
					return error(err_backend_unsupported)
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					backend := &app.backend.wayland
					index := backend.window_record_index(lease.window) or {
						return error(err_render_target_stale)
					}
					record := backend.windows[index]
					if !backend.renderer_ready() || backend.native_operations == unsafe { nil }
						|| backend.native_operations.app_identity != app.instance_id
						|| backend.native_operations.renderer_attempt_token != app.backend.native_operations.renderer_attempt_token
						|| backend.wayland_display_unavailable || record.native_destroyed
						|| !record.frame_ready || record.egl_surface == unsafe { nil }
						|| record.render_target_generation == 0
						|| record.render_target_generation != frame.target.target_identity
						|| frame.metrics.framebuffer_width != record.width
						|| frame.metrics.framebuffer_height != record.height {
						return error(err_render_target_stale)
					}
					return GgEglNativeWindowProofTarget{
						generation: record.render_target_generation
						identity:   native_identity(record.egl_surface)
					}
				} $else {
					return error(err_backend_unsupported)
				}
			}
			else {
				return error(err_backend_unsupported)
			}
		}
	}

	fn gg_egl_native_window_error_context(primary NativeOperationContext) NativeOperationContext {
		return NativeOperationContext{
			...primary
			presence_mask:   primary.presence_mask & ~native_context_has_target_identity
			operation:       .egl_error_query
			target_identity: 0
			ordinal:         primary.ordinal + 1
		}
	}

	fn gg_egl_native_window_primary_evidence() NativePrimitiveEvidence {
		return NativePrimitiveEvidence{
			valid_mask:   native_valid_return_value
			return_value: 0
		}
	}

	fn gg_egl_native_window_error_evidence() NativePrimitiveEvidence {
		return NativePrimitiveEvidence{
			valid_mask: native_valid_egl_error
			egl_error:  0x300b
		}
	}

	fn gg_egl_native_window_proof_owned_by_ticket(proof &NativeOperationProofState, ticket GgEglNativeWindowProofTicket) bool {
		if proof.generation == 0 {
			return false
		}
		primary_evidence := gg_egl_native_window_primary_evidence()
		error_evidence := gg_egl_native_window_error_evidence()
		mut populated := 0
		mut primary_entries := 0
		mut error_entries := 0
		for entry in proof.plan {
			if entry.listener_registration_pending {
				return false
			}
			if entry.context.ordinal == 0 {
				if entry.armed || entry.proof_generation != 0
					|| !native_primitive_evidence_identical(entry.evidence, NativePrimitiveEvidence{}) {
					return false
				}
				continue
			}
			if entry.proof_generation != proof.generation {
				return false
			}
			populated++
			if native_operation_contexts_identical(entry.context, ticket.primary_context) {
				if !native_primitive_evidence_identical(entry.evidence, primary_evidence) {
					return false
				}
				primary_entries++
			} else if native_operation_contexts_identical(entry.context, ticket.error_context) {
				if !native_primitive_evidence_identical(entry.evidence, error_evidence) {
					return false
				}
				error_entries++
			} else {
				return false
			}
		}
		return populated == 2 && primary_entries == 1 && error_entries == 1
	}

	fn gg_egl_verify_native_window_plan(proof &NativeOperationProofState, ticket GgEglNativeWindowProofTicket) ! {
		if !gg_egl_native_window_proof_owned_by_ticket(proof, ticket) {
			return error('multiwindow: gg EGL native-window proof ownership changed')
		}
		for entry in proof.plan {
			if entry.armed || entry.listener_registration_pending {
				return error('multiwindow: gg EGL native-window proof plan was not consumed')
			}
		}
	}

	fn gg_egl_verify_native_window_trace(proof &NativeOperationProofState, ticket GgEglNativeWindowProofTicket) ! {
		if proof.trace_len < 8 {
			return error('multiwindow: gg EGL native-window proof trace is incomplete')
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
			entry := proof.trace[offset]
			expected_context := if offset < 3 || offset >= 6 {
				ticket.primary_context
			} else {
				ticket.error_context
			}
			if entry.milestone != milestone
				|| !native_operation_contexts_identical(entry.context, expected_context) {
				return error('multiwindow: gg EGL native-window proof trace order changed')
			}
		}
		if proof.trace[1].actual.valid_mask != native_valid_return_value
			|| proof.trace[1].actual.return_value != 1
			|| !native_primitive_evidence_identical(proof.trace[2].effective, gg_egl_native_window_primary_evidence())
			|| proof.trace[4].actual.valid_mask != native_valid_egl_error
			|| proof.trace[4].actual.egl_error != 0x3000
			|| !native_primitive_evidence_identical(proof.trace[5].effective, gg_egl_native_window_error_evidence()) {
			return error('multiwindow: gg EGL native-window proof did not preserve actual and effective evidence')
		}
		accepted := proof.trace[6].result
		if accepted.domain != .egl || accepted.operation != .make_current
			|| accepted.scope != .window_target || accepted.disposition != .native_window_lost
			|| accepted.native_code != 0x300b || accepted.local_validation != .none
			|| !native_operation_contexts_identical(accepted.context, ticket.primary_context)
			|| proof.trace[7].health != .ready {
			return error('multiwindow: gg EGL native-window proof classification changed')
		}
		mut primary_real := 0
		mut primary_actual := 0
		mut primary_effective := 0
		mut primary_acceptance := 0
		mut primary_health := 0
		mut error_real := 0
		mut error_actual := 0
		mut error_effective := 0
		for index in 0 .. proof.trace_len {
			entry := proof.trace[index]
			if native_operation_contexts_identical(entry.context, ticket.primary_context) {
				match entry.milestone {
					.real_call { primary_real++ }
					.actual_primitive { primary_actual++ }
					.effective_primitive { primary_effective++ }
					.acceptance { primary_acceptance++ }
					.health_latched { primary_health++ }
					else {}
				}
			} else if native_operation_contexts_identical(entry.context, ticket.error_context) {
				match entry.milestone {
					.real_call { error_real++ }
					.actual_primitive { error_actual++ }
					.effective_primitive { error_effective++ }
					else {}
				}
			}
		}
		if primary_real != 1 || primary_actual != 1 || primary_effective != 1
			|| primary_acceptance != 1 || primary_health != 1 || error_real != 1
			|| error_actual != 1 || error_effective != 1 {
			return error('multiwindow: gg EGL native-window proof was replayed')
		}
	}
}
