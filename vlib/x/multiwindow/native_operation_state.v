module multiwindow

import sync

const native_primitive_plan_capacity = 8

$if linux && sokol_wayland ? {
	const native_operation_trace_capacity = 320
} $else {
	const native_operation_trace_capacity = 256
}

struct NativePrimitivePlanEntry {
mut:
	armed                         bool
	listener_registration_pending bool
	proof_generation              u64
	context                       NativeOperationContext
	evidence                      NativePrimitiveEvidence
}

struct NativeOperationTraceEntry {
	milestone        NativeOperationTraceMilestone
	context          NativeOperationContext
	actual           NativePrimitiveEvidence
	effective        NativePrimitiveEvidence
	local_validation NativeLocalValidation
	result           NativeRenderResult
	health           NativeRendererHealth
}

struct NativeOperationProofState {
mut:
	generation      u64
	ordinal_floor   u64
	accepting_plans bool
	plan            [native_primitive_plan_capacity]NativePrimitivePlanEntry
	trace           [native_operation_trace_capacity]NativeOperationTraceEntry
	trace_len       int
	trace_overflow  bool
}

struct NativeLifetimeReleaseTicket {
	ticket_id           u64
	app_identity        u64
	authority_scope     NativeOperationAuthorityScope
	authority_token     u64
	domain              NativeRenderDomain
	release_kind        NativeLifetimeReleaseKind
	owner_seed          NativeOperationSeed
	proof_generation    u64
	acquisition_context NativeOperationContext
mut:
	context                   NativeOperationContext
	native_identity           u64
	required_parent_identity  u64
	parent_authority_scope    NativeOperationAuthorityScope
	parent_authority_token    u64
	release_evidence_recorded bool
	state                     NativeLifetimeTicketState
}

struct NativeOrdinalRange {
	authority_scope       NativeOperationAuthorityScope
	authority_token       u64
	app_identity          u64
	proof_generation      u64
	owner_thread_identity u64
mut:
	first u64
	count u64
	used  u64
}

struct NativeLifetimeParentAuthority {
	scope NativeOperationAuthorityScope
	token u64
}

struct NativeOperationAuthority {
mut:
	app_identity           u64
	app_lifetime_token     u64
	renderer_attempt_token u64
	owner_thread_identity  u64
	next_ordinal           u64 = 1
	next_proof_generation  u64 = 1
	sequence_exhausted     bool
	terminal_cause         NativeLocalValidation
	proof                  &NativeOperationProofState = unsafe { nil }
	lifetime_tickets       []NativeLifetimeReleaseTicket
}

fn (mut authority NativeOperationAuthority) arm_proof() bool {
	if !authority.owner_thread_is_current() {
		return false
	}
	if authority.proof == unsafe { nil } {
		generation := authority.take_proof_generation() or { return false }
		authority.proof = &NativeOperationProofState{
			generation:      generation
			ordinal_floor:   authority.next_ordinal
			accepting_plans: true
		}
		return true
	}
	return authority.reset_proof()
}

fn (mut authority NativeOperationAuthority) reset_proof() bool {
	if !authority.owner_thread_is_current() || authority.proof == unsafe { nil } {
		return false
	}
	if authority.has_pending_native_plans()
		|| authority.has_live_tickets_for_proof_generation(authority.proof.generation) {
		authority.proof.accepting_plans = false
		return false
	}
	generation := authority.take_proof_generation() or {
		authority.proof.accepting_plans = false
		return false
	}
	unsafe {
		*authority.proof = NativeOperationProofState{
			generation:      generation
			ordinal_floor:   authority.next_ordinal
			accepting_plans: true
		}
	}
	return true
}

fn (mut authority NativeOperationAuthority) disarm_proof() bool {
	if !authority.owner_thread_is_current() {
		return false
	}
	if authority.proof == unsafe { nil } {
		return true
	}
	if authority.has_pending_native_plans()
		|| authority.has_live_tickets_for_proof_generation(authority.proof.generation) {
		authority.proof.accepting_plans = false
		return false
	}
	unsafe {
		free(authority.proof)
	}
	authority.proof = unsafe { nil }
	return true
}

fn (mut authority NativeOperationAuthority) take_proof_generation() ?u64 {
	if !authority.owner_thread_is_current() || authority.next_proof_generation == 0 {
		return none
	}
	generation := authority.next_proof_generation
	authority.next_proof_generation = if generation == ~u64(0) { u64(0) } else { generation + 1 }
	return generation
}

fn (mut authority NativeOperationAuthority) bind_app_lifetime(app_identity u64, app_lifetime_token u64) ! {
	owner_thread_identity := sync.thread_id()
	if app_identity == 0 || app_lifetime_token == 0 || owner_thread_identity == 0 {
		return error(err_render_native_renderer_unavailable)
	}
	if authority.owner_thread_identity != 0
		&& authority.owner_thread_identity != owner_thread_identity {
		return error(err_render_native_renderer_unavailable)
	}
	if authority.app_identity == app_identity && authority.app_lifetime_token == app_lifetime_token {
		if authority.owner_thread_identity != owner_thread_identity {
			return error(err_render_native_renderer_unavailable)
		}
		return
	}
	if authority.app_identity != 0 || authority.app_lifetime_token != 0
		|| authority.renderer_attempt_token != 0 || authority.has_live_lifetime_tickets()
		|| authority.has_pending_native_plans() {
		return error(err_render_native_renderer_unavailable)
	}
	authority.app_identity = app_identity
	authority.app_lifetime_token = app_lifetime_token
	authority.owner_thread_identity = owner_thread_identity
}

fn (mut authority NativeOperationAuthority) advance_renderer_attempt(app_identity u64, renderer_attempt_token u64) ! {
	if app_identity == 0 || renderer_attempt_token == 0 || authority.app_identity != app_identity
		|| authority.app_lifetime_token == 0
		|| renderer_attempt_token <= authority.app_lifetime_token
		|| !authority.owner_thread_is_current() || authority.has_provisional_lifetime_acquisition() {
		return error(err_render_native_renderer_unavailable)
	}
	if authority.renderer_attempt_token != 0 {
		if renderer_attempt_token <= authority.renderer_attempt_token
			|| authority.has_pending_native_plans_for_scope(.renderer_attempt, authority.renderer_attempt_token)
			|| authority.has_live_lifetime_tickets_for_scope(.renderer_attempt, authority.renderer_attempt_token) {
			return error(err_render_native_renderer_unavailable)
		}
	}
	authority.renderer_attempt_token = renderer_attempt_token
}

fn (authority &NativeOperationAuthority) owner_thread_is_current() bool {
	return authority.owner_thread_identity != 0
		&& authority.owner_thread_identity == sync.thread_id()
}

fn (authority &NativeOperationAuthority) authority_token(scope NativeOperationAuthorityScope) u64 {
	return match scope {
		.app_lifetime { authority.app_lifetime_token }
		.renderer_attempt { authority.renderer_attempt_token }
		.none { u64(0) }
	}
}

fn (authority &NativeOperationAuthority) authority_scope_is_current(scope NativeOperationAuthorityScope, token u64) bool {
	return authority.app_identity != 0 && token != 0 && authority.authority_token(scope) == token
}

fn (mut authority NativeOperationAuthority) reserve_ordinals_for(scope NativeOperationAuthorityScope, count u64) !NativeOrdinalRange {
	if !authority.owner_thread_is_current() {
		return error(err_render_native_renderer_unavailable)
	}
	token := authority.authority_token(scope)
	if !authority.authority_scope_is_current(scope, token) {
		return error(err_render_native_renderer_unavailable)
	}
	if count == 0 || authority.sequence_exhausted || authority.next_ordinal == 0 {
		authority.sequence_exhausted = true
		authority.terminal_cause = .sequence_exhausted
		return error(err_render_native_renderer_unavailable)
	}
	remaining := ~u64(0) - authority.next_ordinal
	if count - 1 > remaining {
		authority.next_ordinal = 0
		authority.sequence_exhausted = true
		authority.terminal_cause = .sequence_exhausted
		return error(err_render_native_renderer_unavailable)
	}
	first := authority.next_ordinal
	if count - 1 == remaining {
		authority.next_ordinal = 0
	} else {
		authority.next_ordinal += count
	}
	return NativeOrdinalRange{
		authority_scope:       scope
		authority_token:       token
		app_identity:          authority.app_identity
		proof_generation:      if authority.proof == unsafe { nil } {
			u64(0)
		} else {
			authority.proof.generation
		}
		owner_thread_identity: authority.owner_thread_identity
		first:                 first
		count:                 count
	}
}

fn (mut authority NativeOperationAuthority) reserve_app_lifetime_ordinals(count u64) !NativeOrdinalRange {
	return authority.reserve_ordinals_for(.app_lifetime, count)
}

fn (mut authority NativeOperationAuthority) reserve_renderer_attempt_ordinals(count u64) !NativeOrdinalRange {
	return authority.reserve_ordinals_for(.renderer_attempt, count)
}

fn (mut authority NativeOperationAuthority) reserve_ordinals(count u64) !NativeOrdinalRange {
	return authority.reserve_renderer_attempt_ordinals(count)
}

fn (mut reservation NativeOrdinalRange) materialize(authority &NativeOperationAuthority, domain NativeRenderDomain, operation NativeRenderOperation, seed NativeOperationSeed) !NativeOperationContext {
	if !authority.owner_thread_is_current() || reservation.owner_thread_identity == 0
		|| reservation.owner_thread_identity != authority.owner_thread_identity
		|| reservation.owner_thread_identity != sync.thread_id() || reservation.first == 0
		|| reservation.used >= reservation.count {
		return error(err_render_native_renderer_unavailable)
	}
	if reservation.app_identity != authority.app_identity
		|| !authority.authority_scope_is_current(reservation.authority_scope, reservation.authority_token)
		|| (reservation.proof_generation == 0) != (authority.proof == unsafe { nil })
		|| (reservation.proof_generation != 0
		&& reservation.proof_generation != authority.proof.generation) {
		return error(err_render_native_renderer_unavailable)
	}
	ordinal := reservation.first + reservation.used
	reservation.used++
	return NativeOperationContext{
		authority_scope:        reservation.authority_scope
		authority_token:        reservation.authority_token
		renderer_attempt_token: if reservation.authority_scope == .renderer_attempt {
			reservation.authority_token
		} else {
			u64(0)
		}
		app_identity:           reservation.app_identity
		presence_mask:          seed.presence_mask
		domain:                 domain
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

fn (mut reservation NativeOrdinalRange) skip(count u64) ! {
	if reservation.owner_thread_identity == 0
		|| reservation.owner_thread_identity != sync.thread_id() || count == 0
		|| reservation.first == 0 || count > reservation.count - reservation.used {
		return error(err_render_native_renderer_unavailable)
	}
	reservation.used += count
}

fn (mut reservation NativeOrdinalRange) split_tail(count u64) !NativeOrdinalRange {
	if reservation.owner_thread_identity == 0
		|| reservation.owner_thread_identity != sync.thread_id() || count == 0
		|| reservation.first == 0 || reservation.used != 0 || count >= reservation.count {
		return error(err_render_native_renderer_unavailable)
	}
	reservation.count -= count
	return NativeOrdinalRange{
		authority_scope:       reservation.authority_scope
		authority_token:       reservation.authority_token
		app_identity:          reservation.app_identity
		proof_generation:      reservation.proof_generation
		owner_thread_identity: reservation.owner_thread_identity
		first:                 reservation.first + reservation.count
		count:                 count
	}
}

fn native_lifetime_release_operation(kind NativeLifetimeReleaseKind) NativeRenderOperation {
	return match kind {
		.egl_surface, .wayland_egl_window, .wayland_surface, .wayland_frame_callback { .surface_destroy }
		.egl_context { .context_destroy }
		.egl_display { .display_terminate }
		.egl_thread { .release_thread }
		.com_reference, .metal_device, .appkit_state { .object_release }
		.appkit_autorelease_pool { .render_batch_end }
		.metal_drawable { .clear_state }
		.none { .none }
	}
}

fn native_lifetime_release_domain(kind NativeLifetimeReleaseKind) NativeRenderDomain {
	return match kind {
		.egl_surface, .egl_context, .egl_display, .egl_thread { .egl }
		.wayland_egl_window, .wayland_surface, .wayland_frame_callback { .wayland }
		.com_reference { .dxgi }
		.metal_device, .appkit_state, .appkit_autorelease_pool, .metal_drawable { .metal }
		.none { .none }
	}
}

fn (authority &NativeOperationAuthority) has_live_lifetime_tickets() bool {
	return authority.lifetime_tickets.len != 0
}

fn (authority &NativeOperationAuthority) has_provisional_lifetime_acquisition() bool {
	for ticket in authority.lifetime_tickets {
		if ticket.state in [.acquiring, .provisional_bound]
			|| (ticket.state in [.releasing, .native_released]
			&& ticket.acquisition_context.ordinal != 0) {
			return true
		}
	}
	return false
}

fn (authority &NativeOperationAuthority) lifetime_ticket_index(ticket_id u64) ?int {
	if ticket_id == 0 {
		return none
	}
	for index, ticket in authority.lifetime_tickets {
		if ticket.ticket_id == ticket_id {
			return index
		}
	}
	return none
}

fn (authority &NativeOperationAuthority) has_pending_native_plans() bool {
	if authority.proof == unsafe { nil } {
		return false
	}
	for entry in authority.proof.plan {
		if entry.proof_generation == authority.proof.generation
			&& (entry.armed || entry.listener_registration_pending) {
			return true
		}
	}
	return false
}

fn (authority &NativeOperationAuthority) has_pending_native_plans_for_scope(scope NativeOperationAuthorityScope, token u64) bool {
	if authority.proof == unsafe { nil } || token == 0 {
		return false
	}
	for entry in authority.proof.plan {
		if entry.proof_generation == authority.proof.generation
			&& (entry.armed || entry.listener_registration_pending)
			&& entry.context.authority_scope == scope && entry.context.authority_token == token {
			return true
		}
	}
	return false
}

fn (authority &NativeOperationAuthority) has_live_lifetime_tickets_for_scope(scope NativeOperationAuthorityScope, token u64) bool {
	if token == 0 {
		return false
	}
	for ticket in authority.lifetime_tickets {
		if ticket.authority_scope == scope && ticket.authority_token == token {
			return true
		}
	}
	return false
}

fn (authority &NativeOperationAuthority) has_live_tickets_for_proof_generation(generation u64) bool {
	if generation == 0 {
		return false
	}
	for ticket in authority.lifetime_tickets {
		if ticket.proof_generation == generation {
			return true
		}
	}
	return false
}

fn (authority &NativeOperationAuthority) proof_accepts_context(context NativeOperationContext) bool {
	if authority.proof == unsafe { nil } || !authority.proof.accepting_plans
		|| authority.proof.generation == 0 || context.ordinal == 0
		|| context.app_identity != authority.app_identity
		|| !authority.authority_scope_is_current(context.authority_scope, context.authority_token)
		|| (context.authority_scope == .renderer_attempt
		&& context.renderer_attempt_token != context.authority_token)
		|| (context.authority_scope == .app_lifetime && context.renderer_attempt_token != 0) {
		return false
	}
	if authority.proof.ordinal_floor == 0 {
		return false
	}
	return context.ordinal >= authority.proof.ordinal_floor
}

fn native_operation_context_matches_seed(context NativeOperationContext, seed NativeOperationSeed) bool {
	return context.presence_mask == seed.presence_mask && context.call_site == seed.call_site
		&& context.scope == seed.scope && context.window == seed.window
		&& context.target_generation == seed.target_generation
		&& context.target_identity == seed.target_identity
		&& context.batch_epoch == seed.batch_epoch
		&& context.window_lease_epoch == seed.window_lease_epoch
		&& context.target_lease_epoch == seed.target_lease_epoch
}

fn (authority &NativeOperationAuthority) lifetime_acquisition_proof_generation_is_current(ticket NativeLifetimeReleaseTicket) bool {
	current_generation := if authority.proof == unsafe { nil } {
		u64(0)
	} else {
		authority.proof.generation
	}
	return ticket.proof_generation == current_generation
}

fn (authority &NativeOperationAuthority) lifetime_acquisition_transaction_matches_ticket(ticket NativeLifetimeReleaseTicket, transaction NativeLifetimeAcquisitionTransaction, required_state NativeLifetimeTicketState) bool {
	if !authority.owner_thread_is_current() || ticket.state != required_state
		|| ticket.ticket_id == 0 {
		return false
	}
	if ticket.ticket_id != transaction.ticket_id || ticket.ticket_id != ticket.context.ordinal
		|| ticket.ticket_id != transaction.release_context.ordinal {
		return false
	}
	if ticket.app_identity != authority.app_identity
		|| ticket.app_identity != transaction.app_identity
		|| ticket.authority_scope != .app_lifetime {
		return false
	}
	if ticket.authority_scope != transaction.authority_scope
		|| ticket.authority_token != transaction.authority_token
		|| !authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token) {
		return false
	}
	if ticket.domain != .metal || ticket.release_kind != .metal_device
		|| ticket.release_kind != transaction.release_kind {
		return false
	}
	if ticket.required_parent_identity != 0 || ticket.parent_authority_scope != .none
		|| ticket.parent_authority_token != 0 {
		return false
	}
	if ticket.release_evidence_recorded || ticket.native_identity != transaction.native_identity
		|| !authority.lifetime_acquisition_proof_generation_is_current(ticket) {
		return false
	}
	if !native_operation_contexts_identical(ticket.acquisition_context, transaction.acquisition_context)
		|| !native_operation_contexts_identical(ticket.context, transaction.release_context) {
		return false
	}
	if transaction.acquisition_context.authority_scope != .renderer_attempt
		|| !authority.authority_scope_is_current(transaction.acquisition_context.authority_scope, transaction.acquisition_context.authority_token)
		|| transaction.acquisition_context.renderer_attempt_token != transaction.acquisition_context.authority_token {
		return false
	}
	if transaction.acquisition_context.app_identity != authority.app_identity
		|| transaction.acquisition_context.domain != .metal
		|| transaction.acquisition_context.operation != .device_create {
		return false
	}
	if transaction.release_context.authority_scope != .app_lifetime
		|| !authority.authority_scope_is_current(transaction.release_context.authority_scope, transaction.release_context.authority_token)
		|| transaction.release_context.renderer_attempt_token != 0 {
		return false
	}
	if transaction.release_context.app_identity != authority.app_identity
		|| transaction.release_context.domain != .metal
		|| transaction.release_context.operation != .object_release {
		return false
	}
	if transaction.acquisition_context.ordinal == 0
		|| transaction.release_context.ordinal != transaction.acquisition_context.ordinal + 1
		|| !native_operation_context_matches_seed(transaction.acquisition_context, ticket.owner_seed) {
		return false
	}
	expected_release_seed := if ticket.native_identity == 0 {
		ticket.owner_seed
	} else {
		ticket.owner_seed.with_target_identity(ticket.native_identity)
	}
	if !native_operation_context_matches_seed(transaction.release_context, expected_release_seed) {
		return false
	}
	if required_state == .acquiring {
		return ticket.native_identity == 0 && transaction.native_identity == 0
	}
	return ticket.native_identity != 0 && transaction.native_identity != 0
}

fn (authority &NativeOperationAuthority) lifetime_acquisition_claim_matches_ticket(ticket NativeLifetimeReleaseTicket, claim NativeLifetimeAcquisitionClaim, required_state NativeLifetimeTicketState) bool {
	if !authority.owner_thread_is_current() || ticket.state != required_state
		|| ticket.ticket_id == 0 {
		return false
	}
	if ticket.ticket_id != claim.ticket_id || ticket.ticket_id != ticket.context.ordinal
		|| ticket.app_identity != authority.app_identity
		|| ticket.app_identity != claim.app_identity {
		return false
	}
	if ticket.authority_scope != .app_lifetime || ticket.authority_scope != claim.authority_scope
		|| ticket.authority_token != claim.authority_token
		|| !authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token) {
		return false
	}
	if ticket.domain != .metal || ticket.release_kind != .metal_device
		|| ticket.release_kind != claim.release_kind || ticket.required_parent_identity != 0 {
		return false
	}
	if ticket.parent_authority_scope != .none || ticket.parent_authority_token != 0
		|| ticket.native_identity == 0 || ticket.native_identity != claim.native_identity {
		return false
	}
	if !authority.lifetime_acquisition_proof_generation_is_current(ticket)
		|| !native_operation_contexts_identical(ticket.context, claim.context) {
		return false
	}
	if !native_operation_context_matches_seed(ticket.acquisition_context, ticket.owner_seed)
		|| !native_operation_context_matches_seed(ticket.context, ticket.owner_seed.with_target_identity(ticket.native_identity)) {
		return false
	}
	if ticket.acquisition_context.ordinal == 0
		|| ticket.context.ordinal != ticket.acquisition_context.ordinal + 1 {
		return false
	}
	if ticket.context.authority_scope != ticket.authority_scope
		|| ticket.context.authority_token != ticket.authority_token
		|| ticket.context.renderer_attempt_token != 0 {
		return false
	}
	if ticket.context.app_identity != authority.app_identity || ticket.context.domain != .metal
		|| ticket.context.operation != .object_release {
		return false
	}
	if ticket.acquisition_context.authority_scope != .renderer_attempt
		|| !authority.authority_scope_is_current(ticket.acquisition_context.authority_scope, ticket.acquisition_context.authority_token)
		|| ticket.acquisition_context.renderer_attempt_token != ticket.acquisition_context.authority_token {
		return false
	}
	if ticket.acquisition_context.app_identity != authority.app_identity
		|| ticket.acquisition_context.domain != .metal
		|| ticket.acquisition_context.operation != .device_create {
		return false
	}
	return true
}

fn (mut authority NativeOperationAuthority) begin_lifetime_acquisition(acquisition_context NativeOperationContext, release_context NativeOperationContext, release_kind NativeLifetimeReleaseKind, required_parent_identity u64, owner_seed NativeOperationSeed) !NativeLifetimeAcquisitionTransaction {
	seed := owner_seed.without_target_identity()
	if !authority.owner_thread_is_current() || authority.app_identity == 0
		|| authority.has_provisional_lifetime_acquisition() || release_kind != .metal_device
		|| required_parent_identity != 0 || acquisition_context.ordinal == 0
		|| release_context.ordinal == 0
		|| release_context.ordinal != acquisition_context.ordinal + 1
		|| acquisition_context.app_identity != authority.app_identity
		|| acquisition_context.authority_scope != .renderer_attempt
		|| !authority.authority_scope_is_current(acquisition_context.authority_scope, acquisition_context.authority_token)
		|| acquisition_context.renderer_attempt_token != acquisition_context.authority_token
		|| acquisition_context.domain != .metal || acquisition_context.operation != .device_create
		|| !native_operation_context_matches_seed(acquisition_context, seed)
		|| release_context.app_identity != authority.app_identity
		|| release_context.authority_scope != .app_lifetime
		|| !authority.authority_scope_is_current(release_context.authority_scope, release_context.authority_token)
		|| release_context.renderer_attempt_token != 0 || release_context.domain != .metal
		|| release_context.operation != .object_release
		|| !native_operation_context_matches_seed(release_context, seed) {
		return error(err_render_native_renderer_unavailable)
	}
	if _ := authority.lifetime_ticket_index(release_context.ordinal) {
		return error(err_render_native_renderer_unavailable)
	}
	authority.lifetime_tickets << NativeLifetimeReleaseTicket{
		ticket_id:           release_context.ordinal
		app_identity:        authority.app_identity
		authority_scope:     release_context.authority_scope
		authority_token:     release_context.authority_token
		domain:              release_context.domain
		release_kind:        release_kind
		owner_seed:          seed
		proof_generation:    if authority.proof == unsafe { nil } {
			u64(0)
		} else {
			authority.proof.generation
		}
		acquisition_context: acquisition_context
		context:             release_context
		state:               .acquiring
	}
	return NativeLifetimeAcquisitionTransaction{
		ticket_id:           release_context.ordinal
		app_identity:        authority.app_identity
		authority_scope:     release_context.authority_scope
		authority_token:     release_context.authority_token
		release_kind:        release_kind
		acquisition_context: acquisition_context
		release_context:     release_context
	}
}

fn (mut authority NativeOperationAuthority) bind_lifetime_acquisition_actual(transaction NativeLifetimeAcquisitionTransaction, native_identity u64) ?NativeLifetimeAcquisitionTransaction {
	index := authority.lifetime_ticket_index(transaction.ticket_id) or { return none }
	ticket := authority.lifetime_tickets[index]
	if !authority.lifetime_acquisition_transaction_matches_ticket(ticket, transaction, .acquiring) {
		return none
	}
	if native_identity == 0 {
		authority.lifetime_tickets[index].state = .burned
		authority.lifetime_tickets.delete(index)
		return NativeLifetimeAcquisitionTransaction{}
	}
	release_context := NativeOperationContext{
		...transaction.release_context
		presence_mask:   transaction.release_context.presence_mask | native_context_has_target_identity
		target_identity: native_identity
	}
	authority.lifetime_tickets[index].native_identity = native_identity
	authority.lifetime_tickets[index].context = release_context
	authority.lifetime_tickets[index].release_evidence_recorded = false
	authority.lifetime_tickets[index].state = .provisional_bound
	return NativeLifetimeAcquisitionTransaction{
		...transaction
		release_context: release_context
		native_identity: native_identity
	}
}

fn (mut authority NativeOperationAuthority) cancel_lifetime_acquisition(transaction NativeLifetimeAcquisitionTransaction) bool {
	index := authority.lifetime_ticket_index(transaction.ticket_id) or { return false }
	if !authority.lifetime_acquisition_transaction_matches_ticket(authority.lifetime_tickets[index],
		transaction, .acquiring) {
		return false
	}
	authority.lifetime_tickets[index].state = .burned
	authority.lifetime_tickets.delete(index)
	return true
}

fn (mut authority NativeOperationAuthority) abandon_physically_released_lifetime_acquisition(transaction NativeLifetimeAcquisitionTransaction) bool {
	if !authority.owner_thread_is_current() || transaction.ticket_id == 0
		|| transaction.native_identity != 0 || transaction.app_identity != authority.app_identity
		|| transaction.release_kind != .metal_device {
		return false
	}
	index := authority.lifetime_ticket_index(transaction.ticket_id) or { return false }
	ticket := authority.lifetime_tickets[index]
	if !authority.lifetime_acquisition_transaction_matches_ticket(ticket, transaction, .acquiring)
		|| ticket.native_identity != 0 {
		return false
	}
	authority.lifetime_tickets[index].state = .abandoned
	authority.lifetime_tickets.delete(index)
	return true
}

fn (mut authority NativeOperationAuthority) commit_lifetime_acquisition(transaction NativeLifetimeAcquisitionTransaction) bool {
	index := authority.lifetime_ticket_index(transaction.ticket_id) or { return false }
	if !authority.lifetime_acquisition_transaction_matches_ticket(authority.lifetime_tickets[index],
		transaction, .provisional_bound) {
		return false
	}
	authority.lifetime_tickets[index].state = .bound
	return true
}

fn (mut authority NativeOperationAuthority) claim_provisional_lifetime_release(transaction NativeLifetimeAcquisitionTransaction) ?NativeLifetimeAcquisitionClaim {
	index := authority.lifetime_ticket_index(transaction.ticket_id) or { return none }
	if !authority.lifetime_acquisition_transaction_matches_ticket(authority.lifetime_tickets[index],
		transaction, .provisional_bound) {
		return none
	}
	ticket := authority.lifetime_tickets[index]
	authority.lifetime_tickets[index].release_evidence_recorded = false
	authority.lifetime_tickets[index].state = .releasing
	return NativeLifetimeAcquisitionClaim{
		ticket_id:       ticket.ticket_id
		app_identity:    ticket.app_identity
		authority_scope: ticket.authority_scope
		authority_token: ticket.authority_token
		release_kind:    ticket.release_kind
		context:         ticket.context
		native_identity: ticket.native_identity
	}
}

fn (mut authority NativeOperationAuthority) mark_provisional_lifetime_native_released(claim NativeLifetimeAcquisitionClaim) bool {
	index := authority.lifetime_ticket_index(claim.ticket_id) or { return false }
	if !authority.lifetime_acquisition_claim_matches_ticket(authority.lifetime_tickets[index], claim, .releasing)
		|| authority.lifetime_tickets[index].release_evidence_recorded {
		return false
	}
	authority.lifetime_tickets[index].state = .native_released
	return true
}

fn (mut authority NativeOperationAuthority) record_provisional_lifetime_release_evidence(claim NativeLifetimeAcquisitionClaim, capture NativePrimitiveCapture, result NativeRenderResult) bool {
	index := authority.lifetime_ticket_index(claim.ticket_id) or { return false }
	if !authority.lifetime_acquisition_claim_matches_ticket(authority.lifetime_tickets[index], claim, .native_released)
		|| authority.lifetime_tickets[index].release_evidence_recorded
		|| !native_operation_contexts_identical(result.context, claim.context) {
		return false
	}
	authority.record_release(claim.context, capture, result)
	authority.lifetime_tickets[index].release_evidence_recorded = true
	return true
}

fn (mut authority NativeOperationAuthority) retire_provisional_lifetime_release(claim NativeLifetimeAcquisitionClaim) bool {
	index := authority.lifetime_ticket_index(claim.ticket_id) or { return false }
	if !authority.lifetime_acquisition_claim_matches_ticket(authority.lifetime_tickets[index], claim, .native_released)
		|| !authority.lifetime_tickets[index].release_evidence_recorded {
		return false
	}
	authority.lifetime_tickets[index].state = .released
	authority.lifetime_tickets.delete(index)
	return true
}

fn (mut authority NativeOperationAuthority) reserve_lifetime_ticket(context NativeOperationContext, release_kind NativeLifetimeReleaseKind, owner_seed NativeOperationSeed) !u64 {
	if !authority.owner_thread_is_current() || authority.has_provisional_lifetime_acquisition()
		|| context.ordinal == 0 || context.app_identity != authority.app_identity
		|| !authority.authority_scope_is_current(context.authority_scope, context.authority_token)
		|| context.domain != native_lifetime_release_domain(release_kind)
		|| context.operation != native_lifetime_release_operation(release_kind)
		|| context.target_identity != 0
		|| context.presence_mask & native_context_has_target_identity != 0 {
		return error(err_render_native_renderer_unavailable)
	}
	for ticket in authority.lifetime_tickets {
		if ticket.ticket_id == context.ordinal {
			return error(err_render_native_renderer_unavailable)
		}
	}
	authority.lifetime_tickets << NativeLifetimeReleaseTicket{
		ticket_id:        context.ordinal
		app_identity:     authority.app_identity
		authority_scope:  context.authority_scope
		authority_token:  context.authority_token
		domain:           context.domain
		release_kind:     release_kind
		owner_seed:       owner_seed.without_target_identity()
		proof_generation: if authority.proof == unsafe { nil } {
			u64(0)
		} else {
			authority.proof.generation
		}
		context:          context
		state:            .reserved
	}
	return context.ordinal
}

fn (mut authority NativeOperationAuthority) bind_lifetime_ticket(ticket_id u64, native_identity u64, required_parent_identity u64) {
	if !authority.owner_thread_is_current() || authority.has_provisional_lifetime_acquisition()
		|| ticket_id == 0 || native_identity == 0 {
		return
	}
	for index in 0 .. authority.lifetime_tickets.len {
		ticket := authority.lifetime_tickets[index]
		if ticket.ticket_id != ticket_id {
			continue
		}
		if ticket.state != .reserved || ticket.native_identity != 0 {
			return
		}
		authority.lifetime_tickets[index].native_identity = native_identity
		authority.lifetime_tickets[index].required_parent_identity = required_parent_identity
		authority.lifetime_tickets[index].context = NativeOperationContext{
			...ticket.context
			presence_mask:   ticket.context.presence_mask | native_context_has_target_identity
			target_identity: native_identity
		}
		parent := authority.lifetime_parent_authority(ticket.authority_scope,
			ticket.authority_token, ticket.release_kind, required_parent_identity) or {
			authority.lifetime_tickets[index].state = .abandoned
			return
		}
		authority.lifetime_tickets[index].parent_authority_scope = parent.scope
		authority.lifetime_tickets[index].parent_authority_token = parent.token
		invalid_owner := ticket.app_identity != authority.app_identity
			|| !authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
		authority.lifetime_tickets[index].state = if invalid_owner { .abandoned } else { .bound }
		return
	}
}

fn native_lifetime_scope_may_depend_on(child NativeOperationAuthorityScope, parent NativeOperationAuthorityScope) bool {
	return child == parent || (child == .renderer_attempt && parent == .app_lifetime)
}

fn (authority &NativeOperationAuthority) lifetime_parent_authority(child_scope NativeOperationAuthorityScope, child_token u64, release_kind NativeLifetimeReleaseKind, required_parent_identity u64) ?NativeLifetimeParentAuthority {
	if release_kind == .wayland_egl_window {
		if required_parent_identity == 0 {
			return none
		}
		for ticket in authority.lifetime_tickets {
			if ticket.release_kind == .wayland_surface && ticket.state == .bound
				&& ticket.app_identity == authority.app_identity
				&& ticket.native_identity == required_parent_identity
				&& native_lifetime_scope_may_depend_on(child_scope, ticket.authority_scope)
				&& authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
				&& (ticket.authority_scope != child_scope || ticket.authority_token == child_token) {
				return NativeLifetimeParentAuthority{
					scope: ticket.authority_scope
					token: ticket.authority_token
				}
			}
		}
		return NativeLifetimeParentAuthority{}
	}
	if release_kind in [.wayland_surface, .wayland_frame_callback] {
		if required_parent_identity == 0 {
			return none
		}
		return NativeLifetimeParentAuthority{}
	}
	if release_kind == .metal_drawable {
		if required_parent_identity == 0 {
			return none
		}
		for ticket in authority.lifetime_tickets {
			if ticket.release_kind == .appkit_state && ticket.state == .bound
				&& ticket.app_identity == authority.app_identity
				&& ticket.native_identity == required_parent_identity
				&& native_lifetime_scope_may_depend_on(child_scope, ticket.authority_scope)
				&& authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
				&& (ticket.authority_scope != child_scope || ticket.authority_token == child_token) {
				return NativeLifetimeParentAuthority{
					scope: ticket.authority_scope
					token: ticket.authority_token
				}
			}
		}
		return none
	}
	if release_kind !in [.egl_surface, .egl_context] {
		if required_parent_identity != 0 {
			return none
		}
		return NativeLifetimeParentAuthority{}
	}
	if required_parent_identity == 0 {
		return none
	}
	for ticket in authority.lifetime_tickets {
		if ticket.release_kind == .egl_display && ticket.state == .bound
			&& ticket.app_identity == authority.app_identity
			&& ticket.native_identity == required_parent_identity
			&& native_lifetime_scope_may_depend_on(child_scope, ticket.authority_scope)
			&& authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
			&& (ticket.authority_scope != child_scope || ticket.authority_token == child_token) {
			return NativeLifetimeParentAuthority{
				scope: ticket.authority_scope
				token: ticket.authority_token
			}
		}
	}
	return none
}

fn (mut authority NativeOperationAuthority) burn_lifetime_ticket(ticket_id u64) bool {
	if !authority.owner_thread_is_current() || ticket_id == 0 {
		return false
	}
	for index in 0 .. authority.lifetime_tickets.len {
		ticket := authority.lifetime_tickets[index]
		if ticket.ticket_id != ticket_id {
			continue
		}
		if ticket.state != .reserved || ticket.native_identity != 0 {
			return false
		}
		authority.lifetime_tickets[index].state = .burned
		authority.lifetime_tickets.delete(index)
		return true
	}
	return false
}

fn (mut authority NativeOperationAuthority) claim_lifetime_release(ticket_id u64, release_kind NativeLifetimeReleaseKind, native_identity u64, required_parent_identity u64) ?NativeLifetimeReleaseClaim {
	if !authority.owner_thread_is_current() || authority.has_provisional_lifetime_acquisition()
		|| ticket_id == 0 || native_identity == 0 {
		return none
	}
	for index in 0 .. authority.lifetime_tickets.len {
		ticket := authority.lifetime_tickets[index]
		if ticket.ticket_id != ticket_id {
			continue
		}
		if ticket.state != .bound || ticket.release_kind != release_kind
			|| ticket.app_identity != authority.app_identity
			|| !authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
			|| ticket.native_identity != native_identity
			|| ticket.required_parent_identity != required_parent_identity
			|| ticket.context.target_identity != native_identity
			|| ticket.context.ordinal != ticket_id {
			return none
		}
		parent := authority.lifetime_parent_authority(ticket.authority_scope,
			ticket.authority_token, ticket.release_kind, required_parent_identity) or {
			return none
		}
		if parent.scope != ticket.parent_authority_scope
			|| parent.token != ticket.parent_authority_token
			|| authority.lifetime_ticket_has_live_children(ticket) {
			return none
		}
		authority.lifetime_tickets[index].state = .releasing
		return NativeLifetimeReleaseClaim{
			ticket_id:                ticket_id
			registry_index:           index
			authority_scope:          ticket.authority_scope
			authority_token:          ticket.authority_token
			release_kind:             release_kind
			context:                  ticket.context
			native_identity:          native_identity
			required_parent_identity: required_parent_identity
		}
	}
	return none
}

fn (authority &NativeOperationAuthority) lifetime_ticket_has_live_children(parent NativeLifetimeReleaseTicket) bool {
	for ticket in authority.lifetime_tickets {
		if ticket.ticket_id != parent.ticket_id
			&& ticket.state in [.reserved, .acquiring, .provisional_bound, .bound, .releasing, .native_released]
			&& ticket.required_parent_identity == parent.native_identity
			&& ticket.parent_authority_scope == parent.authority_scope
			&& ticket.parent_authority_token == parent.authority_token {
			return true
		}
	}
	return false
}

fn (mut authority NativeOperationAuthority) complete_claimed_lifetime_release(ticket_id u64, release_kind NativeLifetimeReleaseKind, native_identity u64, required_parent_identity u64, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation, health NativeRendererHealth, error_text string) NativeRenderResult {
	if !authority.owner_thread_is_current() {
		return native_render_outcome(native_lifetime_release_domain(release_kind),
			native_lifetime_release_operation(release_kind), .renderer, .operation_failed, 0, 0,
			err_render_native_renderer_unavailable)
	}
	for index, ticket in authority.lifetime_tickets {
		if ticket.ticket_id == ticket_id && ticket.state == .releasing
			&& ticket.release_kind == release_kind && ticket.app_identity == authority.app_identity
			&& authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
			&& ticket.native_identity == native_identity
			&& ticket.required_parent_identity == required_parent_identity
			&& ticket.context.target_identity == native_identity
			&& ticket.context.ordinal == ticket_id {
			claim := NativeLifetimeReleaseClaim{
				ticket_id:                ticket_id
				registry_index:           index
				authority_scope:          ticket.authority_scope
				authority_token:          ticket.authority_token
				release_kind:             release_kind
				context:                  ticket.context
				native_identity:          native_identity
				required_parent_identity: required_parent_identity
			}
			return authority.complete_lifetime_release(claim, raw, validation, health, error_text)
		}
	}
	return native_render_outcome(native_lifetime_release_domain(release_kind),
		native_lifetime_release_operation(release_kind), .renderer, .operation_failed, 0, 0,
		err_render_native_renderer_unavailable)
}

fn (mut authority NativeOperationAuthority) complete_lifetime_release(claim NativeLifetimeReleaseClaim, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation, health NativeRendererHealth, error_text string) NativeRenderResult {
	if !authority.owner_thread_is_current() {
		return native_render_outcome(claim.context.domain, claim.context.operation,
			claim.context.scope, .operation_failed, 0, 0, err_render_native_renderer_unavailable)
	}
	capture := authority.capture_call(claim.context, raw)
	result := match claim.context.domain {
		.egl {
			authority.accept_egl(claim.context, capture, validation)
		}
		.dxgi {
			authority.accept_dxgi(claim.context, capture, validation)
		}
		.wayland {
			authority.accept_wayland(claim.context, capture, validation, error_text)
		}
		.metal {
			authority.accept_metal(claim.context, capture, validation, error_text)
		}
		else {
			native_render_outcome(claim.context.domain, claim.context.operation,
				claim.context.scope, .operation_failed, 0, 0,
				err_render_native_renderer_unavailable)
		}
	}

	authority.record_health_latch(claim.context, health)
	if !authority.complete_lifetime_release_from_capture(claim, capture, result) {
		return native_render_outcome(claim.context.domain, claim.context.operation,
			claim.context.scope, .operation_failed, 0, 0, err_render_native_renderer_unavailable)
	}
	return result
}

fn (mut authority NativeOperationAuthority) complete_lifetime_release_from_capture(claim NativeLifetimeReleaseClaim, capture NativePrimitiveCapture, result NativeRenderResult) bool {
	if !authority.owner_thread_is_current() || claim.registry_index < 0
		|| claim.registry_index >= authority.lifetime_tickets.len {
		return false
	}
	ticket := authority.lifetime_tickets[claim.registry_index]
	if ticket.ticket_id != claim.ticket_id || ticket.state != .releasing
		|| ticket.release_kind != claim.release_kind
		|| ticket.app_identity != authority.app_identity
		|| ticket.authority_scope != claim.authority_scope
		|| ticket.authority_token != claim.authority_token
		|| !authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
		|| ticket.native_identity != claim.native_identity
		|| ticket.required_parent_identity != claim.required_parent_identity {
		return false
	}
	authority.record_release(claim.context, capture, result)
	authority.lifetime_tickets[claim.registry_index].state = .released
	authority.lifetime_tickets.delete(claim.registry_index)
	return true
}

fn (mut authority NativeOperationAuthority) stage_lifetime_native_release(claim NativeLifetimeReleaseClaim, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation, error_text string) NativeRenderResult {
	authority.mark_claimed_lifetime_native_released(claim)
	capture := authority.capture_call(claim.context, raw)
	result := match claim.context.domain {
		.dxgi {
			authority.accept_dxgi(claim.context, capture, validation)
		}
		.metal {
			authority.accept_metal(claim.context, capture, validation, error_text)
		}
		else {
			native_render_outcome(claim.context.domain, claim.context.operation,
				claim.context.scope, .operation_failed, 0, 0,
				err_render_native_renderer_unavailable)
		}
	}

	authority.record_native_released_lifetime_evidence(claim, capture, result)
	return result
}

// A release claim is the canonical validation boundary. No fallible state check
// is allowed between the physical call and these deterministic transitions.
fn (mut authority NativeOperationAuthority) mark_claimed_lifetime_native_released(claim NativeLifetimeReleaseClaim) {
	authority.lifetime_tickets[claim.registry_index].state = .native_released
}

fn (mut authority NativeOperationAuthority) record_native_released_lifetime_evidence(claim NativeLifetimeReleaseClaim, capture NativePrimitiveCapture, result NativeRenderResult) {
	authority.record_release(claim.context, capture, result)
}

fn (authority &NativeOperationAuthority) claim_native_released_lifetime_retirement(ticket_id u64, release_kind NativeLifetimeReleaseKind) ?NativeLifetimeReleaseClaim {
	if !authority.owner_thread_is_current() || ticket_id == 0 {
		return none
	}
	for index, ticket in authority.lifetime_tickets {
		if ticket.ticket_id != ticket_id {
			continue
		}
		if ticket.state != .native_released || ticket.release_kind != release_kind
			|| ticket.app_identity != authority.app_identity
			|| !authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token) {
			return none
		}
		return NativeLifetimeReleaseClaim{
			ticket_id:                ticket.ticket_id
			registry_index:           index
			authority_scope:          ticket.authority_scope
			authority_token:          ticket.authority_token
			release_kind:             ticket.release_kind
			context:                  ticket.context
			native_identity:          ticket.native_identity
			required_parent_identity: ticket.required_parent_identity
		}
	}
	return none
}

fn (mut authority NativeOperationAuthority) retire_native_released_lifetime_claim(claim NativeLifetimeReleaseClaim) {
	authority.lifetime_tickets[claim.registry_index].state = .released
	authority.lifetime_tickets.delete(claim.registry_index)
}

fn (mut authority NativeOperationAuthority) acknowledge_abandoned_lifetime_ticket(ticket_id u64, release_kind NativeLifetimeReleaseKind, native_identity u64, required_parent_identity u64) bool {
	if !authority.owner_thread_is_current() || ticket_id == 0 {
		return false
	}
	for index, ticket in authority.lifetime_tickets {
		if ticket.ticket_id == ticket_id && ticket.release_kind == release_kind
			&& ticket.state == .abandoned && ticket.app_identity == authority.app_identity
			&& authority.authority_scope_is_current(ticket.authority_scope, ticket.authority_token)
			&& ticket.native_identity == native_identity
			&& ticket.required_parent_identity == required_parent_identity
			&& ticket.context.target_identity == native_identity
			&& ticket.context.ordinal == ticket_id {
			authority.lifetime_tickets.delete(index)
			return true
		}
	}
	return false
}

fn (mut authority NativeOperationAuthority) abandon_egl_display_lifetime(display_identity u64) {
	if !authority.owner_thread_is_current() || display_identity == 0 {
		return
	}
	for index in 0 .. authority.lifetime_tickets.len {
		ticket := authority.lifetime_tickets[index]
		if ticket.domain != .egl || ticket.state != .bound || ticket.release_kind == .egl_thread
			|| ticket.app_identity != authority.app_identity {
			continue
		}
		if ticket.native_identity == display_identity
			|| ticket.required_parent_identity == display_identity {
			authority.lifetime_tickets[index].state = .abandoned
		}
	}
}

fn native_operation_contexts_identical(left NativeOperationContext, right NativeOperationContext) bool {
	return left.authority_scope == right.authority_scope
		&& left.authority_token == right.authority_token
		&& left.renderer_attempt_token == right.renderer_attempt_token
		&& left.app_identity == right.app_identity && left.presence_mask == right.presence_mask
		&& left.domain == right.domain && left.operation == right.operation
		&& left.call_site == right.call_site && left.scope == right.scope
		&& left.window == right.window && left.target_generation == right.target_generation
		&& left.target_identity == right.target_identity && left.batch_epoch == right.batch_epoch
		&& left.window_lease_epoch == right.window_lease_epoch
		&& left.target_lease_epoch == right.target_lease_epoch && left.ordinal == right.ordinal
}

fn (mut authority NativeOperationAuthority) inject(context NativeOperationContext, actual NativePrimitiveEvidence) NativePrimitiveEvidence {
	if !authority.owner_thread_is_current() || authority.proof == unsafe { nil } {
		return actual
	}
	for index in 0 .. native_primitive_plan_capacity {
		entry := authority.proof.plan[index]
		if entry.proof_generation == authority.proof.generation && entry.armed
			&& native_operation_contexts_identical(entry.context, context) {
			authority.proof.plan[index].armed = false
			return entry.evidence
		}
	}
	for index in 0 .. native_primitive_plan_capacity {
		entry := authority.proof.plan[index]
		if entry.proof_generation == authority.proof.generation && entry.armed
			&& native_anchor_acquisition_selector_matches(entry.context, context) {
			authority.proof.plan[index].armed = false
			return entry.evidence
		}
	}
	return actual
}

fn (mut authority NativeOperationAuthority) arm(context NativeOperationContext, evidence NativePrimitiveEvidence) bool {
	if !authority.owner_thread_is_current() || !authority.proof_accepts_context(context) {
		return false
	}
	for index in 0 .. native_primitive_plan_capacity {
		entry := authority.proof.plan[index]
		if entry.proof_generation == authority.proof.generation && entry.armed
			&& native_operation_plan_contexts_overlap(entry.context, context) {
			return false
		}
		if entry.proof_generation == authority.proof.generation
			&& entry.listener_registration_pending
			&& listener_registration_selector_matches(entry.context, context) {
			return false
		}
	}
	for index in 0 .. native_primitive_plan_capacity {
		if !authority.proof.plan[index].armed
			&& !authority.proof.plan[index].listener_registration_pending {
			authority.proof.plan[index] = NativePrimitivePlanEntry{
				armed:            true
				proof_generation: authority.proof.generation
				context:          context
				evidence:         evidence
			}
			return true
		}
	}
	return false
}

fn native_operation_plan_contexts_overlap(left NativeOperationContext, right NativeOperationContext) bool {
	return native_operation_contexts_identical(left, right)
		|| native_anchor_acquisition_selector_matches(left, right)
		|| native_anchor_acquisition_selector_matches(right, left)
}

fn native_anchor_acquisition_selector_matches(selector NativeOperationContext, context NativeOperationContext) bool {
	return selector.authority_scope == .renderer_attempt
		&& selector.authority_scope == context.authority_scope
		&& selector.authority_token == context.authority_token
		&& selector.renderer_attempt_token == selector.authority_token
		&& selector.renderer_attempt_token == context.renderer_attempt_token
		&& selector.app_identity == context.app_identity
		&& selector.presence_mask == native_context_has_target_generation
		&& context.presence_mask == (native_context_has_target_generation | native_context_has_target_identity)
		&& selector.domain in [.wayland, .egl] && selector.domain == context.domain
		&& selector.operation == .anchor_surface_create && selector.operation == context.operation
		&& selector.call_site == .anchor_create && selector.call_site == context.call_site
		&& selector.scope == .anchor && selector.scope == context.scope
		&& selector.window == WindowId{} && context.window == WindowId{}
		&& selector.target_generation != 0
		&& selector.target_generation == context.target_generation && selector.target_identity == 0
		&& context.target_identity != 0 && selector.batch_epoch == 0 && context.batch_epoch == 0
		&& selector.window_lease_epoch == 0 && context.window_lease_epoch == 0
		&& selector.target_lease_epoch == 0 && context.target_lease_epoch == 0
		&& selector.ordinal == context.ordinal
}

fn (mut authority NativeOperationAuthority) arm_anchor_acquisition_for_test(selector NativeOperationContext, evidence NativePrimitiveEvidence) bool {
	if selector.presence_mask != native_context_has_target_generation
		|| selector.target_identity != 0 || selector.domain !in [.wayland, .egl]
		|| selector.operation != .anchor_surface_create || selector.call_site != .anchor_create
		|| selector.scope != .anchor || selector.window != WindowId{}
		|| selector.target_generation == 0 || selector.batch_epoch != 0
		|| selector.window_lease_epoch != 0 || selector.target_lease_epoch != 0 {
		return false
	}
	return authority.arm(selector, evidence)
}

fn listener_registration_selector_matches(selector NativeOperationContext, context NativeOperationContext) bool {
	return selector.authority_scope == context.authority_scope
		&& selector.authority_token == context.authority_token
		&& selector.renderer_attempt_token == context.renderer_attempt_token
		&& selector.app_identity == context.app_identity
		&& (selector.presence_mask | native_context_has_target_identity) == context.presence_mask
		&& selector.domain == context.domain && selector.operation == context.operation
		&& selector.call_site == context.call_site && selector.scope == context.scope
		&& selector.window == context.window
		&& selector.target_generation == context.target_generation && selector.target_identity == 0
		&& context.target_identity != 0 && selector.batch_epoch == context.batch_epoch
		&& selector.window_lease_epoch == context.window_lease_epoch
		&& selector.target_lease_epoch == context.target_lease_epoch
		&& selector.ordinal == context.ordinal
}

fn (mut authority NativeOperationAuthority) queue_listener_registration_injection(selector NativeOperationContext, evidence NativePrimitiveEvidence) bool {
	if !authority.owner_thread_is_current() || !authority.proof_accepts_context(selector)
		|| selector.authority_scope != .renderer_attempt || selector.renderer_attempt_token == 0
		|| selector.app_identity == 0 || selector.ordinal == 0 || selector.domain != .wayland
		|| selector.operation != .frame_callback || selector.call_site != .window_finalize
		|| selector.scope != .window_target
		|| selector.presence_mask != native_context_window_target_fields
		|| selector.target_identity != 0 {
		return false
	}
	for entry in authority.proof.plan {
		if entry.proof_generation == authority.proof.generation && entry.armed
			&& listener_registration_selector_matches(selector, entry.context) {
			return false
		}
		if entry.proof_generation == authority.proof.generation
			&& entry.listener_registration_pending
			&& native_operation_contexts_identical(entry.context, selector) {
			return false
		}
	}
	for index in 0 .. native_primitive_plan_capacity {
		entry := authority.proof.plan[index]
		if !entry.armed && !entry.listener_registration_pending {
			authority.proof.plan[index] = NativePrimitivePlanEntry{
				listener_registration_pending: true
				proof_generation:              authority.proof.generation
				context:                       selector
				evidence:                      evidence
			}
			return true
		}
	}
	return false
}

fn (mut authority NativeOperationAuthority) arm_listener_registration(context NativeOperationContext) bool {
	if !authority.owner_thread_is_current() || authority.proof == unsafe { nil }
		|| authority.proof.generation == 0 || context.app_identity != authority.app_identity
		|| context.authority_scope != .renderer_attempt
		|| !authority.authority_scope_is_current(context.authority_scope, context.authority_token)
		|| context.renderer_attempt_token != context.authority_token
		|| authority.proof.ordinal_floor == 0 || context.ordinal < authority.proof.ordinal_floor
		|| context.domain != .wayland || context.operation != .frame_callback
		|| (context.presence_mask & native_context_has_target_identity) == 0
		|| context.target_identity == 0 {
		return false
	}
	for entry in authority.proof.plan {
		if entry.proof_generation == authority.proof.generation && entry.armed
			&& native_operation_contexts_identical(entry.context, context) {
			return false
		}
	}
	for index in 0 .. native_primitive_plan_capacity {
		if authority.proof.plan[index].proof_generation == authority.proof.generation
			&& authority.proof.plan[index].listener_registration_pending
			&& listener_registration_selector_matches(authority.proof.plan[index].context, context) {
			authority.proof.plan[index].context = context
			authority.proof.plan[index].armed = true
			authority.proof.plan[index].listener_registration_pending = false
			return true
		}
	}
	return false
}

fn (mut authority NativeOperationAuthority) record(milestone NativeOperationTraceMilestone, context NativeOperationContext, actual NativePrimitiveEvidence, effective NativePrimitiveEvidence, local_validation NativeLocalValidation, result NativeRenderResult) {
	if !authority.owner_thread_is_current() {
		return
	}
	authority.record_entry(NativeOperationTraceEntry{
		milestone:        milestone
		context:          context
		actual:           actual
		effective:        effective
		local_validation: local_validation
		result:           result
	})
}

fn (mut authority NativeOperationAuthority) record_health_latch(context NativeOperationContext, health NativeRendererHealth) {
	if !authority.owner_thread_is_current() || context.ordinal == 0 {
		return
	}
	authority.record_entry(NativeOperationTraceEntry{
		milestone: .health_latched
		context:   context
		health:    health
	})
}

fn (mut authority NativeOperationAuthority) record_entry(entry NativeOperationTraceEntry) {
	if !authority.owner_thread_is_current() || authority.proof == unsafe { nil } {
		return
	}
	if authority.proof.trace_len >= native_operation_trace_capacity {
		authority.proof.trace_overflow = true
		return
	}
	authority.proof.trace[authority.proof.trace_len] = entry
	authority.proof.trace_len++
}

fn (mut authority NativeOperationAuthority) capture_call(context NativeOperationContext, raw C.VMultiwindowNativePrimitive) NativePrimitiveCapture {
	return authority.capture_primitive(context, raw)
}

fn (mut authority NativeOperationAuthority) capture_evidence(context NativeOperationContext, raw C.VMultiwindowNativePrimitive) NativePrimitiveCapture {
	return authority.capture_primitive(context, raw)
}

fn (mut authority NativeOperationAuthority) capture_primitive(context NativeOperationContext, raw C.VMultiwindowNativePrimitive) NativePrimitiveCapture {
	actual := native_primitive_from_c(raw)
	if !authority.owner_thread_is_current() {
		return NativePrimitiveCapture{
			actual:    actual
			effective: actual
		}
	}
	authority.record(.real_call, context, NativePrimitiveEvidence{}, NativePrimitiveEvidence{},
		.none, NativeRenderResult{})
	authority.record(.actual_primitive, context, actual, NativePrimitiveEvidence{}, .none, NativeRenderResult{})
	effective := authority.inject(context, actual)
	authority.record(.effective_primitive, context, NativePrimitiveEvidence{}, effective, .none, NativeRenderResult{})
	return NativePrimitiveCapture{
		actual:    actual
		effective: effective
	}
}

fn (mut authority NativeOperationAuthority) record_acceptance(result NativeRenderResult) {
	if !authority.owner_thread_is_current() {
		return
	}
	authority.record(.acceptance, result.context, result.actual_primitive, result.primitive,
		result.local_validation, result)
}

fn (mut authority NativeOperationAuthority) record_release(context NativeOperationContext, capture NativePrimitiveCapture, result NativeRenderResult) {
	if !authority.owner_thread_is_current() {
		return
	}
	authority.record(.authority_release, context, capture.actual, capture.effective,
		result.local_validation, result)
}

fn native_validation_after_injection(context NativeOperationContext, evidence NativePrimitiveEvidence, requested NativeLocalValidation) NativeLocalValidation {
	if requested != .none {
		return requested
	}
	native_succeeded := evidence.has(native_valid_return_value)
		&& !dxgi_hresult_failed(evidence.return_value)
	egl_succeeded := evidence.has(native_valid_return_value) && evidence.return_value == 1
	match context.operation {
		.display_acquire, .display_query, .context_create, .window_surface_create, .frame_callback {
			if context.domain == .wayland && evidence.has(native_valid_handle)
				&& evidence.handle == 0 {
				return .null_output
			}
			if evidence.has(native_valid_handle) && evidence.handle == 0
				&& (!evidence.has(native_valid_egl_error) || evidence.egl_error == 0x3000)
				&& (native_succeeded || !evidence.has(native_valid_return_value)) {
				return .null_output
			}
		}
		.config_choose {
			if egl_succeeded && evidence.has(native_valid_observed_count)
				&& evidence.observed_count == 0 {
				return .zero_count
			}
			if egl_succeeded && evidence.has(native_valid_handle) && evidence.handle == 0 {
				return .null_output
			}
		}
		.config_visual {
			if egl_succeeded && evidence.has(native_valid_selected_value)
				&& evidence.selected_value == 0 {
				return .missing_visual
			}
		}
		.device_create, .swapchain_create, .backbuffer_acquire, .color_texture_create,
		.render_view_create, .depth_texture_create, .depth_view_create, .device_query,
		.adapter_acquire, .factory_acquire, .anchor_surface_create, .render_batch_begin {
			if context.domain == .wayland && evidence.has(native_valid_handle)
				&& evidence.handle == 0 {
				return .null_output
			}
			if context.domain == .metal && evidence.has(native_valid_handle) && evidence.handle == 0 {
				return .null_output
			}
			if native_succeeded && evidence.has(native_valid_handle) && evidence.handle == 0 {
				return .null_output
			}
			if context.domain == .dxgi && context.operation == .device_create && native_succeeded
				&& evidence.object_identity_0 == 0 {
				return .null_output
			}
		}
		.drawable_acquire {
			if evidence.handle == 0 || evidence.object_identity_0 == 0
				|| evidence.observed_count == 0 || evidence.selected_value <= 0 {
				return .null_output
			}
		}
		.display_poll {
			if evidence.has(native_valid_observed_flags) && evidence.observed_flags & u64(0x38) != 0 {
				return .bad_revents
			}
		}
		else {}
	}

	return .none
}

fn egl_capture_requires_error(context NativeOperationContext, capture NativePrimitiveCapture) bool {
	if context.operation in [.current_draw_query, .current_read_query, .current_context_query,
		.egl_error_query] {
		return false
	}
	return egl_primitive_failed(capture.actual) || egl_primitive_failed(capture.effective)
}

fn egl_primitive_failed(evidence NativePrimitiveEvidence) bool {
	if evidence.has(native_valid_return_value) {
		return evidence.return_value != 1
	}
	if evidence.has(native_valid_handle) {
		return evidence.handle == 0
	}
	return true
}

fn dxgi_capture_requires_removal(capture NativePrimitiveCapture) bool {
	return !capture.actual.has(native_valid_return_value)
		|| dxgi_hresult_failed(capture.actual.return_value)
		|| !capture.effective.has(native_valid_return_value)
		|| dxgi_hresult_failed(capture.effective.return_value)
}

fn (mut authority NativeOperationAuthority) accept_egl(context NativeOperationContext, capture NativePrimitiveCapture, requested_validation NativeLocalValidation) NativeRenderResult {
	local_validation := native_validation_after_injection(context, capture.effective,
		requested_validation)
	result := native_result_from_egl(context, capture, local_validation)
	authority.record_acceptance(result)
	return result
}

fn (mut authority NativeOperationAuthority) accept_egl_context_requirements(context NativeOperationContext, capture NativePrimitiveCapture, initialize NativeRenderResult) NativeRenderResult {
	mut local_validation := native_validation_after_injection(context, capture.effective, .none)
	if local_validation == .none
		&& !egl_core_context_attributes_supported(initialize, capture.effective) {
		local_validation = .context_incompatible
	}
	result := native_result_from_egl(context, capture, local_validation)
	authority.record_acceptance(result)
	return result
}

fn (mut authority NativeOperationAuthority) accept_egl_binding_context(context NativeOperationContext, capture NativePrimitiveCapture, draw NativeRenderResult, read NativeRenderResult, expected_draw u64, expected_read u64, expected_context u64) NativeRenderResult {
	mut local_validation := native_validation_after_injection(context, capture.effective, .none)
	if local_validation == .none && (draw.primitive.handle != expected_draw
		|| read.primitive.handle != expected_read
		|| capture.effective.handle != expected_context) {
		local_validation = .binding_mismatch
	}
	result := native_result_from_egl(context, capture, local_validation)
	authority.record_acceptance(result)
	return result
}

fn (mut authority NativeOperationAuthority) accept_dxgi(context NativeOperationContext, capture NativePrimitiveCapture, requested_validation NativeLocalValidation) NativeRenderResult {
	local_validation := native_validation_after_injection(context, capture.effective,
		requested_validation)
	result := native_result_from_dxgi(context, capture, local_validation)
	authority.record_acceptance(result)
	return result
}

fn (mut authority NativeOperationAuthority) accept_wayland(context NativeOperationContext, capture NativePrimitiveCapture, requested_validation NativeLocalValidation, error_text string) NativeRenderResult {
	local_validation := native_validation_after_injection(context, capture.effective,
		requested_validation)
	result := native_result_from_wayland(context, capture, local_validation, error_text)
	authority.record_acceptance(result)
	return result
}

fn (mut authority NativeOperationAuthority) accept_metal(context NativeOperationContext, capture NativePrimitiveCapture, requested_validation NativeLocalValidation, error_text string) NativeRenderResult {
	local_validation := native_validation_after_injection(context, capture.effective,
		requested_validation)
	result := native_result_from_metal(context, capture, local_validation, error_text)
	authority.record_acceptance(result)
	return result
}

fn native_capture_with_egl_error(primary NativePrimitiveCapture, query NativePrimitiveCapture) NativePrimitiveCapture {
	return NativePrimitiveCapture{
		actual:    native_primitive_with_egl_error(primary.actual, query.actual)
		effective: native_primitive_with_egl_error(primary.effective, query.effective)
	}
}

fn native_capture_with_dxgi_removal(primary NativePrimitiveCapture, query NativePrimitiveCapture) NativePrimitiveCapture {
	return NativePrimitiveCapture{
		actual:    native_primitive_with_dxgi_removal(primary.actual, query.actual)
		effective: native_primitive_with_dxgi_removal(primary.effective, query.effective)
	}
}

fn native_capture_with_wayland_display_error(primary NativePrimitiveCapture, query NativePrimitiveCapture) NativePrimitiveCapture {
	return NativePrimitiveCapture{
		actual:    native_primitive_with_wayland_display_error(primary.actual, query.actual)
		effective: native_primitive_with_wayland_display_error(primary.effective, query.effective)
	}
}

fn native_primitive_with_egl_error(primary NativePrimitiveEvidence, query NativePrimitiveEvidence) NativePrimitiveEvidence {
	if !query.has(native_valid_egl_error) {
		return primary
	}
	return NativePrimitiveEvidence{
		...primary
		valid_mask: primary.valid_mask | native_valid_egl_error
		egl_error:  query.egl_error
	}
}

fn native_primitive_with_dxgi_removal(primary NativePrimitiveEvidence, query NativePrimitiveEvidence) NativePrimitiveEvidence {
	if !query.has(native_valid_dxgi_removal_reason) {
		return primary
	}
	return NativePrimitiveEvidence{
		...primary
		valid_mask:          primary.valid_mask | native_valid_dxgi_removal_reason
		dxgi_removal_reason: query.dxgi_removal_reason
	}
}

fn native_primitive_with_wayland_display_error(primary NativePrimitiveEvidence, query NativePrimitiveEvidence) NativePrimitiveEvidence {
	if !query.has(native_valid_wayland_display_error) {
		return primary
	}
	return NativePrimitiveEvidence{
		...primary
		valid_mask:            primary.valid_mask | native_valid_wayland_display_error
		wayland_display_error: query.wayland_display_error
	}
}

fn native_primitive_from_c(raw C.VMultiwindowNativePrimitive) NativePrimitiveEvidence {
	return NativePrimitiveEvidence{
		valid_mask:            raw.valid_mask
		return_value:          raw.return_value
		handle:                raw.handle
		egl_error:             raw.egl_error
		native_errno:          raw.native_errno
		wayland_display_error: raw.wayland_display_error
		dxgi_removal_reason:   raw.dxgi_removal_reason
		observed_count:        raw.observed_count
		observed_flags:        raw.observed_flags
		selected_value:        raw.selected_value
		object_identity_0:     raw.object_identity_0
		object_identity_1:     raw.object_identity_1
		object_identity_2:     raw.object_identity_2
	}
}
