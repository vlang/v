module bin

import crypto.sha256

pub const source_resolve_backoff_seconds = [0, 15, 45]
pub const source_connect_timeout_seconds = 10
pub const source_total_timeout_seconds = 60
pub const source_recovery_period_seconds = 86_400

// SourceFailureKind separates silent transient outages from actionable configuration defects.
pub enum SourceFailureKind {
	dns
	connectivity
	tls_transient
	timeout
	http_429
	http_5xx
	missing_ref
	bad_url
	authentication
	integrity
}

// SourceMode is the durable resolver cadence.
pub enum SourceMode {
	monthly
	upstream_recovery_daily
}

// HandoffState is the durable receiver dispatch lifecycle.
pub enum HandoffState {
	pending
	dispatched
	blocked
	complete
}

// RecoverySubjectModel is the exact native-gate or revalidation subject carried by a handoff.
pub struct RecoverySubjectModel {
pub:
	consumer_id            string
	consumer_kind          string
	intent_or_operation_id string
	target_id              string
	subject_generation     i64
	initial_run_mode       string
	remediation_trigger    RemediationTriggerModel
	sha                    string
	tree                   string
	original_ref           string
	input_fingerprint      string
	artifact_fingerprint   string
	manifest_hash          string
	digests                []DigestModel
}

// ReceiverRunCandidate contains independently re-fetched same-repository Actions facts.
pub struct ReceiverRunCandidate {
pub:
	run_id        i64
	run_attempt   int
	repository    string
	workflow_id   i64
	workflow_path string
	workflow_ref  string
	event         string
	head_sha      string
	run_name      string
	created_at    string
	conclusion    string
	output_digest string
	deadline      string
}

// RecoveryHandoffModel is the append-only durable source-recovery handoff.
pub struct RecoveryHandoffModel {
pub mut:
	handoff_id                 string
	handoff_ordinal            int
	predecessor_handoff_id     string
	successor_handoff_id       string
	audience                   string
	recovery_operation_id      string
	consumer_type              string
	resume_capability          string
	intent_or_operation_id     string
	subject_hash               string
	subject                    RecoverySubjectModel
	subject_generation         i64
	expected_ledger_generation i64
	expected_canonical_head    string
	subject_ref_head           string
	receiver_repository        string
	workflow_id                i64
	workflow_path              string
	workflow_ref               string
	event                      string
	receiver_run_name          string
	state                      HandoffState
	dispatch_generation        int
	dispatch_operation_ids     []string
	ack_operation_id           string
	selected_run_id            i64
	selected_run_attempt       int
	receiver_master_sha        string
	receiver_conclusion        string
	receiver_output_digest     string
	deadline                   string
	terminal_outcome           string
}

// RecoverySuccessor contains the atomic H1 terminal and H2 pending result.
pub struct RecoverySuccessor {
pub:
	predecessor RecoveryHandoffModel
	successor   RecoveryHandoffModel
	active_id   string
}

// NativeRecoveryAtomicResult is the single-CAS H1/H2/native-execution generation projection.
pub struct NativeRecoveryAtomicResult {
pub:
	chain                RecoverySuccessor
	native_gate          NativeGateModel
	target               TargetModel
	resulting_generation i64
}

// HandoffAckAtomicResult is the one-CAS projection for receiver selection and target generation.
pub struct HandoffAckAtomicResult {
pub:
	handoff              RecoveryHandoffModel
	target               TargetModel
	resulting_generation i64
}

// HandoffCreateAtomicResult persists the initial handoff and target pointer in one CAS projection.
pub struct HandoffCreateAtomicResult {
pub:
	handoff              RecoveryHandoffModel
	target               TargetModel
	resulting_generation i64
}

// ReceiverRetryProof proves that the selected run reached an infrastructure terminal state after
// its persisted deadline; callers cannot turn a merely pending run into an immediate retry.
pub struct ReceiverRetryProof {
pub:
	operation_id        string
	expected_generation i64
	observed_at         string
	deadline            string
	evidence_digest     string
}

// source_failure_is_transient reports failures that must never open or update an issue.
pub fn source_failure_is_transient(kind SourceFailureKind) bool {
	return kind in [.dns, .connectivity, .tls_transient, .timeout, .http_429, .http_5xx]
}

// next_source_mode applies the resolver-only cadence transition.
pub fn next_source_mode(current SourceMode, resolved bool, functional_result_known bool) SourceMode {
	if !resolved {
		return .upstream_recovery_daily
	}
	if functional_result_known {
		return .monthly
	}
	return current
}

// source_retry_due enforces at most one lightweight recovery resolution per 24-hour period.
pub fn source_retry_due(mode SourceMode, last_attempt_unix i64, now_unix i64) bool {
	return mode == .upstream_recovery_daily && now_unix >= last_attempt_unix
		&& now_unix - last_attempt_unix >= source_recovery_period_seconds
}

// create_recovery_handoff_atomic creates the first pending handoff and its active target pointer
// in the same generation bump.
pub fn create_recovery_handoff_atomic(audience string, recovery_operation_id string,
	intent_or_operation_id string, subject RecoverySubjectModel, subject_generation i64,
	consumer_type string, resume_capability string, workflow_id i64, target TargetModel,
	operation_id string) !HandoffCreateAtomicResult {
	validate_target_model(target)!
	native_subject := native_subject_from_recovery(subject)
	subject_hash := recovery_subject_hash(subject)!
	if !is_lower_hex_64(operation_id) || target.active_recovery_handoff_id != ''
		|| target.active_native_subject != native_subject
		|| target.active_subject_hash != subject_hash || target.target_id != subject.target_id
		|| target.input_fingerprint != subject.input_fingerprint
		|| target.canonical_observed_sha == '' {
		return error('recovery handoff creation is not bound to the exact active target consumer')
	}
	next_target_base := advance_target_protocol_generation(target, operation_id,
		'handoff_create_${subject_hash}')!
	handoff := build_recovery_handoff(audience, recovery_operation_id, intent_or_operation_id,
		subject, subject_generation, next_target_base.generation, consumer_type, resume_capability,
		workflow_id, target.canonical_observed_sha)!
	mut next_target := next_target_base
	next_target.active_recovery_handoff_id = handoff.handoff_id
	validate_target_model(next_target)!
	return HandoffCreateAtomicResult{
		handoff:              handoff
		target:               next_target
		resulting_generation: next_target.generation
	}
}

fn build_recovery_handoff(audience string, recovery_operation_id string,
	intent_or_operation_id string, subject RecoverySubjectModel, subject_generation i64,
	expected_ledger_generation i64, consumer_type string, resume_capability string,
	workflow_id i64, expected_canonical_head string) !RecoveryHandoffModel {
	validate_recovery_routing(consumer_type, resume_capability)!
	validate_recovery_subject(subject)!
	if subject.consumer_id != intent_or_operation_id
		|| subject.intent_or_operation_id != intent_or_operation_id
		|| subject.subject_generation != subject_generation
		|| expected_ledger_generation < subject_generation
		|| !is_lower_hex_40(expected_canonical_head) {
		return error('recovery handoff does not preserve its original consumer, generation, or HEAD')
	}
	subject_hash := recovery_subject_hash(subject)!
	handoff_id := deterministic_handoff_id(audience, recovery_operation_id, intent_or_operation_id,
		subject_hash, 0)
	handoff := RecoveryHandoffModel{
		handoff_id:                 handoff_id
		audience:                   audience
		recovery_operation_id:      recovery_operation_id
		consumer_type:              consumer_type
		resume_capability:          resume_capability
		intent_or_operation_id:     intent_or_operation_id
		subject_hash:               subject_hash
		subject:                    subject
		subject_generation:         subject_generation
		expected_ledger_generation: expected_ledger_generation
		expected_canonical_head:    expected_canonical_head
		subject_ref_head:           subject.sha
		receiver_repository:        'vlang/v'
		workflow_id:                workflow_id
		workflow_path:              recovery_workflow_path(resume_capability)
		workflow_ref:               'master'
		event:                      'workflow_dispatch'
		receiver_run_name:          'tccbin-recovery-${handoff_id}'
		state:                      .pending
	}
	validate_handoff(handoff)!
	return handoff
}

// record_handoff_dispatch_atomic records one of two logical dispatches in the same target CAS.
pub fn record_handoff_dispatch_atomic(current RecoveryHandoffModel, target TargetModel,
	dispatch_operation_id string) !HandoffAckAtomicResult {
	validate_handoff(current)!
	validate_target_model(target)!
	if !is_lower_hex_64(dispatch_operation_id) {
		return error('handoff dispatch operation ID is invalid')
	}
	if dispatch_operation_id in current.dispatch_operation_ids {
		if target.generation == current.expected_ledger_generation
			&& target.active_recovery_handoff_id == current.handoff_id
			&& target.last_operation_id == dispatch_operation_id {
			return HandoffAckAtomicResult{
				handoff:              current
				target:               target
				resulting_generation: target.generation
			}
		}
		return error('handoff dispatch replay differs from its persisted target CAS')
	}
	if current.state != .pending || current.dispatch_generation >= handoff_max_dispatch_generations
		|| target.generation != current.expected_ledger_generation
		|| target.active_recovery_handoff_id != current.handoff_id {
		return error('handoff dispatch is not pending or its two generations are exhausted')
	}
	mut next := current
	next.dispatch_operation_ids << dispatch_operation_id
	next.dispatch_generation = next.dispatch_operation_ids.len
	next_target := advance_target_protocol_generation(target, dispatch_operation_id,
		'handoff_dispatch_${current.handoff_id}')!
	next.expected_ledger_generation = next_target.generation
	validate_handoff(next)!
	return HandoffAckAtomicResult{
		handoff:              next
		target:               next_target
		resulting_generation: next_target.generation
	}
}

// acknowledge_handoff_dispatch selects one fully correlated receiver and advances the target and
// handoff generations in one pure CAS projection.
pub fn acknowledge_handoff_dispatch(current RecoveryHandoffModel, run ReceiverRunCandidate,
	target TargetModel, ack_operation_id string) !HandoffAckAtomicResult {
	validate_handoff(current)!
	validate_target_model(target)!
	validate_receiver_run(current, run)!
	if !is_lower_hex_64(ack_operation_id) || target.generation != current.expected_ledger_generation
		|| target.active_recovery_handoff_id != current.handoff_id
		|| target.target_id != current.subject.target_id
		|| target.canonical_observed_sha != current.expected_canonical_head
		|| target.input_fingerprint != current.subject.input_fingerprint
		|| target.active_subject_hash != current.subject_hash
		|| target.active_native_subject != native_subject_from_recovery(current.subject)
		|| current.subject_ref_head != current.subject.sha || current.dispatch_generation == 0 {
		return error('handoff ACK preconditions are stale or undispatched')
	}
	if current.state == .dispatched {
		if receiver_matches_handoff(current, run) && current.ack_operation_id == ack_operation_id {
			return HandoffAckAtomicResult{
				handoff:              current
				target:               target
				resulting_generation: target.generation
			}
		}
		return error('handoff ACK cannot replace its write-once receiver run')
	}
	if current.state != .pending || current.selected_run_id != 0 {
		return error('only a pending unselected handoff may ACK a receiver')
	}
	mut next := current
	next.state = .dispatched
	next.selected_run_id = run.run_id
	next.selected_run_attempt = run.run_attempt
	next.receiver_master_sha = run.head_sha
	next.receiver_conclusion = run.conclusion
	next.receiver_output_digest = run.output_digest
	next.deadline = run.deadline
	next.ack_operation_id = ack_operation_id
	next_target := advance_target_protocol_generation(target, ack_operation_id,
		'handoff_ack_${current.handoff_id}')!
	next.expected_ledger_generation = next_target.generation
	validate_handoff(next)!
	return HandoffAckAtomicResult{
		handoff:              next
		target:               next_target
		resulting_generation: next_target.generation
	}
}

// retry_handoff_after_infra_failure returns to pending through one target/handoff CAS.
pub fn retry_handoff_after_infra_failure(current RecoveryHandoffModel, terminal ReceiverRunCandidate,
	target TargetModel, proof ReceiverRetryProof) !HandoffAckAtomicResult {
	validate_handoff(current)!
	validate_target_model(target)!
	validate_receiver_run(current, terminal)!
	if current.state != .dispatched
		|| current.dispatch_generation >= handoff_max_dispatch_generations
		|| !receiver_matches_handoff(current, terminal)
		|| terminal.conclusion !in ['cancelled', 'timed_out']
		|| proof.expected_generation != current.expected_ledger_generation
		|| target.generation != current.expected_ledger_generation
		|| target.active_recovery_handoff_id != current.handoff_id
		|| proof.deadline != current.deadline || !timestamp_is_exact(proof.deadline)
		|| !timestamp_is_exact(proof.observed_at) || proof.observed_at < proof.deadline
		|| !is_lower_hex_64(proof.operation_id) || !is_lower_hex_64(proof.evidence_digest) {
		return error('handoff cannot be retried after infrastructure failure')
	}
	mut next := current
	next.state = .pending
	next.selected_run_id = 0
	next.selected_run_attempt = 0
	next.receiver_master_sha = ''
	next.receiver_conclusion = ''
	next.receiver_output_digest = ''
	next.deadline = ''
	next.ack_operation_id = ''
	next_target := advance_target_protocol_generation(target, proof.operation_id,
		'handoff_retry_${current.handoff_id}')!
	next.expected_ledger_generation = next_target.generation
	validate_handoff(next)!
	return HandoffAckAtomicResult{
		handoff:              next
		target:               next_target
		resulting_generation: next_target.generation
	}
}

// complete_handoff persists a final revalidated outcome through the same target-ledger CAS.
pub fn complete_handoff(current RecoveryHandoffModel, run ReceiverRunCandidate,
	outcome string, target TargetModel, operation_id string) !HandoffAckAtomicResult {
	validate_handoff(current)!
	validate_target_model(target)!
	validate_receiver_run(current, run)!
	if current.state != .dispatched || !receiver_matches_handoff(current, run) {
		return error('only the acknowledged exact receiver can complete a handoff')
	}
	if outcome !in ['green', 'no_op', 'functional_defect_routed', 'source_waiting']
		|| !is_lower_hex_64(run.output_digest) || !is_lower_hex_64(operation_id)
		|| target.generation != current.expected_ledger_generation
		|| target.active_recovery_handoff_id != current.handoff_id
		|| (outcome in ['green', 'no_op', 'source_waiting'] && run.conclusion != 'success')
		|| (outcome == 'functional_defect_routed' && run.conclusion != 'failure') {
		return error('invalid final handoff outcome or receiver output')
	}
	mut next := current
	next.state = .complete
	next.receiver_conclusion = run.conclusion
	next.receiver_output_digest = run.output_digest
	next.terminal_outcome = outcome
	mut next_target := advance_target_protocol_generation(target, operation_id,
		'handoff_complete_${current.handoff_id}')!
	if outcome != 'source_waiting' {
		next_target.active_recovery_handoff_id = ''
	}
	next.expected_ledger_generation = next_target.generation
	validate_handoff(next)!
	validate_target_model(next_target)!
	return HandoffAckAtomicResult{
		handoff:              next
		target:               next_target
		resulting_generation: next_target.generation
	}
}

// native_green_successor atomically closes H1 and creates the one H2 revalidation successor.
fn native_green_successor(current RecoveryHandoffModel, run ReceiverRunCandidate,
	successor_capability string, successor_workflow_id i64,
	resulting_generation i64) !RecoverySuccessor {
	validate_handoff(current)!
	validate_receiver_run(current, run)!
	if current.state != .dispatched || !receiver_matches_handoff(current, run)
		|| current.resume_capability != 'native_gate' || current.successor_handoff_id != ''
		|| run.conclusion != 'success' || !is_lower_hex_64(run.output_digest) {
		return error('only one exact green native handoff can create a successor')
	}
	if successor_capability !in ['v_smoke', 'evidence_only'] {
		return error('native gate successor must route to final revalidation')
	}
	if resulting_generation != current.expected_ledger_generation + 1 {
		return error('H1 to H2 must share the target CAS generation bump')
	}
	successor_id := deterministic_handoff_id(current.audience, current.recovery_operation_id,
		current.intent_or_operation_id, current.subject_hash, current.handoff_ordinal + 1)
	mut predecessor := current
	predecessor.state = .complete
	predecessor.terminal_outcome = 'native_gate_green_successor'
	predecessor.receiver_conclusion = run.conclusion
	predecessor.receiver_output_digest = run.output_digest
	predecessor.successor_handoff_id = successor_id
	predecessor.expected_ledger_generation = resulting_generation
	successor := RecoveryHandoffModel{
		handoff_id:                 successor_id
		handoff_ordinal:            current.handoff_ordinal + 1
		predecessor_handoff_id:     current.handoff_id
		audience:                   current.audience
		recovery_operation_id:      current.recovery_operation_id
		consumer_type:              current.consumer_type
		resume_capability:          successor_capability
		intent_or_operation_id:     current.intent_or_operation_id
		subject_hash:               current.subject_hash
		subject:                    current.subject
		subject_generation:         current.subject_generation
		expected_ledger_generation: resulting_generation
		expected_canonical_head:    current.expected_canonical_head
		subject_ref_head:           current.subject_ref_head
		receiver_repository:        current.receiver_repository
		workflow_id:                successor_workflow_id
		workflow_path:              recovery_workflow_path(successor_capability)
		workflow_ref:               current.workflow_ref
		event:                      current.event
		receiver_run_name:          'tccbin-recovery-${successor_id}'
		state:                      .pending
	}
	validate_handoff(predecessor)!
	validate_handoff(successor)!
	return RecoverySuccessor{
		predecessor: predecessor
		successor:   successor
		active_id:   successor_id
	}
}

// native_green_successor_atomic advances H1, H2, and native execution in one target generation.
pub fn native_green_successor_atomic(current RecoveryHandoffModel, run ReceiverRunCandidate,
	gate NativeGateModel, successor_capability string, successor_workflow_id i64,
	target TargetModel, operation_id string) !NativeRecoveryAtomicResult {
	validate_handoff(current)!
	validate_native_gate(gate)!
	validate_target_model(target)!
	if !is_lower_hex_64(operation_id) || current.expected_ledger_generation != target.generation
		|| gate.expected_ledger_generation != target.generation
		|| gate.subject_generation != current.subject_generation
		|| gate.subject != native_subject_from_recovery(current.subject)
		|| gate.subject_hash != current.subject_hash || gate.subject_sha != current.subject.sha
		|| gate.selected_conclusion != 'success'
		|| target.active_recovery_handoff_id != current.handoff_id
		|| target.target_id != current.subject.target_id
		|| target.canonical_observed_sha != current.expected_canonical_head
		|| target.active_subject_hash != current.subject_hash
		|| target.active_native_subject != native_subject_from_recovery(current.subject)
		|| current.subject_ref_head != current.subject.sha
		|| gate.epochs[gate.active_gate_epoch].state != .completed {
		return error('native recovery CAS does not bind one completed gate, handoff, and generation')
	}
	mut advanced_target := advance_target_protocol_generation(target, operation_id,
		'native_recovery_successor_${current.handoff_id}')!
	resulting_generation := advanced_target.generation
	chain := native_green_successor(current, run, successor_capability, successor_workflow_id,
		resulting_generation)!
	advanced_target.active_recovery_handoff_id = chain.active_id
	validate_target_model(advanced_target)!
	advanced_gate := advance_gate_ledger_generation(gate, target.generation, resulting_generation)!
	if chain.predecessor.expected_ledger_generation != resulting_generation
		|| chain.successor.expected_ledger_generation != resulting_generation
		|| advanced_gate.expected_ledger_generation != resulting_generation
		|| advanced_target.active_native_gate != advanced_gate {
		return error('native recovery CAS projections did not advance atomically')
	}
	return NativeRecoveryAtomicResult{
		chain:                chain
		native_gate:          advanced_gate
		target:               advanced_target
		resulting_generation: resulting_generation
	}
}

// handoff_returns_to_monthly reports only terminal outcomes that end source recovery mode.
pub fn handoff_returns_to_monthly(handoff RecoveryHandoffModel) bool {
	return handoff.state == .complete
		&& handoff.terminal_outcome in ['green', 'no_op', 'functional_defect_routed']
}

// validate_handoff checks identity, subject, routing, generation, ACK, and append-only invariants.
pub fn validate_handoff(handoff RecoveryHandoffModel) ! {
	validate_recovery_subject(handoff.subject)!
	validate_recovery_routing(handoff.consumer_type, handoff.resume_capability)!
	if handoff.audience != 'vlang/v:tccbin-automation-state'
		|| !is_lower_hex_64(handoff.recovery_operation_id)
		|| !is_lower_hex_64(handoff.intent_or_operation_id)
		|| handoff.subject.consumer_id != handoff.intent_or_operation_id
		|| handoff.subject.intent_or_operation_id != handoff.intent_or_operation_id
		|| handoff.subject_hash != recovery_subject_hash(handoff.subject)!
		|| handoff.handoff_id != deterministic_handoff_id(handoff.audience, handoff.recovery_operation_id, handoff.intent_or_operation_id, handoff.subject_hash, handoff.handoff_ordinal) {
		return error('handoff audience, operation, subject, or deterministic identity is invalid')
	}
	if handoff.handoff_ordinal < 0 || handoff.subject_generation < 0
		|| handoff.subject_generation != handoff.subject.subject_generation
		|| handoff.expected_ledger_generation < handoff.subject_generation
		|| !is_lower_hex_40(handoff.expected_canonical_head)
		|| handoff.subject_ref_head != handoff.subject.sha {
		return error('handoff generations are inconsistent')
	}
	if classify_publisher_ref(handoff.subject.original_ref) == .canonical
		&& handoff.expected_canonical_head != handoff.subject.sha {
		return error('canonical recovery subject and expected canonical HEAD disagree')
	}
	if handoff.dispatch_generation != handoff.dispatch_operation_ids.len
		|| handoff.dispatch_generation < 0
		|| handoff.dispatch_generation > handoff_max_dispatch_generations {
		return error('handoff dispatch generations are duplicated or outside 0..2')
	}
	mut dispatch_ids := []string{}
	for operation_id in handoff.dispatch_operation_ids {
		if !is_lower_hex_64(operation_id) || operation_id in dispatch_ids {
			return error('handoff dispatch operation IDs must be unique and exact')
		}
		dispatch_ids << operation_id
	}
	if handoff.receiver_repository != 'vlang/v' || handoff.workflow_id <= 0
		|| handoff.workflow_path != recovery_workflow_path(handoff.resume_capability)
		|| handoff.workflow_ref != 'master' || handoff.event != 'workflow_dispatch'
		|| handoff.receiver_run_name != 'tccbin-recovery-${handoff.handoff_id}' {
		return error('handoff receiver workflow, ref, event, or run-name binding is invalid')
	}
	if handoff.handoff_ordinal == 0 && handoff.predecessor_handoff_id != '' {
		return error('first handoff cannot have a predecessor')
	}
	if handoff.handoff_ordinal > 0 && !is_lower_hex_64(handoff.predecessor_handoff_id) {
		return error('successor handoff must retain an exact predecessor')
	}
	if handoff.successor_handoff_id != '' && !is_lower_hex_64(handoff.successor_handoff_id) {
		return error('handoff successor identity is invalid')
	}
	if handoff.state == .pending && (handoff.selected_run_id != 0
		|| handoff.selected_run_attempt != 0 || handoff.receiver_master_sha != ''
		|| handoff.receiver_conclusion != '' || handoff.receiver_output_digest != ''
		|| handoff.deadline != '' || handoff.terminal_outcome != ''
		|| handoff.ack_operation_id != '') {
		return error('pending handoff cannot contain receiver ACK or terminal output')
	}
	if handoff.state in [.dispatched, .complete] && (handoff.selected_run_id <= 0
		|| handoff.selected_run_attempt <= 0
		|| !is_lower_hex_40(handoff.receiver_master_sha)
		|| !timestamp_is_exact(handoff.deadline)
		|| !is_lower_hex_64(handoff.ack_operation_id)) {
		return error('acknowledged handoff lacks its exact selected receiver binding')
	}
	if handoff.state == .dispatched && handoff.terminal_outcome != '' {
		return error('dispatched handoff cannot already have a business outcome')
	}
	if handoff.state == .complete && (handoff.terminal_outcome == ''
		|| handoff.receiver_conclusion == ''
		|| !is_lower_hex_64(handoff.receiver_output_digest)) {
		return error('complete handoff lacks its receiver conclusion, output, or terminal outcome')
	}
	if handoff.state == .complete
		&& ((handoff.terminal_outcome in ['green', 'no_op', 'source_waiting', 'native_gate_green_successor']
		&& handoff.receiver_conclusion != 'success')
		|| (handoff.terminal_outcome == 'functional_defect_routed'
		&& handoff.receiver_conclusion != 'failure')) {
		return error('complete handoff outcome contradicts its authenticated run conclusion')
	}
	if handoff.terminal_outcome == 'native_gate_green_successor' {
		if handoff.resume_capability != 'native_gate' || handoff.successor_handoff_id == '' {
			return error('native green outcome must point to its one revalidation successor')
		}
	} else if handoff.state == .complete && handoff.successor_handoff_id != '' {
		return error('only native-gate green may create a successor')
	}
}

// deterministic_handoff_id preserves identity across dispatch retries and CAS bumps.
pub fn deterministic_handoff_id(audience string, recovery_operation_id string,
	intent_or_operation_id string, subject_hash string, ordinal int) string {
	material := [audience, recovery_operation_id, intent_or_operation_id, subject_hash,
		ordinal.str()].join('\x1f')
	return sha256.sum256(material.bytes()).hex()
}

// recovery_subject_hash canonically binds the complete handoff subject.
pub fn recovery_subject_hash(subject RecoverySubjectModel) !string {
	validate_recovery_subject(subject)!
	return native_gate_subject_hash(native_subject_from_recovery(subject))!
}

fn validate_recovery_subject(subject RecoverySubjectModel) ! {
	validate_native_gate_subject(native_subject_from_recovery(subject))!
}

fn native_subject_from_recovery(subject RecoverySubjectModel) NativeGateSubjectModel {
	return NativeGateSubjectModel{
		consumer_id:            subject.consumer_id
		consumer_kind:          subject.consumer_kind
		intent_or_operation_id: subject.intent_or_operation_id
		target_id:              subject.target_id
		subject_generation:     subject.subject_generation
		initial_run_mode:       subject.initial_run_mode
		remediation_trigger:    subject.remediation_trigger
		sha:                    subject.sha
		tree:                   subject.tree
		original_ref:           subject.original_ref
		input_fingerprint:      subject.input_fingerprint
		artifact_fingerprint:   subject.artifact_fingerprint
		manifest_hash:          subject.manifest_hash
		digests:                subject.digests.clone()
	}
}

fn validate_recovery_routing(consumer_type string, capability string) ! {
	if consumer_type !in ['candidate', 'post-validation', 'remediation', 'adopt-current', 'rollback']
		|| capability !in ['native_gate', 'v_smoke', 'evidence_only'] {
		return error('unknown recovery consumer type or resume capability')
	}
	if consumer_type in ['candidate', 'adopt-current', 'rollback'] && capability != 'native_gate' {
		return error('candidate, adoption, and rollback recovery must resume the native gate')
	}
}

fn recovery_workflow_path(capability string) string {
	return if capability == 'native_gate' {
		'.github/workflows/update_tccbin.yml'
	} else {
		'.github/workflows/tccbin_revalidate.yml'
	}
}

fn validate_receiver_run(handoff RecoveryHandoffModel, run ReceiverRunCandidate) ! {
	if run.run_id <= 0 || run.run_attempt <= 0
		|| run.repository != handoff.receiver_repository
		|| run.workflow_id != handoff.workflow_id
		|| run.workflow_path != handoff.workflow_path
		|| run.workflow_ref != handoff.workflow_ref || run.event != handoff.event
		|| !is_lower_hex_40(run.head_sha)
		|| run.run_name != handoff.receiver_run_name
		|| !timestamp_is_exact(run.created_at)
		|| run.conclusion !in ['pending', 'success', 'failure', 'cancelled', 'timed_out']
		|| !timestamp_is_exact(run.deadline) || run.deadline < run.created_at
		|| (run.output_digest != '' && !is_lower_hex_64(run.output_digest)) {
		return error('receiver run does not match the exact handoff audience and workflow subject')
	}
}

fn receiver_matches_handoff(handoff RecoveryHandoffModel, run ReceiverRunCandidate) bool {
	return handoff.selected_run_id == run.run_id && handoff.selected_run_attempt == run.run_attempt
		&& handoff.receiver_master_sha == run.head_sha && handoff.deadline == run.deadline
}

fn advance_target_protocol_generation(current TargetModel, operation_id string,
	transition string) !TargetModel {
	validate_target_model(current)!
	if !is_lower_hex_64(operation_id) || transition == ''
		|| current.applied_operations.any(it.operation_id == operation_id)
		|| current.applied_operations.len >= applied_operation_limit {
		return error('protocol CAS operation is invalid, replayed, or exceeds the retained ledger')
	}
	mut next := current
	next.generation = current.generation + 1
	if native_gate_is_set(next.active_native_gate) {
		next.active_native_gate = advance_gate_ledger_generation(next.active_native_gate,
			current.generation, next.generation)!
	}
	next.last_operation_id = operation_id
	next.last_transition = transition
	next.applied_operations << AppliedOperationModel{
		operation_id:         operation_id
		transition:           transition
		resulting_generation: next.generation
	}
	validate_target_model(next)!
	return next
}
