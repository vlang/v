module bin

// RemediationTriggerModel binds a remediation subject to the exact reviewed push that created it.
pub struct RemediationTriggerModel {
pub:
	repository       string
	ref              string
	before           string
	after            string
	tree             string
	diff_fingerprint string
	owner_domain     string
}

// NativeGateSubjectModel mirrors the complete immutable native-gate subject contract. The hash is
// always recalculated from its canonical JSON projection; callers never supply a free subject hash.
pub struct NativeGateSubjectModel {
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

// GateEpochState is the closed selection lifecycle of one immutable trigger ref.
pub enum GateEpochState {
	open_unselected
	selected
	closed_timed_out
	closed_not_rerunnable
	completed
}

// GateEpochModel records the one expected ref and write-once winning run of an epoch.
pub struct GateEpochModel {
pub mut:
	epoch                        int
	reason                       string
	expected_ref                 string
	trigger_id                   string
	state                        GateEpochState
	selected_run_id              i64
	selected_run_attempt         int
	selected_check_suite_id      i64
	conclusion                   string
	opened_at                    string
	closed_at                    string
	source_recovery_operation_id string
}

// GateRunAuthentication freezes all API identities expected for original and rerun events.
pub struct GateRunAuthentication {
pub:
	repository                      string
	workflow_id                     i64
	workflow_path                   string
	original_actor                  string
	original_actor_integration_id   i64
	rerun_triggering_actor          string
	rerun_triggering_integration_id i64
}

// GateRunCandidate contains the remote facts independently re-fetched before ACK.
pub struct GateRunCandidate {
pub:
	epoch                           int
	run_id                          i64
	run_attempt                     int
	repository                      string
	ref                             string
	sha                             string
	event                           string
	actor                           string
	actor_integration_id            i64
	triggering_actor                string
	triggering_actor_integration_id i64
	check_suite_id                  i64
	workflow_id                     i64
	workflow_path                   string
	created_at                      string
	conclusion                      string
}

// NativeGateModel separates immutable subject/authentication from evolving ledger execution.
pub struct NativeGateModel {
pub mut:
	subject                      NativeGateSubjectModel
	subject_hash                 string
	subject_sha                  string
	subject_generation           i64
	expected_ledger_generation   i64
	authentication               GateRunAuthentication
	active_gate_epoch            int
	epochs                       []GateEpochModel
	gate_runs                    []GateRunCandidate
	ack_operation_ids            []string
	completion_operation_ids     []string
	epoch_close_operation_ids    []string
	selected_run_id              i64
	selected_run_attempt         int
	selected_check_suite_id      i64
	selected_conclusion          string
	infra_retry_count            int
	source_recovery_operation_id string
}

// GateAckAtomicResult is the one-CAS projection shared by native-gate execution and target state.
pub struct GateAckAtomicResult {
pub:
	gate                 NativeGateModel
	target               TargetModel
	resulting_generation i64
}

// GateEpochCloseProof authenticates timeout/not-rerunnable evidence before a retrigger epoch.
pub struct GateEpochCloseProof {
pub:
	operation_id        string
	expected_generation i64
	deadline            string
	observed_at         string
	evidence_digest     string
}

// initial_native_gate creates epoch zero for the exact original or V-remediation ref.
pub fn initial_native_gate(subject NativeGateSubjectModel, expected_ledger_generation i64,
	expected_ref string, reason string, trigger_id string, opened_at string,
	authentication GateRunAuthentication, source_recovery_operation_id string) !NativeGateModel {
	validate_native_gate_subject(subject)!
	validate_gate_authentication(authentication)!
	if reason !in ['original_push', 'initial-v-remediation'] {
		return error('initial native gate reason is outside its closed set')
	}
	expected_initial_reason := if subject.initial_run_mode == 'no_native_push_expected' {
		'initial-v-remediation'
	} else {
		'original_push'
	}
	if reason != expected_initial_reason {
		return error('initial native gate reason differs from the subject run mode')
	}
	validate_epoch_reason(reason, trigger_id, source_recovery_operation_id)!
	validate_gate_trigger_identity(subject.consumer_id, 0, reason, source_recovery_operation_id, 0,
		trigger_id)!
	validate_epoch_ref(reason, expected_ref, trigger_id, subject)!
	if expected_ledger_generation != subject.subject_generation || !timestamp_is_exact(opened_at) {
		return error('native gate initial generation or opening timestamp is invalid')
	}
	gate := NativeGateModel{
		subject:                      subject
		subject_hash:                 native_gate_subject_hash(subject)!
		subject_sha:                  subject.sha
		subject_generation:           subject.subject_generation
		expected_ledger_generation:   expected_ledger_generation
		authentication:               authentication
		epochs:                       [
			GateEpochModel{
				reason:                       reason
				expected_ref:                 expected_ref
				trigger_id:                   trigger_id
				state:                        .open_unselected
				opened_at:                    opened_at
				source_recovery_operation_id: source_recovery_operation_id
			},
		]
		source_recovery_operation_id: source_recovery_operation_id
	}
	validate_native_gate(gate)!
	return gate
}

// acknowledge_gate_run persists only the first exact authenticated run in the active epoch.
pub fn acknowledge_gate_run(current NativeGateModel, run GateRunCandidate,
	target TargetModel, operation_id string) !GateAckAtomicResult {
	validate_native_gate(current)!
	validate_target_model(target)!
	if !is_lower_hex_64(operation_id) || target.generation != current.expected_ledger_generation
		|| target.target_id != current.subject.target_id
		|| target.active_subject_hash != current.subject_hash
		|| target.active_native_subject != current.subject || target.active_native_gate != current {
		return error('gate ACK observed a stale ledger generation')
	}
	if run.epoch != current.active_gate_epoch {
		return error('gate ACK can select only the active epoch')
	}
	validate_gate_run_candidate(current, run)!
	mut next := NativeGateModel{
		...current
		epochs:            current.epochs.clone()
		gate_runs:         current.gate_runs.clone()
		ack_operation_ids: current.ack_operation_ids.clone()
	}
	mut epoch := next.epochs[next.active_gate_epoch]
	if epoch.state == .selected {
		if epoch.selected_run_id != run.run_id || current.selected_run_id != run.run_id {
			return error('active epoch already has a different write-once winning run')
		}
		if run.run_attempt == next.selected_run_attempt {
			mut previous_index := -1
			for index, previous in next.gate_runs {
				if previous.run_id == run.run_id && previous.run_attempt == run.run_attempt
					&& previous.epoch == run.epoch {
					previous_index = index
				}
			}
			if previous_index >= 0 && next.gate_runs[previous_index] == run
				&& next.ack_operation_ids[previous_index] == operation_id {
				return GateAckAtomicResult{
					gate:                 current
					target:               target
					resulting_generation: target.generation
				}
			}
			return error('idempotent gate ACK facts changed for the selected attempt')
		}
		if run.run_attempt != next.selected_run_attempt + 1 || next.infra_retry_count >= 1 {
			return error('the single exact non-source gate rerun is exhausted or non-contiguous')
		}
		next.selected_run_attempt = run.run_attempt
		next.selected_check_suite_id = run.check_suite_id
		next.selected_conclusion = run.conclusion
		epoch.selected_run_attempt = run.run_attempt
		epoch.selected_check_suite_id = run.check_suite_id
		epoch.conclusion = run.conclusion
		next.epochs[next.active_gate_epoch] = epoch
		next.gate_runs << run
		next.ack_operation_ids << operation_id
		next.infra_retry_count++
		next_target := advance_target_protocol_generation(target, operation_id,
			'native_gate_ack_${current.subject_hash}')!
		next.expected_ledger_generation = next_target.generation
		mut projected_target := next_target
		projected_target.active_native_gate = next
		validate_native_gate(next)!
		validate_target_model(projected_target)!
		return GateAckAtomicResult{
			gate:                 next
			target:               projected_target
			resulting_generation: projected_target.generation
		}
	}
	if epoch.state != .open_unselected || epoch.selected_run_id != 0 || current.selected_run_id != 0
		|| run.run_attempt != 1 {
		return error('only an open unselected epoch may elect an attempt-one run')
	}
	epoch.state = .selected
	epoch.selected_run_id = run.run_id
	epoch.selected_run_attempt = run.run_attempt
	epoch.selected_check_suite_id = run.check_suite_id
	epoch.conclusion = run.conclusion
	next.epochs[next.active_gate_epoch] = epoch
	next.gate_runs << run
	next.ack_operation_ids << operation_id
	next.selected_run_id = run.run_id
	next.selected_run_attempt = run.run_attempt
	next.selected_check_suite_id = run.check_suite_id
	next.selected_conclusion = run.conclusion
	next_target := advance_target_protocol_generation(target, operation_id,
		'native_gate_ack_${current.subject_hash}')!
	next.expected_ledger_generation = next_target.generation
	mut projected_target := next_target
	projected_target.active_native_gate = next
	validate_native_gate(next)!
	validate_target_model(projected_target)!
	return GateAckAtomicResult{
		gate:                 next
		target:               projected_target
		resulting_generation: projected_target.generation
	}
}

// close_and_open_gate_epoch archives the prior epoch before one immutable retrigger ref.
pub fn close_and_open_gate_epoch(current NativeGateModel, close_state GateEpochState,
	reason string, expected_ref string, trigger_id string, closed_at string,
	opened_at string, source_recovery_operation_id string, target TargetModel,
	proof GateEpochCloseProof) !GateAckAtomicResult {
	validate_native_gate(current)!
	validate_target_model(target)!
	if close_state !in [.closed_timed_out, .closed_not_rerunnable] {
		return error('epoch may advance only after an explicit timeout or not-rerunnable close')
	}
	if current.infra_retry_count >= 1 && reason == 'missing-run-retry' {
		return error('the single non-source gate retry is exhausted')
	}
	if reason !in ['missing-run-retry', 'source-recovery'] {
		return error('successor gate epoch reason is outside its closed set')
	}
	validate_epoch_reason(reason, trigger_id, source_recovery_operation_id)!
	logical_counter := if reason == 'missing-run-retry' { 1 } else { 0 }
	validate_gate_trigger_identity(current.subject.consumer_id, current.active_gate_epoch + 1,
		reason, source_recovery_operation_id, logical_counter, trigger_id)!
	validate_epoch_ref(reason, expected_ref, trigger_id, current.subject)!
	if !timestamp_is_exact(closed_at) || !timestamp_is_exact(opened_at)
		|| closed_at < active_epoch_opened_at(current) || opened_at < closed_at {
		return error('gate epoch close/open timestamps are invalid or non-monotonic')
	}
	if !is_lower_hex_64(proof.operation_id) || proof.expected_generation != target.generation
		|| target.generation != current.expected_ledger_generation
		|| target.active_subject_hash != current.subject_hash
		|| target.active_native_subject != current.subject || target.active_native_gate != current
		|| proof.observed_at != closed_at || !timestamp_is_exact(proof.deadline)
		|| proof.observed_at < proof.deadline || !is_lower_hex_64(proof.evidence_digest) {
		return error('gate epoch close lacks exact timeout/not-rerunnable evidence and target CAS')
	}
	mut next := NativeGateModel{
		...current
		epochs:                    current.epochs.clone()
		epoch_close_operation_ids: current.epoch_close_operation_ids.clone()
	}
	mut active := next.epochs[next.active_gate_epoch]
	if active.state !in [.open_unselected, .selected] {
		return error('active epoch is already closed')
	}
	active.state = close_state
	active.closed_at = closed_at
	next.epochs[next.active_gate_epoch] = active
	next.active_gate_epoch++
	next.epochs << GateEpochModel{
		epoch:                        next.active_gate_epoch
		reason:                       reason
		expected_ref:                 expected_ref
		trigger_id:                   trigger_id
		state:                        .open_unselected
		opened_at:                    opened_at
		source_recovery_operation_id: source_recovery_operation_id
	}
	next.selected_run_id = 0
	next.selected_run_attempt = 0
	next.selected_check_suite_id = 0
	next.selected_conclusion = ''
	if reason == 'missing-run-retry' {
		next.infra_retry_count++
	}
	next.source_recovery_operation_id = if reason == 'source-recovery' {
		source_recovery_operation_id
	} else {
		''
	}
	next.epoch_close_operation_ids << proof.operation_id
	next_target := advance_target_protocol_generation(target, proof.operation_id,
		'native_gate_epoch_${current.subject_hash}')!
	next.expected_ledger_generation = next_target.generation
	mut projected_target := next_target
	projected_target.active_native_gate = next
	validate_native_gate(next)!
	validate_target_model(projected_target)!
	return GateAckAtomicResult{
		gate:                 next
		target:               projected_target
		resulting_generation: projected_target.generation
	}
}

// complete_gate_epoch re-authenticates and persists one terminal selected conclusion.
pub fn complete_gate_epoch(current NativeGateModel, terminal GateRunCandidate,
	target TargetModel, operation_id string, closed_at string) !GateAckAtomicResult {
	validate_native_gate(current)!
	validate_target_model(target)!
	if !is_lower_hex_64(operation_id) || target.generation != current.expected_ledger_generation
		|| target.target_id != current.subject.target_id
		|| target.active_subject_hash != current.subject_hash
		|| target.active_native_subject != current.subject || target.active_native_gate != current {
		return error('gate completion observed a stale ledger generation')
	}
	if terminal.epoch != current.active_gate_epoch {
		return error('gate completion can use only the active epoch')
	}
	validate_gate_run_candidate(current, terminal)!
	active_current := current.epochs[current.active_gate_epoch]
	if active_current.state == .completed && active_current.selected_run_id == terminal.run_id
		&& current.selected_run_attempt == terminal.run_attempt
		&& current.selected_conclusion == terminal.conclusion
		&& operation_id in current.completion_operation_ids {
		return GateAckAtomicResult{
			gate:                 current
			target:               target
			resulting_generation: target.generation
		}
	}
	if current.selected_run_id != terminal.run_id
		|| current.selected_run_attempt != terminal.run_attempt
		|| current.selected_check_suite_id != terminal.check_suite_id {
		return error('only the selected run attempt and check suite may complete the active epoch')
	}
	if terminal.conclusion !in ['success', 'failure', 'cancelled', 'timed_out', 'neutral', 'skipped'] {
		return error('gate completion requires a persisted terminal API conclusion')
	}
	mut next := NativeGateModel{
		...current
		epochs:                   current.epochs.clone()
		gate_runs:                current.gate_runs.clone()
		completion_operation_ids: current.completion_operation_ids.clone()
	}
	mut active := next.epochs[next.active_gate_epoch]
	if !timestamp_is_exact(closed_at) || closed_at < active.opened_at {
		return error('gate completion closing timestamp is invalid or precedes opening')
	}
	if active.state != .selected || active.selected_run_id != terminal.run_id {
		return error('active epoch does not own the selected run')
	}
	mut matches := 0
	for index, run in next.gate_runs {
		if run.run_id == terminal.run_id && run.run_attempt == terminal.run_attempt
			&& run.epoch == terminal.epoch {
			next.gate_runs[index] = terminal
			matches++
		}
	}
	if matches != 1 {
		return error('selected run attempt is missing or duplicated in gate history')
	}
	active.state = .completed
	active.conclusion = terminal.conclusion
	active.closed_at = closed_at
	next.epochs[next.active_gate_epoch] = active
	next.selected_conclusion = terminal.conclusion
	next.completion_operation_ids << operation_id
	next_target := advance_target_protocol_generation(target, operation_id,
		'native_gate_complete_${current.subject_hash}')!
	next.expected_ledger_generation = next_target.generation
	mut projected_target := next_target
	projected_target.active_native_gate = next
	validate_native_gate(next)!
	validate_target_model(projected_target)!
	return GateAckAtomicResult{
		gate:                 next
		target:               projected_target
		resulting_generation: projected_target.generation
	}
}

// advance_gate_ledger_generation preserves the subject while following one atomic target bump.
pub fn advance_gate_ledger_generation(current NativeGateModel, observed_generation i64,
	resulting_generation i64) !NativeGateModel {
	validate_native_gate(current)!
	if current.expected_ledger_generation != observed_generation
		|| resulting_generation != observed_generation + 1 {
		return error('native gate generation did not advance atomically with the target')
	}
	mut next := current
	next.expected_ledger_generation = resulting_generation
	validate_native_gate(next)!
	return next
}

// gate_run_is_winner reports whether a tccbin run may proceed beyond its ACK wait.
pub fn gate_run_is_winner(gate NativeGateModel, run_id i64, run_attempt int, epoch int,
	check_suite_id i64) bool {
	return epoch >= 0 && epoch < gate.epochs.len && gate.active_gate_epoch == epoch
		&& gate.selected_run_id == run_id && gate.selected_run_attempt == run_attempt
		&& gate.selected_check_suite_id == check_suite_id
		&& gate.epochs[epoch].state in [.selected, .completed]
}

// validate_native_gate checks authentication, epoch continuity, history, and generations.
pub fn validate_native_gate(gate NativeGateModel) ! {
	validate_native_gate_subject(gate.subject)!
	if gate.subject_hash != native_gate_subject_hash(gate.subject)!
		|| gate.subject_sha != gate.subject.sha
		|| gate.subject_generation != gate.subject.subject_generation {
		return error('native gate subject is not fully bound')
	}
	if gate.subject_generation < 0 || gate.expected_ledger_generation < gate.subject_generation {
		return error('native gate generations are inconsistent')
	}
	validate_gate_authentication(gate.authentication)!
	if gate.epochs.len == 0 || gate.active_gate_epoch != gate.epochs.len - 1 {
		return error('native gate epochs are not append-only and contiguous')
	}
	if gate.infra_retry_count < 0 || gate.infra_retry_count >= native_gate_max_attempts {
		return error('native gate retry count is outside the closed contract')
	}
	if gate.ack_operation_ids.len != gate.gate_runs.len {
		return error('native gate ACK identities do not map one-to-one to persisted run attempts')
	}
	mut ack_ids := []string{}
	for operation_id in gate.ack_operation_ids {
		if !is_lower_hex_64(operation_id) || operation_id in ack_ids {
			return error('native gate ACK operation IDs are invalid or duplicated')
		}
		ack_ids << operation_id
	}
	mut completion_ids := []string{}
	for operation_id in gate.completion_operation_ids {
		if !is_lower_hex_64(operation_id) || operation_id in completion_ids {
			return error('native gate completion operation IDs are invalid or duplicated')
		}
		completion_ids << operation_id
	}
	if gate.completion_operation_ids.len > gate.epochs.filter(it.state == .completed).len {
		return error('native gate records more completion operations than completed epochs')
	}
	if gate.epoch_close_operation_ids.len != gate.epochs.len - 1 {
		return error('native gate epoch-close operations do not map one-to-one to successor epochs')
	}
	mut epoch_close_ids := []string{}
	for operation_id in gate.epoch_close_operation_ids {
		if !is_lower_hex_64(operation_id) || operation_id in epoch_close_ids {
			return error('native gate epoch-close operation IDs are invalid or duplicated')
		}
		epoch_close_ids << operation_id
	}
	mut source_recovery_ids := []string{}
	mut missing_retry_epochs := 0
	for index, epoch in gate.epochs {
		if epoch.epoch != index {
			return error('native gate epoch index mismatch')
		}
		if index < gate.active_gate_epoch && epoch.state in [.open_unselected, .selected] {
			return error('a prior gate epoch remains open')
		}
		if epoch.expected_ref == '' || !timestamp_is_exact(epoch.opened_at) {
			return error('gate epoch lacks its immutable ref or opening time')
		}
		validate_epoch_reason(epoch.reason, epoch.trigger_id, epoch.source_recovery_operation_id)!
		if index == 0 {
			expected_initial_reason := if gate.subject.initial_run_mode == 'no_native_push_expected' {
				'initial-v-remediation'
			} else {
				'original_push'
			}
			if epoch.reason != expected_initial_reason {
				return error('native gate initial epoch differs from the subject run mode')
			}
		} else if epoch.reason in ['original_push', 'initial-v-remediation'] {
			return error('native gate successor epoch reused an initial reason')
		}
		if epoch.reason == 'missing-run-retry' {
			missing_retry_epochs++
			if missing_retry_epochs > 1 {
				return error('native gate contains more than one missing-run retry epoch')
			}
		}
		epoch_recovery := epoch.source_recovery_operation_id
		if epoch_recovery != '' {
			if epoch_recovery in source_recovery_ids {
				return error('native gate reused a source-recovery operation across epochs')
			}
			source_recovery_ids << epoch_recovery
		}
		epoch_counter := if epoch.reason == 'missing-run-retry' { 1 } else { 0 }
		validate_gate_trigger_identity(gate.subject.consumer_id, epoch.epoch, epoch.reason,
			epoch_recovery, epoch_counter, epoch.trigger_id)!
		validate_epoch_ref(epoch.reason, epoch.expected_ref, epoch.trigger_id, gate.subject)!
		if epoch.state in [.closed_timed_out, .closed_not_rerunnable, .completed]
			&& (!timestamp_is_exact(epoch.closed_at) || epoch.closed_at < epoch.opened_at) {
			return error('closed gate epoch lacks its closing time')
		}
		if epoch.state in [.selected, .completed] && (epoch.selected_run_id <= 0
			|| epoch.selected_run_attempt <= 0 || epoch.selected_check_suite_id <= 0
			|| epoch.conclusion == '') {
			return error('selected gate epoch lacks persisted run facts and conclusion')
		}
	}
	for run in gate.gate_runs {
		validate_gate_run_candidate(gate, run)!
	}
	mut run_keys := []string{}
	for run in gate.gate_runs {
		key := '${run.epoch}/${run.run_id}/${run.run_attempt}'
		if key in run_keys {
			return error('native gate run history contains a duplicate epoch/run/attempt')
		}
		run_keys << key
	}
	active := gate.epochs[gate.active_gate_epoch]
	if missing_retry_epochs == 1 && gate.infra_retry_count != 1 {
		return error('native gate missing-run history lacks its global retry accounting')
	}
	if gate.source_recovery_operation_id != active.source_recovery_operation_id {
		return error('native gate source-recovery projection differs from its active epoch')
	}
	if active.state in [.selected, .completed] && (gate.selected_run_id != active.selected_run_id
		|| gate.selected_run_attempt != active.selected_run_attempt
		|| gate.selected_check_suite_id != active.selected_check_suite_id
		|| gate.selected_conclusion != active.conclusion) {
		return error('selected gate epoch and top-level projection disagree')
	}
	if active.state == .open_unselected && (gate.selected_run_id != 0
		|| gate.selected_run_attempt != 0 || gate.selected_check_suite_id != 0
		|| gate.selected_conclusion != '') {
		return error('open gate epoch cannot already have a winner')
	}
}

// native_gate_subject_hash is the RFC 8785/JCS SHA-256 of the complete subject projection.
pub fn native_gate_subject_hash(subject NativeGateSubjectModel) !string {
	validate_native_gate_subject(subject)!
	return json_sha256(native_gate_subject_json(subject)!)
}

// validate_native_gate_subject closes consumer identity, generation, target/ref and remediation.
pub fn validate_native_gate_subject(subject NativeGateSubjectModel) ! {
	if !is_lower_hex_64(subject.consumer_id)
		|| subject.consumer_id != subject.intent_or_operation_id
		|| subject.consumer_kind !in ['publish_candidate', 'rollback_candidate', 'adopt_current', 'initial_adopt_current', 'publish_post', 'rollback_post', 'remediation']
		|| subject.target_id !in managed_target_ids || subject.subject_generation < 0
		|| subject.initial_run_mode !in ['original_push', 'no_native_push_expected']
		|| !is_lower_hex_40(subject.sha) || !is_lower_hex_40(subject.tree)
		|| !is_lower_hex_64(subject.input_fingerprint)
		|| !is_lower_hex_64(subject.artifact_fingerprint) || !is_lower_hex_64(subject.manifest_hash)
		|| subject.digests.len == 0 {
		return error('native gate subject identity, generation, or fingerprints are incomplete')
	}
	mut paths := []string{}
	for digest in subject.digests {
		if !contract_relative_path_is_safe(digest.path) || !is_lower_hex_64(digest.sha256)
			|| digest.path in paths {
			return error('native gate subject digests are invalid or duplicated')
		}
		paths << digest.path
	}
	candidate_kind := subject.consumer_kind in ['publish_candidate', 'rollback_candidate',
		'adopt_current', 'initial_adopt_current']
	if candidate_kind {
		if !candidate_ref_matches_subject(subject.original_ref, subject.target_id,
			subject.consumer_id) {
			return error('candidate subject ref does not encode its exact target and consumer')
		}
	} else if subject.original_ref != canonical_ref(subject.target_id) {
		return error('post-publication or remediation subject requires its exact canonical ref')
	}
	trigger_set := remediation_trigger_is_set(subject.remediation_trigger)
	if subject.consumer_kind != 'remediation' {
		if trigger_set || subject.initial_run_mode != 'original_push' {
			return error('non-remediation subject cannot carry a remediation trigger or no-push mode')
		}
		return
	}
	validate_remediation_trigger(subject.remediation_trigger)!
	if subject.initial_run_mode == 'no_native_push_expected' {
		if subject.remediation_trigger.repository != 'vlang/v'
			|| subject.remediation_trigger.ref != 'master'
			|| subject.remediation_trigger.owner_domain != 'v' {
			return error('V remediation no-push subject is not bound to vlang/v:master')
		}
	} else if subject.remediation_trigger.repository != 'vlang/tccbin'
		|| subject.remediation_trigger.ref != canonical_ref(subject.target_id)
		|| subject.remediation_trigger.owner_domain != 'tccbin' {
		return error('tccbin remediation subject is not bound to its canonical push')
	}
}

fn native_gate_subject_json(subject NativeGateSubjectModel) !JsonValue {
	mut digests := subject.digests.clone()
	digests.sort_with_compare(compare_digest_models)
	mut digest_values := []JsonValue{cap: digests.len}
	for digest in digests {
		digest_values << object_value_from_pairs(['path', 'sha256'], [
			JsonValue{ kind: .string_value, string_value: digest.path },
			JsonValue{ kind: .string_value, string_value: digest.sha256 },
		])!
	}
	trigger := if remediation_trigger_is_set(subject.remediation_trigger) {
		object_value_from_pairs(['repository', 'ref', 'before', 'after', 'tree', 'diff_fingerprint',
			'owner_domain'], [
			JsonValue{ kind: .string_value, string_value: subject.remediation_trigger.repository },
			JsonValue{ kind: .string_value, string_value: subject.remediation_trigger.ref },
			JsonValue{ kind: .string_value, string_value: subject.remediation_trigger.before },
			JsonValue{ kind: .string_value, string_value: subject.remediation_trigger.after },
			JsonValue{ kind: .string_value, string_value: subject.remediation_trigger.tree },
			JsonValue{
				kind:         .string_value
				string_value: subject.remediation_trigger.diff_fingerprint
			},
			JsonValue{ kind: .string_value, string_value: subject.remediation_trigger.owner_domain },
		])!
	} else {
		JsonValue{
			kind: .null_value
		}
	}
	return object_value_from_pairs(['consumer_id', 'consumer_kind', 'intent_or_operation_id',
		'target_id', 'subject_generation', 'initial_run_mode', 'remediation_trigger', 'sha', 'tree',
		'original_ref', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash', 'digests'], [
		JsonValue{ kind: .string_value, string_value: subject.consumer_id },
		JsonValue{ kind: .string_value, string_value: subject.consumer_kind },
		JsonValue{ kind: .string_value, string_value: subject.intent_or_operation_id },
		JsonValue{ kind: .string_value, string_value: subject.target_id },
		JsonValue{ kind: .integer, int_value: subject.subject_generation },
		JsonValue{ kind: .string_value, string_value: subject.initial_run_mode },
		trigger,
		JsonValue{ kind: .string_value, string_value: subject.sha },
		JsonValue{ kind: .string_value, string_value: subject.tree },
		JsonValue{ kind: .string_value, string_value: subject.original_ref },
		JsonValue{ kind: .string_value, string_value: subject.input_fingerprint },
		JsonValue{ kind: .string_value, string_value: subject.artifact_fingerprint },
		JsonValue{ kind: .string_value, string_value: subject.manifest_hash },
		JsonValue{ kind: .array, array_value: digest_values },
	])!
}

fn validate_gate_run_candidate(gate NativeGateModel, run GateRunCandidate) ! {
	auth := gate.authentication
	if run.epoch < 0 || run.epoch >= gate.epochs.len || run.repository != auth.repository
		|| run.workflow_id != auth.workflow_id || run.workflow_path != auth.workflow_path
		|| run.ref != gate.epochs[run.epoch].expected_ref || run.sha != gate.subject_sha
		|| run.event != 'push' || run.run_id <= 0 || run.run_attempt <= 0 || run.check_suite_id <= 0
		|| !timestamp_is_exact(run.created_at) || run.actor != auth.original_actor
		|| run.actor_integration_id != auth.original_actor_integration_id
		|| run.conclusion !in ['pending', 'success', 'failure', 'cancelled', 'timed_out', 'neutral', 'skipped'] {
		return error('gate run does not match the immutable workflow/ref/event/run/actor subject')
	}
	if run.run_attempt == 1 {
		if run.triggering_actor != auth.original_actor
			|| run.triggering_actor_integration_id != auth.original_actor_integration_id {
			return error('initial gate run triggering actor or Integration ID is not exact')
		}
	} else if run.triggering_actor != auth.rerun_triggering_actor
		|| run.triggering_actor_integration_id != auth.rerun_triggering_integration_id {
		return error('rerun triggering actor or Integration ID is not exact')
	}
}

fn validate_gate_authentication(auth GateRunAuthentication) ! {
	if auth.repository != 'vlang/tccbin' || auth.workflow_id <= 0
		|| auth.workflow_path != '.github/workflows/build-and-test.yml' || auth.original_actor == ''
		|| auth.original_actor_integration_id <= 0 || auth.rerun_triggering_actor == ''
		|| auth.rerun_triggering_integration_id <= 0
		|| auth.original_actor_integration_id == auth.rerun_triggering_integration_id
		|| auth.original_actor == auth.rerun_triggering_actor {
		return error('native gate authentication contract is incomplete or aliased')
	}
}

fn validate_epoch_reason(reason string, trigger_id string,
	source_recovery_operation_id string) ! {
	if reason !in ['original_push', 'initial-v-remediation', 'missing-run-retry', 'source-recovery'] {
		return error('unknown gate epoch reason')
	}
	if reason == 'original_push' && trigger_id != '' {
		return error('original push epoch cannot have a trigger ID')
	}
	if reason != 'original_push' && !is_lower_hex_64(trigger_id) {
		return error('non-original gate epoch requires a deterministic trigger ID')
	}
	if reason == 'source-recovery' {
		if !is_lower_hex_64(source_recovery_operation_id) {
			return error('source-recovery epoch must retain its recovery operation ID')
		}
	} else if source_recovery_operation_id != '' {
		return error('non-recovery epoch cannot invent a source recovery operation ID')
	}
}

// deterministic_gate_trigger_id hashes the closed, versioned JCS trigger identity. Epoch and the
// reason-specific logical counter remain distinct materials.
pub fn deterministic_gate_trigger_id(consumer_id string, epoch int, reason string,
	source_recovery_operation_id string, logical_counter int) !string {
	if !is_lower_hex_64(consumer_id) || epoch < 0
		|| reason !in ['initial-v-remediation', 'missing-run-retry', 'source-recovery'] {
		return error('gate trigger identity material is outside its closed set')
	}
	expected_counter := if reason == 'missing-run-retry' { 1 } else { 0 }
	if logical_counter != expected_counter {
		return error('gate trigger logical counter differs from its reason')
	}
	recovery_material := if reason == 'source-recovery' {
		if !is_lower_hex_64(source_recovery_operation_id) {
			return error('source-recovery trigger lacks its immutable recovery operation')
		}
		source_recovery_operation_id
	} else {
		if source_recovery_operation_id != '' {
			return error('non-recovery trigger cannot carry a recovery operation')
		}
		'none'
	}
	identity := object_value_from_pairs(['schema_version', 'audience', 'consumer_id', 'epoch',
		'reason', 'source_recovery_operation_id', 'logical_counter'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/tccbin:native-gate-trigger:v1'
		},
		JsonValue{ kind: .string_value, string_value: consumer_id },
		JsonValue{ kind: .integer, int_value: i64(epoch) },
		JsonValue{ kind: .string_value, string_value: reason },
		JsonValue{ kind: .string_value, string_value: recovery_material },
		JsonValue{ kind: .integer, int_value: i64(logical_counter) },
	])!
	return json_sha256(identity)
}

fn validate_gate_trigger_identity(consumer_id string, epoch int, reason string,
	source_recovery_operation_id string, logical_counter int, trigger_id string) ! {
	if reason == 'original_push' {
		if trigger_id != '' {
			return error('original push epoch cannot carry a trigger identity')
		}
		return
	}
	if trigger_id != deterministic_gate_trigger_id(consumer_id, epoch, reason,
		source_recovery_operation_id, logical_counter)! {
		return error('gate trigger ID differs from its deterministic JCS identity')
	}
}

fn validate_epoch_ref(reason string, expected_ref string, trigger_id string,
	subject NativeGateSubjectModel) ! {
	kind := classify_publisher_ref(expected_ref)
	if reason == 'original_push' {
		if expected_ref != subject.original_ref || kind !in [.candidate, .canonical]
			|| !publisher_ref_is_preflight_valid(expected_ref) {
			return error('original gate epoch requires the exact immutable subject ref')
		}
		return
	}
	expected_trigger_ref := 'tccbin-gate-trigger/${subject.target_id}/${subject.consumer_id}/${trigger_id}'
	if expected_ref != expected_trigger_ref || kind != .gate_trigger
		|| !publisher_ref_is_preflight_valid(expected_ref) {
		return error('retrigger gate epoch requires its exact subject/trigger create-only ref')
	}
}

fn candidate_ref_matches_subject(reference string, target_id string, consumer_id string) bool {
	return reference == 'tccbin-candidate/${target_id}/${consumer_id}'
		&& classify_publisher_ref(reference) == .candidate
		&& publisher_ref_is_preflight_valid(reference)
}

fn remediation_trigger_is_set(trigger RemediationTriggerModel) bool {
	return trigger.repository != '' || trigger.ref != '' || trigger.before != ''
		|| trigger.after != '' || trigger.tree != '' || trigger.diff_fingerprint != ''
		|| trigger.owner_domain != ''
}

fn validate_remediation_trigger(trigger RemediationTriggerModel) ! {
	if trigger.repository !in ['vlang/v', 'vlang/tccbin'] || trigger.ref == ''
		|| !is_lower_hex_40(trigger.before) || !is_lower_hex_40(trigger.after)
		|| trigger.before == trigger.after || !is_lower_hex_40(trigger.tree)
		|| !is_lower_hex_64(trigger.diff_fingerprint) || trigger.owner_domain !in ['v', 'tccbin'] {
		return error('remediation trigger is incomplete or not immutable')
	}
}

fn compare_digest_models(left &DigestModel, right &DigestModel) int {
	if left.path != right.path {
		return left.path.compare(right.path)
	}
	return left.sha256.compare(right.sha256)
}

fn active_epoch_opened_at(gate NativeGateModel) string {
	return gate.epochs[gate.active_gate_epoch].opened_at
}

// timestamp_is_exact accepts only the fixed UTC shape used by the persisted JSON contract. That
// shape makes lexical order equivalent to chronological order for every accepted timestamp.
fn timestamp_is_exact(value string) bool {
	if value.len != 20 || value[4] != `-` || value[7] != `-` || value[10] != `T` || value[13] != `:`
		|| value[16] != `:` || value[19] != `Z` {
		return false
	}
	for index in [0, 1, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18] {
		if value[index] < `0` || value[index] > `9` {
			return false
		}
	}
	month := value[5..7].int()
	day := value[8..10].int()
	hour := value[11..13].int()
	minute := value[14..16].int()
	second := value[17..19].int()
	return month >= 1 && month <= 12 && day >= 1 && day <= 31 && hour <= 23 && minute <= 59
		&& second <= 59
}
