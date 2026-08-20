module tests

import tccbin_automation.bin

const gate_consumer = '1111111111111111111111111111111111111111111111111111111111111111'
const gate_recovery = '2222222222222222222222222222222222222222222222222222222222222222'
const gate_input = '3333333333333333333333333333333333333333333333333333333333333333'
const gate_artifact = '4444444444444444444444444444444444444444444444444444444444444444'
const gate_manifest = '5555555555555555555555555555555555555555555555555555555555555555'
const gate_digest = '6666666666666666666666666666666666666666666666666666666666666666'
const gate_sha = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
const gate_tree = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
const gate_before = 'cccccccccccccccccccccccccccccccccccccccc'
const receiver_master = 'dddddddddddddddddddddddddddddddddddddddd'

fn gate_operation(character u8) string {
	return character.ascii_str().repeat(64)
}

fn recovery_check_sources() []bin.CheckSourceModel {
	return [
		bin.CheckSourceModel{
			name:           'tccbin-candidate-gate'
			repository:     'vlang/tccbin'
			integration_id: 5001
			workflow_id:    6001
			workflow_path:  '.github/workflows/build-and-test.yml'
			event:          'push'
		},
		bin.CheckSourceModel{
			name:           'v-candidate-smoke'
			repository:     'vlang/v'
			integration_id: 5002
			workflow_id:    6002
			workflow_path:  '.github/workflows/tccbin_revalidate.yml'
			event:          'workflow_dispatch'
		},
	]
}

fn gate_resolved_inputs() bin.ResolvedInputsModel {
	profile := bin.parse_strict_json(t2a_profile_source('windows-amd64')) or { panic(err) }
	profile_id := (profile.object_value('profile_id') or { panic('profile id missing') }).string_value
	profile_sha256 := bin.json_sha256(profile)
	producer := bin.parse_strict_json(t2a_producer_observation_source('windows-amd64',
		profile_sha256)) or { panic(err) }
	producer_digest := (producer.object_value('observation_digest') or {
		panic('producer observation digest missing')
	}).string_value
	return bin.ResolvedInputsModel{
		sources:             [
			bin.ResolvedSourceModel{
				id:         'tinycc'
				repository: 'https://repo.or.cz/tinycc.git'
				ref:        'mob'
				sha:        'c'.repeat(40)
				tree:       'd'.repeat(40)
			},
			bin.ResolvedSourceModel{
				id:         'v-libgc'
				repository: 'https://github.com/vlang/v.git'
				ref:        'master'
				sha:        'b'.repeat(40)
				tree:       'e'.repeat(40)
			},
		]
		source_checks:       [
			bin.SourceCheckModel{
				source_id:       'tinycc'
				resolved_sha:    'c'.repeat(40)
				status:          'resolved'
				evidence_digest: gate_operation(`1`)
			},
			bin.SourceCheckModel{
				source_id:       'v-libgc'
				resolved_sha:    'b'.repeat(40)
				status:          'resolved'
				evidence_digest: gate_operation(`2`)
			},
		]
		recipe_path:         'build.ps1'
		recipe_hash:         '1'.repeat(64)
		contract_repository: 'GGRei/v'
		contract_sha:        'a'.repeat(40)
		v_source_sha:        'b'.repeat(40)
		producer_toolchain:  bin.ProducerToolchainModel{
			profile_id:         profile_id
			profile_sha256:     profile_sha256
			observation_sha256: bin.json_sha256(producer)
			observation_digest: producer_digest
		}
	}
}

fn remediation_gate_subject() bin.NativeGateSubjectModel {
	return bin.NativeGateSubjectModel{
		consumer_id:            gate_consumer
		consumer_kind:          'remediation'
		intent_or_operation_id: gate_consumer
		target_id:              'windows-amd64'
		subject_generation:     3
		initial_run_mode:       'original_push'
		remediation_trigger:    bin.RemediationTriggerModel{
			repository:       'vlang/tccbin'
			ref:              'thirdparty-windows-amd64'
			before:           gate_before
			after:            gate_sha
			tree:             gate_tree
			diff_fingerprint: gate_recovery
			owner_domain:     'tccbin'
		}
		sha:                    gate_sha
		tree:                   gate_tree
		original_ref:           'thirdparty-windows-amd64'
		input_fingerprint:      gate_input
		artifact_fingerprint:   gate_artifact
		manifest_hash:          gate_manifest
		digests:                [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: gate_digest
			},
		]
	}
}

fn recovery_gate_authentication() bin.GateRunAuthentication {
	return bin.GateRunAuthentication{
		repository:                      'vlang/tccbin'
		workflow_id:                     6001
		workflow_path:                   '.github/workflows/build-and-test.yml'
		original_actor:                  'tccbin-publisher[bot]'
		original_actor_integration_id:   5001
		rerun_triggering_actor:          'tccbin-gate-dispatcher[bot]'
		rerun_triggering_integration_id: 5003
	}
}

fn open_recovery_gate() bin.NativeGateModel {
	subject := remediation_gate_subject()
	return bin.initial_native_gate(subject, subject.subject_generation, subject.original_ref,
		'original_push', '', '2026-08-02T00:00:00Z', recovery_gate_authentication(), '') or {
		panic(err)
	}
}

fn open_gate_target() bin.TargetModel {
	subject := remediation_gate_subject()
	gate := open_recovery_gate()
	target := bin.TargetModel{
		target_id:                 'windows-amd64'
		generation:                3
		target_state:              .validating
		publication_state:         .idle
		bootstrap_required:        false
		canonical_observed_sha:    gate_sha
		input_fingerprint:         gate_input
		artifact_fingerprint:      gate_artifact
		manifest_hash:             gate_manifest
		provenance_status:         'complete'
		affected_targets:          ['windows-amd64']
		resolved_inputs:           gate_resolved_inputs()
		last_known_good:           bin.ArtifactTupleModel{
			sha:                  gate_sha
			tree:                 gate_tree
			input_fingerprint:    gate_input
			artifact_fingerprint: gate_artifact
			manifest_hash:        gate_manifest
			digests:              subject.digests
		}
		incident_ids:              [gate_operation(`9`)]
		active_native_subject:     subject
		active_subject_hash:       bin.native_gate_subject_hash(subject) or { panic(err) }
		active_native_gate:        gate
		active_remediation_id:     gate_consumer
		remediation_check_sources: recovery_check_sources()
	}
	bin.validate_target_model(target) or { panic(err) }
	return target
}

fn original_gate_run(gate bin.NativeGateModel, conclusion string) bin.GateRunCandidate {
	return bin.GateRunCandidate{
		epoch:                           gate.active_gate_epoch
		run_id:                          42
		run_attempt:                     1
		repository:                      'vlang/tccbin'
		ref:                             gate.epochs[gate.active_gate_epoch].expected_ref
		sha:                             gate_sha
		event:                           'push'
		actor:                           'tccbin-publisher[bot]'
		actor_integration_id:            5001
		triggering_actor:                'tccbin-publisher[bot]'
		triggering_actor_integration_id: 5001
		check_suite_id:                  55
		workflow_id:                     6001
		workflow_path:                   '.github/workflows/build-and-test.yml'
		created_at:                      '2026-08-02T00:00:05Z'
		conclusion:                      conclusion
	}
}

fn completed_gate_target() bin.TargetModel {
	start := open_gate_target()
	pending := original_gate_run(start.active_native_gate, 'pending')
	ack := bin.acknowledge_gate_run(start.active_native_gate, pending, start, gate_operation(`a`)) or {
		panic(err)
	}
	assert ack.target.resolved_inputs == start.resolved_inputs
	terminal := bin.GateRunCandidate{
		...pending
		conclusion: 'success'
	}
	completed := bin.complete_gate_epoch(ack.gate, terminal, ack.target, gate_operation(`b`),
		'2026-08-02T00:05:00Z') or { panic(err) }
	assert completed.target.resolved_inputs == start.resolved_inputs
	return completed.target
}

fn recovery_subject() bin.RecoverySubjectModel {
	subject := remediation_gate_subject()
	return bin.RecoverySubjectModel{
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
		digests:                subject.digests
	}
}

fn receiver_run(handoff bin.RecoveryHandoffModel, conclusion string,
	output_digest string) bin.ReceiverRunCandidate {
	return bin.ReceiverRunCandidate{
		run_id:        80
		run_attempt:   1
		repository:    handoff.receiver_repository
		workflow_id:   handoff.workflow_id
		workflow_path: handoff.workflow_path
		workflow_ref:  handoff.workflow_ref
		event:         handoff.event
		head_sha:      receiver_master
		run_name:      handoff.receiver_run_name
		created_at:    '2026-08-02T00:10:00Z'
		conclusion:    conclusion
		output_digest: output_digest
		deadline:      '2026-08-02T06:00:00Z'
	}
}

fn dispatched_recovery() (bin.RecoveryHandoffModel, bin.TargetModel) {
	start := completed_gate_target()
	created := bin.create_recovery_handoff_atomic('vlang/v:tccbin-automation-state', gate_recovery,
		gate_consumer, recovery_subject(), 3, 'remediation', 'native_gate', 7001, start,
		gate_operation(`c`)) or { panic(err) }
	assert created.target.resolved_inputs == start.resolved_inputs
	dispatched := bin.record_handoff_dispatch_atomic(created.handoff, created.target,
		gate_operation(`d`)) or { panic(err) }
	assert dispatched.target.resolved_inputs == start.resolved_inputs
	return dispatched.handoff, dispatched.target
}

fn test_gate_ack_and_completion_are_atomic_authenticated_and_idempotent() {
	target := open_gate_target()
	run := original_gate_run(target.active_native_gate, 'pending')
	ack := bin.acknowledge_gate_run(target.active_native_gate, run, target, gate_operation(`a`)) or {
		panic(err)
	}
	assert ack.target.generation == 4
	assert ack.target.active_native_gate == ack.gate
	assert ack.target.resolved_inputs == target.resolved_inputs
	replayed := bin.acknowledge_gate_run(ack.gate, run, ack.target, gate_operation(`a`)) or {
		panic(err)
	}
	assert replayed.target == ack.target
	assert replayed.target.resolved_inputs == target.resolved_inputs
	terminal := bin.GateRunCandidate{
		...run
		conclusion: 'success'
	}
	completed := bin.complete_gate_epoch(ack.gate, terminal, ack.target, gate_operation(`b`),
		'2026-08-02T00:05:00Z') or { panic(err) }
	assert completed.target.generation == 5
	assert completed.gate.selected_conclusion == 'success'
	assert completed.target.resolved_inputs == target.resolved_inputs
	mut rejected := false
	bin.acknowledge_gate_run(target.active_native_gate, bin.GateRunCandidate{
		...run
		repository: 'GGRei/tccbin'
	}, target, gate_operation(`e`)) or { rejected = true }
	assert rejected
}

fn test_gate_mutating_transitions_preserve_inputs_and_accept_one_exact_rerun() {
	target := open_gate_target()
	gate := target.active_native_gate
	gate_epochs_before := gate.epochs.clone()
	gate_runs_before := gate.gate_runs.clone()
	gate_ack_ids_before := gate.ack_operation_ids.clone()
	first := original_gate_run(gate, 'pending')
	first_ack := bin.acknowledge_gate_run(gate, first, target, gate_operation(`a`)) or {
		panic(err)
	}
	assert first_ack.target.resolved_inputs == target.resolved_inputs
	assert gate.epochs == gate_epochs_before
	assert gate.gate_runs == gate_runs_before
	assert gate.ack_operation_ids == gate_ack_ids_before
	assert target.active_native_gate.epochs == gate_epochs_before
	assert target.active_native_gate.gate_runs == gate_runs_before
	assert target.active_native_gate.ack_operation_ids == gate_ack_ids_before

	first_ack_epochs_before := first_ack.gate.epochs.clone()
	first_ack_runs_before := first_ack.gate.gate_runs.clone()
	first_ack_ids_before := first_ack.gate.ack_operation_ids.clone()
	rerun := bin.GateRunCandidate{
		...first
		run_attempt:                     2
		triggering_actor:                'tccbin-gate-dispatcher[bot]'
		triggering_actor_integration_id: 5003
		check_suite_id:                  56
		created_at:                      '2026-08-02T00:00:10Z'
	}
	rerun_ack := bin.acknowledge_gate_run(first_ack.gate, rerun, first_ack.target,
		gate_operation(`b`)) or { panic(err) }
	assert first_ack.gate.epochs == first_ack_epochs_before
	assert first_ack.gate.gate_runs == first_ack_runs_before
	assert first_ack.gate.ack_operation_ids == first_ack_ids_before
	assert first_ack.target.active_native_gate.epochs == first_ack_epochs_before
	assert first_ack.target.active_native_gate.gate_runs == first_ack_runs_before
	assert first_ack.target.active_native_gate.ack_operation_ids == first_ack_ids_before
	assert rerun_ack.target.generation == 5
	assert rerun_ack.gate.selected_run_attempt == 2
	assert rerun_ack.gate.selected_check_suite_id == 56
	assert rerun_ack.gate.infra_retry_count == 1
	assert rerun_ack.gate.gate_runs.len == 2
	assert rerun_ack.gate.ack_operation_ids.len == 2
	assert rerun_ack.target.resolved_inputs == target.resolved_inputs

	completion_epochs_before := rerun_ack.gate.epochs.clone()
	completion_runs_before := rerun_ack.gate.gate_runs.clone()
	completion_ids_before := rerun_ack.gate.completion_operation_ids.clone()
	terminal := bin.GateRunCandidate{
		...rerun
		conclusion: 'success'
	}
	completed := bin.complete_gate_epoch(rerun_ack.gate, terminal, rerun_ack.target,
		gate_operation(`c`), '2026-08-02T00:05:00Z') or { panic(err) }
	assert rerun_ack.gate.epochs == completion_epochs_before
	assert rerun_ack.gate.gate_runs == completion_runs_before
	assert rerun_ack.gate.completion_operation_ids == completion_ids_before
	assert rerun_ack.target.active_native_gate.epochs == completion_epochs_before
	assert rerun_ack.target.active_native_gate.gate_runs == completion_runs_before
	assert rerun_ack.target.active_native_gate.completion_operation_ids == completion_ids_before
	assert completed.target.generation == 6
	assert completed.gate.selected_conclusion == 'success'
	assert completed.gate.gate_runs[1] == terminal
	assert completed.gate.completion_operation_ids == [gate_operation(`c`)]
	assert completed.target.resolved_inputs == target.resolved_inputs
}

fn test_gate_epoch_retrigger_requires_expired_authenticated_proof() {
	target := open_gate_target()
	gate := target.active_native_gate
	trigger_id := bin.deterministic_gate_trigger_id(gate.subject.consumer_id, 1, 'source-recovery',
		gate_recovery, 0) or { panic(err) }
	trigger_ref := 'tccbin-gate-trigger/windows-amd64/${gate.subject.consumer_id}/${trigger_id}'
	mut rejected := false
	bin.close_and_open_gate_epoch(gate, .closed_timed_out, 'source-recovery', trigger_ref,
		trigger_id, '2026-08-02T00:02:00Z', '2026-08-02T00:02:00Z', gate_recovery, target, bin.GateEpochCloseProof{
		operation_id:        gate_operation(`c`)
		expected_generation: target.generation
		deadline:            '2026-08-02T00:03:00Z'
		observed_at:         '2026-08-02T00:02:00Z'
		evidence_digest:     gate_operation(`8`)
	}) or { rejected = true }
	assert rejected
	gate_epochs_before := gate.epochs.clone()
	gate_close_ids_before := gate.epoch_close_operation_ids.clone()
	advanced := bin.close_and_open_gate_epoch(gate, .closed_timed_out, 'source-recovery',
		trigger_ref, trigger_id, '2026-08-02T00:02:00Z', '2026-08-02T00:02:00Z', gate_recovery,
		target, bin.GateEpochCloseProof{
		operation_id:        gate_operation(`c`)
		expected_generation: target.generation
		deadline:            '2026-08-02T00:01:00Z'
		observed_at:         '2026-08-02T00:02:00Z'
		evidence_digest:     gate_operation(`8`)
	}) or { panic(err) }
	assert gate.epochs == gate_epochs_before
	assert gate.epoch_close_operation_ids == gate_close_ids_before
	assert target.active_native_gate.epochs == gate_epochs_before
	assert target.active_native_gate.epoch_close_operation_ids == gate_close_ids_before
	assert advanced.gate.active_gate_epoch == 1
	assert advanced.target.active_native_gate == advanced.gate
	assert advanced.target.resolved_inputs == target.resolved_inputs
	assert advanced.gate.epochs[1].source_recovery_operation_id == gate_recovery
	assert advanced.gate.source_recovery_operation_id == gate_recovery

	missing_trigger := bin.deterministic_gate_trigger_id(gate.subject.consumer_id, 2,
		'missing-run-retry', '', 1) or { panic(err) }
	missing_ref := 'tccbin-gate-trigger/windows-amd64/${gate.subject.consumer_id}/${missing_trigger}'
	after_missing := bin.close_and_open_gate_epoch(advanced.gate, .closed_timed_out,
		'missing-run-retry', missing_ref, missing_trigger, '2026-08-02T00:04:00Z',
		'2026-08-02T00:04:00Z', '', advanced.target, bin.GateEpochCloseProof{
		operation_id:        gate_operation(`d`)
		expected_generation: advanced.target.generation
		deadline:            '2026-08-02T00:03:00Z'
		observed_at:         '2026-08-02T00:04:00Z'
		evidence_digest:     gate_operation(`a`)
	}) or { panic(err) }
	assert after_missing.gate.infra_retry_count == 1
	assert after_missing.target.resolved_inputs == target.resolved_inputs
	assert after_missing.gate.source_recovery_operation_id == ''
	assert after_missing.gate.epochs[1].source_recovery_operation_id == gate_recovery

	recovery_b := gate_operation(`7`)
	recovery_b_trigger := bin.deterministic_gate_trigger_id(gate.subject.consumer_id, 3,
		'source-recovery', recovery_b, 0) or { panic(err) }
	recovery_b_ref := 'tccbin-gate-trigger/windows-amd64/${gate.subject.consumer_id}/${recovery_b_trigger}'
	after_recovery_b := bin.close_and_open_gate_epoch(after_missing.gate, .closed_timed_out,
		'source-recovery', recovery_b_ref, recovery_b_trigger, '2026-08-02T00:06:00Z',
		'2026-08-02T00:06:00Z', recovery_b, after_missing.target, bin.GateEpochCloseProof{
		operation_id:        gate_operation(`e`)
		expected_generation: after_missing.target.generation
		deadline:            '2026-08-02T00:05:00Z'
		observed_at:         '2026-08-02T00:06:00Z'
		evidence_digest:     gate_operation(`b`)
	}) or { panic(err) }
	assert after_recovery_b.gate.epochs[1].source_recovery_operation_id == gate_recovery
	assert after_recovery_b.gate.epochs[3].source_recovery_operation_id == recovery_b
	assert after_recovery_b.gate.epochs[1].expected_ref != after_recovery_b.gate.epochs[3].expected_ref
	assert after_recovery_b.gate.source_recovery_operation_id == recovery_b
	assert after_recovery_b.gate.infra_retry_count == 1
	assert after_recovery_b.target.resolved_inputs == target.resolved_inputs

	mut invalid_gates := []bin.NativeGateModel{}
	mut missing_operation := after_recovery_b.gate
	missing_operation.epochs = after_recovery_b.gate.epochs.clone()
	missing_operation.epochs[3].source_recovery_operation_id = ''
	invalid_gates << missing_operation
	mut extra_operation := after_recovery_b.gate
	extra_operation.epochs = after_recovery_b.gate.epochs.clone()
	extra_operation.epochs[2].source_recovery_operation_id = recovery_b
	invalid_gates << extra_operation
	mut reused_operation := after_recovery_b.gate
	reused_operation.epochs = after_recovery_b.gate.epochs.clone()
	reused_operation.epochs[3].source_recovery_operation_id = gate_recovery
	invalid_gates << reused_operation
	mut projection_mismatch := after_recovery_b.gate
	projection_mismatch.source_recovery_operation_id = gate_recovery
	invalid_gates << projection_mismatch
	mut trigger_mismatch := after_recovery_b.gate
	trigger_mismatch.epochs = after_recovery_b.gate.epochs.clone()
	trigger_mismatch.epochs[3].trigger_id = gate_operation(`8`)
	invalid_gates << trigger_mismatch
	for invalid_gate in invalid_gates {
		mut invalid_rejected := false
		bin.validate_native_gate(invalid_gate) or { invalid_rejected = true }
		assert invalid_rejected
	}
}

fn test_handoff_create_dispatch_ack_retry_and_generation_are_one_cas_each() {
	handoff, target := dispatched_recovery()
	pending := receiver_run(handoff, 'pending', '')
	ack := bin.acknowledge_handoff_dispatch(handoff, pending, target, gate_operation(`e`)) or {
		panic(err)
	}
	assert ack.target.generation == handoff.expected_ledger_generation + 1
	assert ack.handoff.expected_ledger_generation == ack.target.generation
	assert ack.target.resolved_inputs == target.resolved_inputs
	replayed := bin.acknowledge_handoff_dispatch(ack.handoff, pending, ack.target,
		gate_operation(`e`)) or { panic(err) }
	assert replayed.target == ack.target
	assert replayed.target.resolved_inputs == target.resolved_inputs
	terminal := bin.ReceiverRunCandidate{
		...pending
		conclusion: 'timed_out'
	}
	retried := bin.retry_handoff_after_infra_failure(ack.handoff, terminal, ack.target, bin.ReceiverRetryProof{
		operation_id:        gate_operation(`f`)
		expected_generation: ack.target.generation
		observed_at:         '2026-08-02T06:01:00Z'
		deadline:            pending.deadline
		evidence_digest:     gate_operation(`8`)
	}) or { panic(err) }
	assert retried.handoff.state == .pending
	assert retried.target.resolved_inputs == target.resolved_inputs
	second := bin.record_handoff_dispatch_atomic(retried.handoff, retried.target,
		gate_operation(`0`)) or { panic(err) }
	assert second.handoff.dispatch_generation == 2
	assert second.target.generation == second.handoff.expected_ledger_generation
	assert second.target.resolved_inputs == target.resolved_inputs
}

fn test_native_green_handoff_creates_one_atomic_revalidation_successor() {
	handoff, target := dispatched_recovery()
	pending := receiver_run(handoff, 'pending', '')
	ack := bin.acknowledge_handoff_dispatch(handoff, pending, target, gate_operation(`e`)) or {
		panic(err)
	}
	assert ack.target.resolved_inputs == target.resolved_inputs
	green := bin.ReceiverRunCandidate{
		...pending
		conclusion:    'success'
		output_digest: gate_artifact
	}
	result := bin.native_green_successor_atomic(ack.handoff, green, ack.target.active_native_gate,
		'v_smoke', 7002, ack.target, gate_operation(`f`)) or { panic(err) }
	assert result.chain.predecessor.terminal_outcome == 'native_gate_green_successor'
	assert result.chain.successor.predecessor_handoff_id == ack.handoff.handoff_id
	assert result.target.active_recovery_handoff_id == result.chain.successor.handoff_id
	assert result.native_gate.expected_ledger_generation == result.target.generation
	assert result.target.resolved_inputs == target.resolved_inputs
}

fn test_terminal_handoff_preserves_consumer_until_business_verdict() {
	handoff, target := dispatched_recovery()
	pending := receiver_run(handoff, 'pending', '')
	ack := bin.acknowledge_handoff_dispatch(handoff, pending, target, gate_operation(`e`)) or {
		panic(err)
	}
	assert ack.target.resolved_inputs == target.resolved_inputs
	green := bin.ReceiverRunCandidate{
		...pending
		conclusion:    'success'
		output_digest: gate_artifact
	}
	completed := bin.complete_handoff(ack.handoff, green, 'green', ack.target, gate_operation(`f`)) or {
		panic(err)
	}
	assert completed.target.active_recovery_handoff_id == ''
	assert completed.target.active_subject_hash == target.active_subject_hash
	assert completed.target.active_remediation_id == gate_consumer
	assert completed.target.resolved_inputs == target.resolved_inputs
	assert bin.handoff_returns_to_monthly(completed.handoff)
}

fn test_source_outage_policy_is_silent_and_unbounded_by_days() {
	assert bin.source_resolve_backoff_seconds == [0, 15, 45]
	assert bin.source_connect_timeout_seconds == 10
	assert bin.source_total_timeout_seconds == 60
	assert bin.source_failure_is_transient(.dns)
	assert bin.source_failure_is_transient(.http_429)
	assert !bin.source_failure_is_transient(.missing_ref)
	assert bin.next_source_mode(.monthly, false, false) == .upstream_recovery_daily
	assert !bin.source_retry_due(.upstream_recovery_daily, 100, 100 + 86_399)
	assert bin.source_retry_due(.upstream_recovery_daily, 100, 100 + 86_400)
	assert bin.next_source_mode(.upstream_recovery_daily, true, true) == .monthly
}

fn test_source_machine_distinguishes_silent_outage_from_deterministic_defect() {
	base := bin.initial_source_state('tinycc-mob', 'https://repo.or.cz/tinycc.git', 'mob',
		'1111111111111111111111111111111111111111111111111111111111111111', '2026-08-01T03:47:00Z') or {
		panic(err)
	}
	outage := bin.resolve_source(base, base.generation,
		'2222222222222222222222222222222222222222222222222222222222222222', 100,
		'2026-08-02T03:47:00Z', [
		'3333333333333333333333333333333333333333333333333333333333333333',
	], [
		transient_source_attempt(1, 0),
		transient_source_attempt(2, 15),
		transient_source_attempt(3, 45),
	]) or { panic(err) }
	assert outage.external_outage
	assert !outage.should_report
	assert !outage.may_build
	assert outage.state.status == 'source_unreachable'
	assert outage.state.mode == .upstream_recovery_daily
	assert !(bin.source_daily_resolution_is_due(outage.state, 100, 100 + 86_399) or { panic(err) })
	assert bin.source_daily_resolution_is_due(outage.state, 100, 100 + 86_400) or { panic(err) }
	deterministic := bin.resolve_source(base, base.generation,
		'4444444444444444444444444444444444444444444444444444444444444444', 101,
		'2026-08-02T04:00:00Z', [], [
		bin.SourceResolutionAttempt{
			ordinal:                 1
			backoff_seconds:         0
			connect_timeout_seconds: bin.source_connect_timeout_seconds
			total_timeout_seconds:   bin.source_total_timeout_seconds
			failure_kind:            .missing_ref
		},
	]) or { panic(err) }
	assert deterministic.should_report
	assert !deterministic.external_outage
	assert deterministic.state.status == 'invalid_configuration'
	assert deterministic.state.mode == .monthly
}

fn test_source_machine_returns_monthly_only_after_functional_result() {
	base := bin.initial_source_state('tinycc-mob', 'https://repo.or.cz/tinycc.git', 'mob',
		'1111111111111111111111111111111111111111111111111111111111111111', '2026-08-01T03:47:00Z') or {
		panic(err)
	}
	outage := bin.resolve_source(base, 0,
		'2222222222222222222222222222222222222222222222222222222222222222', 100,
		'2026-08-02T03:47:00Z', [], [
		transient_source_attempt(1, 0),
		transient_source_attempt(2, 15),
		transient_source_attempt(3, 45),
	]) or { panic(err) }
	resolved := bin.resolve_source(outage.state, outage.state.generation,
		'3333333333333333333333333333333333333333333333333333333333333333', 100,
		'2026-08-03T03:47:00Z', [], [
		bin.SourceResolutionAttempt{
			ordinal:                 1
			backoff_seconds:         0
			connect_timeout_seconds: bin.source_connect_timeout_seconds
			total_timeout_seconds:   bin.source_total_timeout_seconds
			resolved_sha:            'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
			resolved_tree:           'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		},
	]) or { panic(err) }
	assert resolved.may_build
	assert resolved.state.mode == .upstream_recovery_daily
	monthly := bin.complete_source_resolution(resolved.state, resolved.state.generation,
		'4444444444444444444444444444444444444444444444444444444444444444', 'green',
		'2026-08-03T04:30:00Z') or { panic(err) }
	assert monthly.mode == .monthly
	assert monthly.originating_run_id == 0
	assert monthly.waiting_consumers.len == 0
}
