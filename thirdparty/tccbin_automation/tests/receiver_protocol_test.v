module tests

import os
import tccbin_automation.bin

const receiver_handoff_id = '7777777777777777777777777777777777777777777777777777777777777777'
const receiver_companion_handoff_id = '6513c90942a3e5ac149d70b2c32145bf21e72f3bad6cf262d3e6add9ca19fbba'
const receiver_recovery_operation_id = '8888888888888888888888888888888888888888888888888888888888888888'

fn receiver_ledger_source() string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'receiver-ledger.dark.json')) or { panic(err) }
}

fn receiver_completion_source() string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'receiver-workflow-run.dark.json')) or { panic(err) }
}

fn live_recovery_native_check(subject_hash string) string {
	return live_recovery_native_check_for(subject_hash, live_post_operation_id,
		'thirdparty-linux-amd64')
}

fn live_recovery_successor_id() string {
	return live_recovery_successor_id_for('publish_post')
}

fn live_recovery_smoke_projection(fixture_name string, expected_generation i64) string {
	return live_recovery_smoke_projection_for(fixture_name, expected_generation, 'publish_post')
}

fn live_recovery_rehashed_smoke(fixture_name string, expected_generation i64) string {
	return live_recovery_rehashed_smoke_for(fixture_name, expected_generation, 'publish_post')
}

fn live_native_adoption_source(with_trigger_epoch bool) string {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	mut source := live_pre_subject_adoption_source()
	subject := (os.read_file(os.join_path(fixture_root, 'native-gate-subject.schema-fixture.json')) or {
		panic(err)
	}).trim_space()
	mut execution := (os.read_file(os.join_path(fixture_root,
		'native-gate-execution.schema-fixture.json')) or { panic(err) }).trim_space()
	if with_trigger_epoch {
		trigger_id := bin.deterministic_gate_trigger_id(receiver_consumer_id, 1,
			'missing-run-retry', '', 1) or { panic(err) }
		trigger_ref := 'tccbin-gate-trigger/linux-amd64/${receiver_consumer_id}/${trigger_id}'
		execution = execution.replace_once('"active_gate_epoch": 0', '"active_gate_epoch": 1')
		execution = execution.replace_once('      "state": "open_unselected",',
			'      "state": "closed_timed_out",')
		execution = execution.replace_once('      "closed_at": null,',
			'      "closed_at": "2026-08-02T00:02:00Z",')
		execution = execution.replace_once('      "source_recovery_operation_id": null\n    }\n  ],',
			'      "source_recovery_operation_id": null\n    },\n    {\n      "epoch": 1,\n      "reason": "missing-run-retry",\n      "expected_ref": "${trigger_ref}",\n      "trigger_id": "${trigger_id}",\n      "state": "open_unselected",\n      "selected_run_id": null,\n      "selected_run_attempt": null,\n      "selected_check_suite_id": null,\n      "conclusion": null,\n      "opened_at": "2026-08-02T00:02:00Z",\n      "closed_at": null,\n      "source_recovery_operation_id": null\n    }\n  ],')
		execution = execution.replace_once('"epoch_close_operation_ids": []',
			'"epoch_close_operation_ids": ["9999999999999999999999999999999999999999999999999999999999999999"]')
		execution = execution.replace_once('"infra_retry_count": 0', '"infra_retry_count": 1')
	}
	source = source.replace_once('"generation": 0', '"generation": 1')
	source = source.replace_once('"native_gate_subject": null', '"native_gate_subject": ${subject}')
	source = source.replace_once('"active_subject_hash": null',
		'"active_subject_hash": "d92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd"')
	source = source.replace_once('"native_gate_execution": null',
		'"native_gate_execution": ${execution}')
	return with_pending_v_smoke(source)
}

struct LiveAtomicStateOptions {
	descendant                     bool
	split_source_target            bool
	evidence_count                 int = 4
	cas_attempt                    int = 1
	modify_evidence_at_h           bool
	with_infrastructure_retry      bool
	delete_evidence_at_h           bool
	reintroduce_evidence_at_h      bool
	coordinated_delete_at_h        bool
	orphan_business_evidence_at_h  bool
	non_first_parent_evidence_at_h bool
	modify_terminal_handoff_at_h   bool
	target_evidence_mutation       string
	evicted_source_replay          bool
	wrong_expected_parent          bool
}

struct LiveAtomicStateRepository {
	root   string
	parent string
	target string
	head   string
	proof  string
}

struct LiveMultiAtomicStateRepository {
	root        string
	parent      string
	target      string
	head        string
	proof       string
	handoff_ids []string
}

fn write_live_state_blob(root string, relative_path string, source string) {
	path := os.join_path(root, relative_path)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn live_atomic_cas_attempt(proof bin.JsonValue) int {
	refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	resolution_operation_id := (refetch.object_value('resolution_operation_id') or {
		panic('source resolution operation missing')
	}).string_value
	history := proof.object_value('source_state_cas_history') or {
		panic('source CAS history missing')
	}
	for transition in history.array_value {
		operation_id := transition.object_value('operation_id') or { bin.JsonValue{} }
		if operation_id.kind == .string_value
			&& operation_id.string_value == resolution_operation_id {
			universal := transition.object_value('universal_evidence') or {
				panic('source universal evidence missing')
			}
			cas_attempt := (universal.object_value('cas_attempt') or {
				panic('source CAS attempt missing')
			}).int_value
			if cas_attempt < 1 || cas_attempt > 3 {
				panic('source CAS attempt must be in 1..3')
			}
			return int(cas_attempt)
		}
	}
	panic('source resolution operation is absent from its CAS history')
}

fn live_atomic_target_evidence(target bin.JsonValue, operation bin.JsonValue, proof bin.JsonValue,
	handoff bin.JsonValue, role string, ordinal int) (string, string) {
	target_id := (target.object_value('target_id') or { panic('target ID missing') }).string_value
	operation_id := (operation.object_value('operation_id') or { panic('operation ID missing') }).string_value
	transition := (operation.object_value('transition') or { panic('operation transition missing') }).string_value
	resulting_generation := (operation.object_value('resulting_generation') or {
		panic('operation generation missing')
	}).int_value
	subject_fingerprint := (target.object_value('input_fingerprint') or {
		panic('target input missing')
	}).string_value
	mut selected_attempt := bin.JsonValue{
		kind: .null_value
	}
	smoke := proof.object_value('v_smoke_execution') or { panic('terminal smoke missing') }
	attempts := smoke.object_value('attempts') or { panic('terminal smoke attempts missing') }
	for attempt in attempts.array_value {
		completion := attempt.object_value('completion_operation_id') or { bin.JsonValue{} }
		if completion.kind == .string_value {
			selected_attempt = attempt
		}
	}
	if selected_attempt.kind != .object {
		panic('selected terminal smoke attempt missing')
	}
	run_id := if role == 'smoke' {
		(selected_attempt.object_value('run_id') or { panic('smoke run missing') }).int_value
	} else {
		(handoff.object_value('selected_run_id') or { panic('handoff run missing') }).int_value
	}
	run_attempt := if role == 'smoke' {
		(selected_attempt.object_value('run_attempt') or { panic('smoke attempt missing') }).int_value
	} else {
		(handoff.object_value('selected_run_attempt') or { panic('handoff attempt missing') }).int_value
	}
	workflow := if role == 'smoke' {
		(selected_attempt.object_value('workflow_path') or { panic('smoke workflow missing') }).string_value
	} else {
		(handoff.object_value('workflow_path') or { panic('handoff workflow missing') }).string_value
	}
	workflow_ref := if role == 'smoke' {
		(selected_attempt.object_value('workflow_ref') or { panic('smoke workflow ref missing') }).string_value
	} else {
		(handoff.object_value('workflow_ref') or { panic('handoff workflow ref missing') }).string_value
	}
	pre_projection := proof.object_value('source_atomic_pre_projection') or {
		panic('source atomic pre-projection missing')
	}
	workflow_sha := if role == 'smoke' {
		(pre_projection.object_value('v_source_sha') or { panic('V source SHA missing') }).string_value
	} else {
		(handoff.object_value('receiver_master_sha') or { panic('receiver master SHA missing') }).string_value
	}
	intent_id := (handoff.object_value('intent_or_operation_id') or {
		panic('handoff intent missing')
	}).string_value
	timestamp := if role == 'smoke' {
		(selected_attempt.object_value('completed_at') or {
			panic('smoke completion timestamp missing')
		}).string_value
	} else if role == 'business' {
		refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
		(refetch.object_value('checked_at') or { panic('source refetch timestamp missing') }).string_value
	} else {
		(handoff.object_value('terminal_completed_at') or {
			panic('terminal completion timestamp missing')
		}).string_value
	}
	path := bin.evidence_path(timestamp[..4].int(), timestamp[5..7].int(), run_id,
		int(run_attempt), target_id, operation_id, resulting_generation, transition,
		subject_fingerprint) or { panic(err) }
	target_path := bin.target_state_path(target_id) or { panic(err) }
	input_fingerprint := (target.object_value('input_fingerprint') or {
		panic('target input missing')
	}).string_value
	artifact_fingerprint := (target.object_value('artifact_fingerprint') or {
		panic('target artifact missing')
	}).string_value
	cas_attempt := live_atomic_cas_attempt(proof)
	body := '{"schema_version":1,"operation_id":"${operation_id}","operation_ordinal":${ordinal},"cas_attempt":${cas_attempt},"run_id":${run_id},"run_attempt":${run_attempt},"intent_id":"${intent_id}","transition":"${transition}","workflow":"${workflow}","workflow_ref":"${workflow_ref}","workflow_sha":"${workflow_sha}","subject_id":"${target_id}","subject_fingerprint":"${subject_fingerprint}","target_id":"${target_id}","input_fingerprint":"${input_fingerprint}","artifact_fingerprint":"${artifact_fingerprint}","generation_read":${resulting_generation - 1},"generation_written":${resulting_generation},"result":"blocked","digests":[{"path":"${target_path}","sha256":"${bin.json_sha256(target)}"}]}'
	return path, bin.canonical_json(bin.parse_strict_json(body) or { panic(err) })
}

fn live_subject_hash_from_value(subject bin.JsonValue) string {
	mut digests := []bin.DigestModel{}
	digest_values := subject.object_value('digests') or { panic('subject digests missing') }
	for digest in digest_values.array_value {
		digests << bin.DigestModel{
			path:   (digest.object_value('path') or { panic('digest path missing') }).string_value
			sha256: (digest.object_value('sha256') or { panic('digest SHA missing') }).string_value
		}
	}
	trigger_value := subject.object_value('remediation_trigger') or {
		bin.JsonValue{
			kind: .null_value
		}
	}
	mut trigger := bin.RemediationTriggerModel{}
	if trigger_value.kind == .object {
		trigger = bin.RemediationTriggerModel{
			repository:       (trigger_value.object_value('repository') or {
				panic('trigger repository missing')
			}).string_value
			ref:              (trigger_value.object_value('ref') or { panic('trigger ref missing') }).string_value
			before:           (trigger_value.object_value('before') or {
				panic('trigger before missing')
			}).string_value
			after:            (trigger_value.object_value('after') or {
				panic('trigger after missing')
			}).string_value
			tree:             (trigger_value.object_value('tree') or {
				panic('trigger tree missing')
			}).string_value
			diff_fingerprint: (trigger_value.object_value('diff_fingerprint') or {
				panic('trigger fingerprint missing')
			}).string_value
			owner_domain:     (trigger_value.object_value('owner_domain') or {
				panic('trigger owner missing')
			}).string_value
		}
	}
	return bin.native_gate_subject_hash(bin.NativeGateSubjectModel{
		consumer_id:            (subject.object_value('consumer_id') or {
			panic('consumer ID missing')
		}).string_value
		consumer_kind:          (subject.object_value('consumer_kind') or {
			panic('consumer kind missing')
		}).string_value
		intent_or_operation_id: (subject.object_value('intent_or_operation_id') or {
			panic('intent ID missing')
		}).string_value
		target_id:              (subject.object_value('target_id') or {
			panic('subject target missing')
		}).string_value
		subject_generation:     (subject.object_value('subject_generation') or {
			panic('subject generation missing')
		}).int_value
		initial_run_mode:       (subject.object_value('initial_run_mode') or {
			panic('initial run mode missing')
		}).string_value
		remediation_trigger:    trigger
		sha:                    (subject.object_value('sha') or { panic('subject SHA missing') }).string_value
		tree:                   (subject.object_value('tree') or { panic('subject tree missing') }).string_value
		original_ref:           (subject.object_value('original_ref') or {
			panic('subject ref missing')
		}).string_value
		input_fingerprint:      (subject.object_value('input_fingerprint') or {
			panic('subject input missing')
		}).string_value
		artifact_fingerprint:   (subject.object_value('artifact_fingerprint') or {
			panic('subject artifact missing')
		}).string_value
		manifest_hash:          (subject.object_value('manifest_hash') or {
			panic('subject manifest missing')
		}).string_value
		digests:                digests
	}) or { panic(err) }
}

fn live_retarget_recovery_target(source string, target_id string) string {
	old_root := bin.parse_strict_json(source) or { panic(err) }
	old_target_id := (old_root.object_value('target_id') or { panic('target ID missing') }).string_value
	if old_target_id == target_id {
		return source
	}
	old_subject := old_root.object_value('native_gate_subject') or {
		panic('native subject missing')
	}
	old_hash := (old_root.object_value('active_subject_hash') or {
		panic('active subject hash missing')
	}).string_value
	old_handoffs := old_root.object_value('recovery_handoffs') or {
		panic('recovery handoffs missing')
	}
	old_predecessor := old_handoffs.array_value[0]
	old_successor := old_handoffs.array_value[1]
	old_predecessor_id := (old_predecessor.object_value('handoff_id') or {
		panic('predecessor ID missing')
	}).string_value
	old_successor_id := (old_successor.object_value('handoff_id') or {
		panic('successor ID missing')
	}).string_value
	old_creation_commitment := bin.recovery_handoff_creation_commitment(old_predecessor) or {
		panic(err)
	}
	old_successor_commitment := bin.recovery_native_successor_commitment(old_predecessor) or {
		panic(err)
	}
	old_native_digest := (old_predecessor.object_value('native_gate_evidence_digest') or {
		panic('native evidence digest missing')
	}).string_value
	old_native_check_digest := (old_predecessor.object_value('native_gate_check_digest') or {
		panic('native check digest missing')
	}).string_value
	consumer_id := (old_subject.object_value('consumer_id') or { panic('subject consumer missing') }).string_value
	consumer_kind := (old_subject.object_value('consumer_kind') or { panic('subject kind missing') }).string_value
	plain_owner_transition := match consumer_kind {
		'publish_post' { 'promotion_confirmed' }
		'rollback_post' { 'rollback_promoted' }
		'remediation' { 'begin_remediation' }
		else { panic('unsupported retarget owner') }
	}
	mut old_owner_transition := ''
	old_operations := old_root.object_value('applied_operations') or {
		panic('target operations missing')
	}
	for operation in old_operations.array_value {
		if (operation.object_value('operation_id') or { bin.JsonValue{} }).string_value == consumer_id {
			old_owner_transition = (operation.object_value('transition') or {
				panic('owner transition missing')
			}).string_value
		}
	}
	if !old_owner_transition.starts_with('${plain_owner_transition}_') {
		panic('decorated owner transition missing')
	}

	mut result := bin.canonical_json(old_root).replace(old_target_id, target_id)
	retargeted_root := bin.parse_strict_json(result) or { panic(err) }
	retargeted_subject := retargeted_root.object_value('native_gate_subject') or {
		panic('retargeted subject missing')
	}
	new_hash := live_subject_hash_from_value(retargeted_subject)
	result = result.replace(old_hash, new_hash)
	for attempt in [1, 2] {
		old_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
			consumer_id, old_hash, 3001, attempt) or { panic(err) }
		new_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
			consumer_id, new_hash, 3001, attempt) or { panic(err) }
		result = result.replace(old_external, new_external)
	}
	old_native_external := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		consumer_id, old_hash, 7002, 1) or { panic(err) }
	new_native_external := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		consumer_id, new_hash, 7002, 1) or { panic(err) }
	result = result.replace(old_native_external, new_native_external)

	mut current_root := bin.parse_strict_json(result) or { panic(err) }
	current_smoke := current_root.object_value('v_smoke_execution') or {
		panic('current smoke missing')
	}
	if current_smoke.kind == .object {
		result = result.replace(bin.canonical_json(current_smoke),
			live_refreshed_smoke(current_smoke))
	}
	current_root = bin.parse_strict_json(result) or { panic(err) }
	mut current_handoffs := current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	mut current_successor := current_handoffs.array_value[1]
	mut current_proof := current_successor.object_value('terminal_revalidation') or {
		bin.JsonValue{
			kind: .null_value
		}
	}
	if current_proof.kind == .object {
		proof_smoke := current_proof.object_value('v_smoke_execution') or {
			panic('proof smoke missing')
		}
		result = result.replace(bin.canonical_json(proof_smoke), live_refreshed_smoke(proof_smoke))
		current_root = bin.parse_strict_json(result) or { panic(err) }
		current_handoffs = current_root.object_value('recovery_handoffs') or {
			panic('current handoffs missing')
		}
		current_successor = current_handoffs.array_value[1]
		current_proof = current_successor.object_value('terminal_revalidation') or {
			panic('terminal proof missing')
		}
		atomic_pre_projection := current_proof.object_value('source_atomic_pre_projection') or {
			bin.JsonValue{
				kind: .null_value
			}
		}
		if atomic_pre_projection.kind == .object {
			atomic_pre_smoke := atomic_pre_projection.object_value('v_smoke_execution') or {
				panic('source atomic pre-projection smoke missing')
			}
			canonical_atomic_pre := bin.canonical_json(atomic_pre_projection)
			canonical_atomic_pre_smoke := bin.canonical_json(atomic_pre_smoke)
			atomic_pre_smoke_anchor := '"v_smoke_execution":${canonical_atomic_pre_smoke}'
			if canonical_atomic_pre.count(atomic_pre_smoke_anchor) != 1 {
				panic('source atomic pre-projection smoke anchor must occur exactly once')
			}
			refreshed_atomic_pre := canonical_atomic_pre.replace_once(atomic_pre_smoke_anchor,
				'"v_smoke_execution":${live_refreshed_smoke(atomic_pre_smoke)}')
			atomic_pre_anchor := '"source_atomic_pre_projection":${canonical_atomic_pre}'
			if result.count(atomic_pre_anchor) != 1 {
				panic('source atomic pre-projection anchor must occur exactly once')
			}
			result = result.replace_once(atomic_pre_anchor,
				'"source_atomic_pre_projection":${refreshed_atomic_pre}')
		}
		current_root = bin.parse_strict_json(result) or { panic(err) }
		current_handoffs = current_root.object_value('recovery_handoffs') or {
			panic('current handoffs missing')
		}
		current_successor = current_handoffs.array_value[1]
		current_proof = current_successor.object_value('terminal_revalidation') or {
			panic('terminal proof missing')
		}
		old_check := current_proof.object_value('native_gate_check') or {
			panic('native check missing')
		}
		proof_native := current_proof.object_value('native_gate_execution') or {
			panic('proof native execution missing')
		}
		epochs := proof_native.object_value('gate_epochs') or { panic('native epochs missing') }
		native_ref := (epochs.array_value[0].object_value('expected_ref') or {
			panic('native ref missing')
		}).string_value
		new_check := live_recovery_native_check_for(new_hash, consumer_id, native_ref)
		result = result.replace(bin.canonical_json(old_check), new_check)
	}

	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	current_successor = current_handoffs.array_value[1]
	current_proof = current_successor.object_value('terminal_revalidation') or {
		bin.JsonValue{
			kind: .null_value
		}
	}
	current_native := if current_proof.kind == .object {
		current_proof.object_value('native_gate_execution') or {
			panic('proof native execution missing')
		}
	} else {
		current_root.object_value('native_gate_execution') or {
			panic('current native execution missing')
		}
	}
	new_native_digest := bin.native_gate_evidence_digest(current_native) or { panic(err) }
	epochs := current_native.object_value('gate_epochs') or { panic('native epochs missing') }
	native_ref := (epochs.array_value[0].object_value('expected_ref') or {
		panic('native ref missing')
	}).string_value
	new_check_value := bin.parse_strict_json(live_recovery_native_check_for(new_hash, consumer_id,
		native_ref)) or { panic(err) }
	new_native_check_digest := bin.native_gate_check_digest(new_check_value) or { panic(err) }
	result = result.replace(old_native_digest, new_native_digest).replace(old_native_check_digest,
		new_native_check_digest)

	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	mut current_predecessor := current_handoffs.array_value[0]
	recovery_operation_id := (current_predecessor.object_value('recovery_operation_id') or {
		panic('recovery operation missing')
	}).string_value
	new_predecessor_id := bin.deterministic_handoff_id('vlang/v:tccbin-automation-state',
		recovery_operation_id, consumer_id, new_hash, 0)
	new_successor_id := bin.deterministic_handoff_id('vlang/v:tccbin-automation-state',
		recovery_operation_id, consumer_id, new_hash, 1)
	result = result.replace(old_predecessor_id, new_predecessor_id).replace(old_successor_id,
		new_successor_id)
	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	current_predecessor = current_handoffs.array_value[0]
	new_creation_commitment := bin.recovery_handoff_creation_commitment(current_predecessor) or {
		panic(err)
	}
	new_successor_commitment := bin.recovery_native_successor_commitment(current_predecessor) or {
		panic(err)
	}
	result = result.replace(old_creation_commitment, new_creation_commitment).replace(old_successor_commitment,
		new_successor_commitment)

	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	current_successor = current_handoffs.array_value[1]
	current_proof = current_successor.object_value('terminal_revalidation') or {
		bin.JsonValue{
			kind: .null_value
		}
	}
	owner_projection := if current_proof.kind == .object {
		current_proof.object_value('pre_business_projection') or {
			panic('owner projection missing')
		}
	} else {
		bin.terminal_state_projection(current_root) or { panic(err) }
	}
	new_owner_digest := bin.terminal_owner_payload_digest(owner_projection) or { panic(err) }
	result = result.replace(old_owner_transition, '${plain_owner_transition}_${new_owner_digest}')

	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	current_successor = current_handoffs.array_value[1]
	current_proof = current_successor.object_value('terminal_revalidation') or {
		bin.JsonValue{
			kind: .null_value
		}
	}
	if current_proof.kind == .object {
		old_refetch := current_proof.object_value('source_refetch') or {
			panic('source refetch missing')
		}
		old_refetch_digest := (old_refetch.object_value('evidence_digest') or {
			panic('source refetch digest missing')
		}).string_value
		new_refetch_digest := bin.source_refetch_evidence_digest(old_refetch, current_proof.object_value('source_state_pre_snapshot') or {
			panic('source pre-state missing')
		}, current_proof.object_value('source_state_snapshot') or {
			panic('source post-state missing')
		}, current_proof.object_value('source_state_cas_history') or {
			panic('source history missing')
		}) or { panic(err) }
		result = result.replace(old_refetch_digest, new_refetch_digest)
		current_root = bin.parse_strict_json(result) or { panic(err) }
		current_handoffs = current_root.object_value('recovery_handoffs') or {
			panic('current handoffs missing')
		}
		current_proof = current_handoffs.array_value[1].object_value('terminal_revalidation') or {
			panic('terminal proof missing')
		}
		old_proof_digest := (current_proof.object_value('facts_digest') or {
			panic('terminal proof digest missing')
		}).string_value
		new_proof_digest := bin.terminal_revalidation_facts_digest(current_proof) or { panic(err) }
		result = result.replace(old_proof_digest, new_proof_digest)
	}
	return bin.canonical_json(bin.parse_strict_json(result) or { panic(err) })
}

fn live_atomic_target_operation_id(parent_target bin.JsonValue, target bin.JsonValue,
	proof bin.JsonValue, handoff bin.JsonValue, operation bin.JsonValue, role string,
	ordinal int) string {
	resolved := parent_target.object_value('resolved_inputs') or {
		panic('resolved inputs missing')
	}
	sources := resolved.object_value('sources') or { panic('resolved sources missing') }
	refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	source_id := (refetch.object_value('source_id') or { panic('source ID missing') }).string_value
	mut primary_source := bin.JsonValue{
		kind: .null_value
	}
	for source in sources.array_value {
		id := source.object_value('id') or { bin.JsonValue{} }
		if id.kind == .string_value && id.string_value == source_id {
			primary_source = source
		}
	}
	if primary_source.kind != .object {
		panic('resolved outage source missing')
	}
	pre_projection := proof.object_value('source_atomic_pre_projection') or {
		panic('source atomic pre-projection missing')
	}
	mut selected_attempt := bin.JsonValue{
		kind: .null_value
	}
	smoke := proof.object_value('v_smoke_execution') or { panic('terminal smoke missing') }
	attempts := smoke.object_value('attempts') or { panic('terminal smoke attempts missing') }
	for attempt in attempts.array_value {
		completion := attempt.object_value('completion_operation_id') or { bin.JsonValue{} }
		if completion.kind == .string_value {
			selected_attempt = attempt
		}
	}
	if selected_attempt.kind != .object {
		panic('selected terminal smoke attempt missing')
	}
	run_id := if role == 'smoke' {
		(selected_attempt.object_value('run_id') or { panic('smoke run missing') }).int_value
	} else {
		(handoff.object_value('selected_run_id') or { panic('handoff run missing') }).int_value
	}
	run_attempt := if role == 'smoke' {
		int((selected_attempt.object_value('run_attempt') or { panic('smoke attempt missing') }).int_value)
	} else {
		int((handoff.object_value('selected_run_attempt') or { panic('handoff attempt missing') }).int_value)
	}
	identity_transition := match role {
		'smoke' { 'v-smoke-complete' }
		'business' { 'source_unreachable' }
		'completion' { 'handoff_complete' }
		else { panic('unknown live target operation role') }
	}
	return bin.deterministic_operation_id(bin.OperationIdentityInput{
		audience:                'vlang/v:tccbin-automation-state'
		run_id:                  run_id
		run_attempt:             run_attempt
		ordinal:                 ordinal
		cas_attempt:             live_atomic_cas_attempt(proof)
		subject_id:              (target.object_value('target_id') or { panic('target ID missing') }).string_value
		transition:              identity_transition
		expected_generation:     (operation.object_value('resulting_generation') or {
			panic('completion generation missing')
		}).int_value - 1
		expected_canonical_head: (parent_target.object_value('canonical_observed_sha') or {
			panic('canonical HEAD missing')
		}).string_value
		source_ref:              (primary_source.object_value('ref') or {
			panic('source ref missing')
		}).string_value
		source_sha:              (primary_source.object_value('sha') or {
			panic('source SHA missing')
		}).string_value
		subject_fingerprint:     (parent_target.object_value('input_fingerprint') or {
			panic('target input missing')
		}).string_value
		input_fingerprint:       (parent_target.object_value('input_fingerprint') or {
			panic('target input missing')
		}).string_value
		artifact_fingerprint:    (parent_target.object_value('artifact_fingerprint') or {
			panic('target artifact missing')
		}).string_value
		manifest_hash:           (parent_target.object_value('manifest_hash') or {
			panic('target manifest missing')
		}).string_value
		native_subject_hash:     (pre_projection.object_value('native_subject_hash') or {
			panic('native subject hash missing')
		}).string_value
		intent_id:               (handoff.object_value('intent_or_operation_id') or {
			panic('handoff intent missing')
		}).string_value
	}) or { panic(err) }
}

fn live_refreshed_smoke(smoke bin.JsonValue) string {
	refreshed := bin.parse_strict_json(refresh_v_smoke_facts_digests('{"v_smoke_execution":${bin.canonical_json(smoke)}}')) or {
		panic(err)
	}
	return bin.canonical_json(refreshed.object_value('v_smoke_execution') or {
		panic('refreshed V smoke missing')
	})
}

fn live_atomic_rebind_target_operation_ids(source string, smoke_operation_id string,
	business_operation_id string, completion_operation_id string) string {
	initial_root := bin.parse_strict_json(source) or { panic(err) }
	initial_operations := initial_root.object_value('applied_operations') or {
		panic('target operations missing')
	}
	target_operations := initial_operations.array_value[initial_operations.array_value.len - 3..]
	old_smoke_id := (target_operations[0].object_value('operation_id') or {
		panic('smoke operation missing')
	}).string_value
	old_business_id := (target_operations[1].object_value('operation_id') or {
		panic('business operation missing')
	}).string_value
	business_resulting_generation := (target_operations[1].object_value('resulting_generation') or {
		panic('business resulting generation missing')
	}).int_value
	old_completion_id := (target_operations[2].object_value('operation_id') or {
		panic('completion operation missing')
	}).string_value
	mut result := bin.canonical_json(initial_root).replace(old_smoke_id, smoke_operation_id).replace(old_completion_id,
		completion_operation_id)

	root_after_target_ids := bin.parse_strict_json(result) or { panic(err) }
	handoffs_after_target_ids := root_after_target_ids.object_value('recovery_handoffs') or {
		panic('target handoffs missing')
	}
	proof_after_target_ids := handoffs_after_target_ids.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal proof missing')
	}
	old_refetch := proof_after_target_ids.object_value('source_refetch') or {
		panic('source refetch missing')
	}
	old_refetch_digest := (old_refetch.object_value('evidence_digest') or {
		panic('source refetch digest missing')
	}).string_value
	mut refetch_source := bin.canonical_json(old_refetch).replace_once('"operation_id":"${old_business_id}"',
		'"operation_id":"${business_operation_id}"').replace_once('"evidence_digest":"${old_refetch_digest}"',
		'"evidence_digest":"0000000000000000000000000000000000000000000000000000000000000000"')
	refetch_seed := bin.parse_strict_json(refetch_source) or { panic(err) }
	new_refetch_digest := bin.source_refetch_evidence_digest(refetch_seed, proof_after_target_ids.object_value('source_state_pre_snapshot') or {
		panic('source pre-state missing')
	}, proof_after_target_ids.object_value('source_state_snapshot') or {
		panic('source post-state missing')
	}, proof_after_target_ids.object_value('source_state_cas_history') or {
		panic('source history missing')
	}) or { panic(err) }
	refetch_source = refetch_source.replace_once('0000000000000000000000000000000000000000000000000000000000000000',
		new_refetch_digest)
	result = result.replace(bin.canonical_json(old_refetch), refetch_source)
	result = result.replace('"business_operation_id":"${old_business_id}"',
		'"business_operation_id":"${business_operation_id}"')
	old_business_cas_anchor := '"operation_id":"${old_business_id}","resulting_generation":${business_resulting_generation},"transition":"source_unreachable_${old_refetch_digest}"'
	if result.count(old_business_cas_anchor) != 1 {
		panic('business target CAS anchor must occur exactly once')
	}
	result = result.replace_once(old_business_cas_anchor,
		'"operation_id":"${business_operation_id}","resulting_generation":${business_resulting_generation},"transition":"source_unreachable_${new_refetch_digest}"')

	mut current_root := bin.parse_strict_json(result) or { panic(err) }
	current_smoke := current_root.object_value('v_smoke_execution') or {
		panic('current V smoke missing')
	}
	result = result.replace(bin.canonical_json(current_smoke), live_refreshed_smoke(current_smoke))
	current_root = bin.parse_strict_json(result) or { panic(err) }
	mut current_handoffs := current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	mut current_proof := current_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('current terminal proof missing')
	}
	proof_smoke := current_proof.object_value('v_smoke_execution') or {
		panic('proof V smoke missing')
	}
	result = result.replace(bin.canonical_json(proof_smoke), live_refreshed_smoke(proof_smoke))

	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_operations := current_root.object_value('applied_operations') or {
		panic('current operations missing')
	}
	current_target_operations := current_operations.array_value[current_operations.array_value.len - 3..]
	old_smoke_transition := (current_target_operations[0].object_value('transition') or {
		panic('smoke transition missing')
	}).string_value
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	current_proof = current_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('current terminal proof missing')
	}
	proof_smoke_after_refresh := current_proof.object_value('v_smoke_execution') or {
		panic('refreshed proof V smoke missing')
	}
	proof_attempts := proof_smoke_after_refresh.object_value('attempts') or {
		panic('refreshed proof attempts missing')
	}
	selected_attempt := proof_attempts.array_value[proof_attempts.array_value.len - 1]
	new_smoke_digest := bin.v_smoke_terminal_payload_digest(proof_smoke_after_refresh,
		selected_attempt) or { panic(err) }
	new_smoke_transition := 'v-smoke-complete-${(selected_attempt.object_value('attempt_index') or {
		panic('selected attempt index missing')
	}).int_value}_${new_smoke_digest}'
	result = result.replace('"transition":"${old_smoke_transition}"',
		'"transition":"${new_smoke_transition}"')

	current_root = bin.parse_strict_json(result) or { panic(err) }
	current_handoffs = current_root.object_value('recovery_handoffs') or {
		panic('current handoffs missing')
	}
	current_proof = current_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('current terminal proof missing')
	}
	old_proof_digest := (current_proof.object_value('facts_digest') or {
		panic('terminal proof digest missing')
	}).string_value
	new_proof_digest := bin.terminal_revalidation_facts_digest(current_proof) or { panic(err) }
	result = result.replace(old_proof_digest, new_proof_digest)
	return bin.canonical_json(bin.parse_strict_json(result) or { panic(err) })
}

fn live_atomic_rebind_for_ordinals(parent_source string, target_source string, smoke_ordinal int,
	business_ordinal int, completion_ordinal int) string {
	parent := bin.parse_strict_json(parent_source) or { panic(err) }
	target := bin.parse_strict_json(target_source) or { panic(err) }
	handoffs := target.object_value('recovery_handoffs') or { panic('target handoffs missing') }
	handoff := handoffs.array_value[1]
	proof := handoff.object_value('terminal_revalidation') or { panic('terminal proof missing') }
	operations := target.object_value('applied_operations') or {
		panic('target operations missing')
	}
	target_operations := operations.array_value[operations.array_value.len - 3..]
	smoke_operation_id := live_atomic_target_operation_id(parent, target, proof, handoff,
		target_operations[0], 'smoke', smoke_ordinal)
	business_operation_id := live_atomic_target_operation_id(parent, target, proof, handoff,
		target_operations[1], 'business', business_ordinal)
	completion_operation_id := live_atomic_target_operation_id(parent, target, proof, handoff,
		target_operations[2], 'completion', completion_ordinal)
	return live_atomic_rebind_target_operation_ids(target_source, smoke_operation_id,
		business_operation_id, completion_operation_id)
}

fn live_target_evidence_timestamp(proof bin.JsonValue, handoff bin.JsonValue, role string) string {
	if role == 'smoke' {
		smoke := proof.object_value('v_smoke_execution') or { panic('terminal smoke missing') }
		attempts := smoke.object_value('attempts') or { panic('terminal smoke attempts missing') }
		for attempt in attempts.array_value {
			completion := attempt.object_value('completion_operation_id') or { bin.JsonValue{} }
			if completion.kind == .string_value {
				return (attempt.object_value('completed_at') or {
					panic('smoke completion timestamp missing')
				}).string_value
			}
		}
		panic('selected terminal smoke attempt missing')
	}
	if role == 'business' {
		refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
		return (refetch.object_value('checked_at') or { panic('source refetch timestamp missing') }).string_value
	}
	if role == 'completion' {
		return (handoff.object_value('terminal_completed_at') or {
			panic('terminal completion timestamp missing')
		}).string_value
	}
	panic('unknown target evidence role')
}

fn live_mutated_target_evidence(body string, field string, operation_id string,
	authoritative_timestamp string) (string, string) {
	value := bin.parse_strict_json(body) or { panic(err) }
	mut mutated := bin.canonical_json(value)
	if field in ['run_id', 'run_attempt', 'operation_ordinal', 'cas_attempt'] {
		old_value := (value.object_value(field) or { panic('${field} missing') }).int_value
		new_value := if field == 'cas_attempt' { i64(2) } else { old_value + 17 }
		mutated = mutated.replace_once('"${field}":${old_value}', '"${field}":${new_value}')
	} else if field in ['workflow', 'workflow_ref', 'workflow_sha', 'intent_id', 'subject_id',
		'target_id', 'subject_fingerprint', 'input_fingerprint', 'artifact_fingerprint'] {
		old_value := (value.object_value(field) or { panic('${field} missing') }).string_value
		new_value := match field {
			'workflow' {
				if old_value == '.github/workflows/tccbin_revalidate.yml' {
					'.github/workflows/update_tccbin.yml'
				} else {
					'.github/workflows/tccbin_revalidate.yml'
				}
			}
			'workflow_ref' {
				'main'
			}
			'workflow_sha' {
				if old_value == 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' {
					'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
				} else {
					'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
				}
			}
			'intent_id', 'subject_fingerprint', 'input_fingerprint', 'artifact_fingerprint' {
				'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
			}
			'subject_id', 'target_id' {
				'freebsd-amd64'
			}
			else {
				panic('unsupported target evidence string mutation')
			}
		}
		if old_value == new_value {
			panic('target evidence mutation must change ${field}')
		}
		mutated = mutated.replace_once('"${field}":"${old_value}"', '"${field}":"${new_value}"')
	} else if field == 'generation' {
		generation_read := (value.object_value('generation_read') or {
			panic('generation read missing')
		}).int_value
		generation_written := (value.object_value('generation_written') or {
			panic('generation written missing')
		}).int_value
		mutated = mutated.replace_once('"generation_read":${generation_read}', '"generation_read":${
			generation_read + 1}').replace_once('"generation_written":${generation_written}', '"generation_written":${
			generation_written + 1}')
	} else if field == 'transition' {
		old_value := (value.object_value('transition') or { panic('transition missing') }).string_value
		mutated = mutated.replace_once('"transition":"${old_value}"',
			'"transition":"foreign_${old_value}"')
	} else if field == 'result' {
		old_value := (value.object_value('result') or { panic('result missing') }).string_value
		new_value := if old_value == 'failed' { 'blocked' } else { 'failed' }
		mutated = mutated.replace_once('"result":"${old_value}"', '"result":"${new_value}"')
	} else if field == 'digest' {
		digests := value.object_value('digests') or { panic('digests missing') }
		old_value := (digests.array_value[0].object_value('sha256') or {
			panic('digest SHA missing')
		}).string_value
		new_value := if old_value == 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff' {
			'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
		} else {
			'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		}
		mutated = mutated.replace_once('"sha256":"${old_value}"', '"sha256":"${new_value}"')
	} else if field != 'path_month' {
		panic('unsupported universal evidence mutation ${field}')
	}
	// For path_month, the body and operation identity remain coherent; only the candidate path
	// lies about the month so the live contextual timestamp derivation is the rejecting invariant.
	old_operation_id := (value.object_value('operation_id') or {
		panic('evidence operation missing')
	}).string_value
	mutated = mutated.replace_once('"operation_id":"${old_operation_id}"',
		'"operation_id":"${operation_id}"')
	mutated_value := bin.parse_strict_json(mutated) or { panic(err) }
	new_path := bin.evidence_path(authoritative_timestamp[..4].int(),
		authoritative_timestamp[5..7].int(), (mutated_value.object_value('run_id') or {
		panic('run missing')
	}).int_value,
		int((mutated_value.object_value('run_attempt') or { panic('attempt missing') }).int_value), (mutated_value.object_value('subject_id') or {
		panic('subject missing')
	}).string_value, operation_id, (mutated_value.object_value('generation_written') or {
		panic('generation missing')
	}).int_value,
		(mutated_value.object_value('transition') or { panic('transition missing') }).string_value, (mutated_value.object_value('subject_fingerprint') or {
		panic('fingerprint missing')
	}).string_value) or { panic(err) }
	if field == 'path_month' {
		foreign_month := if authoritative_timestamp[5..7] == '08' { '07' } else { '08' }
		return new_path.replace_once('evidence/${authoritative_timestamp[..4]}/${authoritative_timestamp[5..7]}/',
			'evidence/${authoritative_timestamp[..4]}/${foreign_month}/'), bin.canonical_json(mutated_value)
	}
	return new_path, bin.canonical_json(mutated_value)
}

fn live_foreign_target_operation_id(parent_target bin.JsonValue, proof bin.JsonValue,
	handoff bin.JsonValue, evidence bin.JsonValue, role string, field string) string {
	resolved := parent_target.object_value('resolved_inputs') or {
		panic('resolved inputs missing')
	}
	sources := resolved.object_value('sources') or { panic('resolved sources missing') }
	refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	source_id := (refetch.object_value('source_id') or { panic('source ID missing') }).string_value
	mut primary_source := bin.JsonValue{
		kind: .null_value
	}
	for source in sources.array_value {
		id := source.object_value('id') or { bin.JsonValue{} }
		if id.kind == .string_value && id.string_value == source_id {
			primary_source = source
		}
	}
	if primary_source.kind != .object {
		panic('resolved outage source missing')
	}
	pre_projection := proof.object_value('source_atomic_pre_projection') or {
		panic('source atomic pre-projection missing')
	}
	logical_transition := match role {
		'smoke' { 'v-smoke-complete' }
		'business' { 'source_unreachable' }
		'completion' { 'handoff_complete' }
		else { panic('unknown live target operation role') }
	}
	identity_transition := if field == 'transition' {
		'foreign_${logical_transition}'
	} else {
		logical_transition
	}
	return bin.deterministic_operation_id(bin.OperationIdentityInput{
		audience:                'vlang/v:tccbin-automation-state'
		run_id:                  (evidence.object_value('run_id') or { panic('run missing') }).int_value
		run_attempt:             int((evidence.object_value('run_attempt') or {
			panic('run attempt missing')
		}).int_value)
		ordinal:                 int((evidence.object_value('operation_ordinal') or {
			panic('operation ordinal missing')
		}).int_value)
		cas_attempt:             int((evidence.object_value('cas_attempt') or {
			panic('CAS attempt missing')
		}).int_value)
		subject_id:              (evidence.object_value('subject_id') or {
			panic('subject ID missing')
		}).string_value
		transition:              identity_transition
		expected_generation:     (evidence.object_value('generation_read') or {
			panic('generation read missing')
		}).int_value
		expected_canonical_head: (parent_target.object_value('canonical_observed_sha') or {
			panic('canonical HEAD missing')
		}).string_value
		source_ref:              (primary_source.object_value('ref') or {
			panic('source ref missing')
		}).string_value
		source_sha:              (primary_source.object_value('sha') or {
			panic('source SHA missing')
		}).string_value
		subject_fingerprint:     (evidence.object_value('subject_fingerprint') or {
			panic('subject fingerprint missing')
		}).string_value
		input_fingerprint:       (evidence.object_value('input_fingerprint') or {
			panic('input fingerprint missing')
		}).string_value
		artifact_fingerprint:    (evidence.object_value('artifact_fingerprint') or {
			panic('artifact fingerprint missing')
		}).string_value
		manifest_hash:           (parent_target.object_value('manifest_hash') or {
			panic('target manifest missing')
		}).string_value
		native_subject_hash:     (pre_projection.object_value('native_subject_hash') or {
			panic('native subject hash missing')
		}).string_value
		intent_id:               (evidence.object_value('intent_id') or { panic('intent missing') }).string_value
	}) or { panic(err) }
}

fn prepare_live_multi_source_atomic_state(suffix string,
	shared_source bool) LiveMultiAtomicStateRepository {
	work_root := os.join_path(os.temp_dir(), 'tccbin-live-multi-${os.getpid()}-${suffix}')
	bare_root := '${work_root}.git'
	os.rmdir_all(work_root) or {}
	os.rmdir_all(bare_root) or {}
	os.mkdir_all(os.join_path(work_root, 'targets')) or { panic(err) }
	os.mkdir_all(os.join_path(work_root, 'sources')) or { panic(err) }
	bootstrap := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'target-state.bootstrap.schema-fixture.json')) or { panic(err) }
	for target_id in ['linux-amd64', 'freebsd-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64'] {
		write_live_state_blob(work_root, 'targets/${target_id}.json', bootstrap.replace('linux-amd64',
			target_id))
	}
	source_fixture := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'source-state.outage.schema-fixture.json')) or { panic(err) }
	write_live_state_blob(work_root, 'sources/tinycc-mob.json', source_fixture)
	write_live_state_blob(work_root, 'sources/bdwgc-master.json', source_fixture.replace('tinycc-mob',
		'bdwgc-master').replace('https://repo.or.cz/tinycc.git', 'https://github.com/ivmai/bdwgc').replace('"ref": "mob"',
		'"ref": "master"'))
	write_live_state_blob(work_root, 'sources/libatomic_ops-master.json', source_fixture.replace('tinycc-mob',
		'libatomic_ops-master').replace('https://repo.or.cz/tinycc.git',
		'https://github.com/bdwgc/libatomic_ops').replace('"ref": "mob"', '"ref": "master"'))
	for command in [
		'git -C ${os.quoted_path(work_root)} init -q',
		'git -C ${os.quoted_path(work_root)} checkout -qb tccbin-automation-state',
		'git -C ${os.quoted_path(work_root)} config user.email ci@example.invalid',
		'git -C ${os.quoted_path(work_root)} config user.name "Contract Test"',
		'git -C ${os.quoted_path(work_root)} add -- targets sources',
		'git -C ${os.quoted_path(work_root)} commit -qm bootstrap',
	] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}

	linux_parent := live_recovery_h2_dispatched_source_for('publish_post')
	freebsd_source_kind := if shared_source { 'tinycc' } else { 'bdwgc' }
	freebsd_parent_base := live_target_for_source_kind(live_recovery_h2_dispatched_source_for('remediation'),
		freebsd_source_kind)
	freebsd_parent := live_retarget_recovery_target(freebsd_parent_base, 'freebsd-amd64')
	linux_parent_root := bin.parse_strict_json(linux_parent) or { panic(err) }
	freebsd_parent_root := bin.parse_strict_json(freebsd_parent) or { panic(err) }
	linux_consumer := ((linux_parent_root.object_value('native_gate_subject') or {
		panic('linux subject missing')
	}).object_value('consumer_id') or { panic('linux consumer missing') }).string_value
	freebsd_consumer := ((freebsd_parent_root.object_value('native_gate_subject') or {
		panic('freebsd subject missing')
	}).object_value('consumer_id') or { panic('freebsd consumer missing') }).string_value
	mut shared_consumers := [linux_consumer, freebsd_consumer]
	shared_consumers.sort()
	linux_source_ordinal := if shared_source { 2 } else { 3 }
	freebsd_source_ordinal := 2
	linux_provisional := live_recovery_h2_source_waiting_variant_with_parent_and_consumers_for('publish_post',
		false, false, 'abababababababababababababababababababab', if shared_source {
		shared_consumers
	} else {
		[linux_consumer]
	}, linux_source_ordinal, 'tinycc', 1)
	freebsd_provisional_base := live_recovery_h2_source_waiting_variant_with_parent_and_consumers_for('remediation',
		false, false, 'abababababababababababababababababababab', if shared_source {
		shared_consumers
	} else {
		[freebsd_consumer]
	}, freebsd_source_ordinal, freebsd_source_kind, 1)
	freebsd_provisional := live_retarget_recovery_target(freebsd_provisional_base, 'freebsd-amd64')
	linux_provisional_root := bin.parse_strict_json(linux_provisional) or { panic(err) }
	freebsd_provisional_root := bin.parse_strict_json(freebsd_provisional) or { panic(err) }
	linux_provisional_handoffs := linux_provisional_root.object_value('recovery_handoffs') or {
		panic('linux handoffs missing')
	}
	freebsd_provisional_handoffs := freebsd_provisional_root.object_value('recovery_handoffs') or {
		panic('freebsd handoffs missing')
	}
	linux_pre_source := (linux_provisional_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('linux proof missing')
	}).object_value('source_state_pre_snapshot') or { panic('linux source pre-state missing') }
	freebsd_pre_source := (freebsd_provisional_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('freebsd proof missing')
	}).object_value('source_state_pre_snapshot') or { panic('freebsd source pre-state missing') }
	write_live_state_blob(work_root, 'targets/linux-amd64.json', linux_parent)
	write_live_state_blob(work_root, 'targets/freebsd-amd64.json', freebsd_parent)
	write_live_state_blob(work_root, 'sources/tinycc-mob.json',
		bin.canonical_json(linux_pre_source))
	mut parent_paths := 'targets/linux-amd64.json targets/freebsd-amd64.json sources/tinycc-mob.json'
	if !shared_source {
		write_live_state_blob(work_root, 'sources/bdwgc-master.json',
			bin.canonical_json(freebsd_pre_source))
		parent_paths += ' sources/bdwgc-master.json'
	} else {
		assert bin.canonical_json(freebsd_pre_source) == bin.canonical_json(linux_pre_source)
	}
	for command in [
		'git -C ${os.quoted_path(work_root)} add -- ${parent_paths}',
		'git -C ${os.quoted_path(work_root)} commit -qm atomic-parent-k2',
	] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	parent := os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()

	mut linux_target := live_recovery_h2_source_waiting_variant_with_parent_and_consumers_for('publish_post',
		false, false, parent, if shared_source { shared_consumers } else { [
			linux_consumer,
		] }, linux_source_ordinal, 'tinycc', 1)
	freebsd_target_base := live_recovery_h2_source_waiting_variant_with_parent_and_consumers_for('remediation',
		false, false, parent, if shared_source { shared_consumers } else { [
			freebsd_consumer,
		] }, freebsd_source_ordinal, freebsd_source_kind, 1)
	mut freebsd_target := live_retarget_recovery_target(freebsd_target_base, 'freebsd-amd64')
	source_count := if shared_source { 1 } else { 2 }
	freebsd_business_ordinal := 2 + source_count
	linux_business_ordinal := freebsd_business_ordinal + 2
	freebsd_target = live_atomic_rebind_for_ordinals(freebsd_parent, freebsd_target, 0,
		freebsd_business_ordinal, freebsd_business_ordinal + 1)
	linux_target = live_atomic_rebind_for_ordinals(linux_parent, linux_target, 1,
		linux_business_ordinal, linux_business_ordinal + 1)

	mut source_evidence_by_path := map[string]string{}
	for target_source in [freebsd_target, linux_target] {
		target_root := bin.parse_strict_json(target_source) or { panic(err) }
		handoffs := target_root.object_value('recovery_handoffs') or {
			panic('terminal handoffs missing')
		}
		handoff := handoffs.array_value[1]
		proof := handoff.object_value('terminal_revalidation') or {
			panic('terminal proof missing')
		}
		post_source := proof.object_value('source_state_snapshot') or {
			panic('source post-state missing')
		}
		history := proof.object_value('source_state_cas_history') or {
			panic('source history missing')
		}
		transition := history.array_value[0]
		source_path := bin.source_state_path((transition.object_value('source_id') or {
			panic('transition source ID missing')
		}).string_value) or { panic(err) }
		write_live_state_blob(work_root, source_path, bin.canonical_json(post_source))
		source_evidence := transition.object_value('universal_evidence') or {
			panic('source evidence missing')
		}
		source_evidence_path := (transition.object_value('evidence_path') or {
			panic('source evidence path missing')
		}).string_value
		source_evidence_body := bin.canonical_json(source_evidence)
		if source_evidence_path in source_evidence_by_path {
			assert source_evidence_by_path[source_evidence_path] == source_evidence_body
		} else {
			source_evidence_by_path[source_evidence_path] = source_evidence_body
			write_live_state_blob(work_root, source_evidence_path, source_evidence_body)
		}
		operations := target_root.object_value('applied_operations') or {
			panic('target operations missing')
		}
		target_operations := operations.array_value[operations.array_value.len - 3..]
		role_ordinals := if (target_root.object_value('target_id') or { panic('target ID missing') }).string_value == 'freebsd-amd64' {
			[0, freebsd_business_ordinal, freebsd_business_ordinal + 1]
		} else {
			[1, linux_business_ordinal, linux_business_ordinal + 1]
		}
		for index, operation in target_operations {
			path, body := live_atomic_target_evidence(target_root, operation, proof, handoff, [
				'smoke',
				'business',
				'completion',
			][index], role_ordinals[index])
			write_live_state_blob(work_root, path, body)
		}
	}
	write_live_state_blob(work_root, 'targets/linux-amd64.json', linux_target)
	write_live_state_blob(work_root, 'targets/freebsd-amd64.json', freebsd_target)
	for command in [
		'git -C ${os.quoted_path(work_root)} add -A -- targets sources evidence',
		'git -C ${os.quoted_path(work_root)} commit -qm atomic-target-k2',
	] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	target_commit :=
		os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	assert os.execute('git -C ${os.quoted_path(work_root)} commit --allow-empty -qm atomic-head-k2').exit_code == 0
	head := os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	clone :=
		os.execute('git clone -q --bare --no-local ${os.quoted_path(work_root)} ${os.quoted_path(bare_root)}')
	assert clone.exit_code == 0, clone.output
	proof := live_state_proof_set(os.real_path(bare_root), head, [target_commit])
	freebsd_root := bin.parse_strict_json(freebsd_target) or { panic(err) }
	linux_root := bin.parse_strict_json(linux_target) or { panic(err) }
	freebsd_handoffs := freebsd_root.object_value('recovery_handoffs') or {
		panic('freebsd handoffs missing')
	}
	linux_handoffs := linux_root.object_value('recovery_handoffs') or {
		panic('linux handoffs missing')
	}
	handoff_ids := [
		(freebsd_handoffs.array_value[1].object_value('handoff_id') or {
			panic('freebsd handoff ID missing')
		}).string_value,
		(linux_handoffs.array_value[1].object_value('handoff_id') or {
			panic('linux handoff ID missing')
		}).string_value,
	]
	os.rmdir_all(work_root) or {}
	return LiveMultiAtomicStateRepository{
		root:        os.real_path(bare_root)
		parent:      parent
		target:      target_commit
		head:        head
		proof:       proof
		handoff_ids: handoff_ids
	}
}

fn prepare_live_source_atomic_state(suffix string,
	options LiveAtomicStateOptions) LiveAtomicStateRepository {
	work_root := os.join_path(os.temp_dir(), 'tccbin-live-atomic-${os.getpid()}-${suffix}')
	bare_root := '${work_root}.git'
	os.rmdir_all(work_root) or {}
	os.rmdir_all(bare_root) or {}
	os.mkdir_all(os.join_path(work_root, 'targets')) or { panic(err) }
	os.mkdir_all(os.join_path(work_root, 'sources')) or { panic(err) }
	bootstrap := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'target-state.bootstrap.schema-fixture.json')) or { panic(err) }
	for target_id in ['linux-amd64', 'freebsd-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64'] {
		write_live_state_blob(work_root, 'targets/${target_id}.json', bootstrap.replace('linux-amd64',
			target_id))
	}
	source_fixture := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'source-state.outage.schema-fixture.json')) or { panic(err) }
	write_live_state_blob(work_root, 'sources/tinycc-mob.json', source_fixture)
	write_live_state_blob(work_root, 'sources/bdwgc-master.json', source_fixture.replace('tinycc-mob',
		'bdwgc-master').replace('https://repo.or.cz/tinycc.git', 'https://github.com/ivmai/bdwgc').replace('"ref": "mob"',
		'"ref": "master"'))
	write_live_state_blob(work_root, 'sources/libatomic_ops-master.json', source_fixture.replace('tinycc-mob',
		'libatomic_ops-master').replace('https://repo.or.cz/tinycc.git',
		'https://github.com/bdwgc/libatomic_ops').replace('"ref": "mob"', '"ref": "master"'))
	for command in [
		'git -C ${os.quoted_path(work_root)} init -q',
		'git -C ${os.quoted_path(work_root)} checkout -qb tccbin-automation-state',
		'git -C ${os.quoted_path(work_root)} config user.email ci@example.invalid',
		'git -C ${os.quoted_path(work_root)} config user.name "Contract Test"',
		'git -C ${os.quoted_path(work_root)} add -- targets sources',
		'git -C ${os.quoted_path(work_root)} commit -qm bootstrap',
	] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	atomic_parent_target := if options.with_infrastructure_retry {
		live_recovery_h2_retry_dispatched_source_for('publish_post')
	} else {
		live_recovery_h2_dispatched_source_for('publish_post')
	}
	provisional_target := live_recovery_h2_source_waiting_variant_with_parent_and_cas_for('publish_post',
		options.with_infrastructure_retry, false, 'abababababababababababababababababababab',
		options.cas_attempt)
	provisional_root := bin.parse_strict_json(provisional_target) or { panic(err) }
	provisional_handoffs := provisional_root.object_value('recovery_handoffs') or {
		panic('provisional handoffs missing')
	}
	provisional_proof := provisional_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('provisional source proof missing')
	}
	provisional_operations := provisional_root.object_value('applied_operations') or {
		panic('provisional operations missing')
	}
	provisional_smoke_operation := provisional_operations.array_value[provisional_operations.array_value.len - 3]
	parent_target_value := bin.parse_strict_json(atomic_parent_target) or { panic(err) }
	provisional_smoke_operation_id := live_atomic_target_operation_id(parent_target_value,
		provisional_root, provisional_proof, provisional_handoffs.array_value[1],
		provisional_smoke_operation, 'smoke', 0)
	parent_source := provisional_proof.object_value('source_state_pre_snapshot') or {
		panic('provisional source pre-state missing')
	}
	write_live_state_blob(work_root, 'targets/linux-amd64.json', atomic_parent_target)
	write_live_state_blob(work_root, 'sources/tinycc-mob.json', bin.canonical_json(parent_source))
	mut atomic_parent_add_paths := 'targets/linux-amd64.json sources/tinycc-mob.json'
	if options.evicted_source_replay {
		parent_target_id := (parent_target_value.object_value('target_id') or {
			panic('parent target ID missing')
		}).string_value
		input_fingerprint := (parent_target_value.object_value('input_fingerprint') or {
			panic('parent input fingerprint missing')
		}).string_value
		artifact_fingerprint := (parent_target_value.object_value('artifact_fingerprint') or {
			panic('parent artifact fingerprint missing')
		}).string_value
		parent_generation := (parent_target_value.object_value('generation') or {
			panic('parent generation missing')
		}).int_value
		evicted_path := bin.evidence_path(2026, 7, 2999, 1, parent_target_id,
			provisional_smoke_operation_id, parent_generation, 'evicted_target_operation',
			input_fingerprint) or { panic(err) }
		evicted_body := '{"schema_version":1,"operation_id":"${provisional_smoke_operation_id}","operation_ordinal":0,"cas_attempt":1,"run_id":2999,"run_attempt":1,"intent_id":null,"transition":"evicted_target_operation","workflow":".github/workflows/tccbin_revalidate.yml","workflow_ref":"master","workflow_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","subject_id":"${parent_target_id}","subject_fingerprint":"${input_fingerprint}","target_id":"${parent_target_id}","input_fingerprint":"${input_fingerprint}","artifact_fingerprint":"${artifact_fingerprint}","generation_read":${parent_generation - 1},"generation_written":${parent_generation},"result":"blocked","digests":[{"path":"targets/${parent_target_id}.json","sha256":"${bin.json_sha256(parent_target_value)}"}]}'
		write_live_state_blob(work_root, evicted_path, evicted_body)
		atomic_parent_add_paths += ' evidence'
	}
	for command in [
		'git -C ${os.quoted_path(work_root)} add -- ${atomic_parent_add_paths}',
		'git -C ${os.quoted_path(work_root)} commit -qm atomic-parent',
	] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	mut parent :=
		os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	expected_parent := if options.wrong_expected_parent {
		'cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd'
	} else {
		parent
	}
	mut target_source := live_recovery_h2_source_waiting_variant_with_parent_and_cas_for('publish_post',
		options.with_infrastructure_retry, false, expected_parent, options.cas_attempt)
	target_source = live_atomic_rebind_for_ordinals(atomic_parent_target, target_source, 0, 2, 3)
	roles := ['smoke', 'business', 'completion']
	ordinals := [0, 2, 3]
	mut mutation_role := ''
	mut mutation_field := ''
	if options.target_evidence_mutation != '' {
		mutation_parts := options.target_evidence_mutation.split(':')
		if mutation_parts.len != 2 || mutation_parts[0] !in roles {
			panic('target evidence mutation must be role:field')
		}
		mutation_role = mutation_parts[0]
		mutation_field = mutation_parts[1]
		role_index := roles.index(mutation_role)
		preliminary_root := bin.parse_strict_json(target_source) or { panic(err) }
		preliminary_handoffs := preliminary_root.object_value('recovery_handoffs') or {
			panic('preliminary handoffs missing')
		}
		preliminary_handoff := preliminary_handoffs.array_value[1]
		preliminary_proof := preliminary_handoff.object_value('terminal_revalidation') or {
			panic('preliminary terminal proof missing')
		}
		preliminary_operations := preliminary_root.object_value('applied_operations') or {
			panic('preliminary operations missing')
		}
		preliminary_target_operations := preliminary_operations.array_value[preliminary_operations.array_value.len - 3..]
		_, preliminary_body := live_atomic_target_evidence(preliminary_root,
			preliminary_target_operations[role_index], preliminary_proof, preliminary_handoff,
			mutation_role, ordinals[role_index])
		preliminary_operation_id := (preliminary_target_operations[role_index].object_value('operation_id') or {
			panic('preliminary operation ID missing')
		}).string_value
		_, preliminary_mutated_body := live_mutated_target_evidence(preliminary_body,
			mutation_field, preliminary_operation_id, live_target_evidence_timestamp(preliminary_proof,
			preliminary_handoff, mutation_role))
		preliminary_mutated_evidence := bin.parse_strict_json(preliminary_mutated_body) or {
			panic(err)
		}
		foreign_operation_id := live_foreign_target_operation_id(parent_target_value,
			preliminary_proof, preliminary_handoff, preliminary_mutated_evidence, mutation_role,
			mutation_field)
		mut coordinated_operation_ids := []string{cap: 3}
		for operation in preliminary_target_operations {
			coordinated_operation_ids << (operation.object_value('operation_id') or {
				panic('preliminary operation ID missing')
			}).string_value
		}
		coordinated_operation_ids[role_index] = foreign_operation_id
		target_source = live_atomic_rebind_target_operation_ids(target_source,
			coordinated_operation_ids[0], coordinated_operation_ids[1],
			coordinated_operation_ids[2])
	}
	target_root := bin.parse_strict_json(target_source) or { panic(err) }
	target_handoffs := target_root.object_value('recovery_handoffs') or {
		panic('target handoffs missing')
	}
	target_proof := target_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('target source proof missing')
	}
	post_source := target_proof.object_value('source_state_snapshot') or {
		panic('target source post-state missing')
	}
	history := target_proof.object_value('source_state_cas_history') or {
		panic('target source history missing')
	}
	transition := history.array_value[0]
	source_evidence := transition.object_value('universal_evidence') or {
		panic('target source universal evidence missing')
	}
	source_evidence_path := (transition.object_value('evidence_path') or {
		panic('target source evidence path missing')
	}).string_value
	operations := target_root.object_value('applied_operations') or {
		panic('target operations missing')
	}
	target_operations := operations.array_value[operations.array_value.len - 3..]
	mut target_evidences := [][]string{}
	for index, operation in target_operations {
		path, body := live_atomic_target_evidence(target_root, operation, target_proof,
			target_handoffs.array_value[1], roles[index], ordinals[index])
		if roles[index] == mutation_role {
			operation_id := (operation.object_value('operation_id') or {
				panic('coordinated operation ID missing')
			}).string_value
			mutated_path, mutated_body := live_mutated_target_evidence(body, mutation_field,
				operation_id, live_target_evidence_timestamp(target_proof,
				target_handoffs.array_value[1], roles[index]))
			target_evidences << [mutated_path, mutated_body]
		} else {
			target_evidences << [path, body]
		}
	}
	write_live_state_blob(work_root, 'sources/tinycc-mob.json', bin.canonical_json(post_source))
	write_live_state_blob(work_root, source_evidence_path, bin.canonical_json(source_evidence))
	if options.split_source_target {
		for command in [
			'git -C ${os.quoted_path(work_root)} add -- sources evidence',
			'git -C ${os.quoted_path(work_root)} commit -qm split-source',
		] {
			result := os.execute(command)
			assert result.exit_code == 0, result.output
		}
	}
	write_live_state_blob(work_root, 'targets/linux-amd64.json', target_source)
	mut target_evidence_limit := options.evidence_count - 1
	if target_evidence_limit < 0 || target_evidence_limit > 3 {
		target_evidence_limit = 3
	}
	for index in 0 .. target_evidence_limit {
		write_live_state_blob(work_root, target_evidences[index][0], target_evidences[index][1])
	}
	if options.evidence_count == 5 {
		extra_operation := bin.parse_strict_json('{"operation_id":"fefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefe","transition":"unexpected_atomic_operation","resulting_generation":13}') or {
			panic(err)
		}
		path, body := live_atomic_target_evidence(target_root, extra_operation, target_proof,
			target_handoffs.array_value[1], 'business', 9)
		write_live_state_blob(work_root, path, body)
	}
	for command in [
		'git -C ${os.quoted_path(work_root)} add -A -- targets sources evidence',
		'git -C ${os.quoted_path(work_root)} commit -qm atomic-target',
	] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	atomic_target_commit :=
		os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	mut head := atomic_target_commit
	if options.descendant {
		assert os.execute('git -C ${os.quoted_path(work_root)} commit --allow-empty -qm descendant').exit_code == 0
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	if options.modify_terminal_handoff_at_h {
		mutated_handoff := target_source.replace_once('"terminal_completed_at":"2026-08-03T02:01:02Z"',
			'"terminal_completed_at":"2026-08-03T02:01:03Z"')
		assert mutated_handoff != target_source
		write_live_state_blob(work_root, 'targets/linux-amd64.json', mutated_handoff)
		assert os.execute('git -C ${os.quoted_path(work_root)} add -- targets/linux-amd64.json').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm terminal-handoff-mutated').exit_code == 0
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	if options.modify_evidence_at_h {
		modified := bin.canonical_json(source_evidence).replace_once('"result":"blocked"',
			'"result":"failed"')
		write_live_state_blob(work_root, source_evidence_path, modified)
		assert os.execute('git -C ${os.quoted_path(work_root)} add -- evidence').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm evidence-mutated').exit_code == 0
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	if options.delete_evidence_at_h || options.reintroduce_evidence_at_h {
		os.rm(os.join_path(work_root, source_evidence_path)) or { panic(err) }
		assert os.execute('git -C ${os.quoted_path(work_root)} add -A -- evidence').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm evidence-deleted').exit_code == 0
		if options.reintroduce_evidence_at_h {
			write_live_state_blob(work_root, source_evidence_path,
				bin.canonical_json(source_evidence))
			assert os.execute('git -C ${os.quoted_path(work_root)} add -- evidence').exit_code == 0
			assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm evidence-reintroduced').exit_code == 0
		}
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	if options.coordinated_delete_at_h {
		write_live_state_blob(work_root, 'targets/linux-amd64.json', atomic_parent_target)
		write_live_state_blob(work_root, 'sources/tinycc-mob.json',
			bin.canonical_json(parent_source))
		for evidence in target_evidences {
			os.rm(os.join_path(work_root, evidence[0])) or {}
		}
		os.rm(os.join_path(work_root, source_evidence_path)) or {}
		assert os.execute('git -C ${os.quoted_path(work_root)} add -A -- targets sources evidence').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm coordinated-terminal-delete').exit_code == 0
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	if options.orphan_business_evidence_at_h {
		orphan_generation := (target_root.object_value('generation') or {
			panic('target generation missing')
		}).int_value
		orphan_operation := bin.parse_strict_json('{"operation_id":"edededededededededededededededededededededededededededededededed","transition":"source_unreachable_orphan","resulting_generation":${orphan_generation}}') or {
			panic(err)
		}
		orphan_path, orphan_body := live_atomic_target_evidence(target_root, orphan_operation,
			target_proof, target_handoffs.array_value[1], 'business', 99)
		write_live_state_blob(work_root, orphan_path, orphan_body)
		assert os.execute('git -C ${os.quoted_path(work_root)} add -- evidence').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm orphan-business-evidence').exit_code == 0
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	if options.non_first_parent_evidence_at_h {
		// The merge pivot keeps P's tree as its first-parent state while hiding T behind the
		// second parent. H itself is deliberately mono-parent so only the full raw-parent scan can
		// reject the repository.
		assert os.execute('git -C ${os.quoted_path(work_root)} checkout -qb history-pivot ${parent}').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} merge -q --no-ff -s ours tccbin-automation-state -m hidden-terminal-pivot').exit_code == 0
		assert os.execute('git -C ${os.quoted_path(work_root)} commit --allow-empty -qm pivot-descendant').exit_code == 0
		head = os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	}
	clone :=
		os.execute('git clone -q --bare --no-local ${os.quoted_path(work_root)} ${os.quoted_path(bare_root)}')
	assert clone.exit_code == 0, clone.output
	proof := if options.non_first_parent_evidence_at_h {
		live_state_proof(os.real_path(bare_root), head)
	} else if head == atomic_target_commit {
		live_state_proof(os.real_path(bare_root), head)
	} else {
		live_state_proof_set(os.real_path(bare_root), head, [atomic_target_commit])
	}
	os.rmdir_all(work_root) or {}
	return LiveAtomicStateRepository{
		root:   os.real_path(bare_root)
		parent: parent
		target: atomic_target_commit
		head:   head
		proof:  proof
	}
}

fn prepare_live_state(suffix string, target_source string) (string, string) {
	return prepare_live_state_inventory(suffix, target_source, '', '')
}

fn prepare_live_state_variant(suffix string, target_source string,
	removed_relative_path string) (string, string) {
	return prepare_live_state_inventory(suffix, target_source, removed_relative_path, '')
}

fn prepare_live_state_with_secondary(suffix string, target_source string,
	secondary_target_source string) (string, string) {
	return prepare_live_state_inventory(suffix, target_source, '', secondary_target_source)
}

fn prepare_live_state_inventory(suffix string, target_source string, removed_relative_path string,
	secondary_target_source string) (string, string) {
	root := os.join_path(os.temp_dir(), 'tccbin-live-state-${os.getpid()}-${suffix}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'targets')) or { panic(err) }
	bootstrap := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'target-state.bootstrap.schema-fixture.json')) or { panic(err) }
	os.write_file(os.join_path(root, 'targets', 'linux-amd64.json'), bootstrap) or { panic(err) }
	for target_id in ['freebsd-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64', 'windows-amd64'] {
		os.write_file(os.join_path(root, 'targets', '${target_id}.json'), bootstrap.replace('linux-amd64',
			target_id)) or { panic(err) }
	}
	os.mkdir_all(os.join_path(root, 'sources')) or { panic(err) }
	source := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'source-state.outage.schema-fixture.json')) or { panic(err) }
	os.write_file(os.join_path(root, 'sources', 'tinycc-mob.json'), source) or { panic(err) }
	os.write_file(os.join_path(root, 'sources', 'bdwgc-master.json'), source.replace('tinycc-mob',
		'bdwgc-master').replace('https://repo.or.cz/tinycc.git', 'https://github.com/ivmai/bdwgc').replace('"ref": "mob"',
		'"ref": "master"')) or { panic(err) }
	os.write_file(os.join_path(root, 'sources', 'libatomic_ops-master.json'), source.replace('tinycc-mob',
		'libatomic_ops-master').replace('https://repo.or.cz/tinycc.git',
		'https://github.com/bdwgc/libatomic_ops').replace('"ref": "mob"', '"ref": "master"')) or {
		panic(err)
	}
	bootstrap_commands := [
		'git -C ${os.quoted_path(root)} init -q',
		'git -C ${os.quoted_path(root)} checkout -qb tccbin-automation-state',
		'git -C ${os.quoted_path(root)} config user.email ci@example.invalid',
		'git -C ${os.quoted_path(root)} config user.name "Contract Test"',
		'git -C ${os.quoted_path(root)} add -- targets sources',
		'git -C ${os.quoted_path(root)} commit -qm state',
	]
	for command in bootstrap_commands {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	os.write_file(os.join_path(root, 'targets', 'linux-amd64.json'), target_source) or {
		panic(err)
	}
	if secondary_target_source != '' {
		os.write_file(os.join_path(root, 'targets', 'freebsd-amd64.json'), secondary_target_source) or {
			panic(err)
		}
	}
	if removed_relative_path != '' {
		assert removed_relative_path in ['targets/freebsd-amd64.json', 'sources/bdwgc-master.json']
		os.rm(os.join_path(root, removed_relative_path)) or { panic(err) }
	}
	for command in ['git -C ${os.quoted_path(root)} add -A -- targets sources',
		'git -C ${os.quoted_path(root)} commit -qm state-ready'] {
		result := os.execute(command)
		assert result.exit_code == 0, result.output
	}
	head := os.execute('git -C ${os.quoted_path(root)} rev-parse HEAD')
	assert head.exit_code == 0
	bare_root := '${root}.git'
	os.rmdir_all(bare_root) or {}
	clone :=
		os.execute('git clone -q --bare --no-local ${os.quoted_path(root)} ${os.quoted_path(bare_root)}')
	assert clone.exit_code == 0, clone.output
	os.rmdir_all(root) or {}
	return os.real_path(bare_root), head.output.trim_space()
}

fn live_state_proof(root string, head string) string {
	return live_state_proof_bundle(root, head, [])
}

fn live_state_proof_for(root string, commit string, remote_head string) string {
	tree := os.execute('git --git-dir ${os.quoted_path(root)} rev-parse ${commit}^{tree}')
	line := os.execute('git --git-dir ${os.quoted_path(root)} rev-list --parents -n 1 ${commit}')
	assert tree.exit_code == 0
	assert line.exit_code == 0
	parts := line.output.trim_space().split(' ')
	parents := if parts.len > 1 { parts[1..].map('"${it}"').join(',') } else { '' }
	return '{"schema_version":1,"repository":"vlang/v","ref":"refs/heads/tccbin-automation-state","commit_sha":"${commit}","remote_head":"${remote_head}","tree_sha":"${tree.output.trim_space()}","parent_shas":[${parents}],"verification_verified":true,"verification_reason":"valid","verified_at":"2026-08-02T00:00:00Z","state_writer_app_id":1234,"actor_login":"state-writer[bot]","actor_node_id":"BOT_state_writer","actor_database_id":5678,"actor_type":"Bot"}'
}

fn live_state_proof_set(root string, head string, historical []string) string {
	return live_state_proof_bundle(root, head, historical)
}

fn live_state_proof_bundle(root string, head string, historical []string) string {
	mut sorted_historical := historical.clone()
	sorted_historical.sort()
	bundle_key := if sorted_historical.len == 0 {
		'head-only'
	} else {
		bin.json_sha256(bin.parse_strict_json('[${sorted_historical.map('"${it}"').join(',')}]') or {
			panic(err)
		})
	}
	bundle := os.join_path(root, 'live-proof-bundle-${head}-${bundle_key}')
	os.rmdir_all(bundle) or {}
	os.mkdir_all(os.join_path(bundle, 'historical')) or { panic(err) }
	os.write_file(os.join_path(bundle, 'head.json'), live_state_proof_for(root, head, head)) or {
		panic(err)
	}
	for commit in sorted_historical {
		os.write_file(os.join_path(bundle, 'historical', '${commit}.json'), live_state_proof_for(root,
			commit, head)) or { panic(err) }
	}
	return os.real_path(bundle)
}

fn live_state_mutated_proof_bundle(root string, head string, historical []string, suffix string,
	proof_commit string, old string, replacement string) string {
	bundle := os.join_path(root, 'live-proof-bundle-mutated-${suffix}')
	os.rmdir_all(bundle) or {}
	os.mkdir_all(os.join_path(bundle, 'historical')) or { panic(err) }
	mut head_source := live_state_proof_for(root, head, head)
	if proof_commit == head {
		head_source = head_source.replace_once(old, replacement)
	}
	os.write_file(os.join_path(bundle, 'head.json'), head_source) or { panic(err) }
	for commit in historical {
		mut source := live_state_proof_for(root, commit, head)
		if proof_commit == commit {
			source = source.replace_once(old, replacement)
		}
		os.write_file(os.join_path(bundle, 'historical', '${commit}.json'), source) or {
			panic(err)
		}
	}
	return os.real_path(bundle)
}

fn live_state_trust() bin.LiveStateTrust {
	return bin.LiveStateTrust{
		repository:          'vlang/v'
		state_writer_app_id: 1234
		actor_login:         'state-writer[bot]'
		actor_node_id:       'BOT_state_writer'
		actor_database_id:   5678
	}
}

fn pending_receiver_request() bin.ReceiverRequestFacts {
	return bin.ReceiverRequestFacts{
		opaque_id:                 receiver_consumer_id
		repository:                'vlang/v'
		workflow_id:               1001
		workflow_path:             '.github/workflows/update_tccbin.yml'
		workflow_ref:              'master'
		event:                     'workflow_dispatch'
		observed_canonical_head:   'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		observed_subject_ref_head: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	}
}

fn selected_receiver_request() bin.ReceiverRequestFacts {
	return bin.ReceiverRequestFacts{
		opaque_id:                 receiver_handoff_id
		repository:                'vlang/v'
		workflow_id:               1002
		workflow_path:             '.github/workflows/tccbin_revalidate.yml'
		workflow_ref:              'master'
		event:                     'workflow_dispatch'
		current_run_id:            9001
		current_run_attempt:       1
		current_head_sha:          'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
		current_run_name:          'tccbin-recovery-${receiver_handoff_id}'
		observed_canonical_head:   'cccccccccccccccccccccccccccccccccccccccc'
		observed_subject_ref_head: 'cccccccccccccccccccccccccccccccccccccccc'
	}
}

fn companion_receiver_request() bin.ReceiverRequestFacts {
	return bin.ReceiverRequestFacts{
		...pending_receiver_request()
		opaque_id: receiver_companion_handoff_id
	}
}

fn request_is_rejected(source string, request bin.ReceiverRequestFacts) bool {
	bin.resolve_receiver_request(source, request) or { return true }
	return false
}

fn receiver_replace_nth(source string, needle string, replacement string, ordinal int) string {
	if needle == '' || ordinal < 0 {
		panic('receiver mutation selector is invalid')
	}
	mut offset := 0
	for current in 0 .. ordinal + 1 {
		relative := source[offset..].index(needle) or {
			panic('receiver mutation selector missing')
		}
		position := offset + relative
		if current == ordinal {
			return source[..position] + replacement + source[position + needle.len..]
		}
		offset = position + needle.len
	}
	panic('receiver mutation ordinal is unreachable')
}

fn receiver_completion_is_rejected(source string) bool {
	bin.resolve_receiver_completion(source, receiver_completion_source()) or { return true }
	return false
}

fn active_recovery_is_rejected(source string, handoff_id string) bool {
	bin.resolve_active_recovery_id(source, handoff_id) or { return true }
	return false
}

fn assert_selected_handoff_rejected_everywhere(source string) {
	assert request_is_rejected(source, selected_receiver_request())
	assert receiver_completion_is_rejected(source)
	assert active_recovery_is_rejected(source, receiver_handoff_id)
}

fn test_receiver_resolves_opaque_id_and_derives_nonpublishing_action() {
	assert bin.deterministic_handoff_id('vlang/v:tccbin-automation-state',
		receiver_recovery_operation_id, receiver_consumer_id,
		'a3df03e05747472e2cc68ebf831503018a77a8a5dd7e2c575f0fd810ef3786f3', 0) == receiver_companion_handoff_id
	ledger := bin.parse_receiver_state_ledger(receiver_ledger_source()) or { panic(err) }
	assert ledger.targets.len == 2
	assert ledger.entries.len == 3
	pending := bin.resolve_receiver_request(receiver_ledger_source(), pending_receiver_request()) or {
		panic(err)
	}
	assert pending.target_id == 'linux-amd64'
	assert pending.resume_capability == 'native_gate'
	assert !pending.allowed_to_execute
	assert !pending.publish_allowed
	companion := bin.resolve_receiver_request(receiver_ledger_source(),
		companion_receiver_request()) or { panic(err) }
	assert companion.target_id == 'linux-amd64'
	assert companion.resume_capability == 'native_gate'
	assert !companion.allowed_to_execute
	assert !companion.publish_allowed
	selected := bin.resolve_receiver_request(receiver_ledger_source(), selected_receiver_request()) or {
		panic(err)
	}
	assert selected.target_id == 'windows-amd64'
	assert selected.resume_capability == 'v_smoke'
	assert selected.allowed_to_execute
	assert !selected.publish_allowed
}

fn test_receiver_rejects_unknown_stale_duplicate_or_user_selected_authority() {
	base := pending_receiver_request()
	requests := [
		bin.ReceiverRequestFacts{
			...base
			opaque_id: 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		},
		bin.ReceiverRequestFacts{
			...base
			repository: 'GGRei/v'
		},
		bin.ReceiverRequestFacts{
			...base
			workflow_id: 1002
		},
		bin.ReceiverRequestFacts{
			...base
			workflow_path: '.github/workflows/tccbin_revalidate.yml'
		},
		bin.ReceiverRequestFacts{
			...base
			workflow_ref: 'feature'
		},
		bin.ReceiverRequestFacts{
			...base
			event: 'schedule'
		},
		bin.ReceiverRequestFacts{
			...base
			requested_publish: true
		},
		bin.ReceiverRequestFacts{
			...base
			current_run_id: 1
		},
	]
	for request in requests {
		assert request_is_rejected(receiver_ledger_source(), request)
	}
	duplicate := receiver_ledger_source().replace_once('"id": "7777777777777777777777777777777777777777777777777777777777777777",\n      "record_type":',
		'"id": "1111111111111111111111111111111111111111111111111111111111111111",\n      "record_type":')
	assert request_is_rejected(duplicate, base)
}

fn test_receiver_rejects_every_stale_durable_binding() {
	source := receiver_ledger_source()
	request := pending_receiver_request()
	mutations := [
		source.replace_once('"generation": 5', '"generation": 6'),
		source.replace_once('"expected_ledger_generation": 5', '"expected_ledger_generation": 3'),
		source.replace_once('"canonical_head": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"canonical_head": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
		source.replace_once('"input_fingerprint": "3333333333333333333333333333333333333333333333333333333333333333"',
			'"input_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"artifact_fingerprint": "4444444444444444444444444444444444444444444444444444444444444444"',
			'"artifact_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"manifest_hash": "5555555555555555555555555555555555555555555555555555555555555555"',
			'"manifest_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"active_consumer_id": "1111111111111111111111111111111111111111111111111111111111111111"',
			'"active_consumer_id": null'),
		source.replace_once('"active_recovery_handoff_id": "${receiver_companion_handoff_id}"',
			'"active_recovery_handoff_id": "${receiver_handoff_id}"'),
		source.replace_once('"active_subject_hash": "a3df03e05747472e2cc68ebf831503018a77a8a5dd7e2c575f0fd810ef3786f3"',
			'"active_subject_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"subject_hash": "a3df03e05747472e2cc68ebf831503018a77a8a5dd7e2c575f0fd810ef3786f3"',
			'"subject_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"expected_ledger_generation": 5', '"expected_ledger_generation": 6'),
		source.replace_once('"expected_canonical_head": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"expected_canonical_head": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
		source.replace_once('"subject_ref_head": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"subject_ref_head": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
	]
	for mutation in mutations {
		assert request_is_rejected(mutation, request)
	}
}

fn test_receiver_consumer_and_recovery_handoff_lifecycle_is_asymmetric_and_exact() {
	without_recovery := receiver_ledger_source().replace_once('"active_recovery_handoff_id": "${receiver_companion_handoff_id}"',
		'"active_recovery_handoff_id": null')
	consumer := bin.resolve_receiver_request(without_recovery, pending_receiver_request()) or {
		panic(err)
	}
	assert !consumer.allowed_to_execute
	assert request_is_rejected(without_recovery, companion_receiver_request())
	assert active_recovery_is_rejected(without_recovery, receiver_companion_handoff_id)

	without_consumer := receiver_ledger_source().replace_once('"active_consumer_id": "${receiver_consumer_id}"',
		'"active_consumer_id": null')
	assert request_is_rejected(without_consumer, pending_receiver_request())
	assert request_is_rejected(without_consumer, companion_receiver_request())

	wrong_companion := receiver_ledger_source().replace_once('"id": "${receiver_companion_handoff_id}"',
		'"id": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"')
	assert request_is_rejected(wrong_companion, pending_receiver_request())
}

fn test_request_completion_and_active_id_join_every_handoff_tuple_member() {
	source := receiver_ledger_source()
	mutations := [
		source.replace_once('"generation": 8', '"generation": 9'),
		source.replace_once('"canonical_head": "cccccccccccccccccccccccccccccccccccccccc"',
			'"canonical_head": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
		source.replace_once('"input_fingerprint": "8888888888888888888888888888888888888888888888888888888888888888"',
			'"input_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		receiver_replace_nth(source,
			'"input_fingerprint": "8888888888888888888888888888888888888888888888888888888888888888"',
			'"input_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"', 1),
		source.replace_once('"artifact_fingerprint": "9999999999999999999999999999999999999999999999999999999999999999"',
			'"artifact_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		receiver_replace_nth(source,
			'"artifact_fingerprint": "9999999999999999999999999999999999999999999999999999999999999999"',
			'"artifact_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"', 1),
		source.replace_once('"manifest_hash": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"manifest_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		receiver_replace_nth(source,
			'"manifest_hash": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"manifest_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"', 1),
		source.replace_once('"active_consumer_id": "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"',
			'"active_consumer_id": null'),
		source.replace_once('"active_recovery_handoff_id": "${receiver_handoff_id}"',
			'"active_recovery_handoff_id": "${receiver_companion_handoff_id}"'),
		source.replace_once('"active_subject_hash": "9afdb6dd7a6fc43af42dadaade4808c92689c0c7e45468c45f5a0946f123f466"',
			'"active_subject_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"expected_ledger_generation": 8', '"expected_ledger_generation": 9'),
		source.replace_once('"expected_canonical_head": "cccccccccccccccccccccccccccccccccccccccc"',
			'"expected_canonical_head": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
		source.replace_once('"subject_ref_head": "cccccccccccccccccccccccccccccccccccccccc"',
			'"subject_ref_head": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
		source.replace_once('"subject_hash": "9afdb6dd7a6fc43af42dadaade4808c92689c0c7e45468c45f5a0946f123f466"',
			'"subject_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"sha": "cccccccccccccccccccccccccccccccccccccccc"',
			'"sha": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"'),
	]
	for mutation in mutations {
		assert_selected_handoff_rejected_everywhere(mutation)
	}
}

fn test_pending_receiver_cannot_execute_or_publish_before_durable_ack() {
	base := pending_receiver_request()
	pre_ack_attempt := bin.ReceiverRequestFacts{
		...base
		current_run_id:      9000
		current_run_attempt: 1
		current_head_sha:    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		current_run_name:    'tccbin-gate-${receiver_consumer_id}'
	}
	assert request_is_rejected(receiver_ledger_source(), pre_ack_attempt)
	assert request_is_rejected(receiver_ledger_source(), bin.ReceiverRequestFacts{
		...base
		requested_publish: true
	})
}

fn test_dispatched_receiver_requires_exact_ack_selected_run() {
	base := selected_receiver_request()
	requests := [
		bin.ReceiverRequestFacts{
			...base
			current_run_id: 9002
		},
		bin.ReceiverRequestFacts{
			...base
			current_run_attempt: 2
		},
		bin.ReceiverRequestFacts{
			...base
			current_head_sha: 'ffffffffffffffffffffffffffffffffffffffff'
		},
		bin.ReceiverRequestFacts{
			...base
			current_run_name: 'untrusted'
		},
	]
	for request in requests {
		assert request_is_rejected(receiver_ledger_source(), request)
	}
}

fn test_workflow_run_completion_reauthenticates_every_selected_binding() {
	resolved := bin.resolve_receiver_completion(receiver_ledger_source(),
		receiver_completion_source()) or { panic(err) }
	assert resolved.handoff_id == receiver_handoff_id
	assert resolved.target_id == 'windows-amd64'
	assert resolved.receiver_conclusion == 'success'
	assert !resolved.may_create_successor
	mutations := [
		receiver_completion_source().replace_once('"full_name": "vlang/v"',
			'"full_name": "GGRei/v"'),
		receiver_completion_source().replace_once('"id": 9001', '"id": 9002'),
		receiver_completion_source().replace_once('"run_attempt": 1', '"run_attempt": 2'),
		receiver_completion_source().replace_once('"workflow_id": 1002', '"workflow_id": 1001'),
		receiver_completion_source().replace_once('"path": ".github/workflows/tccbin_revalidate.yml"',
			'"path": ".github/workflows/update_tccbin.yml"'),
		receiver_completion_source().replace_once('"head_branch": "master"',
			'"head_branch": "feature"'),
		receiver_completion_source().replace_once('"event": "workflow_dispatch"', '"event": "push"'),
		receiver_completion_source().replace_once('"head_sha": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"',
			'"head_sha": "ffffffffffffffffffffffffffffffffffffffff"'),
		receiver_completion_source().replace_once('"display_title": "tccbin-recovery-',
			'"display_title": "untrusted-'),
	]
	for mutation in mutations {
		mut rejected := false
		bin.resolve_receiver_completion(receiver_ledger_source(), mutation) or { rejected = true }
		assert rejected
	}
}

fn test_workflow_run_lookup_treats_unrelated_completions_as_inactive() {
	for run_name, conclusion in {
		'Update tccbin':                   'neutral'
		'tccbin-recovery-scheduled-sweep': 'skipped'
	} {
		event := receiver_completion_source().replace_once('"display_title": "tccbin-recovery-${receiver_handoff_id}"',
			'"display_title": "${run_name}"').replace_once('"conclusion": "success"',
			'"conclusion": "${conclusion}"')
		lookup := bin.lookup_receiver_completion(receiver_ledger_source(), event) or { panic(err) }
		assert !lookup.active
		mut strict_rejected := false
		bin.resolve_receiver_completion(receiver_ledger_source(), event) or {
			strict_rejected = true
		}
		assert strict_rejected
	}
}

fn test_workflow_run_lookup_keeps_active_bindings_fail_closed() {
	for event in [
		receiver_completion_source().replace_once('"full_name": "vlang/v"',
			'"full_name": "GGRei/v"'),
		receiver_completion_source().replace_once('"conclusion": "success"',
			'"conclusion": "neutral"'),
	] {
		mut rejected := false
		bin.lookup_receiver_completion(receiver_ledger_source(), event) or { rejected = true }
		assert rejected
	}
}

fn test_source_recovery_resolves_only_the_current_active_handoff() {
	companion := bin.resolve_active_recovery_id(receiver_ledger_source(),
		receiver_companion_handoff_id) or { panic(err) }
	assert companion.target_id == 'linux-amd64'
	assert companion.intent_or_operation_id == receiver_consumer_id
	entry := bin.resolve_active_recovery_id(receiver_ledger_source(), receiver_handoff_id) or {
		panic(err)
	}
	assert entry.target_id == 'windows-amd64'
	mut rejected := false
	bin.resolve_active_recovery_id(receiver_ledger_source(), receiver_consumer_id) or {
		rejected = true
	}
	assert rejected
	rejected = false
	stale := receiver_ledger_source().replace_once('"active_recovery_handoff_id": "7777777777777777777777777777777777777777777777777777777777777777"',
		'"active_recovery_handoff_id": null')
	bin.resolve_active_recovery_id(stale, receiver_handoff_id) or { rejected = true }
	assert rejected
}

fn assert_live_atomic_repository_status(repository LiveAtomicStateRepository, expected string) {
	defer {
		os.rmdir_all(repository.root) or {}
	}
	inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
		live_state_trust(), repository.proof, live_handoff_id) or { panic(err) }
	assert inspection.status == expected
}

fn test_live_source_atomic_parent_target_and_descendant_are_authenticated() {
	at_target := prepare_live_source_atomic_state('at-target', LiveAtomicStateOptions{})
	assert_live_atomic_repository_status(at_target, 'dark_no_op')
	descendant := prepare_live_source_atomic_state('descendant', LiveAtomicStateOptions{
		descendant: true
	})
	assert_live_atomic_repository_status(descendant, 'dark_no_op')
}

fn test_live_source_atomic_accepts_one_consistent_second_cas_attempt_for_all_3k_plus_s_evidence() {
	repository := prepare_live_source_atomic_state('cas-attempt-2', LiveAtomicStateOptions{
		cas_attempt: 2
	})
	assert_live_atomic_repository_status(repository, 'dark_no_op')
}

fn test_live_source_atomic_rejects_a_mixed_cas_attempt_group() {
	repository := prepare_live_source_atomic_state('cas-attempt-mixed', LiveAtomicStateOptions{
		target_evidence_mutation: 'business:cas_attempt'
	})
	assert_live_atomic_repository_status(repository, 'unknown_blocked')
}

fn test_live_source_atomic_rejects_terminal_handoff_drift_after_t() {
	repository := prepare_live_source_atomic_state('terminal-handoff-drift', LiveAtomicStateOptions{
		modify_terminal_handoff_at_h: true
	})
	assert_live_atomic_repository_status(repository, 'unknown_blocked')
}

fn test_live_source_atomic_rejects_split_cardinality_and_modified_evidence() {
	for index, options in [
		LiveAtomicStateOptions{
			split_source_target: true
		},
		LiveAtomicStateOptions{
			evidence_count: 3
		},
		LiveAtomicStateOptions{
			evidence_count: 5
		},
		LiveAtomicStateOptions{
			wrong_expected_parent: true
		},
	] {
		repository := prepare_live_source_atomic_state('negative-${index}', options)
		assert_live_atomic_repository_status(repository, 'unknown_blocked')
	}
	modified := prepare_live_source_atomic_state('negative-modified-evidence', LiveAtomicStateOptions{
		modify_evidence_at_h: true
	})
	assert_live_atomic_repository_status(modified, 'history_recovery_required')
	for index, options in [
		LiveAtomicStateOptions{
			delete_evidence_at_h: true
		},
		LiveAtomicStateOptions{
			reintroduce_evidence_at_h: true
		},
		LiveAtomicStateOptions{
			coordinated_delete_at_h: true
		},
	] {
		repository := prepare_live_source_atomic_state('negative-history-${index}', options)
		assert_live_atomic_repository_status(repository, 'history_recovery_required')
	}
	orphan := prepare_live_source_atomic_state('negative-orphan-evidence', LiveAtomicStateOptions{
		orphan_business_evidence_at_h: true
	})
	assert_live_atomic_repository_status(orphan, 'corrupt_blocked')
	non_first_parent := prepare_live_source_atomic_state('negative-non-first-parent', LiveAtomicStateOptions{
		non_first_parent_evidence_at_h: true
	})
	assert_live_atomic_repository_status(non_first_parent, 'history_recovery_required')
}

fn test_live_source_atomic_rejects_wrong_public_proof_tuple() {
	repository := prepare_live_source_atomic_state('proof-tuple', LiveAtomicStateOptions{
		descendant: true
	})
	defer {
		os.rmdir_all(repository.root) or {}
	}
	mutations := [
		['"verification_verified":true', '"verification_verified":false'],
		['"state_writer_app_id":1234', '"state_writer_app_id":9999'],
		['"tree_sha":"', '"tree_sha":"0000000000000000000000000000000000000000'],
		['"remote_head":"${repository.head}"', '"remote_head":"${repository.parent}"'],
	]
	for index, mutation in mutations {
		proof := live_state_mutated_proof_bundle(repository.root, repository.head, [
			repository.target,
		], 'tuple-${index}', repository.head, mutation[0], mutation[1])
		inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
			live_state_trust(), proof, live_handoff_id) or { panic(err) }
		assert inspection.status == 'corrupt_blocked'
	}
	for proof in [live_state_proof(repository.root, repository.head),
		live_state_proof_set(repository.root, repository.head, [repository.parent])] {
		inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
			live_state_trust(), proof, live_handoff_id) or { panic(err) }
		assert inspection.status == 'history_recovery_required'
	}
	wrong_t := live_state_mutated_proof_bundle(repository.root, repository.head, [
		repository.target,
	], 'wrong-historical-t', repository.target, '"tree_sha":"',
		'"tree_sha":"0000000000000000000000000000000000000000')
	wrong_t_inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
		live_state_trust(), wrong_t, live_handoff_id) or { panic(err) }
	assert wrong_t_inspection.status == 'history_recovery_required'
}

fn test_live_source_atomic_rejects_missing_surplus_symlink_and_oversized_proof_bundle_objects() {
	repository := prepare_live_source_atomic_state('proof-bundle-negatives', LiveAtomicStateOptions{
		descendant: true
	})
	defer {
		os.rmdir_all(repository.root) or {}
	}
	missing := live_state_proof(repository.root, repository.head)
	missing_inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
		live_state_trust(), missing, live_handoff_id) or { panic(err) }
	assert missing_inspection.status == 'history_recovery_required'
	surplus := live_state_proof_set(repository.root, repository.head,
		[repository.target, repository.parent])
	surplus_inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
		live_state_trust(), surplus, live_handoff_id) or { panic(err) }
	assert surplus_inspection.status == 'history_recovery_required'
	valid := live_state_proof_set(repository.root, repository.head, [repository.target])
	symlink_bundle := os.join_path(repository.root, 'proof-bundle-symlink')
	os.symlink(valid, symlink_bundle) or { panic(err) }
	symlink_bundle_inspection := bin.inspect_live_receiver_state(automation_root(),
		repository.root, live_state_trust(), symlink_bundle, live_handoff_id) or { panic(err) }
	assert symlink_bundle_inspection.status == 'corrupt_blocked'
	symlink_file_bundle := os.join_path(repository.root, 'proof-file-symlink')
	os.mkdir_all(os.join_path(symlink_file_bundle, 'historical')) or { panic(err) }
	os.symlink(os.join_path(valid, 'head.json'), os.join_path(symlink_file_bundle, 'head.json')) or {
		panic(err)
	}
	symlink_file_inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
		live_state_trust(), os.real_path(symlink_file_bundle), live_handoff_id) or { panic(err) }
	assert symlink_file_inspection.status == 'corrupt_blocked'
	oversized := live_state_proof_set(repository.root, repository.head, [repository.target])
	os.write_file(os.join_path(oversized, 'historical', '${repository.target}.json'), 'x'.repeat(
		16 * 1024 + 1)) or { panic(err) }
	oversized_inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
		live_state_trust(), oversized, live_handoff_id) or { panic(err) }
	assert oversized_inspection.status == 'history_recovery_required'
}

fn test_live_source_atomic_rejects_rehashed_foreign_universal_target_evidence_metadata() {
	fields := ['run_id', 'run_attempt', 'operation_ordinal', 'cas_attempt', 'intent_id', 'workflow',
		'workflow_ref', 'workflow_sha', 'subject_id', 'target_id', 'subject_fingerprint',
		'input_fingerprint', 'artifact_fingerprint', 'generation', 'transition', 'result', 'digest']
	roles := ['smoke', 'business', 'completion']
	for role_index, role in roles {
		for field_index, field in fields {
			repository := prepare_live_source_atomic_state('foreign-evidence-${role_index}-${field_index}', LiveAtomicStateOptions{
				target_evidence_mutation: '${role}:${field}'
			})
			expected_status := if field == 'workflow_ref'
				|| (role == 'business' && field in ['target_id', 'transition']) {
				// evidence.schema.json intentionally fixes this field to master, so the closed
				// inventory rejects it before the contextual target comparison. A foreign
				// business target_id or transition likewise breaks the closed handoff/evidence
				// lookup key.
				'corrupt_blocked'
			} else {
				'unknown_blocked'
			}
			assert_live_atomic_repository_status(repository, expected_status)
		}
	}
}

fn test_live_source_atomic_derives_each_target_evidence_month_from_its_role_timestamp() {
	for role in ['smoke', 'business', 'completion'] {
		repository := prepare_live_source_atomic_state('foreign-evidence-month-${role}', LiveAtomicStateOptions{
			target_evidence_mutation: '${role}:path_month'
		})
		assert_live_atomic_repository_status(repository, 'unknown_blocked')
	}
}

fn test_live_source_atomic_rejects_replay_of_an_operation_evicted_from_the_source_window() {
	repository := prepare_live_source_atomic_state('evicted-source-replay', LiveAtomicStateOptions{
		evicted_source_replay: true
	})
	// The closed HEAD evidence index rejects the replayed operation ID before contextual CAS
	// validation; no P/T/proof copy is stale in this fixture.
	assert_live_atomic_repository_status(repository, 'corrupt_blocked')
}

fn live_atomic_group_source_signature(source_operation_id string, resolution_operation_id string,
	waiting_consumers []string) string {
	consumer_json := waiting_consumers.map('"${it}"').join(',')
	transition := bin.parse_strict_json('{"operation_id":"${source_operation_id}","source_id":"tinycc-mob","resolution_operation_id":"${resolution_operation_id}","expected_state_parent_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}') or {
		panic(err)
	}
	refetch := bin.parse_strict_json('{"source_state_id":"tinycc-mob","source_state_generation":5,"resolution_operation_id":"${resolution_operation_id}","source_id":"tinycc","source_repository":"https://repo.or.cz/tinycc.git","requested_ref":"mob","previous_sha":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","resolved_sha":null,"resolved_tree":null,"status":"unreachable","failure_kind":"timeout","checked_at":"2026-08-03T00:00:00Z"}') or {
		panic(err)
	}
	pre_snapshot := bin.parse_strict_json('{"generation":4,"source_id":"tinycc-mob","waiting_consumers":[],"operation_count":4,"operation_chain_digest":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"}') or {
		panic(err)
	}
	post_snapshot := bin.parse_strict_json('{"generation":5,"source_id":"tinycc-mob","waiting_consumers":[${consumer_json}],"operation_count":5,"operation_chain_digest":"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"}') or {
		panic(err)
	}
	return bin.live_shared_source_contract_signature(transition, refetch, pre_snapshot,
		post_snapshot) or { panic(err) }
}

fn test_live_atomic_group_contract_accepts_k2_s2_and_k2_s1_exact_unions() {
	target_a := ['aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
		'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
		'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc']
	target_b := ['dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd',
		'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee',
		'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff']
	source_a := '1111111111111111111111111111111111111111111111111111111111111111'
	source_b := '2222222222222222222222222222222222222222222222222222222222222222'
	mut k2_s1_evidence_operation_ids := target_a.clone()
	k2_s1_evidence_operation_ids << target_b
	k2_s1_evidence_operation_ids << source_a
	mut k2_s2_evidence_operation_ids := k2_s1_evidence_operation_ids.clone()
	k2_s2_evidence_operation_ids << source_b
	resolution_a := '3333333333333333333333333333333333333333333333333333333333333333'
	resolution_b := '4444444444444444444444444444444444444444444444444444444444444444'
	consumer_a := '5555555555555555555555555555555555555555555555555555555555555555'
	consumer_b := '6666666666666666666666666666666666666666666666666666666666666666'
	signature_a := live_atomic_group_source_signature(source_a, resolution_a, [
		consumer_a,
	])
	signature_b := live_atomic_group_source_signature(source_b, resolution_b, [
		consumer_b,
	])
	k2_s2 := [
		bin.LiveAtomicGroupContractFact{
			target_id:                     'linux-amd64'
			source_operation_id:           source_a
			source_signature:              signature_a
			target_evidence_operation_ids: target_a
		},
		bin.LiveAtomicGroupContractFact{
			target_id:                     'freebsd-amd64'
			source_operation_id:           source_b
			source_signature:              signature_b
			target_evidence_operation_ids: target_b
		},
	]
	bin.validate_live_atomic_group_contract(k2_s2, k2_s2_evidence_operation_ids) or { panic(err) }
	shared_signature := live_atomic_group_source_signature(source_a, resolution_a, [
		consumer_a,
		consumer_b,
	])
	k2_s1 := [
		bin.LiveAtomicGroupContractFact{
			target_id:                     'linux-amd64'
			source_operation_id:           source_a
			source_signature:              shared_signature
			target_evidence_operation_ids: target_a
		},
		bin.LiveAtomicGroupContractFact{
			target_id:                     'freebsd-amd64'
			source_operation_id:           source_a
			source_signature:              shared_signature
			target_evidence_operation_ids: target_b
		},
	]
	bin.validate_live_atomic_group_contract(k2_s1, k2_s1_evidence_operation_ids) or { panic(err) }
	mut partial_share_rejected := false
	different_resolution_signature := live_atomic_group_source_signature(source_a, resolution_b, [
		consumer_a,
		consumer_b,
	])
	bin.validate_live_atomic_group_contract([k2_s1[0], bin.LiveAtomicGroupContractFact{
		target_id:                     'freebsd-amd64'
		source_operation_id:           source_a
		source_signature:              different_resolution_signature
		target_evidence_operation_ids: target_b
	}], k2_s1_evidence_operation_ids) or { partial_share_rejected = true }
	assert partial_share_rejected
	mut different_consumer_set_rejected := false
	bin.validate_live_atomic_group_contract([k2_s1[0], bin.LiveAtomicGroupContractFact{
		target_id:                     'freebsd-amd64'
		source_operation_id:           source_a
		source_signature:              signature_a
		target_evidence_operation_ids: target_b
	}], k2_s1_evidence_operation_ids) or { different_consumer_set_rejected = true }
	assert different_consumer_set_rejected
}

fn test_live_source_atomic_reader_accepts_real_k2_s2_and_k2_s1_repositories() {
	for shared_source in [false, true] {
		repository := prepare_live_multi_source_atomic_state(if shared_source {
			'k2-s1'
		} else {
			'k2-s2'
		}, shared_source)
		for handoff_id in repository.handoff_ids {
			inspection := bin.inspect_live_receiver_state(automation_root(), repository.root,
				live_state_trust(), repository.proof, handoff_id) or { panic(err) }
			assert inspection.status == 'dark_no_op'
		}
		os.rmdir_all(repository.root) or {}
	}
}

fn test_live_evidence_first_parent_history_bound_is_exact() {
	assert bin.live_evidence_history_count_is_within_bound(1)
	assert bin.live_evidence_history_count_is_within_bound(100_000)
	assert !bin.live_evidence_history_count_is_within_bound(0)
	assert !bin.live_evidence_history_count_is_within_bound(100_001)
}

fn test_live_source_atomic_rejects_shallow_history() {
	repository := prepare_live_source_atomic_state('shallow-source', LiveAtomicStateOptions{
		descendant: true
	})
	defer {
		os.rmdir_all(repository.root) or {}
	}
	shallow := '${repository.root}-shallow.git'
	os.rmdir_all(shallow) or {}
	defer {
		os.rmdir_all(shallow) or {}
	}
	clone :=
		os.execute('git clone -q --bare --depth 1 --branch tccbin-automation-state file://${repository.root} ${os.quoted_path(shallow)}')
	assert clone.exit_code == 0, clone.output
	inspection := bin.inspect_live_receiver_state(automation_root(), os.real_path(shallow),
		live_state_trust(), repository.proof, live_handoff_id) or { panic(err) }
	assert inspection.status == 'history_recovery_required'
}

fn test_live_source_atomic_rejects_replace_graft_alternate_and_environment_redirects() {
	replace_repository := prepare_live_source_atomic_state('replace-ref', LiveAtomicStateOptions{})
	replace_result :=
		os.execute('git --git-dir ${os.quoted_path(replace_repository.root)} update-ref refs/replace/${replace_repository.head} ${replace_repository.parent}')
	assert replace_result.exit_code == 0, replace_result.output
	replace_inspection := bin.inspect_live_receiver_state(automation_root(),
		replace_repository.root, live_state_trust(), replace_repository.proof, live_handoff_id) or {
		panic(err)
	}
	assert replace_inspection.status == 'corrupt_blocked'
	os.rmdir_all(replace_repository.root) or {}

	graft_repository := prepare_live_source_atomic_state('graft-file', LiveAtomicStateOptions{})
	os.write_file(os.join_path(graft_repository.root, 'info', 'grafts'),
		'${graft_repository.head} ${graft_repository.parent}\n') or { panic(err) }
	graft_inspection := bin.inspect_live_receiver_state(automation_root(), graft_repository.root,
		live_state_trust(), graft_repository.proof, live_handoff_id) or { panic(err) }
	assert graft_inspection.status == 'corrupt_blocked'
	os.rmdir_all(graft_repository.root) or {}

	alternate_repository :=
		prepare_live_source_atomic_state('alternate-file', LiveAtomicStateOptions{})
	os.write_file(os.join_path(alternate_repository.root, 'objects', 'info', 'alternates'),
		'/tmp/forbidden-live-state-object-store\n') or { panic(err) }
	alternate_inspection := bin.inspect_live_receiver_state(automation_root(),
		alternate_repository.root, live_state_trust(), alternate_repository.proof, live_handoff_id) or {
		panic(err)
	}
	assert alternate_inspection.status == 'corrupt_blocked'
	os.rmdir_all(alternate_repository.root) or {}

	common_repository :=
		prepare_live_source_atomic_state('common-dir-file', LiveAtomicStateOptions{})
	os.write_file(os.join_path(common_repository.root, 'commondir'),
		'/tmp/forbidden-live-state-common-dir\n') or { panic(err) }
	common_inspection := bin.inspect_live_receiver_state(automation_root(), common_repository.root,
		live_state_trust(), common_repository.proof, live_handoff_id) or { panic(err) }
	assert common_inspection.status == 'corrupt_blocked'
	os.rmdir_all(common_repository.root) or {}

	environment_repository :=
		prepare_live_source_atomic_state('redirecting-environment', LiveAtomicStateOptions{})
	for environment_name in [
		'GIT_DIR',
		'GIT_WORK_TREE',
		'GIT_COMMON_DIR',
		'GIT_OBJECT_DIRECTORY',
		'GIT_ALTERNATE_OBJECT_DIRECTORIES',
		'GIT_REPLACE_REF_BASE',
		'GIT_GRAFT_FILE',
		'GIT_SHALLOW_FILE',
		'GIT_NAMESPACE',
		'GIT_INDEX_FILE',
		'GIT_EXEC_PATH',
		'GIT_CONFIG_PARAMETERS',
		'GIT_CONFIG_COUNT',
		'GIT_CONFIG_SYSTEM',
		'GIT_CONFIG_GLOBAL',
		'GIT_CONFIG_KEY_0',
		'GIT_CONFIG_VALUE_0',
	] {
		os.setenv(environment_name, '/tmp/forbidden-live-state-redirect', true)
		environment_inspection := bin.inspect_live_receiver_state(automation_root(),
			environment_repository.root, live_state_trust(), environment_repository.proof,
			live_handoff_id) or { panic(err) }
		os.unsetenv(environment_name)
		assert environment_inspection.status == 'corrupt_blocked', environment_name
	}
	os.rmdir_all(environment_repository.root) or {}
}

fn test_live_state_reader_distinguishes_absent_current_and_stale_consumer() {
	absent := bin.inspect_live_receiver_state(automation_root(), os.join_path(os.temp_dir(),
		'tccbin-state-does-not-exist-${os.getpid()}'), live_state_trust(), '', live_handoff_id) or {
		panic(err)
	}
	assert absent.status == 'uninitialized'
	root, head := prepare_live_state('current', live_target_source(false))
	defer {
		os.rmdir_all(root) or {}
	}
	proof := live_state_proof(root, head)
	inspection := bin.inspect_live_receiver_state(automation_root(), root, live_state_trust(),
		proof, live_handoff_id) or { panic(err) }
	assert inspection.status == 'active'
	assert inspection.target.target_id == 'linux-amd64'
	assert inspection.canonical_ref == 'thirdparty-linux-amd64'
	assert inspection.subject_ref.starts_with('tccbin-candidate/linux-amd64/')
	request := bin.ReceiverRequestFacts{
		opaque_id:                 live_handoff_id
		repository:                'vlang/v'
		workflow_id:               1001
		workflow_path:             '.github/workflows/update_tccbin.yml'
		workflow_ref:              'master'
		event:                     'workflow_dispatch'
		observed_canonical_head:   'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		observed_subject_ref_head: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	}
	current := bin.resolve_live_receiver_request(automation_root(), root, live_state_trust(),
		proof, request) or { panic(err) }
	assert current.status == 'active'
	assert !current.resolution.allowed_to_execute
	assert !current.resolution.publish_allowed
	stale := bin.resolve_live_receiver_request(automation_root(), root, live_state_trust(), proof, bin.ReceiverRequestFacts{
		...request
		observed_canonical_head: 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
	}) or { panic(err) }
	assert stale.status == 'dark_no_op'
	wrong_event := bin.resolve_live_receiver_request(automation_root(), root, live_state_trust(),
		proof, bin.ReceiverRequestFacts{
		...request
		repository: 'GGRei/v'
	}) or { panic(err) }
	assert wrong_event.status == 'dark_no_op'
	pre_ack := bin.resolve_live_receiver_request(automation_root(), root, live_state_trust(),
		proof, bin.ReceiverRequestFacts{
		...request
		current_run_id: 1
	}) or { panic(err) }
	assert pre_ack.status == 'dark_no_op'
	mut authority_rejected := false
	bin.resolve_live_receiver_request(automation_root(), root, live_state_trust(), proof, bin.ReceiverRequestFacts{
		...request
		requested_publish: true
	}) or { authority_rejected = true }
	assert authority_rejected
	stale_consumer := bin.inspect_live_receiver_state(automation_root(), root, live_state_trust(),
		proof, receiver_consumer_id) or { panic(err) }
	assert stale_consumer.status == 'dark_no_op'
}

fn test_live_state_reader_fails_closed_on_corrupt_duplicate_symlink_or_wrong_checkout() {
	corrupt_root, corrupt_head := prepare_live_state('corrupt', live_target_source(false).replace_once('"schema_version": 1',
		'"schema_version": 2'))
	defer {
		os.rmdir_all(corrupt_root) or {}
	}
	corrupt := bin.inspect_live_receiver_state(automation_root(), corrupt_root, live_state_trust(), live_state_proof(corrupt_root,
		corrupt_head), live_handoff_id) or { panic(err) }
	assert corrupt.status == 'corrupt_blocked'
	duplicate_root, duplicate_head := prepare_live_state('duplicate', live_target_source(true))
	defer {
		os.rmdir_all(duplicate_root) or {}
	}
	duplicate := bin.inspect_live_receiver_state(automation_root(), duplicate_root,
		live_state_trust(), live_state_proof(duplicate_root, duplicate_head), live_handoff_id) or {
		panic(err)
	}
	assert duplicate.status == 'corrupt_blocked'
	nonbare_root := os.join_path(os.temp_dir(), 'tccbin-live-state-${os.getpid()}-nonbare')
	os.rmdir_all(nonbare_root) or {}
	os.mkdir_all(nonbare_root) or { panic(err) }
	nonbare := bin.inspect_live_receiver_state(automation_root(), os.real_path(nonbare_root),
		live_state_trust(), live_state_proof(corrupt_root, corrupt_head), live_handoff_id) or {
		panic(err)
	}
	assert nonbare.status == 'corrupt_blocked'
	os.rmdir_all(nonbare_root) or {}
	base_bare, base_head := prepare_live_state('symlink-base', live_target_source(false))
	work_root := os.join_path(os.temp_dir(), 'tccbin-live-state-${os.getpid()}-symlink-work')
	symlink_bare := '${work_root}.git'
	os.rmdir_all(work_root) or {}
	os.rmdir_all(symlink_bare) or {}
	assert os.execute('git clone -q ${os.quoted_path(base_bare)} ${os.quoted_path(work_root)}').exit_code == 0
	assert os.execute('git -C ${os.quoted_path(work_root)} config user.email ci@example.invalid').exit_code == 0
	assert os.execute('git -C ${os.quoted_path(work_root)} config user.name "Contract Test"').exit_code == 0
	os.rm(os.join_path(work_root, 'targets', 'linux-amd64.json')) or { panic(err) }
	os.symlink('../sources/tinycc-mob.json', os.join_path(work_root, 'targets', 'linux-amd64.json')) or {
		panic(err)
	}
	assert os.execute('git -C ${os.quoted_path(work_root)} add -- targets/linux-amd64.json').exit_code == 0
	assert os.execute('git -C ${os.quoted_path(work_root)} commit -qm symlink').exit_code == 0
	symlink_head :=
		os.execute('git -C ${os.quoted_path(work_root)} rev-parse HEAD').output.trim_space()
	assert os.execute('git clone -q --bare --no-local ${os.quoted_path(work_root)} ${os.quoted_path(symlink_bare)}').exit_code == 0
	symlinked := bin.inspect_live_receiver_state(automation_root(), os.real_path(symlink_bare),
		live_state_trust(), live_state_proof(os.real_path(symlink_bare), symlink_head),
		live_handoff_id) or { panic(err) }
	assert symlinked.status == 'corrupt_blocked'
	os.rmdir_all(base_bare) or {}
	os.rmdir_all(work_root) or {}
	os.rmdir_all(symlink_bare) or {}
	assert base_head.len == 40
}

fn test_live_state_proof_commit_cannot_select_its_parent_inventory() {
	root, head := prepare_live_state('commit-binding', live_target_source(false))
	defer {
		os.rmdir_all(root) or {}
	}
	parent_result := os.execute('git --git-dir ${os.quoted_path(root)} rev-parse ${head}^')
	assert parent_result.exit_code == 0
	parent := parent_result.output.trim_space()
	assert parent.len == 40
	assert os.execute('git --git-dir ${os.quoted_path(root)} rev-parse ${parent}^{tree}').output.trim_space() != os.execute('git --git-dir ${os.quoted_path(root)} rev-parse ${head}^{tree}').output.trim_space()
	mismatched_proof := live_state_mutated_proof_bundle(root, head, [], 'commit-binding', head,
		'"commit_sha":"${head}"', '"commit_sha":"${parent}"')
	inspection := bin.inspect_live_receiver_state(automation_root(), root, live_state_trust(),
		mismatched_proof, live_handoff_id) or { panic(err) }
	assert inspection.status == 'corrupt_blocked'
}

fn test_live_native_resolver_derives_only_create_only_actions_from_durable_state() {
	absent := bin.resolve_live_native_gate_action(automation_root(), '', bin.LiveStateTrust{}, '',
		receiver_consumer_id) or { panic(err) }
	assert absent.action == 'dark_no_op'
	assert !absent.publish_allowed
	cases := [
		['pre-subject', live_pre_subject_adoption_source(), 'candidate_ref_create'],
		['original-push', live_native_adoption_source(false), 'dark_no_op'],
		['gate-trigger', live_native_adoption_source(true), 'gate_trigger_ref_create'],
	]
	for case in cases {
		root, head := prepare_live_state('native-${case[0]}', case[1])
		decision := bin.resolve_live_native_gate_action(automation_root(), root,
			live_state_trust(), live_state_proof(root, head), receiver_consumer_id) or {
			panic(err)
		}
		assert decision.action == case[2]
		assert !decision.publish_allowed
		if decision.action.ends_with('_ref_create') {
			assert decision.create_only
			assert decision.subject_sha == 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		}
		if decision.action == 'gate_trigger_ref_create' {
			assert decision.expected_ref == 'tccbin-gate-trigger/linux-amd64/${receiver_consumer_id}/${decision.trigger_id}'
		}
		os.rmdir_all(root) or {}
	}
}

fn test_live_native_resolver_blocks_hash_generation_ref_owner_and_stage_divergence() {
	base := live_native_adoption_source(true)
	mutations := [
		base.replace_once('"active_subject_hash": "d92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd"',
			'"active_subject_hash": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		base.replace_once('"expected_ledger_generation": 1', '"expected_ledger_generation": 2'),
		base.replace_once('"expected_ref": "tccbin-gate-trigger/linux-amd64/',
			'"expected_ref": "tccbin-gate-trigger/windows-amd64/'),
		base.replace_once('"intent_id": "${receiver_consumer_id}"',
			'"intent_id": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		live_pre_subject_adoption_source().replace_once('"stage": "intent_reserved"',
			'"stage": "candidate_bound"'),
	]
	for index, mutation in mutations {
		root, head := prepare_live_state('native-corrupt-${index}', mutation)
		decision := bin.resolve_live_native_gate_action(automation_root(), root,
			live_state_trust(), live_state_proof(root, head), receiver_consumer_id) or {
			panic(err)
		}
		assert decision.action == 'corrupt_blocked'
		os.rmdir_all(root) or {}
	}
}

fn test_live_native_resolver_blocks_duplicate_consumer_across_targets() {
	linux := live_pre_subject_adoption_source()
	freebsd := linux.replace('linux-amd64', 'freebsd-amd64')
	root, head := prepare_live_state_with_secondary('native-duplicate', linux, freebsd)
	decision := bin.resolve_live_native_gate_action(automation_root(), root, live_state_trust(),
		live_state_proof(root, head), receiver_consumer_id) or { panic(err) }
	assert decision.action == 'corrupt_blocked'
	os.rmdir_all(root) or {}
}

fn test_live_native_post_consumer_is_the_operation_and_not_the_original_intent() {
	source := live_publish_post_source()
	root, head := prepare_live_state('native-publish-post', source)
	post := bin.resolve_live_native_gate_action(automation_root(), root, live_state_trust(),
		live_state_proof(root, head), live_post_operation_id) or { panic(err) }
	assert post.action == 'dark_no_op'
	assert post.consumer_id == live_post_operation_id
	assert post.intent_or_operation_id == live_post_operation_id
	assert !post.create_only
	assert !post.publish_allowed
	old_intent := bin.resolve_live_native_gate_action(automation_root(), root, live_state_trust(),
		live_state_proof(root, head), receiver_consumer_id) or { panic(err) }
	assert old_intent.action == 'dark_no_op'
	assert old_intent.consumer_id == ''
	os.rmdir_all(root) or {}

	mutations := [
		source.replace_once('"post_validation_operation_id": "${live_post_operation_id}"',
			'"post_validation_operation_id": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"provisional_published": {"sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"provisional_published": {"sha":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"'),
		source.replace_once('"candidate_ref": "tccbin-candidate/linux-amd64/${receiver_consumer_id}"',
			'"candidate_ref": "tccbin-candidate/linux-amd64/ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
	]
	for index, mutation in mutations {
		broken_root, broken_head := prepare_live_state('native-publish-post-corrupt-${index}',
			mutation)
		decision := bin.resolve_live_native_gate_action(automation_root(), broken_root,
			live_state_trust(), live_state_proof(broken_root, broken_head), live_post_operation_id) or {
			panic(err)
		}
		assert decision.action == 'corrupt_blocked'
		os.rmdir_all(broken_root) or {}
	}
}

fn test_live_native_remediation_requires_its_operation_and_trigger_backing() {
	source := live_remediation_source()
	root, head := prepare_live_state('native-remediation', source)
	decision := bin.resolve_live_native_gate_action(automation_root(), root, live_state_trust(),
		live_state_proof(root, head), live_remediation_operation_id) or { panic(err) }
	assert decision.action == 'gate_trigger_ref_create'
	assert decision.consumer_kind == 'remediation'
	assert decision.consumer_id == live_remediation_operation_id
	assert decision.create_only
	assert !decision.publish_allowed
	assert decision.expected_ref == 'tccbin-gate-trigger/linux-amd64/${live_remediation_operation_id}/${decision.trigger_id}'
	os.rmdir_all(root) or {}

	mutations := [
		source.replace_once('"active_remediation_id": "${live_remediation_operation_id}"',
			'"active_remediation_id": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		source.replace_once('"before":"cccccccccccccccccccccccccccccccccccccccc"',
			'"before":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"'),
	]
	for index, mutation in mutations {
		broken_root, broken_head := prepare_live_state('native-remediation-corrupt-${index}',
			mutation)
		broken := bin.resolve_live_native_gate_action(automation_root(), broken_root,
			live_state_trust(), live_state_proof(broken_root, broken_head),
			live_remediation_operation_id) or { panic(err) }
		assert broken.action == 'corrupt_blocked'
		os.rmdir_all(broken_root) or {}
	}
}

fn test_live_state_present_ref_with_missing_target_or_source_is_corrupt_blocked() {
	for removed_path in ['targets/freebsd-amd64.json', 'sources/bdwgc-master.json'] {
		suffix := removed_path.replace('/', '-').replace('.json', '')
		root, head := prepare_live_state_variant('missing-${suffix}', live_target_source(false),
			removed_path)
		inspection := bin.inspect_live_receiver_state(automation_root(), root, live_state_trust(),
			live_state_proof(root, head), live_handoff_id) or { panic(err) }
		assert inspection.status == 'corrupt_blocked'
		os.rmdir_all(root) or {}
	}
}

fn test_terminal_projection_v3_and_revalidation_v5_cover_native_validation_without_changing_owner_v1() {
	source := live_recovery_h2_terminal_source()
	root := bin.parse_strict_json(source) or { panic(err) }
	current_projection := bin.terminal_state_projection(root) or { panic(err) }
	assert (current_projection.object_value('schema_version') or {
		panic('terminal projection version missing')
	}).int_value == 3
	assert (current_projection.object_value('last_native_validation') or {
		panic('terminal projection native validation missing')
	}).kind == .null_value
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	proof := handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal revalidation missing')
	}
	assert (proof.object_value('schema_version') or {
		panic('terminal revalidation version missing')
	}).int_value == 5
	final_projection := proof.object_value('final_projection') or {
		panic('terminal final projection missing')
	}
	assert (final_projection.object_value('schema_version') or {
		panic('terminal final projection version missing')
	}).int_value == 3
	canonical_final := bin.canonical_json(final_projection)
	anchor := '"last_native_validation":null'
	assert canonical_final.count(anchor) == 1
	mutated_final := bin.parse_strict_json(canonical_final.replace_once(anchor,
		'"last_native_validation":{"sentinel":true}')) or { panic(err) }
	assert bin.terminal_owner_payload_digest(final_projection) or { panic(err) } == bin.terminal_owner_payload_digest(mutated_final) or {
		panic(err)
	}
	canonical_proof := bin.canonical_json(proof)
	final_anchor := '"final_projection":${canonical_final}'
	assert canonical_proof.count(final_anchor) == 1
	mutated_proof := bin.parse_strict_json(canonical_proof.replace_once(final_anchor,
		'"final_projection":${bin.canonical_json(mutated_final)}')) or { panic(err) }
	assert bin.terminal_revalidation_facts_digest(proof) or { panic(err) } != bin.terminal_revalidation_facts_digest(mutated_proof) or {
		panic(err)
	}
}
