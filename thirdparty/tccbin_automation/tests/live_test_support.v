module tests

import os
import tccbin_automation.bin

const receiver_consumer_id = '1111111111111111111111111111111111111111111111111111111111111111'
const live_handoff_id = '9720f1f66b2319621b89e7144e35cdd011daeb223e686f0c86401dd3812f7d7d'
const live_handoff_create_operation_id = '8181818181818181818181818181818181818181818181818181818181818181'
const live_post_operation_id = '2222222222222222222222222222222222222222222222222222222222222222'
const live_remediation_operation_id = '3333333333333333333333333333333333333333333333333333333333333333'
const live_recovery_operation_id = '8888888888888888888888888888888888888888888888888888888888888888'
const live_h2_dispatch_operation_id = '9191919191919191919191919191919191919191919191919191919191919191'
const live_h2_ack_operation_id = '9292929292929292929292929292929292929292929292929292929292929292'
const live_h2_smoke_completion_operation_id = '9393939393939393939393939393939393939393939393939393939393939393'
const live_h2_business_operation_id = '9494949494949494949494949494949494949494949494949494949494949494'
const live_h2_completion_operation_id = '9595959595959595959595959595959595959595959595959595959595959595'
const live_h2_retry_dispatch_operation_id = 'a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5'
const live_h2_retry_ack_operation_id = 'a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6'
const live_h2_retry_completion_operation_id = 'a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7'
const live_h2_later_operation_id = 'b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0'

// with_pending_v_smoke keeps Phase A live-state fixtures valid under the B1 crossed reservation
// contract. It adds only the initial, undispatched smoke and the same-generation CAS projection.
fn with_pending_v_smoke(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	subject := root.object_value('native_gate_subject') or { panic('native subject missing') }
	if subject.kind == .null_value {
		return source
	}
	existing := root.object_value('v_smoke_execution') or { panic('V smoke projection missing') }
	if existing.kind != .null_value {
		return source
	}
	consumer_id := subject.object_value('consumer_id') or { panic('consumer ID missing') }
	consumer_kind := subject.object_value('consumer_kind') or { panic('consumer kind missing') }
	target_id := subject.object_value('target_id') or { panic('subject target missing') }
	subject_generation := subject.object_value('subject_generation') or {
		panic('subject generation missing')
	}
	subject_ref := subject.object_value('original_ref') or { panic('subject ref missing') }
	subject_sha := subject.object_value('sha') or { panic('subject SHA missing') }
	subject_hash := root.object_value('active_subject_hash') or { panic('subject hash missing') }
	generation := root.object_value('generation') or { panic('target generation missing') }
	reservation_id := if consumer_kind.string_value in ['publish_post', 'rollback_post',
		'remediation'] {
		consumer_id.string_value
	} else {
		'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
	}
	placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	mut smoke := '{"schema_version":1,"consumer_id":"${consumer_id.string_value}","consumer_kind":"${consumer_kind.string_value}","intent_or_operation_id":"${consumer_id.string_value}","target_id":"${target_id.string_value}","subject_hash":"${subject_hash.string_value}","subject_generation":${subject_generation.int_value},"subject_ref":"${subject_ref.string_value}","subject_sha":"${subject_sha.string_value}","v_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","repository":"vlang/v","workflow_id":2002,"workflow_path":".github/workflows/tccbin_revalidate.yml","workflow_ref":"master","event":"workflow_dispatch","actions_integration_id":1001,"validator_integration_id":1002,"run_name":"tccbin-v-smoke/${consumer_id.string_value}","reservation_operation_id":"${reservation_id}","expected_ledger_generation":${generation.int_value},"state":"pending","dispatches":[],"active_dispatch":null,"active_attempt":null,"attempts":[],"run_absent_attempts":[],"infra_retry_count":0,"ack_operation_ids":[],"completion_operation_ids":[],"block_operation_id":null,"block_facts_digest":null,"block_reason":null,"blocked_at":null,"replay_facts_digest":"${placeholder}","created_at":"2026-08-03T00:00:00Z"}'
	smoke_value := bin.parse_strict_json(smoke) or { panic(err) }
	replay_digest := bin.v_smoke_replay_facts_digest(smoke_value) or { panic(err) }
	smoke = smoke.replace_once(placeholder, replay_digest)
	mut result := source.replace_once('"v_smoke_execution": null', '"v_smoke_execution": ${smoke}')
	result = result.replace_once('"applied_operations": []',
		'"applied_operations": [{"operation_id":"${reservation_id}","transition":"${live_reservation_transition(consumer_kind.string_value)}","resulting_generation":${subject_generation.int_value}}]')
	result = result.replace_once('"last_operation_id": null',
		'"last_operation_id": "${reservation_id}"')
	result = result.replace_once('"last_transition": null',
		'"last_transition": "${live_reservation_transition(consumer_kind.string_value)}"')
	return result
}

fn live_reservation_transition(consumer_kind string) string {
	return match consumer_kind {
		'initial_adopt_current' { 'begin_bootstrap' }
		'adopt_current' { 'reserve_adopt_current' }
		'publish_candidate', 'rollback_candidate' { 'bind_candidate' }
		'publish_post' { 'promotion_confirmed' }
		'rollback_post' { 'rollback_promoted' }
		'remediation' { 'begin_remediation' }
		else { panic('unsupported live reservation consumer kind ${consumer_kind}') }
	}
}

fn live_target_source(duplicate_handoff bool) string {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	mut source := os.read_file(os.join_path(fixture_root,
		'target-state.bootstrap.schema-fixture.json')) or { panic(err) }
	intent := (os.read_file(os.join_path(fixture_root,
		'active-intent.bootstrap.schema-fixture.json')) or { panic(err) }).trim_space()
	subject := (os.read_file(os.join_path(fixture_root, 'native-gate-subject.schema-fixture.json')) or {
		panic(err)
	}).trim_space()
	execution := (os.read_file(os.join_path(fixture_root,
		'native-gate-execution.schema-fixture.json')) or { panic(err) }).trim_space()
	handoff := (os.read_file(os.join_path(fixture_root,
		'recovery-handoff.pending.schema-fixture.json')) or { panic(err) }).trim_space()
	handoff_value := bin.parse_strict_json(handoff) or { panic(err) }
	creation_commitment := bin.recovery_handoff_creation_commitment(handoff_value) or { panic(err) }
	creation_transition := 'handoff_create_${creation_commitment}'
	mut handoffs := handoff
	if duplicate_handoff {
		second := handoff.replace_once('"recovery_operation_id": "${live_recovery_operation_id}"',
			'"recovery_operation_id": "9999999999999999999999999999999999999999999999999999999999999999"')
		handoffs = '${handoff},\n${second}'
	}
	source = source.replace_once('"generation": 0', '"generation": 1')
	source = source.replace_once('"target_state": "uninitialized"', '"target_state": "validating"')
	source = source.replace_once('"input_fingerprint": null',
		'"input_fingerprint": "3333333333333333333333333333333333333333333333333333333333333333"')
	source = source.replace_once('"artifact_fingerprint": null',
		'"artifact_fingerprint": "4444444444444444444444444444444444444444444444444444444444444444"')
	source = source.replace_once('"manifest_hash": null',
		'"manifest_hash": "5555555555555555555555555555555555555555555555555555555555555555"')
	source = source.replace_once('"provenance_status": null', '"provenance_status": "complete"')
	source = source.replace_once('"resolved_inputs": null',
		'"resolved_inputs": ${live_resolved_inputs()}')
	source = source.replace_once('"active_intent": null', '"active_intent": ${intent}')
	source = source.replace_once('"native_gate_subject": null', '"native_gate_subject": ${subject}')
	source = source.replace_once('"active_subject_hash": null',
		'"active_subject_hash": "d92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd"')
	source = source.replace_once('"native_gate_execution": null',
		'"native_gate_execution": ${execution}')
	source = source.replace_once('"recovery_handoffs": []', '"recovery_handoffs": [${handoffs}]')
	source = source.replace_once('"active_recovery_handoff_id": null',
		'"active_recovery_handoff_id": "${live_handoff_id}"')
	source = source.replace_once('"generation": 1', '"generation": 2')
	source = source.replace('"expected_ledger_generation": 1', '"expected_ledger_generation": 2')
	mut result := with_pending_v_smoke(source)
	result = result.replace_once('"last_operation_id": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"',
		'"last_operation_id": "${live_handoff_create_operation_id}"')
	result = result.replace_once('"last_transition": "begin_bootstrap"',
		'"last_transition": "${creation_transition}"')
	result = result.replace_once('"applied_operations": [{"operation_id":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee","transition":"begin_bootstrap","resulting_generation":1}]',
		'"applied_operations": [{"operation_id":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee","transition":"begin_bootstrap","resulting_generation":1},{"operation_id":"${live_handoff_create_operation_id}","transition":"${creation_transition}","resulting_generation":2}]')
	return result
}

fn live_recovery_native_check_for(subject_hash string, consumer_id string, native_ref string) string {
	external_id := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		consumer_id, subject_hash, 7002, 1) or { panic(err) }
	return '{"check_name":"tccbin-candidate-gate","repository":"vlang/tccbin","integration_id":1001,"workflow_id":2001,"workflow_path":".github/workflows/build-and-test.yml","event":"push","run_id":7002,"run_attempt":1,"check_suite_id":7102,"check_suite_integration_id":1001,"job_id":7202,"subject_hash":"${subject_hash}","check_run_id":7302,"external_id":"${external_id}","run_name":"tccbin-native-gate/${consumer_id}","run_url":"https://github.com/vlang/tccbin/actions/runs/7002","job_url":"https://github.com/vlang/tccbin/actions/runs/7002/job/7202","details_url":"https://github.com/vlang/tccbin/actions/runs/7002/job/7202","ref":"${native_ref}","workflow_head_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","check_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","actor":"tccbin-publisher[bot]","actor_integration_id":5001,"triggering_actor":"tccbin-publisher[bot]","triggering_actor_integration_id":5001,"created_at":"2026-08-03T00:10:00Z","completed_at":"2026-08-03T01:00:00Z","run_conclusion":"success","check_conclusion":"success","output_digest":"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3","evidence_digest":"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4"}'
}

fn live_recovery_chain_source() string {
	return live_recovery_chain_source_for('publish_post')
}

fn live_recovery_chain_source_for(consumer_kind string) string {
	recovery_operation_id := live_recovery_operation_id
	create_operation_id := '8282828282828282828282828282828282828282828282828282828282828282'
	dispatch_operation_id := '8383838383838383838383838383838383838383838383838383838383838383'
	ack_operation_id := '8484848484848484848484848484848484848484848484848484848484848484'
	native_ack_operation_id := '8585858585858585858585858585858585858585858585858585858585858585'
	native_completion_operation_id := '8686868686868686868686868686868686868686868686868686868686868686'
	successor_operation_id := '8787878787878787878787878787878787878787878787878787878787878787'
	mut source := match consumer_kind {
		'publish_post' { live_publish_post_source() }
		'rollback_post' { live_rollback_post_source() }
		'remediation' { live_remediation_source() }
		else { panic('unsupported recovery consumer ${consumer_kind}') }
	}
	root := bin.parse_strict_json(source) or { panic(err) }
	subject := root.object_value('native_gate_subject') or { panic('native subject missing') }
	subject_hash_value := root.object_value('active_subject_hash') or {
		panic('subject hash missing')
	}
	subject_hash := subject_hash_value.string_value
	consumer_id_value := subject.object_value('consumer_id') or { panic('consumer missing') }
	consumer_id := consumer_id_value.string_value
	consumer_type := if consumer_kind == 'remediation' { 'remediation' } else { 'post-validation' }
	native_execution_before := root.object_value('native_gate_execution') or {
		panic('native execution missing')
	}
	epochs := native_execution_before.object_value('gate_epochs') or { panic('epochs missing') }
	native_ref_value := epochs.array_value[0].object_value('expected_ref') or {
		panic('expected native ref missing')
	}
	native_ref := native_ref_value.string_value
	reservation_transition := live_reservation_transition(consumer_kind)
	owner_projection := bin.terminal_state_projection(root) or { panic(err) }
	owner_digest := bin.terminal_owner_payload_digest(owner_projection) or { panic(err) }
	committed_reservation_transition := '${reservation_transition}_${owner_digest}'
	predecessor_id := bin.deterministic_handoff_id('vlang/v:tccbin-automation-state',
		recovery_operation_id, consumer_id, subject_hash, 0)
	successor_id := bin.deterministic_handoff_id('vlang/v:tccbin-automation-state',
		recovery_operation_id, consumer_id, subject_hash, 1)

	intent := root.object_value('active_intent') or { panic('active intent missing') }
	remediation_binding := root.object_value('active_remediation_binding') or {
		panic('remediation binding missing')
	}
	expected_sources := if intent.kind == .object {
		intent.object_value('expected_check_sources') or { panic('intent sources missing') }
	} else {
		remediation_binding.object_value('expected_check_sources') or {
			panic('remediation sources missing')
		}
	}
	canonical_subject := bin.canonical_json(subject)
	native_digest_placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	mut predecessor := '{"handoff_id":"${predecessor_id}","handoff_ordinal":0,"predecessor_handoff_id":null,"successor_handoff_id":"${successor_id}","audience":"vlang/v:tccbin-automation-state","recovery_operation_id":"${recovery_operation_id}","consumer_type":"${consumer_type}","resume_capability":"native_gate","intent_or_operation_id":"${consumer_id}","subject_hash":"${subject_hash}","subject_generation":1,"expected_ledger_generation":7,"expected_canonical_head":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","subject_ref_head":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","subject":${canonical_subject},"expected_check_sources":${bin.canonical_json(expected_sources)},"native_gate_evidence_digest":"${native_digest_placeholder}","native_gate_check_digest":"${native_digest_placeholder}","receiver_repository":"vlang/v","workflow_id":1001,"workflow_path":".github/workflows/update_tccbin.yml","workflow_ref":"master","event":"workflow_dispatch","receiver_run_name":"tccbin-recovery-${predecessor_id}","state":"complete","dispatch_generation":1,"dispatch_operation_ids":["${dispatch_operation_id}"],"ack_operation_id":"${ack_operation_id}","selected_run_id":9001,"selected_run_attempt":1,"receiver_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","receiver_conclusion":"success","receiver_output_digest":"8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b","deadline":"2026-08-03T01:30:00Z","terminal_outcome":"native_gate_green_successor","completion_operation_id":"${successor_operation_id}","terminal_revalidation":null}'
	mut successor := '{"handoff_id":"${successor_id}","handoff_ordinal":1,"predecessor_handoff_id":"${predecessor_id}","successor_handoff_id":null,"audience":"vlang/v:tccbin-automation-state","recovery_operation_id":"${recovery_operation_id}","consumer_type":"${consumer_type}","resume_capability":"v_smoke","intent_or_operation_id":"${consumer_id}","subject_hash":"${subject_hash}","subject_generation":1,"expected_ledger_generation":7,"expected_canonical_head":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","subject_ref_head":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","subject":${canonical_subject},"expected_check_sources":${bin.canonical_json(expected_sources)},"native_gate_evidence_digest":"${native_digest_placeholder}","native_gate_check_digest":"${native_digest_placeholder}","receiver_repository":"vlang/v","workflow_id":2002,"workflow_path":".github/workflows/tccbin_revalidate.yml","workflow_ref":"master","event":"workflow_dispatch","receiver_run_name":"tccbin-recovery-${successor_id}","state":"pending","dispatch_generation":0,"dispatch_operation_ids":[],"ack_operation_id":null,"selected_run_id":null,"selected_run_attempt":null,"receiver_master_sha":null,"receiver_conclusion":null,"receiver_output_digest":null,"deadline":null,"terminal_outcome":null,"completion_operation_id":null,"terminal_revalidation":null}'
	native_run := '{"gate_epoch":0,"run_id":7002,"run_attempt":1,"repository":"vlang/tccbin","ref":"${native_ref}","sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","event":"push","actor":"tccbin-publisher[bot]","actor_integration_id":5001,"triggering_actor":"tccbin-publisher[bot]","triggering_actor_integration_id":5001,"check_suite_id":7102,"workflow_id":2001,"workflow_path":".github/workflows/build-and-test.yml","created_at":"2026-08-03T00:10:00Z","conclusion":"success"}'

	source = source.replace_once('"generation": 1', '"generation": 7')
	source = source.replace('"expected_ledger_generation": 1', '"expected_ledger_generation": 7')
	source = source.replace_once('"expected_ledger_generation":1', '"expected_ledger_generation":7')
	source = source.replace_once('"state": "open_unselected"', '"state": "completed"')
	source = source.replace_once('"selected_run_id": null', '"selected_run_id": 7002')
	source = source.replace_once('"selected_run_attempt": null', '"selected_run_attempt": 1')
	source = source.replace_once('"selected_check_suite_id": null',
		'"selected_check_suite_id": 7102')
	source = source.replace_once('"conclusion": null', '"conclusion": "success"')
	source = source.replace_once('"closed_at": null', '"closed_at": "2026-08-03T01:00:00Z"')
	source = source.replace_once('"gate_runs": []', '"gate_runs": [${native_run}]')
	source = source.replace_once('"ack_operation_ids": []',
		'"ack_operation_ids": ["${native_ack_operation_id}"]')
	source = source.replace_once('"completion_operation_ids": []',
		'"completion_operation_ids": ["${native_completion_operation_id}"]')
	source = source.replace_once('"selected_run_id": null', '"selected_run_id": 7002')
	source = source.replace_once('"selected_run_attempt": null', '"selected_run_attempt": 1')
	source = source.replace_once('"selected_check_suite_id": null',
		'"selected_check_suite_id": 7102')
	source = source.replace_once('"selected_conclusion": null', '"selected_conclusion": "success"')
	evidence_root := bin.parse_strict_json(source) or { panic(err) }
	evidence_execution := evidence_root.object_value('native_gate_execution') or {
		panic('native evidence missing')
	}
	native_digest := bin.native_gate_evidence_digest(evidence_execution) or { panic(err) }
	native_check := live_recovery_native_check_for(subject_hash, consumer_id, native_ref)
	native_check_value := bin.parse_strict_json(native_check) or { panic(err) }
	native_check_digest := bin.native_gate_check_digest(native_check_value) or { panic(err) }
	predecessor = predecessor.replace_once(native_digest_placeholder, native_digest)
	predecessor = predecessor.replace_once(native_digest_placeholder, native_check_digest)
	successor = successor.replace_once(native_digest_placeholder, native_digest)
	successor = successor.replace_once(native_digest_placeholder, native_check_digest)
	predecessor_value := bin.parse_strict_json(predecessor) or { panic(err) }
	create_commitment := bin.recovery_handoff_creation_commitment(predecessor_value) or {
		panic(err)
	}
	successor_commitment := bin.recovery_native_successor_commitment(predecessor_value) or {
		panic(err)
	}
	source = source.replace_once('"recovery_handoffs": []',
		'"recovery_handoffs": [${predecessor},${successor}]')
	source = source.replace_once('"active_recovery_handoff_id": null',
		'"active_recovery_handoff_id": "${successor_id}"')
	source = source.replace_once('"last_operation_id": "${consumer_id}"',
		'"last_operation_id": "${successor_operation_id}"')
	source = source.replace_once('"last_transition": "${reservation_transition}"',
		'"last_transition": "native_recovery_successor_${successor_commitment}"')
	source = source.replace_once('"applied_operations": [{"operation_id":"${consumer_id}","transition":"${reservation_transition}","resulting_generation":1}]',
		'"applied_operations": [{"operation_id":"${consumer_id}","transition":"${committed_reservation_transition}","resulting_generation":1},{"operation_id":"${create_operation_id}","transition":"handoff_create_${create_commitment}","resulting_generation":2},{"operation_id":"${dispatch_operation_id}","transition":"handoff_dispatch_${predecessor_id}","resulting_generation":3},{"operation_id":"${ack_operation_id}","transition":"handoff_ack_${predecessor_id}","resulting_generation":4},{"operation_id":"${native_ack_operation_id}","transition":"native_gate_ack_${subject_hash}","resulting_generation":5},{"operation_id":"${native_completion_operation_id}","transition":"native_gate_complete_${subject_hash}","resulting_generation":6},{"operation_id":"${successor_operation_id}","transition":"native_recovery_successor_${successor_commitment}","resulting_generation":7}]')
	updated_root := bin.parse_strict_json(source) or { panic(err) }
	smoke := updated_root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	old_replay := smoke.object_value('replay_facts_digest') or { panic('replay digest missing') }
	new_replay := bin.v_smoke_replay_facts_digest(smoke) or { panic(err) }
	return source.replace_once('"replay_facts_digest":"${old_replay.string_value}"',
		'"replay_facts_digest":"${new_replay}"')
}

fn live_recovery_successor_id_for(consumer_kind string) string {
	chain := bin.parse_strict_json(live_recovery_chain_source_for(consumer_kind)) or { panic(err) }
	handoffs := chain.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('recovery successor missing')
	}
	value := handoffs.array_value[1].object_value('handoff_id') or {
		panic('recovery successor ID missing')
	}
	return value.string_value
}

fn canonical_root_member(root bin.JsonValue, key string) string {
	value := root.object_value(key) or { panic('root ${key} missing') }
	return bin.canonical_json(value)
}

fn replace_canonical_root_member(source string, root bin.JsonValue, key string,
	replacement string) string {
	marker := '"${key}":${canonical_root_member(root, key)}'
	if !source.contains(marker) {
		panic('canonical root ${key} projection missing')
	}
	return source.replace_once(marker, '"${key}":${replacement}')
}

fn live_recovery_smoke_projection_for(fixture_name string, expected_generation i64,
	consumer_kind string) string {
	fixture := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures', fixture_name)) or {
		panic(err)
	}
	root := bin.parse_strict_json(fixture) or { panic(err) }
	smoke := root.object_value('v_smoke_execution') or { panic('V smoke fixture missing') }
	old_generation := smoke.object_value('expected_ledger_generation') or {
		panic('V smoke generation missing')
	}
	old_hash := 'd92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd'
	chain_root := bin.parse_strict_json(live_recovery_chain_source_for(consumer_kind)) or {
		panic(err)
	}
	subject := chain_root.object_value('native_gate_subject') or { panic('native subject missing') }
	consumer_id_value := subject.object_value('consumer_id') or { panic('consumer ID missing') }
	consumer_id := consumer_id_value.string_value
	subject_ref_value := subject.object_value('original_ref') or { panic('subject ref missing') }
	subject_ref := subject_ref_value.string_value
	new_hash_value := chain_root.object_value('active_subject_hash') or {
		panic('subject hash missing')
	}
	new_hash := new_hash_value.string_value
	old_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		receiver_consumer_id, old_hash, 3001, 1) or { panic(err) }
	new_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		consumer_id, new_hash, 3001, 1) or { panic(err) }
	old_external_retry := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		receiver_consumer_id, old_hash, 3001, 2) or { panic(err) }
	new_external_retry := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		consumer_id, new_hash, 3001, 2) or { panic(err) }
	mut result := bin.canonical_json(smoke)
	result = result.replace(receiver_consumer_id, consumer_id)
	result = result.replace_once('"consumer_kind":"initial_adopt_current"',
		'"consumer_kind":"${consumer_kind}"')
	result = result.replace('"subject_ref":"tccbin-candidate/linux-amd64/${consumer_id}"',
		'"subject_ref":"${subject_ref}"')
	result = result.replace(old_hash, new_hash)
	result = result.replace('eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee',
		consumer_id)
	result = result.replace_once('"expected_ledger_generation":${old_generation.int_value}',
		'"expected_ledger_generation":${expected_generation}')
	result = result.replace('d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1',
		live_h2_dispatch_operation_id)
	result = result.replace('1212121212121212121212121212121212121212121212121212121212121212',
		live_h2_ack_operation_id)
	result = result.replace('3434343434343434343434343434343434343434343434343434343434343434',
		live_h2_smoke_completion_operation_id)
	return result.replace(old_external, new_external).replace(old_external_retry,
		new_external_retry)
}

fn live_recovery_rehashed_smoke_for(fixture_name string, expected_generation i64,
	consumer_kind string) string {
	base := live_recovery_chain_source_for(consumer_kind)
	root := bin.parse_strict_json(base) or { panic(err) }
	mut source := bin.canonical_json(root)
	source = replace_canonical_root_member(source, root, 'v_smoke_execution', live_recovery_smoke_projection_for(fixture_name,
		expected_generation, consumer_kind))
	refreshed := refresh_v_smoke_facts_digests(source)
	refreshed_root := bin.parse_strict_json(refreshed) or { panic(err) }
	refreshed_smoke := refreshed_root.object_value('v_smoke_execution') or {
		panic('refreshed V smoke missing')
	}
	return bin.canonical_json(refreshed_smoke)
}

fn live_recovery_successor_projection(root bin.JsonValue, state string,
	expected_generation i64, terminal_proof string) string {
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	mut successor := bin.canonical_json(handoffs.array_value[1])
	successor = successor.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":${expected_generation}')
	if expected_generation > 7 {
		successor = successor.replace_once('"dispatch_generation":0', '"dispatch_generation":1')
		successor = successor.replace_once('"dispatch_operation_ids":[]',
			'"dispatch_operation_ids":["${live_h2_dispatch_operation_id}"]')
	}
	if state in ['dispatched', 'complete'] {
		successor = successor.replace_once('"ack_operation_id":null',
			'"ack_operation_id":"${live_h2_ack_operation_id}"')
		successor = successor.replace_once('"selected_run_id":null', '"selected_run_id":3001')
		successor = successor.replace_once('"selected_run_attempt":null',
			'"selected_run_attempt":1')
		successor = successor.replace_once('"receiver_master_sha":null',
			'"receiver_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"')
		successor = successor.replace_once('"receiver_conclusion":null', '"receiver_conclusion":"${if state == 'complete' {
			'success'
		} else {
			'pending'
		}}"')
		successor = successor.replace_once('"deadline":null', '"deadline":"2026-08-03T01:31:00Z"')
	}
	if state == 'complete' {
		successor = successor.replace_once('"receiver_output_digest":null',
			'"receiver_output_digest":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"')
		successor = successor.replace_once('"terminal_outcome":null', '"terminal_outcome":"green"')
		successor = successor.replace_once('"completion_operation_id":null',
			'"completion_operation_id":"${live_h2_completion_operation_id}"')
		successor = successor.replace_once('"terminal_revalidation":null',
			'"terminal_revalidation":${terminal_proof}')
	}
	return successor.replace_once('"state":"pending"', '"state":"${state}"')
}

fn append_live_recovery_h2_operations(operations string, through_generation i64,
	successor_id string, smoke_terminal_payload_digest string, business_transition string,
	completion_facts_digest string) string {
	mut additions := ''
	if through_generation >= 8 {
		additions += ',{"operation_id":"${live_h2_dispatch_operation_id}","transition":"handoff_dispatch_${successor_id}","resulting_generation":8}'
	}
	if through_generation >= 9 {
		additions += ',{"operation_id":"${live_h2_ack_operation_id}","transition":"handoff_ack_${successor_id}","resulting_generation":9}'
	}
	if through_generation >= 10 {
		if smoke_terminal_payload_digest == '' {
			panic('terminal H2 operations require the V-smoke completion commitment')
		}
		additions += ',{"operation_id":"${live_h2_smoke_completion_operation_id}","transition":"v-smoke-complete-1_${smoke_terminal_payload_digest}","resulting_generation":10}'
	}
	if through_generation >= 11 {
		if business_transition == '' {
			panic('terminal H2 operations require their business transition')
		}
		additions += ',{"operation_id":"${live_h2_business_operation_id}","transition":"${business_transition}","resulting_generation":11}'
	}
	if through_generation >= 12 {
		if completion_facts_digest == '' {
			panic('terminal H2 operations require the immutable proof digest')
		}
		additions += ',{"operation_id":"${live_h2_completion_operation_id}","transition":"handoff_complete_${completion_facts_digest}","resulting_generation":12}'
	}
	return operations[..operations.len - 1] + additions + ']'
}

fn live_recovery_active_h2_source_for(consumer_kind string, fixture_name string, state string,
	expected_generation i64) string {
	base := live_recovery_chain_source_for(consumer_kind)
	root := bin.parse_strict_json(base) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	operations := canonical_root_member(root, 'applied_operations')
	successor_id := live_recovery_successor_id_for(consumer_kind)
	predecessor := bin.canonical_json(handoffs.array_value[0])
	successor := live_recovery_successor_projection(root, state, expected_generation, 'null')
	mut native := canonical_root_member(root, 'native_gate_execution')
	native = native.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":${expected_generation}')
	mut source := bin.canonical_json(root)
	source = replace_canonical_root_member(source, root, 'generation', expected_generation.str())
	source = replace_canonical_root_member(source, root, 'native_gate_execution', native)
	source = replace_canonical_root_member(source, root, 'v_smoke_execution', live_recovery_rehashed_smoke_for(fixture_name,
		expected_generation, consumer_kind))
	source = replace_canonical_root_member(source, root, 'recovery_handoffs',
		'[${predecessor},${successor}]')
	source = replace_canonical_root_member(source, root, 'applied_operations', append_live_recovery_h2_operations(operations,
		expected_generation, successor_id, '', '', ''))
	last_operation_id := if expected_generation == 8 {
		live_h2_dispatch_operation_id
	} else {
		live_h2_ack_operation_id
	}
	last_transition := if expected_generation == 8 {
		'handoff_dispatch_${successor_id}'
	} else {
		'handoff_ack_${successor_id}'
	}
	source = replace_canonical_root_member(source, root, 'last_operation_id',
		'"${last_operation_id}"')
	return replace_canonical_root_member(source, root, 'last_transition', '"${last_transition}"')
}

fn live_recovery_active_h2_source(fixture_name string, state string,
	expected_generation i64) string {
	return live_recovery_active_h2_source_for('publish_post', fixture_name, state,
		expected_generation)
}

fn live_recovery_h2_awaiting_ack_source() string {
	return live_recovery_active_h2_source('target-state.v-smoke-awaiting-ack.schema-fixture.json',
		'pending', 8)
}

fn live_recovery_h2_dispatched_source() string {
	return live_recovery_active_h2_source('target-state.v-smoke-dispatched.schema-fixture.json',
		'dispatched', 9)
}

fn live_recovery_h2_dispatched_source_for(consumer_kind string) string {
	return live_recovery_active_h2_source_for(consumer_kind,
		'target-state.v-smoke-dispatched.schema-fixture.json', 'dispatched', 9)
}

fn live_recovery_h2_retry_dispatched_source_for(consumer_kind string) string {
	base := live_recovery_chain_source_for(consumer_kind)
	root := bin.parse_strict_json(base) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor_id := live_recovery_successor_id_for(consumer_kind)
	mut successor := bin.canonical_json(handoffs.array_value[1])
	successor = successor.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":12')
	successor = successor.replace_once('"dispatch_generation":0', '"dispatch_generation":2')
	successor = successor.replace_once('"dispatch_operation_ids":[]',
		'"dispatch_operation_ids":["${live_h2_dispatch_operation_id}","${live_h2_retry_dispatch_operation_id}"]')
	successor = successor.replace_once('"ack_operation_id":null',
		'"ack_operation_id":"${live_h2_retry_ack_operation_id}"')
	successor = successor.replace_once('"selected_run_id":null', '"selected_run_id":3001')
	successor = successor.replace_once('"selected_run_attempt":null', '"selected_run_attempt":2')
	successor = successor.replace_once('"receiver_master_sha":null',
		'"receiver_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"')
	successor = successor.replace_once('"receiver_conclusion":null',
		'"receiver_conclusion":"pending"')
	successor = successor.replace_once('"deadline":null', '"deadline":"2026-08-03T02:31:00Z"')
	successor = successor.replace_once('"state":"pending"', '"state":"dispatched"')
	mut native := canonical_root_member(root, 'native_gate_execution')
	native = native.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":12')
	smoke_source := live_recovery_rehashed_smoke_for('target-state.v-smoke-retry-dispatched.schema-fixture.json',
		12, consumer_kind)
	smoke := bin.parse_strict_json(smoke_source) or { panic(err) }
	attempts := smoke.object_value('attempts') or { panic('retry smoke attempts missing') }
	first_smoke_digest := bin.v_smoke_terminal_payload_digest(smoke, attempts.array_value[0]) or {
		panic(err)
	}
	mut operations := canonical_root_member(root, 'applied_operations')
	operations = operations[..operations.len - 1] +
		',{"operation_id":"${live_h2_dispatch_operation_id}","transition":"handoff_dispatch_${successor_id}","resulting_generation":8}' +
		',{"operation_id":"${live_h2_ack_operation_id}","transition":"handoff_ack_${successor_id}","resulting_generation":9}' +
		',{"operation_id":"${live_h2_smoke_completion_operation_id}","transition":"v-smoke-complete-1_${first_smoke_digest}","resulting_generation":10}' +
		',{"operation_id":"${live_h2_retry_dispatch_operation_id}","transition":"handoff_dispatch_${successor_id}","resulting_generation":11}' +
		',{"operation_id":"${live_h2_retry_ack_operation_id}","transition":"handoff_ack_${successor_id}","resulting_generation":12}]'
	mut source := bin.canonical_json(root)
	source = replace_canonical_root_member(source, root, 'generation', '12')
	source = replace_canonical_root_member(source, root, 'native_gate_execution', native)
	source = replace_canonical_root_member(source, root, 'v_smoke_execution', smoke_source)
	source = replace_canonical_root_member(source, root, 'recovery_handoffs',
		'[${bin.canonical_json(handoffs.array_value[0])},${successor}]')
	source = replace_canonical_root_member(source, root, 'applied_operations', operations)
	source = replace_canonical_root_member(source, root, 'last_operation_id',
		'"${live_h2_retry_ack_operation_id}"')
	return replace_canonical_root_member(source, root, 'last_transition',
		'"handoff_ack_${successor_id}"')
}

fn live_recovery_h2_terminal_source() string {
	return live_recovery_h2_green_source_for('publish_post')
}

fn live_artifact_tuple_from_subject(subject bin.JsonValue) string {
	sha := subject.object_value('sha') or { panic('subject SHA missing') }
	tree := subject.object_value('tree') or { panic('subject tree missing') }
	input := subject.object_value('input_fingerprint') or { panic('subject input missing') }
	artifact := subject.object_value('artifact_fingerprint') or {
		panic('subject artifact missing')
	}
	manifest := subject.object_value('manifest_hash') or { panic('subject manifest missing') }
	digests := subject.object_value('digests') or { panic('subject digests missing') }
	return '{"sha":${bin.canonical_json(sha)},"tree":${bin.canonical_json(tree)},"input_fingerprint":${bin.canonical_json(input)},"artifact_fingerprint":${bin.canonical_json(artifact)},"manifest_hash":${bin.canonical_json(manifest)},"digests":${bin.canonical_json(digests)}}'
}

fn live_recovery_h2_green_source_for(consumer_kind string) string {
	base := live_recovery_chain_source_for(consumer_kind)
	root := bin.parse_strict_json(base) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	intent := root.object_value('active_intent') or { panic('active intent missing') }
	remediation_binding := root.object_value('active_remediation_binding') or {
		panic('remediation binding missing')
	}
	expected_sources := if intent.kind == .object {
		intent.object_value('expected_check_sources') or { panic('intent sources missing') }
	} else {
		remediation_binding.object_value('expected_check_sources') or {
			panic('remediation sources missing')
		}
	}
	subject := handoffs.array_value[1].object_value('subject') or { panic('subject missing') }
	subject_hash_value := handoffs.array_value[1].object_value('subject_hash') or {
		panic('subject hash missing')
	}
	subject_hash := subject_hash_value.string_value
	consumer_id_value := subject.object_value('consumer_id') or { panic('consumer ID missing') }
	consumer_id := consumer_id_value.string_value
	epochs := (root.object_value('native_gate_execution') or { panic('native execution missing') }).object_value('gate_epochs') or {
		panic('native epochs missing')
	}
	native_ref := (epochs.array_value[0].object_value('expected_ref') or {
		panic('native ref missing')
	}).string_value
	successor_id := (handoffs.array_value[1].object_value('handoff_id') or {
		panic('successor ID missing')
	}).string_value
	business_transition := match consumer_kind {
		'publish_post' { 'post_check_green' }
		'rollback_post' { 'rollback_post_green' }
		'remediation' { 'remediation_green' }
		else { panic('unsupported green recovery consumer ${consumer_kind}') }
	}
	final_target_state := if consumer_kind == 'rollback_post' { 'quarantined' } else { 'eligible' }
	final_publication_state := if consumer_kind == 'rollback_post' {
		'restored_last_known_good'
	} else {
		'idle'
	}
	mut native := canonical_root_member(root, 'native_gate_execution')
	native = native.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":10')
	smoke := live_recovery_rehashed_smoke_for('target-state.v-smoke-terminal-check.schema-fixture.json',
		10, consumer_kind)
	mut pre_source := bin.canonical_json(root)
	pre_source = replace_canonical_root_member(pre_source, root, 'generation', '10')
	pre_source = replace_canonical_root_member(pre_source, root, 'native_gate_execution', native)
	pre_source = replace_canonical_root_member(pre_source, root, 'v_smoke_execution', smoke)
	pre_root := bin.parse_strict_json(pre_source) or { panic(err) }
	pre_projection := bin.terminal_state_projection(pre_root) or { panic(err) }
	last_known_good := live_artifact_tuple_from_subject(subject)
	last_validation := '{"run_id":3001,"run_attempt":1,"subject_hash":"${subject_hash}","conclusion":"success","evidence_digest":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"}'
	mut final_projection_source := bin.canonical_json(root)
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'generation', '12')
	if consumer_kind == 'remediation' {
		final_projection_source = replace_canonical_root_member(final_projection_source, root,
			'bootstrap_required', 'false')
	}
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'target_state', '"${final_target_state}"')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'publication_state', '"${final_publication_state}"')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'last_known_good', last_known_good)
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'provisional_published', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'active_intent', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'post_validation_operation_id', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'native_gate_subject', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'active_subject_hash', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'native_gate_execution', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'v_smoke_execution', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'active_recovery_handoff_id', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'active_remediation_id', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'active_remediation_binding', 'null')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'remediation_check_sources', '[]')
	final_projection_source = replace_canonical_root_member(final_projection_source, root,
		'last_validation', last_validation)
	final_projection_root := bin.parse_strict_json(final_projection_source) or { panic(err) }
	final_projection := bin.terminal_state_projection(final_projection_root) or { panic(err) }
	placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	native_check := live_recovery_native_check_for(subject_hash, consumer_id, native_ref)
	mut proof := '{"schema_version":5,"expected_check_sources":${bin.canonical_json(expected_sources)},"native_gate_execution":${native},"native_gate_check":${native_check},"v_smoke_execution":${smoke},"business_operation_id":"${live_h2_business_operation_id}","business_transition":"${business_transition}","source_refetch":null,"source_state_pre_snapshot":null,"source_state_snapshot":null,"source_state_cas_history":[],"git_ancestry_proof":null,"source_atomic_pre_projection":null,"pre_business_projection":${bin.canonical_json(pre_projection)},"final_projection":${bin.canonical_json(final_projection)},"facts_digest":"${placeholder}"}'
	proof_value := bin.parse_strict_json(proof) or { panic(err) }
	proof_digest := bin.terminal_revalidation_facts_digest(proof_value) or { panic(err) }
	proof = proof.replace_once(placeholder, proof_digest)
	proof = bin.canonical_json(bin.parse_strict_json(proof) or { panic(err) })
	proof_smoke := (bin.parse_strict_json(proof) or { panic(err) }).object_value('v_smoke_execution') or {
		panic('terminal proof V smoke missing')
	}
	proof_attempts := proof_smoke.object_value('attempts') or { panic('terminal attempts missing') }
	smoke_terminal_digest := bin.v_smoke_terminal_payload_digest(proof_smoke,
		proof_attempts.array_value[0]) or { panic(err) }
	predecessor := bin.canonical_json(handoffs.array_value[0])
	successor := live_recovery_successor_projection(root, 'complete', 12, proof)
	operations := append_live_recovery_h2_operations(canonical_root_member(root,
		'applied_operations'), 12, successor_id, smoke_terminal_digest, business_transition,
		proof_digest)
	mut source := bin.canonical_json(root)
	source = replace_canonical_root_member(source, root, 'generation', '12')
	if consumer_kind == 'remediation' {
		source = replace_canonical_root_member(source, root, 'bootstrap_required', 'false')
	}
	source = replace_canonical_root_member(source, root, 'target_state', '"${final_target_state}"')
	source = replace_canonical_root_member(source, root, 'publication_state',
		'"${final_publication_state}"')
	source = replace_canonical_root_member(source, root, 'last_known_good', last_known_good)
	source = replace_canonical_root_member(source, root, 'provisional_published', 'null')
	source = replace_canonical_root_member(source, root, 'active_intent', 'null')
	source = replace_canonical_root_member(source, root, 'post_validation_operation_id', 'null')
	source = replace_canonical_root_member(source, root, 'native_gate_subject', 'null')
	source = replace_canonical_root_member(source, root, 'active_subject_hash', 'null')
	source = replace_canonical_root_member(source, root, 'native_gate_execution', 'null')
	source = replace_canonical_root_member(source, root, 'v_smoke_execution', 'null')
	source = replace_canonical_root_member(source, root, 'active_recovery_handoff_id', 'null')
	source = replace_canonical_root_member(source, root, 'active_remediation_id', 'null')
	source = replace_canonical_root_member(source, root, 'active_remediation_binding', 'null')
	source = replace_canonical_root_member(source, root, 'remediation_check_sources', '[]')
	source = replace_canonical_root_member(source, root, 'recovery_handoffs',
		'[${predecessor},${successor}]')
	source = replace_canonical_root_member(source, root, 'applied_operations', operations)
	source = replace_canonical_root_member(source, root, 'last_validation', last_validation)
	source = replace_canonical_root_member(source, root, 'last_operation_id',
		'"${live_h2_completion_operation_id}"')
	return replace_canonical_root_member(source, root, 'last_transition',
		'"handoff_complete_${proof_digest}"')
}

fn live_functional_v_smoke(smoke bin.JsonValue) bin.JsonValue {
	mut source := '{"v_smoke_execution":${bin.canonical_json(smoke)}}'
	source = source.replace_once('"state":"completed"', '"state":"blocked"')
	source = source.replace_once('"run_conclusion":"success"', '"run_conclusion":"failure"')
	source = source.replace_once('"check_conclusion":"success"', '"check_conclusion":"failure"')
	refreshed := bin.parse_strict_json(refresh_v_smoke_facts_digests(source)) or { panic(err) }
	return refreshed.object_value('v_smoke_execution') or { panic('functional smoke missing') }
}

fn live_reserved_rollback_intent(pre_root bin.JsonValue) string {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	mut intent := os.read_file(os.join_path(fixture_root,
		'active-intent.bootstrap.schema-fixture.json')) or { panic(err) }
	rollback_id := live_h2_business_operation_id
	intent = intent.replace(receiver_consumer_id, rollback_id)
	intent = intent.replace_once('"intent_type": "initial_adopt_current"',
		'"intent_type": "rollback"')
	validation_start := intent.index('"validation_subject": {') or { panic(err) }
	previous_start := intent.index('"previous_last_known_good": null') or { panic(err) }
	intent = intent[..validation_start] + '"validation_subject": null,\n  ' +
		intent[previous_start..]
	intent = intent.replace_once('"previous_last_known_good": null', '"previous_last_known_good": ${canonical_root_member(pre_root,
		'last_known_good')}')
	intent = intent.replace_once('"bad_provisional": null', '"bad_provisional": ${canonical_root_member(pre_root,
		'provisional_published')}')
	intent = intent.replace_once('"rollback_diff_fingerprint": null',
		'"rollback_diff_fingerprint": "9797979797979797979797979797979797979797979797979797979797979797"')
	pre_generation := pre_root.object_value('generation') or { panic('pre generation missing') }
	intent = intent.replace_once('"generation": 0', '"generation": ${pre_generation.int_value}')
	return bin.canonical_json(bin.parse_strict_json(intent) or { panic(err) })
}

fn live_terminal_head_observation(pre_root bin.JsonValue, relationship string,
	canonical_head string, evidence_digest string) string {
	pre_generation := pre_root.object_value('generation') or { panic('pre generation missing') }
	previous_head := pre_root.object_value('canonical_observed_sha') or {
		panic('pre canonical HEAD missing')
	}
	return '{"target_id":"linux-amd64","expected_generation":${pre_generation.int_value},"expected_previous_head":"${previous_head.string_value}","canonical_head":"${canonical_head}","subject_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","relationship":"${relationship}","observed_at":"2026-08-03T02:00:00Z","operation_id":"${live_h2_business_operation_id}","evidence_digest":"${evidence_digest}"}'
}

fn live_git_ancestry_proof(canonical_head string) string {
	placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	mut proof := '{"schema_version":1,"repository":"vlang/tccbin","canonical_ref":"thirdparty-linux-amd64","target_id":"linux-amd64","subject_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","canonical_head":"${canonical_head}","merge_base_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","relationship":"subject_ancestor","query_method":"git_merge_base_is_ancestor","observed_at":"2026-08-03T02:00:00Z","operation_id":"${live_h2_business_operation_id}","evidence_digest":"${placeholder}"}'
	digest := bin.git_ancestry_evidence_digest(bin.parse_strict_json(proof) or { panic(err) }) or {
		panic(err)
	}
	proof = proof.replace_once(placeholder, digest)
	return bin.canonical_json(bin.parse_strict_json(proof) or { panic(err) })
}

fn live_recovery_h2_functional_source_for(consumer_kind string) string {
	if consumer_kind !in ['publish_post', 'rollback_post', 'remediation'] {
		panic('unsupported functional recovery consumer ${consumer_kind}')
	}
	green_source := live_recovery_h2_green_source_for(consumer_kind)
	green_root := bin.parse_strict_json(green_source) or { panic(err) }
	base_root := bin.parse_strict_json(live_recovery_chain_source_for(consumer_kind)) or {
		panic(err)
	}
	green_handoffs := green_root.object_value('recovery_handoffs') or {
		panic('green handoffs missing')
	}
	predecessor := green_handoffs.array_value[0]
	successor := green_handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or { panic('green proof missing') }
	old_proof_digest := (proof.object_value('facts_digest') or {
		panic('green proof digest missing')
	}).string_value
	green_smoke := proof.object_value('v_smoke_execution') or { panic('green smoke missing') }
	failed_smoke := live_functional_v_smoke(green_smoke)
	green_attempts := green_smoke.object_value('attempts') or { panic('green attempts missing') }
	failed_attempts := failed_smoke.object_value('attempts') or { panic('failed attempts missing') }
	old_smoke_completion_digest := bin.v_smoke_terminal_payload_digest(green_smoke,
		green_attempts.array_value[0]) or { panic(err) }
	new_smoke_completion_digest := bin.v_smoke_terminal_payload_digest(failed_smoke,
		failed_attempts.array_value[0]) or { panic(err) }
	subject := successor.object_value('subject') or { panic('subject missing') }
	subject_hash := (successor.object_value('subject_hash') or { panic('subject hash missing') }).string_value
	consumer_id := (subject.object_value('consumer_id') or { panic('consumer ID missing') }).string_value
	new_business_transition := match consumer_kind {
		'publish_post' { 'post_check_red' }
		'rollback_post' { 'rollback_failed' }
		else { 'remediation_red' }
	}
	last_validation := '{"run_id":3001,"run_attempt":1,"subject_hash":"${subject_hash}","conclusion":"failure","evidence_digest":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"}'
	mut final_source := bin.canonical_json(green_root)
	root_target_id := canonical_root_member(green_root, 'target_id')
	root_target_state := canonical_root_member(green_root, 'target_state')
	root_target_anchor := '"target_id":${root_target_id},"target_state":${root_target_state}'
	if final_source.count(root_target_anchor) != 1 {
		panic('canonical root target_id/target_state anchor must occur exactly once')
	}
	final_source = final_source.replace_once(root_target_anchor,
		'"target_id":${root_target_id},"target_state":"quarantined"')
	final_source = replace_canonical_root_member(final_source, green_root, 'last_known_good', canonical_root_member(base_root,
		'last_known_good'))
	final_source = replace_canonical_root_member(final_source, green_root, 'provisional_published', canonical_root_member(base_root,
		'provisional_published'))
	if consumer_kind == 'remediation' {
		final_source = replace_canonical_root_member(final_source, green_root,
			'bootstrap_required', canonical_root_member(base_root, 'bootstrap_required'))
	}
	if consumer_kind == 'publish_post' {
		pre_business_projection := proof.object_value('pre_business_projection') or {
			panic('pre projection missing')
		}
		final_source = replace_canonical_root_member(final_source, green_root, 'publication_state',
			'"rollback_pending"')
		final_source = replace_canonical_root_member(final_source, green_root, 'active_intent',
			live_reserved_rollback_intent(pre_business_projection))
		final_source = replace_canonical_root_member(final_source, green_root,
			'last_head_observation', live_terminal_head_observation(pre_business_projection,
			'exact_subject', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
			'9898989898989898989898989898989898989898989898989898989898989898'))
	} else if consumer_kind == 'rollback_post' {
		blocked_intent := canonical_root_member(base_root, 'active_intent').replace_once('"stage":"post_checks_running"',
			'"stage":"blocked"')
		final_source = replace_canonical_root_member(final_source, green_root, 'publication_state',
			'"rollback_blocked"')
		final_source = replace_canonical_root_member(final_source, green_root, 'active_intent',
			blocked_intent)
		final_source = replace_canonical_root_member(final_source, green_root,
			'post_validation_operation_id', '"${consumer_id}"')
		final_source = replace_canonical_root_member(final_source, green_root,
			'native_gate_subject', bin.canonical_json(subject))
		final_source = replace_canonical_root_member(final_source, green_root,
			'active_subject_hash', '"${subject_hash}"')
		proof_native := proof.object_value('native_gate_execution') or {
			panic('native proof missing')
		}
		current_native := bin.canonical_json(proof_native).replace_once('"expected_ledger_generation":10',
			'"expected_ledger_generation":12')
		mut current_smoke_wrapper := '{"v_smoke_execution":${bin.canonical_json(failed_smoke).replace_once('"expected_ledger_generation":10',
			'"expected_ledger_generation":12')}}'
		current_smoke_wrapper = refresh_v_smoke_facts_digests(current_smoke_wrapper)
		current_smoke_root := bin.parse_strict_json(current_smoke_wrapper) or { panic(err) }
		current_smoke := current_smoke_root.object_value('v_smoke_execution') or {
			panic('current functional smoke missing')
		}
		final_source = replace_canonical_root_member(final_source, green_root,
			'native_gate_execution', current_native)
		root_v_smoke := canonical_root_member(green_root, 'v_smoke_execution')
		root_v_smoke_anchor := '"target_id":${root_target_id},"target_state":"quarantined","v_smoke_execution":${root_v_smoke}'
		if final_source.count(root_v_smoke_anchor) != 1 {
			panic('canonical root target_id/target_state/v_smoke_execution anchor must occur exactly once')
		}
		final_source = final_source.replace_once(root_v_smoke_anchor,
			'"target_id":${root_target_id},"target_state":"quarantined","v_smoke_execution":${bin.canonical_json(current_smoke)}')
	}
	final_source = replace_canonical_root_member(final_source, green_root, 'last_validation',
		last_validation)
	projection_root := bin.parse_strict_json(final_source) or { panic(err) }
	final_projection := bin.terminal_state_projection(projection_root) or { panic(err) }
	canonical_proof := bin.canonical_json(proof)
	canonical_green_smoke := bin.canonical_json(green_smoke)
	canonical_failed_smoke := bin.canonical_json(failed_smoke)
	proof_smoke_member := '"v_smoke_execution":${canonical_green_smoke}'
	if canonical_proof.count(proof_smoke_member) != 2 {
		panic('functional proof must contain exactly its pre-business and proof-level V-smoke copies')
	}
	pre_business_projection := proof.object_value('pre_business_projection') or {
		panic('pre-business projection missing')
	}
	canonical_pre_business := bin.canonical_json(pre_business_projection)
	if canonical_pre_business.count(proof_smoke_member) != 1 {
		panic('functional pre-business V-smoke projection must occur exactly once')
	}
	updated_pre_business := canonical_pre_business.replace_once(proof_smoke_member,
		'"v_smoke_execution":${canonical_failed_smoke}')
	mut updated_proof := canonical_proof.replace_once('"pre_business_projection":${canonical_pre_business}',
		'"pre_business_projection":${updated_pre_business}')
	proof_smoke_anchor := '"source_state_snapshot":null,${proof_smoke_member}'
	if updated_proof.count(proof_smoke_anchor) != 1 {
		panic('functional proof-level V-smoke suffix must occur exactly once')
	}
	updated_proof = updated_proof.replace_once(proof_smoke_anchor,
		'"source_state_snapshot":null,"v_smoke_execution":${canonical_failed_smoke}')
	old_business_transition := match consumer_kind {
		'publish_post' { 'post_check_green' }
		'rollback_post' { 'rollback_post_green' }
		else { 'remediation_green' }
	}
	updated_proof = updated_proof.replace_once('"business_transition":"${old_business_transition}"',
		'"business_transition":"${new_business_transition}"')
	old_projection := proof.object_value('final_projection') or {
		panic('final projection missing')
	}
	updated_proof = updated_proof.replace_once('"final_projection":${bin.canonical_json(old_projection)}',
		'"final_projection":${bin.canonical_json(final_projection)}')
	updated_proof = updated_proof.replace_once('"facts_digest":"${old_proof_digest}"',
		'"facts_digest":"0000000000000000000000000000000000000000000000000000000000000000"')
	updated_proof_value := bin.parse_strict_json(updated_proof) or { panic(err) }
	new_proof_digest := bin.terminal_revalidation_facts_digest(updated_proof_value) or {
		panic(err)
	}
	updated_proof = updated_proof.replace_once('0000000000000000000000000000000000000000000000000000000000000000',
		new_proof_digest)
	canonical_successor := bin.canonical_json(successor)
	mut updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_successor = updated_successor.replace_once('"receiver_conclusion":"success"',
		'"receiver_conclusion":"failure"')
	updated_successor = updated_successor.replace_once('"terminal_outcome":"green"',
		'"terminal_outcome":"functional_defect_routed"')
	final_source = replace_canonical_root_member(final_source, green_root, 'recovery_handoffs',
		'[${bin.canonical_json(predecessor)},${updated_successor}]')
	mut operations := canonical_root_member(green_root, 'applied_operations').replace_once('v-smoke-complete-1_${old_smoke_completion_digest}',
		'v-smoke-complete-1_${new_smoke_completion_digest}')
	business_cas_anchor := '"operation_id":"${live_h2_business_operation_id}","resulting_generation":11,"transition":"${old_business_transition}"'
	if operations.count(business_cas_anchor) != 1 {
		panic('functional business CAS must occur exactly once in canonical key order')
	}
	operations = operations.replace_once(business_cas_anchor,
		'"operation_id":"${live_h2_business_operation_id}","resulting_generation":11,"transition":"${new_business_transition}"')
	operations = operations.replace('handoff_complete_${old_proof_digest}',
		'handoff_complete_${new_proof_digest}')
	final_source = replace_canonical_root_member(final_source, green_root, 'applied_operations',
		operations)
	final_source = replace_canonical_root_member(final_source, green_root, 'last_transition',
		'"handoff_complete_${new_proof_digest}"')
	return final_source
}

fn live_source_waiting_v_smoke_for(expected_generation i64, consumer_kind string,
	with_infrastructure_retry bool) string {
	fixture := if with_infrastructure_retry {
		'target-state.v-smoke-retry-terminal.schema-fixture.json'
	} else {
		'target-state.v-smoke-terminal-check.schema-fixture.json'
	}
	mut source := '{"v_smoke_execution":${live_recovery_smoke_projection_for(fixture,
		expected_generation, consumer_kind)}}'
	if with_infrastructure_retry {
		source = source.replace('d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2',
			live_h2_retry_dispatch_operation_id)
		source = source.replace('5656565656565656565656565656565656565656565656565656565656565656',
			live_h2_retry_ack_operation_id)
		source = source.replace('7878787878787878787878787878787878787878787878787878787878787878',
			live_h2_retry_completion_operation_id)
	}
	source = source.replace_once('"state":"completed"', '"state":"blocked"')
	source = source.replace_once('"run_conclusion":"success"', '"run_conclusion":"timed_out"')
	refreshed := bin.parse_strict_json(refresh_v_smoke_facts_digests(source)) or { panic(err) }
	smoke := refreshed.object_value('v_smoke_execution') or {
		panic('source-waiting V smoke missing')
	}
	return bin.canonical_json(smoke)
}

fn live_source_evidence(waiting_consumer_ids []string, expected_target_generation i64,
	repeated_daily_outage bool, expected_head_oid string,
	source_operation_ordinal int, source_kind string, cas_attempt int) (string, string, string, string, string, string) {
	if waiting_consumer_ids.len == 0 {
		panic('source evidence requires at least one waiting consumer')
	}
	if cas_attempt < 1 || cas_attempt > 3 {
		panic('source evidence CAS attempt must be in 1..3')
	}
	waiting_consumers := waiting_consumer_ids.map('"${it}"').join(',')
	if source_kind !in ['tinycc', 'bdwgc'] {
		panic('unsupported source evidence kind')
	}
	source_id := source_kind
	source_state_id := if source_kind == 'tinycc' { 'tinycc-mob' } else { 'bdwgc-master' }
	source_repository := if source_kind == 'tinycc' {
		'https://repo.or.cz/tinycc.git'
	} else {
		'https://github.com/ivmai/bdwgc'
	}
	source_ref := if source_kind == 'tinycc' { 'mob' } else { 'master' }
	placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	transition_placeholder := '0101010101010101010101010101010101010101010101010101010101010101'
	previous_generation := if repeated_daily_outage { i64(4) } else { i64(0) }
	resulting_generation := previous_generation + 1
	resolver_run_id := if repeated_daily_outage { i64(8002) } else { i64(8001) }
	originating_run_id := if repeated_daily_outage { i64(7999) } else { i64(8001) }
	observed_at := if repeated_daily_outage {
		'2026-08-04T02:01:00Z'
	} else {
		'2026-08-03T02:01:00Z'
	}
	pre_mode := if repeated_daily_outage { 'upstream-recovery-daily' } else { 'monthly' }
	pre_origin := if repeated_daily_outage { '7999' } else { 'null' }
	pre_waiting := if repeated_daily_outage { '[${waiting_consumers}]' } else { '[]' }
	// A v1 -> v2 migration starts its independent operation log at C=0 even when the
	// business generation is already non-zero.
	pre_operation_count := i64(0)
	pre_chain_digest := placeholder
	pre_attempt_at := if repeated_daily_outage {
		'2026-08-03T02:01:00Z'
	} else {
		'2026-08-02T02:01:00Z'
	}
	source_state_pre := '{"schema_version":2,"generation":${previous_generation},"source_id":"${source_state_id}","canonical_url":"${source_repository}","ref":"${source_ref}","status":"resolved","resolved_sha":"cccccccccccccccccccccccccccccccccccccccc","source_fingerprint":"1111111111111111111111111111111111111111111111111111111111111111","last_attempt_at":"${pre_attempt_at}","mode":"${pre_mode}","originating_run_id":${pre_origin},"waiting_consumers":${pre_waiting},"operation_count":${pre_operation_count},"operation_chain_digest":"${pre_chain_digest}","operation_window":{"start_count":${pre_operation_count},"anchor_digest":"${pre_chain_digest}","entries":[]}}'
	pre_state_value := bin.parse_strict_json(source_state_pre) or { panic(err) }
	subject_fingerprint := bin.source_state_subject_fingerprint(pre_state_value) or { panic(err) }
	not_applicable_digest := placeholder
	operation_id := bin.deterministic_operation_id(bin.OperationIdentityInput{
		audience:                'vlang/v:tccbin-source-state:v2'
		run_id:                  resolver_run_id
		run_attempt:             1
		ordinal:                 source_operation_ordinal
		cas_attempt:             cas_attempt
		subject_id:              source_state_id
		transition:              'resolve_source_unreachable'
		expected_generation:     previous_generation
		expected_canonical_head: expected_head_oid
		source_ref:              source_ref
		source_sha:              'cccccccccccccccccccccccccccccccccccccccc'
		subject_fingerprint:     subject_fingerprint
		input_fingerprint:       not_applicable_digest
		artifact_fingerprint:    not_applicable_digest
		manifest_hash:           not_applicable_digest
		native_subject_hash:     not_applicable_digest
	}) or { panic(err) }
	post_state_seed := '{"schema_version":2,"generation":${resulting_generation},"source_id":"${source_state_id}","canonical_url":"${source_repository}","ref":"${source_ref}","status":"source_unreachable","resolved_sha":null,"source_fingerprint":"1111111111111111111111111111111111111111111111111111111111111111","last_attempt_at":"${observed_at}","mode":"upstream-recovery-daily","originating_run_id":${originating_run_id},"waiting_consumers":[${waiting_consumers}],"operation_count":${
		pre_operation_count + 1},"operation_chain_digest":"${placeholder}","operation_window":{"start_count":${pre_operation_count},"anchor_digest":"${pre_chain_digest}","entries":[]}}'
	state_seed_value := bin.parse_strict_json(post_state_seed) or { panic(err) }
	pre_state_digest := bin.source_state_snapshot_digest(pre_state_value) or { panic(err) }
	post_state_digest := bin.source_state_snapshot_digest(state_seed_value) or { panic(err) }
	evidence_path := bin.evidence_path(2026, 8, resolver_run_id, 1, source_state_id, operation_id,
		resulting_generation, 'resolve_source_unreachable', subject_fingerprint) or { panic(err) }
	state_path := bin.source_state_path(source_state_id) or { panic(err) }
	universal_evidence := '{"schema_version":1,"operation_id":"${operation_id}","operation_ordinal":${source_operation_ordinal},"cas_attempt":${cas_attempt},"run_id":${resolver_run_id},"run_attempt":1,"intent_id":null,"transition":"resolve_source_unreachable","workflow":".github/workflows/tccbin_source_recovery.yml","workflow_ref":"master","workflow_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","subject_id":"${source_state_id}","subject_fingerprint":"${subject_fingerprint}","target_id":null,"input_fingerprint":null,"artifact_fingerprint":null,"generation_read":${previous_generation},"generation_written":${resulting_generation},"result":"blocked","digests":[{"path":"${state_path}","sha256":"${post_state_digest}"}]}'
	universal_value := bin.parse_strict_json(universal_evidence) or { panic(err) }
	universal_evidence_digest := bin.source_state_universal_evidence_digest(universal_value) or {
		panic(err)
	}
	mut operation_entry := '{"sequence":${pre_operation_count + 1},"operation_id":"${operation_id}","transition":"resolve_source_unreachable","previous_generation":${previous_generation},"resulting_generation":${resulting_generation},"previous_state_digest":"${pre_state_digest}","resulting_state_digest":"${post_state_digest}","evidence_path":"${evidence_path}","evidence_digest":"${universal_evidence_digest}","previous_chain_digest":"${pre_chain_digest}","resulting_chain_digest":"${placeholder}"}'
	resulting_chain_digest := bin.source_state_operation_chain_digest(bin.parse_strict_json(operation_entry) or {
		panic(err)
	}) or { panic(err) }
	operation_entry = operation_entry.replace_once('"resulting_chain_digest":"${placeholder}"',
		'"resulting_chain_digest":"${resulting_chain_digest}"')
	source_state := post_state_seed.replace_once('"operation_chain_digest":"${placeholder}"',
		'"operation_chain_digest":"${resulting_chain_digest}"').replace_once('"entries":[]',
		'"entries":[${operation_entry}]')
	state_value := bin.parse_strict_json(source_state) or { panic(err) }
	mut transition := '{"schema_version":1,"source_id":"${source_state_id}","sequence":${
		pre_operation_count + 1},"operation_id":"${operation_id}","transition":"resolve_source_unreachable","previous_generation":${previous_generation},"resulting_generation":${resulting_generation},"previous_state_digest":"${pre_state_digest}","resulting_state_digest":"${post_state_digest}","observed_at":"${observed_at}","originating_run_id":${originating_run_id},"expected_state_parent_sha":"${expected_head_oid}","universal_evidence":${universal_evidence},"universal_evidence_digest":"${universal_evidence_digest}","evidence_path":"${evidence_path}","previous_chain_digest":"${pre_chain_digest}","resulting_chain_digest":"${resulting_chain_digest}","evidence_digest":"${transition_placeholder}"}'
	transition_digest := bin.source_state_transition_evidence_digest(bin.parse_strict_json(transition) or {
		panic(err)
	}) or { panic(err) }
	transition = transition.replace_once('"evidence_digest":"${transition_placeholder}"',
		'"evidence_digest":"${transition_digest}"')
	canonical_history := '[${bin.canonical_json(bin.parse_strict_json(transition) or { panic(err) })}]'
	mut refetch := '{"target_id":"linux-amd64","expected_generation":${expected_target_generation},"expected_canonical_head":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","source_state_id":"${source_state_id}","source_state_generation":${resulting_generation},"resolution_operation_id":"${operation_id}","source_id":"${source_id}","source_repository":"${source_repository}","requested_ref":"${source_ref}","previous_sha":"cccccccccccccccccccccccccccccccccccccccc","resolved_sha":null,"resolved_tree":null,"status":"unreachable","failure_kind":"timeout","evidence_digest":"${placeholder}","input_fingerprint":"3333333333333333333333333333333333333333333333333333333333333333","checked_at":"${observed_at}","operation_id":"${live_h2_business_operation_id}"}'
	refetch_value := bin.parse_strict_json(refetch) or { panic(err) }
	history_value := bin.parse_strict_json(canonical_history) or { panic(err) }
	evidence_digest := bin.source_refetch_evidence_digest(refetch_value, pre_state_value,
		state_value, history_value) or { panic(err) }
	refetch = refetch.replace_once('"evidence_digest":"${placeholder}"',
		'"evidence_digest":"${evidence_digest}"')
	return bin.canonical_json(bin.parse_strict_json(refetch) or { panic(err) }), bin.canonical_json(pre_state_value), bin.canonical_json(state_value), canonical_history, evidence_digest, operation_id
}

fn live_recovery_source_successor_projection(root bin.JsonValue, terminal_proof string,
	with_infrastructure_retry bool) string {
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	mut successor := bin.canonical_json(handoffs.array_value[1])
	final_generation := if with_infrastructure_retry { 15 } else { 12 }
	dispatch_generation := if with_infrastructure_retry { 2 } else { 1 }
	dispatch_ids := if with_infrastructure_retry {
		'["${live_h2_dispatch_operation_id}","${live_h2_retry_dispatch_operation_id}"]'
	} else {
		'["${live_h2_dispatch_operation_id}"]'
	}
	selected_ack := if with_infrastructure_retry {
		live_h2_retry_ack_operation_id
	} else {
		live_h2_ack_operation_id
	}
	selected_attempt := if with_infrastructure_retry { 2 } else { 1 }
	selected_output := if with_infrastructure_retry {
		'dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd'
	} else {
		'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
	}
	selected_deadline := if with_infrastructure_retry {
		'2026-08-03T02:31:00Z'
	} else {
		'2026-08-03T01:31:00Z'
	}
	proof_value := bin.parse_strict_json(terminal_proof) or { panic(err) }
	refetch := proof_value.object_value('source_refetch') or { panic('source refetch missing') }
	checked_at := (refetch.object_value('checked_at') or {
		panic('source refetch timestamp missing')
	}).string_value
	terminal_completed_at := checked_at[..17] + '02Z'
	successor = successor.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":${final_generation}')
	successor = successor.replace_once('"dispatch_generation":0',
		'"dispatch_generation":${dispatch_generation}')
	successor = successor.replace_once('"dispatch_operation_ids":[]',
		'"dispatch_operation_ids":${dispatch_ids}')
	successor = successor.replace_once('"ack_operation_id":null',
		'"ack_operation_id":"${selected_ack}"')
	successor = successor.replace_once('"selected_run_id":null', '"selected_run_id":3001')
	successor = successor.replace_once('"selected_run_attempt":null',
		'"selected_run_attempt":${selected_attempt}')
	successor = successor.replace_once('"receiver_master_sha":null',
		'"receiver_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"')
	successor = successor.replace_once('"receiver_conclusion":null',
		'"receiver_conclusion":"timed_out"')
	successor = successor.replace_once('"receiver_output_digest":null',
		'"receiver_output_digest":"${selected_output}"')
	successor = successor.replace_once('"deadline":null', '"deadline":"${selected_deadline}"')
	successor = successor.replace_once('"terminal_outcome":null',
		'"terminal_outcome":"source_waiting"')
	successor = successor.replace_once('"completion_operation_id":null',
		'"completion_operation_id":"${live_h2_completion_operation_id}"')
	successor = successor.replace_once('"terminal_revalidation":null',
		'"terminal_completed_at":"${terminal_completed_at}","terminal_revalidation":${terminal_proof}')
	successor = successor.replace_once('"state":"pending"', '"state":"complete"')
	return bin.canonical_json(bin.parse_strict_json(successor) or { panic(err) })
}

fn append_live_recovery_source_operations(operations string, successor_id string,
	proof_digest string, source_evidence_digest string, first_smoke_digest string,
	second_smoke_digest string, with_infrastructure_retry bool) string {
	mut additions := ',{"operation_id":"${live_h2_dispatch_operation_id}","transition":"handoff_dispatch_${successor_id}","resulting_generation":8},{"operation_id":"${live_h2_ack_operation_id}","transition":"handoff_ack_${successor_id}","resulting_generation":9},{"operation_id":"${live_h2_smoke_completion_operation_id}","transition":"v-smoke-complete-1_${first_smoke_digest}","resulting_generation":10}'
	if with_infrastructure_retry {
		additions += ',{"operation_id":"${live_h2_retry_dispatch_operation_id}","transition":"handoff_dispatch_${successor_id}","resulting_generation":11},{"operation_id":"${live_h2_retry_ack_operation_id}","transition":"handoff_ack_${successor_id}","resulting_generation":12},{"operation_id":"${live_h2_retry_completion_operation_id}","transition":"v-smoke-complete-2_${second_smoke_digest}","resulting_generation":13},{"operation_id":"${live_h2_business_operation_id}","transition":"source_unreachable_${source_evidence_digest}","resulting_generation":14},{"operation_id":"${live_h2_completion_operation_id}","transition":"handoff_complete_${proof_digest}","resulting_generation":15}'
	} else {
		additions += ',{"operation_id":"${live_h2_business_operation_id}","transition":"source_unreachable_${source_evidence_digest}","resulting_generation":11},{"operation_id":"${live_h2_completion_operation_id}","transition":"handoff_complete_${proof_digest}","resulting_generation":12}'
	}
	return operations[..operations.len - 1] + additions + ']'
}

fn live_recovery_h2_source_waiting_source_for(consumer_kind string) string {
	return live_recovery_h2_source_waiting_variant_for(consumer_kind, false, false)
}

fn live_recovery_h2_source_waiting_repeated_daily_source_for(consumer_kind string) string {
	return live_recovery_h2_source_waiting_variant_for(consumer_kind, false, true)
}

fn live_recovery_h2_source_waiting_variant_for(consumer_kind string,
	with_infrastructure_retry bool, repeated_daily_outage bool) string {
	return live_recovery_h2_source_waiting_variant_with_parent_for(consumer_kind,
		with_infrastructure_retry, repeated_daily_outage,
		'abababababababababababababababababababab')
}

fn live_recovery_h2_source_waiting_variant_with_parent_for(consumer_kind string,
	with_infrastructure_retry bool, repeated_daily_outage bool,
	expected_state_parent_sha string) string {
	return live_recovery_h2_source_waiting_variant_with_parent_and_cas_for(consumer_kind,
		with_infrastructure_retry, repeated_daily_outage, expected_state_parent_sha, 1)
}

fn live_recovery_h2_source_waiting_variant_with_parent_and_cas_for(consumer_kind string,
	with_infrastructure_retry bool, repeated_daily_outage bool, expected_state_parent_sha string,
	cas_attempt int) string {
	return live_recovery_h2_source_waiting_variant_with_parent_and_consumers_for(consumer_kind,
		with_infrastructure_retry, repeated_daily_outage, expected_state_parent_sha, []string{}, 1,
		'tinycc', cas_attempt)
}

fn live_target_for_source_kind(source string, source_kind string) string {
	if source_kind == 'tinycc' {
		return source
	}
	if source_kind != 'bdwgc' {
		panic('unsupported target source kind')
	}
	mut result := bin.canonical_json(bin.parse_strict_json(source) or { panic(err) })
	result = result.replace('"id":"tinycc"', '"id":"bdwgc"')
	result = result.replace('"source_id":"tinycc"', '"source_id":"bdwgc"')
	result = result.replace('https://repo.or.cz/tinycc.git', 'https://github.com/ivmai/bdwgc')
	result = result.replace('"ref":"mob"', '"ref":"master"')
	return bin.canonical_json(bin.parse_strict_json(result) or { panic(err) })
}

fn live_recovery_h2_source_waiting_variant_with_parent_and_consumers_for(consumer_kind string,
	with_infrastructure_retry bool, repeated_daily_outage bool, expected_state_parent_sha string,
	shared_waiting_consumers []string, source_operation_ordinal int, source_kind string,
	cas_attempt int) string {
	if consumer_kind !in ['publish_post', 'rollback_post', 'remediation'] {
		panic('unsupported source-waiting recovery consumer ${consumer_kind}')
	}
	proof_generation := if with_infrastructure_retry { i64(13) } else { i64(10) }
	final_generation := if with_infrastructure_retry { i64(15) } else { i64(12) }
	base := live_target_for_source_kind(live_recovery_chain_source_for(consumer_kind), source_kind)
	root := bin.parse_strict_json(base) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	intent := root.object_value('active_intent') or { panic('active intent missing') }
	remediation_binding := root.object_value('active_remediation_binding') or {
		panic('remediation binding missing')
	}
	expected_sources := if intent.kind == .object {
		intent.object_value('expected_check_sources') or { panic('intent sources missing') }
	} else {
		remediation_binding.object_value('expected_check_sources') or {
			panic('remediation sources missing')
		}
	}
	successor := handoffs.array_value[1]
	subject := successor.object_value('subject') or { panic('subject missing') }
	subject_hash := (successor.object_value('subject_hash') or { panic('subject hash missing') }).string_value
	consumer_id := (subject.object_value('consumer_id') or { panic('consumer ID missing') }).string_value
	mut waiting_consumers := if shared_waiting_consumers.len == 0 {
		[consumer_id]
	} else {
		shared_waiting_consumers.clone()
	}
	waiting_consumers.sort()
	if consumer_id !in waiting_consumers {
		panic('shared source consumer union omits the target consumer')
	}
	epochs := (root.object_value('native_gate_execution') or { panic('native execution missing') }).object_value('gate_epochs') or {
		panic('native epochs missing')
	}
	native_ref := (epochs.array_value[0].object_value('expected_ref') or {
		panic('native ref missing')
	}).string_value
	successor_id := (successor.object_value('handoff_id') or { panic('successor ID missing') }).string_value

	mut proof_native := canonical_root_member(root, 'native_gate_execution')
	proof_native = proof_native.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":${proof_generation}')
	proof_smoke := live_source_waiting_v_smoke_for(proof_generation, consumer_kind,
		with_infrastructure_retry)
	proof_smoke_value := bin.parse_strict_json(proof_smoke) or { panic(err) }
	proof_attempts := proof_smoke_value.object_value('attempts') or {
		panic('source attempts missing')
	}
	selected_attempt := proof_attempts.array_value[proof_attempts.array_value.len - 1]
	mut pre_source := bin.canonical_json(root)
	pre_source = replace_canonical_root_member(pre_source, root, 'generation',
		proof_generation.str())
	pre_source = replace_canonical_root_member(pre_source, root, 'native_gate_execution',
		proof_native)
	pre_source = replace_canonical_root_member(pre_source, root, 'v_smoke_execution', proof_smoke)
	pre_root := bin.parse_strict_json(pre_source) or { panic(err) }
	pre_projection := bin.terminal_state_projection(pre_root) or { panic(err) }

	mut current_native := canonical_root_member(root, 'native_gate_execution')
	current_native = current_native.replace_once('"expected_ledger_generation":7',
		'"expected_ledger_generation":${final_generation}')
	current_smoke := live_source_waiting_v_smoke_for(final_generation, consumer_kind,
		with_infrastructure_retry)
	last_validation := '{"run_id":${(selected_attempt.object_value('run_id') or {
		panic('source run missing')
	}).int_value},"run_attempt":${(selected_attempt.object_value('run_attempt') or {
		panic('source run attempt missing')
	}).int_value},"subject_hash":"${subject_hash}","conclusion":"blocked","evidence_digest":"${(selected_attempt.object_value('evidence_digest') or {
		panic('source evidence missing')
	}).string_value}"}'
	source_refetch, source_state_pre, source_state, source_history, source_evidence_digest, _ := live_source_evidence(waiting_consumers,
		proof_generation, repeated_daily_outage, expected_state_parent_sha,
		source_operation_ordinal, source_kind, cas_attempt)
	final_target_state := if consumer_kind == 'publish_post' { 'validating' } else { 'quarantined' }
	final_publication_state := match consumer_kind {
		'publish_post' { 'post_publish_waiting_source' }
		'rollback_post' { 'rollback_waiting_source' }
		else { 'idle' }
	}
	mut final_source := bin.canonical_json(root)
	final_source = replace_canonical_root_member(final_source, root, 'generation',
		final_generation.str())
	final_source = replace_canonical_root_member(final_source, root, 'target_state',
		'"${final_target_state}"')
	final_source = replace_canonical_root_member(final_source, root, 'publication_state',
		'"${final_publication_state}"')
	if intent.kind == .object {
		waiting_intent := bin.canonical_json(intent).replace_once('"stage":"post_checks_running"',
			'"stage":"post_checks_waiting_source"')
		final_source = replace_canonical_root_member(final_source, root, 'active_intent',
			waiting_intent)
	}
	final_source = replace_canonical_root_member(final_source, root, 'native_gate_execution',
		current_native)
	final_source = replace_canonical_root_member(final_source, root, 'v_smoke_execution',
		current_smoke)
	final_source = replace_canonical_root_member(final_source, root, 'active_recovery_handoff_id',
		'null')
	final_source = replace_canonical_root_member(final_source, root, 'last_source_refetch',
		source_refetch)
	final_source = replace_canonical_root_member(final_source, root, 'last_validation',
		last_validation)
	final_projection_root := bin.parse_strict_json(final_source) or { panic(err) }
	final_projection := bin.terminal_state_projection(final_projection_root) or { panic(err) }
	atomic_pre_source_base := if with_infrastructure_retry {
		live_recovery_h2_retry_dispatched_source_for(consumer_kind)
	} else {
		live_recovery_h2_dispatched_source_for(consumer_kind)
	}
	atomic_pre_source := live_target_for_source_kind(atomic_pre_source_base, source_kind)
	atomic_pre_root := bin.parse_strict_json(atomic_pre_source) or { panic(err) }
	atomic_pre_projection := bin.terminal_state_projection(atomic_pre_root) or { panic(err) }

	placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	native_check := live_recovery_native_check_for(subject_hash, consumer_id, native_ref)
	mut proof := '{"schema_version":5,"expected_check_sources":${bin.canonical_json(expected_sources)},"native_gate_execution":${proof_native},"native_gate_check":${native_check},"v_smoke_execution":${proof_smoke},"business_operation_id":"${live_h2_business_operation_id}","business_transition":"source_unreachable","source_refetch":${source_refetch},"source_state_pre_snapshot":${source_state_pre},"source_state_snapshot":${source_state},"source_state_cas_history":${source_history},"git_ancestry_proof":null,"source_atomic_pre_projection":${bin.canonical_json(atomic_pre_projection)},"pre_business_projection":${bin.canonical_json(pre_projection)},"final_projection":${bin.canonical_json(final_projection)},"facts_digest":"${placeholder}"}'
	proof_value := bin.parse_strict_json(proof) or { panic(err) }
	proof_digest := bin.terminal_revalidation_facts_digest(proof_value) or { panic(err) }
	proof = proof.replace_once('"facts_digest":"${placeholder}"',
		'"facts_digest":"${proof_digest}"')
	proof = bin.canonical_json(bin.parse_strict_json(proof) or { panic(err) })
	committed_smoke := (bin.parse_strict_json(proof) or { panic(err) }).object_value('v_smoke_execution') or {
		panic('source proof V smoke missing')
	}
	committed_attempts := committed_smoke.object_value('attempts') or {
		panic('source attempts missing')
	}
	first_smoke_digest := bin.v_smoke_terminal_payload_digest(committed_smoke,
		committed_attempts.array_value[0]) or { panic(err) }
	second_smoke_digest := if with_infrastructure_retry {
		bin.v_smoke_terminal_payload_digest(committed_smoke, committed_attempts.array_value[1]) or {
			panic(err)
		}
	} else {
		''
	}
	predecessor := bin.canonical_json(handoffs.array_value[0])
	completed_successor := live_recovery_source_successor_projection(root, proof,
		with_infrastructure_retry)
	operations := append_live_recovery_source_operations(canonical_root_member(root,
		'applied_operations'), successor_id, proof_digest, source_evidence_digest,
		first_smoke_digest, second_smoke_digest, with_infrastructure_retry)
	final_source = replace_canonical_root_member(final_source, root, 'recovery_handoffs',
		'[${predecessor},${completed_successor}]')
	final_source = replace_canonical_root_member(final_source, root, 'applied_operations',
		operations)
	final_source = replace_canonical_root_member(final_source, root, 'last_operation_id',
		'"${live_h2_completion_operation_id}"')
	return replace_canonical_root_member(final_source, root, 'last_transition',
		'"handoff_complete_${proof_digest}"')
}

fn live_recovery_h2_infrastructure_source_for(consumer_kind string) string {
	if consumer_kind !in ['publish_post', 'rollback_post', 'remediation'] {
		panic('unsupported infrastructure recovery consumer ${consumer_kind}')
	}
	source_waiting := live_recovery_h2_source_waiting_variant_for(consumer_kind, true, false)
	root := bin.parse_strict_json(source_waiting) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	predecessor := handoffs.array_value[0]
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or { panic('terminal proof missing') }
	pre_projection := proof.object_value('pre_business_projection') or {
		panic('pre projection missing')
	}
	old_final_projection := proof.object_value('final_projection') or {
		panic('final projection missing')
	}
	old_refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	old_source_state_pre := proof.object_value('source_state_pre_snapshot') or {
		panic('pre source state missing')
	}
	old_source_state := proof.object_value('source_state_snapshot') or {
		panic('source state missing')
	}
	old_source_history := proof.object_value('source_state_cas_history') or {
		panic('source history missing')
	}
	old_source_atomic_pre := proof.object_value('source_atomic_pre_projection') or {
		panic('source atomic pre-projection missing')
	}
	old_proof_digest := (proof.object_value('facts_digest') or {
		panic('terminal proof digest missing')
	}).string_value
	old_source_digest := (old_refetch.object_value('evidence_digest') or {
		panic('source evidence digest missing')
	}).string_value
	new_business_transition := match consumer_kind {
		'publish_post' { 'post_check_infra_exhausted' }
		'rollback_post' { 'rollback_failed' }
		else { 'remediation_red' }
	}
	new_publication_state := match consumer_kind {
		'publish_post' { 'post_publish_blocked' }
		'rollback_post' { 'rollback_blocked' }
		else { 'idle' }
	}
	mut final_source := bin.canonical_json(root)
	root_target_id := canonical_root_member(root, 'target_id')
	root_target_state := canonical_root_member(root, 'target_state')
	root_target_anchor := '"target_id":${root_target_id},"target_state":${root_target_state}'
	if final_source.count(root_target_anchor) != 1 {
		panic('canonical root target_id/target_state anchor must occur exactly once')
	}
	final_source = final_source.replace_once(root_target_anchor,
		'"target_id":${root_target_id},"target_state":"quarantined"')
	final_source = replace_canonical_root_member(final_source, root, 'publication_state',
		'"${new_publication_state}"')
	final_source = replace_canonical_root_member(final_source, root, 'last_source_refetch', bin.canonical_json(pre_projection.object_value('last_source_refetch') or {
		panic('pre source refetch missing')
	}))
	if consumer_kind in ['publish_post', 'rollback_post'] {
		intent := root.object_value('active_intent') or { panic('active intent missing') }
		blocked_intent := bin.canonical_json(intent).replace_once('"stage":"post_checks_waiting_source"',
			'"stage":"blocked"')
		final_source = replace_canonical_root_member(final_source, root, 'active_intent',
			blocked_intent)
	} else {
		final_source = replace_canonical_root_member(final_source, root, 'active_remediation_id',
			'null')
		final_source = replace_canonical_root_member(final_source, root,
			'active_remediation_binding', 'null')
		root_remediation_sources := canonical_root_member(root, 'remediation_check_sources')
		root_resolved_by := canonical_root_member(root, 'resolved_by')
		root_remediation_anchor := '"remediation_check_sources":${root_remediation_sources},"resolved_by":${root_resolved_by}'
		if final_source.count(root_remediation_anchor) != 1 {
			panic('canonical root remediation_check_sources/resolved_by anchor must occur exactly once')
		}
		final_source = final_source.replace_once(root_remediation_anchor,
			'"remediation_check_sources":[],"resolved_by":${root_resolved_by}')
		final_source = replace_canonical_root_member(final_source, root, 'native_gate_subject',
			'null')
		final_source = replace_canonical_root_member(final_source, root, 'active_subject_hash',
			'null')
		final_source = replace_canonical_root_member(final_source, root, 'native_gate_execution',
			'null')
		root_v_smoke := canonical_root_member(root, 'v_smoke_execution')
		root_v_smoke_anchor := '"target_id":${root_target_id},"target_state":"quarantined","v_smoke_execution":${root_v_smoke}'
		if final_source.count(root_v_smoke_anchor) != 1 {
			panic('canonical root target_id/target_state/v_smoke_execution anchor must occur exactly once')
		}
		final_source = final_source.replace_once(root_v_smoke_anchor,
			'"target_id":${root_target_id},"target_state":"quarantined","v_smoke_execution":null')
		final_source = replace_canonical_root_member(final_source, root,
			'post_validation_operation_id', 'null')
	}
	if consumer_kind in ['publish_post', 'remediation'] {
		final_source = replace_canonical_root_member(final_source, root, 'last_head_observation', live_terminal_head_observation(pre_projection,
			'exact_subject', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
			'9898989898989898989898989898989898989898989898989898989898989898'))
	}
	final_root := bin.parse_strict_json(final_source) or { panic(err) }
	final_projection := bin.terminal_state_projection(final_root) or { panic(err) }
	canonical_proof := bin.canonical_json(proof)
	mut updated_proof := canonical_proof.replace_once('"business_transition":"source_unreachable"',
		'"business_transition":"${new_business_transition}"')
	updated_proof = updated_proof.replace_once('"source_refetch":${bin.canonical_json(old_refetch)}',
		'"source_refetch":null')
	updated_proof = updated_proof.replace_once('"source_state_pre_snapshot":${bin.canonical_json(old_source_state_pre)}',
		'"source_state_pre_snapshot":null')
	updated_proof = updated_proof.replace_once('"source_state_snapshot":${bin.canonical_json(old_source_state)}',
		'"source_state_snapshot":null')
	updated_proof = updated_proof.replace_once('"source_state_cas_history":${bin.canonical_json(old_source_history)}',
		'"source_state_cas_history":[]')
	updated_proof = updated_proof.replace_once('"source_atomic_pre_projection":${bin.canonical_json(old_source_atomic_pre)}',
		'"source_atomic_pre_projection":null')
	updated_proof = updated_proof.replace_once('"final_projection":${bin.canonical_json(old_final_projection)}',
		'"final_projection":${bin.canonical_json(final_projection)}')
	updated_proof = updated_proof.replace_once('"facts_digest":"${old_proof_digest}"',
		'"facts_digest":"0000000000000000000000000000000000000000000000000000000000000000"')
	updated_proof_value := bin.parse_strict_json(updated_proof) or { panic(err) }
	new_proof_digest := bin.terminal_revalidation_facts_digest(updated_proof_value) or {
		panic(err)
	}
	updated_proof = updated_proof.replace_once('0000000000000000000000000000000000000000000000000000000000000000',
		new_proof_digest)
	canonical_successor := bin.canonical_json(successor)
	mut updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_successor = updated_successor.replace_once('"terminal_outcome":"source_waiting"',
		'"terminal_outcome":"infrastructure_blocked"')
	terminal_completed_at := successor.object_value('terminal_completed_at') or {
		panic('source-waiting terminal completion missing')
	}
	updated_successor = updated_successor.replace_once('"terminal_completed_at":${bin.canonical_json(terminal_completed_at)}',
		'"terminal_completed_at":null')
	final_source = replace_canonical_root_member(final_source, root, 'recovery_handoffs',
		'[${bin.canonical_json(predecessor)},${updated_successor}]')
	mut operations := canonical_root_member(root, 'applied_operations')
	operations = operations.replace_once('"transition":"source_unreachable_${old_source_digest}"',
		'"transition":"${new_business_transition}"')
	operations = operations.replace('handoff_complete_${old_proof_digest}',
		'handoff_complete_${new_proof_digest}')
	final_source = replace_canonical_root_member(final_source, root, 'applied_operations',
		operations)
	return replace_canonical_root_member(final_source, root, 'last_transition',
		'"handoff_complete_${new_proof_digest}"')
}

fn live_pending_smoke_for_subject(subject bin.JsonValue, subject_hash string,
	expected_generation i64) string {
	consumer_id := (subject.object_value('consumer_id') or { panic('consumer ID missing') }).string_value
	consumer_kind := (subject.object_value('consumer_kind') or { panic('consumer kind missing') }).string_value
	target_id := (subject.object_value('target_id') or { panic('target ID missing') }).string_value
	subject_generation := (subject.object_value('subject_generation') or {
		panic('subject generation missing')
	}).int_value
	subject_ref := (subject.object_value('original_ref') or { panic('subject ref missing') }).string_value
	subject_sha := (subject.object_value('sha') or { panic('subject SHA missing') }).string_value
	placeholder := '0000000000000000000000000000000000000000000000000000000000000000'
	mut smoke := '{"schema_version":1,"consumer_id":"${consumer_id}","consumer_kind":"${consumer_kind}","intent_or_operation_id":"${consumer_id}","target_id":"${target_id}","subject_hash":"${subject_hash}","subject_generation":${subject_generation},"subject_ref":"${subject_ref}","subject_sha":"${subject_sha}","v_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","repository":"vlang/v","workflow_id":2002,"workflow_path":".github/workflows/tccbin_revalidate.yml","workflow_ref":"master","event":"workflow_dispatch","actions_integration_id":1001,"validator_integration_id":1002,"run_name":"tccbin-v-smoke/${consumer_id}","reservation_operation_id":"${live_h2_business_operation_id}","expected_ledger_generation":${expected_generation},"state":"pending","dispatches":[],"active_dispatch":null,"active_attempt":null,"attempts":[],"run_absent_attempts":[],"infra_retry_count":0,"ack_operation_ids":[],"completion_operation_ids":[],"block_operation_id":null,"block_facts_digest":null,"block_reason":null,"blocked_at":null,"replay_facts_digest":"${placeholder}","created_at":"2026-08-03T02:00:00Z"}'
	replay_digest := bin.v_smoke_replay_facts_digest(bin.parse_strict_json(smoke) or { panic(err) }) or {
		panic(err)
	}
	return smoke.replace_once(placeholder, replay_digest)
}

fn live_recovery_h2_publish_adopt_current_source() string {
	functional := live_recovery_h2_functional_source_for('publish_post')
	root := bin.parse_strict_json(functional) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	predecessor := handoffs.array_value[0]
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or { panic('terminal proof missing') }
	pre_projection := proof.object_value('pre_business_projection') or {
		panic('pre projection missing')
	}
	old_final_projection := proof.object_value('final_projection') or {
		panic('final projection missing')
	}
	old_proof_digest := (proof.object_value('facts_digest') or {
		panic('terminal proof digest missing')
	}).string_value
	new_head := 'ffffffffffffffffffffffffffffffffffffffff'
	new_tree := 'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	mut intent := os.read_file(os.join_path(fixture_root,
		'active-intent.bootstrap.schema-fixture.json')) or { panic(err) }
	intent = intent.replace(receiver_consumer_id, live_h2_business_operation_id)
	intent = intent.replace_once('"intent_type": "initial_adopt_current"',
		'"intent_type": "adopt-current"')
	intent = intent.replace_once('"generation": 0', '"generation": 10')
	intent = intent.replace_once('"expected_canonical_head": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"expected_canonical_head": "${new_head}"')
	intent = intent.replace_once('"sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"sha": "${new_head}"')
	intent = intent.replace_once('"tree": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"',
		'"tree": "${new_tree}"')
	intent = intent.replace_once('"previous_last_known_good": null', '"previous_last_known_good": ${bin.canonical_json(pre_projection.object_value('last_known_good') or {
		panic('pre last-known-good missing')
	})}')
	canonical_intent := bin.canonical_json(bin.parse_strict_json(intent) or { panic(err) })
	mut subject_source := os.read_file(os.join_path(fixture_root,
		'native-gate-subject.schema-fixture.json')) or { panic(err) }
	subject_source = subject_source.replace(receiver_consumer_id, live_h2_business_operation_id)
	subject_source = subject_source.replace_once('"consumer_kind": "initial_adopt_current"',
		'"consumer_kind": "adopt_current"')
	subject_source = subject_source.replace_once('"subject_generation": 1',
		'"subject_generation": 11')
	subject_source = subject_source.replace_once('"sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"sha": "${new_head}"')
	subject_source = subject_source.replace_once('"tree": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"',
		'"tree": "${new_tree}"')
	subject := bin.parse_strict_json(subject_source) or { panic(err) }
	canonical_subject := bin.canonical_json(subject)
	subject_hash := bin.json_sha256(subject)
	mut native_execution := os.read_file(os.join_path(fixture_root,
		'native-gate-execution.schema-fixture.json')) or { panic(err) }
	native_execution = native_execution.replace(receiver_consumer_id, live_h2_business_operation_id)
	execution_subject_start := native_execution.index('"subject": {') or { panic(err) }
	execution_hash_start := native_execution.index('"subject_hash":') or { panic(err) }
	native_execution = native_execution[..execution_subject_start] +
		'"subject": ${canonical_subject},\n  ' + native_execution[execution_hash_start..]
	native_execution = native_execution.replace_once('"subject_hash": "d92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd"',
		'"subject_hash": "${subject_hash}"')
	native_execution = native_execution.replace_once('"subject_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"subject_sha": "${new_head}"')
	native_execution = native_execution.replace_once('"subject_generation": 1',
		'"subject_generation": 11')
	native_execution = native_execution.replace_once('"expected_ledger_generation": 1',
		'"expected_ledger_generation": 12')
	canonical_native_execution := bin.canonical_json(bin.parse_strict_json(native_execution) or {
		panic(err)
	})
	smoke := live_pending_smoke_for_subject(subject, subject_hash, 12)
	ancestry_proof := live_git_ancestry_proof(new_head)
	ancestry_value := bin.parse_strict_json(ancestry_proof) or { panic(err) }
	ancestry_digest := (ancestry_value.object_value('evidence_digest') or {
		panic('ancestry evidence digest missing')
	}).string_value
	head_observation := live_terminal_head_observation(pre_projection, 'subject_ancestor',
		new_head, ancestry_digest)
	mut final_source := bin.canonical_json(root)
	final_source = replace_canonical_root_member(final_source, root, 'canonical_observed_sha',
		'"${new_head}"')
	final_source = replace_canonical_root_member(final_source, root, 'publication_state',
		'"candidate_pending"')
	final_source = replace_canonical_root_member(final_source, root, 'active_intent',
		canonical_intent)
	final_source = replace_canonical_root_member(final_source, root, 'native_gate_subject',
		canonical_subject)
	final_source = replace_canonical_root_member(final_source, root, 'active_subject_hash',
		'"${subject_hash}"')
	final_source = replace_canonical_root_member(final_source, root, 'native_gate_execution',
		canonical_native_execution)
	root_target_id := canonical_root_member(root, 'target_id')
	root_target_state := canonical_root_member(root, 'target_state')
	root_v_smoke := canonical_root_member(root, 'v_smoke_execution')
	root_v_smoke_anchor := '"target_id":${root_target_id},"target_state":${root_target_state},"v_smoke_execution":${root_v_smoke}'
	if final_source.count(root_v_smoke_anchor) != 1 {
		panic('canonical root target_id/target_state/v_smoke_execution anchor must occur exactly once')
	}
	final_source = final_source.replace_once(root_v_smoke_anchor,
		'"target_id":${root_target_id},"target_state":${root_target_state},"v_smoke_execution":${smoke}')
	final_source = replace_canonical_root_member(final_source, root, 'last_head_observation',
		head_observation)
	final_root := bin.parse_strict_json(final_source) or { panic(err) }
	final_projection := bin.terminal_state_projection(final_root) or { panic(err) }
	canonical_proof := bin.canonical_json(proof)
	mut updated_proof := canonical_proof.replace_once('"git_ancestry_proof":null',
		'"git_ancestry_proof":${ancestry_proof}')
	updated_proof = updated_proof.replace_once('"final_projection":${bin.canonical_json(old_final_projection)}',
		'"final_projection":${bin.canonical_json(final_projection)}')
	updated_proof = updated_proof.replace_once('"facts_digest":"${old_proof_digest}"',
		'"facts_digest":"0000000000000000000000000000000000000000000000000000000000000000"')
	new_proof_digest := bin.terminal_revalidation_facts_digest(bin.parse_strict_json(updated_proof) or {
		panic(err)
	}) or { panic(err) }
	updated_proof = updated_proof.replace_once('0000000000000000000000000000000000000000000000000000000000000000',
		new_proof_digest)
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	final_source = replace_canonical_root_member(final_source, root, 'recovery_handoffs',
		'[${bin.canonical_json(predecessor)},${updated_successor}]')
	operations := canonical_root_member(root, 'applied_operations').replace('handoff_complete_${old_proof_digest}',
		'handoff_complete_${new_proof_digest}')
	final_source = replace_canonical_root_member(final_source, root, 'applied_operations',
		operations)
	return replace_canonical_root_member(final_source, root, 'last_transition',
		'"handoff_complete_${new_proof_digest}"')
}

fn live_adopt_current_waiting_source() string {
	candidate := live_recovery_h2_publish_adopt_current_source()
	root := bin.parse_strict_json(candidate) or { panic(err) }
	intent := root.object_value('active_intent') or { panic('adopt-current intent missing') }
	mut waiting_intent := bin.canonical_json(intent)
	if waiting_intent.count('"stage":"intent_reserved"') != 1 {
		panic('adopt-current reserved stage must occur exactly once')
	}
	waiting_intent = waiting_intent.replace_once('"stage":"intent_reserved"',
		'"stage":"checks_waiting_source"')
	mut source := bin.canonical_json(root)
	source = replace_canonical_root_member(source, root, 'publication_state',
		'"adopt_current_waiting_source"')
	source = replace_canonical_root_member(source, root, 'active_intent', waiting_intent)
	return replace_canonical_root_member(source, root, 'recovery_handoffs', '[]')
}

fn live_recovery_h2_historical_source() string {
	terminal := live_recovery_h2_terminal_source()
	root := bin.parse_strict_json(terminal) or { panic(err) }
	mut operations := canonical_root_member(root, 'applied_operations')
	operations = operations[..operations.len - 1] +
		',{"operation_id":"${live_h2_later_operation_id}","transition":"later_observation","resulting_generation":13}]'
	later_validation := '{"run_id":3002,"run_attempt":1,"subject_hash":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee","conclusion":"success","evidence_digest":"edededededededededededededededededededededededededededededededed"}'
	mut source := replace_canonical_root_member(bin.canonical_json(root), root, 'generation', '13')
	source = replace_canonical_root_member(source, root, 'applied_operations', operations)
	source = replace_canonical_root_member(source, root, 'last_validation', later_validation)
	source = replace_canonical_root_member(source, root, 'last_operation_id',
		'"${live_h2_later_operation_id}"')
	return replace_canonical_root_member(source, root, 'last_transition', '"later_observation"')
}

fn live_pre_subject_adoption_source() string {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	mut source := os.read_file(os.join_path(fixture_root,
		'target-state.bootstrap.schema-fixture.json')) or { panic(err) }
	intent := (os.read_file(os.join_path(fixture_root,
		'active-intent.bootstrap.schema-fixture.json')) or { panic(err) }).trim_space()
	source = source.replace_once('"target_state": "uninitialized"', '"target_state": "validating"')
	source = source.replace_once('"publication_state": "idle"',
		'"publication_state": "candidate_pending"')
	source = source.replace_once('"input_fingerprint": null',
		'"input_fingerprint": "3333333333333333333333333333333333333333333333333333333333333333"')
	source = source.replace_once('"artifact_fingerprint": null',
		'"artifact_fingerprint": "4444444444444444444444444444444444444444444444444444444444444444"')
	source = source.replace_once('"manifest_hash": null',
		'"manifest_hash": "5555555555555555555555555555555555555555555555555555555555555555"')
	source = source.replace_once('"provenance_status": null', '"provenance_status": "complete"')
	source = source.replace_once('"resolved_inputs": null',
		'"resolved_inputs": ${live_resolved_inputs()}')
	source = source.replace_once('"active_intent": null', '"active_intent": ${intent}')
	return with_pending_v_smoke(source)
}

fn live_artifact_tuple(sha string, tree string) string {
	return '{"sha":"${sha}","tree":"${tree}","input_fingerprint":"3333333333333333333333333333333333333333333333333333333333333333","artifact_fingerprint":"4444444444444444444444444444444444444444444444444444444444444444","manifest_hash":"5555555555555555555555555555555555555555555555555555555555555555","digests":[{"path":"tcc.exe","sha256":"6666666666666666666666666666666666666666666666666666666666666666"}]}'
}

fn live_candidate_binding() string {
	return '{"sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","tree":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","parent":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","artifact_fingerprint":"4444444444444444444444444444444444444444444444444444444444444444","manifest_hash":"5555555555555555555555555555555555555555555555555555555555555555","digests":[{"path":"tcc.exe","sha256":"6666666666666666666666666666666666666666666666666666666666666666"}]}'
}

fn live_publish_candidate_subject_hash() string {
	return bin.native_gate_subject_hash(bin.NativeGateSubjectModel{
		consumer_id:            receiver_consumer_id
		consumer_kind:          'publish_candidate'
		intent_or_operation_id: receiver_consumer_id
		target_id:              'linux-amd64'
		subject_generation:     1
		initial_run_mode:       'original_push'
		sha:                    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		tree:                   'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		original_ref:           'tccbin-candidate/linux-amd64/${receiver_consumer_id}'
		input_fingerprint:      '3333333333333333333333333333333333333333333333333333333333333333'
		artifact_fingerprint:   '4444444444444444444444444444444444444444444444444444444444444444'
		manifest_hash:          '5555555555555555555555555555555555555555555555555555555555555555'
		digests:                [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: '6666666666666666666666666666666666666666666666666666666666666666'
			},
		]
	}) or { panic(err) }
}

fn live_rollback_candidate_subject_hash() string {
	return bin.native_gate_subject_hash(bin.NativeGateSubjectModel{
		consumer_id:            receiver_consumer_id
		consumer_kind:          'rollback_candidate'
		intent_or_operation_id: receiver_consumer_id
		target_id:              'linux-amd64'
		subject_generation:     1
		initial_run_mode:       'original_push'
		sha:                    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		tree:                   'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		original_ref:           'tccbin-candidate/linux-amd64/${receiver_consumer_id}'
		input_fingerprint:      '3333333333333333333333333333333333333333333333333333333333333333'
		artifact_fingerprint:   '4444444444444444444444444444444444444444444444444444444444444444'
		manifest_hash:          '5555555555555555555555555555555555555555555555555555555555555555'
		digests:                [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: '6666666666666666666666666666666666666666666666666666666666666666'
			},
		]
	}) or { panic(err) }
}

fn live_historical_candidate_gate_runs() string {
	fixture := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'target-state.v-smoke-terminal-check.schema-fixture.json')) or { panic(err) }
	root := bin.parse_strict_json(fixture) or { panic(err) }
	intent := root.object_value('active_intent') or { panic('active intent missing') }
	gates := intent.object_value('gate_runs') or { panic('candidate gates missing') }
	old_hash := 'd92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd'
	new_hash := live_publish_candidate_subject_hash()
	old_native_external := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		receiver_consumer_id, old_hash, 7001, 1) or { panic(err) }
	new_native_external := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		receiver_consumer_id, new_hash, 7001, 1) or { panic(err) }
	old_v_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		receiver_consumer_id, old_hash, 3001, 1) or { panic(err) }
	new_v_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		receiver_consumer_id, new_hash, 3001, 1) or { panic(err) }
	return bin.canonical_json(gates).replace(old_hash, new_hash).replace(old_native_external,
		new_native_external).replace(old_v_external, new_v_external)
}

fn live_publish_post_subject_hash() string {
	return bin.native_gate_subject_hash(bin.NativeGateSubjectModel{
		consumer_id:            live_post_operation_id
		consumer_kind:          'publish_post'
		intent_or_operation_id: live_post_operation_id
		target_id:              'linux-amd64'
		subject_generation:     1
		initial_run_mode:       'original_push'
		sha:                    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		tree:                   'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		original_ref:           'thirdparty-linux-amd64'
		input_fingerprint:      '3333333333333333333333333333333333333333333333333333333333333333'
		artifact_fingerprint:   '4444444444444444444444444444444444444444444444444444444444444444'
		manifest_hash:          '5555555555555555555555555555555555555555555555555555555555555555'
		digests:                [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: '6666666666666666666666666666666666666666666666666666666666666666'
			},
		]
	}) or { panic(err) }
}

fn live_resolved_inputs() string {
	fixture := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'active-intent.bootstrap.schema-fixture.json')) or { panic(err) }
	intent := bin.parse_strict_json(fixture) or { panic(err) }
	resolved := intent.object_value('resolved_inputs') or { panic('resolved inputs missing') }
	return bin.canonical_json(resolved)
}

fn live_rollback_post_subject_hash() string {
	return bin.native_gate_subject_hash(bin.NativeGateSubjectModel{
		consumer_id:            live_post_operation_id
		consumer_kind:          'rollback_post'
		intent_or_operation_id: live_post_operation_id
		target_id:              'linux-amd64'
		subject_generation:     1
		initial_run_mode:       'original_push'
		sha:                    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		tree:                   'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		original_ref:           'thirdparty-linux-amd64'
		input_fingerprint:      '3333333333333333333333333333333333333333333333333333333333333333'
		artifact_fingerprint:   '4444444444444444444444444444444444444444444444444444444444444444'
		manifest_hash:          '5555555555555555555555555555555555555555555555555555555555555555'
		digests:                [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: '6666666666666666666666666666666666666666666666666666666666666666'
			},
		]
	}) or { panic(err) }
}

fn live_publish_post_source() string {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	mut intent := os.read_file(os.join_path(fixture_root,
		'active-intent.bootstrap.schema-fixture.json')) or { panic(err) }
	intent = intent.replace_once('"intent_type": "initial_adopt_current"',
		'"intent_type": "publish"')
	intent = intent.replace_once('"stage": "intent_reserved"', '"stage": "post_checks_running"')
	intent = intent.replace_once('"candidate_binding": null',
		'"candidate_binding": ${live_candidate_binding()}')
	intent = intent.replace_once('"gate_runs": []',
		'"gate_runs": ${live_historical_candidate_gate_runs()}')
	validation_start := intent.index('"validation_subject": {') or { panic(err) }
	previous_start := intent.index('"previous_last_known_good": null') or { panic(err) }
	intent = intent[..validation_start] + '"validation_subject": null,\n  ' +
		intent[previous_start..]
	intent = intent.replace_once('"previous_last_known_good": null', '"previous_last_known_good": ${live_artifact_tuple('cccccccccccccccccccccccccccccccccccccccc',
		'dddddddddddddddddddddddddddddddddddddddd')}')

	mut subject := os.read_file(os.join_path(fixture_root,
		'native-gate-subject.schema-fixture.json')) or { panic(err) }
	subject = subject.replace(receiver_consumer_id, live_post_operation_id)
	subject = subject.replace_once('"consumer_kind": "initial_adopt_current"',
		'"consumer_kind": "publish_post"')
	subject = subject.replace_once('"original_ref": "tccbin-candidate/linux-amd64/${live_post_operation_id}"',
		'"original_ref": "thirdparty-linux-amd64"')
	mut execution := os.read_file(os.join_path(fixture_root,
		'native-gate-execution.schema-fixture.json')) or { panic(err) }
	execution_subject_start := execution.index('"subject": {') or { panic(err) }
	execution_hash_start := execution.index('"subject_hash":') or { panic(err) }
	execution = execution[..execution_subject_start] + '"subject": ${subject.trim_space()},\n  ' +
		execution[execution_hash_start..]
	post_hash := live_publish_post_subject_hash()
	execution = execution.replace_once('"subject_hash": "d92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd"',
		'"subject_hash": "${post_hash}"')
	execution = execution.replace_once('"expected_ref": "tccbin-candidate/linux-amd64/${receiver_consumer_id}"',
		'"expected_ref": "thirdparty-linux-amd64"')

	mut source := live_pre_subject_adoption_source()
	intent_start := source.index('"active_intent": {') or { panic(err) }
	post_operation_start := source.index('"post_validation_operation_id":') or { panic(err) }
	source = source[..intent_start] + '"active_intent": ${intent.trim_space()},\n  ' +
		source[post_operation_start..]
	source = source.replace_once('"generation": 0', '"generation": 1')
	source = source.replace_once('"target_state": "uninitialized"', '"target_state": "validating"')
	source = source.replace_once('"publication_state": "candidate_pending"',
		'"publication_state": "post_publish_validating"')
	source = source.replace_once('"bootstrap_required": true', '"bootstrap_required": false')
	source = source.replace_once('"last_known_good": null', '"last_known_good": ${live_artifact_tuple('cccccccccccccccccccccccccccccccccccccccc',
		'dddddddddddddddddddddddddddddddddddddddd')}')
	source = source.replace_once('"provisional_published": null', '"provisional_published": ${live_artifact_tuple('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
		'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb')}')
	source = source.replace_once('"resolved_inputs": null',
		'"resolved_inputs": ${live_resolved_inputs()}')
	source = source.replace_once('"post_validation_operation_id": null',
		'"post_validation_operation_id": "${live_post_operation_id}"')
	source = source.replace_once('"native_gate_subject": null',
		'"native_gate_subject": ${subject.trim_space()}')
	source = source.replace_once('"active_subject_hash": null',
		'"active_subject_hash": "${post_hash}"')
	source = source.replace_once('"native_gate_execution": null',
		'"native_gate_execution": ${execution.trim_space()}')
	return with_pending_v_smoke(source)
}

fn live_rollback_post_source() string {
	mut source := live_publish_post_source()
	old_post_hash := live_publish_post_subject_hash()
	new_post_hash := live_rollback_post_subject_hash()
	old_candidate_hash := live_publish_candidate_subject_hash()
	new_candidate_hash := live_rollback_candidate_subject_hash()
	old_native_external := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		receiver_consumer_id, old_candidate_hash, 7001, 1) or { panic(err) }
	new_native_external := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		receiver_consumer_id, new_candidate_hash, 7001, 1) or { panic(err) }
	old_v_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		receiver_consumer_id, old_candidate_hash, 3001, 1) or { panic(err) }
	new_v_external := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		receiver_consumer_id, new_candidate_hash, 3001, 1) or { panic(err) }
	source = source.replace_once('"intent_type": "publish"', '"intent_type": "rollback"')
	source = source.replace('"consumer_kind": "publish_post"', '"consumer_kind": "rollback_post"')
	source = source.replace('"consumer_kind":"publish_post"', '"consumer_kind":"rollback_post"')
	source = source.replace_once('"publication_state": "post_publish_validating"',
		'"publication_state": "rollback_pending"')
	source = source.replace_once('"target_state": "validating"', '"target_state": "quarantined"')
	source = source.replace_once('"bad_provisional": null', '"bad_provisional": ${live_artifact_tuple('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
		'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb')}')
	source = source.replace_once('"rollback_diff_fingerprint": null',
		'"rollback_diff_fingerprint": "7979797979797979797979797979797979797979797979797979797979797979"')
	source = source.replace_once('"rollback_provisional": null',
		'"rollback_provisional": ${live_candidate_binding()}')
	source = source.replace_once('"transition":"promotion_confirmed"',
		'"transition":"rollback_promoted"')
	source = source.replace_once('"last_transition": "promotion_confirmed"',
		'"last_transition": "rollback_promoted"')
	source = source.replace(old_post_hash, new_post_hash)
	source = source.replace(old_candidate_hash, new_candidate_hash)
	source = source.replace(old_native_external, new_native_external)
	source = source.replace(old_v_external, new_v_external)
	root := bin.parse_strict_json(source) or { panic(err) }
	smoke := root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	old_replay := smoke.object_value('replay_facts_digest') or { panic('replay digest missing') }
	new_replay := bin.v_smoke_replay_facts_digest(smoke) or { panic(err) }
	return source.replace_once('"replay_facts_digest":"${old_replay.string_value}"',
		'"replay_facts_digest":"${new_replay}"')
}

fn live_remediation_check_sources() string {
	return '[{"name":"tccbin-candidate-gate","repository":"vlang/tccbin","integration_id":1001,"workflow_id":2001,"workflow_path":".github/workflows/build-and-test.yml","event":"push"},{"name":"v-candidate-smoke","repository":"vlang/v","integration_id":1002,"workflow_id":2002,"workflow_path":".github/workflows/tccbin_revalidate.yml","event":"workflow_dispatch"}]'
}

fn live_remediation_binding() string {
	return '{"operation_id":"${live_remediation_operation_id}","subject_generation":1,"validation_subject":{"sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","tree":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","input_fingerprint":"3333333333333333333333333333333333333333333333333333333333333333","artifact_fingerprint":"4444444444444444444444444444444444444444444444444444444444444444","manifest_hash":"5555555555555555555555555555555555555555555555555555555555555555","digests":[{"path":"tcc.exe","sha256":"6666666666666666666666666666666666666666666666666666666666666666"}],"candidate_ref":"thirdparty-linux-amd64"},"remediation_trigger":{"repository":"vlang/v","ref":"master","before":"cccccccccccccccccccccccccccccccccccccccc","after":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","tree":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","diff_fingerprint":"7777777777777777777777777777777777777777777777777777777777777777","owner_domain":"v"},"v_source_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","expected_check_sources":${live_remediation_check_sources()}}'
}

fn live_remediation_subject() bin.NativeGateSubjectModel {
	return bin.NativeGateSubjectModel{
		consumer_id:            live_remediation_operation_id
		consumer_kind:          'remediation'
		intent_or_operation_id: live_remediation_operation_id
		target_id:              'linux-amd64'
		subject_generation:     1
		initial_run_mode:       'no_native_push_expected'
		remediation_trigger:    bin.RemediationTriggerModel{
			repository:       'vlang/v'
			ref:              'master'
			before:           'cccccccccccccccccccccccccccccccccccccccc'
			after:            'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
			tree:             'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
			diff_fingerprint: '7777777777777777777777777777777777777777777777777777777777777777'
			owner_domain:     'v'
		}
		sha:                    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		tree:                   'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		original_ref:           'thirdparty-linux-amd64'
		input_fingerprint:      '3333333333333333333333333333333333333333333333333333333333333333'
		artifact_fingerprint:   '4444444444444444444444444444444444444444444444444444444444444444'
		manifest_hash:          '5555555555555555555555555555555555555555555555555555555555555555'
		digests:                [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: '6666666666666666666666666666666666666666666666666666666666666666'
			},
		]
	}
}

fn live_remediation_source() string {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	subject_model := live_remediation_subject()
	mut subject := os.read_file(os.join_path(fixture_root,
		'native-gate-subject.schema-fixture.json')) or { panic(err) }
	subject = subject.replace(receiver_consumer_id, live_remediation_operation_id)
	subject = subject.replace_once('"consumer_kind": "initial_adopt_current"',
		'"consumer_kind": "remediation"')
	subject = subject.replace_once('"initial_run_mode": "original_push"',
		'"initial_run_mode": "no_native_push_expected"')
	subject = subject.replace_once('"remediation_trigger": null',
		'"remediation_trigger": {"repository":"vlang/v","ref":"master","before":"cccccccccccccccccccccccccccccccccccccccc","after":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","tree":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","diff_fingerprint":"7777777777777777777777777777777777777777777777777777777777777777","owner_domain":"v"}')
	subject = subject.replace_once('"original_ref": "tccbin-candidate/linux-amd64/${live_remediation_operation_id}"',
		'"original_ref": "thirdparty-linux-amd64"')
	mut execution := os.read_file(os.join_path(fixture_root,
		'native-gate-execution.schema-fixture.json')) or { panic(err) }
	execution_subject_start := execution.index('"subject": {') or { panic(err) }
	execution_hash_start := execution.index('"subject_hash":') or { panic(err) }
	execution = execution[..execution_subject_start] + '"subject": ${subject.trim_space()},\n  ' +
		execution[execution_hash_start..]
	subject_hash := bin.native_gate_subject_hash(subject_model) or { panic(err) }
	trigger_id := bin.deterministic_gate_trigger_id(live_remediation_operation_id, 0,
		'initial-v-remediation', '', 0) or { panic(err) }
	execution = execution.replace_once('"subject_hash": "d92d02fd9ab49678ad2957e36da68e91db51a3e7a42de837e3c0693b2b38f8fd"',
		'"subject_hash": "${subject_hash}"')
	execution = execution.replace_once('"reason": "original_push"',
		'"reason": "initial-v-remediation"')
	execution = execution.replace_once('"expected_ref": "tccbin-candidate/linux-amd64/${receiver_consumer_id}"',
		'"expected_ref": "tccbin-gate-trigger/linux-amd64/${live_remediation_operation_id}/${trigger_id}"')
	execution = execution.replace_once('"trigger_id": null', '"trigger_id": "${trigger_id}"')
	mut source := os.read_file(os.join_path(fixture_root,
		'target-state.bootstrap.schema-fixture.json')) or { panic(err) }
	source = source.replace_once('"generation": 0', '"generation": 1')
	source = source.replace_once('"target_state": "uninitialized"', '"target_state": "validating"')
	source = source.replace_once('"input_fingerprint": null',
		'"input_fingerprint": "${subject_model.input_fingerprint}"')
	source = source.replace_once('"artifact_fingerprint": null',
		'"artifact_fingerprint": "${subject_model.artifact_fingerprint}"')
	source = source.replace_once('"manifest_hash": null',
		'"manifest_hash": "${subject_model.manifest_hash}"')
	source = source.replace_once('"provenance_status": null', '"provenance_status": "complete"')
	source = source.replace_once('"resolved_inputs": null',
		'"resolved_inputs": ${live_resolved_inputs()}')
	source = source.replace_once('"native_gate_subject": null',
		'"native_gate_subject": ${subject.trim_space()}')
	source = source.replace_once('"active_subject_hash": null',
		'"active_subject_hash": "${subject_hash}"')
	source = source.replace_once('"native_gate_execution": null',
		'"native_gate_execution": ${execution.trim_space()}')
	source = source.replace_once('"active_remediation_id": null',
		'"active_remediation_id": "${live_remediation_operation_id}"')
	source = source.replace_once('"active_remediation_binding": null',
		'"active_remediation_binding": ${live_remediation_binding()}')
	source = source.replace_once('"remediation_check_sources": []',
		'"remediation_check_sources": ${live_remediation_check_sources()}')
	return with_pending_v_smoke(source)
}

fn replace_nth_json_digest(source string, key string, ordinal int, old_digest string, new_digest string) string {
	if ordinal < 1 {
		panic('digest replacement ordinal must be positive')
	}
	pretty_marker := '"${key}": "'
	compact_marker := '"${key}":"'
	mut offset := 0
	for current in 1 .. ordinal + 1 {
		pretty_start := source.index_after(pretty_marker, offset) or { -1 }
		compact_start := source.index_after(compact_marker, offset) or { -1 }
		marker_start := if pretty_start >= 0 && (compact_start < 0 || pretty_start < compact_start) {
			pretty_start
		} else if compact_start >= 0 {
			compact_start
		} else {
			panic('${key} digest occurrence ${ordinal} missing')
		}
		marker := if marker_start == pretty_start { pretty_marker } else { compact_marker }
		value_start := marker_start + marker.len
		value_end := source.index_after('"', value_start) or {
			panic('${key} digest occurrence ${ordinal} is open')
		}
		if current == ordinal {
			selected := source[value_start..value_end]
			if selected != old_digest {
				panic('${key} digest occurrence ${ordinal} does not match its parsed projection')
			}
			return source[..value_start] + new_digest + source[value_end..]
		}
		offset = value_end + 1
	}
	panic('unreachable digest replacement')
}

fn replace_json_digest_all(source string, key string, old_digest string, new_digest string) string {
	mut result := source
	result = result.replace('"${key}": "${old_digest}"', '"${key}": "${new_digest}"')
	result = result.replace('"${key}":"${old_digest}"', '"${key}":"${new_digest}"')
	return result
}

fn replace_json_object_digest(source string, object bin.JsonValue, key string, old_digest string,
	new_digest string) string {
	canonical_object := bin.canonical_json(object)
	if !source.contains(canonical_object) {
		panic('${key} object projection missing from canonical JSON path')
	}
	updated_object := replace_nth_json_digest(canonical_object, key, 1, old_digest, new_digest)
	if updated_object == canonical_object {
		return source
	}
	updated_source := source.replace_once(canonical_object, updated_object)
	if updated_source == source {
		panic('${key} object projection missing from canonical JSON path')
	}
	return updated_source
}

// refresh_v_smoke_facts_digests keeps negative semantic mutations discriminating: each
// dependent dispatch/run-absent/ACK/completion/block/replay digest is recomputed after the fact
// under test changes.
fn refresh_v_smoke_facts_digests(source string) string {
	parsed := bin.parse_strict_json(source) or { panic(err) }
	mut result := bin.canonical_json(parsed)
	mut root := bin.parse_strict_json(result) or { panic(err) }
	mut smoke := root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	dispatches := smoke.object_value('dispatches') or { panic('dispatch reservations missing') }
	for dispatch in dispatches.array_value {
		old_digest := dispatch.object_value('facts_digest') or {
			panic('dispatch facts digest missing')
		}
		new_digest := bin.v_smoke_dispatch_facts_digest(smoke, dispatch) or { panic(err) }
		result = replace_json_object_digest(result, dispatch, 'facts_digest',
			old_digest.string_value, new_digest)
		result = replace_json_digest_all(result, 'dispatch_facts_digest', old_digest.string_value,
			new_digest)
	}
	root = bin.parse_strict_json(result) or { panic(err) }
	smoke = root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	run_absent_attempts := smoke.object_value('run_absent_attempts') or {
		panic('run-absent attempts missing')
	}
	for run_absent in run_absent_attempts.array_value {
		old_digest := run_absent.object_value('facts_digest') or {
			panic('run-absent facts digest missing')
		}
		new_digest := bin.v_smoke_run_absent_facts_digest(smoke, run_absent) or { panic(err) }
		result = replace_json_object_digest(result, run_absent, 'facts_digest',
			old_digest.string_value, new_digest)
	}
	root = bin.parse_strict_json(result) or { panic(err) }
	smoke = root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	attempts := smoke.object_value('attempts') or { panic('V smoke attempts missing') }
	for attempt in attempts.array_value {
		old_digest := attempt.object_value('ack_facts_digest') or { panic('ACK digest missing') }
		new_digest := bin.v_smoke_ack_facts_digest(smoke, attempt) or { panic(err) }
		result = replace_json_object_digest(result, attempt, 'ack_facts_digest',
			old_digest.string_value, new_digest)
	}
	root = bin.parse_strict_json(result) or { panic(err) }
	smoke = root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	updated_attempts := smoke.object_value('attempts') or { panic('V smoke attempts missing') }
	for attempt in updated_attempts.array_value {
		old_digest := attempt.object_value('completion_facts_digest') or {
			panic('completion digest missing')
		}
		if old_digest.kind != .null_value {
			new_digest := bin.v_smoke_completion_facts_digest(smoke, attempt) or { panic(err) }
			result = replace_json_object_digest(result, attempt, 'completion_facts_digest',
				old_digest.string_value, new_digest)
		}
	}
	root = bin.parse_strict_json(result) or { panic(err) }
	smoke = root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	block_digest := smoke.object_value('block_facts_digest') or { panic('block digest missing') }
	if block_digest.kind != .null_value {
		new_digest := bin.v_smoke_block_facts_digest(smoke) or { panic(err) }
		result = replace_nth_json_digest(result, 'block_facts_digest', 1,
			block_digest.string_value, new_digest)
	}
	root = bin.parse_strict_json(result) or { panic(err) }
	smoke = root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	old_replay := smoke.object_value('replay_facts_digest') or { panic('replay digest missing') }
	new_replay := bin.v_smoke_replay_facts_digest(smoke) or { panic(err) }
	return replace_nth_json_digest(result, 'replay_facts_digest', 1, old_replay.string_value,
		new_replay)
}

fn transient_source_attempt(ordinal int, delay int) bin.SourceResolutionAttempt {
	return bin.SourceResolutionAttempt{
		ordinal:                 ordinal
		backoff_seconds:         delay
		connect_timeout_seconds: bin.source_connect_timeout_seconds
		total_timeout_seconds:   bin.source_total_timeout_seconds
		failure_kind:            .timeout
	}
}
