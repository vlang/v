module tests

import crypto.sha256
import tccbin_automation.bin

const durable_evidence_test_operation = '9191919191919191919191919191919191919191919191919191919191919191'
const durable_evidence_test_subject = '8181818181818181818181818181818181818181818181818181818181818181'
const durable_evidence_test_input = '7171717171717171717171717171717171717171717171717171717171717171'
const durable_evidence_test_artifact = '6161616161616161616161616161616161616161616161616161616161616161'
const durable_evidence_test_target_digest = '5151515151515151515151515151515151515151515151515151515151515151'

fn durable_evidence_invocation() bin.DurableTargetPlanInvocation {
	return bin.DurableTargetPlanInvocation{
		source_id:         'tinycc'
		run_id:            7001
		run_attempt:       2
		operation_ordinal: 4
		workflow:          '.github/workflows/tccbin_automation.yml'
		workflow_sha:      'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		observed_at:       '2026-08-18T12:34:56Z'
	}
}

fn durable_evidence_prepare(invocation bin.DurableTargetPlanInvocation, transition string,
	result string) !bin.DurableTargetEvidenceTestObservation {
	return bin.prepare_durable_target_evidence_for_test(automation_root(), invocation,
		durable_evidence_test_operation, transition, 'linux-amd64', durable_evidence_test_subject,
		durable_evidence_test_input, durable_evidence_test_artifact, 9, result,
		durable_evidence_test_target_digest)
}

fn durable_evidence_assert_rejected(invocation bin.DurableTargetPlanInvocation,
	transition string, result string, expected string) {
	durable_evidence_prepare(invocation, transition, result) or {
		assert err.msg() == expected, 'expected `${expected}`, got `${err.msg()}`'
		return
	}
	panic('private durable evidence builder unexpectedly accepted ${expected}')
}

fn test_durable_target_evidence_is_exact20_canonical_and_deterministic() {
	first := durable_evidence_prepare(durable_evidence_invocation(),
		'ledger_repaired_without_blockers', 'passed') or { panic(err) }
	second := durable_evidence_prepare(durable_evidence_invocation(),
		'ledger_repaired_without_blockers', 'passed') or { panic(err) }
	assert first == second
	assert first.operation_id == durable_evidence_test_operation
	assert first.path == 'evidence/2026/08/7001/2/linux-amd64/${durable_evidence_test_operation}/10-ledger_repaired_without_blockers-${durable_evidence_test_subject}.json'
	assert first.path.len <= 512
	assert first.source.len <= 256 * 1024
	assert !first.source.ends_with('\n')
	assert first.sha256 == sha256.sum256(first.source.bytes()).hex()
	root := bin.parse_strict_json(first.source) or { panic(err) }
	assert root.object_keys.len == 20
	mut expected_keys := ['schema_version', 'operation_id', 'operation_ordinal', 'cas_attempt',
		'run_id', 'run_attempt', 'intent_id', 'transition', 'workflow', 'workflow_ref',
		'workflow_sha', 'subject_id', 'subject_fingerprint', 'target_id', 'input_fingerprint',
		'artifact_fingerprint', 'generation_read', 'generation_written', 'result', 'digests']
	expected_keys.sort()
	assert root.object_keys == expected_keys
	assert bin.canonical_json(root) == first.source
	digests := (root.object_value('digests') or { panic('digests') }).array_value
	assert digests.len == 1
	assert (digests[0].object_value('path') or { panic('path') }).string_value == 'targets/linux-amd64.json'
	assert (digests[0].object_value('sha256') or { panic('sha256') }).string_value == durable_evidence_test_target_digest
}

fn test_durable_target_evidence_closes_lane_timestamp_and_invocation_inputs() {
	blocked := durable_evidence_prepare(durable_evidence_invocation(),
		'ledger_repaired_with_blockers', 'blocked') or { panic(err) }
	assert blocked.source.contains('"result":"blocked"')
	mut next_run := durable_evidence_invocation()
	next_run = bin.DurableTargetPlanInvocation{
		...next_run
		run_id: 7002
	}
	run_shifted := durable_evidence_prepare(next_run, 'ledger_repaired_with_blockers', 'blocked') or {
		panic(err)
	}
	assert run_shifted.source != blocked.source && run_shifted.path != blocked.path
	mut next_attempt := durable_evidence_invocation()
	next_attempt = bin.DurableTargetPlanInvocation{
		...next_attempt
		run_attempt: 3
	}
	attempt_shifted := durable_evidence_prepare(next_attempt, 'ledger_repaired_with_blockers',
		'blocked') or { panic(err) }
	assert attempt_shifted.source != blocked.source && attempt_shifted.path != blocked.path
	mut next_ordinal := durable_evidence_invocation()
	next_ordinal = bin.DurableTargetPlanInvocation{
		...next_ordinal
		operation_ordinal: 5
	}
	assert (durable_evidence_prepare(next_ordinal, 'ledger_repaired_with_blockers', 'blocked') or {
		panic(err)
	}).source != blocked.source
	mut next_workflow := durable_evidence_invocation()
	next_workflow = bin.DurableTargetPlanInvocation{
		...next_workflow
		workflow: '.github/workflows/tccbin_automation_recovery.yml'
	}
	assert (durable_evidence_prepare(next_workflow, 'ledger_repaired_with_blockers', 'blocked') or {
		panic(err)
	}).source != blocked.source
	mut next_workflow_sha := durable_evidence_invocation()
	next_workflow_sha = bin.DurableTargetPlanInvocation{
		...next_workflow_sha
		workflow_sha: 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
	}
	assert (durable_evidence_prepare(next_workflow_sha, 'ledger_repaired_with_blockers', 'blocked') or {
		panic(err)
	}).source != blocked.source
	mut next_month := durable_evidence_invocation()
	next_month = bin.DurableTargetPlanInvocation{
		...next_month
		observed_at: '2026-09-18T12:34:56Z'
	}
	shifted := durable_evidence_prepare(next_month, 'ledger_repaired_with_blockers', 'blocked') or {
		panic(err)
	}
	assert shifted.path.starts_with('evidence/2026/09/')
	assert shifted.path != blocked.path
	assert shifted.source == blocked.source
	mut next_subject := bin.prepare_durable_target_evidence_for_test(automation_root(),
		durable_evidence_invocation(), durable_evidence_test_operation,
		'ledger_repaired_with_blockers', 'linux-amd64', '9'.repeat(64),
		durable_evidence_test_input, durable_evidence_test_artifact, 9, 'blocked',
		durable_evidence_test_target_digest) or { panic(err) }
	assert next_subject.source != blocked.source && next_subject.path != blocked.path
	next_subject = bin.prepare_durable_target_evidence_for_test(automation_root(),
		durable_evidence_invocation(), durable_evidence_test_operation,
		'ledger_repaired_with_blockers', 'linux-amd64', durable_evidence_test_subject,
		'2'.repeat(64), '3'.repeat(64), 10, 'blocked', '4'.repeat(64)) or { panic(err) }
	assert next_subject.source != blocked.source && next_subject.path != blocked.path
	durable_evidence_assert_rejected(durable_evidence_invocation(),
		'ledger_repaired_with_blockers', 'passed',
		'durable target evidence result differs from its ledger-repair lane')
	durable_evidence_assert_rejected(durable_evidence_invocation(), 'start_build', 'passed',
		'durable target evidence result differs from its ledger-repair lane')
	mut invalid_timestamp := durable_evidence_invocation()
	invalid_timestamp = bin.DurableTargetPlanInvocation{
		...invalid_timestamp
		observed_at: '2026-02-30T12:34:56Z'
	}
	durable_evidence_assert_rejected(invalid_timestamp, 'ledger_repaired_without_blockers',
		'passed', 'durable target invocation observed_at is not canonical UTC RFC3339 seconds')
}
