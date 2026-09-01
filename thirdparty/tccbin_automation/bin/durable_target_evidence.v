module bin

import crypto.sha256
import os

const durable_evidence_max_bytes = 256 * 1024
const durable_evidence_path_max_bytes = 512

const durable_evidence_member_names = ['schema_version', 'operation_id', 'operation_ordinal',
	'cas_attempt', 'run_id', 'run_attempt', 'intent_id', 'transition', 'workflow', 'workflow_ref',
	'workflow_sha', 'subject_id', 'subject_fingerprint', 'target_id', 'input_fingerprint',
	'artifact_fingerprint', 'generation_read', 'generation_written', 'result', 'digests']

// DurableTargetPlanInvocation is caller-supplied identity material, not an authenticated Actions
// observation. The planner commits every field into its plan subject before deriving an operation
// ID. It accepts neither a TransitionContext nor an operation-ID slot.
pub struct DurableTargetPlanInvocation {
pub:
	source_id         string
	run_id            i64
	run_attempt       int
	operation_ordinal int
	workflow          string
	workflow_sha      string
	observed_at       string
}

struct DurableTargetEvidenceRequest {
	invocation           DurableTargetPlanInvocation
	operation_id         string
	transition           string
	subject_id           string
	subject_fingerprint  string
	input_fingerprint    string
	artifact_fingerprint string
	generation_read      i64
	generation_written   i64
	result               string
	target_path          string
	target_sha256        string
}

struct PreparedDurableTargetEvidence {
	path         string
	operation_id string
	transition   string
	subject_hash string
	source       string
	sha256       string
	blob_oid     string
	root         JsonValue
}

fn validate_durable_plan_invocation(invocation DurableTargetPlanInvocation) ! {
	if invocation.source_id == '' || !safe_path_segment(invocation.source_id)
		|| invocation.run_id <= 0 || invocation.run_attempt <= 0 || invocation.operation_ordinal < 0
		|| !contract_relative_path_is_safe(invocation.workflow)
		|| !is_lower_hex_40(invocation.workflow_sha) {
		return error('durable target invocation is incomplete or outside its closed contract')
	}
	exact_timestamp_unix(invocation.observed_at) or {
		return error('durable target invocation observed_at is not canonical UTC RFC3339 seconds')
	}
}

fn prepare_durable_target_evidence(automation_root string,
	request DurableTargetEvidenceRequest) !PreparedDurableTargetEvidence {
	validate_durable_plan_invocation(request.invocation)!
	if !is_lower_hex_64(request.operation_id) || !safe_path_segment(request.transition)
		|| !safe_path_segment(request.subject_id) || !is_lower_hex_64(request.subject_fingerprint)
		|| !is_lower_hex_64(request.input_fingerprint)
		|| !is_lower_hex_64(request.artifact_fingerprint) || request.generation_read < 0
		|| request.generation_written != request.generation_read + 1
		|| request.result !in ['passed', 'blocked']
		|| request.target_path != target_state_path(request.subject_id)!
		|| !is_lower_hex_64(request.target_sha256) {
		return error('durable target evidence identity is incomplete or inconsistent')
	}
	if (request.transition == 'ledger_repaired_with_blockers' && request.result != 'blocked')
		|| (request.transition == 'ledger_repaired_without_blockers' && request.result != 'passed')
		|| request.transition !in ['ledger_repaired_with_blockers', 'ledger_repaired_without_blockers'] {
		return error('durable target evidence result differs from its ledger-repair lane')
	}
	digest := object_value_from_pairs(['path', 'sha256'], [
		durable_json_string(request.target_path),
		durable_json_string(request.target_sha256),
	])!
	root := object_value_from_pairs(durable_evidence_member_names, [
		durable_json_integer(1),
		durable_json_string(request.operation_id),
		durable_json_integer(i64(request.invocation.operation_ordinal)),
		durable_json_integer(1),
		durable_json_integer(request.invocation.run_id),
		durable_json_integer(i64(request.invocation.run_attempt)),
		durable_json_null(),
		durable_json_string(request.transition),
		durable_json_string(request.invocation.workflow),
		durable_json_string('master'),
		durable_json_string(request.invocation.workflow_sha),
		durable_json_string(request.subject_id),
		durable_json_string(request.subject_fingerprint),
		durable_json_string(request.subject_id),
		durable_json_string(request.input_fingerprint),
		durable_json_string(request.artifact_fingerprint),
		durable_json_integer(request.generation_read),
		durable_json_integer(request.generation_written),
		durable_json_string(request.result),
		durable_json_array([digest]),
	])!
	if root.object_keys != durable_evidence_member_names || root.object_keys.len != 20 {
		return error('durable target evidence is not the exact closed twenty-member object')
	}
	source := canonical_json(root)
	if source.len == 0 || source.len > durable_evidence_max_bytes || source.ends_with('\n')
		|| source.ends_with('\r') {
		return error('durable target evidence bytes are empty, oversized, or non-canonical')
	}
	reparsed := parse_strict_json(source)!
	if !json_equal(reparsed, root) || canonical_json(reparsed) != source {
		return error('durable target evidence does not round-trip as exact JCS')
	}
	issues := validate_json_value(os.join_path(automation_root, 'schemas', 'evidence.schema.json'),
		reparsed)!
	if issues.len != 0 {
		return error('durable target evidence failed its authoritative schema')
	}
	year := request.invocation.observed_at[..4].int()
	month := request.invocation.observed_at[5..7].int()
	path := evidence_path(year, month, request.invocation.run_id, request.invocation.run_attempt,
		request.subject_id, request.operation_id, request.generation_written, request.transition,
		request.subject_fingerprint)!
	if path.len == 0 || path.len > durable_evidence_path_max_bytes
		|| !contract_relative_path_is_safe(path) {
		return error('durable target evidence path exceeds its closed safe bound')
	}
	validate_live_evidence_value(automation_root, reparsed, path) or {
		return error('durable target evidence body and injective path do not revalidate together')
	}
	return PreparedDurableTargetEvidence{
		path:         path
		operation_id: request.operation_id
		transition:   request.transition
		subject_hash: request.subject_fingerprint
		source:       source
		sha256:       sha256.sum256(source.bytes()).hex()
		blob_oid:     git_blob_oid(source.bytes())
		root:         reparsed
	}
}

fn durable_json_null() JsonValue {
	return JsonValue{
		kind: .null_value
	}
}

fn durable_json_bool(value bool) JsonValue {
	return JsonValue{
		kind:       .boolean
		bool_value: value
	}
}

fn durable_json_integer(value i64) JsonValue {
	return JsonValue{
		kind:      .integer
		int_value: value
	}
}

fn durable_json_string(value string) JsonValue {
	return JsonValue{
		kind:         .string_value
		string_value: value
	}
}

fn durable_json_array(values []JsonValue) JsonValue {
	return JsonValue{
		kind:        .array
		array_value: values.clone()
	}
}

$if test {
	// The observation returned by this seam is detached and accepted by no production consumer.
	pub struct DurableTargetEvidenceTestObservation {
	pub:
		path         string
		operation_id string
		source       string
		sha256       string
		blob_oid     string
	}

	pub fn prepare_durable_target_evidence_for_test(automation_root string,
	invocation DurableTargetPlanInvocation, operation_id string, transition string,
	subject_id string, subject_fingerprint string, input_fingerprint string,
	artifact_fingerprint string, generation_read i64, result string, target_sha256 string) !DurableTargetEvidenceTestObservation {
		prepared := prepare_durable_target_evidence(automation_root, DurableTargetEvidenceRequest{
			invocation:           invocation
			operation_id:         operation_id
			transition:           transition
			subject_id:           subject_id
			subject_fingerprint:  subject_fingerprint
			input_fingerprint:    input_fingerprint
			artifact_fingerprint: artifact_fingerprint
			generation_read:      generation_read
			generation_written:   generation_read + 1
			result:               result
			target_path:          target_state_path(subject_id)!
			target_sha256:        target_sha256
		})!
		return DurableTargetEvidenceTestObservation{
			path:         prepared.path
			operation_id: prepared.operation_id
			source:       prepared.source
			sha256:       prepared.sha256
			blob_oid:     prepared.blob_oid
		}
	}
}
