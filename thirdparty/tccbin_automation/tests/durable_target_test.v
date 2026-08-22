module tests

import crypto.sha1
import crypto.sha256
import os
import tccbin_automation.bin

const durable_test_sha40 = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
const durable_test_other_head = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
const durable_test_operation = '9191919191919191919191919191919191919191919191919191919191919191'
const durable_test_i32_plus_one = i64(2147483648)
const durable_test_u32_plus_two = i64(4294967297)

struct DurableNumericMutation {
	path  []string
	value i64
}

fn durable_fixture(name string) string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures', name)) or {
		panic(err)
	}
}

fn durable_git_blob_oid(source string) string {
	mut material := 'blob ${source.len}\x00'.bytes()
	material << source.bytes()
	return sha1.sum(material).hex()
}

fn durable_preconditions(source string, target_id string, generation i64) bin.TargetStateWritePreconditions {
	return bin.TargetStateWritePreconditions{
		target_id:               target_id
		expected_generation:     generation
		expected_blob_oid:       durable_git_blob_oid(source)
		expected_source_sha256:  sha256.sum256(source.bytes()).hex()
		expected_state_head_oid: durable_test_sha40
	}
}

fn durable_preconditions_with_head(source string, target_id string, generation i64,
	head string) bin.TargetStateWritePreconditions {
	return bin.TargetStateWritePreconditions{
		...durable_preconditions(source, target_id, generation)
		expected_state_head_oid: head
	}
}

fn durable_replace_value(root bin.JsonValue, key string, value bin.JsonValue) bin.JsonValue {
	mut values := root.object_values.clone()
	index := root.object_keys.index(key)
	if index < 0 {
		panic('missing durable test member ${key}')
	}
	values[index] = value
	return bin.JsonValue{
		kind:          .object
		object_keys:   root.object_keys.clone()
		object_values: values
	}
}

fn durable_replace_source(source string, key string, value bin.JsonValue) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	return bin.canonical_json(durable_replace_value(root, key, value))
}

fn durable_string(value string) bin.JsonValue {
	return bin.JsonValue{
		kind:         .string_value
		string_value: value
	}
}

fn durable_integer(value i64) bin.JsonValue {
	return bin.JsonValue{
		kind:      .integer
		int_value: value
	}
}

fn durable_array(values []bin.JsonValue) bin.JsonValue {
	return bin.JsonValue{
		kind:        .array
		array_value: values
	}
}

fn durable_object(keys []string, values []bin.JsonValue) bin.JsonValue {
	return bin.JsonValue{
		kind:          .object
		object_keys:   keys
		object_values: values
	}
}

fn durable_null() bin.JsonValue {
	return bin.JsonValue{
		kind: .null_value
	}
}

fn durable_member(value bin.JsonValue, key string) bin.JsonValue {
	return value.object_value(key) or { panic('missing durable test member ${key}') }
}

fn durable_replace_path(value bin.JsonValue, path []string,
	replacement bin.JsonValue) bin.JsonValue {
	if path.len == 0 {
		return replacement
	}
	if value.kind == .object {
		child := durable_member(value, path[0])
		return durable_replace_value(value, path[0], durable_replace_path(child, path[1..],
			replacement))
	}
	if value.kind == .array {
		index := path[0].int()
		if index < 0 || index >= value.array_value.len {
			panic('durable test array path is outside its value')
		}
		mut values := value.array_value.clone()
		values[index] = durable_replace_path(values[index], path[1..], replacement)
		return durable_array(values)
	}
	panic('durable test path crosses a scalar value')
}

fn durable_replace_path_source(source string, path []string, replacement bin.JsonValue) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	return bin.canonical_json(durable_replace_path(root, path, replacement))
}

fn durable_unknown_source() string {
	return durable_replace_source(durable_fixture('target-state.bootstrap.schema-fixture.json'),
		'target_state', durable_string('unknown_blocked'))
}

fn durable_prepare(source string, event bin.TransitionEvent,
	operation_id string) !bin.PreparedTargetStateWrite {
	root := bin.parse_strict_json(source)!
	return bin.prepare_target_state_transition(automation_root(), source, durable_preconditions(source, (root.object_value('target_id') or {
		return error('target ID absent')
	}).string_value,
		(root.object_value('generation') or { return error('generation absent') }).int_value),
		event, bin.TransitionContext{
		operation_id: operation_id
	})
}

fn durable_prepare_with_head(source string, event bin.TransitionEvent, operation_id string,
	head string) !bin.PreparedTargetStateWrite {
	root := bin.parse_strict_json(source)!
	return bin.prepare_target_state_transition(automation_root(), source, durable_preconditions_with_head(source, durable_member(root,
		'target_id').string_value, durable_member(root, 'generation').int_value, head), event, bin.TransitionContext{
		operation_id: operation_id
	})
}

fn durable_assert_prepare_fails(source string, preconditions bin.TargetStateWritePreconditions,
	event bin.TransitionEvent, operation_id string, expected string) {
	bin.prepare_target_state_transition(automation_root(), source, preconditions, event, bin.TransitionContext{
		operation_id: operation_id
	}) or {
		assert err.msg().contains(expected), '${err.msg()} does not contain ${expected}'
		return
	}
	assert false, 'durable target preparation unexpectedly succeeded'
}

fn durable_assert_prepare_fails_exact(source string, event bin.TransitionEvent,
	operation_id string, expected string) {
	durable_prepare(source, event, operation_id) or {
		assert err.msg() == expected, '${err.msg()} != ${expected}'
		return
	}
	assert false, 'durable target preparation unexpectedly succeeded'
}

fn durable_artifact_from_validation_subject(subject bin.JsonValue) bin.JsonValue {
	return durable_object(['sha', 'tree', 'input_fingerprint', 'artifact_fingerprint',
		'manifest_hash', 'digests'], [
		durable_member(subject, 'sha'),
		durable_member(subject, 'tree'),
		durable_member(subject, 'input_fingerprint'),
		durable_member(subject, 'artifact_fingerprint'),
		durable_member(subject, 'manifest_hash'),
		durable_member(subject, 'digests'),
	])
}

fn durable_seeded_reserved_source() string {
	mut root := bin.parse_strict_json(durable_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')) or {
		panic(err)
	}
	mut intent := durable_member(root, 'active_intent')
	good := durable_artifact_from_validation_subject(durable_member(intent, 'validation_subject'))
	for binding in [
		['intent_type', 'publish'],
		['stage', 'intent_reserved'],
	] {
		intent = durable_replace_value(intent, binding[0], durable_string(binding[1]))
	}
	intent = durable_replace_value(intent, 'generation', durable_member(root, 'generation'))
	intent = durable_replace_value(intent, 'gate_runs', durable_array([]))
	intent = durable_replace_value(intent, 'candidate_binding', durable_null())
	intent = durable_replace_value(intent, 'validation_subject', durable_null())
	intent = durable_replace_value(intent, 'previous_last_known_good', good)
	root = durable_replace_value(root, 'target_state', durable_string('unknown_blocked'))
	root = durable_replace_value(root, 'publication_state', durable_string('idle'))
	root = durable_replace_value(root, 'bootstrap_required', bin.JsonValue{
		kind:       .boolean
		bool_value: false
	})
	root = durable_replace_value(root, 'last_known_good', good)
	root = durable_replace_value(root, 'provisional_published', durable_null())
	root = durable_replace_value(root, 'active_intent', intent)
	root = durable_replace_value(root, 'post_validation_operation_id', durable_null())
	root = durable_replace_value(root, 'native_gate_subject', durable_null())
	root = durable_replace_value(root, 'active_subject_hash', durable_null())
	root = durable_replace_value(root, 'native_gate_execution', durable_null())
	root = durable_replace_value(root, 'v_smoke_execution', durable_null())
	root = durable_replace_value(root, 'last_native_validation', durable_null())
	return bin.canonical_json(root)
}

fn durable_native_subject_from_value(value bin.JsonValue) bin.NativeGateSubjectModel {
	mut digests := []bin.DigestModel{}
	for digest in durable_member(value, 'digests').array_value {
		digests << bin.DigestModel{
			path:   durable_member(digest, 'path').string_value
			sha256: durable_member(digest, 'sha256').string_value
		}
	}
	return bin.NativeGateSubjectModel{
		consumer_id:            durable_member(value, 'consumer_id').string_value
		consumer_kind:          durable_member(value, 'consumer_kind').string_value
		intent_or_operation_id: durable_member(value, 'intent_or_operation_id').string_value
		target_id:              durable_member(value, 'target_id').string_value
		subject_generation:     durable_member(value, 'subject_generation').int_value
		initial_run_mode:       durable_member(value, 'initial_run_mode').string_value
		sha:                    durable_member(value, 'sha').string_value
		tree:                   durable_member(value, 'tree').string_value
		original_ref:           durable_member(value, 'original_ref').string_value
		input_fingerprint:      durable_member(value, 'input_fingerprint').string_value
		artifact_fingerprint:   durable_member(value, 'artifact_fingerprint').string_value
		manifest_hash:          durable_member(value, 'manifest_hash').string_value
		digests:                digests
	}
}

fn durable_inverse_subject_digest_source() string {
	mut root := bin.parse_strict_json(durable_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')) or {
		panic(err)
	}
	digests := durable_array([
		durable_object(['path', 'sha256'], [durable_string('z-runtime.exe'),
			durable_string('6767676767676767676767676767676767676767676767676767676767676767')]),
		durable_object(['path', 'sha256'], [durable_string('a-compiler.exe'),
			durable_string('6868686868686868686868686868686868686868686868686868686868686868')]),
	])
	mut subject := durable_replace_value(durable_member(root, 'native_gate_subject'), 'digests',
		digests)
	subject_hash := bin.native_gate_subject_hash(durable_native_subject_from_value(subject)) or {
		panic(err)
	}
	mut execution := durable_member(root, 'native_gate_execution')
	execution = durable_replace_value(execution, 'subject', subject)
	execution = durable_replace_value(execution, 'subject_hash', durable_string(subject_hash))
	mut intent := durable_member(root, 'active_intent')
	mut validation_subject := durable_member(intent, 'validation_subject')
	validation_subject = durable_replace_value(validation_subject, 'digests', digests)
	intent = durable_replace_value(intent, 'validation_subject', validation_subject)
	mut gate_runs := durable_member(intent, 'gate_runs').array_value.clone()
	for index, mut run in gate_runs {
		run_id := durable_member(run, 'run_id').int_value
		run_attempt := int(durable_member(run, 'run_attempt').int_value)
		check_name := durable_member(run, 'check_name').string_value
		audience := if check_name == 'tccbin-candidate-gate' {
			'vlang/tccbin:native-gate-check:v1'
		} else {
			'vlang/tccbin:v-smoke-check:v1'
		}
		external_id := bin.deterministic_check_external_id(audience, durable_member(subject,
			'consumer_id').string_value, subject_hash, run_id, run_attempt) or { panic(err) }
		run = durable_replace_value(run, 'subject_hash', durable_string(subject_hash))
		run = durable_replace_value(run, 'external_id', durable_string(external_id))
		gate_runs[index] = run
	}
	intent = durable_replace_value(intent, 'gate_runs', durable_array(gate_runs))
	mut smoke := durable_member(root, 'v_smoke_execution')
	smoke = durable_replace_value(smoke, 'subject_hash', durable_string(subject_hash))
	mut attempts := durable_member(smoke, 'attempts').array_value.clone()
	for index, mut attempt in attempts {
		external_id := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1', durable_member(subject,
			'consumer_id').string_value, subject_hash, durable_member(attempt, 'run_id').int_value, int(durable_member(attempt,
			'run_attempt').int_value)) or { panic(err) }
		attempt = durable_replace_value(attempt, 'external_id', durable_string(external_id))
		attempts[index] = attempt
	}
	smoke = durable_replace_value(smoke, 'attempts', durable_array(attempts))
	refreshed := bin.parse_strict_json(refresh_v_smoke_facts_digests('{"v_smoke_execution":${bin.canonical_json(smoke)}}')) or {
		panic(err)
	}
	smoke = durable_member(refreshed, 'v_smoke_execution')
	root = durable_replace_value(root, 'native_gate_subject', subject)
	root = durable_replace_value(root, 'active_subject_hash', durable_string(subject_hash))
	root = durable_replace_value(root, 'native_gate_execution', execution)
	root = durable_replace_value(root, 'active_intent', intent)
	root = durable_replace_value(root, 'v_smoke_execution', smoke)
	mut operations := durable_member(root, 'applied_operations').array_value.clone()
	operations[1] = durable_replace_value(operations[1], 'transition',
		durable_string('native_gate_ack_${subject_hash}'))
	operations[2] = durable_replace_value(operations[2], 'transition',
		durable_string('native_gate_complete_${subject_hash}'))
	root = durable_replace_value(root, 'applied_operations', durable_array(operations))
	return bin.canonical_json(root)
}

fn durable_reversed_root_source(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	mut members := []string{}
	for index := root.object_keys.len - 1; index >= 0; index-- {
		members << '${bin.canonical_json(durable_string(root.object_keys[index]))}: ${bin.canonical_json(root.object_values[index])}'
	}
	return '{\n  ' + members.join(',\n  ') + '\n}'
}

fn durable_ledger_source(count int) string {
	mut root := bin.parse_strict_json(durable_unknown_source()) or { panic(err) }
	mut operations := []bin.JsonValue{cap: count}
	for index in 1 .. count + 1 {
		operation_id := sha256.sum256('durable-ledger/${index}'.bytes()).hex()
		operations << durable_object(['operation_id', 'transition', 'resulting_generation'], [
			durable_string(operation_id),
			durable_string('durable-ledger-${index}'),
			durable_integer(i64(index)),
		])
	}
	root = durable_replace_value(root, 'generation', durable_integer(i64(count)))
	root = durable_replace_value(root, 'applied_operations', durable_array(operations))
	root = durable_replace_value(root, 'last_operation_id',
		durable_string(sha256.sum256('durable-ledger/${count}'.bytes()).hex()))
	root = durable_replace_value(root, 'last_transition', durable_string('durable-ledger-${count}'))
	return bin.canonical_json(root)
}

fn durable_oversized_incident(index int) bin.JsonValue {
	identity := sha256.sum256('durable-oversize-incident/${index}'.bytes()).hex()
	label := '${index:04d}-' + 'x'.repeat(123)
	return durable_object(['incident_id', 'owner_repository', 'status', 'failure_class', 'component',
		'test_id', 'lane', 'input_fingerprint', 'artifact_fingerprint', 'created_by_operation_id',
		'resolved_by_sha'], [
		durable_string(identity),
		durable_string('vlang/tccbin'),
		durable_string('active'),
		durable_string(label),
		durable_string(label),
		durable_string(label),
		durable_string(label),
		durable_string(identity),
		durable_string(sha256.sum256('durable-oversize-artifact/${index}'.bytes()).hex()),
		durable_string(identity),
		durable_null(),
	])
}

fn durable_probe_padding(added_bytes int) []bin.JsonValue {
	for count in 1 .. 32 {
		mut minimum := -1
		mut prefixes := []string{cap: count}
		for index in 0 .. count {
			prefix := 'p${index}-'
			prefixes << prefix
			minimum += prefix.len + 3
		}
		maximum := count * 128 + count * 3 - 1
		if added_bytes < minimum || added_bytes > maximum {
			continue
		}
		mut remaining := added_bytes - minimum
		mut values := []bin.JsonValue{cap: count}
		for prefix in prefixes {
			extra := if remaining < 128 - prefix.len { remaining } else { 128 - prefix.len }
			values << durable_string(prefix + 'x'.repeat(extra))
			remaining -= extra
		}
		assert remaining == 0
		return values
	}
	panic('durable output-bound padding cannot represent ${added_bytes} bytes')
}

fn durable_near_limit_source() string {
	max_bytes := 2 * 1024 * 1024
	target_bytes := max_bytes - 32
	mut root := bin.parse_strict_json(durable_unknown_source()) or { panic(err) }
	empty_len := bin.canonical_json(root).len
	incident_length := bin.canonical_json(durable_oversized_incident(0)).len
	mut count := (target_bytes - empty_len + 1) / (incident_length + 1)
	mut incidents := []bin.JsonValue{cap: count}
	for index in 0 .. count {
		incidents << durable_oversized_incident(index)
	}
	root = durable_replace_value(root, 'incidents', durable_array(incidents))
	mut current := bin.canonical_json(root)
	mut missing := target_bytes - current.len
	if missing < 8 {
		count--
		incidents = incidents[..count].clone()
		root = durable_replace_value(root, 'incidents', durable_array(incidents))
		current = bin.canonical_json(root)
		missing = target_bytes - current.len
	}
	root = durable_replace_value(root, 'blocking_probe_ids',
		durable_array(durable_probe_padding(missing)))
	result := bin.canonical_json(root)
	assert result.len == target_bytes
	return result
}

fn durable_rich_unknown_source() string {
	mut root := bin.parse_strict_json(durable_unknown_source()) or { panic(err) }
	incident_id := '8181818181818181818181818181818181818181818181818181818181818181'
	incident := durable_object(['incident_id', 'owner_repository', 'status', 'failure_class',
		'component', 'test_id', 'lane', 'input_fingerprint', 'artifact_fingerprint',
		'created_by_operation_id', 'resolved_by_sha'], [
		durable_string(incident_id),
		durable_string('vlang/tccbin'),
		durable_string('active'),
		durable_string('compiler_regression'),
		durable_string('tinycc'),
		durable_string('compile-smoke'),
		durable_string('linux-amd64'),
		durable_string('7171717171717171717171717171717171717171717171717171717171717171'),
		bin.JsonValue{ kind: .null_value },
		durable_string('6161616161616161616161616161616161616161616161616161616161616161'),
		bin.JsonValue{ kind: .null_value },
	])
	root = durable_replace_value(root, 'incidents', durable_array([incident]))
	root = durable_replace_value(root, 'owner_repository', durable_string('vlang/tccbin'))
	root = durable_replace_value(root, 'issue_number', durable_integer(42))
	root = durable_replace_value(root, 'blocking_probe_ids', durable_array([
		durable_string('compile-smoke'),
		durable_string('link-smoke'),
	]))
	root = durable_replace_value(root, 'last_validation', durable_object(['run_id', 'run_attempt',
		'subject_hash', 'conclusion', 'evidence_digest'], [
		durable_integer(77),
		durable_integer(2),
		durable_string('5151515151515151515151515151515151515151515151515151515151515151'),
		durable_string('blocked'),
		durable_string('4141414141414141414141414141414141414141414141414141414141414141'),
	]))
	root = durable_replace_value(root, 'resolved_by', durable_object(['repository', 'sha', 'run_id'], [
		durable_string('vlang/tccbin'),
		durable_string('3030303030303030303030303030303030303030'),
		durable_integer(78),
	]))
	return bin.canonical_json(root)
}

fn test_durable_target_preparation_contract() {
	member_partition := ['schema_version', 'generation', 'target_id', 'target_state',
		'publication_state', 'bootstrap_required', 'canonical_observed_sha', 'input_fingerprint',
		'artifact_fingerprint', 'manifest_hash', 'provenance_status', 'affected_targets',
		'resolved_inputs', 'last_source_refetch', 'last_known_good', 'provisional_published',
		'active_intent', 'post_validation_operation_id', 'native_gate_subject', 'active_subject_hash',
		'native_gate_execution', 'v_smoke_execution', 'recovery_handoffs',
		'active_recovery_handoff_id', 'active_remediation_id', 'active_remediation_binding',
		'remediation_check_sources', 'last_head_observation', 'last_native_validation',
		'applied_operations', 'incidents', 'owner_repository', 'issue_number', 'blocking_probe_ids',
		'last_validation', 'resolved_by', 'last_operation_id', 'last_transition',
		'manual_green_publications']
	assert member_partition.len == 39
	immutables := ['schema_version', 'target_id', 'affected_targets']
	owned := ['generation', 'target_state', 'publication_state', 'bootstrap_required',
		'canonical_observed_sha', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash',
		'provenance_status', 'resolved_inputs', 'last_source_refetch', 'last_known_good',
		'provisional_published', 'active_intent', 'post_validation_operation_id',
		'native_gate_subject', 'active_subject_hash', 'native_gate_execution',
		'active_recovery_handoff_id', 'active_remediation_id', 'remediation_check_sources',
		'last_head_observation', 'last_native_validation', 'applied_operations', 'last_operation_id',
		'last_transition', 'manual_green_publications']
	root_only := ['v_smoke_execution', 'recovery_handoffs', 'active_remediation_binding',
		'owner_repository', 'issue_number', 'blocking_probe_ids', 'last_validation', 'resolved_by']
	assert immutables.len == 3
	assert owned.len == 27
	assert root_only.len == 8
	mut closed_partition := immutables.clone()
	closed_partition << owned
	closed_partition << 'incidents'
	closed_partition << root_only
	closed_partition.sort()
	mut sorted_members := member_partition.clone()
	sorted_members.sort()
	assert closed_partition == sorted_members

	core_source := durable_unknown_source()
	prepared := durable_prepare(core_source, .ledger_repaired_without_blockers,
		durable_test_operation) or { panic(err) }
	assert prepared.target_id == 'linux-amd64'
	assert prepared.target_path == 'targets/linux-amd64.json'
	assert prepared.transition == 'ledger_repaired_without_blockers'
	assert prepared.operation_id == durable_test_operation
	assert prepared.expected_generation == 0
	assert prepared.resulting_generation == 1
	assert prepared.expected_state_head_oid == durable_test_sha40
	assert prepared.predecessor_source_sha256 == sha256.sum256(core_source.bytes()).hex()
	assert prepared.predecessor_blob_oid == durable_git_blob_oid(core_source)
	assert prepared.resulting_source_sha256 == sha256.sum256(prepared.source.bytes()).hex()
	assert prepared.resulting_blob_oid == durable_git_blob_oid(prepared.source)
	assert prepared.predecessor_blob_oid != prepared.predecessor_source_sha256
	assert prepared.resulting_blob_oid != prepared.resulting_source_sha256
	assert prepared.source == bin.canonical_json(bin.parse_strict_json(prepared.source) or {
		panic(err)
	})
	assert !prepared.source.ends_with('\n')
	assert prepared.source.len <= 2 * 1024 * 1024
	assert !bin.durable_target_output_size_is_valid_for_test(0)
	assert bin.durable_target_output_size_is_valid_for_test(2 * 1024 * 1024)
	assert !bin.durable_target_output_size_is_valid_for_test(2 * 1024 * 1024 + 1)
	assert prepared.changed_members == ['applied_operations', 'generation', 'last_operation_id',
		'last_transition', 'target_state']
	mut result_keys :=
		(bin.parse_strict_json(prepared.source) or { panic(err) }).object_keys.clone()
	mut expected_keys := member_partition.clone()
	result_keys.sort()
	expected_keys.sort()
	assert result_keys == expected_keys

	rich_source := durable_rich_unknown_source()
	rich_before := bin.parse_strict_json(rich_source) or { panic(err) }
	rich := durable_prepare(rich_source, .ledger_repaired_with_blockers,
		'3131313131313131313131313131313131313131313131313131313131313131') or { panic(err) }
	rich_after := bin.parse_strict_json(rich.source) or { panic(err) }
	for key in ['schema_version', 'target_id', 'affected_targets', 'incidents', 'v_smoke_execution',
		'recovery_handoffs', 'active_remediation_binding', 'owner_repository', 'issue_number',
		'blocking_probe_ids', 'last_validation', 'resolved_by'] {
		assert bin.json_equal(rich_before.object_value(key) or { panic(key) }, rich_after.object_value(key) or {
			panic(key)
		})
	}
	assert rich.changed_members == ['applied_operations', 'generation', 'last_operation_id',
		'last_transition', 'target_state']
	second := durable_prepare(rich_source, .ledger_repaired_with_blockers,
		'3131313131313131313131313131313131313131313131313131313131313131') or { panic(err) }
	assert second == rich

	mut wrong := durable_preconditions(core_source, 'linux-amd64', 0)
	wrong = bin.TargetStateWritePreconditions{
		...wrong
		target_id: 'freebsd-amd64'
	}
	durable_assert_prepare_fails(core_source, wrong, .ledger_repaired_without_blockers,
		durable_test_operation, 'expected target or generation')
	wrong = bin.TargetStateWritePreconditions{
		...durable_preconditions(core_source, 'linux-amd64', 0)
		expected_generation: 1
	}
	durable_assert_prepare_fails(core_source, wrong, .ledger_repaired_without_blockers,
		durable_test_operation, 'expected target or generation')
	wrong = bin.TargetStateWritePreconditions{
		...durable_preconditions(core_source, 'linux-amd64', 0)
		expected_blob_oid: 'b'.repeat(40)
	}
	durable_assert_prepare_fails(core_source, wrong, .ledger_repaired_without_blockers,
		durable_test_operation, 'Git blob OID')
	wrong = bin.TargetStateWritePreconditions{
		...durable_preconditions(core_source, 'linux-amd64', 0)
		expected_source_sha256: 'b'.repeat(64)
	}
	durable_assert_prepare_fails(core_source, wrong, .ledger_repaired_without_blockers,
		durable_test_operation, 'SHA-256')
	wrong = bin.TargetStateWritePreconditions{
		...durable_preconditions(core_source, 'linux-amd64', 0)
		expected_state_head_oid: 'B'.repeat(40)
	}
	durable_assert_prepare_fails(core_source, wrong, .ledger_repaired_without_blockers,
		durable_test_operation, 'malformed')

	malformed := '{'
	durable_assert_prepare_fails(malformed, durable_preconditions(malformed, 'linux-amd64', 0),
		.ledger_repaired_without_blockers, durable_test_operation, 'JSON')
	duplicate := '{"generation":0,' + core_source[1..]
	durable_assert_prepare_fails(duplicate, durable_preconditions(duplicate, 'linux-amd64', 0),
		.ledger_repaired_without_blockers, durable_test_operation, 'duplicate')
	schema_invalid := durable_replace_source(core_source, 'target_state', durable_string('broken'))
	durable_assert_prepare_fails(schema_invalid, durable_preconditions(schema_invalid,
		'linux-amd64', 0), .ledger_repaired_without_blockers, durable_test_operation,
		'$/target_state')
	semantic_invalid := durable_replace_source(core_source, 'generation', durable_integer(1))
	durable_assert_prepare_fails_exact(semantic_invalid, .ledger_repaired_without_blockers,
		durable_test_operation,
		'predecessor target-state is invalid at $/applied_operations: a nonzero target generation must retain its bounded final CAS operation')

	durable_assert_prepare_fails('', durable_preconditions('', 'linux-amd64', 0),
		.ledger_repaired_without_blockers, durable_test_operation, 'between one byte and two MiB')
	oversized := ' '.repeat(2 * 1024 * 1024 + 1)
	durable_assert_prepare_fails(oversized, durable_preconditions(oversized, 'linux-amd64', 0),
		.ledger_repaired_without_blockers, durable_test_operation, 'between one byte and two MiB')

	bootstrap := durable_fixture('target-state.bootstrap.schema-fixture.json')
	durable_assert_prepare_fails(bootstrap, durable_preconditions(bootstrap, 'linux-amd64', 0),
		.ledger_invalid, durable_test_operation, 'incident companion update')

	companion_source := durable_replace_source(durable_fixture('target-state.v-smoke-terminal-check.schema-fixture.json'),
		'target_state', durable_string('unknown_blocked'))
	durable_assert_prepare_fails_exact(companion_source, .ledger_repaired_without_blockers,
		'1111111111111111111111111111111111111111111111111111111111111112',
		'prepared target-state is invalid at $/v_smoke_execution/expected_ledger_generation: V smoke CAS generation is stale')
}

fn test_durable_rich_projection_input_order_and_inert_head_token() {
	source := durable_seeded_reserved_source()
	before := bin.parse_strict_json(source) or { panic(err) }
	operation_id := sha256.sum256('durable-rich-positive'.bytes()).hex()
	prepared := durable_prepare_with_head(source, .ledger_repaired_without_blockers, operation_id,
		durable_test_sha40) or { panic(err) }
	after := bin.parse_strict_json(prepared.source) or { panic(err) }
	for key in ['resolved_inputs', 'last_known_good', 'active_intent'] {
		assert durable_member(before, key).kind != .null_value
		assert bin.json_equal(durable_member(before, key), durable_member(after, key)), key
	}
	assert durable_member(after, 'native_gate_subject').kind == .null_value
	assert durable_member(after, 'last_native_validation').kind == .null_value

	other_head := durable_prepare_with_head(source, .ledger_repaired_without_blockers,
		operation_id, durable_test_other_head) or { panic(err) }
	assert prepared.source == other_head.source
	assert prepared.resulting_blob_oid == other_head.resulting_blob_oid
	assert prepared.resulting_source_sha256 == other_head.resulting_source_sha256
	assert prepared.expected_state_head_oid == durable_test_sha40
	assert other_head.expected_state_head_oid == durable_test_other_head

	reordered := durable_reversed_root_source(source)
	assert reordered != source
	from_reordered := durable_prepare_with_head(reordered, .ledger_repaired_without_blockers,
		operation_id, durable_test_sha40) or { panic(err) }
	assert from_reordered.source == prepared.source
	assert from_reordered.resulting_blob_oid == prepared.resulting_blob_oid
	assert from_reordered.resulting_source_sha256 == prepared.resulting_source_sha256
	assert from_reordered.predecessor_blob_oid != prepared.predecessor_blob_oid
	assert from_reordered.predecessor_source_sha256 != prepared.predecessor_source_sha256
}

fn test_durable_subject_order_and_nonnull_native_record_codec_are_discriminated() {
	source := durable_inverse_subject_digest_source()
	before := bin.parse_strict_json(source) or { panic(err) }
	round_trip := bin.durable_target_round_trip_for_test(automation_root(), source) or {
		panic(err)
	}
	after := bin.parse_strict_json(round_trip) or { panic(err) }
	before_subject := durable_member(before, 'native_gate_subject')
	after_subject := durable_member(after, 'native_gate_subject')
	before_nested := durable_member(durable_member(before, 'native_gate_execution'), 'subject')
	after_nested := durable_member(durable_member(after, 'native_gate_execution'), 'subject')
	for value in [before_subject, after_subject, before_nested, after_nested] {
		digests := durable_member(value, 'digests').array_value
		assert digests.len == 2
		assert durable_member(digests[0], 'path').string_value == 'z-runtime.exe'
		assert durable_member(digests[1], 'path').string_value == 'a-compiler.exe'
	}
	assert bin.json_equal(before_subject, after_subject)
	assert bin.json_equal(before_nested, after_nested)
	ordered_model := durable_native_subject_from_value(after_subject)
	reversed_model := bin.NativeGateSubjectModel{
		...ordered_model
		digests: [ordered_model.digests[1], ordered_model.digests[0]]
	}
	canonical_hash := bin.native_gate_subject_hash(ordered_model) or { panic(err) }
	assert canonical_hash == bin.native_gate_subject_hash(reversed_model) or { panic(err) }
	assert canonical_hash == durable_member(after, 'active_subject_hash').string_value

	gate := bin.PersistedGateRunModel{
		check_name:      'tccbin-candidate-gate'
		run_attempt:     1
		subject_hash:    canonical_hash
		output_digest:   '1'.repeat(64)
		evidence_digest: '2'.repeat(64)
	}
	record := bin.NativeValidationRecordModel{
		schema_version:       1
		operation_id:         '3'.repeat(64)
		transition:           'candidate_checks_green'
		resulting_generation: 7
		verdict:              'green'
		manifest_source:      '{"rich":true}'
		manifest_hash:        '4'.repeat(64)
		native_lane_matrix:   durable_object(['subject', 'selected'], [after_subject,
			durable_string('native')])
		matrix_digest:        '5'.repeat(64)
		evidence:             [
			bin.NativeValidationEvidenceModel{
				sha256: '6'.repeat(64)
				size:   17
			},
		]
		capsule_digest:       '7'.repeat(64)
		native_gate:          gate
		v_smoke_gate:         bin.PersistedGateRunModel{
			...gate
			check_name: 'v-candidate-smoke'
		}
		validation_digest:    '8'.repeat(64)
	}
	record_value := bin.native_validation_record_json(record) or { panic(err) }
	record_round_trip := bin.durable_native_validation_round_trip_for_test(record_value) or {
		panic(err)
	}
	assert bin.json_equal(record_value, record_round_trip)
	assert durable_member(record_round_trip, 'manifest_source').string_value == '{"rich":true}'
	assert durable_member(durable_member(record_round_trip, 'native_gate'), 'run_attempt').int_value == 1
}

fn test_durable_all_int_backed_paths_reject_narrowing_before_shared_parsers() {
	base := durable_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')
	mutations := [
		DurableNumericMutation{
			path:  ['manual_green_publications']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['active_intent', 'run_attempt']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['active_intent', 'ordinal']
			value: durable_test_u32_plus_two
		},
		DurableNumericMutation{
			path:  ['active_intent', 'infra_retry_count']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['active_intent', 'source_retry_count']
			value: durable_test_u32_plus_two
		},
		DurableNumericMutation{
			path:  ['active_intent', 'gate_runs', '0', 'run_attempt']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'active_gate_epoch']
			value: durable_test_u32_plus_two
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'selected_run_attempt']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'infra_retry_count']
			value: durable_test_u32_plus_two
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'gate_epochs', '0', 'epoch']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'gate_epochs', '0', 'selected_run_attempt']
			value: durable_test_u32_plus_two
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'gate_runs', '0', 'gate_epoch']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['native_gate_execution', 'gate_runs', '0', 'run_attempt']
			value: durable_test_u32_plus_two
		},
	]
	for index, mutation in mutations {
		source := durable_replace_path_source(base, mutation.path, durable_integer(mutation.value))
		path := '$/' + mutation.path.join('/')
		durable_assert_prepare_fails_exact(source, .ledger_repaired_without_blockers,
			sha256.sum256('durable-narrowing/${index}'.bytes()).hex(),
			'${path}: integer is outside the host int range')
	}

	minimal_gate := durable_object(['run_attempt'], [durable_integer(1)])
	record := durable_object(['schema_version', 'native_gate', 'v_smoke_gate'], [
		durable_integer(1),
		minimal_gate,
		minimal_gate,
	])
	record_base := durable_replace_path_source(base, ['last_native_validation'], record)
	record_mutations := [
		DurableNumericMutation{
			path:  ['last_native_validation', 'schema_version']
			value: durable_test_i32_plus_one
		},
		DurableNumericMutation{
			path:  ['last_native_validation', 'native_gate', 'run_attempt']
			value: durable_test_u32_plus_two
		},
		DurableNumericMutation{
			path:  ['last_native_validation', 'v_smoke_gate', 'run_attempt']
			value: durable_test_i32_plus_one
		},
	]
	for index, mutation in record_mutations {
		source := durable_replace_path_source(record_base, mutation.path,
			durable_integer(mutation.value))
		path := '$/' + mutation.path.join('/')
		durable_assert_prepare_fails_exact(source, .ledger_repaired_without_blockers,
			sha256.sum256('durable-record-narrowing/${index}'.bytes()).hex(),
			'${path}: integer is outside the host int range')
	}
}

fn test_durable_replay_collision_full_ledger_and_real_output_bound_fail_closed() {
	source := durable_unknown_source()
	prepared := durable_prepare(source, .ledger_repaired_without_blockers, durable_test_operation) or {
		panic(err)
	}
	durable_assert_prepare_fails_exact(prepared.source, .ledger_repaired_without_blockers,
		durable_test_operation,
		'target-state transition did not produce one new ledger generation and operation')
	durable_assert_prepare_fails_exact(prepared.source, .ledger_invalid, durable_test_operation,
		'operation ID was already applied or collided with another target transition')

	full_ledger := durable_ledger_source(128)
	durable_assert_prepare_fails_exact(full_ledger, .ledger_repaired_without_blockers,
		sha256.sum256('durable-ledger-overflow'.bytes()).hex(),
		'bounded applied-operation ledger is full and requires reviewed compaction')

	near_limit := durable_near_limit_source()
	assert near_limit.len == 2 * 1024 * 1024 - 32
	durable_assert_prepare_fails_exact(near_limit, .ledger_repaired_with_blockers,
		sha256.sum256('durable-output-overflow'.bytes()).hex(),
		'prepared target-state bytes are empty, oversized, or non-canonical')
}
