module bin

import crypto.sha256
import os

const durable_target_max_bytes = 2 * 1024 * 1024

const durable_target_members = ['schema_version', 'generation', 'target_id', 'target_state',
	'publication_state', 'bootstrap_required', 'canonical_observed_sha', 'input_fingerprint',
	'artifact_fingerprint', 'manifest_hash', 'provenance_status', 'affected_targets',
	'resolved_inputs', 'last_source_refetch', 'last_known_good', 'provisional_published',
	'active_intent', 'post_validation_operation_id', 'native_gate_subject', 'active_subject_hash',
	'native_gate_execution', 'v_smoke_execution', 'recovery_handoffs', 'active_recovery_handoff_id',
	'active_remediation_id', 'active_remediation_binding', 'remediation_check_sources',
	'last_head_observation', 'last_native_validation', 'applied_operations', 'incidents',
	'owner_repository', 'issue_number', 'blocking_probe_ids', 'last_validation', 'resolved_by',
	'last_operation_id', 'last_transition', 'manual_green_publications']

const durable_target_immutable_members = ['schema_version', 'target_id', 'affected_targets']

const durable_target_owned_members = ['generation', 'target_state', 'publication_state',
	'bootstrap_required', 'canonical_observed_sha', 'input_fingerprint', 'artifact_fingerprint',
	'manifest_hash', 'provenance_status', 'resolved_inputs', 'last_source_refetch', 'last_known_good',
	'provisional_published', 'active_intent', 'post_validation_operation_id', 'native_gate_subject',
	'active_subject_hash', 'native_gate_execution', 'active_recovery_handoff_id',
	'active_remediation_id', 'remediation_check_sources', 'last_head_observation',
	'last_native_validation', 'applied_operations', 'last_operation_id', 'last_transition',
	'manual_green_publications']

const durable_target_root_only_members = ['v_smoke_execution', 'recovery_handoffs',
	'active_remediation_binding', 'owner_repository', 'issue_number', 'blocking_probe_ids',
	'last_validation', 'resolved_by']

// TargetStateWritePreconditions binds the exact predecessor bytes and typed target generation.
// expected_state_head_oid is deliberately an inert, untrusted correlation token: this type does
// not authenticate a ref and cannot authorize a GitHub compare-and-swap.
pub struct TargetStateWritePreconditions {
pub:
	target_id               string
	expected_generation     i64
	expected_blob_oid       string
	expected_source_sha256  string
	expected_state_head_oid string
}

// PreparedTargetStateWrite is a deterministic, locally validated byte artifact. It is not a
// writer, commit, ref proof, CAS authorization, GraphQL request, or publication capability. A
// future writer must independently reauthenticate the state HEAD before and after every attempt.
pub struct PreparedTargetStateWrite {
pub:
	target_id                 string
	target_path               string
	transition                string
	operation_id              string
	expected_generation       i64
	resulting_generation      i64
	expected_state_head_oid   string
	predecessor_blob_oid      string
	predecessor_source_sha256 string
	resulting_blob_oid        string
	resulting_source_sha256   string
	changed_members           []string
	source                    string
}

struct DurableTargetRoot {
	root         JsonValue
	model        TargetModel
	incident_ids []string
}

// prepare_target_state_transition authenticates predecessor bytes against caller-supplied content
// identities, applies one pure TargetModel transition, and emits canonical target-state bytes.
// It performs no filesystem write, Git mutation, network request, ref lookup, or CAS.
pub fn prepare_target_state_transition(automation_root string, predecessor_source string,
	preconditions TargetStateWritePreconditions, event TransitionEvent,
	context TransitionContext) !PreparedTargetStateWrite {
	validate_durable_target_preconditions(predecessor_source, preconditions)!
	current := load_durable_target_root(automation_root, predecessor_source)!
	if current.model.target_id != preconditions.target_id
		|| current.model.generation != preconditions.expected_generation {
		return error('target-state predecessor differs from the expected target or generation')
	}
	next := transition_target(current.model, event, context)!
	if next.generation != current.model.generation + 1
		|| next.applied_operations.len != current.model.applied_operations.len + 1
		|| next.last_operation_id != context.operation_id || next.last_transition != event.str() {
		return error('target-state transition did not produce one new ledger generation and operation')
	}
	if next.incident_ids != current.incident_ids {
		return error('target-state transition requires an incident companion update outside T2c3a')
	}
	result_root := replace_durable_target_owned_projection(current.root, next)!
	validate_durable_target_preserved_members(current.root, result_root)!
	result_source := canonical_json(result_root)
	if !durable_target_output_size_is_valid(result_source.len) || result_source.ends_with('\n')
		|| result_source.ends_with('\r') {
		return error('prepared target-state bytes are empty, oversized, or non-canonical')
	}
	reparsed := parse_strict_json(result_source)!
	if !json_equal(reparsed, result_root) || canonical_json(reparsed) != result_source {
		return error('prepared target-state bytes do not round-trip as exact JCS')
	}
	validate_durable_target_int_ranges(reparsed)!
	validate_durable_target_schema(automation_root, reparsed, 'prepared')!
	reprojected := durable_target_model_from_value(reparsed)!
	if reprojected != next {
		return error('prepared target-state projection differs from the pure transition result')
	}
	validate_durable_target_preserved_members(current.root, reparsed)!
	changed_members := durable_target_changed_members(current.root, reparsed)!
	return PreparedTargetStateWrite{
		target_id:                 next.target_id
		target_path:               target_state_path(next.target_id)!
		transition:                event.str()
		operation_id:              context.operation_id
		expected_generation:       current.model.generation
		resulting_generation:      next.generation
		expected_state_head_oid:   preconditions.expected_state_head_oid
		predecessor_blob_oid:      preconditions.expected_blob_oid
		predecessor_source_sha256: preconditions.expected_source_sha256
		resulting_blob_oid:        git_blob_oid(result_source.bytes())
		resulting_source_sha256:   sha256.sum256(result_source.bytes()).hex()
		changed_members:           changed_members
		source:                    result_source
	}
}

fn durable_target_output_size_is_valid(size int) bool {
	return size > 0 && size <= durable_target_max_bytes
}

$if test {
	// durable_target_output_size_is_valid_for_test exposes only the closed numeric bound. It cannot
	// construct, inject, authenticate, or prepare target-state bytes.
	pub fn durable_target_output_size_is_valid_for_test(size int) bool {
		return durable_target_output_size_is_valid(size)
	}

	// This test-only codec exercises the private validated root/model boundary without adding a
	// production injection surface. It accepts no replacement model and performs no transition.
	pub fn durable_target_round_trip_for_test(automation_root string, source string) !string {
		if source.len == 0 || source.len > durable_target_max_bytes {
			return error('target-state predecessor must contain between one byte and two MiB')
		}
		current := load_durable_target_root(automation_root, source)!
		result_root := replace_durable_target_owned_projection(current.root, current.model)!
		validate_durable_target_preserved_members(current.root, result_root)!
		result_source := canonical_json(result_root)
		if !durable_target_output_size_is_valid(result_source.len) || result_source.ends_with('\n')
			|| result_source.ends_with('\r') {
			return error('durable target round-trip bytes are empty, oversized, or non-canonical')
		}
		reparsed := parse_strict_json(result_source)!
		if !json_equal(reparsed, result_root) || canonical_json(reparsed) != result_source {
			return error('durable target round-trip bytes do not retain exact JCS')
		}
		validate_durable_target_int_ranges(reparsed)!
		validate_durable_target_schema(automation_root, reparsed, 'round-trip')!
		if durable_target_model_from_value(reparsed)! != current.model {
			return error('durable target round-trip projection differs from its predecessor')
		}
		validate_durable_target_preserved_members(current.root, reparsed)!
		return result_source
	}

	// The record codec is exposed only to the test build so the durable adapter's non-null branch
	// can be discriminated independently of physical capsule construction.
	pub fn durable_native_validation_round_trip_for_test(value JsonValue) !JsonValue {
		durable_validate_native_validation_ints(value)!
		return durable_native_validation_json(native_validation_record_from_json(value)!)
	}
}

fn validate_durable_target_preconditions(source string,
	preconditions TargetStateWritePreconditions) ! {
	if source.len == 0 || source.len > durable_target_max_bytes {
		return error('target-state predecessor must contain between one byte and two MiB')
	}
	if preconditions.target_id !in managed_target_ids || preconditions.expected_generation < 0 {
		return error('target-state precondition target or generation is invalid')
	}
	if !is_lower_hex_40(preconditions.expected_blob_oid)
		|| !is_lower_hex_64(preconditions.expected_source_sha256)
		|| !is_lower_hex_40(preconditions.expected_state_head_oid) {
		return error('target-state precondition digests or inert HEAD token are malformed')
	}
	observed_sha256 := sha256.sum256(source.bytes()).hex()
	observed_blob_oid := git_blob_oid(source.bytes())
	if observed_sha256 != preconditions.expected_source_sha256 {
		return error('target-state predecessor SHA-256 differs from the expected bytes')
	}
	if observed_blob_oid != preconditions.expected_blob_oid {
		return error('target-state predecessor Git blob OID differs from the expected bytes')
	}
}

fn load_durable_target_root(automation_root string, source string) !DurableTargetRoot {
	root := parse_strict_json(source) or {
		return error('target-state predecessor JSON is malformed: ${err}')
	}
	require_exact_keys(root, durable_target_members)!
	validate_durable_target_int_ranges(root)!
	validate_durable_target_schema(automation_root, root, 'predecessor')!
	model := durable_target_model_from_value(root)!
	validate_target_model(model)!
	incident_ids := durable_target_incident_ids(root)!
	if model.incident_ids != incident_ids {
		return error('target-state incident projection differs from its typed model')
	}
	return DurableTargetRoot{
		root:         root
		model:        model
		incident_ids: incident_ids
	}
}

fn validate_durable_target_schema(automation_root string, root JsonValue, label string) ! {
	schema_path := os.join_path(automation_root, 'schemas', 'target-state.schema.json')
	issues := validate_json_value(schema_path, root)!
	if issues.len > 0 {
		return error('${label} target-state is invalid at ${issues[0].path}: ${issues[0].message}')
	}
}

fn durable_target_incident_ids(root JsonValue) ![]string {
	mut ids := []string{}
	for incident in require_array_member(root, 'incidents')! {
		ids << require_string_member(incident, 'incident_id')!
	}
	return ids
}

fn validate_durable_target_preserved_members(before JsonValue, after JsonValue) ! {
	for key in durable_target_immutable_members {
		if !json_equal(require_member(before, key)!, require_member(after, key)!) {
			return error('target-state transition changed immutable member ${key}')
		}
	}
	for key in durable_target_root_only_members {
		if !json_equal(require_member(before, key)!, require_member(after, key)!) {
			return error('target-state transition changed root-only companion ${key}')
		}
	}
	if !json_equal(require_member(before, 'incidents')!, require_member(after, 'incidents')!) {
		return error('target-state transition changed the preserved incident objects')
	}
}

fn durable_target_changed_members(before JsonValue, after JsonValue) ![]string {
	mut changed := []string{}
	for key in durable_target_members {
		if !json_equal(require_member(before, key)!, require_member(after, key)!) {
			changed << key
		}
	}
	changed.sort()
	return changed
}

fn replace_durable_target_owned_projection(root JsonValue, model TargetModel) !JsonValue {
	mut keys := root.object_keys.clone()
	mut values := root.object_values.clone()
	for key in durable_target_owned_members {
		index := keys.index(key)
		if index < 0 {
			return error('target-state owned member ${key} is absent')
		}
		values[index] = durable_target_owned_value(key, model)!
	}
	return object_value_from_pairs(keys, values)
}

fn durable_target_owned_value(key string, model TargetModel) !JsonValue {
	return match key {
		'generation' { dt_integer(model.generation) }
		'target_state' { dt_string(model.target_state.str()) }
		'publication_state' { dt_string(model.publication_state.str()) }
		'bootstrap_required' { dt_boolean(model.bootstrap_required) }
		'canonical_observed_sha' { dt_string(model.canonical_observed_sha) }
		'input_fingerprint' { dt_nullable_string(model.input_fingerprint) }
		'artifact_fingerprint' { dt_nullable_string(model.artifact_fingerprint) }
		'manifest_hash' { dt_nullable_string(model.manifest_hash) }
		'provenance_status' { dt_nullable_string(model.provenance_status) }
		'resolved_inputs' { durable_resolved_inputs_json(model.resolved_inputs)! }
		'last_source_refetch' { durable_source_refetch_json(model.last_source_refetch)! }
		'last_known_good' { durable_artifact_tuple_json(model.last_known_good)! }
		'provisional_published' { durable_artifact_tuple_json(model.provisional_published)! }
		'active_intent' { durable_active_intent_json(model.active_intent)! }
		'post_validation_operation_id' { dt_nullable_string(model.post_validation_operation_id) }
		'native_gate_subject' { durable_native_subject_json(model.active_native_subject)! }
		'active_subject_hash' { dt_nullable_string(model.active_subject_hash) }
		'native_gate_execution' { durable_native_gate_json(model.active_native_gate)! }
		'active_recovery_handoff_id' { dt_nullable_string(model.active_recovery_handoff_id) }
		'active_remediation_id' { dt_nullable_string(model.active_remediation_id) }
		'remediation_check_sources' { durable_check_sources_json(model.remediation_check_sources)! }
		'last_head_observation' { durable_head_observation_json(model.last_head_observation)! }
		'last_native_validation' { durable_native_validation_json(model.last_native_validation)! }
		'applied_operations' { durable_applied_operations_json(model.applied_operations)! }
		'last_operation_id' { dt_nullable_string(model.last_operation_id) }
		'last_transition' { dt_nullable_string(model.last_transition) }
		'manual_green_publications' { dt_integer(model.manual_green_publications) }
		else { return error('target-state member ${key} is outside the owned projection') }
	}
}

fn durable_target_model_from_value(root JsonValue) !TargetModel {
	native_subject_value := require_member(root, 'native_gate_subject')!
	native_subject := if native_subject_value.kind == .null_value {
		NativeGateSubjectModel{}
	} else {
		native_subject_from_recovery(parse_receiver_subject(native_subject_value)!)
	}
	native_gate_value := require_member(root, 'native_gate_execution')!
	native_gate := if native_gate_value.kind == .null_value {
		NativeGateModel{}
	} else {
		durable_native_gate_from_value(native_gate_value, native_subject)!
	}
	native_validation_value := require_member(root, 'last_native_validation')!
	native_validation := if native_validation_value.kind == .null_value {
		NativeValidationRecordModel{}
	} else {
		durable_native_validation_from_value(native_validation_value)!
	}
	return TargetModel{
		target_id:                    require_string_member(root, 'target_id')!
		generation:                   require_integer_member(root, 'generation')!
		target_state:                 durable_target_state(require_string_member(root,
			'target_state')!)!
		publication_state:            durable_publication_state(require_string_member(root,
			'publication_state')!)!
		bootstrap_required:           require_bool_member(root, 'bootstrap_required')!
		canonical_observed_sha:       require_string_member(root, 'canonical_observed_sha')!
		input_fingerprint:            require_nullable_string_member(root, 'input_fingerprint')!
		artifact_fingerprint:         require_nullable_string_member(root, 'artifact_fingerprint')!
		manifest_hash:                require_nullable_string_member(root, 'manifest_hash')!
		provenance_status:            require_nullable_string_member(root, 'provenance_status')!
		affected_targets:             dt_string_array(require_array_member(root, 'affected_targets')!)!
		resolved_inputs:              durable_resolved_inputs_from_value(require_member(root,
			'resolved_inputs')!)!
		last_known_good:              durable_artifact_tuple_from_value(require_member(root,
			'last_known_good')!)!
		provisional_published:        durable_artifact_tuple_from_value(require_member(root,
			'provisional_published')!)!
		active_intent:                durable_active_intent_from_value(require_member(root,
			'active_intent')!)!
		incident_ids:                 durable_target_incident_ids(root)!
		active_recovery_handoff_id:   require_nullable_string_member(root,
			'active_recovery_handoff_id')!
		active_native_subject:        native_subject
		active_subject_hash:          require_nullable_string_member(root, 'active_subject_hash')!
		active_native_gate:           native_gate
		active_remediation_id:        require_nullable_string_member(root, 'active_remediation_id')!
		post_validation_operation_id: require_nullable_string_member(root,
			'post_validation_operation_id')!
		remediation_check_sources:    durable_check_sources_from_value(require_member(root,
			'remediation_check_sources')!)!
		last_source_refetch:          durable_source_refetch_from_value(require_member(root,
			'last_source_refetch')!)!
		last_head_observation:        durable_head_observation_from_value(require_member(root,
			'last_head_observation')!)!
		last_native_validation:       native_validation
		applied_operations:           durable_applied_operations_from_value(require_member(root,
			'applied_operations')!)!
		last_operation_id:            require_nullable_string_member(root, 'last_operation_id')!
		last_transition:              require_nullable_string_member(root, 'last_transition')!
		manual_green_publications:    durable_int_member(root, 'manual_green_publications',
			'$/manual_green_publications')!
	}
}

fn durable_checked_int(value i64, path string) !int {
	narrowed := int(value)
	if i64(narrowed) != value {
		return error('${path}: integer is outside the host int range')
	}
	return narrowed
}

fn durable_int_member(value JsonValue, key string, path string) !int {
	return durable_checked_int(require_integer_member(value, key)!, path)
}

fn durable_nullable_int_member(value JsonValue, key string, path string) !int {
	return durable_checked_int(require_nullable_integer(value, key)!, path)
}

// Pre-schema narrowing checks inspect only structurally present integers. Missing members and wrong
// JSON kinds remain the schema validator's responsibility and retain its path-specific diagnostics.
fn durable_prevalidate_int_member(value JsonValue, key string, path string) ! {
	if value.kind != .object {
		return
	}
	member := value.object_value(key) or { return }
	if member.kind == .integer {
		durable_checked_int(member.int_value, path)!
	}
}

fn durable_prevalidate_persisted_gate_run_ints(value JsonValue, path string) ! {
	durable_prevalidate_int_member(value, 'run_attempt', '${path}/run_attempt')!
}

fn durable_validate_persisted_gate_run_ints(value JsonValue, path string) ! {
	durable_int_member(value, 'run_attempt', '${path}/run_attempt')!
}

fn durable_native_validation_from_value(value JsonValue) !NativeValidationRecordModel {
	durable_validate_native_validation_ints(value)!
	return native_validation_record_from_json(value)
}

fn durable_validate_native_validation_ints(value JsonValue) ! {
	durable_int_member(value, 'schema_version', '$/last_native_validation/schema_version')!
	native_gate := require_object_member(value, 'native_gate')!
	v_smoke_gate := require_object_member(value, 'v_smoke_gate')!
	durable_validate_persisted_gate_run_ints(native_gate, '$/last_native_validation/native_gate')!
	durable_validate_persisted_gate_run_ints(v_smoke_gate, '$/last_native_validation/v_smoke_gate')!
}

fn durable_native_gate_from_value(value JsonValue,
	expected_subject NativeGateSubjectModel) !NativeGateModel {
	durable_validate_native_gate_ints(value)!
	return parse_live_native_gate(value, expected_subject)
}

fn durable_validate_native_gate_ints(value JsonValue) ! {
	durable_int_member(value, 'active_gate_epoch', '$/native_gate_execution/active_gate_epoch')!
	durable_nullable_int_member(value, 'selected_run_attempt',
		'$/native_gate_execution/selected_run_attempt')!
	durable_int_member(value, 'infra_retry_count', '$/native_gate_execution/infra_retry_count')!
	for index, epoch in require_array_member(value, 'gate_epochs')! {
		path := '$/native_gate_execution/gate_epochs/${index}'
		durable_int_member(epoch, 'epoch', '${path}/epoch')!
		durable_nullable_int_member(epoch, 'selected_run_attempt', '${path}/selected_run_attempt')!
	}
	for index, run in require_array_member(value, 'gate_runs')! {
		path := '$/native_gate_execution/gate_runs/${index}'
		durable_int_member(run, 'gate_epoch', '${path}/gate_epoch')!
		durable_int_member(run, 'run_attempt', '${path}/run_attempt')!
	}
}

fn validate_durable_target_int_ranges(root JsonValue) ! {
	durable_prevalidate_int_member(root, 'manual_green_publications', '$/manual_green_publications')!
	intent := root.object_value('active_intent') or { dt_null() }
	if intent.kind == .object {
		durable_prevalidate_int_member(intent, 'run_attempt', '$/active_intent/run_attempt')!
		durable_prevalidate_int_member(intent, 'ordinal', '$/active_intent/ordinal')!
		durable_prevalidate_int_member(intent, 'infra_retry_count',
			'$/active_intent/infra_retry_count')!
		durable_prevalidate_int_member(intent, 'source_retry_count',
			'$/active_intent/source_retry_count')!
		gate_runs := intent.object_value('gate_runs') or { dt_null() }
		if gate_runs.kind == .array {
			for index, run in gate_runs.array_value {
				durable_prevalidate_persisted_gate_run_ints(run,
					'$/active_intent/gate_runs/${index}')!
			}
		}
	}
	native_gate := root.object_value('native_gate_execution') or { dt_null() }
	if native_gate.kind == .object {
		durable_prevalidate_native_gate_ints(native_gate)!
	}
	native_validation := root.object_value('last_native_validation') or { dt_null() }
	if native_validation.kind == .object {
		durable_prevalidate_native_validation_ints(native_validation)!
	}
}

fn durable_prevalidate_native_gate_ints(value JsonValue) ! {
	durable_prevalidate_int_member(value, 'active_gate_epoch',
		'$/native_gate_execution/active_gate_epoch')!
	durable_prevalidate_int_member(value, 'selected_run_attempt',
		'$/native_gate_execution/selected_run_attempt')!
	durable_prevalidate_int_member(value, 'infra_retry_count',
		'$/native_gate_execution/infra_retry_count')!
	epochs := value.object_value('gate_epochs') or { dt_null() }
	if epochs.kind == .array {
		for index, epoch in epochs.array_value {
			path := '$/native_gate_execution/gate_epochs/${index}'
			durable_prevalidate_int_member(epoch, 'epoch', '${path}/epoch')!
			durable_prevalidate_int_member(epoch, 'selected_run_attempt',
				'${path}/selected_run_attempt')!
		}
	}
	runs := value.object_value('gate_runs') or { dt_null() }
	if runs.kind == .array {
		for index, run in runs.array_value {
			path := '$/native_gate_execution/gate_runs/${index}'
			durable_prevalidate_int_member(run, 'gate_epoch', '${path}/gate_epoch')!
			durable_prevalidate_int_member(run, 'run_attempt', '${path}/run_attempt')!
		}
	}
}

fn durable_prevalidate_native_validation_ints(value JsonValue) ! {
	durable_prevalidate_int_member(value, 'schema_version',
		'$/last_native_validation/schema_version')!
	native_gate := value.object_value('native_gate') or { dt_null() }
	if native_gate.kind == .object {
		durable_prevalidate_persisted_gate_run_ints(native_gate,
			'$/last_native_validation/native_gate')!
	}
	v_smoke_gate := value.object_value('v_smoke_gate') or { dt_null() }
	if v_smoke_gate.kind == .object {
		durable_prevalidate_persisted_gate_run_ints(v_smoke_gate,
			'$/last_native_validation/v_smoke_gate')!
	}
}

fn durable_target_state(value string) !TargetState {
	return match value {
		'uninitialized' { .uninitialized }
		'eligible' { .eligible }
		'quarantined' { .quarantined }
		'validating' { .validating }
		'unknown_blocked' { .unknown_blocked }
		else { return error('target state is outside the closed enum') }
	}
}

fn durable_publication_state(value string) !PublicationState {
	return match value {
		'idle' { .idle }
		'candidate_pending' { .candidate_pending }
		'promotion_blocked' { .promotion_blocked }
		'post_publish_validating' { .post_publish_validating }
		'post_publish_waiting_source' { .post_publish_waiting_source }
		'adopt_current_waiting_source' { .adopt_current_waiting_source }
		'post_publish_blocked' { .post_publish_blocked }
		'rollback_pending' { .rollback_pending }
		'rollback_waiting_source' { .rollback_waiting_source }
		'rollback_blocked' { .rollback_blocked }
		'restored_last_known_good' { .restored_last_known_good }
		else { return error('publication state is outside the closed enum') }
	}
}

fn durable_head_relationship(value string) !HeadRelationship {
	return match value {
		'unknown' { .unknown }
		'exact_subject' { .exact_subject }
		'subject_ancestor' { .subject_ancestor }
		'unrelated' { .unrelated }
		else { return error('head relationship is outside the closed enum') }
	}
}

fn durable_resolved_inputs_from_value(value JsonValue) !ResolvedInputsModel {
	if value.kind == .null_value {
		return ResolvedInputsModel{}
	}
	mut sources := []ResolvedSourceModel{}
	for source in require_array_member(value, 'sources')! {
		sources << ResolvedSourceModel{
			id:         require_string_member(source, 'id')!
			repository: require_string_member(source, 'repository')!
			ref:        require_string_member(source, 'ref')!
			sha:        require_string_member(source, 'sha')!
			tree:       require_string_member(source, 'tree')!
		}
	}
	mut checks := []SourceCheckModel{}
	for check in require_array_member(value, 'source_checks')! {
		checks << SourceCheckModel{
			source_id:       require_string_member(check, 'source_id')!
			resolved_sha:    require_string_member(check, 'resolved_sha')!
			status:          require_string_member(check, 'status')!
			evidence_digest: require_string_member(check, 'evidence_digest')!
		}
	}
	producer := require_object_member(value, 'producer_toolchain')!
	return ResolvedInputsModel{
		sources:             sources
		source_checks:       checks
		recipe_path:         require_string_member(value, 'recipe_path')!
		recipe_hash:         require_string_member(value, 'recipe_hash')!
		contract_repository: require_string_member(value, 'contract_repository')!
		contract_sha:        require_string_member(value, 'contract_sha')!
		v_source_sha:        require_string_member(value, 'v_source_sha')!
		producer_toolchain:  ProducerToolchainModel{
			profile_id:         require_string_member(producer, 'profile_id')!
			profile_sha256:     require_string_member(producer, 'profile_sha256')!
			observation_sha256: require_string_member(producer, 'observation_sha256')!
			observation_digest: require_string_member(producer, 'observation_digest')!
		}
	}
}

fn durable_artifact_tuple_from_value(value JsonValue) !ArtifactTupleModel {
	if value.kind == .null_value {
		return ArtifactTupleModel{}
	}
	return ArtifactTupleModel{
		sha:                  require_string_member(value, 'sha')!
		tree:                 require_string_member(value, 'tree')!
		input_fingerprint:    require_string_member(value, 'input_fingerprint')!
		artifact_fingerprint: require_string_member(value, 'artifact_fingerprint')!
		manifest_hash:        require_string_member(value, 'manifest_hash')!
		digests:              durable_digests_from_value(require_member(value, 'digests')!)!
	}
}

fn durable_candidate_binding_from_value(value JsonValue) !CandidateBindingModel {
	if value.kind == .null_value {
		return CandidateBindingModel{}
	}
	return CandidateBindingModel{
		sha:                  require_string_member(value, 'sha')!
		tree:                 require_string_member(value, 'tree')!
		parent:               require_string_member(value, 'parent')!
		artifact_fingerprint: require_string_member(value, 'artifact_fingerprint')!
		manifest_hash:        require_string_member(value, 'manifest_hash')!
		digests:              durable_digests_from_value(require_member(value, 'digests')!)!
	}
}

fn durable_validation_subject_from_value(value JsonValue) !ValidationSubjectModel {
	if value.kind == .null_value {
		return ValidationSubjectModel{}
	}
	return ValidationSubjectModel{
		sha:                  require_string_member(value, 'sha')!
		tree:                 require_string_member(value, 'tree')!
		input_fingerprint:    require_string_member(value, 'input_fingerprint')!
		artifact_fingerprint: require_string_member(value, 'artifact_fingerprint')!
		manifest_hash:        require_string_member(value, 'manifest_hash')!
		digests:              durable_digests_from_value(require_member(value, 'digests')!)!
		candidate_ref:        require_string_member(value, 'candidate_ref')!
	}
}

fn durable_digests_from_value(value JsonValue) ![]DigestModel {
	if value.kind != .array {
		return error('durable digest projection must be an array')
	}
	mut digests := []DigestModel{}
	for digest in value.array_value {
		digests << DigestModel{
			path:   require_string_member(digest, 'path')!
			sha256: require_string_member(digest, 'sha256')!
		}
	}
	return digests
}

fn durable_check_sources_from_value(value JsonValue) ![]CheckSourceModel {
	if value.kind != .array {
		return error('durable check-source projection must be an array')
	}
	mut sources := []CheckSourceModel{}
	for source in value.array_value {
		sources << CheckSourceModel{
			name:           require_string_member(source, 'name')!
			repository:     require_string_member(source, 'repository')!
			integration_id: require_integer_member(source, 'integration_id')!
			workflow_id:    require_integer_member(source, 'workflow_id')!
			workflow_path:  require_string_member(source, 'workflow_path')!
			event:          require_string_member(source, 'event')!
		}
	}
	return sources
}

fn durable_persisted_gate_runs_from_value(value JsonValue,
	path string) ![]PersistedGateRunModel {
	if value.kind != .array {
		return error('durable gate-run projection must be an array')
	}
	mut runs := []PersistedGateRunModel{}
	for index, run in value.array_value {
		durable_validate_persisted_gate_run_ints(run, '${path}/${index}')!
		runs << persisted_gate_run_from_json(run)!
	}
	return runs
}

fn durable_active_intent_from_value(value JsonValue) !ActiveIntentModel {
	if value.kind == .null_value {
		return ActiveIntentModel{}
	}
	deadlines := require_object_member(value, 'deadlines')!
	run_attempt := durable_int_member(value, 'run_attempt', '$/active_intent/run_attempt')!
	ordinal := durable_int_member(value, 'ordinal', '$/active_intent/ordinal')!
	infra_retry_count := durable_int_member(value, 'infra_retry_count',
		'$/active_intent/infra_retry_count')!
	source_retry_count := durable_int_member(value, 'source_retry_count',
		'$/active_intent/source_retry_count')!
	return ActiveIntentModel{
		intent_id:                 require_string_member(value, 'intent_id')!
		intent_type:               require_string_member(value, 'intent_type')!
		stage:                     require_string_member(value, 'stage')!
		run_id:                    require_integer_member(value, 'run_id')!
		run_attempt:               run_attempt
		ordinal:                   ordinal
		input_fingerprint:         require_string_member(value, 'input_fingerprint')!
		expected_canonical_head:   require_string_member(value, 'expected_canonical_head')!
		candidate_ref:             require_string_member(value, 'candidate_ref')!
		generation:                require_integer_member(value, 'generation')!
		resolved_inputs:           durable_resolved_inputs_from_value(require_member(value,
			'resolved_inputs')!)!
		expected_check_sources:    durable_check_sources_from_value(require_member(value,
			'expected_check_sources')!)!
		gate_runs:                 durable_persisted_gate_runs_from_value(require_member(value,
			'gate_runs')!, '$/active_intent/gate_runs')!
		gate_trigger_refs:         dt_string_array(require_array_member(value, 'gate_trigger_refs')!)!
		deadlines:                 IntentDeadlinesModel{
			build_deadline:     require_string_member(deadlines, 'build_deadline')!
			checks_deadline:    require_string_member(deadlines, 'checks_deadline')!
			promotion_deadline: require_string_member(deadlines, 'promotion_deadline')!
		}
		infra_retry_count:         infra_retry_count
		source_retry_count:        source_retry_count
		candidate_binding:         durable_candidate_binding_from_value(require_member(value,
			'candidate_binding')!)!
		validation_subject:        durable_validation_subject_from_value(require_member(value,
			'validation_subject')!)!
		previous_last_known_good:  durable_artifact_tuple_from_value(require_member(value,
			'previous_last_known_good')!)!
		bad_provisional:           durable_artifact_tuple_from_value(require_member(value,
			'bad_provisional')!)!
		rollback_diff_fingerprint: require_nullable_string_member(value,
			'rollback_diff_fingerprint')!
		rollback_provisional:      durable_candidate_binding_from_value(require_member(value,
			'rollback_provisional')!)!
	}
}

fn durable_source_refetch_from_value(value JsonValue) !SourceRefetchModel {
	if value.kind == .null_value {
		return SourceRefetchModel{}
	}
	return SourceRefetchModel{
		target_id:               require_string_member(value, 'target_id')!
		expected_generation:     require_integer_member(value, 'expected_generation')!
		expected_canonical_head: require_string_member(value, 'expected_canonical_head')!
		source_state_id:         require_string_member(value, 'source_state_id')!
		source_state_generation: require_integer_member(value, 'source_state_generation')!
		resolution_operation_id: require_string_member(value, 'resolution_operation_id')!
		source_id:               require_string_member(value, 'source_id')!
		source_repository:       require_string_member(value, 'source_repository')!
		requested_ref:           require_string_member(value, 'requested_ref')!
		previous_sha:            require_string_member(value, 'previous_sha')!
		resolved_sha:            require_nullable_string_member(value, 'resolved_sha')!
		resolved_tree:           require_nullable_string_member(value, 'resolved_tree')!
		status:                  require_string_member(value, 'status')!
		failure_kind:            require_nullable_string_member(value, 'failure_kind')!
		evidence_digest:         require_string_member(value, 'evidence_digest')!
		input_fingerprint:       require_string_member(value, 'input_fingerprint')!
		checked_at:              require_string_member(value, 'checked_at')!
		operation_id:            require_string_member(value, 'operation_id')!
	}
}

fn durable_head_observation_from_value(value JsonValue) !HeadObservationModel {
	if value.kind == .null_value {
		return HeadObservationModel{}
	}
	return HeadObservationModel{
		target_id:              require_string_member(value, 'target_id')!
		expected_generation:    require_integer_member(value, 'expected_generation')!
		expected_previous_head: require_string_member(value, 'expected_previous_head')!
		canonical_head:         require_string_member(value, 'canonical_head')!
		subject_sha:            require_string_member(value, 'subject_sha')!
		relationship:           durable_head_relationship(require_string_member(value,
			'relationship')!)!
		observed_at:            require_string_member(value, 'observed_at')!
		operation_id:           require_string_member(value, 'operation_id')!
		evidence_digest:        require_string_member(value, 'evidence_digest')!
	}
}

fn durable_applied_operations_from_value(value JsonValue) ![]AppliedOperationModel {
	if value.kind != .array {
		return error('durable applied-operation projection must be an array')
	}
	mut operations := []AppliedOperationModel{}
	for operation in value.array_value {
		operations << AppliedOperationModel{
			operation_id:         require_string_member(operation, 'operation_id')!
			transition:           require_string_member(operation, 'transition')!
			resulting_generation: require_integer_member(operation, 'resulting_generation')!
		}
	}
	return operations
}

fn durable_resolved_inputs_json(inputs ResolvedInputsModel) !JsonValue {
	if !resolved_inputs_is_set(inputs) {
		return dt_null()
	}
	mut sources := []JsonValue{}
	for source in inputs.sources {
		sources << object_value_from_pairs(['id', 'repository', 'ref', 'sha', 'tree'], [
			dt_string(source.id),
			dt_string(source.repository),
			dt_string(source.ref),
			dt_string(source.sha),
			dt_string(source.tree),
		])!
	}
	mut checks := []JsonValue{}
	for check in inputs.source_checks {
		checks << object_value_from_pairs(['source_id', 'resolved_sha', 'status', 'evidence_digest'], [
			dt_string(check.source_id),
			dt_string(check.resolved_sha),
			dt_string(check.status),
			dt_string(check.evidence_digest),
		])!
	}
	producer := object_value_from_pairs(['profile_id', 'profile_sha256', 'observation_sha256',
		'observation_digest'], [
		dt_string(inputs.producer_toolchain.profile_id),
		dt_string(inputs.producer_toolchain.profile_sha256),
		dt_string(inputs.producer_toolchain.observation_sha256),
		dt_string(inputs.producer_toolchain.observation_digest),
	])!
	return object_value_from_pairs(['sources', 'source_checks', 'recipe_path', 'recipe_hash',
		'contract_repository', 'contract_sha', 'v_source_sha', 'producer_toolchain'], [
		JsonValue{ kind: .array, array_value: sources },
		JsonValue{ kind: .array, array_value: checks },
		dt_string(inputs.recipe_path),
		dt_string(inputs.recipe_hash),
		dt_string(inputs.contract_repository),
		dt_string(inputs.contract_sha),
		dt_string(inputs.v_source_sha),
		producer,
	])
}

fn durable_artifact_tuple_json(tuple ArtifactTupleModel) !JsonValue {
	if !artifact_tuple_is_set(tuple) {
		return dt_null()
	}
	return object_value_from_pairs(['sha', 'tree', 'input_fingerprint', 'artifact_fingerprint',
		'manifest_hash', 'digests'], [
		dt_string(tuple.sha),
		dt_string(tuple.tree),
		dt_string(tuple.input_fingerprint),
		dt_string(tuple.artifact_fingerprint),
		dt_string(tuple.manifest_hash),
		durable_digests_json(tuple.digests)!,
	])
}

fn durable_candidate_binding_json(binding CandidateBindingModel) !JsonValue {
	if !candidate_binding_is_set(binding) {
		return dt_null()
	}
	return object_value_from_pairs(['sha', 'tree', 'parent', 'artifact_fingerprint', 'manifest_hash',
		'digests'], [
		dt_string(binding.sha),
		dt_string(binding.tree),
		dt_string(binding.parent),
		dt_string(binding.artifact_fingerprint),
		dt_string(binding.manifest_hash),
		durable_digests_json(binding.digests)!,
	])
}

fn durable_validation_subject_json(subject ValidationSubjectModel) !JsonValue {
	if !validation_subject_is_set(subject) {
		return dt_null()
	}
	return object_value_from_pairs(['sha', 'tree', 'input_fingerprint', 'artifact_fingerprint',
		'manifest_hash', 'digests', 'candidate_ref'], [
		dt_string(subject.sha),
		dt_string(subject.tree),
		dt_string(subject.input_fingerprint),
		dt_string(subject.artifact_fingerprint),
		dt_string(subject.manifest_hash),
		durable_digests_json(subject.digests)!,
		dt_string(subject.candidate_ref),
	])
}

fn durable_digests_json(digests []DigestModel) !JsonValue {
	mut values := []JsonValue{}
	for digest in digests {
		values << object_value_from_pairs(['path', 'sha256'], [
			dt_string(digest.path),
			dt_string(digest.sha256),
		])!
	}
	return JsonValue{
		kind:        .array
		array_value: values
	}
}

fn durable_check_sources_json(sources []CheckSourceModel) !JsonValue {
	mut values := []JsonValue{}
	for source in sources {
		values << object_value_from_pairs(['name', 'repository', 'integration_id', 'workflow_id',
			'workflow_path', 'event'], [
			dt_string(source.name),
			dt_string(source.repository),
			dt_integer(source.integration_id),
			dt_integer(source.workflow_id),
			dt_string(source.workflow_path),
			dt_string(source.event),
		])!
	}
	return JsonValue{
		kind:        .array
		array_value: values
	}
}

fn durable_persisted_gate_runs_json(runs []PersistedGateRunModel) !JsonValue {
	mut values := []JsonValue{}
	for run in runs {
		values << persisted_gate_run_json(run)!
	}
	return JsonValue{
		kind:        .array
		array_value: values
	}
}

fn durable_active_intent_json(intent ActiveIntentModel) !JsonValue {
	if !intent_is_set(intent) {
		return dt_null()
	}
	deadlines := object_value_from_pairs(['build_deadline', 'checks_deadline', 'promotion_deadline'], [
		dt_string(intent.deadlines.build_deadline),
		dt_string(intent.deadlines.checks_deadline),
		dt_string(intent.deadlines.promotion_deadline),
	])!
	return object_value_from_pairs(['intent_id', 'intent_type', 'stage', 'run_id', 'run_attempt',
		'ordinal', 'input_fingerprint', 'resolved_inputs', 'expected_canonical_head', 'candidate_ref',
		'generation', 'expected_check_sources', 'gate_runs', 'gate_trigger_refs', 'deadlines',
		'infra_retry_count', 'source_retry_count', 'candidate_binding', 'validation_subject',
		'previous_last_known_good', 'bad_provisional', 'rollback_diff_fingerprint',
		'rollback_provisional'], [
		dt_string(intent.intent_id),
		dt_string(intent.intent_type),
		dt_string(intent.stage),
		dt_integer(intent.run_id),
		dt_integer(intent.run_attempt),
		dt_integer(intent.ordinal),
		dt_string(intent.input_fingerprint),
		durable_resolved_inputs_json(intent.resolved_inputs)!,
		dt_string(intent.expected_canonical_head),
		dt_string(intent.candidate_ref),
		dt_integer(intent.generation),
		durable_check_sources_json(intent.expected_check_sources)!,
		durable_persisted_gate_runs_json(intent.gate_runs)!,
		dt_strings_json(intent.gate_trigger_refs),
		deadlines,
		dt_integer(intent.infra_retry_count),
		dt_integer(intent.source_retry_count),
		durable_candidate_binding_json(intent.candidate_binding)!,
		durable_validation_subject_json(intent.validation_subject)!,
		durable_artifact_tuple_json(intent.previous_last_known_good)!,
		durable_artifact_tuple_json(intent.bad_provisional)!,
		dt_nullable_string(intent.rollback_diff_fingerprint),
		durable_candidate_binding_json(intent.rollback_provisional)!,
	])
}

fn durable_native_subject_json(subject NativeGateSubjectModel) !JsonValue {
	if !native_subject_is_set(subject) {
		return dt_null()
	}
	return durable_ordered_native_subject_json(subject)
}

// Hashing deliberately uses native_gate_subject_json, whose digest projection is sorted. Durable
// state instead preserves the authenticated array order at both subject locations.
fn durable_ordered_native_subject_json(subject NativeGateSubjectModel) !JsonValue {
	trigger := if remediation_trigger_is_set(subject.remediation_trigger) {
		object_value_from_pairs(['repository', 'ref', 'before', 'after', 'tree', 'diff_fingerprint',
			'owner_domain'], [
			dt_string(subject.remediation_trigger.repository),
			dt_string(subject.remediation_trigger.ref),
			dt_string(subject.remediation_trigger.before),
			dt_string(subject.remediation_trigger.after),
			dt_string(subject.remediation_trigger.tree),
			dt_string(subject.remediation_trigger.diff_fingerprint),
			dt_string(subject.remediation_trigger.owner_domain),
		])!
	} else {
		dt_null()
	}
	return object_value_from_pairs(['consumer_id', 'consumer_kind', 'intent_or_operation_id',
		'target_id', 'subject_generation', 'initial_run_mode', 'remediation_trigger', 'sha', 'tree',
		'original_ref', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash', 'digests'], [
		dt_string(subject.consumer_id),
		dt_string(subject.consumer_kind),
		dt_string(subject.intent_or_operation_id),
		dt_string(subject.target_id),
		dt_integer(subject.subject_generation),
		dt_string(subject.initial_run_mode),
		trigger,
		dt_string(subject.sha),
		dt_string(subject.tree),
		dt_string(subject.original_ref),
		dt_string(subject.input_fingerprint),
		dt_string(subject.artifact_fingerprint),
		dt_string(subject.manifest_hash),
		durable_digests_json(subject.digests)!,
	])
}

fn durable_native_gate_json(gate NativeGateModel) !JsonValue {
	if !native_gate_is_set(gate) {
		return dt_null()
	}
	mut epochs := []JsonValue{}
	for epoch in gate.epochs {
		epochs << object_value_from_pairs(['epoch', 'reason', 'expected_ref', 'trigger_id', 'state',
			'selected_run_id', 'selected_run_attempt', 'selected_check_suite_id', 'conclusion',
			'opened_at', 'closed_at', 'source_recovery_operation_id'], [
			dt_integer(epoch.epoch),
			dt_string(epoch.reason),
			dt_string(epoch.expected_ref),
			dt_nullable_string(epoch.trigger_id),
			dt_string(epoch.state.str()),
			dt_nullable_positive_integer(epoch.selected_run_id),
			dt_nullable_positive_integer(epoch.selected_run_attempt),
			dt_nullable_positive_integer(epoch.selected_check_suite_id),
			dt_nullable_string(epoch.conclusion),
			dt_string(epoch.opened_at),
			dt_nullable_string(epoch.closed_at),
			dt_nullable_string(epoch.source_recovery_operation_id),
		])!
	}
	mut runs := []JsonValue{}
	for run in gate.gate_runs {
		runs << object_value_from_pairs(['gate_epoch', 'run_id', 'run_attempt', 'repository', 'ref',
			'sha', 'event', 'actor', 'actor_integration_id', 'triggering_actor',
			'triggering_actor_integration_id', 'check_suite_id', 'workflow_id', 'workflow_path',
			'created_at', 'conclusion'], [
			dt_integer(run.epoch),
			dt_integer(run.run_id),
			dt_integer(run.run_attempt),
			dt_string(run.repository),
			dt_string(run.ref),
			dt_string(run.sha),
			dt_string(run.event),
			dt_string(run.actor),
			dt_integer(run.actor_integration_id),
			dt_string(run.triggering_actor),
			dt_integer(run.triggering_actor_integration_id),
			dt_integer(run.check_suite_id),
			dt_integer(run.workflow_id),
			dt_string(run.workflow_path),
			dt_string(run.created_at),
			dt_string(run.conclusion),
		])!
	}
	return object_value_from_pairs(['subject', 'subject_hash', 'subject_sha', 'subject_generation',
		'repository', 'workflow_id', 'workflow_path', 'original_actor',
		'original_actor_integration_id', 'rerun_triggering_actor', 'rerun_triggering_integration_id',
		'expected_ledger_generation', 'active_gate_epoch', 'gate_epochs', 'gate_runs',
		'ack_operation_ids', 'completion_operation_ids', 'epoch_close_operation_ids',
		'selected_run_id', 'selected_run_attempt', 'selected_check_suite_id', 'selected_conclusion',
		'infra_retry_count', 'source_recovery_operation_id'], [
		durable_ordered_native_subject_json(gate.subject)!,
		dt_string(gate.subject_hash),
		dt_string(gate.subject_sha),
		dt_integer(gate.subject_generation),
		dt_string(gate.authentication.repository),
		dt_integer(gate.authentication.workflow_id),
		dt_string(gate.authentication.workflow_path),
		dt_string(gate.authentication.original_actor),
		dt_integer(gate.authentication.original_actor_integration_id),
		dt_string(gate.authentication.rerun_triggering_actor),
		dt_integer(gate.authentication.rerun_triggering_integration_id),
		dt_integer(gate.expected_ledger_generation),
		dt_integer(gate.active_gate_epoch),
		JsonValue{ kind: .array, array_value: epochs },
		JsonValue{ kind: .array, array_value: runs },
		dt_strings_json(gate.ack_operation_ids),
		dt_strings_json(gate.completion_operation_ids),
		dt_strings_json(gate.epoch_close_operation_ids),
		dt_nullable_positive_integer(gate.selected_run_id),
		dt_nullable_positive_integer(gate.selected_run_attempt),
		dt_nullable_positive_integer(gate.selected_check_suite_id),
		dt_nullable_string(gate.selected_conclusion),
		dt_integer(gate.infra_retry_count),
		dt_nullable_string(gate.source_recovery_operation_id),
	])
}

fn durable_source_refetch_json(refetch SourceRefetchModel) !JsonValue {
	if refetch == SourceRefetchModel{} {
		return dt_null()
	}
	return object_value_from_pairs(['target_id', 'expected_generation', 'expected_canonical_head',
		'source_state_id', 'source_state_generation', 'resolution_operation_id', 'source_id',
		'source_repository', 'requested_ref', 'previous_sha', 'resolved_sha', 'resolved_tree',
		'status', 'failure_kind', 'evidence_digest', 'input_fingerprint', 'checked_at',
		'operation_id'], [
		dt_string(refetch.target_id),
		dt_integer(refetch.expected_generation),
		dt_string(refetch.expected_canonical_head),
		dt_string(refetch.source_state_id),
		dt_integer(refetch.source_state_generation),
		dt_string(refetch.resolution_operation_id),
		dt_string(refetch.source_id),
		dt_string(refetch.source_repository),
		dt_string(refetch.requested_ref),
		dt_string(refetch.previous_sha),
		dt_nullable_string(refetch.resolved_sha),
		dt_nullable_string(refetch.resolved_tree),
		dt_string(refetch.status),
		dt_nullable_string(refetch.failure_kind),
		dt_string(refetch.evidence_digest),
		dt_string(refetch.input_fingerprint),
		dt_string(refetch.checked_at),
		dt_string(refetch.operation_id),
	])
}

fn durable_head_observation_json(observation HeadObservationModel) !JsonValue {
	if observation == HeadObservationModel{} {
		return dt_null()
	}
	return object_value_from_pairs(['target_id', 'expected_generation', 'expected_previous_head',
		'canonical_head', 'subject_sha', 'relationship', 'observed_at', 'operation_id',
		'evidence_digest'], [
		dt_string(observation.target_id),
		dt_integer(observation.expected_generation),
		dt_string(observation.expected_previous_head),
		dt_string(observation.canonical_head),
		dt_string(observation.subject_sha),
		dt_string(observation.relationship.str()),
		dt_string(observation.observed_at),
		dt_string(observation.operation_id),
		dt_string(observation.evidence_digest),
	])
}

fn durable_native_validation_json(record NativeValidationRecordModel) !JsonValue {
	if !native_validation_record_is_set(record) {
		return dt_null()
	}
	return native_validation_record_json(record)
}

fn durable_applied_operations_json(operations []AppliedOperationModel) !JsonValue {
	mut values := []JsonValue{}
	for operation in operations {
		values << object_value_from_pairs(['operation_id', 'transition', 'resulting_generation'], [
			dt_string(operation.operation_id),
			dt_string(operation.transition),
			dt_integer(operation.resulting_generation),
		])!
	}
	return JsonValue{
		kind:        .array
		array_value: values
	}
}

fn dt_null() JsonValue {
	return JsonValue{
		kind: .null_value
	}
}

fn dt_string(value string) JsonValue {
	return JsonValue{
		kind:         .string_value
		string_value: value
	}
}

fn dt_nullable_string(value string) JsonValue {
	if value == '' {
		return dt_null()
	}
	return dt_string(value)
}

fn dt_integer[T](value T) JsonValue {
	return JsonValue{
		kind:      .integer
		int_value: i64(value)
	}
}

fn dt_nullable_positive_integer[T](value T) JsonValue {
	if i64(value) == 0 {
		return dt_null()
	}
	return dt_integer(value)
}

fn dt_boolean(value bool) JsonValue {
	return JsonValue{
		kind:       .boolean
		bool_value: value
	}
}

fn dt_strings_json(values []string) JsonValue {
	return JsonValue{
		kind:        .array
		array_value: values.map(dt_string(it))
	}
}

fn dt_string_array(values []JsonValue) ![]string {
	mut result := []string{}
	for value in values {
		result << require_string(value)!
	}
	return result
}
