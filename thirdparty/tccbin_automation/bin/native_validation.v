module bin

import crypto.sha256
import os

const native_validation_evidence_max_bytes = u64(256 * 1024)

const native_validation_capsule_max_bytes = u64(16 * 1024 * 1024)

const native_validation_semantic_evidence_max_files = 1028

const native_validation_walker_max_entries = 1056

const native_validation_manifest_source_max_bytes = 512 * 1024

const native_validation_record_audience = 'vlang/v:tccbin-native-validation-record:v1'

struct NativeDirectoryHandle {
mut:
	handle voidptr
	fd     int = -1
	opened bool
}

struct NativeValidationEvidenceFile {
	sha256 string
	size   u64
}

// NativeValidationEvidenceModel is the compact durable projection of one authenticated capsule
// member. The bytes remain external until the separate T2c3 transport and retention migration.
pub struct NativeValidationEvidenceModel {
pub:
	sha256 string
	size   u64
}

// NativeValidationRecordModel is the complete replayable T2c2 validation fact. Every redundant
// target, consumer, subject, producer, selected-run and source field is derived from the retained
// raw manifest, canonical matrix and two full gate runs.
pub struct NativeValidationRecordModel {
pub:
	schema_version       int
	operation_id         string
	transition           string
	resulting_generation i64
	verdict              string
	manifest_source      string
	manifest_hash        string
	native_lane_matrix   JsonValue
	matrix_digest        string
	evidence             []NativeValidationEvidenceModel
	capsule_digest       string
	native_gate          PersistedGateRunModel
	v_smoke_gate         PersistedGateRunModel
	validation_digest    string
}

struct NativeValidationCapsuleFacts {
	manifest_source    string
	manifest_hash      string
	native_lane_matrix JsonValue
	matrix_digest      string
	evidence           []NativeValidationEvidenceModel
	capsule_digest     string
	subject            NativeGateSubjectModel
	subject_hash       string
	producer           ProducerToolchainModel
	selected_run       NativeLaneRunKey
	matrix_outcome     NativeLaneOutcome
}

// AuthenticatedNativeValidationCapsule seals one physical native-lane matrix and the exact
// content-addressed evidence set declared by its producer, validator, and lane results. T2c1 is
// deliberately dormant: this envelope is not a verdict, durable state, or publication authority.
pub struct AuthenticatedNativeValidationCapsule {
	manifest_hash string
	subject_hash  string
	matrix        AuthenticatedNativeLaneMatrix
	matrix_size   u64
	evidence      []NativeValidationEvidenceFile
	digest        string
}

// authenticate_native_validation_capsule authenticates the exact closed physical capsule. The
// caller supplies the already authenticated manifest and complete subject; neither is accepted
// from capsule-controlled bytes.
pub fn authenticate_native_validation_capsule(automation_root string,
	manifest AuthenticatedManifestModel, subject NativeGateSubjectModel,
	capsule_root string) !AuthenticatedNativeValidationCapsule {
	if capsule_root == '' || !os.is_abs_path(capsule_root)
		|| os.real_path(capsule_root) != capsule_root || os.is_link(capsule_root) {
		return error('native validation capsule root must be an exact absolute physical directory')
	}
	root_path_before := native_directory_path_snapshot(capsule_root) or {
		return error('native validation capsule root must be an exact absolute physical directory')
	}
	mut root, root_before := native_open_directory(capsule_root) or {
		return error('native validation capsule root must be an exact absolute physical directory')
	}
	defer {
		native_close_directory(mut root)
	}
	if root_path_before != root_before {
		return error('native validation capsule root path does not identify its open directory')
	}
	mut root_entries_before := native_directory_entries_bounded(&root, 2) or {
		return error('native validation capsule root differs from its exact closed entries')
	}
	root_entries_before.sort()
	if root_entries_before != ['evidence', 'native-lane-matrix.json'] {
		return error('native validation capsule root differs from its exact closed entries')
	}

	require_native_validation_child_name('evidence')!
	mut evidence_root, evidence_root_before := native_open_child_directory(&root, 'evidence') or {
		return error('native validation evidence directory must be an exact physical directory')
	}
	defer {
		native_close_directory(mut evidence_root)
	}
	evidence_lookup_before := native_validation_child_directory_snapshot(&root, 'evidence') or {
		return error('native validation evidence directory must be an exact physical directory')
	}
	if evidence_lookup_before != evidence_root_before {
		return error('native validation evidence directory entry does not identify its open handle')
	}

	matrix_source, matrix_size := native_validation_matrix_source(&root)!
	matrix := authenticate_native_lane_matrix_exact_source(automation_root, manifest, subject,
		matrix_source)!

	expected, declaration_count := native_validation_expected_evidence(manifest, matrix)!
	if declaration_count > native_validation_semantic_evidence_max_files
		|| expected.len > native_validation_semantic_evidence_max_files {
		return error('native validation capsule evidence count exceeds its strict bound')
	}
	mut evidence_names := expected.keys()
	evidence_names.sort()
	mut entries_before := native_directory_entries_bounded(&evidence_root, evidence_names.len) or {
		return error('native validation capsule evidence directory differs from the exact declared set')
	}
	entries_before.sort()
	if entries_before != evidence_names {
		return error('native validation capsule evidence directory differs from the exact declared set')
	}

	mut total_bytes := matrix_size
	if total_bytes > native_validation_capsule_max_bytes {
		return error('native validation capsule exceeds its strict total byte bound')
	}
	mut evidence := []NativeValidationEvidenceFile{cap: evidence_names.len}
	for evidence_sha256 in evidence_names {
		require_native_validation_child_name(evidence_sha256)!
		observed := hash_stable_native_validation_evidence(&evidence_root, evidence_sha256)!
		if observed.sha256 != evidence_sha256 {
			return error('native validation evidence bytes differ from their filename digest')
		}
		if observed.size > native_validation_capsule_max_bytes - total_bytes {
			return error('native validation capsule exceeds its strict total byte bound')
		}
		total_bytes += observed.size
		evidence << observed
	}

	mut entries_after := native_directory_entries_bounded(&evidence_root, evidence_names.len) or {
		return error('native validation capsule evidence directory differs from the exact declared set')
	}
	entries_after.sort()
	if entries_after != entries_before {
		return error('native validation evidence directory changed while being authenticated')
	}
	evidence_root_after := native_directory_snapshot(&evidence_root) or {
		return error('native validation evidence directory identity changed while being authenticated')
	}
	if evidence_root_after != evidence_root_before {
		return error('native validation evidence directory identity changed while being authenticated')
	}
	evidence_lookup_after := native_validation_child_directory_snapshot(&root, 'evidence') or {
		return error('native validation evidence directory entry changed while being authenticated')
	}
	if evidence_lookup_after != evidence_root_after {
		return error('native validation evidence directory entry changed while being authenticated')
	}
	mut root_entries_after := native_directory_entries_bounded(&root, 2) or {
		return error('native validation capsule root differs from its exact closed entries')
	}
	root_entries_after.sort()
	if root_entries_after != root_entries_before {
		return error('native validation capsule root changed while being authenticated')
	}
	root_after := native_directory_snapshot(&root) or {
		return error('native validation capsule root identity changed while being authenticated')
	}
	if root_after != root_before {
		return error('native validation capsule root identity changed while being authenticated')
	}
	root_path_after := native_directory_path_snapshot(capsule_root) or {
		return error('native validation capsule root path changed while being authenticated')
	}
	if root_path_after != root_after {
		return error('native validation capsule root path changed while being authenticated')
	}

	manifest_hash := manifest.fingerprints.manifest_hash
	subject_hash := native_gate_subject_hash(subject)!
	matrix_digest := authenticated_native_lane_matrix_digest(matrix)!
	digest := native_validation_capsule_digest_projection(manifest_hash, subject_hash,
		matrix_digest, matrix_size, evidence)!
	return AuthenticatedNativeValidationCapsule{
		manifest_hash: manifest_hash
		subject_hash:  subject_hash
		matrix:        matrix
		matrix_size:   matrix_size
		evidence:      evidence
		digest:        digest
	}
}

// authenticated_native_validation_capsule_digest is the sole T2c1 accessor. It rederives the
// compact digest from sealed fields and never turns the capsule into an authorization decision.
pub fn authenticated_native_validation_capsule_digest(capsule AuthenticatedNativeValidationCapsule) !string {
	if !is_lower_hex_64(capsule.manifest_hash) || !is_lower_hex_64(capsule.subject_hash)
		|| !is_lower_hex_64(capsule.digest) || capsule.evidence.len == 0
		|| capsule.evidence.len > native_validation_semantic_evidence_max_files {
		return error('authenticated native validation capsule is empty or malformed')
	}
	matrix_digest := authenticated_native_lane_matrix_digest(capsule.matrix)!
	if capsule.matrix_size != u64(capsule.matrix.raw_source.len) {
		return error('authenticated native validation capsule matrix size diverged')
	}
	recomputed := native_validation_capsule_digest_projection(capsule.manifest_hash,
		capsule.subject_hash, matrix_digest, capsule.matrix_size, capsule.evidence)!
	if recomputed != capsule.digest {
		return error('authenticated native validation capsule digest diverged from its sealed facts')
	}
	return capsule.digest
}

fn authenticated_native_validation_capsule_facts(manifest AuthenticatedManifestModel,
	subject NativeGateSubjectModel, capsule AuthenticatedNativeValidationCapsule) !NativeValidationCapsuleFacts {
	validate_authenticated_manifest(manifest)!
	if manifest.raw_source.len == 0
		|| manifest.raw_source.len > native_validation_manifest_source_max_bytes {
		return error('native validation manifest source exceeds its durable byte bound')
	}
	expected_subject_hash := native_gate_subject_hash(subject)!
	if capsule.manifest_hash != manifest.fingerprints.manifest_hash
		|| capsule.subject_hash != expected_subject_hash {
		return error('native validation capsule differs from its manifest or complete subject')
	}
	matrix := authenticated_native_lane_matrix_facts(manifest, subject, capsule.matrix)!
	matrix_digest := authenticated_native_lane_matrix_digest(capsule.matrix)!
	if capsule.matrix_size != u64(capsule.matrix.raw_source.len)
		|| capsule.matrix.raw_source != canonical_json(capsule.matrix.matrix)
		|| capsule.matrix_size == 0 || capsule.matrix_size > toolchain_identity_document_max_bytes {
		return error('native validation capsule matrix differs from its exact canonical bytes')
	}
	expected, declaration_count := native_validation_expected_evidence(manifest, capsule.matrix)!
	if declaration_count > native_validation_semantic_evidence_max_files
		|| expected.len > native_validation_semantic_evidence_max_files
		|| capsule.evidence.len != expected.len {
		return error('native validation capsule evidence differs from its exact declared set')
	}
	mut expected_names := expected.keys()
	expected_names.sort()
	mut evidence := []NativeValidationEvidenceModel{cap: capsule.evidence.len}
	mut total_bytes := capsule.matrix_size
	for index, item in capsule.evidence {
		if item.sha256 != expected_names[index] || !is_lower_hex_64(item.sha256) || item.size == 0
			|| item.size > native_validation_evidence_max_bytes
			|| item.size > native_validation_capsule_max_bytes - total_bytes {
			return error('native validation capsule evidence differs from its exact declared set')
		}
		total_bytes += item.size
		evidence << NativeValidationEvidenceModel{
			sha256: item.sha256
			size:   item.size
		}
	}
	capsule_digest := native_validation_capsule_digest_projection(manifest.fingerprints.manifest_hash,
		expected_subject_hash, matrix_digest, capsule.matrix_size, capsule.evidence)!
	if authenticated_native_validation_capsule_digest(capsule)! != capsule_digest {
		return error('native validation capsule digest differs from its exact sealed projection')
	}
	return NativeValidationCapsuleFacts{
		manifest_source:    manifest.raw_source
		manifest_hash:      manifest.fingerprints.manifest_hash
		native_lane_matrix: capsule.matrix.matrix
		matrix_digest:      matrix_digest
		evidence:           evidence
		capsule_digest:     capsule_digest
		subject:            subject
		subject_hash:       expected_subject_hash
		producer:           producer_toolchain_model(manifest.producer)!
		selected_run:       matrix.selected_run
		matrix_outcome:     matrix.outcome
	}
}

fn native_validation_record_from_facts(operation_id string, transition string,
	resulting_generation i64, verdict string, facts NativeValidationCapsuleFacts,
	native_gate PersistedGateRunModel, v_smoke_gate PersistedGateRunModel) !NativeValidationRecordModel {
	mut record := NativeValidationRecordModel{
		schema_version:       1
		operation_id:         operation_id
		transition:           transition
		resulting_generation: resulting_generation
		verdict:              verdict
		manifest_source:      facts.manifest_source
		manifest_hash:        facts.manifest_hash
		native_lane_matrix:   facts.native_lane_matrix
		matrix_digest:        facts.matrix_digest
		evidence:             facts.evidence
		capsule_digest:       facts.capsule_digest
		native_gate:          native_gate
		v_smoke_gate:         v_smoke_gate
		validation_digest:    '0'.repeat(64)
	}
	record = NativeValidationRecordModel{
		...record
		validation_digest: native_validation_record_digest(native_validation_record_json(record)!)!
	}
	validate_native_validation_record(record)!
	return record
}

fn native_validation_record_is_set(record NativeValidationRecordModel) bool {
	return record.schema_version != 0 || record.operation_id != '' || record.transition != ''
		|| record.resulting_generation != 0 || record.verdict != '' || record.manifest_source != ''
		|| record.manifest_hash != '' || record.native_lane_matrix.kind != .null_value
		|| record.matrix_digest != '' || record.evidence.len != 0 || record.capsule_digest != ''
		|| record.native_gate != PersistedGateRunModel{}
		|| record.v_smoke_gate != PersistedGateRunModel{} || record.validation_digest != ''
}

// native_validation_record_json is the single durable codec for the closed fourteen-field model.
pub fn native_validation_record_json(record NativeValidationRecordModel) !JsonValue {
	mut evidence := []JsonValue{cap: record.evidence.len}
	for item in record.evidence {
		evidence << object_value_from_pairs(['sha256', 'size'], [
			JsonValue{ kind: .string_value, string_value: item.sha256 },
			JsonValue{ kind: .integer, int_value: i64(item.size) },
		])!
	}
	return object_value_from_pairs(['schema_version', 'operation_id', 'transition',
		'resulting_generation', 'verdict', 'manifest_source', 'manifest_hash', 'native_lane_matrix',
		'matrix_digest', 'evidence', 'capsule_digest', 'native_gate', 'v_smoke_gate',
		'validation_digest'], [
		JsonValue{ kind: .integer, int_value: record.schema_version },
		JsonValue{ kind: .string_value, string_value: record.operation_id },
		JsonValue{ kind: .string_value, string_value: record.transition },
		JsonValue{ kind: .integer, int_value: record.resulting_generation },
		JsonValue{ kind: .string_value, string_value: record.verdict },
		JsonValue{ kind: .string_value, string_value: record.manifest_source },
		JsonValue{ kind: .string_value, string_value: record.manifest_hash },
		record.native_lane_matrix,
		JsonValue{ kind: .string_value, string_value: record.matrix_digest },
		JsonValue{ kind: .array, array_value: evidence },
		JsonValue{ kind: .string_value, string_value: record.capsule_digest },
		persisted_gate_run_json(record.native_gate)!,
		persisted_gate_run_json(record.v_smoke_gate)!,
		JsonValue{ kind: .string_value, string_value: record.validation_digest },
	])!
}

fn native_validation_record_from_json(value JsonValue) !NativeValidationRecordModel {
	require_exact_keys(value, ['schema_version', 'operation_id', 'transition', 'resulting_generation',
		'verdict', 'manifest_source', 'manifest_hash', 'native_lane_matrix', 'matrix_digest',
		'evidence', 'capsule_digest', 'native_gate', 'v_smoke_gate', 'validation_digest'])!
	mut evidence := []NativeValidationEvidenceModel{}
	for item in require_array_member(value, 'evidence')! {
		require_exact_keys(item, ['sha256', 'size'])!
		size := require_integer_member(item, 'size')!
		if size <= 0 {
			return error('last native validation evidence size is invalid')
		}
		evidence << NativeValidationEvidenceModel{
			sha256: require_string_member(item, 'sha256')!
			size:   u64(size)
		}
	}
	return NativeValidationRecordModel{
		schema_version:       int(require_integer_member(value, 'schema_version')!)
		operation_id:         require_string_member(value, 'operation_id')!
		transition:           require_string_member(value, 'transition')!
		resulting_generation: require_integer_member(value, 'resulting_generation')!
		verdict:              require_string_member(value, 'verdict')!
		manifest_source:      require_string_member(value, 'manifest_source')!
		manifest_hash:        require_string_member(value, 'manifest_hash')!
		native_lane_matrix:   require_object_member(value, 'native_lane_matrix')!
		matrix_digest:        require_string_member(value, 'matrix_digest')!
		evidence:             evidence
		capsule_digest:       require_string_member(value, 'capsule_digest')!
		native_gate:          persisted_gate_run_from_json(require_object_member(value,
			'native_gate')!)!
		v_smoke_gate:         persisted_gate_run_from_json(require_object_member(value,
			'v_smoke_gate')!)!
		validation_digest:    require_string_member(value, 'validation_digest')!
	}
}

// validate_native_validation_record_authority replays durable JSON against the current reviewed
// registry, profile, manifest schema and complete resolved-input projection. It deliberately uses
// the non-staged manifest constructor: opaque and production records remain fail-closed until the
// separate T2c3 writer can supply an observed staging authority.
fn validate_native_validation_record_authority(automation_root string,
	record NativeValidationRecordModel, resolved_inputs JsonValue, input_fingerprint string,
	artifact_fingerprint string, manifest_hash string) !NativeValidationCapsuleFacts {
	validate_native_validation_record(record)!
	authenticated := authenticate_manifest_source(automation_root, record.manifest_source) or {
		return error('last native validation manifest lacks replayable non-staged authority: ${err}')
	}
	fingerprints := authenticated_manifest_fingerprints(authenticated)!
	if fingerprints.manifest_hash != record.manifest_hash
		|| fingerprints.manifest_hash != manifest_hash
		|| fingerprints.input_fingerprint != input_fingerprint
		|| fingerprints.artifact_fingerprint != artifact_fingerprint {
		return error('last native validation manifest projections differ from the target root')
	}
	validate_authenticated_manifest_resolved_inputs_value(authenticated, resolved_inputs,
		input_fingerprint) or {
		return error('last native validation resolved inputs differ from the authenticated manifest: ${err}')
	}
	producer := producer_toolchain_model(authenticated.producer)!
	subject := native_subject_from_recovery(parse_receiver_subject(require_object_member(record.native_lane_matrix,
		'subject')!)!)
	matrix_source := canonical_json(record.native_lane_matrix)
	matrix := authenticate_native_lane_matrix_exact_source(automation_root, authenticated, subject,
		matrix_source) or {
		return error('last native validation matrix lacks exact reviewed validator authority: ${err}')
	}
	matrix_facts := authenticated_native_lane_matrix_facts(authenticated, subject, matrix)!
	record_facts := native_validation_record_facts(record)!
	if matrix_facts.matrix_digest != record.matrix_digest
		|| matrix_facts.subject_hash != record_facts.subject_hash
		|| matrix_facts.selected_run != record_facts.selected_run
		|| matrix_facts.outcome != record_facts.matrix_outcome || producer != record_facts.producer {
		return error('last native validation reviewed authority differs from its durable projection')
	}
	return record_facts
}

$if test {
	// validate_native_validation_record_authority_for_test exposes the complete durable replay only
	// to the test module without widening the production helper surface.
	pub fn validate_native_validation_record_authority_for_test(automation_root string,
	record NativeValidationRecordModel, resolved_inputs JsonValue, input_fingerprint string,
	artifact_fingerprint string, manifest_hash string) ! {
		validate_native_validation_record_authority(automation_root, record, resolved_inputs,
			input_fingerprint, artifact_fingerprint, manifest_hash)!
	}

	// resolved_inputs_manifest_projection_for_test returns the seven manifest-backed members after
	// the closed root and its independent source-check evidence have been validated.
	pub fn resolved_inputs_manifest_projection_for_test(inputs JsonValue) !JsonValue {
		return resolved_inputs_json_manifest_projection(inputs)!
	}
}

// native_validation_record_digest binds the complete record while excluding only its own digest.
pub fn native_validation_record_digest(record JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in record.object_keys {
		if key != 'validation_digest' {
			keys << key
			values << record.object_values[index]
		}
	}
	mut projection := object_value_from_pairs(keys, values)!
	projection = append_object_members(projection, ['audience'], [
		JsonValue{ kind: .string_value, string_value: native_validation_record_audience },
	])!
	return json_sha256(projection)
}

fn validate_native_validation_record(record NativeValidationRecordModel) ! {
	if record.schema_version != 1 || !is_lower_hex_64(record.operation_id)
		|| record.transition !in native_validation_record_transitions()
		|| record.resulting_generation <= 0
		|| record.verdict !in ['green', 'functional', 'infrastructure']
		|| record.manifest_source.len == 0
		|| record.manifest_source.len > native_validation_manifest_source_max_bytes
		|| !is_lower_hex_64(record.manifest_hash) || !is_lower_hex_64(record.matrix_digest)
		|| !is_lower_hex_64(record.capsule_digest) || !is_lower_hex_64(record.validation_digest) {
		return error('last native validation identity, version, verdict, or digest is invalid')
	}
	manifest := parse_strict_json(record.manifest_source)!
	if sha256.sum256(record.manifest_source.bytes()).hex() != record.manifest_hash {
		return error('last native validation manifest hash differs from its exact source bytes')
	}
	matrix_source := canonical_json(record.native_lane_matrix)
	if record.native_lane_matrix.kind != .object || matrix_source.len == 0
		|| matrix_source.len > toolchain_identity_document_max_bytes
		|| sha256.sum256(matrix_source.bytes()).hex() != record.matrix_digest {
		return error('last native validation matrix digest differs from its canonical object')
	}
	validate_native_validation_matrix_replay(manifest, record.native_lane_matrix)!
	subject := require_object_member(record.native_lane_matrix, 'subject')!
	subject_hash := require_string_member(record.native_lane_matrix, 'subject_hash')!
	if json_sha256(subject) != subject_hash
		|| require_string_member(subject, 'manifest_hash')! != record.manifest_hash
		|| require_string_member(subject, 'target_id')! != require_string_member(manifest, 'target_id')! {
		return error('last native validation matrix subject differs from its manifest')
	}
	consumer_kind := require_string_member(subject, 'consumer_kind')!
	if !native_validation_transition_matches_outcome(record.transition, record.verdict,
		consumer_kind) {
		return error('last native validation transition differs from its subject and verdict')
	}
	profile_id, profile_sha256, producer := manifest_toolchain_members(manifest)!
	if producer.kind != .object {
		return error('last native validation manifest lacks its producer observation')
	}
	producer_projection := require_object_member(record.native_lane_matrix, 'producer_toolchain')!
	if require_string_member(producer_projection, 'profile_id')! != profile_id
		|| require_string_member(producer_projection, 'profile_sha256')! != profile_sha256
		|| require_string_member(producer_projection, 'observation_sha256')! != json_sha256(producer)
		|| require_string_member(producer_projection, 'observation_digest')! != toolchain_observation_digest(producer)! {
		return error('last native validation producer differs from its exact manifest source')
	}
	expected, declaration_count := native_validation_record_expected_evidence(producer,
		record.native_lane_matrix)!
	if declaration_count > native_validation_semantic_evidence_max_files
		|| expected.len != record.evidence.len {
		return error('last native validation evidence differs from the exact declared set')
	}
	mut expected_names := expected.keys()
	expected_names.sort()
	mut projected_evidence := []NativeValidationEvidenceFile{cap: record.evidence.len}
	mut total_bytes := u64(matrix_source.bytes().len)
	for index, item in record.evidence {
		if item.sha256 != expected_names[index] || !is_lower_hex_64(item.sha256) || item.size == 0
			|| item.size > native_validation_evidence_max_bytes
			|| item.size > native_validation_capsule_max_bytes - total_bytes {
			return error('last native validation evidence differs from the exact declared set')
		}
		total_bytes += item.size
		projected_evidence << NativeValidationEvidenceFile{
			sha256: item.sha256
			size:   item.size
		}
	}
	recomputed_capsule := native_validation_capsule_digest_projection(record.manifest_hash,
		subject_hash, record.matrix_digest, u64(matrix_source.len), projected_evidence)!
	if recomputed_capsule != record.capsule_digest {
		return error('last native validation capsule digest differs from its durable projection')
	}
	encoded := native_validation_record_json(record)!
	if native_validation_record_digest(encoded)! != record.validation_digest {
		return error('last native validation digest differs from its complete durable facts')
	}
}

fn native_validation_transition_matches_outcome(transition string, verdict string,
	consumer_kind string) bool {
	expected_green := transition in ['bootstrap_green', 'remediation_green', 'candidate_checks_green',
		'post_check_green', 'rollback_post_green']
	if expected_green != (verdict == 'green') {
		return false
	}
	if transition == 'post_check_red' && verdict != 'functional' {
		return false
	}
	if transition == 'post_check_infra_exhausted' && verdict != 'infrastructure' {
		return false
	}
	if transition in ['bootstrap_red', 'remediation_red', 'candidate_failed', 'rollback_failed']
		&& verdict !in ['functional', 'infrastructure'] {
		return false
	}
	return match transition {
		'bootstrap_green', 'bootstrap_red' {
			consumer_kind == 'initial_adopt_current'
		}
		'remediation_green', 'remediation_red' {
			consumer_kind == 'remediation'
		}
		'candidate_checks_green', 'candidate_failed' {
			consumer_kind in ['publish_candidate', 'rollback_candidate', 'adopt_current',
				'initial_adopt_current']
		}
		'post_check_green', 'post_check_red', 'post_check_infra_exhausted' {
			consumer_kind == 'publish_post'
		}
		'rollback_post_green' {
			consumer_kind == 'rollback_post'
		}
		'rollback_failed' {
			consumer_kind in ['rollback_candidate', 'rollback_post']
		}
		else {
			false
		}
	}
}

fn validate_native_validation_matrix_replay(manifest JsonValue, matrix JsonValue) ! {
	require_exact_keys(matrix, ['schema_version', 'subject', 'subject_hash', 'producer_toolchain',
		'selected_run', 'validator_observation', 'results'])!
	if require_integer_member(matrix, 'schema_version')! != 1 {
		return error('last native validation matrix version is not supported')
	}
	subject_value := require_object_member(matrix, 'subject')!
	subject := native_subject_from_recovery(parse_receiver_subject(subject_value)!)
	if !json_equal(subject_value, native_gate_subject_json(subject)!)
		|| require_string_member(matrix, 'subject_hash')! != native_gate_subject_hash(subject)! {
		return error('last native validation matrix subject is not one exact closed subject')
	}
	producer := require_object_member(matrix, 'producer_toolchain')!
	require_exact_keys(producer, ['profile_id', 'profile_sha256', 'observation_sha256',
		'observation_digest'])!
	selected := require_object_member(matrix, 'selected_run')!
	require_exact_keys(selected, ['run_id', 'run_attempt', 'check_suite_id'])!
	if require_integer_member(selected, 'run_id')! <= 0
		|| require_integer_member(selected, 'run_attempt')! !in [i64(1), 2]
		|| require_integer_member(selected, 'check_suite_id')! <= 0 {
		return error('last native validation selected run key is invalid')
	}
	profile_id, profile_sha256, _ := manifest_toolchain_members(manifest)!
	validator := require_object_member(matrix, 'validator_observation')!
	require_exact_keys(validator, ['schema_version', 'target_id', 'profile_id', 'profile_sha256',
		'phase', 'roles', 'observation_digest'])!
	if require_integer_member(validator, 'schema_version')! != 1
		|| require_string_member(validator, 'target_id')! != require_string_member(manifest, 'target_id')!
		|| require_string_member(validator, 'profile_id')! != profile_id
		|| require_string_member(validator, 'profile_sha256')! != profile_sha256
		|| require_string_member(validator, 'phase')! != 'validator'
		|| require_string_member(validator, 'observation_digest')! != toolchain_observation_digest(validator)! {
		return error('last native validation validator observation is not self-authenticating')
	}
	roles := ordered_toolchain_roles(validator, 'roles')!
	if roles.len == 0 || roles.len > 16 {
		return error('last native validation validator role set is invalid')
	}
	for role in roles {
		require_exact_keys(role, ['role_id', 'identity_strategy', 'resolved_identity',
			'resolution_digest', 'evidence_sha256'])!
		if require_string_member(role, 'identity_strategy')! !in ['github-hosted', 'cpa-host', 'cpa-guest', 'github-hosted-msys2']
			|| !is_lower_hex_64(require_string_member(role, 'evidence_sha256')!)
			|| require_string_member(role, 'resolution_digest')! != toolchain_role_resolution_digest(validator, role)! {
			return error('last native validation validator role is not self-authenticating')
		}
		facts := ordered_toolchain_facts(role, 'resolved_identity')!
		if facts.len == 0 || facts.len > 16 {
			return error('last native validation validator identity fact set is invalid')
		}
		for fact in facts {
			require_exact_keys(fact, ['name', 'value'])!
		}
	}
	results := require_array_member(matrix, 'results')!
	for result in results {
		require_exact_keys(result, ['probe_id', 'lane_id', 'required', 'status', 'expected_count',
			'evidence_sha256', 'fallback_used', 'object_linked', 'consumer_group'])!
	}
	authenticate_native_lane_results(manifest, results)!
}

fn native_validation_record_facts(record NativeValidationRecordModel) !NativeValidationCapsuleFacts {
	validate_native_validation_record(record)!
	manifest := parse_strict_json(record.manifest_source)!
	_, _, producer_observation := manifest_toolchain_members(manifest)!
	producer := ProducerToolchainModel{
		profile_id:         require_string_member(require_object_member(record.native_lane_matrix,
			'producer_toolchain')!, 'profile_id')!
		profile_sha256:     require_string_member(require_object_member(record.native_lane_matrix,
			'producer_toolchain')!, 'profile_sha256')!
		observation_sha256: json_sha256(producer_observation)
		observation_digest: toolchain_observation_digest(producer_observation)!
	}
	subject := native_subject_from_recovery(parse_receiver_subject(require_object_member(record.native_lane_matrix,
		'subject')!)!)
	selected := require_object_member(record.native_lane_matrix, 'selected_run')!
	mut outcome := NativeLaneOutcome.green
	for result in require_array_member(record.native_lane_matrix, 'results')! {
		status := require_string_member(result, 'status')!
		if status == 'failed' || require_bool_member(result, 'fallback_used')! {
			outcome = .functional
		} else if status == 'blocked' && outcome == .green {
			outcome = .infrastructure
		}
	}
	return NativeValidationCapsuleFacts{
		manifest_source:    record.manifest_source
		manifest_hash:      record.manifest_hash
		native_lane_matrix: record.native_lane_matrix
		matrix_digest:      record.matrix_digest
		evidence:           record.evidence
		capsule_digest:     record.capsule_digest
		subject:            subject
		subject_hash:       require_string_member(record.native_lane_matrix, 'subject_hash')!
		producer:           producer
		selected_run:       NativeLaneRunKey{
			run_id:         require_integer_member(selected, 'run_id')!
			run_attempt:    int(require_integer_member(selected, 'run_attempt')!)
			check_suite_id: require_integer_member(selected, 'check_suite_id')!
		}
		matrix_outcome:     outcome
	}
}

fn native_validation_record_expected_evidence(producer JsonValue,
	matrix JsonValue) !(map[string]bool, int) {
	mut expected := map[string]bool{}
	mut declaration_count := 0
	validator := require_object_member(matrix, 'validator_observation')!
	for observation in [producer, validator] {
		for role in require_array_member(observation, 'roles')! {
			digest := require_string_member(role, 'evidence_sha256')!
			if !is_lower_hex_64(digest) {
				return error('last native validation toolchain evidence digest is invalid')
			}
			declaration_count++
			expected[digest] = true
		}
	}
	for result in require_array_member(matrix, 'results')! {
		digest := require_string_member(result, 'evidence_sha256')!
		if !is_lower_hex_64(digest) {
			return error('last native validation lane evidence digest is invalid')
		}
		declaration_count++
		expected[digest] = true
	}
	return expected, declaration_count
}

fn native_validation_record_transitions() []string {
	return ['bootstrap_green', 'bootstrap_red', 'remediation_green', 'remediation_red',
		'candidate_checks_green', 'candidate_failed', 'post_check_green', 'post_check_red',
		'post_check_infra_exhausted', 'rollback_post_green', 'rollback_failed']
}

fn persisted_gate_run_json(gate PersistedGateRunModel) !JsonValue {
	return object_value_from_pairs(['check_name', 'repository', 'integration_id', 'workflow_id',
		'workflow_path', 'event', 'run_id', 'run_attempt', 'check_suite_id',
		'check_suite_integration_id', 'job_id', 'subject_hash', 'check_run_id', 'external_id',
		'run_name', 'run_url', 'job_url', 'details_url', 'ref', 'workflow_head_sha', 'sha',
		'check_sha', 'actor', 'actor_integration_id', 'triggering_actor',
		'triggering_actor_integration_id', 'created_at', 'completed_at', 'run_conclusion',
		'check_conclusion', 'output_digest', 'evidence_digest'], [
		JsonValue{ kind: .string_value, string_value: gate.check_name },
		JsonValue{ kind: .string_value, string_value: gate.repository },
		JsonValue{ kind: .integer, int_value: gate.integration_id },
		JsonValue{ kind: .integer, int_value: gate.workflow_id },
		JsonValue{ kind: .string_value, string_value: gate.workflow_path },
		JsonValue{ kind: .string_value, string_value: gate.event },
		JsonValue{ kind: .integer, int_value: gate.run_id },
		JsonValue{ kind: .integer, int_value: gate.run_attempt },
		JsonValue{ kind: .integer, int_value: gate.check_suite_id },
		JsonValue{ kind: .integer, int_value: gate.check_suite_integration_id },
		JsonValue{ kind: .integer, int_value: gate.job_id },
		JsonValue{ kind: .string_value, string_value: gate.subject_hash },
		JsonValue{ kind: .integer, int_value: gate.check_run_id },
		JsonValue{ kind: .string_value, string_value: gate.external_id },
		JsonValue{ kind: .string_value, string_value: gate.run_name },
		JsonValue{ kind: .string_value, string_value: gate.run_url },
		JsonValue{ kind: .string_value, string_value: gate.job_url },
		JsonValue{ kind: .string_value, string_value: gate.details_url },
		JsonValue{ kind: .string_value, string_value: gate.ref },
		JsonValue{ kind: .string_value, string_value: gate.workflow_head_sha },
		JsonValue{ kind: .string_value, string_value: gate.sha },
		JsonValue{ kind: .string_value, string_value: gate.check_sha },
		JsonValue{ kind: .string_value, string_value: gate.actor },
		JsonValue{ kind: .integer, int_value: gate.actor_integration_id },
		JsonValue{ kind: .string_value, string_value: gate.triggering_actor },
		JsonValue{ kind: .integer, int_value: gate.triggering_actor_integration_id },
		JsonValue{ kind: .string_value, string_value: gate.created_at },
		JsonValue{ kind: .string_value, string_value: gate.completed_at },
		JsonValue{ kind: .string_value, string_value: gate.run_conclusion },
		JsonValue{ kind: .string_value, string_value: gate.check_conclusion },
		JsonValue{ kind: .string_value, string_value: gate.output_digest },
		JsonValue{ kind: .string_value, string_value: gate.evidence_digest },
	])!
}

fn persisted_gate_run_from_json(gate JsonValue) !PersistedGateRunModel {
	require_exact_keys(gate, ['check_name', 'repository', 'integration_id', 'workflow_id',
		'workflow_path', 'event', 'run_id', 'run_attempt', 'check_suite_id',
		'check_suite_integration_id', 'job_id', 'subject_hash', 'check_run_id', 'external_id',
		'run_name', 'run_url', 'job_url', 'details_url', 'ref', 'workflow_head_sha', 'sha',
		'check_sha', 'actor', 'actor_integration_id', 'triggering_actor',
		'triggering_actor_integration_id', 'created_at', 'completed_at', 'run_conclusion',
		'check_conclusion', 'output_digest', 'evidence_digest'])!
	return PersistedGateRunModel{
		check_name:                      require_string_member(gate, 'check_name')!
		repository:                      require_string_member(gate, 'repository')!
		integration_id:                  require_integer_member(gate, 'integration_id')!
		workflow_id:                     require_integer_member(gate, 'workflow_id')!
		workflow_path:                   require_string_member(gate, 'workflow_path')!
		event:                           require_string_member(gate, 'event')!
		run_id:                          require_integer_member(gate, 'run_id')!
		run_attempt:                     int(require_integer_member(gate, 'run_attempt')!)
		check_suite_id:                  require_integer_member(gate, 'check_suite_id')!
		check_suite_integration_id:      require_integer_member(gate, 'check_suite_integration_id')!
		job_id:                          require_integer_member(gate, 'job_id')!
		subject_hash:                    require_string_member(gate, 'subject_hash')!
		check_run_id:                    require_integer_member(gate, 'check_run_id')!
		external_id:                     require_string_member(gate, 'external_id')!
		run_name:                        require_string_member(gate, 'run_name')!
		run_url:                         require_string_member(gate, 'run_url')!
		job_url:                         require_string_member(gate, 'job_url')!
		details_url:                     require_string_member(gate, 'details_url')!
		ref:                             require_string_member(gate, 'ref')!
		workflow_head_sha:               require_string_member(gate, 'workflow_head_sha')!
		sha:                             require_string_member(gate, 'sha')!
		check_sha:                       require_string_member(gate, 'check_sha')!
		actor:                           require_string_member(gate, 'actor')!
		actor_integration_id:            require_integer_member(gate, 'actor_integration_id')!
		triggering_actor:                require_string_member(gate, 'triggering_actor')!
		triggering_actor_integration_id: require_integer_member(gate,
			'triggering_actor_integration_id')!
		created_at:                      require_string_member(gate, 'created_at')!
		completed_at:                    require_string_member(gate, 'completed_at')!
		run_conclusion:                  require_string_member(gate, 'run_conclusion')!
		check_conclusion:                require_string_member(gate, 'check_conclusion')!
		output_digest:                   require_string_member(gate, 'output_digest')!
		evidence_digest:                 require_string_member(gate, 'evidence_digest')!
	}
}

fn native_validation_expected_evidence(manifest AuthenticatedManifestModel,
	matrix AuthenticatedNativeLaneMatrix) !(map[string]bool, int) {
	mut expected := map[string]bool{}
	mut declaration_count := 0
	for observation in [manifest.producer.observation, matrix.validator.observation] {
		roles := require_array_member(observation, 'roles')!
		for role in roles {
			evidence_sha256 := require_string_member(role, 'evidence_sha256')!
			if !is_lower_hex_64(evidence_sha256) {
				return error('native validation toolchain evidence digest is invalid')
			}
			declaration_count++
			expected[evidence_sha256] = true
		}
	}
	for result in matrix.results {
		if !is_lower_hex_64(result.evidence_sha256) {
			return error('native validation lane evidence digest is invalid')
		}
		declaration_count++
		expected[result.evidence_sha256] = true
	}
	return expected, declaration_count
}

fn native_validation_capsule_digest_projection(manifest_hash string, subject_hash string,
	matrix_digest string, matrix_size u64, evidence []NativeValidationEvidenceFile) !string {
	if !is_lower_hex_64(manifest_hash) || !is_lower_hex_64(subject_hash)
		|| !is_lower_hex_64(matrix_digest) || evidence.len == 0
		|| evidence.len > native_validation_semantic_evidence_max_files || matrix_size == 0
		|| matrix_size > toolchain_identity_document_max_bytes {
		return error('native validation capsule digest projection is invalid')
	}
	mut evidence_sources := []string{cap: evidence.len}
	mut previous := ''
	mut total_bytes := matrix_size
	for item in evidence {
		if !is_lower_hex_64(item.sha256) || item.sha256 <= previous || item.size == 0
			|| item.size > native_validation_evidence_max_bytes {
			return error('native validation capsule evidence projection is invalid')
		}
		if item.size > native_validation_capsule_max_bytes - total_bytes {
			return error('native validation capsule evidence projection exceeds its byte bound')
		}
		total_bytes += item.size
		evidence_sources << '{"sha256":"${item.sha256}","size":${item.size}}'
		previous = item.sha256
	}
	projection_source := '{"evidence":[${evidence_sources.join(',')}],"manifest_hash":"${manifest_hash}","matrix_digest":"${matrix_digest}","schema_version":1,"subject_hash":"${subject_hash}"}'
	projection := parse_strict_json(projection_source)!
	if canonical_json(projection) != projection_source {
		return error('native validation capsule digest projection is not canonical')
	}
	return sha256.sum256(projection_source.bytes()).hex()
}

fn hash_stable_native_validation_evidence(parent &NativeDirectoryHandle,
	name string) !NativeValidationEvidenceFile {
	mut document, handle_before := native_open_child_document(parent, name) or {
		return error('native validation evidence cannot be opened as a physical regular file')
	}
	defer {
		native_close_toolchain_document(mut document)
	}
	require_physical_toolchain_document_snapshot(handle_before, 'native validation evidence')!
	if handle_before.identity.nlink != 1 {
		return error('native validation evidence must have exactly one physical link')
	}
	if handle_before.size == 0 || handle_before.size > native_validation_evidence_max_bytes {
		return error('native validation evidence is empty or exceeds its strict byte bound')
	}
	mut digest := sha256.new()
	mut buffer := []u8{len: toolchain_identity_document_buffer_bytes}
	mut total := u64(0)
	for {
		read := native_read_toolchain_document(&document, mut buffer) or {
			return error('native validation evidence cannot be read inside its byte bound')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > native_validation_evidence_max_bytes {
			return error('native validation evidence grew beyond its strict byte bound')
		}
		digest.write(buffer[..read])!
	}
	handle_after := native_toolchain_document_snapshot(&document) or {
		return error('native validation evidence identity changed while being read')
	}
	require_physical_toolchain_document_snapshot(handle_after, 'native validation evidence')!
	if handle_before != handle_after || handle_after.identity.nlink != 1 {
		return error('native validation evidence changed while being read')
	}
	lookup_after := native_validation_child_document_snapshot(parent, name) or {
		return error('native validation evidence entry changed while being read')
	}
	if lookup_after != handle_after {
		return error('native validation evidence entry changed while being read')
	}
	if total != handle_before.size {
		return error('native validation evidence changed while being read')
	}
	return NativeValidationEvidenceFile{
		sha256: digest.sum([]).hex()
		size:   total
	}
}

fn native_validation_matrix_source(parent &NativeDirectoryHandle) !(string, u64) {
	name := 'native-lane-matrix.json'
	require_native_validation_child_name(name)!
	mut document, handle_before := native_open_child_document(parent, name) or {
		return error('native validation capsule matrix is not a physical regular file')
	}
	defer {
		native_close_toolchain_document(mut document)
	}
	require_physical_toolchain_document_snapshot(handle_before, 'native validation capsule matrix')!
	if handle_before.size > toolchain_identity_document_max_bytes {
		return error('native lane matrix exceeds its strict byte bound')
	}
	mut bytes := []u8{cap: int(handle_before.size)}
	mut buffer := []u8{len: toolchain_identity_document_buffer_bytes}
	mut total := u64(0)
	for {
		read := native_read_toolchain_document(&document, mut buffer) or {
			return error('native validation capsule matrix cannot be read inside its byte bound')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > toolchain_identity_document_max_bytes {
			return error('native lane matrix exceeds its strict byte bound')
		}
		bytes << buffer[..read]
	}
	handle_after := native_toolchain_document_snapshot(&document) or {
		return error('native validation capsule matrix changed while being authenticated')
	}
	require_physical_toolchain_document_snapshot(handle_after, 'native validation capsule matrix')!
	if handle_before != handle_after || total != handle_before.size {
		return error('native validation capsule matrix changed while being authenticated')
	}
	lookup_after := native_validation_child_document_snapshot(parent, name) or {
		return error('native validation capsule matrix entry changed while being authenticated')
	}
	if lookup_after != handle_after {
		return error('native validation capsule matrix entry changed while being authenticated')
	}
	return bytes.bytestr(), total
}

fn native_validation_child_directory_snapshot(parent &NativeDirectoryHandle,
	name string) !NativeFileSnapshot {
	require_native_validation_child_name(name)!
	snapshot := native_child_snapshot(parent, name)!
	if snapshot.regular {
		return error('native validation child directory entry is not a physical directory')
	}
	return snapshot
}

fn native_validation_child_document_snapshot(parent &NativeDirectoryHandle,
	name string) !NativeFileSnapshot {
	require_native_validation_child_name(name)!
	snapshot := native_child_snapshot(parent, name)!
	if !snapshot.regular {
		return error('native validation child document entry is not a physical regular file')
	}
	return snapshot
}

fn require_native_validation_child_name(name string) ! {
	if name == '' || name in ['.', '..'] || name.contains('/') || name.contains('\\')
		|| name.bytes().any(it == 0) {
		return error('native validation child name is not one exact path component')
	}
}
