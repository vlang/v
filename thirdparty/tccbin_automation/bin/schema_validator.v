module bin

import crypto.sha256
import os

// SchemaIssue is one deterministic validation failure.
pub struct SchemaIssue {
pub:
	path    string
	message string
}

struct SchemaContext {
	root_dir string
}

// validate_json_file validates one strict JSON document against a local contract schema.
pub fn validate_json_file(schema_path string, input_path string) ![]SchemaIssue {
	schema_source := os.read_file(schema_path)!
	input_source := os.read_file(input_path)!
	schema := parse_strict_json(schema_source)!
	instance := parse_strict_json(input_source)!
	context := SchemaContext{
		root_dir: os.dir(os.real_path(schema_path))
	}
	mut issues := context.validate(schema, instance, '$', os.real_path(schema_path))!
	if issues.len == 0 && os.file_name(schema_path) == 'target-state.schema.json' {
		issues << validate_target_state_schema_semantics(instance, os.dir(context.root_dir))!
	} else if issues.len == 0 && os.file_name(schema_path) == 'source-state.schema.json' {
		issues << validate_source_state_schema_semantics(instance)!
	}
	return issues
}

// validate_json_value validates an already parsed value against a schema file.
pub fn validate_json_value(schema_path string, instance JsonValue) ![]SchemaIssue {
	schema := parse_strict_json(os.read_file(schema_path)!)!
	context := SchemaContext{
		root_dir: os.dir(os.real_path(schema_path))
	}
	mut issues := context.validate(schema, instance, '$', os.real_path(schema_path))!
	if issues.len == 0 && os.file_name(schema_path) == 'target-state.schema.json' {
		issues << validate_target_state_schema_semantics(instance, os.dir(context.root_dir))!
	} else if issues.len == 0 && os.file_name(schema_path) == 'source-state.schema.json' {
		issues << validate_source_state_schema_semantics(instance)!
	}
	return issues
}

fn (context SchemaContext) validate(schema JsonValue, instance JsonValue, path string,
	current_schema_path string) ![]SchemaIssue {
	if schema.kind == .boolean {
		if schema.bool_value {
			return []SchemaIssue{}
		}
		return [SchemaIssue{path, 'boolean false schema rejected the value'}]
	}
	if schema.kind != .object {
		return error('schema at ${current_schema_path} is not an object or boolean')
	}
	mut issues := []SchemaIssue{}
	if reference := schema.object_value('$ref') {
		if reference.kind != .string_value {
			return error('schema $ref must be a string')
		}
		resolved_schema, resolved_path := context.resolve_reference(reference.string_value,
			current_schema_path)!
		issues << context.validate(resolved_schema, instance, path, resolved_path)!
	}
	if expected_type := schema.object_value('type') {
		if !schema_type_matches(expected_type, instance) {
			issues << SchemaIssue{path, 'expected type ${canonical_json(expected_type)}, got ${instance.kind}'}
			return issues
		}
	}
	if constant := schema.object_value('const') {
		if !json_equal(constant, instance) {
			issues << SchemaIssue{path, 'value does not match const'}
		}
	}
	if alternatives := schema.object_value('enum') {
		if alternatives.kind != .array {
			return error('schema enum must be an array')
		}
		if !alternatives.array_value.any(json_equal(it, instance)) {
			issues << SchemaIssue{path, 'value is outside the closed enum'}
		}
	}
	if all_of := schema.object_value('allOf') {
		for child in require_schema_array(all_of, 'allOf')! {
			issues << context.validate(child, instance, path, current_schema_path)!
		}
	}
	if any_of := schema.object_value('anyOf') {
		mut passed := false
		for child in require_schema_array(any_of, 'anyOf')! {
			if context.validate(child, instance, path, current_schema_path)!.len == 0 {
				passed = true
				break
			}
		}
		if !passed {
			issues << SchemaIssue{path, 'no anyOf branch matched'}
		}
	}
	if one_of := schema.object_value('oneOf') {
		mut passes := 0
		for child in require_schema_array(one_of, 'oneOf')! {
			if context.validate(child, instance, path, current_schema_path)!.len == 0 {
				passes++
			}
		}
		if passes != 1 {
			issues << SchemaIssue{path, 'expected exactly one oneOf branch, got ${passes}'}
		}
	}
	if denied := schema.object_value('not') {
		if context.validate(denied, instance, path, current_schema_path)!.len == 0 {
			issues << SchemaIssue{path, 'value matched a forbidden schema'}
		}
	}
	if condition := schema.object_value('if') {
		condition_passed := context.validate(condition, instance, path, current_schema_path)!.len == 0
		if condition_passed {
			if then_schema := schema.object_value('then') {
				issues << context.validate(then_schema, instance, path, current_schema_path)!
			}
		} else if else_schema := schema.object_value('else') {
			issues << context.validate(else_schema, instance, path, current_schema_path)!
		}
	}
	match instance.kind {
		.object {
			if schema_has_any(schema, ['properties', 'required', 'additionalProperties',
				'minProperties', 'maxProperties'])
			{
				issues << context.validate_object(schema, instance, path, current_schema_path)!
			}
		}
		.array {
			if schema_has_any(schema, ['prefixItems', 'items', 'minItems', 'maxItems', 'uniqueItems',
				'contains', 'minContains', 'maxContains'])
			{
				issues << context.validate_array(schema, instance, path, current_schema_path)!
			}
		}
		.string_value {
			if schema_has_any(schema, ['pattern', 'minLength', 'maxLength']) {
				issues << validate_string(schema, instance, path)!
			}
		}
		.integer {
			if schema_has_any(schema, ['minimum', 'maximum']) {
				issues << validate_integer(schema, instance, path)!
			}
		}
		else {}
	}
	return issues
}

fn (context SchemaContext) validate_object(schema JsonValue, instance JsonValue, path string,
	current_schema_path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	if minimum := schema.object_value('minProperties') {
		if minimum.kind != .integer {
			return error('minProperties must be an integer')
		}
		if i64(instance.object_keys.len) < minimum.int_value {
			issues << SchemaIssue{path, 'object has fewer than ${minimum.int_value} properties'}
		}
	}
	if maximum := schema.object_value('maxProperties') {
		if maximum.kind != .integer {
			return error('maxProperties must be an integer')
		}
		if i64(instance.object_keys.len) > maximum.int_value {
			issues << SchemaIssue{path, 'object has more than ${maximum.int_value} properties'}
		}
	}
	if required := schema.object_value('required') {
		if required.kind != .array {
			return error('required must be an array')
		}
		for entry in required.array_value {
			if entry.kind != .string_value {
				return error('required entries must be strings')
			}
			if !instance.has_object_key(entry.string_value) {
				issues << SchemaIssue{path, 'missing required property ${entry.string_value}'}
			}
		}
	}
	properties := schema.object_value('properties') or {
		JsonValue{
			kind: .object
		}
	}
	if properties.kind != .object {
		return error('properties must be an object')
	}
	additional := schema.object_value('additionalProperties') or {
		JsonValue{
			kind:       .boolean
			bool_value: true
		}
	}
	if additional.kind != .boolean && additional.kind != .object {
		return error('additionalProperties must be boolean or a schema')
	}
	for index, key in instance.object_keys {
		child_path := '${path}/${escape_pointer(key)}'
		if property_schema := properties.object_value(key) {
			issues << context.validate(property_schema, instance.object_values[index], child_path,
				current_schema_path)!
		} else if additional.kind == .boolean {
			if !additional.bool_value {
				issues << SchemaIssue{child_path, 'unknown property'}
			}
		} else {
			issues << context.validate(additional, instance.object_values[index], child_path,
				current_schema_path)!
		}
	}
	return issues
}

fn (context SchemaContext) validate_array(schema JsonValue, instance JsonValue, path string,
	current_schema_path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	if minimum := schema.object_value('minItems') {
		if minimum.kind != .integer {
			return error('minItems must be an integer')
		}
		if i64(instance.array_value.len) < minimum.int_value {
			issues << SchemaIssue{path, 'array has fewer than ${minimum.int_value} items'}
		}
	}
	if maximum := schema.object_value('maxItems') {
		if maximum.kind != .integer {
			return error('maxItems must be an integer')
		}
		if i64(instance.array_value.len) > maximum.int_value {
			issues << SchemaIssue{path, 'array has more than ${maximum.int_value} items'}
		}
	}
	if unique := schema.object_value('uniqueItems') {
		if unique.kind != .boolean {
			return error('uniqueItems must be boolean')
		}
		if unique.bool_value {
			mut seen := []string{}
			for item in instance.array_value {
				canonical := canonical_json(item)
				if canonical in seen {
					issues << SchemaIssue{path, 'array items are not unique'}
					break
				}
				seen << canonical
			}
		}
	}
	mut prefix_length := 0
	if prefix_schemas := schema.object_value('prefixItems') {
		if prefix_schemas.kind != .array {
			return error('prefixItems must be an array')
		}
		prefix_length = prefix_schemas.array_value.len
		for index, prefix_schema in prefix_schemas.array_value {
			if index >= instance.array_value.len {
				break
			}
			issues << context.validate(prefix_schema, instance.array_value[index],
				'${path}/${index}', current_schema_path)!
		}
	}
	if item_schema := schema.object_value('items') {
		for index, item in instance.array_value {
			if index >= prefix_length {
				issues << context.validate(item_schema, item, '${path}/${index}',
					current_schema_path)!
			}
		}
	}
	if contains_schema := schema.object_value('contains') {
		mut matches := i64(0)
		for item in instance.array_value {
			if context.validate(contains_schema, item, path, current_schema_path)!.len == 0 {
				matches++
			}
		}
		minimum := if value := schema.object_value('minContains') {
			if value.kind != .integer || value.int_value < 0 {
				return error('minContains must be a non-negative integer')
			}
			value.int_value
		} else {
			i64(1)
		}
		if matches < minimum {
			issues << SchemaIssue{path, 'array contains fewer than ${minimum} matching items'}
		}
		if maximum := schema.object_value('maxContains') {
			if maximum.kind != .integer || maximum.int_value < 0 {
				return error('maxContains must be a non-negative integer')
			}
			if matches > maximum.int_value {
				issues << SchemaIssue{path, 'array contains more than ${maximum.int_value} matching items'}
			}
		}
	}
	return issues
}

fn validate_string(schema JsonValue, instance JsonValue, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	rune_count := instance.string_value.runes().len
	if minimum := schema.object_value('minLength') {
		if minimum.kind != .integer {
			return error('minLength must be an integer')
		}
		if i64(rune_count) < minimum.int_value {
			issues << SchemaIssue{path, 'string is shorter than ${minimum.int_value} code points'}
		}
	}
	if maximum := schema.object_value('maxLength') {
		if maximum.kind != .integer {
			return error('maxLength must be an integer')
		}
		if i64(rune_count) > maximum.int_value {
			issues << SchemaIssue{path, 'string is longer than ${maximum.int_value} code points'}
		}
	}
	if pattern := schema.object_value('pattern') {
		if pattern.kind != .string_value {
			return error('pattern must be a string')
		}
		if !matches_json_pattern(pattern.string_value, instance.string_value)! {
			issues << SchemaIssue{path, 'string does not match the required pattern'}
		}
	}
	return issues
}

fn validate_integer(schema JsonValue, instance JsonValue, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	if minimum := schema.object_value('minimum') {
		if minimum.kind != .integer {
			return error('minimum must be an integer')
		}
		if instance.int_value < minimum.int_value {
			issues << SchemaIssue{path, 'integer is below ${minimum.int_value}'}
		}
	}
	if maximum := schema.object_value('maximum') {
		if maximum.kind != .integer {
			return error('maximum must be an integer')
		}
		if instance.int_value > maximum.int_value {
			issues << SchemaIssue{path, 'integer is above ${maximum.int_value}'}
		}
	}
	return issues
}

fn schema_type_matches(expected JsonValue, instance JsonValue) bool {
	if expected.kind == .string_value {
		return json_type_name(instance.kind) == expected.string_value
	}
	if expected.kind == .array {
		return expected.array_value.any(it.kind == .string_value
			&& json_type_name(instance.kind) == it.string_value)
	}
	return false
}

fn json_type_name(kind JsonKind) string {
	return match kind {
		.null_value { 'null' }
		.boolean { 'boolean' }
		.integer { 'integer' }
		.string_value { 'string' }
		.array { 'array' }
		.object { 'object' }
	}
}

fn require_schema_array(value JsonValue, keyword string) ![]JsonValue {
	if value.kind != .array {
		return error('${keyword} must be an array')
	}
	return value.array_value
}

fn (context SchemaContext) resolve_reference(reference string,
	current_schema_path string) !(JsonValue, string) {
	if reference.contains('://') || reference.starts_with('/') || reference.contains('\\') {
		return error('network, absolute, and non-POSIX schema references are forbidden')
	}
	parts := reference.split_nth('#', 2)
	file_part := parts[0]
	pointer := if parts.len == 2 { parts[1] } else { '' }
	if file_part.contains('..') {
		return error('parent traversal in schema reference is forbidden')
	}
	resolved_path := if file_part == '' {
		current_schema_path
	} else {
		os.real_path(os.join_path(context.root_dir, file_part))
	}
	if os.dir(resolved_path) != context.root_dir {
		return error('schema reference escaped the contract schema directory')
	}
	mut resolved := parse_strict_json(os.read_file(resolved_path)!)!
	if pointer != '' {
		if !pointer.starts_with('/') {
			return error('schema fragment must be a JSON pointer')
		}
		for raw_segment in pointer[1..].split('/') {
			segment := raw_segment.replace('~1', '/').replace('~0', '~')
			if resolved.kind == .object {
				resolved = resolved.object_value(segment) or {
					return error('unresolved schema pointer ${reference}')
				}
			} else if resolved.kind == .array {
				index := segment.int()
				if index < 0 || index >= resolved.array_value.len || index.str() != segment {
					return error('invalid schema array pointer ${reference}')
				}
				resolved = resolved.array_value[index]
			} else {
				return error('schema pointer traverses a scalar value')
			}
		}
	}
	return resolved, resolved_path
}

fn escape_pointer(value string) string {
	return value.replace('~', '~0').replace('/', '~1')
}

fn schema_has_any(schema JsonValue, keywords []string) bool {
	return keywords.any(schema.has_object_key(it))
}

// validate_target_state_schema_semantics closes the cross-object invariants that JSON Schema
// cannot express: the native subject, both gate projections and every CAS operation describe one
// immutable validation subject. It is deliberately invoked only after structural validation.
fn validate_target_state_schema_semantics(root JsonValue,
	automation_root string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	issues << validate_applied_operation_ledger_semantics(root)!
	issues << validate_recovery_handoff_history_semantics(root)!
	issues << validate_target_resolved_inputs_semantics(root)!
	issues << validate_last_native_validation_semantics(root, automation_root)!
	subject := require_member(root, 'native_gate_subject')!
	smoke := require_member(root, 'v_smoke_execution')!
	if subject.kind == .null_value {
		if smoke.kind != .null_value {
			issues << semantic_issue('$/v_smoke_execution',
				'V smoke cannot exist without a native gate subject')
		}
		issues << validate_gate_run_semantics(root, '', JsonValue{ kind: .null_value })!
		return issues
	}
	if smoke.kind == .null_value {
		issues << semantic_issue('$/v_smoke_execution',
			'native subject and V smoke must be reserved atomically')
		return issues
	}
	recovery_subject := parse_receiver_subject(subject) or {
		issues << semantic_issue('$/native_gate_subject',
			'native subject violates its canonical consumer/ref contract: ${err}')
		return issues
	}
	subject_hash := native_gate_subject_hash(native_subject_from_recovery(recovery_subject)) or {
		issues << semantic_issue('$/native_gate_subject',
			'native subject cannot produce its canonical hash: ${err}')
		return issues
	}
	if require_nullable_string_member(root, 'active_subject_hash')! != subject_hash {
		issues << semantic_issue('$/active_subject_hash',
			'active subject hash does not match the canonical native subject')
	}
	native_execution := require_object_member(root, 'native_gate_execution')!
	if !json_equal(require_object_member(native_execution, 'subject')!, subject) {
		issues << semantic_issue('$/native_gate_execution/subject',
			'native execution subject differs from the authoritative target subject')
	}
	if require_string_member(native_execution, 'subject_hash')! != subject_hash {
		issues << semantic_issue('$/native_gate_execution/subject_hash',
			'native execution subject hash is not canonical')
	}
	if require_string_member(native_execution, 'subject_sha')! != require_string_member(subject, 'sha')!
		|| require_integer_member(native_execution, 'subject_generation')! != require_integer_member(subject, 'subject_generation')! {
		issues << semantic_issue('$/native_gate_execution',
			'native execution SHA or subject generation is not bound to its subject')
	}
	generation := require_integer_member(root, 'generation')!
	if require_integer_member(native_execution, 'expected_ledger_generation')! != generation {
		issues << semantic_issue('$/native_gate_execution/expected_ledger_generation',
			'native execution CAS generation is stale')
	}
	issues << validate_native_subject_owner_semantics(root, subject)!
	issues << validate_v_smoke_execution_semantics(root, subject, subject_hash, smoke)!
	issues << validate_gate_run_semantics(root, subject_hash, subject)!
	return issues
}

fn validate_target_resolved_inputs_semantics(root JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	resolved_inputs := require_member(root, 'resolved_inputs')!
	intent := require_member(root, 'active_intent')!
	if resolved_inputs.kind == .null_value {
		if require_nullable_string_member(root, 'input_fingerprint')! != '' {
			issues << semantic_issue('$/input_fingerprint',
				'unresolved target must not retain an input fingerprint')
		}
		if !require_bool_member(root, 'bootstrap_required')! {
			issues << semantic_issue('$/resolved_inputs',
				'seeded target must retain complete root resolved inputs')
		}
		if intent.kind == .object {
			issues << semantic_issue('$/resolved_inputs',
				'an active intent requires complete root resolved inputs')
		}
		if require_nullable_string_member(root, 'active_remediation_id')! != ''
			|| require_nullable_string_member(root, 'active_subject_hash')! != ''
			|| require_string_member(root, 'target_state')! == 'eligible' {
			issues << semantic_issue('$/resolved_inputs',
				'a durable validation consumer requires complete root resolved inputs')
		}
		return issues
	}
	if intent.kind == .object
		&& !json_equal(resolved_inputs, require_member(intent, 'resolved_inputs')!) {
		issues << semantic_issue('$/resolved_inputs',
			'active intent resolved inputs must equal the complete target root')
	}
	if intent.kind == .object
		&& require_string_member(intent, 'input_fingerprint')! != require_nullable_string_member(root, 'input_fingerprint')! {
		issues << semantic_issue('$/active_intent/input_fingerprint',
			'active intent input fingerprint must equal the complete target root')
	}
	if require_nullable_string_member(root, 'input_fingerprint')! == '' {
		issues << semantic_issue('$/input_fingerprint',
			'resolved inputs require a complete target input fingerprint')
	}
	return issues
}

fn validate_last_native_validation_semantics(root JsonValue,
	automation_root string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	value := require_member(root, 'last_native_validation')!
	intent := require_member(root, 'active_intent')!
	if value.kind == .null_value {
		return issues
	}
	record := native_validation_record_from_json(value) or {
		issues << semantic_issue('$/last_native_validation',
			'last native validation cannot decode its exact fourteen fields')
		return issues
	}
	validate_native_validation_record_authority(automation_root, record, require_member(root,
		'resolved_inputs')!, require_nullable_string_member(root, 'input_fingerprint')!, require_nullable_string_member(root,
		'artifact_fingerprint')!, require_nullable_string_member(root, 'manifest_hash')!) or {
		issues << semantic_issue('$/last_native_validation/manifest_source',
			'last native validation does not replay current reviewed authority: ${err}')
	}
	manifest_source := require_string_member(value, 'manifest_source')!
	manifest := parse_strict_json(manifest_source) or {
		issues << semantic_issue('$/last_native_validation/manifest_source',
			'last native validation manifest source is not strict JSON')
		return issues
	}
	manifest_hash := require_string_member(value, 'manifest_hash')!
	if manifest_source.bytes().len > native_validation_manifest_source_max_bytes
		|| sha256.sum256(manifest_source.bytes()).hex() != manifest_hash {
		issues << semantic_issue('$/last_native_validation/manifest_hash',
			'last native validation manifest hash differs from its exact bounded source')
	}
	matrix := require_object_member(value, 'native_lane_matrix')!
	matrix_source := canonical_json(matrix)
	matrix_digest := require_string_member(value, 'matrix_digest')!
	if matrix_source.bytes().len > toolchain_identity_document_max_bytes
		|| sha256.sum256(matrix_source.bytes()).hex() != matrix_digest {
		issues << semantic_issue('$/last_native_validation/matrix_digest',
			'last native validation matrix digest differs from its canonical object')
	}
	validate_native_validation_matrix_replay(manifest, matrix) or {
		issues << semantic_issue('$/last_native_validation/native_lane_matrix',
			'last native validation matrix does not replay its closed manifest contract')
	}
	subject := require_object_member(matrix, 'subject')!
	subject_hash := require_string_member(matrix, 'subject_hash')!
	if json_sha256(subject) != subject_hash
		|| require_string_member(subject, 'target_id')! != require_string_member(root, 'target_id')!
		|| require_string_member(subject, 'manifest_hash')! != manifest_hash
		|| require_string_member(manifest, 'target_id')! != require_string_member(root, 'target_id')! {
		issues << semantic_issue('$/last_native_validation/native_lane_matrix/subject',
			'last native validation subject is not joined to its target and manifest')
	}
	transition := require_string_member(value, 'transition')!
	verdict := require_string_member(value, 'verdict')!
	if !native_validation_transition_matches_outcome(transition, verdict, require_string_member(subject,
		'consumer_kind')!) {
		issues << semantic_issue('$/last_native_validation/transition',
			'last native validation transition differs from its subject and verdict')
	}
	profile_id, profile_sha256, producer_observation := manifest_toolchain_members(manifest) or {
		issues << semantic_issue('$/last_native_validation/manifest_source',
			'last native validation manifest lacks its producer observation')
		return issues
	}
	producer := require_object_member(matrix, 'producer_toolchain')!
	if require_string_member(producer, 'profile_id')! != profile_id
		|| require_string_member(producer, 'profile_sha256')! != profile_sha256
		|| require_string_member(producer, 'observation_sha256')! != json_sha256(producer_observation)
		|| require_string_member(producer, 'observation_digest')! != toolchain_observation_digest(producer_observation)! {
		issues << semantic_issue('$/last_native_validation/native_lane_matrix/producer_toolchain',
			'last native validation producer differs from its exact manifest source')
	}
	resolved := require_member(root, 'resolved_inputs')!
	mut v_source_sha := ''
	if resolved.kind != .object {
		issues << semantic_issue('$/last_native_validation/native_lane_matrix/producer_toolchain',
			'last native validation producer differs from target resolved inputs')
	} else {
		v_source_sha = require_string_member(resolved, 'v_source_sha')!
		if !json_equal(producer, require_object_member(resolved, 'producer_toolchain')!) {
			issues << semantic_issue('$/last_native_validation/native_lane_matrix/producer_toolchain',
				'last native validation producer differs from target resolved inputs')
		}
	}
	expected, declaration_count := native_validation_record_expected_evidence(producer_observation,
		matrix) or {
		issues << semantic_issue('$/last_native_validation/evidence',
			'last native validation evidence declarations are invalid')
		return issues
	}
	evidence := require_array_member(value, 'evidence')!
	mut expected_names := expected.keys()
	expected_names.sort()
	mut durable_evidence := []NativeValidationEvidenceFile{cap: evidence.len}
	mut total_bytes := u64(matrix_source.bytes().len)
	for index, item in evidence {
		sha := require_string_member(item, 'sha256')!
		size := require_integer_member(item, 'size')!
		if index >= expected_names.len || sha != expected_names[index] || size <= 0
			|| size > i64(native_validation_evidence_max_bytes)
			|| u64(size) > native_validation_capsule_max_bytes - total_bytes {
			issues << semantic_issue('$/last_native_validation/evidence/${index}',
				'last native validation evidence differs from the sorted exact declared set')
		} else {
			total_bytes += u64(size)
		}
		durable_evidence << NativeValidationEvidenceFile{
			sha256: sha
			size:   u64(size)
		}
	}
	if declaration_count > native_validation_semantic_evidence_max_files
		|| evidence.len != expected_names.len {
		issues << semantic_issue('$/last_native_validation/evidence',
			'last native validation evidence cardinality differs from its declarations')
	} else {
		expected_capsule := native_validation_capsule_digest_projection(manifest_hash,
			subject_hash, matrix_digest, u64(matrix_source.bytes().len), durable_evidence) or { '' }
		if expected_capsule == ''
			|| require_string_member(value, 'capsule_digest')! != expected_capsule {
			issues << semantic_issue('$/last_native_validation/capsule_digest',
				'last native validation capsule digest differs from its durable projection')
		}
	}
	if require_string_member(value, 'validation_digest')! != native_validation_record_digest(value)! {
		issues << semantic_issue('$/last_native_validation/validation_digest',
			'last native validation digest differs from its complete fourteen fields')
	}
	operation_id := require_string_member(value, 'operation_id')!
	resulting_generation := require_integer_member(value, 'resulting_generation')!
	mut operation_matches := 0
	for operation in require_array_member(root, 'applied_operations')! {
		if require_string_member(operation, 'operation_id')! == operation_id
			&& require_string_member(operation, 'transition')! == transition
			&& require_integer_member(operation, 'resulting_generation')! == resulting_generation {
			operation_matches++
		}
	}
	if operation_matches != 1 || resulting_generation > require_integer_member(root, 'generation')! {
		issues << semantic_issue('$/last_native_validation/operation_id',
			'last native validation is not joined to one retained CAS operation')
	}
	if require_integer_member(subject, 'subject_generation')! > resulting_generation {
		issues << semantic_issue('$/last_native_validation/resulting_generation',
			'last native validation predates its complete subject generation')
	}
	native_gate := require_object_member(value, 'native_gate')!
	smoke_gate := require_object_member(value, 'v_smoke_gate')!
	native_gate_model := persisted_gate_run_from_json(native_gate)!
	smoke_gate_model := persisted_gate_run_from_json(smoke_gate)!
	subject_model := native_subject_from_recovery(parse_receiver_subject(subject)!)
	mut gates_outcome := NativeLaneOutcome.green
	validate_native_validation_gate_pair(native_gate_model, smoke_gate_model, subject_model,
		subject_hash, v_source_sha) or {
		issues << semantic_issue('$/last_native_validation/native_gate',
			'last native validation gates do not replay two exact authenticated sources')
		gates_outcome = .infrastructure
	}
	selected := require_object_member(matrix, 'selected_run')!
	if require_string_member(native_gate, 'check_name')! != 'tccbin-candidate-gate'
		|| require_string_member(smoke_gate, 'check_name')! != 'v-candidate-smoke'
		|| require_string_member(native_gate, 'subject_hash')! != subject_hash
		|| require_string_member(smoke_gate, 'subject_hash')! != subject_hash
		|| require_integer_member(native_gate, 'run_id')! != require_integer_member(selected, 'run_id')!
		|| require_integer_member(native_gate, 'run_attempt')! != require_integer_member(selected, 'run_attempt')!
		|| require_integer_member(native_gate, 'check_suite_id')! != require_integer_member(selected, 'check_suite_id')!
		|| require_string_member(native_gate, 'output_digest')! != matrix_digest {
		issues << semantic_issue('$/last_native_validation/native_gate',
			'last native validation gates differ from its subject, selected run, or matrix output')
	}
	mut matrix_outcome := NativeLaneOutcome.green
	for result in require_array_member(matrix, 'results')! {
		if require_string_member(result, 'status')! == 'failed'
			|| require_bool_member(result, 'fallback_used')! {
			matrix_outcome = .functional
		} else if require_string_member(result, 'status')! == 'blocked' && matrix_outcome == .green {
			matrix_outcome = .infrastructure
		}
	}
	outcome := combine_native_outcomes(matrix_outcome, gates_outcome)
	expected_verdict := match outcome {
		.green { 'green' }
		.functional { 'functional' }
		.infrastructure { 'infrastructure' }
	}
	if verdict != expected_verdict {
		issues << semantic_issue('$/last_native_validation/verdict',
			'last native validation verdict differs from its matrix and gates')
	}
	if intent.kind == .object {
		stage := require_string_member(intent, 'stage')!
		active_subject := require_member(root, 'native_gate_subject')!
		active_kind := if active_subject.kind == .object {
			require_string_member(active_subject, 'consumer_kind')!
		} else {
			''
		}
		publication_state := require_string_member(root, 'publication_state')!
		intent_type := require_string_member(intent, 'intent_type')!
		publisher_lane_is_exact := (publication_state == 'promotion_blocked'
			&& intent_type == 'publish' && active_kind == 'publish_candidate')
			|| (publication_state == 'rollback_blocked' && intent_type == 'rollback'
			&& active_kind in ['rollback_candidate', 'rollback_post'])
		publisher_preserved := stage == 'blocked' && transition == 'candidate_checks_green'
			&& verdict == 'green' && publisher_lane_is_exact
		blocked_red := stage == 'blocked'
			&& transition in ['candidate_failed', 'post_check_infra_exhausted', 'rollback_failed']
		if stage == 'checks_green' || publisher_preserved {
			active_is_candidate := active_kind !in ['publish_post', 'rollback_post']
			if !json_equal(require_member(intent, 'gate_runs')!, JsonValue{
				kind:        .array
				array_value: [native_gate, smoke_gate]
			})
				|| !native_validation_subject_matches_intent_json(subject, intent)
				|| (active_is_candidate && !json_equal(active_subject, subject)) {
				issues << semantic_issue('$/last_native_validation',
					'checked target differs from its durable subject and two gate runs')
			}
		}
		if blocked_red {
			branch_is_exact := blocked_red_native_validation_owner_is_exact(root, subject,
				transition, verdict)
			if !branch_is_exact
				|| !json_equal(require_member(intent, 'gate_runs')!, JsonValue{
				kind:        .array
				array_value: [native_gate, smoke_gate]
			})
				|| !json_equal(active_subject, subject)
				|| require_nullable_string_member(root, 'active_subject_hash')! != subject_hash {
				issues << semantic_issue('$/last_native_validation',
					'blocked red validation differs from its active subject and two gate runs')
			}
		}
		if stage == 'blocked' && !publisher_preserved && !blocked_red {
			issues << semantic_issue('$/last_native_validation',
				'blocked target native validation is outside the closed publisher-preserved or red transition classes')
		}
	}
	if require_string_member(root, 'target_state')! == 'eligible' {
		last_known_good := require_object_member(root, 'last_known_good')!
		if expected_verdict != 'green'
			|| transition !in ['bootstrap_green', 'remediation_green', 'post_check_green']
			|| !native_validation_subject_matches_artifact_json(subject, last_known_good) {
			issues << semantic_issue('$/last_native_validation',
				'eligible target is not backed by a green validation of its last-known-good tuple')
		}
	}
	return issues
}

fn native_validation_subject_matches_intent_json(subject JsonValue, intent JsonValue) bool {
	intent_type := require_string_member(intent, 'intent_type') or { return false }
	intent_id := require_string_member(intent, 'intent_id') or { return false }
	consumer_kind := match intent_type {
		'publish' { 'publish_candidate' }
		'rollback' { 'rollback_candidate' }
		'adopt-current' { 'adopt_current' }
		'initial_adopt_current' { 'initial_adopt_current' }
		else { return false }
	}
	subject_consumer_id := require_string_member(subject, 'consumer_id') or { return false }
	subject_operation_id := require_string_member(subject, 'intent_or_operation_id') or {
		return false
	}
	subject_consumer_kind := require_string_member(subject, 'consumer_kind') or { return false }
	if subject_consumer_id != intent_id || subject_operation_id != intent_id
		|| subject_consumer_kind != consumer_kind {
		return false
	}
	expected := if intent_type in ['adopt-current', 'initial_adopt_current'] {
		require_member(intent, 'validation_subject') or { return false }
	} else {
		require_member(intent, 'candidate_binding') or { return false }
	}
	for key in ['sha', 'tree', 'artifact_fingerprint', 'manifest_hash', 'digests'] {
		if !json_equal(require_member(subject, key) or { return false }, require_member(expected,
			key) or { return false }) {
			return false
		}
	}
	subject_input := require_string_member(subject, 'input_fingerprint') or { return false }
	intent_input := require_string_member(intent, 'input_fingerprint') or { return false }
	subject_ref := require_string_member(subject, 'original_ref') or { return false }
	intent_ref := require_string_member(intent, 'candidate_ref') or { return false }
	return subject_input == intent_input && subject_ref == intent_ref
}

fn native_validation_subject_matches_exact_owner_json(subject JsonValue, expected JsonValue,
	input_fingerprint string, expected_ref string, consumer_id string, consumer_kind string) bool {
	if subject.kind != .object || expected.kind != .object || consumer_id == '' {
		return false
	}
	if require_string_member(subject, 'consumer_id') or { return false } != consumer_id
		|| require_string_member(subject, 'intent_or_operation_id') or {
		return false
	} != consumer_id
		|| require_string_member(subject, 'consumer_kind') or { return false } != consumer_kind
		|| require_string_member(subject, 'original_ref') or {
		return false
	} != expected_ref
		|| require_string_member(subject, 'input_fingerprint') or { return false } != input_fingerprint {
		return false
	}
	for key in ['sha', 'tree', 'artifact_fingerprint', 'manifest_hash', 'digests'] {
		if !json_equal(require_member(subject, key) or { return false }, require_member(expected,
			key) or { return false }) {
			return false
		}
	}
	return true
}

fn blocked_red_native_validation_owner_is_exact(root JsonValue, subject JsonValue,
	transition string, verdict string) bool {
	intent := require_member(root, 'active_intent') or { return false }
	if intent.kind != .object
		|| require_string_member(intent, 'stage') or { return false } != 'blocked' {
		return false
	}
	publication_state := require_string_member(root, 'publication_state') or { return false }
	intent_type := require_string_member(intent, 'intent_type') or { return false }
	if transition == 'candidate_failed' {
		return publication_state == 'rollback_blocked' && intent_type == 'rollback'
			&& native_validation_subject_matches_intent_json(subject, intent)
	}
	if transition == 'post_check_infra_exhausted' {
		return publication_state == 'post_publish_blocked' && intent_type == 'publish' && native_validation_subject_matches_exact_owner_json(subject, require_member(root, 'provisional_published') or {
			return false
		}, require_nullable_string_member(root, 'input_fingerprint') or { return false }, 'thirdparty-${require_string_member(root, 'target_id') or {
			return false
		}}', require_nullable_string_member(root, 'post_validation_operation_id') or {
			return false
		}, 'publish_post')
	}
	if transition != 'rollback_failed' || verdict !in ['functional', 'infrastructure']
		|| publication_state != 'rollback_blocked' || intent_type != 'rollback' {
		return false
	}
	rollback_provisional := require_member(intent, 'rollback_provisional') or { return false }
	if rollback_provisional.kind == .object {
		return native_validation_subject_matches_exact_owner_json(subject, rollback_provisional, require_nullable_string_member(root,
			'input_fingerprint') or { return false }, 'thirdparty-${require_string_member(root,
			'target_id') or { return false }}', require_nullable_string_member(root,
			'post_validation_operation_id') or { return false }, 'rollback_post')
	}
	return native_validation_subject_matches_intent_json(subject, intent)
}

fn native_validation_subject_matches_artifact_json(subject JsonValue, artifact JsonValue) bool {
	for key in ['sha', 'tree', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash',
		'digests'] {
		left := require_member(subject, key) or { return false }
		right := require_member(artifact, key) or { return false }
		if !json_equal(left, right) {
			return false
		}
	}
	return true
}

fn blocked_post_native_validation_is_current(root JsonValue, subject JsonValue,
	stage string) bool {
	if stage != 'blocked' || subject.kind != .object {
		return false
	}
	consumer_kind := require_string_member(subject, 'consumer_kind') or { return false }
	if consumer_kind !in ['publish_post', 'rollback_post'] {
		return false
	}
	record := require_member(root, 'last_native_validation') or { return false }
	if record.kind != .object {
		return false
	}
	matrix := require_object_member(record, 'native_lane_matrix') or { return false }
	record_subject := require_object_member(matrix, 'subject') or { return false }
	if !json_equal(record_subject, subject) {
		return false
	}
	transition := require_string_member(record, 'transition') or { return false }
	verdict := require_string_member(record, 'verdict') or { return false }
	return (consumer_kind == 'publish_post' && transition == 'post_check_infra_exhausted'
		&& verdict == 'infrastructure') || (consumer_kind == 'rollback_post'
		&& transition == 'rollback_failed' && verdict in ['functional', 'infrastructure'])
}

fn validate_applied_operation_ledger_semantics(root JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	operations := require_array_member(root, 'applied_operations')!
	generation := require_integer_member(root, 'generation')!
	last_operation_id := require_nullable_string_member(root, 'last_operation_id')!
	last_transition := require_nullable_string_member(root, 'last_transition')!
	if operations.len == 0 {
		if generation != 0 || last_operation_id != '' || last_transition != '' {
			issues << semantic_issue('$/applied_operations',
				'a nonzero target generation must retain its bounded final CAS operation')
		}
		return issues
	}
	mut operation_ids := []string{}
	mut previous_generation := i64(-1)
	for index, operation in operations {
		path := '$/applied_operations/${index}'
		operation_id := require_string_member(operation, 'operation_id')!
		operation_generation := require_integer_member(operation, 'resulting_generation')!
		if operation_id in operation_ids {
			issues << semantic_issue('${path}/operation_id',
				'applied operation IDs must be globally unique within the bounded CAS ledger')
		}
		operation_ids << operation_id
		if operation_generation <= 0 || operation_generation > generation
			|| (previous_generation >= 0 && operation_generation != previous_generation + 1) {
			issues << semantic_issue('${path}/resulting_generation',
				'applied operation generations must be positive, contiguous and never future')
		}
		previous_generation = operation_generation
	}
	last := operations[operations.len - 1]
	if require_integer_member(last, 'resulting_generation')! != generation
		|| require_string_member(last, 'operation_id')! != last_operation_id
		|| require_string_member(last, 'transition')! != last_transition {
		issues << semantic_issue('$/last_operation_id',
			'last operation and transition must project the exact final CAS ledger record')
	}
	return issues
}

fn validate_recovery_handoff_history_semantics(root JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	handoffs := require_array_member(root, 'recovery_handoffs')!
	active_id := require_nullable_string_member(root, 'active_recovery_handoff_id')!
	generation := require_integer_member(root, 'generation')!
	target_id := require_string_member(root, 'target_id')!
	root_subject := require_member(root, 'native_gate_subject')!
	root_subject_hash := require_nullable_string_member(root, 'active_subject_hash')!
	mut ids := []string{}
	mut active_matches := 0
	mut unfinished_count := 0
	for index, handoff in handoffs {
		path := '$/recovery_handoffs/${index}'
		handoff_id := require_string_member(handoff, 'handoff_id')!
		if handoff_id in ids {
			issues << semantic_issue('${path}/handoff_id',
				'recovery handoff IDs must be unique even when the surrounding objects differ')
		}
		ids << handoff_id
		handoff_subject := require_object_member(handoff, 'subject')!
		parsed_subject := parse_receiver_subject(handoff_subject) or {
			issues << semantic_issue('${path}/subject',
				'recovery handoff subject is not canonical: ${err}')
			continue
		}
		handoff_subject_hash := native_gate_subject_hash(native_subject_from_recovery(parsed_subject)) or {
			issues << semantic_issue('${path}/subject_hash',
				'recovery handoff subject hash cannot be derived: ${err}')
			continue
		}
		ordinal := require_integer_member(handoff, 'handoff_ordinal')!
		recovery_operation_id := require_string_member(handoff, 'recovery_operation_id')!
		consumer_id := require_string_member(handoff, 'intent_or_operation_id')!
		if require_string_member(handoff, 'subject_hash')! != handoff_subject_hash
			|| require_string_member(handoff_subject, 'consumer_id')! != consumer_id
			|| require_string_member(handoff_subject, 'intent_or_operation_id')! != consumer_id
			|| require_string_member(handoff_subject, 'target_id')! != require_string_member(root, 'target_id')!
			|| require_integer_member(handoff, 'subject_generation')! != require_integer_member(handoff_subject, 'subject_generation')!
			|| require_integer_member(handoff, 'expected_ledger_generation')! < require_integer_member(handoff, 'subject_generation')!
			|| require_integer_member(handoff, 'expected_ledger_generation')! > generation {
			issues << semantic_issue(path,
				'recovery handoff must retain its exact consumer, target, subject hash and bounded generations')
		}
		expected_id := deterministic_handoff_id(require_string_member(handoff, 'audience')!,
			recovery_operation_id, consumer_id, handoff_subject_hash, int(ordinal))
		if handoff_id != expected_id
			|| require_string_member(handoff, 'receiver_run_name')! != 'tccbin-recovery-${handoff_id}' {
			issues << semantic_issue('${path}/handoff_id',
				'recovery handoff ID and run name must be deterministic from the immutable chain identity')
		}
		if require_string_member(handoff, 'subject_ref_head')! != require_string_member(handoff_subject,
			'sha')! {
			issues << semantic_issue('${path}/subject_ref_head',
				'recovery handoff ref HEAD must equal its immutable native subject SHA')
		}
		target_canonical_ref := 'thirdparty-${target_id}'
		if require_string_member(handoff_subject, 'original_ref')! == target_canonical_ref
			&& require_string_member(handoff, 'expected_canonical_head')! != require_string_member(handoff_subject, 'sha')! {
			issues << semantic_issue('${path}/expected_canonical_head',
				'canonical recovery handoff must retain the subject SHA as expected HEAD')
		}
		issues << validate_recovery_handoff_cas_semantics(root, handoff, path, handoff_id, ordinal)!
		state := require_string_member(handoff, 'state')!
		if state != 'complete' {
			unfinished_count++
		}
		if handoff_id == active_id {
			active_matches++
			configured_sources := current_authority_check_sources(root)!
			if state !in ['pending', 'dispatched', 'blocked']
				|| require_integer_member(handoff, 'expected_ledger_generation')! != generation
				|| root_subject.kind != .object || !json_equal(handoff_subject, root_subject)
				|| handoff_subject_hash != root_subject_hash || configured_sources.kind != .array
				|| !json_equal(require_member(handoff, 'expected_check_sources')!, configured_sources) {
				issues << semantic_issue('$/active_recovery_handoff_id',
					'active recovery handoff must retain the one exact current consumer, subject and pre-existing check authority')
			}
		}
	}
	if (active_id == '' && unfinished_count != 0)
		|| (active_id != '' && (active_matches != 1 || unfinished_count != 1)) {
		issues << semantic_issue('$/active_recovery_handoff_id',
			'recovery history must expose exactly its sole unfinished handoff, or no active pointer')
	}
	for index, handoff in handoffs {
		path := '$/recovery_handoffs/${index}'
		handoff_id := require_string_member(handoff, 'handoff_id')!
		ordinal := require_integer_member(handoff, 'handoff_ordinal')!
		predecessor_id := require_nullable_string_member(handoff, 'predecessor_handoff_id')!
		successor_id := require_nullable_string_member(handoff, 'successor_handoff_id')!
		if (ordinal == 0 && predecessor_id != '') || (ordinal > 0 && predecessor_id == '') {
			issues << semantic_issue('${path}/predecessor_handoff_id',
				'recovery handoff ordinal and predecessor pointer disagree')
		}
		if predecessor_id != '' {
			mut matches := 0
			for predecessor in handoffs {
				if require_string_member(predecessor, 'handoff_id')! == predecessor_id {
					matches++
					if require_nullable_string_member(predecessor, 'successor_handoff_id')! != handoff_id
						|| require_integer_member(predecessor, 'handoff_ordinal')! + 1 != ordinal
						|| require_string_member(predecessor, 'audience')! != require_string_member(handoff, 'audience')!
						|| require_string_member(predecessor, 'recovery_operation_id')! != require_string_member(handoff, 'recovery_operation_id')!
						|| require_string_member(predecessor, 'intent_or_operation_id')! != require_string_member(handoff, 'intent_or_operation_id')!
						|| require_string_member(predecessor, 'subject_hash')! != require_string_member(handoff, 'subject_hash')! {
						issues << semantic_issue('${path}/predecessor_handoff_id',
							'recovery predecessor must be the exact prior ordinal in the same immutable chain')
					}
				}
			}
			if matches != 1 {
				issues << semantic_issue('${path}/predecessor_handoff_id',
					'recovery predecessor pointer must resolve exactly once')
			}
		}
		if successor_id != '' {
			mut matches := 0
			for successor_index, successor in handoffs {
				if require_string_member(successor, 'handoff_id')! == successor_id {
					matches++
					if require_nullable_string_member(successor, 'predecessor_handoff_id')! != handoff_id
						|| require_integer_member(successor, 'handoff_ordinal')! != ordinal + 1
						|| require_string_member(successor, 'audience')! != require_string_member(handoff, 'audience')!
						|| require_string_member(successor, 'recovery_operation_id')! != require_string_member(handoff, 'recovery_operation_id')!
						|| require_string_member(successor, 'intent_or_operation_id')! != require_string_member(handoff, 'intent_or_operation_id')!
						|| require_string_member(successor, 'subject_hash')! != require_string_member(handoff, 'subject_hash')! {
						issues << semantic_issue('${path}/successor_handoff_id',
							'recovery successor must point back to the exact next chain ordinal')
					}
					successor_path := '$/recovery_handoffs/${successor_index}'
					issues << validate_native_recovery_successor_semantics(root, handoff,
						successor, active_id, generation, path, successor_path)!
				}
			}
			if matches != 1 {
				issues << semantic_issue('${path}/successor_handoff_id',
					'recovery successor pointer must resolve exactly once')
			}
		}
	}
	return issues
}

fn current_authority_check_sources(root JsonValue) !JsonValue {
	intent := require_member(root, 'active_intent')!
	if intent.kind == .object {
		return require_member(intent, 'expected_check_sources')!
	}
	remediation := require_member(root, 'active_remediation_binding')!
	if remediation.kind == .object {
		return require_member(remediation, 'expected_check_sources')!
	}
	return JsonValue{
		kind: .null_value
	}
}

fn validate_recovery_handoff_cas_semantics(root JsonValue, handoff JsonValue, path string,
	handoff_id string, ordinal i64) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	dispatch_ids := require_array_member(handoff, 'dispatch_operation_ids')!
	if require_integer_member(handoff, 'dispatch_generation')! != i64(dispatch_ids.len) {
		issues << semantic_issue('${path}/dispatch_generation',
			'recovery dispatch generation must equal its durable operation count')
	}
	mut observed_dispatch_ids := []string{}
	mut latest_dispatch_generation := i64(-1)
	for dispatch_index, dispatch_value in dispatch_ids {
		dispatch_id := require_string(dispatch_value)!
		if dispatch_id in observed_dispatch_ids {
			issues << semantic_issue('${path}/dispatch_operation_ids/${dispatch_index}',
				'recovery dispatch operation IDs must be unique')
		}
		observed_dispatch_ids << dispatch_id
		dispatch_count, dispatch_generation, dispatch_transition := operation_occurrences(root,
			dispatch_id)!
		if dispatch_count != 1
			|| dispatch_generation <= require_integer_member(handoff, 'subject_generation')!
			|| dispatch_generation > require_integer_member(handoff, 'expected_ledger_generation')!
			|| dispatch_transition != 'handoff_dispatch_${handoff_id}' {
			issues << semantic_issue('${path}/dispatch_operation_ids/${dispatch_index}',
				'recovery dispatch must be one exact target CAS owned by this handoff')
		}
		if dispatch_generation <= latest_dispatch_generation {
			issues << semantic_issue('${path}/dispatch_operation_ids/${dispatch_index}',
				'recovery dispatch CAS operations must retain their strict generation order')
		}
		latest_dispatch_generation = dispatch_generation
	}
	if ordinal == 0 {
		creation_transition := 'handoff_create_${recovery_handoff_creation_commitment(handoff)!}'
		create_count, create_generation, create_operation_id := transition_occurrences(root,
			creation_transition)!
		if create_count != 1 || create_operation_id == ''
			|| create_generation <= require_integer_member(handoff, 'subject_generation')!
			|| create_generation > require_integer_member(handoff, 'expected_ledger_generation')!
			|| (latest_dispatch_generation >= 0 && create_generation >= latest_dispatch_generation) {
			issues << semantic_issue(path,
				'first recovery handoff must retain its unique pre-dispatch creation CAS')
		}
	}
	ack_id := require_nullable_string_member(handoff, 'ack_operation_id')!
	mut ack_generation := i64(-1)
	if ack_id != '' {
		ack_count, observed_ack_generation, ack_transition := operation_occurrences(root, ack_id)!
		ack_generation = observed_ack_generation
		if latest_dispatch_generation < 0 || ack_count != 1
			|| ack_generation <= latest_dispatch_generation
			|| ack_generation > require_integer_member(handoff, 'expected_ledger_generation')!
			|| ack_transition != 'handoff_ack_${handoff_id}' {
			issues << semantic_issue('${path}/ack_operation_id',
				'recovery ACK must be one exact later target CAS owned by this handoff')
		}
	}
	if require_string_member(handoff, 'state')! == 'complete' {
		completion_id := require_nullable_string_member(handoff, 'completion_operation_id')!
		completion_count, completion_generation, completion_transition := operation_occurrences(root,
			completion_id)!
		terminal_outcome := require_nullable_string_member(handoff, 'terminal_outcome')!
		expected_transition := if terminal_outcome == 'native_gate_green_successor' {
			'native_recovery_successor_${recovery_native_successor_commitment(handoff)!}'
		} else if require_member(handoff, 'terminal_revalidation')!.kind == .object {
			proof := require_object_member(handoff, 'terminal_revalidation')!
			'handoff_complete_${require_string_member(proof, 'facts_digest')!}'
		} else {
			'handoff_complete_${handoff_id}'
		}
		if completion_id == '' || ack_generation < 0 || completion_count != 1
			|| completion_generation <= ack_generation
			|| completion_generation != require_integer_member(handoff, 'expected_ledger_generation')!
			|| completion_transition != expected_transition {
			issues << semantic_issue('${path}/completion_operation_id',
				'complete recovery handoff must retain its one exact post-ACK completion CAS')
		}
	}
	return issues
}

fn validate_native_recovery_successor_semantics(root JsonValue, predecessor JsonValue,
	successor JsonValue, active_id string, generation i64, predecessor_path string,
	successor_path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	predecessor_consumer_type := require_string_member(predecessor, 'consumer_type')!
	predecessor_generation := require_integer_member(predecessor, 'expected_ledger_generation')!
	predecessor_subject_generation := require_integer_member(predecessor, 'subject_generation')!
	successor_id := require_string_member(successor, 'handoff_id')!
	successor_capability := require_string_member(successor, 'resume_capability')!
	successor_generation := require_integer_member(successor, 'expected_ledger_generation')!
	chain_transition := 'native_recovery_successor_${recovery_native_successor_commitment(predecessor)!}'
	chain_count, chain_generation, chain_operation_id := transition_occurrences(root,
		chain_transition)!
	valid_consumer := predecessor_consumer_type in ['post-validation', 'remediation']
	valid_successor_capability := successor_capability in ['v_smoke', 'evidence_only']
	valid_identity :=
		require_string_member(successor, 'consumer_type')! == predecessor_consumer_type
		&& json_equal(require_object_member(successor, 'subject')!, require_object_member(predecessor, 'subject')!)
		&& json_equal(require_member(successor, 'expected_check_sources')!, require_member(predecessor, 'expected_check_sources')!)
		&& require_nullable_string_member(successor, 'native_gate_check_digest')! == require_nullable_string_member(predecessor, 'native_gate_check_digest')!
		&& require_integer_member(successor, 'subject_generation')! == predecessor_subject_generation
		&& require_string_member(successor, 'expected_canonical_head')! == require_string_member(predecessor, 'expected_canonical_head')!
		&& require_string_member(successor, 'subject_ref_head')! == require_string_member(predecessor, 'subject_ref_head')!
	successor_state := require_string_member(successor, 'state')!
	successor_dispatch_generation := require_integer_member(successor, 'dispatch_generation')!
	mut dispatches_follow_creation := true
	for dispatch_value in require_array_member(successor, 'dispatch_operation_ids')! {
		dispatch_id := require_string(dispatch_value)!
		dispatch_count, dispatch_generation, _ := operation_occurrences(root, dispatch_id)!
		if dispatch_count != 1 || dispatch_generation <= chain_generation {
			dispatches_follow_creation = false
		}
	}
	at_creation := successor_generation == chain_generation
	valid_successor_lifecycle := if at_creation {
		successor_state == 'pending' && successor_dispatch_generation == 0
			&& active_id == successor_id && generation == chain_generation
	} else {
		successor_generation > chain_generation && dispatches_follow_creation
			&& ((active_id == successor_id
			&& successor_state in ['pending', 'dispatched', 'blocked'])
			|| (active_id != successor_id && successor_state == 'complete'))
	}
	terminal_proof := require_member(successor, 'terminal_revalidation')!
	mut evidence_smoke := require_member(root, 'v_smoke_execution')!
	mut evidence_native := require_member(root, 'native_gate_execution')!
	if terminal_proof.kind == .object {
		evidence_smoke = require_object_member(terminal_proof, 'v_smoke_execution')!
		evidence_native = require_object_member(terminal_proof, 'native_gate_execution')!
	}
	valid_successor_workflow_id := evidence_smoke.kind == .object
		&& require_integer_member(successor, 'workflow_id')! == require_integer_member(evidence_smoke, 'workflow_id')!
	mut predecessor_evidence_closed := false
	mut successor_evidence_closed := false
	mut native_evidence_closed := false
	native_green := evidence_native.kind == .object
		&& native_recovery_gate_is_green_and_cas_closed(root, predecessor, chain_generation, evidence_native)!
	if evidence_native.kind == .object {
		evidence_digest := native_gate_evidence_digest(evidence_native)!
		predecessor_evidence_closed = require_nullable_string_member(predecessor,
			'native_gate_evidence_digest')! == evidence_digest
		successor_evidence_closed = require_nullable_string_member(successor,
			'native_gate_evidence_digest')! == evidence_digest
		native_evidence_closed = predecessor_evidence_closed && successor_evidence_closed
		if !predecessor_evidence_closed {
			issues << semantic_issue('${predecessor_path}/native_gate_evidence_digest',
				'native recovery evidence must equal the immutable predecessor H1 evidence digest')
		}
		if !successor_evidence_closed {
			issues << semantic_issue('${successor_path}/native_gate_evidence_digest',
				'native recovery evidence must equal the immutable predecessor H1 evidence digest')
		}
		issues << validate_historical_native_gate_semantics(predecessor, evidence_native,
			successor_path)!
	}
	if evidence_smoke.kind == .object {
		issues << validate_recovery_successor_smoke_binding(successor, evidence_smoke,
			predecessor_path, successor_path)!
	}
	if successor_state == 'complete' {
		issues << validate_terminal_revalidation_semantics(root, predecessor, successor,
			chain_generation, successor_path)!
	}
	if require_integer_member(predecessor, 'handoff_ordinal')! != 0 || !valid_consumer
		|| require_string_member(predecessor, 'resume_capability')! != 'native_gate'
		|| require_string_member(predecessor, 'state')! != 'complete'
		|| require_nullable_string_member(predecessor, 'terminal_outcome')! != 'native_gate_green_successor'
		|| require_nullable_string_member(predecessor, 'receiver_conclusion')! != 'success'
		|| !valid_successor_lifecycle || !valid_successor_capability
		|| require_string_member(successor, 'workflow_path')! != '.github/workflows/tccbin_revalidate.yml'
		|| !valid_successor_workflow_id || !valid_identity
		|| predecessor_generation <= predecessor_subject_generation
		|| successor_generation < predecessor_generation || chain_count != 1
		|| chain_operation_id == '' || chain_generation != predecessor_generation || !native_green
		|| !native_evidence_closed {
		issues << semantic_issue('${predecessor_path}/successor_handoff_id',
			'native recovery successor requires one atomic green post/remediation H1 to pending revalidator H2 CAS')
	}
	return issues
}

fn validate_recovery_successor_smoke_binding(successor JsonValue, smoke JsonValue,
	predecessor_path string, successor_path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	subject := require_object_member(successor, 'subject')!
	bindings := [
		['consumer_id', 'intent_or_operation_id'],
		['consumer_kind', 'consumer_kind'],
		['target_id', 'target_id'],
		['subject_hash', 'subject_hash'],
	]
	for binding in bindings {
		expected := if binding[1] == 'subject_hash' {
			require_string_member(successor, binding[1])!
		} else {
			require_string_member(subject, binding[1])!
		}
		if require_string_member(smoke, binding[0])! != expected {
			issues << semantic_issue('${predecessor_path}/successor_handoff_id',
				'recovery H2 and V-smoke identities must remain one exact consumer and subject')
		}
	}
	if require_string_member(smoke, 'subject_ref')! != require_string_member(subject, 'original_ref')!
		|| require_string_member(smoke, 'subject_sha')! != require_string_member(subject, 'sha')!
		|| require_integer_member(smoke, 'subject_generation')! != require_integer_member(successor, 'subject_generation')!
		|| require_integer_member(smoke, 'workflow_id')! != require_integer_member(successor, 'workflow_id')!
		|| require_string_member(smoke, 'repository')! != require_string_member(successor, 'receiver_repository')!
		|| require_string_member(smoke, 'workflow_path')! != require_string_member(successor, 'workflow_path')!
		|| require_string_member(smoke, 'workflow_ref')! != require_string_member(successor, 'workflow_ref')!
		|| require_string_member(smoke, 'event')! != require_string_member(successor, 'event')! {
		issues << semantic_issue('${predecessor_path}/successor_handoff_id',
			'recovery H2 workflow and immutable V-smoke subject must match exactly')
	}
	dispatch_ids := require_array_member(successor, 'dispatch_operation_ids')!
	smoke_dispatches := require_array_member(smoke, 'dispatches')!
	if dispatch_ids.len != smoke_dispatches.len
		|| require_integer_member(successor, 'dispatch_generation')! != i64(smoke_dispatches.len) {
		issues << semantic_issue('${successor_path}/dispatch_operation_ids',
			'recovery H2 dispatch history must be the exact V-smoke pre-side-effect CAS history')
	} else {
		for index, dispatch_id in dispatch_ids {
			if require_string(dispatch_id)! != require_string_member(smoke_dispatches[index],
				'dispatch_operation_id')! {
				issues << semantic_issue('${successor_path}/dispatch_operation_ids/${index}',
					'recovery H2 dispatch history must be the exact V-smoke pre-side-effect CAS history')
			}
		}
	}
	successor_state := require_string_member(successor, 'state')!
	smoke_state := require_string_member(smoke, 'state')!
	if (successor_state == 'pending' && smoke_state != 'pending')
		|| (successor_state == 'dispatched'
		&& smoke_state !in ['dispatched', 'completed', 'blocked'])
		|| (successor_state == 'blocked' && smoke_state != 'blocked')
		|| (successor_state == 'complete' && smoke_state !in ['completed', 'blocked']) {
		issues << semantic_issue('${successor_path}/state',
			'recovery H2 and V-smoke lifecycle projections contradict each other')
	}
	if successor_state != 'complete'
		&& require_integer_member(smoke, 'expected_ledger_generation')! != require_integer_member(successor, 'expected_ledger_generation')! {
		issues << semantic_issue('${successor_path}/expected_ledger_generation',
			'active recovery H2 and V-smoke must share the current target CAS generation')
	}
	ack_id := require_nullable_string_member(successor, 'ack_operation_id')!
	if ack_id != '' {
		mut selected_matches := 0
		for attempt in require_array_member(smoke, 'attempts')! {
			if require_integer_member(attempt, 'run_id')! == require_nullable_integer_member(successor, 'selected_run_id')!
				&& require_integer_member(attempt, 'run_attempt')! == require_nullable_integer_member(successor, 'selected_run_attempt')!
				&& require_string_member(attempt, 'ack_operation_id')! == ack_id {
				selected_matches++
				if require_nullable_string_member(successor, 'receiver_master_sha')! != require_string_member(smoke, 'v_master_sha')!
					|| require_string_member(attempt, 'head_sha')! != require_string_member(smoke, 'v_master_sha')!
					|| require_nullable_string_member(successor, 'deadline')! != require_string_member(attempt, 'deadline')! {
					issues << semantic_issue('${successor_path}/receiver_master_sha',
						'recovery H2 receiver SHA and deadline must equal its selected V-smoke reservation and observed run')
				}
			}
		}
		if selected_matches != 1 {
			issues << semantic_issue('${successor_path}/ack_operation_id',
				'recovery H2 ACK must select the exact one durable V-smoke attempt')
		}
	}
	return issues
}

fn validate_terminal_revalidation_semantics(root JsonValue, predecessor JsonValue,
	successor JsonValue, chain_generation i64, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	proof := require_member(successor, 'terminal_revalidation')!
	if proof.kind != .object {
		issues << semantic_issue('${path}/terminal_revalidation',
			'terminal recovery H2 must retain its complete native and revalidator proof')
		return issues
	}
	if require_string_member(proof, 'facts_digest')! != terminal_revalidation_facts_digest(proof)! {
		issues << semantic_issue('${path}/terminal_revalidation/facts_digest',
			'terminal revalidation digest does not cover its complete immutable proof')
	}
	subject := require_object_member(successor, 'subject')!
	subject_hash := require_string_member(successor, 'subject_hash')!
	native_execution := require_object_member(proof, 'native_gate_execution')!
	native_check := require_object_member(proof, 'native_gate_check')!
	smoke := require_object_member(proof, 'v_smoke_execution')!
	if !json_equal(require_object_member(native_execution, 'subject')!, subject)
		|| require_string_member(native_execution, 'subject_hash')! != subject_hash
		|| require_string_member(native_execution, 'subject_sha')! != require_string_member(subject, 'sha')!
		|| require_integer_member(native_execution, 'subject_generation')! != require_integer_member(successor, 'subject_generation')! {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution',
			'terminal H2 native evidence differs from the immutable recovery subject')
	}
	pre_projection := require_object_member(proof, 'pre_business_projection')!
	source_waiting := require_nullable_string_member(successor, 'terminal_outcome')! == 'source_waiting'
	issues << validate_terminal_source_atomic_projection(proof, source_waiting, path)!
	issues << validate_v_smoke_execution_semantics_mode(root, subject, subject_hash, smoke, false, require_member(proof,
		'expected_check_sources')!, require_string_member(pre_projection, 'v_source_sha')!,
		source_waiting)!
	completion_id := require_nullable_string_member(successor, 'completion_operation_id')!
	_, completion_generation, _ := operation_occurrences(root, completion_id)!
	business_id := require_string_member(proof, 'business_operation_id')!
	business_transition := require_string_member(proof, 'business_transition')!
	business_count, business_generation, observed_business_transition := operation_occurrences(root,
		business_id)!
	outcome := require_nullable_string_member(successor, 'terminal_outcome')!
	consumer_kind := require_string_member(subject, 'consumer_kind')!
	expected_business_transition := terminal_revalidation_business_transition(consumer_kind,
		outcome)
	source_refetch := require_member(proof, 'source_refetch')!
	expected_observed_business_transition := if business_transition == 'source_unreachable'
		&& source_refetch.kind == .object {
		'source_unreachable_${require_string_member(source_refetch, 'evidence_digest')!}'
	} else {
		business_transition
	}
	if business_count != 1 || business_generation <= chain_generation
		|| business_generation + 1 != completion_generation
		|| observed_business_transition != expected_observed_business_transition
		|| business_transition != expected_business_transition {
		issues << semantic_issue('${path}/terminal_revalidation/business_operation_id',
			'terminal H2 must follow the exact final revalidator business CAS')
	}
	issues << validate_terminal_revalidation_sources(proof, predecessor, successor,
		native_execution, smoke, path)!
	issues << validate_terminal_native_check(proof, predecessor, successor, native_execution,
		native_check, path)!
	mut selected_attempt := JsonValue{
		kind: .null_value
	}
	mut selected_matches := 0
	for attempt in require_array_member(smoke, 'attempts')! {
		if require_integer_member(attempt, 'run_id')! == require_nullable_integer_member(successor, 'selected_run_id')!
			&& require_integer_member(attempt, 'run_attempt')! == require_nullable_integer_member(successor, 'selected_run_attempt')! {
			selected_attempt = attempt
			selected_matches++
		}
	}
	if selected_matches != 1 || selected_attempt.kind != .object {
		issues << semantic_issue('${path}/selected_run_id',
			'terminal H2 must retain exactly its selected terminal V-smoke run')
		return issues
	}
	selected_completion_id := require_nullable_string_member(selected_attempt,
		'completion_operation_id')!
	_, selected_completion_generation, _ := operation_occurrences(root, selected_completion_id)!
	native_generation := require_integer_member(native_execution, 'expected_ledger_generation')!
	smoke_generation := require_integer_member(smoke, 'expected_ledger_generation')!
	if native_generation != smoke_generation || smoke_generation != selected_completion_generation
		|| selected_completion_generation + 1 != business_generation
		|| business_generation + 1 != completion_generation {
		issues << semantic_issue('${path}/terminal_revalidation',
			'terminal native, V-smoke, selected completion, business and H2 completion generations must be one exact contiguous CAS chain')
	}
	if require_string_member(selected_attempt, 'ack_operation_id')! != require_nullable_string_member(successor, 'ack_operation_id')!
		|| selected_completion_id == '' || selected_completion_generation >= business_generation
		|| require_integer_member(selected_attempt, 'attempt_index')! != i64(require_array_member(smoke, 'dispatches')!.len)
		|| require_nullable_string_member(selected_attempt, 'run_conclusion')! != require_nullable_string_member(successor, 'receiver_conclusion')!
		|| require_nullable_string_member(selected_attempt, 'output_digest')! != require_nullable_string_member(successor, 'receiver_output_digest')! {
		issues << semantic_issue('${path}/terminal_revalidation/v_smoke_execution',
			'terminal H2 verdict must equal its exact ACKed and completed V-smoke attempt')
	}
	logical_outcome := v_smoke_attempt_outcome(selected_attempt)!
	smoke_state := require_string_member(smoke, 'state')!
	logical_projection_is_exact := match outcome {
		'green', 'no_op' { logical_outcome == 'green' && smoke_state == 'completed' }
		'functional_defect_routed' { logical_outcome == 'functional' && smoke_state == 'blocked' }
		'infrastructure_blocked' { logical_outcome == 'infrastructure' && smoke_state == 'blocked' }
		'source_waiting' { logical_outcome == 'infrastructure' && smoke_state == 'blocked' }
		else { false }
	}
	if !logical_projection_is_exact {
		issues << semantic_issue('${path}/terminal_outcome',
			'terminal H2 outcome and business verdict must equal the selected V-smoke run/check logical outcome')
	}
	if outcome == 'infrastructure_blocked' && !terminal_infrastructure_retry_is_exhausted(smoke)! {
		issues << semantic_issue('${path}/terminal_revalidation/v_smoke_execution',
			'terminal infrastructure routing requires both bounded logical attempts to finish as infrastructure with the single retry consumed')
	}
	if outcome == 'source_waiting' && !terminal_source_waiting_preserves_retry(smoke)! {
		issues << semantic_issue('${path}/terminal_revalidation/v_smoke_execution',
			'source waiting must stop after the first infrastructure observation without consuming the CI infrastructure retry')
	}
	final_projection := require_object_member(proof, 'final_projection')!
	last_validation := require_member(final_projection, 'last_validation')!
	expected_validation_conclusion := match outcome {
		'green', 'no_op' { 'success' }
		'functional_defect_routed' { 'failure' }
		else { 'blocked' }
	}
	if last_validation.kind != .object
		|| require_integer_member(last_validation, 'run_id')! != require_integer_member(selected_attempt, 'run_id')!
		|| require_integer_member(last_validation, 'run_attempt')! != require_integer_member(selected_attempt, 'run_attempt')!
		|| require_string_member(last_validation, 'subject_hash')! != subject_hash
		|| require_string_member(last_validation, 'conclusion')! != expected_validation_conclusion
		|| require_string_member(last_validation, 'evidence_digest')! != require_nullable_string_member(selected_attempt, 'evidence_digest')! {
		issues << semantic_issue('$/last_validation',
			'terminal H2 append-only snapshot must retain its exact durable revalidator verdict')
	}
	terminal_completed_at_value := successor.object_value('terminal_completed_at') or {
		JsonValue{
			kind: .null_value
		}
	}
	terminal_completed_at := if terminal_completed_at_value.kind == .string_value {
		terminal_completed_at_value.string_value
	} else {
		''
	}
	issues << validate_terminal_source_refetch(proof, subject, outcome, business_id,
		smoke_generation, selected_attempt, pre_projection, final_projection,
		terminal_completed_at, path)!
	issues << validate_terminal_business_projection(root, subject, subject_hash, successor,
		outcome, business_transition, pre_projection, final_projection, selected_attempt,
		completion_generation, path)!
	issues << validate_current_terminal_projection(root, successor, final_projection, path)!
	return issues
}

fn validate_terminal_source_atomic_projection(proof JsonValue, source_waiting bool,
	path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	atomic_projection := require_member(proof, 'source_atomic_pre_projection')!
	if !source_waiting {
		if atomic_projection.kind != .null_value {
			issues << semantic_issue('${path}/terminal_revalidation/source_atomic_pre_projection',
				'non-source terminal outcomes cannot carry a source atomic parent projection')
		}
		return issues
	}
	if atomic_projection.kind != .object {
		return [
			semantic_issue('${path}/terminal_revalidation/source_atomic_pre_projection',
				'source_waiting must retain the non-authoritative projection of its real Git parent'),
		]
	}
	pre := require_object_member(proof, 'pre_business_projection')!
	atomic_native := require_object_member(atomic_projection, 'native_gate_execution')!
	pre_native := require_object_member(pre, 'native_gate_execution')!
	atomic_smoke := require_object_member(atomic_projection, 'v_smoke_execution')!
	pre_smoke := require_object_member(pre, 'v_smoke_execution')!
	atomic_attempts := require_array_member(atomic_smoke, 'attempts')!
	pre_attempts := require_array_member(pre_smoke, 'attempts')!
	mut exact :=
		require_integer_member(atomic_projection, 'generation')! + 1 == require_integer_member(pre, 'generation')!
		&& require_integer_member(atomic_native, 'expected_ledger_generation')! == require_integer_member(atomic_projection, 'generation')!
		&& require_integer_member(pre_native, 'expected_ledger_generation')! == require_integer_member(pre, 'generation')!
		&& require_integer_member(atomic_smoke, 'expected_ledger_generation')! == require_integer_member(atomic_projection, 'generation')!
		&& require_integer_member(pre_smoke, 'expected_ledger_generation')! == require_integer_member(pre, 'generation')!
		&& require_string_member(atomic_smoke, 'state')! == 'dispatched'
		&& require_string_member(pre_smoke, 'state')! == 'blocked' && atomic_attempts.len == 1
		&& pre_attempts.len == 1
		&& json_objects_equal_except(atomic_projection, pre, ['generation', 'native_gate_execution', 'v_smoke_execution'])!
		&& json_objects_equal_except(atomic_native, pre_native, ['expected_ledger_generation'])!
		&& json_objects_equal_except(atomic_smoke, pre_smoke, ['expected_ledger_generation', 'state', 'attempts', 'active_attempt', 'completion_operation_ids', 'replay_facts_digest'])!
	if atomic_attempts.len == 1 && pre_attempts.len == 1 {
		exact = exact
			&& json_objects_equal_except(atomic_attempts[0], pre_attempts[0], ['run_conclusion', 'completion_operation_id', 'completion_facts_digest', 'check_run_id', 'check_name', 'check_sha', 'details_url', 'external_id', 'validator_integration_id', 'check_conclusion', 'output_digest', 'evidence_digest', 'completed_at', 'completion_kind'])!
			&& require_member(atomic_attempts[0], 'completion_operation_id')!.kind == .null_value
			&& require_member(pre_attempts[0], 'completion_operation_id')!.kind == .string_value
	}
	if !exact {
		issues << semantic_issue('${path}/terminal_revalidation/source_atomic_pre_projection',
			'source atomic parent must be the exact dispatched generation immediately before V-smoke completion')
	}
	return issues
}

fn terminal_revalidation_business_transition(consumer_kind string, outcome string) string {
	if outcome == 'source_waiting' {
		return 'source_unreachable'
	}
	if consumer_kind == 'publish_post' {
		return if outcome in ['green', 'no_op'] {
			'post_check_green'
		} else if outcome == 'infrastructure_blocked' {
			'post_check_infra_exhausted'
		} else {
			'post_check_red'
		}
	}
	if consumer_kind == 'rollback_post' {
		return if outcome in ['green', 'no_op'] {
			'rollback_post_green'
		} else {
			'rollback_failed'
		}
	}
	if consumer_kind == 'remediation' {
		return if outcome in ['green', 'no_op'] { 'remediation_green' } else { 'remediation_red' }
	}
	return ''
}

fn terminal_infrastructure_retry_is_exhausted(smoke JsonValue) !bool {
	return require_string_member(smoke, 'state')! == 'blocked'
		&& require_array_member(smoke, 'dispatches')!.len == 2
		&& require_integer_member(smoke, 'infra_retry_count')! == 1
		&& v_smoke_logical_outcome(require_array_member(smoke, 'attempts')!, require_array_member(smoke, 'run_absent_attempts')!, 1)! == 'infrastructure'
		&& v_smoke_logical_outcome(require_array_member(smoke, 'attempts')!, require_array_member(smoke, 'run_absent_attempts')!, 2)! == 'infrastructure'
}

fn terminal_source_waiting_preserves_retry(smoke JsonValue) !bool {
	attempts := require_array_member(smoke, 'attempts')!
	run_absent_attempts := require_array_member(smoke, 'run_absent_attempts')!
	return require_string_member(smoke, 'state')! == 'blocked'
		&& require_array_member(smoke, 'dispatches')!.len == 1
		&& require_integer_member(smoke, 'infra_retry_count')! == 0
		&& attempts.len + run_absent_attempts.len == 1
		&& v_smoke_logical_outcome(attempts, run_absent_attempts, 1)! == 'infrastructure'
}

fn validate_terminal_native_check(proof JsonValue, predecessor JsonValue, successor JsonValue,
	native_execution JsonValue, check JsonValue, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	mut native_source := JsonValue{
		kind: .null_value
	}
	mut source_matches := 0
	for source in require_array_member(proof, 'expected_check_sources')! {
		if require_string_member(source, 'name')! == 'tccbin-candidate-gate' {
			native_source = source
			source_matches++
		}
	}
	active_epoch := require_integer_member(native_execution, 'active_gate_epoch')!
	mut epoch := JsonValue{
		kind: .null_value
	}
	epochs := require_array_member(native_execution, 'gate_epochs')!
	if active_epoch >= 0 && active_epoch < i64(epochs.len) {
		epoch = epochs[int(active_epoch)]
	}
	selected_id := require_nullable_integer_member(native_execution, 'selected_run_id')!
	selected_attempt := require_nullable_integer_member(native_execution, 'selected_run_attempt')!
	selected_suite := require_nullable_integer_member(native_execution, 'selected_check_suite_id')!
	mut observed := JsonValue{
		kind: .null_value
	}
	mut observed_matches := 0
	for run in require_array_member(native_execution, 'gate_runs')! {
		if require_integer_member(run, 'gate_epoch')! == active_epoch
			&& require_integer_member(run, 'run_id')! == selected_id
			&& require_integer_member(run, 'run_attempt')! == selected_attempt
			&& require_integer_member(run, 'check_suite_id')! == selected_suite {
			observed = run
			observed_matches++
		}
	}
	check_digest := native_gate_check_digest(check)!
	if check_digest != require_nullable_string_member(predecessor, 'native_gate_check_digest')!
		|| check_digest != require_nullable_string_member(successor, 'native_gate_check_digest')! {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_check',
			'terminal native check differs from the H1/H2 append-only check commitment')
	}
	if source_matches != 1 || native_source.kind != .object || observed_matches != 1
		|| observed.kind != .object || epoch.kind != .object {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_check',
			'terminal native check must select exactly one allowlisted observed H1 run')
		return issues
	}
	subject := require_object_member(successor, 'subject')!
	consumer_id := require_string_member(subject, 'consumer_id')!
	subject_hash := require_string_member(successor, 'subject_hash')!
	run_id := require_integer_member(check, 'run_id')!
	job_id := require_integer_member(check, 'job_id')!
	repository := require_string_member(check, 'repository')!
	run_url := 'https://github.com/${repository}/actions/runs/${run_id}'
	job_url := '${run_url}/job/${job_id}'
	strings_are_exact := require_string_member(check, 'check_name')! == 'tccbin-candidate-gate'
		&& require_string_member(check, 'repository')! == require_string_member(native_source, 'repository')!
		&& require_string_member(check, 'workflow_path')! == require_string_member(native_source, 'workflow_path')!
		&& require_string_member(check, 'event')! == require_string_member(native_source, 'event')!
		&& require_string_member(check, 'subject_hash')! == subject_hash
		&& require_string_member(check, 'run_name')! == 'tccbin-native-gate/${consumer_id}'
		&& require_string_member(check, 'run_url')! == run_url
		&& require_string_member(check, 'job_url')! == job_url
		&& require_string_member(check, 'details_url')! == job_url
		&& require_string_member(check, 'ref')! == require_string_member(observed, 'ref')!
		&& require_string_member(check, 'workflow_head_sha')! == require_string_member(observed, 'sha')!
		&& require_string_member(check, 'sha')! == require_string_member(subject, 'sha')!
		&& require_string_member(check, 'check_sha')! == require_string_member(subject, 'sha')!
		&& require_string_member(check, 'actor')! == require_string_member(observed, 'actor')!
		&& require_string_member(check, 'triggering_actor')! == require_string_member(observed, 'triggering_actor')!
		&& require_string_member(check, 'created_at')! == require_string_member(observed, 'created_at')!
		&& require_string_member(check, 'run_conclusion')! == 'success'
		&& require_string_member(check, 'check_conclusion')! == 'success'
	integers_are_exact :=
		require_integer_member(check, 'integration_id')! == require_integer_member(native_source, 'integration_id')!
		&& require_integer_member(check, 'workflow_id')! == require_integer_member(native_source, 'workflow_id')!
		&& require_integer_member(check, 'run_id')! == require_integer_member(observed, 'run_id')!
		&& require_integer_member(check, 'run_attempt')! == require_integer_member(observed, 'run_attempt')!
		&& require_integer_member(check, 'check_suite_id')! == require_integer_member(observed, 'check_suite_id')!
		&& require_integer_member(check, 'check_suite_integration_id')! == require_integer_member(native_source, 'integration_id')!
		&& require_integer_member(check, 'actor_integration_id')! == require_integer_member(observed, 'actor_integration_id')!
		&& require_integer_member(check, 'triggering_actor_integration_id')! == require_integer_member(observed, 'triggering_actor_integration_id')!
	expected_external_id := deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		consumer_id, subject_hash, run_id, int(require_integer_member(check, 'run_attempt')!))!
	completed_at := require_string_member(check, 'completed_at')!
	time_is_exact := completed_at >= require_string_member(check, 'created_at')!
		&& completed_at >= require_string_member(epoch, 'opened_at')!
		&& completed_at <= require_nullable_string_member(epoch, 'closed_at')!
	if !strings_are_exact || !integers_are_exact
		|| require_string_member(check, 'external_id')! != expected_external_id || !time_is_exact {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_check',
			'terminal native check must retain its exact successful check-run, App, workflow, subject, actors, URLs and completion window')
	}
	return issues
}

fn terminal_source_transition_anchor_is_exact(transition JsonValue, source_state_pre JsonValue,
	source_state_post JsonValue, pre_projection JsonValue) !bool {
	universal := require_object_member(transition, 'universal_evidence')!
	operation_id := require_string_member(transition, 'operation_id')!
	transition_name := require_string_member(transition, 'transition')!
	source_id := require_string_member(transition, 'source_id')!
	previous_generation := require_integer_member(transition, 'previous_generation')!
	resulting_generation := require_integer_member(transition, 'resulting_generation')!
	run_id := require_integer_member(universal, 'run_id')!
	run_attempt := require_integer_member(universal, 'run_attempt')!
	operation_ordinal := require_integer_member(universal, 'operation_ordinal')!
	cas_attempt := require_integer_member(universal, 'cas_attempt')!
	subject_fingerprint := source_state_subject_fingerprint(source_state_pre)!
	state_path := source_state_path(source_id)!
	observed_at := require_string_member(transition, 'observed_at')!
	_ := exact_timestamp_unix(observed_at) or { return false }
	year := observed_at[0..4].int()
	month := observed_at[5..7].int()
	expected_evidence_path := evidence_path(year, month, run_id, int(run_attempt), source_id,
		operation_id, resulting_generation, transition_name, subject_fingerprint) or {
		return false
	}
	not_applicable_digest := '0000000000000000000000000000000000000000000000000000000000000000'
	expected_operation_id := deterministic_operation_id(OperationIdentityInput{
		audience:                'vlang/v:tccbin-source-state:v2'
		run_id:                  run_id
		run_attempt:             int(run_attempt)
		ordinal:                 int(operation_ordinal)
		cas_attempt:             int(cas_attempt)
		subject_id:              source_id
		transition:              transition_name
		expected_generation:     previous_generation
		expected_canonical_head: require_string_member(transition, 'expected_state_parent_sha')!
		source_ref:              require_string_member(source_state_pre, 'ref')!
		source_sha:              require_nullable_string_member(source_state_pre, 'resolved_sha')!
		subject_fingerprint:     subject_fingerprint
		input_fingerprint:       not_applicable_digest
		artifact_fingerprint:    not_applicable_digest
		manifest_hash:           not_applicable_digest
		native_subject_hash:     not_applicable_digest
	}) or { return false }
	digests := require_array_member(universal, 'digests')!
	digests_are_exact := digests.len == 1
		&& require_string_member(digests[0], 'path')! == state_path
		&& require_string_member(digests[0], 'sha256')! == source_state_snapshot_digest(source_state_post)!
	universal_is_exact := expected_operation_id == operation_id
		&& require_string_member(universal, 'operation_id')! == operation_id
		&& require_integer_member(universal, 'run_id')! == run_id
		&& require_member(universal, 'intent_id')!.kind == .null_value
		&& require_string_member(universal, 'transition')! == transition_name
		&& require_string_member(universal, 'workflow')! == '.github/workflows/tccbin_source_recovery.yml'
		&& require_string_member(universal, 'workflow_ref')! == 'master'
		&& require_string_member(universal, 'workflow_sha')! == require_string_member(pre_projection, 'v_source_sha')!
		&& require_string_member(universal, 'subject_id')! == source_id
		&& require_string_member(universal, 'subject_fingerprint')! == subject_fingerprint
		&& require_member(universal, 'target_id')!.kind == .null_value
		&& require_member(universal, 'input_fingerprint')!.kind == .null_value
		&& require_member(universal, 'artifact_fingerprint')!.kind == .null_value
		&& require_integer_member(universal, 'generation_read')! == previous_generation
		&& require_integer_member(universal, 'generation_written')! == resulting_generation
		&& require_string_member(universal, 'result')! == 'blocked' && digests_are_exact
	return universal_is_exact
		&& require_string_member(transition, 'universal_evidence_digest')! == source_state_universal_evidence_digest(universal)!
		&& require_string_member(transition, 'evidence_path')! == expected_evidence_path
}

fn validate_terminal_source_refetch(proof JsonValue, subject JsonValue, outcome string,
	business_id string, pre_generation i64, selected_attempt JsonValue, pre_projection JsonValue,
	final_projection JsonValue, terminal_completed_at string, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	refetch := require_member(proof, 'source_refetch')!
	source_state_pre := require_member(proof, 'source_state_pre_snapshot')!
	source_state := require_member(proof, 'source_state_snapshot')!
	source_history := require_member(proof, 'source_state_cas_history')!
	history := require_array_member(proof, 'source_state_cas_history')!
	pre_refetch := require_member(pre_projection, 'last_source_refetch')!
	final_refetch := require_member(final_projection, 'last_source_refetch')!
	if outcome != 'source_waiting' {
		if refetch.kind != .null_value || source_state_pre.kind != .null_value
			|| source_state.kind != .null_value || history.len != 0
			|| !json_equal(pre_refetch, final_refetch) {
			issues << semantic_issue('${path}/terminal_revalidation/source_refetch',
				'non-source terminal outcomes cannot invent a refetch, source-state CAS history, or source outage replacement')
		}
		return issues
	}
	if refetch.kind != .object || source_state_pre.kind != .object || source_state.kind != .object
		|| source_history.kind != .array || history.len != 1 {
		issues << semantic_issue('${path}/terminal_revalidation/source_refetch',
			'source_waiting requires one explicit refetch plus independent pre/post source-state snapshots and exactly one fully evidenced source CAS')
		return issues
	}
	failure_kind := require_nullable_string_member(refetch, 'failure_kind')!
	valid_failure := failure_kind in ['dns', 'connectivity', 'tls_transient', 'timeout', 'http_429',
		'http_5xx']
	refetch_is_exact :=
		require_string_member(refetch, 'target_id')! == require_string_member(subject, 'target_id')!
		&& require_integer_member(refetch, 'expected_generation')! == pre_generation
		&& require_string_member(refetch, 'expected_canonical_head')! == require_string_member(pre_projection, 'canonical_observed_sha')!
		&& require_string_member(refetch, 'input_fingerprint')! == require_nullable_string_member(pre_projection, 'input_fingerprint')!
		&& require_string_member(refetch, 'operation_id')! == business_id
		&& require_string_member(refetch, 'status')! == 'unreachable' && valid_failure
		&& require_member(refetch, 'resolved_sha')!.kind == .null_value
		&& require_member(refetch, 'resolved_tree')!.kind == .null_value
		&& !json_equal(pre_refetch, refetch) && json_equal(final_refetch, refetch)
	resolved_inputs := require_member(pre_projection, 'resolved_inputs')!
	mut source_matches := 0
	mut source_check_matches := 0
	if resolved_inputs.kind == .object {
		for source in require_array_member(resolved_inputs, 'sources')! {
			if require_string_member(source, 'id')! == require_string_member(refetch, 'source_id')!
				&& require_string_member(source, 'repository')! == require_string_member(refetch, 'source_repository')!
				&& require_string_member(source, 'ref')! == require_string_member(refetch, 'requested_ref')!
				&& require_string_member(source, 'sha')! == require_string_member(refetch, 'previous_sha')! {
				source_matches++
			}
		}
		for source_check in require_array_member(resolved_inputs, 'source_checks')! {
			if require_string_member(source_check, 'source_id')! == require_string_member(refetch, 'source_id')!
				&& require_string_member(source_check, 'resolved_sha')! == require_string_member(refetch, 'previous_sha')!
				&& require_string_member(source_check, 'status')! == 'resolved' {
				source_check_matches++
			}
		}
	}
	resolution_operation_id := require_string_member(refetch, 'resolution_operation_id')!
	pre_resolution_in_window := source_state_operation_window_contains(source_state_pre,
		resolution_operation_id)!
	resolution_in_window := source_state_operation_window_contains(source_state,
		resolution_operation_id)!
	pre_waiting_consumers := require_array_member(source_state_pre, 'waiting_consumers')!
	waiting_consumer_id := require_string_member(subject, 'consumer_id')!
	mut waiting_matches := 0
	for consumer in require_array_member(source_state, 'waiting_consumers')! {
		if require_string(consumer)! == waiting_consumer_id {
			waiting_matches++
		}
	}
	pre_state_digest := source_state_snapshot_digest(source_state_pre)!
	post_state_digest := source_state_snapshot_digest(source_state)!
	pre_state_generation := require_integer_member(source_state_pre, 'generation')!
	post_state_generation := require_integer_member(source_state, 'generation')!
	mut history_is_exact := post_state_generation == pre_state_generation + i64(history.len)
		&& source_state_append_is_exact(source_state_pre, source_state, history[0])!
	mut expected_generation := pre_state_generation
	mut expected_previous_digest := pre_state_digest
	mut source_timestamps_are_exact := true
	mut previous_observed_unix := exact_timestamp_unix(require_string_member(source_state_pre,
		'last_attempt_at')!) or {
		issues << semantic_issue('${path}/terminal_revalidation/source_state_pre_snapshot/last_attempt_at',
			'source pre-state attempt time must be one exact UTC RFC3339 second')
		source_timestamps_are_exact = false
		i64(0)
	}
	for index, transition in history {
		operation_id := require_string_member(transition, 'operation_id')!
		observed_at := require_string_member(transition, 'observed_at')!
		observed_unix := exact_timestamp_unix(observed_at) or {
			issues << semantic_issue('${path}/terminal_revalidation/source_state_cas_history/${index}/observed_at',
				'source CAS observation time must be one exact UTC RFC3339 second')
			source_timestamps_are_exact = false
			i64(0)
		}
		resulting_digest := require_string_member(transition, 'resulting_state_digest')!
		history_is_exact = history_is_exact
			&& require_string_member(transition, 'source_id')! == require_string_member(source_state, 'source_id')!
			&& require_string_member(transition, 'transition')! == 'resolve_source_unreachable'
			&& require_integer_member(transition, 'previous_generation')! == expected_generation
			&& require_integer_member(transition, 'resulting_generation')! == expected_generation + 1
			&& require_string_member(transition, 'previous_state_digest')! == expected_previous_digest
			&& require_string_member(transition, 'evidence_digest')! == source_state_transition_evidence_digest(transition)!
			&& terminal_source_transition_anchor_is_exact(transition, source_state_pre, source_state, pre_projection)!
			&& operation_id != business_id && observed_unix > previous_observed_unix
		expected_generation++
		expected_previous_digest = resulting_digest
		previous_observed_unix = observed_unix
	}
	last_transition := history[history.len - 1]
	history_is_exact = history_is_exact && expected_previous_digest == post_state_digest
		&& require_string_member(last_transition, 'operation_id')! == resolution_operation_id
		&& require_string_member(last_transition, 'observed_at')! == require_string_member(source_state, 'last_attempt_at')!
		&& require_integer_member(last_transition, 'originating_run_id')! == require_nullable_integer_member(source_state, 'originating_run_id')!
	post_waiting_consumers := require_array_member(source_state, 'waiting_consumers')!
	mut waiting_history_is_exact := post_waiting_consumers.len >= pre_waiting_consumers.len
	mut seen_waiting_consumers := []string{}
	for index, consumer in post_waiting_consumers {
		consumer_id := require_string(consumer)!
		if consumer_id in seen_waiting_consumers {
			waiting_history_is_exact = false
		} else {
			seen_waiting_consumers << consumer_id
		}
		if index < pre_waiting_consumers.len && !json_equal(consumer, pre_waiting_consumers[index]) {
			waiting_history_is_exact = false
		}
	}
	pre_mode := require_string_member(source_state_pre, 'mode')!
	pre_originating_run_id := require_nullable_integer_member(source_state_pre,
		'originating_run_id')!
	post_originating_run_id := require_nullable_integer_member(source_state, 'originating_run_id')!
	initial_monthly_outage := pre_mode == 'monthly' && pre_originating_run_id == 0
	repeated_daily_outage := pre_mode == 'upstream-recovery-daily' && pre_originating_run_id > 0
		&& pre_originating_run_id == post_originating_run_id
	pre_attempt_unix := exact_timestamp_unix(require_string_member(source_state_pre,
		'last_attempt_at')!) or {
		source_timestamps_are_exact = false
		i64(0)
	}
	post_attempt_unix := exact_timestamp_unix(require_string_member(source_state, 'last_attempt_at')!) or {
		issues << semantic_issue('${path}/terminal_revalidation/source_state_snapshot/last_attempt_at',
			'source post-state attempt time must be one exact UTC RFC3339 second')
		source_timestamps_are_exact = false
		i64(0)
	}
	refetch_unix := exact_timestamp_unix(require_string_member(refetch, 'checked_at')!) or {
		issues << semantic_issue('${path}/terminal_revalidation/source_refetch/checked_at',
			'source refetch time must be one exact UTC RFC3339 second')
		source_timestamps_are_exact = false
		i64(0)
	}
	terminal_completed_unix := exact_timestamp_unix(terminal_completed_at) or {
		issues << semantic_issue('${path}/terminal_completed_at',
			'source_waiting completion time must be one exact UTC RFC3339 second')
		source_timestamps_are_exact = false
		i64(0)
	}
	selected_completion_unix := exact_timestamp_unix(require_nullable_string_member(selected_attempt,
		'completed_at')!) or {
		source_timestamps_are_exact = false
		i64(0)
	}
	if refetch_unix <= selected_completion_unix {
		issues << semantic_issue('${path}/terminal_revalidation/source_refetch/checked_at',
			'source refetch must be strictly later than the selected terminal V-smoke completion')
	}
	if terminal_completed_unix <= refetch_unix {
		issues << semantic_issue('${path}/terminal_completed_at',
			'source_waiting terminal completion must be strictly later than the source refetch')
	}
	if pre_mode == 'upstream-recovery-daily' && post_attempt_unix - pre_attempt_unix < 86_400 {
		issues << semantic_issue('${path}/terminal_revalidation/source_state_snapshot/last_attempt_at',
			'daily source resolution cannot run before the exact 24-hour recovery cadence')
	}
	source_state_is_exact := source_timestamps_are_exact
		&& terminal_source_state_matches(require_string_member(refetch, 'source_id')!, require_string_member(refetch, 'source_state_id')!)
		&& require_integer_member(refetch, 'source_state_generation')! == post_state_generation
		&& require_string_member(source_state_pre, 'source_id')! == require_string_member(refetch, 'source_state_id')!
		&& require_string_member(source_state, 'source_id')! == require_string_member(refetch, 'source_state_id')!
		&& require_string_member(source_state_pre, 'canonical_url')! == require_string_member(refetch, 'source_repository')!
		&& require_string_member(source_state, 'canonical_url')! == require_string_member(refetch, 'source_repository')!
		&& require_string_member(source_state_pre, 'ref')! == require_string_member(refetch, 'requested_ref')!
		&& require_string_member(source_state, 'ref')! == require_string_member(refetch, 'requested_ref')!
		&& require_string_member(source_state_pre, 'status')! == 'resolved'
		&& require_nullable_string_member(source_state_pre, 'resolved_sha')! == require_string_member(refetch, 'previous_sha')!
		&& require_string_member(source_state, 'status')! == 'source_unreachable'
		&& require_member(source_state, 'resolved_sha')!.kind == .null_value
		&& require_string_member(source_state_pre, 'source_fingerprint')! == require_string_member(source_state, 'source_fingerprint')!
		&& pre_attempt_unix < post_attempt_unix
		&& (pre_mode != 'upstream-recovery-daily' || post_attempt_unix - pre_attempt_unix >= 86_400)
		&& require_string_member(source_state, 'last_attempt_at')! == require_string_member(refetch, 'checked_at')!
		&& require_string_member(source_state, 'mode')! == 'upstream-recovery-daily'
		&& (initial_monthly_outage || repeated_daily_outage) && post_originating_run_id > 0
		&& refetch_unix > selected_completion_unix && resolution_operation_id != business_id
		&& terminal_completed_unix > refetch_unix && !pre_resolution_in_window
		&& resolution_in_window && history_is_exact && waiting_matches == 1
		&& waiting_history_is_exact
		&& require_integer_member(source_state, 'operation_count')! == require_integer_member(source_state_pre, 'operation_count')! + 1
		&& require_string_member(refetch, 'evidence_digest')! == source_refetch_evidence_digest(refetch, source_state_pre, source_state, source_history)!
	if !refetch_is_exact {
		issues << semantic_issue('${path}/terminal_revalidation/source_refetch',
			'source_waiting proof must bind the exact target generation, HEAD, input, transient failure and business CAS')
	}
	if source_matches != 1 || source_check_matches != 1 || !source_state_is_exact {
		issues << semantic_issue('${path}/terminal_revalidation/source_refetch',
			'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS from the independent pre-state to the exact durable outage state')
	}
	return issues
}

fn terminal_source_state_matches(source_id string, source_state_id string) bool {
	return match source_id {
		'tinycc' { source_state_id == 'tinycc-mob' }
		'bdwgc' { source_state_id == 'bdwgc-master' }
		'libatomic_ops' { source_state_id == 'libatomic_ops-master' }
		else { false }
	}
}

fn validate_terminal_business_projection(root JsonValue, subject JsonValue, subject_hash string,
	successor JsonValue, outcome string, business_transition string, pre JsonValue, final JsonValue,
	selected_attempt JsonValue, completion_generation i64, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	terminal_proof := require_object_member(successor, 'terminal_revalidation')!
	proof_native := require_object_member(terminal_proof, 'native_gate_execution')!
	proof_smoke := require_object_member(terminal_proof, 'v_smoke_execution')!
	consumer_id := require_string_member(subject, 'consumer_id')!
	consumer_kind := require_string_member(subject, 'consumer_kind')!
	owner_count, owner_generation, owner_transition := operation_occurrences(root, consumer_id)!
	plain_owner_transition := match consumer_kind {
		'publish_post' { 'promotion_confirmed' }
		'rollback_post' { 'rollback_promoted' }
		'remediation' { 'begin_remediation' }
		else { '' }
	}
	expected_owner_transition := '${plain_owner_transition}_${terminal_owner_payload_digest(pre)!}'
	owner_cas_is_exact := owner_count == 1
		&& owner_generation == require_integer_member(subject, 'subject_generation')!
		&& plain_owner_transition != '' && owner_transition == expected_owner_transition
	pre_generation := require_integer_member(pre, 'generation')!
	final_generation := require_integer_member(final, 'generation')!
	pre_owner_sources := require_member(pre, 'owner_check_sources')!
	expected_sources := require_member(successor, 'expected_check_sources')!
	pre_common_is_exact := final_generation == completion_generation
		&& pre_generation + 2 == completion_generation
		&& require_nullable_string_member(pre, 'active_recovery_handoff_id')! == require_string_member(successor, 'handoff_id')!
		&& require_nullable_string_member(pre, 'native_subject_hash')! == subject_hash
		&& require_nullable_string_member(pre, 'native_consumer_kind')! == consumer_kind
		&& require_bool_member(pre, 'native_gate_execution_present')!
		&& require_bool_member(pre, 'v_smoke_execution_present')!
		&& require_nullable_string_member(final, 'active_recovery_handoff_id')! == ''
		&& json_equal(pre_owner_sources, expected_sources)
		&& json_equal(require_member(pre, 'native_gate_subject')!, subject)
		&& json_equal(require_member(pre, 'native_gate_execution')!, proof_native)
		&& json_equal(require_member(pre, 'v_smoke_execution')!, proof_smoke)
		&& terminal_projection_is_self_consistent(pre)!
		&& terminal_projection_is_self_consistent(final)!
		&& terminal_projection_root_tuple_matches_subject(pre, subject)!
	mut pre_owner_is_exact := false
	if consumer_kind in ['publish_post', 'rollback_post'] {
		expected_intent_type := if consumer_kind == 'publish_post' { 'publish' } else { 'rollback' }
		pre_owner_is_exact = require_nullable_string_member(pre, 'active_intent_id')! != '' && require_nullable_string_member(pre, 'active_intent_type')! == expected_intent_type && require_nullable_string_member(pre, 'active_intent_stage')! == 'post_checks_running' && require_nullable_string_member(pre, 'post_validation_operation_id')! == consumer_id && require_nullable_string_member(pre, 'active_remediation_id')! == '' && require_nullable_string_member(pre, 'active_remediation_operation_id')! == '' && require_string_member(pre, 'target_state')! == if consumer_kind == 'publish_post' {
			'validating'
		} else {
			'quarantined'
		} && require_string_member(pre, 'publication_state')! == if consumer_kind == 'publish_post' {
			'post_publish_validating'
		} else {
			'rollback_pending'
		}
	} else if consumer_kind == 'remediation' {
		pre_owner_is_exact = require_nullable_string_member(pre, 'active_intent_id')! == ''
			&& require_nullable_string_member(pre, 'post_validation_operation_id')! == ''
			&& require_nullable_string_member(pre, 'active_remediation_id')! == consumer_id
			&& require_nullable_string_member(pre, 'active_remediation_operation_id')! == consumer_id
			&& require_string_member(pre, 'target_state')! == 'validating'
			&& require_string_member(pre, 'publication_state')! == 'idle'
	}
	if !pre_common_is_exact || !pre_owner_is_exact {
		issues << semantic_issue('${path}/terminal_revalidation/pre_business_projection',
			'terminal pre-business snapshot must retain the exact selected subject, CAS generation and owning consumer')
	}
	if !owner_cas_is_exact {
		issues << semantic_issue('${path}/terminal_revalidation/pre_business_projection',
			'terminal owner payload differs from its immutable reservation CAS commitment')
	}
	final_lkg := require_member(final, 'last_known_good')!
	final_provisional := require_member(final, 'provisional_published')!
	pre_lkg := require_member(pre, 'last_known_good')!
	pre_provisional := require_member(pre, 'provisional_published')!
	mut final_is_exact := false
	match business_transition {
		'post_check_green' {
			final_is_exact = consumer_kind == 'publish_post' && outcome in ['green', 'no_op']
				&& require_string_member(pre, 'target_state')! == 'validating'
				&& require_string_member(pre, 'publication_state')! == 'post_publish_validating'
				&& pre_provisional.kind == .object
				&& subject_artifact_tuple_matches(pre_provisional, subject, true)!
				&& require_string_member(final, 'target_state')! == 'eligible'
				&& require_string_member(final, 'publication_state')! == 'idle'
				&& final_lkg.kind == .object
				&& subject_artifact_tuple_matches(final_lkg, subject, true)!
				&& final_provisional.kind == .null_value
				&& terminal_projection_owners_are_cleared(final)!
				&& terminal_projection_root_tuple_matches_subject(final, subject)!
				&& terminal_projection_has_no_blockers(final)!
		}
		'post_check_red' {
			business_id := require_string_member(terminal_proof, 'business_operation_id')!
			head_observation := require_member(final, 'last_head_observation')!
			ancestry_proof := require_member(terminal_proof, 'git_ancestry_proof')!
			head_is_bound := terminal_head_observation_is_exact(head_observation, subject, pre,
				business_id)!
			rollback_owner :=
				require_string_member(final, 'publication_state')! == 'rollback_pending'
				&& require_nullable_string_member(final, 'active_intent_id')! == business_id
				&& require_nullable_string_member(final, 'active_intent_type')! == 'rollback'
				&& require_nullable_string_member(final, 'active_intent_stage')! == 'intent_reserved'
				&& require_nullable_string_member(final, 'native_subject_hash')! == ''
				&& !require_bool_member(final, 'native_gate_execution_present')!
				&& !require_bool_member(final, 'v_smoke_execution_present')! && head_is_bound
				&& require_string_member(head_observation, 'relationship')! == 'exact_subject'
				&& ancestry_proof.kind == .null_value
				&& terminal_reserved_rollback_is_exact(final, pre, subject, business_id, expected_sources)!
			adopt_owner :=
				require_string_member(final, 'publication_state')! == 'candidate_pending'
				&& require_nullable_string_member(final, 'active_intent_id')! == business_id
				&& require_nullable_string_member(final, 'active_intent_type')! == 'adopt-current'
				&& require_nullable_string_member(final, 'native_subject_hash')! != ''
				&& require_nullable_string_member(final, 'native_consumer_kind')! == 'adopt_current'
				&& require_bool_member(final, 'native_gate_execution_present')!
				&& require_bool_member(final, 'v_smoke_execution_present')! && head_is_bound
				&& require_string_member(head_observation, 'relationship')! == 'subject_ancestor'
				&& terminal_git_ancestry_is_exact(ancestry_proof, head_observation, subject, business_id)!
				&& terminal_reserved_adopt_current_is_exact(final, pre, subject, business_id, expected_sources)!
			final_is_exact = consumer_kind == 'publish_post'
				&& outcome == 'functional_defect_routed'
				&& require_string_member(final, 'target_state')! == 'quarantined'
				&& json_equal(final_lkg, pre_lkg) && json_equal(final_provisional, pre_provisional)
				&& pre_provisional.kind == .object
				&& subject_artifact_tuple_matches(pre_provisional, subject, true)!
				&& require_nullable_string_member(final, 'post_validation_operation_id')! == ''
				&& require_nullable_string_member(final, 'active_remediation_id')! == ''
				&& (rollback_owner || adopt_owner)
		}
		'post_check_infra_exhausted' {
			final_is_exact = consumer_kind == 'publish_post' && outcome == 'infrastructure_blocked'
				&& require_string_member(final, 'target_state')! == 'quarantined'
				&& require_string_member(final, 'publication_state')! == 'post_publish_blocked'
				&& json_equal(final_lkg, pre_lkg) && json_equal(final_provisional, pre_provisional)
				&& pre_provisional.kind == .object
				&& subject_artifact_tuple_matches(pre_provisional, subject, true)!
				&& terminal_projection_root_tuple_matches_subject(final, subject)!
				&& require_nullable_string_member(final, 'active_intent_id')! == require_nullable_string_member(pre, 'active_intent_id')!
				&& require_nullable_string_member(final, 'active_intent_type')! == 'publish'
				&& require_nullable_string_member(final, 'active_intent_stage')! == 'blocked'
				&& require_nullable_string_member(final, 'post_validation_operation_id')! == consumer_id
				&& require_nullable_string_member(final, 'native_subject_hash')! == subject_hash
				&& require_nullable_string_member(final, 'native_consumer_kind')! == consumer_kind
				&& require_bool_member(final, 'native_gate_execution_present')!
				&& require_bool_member(final, 'v_smoke_execution_present')!
				&& json_objects_equal_except(require_object_member(pre, 'active_intent')!, require_object_member(final, 'active_intent')!, ['stage'])!
				&& terminal_projection_retained_executions_are_exact(pre, final)!
				&& json_equal(require_member(final, 'owner_check_sources')!, expected_sources)
		}
		'rollback_post_green' {
			final_is_exact = consumer_kind == 'rollback_post' && outcome in ['green', 'no_op']
				&& require_string_member(pre, 'target_state')! == 'quarantined'
				&& require_string_member(pre, 'publication_state')! == 'rollback_pending'
				&& require_string_member(final, 'target_state')! == 'quarantined'
				&& require_string_member(final, 'publication_state')! == 'restored_last_known_good'
				&& final_lkg.kind == .object
				&& subject_artifact_tuple_matches(final_lkg, subject, true)!
				&& final_provisional.kind == .null_value
				&& terminal_projection_owners_are_cleared(final)!
				&& terminal_projection_root_tuple_matches_subject(final, subject)!
		}
		'rollback_failed' {
			final_is_exact = consumer_kind == 'rollback_post'
				&& outcome in ['functional_defect_routed', 'infrastructure_blocked']
				&& require_string_member(final, 'target_state')! == 'quarantined'
				&& require_string_member(final, 'publication_state')! == 'rollback_blocked'
				&& json_equal(final_lkg, pre_lkg) && json_equal(final_provisional, pre_provisional)
				&& terminal_projection_root_tuple_matches_subject(final, subject)!
				&& require_nullable_string_member(final, 'active_intent_id')! == require_nullable_string_member(pre, 'active_intent_id')!
				&& require_nullable_string_member(final, 'active_intent_type')! == 'rollback'
				&& require_nullable_string_member(final, 'active_intent_stage')! == 'blocked'
				&& require_nullable_string_member(final, 'post_validation_operation_id')! == consumer_id
				&& require_nullable_string_member(final, 'native_subject_hash')! == subject_hash
				&& require_nullable_string_member(final, 'native_consumer_kind')! == consumer_kind
				&& require_bool_member(final, 'native_gate_execution_present')!
				&& require_bool_member(final, 'v_smoke_execution_present')!
				&& json_objects_equal_except(require_object_member(pre, 'active_intent')!, require_object_member(final, 'active_intent')!, ['stage'])!
				&& terminal_projection_retained_executions_are_exact(pre, final)!
				&& json_equal(require_member(final, 'owner_check_sources')!, expected_sources)
		}
		'remediation_green' {
			final_is_exact = consumer_kind == 'remediation' && outcome in ['green', 'no_op']
				&& require_string_member(final, 'target_state')! == 'eligible'
				&& require_string_member(final, 'publication_state')! == 'idle'
				&& final_lkg.kind == .object
				&& subject_artifact_tuple_matches(final_lkg, subject, true)!
				&& final_provisional.kind == .null_value
				&& terminal_projection_owners_are_cleared(final)!
				&& terminal_projection_root_tuple_matches_subject(final, subject)!
				&& terminal_projection_has_no_blockers(final)!
		}
		'remediation_red' {
			final_is_exact = consumer_kind == 'remediation'
				&& outcome in ['functional_defect_routed', 'infrastructure_blocked']
				&& require_string_member(final, 'target_state')! == 'quarantined'
				&& require_string_member(final, 'publication_state')! == require_string_member(pre, 'publication_state')!
				&& json_equal(final_lkg, pre_lkg) && json_equal(final_provisional, pre_provisional)
				&& terminal_projection_root_tuple_matches_subject(final, subject)!
				&& terminal_projection_owners_are_cleared(final)!
		}
		'source_unreachable' {
			preserved := terminal_projection_fields_equal(pre, final, [
				'canonical_observed_sha',
				'input_fingerprint',
				'artifact_fingerprint',
				'manifest_hash',
				'v_source_sha',
				'resolved_inputs',
				'last_known_good',
				'provisional_published',
				'blocking_probe_ids',
				'issue_number',
			])!
			mut waiting_owner := false
			if consumer_kind in ['publish_post', 'rollback_post'] {
				expected_target := if consumer_kind == 'publish_post' {
					'validating'
				} else {
					'quarantined'
				}
				expected_publication := if consumer_kind == 'publish_post' {
					'post_publish_waiting_source'
				} else {
					'rollback_waiting_source'
				}
				waiting_owner = require_string_member(final, 'target_state')! == expected_target
					&& require_string_member(final, 'publication_state')! == expected_publication
					&& require_nullable_string_member(final, 'active_intent_id')! == require_nullable_string_member(pre, 'active_intent_id')!
					&& require_nullable_string_member(final, 'active_intent_type')! == require_nullable_string_member(pre, 'active_intent_type')!
					&& require_nullable_string_member(final, 'active_intent_stage')! == 'post_checks_waiting_source'
					&& require_nullable_string_member(final, 'post_validation_operation_id')! == consumer_id
					&& json_objects_equal_except(require_object_member(pre, 'active_intent')!, require_object_member(final, 'active_intent')!, ['stage'])!
			} else if consumer_kind == 'remediation' {
				waiting_owner = require_string_member(final, 'target_state')! == 'quarantined'
					&& require_string_member(final, 'publication_state')! == require_string_member(pre, 'publication_state')!
					&& require_nullable_string_member(final, 'active_intent_id')! == ''
					&& require_nullable_string_member(final, 'post_validation_operation_id')! == ''
					&& require_nullable_string_member(final, 'active_remediation_id')! == consumer_id
					&& require_nullable_string_member(final, 'active_remediation_operation_id')! == consumer_id
					&& json_equal(require_member(pre, 'active_remediation_binding')!, require_member(final, 'active_remediation_binding')!)
					&& json_equal(require_member(pre, 'remediation_check_sources')!, require_member(final, 'remediation_check_sources')!)
			}
			final_is_exact = outcome == 'source_waiting' && preserved && waiting_owner
				&& require_nullable_string_member(final, 'native_subject_hash')! == subject_hash
				&& require_nullable_string_member(final, 'native_consumer_kind')! == consumer_kind
				&& require_bool_member(final, 'native_gate_execution_present')!
				&& require_bool_member(final, 'v_smoke_execution_present')!
				&& terminal_projection_retained_executions_are_exact(pre, final)!
				&& json_equal(require_member(final, 'owner_check_sources')!, expected_sources)
		}
		else {}
	}
	if business_transition != 'post_check_red'
		&& require_member(terminal_proof, 'git_ancestry_proof')!.kind != .null_value {
		final_is_exact = false
	}
	if !terminal_native_validation_projection_is_exact(pre, final) {
		final_is_exact = false
		issues << semantic_issue('${path}/terminal_revalidation/final_projection/last_native_validation',
			'terminal business CAS does not preserve or derive its exact native validation record')
	}
	if !final_is_exact {
		issues << semantic_issue('${path}/terminal_revalidation/final_projection',
			'terminal business CAS must project the exact target, artifact tuple, blockers and successor owners')
	}
	if require_integer_member(selected_attempt, 'run_id')! <= 0 {
		issues << semantic_issue('${path}/terminal_revalidation/v_smoke_execution',
			'terminal projection lacks its selected durable V-smoke run')
	}
	return issues
}

fn terminal_native_validation_projection_is_exact(pre JsonValue, final JsonValue) bool {
	before := require_member(pre, 'last_native_validation') or { return false }
	after := require_member(final, 'last_native_validation') or { return false }
	// T2c2 records remain dormant until T2c3 can project the exact capsule authority into H2.
	// Every legacy or source-waiting handoff therefore carries only the explicit null migration
	// value; a non-null record fails closed instead of being synthesized from recovery JSON.
	return before.kind == .null_value && after.kind == .null_value
}

fn validate_current_terminal_projection(root JsonValue, successor JsonValue, projection JsonValue,
	path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	completion_id := require_nullable_string_member(successor, 'completion_operation_id')!
	if require_integer_member(root, 'generation')! != require_integer_member(successor, 'expected_ledger_generation')!
		|| require_nullable_string_member(root, 'last_operation_id')! != completion_id {
		return issues
	}
	intent := require_member(root, 'active_intent')!
	native_subject := require_member(root, 'native_gate_subject')!
	remediation_binding := require_member(root, 'active_remediation_binding')!
	mut intent_id := ''
	mut intent_type := ''
	mut intent_stage := ''
	mut native_kind := ''
	mut remediation_operation_id := ''
	mut owner_sources := JsonValue{
		kind:        .array
		array_value: []JsonValue{}
	}
	mut v_source_sha := ''
	if intent.kind == .object {
		intent_id = require_string_member(intent, 'intent_id')!
		intent_type = require_string_member(intent, 'intent_type')!
		intent_stage = require_string_member(intent, 'stage')!
		owner_sources = require_member(intent, 'expected_check_sources')!
		v_source_sha = require_string_member(require_object_member(intent, 'resolved_inputs')!,
			'v_source_sha')!
	}
	if native_subject.kind == .object {
		native_kind = require_string_member(native_subject, 'consumer_kind')!
	}
	if remediation_binding.kind == .object {
		remediation_operation_id = require_string_member(remediation_binding, 'operation_id')!
		owner_sources = require_member(remediation_binding, 'expected_check_sources')!
		v_source_sha = require_string_member(remediation_binding, 'v_source_sha')!
	}
	resolved_inputs := require_member(root, 'resolved_inputs')!
	if resolved_inputs.kind == .object {
		v_source_sha = require_string_member(resolved_inputs, 'v_source_sha')!
	}
	direct_fields_are_exact := terminal_projection_fields_equal(root, projection, [
		'generation',
		'target_state',
		'publication_state',
		'canonical_observed_sha',
		'input_fingerprint',
		'artifact_fingerprint',
		'manifest_hash',
		'resolved_inputs',
		'last_known_good',
		'provisional_published',
		'active_intent',
		'post_validation_operation_id',
		'native_gate_subject',
		'native_gate_execution',
		'v_smoke_execution',
		'active_recovery_handoff_id',
		'active_remediation_id',
		'active_remediation_binding',
		'remediation_check_sources',
		'last_head_observation',
		'last_validation',
		'last_native_validation',
		'last_source_refetch',
		'blocking_probe_ids',
		'issue_number',
	])!
	derived_fields_are_exact :=
		require_nullable_string_member(projection, 'active_intent_id')! == intent_id
		&& require_nullable_string_member(projection, 'active_intent_type')! == intent_type
		&& require_nullable_string_member(projection, 'active_intent_stage')! == intent_stage
		&& require_nullable_string_member(projection, 'native_subject_hash')! == require_nullable_string_member(root, 'active_subject_hash')!
		&& require_nullable_string_member(projection, 'native_consumer_kind')! == native_kind
		&& require_bool_member(projection, 'native_gate_execution_present')! == (require_member(root, 'native_gate_execution')!.kind == .object)
		&& require_bool_member(projection, 'v_smoke_execution_present')! == (require_member(root, 'v_smoke_execution')!.kind == .object)
		&& require_nullable_string_member(projection, 'active_remediation_operation_id')! == remediation_operation_id
		&& require_string_member(projection, 'v_source_sha')! == v_source_sha
		&& json_equal(require_member(projection, 'owner_check_sources')!, owner_sources)
	if !direct_fields_are_exact || !derived_fields_are_exact {
		issues << semantic_issue('${path}/terminal_revalidation/final_projection',
			'current terminal H2 snapshot differs from the authoritative current target root')
	}
	return issues
}

fn terminal_projection_fields_equal(left JsonValue, right JsonValue, keys []string) !bool {
	for key in keys {
		if !json_equal(require_member(left, key)!, require_member(right, key)!) {
			return false
		}
	}
	return true
}

fn json_objects_equal_except(left JsonValue, right JsonValue, ignored_keys []string) !bool {
	if left.kind != .object || right.kind != .object {
		return false
	}
	mut left_keys := []string{}
	mut left_values := []JsonValue{}
	for index, key in left.object_keys {
		if key !in ignored_keys {
			left_keys << key
			left_values << left.object_values[index]
		}
	}
	mut right_keys := []string{}
	mut right_values := []JsonValue{}
	for index, key in right.object_keys {
		if key !in ignored_keys {
			right_keys << key
			right_values << right.object_values[index]
		}
	}
	return json_equal(object_value_from_pairs(left_keys, left_values)!, object_value_from_pairs(right_keys,
		right_values)!)
}

fn terminal_projection_is_self_consistent(projection JsonValue) !bool {
	intent := require_member(projection, 'active_intent')!
	native_subject := require_member(projection, 'native_gate_subject')!
	native_execution := require_member(projection, 'native_gate_execution')!
	smoke := require_member(projection, 'v_smoke_execution')!
	remediation_binding := require_member(projection, 'active_remediation_binding')!
	remediation_sources := require_member(projection, 'remediation_check_sources')!
	owner_sources := require_member(projection, 'owner_check_sources')!
	mut expected_owner_sources := JsonValue{
		kind:        .array
		array_value: []JsonValue{}
	}
	mut expected_v_source_sha := ''
	if intent.kind == .object {
		intent_type := require_string_member(intent, 'intent_type')!
		intent_inputs_are_exact := json_equal(require_member(intent, 'resolved_inputs')!, require_member(projection,
			'resolved_inputs')!)
		intent_previous_is_exact := intent_type == 'initial_adopt_current'
			|| json_equal(require_member(intent, 'previous_last_known_good')!, require_member(projection, 'last_known_good')!)
		intent_bad_is_exact := intent_type != 'rollback'
			|| json_equal(require_member(intent, 'bad_provisional')!, require_member(projection, 'provisional_published')!)
		if remediation_binding.kind == .object || !intent_inputs_are_exact
			|| !intent_previous_is_exact || !intent_bad_is_exact
			|| require_string_member(intent, 'input_fingerprint')! != require_nullable_string_member(projection, 'input_fingerprint')! {
			return false
		}
		if require_nullable_string_member(projection, 'active_intent_id')! != require_string_member(intent, 'intent_id')!
			|| require_nullable_string_member(projection, 'active_intent_type')! != require_string_member(intent, 'intent_type')!
			|| require_nullable_string_member(projection, 'active_intent_stage')! != require_string_member(intent, 'stage')! {
			return false
		}
		expected_owner_sources = require_member(intent, 'expected_check_sources')!
		expected_v_source_sha = require_string_member(require_object_member(intent,
			'resolved_inputs')!, 'v_source_sha')!
	} else if require_nullable_string_member(projection, 'active_intent_id')! != ''
		|| require_nullable_string_member(projection, 'active_intent_type')! != ''
		|| require_nullable_string_member(projection, 'active_intent_stage')! != '' {
		return false
	}
	if native_subject.kind == .object {
		parsed_subject := parse_receiver_subject(native_subject) or { return false }
		expected_hash := native_gate_subject_hash(native_subject_from_recovery(parsed_subject)) or {
			return false
		}
		if require_nullable_string_member(projection, 'native_subject_hash')! != expected_hash
			|| require_nullable_string_member(projection, 'native_consumer_kind')! != require_string_member(native_subject, 'consumer_kind')! {
			return false
		}
		if native_execution.kind != .object || smoke.kind != .object
			|| !terminal_projection_root_tuple_matches_subject(projection, native_subject)!
			|| !json_equal(require_member(native_execution, 'subject')!, native_subject)
			|| require_string_member(native_execution, 'subject_hash')! != expected_hash
			|| require_string_member(native_execution, 'subject_sha')! != require_string_member(native_subject, 'sha')!
			|| require_integer_member(native_execution, 'subject_generation')! != require_integer_member(native_subject, 'subject_generation')!
			|| require_integer_member(native_execution, 'expected_ledger_generation')! != require_integer_member(projection, 'generation')!
			|| require_string_member(smoke, 'consumer_id')! != require_string_member(native_subject, 'consumer_id')!
			|| require_string_member(smoke, 'intent_or_operation_id')! != require_string_member(native_subject, 'intent_or_operation_id')!
			|| require_string_member(smoke, 'consumer_kind')! != require_string_member(native_subject, 'consumer_kind')!
			|| require_string_member(smoke, 'target_id')! != require_string_member(native_subject, 'target_id')!
			|| require_string_member(smoke, 'subject_hash')! != expected_hash
			|| require_integer_member(smoke, 'subject_generation')! != require_integer_member(native_subject, 'subject_generation')!
			|| require_string_member(smoke, 'subject_ref')! != require_string_member(native_subject, 'original_ref')!
			|| require_string_member(smoke, 'subject_sha')! != require_string_member(native_subject, 'sha')!
			|| require_integer_member(smoke, 'expected_ledger_generation')! != require_integer_member(projection, 'generation')! {
			return false
		}
	} else if require_nullable_string_member(projection, 'native_subject_hash')! != ''
		|| require_nullable_string_member(projection, 'native_consumer_kind')! != ''
		|| native_execution.kind != .null_value || smoke.kind != .null_value {
		return false
	}
	if require_bool_member(projection, 'native_gate_execution_present')! != (native_execution.kind == .object)
		|| require_bool_member(projection, 'v_smoke_execution_present')! != (smoke.kind == .object) {
		return false
	}
	if remediation_binding.kind == .object {
		operation_id := require_string_member(remediation_binding, 'operation_id')!
		binding_sources := require_member(remediation_binding, 'expected_check_sources')!
		binding_validation_subject := require_member(remediation_binding, 'validation_subject')!
		if require_nullable_string_member(projection, 'active_remediation_id')! != operation_id
			|| require_nullable_string_member(projection, 'active_remediation_operation_id')! != operation_id
			|| !json_equal(remediation_sources, binding_sources) || native_subject.kind != .object
			|| require_string_member(native_subject, 'consumer_id')! != operation_id
			|| require_string_member(native_subject, 'consumer_kind')! != 'remediation'
			|| require_integer_member(remediation_binding, 'subject_generation')! != require_integer_member(native_subject, 'subject_generation')!
			|| !subject_artifact_tuple_matches(binding_validation_subject, native_subject, true)!
			|| require_string_member(binding_validation_subject, 'candidate_ref')! != require_string_member(native_subject, 'original_ref')!
			|| !json_equal(require_member(remediation_binding, 'remediation_trigger')!, require_member(native_subject, 'remediation_trigger')!) {
			return false
		}
		expected_owner_sources = binding_sources
		expected_v_source_sha = require_string_member(remediation_binding, 'v_source_sha')!
	} else if require_nullable_string_member(projection, 'active_remediation_operation_id')! != ''
		|| require_nullable_string_member(projection, 'active_remediation_id')! != ''
		|| require_array_member(projection, 'remediation_check_sources')!.len != 0 {
		return false
	}
	resolved_inputs := require_member(projection, 'resolved_inputs')!
	if resolved_inputs.kind == .object {
		expected_v_source_sha = require_string_member(resolved_inputs, 'v_source_sha')!
	}
	return expected_v_source_sha != ''
		&& require_string_member(projection, 'v_source_sha')! == expected_v_source_sha
		&& json_equal(owner_sources, expected_owner_sources)
}

fn terminal_projection_retained_executions_are_exact(pre JsonValue, final JsonValue) !bool {
	pre_native := require_member(pre, 'native_gate_execution')!
	final_native := require_member(final, 'native_gate_execution')!
	pre_smoke := require_member(pre, 'v_smoke_execution')!
	final_smoke := require_member(final, 'v_smoke_execution')!
	if pre_native.kind != .object || final_native.kind != .object || pre_smoke.kind != .object
		|| final_smoke.kind != .object {
		return false
	}
	return
		json_equal(require_member(pre, 'native_gate_subject')!, require_member(final, 'native_gate_subject')!)
		&& require_nullable_string_member(pre, 'native_subject_hash')! == require_nullable_string_member(final, 'native_subject_hash')!
		&& json_objects_equal_except(pre_native, final_native, ['expected_ledger_generation'])!
		&& require_integer_member(pre_native, 'expected_ledger_generation')! == require_integer_member(pre, 'generation')!
		&& require_integer_member(final_native, 'expected_ledger_generation')! == require_integer_member(final, 'generation')!
		&& json_objects_equal_except(pre_smoke, final_smoke, ['expected_ledger_generation', 'replay_facts_digest'])!
		&& require_integer_member(pre_smoke, 'expected_ledger_generation')! == require_integer_member(pre, 'generation')!
		&& require_integer_member(final_smoke, 'expected_ledger_generation')! == require_integer_member(final, 'generation')!
		&& require_string_member(pre_smoke, 'replay_facts_digest')! == v_smoke_replay_facts_digest(pre_smoke)!
		&& require_string_member(final_smoke, 'replay_facts_digest')! == v_smoke_replay_facts_digest(final_smoke)!
}

fn terminal_projection_root_tuple_matches_subject(projection JsonValue, subject JsonValue) !bool {
	return
		require_string_member(projection, 'canonical_observed_sha')! == require_string_member(subject, 'sha')!
		&& require_nullable_string_member(projection, 'input_fingerprint')! == require_string_member(subject, 'input_fingerprint')!
		&& require_nullable_string_member(projection, 'artifact_fingerprint')! == require_string_member(subject, 'artifact_fingerprint')!
		&& require_nullable_string_member(projection, 'manifest_hash')! == require_string_member(subject, 'manifest_hash')!
}

fn terminal_head_observation_is_exact(observation JsonValue, old_subject JsonValue,
	pre JsonValue, business_id string) !bool {
	if observation.kind != .object {
		return false
	}
	relationship := require_string_member(observation, 'relationship')!
	canonical_head := require_string_member(observation, 'canonical_head')!
	subject_sha := require_string_member(old_subject, 'sha')!
	return
		require_string_member(observation, 'target_id')! == require_string_member(old_subject, 'target_id')!
		&& require_integer_member(observation, 'expected_generation')! == require_integer_member(pre, 'generation')!
		&& require_string_member(observation, 'expected_previous_head')! == require_string_member(pre, 'canonical_observed_sha')!
		&& require_string_member(observation, 'subject_sha')! == subject_sha
		&& require_string_member(observation, 'operation_id')! == business_id
		&& relationship in ['exact_subject', 'subject_ancestor']
		&& ((relationship == 'exact_subject' && canonical_head == subject_sha)
		|| (relationship == 'subject_ancestor' && canonical_head != subject_sha))
}

fn terminal_git_ancestry_is_exact(ancestry JsonValue, observation JsonValue,
	old_subject JsonValue, business_id string) !bool {
	if ancestry.kind != .object || observation.kind != .object {
		return false
	}
	target_id := require_string_member(old_subject, 'target_id')!
	subject_sha := require_string_member(old_subject, 'sha')!
	canonical_head := require_string_member(observation, 'canonical_head')!
	evidence_digest := require_string_member(ancestry, 'evidence_digest')!
	return require_string_member(ancestry, 'repository')! == 'vlang/tccbin'
		&& require_string_member(ancestry, 'canonical_ref')! == 'thirdparty-${target_id}'
		&& require_string_member(ancestry, 'target_id')! == target_id
		&& require_string_member(ancestry, 'subject_sha')! == subject_sha
		&& require_string_member(ancestry, 'canonical_head')! == canonical_head
		&& require_string_member(ancestry, 'merge_base_sha')! == subject_sha
		&& require_string_member(ancestry, 'relationship')! == 'subject_ancestor'
		&& require_string_member(ancestry, 'query_method')! == 'git_merge_base_is_ancestor'
		&& require_string_member(ancestry, 'observed_at')! == require_string_member(observation, 'observed_at')!
		&& require_string_member(ancestry, 'operation_id')! == business_id
		&& require_string_member(observation, 'evidence_digest')! == evidence_digest
		&& evidence_digest == git_ancestry_evidence_digest(ancestry)!
}

fn terminal_reserved_rollback_is_exact(final JsonValue, pre JsonValue, subject JsonValue,
	business_id string, expected_sources JsonValue) !bool {
	intent := require_member(final, 'active_intent')!
	if intent.kind != .object {
		return false
	}
	return require_string_member(intent, 'intent_id')! == business_id
		&& require_string_member(intent, 'intent_type')! == 'rollback'
		&& require_string_member(intent, 'stage')! == 'intent_reserved'
		&& require_integer_member(intent, 'generation')! == require_integer_member(pre, 'generation')!
		&& require_string_member(intent, 'input_fingerprint')! == require_nullable_string_member(pre, 'input_fingerprint')!
		&& json_equal(require_member(intent, 'resolved_inputs')!, require_member(pre, 'resolved_inputs')!)
		&& require_string_member(intent, 'expected_canonical_head')! == require_string_member(subject, 'sha')!
		&& require_string_member(intent, 'candidate_ref')! == 'tccbin-candidate/${require_string_member(subject, 'target_id')!}/${business_id}'
		&& json_equal(require_member(intent, 'expected_check_sources')!, expected_sources)
		&& json_equal(require_member(intent, 'previous_last_known_good')!, require_member(pre, 'last_known_good')!)
		&& json_equal(require_member(intent, 'bad_provisional')!, require_member(pre, 'provisional_published')!)
		&& require_member(intent, 'validation_subject')!.kind == .null_value
		&& require_member(intent, 'candidate_binding')!.kind == .null_value
		&& require_member(intent, 'rollback_provisional')!.kind == .null_value
		&& terminal_projection_root_tuple_matches_subject(final, subject)!
}

fn terminal_reserved_adopt_current_is_exact(final JsonValue, pre JsonValue,
	old_subject JsonValue, business_id string, expected_sources JsonValue) !bool {
	intent := require_member(final, 'active_intent')!
	native_subject := require_member(final, 'native_gate_subject')!
	native_execution := require_member(final, 'native_gate_execution')!
	smoke := require_member(final, 'v_smoke_execution')!
	head_observation := require_member(final, 'last_head_observation')!
	if intent.kind != .object || native_subject.kind != .object || native_execution.kind != .object
		|| smoke.kind != .object || head_observation.kind != .object {
		return false
	}
	validation_subject := require_member(intent, 'validation_subject')!
	if validation_subject.kind != .object {
		return false
	}
	canonical_head := require_string_member(head_observation, 'canonical_head')!
	new_subject_hash := require_nullable_string_member(final, 'native_subject_hash')!
	validation_tuple_is_exact :=
		require_string_member(validation_subject, 'sha')! == canonical_head
		&& require_string_member(validation_subject, 'sha')! == require_string_member(native_subject, 'sha')!
		&& require_string_member(validation_subject, 'tree')! == require_string_member(native_subject, 'tree')!
		&& require_string_member(validation_subject, 'candidate_ref')! == require_string_member(native_subject, 'original_ref')!
		&& require_string_member(validation_subject, 'input_fingerprint')! == require_string_member(native_subject, 'input_fingerprint')!
		&& require_string_member(validation_subject, 'artifact_fingerprint')! == require_string_member(native_subject, 'artifact_fingerprint')!
		&& require_string_member(validation_subject, 'manifest_hash')! == require_string_member(native_subject, 'manifest_hash')!
		&& json_equal(require_member(validation_subject, 'digests')!, require_member(native_subject, 'digests')!)
	intent_is_exact := require_string_member(intent, 'intent_id')! == business_id
		&& require_string_member(intent, 'intent_type')! == 'adopt-current'
		&& require_string_member(intent, 'stage')! == 'intent_reserved'
		&& require_integer_member(intent, 'generation')! == require_integer_member(pre, 'generation')!
		&& require_string_member(intent, 'input_fingerprint')! == require_nullable_string_member(pre, 'input_fingerprint')!
		&& json_equal(require_member(intent, 'resolved_inputs')!, require_member(pre, 'resolved_inputs')!)
		&& require_string_member(intent, 'expected_canonical_head')! == canonical_head
		&& require_string_member(intent, 'candidate_ref')! == 'tccbin-candidate/${require_string_member(old_subject, 'target_id')!}/${business_id}'
		&& json_equal(require_member(intent, 'expected_check_sources')!, expected_sources)
		&& json_equal(require_member(intent, 'previous_last_known_good')!, require_member(pre, 'last_known_good')!)
		&& require_member(intent, 'bad_provisional')!.kind == .null_value
		&& require_member(intent, 'candidate_binding')!.kind == .null_value
		&& require_member(intent, 'rollback_provisional')!.kind == .null_value
	native_is_exact := require_string_member(native_subject, 'consumer_id')! == business_id
		&& require_string_member(native_subject, 'intent_or_operation_id')! == business_id
		&& require_string_member(native_subject, 'consumer_kind')! == 'adopt_current'
		&& require_string_member(native_subject, 'target_id')! == require_string_member(old_subject, 'target_id')!
		&& require_integer_member(native_subject, 'subject_generation')! == require_integer_member(pre, 'generation')! + 1
		&& json_equal(require_member(native_execution, 'subject')!, native_subject)
		&& require_string_member(native_execution, 'subject_hash')! == new_subject_hash
		&& require_integer_member(native_execution, 'expected_ledger_generation')! == require_integer_member(final, 'generation')!
	smoke_is_exact := require_string_member(smoke, 'consumer_id')! == business_id
		&& require_string_member(smoke, 'intent_or_operation_id')! == business_id
		&& require_string_member(smoke, 'consumer_kind')! == 'adopt_current'
		&& require_string_member(smoke, 'target_id')! == require_string_member(old_subject, 'target_id')!
		&& require_string_member(smoke, 'subject_hash')! == new_subject_hash
		&& require_integer_member(smoke, 'subject_generation')! == require_integer_member(pre, 'generation')! + 1
		&& require_string_member(smoke, 'subject_sha')! == canonical_head
		&& require_string_member(smoke, 'state')! == 'pending'
		&& require_integer_member(smoke, 'expected_ledger_generation')! == require_integer_member(final, 'generation')!
		&& require_string_member(smoke, 'replay_facts_digest')! == v_smoke_replay_facts_digest(smoke)!
	return validation_tuple_is_exact && intent_is_exact && native_is_exact && smoke_is_exact
		&& require_string_member(final, 'canonical_observed_sha')! == canonical_head
		&& terminal_projection_root_tuple_matches_subject(final, native_subject)!
}

fn terminal_projection_owners_are_cleared(projection JsonValue) !bool {
	return require_member(projection, 'active_intent')!.kind == .null_value
		&& require_nullable_string_member(projection, 'active_intent_id')! == ''
		&& require_nullable_string_member(projection, 'active_intent_type')! == ''
		&& require_nullable_string_member(projection, 'active_intent_stage')! == ''
		&& require_nullable_string_member(projection, 'post_validation_operation_id')! == ''
		&& require_member(projection, 'native_gate_subject')!.kind == .null_value
		&& require_nullable_string_member(projection, 'native_subject_hash')! == ''
		&& require_nullable_string_member(projection, 'native_consumer_kind')! == ''
		&& require_member(projection, 'native_gate_execution')!.kind == .null_value
		&& require_member(projection, 'v_smoke_execution')!.kind == .null_value
		&& !require_bool_member(projection, 'native_gate_execution_present')!
		&& !require_bool_member(projection, 'v_smoke_execution_present')!
		&& require_nullable_string_member(projection, 'active_recovery_handoff_id')! == ''
		&& require_nullable_string_member(projection, 'active_remediation_id')! == ''
		&& require_member(projection, 'active_remediation_binding')!.kind == .null_value
		&& require_nullable_string_member(projection, 'active_remediation_operation_id')! == ''
		&& require_array_member(projection, 'remediation_check_sources')!.len == 0
		&& require_array_member(projection, 'owner_check_sources')!.len == 0
}

fn terminal_projection_has_no_blockers(projection JsonValue) !bool {
	return require_array_member(projection, 'blocking_probe_ids')!.len == 0
		&& require_member(projection, 'issue_number')!.kind == .null_value
}

// terminal_state_projection snapshots the complete terminal decision surface while leaving the
// append-only operation ledger and historical handoff array outside the recursive proof.
pub fn terminal_state_projection(root JsonValue) !JsonValue {
	intent := require_member(root, 'active_intent')!
	native_subject := require_member(root, 'native_gate_subject')!
	remediation_binding := require_member(root, 'active_remediation_binding')!
	mut intent_id := JsonValue{
		kind: .null_value
	}
	mut intent_type := JsonValue{
		kind: .null_value
	}
	mut intent_stage := JsonValue{
		kind: .null_value
	}
	mut native_kind := JsonValue{
		kind: .null_value
	}
	mut remediation_operation_id := JsonValue{
		kind: .null_value
	}
	mut owner_sources := JsonValue{
		kind:        .array
		array_value: []JsonValue{}
	}
	mut v_source_sha := ''
	if intent.kind == .object {
		intent_id = require_member(intent, 'intent_id')!
		intent_type = require_member(intent, 'intent_type')!
		intent_stage = require_member(intent, 'stage')!
		owner_sources = require_member(intent, 'expected_check_sources')!
		v_source_sha = require_string_member(require_object_member(intent, 'resolved_inputs')!,
			'v_source_sha')!
	}
	if native_subject.kind == .object {
		native_kind = require_member(native_subject, 'consumer_kind')!
	}
	if remediation_binding.kind == .object {
		remediation_operation_id = require_member(remediation_binding, 'operation_id')!
		owner_sources = require_member(remediation_binding, 'expected_check_sources')!
		v_source_sha = require_string_member(remediation_binding, 'v_source_sha')!
	}
	resolved_inputs := require_member(root, 'resolved_inputs')!
	if resolved_inputs.kind == .object {
		v_source_sha = require_string_member(resolved_inputs, 'v_source_sha')!
	}
	if v_source_sha == '' {
		return error('terminal projection has no durable V source SHA')
	}
	keys := ['schema_version', 'generation', 'target_state', 'publication_state',
		'canonical_observed_sha', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash',
		'v_source_sha', 'resolved_inputs', 'last_known_good', 'provisional_published',
		'active_intent', 'active_intent_id', 'active_intent_type', 'active_intent_stage',
		'post_validation_operation_id', 'native_gate_subject', 'native_subject_hash',
		'native_consumer_kind', 'native_gate_execution', 'native_gate_execution_present',
		'v_smoke_execution', 'v_smoke_execution_present', 'active_recovery_handoff_id',
		'active_remediation_id', 'active_remediation_binding', 'active_remediation_operation_id',
		'remediation_check_sources', 'owner_check_sources', 'last_head_observation',
		'last_validation', 'last_native_validation', 'last_source_refetch', 'blocking_probe_ids',
		'issue_number']
	values := [JsonValue{ kind: .integer, int_value: 3 }, require_member(root, 'generation')!,
		require_member(root, 'target_state')!, require_member(root, 'publication_state')!,
		require_member(root, 'canonical_observed_sha')!, require_member(root, 'input_fingerprint')!,
		require_member(root, 'artifact_fingerprint')!, require_member(root, 'manifest_hash')!,
		JsonValue{
			kind:         .string_value
			string_value: v_source_sha
		}, require_member(root, 'resolved_inputs')!, require_member(root, 'last_known_good')!,
		require_member(root, 'provisional_published')!, intent, intent_id, intent_type, intent_stage,
		require_member(root, 'post_validation_operation_id')!, native_subject,
		require_member(root,
			'active_subject_hash')!, native_kind,
		require_member(root,
			'native_gate_execution')!, JsonValue{
			kind:       .boolean
			bool_value: require_member(root, 'native_gate_execution')!.kind == .object
		}, require_member(root, 'v_smoke_execution')!, JsonValue{
			kind:       .boolean
			bool_value: require_member(root, 'v_smoke_execution')!.kind == .object
		}, require_member(root, 'active_recovery_handoff_id')!,
		require_member(root, 'active_remediation_id')!, remediation_binding, remediation_operation_id,
		require_member(root, 'remediation_check_sources')!, owner_sources,
		require_member(root,
			'last_head_observation')!, require_member(root, 'last_validation')!,
		require_member(root, 'last_native_validation')!, require_member(root, 'last_source_refetch')!,
		require_member(root, 'blocking_probe_ids')!, require_member(root, 'issue_number')!]
	return object_value_from_pairs(keys, values)
}

fn validate_terminal_revalidation_sources(proof JsonValue, predecessor JsonValue,
	successor JsonValue, native_execution JsonValue, smoke JsonValue, path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	proof_sources := require_member(proof, 'expected_check_sources')!
	if !json_equal(proof_sources, require_member(predecessor, 'expected_check_sources')!)
		|| !json_equal(proof_sources, require_member(successor, 'expected_check_sources')!) {
		issues << semantic_issue('${path}/terminal_revalidation/expected_check_sources',
			'terminal H2 check authority must equal the immutable predecessor H1 authority')
	}
	mut native_source := JsonValue{
		kind: .null_value
	}
	mut smoke_source := JsonValue{
		kind: .null_value
	}
	for source in require_array_member(proof, 'expected_check_sources')! {
		if require_string_member(source, 'name')! == 'tccbin-candidate-gate' {
			native_source = source
		} else if require_string_member(source, 'name')! == 'v-candidate-smoke' {
			smoke_source = source
		}
	}
	if native_source.kind != .object || smoke_source.kind != .object {
		issues << semantic_issue('${path}/terminal_revalidation/expected_check_sources',
			'terminal H2 proof must retain exactly both allowlisted check sources')
		return issues
	}
	if require_integer_member(smoke_source, 'integration_id')! != require_integer_member(smoke, 'validator_integration_id')!
		|| require_integer_member(smoke_source, 'workflow_id')! != require_integer_member(smoke, 'workflow_id')!
		|| require_string_member(smoke_source, 'repository')! != require_string_member(smoke, 'repository')!
		|| require_string_member(smoke_source, 'workflow_path')! != require_string_member(smoke, 'workflow_path')!
		|| require_string_member(smoke_source, 'event')! != require_string_member(smoke, 'event')!
		|| require_integer_member(native_source, 'integration_id')! != require_integer_member(smoke, 'actions_integration_id')! {
		issues << semantic_issue('${path}/terminal_revalidation/expected_check_sources',
			'terminal H2 V-smoke proof differs from its allowlisted workflow and Apps')
	}
	active_epoch := require_integer_member(native_execution, 'active_gate_epoch')!
	selected_id := require_nullable_integer_member(native_execution, 'selected_run_id')!
	selected_attempt := require_nullable_integer_member(native_execution, 'selected_run_attempt')!
	mut native_matches := 0
	for run in require_array_member(native_execution, 'gate_runs')! {
		if require_integer_member(run, 'gate_epoch')! == active_epoch
			&& require_integer_member(run, 'run_id')! == selected_id
			&& require_integer_member(run, 'run_attempt')! == selected_attempt {
			native_matches++
			if require_integer_member(run, 'workflow_id')! != require_integer_member(native_source, 'workflow_id')!
				|| require_string_member(run, 'repository')! != require_string_member(native_source, 'repository')!
				|| require_string_member(run, 'workflow_path')! != require_string_member(native_source, 'workflow_path')!
				|| require_string_member(run, 'event')! != require_string_member(native_source, 'event')! {
				issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution',
					'terminal H1 native run differs from its allowlisted workflow')
			}
		}
	}
	if native_matches != 1 {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution',
			'terminal H1 proof must retain exactly its selected native run')
	}
	return issues
}

fn validate_historical_native_gate_semantics(predecessor JsonValue, execution JsonValue,
	path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	subject := require_object_member(predecessor, 'subject')!
	mut native_source := JsonValue{
		kind: .null_value
	}
	for source in require_array_member(predecessor, 'expected_check_sources')! {
		if require_string_member(source, 'name')! == 'tccbin-candidate-gate' {
			native_source = source
		}
	}
	if native_source.kind != .object
		|| require_string_member(execution, 'repository')! != require_string_member(native_source, 'repository')!
		|| require_integer_member(execution, 'workflow_id')! != require_integer_member(native_source, 'workflow_id')!
		|| require_string_member(execution, 'workflow_path')! != require_string_member(native_source, 'workflow_path')! {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution',
			'native recovery evidence must retain its predecessor-anchored workflow authority')
		return issues
	}
	epochs := require_array_member(execution, 'gate_epochs')!
	active_epoch := require_integer_member(execution, 'active_gate_epoch')!
	mut epochs_are_contiguous := active_epoch == i64(epochs.len - 1)
	for index, epoch in epochs {
		if require_integer_member(epoch, 'epoch')! != i64(index) {
			epochs_are_contiguous = false
		}
	}
	if active_epoch < 0 || active_epoch >= i64(epochs.len) {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution/active_gate_epoch',
			'native recovery evidence must retain its exact latest contiguous selected epoch')
		return issues
	}
	epoch := epochs[int(active_epoch)]
	selected_id := require_nullable_integer_member(execution, 'selected_run_id')!
	selected_attempt := require_nullable_integer_member(execution, 'selected_run_attempt')!
	selected_suite := require_nullable_integer_member(execution, 'selected_check_suite_id')!
	selected_conclusion := require_nullable_string_member(execution, 'selected_conclusion')!
	if !epochs_are_contiguous || require_string_member(epoch, 'state')! != 'completed'
		|| require_nullable_integer_member(epoch, 'selected_run_id')! != selected_id
		|| require_nullable_integer_member(epoch, 'selected_run_attempt')! != selected_attempt
		|| require_nullable_integer_member(epoch, 'selected_check_suite_id')! != selected_suite
		|| require_nullable_string_member(epoch, 'conclusion')! != selected_conclusion {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution/gate_epochs',
			'native recovery evidence must retain its exact latest contiguous selected epoch')
	}
	mut selected_matches := 0
	for run in require_array_member(execution, 'gate_runs')! {
		if require_integer_member(run, 'gate_epoch')! != active_epoch
			|| require_integer_member(run, 'run_id')! != selected_id
			|| require_integer_member(run, 'run_attempt')! != selected_attempt
			|| require_integer_member(run, 'check_suite_id')! != selected_suite {
			continue
		}
		selected_matches++
		actor_is_exact :=
			require_string_member(run, 'actor')! == require_string_member(execution, 'original_actor')!
			&& require_integer_member(run, 'actor_integration_id')! == require_integer_member(execution, 'original_actor_integration_id')!
		trigger_is_exact := if selected_attempt == 1 {
				require_string_member(run, 'triggering_actor')! == require_string_member(execution, 'original_actor')!
				&& require_integer_member(run, 'triggering_actor_integration_id')! == require_integer_member(execution, 'original_actor_integration_id')!
		} else {
				require_string_member(run, 'triggering_actor')! == require_string_member(execution, 'rerun_triggering_actor')!
				&& require_integer_member(run, 'triggering_actor_integration_id')! == require_integer_member(execution, 'rerun_triggering_integration_id')!
		}
		run_created := require_string_member(run, 'created_at')!
		closed_at := require_nullable_string_member(epoch, 'closed_at')!
		original_ref_is_exact := require_string_member(epoch, 'reason')! != 'original_push'
			|| (require_string_member(epoch, 'expected_ref')! == require_string_member(subject, 'original_ref')!
			&& require_nullable_string_member(epoch, 'trigger_id')! == '')
		if require_string_member(run, 'repository')! != require_string_member(native_source, 'repository')!
			|| require_integer_member(run, 'workflow_id')! != require_integer_member(native_source, 'workflow_id')!
			|| require_string_member(run, 'workflow_path')! != require_string_member(native_source, 'workflow_path')!
			|| require_string_member(run, 'event')! != require_string_member(native_source, 'event')!
			|| require_string_member(run, 'ref')! != require_string_member(epoch, 'expected_ref')!
			|| require_string_member(run, 'sha')! != require_string_member(subject, 'sha')!
			|| require_string_member(run, 'conclusion')! != selected_conclusion || !actor_is_exact
			|| !trigger_is_exact || !original_ref_is_exact
			|| run_created < require_string_member(epoch, 'opened_at')! || closed_at == ''
			|| run_created > closed_at {
			issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution/gate_runs',
				'native recovery selected run must retain its exact subject ref, SHA, workflow, actors, Integration IDs and epoch')
		}
	}
	if selected_matches != 1 {
		issues << semantic_issue('${path}/terminal_revalidation/native_gate_execution/gate_runs',
			'native recovery evidence must retain exactly its selected observed run')
	}
	return issues
}

fn native_recovery_gate_is_green_and_cas_closed(root JsonValue, predecessor JsonValue,
	chain_generation i64, native_execution JsonValue) !bool {
	active_epoch := require_integer_member(native_execution, 'active_gate_epoch')!
	epochs := require_array_member(native_execution, 'gate_epochs')!
	if active_epoch < 0 || active_epoch >= i64(epochs.len) {
		return false
	}
	epoch := epochs[int(active_epoch)]
	selected_run_id := require_nullable_integer_member(native_execution, 'selected_run_id')!
	selected_run_attempt := require_nullable_integer_member(native_execution,
		'selected_run_attempt')!
	selected_suite := require_nullable_integer_member(native_execution, 'selected_check_suite_id')!
	gate_runs := require_array_member(native_execution, 'gate_runs')!
	ack_ids := require_array_member(native_execution, 'ack_operation_ids')!
	completion_ids := require_array_member(native_execution, 'completion_operation_ids')!
	mut selected_matches := 0
	for gate_run in gate_runs {
		if require_integer_member(gate_run, 'gate_epoch')! == active_epoch
			&& require_integer_member(gate_run, 'run_id')! == selected_run_id
			&& require_integer_member(gate_run, 'run_attempt')! == selected_run_attempt
			&& require_integer_member(gate_run, 'check_suite_id')! == selected_suite
			&& require_string_member(gate_run, 'conclusion')! == 'success' {
			selected_matches++
		}
	}
	if require_string_member(epoch, 'state')! != 'completed'
		|| require_nullable_string_member(epoch, 'conclusion')! != 'success'
		|| require_nullable_string_member(native_execution, 'selected_conclusion')! != 'success'
		|| selected_matches != 1 || ack_ids.len != gate_runs.len || completion_ids.len == 0 {
		return false
	}
	subject_hash := require_string_member(predecessor, 'subject_hash')!
	mut latest_ack_generation := i64(-1)
	for ack_value in ack_ids {
		ack_id := require_string(ack_value)!
		ack_count, ack_generation, ack_transition := operation_occurrences(root, ack_id)!
		if ack_count != 1 || ack_generation >= chain_generation
			|| ack_transition != 'native_gate_ack_${subject_hash}' {
			return false
		}
		if ack_generation > latest_ack_generation {
			latest_ack_generation = ack_generation
		}
	}
	for completion_value in completion_ids {
		completion_id := require_string(completion_value)!
		completion_count, completion_generation, completion_transition := operation_occurrences(root,
			completion_id)!
		if completion_count != 1 || completion_generation <= latest_ack_generation
			|| completion_generation >= chain_generation
			|| completion_transition != 'native_gate_complete_${subject_hash}' {
			return false
		}
	}
	return true
}

fn validate_native_subject_owner_semantics(root JsonValue, subject JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	consumer_id := require_string_member(subject, 'consumer_id')!
	consumer_kind := require_string_member(subject, 'consumer_kind')!
	target_id := require_string_member(root, 'target_id')!
	intent := require_member(root, 'active_intent')!
	post_operation_id := require_nullable_string_member(root, 'post_validation_operation_id')!
	remediation_id := require_nullable_string_member(root, 'active_remediation_id')!
	if remediation_id != '' && (intent.kind == .object || post_operation_id != '') {
		issues << semantic_issue('$/active_remediation_id',
			'remediation, publication intent and post-validation owners are mutually exclusive')
	}
	if post_operation_id != '' && intent.kind != .object {
		issues << semantic_issue('$/post_validation_operation_id',
			'post-validation must retain its owning active intent')
	}
	if intent.kind == .object && require_string_member(intent, 'stage')! == 'completed' {
		issues << semantic_issue('$/active_intent/stage',
			'completed intent cannot retain an active native subject or smoke consumer')
	}
	for binding in [['input_fingerprint', 'input_fingerprint'],
		['artifact_fingerprint', 'artifact_fingerprint'], ['manifest_hash', 'manifest_hash']] {
		if require_nullable_string_member(root, binding[0])! != require_string_member(subject,
			binding[1])! {
			issues << semantic_issue('$/native_gate_subject/${binding[1]}',
				'native subject fingerprints must equal the authoritative target projection')
		}
	}
	if consumer_kind in ['publish_candidate', 'rollback_candidate', 'adopt_current',
		'initial_adopt_current'] {
		if intent.kind != .object {
			issues << semantic_issue('$/active_intent',
				'candidate or adoption native subject requires its exact active intent; a handoff cannot replace it')
			return issues
		}
		expected_type := match consumer_kind {
			'publish_candidate' { 'publish' }
			'rollback_candidate' { 'rollback' }
			'adopt_current' { 'adopt-current' }
			else { 'initial_adopt_current' }
		}
		expected_ref := 'tccbin-candidate/${target_id}/${consumer_id}'
		if require_string_member(intent, 'intent_id')! != consumer_id
			|| require_string_member(intent, 'intent_type')! != expected_type
			|| require_string_member(intent, 'candidate_ref')! != expected_ref
			|| require_string_member(subject, 'original_ref')! != expected_ref
			|| require_string_member(intent, 'input_fingerprint')! != require_string_member(subject, 'input_fingerprint')!
			|| require_string_member(intent, 'expected_canonical_head')! != require_string_member(root, 'canonical_observed_sha')!
			|| require_integer_member(intent, 'generation')! >= require_integer_member(subject, 'subject_generation')!
			|| post_operation_id != '' || remediation_id != '' {
			issues << semantic_issue('$/active_intent',
				'native candidate subject identity, ref, generation or exclusive owner differs from its intent')
		}
		if consumer_kind in ['adopt_current', 'initial_adopt_current'] {
			validation_subject := require_member(intent, 'validation_subject')!
			if validation_subject.kind != .object
				|| !subject_artifact_tuple_matches(validation_subject, subject, true)!
				|| require_string_member(validation_subject, 'candidate_ref')! != require_string_member(subject, 'original_ref')!
				|| require_string_member(validation_subject, 'sha')! != require_string_member(intent, 'expected_canonical_head')! {
				issues << semantic_issue('$/active_intent/validation_subject',
					'native adoption subject must equal the complete durable validation subject tuple')
			}
		} else {
			candidate_binding := require_member(intent, 'candidate_binding')!
			if candidate_binding.kind != .object
				|| !subject_artifact_tuple_matches(candidate_binding, subject, false)!
				|| require_string_member(candidate_binding, 'parent')! != require_string_member(intent, 'expected_canonical_head')! {
				issues << semantic_issue('$/active_intent/candidate_binding',
					'native candidate subject must equal the complete durable candidate binding tuple')
			}
		}
	} else if consumer_kind in ['publish_post', 'rollback_post'] {
		expected_type := if consumer_kind == 'publish_post' { 'publish' } else { 'rollback' }
		intent_stage := if intent.kind == .object {
			require_string_member(intent, 'stage')!
		} else {
			''
		}
		publication_state := require_string_member(root, 'publication_state')!
		target_state := require_string_member(root, 'target_state')!
		expected_publication_state := if consumer_kind == 'publish_post' {
			match intent_stage {
				'post_checks_running' { 'post_publish_validating' }
				'post_checks_waiting_source' { 'post_publish_waiting_source' }
				'blocked' { 'post_publish_blocked' }
				else { '' }
			}
		} else {
			match intent_stage {
				'post_checks_running' { 'rollback_pending' }
				'post_checks_waiting_source' { 'rollback_waiting_source' }
				'blocked' { 'rollback_blocked' }
				else { '' }
			}
		}
		expected_target_state := if consumer_kind == 'publish_post' && intent_stage != 'blocked' {
			'validating'
		} else {
			'quarantined'
		}
		if intent.kind != .object || require_string_member(intent, 'intent_type')! != expected_type
			|| post_operation_id != consumer_id || remediation_id != ''
			|| require_string_member(subject, 'original_ref')! != 'thirdparty-${target_id}'
			|| require_string_member(root, 'canonical_observed_sha')! != require_string_member(subject, 'sha')!
			|| require_string_member(intent, 'input_fingerprint')! != require_string_member(subject, 'input_fingerprint')!
			|| require_integer_member(intent, 'generation')! >= require_integer_member(subject, 'subject_generation')! {
			issues << semantic_issue('$/post_validation_operation_id',
				'native post-validation subject differs from its exact intent, operation, ref or generation')
		}
		if expected_publication_state == '' || publication_state != expected_publication_state
			|| target_state != expected_target_state {
			issues << semantic_issue('$/publication_state',
				'post-validation kind, intent stage, publication state and target state must form one exact publish or rollback lane')
		}
		post_tuple := if consumer_kind == 'publish_post' {
			require_member(root, 'provisional_published')!
		} else {
			require_member(intent, 'rollback_provisional')!
		}
		if post_tuple.kind != .object
			|| !subject_artifact_tuple_matches(post_tuple, subject, consumer_kind == 'publish_post')! {
			issues << semantic_issue('$/native_gate_subject',
				'native post-validation subject must equal the complete promoted provisional tuple')
		}
	} else if consumer_kind == 'remediation' {
		remediation_binding := require_member(root, 'active_remediation_binding')!
		if remediation_id != consumer_id || intent.kind == .object || post_operation_id != ''
			|| remediation_binding.kind != .object
			|| require_string_member(subject, 'original_ref')! != 'thirdparty-${target_id}'
			|| require_string_member(root, 'canonical_observed_sha')! != require_string_member(subject, 'sha')! {
			issues << semantic_issue('$/active_remediation_id',
				'native remediation subject differs from its exclusive operation and canonical target HEAD')
		}
		trigger := require_object_member(subject, 'remediation_trigger')!
		if remediation_binding.kind == .object {
			validation_subject := require_object_member(remediation_binding, 'validation_subject')!
			if require_string_member(remediation_binding, 'operation_id')! != consumer_id
				|| require_integer_member(remediation_binding, 'subject_generation')! != require_integer_member(subject, 'subject_generation')!
				|| !subject_artifact_tuple_matches(validation_subject, subject, true)!
				|| require_string_member(validation_subject, 'candidate_ref')! != require_string_member(subject, 'original_ref')!
				|| !json_equal(require_object_member(remediation_binding, 'remediation_trigger')!, trigger)
				|| !json_equal(require_member(remediation_binding, 'expected_check_sources')!, require_member(root, 'remediation_check_sources')!) {
				issues << semantic_issue('$/active_remediation_binding',
					'remediation binding must independently retain the exact operation, subject, trigger and check authority')
			}
		}
		if require_string_member(trigger, 'repository')! == 'vlang/tccbin'
			&& (require_string_member(trigger, 'ref')! != 'thirdparty-${target_id}'
			|| require_string_member(trigger, 'after')! != require_string_member(subject, 'sha')!
			|| require_string_member(trigger, 'tree')! != require_string_member(subject, 'tree')!) {
			issues << semantic_issue('$/native_gate_subject/remediation_trigger',
				'tccbin remediation trigger after/tree/ref must equal the exact native subject')
		}
	}
	return issues
}

fn subject_artifact_tuple_matches(value JsonValue, subject JsonValue, includes_input bool) !bool {
	if require_string_member(value, 'sha')! != require_string_member(subject, 'sha')!
		|| require_string_member(value, 'tree')! != require_string_member(subject, 'tree')!
		|| require_string_member(value, 'artifact_fingerprint')! != require_string_member(subject, 'artifact_fingerprint')!
		|| require_string_member(value, 'manifest_hash')! != require_string_member(subject, 'manifest_hash')! {
		return false
	}
	if includes_input
		&& require_string_member(value, 'input_fingerprint')! != require_string_member(subject, 'input_fingerprint')! {
		return false
	}
	if !json_equal(require_member(value, 'digests')!, require_member(subject, 'digests')!) {
		return false
	}
	return true
}

fn validate_v_smoke_execution_semantics(root JsonValue, subject JsonValue, subject_hash string,
	smoke JsonValue) ![]SchemaIssue {
	mut retained_source_waiting := false
	for handoff in require_array_member(root, 'recovery_handoffs')! {
		if require_string_member(handoff, 'state')! == 'complete'
			&& require_nullable_string_member(handoff, 'terminal_outcome')! == 'source_waiting'
			&& require_string_member(handoff, 'subject_hash')! == subject_hash
			&& require_member(handoff, 'terminal_revalidation')!.kind == .object {
			retained_source_waiting = true
		}
	}
	return validate_v_smoke_execution_semantics_mode(root, subject, subject_hash, smoke, true, JsonValue{
		kind: .null_value
	}, '', retained_source_waiting)
}

fn validate_v_smoke_execution_semantics_mode(root JsonValue, subject JsonValue,
	subject_hash string, smoke JsonValue, require_current_owner bool, historical_sources JsonValue,
	historical_v_source_sha string, allow_source_retry_short_circuit bool) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	consumer_id := require_string_member(subject, 'consumer_id')!
	consumer_kind := require_string_member(subject, 'consumer_kind')!
	if consumer_id != require_string_member(subject, 'intent_or_operation_id')! {
		issues << semantic_issue('$/native_gate_subject/consumer_id',
			'native subject consumer must equal its intent or operation identity')
	}
	if require_string_member(subject, 'target_id')! != require_string_member(root, 'target_id')! {
		issues << semantic_issue('$/native_gate_subject/target_id',
			'native subject is bound to a different target row')
	}
	if require_current_owner
		&& consumer_kind in ['publish_candidate', 'rollback_candidate', 'adopt_current', 'initial_adopt_current'] {
		intent := require_member(root, 'active_intent')!
		if intent.kind != .object {
			issues << semantic_issue('$/active_intent',
				'candidate V smoke requires its exact active intent; a recovery handoff cannot replace the consumer')
		} else {
			expected_intent_type := match consumer_kind {
				'publish_candidate' { 'publish' }
				'rollback_candidate' { 'rollback' }
				'adopt_current' { 'adopt-current' }
				else { 'initial_adopt_current' }
			}
			if require_string_member(intent, 'intent_id')! != consumer_id
				|| require_string_member(intent, 'intent_type')! != expected_intent_type {
				issues << semantic_issue('$/active_intent',
					'active intent identity or kind differs from the native subject consumer')
			}
		}
	} else if require_current_owner && consumer_kind in ['publish_post', 'rollback_post'] {
		if require_nullable_string_member(root, 'post_validation_operation_id')! != consumer_id {
			issues << semantic_issue('$/post_validation_operation_id',
				'post-validation consumer does not own the active native subject and V smoke')
		}
		intent := require_member(root, 'active_intent')!
		expected_intent_type := if consumer_kind == 'publish_post' { 'publish' } else { 'rollback' }
		if intent.kind != .object
			|| require_string_member(intent, 'intent_type')! != expected_intent_type {
			issues << semantic_issue('$/active_intent',
				'post-validation consumer kind does not match its owning publish or rollback intent')
		}
	} else if require_current_owner && consumer_kind == 'remediation'
		&& require_nullable_string_member(root, 'active_remediation_id')! != consumer_id {
		issues << semantic_issue('$/active_remediation_id',
			'remediation consumer does not own the active native subject and V smoke')
	}
	bindings := [
		['consumer_id', 'consumer_id'],
		['consumer_kind', 'consumer_kind'],
		['intent_or_operation_id', 'intent_or_operation_id'],
		['target_id', 'target_id'],
		['subject_ref', 'original_ref'],
		['subject_sha', 'sha'],
	]
	for binding in bindings {
		if require_string_member(smoke, binding[0])! != require_string_member(subject, binding[1])! {
			issues << semantic_issue('$/v_smoke_execution/${binding[0]}',
				'V smoke identity differs from native subject ${binding[1]}')
		}
	}
	if require_string_member(smoke, 'subject_hash')! != subject_hash {
		issues << semantic_issue('$/v_smoke_execution/subject_hash',
			'V smoke subject hash is not canonical')
	}
	subject_generation := require_integer_member(subject, 'subject_generation')!
	if require_integer_member(smoke, 'subject_generation')! != subject_generation {
		issues << semantic_issue('$/v_smoke_execution/subject_generation',
			'V smoke generation differs from the immutable native subject generation')
	}
	generation := require_integer_member(root, 'generation')!
	smoke_generation := require_integer_member(smoke, 'expected_ledger_generation')!
	if (require_current_owner && smoke_generation != generation)
		|| (!require_current_owner && (smoke_generation < subject_generation
		|| smoke_generation > generation)) {
		issues << semantic_issue('$/v_smoke_execution/expected_ledger_generation',
			'V smoke CAS generation is stale')
	}
	if require_string_member(smoke, 'run_name')! != 'tccbin-v-smoke/${consumer_id}' {
		issues << semantic_issue('$/v_smoke_execution/run_name',
			'V smoke run name does not bind the exact consumer')
	}
	actions_integration_id := require_integer_member(smoke, 'actions_integration_id')!
	validator_integration_id := require_integer_member(smoke, 'validator_integration_id')!
	if actions_integration_id == validator_integration_id {
		issues << semantic_issue('$/v_smoke_execution/validator_integration_id',
			'GitHub Actions and validator App Integration IDs must be distinct')
	}
	mut configured_sources := []JsonValue{}
	if require_current_owner {
		if intent := root.object_value('active_intent') {
			if intent.kind == .object {
				configured_sources = require_array_member(intent, 'expected_check_sources')!
			}
		}
		if configured_sources.len == 0
			&& require_nullable_string_member(root, 'active_remediation_id')! != '' {
			remediation_binding := require_object_member(root, 'active_remediation_binding')!
			configured_sources =
				require_array_member(remediation_binding, 'expected_check_sources')!
		}
	} else if historical_sources.kind == .array {
		configured_sources = historical_sources.array_value.clone()
	}
	for source in configured_sources {
		name := require_string_member(source, 'name')!
		if name == 'v-candidate-smoke'
			&& (validator_integration_id != require_integer_member(source, 'integration_id')!
			|| require_integer_member(smoke, 'workflow_id')! != require_integer_member(source, 'workflow_id')!
			|| require_string_member(smoke, 'repository')! != require_string_member(source, 'repository')!
			|| require_string_member(smoke, 'workflow_path')! != require_string_member(source, 'workflow_path')!
			|| require_string_member(smoke, 'event')! != require_string_member(source, 'event')!) {
			issues << semantic_issue('$/v_smoke_execution',
				'V smoke reservation differs from the allowlisted validator check source')
		}
		if name == 'tccbin-candidate-gate'
			&& actions_integration_id != require_integer_member(source, 'integration_id')! {
			issues << semantic_issue('$/v_smoke_execution/actions_integration_id',
				'V run check suite is not bound to the allowlisted GitHub Actions App')
		}
	}
	if require_current_owner {
		if resolved_inputs := root.object_value('resolved_inputs') {
			if resolved_inputs.kind == .object
				&& require_string_member(resolved_inputs, 'v_source_sha')! != require_string_member(smoke, 'v_master_sha')! {
				issues << semantic_issue('$/v_smoke_execution/v_master_sha',
					'V workflow SHA differs from the resolved V source SHA')
			}
		}
		if intent := root.object_value('active_intent') {
			if intent.kind == .object {
				intent_inputs := require_object_member(intent, 'resolved_inputs')!
				if require_string_member(intent_inputs, 'v_source_sha')! != require_string_member(smoke,
					'v_master_sha')! {
					issues << semantic_issue('$/v_smoke_execution/v_master_sha',
						'V workflow SHA differs from the active intent resolved V source SHA')
				}
			}
		}
		if remediation_binding := root.object_value('active_remediation_binding') {
			if remediation_binding.kind == .object
				&& require_string_member(remediation_binding, 'v_source_sha')! != require_string_member(smoke, 'v_master_sha')! {
				issues << semantic_issue('$/v_smoke_execution/v_master_sha',
					'V workflow SHA differs from the durable remediation binding')
			}
		}
	} else if historical_v_source_sha == ''
		|| historical_v_source_sha != require_string_member(smoke, 'v_master_sha')! {
		issues << semantic_issue('$/v_smoke_execution/v_master_sha',
			'historical V workflow SHA differs from its append-only terminal projection')
	}
	remediation_trigger := require_member(subject, 'remediation_trigger')!
	if consumer_kind == 'remediation' && remediation_trigger.kind == .object
		&& require_string_member(remediation_trigger, 'repository')! == 'vlang/v'
		&& (require_string_member(remediation_trigger, 'ref')! != 'master'
		|| require_string_member(remediation_trigger, 'after')! != require_string_member(smoke, 'v_master_sha')!) {
		issues << semantic_issue('$/v_smoke_execution/v_master_sha',
			'V-owned remediation smoke is not bound to the exact reviewed vlang/v:master push')
	}
	reservation_id := require_string_member(smoke, 'reservation_operation_id')!
	reservation_count, reservation_generation, reservation_transition := operation_occurrences(root,
		reservation_id)!
	valid_reservation_transition := match consumer_kind {
		'initial_adopt_current' {
			reservation_transition == 'begin_bootstrap'
		}
		'adopt_current' {
			reservation_transition in ['reserve_adopt_current', 'post_check_red']
		}
		'publish_candidate', 'rollback_candidate' {
			reservation_transition == 'bind_candidate'
		}
		'publish_post' {
			reservation_transition == 'promotion_confirmed'
				|| reservation_transition.starts_with('promotion_confirmed_')
		}
		'rollback_post' {
			reservation_transition == 'rollback_promoted'
				|| reservation_transition.starts_with('rollback_promoted_')
		}
		'remediation' {
			reservation_transition == 'begin_remediation'
				|| reservation_transition.starts_with('begin_remediation_')
		}
		else {
			false
		}
	}
	if subject_generation <= 0 || reservation_count != 1
		|| reservation_generation != subject_generation
		|| reservation_generation > generation || !valid_reservation_transition
		|| (consumer_kind in ['publish_post', 'rollback_post', 'remediation']
		&& reservation_id != consumer_id) {
		issues << semantic_issue('$/v_smoke_execution/reservation_operation_id',
			'V smoke reservation must reuse the unique current owner CAS that atomically created the native subject')
	}

	dispatches := require_array_member(smoke, 'dispatches')!
	active_dispatch := require_nullable_integer_member(smoke, 'active_dispatch')!
	attempts := require_array_member(smoke, 'attempts')!
	run_absent_attempts := require_array_member(smoke, 'run_absent_attempts')!
	ack_ids := require_array_member(smoke, 'ack_operation_ids')!
	completion_ids := require_array_member(smoke, 'completion_operation_ids')!
	if attempts.len != ack_ids.len {
		issues << semantic_issue('$/v_smoke_execution/ack_operation_ids',
			'each attempt must have exactly one ordered ACK operation')
	}
	mut observed_completion_ids := []string{}
	execution_created_at := require_string_member(smoke, 'created_at')!
	execution_created_unix := exact_timestamp_unix(execution_created_at) or {
		issues << semantic_issue('$/v_smoke_execution/created_at',
			'V smoke creation time must be one exact UTC RFC3339 second')
		i64(0)
	}
	mut event_start_generations := [i64(-1), i64(-1)]
	mut event_end_generations := [i64(-1), i64(-1)]
	mut event_started_at := ['', '']
	mut event_completed_at := ['', '']
	mut dispatch_operation_ids := ['', '']
	mut dispatch_facts_digests := ['', '']
	mut dispatch_modes := ['', '']
	mut dispatch_rerun_run_ids := [i64(0), i64(0)]
	mut dispatch_expected_attempts := [i64(0), i64(0)]
	mut dispatch_requested_unix := [i64(-1), i64(-1)]
	mut dispatch_deadline_unix := [i64(-1), i64(-1)]
	mut outcome_counts := [0, 0]
	mut previous_dispatch_generation := reservation_generation
	for index, dispatch in dispatches {
		path := '$/v_smoke_execution/dispatches/${index}'
		attempt_index := require_integer_member(dispatch, 'attempt_index')!
		if attempt_index != i64(index + 1) {
			issues << semantic_issue('${path}/attempt_index',
				'dispatch reservations must be contiguous and ordered from logical attempt 1')
			continue
		}
		logical_index := int(attempt_index - 1)
		operation_id := require_string_member(dispatch, 'dispatch_operation_id')!
		operation_count, operation_generation, operation_transition := operation_occurrences(root,
			operation_id)!
		valid_dispatch_transition := operation_transition == 'v-smoke-dispatch-${attempt_index}'
			|| recovery_handoff_smoke_operation_matches(root, smoke, operation_id, operation_transition, 'dispatch')!
		if operation_count != 1 || operation_generation <= previous_dispatch_generation
			|| operation_generation > generation || !valid_dispatch_transition {
			issues << semantic_issue('${path}/dispatch_operation_id',
				'dispatch must be one unique, later and correctly typed pre-side-effect CAS operation')
		}
		previous_dispatch_generation = operation_generation
		requested_at := require_string_member(dispatch, 'requested_at')!
		discovery_deadline := require_string_member(dispatch, 'discovery_deadline')!
		requested_unix := exact_timestamp_unix(requested_at) or {
			issues << semantic_issue('${path}/requested_at',
				'dispatch request time must be one exact UTC RFC3339 second')
			i64(0)
		}
		discovery_deadline_unix := exact_timestamp_unix(discovery_deadline) or {
			issues << semantic_issue('${path}/discovery_deadline',
				'dispatch discovery deadline must be one exact UTC RFC3339 second')
			i64(0)
		}
		if requested_unix < execution_created_unix
			|| discovery_deadline_unix - requested_unix != 120 {
			issues << semantic_issue(path,
				'dispatch must retain its exact two-minute run-discovery window')
		}
		facts_digest := require_string_member(dispatch, 'facts_digest')!
		if facts_digest != v_smoke_dispatch_facts_digest(smoke, dispatch)! {
			issues << semantic_issue('${path}/facts_digest',
				'dispatch facts digest does not cover its complete immutable authority')
		}
		dispatch_operation_ids[logical_index] = operation_id
		dispatch_facts_digests[logical_index] = facts_digest
		dispatch_modes[logical_index] = require_string_member(dispatch, 'mode')!
		dispatch_rerun_run_ids[logical_index] = require_nullable_integer_member(dispatch,
			'rerun_of_run_id')!
		dispatch_expected_attempts[logical_index] = require_integer_member(dispatch,
			'expected_run_attempt')!
		dispatch_requested_unix[logical_index] = requested_unix
		dispatch_deadline_unix[logical_index] = discovery_deadline_unix
		event_start_generations[logical_index] = operation_generation
		event_end_generations[logical_index] = operation_generation
		event_started_at[logical_index] = requested_at
		event_completed_at[logical_index] = requested_at
	}
	for index, run_absent in run_absent_attempts {
		path := '$/v_smoke_execution/run_absent_attempts/${index}'
		attempt_index := require_integer_member(run_absent, 'attempt_index')!
		if attempt_index < 1 || attempt_index > 2 || (index > 0
			&& attempt_index <= require_integer_member(run_absent_attempts[index - 1], 'attempt_index')!) {
			issues << semantic_issue('${path}/attempt_index',
				'run-absent attempt indexes must be unique and strictly ordered')
			continue
		}
		logical_index := int(attempt_index - 1)
		if logical_index >= dispatches.len
			|| require_string_member(run_absent, 'dispatch_operation_id')! != dispatch_operation_ids[logical_index]
			|| require_string_member(run_absent, 'dispatch_facts_digest')! != dispatch_facts_digests[logical_index] {
			issues << semantic_issue('${path}/dispatch_operation_id',
				'run-absent outcome must chain the exact prior durable dispatch reservation')
		}
		operation_id := require_string_member(run_absent, 'run_absent_operation_id')!
		operation_count, operation_generation, operation_transition := operation_occurrences(root,
			operation_id)!
		if operation_count != 1 || operation_generation <= event_start_generations[logical_index]
			|| operation_generation > generation
			|| operation_transition != 'v-smoke-run-absent-${attempt_index}' {
			issues << semantic_issue('${path}/run_absent_operation_id',
				'run-absent outcome must be one unique, later and correctly typed CAS operation')
		}
		completed_at := require_string_member(run_absent, 'completed_at')!
		completed_unix := exact_timestamp_unix(completed_at) or {
			issues << semantic_issue('${path}/completed_at',
				'run-absent completion time must be one exact UTC RFC3339 second')
			i64(0)
		}
		if completed_unix < dispatch_deadline_unix[logical_index] {
			issues << semantic_issue(path,
				'run-absent outcome cannot precede the exact two-minute discovery deadline')
		}
		if require_string_member(run_absent, 'facts_digest')! != v_smoke_run_absent_facts_digest(smoke,
			run_absent)! {
			issues << semantic_issue('${path}/facts_digest',
				'run-absent facts digest does not cover the immutable dispatch failure')
		}
		event_end_generations[logical_index] = operation_generation
		event_completed_at[logical_index] = completed_at
		outcome_counts[logical_index]++
	}
	for index, attempt in attempts {
		path := '$/v_smoke_execution/attempts/${index}'
		attempt_index := require_integer_member(attempt, 'attempt_index')!
		if attempt_index < 1 || attempt_index > 2 || (index > 0
			&& attempt_index <= require_integer_member(attempts[index - 1], 'attempt_index')!) {
			issues << semantic_issue('${path}/attempt_index',
				'ACKed attempt indexes must be unique and strictly ordered')
		}
		logical_index := int(attempt_index - 1)
		if logical_index >= dispatches.len
			|| require_string_member(attempt, 'dispatch_operation_id')! != dispatch_operation_ids[logical_index]
			|| require_string_member(attempt, 'dispatch_facts_digest')! != dispatch_facts_digests[logical_index] {
			issues << semantic_issue('${path}/dispatch_operation_id',
				'ACKed attempt must chain the exact prior durable dispatch reservation')
		}
		outcome_counts[logical_index]++
		if outcome_counts[logical_index] > 1 {
			issues << semantic_issue('${path}/attempt_index',
				'one logical attempt cannot be both run-absent and ACKed')
		}
		for key in ['repository', 'workflow_path', 'workflow_ref', 'event', 'run_name'] {
			if require_string_member(attempt, key)! != require_string_member(smoke, key)! {
				issues << semantic_issue('${path}/${key}',
					'attempt field differs from reserved V smoke execution')
			}
		}
		if require_integer_member(attempt, 'workflow_id')! != require_integer_member(smoke, 'workflow_id')!
			|| require_string_member(attempt, 'head_sha')! != require_string_member(smoke, 'v_master_sha')!
			|| require_string_member(attempt, 'subject_ref')! != require_string_member(smoke, 'subject_ref')!
			|| require_string_member(attempt, 'subject_sha')! != require_string_member(smoke, 'subject_sha')! {
			issues << semantic_issue(path,
				'attempt workflow or subject binding differs from the reservation')
		}
		if require_integer_member(attempt, 'check_suite_integration_id')! != actions_integration_id {
			issues << semantic_issue('${path}/check_suite_integration_id',
				'run check suite is not owned by the allowlisted GitHub Actions App')
		}
		if require_integer_member(attempt, 'actor_integration_id')! != validator_integration_id
			|| require_integer_member(attempt, 'triggering_actor_integration_id')! != validator_integration_id
			|| require_string_member(attempt, 'actor')! != 'validator-dispatcher[bot]'
			|| require_string_member(attempt, 'triggering_actor')! != 'validator-dispatcher[bot]' {
			issues << semantic_issue(path,
				'run actor or triggering actor is not the allowlisted validator dispatcher App')
		}
		run_id := require_integer_member(attempt, 'run_id')!
		job_id := require_integer_member(attempt, 'job_id')!
		expected_run_url := 'https://github.com/vlang/v/actions/runs/${run_id}'
		expected_job_url := '${expected_run_url}/job/${job_id}'
		if require_string_member(attempt, 'run_url')! != expected_run_url
			|| require_string_member(attempt, 'job_url')! != expected_job_url {
			issues << semantic_issue(path,
				'run and job URLs must be exact, sanitized projections of their IDs')
		}
		created_at := require_string_member(attempt, 'created_at')!
		deadline := require_string_member(attempt, 'deadline')!
		rerunnable_until := require_string_member(attempt, 'rerunnable_until')!
		created_unix := exact_timestamp_unix(created_at) or {
			issues << semantic_issue('${path}/created_at',
				'attempt creation time must be one exact UTC RFC3339 second')
			i64(0)
		}
		deadline_unix := exact_timestamp_unix(deadline) or {
			issues << semantic_issue('${path}/deadline',
				'attempt deadline must be one exact UTC RFC3339 second')
			i64(0)
		}
		rerunnable_unix := exact_timestamp_unix(rerunnable_until) or {
			issues << semantic_issue('${path}/rerunnable_until',
				'attempt rerun cutoff must be one exact UTC RFC3339 second')
			i64(0)
		}
		if created_unix < execution_created_unix || deadline_unix - created_unix != 5_400
			|| rerunnable_unix - created_unix != 2_592_000 {
			issues << semantic_issue(path,
				'attempt must retain the exact 90-minute deadline and immutable 30-day rerun cutoff')
		}
		dispatch_run_matches := dispatch_modes[logical_index] != 'api_rerun'
			|| run_id == dispatch_rerun_run_ids[logical_index]
		if created_unix < dispatch_requested_unix[logical_index]
			|| created_unix > dispatch_deadline_unix[logical_index]
			|| require_integer_member(attempt, 'run_attempt')! != dispatch_expected_attempts[logical_index]
			|| !dispatch_run_matches {
			issues << semantic_issue(path,
				'ACKed run must be created inside and exactly match its durable dispatch mode, run and attempt')
		}

		ack_id := require_string_member(attempt, 'ack_operation_id')!
		if index >= ack_ids.len || require_string(ack_ids[index])! != ack_id {
			issues << semantic_issue('${path}/ack_operation_id',
				'attempt ACK is not linked 1:1 to the ordered ACK projection')
		}
		ack_count, ack_generation, ack_transition := operation_occurrences(root, ack_id)!
		valid_ack_transition := ack_transition == 'v-smoke-ack-${attempt_index}'
			|| recovery_handoff_smoke_operation_matches(root, smoke, ack_id, ack_transition, 'ack')!
		if ack_count != 1 || ack_generation <= event_start_generations[logical_index]
			|| ack_generation > generation || !valid_ack_transition {
			issues << semantic_issue('${path}/ack_operation_id',
				'attempt ACK must be one unique, current and correctly typed later CAS operation')
		}
		event_end_generations[logical_index] = ack_generation
		event_completed_at[logical_index] = created_at
		if require_string_member(attempt, 'ack_facts_digest')! != v_smoke_ack_facts_digest(smoke,
			attempt)! {
			issues << semantic_issue('${path}/ack_facts_digest',
				'ACK facts digest does not cover the immutable observed attempt')
		}

		completion_kind := require_nullable_string_member(attempt, 'completion_kind')!
		run_conclusion := require_nullable_string_member(attempt, 'run_conclusion')!
		completion_id := require_nullable_string_member(attempt, 'completion_operation_id')!
		if completion_kind != '' {
			completed_at := require_nullable_string_member(attempt, 'completed_at')!
			completed_unix := exact_timestamp_unix(completed_at) or { i64(-1) }
			if completed_at == '' || completed_unix < created_unix
				|| (completion_kind == 'deadline_exceeded' && completed_unix < deadline_unix)
				|| (completion_kind == 'actions_terminal' && completed_unix > deadline_unix) {
				issues << semantic_issue('${path}/completed_at',
					'attempt completion must be exact, ordered, respect a persisted deadline timeout and be actions-terminal no later than its persisted deadline')
			}
			observed_completion_ids << completion_id
			completion_count, completion_generation, completion_transition := operation_occurrences(root,
				completion_id)!
			completion_facts_digest := require_nullable_string_member(attempt,
				'completion_facts_digest')!
			plain_completion_transition := 'v-smoke-complete-${attempt_index}'
			terminal_payload_digest := v_smoke_terminal_payload_digest(smoke, attempt)!
			committed_completion_transition := '${plain_completion_transition}_${terminal_payload_digest}'
			completion_transition_is_exact := if require_current_owner {
				completion_transition == plain_completion_transition
					|| (completion_transition.starts_with('${plain_completion_transition}_')
					&& is_lower_hex_64(completion_transition.all_after('${plain_completion_transition}_')))
			} else {
				completion_transition == committed_completion_transition
			}
			if completion_count != 1 || completion_generation <= ack_generation
				|| completion_generation > generation || !completion_transition_is_exact {
				issues << semantic_issue('${path}/completion_operation_id',
					'attempt completion must be one unique later CAS operation committed to the complete historical V-smoke payload')
			}
			event_end_generations[logical_index] = completion_generation
			event_completed_at[logical_index] = completed_at
			if completion_facts_digest != v_smoke_completion_facts_digest(smoke, attempt)! {
				issues << semantic_issue('${path}/completion_facts_digest',
					'completion facts digest does not cover the terminal run and check facts')
			}
		}
		check_run := require_member(attempt, 'check_run_id')!
		if check_run.kind != .null_value {
			if run_conclusion == '' {
				issues << semantic_issue('${path}/check_run_id',
					'check cannot exist before its Actions run is terminal')
			}
			if require_nullable_string_member(attempt, 'check_sha')! != require_string_member(smoke, 'subject_sha')!
				|| require_nullable_string_member(attempt, 'details_url')! != expected_job_url
				|| require_nullable_integer_member(attempt, 'validator_integration_id')! != validator_integration_id {
				issues << semantic_issue(path,
					'validator check is not bound to the reserved subject, job and App')
			}
			expected_external_id := deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
				consumer_id, subject_hash, run_id, int(require_integer_member(attempt,
				'run_attempt')!))!
			if require_nullable_string_member(attempt, 'external_id')! != expected_external_id {
				issues << semantic_issue('${path}/external_id',
					'validator check external ID is not the deterministic JCS identity')
			}
		}
	}
	logical_outcome_count := attempts.len + run_absent_attempts.len
	if logical_outcome_count > dispatches.len || dispatches.len > 2
		|| outcome_counts[0] > 1 || outcome_counts[1] > 1
		|| (outcome_counts[1] > 0 && outcome_counts[0] != 1) {
		issues << semantic_issue('$/v_smoke_execution',
			'each durable dispatch must have at most one contiguous ACK or run-absent outcome')
	}
	if dispatches.len == 2 {
		if outcome_counts[0] != 1 || event_start_generations[1] <= event_end_generations[0]
			|| event_started_at[1] < event_completed_at[0] {
			issues << semantic_issue('$/v_smoke_execution/dispatches/1',
				'second dispatch must follow the first durable outcome in time and CAS order')
		}
		mut first_attempt := JsonValue{
			kind: .null_value
		}
		for attempt in attempts {
			if require_integer_member(attempt, 'attempt_index')! == 1 {
				first_attempt = attempt
			}
		}
		if first_attempt.kind == .object {
			first_completed_at := require_nullable_string_member(first_attempt, 'completed_at')!
			first_completed_unix := exact_timestamp_unix(first_completed_at) or { i64(-1) }
			first_cutoff := exact_timestamp_unix(require_string_member(first_attempt,
				'rerunnable_until')!) or { i64(-1) }
			if first_completed_at == '' || dispatch_requested_unix[1] < first_completed_unix {
				issues << semantic_issue('$/v_smoke_execution/dispatches/1/requested_at',
					'second dispatch cannot precede completion of the first ACKed attempt')
			}
			if dispatch_requested_unix[1] <= first_cutoff {
				if dispatch_modes[1] != 'api_rerun'
					|| dispatch_rerun_run_ids[1] != require_integer_member(first_attempt, 'run_id')!
					|| dispatch_expected_attempts[1] != require_integer_member(first_attempt, 'run_attempt')! + 1 {
					issues << semantic_issue('$/v_smoke_execution/dispatches/1',
						'within 30 days the retry must reserve the exact prior Actions run and next attempt')
				}
			} else if dispatch_modes[1] != 'workflow_dispatch' || dispatch_rerun_run_ids[1] != 0
				|| dispatch_expected_attempts[1] != 1 {
				issues << semantic_issue('$/v_smoke_execution/dispatches/1',
					'after 30 days the retry must reserve one fresh workflow dispatch')
			}
		} else if dispatch_modes[1] != 'workflow_dispatch' || dispatch_rerun_run_ids[1] != 0
			|| dispatch_expected_attempts[1] != 1 {
			issues << semantic_issue('$/v_smoke_execution/dispatches/1',
				'a retry after no run was observed must reserve one fresh workflow dispatch')
		}
	}
	unresolved_dispatches := dispatches.len - logical_outcome_count
	state := require_string_member(smoke, 'state')!
	if active_dispatch != 0 {
		if state != 'pending' || active_dispatch != i64(dispatches.len)
			|| unresolved_dispatches != 1 || outcome_counts[int(active_dispatch - 1)] != 0 {
			issues << semantic_issue('$/v_smoke_execution/active_dispatch',
				'active dispatch must identify the sole latest pre-ACK reservation')
		}
	} else if state == 'pending' && unresolved_dispatches != 0 {
		issues << semantic_issue('$/v_smoke_execution/active_dispatch',
			'pending dispatch history cannot hide an unresolved pre-ACK reservation')
	} else if state == 'blocked' {
		if unresolved_dispatches < 0 || unresolved_dispatches > 1
			|| (unresolved_dispatches == 1 && outcome_counts[dispatches.len - 1] != 0) {
			issues << semantic_issue('$/v_smoke_execution/dispatches',
				'pre-ACK block may retain at most the one latest unmatched dispatch')
		}
	} else if unresolved_dispatches != 0 {
		issues << semantic_issue('$/v_smoke_execution/dispatches',
			'non-pending execution cannot retain an unmatched dispatch reservation')
	}
	if completion_ids.len != observed_completion_ids.len {
		issues << semantic_issue('$/v_smoke_execution/completion_operation_ids',
			'completion projection is not linked 1:1 to terminal attempts')
	} else {
		for index, completion_id in completion_ids {
			if require_string(completion_id)! != observed_completion_ids[index] {
				issues << semantic_issue('$/v_smoke_execution/completion_operation_ids/${index}',
					'completion operations must preserve attempt order')
			}
		}
	}
	issues << validate_v_smoke_state_semantics(smoke, attempts, run_absent_attempts,
		allow_source_retry_short_circuit)!
	block_id := require_nullable_string_member(smoke, 'block_operation_id')!
	if block_id != '' {
		block_count, block_generation, block_transition := operation_occurrences(root, block_id)!
		mut latest_event_generation := reservation_generation
		for event_generation in event_end_generations {
			if event_generation > latest_event_generation {
				latest_event_generation = event_generation
			}
		}
		mut shared_run_absent_index := i64(0)
		for run_absent in run_absent_attempts {
			if require_string_member(run_absent, 'run_absent_operation_id')! == block_id {
				shared_run_absent_index = require_integer_member(run_absent, 'attempt_index')!
			}
		}
		valid_transition := if shared_run_absent_index > 0 {
			block_transition == 'v-smoke-run-absent-${shared_run_absent_index}'
				&& block_generation == latest_event_generation
		} else {
			block_transition == 'v-smoke-block-pre-ack'
				&& block_generation > latest_event_generation
		}
		block_reason := require_nullable_string_member(smoke, 'block_reason')!
		if (block_reason == 'run_absent_exhausted') != (shared_run_absent_index == 2) {
			issues << semantic_issue('$/v_smoke_execution/block_reason',
				'run_absent_exhausted must atomically reuse the second run-absent CAS operation')
		}
		if block_count != 1 || block_generation > generation || !valid_transition {
			issues << semantic_issue('$/v_smoke_execution/block_operation_id',
				'pre-ACK block must be one unique, current and correctly typed later CAS operation')
		}
		if require_nullable_string_member(smoke, 'block_facts_digest')! != v_smoke_block_facts_digest(smoke)! {
			issues << semantic_issue('$/v_smoke_execution/block_facts_digest',
				'block facts digest does not cover its deterministic cause')
		}
		blocked_at := require_nullable_string_member(smoke, 'blocked_at')!
		mut latest_event_completed_at := execution_created_at
		for completed_at in event_completed_at {
			if completed_at > latest_event_completed_at {
				latest_event_completed_at = completed_at
			}
		}
		if blocked_at == '' || blocked_at < latest_event_completed_at {
			issues << semantic_issue('$/v_smoke_execution/blocked_at',
				'pre-ACK block cannot precede its execution or retained completed attempt')
		}
	}
	if require_string_member(smoke, 'replay_facts_digest')! != v_smoke_replay_facts_digest(smoke)! {
		issues << semantic_issue('$/v_smoke_execution/replay_facts_digest',
			'replay facts digest does not cover the complete execution projection')
	}
	return issues
}

fn validate_v_smoke_state_semantics(smoke JsonValue, attempts []JsonValue,
	run_absent_attempts []JsonValue, allow_source_retry_short_circuit bool) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	state := require_string_member(smoke, 'state')!
	active := require_nullable_integer_member(smoke, 'active_attempt')!
	if state == 'dispatched' {
		if attempts.len == 0
			|| active != require_integer_member(attempts[attempts.len - 1], 'attempt_index')!
			|| require_member(attempts[attempts.len - 1], 'completion_kind')!.kind != .null_value {
			issues << semantic_issue('$/v_smoke_execution/active_attempt',
				'active attempt must identify the one incomplete last attempt')
		}
	} else if active != 0 {
		issues << semantic_issue('$/v_smoke_execution/active_attempt',
			'non-dispatched execution cannot expose an active attempt')
	}
	first_outcome := v_smoke_logical_outcome(attempts, run_absent_attempts, 1)!
	second_outcome := v_smoke_logical_outcome(attempts, run_absent_attempts, 2)!
	retry_count := require_integer_member(smoke, 'infra_retry_count')!
	if retry_count == 1 && first_outcome != 'infrastructure' {
		issues << semantic_issue('$/v_smoke_execution/infra_retry_count',
			'retry requires a retained first logical infrastructure outcome')
	}
	if second_outcome != '' && first_outcome != 'infrastructure' {
		issues << semantic_issue('$/v_smoke_execution/infra_retry_count',
			'second logical outcome is permitted only after the one infrastructure retry')
	}
	if first_outcome == 'infrastructure' && second_outcome == '' {
		if !(allow_source_retry_short_circuit && retry_count == 0 && state == 'blocked')
			&& (retry_count != 1 || (state != 'pending' && (state != 'blocked'
			|| require_member(smoke, 'block_operation_id')!.kind == .null_value))) {
			issues << semantic_issue('$/v_smoke_execution/infra_retry_count',
				'first infrastructure attempt must reserve its one retry or retain the second pre-ACK block')
		}
	} else if first_outcome in ['green', 'functional'] && retry_count != 0 {
		issues << semantic_issue('$/v_smoke_execution/infra_retry_count',
			'green or functional first outcome cannot consume an infrastructure retry')
	}
	latest_outcome := if second_outcome != '' { second_outcome } else { first_outcome }
	if state in ['completed', 'blocked'] {
		if state == 'completed' && latest_outcome != 'green' {
			issues << semantic_issue('$/v_smoke_execution/state',
				'completed V smoke requires a timely selected run and validator check both green')
		}
		if state == 'blocked' && latest_outcome == 'green' {
			issues << semantic_issue('$/v_smoke_execution/state',
				'green terminal evidence cannot be projected as blocked')
		}
		if state == 'blocked' && require_member(smoke, 'block_operation_id')!.kind == .null_value
			&& latest_outcome !in ['functional', 'infrastructure'] {
			issues << semantic_issue('$/v_smoke_execution/state',
				'blocked terminal V smoke requires a functional or infrastructure outcome')
		}
	}
	return issues
}

fn v_smoke_logical_outcome(attempts []JsonValue, run_absent_attempts []JsonValue,
	attempt_index i64) !string {
	for run_absent in run_absent_attempts {
		if require_integer_member(run_absent, 'attempt_index')! == attempt_index {
			return 'infrastructure'
		}
	}
	for attempt in attempts {
		if require_integer_member(attempt, 'attempt_index')! == attempt_index {
			return v_smoke_attempt_outcome(attempt)!
		}
	}
	return ''
}

fn v_smoke_attempt_outcome(attempt JsonValue) !string {
	completion_kind := require_nullable_string_member(attempt, 'completion_kind')!
	if completion_kind == '' {
		return 'incomplete'
	}
	if completion_kind == 'deadline_exceeded' {
		return 'infrastructure'
	}
	completed_unix :=
		exact_timestamp_unix(require_nullable_string_member(attempt, 'completed_at')!)!
	deadline_unix := exact_timestamp_unix(require_string_member(attempt, 'deadline')!)!
	if completed_unix > deadline_unix {
		return 'infrastructure'
	}
	run_conclusion := require_nullable_string_member(attempt, 'run_conclusion')!
	if run_conclusion == 'failure' {
		return 'functional'
	}
	if run_conclusion in ['cancelled', 'timed_out', 'neutral', 'skipped'] {
		return 'infrastructure'
	}
	if require_member(attempt, 'check_run_id')!.kind == .null_value {
		return 'infrastructure'
	}
	check_conclusion := require_nullable_string_member(attempt, 'check_conclusion')!
	if check_conclusion in ['cancelled', 'timed_out', 'neutral', 'skipped'] {
		return 'infrastructure'
	}
	if run_conclusion == 'success' && check_conclusion == 'success' {
		return 'green'
	}
	if check_conclusion == 'failure' {
		return 'functional'
	}
	return 'infrastructure'
}

fn validate_gate_run_semantics(root JsonValue, subject_hash string,
	subject JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	intent := require_member(root, 'active_intent')!
	if intent.kind != .object {
		return issues
	}
	intent_consumer_id := require_string_member(intent, 'intent_id')!
	stage := require_string_member(intent, 'stage')!
	subject_consumer_kind := if subject.kind == .object {
		require_string_member(subject, 'consumer_kind')!
	} else {
		''
	}
	candidate_current_projection := subject.kind == .object
		&& subject_consumer_kind !in ['publish_post', 'rollback_post']
		&& require_string_member(subject, 'consumer_id')! == intent_consumer_id
	post_current_red_projection := blocked_post_native_validation_is_current(root, subject, stage)
	post_history_projection := subject.kind == .object
		&& subject_consumer_kind in ['publish_post', 'rollback_post']
		&& !post_current_red_projection
	current_projection := candidate_current_projection || post_current_red_projection
	consumer_id := if post_current_red_projection {
		require_string_member(subject, 'consumer_id')!
	} else {
		intent_consumer_id
	}
	intent_type := require_string_member(intent, 'intent_type')!
	mut gate_subject_sha := ''
	mut gate_subject_ref := require_string_member(intent, 'candidate_ref')!
	if post_current_red_projection {
		gate_subject_sha = require_string_member(subject, 'sha')!
		gate_subject_ref = require_string_member(subject, 'original_ref')!
	} else if intent_type in ['adopt-current', 'initial_adopt_current'] {
		validation_subject := require_member(intent, 'validation_subject')!
		if validation_subject.kind == .object {
			gate_subject_sha = require_string_member(validation_subject, 'sha')!
			gate_subject_ref = require_string_member(validation_subject, 'candidate_ref')!
		}
	} else {
		candidate_binding := require_member(intent, 'candidate_binding')!
		if candidate_binding.kind == .object {
			gate_subject_sha = require_string_member(candidate_binding, 'sha')!
		}
	}
	intent_inputs := require_object_member(intent, 'resolved_inputs')!
	gate_v_sha := require_string_member(intent_inputs, 'v_source_sha')!
	mut actions_integration_id := i64(0)
	for source in require_array_member(intent, 'expected_check_sources')! {
		if require_string_member(source, 'name')! == 'tccbin-candidate-gate' {
			actions_integration_id = require_integer_member(source, 'integration_id')!
		}
	}
	native_execution := require_member(root, 'native_gate_execution')!
	smoke_execution := require_member(root, 'v_smoke_execution')!
	mut native_gate_count := 0
	mut v_gate_count := 0
	mut historical_subject_hash := ''
	promotion_blocked := stage == 'blocked'
		&& require_string_member(root, 'publication_state')! == 'promotion_blocked'
	post_history_blocked := stage == 'blocked' && post_history_projection
	green_required :=
		stage in ['checks_green', 'promotion_unknown', 'post_checks_running', 'post_checks_waiting_source', 'completed']
		|| promotion_blocked || post_history_blocked
	for index, run in require_array_member(intent, 'gate_runs')! {
		path := '$/active_intent/gate_runs/${index}'
		name := require_string_member(run, 'check_name')!
		mut expected_integration_id := i64(0)
		for source in require_array_member(intent, 'expected_check_sources')! {
			if require_string_member(source, 'name')! == name {
				expected_integration_id = require_integer_member(source, 'integration_id')!
				if require_string_member(run, 'repository')! != require_string_member(source, 'repository')!
					|| require_integer_member(run, 'workflow_id')! != require_integer_member(source, 'workflow_id')!
					|| require_string_member(run, 'workflow_path')! != require_string_member(source, 'workflow_path')!
					|| require_string_member(run, 'event')! != require_string_member(source, 'event')! {
					issues << semantic_issue(path,
						'gate run does not match its allowlisted check source')
				}
			}
		}
		if expected_integration_id == 0
			|| require_integer_member(run, 'integration_id')! != expected_integration_id {
			issues << semantic_issue('${path}/integration_id',
				'gate check is not owned by the allowlisted Integration ID')
		}
		run_subject_hash := require_string_member(run, 'subject_hash')!
		if current_projection {
			if run_subject_hash != subject_hash {
				issues << semantic_issue('${path}/subject_hash',
					'current candidate gate run is orphaned from the active native subject')
				continue
			}
		} else if historical_subject_hash == '' {
			historical_subject_hash = run_subject_hash
		} else if run_subject_hash != historical_subject_hash {
			issues << semantic_issue('${path}/subject_hash',
				'historical candidate gates must retain one immutable subject hash')
		}
		is_native := name == 'tccbin-candidate-gate'
		if is_native {
			native_gate_count++
		} else {
			v_gate_count++
		}
		expected_name := if is_native {
			'tccbin-native-gate/${consumer_id}'
		} else {
			'tccbin-v-smoke/${consumer_id}'
		}
		if require_string_member(run, 'run_name')! != expected_name {
			issues << semantic_issue('${path}/run_name',
				'gate run name does not bind the active consumer')
		}
		run_id := require_integer_member(run, 'run_id')!
		job_id := require_integer_member(run, 'job_id')!
		repository := require_string_member(run, 'repository')!
		expected_run_url := 'https://github.com/${repository}/actions/runs/${run_id}'
		expected_job_url := '${expected_run_url}/job/${job_id}'
		if require_string_member(run, 'run_url')! != expected_run_url
			|| require_string_member(run, 'job_url')! != expected_job_url
			|| require_string_member(run, 'details_url')! != expected_job_url {
			issues << semantic_issue(path,
				'gate run URLs must be exact, sanitized projections of their IDs')
		}
		if require_string_member(run, 'completed_at')! < require_string_member(run, 'created_at')! {
			issues << semantic_issue(path, 'gate run completion cannot precede creation')
		}
		if green_required && (require_string_member(run, 'run_conclusion')! != 'success'
			|| require_string_member(run, 'check_conclusion')! != 'success') {
			issues << semantic_issue(path,
				'green or retained historical gates require both run and check conclusions success')
		}
		if gate_subject_sha == '' || require_string_member(run, 'sha')! != gate_subject_sha
			|| require_string_member(run, 'check_sha')! != gate_subject_sha {
			issues << semantic_issue(path,
				'candidate gate run and check SHA must bind the immutable intent subject')
		}
		if is_native {
			if native_execution.kind != .object
				|| require_string_member(run, 'ref')! != gate_subject_ref
				|| require_string_member(run, 'workflow_head_sha')! != gate_subject_sha
				|| require_integer_member(run, 'check_suite_integration_id')! != expected_integration_id
				|| require_string_member(run, 'actor')! != require_string_member(native_execution, 'original_actor')!
				|| require_integer_member(run, 'actor_integration_id')! != require_integer_member(native_execution, 'original_actor_integration_id')! {
				issues << semantic_issue(path,
					'native gate ref, workflow SHA, Actions App or original actor is not allowlisted')
			}
			if require_integer_member(run, 'run_attempt')! == 1 {
				if require_string_member(run, 'triggering_actor')! != require_string_member(native_execution, 'original_actor')!
					|| require_integer_member(run, 'triggering_actor_integration_id')! != require_integer_member(native_execution, 'original_actor_integration_id')! {
					issues << semantic_issue(path,
						'initial native gate triggering actor is not the original allowlisted App')
				}
			} else if
				require_string_member(run, 'triggering_actor')! != require_string_member(native_execution, 'rerun_triggering_actor')!
				|| require_integer_member(run, 'triggering_actor_integration_id')! != require_integer_member(native_execution, 'rerun_triggering_integration_id')! {
				issues << semantic_issue(path,
					'native rerun triggering actor is not the allowlisted gate dispatcher App')
			}
			if current_projection && native_execution.kind == .object {
				if require_nullable_integer_member(native_execution, 'selected_run_id')! != run_id
					|| require_nullable_integer_member(native_execution, 'selected_run_attempt')! != require_integer_member(run, 'run_attempt')!
					|| require_nullable_integer_member(native_execution, 'selected_check_suite_id')! != require_integer_member(run, 'check_suite_id')!
					|| require_nullable_string_member(native_execution, 'selected_conclusion')! != require_string_member(run, 'run_conclusion')! {
					issues << semantic_issue(path,
						'native gate check does not project the execution selected run')
				}
				active_epoch := require_integer_member(native_execution, 'active_gate_epoch')!
				epochs := require_array_member(native_execution, 'gate_epochs')!
				if active_epoch < 0 || active_epoch >= i64(epochs.len) {
					issues << semantic_issue(path, 'native gate active epoch is out of range')
				} else {
					epoch := epochs[int(active_epoch)]
					if require_string_member(epoch, 'state')! != 'completed'
						|| require_nullable_integer_member(epoch, 'selected_run_id')! != run_id
						|| require_nullable_integer_member(epoch, 'selected_run_attempt')! != require_integer_member(run, 'run_attempt')!
						|| require_nullable_integer_member(epoch, 'selected_check_suite_id')! != require_integer_member(run, 'check_suite_id')!
						|| require_nullable_string_member(epoch, 'conclusion')! != require_string_member(run, 'run_conclusion')! {
						issues << semantic_issue(path,
							'native gate check does not project the completed active epoch')
					}
				}
				mut observed_matches := 0
				for observed in require_array_member(native_execution, 'gate_runs')! {
					if native_gate_run_projection_matches(run, observed)! {
						observed_matches++
					}
				}
				if observed_matches != 1 {
					issues << semantic_issue(path,
						'native gate check must match exactly one immutable observed native run')
				}
			}
		} else if require_string_member(run, 'ref')! != 'master'
			|| require_string_member(run, 'workflow_head_sha')! != gate_v_sha
			|| require_integer_member(run, 'check_suite_integration_id')! != actions_integration_id
			|| require_string_member(run, 'actor')! != 'validator-dispatcher[bot]'
			|| require_string_member(run, 'triggering_actor')! != 'validator-dispatcher[bot]'
			|| require_integer_member(run, 'actor_integration_id')! != expected_integration_id
			|| require_integer_member(run, 'triggering_actor_integration_id')! != expected_integration_id {
			issues << semantic_issue(path,
				'V gate ref, workflow SHA, Actions App or validator dispatcher actor is not allowlisted')
		} else if current_projection {
			if smoke_execution.kind != .object {
				issues << semantic_issue(path,
					'current V gate check has no reserved V smoke execution')
				continue
			}
			mut attempt_matches := 0
			for attempt in require_array_member(smoke_execution, 'attempts')! {
				if require_member(attempt, 'check_run_id')!.kind != .null_value
					&& v_gate_run_projection_matches(run, attempt)! {
					attempt_matches++
				}
			}
			if attempt_matches != 1
				|| require_string_member(smoke_execution, 'state')! !in ['completed', 'blocked'] {
				issues << semantic_issue(path,
					'V gate check must match exactly one terminal immutable V smoke attempt')
			}
		}
		audience := if is_native {
			'vlang/tccbin:native-gate-check:v1'
		} else {
			'vlang/tccbin:v-smoke-check:v1'
		}
		expected_external_id := deterministic_check_external_id(audience, consumer_id,
			run_subject_hash, run_id, int(require_integer_member(run, 'run_attempt')!))!
		if require_string_member(run, 'external_id')! != expected_external_id {
			issues << semantic_issue('${path}/external_id',
				'gate check external ID is not the deterministic JCS identity')
		}
	}
	if native_gate_count > 1 || v_gate_count > 1 {
		issues << semantic_issue('$/active_intent/gate_runs',
			'each required gate kind may be projected at most once')
	}
	if green_required && (native_gate_count != 1 || v_gate_count != 1) {
		issues << semantic_issue('$/active_intent/gate_runs',
			'green or retained historical stages require exactly both gate kinds')
	}
	if post_history_projection
		&& stage in ['post_checks_running', 'post_checks_waiting_source', 'blocked']
		&& (native_gate_count != 1 || v_gate_count != 1) {
		issues << semantic_issue('$/active_intent/gate_runs',
			'post-validation must retain exactly the two historical candidate gate proofs')
	}
	if current_projection && native_execution.kind == .object {
		selected_conclusion := require_nullable_string_member(native_execution,
			'selected_conclusion')!
		if selected_conclusion != '' && selected_conclusion != 'pending' && native_gate_count != 1 {
			issues << semantic_issue('$/active_intent/gate_runs',
				'terminal native execution requires exactly one correlated native gate check')
		}
	}
	if current_projection && smoke_execution.kind == .object
		&& require_string_member(smoke_execution, 'state')! in ['completed', 'blocked'] {
		attempts := require_array_member(smoke_execution, 'attempts')!
		if attempts.len > 0
			&& require_member(attempts[attempts.len - 1], 'check_run_id')!.kind != .null_value
			&& v_gate_count != 1 {
			issues << semantic_issue('$/active_intent/gate_runs',
				'terminal V smoke check requires exactly one correlated V gate check')
		}
	}
	return issues
}

fn native_gate_run_projection_matches(gate_run JsonValue, observed JsonValue) !bool {
	for key in ['repository', 'ref', 'sha', 'event', 'actor', 'triggering_actor', 'workflow_path',
		'created_at'] {
		if require_string_member(gate_run, key)! != require_string_member(observed, key)! {
			return false
		}
	}
	for key in ['run_id', 'run_attempt', 'check_suite_id', 'actor_integration_id',
		'triggering_actor_integration_id', 'workflow_id'] {
		if require_integer_member(gate_run, key)! != require_integer_member(observed, key)! {
			return false
		}
	}
	return require_string_member(gate_run, 'run_conclusion')! == require_string_member(observed,
		'conclusion')!
}

fn v_gate_run_projection_matches(gate_run JsonValue, attempt JsonValue) !bool {
	string_bindings := [
		['repository', 'repository'],
		['workflow_path', 'workflow_path'],
		['event', 'event'],
		['run_name', 'run_name'],
		['run_url', 'run_url'],
		['job_url', 'job_url'],
		['details_url', 'details_url'],
		['ref', 'workflow_ref'],
		['workflow_head_sha', 'head_sha'],
		['sha', 'subject_sha'],
		['check_sha', 'check_sha'],
		['actor', 'actor'],
		['triggering_actor', 'triggering_actor'],
		['created_at', 'created_at'],
		['completed_at', 'completed_at'],
		['run_conclusion', 'run_conclusion'],
		['check_conclusion', 'check_conclusion'],
		['external_id', 'external_id'],
		['output_digest', 'output_digest'],
		['evidence_digest', 'evidence_digest'],
		['check_name', 'check_name'],
	]
	for binding in string_bindings {
		if require_string_member(gate_run, binding[0])! != require_string_member(attempt,
			binding[1])! {
			return false
		}
	}
	integer_bindings := [
		['workflow_id', 'workflow_id'],
		['run_id', 'run_id'],
		['run_attempt', 'run_attempt'],
		['check_suite_id', 'check_suite_id'],
		['check_suite_integration_id', 'check_suite_integration_id'],
		['job_id', 'job_id'],
		['check_run_id', 'check_run_id'],
		['integration_id', 'validator_integration_id'],
		['actor_integration_id', 'actor_integration_id'],
		['triggering_actor_integration_id', 'triggering_actor_integration_id'],
	]
	for binding in integer_bindings {
		if require_integer_member(gate_run, binding[0])! != require_integer_member(attempt,
			binding[1])! {
			return false
		}
	}
	return true
}

pub fn deterministic_check_external_id(audience string, consumer_id string, subject_hash string,
	run_id i64, run_attempt int) !string {
	identity := object_value_from_pairs(['schema_version', 'audience', 'consumer_id', 'subject_hash',
		'run_id', 'run_attempt'], [JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{ kind: .string_value, string_value: audience },
		JsonValue{ kind: .string_value, string_value: consumer_id },
		JsonValue{ kind: .string_value, string_value: subject_hash },
		JsonValue{ kind: .integer, int_value: run_id }, JsonValue{
			kind:      .integer
			int_value: i64(run_attempt)
		}])!
	return json_sha256(identity)
}

pub fn v_smoke_dispatch_facts_digest(smoke JsonValue, dispatch JsonValue) !string {
	mut facts := select_object_members(dispatch, ['attempt_index', 'mode', 'rerun_of_run_id',
		'expected_run_attempt', 'dispatch_operation_id', 'requested_at', 'discovery_deadline'])!
	facts = append_object_members(facts, ['schema_version', 'audience', 'consumer_id',
		'consumer_kind', 'target_id', 'subject_hash', 'subject_generation', 'subject_ref',
		'subject_sha', 'v_master_sha', 'repository', 'workflow_id', 'workflow_path', 'workflow_ref',
		'event', 'actions_integration_id', 'validator_integration_id', 'run_name'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-v-smoke-dispatch:v1'
		},
		require_member(smoke, 'consumer_id')!,
		require_member(smoke, 'consumer_kind')!,
		require_member(smoke, 'target_id')!,
		require_member(smoke, 'subject_hash')!,
		require_member(smoke, 'subject_generation')!,
		require_member(smoke, 'subject_ref')!,
		require_member(smoke, 'subject_sha')!,
		require_member(smoke, 'v_master_sha')!,
		require_member(smoke, 'repository')!,
		require_member(smoke, 'workflow_id')!,
		require_member(smoke, 'workflow_path')!,
		require_member(smoke, 'workflow_ref')!,
		require_member(smoke, 'event')!,
		require_member(smoke, 'actions_integration_id')!,
		require_member(smoke, 'validator_integration_id')!,
		require_member(smoke, 'run_name')!,
	])!
	return json_sha256(facts)
}

pub fn v_smoke_run_absent_facts_digest(smoke JsonValue, run_absent JsonValue) !string {
	mut facts := select_object_members(run_absent, ['attempt_index', 'dispatch_operation_id',
		'dispatch_facts_digest', 'run_absent_operation_id', 'outcome', 'completed_at'])!
	facts = append_object_members(facts,
		['schema_version', 'audience', 'consumer_id', 'subject_hash'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-v-smoke-run-absent:v1'
		},
		require_member(smoke, 'consumer_id')!,
		require_member(smoke, 'subject_hash')!,
	])!
	return json_sha256(facts)
}

pub fn v_smoke_ack_facts_digest(smoke JsonValue, attempt JsonValue) !string {
	mut facts := select_object_members(attempt, ['attempt_index', 'dispatch_operation_id',
		'dispatch_facts_digest', 'repository', 'workflow_id', 'workflow_path', 'workflow_ref',
		'event', 'run_id', 'run_attempt', 'check_suite_id', 'check_suite_integration_id', 'job_id',
		'run_name', 'run_url', 'job_url', 'head_sha', 'subject_ref', 'subject_sha', 'actor',
		'actor_integration_id', 'triggering_actor', 'triggering_actor_integration_id', 'created_at',
		'deadline', 'rerunnable_until', 'ack_operation_id'])!
	facts = append_object_members(facts,
		['schema_version', 'audience', 'consumer_id', 'subject_hash'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-v-smoke-ack:v1'
		},
		require_member(smoke, 'consumer_id')!,
		require_member(smoke, 'subject_hash')!,
	])!
	return json_sha256(facts)
}

pub fn v_smoke_completion_facts_digest(smoke JsonValue, attempt JsonValue) !string {
	mut facts := select_object_members(attempt, ['attempt_index', 'ack_facts_digest',
		'completion_kind', 'run_conclusion', 'completion_operation_id', 'check_run_id', 'check_name',
		'check_sha', 'details_url', 'external_id', 'validator_integration_id', 'check_conclusion',
		'output_digest', 'evidence_digest', 'completed_at'])!
	facts = append_object_members(facts,
		['schema_version', 'audience', 'consumer_id', 'subject_hash'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-v-smoke-completion:v1'
		},
		require_member(smoke, 'consumer_id')!,
		require_member(smoke, 'subject_hash')!,
	])!
	return json_sha256(facts)
}

// v_smoke_terminal_payload_digest is the v4 durable completion-CAS commitment. It hashes the
// complete historical V-smoke projection without exclusions; later target generations validate
// the suffix against the immutable terminal-revalidation copy, never a rewritten current copy.
pub fn v_smoke_terminal_payload_digest(smoke JsonValue, attempt JsonValue) !string {
	if require_member(attempt, 'completion_operation_id')!.kind != .string_value {
		return error('terminal payload commitment requires one completed selected attempt')
	}
	facts := object_value_from_pairs(['schema_version', 'audience', 'v_smoke_execution'], [
		JsonValue{
			kind:      .integer
			int_value: 4
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-v-smoke-terminal-payload:v4'
		},
		smoke,
	])!
	return json_sha256(facts)
}

pub fn v_smoke_block_facts_digest(smoke JsonValue) !string {
	mut facts := select_object_members(smoke, ['consumer_id', 'subject_hash', 'block_operation_id',
		'block_reason', 'blocked_at', 'dispatches'])!
	facts = append_object_members(facts, ['schema_version', 'audience'], [
		JsonValue{
			kind:      .integer
			int_value: 1
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-v-smoke-block:v1'
		},
	])!
	return json_sha256(facts)
}

pub fn v_smoke_replay_facts_digest(smoke JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in smoke.object_keys {
		if key != 'replay_facts_digest' {
			keys << key
			values << smoke.object_values[index]
		}
	}
	return json_sha256(object_value_from_pairs(keys, values)!)
}

pub fn terminal_revalidation_facts_digest(proof JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in proof.object_keys {
		if key != 'facts_digest' {
			keys << key
			values << proof.object_values[index]
		}
	}
	mut facts := object_value_from_pairs(keys, values)!
	facts = append_object_members(facts, ['audience'], [
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-terminal-revalidation:v5'
		},
	])!
	return json_sha256(facts)
}

// terminal_owner_payload_digest commits the durable post/remediation owner that existed when
// its reservation CAS created the native subject. Mutable execution evidence is excluded.
pub fn terminal_owner_payload_digest(projection JsonValue) !string {
	mut facts := select_object_members(projection, ['target_state', 'publication_state',
		'canonical_observed_sha', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash',
		'v_source_sha', 'resolved_inputs', 'last_known_good', 'provisional_published',
		'active_intent', 'post_validation_operation_id', 'native_gate_subject', 'native_subject_hash',
		'native_consumer_kind', 'active_remediation_id', 'active_remediation_binding',
		'active_remediation_operation_id', 'remediation_check_sources', 'owner_check_sources'])!
	facts = append_object_members(facts, ['schema_version', 'audience'], [
		JsonValue{
			kind:      .integer
			int_value: 1
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-terminal-owner-payload:v1'
		},
	])!
	return json_sha256(facts)
}

// source_state_operation_chain_digest commits one append-only SourceState v2 window entry.
// The resulting digest is excluded to avoid a recursive hash definition.
pub fn source_state_operation_chain_digest(entry JsonValue) !string {
	mut facts := select_object_members(entry, ['sequence', 'operation_id', 'transition',
		'previous_generation', 'resulting_generation', 'previous_state_digest',
		'resulting_state_digest', 'evidence_path', 'evidence_digest', 'previous_chain_digest'])!
	facts = append_object_members(facts, ['schema_version', 'audience'], [
		JsonValue{
			kind:      .integer
			int_value: 2
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-source-state-operation-chain:v2'
		},
	])!
	return json_sha256(facts)
}

fn validate_source_state_schema_semantics(source_state JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	if require_integer_member(source_state, 'schema_version')! != 2 {
		return [
			semantic_issue('$/schema_version',
				'SourceState durable operation window requires schema version 2'),
		]
	}
	operation_count := require_integer_member(source_state, 'operation_count')!
	_ := exact_timestamp_unix(require_string_member(source_state, 'last_attempt_at')!) or {
		issues << semantic_issue('$/last_attempt_at',
			'SourceState attempt time must be one exact calendar-valid UTC RFC3339 second')
		i64(0)
	}
	chain_digest := require_string_member(source_state, 'operation_chain_digest')!
	window := require_object_member(source_state, 'operation_window')!
	start_count := require_integer_member(window, 'start_count')!
	anchor_digest := require_string_member(window, 'anchor_digest')!
	entries := require_array_member(window, 'entries')!
	expected_window_length := if operation_count < 128 { int(operation_count) } else { 128 }
	expected_start_count := operation_count - i64(expected_window_length)
	if entries.len != expected_window_length || start_count != expected_start_count {
		issues << semantic_issue('$/operation_window',
			'SourceState must retain exactly min(operation_count, 128) tail entries with the exact start count')
	}
	mut previous_chain := anchor_digest
	mut previous_sequence := start_count
	mut previous_generation := i64(-1)
	mut previous_state_digest := ''
	mut operation_ids := []string{}
	for index, entry in entries {
		sequence := require_integer_member(entry, 'sequence')!
		operation_id := require_string_member(entry, 'operation_id')!
		entry_previous_chain := require_string_member(entry, 'previous_chain_digest')!
		entry_resulting_chain := require_string_member(entry, 'resulting_chain_digest')!
		entry_previous_generation := require_integer_member(entry, 'previous_generation')!
		entry_resulting_generation := require_integer_member(entry, 'resulting_generation')!
		entry_previous_state := require_string_member(entry, 'previous_state_digest')!
		entry_resulting_state := require_string_member(entry, 'resulting_state_digest')!
		if sequence != previous_sequence + 1
			|| entry_previous_chain != previous_chain
			|| entry_resulting_generation != entry_previous_generation + 1
			|| (index > 0 && (entry_previous_generation != previous_generation
			|| entry_previous_state != previous_state_digest))
			|| entry_resulting_chain != source_state_operation_chain_digest(entry)!
			|| operation_id in operation_ids {
			issues << semantic_issue('$/operation_window/entries/${index}',
				'SourceState retained operations must form one contiguous unique generation/state/hash chain')
		}
		operation_ids << operation_id
		previous_sequence = sequence
		previous_generation = entry_resulting_generation
		previous_state_digest = entry_resulting_state
		previous_chain = entry_resulting_chain
	}
	if previous_sequence != operation_count || previous_chain != chain_digest {
		issues << semantic_issue('$/operation_chain_digest',
			'SourceState chain root must equal the exact retained window tail')
	}
	if entries.len > 0
		&& (require_integer_member(entries[entries.len - 1], 'resulting_generation')! != require_integer_member(source_state, 'generation')!
		|| require_string_member(entries[entries.len - 1], 'resulting_state_digest')! != source_state_snapshot_digest(source_state)!) {
		issues << semantic_issue('$/generation',
			'SourceState generation and snapshot digest must equal the retained operation window tail')
	}
	return issues
}

fn source_state_operation_window_contains(source_state JsonValue, operation_id string) !bool {
	window := require_object_member(source_state, 'operation_window')!
	for entry in require_array_member(window, 'entries')! {
		if require_string_member(entry, 'operation_id')! == operation_id {
			return true
		}
	}
	return false
}

fn source_state_transition_matches_window_entry(transition JsonValue, entry JsonValue) !bool {
	return
		require_integer_member(entry, 'sequence')! == require_integer_member(transition, 'sequence')!
		&& require_string_member(entry, 'operation_id')! == require_string_member(transition, 'operation_id')!
		&& require_string_member(entry, 'transition')! == require_string_member(transition, 'transition')!
		&& require_integer_member(entry, 'previous_generation')! == require_integer_member(transition, 'previous_generation')!
		&& require_integer_member(entry, 'resulting_generation')! == require_integer_member(transition, 'resulting_generation')!
		&& require_string_member(entry, 'previous_state_digest')! == require_string_member(transition, 'previous_state_digest')!
		&& require_string_member(entry, 'resulting_state_digest')! == require_string_member(transition, 'resulting_state_digest')!
		&& require_string_member(entry, 'evidence_path')! == require_string_member(transition, 'evidence_path')!
		&& require_string_member(entry, 'evidence_digest')! == require_string_member(transition, 'universal_evidence_digest')!
		&& require_string_member(entry, 'previous_chain_digest')! == require_string_member(transition, 'previous_chain_digest')!
		&& require_string_member(entry, 'resulting_chain_digest')! == require_string_member(transition, 'resulting_chain_digest')!
}

fn source_state_append_is_exact(pre JsonValue, post JsonValue, transition JsonValue) !bool {
	if validate_source_state_schema_semantics(pre)!.len > 0
		|| validate_source_state_schema_semantics(post)!.len > 0 {
		return false
	}
	pre_count := require_integer_member(pre, 'operation_count')!
	post_count := require_integer_member(post, 'operation_count')!
	pre_window := require_object_member(pre, 'operation_window')!
	post_window := require_object_member(post, 'operation_window')!
	pre_entries := require_array_member(pre_window, 'entries')!
	post_entries := require_array_member(post_window, 'entries')!
	drop_count := if pre_entries.len == 128 { 1 } else { 0 }
	if post_count != pre_count + 1
		|| require_integer_member(post_window, 'start_count')! != require_integer_member(pre_window, 'start_count')! + drop_count
		|| post_entries.len != pre_entries.len + 1 - drop_count || post_entries.len == 0 {
		return false
	}
	expected_anchor := if drop_count == 0 {
		require_string_member(pre_window, 'anchor_digest')!
	} else {
		require_string_member(pre_entries[0], 'resulting_chain_digest')!
	}
	if require_string_member(post_window, 'anchor_digest')! != expected_anchor {
		return false
	}
	for index in drop_count .. pre_entries.len {
		if !json_equal(pre_entries[index], post_entries[index - drop_count]) {
			return false
		}
	}
	entry := post_entries[post_entries.len - 1]
	return
		require_integer_member(post, 'generation')! == require_integer_member(pre, 'generation')! + 1
		&& require_integer_member(entry, 'sequence')! == post_count
		&& require_integer_member(entry, 'previous_generation')! == require_integer_member(pre, 'generation')!
		&& require_integer_member(entry, 'resulting_generation')! == require_integer_member(post, 'generation')!
		&& require_string_member(entry, 'previous_state_digest')! == source_state_snapshot_digest(pre)!
		&& require_string_member(entry, 'resulting_state_digest')! == source_state_snapshot_digest(post)!
		&& require_string_member(entry, 'previous_chain_digest')! == require_string_member(pre, 'operation_chain_digest')!
		&& require_string_member(entry, 'resulting_chain_digest')! == require_string_member(post, 'operation_chain_digest')!
		&& source_state_transition_matches_window_entry(transition, entry)!
}

// source_state_append_contract_is_exact validates the durable SourceState v2 window append and
// exact prefix truncation independently from the future state-machine implementation.
pub fn source_state_append_contract_is_exact(pre JsonValue, post JsonValue,
	transition JsonValue) !bool {
	return source_state_append_is_exact(pre, post, transition)
}

// source_state_snapshot_digest gives the non-recursive durable SourceState payload a JCS identity.
pub fn source_state_snapshot_digest(source_state JsonValue) !string {
	payload := select_object_members(source_state, ['schema_version', 'generation', 'source_id',
		'canonical_url', 'ref', 'status', 'resolved_sha', 'source_fingerprint', 'last_attempt_at',
		'mode', 'originating_run_id', 'waiting_consumers'])!
	facts := object_value_from_pairs(['schema_version', 'audience', 'source_state_snapshot'], [
		JsonValue{
			kind:      .integer
			int_value: 2
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-source-state-snapshot:v2'
		},
		payload,
	])!
	return json_sha256(facts)
}

// source_state_transition_evidence_digest makes every source-scoped CAS record tamper evident.
pub fn source_state_transition_evidence_digest(transition JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in transition.object_keys {
		if key != 'evidence_digest' {
			keys << key
			values << transition.object_values[index]
		}
	}
	mut facts := object_value_from_pairs(keys, values)!
	facts = append_object_members(facts, ['audience'], [
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-source-state-transition:v2'
		},
	])!
	return json_sha256(facts)
}

// source_state_subject_fingerprint follows the audit's source identity rule: the URL/ref and the
// exact status/SHA observed before the CAS are inseparable operation identity material.
pub fn source_state_subject_fingerprint(source_state JsonValue) !string {
	facts := object_value_from_pairs(['schema_version', 'audience', 'canonical_url', 'ref', 'status',
		'resolved_sha'], [
		JsonValue{
			kind:      .integer
			int_value: 1
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-source-state-subject:v1'
		},
		require_member(source_state, 'canonical_url')!,
		require_member(source_state, 'ref')!,
		require_member(source_state, 'status')!,
		require_member(source_state, 'resolved_sha')!,
	])!
	return json_sha256(facts)
}

// source_state_universal_evidence_digest commits the schema-valid, injective universal evidence
// record which is written beside the SourceState row in the same authoritative state-ref commit.
pub fn source_state_universal_evidence_digest(evidence JsonValue) !string {
	facts := object_value_from_pairs(['schema_version', 'audience', 'evidence'], [
		JsonValue{
			kind:      .integer
			int_value: 1
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-source-state-universal-evidence:v1'
		},
		evidence,
	])!
	return json_sha256(facts)
}

// source_refetch_evidence_digest binds the complete target refetch to an independently
// digested source-scoped pre/post CAS history that authorized source_waiting.
pub fn source_refetch_evidence_digest(refetch JsonValue, source_state_pre JsonValue,
	source_state_post JsonValue, source_state_cas_history JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in refetch.object_keys {
		if key != 'evidence_digest' {
			keys << key
			values << refetch.object_values[index]
		}
	}
	mut facts := object_value_from_pairs(keys, values)!
	facts = append_object_members(facts, ['schema_version', 'audience', 'source_state_pre_snapshot',
		'source_state_snapshot', 'source_state_cas_history'], [
		JsonValue{
			kind:      .integer
			int_value: 2
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-source-refetch-evidence:v2'
		},
		source_state_pre,
		source_state_post,
		source_state_cas_history,
	])!
	return json_sha256(facts)
}

// git_ancestry_evidence_digest binds the exact repository/ref/subject/HEAD/merge-base query.
pub fn git_ancestry_evidence_digest(proof JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in proof.object_keys {
		if key != 'evidence_digest' {
			keys << key
			values << proof.object_values[index]
		}
	}
	mut facts := object_value_from_pairs(keys, values)!
	facts = append_object_members(facts, ['audience'], [
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-git-ancestry-evidence:v1'
		},
	])!
	return json_sha256(facts)
}

// native_gate_evidence_digest freezes the native authority and observed runs while allowing the
// target CAS generation projection to advance as H2 is dispatched and completed.
pub fn native_gate_evidence_digest(execution JsonValue) !string {
	mut keys := []string{}
	mut values := []JsonValue{}
	for index, key in execution.object_keys {
		if key != 'expected_ledger_generation' {
			keys << key
			values << execution.object_values[index]
		}
	}
	mut facts := object_value_from_pairs(keys, values)!
	facts = append_object_members(facts, ['audience'], [
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/tccbin:native-gate-evidence:v1'
		},
	])!
	return json_sha256(facts)
}

// native_gate_check_digest commits the complete selected native check, including its owning
// Integration, workflow, run, check-run, conclusions and sanitized evidence URLs.
pub fn native_gate_check_digest(check JsonValue) !string {
	mut facts := select_object_members(check, check.object_keys)!
	facts = append_object_members(facts, ['schema_version', 'audience'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/tccbin:native-gate-check-evidence:v1'
		},
	])!
	return json_sha256(facts)
}

// recovery_handoff_creation_commitment binds the immutable H1 identity and its pre-existing
// check authority into the append-only creation transition without changing retry identity.
pub fn recovery_handoff_creation_commitment(handoff JsonValue) !string {
	mut facts := select_object_members(handoff, ['handoff_id', 'handoff_ordinal', 'audience',
		'recovery_operation_id', 'consumer_type', 'resume_capability', 'intent_or_operation_id',
		'subject_hash', 'subject_generation', 'expected_canonical_head', 'subject_ref_head',
		'expected_check_sources'])!
	facts = append_object_members(facts, ['schema_version', 'commitment_audience'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-recovery-h1-create:v1'
		},
	])!
	return json_sha256(facts)
}

// recovery_native_successor_commitment binds the exact H1->H2 link and both complete native
// evidence commitments into the single atomic successor ledger transition.
pub fn recovery_native_successor_commitment(predecessor JsonValue) !string {
	mut facts := select_object_members(predecessor, ['handoff_id', 'successor_handoff_id', 'audience',
		'recovery_operation_id', 'intent_or_operation_id', 'subject_hash', 'subject_generation',
		'expected_check_sources', 'native_gate_evidence_digest', 'native_gate_check_digest'])!
	facts = append_object_members(facts, ['schema_version', 'commitment_audience'], [
		JsonValue{ kind: .integer, int_value: 1 },
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-recovery-h1-successor:v1'
		},
	])!
	return json_sha256(facts)
}

fn recovery_handoff_smoke_operation_matches(root JsonValue, smoke JsonValue,
	operation_id string, transition string, operation_kind string) !bool {
	for handoff in require_array_member(root, 'recovery_handoffs')! {
		if require_string_member(handoff, 'resume_capability')! !in ['v_smoke', 'evidence_only']
			|| require_string_member(handoff, 'intent_or_operation_id')! != require_string_member(smoke, 'consumer_id')!
			|| require_string_member(handoff, 'subject_hash')! != require_string_member(smoke, 'subject_hash')! {
			continue
		}
		handoff_id := require_string_member(handoff, 'handoff_id')!
		if operation_kind == 'ack' {
			if transition != 'handoff_ack_${handoff_id}' {
				continue
			}
			for ack_value in require_array_member(smoke, 'ack_operation_ids')! {
				if require_string(ack_value)! == operation_id {
					return true
				}
			}
			continue
		}
		if transition != 'handoff_dispatch_${handoff_id}' {
			continue
		}
		for dispatch_value in require_array_member(handoff, 'dispatch_operation_ids')! {
			if require_string(dispatch_value)! == operation_id {
				return true
			}
		}
	}
	return false
}

fn operation_occurrences(root JsonValue, operation_id string) !(int, i64, string) {
	if operation_id == '' {
		return 0, i64(-1), ''
	}
	mut count := 0
	mut generation := i64(-1)
	mut transition := ''
	for operation in require_array_member(root, 'applied_operations')! {
		if require_string_member(operation, 'operation_id')! == operation_id {
			count++
			generation = require_integer_member(operation, 'resulting_generation')!
			transition = require_string_member(operation, 'transition')!
		}
	}
	return count, generation, transition
}

fn transition_occurrences(root JsonValue, expected_transition string) !(int, i64, string) {
	mut count := 0
	mut generation := i64(-1)
	mut operation_id := ''
	for operation in require_array_member(root, 'applied_operations')! {
		if require_string_member(operation, 'transition')! == expected_transition {
			count++
			generation = require_integer_member(operation, 'resulting_generation')!
			operation_id = require_string_member(operation, 'operation_id')!
		}
	}
	return count, generation, operation_id
}

fn exact_timestamp_unix(value string) !i64 {
	if value.len != 20 || value[4] != `-` || value[7] != `-` || value[10] != `T` || value[13] != `:`
		|| value[16] != `:` || value[19] != `Z` {
		return error('timestamp is not canonical UTC RFC3339 seconds')
	}
	year := timestamp_decimal_component(value, 0, 4)!
	month := timestamp_decimal_component(value, 5, 2)!
	day := timestamp_decimal_component(value, 8, 2)!
	hour := timestamp_decimal_component(value, 11, 2)!
	minute := timestamp_decimal_component(value, 14, 2)!
	second := timestamp_decimal_component(value, 17, 2)!
	if year < 1970 || year > 9999 || month < 1 || month > 12 || hour > 23 || minute > 59
		|| second > 59 || day < 1 || day > utc_days_in_month(year, month)! {
		return error('timestamp contains an invalid UTC calendar instant')
	}
	mut days := i64(0)
	for current_year in 1970 .. year {
		days += if utc_is_leap_year(current_year) { 366 } else { 365 }
	}
	for current_month in 1 .. month {
		days += utc_days_in_month(year, current_month)!
	}
	days += day - 1
	return days * 86_400 + i64(hour * 3_600 + minute * 60 + second)
}

fn timestamp_decimal_component(value string, start int, length int) !int {
	mut result := 0
	for index in start .. start + length {
		character := value[index]
		if character < `0` || character > `9` {
			return error('timestamp component is not decimal')
		}
		result = result * 10 + int(character - `0`)
	}
	return result
}

fn utc_is_leap_year(year int) bool {
	return (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn utc_days_in_month(year int, month int) !int {
	return match month {
		1, 3, 5, 7, 8, 10, 12 {
			31
		}
		4, 6, 9, 11 {
			30
		}
		2 {
			if utc_is_leap_year(year) {
				29
			} else {
				28
			}
		}
		else {
			error('timestamp month is outside 1..12')
		}
	}
}

fn require_nullable_integer_member(value JsonValue, key string) !i64 {
	member := require_member(value, key)!
	if member.kind == .null_value {
		return 0
	}
	if member.kind != .integer {
		return error('${key} must be an integer or null')
	}
	return member.int_value
}

fn semantic_issue(path string, message string) SchemaIssue {
	return SchemaIssue{
		path:    path
		message: message
	}
}
