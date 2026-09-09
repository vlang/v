module bin

import os

const schema_file_names = [
	'active-intent.schema.json',
	'artifact-projection.schema.json',
	'bundle-manifest.schema.json',
	'common.schema.json',
	'diagnostic.schema.json',
	'evidence.schema.json',
	'input-projection.schema.json',
	'inventory-entry.schema.json',
	'issue-projection.schema.json',
	'lane-result.schema.json',
	'native-gate-execution.schema.json',
	'native-gate-subject.schema.json',
	'native-lane-matrix.schema.json',
	'onboarding-policy.schema.json',
	'patch-effect.schema.json',
	'recovery-handoff.schema.json',
	'source-state.schema.json',
	'target-state.schema.json',
	'targets.schema.json',
	'toolchain-observation.schema.json',
	'toolchain-profile.schema.json',
	'transition-envelope.schema.json',
]

// ContractReport summarizes bounded Phase A contract validation.
pub struct ContractReport {
pub:
	schema_count   int
	manifest_count int
	hygiene_files  int
}

// StagedManifestEligibility is the fail-closed production handoff from real staged bytes to the
// build/publish controller. Missing Phase A material is a no-op only when publication is false.
pub struct StagedManifestEligibility {
pub:
	eligible             bool
	publish_allowed      bool
	reason               string
	manifest_hash        string
	input_fingerprint    string
	artifact_fingerprint string
}

// RuntimeContractBinding identifies the immutable V contract embedded in the validator binary.
pub struct RuntimeContractBinding {
pub:
	repository string
	sha        string
}

// evaluate_staged_manifest_for_execution is the sole production eligibility adapter. It always
// invokes the staged manifest authenticator; an opaque declaration can therefore never be made
// eligible from manifest text alone.
pub fn evaluate_staged_manifest_for_execution(automation_root string, manifest_path string,
	staging StagingContract, runtime RuntimeContractBinding,
	publish_requested bool) !StagedManifestEligibility {
	validate_runtime_contract_binding(runtime)!
	if !os.is_file(manifest_path) || !os.is_dir(staging.staging_root)
		|| !os.is_dir(staging.source_git_root) {
		if publish_requested {
			return error('publication requires the complete staged Phase A contract')
		}
		return StagedManifestEligibility{
			reason: 'phase_a_material_absent'
		}
	}
	manifest := authenticate_staged_manifest_file(automation_root, manifest_path, staging) or {
		if err.msg() == staged_provenance_incomplete_error {
			if publish_requested {
				return error('publication refused because staged provenance is incomplete')
			}
			return StagedManifestEligibility{
				reason: 'staged_provenance_incomplete'
			}
		}
		if publish_requested {
			return error('publication refused because staged provenance is not authentic: ${err.msg()}')
		}
		return StagedManifestEligibility{
			reason: 'staged_provenance_ineligible'
		}
	}
	manifest_repository := require_string_member(manifest.manifest, 'contract_repository')!
	manifest_sha := require_string_member(manifest.manifest, 'contract_sha')!
	manifest_mode := require_string_member(manifest.manifest, 'contract_mode')!
	if runtime.repository != manifest_repository || runtime.sha != manifest_sha {
		return error('runtime contract binding differs from the authenticated manifest')
	}
	if publish_requested && (manifest_mode != 'production' || runtime.repository != 'vlang/v') {
		return error('publication requires an authenticated production vlang/v contract')
	}
	fingerprints := authenticated_manifest_fingerprints(manifest)!
	return StagedManifestEligibility{
		eligible:             true
		publish_allowed:      publish_requested
		reason:               'authenticated_staging'
		manifest_hash:        fingerprints.manifest_hash
		input_fingerprint:    fingerprints.input_fingerprint
		artifact_fingerprint: fingerprints.artifact_fingerprint
	}
}

fn validate_runtime_contract_binding(runtime RuntimeContractBinding) ! {
	if runtime.repository !in ['GGRei/v', 'vlang/v'] {
		return error('runtime contract repository is not allowlisted')
	}
	if !is_lower_hex_40(runtime.sha) {
		return error('runtime contract SHA must be a full lowercase commit SHA')
	}
}

// attest_runtime_contract_binding is the executable bootstrap oracle for the two immutable
// compile-time values embedded in the validator.
pub fn attest_runtime_contract_binding(runtime RuntimeContractBinding) !RuntimeContractBinding {
	validate_runtime_contract_binding(runtime)!
	return runtime
}

// run_contract_checks validates every authoritative Phase A contract artifact.
pub fn run_contract_checks(automation_root string) !ContractReport {
	validate_vc_bootstrap_contract(automation_root)!
	schema_dir := os.join_path(automation_root, 'schemas')
	mut actual_schema_names := os.walk_ext(schema_dir, '.schema.json').map(os.file_name(it))
	actual_schema_names.sort()
	if actual_schema_names != schema_file_names {
		return error('authoritative schema set must contain exactly 22 named files')
	}
	for schema_name in schema_file_names {
		path := os.join_path(schema_dir, schema_name)
		schema := parse_strict_json(os.read_file(path)!)!
		if require_string_member(schema, '$schema')! != 'https://json-schema.org/draft/2020-12/schema' {
			return error('schema ${schema_name} does not declare draft 2020-12')
		}
		validate_schema_profile(schema, schema_name)!
	}
	registry_issues := validate_registry(automation_root)!
	if registry_issues.len > 0 {
		return error('registry failed with ${registry_issues.len} issue(s)')
	}
	fixture_dir := os.join_path(automation_root, 'tests', 'fixtures')
	manifest_paths := os.walk_ext(fixture_dir, '.valid.json')
	if manifest_paths.len != 2 {
		return error('contract suite requires the two dormant manifest fixtures')
	}
	for manifest_path in manifest_paths {
		issues := validate_manifest(automation_root, manifest_path)!
		if issues.len > 0 {
			return error('positive manifest ${os.file_name(manifest_path)} failed with ${issues.len} issue(s)')
		}
	}
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	complete_path := os.join_path(fixture_dir, 'manifest-complete.valid.json')
	complete := parse_strict_json(os.read_file(complete_path)!)!
	if recalculate_provenance(complete, registry, [])! != 'incomplete' {
		return error('dormant Linux fixture did not remain incomplete without a producer profile')
	}
	windows_path := os.join_path(fixture_dir, 'manifest-windows-opaque.valid.json')
	windows := parse_strict_json(os.read_file(windows_path)!)!
	if classify_declared_provenance(windows, registry)! != 'incomplete' {
		return error('dormant Windows fixture did not remain incomplete without a producer profile')
	}
	for manifest_path in manifest_paths {
		manifest_source := os.read_file(manifest_path)!
		projections := manifest_projections(manifest_source, registry)!
		input_issues := validate_json_value(os.join_path(schema_dir, 'input-projection.schema.json'),
			projections.input)!
		artifact_issues := validate_json_value(os.join_path(schema_dir,
			'artifact-projection.schema.json'), projections.artifact)!
		if input_issues.len > 0 || artifact_issues.len > 0 {
			return error('manifest projections failed their authoritative schemas')
		}
		fingerprints := manifest_fingerprints(manifest_source, registry)!
		if fingerprints.manifest_hash.len != 64 || fingerprints.input_fingerprint.len != 64
			|| fingerprints.artifact_fingerprint.len != 64 {
			return error('fingerprint output is incomplete')
		}
	}
	durable_schema_fixtures := {
		'active-intent.schema.json':         'active-intent.bootstrap.schema-fixture.json'
		'issue-projection.schema.json':      'issue-projection.schema-fixture.json'
		'native-gate-subject.schema.json':   'native-gate-subject.schema-fixture.json'
		'native-gate-execution.schema.json': 'native-gate-execution.schema-fixture.json'
		'recovery-handoff.schema.json':      'recovery-handoff.pending.schema-fixture.json'
		'source-state.schema.json':          'source-state.outage.schema-fixture.json'
		'target-state.schema.json':          'target-state.bootstrap.schema-fixture.json'
	}
	for schema_name, fixture_name in durable_schema_fixtures {
		issues := validate_json_file(os.join_path(schema_dir, schema_name), os.join_path(fixture_dir,
			fixture_name))!
		if issues.len > 0 {
			return error('durable fixture ${fixture_name} failed with ${issues.len} issue(s)')
		}
	}
	issue_fixture := parse_strict_json(os.read_file(os.join_path(fixture_dir,
		'issue-projection.schema-fixture.json'))!)!
	issue_owner := require_string_member(issue_fixture, 'owner_repository')!
	issue_os := require_string_member(issue_fixture, 'os')!
	if require_string_member(issue_fixture, 'marker_hash')! != issue_marker_hash(issue_owner,
		issue_os) {
		return error('issue projection fixture marker is not derived from exactly owner+OS')
	}
	receiver_ledger_source := os.read_file(os.join_path(fixture_dir, 'receiver-ledger.dark.json'))!
	parse_receiver_state_ledger(receiver_ledger_source)!
	resolve_receiver_completion(receiver_ledger_source, os.read_file(os.join_path(fixture_dir,
		'receiver-workflow-run.dark.json'))!)!
	issue_projection := project_issue_ledger(os.read_file(os.join_path(fixture_dir,
		'issue-ledger.dark.json'))!)!
	if issue_projection.owner_repository != 'vlang/tccbin' || issue_projection.os != 'windows'
		|| issue_projection.entries.len != 1 || !issue_projection.should_be_open {
		return error('persisted issue ledger fixture did not derive its exact owner+OS projection')
	}
	validate_strict_json_profile()!
	validate_security_contract()!
	mut public_files := os.walk_ext(automation_root, '')
	current_executable := os.real_path(os.executable())
	public_files = public_files.filter(os.is_file(it)
		&& (os.is_link(it) || os.real_path(it) != current_executable))
	vroot := os.real_path(os.join_path(automation_root, '..', '..'))
	for relative_path in [
		'.gitignore',
		'.github/workflows/update_tccbin.yml',
		'.github/workflows/tccbin_automation_contract.yml',
		'.github/workflows/tccbin_issue_reconcile.yml',
		'.github/workflows/tccbin_revalidate.yml',
		'.github/workflows/tccbin_source_recovery.yml',
		'doc/tccbin_automation.md',
	] {
		public_files << os.join_path(vroot, relative_path)
	}
	findings := public_hygiene_findings(public_files)!
	if findings.len > 0 {
		return error('public hygiene rejected ${findings.len} file(s)')
	}
	return ContractReport{
		schema_count:   actual_schema_names.len
		manifest_count: manifest_paths.len
		hygiene_files:  public_files.len
	}
}

// run_dark_mode_dry_run exercises safety transitions without credentials or external writes.
pub fn run_dark_mode_dry_run(automation_root string) ! {
	run_contract_checks(automation_root)!
	sha := 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	tree := 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
	source_sha := 'cccccccccccccccccccccccccccccccccccccccc'
	source_tree := 'dddddddddddddddddddddddddddddddddddddddd'
	resolved_inputs := ResolvedInputsModel{
		sources:             [
			ResolvedSourceModel{
				id:         'tinycc'
				repository: 'https://repo.or.cz/tinycc.git'
				ref:        'mob'
				sha:        source_sha
				tree:       source_tree
			},
		]
		source_checks:       [
			SourceCheckModel{
				source_id:       'tinycc'
				resolved_sha:    source_sha
				status:          'resolved'
				evidence_digest: '1111111111111111111111111111111111111111111111111111111111111111'
			},
		]
		recipe_path:         'thirdparty/build_scripts/thirdparty-linux-amd64_tcc.sh'
		recipe_hash:         '2222222222222222222222222222222222222222222222222222222222222222'
		contract_repository: 'vlang/v'
		contract_sha:        sha
		v_source_sha:        sha
		producer_toolchain:  ProducerToolchainModel{
			profile_id:         'linux-amd64-synthetic-v1'
			profile_sha256:     '3333333333333333333333333333333333333333333333333333333333333333'
			observation_sha256: '4444444444444444444444444444444444444444444444444444444444444444'
			observation_digest: '5555555555555555555555555555555555555555555555555555555555555555'
		}
	}
	mut target := initial_target_model('linux-amd64', sha)
	// This state-machine-only dry run never presents the dormant manifest fixtures as resolved.
	// Exact manifest binding is exercised only by authenticated synthetic test authorities.
	target.input_fingerprint = '6666666666666666666666666666666666666666666666666666666666666666'
	target.artifact_fingerprint = '7777777777777777777777777777777777777777777777777777777777777777'
	target.manifest_hash = '8888888888888888888888888888888888888888888888888888888888888888'
	target.provenance_status = 'complete'
	target.resolved_inputs = resolved_inputs
	intent_id := deterministic_intent_id('vlang/v:tccbin-automation-state', target.target_id,
		'initial_adopt_current', 100, 1, 0, target.input_fingerprint, sha)!
	candidate_ref := 'tccbin-candidate/${target.target_id}/${intent_id}'
	subject := ValidationSubjectModel{
		sha:                  sha
		tree:                 tree
		input_fingerprint:    target.input_fingerprint
		artifact_fingerprint: target.artifact_fingerprint
		manifest_hash:        target.manifest_hash
		digests:              [
			DigestModel{
				path:   'tcc.exe'
				sha256: 'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
			},
		]
		candidate_ref:        candidate_ref
	}
	intent := ActiveIntentModel{
		intent_id:               intent_id
		intent_type:             'initial_adopt_current'
		stage:                   'intent_reserved'
		run_id:                  100
		run_attempt:             1
		input_fingerprint:       target.input_fingerprint
		expected_canonical_head: sha
		candidate_ref:           candidate_ref
		generation:              target.generation
		resolved_inputs:         resolved_inputs
		expected_check_sources:  [
			CheckSourceModel{
				name:           'tccbin-candidate-gate'
				repository:     'vlang/tccbin'
				integration_id: 1001
				workflow_id:    2001
				workflow_path:  '.github/workflows/build-and-test.yml'
				event:          'push'
			},
			CheckSourceModel{
				name:           'v-candidate-smoke'
				repository:     'vlang/v'
				integration_id: 1002
				workflow_id:    2002
				workflow_path:  '.github/workflows/tccbin_revalidate.yml'
				event:          'workflow_dispatch'
			},
		]
		deadlines:               IntentDeadlinesModel{
			build_deadline:     '2026-08-02T01:00:00Z'
			checks_deadline:    '2026-08-02T02:30:00Z'
			promotion_deadline: '2026-08-02T02:45:00Z'
		}
		validation_subject:      subject
	}
	native_subject := NativeGateSubjectModel{
		consumer_id:            intent_id
		consumer_kind:          'initial_adopt_current'
		intent_or_operation_id: intent_id
		target_id:              target.target_id
		subject_generation:     target.generation + 1
		initial_run_mode:       'original_push'
		sha:                    subject.sha
		tree:                   subject.tree
		original_ref:           subject.candidate_ref
		input_fingerprint:      subject.input_fingerprint
		artifact_fingerprint:   subject.artifact_fingerprint
		manifest_hash:          subject.manifest_hash
		digests:                subject.digests
	}
	subject_hash := native_gate_subject_hash(native_subject)!
	native_gate := initial_native_gate(native_subject, target.generation + 1, candidate_ref,
		'original_push', '', '2026-08-02T00:00:00Z', GateRunAuthentication{
		repository:                      'vlang/tccbin'
		workflow_id:                     3001
		workflow_path:                   '.github/workflows/build-and-test.yml'
		original_actor:                  'vlang-publisher'
		original_actor_integration_id:   4001
		rerun_triggering_actor:          'vlang-gate-dispatcher'
		rerun_triggering_integration_id: 4002
	}, '')!
	operation_id := deterministic_operation_id(OperationIdentityInput{
		audience:                'vlang/v:tccbin-automation-state'
		run_id:                  100
		run_attempt:             1
		ordinal:                 0
		cas_attempt:             1
		subject_id:              target.target_id
		transition:              'begin-bootstrap'
		expected_generation:     target.generation
		expected_canonical_head: sha
		source_ref:              'mob'
		source_sha:              source_sha
		subject_fingerprint:     target.input_fingerprint
		input_fingerprint:       target.input_fingerprint
		artifact_fingerprint:    target.artifact_fingerprint
		manifest_hash:           target.manifest_hash
		native_subject_hash:     subject_hash
		intent_id:               intent_id
	})!
	target = transition_target(target, .begin_bootstrap, TransitionContext{
		operation_id:       operation_id
		intent:             intent
		validation_subject: subject
		native_subject:     native_subject
		native_gate:        native_gate
	})!
	if target.target_state != .validating || target.last_known_good.sha != ''
		|| can_begin_normal_publication(target) {
		return error('dark bootstrap must remain noneligible and unseeded without native evidence')
	}
	mut missing_proof_rejected := false
	transition_target(target, .bootstrap_green, TransitionContext{
		operation_id: 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
	}) or {
		missing_proof_rejected = true
		TargetModel{}
	}
	if !missing_proof_rejected || target.last_known_good.sha != '' {
		return error('bootstrap without exact native evidence did not fail closed')
	}
	gate := target.active_native_gate
	validate_native_gate(gate)!
	recovery_subject := RecoverySubjectModel{
		consumer_id:            intent_id
		consumer_kind:          'initial_adopt_current'
		intent_or_operation_id: intent_id
		target_id:              target.target_id
		subject_generation:     native_subject.subject_generation
		initial_run_mode:       'original_push'
		sha:                    subject.sha
		tree:                   subject.tree
		original_ref:           subject.candidate_ref
		input_fingerprint:      subject.input_fingerprint
		artifact_fingerprint:   subject.artifact_fingerprint
		manifest_hash:          subject.manifest_hash
		digests:                subject.digests
	}
	handoff_create := create_recovery_handoff_atomic('vlang/v:tccbin-automation-state',
		'4444444444444444444444444444444444444444444444444444444444444444', intent_id,
		recovery_subject, native_subject.subject_generation, 'candidate', 'native_gate', 5001,
		target, '5555555555555555555555555555555555555555555555555555555555555555')!
	target = handoff_create.target
	handoff := handoff_create.handoff
	validate_handoff(handoff)!
	fixture_source := os.read_file(os.join_path(automation_root, 'tests', 'fixtures',
		'receiver-ledger.dark.json'))!
	pending := resolve_receiver_request(fixture_source, ReceiverRequestFacts{
		opaque_id:                 '1111111111111111111111111111111111111111111111111111111111111111'
		repository:                'vlang/v'
		workflow_id:               1001
		workflow_path:             '.github/workflows/update_tccbin.yml'
		workflow_ref:              'master'
		event:                     'workflow_dispatch'
		observed_canonical_head:   'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		observed_subject_ref_head: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	})!
	if pending.allowed_to_execute || pending.publish_allowed {
		return error('pending receiver crossed the ACK or publication boundary')
	}
	selected := resolve_receiver_request(fixture_source, ReceiverRequestFacts{
		opaque_id:                 '7777777777777777777777777777777777777777777777777777777777777777'
		repository:                'vlang/v'
		workflow_id:               1002
		workflow_path:             '.github/workflows/tccbin_revalidate.yml'
		workflow_ref:              'master'
		event:                     'workflow_dispatch'
		current_run_id:            9001
		current_run_attempt:       1
		current_head_sha:          'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
		current_run_name:          'tccbin-recovery-7777777777777777777777777777777777777777777777777777777777777777'
		observed_canonical_head:   'cccccccccccccccccccccccccccccccccccccccc'
		observed_subject_ref_head: 'cccccccccccccccccccccccccccccccccccccccc'
	})!
	if !selected.allowed_to_execute || selected.publish_allowed {
		return error('ACK-selected receiver did not remain a no-publish pure simulation')
	}
	completion := resolve_receiver_completion(fixture_source, os.read_file(os.join_path(automation_root,
		'tests', 'fixtures', 'receiver-workflow-run.dark.json'))!)!
	if completion.handoff_id != '7777777777777777777777777777777777777777777777777777777777777777'
		|| completion.may_create_successor {
		return error('workflow_run completion did not preserve exact handoff routing')
	}
}

fn validate_schema_profile(value JsonValue, schema_name string) ! {
	if value.kind == .object {
		if type_value := value.object_value('type') {
			if type_value.kind == .string_value && type_value.string_value == 'object'
				&& !value.has_object_key('additionalProperties') {
				return error('${schema_name} contains an object schema without additionalProperties')
			}
			if type_value.kind == .string_value && type_value.string_value == 'object' {
				additional := value.object_value('additionalProperties') or {
					return error('${schema_name} contains an open object schema')
				}
				if additional.kind != .boolean || additional.bool_value {
					return error('${schema_name} contains an open object schema')
				}
			}
		}
		if reference := value.object_value('$ref') {
			if reference.kind != .string_value || reference.string_value.contains('://')
				|| reference.string_value.starts_with('/') || reference.string_value.contains('..')
				|| reference.string_value.contains('\\') {
				return error('${schema_name} contains a forbidden schema reference')
			}
		}
		for child in value.object_values {
			validate_schema_profile(child, schema_name)!
		}
	} else if value.kind == .array {
		for child in value.array_value {
			validate_schema_profile(child, schema_name)!
		}
	}
}

fn validate_strict_json_profile() ! {
	valid := parse_strict_json('{"b":1,"a":2}')!
	if canonical_json(valid) != '{"a":2,"b":1}' {
		return error('canonical object ordering is not deterministic')
	}
	invalid_inputs := [
		'{"a":1,"a":2}',
		'{"n":-0}',
		'{"n":1.0}',
		'{"n":1e2}',
		'{"n":9007199254740992}',
		'{"s":"\\uD800"}',
	]
	for source in invalid_inputs {
		parse_strict_json(source) or { continue }
		return error('strict JSON parser accepted a forbidden lexical form')
	}
}
