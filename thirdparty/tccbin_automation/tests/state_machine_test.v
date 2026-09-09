module tests

import crypto.sha256
import os
import tccbin_automation.bin

const state_sha = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
const state_tree = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
const state_candidate_sha = 'cccccccccccccccccccccccccccccccccccccccc'
const state_candidate_tree = 'dddddddddddddddddddddddddddddddddddddddd'
const state_source_sha = 'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
const state_source_tree = 'ffffffffffffffffffffffffffffffffffffffff'
const state_digest = '4444444444444444444444444444444444444444444444444444444444444444'
const state_subject_hash = '5555555555555555555555555555555555555555555555555555555555555555'

fn state_manifest_value() bin.JsonValue {
	profile := bin.parse_strict_json(t2a_profile_source('linux-amd64')) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	producer_source := t2a_producer_observation_source('linux-amd64', profile_sha256)
	producer := bin.parse_strict_json(producer_source) or { panic(err) }
	authority := SyntheticToolchainAuthority{
		target_id:       'linux-amd64'
		profile_id:      'linux-amd64-synthetic-v1'
		profile_sha256:  profile_sha256
		producer_source: producer_source
		producer_sha256: bin.json_sha256(producer)
		producer_digest: (producer.object_value('observation_digest') or {
			panic('observation digest missing')
		}).string_value
	}
	source := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-complete.valid.json')) or { panic(err) }
	return bin.parse_strict_json(t2a_resolved_manifest_toolchain(source, authority)) or {
		panic(err)
	}
}

fn authenticated_state_manifest() bin.AuthenticatedManifestModel {
	base := os.join_path(os.temp_dir(), 'tccbin-state-manifest-${os.getpid()}')
	os.rmdir_all(base) or {}
	authority := t2a_prepare_toolchain_authority(base, 'linux-amd64')
	source := t2a_resolved_manifest_toolchain(os.read_file(os.join_path(automation_root(), 'tests',
		'fixtures', 'manifest-complete.valid.json')) or { panic(err) }, authority)
	path := os.join_path(base, 'manifest.json')
	os.write_file(path, source) or { panic(err) }
	authenticated := bin.authenticate_manifest_file(authority.root, path) or { panic(err) }
	os.rmdir_all(base) or { panic(err) }
	return authenticated
}

fn state_fingerprints() bin.FingerprintSet {
	return bin.authenticated_manifest_fingerprints(authenticated_state_manifest()) or { panic(err) }
}

fn operation(character u8) string {
	return character.ascii_str().repeat(64)
}

fn unique_operation(character u8, suffix u8) string {
	return character.ascii_str().repeat(63) + suffix.ascii_str()
}

fn labelled_operation(label string) string {
	return sha256.sum256(label.bytes()).hex()
}

fn base_digest() []bin.DigestModel {
	return [bin.DigestModel{
		path:   'tcc.exe'
		sha256: state_digest
	}]
}

fn resolved_inputs() bin.ResolvedInputsModel {
	profile := bin.parse_strict_json(t2a_profile_source('linux-amd64')) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	producer_source := t2a_producer_observation_source('linux-amd64', profile_sha256)
	producer := bin.parse_strict_json(producer_source) or { panic(err) }
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
				id:         'bdwgc'
				repository: 'https://github.com/ivmai/bdwgc.git'
				ref:        'master'
				sha:        'e'.repeat(40)
				tree:       'f'.repeat(40)
			},
		]
		source_checks:       [
			bin.SourceCheckModel{
				source_id:       'tinycc'
				resolved_sha:    'c'.repeat(40)
				status:          'resolved'
				evidence_digest: operation(`6`)
			},
			bin.SourceCheckModel{
				source_id:       'bdwgc'
				resolved_sha:    'e'.repeat(40)
				status:          'resolved'
				evidence_digest: operation(`5`)
			},
		]
		recipe_path:         'build.sh'
		recipe_hash:         operation(`1`)
		contract_repository: 'GGRei/v'
		contract_sha:        state_sha
		v_source_sha:        'b'.repeat(40)
		producer_toolchain:  bin.ProducerToolchainModel{
			profile_id:         'linux-amd64-synthetic-v1'
			profile_sha256:     profile_sha256
			observation_sha256: bin.json_sha256(producer)
			observation_digest: (producer.object_value('observation_digest') or {
				panic('observation digest missing')
			}).string_value
		}
	}
}

fn test_authenticated_manifest_closes_every_durable_resolved_input_projection() {
	manifest := authenticated_state_manifest()
	fingerprints := bin.authenticated_manifest_fingerprints(manifest) or { panic(err) }
	inputs := resolved_inputs()
	bin.validate_authenticated_manifest_resolved_inputs(manifest, inputs,
		fingerprints.input_fingerprint) or { panic(err) }
	mut reordered_checks := inputs.source_checks.clone()
	reordered_checks.reverse_in_place()
	bin.validate_authenticated_manifest_resolved_inputs(manifest, bin.ResolvedInputsModel{
		...inputs
		source_checks: reordered_checks
	}, fingerprints.input_fingerprint) or { panic(err) }

	mut source_drift := inputs.sources.clone()
	source_drift[0] = bin.ResolvedSourceModel{
		...source_drift[0]
		tree: '9'.repeat(40)
	}
	mutations := [
		bin.ResolvedInputsModel{
			...inputs
			sources: source_drift
		},
		bin.ResolvedInputsModel{
			...inputs
			recipe_path: 'reviewed/build.sh'
		},
		bin.ResolvedInputsModel{
			...inputs
			recipe_hash: operation(`9`)
		},
		bin.ResolvedInputsModel{
			...inputs
			contract_repository: 'vlang/v'
		},
		bin.ResolvedInputsModel{
			...inputs
			contract_sha: '9'.repeat(40)
		},
		bin.ResolvedInputsModel{
			...inputs
			v_source_sha: '9'.repeat(40)
		},
		bin.ResolvedInputsModel{
			...inputs
			producer_toolchain: bin.ProducerToolchainModel{
				...inputs.producer_toolchain
				observation_sha256: operation(`9`)
			}
		},
	]
	for mutation in mutations {
		mut rejected := ''
		bin.validate_authenticated_manifest_resolved_inputs(manifest, mutation,
			fingerprints.input_fingerprint) or { rejected = err.msg() }
		assert rejected == 'resolved inputs differ from the authenticated manifest projection'
	}
	mut missing_check := inputs.source_checks.clone()
	missing_check.delete(missing_check.len - 1)
	mut duplicate_check := inputs.source_checks.clone()
	duplicate_check[1] = duplicate_check[0]
	mut wrong_check_sha := inputs.source_checks.clone()
	wrong_check_sha[0] = bin.SourceCheckModel{
		...wrong_check_sha[0]
		resolved_sha: '9'.repeat(40)
	}
	mut wrong_check_status := inputs.source_checks.clone()
	wrong_check_status[0] = bin.SourceCheckModel{
		...wrong_check_status[0]
		status: 'unreachable'
	}
	mut missing_check_rejected := ''
	bin.validate_authenticated_manifest_resolved_inputs(manifest, bin.ResolvedInputsModel{
		...inputs
		source_checks: missing_check
	}, fingerprints.input_fingerprint) or { missing_check_rejected = err.msg() }
	assert missing_check_rejected == 'resolved source/recipe/contract/toolchain bindings are incomplete'
	for invalid_checks in [duplicate_check, wrong_check_sha, wrong_check_status] {
		mut rejected := ''
		bin.validate_authenticated_manifest_resolved_inputs(manifest, bin.ResolvedInputsModel{
			...inputs
			source_checks: invalid_checks
		}, fingerprints.input_fingerprint) or { rejected = err.msg() }
		assert rejected == 'each resolved source requires one exact source check'
	}
	invalid_profile := bin.ResolvedInputsModel{
		...inputs
		producer_toolchain: bin.ProducerToolchainModel{
			...inputs.producer_toolchain
			profile_id: 'Invalid_Profile'
		}
	}
	mut invalid_profile_rejected := ''
	bin.validate_authenticated_manifest_resolved_inputs(manifest, invalid_profile,
		fingerprints.input_fingerprint) or { invalid_profile_rejected = err.msg() }
	assert invalid_profile_rejected == 'resolved source/recipe/contract/toolchain bindings are incomplete'
	mut fingerprint_rejected := ''
	bin.validate_authenticated_manifest_resolved_inputs(manifest, inputs, operation(`9`)) or {
		fingerprint_rejected = err.msg()
	}
	assert fingerprint_rejected == 'resolved inputs do not bind the authenticated manifest input fingerprint'
}

fn check_sources() []bin.CheckSourceModel {
	return [
		bin.CheckSourceModel{
			name:           'tccbin-candidate-gate'
			repository:     'vlang/tccbin'
			integration_id: 1001
			workflow_id:    2001
			workflow_path:  '.github/workflows/build-and-test.yml'
			event:          'push'
		},
		bin.CheckSourceModel{
			name:           'v-candidate-smoke'
			repository:     'vlang/v'
			integration_id: 1002
			workflow_id:    2002
			workflow_path:  '.github/workflows/tccbin_revalidate.yml'
			event:          'workflow_dispatch'
		},
	]
}

fn validation_subject(sha string, tree string, candidate_ref string) bin.ValidationSubjectModel {
	fingerprints := state_fingerprints()
	return bin.ValidationSubjectModel{
		sha:                  sha
		tree:                 tree
		input_fingerprint:    fingerprints.input_fingerprint
		artifact_fingerprint: fingerprints.artifact_fingerprint
		manifest_hash:        fingerprints.manifest_hash
		digests:              base_digest()
		candidate_ref:        candidate_ref
	}
}

fn artifact_tuple(sha string, tree string) bin.ArtifactTupleModel {
	fingerprints := state_fingerprints()
	return bin.ArtifactTupleModel{
		sha:                  sha
		tree:                 tree
		input_fingerprint:    fingerprints.input_fingerprint
		artifact_fingerprint: fingerprints.artifact_fingerprint
		manifest_hash:        fingerprints.manifest_hash
		digests:              base_digest()
	}
}

fn reserved_intent(target bin.TargetModel, intent_type string, intent_id string,
	subject bin.ValidationSubjectModel) bin.ActiveIntentModel {
	return bin.ActiveIntentModel{
		intent_id:                intent_id
		intent_type:              intent_type
		stage:                    'intent_reserved'
		run_id:                   10
		run_attempt:              1
		ordinal:                  0
		input_fingerprint:        target.input_fingerprint
		expected_canonical_head:  target.canonical_observed_sha
		candidate_ref:            'tccbin-candidate/${target.target_id}/${intent_id}'
		generation:               target.generation
		resolved_inputs:          resolved_inputs()
		expected_check_sources:   check_sources()
		deadlines:                bin.IntentDeadlinesModel{
			build_deadline:     '2026-08-02T01:00:00Z'
			checks_deadline:    '2026-08-02T02:30:00Z'
			promotion_deadline: '2026-08-02T02:45:00Z'
		}
		validation_subject:       subject
		previous_last_known_good: if intent_type == 'initial_adopt_current' {
			bin.ArtifactTupleModel{}
		} else {
			target.last_known_good
		}
	}
}

fn candidate_binding(parent string) bin.CandidateBindingModel {
	return candidate_binding_with_sha(parent, state_candidate_sha, state_candidate_tree)
}

fn candidate_binding_with_sha(parent string, sha string, tree string) bin.CandidateBindingModel {
	fingerprints := state_fingerprints()
	return bin.CandidateBindingModel{
		sha:                  sha
		tree:                 tree
		parent:               parent
		artifact_fingerprint: fingerprints.artifact_fingerprint
		manifest_hash:        fingerprints.manifest_hash
		digests:              base_digest()
	}
}

fn native_subject_for(target bin.TargetModel, consumer_id string, consumer_kind string,
	subject bin.ValidationSubjectModel, subject_generation i64) bin.NativeGateSubjectModel {
	return bin.NativeGateSubjectModel{
		consumer_id:            consumer_id
		consumer_kind:          consumer_kind
		intent_or_operation_id: consumer_id
		target_id:              target.target_id
		subject_generation:     subject_generation
		initial_run_mode:       'original_push'
		remediation_trigger:    if consumer_kind == 'remediation' {
			bin.RemediationTriggerModel{
				repository:       'vlang/tccbin'
				ref:              'thirdparty-${target.target_id}'
				before:           if subject.sha == state_candidate_sha {
					state_source_sha
				} else {
					state_candidate_sha
				}
				after:            subject.sha
				tree:             subject.tree
				diff_fingerprint: labelled_operation('remediation-trigger-${consumer_id}')
				owner_domain:     'tccbin'
			}
		} else {
			bin.RemediationTriggerModel{}
		}
		sha:                    subject.sha
		tree:                   subject.tree
		original_ref:           subject.candidate_ref
		input_fingerprint:      subject.input_fingerprint
		artifact_fingerprint:   subject.artifact_fingerprint
		manifest_hash:          subject.manifest_hash
		digests:                subject.digests
	}
}

fn test_t2b_native_subject_builder_preserves_remediation_trigger_nullability_and_hash() {
	target := initialized_target()
	remediation_id := unique_operation(`8`, `1`)
	remediation_subject := native_subject_for(target, remediation_id, 'remediation', validation_subject(state_sha,
		state_tree, 'thirdparty-${target.target_id}'), target.generation + 1)
	remediation_value := t2b_native_subject_value(remediation_subject)
	trigger := remediation_value.object_value('remediation_trigger') or {
		panic('remediation trigger missing')
	}
	assert trigger.kind == .object
	assert trigger.object_keys == ['repository', 'ref', 'before', 'after', 'tree', 'diff_fingerprint',
		'owner_domain']
	assert (trigger.object_value('repository') or { panic('trigger repository missing') }).string_value == remediation_subject.remediation_trigger.repository
	assert (trigger.object_value('ref') or { panic('trigger ref missing') }).string_value == remediation_subject.remediation_trigger.ref
	assert (trigger.object_value('before') or { panic('trigger before missing') }).string_value == remediation_subject.remediation_trigger.before
	assert (trigger.object_value('after') or { panic('trigger after missing') }).string_value == remediation_subject.remediation_trigger.after
	assert (trigger.object_value('tree') or { panic('trigger tree missing') }).string_value == remediation_subject.remediation_trigger.tree
	assert (trigger.object_value('diff_fingerprint') or {
		panic('trigger diff fingerprint missing')
	}).string_value == remediation_subject.remediation_trigger.diff_fingerprint
	assert (trigger.object_value('owner_domain') or { panic('trigger owner domain missing') }).string_value == remediation_subject.remediation_trigger.owner_domain
	remediation_hash := bin.native_gate_subject_hash(remediation_subject) or { panic(err) }
	assert bin.json_sha256(remediation_value) == remediation_hash

	candidate_id := unique_operation(`8`, `2`)
	candidate_subject := native_subject_for(target, candidate_id, 'publish_candidate', validation_subject(state_candidate_sha,
		state_candidate_tree, 'tccbin-candidate/${target.target_id}/${candidate_id}'),

		target.generation + 1)
	candidate_value := t2b_native_subject_value(candidate_subject)
	assert (candidate_value.object_value('remediation_trigger') or {
		panic('candidate remediation trigger missing')
	}).kind == .null_value
	assert bin.json_sha256(candidate_value) == bin.native_gate_subject_hash(candidate_subject) or {
		panic(err)
	}

	base := os.join_path(os.temp_dir(), 'tccbin-state-subject-oracle-${os.getpid()}')
	os.rmdir_all(base) or {}
	authority := t2a_prepare_toolchain_authority(base, target.target_id)
	defer {
		os.rmdir_all(base) or { panic(err) }
	}
	matrix_source := t2b_native_matrix_source_for_run(bin.canonical_json(state_manifest_value()),
		authority, remediation_subject, 7001, 1, 7101)
	matrix := bin.parse_strict_json(matrix_source) or { panic(err) }
	matrix_subject := matrix.object_value('subject') or { panic('matrix subject missing') }
	assert bin.canonical_json(matrix_subject) == bin.canonical_json(remediation_value)
	assert (matrix.object_value('subject_hash') or { panic('matrix subject hash missing') }).string_value == remediation_hash
}

fn gate_authentication() bin.GateRunAuthentication {
	return bin.GateRunAuthentication{
		repository:                      'vlang/tccbin'
		workflow_id:                     2001
		workflow_path:                   '.github/workflows/build-and-test.yml'
		original_actor:                  'tccbin-publisher[bot]'
		original_actor_integration_id:   1001
		rerun_triggering_actor:          'tccbin-gate-dispatcher[bot]'
		rerun_triggering_integration_id: 1003
	}
}

fn initial_gate_for(subject bin.NativeGateSubjectModel) bin.NativeGateModel {
	return bin.initial_native_gate(subject, subject.subject_generation, subject.original_ref,
		'original_push', '', '2026-08-02T00:00:00Z', gate_authentication(), '') or { panic(err) }
}

fn complete_active_native_gate(input bin.TargetModel, conclusion string) bin.TargetModel {
	gate := input.active_native_gate
	run := bin.GateRunCandidate{
		epoch:                           gate.active_gate_epoch
		run_id:                          i64(3000 + input.generation)
		run_attempt:                     1
		repository:                      'vlang/tccbin'
		ref:                             gate.epochs[gate.active_gate_epoch].expected_ref
		sha:                             gate.subject.sha
		event:                           'push'
		actor:                           'tccbin-publisher[bot]'
		actor_integration_id:            1001
		triggering_actor:                'tccbin-publisher[bot]'
		triggering_actor_integration_id: 1001
		check_suite_id:                  i64(4000 + input.generation)
		workflow_id:                     2001
		workflow_path:                   '.github/workflows/build-and-test.yml'
		created_at:                      '2026-08-02T00:04:00Z'
		conclusion:                      'pending'
	}
	ack_id := labelled_operation('${gate.subject_hash}/ack/${input.generation}')
	ack := bin.acknowledge_gate_run(gate, run, input, ack_id) or { panic(err) }
	terminal := bin.GateRunCandidate{
		...run
		conclusion: conclusion
	}
	complete_id := labelled_operation('${gate.subject_hash}/complete/${ack.target.generation}')
	completed := bin.complete_gate_epoch(ack.gate, terminal, ack.target, complete_id,
		'2026-08-02T00:05:00Z') or { panic(err) }
	return completed.target
}

struct StateNativeCapsule {
	capsule       bin.AuthenticatedNativeValidationCapsule
	matrix_digest string
}

fn state_native_capsule_for_run(target bin.TargetModel, manifest bin.AuthenticatedManifestModel,
	outcome string, run_id i64, run_attempt int, check_suite_id i64) StateNativeCapsule {
	base := os.join_path(os.temp_dir(),
		'tccbin-state-native-matrix-${os.getpid()}-${target.generation}-${outcome}-${run_id}-${run_attempt}-${check_suite_id}')
	os.rmdir_all(base) or {}
	authority := t2a_prepare_toolchain_authority(base, 'linux-amd64')
	defer {
		os.rmdir_all(base) or { panic(err) }
	}
	subject := target.active_native_subject
	mut source := t2b_native_matrix_source_for_run(bin.canonical_json(state_manifest_value()),
		authority, subject, run_id, run_attempt, check_suite_id)
	if outcome == 'functional' {
		source = t2b_replace_matrix_result_member(source, 0, 'status', '"failed"')
	} else if outcome == 'fallback' {
		source = t2b_replace_matrix_result_member(source, 0, 'fallback_used', 'true')
	} else if outcome == 'infrastructure' {
		source = t2b_replace_matrix_result_member(source, 0, 'status', '"blocked"')
	} else if outcome != 'green' {
		panic('unknown state matrix outcome')
	}
	capsule_root := os.join_path(base, 'native-validation-capsule')
	t2c_write_native_validation_capsule(capsule_root, source, authority, false)
	capsule := bin.authenticate_native_validation_capsule(authority.root, manifest, subject,
		capsule_root) or { panic(err) }
	return StateNativeCapsule{
		capsule:       capsule
		matrix_digest: sha256.sum256(source.bytes()).hex()
	}
}

fn state_native_capsule(target bin.TargetModel, manifest bin.AuthenticatedManifestModel,
	outcome string) StateNativeCapsule {
	native_run := target.active_native_gate.gate_runs.filter(
		it.run_id == target.active_native_gate.selected_run_id
		&& it.run_attempt == target.active_native_gate.selected_run_attempt)[0]
	return state_native_capsule_for_run(target, manifest, outcome, native_run.run_id,
		native_run.run_attempt, native_run.check_suite_id)
}

fn persisted_gate_proofs(target bin.TargetModel,
	matrix_digest string) (bin.PersistedGateRunModel, bin.PersistedGateRunModel) {
	subject := target.active_native_subject
	subject_hash := bin.native_gate_subject_hash(subject) or { panic(err) }
	native_run := target.active_native_gate.gate_runs.filter(
		it.run_id == target.active_native_gate.selected_run_id
		&& it.run_attempt == target.active_native_gate.selected_run_attempt)[0]
	native_job_id := i64(5000 + target.generation)
	native_check_run_id := i64(6000 + target.generation)
	native_external_id := bin.deterministic_check_external_id('vlang/tccbin:native-gate-check:v1',
		subject.consumer_id, subject_hash, native_run.run_id, native_run.run_attempt) or {
		panic(err)
	}
	native_run_url := 'https://github.com/vlang/tccbin/actions/runs/${native_run.run_id}'
	native_job_url := '${native_run_url}/job/${native_job_id}'
	smoke_run_id := i64(7000 + target.generation)
	smoke_run_attempt := 1
	smoke_check_suite_id := i64(8000 + target.generation)
	smoke_job_id := i64(9000 + target.generation)
	smoke_check_run_id := i64(10000 + target.generation)
	smoke_external_id := bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
		subject.consumer_id, subject_hash, smoke_run_id, smoke_run_attempt) or { panic(err) }
	smoke_run_url := 'https://github.com/vlang/v/actions/runs/${smoke_run_id}'
	smoke_job_url := '${smoke_run_url}/job/${smoke_job_id}'
	return bin.PersistedGateRunModel{
		check_name:                      'tccbin-candidate-gate'
		repository:                      'vlang/tccbin'
		integration_id:                  1001
		workflow_id:                     2001
		workflow_path:                   '.github/workflows/build-and-test.yml'
		event:                           'push'
		run_id:                          native_run.run_id
		run_attempt:                     native_run.run_attempt
		check_suite_id:                  native_run.check_suite_id
		check_suite_integration_id:      1001
		job_id:                          native_job_id
		subject_hash:                    subject_hash
		check_run_id:                    native_check_run_id
		external_id:                     native_external_id
		run_name:                        'tccbin-native-gate/${subject.consumer_id}'
		run_url:                         native_run_url
		job_url:                         native_job_url
		details_url:                     native_job_url
		ref:                             native_run.ref
		workflow_head_sha:               subject.sha
		sha:                             subject.sha
		check_sha:                       subject.sha
		actor:                           native_run.actor
		actor_integration_id:            native_run.actor_integration_id
		triggering_actor:                native_run.triggering_actor
		triggering_actor_integration_id: native_run.triggering_actor_integration_id
		created_at:                      native_run.created_at
		completed_at:                    '2026-08-02T00:05:00Z'
		run_conclusion:                  native_run.conclusion
		check_conclusion:                'success'
		output_digest:                   matrix_digest
		evidence_digest:                 operation(`9`)
	}, bin.PersistedGateRunModel{
		check_name:                      'v-candidate-smoke'
		repository:                      'vlang/v'
		integration_id:                  1002
		workflow_id:                     2002
		workflow_path:                   '.github/workflows/tccbin_revalidate.yml'
		event:                           'workflow_dispatch'
		run_id:                          smoke_run_id
		run_attempt:                     smoke_run_attempt
		check_suite_id:                  smoke_check_suite_id
		check_suite_integration_id:      1001
		job_id:                          smoke_job_id
		subject_hash:                    subject_hash
		check_run_id:                    smoke_check_run_id
		external_id:                     smoke_external_id
		run_name:                        'tccbin-v-smoke/${subject.consumer_id}'
		run_url:                         smoke_run_url
		job_url:                         smoke_job_url
		details_url:                     smoke_job_url
		ref:                             'master'
		workflow_head_sha:               target.resolved_inputs.v_source_sha
		sha:                             subject.sha
		check_sha:                       subject.sha
		actor:                           'validator-dispatcher[bot]'
		actor_integration_id:            1002
		triggering_actor:                'validator-dispatcher[bot]'
		triggering_actor_integration_id: 1002
		created_at:                      '2026-08-02T00:06:00Z'
		completed_at:                    '2026-08-02T00:07:00Z'
		run_conclusion:                  'success'
		check_conclusion:                'success'
		output_digest:                   operation(`8`)
		evidence_digest:                 operation(`a`)
	}
}

fn green_proof(target bin.TargetModel) bin.GreenVerdictProof {
	manifest := authenticated_state_manifest()
	capsule := state_native_capsule(target, manifest, 'green')
	native_gate, smoke_gate := persisted_gate_proofs(target, capsule.matrix_digest)
	return bin.GreenVerdictProof{
		expected_ledger_generation: target.generation
		manifest:                   manifest
		native_capsule:             capsule.capsule
		expected_check_sources:     check_sources()
		native_gate:                native_gate
		v_smoke_gate:               smoke_gate
	}
}

fn red_proof(target bin.TargetModel, failure_kind string) bin.RedVerdictProof {
	return red_proof_with_matrix_outcome(target, failure_kind, if failure_kind == 'functional' {
		'functional'
	} else if failure_kind == 'infrastructure' {
		'infrastructure'
	} else {
		'green'
	})
}

fn red_proof_with_matrix_outcome(target bin.TargetModel, failure_kind string,
	matrix_outcome string) bin.RedVerdictProof {
	manifest := authenticated_state_manifest()
	capsule := state_native_capsule(target, manifest, matrix_outcome)
	native_gate, smoke_gate := persisted_gate_proofs(target, capsule.matrix_digest)
	return bin.RedVerdictProof{
		expected_ledger_generation: target.generation
		manifest:                   manifest
		native_capsule:             capsule.capsule
		expected_check_sources:     check_sources()
		native_gate:                native_gate
		v_smoke_gate:               smoke_gate
		failure_kind:               failure_kind
	}
}

fn resealed_native_validation_record(record bin.NativeValidationRecordModel) bin.NativeValidationRecordModel {
	seed := bin.NativeValidationRecordModel{
		...record
		validation_digest: operation(`0`)
	}
	digest := bin.native_validation_record_digest(bin.native_validation_record_json(seed) or {
		panic(err)
	}) or { panic(err) }
	return bin.NativeValidationRecordModel{
		...seed
		validation_digest: digest
	}
}

fn native_validation_record_is_rejected(target bin.TargetModel,
	record bin.NativeValidationRecordModel) bool {
	mut forged := target
	forged.last_native_validation = record
	bin.validate_target_model(forged) or { return true }
	return false
}

fn assert_blocked_red_gate_join_is_enforced(target bin.TargetModel) {
	assert target.active_intent.stage == 'blocked'
	assert target.active_intent.gate_runs.len == 2
	mut gates := target.active_intent.gate_runs.clone()
	gates[0] = bin.PersistedGateRunModel{
		...gates[0]
		evidence_digest: unique_operation(`d`, `e`)
	}
	mut forged := target
	forged.active_intent = bin.ActiveIntentModel{
		...target.active_intent
		gate_runs: gates
	}
	mut rejected := ''
	bin.validate_target_model(forged) or { rejected = err.msg() }
	assert rejected == 'blocked red validation differs from its active subject and two gate runs'

	mut owner_forged := target
	match target.last_native_validation.transition {
		'candidate_failed' {
			owner_forged.active_intent = bin.ActiveIntentModel{
				...target.active_intent
				candidate_binding: bin.CandidateBindingModel{
					...target.active_intent.candidate_binding
					tree: state_source_tree
				}
			}
		}
		'post_check_infra_exhausted' {
			owner_forged.provisional_published = bin.ArtifactTupleModel{
				...target.provisional_published
				tree: state_source_tree
			}
		}
		'rollback_failed' {
			if target.active_intent.rollback_provisional.sha != '' {
				owner_forged.active_intent = bin.ActiveIntentModel{
					...target.active_intent
					rollback_provisional: bin.CandidateBindingModel{
						...target.active_intent.rollback_provisional
						tree: state_source_tree
					}
				}
			} else {
				owner_forged.active_intent = bin.ActiveIntentModel{
					...target.active_intent
					candidate_binding: bin.CandidateBindingModel{
						...target.active_intent.candidate_binding
						tree: state_source_tree
					}
				}
			}
		}
		else {
			panic('unexpected blocked-red transition')
		}
	}
	rejected = ''
	bin.validate_target_model(owner_forged) or { rejected = err.msg() }
	assert rejected == 'blocked red validation does not match its exact transition owner'
}

fn head_observation(target bin.TargetModel, operation_id string, canonical_head string,
	subject_sha string, relationship bin.HeadRelationship) bin.HeadObservationModel {
	return bin.HeadObservationModel{
		target_id:              target.target_id
		expected_generation:    target.generation
		expected_previous_head: target.canonical_observed_sha
		canonical_head:         canonical_head
		subject_sha:            subject_sha
		relationship:           relationship
		observed_at:            '2026-08-02T00:10:00Z'
		operation_id:           operation_id
		evidence_digest:        operation(`5`)
	}
}

fn initialized_target() bin.TargetModel {
	mut target := bin.initial_target_model('linux-amd64', state_sha)
	fingerprints := state_fingerprints()
	target.input_fingerprint = fingerprints.input_fingerprint
	target.artifact_fingerprint = fingerprints.artifact_fingerprint
	target.manifest_hash = fingerprints.manifest_hash
	target.provenance_status = 'complete'
	target.resolved_inputs = resolved_inputs()
	return target
}

fn test_target_root_and_reserved_intent_share_one_complete_resolved_input_tuple() {
	target := initialized_target()
	intent_id := operation(`8`)
	intent := reserved_intent(target, 'publish', intent_id, bin.ValidationSubjectModel{})
	mut with_intent := target
	with_intent.publication_state = .candidate_pending
	with_intent.active_intent = intent
	bin.validate_target_model(with_intent) or { panic(err) }

	drifted_inputs := bin.ResolvedInputsModel{
		...intent.resolved_inputs
		producer_toolchain: bin.ProducerToolchainModel{
			...intent.resolved_inputs.producer_toolchain
			observation_digest: operation(`9`)
		}
	}
	mut drifted := with_intent
	drifted.active_intent = bin.ActiveIntentModel{
		...intent
		resolved_inputs: drifted_inputs
	}
	mut rejected := ''
	bin.validate_target_model(drifted) or { rejected = err.msg() }
	assert rejected == 'active intention resolved inputs differ from the target root'
	mut fingerprint_drift := with_intent
	fingerprint_drift.active_intent = bin.ActiveIntentModel{
		...intent
		input_fingerprint: operation(`9`)
	}
	rejected = ''
	bin.validate_target_model(fingerprint_drift) or { rejected = err.msg() }
	assert rejected == 'active intention resolved inputs differ from the target root'

	mut unresolved := bin.initial_target_model('linux-amd64', state_sha)
	unresolved.active_intent = intent
	bin.validate_target_model(unresolved) or { rejected = err.msg() }
	assert rejected == 'an unresolved target cannot retain resolved inputs or an active intention'
	mut fingerprint_without_inputs := bin.initial_target_model('linux-amd64', state_sha)
	fingerprint_without_inputs.input_fingerprint = operation(`9`)
	bin.validate_target_model(fingerprint_without_inputs) or { rejected = err.msg() }
	assert rejected == 'a resolved target fingerprint requires complete resolved inputs'

	mut seeded_without_inputs := seeded_target()
	seeded_without_inputs.resolved_inputs = bin.ResolvedInputsModel{}
	bin.validate_target_model(seeded_without_inputs) or { rejected = err.msg() }
	assert rejected == 'seeded target must retain complete resolved inputs'
}

fn bootstrap_validation_ready() bin.TargetModel {
	mut target := initialized_target()
	intent_id := operation(`b`)
	subject := validation_subject(state_sha, state_tree,
		'tccbin-candidate/linux-amd64/${intent_id}')
	intent := reserved_intent(target, 'initial_adopt_current', intent_id, subject)
	native_subject := native_subject_for(target, intent_id, 'initial_adopt_current', subject,

		target.generation + 1)
	target = bin.transition_target(target, .begin_bootstrap, bin.TransitionContext{
		operation_id:       operation(`c`)
		intent:             intent
		validation_subject: subject
		native_subject:     native_subject
		native_gate:        initial_gate_for(native_subject)
	}) or { panic(err) }
	target = bin.transition_target(target, .bind_candidate, bin.TransitionContext{
		operation_id:       operation(`d`)
		validation_subject: subject
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	return target
}

fn seeded_target() bin.TargetModel {
	mut target := bootstrap_validation_ready()
	proof := green_proof(target)
	target = bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: operation(`e`)
		green_proof:  proof
	}) or { panic(err) }
	target = bin.transition_target(target, .bootstrap_green, bin.TransitionContext{
		operation_id:     operation(`f`)
		head_observation: head_observation(target, operation(`f`), state_sha, state_sha,
			.exact_subject)
		green_proof:      green_proof(target)
	}) or { panic(err) }
	return target
}

fn remediation_validation_ready() (bin.TargetModel, bin.ValidationSubjectModel) {
	mut target := seeded_target()
	defect_operation := unique_operation(`b`, `1`)
	target = bin.transition_target(target, .actionable_defect, bin.TransitionContext{
		operation_id: defect_operation
	}) or { panic(err) }
	subject := validation_subject(target.last_known_good.sha, target.last_known_good.tree,
		'thirdparty-${target.target_id}')
	remediation_id := unique_operation(`b`, `2`)
	native_subject := native_subject_for(target, remediation_id, 'remediation', subject,

		target.generation + 1)
	target = bin.transition_target(target, .begin_remediation, bin.TransitionContext{
		operation_id:       remediation_id
		validation_subject: subject
		check_sources:      check_sources()
		native_subject:     native_subject
		native_gate:        initial_gate_for(native_subject)
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	return target, subject
}

fn test_bootstrap_never_seeds_without_exact_dynamic_proof() {
	mut target := initialized_target()
	intent_id := operation(`b`)
	subject := validation_subject(state_sha, state_tree,
		'tccbin-candidate/linux-amd64/${intent_id}')
	intent := reserved_intent(target, 'initial_adopt_current', intent_id, subject)
	native_subject := native_subject_for(target, intent_id, 'initial_adopt_current', subject,

		target.generation + 1)
	target = bin.transition_target(target, .begin_bootstrap, bin.TransitionContext{
		operation_id:       operation(`c`)
		intent:             intent
		validation_subject: subject
		native_subject:     native_subject
		native_gate:        initial_gate_for(native_subject)
	}) or { panic(err) }
	assert target.last_known_good.sha == ''
	assert target.target_state == .validating
	mut rejected := false
	bin.transition_target(target, .bootstrap_green, bin.TransitionContext{
		operation_id: operation(`d`)
	}) or { rejected = true }
	assert rejected
	assert !bin.can_begin_normal_publication(target)
	target = bin.transition_target(target, .bind_candidate, bin.TransitionContext{
		operation_id:       operation(`e`)
		validation_subject: subject
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	proof := green_proof(target)
	bad_proof := bin.GreenVerdictProof{
		...proof
		native_gate: bin.PersistedGateRunModel{
			...proof.native_gate
			integration_id: 9999
		}
	}
	rejected = false
	bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: operation(`6`)
		green_proof:  bad_proof
	}) or { rejected = true }
	assert rejected
	seeded := seeded_target()
	assert seeded.last_known_good.sha == state_sha
	assert seeded.target_state == .eligible
	assert !seeded.bootstrap_required
	record := seeded.last_native_validation
	assert record.schema_version == 1
	assert record.operation_id == operation(`f`)
	assert record.transition == 'bootstrap_green'
	assert record.resulting_generation == seeded.generation
	assert record.verdict == 'green'
	assert record.manifest_hash == seeded.manifest_hash
	assert record.native_lane_matrix.kind == .object
	assert record.evidence.len > 0
	assert record.native_gate.output_digest == record.matrix_digest
	encoded_record := bin.native_validation_record_json(record) or { panic(err) }
	assert encoded_record.object_keys == ['schema_version', 'operation_id', 'transition',
		'resulting_generation', 'verdict', 'manifest_source', 'manifest_hash', 'native_lane_matrix',
		'matrix_digest', 'evidence', 'capsule_digest', 'native_gate', 'v_smoke_gate',
		'validation_digest']
	assert record.validation_digest == bin.native_validation_record_digest(encoded_record) or {
		panic(err)
	}
	assert bin.can_begin_normal_publication(seeded)
}

fn test_durable_native_validation_record_replay_closes_digest_cas_output_and_verdict_joins() {
	target := checked_candidate()
	record := target.last_native_validation
	assert record.transition == 'candidate_checks_green'
	assert record.verdict == 'green'
	assert record.operation_id == operation(`5`)
	assert record.resulting_generation == target.generation
	bin.validate_target_model(target) or { panic(err) }

	mut rejected := ''
	mut forged := target
	forged.last_native_validation = bin.NativeValidationRecordModel{
		...record
		validation_digest: operation(`f`)
	}
	bin.validate_target_model(forged) or { rejected = err.msg() }
	assert rejected == 'last native validation digest differs from its complete durable facts'

	rejected = ''
	forged = target
	forged.last_native_validation = resealed_native_validation_record(bin.NativeValidationRecordModel{
		...record
		operation_id: operation(`7`)
	})
	bin.validate_target_model(forged) or { rejected = err.msg() }
	assert rejected == 'last native validation is not joined to its target, CAS operation, or producer'

	rejected = ''
	forged = target
	forged.last_native_validation = resealed_native_validation_record(bin.NativeValidationRecordModel{
		...record
		native_gate: bin.PersistedGateRunModel{
			...record.native_gate
			output_digest: operation(`7`)
		}
	})
	bin.validate_target_model(forged) or { rejected = err.msg() }
	assert rejected == 'last native validation differs from its selected native winner or gate sources'

	rejected = ''
	forged = target
	forged.last_native_validation = resealed_native_validation_record(bin.NativeValidationRecordModel{
		...record
		verdict: 'functional'
	})
	bin.validate_target_model(forged) or { rejected = err.msg() }
	assert rejected == 'last native validation transition differs from its subject and verdict'

	rejected = ''
	forged = target
	forged.last_native_validation = resealed_native_validation_record(bin.NativeValidationRecordModel{
		...record
		manifest_source: '${record.manifest_source}\n'
	})
	bin.validate_target_model(forged) or { rejected = err.msg() }
	assert rejected == 'last native validation manifest hash differs from its exact source bytes'

	mut missing_evidence := record.evidence.clone()
	assert missing_evidence.len > 1
	missing_evidence.delete(missing_evidence.len - 1)
	mut reversed_evidence := record.evidence.clone()
	reversed_evidence[0], reversed_evidence[1] = reversed_evidence[1], reversed_evidence[0]
	mut duplicate_evidence := record.evidence.clone()
	duplicate_evidence << record.evidence[0]
	mut oversized_evidence := record.evidence.clone()
	oversized_evidence[0] = bin.NativeValidationEvidenceModel{
		...oversized_evidence[0]
		size: 262145
	}
	variants := [
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			schema_version: 2
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			operation_id: operation(`7`)
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			transition: 'candidate_failed'
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			resulting_generation: record.resulting_generation + 1
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			verdict: 'functional'
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			manifest_source: '${record.manifest_source}\n'
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			manifest_hash: operation(`7`)
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			native_lane_matrix: bin.JsonValue{
				kind: .null_value
			}
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			matrix_digest: operation(`7`)
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			evidence: missing_evidence
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			capsule_digest: operation(`7`)
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			native_gate: bin.PersistedGateRunModel{
				...record.native_gate
				repository: 'attacker/example'
			}
		}),
		resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			v_smoke_gate: bin.PersistedGateRunModel{
				...record.v_smoke_gate
				ref: 'attacker-ref'
			}
		}),
		bin.NativeValidationRecordModel{
			...record
			validation_digest: operation(`7`)
		},
	]
	assert variants.len == 14
	for variant in variants {
		assert native_validation_record_is_rejected(target, variant)
	}
	for evidence_variant in [reversed_evidence, duplicate_evidence, oversized_evidence] {
		assert native_validation_record_is_rejected(target, resealed_native_validation_record(bin.NativeValidationRecordModel{
			...record
			evidence: evidence_variant
		}))
	}
}

fn test_native_validation_record_write_table_covers_red_candidate_and_remediation_results() {
	bootstrap_ready := bootstrap_validation_ready()
	bootstrap_red_operation := unique_operation(`b`, `3`)
	bootstrap_red := bin.transition_target(bootstrap_ready, .bootstrap_red, bin.TransitionContext{
		operation_id:     bootstrap_red_operation
		head_observation: head_observation(bootstrap_ready, bootstrap_red_operation, state_sha,
			state_sha, .exact_subject)
		red_proof:        red_proof(bootstrap_ready, 'functional')
	}) or { panic(err) }
	assert bootstrap_red.last_native_validation.transition == 'bootstrap_red'
	assert bootstrap_red.last_native_validation.verdict == 'functional'
	assert bootstrap_red.last_native_validation.operation_id == bootstrap_red_operation

	candidate_ready := candidate_with_completed_native_gate()
	candidate_failed_operation := unique_operation(`b`, `4`)
	candidate_failed := bin.transition_target(candidate_ready, .candidate_failed, bin.TransitionContext{
		operation_id: candidate_failed_operation
		red_proof:    red_proof(candidate_ready, 'infrastructure')
	}) or { panic(err) }
	assert candidate_failed.last_native_validation.transition == 'candidate_failed'
	assert candidate_failed.last_native_validation.verdict == 'infrastructure'
	assert candidate_failed.last_native_validation.operation_id == candidate_failed_operation

	remediation_ready, remediation_subject := remediation_validation_ready()
	remediation_green_operation := unique_operation(`b`, `5`)
	remediation_green := bin.transition_target(remediation_ready, .remediation_green, bin.TransitionContext{
		operation_id:       remediation_green_operation
		validation_subject: remediation_subject
		head_observation:   head_observation(remediation_ready, remediation_green_operation,
			remediation_subject.sha, remediation_subject.sha, .exact_subject)
		green_proof:        green_proof(remediation_ready)
	}) or { panic(err) }
	assert remediation_green.last_native_validation.transition == 'remediation_green'
	assert remediation_green.last_native_validation.verdict == 'green'
	assert remediation_green.last_native_validation.operation_id == remediation_green_operation

	remediation_red_operation := unique_operation(`b`, `6`)
	remediation_red := bin.transition_target(remediation_ready, .remediation_red, bin.TransitionContext{
		operation_id:       remediation_red_operation
		validation_subject: remediation_subject
		head_observation:   head_observation(remediation_ready, remediation_red_operation,
			remediation_subject.sha, remediation_subject.sha, .exact_subject)
		red_proof:          red_proof(remediation_ready, 'infrastructure')
	}) or { panic(err) }
	assert remediation_red.last_native_validation.transition == 'remediation_red'
	assert remediation_red.last_native_validation.verdict == 'infrastructure'
	assert remediation_red.last_native_validation.operation_id == remediation_red_operation
}

fn test_reservation_is_non_overwriting_and_idempotent_without_generation_bump() {
	mut target := seeded_target()
	intent_id := operation(`1`)
	intent := reserved_intent(target, 'publish', intent_id, bin.ValidationSubjectModel{})
	context := bin.TransitionContext{
		operation_id: operation(`2`)
		intent:       intent
	}
	reserved := bin.transition_target(target, .reserve_publish, context) or { panic(err) }
	replayed := bin.transition_target(reserved, .reserve_publish, context) or { panic(err) }
	assert replayed.generation == reserved.generation
	assert replayed.active_intent.intent_id == intent_id
	mut rejected := false
	other_intent :=
		reserved_intent(reserved, 'publish', operation(`3`), bin.ValidationSubjectModel{})
	bin.transition_target(reserved, .reserve_publish, bin.TransitionContext{
		operation_id: operation(`4`)
		intent:       other_intent
	}) or { rejected = true }
	assert rejected, 'reservation must never overwrite an active intention'
}

fn test_adoption_and_bootstrap_never_enter_building() {
	mut target := seeded_target()
	descendant := '1234567890abcdef1234567890abcdef12345678'
	intent_id := operation(`1`)
	subject := validation_subject(descendant, state_candidate_tree,
		'tccbin-candidate/linux-amd64/${intent_id}')
	target.canonical_observed_sha = descendant
	intent := reserved_intent(target, 'adopt-current', intent_id, subject)
	native_subject := native_subject_for(target, intent_id, 'adopt_current', subject,

		target.generation + 1)
	target = bin.transition_target(target, .reserve_adopt_current, bin.TransitionContext{
		operation_id:       operation(`2`)
		intent:             intent
		validation_subject: subject
		native_subject:     native_subject
		native_gate:        initial_gate_for(native_subject)
	}) or { panic(err) }
	mut rejected := false
	bin.transition_target(target, .start_build, bin.TransitionContext{ operation_id: operation(`3`) }) or {
		rejected = true
	}
	assert rejected
}

fn candidate_with_completed_native_gate() bin.TargetModel {
	mut target := seeded_target()
	intent_id := operation(`1`)
	intent := reserved_intent(target, 'publish', intent_id, bin.ValidationSubjectModel{})
	target = bin.transition_target(target, .reserve_publish, bin.TransitionContext{
		operation_id: operation(`2`)
		intent:       intent
	}) or { panic(err) }
	target = bin.transition_target(target, .start_build, bin.TransitionContext{
		operation_id: operation(`3`)
	}) or { panic(err) }
	binding := candidate_binding(state_sha)
	candidate_subject := validation_subject(binding.sha, binding.tree, intent.candidate_ref)
	native_subject := native_subject_for(target, intent_id, 'publish_candidate', candidate_subject,

		target.generation + 1)
	target = bin.transition_target(target, .bind_candidate, bin.TransitionContext{
		operation_id:      operation(`4`)
		candidate_binding: binding
		native_subject:    native_subject
		native_gate:       initial_gate_for(native_subject)
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	return target
}

fn checked_candidate() bin.TargetModel {
	mut target := candidate_with_completed_native_gate()
	proof := green_proof(target)
	target = bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: operation(`5`)
		green_proof:  proof
	}) or { panic(err) }
	return target
}

fn checked_adopt_current() bin.TargetModel {
	mut target := seeded_target()
	descendant := '1234567890abcdef1234567890abcdef12345678'
	intent_id := labelled_operation('publisher-class-adopt-current-intent')
	target.canonical_observed_sha = descendant
	subject := validation_subject(descendant, state_candidate_tree,
		'tccbin-candidate/linux-amd64/${intent_id}')
	intent := reserved_intent(target, 'adopt-current', intent_id, subject)
	native_subject := native_subject_for(target, intent_id, 'adopt_current', subject,

		target.generation + 1)
	target = bin.transition_target(target, .reserve_adopt_current, bin.TransitionContext{
		operation_id:       labelled_operation('publisher-class-adopt-current-reserve')
		intent:             intent
		validation_subject: subject
		native_subject:     native_subject
		native_gate:        initial_gate_for(native_subject)
	}) or { panic(err) }
	target = bin.transition_target(target, .bind_candidate, bin.TransitionContext{
		operation_id:       labelled_operation('publisher-class-adopt-current-bind')
		validation_subject: subject
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	target = bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: labelled_operation('publisher-class-adopt-current-green')
		green_proof:  green_proof(target)
	}) or { panic(err) }
	return target
}

fn assert_publisher_preserved_class_rejected(target bin.TargetModel) {
	mut rejected := ''
	bin.validate_target_model(target) or { rejected = err.msg() }
	assert rejected == 'blocked target native validation is outside the closed publisher-preserved or red transition classes'
}

fn published_provisional() bin.TargetModel {
	mut target := checked_candidate()
	intent_id := target.active_intent.intent_id
	binding := target.active_intent.candidate_binding
	mut post_subject := validation_subject(binding.sha, binding.tree,
		'tccbin-candidate/linux-amd64/${intent_id}')
	post_subject = bin.ValidationSubjectModel{
		...post_subject
		candidate_ref: 'thirdparty-linux-amd64'
	}
	post_native_subject := native_subject_for(target, operation(`6`), 'publish_post', post_subject,

		target.generation + 1)
	target = bin.transition_target(target, .promotion_confirmed, bin.TransitionContext{
		operation_id:     operation(`6`)
		head_observation: head_observation(target, operation(`6`), binding.sha, binding.sha,
			.exact_subject)
		native_subject:   post_native_subject
		native_gate:      initial_gate_for(post_native_subject)
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	return target
}

fn test_sealed_matrix_selected_run_subject_and_head_forgery_fail_closed() {
	target := published_provisional()
	operation_id := unique_operation(`7`, `0`)
	observation := head_observation(target, operation_id, target.provisional_published.sha,
		target.provisional_published.sha, .exact_subject)
	proof := green_proof(target)
	forged_output := bin.GreenVerdictProof{
		...proof
		native_gate: bin.PersistedGateRunModel{
			...proof.native_gate
			output_digest: operation(`0`)
		}
	}
	mut rejected := false
	bin.transition_target(target, .post_check_green, bin.TransitionContext{
		operation_id:     operation_id
		head_observation: observation
		green_proof:      forged_output
	}) or { rejected = true }
	assert rejected
	mut forged_target := target
	forged_target.active_native_subject = bin.NativeGateSubjectModel{
		...target.active_native_subject
		consumer_id:            operation(`1`)
		intent_or_operation_id: operation(`1`)
	}
	forged_capsule := state_native_capsule(forged_target, proof.manifest, 'green')
	rejected = false
	bin.transition_target(target, .post_check_green, bin.TransitionContext{
		operation_id:     unique_operation(`7`, `1`)
		head_observation: head_observation(target, unique_operation(`7`, `1`),
			target.provisional_published.sha, target.provisional_published.sha, .exact_subject)
		green_proof:      bin.GreenVerdictProof{
			...proof
			native_capsule: forged_capsule.capsule
		}
	}) or { rejected = true }
	assert rejected
	assert proof.native_gate.run_attempt == 1
	native_run_id_capsule := state_native_capsule_for_run(target, proof.manifest, 'green',

		proof.native_gate.run_id + 1, proof.native_gate.run_attempt,
		proof.native_gate.check_suite_id)
	native_attempt_capsule := state_native_capsule_for_run(target, proof.manifest, 'green',
		proof.native_gate.run_id, 2, proof.native_gate.check_suite_id)
	selected_suffixes := [`2`, `3`]
	for index, selected_capsule in [native_run_id_capsule, native_attempt_capsule] {
		mut selected_rejected := ''
		selected_operation := unique_operation(`7`, selected_suffixes[index])
		bin.transition_target(target, .post_check_green, bin.TransitionContext{
			operation_id:     selected_operation
			head_observation: head_observation(target, selected_operation,
				target.provisional_published.sha, target.provisional_published.sha, .exact_subject)
			green_proof:      bin.GreenVerdictProof{
				...proof
				native_capsule: selected_capsule.capsule
				native_gate:    bin.PersistedGateRunModel{
					...proof.native_gate
					output_digest: selected_capsule.matrix_digest
				}
			}
		}) or { selected_rejected = err.msg() }
		assert selected_rejected == 'native gate proof differs from the sealed matrix output or selected run'
	}
	mut suite_rejected := ''
	bin.transition_target(target, .post_check_green, bin.TransitionContext{
		operation_id:     unique_operation(`7`, `4`)
		head_observation: head_observation(target, unique_operation(`7`, `4`),
			target.provisional_published.sha, target.provisional_published.sha, .exact_subject)
		green_proof:      bin.GreenVerdictProof{
			...proof
			native_gate: bin.PersistedGateRunModel{
				...proof.native_gate
				check_suite_id: proof.native_gate.check_suite_id + 1
			}
		}
	}) or { suite_rejected = err.msg() }
	assert suite_rejected == 'native gate proof differs from the sealed matrix output or selected run'
	rejected = false
	bin.transition_target(target, .post_check_green, bin.TransitionContext{
		operation_id:     unique_operation(`7`, `5`)
		head_observation: head_observation(target, unique_operation(`7`, `5`), state_sha,
			target.provisional_published.sha, .exact_subject)
		green_proof:      proof
	}) or { rejected = true }
	assert rejected
}

fn test_full_gate_run_fields_and_validator_authority_are_not_reduced_to_green_booleans() {
	target := candidate_with_completed_native_gate()
	proof := green_proof(target)
	shape_mutations := [
		bin.PersistedGateRunModel{
			...proof.native_gate
			check_suite_integration_id: 9999
		},
		bin.PersistedGateRunModel{
			...proof.native_gate
			job_id: 0
		},
		bin.PersistedGateRunModel{
			...proof.native_gate
			external_id: operation(`0`)
		},
		bin.PersistedGateRunModel{
			...proof.native_gate
			run_url: 'https://github.com/vlang/tccbin/actions/runs/1'
		},
		bin.PersistedGateRunModel{
			...proof.native_gate
			completed_at: '2026-08-01T00:00:00Z'
		},
		bin.PersistedGateRunModel{
			...proof.native_gate
			evidence_digest: ''
		},
	]
	for index, mutation in shape_mutations {
		mut rejected := ''
		bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
			operation_id: labelled_operation('gate-shape-${index}')
			green_proof:  bin.GreenVerdictProof{
				...proof
				native_gate: mutation
			}
		}) or { rejected = err.msg() }
		assert rejected == 'persisted gate run is not one complete common gate_run'
	}

	mut rejected := ''
	bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: labelled_operation('gate-validator-authority')
		green_proof:  bin.GreenVerdictProof{
			...proof
			v_smoke_gate: bin.PersistedGateRunModel{
				...proof.v_smoke_gate
				actor: 'untrusted[bot]'
			}
		}
	}) or { rejected = err.msg() }
	assert rejected == 'V smoke proof differs from trusted master or its validator authority'

	rejected = ''
	bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: labelled_operation('gate-functional-not-green')
		green_proof:  bin.GreenVerdictProof{
			...proof
			v_smoke_gate: bin.PersistedGateRunModel{
				...proof.v_smoke_gate
				check_conclusion: 'failure'
			}
		}
	}) or { rejected = err.msg() }
	assert rejected == 'green verdict requires a green matrix and two successful run/check gates'

	rejected = ''
	bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: labelled_operation('gate-native-winner-facts')
		green_proof:  bin.GreenVerdictProof{
			...proof
			native_gate: bin.PersistedGateRunModel{
				...proof.native_gate
				actor: 'other-publisher[bot]'
			}
		}
	}) or { rejected = err.msg() }
	assert rejected == 'native proof facts differ from the persisted authenticated gate run'
}

fn test_publisher_failure_requires_an_already_green_matrix_and_two_green_gates() {
	target := checked_candidate()
	prior_gate_runs := target.active_intent.gate_runs.clone()
	prior_validation := target.last_native_validation
	publisher := red_proof(target, 'publisher')
	assert prior_gate_runs != [publisher.native_gate, publisher.v_smoke_gate]
	blocked := bin.transition_target(target, .promotion_failed, bin.TransitionContext{
		operation_id: operation(`6`)
		red_proof:    publisher
	}) or { panic(err) }
	assert blocked.active_intent.stage == 'blocked'
	assert blocked.publication_state == .promotion_blocked
	assert blocked.active_intent.intent_type == 'publish'
	assert blocked.active_native_subject.consumer_kind == 'publish_candidate'
	assert blocked.active_intent.gate_runs == prior_gate_runs
	assert blocked.last_native_validation == prior_validation
	bin.validate_target_model(blocked) or { panic(err) }
	mut crossed_publication := blocked
	crossed_publication.publication_state = .rollback_blocked
	assert_publisher_preserved_class_rejected(crossed_publication)
	mut forged_blocked := blocked
	forged_blocked.active_intent = bin.ActiveIntentModel{
		...blocked.active_intent
		gate_runs: [blocked.active_intent.gate_runs[0], bin.PersistedGateRunModel{
			...blocked.active_intent.gate_runs[1]
			evidence_digest: operation(`9`)
		}]
	}
	mut forged_blocked_rejected := ''
	bin.validate_target_model(forged_blocked) or { forged_blocked_rejected = err.msg() }
	assert forged_blocked_rejected == 'checked candidate differs from its durable native validation record'

	red_matrix := red_proof_with_matrix_outcome(target, 'publisher', 'functional')
	mut rejected := ''
	bin.transition_target(target, .promotion_failed, bin.TransitionContext{
		operation_id: unique_operation(`6`, `1`)
		red_proof:    red_matrix
	}) or { rejected = err.msg() }
	assert rejected == 'publisher failure cannot rewrite already-green lane or gate evidence'
}

fn test_adoption_kinds_cannot_enter_the_preserved_publisher_class() {
	mut initial := bootstrap_validation_ready()
	initial = bin.transition_target(initial, .candidate_checks_green, bin.TransitionContext{
		operation_id: labelled_operation('publisher-class-initial-adopt-green')
		green_proof:  green_proof(initial)
	}) or { panic(err) }
	for checked in [checked_adopt_current(), initial] {
		assert checked.active_native_subject.consumer_kind in ['adopt_current',
			'initial_adopt_current']
		mut rejected := ''
		bin.transition_target(checked, .promotion_failed, bin.TransitionContext{
			operation_id: labelled_operation('publisher-class-${checked.active_native_subject.consumer_kind}-blocked')
			red_proof:    red_proof(checked, 'publisher')
		}) or { rejected = err.msg() }
		assert rejected == 'blocked target native validation is outside the closed publisher-preserved or red transition classes'
	}
}

fn test_functional_matrix_dominates_an_infrastructure_gate_and_fallback_is_red() {
	target := published_provisional()
	rollback_id := unique_operation(`6`, `2`)
	mut rollback := reserved_intent(target, 'rollback', rollback_id, bin.ValidationSubjectModel{})
	rollback = bin.ActiveIntentModel{
		...rollback
		bad_provisional:           target.provisional_published
		rollback_diff_fingerprint: operation(`8`)
	}
	base_proof := red_proof_with_matrix_outcome(target, 'functional', 'fallback')
	mixed := bin.RedVerdictProof{
		...base_proof
		v_smoke_gate: bin.PersistedGateRunModel{
			...base_proof.v_smoke_gate
			run_conclusion: 'timed_out'
		}
	}
	operation_id := unique_operation(`6`, `3`)
	accepted := bin.transition_target(target, .post_check_red, bin.TransitionContext{
		operation_id:     operation_id
		head_observation: head_observation(target, operation_id, target.provisional_published.sha,
			target.provisional_published.sha, .exact_subject)
		intent:           rollback
		red_proof:        mixed
	}) or { panic(err) }
	assert accepted.publication_state == .rollback_pending

	mut rejected := ''
	bin.transition_target(target, .post_check_red, bin.TransitionContext{
		operation_id:     unique_operation(`6`, `4`)
		head_observation: head_observation(target, unique_operation(`6`, `4`),
			target.provisional_published.sha, target.provisional_published.sha, .exact_subject)
		intent:           rollback
		red_proof:        bin.RedVerdictProof{
			...mixed
			failure_kind: 'infrastructure'
		}
	}) or { rejected = err.msg() }
	assert rejected == 'red verdict failure class is not allowed by this transition'

	infrastructure_matrix := red_proof_with_matrix_outcome(target, 'functional', 'infrastructure')
	mismatch_operation_id := unique_operation(`6`, `5`)
	rejected = ''
	bin.transition_target(target, .post_check_red, bin.TransitionContext{
		operation_id:     mismatch_operation_id
		head_observation: head_observation(target, mismatch_operation_id,
			target.provisional_published.sha, target.provisional_published.sha, .exact_subject)
		intent:           rollback
		red_proof:        infrastructure_matrix
	}) or { rejected = err.msg() }
	assert rejected == 'red verdict failure class differs from its authenticated matrix and gates'
}

fn test_durable_intent_gate_runs_are_early_zero_and_terminal_exact_ordered_two() {
	mut collecting := candidate_with_completed_native_gate()
	collecting_proof := green_proof(collecting)
	for stage in ['checks_running', 'checks_waiting_source'] {
		for gates in [
			[]bin.PersistedGateRunModel{},
			[collecting_proof.native_gate],
			[collecting_proof.v_smoke_gate],
			[collecting_proof.native_gate, collecting_proof.v_smoke_gate],
		] {
			mut state := collecting
			state.active_intent = bin.ActiveIntentModel{
				...collecting.active_intent
				stage:     stage
				gate_runs: gates
			}
			bin.validate_target_model(state) or { panic('${stage}/${gates.len}: ${err}') }
		}
	}
	collecting.active_intent = bin.ActiveIntentModel{
		...collecting.active_intent
		stage:     'checks_running'
		gate_runs: [collecting_proof.v_smoke_gate, collecting_proof.native_gate]
	}
	mut rejected := ''
	bin.validate_target_model(collecting) or { rejected = err.msg() }
	assert rejected == 'collecting intention permits at most the ordered native and V smoke proofs'
	forged_subject_hash := operation(`0`)
	forged_smoke := bin.PersistedGateRunModel{
		...collecting_proof.v_smoke_gate
		subject_hash: forged_subject_hash
		external_id:  bin.deterministic_check_external_id('vlang/tccbin:v-smoke-check:v1',
			collecting.active_native_subject.consumer_id, forged_subject_hash,
			collecting_proof.v_smoke_gate.run_id, collecting_proof.v_smoke_gate.run_attempt) or {
			panic(err)
		}
	}
	mut orphaned := candidate_with_completed_native_gate()
	orphaned.active_intent = bin.ActiveIntentModel{
		...orphaned.active_intent
		stage:     'checks_running'
		gate_runs: [forged_smoke]
	}
	rejected = ''
	bin.validate_target_model(orphaned) or { rejected = err.msg() }
	assert rejected == 'collected gate proof is orphaned from the active native subject'

	terminal := published_provisional()
	assert terminal.active_intent.gate_runs.len == 2
	assert terminal.active_intent.gate_runs[0].check_name == 'tccbin-candidate-gate'
	assert terminal.active_intent.gate_runs[1].check_name == 'v-candidate-smoke'
	bin.validate_target_model(terminal) or { panic(err) }

	mut missing := terminal
	missing.active_intent = bin.ActiveIntentModel{
		...terminal.active_intent
		gate_runs: [terminal.active_intent.gate_runs[0]]
	}
	rejected = ''
	bin.validate_target_model(missing) or { rejected = err.msg() }
	assert rejected == 'terminal intention requires exactly the ordered native and V smoke proofs'

	mut reversed := terminal
	reversed.active_intent = bin.ActiveIntentModel{
		...terminal.active_intent
		gate_runs: [terminal.active_intent.gate_runs[1], terminal.active_intent.gate_runs[0]]
	}
	rejected = ''
	bin.validate_target_model(reversed) or { rejected = err.msg() }
	assert rejected == 'terminal intention requires exactly the ordered native and V smoke proofs'

	mut incomplete := terminal
	incomplete.active_intent = bin.ActiveIntentModel{
		...terminal.active_intent
		gate_runs: [
			bin.PersistedGateRunModel{
				...terminal.active_intent.gate_runs[0]
				job_url: 'https://github.com/vlang/tccbin/actions/runs/1/job/1'
			},
			terminal.active_intent.gate_runs[1],
		]
	}
	rejected = ''
	bin.validate_target_model(incomplete) or { rejected = err.msg() }
	assert rejected == 'persisted gate run is not one complete common gate_run'

	mut non_green := terminal
	non_green.active_intent = bin.ActiveIntentModel{
		...terminal.active_intent
		gate_runs: [
			bin.PersistedGateRunModel{
				...terminal.active_intent.gate_runs[0]
				run_conclusion: 'failure'
			},
			terminal.active_intent.gate_runs[1],
		]
	}
	rejected = ''
	bin.validate_target_model(non_green) or { rejected = err.msg() }
	assert rejected == 'non-blocked terminal intention requires two green gate proofs'

	mut early := seeded_target()
	early_intent_id := unique_operation(`6`, `5`)
	early = bin.transition_target(early, .reserve_publish, bin.TransitionContext{
		operation_id: unique_operation(`6`, `6`)
		intent:       reserved_intent(early, 'publish', early_intent_id, bin.ValidationSubjectModel{})
	}) or { panic(err) }
	early.active_intent = bin.ActiveIntentModel{
		...early.active_intent
		gate_runs: terminal.active_intent.gate_runs
	}
	rejected = ''
	bin.validate_target_model(early) or { rejected = err.msg() }
	assert rejected == 'pre-check intention cannot retain collected terminal gate proofs'

	for stage in ['aborted', 'superseded'] {
		for gates in [[terminal.active_intent.gate_runs[1]], terminal.active_intent.gate_runs] {
			mut retained := early
			retained.active_intent = bin.ActiveIntentModel{
				...early.active_intent
				stage:             stage
				candidate_binding: bin.CandidateBindingModel{}
				gate_runs:         gates
			}
			bin.validate_target_model(retained) or { panic('${stage}/${gates.len}: ${err}') }
		}
	}
}

fn test_post_infrastructure_block_retains_exact_consumer_and_blocks_publication() {
	mut target := published_provisional()
	preserved_candidate_validation := target.last_native_validation
	preserved_candidate_gates := target.active_intent.gate_runs.clone()
	post_operation_id := target.post_validation_operation_id
	subject_hash := target.active_subject_hash
	operation_id := unique_operation(`8`, `0`)
	target = bin.transition_target(target, .post_check_infra_exhausted, bin.TransitionContext{
		operation_id:     operation_id
		head_observation: head_observation(target, operation_id, target.provisional_published.sha,
			target.provisional_published.sha, .exact_subject)
		red_proof:        red_proof(target, 'infrastructure')
	}) or { panic(err) }
	assert target.publication_state == .post_publish_blocked
	assert target.target_state == .quarantined
	assert target.post_validation_operation_id == post_operation_id
	assert target.active_subject_hash == subject_hash
	assert target.active_intent.stage == 'blocked'
	assert target.last_native_validation.transition == 'post_check_infra_exhausted'
	assert target.last_native_validation.verdict == 'infrastructure'
	assert target.last_native_validation.operation_id == operation_id
	assert_blocked_red_gate_join_is_enforced(target)
	mut forged_history := target
	forged_history.last_native_validation = preserved_candidate_validation
	forged_history.active_intent = bin.ActiveIntentModel{
		...target.active_intent
		gate_runs: preserved_candidate_gates
	}
	assert forged_history.active_native_subject.consumer_kind == 'publish_post'
	mut forged_history_rejected := ''
	bin.validate_target_model(forged_history) or { forged_history_rejected = err.msg() }
	assert forged_history_rejected == 'blocked target native validation is outside the closed publisher-preserved or red transition classes'
	assert !bin.can_begin_normal_publication(target)
	mut rejected := false
	bin.transition_target(target, .reserve_publish, bin.TransitionContext{
		operation_id: unique_operation(`8`, `1`)
		intent:       reserved_intent(target, 'publish', unique_operation(`8`, `2`), bin.ValidationSubjectModel{})
	}) or { rejected = true }
	assert rejected
	green_operation := unique_operation(`8`, `3`)
	target = bin.transition_target(target, .post_check_green, bin.TransitionContext{
		operation_id:     green_operation
		head_observation: head_observation(target, green_operation,
			target.provisional_published.sha, target.provisional_published.sha, .exact_subject)
		green_proof:      green_proof(target)
	}) or { panic(err) }
	assert target.target_state == .eligible
	assert target.publication_state == .idle
	assert target.last_known_good.sha == state_candidate_sha
	assert target.active_subject_hash == ''
	assert target.last_native_validation.transition == 'post_check_green'
	assert target.last_native_validation.verdict == 'green'
	assert target.last_native_validation.operation_id == green_operation
}

fn test_rollback_only_targets_exact_bad_head_and_preserves_prior_good() {
	mut target := published_provisional()
	prior := target.last_known_good
	bad := target.provisional_published
	blocked_operation := unique_operation(`9`, `0`)
	target = bin.transition_target(target, .post_check_infra_exhausted, bin.TransitionContext{
		operation_id:     blocked_operation
		head_observation: head_observation(target, blocked_operation, bad.sha, bad.sha,
			.exact_subject)
		red_proof:        red_proof(target, 'infrastructure')
	}) or { panic(err) }
	assert target.publication_state == .post_publish_blocked
	rollback_id := operation(`7`)
	mut rollback := reserved_intent(target, 'rollback', rollback_id, bin.ValidationSubjectModel{})
	rollback = bin.ActiveIntentModel{
		...rollback
		bad_provisional:           bad
		rollback_diff_fingerprint: operation(`8`)
	}
	post_red := red_proof(target, 'functional')
	target = bin.transition_target(target, .post_check_red, bin.TransitionContext{
		operation_id:     operation(`9`)
		head_observation: head_observation(target, operation(`9`), bad.sha, bad.sha, .exact_subject)
		intent:           rollback
		red_proof:        post_red
	}) or { panic(err) }
	assert target.publication_state == .rollback_pending
	assert target.last_known_good == prior
	assert target.active_intent.bad_provisional == bad
	assert target.last_native_validation.transition == 'post_check_red'
	assert target.last_native_validation.verdict == 'functional'
	assert target.last_native_validation.operation_id == operation(`9`)
	target = bin.transition_target(target, .start_build, bin.TransitionContext{
		operation_id: unique_operation(`a`, `0`)
	}) or { panic(err) }
	revert := candidate_binding_with_sha(bad.sha, '0123456789abcdef0123456789abcdef01234567',
		state_tree)
	revert_subject := validation_subject(revert.sha, revert.tree,
		target.active_intent.candidate_ref)
	revert_native := native_subject_for(target, rollback_id, 'rollback_candidate', revert_subject,

		target.generation + 1)
	target = bin.transition_target(target, .bind_candidate, bin.TransitionContext{
		operation_id:      unique_operation(`a`, `1`)
		candidate_binding: revert
		native_subject:    revert_native
		native_gate:       initial_gate_for(revert_native)
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	candidate_red_blocked := bin.transition_target(target, .candidate_failed, bin.TransitionContext{
		operation_id: unique_operation(`a`, `7`)
		red_proof:    red_proof(target, 'functional')
	}) or { panic(err) }
	assert candidate_red_blocked.publication_state == .rollback_blocked
	assert candidate_red_blocked.last_native_validation.transition == 'candidate_failed'
	assert_blocked_red_gate_join_is_enforced(candidate_red_blocked)
	target = bin.transition_target(target, .candidate_checks_green, bin.TransitionContext{
		operation_id: unique_operation(`a`, `2`)
		green_proof:  green_proof(target)
	}) or { panic(err) }
	checked_rollback_validation := target.last_native_validation
	publisher_blocked := bin.transition_target(target, .rollback_failed, bin.TransitionContext{
		operation_id: unique_operation(`a`, `5`)
		red_proof:    red_proof(target, 'publisher')
	}) or { panic(err) }
	assert publisher_blocked.last_native_validation == checked_rollback_validation
	assert publisher_blocked.publication_state == .rollback_blocked
	assert publisher_blocked.active_intent.intent_type == 'rollback'
	assert publisher_blocked.active_native_subject.consumer_kind == 'rollback_candidate'
	bin.validate_target_model(publisher_blocked) or { panic(err) }
	mut crossed_rollback_publication := publisher_blocked
	crossed_rollback_publication.publication_state = .promotion_blocked
	assert_publisher_preserved_class_rejected(crossed_rollback_publication)
	functional_blocked := bin.transition_target(target, .rollback_failed, bin.TransitionContext{
		operation_id: unique_operation(`a`, `6`)
		red_proof:    red_proof(target, 'functional')
	}) or { panic(err) }
	assert functional_blocked.last_native_validation != checked_rollback_validation
	assert functional_blocked.last_native_validation.transition == 'rollback_failed'
	assert functional_blocked.last_native_validation.verdict == 'functional'
	assert_blocked_red_gate_join_is_enforced(functional_blocked)
	rollback_post_operation := unique_operation(`a`, `3`)
	mut rollback_post_subject := validation_subject(revert.sha, revert.tree,
		target.active_intent.candidate_ref)
	rollback_post_subject = bin.ValidationSubjectModel{
		...rollback_post_subject
		candidate_ref: 'thirdparty-linux-amd64'
	}
	rollback_post_native := native_subject_for(target, rollback_post_operation, 'rollback_post',
		rollback_post_subject, target.generation + 1)
	target = bin.transition_target(target, .rollback_promoted, bin.TransitionContext{
		operation_id:     rollback_post_operation
		head_observation: head_observation(target, rollback_post_operation, revert.sha, revert.sha,
			.exact_subject)
		native_subject:   rollback_post_native
		native_gate:      initial_gate_for(rollback_post_native)
	}) or { panic(err) }
	target = complete_active_native_gate(target, 'success')
	rollback_post_candidate_validation := target.last_native_validation
	rollback_post_publisher_blocked := bin.transition_target(target, .rollback_failed, bin.TransitionContext{
		operation_id: unique_operation(`a`, `9`)
		red_proof:    red_proof(target, 'publisher')
	}) or { panic(err) }
	assert rollback_post_publisher_blocked.last_native_validation == rollback_post_candidate_validation
	assert rollback_post_publisher_blocked.publication_state == .rollback_blocked
	assert rollback_post_publisher_blocked.active_intent.intent_type == 'rollback'
	assert rollback_post_publisher_blocked.active_native_subject.consumer_kind == 'rollback_post'
	bin.validate_target_model(rollback_post_publisher_blocked) or { panic(err) }
	rollback_post_failed := bin.transition_target(target, .rollback_failed, bin.TransitionContext{
		operation_id: unique_operation(`a`, `8`)
		red_proof:    red_proof(target, 'infrastructure')
	}) or { panic(err) }
	assert rollback_post_failed.last_native_validation.transition == 'rollback_failed'
	assert rollback_post_failed.last_native_validation.verdict == 'infrastructure'
	rollback_post_matrix_subject := rollback_post_failed.last_native_validation.native_lane_matrix.object_value('subject') or {
		panic('rollback post subject missing')
	}
	rollback_post_kind := rollback_post_matrix_subject.object_value('consumer_kind') or {
		panic('rollback post kind missing')
	}
	assert rollback_post_kind.string_value == 'rollback_post'
	assert_blocked_red_gate_join_is_enforced(rollback_post_failed)
	rollback_green_operation := unique_operation(`a`, `4`)
	target = bin.transition_target(target, .rollback_post_green, bin.TransitionContext{
		operation_id:     rollback_green_operation
		head_observation: head_observation(target, rollback_green_operation, revert.sha,
			revert.sha, .exact_subject)
		green_proof:      green_proof(target)
	}) or { panic(err) }
	assert target.publication_state == .restored_last_known_good
	assert target.last_known_good.sha == revert.sha
	assert target.provisional_published.sha == ''
	assert target.post_validation_operation_id == ''
	assert target.active_subject_hash == ''
	assert target.last_native_validation.transition == 'rollback_post_green'
	assert target.last_native_validation.verdict == 'green'
	assert target.last_native_validation.operation_id == rollback_green_operation
}

fn test_human_descendant_is_adopted_and_never_blindly_rolled_back() {
	mut target := published_provisional()
	descendant := '1234567890abcdef1234567890abcdef12345678'
	intent_id := operation(`7`)
	mut observed := target
	observed.canonical_observed_sha = descendant
	subject := validation_subject(descendant, state_candidate_tree,
		'tccbin-candidate/linux-amd64/${intent_id}')
	adopt := reserved_intent(observed, 'adopt-current', intent_id, subject)
	adopt_native := native_subject_for(observed, intent_id, 'adopt_current', subject,

		target.generation + 1)
	target = bin.transition_target(target, .post_check_red, bin.TransitionContext{
		operation_id:       operation(`8`)
		head_observation:   head_observation(target, operation(`8`), descendant,
			target.provisional_published.sha, .subject_ancestor)
		intent:             adopt
		validation_subject: subject
		native_subject:     adopt_native
		native_gate:        initial_gate_for(adopt_native)
		red_proof:          red_proof(target, 'functional')
	}) or { panic(err) }
	assert target.active_intent.intent_type == 'adopt-current'
	assert target.publication_state == .candidate_pending
	assert target.provisional_published.sha == state_candidate_sha
}

fn test_source_refetch_is_bound_to_source_state_intent_and_operation() {
	mut target := seeded_target()
	intent_id := unique_operation(`d`, `0`)
	target = bin.transition_target(target, .reserve_publish, bin.TransitionContext{
		operation_id: unique_operation(`d`, `1`)
		intent:       reserved_intent(target, 'publish', intent_id, bin.ValidationSubjectModel{})
	}) or { panic(err) }
	target = bin.transition_target(target, .start_build, bin.TransitionContext{
		operation_id: unique_operation(`d`, `2`)
	}) or { panic(err) }
	tinycc_sources := target.active_intent.resolved_inputs.sources.filter(it.id == 'tinycc')
	assert tinycc_sources.len == 1
	tinycc_source := tinycc_sources[0]
	tinycc_source_checks :=
		target.active_intent.resolved_inputs.source_checks.filter(it.source_id == tinycc_source.id)
	assert tinycc_source_checks.len == 1
	assert tinycc_source_checks[0].source_id == tinycc_source.id
	assert tinycc_source_checks[0].resolved_sha == tinycc_source.sha
	fingerprints := state_fingerprints()
	source_base := bin.initial_source_state('${tinycc_source.id}-${tinycc_source.ref}',
		tinycc_source.repository, tinycc_source.ref, fingerprints.input_fingerprint,
		'2026-08-01T03:47:00Z') or { panic(err) }
	resolution_operation := unique_operation(`d`, `3`)
	source_outage := bin.resolve_source(source_base, source_base.generation, resolution_operation,
		100, '2026-08-02T03:47:00Z', [intent_id], [
		transient_source_attempt(1, 0),
		transient_source_attempt(2, 15),
		transient_source_attempt(3, 45),
	]) or { panic(err) }
	transition_operation := unique_operation(`d`, `4`)
	refetch := bin.SourceRefetchModel{
		target_id:               target.target_id
		expected_generation:     target.generation
		expected_canonical_head: target.canonical_observed_sha
		source_state_id:         source_outage.state.source_id
		source_state_generation: source_outage.state.generation
		resolution_operation_id: resolution_operation
		source_id:               tinycc_source.id
		source_repository:       tinycc_source.repository
		requested_ref:           tinycc_source.ref
		previous_sha:            tinycc_source.sha
		status:                  'unreachable'
		failure_kind:            'timeout'
		evidence_digest:         unique_operation(`d`, `5`)
		input_fingerprint:       target.input_fingerprint
		checked_at:              source_outage.state.last_attempt_at
		operation_id:            transition_operation
	}
	mut rejected := false
	bin.transition_target(target, .source_unreachable, bin.TransitionContext{
		operation_id:   transition_operation
		source_state:   source_outage.state
		source_refetch: bin.SourceRefetchModel{
			...refetch
			source_state_generation: source_outage.state.generation - 1
		}
	}) or { rejected = true }
	assert rejected
	rejected = false
	bin.transition_target(target, .source_unreachable, bin.TransitionContext{
		operation_id:   transition_operation
		source_state:   source_outage.state
		source_refetch: bin.SourceRefetchModel{
			...refetch
			resolution_operation_id: unique_operation(`d`, `6`)
		}
	}) or { rejected = true }
	assert rejected
	waiting := bin.transition_target(target, .source_unreachable, bin.TransitionContext{
		operation_id:   transition_operation
		source_state:   source_outage.state
		source_refetch: refetch
	}) or { panic(err) }
	assert waiting.active_intent.intent_id == intent_id
	assert waiting.active_intent.stage == 'build_waiting_source'
	resolved_operation := unique_operation(`d`, `7`)
	source_resolved := bin.resolve_source(source_outage.state, source_outage.state.generation,
		resolved_operation, 100, '2026-08-03T03:47:00Z', [intent_id], [
		bin.SourceResolutionAttempt{
			ordinal:                 1
			backoff_seconds:         0
			connect_timeout_seconds: bin.source_connect_timeout_seconds
			total_timeout_seconds:   bin.source_total_timeout_seconds
			resolved_sha:            state_source_sha
			resolved_tree:           state_source_tree
		},
	]) or { panic(err) }
	restore_operation := unique_operation(`d`, `8`)
	restored := bin.transition_target(waiting, .source_restored, bin.TransitionContext{
		operation_id:   restore_operation
		source_state:   source_resolved.state
		source_refetch: bin.SourceRefetchModel{
			...refetch
			expected_generation:     waiting.generation
			source_state_generation: source_resolved.state.generation
			resolution_operation_id: resolved_operation
			resolved_sha:            state_source_sha
			resolved_tree:           state_source_tree
			status:                  'resolved'
			failure_kind:            ''
			checked_at:              source_resolved.state.last_attempt_at
			operation_id:            restore_operation
		}
	}) or { panic(err) }
	assert restored.active_intent.intent_id == intent_id
	assert restored.active_intent.stage == 'building'
}

fn test_operation_identity_is_injective_over_head_source_and_full_subject() {
	fingerprints := state_fingerprints()
	base := bin.OperationIdentityInput{
		audience:                'vlang/v:tccbin-automation-state'
		run_id:                  10
		run_attempt:             1
		ordinal:                 0
		cas_attempt:             1
		subject_id:              'linux-amd64'
		transition:              'quarantine'
		expected_generation:     2
		expected_canonical_head: state_sha
		source_ref:              'mob'
		source_sha:              state_source_sha
		subject_fingerprint:     fingerprints.input_fingerprint
		input_fingerprint:       fingerprints.input_fingerprint
		artifact_fingerprint:    fingerprints.artifact_fingerprint
		manifest_hash:           fingerprints.manifest_hash
		native_subject_hash:     state_subject_hash
		intent_id:               operation(`1`)
	}
	base_id := bin.deterministic_operation_id(base) or { panic(err) }
	mutations := [
		bin.OperationIdentityInput{
			...base
			cas_attempt: 2
		},
		bin.OperationIdentityInput{
			...base
			expected_canonical_head: state_candidate_sha
		},
		bin.OperationIdentityInput{
			...base
			source_sha: state_candidate_sha
		},
		bin.OperationIdentityInput{
			...base
			source_ref: 'master'
		},
		bin.OperationIdentityInput{
			...base
			artifact_fingerprint: operation(`6`)
		},
		bin.OperationIdentityInput{
			...base
			manifest_hash: operation(`7`)
		},
		bin.OperationIdentityInput{
			...base
			native_subject_hash: operation(`8`)
		},
	]
	for mutation in mutations {
		assert bin.deterministic_operation_id(mutation) or { panic(err) } != base_id
	}
	path := bin.evidence_path(2026, 8, 10, 1, 'linux-amd64', base_id, 2, 'quarantine',
		fingerprints.input_fingerprint) or { panic(err) }
	assert path.starts_with('evidence/2026/08/10/1/linux-amd64/')
	for invalid in ['.', '..', '...'] {
		mut rejected := false
		bin.evidence_path(2026, 8, 10, 1, invalid, base_id, 2, 'quarantine',
			fingerprints.input_fingerprint) or { rejected = true }
		assert rejected
	}
}
