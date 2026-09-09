module bin

import crypto.sha256
import os

const durable_target_authority_tree_record_max_bytes = 4096

// ReauthenticatedPreparedTargetStateWrite is a read-only observation of two matching local
// authentication passes. Its fields are deliberately private. Copies returned by the getters are
// not capabilities: callers can forge or alter them and no production consumer accepts them.
pub struct ReauthenticatedPreparedTargetStateWrite {
	proof    LiveStateCommitProof
	prepared PreparedTargetStateWrite
}

// state_proof returns a detached observation of the authenticated local commit proof.
pub fn (value ReauthenticatedPreparedTargetStateWrite) state_proof() LiveStateCommitProof {
	return clone_live_state_commit_proof(value.proof)
}

// prepared_write returns a detached observation of the deterministic target-state bytes.
pub fn (value ReauthenticatedPreparedTargetStateWrite) prepared_write() PreparedTargetStateWrite {
	return clone_prepared_target_state_write(value.prepared)
}

struct ReauthenticatedTargetTreeEntry {
	mode string
	kind string
	oid  string
	size i64
	path string
}

struct ReauthenticatedTargetStateObservation {
	proof         LiveStateCommitProof
	entry         ReauthenticatedTargetTreeEntry
	target_id     string
	source        string
	source_sha256 string
	schema_sha256 string
	generation    i64
	root          JsonValue
	model         TargetModel
}

// prepare_reauthenticated_target_state_transition authenticates the target predecessor twice from
// the local bare state repository and closed public-proof bundle before returning a dormant
// observation. It performs no network request, filesystem write, Git object/ref mutation, or CAS.
pub fn prepare_reauthenticated_target_state_transition(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, context TransitionContext) !ReauthenticatedPreparedTargetStateWrite {
	return prepare_reauthenticated_target_state_transition_core(automation_root, state_git_dir,
		trust, proof_bundle_dir, target_id, event, context, '')
}

$if test {
	// This test-only seam mutates only the already authenticated second in-memory observation. It
	// cannot inject bytes, paths, digests, generations, or preconditions into production.
	pub fn prepare_reauthenticated_target_state_transition_with_mutation_for_test(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, context TransitionContext,
	mutation string) !ReauthenticatedPreparedTargetStateWrite {
		return prepare_reauthenticated_target_state_transition_core(automation_root, state_git_dir,
			trust, proof_bundle_dir, target_id, event, context, mutation)
	}

	// This seam loads two independently valid physical snapshots before applying the production
	// comparator. comparison_focus only aligns earlier comparator fields after both fresh loads.
	pub fn prepare_reauthenticated_target_state_transition_from_distinct_snapshots_for_test(automation_root string,
	first_state_git_dir string, first_proof_bundle_dir string, second_state_git_dir string,
	second_proof_bundle_dir string, trust LiveStateTrust, target_id string, event TransitionEvent,
	context TransitionContext, comparison_focus string) !ReauthenticatedPreparedTargetStateWrite {
		first := load_reauthenticated_target_state_observation(automation_root,
			first_state_git_dir, trust, first_proof_bundle_dir, target_id)!
		prepared := prepare_reauthenticated_target_state_write(first, automation_root, event,
			context)!
		second_fresh := load_reauthenticated_target_state_observation(automation_root,
			second_state_git_dir, trust, second_proof_bundle_dir, target_id)!
		second :=
			align_distinct_snapshot_comparison_for_test(first, second_fresh, comparison_focus)!
		return finish_reauthenticated_target_state_transition(first, second, prepared, target_id,
			event, context)
	}

	// This seam mutates one prepared observation only after both physical authentication passes.
	pub fn prepare_reauthenticated_target_state_transition_with_prepared_mutation_for_test(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, context TransitionContext,
	mutation string) !ReauthenticatedPreparedTargetStateWrite {
		first := load_reauthenticated_target_state_observation(automation_root, state_git_dir,
			trust, proof_bundle_dir, target_id)!
		prepared_fresh := prepare_reauthenticated_target_state_write(first, automation_root, event,
			context)!
		prepared := mutate_reauthenticated_prepared_write_for_test(prepared_fresh, mutation)!
		second := load_reauthenticated_target_state_observation(automation_root, state_git_dir,
			trust, proof_bundle_dir, target_id)!
		return finish_reauthenticated_target_state_transition(first, second, prepared, target_id,
			event, context)
	}

	// This pure framing seam exposes no repository, bytes loader, or production capability.
	pub fn parse_reauthenticated_target_tree_entry_for_test(source string,
	target_path string) !string {
		entry := parse_reauthenticated_target_tree_entry(source, target_path)!
		return '${entry.mode} ${entry.kind} ${entry.oid} ${entry.size}\t${entry.path}'
	}
}

fn prepare_reauthenticated_target_state_transition_core(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, context TransitionContext,
	test_mutation string) !ReauthenticatedPreparedTargetStateWrite {
	first := load_reauthenticated_target_state_observation(automation_root, state_git_dir, trust,
		proof_bundle_dir, target_id)!
	prepared := prepare_reauthenticated_target_state_write(first, automation_root, event, context)!
	second_fresh := load_reauthenticated_target_state_observation(automation_root, state_git_dir,
		trust, proof_bundle_dir, target_id)!
	second := mutate_reauthenticated_observation_for_test(second_fresh, test_mutation)!
	return finish_reauthenticated_target_state_transition(first, second, prepared, target_id,
		event, context)
}

fn prepare_reauthenticated_target_state_write(observation ReauthenticatedTargetStateObservation,
	automation_root string, event TransitionEvent,
	context TransitionContext) !PreparedTargetStateWrite {
	return prepare_target_state_transition(automation_root, observation.source, TargetStateWritePreconditions{
		target_id:               observation.target_id
		expected_generation:     observation.generation
		expected_blob_oid:       observation.entry.oid
		expected_source_sha256:  observation.source_sha256
		expected_state_head_oid: observation.proof.commit_sha
	}, event, context)!
}

fn finish_reauthenticated_target_state_transition(first ReauthenticatedTargetStateObservation,
	second ReauthenticatedTargetStateObservation, prepared PreparedTargetStateWrite,
	target_id string, event TransitionEvent,
	context TransitionContext) !ReauthenticatedPreparedTargetStateWrite {
	validate_reauthenticated_observations_match(first, second)!
	validate_reauthenticated_prepared_binding(first, prepared, target_id, event, context)!
	validate_reauthenticated_prepared_binding(second, prepared, target_id, event, context)!
	return ReauthenticatedPreparedTargetStateWrite{
		proof:    clone_live_state_commit_proof(first.proof)
		prepared: clone_prepared_target_state_write(prepared)
	}
}

fn load_reauthenticated_target_state_observation(automation_root string, state_git_dir string,
	trust LiveStateTrust, proof_bundle_dir string,
	target_id string) !ReauthenticatedTargetStateObservation {
	target_path := target_state_path(target_id)!
	schema_path := os.join_path(automation_root, 'schemas', 'target-state.schema.json')
	schema_before := sha256.sum256(os.read_bytes(schema_path)!).hex()
	proof_bundle := authenticate_live_state_proof_bundle(state_git_dir, trust, proof_bundle_dir)!
	inventory := load_live_state_inventory(automation_root, state_git_dir, trust, proof_bundle)!
	entry := read_reauthenticated_target_tree_entry(state_git_dir, proof_bundle.head.tree_sha,
		target_path)!
	source := inventory.blobs[target_path] or {
		return error('reauthenticated target-state path is absent from the complete inventory')
	}
	if i64(source.len) != entry.size {
		return error('reauthenticated target-state bytes differ from their tree entry size')
	}
	observed_oid := git_blob_oid(source.bytes())
	if observed_oid != entry.oid {
		return error('reauthenticated target-state bytes differ from their tree blob OID')
	}
	source_sha256 := sha256.sum256(source.bytes()).hex()
	durable := load_durable_target_root(automation_root, source)!
	if durable.model.target_id != target_id {
		return error('reauthenticated target-state model differs from its derived path identity')
	}
	schema_after := sha256.sum256(os.read_bytes(schema_path)!).hex()
	if schema_before != schema_after {
		return error('target-state schema changed during one authentication pass')
	}
	return ReauthenticatedTargetStateObservation{
		proof:         clone_live_state_commit_proof(proof_bundle.head)
		entry:         entry
		target_id:     target_id
		source:        source
		source_sha256: source_sha256
		schema_sha256: schema_before
		generation:    durable.model.generation
		root:          durable.root
		model:         durable.model
	}
}

fn read_reauthenticated_target_tree_entry(state_git_dir string, tree_sha string,
	target_path string) !ReauthenticatedTargetTreeEntry {
	if !is_lower_hex_40(tree_sha) || !contract_relative_path_is_safe(target_path)
		|| !target_path.starts_with('targets/') {
		return error('reauthenticated target-state tree selector is malformed')
	}
	result := live_git(state_git_dir, 'ls-tree -l -z --full-tree ${tree_sha} -- ${target_path}')
	if result.exit_code != 0 || result.output.len > durable_target_authority_tree_record_max_bytes {
		return error('reauthenticated target-state tree entry cannot be read inside its bound')
	}
	return parse_reauthenticated_target_tree_entry(result.output, target_path)
}

fn parse_reauthenticated_target_tree_entry(source string,
	target_path string) !ReauthenticatedTargetTreeEntry {
	records := source.split('\x00')
	if records.len != 2 || records[0] == '' || records[1] != '' {
		return error('reauthenticated target-state tree lookup is not one exact NUL-terminated entry')
	}
	parts := records[0].split_nth('\t', 2)
	if parts.len != 2 {
		return error('reauthenticated target-state tree entry is malformed')
	}
	metadata := parts[0].fields()
	if metadata.len != 4 {
		return error('reauthenticated target-state tree metadata is malformed')
	}
	size_source := metadata[3]
	size := size_source.i64()
	if metadata[0] != '100644' {
		return error('reauthenticated target-state tree mode is not 100644')
	}
	if metadata[1] != 'blob' {
		return error('reauthenticated target-state tree type is not blob')
	}
	if !is_lower_hex_40(metadata[2]) {
		return error('reauthenticated target-state tree blob OID is not lowercase-40')
	}
	if size <= 0 || size_source != size.str() || size > i64(live_target_state_max_bytes) {
		return error('reauthenticated target-state tree size is not one canonical positive bounded decimal')
	}
	if parts[1] != target_path {
		return error('reauthenticated target-state tree path differs from its derived target')
	}
	return ReauthenticatedTargetTreeEntry{
		mode: metadata[0]
		kind: metadata[1]
		oid:  metadata[2]
		size: size
		path: parts[1]
	}
}

fn validate_reauthenticated_observations_match(first ReauthenticatedTargetStateObservation,
	second ReauthenticatedTargetStateObservation) ! {
	if first.proof.repository != second.proof.repository {
		return error('reauthenticated state proof repository changed between passes')
	}
	if first.proof.ref != second.proof.ref {
		return error('reauthenticated state proof ref changed between passes')
	}
	if first.proof.commit_sha != second.proof.commit_sha
		|| first.proof.remote_head != second.proof.remote_head {
		return error('reauthenticated state proof HEAD changed between passes')
	}
	if first.proof.tree_sha != second.proof.tree_sha {
		return error('reauthenticated state proof tree changed between passes')
	}
	if first.proof.parent_shas != second.proof.parent_shas {
		return error('reauthenticated state proof parent tuple changed between passes')
	}
	if first.proof.verification_verified != second.proof.verification_verified
		|| first.proof.verification_reason != second.proof.verification_reason {
		return error('reauthenticated state proof signature changed between passes')
	}
	if first.proof.verified_at != second.proof.verified_at {
		return error('reauthenticated state proof verification time changed between passes')
	}
	if first.proof.state_writer_app_id != second.proof.state_writer_app_id {
		return error('reauthenticated state proof App changed between passes')
	}
	if first.proof.actor_login != second.proof.actor_login
		|| first.proof.actor_node_id != second.proof.actor_node_id
		|| first.proof.actor_database_id != second.proof.actor_database_id
		|| first.proof.actor_type != second.proof.actor_type {
		return error('reauthenticated state proof actor changed between passes')
	}
	if first.entry.mode != second.entry.mode {
		return error('reauthenticated target-state tree mode changed between passes')
	}
	if first.entry.kind != second.entry.kind {
		return error('reauthenticated target-state tree type changed between passes')
	}
	if first.entry.oid != second.entry.oid {
		return error('reauthenticated target-state tree blob OID changed between passes')
	}
	if first.entry.size != second.entry.size {
		return error('reauthenticated target-state tree size changed between passes')
	}
	if first.entry.path != second.entry.path {
		return error('reauthenticated target-state tree path changed between passes')
	}
	if first.source != second.source {
		return error('reauthenticated target-state bytes changed between passes')
	}
	if first.source_sha256 != second.source_sha256 {
		return error('reauthenticated target-state SHA-256 changed between passes')
	}
	if first.schema_sha256 != second.schema_sha256 {
		return error('reauthenticated target-state schema changed between passes')
	}
	if first.target_id != second.target_id {
		return error('reauthenticated target identity changed between passes')
	}
	if first.generation != second.generation {
		return error('reauthenticated target generation changed between passes')
	}
	if first.model != second.model || !json_equal(first.root, second.root) {
		return error('reauthenticated target semantic projection changed between passes')
	}
}

fn validate_reauthenticated_prepared_binding(observation ReauthenticatedTargetStateObservation,
	prepared PreparedTargetStateWrite, target_id string, event TransitionEvent,
	context TransitionContext) ! {
	if prepared.target_id != target_id || prepared.target_id != observation.target_id
		|| prepared.target_path != observation.entry.path {
		return error('prepared target-state identity differs from its reauthenticated predecessor')
	}
	if prepared.transition != event.str() || prepared.operation_id != context.operation_id {
		return error('prepared target-state transition differs from its typed request')
	}
	if prepared.expected_generation != observation.generation
		|| prepared.resulting_generation != observation.generation + 1 {
		return error('prepared target-state generation differs from its reauthenticated predecessor')
	}
	if prepared.expected_state_head_oid != observation.proof.commit_sha
		|| prepared.predecessor_blob_oid != observation.entry.oid
		|| prepared.predecessor_source_sha256 != observation.source_sha256 {
		return error('prepared target-state preconditions differ from reauthenticated Git bytes')
	}
	if prepared.source.len == 0 || prepared.source.len > durable_target_max_bytes
		|| prepared.resulting_blob_oid != git_blob_oid(prepared.source.bytes())
		|| prepared.resulting_source_sha256 != sha256.sum256(prepared.source.bytes()).hex() {
		return error('prepared target-state result differs from its content identities')
	}
	result_root := parse_strict_json(prepared.source)!
	expected_changed_members := durable_target_changed_members(observation.root, result_root)!
	if prepared.changed_members != expected_changed_members {
		return error('prepared target-state changed-member set differs from its exact roots')
	}
}

fn clone_live_state_commit_proof(value LiveStateCommitProof) LiveStateCommitProof {
	return LiveStateCommitProof{
		...value
		parent_shas: value.parent_shas.clone()
	}
}

fn clone_prepared_target_state_write(value PreparedTargetStateWrite) PreparedTargetStateWrite {
	return PreparedTargetStateWrite{
		...value
		changed_members: value.changed_members.clone()
		source:          value.source.clone()
	}
}

$if test {
	fn align_distinct_snapshot_comparison_for_test(first ReauthenticatedTargetStateObservation,
	second ReauthenticatedTargetStateObservation,
	comparison_focus string) !ReauthenticatedTargetStateObservation {
		match comparison_focus {
			'head' {
				return second
			}
			'tree' {
				return ReauthenticatedTargetStateObservation{
					...second
					proof: LiveStateCommitProof{
						...second.proof
						commit_sha:  first.proof.commit_sha
						remote_head: first.proof.remote_head
					}
				}
			}
			'blob' {
				return ReauthenticatedTargetStateObservation{
					...second
					proof: clone_live_state_commit_proof(first.proof)
				}
			}
			else {
				return error('unknown distinct-snapshot comparison focus')
			}
		}
	}

	fn mutate_reauthenticated_prepared_write_for_test(value PreparedTargetStateWrite,
	mutation string) !PreparedTargetStateWrite {
		match mutation {
			'prepared_target_id' {
				return PreparedTargetStateWrite{
					...value
					target_id: 'freebsd-amd64'
				}
			}
			'prepared_target_path' {
				return PreparedTargetStateWrite{
					...value
					target_path: 'targets/freebsd-amd64.json'
				}
			}
			'prepared_transition' {
				return PreparedTargetStateWrite{
					...value
					transition: 'candidate_failed'
				}
			}
			'prepared_operation_id' {
				return PreparedTargetStateWrite{
					...value
					operation_id: 'a'.repeat(64)
				}
			}
			'prepared_expected_generation' {
				return PreparedTargetStateWrite{
					...value
					expected_generation: value.expected_generation + 1
				}
			}
			'prepared_resulting_generation' {
				return PreparedTargetStateWrite{
					...value
					resulting_generation: value.resulting_generation + 1
				}
			}
			'prepared_expected_head' {
				return PreparedTargetStateWrite{
					...value
					expected_state_head_oid: 'b'.repeat(40)
				}
			}
			'prepared_predecessor_oid' {
				return PreparedTargetStateWrite{
					...value
					predecessor_blob_oid: 'b'.repeat(40)
				}
			}
			'prepared_predecessor_sha256' {
				return PreparedTargetStateWrite{
					...value
					predecessor_source_sha256: 'b'.repeat(64)
				}
			}
			'prepared_resulting_oid' {
				return PreparedTargetStateWrite{
					...value
					resulting_blob_oid: 'b'.repeat(40)
				}
			}
			'prepared_resulting_sha256' {
				return PreparedTargetStateWrite{
					...value
					resulting_source_sha256: 'b'.repeat(64)
				}
			}
			'prepared_changed_members' {
				mut changed_members := value.changed_members.clone()
				changed_members << 'forged'
				return PreparedTargetStateWrite{
					...value
					changed_members: changed_members
				}
			}
			'prepared_source' {
				return PreparedTargetStateWrite{
					...value
					source: value.source + ' '
				}
			}
			else {
				return error('unknown test-only prepared mutation')
			}
		}
	}
}

fn mutate_reauthenticated_observation_for_test(value ReauthenticatedTargetStateObservation,
	mutation string) !ReauthenticatedTargetStateObservation {
	if mutation == '' {
		return value
	}
	$if test {
		match mutation {
			'proof_repository' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						repository: 'GGRei/v'
					}
				}
			}
			'proof_ref' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						ref: 'refs/heads/other'
					}
				}
			}
			'proof_head' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						commit_sha: 'b'.repeat(40)
					}
				}
			}
			'proof_remote_head' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						remote_head: 'b'.repeat(40)
					}
				}
			}
			'proof_tree' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						tree_sha: 'b'.repeat(40)
					}
				}
			}
			'proof_parent' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						parent_shas: ['b'.repeat(40)]
					}
				}
			}
			'proof_signature' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						verification_verified: false
					}
				}
			}
			'proof_signature_reason' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						verification_reason: 'test-only-mutation'
					}
				}
			}
			'proof_time' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						verified_at: '2026-08-02T00:00:01Z'
					}
				}
			}
			'proof_app' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						state_writer_app_id: value.proof.state_writer_app_id + 1
					}
				}
			}
			'proof_actor_node_id' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						actor_node_id: 'BOT_other'
					}
				}
			}
			'proof_actor_login' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						actor_login: 'other[bot]'
					}
				}
			}
			'proof_actor_database_id' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						actor_database_id: value.proof.actor_database_id + 1
					}
				}
			}
			'proof_actor_type' {
				return ReauthenticatedTargetStateObservation{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						actor_type: 'User'
					}
				}
			}
			'entry_mode' {
				return ReauthenticatedTargetStateObservation{
					...value
					entry: ReauthenticatedTargetTreeEntry{
						...value.entry
						mode: '100755'
					}
				}
			}
			'entry_type' {
				return ReauthenticatedTargetStateObservation{
					...value
					entry: ReauthenticatedTargetTreeEntry{
						...value.entry
						kind: 'tree'
					}
				}
			}
			'entry_oid' {
				return ReauthenticatedTargetStateObservation{
					...value
					entry: ReauthenticatedTargetTreeEntry{
						...value.entry
						oid: 'b'.repeat(40)
					}
				}
			}
			'entry_size' {
				return ReauthenticatedTargetStateObservation{
					...value
					entry: ReauthenticatedTargetTreeEntry{
						...value.entry
						size: value.entry.size + 1
					}
				}
			}
			'entry_path' {
				return ReauthenticatedTargetStateObservation{
					...value
					entry: ReauthenticatedTargetTreeEntry{
						...value.entry
						path: 'targets/freebsd-amd64.json'
					}
				}
			}
			'bytes' {
				return ReauthenticatedTargetStateObservation{
					...value
					source: value.source + ' '
				}
			}
			'sha256' {
				return ReauthenticatedTargetStateObservation{
					...value
					source_sha256: 'b'.repeat(64)
				}
			}
			'schema' {
				return ReauthenticatedTargetStateObservation{
					...value
					schema_sha256: 'b'.repeat(64)
				}
			}
			'target' {
				return ReauthenticatedTargetStateObservation{
					...value
					target_id: 'freebsd-amd64'
				}
			}
			'generation' {
				return ReauthenticatedTargetStateObservation{
					...value
					generation: value.generation + 1
				}
			}
			'semantic' {
				mut changed_model := value.model
				changed_model.last_transition = 'test-only-mutation'
				return ReauthenticatedTargetStateObservation{
					...value
					model: changed_model
				}
			}
			'root' {
				changed_root := parse_strict_json(canonical_json(value.root).replace_once('"schema_version":1',
					'"schema_version":2'))!
				return ReauthenticatedTargetStateObservation{
					...value
					root: changed_root
				}
			}
			else {
				return error('unknown test-only reauthentication mutation')
			}
		}
	}
	return error('test-only reauthentication mutation is unavailable')
}
