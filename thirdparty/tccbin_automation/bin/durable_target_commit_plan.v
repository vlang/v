module bin

import crypto.sha1
import crypto.sha256
import encoding.hex
import os

const durable_plan_audience = 'vlang/v:tccbin-automation-state'
const durable_plan_subject_domain = 'vlang/v:tccbin-durable-target-plan-subject:v1'
const durable_plan_inventory_max_files = 100_000
const durable_plan_inventory_max_bytes = i64(16 * 1024 * 1024)
const durable_plan_schema_max_bytes = 256 * 1024

const durable_plan_schema_paths = ['active-intent.schema.json', 'common.schema.json',
	'evidence.schema.json', 'lane-result.schema.json', 'native-gate-execution.schema.json',
	'native-gate-subject.schema.json', 'native-lane-matrix.schema.json',
	'recovery-handoff.schema.json', 'source-state.schema.json', 'target-state.schema.json',
	'toolchain-observation.schema.json']

const durable_plan_schema_roots = ['target-state.schema.json', 'evidence.schema.json',
	'source-state.schema.json']

// DurableTargetChangedBlob is detached output metadata. No production API accepts it as proof,
// authority, a write request, or a CAS capability.
pub struct DurableTargetChangedBlob {
pub:
	path     string
	mode     string
	blob_oid string
	sha256   string
	size     i64
	source   string
}

// DurableTargetCommitPlanObservation is forgeable, immediately stale observation data. A future
// consumer must reauthenticate and replan; these fields cannot authorize a write or ref update.
pub struct DurableTargetCommitPlanObservation {
pub:
	state_commit             string
	predecessor_tree_oid     string
	postimage_tree_oid       string
	target_id                string
	event                    string
	result                   string
	operation_id             string
	plan_subject_fingerprint string
	evidence_path            string
	changed_paths            []string
	changed_blobs            []DurableTargetChangedBlob
}

// DurableTargetCommitPlan deliberately keeps its authenticated proof and prepared bytes private.
// It is a dormant plan observation, not a writer, commit, retry token, or compare-and-swap handle.
pub struct DurableTargetCommitPlan {
	observation DurableTargetCommitPlanObservation
}

// observation returns detached copies accepted by no production consumer.
pub fn (plan DurableTargetCommitPlan) observation() DurableTargetCommitPlanObservation {
	return clone_durable_plan_observation(plan.observation)
}

struct DurablePlanInventoryRecord {
	path   string
	mode   string
	kind   string
	oid    string
	size   i64
	sha256 string
	source string
}

struct DurablePlanTreeRecord {
	node_index int
	oid        string
}

struct DurablePlanPhysicalSnapshot {
	proof         LiveStateCommitProof
	entries       []DurablePlanInventoryRecord
	entry_indices map[string]int
	tree_records  []DurablePlanTreeRecord
	root_tree_oid string
	total_bytes   i64
}

struct DurablePlanSchemaFact {
	relative_path string
	sha256        string
	source        string
	refs          []string
}

struct DurablePlanSchemaClosure {
	facts []DurablePlanSchemaFact
}

struct DurablePlanSourceBinding {
	source ResolvedSourceModel
	check  SourceCheckModel
}

struct DurablePlanPass {
	proof                    LiveStateCommitProof
	inventory                []DurablePlanInventoryRecord
	predecessor_tree_records []DurablePlanTreeRecord
	predecessor_tree_oid     string
	schema_closure           DurablePlanSchemaClosure
	target_root              JsonValue
	target_model             TargetModel
	source_binding           DurablePlanSourceBinding
	plan_subject_source      string
	plan_subject_fingerprint string
	identity                 OperationIdentityInput
	operation_id             string
	prepared                 PreparedTargetStateWrite
	evidence                 PreparedDurableTargetEvidence
	postimage_entries        []DurablePlanInventoryRecord
	postimage_tree_records   []DurablePlanTreeRecord
	postimage_tree_oid       string
	changed_paths            []string
}

struct DurablePlanTreePart {
	name         string
	mode         string
	oid          string
	is_directory bool
	child_index  int = -1
}

struct DurablePlanTreeNode {
mut:
	parts []DurablePlanTreePart
}

struct DurablePlanLinearCounters {
	parsed_records      int
	path_components     int
	tree_parts          int
	exact2_predecessors int
	exact2_postimages   int
}

struct DurablePlanExact2Changes {
mut:
	target_changed   bool
	evidence_changed bool
	count            int
}

struct DurablePlanTreeTraversal {
mut:
	causal_parts int
}

struct DurablePlanSchemaReference {
	source_path string
	target_path string
	fragment    string
	raw         string
}

struct DurableCommitRetryDecision {
	verdict       string
	next_attempt  int
	delay_seconds int
}

// prepare_durable_target_commit_plan performs two independent local reads and returns only when
// every authenticated input and deterministic output is identical. It does no network access,
// write, Git object creation, commit, ref mutation, retry, or CAS.
pub fn prepare_durable_target_commit_plan(automation_root string, state_git_dir string,
	trust LiveStateTrust, proof_bundle_dir string, target_id string, event TransitionEvent,
	invocation DurableTargetPlanInvocation) !DurableTargetCommitPlan {
	$if windows {
		return error('durable target commit planning is unavailable on Windows without a raw-byte Git runner')
	} $else $if linux || macos || freebsd || openbsd {
		mut session := durable_git_runner_begin(state_git_dir)!
		mut failure := ''
		plan := prepare_durable_target_commit_plan_core(automation_root, state_git_dir, trust,
			proof_bundle_dir, target_id, event, invocation) or {
			failure = err.msg()
			DurableTargetCommitPlan{}
		}
		if failure != '' {
			// Preserve an earlier nested runner cause; otherwise freeze the core cause before
			// cleanup can observe signal drift or a poisoned lease.
			durable_git_record_first_failure(mut session, failure)
		}
		mut cleanup_failure := ''
		durable_git_runner_end(mut session) or { cleanup_failure = err.msg() }
		// An os.Result adapter may have collapsed a nested callsite to a generic message. The
		// session's first runner failure is authoritative, followed by the core cause, then a
		// cleanup-only failure. A poisoned cleanup can therefore never mask the first cause.
		selected_failure := durable_git_prioritized_failure(session, failure, cleanup_failure)
		if selected_failure != '' {
			return error(selected_failure)
		}
		return plan
	} $else {
		return error('durable target commit planning is unavailable on this POSIX platform without mandatory bulk descriptor close')
	}
}

fn prepare_durable_target_commit_plan_core(automation_root string, state_git_dir string,
	trust LiveStateTrust, proof_bundle_dir string, target_id string, event TransitionEvent,
	invocation DurableTargetPlanInvocation) !DurableTargetCommitPlan {
	validate_durable_plan_invocation(invocation)!
	validate_durable_plan_event(event)!
	first := prepare_durable_target_commit_plan_pass(automation_root, state_git_dir, trust,
		proof_bundle_dir, target_id, event, invocation)!
	second := prepare_durable_target_commit_plan_pass(automation_root, state_git_dir, trust,
		proof_bundle_dir, target_id, event, invocation)!
	validate_durable_plan_passes_match(first, second)!
	return DurableTargetCommitPlan{
		observation: durable_plan_observation(first)
	}
}

fn validate_durable_plan_event(event TransitionEvent) ! {
	if event !in [.ledger_repaired_with_blockers, .ledger_repaired_without_blockers] {
		return error('durable target commit planning accepts only the two closed ledger-repair events')
	}
}

fn prepare_durable_target_commit_plan_pass(automation_root string, state_git_dir string,
	trust LiveStateTrust, proof_bundle_dir string, target_id string, event TransitionEvent,
	invocation DurableTargetPlanInvocation) !DurablePlanPass {
	physical := load_durable_plan_physical_snapshot(state_git_dir, trust, proof_bundle_dir)!
	validate_durable_plan_null6_and_terminal_absence(physical)!
	closure_before := load_durable_plan_schema_closure(automation_root)!
	authenticated := durable_plan_observation_from_physical(automation_root, physical,
		closure_before, target_id)!
	closure_after_load := load_durable_plan_schema_closure(automation_root)!
	validate_durable_schema_closures_match(closure_before, closure_after_load)!
	target_entry := durable_inventory_entry_at(physical.entries, physical.entry_indices,
		target_state_path(target_id)!)!
	if authenticated.proof != physical.proof || authenticated.source != target_entry.source
		|| authenticated.entry.oid != target_entry.oid
		|| authenticated.entry.size != target_entry.size
		|| authenticated.source_sha256 != target_entry.sha256 {
		return error('durable target authority observation differs from the complete physical inventory')
	}
	validate_durable_plan_target_lane(authenticated.root, authenticated.model, event)!
	source_binding := select_durable_plan_source(authenticated.model, invocation)!
	plan_subject := durable_plan_subject(physical, closure_before, authenticated, source_binding,
		invocation, event)!
	plan_subject_source := canonical_json(plan_subject)
	plan_subject_fingerprint := sha256.sum256(plan_subject_source.bytes()).hex()
	if plan_subject_fingerprint == authenticated.model.input_fingerprint {
		return error('durable target plan-subject commitment collides with the distinct input fingerprint')
	}
	native_subject_hash := '0'.repeat(64)
	identity := OperationIdentityInput{
		audience:                durable_plan_audience
		run_id:                  invocation.run_id
		run_attempt:             invocation.run_attempt
		ordinal:                 invocation.operation_ordinal
		cas_attempt:             1
		subject_id:              target_id
		transition:              event.str()
		expected_generation:     authenticated.model.generation
		expected_canonical_head: authenticated.model.canonical_observed_sha
		source_ref:              source_binding.source.ref
		source_sha:              source_binding.source.sha
		subject_fingerprint:     plan_subject_fingerprint
		input_fingerprint:       authenticated.model.input_fingerprint
		artifact_fingerprint:    authenticated.model.artifact_fingerprint
		manifest_hash:           authenticated.model.manifest_hash
		native_subject_hash:     native_subject_hash
		intent_id:               ''
	}
	operation_id := deterministic_operation_id(identity)!
	planned_evidence_path := durable_planned_evidence_path(invocation, target_id, operation_id,

		authenticated.model.generation + 1, event.str(), plan_subject_fingerprint)!
	validate_durable_global_collision_absent(physical.entries, operation_id, planned_evidence_path)!
	context := TransitionContext{
		operation_id: operation_id
	}
	prepared := prepare_reauthenticated_target_state_write(authenticated, automation_root, event,
		context)!
	validate_reauthenticated_prepared_binding(authenticated, prepared, target_id, event, context)!
	if prepared.expected_state_head_oid != physical.proof.commit_sha
		|| prepared.predecessor_blob_oid != target_entry.oid
		|| prepared.predecessor_source_sha256 != target_entry.sha256
		|| prepared.operation_id != operation_id {
		return error('durable target prepared write differs from its committed physical authority')
	}
	result := if event == .ledger_repaired_with_blockers { 'blocked' } else { 'passed' }
	evidence := prepare_durable_target_evidence(automation_root, DurableTargetEvidenceRequest{
		invocation:           invocation
		operation_id:         operation_id
		transition:           event.str()
		subject_id:           target_id
		subject_fingerprint:  plan_subject_fingerprint
		input_fingerprint:    authenticated.model.input_fingerprint
		artifact_fingerprint: authenticated.model.artifact_fingerprint
		generation_read:      authenticated.model.generation
		generation_written:   prepared.resulting_generation
		result:               result
		target_path:          prepared.target_path
		target_sha256:        prepared.resulting_source_sha256
	})!
	if evidence.path != planned_evidence_path {
		return error('durable target evidence path differs from the pre-ID committed derivation')
	}
	postimage_entries := durable_plan_postimage_entries(physical.entries, prepared, evidence)!
	post_tree_records, post_tree_oid := rebuild_durable_plan_tree(postimage_entries)!
	validate_durable_plan_exact2(physical, postimage_entries, prepared, evidence, post_tree_oid)!
	closure_after_prepare := load_durable_plan_schema_closure(automation_root)!
	validate_durable_schema_closures_match(closure_before, closure_after_prepare)!
	validate_durable_postimage_null6(postimage_entries)!
	mut changed_paths := [prepared.target_path, evidence.path]
	changed_paths.sort()
	return DurablePlanPass{
		proof:                    clone_live_state_commit_proof(physical.proof)
		inventory:                clone_durable_inventory(physical.entries)
		predecessor_tree_records: physical.tree_records.clone()
		predecessor_tree_oid:     physical.root_tree_oid
		schema_closure:           closure_before
		target_root:              authenticated.root
		target_model:             authenticated.model
		source_binding:           source_binding
		plan_subject_source:      plan_subject_source
		plan_subject_fingerprint: plan_subject_fingerprint
		identity:                 identity
		operation_id:             operation_id
		prepared:                 prepared
		evidence:                 evidence
		postimage_entries:        postimage_entries
		postimage_tree_records:   post_tree_records
		postimage_tree_oid:       post_tree_oid
		changed_paths:            changed_paths
	}
}

// The planner validates only bytes already bound by its one complete physical snapshot. It does
// not call the public reauthentication loader, load history, or perform a second hidden Git read.
fn durable_plan_observation_from_physical(automation_root string,
	physical DurablePlanPhysicalSnapshot, closure DurablePlanSchemaClosure,
	target_id string) !ReauthenticatedTargetStateObservation {
	mut records := []LiveStateInventoryBlob{cap: physical.entries.len}
	for entry in physical.entries {
		records << LiveStateInventoryBlob{
			path:   entry.path
			mode:   entry.mode
			kind:   entry.kind
			oid:    entry.oid
			size:   entry.size
			source: entry.source
		}
	}
	inventory := validate_live_state_inventory_blobs(automation_root, records)!
	target_path := target_state_path(target_id)!
	entry := durable_inventory_entry_at(physical.entries, physical.entry_indices, target_path)!
	source := inventory.blobs[target_path] or {
		return error('durable target physical inventory lacks its selected target bytes')
	}
	durable := load_durable_target_root(automation_root, source)!
	if durable.model.target_id != target_id {
		return error('durable target physical model differs from its path identity')
	}
	mut target_schema_sha256 := ''
	for fact in closure.facts {
		if fact.relative_path == 'target-state.schema.json' {
			target_schema_sha256 = fact.sha256
			break
		}
	}
	if !is_lower_hex_64(target_schema_sha256) {
		return error('durable target schema closure lacks its target-state root')
	}
	return ReauthenticatedTargetStateObservation{
		proof:         clone_live_state_commit_proof(physical.proof)
		entry:         ReauthenticatedTargetTreeEntry{
			mode: entry.mode
			kind: entry.kind
			oid:  entry.oid
			size: entry.size
			path: entry.path
		}
		target_id:     target_id
		source:        source
		source_sha256: entry.sha256
		schema_sha256: target_schema_sha256
		generation:    durable.model.generation
		root:          durable.root
		model:         durable.model
	}
}

fn load_durable_plan_physical_snapshot(state_git_dir string, trust LiveStateTrust,
	proof_bundle_dir string) !DurablePlanPhysicalSnapshot {
	bundle := authenticate_live_state_proof_bundle(state_git_dir, trust, proof_bundle_dir)!
	if bundle.historical_paths.len != 0 {
		return error('durable target ledger-only planning requires an exactly empty historical proof bundle')
	}
	result := durable_git_run_args(state_git_dir, ['ls-tree', '-r', '-l', '-z', '--full-tree',
		bundle.head.tree_sha], .inventory)!
	if result.exit_code != 0 || result.stderr != ''
		|| i64(result.stdout.len) > durable_plan_inventory_max_bytes {
		return error('durable target full-tree inventory cannot be read inside its byte bound')
	}
	raw_entries := parse_durable_plan_tree_listing(result.stdout)!
	durable_plan_require_exact2_slot(raw_entries.len)!
	mut entries := []DurablePlanInventoryRecord{cap: raw_entries.len}
	mut total_bytes := i64(0)
	for raw in raw_entries {
		read_class := if raw.path.starts_with('targets/') {
			DurableGitReadClass.target_blob
		} else {
			DurableGitReadClass.evidence_blob
		}
		blob := durable_git_run_args(state_git_dir, ['cat-file', 'blob', raw.oid], read_class)!
		if blob.exit_code != 0 || blob.stderr != '' || i64(blob.stdout.len) != raw.size
			|| git_blob_oid(blob.stdout.bytes()) != raw.oid {
			return error('durable target inventory blob differs from its full-tree identity')
		}
		total_bytes += raw.size
		if total_bytes > durable_plan_inventory_max_bytes {
			return error('durable target physical inventory exceeds its global byte bound')
		}
		entries << DurablePlanInventoryRecord{
			...raw
			sha256: sha256.sum256(blob.stdout.bytes()).hex()
			source: blob.stdout
		}
	}
	validate_durable_inventory_pathset(entries)!
	entry_indices := durable_inventory_indices(entries)!
	tree_records, root_tree_oid := rebuild_durable_plan_tree(entries)!
	if root_tree_oid != bundle.head.tree_sha {
		return error('durable target reconstructed predecessor tree differs from the authenticated root')
	}
	return DurablePlanPhysicalSnapshot{
		proof:         clone_live_state_commit_proof(bundle.head)
		entries:       entries
		entry_indices: entry_indices
		tree_records:  tree_records
		root_tree_oid: root_tree_oid
		total_bytes:   total_bytes
	}
}

fn parse_durable_plan_tree_listing(source string) ![]DurablePlanInventoryRecord {
	if source == '' || i64(source.len) > durable_plan_inventory_max_bytes
		|| !source.ends_with('\x00') {
		return error('durable target full-tree inventory lacks its unique terminal NUL')
	}
	mut record_count := 1
	mut preflight_start := 0
	for index in 0 .. (source.len - 1) {
		if source[index] != 0 {
			continue
		}
		if index == preflight_start {
			return error('durable target full-tree inventory contains an empty interior record')
		}
		record_count++
		if record_count > durable_plan_inventory_max_files {
			return error('durable target full-tree inventory exceeds its global file bound')
		}
		preflight_start = index + 1
	}
	if preflight_start >= source.len - 1 {
		return error('durable target full-tree inventory contains an empty interior record')
	}
	mut result := []DurablePlanInventoryRecord{cap: record_count}
	mut paths := map[string]bool{}
	mut previous_path := ''
	mut declared_bytes := i64(0)
	mut start := 0
	for index in 0 .. source.len {
		if source[index] != 0 {
			continue
		}
		record := source[start..index]
		parts := record.split_nth('\t', 2)
		if parts.len != 2 {
			return error('durable target full-tree inventory record is malformed')
		}
		metadata := parts[0].fields()
		if metadata.len != 4 {
			return error('durable target full-tree inventory metadata is malformed')
		}
		size_source := metadata[3]
		size := size_source.i64()
		path := parts[1]
		if metadata[0] != '100644' || metadata[1] != 'blob'
			|| !is_lower_hex_40(metadata[2]) || size < 0 || size_source != size.str()
			|| !contract_relative_path_is_safe(path)
			|| path.len > durable_evidence_path_max_bytes || path in paths
			|| (previous_path != '' && path <= previous_path) {
			return error('durable target full-tree entry mode, type, OID, size, or path is invalid')
		}
		max_size := if path.starts_with('targets/') {
			i64(durable_target_max_bytes)
		} else {
			i64(durable_evidence_max_bytes)
		}
		if size == 0 || size > max_size {
			return error('durable target full-tree entry exceeds its closed per-file bound')
		}
		if declared_bytes > durable_plan_inventory_max_bytes - size {
			return error('durable target full-tree declared bytes exceed the global bound')
		}
		declared_bytes += size
		paths[path] = true
		previous_path = path
		result << DurablePlanInventoryRecord{
			path: path
			mode: metadata[0]
			kind: metadata[1]
			oid:  metadata[2]
			size: size
		}
		start = index + 1
	}
	if start != source.len || result.len != record_count {
		return error('durable target full-tree inventory exceeds its global file bound')
	}
	return result
}

fn validate_durable_inventory_pathset(entries []DurablePlanInventoryRecord) ! {
	mut targets := []string{}
	mut sources := []string{}
	mut previous_path := ''
	for entry in entries {
		if entry.path.starts_with('targets/') {
			targets << entry.path
		} else if entry.path.starts_with('sources/') {
			sources << entry.path
		} else if !entry.path.starts_with('evidence/') || !entry.path.ends_with('.json') {
			return error('durable target inventory contains a path outside targets, sources, and evidence')
		}
		if previous_path != '' {
			if entry.path <= previous_path {
				return error('durable target inventory paths are duplicated or noncanonical')
			}
			if entry.path.starts_with('${previous_path}/') {
				return error('durable target inventory contains a file/directory prefix collision')
			}
		}
		previous_path = entry.path
	}
	expected_targets := managed_target_ids.map('targets/${it}.json')
	expected_sources := ['sources/tinycc-mob.json', 'sources/bdwgc-master.json',
		'sources/libatomic_ops-master.json']
	mut sorted_targets := expected_targets.clone()
	mut sorted_sources := expected_sources.clone()
	targets.sort()
	sources.sort()
	sorted_targets.sort()
	sorted_sources.sort()
	if targets != sorted_targets || sources != sorted_sources {
		return error('durable target inventory does not contain exactly six targets and three sources')
	}
}

fn rebuild_durable_plan_tree(entries []DurablePlanInventoryRecord) !([]DurablePlanTreeRecord, string) {
	records, root_oid, _ := rebuild_durable_plan_tree_linear(entries)!
	return records, root_oid
}

fn rebuild_durable_plan_tree_linear(entries []DurablePlanInventoryRecord) !([]DurablePlanTreeRecord, string, int) {
	if entries.len == 0 || entries.len > durable_plan_inventory_max_files {
		return error('durable target tree reconstruction received an invalid entry count')
	}
	mut nodes := [DurablePlanTreeNode{}]
	mut child_nodes := map[string]int{}
	mut occupied_parts := map[string]bool{}
	for entry in entries {
		components := entry.path.split('/')
		if components.len == 0 {
			return error('durable target tree reconstruction found an empty path')
		}
		mut node_index := 0
		for component_index, name in components {
			part_key := '${node_index}\x1f${name}'
			if component_index == components.len - 1 {
				if part_key in occupied_parts {
					return error('durable target tree reconstruction found a file/directory collision')
				}
				occupied_parts[part_key] = true
				nodes[node_index].parts << DurablePlanTreePart{
					name: name
					mode: entry.mode
					oid:  entry.oid
				}
				continue
			}
			child_index := child_nodes[part_key] or {
				if part_key in occupied_parts {
					return error('durable target tree reconstruction found a file/directory collision')
				}
				occupied_parts[part_key] = true
				new_index := nodes.len
				nodes << DurablePlanTreeNode{}
				nodes[node_index].parts << DurablePlanTreePart{
					name:         name
					mode:         '40000'
					is_directory: true
					child_index:  new_index
				}
				child_nodes[part_key] = new_index
				new_index
			}
			node_index = child_index
		}
	}
	mut records := []DurablePlanTreeRecord{}
	mut visited := []bool{len: nodes.len}
	mut traversal := DurablePlanTreeTraversal{}
	root_oid := durable_plan_tree_node_oid(nodes, 0, mut records, mut visited, mut traversal)!
	if visited.any(!it) || traversal.causal_parts < entries.len {
		return error('durable target tree reconstruction did not consume its exact linear index')
	}
	return records, root_oid, traversal.causal_parts
}

fn durable_plan_tree_node_oid(nodes []DurablePlanTreeNode, node_index int,
	mut records []DurablePlanTreeRecord, mut visited []bool,
	mut traversal DurablePlanTreeTraversal) !string {
	if node_index < 0 || node_index >= nodes.len || visited[node_index] {
		return error('durable target tree reconstruction contains a cycle or invalid node')
	}
	visited[node_index] = true
	node := nodes[node_index]
	mut body := []u8{}
	mut previous_key := ''
	for part in node.parts {
		key := part.name + if part.is_directory { '/' } else { '' }
		if previous_key != '' && key <= previous_key {
			return error('durable target tree reconstruction parts are not in canonical Git order')
		}
		previous_key = key
		traversal.causal_parts++
		oid := if part.is_directory {
			durable_plan_tree_node_oid(nodes, part.child_index, mut records, mut visited, mut
				traversal)!
		} else {
			part.oid
		}
		if !is_lower_hex_40(oid) {
			return error('durable target tree reconstruction encountered a malformed object OID')
		}
		header := '${part.mode} ${part.name}\x00'.bytes()
		decoded_oid := hex.decode(oid) or {
			return error('durable target tree reconstruction encountered a malformed object OID')
		}
		if decoded_oid.len != 20 || header.len > int(durable_plan_inventory_max_bytes) - 20
			|| body.len > int(durable_plan_inventory_max_bytes) - header.len - 20 {
			return error('durable target tree reconstruction exceeds its byte bound')
		}
		body << header
		body << decoded_oid
	}
	material_header := 'tree ${body.len}\x00'.bytes()
	if material_header.len > int(durable_plan_inventory_max_bytes)
		|| body.len > int(durable_plan_inventory_max_bytes) - material_header.len {
		return error('durable target tree reconstruction exceeds its byte bound')
	}
	mut material := material_header.clone()
	material << body
	oid := sha1.sum(material).hex()
	records << DurablePlanTreeRecord{
		node_index: node_index
		oid:        oid
	}
	return oid
}

fn durable_compare_strings(left string, right string) int {
	if left < right {
		return -1
	}
	if left > right {
		return 1
	}
	return 0
}

fn validate_durable_plan_null6_and_terminal_absence(snapshot DurablePlanPhysicalSnapshot) ! {
	for target_id in managed_target_ids {
		entry := durable_inventory_entry_at(snapshot.entries, snapshot.entry_indices,
			target_state_path(target_id)!)!
		root := parse_strict_json(entry.source) or {
			return error('durable target null6 pre-scan found malformed strict JSON')
		}
		if root.kind != .object || require_string_member(root, 'target_id')! != target_id {
			return error('durable target null6 pre-scan found a root/path identity mismatch')
		}
		native := require_member(root, 'last_native_validation')!
		if native.kind != .null_value {
			return error('durable target null6 pre-scan requires last_native_validation null on all six targets')
		}
		for handoff in require_array_member(root, 'recovery_handoffs')! {
			outcome := require_nullable_string_member(handoff, 'terminal_outcome') or { '' }
			if outcome == 'source_waiting' {
				return error('durable target ledger-only planning rejects terminal source handoffs')
			}
		}
	}
	for entry in snapshot.entries {
		if !entry.path.starts_with('evidence/') {
			continue
		}
		root := parse_strict_json(entry.source) or {
			return error('durable target terminal pre-scan found malformed evidence JSON')
		}
		if require_string_member(root, 'transition')!.starts_with('source_unreachable_') {
			return error('durable target ledger-only planning rejects terminal source evidence')
		}
	}
}

fn validate_durable_postimage_null6(entries []DurablePlanInventoryRecord) ! {
	indices := durable_inventory_indices(entries)!
	for target_id in managed_target_ids {
		root := parse_strict_json(durable_inventory_entry_at(entries, indices,
			target_state_path(target_id)!)!.source)!
		if require_member(root, 'last_native_validation')!.kind != .null_value {
			return error('durable target exact2 postimage violates the global null6 invariant')
		}
	}
}

fn load_durable_plan_schema_closure(automation_root string) !DurablePlanSchemaClosure {
	schemas_root := os.join_path(automation_root, 'schemas')
	if !os.is_abs_path(schemas_root) || os.real_path(schemas_root) != schemas_root
		|| os.is_link(schemas_root) || !os.is_dir(schemas_root) {
		return error('durable target schema root is not one exact physical directory')
	}
	mut pending := durable_plan_schema_roots.clone()
	mut facts := []DurablePlanSchemaFact{}
	mut references := []DurablePlanSchemaReference{}
	for pending.len > 0 {
		pending.sort()
		relative_path := pending[0]
		pending.delete(0)
		if facts.any(it.relative_path == relative_path) {
			continue
		}
		validate_durable_schema_relative_path(relative_path)!
		path := os.join_path(schemas_root, relative_path)
		if os.real_path(path) != path || os.is_link(path) || !os.is_file(path) {
			return error('durable target schema closure contains a missing, linked, or escaped file')
		}
		source := os.read_file(path)!
		if source.len == 0 || source.len > durable_plan_schema_max_bytes {
			return error('durable target schema closure file exceeds its closed byte bound')
		}
		root := parse_strict_json(source)!
		mut raw_refs := []string{}
		collect_durable_schema_refs(root, mut raw_refs)!
		raw_refs.sort()
		for raw in raw_refs {
			target_path, fragment := parse_durable_schema_reference(relative_path, raw)!
			if target_path !in durable_plan_schema_paths {
				return error('durable target schema reference leaves the exact eleven-file closure')
			}
			references << DurablePlanSchemaReference{
				source_path: relative_path
				target_path: target_path
				fragment:    fragment
				raw:         raw
			}
			if !facts.any(it.relative_path == target_path) && target_path !in pending {
				pending << target_path
			}
		}
		facts << DurablePlanSchemaFact{
			relative_path: relative_path
			sha256:        sha256.sum256(source.bytes()).hex()
			source:        source
			refs:          raw_refs
		}
	}
	facts.sort_with_compare(compare_durable_schema_facts)
	mut paths := facts.map(it.relative_path)
	if paths != durable_plan_schema_paths {
		return error('durable target schema closure differs from the exact eleven-file set')
	}
	for reference in references {
		document := durable_schema_fact(facts, reference.target_path)!
		resolve_durable_schema_fragment(parse_strict_json(document.source)!, reference.fragment) or {
			return error('durable target schema reference has an unresolved JSON fragment')
		}
	}
	return DurablePlanSchemaClosure{
		facts: facts
	}
}

fn validate_durable_schema_relative_path(path string) ! {
	if path == '' || path.contains('/') || path.contains('\\') || path.contains('..')
		|| path.contains(':') || path.contains('?') || !path.ends_with('.schema.json') {
		return error('durable target schema reference escapes the closed relative namespace')
	}
}

fn collect_durable_schema_refs(value JsonValue, mut refs []string) ! {
	match value.kind {
		.object {
			for index, key in value.object_keys {
				child := value.object_values[index]
				if key == '$ref' {
					if child.kind != .string_value || child.string_value == '' {
						return error('durable target schema contains a non-string or empty reference')
					}
					refs << child.string_value
				}
				collect_durable_schema_refs(child, mut refs)!
			}
		}
		.array {
			for child in value.array_value {
				collect_durable_schema_refs(child, mut refs)!
			}
		}
		else {}
	}
}

fn parse_durable_schema_reference(current_path string, raw string) !(string, string) {
	if raw.contains('://') || raw.starts_with('/') || raw.contains('\\') || raw.contains('?') {
		return error('durable target schema contains an external or unsafe reference')
	}
	index := raw.index('#') or { -1 }
	document := if index < 0 { raw } else { raw[..index] }
	fragment := if index < 0 { '' } else { raw[index + 1..] }
	target_path := if document == '' { current_path } else { document }
	validate_durable_schema_relative_path(target_path)!
	if fragment != '' && !fragment.starts_with('/') {
		return error('durable target schema reference fragment is not a JSON pointer')
	}
	validate_durable_schema_pointer_encoding(fragment)!
	return target_path, fragment
}

fn validate_durable_schema_pointer_encoding(fragment string) ! {
	mut index := 0
	for index < fragment.len {
		if fragment[index] == `~` {
			if index + 1 >= fragment.len || fragment[index + 1] !in [`0`, `1`] {
				return error('durable target schema reference contains an invalid JSON pointer escape')
			}
			index += 2
			continue
		}
		index++
	}
}

fn resolve_durable_schema_fragment(root JsonValue, fragment string) !JsonValue {
	if fragment == '' {
		return root
	}
	mut current := root
	for encoded in fragment[1..].split('/') {
		token := encoded.replace('~1', '/').replace('~0', '~')
		if current.kind == .object {
			current = current.object_value(token) or {
				return error('missing schema object fragment')
			}
		} else if current.kind == .array {
			index := token.int()
			if token != index.str() || index < 0 || index >= current.array_value.len {
				return error('invalid schema array fragment')
			}
			current = current.array_value[index]
		} else {
			return error('schema fragment crosses a scalar')
		}
	}
	return current
}

fn compare_durable_schema_facts(left &DurablePlanSchemaFact,
	right &DurablePlanSchemaFact) int {
	return durable_compare_strings(left.relative_path, right.relative_path)
}

fn durable_schema_fact(facts []DurablePlanSchemaFact, path string) !DurablePlanSchemaFact {
	matches := facts.filter(it.relative_path == path)
	if matches.len != 1 {
		return error('durable target schema closure lacks one exact referenced document')
	}
	return matches[0]
}

fn validate_durable_schema_closures_match(first DurablePlanSchemaClosure,
	second DurablePlanSchemaClosure) ! {
	if first.facts != second.facts {
		return error('durable target schema closure changed during one planning pass')
	}
}

fn validate_durable_plan_target_lane(root JsonValue, model TargetModel,
	event TransitionEvent) ! {
	if model.target_state != .unknown_blocked || model.publication_state != .idle
		|| model.bootstrap_required || !resolved_inputs_is_set(model.resolved_inputs)
		|| model.input_fingerprint == '' || model.artifact_fingerprint == ''
		|| model.manifest_hash == '' || model.applied_operations.len >= applied_operation_limit {
		return error('durable target ledger repair predecessor is not one seeded bounded unknown ledger')
	}
	for key in ['active_intent', 'post_validation_operation_id', 'native_gate_subject',
		'active_subject_hash', 'native_gate_execution', 'v_smoke_execution',
		'active_recovery_handoff_id', 'active_remediation_id', 'active_remediation_binding',
		'last_native_validation'] {
		value := require_member(root, key)!
		if value.kind != .null_value {
			return error('durable target ledger repair predecessor retains an active subject or companion')
		}
	}
	if require_array_member(root, 'remediation_check_sources')!.len != 0 {
		return error('durable target ledger repair predecessor retains remediation check ownership')
	}
	for handoff in require_array_member(root, 'recovery_handoffs')! {
		if require_string_member(handoff, 'state')! != 'complete'
			|| require_nullable_string_member(handoff, 'terminal_outcome')! == 'source_waiting' {
			return error('durable target ledger repair predecessor retains a nonterminal recovery handoff')
		}
	}
	incidents := require_array_member(root, 'incidents')!
	if (event == .ledger_repaired_with_blockers && incidents.len == 0)
		|| (event == .ledger_repaired_without_blockers && incidents.len != 0) {
		return error('durable target ledger repair event differs from its exact incident lane')
	}
}

fn select_durable_plan_source(model TargetModel,
	invocation DurableTargetPlanInvocation) !DurablePlanSourceBinding {
	sources := model.resolved_inputs.sources.filter(it.id == invocation.source_id)
	checks := model.resolved_inputs.source_checks.filter(it.source_id == invocation.source_id)
	if sources.len != 1 || checks.len != 1 || checks[0].status != 'resolved'
		|| checks[0].resolved_sha != sources[0].sha || checks[0].evidence_digest == ''
		|| invocation.workflow_sha != model.resolved_inputs.v_source_sha {
		return error('durable target invocation lacks one exact resolved source and source-check binding')
	}
	if model.resolved_inputs.sources.count(it.ref == sources[0].ref && it.sha == sources[0].sha) != 1 {
		return error('durable target source ref/SHA pair is not unique in resolved inputs')
	}
	return DurablePlanSourceBinding{
		source: sources[0]
		check:  checks[0]
	}
}

fn durable_plan_subject(physical DurablePlanPhysicalSnapshot,
	closure DurablePlanSchemaClosure, authenticated ReauthenticatedTargetStateObservation,
	binding DurablePlanSourceBinding, invocation DurableTargetPlanInvocation,
	event TransitionEvent) !JsonValue {
	mut inventory_values := []JsonValue{cap: physical.entries.len}
	for entry in physical.entries {
		inventory_values << object_value_from_pairs(['path', 'mode', 'type', 'size', 'blob_oid',
			'sha256'], [durable_json_string(entry.path), durable_json_string(entry.mode),
			durable_json_string(entry.kind), durable_json_integer(entry.size),
			durable_json_string(entry.oid), durable_json_string(entry.sha256)])!
	}
	mut schema_values := []JsonValue{cap: closure.facts.len}
	for fact in closure.facts {
		schema_values << object_value_from_pairs(['relative_path', 'sha256'], [
			durable_json_string(fact.relative_path),
			durable_json_string(fact.sha256),
		])!
	}
	authority := durable_plan_authority_json(physical.proof)!
	inventory := object_value_from_pairs(['entry_count', 'total_bytes', 'reconstructed_root_tree_oid',
		'entries'], [durable_json_integer(i64(physical.entries.len)),
		durable_json_integer(physical.total_bytes), durable_json_string(physical.root_tree_oid),
		durable_json_array(inventory_values)])!
	contracts := object_value_from_pairs(['schema_closure', 'global_last_native_validation',
		'target_max_bytes', 'evidence_max_bytes', 'evidence_path_max_bytes', 'inventory_max_files',
		'inventory_max_bytes', 'target_serializer', 'evidence_serializer', 'tree_serializer'], [
		durable_json_array(schema_values),
		durable_json_string('null6'),
		durable_json_integer(i64(durable_target_max_bytes)),
		durable_json_integer(i64(durable_evidence_max_bytes)),
		durable_json_integer(i64(durable_evidence_path_max_bytes)),
		durable_json_integer(i64(durable_plan_inventory_max_files)),
		durable_json_integer(durable_plan_inventory_max_bytes),
		durable_json_string('durable-target-jcs:v1'),
		durable_json_string('evidence-exact20-jcs:v1'),
		durable_json_string('git-tree-sha1:v1'),
	])!
	target := object_value_from_pairs(['target_id', 'target_path', 'target_mode', 'target_blob_oid',
		'target_sha256', 'generation', 'canonical_head', 'input_fingerprint', 'artifact_fingerprint',
		'manifest_hash', 'native_subject_hash', 'intent_id'], [
		durable_json_string(authenticated.target_id),
		durable_json_string(authenticated.entry.path),
		durable_json_string(authenticated.entry.mode),
		durable_json_string(authenticated.entry.oid),
		durable_json_string(authenticated.source_sha256),
		durable_json_integer(authenticated.model.generation),
		durable_json_string(authenticated.model.canonical_observed_sha),
		durable_json_string(authenticated.model.input_fingerprint),
		durable_json_string(authenticated.model.artifact_fingerprint),
		durable_json_string(authenticated.model.manifest_hash),
		durable_json_null(),
		durable_json_null(),
	])!
	resolved_source := object_value_from_pairs(['id', 'repository', 'ref', 'sha', 'tree'], [
		durable_json_string(binding.source.id),
		durable_json_string(binding.source.repository),
		durable_json_string(binding.source.ref),
		durable_json_string(binding.source.sha),
		durable_json_string(binding.source.tree),
	])!
	source_check := object_value_from_pairs(['source_id', 'resolved_sha', 'status', 'evidence_digest'], [
		durable_json_string(binding.check.source_id),
		durable_json_string(binding.check.resolved_sha),
		durable_json_string(binding.check.status),
		durable_json_string(binding.check.evidence_digest),
	])!
	source := object_value_from_pairs(['source_id', 'resolved_source', 'source_check'], [
		durable_json_string(invocation.source_id),
		resolved_source,
		source_check,
	])!
	invocation_json := object_value_from_pairs(['run_id', 'run_attempt', 'operation_ordinal',
		'cas_attempt', 'workflow', 'workflow_ref', 'workflow_sha', 'observed_at'], [
		durable_json_integer(invocation.run_id),
		durable_json_integer(i64(invocation.run_attempt)),
		durable_json_integer(i64(invocation.operation_ordinal)),
		durable_json_integer(1),
		durable_json_string(invocation.workflow),
		durable_json_string('master'),
		durable_json_string(invocation.workflow_sha),
		durable_json_string(invocation.observed_at),
	])!
	result := if event == .ledger_repaired_with_blockers { 'blocked' } else { 'passed' }
	identity_template := object_value_from_pairs(['audience', 'run_id', 'run_attempt', 'ordinal',
		'cas_attempt', 'subject_id', 'transition', 'expected_generation', 'expected_canonical_head',
		'source_ref', 'source_sha', 'subject_fingerprint', 'input_fingerprint',
		'artifact_fingerprint', 'manifest_hash', 'native_subject_hash', 'intent_id'], [
		durable_json_string(durable_plan_audience),
		durable_json_integer(invocation.run_id),
		durable_json_integer(i64(invocation.run_attempt)),
		durable_json_integer(i64(invocation.operation_ordinal)),
		durable_json_integer(1),
		durable_json_string(authenticated.target_id),
		durable_json_string(event.str()),
		durable_json_integer(authenticated.model.generation),
		durable_json_string(authenticated.model.canonical_observed_sha),
		durable_json_string(binding.source.ref),
		durable_json_string(binding.source.sha),
		durable_json_string('$plan_subject_fingerprint'),
		durable_json_string(authenticated.model.input_fingerprint),
		durable_json_string(authenticated.model.artifact_fingerprint),
		durable_json_string(authenticated.model.manifest_hash),
		durable_json_string('0'.repeat(64)),
		durable_json_null(),
	])!
	normalized_context := object_value_from_pairs(['operation_id'], [
		durable_json_string('$operation_id'),
	])!
	transition := object_value_from_pairs(['event', 'result', 'lane', 'operation_identity_template',
		'normalized_context'], [durable_json_string(event.str()),
		durable_json_string(result), durable_json_string(event.str()), identity_template,
		normalized_context])!
	postimage_policy := object_value_from_pairs(['target_replace_path', 'evidence_create_only',
		'evidence_exact20_member_names', 'evidence_path_grammar', 'changed_path_count',
		'preserve_all_other_entries'], [durable_json_string(authenticated.entry.path),
		durable_json_bool(true),
		durable_json_array(durable_evidence_member_names.map(durable_json_string(it))),
		durable_json_string('evidence/YYYY/MM/run/attempt/subject/operation/generation-transition-subject.json'),
		durable_json_integer(2), durable_json_bool(true)])!
	return object_value_from_pairs(['schema_version', 'domain', 'operation_audience', 'cas_attempt',
		'authority', 'inventory', 'contracts', 'target', 'source', 'invocation', 'transition',
		'postimage_policy'], [durable_json_integer(1),
		durable_json_string(durable_plan_subject_domain), durable_json_string(durable_plan_audience),
		durable_json_integer(1), authority, inventory, contracts, target, source, invocation_json,
		transition, postimage_policy])!
}

fn durable_plan_authority_json(proof LiveStateCommitProof) !JsonValue {
	return object_value_from_pairs(['repository', 'ref', 'commit_sha', 'remote_head', 'tree_sha',
		'parent_shas', 'verification_verified', 'verification_reason', 'verified_at',
		'state_writer_app_id', 'actor_login', 'actor_node_id', 'actor_database_id', 'actor_type'], [
		durable_json_string(proof.repository),
		durable_json_string(proof.ref),
		durable_json_string(proof.commit_sha),
		durable_json_string(proof.remote_head),
		durable_json_string(proof.tree_sha),
		durable_json_array(proof.parent_shas.map(durable_json_string(it))),
		durable_json_bool(proof.verification_verified),
		durable_json_string(proof.verification_reason),
		durable_json_string(proof.verified_at),
		durable_json_integer(proof.state_writer_app_id),
		durable_json_string(proof.actor_login),
		durable_json_string(proof.actor_node_id),
		durable_json_integer(proof.actor_database_id),
		durable_json_string(proof.actor_type),
	])!
}

fn durable_planned_evidence_path(invocation DurableTargetPlanInvocation, target_id string,
	operation_id string, generation i64, transition string, subject_fingerprint string) !string {
	exact_timestamp_unix(invocation.observed_at)!
	return evidence_path(invocation.observed_at[..4].int(), invocation.observed_at[5..7].int(),
		invocation.run_id, invocation.run_attempt, target_id, operation_id, generation, transition,
		subject_fingerprint)
}

fn validate_durable_global_collision_absent(entries []DurablePlanInventoryRecord,
	operation_id string, planned_path string) ! {
	for entry in entries {
		if entry.path == planned_path {
			return error('durable target evidence path already exists in the predecessor inventory')
		}
		if entry.path.starts_with('evidence/') {
			segments := entry.path.split('/')
			if segments.len > 6 && segments[6] == operation_id {
				return error('durable target operation ID collides with an existing evidence path')
			}
		}
		root := parse_strict_json(entry.source)!
		if durable_json_contains_semantic_collision(root, '', operation_id, planned_path) {
			return error('durable target operation ID or evidence path collides with durable state')
		}
	}
}

fn durable_json_contains_semantic_collision(value JsonValue, parent_key string,
	operation_id string, planned_path string) bool {
	if value.kind == .string_value {
		if parent_key in ['evidence_path', 'business_evidence_path']
			&& value.string_value == planned_path {
			return true
		}
		if durable_collision_identity_key(parent_key) && value.string_value == operation_id {
			return true
		}
		return false
	}
	if value.kind == .array {
		for child in value.array_value {
			if durable_json_contains_semantic_collision(child, parent_key, operation_id,
				planned_path)
			{
				return true
			}
		}
		return false
	}
	if value.kind == .object {
		for index, key in value.object_keys {
			if durable_json_contains_semantic_collision(value.object_values[index], key,
				operation_id, planned_path)
			{
				return true
			}
		}
	}
	return false
}

fn durable_collision_identity_key(key string) bool {
	return key == 'operation_id' || key == 'operation_ids' || key.ends_with('_operation_id')
		|| key.ends_with('_operation_ids') || key == 'intent_id' || key == 'consumer_id'
		|| key == 'incident_id' || key == 'incident_ids' || key == 'handoff_id'
		|| key == 'active_recovery_handoff_id' || key == 'active_remediation_id'
		|| key == 'created_by_operation_id' || key == 'waiting_consumers'
		|| key in ['consumer_ids', 'intent_ids', 'handoff_ids', 'active_intent_id', 'predecessor_handoff_id', 'successor_handoff_id']
}

fn durable_plan_postimage_entries(entries []DurablePlanInventoryRecord,
	prepared PreparedTargetStateWrite,
	evidence PreparedDurableTargetEvidence) ![]DurablePlanInventoryRecord {
	durable_plan_require_exact2_slot(entries.len)!
	if prepared.source.len == 0 || prepared.source.len > durable_target_max_bytes
		|| evidence.source.len == 0 || evidence.source.len > durable_evidence_max_bytes
		|| !contract_relative_path_is_safe(evidence.path)
		|| evidence.path.len > durable_evidence_path_max_bytes {
		return error('durable target exact2 postimage violates its pre-construction bounds')
	}
	mut target_count := 0
	mut evidence_count := 0
	mut predecessor_bytes := i64(0)
	mut target_predecessor_size := i64(0)
	for entry in entries {
		if entry.size < 0 || predecessor_bytes > durable_plan_inventory_max_bytes - entry.size {
			return error('durable target predecessor inventory byte sum overflows its closed bound')
		}
		predecessor_bytes += entry.size
		if entry.path == prepared.target_path {
			target_count++
			target_predecessor_size = entry.size
		}
		if entry.path == evidence.path {
			evidence_count++
		}
	}
	if target_count != 1 || evidence_count != 0 {
		return error('durable target exact2 paths are absent or not create-only')
	}
	mut postimage_bytes := predecessor_bytes - target_predecessor_size
	for addition in [i64(prepared.source.len), i64(evidence.source.len)] {
		if addition < 0 || postimage_bytes > durable_plan_inventory_max_bytes - addition {
			return error('durable target exact2 postimage exceeds its global byte bound')
		}
		postimage_bytes += addition
	}
	target_record := DurablePlanInventoryRecord{
		path:   prepared.target_path
		mode:   '100644'
		kind:   'blob'
		oid:    prepared.resulting_blob_oid
		size:   i64(prepared.source.len)
		sha256: prepared.resulting_source_sha256
		source: prepared.source
	}
	evidence_record := DurablePlanInventoryRecord{
		path:   evidence.path
		mode:   '100644'
		kind:   'blob'
		oid:    evidence.blob_oid
		size:   i64(evidence.source.len)
		sha256: evidence.sha256
		source: evidence.source
	}
	mut result := []DurablePlanInventoryRecord{cap: entries.len + 1}
	mut evidence_inserted := false
	for entry in entries {
		if !evidence_inserted && evidence_record.path < entry.path {
			result << evidence_record
			evidence_inserted = true
		}
		if entry.path == prepared.target_path {
			result << target_record
		} else {
			result << DurablePlanInventoryRecord{
				...entry
				source: entry.source.clone()
			}
		}
	}
	if !evidence_inserted {
		result << evidence_record
	}
	validate_durable_inventory_pathset(result)!
	return result
}

fn durable_plan_require_exact2_slot(predecessor_count int) ! {
	if predecessor_count <= 0 || predecessor_count >= durable_plan_inventory_max_files {
		return error('durable target predecessor inventory has no bounded slot for exact2 creation')
	}
}

fn durable_plan_record_exact2_change(path string, prepared_path string, evidence_path string,
	mut changes DurablePlanExact2Changes) ! {
	changes.count++
	if changes.count > 2 {
		return error('durable target postimage contains more than the exact two permitted changes')
	}
	if path == prepared_path && !changes.target_changed {
		changes.target_changed = true
		return
	}
	if path == evidence_path && !changes.evidence_changed {
		changes.evidence_changed = true
		return
	}
	return error('durable target postimage is not the exact target replacement plus evidence creation')
}

fn validate_durable_plan_exact2(predecessor DurablePlanPhysicalSnapshot,
	postimage []DurablePlanInventoryRecord, prepared PreparedTargetStateWrite,
	evidence PreparedDurableTargetEvidence, post_tree_oid string) ! {
	_ = validate_durable_plan_exact2_linear(predecessor, postimage, prepared, evidence,
		post_tree_oid)!
}

fn validate_durable_plan_exact2_linear(predecessor DurablePlanPhysicalSnapshot,
	postimage []DurablePlanInventoryRecord, prepared PreparedTargetStateWrite,
	evidence PreparedDurableTargetEvidence, post_tree_oid string) !DurablePlanLinearCounters {
	durable_plan_require_exact2_slot(predecessor.entries.len)!
	if postimage.len != predecessor.entries.len + 1
		|| postimage.len > durable_plan_inventory_max_files || !is_lower_hex_40(post_tree_oid)
		|| prepared.source.len == 0 || prepared.source.len > durable_target_max_bytes
		|| evidence.source.len == 0 || evidence.source.len > durable_evidence_max_bytes
		|| evidence.path.len > durable_evidence_path_max_bytes {
		return error('durable target exact2 postimage violates its file, byte, or tree bounds')
	}
	// Fixed scalar bookkeeping refuses a third difference at observation time. It neither
	// allocates a change list nor sorts attacker-influenced paths after the comparison.
	mut changes := DurablePlanExact2Changes{}
	mut total_bytes := i64(0)
	mut predecessor_index := 0
	mut postimage_index := 0
	mut predecessor_steps := 0
	mut postimage_steps := 0
	for postimage_index < postimage.len {
		entry := postimage[postimage_index]
		postimage_steps++
		if entry.size < 0 || total_bytes > durable_plan_inventory_max_bytes - entry.size {
			return error('durable target exact2 postimage byte sum exceeds its closed bound')
		}
		total_bytes += entry.size
		if predecessor_index >= predecessor.entries.len {
			durable_plan_record_exact2_change(entry.path, prepared.target_path, evidence.path, mut
				changes)!
			postimage_index++
			continue
		}
		previous := predecessor.entries[predecessor_index]
		if entry.path < previous.path {
			durable_plan_record_exact2_change(entry.path, prepared.target_path, evidence.path, mut
				changes)!
			postimage_index++
			continue
		}
		if entry.path > previous.path {
			return error('durable target exact2 postimage deletes a predecessor path')
		}
		predecessor_steps++
		if previous != entry {
			durable_plan_record_exact2_change(entry.path, prepared.target_path, evidence.path, mut
				changes)!
		}
		predecessor_index++
		postimage_index++
	}
	if predecessor_index != predecessor.entries.len {
		return error('durable target exact2 postimage omits a predecessor suffix')
	}
	if changes.count != 2 || !changes.target_changed || !changes.evidence_changed
		|| total_bytes > durable_plan_inventory_max_bytes {
		return error('durable target postimage is not the exact target replacement plus evidence creation')
	}
	return DurablePlanLinearCounters{
		parsed_records:      predecessor.entries.len
		exact2_predecessors: predecessor_steps
		exact2_postimages:   postimage_steps
	}
}

fn validate_durable_plan_passes_match(first DurablePlanPass, second DurablePlanPass) ! {
	if first.proof != second.proof {
		return error('durable target state proof changed between independent planner passes')
	}
	if first.inventory != second.inventory
		|| first.predecessor_tree_records != second.predecessor_tree_records
		|| first.predecessor_tree_oid != second.predecessor_tree_oid {
		return error('durable target physical inventory or predecessor tree changed between passes')
	}
	if first.schema_closure != second.schema_closure {
		return error('durable target schema closure changed between independent passes')
	}
	if !json_equal(first.target_root, second.target_root)
		|| first.target_model != second.target_model
		|| first.source_binding != second.source_binding {
		return error('durable target model or selected source changed between passes')
	}
	if first.plan_subject_source != second.plan_subject_source
		|| first.plan_subject_fingerprint != second.plan_subject_fingerprint
		|| first.identity != second.identity || first.operation_id != second.operation_id {
		return error('durable target plan subject or private operation identity changed between passes')
	}
	if first.prepared != second.prepared {
		return error('durable target prepared target-state write changed between passes')
	}
	if first.evidence != second.evidence {
		return error('durable target prepared evidence changed between passes')
	}
	if first.postimage_entries != second.postimage_entries
		|| first.postimage_tree_records != second.postimage_tree_records
		|| first.postimage_tree_oid != second.postimage_tree_oid
		|| first.changed_paths != second.changed_paths {
		return error('durable target exact2 postimage or reconstructed tree changed between passes')
	}
}

fn durable_plan_observation(pass DurablePlanPass) DurableTargetCommitPlanObservation {
	mut changed_blobs := []DurableTargetChangedBlob{}
	for path in pass.changed_paths {
		entry := durable_inventory_entry(pass.postimage_entries, path) or { continue }
		changed_blobs << DurableTargetChangedBlob{
			path:     entry.path
			mode:     entry.mode
			blob_oid: entry.oid
			sha256:   entry.sha256
			size:     entry.size
			source:   entry.source
		}
	}
	return DurableTargetCommitPlanObservation{
		state_commit:             pass.proof.commit_sha
		predecessor_tree_oid:     pass.predecessor_tree_oid
		postimage_tree_oid:       pass.postimage_tree_oid
		target_id:                pass.target_model.target_id
		event:                    pass.prepared.transition
		result:                   if pass.prepared.transition == 'ledger_repaired_with_blockers' {
			'blocked'
		} else {
			'passed'
		}
		operation_id:             pass.operation_id
		plan_subject_fingerprint: pass.plan_subject_fingerprint
		evidence_path:            pass.evidence.path
		changed_paths:            pass.changed_paths.clone()
		changed_blobs:            changed_blobs
	}
}

fn clone_durable_plan_observation(value DurableTargetCommitPlanObservation) DurableTargetCommitPlanObservation {
	mut blobs := []DurableTargetChangedBlob{cap: value.changed_blobs.len}
	for blob in value.changed_blobs {
		blobs << DurableTargetChangedBlob{
			...blob
			source: blob.source.clone()
		}
	}
	return DurableTargetCommitPlanObservation{
		...value
		changed_paths: value.changed_paths.clone()
		changed_blobs: blobs
	}
}

fn clone_durable_inventory(entries []DurablePlanInventoryRecord) []DurablePlanInventoryRecord {
	mut result := []DurablePlanInventoryRecord{cap: entries.len}
	for entry in entries {
		result << DurablePlanInventoryRecord{
			...entry
			source: entry.source.clone()
		}
	}
	return result
}

fn durable_inventory_entry(entries []DurablePlanInventoryRecord,
	path string) !DurablePlanInventoryRecord {
	matches := entries.filter(it.path == path)
	if matches.len != 1 {
		return error('durable target inventory lacks one exact path')
	}
	return matches[0]
}

fn durable_inventory_indices(entries []DurablePlanInventoryRecord) !map[string]int {
	mut result := map[string]int{}
	for index, entry in entries {
		if entry.path in result {
			return error('durable target inventory path index contains a duplicate')
		}
		result[entry.path] = index
	}
	return result
}

fn durable_inventory_entry_at(entries []DurablePlanInventoryRecord, indices map[string]int,
	path string) !DurablePlanInventoryRecord {
	index := indices[path] or {
		return error('durable target inventory lacks one exact indexed path')
	}
	if index < 0 || index >= entries.len || entries[index].path != path {
		return error('durable target inventory path index is inconsistent')
	}
	return entries[index]
}

fn reduce_durable_commit_outcome(attempt int, outcome string) !DurableCommitRetryDecision {
	if attempt < 1 || attempt > 3 {
		return error('durable target reducer attempt is outside one through three')
	}
	return match outcome {
		'ambiguous' {
			DurableCommitRetryDecision{
				verdict:      'reconcile_required'
				next_attempt: attempt
			}
		}
		'confirmed_conflict' {
			if attempt == 1 {
				DurableCommitRetryDecision{
					verdict:       'replan_required'
					next_attempt:  2
					delay_seconds: 1
				}
			} else if attempt == 2 {
				DurableCommitRetryDecision{
					verdict:       'replan_required'
					next_attempt:  3
					delay_seconds: 3
				}
			} else {
				DurableCommitRetryDecision{
					verdict:      'unknown_blocked'
					next_attempt: 3
				}
			}
		}
		else {
			DurableCommitRetryDecision{
				verdict:      'unknown_blocked'
				next_attempt: attempt
			}
		}
	}
}

$if test {
	pub struct DurablePlanLinearCountersForTest {
	pub:
		parsed_records      int
		path_components     int
		tree_parts          int
		exact2_predecessors int
		exact2_postimages   int
	}

	pub struct DurableCommitRetryDecisionForTest {
	pub:
		verdict       string
		next_attempt  int
		delay_seconds int
	}

	pub fn reduce_durable_commit_outcome_for_test(attempt int,
	outcome string) !DurableCommitRetryDecisionForTest {
		decision := reduce_durable_commit_outcome(attempt, outcome)!
		return DurableCommitRetryDecisionForTest{
			verdict:       decision.verdict
			next_attempt:  decision.next_attempt
			delay_seconds: decision.delay_seconds
		}
	}

	pub fn durable_target_event_allowed_for_test(event TransitionEvent) bool {
		validate_durable_plan_event(event) or { return false }
		return true
	}

	pub fn parse_durable_plan_tree_listing_for_test(source string) ![]string {
		entries := parse_durable_plan_tree_listing(source)!
		return entries.map('${it.mode} ${it.kind} ${it.oid} ${it.size}\t${it.path}')
	}

	pub fn durable_plan_tree_linear_probe_for_test(paths []string) !DurablePlanLinearCountersForTest {
		if paths.len == 0 {
			return error('durable target linear tree probe is empty')
		}
		mut entries := []DurablePlanInventoryRecord{cap: paths.len}
		mut previous := ''
		mut components := 0
		for path in paths {
			if !contract_relative_path_is_safe(path) || (previous != '' && path <= previous) {
				return error('durable target linear tree probe paths are noncanonical')
			}
			components += path.split('/').len
			entries << DurablePlanInventoryRecord{
				path: path
				mode: '100644'
				kind: 'blob'
				oid:  git_blob_oid(path.bytes())
				size: i64(path.len)
			}
			previous = path
		}
		_, _, tree_parts := rebuild_durable_plan_tree_linear(entries)!
		return DurablePlanLinearCountersForTest{
			parsed_records:  entries.len
			path_components: components
			tree_parts:      tree_parts
		}
	}

	pub fn durable_plan_exact2_capacity_probe_for_test(predecessor_count int) !DurablePlanLinearCountersForTest {
		if predecessor_count < 2 {
			return error('durable target exact2 capacity probe count is invalid')
		}
		// This boundary seam calls the same pre-allocation guard as the physical loader,
		// postimage builder, and exact2 comparator; it does not allocate 100k fake records.
		durable_plan_require_exact2_slot(predecessor_count)!
		return DurablePlanLinearCountersForTest{
			parsed_records:      predecessor_count
			exact2_predecessors: predecessor_count
			exact2_postimages:   predecessor_count + 1
		}
	}

	pub fn durable_plan_exact2_change_cap_for_test(paths []string, target_path string,
	evidence_path string) !int {
		mut changes := DurablePlanExact2Changes{}
		for path in paths {
			durable_plan_record_exact2_change(path, target_path, evidence_path, mut changes)!
		}
		return changes.count
	}

	pub fn validate_durable_plan_comparator_mutations_for_test(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, invocation DurableTargetPlanInvocation) ![]string {
		mut session := durable_git_runner_begin(state_git_dir)!
		mut failure := ''
		rejected := validate_durable_plan_comparator_mutations_with_session_for_test(automation_root,
			state_git_dir, trust, proof_bundle_dir, target_id, event, invocation) or {
			failure = err.msg()
			[]string{}
		}
		if failure != '' {
			durable_git_runner_end(mut session) or {}
			return error(failure)
		}
		durable_git_runner_end(mut session)!
		return rejected
	}

	fn validate_durable_plan_comparator_mutations_with_session_for_test(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, invocation DurableTargetPlanInvocation) ![]string {
		first := prepare_durable_target_commit_plan_pass(automation_root, state_git_dir, trust,
			proof_bundle_dir, target_id, event, invocation)!
		second := prepare_durable_target_commit_plan_pass(automation_root, state_git_dir, trust,
			proof_bundle_dir, target_id, event, invocation)!
		labels := ['proof', 'inventory', 'schema', 'target', 'source', 'commitment', 'identity',
			'target_postimage', 'evidence', 'post_tree']
		mut rejected := []string{}
		for label in labels {
			mutated := mutate_durable_plan_pass_for_test(second, label)!
			validate_durable_plan_passes_match(first, mutated) or {
				rejected << label
				continue
			}
			return error('durable target independent-pass comparator accepted ${label}')
		}
		return rejected
	}

	pub fn durable_plan_between_pass_physical_mutation_for_test(automation_root string,
	state_git_dir string, trust LiveStateTrust, proof_bundle_dir string, target_id string,
	event TransitionEvent, invocation DurableTargetPlanInvocation,
	mutated_head_source string) !string {
		mut session := durable_git_runner_begin(state_git_dir)!
		head_path := os.join_path(proof_bundle_dir, 'head.json')
		original := os.read_file(head_path) or {
			durable_git_runner_end(mut session) or {}
			return err
		}
		_ = prepare_durable_target_commit_plan_pass(automation_root, state_git_dir, trust,
			proof_bundle_dir, target_id, event, invocation) or {
			durable_git_runner_end(mut session) or {}
			return err
		}
		os.write_file(head_path, mutated_head_source) or {
			durable_git_runner_end(mut session) or {}
			return err
		}
		mut detected := ''
		second := prepare_durable_target_commit_plan_pass(automation_root, state_git_dir, trust,
			proof_bundle_dir, target_id, event, invocation) or {
			detected = err.msg()
			DurablePlanPass{}
		}
		_ = second
		os.write_file(head_path, original) or {
			durable_git_runner_end(mut session) or {}
			return err
		}
		durable_git_runner_end(mut session)!
		if detected == '' {
			return error('durable target independent second pass accepted a physical proof mutation')
		}
		return detected
	}

	pub fn durable_semantic_collision_for_test(source string, operation_id string,
	planned_path string) !bool {
		if !is_lower_hex_64(operation_id) || !contract_relative_path_is_safe(planned_path) {
			return error('durable target collision test identity is malformed')
		}
		return durable_json_contains_semantic_collision(parse_strict_json(source)!, '',
			operation_id, planned_path)
	}

	pub fn durable_schema_closure_paths_for_test(automation_root string) ![]string {
		closure := load_durable_plan_schema_closure(automation_root)!
		return closure.facts.map(it.relative_path)
	}

	pub fn validate_durable_null6_sources_for_test(sources []string) ! {
		if sources.len != managed_target_ids.len {
			return error('durable target null6 test requires exactly six target sources')
		}
		mut entries := []DurablePlanInventoryRecord{}
		for index, target_id in managed_target_ids {
			entries << DurablePlanInventoryRecord{
				path:   target_state_path(target_id)!
				mode:   '100644'
				kind:   'blob'
				oid:    git_blob_oid(sources[index].bytes())
				size:   i64(sources[index].len)
				sha256: sha256.sum256(sources[index].bytes()).hex()
				source: sources[index]
			}
		}
		validate_durable_plan_null6_and_terminal_absence(DurablePlanPhysicalSnapshot{
			entries:       entries
			entry_indices: durable_inventory_indices(entries)!
		})!
	}

	pub fn durable_managed_target_ids_for_test() []string {
		return managed_target_ids.clone()
	}

	fn mutate_durable_plan_pass_for_test(value DurablePlanPass,
	mutation string) !DurablePlanPass {
		if mutation == '' {
			return value
		}
		return match mutation {
			'proof' {
				DurablePlanPass{
					...value
					proof: LiveStateCommitProof{
						...value.proof
						actor_login: '${value.proof.actor_login}-mutated'
					}
				}
			}
			'inventory' {
				mut entries := clone_durable_inventory(value.inventory)
				entries[0] = DurablePlanInventoryRecord{
					...entries[0]
					sha256: 'f'.repeat(64)
				}
				DurablePlanPass{
					...value
					inventory: entries
				}
			}
			'schema' {
				mut closure := value.schema_closure
				mut facts := closure.facts.clone()
				facts[0] = DurablePlanSchemaFact{
					...facts[0]
					sha256: 'f'.repeat(64)
				}
				closure = DurablePlanSchemaClosure{
					facts: facts
				}
				DurablePlanPass{
					...value
					schema_closure: closure
				}
			}
			'target' {
				DurablePlanPass{
					...value
					target_model: TargetModel{
						...value.target_model
						generation: value.target_model.generation + 1
					}
				}
			}
			'source' {
				DurablePlanPass{
					...value
					source_binding: DurablePlanSourceBinding{
						...value.source_binding
						source: ResolvedSourceModel{
							...value.source_binding.source
							ref: '${value.source_binding.source.ref}-mutated'
						}
					}
				}
			}
			'commitment' {
				DurablePlanPass{
					...value
					plan_subject_fingerprint: 'f'.repeat(64)
				}
			}
			'identity' {
				DurablePlanPass{
					...value
					operation_id: 'f'.repeat(64)
				}
			}
			'target_postimage' {
				DurablePlanPass{
					...value
					prepared: PreparedTargetStateWrite{
						...value.prepared
						resulting_source_sha256: 'f'.repeat(64)
					}
				}
			}
			'evidence' {
				DurablePlanPass{
					...value
					evidence: PreparedDurableTargetEvidence{
						...value.evidence
						sha256: 'f'.repeat(64)
					}
				}
			}
			'post_tree' {
				DurablePlanPass{
					...value
					postimage_tree_oid: 'f'.repeat(40)
				}
			}
			else {
				return error('unknown durable target planner test mutation')
			}
		}
	}
}
