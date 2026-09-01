module bin

import os

const candidate_manifest_path = 'automation/bundle-manifest.json'
const candidate_transition_max_status_bytes = u64(1024 * 1024)

pub enum CandidateTransitionKind {
	monthly
	legacy_onboard
	baseline_activate
}

pub fn parse_candidate_transition_kind(source string) !CandidateTransitionKind {
	return match source {
		'monthly' {
			.monthly
		}
		'legacy-onboard' {
			.legacy_onboard
		}
		'baseline-activate' {
			.baseline_activate
		}
		else {
			return error('candidate transition kind must be monthly, legacy-onboard, or baseline-activate')
		}
	}
}

struct CandidateManifestGitState {
	present bool
	entry   GitBlobObservation
}

fn candidate_manifest_git_state(repository_root string,
	source_ref string) !CandidateManifestGitState {
	if !git_reference_is_safe(source_ref) {
		return error('candidate manifest lookup received an unsafe ref')
	}
	result := candidate_repository_git(repository_root, ['ls-tree', '-z', '--full-tree', source_ref,
		'--', candidate_manifest_path])!
	if result.exit_code != 0 {
		return error('candidate manifest Git tree lookup failed')
	}
	if result.output == '' {
		return CandidateManifestGitState{}
	}
	records := result.output.split('\x00')
	if records.len != 2 || records[1] != '' {
		return error('candidate manifest Git tree lookup was not exact')
	}
	parts := records[0].split_nth('\t', 2)
	if parts.len != 2 || parts[1] != candidate_manifest_path {
		return error('candidate manifest Git tree returned a different path')
	}
	metadata := parts[0].fields()
	if metadata.len != 3 || metadata[1] != 'blob' || metadata[0] !in ['100644', '100755', '120000']
		|| !is_lower_hex_40(metadata[2]) {
		return error('candidate manifest Git tree returned an unsupported object')
	}
	return CandidateManifestGitState{
		present: true
		entry:   GitBlobObservation{
			git_mode: metadata[0]
			oid:      metadata[2]
		}
	}
}

fn attest_candidate_manifest_present(repository_root string,
	source_ref string) !StagedPathObservation {
	state := candidate_manifest_git_state(repository_root, source_ref) or {
		return error('candidate manifest must be an exact physical 100644 Git blob before parsing')
	}
	if !state.present || state.entry.git_mode != '100644' {
		return error('candidate manifest must be an exact physical 100644 Git blob before parsing')
	}
	observed := observe_candidate_path(repository_root, candidate_manifest_path) or {
		return error('candidate manifest must be an exact physical 100644 Git blob before parsing')
	}
	if observed.kind != 'file' || !observed.identity_reliable || observed.nlink != 1
		|| observed.git_blob_oid != state.entry.oid {
		return error('candidate manifest must be an exact physical 100644 Git blob before parsing')
	}
	return observed
}

fn attest_legacy_candidate_manifest_absent(repository_root string, source_ref string) ! {
	state := candidate_manifest_git_state(repository_root, source_ref) or {
		return error('legacy onboarding base manifest must be absent from Git and checkout')
	}
	if state.present {
		return error('legacy onboarding base manifest must be absent from Git and checkout')
	}
	parts := candidate_manifest_path.split('/')
	mut current := repository_root
	for part in parts[..parts.len - 1] {
		current = os.join_path(current, part)
		if os.is_link(current) {
			return error('legacy onboarding base manifest must be absent from Git and checkout')
		}
		if !os.exists(current) {
			return
		}
		stat := os.lstat(current) or {
			return error('legacy onboarding base manifest must be absent from Git and checkout')
		}
		if stat.get_filetype() != .directory {
			return error('legacy onboarding base manifest must be absent from Git and checkout')
		}
	}
	manifest_path := os.join_path(repository_root, candidate_manifest_path)
	if os.exists(manifest_path) || os.is_link(manifest_path) {
		return error('legacy onboarding base manifest must be absent from Git and checkout')
	}
}

// evaluate_candidate_manifest_for_execution proves one closed, direct-child candidate transition,
// exports only its declared payload into a fresh physical tree, and delegates the final verdict to
// the sole staged authenticator.
pub fn evaluate_candidate_manifest_for_execution(automation_root string, target_id string,
	kind CandidateTransitionKind, candidate_repo_root string, base_sha string, candidate_sha string,
	work_root string, runtime RuntimeContractBinding, publish_requested bool) !StagedManifestEligibility {
	attest_runtime_contract_binding(runtime)!
	if target_id !in managed_target_ids {
		return error('candidate transition target is not managed')
	}
	mut onboarding_policy := JsonValue{}
	mut activation_binding := ManagedBaselineActivationBinding{}
	mut activation_policy := JsonValue{}
	if kind == .legacy_onboard {
		_, onboarding_policy = reviewed_legacy_onboarding_binding(automation_root, target_id,
			base_sha)!
	} else if kind == .baseline_activate {
		activation_binding, activation_policy = reviewed_managed_baseline_activation_binding(automation_root,
			target_id, base_sha)!
	}
	if kind == .baseline_activate && publish_requested {
		return error('managed baseline activation publication is disabled pending native publication proof')
	}
	if !is_lower_hex_40(base_sha) || !is_lower_hex_40(candidate_sha) || base_sha == candidate_sha {
		return error('candidate base and candidate must be distinct full lowercase commit SHAs')
	}
	if !os.is_dir(candidate_repo_root) || os.is_link(candidate_repo_root) {
		return error('candidate repository root must be a physical directory')
	}
	candidate_root := canonical_contract_root(candidate_repo_root)!
	contract_root := canonical_contract_root(os.join_path(automation_root, '..', '..'))!
	new_work_root := canonical_candidate_work_root(work_root)!
	if roots_overlap(candidate_root, new_work_root) || roots_overlap(contract_root, new_work_root)
		|| roots_overlap(candidate_root, contract_root) {
		return error('candidate work root must be physically separate from source repositories')
	}
	if os.exists(new_work_root) || os.is_link(new_work_root) {
		return error('candidate work root must not already exist')
	}
	validate_candidate_repository(candidate_root, base_sha, candidate_sha)!

	os.mkdir(new_work_root, mode: 0o700)!
	mut keep_work_root := false
	defer {
		if !keep_work_root {
			os.rmdir_all(new_work_root) or {}
		}
	}
	source_root := os.join_path(new_work_root, 'candidate-source')
	base_root := os.join_path(new_work_root, 'base-source')
	payload_root := os.join_path(new_work_root, 'payload')
	clone_candidate_source(candidate_root, source_root, candidate_sha)!
	clone_candidate_source(candidate_root, base_root, base_sha)!

	manifest_path := os.join_path(source_root, candidate_manifest_path)
	base_manifest_path := os.join_path(base_root, candidate_manifest_path)
	attest_candidate_manifest_present(source_root, candidate_sha)!
	if kind != .legacy_onboard {
		attest_candidate_manifest_present(base_root, base_sha)!
	} else {
		attest_legacy_candidate_manifest_absent(base_root, base_sha)!
	}
	issues := validate_manifest(automation_root, manifest_path)!
	if issues.len > 0 {
		return error('candidate manifest schema or semantics failed with ${issues.len} issue(s)')
	}
	manifest := parse_strict_json(os.read_file(manifest_path)!)!
	if require_string_member(manifest, 'target_id')! != target_id {
		return error('candidate manifest target differs from the requested target')
	}
	if require_string_member(manifest, 'contract_repository')! != runtime.repository
		|| require_string_member(manifest, 'contract_sha')! != runtime.sha {
		return error('runtime contract binding differs from the authenticated manifest')
	}
	if kind == .monthly {
		base_issues := validate_manifest(automation_root, base_manifest_path)!
		if base_issues.len > 0 {
			return error('base manifest schema or semantics failed with ${base_issues.len} issue(s)')
		}
		base_manifest := parse_strict_json(os.read_file(base_manifest_path)!)!
		validate_candidate_base_controls(base_root, base_sha, base_manifest_path, base_manifest)!
		validate_candidate_transition(base_root, base_sha, candidate_sha, base_manifest, manifest)!
	} else if kind == .legacy_onboard {
		validate_manifest_legacy_onboarding_policy(manifest, onboarding_policy)!
		validate_legacy_onboarding_base_controls(automation_root, base_root, base_sha, manifest)!
		validate_legacy_onboarding_transition(automation_root, base_root, base_sha, candidate_sha,
			manifest)!
	} else {
		base_manifest := attest_managed_baseline_activation_base(automation_root, target_id,
			base_root, base_sha, base_manifest_path, activation_binding)!
		validate_managed_baseline_activation_candidate(base_manifest, manifest, runtime,
			activation_binding, activation_policy, contract_root)!
		validate_managed_baseline_activation_transition(base_root, base_sha, candidate_sha)!
	}
	export_candidate_payload(source_root, payload_root, manifest)!
	validate_independent_candidate_checkout(base_root, base_sha)!
	validate_independent_candidate_checkout(source_root, candidate_sha)!
	validate_candidate_repository(candidate_root, base_sha, candidate_sha)!

	decision := evaluate_staged_manifest_for_execution(automation_root, manifest_path, StagingContract{
		staging_root:    payload_root
		source_git_root: source_root
		source_git_ref:  candidate_sha
	}, runtime, publish_requested)!
	if kind == .baseline_activate && !decision.eligible {
		return error('managed baseline activation preflight did not authenticate the candidate')
	}
	keep_work_root = true
	return decision
}

fn attest_managed_baseline_activation_base(automation_root string, target_id string,
	base_root string, base_sha string, manifest_path string,
	binding ManagedBaselineActivationBinding) !JsonValue {
	if base_sha != binding.base_sha {
		return error('managed baseline activation base differs from the reviewed target pin')
	}
	tree := successful_candidate_git(base_root, ['rev-parse', '${base_sha}^{tree}'],
		'managed baseline activation base tree cannot be resolved')!.trim_space()
	if tree != binding.base_tree {
		return error('managed baseline activation base tree differs from the reviewed pin')
	}
	raw_commit := successful_candidate_git(base_root, ['cat-file', 'commit', base_sha],
		'managed baseline activation base commit cannot be inspected')!
	if !raw_commit_has_exact_parent(raw_commit, binding.parent_sha) {
		return error('managed baseline activation base parent differs from the reviewed pin')
	}
	manifest_observation := attest_candidate_manifest_present(base_root, base_sha)!
	if manifest_observation.sha256 != binding.base_manifest_sha256
		|| manifest_observation.sha256 != sha256_file(manifest_path)! {
		return error('managed baseline activation base manifest differs from the reviewed pin')
	}
	base_issues := validate_manifest(automation_root, manifest_path)!
	if base_issues.len > 0 {
		return error('managed baseline activation base manifest failed with ${base_issues.len} issue(s)')
	}
	manifest := parse_strict_json(os.read_file(manifest_path)!)!
	if require_string_member(manifest, 'target_id')! != target_id {
		return error('managed baseline activation base target differs from the request')
	}
	if require_string_member(manifest, 'contract_repository')! != binding.base_contract_repository
		|| require_string_member(manifest, 'contract_sha')! != binding.base_contract_sha {
		return error('managed baseline activation base contract differs from the reviewed pin')
	}
	if require_string_member(manifest, 'contract_mode')! != 'production'
		|| require_string_member(manifest, 'provenance_status')! != 'incomplete' {
		return error('managed baseline activation base must be the reviewed incomplete production manifest')
	}
	profile_id, profile_sha256, producer := manifest_toolchain_members(manifest)!
	if profile_id != '' || profile_sha256 != '' || producer.kind != .null_value {
		return error('managed baseline activation base toolchain must remain entirely unresolved')
	}
	for source in require_array_member(manifest, 'sources')! {
		if require_member(source, 'sha')!.kind != .null_value
			|| require_member(source, 'tree')!.kind != .null_value {
			return error('managed baseline activation base sources must remain entirely unresolved')
		}
	}
	for collection in ['overlays', 'inventory', 'outputs'] {
		for entry in require_array_member(manifest, collection)! {
			provenance := require_object_member(entry, 'provenance')!
			if require_string_member(provenance, 'status')! != 'incomplete'
				|| require_member(provenance, 'repository')!.kind != .null_value
				|| require_member(provenance, 'sha')!.kind != .null_value
				|| require_member(provenance, 'source_path')!.kind != .null_value
				|| require_member(provenance, 'license')!.kind != .null_value {
				return error('managed baseline activation base payload provenance must remain entirely unresolved')
			}
		}
	}
	validate_candidate_base_controls(base_root, base_sha, manifest_path, manifest)!
	return manifest
}

fn validate_managed_baseline_activation_candidate(base_manifest JsonValue, manifest JsonValue,
	runtime RuntimeContractBinding, binding ManagedBaselineActivationBinding, policy JsonValue,
	contract_root string) ! {
	validate_manifest_managed_baseline_activation_policy(manifest, policy)!
	evidence := managed_baseline_source_commit_evidence(policy)!
	if runtime.repository != binding.base_contract_repository
		|| require_string_member(manifest, 'contract_repository')! != binding.base_contract_repository
		|| require_string_member(manifest, 'contract_mode')! != require_string_member(base_manifest, 'contract_mode')!
		|| require_string_member(manifest, 'contract_mode')! != 'production' {
		return error('managed baseline activation candidate must retain the production contract authority')
	}
	if runtime.sha == binding.base_contract_sha {
		return error('managed baseline activation runtime contract must differ from the reviewed base')
	}
	if require_string_member(manifest, 'v_source_sha')! != runtime.sha {
		return error('managed baseline activation V source must equal the runtime contract SHA')
	}
	for key in ['schema_version', 'contract_version', 'target_id', 'branch', 'recipe', 'patches',
		'transforms', 'header_effects', 'integrations', 'probes', 'affected_targets'] {
		if !json_equal(require_member(base_manifest, key)!, require_member(manifest, key)!) {
			return error('managed baseline activation changed immutable manifest policy')
		}
	}
	base_sources := require_array_member(base_manifest, 'sources')!
	sources := require_array_member(manifest, 'sources')!
	if base_sources.len != sources.len {
		return error('managed baseline activation changed the reviewed source set')
	}
	mut runtime_tree := ''
	for authority in evidence {
		if authority.authority == 'runtime-contract' {
			runtime_tree = validate_managed_baseline_runtime_contract_checkout(contract_root,
				runtime)!
		}
	}
	for index, source in sources {
		base_source := base_sources[index]
		for key in ['id', 'repository', 'ref'] {
			if !json_equal(require_member(base_source, key)!, require_member(source, key)!) {
				return error('managed baseline activation changed immutable source policy')
			}
		}
		if require_member(base_source, 'sha')!.kind != .null_value
			|| require_member(base_source, 'tree')!.kind != .null_value {
			return error('managed baseline activation base sources must remain entirely unresolved')
		}
		sha := require_string_member(source, 'sha')!
		tree := require_string_member(source, 'tree')!
		if !is_lower_hex_40(sha) || !is_lower_hex_40(tree) {
			return error('managed baseline activation candidate sources must resolve to exact Git objects')
		}
		mut evidence_matches := 0
		for authority in evidence {
			if authority.id != require_string_member(source, 'id')! {
				continue
			}
			evidence_matches++
			expected_sha := if authority.authority == 'runtime-contract' {
				runtime.sha
			} else {
				authority.sha
			}
			expected_tree := if authority.authority == 'runtime-contract' {
				runtime_tree
			} else {
				authority.tree
			}
			if sha != expected_sha || tree != expected_tree {
				return error('managed baseline activation source differs from reviewed commit evidence')
			}
		}
		if evidence_matches != 1 {
			return error('managed baseline activation source has no unique reviewed commit evidence')
		}
	}
	for collection in ['overlays', 'inventory', 'outputs'] {
		base_entries := require_array_member(base_manifest, collection)!
		entries := require_array_member(manifest, collection)!
		if base_entries.len != entries.len {
			return error('managed baseline activation changed immutable payload membership')
		}
		for index, entry in entries {
			if !json_equal(managed_baseline_payload_entry_projection(base_entries[index])!,
				managed_baseline_payload_entry_projection(entry)!) {
				return error('managed baseline activation changed immutable payload bytes or policy')
			}
		}
	}
	status := require_string_member(manifest, 'provenance_status')!
	if status !in ['complete', 'opaque-accepted'] || !manifest_sources_are_resolved(manifest)!
		|| !manifest_toolchain_is_resolved(manifest)! {
		return error('managed baseline activation candidate must have complete resolved provenance')
	}
	validate_managed_baseline_activation_payload_provenance(manifest, binding, runtime, evidence)!
}

fn validate_managed_baseline_activation_payload_provenance(manifest JsonValue,
	binding ManagedBaselineActivationBinding, runtime RuntimeContractBinding,
	evidence []ManagedBaselineSourceCommitEvidence) ! {
	for collection in ['overlays', 'inventory', 'outputs'] {
		for entry in require_array_member(manifest, collection)! {
			provenance := require_object_member(entry, 'provenance')!
			repository := require_member(provenance, 'repository')!
			sha := require_member(provenance, 'sha')!
			if require_string_member(provenance, 'status')! == 'incomplete' {
				continue
			}
			if repository.kind != .string_value || sha.kind != .string_value {
				return error('managed baseline activation complete payload provenance must resolve repository and SHA')
			}
			expected_sha := match repository.string_value {
				'TinyCC/tinycc' {
					managed_baseline_activation_evidence_sha(evidence, 'tinycc', runtime)!
				}
				'ivmai/bdwgc' {
					managed_baseline_activation_evidence_sha(evidence, 'bdwgc', runtime)!
				}
				'bdwgc/libatomic_ops' {
					managed_baseline_activation_evidence_sha(evidence, 'libatomic_ops', runtime)!
				}
				'vlang/tccbin' {
					binding.base_sha
				}
				'vlang/v' {
					managed_baseline_activation_evidence_sha(evidence, 'v-libgc', runtime)!
				}
				else {
					return error('managed baseline activation payload provenance repository is not an authenticated authority')
				}
			}
			if sha.string_value != expected_sha {
				return error('managed baseline activation payload provenance SHA differs from its authenticated authority')
			}
		}
	}
}

fn managed_baseline_activation_evidence_sha(evidence []ManagedBaselineSourceCommitEvidence,
	source_id string, runtime RuntimeContractBinding) !string {
	mut matches := 0
	mut expected_sha := ''
	for authority in evidence {
		if authority.id == source_id {
			matches++
			expected_sha = if authority.authority == 'runtime-contract' {
				runtime.sha
			} else {
				authority.sha
			}
		}
	}
	if matches != 1 || !is_lower_hex_40(expected_sha) {
		return error('managed baseline activation payload provenance authority is absent or ambiguous')
	}
	return expected_sha
}

fn managed_baseline_payload_entry_projection(entry JsonValue) !JsonValue {
	return select_object_members(entry, ['path', 'kind', 'git_mode', 'sha256', 'symlink_target',
		'role', 'opaque', 'opaque_acceptance_id', 'format', 'object_type', 'machine', 'os_abi'])!
}

fn validate_managed_baseline_activation_transition(repository_root string, base_sha string,
	candidate_sha string) ! {
	diff := read_candidate_transition_status(repository_root, base_sha, candidate_sha)!
	if diff != 'M\x00${candidate_manifest_path}\x00' {
		return error('managed baseline activation transition must modify only the authoritative manifest')
	}
}

fn validate_candidate_base_controls(base_root string, base_sha string, manifest_path string,
	manifest JsonValue) ! {
	manifest_observation := attest_candidate_manifest_present(base_root, base_sha)!
	if manifest_observation.sha256 != sha256_file(manifest_path)! {
		return error('base manifest bytes do not match its immutable Git blob')
	}
	recipe := require_object_member(manifest, 'recipe')!
	mut issues := validate_git_input_hash(StagingContract{
		source_git_root: base_root
		source_git_ref:  base_sha
	}, require_string_member(recipe, 'path')!, require_string_member(recipe, 'sha256')!,
		'$/recipe', true)!
	for patch in require_array_member(manifest, 'patches')! {
		issues << validate_git_input_hash(StagingContract{
			source_git_root: base_root
			source_git_ref:  base_sha
		}, require_string_member(patch, 'path')!, require_string_member(patch, 'sha256')!,
			'$/patches', false)!
	}
	for transform in require_array_member(manifest, 'transforms')! {
		issues << validate_git_input_hash(StagingContract{
			source_git_root: base_root
			source_git_ref:  base_sha
		}, require_string_member(transform, 'path')!, require_string_member(transform, 'sha256')!,
			'$/transforms', false)!
	}
	if issues.len > 0 {
		return error('base manifest controls differ from their immutable Git blobs')
	}
}

fn validate_legacy_onboarding_base_controls(automation_root string, base_root string,
	base_sha string, manifest JsonValue) ! {
	validate_composition_control_inputs(base_root, base_sha, manifest)!
	target_id := require_string_member(manifest, 'target_id')!
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	registry_target := registry_target_by_id(registry, target_id)!
	native_workflow := require_string_member(registry_target, 'native_workflow')!
	base_paths := read_candidate_tree_paths(base_root, base_sha)!
	if candidate_manifest_path in base_paths {
		return error('legacy onboarding base must not contain an automation manifest')
	}
	mut control_paths := [native_workflow,
		require_string_member(require_object_member(manifest, 'recipe')!, 'path')!]
	for collection in ['patches', 'transforms'] {
		for entry in require_array_member(manifest, collection)! {
			control_paths << require_string_member(entry, 'path')!
		}
	}
	for optional in ['.gitignore', '.gitattributes'] {
		if optional in base_paths {
			control_paths << optional
		}
	}
	mut payload_paths := []string{}
	for entry in candidate_payload_entries(manifest)! {
		payload_paths << require_string_member(entry, 'path')!
	}
	for path in base_paths {
		if path.starts_with('automation/') {
			return error('legacy onboarding base contains an undeclared automation control')
		}
		if path.starts_with('.github/') && path != native_workflow {
			return error('legacy onboarding base contains an undeclared workflow control')
		}
		if path !in control_paths && path !in payload_paths {
			return error('legacy onboarding base path is outside the reviewed control and payload closure')
		}
	}
	for path in control_paths {
		if path !in base_paths {
			return error('legacy onboarding base is missing a reviewed control path')
		}
	}
	for entry in require_array_member(manifest, 'overlays')! {
		if require_string_member(entry, 'path')! !in base_paths {
			return error('legacy onboarding overlay must originate in the immutable base')
		}
	}
}

fn validate_legacy_onboarding_transition(automation_root string, repository_root string,
	base_sha string, candidate_sha string, manifest JsonValue) ! {
	target_id := require_string_member(manifest, 'target_id')!
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	registry_target := registry_target_by_id(registry, target_id)!
	native_workflow := require_string_member(registry_target, 'native_workflow')!
	mut controls := [native_workflow,
		require_string_member(require_object_member(manifest, 'recipe')!, 'path')!]
	for collection in ['patches', 'transforms'] {
		for entry in require_array_member(manifest, collection)! {
			controls << require_string_member(entry, 'path')!
		}
	}
	base_paths := read_candidate_tree_paths(repository_root, base_sha)!
	for optional in ['.gitignore', '.gitattributes'] {
		if optional in base_paths {
			controls << optional
		}
	}
	for path in controls {
		if authoritative_git_entry(repository_root, base_sha, path)! != authoritative_git_entry(repository_root,
			candidate_sha, path)! {
			return error('legacy onboarding candidate changed an immutable base control')
		}
	}
	mut raw_paths := []string{}
	for collection in ['inventory', 'outputs'] {
		for entry in require_array_member(manifest, collection)! {
			raw_paths << require_string_member(entry, 'path')!
		}
	}
	mut overlay_paths := []string{}
	for entry in require_array_member(manifest, 'overlays')! {
		overlay_paths << require_string_member(entry, 'path')!
	}
	diff := read_candidate_transition_status(repository_root, base_sha, candidate_sha)!
	mut records := diff.split('\x00')
	if records.len == 0 || records.last() != '' {
		return error('legacy onboarding transition status stream is not terminated')
	}
	records.delete_last()
	if records.len == 0 || records.len % 2 != 0 {
		return error('legacy onboarding transition status stream is not paired')
	}
	mut manifest_status := ''
	for index := 0; index < records.len; index += 2 {
		status := records[index]
		path := records[index + 1]
		if status !in ['A', 'M', 'T'] {
			return error('legacy onboarding transition must not delete a base path')
		}
		if path == candidate_manifest_path {
			manifest_status = status
			continue
		}
		if path in controls || path in overlay_paths {
			return error('legacy onboarding transition changed an immutable control or overlay')
		}
		if path !in raw_paths {
			return error('legacy onboarding transition changed a path outside the selected RAW payload')
		}
	}
	if manifest_status != 'A' {
		return error('legacy onboarding must add exactly one authoritative manifest')
	}
	mut expected_paths := base_paths.clone()
	for path in raw_paths {
		if path !in expected_paths {
			expected_paths << path
		}
	}
	expected_paths << candidate_manifest_path
	expected_paths.sort()
	if read_candidate_tree_paths(repository_root, candidate_sha)! != expected_paths {
		return error('legacy onboarding candidate tree differs from the exact additive closure')
	}
}

fn canonical_candidate_work_root(path string) !string {
	if path == '' || path.contains('\x00') || path.contains('\n') || path.contains('\r')
		|| path.contains('\t') {
		return error('candidate work root is invalid')
	}
	base := os.file_name(path)
	if base == '' || base in ['.', '..'] {
		return error('candidate work root basename is invalid')
	}
	parent := os.dir(path)
	if !os.is_dir(parent) || os.is_link(parent) {
		return error('candidate work root parent must be a physical directory')
	}
	canonical_parent := canonical_contract_root(parent)!
	return '${canonical_parent}/${base}'
}

fn validate_managed_baseline_runtime_contract_checkout(root string,
	runtime RuntimeContractBinding) !string {
	if runtime.repository != 'vlang/v' {
		return error('managed baseline runtime source requires the vlang/v contract authority')
	}
	inside := successful_candidate_git(root, ['rev-parse', '--is-inside-work-tree'],
		'managed baseline runtime contract checkout cannot be inspected')!
	if inside.trim_space() != 'true' {
		return error('managed baseline runtime contract root is not a Git worktree')
	}
	toplevel := successful_candidate_git(root, ['rev-parse', '--show-toplevel'],
		'managed baseline runtime contract top level cannot be resolved')!
	if canonical_contract_root(toplevel.trim_space())! != root {
		return error('managed baseline runtime contract root differs from its Git top level')
	}
	object_format := successful_candidate_git(root, ['rev-parse', '--show-object-format'],
		'managed baseline runtime contract object format cannot be resolved')!
	if object_format.trim_space() != 'sha1' {
		return error('managed baseline runtime contract must use the SHA-1 Git object format')
	}
	shallow := successful_candidate_git(root, ['rev-parse', '--is-shallow-repository'],
		'managed baseline runtime contract shallow state cannot be resolved')!
	if shallow.trim_space() != 'false' {
		return error('managed baseline runtime contract must contain complete non-shallow history')
	}
	autocrlf := successful_candidate_git(root, ['config', '--local', '--get', 'core.autocrlf'],
		'managed baseline runtime contract must set core.autocrlf=false locally')!
	if autocrlf.trim_space() != 'false' {
		return error('managed baseline runtime contract must set core.autocrlf=false locally')
	}
	validate_candidate_local_git_config(root)!
	validate_candidate_git_storage(root)!
	origin := successful_candidate_git(root, ['remote', 'get-url', 'origin'],
		'managed baseline runtime contract origin cannot be resolved')!.trim_space()
	if origin !in ['https://github.com/vlang/v', 'https://github.com/vlang/v.git'] {
		return error('managed baseline runtime contract origin is not the vlang/v HTTPS repository')
	}
	replacements := successful_candidate_git(root, ['for-each-ref', '--format=%(refname)',
		'refs/replace/'], 'managed baseline runtime contract replacement refs cannot be inspected')!
	if replacements != '' {
		return error('managed baseline runtime contract must not contain replacement refs')
	}
	resolved := successful_candidate_git(root,
		['rev-parse', '--verify', '${runtime.sha}^{commit}'],
		'managed baseline runtime contract commit cannot be resolved without lazy fetching')!
	if resolved.trim_space() != runtime.sha {
		return error('managed baseline runtime contract commit resolution is not exact')
	}
	head := successful_candidate_git(root, ['rev-parse', 'HEAD'],
		'managed baseline runtime contract HEAD cannot be resolved')!
	if head.trim_space() != runtime.sha {
		return error('managed baseline runtime contract checkout is not at the runtime SHA')
	}
	symbolic := candidate_repository_git(root, ['symbolic-ref', '-q', 'HEAD'])!
	if symbolic.exit_code != 1 || symbolic.output != '' {
		return error('managed baseline runtime contract checkout must be detached')
	}
	status := successful_candidate_git(root, ['status', '--porcelain=v1', '--untracked-files=no'],
		'managed baseline runtime contract status cannot be inspected')!
	if status != '' {
		return error('managed baseline runtime contract checkout must have no tracked changes')
	}
	tree := successful_candidate_git(root, ['rev-parse', '--verify', '${runtime.sha}^{tree}'],
		'managed baseline runtime contract tree cannot be resolved')!.trim_space()
	if !is_lower_hex_40(tree) {
		return error('managed baseline runtime contract tree is not an exact Git object ID')
	}
	return tree
}

fn candidate_git(args []string) !os.Result {
	validate_candidate_git_environment()!
	if os.setenv('GIT_NO_LAZY_FETCH', '1', true) != 0
		|| os.setenv('GIT_TERMINAL_PROMPT', '0', true) != 0
		|| os.setenv('GIT_CONFIG_NOSYSTEM', '1', true) != 0
		|| os.setenv('GIT_CONFIG_SYSTEM', os.path_devnull, true) != 0
		|| os.setenv('GIT_CONFIG_GLOBAL', os.path_devnull, true) != 0 {
		return error('Git safety environment cannot be established')
	}
	return os.exec(args)
}

fn validate_candidate_git_environment() ! {
	environment := os.environ()
	redirecting_names := [
		'GIT_DIR',
		'GIT_WORK_TREE',
		'GIT_COMMON_DIR',
		'GIT_OBJECT_DIRECTORY',
		'GIT_ALTERNATE_OBJECT_DIRECTORIES',
		'GIT_REPLACE_REF_BASE',
		'GIT_GRAFT_FILE',
		'GIT_SHALLOW_FILE',
		'GIT_NAMESPACE',
		'GIT_INDEX_FILE',
		'GIT_EXEC_PATH',
		'GIT_CONFIG',
		'GIT_CONFIG_PARAMETERS',
		'GIT_CONFIG_COUNT',
		'GIT_TEMPLATE_DIR',
	]
	safety_values := {
		'GIT_NO_LAZY_FETCH':   '1'
		'GIT_TERMINAL_PROMPT': '0'
		'GIT_CONFIG_NOSYSTEM': '1'
		'GIT_CONFIG_SYSTEM':   os.path_devnull
		'GIT_CONFIG_GLOBAL':   os.path_devnull
	}
	for name in environment.keys() {
		upper_name := name.to_upper()
		if upper_name.starts_with('GIT_CONFIG_KEY_') || upper_name.starts_with('GIT_CONFIG_VALUE_') {
			return error('candidate Git environment contains injected configuration')
		}
		if upper_name in redirecting_names && name != upper_name {
			return error('candidate Git environment contains a repository, object, or configuration redirection')
		}
		if upper_name in safety_values && name != upper_name {
			return error('candidate Git environment contains a case-variant safety override')
		}
	}
	for name in redirecting_names {
		if _ := os.getenv_opt(name) {
			return error('candidate Git environment contains a repository, object, or configuration redirection')
		}
	}
	for name, expected in safety_values {
		if value := os.getenv_opt(name) {
			if value != expected {
				return error('candidate Git environment contains an invalid safety override')
			}
		}
	}
}

fn candidate_repository_git(root string, args []string) !os.Result {
	mut command := ['git', '--no-replace-objects', '-C', root, '-c', 'core.autocrlf=false', '-c',
		'core.fsmonitor=false', '-c', 'core.hooksPath=${os.path_devnull}']
	command << args
	return candidate_git(command)!
}

fn successful_candidate_git(root string, args []string, failure string) !string {
	result := candidate_repository_git(root, args)!
	if result.exit_code != 0 {
		return error(failure)
	}
	return result.output
}

fn validate_candidate_repository(root string, base_sha string, candidate_sha string) ! {
	inside := successful_candidate_git(root, ['rev-parse', '--is-inside-work-tree'],
		'candidate repository cannot be inspected')!
	if inside.trim_space() != 'true' {
		return error('candidate repository is not a Git worktree')
	}
	toplevel := successful_candidate_git(root, ['rev-parse', '--show-toplevel'],
		'candidate repository top level cannot be resolved')!
	if canonical_contract_root(toplevel.trim_space())! != root {
		return error('candidate repository root must equal its exact Git top level')
	}
	object_format := successful_candidate_git(root, ['rev-parse', '--show-object-format'],
		'candidate object format cannot be resolved')!
	if object_format.trim_space() != 'sha1' {
		return error('candidate repository must use the SHA-1 Git object format')
	}
	shallow := successful_candidate_git(root, ['rev-parse', '--is-shallow-repository'],
		'candidate shallow state cannot be resolved')!
	if shallow.trim_space() != 'false' {
		return error('candidate repository must contain complete non-shallow history')
	}
	autocrlf := successful_candidate_git(root, ['config', '--local', '--get', 'core.autocrlf'],
		'candidate repository must set core.autocrlf=false locally')!
	if autocrlf.trim_space() != 'false' {
		return error('candidate repository must set core.autocrlf=false locally')
	}
	validate_candidate_local_git_config(root)!
	validate_candidate_git_storage(root)!
	replacements := successful_candidate_git(root, ['for-each-ref', '--format=%(refname)',
		'refs/replace/'], 'candidate replacement refs cannot be inspected')!
	if replacements != '' {
		return error('candidate repository must not contain replacement refs')
	}
	for sha in [base_sha, candidate_sha] {
		resolved := successful_candidate_git(root, ['rev-parse', '--verify', '${sha}^{commit}'],
			'candidate transition commit cannot be resolved without lazy fetching')!
		if resolved.trim_space() != sha {
			return error('candidate transition commit resolution is not exact')
		}
	}
	head := successful_candidate_git(root, ['rev-parse', 'HEAD'],
		'candidate repository HEAD cannot be resolved')!
	if head.trim_space() != candidate_sha {
		return error('candidate repository HEAD must equal the candidate SHA')
	}
	status := successful_candidate_git(root, ['status', '--porcelain=v1', '--untracked-files=all',
		'--ignored=matching'], 'candidate repository status cannot be inspected')!
	if status != '' {
		return error('candidate repository must be clean, including ignored files')
	}
	commit_source := successful_candidate_git(root, ['cat-file', 'commit', candidate_sha],
		'candidate raw commit cannot be inspected')!
	if !raw_commit_has_exact_parent(commit_source, base_sha) {
		return error('candidate commit must have the exact base as its sole parent')
	}
}

fn raw_commit_has_exact_parent(source string, base_sha string) bool {
	header := source.all_before('\n\n')
	mut tree_count := 0
	mut parents := []string{}
	for line in header.split('\n') {
		if line.starts_with('tree ') {
			tree_count++
			if !is_lower_hex_40(line.all_after('tree ')) {
				return false
			}
		} else if line.starts_with('parent ') {
			parents << line.all_after('parent ')
		}
	}
	return tree_count == 1 && parents == [base_sha]
}

fn validate_candidate_git_storage(root string) ! {
	git_dir_source := successful_candidate_git(root, ['rev-parse', '--absolute-git-dir'],
		'candidate Git directory cannot be resolved')!.trim_space()
	common_dir_source := successful_candidate_git(root, ['rev-parse', '--path-format=absolute',
		'--git-common-dir'], 'candidate Git common directory cannot be resolved')!.trim_space()
	git_dir := canonical_contract_root(git_dir_source)!
	common_dir := canonical_contract_root(common_dir_source)!
	for path in [os.join_path(git_dir, 'info', 'grafts'), os.join_path(common_dir, 'info', 'grafts'),
		os.join_path(git_dir, 'objects', 'info', 'alternates'),
		os.join_path(common_dir, 'objects', 'info', 'alternates')] {
		if os.exists(path) || os.is_link(path) {
			return error('candidate repository contains a graft or object alternate')
		}
	}
}

fn validate_candidate_local_git_config(root string) ! {
	local_config := candidate_repository_git(root, ['config', '--local', '--name-only',
		'--get-regexp', '.*'])!
	if local_config.exit_code !in [0, 1] {
		return error('candidate repository local configuration cannot be inspected')
	}
	for config_name in local_config.output.split_into_lines() {
		name := config_name.to_lower()
		if name.starts_with('filter.') || name.starts_with('include.')
			|| name.starts_with('includeif.')
			|| name in ['core.attributesfile', 'core.hookspath', 'core.fsmonitor', 'init.templatedir'] {
			return error('candidate repository local configuration contains a filter, include, hook, or template override')
		}
	}
}

fn clone_candidate_source(candidate_root string, source_root string, candidate_sha string) ! {
	template_root := '${source_root}.empty-template'
	os.mkdir(template_root, mode: 0o700)!
	defer {
		os.rmdir_all(template_root) or {}
	}
	clone := candidate_git(['git', '--no-replace-objects', '-c', 'core.autocrlf=false', '-c',
		'core.fsmonitor=false', '-c', 'core.hooksPath=${os.path_devnull}', '-c',
		'protocol.file.allow=always', 'clone', '--quiet', '--no-checkout', '--no-local',
		'--no-hardlinks', '--template=${template_root}', candidate_root, source_root])!
	if clone.exit_code != 0 {
		return error('independent candidate clone failed without lazy fetching')
	}
	for args in [
		['config', '--local', 'core.autocrlf', 'false'],
		['checkout', '--quiet', '--detach', '--force', candidate_sha],
	] {
		result := candidate_repository_git(source_root, args)!
		if result.exit_code != 0 {
			return error('independent candidate checkout could not be materialized exactly')
		}
	}
	origin := successful_candidate_git(source_root, ['remote', 'get-url', 'origin'],
		'independent candidate origin cannot be resolved')!
	if canonical_contract_root(origin.trim_space())! != candidate_root {
		return error('independent candidate origin differs from its validated local source')
	}
	validate_candidate_local_git_config(source_root)!
	validate_independent_candidate_checkout(source_root, candidate_sha)!
}

fn validate_independent_candidate_checkout(source_root string, candidate_sha string) ! {
	toplevel := successful_candidate_git(source_root, ['rev-parse', '--show-toplevel'],
		'independent candidate top level cannot be resolved')!
	if canonical_contract_root(toplevel.trim_space())! != source_root {
		return error('independent candidate root differs from its Git top level')
	}
	object_format := successful_candidate_git(source_root, ['rev-parse', '--show-object-format'],
		'independent candidate object format cannot be resolved')!
	if object_format.trim_space() != 'sha1' {
		return error('independent candidate must use the SHA-1 Git object format')
	}
	shallow := successful_candidate_git(source_root, ['rev-parse', '--is-shallow-repository'],
		'independent candidate shallow state cannot be resolved')!
	if shallow.trim_space() != 'false' {
		return error('independent candidate must contain complete non-shallow history')
	}
	autocrlf := successful_candidate_git(source_root,
		['config', '--local', '--get', 'core.autocrlf'],
		'independent candidate autocrlf policy cannot be resolved')!
	if autocrlf.trim_space() != 'false' {
		return error('independent candidate must disable autocrlf')
	}
	validate_candidate_local_git_config(source_root)!
	validate_candidate_git_storage(source_root)!
	head := successful_candidate_git(source_root, ['rev-parse', 'HEAD'],
		'independent candidate HEAD cannot be resolved')!
	if head.trim_space() != candidate_sha {
		return error('independent candidate checkout is not at the candidate SHA')
	}
	symbolic := candidate_repository_git(source_root, ['symbolic-ref', '-q', 'HEAD'])!
	if symbolic.exit_code != 1 || symbolic.output != '' {
		return error('independent candidate checkout must be detached')
	}
	status := successful_candidate_git(source_root, ['status', '--porcelain=v1',
		'--untracked-files=all', '--ignored=matching'],
		'independent candidate status cannot be inspected')!
	if status != '' {
		return error('independent candidate checkout must be clean, including ignored files')
	}
	replacements := successful_candidate_git(source_root, ['for-each-ref', '--format=%(refname)',
		'refs/replace/'], 'independent candidate replacement refs cannot be inspected')!
	if replacements != '' {
		return error('independent candidate checkout must not contain replacement refs')
	}
}

fn candidate_payload_entries(manifest JsonValue) ![]JsonValue {
	mut payload_entries := []JsonValue{}
	for collection in ['inventory', 'overlays', 'outputs'] {
		for entry in require_array_member(manifest, collection)! {
			payload_entries << entry
		}
	}
	return payload_entries
}

fn validate_candidate_transition(candidate_root string, base_sha string, candidate_sha string,
	base_manifest JsonValue, manifest JsonValue) ! {
	base_target := require_string_member(base_manifest, 'target_id')!
	target := require_string_member(manifest, 'target_id')!
	base_branch := require_string_member(base_manifest, 'branch')!
	branch := require_string_member(manifest, 'branch')!
	base_recipe := require_object_member(base_manifest, 'recipe')!
	recipe := require_object_member(manifest, 'recipe')!
	base_recipe_path := require_string_member(base_recipe, 'path')!
	recipe_path := require_string_member(recipe, 'path')!
	if base_target != target || base_branch != branch || base_recipe_path != recipe_path {
		return error('candidate target, branch, and recipe path must remain stable from its base')
	}
	mut base_payload_paths := []string{}
	for entry in candidate_payload_entries(base_manifest)! {
		base_payload_paths << require_string_member(entry, 'path')!
	}
	mut candidate_payload_paths := []string{}
	for entry in candidate_payload_entries(manifest)! {
		candidate_payload_paths << require_string_member(entry, 'path')!
	}
	if target_paths_are_case_insensitive(target) {
		mut cross_version_paths := map[string]string{}
		for path in base_payload_paths {
			cross_version_paths[path.to_lower()] = path
		}
		for path in candidate_payload_paths {
			key := path.to_lower()
			if prior := cross_version_paths[key] {
				if prior != path {
					return error('candidate transition cannot rename a payload by case only')
				}
			}
			cross_version_paths[key] = path
		}
	}
	validate_candidate_policy_projection(base_manifest, manifest)!
	validate_candidate_immutable_recipe(candidate_root, base_sha, candidate_sha, manifest)!
	validate_candidate_immutable_overlays(candidate_root, base_sha, candidate_sha, base_manifest,
		manifest)!
	validate_candidate_immutable_inputs(candidate_root, base_sha, candidate_sha, base_manifest,
		manifest, 'patches')!
	validate_candidate_immutable_inputs(candidate_root, base_sha, candidate_sha, base_manifest,
		manifest, 'transforms')!
	mut allowed_paths := [candidate_manifest_path, recipe_path]
	for path in base_payload_paths {
		if path !in allowed_paths {
			allowed_paths << path
		}
	}
	for path in candidate_payload_paths {
		if path !in allowed_paths {
			allowed_paths << path
		}
	}
	diff := read_candidate_transition_status(candidate_root, base_sha, candidate_sha)!
	mut records := diff.split('\x00')
	if records.len == 0 || records.last() != '' {
		return error('candidate transition path stream is not terminated')
	}
	records.delete_last()
	if records.len == 0 || records.len % 2 != 0 {
		return error('candidate transition status stream is not paired')
	}
	mut changed_paths := []string{cap: records.len / 2}
	mut manifest_status := ''
	for index := 0; index < records.len; index += 2 {
		status := records[index]
		path := records[index + 1]
		if status !in ['A', 'M', 'D', 'T'] {
			return error('candidate transition contains an unsupported path status')
		}
		if path == candidate_manifest_path {
			manifest_status = status
		}
		changed_paths << path
	}
	if manifest_status != 'M' {
		return error('candidate transition must update the authoritative manifest')
	}
	mut seen := []string{}
	mut seen_keys := []string{}
	mut status_by_path := map[string]string{}
	for index := 0; index < changed_paths.len; index++ {
		path := changed_paths[index]
		status := records[index * 2]
		if !contract_relative_path_is_safe(path) || path in seen {
			return error('candidate transition contains an unsafe or duplicate path')
		}
		seen << path
		status_by_path[path] = status
		path_key := manifest_path_key(target, path)
		if path_key in seen_keys {
			return error('candidate transition contains a case-folded path collision')
		}
		seen_keys << path_key
		if path !in allowed_paths {
			if path.starts_with('.github/') {
				return error('candidate transition must not modify workflow controls')
			}
			if path.starts_with('automation/') {
				return error('candidate transition may modify only the authoritative automation manifest')
			}
			return error('candidate transition contains a path outside its manifest closure')
		}
		if status in ['A', 'D'] && path in base_payload_paths && path in candidate_payload_paths {
			return error('candidate transition cannot add or delete a shared payload path')
		}
		if status == 'A' && path !in candidate_payload_paths {
			return error('candidate transition added a path not declared only by the candidate payload')
		}
		if status == 'D' && path !in base_payload_paths {
			return error('candidate transition deleted a path not declared only by the base payload')
		}
		if status in ['M', 'T'] && path !in [candidate_manifest_path, recipe_path]
			&& (path !in base_payload_paths || path !in candidate_payload_paths) {
			return error('candidate transition modified a path not shared by both payload contracts')
		}
	}
	for path in candidate_payload_paths {
		if path !in base_payload_paths && status_by_path[path] != 'A' {
			return error('candidate payload additions require an exact Git add')
		}
	}
	for path in base_payload_paths {
		if path !in candidate_payload_paths && status_by_path[path] != 'D' {
			return error('candidate payload removals require an exact Git delete')
		}
	}
}

fn read_candidate_transition_status(candidate_root string, base_sha string, candidate_sha string) !string {
	output_path := os.join_path(os.dir(candidate_root), 'candidate-transition-status')
	if os.exists(output_path) || os.is_link(output_path) {
		return error('candidate transition status destination must not already exist')
	}
	defer {
		os.rm(output_path) or {}
	}
	result := candidate_repository_git(candidate_root, ['diff-tree', '--no-commit-id',
		'--name-status', '-r', '-z', '--no-renames', '--output=${output_path}', base_sha,
		candidate_sha, '--'])!
	if result.exit_code != 0 || result.output != '' {
		return error('candidate transition cannot be inspected')
	}
	if os.is_link(output_path) {
		return error('candidate transition status is not a physical regular file')
	}
	before := os.lstat(output_path) or {
		return error('candidate transition status was not materialized')
	}
	if before.get_filetype() != .regular || before.size > candidate_transition_max_status_bytes {
		return error('candidate transition status is not a bounded regular file')
	}
	bytes := os.read_bytes(output_path)!
	after := os.lstat(output_path)!
	if u64(bytes.len) != before.size || !same_file_snapshot(before, after) {
		return error('candidate transition status changed while it was read')
	}
	return bytes.bytestr()
}

fn validate_candidate_policy_projection(base_manifest JsonValue, manifest JsonValue) ! {
	for key in ['schema_version', 'contract_version', 'contract_repository', 'contract_sha',
		'contract_mode', 'target_id', 'branch', 'recipe', 'header_effects', 'integrations', 'probes',
		'affected_targets', 'patches', 'transforms'] {
		if !json_equal(require_member(base_manifest, key)!, require_member(manifest, key)!) {
			return error('candidate immutable policy projection differs from its base')
		}
	}
	base_toolchain := manifest_toolchain_profile_projection(base_manifest)!
	candidate_toolchain := manifest_toolchain_profile_projection(manifest)!
	_, _, base_producer := manifest_toolchain_members(base_manifest)!
	_, _, candidate_producer := manifest_toolchain_members(manifest)!
	if require_nullable_string_member(base_toolchain, 'profile_id')! == ''
		|| require_nullable_string_member(base_toolchain, 'profile_sha256')! == ''
		|| require_nullable_string_member(candidate_toolchain, 'profile_id')! == ''
		|| require_nullable_string_member(candidate_toolchain, 'profile_sha256')! == ''
		|| base_producer.kind != .object || candidate_producer.kind != .object {
		return error('monthly candidate requires authenticated producer observations in both manifests')
	}
	if !json_equal(base_toolchain, candidate_toolchain) {
		return error('candidate producer toolchain profile differs from its base')
	}
	if !json_equal(manifest_static_payload_policy(base_manifest)!,
		manifest_static_payload_policy(manifest)!) {
		return error('candidate immutable payload policy differs from its base')
	}
}

fn validate_candidate_immutable_recipe(repository_root string, base_sha string,
	candidate_sha string, manifest JsonValue) ! {
	recipe := require_object_member(manifest, 'recipe')!
	path := require_string_member(recipe, 'path')!
	if authoritative_git_entry(repository_root, base_sha, path)! != authoritative_git_entry(repository_root,
		candidate_sha, path)! {
		return error('candidate immutable recipe blob differs from its base')
	}
}

fn validate_candidate_immutable_overlays(repository_root string, base_sha string,
	candidate_sha string, base_manifest JsonValue, manifest JsonValue) ! {
	base_overlays := require_array_member(base_manifest, 'overlays')!
	overlays := require_array_member(manifest, 'overlays')!
	if base_overlays.len != overlays.len {
		return error('candidate overlay bytes must remain exact from its base')
	}
	for index, overlay in overlays {
		if require_string_member(base_overlays[index], 'sha256')! != require_string_member(overlay,
			'sha256')! {
			return error('candidate overlay bytes must remain exact from its base')
		}
		path := require_string_member(overlay, 'path')!
		if authoritative_git_entry(repository_root, base_sha, path)! != authoritative_git_entry(repository_root,
			candidate_sha, path)! {
			return error('candidate overlay bytes must remain exact from its base')
		}
	}
}

fn validate_candidate_immutable_inputs(repository_root string, base_sha string, candidate_sha string,
	base_manifest JsonValue, manifest JsonValue, collection string) ! {
	base_inputs := require_array_member(base_manifest, collection)!
	inputs := require_array_member(manifest, collection)!
	if !json_equal(JsonValue{
		kind:        .array
		array_value: base_inputs
	}, JsonValue{
		kind:        .array
		array_value: inputs
	}) {
		return error('candidate patches and transforms cannot change in the generic preflight')
	}
	for input in inputs {
		path := require_string_member(input, 'path')!
		base_entry := authoritative_git_entry(repository_root, base_sha, path)!
		candidate_entry := authoritative_git_entry(repository_root, candidate_sha, path)!
		if base_entry != candidate_entry {
			return error('candidate patch or transform blob differs from its base')
		}
	}
}

fn export_candidate_payload(source_root string, payload_root string, manifest JsonValue) ! {
	entries := candidate_payload_entries(manifest)!
	mut paths := []string{cap: entries.len}
	for entry in entries {
		paths << require_string_member(entry, 'path')!
	}
	paths.sort()
	os.mkdir(payload_root, mode: 0o700)!
	for relative_directory in payload_parent_directories(paths) {
		os.mkdir(os.join_path(payload_root, relative_directory), mode: 0o700)!
	}
	for entry in entries {
		export_candidate_payload_entry(source_root, payload_root, entry)!
	}
}

fn export_candidate_payload_entry(source_root string, payload_root string, entry JsonValue) ! {
	path := require_string_member(entry, 'path')!
	kind := require_string_member(entry, 'kind')!
	mode := require_string_member(entry, 'git_mode')!
	validate_staged_parent_chain(source_root, path)!
	git_entry := authoritative_git_entry(source_root, successful_candidate_git(source_root, [
		'rev-parse',
		'HEAD',
	], 'independent candidate HEAD cannot be resolved')!.trim_space(), path)!
	if git_entry.git_mode != mode {
		return error('candidate payload Git mode differs from the manifest')
	}
	observed := observe_candidate_path(source_root, path)!
	if !observed.identity_reliable || observed.nlink != 1 {
		return error('candidate payload source must be a private single-link checkout file')
	}
	source_path := os.join_path(source_root, path)
	destination_path := os.join_path(payload_root, path)
	if kind == 'symlink' {
		if observed.kind != 'symlink' || mode != '120000' {
			return error('candidate symlink payload differs from the manifest')
		}
		$if windows {
			return error('Windows payload reparse points are not supported')
		} $else {
			os.symlink(observed.symlink_target, destination_path)!
		}
		return
	}
	if observed.kind != 'file' || kind !in ['file', 'executable'] || mode !in ['100644', '100755']
		|| os.is_link(source_path) {
		return error('candidate regular payload differs from the manifest')
	}
	copy_candidate_regular_file(source_path, destination_path)!
	$if !windows {
		os.chmod(destination_path, if mode == '100755' { 0o755 } else { 0o644 })!
	}
}

fn copy_candidate_regular_file(source_path string, destination_path string) ! {
	if os.exists(destination_path) || os.is_link(destination_path) {
		return error('candidate payload destination must not already exist')
	}
	mut destination := os.create(destination_path)!
	defer {
		destination.close()
	}
	copy_candidate_regular_file_to_open_destination(source_path, mut destination)!
}

fn copy_candidate_regular_file_to_open_destination(source_path string, mut destination os.File) ! {
	before := os.lstat(source_path)!
	if before.get_filetype() != .regular || before.size > staged_payload_max_file_size {
		return error('candidate payload source is not a bounded regular file')
	}
	mut source := os.open(source_path)!
	defer {
		source.close()
	}
	mut buffer := []u8{len: staged_payload_hash_buffer_size}
	mut total := u64(0)
	for {
		read := source.read(mut buffer) or {
			if err is os.Eof {
				break
			}
			return error('candidate payload source cannot be read')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > staged_payload_max_file_size {
			return error('candidate payload source exceeded its byte bound')
		}
		mut offset := 0
		for offset < read {
			written := destination.write(buffer[offset..read])!
			if written <= 0 {
				return error('candidate payload export made no write progress')
			}
			offset += written
		}
	}
	after := os.lstat(source_path)!
	if total != before.size || !same_file_snapshot(before, after) {
		return error('candidate payload source changed during export')
	}
}
