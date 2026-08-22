module bin

import io.util
import os

pub struct CandidateCompositionRequest {
pub:
	target_id      string
	kind           CandidateTransitionKind
	base_repo_root string
	base_sha       string
	raw_root       string
	manifest_path  string
	result_root    string
}

pub struct CandidateCompositionResult {
pub:
	target_id     string
	kind          CandidateTransitionKind
	base_sha      string
	candidate_sha string
	tree          string
	decision      StagedManifestEligibility
}

// compose_candidate_for_execution creates one private direct-child candidate, proves it through
// the real publication-disabled preflight, then exposes only the clean candidate repository by a
// same-filesystem atomic rename. Callers must not treat stdout or a sidecar as authority.
pub fn compose_candidate_for_execution(automation_root string, request CandidateCompositionRequest,
	runtime RuntimeContractBinding) !CandidateCompositionResult {
	attest_runtime_contract_binding(runtime)!
	if request.target_id !in managed_target_ids {
		return error('candidate composition target is not managed')
	}
	mut onboarding_policy := JsonValue{}
	mut activation_binding := ManagedBaselineActivationBinding{}
	mut activation_policy := JsonValue{}
	if request.kind == .legacy_onboard {
		_, onboarding_policy = reviewed_legacy_onboarding_binding(automation_root,
			request.target_id, request.base_sha)!
	} else if request.kind == .baseline_activate {
		activation_binding, activation_policy = reviewed_managed_baseline_activation_binding(automation_root,
			request.target_id, request.base_sha)!
	}
	if !is_lower_hex_40(request.base_sha) {
		return error('candidate composition base must be a full lowercase commit SHA')
	}
	base_root, raw_root, manifest_source_path, result_root := validate_candidate_composition_roots(automation_root,
		request)!
	contract_root := canonical_contract_root(os.join_path(automation_root, '..', '..'))!
	validate_composition_base_repository(base_root, request.base_sha)!

	result_parent := os.dir(result_root)
	temporary_root := util.temp_dir(path: result_parent, pattern: '.tccbin-compose-*.tmp')!
	$if !windows {
		os.chmod(temporary_root, 0o700)!
	}
	mut exposed := false
	defer {
		if !exposed {
			os.rmdir_all(temporary_root) or {}
		}
	}
	candidate_root := os.join_path(temporary_root, 'candidate-repository')
	preflight_root := os.join_path(temporary_root, 'preflight')
	clone_candidate_source(base_root, candidate_root, request.base_sha)!

	manifest_destination := os.join_path(candidate_root, candidate_manifest_path)
	mut base_manifest := JsonValue{}
	if request.kind == .monthly {
		attest_candidate_manifest_present(candidate_root, request.base_sha)!
		base_issues := validate_manifest(automation_root, manifest_destination)!
		if base_issues.len > 0 {
			return error('monthly composition base manifest failed with ${base_issues.len} issue(s)')
		}
		base_manifest = parse_strict_json(os.read_file(manifest_destination)!)!
	} else if request.kind == .legacy_onboard {
		attest_legacy_candidate_manifest_absent(candidate_root, request.base_sha)!
	} else {
		base_manifest = attest_managed_baseline_activation_base(automation_root, request.target_id,
			candidate_root, request.base_sha, manifest_destination, activation_binding)!
	}
	materialize_candidate_manifest(candidate_root, request.base_sha, request.kind,
		manifest_source_path)!
	manifest_source := os.read_file(manifest_destination)!
	manifest := parse_strict_json(manifest_source)!
	issues := validate_manifest(automation_root, manifest_destination)!
	if issues.len > 0 {
		return error('candidate composition manifest failed with ${issues.len} issue(s)')
	}
	if require_string_member(manifest, 'target_id')! != request.target_id {
		return error('candidate composition manifest target differs from the request')
	}
	if request.kind == .legacy_onboard {
		validate_manifest_legacy_onboarding_policy(manifest, onboarding_policy)!
		validate_legacy_onboarding_base_controls(automation_root, candidate_root, request.base_sha,
			manifest)!
	} else if request.kind == .monthly {
		validate_candidate_policy_projection(base_manifest, manifest)!
		validate_composition_control_inputs(candidate_root, request.base_sha, manifest)!
	} else {
		validate_managed_baseline_activation_candidate(base_manifest, manifest, runtime,
			activation_binding, activation_policy, contract_root)!
	}

	compose_declared_candidate_payload(candidate_root, request.base_sha, raw_root, manifest)!
	index_composed_manifest(candidate_root, manifest_destination)!
	desired_tree := write_composed_candidate_tree(candidate_root, request.base_sha, manifest,
		request.kind)!
	manifest_hash := sha256_file(manifest_destination)!
	candidate_sha := commit_composed_candidate_tree(candidate_root, desired_tree, request.base_sha,
		request.target_id, manifest_hash)!
	move_composed_candidate_head(candidate_root, request.base_sha, candidate_sha)!
	validate_candidate_repository(candidate_root, request.base_sha, candidate_sha)!
	observed_tree := successful_candidate_git(candidate_root, ['rev-parse',
		'${candidate_sha}^{tree}'], 'composed candidate tree cannot be resolved')!.trim_space()
	if observed_tree != desired_tree {
		return error('composed candidate commit tree differs from the exact desired tree')
	}

	decision := evaluate_candidate_manifest_for_execution(automation_root, request.target_id,
		request.kind, candidate_root, request.base_sha, candidate_sha, preflight_root, runtime,
		false)!
	if decision.publish_allowed {
		return error('candidate composition preflight must never authorize publication')
	}
	if !decision.eligible && !(request.kind == .legacy_onboard
		&& decision.reason == 'staged_provenance_incomplete') {
		return error('candidate composition preflight did not authenticate the candidate')
	}
	os.rmdir_all(preflight_root)!
	validate_candidate_repository(candidate_root, request.base_sha, candidate_sha)!
	if successful_candidate_git(candidate_root, ['rev-parse', '${candidate_sha}^{tree}'],
		'composed candidate tree cannot be revalidated')!.trim_space() != desired_tree {
		return error('composed candidate tree changed after preflight')
	}
	mut remaining := os.ls(temporary_root)!
	remaining.sort()
	if remaining != ['candidate-repository'] {
		return error('candidate composition scratch material was not fully removed')
	}
	os.rename(temporary_root, result_root)!
	exposed = true
	return CandidateCompositionResult{
		target_id:     request.target_id
		kind:          request.kind
		base_sha:      request.base_sha
		candidate_sha: candidate_sha
		tree:          desired_tree
		decision:      decision
	}
}

fn validate_candidate_composition_roots(automation_root string,
	request CandidateCompositionRequest) !(string, string, string, string) {
	if !os.is_dir(request.base_repo_root) || os.is_link(request.base_repo_root) {
		return error('candidate composition base repository must be a physical directory')
	}
	if !os.is_dir(request.raw_root) || os.is_link(request.raw_root) {
		return error('candidate composition RAW root must be a physical directory')
	}
	if !os.is_file(request.manifest_path) || os.is_link(request.manifest_path) {
		return error('candidate composition manifest must be a physical regular file')
	}
	base_root := canonical_contract_root(request.base_repo_root)!
	raw_root := canonical_contract_root(request.raw_root)!
	manifest_path := canonical_contract_root(request.manifest_path)!
	result_root := canonical_candidate_work_root(request.result_root)!
	if os.exists(result_root) || os.is_link(result_root) {
		return error('candidate composition result root must not already exist')
	}
	contract_root := canonical_contract_root(os.join_path(automation_root, '..', '..'))!
	for pair in [[base_root, raw_root], [base_root, result_root],
		[raw_root, result_root], [contract_root, base_root], [contract_root, raw_root],
		[contract_root, result_root]] {
		if roots_overlap(pair[0], pair[1]) {
			return error('candidate composition roots must be physically separate')
		}
	}
	for root in [base_root, raw_root, result_root, contract_root] {
		if roots_overlap(manifest_path, root) {
			return error('candidate composition manifest must be separate from repository and staging roots')
		}
	}
	return base_root, raw_root, manifest_path, result_root
}

fn validate_composition_base_repository(root string, base_sha string) ! {
	inside := successful_candidate_git(root, ['rev-parse', '--is-inside-work-tree'],
		'composition base repository cannot be inspected')!
	if inside.trim_space() != 'true' {
		return error('composition base repository is not a Git worktree')
	}
	toplevel := successful_candidate_git(root, ['rev-parse', '--show-toplevel'],
		'composition base top level cannot be resolved')!
	if canonical_contract_root(toplevel.trim_space())! != root {
		return error('composition base root must equal its exact Git top level')
	}
	if successful_candidate_git(root, ['rev-parse', '--show-object-format'],
		'composition base object format cannot be resolved')!.trim_space() != 'sha1' {
		return error('composition base repository must use the SHA-1 Git object format')
	}
	if successful_candidate_git(root, ['rev-parse', '--is-shallow-repository'],
		'composition base shallow state cannot be resolved')!.trim_space() != 'false' {
		return error('composition base repository must contain complete non-shallow history')
	}
	validate_candidate_local_git_config(root)!
	validate_candidate_git_storage(root)!
	if successful_candidate_git(root, ['rev-parse', '--verify', '${base_sha}^{commit}'],
		'composition base commit cannot be resolved without lazy fetching')!.trim_space() != base_sha {
		return error('composition base commit resolution is not exact')
	}
	if successful_candidate_git(root, ['rev-parse', 'HEAD'],
		'composition base HEAD cannot be resolved')!.trim_space() != base_sha {
		return error('composition base repository HEAD must equal the exact base SHA')
	}
	status := successful_candidate_git(root, ['status', '--porcelain=v1', '--untracked-files=all',
		'--ignored=matching'], 'composition base repository status cannot be inspected')!
	if status != '' {
		return error('composition base repository must be clean, including ignored files')
	}
	if successful_candidate_git(root, ['for-each-ref', '--format=%(refname)', 'refs/replace/'],
		'composition base replacement refs cannot be inspected')! != '' {
		return error('composition base repository must not contain replacement refs')
	}
}

fn materialize_candidate_manifest(candidate_root string, base_sha string,
	kind CandidateTransitionKind, source_path string) ! {
	if kind != .legacy_onboard {
		attest_candidate_manifest_present(candidate_root, base_sha)!
	} else {
		attest_legacy_candidate_manifest_absent(candidate_root, base_sha)!
	}
	ensure_candidate_parent_directories(candidate_root, candidate_manifest_path)!
	destination_path := os.join_path(candidate_root, candidate_manifest_path)
	mut temporary_file, temporary_file_path_source := util.temp_file(
		path:    os.dir(destination_path)
		pattern: '.tccbin-manifest-*.tmp'
	)!
	mut temporary_file_path := temporary_file_path_source
	mut temporary_file_open := true
	defer {
		if temporary_file_open {
			temporary_file.close()
		}
		if temporary_file_path != '' {
			os.rm(temporary_file_path) or {}
		}
	}
	copy_candidate_regular_file_to_open_destination(source_path, mut temporary_file)!
	temporary_file.close()
	temporary_file_open = false
	$if !windows {
		os.chmod(temporary_file_path, 0o644)!
	}
	temporary_relative_path := 'automation/${os.file_name(temporary_file_path)}'
	temporary_observation := observe_candidate_path(candidate_root, temporary_relative_path) or {
		return error('candidate manifest temporary materialization is not a private regular file')
	}
	if temporary_observation.kind != 'file' || !temporary_observation.identity_reliable
		|| temporary_observation.nlink != 1 {
		return error('candidate manifest temporary materialization is not a private regular file')
	}
	if kind != .legacy_onboard {
		attest_candidate_manifest_present(candidate_root, base_sha)!
		os.rm(destination_path)!
	} else {
		attest_legacy_candidate_manifest_absent(candidate_root, base_sha)!
	}
	os.rename(temporary_file_path, destination_path)!
	temporary_file_path = ''
	materialized := observe_candidate_path(candidate_root, candidate_manifest_path) or {
		return error('candidate manifest replacement is not a private regular file')
	}
	if materialized.kind != 'file' || !materialized.identity_reliable || materialized.nlink != 1
		|| materialized.sha256 != temporary_observation.sha256
		|| materialized.git_blob_oid != temporary_observation.git_blob_oid {
		return error('candidate manifest replacement changed during atomic materialization')
	}
}

fn validate_composition_control_inputs(candidate_root string, base_sha string,
	manifest JsonValue) ! {
	recipe := require_object_member(manifest, 'recipe')!
	mut issues := validate_git_input_hash(StagingContract{
		source_git_root: candidate_root
		source_git_ref:  base_sha
	}, require_string_member(recipe, 'path')!, require_string_member(recipe, 'sha256')!,
		'$/recipe', true)!
	for patch in require_array_member(manifest, 'patches')! {
		issues << validate_git_input_hash(StagingContract{
			source_git_root: candidate_root
			source_git_ref:  base_sha
		}, require_string_member(patch, 'path')!, require_string_member(patch, 'sha256')!,
			'$/patches', false)!
	}
	for transform in require_array_member(manifest, 'transforms')! {
		issues << validate_git_input_hash(StagingContract{
			source_git_root: candidate_root
			source_git_ref:  base_sha
		}, require_string_member(transform, 'path')!, require_string_member(transform, 'sha256')!,
			'$/transforms', false)!
	}
	if issues.len > 0 {
		return error('candidate composition controls differ from the immutable base')
	}
}

fn compose_declared_candidate_payload(candidate_root string, base_sha string, raw_root string,
	manifest JsonValue) ! {
	for entry in require_array_member(manifest, 'overlays')! {
		validate_composed_overlay(candidate_root, base_sha, entry)!
	}
	for collection in ['inventory', 'outputs'] {
		for entry in require_array_member(manifest, collection)! {
			copy_raw_candidate_entry(raw_root, candidate_root, entry)!
			index_composed_candidate_entry(candidate_root, entry)!
		}
	}
}

fn validate_composed_overlay(candidate_root string, base_sha string, entry JsonValue) ! {
	path := require_string_member(entry, 'path')!
	git_entry := authoritative_git_entry(candidate_root, base_sha, path) or {
		return error('candidate overlay is absent from the immutable base')
	}
	observed := observe_candidate_path(candidate_root, path)!
	if git_entry.git_mode != require_string_member(entry, 'git_mode')!
		|| observed.git_blob_oid != git_entry.oid
		|| observed.sha256 != require_string_member(entry, 'sha256')!
		|| observed.symlink_target != require_nullable_string_member(entry, 'symlink_target')! {
		return error('candidate overlay differs from its immutable base declaration')
	}
}

fn copy_raw_candidate_entry(raw_root string, candidate_root string, entry JsonValue) ! {
	path := require_string_member(entry, 'path')!
	kind := require_string_member(entry, 'kind')!
	mode := require_string_member(entry, 'git_mode')!
	declared_sha := require_string_member(entry, 'sha256')!
	declared_target := require_nullable_string_member(entry, 'symlink_target')!
	observed := observe_staged_path(raw_root, path)!
	if !observed.identity_reliable || observed.nlink != 1 || observed.sha256 != declared_sha
		|| observed.symlink_target != declared_target {
		return error('candidate RAW entry differs from its manifest declaration')
	}
	$if !windows {
		if observed.kind != kind || observed.git_mode != mode {
			return error('candidate RAW entry kind or mode differs from its manifest declaration')
		}
	} $else {
		if observed.kind != 'file' || kind !in ['file', 'executable']
			|| mode !in ['100644', '100755'] {
			return error('candidate RAW entry kind or mode differs from its manifest declaration')
		}
	}
	ensure_candidate_parent_directories(candidate_root, path)!
	destination := os.join_path(candidate_root, path)
	if os.exists(destination) || os.is_link(destination) {
		if os.is_dir(destination) && !os.is_link(destination) {
			return error('candidate payload destination collides with a directory')
		}
		os.rm(destination)!
	}
	if kind == 'symlink' {
		$if windows {
			return error('Windows payload reparse points are not supported')
		} $else {
			os.symlink(declared_target, destination)!
		}
	} else {
		copy_candidate_regular_file(os.join_path(raw_root, path), destination)!
		$if !windows {
			os.chmod(destination, if mode == '100755' { 0o755 } else { 0o644 })!
		}
	}
	materialized := observe_candidate_path(candidate_root, path)!
	if materialized.sha256 != declared_sha || materialized.git_blob_oid != observed.git_blob_oid
		|| materialized.symlink_target != declared_target {
		return error('candidate RAW entry changed while it was materialized')
	}
}

fn ensure_candidate_parent_directories(root string, path string) ! {
	parts := path.split('/')
	mut current := root
	for part in parts[..parts.len - 1] {
		current = os.join_path(current, part)
		if os.exists(current) || os.is_link(current) {
			if os.is_link(current) || !os.is_dir(current) {
				return error('candidate payload parent is not a physical directory')
			}
			continue
		}
		os.mkdir(current, mode: 0o700)!
	}
}

fn index_composed_candidate_entry(candidate_root string, entry JsonValue) ! {
	path := require_string_member(entry, 'path')!
	mode := require_string_member(entry, 'git_mode')!
	observed := observe_candidate_path(candidate_root, path)!
	mut materialized_path := os.join_path(candidate_root, path)
	mut temporary_blob_path := ''
	if observed.kind == 'symlink' {
		mut temporary_blob, path_to_blob := util.temp_file(
			path:    os.dir(candidate_root)
			pattern: '.tccbin-symlink-*.blob'
		)!
		temporary_blob.close()
		temporary_blob_path = path_to_blob
		os.write_file(temporary_blob_path, observed.symlink_target)!
		materialized_path = temporary_blob_path
	}
	defer {
		if temporary_blob_path != '' {
			os.rm(temporary_blob_path) or {}
		}
	}
	hash := successful_candidate_git(candidate_root, ['hash-object', '-w', '--no-filters', '--',
		materialized_path], 'candidate payload blob could not be stored')!.trim_space()
	if hash != observed.git_blob_oid {
		return error('candidate payload Git blob differs after object materialization')
	}
	index_composed_blob(candidate_root, path, mode, hash)!
}

fn index_composed_manifest(candidate_root string, manifest_path string) ! {
	observed := observe_candidate_path(candidate_root, candidate_manifest_path)!
	hash := successful_candidate_git(candidate_root, ['hash-object', '-w', '--no-filters', '--',
		manifest_path], 'candidate manifest blob could not be stored')!.trim_space()
	if hash != observed.git_blob_oid {
		return error('candidate manifest Git blob differs after object materialization')
	}
	index_composed_blob(candidate_root, candidate_manifest_path, '100644', hash)!
}

fn index_composed_blob(candidate_root string, path string, mode string, oid string) ! {
	result := candidate_repository_git(candidate_root, ['update-index', '--add', '--cacheinfo',
		'${mode},${oid},${path}'])!
	if result.exit_code != 0 || result.output != '' {
		return error('candidate index could not bind an exact composed blob')
	}
}

fn write_composed_candidate_tree(candidate_root string, base_sha string, manifest JsonValue,
	kind CandidateTransitionKind) !string {
	tree := successful_candidate_git(candidate_root, ['write-tree'],
		'candidate exact tree could not be written')!.trim_space()
	if !is_lower_hex_40(tree) {
		return error('candidate exact tree is not a full lowercase Git object ID')
	}
	validate_composed_tree_closure(candidate_root, base_sha, tree, manifest, kind)!
	return tree
}

fn validate_composed_tree_closure(candidate_root string, base_sha string, tree string,
	manifest JsonValue, kind CandidateTransitionKind) ! {
	base_paths := read_candidate_tree_paths(candidate_root, base_sha)!
	candidate_paths := read_candidate_tree_paths(candidate_root, tree)!
	mut expected := []string{}
	mut payload := []string{}
	for entry in candidate_payload_entries(manifest)! {
		payload << require_string_member(entry, 'path')!
	}
	if kind == .legacy_onboard {
		expected << base_paths
		for path in payload {
			if path !in expected {
				expected << path
			}
		}
		if candidate_manifest_path !in expected {
			expected << candidate_manifest_path
		}
	} else {
		expected << base_paths
	}
	expected.sort()
	if expected != candidate_paths {
		return error('candidate composed tree differs from its exact declared closure')
	}
}

fn read_candidate_tree_paths(root string, reference string) ![]string {
	result := candidate_repository_git(root, ['ls-tree', '-r', '-z', '--name-only', '--full-tree',
		reference, '--'])!
	if result.exit_code != 0 || u64(result.output.len) > candidate_transition_max_status_bytes {
		return error('candidate tree path set cannot be inspected inside its bound')
	}
	mut records := result.output.split('\x00')
	if records.len == 0 || records.last() != '' {
		return error('candidate tree path stream is not terminated')
	}
	records.delete_last()
	mut seen := []string{}
	for path in records {
		if !contract_relative_path_is_safe(path) || path in seen {
			return error('candidate tree contains an unsafe or duplicate path')
		}
		seen << path
	}
	seen.sort()
	return seen
}

fn commit_composed_candidate_tree(candidate_root string, tree string, base_sha string,
	target_id string, manifest_hash string) !string {
	validate_candidate_commit_environment()!
	epoch_source := successful_candidate_git(candidate_root,
		['show', '-s', '--format=%ct', base_sha],
		'candidate base committer epoch cannot be resolved')!.trim_space()
	if epoch_source == '' || !epoch_source.bytes().all(it >= `0` && it <= `9`) {
		return error('candidate base committer epoch is invalid')
	}
	epoch := epoch_source.i64() + 1
	values := {
		'GIT_AUTHOR_NAME':     'vlang-bot'
		'GIT_AUTHOR_EMAIL':    'alexander+bot@vlang.io'
		'GIT_AUTHOR_DATE':     '@${epoch} +0000'
		'GIT_COMMITTER_NAME':  'vlang-bot'
		'GIT_COMMITTER_EMAIL': 'alexander+bot@vlang.io'
		'GIT_COMMITTER_DATE':  '@${epoch} +0000'
	}
	mut established := []string{}
	defer {
		for name in established {
			os.unsetenv(name)
		}
	}
	for name in ['GIT_AUTHOR_NAME', 'GIT_AUTHOR_EMAIL', 'GIT_AUTHOR_DATE', 'GIT_COMMITTER_NAME',
		'GIT_COMMITTER_EMAIL', 'GIT_COMMITTER_DATE'] {
		value := values[name]
		if os.setenv(name, value, true) != 0 {
			return error('candidate deterministic commit environment cannot be established')
		}
		established << name
	}
	message := 'tccbin: compose ${target_id} ${manifest_hash}'
	result := candidate_repository_git(candidate_root, ['commit-tree', tree, '-p', base_sha, '-m',
		message])!
	if result.exit_code != 0 || !is_lower_hex_40(result.output.trim_space()) {
		return error('candidate deterministic direct-child commit could not be created')
	}
	return result.output.trim_space()
}

fn validate_candidate_commit_environment() ! {
	closed := ['GIT_AUTHOR_NAME', 'GIT_AUTHOR_EMAIL', 'GIT_AUTHOR_DATE', 'GIT_COMMITTER_NAME',
		'GIT_COMMITTER_EMAIL', 'GIT_COMMITTER_DATE']
	for name in os.environ().keys() {
		if name.to_upper() in closed {
			return error('candidate composition caller cannot inject commit identity or time')
		}
	}
}

fn move_composed_candidate_head(candidate_root string, base_sha string, candidate_sha string) ! {
	result := candidate_repository_git(candidate_root, ['update-ref', '--no-deref', 'HEAD',
		candidate_sha, base_sha])!
	if result.exit_code != 0 || result.output != '' {
		return error('candidate detached HEAD could not be moved by exact CAS')
	}
}
