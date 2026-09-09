module bin

import crypto.sha1
import crypto.sha256
import encoding.base64
import os

const managed_target_ids = [
	'freebsd-amd64',
	'linux-amd64',
	'macos-amd64',
	'macos-arm64',
	'openbsd-amd64',
	'windows-amd64',
]

const known_bundle_branches = [
	'thirdparty-freebsd-aarch64',
	'thirdparty-freebsd-amd64',
	'thirdparty-freebsd-arm64',
	'thirdparty-linux-aarch64',
	'thirdparty-linux-amd64',
	'thirdparty-linux-arm',
	'thirdparty-linux-arm64',
	'thirdparty-linux-riscv64',
	'thirdparty-linuxmusl-aarch64',
	'thirdparty-linuxmusl-amd64',
	'thirdparty-linuxmusl-arm64',
	'thirdparty-macos-amd64',
	'thirdparty-macos-arm64',
	'thirdparty-openbsd-amd64',
	'thirdparty-windows-amd64',
	'thirdparty-windows-i386',
]

const normalized_gate_ids = [
	'manifest-contract',
	'source-provenance',
	'native-build',
	'payload-inventory',
	'patch-probes',
	'tccbin-conformance',
	'v-no-fallback-smoke',
	'artifact-digests',
	'publish-preflight-dry-run',
]

const staged_provenance_incomplete_error = 'staged provenance is incomplete'

const managed_baseline_contract_sha = '7545e515b434cd399333d43659238427d72e22e7'

const toolchain_identity_document_max_bytes = u64(512 * 1024)

const toolchain_identity_document_buffer_bytes = 16 * 1024

const managed_baseline_source_commit_max_bytes = 64 * 1024

const managed_baseline_source_commit_base64_max_bytes = 87_384

// FingerprintSet keeps the three non-interchangeable hashes of a bundle contract.
pub struct FingerprintSet {
pub:
	manifest_hash        string
	input_fingerprint    string
	artifact_fingerprint string
	digest_lines         []string
}

// ManifestProjections exposes the two schema-validated semantic projections.
pub struct ManifestProjections {
pub:
	input        JsonValue
	artifact     JsonValue
	digest_lines []string
}

// OpaqueObservation is the statically observed tuple for one opaque input.
pub struct OpaqueObservation {
pub:
	present     bool
	target_id   string
	path        string
	kind        string
	git_mode    string
	sha256      string
	format      string
	object_type string
	machine     string
	os_abi      string
}

// NativeLaneRunKey is the non-circular Actions selection declared by a native matrix. T2b1
// authenticates this declaration as matrix content but does not authorize or select a gate run.
pub struct NativeLaneRunKey {
pub:
	run_id         i64
	run_attempt    int
	check_suite_id i64
}

struct NativeLaneResult {
	probe_id        string
	lane_id         string
	required        bool
	status          string
	expected_count  int
	evidence_sha256 string
	fallback_used   bool
	object_linked   bool
	consumer_group  string
}

enum NativeLaneOutcome {
	green
	functional
	infrastructure
}

struct NativeLaneMatrixFacts {
	matrix_digest string
	subject_hash  string
	selected_run  NativeLaneRunKey
	outcome       NativeLaneOutcome
}

// StagingContract identifies the filesystem and immutable Git tree inspected for provenance.
pub struct StagingContract {
pub:
	staging_root    string
	source_git_root string
	source_git_ref  string
}

struct LegacyOnboardingBinding {
	base_sha      string
	policy_path   string
	policy_sha256 string
}

struct ManagedBaselineActivationBinding {
	base_sha                 string
	base_tree                string
	parent_sha               string
	base_manifest_sha256     string
	base_contract_repository string
	base_contract_sha        string
	policy_path              string
	policy_sha256            string
}

struct ManagedBaselineSourceCommitEvidence {
	id                string
	repository        string
	ref               string
	authority         string
	sha               string
	tree              string
	raw_commit_base64 string
}

struct ToolchainProfileBinding {
	profile_id     string
	profile_path   string
	profile_sha256 string
}

struct ToolchainFactRequirement {
	name       string
	match_kind string
}

// AuthenticatedToolchainObservation is a canonical producer or validator observation bound to
// one reviewed target profile. Volatile workflow identity is deliberately absent.
pub struct AuthenticatedToolchainObservation {
pub:
	target_id          string
	profile_id         string
	profile_sha256     string
	phase              string
	observation_sha256 string
	observation        JsonValue
}

// ProducerToolchainModel is the durable, derived identity carried by resolved inputs. It contains
// no caller-selected digest: both hashes come from the authenticated manifest observation.
pub struct ProducerToolchainModel {
pub:
	profile_id         string
	profile_sha256     string
	observation_sha256 string
	observation_digest string
}

// AuthenticatedManifestModel can only be constructed by the schema+semantic contract loaders in
// this module. Its private fields prevent callers from presenting an arbitrary reduced JsonValue
// directly to the native-matrix verdict bridge.
pub struct AuthenticatedManifestModel {
	raw_source                string
	manifest                  JsonValue
	registry                  JsonValue
	fingerprints              FingerprintSet
	target_id                 string
	producer                  AuthenticatedToolchainObservation
	toolchain_profile_binding ToolchainProfileBinding
	toolchain_profile         JsonValue
	staging_observed          bool
}

// AuthenticatedNativeLaneMatrix is a file-authenticated envelope. Its fields stay private so only
// the T2b2 verdict bridge can reauthenticate its exact raw bytes and derive closed facts.
pub struct AuthenticatedNativeLaneMatrix {
	raw_source    string
	matrix        JsonValue
	matrix_sha256 string
	target_id     string
	subject       NativeGateSubjectModel
	subject_hash  string
	producer      ProducerToolchainModel
	validator     AuthenticatedToolchainObservation
	selected_run  NativeLaneRunKey
	results       []NativeLaneResult
}

// authenticate_manifest_file validates schema and semantics and accepts only manifests that do
// not depend on opaque filesystem provenance.
pub fn authenticate_manifest_file(automation_root string,
	manifest_path string) !AuthenticatedManifestModel {
	raw_source := os.read_file(manifest_path)!
	return authenticate_manifest_source(automation_root, raw_source)!
}

// authenticate_manifest_source is the unique non-staged authority path for both file loaders and
// durable replay. It validates the exact bytes against the complete Phase-A contract before it
// seals any derived registry, profile, producer, or fingerprint facts.
fn authenticate_manifest_source(automation_root string,
	raw_source string) !AuthenticatedManifestModel {
	issues := validate_manifest_source(automation_root, raw_source)!
	if issues.len > 0 {
		return error('manifest schema or semantics failed with ${issues.len} issue(s)')
	}
	manifest := parse_strict_json(raw_source)!
	if require_string_member(manifest, 'provenance_status')! == 'incomplete' {
		return error('incomplete provenance cannot produce an authenticated manifest')
	}
	if require_string_member(manifest, 'provenance_status')! == 'opaque-accepted'
		|| manifest_has_opaque_inputs(manifest)! {
		return error('opaque manifest requires observed staging and an immutable Git tree')
	}
	return build_authenticated_manifest(automation_root, raw_source, manifest, false)!
}

// authenticate_staged_manifest_file additionally observes opaque bytes and their authoritative
// Git mode before a manifest can become eligible for a verdict.
pub fn authenticate_staged_manifest_file(automation_root string, manifest_path string,
	staging StagingContract) !AuthenticatedManifestModel {
	issues := validate_staged_manifest(automation_root, manifest_path, staging)!
	if issues.len > 0 {
		return error('staged manifest contract failed with ${issues.len} issue(s)')
	}
	raw_source := os.read_file(manifest_path)!
	manifest := parse_strict_json(raw_source)!
	if require_string_member(manifest, 'provenance_status')! == 'incomplete' {
		return error(staged_provenance_incomplete_error)
	}
	return build_authenticated_manifest(automation_root, raw_source, manifest, true)!
}

fn build_authenticated_manifest(automation_root string, raw_source string, manifest JsonValue,
	staging_observed bool) !AuthenticatedManifestModel {
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	if validate_registry_semantics(registry)!.len > 0 {
		return error('authenticated manifest uses a non-authoritative target registry')
	}
	target_id := require_string_member(manifest, 'target_id')!
	profile_binding, profile := reviewed_toolchain_profile(automation_root, target_id)!
	producer := authenticated_manifest_producer_observation(target_id, manifest, profile_binding,
		profile)!
	fingerprints := manifest_fingerprints(raw_source, registry)!
	model := AuthenticatedManifestModel{
		raw_source:                raw_source
		manifest:                  manifest
		registry:                  registry
		fingerprints:              fingerprints
		target_id:                 target_id
		producer:                  producer
		toolchain_profile_binding: profile_binding
		toolchain_profile:         profile
		staging_observed:          staging_observed
	}
	validate_authenticated_manifest(model)!
	return model
}

fn validate_authenticated_manifest(model AuthenticatedManifestModel) ! {
	if model.raw_source == '' || model.target_id !in managed_target_ids {
		return error('authenticated manifest envelope is empty')
	}
	reparsed := parse_strict_json(model.raw_source)!
	if !json_equal(reparsed, model.manifest)
		|| require_string_member(reparsed, 'target_id')! != model.target_id
		|| validate_registry_semantics(model.registry)!.len > 0 {
		return error('authenticated manifest raw bytes, strict value, target, or registry diverged')
	}
	recomputed := manifest_fingerprints(model.raw_source, model.registry)!
	if recomputed != model.fingerprints {
		return error('authenticated manifest raw hash or semantic projections diverged')
	}
	registry_target := registry_target_by_id(model.registry, model.target_id)!
	registry_profile_binding := toolchain_profile_binding(registry_target)!
	if registry_profile_binding != model.toolchain_profile_binding
		|| require_string_member(model.toolchain_profile, 'target_id')! != model.target_id
		|| require_string_member(model.toolchain_profile, 'profile_id')! != model.toolchain_profile_binding.profile_id
		|| json_sha256(model.toolchain_profile) != model.toolchain_profile_binding.profile_sha256 {
		return error('authenticated manifest reviewed toolchain profile diverged')
	}
	validate_toolchain_profile_roles(model.toolchain_profile)!
	profile_id, profile_sha256, producer := manifest_toolchain_members(reparsed)!
	if profile_id != model.producer.profile_id || profile_sha256 != model.producer.profile_sha256
		|| require_string_member(producer, 'phase')! != model.producer.phase
		|| json_sha256(producer) != model.producer.observation_sha256
		|| !json_equal(producer, model.producer.observation) {
		return error('authenticated manifest producer toolchain binding diverged')
	}
	reauthenticated := authenticate_toolchain_observation_against_profile(model.target_id,
		model.toolchain_profile_binding, model.toolchain_profile, producer)!
	if reauthenticated.profile_id != model.producer.profile_id
		|| reauthenticated.profile_sha256 != model.producer.profile_sha256
		|| reauthenticated.phase != model.producer.phase
		|| reauthenticated.observation_sha256 != model.producer.observation_sha256
		|| !json_equal(reauthenticated.observation, model.producer.observation) {
		return error('authenticated manifest producer observation diverged from its sealed profile')
	}
	if (require_string_member(reparsed, 'provenance_status')! == 'opaque-accepted'
		|| manifest_has_opaque_inputs(reparsed)!) && !model.staging_observed {
		return error('opaque manifest is not backed by an observed staging contract')
	}
	if require_string_member(reparsed, 'contract_mode')! == 'production' && !model.staging_observed {
		return error('production manifest is not backed by observed staging and immutable Git bytes')
	}
}

// authenticated_manifest_producer_toolchain exposes only the four derived durable values needed
// by state consumers; the complete observation remains sealed in the authenticated envelope.
pub fn authenticated_manifest_producer_toolchain(model AuthenticatedManifestModel) !ProducerToolchainModel {
	validate_authenticated_manifest(model)!
	return producer_toolchain_model(model.producer)!
}

fn producer_toolchain_projection(producer ProducerToolchainModel) !JsonValue {
	return object_value_from_pairs(['profile_id', 'profile_sha256', 'observation_sha256',
		'observation_digest'], [
		JsonValue{ kind: .string_value, string_value: producer.profile_id },
		JsonValue{ kind: .string_value, string_value: producer.profile_sha256 },
		JsonValue{ kind: .string_value, string_value: producer.observation_sha256 },
		JsonValue{ kind: .string_value, string_value: producer.observation_digest },
	])!
}

fn manifest_resolved_inputs_projection(manifest JsonValue,
	producer ProducerToolchainModel) !JsonValue {
	mut sources := []JsonValue{}
	for source in require_array_member(manifest, 'sources')! {
		sources << select_object_members(source, ['id', 'repository', 'ref', 'sha', 'tree'])!
	}
	recipe := require_object_member(manifest, 'recipe')!
	return object_value_from_pairs(['sources', 'recipe_path', 'recipe_hash', 'contract_repository',
		'contract_sha', 'v_source_sha', 'producer_toolchain'], [
		JsonValue{ kind: .array, array_value: sources },
		require_member(recipe, 'path')!,
		require_member(recipe, 'sha256')!,
		require_member(manifest, 'contract_repository')!,
		require_member(manifest, 'contract_sha')!,
		require_member(manifest, 'v_source_sha')!,
		producer_toolchain_projection(producer)!,
	])!
}

fn resolved_inputs_manifest_projection(inputs ResolvedInputsModel) !JsonValue {
	mut sources := []JsonValue{}
	for source in inputs.sources {
		sources << object_value_from_pairs(['id', 'repository', 'ref', 'sha', 'tree'], [
			JsonValue{ kind: .string_value, string_value: source.id },
			JsonValue{ kind: .string_value, string_value: source.repository },
			JsonValue{ kind: .string_value, string_value: source.ref },
			JsonValue{ kind: .string_value, string_value: source.sha },
			JsonValue{ kind: .string_value, string_value: source.tree },
		])!
	}
	return object_value_from_pairs(['sources', 'recipe_path', 'recipe_hash', 'contract_repository',
		'contract_sha', 'v_source_sha', 'producer_toolchain'], [
		JsonValue{ kind: .array, array_value: sources },
		JsonValue{ kind: .string_value, string_value: inputs.recipe_path },
		JsonValue{ kind: .string_value, string_value: inputs.recipe_hash },
		JsonValue{ kind: .string_value, string_value: inputs.contract_repository },
		JsonValue{ kind: .string_value, string_value: inputs.contract_sha },
		JsonValue{ kind: .string_value, string_value: inputs.v_source_sha },
		producer_toolchain_projection(inputs.producer_toolchain)!,
	])!
}

// resolved_inputs_json_manifest_projection validates the complete durable root shape, including
// the independently evidenced source-check bijection, while returning only the seven fields
// backed by manifest bytes. source_checks must not be dropped from authority validation or
// compared as though they were manifest members.
fn resolved_inputs_json_manifest_projection(inputs JsonValue) !JsonValue {
	require_exact_keys(inputs, ['sources', 'source_checks', 'recipe_path', 'recipe_hash',
		'contract_repository', 'contract_sha', 'v_source_sha', 'producer_toolchain'])!
	sources := require_array_member(inputs, 'sources')!
	checks := require_array_member(inputs, 'source_checks')!
	if sources.len == 0 || checks.len != sources.len {
		return error('resolved inputs source checks are not a one-to-one source set')
	}
	mut source_ids := []string{}
	mut projected_sources := []JsonValue{cap: sources.len}
	for source in sources {
		require_exact_keys(source, ['id', 'repository', 'ref', 'sha', 'tree'])!
		source_id := require_string_member(source, 'id')!
		source_sha := require_string_member(source, 'sha')!
		if source_id == '' || source_id in source_ids
			|| require_string_member(source, 'repository')! == ''
			|| require_string_member(source, 'ref')! == '' || !is_lower_hex_40(source_sha)
			|| !is_lower_hex_40(require_string_member(source, 'tree')!) {
			return error('resolved inputs source projection is invalid or duplicated')
		}
		source_ids << source_id
		mut matches := 0
		for check in checks {
			require_exact_keys(check, ['source_id', 'resolved_sha', 'status', 'evidence_digest'])!
			if require_string_member(check, 'source_id')! == source_id
				&& require_string_member(check, 'resolved_sha')! == source_sha
				&& require_string_member(check, 'status')! == 'resolved'
				&& is_lower_hex_64(require_string_member(check, 'evidence_digest')!) {
				matches++
			}
		}
		if matches != 1 {
			return error('resolved inputs source checks are not a one-to-one source set')
		}
		projected_sources << select_object_members(source,
			['id', 'repository', 'ref', 'sha', 'tree'])!
	}
	mut check_ids := []string{}
	for check in checks {
		check_id := require_string_member(check, 'source_id')!
		if check_id in check_ids || check_id !in source_ids {
			return error('resolved inputs source checks are not a one-to-one source set')
		}
		check_ids << check_id
	}
	producer := require_object_member(inputs, 'producer_toolchain')!
	require_exact_keys(producer, ['profile_id', 'profile_sha256', 'observation_sha256',
		'observation_digest'])!
	if !is_toolchain_profile_id(require_string_member(producer, 'profile_id')!)
		|| !is_lower_hex_64(require_string_member(producer, 'profile_sha256')!)
		|| !is_lower_hex_64(require_string_member(producer, 'observation_sha256')!)
		|| !is_lower_hex_64(require_string_member(producer, 'observation_digest')!) {
		return error('resolved inputs producer projection is incomplete')
	}
	return object_value_from_pairs(['sources', 'recipe_path', 'recipe_hash', 'contract_repository',
		'contract_sha', 'v_source_sha', 'producer_toolchain'], [
		JsonValue{ kind: .array, array_value: projected_sources },
		require_member(inputs, 'recipe_path')!,
		require_member(inputs, 'recipe_hash')!,
		require_member(inputs, 'contract_repository')!,
		require_member(inputs, 'contract_sha')!,
		require_member(inputs, 'v_source_sha')!,
		producer,
	])!
}

// validate_authenticated_manifest_resolved_inputs closes the durable state back to the complete
// authenticated manifest projection. Both sides are rebuilt as canonical JSON instead of relying
// on a caller-selected partial comparison. Source-check evidence remains independently validated
// by the state machine, but no source, recipe, contract, producer identity, or input fingerprint
// may drift.
pub fn validate_authenticated_manifest_resolved_inputs(model AuthenticatedManifestModel,
	inputs ResolvedInputsModel, input_fingerprint string) ! {
	validate_authenticated_manifest(model)!
	validate_resolved_inputs(inputs)!
	if input_fingerprint != model.fingerprints.input_fingerprint {
		return error('resolved inputs do not bind the authenticated manifest input fingerprint')
	}
	producer := producer_toolchain_model(model.producer)!
	if !json_equal(resolved_inputs_manifest_projection(inputs)!, manifest_resolved_inputs_projection(model.manifest,
		producer)!) {
		return error('resolved inputs differ from the authenticated manifest projection')
	}
}

// validate_authenticated_manifest_resolved_inputs_value closes a schema-decoded durable target
// root back to the same manifest projection while retaining source_checks as independent evidence.
fn validate_authenticated_manifest_resolved_inputs_value(model AuthenticatedManifestModel,
	inputs JsonValue, input_fingerprint string) ! {
	validate_authenticated_manifest(model)!
	if input_fingerprint != model.fingerprints.input_fingerprint {
		return error('resolved inputs do not bind the authenticated manifest input fingerprint')
	}
	producer := producer_toolchain_model(model.producer)!
	if !json_equal(resolved_inputs_json_manifest_projection(inputs)!, manifest_resolved_inputs_projection(model.manifest,
		producer)!) {
		return error('resolved inputs differ from the authenticated manifest projection')
	}
}

fn manifest_has_opaque_inputs(manifest JsonValue) !bool {
	mut entries := require_array_member(manifest, 'inventory')!
	entries << require_array_member(manifest, 'overlays')!
	return entries.any(require_bool_member(it, 'opaque') or { false })
}

// validate_registry validates both its schema and the fixed Phase A inventory semantics.
pub fn validate_registry(automation_root string) ![]SchemaIssue {
	registry_path := os.join_path(automation_root, 'targets.json')
	schema_path := os.join_path(automation_root, 'schemas', 'targets.schema.json')
	mut issues := validate_json_file(schema_path, registry_path)!
	if issues.len > 0 {
		return issues
	}
	registry := parse_strict_json(os.read_file(registry_path)!)!
	issues << validate_registry_semantics(registry)!
	if issues.len == 0 {
		issues << validate_registry_onboarding_policies(automation_root, registry)!
		issues << validate_registry_managed_baseline_activation_policies(automation_root, registry)!
		issues << validate_registry_toolchain_profiles(automation_root, registry)!
	}
	return issues
}

fn validate_registry_toolchain_profiles(automation_root string,
	registry JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	for target in require_array_member(registry, 'managed_ci_targets')! {
		target_id := require_string_member(target, 'id')!
		binding := toolchain_profile_binding(target)!
		if binding.profile_path == '' {
			continue
		}
		load_toolchain_profile(automation_root, target_id, binding) or {
			issues << SchemaIssue{'$/managed_ci_targets', err.msg()}
		}
	}
	return issues
}

fn toolchain_profile_binding(target JsonValue) !ToolchainProfileBinding {
	profile := require_object_member(target, 'toolchain_profile')!
	profile_id := require_nullable_string_member(profile, 'profile_id')!
	profile_path := require_nullable_string_member(profile, 'profile_path')!
	profile_sha256 := require_nullable_string_member(profile, 'profile_sha256')!
	resolved := [profile_id != '', profile_path != '', profile_sha256 != '']
	if resolved.any(it) && resolved.any(!it) {
		return error('toolchain profile ID, path, and hash must all be null or all be resolved')
	}
	return ToolchainProfileBinding{
		profile_id:     profile_id
		profile_path:   profile_path
		profile_sha256: profile_sha256
	}
}

fn reviewed_toolchain_profile(automation_root string,
	target_id string) !(ToolchainProfileBinding, JsonValue) {
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	target := registry_target_by_id(registry, target_id)!
	binding := toolchain_profile_binding(target)!
	if binding.profile_path == '' {
		return error('target has no reviewed toolchain profile')
	}
	return binding, load_toolchain_profile(automation_root, target_id, binding)!
}

fn load_toolchain_profile(automation_root string, target_id string,
	binding ToolchainProfileBinding) !JsonValue {
	expected_path := 'toolchain-profiles/${target_id}.profile.json'
	if binding.profile_path != expected_path
		|| !contract_relative_path_is_safe(binding.profile_path) {
		return error('toolchain profile path differs from the exact target path')
	}
	validate_staged_parent_chain(automation_root, binding.profile_path) or {
		return error('toolchain profile parent chain is not physical')
	}
	profile_path := os.join_path(automation_root, binding.profile_path)
	profile_source := read_stable_toolchain_document(profile_path, 'toolchain profile')!
	profile := parse_strict_json(profile_source)!
	if profile_source != canonical_json(profile) {
		return error('toolchain profile bytes must be exact canonical JSON')
	}
	profile_issues := validate_json_value(os.join_path(automation_root, 'schemas',
		'toolchain-profile.schema.json'), profile)!
	if profile_issues.len > 0 {
		return error('toolchain profile schema failed with ${profile_issues.len} issue(s)')
	}
	if require_string_member(profile, 'target_id')! != target_id {
		return error('toolchain profile target differs from its registry target')
	}
	if require_string_member(profile, 'profile_id')! != binding.profile_id {
		return error('toolchain profile ID differs from the registry')
	}
	if json_sha256(profile) != binding.profile_sha256 {
		return error('toolchain profile hash differs from the registry')
	}
	validate_toolchain_profile_roles(profile)!
	return profile
}

fn read_stable_toolchain_document(path string, description string) !string {
	return read_stable_toolchain_document_with_boundary(path, description,
		no_toolchain_document_boundary)!
}

fn no_toolchain_document_boundary(_ string) ! {}

fn read_stable_toolchain_document_with_boundary(path string, description string,
	boundary fn (string) !) !string {
	path_before := physical_toolchain_document_path_snapshot(path, description)!
	mut document, handle_before := native_open_toolchain_document(path) or {
		return error('${description} cannot be opened as a physical regular file')
	}
	defer {
		native_close_toolchain_document(mut document)
	}
	require_physical_toolchain_document_snapshot(handle_before, description)!
	require_toolchain_document_path_handle_association(path_before, handle_before, description)!
	if handle_before.size > toolchain_identity_document_max_bytes {
		return error('${description} exceeds its strict byte bound')
	}
	mut bytes := []u8{cap: int(handle_before.size)}
	mut buffer := []u8{len: toolchain_identity_document_buffer_bytes}
	mut total := u64(0)
	for {
		read := native_read_toolchain_document(&document, mut buffer) or {
			return error('${description} cannot be read inside its byte bound')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > toolchain_identity_document_max_bytes {
			return error('${description} grew beyond its strict byte bound')
		}
		bytes << buffer[..read]
	}
	boundary(path)!
	handle_after := native_toolchain_document_snapshot(&document) or {
		return error('${description} identity changed while being read')
	}
	require_physical_toolchain_document_snapshot(handle_after, description)!
	if handle_before != handle_after {
		return error('${description} changed while being read')
	}
	path_after := physical_toolchain_document_path_snapshot(path, description)!
	require_toolchain_document_path_handle_association(path_after, handle_after, description)!
	if total != handle_before.size {
		return error('${description} changed while being read')
	}
	return bytes.bytestr()
}

fn require_physical_toolchain_document_snapshot(snapshot NativeFileSnapshot,
	description string) ! {
	if !snapshot.regular {
		return error('${description} is not a physical regular file')
	}
	if !snapshot.identity.reliable {
		return error('${description} identity is unavailable')
	}
}

fn physical_toolchain_document_path_snapshot(path string,
	description string) !NativeFileSnapshot {
	snapshot := native_path_file_snapshot(path) or {
		return error('${description} is not a physical regular file')
	}
	require_physical_toolchain_document_snapshot(snapshot, description)!
	return snapshot
}

fn physical_toolchain_document_handle_snapshot(file &os.File,
	description string) !NativeFileSnapshot {
	snapshot := native_open_file_snapshot(file) or {
		return error('${description} identity is unavailable')
	}
	require_physical_toolchain_document_snapshot(snapshot, description)!
	return snapshot
}

fn require_toolchain_document_path_handle_association(path_snapshot NativeFileSnapshot,
	handle_snapshot NativeFileSnapshot, description string) ! {
	if path_snapshot != handle_snapshot {
		return error('${description} path does not identify its open handle')
	}
}

// attest_toolchain_document_path_handle exposes the production path/handle association check so
// deterministic contract tests can replace a pathname after opening its former regular file.
pub fn attest_toolchain_document_path_handle(path string, file &os.File) ! {
	path_snapshot := physical_toolchain_document_path_snapshot(path, 'toolchain document')!
	handle_snapshot := physical_toolchain_document_handle_snapshot(file, 'toolchain document')!
	require_toolchain_document_path_handle_association(path_snapshot, handle_snapshot,
		'toolchain document')!
}

// attest_native_toolchain_document_open exposes the production native open/fstat guard to
// deterministic regular-file, FIFO, and device contract tests.
pub fn attest_native_toolchain_document_open(path string) ! {
	mut document, snapshot := native_open_toolchain_document(path)!
	defer {
		native_close_toolchain_document(mut document)
	}
	require_physical_toolchain_document_snapshot(snapshot, 'toolchain document')!
}

// attest_stable_toolchain_document_boundary exposes the production reader with one deterministic
// mutation boundary after the bounded read and before the second handle snapshot.
pub fn attest_stable_toolchain_document_boundary(path string, boundary fn (string) !) ! {
	read_stable_toolchain_document_with_boundary(path, 'toolchain document', boundary)!
}

fn ordered_toolchain_roles(value JsonValue, key string) ![]JsonValue {
	roles := require_array_member(value, key)!
	names := roles.map(require_string_member(it, 'role_id') or { '' })
	mut sorted_names := names.clone()
	sorted_names.sort()
	if names != sorted_names {
		return error('toolchain ${key} roles must be in strict lexical order')
	}
	for index, name in names {
		if index > 0 && name == names[index - 1] {
			return error('toolchain ${key} roles must be unique')
		}
	}
	return roles
}

fn ordered_toolchain_facts(value JsonValue, key string) ![]JsonValue {
	facts := require_array_member(value, key)!
	names := facts.map(require_string_member(it, 'name') or { '' })
	mut sorted_names := names.clone()
	sorted_names.sort()
	if names != sorted_names {
		return error('toolchain ${key} facts must be in strict lexical order')
	}
	for index, name in names {
		if index > 0 && name == names[index - 1] {
			return error('toolchain ${key} fact names must be unique')
		}
	}
	return facts
}

fn toolchain_strategy_requirements(strategy string) ![]ToolchainFactRequirement {
	return match strategy {
		'github-hosted' {
			[
				ToolchainFactRequirement{'arch', 'exact'},
				ToolchainFactRequirement{'compiler_binary_sha256', 'sha256'},
				ToolchainFactRequirement{'compiler_command', 'exact'},
				ToolchainFactRequirement{'compiler_family', 'exact'},
				ToolchainFactRequirement{'compiler_target', 'present'},
				ToolchainFactRequirement{'compiler_version', 'present'},
				ToolchainFactRequirement{'image_os', 'present'},
				ToolchainFactRequirement{'image_version', 'present'},
				ToolchainFactRequirement{'os', 'exact'},
				ToolchainFactRequirement{'runner_label', 'exact'},
			]
		}
		'cpa-host' {
			[
				ToolchainFactRequirement{'action_sha', 'exact'},
				ToolchainFactRequirement{'arch', 'exact'},
				ToolchainFactRequirement{'compiler_binary_sha256', 'sha256'},
				ToolchainFactRequirement{'compiler_command', 'exact'},
				ToolchainFactRequirement{'compiler_family', 'exact'},
				ToolchainFactRequirement{'compiler_target', 'present'},
				ToolchainFactRequirement{'compiler_version', 'present'},
				ToolchainFactRequirement{'image_os', 'present'},
				ToolchainFactRequirement{'image_version', 'present'},
				ToolchainFactRequirement{'os', 'exact'},
				ToolchainFactRequirement{'runner_label', 'exact'},
			]
		}
		'cpa-guest' {
			[
				ToolchainFactRequirement{'arch', 'exact'},
				ToolchainFactRequirement{'compiler_binary_sha256', 'sha256'},
				ToolchainFactRequirement{'compiler_command', 'exact'},
				ToolchainFactRequirement{'compiler_family', 'exact'},
				ToolchainFactRequirement{'compiler_target', 'present'},
				ToolchainFactRequirement{'compiler_version', 'present'},
				ToolchainFactRequirement{'guest_os', 'exact'},
				ToolchainFactRequirement{'observed_release', 'release-compatible'},
				ToolchainFactRequirement{'requested_release', 'exact'},
			]
		}
		'github-hosted-msys2' {
			[
				ToolchainFactRequirement{'arch', 'exact'},
				ToolchainFactRequirement{'compiler_binary_sha256', 'sha256'},
				ToolchainFactRequirement{'compiler_command', 'exact'},
				ToolchainFactRequirement{'compiler_family', 'exact'},
				ToolchainFactRequirement{'compiler_package', 'exact'},
				ToolchainFactRequirement{'compiler_target', 'present'},
				ToolchainFactRequirement{'compiler_version', 'present'},
				ToolchainFactRequirement{'image_os', 'present'},
				ToolchainFactRequirement{'image_version', 'present'},
				ToolchainFactRequirement{'msystem', 'exact'},
				ToolchainFactRequirement{'os', 'exact'},
				ToolchainFactRequirement{'package_version', 'present'},
				ToolchainFactRequirement{'runner_label', 'exact'},
				ToolchainFactRequirement{'setup_action_sha', 'exact'},
			]
		}
		else {
			return error('toolchain profile identity strategy is not closed')
		}
	}
}

fn toolchain_target_phase_strategies(target_id string) ![]string {
	return match target_id {
		'freebsd-amd64', 'openbsd-amd64' { ['cpa-guest', 'cpa-host'] }
		'linux-amd64', 'macos-amd64', 'macos-arm64' { ['github-hosted'] }
		'windows-amd64' { ['github-hosted-msys2'] }
		else { return error('toolchain profile target has no closed phase topology') }
	}
}

fn toolchain_target_exact_facts(target_id string, strategy string) !map[string]string {
	return match strategy {
		'github-hosted' {
			match target_id {
				'linux-amd64' {
					{
						'arch':             'amd64'
						'compiler_command': 'gcc'
						'compiler_family':  'gcc'
						'os':               'linux'
						'runner_label':     'ubuntu-24.04'
					}
				}
				'macos-amd64' {
					{
						'arch':             'amd64'
						'compiler_command': 'clang'
						'compiler_family':  'clang'
						'os':               'macos'
						'runner_label':     'macos-15-intel'
					}
				}
				'macos-arm64' {
					{
						'arch':             'arm64'
						'compiler_command': 'clang'
						'compiler_family':  'clang'
						'os':               'macos'
						'runner_label':     'macos-15'
					}
				}
				else {
					return error('GitHub-hosted toolchain strategy is not valid for this target')
				}
			}
		}
		'cpa-host' {
			if target_id !in ['freebsd-amd64', 'openbsd-amd64'] {
				return error('CPA host toolchain strategy is not valid for this target')
			}
			{
				'arch':             'amd64'
				'compiler_command': 'clang'
				'compiler_family':  'clang'
				'os':               'linux'
				'runner_label':     'ubuntu-24.04'
			}
		}
		'cpa-guest' {
			match target_id {
				'freebsd-amd64' {
					{
						'arch':              'amd64'
						'compiler_command':  'clang'
						'compiler_family':   'clang'
						'guest_os':          'freebsd'
						'observed_release':  '15.1'
						'requested_release': '15.1'
					}
				}
				'openbsd-amd64' {
					{
						'arch':              'amd64'
						'compiler_command':  'clang'
						'compiler_family':   'clang'
						'guest_os':          'openbsd'
						'observed_release':  '7.8'
						'requested_release': '7.8'
					}
				}
				else {
					return error('CPA guest toolchain strategy is not valid for this target')
				}
			}
		}
		'github-hosted-msys2' {
			if target_id != 'windows-amd64' {
				return error('MSYS2 toolchain strategy is not valid for this target')
			}
			{
				'arch':             'amd64'
				'compiler_command': 'gcc'
				'compiler_family':  'gcc'
				'compiler_package': 'mingw-w64-ucrt-x86_64-gcc'
				'msystem':          'UCRT64'
				'os':               'windows'
				'runner_label':     'windows-2022'
			}
		}
		else {
			return error('toolchain profile identity strategy is not closed')
		}
	}
}

fn validate_toolchain_role_policy(target_id string, role JsonValue) ! {
	strategy := require_string_member(role, 'identity_strategy')!
	requirements := toolchain_strategy_requirements(strategy)!
	facts := ordered_toolchain_facts(role, 'identity_policy')!
	actual_names := facts.map(require_string_member(it, 'name') or { '' })
	actual_matches := facts.map(require_string_member(it, 'match') or { '' })
	if actual_names != requirements.map(it.name)
		|| actual_matches != requirements.map(it.match_kind) {
		return error('toolchain identity policy differs from the exact strategy requirements')
	}
	for fact in facts {
		name := require_string_member(fact, 'name')!
		match_kind := require_string_member(fact, 'match')!
		if match_kind in ['exact', 'release-compatible'] {
			value := require_string_member(fact, 'value')!
			if name in ['action_sha', 'setup_action_sha'] && !is_lower_hex_40(value) {
				return error('toolchain action identity must be a full lowercase commit SHA')
			}
			if name == 'msystem' && value != 'UCRT64' {
				return error('MSYS2 toolchain identity must require UCRT64')
			}
		}
	}
	exact_values := toolchain_target_exact_facts(target_id, strategy)!
	for fact in facts {
		name := require_string_member(fact, 'name')!
		if name in exact_values && require_string_member(fact, 'value')! != exact_values[name] {
			return error('toolchain identity policy differs from the exact managed target values')
		}
	}
	if strategy == 'cpa-guest' {
		mut requested_release := ''
		mut compatible_release := ''
		for fact in facts {
			name := require_string_member(fact, 'name')!
			if name == 'requested_release' {
				requested_release = require_string_member(fact, 'value')!
			} else if name == 'observed_release' {
				compatible_release = require_string_member(fact, 'value')!
			}
		}
		parts := requested_release.split('.')
		if requested_release != compatible_release || parts.len != 2 || parts.any(it == ''
			|| it.bytes().any(!it.is_digit())) {
			return error('guest release policy must bind one numeric major.minor series')
		}
	}
}

fn validate_toolchain_profile_roles(profile JsonValue) ! {
	target_id := require_string_member(profile, 'target_id')!
	producer := ordered_toolchain_roles(profile, 'producer')!
	validator := ordered_toolchain_roles(profile, 'validator')!
	expected_strategies := toolchain_target_phase_strategies(target_id)!
	for roles in [producer, validator] {
		mut strategies := roles.map(require_string_member(it, 'identity_strategy') or { '' })
		strategies.sort()
		if strategies != expected_strategies {
			return error('toolchain phase strategy topology differs from the exact managed target')
		}
		for role in roles {
			validate_toolchain_role_policy(target_id, role)!
		}
	}
	mut names := producer.map(require_string_member(it, 'role_id') or { '' })
	names << validator.map(require_string_member(it, 'role_id') or { '' })
	mut sorted_names := names.clone()
	sorted_names.sort()
	for index, name in sorted_names {
		if index > 0 && name == sorted_names[index - 1] {
			return error('toolchain profile roles must be globally unique')
		}
	}
}

fn release_is_compatible(expected string, observed string) bool {
	if observed == expected {
		return true
	}
	for separator in ['-', '.'] {
		prefix := '${expected}${separator}'
		if observed.starts_with(prefix) && observed.len > prefix.len {
			suffix := observed[prefix.len..]
			return suffix.bytes().all(it.is_alnum() || it in [`_`, `.`, `+`, `-`])
		}
	}
	return false
}

fn validate_toolchain_resolved_identity(policy_role JsonValue, observed_role JsonValue) ! {
	policy_facts := ordered_toolchain_facts(policy_role, 'identity_policy')!
	observed_facts := ordered_toolchain_facts(observed_role, 'resolved_identity')!
	policy_names := policy_facts.map(require_string_member(it, 'name') or { '' })
	observed_names := observed_facts.map(require_string_member(it, 'name') or { '' })
	if observed_names != policy_names {
		return error('toolchain observed facts differ from the exact strategy fact set')
	}
	for index, observed_fact in observed_facts {
		policy_fact := policy_facts[index]
		match_kind := require_string_member(policy_fact, 'match')!
		observed_value := require_string_member(observed_fact, 'value')!
		valid := match match_kind {
			'exact' {
				observed_value == require_string_member(policy_fact, 'value')!
			}
			'present' {
				observed_value != ''
			}
			'sha256' {
				is_lower_hex_64(observed_value)
			}
			'release-compatible' {
				release_is_compatible(require_string_member(policy_fact, 'value')!, observed_value)
			}
			else {
				false
			}
		}
		if !valid {
			return error('toolchain observed fact does not satisfy its reviewed match policy')
		}
	}
}

// toolchain_role_resolution_digest derives one role resolution from its immutable profile
// binding, phase, strategy, and closed readable fact set.
pub fn toolchain_role_resolution_digest(observation JsonValue, role JsonValue) !string {
	projection := object_value_from_pairs(['schema_version', 'target_id', 'profile_id',
		'profile_sha256', 'phase', 'role_id', 'identity_strategy', 'resolved_identity'], [
		require_member(observation, 'schema_version')!,
		require_member(observation, 'target_id')!,
		require_member(observation, 'profile_id')!,
		require_member(observation, 'profile_sha256')!,
		require_member(observation, 'phase')!,
		require_member(role, 'role_id')!,
		require_member(role, 'identity_strategy')!,
		require_member(role, 'resolved_identity')!,
	])!
	return json_sha256(projection)
}

// toolchain_observation_digest derives the top-level digest over every observation field except
// the digest itself, including each external evidence hash and derived role resolution.
pub fn toolchain_observation_digest(observation JsonValue) !string {
	projection := select_object_members(observation, ['schema_version', 'target_id', 'profile_id',
		'profile_sha256', 'phase', 'roles'])!
	return json_sha256(projection)
}

// authenticate_toolchain_observation_value proves that one strict JSON value matches the reviewed
// target profile. Embedded manifest observations use this path so their authority is value-level,
// while the file wrapper below additionally attests canonical file bytes.
pub fn authenticate_toolchain_observation_value(automation_root string, target_id string,
	observation JsonValue) !AuthenticatedToolchainObservation {
	binding, profile := reviewed_toolchain_profile(automation_root, target_id)!
	observation_issues := validate_json_value(os.join_path(automation_root, 'schemas',
		'toolchain-observation.schema.json'), observation)!
	if observation_issues.len > 0 {
		return error('toolchain observation schema failed with ${observation_issues.len} issue(s)')
	}
	return authenticate_toolchain_observation_against_profile(target_id, binding, profile,
		observation)!
}

// authenticate_toolchain_observation_against_profile binds an already schema-validated value to
// one exact profile snapshot. Native matrices use the profile retained by their authenticated
// manifest, so a second registry or profile read cannot silently change the authority.
fn authenticate_toolchain_observation_against_profile(target_id string,
	binding ToolchainProfileBinding, profile JsonValue,
	observation JsonValue) !AuthenticatedToolchainObservation {
	if require_string_member(observation, 'target_id')! != target_id {
		return error('toolchain observation target differs from the requested target')
	}
	if require_string_member(observation, 'profile_id')! != binding.profile_id
		|| require_string_member(observation, 'profile_sha256')! != binding.profile_sha256 {
		return error('toolchain observation profile binding differs from the registry')
	}
	phase := require_string_member(observation, 'phase')!
	expected_roles := ordered_toolchain_roles(profile, phase)!
	observed_roles := ordered_toolchain_roles(observation, 'roles')!
	expected_names := expected_roles.map(require_string_member(it, 'role_id') or { '' })
	observed_names := observed_roles.map(require_string_member(it, 'role_id') or { '' })
	if observed_names != expected_names {
		return error('toolchain observation roles differ from the exact profile phase')
	}
	for index, observed in observed_roles {
		expected_role := expected_roles[index]
		if require_string_member(observed, 'identity_strategy')! != require_string_member(expected_role,
			'identity_strategy')! {
			return error('toolchain observation strategy differs from the reviewed profile')
		}
		validate_toolchain_resolved_identity(expected_role, observed)!
		if require_string_member(observed, 'resolution_digest')! != toolchain_role_resolution_digest(observation,
			observed)! {
			return error('toolchain observation resolution digest is not derived from its canonical identity')
		}
	}
	if require_string_member(observation, 'observation_digest')! != toolchain_observation_digest(observation)! {
		return error('toolchain observation digest is not derived from the complete canonical observation')
	}
	return AuthenticatedToolchainObservation{
		target_id:          target_id
		profile_id:         binding.profile_id
		profile_sha256:     binding.profile_sha256
		phase:              phase
		observation_sha256: json_sha256(observation)
		observation:        observation
	}
}

// authenticate_toolchain_observation_file loads one canonical observation and proves that its
// phase, roles, and resolution digests exactly match the reviewed target profile.
pub fn authenticate_toolchain_observation_file(automation_root string, target_id string,
	observation_path string) !AuthenticatedToolchainObservation {
	reviewed_toolchain_profile(automation_root, target_id)!
	observation_source := read_stable_toolchain_document(observation_path, 'toolchain observation')!
	observation := parse_strict_json(observation_source)!
	if observation_source != canonical_json(observation) {
		return error('toolchain observation bytes must be exact canonical JSON')
	}
	return authenticate_toolchain_observation_value(automation_root, target_id, observation)!
}

fn is_toolchain_profile_id(value string) bool {
	if value.len < 3 || value.len > 128 {
		return false
	}
	bytes := value.bytes()
	if !((bytes[0] >= `a` && bytes[0] <= `z`) || bytes[0].is_digit()) {
		return false
	}
	return bytes.all((it >= `a` && it <= `z`) || it.is_digit() || it in [`.`, `_`, `-`])
}

fn manifest_toolchain_members(manifest JsonValue) !(string, string, JsonValue) {
	toolchain := require_object_member(manifest, 'toolchain')!
	profile_id := require_nullable_string_member(toolchain, 'profile_id')!
	profile_sha256 := require_nullable_string_member(toolchain, 'profile_sha256')!
	producer := require_member(toolchain, 'producer_observation')!
	if (profile_id == '') != (profile_sha256 == '') {
		return error('manifest toolchain profile ID and hash must be resolved as one pair')
	}
	if profile_id != '' && !is_toolchain_profile_id(profile_id) {
		return error('manifest toolchain profile ID is outside the closed syntax')
	}
	if profile_id == '' && producer.kind != .null_value {
		return error('manifest producer observation requires a resolved profile binding')
	}
	if producer.kind !in [.null_value, .object] {
		return error('manifest producer observation must be an object or null')
	}
	return profile_id, profile_sha256, producer
}

fn validate_manifest_producer_toolchain(automation_root string, target_id string,
	manifest JsonValue) ! {
	profile_id, profile_sha256, producer := manifest_toolchain_members(manifest)!
	if profile_id != '' {
		binding, _ := reviewed_toolchain_profile(automation_root, target_id)!
		if binding.profile_id != profile_id || binding.profile_sha256 != profile_sha256 {
			return error('manifest toolchain profile binding differs from the registry')
		}
	}
	if producer.kind == .null_value {
		return
	}
	authenticated := authenticate_toolchain_observation_value(automation_root, target_id, producer)!
	if authenticated.phase != 'producer' {
		return error('manifest toolchain observation must have producer phase')
	}
	if authenticated.profile_id != profile_id || authenticated.profile_sha256 != profile_sha256 {
		return error('manifest toolchain profile binding differs from its producer observation')
	}
}

fn authenticated_manifest_producer_observation(target_id string, manifest JsonValue,
	binding ToolchainProfileBinding, profile JsonValue) !AuthenticatedToolchainObservation {
	profile_id, profile_sha256, producer := manifest_toolchain_members(manifest)!
	if producer.kind == .null_value {
		return error('incomplete producer toolchain cannot authenticate a manifest')
	}
	authenticated := authenticate_toolchain_observation_against_profile(target_id, binding,
		profile, producer)!
	// Registry and observation authentication normally reject a mismatched profile first. This
	// final comparison is defense-in-depth for the sealed envelope construction.
	if authenticated.phase != 'producer' || authenticated.profile_id != profile_id
		|| authenticated.profile_sha256 != profile_sha256 {
		return error('manifest producer toolchain differs from its reviewed profile binding')
	}
	return authenticated
}

fn producer_toolchain_model(producer AuthenticatedToolchainObservation) !ProducerToolchainModel {
	return ProducerToolchainModel{
		profile_id:         producer.profile_id
		profile_sha256:     producer.profile_sha256
		observation_sha256: producer.observation_sha256
		observation_digest: require_string_member(producer.observation, 'observation_digest')!
	}
}

fn validate_registry_onboarding_policies(automation_root string, registry JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	for target in require_array_member(registry, 'managed_ci_targets')! {
		target_id := require_string_member(target, 'id')!
		binding := legacy_onboarding_binding(target)!
		if binding.policy_path == '' {
			continue
		}
		load_legacy_onboarding_policy(automation_root, target_id, binding,
			toolchain_profile_binding(target)!) or {
			issues << SchemaIssue{'$/managed_ci_targets', err.msg()}
		}
	}
	return issues
}

fn validate_registry_managed_baseline_activation_policies(automation_root string,
	registry JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	for target in require_array_member(registry, 'managed_ci_targets')! {
		target_id := require_string_member(target, 'id')!
		binding := managed_baseline_activation_binding(target)!
		if binding.policy_path == '' {
			continue
		}
		load_managed_baseline_activation_policy(automation_root, target_id, binding,
			toolchain_profile_binding(target)!) or {
			issues << SchemaIssue{'$/managed_ci_targets', err.msg()}
		}
	}
	return issues
}

fn legacy_onboarding_binding(target JsonValue) !LegacyOnboardingBinding {
	onboarding := require_object_member(target, 'legacy_onboarding')!
	policy_path := require_nullable_string_member(onboarding, 'policy_path')!
	policy_sha256 := require_nullable_string_member(onboarding, 'policy_sha256')!
	if (policy_path == '') != (policy_sha256 == '') {
		return error('legacy onboarding policy path and hash must both be null or both be resolved')
	}
	return LegacyOnboardingBinding{
		base_sha:      require_string_member(onboarding, 'base_sha')!
		policy_path:   policy_path
		policy_sha256: policy_sha256
	}
}

fn managed_baseline_activation_binding(target JsonValue) !ManagedBaselineActivationBinding {
	activation := require_object_member(target, 'managed_baseline_activation')!
	policy_path := require_nullable_string_member(activation, 'policy_path')!
	policy_sha256 := require_nullable_string_member(activation, 'policy_sha256')!
	if (policy_path == '') != (policy_sha256 == '') {
		return error('managed baseline activation policy path and hash must both be null or both be resolved')
	}
	return ManagedBaselineActivationBinding{
		base_sha:                 require_string_member(activation, 'base_sha')!
		base_tree:                require_string_member(activation, 'base_tree')!
		parent_sha:               require_string_member(activation, 'parent_sha')!
		base_manifest_sha256:     require_string_member(activation, 'base_manifest_sha256')!
		base_contract_repository: require_string_member(activation, 'base_contract_repository')!
		base_contract_sha:        require_string_member(activation, 'base_contract_sha')!
		policy_path:              policy_path
		policy_sha256:            policy_sha256
	}
}

fn reviewed_legacy_onboarding_binding(automation_root string, target_id string,
	base_sha string) !(LegacyOnboardingBinding, JsonValue) {
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	target := registry_target_by_id(registry, target_id)!
	binding := legacy_onboarding_binding(target)!
	if binding.policy_path == '' {
		return error('target has no reviewed legacy onboarding policy')
	}
	if base_sha != binding.base_sha {
		return error('legacy onboarding base differs from the reviewed target pin')
	}
	policy := load_legacy_onboarding_policy(automation_root, target_id, binding,
		toolchain_profile_binding(target)!)!
	return binding, policy
}

fn reviewed_managed_baseline_activation_binding(automation_root string, target_id string,
	base_sha string) !(ManagedBaselineActivationBinding, JsonValue) {
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	target := registry_target_by_id(registry, target_id)!
	binding := managed_baseline_activation_binding(target)!
	if binding.policy_path == '' {
		return error('target has no reviewed managed baseline activation policy')
	}
	if base_sha != binding.base_sha {
		return error('managed baseline activation base differs from the reviewed target pin')
	}
	policy := load_managed_baseline_activation_policy(automation_root, target_id, binding,
		toolchain_profile_binding(target)!)!
	return binding, policy
}

fn load_legacy_onboarding_policy(automation_root string, target_id string,
	binding LegacyOnboardingBinding, toolchain_binding ToolchainProfileBinding) !JsonValue {
	expected_path := 'onboarding/${target_id}.policy.json'
	if binding.policy_path != expected_path || !contract_relative_path_is_safe(binding.policy_path) {
		return error('legacy onboarding policy path differs from the exact target path')
	}
	validate_staged_parent_chain(automation_root, binding.policy_path) or {
		return error('legacy onboarding policy parent chain is not physical')
	}
	policy_path := os.join_path(automation_root, binding.policy_path)
	if !os.is_file(policy_path) || os.is_link(policy_path) {
		return error('legacy onboarding policy is not a physical regular file')
	}
	policy_source := os.read_file(policy_path)!
	policy := parse_strict_json(policy_source)!
	if policy_source != canonical_json(policy) {
		return error('legacy onboarding policy bytes must be exact canonical JSON')
	}
	policy_issues := validate_json_value(os.join_path(automation_root, 'schemas',
		'onboarding-policy.schema.json'), policy)!
	if policy_issues.len > 0 {
		return error('legacy onboarding policy schema failed with ${policy_issues.len} issue(s)')
	}
	if require_integer_member(policy, 'projection_version')! != 1 {
		return error('legacy onboarding policy must retain projection version 1')
	}
	if require_string_member(policy, 'target_id')! != target_id {
		return error('legacy onboarding policy target differs from its registry target')
	}
	policy_toolchain := require_object_member(policy, 'toolchain')!
	if toolchain_binding.profile_id == ''
		|| require_string_member(policy_toolchain, 'profile_id')! != toolchain_binding.profile_id
		|| require_string_member(policy_toolchain, 'profile_sha256')! != toolchain_binding.profile_sha256 {
		return error('legacy onboarding policy toolchain differs from the reviewed target profile')
	}
	if json_sha256(policy) != binding.policy_sha256 {
		return error('legacy onboarding policy hash differs from the registry')
	}
	return policy
}

fn load_managed_baseline_activation_policy(automation_root string, target_id string,
	binding ManagedBaselineActivationBinding,
	toolchain_binding ToolchainProfileBinding) !JsonValue {
	expected_path := 'baseline-activation/${target_id}.policy.json'
	if binding.policy_path != expected_path || !contract_relative_path_is_safe(binding.policy_path) {
		return error('managed baseline activation policy path differs from the exact target path')
	}
	validate_staged_parent_chain(automation_root, binding.policy_path) or {
		return error('managed baseline activation policy parent chain is not physical')
	}
	policy_path := os.join_path(automation_root, binding.policy_path)
	if !os.is_file(policy_path) || os.is_link(policy_path) {
		return error('managed baseline activation policy is not a physical regular file')
	}
	policy_source := read_stable_toolchain_document(policy_path,
		'managed baseline activation policy')!
	policy := parse_strict_json(policy_source)!
	if policy_source != canonical_json(policy) {
		return error('managed baseline activation policy bytes must be exact canonical JSON')
	}
	policy_issues := validate_json_value(os.join_path(automation_root, 'schemas',
		'onboarding-policy.schema.json'), policy)!
	if policy_issues.len > 0 {
		return error('managed baseline activation policy schema failed with ${policy_issues.len} issue(s)')
	}
	if require_integer_member(policy, 'projection_version')! != 2 {
		return error('managed baseline activation policy must use projection version 2')
	}
	managed_baseline_source_commit_evidence(policy)!
	if require_string_member(policy, 'target_id')! != target_id {
		return error('managed baseline activation policy target differs from its registry target')
	}
	policy_toolchain := require_object_member(policy, 'toolchain')!
	if toolchain_binding.profile_id == ''
		|| require_string_member(policy_toolchain, 'profile_id')! != toolchain_binding.profile_id
		|| require_string_member(policy_toolchain, 'profile_sha256')! != toolchain_binding.profile_sha256 {
		return error('managed baseline activation policy toolchain differs from the reviewed target profile')
	}
	if json_sha256(policy) != binding.policy_sha256 {
		return error('managed baseline activation policy hash differs from the registry')
	}
	return policy
}

fn manifest_static_payload_policy(manifest JsonValue) !JsonValue {
	mut payload_values := []JsonValue{}
	for collection in ['overlays', 'inventory', 'outputs'] {
		mut projected_entries := []JsonValue{}
		for entry in require_array_member(manifest, collection)! {
			provenance := select_object_members(require_object_member(entry, 'provenance')!, [
				'repository',
				'source_path',
				'license',
			])!
			static_entry := select_object_members(entry, ['path', 'kind', 'git_mode',
				'symlink_target', 'role', 'opaque', 'opaque_acceptance_id', 'format', 'object_type',
				'machine', 'os_abi'])!
			projected_entries << append_object_members(static_entry, ['provenance'], [
				provenance,
			])!
		}
		payload_values << JsonValue{
			kind:        .array
			array_value: projected_entries
		}
	}
	return object_value_from_pairs(['overlays', 'inventory', 'outputs'], payload_values)!
}

// legacy_onboarding_policy_projection removes only byte- and run-specific facts from a manifest.
// Array order and the payload collection boundary remain authoritative inputs to its JCS hash.
pub fn legacy_onboarding_policy_projection(manifest JsonValue) !JsonValue {
	mut sources := []JsonValue{}
	for source in require_array_member(manifest, 'sources')! {
		sources << select_object_members(source, ['id', 'repository', 'ref'])!
	}
	mut patches := []JsonValue{}
	for patch in require_array_member(manifest, 'patches')! {
		patches << select_object_members(patch, ['id', 'path', 'order', 'category',
			'auto_deprecatable', 'state', 'effects'])!
	}
	mut transforms := []JsonValue{}
	for transform in require_array_member(manifest, 'transforms')! {
		transforms << select_object_members(transform, ['id', 'path', 'owner', 'order', 'apply_stage',
			'effect_ids'])!
	}
	payload_policy := manifest_static_payload_policy(manifest)!
	recipe := select_object_members(require_object_member(manifest, 'recipe')!, [
		'path',
		'version',
	])!
	toolchain := manifest_toolchain_profile_projection(manifest)!
	return object_value_from_pairs(['projection_version', 'schema_version', 'contract_version',
		'target_id', 'branch', 'sources', 'recipe', 'toolchain', 'patches', 'transforms',
		'header_effects', 'integrations', 'probes', 'affected_targets', 'payload_policy'], [
		JsonValue{ kind: .integer, int_value: 1 },
		require_member(manifest, 'schema_version')!,
		require_member(manifest, 'contract_version')!,
		require_member(manifest, 'target_id')!,
		require_member(manifest, 'branch')!,
		JsonValue{ kind: .array, array_value: sources },
		recipe,
		toolchain,
		JsonValue{ kind: .array, array_value: patches },
		JsonValue{ kind: .array, array_value: transforms },
		require_member(manifest, 'header_effects')!,
		require_member(manifest, 'integrations')!,
		require_member(manifest, 'probes')!,
		require_member(manifest, 'affected_targets')!,
		payload_policy,
	])!
}

// managed_baseline_activation_policy_projection adds an independent, reviewed commit-object
// authority to the otherwise static onboarding projection. The legacy version 1 projection is
// deliberately left byte-for-byte unchanged.
pub fn managed_baseline_activation_policy_projection(manifest JsonValue,
	source_commit_evidence JsonValue) !JsonValue {
	legacy := legacy_onboarding_policy_projection(manifest)!
	mut keys := legacy.object_keys.clone()
	mut values := legacy.object_values.clone()
	values[0] = JsonValue{
		kind:      .integer
		int_value: 2
	}
	keys << 'source_commit_evidence'
	values << source_commit_evidence
	projection := object_value_from_pairs(keys, values)!
	managed_baseline_source_commit_evidence(projection)!
	return projection
}

fn managed_baseline_source_commit_evidence(policy JsonValue) ![]ManagedBaselineSourceCommitEvidence {
	if require_integer_member(policy, 'projection_version')! != 2 {
		return error('managed baseline source commit evidence requires projection version 2')
	}
	sources := require_array_member(policy, 'sources')!
	entries := require_array_member(policy, 'source_commit_evidence')!
	if sources.len == 0 || sources.len > 3 || entries.len != sources.len {
		return error('managed baseline source commit evidence must be bijective with the source matrix')
	}
	mut source_ids := []string{cap: sources.len}
	for source in sources {
		source_id := require_string_member(source, 'id')!
		if source_id in source_ids {
			return error('managed baseline source matrix contains a duplicate source ID')
		}
		source_ids << source_id
	}
	mut evidence := []ManagedBaselineSourceCommitEvidence{cap: entries.len}
	mut evidence_ids := []string{cap: entries.len}
	for entry in entries {
		authority := require_string_member(entry, 'authority')!
		mut parsed := ManagedBaselineSourceCommitEvidence{
			id:         require_string_member(entry, 'id')!
			repository: require_string_member(entry, 'repository')!
			ref:        require_string_member(entry, 'ref')!
			authority:  authority
		}
		if parsed.id in evidence_ids {
			return error('managed baseline source commit evidence contains a duplicate source ID')
		}
		if authority == 'source-commit-object' {
			require_exact_keys(entry, ['id', 'repository', 'ref', 'authority', 'sha', 'tree',
				'raw_commit_base64'])!
			if parsed.id !in ['tinycc', 'bdwgc', 'libatomic_ops'] {
				return error('managed baseline external commit authority has an unsupported source ID')
			}
			parsed = ManagedBaselineSourceCommitEvidence{
				...parsed
				sha:               require_string_member(entry, 'sha')!
				tree:              require_string_member(entry, 'tree')!
				raw_commit_base64: require_string_member(entry, 'raw_commit_base64')!
			}
			validate_managed_baseline_raw_commit_evidence(parsed)!
		} else if authority == 'runtime-contract' {
			require_exact_keys(entry, ['id', 'repository', 'ref', 'authority'])!
			if parsed.id != 'v-libgc' || parsed.repository != 'https://github.com/vlang/v.git'
				|| parsed.ref != 'master' {
				return error('managed baseline runtime-contract authority is restricted to v-libgc')
			}
		} else {
			return error('managed baseline source commit evidence authority is unsupported')
		}
		evidence_ids << parsed.id
		evidence << parsed
	}
	for entry in evidence {
		if entry.id !in source_ids {
			return error('managed baseline source commit evidence contains an unreferenced source ID')
		}
	}
	for index, source in sources {
		source_id := require_string_member(source, 'id')!
		entry := evidence[index]
		if entry.id != source_id {
			return error('managed baseline source commit evidence order differs from the source matrix')
		}
		if entry.repository != require_string_member(source, 'repository')!
			|| entry.ref != require_string_member(source, 'ref')! {
			return error('managed baseline source commit evidence changed source repository or ref')
		}
	}
	return evidence
}

fn validate_managed_baseline_raw_commit_evidence(
	evidence ManagedBaselineSourceCommitEvidence) ! {
	if !is_lower_hex_40(evidence.sha) || !is_lower_hex_40(evidence.tree) {
		return error('managed baseline commit evidence must use lowercase SHA-1 object IDs')
	}
	if evidence.raw_commit_base64.len < 4
		|| evidence.raw_commit_base64.len > managed_baseline_source_commit_base64_max_bytes {
		return error('managed baseline raw commit evidence exceeds its encoded byte bound')
	}
	raw := base64.decode(evidence.raw_commit_base64)
	if raw.len == 0 || raw.len > managed_baseline_source_commit_max_bytes
		|| base64.encode(raw) != evidence.raw_commit_base64 {
		return error('managed baseline raw commit evidence is not canonical bounded base64')
	}
	mut material := 'commit ${raw.len}\x00'.bytes()
	material << raw
	if sha1.sum(material).hex() != evidence.sha {
		return error('managed baseline raw commit evidence SHA differs from its Git object ID')
	}
	raw_source := raw.bytestr()
	header_end := raw_source.index('\n\n') or {
		return error('managed baseline raw commit evidence has no complete Git commit header')
	}
	header := raw_source[..header_end]
	if header == '' || header.contains('\x00') || header.contains('\r') {
		return error('managed baseline raw commit evidence has a malformed Git commit header')
	}
	lines := header.split('\n')
	mut tree_count := 0
	mut observed_tree := ''
	for index, line in lines {
		if line == '' {
			return error('managed baseline raw commit evidence has a malformed Git commit header')
		}
		if line.starts_with('tree ') {
			tree_count++
			if index == 0 {
				observed_tree = line.all_after('tree ')
			}
		}
	}
	if tree_count != 1 || observed_tree == '' || !is_lower_hex_40(observed_tree) {
		return error('managed baseline raw commit evidence must start with one exact tree header')
	}
	if observed_tree != evidence.tree {
		return error('managed baseline raw commit evidence tree differs from its declared tree')
	}
}

// manifest_toolchain_profile_projection intentionally excludes the producer observation. A
// monthly build may refresh observed facts, but it cannot migrate the reviewed profile authority.
fn manifest_toolchain_profile_projection(manifest JsonValue) !JsonValue {
	return select_object_members(require_object_member(manifest, 'toolchain')!, [
		'profile_id',
		'profile_sha256',
	])!
}

pub fn legacy_onboarding_policy_sha256(manifest JsonValue) !string {
	return json_sha256(legacy_onboarding_policy_projection(manifest)!)
}

fn validate_manifest_legacy_onboarding_policy(manifest JsonValue, policy JsonValue) ! {
	projection := legacy_onboarding_policy_projection(manifest)!
	if !json_equal(projection, policy) {
		return error('candidate manifest differs from the reviewed legacy onboarding policy')
	}
}

fn validate_manifest_managed_baseline_activation_policy(manifest JsonValue,
	policy JsonValue) ! {
	projection := managed_baseline_activation_policy_projection(manifest, require_member(policy,
		'source_commit_evidence')!)!
	if !json_equal(projection, policy) {
		return error('candidate manifest differs from the reviewed managed baseline activation policy')
	}
}

// validate_registry_semantics checks the immutable six-target graph independently of I/O.
pub fn validate_registry_semantics(registry JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	known := require_array_member(registry, 'known_bundle_branches')!
	mut actual_known := known.map(require_string(it) or { '' })
	actual_known.sort()
	if actual_known != known_bundle_branches {
		issues << SchemaIssue{'$/known_bundle_branches', 'registry must contain exactly the 16 fixed bundle branches'}
	}
	managed := require_array_member(registry, 'managed_ci_targets')!
	mut actual_ids := []string{}
	mut actual_branches := []string{}
	mut seen_target_rows := []string{}
	mut onboarding_base_shas := []string{}
	mut managed_baseline_shas := []string{}
	mut managed_baseline_trees := []string{}
	mut managed_baseline_manifest_hashes := []string{}
	mut toolchain_profile_ids := []string{}
	for target in managed {
		target_id := require_string_member(target, 'id')!
		actual_ids << target_id
		actual_branches << require_string_member(target, 'branch')!
		binding := legacy_onboarding_binding(target)!
		if binding.base_sha in onboarding_base_shas {
			issues << SchemaIssue{'$/managed_ci_targets', 'legacy onboarding base SHAs must be unique'}
		}
		onboarding_base_shas << binding.base_sha
		activation := managed_baseline_activation_binding(target)!
		if activation.base_sha in managed_baseline_shas {
			issues << SchemaIssue{'$/managed_ci_targets', 'managed baseline activation SHAs must be unique'}
		}
		managed_baseline_shas << activation.base_sha
		if activation.base_tree in managed_baseline_trees {
			issues << SchemaIssue{'$/managed_ci_targets', 'managed baseline activation trees must be unique'}
		}
		managed_baseline_trees << activation.base_tree
		if activation.base_manifest_sha256 in managed_baseline_manifest_hashes {
			issues << SchemaIssue{'$/managed_ci_targets', 'managed baseline activation manifest hashes must be unique'}
		}
		managed_baseline_manifest_hashes << activation.base_manifest_sha256
		if activation.parent_sha != binding.base_sha || activation.base_sha == activation.parent_sha {
			issues << SchemaIssue{'$/managed_ci_targets', 'managed baseline activation parent must equal the distinct legacy onboarding base'}
		}
		if activation.base_contract_repository != 'vlang/v'
			|| activation.base_contract_sha != managed_baseline_contract_sha {
			issues << SchemaIssue{'$/managed_ci_targets', 'managed baseline activation contract binding must equal the reviewed Phase A contract'}
		}
		toolchain_binding := toolchain_profile_binding(target)!
		if toolchain_binding.profile_id != '' {
			if toolchain_binding.profile_id in toolchain_profile_ids {
				issues << SchemaIssue{'$/managed_ci_targets', 'reviewed toolchain profile IDs must be unique'}
			}
			toolchain_profile_ids << toolchain_binding.profile_id
		}
		row := canonical_json(target)
		if row in seen_target_rows {
			issues << SchemaIssue{'$/managed_ci_targets', 'managed target rows must be unique'}
		}
		seen_target_rows << row
		issues << validate_registry_target_tuple(target)!
	}
	actual_ids.sort()
	actual_branches.sort()
	if actual_ids != managed_target_ids {
		issues << SchemaIssue{'$/managed_ci_targets', 'registry must contain exactly the six managed target IDs'}
	}
	expected_managed_branches := managed_target_ids.map('thirdparty-${it}')
	if actual_branches != expected_managed_branches {
		issues << SchemaIssue{'$/managed_ci_targets', 'managed branches must map one-to-one to managed target IDs'}
	}
	acceptances := require_array_member(registry, 'opaque_acceptances')!
	if acceptances.len != 1
		|| require_string_member(acceptances[0], 'id')! != 'windows-amd64-openlibm-v1' {
		issues << SchemaIssue{'$/opaque_acceptances', 'only the reviewed Windows openlibm acceptance is permitted'}
	} else {
		issues << validate_opaque_acceptance_tuple(acceptances[0])!
	}
	return issues
}

// validate_staged_manifest revalidates opaque inputs from a real staging and authoritative Git tree.
pub fn validate_staged_manifest(automation_root string, manifest_path string,
	staging StagingContract) ![]SchemaIssue {
	mut issues := validate_manifest(automation_root, manifest_path)!
	if issues.len > 0 {
		return issues
	}
	manifest_source := os.read_file(manifest_path)!
	manifest := parse_strict_json(manifest_source)!
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	issues << validate_staged_manifest_material(manifest, manifest_source, staging)!
	if issues.len > 0 {
		return issues
	}
	observations := scan_manifest_opaque_inputs(manifest, registry, staging) or {
		issues << SchemaIssue{'$/inventory', err.msg()}
		return issues
	}
	declared_status := require_string_member(manifest, 'provenance_status')!
	derived_status := recalculate_provenance(manifest, registry, observations)!
	if declared_status != derived_status {
		issues << SchemaIssue{
			path:    '$/provenance_status'
			message: 'declared provenance status does not match the observed staging and Git tree'
		}
	}
	return issues
}

// validate_manifest validates a manifest and its semantic Phase A invariants.
pub fn validate_manifest(automation_root string, manifest_path string) ![]SchemaIssue {
	return validate_manifest_source(automation_root, os.read_file(manifest_path)!)!
}

// validate_manifest_source is shared by the path loader and durable replay so neither authority
// can omit a Phase-A schema, registry, profile, source-matrix, or payload invariant.
fn validate_manifest_source(automation_root string, manifest_source string) ![]SchemaIssue {
	manifest := parse_strict_json(manifest_source)!
	schema_path := os.join_path(automation_root, 'schemas', 'bundle-manifest.schema.json')
	mut issues := validate_json_value(schema_path, manifest)!
	if issues.len > 0 {
		return issues
	}
	issues << validate_manifest_value(automation_root, manifest)!
	return issues
}

// validate_manifest_value owns the complete semantic Phase-A contract after strict schema
// validation. All callers reach it through validate_manifest_source.
fn validate_manifest_value(automation_root string, manifest JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	registry_issues := validate_registry(automation_root)!
	if registry_issues.len > 0 {
		return registry_issues
	}
	target_id := require_string_member(manifest, 'target_id')!
	branch := require_string_member(manifest, 'branch')!
	if branch != 'thirdparty-${target_id}' {
		issues << SchemaIssue{'$/branch', 'target and canonical branch do not match'}
	}
	mode := require_string_member(manifest, 'contract_mode')!
	repository := require_string_member(manifest, 'contract_repository')!
	if mode == 'fork-dry-run' && repository != 'GGRei/v' {
		issues << SchemaIssue{'$/contract_repository', 'fork-dry-run must use GGRei/v'}
	}
	if mode == 'production' && repository != 'vlang/v' {
		issues << SchemaIssue{'$/contract_repository', 'production must use vlang/v'}
	}
	patches := require_array_member(manifest, 'patches')!
	if target_id != 'windows-amd64' && patches.len != 0 {
		issues << SchemaIssue{'$/patches', 'the five non-Windows Phase A patchsets must be explicitly empty'}
	}
	mut source_ids := []string{}
	sources := require_array_member(manifest, 'sources')!
	for source in sources {
		source_id := require_string_member(source, 'id')!
		if source_id in source_ids {
			issues << SchemaIssue{'$/sources', 'source IDs must be unique'}
		}
		source_ids << source_id
	}
	issues << validate_manifest_source_matrix(target_id, require_string_member(manifest,
		'v_source_sha')!, sources)!
	transforms := require_array_member(manifest, 'transforms')!
	recipe_path := require_string_member(require_object_member(manifest, 'recipe')!, 'path')!
	recipe_path_key := manifest_path_key(target_id, recipe_path)
	mut orders := []i64{}
	mut input_ids := []string{}
	mut input_paths := [recipe_path_key]
	if manifest_path_is_reserved(target_id, recipe_path) {
		issues << SchemaIssue{'$/recipe/path', 'control input path overlaps a reserved control-plane path'}
	}
	mut patch_ids_in_order := []string{}
	mut previous_order := i64(0)
	for patch in patches {
		patch_id := require_string_member(patch, 'id')!
		patch_path := require_string_member(patch, 'path')!
		patch_path_key := manifest_path_key(target_id, patch_path)
		order := require_integer_member(patch, 'order')!
		if manifest_path_is_reserved(target_id, patch_path) {
			issues << SchemaIssue{'$/patches', 'control input path overlaps a reserved control-plane path'}
		}
		if order in orders || patch_id in input_ids || patch_path_key in input_paths {
			issues << SchemaIssue{
				path:    '$/patches'
				message: 'patch and transform IDs, paths, and order values must be globally unique'
			}
		}
		if order <= previous_order {
			issues << SchemaIssue{
				path:    '$/patches'
				message: 'patches must appear in strictly increasing declared order'
			}
		}
		orders << order
		input_ids << patch_id
		input_paths << patch_path_key
		patch_ids_in_order << patch_id
		previous_order = order
	}
	for transform in transforms {
		transform_id := require_string_member(transform, 'id')!
		transform_path := require_string_member(transform, 'path')!
		transform_path_key := manifest_path_key(target_id, transform_path)
		order := require_integer_member(transform, 'order')!
		if manifest_path_is_reserved(target_id, transform_path) {
			issues << SchemaIssue{'$/transforms', 'control input path overlaps a reserved control-plane path'}
		}
		if order in orders || transform_id in input_ids || transform_path_key in input_paths {
			issues << SchemaIssue{
				path:    '$/transforms'
				message: 'patch and transform IDs, paths, and order values must be globally unique'
			}
		}
		if order <= previous_order {
			issues << SchemaIssue{
				path:    '$/transforms'
				message: 'transforms must follow patches in strictly increasing declared order'
			}
		}
		orders << order
		input_ids << transform_id
		input_paths << transform_path_key
		previous_order = order
	}
	header_effects := require_array_member(manifest, 'header_effects')!
	integrations := require_array_member(manifest, 'integrations')!
	if target_id == 'windows-amd64' {
		expected_patch_ids := [
			'patch-0001',
			'patch-0002',
			'patch-0003',
			'patch-0004',
			'patch-0005',
			'patch-0006',
			'patch-0007',
			'patch-0008',
			'patch-0009',
		]
		if patch_ids_in_order != expected_patch_ids {
			issues << SchemaIssue{'$/patches', 'Windows must declare the nine ordered TinyCC patches'}
		}
		if orders != [i64(1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] {
			issues << SchemaIssue{'$/transforms', 'Windows patch and transform order must be exactly 1 through 11'}
		}
		mut effect_ids := header_effects.map(require_string_member(it, 'id') or { '' })
		effect_ids.sort()
		if effect_ids != ['header-condition-variable', 'header-faststorefence', 'header-gmtime-s'] {
			issues << SchemaIssue{'$/header_effects', 'Windows must keep the three header effects independent'}
		}
		if integrations.len != 1
			|| require_string_member(integrations[0], 'id')! != 'bdwgc-v-integration' {
			issues << SchemaIssue{'$/integrations', 'Windows must declare V/libgc integration separately'}
		}
		issues << validate_windows_transforms(transforms, header_effects, integrations)!
	} else if header_effects.len != 0 || integrations.len != 0 || transforms.len != 0 {
		issues << SchemaIssue{'$', 'non-Windows Phase A effects and integrations must be explicitly empty'}
	}
	probes := require_array_member(manifest, 'probes')!
	mut probe_ids := []string{}
	mut patch_probe_lane_count := -1
	mut materialized_result_count := u64(0)
	for probe in probes {
		probe_id := require_string_member(probe, 'id')!
		if probe_id in probe_ids {
			issues << SchemaIssue{
				path:    '$/probes'
				message: 'probe IDs must be unique'
			}
		}
		probe_ids << probe_id
		if !require_bool_member(probe, 'required')! {
			issues << SchemaIssue{'$/probes', 'every declared Phase A probe must be required'}
		}
		lanes := require_array_member(probe, 'expected_lanes')!
		if probe_id == 'patch-probes' {
			patch_probe_lane_count = lanes.len
		}
		if lanes.len == 0 {
			if probe_id == 'patch-probes' {
				materialized_result_count++
			} else {
				issues << SchemaIssue{
					path:    '$/probes'
					message: 'only an explicitly empty patchset may have zero expected lanes'
				}
			}
		} else {
			materialized_result_count += u64(lanes.len)
		}
		mut lane_ids := []string{}
		for lane in lanes {
			lane_id := require_string(lane)!
			if lane_id in lane_ids {
				issues << SchemaIssue{'$/probes', 'expected lane IDs must be unique per probe'}
			}
			lane_ids << lane_id
		}
	}
	if materialized_result_count > 1024 {
		issues << SchemaIssue{
			path:    '$/probes'
			message: 'manifest materializes more than 1024 native lane results'
		}
	}
	if patch_probe_lane_count >= 0 && (patches.len == 0) != (patch_probe_lane_count == 0) {
		issues << SchemaIssue{
			path:    '$/probes'
			message: 'patch-probes lanes must be empty exactly when the patch list is empty'
		}
	}
	for gate_id in normalized_gate_ids {
		if gate_id !in probe_ids {
			issues << SchemaIssue{
				path:    '$/probes'
				message: 'missing normalized gate ${gate_id}'
			}
		}
	}
	mut effects := []JsonValue{}
	for patch in patches {
		effects << require_array_member(patch, 'effects')!
	}
	effects << header_effects
	effects << integrations
	for effect in effects {
		for required_probe in require_array_member(effect, 'required_probe_ids')! {
			probe_id := require_string(required_probe)!
			if probe_id !in probe_ids {
				issues << SchemaIssue{
					path:    '$/probes'
					message: 'effect requires undeclared probe ${probe_id}'
				}
			}
		}
	}
	mut inventory_entries := require_array_member(manifest, 'inventory')!
	inventory_entries << require_array_member(manifest, 'overlays')!
	inventory_entries << require_array_member(manifest, 'outputs')!
	mut control_input_paths := [
		manifest_path_key(target_id, 'automation/bundle-manifest.json'),
	]
	control_input_paths << input_paths
	mut inventory_paths := []string{}
	for entry in inventory_entries {
		entry_path := require_string_member(entry, 'path')!
		entry_path_key := manifest_path_key(target_id, entry_path)
		if entry_path_key in control_input_paths || manifest_path_is_reserved(target_id, entry_path) {
			issues << SchemaIssue{'$/inventory', 'payload paths cannot overlap control-plane paths'}
		}
		if entry_path_key in inventory_paths {
			issues << SchemaIssue{
				path:    '$/inventory'
				message: 'inventory, overlays, and outputs must not collide'
			}
		}
		inventory_paths << entry_path_key
		kind := require_string_member(entry, 'kind')!
		git_mode := require_string_member(entry, 'git_mode')!
		target := require_member(entry, 'symlink_target')!
		if kind == 'symlink' {
			if git_mode != '120000' || target.kind != .string_value {
				issues << SchemaIssue{'$/inventory', 'symlink must bind mode 120000 and target text'}
			} else if !symlink_target_is_allowed(target_id, entry_path, target.string_value) {
				issues << SchemaIssue{'$/inventory', 'symlink target is not allowed for this target and path'}
			}
		} else if target.kind != .null_value
			|| (kind == 'file' && git_mode != '100644')
			|| (kind == 'executable' && git_mode != '100755') {
			issues << SchemaIssue{'$/inventory', 'regular inventory kind, mode, and target are inconsistent'}
		}
	}
	if require_array_member(manifest, 'outputs')!.len == 0 {
		issues << SchemaIssue{'$/outputs', 'a managed bundle must declare at least one output'}
	}
	registry := parse_strict_json(os.read_file(os.join_path(automation_root, 'targets.json'))!)!
	registry_target := registry_target_by_id(registry, target_id)!
	validate_manifest_producer_toolchain(automation_root, target_id, manifest) or {
		issues << SchemaIssue{'$/toolchain', err.msg()}
	}
	if branch != require_string_member(registry_target, 'branch')! {
		issues << SchemaIssue{'$/branch', 'manifest branch differs from the authoritative registry tuple'}
	}
	affected := require_array_member(manifest, 'affected_targets')!
	if !json_equal(JsonValue{ kind: .array, array_value: affected }, require_member(registry_target,
		'affected_targets')!) {
		issues << SchemaIssue{'$/affected_targets', 'manifest affected_targets differs from the registry graph'}
	}
	declared_status := require_string_member(manifest, 'provenance_status')!
	derived_status := classify_declared_provenance(manifest, registry)!
	if declared_status != derived_status {
		issues << SchemaIssue{
			path:    '$/provenance_status'
			message: 'declared provenance status does not match the recalculated static contract'
		}
	}
	return issues
}

struct ManifestSourceExpectation {
	id         string
	repository string
	ref        string
}

fn validate_manifest_source_matrix(target_id string, v_source_sha string,
	sources []JsonValue) ![]SchemaIssue {
	expected := match target_id {
		'freebsd-amd64', 'linux-amd64' {
			[
				ManifestSourceExpectation{'tinycc', 'https://repo.or.cz/tinycc.git', 'mob'},
				ManifestSourceExpectation{'bdwgc', 'https://github.com/ivmai/bdwgc.git', 'master'},
			]
		}
		'macos-amd64', 'macos-arm64' {
			[
				ManifestSourceExpectation{'tinycc', 'https://repo.or.cz/tinycc.git', 'mob'},
				ManifestSourceExpectation{'bdwgc', 'https://github.com/ivmai/bdwgc.git', 'master'},
				ManifestSourceExpectation{'libatomic_ops', 'https://github.com/bdwgc/libatomic_ops.git', 'master'},
			]
		}
		'openbsd-amd64' {
			[
				ManifestSourceExpectation{'tinycc', 'https://repo.or.cz/tinycc.git', 'mob'},
			]
		}
		'windows-amd64' {
			[
				ManifestSourceExpectation{'tinycc', 'https://repo.or.cz/tinycc.git', 'mob'},
				ManifestSourceExpectation{'v-libgc', 'https://github.com/vlang/v.git', 'master'},
			]
		}
		else {
			return [SchemaIssue{'$/sources', 'target has no closed source matrix'}]
		}
	}
	mut issues := []SchemaIssue{}
	if sources.len != expected.len {
		return [
			SchemaIssue{'$/sources', 'manifest sources differ from the exact target source matrix'},
		]
	}
	for index, expectation in expected {
		source := sources[index]
		if require_string_member(source, 'id')! != expectation.id
			|| require_string_member(source, 'repository')! != expectation.repository
			|| require_string_member(source, 'ref')! != expectation.ref {
			issues << SchemaIssue{'$/sources/${index}', 'source ID, repository, ref, or order differs from the exact target matrix'}
		}
		sha := require_member(source, 'sha')!
		tree := require_member(source, 'tree')!
		if (sha.kind == .null_value) != (tree.kind == .null_value) {
			issues << SchemaIssue{'$/sources/${index}', 'source SHA and tree must be resolved or null as one pair'}
			continue
		}
		if expectation.id == 'v-libgc' && sha.kind == .string_value
			&& sha.string_value != v_source_sha {
			issues << SchemaIssue{'$/sources/${index}/sha', 'Windows v-libgc SHA must equal v_source_sha'}
		}
	}
	return issues
}

fn validate_windows_transforms(transforms []JsonValue, header_effects []JsonValue,
	integrations []JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	if transforms.len != 2 {
		return [
			SchemaIssue{'$/transforms', 'Windows must declare exactly two ordered input transforms'},
		]
	}
	expected_ids := ['vlang-header-compat', 'v-libgc-tinycc-compat']
	expected_paths := ['vlang-header-compat.patch', 'v-ae88ee5-tinycc-bdwgc.patch']
	expected_owners := ['bundle-overlay', 'v-libgc']
	expected_stages := ['bundle-payload-post-copy', 'v-libgc-source-prebuild']
	expected_effects := [
		['header-gmtime-s', 'header-condition-variable', 'header-faststorefence'],
		['bdwgc-v-integration'],
	]
	mut bound_effect_ids := []string{}
	for index, transform in transforms {
		actual_effects := require_array_member(transform, 'effect_ids')!.map(require_string(it) or {
			''
		})
		if require_string_member(transform, 'id')! != expected_ids[index]
			|| require_string_member(transform, 'path')! != expected_paths[index]
			|| require_string_member(transform, 'owner')! != expected_owners[index]
			|| require_integer_member(transform, 'order')! != i64(index + 10)
			|| require_string_member(transform, 'apply_stage')! != expected_stages[index]
			|| actual_effects != expected_effects[index] {
			issues << SchemaIssue{'$/transforms/${index}', 'Windows transform binding differs from the exact reviewed tuple'}
		}
		for effect_id in actual_effects {
			if effect_id in bound_effect_ids {
				issues << SchemaIssue{'$/transforms/${index}/effect_ids', 'transform effect bindings must be globally unique'}
			}
			bound_effect_ids << effect_id
		}
	}
	mut declared_effect_ids := header_effects.map(require_string_member(it, 'id') or { '' })
	declared_effect_ids << integrations.map(require_string_member(it, 'id') or { '' })
	mut sorted_bound := bound_effect_ids.clone()
	mut sorted_declared := declared_effect_ids.clone()
	sorted_bound.sort()
	sorted_declared.sort()
	mut declared_effect_ids_have_duplicates := false
	for index, effect_id in sorted_declared {
		if index > 0 && effect_id == sorted_declared[index - 1] {
			declared_effect_ids_have_duplicates = true
			break
		}
	}
	if sorted_bound != sorted_declared || declared_effect_ids_have_duplicates {
		issues << SchemaIssue{'$/transforms', 'transform effect bindings must form an exact bijection with header effects and integrations'}
	}
	return issues
}

// recalculate_provenance derives the static provenance status before any native lane runs.
pub fn recalculate_provenance(manifest JsonValue, registry JsonValue,
	observations []OpaqueObservation) !string {
	if !manifest_sources_are_resolved(manifest)! || !manifest_toolchain_is_resolved(manifest)! {
		return 'incomplete'
	}
	for output in require_array_member(manifest, 'outputs')! {
		if require_bool_member(output, 'opaque')!
			|| !provenance_is_complete(require_object_member(output, 'provenance')!) {
			return 'incomplete'
		}
	}
	target_id := require_string_member(manifest, 'target_id')!
	mut entries := require_array_member(manifest, 'inventory')!
	entries << require_array_member(manifest, 'overlays')!
	mut opaque_entries := []JsonValue{}
	for entry in entries {
		provenance := require_object_member(entry, 'provenance')!
		if require_bool_member(entry, 'opaque')! {
			opaque_entries << entry
		} else if !provenance_is_complete(provenance) {
			return 'incomplete'
		}
	}
	if opaque_entries.len == 0 {
		return 'complete'
	}
	if opaque_entries.len != 1 || target_id != 'windows-amd64' || observations.len != 1 {
		return 'incomplete'
	}
	acceptances := require_array_member(registry, 'opaque_acceptances')!
	if acceptances.len != 1 {
		return 'incomplete'
	}
	entry := opaque_entries[0]
	acceptance := acceptances[0]
	observation := observations[0]
	if !observation.present || observation.target_id != target_id {
		return 'incomplete'
	}
	checks := [
		require_string_member(entry, 'opaque_acceptance_id')! == require_string_member(acceptance, 'id')!,
		require_string_member(entry, 'path')! == require_string_member(acceptance, 'path')!,
		require_string_member(entry, 'kind')! == require_string_member(acceptance, 'type')!,
		require_string_member(entry, 'git_mode')! == require_string_member(acceptance, 'git_mode')!,
		require_string_member(entry, 'sha256')! == require_string_member(acceptance, 'sha256')!,
		require_nullable_string_member(entry, 'format')! == require_string_member(acceptance,
			'format')!,
		require_nullable_string_member(entry, 'object_type')! == require_string_member(acceptance,
			'object_type')!,
		require_nullable_string_member(entry, 'machine')! == require_string_member(acceptance,
			'machine')!,
		require_nullable_string_member(entry, 'os_abi')! == require_string_member(acceptance,
			'os_abi')!,
		require_string_member(entry, 'role')! == require_string_member(acceptance, 'role')!,
		observation.path == require_string_member(acceptance, 'path')!,
		observation.kind == require_string_member(acceptance, 'type')!,
		observation.git_mode == require_string_member(acceptance, 'git_mode')!,
		observation.sha256 == require_string_member(acceptance, 'sha256')!,
		observation.format == require_string_member(acceptance, 'format')!,
		observation.object_type == require_string_member(acceptance, 'object_type')!,
		observation.machine == require_string_member(acceptance, 'machine')!,
		observation.os_abi == require_string_member(acceptance, 'os_abi')!,
	]
	if checks.any(!it) {
		return 'incomplete'
	}
	probe_ids := require_array_member(manifest, 'probes')!.map(require_string_member(it, 'id') or {
		''
	})
	if require_string_member(acceptance, 'required_probe_id')! !in probe_ids {
		return 'incomplete'
	}
	return 'opaque-accepted'
}

// manifest_projections builds deterministic input and sorted-output projections.
pub fn manifest_projections(manifest_source string, registry JsonValue) !ManifestProjections {
	manifest := parse_strict_json(manifest_source)!
	projection_source_keys := [
		'contract_version',
		'contract_repository',
		'contract_sha',
		'contract_mode',
		'v_source_sha',
		'target_id',
		'branch',
		'sources',
		'recipe',
		'toolchain',
		'patches',
		'transforms',
		'header_effects',
		'integrations',
		'overlays',
		'inventory',
		'probes',
		'provenance_status',
		'affected_targets',
	]
	projection_names := [
		'contract_version',
		'contract_repository',
		'contract_sha',
		'contract_mode',
		'v_source_sha',
		'target_id',
		'branch',
		'sources',
		'recipe',
		'toolchain',
		'patches',
		'transforms',
		'header_effects',
		'integrations',
		'overlays',
		'input_inventory',
		'probes',
		'provenance_status',
		'affected_targets',
	]
	mut projection_values := []JsonValue{}
	for key in projection_source_keys {
		projection_values << require_member(manifest, key)!
	}
	mut input_projection := object_value_from_pairs(projection_names, projection_values)!
	acceptances := require_member(registry, 'opaque_acceptances')!
	input_projection = append_object_members(input_projection, [
		'opaque_policy_hash',
		'opaque_acceptance_ids',
	], [
		JsonValue{ kind: .string_value, string_value: json_sha256(acceptances) },
		JsonValue{
			kind:        .array
			array_value: require_array_member(registry, 'opaque_acceptances')!.map(JsonValue{
				kind:         .string_value
				string_value: require_string_member(it, 'id') or { '' }
			})
		},
	])!
	mut outputs := require_array_member(manifest, 'outputs')!.clone()
	outputs.sort_with_compare(compare_inventory_paths)
	mut digest_lines := []string{}
	mut artifact_entries := []JsonValue{cap: outputs.len}
	for output in outputs {
		path := require_string_member(output, 'path')!
		digest := require_string_member(output, 'sha256')!
		digest_lines << '${path}\t${digest}'
		artifact_entries << select_object_members(output, [
			'path',
			'kind',
			'git_mode',
			'sha256',
			'symlink_target',
		])!
	}
	digest_lines.sort()
	artifact_projection := object_value_from_pairs(['target_id', 'outputs'], [
		require_member(manifest, 'target_id')!,
		JsonValue{ kind: .array, array_value: artifact_entries },
	])!
	return ManifestProjections{
		input:        input_projection
		artifact:     artifact_projection
		digest_lines: digest_lines
	}
}

// manifest_fingerprints computes the raw manifest, semantic input, and output byte hashes.
pub fn manifest_fingerprints(manifest_source string, registry JsonValue) !FingerprintSet {
	projections := manifest_projections(manifest_source, registry)!
	return FingerprintSet{
		manifest_hash:        sha256.sum256(manifest_source.bytes()).hex()
		input_fingerprint:    json_sha256(projections.input)
		artifact_fingerprint: json_sha256(projections.artifact)
		digest_lines:         projections.digest_lines
	}
}

// authenticate_native_lane_matrix_file is the only public T2b1 matrix loader. It authenticates
// canonical, stable file bytes and returns a sealed dormant envelope; no state or publication
// consumer accepts that envelope until the separate T2b2 migration.
pub fn authenticate_native_lane_matrix_file(automation_root string,
	manifest AuthenticatedManifestModel, subject NativeGateSubjectModel,
	matrix_path string) !AuthenticatedNativeLaneMatrix {
	matrix_source := read_stable_toolchain_document(matrix_path, 'native lane matrix')!
	return authenticate_native_lane_matrix_exact_source(automation_root, manifest, subject,
		matrix_source)!
}

fn authenticate_native_lane_matrix_exact_source(automation_root string,
	manifest AuthenticatedManifestModel, subject NativeGateSubjectModel,
	matrix_source string) !AuthenticatedNativeLaneMatrix {
	matrix := parse_strict_json(matrix_source)!
	if matrix_source != canonical_json(matrix) {
		return error('native lane matrix bytes must be exact canonical JSON')
	}
	matrix_issues := validate_json_value(os.join_path(automation_root, 'schemas',
		'native-lane-matrix.schema.json'), matrix)!
	if matrix_issues.len > 0 {
		return error('native lane matrix schema failed with ${matrix_issues.len} issue(s)')
	}
	return authenticate_native_lane_matrix_source(manifest, subject, matrix_source, matrix)!
}

fn authenticate_native_lane_matrix_source(manifest AuthenticatedManifestModel,
	subject NativeGateSubjectModel, matrix_source string,
	matrix JsonValue) !AuthenticatedNativeLaneMatrix {
	validate_authenticated_manifest(manifest)!
	validate_native_gate_subject(subject)!
	expected_subject := native_gate_subject_json(subject)!
	declared_subject := require_object_member(matrix, 'subject')!
	if !json_equal(declared_subject, expected_subject) {
		return error('native lane matrix subject differs from the complete expected subject')
	}
	subject_hash := native_gate_subject_hash(subject)!
	if require_string_member(matrix, 'subject_hash')! != subject_hash {
		return error('native lane matrix subject hash is not derived from its complete subject')
	}
	if subject.target_id != manifest.target_id
		|| subject.manifest_hash != manifest.fingerprints.manifest_hash
		|| subject.input_fingerprint != manifest.fingerprints.input_fingerprint
		|| subject.artifact_fingerprint != manifest.fingerprints.artifact_fingerprint
		|| native_subject_digest_lines(subject) != manifest.fingerprints.digest_lines {
		return error('native lane matrix subject differs from the authenticated manifest')
	}
	producer := producer_toolchain_model(manifest.producer)!
	if !json_equal(require_object_member(matrix, 'producer_toolchain')!,
		producer_toolchain_projection(producer)!) {
		return error('native lane matrix producer differs from the authenticated manifest')
	}
	validator_value := require_object_member(matrix, 'validator_observation')!
	validator := authenticate_toolchain_observation_against_profile(manifest.target_id,
		manifest.toolchain_profile_binding, manifest.toolchain_profile, validator_value)!
	if validator.phase != 'validator' || validator.target_id != manifest.target_id
		|| validator.profile_id != producer.profile_id
		|| validator.profile_sha256 != producer.profile_sha256 {
		return error('native lane matrix validator differs from the sealed target profile')
	}
	selected := require_object_member(matrix, 'selected_run')!
	selected_run := NativeLaneRunKey{
		run_id:         require_integer_member(selected, 'run_id')!
		run_attempt:    int(require_integer_member(selected, 'run_attempt')!)
		check_suite_id: require_integer_member(selected, 'check_suite_id')!
	}
	if selected_run.run_id <= 0 || selected_run.run_attempt !in [1, 2]
		|| selected_run.check_suite_id <= 0 {
		return error('native lane matrix selected run key is invalid')
	}
	results := authenticate_native_lane_results(manifest.manifest, require_array_member(matrix,
		'results')!)!
	return AuthenticatedNativeLaneMatrix{
		raw_source:    matrix_source
		matrix:        matrix
		matrix_sha256: sha256.sum256(matrix_source.bytes()).hex()
		target_id:     manifest.target_id
		subject:       subject
		subject_hash:  subject_hash
		producer:      producer
		validator:     validator
		selected_run:  selected_run
		results:       results
	}
}

fn native_subject_digest_lines(subject NativeGateSubjectModel) []string {
	mut lines := subject.digests.map('${it.path}\t${it.sha256}')
	lines.sort()
	return lines
}

fn manifest_probe_expected_count(manifest JsonValue, probe_id string) !int {
	mut matches := 0
	mut count := 0
	for probe in require_array_member(manifest, 'probes')! {
		if require_string_member(probe, 'id')! == probe_id {
			matches++
			count = require_array_member(probe, 'expected_lanes')!.len
		}
	}
	if matches != 1 {
		return error('native lane matrix probe does not resolve exactly once in the manifest')
	}
	return count
}

fn authenticate_native_lane_results(manifest JsonValue,
	values []JsonValue) ![]NativeLaneResult {
	expected_pairs := required_lane_pairs(manifest)!
	if values.len != expected_pairs.len {
		return error('native lane matrix result count differs from the manifest order')
	}
	target_id := require_string_member(manifest, 'target_id')!
	expected_openlibm := ['x64-fontstash', 'x64-json', 'x64-math', 'x64-stbi', 'x64-vorbis']
	mut manifest_openlibm := []string{}
	for probe in require_array_member(manifest, 'probes')! {
		if require_string_member(probe, 'id')! == 'opaque-openlibm' {
			manifest_openlibm = require_array_member(probe, 'expected_lanes')!.map(require_string(it) or {
				''
			})
		}
	}
	mut sorted_openlibm := manifest_openlibm.clone()
	sorted_openlibm.sort()
	if target_id == 'windows-amd64' && sorted_openlibm != expected_openlibm {
		return error('Windows native lane matrix requires the exact five openlibm consumers')
	}
	if target_id != 'windows-amd64' && manifest_openlibm.len > 0 {
		return error('native lane matrix forbids openlibm outside windows-amd64')
	}
	mut results := []NativeLaneResult{cap: values.len}
	mut observed_pairs := []string{cap: values.len}
	for index, value in values {
		parts := expected_pairs[index].split_nth('/', 2)
		if parts.len != 2 {
			return error('native lane matrix manifest pair is invalid')
		}
		result := NativeLaneResult{
			probe_id:        require_string_member(value, 'probe_id')!
			lane_id:         require_string_member(value, 'lane_id')!
			required:        require_bool_member(value, 'required')!
			status:          require_string_member(value, 'status')!
			expected_count:  int(require_integer_member(value, 'expected_count')!)
			evidence_sha256: require_string_member(value, 'evidence_sha256')!
			fallback_used:   require_bool_member(value, 'fallback_used')!
			object_linked:   require_bool_member(value, 'object_linked')!
			consumer_group:  require_string_member(value, 'consumer_group')!
		}
		observed_pair := '${result.probe_id}/${result.lane_id}'
		if observed_pair in observed_pairs {
			return error('native lane matrix contains a duplicate probe and lane result')
		}
		observed_pairs << observed_pair
		if result.probe_id != parts[0] || result.lane_id != parts[1] {
			return error('native lane matrix results differ from the strict manifest order')
		}
		expected_count := manifest_probe_expected_count(manifest, result.probe_id)!
		if !result.required || result.status !in ['passed', 'failed', 'blocked']
			|| !is_lower_hex_64(result.evidence_sha256) || result.expected_count != expected_count {
			return error('native lane matrix result is not one explicit closed result')
		}
		if result.lane_id == 'expected=0' {
			if result.probe_id != 'patch-probes' || expected_count != 0 || result.status != 'passed'
				|| result.fallback_used || result.object_linked || result.consumer_group != 'none' {
				return error('empty patch-probes requires the exact passed expected=0 result')
			}
		} else if result.probe_id == 'opaque-openlibm' {
			expected_group := result.lane_id.all_after('x64-')
			if target_id != 'windows-amd64' || result.lane_id !in expected_openlibm
				|| result.consumer_group != expected_group
				|| (result.status == 'passed' && !result.fallback_used && !result.object_linked) {
				return error('openlibm matrix result is not bound to its exact linked consumer')
			}
		} else if result.consumer_group != 'none' || result.object_linked {
			return error('non-openlibm matrix result cannot declare an object consumer')
		}
		results << result
	}
	return results
}

// authenticated_native_lane_matrix_digest exposes only the digest of already authenticated,
// exact canonical file bytes. The digest alone is never a verdict or a gate-output proof.
pub fn authenticated_native_lane_matrix_digest(matrix AuthenticatedNativeLaneMatrix) !string {
	if matrix.raw_source == '' || matrix.raw_source != canonical_json(matrix.matrix)
		|| sha256.sum256(matrix.raw_source.bytes()).hex() != matrix.matrix_sha256 {
		return error('authenticated native lane matrix bytes or digest diverged')
	}
	return matrix.matrix_sha256
}

// authenticated_native_lane_matrix_facts is the sole T2b2 verdict bridge. It replays the complete
// raw JCS authentication against the exact manifest and subject instead of trusting cached fields.
fn authenticated_native_lane_matrix_facts(manifest AuthenticatedManifestModel,
	subject NativeGateSubjectModel, matrix AuthenticatedNativeLaneMatrix) !NativeLaneMatrixFacts {
	if matrix.raw_source == '' {
		return error('authenticated native lane matrix is empty')
	}
	reparsed := parse_strict_json(matrix.raw_source)!
	if matrix.raw_source != canonical_json(reparsed) {
		return error('authenticated native lane matrix bytes are no longer canonical')
	}
	recomputed := authenticate_native_lane_matrix_source(manifest, subject, matrix.raw_source,
		reparsed)!
	if recomputed.raw_source != matrix.raw_source || !json_equal(recomputed.matrix, matrix.matrix)
		|| recomputed.matrix_sha256 != matrix.matrix_sha256
		|| recomputed.target_id != matrix.target_id || recomputed.subject != matrix.subject
		|| recomputed.subject_hash != matrix.subject_hash || recomputed.producer != matrix.producer
		|| recomputed.validator.target_id != matrix.validator.target_id
		|| recomputed.validator.profile_id != matrix.validator.profile_id
		|| recomputed.validator.profile_sha256 != matrix.validator.profile_sha256
		|| recomputed.validator.phase != matrix.validator.phase
		|| recomputed.validator.observation_sha256 != matrix.validator.observation_sha256
		|| !json_equal(recomputed.validator.observation, matrix.validator.observation)
		|| recomputed.selected_run != matrix.selected_run || recomputed.results != matrix.results {
		return error('authenticated native lane matrix sealed facts diverged from its raw bytes')
	}
	mut outcome := NativeLaneOutcome.green
	for result in recomputed.results {
		if result.status == 'failed' || result.fallback_used {
			outcome = .functional
		} else if result.status == 'blocked' && outcome == .green {
			outcome = .infrastructure
		}
	}
	return NativeLaneMatrixFacts{
		matrix_digest: recomputed.matrix_sha256
		subject_hash:  recomputed.subject_hash
		selected_run:  recomputed.selected_run
		outcome:       outcome
	}
}

// authenticated_manifest_target_id exposes only the already validated target projection.
pub fn authenticated_manifest_target_id(authenticated AuthenticatedManifestModel) !string {
	validate_authenticated_manifest(authenticated)!
	return authenticated.target_id
}

// authenticated_manifest_fingerprints exposes only projections revalidated from the retained raw
// manifest bytes and authoritative registry.
pub fn authenticated_manifest_fingerprints(authenticated AuthenticatedManifestModel) !FingerprintSet {
	validate_authenticated_manifest(authenticated)!
	return authenticated.fingerprints
}

// required_lane_pairs returns the unique required probe/lane matrix in manifest order.
pub fn required_lane_pairs(manifest JsonValue) ![]string {
	mut expected_pairs := []string{}
	patches := require_array_member(manifest, 'patches')!
	for probe in require_array_member(manifest, 'probes')! {
		probe_id := require_string_member(probe, 'id')!
		if !require_bool_member(probe, 'required')! {
			return error('all manifest probes must be required')
		}
		lanes := require_array_member(probe, 'expected_lanes')!
		if probe_id == 'patch-probes' && (patches.len == 0) != (lanes.len == 0) {
			return error('patch-probes lanes must be empty exactly when the patch list is empty')
		}
		if lanes.len == 0 {
			if probe_id != 'patch-probes' {
				return error('only an explicitly empty patchset may have zero expected lanes')
			}
			if expected_pairs.len >= 1024 {
				return error('manifest materializes more than 1024 native lane results')
			}
			expected_pairs << '${probe_id}/expected=0'
			continue
		}
		for lane in lanes {
			if expected_pairs.len >= 1024 {
				return error('manifest materializes more than 1024 native lane results')
			}
			pair := '${probe_id}/${require_string(lane)!}'
			if pair in expected_pairs {
				return error('manifest declares duplicate required pair ${pair}')
			}
			expected_pairs << pair
		}
	}
	return expected_pairs
}

fn provenance_is_complete(provenance JsonValue) bool {
	status := require_string_member(provenance, 'status') or { return false }
	if status != 'complete' {
		return false
	}
	for key in ['repository', 'sha', 'source_path', 'license'] {
		member := require_member(provenance, key) or { return false }
		if member.kind != .string_value || member.string_value == '' {
			return false
		}
	}
	return true
}

fn classify_declared_provenance(manifest JsonValue, registry JsonValue) !string {
	if !manifest_sources_are_resolved(manifest)! || !manifest_toolchain_is_resolved(manifest)! {
		return 'incomplete'
	}
	for output in require_array_member(manifest, 'outputs')! {
		if require_bool_member(output, 'opaque')!
			|| !provenance_is_complete(require_object_member(output, 'provenance')!) {
			return 'incomplete'
		}
	}
	target_id := require_string_member(manifest, 'target_id')!
	mut entries := require_array_member(manifest, 'inventory')!
	entries << require_array_member(manifest, 'overlays')!
	mut opaque_entries := []JsonValue{}
	for entry in entries {
		if require_bool_member(entry, 'opaque')! {
			opaque_entries << entry
		} else if !provenance_is_complete(require_object_member(entry, 'provenance')!) {
			return 'incomplete'
		}
	}
	if opaque_entries.len == 0 {
		return 'complete'
	}
	if opaque_entries.len != 1 || target_id != 'windows-amd64' {
		return 'incomplete'
	}
	acceptances := require_array_member(registry, 'opaque_acceptances')!
	if acceptances.len != 1 {
		return 'incomplete'
	}
	entry := opaque_entries[0]
	acceptance := acceptances[0]
	checks := [
		require_string_member(entry, 'opaque_acceptance_id')! == require_string_member(acceptance, 'id')!,
		require_string_member(entry, 'path')! == require_string_member(acceptance, 'path')!,
		require_string_member(entry, 'kind')! == require_string_member(acceptance, 'type')!,
		require_string_member(entry, 'git_mode')! == require_string_member(acceptance, 'git_mode')!,
		require_string_member(entry, 'sha256')! == require_string_member(acceptance, 'sha256')!,
		require_nullable_string_member(entry, 'format')! == require_string_member(acceptance,
			'format')!,
		require_nullable_string_member(entry, 'object_type')! == require_string_member(acceptance,
			'object_type')!,
		require_nullable_string_member(entry, 'machine')! == require_string_member(acceptance,
			'machine')!,
		require_nullable_string_member(entry, 'os_abi')! == require_string_member(acceptance,
			'os_abi')!,
		require_string_member(entry, 'role')! == require_string_member(acceptance, 'role')!,
	]
	if checks.any(!it) {
		return 'incomplete'
	}
	probe_ids := require_array_member(manifest, 'probes')!.map(require_string_member(it, 'id') or {
		''
	})
	if require_string_member(acceptance, 'required_probe_id')! !in probe_ids || require_array_member(acceptance, 'architectures')!.map(require_string(it) or {
		''
	}) != ['x64'] {
		return 'incomplete'
	}
	return 'opaque-accepted'
}

fn manifest_sources_are_resolved(manifest JsonValue) !bool {
	for source in require_array_member(manifest, 'sources')! {
		sha := require_member(source, 'sha')!
		tree := require_member(source, 'tree')!
		if sha.kind == .null_value || tree.kind == .null_value {
			return false
		}
		if sha.kind != .string_value || tree.kind != .string_value {
			return error('source SHA and tree must be resolved or null as one pair')
		}
	}
	return true
}

fn manifest_toolchain_is_resolved(manifest JsonValue) !bool {
	profile_id, profile_sha256, producer := manifest_toolchain_members(manifest)!
	return profile_id != '' && profile_sha256 != '' && producer.kind == .object
}

fn target_paths_are_case_insensitive(target_id string) bool {
	return target_id in ['macos-amd64', 'macos-arm64', 'windows-amd64']
}

fn manifest_path_key(target_id string, path string) string {
	return if target_paths_are_case_insensitive(target_id) { path.to_lower() } else { path }
}

fn manifest_path_is_reserved(target_id string, path string) bool {
	key := manifest_path_key(target_id, path)
	return key == 'automation/bundle-manifest.json' || key.starts_with('automation/')
		|| key.starts_with('.github/') || key.split('/').any(it == '.git')
}

fn validate_registry_target_tuple(target JsonValue) ![]SchemaIssue {
	target_id := require_string_member(target, 'id')!
	expected := match target_id {
		'freebsd-amd64' {
			RegistryTargetExpectation{'freebsd', 'elf', 'amd64'}
		}
		'linux-amd64' {
			RegistryTargetExpectation{'linux', 'glibc', 'amd64'}
		}
		'macos-amd64' {
			RegistryTargetExpectation{'macos', 'darwin', 'amd64'}
		}
		'macos-arm64' {
			RegistryTargetExpectation{'macos', 'darwin', 'arm64'}
		}
		'openbsd-amd64' {
			RegistryTargetExpectation{'openbsd', 'elf', 'amd64'}
		}
		'windows-amd64' {
			RegistryTargetExpectation{'windows', 'ucrt-pe', 'amd64'}
		}
		else {
			return [
				SchemaIssue{'$/managed_ci_targets', 'unknown managed target tuple'},
			]
		}
	}
	mut issues := []SchemaIssue{}
	checks := [
		require_string_member(target, 'os')! == expected.os,
		require_string_member(target, 'abi')! == expected.abi,
		require_string_member(target, 'architecture')! == expected.architecture,
		require_string_member(target, 'branch')! == 'thirdparty-${target_id}',
		require_string_member(target, 'native_workflow')! == '.github/workflows/build-and-test.yml',
		require_string_member(target, 'publish_unlock_variable')! == target_unlock_variable(target_id)!,
	]
	binding := legacy_onboarding_binding(target)!
	if binding.policy_path != '' && binding.policy_path != 'onboarding/${target_id}.policy.json' {
		issues << SchemaIssue{'$/managed_ci_targets', 'managed target legacy onboarding policy path is not exact'}
	}
	activation := managed_baseline_activation_binding(target)!
	if activation.policy_path != ''
		&& activation.policy_path != 'baseline-activation/${target_id}.policy.json' {
		issues << SchemaIssue{'$/managed_ci_targets', 'managed target baseline activation policy path is not exact'}
	}
	toolchain_binding := toolchain_profile_binding(target)!
	if toolchain_binding.profile_path != ''
		&& toolchain_binding.profile_path != 'toolchain-profiles/${target_id}.profile.json' {
		issues << SchemaIssue{'$/managed_ci_targets', 'managed target toolchain profile path is not exact'}
	}
	if checks.any(!it) {
		issues << SchemaIssue{'$/managed_ci_targets', 'managed target OS/ABI/architecture/workflow/unlock tuple is not exact'}
	}
	affected := require_array_member(target, 'affected_targets')!.map(require_string(it) or { '' })
	if affected != [target_id] {
		issues << SchemaIssue{'$/managed_ci_targets', 'managed target affected_targets must be its exact closed singleton'}
	}
	return issues
}

struct RegistryTargetExpectation {
	os           string
	abi          string
	architecture string
}

fn validate_opaque_acceptance_tuple(acceptance JsonValue) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	checks := [
		require_string_member(acceptance, 'id')! == 'windows-amd64-openlibm-v1',
		require_string_member(acceptance, 'target_id')! == 'windows-amd64',
		require_string_member(acceptance, 'path')! == 'lib/openlibm.o',
		require_string_member(acceptance, 'type')! == 'file',
		require_string_member(acceptance, 'git_mode')! == '100644',
		require_string_member(acceptance, 'sha256')! == '9a11e182e1f6b522030d1b8685666147de0ebb562b9d02cce189690fd07cb7db',
		require_string_member(acceptance, 'format')! == 'ELF64 little-endian',
		require_string_member(acceptance, 'object_type')! == 'ET_REL',
		require_string_member(acceptance, 'machine')! == 'EM_X86_64',
		require_string_member(acceptance, 'os_abi')! == 'System V',
		require_string_member(acceptance, 'role')! == 'legacy-math-runtime',
		require_string_member(acceptance, 'required_probe_id')! == 'opaque-openlibm',
	]
	architectures := require_array_member(acceptance, 'architectures')!.map(require_string(it) or {
		''
	})
	if checks.any(!it) || architectures != ['x64'] {
		issues << SchemaIssue{
			path:    '$/opaque_acceptances'
			message: 'the sole reviewed opaque acceptance tuple must remain byte-for-byte semantic-equivalent'
		}
	}
	return issues
}

fn registry_target_by_id(registry JsonValue, target_id string) !JsonValue {
	mut matches := []JsonValue{}
	for target in require_array_member(registry, 'managed_ci_targets')! {
		if require_string_member(target, 'id')! == target_id {
			matches << target
		}
	}
	if matches.len != 1 {
		return error('target must resolve to exactly one authoritative registry row')
	}
	return matches[0]
}

fn compare_inventory_paths(left &JsonValue, right &JsonValue) int {
	left_path := require_string_member(left, 'path') or { return -1 }
	right_path := require_string_member(right, 'path') or { return 1 }
	if left_path < right_path {
		return -1
	}
	if left_path > right_path {
		return 1
	}
	return 0
}

// sha256_file returns the lowercase SHA-256 for a bounded local file.
pub fn sha256_file(path string) !string {
	return sha256.sum256(os.read_bytes(path)!).hex()
}

fn require_member(value JsonValue, key string) !JsonValue {
	if value.kind != .object {
		return error('expected object while reading ${key}')
	}
	return value.object_value(key) or { return error('missing object member ${key}') }
}

fn require_object_member(value JsonValue, key string) !JsonValue {
	member := require_member(value, key)!
	if member.kind != .object {
		return error('${key} must be an object')
	}
	return member
}

fn require_array_member(value JsonValue, key string) ![]JsonValue {
	member := require_member(value, key)!
	if member.kind != .array {
		return error('${key} must be an array')
	}
	return member.array_value
}

fn require_string_member(value JsonValue, key string) !string {
	return require_string(require_member(value, key)!)
}

fn require_nullable_string_member(value JsonValue, key string) !string {
	member := require_member(value, key)!
	if member.kind == .null_value {
		return ''
	}
	return require_string(member)
}

fn require_string(value JsonValue) !string {
	if value.kind != .string_value {
		return error('expected string')
	}
	return value.string_value
}

fn require_bool_member(value JsonValue, key string) !bool {
	member := require_member(value, key)!
	if member.kind != .boolean {
		return error('${key} must be boolean')
	}
	return member.bool_value
}

fn require_integer_member(value JsonValue, key string) !i64 {
	member := require_member(value, key)!
	if member.kind != .integer {
		return error('${key} must be integer')
	}
	return member.int_value
}

fn select_object_members(value JsonValue, keys []string) !JsonValue {
	mut values := []JsonValue{}
	for key in keys {
		values << require_member(value, key)!
	}
	return object_value_from_pairs(keys, values)
}

fn append_object_members(value JsonValue, keys []string, values []JsonValue) !JsonValue {
	if value.kind != .object {
		return error('cannot append members to a non-object')
	}
	mut result_keys := value.object_keys.clone()
	mut result_values := value.object_values.clone()
	result_keys << keys
	result_values << values
	return object_value_from_pairs(result_keys, result_values)
}

fn object_value_from_pairs(keys []string, values []JsonValue) !JsonValue {
	if keys.len != values.len {
		return error('object key/value length mismatch')
	}
	mut seen := []string{}
	for key in keys {
		if key in seen {
			return error('duplicate constructed object key ${key}')
		}
		seen << key
	}
	return JsonValue{
		kind:          .object
		object_keys:   keys.clone()
		object_values: values.clone()
	}
}
