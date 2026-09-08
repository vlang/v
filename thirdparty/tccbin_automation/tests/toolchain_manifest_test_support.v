module tests

import crypto.sha256
import encoding.base64
import os
import tccbin_automation.bin

struct ManagedBaselineEvidenceFixture {
	manifest_source string
	evidence        bin.JsonValue
}

fn managed_baseline_string(value string) bin.JsonValue {
	return bin.JsonValue{
		kind:         .string_value
		string_value: value
	}
}

fn managed_baseline_replace_member(value bin.JsonValue, key string,
	replacement bin.JsonValue) bin.JsonValue {
	mut values := value.object_values.clone()
	index := value.object_keys.index(key)
	assert index >= 0, key
	values[index] = replacement
	return bin.JsonValue{
		kind:          .object
		object_keys:   value.object_keys.clone()
		object_values: values
	}
}

fn managed_baseline_resolve_manifest_source(source string, source_id string, sha string,
	tree string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	sources_value := root.object_value('sources') or { panic('sources missing') }
	mut sources := sources_value.array_value.clone()
	mut old_sha := ''
	mut found := false
	for index, candidate_source in sources {
		id := candidate_source.object_value('id') or { panic('source ID missing') }
		if id.string_value == source_id {
			old_sha = (candidate_source.object_value('sha') or { panic('source SHA missing') }).string_value
			sources[index] = managed_baseline_replace_member(managed_baseline_replace_member(candidate_source,
				'sha', managed_baseline_string(sha)), 'tree', managed_baseline_string(tree))
			found = true
		}
	}
	assert found
	mut updated := managed_baseline_replace_member(root, 'sources', bin.JsonValue{
		kind:        .array
		array_value: sources
	})
	for collection in ['overlays', 'inventory', 'outputs'] {
		collection_value := updated.object_value(collection) or { panic('${collection} missing') }
		mut entries := collection_value.array_value.clone()
		for index, entry in entries {
			provenance := entry.object_value('provenance') or { panic('provenance missing') }
			provenance_sha := provenance.object_value('sha') or { panic('provenance SHA missing') }
			if provenance_sha.kind == .string_value && provenance_sha.string_value == old_sha {
				entries[index] = managed_baseline_replace_member(entry, 'provenance', managed_baseline_replace_member(provenance,
					'sha', managed_baseline_string(sha)))
			}
		}
		updated = managed_baseline_replace_member(updated, collection, bin.JsonValue{
			kind:        .array
			array_value: entries
		})
	}
	return bin.canonical_json(updated)
}

fn managed_baseline_evidence_fixture(manifest_source string) ManagedBaselineEvidenceFixture {
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	sources := (manifest.object_value('sources') or { panic('sources missing') }).array_value
	evidence_root := os.join_path(os.temp_dir(), 'tccbin-reviewed-source-evidence-${os.getpid()}')
	os.rmdir_all(evidence_root) or {}
	os.mkdir_all(evidence_root) or { panic(err) }
	defer {
		os.rmdir_all(evidence_root) or {}
	}
	mut resolved_source := bin.canonical_json(manifest)
	mut entries := []bin.JsonValue{cap: sources.len}
	for source in sources {
		id := (source.object_value('id') or { panic('source ID missing') }).string_value
		repository := source.object_value('repository') or { panic('source repository missing') }
		reference := source.object_value('ref') or { panic('source ref missing') }
		if id == 'v-libgc' {
			entries << bin.JsonValue{
				kind:          .object
				object_keys:   ['id', 'repository', 'ref', 'authority']
				object_values: [source.object_value('id') or { panic('source ID missing') },
					repository, reference, bin.JsonValue{
						kind:         .string_value
						string_value: 'runtime-contract'
					}]
			}
			continue
		}
		source_root := os.join_path(evidence_root, id)
		os.mkdir_all(source_root) or { panic(err) }
		os.write_file(os.join_path(source_root, 'reviewed-source.txt'), '${id}\n') or { panic(err) }
		for args in [
			['git', '-C', source_root, 'init', '-q', '--object-format=sha1'],
			['git', '-C', source_root, 'config', 'user.email', 'source@example.invalid'],
			['git', '-C', source_root, 'config', 'user.name', 'Reviewed Source'],
			['git', '-C', source_root, 'add', '--all'],
			['git', '-C', source_root, '-c', 'commit.gpgsign=false', 'commit', '-qm',
				'reviewed ${id} source'],
		] {
			result := os.exec(args)
			assert result.exit_code == 0, result.output
		}
		sha_result := os.exec(['git', '-C', source_root, 'rev-parse', 'HEAD'])
		tree_result := os.exec(['git', '-C', source_root, 'rev-parse', 'HEAD^{tree}'])
		assert sha_result.exit_code == 0, sha_result.output
		assert tree_result.exit_code == 0, tree_result.output
		sha := sha_result.output.trim_space()
		tree := tree_result.output.trim_space()
		raw_result := os.exec(['git', '-C', source_root, 'cat-file', 'commit', sha])
		assert raw_result.exit_code == 0, raw_result.output
		raw := raw_result.output.bytes()
		resolved_source = managed_baseline_resolve_manifest_source(resolved_source, id, sha, tree)
		entries << bin.JsonValue{
			kind:          .object
			object_keys:   ['id', 'repository', 'ref', 'authority', 'sha', 'tree',
				'raw_commit_base64']
			object_values: [source.object_value('id') or { panic('source ID missing') },
				repository, reference, bin.JsonValue{
					kind:         .string_value
					string_value: 'source-commit-object'
				}, bin.JsonValue{
					kind:         .string_value
					string_value: sha
				}, managed_baseline_string(tree), bin.JsonValue{
					kind:         .string_value
					string_value: base64.encode(raw)
				}]
		}
	}
	return ManagedBaselineEvidenceFixture{
		manifest_source: resolved_source
		evidence:        bin.JsonValue{
			kind:        .array
			array_value: entries
		}
	}
}

struct SyntheticToolchainAuthority {
	root            string
	contract_root   string
	target_id       string
	profile_id      string
	profile_sha256  string
	producer_source string
	producer_sha256 string
	producer_digest string
}

fn t2a_toolchain_strategies(target_id string) []string {
	if target_id in ['freebsd-amd64', 'openbsd-amd64'] {
		return ['cpa-guest', 'cpa-host']
	}
	if target_id == 'windows-amd64' {
		return ['github-hosted-msys2']
	}
	return ['github-hosted']
}

fn t2a_role_id(phase string, strategy string, role_count int) string {
	base := if phase == 'producer' { 'bundle-builder' } else { 'contract-validator' }
	if role_count == 1 {
		return base
	}
	return '${base}-${if strategy == 'cpa-guest' {
		'guest'
	} else {
		'host'
	}}'
}

fn t2a_policy_facts(target_id string, strategy string) string {
	if strategy == 'cpa-guest' {
		guest_os := if target_id == 'freebsd-amd64' { 'freebsd' } else { 'openbsd' }
		release := if target_id == 'freebsd-amd64' { '15.1' } else { '7.8' }
		return '[{"name":"arch","match":"exact","value":"amd64"},{"name":"compiler_binary_sha256","match":"sha256"},{"name":"compiler_command","match":"exact","value":"clang"},{"name":"compiler_family","match":"exact","value":"clang"},{"name":"compiler_target","match":"present"},{"name":"compiler_version","match":"present"},{"name":"guest_os","match":"exact","value":"${guest_os}"},{"name":"observed_release","match":"release-compatible","value":"${release}"},{"name":"requested_release","match":"exact","value":"${release}"}]'
	}
	if strategy == 'github-hosted-msys2' {
		return '[{"name":"arch","match":"exact","value":"amd64"},{"name":"compiler_binary_sha256","match":"sha256"},{"name":"compiler_command","match":"exact","value":"gcc"},{"name":"compiler_family","match":"exact","value":"gcc"},{"name":"compiler_package","match":"exact","value":"mingw-w64-ucrt-x86_64-gcc"},{"name":"compiler_target","match":"present"},{"name":"compiler_version","match":"present"},{"name":"image_os","match":"present"},{"name":"image_version","match":"present"},{"name":"msystem","match":"exact","value":"UCRT64"},{"name":"os","match":"exact","value":"windows"},{"name":"package_version","match":"present"},{"name":"runner_label","match":"exact","value":"windows-2022"},{"name":"setup_action_sha","match":"exact","value":"${'b'.repeat(40)}"}]'
	}
	mut os_name := 'linux'
	mut runner_label := 'ubuntu-24.04'
	mut arch := 'amd64'
	mut compiler := 'clang'
	if target_id == 'linux-amd64' {
		compiler = 'gcc'
	} else if target_id == 'macos-amd64' {
		os_name = 'macos'
		runner_label = 'macos-15-intel'
	} else if target_id == 'macos-arm64' {
		os_name = 'macos'
		runner_label = 'macos-15'
		arch = 'arm64'
	}
	prefix := if strategy == 'cpa-host' {
		'{"name":"action_sha","match":"exact","value":"${'a'.repeat(40)}"},'
	} else {
		''
	}
	return '[${prefix}{"name":"arch","match":"exact","value":"${arch}"},{"name":"compiler_binary_sha256","match":"sha256"},{"name":"compiler_command","match":"exact","value":"${compiler}"},{"name":"compiler_family","match":"exact","value":"${compiler}"},{"name":"compiler_target","match":"present"},{"name":"compiler_version","match":"present"},{"name":"image_os","match":"present"},{"name":"image_version","match":"present"},{"name":"os","match":"exact","value":"${os_name}"},{"name":"runner_label","match":"exact","value":"${runner_label}"}]'
}

fn t2a_resolved_facts(target_id string, strategy string) string {
	if strategy == 'cpa-guest' {
		guest_os := if target_id == 'freebsd-amd64' { 'freebsd' } else { 'openbsd' }
		release := if target_id == 'freebsd-amd64' { '15.1' } else { '7.8' }
		compiler_target := if target_id == 'freebsd-amd64' {
			'x86_64-unknown-freebsd15.1'
		} else {
			'x86_64-unknown-openbsd7.8'
		}
		return '[{"name":"arch","value":"amd64"},{"name":"compiler_binary_sha256","value":"${'3'.repeat(64)}"},{"name":"compiler_command","value":"clang"},{"name":"compiler_family","value":"clang"},{"name":"compiler_target","value":"${compiler_target}"},{"name":"compiler_version","value":"clang 19.1.7"},{"name":"guest_os","value":"${guest_os}"},{"name":"observed_release","value":"${release}-RELEASE-p2"},{"name":"requested_release","value":"${release}"}]'
	}
	if strategy == 'github-hosted-msys2' {
		return '[{"name":"arch","value":"amd64"},{"name":"compiler_binary_sha256","value":"${'4'.repeat(64)}"},{"name":"compiler_command","value":"gcc"},{"name":"compiler_family","value":"gcc"},{"name":"compiler_package","value":"mingw-w64-ucrt-x86_64-gcc"},{"name":"compiler_target","value":"x86_64-w64-mingw32"},{"name":"compiler_version","value":"gcc 15.1.0"},{"name":"image_os","value":"Windows Server 2022"},{"name":"image_version","value":"20260801.1"},{"name":"msystem","value":"UCRT64"},{"name":"os","value":"windows"},{"name":"package_version","value":"15.1.0-2"},{"name":"runner_label","value":"windows-2022"},{"name":"setup_action_sha","value":"${'b'.repeat(40)}"}]'
	}
	mut os_name := 'linux'
	mut image_os := 'ubuntu24'
	mut runner_label := 'ubuntu-24.04'
	mut arch := 'amd64'
	mut compiler := 'clang'
	mut compiler_target := 'x86_64-unknown-linux-gnu'
	if target_id == 'linux-amd64' {
		compiler = 'gcc'
	} else if target_id == 'macos-amd64' {
		os_name = 'macos'
		image_os = 'macOS'
		runner_label = 'macos-15-intel'
		compiler_target = 'x86_64-apple-darwin24'
	} else if target_id == 'macos-arm64' {
		os_name = 'macos'
		image_os = 'macOS'
		runner_label = 'macos-15'
		arch = 'arm64'
		compiler_target = 'arm64-apple-darwin24'
	}
	prefix := if strategy == 'cpa-host' {
		'{"name":"action_sha","value":"${'a'.repeat(40)}"},'
	} else {
		''
	}
	return '[${prefix}{"name":"arch","value":"${arch}"},{"name":"compiler_binary_sha256","value":"${'5'.repeat(64)}"},{"name":"compiler_command","value":"${compiler}"},{"name":"compiler_family","value":"${compiler}"},{"name":"compiler_target","value":"${compiler_target}"},{"name":"compiler_version","value":"${compiler} 19.1.7"},{"name":"image_os","value":"${image_os}"},{"name":"image_version","value":"20260801.1"},{"name":"os","value":"${os_name}"},{"name":"runner_label","value":"${runner_label}"}]'
}

fn t2a_profile_source_with_id(target_id string, profile_id string) string {
	strategies := t2a_toolchain_strategies(target_id)
	mut phases := map[string]string{}
	for phase in ['producer', 'validator'] {
		mut roles := []string{}
		for strategy in strategies {
			roles << '{"role_id":"${t2a_role_id(phase, strategy, strategies.len)}","identity_strategy":"${strategy}","identity_policy":${t2a_policy_facts(target_id,
				strategy)}}'
		}
		phases[phase] = roles.join(',')
	}
	profile := bin.parse_strict_json('{"schema_version":1,"profile_id":"${profile_id}","target_id":"${target_id}","producer":[${phases['producer']}],"validator":[${phases['validator']}]}') or {
		panic(err)
	}
	return bin.canonical_json(profile)
}

fn t2a_profile_source(target_id string) string {
	return t2a_profile_source_with_id(target_id, '${target_id}-synthetic-v1')
}

fn t2c_toolchain_evidence_source(phase string, role_id string) string {
	assert phase in ['producer', 'validator']
	return 'tccbin-toolchain-evidence-v1\nphase=${phase}\nrole=${role_id}\n'
}

fn t2c_lane_evidence_source(probe_id string, lane_id string) string {
	return '${probe_id}/${lane_id}\n'
}

fn t2b_toolchain_observation_source_with_profile(target_id string, profile_id string,
	profile_sha256 string, phase string) string {
	assert phase in ['producer', 'validator']
	strategies := t2a_toolchain_strategies(target_id)
	mut roles := []string{}
	for index, strategy in strategies {
		placeholder := if index == 0 { '8'.repeat(64) } else { '9'.repeat(64) }
		role_id := t2a_role_id(phase, strategy, strategies.len)
		evidence_sha256 :=
			sha256.sum256(t2c_toolchain_evidence_source(phase, role_id).bytes()).hex()
		roles << '{"role_id":"${role_id}","identity_strategy":"${strategy}","resolved_identity":${t2a_resolved_facts(target_id,
			strategy)},"resolution_digest":"${placeholder}","evidence_sha256":"${evidence_sha256}"}'
	}
	observation_placeholder := '0'.repeat(64)
	mut source := bin.canonical_json(bin.parse_strict_json('{"schema_version":1,"target_id":"${target_id}","profile_id":"${profile_id}","profile_sha256":"${profile_sha256}","phase":"${phase}","roles":[${roles.join(',')}],"observation_digest":"${observation_placeholder}"}') or {
		panic(err)
	})
	observation := bin.parse_strict_json(source) or { panic(err) }
	observation_roles := observation.object_value('roles') or { panic('roles missing') }
	for role in observation_roles.array_value {
		placeholder := (role.object_value('resolution_digest') or {
			panic('resolution digest missing')
		}).string_value
		derived := bin.toolchain_role_resolution_digest(observation, role) or { panic(err) }
		source = source.replace_once('"resolution_digest":"${placeholder}"',
			'"resolution_digest":"${derived}"')
	}
	with_resolution := bin.parse_strict_json(source) or { panic(err) }
	observation_digest := bin.toolchain_observation_digest(with_resolution) or { panic(err) }
	return source.replace_once('"observation_digest":"${observation_placeholder}"',
		'"observation_digest":"${observation_digest}"')
}

fn t2a_producer_observation_source_with_profile(target_id string, profile_id string,
	profile_sha256 string) string {
	return t2b_toolchain_observation_source_with_profile(target_id, profile_id, profile_sha256,
		'producer')
}

fn t2a_producer_observation_source(target_id string, profile_sha256 string) string {
	return t2a_producer_observation_source_with_profile(target_id, '${target_id}-synthetic-v1',
		profile_sha256)
}

fn t2b_validator_observation_source(authority SyntheticToolchainAuthority) string {
	return t2b_toolchain_observation_source_with_profile(authority.target_id, authority.profile_id,
		authority.profile_sha256, 'validator')
}

fn t2b_native_subject_value(subject bin.NativeGateSubjectModel) bin.JsonValue {
	digest_source := subject.digests.map('{"path":"${it.path}","sha256":"${it.sha256}"}').join(',')
	trigger := subject.remediation_trigger
	trigger_is_empty := trigger.repository == '' && trigger.ref == '' && trigger.before == ''
		&& trigger.after == '' && trigger.tree == '' && trigger.diff_fingerprint == ''
		&& trigger.owner_domain == ''
	trigger_source := if trigger_is_empty {
		'null'
	} else {
		'{"repository":"${trigger.repository}","ref":"${trigger.ref}","before":"${trigger.before}","after":"${trigger.after}","tree":"${trigger.tree}","diff_fingerprint":"${trigger.diff_fingerprint}","owner_domain":"${trigger.owner_domain}"}'
	}
	return bin.parse_strict_json('{"consumer_id":"${subject.consumer_id}","consumer_kind":"${subject.consumer_kind}","intent_or_operation_id":"${subject.intent_or_operation_id}","target_id":"${subject.target_id}","subject_generation":${subject.subject_generation},"initial_run_mode":"${subject.initial_run_mode}","remediation_trigger":${trigger_source},"sha":"${subject.sha}","tree":"${subject.tree}","original_ref":"${subject.original_ref}","input_fingerprint":"${subject.input_fingerprint}","artifact_fingerprint":"${subject.artifact_fingerprint}","manifest_hash":"${subject.manifest_hash}","digests":[${digest_source}]}') or {
		panic(err)
	}
}

fn t2b_native_result_values(manifest bin.JsonValue) []bin.JsonValue {
	mut values := []bin.JsonValue{}
	probes := manifest.object_value('probes') or { panic('probes missing') }
	for probe in probes.array_value {
		probe_id := (probe.object_value('id') or { panic('probe ID missing') }).string_value
		lanes := (probe.object_value('expected_lanes') or { panic('lanes missing') }).array_value
		lane_ids := if lanes.len == 0 {
			['expected=0']
		} else {
			lanes.map(it.string_value)
		}
		for lane_id in lane_ids {
			is_openlibm := probe_id == 'opaque-openlibm'
			consumer_group := if is_openlibm { lane_id.all_after('x64-') } else { 'none' }
			evidence := sha256.sum256(t2c_lane_evidence_source(probe_id, lane_id).bytes()).hex()
			values << bin.parse_strict_json('{"probe_id":"${probe_id}","lane_id":"${lane_id}","required":true,"status":"passed","expected_count":${lanes.len},"evidence_sha256":"${evidence}","fallback_used":false,"object_linked":${is_openlibm},"consumer_group":"${consumer_group}"}') or {
				panic(err)
			}
		}
	}
	return values
}

fn t2b_native_matrix_source_for_run(manifest_source string,
	authority SyntheticToolchainAuthority, subject bin.NativeGateSubjectModel, run_id i64,
	run_attempt int, check_suite_id i64) string {
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	subject_value := t2b_native_subject_value(subject)
	validator := t2b_validator_observation_source(authority)
	results := t2b_native_result_values(manifest).map(bin.canonical_json(it)).join(',')
	matrix := bin.parse_strict_json('{"schema_version":1,"subject":${bin.canonical_json(subject_value)},"subject_hash":"${bin.native_gate_subject_hash(subject) or {
		panic(err)
	}}","producer_toolchain":{"profile_id":"${authority.profile_id}","profile_sha256":"${authority.profile_sha256}","observation_sha256":"${authority.producer_sha256}","observation_digest":"${authority.producer_digest}"},"selected_run":{"run_id":${run_id},"run_attempt":${run_attempt},"check_suite_id":${check_suite_id}},"validator_observation":${validator},"results":[${results}]}') or {
		panic(err)
	}
	return bin.canonical_json(matrix)
}

fn t2b_native_matrix_source(manifest_source string, authority SyntheticToolchainAuthority,
	subject bin.NativeGateSubjectModel) string {
	return t2b_native_matrix_source_for_run(manifest_source, authority, subject, 7001, 1, 7101)
}

fn t2c_native_validation_evidence_sources(matrix_source string,
	authority SyntheticToolchainAuthority) map[string]string {
	matrix := bin.parse_strict_json(matrix_source) or { panic(err) }
	validator := matrix.object_value('validator_observation') or {
		panic('validator observation missing')
	}
	producer := bin.parse_strict_json(authority.producer_source) or { panic(err) }
	mut sources := map[string]string{}
	for observation in [producer, validator] {
		phase := (observation.object_value('phase') or { panic('observation phase missing') }).string_value
		roles := observation.object_value('roles') or { panic('observation roles missing') }
		for role in roles.array_value {
			role_id := (role.object_value('role_id') or { panic('role ID missing') }).string_value
			digest := (role.object_value('evidence_sha256') or { panic('role evidence missing') }).string_value
			source := t2c_toolchain_evidence_source(phase, role_id)
			assert sha256.sum256(source.bytes()).hex() == digest
			if digest in sources {
				assert sources[digest] == source
			} else {
				sources[digest] = source
			}
		}
	}
	results := matrix.object_value('results') or { panic('matrix results missing') }
	for result in results.array_value {
		probe_id := (result.object_value('probe_id') or { panic('probe ID missing') }).string_value
		lane_id := (result.object_value('lane_id') or { panic('lane ID missing') }).string_value
		digest := (result.object_value('evidence_sha256') or { panic('lane evidence missing') }).string_value
		source := t2c_lane_evidence_source(probe_id, lane_id)
		if sha256.sum256(source.bytes()).hex() == digest {
			sources[digest] = source
		} else {
			assert digest in sources
		}
	}
	return sources
}

struct T2cMatrixEvidenceSources {
	matrix_source string
	sources       map[string]string
}

fn t2c_manifest_with_materialized_result_count(source string, total int) string {
	assert total >= 9 && total <= 1025
	canonical_source := bin.canonical_json(bin.parse_strict_json(source) or { panic(err) })
	root := bin.parse_strict_json(canonical_source) or { panic(err) }
	probes_value := root.object_value('probes') or { panic('probes missing') }
	mut probes := probes_value.array_value.clone()
	assert probes.len == 9
	mut fixed_results := 0
	for index, probe in probes {
		if index == 0 {
			continue
		}
		lanes := probe.object_value('expected_lanes') or { panic('expected lanes missing') }
		probe_id := probe.object_value('id') or { panic('probe ID missing') }
		fixed_results += if lanes.array_value.len == 0 && probe_id.string_value == 'patch-probes' {
			1
		} else {
			lanes.array_value.len
		}
	}
	first_count := total - fixed_results
	assert first_count > 0 && first_count <= 1024
	mut lanes := []bin.JsonValue{cap: first_count}
	for index in 0 .. first_count {
		lanes << bin.parse_strict_json('"lane-${index:04}"') or { panic(err) }
	}
	first := probes[0]
	first_source := bin.canonical_json(first)
	lanes_value := bin.JsonValue{
		kind:        .array
		array_value: lanes
	}
	probes[0] = bin.parse_strict_json(replace_canonical_root_member(first_source, first,
		'expected_lanes', bin.canonical_json(lanes_value))) or { panic(err) }
	updated_probes := bin.JsonValue{
		kind:        .array
		array_value: probes
	}
	return replace_canonical_root_member(canonical_source, root, 'probes',
		bin.canonical_json(updated_probes))
}

fn t2c_manifest_for_authority(source string, authority SyntheticToolchainAuthority,
	total int) string {
	mut result := bin.canonical_json(bin.parse_strict_json(source) or { panic(err) })
	for key, replacement in {
		'target_id':        '"${authority.target_id}"'
		'branch':           '"thirdparty-${authority.target_id}"'
		'affected_targets': '["${authority.target_id}"]'
		'toolchain':        '{"producer_observation":${authority.producer_source},"profile_id":"${authority.profile_id}","profile_sha256":"${authority.profile_sha256}"}'
	} {
		root := bin.parse_strict_json(result) or { panic(err) }
		result = replace_canonical_root_member(result, root, key, replacement)
	}
	return t2c_manifest_with_materialized_result_count(result, total)
}

fn t2c_toolchain_evidence_sources(matrix_source string,
	authority SyntheticToolchainAuthority) map[string]string {
	matrix := bin.parse_strict_json(matrix_source) or { panic(err) }
	validator := matrix.object_value('validator_observation') or {
		panic('validator observation missing')
	}
	producer := bin.parse_strict_json(authority.producer_source) or { panic(err) }
	mut sources := map[string]string{}
	for observation in [producer, validator] {
		phase := (observation.object_value('phase') or { panic('observation phase missing') }).string_value
		roles := observation.object_value('roles') or { panic('observation roles missing') }
		for role in roles.array_value {
			role_id := (role.object_value('role_id') or { panic('role ID missing') }).string_value
			digest := (role.object_value('evidence_sha256') or { panic('role evidence missing') }).string_value
			source := t2c_toolchain_evidence_source(phase, role_id)
			assert sha256.sum256(source.bytes()).hex() == digest
			sources[digest] = source
		}
	}
	return sources
}

fn t2c_sized_lane_evidence(index int, size int) string {
	prefix := 'lane-evidence-${index:04}\n'
	assert size >= prefix.len
	return '${prefix}${'x'.repeat(size - prefix.len)}'
}

fn t2c_matrix_with_evidence_sizes(matrix_source string,
	authority SyntheticToolchainAuthority, sizes []int) T2cMatrixEvidenceSources {
	root := bin.parse_strict_json(matrix_source) or { panic(err) }
	results_value := root.object_value('results') or { panic('matrix results missing') }
	mut results := results_value.array_value.clone()
	assert results.len == sizes.len
	mut sources := t2c_toolchain_evidence_sources(matrix_source, authority)
	for index, result in results {
		source := t2c_sized_lane_evidence(index, sizes[index])
		digest := sha256.sum256(source.bytes()).hex()
		result_source := bin.canonical_json(result)
		results[index] = bin.parse_strict_json(replace_canonical_root_member(result_source, result,
			'evidence_sha256', '"${digest}"')) or { panic(err) }
		assert digest !in sources
		sources[digest] = source
	}
	updated_results := bin.JsonValue{
		kind:        .array
		array_value: results
	}
	updated_source := replace_canonical_root_member(matrix_source, root, 'results',
		bin.canonical_json(updated_results))
	assert updated_source.len == matrix_source.len
	return T2cMatrixEvidenceSources{
		matrix_source: updated_source
		sources:       sources
	}
}

fn t2c_write_native_validation_capsule_with_sources(capsule_root string, matrix_source string,
	sources map[string]string, reverse_order bool) []string {
	os.mkdir_all(os.join_path(capsule_root, 'evidence')) or { panic(err) }
	os.write_file(os.join_path(capsule_root, 'native-lane-matrix.json'), matrix_source) or {
		panic(err)
	}
	mut names := sources.keys()
	names.sort()
	mut write_names := names.clone()
	if reverse_order {
		write_names.reverse_in_place()
	}
	for name in write_names {
		os.write_file(os.join_path(capsule_root, 'evidence', name), sources[name]) or { panic(err) }
	}
	return names
}

fn t2c_write_native_validation_capsule(capsule_root string, matrix_source string,
	authority SyntheticToolchainAuthority, reverse_order bool) []string {
	sources := t2c_native_validation_evidence_sources(matrix_source, authority)
	return t2c_write_native_validation_capsule_with_sources(capsule_root, matrix_source, sources,
		reverse_order)
}

fn t2b_replace_matrix_result_member(source string, index int, member string,
	replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	result_value := root.object_value('results') or { panic('matrix results missing') }
	mut results := result_value.array_value.clone()
	assert index >= 0 && index < results.len
	result_source := bin.canonical_json(results[index])
	updated_source := replace_canonical_root_member(result_source, results[index], member,
		replacement)
	results[index] = bin.parse_strict_json(updated_source) or { panic(err) }
	updated := bin.JsonValue{
		kind:        .array
		array_value: results
	}
	return replace_canonical_root_member(source, root, 'results', bin.canonical_json(updated))
}

fn t2a_prepare_toolchain_authority(base string, target_id string) SyntheticToolchainAuthority {
	contract_root := os.join_path(base, 'contract-authority-${target_id}')
	root := os.join_path(contract_root, 'thirdparty', 'tccbin_automation')
	os.mkdir_all(root) or { panic(err) }
	os.cp_all(os.join_path(automation_root(), 'schemas'), os.join_path(root, 'schemas'), true) or {
		panic(err)
	}
	profile_id := '${target_id}-synthetic-v1'
	profile_source := t2a_profile_source(target_id)
	profile := bin.parse_strict_json(profile_source) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	profile_relative_path := 'toolchain-profiles/${target_id}.profile.json'
	profile_path := os.join_path(root, profile_relative_path)
	os.mkdir_all(os.dir(profile_path)) or { panic(err) }
	os.write_file(profile_path, profile_source) or { panic(err) }
	mut registry_source := os.read_file(os.join_path(automation_root(), 'targets.json')) or {
		panic(err)
	}
	target_marker := '"id": "${target_id}"'
	target_offset := registry_source.index(target_marker) or { panic('target missing') }
	prefix := registry_source[..target_offset]
	tail := registry_source[target_offset..]
	binding_marker := '"toolchain_profile": {\n        "profile_id": null,\n        "profile_path": null,\n        "profile_sha256": null\n      }'
	assert tail.count(binding_marker) > 0
	registry_source = prefix +
		tail.replace_once(binding_marker, '"toolchain_profile": {\n        "profile_id": "${profile_id}",\n        "profile_path": "${profile_relative_path}",\n        "profile_sha256": "${profile_sha256}"\n      }')
	os.write_file(os.join_path(root, 'targets.json'), registry_source) or { panic(err) }
	producer_source := t2a_producer_observation_source(target_id, profile_sha256)
	producer := bin.parse_strict_json(producer_source) or { panic(err) }
	return SyntheticToolchainAuthority{
		root:            root
		contract_root:   contract_root
		target_id:       target_id
		profile_id:      profile_id
		profile_sha256:  profile_sha256
		producer_source: producer_source
		producer_sha256: bin.json_sha256(producer)
		producer_digest: (producer.object_value('observation_digest') or {
			panic('observation digest missing')
		}).string_value
	}
}

fn t2a_assert_contract_authority_is_sibling(authority SyntheticToolchainAuthority,
	siblings []string) {
	contract_root := os.real_path(authority.contract_root)
	assert os.real_path(authority.root) == os.join_path(contract_root, 'thirdparty',
		'tccbin_automation')
	for sibling in siblings {
		physical := os.real_path(sibling)
		assert physical != contract_root
		assert os.dir(physical) == os.dir(contract_root)
	}
}

fn t2a_resolved_manifest_toolchain(source string, authority SyntheticToolchainAuthority) string {
	marker := '"toolchain": {\n    "profile_id": null,\n    "profile_sha256": null,\n    "producer_observation": null\n  }'
	assert source.count(marker) == 1
	return source.replace_once(marker,
		'"toolchain": {\n    "profile_id": "${authority.profile_id}",\n    "profile_sha256": "${authority.profile_sha256}",\n    "producer_observation": ${authority.producer_source}\n  }').replace_once('"provenance_status": "incomplete"', if authority.target_id == 'windows-amd64' {
		'"provenance_status": "opaque-accepted"'
	} else {
		'"provenance_status": "complete"'
	})
}

fn t2a_profile_bound_unobserved_toolchain(source string,
	authority SyntheticToolchainAuthority) string {
	marker := '"toolchain": {\n    "profile_id": null,\n    "profile_sha256": null,\n    "producer_observation": null\n  }'
	assert source.count(marker) == 1
	return source.replace_once(marker,
		'"toolchain": {\n    "profile_id": "${authority.profile_id}",\n    "profile_sha256": "${authority.profile_sha256}",\n    "producer_observation": null\n  }')
}

fn t2a_rebind_manifest_toolchain(source string, previous SyntheticToolchainAuthority,
	next SyntheticToolchainAuthority) string {
	previous_block := '"toolchain": {\n    "profile_id": "${previous.profile_id}",\n    "profile_sha256": "${previous.profile_sha256}",\n    "producer_observation": ${previous.producer_source}\n  }'
	next_block := '"toolchain": {\n    "profile_id": "${next.profile_id}",\n    "profile_sha256": "${next.profile_sha256}",\n    "producer_observation": ${next.producer_source}\n  }'
	assert source.count(previous_block) == 1
	return source.replace_once(previous_block, next_block)
}

fn t2a_authority_with_refreshed_producer(authority SyntheticToolchainAuthority,
	evidence_sha256 string) SyntheticToolchainAuthority {
	assert evidence_sha256.len == 64
	producer := bin.parse_strict_json(authority.producer_source) or { panic(err) }
	roles := producer.object_value('roles') or { panic('producer roles missing') }
	assert roles.array_value.len > 0
	old_evidence := (roles.array_value[0].object_value('evidence_sha256') or {
		panic('producer evidence missing')
	}).string_value
	old_digest := (producer.object_value('observation_digest') or {
		panic('producer digest missing')
	}).string_value
	mut source := authority.producer_source.replace_once('"evidence_sha256":"${old_evidence}"',
		'"evidence_sha256":"${evidence_sha256}"')
	without_digest := bin.parse_strict_json(source) or { panic(err) }
	new_digest := bin.toolchain_observation_digest(without_digest) or { panic(err) }
	source = source.replace_once('"observation_digest":"${old_digest}"',
		'"observation_digest":"${new_digest}"')
	refreshed := bin.parse_strict_json(source) or { panic(err) }
	return SyntheticToolchainAuthority{
		...authority
		producer_source: source
		producer_sha256: bin.json_sha256(refreshed)
		producer_digest: new_digest
	}
}

fn t2a_authority_with_migrated_profile(authority SyntheticToolchainAuthority) SyntheticToolchainAuthority {
	profile_id := '${authority.target_id}-synthetic-v2'
	profile := bin.parse_strict_json(t2a_profile_source_with_id(authority.target_id, profile_id)) or {
		panic(err)
	}
	profile_sha256 := bin.json_sha256(profile)
	producer_source := t2a_producer_observation_source_with_profile(authority.target_id,
		profile_id, profile_sha256)
	producer := bin.parse_strict_json(producer_source) or { panic(err) }
	return SyntheticToolchainAuthority{
		...authority
		profile_id:      profile_id
		profile_sha256:  profile_sha256
		producer_source: producer_source
		producer_sha256: bin.json_sha256(producer)
		producer_digest: (producer.object_value('observation_digest') or {
			panic('observation digest missing')
		}).string_value
	}
}

fn t2a_producer_model(authority SyntheticToolchainAuthority) bin.ProducerToolchainModel {
	return bin.ProducerToolchainModel{
		profile_id:         authority.profile_id
		profile_sha256:     authority.profile_sha256
		observation_sha256: authority.producer_sha256
		observation_digest: authority.producer_digest
	}
}
