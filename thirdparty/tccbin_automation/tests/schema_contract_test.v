module tests

import os
import crypto.sha256
import tccbin_automation.bin

struct SchemaPatternOccurrence {
	schema_name string
	pointer     string
	pattern     string
}

struct StringContractMatrix {
	label                string
	pattern              string
	expected_occurrences int
	accepts              []string
	rejects              []string
}

fn escape_json_pointer_token(token string) string {
	return token.replace('~', '~0').replace('/', '~1')
}

fn collect_schema_pattern_occurrences(schema_name string, value bin.JsonValue, pointer string,
	mut occurrences []SchemaPatternOccurrence) {
	match value.kind {
		.object {
			for index, key in value.object_keys {
				child := value.object_values[index]
				child_pointer := '${pointer}/${escape_json_pointer_token(key)}'
				if key == 'pattern' {
					if child.kind != .string_value {
						panic('${schema_name}${child_pointer} pattern is not a string')
					}
					occurrences << SchemaPatternOccurrence{
						schema_name: schema_name
						pointer:     child_pointer
						pattern:     child.string_value
					}
				}
				collect_schema_pattern_occurrences(schema_name, child, child_pointer, mut
					occurrences)
			}
		}
		.array {
			for index, child in value.array_value {
				collect_schema_pattern_occurrences(schema_name, child, '${pointer}/${index}', mut
					occurrences)
			}
		}
		else {}
	}
}

fn schema_pattern_occurrences() []SchemaPatternOccurrence {
	schema_root := os.join_path(automation_root(), 'schemas')
	mut schema_names := os.ls(schema_root) or { panic(err) }
	schema_names = schema_names.filter(it.ends_with('.json'))
	schema_names.sort()
	assert schema_names.len == 22, '${schema_names}'
	mut occurrences := []SchemaPatternOccurrence{}
	for schema_name in schema_names {
		source := os.read_file(os.join_path(schema_root, schema_name)) or { panic(err) }
		schema := bin.parse_strict_json(source) or { panic('${schema_name}: ${err}') }
		collect_schema_pattern_occurrences(schema_name, schema, '', mut occurrences)
	}
	return occurrences
}

fn required_inventory_pattern(occurrences []SchemaPatternOccurrence, expected string,
	expected_occurrences int) string {
	matches := occurrences.filter(it.pattern == expected)
	assert matches.len == expected_occurrences, '${expected}: ${matches}'
	return matches[0].pattern
}

fn inline_pattern_schema(pattern string) bin.JsonValue {
	pattern_value := bin.JsonValue{
		kind:         .string_value
		string_value: pattern
	}
	return bin.parse_strict_json('{"type":"string","pattern":${bin.canonical_json(pattern_value)}}') or {
		panic(err)
	}
}

fn validate_inline_string_schema(schema bin.JsonValue, candidate string, suffix string) []bin.SchemaIssue {
	schema_path := os.join_path(os.temp_dir(), 'tccbin-inline-schema-${os.getpid()}-${suffix}.json')
	input_path := os.join_path(os.temp_dir(), 'tccbin-inline-input-${os.getpid()}-${suffix}.json')
	input := bin.JsonValue{
		kind:         .string_value
		string_value: candidate
	}
	os.write_file(schema_path, bin.canonical_json(schema)) or { panic(err) }
	os.write_file(input_path, bin.canonical_json(input)) or { panic(err) }
	defer {
		os.rm(schema_path) or {}
		os.rm(input_path) or {}
	}
	return bin.validate_json_file(schema_path, input_path) or { panic(err) }
}

fn validate_inline_json_schema(schema bin.JsonValue, input bin.JsonValue,
	suffix string) []bin.SchemaIssue {
	schema_path := os.join_path(os.temp_dir(),
		'tccbin-inline-json-schema-${os.getpid()}-${suffix}.json')
	input_path := os.join_path(os.temp_dir(),
		'tccbin-inline-json-input-${os.getpid()}-${suffix}.json')
	os.write_file(schema_path, bin.canonical_json(schema)) or { panic(err) }
	os.write_file(input_path, bin.canonical_json(input)) or { panic(err) }
	defer {
		os.rm(schema_path) or {}
		os.rm(input_path) or {}
	}
	return bin.validate_json_file(schema_path, input_path) or { panic(err) }
}

fn test_prefix_items_is_ordered_and_items_applies_only_after_the_prefix() {
	closed := bin.parse_strict_json('{"type":"array","prefixItems":[{"const":"native"},{"const":"smoke"}],"items":false}') or {
		panic(err)
	}
	for source in ['[]', '["native"]', '["native","smoke"]'] {
		input := bin.parse_strict_json(source) or { panic(err) }
		assert validate_inline_json_schema(closed, input, 'prefix-closed-${source.len}').len == 0
	}
	for source in ['["smoke"]', '["native","wrong"]', '["native","smoke","extra"]'] {
		input := bin.parse_strict_json(source) or { panic(err) }
		assert validate_inline_json_schema(closed, input, 'prefix-reject-${source.len}').len == 1
	}

	tail := bin.parse_strict_json('{"type":"array","prefixItems":[{"const":"native"}],"items":{"type":"integer"}}') or {
		panic(err)
	}
	assert validate_inline_json_schema(tail, bin.parse_strict_json('["native",1,2]') or {
		panic(err)
	}, 'prefix-tail-valid').len == 0
	assert validate_inline_json_schema(tail, bin.parse_strict_json('["native","not-integer"]') or {
		panic(err)
	}, 'prefix-tail-invalid').len == 1
}

fn assert_inline_string_contract(schema bin.JsonValue, accepts []string, rejects []string,
	label string) {
	for index, candidate in accepts {
		issues := validate_inline_string_schema(schema, candidate, '${label}-accept-${index}')
		assert issues.len == 0, '${label} accepted ${candidate}: ${issues}'
	}
	for index, candidate in rejects {
		issues := validate_inline_string_schema(schema, candidate, '${label}-reject-${index}')
		assert issues.any(it.path == '$'), '${label} rejected ${candidate}: ${issues}'
	}
}

fn validate_manifest_source(source string, suffix string) []bin.SchemaIssue {
	return validate_manifest_source_at(automation_root(), source, suffix)
}

fn validate_manifest_source_at(contract_automation_root string, source string,
	suffix string) []bin.SchemaIssue {
	temporary := os.join_path(os.temp_dir(), 'tccbin-manifest-${os.getpid()}-${suffix}.json')
	os.write_file(temporary, source) or { panic(err) }
	defer {
		os.rm(temporary) or {}
	}
	return bin.validate_manifest(contract_automation_root, temporary) or { panic(err) }
}

fn validate_schema_source_at(contract_automation_root string, schema_name string, source string,
	suffix string) []bin.SchemaIssue {
	temporary := os.join_path(os.temp_dir(), 'tccbin-schema-${os.getpid()}-${suffix}.json')
	os.write_file(temporary, source) or { panic(err) }
	defer {
		os.rm(temporary) or {}
	}
	return bin.validate_json_file(os.join_path(contract_automation_root, 'schemas', schema_name),
		temporary) or { panic(err) }
}

fn validate_schema_source(schema_name string, source string, suffix string) []bin.SchemaIssue {
	return validate_schema_source_at(automation_root(), schema_name, source, suffix)
}

fn schema_fixture(name string) string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures', name)) or {
		panic(err)
	}
}

fn schema_fixture_with_resolved_producer(name string, target_id string) string {
	profile := bin.parse_strict_json(t2a_profile_source(target_id)) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	producer_source := t2a_producer_observation_source(target_id, profile_sha256)
	producer := bin.parse_strict_json(producer_source) or { panic(err) }
	authority := SyntheticToolchainAuthority{
		target_id:       target_id
		profile_id:      '${target_id}-synthetic-v1'
		profile_sha256:  profile_sha256
		producer_source: producer_source
		producer_sha256: bin.json_sha256(producer)
		producer_digest: (producer.object_value('observation_digest') or {
			panic('observation digest missing')
		}).string_value
	}
	return t2a_resolved_manifest_toolchain(schema_fixture(name), authority)
}

fn synthetic_toolchain_strategies(target_id string) []string {
	if target_id in ['freebsd-amd64', 'openbsd-amd64'] {
		return ['cpa-guest', 'cpa-host']
	}
	if target_id == 'windows-amd64' {
		return ['github-hosted-msys2']
	}
	return ['github-hosted']
}

fn synthetic_toolchain_role_id(phase string, strategy string, role_count int) string {
	base := if phase == 'producer' { 'bundle-builder' } else { 'contract-validator' }
	if role_count == 1 {
		return base
	}
	suffix := if strategy == 'cpa-guest' { 'guest' } else { 'host' }
	return '${base}-${suffix}'
}

fn synthetic_toolchain_policy_facts(target_id string, strategy string) string {
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

fn synthetic_toolchain_resolved_facts(target_id string, strategy string) string {
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

fn synthetic_toolchain_profile_source(target_id string) string {
	strategies := synthetic_toolchain_strategies(target_id)
	mut phases := map[string]string{}
	for phase in ['producer', 'validator'] {
		mut roles := []string{}
		for strategy in strategies {
			role_id := synthetic_toolchain_role_id(phase, strategy, strategies.len)
			roles << '{"role_id":"${role_id}","identity_strategy":"${strategy}","identity_policy":${synthetic_toolchain_policy_facts(target_id,
				strategy)}}'
		}
		phases[phase] = roles.join(',')
	}
	profile := bin.parse_strict_json('{"schema_version":1,"profile_id":"${target_id}-synthetic-v1","target_id":"${target_id}","producer":[${phases['producer']}],"validator":[${phases['validator']}]}') or {
		panic(err)
	}
	return bin.canonical_json(profile)
}

fn synthetic_toolchain_observation_source(target_id string, profile_sha256 string,
	phase string) string {
	strategies := synthetic_toolchain_strategies(target_id)
	mut role_sources := []string{}
	for index, strategy in strategies {
		role_id := synthetic_toolchain_role_id(phase, strategy, strategies.len)
		resolution_placeholder := if index == 0 { '8'.repeat(64) } else { '9'.repeat(64) }
		evidence_sha256 := if index == 0 { 'e'.repeat(64) } else { 'f'.repeat(64) }
		role_sources << '{"role_id":"${role_id}","identity_strategy":"${strategy}","resolved_identity":${synthetic_toolchain_resolved_facts(target_id,
			strategy)},"resolution_digest":"${resolution_placeholder}","evidence_sha256":"${evidence_sha256}"}'
	}
	observation_placeholder := '0'.repeat(64)
	mut source := bin.canonical_json(bin.parse_strict_json('{"schema_version":1,"target_id":"${target_id}","profile_id":"${target_id}-synthetic-v1","profile_sha256":"${profile_sha256}","phase":"${phase}","roles":[${role_sources.join(',')}],"observation_digest":"${observation_placeholder}"}') or {
		panic(err)
	})
	observation := bin.parse_strict_json(source) or { panic(err) }
	roles := observation.object_value('roles') or { panic('toolchain roles missing') }
	for role in roles.array_value {
		resolution_placeholder := (role.object_value('resolution_digest') or {
			panic('resolution digest missing')
		}).string_value
		resolution_digest := bin.toolchain_role_resolution_digest(observation, role) or {
			panic(err)
		}
		source = source.replace_once('"resolution_digest":"${resolution_placeholder}"',
			'"resolution_digest":"${resolution_digest}"')
	}
	with_resolution := bin.parse_strict_json(source) or { panic(err) }
	observation_digest := bin.toolchain_observation_digest(with_resolution) or { panic(err) }
	return source.replace_once('"observation_digest":"${observation_placeholder}"',
		'"observation_digest":"${observation_digest}"')
}

fn registry_with_toolchain_profile(registry_source string, target_id string, profile_id string,
	profile_path string, profile_sha256 string) string {
	target_marker := '"id": "${target_id}"'
	target_offset := registry_source.index(target_marker) or { panic('target marker missing') }
	prefix := registry_source[..target_offset]
	tail := registry_source[target_offset..]
	binding_marker := '"toolchain_profile": {\n        "profile_id": null,\n        "profile_path": null,\n        "profile_sha256": null\n      }'
	assert tail.count(binding_marker) > 0
	return prefix +
		tail.replace_once(binding_marker, '"toolchain_profile": {\n        "profile_id": "${profile_id}",\n        "profile_path": "${profile_path}",\n        "profile_sha256": "${profile_sha256}"\n      }')
}

fn registry_with_managed_baseline_activation_policy(registry_source string, target_id string,
	policy_path string, policy_sha256 string) string {
	target_marker := '"id": "${target_id}"'
	target_offset := registry_source.index(target_marker) or { panic('target marker missing') }
	prefix := registry_source[..target_offset]
	tail := registry_source[target_offset..]
	binding_marker := '"policy_path": null,\n        "policy_sha256": null\n      },\n      "toolchain_profile"'
	assert tail.count(binding_marker) > 0
	return prefix +
		tail.replace_once(binding_marker, '"policy_path": "${policy_path}",\n        "policy_sha256": "${policy_sha256}"\n      },\n      "toolchain_profile"')
}

fn test_all_schema_patterns_compile_and_contract_boundaries_are_discriminating() {
	occurrences := schema_pattern_occurrences()
	assert occurrences.len > 0
	mut compile_failures := []string{}
	for occurrence in occurrences {
		_ = bin.matches_json_pattern(occurrence.pattern, '') or {
			compile_failures << '${occurrence.schema_name}${occurrence.pointer}: ${err}'
			continue
		}
	}
	assert compile_failures.len == 0, '${compile_failures}'

	sha64 := 'a'.repeat(64)
	sha63 := sha64[..63]
	platforms := ['freebsd-amd64', 'linux-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64']
	mut candidate_refs := []string{}
	mut gate_trigger_refs := []string{}
	mut thirdparty_refs := []string{}
	mut validation_subjects := []string{}
	for platform in platforms {
		candidate_ref := 'tccbin-candidate/${platform}/${sha64}'
		candidate_refs << candidate_ref
		gate_trigger_refs << 'tccbin-gate-trigger/${platform}/${sha64}/${sha64}'
		thirdparty_refs << 'thirdparty-${platform}'
		validation_subjects << candidate_ref
		validation_subjects << 'thirdparty-${platform}'
	}
	recovery_prefix := 'evidence/2026/08/1/1'
	recovery_suffix := '${sha64}/1-resolve_source_unreachable-${sha64}.json'
	matrices := [
		StringContractMatrix{
			label:                'lower-id'
			pattern:              r'^[a-z0-9][a-z0-9._\-]+$'
			expected_occurrences: 12
			accepts:              ['patch-0001', 'a-b']
			rejects:              ['A-b', 'a/b', 'a']
		},
		StringContractMatrix{
			label:                'normal-lane-id'
			pattern:              r'^[A-Za-z0-9][A-Za-z0-9._\-]{0,127}$'
			expected_occurrences: 1
			accepts:              ['Windows-amd64', 'A-b', 'A']
			rejects:              ['-Windows', 'A/B', 'expected=0']
		},
		StringContractMatrix{
			label:                'repository'
			pattern:              r'^[A-Za-z0-9_.\-]+/[A-Za-z0-9_.\-]+$'
			expected_occurrences: 1
			accepts:              ['vlang/tccbin', 'owner-a/repo_b']
			rejects:              ['vlang', 'vlang/tcc/bin', 'vlang/repo+']
		},
		StringContractMatrix{
			label:                'relative-path-grammar'
			pattern:              r'^[A-Za-z0-9._+\-]+(/[A-Za-z0-9._+\-]+)*$'
			expected_occurrences: 2
			accepts:              ['patches/0001-fix.diff', 'a+b/c-d']
			rejects:              ['/a', 'a//b', 'a b']
		},
		StringContractMatrix{
			label:                'branch-grammar'
			pattern:              r'^[A-Za-z0-9._/\-]+$'
			expected_occurrences: 1
			accepts:              ['mob', 'feature/a-b']
			rejects:              ['feature+a', 'feature a', '']
		},
		StringContractMatrix{
			label:                'validation-subject'
			pattern:              r'^((tccbin-candidate/((freebsd-amd64)|(linux-amd64)|(macos-amd64)|(macos-arm64)|(openbsd-amd64)|(windows-amd64))/[0-9a-f]{64})|(thirdparty-((freebsd-amd64)|(linux-amd64)|(macos-amd64)|(macos-arm64)|(openbsd-amd64)|(windows-amd64))))$'
			expected_occurrences: 1
			accepts:              validation_subjects
			rejects:              ['tccbin-candidate/solaris-amd64/${sha64}',
				'thirdparty-windows-amd64/${sha64}']
		},
		StringContractMatrix{
			label:                'gate-run-name'
			pattern:              r'^tccbin-((native-gate)|(v-smoke))/[0-9a-f]{64}$'
			expected_occurrences: 1
			accepts:              ['tccbin-native-gate/${sha64}', 'tccbin-v-smoke/${sha64}']
			rejects:              ['tccbin-recovery/${sha64}', 'tccbin-native/${sha64}']
		},
		StringContractMatrix{
			label:                'run-url'
			pattern:              r'^https://github\.com/((vlang/v)|(vlang/tccbin))/actions/runs/[1-9][0-9]*$'
			expected_occurrences: 1
			accepts:              ['https://github.com/vlang/v/actions/runs/1',
				'https://github.com/vlang/tccbin/actions/runs/99']
			rejects:              ['https://githubXcom/vlang/v/actions/runs/1',
				'https://github.com/vlang/v/actions/runs/0',
				'https://github.com/other/repo/actions/runs/1']
		},
		StringContractMatrix{
			label:                'job-url'
			pattern:              r'^https://github\.com/((vlang/v)|(vlang/tccbin))/actions/runs/[1-9][0-9]*/job/[1-9][0-9]*$'
			expected_occurrences: 2
			accepts:              ['https://github.com/vlang/v/actions/runs/1/job/2',
				'https://github.com/vlang/tccbin/actions/runs/99/job/100']
			rejects:              ['https://github.com/vlang/v/actions/runs/1/job/0',
				'https://github.com/vlang/v/actions/runs/1',
				'https://github.com/other/repo/actions/runs/1/job/2']
		},
		StringContractMatrix{
			label:                'candidate-ref'
			pattern:              r'^tccbin-candidate/((freebsd-amd64)|(linux-amd64)|(macos-amd64)|(macos-arm64)|(openbsd-amd64)|(windows-amd64))/[0-9a-f]{64}$'
			expected_occurrences: 2
			accepts:              candidate_refs
			rejects:              ['tccbin-candidate/solaris-amd64/${sha64}',
				'tccbin-candidate/windows-amd64/${sha63}']
		},
		StringContractMatrix{
			label:                'gate-trigger-ref'
			pattern:              r'^tccbin-gate-trigger/((freebsd-amd64)|(linux-amd64)|(macos-amd64)|(macos-arm64)|(openbsd-amd64)|(windows-amd64))/[0-9a-f]{64}/[0-9a-f]{64}$'
			expected_occurrences: 1
			accepts:              gate_trigger_refs
			rejects:              ['tccbin-gate-trigger/windows-amd64/${sha64}',
				'tccbin-gate-trigger/solaris-amd64/${sha64}/${sha64}']
		},
		StringContractMatrix{
			label:                'thirdparty-ref'
			pattern:              r'^thirdparty-((freebsd-amd64)|(linux-amd64)|(macos-amd64)|(macos-arm64)|(openbsd-amd64)|(windows-amd64))$'
			expected_occurrences: 2
			accepts:              thirdparty_refs
			rejects:              ['thirdparty-openbsd-arm64', 'thirdparty-windows-amd64-extra']
		},
		StringContractMatrix{
			label:                'recovery-evidence'
			pattern:              r'^evidence/[0-9]{4}/[0-9]{2}/[1-9][0-9]*/[1-9][0-9]*/((tinycc-mob)|(bdwgc-master)|(libatomic_ops-master))/[0-9a-f]{64}/[0-9]+-resolve_source_unreachable-[0-9a-f]{64}\.json$'
			expected_occurrences: 1
			accepts:              ['${recovery_prefix}/tinycc-mob/${recovery_suffix}',
				'${recovery_prefix}/bdwgc-master/${recovery_suffix}',
				'${recovery_prefix}/libatomic_ops-master/${recovery_suffix}']
			rejects:              ['${recovery_prefix}/tinycc-main/${recovery_suffix}',
				'${recovery_prefix}/tinycc-mob/${recovery_suffix}x']
		},
		StringContractMatrix{
			label:                'github-prefix'
			pattern:              r'^https://github\.com/'
			expected_occurrences: 3
			accepts:              ['https://github.com/vlang/v']
			rejects:              ['https://githubXcom/vlang/v', 'http://github.com/vlang/v']
		},
	]
	for matrix in matrices {
		pattern := required_inventory_pattern(occurrences, matrix.pattern,
			matrix.expected_occurrences)
		assert_inline_string_contract(inline_pattern_schema(pattern), matrix.accepts,
			matrix.rejects, matrix.label)
	}

	common := bin.parse_strict_json(os.read_file(os.join_path(automation_root(), 'schemas',
		'common.schema.json')) or { panic(err) }) or { panic(err) }
	definitions := common.object_value('$defs') or { panic('common definitions missing') }
	relative_path_schema := definitions.object_value('relative_path') or {
		panic('relative path schema missing')
	}
	branch_schema := definitions.object_value('branch') or { panic('branch schema missing') }
	symlink_target_schema := definitions.object_value('symlink_target') or {
		panic('symlink target schema missing')
	}
	dot_accepts := ['.hidden', '....', 'a/.hidden/b', 'feature/a-b']
	mut dot_rejects := []string{}
	for dots in ['.', '..', '...'] {
		dot_rejects << dots
		dot_rejects << '${dots}/a'
		dot_rejects << 'a/${dots}'
		dot_rejects << 'a/${dots}/b'
	}
	assert_inline_string_contract(relative_path_schema, dot_accepts, dot_rejects,
		'relative-dot-segments')
	assert_inline_string_contract(branch_schema, dot_accepts, dot_rejects, 'branch-dot-segments')
	assert_inline_string_contract(symlink_target_schema, [
		'libgc.1.dylib',
		'../libgc.la',
		'../../outside',
		'/System/DriverKit/usr/lib/libSystem.dylib',
	], ['', '/usr/lib/libSystem.dylib', '..\\libgc.la'], 'symlink-target-schema-layer')

	source_state := bin.parse_strict_json(os.read_file(os.join_path(automation_root(), 'schemas',
		'source-state.schema.json')) or { panic(err) }) or { panic(err) }
	source_definitions := source_state.object_value('$defs') or {
		panic('source-state definitions missing')
	}
	operation_entry := source_definitions.object_value('operation_entry') or {
		panic('source-state operation entry missing')
	}
	entry_properties := operation_entry.object_value('properties') or {
		panic('source-state operation properties missing')
	}
	evidence_path_schema := entry_properties.object_value('evidence_path') or {
		panic('source-state evidence path schema missing')
	}
	source_prefix := 'evidence/2026/08/1/2'
	source_accepts := [
		'${source_prefix}/tinycc-mob/${sha64}/1-resolve_source_unreachable-${sha64}.json',
		'${source_prefix}/source-/${sha64}/3-event-${sha64}.json',
		'${source_prefix}/source/${sha64}/3-event--${sha64}.json',
		'${source_prefix}/-/${sha64}/3---${sha64}.json',
		'${source_prefix}/source/${sha64}/3--event-${sha64}.json',
		'${source_prefix}/source-name/${sha64}/3-event-name-${sha64}.json',
	]
	source_rejects := [
		'${source_prefix}/source/${sha64}/3--${sha64}.json',
		'${source_prefix}/source+bad/${sha64}/3-event-${sha64}.json',
		'${source_prefix}/source/${sha64}/3-event+bad-${sha64}.json',
		'${source_prefix}/source/${sha64}/3-event-${sha63}.json',
		'${source_prefix}/source/${sha64}/3-event-${sha64.to_upper()}.json',
		'${source_prefix}/source/${sha64}/3-event-${sha64}.js',
		'evidence/2026/08/0/2/source/${sha64}/3-event-${sha64}.json',
		'evidence/2026/08/1/0/source/${sha64}/3-event-${sha64}.json',
		'${source_prefix}/source/${sha64}/3-event/name-${sha64}.json',
	]
	assert_inline_string_contract(evidence_path_schema, source_accepts, source_rejects,
		'source-state-evidence-path')
}

fn source_state_window_source(operation_count int, retained_count int) string {
	if retained_count < 0 || retained_count > operation_count {
		panic('invalid SourceState retained window size')
	}
	start_count := operation_count - retained_count
	anchor_seed := bin.parse_strict_json('{"source_window_anchor":0}') or { panic(err) }
	genesis_digest := bin.json_sha256(anchor_seed)
	mut anchor_digest := genesis_digest
	mut previous_chain := genesis_digest
	mut entries := []string{}
	subject_fingerprint := '1111111111111111111111111111111111111111111111111111111111111111'
	for sequence in 1 .. operation_count + 1 {
		operation_id := bin.json_sha256(bin.parse_strict_json('{"source_operation":${sequence}}') or {
			panic(err)
		})
		previous_state := bin.parse_strict_json('{"schema_version":2,"generation":${sequence - 1},"source_id":"tinycc-mob","canonical_url":"https://repo.or.cz/tinycc.git","ref":"mob","status":"source_unreachable","resolved_sha":null,"source_fingerprint":"${subject_fingerprint}","last_attempt_at":"2026-08-03T02:01:00Z","mode":"upstream-recovery-daily","originating_run_id":9001,"waiting_consumers":[],"operation_count":0,"operation_chain_digest":"${genesis_digest}","operation_window":{"start_count":0,"anchor_digest":"${genesis_digest}","entries":[]}}') or {
			panic(err)
		}
		resulting_state := bin.parse_strict_json(bin.canonical_json(previous_state).replace_once('"generation":${sequence - 1}',
			'"generation":${sequence}')) or { panic(err) }
		previous_state_digest := bin.source_state_snapshot_digest(previous_state) or { panic(err) }
		resulting_state_digest := bin.source_state_snapshot_digest(resulting_state) or {
			panic(err)
		}
		evidence_digest := bin.json_sha256(bin.parse_strict_json('{"source_evidence":${sequence}}') or {
			panic(err)
		})
		evidence_path := 'evidence/2026/08/9001/1/tinycc-mob/${operation_id}/${sequence}-resolve_source_unreachable-${subject_fingerprint}.json'
		mut entry := '{"sequence":${sequence},"operation_id":"${operation_id}","transition":"resolve_source_unreachable","previous_generation":${sequence - 1},"resulting_generation":${sequence},"previous_state_digest":"${previous_state_digest}","resulting_state_digest":"${resulting_state_digest}","evidence_path":"${evidence_path}","evidence_digest":"${evidence_digest}","previous_chain_digest":"${previous_chain}","resulting_chain_digest":"0000000000000000000000000000000000000000000000000000000000000000"}'
		chain_digest := bin.source_state_operation_chain_digest(bin.parse_strict_json(entry) or {
			panic(err)
		}) or { panic(err) }
		entry = entry.replace_once('"resulting_chain_digest":"0000000000000000000000000000000000000000000000000000000000000000"',
			'"resulting_chain_digest":"${chain_digest}"')
		if sequence > start_count {
			entries << entry
		}
		previous_chain = chain_digest
		if sequence == start_count {
			anchor_digest = chain_digest
		}
	}
	return '{"schema_version":2,"generation":${operation_count},"source_id":"tinycc-mob","canonical_url":"https://repo.or.cz/tinycc.git","ref":"mob","status":"source_unreachable","resolved_sha":null,"source_fingerprint":"${subject_fingerprint}","last_attempt_at":"2026-08-03T02:01:00Z","mode":"upstream-recovery-daily","originating_run_id":9001,"waiting_consumers":[],"operation_count":${operation_count},"operation_chain_digest":"${previous_chain}","operation_window":{"start_count":${start_count},"anchor_digest":"${anchor_digest}","entries":[${entries.join(',')}]}}'
}

fn rehashed_source_window_discontinuity(source string, kind string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	window := root.object_value('operation_window') or { panic('operation window missing') }
	entries := window.object_value('entries') or { panic('operation entries missing') }
	if entries.array_value.len < 3 || kind !in ['generation', 'state', 'chain'] {
		panic('rehashed discontinuity needs three entries and one closed kind')
	}
	mut previous_chain := (window.object_value('anchor_digest') or {
		panic('operation anchor missing')
	}).string_value
	mut rebuilt := []string{}
	for index, original in entries.array_value {
		mut previous_generation := (original.object_value('previous_generation') or {
			panic('previous generation missing')
		}).int_value
		mut resulting_generation := (original.object_value('resulting_generation') or {
			panic('resulting generation missing')
		}).int_value
		mut previous_state_digest := (original.object_value('previous_state_digest') or {
			panic('previous state missing')
		}).string_value
		mut entry_previous_chain := previous_chain
		if index == 1 {
			if kind == 'generation' {
				previous_generation--
				resulting_generation--
			} else if kind == 'state' {
				previous_state_digest = 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
			} else {
				entry_previous_chain = 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
			}
		}
		mut entry := '{"sequence":${(original.object_value('sequence') or {
			panic('sequence missing')
		}).int_value},"operation_id":${bin.canonical_json(original.object_value('operation_id') or {
			panic('operation ID missing')
		})},"transition":${bin.canonical_json(original.object_value('transition') or {
			panic('transition missing')
		})},"previous_generation":${previous_generation},"resulting_generation":${resulting_generation},"previous_state_digest":"${previous_state_digest}","resulting_state_digest":${bin.canonical_json(original.object_value('resulting_state_digest') or {
			panic('resulting state missing')
		})},"evidence_path":${bin.canonical_json(original.object_value('evidence_path') or {
			panic('evidence path missing')
		})},"evidence_digest":${bin.canonical_json(original.object_value('evidence_digest') or {
			panic('evidence digest missing')
		})},"previous_chain_digest":"${entry_previous_chain}","resulting_chain_digest":"0000000000000000000000000000000000000000000000000000000000000000"}'
		resulting_chain := bin.source_state_operation_chain_digest(bin.parse_strict_json(entry) or {
			panic(err)
		}) or { panic(err) }
		entry = entry.replace_once('0000000000000000000000000000000000000000000000000000000000000000',
			resulting_chain)
		rebuilt << entry
		previous_chain = resulting_chain
	}
	old_window := bin.canonical_json(window)
	new_window := '{"start_count":${(window.object_value('start_count') or {
		panic('start count missing')
	}).int_value},"anchor_digest":${bin.canonical_json(window.object_value('anchor_digest') or {
		panic('anchor missing')
	})},"entries":[${rebuilt.join(',')}]}'
	mut result := bin.canonical_json(root).replace_once('"operation_window":${old_window}',
		'"operation_window":${new_window}')
	old_chain := (root.object_value('operation_chain_digest') or { panic('chain root missing') }).string_value
	result = result.replace_once('"operation_chain_digest":"${old_chain}"',
		'"operation_chain_digest":"${previous_chain}"')
	return result
}

fn assert_v_smoke_attempt_digests_match(source string) {
	root := bin.parse_strict_json(source) or { panic(err) }
	smoke := root.object_value('v_smoke_execution') or { panic('V smoke missing') }
	dispatches := smoke.object_value('dispatches') or { panic('dispatch reservations missing') }
	for dispatch in dispatches.array_value {
		facts_digest := dispatch.object_value('facts_digest') or {
			panic('dispatch facts digest missing')
		}
		assert facts_digest.string_value == bin.v_smoke_dispatch_facts_digest(smoke, dispatch) or {
			panic(err)
		}
	}
	run_absent_attempts := smoke.object_value('run_absent_attempts') or {
		panic('run-absent attempts missing')
	}
	for run_absent in run_absent_attempts.array_value {
		facts_digest := run_absent.object_value('facts_digest') or {
			panic('run-absent facts digest missing')
		}
		assert facts_digest.string_value == bin.v_smoke_run_absent_facts_digest(smoke, run_absent) or {
			panic(err)
		}
	}
	attempts := smoke.object_value('attempts') or { panic('V smoke attempts missing') }
	for attempt in attempts.array_value {
		ack_digest := attempt.object_value('ack_facts_digest') or { panic('ACK digest missing') }
		assert ack_digest.string_value == bin.v_smoke_ack_facts_digest(smoke, attempt) or {
			panic(err)
		}
		completion_digest := attempt.object_value('completion_facts_digest') or {
			panic('completion digest missing')
		}
		if completion_digest.kind != .null_value {
			assert completion_digest.string_value == bin.v_smoke_completion_facts_digest(smoke,
				attempt) or { panic(err) }
		}
	}
	replay_digest := smoke.object_value('replay_facts_digest') or { panic('replay digest missing') }
	assert replay_digest.string_value == bin.v_smoke_replay_facts_digest(smoke) or { panic(err) }
}

fn null_target_object(source string, key string, next_key string) string {
	start_marker := '  "${key}": {'
	next_marker := '  "${next_key}":'
	start := source.index(start_marker) or { panic('${key} object marker missing') }
	next_after := source.index_after(next_marker, start) or { panic('${next_key} marker missing') }
	next := next_after
	return source[..start] + '  "${key}": null,\n' + source[next..]
}

fn replace_nth_occurrence(source string, old string, replacement string, ordinal int) string {
	if ordinal < 1 {
		panic('replacement ordinal must be positive')
	}
	mut offset := 0
	for current in 1 .. ordinal + 1 {
		start := source.index_after(old, offset) or { panic('occurrence ${ordinal} missing') }
		end := start + old.len
		if current == ordinal {
			return source[..start] + replacement + source[end..]
		}
		offset = end
	}
	panic('unreachable replacement')
}

fn replace_nth_json_string_value(source string, key string, ordinal int, replacement string) string {
	marker := '"${key}": "'
	if ordinal < 1 {
		panic('replacement ordinal must be positive')
	}
	mut offset := 0
	for current in 1 .. ordinal + 1 {
		marker_start := source.index_after(marker, offset) or { panic('${key} occurrence missing') }
		value_start := marker_start + marker.len
		value_end := source.index_after('"', value_start) or { panic('${key} value is open') }
		if current == ordinal {
			return source[..value_start] + replacement + source[value_end..]
		}
		offset = value_end + 1
	}
	panic('unreachable JSON string replacement')
}

fn replace_recovery_successor_projection(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	successor := bin.canonical_json(handoffs.array_value[1])
	mut updated := successor.replace_once(old, replacement)
	if updated == successor {
		panic('recovery successor mutation target missing')
	}
	updated_source := source.replace_once(successor, updated)
	if updated_source == source {
		panic('canonical recovery successor projection missing')
	}
	return updated_source
}

fn replace_recovery_successor_root_member(source string, key string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	successor := handoffs.array_value[1]
	if successor.kind != .object {
		panic('recovery successor must be an object')
	}
	replacement_value := bin.parse_strict_json(replacement) or {
		panic('invalid recovery successor replacement: ${err}')
	}
	mut successor_keys := successor.object_keys.clone()
	mut successor_values := successor.object_values.clone()
	mut matching_indices := []int{}
	for index, candidate in successor_keys {
		if candidate == key {
			matching_indices << index
		}
	}
	if matching_indices.len != 1 {
		panic('recovery successor root member ${key} must occur exactly once')
	}
	successor_values[matching_indices[0]] = replacement_value
	canonical_successor := bin.canonical_json(successor)
	updated_successor := bin.canonical_json(bin.JsonValue{
		kind:          .object
		object_keys:   successor_keys
		object_values: successor_values
	})
	if source.count(canonical_successor) != 1 {
		panic('canonical recovery successor must occur exactly once')
	}
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('canonical recovery successor projection missing')
	}
	return updated_source
}

fn replace_recovery_predecessor_root_member(source string, key string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	predecessor := handoffs.array_value[0]
	if predecessor.kind != .object {
		panic('recovery predecessor must be an object')
	}
	replacement_value := bin.parse_strict_json(replacement) or {
		panic('invalid recovery predecessor replacement: ${err}')
	}
	mut predecessor_values := predecessor.object_values.clone()
	mut matching_indices := []int{}
	for index, candidate in predecessor.object_keys {
		if candidate == key {
			matching_indices << index
		}
	}
	if matching_indices.len != 1 {
		panic('recovery predecessor root member ${key} must occur exactly once')
	}
	predecessor_values[matching_indices[0]] = replacement_value
	canonical_predecessor := bin.canonical_json(predecessor)
	updated_predecessor := bin.canonical_json(bin.JsonValue{
		kind:          .object
		object_keys:   predecessor.object_keys.clone()
		object_values: predecessor_values
	})
	if source.count(canonical_predecessor) != 1 {
		panic('canonical recovery predecessor must occur exactly once')
	}
	updated_source := source.replace_once(canonical_predecessor, updated_predecessor)
	if updated_source == source {
		panic('canonical recovery predecessor projection missing')
	}
	return updated_source
}

fn replace_recovery_successor_all(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	successor := bin.canonical_json(handoffs.array_value[1])
	updated := successor.replace(old, replacement)
	if updated == successor {
		panic('recovery successor mutation target missing')
	}
	updated_source := source.replace_once(successor, updated)
	if updated_source == source {
		panic('canonical recovery successor projection missing')
	}
	return updated_source
}

fn refresh_terminal_revalidation_facts_digest(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	old_digest := proof.object_value('facts_digest') or {
		panic('terminal revalidation digest missing')
	}
	new_digest := bin.terminal_revalidation_facts_digest(proof) or { panic(err) }
	canonical_proof := bin.canonical_json(proof)
	updated_proof := replace_nth_json_digest(canonical_proof, 'facts_digest', 1,
		old_digest.string_value, new_digest)
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	if updated_successor == canonical_successor {
		panic('terminal revalidation projection missing')
	}
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal recovery successor projection missing')
	}
	return updated_source.replace('handoff_complete_${old_digest.string_value}',
		'handoff_complete_${new_digest}')
}

fn refresh_terminal_revalidation_smoke_digests(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	smoke := proof.object_value('v_smoke_execution') or { panic('terminal V smoke missing') }
	wrapper := '{"v_smoke_execution":${bin.canonical_json(smoke)}}'
	refreshed_wrapper := bin.parse_strict_json(refresh_v_smoke_facts_digests(wrapper)) or {
		panic(err)
	}
	refreshed_smoke := refreshed_wrapper.object_value('v_smoke_execution') or {
		panic('refreshed terminal V smoke missing')
	}
	canonical_proof := bin.canonical_json(proof)
	mut updated_proof := canonical_proof.replace_once('"v_smoke_execution":${bin.canonical_json(smoke)}',
		'"v_smoke_execution":${bin.canonical_json(refreshed_smoke)}')
	for projection_key in ['source_atomic_pre_projection', 'pre_business_projection',
		'final_projection'] {
		updated_proof_value := bin.parse_strict_json(updated_proof) or { panic(err) }
		projection := updated_proof_value.object_value(projection_key) or {
			panic('${projection_key} missing')
		}
		if projection.kind != .object {
			continue
		}
		projection_smoke := projection.object_value('v_smoke_execution') or {
			panic('${projection_key} V smoke missing')
		}
		if projection_smoke.kind != .object {
			continue
		}
		projection_wrapper := '{"v_smoke_execution":${bin.canonical_json(projection_smoke)}}'
		refreshed_projection_wrapper := bin.parse_strict_json(refresh_v_smoke_facts_digests(projection_wrapper)) or {
			panic(err)
		}
		refreshed_projection_smoke := refreshed_projection_wrapper.object_value('v_smoke_execution') or {
			panic('refreshed ${projection_key} V smoke missing')
		}
		canonical_projection := bin.canonical_json(projection)
		updated_projection := canonical_projection.replace_once('"v_smoke_execution":${bin.canonical_json(projection_smoke)}',
			'"v_smoke_execution":${bin.canonical_json(refreshed_projection_smoke)}')
		updated_proof = updated_proof.replace_once('"${projection_key}":${canonical_projection}',
			'"${projection_key}":${updated_projection}')
	}
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal smoke successor projection missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

// replace_terminal_historical_smoke_created_at coordinates the same top-level mutation across
// every historical copy and recomputes all smoke/proof self-digests. It deliberately leaves the
// durable v-smoke-complete CAS suffix untouched, so only the v4 full-payload commitment can reject
// the forged projection.
fn replace_terminal_historical_smoke_created_at(source string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	proof_smoke := proof.object_value('v_smoke_execution') or { panic('proof smoke missing') }
	old_created_at := (proof_smoke.object_value('created_at') or {
		panic('proof smoke created_at missing')
	}).string_value
	mut updated_proof := bin.canonical_json(proof)
	for key in ['source_atomic_pre_projection', 'pre_business_projection', 'final_projection',
		'v_smoke_execution'] {
		current_proof := bin.parse_strict_json(updated_proof) or { panic(err) }
		mut smoke := if key == 'v_smoke_execution' {
			current_proof.object_value(key) or { panic('proof smoke missing') }
		} else {
			projection := current_proof.object_value(key) or { panic('${key} missing') }
			if projection.kind == .object {
				projection.object_value('v_smoke_execution') or { panic('${key} smoke missing') }
			} else {
				bin.JsonValue{
					kind: .null_value
				}
			}
		}
		if smoke.kind != .object {
			continue
		}
		canonical_smoke := bin.canonical_json(smoke)
		mutated_smoke := canonical_smoke.replace_once('"created_at":"${old_created_at}"',
			'"created_at":"${replacement}"')
		if mutated_smoke == canonical_smoke {
			panic('${key} smoke top-level created_at missing')
		}
		wrapper := refresh_v_smoke_facts_digests('{"v_smoke_execution":${mutated_smoke}}')
		refreshed_smoke := (bin.parse_strict_json(wrapper) or { panic(err) }).object_value('v_smoke_execution') or {
			panic('refreshed ${key} smoke missing')
		}
		if key == 'v_smoke_execution' {
			updated_proof = updated_proof.replace_once('"v_smoke_execution":${canonical_smoke}',
				'"v_smoke_execution":${bin.canonical_json(refreshed_smoke)}')
		} else {
			projection := current_proof.object_value(key) or { panic('${key} missing') }
			canonical_projection := bin.canonical_json(projection)
			updated_projection := canonical_projection.replace_once('"v_smoke_execution":${canonical_smoke}',
				'"v_smoke_execution":${bin.canonical_json(refreshed_smoke)}')
			updated_proof = updated_proof.replace_once('"${key}":${canonical_projection}',
				'"${key}":${updated_projection}')
		}
	}
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${bin.canonical_json(proof)}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('coordinated terminal smoke mutation did not change the source')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_native_evidence(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	native := proof.object_value('native_gate_execution') or { panic('native evidence missing') }
	canonical_native := bin.canonical_json(native)
	updated_native := canonical_native.replace_once(old, replacement)
	if updated_native == canonical_native {
		panic('terminal native mutation target missing')
	}
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"native_gate_execution":${canonical_native}',
		'"native_gate_execution":${updated_native}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal native successor projection missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_smoke_projection(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	smoke := proof.object_value('v_smoke_execution') or { panic('terminal V smoke missing') }
	canonical_smoke := bin.canonical_json(smoke)
	mutation_start := canonical_smoke.last_index(old) or {
		panic('terminal V-smoke mutation target missing')
	}
	mutation_end := mutation_start + old.len
	updated_smoke := canonical_smoke[..mutation_start] + replacement +
		canonical_smoke[mutation_end..]
	if updated_smoke == canonical_smoke {
		panic('terminal V-smoke mutation target missing')
	}
	canonical_proof := bin.canonical_json(proof)
	proof_marker := '"v_smoke_execution":${canonical_smoke}'
	proof_start := canonical_proof.last_index(proof_marker) or {
		panic('terminal V-smoke proof projection missing')
	}
	proof_end := proof_start + proof_marker.len
	updated_proof := canonical_proof[..proof_start] + '"v_smoke_execution":${updated_smoke}' +
		canonical_proof[proof_end..]
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal V-smoke successor projection missing')
	}
	return refresh_terminal_revalidation_smoke_digests(updated_source)
}

fn replace_terminal_native_check(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	check := proof.object_value('native_gate_check') or { panic('terminal native check missing') }
	canonical_check := bin.canonical_json(check)
	updated_check := canonical_check.replace_once(old, replacement)
	if updated_check == canonical_check {
		panic('terminal native check mutation target missing')
	}
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"native_gate_check":${canonical_check}',
		'"native_gate_check":${updated_check}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal native check successor projection missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn refresh_terminal_handoff_native_digests(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	if handoffs.array_value.len != 2 {
		panic('two-step recovery chain missing')
	}
	proof := handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	native := proof.object_value('native_gate_execution') or { panic('terminal native missing') }
	check := proof.object_value('native_gate_check') or { panic('terminal native check missing') }
	new_evidence_digest := bin.native_gate_evidence_digest(native) or { panic(err) }
	new_check_digest := bin.native_gate_check_digest(check) or { panic(err) }
	mut result := source
	for handoff in handoffs.array_value {
		old_evidence := handoff.object_value('native_gate_evidence_digest') or {
			panic('native evidence commitment missing')
		}
		old_check := handoff.object_value('native_gate_check_digest') or {
			panic('native check commitment missing')
		}
		result = replace_json_digest_all(result, 'native_gate_evidence_digest',
			old_evidence.string_value, new_evidence_digest)
		result = replace_json_digest_all(result, 'native_gate_check_digest',
			old_check.string_value, new_check_digest)
	}
	return refresh_terminal_revalidation_facts_digest(result)
}

fn replace_terminal_source_refetch_with_null(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"source_refetch":${bin.canonical_json(refetch)}',
		'"source_refetch":null')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal source-refetch successor projection missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_source_state(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	state := proof.object_value('source_state_snapshot') or { panic('source state missing') }
	canonical_state := bin.canonical_json(state)
	updated_state := canonical_state.replace_once(old, replacement)
	if updated_state == canonical_state {
		panic('terminal source-state mutation target missing')
	}
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"source_state_snapshot":${canonical_state}',
		'"source_state_snapshot":${updated_state}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal source-state successor projection missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_proof_member(source string, key string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	canonical_proof := bin.canonical_json(proof)
	member := proof.object_value(key) or { panic('${key} terminal proof member missing') }
	updated_proof := canonical_proof.replace_once('"${key}":${bin.canonical_json(member)}',
		'"${key}":${replacement}')
	if updated_proof == canonical_proof {
		panic('${key} terminal proof projection missing')
	}
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('${key} terminal successor projection missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn terminal_source_resolution_operation_id(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	proof := handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	return (refetch.object_value('resolution_operation_id') or {
		panic('source resolution operation missing')
	}).string_value
}

fn refresh_terminal_source_evidence_digests(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	pre_state := proof.object_value('source_state_pre_snapshot') or {
		panic('pre source state missing')
	}
	post_state := proof.object_value('source_state_snapshot') or {
		panic('post source state missing')
	}
	history := proof.object_value('source_state_cas_history') or { panic('source history missing') }
	if history.array_value.len != 1 {
		panic('coordinated source test requires one CAS record')
	}
	transition := history.array_value[0]
	old_transition_digest := (transition.object_value('evidence_digest') or {
		panic('source transition digest missing')
	}).string_value
	mut updated_transition := bin.canonical_json(transition)
	updated_transition = updated_transition.replace_once('"previous_state_digest":"${(transition.object_value('previous_state_digest') or {
		panic('previous state digest missing')
	}).string_value}"', '"previous_state_digest":"${bin.source_state_snapshot_digest(pre_state) or {
		panic(err)
	}}"')
	updated_transition = updated_transition.replace_once('"resulting_state_digest":"${(transition.object_value('resulting_state_digest') or {
		panic('resulting state digest missing')
	}).string_value}"', '"resulting_state_digest":"${bin.source_state_snapshot_digest(post_state) or {
		panic(err)
	}}"')
	transition_without_new_digest := bin.parse_strict_json(updated_transition) or { panic(err) }
	new_transition_digest := bin.source_state_transition_evidence_digest(transition_without_new_digest) or {
		panic(err)
	}
	updated_transition = updated_transition.replace_once('"evidence_digest":"${old_transition_digest}"',
		'"evidence_digest":"${new_transition_digest}"')
	updated_history := bin.parse_strict_json('[${updated_transition}]') or { panic(err) }
	refetch := proof.object_value('source_refetch') or { panic('source refetch missing') }
	old_refetch_digest := (refetch.object_value('evidence_digest') or {
		panic('source refetch digest missing')
	}).string_value
	new_refetch_digest := bin.source_refetch_evidence_digest(refetch, pre_state, post_state,
		updated_history) or { panic(err) }
	updated_refetch := bin.canonical_json(refetch).replace_once('"evidence_digest":"${old_refetch_digest}"',
		'"evidence_digest":"${new_refetch_digest}"')
	canonical_proof := bin.canonical_json(proof)
	mut updated_proof := canonical_proof.replace_once('"source_state_cas_history":${bin.canonical_json(history)}',
		'"source_state_cas_history":${bin.canonical_json(updated_history)}')
	updated_proof = updated_proof.replace_once('"source_refetch":${bin.canonical_json(refetch)}',
		'"source_refetch":${updated_refetch}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	mut updated_source := source.replace_once(canonical_successor, updated_successor)
	updated_source = updated_source.replace(old_refetch_digest, new_refetch_digest)
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_ancestry_coordinated(source string, old string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	ancestry := proof.object_value('git_ancestry_proof') or { panic('Git ancestry missing') }
	old_evidence_digest := (ancestry.object_value('evidence_digest') or {
		panic('Git ancestry evidence digest missing')
	}).string_value
	mut updated_ancestry := bin.canonical_json(ancestry).replace_once(old, replacement)
	if updated_ancestry == bin.canonical_json(ancestry) {
		panic('Git ancestry mutation target missing')
	}
	new_evidence_digest := bin.git_ancestry_evidence_digest(bin.parse_strict_json(updated_ancestry) or {
		panic(err)
	}) or { panic(err) }
	updated_ancestry = updated_ancestry.replace_once('"evidence_digest":"${old_evidence_digest}"',
		'"evidence_digest":"${new_evidence_digest}"')
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"git_ancestry_proof":${bin.canonical_json(ancestry)}',
		'"git_ancestry_proof":${updated_ancestry}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	mut updated_source := source.replace_once(canonical_successor, updated_successor)
	updated_source = updated_source.replace(old_evidence_digest, new_evidence_digest)
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_final_projection_member(source string, key string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	projection := proof.object_value('final_projection') or {
		panic('terminal final projection missing')
	}
	canonical_projection := bin.canonical_json(projection)
	updated_projection := replace_canonical_root_member(canonical_projection, projection, key,
		replacement)
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"final_projection":${canonical_projection}',
		'"final_projection":${updated_projection}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal final projection successor missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn replace_terminal_pre_projection_member(source string, key string, replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	successor := handoffs.array_value[1]
	proof := successor.object_value('terminal_revalidation') or {
		panic('terminal revalidation proof missing')
	}
	projection := proof.object_value('pre_business_projection') or {
		panic('terminal pre-business projection missing')
	}
	canonical_projection := bin.canonical_json(projection)
	updated_projection := replace_canonical_root_member(canonical_projection, projection, key,
		replacement)
	canonical_proof := bin.canonical_json(proof)
	updated_proof := canonical_proof.replace_once('"pre_business_projection":${canonical_projection}',
		'"pre_business_projection":${updated_projection}')
	canonical_successor := bin.canonical_json(successor)
	updated_successor := canonical_successor.replace_once('"terminal_revalidation":${canonical_proof}',
		'"terminal_revalidation":${updated_proof}')
	updated_source := source.replace_once(canonical_successor, updated_successor)
	if updated_source == source {
		panic('terminal pre-business projection successor missing')
	}
	return refresh_terminal_revalidation_facts_digest(updated_source)
}

fn test_full_contract_report_is_green() {
	report := bin.run_contract_checks(automation_root()) or { panic(err) }
	assert report.schema_count == 22
	assert report.manifest_count == 2
	assert report.hygiene_files == 106
}

fn test_durable_git_sigchld_snapshot_uses_the_linked_posix_header_helper() {
	header := os.read_file(os.join_path(automation_root(), 'bin', 'provenance_native_nix.h')) or {
		panic(err)
	}
	live_state := os.read_file(os.join_path(automation_root(), 'bin', 'live_state.v')) or {
		panic(err)
	}
	posix_marker := '\n#else\n\n#include <dirent.h>'
	posix_offset := header.index(posix_marker) or { panic('POSIX native-header branch missing') }
	posix_suffix := '\n#endif\n\n#endif\n'
	assert header.ends_with(posix_suffix)
	posix_end := header.len - posix_suffix.len
	posix_branch := header[posix_offset..posix_end]
	expected_helper := 'static inline int tccbin_sigchld_read(struct sigaction *previous) {\n\tif (previous == NULL) {\n\t\terrno = EINVAL;\n\t\treturn -1;\n\t}\n\treturn sigaction(SIGCHLD, NULL, previous);\n}'
	assert header[..posix_offset].count('tccbin_sigchld_read') == 0
	assert posix_branch.count(expected_helper) == 1
	assert posix_branch.count('tccbin_sigchld_read') == 1
	assert posix_branch.count('#include <signal.h>') == 1
	assert header[posix_end..].count('tccbin_sigchld_read') == 0
	assert live_state.count('fn C.tccbin_sigchld_read(previous &C.sigaction) int') == 1
	assert live_state.count('C.tccbin_sigchld_read(&action)') == 2
	assert live_state.count('mut session DurableGitRunnerSession') == 12
	assert live_state.count('mut session &DurableGitRunnerSession') == 0
	assert !live_state.contains("@[c: 'sigaction']")
	assert !live_state.contains('durable_posix_sigaction')
	assert !live_state.contains('fn C.sigaction(')
}

fn test_native_lane_result_and_matrix_schemas_are_closed_and_cross_platform() {
	evidence := 'e'.repeat(64)
	sentinel := '{"probe_id":"patch-probes","lane_id":"expected=0","required":true,"status":"passed","expected_count":0,"evidence_sha256":"${evidence}","fallback_used":false,"object_linked":false,"consumer_group":"none"}'
	assert validate_schema_source('lane-result.schema.json', sentinel, 'lane-sentinel').len == 0
	blocked := sentinel.replace_once('"lane_id":"expected=0"', '"lane_id":"x64"').replace_once('"status":"passed"',
		'"status":"blocked"').replace_once('"expected_count":0', '"expected_count":1')
	assert validate_schema_source('lane-result.schema.json', blocked, 'lane-blocked').len == 0
	for status in ['skipped', 'not_run', 'neutral', 'waived'] {
		issues := validate_schema_source('lane-result.schema.json', sentinel.replace_once('"status":"passed"',
			'"status":"${status}"'), 'lane-legacy-${status}')
		assert issues.len == 1, '${status}: ${issues}'
	}

	fixture_subject := bin.parse_strict_json(schema_fixture('native-gate-subject.schema-fixture.json')) or {
		panic(err)
	}
	for target_id in ['linux-amd64', 'windows-amd64'] {
		profile := bin.parse_strict_json(t2a_profile_source(target_id)) or { panic(err) }
		profile_sha256 := bin.json_sha256(profile)
		producer_source := t2a_producer_observation_source(target_id, profile_sha256)
		producer := bin.parse_strict_json(producer_source) or { panic(err) }
		validator := t2b_toolchain_observation_source_with_profile(target_id,
			'${target_id}-synthetic-v1', profile_sha256, 'validator')
		subject_source := if target_id == 'linux-amd64' {
			bin.canonical_json(fixture_subject)
		} else {
			bin.canonical_json(fixture_subject).replace('linux-amd64', 'windows-amd64')
		}
		subject := bin.parse_strict_json(subject_source) or { panic(err) }
		lane := if target_id == 'windows-amd64' {
			'{"probe_id":"opaque-openlibm","lane_id":"x64-math","required":true,"status":"passed","expected_count":5,"evidence_sha256":"${evidence}","fallback_used":false,"object_linked":true,"consumer_group":"math"}'
		} else {
			blocked
		}
		matrix := bin.canonical_json(bin.parse_strict_json('{"schema_version":1,"subject":${bin.canonical_json(subject)},"subject_hash":"${bin.json_sha256(subject)}","producer_toolchain":{"profile_id":"${target_id}-synthetic-v1","profile_sha256":"${profile_sha256}","observation_sha256":"${bin.json_sha256(producer)}","observation_digest":"${(producer.object_value('observation_digest') or {
			panic('producer digest missing')
		}).string_value}"},"selected_run":{"run_id":7001,"run_attempt":1,"check_suite_id":7101},"validator_observation":${validator},"results":[${lane}]}') or {
			panic(err)
		})
		assert validate_schema_source('native-lane-matrix.schema.json', matrix,
			'matrix-${target_id}').len == 0
		with_digest := matrix[..matrix.len - 1] + ',"matrix_digest":"${'f'.repeat(64)}"}'
		assert validate_schema_source('native-lane-matrix.schema.json', with_digest,
			'matrix-digest-${target_id}').len == 1
	}
}

fn t2b_lane_array_source(prefix string, count int) string {
	mut lanes := []string{cap: count}
	for index in 0 .. count {
		lanes << '"${prefix}-${index:04}"'
	}
	return '[${lanes.join(',')}]'
}

fn t2b_manifest_with_probe_lanes(source string, probe_id string, lanes_source string) string {
	marker := '{"id": "${probe_id}", "required": true, "expected_lanes": ["native"],'
	assert source.count(marker) == 1
	return source.replace_once(marker,
		'{"id": "${probe_id}", "required": true, "expected_lanes": ${lanes_source},')
}

fn t2b_required_lane_error(source string) string {
	manifest := bin.parse_strict_json(source) or { panic(err) }
	bin.required_lane_pairs(manifest) or { return err.msg() }
	return ''
}

fn test_manifest_native_lane_cardinality_is_closed_at_1024_materialized_results() {
	linux := schema_fixture('manifest-complete.valid.json')
	empty_nonpatch := t2b_manifest_with_probe_lanes(linux, 'manifest-contract', '[]')
	empty_issues := validate_manifest_source(empty_nonpatch, 'native-lane-empty-nonpatch')
	assert empty_issues.len == 1, '${empty_issues}'
	assert empty_issues[0].path == '$/probes'
	assert empty_issues[0].message == 'only an explicitly empty patchset may have zero expected lanes'
	assert t2b_required_lane_error(empty_nonpatch) == 'only an explicitly empty patchset may have zero expected lanes'

	per_probe_overflow := t2b_manifest_with_probe_lanes(linux, 'manifest-contract',
		t2b_lane_array_source('single', 1025))
	per_probe_issues := validate_manifest_source(per_probe_overflow, 'native-lane-per-probe-1025')
	assert per_probe_issues.len == 1, '${per_probe_issues}'
	assert per_probe_issues[0].path == '$/probes/0/expected_lanes'
	assert per_probe_issues[0].message == 'array has more than 1024 items'
	assert t2b_required_lane_error(per_probe_overflow) == 'manifest materializes more than 1024 native lane results'

	distributed_overflow := t2b_manifest_with_probe_lanes(t2b_manifest_with_probe_lanes(linux,
		'manifest-contract', t2b_lane_array_source('first', 509)), 'source-provenance',
		t2b_lane_array_source('second', 509))
	distributed_issues := validate_manifest_source(distributed_overflow,
		'native-lane-distributed-1025')
	assert distributed_issues.len == 1, '${distributed_issues}'
	assert distributed_issues[0].path == '$/probes'
	assert distributed_issues[0].message == 'manifest materializes more than 1024 native lane results'
	assert t2b_required_lane_error(distributed_overflow) == 'manifest materializes more than 1024 native lane results'

	exact_bound := t2b_manifest_with_probe_lanes(t2b_manifest_with_probe_lanes(linux,
		'manifest-contract', t2b_lane_array_source('first', 508)), 'source-provenance',
		t2b_lane_array_source('second', 509))
	assert validate_manifest_source(exact_bound, 'native-lane-exact-1024').len == 0
	pairs := bin.required_lane_pairs(bin.parse_strict_json(exact_bound) or { panic(err) }) or {
		panic(err)
	}
	assert pairs.len == 1024
	assert pairs.filter(it == 'patch-probes/expected=0').len == 1
}

fn test_manifest_patch_probe_lane_cardinality_is_biconditional_and_never_serializes_sentinel() {
	linux := schema_fixture('manifest-complete.valid.json')
	linux_with_lane := linux.replace_once('"id": "patch-probes", "required": true, "expected_lanes": []',
		'"id": "patch-probes", "required": true, "expected_lanes": ["x64"]')
	linux_issues := validate_manifest_source(linux_with_lane,
		'patch-probes-nonempty-without-patches')
	assert linux_issues.any(it.path == '$/probes'
		&& it.message == 'patch-probes lanes must be empty exactly when the patch list is empty')
	linux_sentinel := linux.replace_once('"id": "patch-probes", "required": true, "expected_lanes": []',
		'"id": "patch-probes", "required": true, "expected_lanes": ["expected=0"]')
	sentinel_issues := validate_schema_source('bundle-manifest.schema.json', linux_sentinel,
		'patch-probes-sentinel-forbidden')
	assert sentinel_issues.any(it.path.contains('/expected_lanes/0'))

	windows := schema_fixture('manifest-windows-opaque.valid.json')
	windows_without_lanes := windows.replace_once('"id": "patch-probes", "required": true, "expected_lanes": ["x64", "i386"]',
		'"id": "patch-probes", "required": true, "expected_lanes": []')
	windows_issues := validate_manifest_source(windows_without_lanes,
		'patch-probes-empty-with-patches')
	assert windows_issues.any(it.path == '$/probes'
		&& it.message == 'patch-probes lanes must be empty exactly when the patch list is empty')
}

fn test_registry_has_exact_closed_inventory() {
	issues := bin.validate_registry(automation_root()) or { panic(err) }
	assert issues.len == 0
}

fn test_registry_rejects_every_semantic_tuple_drift() {
	source := os.read_file(os.join_path(automation_root(), 'targets.json')) or { panic(err) }
	mutations := [
		source.replace_once('"os": "freebsd"', '"os": "linux"'),
		source.replace_once('"abi": "elf"', '"abi": "glibc"'),
		source.replace_once('"architecture": "amd64"', '"architecture": "arm64"'),
		source.replace_once('.github/workflows/build-and-test.yml', '.github/workflows/other.yml'),
		source.replace_once('TCCBIN_FREEBSD_AMD64_PUBLISH_UNLOCKED',
			'TCCBIN_LINUX_AMD64_PUBLISH_UNLOCKED'),
		source.replace_once('"affected_targets": ["freebsd-amd64"]',
			'"affected_targets": ["linux-amd64"]'),
		source.replace_once('fdf5cdfea6ea84612e068bc3bea433dbba263404',
			'ece46f06fbe6eb701d52442f11dd59c48d166cae'),
		source.replace_once('"base_contract_sha": "7545e515b434cd399333d43659238427d72e22e7"',
			'"base_contract_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"'),
		source.replace_once('"parent_sha": "fdf5cdfea6ea84612e068bc3bea433dbba263404"',
			'"parent_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"'),
		source.replace_once('"path": "lib/openlibm.o"', '"path": "lib/other.o"'),
		source.replace_once('"architectures": ["x64"]', '"architectures": ["i386"]'),
	]
	for mutation in mutations {
		registry := bin.parse_strict_json(mutation) or { panic(err) }
		issues := bin.validate_registry_semantics(registry) or { panic(err) }
		assert issues.len > 0
	}
}

fn test_production_registry_locks_the_six_managed_baseline_activation_tuples() {
	registry_source := os.read_file(os.join_path(automation_root(), 'targets.json')) or {
		panic(err)
	}
	registry := bin.parse_strict_json(registry_source) or { panic(err) }
	managed := registry.object_value('managed_ci_targets') or { panic('managed targets missing') }
	expected := {
		'freebsd-amd64': [
			'e71cda6242e88e47312ca9bfc4548b0579636e0c',
			'9438879ad9906e970d45bafacf6ce2cc63ae4c53',
			'fdf5cdfea6ea84612e068bc3bea433dbba263404',
			'a9c2f15451a7e94261c6dd4d9e47cc3965414a2179d04af5902dffc9471a4db3',
		]
		'linux-amd64':   [
			'd6e7ac1b1bcc98aed734a6ecbfa8509f24606c74',
			'22851c0f356fefcb63718ce63d50a870150a491c',
			'ece46f06fbe6eb701d52442f11dd59c48d166cae',
			'bcdce1bea1facb24175229a16dc6e8a2c4210aafc05f0526b2728dc060223ed4',
		]
		'macos-amd64':   [
			'199fa78395ca413aac23d02ec69cc5e7b1d805a2',
			'db67711bfeb33be63dbc8eb03ecdddc2e127cc8c',
			'da8ac5a4369accc67c485191d02535d77718a1c8',
			'09dc54928f4690cfee7fd113de93f36479a40cabd10eeb3ee416a3175b42270a',
		]
		'macos-arm64':   [
			'1d0ad0ecf70a91a1df64cebf215e683b1d5aedb5',
			'96af5121f065310ba9168e3e2dc61adf340e2738',
			'274abd2466a14861b75e5b91fd946ad27d114499',
			'be45aaee1e65cc1ed2ee6bd2f121d72cbc248887ffa3d57f4b6d59cb6ea73525',
		]
		'openbsd-amd64': [
			'8c7d96c75ea8548f007432d70f1ae33cccd81838',
			'f75c4862711184c4c191a73e6f996eb421bd37b4',
			'45230fde96c17fff4baf37deb55e90803c043063',
			'873b62af697ba25f0abe5887ba05972a93bd48990233853b362bed6cb9137699',
		]
		'windows-amd64': [
			'86ae5844b8b56071b21ae3aa138b247d5eb9ddd9',
			'818d7794ebdf41de60e5679d485e3a5d49272171',
			'f7c7199bb87fda8b80b31fefa470b2efc952326b',
			'b5728124ecf8dc01e4f16cf4188411d6f633bb57997ef36df2dc5fd182e8535a',
		]
	}
	mut observed := map[string][]string{}
	for target in managed.array_value {
		id := (target.object_value('id') or { panic('target id missing') }).string_value
		activation := target.object_value('managed_baseline_activation') or {
			panic('managed baseline activation binding missing')
		}
		observed[id] = [
			(activation.object_value('base_sha') or { panic('base SHA missing') }).string_value,
			(activation.object_value('base_tree') or { panic('base tree missing') }).string_value,
			(activation.object_value('parent_sha') or { panic('parent SHA missing') }).string_value,
			(activation.object_value('base_manifest_sha256') or {
				panic('base manifest SHA-256 missing')
			}).string_value,
		]
		assert (activation.object_value('base_contract_repository') or {
			panic('base contract repository missing')
		}).string_value == 'vlang/v'
		assert (activation.object_value('base_contract_sha') or {
			panic('base contract SHA missing')
		}).string_value == '7545e515b434cd399333d43659238427d72e22e7'
		assert (activation.object_value('policy_path') or {
			panic('activation policy path missing')
		}).kind == .null_value
		assert (activation.object_value('policy_sha256') or {
			panic('activation policy hash missing')
		}).kind == .null_value
	}
	assert observed == expected

	half_pair := registry_source.replace_once('"policy_path": null,\n        "policy_sha256": null\n      },\n      "toolchain_profile"',
		'"policy_path": "baseline-activation/freebsd-amd64.policy.json",\n        "policy_sha256": null\n      },\n      "toolchain_profile"')
	half_pair_issues := validate_schema_source('targets.schema.json', half_pair,
		'managed-baseline-activation-half-pair')
	assert half_pair_issues.len == 1, '${half_pair_issues}'
	assert half_pair_issues[0].path == '$/managed_ci_targets/0/managed_baseline_activation/policy_sha256'
}

fn test_production_registry_locks_the_six_reviewed_legacy_base_literals() {
	registry := bin.parse_strict_json(os.read_file(os.join_path(automation_root(), 'targets.json')) or {
		panic(err)
	}) or { panic(err) }
	managed := registry.object_value('managed_ci_targets') or { panic('managed targets missing') }
	expected := {
		'freebsd-amd64': 'fdf5cdfea6ea84612e068bc3bea433dbba263404'
		'linux-amd64':   'ece46f06fbe6eb701d52442f11dd59c48d166cae'
		'macos-amd64':   'da8ac5a4369accc67c485191d02535d77718a1c8'
		'macos-arm64':   '274abd2466a14861b75e5b91fd946ad27d114499'
		'openbsd-amd64': '45230fde96c17fff4baf37deb55e90803c043063'
		'windows-amd64': 'f7c7199bb87fda8b80b31fefa470b2efc952326b'
	}
	mut observed := map[string]string{}
	for target in managed.array_value {
		id := (target.object_value('id') or { panic('target id missing') }).string_value
		onboarding := target.object_value('legacy_onboarding') or {
			panic('legacy onboarding binding missing')
		}
		observed[id] = (onboarding.object_value('base_sha') or { panic('legacy base SHA missing') }).string_value
		assert (onboarding.object_value('policy_path') or { panic('policy path missing') }).kind == .null_value
		assert (onboarding.object_value('policy_sha256') or { panic('policy hash missing') }).kind == .null_value
		toolchain := target.object_value('toolchain_profile') or {
			panic('toolchain profile binding missing')
		}
		assert (toolchain.object_value('profile_id') or { panic('profile ID missing') }).kind == .null_value
		assert (toolchain.object_value('profile_path') or { panic('profile path missing') }).kind == .null_value
		assert (toolchain.object_value('profile_sha256') or { panic('profile hash missing') }).kind == .null_value
	}
	assert observed == expected
}

fn test_toolchain_profile_registry_is_dormant_and_both_wire_schemas_are_closed() {
	registry_source := os.read_file(os.join_path(automation_root(), 'targets.json')) or {
		panic(err)
	}
	production_issues := validate_schema_source('targets.schema.json', registry_source,
		'toolchain-profile-production-null-bindings')
	assert production_issues.len == 0, '${production_issues}'
	binding_marker := '"toolchain_profile": {\n        "profile_id": null,\n        "profile_path": null,\n        "profile_sha256": null\n      }'
	assert registry_source.count(binding_marker) == 6
	half_binding := registry_source.replace_once(binding_marker,
		'"toolchain_profile": {\n        "profile_id": "freebsd-amd64-synthetic-v1",\n        "profile_path": null,\n        "profile_sha256": null\n      }')
	half_issues := validate_schema_source('targets.schema.json', half_binding,
		'toolchain-profile-half-binding')
	assert half_issues.len == 1, '${half_issues}'
	assert half_issues[0].path == '$/managed_ci_targets/0/toolchain_profile'

	temporary := os.join_path(os.temp_dir(), 'tccbin-toolchain-schema-${os.getpid()}')
	os.rmdir_all(temporary) or {}
	os.mkdir_all(temporary) or { panic(err) }
	defer {
		os.rmdir_all(temporary) or {}
	}
	os.cp_all(os.join_path(automation_root(), 'schemas'), os.join_path(temporary, 'schemas'), true) or {
		panic(err)
	}
	for target_id in ['freebsd-amd64', 'linux-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64'] {
		profile_source := synthetic_toolchain_profile_source(target_id)
		profile_issues := validate_schema_source('toolchain-profile.schema.json', profile_source,
			'toolchain-profile-${target_id}')
		assert profile_issues.len == 0, '${profile_issues}'
		profile := bin.parse_strict_json(profile_source) or { panic(err) }
		profile_sha256 := bin.json_sha256(profile)
		profile_id := '${target_id}-synthetic-v1'
		profile_relative_path := 'toolchain-profiles/${target_id}.profile.json'
		profile_path := os.join_path(temporary, profile_relative_path)
		os.mkdir_all(os.dir(profile_path)) or { panic(err) }
		os.write_file(profile_path, profile_source) or { panic(err) }
		resolved_registry := registry_with_toolchain_profile(registry_source, target_id,
			profile_id, profile_relative_path, profile_sha256)
		os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry) or { panic(err) }
		registry_issues := bin.validate_registry(temporary) or { panic(err) }
		assert registry_issues.len == 0, '${target_id}: ${registry_issues}'
		for phase in ['producer', 'validator'] {
			observation_source := synthetic_toolchain_observation_source(target_id, profile_sha256,
				phase)
			observation_issues := validate_schema_source('toolchain-observation.schema.json',
				observation_source, 'toolchain-observation-${target_id}-${phase}')
			assert observation_issues.len == 0, '${observation_issues}'
			observation_path := os.join_path(temporary,
				'toolchain-observation-${target_id}-${phase}.json')
			os.write_file(observation_path, observation_source) or { panic(err) }
			authenticated := bin.authenticate_toolchain_observation_file(temporary, target_id,
				observation_path) or { panic(err) }
			assert authenticated.phase == phase
			assert authenticated.profile_sha256 == profile_sha256
		}
	}

	freebsd_profile_source := synthetic_toolchain_profile_source('freebsd-amd64')
	freebsd_profile := bin.parse_strict_json(freebsd_profile_source) or { panic(err) }
	freebsd_producer := freebsd_profile.object_value('producer') or { panic('producer missing') }
	freebsd_guest_role := bin.canonical_json(freebsd_producer.array_value[0])
	freebsd_host_role := bin.canonical_json(freebsd_producer.array_value[1])
	freebsd_producer_marker := '"producer":[${freebsd_guest_role},${freebsd_host_role}]'
	freebsd_extra_role := freebsd_host_role.replace_once('"role_id":"bundle-builder-host"',
		'"role_id":"bundle-builder-third"')
	for mutation in [
		[
			freebsd_profile_source.replace_once(freebsd_producer_marker,
				'"producer":[${freebsd_host_role}]'),
			'toolchain phase strategy topology differs from the exact managed target',
		],
		[
			freebsd_profile_source.replace_once(freebsd_producer_marker,
				'"producer":[${freebsd_host_role},${freebsd_guest_role}]'),
			'toolchain producer roles must be in strict lexical order',
		],
		[
			freebsd_profile_source.replace_once(freebsd_producer_marker,
				'"producer":[${freebsd_guest_role},${freebsd_host_role},${freebsd_extra_role}]'),
			'toolchain phase strategy topology differs from the exact managed target',
		],
		[
			freebsd_profile_source.replace_once('"identity_strategy":"cpa-host"',
				'"identity_strategy":"cpa-guest"'),
			'toolchain phase strategy topology differs from the exact managed target',
		],
		[
			synthetic_toolchain_profile_source('freebsd-amd64').replace_once('"value":"${'a'.repeat(40)}"',
				'"value":"not-a-commit"'),
			'toolchain action identity must be a full lowercase commit SHA',
		],
		[
			synthetic_toolchain_profile_source('freebsd-amd64').replace_once('{"match":"release-compatible","name":"observed_release","value":"15.1"}',
				'{"match":"present","name":"observed_release"}'),
			'toolchain identity policy differs from the exact strategy requirements',
		],
		[
			synthetic_toolchain_profile_source('freebsd-amd64').replace_once('{"match":"exact","name":"requested_release","value":"15.1"}',
				'{"match":"exact","name":"requested_release","value":"15.2"}'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('windows-amd64').replace_once('"value":"UCRT64"',
				'"value":"MINGW64"'),
			'MSYS2 toolchain identity must require UCRT64',
		],
		[
			synthetic_toolchain_profile_source('windows-amd64').replace_once('"value":"${'b'.repeat(40)}"',
				'"value":"not-a-commit"'),
			'toolchain action identity must be a full lowercase commit SHA',
		],
		[
			synthetic_toolchain_profile_source('linux-amd64').replace_once('"value":"ubuntu-24.04"',
				'"value":"ubuntu-22.04"'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('linux-amd64').replace_once('{"match":"exact","name":"os","value":"linux"}',
				'{"match":"exact","name":"os","value":"macos"}'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('macos-amd64').replace_once('"value":"macos-15-intel"',
				'"value":"macos-15"'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('macos-arm64').replace_once('"value":"arm64"',
				'"value":"amd64"'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('freebsd-amd64').replace_once('"value":"freebsd"',
				'"value":"openbsd"'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('openbsd-amd64').replace_once('"value":"7.8"',
				'"value":"15.1"'),
			'toolchain identity policy differs from the exact managed target values',
		],
		[
			synthetic_toolchain_profile_source('windows-amd64').replace_once('"value":"gcc"',
				'"value":"msvc"'),
			'toolchain identity policy differs from the exact managed target values',
		],
	] {
		profile := bin.parse_strict_json(mutation[0]) or { panic(err) }
		target_id := (profile.object_value('target_id') or { panic('target missing') }).string_value
		profile_sha256 := bin.json_sha256(profile)
		profile_relative_path := 'toolchain-profiles/${target_id}.profile.json'
		os.write_file(os.join_path(temporary, profile_relative_path), mutation[0]) or { panic(err) }
		resolved_registry := registry_with_toolchain_profile(registry_source, target_id,
			'${target_id}-synthetic-v1', profile_relative_path, profile_sha256)
		os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry) or { panic(err) }
		registry_issues := bin.validate_registry(temporary) or { panic(err) }
		assert registry_issues.len == 1, '${registry_issues}'
		assert registry_issues[0].message == mutation[1]
	}

	profile_source := synthetic_toolchain_profile_source('linux-amd64')
	for index, mutation in [
		profile_source.replace_once('"schema_version":1', '"schema_version":2'),
		profile_source.replace_once('"target_id":"linux-amd64"',
			'"target_id":"linux-amd64","timestamp":"2026-08-09T00:00:00Z"'),
	] {
		issues := validate_schema_source('toolchain-profile.schema.json', mutation,
			'toolchain-profile-closed-${index}')
		assert issues.len == 1, '${issues}'
	}
	profile := bin.parse_strict_json(profile_source) or { panic(err) }
	observation_source := synthetic_toolchain_observation_source('linux-amd64',
		bin.json_sha256(profile), 'producer')
	for index, member in [
		'"timestamp":"2026-08-09T00:00:00Z"',
		'"run_id":1',
		'"working_path":"tmp/toolchain"',
	] {
		mutation := observation_source.replace_once('{', '{${member},')
		issues := validate_schema_source('toolchain-observation.schema.json', mutation,
			'toolchain-observation-closed-${index}')
		assert issues.len == 1, '${issues}'
	}
}

fn test_legacy_onboarding_registry_is_dormant_until_one_canonical_policy_is_reviewed() {
	root := automation_root()
	temporary := os.join_path(os.temp_dir(), 'tccbin-onboarding-policy-${os.getpid()}')
	os.rmdir_all(temporary) or {}
	os.mkdir_all(temporary) or { panic(err) }
	defer {
		os.rmdir_all(temporary) or {}
	}
	os.cp_all(os.join_path(root, 'schemas'), os.join_path(temporary, 'schemas'), true) or {
		panic(err)
	}
	registry_source := os.read_file(os.join_path(root, 'targets.json')) or { panic(err) }
	half_pair := registry_source.replace_once('"policy_path": null,\n        "policy_sha256": null',
		'"policy_path": "onboarding/freebsd-amd64.policy.json",\n        "policy_sha256": null')
	half_pair_issues := validate_schema_source('targets.schema.json', half_pair,
		'legacy-onboarding-half-pair')
	assert half_pair_issues.len == 1, '${half_pair_issues}'

	manifest := bin.parse_strict_json(schema_fixture_with_resolved_producer('manifest-complete.valid.json',
		'linux-amd64')) or { panic(err) }
	policy := bin.legacy_onboarding_policy_projection(manifest) or { panic(err) }
	policy_source := bin.canonical_json(policy)
	policy_hash := bin.legacy_onboarding_policy_sha256(manifest) or { panic(err) }
	assert policy_hash == sha256.sum256(policy_source.bytes()).hex()
	linux_policy_issues := validate_schema_source('onboarding-policy.schema.json', policy_source,
		'legacy-onboarding-linux-policy')
	assert linux_policy_issues.len == 0
	windows_manifest := bin.parse_strict_json(schema_fixture_with_resolved_producer('manifest-windows-opaque.valid.json',
		'windows-amd64')) or { panic(err) }
	windows_policy := bin.legacy_onboarding_policy_projection(windows_manifest) or { panic(err) }
	windows_policy_source := bin.canonical_json(windows_policy)
	windows_policy_issues := validate_schema_source('onboarding-policy.schema.json',
		windows_policy_source, 'legacy-onboarding-windows-policy')
	assert windows_policy_issues.len == 0
	assert (windows_policy.object_value('patches') or { panic('patches missing') }).array_value.len == 9
	assert (windows_policy.object_value('transforms') or { panic('transforms missing') }).array_value.len == 2
	assert (windows_policy.object_value('header_effects') or { panic('header effects missing') }).array_value.len == 3
	assert (windows_policy.object_value('integrations') or { panic('integrations missing') }).array_value.len == 1
	assert (windows_policy.object_value('probes') or { panic('probes missing') }).array_value.len == 23
	windows_payload := windows_policy.object_value('payload_policy') or {
		panic('payload policy missing')
	}
	assert (windows_payload.object_value('overlays') or { panic('overlays missing') }).array_value.len == 1
	assert (windows_payload.object_value('inventory') or { panic('inventory missing') }).array_value.len == 2
	assert (windows_payload.object_value('outputs') or { panic('outputs missing') }).array_value.len == 1
	assert policy_source.count('"schema_version":1') == 1
	version_issues := validate_schema_source('onboarding-policy.schema.json', policy_source.replace_once('"schema_version":1',
		'"schema_version":2'), 'legacy-onboarding-schema-version')
	assert version_issues.len == 1
	assert policy_source.count('https://repo.or.cz/tinycc.git') == 1
	repository_issues := validate_schema_source('onboarding-policy.schema.json',
		policy_source.replace_once('https://repo.or.cz/tinycc.git', 'x'),
		'legacy-onboarding-source-repository')
	assert repository_issues.len == 1
	policy_relative_path := 'onboarding/linux-amd64.policy.json'
	policy_path := os.join_path(temporary, policy_relative_path)
	os.mkdir_all(os.dir(policy_path)) or { panic(err) }
	os.write_file(policy_path, policy_source) or { panic(err) }
	legacy_registry := registry_source.replace_once('"base_sha": "ece46f06fbe6eb701d52442f11dd59c48d166cae",\n        "policy_path": null,\n        "policy_sha256": null',
		'"base_sha": "ece46f06fbe6eb701d52442f11dd59c48d166cae",\n        "policy_path": "${policy_relative_path}",\n        "policy_sha256": "${policy_hash}"')
	profile_source := t2a_profile_source('linux-amd64')
	profile := bin.parse_strict_json(profile_source) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	profile_relative_path := 'toolchain-profiles/linux-amd64.profile.json'
	profile_path := os.join_path(temporary, profile_relative_path)
	os.mkdir_all(os.dir(profile_path)) or { panic(err) }
	os.write_file(profile_path, profile_source) or { panic(err) }
	resolved_registry := registry_with_toolchain_profile(legacy_registry, 'linux-amd64',
		'linux-amd64-synthetic-v1', profile_relative_path, profile_sha256)
	os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry) or { panic(err) }
	issues := bin.validate_registry(temporary) or { panic(err) }
	assert issues.len == 0, '${issues}'
	mismatched_policy_source := policy_source.replace_once('"profile_id":"linux-amd64-synthetic-v1"',
		'"profile_id":"linux-amd64-synthetic-v2"')
	mismatched_policy := bin.parse_strict_json(mismatched_policy_source) or { panic(err) }
	mismatched_policy_hash := bin.json_sha256(mismatched_policy)
	os.write_file(policy_path, mismatched_policy_source) or { panic(err) }
	os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry.replace_once(policy_hash,
		mismatched_policy_hash)) or { panic(err) }
	mismatched_policy_issues := bin.validate_registry(temporary) or { panic(err) }
	assert mismatched_policy_issues.len == 1, '${mismatched_policy_issues}'
	assert mismatched_policy_issues[0].message == 'legacy onboarding policy toolchain differs from the reviewed target profile'

	noncanonical := '${policy_source}\n'
	os.write_file(policy_path, noncanonical) or { panic(err) }
	os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry) or { panic(err) }
	noncanonical_issues := bin.validate_registry(temporary) or { panic(err) }
	assert noncanonical_issues.len == 1, '${noncanonical_issues}'
	assert noncanonical_issues[0].message == 'legacy onboarding policy bytes must be exact canonical JSON'
}

fn test_managed_baseline_activation_policy_loader_is_dormant_and_closed() {
	root := automation_root()
	temporary := os.join_path(os.temp_dir(), 'tccbin-baseline-activation-policy-${os.getpid()}')
	os.rmdir_all(temporary) or {}
	os.mkdir_all(temporary) or { panic(err) }
	defer {
		os.rmdir_all(temporary) or {}
	}
	os.cp_all(os.join_path(root, 'schemas'), os.join_path(temporary, 'schemas'), true) or {
		panic(err)
	}
	registry_source := os.read_file(os.join_path(root, 'targets.json')) or { panic(err) }
	evidence_fixture := managed_baseline_evidence_fixture(schema_fixture_with_resolved_producer('manifest-complete.valid.json',
		'linux-amd64'))
	manifest := bin.parse_strict_json(evidence_fixture.manifest_source) or { panic(err) }
	policy := bin.managed_baseline_activation_policy_projection(manifest, evidence_fixture.evidence) or {
		panic(err)
	}
	policy_source := bin.canonical_json(policy)
	policy_hash := bin.json_sha256(policy)
	policy_relative_path := 'baseline-activation/linux-amd64.policy.json'
	policy_path := os.join_path(temporary, policy_relative_path)
	os.mkdir_all(os.dir(policy_path)) or { panic(err) }
	os.write_file(policy_path, policy_source) or { panic(err) }

	profile_source := t2a_profile_source('linux-amd64')
	profile := bin.parse_strict_json(profile_source) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	profile_relative_path := 'toolchain-profiles/linux-amd64.profile.json'
	profile_path := os.join_path(temporary, profile_relative_path)
	os.mkdir_all(os.dir(profile_path)) or { panic(err) }
	os.write_file(profile_path, profile_source) or { panic(err) }
	with_profile := registry_with_toolchain_profile(registry_source, 'linux-amd64',
		'linux-amd64-synthetic-v1', profile_relative_path, profile_sha256)
	resolved_registry := registry_with_managed_baseline_activation_policy(with_profile,
		'linux-amd64', policy_relative_path, policy_hash)
	os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry) or { panic(err) }
	issues := bin.validate_registry(temporary) or { panic(err) }
	assert issues.len == 0, '${issues}'
	mut missing_evidence_keys := policy.object_keys.clone()
	mut missing_evidence_values := policy.object_values.clone()
	evidence_index := missing_evidence_keys.index('source_commit_evidence')
	assert evidence_index >= 0
	missing_evidence_keys.delete(evidence_index)
	missing_evidence_values.delete(evidence_index)
	missing_evidence_policy := bin.JsonValue{
		kind:          .object
		object_keys:   missing_evidence_keys
		object_values: missing_evidence_values
	}
	missing_evidence_issues := validate_schema_source('onboarding-policy.schema.json',
		bin.canonical_json(missing_evidence_policy), 'baseline-activation-v2-missing-evidence')
	assert missing_evidence_issues.len > 0
	mut legacy_with_evidence_values := policy.object_values.clone()
	legacy_with_evidence_values[0] = bin.JsonValue{
		kind:      .integer
		int_value: 1
	}
	legacy_with_evidence_policy := bin.JsonValue{
		kind:          .object
		object_keys:   policy.object_keys.clone()
		object_values: legacy_with_evidence_values
	}
	legacy_with_evidence_issues := validate_schema_source('onboarding-policy.schema.json',
		bin.canonical_json(legacy_with_evidence_policy), 'legacy-v1-with-source-evidence')
	assert legacy_with_evidence_issues.len > 0

	wrong_path_registry := resolved_registry.replace_once(policy_relative_path,
		'baseline-activation/freebsd-amd64.policy.json')
	os.write_file(os.join_path(temporary, 'targets.json'), wrong_path_registry) or { panic(err) }
	wrong_path_issues := bin.validate_registry(temporary) or { panic(err) }
	assert wrong_path_issues.len == 1, '${wrong_path_issues}'
	assert wrong_path_issues[0].message == 'managed target baseline activation policy path is not exact'

	os.write_file(policy_path, '${policy_source}\n') or { panic(err) }
	os.write_file(os.join_path(temporary, 'targets.json'), resolved_registry) or { panic(err) }
	noncanonical_issues := bin.validate_registry(temporary) or { panic(err) }
	assert noncanonical_issues.len == 1, '${noncanonical_issues}'
	assert noncanonical_issues[0].message == 'managed baseline activation policy bytes must be exact canonical JSON'
}

fn test_legacy_onboarding_policy_projection_excludes_only_dynamic_identity_and_bytes() {
	source := schema_fixture_with_resolved_producer('manifest-complete.valid.json', 'linux-amd64')
	manifest := bin.parse_strict_json(source) or { panic(err) }
	baseline := bin.legacy_onboarding_policy_sha256(manifest) or { panic(err) }
	dynamic_mutation := source.replace_once('"contract_repository": "GGRei/v"',
		'"contract_repository": "vlang/v"').replace_once('"contract_sha": "${'a'.repeat(40)}"',
		'"contract_sha": "${'b'.repeat(40)}"').replace_once('"contract_mode": "fork-dry-run"',
		'"contract_mode": "production"').replace_once('"v_source_sha": "${'b'.repeat(40)}"',
		'"v_source_sha": "${'c'.repeat(40)}"').replace('"sha": "${'c'.repeat(40)}"',
		'"sha": "${'9'.repeat(40)}"').replace_once('"tree": "${'d'.repeat(40)}"',
		'"tree": "${'e'.repeat(40)}"').replace_once('"compiler_version":"gcc 19.1.7"',
		'"compiler_version":"gcc 15.0.0"').replace_once('"sha256": "${'1'.repeat(64)}"',
		'"sha256": "${'6'.repeat(64)}"').replace_once('"sha256": "${'3'.repeat(64)}"',
		'"sha256": "${'7'.repeat(64)}"').replace_once('"sha256": "${'4'.repeat(64)}"',
		'"sha256": "${'8'.repeat(64)}"').replace('"status": "complete"', '"status": "incomplete"').replace_once('"provenance_status": "complete"',
		'"provenance_status": "incomplete"')
	dynamic_manifest := bin.parse_strict_json(dynamic_mutation) or { panic(err) }
	assert bin.legacy_onboarding_policy_sha256(dynamic_manifest) or { panic(err) } == baseline
	static_manifest := bin.parse_strict_json(source.replace_once('"profile_id": "linux-amd64-synthetic-v1"',
		'"profile_id": "linux-amd64-synthetic-v2"')) or { panic(err) }
	assert bin.legacy_onboarding_policy_sha256(static_manifest) or { panic(err) } != baseline
	branch_manifest := bin.parse_strict_json(source.replace_once('"branch": "thirdparty-linux-amd64"',
		'"branch": "thirdparty-linux-amd64-review"')) or { panic(err) }
	assert bin.legacy_onboarding_policy_sha256(branch_manifest) or { panic(err) } != baseline
	static_mutations := [
		source.replace_once('"schema_version": 1', '"schema_version": 2'),
		source.replace_once('"contract_version": 1', '"contract_version": 2'),
		source.replace_once('"id": "tinycc"', '"id": "tinycc-review"'),
		source.replace_once('https://repo.or.cz/tinycc.git', 'https://example.invalid/tinycc.git'),
		source.replace_once('"ref": "mob"', '"ref": "reviewed-mob"'),
		source.replace_once('"path": "build.sh"', '"path": "reviewed/build.sh"'),
		source.replace_once('"version": 1', '"version": 2'),
		source.replace_once('"profile_sha256": "', '"profile_sha256": "f'),
		source.replace_once('"oracle": "strict schema and semantic validation"',
			'"oracle": "reviewed schema and semantic validation"'),
		source.replace_once('"affected_targets": ["linux-amd64"]',
			'"affected_targets": ["linux-amd64","freebsd-amd64"]'),
		source.replace_once('"path": "src/tcc.c"', '"path": "src/reviewed-tcc.c"'),
		source.replace_once('"kind": "file"', '"kind": "executable"'),
		source.replace_once('"git_mode": "100644"', '"git_mode": "100755"'),
		source.replace_once('"role": "compiler-source"', '"role": "reviewed-compiler-source"'),
		source.replace_once('"opaque": false', '"opaque": true'),
		source.replace_once('"repository": "TinyCC/tinycc"',
			'"repository": "TinyCC/reviewed-tinycc"'),
		source.replace_once('"source_path": "tcc.c"', '"source_path": "reviewed/tcc.c"'),
		source.replace_once('"license": "LGPL-2.1-or-later"',
			'"license": "LGPL-2.1-or-later reviewed"'),
	]
	for mutation in static_mutations {
		mutated := bin.parse_strict_json(mutation) or { panic(err) }
		assert bin.legacy_onboarding_policy_sha256(mutated) or { panic(err) } != baseline
	}
	windows_source := schema_fixture_with_resolved_producer('manifest-windows-opaque.valid.json',
		'windows-amd64')
	windows_manifest := bin.parse_strict_json(windows_source) or { panic(err) }
	windows_baseline := bin.legacy_onboarding_policy_sha256(windows_manifest) or { panic(err) }
	for mutation in [
		windows_source.replace_once('"id": "patch-0001"', '"id": "patch-0001-reviewed"'),
		windows_source.replace_once('"id": "vlang-header-compat"',
			'"id": "vlang-header-compat-reviewed"'),
		windows_source.replace_once('"id": "header-gmtime-s"', '"id": "header-gmtime-s-reviewed"'),
		windows_source.replace_once('"id": "bdwgc-v-integration"',
			'"id": "bdwgc-v-integration-reviewed"'),
		windows_source.replace_once('"path": "include/winapi/synchapi.h"',
			'"path": "include/winapi/reviewed-synchapi.h"'),
	] {
		mutated := bin.parse_strict_json(mutation) or { panic(err) }
		assert bin.legacy_onboarding_policy_sha256(mutated) or { panic(err) } != windows_baseline
	}
	policy := bin.legacy_onboarding_policy_projection(manifest) or { panic(err) }
	payload_policy := policy.object_value('payload_policy') or { panic('payload policy missing') }
	inventory := payload_policy.object_value('inventory') or { panic('inventory policy missing') }
	assert inventory.array_value.len == 1
	projected_inventory := inventory.array_value[0]
	provenance := projected_inventory.object_value('provenance') or {
		panic('projected provenance missing')
	}
	for omitted in ['sha256', 'status', 'sha'] {
		assert omitted !in projected_inventory.object_keys
		assert omitted !in provenance.object_keys
	}
}

fn test_candidate_compose_cli_has_an_explicit_mode_and_no_stdout_result_protocol() {
	assert bin.parse_candidate_transition_kind('monthly') or { panic(err) } == .monthly
	assert bin.parse_candidate_transition_kind('legacy-onboard') or { panic(err) } == .legacy_onboard
	assert bin.parse_candidate_transition_kind('baseline-activate') or { panic(err) } == .baseline_activate
	mut rejected := ''
	bin.parse_candidate_transition_kind('legacy') or { rejected = err.msg() }
	assert rejected == 'candidate transition kind must be monthly, legacy-onboard, or baseline-activate'
	source := os.read_file(os.join_path(automation_root(), 'bin', 'cmd', 'main.v')) or {
		panic(err)
	}
	start := source.index("\t\t'candidate-compose' {") or { panic('candidate compose missing') }
	finish_relative := source[start..].index("\t\t'issue-dry-run' {") or {
		panic('candidate compose terminator missing')
	}
	block := source[start..start + finish_relative]
	assert block.contains('if os.args.len != 9')
	assert block.contains('<target-id> <monthly|legacy-onboard|baseline-activate>')
	assert block.count('bin.compose_candidate_for_execution(') == 1
	assert block.split_into_lines().all(!it.trim_space().starts_with('print'))
	assert source.contains('candidate-preflight <target-id> <monthly|legacy-onboard|baseline-activate>')
	composition_source := os.read_file(os.join_path(automation_root(), 'bin',
		'candidate_composition.v')) or { panic(err) }
	assert composition_source.count("['hash-object', '-w', '--no-filters', '--',") == 2
	assert composition_source.count("['hash-object', '-w', '--',") == 0
	assert composition_source.count('if roots_overlap(manifest_path, root)') == 1
	assert !composition_source.contains('manifest_path.starts_with(')
}

fn manifest_with_source_ids(target_id string, source_ids []string) string {
	mut source := schema_fixture('manifest-complete.valid.json')
	source = source.replace_once('"target_id": "linux-amd64"', '"target_id": "${target_id}"')
		.replace_once('"branch": "thirdparty-linux-amd64"', '"branch": "thirdparty-${target_id}"').replace_once('"affected_targets": ["linux-amd64"]',
		'"affected_targets": ["${target_id}"]')
	mut entries := []string{}
	for source_id in source_ids {
		repository, reference, sha, tree := match source_id {
			'tinycc' {
				'https://repo.or.cz/tinycc.git', 'mob', 'c'.repeat(40), 'd'.repeat(40)
			}
			'bdwgc' {
				'https://github.com/ivmai/bdwgc.git', 'master', 'e'.repeat(40), 'f'.repeat(40)
			}
			'libatomic_ops' {
				'https://github.com/bdwgc/libatomic_ops.git', 'master', '1'.repeat(40), '2'.repeat(40)
			}
			else {
				'https://repo.or.cz/tinycc.git', 'mob', 'c'.repeat(40), 'd'.repeat(40)
			}
		}
		entries << '    {"id":"${source_id}","repository":"${repository}","ref":"${reference}","sha":"${sha}","tree":"${tree}"}'
	}
	sources_start := source.index('  "sources": [') or { panic('sources marker missing') }
	recipe_start := source.index('  "recipe":') or { panic('recipe marker missing') }
	source = source[..sources_start] + '  "sources": [\n${entries.join(',\n')}\n  ],\n' +
		source[recipe_start..]
	return source
}

fn manifest_for_target(target_id string) string {
	source_ids := match target_id {
		'freebsd-amd64', 'linux-amd64' { ['tinycc', 'bdwgc'] }
		'macos-amd64', 'macos-arm64' { ['tinycc', 'bdwgc', 'libatomic_ops'] }
		'openbsd-amd64' { ['tinycc'] }
		else { panic('non-Windows target expected') }
	}
	return manifest_with_source_ids(target_id, source_ids)
}

fn macos_manifest_with_symlink(path string, target string) string {
	mut source := manifest_for_target('macos-amd64')
	target_value := bin.JsonValue{
		kind:         .string_value
		string_value: target
	}
	source = source.replace_once('"path": "src/tcc.c"', '"path": "${path}"')
		.replace_once('"kind": "file"', '"kind": "symlink"').replace_once('"git_mode": "100644"',
		'"git_mode": "120000"').replace_once('"sha256": "${'3'.repeat(64)}"',
		'"sha256": "${sha256.sum256(target.bytes()).hex()}"').replace_once('"symlink_target": null',
		'"symlink_target": ${bin.canonical_json(target_value)}')
	return source
}

fn test_manifest_source_matrix_transforms_symlinks_and_control_boundaries_are_closed() {
	complete := schema_fixture('manifest-complete.valid.json')
	windows := schema_fixture('manifest-windows-opaque.valid.json')
	for target_id in ['freebsd-amd64', 'linux-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64'] {
		issues := validate_manifest_source(manifest_for_target(target_id),
			'source-matrix-${target_id}')
		assert issues.len == 0, '${target_id}: ${issues}'
	}
	assert validate_manifest_source(windows, 'source-matrix-windows-amd64').len == 0
	source_matrix_mutations := [
		manifest_with_source_ids('linux-amd64', ['tinycc']),
		manifest_with_source_ids('linux-amd64', ['tinycc', 'bdwgc', 'libatomic_ops']),
		manifest_with_source_ids('linux-amd64', ['bdwgc', 'tinycc']),
		manifest_with_source_ids('linux-amd64', ['tinycc-alias', 'bdwgc']),
		complete.replace_once('"ref": "mob"', '"ref": "main"'),
		complete.replace_once('https://repo.or.cz/tinycc.git',
			'https://github.com/TinyCC/tinycc.git'),
	]
	for index, mutation in source_matrix_mutations {
		issues := validate_manifest_source(mutation, 'source-matrix-reject-${index}')
		assert issues.len > 0, '${issues}'
	}
	mac_arm_unresolved_atomic := manifest_for_target('macos-arm64').replace_once('"sha":"${'1'.repeat(40)}","tree":"${'2'.repeat(40)}"',
		'"sha":null,"tree":null').replace_once('"provenance_status": "complete"',
		'"provenance_status": "incomplete"')
	assert validate_manifest_source(mac_arm_unresolved_atomic, 'mac-arm-unresolved-atomic').len == 0
	for source in [
		macos_manifest_with_symlink('lib/libc.dylib', '/System/DriverKit/usr/lib/libSystem.dylib'),
		macos_manifest_with_symlink('lib/libgc.dylib', 'libgc.1.dylib'),
		macos_manifest_with_symlink('lib/libgc.la', '../libgc.la'),
	] {
		issues := validate_manifest_source(source,
			'macos-symlink-${sha256.sum256(source.bytes()).hex()[..8]}')
		assert issues.len == 0, '${issues}'
	}
	invalid_absolute := macos_manifest_with_symlink('lib/libc.dylib', '/usr/lib/libSystem.dylib')
	invalid_absolute_issues := validate_manifest_source(invalid_absolute,
		'macos-symlink-schema-invalid-absolute')
	assert invalid_absolute_issues.len == 1, '${invalid_absolute_issues}'
	assert invalid_absolute_issues[0].path == '$/inventory/0/symlink_target'
	assert invalid_absolute_issues[0].message == 'expected exactly one oneOf branch, got 0'
	wrong_path_absolute := macos_manifest_with_symlink('lib/libgc.dylib',
		'/System/DriverKit/usr/lib/libSystem.dylib')
	wrong_path_absolute_issues := validate_manifest_source(wrong_path_absolute,
		'macos-symlink-semantic-wrong-path')
	assert wrong_path_absolute_issues.len == 1, '${wrong_path_absolute_issues}'
	assert wrong_path_absolute_issues[0].path == '$/inventory'
	assert wrong_path_absolute_issues[0].message == 'symlink target is not allowed for this target and path'
	escaping_relative := macos_manifest_with_symlink('lib/libgc.la', '../../outside')
	escaping_relative_issues := validate_manifest_source(escaping_relative,
		'macos-symlink-semantic-escape')
	assert escaping_relative_issues.len == 1, '${escaping_relative_issues}'
	assert escaping_relative_issues[0].path == '$/inventory'
	assert escaping_relative_issues[0].message == 'symlink target is not allowed for this target and path'

	transform_mutations := [
		windows.replace_once('"id": "vlang-header-compat"', '"id": "other-header-compat"'),
		windows.replace_once('"path": "vlang-header-compat.patch"', '"path": "other.patch"'),
		windows.replace_once('"owner": "bundle-overlay"', '"owner": "v-libgc"'),
		windows.replace_once('"order": 10', '"order": 12'),
		windows.replace_once('"apply_stage": "bundle-payload-post-copy"',
			'"apply_stage": "v-libgc-source-prebuild"'),
		windows.replace_once('"header-gmtime-s", "header-condition-variable", "header-faststorefence"',
			'"header-gmtime-s", "header-faststorefence"'),
	]
	for index, mutation in transform_mutations {
		issues := validate_manifest_source(mutation, 'transform-tuple-${index}')
		assert issues.len > 0, '${issues}'
	}
	transform_hash_only := windows.replace_once('bc1f63053dedeae665a01ca508b6123f2145c4e1895642ca38e406f8eb7fdf55',
		'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
	assert validate_manifest_source(transform_hash_only, 'transform-hash-manifest-authority').len == 0

	control_mutations := [
		complete.replace_once('"path": "tcc.exe"', '"path": "build.sh"'),
		complete.replace_once('"path": "src/tcc.c"', '"path": "automation/declared-payload.json"'),
		complete.replace_once('"path": "tcc.exe"', '"path": ".github/workflows/payload.yml"'),
		windows.replace_once('"path": "src/tcc.c"',
			'"path": "patches/0001-tccpe-strip-quotes-and-default-.dll-extension-in-DEF.patch"'),
	]
	for index, mutation in control_mutations {
		issues := validate_manifest_source(mutation, 'control-overlap-${index}')
		assert issues.any(it.message.contains('control-plane paths')), '${issues}'
	}
	recipe_equals_manifest := complete.replace_once('"path": "build.sh"',
		'"path": "automation/bundle-manifest.json"')
	assert validate_manifest_source(recipe_equals_manifest, 'recipe-equals-manifest').any(it.message.contains('reserved control-plane'))
	recipe_equals_patch := windows.replace_once('"path": "build.ps1"',
		'"path": "patches/0001-tccpe-strip-quotes-and-default-.dll-extension-in-DEF.patch"')
	assert validate_manifest_source(recipe_equals_patch, 'recipe-equals-patch').any(it.message.contains('globally unique'))
	casefold_collisions := [
		windows.replace_once('"path": "vlang-header-compat.patch"', '"path": "BUILD.PS1"'),
		windows.replace_once('"path": "tcc.exe"', '"path": "VLANG-HEADER-COMPAT.PATCH"'),
		windows.replace_once('"path": "src/tcc.c"', '"path": ".Git/config"'),
	]
	for index, mutation in casefold_collisions {
		issues := validate_manifest_source(mutation, 'windows-casefold-${index}')
		assert issues.any(it.message.contains('globally unique')
			|| it.message.contains('control-plane paths')), '${issues}'
	}
	for target_id in ['macos-amd64', 'macos-arm64'] {
		casefold_collision := manifest_for_target(target_id).replace_once('"path": "tcc.exe"',
			'"path": "BUILD.SH"')
		issues := validate_manifest_source(casefold_collision, '${target_id}-casefold-policy')
		assert issues.any(it.message == 'payload paths cannot overlap control-plane paths'), '${target_id}: ${issues}'
	}
	linux_case_sensitive := manifest_for_target('linux-amd64').replace_once('"path": "tcc.exe"',
		'"path": "BUILD.SH"')
	linux_case_sensitive_issues := validate_manifest_source(linux_case_sensitive,
		'linux-case-sensitive-boundary')
	assert linux_case_sensitive_issues.len == 0, '${linux_case_sensitive_issues}'

	unresolved := complete.replace_once('"sha": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",\n      "tree": "ffffffffffffffffffffffffffffffffffffffff"',
		'"sha": null,\n      "tree": null')
	assert validate_manifest_source(unresolved, 'source-unresolved-pair').len == 0
	toolchain_base := os.join_path(os.temp_dir(), 'tccbin-manifest-toolchain-${os.getpid()}')
	os.rmdir_all(toolchain_base) or {}
	authority := t2a_prepare_toolchain_authority(toolchain_base, 'linux-amd64')
	defer {
		os.rmdir_all(toolchain_base) or {}
	}
	bound_toolchain := t2a_profile_bound_unobserved_toolchain(complete, authority)
	assert validate_manifest_source_at(authority.root, bound_toolchain,
		'toolchain-profile-bound-unobserved').len == 0
	unreviewed_bound_issues := validate_manifest_source(bound_toolchain,
		'toolchain-unreviewed-profile-binding')
	assert unreviewed_bound_issues.len == 1, '${unreviewed_bound_issues}'
	assert unreviewed_bound_issues[0].path == '$/toolchain'
	assert unreviewed_bound_issues[0].message == 'target has no reviewed toolchain profile'
	resolved_toolchain := t2a_resolved_manifest_toolchain(complete, authority)
	assert validate_manifest_source_at(authority.root, resolved_toolchain,
		'toolchain-producer-resolved').len == 0
	partial_toolchain := complete.replace_once('"profile_id": null',
		'"profile_id": "${authority.profile_id}"')
	partial_toolchain_issues := validate_manifest_source(partial_toolchain,
		'toolchain-partial-profile-pair')
	assert partial_toolchain_issues.len == 1, '${partial_toolchain_issues}'
	assert partial_toolchain_issues[0].path == '$/toolchain'
	producer_without_profile := complete.replace_once('"producer_observation": null',
		'"producer_observation": ${authority.producer_source}')
	producer_without_profile_issues := validate_manifest_source(producer_without_profile,
		'toolchain-producer-without-profile')
	assert producer_without_profile_issues.len == 1, '${producer_without_profile_issues}'
	assert producer_without_profile_issues[0].path == '$/toolchain'
	outer_profile_mismatch := resolved_toolchain.replace_once('"profile_id": "${authority.profile_id}"',
		'"profile_id": "linux-amd64-synthetic-v2"')
	outer_profile_issues := validate_manifest_source_at(authority.root, outer_profile_mismatch,
		'toolchain-outer-profile-mismatch')
	assert outer_profile_issues.len == 1, '${outer_profile_issues}'
	assert outer_profile_issues[0].path == '$/toolchain'
	assert outer_profile_issues[0].message == 'manifest toolchain profile binding differs from the registry'
	validator_source := synthetic_toolchain_observation_source('linux-amd64',
		authority.profile_sha256, 'validator')
	validator_toolchain := resolved_toolchain.replace_once(authority.producer_source,
		validator_source)
	validator_issues := validate_manifest_source_at(authority.root, validator_toolchain,
		'toolchain-validator-in-producer-slot')
	assert validator_issues.len == 1, '${validator_issues}'
	assert validator_issues[0].message == 'manifest toolchain observation must have producer phase'
	digest_marker := '"observation_digest":"${authority.producer_digest}"'
	assert resolved_toolchain.count(digest_marker) == 1
	digest_issues := validate_manifest_source_at(authority.root, resolved_toolchain.replace_once(digest_marker,
		'"observation_digest":"${'0'.repeat(64)}"'), 'toolchain-producer-digest-drift')
	assert digest_issues.len == 1, '${digest_issues}'
	assert digest_issues[0].message == 'toolchain observation digest is not derived from the complete canonical observation'
	half_resolved := unresolved.replace_once('"sha": null,\n      "tree": null',
		'"sha": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",\n      "tree": null')
	half_resolved_issues := validate_manifest_source(half_resolved, 'source-half-resolved')
	assert half_resolved_issues.len == 1, '${half_resolved_issues}'
	assert half_resolved_issues[0].path == '$/sources/1'
	assert half_resolved_issues[0].message == 'source SHA and tree must be resolved or null as one pair'
	wrong_source := complete.replace_once('https://github.com/ivmai/bdwgc.git',
		'https://github.com/bdwgc/bdwgc.git')
	assert validate_manifest_source(wrong_source, 'source-registry-drift').any(it.message.contains('exact target matrix'))
	window_v_sha_drift := windows.replace_once('"sha": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",\n      "tree": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"',
		'"sha": "cccccccccccccccccccccccccccccccccccccccc",\n      "tree": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"')
	assert validate_manifest_source(window_v_sha_drift, 'windows-v-libgc-sha').any(it.message.contains('must equal v_source_sha'))
}

fn test_manifests_validate_semantically() {
	fixture_root := os.join_path(automation_root(), 'tests', 'fixtures')
	for name in ['manifest-complete.valid.json', 'manifest-windows-opaque.valid.json'] {
		manifest_path := os.join_path(fixture_root, name)
		issues := bin.validate_manifest(automation_root(), manifest_path) or { panic(err) }
		assert issues.len == 0, '${name}: ${issues}'
		mut rejected := ''
		bin.authenticate_manifest_file(automation_root(), manifest_path) or { rejected = err.msg() }
		assert rejected == 'incomplete provenance cannot produce an authenticated manifest', '${name}: ${rejected}'
	}
}

fn test_durable_schema_fixtures_validate_with_local_refs_and_conditionals() {
	fixtures := {
		'active-intent.schema.json':         'active-intent.bootstrap.schema-fixture.json'
		'issue-projection.schema.json':      'issue-projection.schema-fixture.json'
		'native-gate-subject.schema.json':   'native-gate-subject.schema-fixture.json'
		'native-gate-execution.schema.json': 'native-gate-execution.schema-fixture.json'
		'recovery-handoff.schema.json':      'recovery-handoff.pending.schema-fixture.json'
		'source-state.schema.json':          'source-state.outage.schema-fixture.json'
		'target-state.schema.json':          'target-state.bootstrap.schema-fixture.json'
	}
	for schema_name, fixture_name in fixtures {
		assert validate_schema_source(schema_name, schema_fixture(fixture_name), fixture_name).len == 0
	}
}

fn test_v_smoke_full_target_state_matrix_is_structurally_and_semantically_valid() {
	fixtures := [
		'target-state.v-smoke-pending.schema-fixture.json',
		'target-state.v-smoke-awaiting-ack.schema-fixture.json',
		'target-state.v-smoke-dispatched.schema-fixture.json',
		'target-state.v-smoke-terminal-check.schema-fixture.json',
		'target-state.v-smoke-retry-blocked-pre-ack.schema-fixture.json',
		'target-state.v-smoke-retry-pending.schema-fixture.json',
		'target-state.v-smoke-deadline-exceeded-retry-pending.schema-fixture.json',
		'target-state.v-smoke-retry-awaiting-ack.schema-fixture.json',
		'target-state.v-smoke-retry-dispatched.schema-fixture.json',
		'target-state.v-smoke-retry-terminal.schema-fixture.json',
		'target-state.v-smoke-terminal-infra-retry-blocked-pre-ack.schema-fixture.json',
		'target-state.v-smoke-blocked-pre-ack.schema-fixture.json',
		'target-state.v-smoke-run-absent-retry-pending.schema-fixture.json',
		'target-state.v-smoke-run-absent-retry-awaiting-ack.schema-fixture.json',
		'target-state.v-smoke-run-absent-retry-blocked-pre-ack.schema-fixture.json',
		'target-state.v-smoke-run-absent-retry-dispatched.schema-fixture.json',
		'target-state.v-smoke-run-absent-retry-terminal.schema-fixture.json',
		'target-state.v-smoke-run-absent-exhausted.schema-fixture.json',
	]
	for fixture in fixtures {
		issues := validate_schema_source('target-state.schema.json', schema_fixture(fixture),
			fixture)
		assert issues.len == 0, '${fixture}: ${issues}'
	}
}

fn test_v_smoke_digest_tampering_is_rejected_by_the_targeted_proof() {
	bad_digest := 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	awaiting := schema_fixture('target-state.v-smoke-awaiting-ack.schema-fixture.json')
	dispatched := schema_fixture('target-state.v-smoke-dispatched.schema-fixture.json')
	terminal := schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')
	blocked := schema_fixture('target-state.v-smoke-blocked-pre-ack.schema-fixture.json')
	pending := schema_fixture('target-state.v-smoke-pending.schema-fixture.json')
	assert_target_semantic_rejection(replace_nth_json_string_value(awaiting, 'facts_digest', 1,
		bad_digest), 'dispatch-digest', 'dispatch facts digest does not cover')
	assert_target_semantic_rejection(replace_nth_json_string_value(dispatched, 'ack_facts_digest',
		1, bad_digest), 'ack-digest', 'ACK facts digest does not cover')
	assert_target_semantic_rejection(replace_nth_json_string_value(terminal,
		'completion_facts_digest', 1, bad_digest), 'completion-digest',
		'completion facts digest does not cover')
	assert_target_semantic_rejection(replace_nth_json_string_value(blocked, 'block_facts_digest',
		1, bad_digest), 'block-digest', 'block facts digest does not cover')
	assert_target_semantic_rejection(replace_nth_json_string_value(pending, 'replay_facts_digest',
		1, bad_digest), 'replay-digest', 'replay facts digest does not cover')
}

fn assert_target_semantic_rejection(source string, suffix string, expected_message string) {
	issues := validate_schema_source('target-state.schema.json', source, suffix)
	assert issues.any(it.message.contains(expected_message)), '${suffix}: ${issues}'
}

struct SchemaNativeAuthorityCase {
	source       string
	authority    SyntheticToolchainAuthority
	record       bin.NativeValidationRecordModel
	inputs       bin.ResolvedInputsModel
	fingerprints bin.FingerprintSet
}

fn schema_oracle_digest(label string) string {
	return sha256.sum256(label.bytes()).hex()
}

fn schema_replace_json_member(value bin.JsonValue, key string, replacement string) bin.JsonValue {
	if value.kind != .object {
		panic('schema JSON replacement root must be an object')
	}
	replacement_value := bin.parse_strict_json(replacement) or {
		panic('invalid schema JSON replacement: ${err}')
	}
	mut object_values := value.object_values.clone()
	mut matching_indices := []int{}
	for index, candidate in value.object_keys {
		if candidate == key {
			matching_indices << index
		}
	}
	if matching_indices.len != 1 {
		panic('schema JSON root member ${key} must occur exactly once')
	}
	object_values[matching_indices[0]] = replacement_value
	return bin.JsonValue{
		kind:          .object
		object_keys:   value.object_keys.clone()
		object_values: object_values
	}
}

fn schema_fingerprint_digests(fingerprints bin.FingerprintSet) []bin.DigestModel {
	mut digests := []bin.DigestModel{cap: fingerprints.digest_lines.len}
	for line in fingerprints.digest_lines {
		parts := line.split('\t')
		assert parts.len == 2, line
		digests << bin.DigestModel{
			path:   parts[0]
			sha256: parts[1]
		}
	}
	return digests
}

fn schema_subject_digest_lines(subject bin.JsonValue) []string {
	digests := subject.object_value('digests') or { panic('subject digests missing') }
	mut lines := []string{cap: digests.array_value.len}
	for digest in digests.array_value {
		path := (digest.object_value('path') or { panic('digest path missing') }).string_value
		sha := (digest.object_value('sha256') or { panic('digest SHA missing') }).string_value
		lines << '${path}\t${sha}'
	}
	lines.sort()
	return lines
}

fn schema_candidate_binding_from_subject(subject bin.JsonValue, parent string) string {
	sha := subject.object_value('sha') or { panic('subject SHA missing') }
	tree := subject.object_value('tree') or { panic('subject tree missing') }
	artifact := subject.object_value('artifact_fingerprint') or {
		panic('subject artifact fingerprint missing')
	}
	manifest := subject.object_value('manifest_hash') or { panic('subject manifest hash missing') }
	digests := subject.object_value('digests') or { panic('subject digests missing') }
	parent_value := bin.JsonValue{
		kind:         .string_value
		string_value: parent
	}
	return '{"sha":${bin.canonical_json(sha)},"tree":${bin.canonical_json(tree)},"parent":${bin.canonical_json(parent_value)},"artifact_fingerprint":${bin.canonical_json(artifact)},"manifest_hash":${bin.canonical_json(manifest)},"digests":${bin.canonical_json(digests)}}'
}

fn test_schema_replace_json_member_changes_only_the_root_member() {
	root := bin.parse_strict_json('{"nested":{"stage":"blocked"},"stage":"blocked"}') or {
		panic(err)
	}
	updated := schema_replace_json_member(root, 'stage', '"checks_green"')
	nested := updated.object_value('nested') or { panic('nested object missing') }
	nested_stage := nested.object_value('stage') or { panic('nested stage missing') }
	root_stage := updated.object_value('stage') or { panic('root stage missing') }
	assert nested_stage.string_value == 'blocked'
	assert root_stage.string_value == 'checks_green'
	assert bin.canonical_json(updated) == '{"nested":{"stage":"blocked"},"stage":"checks_green"}'
}

fn schema_manifest_fingerprints(authority SyntheticToolchainAuthority,
	manifest_source string) bin.FingerprintSet {
	path := os.join_path(os.temp_dir(),
		'tccbin-schema-manifest-${os.getpid()}-${schema_oracle_digest(manifest_source)[..12]}.json')
	os.write_file(path, manifest_source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	manifest := bin.authenticate_manifest_file(authority.root, path) or { panic(err) }
	return bin.authenticated_manifest_fingerprints(manifest) or { panic(err) }
}

fn schema_resolved_inputs(manifest_source string,
	authority SyntheticToolchainAuthority) bin.ResolvedInputsModel {
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	mut sources := []bin.ResolvedSourceModel{}
	mut checks := []bin.SourceCheckModel{}
	manifest_sources := manifest.object_value('sources') or { panic('manifest sources missing') }
	for source in manifest_sources.array_value {
		id := (source.object_value('id') or { panic('source ID missing') }).string_value
		sha := (source.object_value('sha') or { panic('source SHA missing') }).string_value
		sources << bin.ResolvedSourceModel{
			id:         id
			repository: (source.object_value('repository') or { panic('source repository missing') }).string_value
			ref:        (source.object_value('ref') or { panic('source ref missing') }).string_value
			sha:        sha
			tree:       (source.object_value('tree') or { panic('source tree missing') }).string_value
		}
		checks << bin.SourceCheckModel{
			source_id:       id
			resolved_sha:    sha
			status:          'resolved'
			evidence_digest: schema_oracle_digest('resolved-source/${id}/${sha}')
		}
	}
	recipe := manifest.object_value('recipe') or { panic('manifest recipe missing') }
	return bin.ResolvedInputsModel{
		sources:             sources
		source_checks:       checks
		recipe_path:         (recipe.object_value('path') or { panic('recipe path missing') }).string_value
		recipe_hash:         (recipe.object_value('sha256') or { panic('recipe hash missing') }).string_value
		contract_repository: (manifest.object_value('contract_repository') or {
			panic('contract repository missing')
		}).string_value
		contract_sha:        (manifest.object_value('contract_sha') or {
			panic('contract SHA missing')
		}).string_value
		v_source_sha:        (manifest.object_value('v_source_sha') or {
			panic('V source SHA missing')
		}).string_value
		producer_toolchain:  bin.ProducerToolchainModel{
			profile_id:         authority.profile_id
			profile_sha256:     authority.profile_sha256
			observation_sha256: authority.producer_sha256
			observation_digest: authority.producer_digest
		}
	}
}

fn schema_blocked_subject(consumer_kind string, consumer_id string,
	fingerprints bin.FingerprintSet) bin.NativeGateSubjectModel {
	original_ref := if consumer_kind in ['publish_post', 'rollback_post'] {
		'thirdparty-linux-amd64'
	} else {
		'tccbin-candidate/linux-amd64/${consumer_id}'
	}
	return bin.NativeGateSubjectModel{
		consumer_id:            consumer_id
		consumer_kind:          consumer_kind
		intent_or_operation_id: consumer_id
		target_id:              'linux-amd64'
		subject_generation:     1
		initial_run_mode:       'original_push'
		sha:                    'a'.repeat(40)
		tree:                   'b'.repeat(40)
		original_ref:           original_ref
		input_fingerprint:      fingerprints.input_fingerprint
		artifact_fingerprint:   fingerprints.artifact_fingerprint
		manifest_hash:          fingerprints.manifest_hash
		digests:                schema_fingerprint_digests(fingerprints)
	}
}

fn schema_validation_subject_from_native_subject(subject bin.JsonValue) string {
	return '{"sha":${bin.canonical_json(subject.object_value('sha') or {
		panic('subject SHA missing')
	})},"tree":${bin.canonical_json(subject.object_value('tree') or {
		panic('subject tree missing')
	})},"input_fingerprint":${bin.canonical_json(subject.object_value('input_fingerprint') or {
		panic('subject input fingerprint missing')
	})},"artifact_fingerprint":${bin.canonical_json(subject.object_value('artifact_fingerprint') or {
		panic('subject artifact fingerprint missing')
	})},"manifest_hash":${bin.canonical_json(subject.object_value('manifest_hash') or {
		panic('subject manifest hash missing')
	})},"digests":${bin.canonical_json(subject.object_value('digests') or {
		panic('subject digests missing')
	})},"candidate_ref":${bin.canonical_json(subject.object_value('original_ref') or {
		panic('subject ref missing')
	})}}'
}

fn schema_native_record_gate(subject bin.NativeGateSubjectModel, subject_hash string,
	matrix_digest string, inputs bin.ResolvedInputsModel, check_name string) bin.PersistedGateRunModel {
	is_native := check_name == 'tccbin-candidate-gate'
	repository := if is_native { 'vlang/tccbin' } else { 'vlang/v' }
	run_id := if is_native { i64(7001) } else { i64(3001) }
	job_id := if is_native { i64(7201) } else { i64(4001) }
	run_url := 'https://github.com/${repository}/actions/runs/${run_id}'
	job_url := '${run_url}/job/${job_id}'
	audience := if is_native {
		'vlang/tccbin:native-gate-check:v1'
	} else {
		'vlang/tccbin:v-smoke-check:v1'
	}
	return bin.PersistedGateRunModel{
		check_name:                      check_name
		repository:                      repository
		integration_id:                  if is_native { 1001 } else { 1002 }
		workflow_id:                     if is_native { 2001 } else { 2002 }
		workflow_path:                   if is_native {
			'.github/workflows/build-and-test.yml'
		} else {
			'.github/workflows/tccbin_revalidate.yml'
		}
		event:                           if is_native { 'push' } else { 'workflow_dispatch' }
		run_id:                          run_id
		run_attempt:                     1
		check_suite_id:                  if is_native { 7101 } else { 5001 }
		check_suite_integration_id:      1001
		job_id:                          job_id
		subject_hash:                    subject_hash
		check_run_id:                    if is_native { 7301 } else { 6001 }
		external_id:                     bin.deterministic_check_external_id(audience,
			subject.consumer_id, subject_hash, run_id, 1) or { panic(err) }
		run_name:                        if is_native {
			'tccbin-native-gate/${subject.consumer_id}'
		} else {
			'tccbin-v-smoke/${subject.consumer_id}'
		}
		run_url:                         run_url
		job_url:                         job_url
		details_url:                     job_url
		ref:                             if is_native { subject.original_ref } else { 'master' }
		workflow_head_sha:               if is_native { subject.sha } else { inputs.v_source_sha }
		sha:                             subject.sha
		check_sha:                       subject.sha
		actor:                           if is_native {
			'tccbin-publisher[bot]'
		} else {
			'validator-dispatcher[bot]'
		}
		actor_integration_id:            if is_native { 5001 } else { 1002 }
		triggering_actor:                if is_native {
			'tccbin-publisher[bot]'
		} else {
			'validator-dispatcher[bot]'
		}
		triggering_actor_integration_id: if is_native { 5001 } else { 1002 }
		created_at:                      if is_native {
			'2026-08-03T00:00:30Z'
		} else {
			'2026-08-03T00:01:00Z'
		}
		completed_at:                    if is_native {
			'2026-08-03T00:59:00Z'
		} else {
			'2026-08-03T01:00:00Z'
		}
		run_conclusion:                  'success'
		check_conclusion:                'success'
		output_digest:                   if is_native {
			matrix_digest
		} else {
			schema_oracle_digest('blocked-red/v-smoke/output')
		}
		evidence_digest:                 schema_oracle_digest(if is_native {
			'blocked-red/native/evidence'
		} else {
			'blocked-red/v-smoke/evidence'
		})
	}
}

fn schema_native_validation_record(authority SyntheticToolchainAuthority,
	manifest_source string, inputs bin.ResolvedInputsModel, subject bin.NativeGateSubjectModel,
	transition string, verdict string, operation_id string,
	resulting_generation i64) bin.NativeValidationRecordModel {
	mut matrix_source := t2b_native_matrix_source_for_run(manifest_source, authority, subject,
		7001, 1, 7101)
	if verdict == 'functional' {
		matrix_source = t2b_replace_matrix_result_member(matrix_source, 0, 'status', '"failed"')
	} else if verdict == 'infrastructure' {
		matrix_source = t2b_replace_matrix_result_member(matrix_source, 0, 'status', '"blocked"')
	} else if verdict != 'green' {
		panic('unsupported native record verdict ${verdict}')
	}
	matrix := bin.parse_strict_json(matrix_source) or { panic(err) }
	matrix_digest := sha256.sum256(matrix_source.bytes()).hex()
	subject_hash := bin.native_gate_subject_hash(subject) or { panic(err) }
	evidence_sources := t2c_native_validation_evidence_sources(matrix_source, authority)
	mut evidence_names := evidence_sources.keys()
	evidence_names.sort()
	mut evidence := []bin.NativeValidationEvidenceModel{cap: evidence_names.len}
	mut evidence_projection := []string{cap: evidence_names.len}
	for name in evidence_names {
		size := u64(evidence_sources[name].bytes().len)
		evidence << bin.NativeValidationEvidenceModel{
			sha256: name
			size:   size
		}
		evidence_projection << '{"sha256":"${name}","size":${size}}'
	}
	capsule_projection := bin.parse_strict_json('{"evidence":[${evidence_projection.join(',')}],"manifest_hash":"${subject.manifest_hash}","matrix_digest":"${matrix_digest}","schema_version":1,"subject_hash":"${subject_hash}"}') or {
		panic(err)
	}
	seed := bin.NativeValidationRecordModel{
		schema_version:       1
		operation_id:         operation_id
		transition:           transition
		resulting_generation: resulting_generation
		verdict:              verdict
		manifest_source:      manifest_source
		manifest_hash:        subject.manifest_hash
		native_lane_matrix:   matrix
		matrix_digest:        matrix_digest
		evidence:             evidence
		capsule_digest:       bin.json_sha256(capsule_projection)
		native_gate:          schema_native_record_gate(subject, subject_hash, matrix_digest,
			inputs, 'tccbin-candidate-gate')
		v_smoke_gate:         schema_native_record_gate(subject, subject_hash, matrix_digest,
			inputs, 'v-candidate-smoke')
		validation_digest:    '0'.repeat(64)
	}
	return bin.NativeValidationRecordModel{
		...seed
		validation_digest: bin.native_validation_record_digest(bin.native_validation_record_json(seed) or {
			panic(err)
		}) or { panic(err) }
	}
}

fn schema_authority_case(authority SyntheticToolchainAuthority,
	consumer_kind string, consumer_id string, transition string, verdict string, operation_id string,
	resulting_generation i64) SchemaNativeAuthorityCase {
	manifest_source := t2a_resolved_manifest_toolchain(schema_fixture('manifest-complete.valid.json'),
		authority)
	fingerprints := schema_manifest_fingerprints(authority, manifest_source)
	inputs := schema_resolved_inputs(manifest_source, authority)
	subject := schema_blocked_subject(consumer_kind, consumer_id, fingerprints)
	return SchemaNativeAuthorityCase{
		authority:    authority
		record:       schema_native_validation_record(authority, manifest_source, inputs, subject,
			transition, verdict, operation_id, resulting_generation)
		inputs:       inputs
		fingerprints: fingerprints
	}
}

fn schema_native_gate_execution(record bin.NativeValidationRecordModel, generation i64) bin.JsonValue {
	subject := record.native_lane_matrix.object_value('subject') or {
		panic('record subject missing')
	}
	subject_hash := (record.native_lane_matrix.object_value('subject_hash') or {
		panic('record subject hash missing')
	}).string_value
	mut execution := bin.parse_strict_json(schema_fixture('native-gate-execution.schema-fixture.json')) or {
		panic(err)
	}
	execution_source := replace_canonical_root_member(bin.canonical_json(execution), execution,
		'subject', bin.canonical_json(subject))
	execution = bin.parse_strict_json(execution_source) or { panic(err) }
	for binding in [
		['subject_hash', '"${subject_hash}"'],
		['subject_sha',
			bin.canonical_json(subject.object_value('sha') or { panic('subject SHA missing') })],
		['subject_generation', (subject.object_value('subject_generation') or {
			panic('subject generation missing')
		}).int_value.str()],
		['expected_ledger_generation', generation.str()],
	] {
		execution = schema_replace_json_member(execution, binding[0], binding[1])
	}
	mut epoch := bin.parse_strict_json(bin.canonical_json((execution.object_value('gate_epochs') or {
		panic('gate epochs missing')
	}).array_value[0])) or { panic(err) }
	epoch = schema_replace_json_member(epoch, 'expected_ref', '"${record.native_gate.ref}"')
	epoch = schema_replace_json_member(epoch, 'state', '"completed"')
	epoch = schema_replace_json_member(epoch, 'selected_run_id', record.native_gate.run_id.str())
	epoch = schema_replace_json_member(epoch, 'selected_run_attempt',
		record.native_gate.run_attempt.str())
	epoch = schema_replace_json_member(epoch, 'selected_check_suite_id',
		record.native_gate.check_suite_id.str())
	epoch = schema_replace_json_member(epoch, 'conclusion',
		'"${record.native_gate.run_conclusion}"')
	epoch = schema_replace_json_member(epoch, 'closed_at', '"${record.native_gate.completed_at}"')
	execution = schema_replace_json_member(execution, 'gate_epochs',
		'[${bin.canonical_json(epoch)}]')
	observed_run := bin.parse_strict_json('{"gate_epoch":0,"run_id":${record.native_gate.run_id},"run_attempt":${record.native_gate.run_attempt},"repository":"${record.native_gate.repository}","ref":"${record.native_gate.ref}","sha":"${record.native_gate.sha}","event":"${record.native_gate.event}","actor":"${record.native_gate.actor}","actor_integration_id":${record.native_gate.actor_integration_id},"triggering_actor":"${record.native_gate.triggering_actor}","triggering_actor_integration_id":${record.native_gate.triggering_actor_integration_id},"check_suite_id":${record.native_gate.check_suite_id},"workflow_id":${record.native_gate.workflow_id},"workflow_path":"${record.native_gate.workflow_path}","created_at":"${record.native_gate.created_at}","conclusion":"${record.native_gate.run_conclusion}"}') or {
		panic(err)
	}
	execution = schema_replace_json_member(execution, 'gate_runs',
		'[${bin.canonical_json(observed_run)}]')
	execution = schema_replace_json_member(execution, 'ack_operation_ids',
		'["${schema_oracle_digest('${subject_hash}/native-ack')}"]')
	execution = schema_replace_json_member(execution, 'completion_operation_ids',
		'["${schema_oracle_digest('${subject_hash}/native-complete')}"]')
	execution = schema_replace_json_member(execution, 'selected_run_id',
		record.native_gate.run_id.str())
	execution = schema_replace_json_member(execution, 'selected_run_attempt',
		record.native_gate.run_attempt.str())
	execution = schema_replace_json_member(execution, 'selected_check_suite_id',
		record.native_gate.check_suite_id.str())
	return schema_replace_json_member(execution, 'selected_conclusion',
		'"${record.native_gate.run_conclusion}"')
}

fn schema_v_smoke_execution(record bin.NativeValidationRecordModel, inputs bin.ResolvedInputsModel,
	generation i64) bin.JsonValue {
	subject := record.native_lane_matrix.object_value('subject') or {
		panic('record subject missing')
	}
	subject_hash := (record.native_lane_matrix.object_value('subject_hash') or {
		panic('record subject hash missing')
	}).string_value
	fixture := bin.parse_strict_json(schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')) or {
		panic(err)
	}
	mut smoke := fixture.object_value('v_smoke_execution') or { panic('smoke fixture missing') }
	consumer_id := (subject.object_value('consumer_id') or { panic('consumer ID missing') }).string_value
	consumer_kind := (subject.object_value('consumer_kind') or { panic('consumer kind missing') }).string_value
	for binding in [
		['consumer_id', '"${consumer_id}"'],
		['consumer_kind', '"${consumer_kind}"'],
		['intent_or_operation_id', '"${consumer_id}"'],
		['subject_hash', '"${subject_hash}"'],
		['subject_generation', (subject.object_value('subject_generation') or {
			panic('subject generation missing')
		}).int_value.str()],
		['subject_ref',
			bin.canonical_json(subject.object_value('original_ref') or {
				panic('subject ref missing')
			})],
		['subject_sha',
			bin.canonical_json(subject.object_value('sha') or { panic('subject SHA missing') })],
		['v_master_sha', '"${inputs.v_source_sha}"'],
		['run_name', '"tccbin-v-smoke/${consumer_id}"'],
		['reservation_operation_id', '"${consumer_id}"'],
		['expected_ledger_generation', generation.str()],
	] {
		smoke = schema_replace_json_member(smoke, binding[0], binding[1])
	}
	attempts := smoke.object_value('attempts') or { panic('smoke attempts missing') }
	mut attempt := attempts.array_value[0]
	for binding in [
		['run_id', record.v_smoke_gate.run_id.str()],
		['run_attempt', record.v_smoke_gate.run_attempt.str()],
		['check_suite_id', record.v_smoke_gate.check_suite_id.str()],
		['job_id', record.v_smoke_gate.job_id.str()],
		['run_name', '"${record.v_smoke_gate.run_name}"'],
		['run_url', '"${record.v_smoke_gate.run_url}"'],
		['job_url', '"${record.v_smoke_gate.job_url}"'],
		['head_sha', '"${record.v_smoke_gate.workflow_head_sha}"'],
		['subject_ref',
			bin.canonical_json(subject.object_value('original_ref') or {
				panic('subject ref missing')
			})],
		['subject_sha', '"${record.v_smoke_gate.sha}"'],
		['created_at', '"${record.v_smoke_gate.created_at}"'],
		['check_run_id', record.v_smoke_gate.check_run_id.str()],
		['check_sha', '"${record.v_smoke_gate.check_sha}"'],
		['details_url', '"${record.v_smoke_gate.details_url}"'],
		['external_id', '"${record.v_smoke_gate.external_id}"'],
		['run_conclusion', '"${record.v_smoke_gate.run_conclusion}"'],
		['check_conclusion', '"${record.v_smoke_gate.check_conclusion}"'],
		['output_digest', '"${record.v_smoke_gate.output_digest}"'],
		['evidence_digest', '"${record.v_smoke_gate.evidence_digest}"'],
		['completed_at', '"${record.v_smoke_gate.completed_at}"'],
	] {
		attempt = schema_replace_json_member(attempt, binding[0], binding[1])
	}
	smoke = schema_replace_json_member(smoke, 'attempts', '[${bin.canonical_json(attempt)}]')
	refreshed_wrapper := bin.parse_strict_json(refresh_v_smoke_facts_digests('{"v_smoke_execution":${bin.canonical_json(smoke)}}')) or {
		panic(err)
	}
	return refreshed_wrapper.object_value('v_smoke_execution') or {
		panic('refreshed smoke missing')
	}
}

fn schema_blocked_red_source(authority SyntheticToolchainAuthority, consumer_kind string,
	transition string, verdict string) SchemaNativeAuthorityCase {
	consumer_id := schema_oracle_digest('blocked-red/${consumer_kind}/consumer')
	operation_id := schema_oracle_digest('blocked-red/${consumer_kind}/${transition}/${verdict}')
	case := schema_authority_case(authority, consumer_kind, consumer_id, transition, verdict,
		operation_id, 9)
	record := case.record
	subject := record.native_lane_matrix.object_value('subject') or {
		panic('record subject missing')
	}
	subject_hash := (record.native_lane_matrix.object_value('subject_hash') or {
		panic('record subject hash missing')
	}).string_value
	candidate_binding := schema_candidate_binding_from_subject(subject, 'a'.repeat(40))
	subject_artifact := live_artifact_tuple_from_subject(subject)
	mut root := bin.parse_strict_json(schema_fixture('target-state.bootstrap.schema-fixture.json')) or {
		panic(err)
	}
	intent_fixture := bin.parse_strict_json(schema_fixture('active-intent.bootstrap.schema-fixture.json')) or {
		panic(err)
	}
	mut intent := intent_fixture
	intent_type := match consumer_kind {
		'publish_candidate', 'publish_post' { 'publish' }
		'rollback_candidate', 'rollback_post' { 'rollback' }
		'adopt_current' { 'adopt-current' }
		'initial_adopt_current' { 'initial_adopt_current' }
		else { panic('unsupported blocked native subject kind ${consumer_kind}') }
	}
	is_adoption := intent_type in ['adopt-current', 'initial_adopt_current']
	intent_id := if consumer_kind in ['publish_candidate', 'rollback_candidate', 'adopt_current',
		'initial_adopt_current'] {
		consumer_id
	} else {
		schema_oracle_digest('blocked-red/${consumer_kind}/intent')
	}
	for binding in [
		['intent_id', '"${intent_id}"'],
		['intent_type', '"${intent_type}"'],
		['stage', '"blocked"'],
		['input_fingerprint', '"${case.fingerprints.input_fingerprint}"'],
		['resolved_inputs', resolved_inputs_source_for_authority(case.inputs)],
		['expected_canonical_head', '"${'a'.repeat(40)}"'],
		['candidate_ref', '"tccbin-candidate/linux-amd64/${intent_id}"'],
		['generation', '0'],
		['candidate_binding', if is_adoption {
			'null'
		} else {
			candidate_binding
		}],
		['validation_subject', if is_adoption {
			schema_validation_subject_from_native_subject(subject)
		} else {
			'null'
		}],
		['previous_last_known_good', if intent_type == 'initial_adopt_current' {
			'null'
		} else {
			'{"sha":"${'c'.repeat(40)}","tree":"${'d'.repeat(40)}","input_fingerprint":"${case.fingerprints.input_fingerprint}","artifact_fingerprint":"${case.fingerprints.artifact_fingerprint}","manifest_hash":"${case.fingerprints.manifest_hash}","digests":[{"path":"tcc.exe","sha256":"${schema_oracle_digest('blocked-red/last-good')}"}]}'
		}],
	] {
		intent = schema_replace_json_member(intent, binding[0], binding[1])
	}
	if intent_type == 'rollback' {
		intent = schema_replace_json_member(intent, 'bad_provisional', subject_artifact)
		intent = schema_replace_json_member(intent, 'rollback_diff_fingerprint',
			'"${schema_oracle_digest('blocked-red/rollback-diff')}"')
	}
	if consumer_kind == 'rollback_post' {
		intent = schema_replace_json_member(intent, 'rollback_provisional', candidate_binding)
	}
	record_value := bin.native_validation_record_json(record) or { panic(err) }
	gate_values := [
		record_value.object_value('native_gate') or { panic('record native gate missing') },
		record_value.object_value('v_smoke_gate') or { panic('record V smoke gate missing') },
	]
	intent = schema_replace_json_member(intent, 'gate_runs', bin.canonical_json(bin.JsonValue{
		kind:        .array
		array_value: gate_values
	}))
	for binding in [
		['generation', '9'],
		['target_state', '"quarantined"'],
		['publication_state', if consumer_kind == 'publish_post' {
			'"post_publish_blocked"'
		} else if consumer_kind in ['publish_candidate', 'adopt_current', 'initial_adopt_current'] {
			'"promotion_blocked"'
		} else {
			'"rollback_blocked"'
		}],
		['bootstrap_required', if intent_type == 'initial_adopt_current' {
			'true'
		} else {
			'false'
		}],
		['canonical_observed_sha', '"${'a'.repeat(40)}"'],
		['input_fingerprint', '"${case.fingerprints.input_fingerprint}"'],
		['artifact_fingerprint', '"${case.fingerprints.artifact_fingerprint}"'],
		['manifest_hash', '"${case.fingerprints.manifest_hash}"'],
		['provenance_status', '"complete"'],
		['resolved_inputs', resolved_inputs_source_for_authority(case.inputs)],
		['last_known_good',
			bin.canonical_json(intent.object_value('previous_last_known_good') or {
				panic('last good missing')
			})],
		['provisional_published', if consumer_kind == 'publish_post' {
			live_artifact_tuple_from_subject(subject)
		} else if intent_type == 'rollback' {
			bin.canonical_json(intent.object_value('bad_provisional') or {
				panic('bad provisional missing')
			})
		} else {
			'null'
		}],
		['active_intent', bin.canonical_json(intent)],
		['post_validation_operation_id', if consumer_kind in ['publish_post', 'rollback_post'] {
			'"${consumer_id}"'
		} else {
			'null'
		}],
		['native_gate_subject', bin.canonical_json(subject)],
		['active_subject_hash', '"${subject_hash}"'],
		['last_native_validation',
			bin.canonical_json(bin.native_validation_record_json(record) or { panic(err) })],
	] {
		root = schema_replace_json_member(root, binding[0], binding[1])
	}
	native_execution := schema_native_gate_execution(record, 9)
	smoke := schema_v_smoke_execution(record, case.inputs, 9)
	root = schema_replace_json_member(root, 'native_gate_execution',
		bin.canonical_json(native_execution))
	root = schema_replace_json_member(root, 'v_smoke_execution', bin.canonical_json(smoke))
	ack_id := schema_oracle_digest('${subject_hash}/native-ack')
	complete_id := schema_oracle_digest('${subject_hash}/native-complete')
	smoke_dispatch := (smoke.object_value('dispatches') or { panic('smoke dispatch missing') }).array_value[0]
	smoke_attempt := (smoke.object_value('attempts') or { panic('smoke attempt missing') }).array_value[0]
	root = schema_replace_json_member(root, 'applied_operations', '[{"operation_id":"${consumer_id}","transition":"${if consumer_kind == 'publish_post' {
		'promotion_confirmed'
	} else if consumer_kind == 'rollback_post' {
		'rollback_promoted'
	} else {
		'bind_candidate'
	}}","resulting_generation":1},{"operation_id":"${ack_id}","transition":"native_gate_ack_${subject_hash}","resulting_generation":2},{"operation_id":"${complete_id}","transition":"native_gate_complete_${subject_hash}","resulting_generation":3},{"operation_id":"${(smoke_dispatch.object_value('dispatch_operation_id') or {
		panic('smoke dispatch ID missing')
	}).string_value}","transition":"v-smoke-dispatch-1","resulting_generation":4},{"operation_id":"${(smoke_attempt.object_value('ack_operation_id') or {
		panic('smoke ACK ID missing')
	}).string_value}","transition":"v-smoke-ack-1","resulting_generation":5},{"operation_id":"${(smoke_attempt.object_value('completion_operation_id') or {
		panic('smoke completion ID missing')
	}).string_value}","transition":"v-smoke-complete-1","resulting_generation":6},{"operation_id":"${schema_oracle_digest('blocked-red/filler-7')}","transition":"blocked-red-preparation","resulting_generation":7},{"operation_id":"${schema_oracle_digest('blocked-red/filler-8')}","transition":"blocked-red-observation","resulting_generation":8},{"operation_id":"${operation_id}","transition":"${transition}","resulting_generation":9}]')
	root = schema_replace_json_member(root, 'incidents',
		'[{"incident_id":"${operation_id}","owner_repository":"vlang/tccbin","status":"active","failure_class":"native_validation","component":"tccbin","test_id":"${transition}","lane":"${consumer_kind}","input_fingerprint":"${case.fingerprints.input_fingerprint}","artifact_fingerprint":"${case.fingerprints.artifact_fingerprint}","created_by_operation_id":"${operation_id}","resolved_by_sha":null}]')
	root = schema_replace_json_member(root, 'last_operation_id', '"${operation_id}"')
	root = schema_replace_json_member(root, 'last_transition', '"${transition}"')
	return SchemaNativeAuthorityCase{
		...case
		source: bin.canonical_json(root)
	}
}

fn schema_reseal_native_record(record bin.NativeValidationRecordModel, transition string,
	verdict string) bin.NativeValidationRecordModel {
	seed := bin.NativeValidationRecordModel{
		...record
		transition:        transition
		verdict:           verdict
		validation_digest: '0'.repeat(64)
	}
	return bin.NativeValidationRecordModel{
		...seed
		validation_digest: bin.native_validation_record_digest(bin.native_validation_record_json(seed) or {
			panic(err)
		}) or { panic(err) }
	}
}

fn schema_source_with_last_record(source string, record bin.NativeValidationRecordModel) string {
	mut root := bin.parse_strict_json(source) or { panic(err) }
	root = schema_replace_json_member(root, 'last_native_validation', bin.canonical_json(bin.native_validation_record_json(record) or {
		panic(err)
	}))
	operations_value := root.object_value('applied_operations') or {
		panic('applied operations missing')
	}
	mut operations := operations_value.array_value.clone()
	mut operation_matches := 0
	for index, operation in operations {
		generation := (operation.object_value('resulting_generation') or {
			panic('operation generation missing')
		}).int_value
		if generation == record.resulting_generation {
			mut updated := schema_replace_json_member(operation, 'operation_id',
				'"${record.operation_id}"')
			updated = schema_replace_json_member(updated, 'transition', '"${record.transition}"')
			operations[index] = updated
			operation_matches++
		}
	}
	assert operation_matches == 1
	root = schema_replace_json_member(root, 'applied_operations', bin.canonical_json(bin.JsonValue{
		kind:        .array
		array_value: operations
	}))
	if record.resulting_generation == (root.object_value('generation') or {
		panic('target generation missing')
	}).int_value {
		root = schema_replace_json_member(root, 'last_operation_id', '"${record.operation_id}"')
		root = schema_replace_json_member(root, 'last_transition', '"${record.transition}"')
	}
	return bin.canonical_json(root)
}

fn schema_source_with_last_ledger_transition(source string, transition string) string {
	mut root := bin.parse_strict_json(source) or { panic(err) }
	generation := (root.object_value('generation') or { panic('target generation missing') }).int_value
	operations_value := root.object_value('applied_operations') or {
		panic('applied operations missing')
	}
	mut operations := operations_value.array_value.clone()
	mut matches := 0
	for index, operation in operations {
		if (operation.object_value('resulting_generation') or {
			panic('operation generation missing')
		}).int_value == generation {
			operations[index] = schema_replace_json_member(operation, 'transition',
				'"${transition}"')
			matches++
		}
	}
	assert matches == 1
	root = schema_replace_json_member(root, 'applied_operations', bin.canonical_json(bin.JsonValue{
		kind:        .array
		array_value: operations
	}))
	root = schema_replace_json_member(root, 'last_transition', '"${transition}"')
	return bin.canonical_json(root)
}

fn schema_source_with_intent_gates(source string, gates []bin.JsonValue) string {
	mut root := bin.parse_strict_json(source) or { panic(err) }
	mut intent := root.object_value('active_intent') or { panic('active intent missing') }
	intent = schema_replace_json_member(intent, 'gate_runs', bin.canonical_json(bin.JsonValue{
		kind:        .array
		array_value: gates
	}))
	root = schema_replace_json_member(root, 'active_intent', bin.canonical_json(intent))
	return bin.canonical_json(root)
}

fn schema_source_with_intent_gate_member(source string, gate_index int, key string,
	replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	intent := root.object_value('active_intent') or { panic('active intent missing') }
	gates_value := intent.object_value('gate_runs') or { panic('intent gate runs missing') }
	mut gates := gates_value.array_value.clone()
	assert gate_index >= 0 && gate_index < gates.len
	gates[gate_index] = schema_replace_json_member(gates[gate_index], key, replacement)
	return schema_source_with_intent_gates(source, gates)
}

fn schema_source_with_native_execution_member(source string, key string,
	replacement string) string {
	mut root := bin.parse_strict_json(source) or { panic(err) }
	mut execution := root.object_value('native_gate_execution') or {
		panic('native execution missing')
	}
	execution = schema_replace_json_member(execution, key, replacement)
	root = schema_replace_json_member(root, 'native_gate_execution', bin.canonical_json(execution))
	return bin.canonical_json(root)
}

fn schema_source_with_observed_native_member(source string, key string,
	replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	execution := root.object_value('native_gate_execution') or { panic('native execution missing') }
	runs_value := execution.object_value('gate_runs') or { panic('observed native runs missing') }
	mut runs := runs_value.array_value.clone()
	assert runs.len == 1
	runs[0] = schema_replace_json_member(runs[0], key, replacement)
	return schema_source_with_native_execution_member(source, 'gate_runs', bin.canonical_json(bin.JsonValue{
		kind:        .array
		array_value: runs
	}))
}

fn schema_record_for_subject(authority SyntheticToolchainAuthority,
	subject bin.NativeGateSubjectModel, transition string, verdict string, operation_id string,
	resulting_generation i64) bin.NativeValidationRecordModel {
	manifest_source := t2a_resolved_manifest_toolchain(schema_fixture('manifest-complete.valid.json'),
		authority)
	inputs := schema_resolved_inputs(manifest_source, authority)
	return schema_native_validation_record(authority, manifest_source, inputs, subject, transition,
		verdict, operation_id, resulting_generation)
}

fn schema_publisher_green_preserved_source(authority SyntheticToolchainAuthority,
	active_kind string) string {
	red_transition := match active_kind {
		'publish_post' { 'post_check_infra_exhausted' }
		'rollback_post' { 'rollback_failed' }
		else { 'candidate_failed' }
	}
	base := schema_blocked_red_source(authority, active_kind, red_transition, 'infrastructure')
	mut root := bin.parse_strict_json(base.source) or { panic(err) }
	intent := root.object_value('active_intent') or { panic('active intent missing') }
	intent_id := (intent.object_value('intent_id') or { panic('intent ID missing') }).string_value
	record_kind := if active_kind == 'publish_post' {
		'publish_candidate'
	} else if active_kind == 'rollback_post' {
		'rollback_candidate'
	} else {
		active_kind
	}
	green_operation_id := schema_oracle_digest('blocked-red/${active_kind}/preserved-green')
	green := schema_authority_case(authority, record_kind, intent_id, 'candidate_checks_green',
		'green', green_operation_id, 7)
	green_value := bin.native_validation_record_json(green.record) or { panic(err) }
	green_gates := [
		green_value.object_value('native_gate') or { panic('green native gate missing') },
		green_value.object_value('v_smoke_gate') or { panic('green V gate missing') },
	]
	root = bin.parse_strict_json(schema_source_with_last_record(bin.canonical_json(root),
		green.record)) or { panic(err) }
	mut source := schema_source_with_intent_gates(bin.canonical_json(root), green_gates)
	if active_kind == record_kind {
		green_subject := green.record.native_lane_matrix.object_value('subject') or {
			panic('green subject missing')
		}
		green_subject_hash := (green.record.native_lane_matrix.object_value('subject_hash') or {
			panic('green subject hash missing')
		}).string_value
		root = bin.parse_strict_json(source) or { panic(err) }
		root = schema_replace_json_member(root, 'native_gate_subject',
			bin.canonical_json(green_subject))
		root = schema_replace_json_member(root, 'active_subject_hash', '"${green_subject_hash}"')
		root = schema_replace_json_member(root, 'native_gate_execution',
			bin.canonical_json(schema_native_gate_execution(green.record, 9)))
		root = schema_replace_json_member(root, 'v_smoke_execution', bin.canonical_json(schema_v_smoke_execution(green.record,
			green.inputs, 9)))
		source = bin.canonical_json(root)
	}
	publisher_transition := match active_kind {
		'publish_candidate', 'adopt_current', 'initial_adopt_current' { 'promotion_failed' }
		'rollback_candidate' { 'rollback_failed' }
		else { '' }
	}
	if publisher_transition != '' {
		source = schema_source_with_last_ledger_transition(source, publisher_transition)
	}
	return source
}

fn assert_schema_exact_issue(authority SyntheticToolchainAuthority, source string, suffix string,
	path string, message string) {
	issues := validate_schema_source_at(authority.root, 'target-state.schema.json', source, suffix)
	assert issues.any(it.path == path && it.message == message), '${suffix}: ${issues}'
}

fn assert_schema_blocked_red_rejection(authority SyntheticToolchainAuthority, source string,
	suffix string) {
	assert_schema_exact_issue(authority, source, suffix, '$/last_native_validation',
		'blocked red validation differs from its active subject and two gate runs')
}

fn test_blocked_red_schema_joins_current_record_subject_gates_and_selected_winner() {
	base := os.join_path(os.temp_dir(), 'tccbin-blocked-red-schema-${os.getpid()}')
	os.rmdir_all(base) or {}
	authority := t2a_prepare_toolchain_authority(base, 'linux-amd64')
	defer {
		os.rmdir_all(base) or { panic(err) }
	}
	for blocked_case in [
		['rollback_candidate', 'candidate_failed', 'functional'],
		['rollback_candidate', 'rollback_failed', 'functional'],
		['publish_post', 'post_check_infra_exhausted', 'infrastructure'],
		['rollback_post', 'rollback_failed', 'functional'],
		['rollback_post', 'rollback_failed', 'infrastructure'],
	] {
		case := schema_blocked_red_source(authority, blocked_case[0], blocked_case[1],
			blocked_case[2])
		root := bin.parse_strict_json(case.source) or { panic(err) }
		subject := root.object_value('native_gate_subject') or { panic('native subject missing') }
		assert schema_subject_digest_lines(subject) == case.fingerprints.digest_lines
		intent := root.object_value('active_intent') or { panic('active intent missing') }
		candidate_binding := intent.object_value('candidate_binding') or {
			panic('candidate binding missing')
		}
		assert schema_subject_digest_lines(candidate_binding) == case.fingerprints.digest_lines
		if blocked_case[0] == 'publish_post' {
			provisional := root.object_value('provisional_published') or {
				panic('publish provisional missing')
			}
			assert schema_subject_digest_lines(provisional) == case.fingerprints.digest_lines
		} else if blocked_case[0] == 'rollback_post' {
			provisional := intent.object_value('rollback_provisional') or {
				panic('rollback provisional missing')
			}
			assert schema_subject_digest_lines(provisional) == case.fingerprints.digest_lines
		} else {
			provisional := intent.object_value('bad_provisional') or {
				panic('failed candidate provisional missing')
			}
			assert schema_subject_digest_lines(provisional) == case.fingerprints.digest_lines
		}
		positive_issues := validate_schema_source_at(authority.root, 'target-state.schema.json',
			case.source, 'blocked-red-${blocked_case[0]}-${blocked_case[2]}-positive')
		assert positive_issues.len == 0, '${blocked_case}: ${positive_issues}'
	}

	candidate := schema_blocked_red_source(authority, 'rollback_candidate', 'candidate_failed',
		'functional')
	publish := schema_blocked_red_source(authority, 'publish_post', 'post_check_infra_exhausted',
		'infrastructure')
	rollback_post := schema_blocked_red_source(authority, 'rollback_post', 'rollback_failed',
		'functional')

	// Every replacement below remains closed-schema JSON. Record mutations are re-sealed and their
	// CAS row is updated, so the oracle reaches the intended semantic join instead of failing on a
	// stale validation digest or an unrelated operation projection.
	wrong_transition := schema_reseal_native_record(candidate.record, 'post_check_red',
		'functional')
	assert_schema_exact_issue(authority, schema_source_with_last_record(candidate.source,
		wrong_transition), 'blocked-red-transition-kind', '$/last_native_validation/transition',
		'last native validation transition differs from its subject and verdict')
	wrong_verdict := schema_reseal_native_record(candidate.record, 'candidate_failed',
		'infrastructure')
	assert_schema_exact_issue(authority, schema_source_with_last_record(candidate.source,
		wrong_verdict), 'blocked-red-verdict-outcome', '$/last_native_validation/verdict',
		'last native validation verdict differs from its matrix and gates')

	// A complete record from the other post branch is internally replayable, but cannot acquire the
	// current rollback owner merely by replacing the durable record and its matching CAS row.
	assert_schema_blocked_red_rejection(authority, schema_source_with_last_record(rollback_post.source,
		publish.record), 'blocked-red-kind-owner')
	wrong_consumer_id := schema_oracle_digest('blocked-red/wrong-consumer')
	wrong_consumer := schema_authority_case(authority, 'publish_post', wrong_consumer_id,
		'post_check_infra_exhausted', 'infrastructure',
		schema_oracle_digest('blocked-red/wrong-consumer/operation'), 9)
	assert_schema_blocked_red_rejection(authority, schema_source_with_last_record(publish.source,
		wrong_consumer.record), 'blocked-red-record-consumer-id')

	publish_record_subject := publish.record.native_lane_matrix.object_value('subject') or {
		panic('publish subject missing')
	}
	publish_consumer := publish_record_subject.object_value('consumer_id') or {
		panic('publish consumer missing')
	}
	publish_subject := schema_blocked_subject('publish_post', publish_consumer.string_value,
		publish.fingerprints)
	wrong_tuple_subject := bin.NativeGateSubjectModel{
		...publish_subject
		sha:     'e'.repeat(40)
		tree:    'f'.repeat(40)
		digests: [
			bin.DigestModel{
				path:   'tcc.exe'
				sha256: schema_oracle_digest('blocked-red/wrong-tuple/tcc.exe')
			},
		]
	}
	wrong_tuple_record := schema_record_for_subject(authority, wrong_tuple_subject,
		'post_check_infra_exhausted', 'infrastructure',
		schema_oracle_digest('blocked-red/wrong-tuple/operation'), 9)
	assert_schema_blocked_red_rejection(authority, schema_source_with_last_record(publish.source,
		wrong_tuple_record), 'blocked-red-record-subject-tuple')

	publish_root := bin.parse_strict_json(publish.source) or { panic(err) }
	wrong_tuple_value := bin.native_validation_record_json(wrong_tuple_record) or { panic(err) }
	wrong_tuple_matrix := wrong_tuple_value.object_value('native_lane_matrix') or {
		panic('wrong tuple matrix missing')
	}
	wrong_tuple_json := wrong_tuple_matrix.object_value('subject') or {
		panic('wrong tuple subject missing')
	}
	wrong_tuple_hash := (wrong_tuple_matrix.object_value('subject_hash') or {
		panic('wrong tuple subject hash missing')
	}).string_value
	mut wrong_active_subject := schema_replace_json_member(publish_root, 'native_gate_subject',
		bin.canonical_json(wrong_tuple_json))
	wrong_active_subject = schema_replace_json_member(wrong_active_subject, 'active_subject_hash',
		'"${wrong_tuple_hash}"')
	assert_schema_blocked_red_rejection(authority, bin.canonical_json(wrong_active_subject),
		'blocked-red-active-subject')
	assert_schema_exact_issue(authority, bin.canonical_json(wrong_active_subject),
		'blocked-red-active-subject-execution', '$/native_gate_execution/subject',
		'native execution subject differs from the authoritative target subject')

	wrong_hash := schema_replace_json_member(publish_root, 'active_subject_hash',
		'"${schema_oracle_digest('blocked-red/wrong-subject-hash')}"')
	assert_schema_exact_issue(authority, bin.canonical_json(wrong_hash),
		'blocked-red-active-subject-hash', '$/active_subject_hash',
		'active subject hash does not match the canonical native subject')
	assert_schema_blocked_red_rejection(authority, bin.canonical_json(wrong_hash),
		'blocked-red-record-subject-hash')

	wrong_gate := schema_source_with_intent_gate_member(publish.source, 0, 'evidence_digest',
		'"${schema_oracle_digest('blocked-red/wrong-gate-evidence')}"')
	assert_schema_blocked_red_rejection(authority, wrong_gate, 'blocked-red-gate-pair')
	wrong_post_consumer := schema_replace_json_member(publish_root, 'post_validation_operation_id',
		'"${schema_oracle_digest('blocked-red/wrong-post-consumer')}"')
	assert_schema_exact_issue(authority, bin.canonical_json(wrong_post_consumer),
		'blocked-red-current-consumer', '$/post_validation_operation_id',
		'post-validation consumer does not own the active native subject and V smoke')

	wrong_run_name := schema_source_with_intent_gate_member(publish.source, 0, 'run_name',
		'"tccbin-native-gate/${schema_oracle_digest('blocked-red/wrong-run-name')}"')
	assert_schema_exact_issue(authority, wrong_run_name, 'blocked-red-run-name',
		'$/active_intent/gate_runs/0/run_name', 'gate run name does not bind the active consumer')
	wrong_external := schema_source_with_intent_gate_member(publish.source, 0, 'external_id',
		'"${schema_oracle_digest('blocked-red/wrong-external')}"')
	assert_schema_exact_issue(authority, wrong_external, 'blocked-red-external-id',
		'$/active_intent/gate_runs/0/external_id',
		'gate check external ID is not the deterministic JCS identity')
	wrong_ref := schema_source_with_intent_gate_member(publish.source, 0, 'ref',
		'"thirdparty-windows-amd64"')
	assert_schema_exact_issue(authority, wrong_ref, 'blocked-red-native-ref',
		'$/active_intent/gate_runs/0',
		'native gate ref, workflow SHA, Actions App or original actor is not allowlisted')
	wrong_winner := schema_source_with_native_execution_member(publish.source, 'selected_run_id',
		'7999')
	assert_schema_exact_issue(authority, wrong_winner, 'blocked-red-selected-winner',
		'$/active_intent/gate_runs/0',
		'native gate check does not project the execution selected run')
	wrong_execution := schema_source_with_observed_native_member(publish.source, 'run_id', '7999')
	assert_schema_exact_issue(authority, wrong_execution, 'blocked-red-observed-execution',
		'$/active_intent/gate_runs/0',
		'native gate check must match exactly one immutable observed native run')

	// The three closed publisher-preserved lanes use complete physical records, gates and
	// executions. Publish-to-rollback remains structurally valid and reaches the semantic
	// classifier. Rollback-to-promotion retains a provisional artifact and is rejected first by the
	// closed publication-state schema. Wrong-kind mutants remain semantic rejects.
	promotion_green := schema_publisher_green_preserved_source(authority, 'publish_candidate')
	rollback_candidate_green := schema_publisher_green_preserved_source(authority,
		'rollback_candidate')
	preserved_green := schema_publisher_green_preserved_source(authority, 'rollback_post')
	for positive in [
		['promotion-publish-candidate', promotion_green],
		['rollback-rollback-candidate', rollback_candidate_green],
		['rollback-rollback-post-history', preserved_green],
	] {
		positive_issues := validate_schema_source_at(authority.root, 'target-state.schema.json',
			positive[1], 'publisher-preserved-${positive[0]}-positive')
		assert positive_issues.len == 0, '${positive[0]}: ${positive_issues}'
	}
	promotion_root := bin.parse_strict_json(promotion_green) or { panic(err) }
	promotion_crossed := schema_replace_json_member(promotion_root, 'publication_state',
		'"rollback_blocked"')
	assert_schema_exact_issue(authority, bin.canonical_json(promotion_crossed),
		'publisher-preserved-promotion-crossed-to-rollback', '$/last_native_validation',
		'blocked target native validation is outside the closed publisher-preserved or red transition classes')
	rollback_candidate_root := bin.parse_strict_json(rollback_candidate_green) or { panic(err) }
	rollback_crossed := schema_replace_json_member(rollback_candidate_root, 'publication_state',
		'"promotion_blocked"')
	assert_schema_exact_issue(authority, bin.canonical_json(rollback_crossed),
		'publisher-preserved-rollback-crossed-to-promotion', '$/publication_state',
		'value is outside the closed enum')
	for forbidden_kind in ['publish_post', 'adopt_current', 'initial_adopt_current'] {
		forbidden_source := schema_publisher_green_preserved_source(authority, forbidden_kind)
		assert_schema_exact_issue(authority, forbidden_source,
			'publisher-preserved-${forbidden_kind}-rejected', '$/last_native_validation',
			'blocked target native validation is outside the closed publisher-preserved or red transition classes')
	}
	preserved_root := bin.parse_strict_json(preserved_green) or { panic(err) }
	preserved_intent := preserved_root.object_value('active_intent') or {
		panic('preserved intent missing')
	}
	historical_gates := preserved_intent.object_value('gate_runs') or {
		panic('historical gates missing')
	}
	red_with_history := schema_source_with_intent_gates(rollback_post.source,
		historical_gates.array_value)
	assert_schema_blocked_red_rejection(authority, red_with_history,
		'blocked-post-red-record-with-historical-gates')
	current_root := bin.parse_strict_json(rollback_post.source) or { panic(err) }
	current_intent := current_root.object_value('active_intent') or {
		panic('current intent missing')
	}
	current_gates := current_intent.object_value('gate_runs') or { panic('current gates missing') }
	green_with_current := schema_source_with_intent_gates(preserved_green,
		current_gates.array_value)
	assert_schema_exact_issue(authority, green_with_current,
		'blocked-post-green-record-with-current-gates', '$/last_native_validation',
		'checked target differs from its durable subject and two gate runs')
	mut null_with_current := current_root
	null_with_current = schema_replace_json_member(null_with_current, 'last_native_validation',
		'null')
	assert_schema_exact_issue(authority, bin.canonical_json(null_with_current),
		'blocked-post-null-history-with-current-gates', '$/active_intent/gate_runs/0/run_name',
		'gate run name does not bind the active consumer')
}

fn h2_schema_valid_gate(subject bin.NativeGateSubjectModel, subject_hash string,
	matrix_digest string, check_name string) bin.PersistedGateRunModel {
	is_native := check_name == 'tccbin-candidate-gate'
	repository := if is_native { 'vlang/tccbin' } else { 'vlang/v' }
	run_id := if is_native { i64(7001) } else { i64(8001) }
	run_attempt := 1
	job_id := if is_native { i64(7201) } else { i64(8201) }
	audience := if is_native {
		'vlang/tccbin:native-gate-check:v1'
	} else {
		'vlang/tccbin:v-smoke-check:v1'
	}
	run_url := 'https://github.com/${repository}/actions/runs/${run_id}'
	job_url := '${run_url}/job/${job_id}'
	return bin.PersistedGateRunModel{
		check_name:                      check_name
		repository:                      repository
		integration_id:                  if is_native { 1001 } else { 1002 }
		workflow_id:                     if is_native { 2001 } else { 2002 }
		workflow_path:                   if is_native {
			'.github/workflows/build-and-test.yml'
		} else {
			'.github/workflows/tccbin_revalidate.yml'
		}
		event:                           if is_native { 'push' } else { 'workflow_dispatch' }
		run_id:                          run_id
		run_attempt:                     run_attempt
		check_suite_id:                  if is_native { 7101 } else { 8101 }
		check_suite_integration_id:      1001
		job_id:                          job_id
		subject_hash:                    subject_hash
		check_run_id:                    if is_native { 7301 } else { 8301 }
		external_id:                     bin.deterministic_check_external_id(audience,
			subject.consumer_id, subject_hash, run_id, run_attempt) or { panic(err) }
		run_name:                        if is_native {
			'tccbin-native-gate/${subject.consumer_id}'
		} else {
			'tccbin-v-smoke/${subject.consumer_id}'
		}
		run_url:                         run_url
		job_url:                         job_url
		details_url:                     job_url
		ref:                             if is_native { subject.original_ref } else { 'master' }
		workflow_head_sha:               if is_native { subject.sha } else { 'b'.repeat(40) }
		sha:                             subject.sha
		check_sha:                       subject.sha
		actor:                           if is_native {
			'tccbin-publisher[bot]'
		} else {
			'validator-dispatcher[bot]'
		}
		actor_integration_id:            if is_native { 1001 } else { 1002 }
		triggering_actor:                if is_native {
			'tccbin-publisher[bot]'
		} else {
			'validator-dispatcher[bot]'
		}
		triggering_actor_integration_id: if is_native { 1001 } else { 1002 }
		created_at:                      '2026-08-03T00:10:00Z'
		completed_at:                    '2026-08-03T01:00:00Z'
		run_conclusion:                  if is_native { 'failure' } else { 'success' }
		check_conclusion:                if is_native { 'failure' } else { 'success' }
		output_digest:                   if is_native { matrix_digest } else { '8'.repeat(64) }
		evidence_digest:                 if is_native { '9'.repeat(64) } else { 'a'.repeat(64) }
	}
}

// H2 must reject non-null T2c2 records at the terminal projection join even when the record is
// strict JSON and satisfies the complete native_validation_record schema. T2c3, not recovery
// JSON, will define how a record is preserved or derived across that CAS.
fn h2_schema_valid_native_validation_record(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	handoffs := root.object_value('recovery_handoffs') or { panic('recovery handoffs missing') }
	subject_value := handoffs.array_value[1].object_value('subject') or {
		panic('recovery subject missing')
	}
	mut digests := []bin.DigestModel{}
	subject_digests := subject_value.object_value('digests') or { panic('subject digests missing') }
	for digest in subject_digests.array_value {
		digests << bin.DigestModel{
			path:   (digest.object_value('path') or { panic('digest path missing') }).string_value
			sha256: (digest.object_value('sha256') or { panic('digest SHA missing') }).string_value
		}
	}
	subject := bin.NativeGateSubjectModel{
		consumer_id:            (subject_value.object_value('consumer_id') or {
			panic('consumer ID missing')
		}).string_value
		consumer_kind:          (subject_value.object_value('consumer_kind') or {
			panic('consumer kind missing')
		}).string_value
		intent_or_operation_id: (subject_value.object_value('intent_or_operation_id') or {
			panic('subject operation missing')
		}).string_value
		target_id:              (subject_value.object_value('target_id') or {
			panic('subject target missing')
		}).string_value
		subject_generation:     (subject_value.object_value('subject_generation') or {
			panic('subject generation missing')
		}).int_value
		initial_run_mode:       (subject_value.object_value('initial_run_mode') or {
			panic('subject run mode missing')
		}).string_value
		sha:                    (subject_value.object_value('sha') or {
			panic('subject SHA missing')
		}).string_value
		tree:                   (subject_value.object_value('tree') or {
			panic('subject tree missing')
		}).string_value
		original_ref:           (subject_value.object_value('original_ref') or {
			panic('subject ref missing')
		}).string_value
		input_fingerprint:      (subject_value.object_value('input_fingerprint') or {
			panic('subject input missing')
		}).string_value
		artifact_fingerprint:   (subject_value.object_value('artifact_fingerprint') or {
			panic('subject artifact missing')
		}).string_value
		manifest_hash:          (subject_value.object_value('manifest_hash') or {
			panic('subject manifest missing')
		}).string_value
		digests:                digests
	}
	profile := bin.parse_strict_json(t2a_profile_source(subject.target_id)) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	producer_source := t2a_producer_observation_source(subject.target_id, profile_sha256)
	producer := bin.parse_strict_json(producer_source) or { panic(err) }
	authority := SyntheticToolchainAuthority{
		target_id:       subject.target_id
		profile_id:      '${subject.target_id}-synthetic-v1'
		profile_sha256:  profile_sha256
		producer_source: producer_source
		producer_sha256: bin.json_sha256(producer)
		producer_digest: (producer.object_value('observation_digest') or {
			panic('producer digest missing')
		}).string_value
	}
	manifest_source := t2a_resolved_manifest_toolchain(schema_fixture('manifest-complete.valid.json'),
		authority)
	matrix_source :=
		t2b_native_matrix_source_for_run(manifest_source, authority, subject, 7001, 1, 7101)
	matrix := bin.parse_strict_json(matrix_source) or { panic(err) }
	matrix_digest := sha256.sum256(matrix_source.bytes()).hex()
	subject_hash := bin.native_gate_subject_hash(subject) or { panic(err) }
	evidence_sources := t2c_native_validation_evidence_sources(matrix_source, authority)
	mut evidence_names := evidence_sources.keys()
	evidence_names.sort()
	mut evidence := []bin.NativeValidationEvidenceModel{cap: evidence_names.len}
	for name in evidence_names {
		evidence << bin.NativeValidationEvidenceModel{
			sha256: name
			size:   u64(evidence_sources[name].bytes().len)
		}
	}
	seed := bin.NativeValidationRecordModel{
		schema_version:       1
		operation_id:         '7'.repeat(64)
		transition:           'post_check_red'
		resulting_generation: 1
		verdict:              'functional'
		manifest_source:      manifest_source
		manifest_hash:        sha256.sum256(manifest_source.bytes()).hex()
		native_lane_matrix:   matrix
		matrix_digest:        matrix_digest
		evidence:             evidence
		capsule_digest:       '6'.repeat(64)
		native_gate:          h2_schema_valid_gate(subject, subject_hash, matrix_digest,
			'tccbin-candidate-gate')
		v_smoke_gate:         h2_schema_valid_gate(subject, subject_hash, matrix_digest,
			'v-candidate-smoke')
		validation_digest:    '0'.repeat(64)
	}
	seed_value := bin.native_validation_record_json(seed) or { panic(err) }
	record := bin.NativeValidationRecordModel{
		...seed
		validation_digest: bin.native_validation_record_digest(seed_value) or { panic(err) }
	}
	return bin.canonical_json(bin.native_validation_record_json(record) or { panic(err) })
}

fn resolved_inputs_value_for_authority(inputs bin.ResolvedInputsModel) bin.JsonValue {
	sources := inputs.sources.map('{' +
		'"id":"${it.id}","repository":"${it.repository}","ref":"${it.ref}","sha":"${it.sha}","tree":"${it.tree}"' +
		'}').join(',')
	checks := inputs.source_checks.map('{' +
		'"source_id":"${it.source_id}","resolved_sha":"${it.resolved_sha}","status":"${it.status}","evidence_digest":"${it.evidence_digest}"' +
		'}').join(',')
	producer := inputs.producer_toolchain
	return bin.parse_strict_json('{"sources":[${sources}],"source_checks":[${checks}],"recipe_path":"${inputs.recipe_path}","recipe_hash":"${inputs.recipe_hash}","contract_repository":"${inputs.contract_repository}","contract_sha":"${inputs.contract_sha}","v_source_sha":"${inputs.v_source_sha}","producer_toolchain":{"profile_id":"${producer.profile_id}","profile_sha256":"${producer.profile_sha256}","observation_sha256":"${producer.observation_sha256}","observation_digest":"${producer.observation_digest}"}}') or {
		panic(err)
	}
}

fn resolved_inputs_source_for_authority(inputs bin.ResolvedInputsModel) string {
	return bin.canonical_json(resolved_inputs_value_for_authority(inputs))
}

fn assert_h2_nonnull_native_validation_rejection(source string, suffix string,
	expected_path string, expected_message string) {
	issues := validate_schema_source('target-state.schema.json', source, suffix)
	assert issues.any(it.path == expected_path && it.message == expected_message), '${suffix}: ${issues}'
}

fn test_v_smoke_dispatch_and_exact_time_proofs_rehash_before_rejection() {
	awaiting := schema_fixture('target-state.v-smoke-awaiting-ack.schema-fixture.json')
	dispatched := schema_fixture('target-state.v-smoke-dispatched.schema-fixture.json')
	terminal := schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')
	retry_awaiting := schema_fixture('target-state.v-smoke-retry-awaiting-ack.schema-fixture.json')
	retry_pending := schema_fixture('target-state.v-smoke-retry-pending.schema-fixture.json')
	run_absent_pending :=
		schema_fixture('target-state.v-smoke-run-absent-retry-pending.schema-fixture.json')
	deadline_exceeded :=
		schema_fixture('target-state.v-smoke-deadline-exceeded-retry-pending.schema-fixture.json')

	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(awaiting.replace_once('"discovery_deadline": "2026-08-03T00:02:00Z"',
		'"discovery_deadline": "2026-08-03T00:02:01Z"')), 'dispatch-window-plus-one',
		'exact two-minute run-discovery window')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(awaiting.replace_once('"requested_at": "2026-08-03T00:00:00Z"',
		'"requested_at": "2026-02-30T00:00:00Z"')), 'dispatch-pseudo-date',
		'dispatch request time must be one exact UTC')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(dispatched.replace_once('"deadline": "2026-08-03T01:31:00Z"',
		'"deadline": "2026-08-03T01:31:01Z"')), 'attempt-window-plus-one',
		'exact 90-minute deadline')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(dispatched.replace_once('"rerunnable_until": "2026-09-02T00:01:00Z"',
		'"rerunnable_until": "2026-09-02T00:01:01Z"')), 'rerun-window-plus-one',
		'immutable 30-day rerun cutoff')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(retry_pending.replace_once('"completed_at": "2026-08-03T01:00:00Z"',
		'"completed_at": "2026-08-03T01:31:01Z"')), 'actions-terminal-after-deadline',
		'actions-terminal no later than its persisted deadline')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(retry_pending.replace_once('"completed_at": "2026-08-03T01:00:00Z"',
		'"completed_at": "2026-08-02T01:00:00Z"')), 'attempt-completion-time',
		'attempt completion must be exact, ordered')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(deadline_exceeded.replace_once('"completed_at": "2026-08-03T01:31:00Z"',
		'"completed_at": "2026-08-03T01:30:00Z"')), 'deadline-before-timeout',
		'respect a persisted deadline timeout')
	rehashed_run_absent := refresh_v_smoke_facts_digests(run_absent_pending.replace_once('"completed_at": "2026-08-03T00:03:00Z"',
		'"completed_at": "2026-08-03T00:02:00Z"'))
	assert_v_smoke_attempt_digests_match(rehashed_run_absent)
	assert_target_semantic_rejection(rehashed_run_absent, 'run-absent-time',
		'cannot precede the exact two-minute discovery deadline')

	mut wrong_within_window := retry_awaiting.replace_once('"mode": "api_rerun"',
		'"mode": "workflow_dispatch"')
	wrong_within_window = wrong_within_window.replace_once('"rerun_of_run_id": 3001',
		'"rerun_of_run_id": null')
	wrong_within_window = wrong_within_window.replace_once('"expected_run_attempt": 2',
		'"expected_run_attempt": 1')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(wrong_within_window),
		'within-window-fresh-dispatch',
		'within 30 days the retry must reserve the exact prior Actions run')
	mut late_api_rerun := replace_nth_json_string_value(retry_awaiting, 'requested_at', 2,
		'2026-09-03T01:00:00Z')
	late_api_rerun = replace_nth_json_string_value(late_api_rerun, 'discovery_deadline', 2,
		'2026-09-03T01:02:00Z')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(late_api_rerun),
		'after-window-api-rerun',
		'after 30 days the retry must reserve one fresh workflow dispatch')
	assert_target_semantic_rejection(terminal.replace_once('"transition": "v-smoke-complete-1"',
		'"transition": "v-smoke-complete-1_not-a-digest"'),
		'completion-transition-malformed-suffix',
		'attempt completion must be one unique later CAS operation committed to the complete historical V-smoke payload')
}

fn test_target_owner_cas_dispatch_and_recovery_histories_are_closed() {
	pending := schema_fixture('target-state.v-smoke-pending.schema-fixture.json')
	awaiting := schema_fixture('target-state.v-smoke-awaiting-ack.schema-fixture.json')
	dispatched := schema_fixture('target-state.v-smoke-dispatched.schema-fixture.json')
	bootstrap := schema_fixture('target-state.bootstrap.schema-fixture.json')
	reservation_id := 'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee'
	dispatch_id := 'd1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1'
	pending_root := bin.parse_strict_json(pending) or { panic(err) }
	canonical_pending := bin.canonical_json(pending_root)
	candidate_without_intent := replace_canonical_root_member(canonical_pending, pending_root,
		'active_intent', 'null')
	candidate_without_intent_issues := validate_schema_source('target-state.schema.json',
		candidate_without_intent, 'candidate-active-intent-required')
	assert candidate_without_intent_issues.any(it.path == '$/active_intent'
		&& it.message == 'expected type "object", got null_value'), '${candidate_without_intent_issues}'
	resolved_inputs_without_root := null_target_object(pending, 'resolved_inputs',
		'last_source_refetch')
	resolved_inputs_without_root_issues := validate_schema_source('target-state.schema.json',
		resolved_inputs_without_root, 'active-intent-without-root-resolved-inputs')
	causal_root_issues := resolved_inputs_without_root_issues.filter(it.path == '$/resolved_inputs'
		&& it.message == 'an active intent requires complete root resolved inputs')
	assert causal_root_issues.len == 1, '${resolved_inputs_without_root_issues}'
	assert !resolved_inputs_without_root_issues.any(it.path == '$/active_intent/resolved_inputs'
		&& it.message == 'expected type "object", got null_value'), '${resolved_inputs_without_root_issues}'
	assert !resolved_inputs_without_root_issues.any(it.path == '$/active_intent'
		&& it.message == 'expected exactly one oneOf branch, got 0'), '${resolved_inputs_without_root_issues}'
	bootstrap_root := bin.parse_strict_json(bootstrap) or { panic(err) }
	canonical_bootstrap := bin.canonical_json(bootstrap_root)
	fingerprint_without_inputs := replace_canonical_root_member(canonical_bootstrap,
		bootstrap_root, 'input_fingerprint', '"${'9'.repeat(64)}"')
	fingerprint_without_inputs_issues := validate_schema_source('target-state.schema.json',
		fingerprint_without_inputs, 'fingerprint-without-root-resolved-inputs')
	assert fingerprint_without_inputs_issues.any(it.path == '$/input_fingerprint'
		&& it.message == 'unresolved target must not retain an input fingerprint'), '${fingerprint_without_inputs_issues}'
	intent_fingerprint_drift := replace_nth_json_string_value(pending, 'input_fingerprint', 2,
		'9'.repeat(64))
	intent_fingerprint_drift_issues := validate_schema_source('target-state.schema.json',
		intent_fingerprint_drift, 'active-intent-fingerprint-root-drift')
	assert intent_fingerprint_drift_issues.any(it.path == '$/active_intent/input_fingerprint'
		&& it.message == 'active intent input fingerprint must equal the complete target root'), '${intent_fingerprint_drift_issues}'
	seeded_bootstrap := replace_canonical_root_member(canonical_bootstrap, bootstrap_root,
		'bootstrap_required', 'false')
	seeded_without_root := replace_canonical_root_member(seeded_bootstrap, bootstrap_root,
		'last_known_good', live_artifact_tuple('c'.repeat(40), 'd'.repeat(40)))
	seeded_without_root_issues := validate_schema_source('target-state.schema.json',
		seeded_without_root, 'seeded-without-root-resolved-inputs')
	assert seeded_without_root_issues.len == 1, '${seeded_without_root_issues}'
	assert seeded_without_root_issues[0].path == '$/resolved_inputs', '${seeded_without_root_issues}'
	assert seeded_without_root_issues[0].message == 'seeded target must retain complete root resolved inputs', '${seeded_without_root_issues}'
	producer_profile_marker := '"profile_sha256": "${'9'.repeat(64)}"'
	assert pending.count(producer_profile_marker) == 2
	producer_root_drift := replace_nth_occurrence(pending, producer_profile_marker,
		'"profile_sha256": "${'8'.repeat(64)}"', 2)
	producer_root_drift_issues := validate_schema_source('target-state.schema.json',
		producer_root_drift, 'active-intent-producer-root-drift')
	assert producer_root_drift_issues.any(it.path == '$/resolved_inputs'
		&& it.message == 'active intent resolved inputs must equal the complete target root'), '${producer_root_drift_issues}'

	assert_target_semantic_rejection(pending.replace_once('"transition": "begin_bootstrap"',
		'"transition": "v-smoke-reserve"'), 'owner-cas-transition',
		'reuse the unique current owner CAS')
	assert_target_semantic_rejection(bootstrap.replace_once('"generation": 0', '"generation": 1'),
		'subject-null-ledger',
		'nonzero target generation must retain its bounded final CAS operation')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(awaiting.replace(dispatch_id,
		reservation_id)), 'duplicate-ledger-operation',
		'applied operation IDs must be globally unique')
	assert_target_semantic_rejection(awaiting.replace_once('"resulting_generation": 2',
		'"resulting_generation": 3'), 'ledger-gap',
		'applied operation generations must be positive, contiguous and never future')
	assert_target_semantic_rejection(awaiting.replace_once('"last_transition": "v-smoke-dispatch-1"',
		'"last_transition": "v-smoke-ack-1"'), 'ledger-last-projection',
		'last operation and transition must project the exact final CAS ledger record')
	assert_target_semantic_rejection(dispatched.replace_once('"transition": "v-smoke-dispatch-1"',
		'"transition": "other-domain"'), 'dispatch-cas-transition',
		'dispatch must be one unique, later and correctly typed pre-side-effect CAS operation')
	mut wrong_dispatch_link := replace_nth_occurrence(dispatched,
		'"dispatch_operation_id": "${dispatch_id}"',
		'"dispatch_operation_id": "d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2"', 2)
	wrong_dispatch_link = refresh_v_smoke_facts_digests(wrong_dispatch_link)
	assert_target_semantic_rejection(wrong_dispatch_link, 'ack-dispatch-link',
		'ACKed attempt must chain the exact prior durable dispatch reservation')

	intent_fingerprint := '"input_fingerprint": "3333333333333333333333333333333333333333333333333333333333333333"'
	assert_target_semantic_rejection(replace_nth_occurrence(pending, intent_fingerprint,
		'"input_fingerprint": "f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3"',
		2), 'intent-owner-tuple',
		'native candidate subject identity, ref, generation or exclusive owner')
	manifest := '"manifest_hash": "5555555555555555555555555555555555555555555555555555555555555555"'
	assert_target_semantic_rejection(replace_nth_occurrence(pending, manifest,
		'"manifest_hash": "f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5"', 2),
		'validation-subject-tuple',
		'native adoption subject must equal the complete durable validation subject tuple')

	recovery := live_target_source(false)
	recovery_issues := validate_schema_source('target-state.schema.json', recovery,
		'recovery-history-positive')
	assert recovery_issues.len == 0, '${recovery_issues}'
	assert_target_semantic_rejection(recovery.replace_once('"active_recovery_handoff_id": "${live_handoff_id}"',
		'"active_recovery_handoff_id": null'), 'recovery-active-complete',
		'recovery history must expose exactly its sole unfinished handoff')
	assert_target_semantic_rejection(live_target_source(true), 'recovery-duplicate-id',
		'recovery handoff IDs must be unique')
	chain := live_recovery_chain_source()
	chain_issues := validate_schema_source('target-state.schema.json', chain,
		'recovery-history-chain-positive')
	assert chain_issues.len == 0, '${chain_issues}'
	chain_root := bin.parse_strict_json(chain) or { panic(err) }
	chain_handoffs := chain_root.object_value('recovery_handoffs') or {
		panic('recovery chain handoffs missing')
	}
	chain_successor_id := (chain_handoffs.array_value[0].object_value('successor_handoff_id') or {
		panic('recovery chain successor pointer missing')
	}).string_value
	broken_chain := replace_nth_occurrence(chain, '"successor_handoff_id":"${chain_successor_id}"',
		'"successor_handoff_id":"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"', 1)
	assert_target_semantic_rejection(broken_chain, 'recovery-successor-reciprocity',
		'recovery successor pointer must resolve exactly once')
	recovery_operation_id := '8888888888888888888888888888888888888888888888888888888888888888'
	chain_predecessor_id := bin.deterministic_handoff_id('vlang/v:tccbin-automation-state',
		recovery_operation_id, live_post_operation_id, live_publish_post_subject_hash(), 0)
	chain_successor_commitment := bin.recovery_native_successor_commitment(chain_handoffs.array_value[0]) or {
		panic(err)
	}
	mut native_loop := replace_nth_occurrence(chain,
		'"consumer_type":"post-validation","resume_capability":"v_smoke","intent_or_operation_id":"${live_post_operation_id}"',
		'"consumer_type":"post-validation","resume_capability":"native_gate","intent_or_operation_id":"${live_post_operation_id}"', 1)
	native_loop = replace_nth_occurrence(native_loop,
		'"receiver_repository":"vlang/v","workflow_id":2002,"workflow_path":".github/workflows/tccbin_revalidate.yml"',
		'"receiver_repository":"vlang/v","workflow_id":1001,"workflow_path":".github/workflows/update_tccbin.yml"', 1)
	assert_target_semantic_rejection(native_loop, 'recovery-native-loop',
		'native recovery successor requires one atomic green post/remediation H1')
	assert validate_schema_source('target-state.schema.json', chain.replace_once('"workflow_path":".github/workflows/tccbin_revalidate.yml"',
		'"workflow_path":".github/workflows/update_tccbin.yml"'),
		'recovery-successor-wrong-workflow').len > 0
	assert_target_semantic_rejection(replace_nth_occurrence(chain,
		'"workflow_id":2002,"workflow_path":".github/workflows/tccbin_revalidate.yml"',
		'"workflow_id":2999,"workflow_path":".github/workflows/tccbin_revalidate.yml"', 2),
		'recovery-successor-wrong-workflow-id',
		'native recovery successor requires one atomic green post/remediation H1')
	assert_target_semantic_rejection(chain.replace('"native_recovery_successor_${chain_successor_commitment}"',
		'"native_recovery_successor_invalid"'), 'recovery-successor-cas-absent',
		'native recovery successor requires one atomic green post/remediation H1')
	assert_target_semantic_rejection(chain.replace_once('"subject_generation":1,"expected_ledger_generation":7',
		'"subject_generation":1,"expected_ledger_generation":6'), 'recovery-successor-generation',
		'native recovery successor requires one atomic green post/remediation H1')
	assert_target_semantic_rejection(replace_nth_occurrence(chain,
		'"subject_generation":1,"expected_ledger_generation":7',
		'"subject_generation":1,"expected_ledger_generation":6', 2),
		'recovery-successor-h2-generation',
		'native recovery successor requires one atomic green post/remediation H1')
	assert_target_semantic_rejection(chain.replace_once('"expected_ledger_generation": 7',
		'"expected_ledger_generation": 6'), 'recovery-successor-execution-generation',
		'native execution CAS generation is stale')
	assert_target_semantic_rejection(chain.replace_once('"selected_conclusion": "success"',
		'"selected_conclusion": "failure"'), 'recovery-successor-native-not-green',
		'native recovery successor requires one atomic green post/remediation H1')
	assert_target_semantic_rejection(chain.replace_once('"native_gate_ack_${live_publish_post_subject_hash()}"',
		'"native_gate_ack_invalid"'), 'recovery-successor-native-cas',
		'native recovery successor requires one atomic green post/remediation H1')
	assert validate_schema_source('target-state.schema.json', chain.replace_once('"terminal_outcome":"native_gate_green_successor"',
		'"terminal_outcome":"green"'), 'recovery-successor-wrong-outcome').len > 0
	assert_target_semantic_rejection(chain.replace_once('"successor_handoff_id":null,"audience":"vlang/v:tccbin-automation-state"',
		'"successor_handoff_id":"${chain_predecessor_id}","audience":"vlang/v:tccbin-automation-state"'),
		'recovery-second-successor-loop',
		'native recovery successor requires one atomic green post/remediation H1')

	h2_awaiting := live_recovery_h2_awaiting_ack_source()
	h2_awaiting_issues := validate_schema_source('target-state.schema.json', h2_awaiting,
		'recovery-h2-awaiting-positive')
	assert h2_awaiting_issues.len == 0, '${h2_awaiting_issues}'
	h2_dispatched := live_recovery_h2_dispatched_source()
	h2_dispatched_issues := validate_schema_source('target-state.schema.json', h2_dispatched,
		'recovery-h2-dispatched-positive')
	assert h2_dispatched_issues.len == 0, '${h2_dispatched_issues}'
	h2_terminal := live_recovery_h2_terminal_source()
	h2_terminal_issues := validate_schema_source('target-state.schema.json', h2_terminal,
		'recovery-h2-terminal-positive')
	assert h2_terminal_issues.len == 0, '${h2_terminal_issues}'
	for terminal_case in [
		['rollback-green', live_recovery_h2_green_source_for('rollback_post')],
		['remediation-green', live_recovery_h2_green_source_for('remediation')],
		['publish-functional', live_recovery_h2_functional_source_for('publish_post')],
		['rollback-functional', live_recovery_h2_functional_source_for('rollback_post')],
		['remediation-functional', live_recovery_h2_functional_source_for('remediation')],
		['publish-functional-adopt-current', live_recovery_h2_publish_adopt_current_source()],
		['publish-infrastructure', live_recovery_h2_infrastructure_source_for('publish_post')],
		['rollback-infrastructure', live_recovery_h2_infrastructure_source_for('rollback_post')],
		['remediation-infrastructure', live_recovery_h2_infrastructure_source_for('remediation')],
		['publish-source-waiting', live_recovery_h2_source_waiting_source_for('publish_post')],
		['publish-source-waiting-daily',
			live_recovery_h2_source_waiting_repeated_daily_source_for('publish_post')],
		['rollback-source-waiting', live_recovery_h2_source_waiting_source_for('rollback_post')],
		['remediation-source-waiting', live_recovery_h2_source_waiting_source_for('remediation')],
	] {
		terminal_issues := validate_schema_source('target-state.schema.json', terminal_case[1],
			'recovery-h2-${terminal_case[0]}-positive')
		assert terminal_issues.len == 0, '${terminal_case[0]}: ${terminal_issues}'
	}
	h2_native_path := '$/recovery_handoffs/1/terminal_revalidation/final_projection/last_native_validation'
	h2_native_message := 'terminal business CAS does not preserve or derive its exact native validation record'
	functional_null := live_recovery_h2_functional_source_for('publish_post')
	infrastructure_null := live_recovery_h2_infrastructure_source_for('publish_post')
	nonnull_record := h2_schema_valid_native_validation_record(functional_null)
	for nonnull_case in [
		['functional', functional_null],
		['infrastructure', infrastructure_null],
	] {
		null_to_nonnull := replace_terminal_final_projection_member(nonnull_case[1],
			'last_native_validation', nonnull_record)
		assert_h2_nonnull_native_validation_rejection(null_to_nonnull,
			'recovery-h2-${nonnull_case[0]}-null-to-nonnull', h2_native_path, h2_native_message)
		nonnull_to_same := replace_terminal_final_projection_member(replace_terminal_pre_projection_member(nonnull_case[1],
			'last_native_validation', nonnull_record), 'last_native_validation', nonnull_record)
		assert_h2_nonnull_native_validation_rejection(nonnull_to_same,
			'recovery-h2-${nonnull_case[0]}-nonnull-to-same', h2_native_path, h2_native_message)
	}
	for null_case in [
		['legacy', h2_terminal],
		['source-waiting', live_recovery_h2_source_waiting_source_for('publish_post')],
	] {
		null_root := bin.parse_strict_json(null_case[1]) or { panic(err) }
		null_handoffs := null_root.object_value('recovery_handoffs') or {
			panic('null-positive handoffs missing')
		}
		null_proof := null_handoffs.array_value[1].object_value('terminal_revalidation') or {
			panic('null-positive terminal proof missing')
		}
		for projection_name in ['pre_business_projection', 'final_projection'] {
			projection := null_proof.object_value(projection_name) or {
				panic('null-positive ${projection_name} missing')
			}
			last_native := projection.object_value('last_native_validation') or {
				panic('null-positive native validation missing')
			}
			assert last_native.kind == .null_value, '${null_case[0]} ${projection_name}'
		}
	}
	retry_ack_h2 := live_recovery_h2_infrastructure_source_for('publish_post')
	retry_ack_issues := validate_schema_source('target-state.schema.json', retry_ack_h2,
		'recovery-h2-two-ack-history-positive')
	assert retry_ack_issues.len == 0, '${retry_ack_issues}'
	retry_ack_root := bin.parse_strict_json(retry_ack_h2) or { panic(err) }
	retry_ack_handoffs := retry_ack_root.object_value('recovery_handoffs') or {
		panic('retry ACK handoffs missing')
	}
	retry_ack_successor := retry_ack_handoffs.array_value[1]
	retry_ack_handoff_id := (retry_ack_successor.object_value('handoff_id') or {
		panic('retry ACK handoff ID missing')
	}).string_value
	retry_ack_subject_hash := (retry_ack_successor.object_value('subject_hash') or {
		panic('retry ACK subject hash missing')
	}).string_value
	unrelated_ack_id := 'f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1'
	mut ack_absent_from_projection := retry_ack_h2.replace('"ack_operation_id":"${live_h2_ack_operation_id}"',
		'"ack_operation_id":"${unrelated_ack_id}"')
	ack_absent_from_projection = ack_absent_from_projection.replace_once('"operation_id":"${live_h2_ack_operation_id}","resulting_generation":9,"transition":"handoff_ack_${retry_ack_handoff_id}"',
		'"operation_id":"${unrelated_ack_id}","resulting_generation":9,"transition":"handoff_ack_${retry_ack_handoff_id}"')
	assert_target_semantic_rejection(ack_absent_from_projection,
		'recovery-h2-ack-absent-from-ordered-projection',
		'attempt ACK is not linked 1:1 to the ordered ACK projection')
	assert_target_semantic_rejection(retry_ack_h2.replace('"ack_operation_ids":["${live_h2_ack_operation_id}","${live_h2_retry_ack_operation_id}"]',
		'"ack_operation_ids":["${live_h2_retry_ack_operation_id}","${live_h2_ack_operation_id}"]'),
		'recovery-h2-ack-order', 'attempt ACK is not linked 1:1 to the ordered ACK projection')
	assert_target_semantic_rejection(retry_ack_h2.replace_once('"operation_id":"${live_h2_ack_operation_id}","resulting_generation":9,"transition":"handoff_ack_${retry_ack_handoff_id}"',
		'"operation_id":"${live_h2_ack_operation_id}","resulting_generation":9,"transition":"handoff_ack_invalid"'),
		'recovery-h2-ack-transition',
		'attempt ACK must be one unique, current and correctly typed later CAS operation')
	assert_target_semantic_rejection(replace_recovery_successor_projection(retry_ack_h2,
		'"subject_hash":"${retry_ack_subject_hash}"',
		'"subject_hash":"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		'recovery-h2-ack-handoff-subject',
		'attempt ACK must be one unique, current and correctly typed later CAS operation')
	historical_h2 := live_recovery_h2_historical_source()
	historical_h2_issues := validate_schema_source('target-state.schema.json', historical_h2,
		'recovery-h2-historical-after-later-transition-positive')
	assert historical_h2_issues.len == 0, '${historical_h2_issues}'

	assert_target_semantic_rejection(h2_awaiting.replace_once('"selected_conclusion":"success"',
		'"selected_conclusion":"failure"'), 'recovery-h2-awaiting-native-red',
		'native recovery successor requires one atomic green post/remediation H1')
	dispatch_history := replace_recovery_successor_projection(h2_awaiting,
		'"dispatch_operation_ids":["${live_h2_dispatch_operation_id}"]',
		'"dispatch_operation_ids":[]')
	dispatch_history_issues := validate_schema_source('target-state.schema.json', dispatch_history,
		'recovery-h2-dispatch-history')
	dispatch_history_message := 'recovery H2 dispatch history must be the exact V-smoke pre-side-effect CAS history'
	assert dispatch_history_issues.any(it.path == '$/recovery_handoffs/1/dispatch_operation_ids'
		&& it.message == dispatch_history_message), '${dispatch_history_issues}'
	assert !dispatch_history_issues.any(it.path == '$/recovery_handoffs/0/dispatch_operation_ids'
		&& it.message == dispatch_history_message), '${dispatch_history_issues}'
	assert_target_semantic_rejection(replace_recovery_successor_projection(h2_dispatched,
		'"ack_operation_id":"${live_h2_ack_operation_id}"',
		'"ack_operation_id":"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		'recovery-h2-ack-binding',
		'recovery H2 ACK must select the exact one durable V-smoke attempt')

	undispatched_terminal := replace_recovery_successor_projection(replace_recovery_successor_projection(h2_terminal,
		'"dispatch_generation":1', '"dispatch_generation":0'),
		'"dispatch_operation_ids":["${live_h2_dispatch_operation_id}"]',
		'"dispatch_operation_ids":[]')
	undispatched_terminal_issues := validate_schema_source('target-state.schema.json',
		undispatched_terminal, 'recovery-h2-terminal-without-dispatch')
	assert undispatched_terminal_issues.any(it.path == '$/recovery_handoffs/1/dispatch_generation'
		&& it.message == 'integer is below 1'), '${undispatched_terminal_issues}'
	assert undispatched_terminal_issues.any(
		it.path == '$/recovery_handoffs/1/dispatch_operation_ids'
		&& it.message == 'array has fewer than 1 items'), '${undispatched_terminal_issues}'

	terminal_for_completion := bin.parse_strict_json(h2_terminal) or { panic(err) }
	terminal_handoffs := terminal_for_completion.object_value('recovery_handoffs') or {
		panic('terminal handoffs missing')
	}
	terminal_proof := terminal_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal proof missing')
	}
	terminal_facts_digest := terminal_proof.object_value('facts_digest') or {
		panic('terminal facts digest missing')
	}
	wrong_completion_transition := h2_terminal.replace('"handoff_complete_${terminal_facts_digest.string_value}"',
		'"handoff_complete_invalid"')
	assert_target_semantic_rejection(wrong_completion_transition, 'recovery-h2-completion-cas',
		'complete recovery handoff must retain its one exact post-ACK completion CAS')
	mut terminal_native_red := replace_recovery_successor_projection(h2_terminal,
		'"selected_conclusion":"success"', '"selected_conclusion":"failure"')
	terminal_native_red = refresh_terminal_revalidation_facts_digest(terminal_native_red)
	assert_target_semantic_rejection(terminal_native_red, 'recovery-h2-terminal-native-red',
		'native recovery successor requires one atomic green post/remediation H1')
	assert_target_semantic_rejection(replace_recovery_successor_projection(h2_terminal,
		'"receiver_output_digest":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"',
		'"receiver_output_digest":"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
		'recovery-h2-terminal-verdict',
		'terminal H2 verdict must equal its exact ACKed and completed V-smoke attempt')
	identity_drift := replace_recovery_successor_root_member(h2_terminal, 'subject_hash',
		'"${'f'.repeat(64)}"')
	identity_drift_issues := validate_schema_source('target-state.schema.json', identity_drift,
		'recovery-h2-terminal-identity')
	identity_drift_message := 'recovery H2 and V-smoke identities must remain one exact consumer and subject'
	assert identity_drift_issues.any(it.path == '$/recovery_handoffs/0/successor_handoff_id'
		&& it.message == identity_drift_message), '${identity_drift_issues}'
	assert !identity_drift_issues.any(it.path == '$/recovery_handoffs/1/successor_handoff_id'
		&& it.message == identity_drift_message), '${identity_drift_issues}'
	terminal_workflow := replace_recovery_successor_root_member(h2_terminal, 'workflow_id', '2999')
	terminal_workflow_issues := validate_schema_source('target-state.schema.json',
		terminal_workflow, 'recovery-h2-terminal-workflow')
	assert terminal_workflow_issues.any(it.path == '$/recovery_handoffs/0/successor_handoff_id'
		&& it.message == 'recovery H2 workflow and immutable V-smoke subject must match exactly'), '${terminal_workflow_issues}'
	assert !terminal_workflow_issues.any(it.path == '$/recovery_handoffs/1/successor_handoff_id'
		&& it.message == 'recovery H2 workflow and immutable V-smoke subject must match exactly'), '${terminal_workflow_issues}'
	assert !terminal_workflow_issues.any(it.path.ends_with('/expected_check_sources')
		|| it.path.ends_with('/pre_business_projection')), '${terminal_workflow_issues}'
	mut wrong_business := replace_recovery_successor_projection(h2_terminal,
		'"business_operation_id":"${live_h2_business_operation_id}"',
		'"business_operation_id":"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"')
	wrong_business = refresh_terminal_revalidation_facts_digest(wrong_business)
	wrong_business_issues := validate_schema_source('target-state.schema.json', wrong_business,
		'recovery-h2-terminal-business-cas')
	wrong_business_message := 'terminal H2 must follow the exact final revalidator business CAS'
	assert wrong_business_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/business_operation_id'
		&& it.message == wrong_business_message), '${wrong_business_issues}'
	assert !wrong_business_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/business_operation_id'
		&& it.message == wrong_business_message), '${wrong_business_issues}'
	terminal_without_proof := replace_recovery_successor_projection(h2_terminal,
		'"terminal_revalidation":${bin.canonical_json(terminal_proof)}',
		'"terminal_revalidation":null')
	terminal_without_proof_issues := validate_schema_source('target-state.schema.json',
		terminal_without_proof, 'recovery-h2-terminal-proof-required')
	assert terminal_without_proof_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation'
		&& it.message == 'expected type "object", got null_value'), '${terminal_without_proof_issues}'

	remediation := live_remediation_source()
	remediation_issues := validate_schema_source('target-state.schema.json', remediation,
		'remediation-binding-positive')
	assert remediation_issues.len == 0, '${remediation_issues}'
	remediation_root := bin.parse_strict_json(remediation) or { panic(err) }
	canonical_remediation := bin.canonical_json(remediation_root)
	remediation_without_binding := replace_canonical_root_member(canonical_remediation,
		remediation_root, 'active_remediation_binding', 'null')
	remediation_without_binding_issues := validate_schema_source('target-state.schema.json',
		remediation_without_binding, 'remediation-binding-required')
	assert remediation_without_binding_issues.any(it.path == '$/active_remediation_binding'
		&& it.message == 'expected type "object", got null_value'), '${remediation_without_binding_issues}'
	candidate_intent := pending_root.object_value('active_intent') or {
		panic('candidate intent missing')
	}
	remediation_with_intent := replace_canonical_root_member(canonical_remediation,
		remediation_root, 'active_intent', bin.canonical_json(candidate_intent))
	remediation_with_intent_issues := validate_schema_source('target-state.schema.json',
		remediation_with_intent, 'remediation-intent-must-be-null')
	assert remediation_with_intent_issues.any(it.path == '$/active_intent'
		&& it.message == 'expected type "null", got object'), '${remediation_with_intent_issues}'
	bad_binding := live_remediation_binding().replace_once('"operation_id":"${live_remediation_operation_id}"',
		'"operation_id":"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"')
	assert_target_semantic_rejection(remediation.replace_once(live_remediation_binding(),
		bad_binding), 'remediation-owner-binding',
		'remediation binding must independently retain the exact operation')
	drifted_sources_binding := live_remediation_binding().replace_once('"integration_id":1001',
		'"integration_id":1999')
	assert_target_semantic_rejection(remediation.replace_once(live_remediation_binding(),
		drifted_sources_binding), 'remediation-owner-sources-drift',
		'remediation binding must independently retain the exact operation, subject, trigger and check authority')
	sources := bin.parse_strict_json(live_remediation_check_sources()) or { panic(err) }
	assert sources.array_value.len == 2
	permuted_sources := '[${bin.canonical_json(sources.array_value[1])},${bin.canonical_json(sources.array_value[0])}]'
	permuted_sources_binding := live_remediation_binding().replace_once(live_remediation_check_sources(),
		permuted_sources)
	assert_target_semantic_rejection(remediation.replace_once(live_remediation_binding(),
		permuted_sources_binding), 'remediation-owner-sources-permutation',
		'remediation binding must independently retain the exact operation, subject, trigger and check authority')
}

fn test_retained_provisional_adopt_current_phase_contract_is_closed() {
	candidate := live_recovery_h2_publish_adopt_current_source()
	candidate_issues := validate_schema_source('target-state.schema.json', candidate,
		'adopt-current-candidate-pending-positive')
	assert candidate_issues.len == 0, '${candidate_issues}'

	waiting := live_adopt_current_waiting_source()
	waiting_issues := validate_schema_source('target-state.schema.json', waiting,
		'adopt-current-waiting-source-positive')
	assert waiting_issues.len == 0, '${waiting_issues}'
	waiting_root := bin.parse_strict_json(waiting) or { panic(err) }
	canonical_waiting := bin.canonical_json(waiting_root)

	wrong_target := replace_canonical_root_member(canonical_waiting, waiting_root, 'target_state',
		'"validating"')
	wrong_target_issues := validate_schema_source('target-state.schema.json', wrong_target,
		'adopt-current-waiting-source-target')
	assert wrong_target_issues.any(it.path == '$/target_state'
		&& it.message == 'value does not match const'), '${wrong_target_issues}'

	intent := waiting_root.object_value('active_intent') or {
		panic('adopt-current intent missing')
	}
	wrong_intent := bin.canonical_json(intent).replace_once('"intent_type":"adopt-current"',
		'"intent_type":"publish"')
	wrong_intent_source := replace_canonical_root_member(canonical_waiting, waiting_root,
		'active_intent', wrong_intent)
	wrong_intent_issues := validate_schema_source('target-state.schema.json', wrong_intent_source,
		'adopt-current-waiting-source-intent')
	assert wrong_intent_issues.any(it.path == '$/active_intent/intent_type'
		&& it.message == 'value does not match const'), '${wrong_intent_issues}'

	subject := waiting_root.object_value('native_gate_subject') or {
		panic('adopt-current subject missing')
	}
	wrong_subject := bin.canonical_json(subject).replace_once('"consumer_kind":"adopt_current"',
		'"consumer_kind":"publish_candidate"')
	wrong_subject_source := replace_canonical_root_member(canonical_waiting, waiting_root,
		'native_gate_subject', wrong_subject)
	wrong_subject_issues := validate_schema_source('target-state.schema.json',
		wrong_subject_source, 'adopt-current-waiting-source-consumer')
	assert wrong_subject_issues.any(it.path == '$/native_gate_subject/consumer_kind'
		&& it.message == 'value does not match const'), '${wrong_subject_issues}'

	wrong_stage := bin.canonical_json(intent).replace_once('"stage":"checks_waiting_source"',
		'"stage":"checks_running"')
	wrong_stage_source := replace_canonical_root_member(canonical_waiting, waiting_root,
		'active_intent', wrong_stage)
	wrong_stage_issues := validate_schema_source('target-state.schema.json', wrong_stage_source,
		'adopt-current-waiting-source-stage')
	assert wrong_stage_issues.any(it.path == '$'
		&& it.message == 'expected exactly one oneOf branch, got 0'), '${wrong_stage_issues}'
}

fn test_terminal_recovery_h2_rejects_every_independent_and_coordinated_drift() {
	h2_terminal := live_recovery_h2_terminal_source()

	wrong_native_generation := replace_terminal_native_evidence(h2_terminal,
		'"expected_ledger_generation":10', '"expected_ledger_generation":7')
	assert_target_semantic_rejection(wrong_native_generation, 'recovery-h2-native-generation',
		'selected completion, business and H2 completion generations must be one exact contiguous CAS chain')
	wrong_smoke_generation := replace_terminal_smoke_projection(h2_terminal,
		'"expected_ledger_generation":10', '"expected_ledger_generation":9')
	assert_target_semantic_rejection(wrong_smoke_generation, 'recovery-h2-smoke-generation',
		'selected completion, business and H2 completion generations must be one exact contiguous CAS chain')
	wrong_business_generation := h2_terminal.replace_once('"transition":"post_check_green","resulting_generation":11',
		'"transition":"post_check_green","resulting_generation":10')
	assert_target_semantic_rejection(wrong_business_generation, 'recovery-h2-business-generation',
		'selected completion, business and H2 completion generations must be one exact contiguous CAS chain')

	for native_case in [
		['native-run-ref', '"ref":"thirdparty-linux-amd64"', '"ref":"thirdparty-other-amd64"'],
		['native-run-sha', '"sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
			'"sha":"ffffffffffffffffffffffffffffffffffffffff"'],
		['native-run-workflow', '"workflow_id":2001', '"workflow_id":2998'],
		['native-run-actor', '"actor":"tccbin-publisher[bot]"', '"actor":"intruder[bot]"'],
		['native-run-integration', '"actor_integration_id":5001', '"actor_integration_id":5999'],
	] {
		mutation := replace_terminal_native_evidence(h2_terminal, native_case[1], native_case[2])
		assert_target_semantic_rejection(mutation, native_case[0],
			'native recovery selected run must retain its exact subject ref, SHA, workflow, actors, Integration IDs and epoch')
	}
	wrong_native_epoch := replace_terminal_native_evidence(h2_terminal, '"active_gate_epoch":0',
		'"active_gate_epoch":1')
	wrong_native_epoch_issues := validate_schema_source('target-state.schema.json',
		wrong_native_epoch, 'native-active-epoch')
	wrong_native_epoch_message := 'native recovery evidence must retain its exact latest contiguous selected epoch'
	assert wrong_native_epoch_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/native_gate_execution/active_gate_epoch'
		&& it.message == wrong_native_epoch_message), '${wrong_native_epoch_issues}'
	assert !wrong_native_epoch_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/native_gate_execution/active_gate_epoch'
		&& it.message == wrong_native_epoch_message), '${wrong_native_epoch_issues}'
	wrong_native_run := replace_terminal_native_evidence(h2_terminal, '"run_id":7002',
		'"run_id":7999')
	assert_target_semantic_rejection(wrong_native_run, 'native-selected-run',
		'native recovery evidence must retain exactly its selected observed run')
	evidence_commitment_message := 'native recovery evidence must equal the immutable predecessor H1 evidence digest'
	wrong_evidence_digest := '"${'f'.repeat(64)}"'
	predecessor_evidence_drift := replace_recovery_predecessor_root_member(h2_terminal,
		'native_gate_evidence_digest', wrong_evidence_digest)
	predecessor_evidence_issues := validate_schema_source('target-state.schema.json',
		predecessor_evidence_drift, 'native-predecessor-evidence-commitment')
	assert predecessor_evidence_issues.any(
		it.path == '$/recovery_handoffs/0/native_gate_evidence_digest'
		&& it.message == evidence_commitment_message), '${predecessor_evidence_issues}'
	assert !predecessor_evidence_issues.any(
		it.path == '$/recovery_handoffs/1/native_gate_evidence_digest'
		&& it.message == evidence_commitment_message), '${predecessor_evidence_issues}'
	successor_evidence_drift := replace_recovery_successor_root_member(h2_terminal,
		'native_gate_evidence_digest', wrong_evidence_digest)
	successor_evidence_issues := validate_schema_source('target-state.schema.json',
		successor_evidence_drift, 'native-successor-evidence-commitment')
	assert successor_evidence_issues.any(
		it.path == '$/recovery_handoffs/1/native_gate_evidence_digest'
		&& it.message == evidence_commitment_message), '${successor_evidence_issues}'
	assert !successor_evidence_issues.any(
		it.path == '$/recovery_handoffs/0/native_gate_evidence_digest'
		&& it.message == evidence_commitment_message), '${successor_evidence_issues}'

	mut coordinated_native_actor := replace_recovery_successor_all(h2_terminal,
		'tccbin-publisher[bot]', 'intruder[bot]')
	coordinated_native_actor = refresh_terminal_revalidation_facts_digest(coordinated_native_actor)
	assert_target_semantic_rejection(coordinated_native_actor, 'native-coordinated-actor',
		'native recovery evidence must equal the immutable predecessor H1 evidence digest')

	terminal_root_for_owner := bin.parse_strict_json(h2_terminal) or { panic(err) }
	terminal_handoffs_for_owner := terminal_root_for_owner.object_value('recovery_handoffs') or {
		panic('terminal owner handoffs missing')
	}
	terminal_proof_for_owner := terminal_handoffs_for_owner.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal owner proof missing')
	}
	terminal_pre_for_owner := terminal_proof_for_owner.object_value('pre_business_projection') or {
		panic('terminal owner pre projection missing')
	}
	terminal_owner := terminal_pre_for_owner.object_value('active_intent') or {
		panic('terminal active owner missing')
	}
	wrong_owner := bin.canonical_json(terminal_owner).replace_once('tccbin-candidate/linux-amd64/${receiver_consumer_id}',
		'tccbin-candidate/linux-amd64/ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff')
	coordinated_owner := replace_terminal_pre_projection_member(h2_terminal, 'active_intent',
		wrong_owner)
	assert_target_semantic_rejection(coordinated_owner, 'owner-coordinated-candidate-ref',
		'terminal owner payload differs from its immutable reservation CAS commitment')

	red_smoke := replace_terminal_smoke_projection(h2_terminal, '"check_conclusion":"success"',
		'"check_conclusion":"failure"')
	assert_target_semantic_rejection(red_smoke, 'recovery-h2-green-on-red-smoke',
		'terminal H2 outcome and business verdict must equal the selected V-smoke run/check logical outcome')

	mut coordinated_source := replace_recovery_successor_all(h2_terminal, '"workflow_id":2002',
		'"workflow_id":2999')
	coordinated_source = refresh_terminal_revalidation_smoke_digests(coordinated_source)
	assert_target_semantic_rejection(coordinated_source, 'recovery-h2-coordinated-source',
		'terminal H2 check authority must equal the immutable predecessor H1 authority')

	mut coordinated_smoke_payload := replace_recovery_successor_all(h2_terminal,
		'"output_digest":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"',
		'"output_digest":"edededededededededededededededededededededededededededededededed"')
	coordinated_smoke_payload =
		refresh_terminal_revalidation_smoke_digests(coordinated_smoke_payload)
	assert_target_semantic_rejection(coordinated_smoke_payload,
		'recovery-h2-coordinated-smoke-payload',
		'attempt completion must be one unique later CAS operation committed to the complete historical V-smoke payload')

	assert_target_semantic_rejection(h2_terminal.replace_once('"target_state":"eligible"',
		'"target_state":"quarantined"'), 'recovery-h2-final-target',
		'terminal business CAS must project the exact target')
	terminal_root := bin.parse_strict_json(h2_terminal) or { panic(err) }
	last_known_good := terminal_root.object_value('last_known_good') or {
		panic('terminal last-known-good missing')
	}
	wrong_last_known_good := bin.canonical_json(last_known_good).replace_once('"sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"sha":"ffffffffffffffffffffffffffffffffffffffff"')
	assert_target_semantic_rejection(replace_canonical_root_member(h2_terminal, terminal_root,
		'last_known_good', wrong_last_known_good), 'recovery-h2-final-artifact',
		'terminal business CAS must project the exact target')
	last_validation := terminal_root.object_value('last_validation') or {
		panic('terminal last validation missing')
	}
	wrong_last_validation := bin.canonical_json(last_validation).replace_once('"conclusion":"success"',
		'"conclusion":"failure"')
	canonical_terminal := bin.canonical_json(terminal_root)
	assert_target_semantic_rejection(replace_canonical_root_member(canonical_terminal,
		terminal_root, 'last_validation', wrong_last_validation), 'recovery-h2-final-validation',
		'current terminal H2 snapshot differs from the authoritative current target root')

	handoffs := terminal_root.object_value('recovery_handoffs') or {
		panic('terminal recovery handoffs missing')
	}
	successor := handoffs.array_value[1]
	successor_id := successor.object_value('handoff_id') or {
		panic('terminal recovery successor ID missing')
	}
	leaked_owner := replace_terminal_final_projection_member(h2_terminal,
		'active_recovery_handoff_id', '"${successor_id.string_value}"')
	assert_target_semantic_rejection(leaked_owner, 'recovery-h2-final-owner',
		'terminal business CAS must project the exact target')

	assert_target_semantic_rejection(replace_recovery_successor_projection(h2_terminal,
		'"receiver_master_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"receiver_master_sha":"ffffffffffffffffffffffffffffffffffffffff"'),
		'recovery-h2-receiver-master',
		'recovery H2 receiver SHA and deadline must equal its selected V-smoke reservation and observed run')
	assert_target_semantic_rejection(replace_recovery_successor_projection(h2_terminal,
		'"deadline":"2026-08-03T01:31:00Z"', '"deadline":"2026-08-03T01:32:00Z"'),
		'recovery-h2-receiver-deadline',
		'recovery H2 receiver SHA and deadline must equal its selected V-smoke reservation and observed run')
	missing_deadline := replace_recovery_successor_projection(h2_terminal,
		'"deadline":"2026-08-03T01:31:00Z"', '"deadline":null')
	missing_deadline_issues := validate_schema_source('target-state.schema.json', missing_deadline,
		'recovery-h2-complete-deadline-required')
	assert missing_deadline_issues.any(it.path == '$/recovery_handoffs/1/deadline'), '${missing_deadline_issues}'
}

fn test_terminal_recovery_h2_review_regression_matrix_is_discriminating() {
	h2_terminal := live_recovery_h2_terminal_source()
	coordinated_smoke_drift := replace_terminal_historical_smoke_created_at(h2_terminal,
		'2026-08-02T23:59:59Z')
	assert_target_semantic_rejection(coordinated_smoke_drift,
		'recovery-h2-smoke-v4-full-payload-commitment',
		'attempt completion must be one unique later CAS operation committed to the complete historical V-smoke payload')
	h2_root := bin.parse_strict_json(h2_terminal) or { panic(err) }
	h2_handoffs := h2_root.object_value('recovery_handoffs') or { panic('handoffs missing') }
	h2_proof := h2_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('terminal proof missing')
	}
	h2_smoke := h2_proof.object_value('v_smoke_execution') or { panic('terminal smoke missing') }
	h2_attempts := h2_smoke.object_value('attempts') or { panic('terminal attempts missing') }
	old_payload_digest := bin.v_smoke_terminal_payload_digest(h2_smoke, h2_attempts.array_value[0]) or {
		panic(err)
	}
	old_attempt_digest := (h2_attempts.array_value[0].object_value('completion_facts_digest') or {
		panic('attempt completion digest missing')
	}).string_value
	assert_target_semantic_rejection(h2_terminal.replace_once('v-smoke-complete-1_${old_payload_digest}',
		'v-smoke-complete-1_${old_attempt_digest}'), 'recovery-h2-smoke-old-attempt-digest-suffix',
		'attempt completion must be one unique later CAS operation committed to the complete historical V-smoke payload')
	for check_case in [
		['check-run-id', '"check_run_id":7302', '"check_run_id":7399'],
		['check-conclusion', '"check_conclusion":"success"', '"check_conclusion":"failure"'],
		['check-integration', '"integration_id":1001', '"integration_id":1003'],
	] {
		mutation := replace_terminal_native_check(h2_terminal, check_case[1], check_case[2])
		assert_target_semantic_rejection(mutation, 'recovery-h2-native-${check_case[0]}',
			'terminal native check differs from the H1/H2 append-only check commitment')
	}

	mut coordinated_sources := h2_terminal.replace('"workflow_id":2001', '"workflow_id":2998')
	coordinated_sources = refresh_terminal_handoff_native_digests(coordinated_sources)
	assert_target_semantic_rejection(coordinated_sources,
		'recovery-h2-coordinated-h1-check-authority',
		'first recovery handoff must retain its unique pre-dispatch creation CAS')

	mut coordinated_native := replace_recovery_successor_all(h2_terminal, 'tccbin-publisher[bot]',
		'replacement-native-app[bot]')
	coordinated_native = refresh_terminal_handoff_native_digests(coordinated_native)
	assert_target_semantic_rejection(coordinated_native,
		'recovery-h2-coordinated-native-commitments',
		'native recovery successor requires one atomic green post/remediation H1')

	source_waiting := live_recovery_h2_source_waiting_source_for('publish_post')
	source_resolution_operation_id := terminal_source_resolution_operation_id(source_waiting)
	missing_terminal_time := replace_recovery_successor_projection(source_waiting,
		'"terminal_completed_at":"2026-08-03T02:01:02Z",', '')
	missing_terminal_time_issues := validate_schema_source('target-state.schema.json',
		missing_terminal_time, 'recovery-h2-source-terminal-time-required')
	assert missing_terminal_time_issues.any(it.path == '$/recovery_handoffs/1'
		&& it.message == 'missing required property terminal_completed_at'), '${missing_terminal_time_issues}'
	assert_target_semantic_rejection(replace_recovery_successor_projection(source_waiting,
		'"terminal_completed_at":"2026-08-03T02:01:02Z"',
		'"terminal_completed_at":"2026-02-30T02:01:02Z"'),
		'recovery-h2-source-terminal-time-exact',
		'source_waiting completion time must be one exact UTC RFC3339 second')
	assert_target_semantic_rejection(replace_recovery_successor_projection(source_waiting,
		'"terminal_completed_at":"2026-08-03T02:01:02Z"',
		'"terminal_completed_at":"2026-08-03T02:01:00Z"'),
		'recovery-h2-source-terminal-after-refetch',
		'source_waiting terminal completion must be strictly later than the source refetch')
	assert_target_semantic_rejection(replace_terminal_source_refetch_with_null(source_waiting),
		'recovery-h2-source-waiting-without-refetch',
		'source_waiting requires one explicit refetch plus independent pre/post source-state snapshots and exactly one fully evidenced source CAS')
	assert_target_semantic_rejection(replace_terminal_source_state(source_waiting,
		'"generation":1', '"generation":2'), 'recovery-h2-source-state-generation',
		'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS')
	assert_target_semantic_rejection(replace_terminal_source_state(source_waiting,
		'"status":"source_unreachable"', '"status":"resolved"'), 'recovery-h2-source-state-status',
		'source_waiting proof must identify one prior resolved input')
	assert_target_semantic_rejection(replace_terminal_source_state(source_waiting,
		'"waiting_consumers":["${live_post_operation_id}"]',
		'"waiting_consumers":["ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"]'),
		'recovery-h2-source-state-consumer',
		'source_waiting proof must identify one prior resolved input')
	assert_target_semantic_rejection(replace_terminal_source_state(source_waiting,
		'"operation_count":1', '"operation_count":2'), 'recovery-h2-source-state-operation',
		'source_waiting proof must identify one prior resolved input')
	assert_target_semantic_rejection(replace_terminal_proof_member(source_waiting,
		'source_state_cas_history', '[]'), 'recovery-h2-source-history-required',
		'source_waiting requires one explicit refetch plus independent pre/post source-state snapshots and exactly one fully evidenced source CAS')
	source_root := bin.parse_strict_json(source_waiting) or { panic(err) }
	source_handoffs := source_root.object_value('recovery_handoffs') or {
		panic('source handoffs missing')
	}
	source_proof := source_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('source proof missing')
	}
	atomic_projection := source_proof.object_value('source_atomic_pre_projection') or {
		panic('source atomic parent projection missing')
	}
	assert (atomic_projection.object_value('generation') or {
		panic('source atomic parent generation missing')
	}).int_value == 9
	assert_target_semantic_rejection(replace_terminal_proof_member(source_waiting,
		'source_atomic_pre_projection', 'null'), 'recovery-h2-source-atomic-parent-absent',
		'source_waiting must retain the non-authoritative projection of its real Git parent')
	wrong_atomic_projection := bin.canonical_json(atomic_projection).replace_once('"generation":9',
		'"generation":8')
	assert_target_semantic_rejection(replace_terminal_proof_member(source_waiting,
		'source_atomic_pre_projection', wrong_atomic_projection),
		'recovery-h2-source-atomic-parent-generation',
		'source atomic parent must be the exact dispatched generation immediately before V-smoke completion')
	source_smoke := source_proof.object_value('v_smoke_execution') or {
		panic('source smoke missing')
	}
	assert (source_smoke.object_value('dispatches') or { panic('source dispatches missing') }).array_value.len == 1
	assert (source_smoke.object_value('attempts') or { panic('source attempts missing') }).array_value.len == 1
	assert (source_smoke.object_value('infra_retry_count') or {
		panic('source retry count missing')
	}).int_value == 0
	source_pre_state := source_proof.object_value('source_state_pre_snapshot') or {
		panic('pre source state missing')
	}
	source_post_state := source_proof.object_value('source_state_snapshot') or {
		panic('post source state missing')
	}
	stale_pre_state := bin.canonical_json(source_pre_state).replace_once('"last_attempt_at":"2026-08-02T02:01:00Z"',
		'"last_attempt_at":"2026-08-03T02:01:00Z"')
	mut coordinated_stale_source := replace_terminal_proof_member(source_waiting,
		'source_state_pre_snapshot', stale_pre_state)
	coordinated_stale_source = refresh_terminal_source_evidence_digests(coordinated_stale_source)
	assert_target_semantic_rejection(coordinated_stale_source,
		'recovery-h2-source-coordinated-stale-pre-state',
		'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS')
	preloaded_pre_state := bin.canonical_json(source_pre_state).replace_once('"operation_count":0',
		'"operation_count":1')
	mut coordinated_non_append_source := replace_terminal_proof_member(source_waiting,
		'source_state_pre_snapshot', preloaded_pre_state)
	coordinated_non_append_source =
		refresh_terminal_source_evidence_digests(coordinated_non_append_source)
	assert_target_semantic_rejection(coordinated_non_append_source,
		'recovery-h2-source-coordinated-non-append-history',
		'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS')
	for source_case in [
		['url', '"canonical_url":"https://repo.or.cz/tinycc.git"',
			'"canonical_url":"https://example.invalid/tinycc.git"'],
		['ref', '"ref":"mob"', '"ref":"other"'],
		['last-attempt', '"last_attempt_at":"2026-08-03T02:01:00Z"',
			'"last_attempt_at":"2026-08-03T02:02:00Z"'],
		['fingerprint',
			'"source_fingerprint":"1111111111111111111111111111111111111111111111111111111111111111"',
			'"source_fingerprint":"2222222222222222222222222222222222222222222222222222222222222222"'],
		['originating-run', '"originating_run_id":8001', '"originating_run_id":8002'],
	] {
		mut coordinated_source_drift := replace_terminal_proof_member(source_waiting,
			'source_state_snapshot', bin.canonical_json(source_post_state).replace_once(source_case[1],
			source_case[2]))
		coordinated_source_drift =
			refresh_terminal_source_evidence_digests(coordinated_source_drift)
		assert_target_semantic_rejection(coordinated_source_drift,
			'recovery-h2-source-coordinated-${source_case[0]}',
			'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS')
	}
	mut coordinated_mode_drift := replace_terminal_proof_member(source_waiting,
		'source_state_snapshot', bin.canonical_json(source_post_state).replace_once('"mode":"upstream-recovery-daily"',
		'"mode":"monthly"'))
	coordinated_mode_drift = refresh_terminal_source_evidence_digests(coordinated_mode_drift)
	coordinated_mode_issues := validate_schema_source('target-state.schema.json',
		coordinated_mode_drift, 'recovery-h2-source-coordinated-mode')
	assert coordinated_mode_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/source_state_snapshot'
		&& it.message == 'expected exactly one oneOf branch, got 0'), '${coordinated_mode_issues}'
	source_waiting_with_infrastructure_retry := live_recovery_h2_source_waiting_variant_for('publish_post',
		true, false)
	source_waiting_retry_issues := validate_schema_source('target-state.schema.json',
		source_waiting_with_infrastructure_retry,
		'recovery-h2-source-does-not-consume-infrastructure-retry')
	assert source_waiting_retry_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/v_smoke_execution'
		&& it.message == 'source waiting must stop after the first infrastructure observation without consuming the CI infrastructure retry'), '${source_waiting_retry_issues}'
	assert !source_waiting_retry_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/v_smoke_execution'
		&& it.message == 'source waiting must stop after the first infrastructure observation without consuming the CI infrastructure retry'), '${source_waiting_retry_issues}'
	assert !source_waiting_retry_issues.any(it.message == 'expected exactly one oneOf branch, got 0'), '${source_waiting_retry_issues}'
	mut refetch_not_later := source_waiting.replace('2026-08-03T02:01:00Z', '2026-08-03T01:00:00Z')
	refetch_not_later = refresh_terminal_source_evidence_digests(refetch_not_later)
	assert_target_semantic_rejection(refetch_not_later,
		'recovery-h2-source-refetch-equals-smoke-completion',
		'source refetch must be strictly later than the selected terminal V-smoke completion')
	repeated_daily := live_recovery_h2_source_waiting_repeated_daily_source_for('publish_post')
	mut daily_too_early := repeated_daily.replace('2026-08-04T02:01:00Z', '2026-08-04T01:01:00Z')
	daily_too_early = refresh_terminal_source_evidence_digests(daily_too_early)
	assert_target_semantic_rejection(daily_too_early, 'recovery-h2-source-daily-before-24-hours',
		'daily source resolution cannot run before the exact 24-hour recovery cadence')
	invalid_source_time := source_waiting.replace('2026-08-03T02:01:00Z', '2026-02-30T02:01:00Z')
	assert_target_semantic_rejection(invalid_source_time, 'recovery-h2-source-impossible-date',
		'source post-state attempt time must be one exact UTC RFC3339 second')
	fictive_operation := 'f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0'
	mut fictive_source_operation := source_waiting.replace(source_resolution_operation_id,
		fictive_operation)
	fictive_source_operation = refresh_terminal_source_evidence_digests(fictive_source_operation)
	assert_target_semantic_rejection(fictive_source_operation,
		'recovery-h2-source-fictive-operation-without-transition',
		'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS')
	old_previous_sha := '"previous_sha":"cccccccccccccccccccccccccccccccccccccccc"'
	new_previous_sha := '"previous_sha":"ffffffffffffffffffffffffffffffffffffffff"'
	assert source_waiting.count(old_previous_sha) == 3
	mut wrong_source := source_waiting.replace(old_previous_sha, new_previous_sha)
	assert wrong_source.count(old_previous_sha) == 0
	assert wrong_source.count(new_previous_sha) == 3
	wrong_source = refresh_terminal_source_evidence_digests(wrong_source)
	wrong_source_issues := validate_schema_source('target-state.schema.json', wrong_source,
		'recovery-h2-source-waiting-unbound-source')
	assert wrong_source_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/source_refetch'
		&& it.message == 'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS from the independent pre-state to the exact durable outage state'), '${wrong_source_issues}'
	assert !wrong_source_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/source_refetch'
		&& it.message == 'source_waiting proof must identify one prior resolved input and a fresh append-only source CAS from the independent pre-state to the exact durable outage state'), '${wrong_source_issues}'
	assert !wrong_source_issues.any(it.message == 'current terminal H2 snapshot differs from the authoritative current target root'), '${wrong_source_issues}'

	publish_infra := live_recovery_h2_infrastructure_source_for('publish_post')
	mut infrastructure_as_source := replace_recovery_successor_root_member(publish_infra,
		'terminal_outcome', '"source_waiting"')
	infrastructure_as_source = replace_recovery_successor_root_member(infrastructure_as_source,
		'terminal_completed_at', '"2026-08-03T02:01:02Z"')
	infrastructure_as_source_issues := validate_schema_source('target-state.schema.json',
		infrastructure_as_source, 'recovery-h2-infrastructure-is-not-source')
	assert infrastructure_as_source_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/business_operation_id'
		&& it.message == 'terminal H2 must follow the exact final revalidator business CAS'), '${infrastructure_as_source_issues}'
	assert !infrastructure_as_source_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/business_operation_id'
		&& it.message == 'terminal H2 must follow the exact final revalidator business CAS'), '${infrastructure_as_source_issues}'
	assert !infrastructure_as_source_issues.any(it.path == '$/recovery_handoffs/1/terminal_completed_at'), '${infrastructure_as_source_issues}'
	assert !infrastructure_as_source_issues.any(it.message == 'expected exactly one oneOf branch, got 0'), '${infrastructure_as_source_issues}'
	mut source_as_infrastructure := replace_recovery_successor_root_member(source_waiting,
		'terminal_outcome', '"infrastructure_blocked"')
	source_as_infrastructure = replace_recovery_successor_root_member(source_as_infrastructure,
		'terminal_completed_at', 'null')
	source_as_infrastructure_issues := validate_schema_source('target-state.schema.json',
		source_as_infrastructure, 'recovery-h2-source-is-not-generic-infrastructure')
	assert source_as_infrastructure_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/source_refetch'
		&& it.message == 'non-source terminal outcomes cannot invent a refetch, source-state CAS history, or source outage replacement'), '${source_as_infrastructure_issues}'
	assert !source_as_infrastructure_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/source_refetch'
		&& it.message == 'non-source terminal outcomes cannot invent a refetch, source-state CAS history, or source outage replacement'), '${source_as_infrastructure_issues}'
	assert !source_as_infrastructure_issues.any(it.path == '$/recovery_handoffs/1/terminal_completed_at'), '${source_as_infrastructure_issues}'
	assert !source_as_infrastructure_issues.any(it.message == 'expected exactly one oneOf branch, got 0'), '${source_as_infrastructure_issues}'
	unexhausted_infrastructure_issues := validate_schema_source('target-state.schema.json',
		source_as_infrastructure, 'recovery-h2-infrastructure-retry-not-exhausted')
	assert unexhausted_infrastructure_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/v_smoke_execution'
		&& it.message == 'terminal infrastructure routing requires both bounded logical attempts to finish as infrastructure with the single retry consumed'), '${unexhausted_infrastructure_issues}'
	assert !unexhausted_infrastructure_issues.any(
		it.path == '$/recovery_handoffs/0/terminal_revalidation/v_smoke_execution'
		&& it.message == 'terminal infrastructure routing requires both bounded logical attempts to finish as infrastructure with the single retry consumed'), '${unexhausted_infrastructure_issues}'
	assert !unexhausted_infrastructure_issues.any(it.message == 'expected exactly one oneOf branch, got 0'), '${unexhausted_infrastructure_issues}'

	publish_red := live_recovery_h2_functional_source_for('publish_post')
	assert_target_semantic_rejection(replace_terminal_pre_projection_member(publish_red,
		'target_state', '"quarantined"'), 'recovery-h2-publish-red-crossed-pre-state',
		'terminal pre-business snapshot must retain the exact selected subject')
	publish_root := bin.parse_strict_json(publish_red) or { panic(err) }
	publish_handoffs := publish_root.object_value('recovery_handoffs') or {
		panic('publish red handoffs missing')
	}
	publish_proof := publish_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('publish red proof missing')
	}
	publish_final := publish_proof.object_value('final_projection') or {
		panic('publish red final projection missing')
	}
	publish_lkg := publish_final.object_value('last_known_good') or {
		panic('publish red LKG missing')
	}
	wrong_publish_lkg := bin.canonical_json(publish_lkg).replace_once('"sha":"cccccccccccccccccccccccccccccccccccccccc"',
		'"sha":"ffffffffffffffffffffffffffffffffffffffff"')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(publish_red,
		'last_known_good', wrong_publish_lkg), 'recovery-h2-post-red-lkg-drift',
		'terminal business CAS must project the exact target')
	publish_provisional := publish_final.object_value('provisional_published') or {
		panic('publish red provisional missing')
	}
	wrong_publish_provisional := bin.canonical_json(publish_provisional).replace_once('"tree":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"',
		'"tree":"ffffffffffffffffffffffffffffffffffffffff"')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(publish_red,
		'provisional_published', wrong_publish_provisional),
		'recovery-h2-post-red-provisional-drift',
		'terminal business CAS must project the exact target')

	rollback_red := live_recovery_h2_functional_source_for('rollback_post')
	assert_target_semantic_rejection(replace_terminal_pre_projection_member(rollback_red,
		'publication_state', '"post_publish_validating"'),
		'recovery-h2-rollback-red-crossed-pre-state',
		'terminal pre-business snapshot must retain the exact selected subject')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(rollback_red,
		'canonical_observed_sha', '"ffffffffffffffffffffffffffffffffffffffff"'),
		'recovery-h2-rollback-red-root-tuple-drift',
		'terminal business CAS must project the exact target')

	remediation_red := live_recovery_h2_functional_source_for('remediation')
	assert_target_semantic_rejection(replace_terminal_pre_projection_member(remediation_red,
		'target_state', '"quarantined"'), 'recovery-h2-remediation-red-crossed-pre-state',
		'terminal pre-business snapshot must retain the exact selected subject')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(remediation_red,
		'active_remediation_id', '"${live_remediation_operation_id}"'),
		'recovery-h2-remediation-red-owner-drift',
		'terminal business CAS must project the exact target')

	adopt_red := live_recovery_h2_publish_adopt_current_source()
	adopt_root := bin.parse_strict_json(adopt_red) or { panic(err) }
	adopt_handoffs := adopt_root.object_value('recovery_handoffs') or {
		panic('adopt handoffs missing')
	}
	adopt_proof := adopt_handoffs.array_value[1].object_value('terminal_revalidation') or {
		panic('adopt proof missing')
	}
	adopt_final := adopt_proof.object_value('final_projection') or {
		panic('adopt final projection missing')
	}
	adopt_head := adopt_final.object_value('last_head_observation') or {
		panic('adopt HEAD observation missing')
	}
	wrong_relationship := bin.canonical_json(adopt_head).replace_once('"relationship":"subject_ancestor"',
		'"relationship":"exact_subject"')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(adopt_red,
		'last_head_observation', wrong_relationship), 'recovery-h2-adopt-head-relationship',
		'terminal business CAS must project the exact target')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(adopt_red,
		'canonical_observed_sha', '"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"'),
		'recovery-h2-adopt-new-head', 'terminal business CAS must project the exact target')
	adopt_intent := adopt_final.object_value('active_intent') or { panic('adopt intent missing') }
	wrong_validation := bin.canonical_json(adopt_intent).replace_once('"sha":"ffffffffffffffffffffffffffffffffffffffff"',
		'"sha":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(adopt_red,
		'active_intent', wrong_validation), 'recovery-h2-adopt-validation-subject',
		'terminal business CAS must project the exact target')
	assert_target_semantic_rejection(replace_terminal_final_projection_member(adopt_red,
		'native_subject_hash', '"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"'),
		'recovery-h2-adopt-subject-hash', 'terminal business CAS must project the exact target')
	assert_target_semantic_rejection(replace_terminal_proof_member(adopt_red, 'git_ancestry_proof',
		'null'), 'recovery-h2-adopt-ancestry-required',
		'terminal business CAS must project the exact target')
	coordinated_unrelated_merge_base := replace_terminal_ancestry_coordinated(adopt_red,
		'"merge_base_sha":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"merge_base_sha":"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"')
	assert_target_semantic_rejection(coordinated_unrelated_merge_base,
		'recovery-h2-adopt-unrelated-merge-base',
		'terminal business CAS must project the exact target')
	adopt_ancestry := adopt_proof.object_value('git_ancestry_proof') or {
		panic('adopt ancestry proof missing')
	}
	unrelated_ancestry := bin.canonical_json(adopt_ancestry).replace_once('"relationship":"subject_ancestor"',
		'"relationship":"unrelated"')
	unrelated_ancestry_source := replace_terminal_proof_member(adopt_red, 'git_ancestry_proof',
		unrelated_ancestry)
	unrelated_ancestry_issues := validate_schema_source('target-state.schema.json',
		unrelated_ancestry_source, 'recovery-h2-adopt-unrelated-relationship')
	assert unrelated_ancestry_issues.any(
		it.path == '$/recovery_handoffs/1/terminal_revalidation/git_ancestry_proof'
		&& it.message == 'expected exactly one oneOf branch, got 0'), '${unrelated_ancestry_issues}'
}

fn test_infrastructure_check_conclusions_require_the_one_retry() {
	terminal := schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')
	for conclusion in ['cancelled', 'timed_out', 'neutral', 'skipped'] {
		mut infra := replace_nth_json_string_value(terminal, 'check_conclusion', 2, conclusion)
		infra = replace_nth_json_string_value(infra, 'check_conclusion', 3, conclusion)
		assert_target_semantic_rejection(refresh_v_smoke_facts_digests(infra),
			'check-infra-${conclusion}',
			'completed V smoke requires a timely selected run and validator check both green')
	}

	mut functional := schema_fixture('target-state.v-smoke-retry-pending.schema-fixture.json')
	functional = functional.replace_once('"state": "pending"', '"state": "blocked"')
	functional = functional.replace_once('"run_conclusion": "success"',
		'"run_conclusion": "failure"')
	functional = functional.replace_once('"infra_retry_count": 1', '"infra_retry_count": 0')
	functional = refresh_v_smoke_facts_digests(functional)
	functional_issues := validate_schema_source('target-state.schema.json', functional,
		'run-failure-without-check-is-functional')
	assert functional_issues.len == 0, '${functional_issues}'
}

fn test_gate_history_and_post_round_trip_keep_candidate_proofs_green() {
	terminal := schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')
	checks_green := terminal.replace_once('"stage": "checks_running"', '"stage": "checks_green"')
	checks_green_issues := validate_schema_source('target-state.schema.json', checks_green,
		'checks-green-positive')
	assert checks_green_issues.len == 0, '${checks_green_issues}'
	assert_target_semantic_rejection(checks_green.replace_once('"run_conclusion": "success"',
		'"run_conclusion": "failure"'), 'checks-green-run-red',
		'green or retained historical gates require both run and check conclusions success')

	for post_case in [
		['publish', live_publish_post_source(), 'post_publish_validating',
			'post_publish_waiting_source', 'post_publish_blocked'],
		['rollback', live_rollback_post_source(), 'rollback_pending', 'rollback_waiting_source',
			'rollback_blocked'],
	] {
		post := post_case[1]
		post_issues := validate_schema_source('target-state.schema.json', post,
			'${post_case[0]}-post-history-positive')
		assert post_issues.len == 0, '${post_case[0]}: ${post_issues}'
		waiting_post := post.replace_once('"stage": "post_checks_running"',
			'"stage": "post_checks_waiting_source"').replace_once('"publication_state": "${post_case[2]}"',
			'"publication_state": "${post_case[3]}"')
		waiting_issues := validate_schema_source('target-state.schema.json', waiting_post,
			'${post_case[0]}-post-waiting-positive')
		assert waiting_issues.len == 0, '${post_case[0]} waiting: ${waiting_issues}'
		mut blocked_post := post.replace_once('"stage": "post_checks_running"',
			'"stage": "blocked"')
		blocked_post = blocked_post.replace_once('"publication_state": "${post_case[2]}"',
			'"publication_state": "${post_case[4]}"')
		if post_case[0] == 'publish' {
			blocked_post = blocked_post.replace_once('"target_state": "validating"',
				'"target_state": "quarantined"')
		}
		blocked_issues := validate_schema_source('target-state.schema.json', blocked_post,
			'${post_case[0]}-post-blocked-classification')
		blocked_root := bin.parse_strict_json(blocked_post) or { panic(err) }
		blocked_last_native := blocked_root.object_value('last_native_validation') or {
			panic('blocked post native validation missing')
		}
		assert blocked_last_native.kind == .null_value
		assert blocked_issues.len == 0, '${post_case[0]} blocked null migration: ${blocked_issues}'
		assert_target_semantic_rejection(blocked_post.replace_once('"check_conclusion":"success"',
			'"check_conclusion":"failure"'), '${post_case[0]}-post-history-red',
			'green or retained historical gates require both run and check conclusions success')
		assert_target_semantic_rejection(post.replace_once('"post_validation_operation_id": "${live_post_operation_id}"',
			'"post_validation_operation_id": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"'),
			'${post_case[0]}-post-owner', 'post-validation consumer does not own')
	}

	publish_post := live_publish_post_source()
	rollback_post := live_rollback_post_source()
	publish_rollback_issues := validate_schema_source('target-state.schema.json', publish_post.replace_once('"publication_state": "post_publish_validating"',
		'"publication_state": "rollback_pending"'), 'publish-post-crossed-rollback-state')
	assert publish_rollback_issues.any(it.path == '$/publication_state'
		&& it.message == 'value is outside the closed enum'), '${publish_rollback_issues}'
	rollback_publish_issues := validate_schema_source('target-state.schema.json', rollback_post.replace_once('"publication_state": "rollback_pending"',
		'"publication_state": "post_publish_validating"'), 'rollback-post-crossed-publish-state')
	assert rollback_publish_issues.any(it.path == '$/publication_state'
		&& it.message == 'value is outside the closed enum'), '${rollback_publish_issues}'
	assert validate_schema_source('target-state.schema.json', publish_post.replace_once('"publication_state": "post_publish_validating"',
		'"publication_state": "post_publish_waiting_source"'), 'publish-post-stage-state-cross').len > 0
	assert validate_schema_source('target-state.schema.json', rollback_post.replace_once('"stage": "post_checks_running"',
		'"stage": "blocked"'), 'rollback-post-stage-state-cross').len > 0

	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_occurrence(terminal,
		'"output_digest": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"',
		'"output_digest": "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"', 2)),
		'gate-v-attempt-correlation',
		'V gate check must match exactly one terminal immutable V smoke attempt')
	terminal_root := bin.parse_strict_json(terminal) or { panic(err) }
	terminal_intent := terminal_root.object_value('active_intent') or { panic('intent missing') }
	terminal_gates := terminal_intent.object_value('gate_runs') or { panic('gates missing') }
	canonical_terminal := bin.canonical_json(terminal_root)
	native_gate := bin.canonical_json(terminal_gates.array_value[0])
	v_gate := bin.canonical_json(terminal_gates.array_value[1])
	missing_v_gate := canonical_terminal.replace_once(',${v_gate}', '')
	assert_target_semantic_rejection(missing_v_gate, 'gate-missing-kind',
		'terminal V smoke check requires exactly one correlated V gate check')
	second_native_gate := native_gate.replace_once('"evidence_digest":"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2"',
		'"evidence_digest":"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3"')
	duplicate_native_gate := canonical_terminal.replace_once(v_gate, second_native_gate)
	duplicate_issues := validate_schema_source('target-state.schema.json', duplicate_native_gate,
		'gate-duplicate-kind')
	assert duplicate_issues.len == 3, '${duplicate_issues}'
	assert duplicate_issues.any(it.path == '$/active_intent/gate_runs'
		&& it.message == 'array contains more than 1 matching items'), '${duplicate_issues}'
	assert duplicate_issues.any(it.path == '$/active_intent/gate_runs/1/check_name'
		&& it.message == 'value does not match const'), '${duplicate_issues}'
	assert duplicate_issues.any(it.path == '$/active_intent'
		&& it.message == 'expected exactly one oneOf branch, got 0'), '${duplicate_issues}'
}

fn test_v_smoke_negative_authority_and_lifecycle_matrix_is_discriminating() {
	terminal := schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')
	consumer_id := '1111111111111111111111111111111111111111111111111111111111111111'
	other_id := 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"v_smoke_execution": {\n    "schema_version": 1,\n    "consumer_id": "${consumer_id}"',
		'"v_smoke_execution": {\n    "schema_version": 1,\n    "consumer_id": "${other_id}"')),
		'smoke-consumer-identity', 'V smoke identity differs from native subject consumer_id')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"validator_integration_id": 1002',
		'"validator_integration_id": 1003')), 'smoke-validator-app',
		'V smoke reservation differs from the allowlisted validator check source')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_occurrence(terminal,
		'"check_suite_integration_id": 1001', '"check_suite_integration_id": 1003', 3)),
		'smoke-actions-app', 'run check suite is not owned by the allowlisted GitHub Actions App')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"repository": "vlang/v",\n    "workflow_id": 2002,\n    "workflow_path": ".github/workflows/tccbin_revalidate.yml"',
		'"repository": "vlang/v",\n    "workflow_id": 2999,\n    "workflow_path": ".github/workflows/tccbin_revalidate.yml"')),
		'smoke-workflow-id',
		'V smoke reservation differs from the allowlisted validator check source')
	assert validate_schema_source('target-state.schema.json', refresh_v_smoke_facts_digests(terminal.replace_once('"repository": "vlang/v",\n    "workflow_id": 2002,\n    "workflow_path": ".github/workflows/tccbin_revalidate.yml"',
		'"repository": "vlang/v",\n    "workflow_id": 2002,\n    "workflow_path": ".github/workflows/untrusted.yml"')),
		'smoke-workflow-path').len > 0
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"subject_ref": "tccbin-candidate/linux-amd64/${consumer_id}",\n    "subject_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"subject_ref": "tccbin-candidate/linux-amd64/${other_id}",\n    "subject_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"')),
		'smoke-subject-ref', 'V smoke identity differs from native subject original_ref')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"subject_ref": "tccbin-candidate/linux-amd64/${consumer_id}",\n    "subject_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
		'"subject_ref": "tccbin-candidate/linux-amd64/${consumer_id}",\n    "subject_sha": "ffffffffffffffffffffffffffffffffffffffff"')),
		'smoke-subject-sha', 'V smoke identity differs from native subject sha')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_json_string_value(terminal,
		'actor', 4, 'intruder[bot]')), 'smoke-actor',
		'run actor or triggering actor is not the allowlisted validator dispatcher App')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_json_string_value(terminal,
		'triggering_actor', 4, 'intruder[bot]')), 'smoke-triggering-actor',
		'run actor or triggering actor is not the allowlisted validator dispatcher App')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_json_string_value(terminal,
		'run_url', 3, 'https://github.com/vlang/v/actions/runs/3999')), 'smoke-run-url',
		'run and job URLs must be exact, sanitized projections of their IDs')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_json_string_value(terminal,
		'job_url', 3, 'https://github.com/vlang/v/actions/runs/3001/job/4999')), 'smoke-job-url',
		'run and job URLs must be exact, sanitized projections of their IDs')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_json_string_value(terminal,
		'details_url', 3, 'https://github.com/vlang/v/actions/runs/3001/job/4999')),
		'smoke-details-url', 'validator check is not bound to the reserved subject, job and App')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_json_string_value(terminal,
		'external_id', 3, other_id)), 'smoke-check-external-id',
		'validator check external ID is not the deterministic JCS identity')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"intent_id": "${consumer_id}"',
		'"intent_id": "${other_id}"')), 'smoke-orphaned-owner',
		'native candidate subject identity, ref, generation or exclusive owner')
	assert validate_schema_source('target-state.schema.json', refresh_v_smoke_facts_digests(terminal.replace_once('"active_attempt": null',
		'"active_attempt": 1')), 'smoke-terminal-active-attempt').len > 0
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(replace_nth_occurrence(terminal,
		'"run_attempt": 1', '"run_attempt": 2', 5)), 'smoke-run-attempt',
		'ACKed run must be created inside and exactly match its durable dispatch mode')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"ack_operation_ids": [\n      "1212121212121212121212121212121212121212121212121212121212121212"',
		'"ack_operation_ids": [\n      "${other_id}"')), 'smoke-ack-projection',
		'attempt ACK is not linked 1:1 to the ordered ACK projection')
	assert_target_semantic_rejection(refresh_v_smoke_facts_digests(terminal.replace_once('"completion_operation_ids": [\n      "3434343434343434343434343434343434343434343434343434343434343434"',
		'"completion_operation_ids": [\n      "${other_id}"')), 'smoke-completion-projection',
		'completion operations must preserve attempt order')
	assert validate_schema_source('target-state.schema.json', refresh_v_smoke_facts_digests(replace_nth_occurrence(terminal,
		'"run_conclusion": "success"', '"run_conclusion": null', 3)),
		'smoke-check-without-terminal-run').len > 0

	terminal_root := bin.parse_strict_json(terminal) or { panic(err) }
	terminal_smoke := terminal_root.object_value('v_smoke_execution') or { panic('smoke missing') }
	terminal_attempts := terminal_smoke.object_value('attempts') or { panic('attempts missing') }
	canonical_terminal := bin.canonical_json(terminal_root)
	canonical_terminal_attempts := bin.canonical_json(terminal_attempts)
	canonical_attempt := bin.canonical_json(terminal_attempts.array_value[0])
	duplicate_attempts := canonical_terminal.replace_once('"attempts":${canonical_terminal_attempts}',
		'"attempts":[${canonical_attempt},${canonical_attempt}]')
	rehashed_duplicate := refresh_v_smoke_facts_digests(duplicate_attempts)
	assert_v_smoke_attempt_digests_match(rehashed_duplicate)
	duplicate_issues := validate_schema_source('target-state.schema.json', rehashed_duplicate,
		'smoke-duplicate-attempt')
	assert duplicate_issues.any(it.path == '$/v_smoke_execution/attempts'
		&& it.message == 'array items are not unique'), '${duplicate_issues}'

	retry_terminal := schema_fixture('target-state.v-smoke-retry-terminal.schema-fixture.json')
	retry_root := bin.parse_strict_json(retry_terminal) or { panic(err) }
	retry_smoke := retry_root.object_value('v_smoke_execution') or { panic('smoke missing') }
	retry_attempts := retry_smoke.object_value('attempts') or { panic('attempts missing') }
	canonical_retry := bin.canonical_json(retry_root)
	canonical_retry_attempts := bin.canonical_json(retry_attempts)
	third_attempt := bin.canonical_json(retry_attempts.array_value[1]).replace_once('"attempt_index":2',
		'"attempt_index":3')
	retry_attempts_prefix := canonical_retry_attempts[..canonical_retry_attempts.len - 1]
	three_attempts := canonical_retry.replace_once('"attempts":${canonical_retry_attempts}',
		'"attempts":${retry_attempts_prefix},${third_attempt}]')
	rehashed_three := refresh_v_smoke_facts_digests(three_attempts)
	assert_v_smoke_attempt_digests_match(rehashed_three)
	third_attempt_issues := validate_schema_source('target-state.schema.json', rehashed_three,
		'smoke-third-attempt')
	assert third_attempt_issues.any(it.path == '$/v_smoke_execution/attempts'
		&& it.message == 'array has more than 2 items'), '${third_attempt_issues}'
	assert third_attempt_issues.any(it.path == '$/v_smoke_execution/attempts/2/attempt_index'
		&& it.message == 'integer is above 2'), '${third_attempt_issues}'
}

fn test_unknown_manifest_property_is_rejected() {
	fixture := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-complete.valid.json')) or { panic(err) }
	mutated := fixture.replace_once('"schema_version": 1,',
		'"schema_version": 1,\n  "unknown": true,')
	temporary := os.join_path(os.temp_dir(), 'tccbin-manifest-unknown.json')
	os.write_file(temporary, mutated) or { panic(err) }
	defer {
		os.rm(temporary) or {}
	}
	issues := bin.validate_manifest(automation_root(), temporary) or { panic(err) }
	assert issues.any(it.message == 'unknown property')
}

fn test_declared_provenance_is_recalculated_fail_closed() {
	toolchain_base := os.join_path(os.temp_dir(), 'tccbin-declared-provenance-${os.getpid()}')
	os.rmdir_all(toolchain_base) or {}
	authority := t2a_prepare_toolchain_authority(toolchain_base, 'linux-amd64')
	defer {
		os.rmdir_all(toolchain_base) or {}
	}
	complete := t2a_resolved_manifest_toolchain(schema_fixture('manifest-complete.valid.json'),
		authority)
	complete_with_opaque := complete.replace_once('"opaque": false', '"opaque": true')
	incomplete_nonopaque := complete.replace_once('"license": "LGPL-2.1-or-later"',
		'"license": null')
	output_incomplete := replace_nth_occurrence(complete, '"status": "complete"',
		'"status": "incomplete"', 2)
	provenance_mismatches := [complete_with_opaque, incomplete_nonopaque, output_incomplete]
	provenance_suffixes := ['complete-opaque', 'incomplete-provenance',
		'output-incomplete-declared-complete']
	for index, mutation in provenance_mismatches {
		issues := validate_manifest_source_at(authority.root, mutation, provenance_suffixes[index])
		assert issues.len == 1, '${issues}'
		assert issues[0].path == '$/provenance_status'
		assert issues[0].message == 'declared provenance status does not match the recalculated static contract'
	}
	declared_output_incomplete := output_incomplete.replace_once('"provenance_status": "complete"',
		'"provenance_status": "incomplete"')
	assert validate_manifest_source_at(authority.root, declared_output_incomplete,
		'output-incomplete-declared-incomplete').len == 0
	registry := bin.parse_strict_json(os.read_file(os.join_path(authority.root, 'targets.json')) or {
		panic(err)
	}) or { panic(err) }
	assert bin.recalculate_provenance(bin.parse_strict_json(declared_output_incomplete) or {
		panic(err)
	}, registry, []) or { panic(err) } == 'incomplete'
}

fn test_invented_acceptance_order_drift_and_inventory_collision_are_rejected() {
	windows := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-windows-opaque.valid.json')) or { panic(err) }
	invented := windows.replace_once('windows-amd64-openlibm-v1', 'invented-acceptance')
	assert validate_manifest_source(invented, 'invented-acceptance').len > 0
	order_drift := windows.replace_once('"order": 1,', '"order": 3,')
	assert validate_manifest_source(order_drift, 'order-drift').any(it.message.contains('strictly increasing'))
	collision := windows.replace_once('"path": "tcc.exe",', '"path": "src/tcc.c",')
	assert validate_manifest_source(collision, 'inventory-collision').any(it.message.contains('must not collide'))
	undeclared_probe := windows.replace_once('"required_probe_ids": ["patch-0001"]',
		'"required_probe_ids": ["undeclared-probe"]')
	assert validate_manifest_source(undeclared_probe, 'undeclared-probe').any(it.message.contains('undeclared probe'))
}

fn test_manifest_rejects_optional_duplicate_or_incomplete_required_matrix() {
	complete := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-complete.valid.json')) or { panic(err) }
	optional := complete.replace_once('"id": "manifest-contract", "required": true',
		'"id": "manifest-contract", "required": false')
	assert validate_manifest_source(optional, 'optional-probe').any(it.message.contains('must be required'))
	duplicate_lane := complete.replace_once('"expected_lanes": ["native"]',
		'"expected_lanes": ["native", "native"]')
	assert validate_manifest_source(duplicate_lane, 'duplicate-lane').any(it.message.contains('unique'))
	duplicate_probe := complete.replace_once('"id": "source-provenance"',
		'"id": "manifest-contract"')
	assert validate_manifest_source(duplicate_probe, 'duplicate-probe').any(it.message.contains('unique'))
	affected_drift := complete.replace_once('"affected_targets": ["linux-amd64"]',
		'"affected_targets": ["windows-amd64"]')
	assert validate_manifest_source(affected_drift, 'affected-drift').any(it.message.contains('registry graph'))
	outputs_index := complete.index('"outputs": [') or { panic('outputs marker missing') }
	probes_index := complete.index('"probes": [') or { panic('probes marker missing') }
	empty_outputs := complete[..outputs_index] + '"outputs": [],\n  ' + complete[probes_index..]
	assert validate_manifest_source(empty_outputs, 'empty-output').len > 0
}

fn test_dark_mode_never_creates_candidate_outputs() {
	bin.run_dark_mode_dry_run(automation_root()) or { panic(err) }
}

fn test_active_intent_schema_rejects_build_binding_and_rollback_cross_drift() {
	source := schema_fixture('active-intent.bootstrap.schema-fixture.json')
	mutations := [
		source.replace_once('"stage": "intent_reserved"', '"stage": "building"'),
		source.replace_once('"intent_type": "initial_adopt_current"', '"intent_type": "publish"'),
		source.replace_once('"intent_type": "initial_adopt_current"', '"intent_type": "rollback"'),
		source.replace_once('"stage": "intent_reserved"', '"stage": "checks_green"'),
		source.replace_once('"producer_toolchain": {', '"toolchain_digest": {'),
		source.replace_once('"observation_digest": "${'7'.repeat(64)}"',
			'"observation_digest": null'),
		source.replace_once(',\n      "observation_digest": "${'7'.repeat(64)}"', ''),
	]
	for index, mutation in mutations {
		assert validate_schema_source('active-intent.schema.json', mutation, 'active-${index}').len > 0
	}
}

fn test_active_intent_gate_run_schema_is_exact32_and_preserves_collection_states() {
	common_source := os.read_file(os.join_path(automation_root(), 'schemas', 'common.schema.json')) or {
		panic(err)
	}
	common := bin.parse_strict_json(common_source) or { panic(err) }
	definitions := common.object_value('$defs') or { panic('common definitions missing') }
	gate_run := definitions.object_value('gate_run') or { panic('gate_run definition missing') }
	required := gate_run.object_value('required') or { panic('gate_run required list missing') }
	mut observed_fields := required.array_value.map(it.string_value)
	observed_fields.sort()
	mut expected_fields := ['actor', 'actor_integration_id', 'check_conclusion', 'check_name',
		'check_run_id', 'check_sha', 'check_suite_id', 'check_suite_integration_id', 'completed_at',
		'created_at', 'details_url', 'event', 'evidence_digest', 'external_id', 'integration_id',
		'job_id', 'job_url', 'output_digest', 'ref', 'repository', 'run_attempt', 'run_conclusion',
		'run_id', 'run_name', 'run_url', 'sha', 'subject_hash', 'triggering_actor',
		'triggering_actor_integration_id', 'workflow_head_sha', 'workflow_id', 'workflow_path']
	expected_fields.sort()
	assert observed_fields.len == 32
	assert observed_fields == expected_fields

	target := bin.parse_strict_json(schema_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')) or {
		panic(err)
	}
	intent := target.object_value('active_intent') or { panic('active intent missing') }
	gates := intent.object_value('gate_runs') or { panic('gate runs missing') }
	assert gates.array_value.len == 2
	canonical_intent := bin.canonical_json(intent)
	native_source := bin.canonical_json(gates.array_value[0])
	smoke_source := bin.canonical_json(gates.array_value[1])
	for gate_source in ['[]', '[${native_source}]', '[${smoke_source}]',
		'[${native_source},${smoke_source}]'] {
		collecting := replace_canonical_root_member(canonical_intent, intent, 'gate_runs',
			gate_source)
		issues := validate_schema_source('active-intent.schema.json', collecting,
			'active-collecting-${gate_source.len}')
		assert issues.len == 0, '${gate_source}: ${issues}'
	}
	duplicate := replace_canonical_root_member(canonical_intent, intent, 'gate_runs',
		'[${native_source},${native_source}]')
	assert validate_schema_source('active-intent.schema.json', duplicate,
		'active-collecting-duplicate').len > 0
	reversed_collection := replace_canonical_root_member(canonical_intent, intent, 'gate_runs',
		'[${smoke_source},${native_source}]')
	assert validate_schema_source('active-intent.schema.json', reversed_collection,
		'active-collecting-reversed').len > 0

	terminal := replace_canonical_root_member(canonical_intent, intent, 'stage', '"checks_green"')
	assert validate_schema_source('active-intent.schema.json', terminal, 'active-terminal-two').len == 0
	terminal_value := bin.parse_strict_json(terminal) or { panic(err) }
	terminal_missing := replace_canonical_root_member(terminal, terminal_value, 'gate_runs',
		'[${native_source}]')
	assert validate_schema_source('active-intent.schema.json', terminal_missing,
		'active-terminal-missing').len > 0
	terminal_reversed := replace_canonical_root_member(terminal, terminal_value, 'gate_runs',
		'[${smoke_source},${native_source}]')
	assert validate_schema_source('active-intent.schema.json', terminal_reversed,
		'active-terminal-reversed').len > 0

	early_source := schema_fixture('active-intent.bootstrap.schema-fixture.json')
	early := bin.parse_strict_json(early_source) or { panic(err) }
	early_with_gate := replace_canonical_root_member(bin.canonical_json(early), early, 'gate_runs',
		'[${smoke_source}]')
	assert validate_schema_source('active-intent.schema.json', early_with_gate, 'active-early-gate').len > 0
	for stage in ['aborted', 'superseded'] {
		retained_stage := replace_canonical_root_member(bin.canonical_json(early), early, 'stage',
			'"${stage}"')
		retained_value := bin.parse_strict_json(retained_stage) or { panic(err) }
		retained := replace_canonical_root_member(retained_stage, retained_value, 'gate_runs',
			'[${native_source},${smoke_source}]')
		assert validate_schema_source('active-intent.schema.json', retained, 'active-${stage}-two').len == 0
	}
	fixture_cases := [
		['target-state.v-smoke-retry-terminal.schema-fixture.json', 'v-candidate-smoke'],
		['target-state.v-smoke-run-absent-retry-terminal.schema-fixture.json', 'v-candidate-smoke'],
		['target-state.v-smoke-terminal-check.schema-fixture.json',
			'tccbin-candidate-gate,v-candidate-smoke'],
	]
	for fixture_case in fixture_cases {
		fixture_source := schema_fixture(fixture_case[0])
		fixture := bin.parse_strict_json(fixture_source) or { panic(err) }
		fixture_intent := fixture.object_value('active_intent') or {
			panic('fixture intent missing')
		}
		fixture_gates := fixture_intent.object_value('gate_runs') or {
			panic('fixture gates missing')
		}
		fixture_stage := fixture_intent.object_value('stage') or { panic('fixture stage missing') }
		assert fixture_stage.string_value == 'checks_running'
		mut fixture_gate_names := []string{}
		for fixture_gate in fixture_gates.array_value {
			fixture_gate_name := fixture_gate.object_value('check_name') or {
				panic('fixture check name missing')
			}
			fixture_gate_names << fixture_gate_name.string_value
		}
		assert fixture_gate_names.join(',') == fixture_case[1]
		assert validate_schema_source('target-state.schema.json', fixture_source,
			'active-preserved-${fixture_case[0]}').len == 0
	}
}

fn test_native_subject_and_execution_schemas_reject_cross_drift() {
	subject := schema_fixture('native-gate-subject.schema-fixture.json')
	subject_mutations := [
		subject.replace_once('"initial_run_mode": "original_push"',
			'"initial_run_mode": "no_native_push_expected"'),
		subject.replace_once('"consumer_kind": "initial_adopt_current"',
			'"consumer_kind": "remediation"'),
		subject.replace_once('"original_ref": "tccbin-candidate/linux-amd64/1111111111111111111111111111111111111111111111111111111111111111"',
			'"original_ref": "thirdparty-linux-amd64"'),
	]
	for index, mutation in subject_mutations {
		assert validate_schema_source('native-gate-subject.schema.json', mutation,
			'subject-${index}').len > 0
	}
	execution := schema_fixture('native-gate-execution.schema-fixture.json')
	execution_mutations := [
		execution.replace_once('"trigger_id": null',
			'"trigger_id": "2222222222222222222222222222222222222222222222222222222222222222"'),
		execution.replace_once('"selected_run_id": null', '"selected_run_id": 10'),
		execution.replace_once('"source_recovery_operation_id": null',
			'"source_recovery_operation_id": "3333333333333333333333333333333333333333333333333333333333333333"'),
	]
	for index, mutation in execution_mutations {
		assert validate_schema_source('native-gate-execution.schema.json', mutation,
			'execution-${index}').len > 0
	}
}

fn test_recovery_and_target_schemas_reject_impossible_durable_combinations() {
	handoff := schema_fixture('recovery-handoff.pending.schema-fixture.json')
	handoff_mutations := [
		handoff.replace_once('"resume_capability": "native_gate"', '"resume_capability": "v_smoke"'),
		handoff.replace_once('"workflow_path": ".github/workflows/update_tccbin.yml"',
			'"workflow_path": ".github/workflows/tccbin_revalidate.yml"'),
		handoff.replace_once('"selected_run_id": null', '"selected_run_id": 10'),
		handoff.replace_once('"handoff_ordinal": 0', '"handoff_ordinal": 1'),
	]
	for index, mutation in handoff_mutations {
		assert validate_schema_source('recovery-handoff.schema.json', mutation, 'handoff-${index}').len > 0
	}
	target := schema_fixture('target-state.bootstrap.schema-fixture.json')
	target_mutations := [
		target.replace_once('"target_state": "uninitialized"', '"target_state": "eligible"'),
		target.replace_once('"bootstrap_required": true', '"bootstrap_required": false'),
		target.replace_once('"active_recovery_handoff_id": null',
			'"active_recovery_handoff_id": "1111111111111111111111111111111111111111111111111111111111111111"'),
		target.replace_once('"last_operation_id": null',
			'"last_operation_id": "2222222222222222222222222222222222222222222222222222222222222222"'),
	]
	for index, mutation in target_mutations {
		assert validate_schema_source('target-state.schema.json', mutation, 'target-${index}').len > 0
	}
}

fn test_native_validation_record_shape_caps_and_terminal_versions_are_closed() {
	schema_root := os.join_path(automation_root(), 'schemas')
	common := bin.parse_strict_json(os.read_file(os.join_path(schema_root, 'common.schema.json')) or {
		panic(err)
	}) or { panic(err) }
	definitions := common.object_value('$defs') or { panic('common definitions missing') }
	record := definitions.object_value('native_validation_record') or {
		panic('native validation record schema missing')
	}
	required := record.object_value('required') or {
		panic('native validation required set missing')
	}
	assert required.array_value.map(it.string_value) == ['schema_version', 'operation_id',
		'transition', 'resulting_generation', 'verdict', 'manifest_source', 'manifest_hash',
		'native_lane_matrix', 'matrix_digest', 'evidence', 'capsule_digest', 'native_gate',
		'v_smoke_gate', 'validation_digest']
	properties := record.object_value('properties') or {
		panic('native validation properties missing')
	}
	assert (properties.object_value('manifest_source') or {
		panic('manifest source schema missing')
	}).object_value('maxLength') or { panic('manifest source cap missing') }.int_value == 524288
	assert (properties.object_value('native_lane_matrix') or {
		panic('native matrix schema missing')
	}).object_value('$ref') or { panic('native matrix ref missing') }.string_value == 'native-lane-matrix.schema.json'
	evidence := properties.object_value('evidence') or { panic('evidence schema missing') }
	assert (evidence.object_value('maxItems') or { panic('evidence cardinality cap missing') }).int_value == 1028
	evidence_item := evidence.object_value('items') or { panic('evidence item schema missing') }
	evidence_properties := evidence_item.object_value('properties') or {
		panic('evidence item properties missing')
	}
	assert ((evidence_properties.object_value('size') or { panic('evidence size schema missing') }).object_value('maximum') or {
		panic('evidence size cap missing')
	}).int_value == 262144

	target_schema := bin.parse_strict_json(os.read_file(os.join_path(schema_root,
		'target-state.schema.json')) or { panic(err) }) or { panic(err) }
	target_required := target_schema.object_value('required') or {
		panic('target required set missing')
	}
	assert target_required.array_value.count(it.string_value == 'last_native_validation') == 1
	mut fixture_names := os.ls(os.join_path(automation_root(), 'tests', 'fixtures')) or {
		panic(err)
	}
	fixture_names = fixture_names.filter(it.starts_with('target-state')
		&& it.ends_with('.schema-fixture.json'))
	fixture_names.sort()
	assert fixture_names.len == 19, '${fixture_names}'
	for fixture_name in fixture_names {
		fixture_source := schema_fixture(fixture_name)
		fixture := bin.parse_strict_json(fixture_source) or { panic(err) }
		assert (fixture.object_value('last_native_validation') or {
			panic('${fixture_name} native migration field missing')
		}).kind == .null_value
		issues := validate_schema_source('target-state.schema.json', fixture_source,
			'native-null-${fixture_name}')
		assert issues.len == 0, '${fixture_name}: ${issues}'
	}
	malformed_record_source := schema_fixture('target-state.bootstrap.schema-fixture.json').replace_once('"last_native_validation": null',
		'"last_native_validation": {}')
	malformed_record_issues := validate_schema_source('target-state.schema.json',
		malformed_record_source, 'native-record-closed-required')
	assert malformed_record_issues.len > 0
	assert malformed_record_issues.any(it.path.starts_with('$/last_native_validation')), '${malformed_record_issues}'

	recovery := bin.parse_strict_json(os.read_file(os.join_path(schema_root,
		'recovery-handoff.schema.json')) or { panic(err) }) or { panic(err) }
	recovery_definitions := recovery.object_value('$defs') or {
		panic('recovery definitions missing')
	}
	projection := recovery_definitions.object_value('terminal_state_projection') or {
		panic('terminal projection schema missing')
	}
	projection_properties := projection.object_value('properties') or {
		panic('terminal projection properties missing')
	}
	assert ((projection_properties.object_value('schema_version') or {
		panic('terminal projection version missing')
	}).object_value('const') or { panic('terminal projection version const missing') }).int_value == 3
	assert (projection.object_value('required') or {
		panic('terminal projection required set missing')
	}).array_value.count(it.string_value == 'last_native_validation') == 1
	recovery_properties := recovery.object_value('properties') or {
		panic('recovery properties missing')
	}
	revalidation := recovery_properties.object_value('terminal_revalidation') or {
		panic('terminal revalidation schema missing')
	}
	revalidation_alternatives := revalidation.object_value('oneOf') or {
		panic('terminal revalidation alternatives missing')
	}
	revalidation_properties := revalidation_alternatives.array_value[1].object_value('properties') or {
		panic('terminal revalidation properties missing')
	}
	assert ((revalidation_properties.object_value('schema_version') or {
		panic('terminal revalidation version missing')
	}).object_value('const') or { panic('terminal revalidation version const missing') }).int_value == 5
}

fn test_durable_native_validation_replays_full_phase_a_and_closed_resolved_inputs_authority() {
	authority_base := os.join_path(os.temp_dir(), 'tccbin-durable-authority-${os.getpid()}')
	os.rmdir_all(authority_base) or {}
	authority := t2a_prepare_toolchain_authority(authority_base, 'linux-amd64')
	defer {
		os.rmdir_all(authority_base) or { panic(err) }
	}
	case := schema_authority_case(authority, 'publish_candidate',
		schema_oracle_digest('authority-consumer'), 'candidate_checks_green', 'green',
		schema_oracle_digest('authority-operation'), 1)
	resolved_value := resolved_inputs_value_for_authority(case.inputs)
	authority_record := case.record
	bin.validate_native_validation_record_authority_for_test(authority.root, authority_record,
		resolved_value, case.fingerprints.input_fingerprint,
		case.fingerprints.artifact_fingerprint, case.fingerprints.manifest_hash) or { panic(err) }
	projection := bin.resolved_inputs_manifest_projection_for_test(resolved_value) or { panic(err) }
	assert projection.object_keys == ['sources', 'recipe_path', 'recipe_hash', 'contract_repository',
		'contract_sha', 'v_source_sha', 'producer_toolchain']
	assert !projection.has_object_key('source_checks')

	mut root_mutations := []bin.JsonValue{}
	for mutation in [
		resolved_inputs_source_for_authority(case.inputs).replace_once('"recipe_hash":"${case.inputs.recipe_hash}"',
			'"recipe_hash":"${schema_oracle_digest('authority-recipe-drift')}"'),
		resolved_inputs_source_for_authority(case.inputs).replace_once('"source_checks":[',
			'"source_checks":[{"source_id":"tinycc","resolved_sha":"${'c'.repeat(40)}","status":"resolved","evidence_digest":"${schema_oracle_digest('authority-extra-check')}"},'),
		resolved_inputs_source_for_authority(case.inputs).replace_once('"status":"resolved"',
			'"status":"unreachable"'),
	] {
		root_mutations << (bin.parse_strict_json(mutation) or { panic(err) })
	}
	mut open_root := bin.canonical_json(resolved_value)
	open_root = open_root.replace_once('{', '{"attacker":true,')
	root_mutations << (bin.parse_strict_json(open_root) or { panic(err) })
	for mutation in root_mutations {
		mut authority_rejected := ''
		bin.validate_native_validation_record_authority_for_test(authority.root, authority_record,
			mutation, case.fingerprints.input_fingerprint, case.fingerprints.artifact_fingerprint,
			case.fingerprints.manifest_hash) or { authority_rejected = err.msg() }
		assert authority_rejected.starts_with('last native validation resolved inputs differ from the authenticated manifest:'), authority_rejected
	}

	for authority_mutation in ['profile', 'registry', 'phase-a'] {
		if authority_mutation == 'profile' {
			profile_path := os.join_path(authority.root, 'toolchain-profiles',
				'linux-amd64.profile.json')
			profile_source := os.read_file(profile_path) or { panic(err) }
			os.write_file(profile_path, profile_source.replace_once('"schema_version":1',
				'"schema_version":2')) or { panic(err) }
		} else if authority_mutation == 'registry' {
			registry_path := os.join_path(authority.root, 'targets.json')
			registry_source := os.read_file(registry_path) or { panic(err) }
			os.write_file(registry_path, registry_source.replace_once('"branch": "thirdparty-linux-amd64"',
				'"branch": "thirdparty-macos-amd64"')) or { panic(err) }
		} else {
			schema_path := os.join_path(authority.root, 'schemas', 'bundle-manifest.schema.json')
			schema_source := os.read_file(schema_path) or { panic(err) }
			os.write_file(schema_path, schema_source.replace_once('"schema_version": {"type": "integer", "const": 1}',
				'"schema_version": {"type": "integer", "const": 2}')) or { panic(err) }
		}
		mut authority_rejected := ''
		bin.validate_native_validation_record_authority_for_test(authority.root, authority_record,
			resolved_value, case.fingerprints.input_fingerprint,
			case.fingerprints.artifact_fingerprint, case.fingerprints.manifest_hash) or {
			authority_rejected = err.msg()
		}
		assert authority_rejected.starts_with('last native validation manifest lacks replayable non-staged authority:'), authority_rejected
		os.rmdir_all(authority_base) or { panic(err) }
		_ = t2a_prepare_toolchain_authority(authority_base, 'linux-amd64')
	}
}

fn test_source_state_schema_rejects_outage_cadence_and_sha_drift() {
	source := schema_fixture('source-state.outage.schema-fixture.json')
	mutations := [
		source.replace_once('"mode": "upstream-recovery-daily"', '"mode": "monthly"'),
		source.replace_once('"resolved_sha": null',
			'"resolved_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"'),
		source.replace_once('"originating_run_id": 100', '"originating_run_id": null'),
	]
	for index, mutation in mutations {
		assert validate_schema_source('source-state.schema.json', mutation, 'source-${index}').len > 0
	}
}

fn test_source_state_v2_window_is_bounded_without_a_128_operation_lifetime_limit() {
	for case in [[0, 0], [1, 1], [127, 127], [128, 128], [129, 128]] {
		source := source_state_window_source(case[0], case[1])
		assert validate_schema_source('source-state.schema.json', source,
			'source-window-${case[0]}-${case[1]}').len == 0
	}
	overflow := source_state_window_source(129, 129)
	assert validate_schema_source('source-state.schema.json', overflow, 'source-window-overflow').any(it.message.contains('more than 128 items'))
	early_truncation := source_state_window_source(2, 1)
	assert validate_schema_source('source-state.schema.json', early_truncation,
		'source-window-early-truncation').any(it.message.contains('exactly min(operation_count, 128)'))
	broken_chain := source_state_window_source(129, 128).replace_once('"sequence":2',
		'"sequence":3')
	assert validate_schema_source('source-state.schema.json', broken_chain,
		'source-window-broken-chain').any(it.message.contains('contiguous unique generation/state/hash chain'))
	for index, kind in ['generation', 'state', 'chain'] {
		mutation := rehashed_source_window_discontinuity(source_state_window_source(3, 3), kind)
		assert validate_schema_source('source-state.schema.json', mutation,
			'source-window-continuity-${index}').len > 0
	}
	pre := bin.parse_strict_json(source_state_window_source(128, 128)) or { panic(err) }
	post_source := source_state_window_source(129, 128)
	post := bin.parse_strict_json(post_source) or { panic(err) }
	window := post.object_value('operation_window') or { panic('post operation window missing') }
	entries := window.object_value('entries') or { panic('post operation entries missing') }
	entry := entries.array_value[entries.array_value.len - 1]
	transition := bin.parse_strict_json('{"sequence":${(entry.object_value('sequence') or {
		panic('entry sequence missing')
	}).int_value},"operation_id":${bin.canonical_json(entry.object_value('operation_id') or {
		panic('entry operation missing')
	})},"transition":${bin.canonical_json(entry.object_value('transition') or {
		panic('entry transition missing')
	})},"previous_generation":${(entry.object_value('previous_generation') or {
		panic('entry previous generation missing')
	}).int_value},"resulting_generation":${(entry.object_value('resulting_generation') or {
		panic('entry resulting generation missing')
	}).int_value},"previous_state_digest":${bin.canonical_json(entry.object_value('previous_state_digest') or {
		panic('entry previous state digest missing')
	})},"resulting_state_digest":${bin.canonical_json(entry.object_value('resulting_state_digest') or {
		panic('entry resulting state digest missing')
	})},"evidence_path":${bin.canonical_json(entry.object_value('evidence_path') or {
		panic('entry evidence path missing')
	})},"universal_evidence_digest":${bin.canonical_json(entry.object_value('evidence_digest') or {
		panic('entry evidence digest missing')
	})},"previous_chain_digest":${bin.canonical_json(entry.object_value('previous_chain_digest') or {
		panic('entry previous chain missing')
	})},"resulting_chain_digest":${bin.canonical_json(entry.object_value('resulting_chain_digest') or {
		panic('entry resulting chain missing')
	})}}') or { panic(err) }
	assert bin.source_state_append_contract_is_exact(pre, post, transition) or { panic(err) }
	wrong_anchor := bin.parse_strict_json(post_source.replace_once('"start_count":1',
		'"start_count":0')) or { panic(err) }
	assert !(bin.source_state_append_contract_is_exact(pre, wrong_anchor, transition) or {
		panic(err)
	})
}

fn test_source_state_v2_validates_gregorian_autonomous_attempt_timestamps() {
	fixture := schema_fixture('source-state.outage.schema-fixture.json')
	leap_2000 := fixture.replace_once('2026-08-02T03:47:00Z', '2000-02-29T03:47:00Z')
	leap_2000_issues := validate_schema_source('source-state.schema.json', leap_2000,
		'source-calendar-leap-2000')
	assert leap_2000_issues.len == 0, '${leap_2000_issues}'
	century_2100 := fixture.replace_once('2026-08-02T03:47:00Z', '2100-02-29T03:47:00Z')
	assert validate_schema_source('source-state.schema.json', century_2100,
		'source-calendar-century-2100').any(it.message.contains('calendar-valid UTC'))
	invalid_day := fixture.replace_once('2026-08-02T03:47:00Z', '2026-02-30T03:47:00Z')
	assert validate_schema_source('source-state.schema.json', invalid_day,
		'source-calendar-invalid-day').any(it.message.contains('calendar-valid UTC'))
}
