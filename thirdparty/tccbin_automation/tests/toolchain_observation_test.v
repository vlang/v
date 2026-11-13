module tests

import crypto.sha256
import os
import tccbin_automation.bin

const macos_arm64_profile_id = 'macos-arm64-github-hosted-clang-v1'

const macos_arm64_profile_sha256 = 'f8022035063578f6102896ed1fb0046dd3f286880f44dab6173cac8e6711b4bc'

fn macos_arm64_runner_facts() map[string]string {
	return {
		'arch':                   'arm64'
		'compiler_binary_sha256': '6'.repeat(64)
		'compiler_command':       'clang'
		'compiler_family':        'clang'
		'compiler_target':        'arm64-apple-darwin24.6.0'
		'compiler_version':       'Apple clang version 17.0.0 (clang-1700.0.13.5)'
		'image_os':               'macos15'
		'image_version':          '20261020.0123'
		'os':                     'macos'
		'runner_label':           'macos-15'
	}
}

fn macos_arm64_role_input(role_id string, facts map[string]string) map[string]bin.ToolchainRoleObservationInput {
	return {
		role_id: bin.ToolchainRoleObservationInput{
			facts:    facts
			evidence: 'tccbin-toolchain-evidence-v1\nrole=${role_id}\n'
		}
	}
}

fn macos_arm64_observation_error(phase string, inputs map[string]bin.ToolchainRoleObservationInput) string {
	bin.build_toolchain_observation(automation_root(), 'macos-arm64', phase, inputs) or {
		return err.msg()
	}
	return ''
}

fn test_production_macos_arm64_profile_is_the_reviewed_canonical_registry_binding() {
	profile_path := os.join_path(automation_root(), 'toolchain-profiles', 'macos-arm64.profile.json')
	source := os.read_file(profile_path) or { panic(err) }
	profile := bin.parse_strict_json(source) or { panic(err) }
	assert source == bin.canonical_json(profile)
	assert bin.json_sha256(profile) == macos_arm64_profile_sha256
	assert (profile.object_value('profile_id') or { panic('profile ID missing') }).string_value == macos_arm64_profile_id
	issues := bin.validate_registry(automation_root()) or { panic(err) }
	assert issues.len == 0, '${issues}'
}

fn test_producer_observation_is_derived_authenticated_and_deterministic() {
	inputs := macos_arm64_role_input('bundle-builder', macos_arm64_runner_facts())
	first := bin.build_toolchain_observation(automation_root(), 'macos-arm64', 'producer', inputs) or {
		panic(err)
	}
	second := bin.build_toolchain_observation(automation_root(), 'macos-arm64', 'producer',
		inputs) or { panic(err) }
	assert bin.canonical_json(first.observation) == bin.canonical_json(second.observation)
	assert first.observation_sha256 == second.observation_sha256
	assert first.phase == 'producer'
	assert first.profile_id == macos_arm64_profile_id
	assert first.profile_sha256 == macos_arm64_profile_sha256
	roles := first.observation.object_value('roles') or { panic('roles missing') }
	assert roles.array_value.len == 1
	role := roles.array_value[0]
	assert (role.object_value('role_id') or { panic('role ID missing') }).string_value == 'bundle-builder'
	evidence := 'tccbin-toolchain-evidence-v1\nrole=bundle-builder\n'
	assert (role.object_value('evidence_sha256') or { panic('evidence missing') }).string_value == sha256.sum256(evidence.bytes()).hex()
	assert (role.object_value('resolution_digest') or { panic('resolution missing') }).string_value == bin.toolchain_role_resolution_digest(first.observation,
		role) or { panic(err) }
	assert (first.observation.object_value('observation_digest') or { panic('digest missing') }).string_value == bin.toolchain_observation_digest(first.observation) or {
		panic(err)
	}
	// The emitted bytes must round-trip through the file authenticator the workflow will use.
	path := os.join_path(os.temp_dir(), 'tccbin-toolchain-observation-${os.getpid()}.json')
	os.write_file(path, bin.canonical_json(first.observation)) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	from_file := bin.authenticate_toolchain_observation_file(automation_root(), 'macos-arm64',
		path) or { panic(err) }
	assert from_file.observation_sha256 == first.observation_sha256
}

fn test_validator_observation_uses_the_validator_role() {
	observation := bin.build_toolchain_observation(automation_root(), 'macos-arm64', 'validator',
		macos_arm64_role_input('contract-validator', macos_arm64_runner_facts())) or { panic(err) }
	assert observation.phase == 'validator'
	assert macos_arm64_observation_error('validator', macos_arm64_role_input('bundle-builder',
		macos_arm64_runner_facts())) == 'toolchain observation is missing profile role contract-validator'
}

fn test_observation_rejects_facts_outside_the_reviewed_profile() {
	policy_error := 'toolchain observed fact does not satisfy its reviewed match policy'
	for name, value in {
		'arch':                   'amd64'
		'runner_label':           'macos-14'
		'os':                     'linux'
		'compiler_family':        'gcc'
		'compiler_version':       ''
		'image_os':               ''
		'compiler_binary_sha256': 'not-a-digest'
	} {
		mut facts := macos_arm64_runner_facts()
		facts[name] = value
		assert macos_arm64_observation_error('producer', macos_arm64_role_input('bundle-builder',
			facts)) == policy_error, name
	}
	mut missing := macos_arm64_runner_facts()
	missing.delete('image_version')
	assert macos_arm64_observation_error('producer', macos_arm64_role_input('bundle-builder',
		missing)) == 'toolchain observation facts for bundle-builder differ from its identity policy'
	mut renamed := macos_arm64_runner_facts()
	renamed.delete('image_version')
	renamed['image_release'] = '20261020.0123'
	assert macos_arm64_observation_error('producer', macos_arm64_role_input('bundle-builder',
		renamed)) == 'toolchain observation for bundle-builder is missing fact image_version'
}

fn test_observation_rejects_role_and_target_drift() {
	mut two_roles := macos_arm64_role_input('bundle-builder', macos_arm64_runner_facts())
	two_roles['bundle-builder-guest'] = two_roles['bundle-builder']
	assert macos_arm64_observation_error('producer', two_roles) == 'toolchain observation roles differ from the exact profile phase'
	assert macos_arm64_observation_error('consumer', macos_arm64_role_input('bundle-builder',
		macos_arm64_runner_facts())) == 'toolchain observation phase must be producer or validator'
	mut unbound := ''
	bin.build_toolchain_observation(automation_root(), 'linux-amd64', 'producer', macos_arm64_role_input('bundle-builder',
		macos_arm64_runner_facts())) or { unbound = err.msg() }
	assert unbound == 'target has no reviewed toolchain profile'
}

fn test_observation_collection_refuses_unsafe_destinations_before_probing() {
	base := os.join_path(os.temp_dir(), 'tccbin-toolchain-collect-${os.getpid()}')
	os.rmdir_all(base) or {}
	os.mkdir_all(base) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	existing := os.join_path(base, 'observation.json')
	os.write_file(existing, '{}') or { panic(err) }
	evidence_dir := os.join_path(base, 'evidence')
	os.mkdir_all(evidence_dir) or { panic(err) }
	mut message := ''
	bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer', 'macos-15',
		os.join_path(base, 'missing'), os.join_path(base, 'fresh.json')) or { message = err.msg() }
	assert message == 'toolchain evidence directory must be an existing physical directory'
	bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer', 'macos-15',
		evidence_dir, existing) or { message = err.msg() }
	assert message == 'toolchain observation destination must not already exist'
	assert os.read_file(existing) or { panic(err) } == '{}'
	// An observation inside the evidence directory could alias an evidence file named by its
	// SHA-256, so it is refused before probing, also through a symlinked spelling of the directory.
	evidence_link := os.join_path(base, 'evidence-link')
	os.symlink(evidence_dir, evidence_link) or { panic(err) }
	inside_error := 'toolchain observation destination must be outside the evidence directory'
	for observation_path in [os.join_path(evidence_dir, '6'.repeat(64)),
		os.join_path(evidence_link, 'observation.json'), evidence_dir] {
		message = ''
		bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer', 'macos-15',
			evidence_dir, observation_path) or { message = err.msg() }
		assert message == inside_error, observation_path
	}
	$if macos {
		// The default macOS file system is case-insensitive, so this spelling is the same directory.
		message = ''
		bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer', 'macos-15',
			evidence_dir, os.join_path(base, 'EVIDENCE', '6'.repeat(64))) or { message = err.msg() }
		assert message == inside_error
	}
	assert os.ls(evidence_dir) or { panic(err) } == []
}

// The collector needs a host that satisfies the reviewed profile's exact facts, so its success path
// can run only on macOS arm64. `ImageOS`/`ImageVersion` are runner-only variables and are supplied
// here; everything else is observed from the host.
fn test_observation_collection_writes_authenticated_evidence_on_macos_arm64() {
	$if macos && arm64 {
		base := os.join_path(os.temp_dir(), 'tccbin-toolchain-collect-ok-${os.getpid()}')
		os.rmdir_all(base) or {}
		evidence_dir := os.join_path(base, 'evidence')
		os.mkdir_all(evidence_dir) or { panic(err) }
		old_image_os := os.getenv_opt('ImageOS')
		old_image_version := os.getenv_opt('ImageVersion')
		os.setenv('ImageOS', 'macos15', true)
		os.setenv('ImageVersion', '20261020.0123', true)
		defer {
			if value := old_image_os {
				os.setenv('ImageOS', value, true)
			} else {
				os.unsetenv('ImageOS')
			}
			if value := old_image_version {
				os.setenv('ImageVersion', value, true)
			} else {
				os.unsetenv('ImageVersion')
			}
			os.rmdir_all(base) or {}
		}
		observation_path := os.join_path(base, 'observation.json')
		collected := bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer',
			'macos-15', evidence_dir, observation_path) or { panic(err) }
		assert collected.phase == 'producer'
		from_file := bin.authenticate_toolchain_observation_file(automation_root(), 'macos-arm64',
			observation_path) or { panic(err) }
		assert from_file.observation_sha256 == collected.observation_sha256
		role := (collected.observation.object_value('roles') or { panic('roles missing') }).array_value[0]
		evidence_sha256 := (role.object_value('evidence_sha256') or { panic('evidence missing') }).string_value
		assert os.ls(evidence_dir) or { panic(err) } == [evidence_sha256]
		evidence := os.read_file(os.join_path(evidence_dir, evidence_sha256)) or { panic(err) }
		assert sha256.sum256(evidence.bytes()).hex() == evidence_sha256
		assert evidence.starts_with('tccbin-toolchain-evidence-v1\nphase=producer\nrole=bundle-builder\n')
		// Collecting again into the same evidence directory must refuse before writing anything.
		second_path := os.join_path(base, 'second.json')
		mut message := ''
		bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer', 'macos-15',
			evidence_dir, second_path) or { message = err.msg() }
		assert message == 'toolchain evidence destination must not already exist'
		assert !os.exists(second_path)
		assert os.read_file(os.join_path(evidence_dir, evidence_sha256)) or { panic(err) } == evidence
		// vlang/v#31426 review: naming the observation after the evidence it will produce, inside a
		// fresh evidence directory, must not let the observation overwrite that evidence.
		fresh_dir := os.join_path(base, 'fresh-evidence')
		os.mkdir_all(fresh_dir) or { panic(err) }
		aliased_path := os.join_path(fresh_dir, evidence_sha256)
		message = ''
		bin.collect_toolchain_observation(automation_root(), 'macos-arm64', 'producer', 'macos-15',
			fresh_dir, aliased_path) or { message = err.msg() }
		assert message == 'toolchain observation destination must be outside the evidence directory'
		assert os.ls(fresh_dir) or { panic(err) } == []
	}
}
