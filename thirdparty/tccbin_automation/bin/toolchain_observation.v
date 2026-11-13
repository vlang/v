module bin

import crypto.sha256
import os

const toolchain_evidence_header = 'tccbin-toolchain-evidence-v1'

// ToolchainRoleObservationInput holds the readable facts and raw evidence observed for one profile
// role. Facts are keyed by name; their order comes from the reviewed profile, not from the input.
pub struct ToolchainRoleObservationInput {
pub:
	facts    map[string]string
	evidence string
}

// build_toolchain_observation derives one canonical observation for a reviewed target profile phase
// from observed facts, then authenticates it against that same profile before returning it. Every
// profile role must have exactly the facts its identity policy names.
pub fn build_toolchain_observation(automation_root string, target_id string, phase string,
	roles map[string]ToolchainRoleObservationInput) !AuthenticatedToolchainObservation {
	if phase !in ['producer', 'validator'] {
		return error('toolchain observation phase must be producer or validator')
	}
	binding, profile := reviewed_toolchain_profile(automation_root, target_id)!
	profile_roles := ordered_toolchain_roles(profile, phase)!
	if roles.len != profile_roles.len {
		return error('toolchain observation roles differ from the exact profile phase')
	}
	header := object_value_from_pairs(['schema_version', 'target_id', 'profile_id', 'profile_sha256',
		'phase'], [
		JsonValue{
			kind:      .integer
			int_value: 1
		},
		json_string_value(target_id),
		json_string_value(binding.profile_id),
		json_string_value(binding.profile_sha256),
		json_string_value(phase),
	])!
	mut observed_roles := []JsonValue{cap: profile_roles.len}
	for profile_role in profile_roles {
		role_id := require_string_member(profile_role, 'role_id')!
		input := roles[role_id] or {
			return error('toolchain observation is missing profile role ${role_id}')
		}
		policy_facts := ordered_toolchain_facts(profile_role, 'identity_policy')!
		if input.facts.len != policy_facts.len {
			return error('toolchain observation facts for ${role_id} differ from its identity policy')
		}
		mut resolved := []JsonValue{cap: policy_facts.len}
		for policy_fact in policy_facts {
			name := require_string_member(policy_fact, 'name')!
			value := input.facts[name] or {
				return error('toolchain observation for ${role_id} is missing fact ${name}')
			}
			resolved << object_value_from_pairs(['name', 'value'], [
				json_string_value(name),
				json_string_value(value),
			])!
		}
		identity := object_value_from_pairs(['role_id', 'identity_strategy', 'resolved_identity'], [
			json_string_value(role_id),
			require_member(profile_role, 'identity_strategy')!,
			JsonValue{
				kind:        .array
				array_value: resolved
			},
		])!
		resolution_digest := toolchain_role_resolution_digest(header, identity)!
		observed_roles << append_object_members(identity, ['resolution_digest', 'evidence_sha256'], [
			json_string_value(resolution_digest),
			json_string_value(sha256.sum256(input.evidence.bytes()).hex()),
		])!
	}
	without_digest := append_object_members(header, ['roles'], [
		JsonValue{
			kind:        .array
			array_value: observed_roles
		},
	])!
	observation := append_object_members(without_digest, ['observation_digest'], [
		json_string_value(toolchain_observation_digest(without_digest)!),
	])!
	return authenticate_toolchain_observation_value(automation_root, target_id, observation)!
}

// collect_toolchain_observation observes the current GitHub-hosted runner for every role of one
// reviewed profile phase, writes each role's raw evidence into `evidence_dir` under its SHA-256,
// and writes the canonical observation bytes to `observation_path`. The runner label is supplied
// by the caller because no runner environment variable exposes it. A runner whose facts do not
// satisfy the reviewed profile fails here instead of producing an observation.
pub fn collect_toolchain_observation(automation_root string, target_id string, phase string,
	runner_label string, evidence_dir string, observation_path string) !AuthenticatedToolchainObservation {
	if !os.is_dir(evidence_dir) || os.is_link(evidence_dir) {
		return error('toolchain evidence directory must be an existing physical directory')
	}
	// Evidence files are named by their SHA-256 directly inside `evidence_dir`, so an observation
	// written there could replace one of them. Compare directory identity, not spelling, so a
	// symlinked or differently cased path to the same directory is refused too.
	if toolchain_paths_identify_same_directory(observation_path, evidence_dir)
		|| toolchain_paths_identify_same_directory(os.dir(observation_path), evidence_dir) {
		return error('toolchain observation destination must be outside the evidence directory')
	}
	if os.exists(observation_path) || os.is_link(observation_path) {
		return error('toolchain observation destination must not already exist')
	}
	_, profile := reviewed_toolchain_profile(automation_root, target_id)!
	mut inputs := map[string]ToolchainRoleObservationInput{}
	for profile_role in ordered_toolchain_roles(profile, phase)! {
		role_id := require_string_member(profile_role, 'role_id')!
		if require_string_member(profile_role, 'identity_strategy')! != 'github-hosted' {
			return error('toolchain observation collection supports only GitHub-hosted roles')
		}
		mut compiler_command := ''
		for fact in ordered_toolchain_facts(profile_role, 'identity_policy')! {
			if require_string_member(fact, 'name')! == 'compiler_command' {
				compiler_command = require_string_member(fact, 'value')!
			}
		}
		inputs[role_id] = observe_github_hosted_role(phase, role_id, compiler_command,
			runner_label)!
	}
	authenticated := build_toolchain_observation(automation_root, target_id, phase, inputs)!
	// Check every destination before writing any, so a refusal leaves no partial evidence behind.
	mut evidence_files := map[string]string{}
	for _, input in inputs {
		evidence_file := os.join_path(evidence_dir, sha256.sum256(input.evidence.bytes()).hex())
		if os.exists(evidence_file) || os.is_link(evidence_file) {
			return error('toolchain evidence destination must not already exist')
		}
		evidence_files[evidence_file] = input.evidence
	}
	for evidence_file, evidence in evidence_files {
		os.write_file(evidence_file, evidence)!
	}
	os.write_file(observation_path, canonical_json(authenticated.observation))!
	for evidence_file, evidence in evidence_files {
		if os.read_file(evidence_file)! != evidence {
			return error('toolchain evidence changed after it was written')
		}
	}
	return authenticate_toolchain_observation_file(automation_root, target_id, observation_path)!
}

fn toolchain_paths_identify_same_directory(path string, directory string) bool {
	if !os.exists(path) || !os.exists(directory) {
		return false
	}
	$if windows {
		// Windows `stat` reports no inode, so compare case-folded resolved paths instead.
		return os.real_path(path).to_lower() == os.real_path(directory).to_lower()
	} $else {
		first := os.stat(path) or { return false }
		second := os.stat(directory) or { return false }
		return first.dev == second.dev && first.inode == second.inode
	}
}

fn observe_github_hosted_role(phase string, role_id string, compiler_command string,
	runner_label string) !ToolchainRoleObservationInput {
	if compiler_command !in ['clang', 'gcc'] {
		return error('toolchain observation compiler command is not closed')
	}
	// Every compiler fact is read from the one resolved binary that is also hashed.
	compiler_path := resolve_toolchain_compiler(compiler_command)!
	version_output := run_toolchain_probe([compiler_path, '--version'])!
	compiler_version := version_output.split_into_lines()[0] or { '' }.trim_space()
	compiler_target := run_toolchain_probe([compiler_path, '-dumpmachine'])!.trim_space()
	compiler_bytes := os.read_bytes(compiler_path)!
	facts := {
		'arch':                   toolchain_arch_name(os.uname().machine)
		'compiler_binary_sha256': sha256.sum256(compiler_bytes).hex()
		'compiler_command':       compiler_command
		'compiler_family':        toolchain_compiler_family(compiler_version)
		'compiler_target':        compiler_target
		'compiler_version':       compiler_version
		'image_os':               os.getenv('ImageOS')
		'image_version':          os.getenv('ImageVersion')
		'os':                     os.user_os()
		'runner_label':           runner_label
	}
	mut names := facts.keys()
	names.sort()
	mut evidence := '${toolchain_evidence_header}\nphase=${phase}\nrole=${role_id}\n'
	evidence += 'compiler_path=${compiler_path}\n'
	for name in names {
		evidence += '${name}=${facts[name]}\n'
	}
	evidence += '--- ${compiler_command} --version\n${version_output}'
	if !evidence.ends_with('\n') {
		evidence += '\n'
	}
	return ToolchainRoleObservationInput{
		facts:    facts
		evidence: evidence
	}
}

// resolve_toolchain_compiler returns the physical binary that `command` runs from PATH, which is
// what the build recipes invoke. On macOS `/usr/bin/clang` and `/usr/bin/gcc` are xcrun shims, so
// the shim is followed to the toolchain binary it dispatches to.
fn resolve_toolchain_compiler(command string) !string {
	mut path := os.real_path(os.find_abs_path_of_executable(command)!)
	$if macos {
		if path == '/usr/bin/${command}' {
			path = run_toolchain_probe(['xcrun', '--find', command])!.trim_space()
		}
	}
	real := os.real_path(path)
	if !os.is_file(real) {
		return error('toolchain compiler ${command} does not resolve to a regular file')
	}
	return real
}

fn run_toolchain_probe(args []string) !string {
	result := os.exec(args)
	if result.exit_code != 0 {
		return error('toolchain probe `${args.join(' ')}` failed with exit code ${result.exit_code}')
	}
	return result.output
}

fn toolchain_arch_name(machine string) string {
	return match machine {
		'x86_64' { 'amd64' }
		'aarch64' { 'arm64' }
		else { machine }
	}
}

fn toolchain_compiler_family(version_line string) string {
	if version_line.contains('clang') {
		return 'clang'
	}
	if version_line.contains('gcc') || version_line.contains('GCC') {
		return 'gcc'
	}
	return ''
}

fn json_string_value(value string) JsonValue {
	return JsonValue{
		kind:         .string_value
		string_value: value
	}
}
