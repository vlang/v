module tests

import os
import crypto.sha256
import tccbin_automation.bin

const bootstrap_blocked_git_environment = [
	'GIT_DIR',
	'GIT_WORK_TREE',
	'GIT_COMMON_DIR',
	'GIT_INDEX_FILE',
	'GIT_OBJECT_DIRECTORY',
	'GIT_ALTERNATE_OBJECT_DIRECTORIES',
	'GIT_REPLACE_REF_BASE',
	'GIT_GRAFT_FILE',
	'GIT_SHALLOW_FILE',
	'GIT_NAMESPACE',
	'GIT_EXEC_PATH',
	'GIT_CONFIG',
	'GIT_CONFIG_PARAMETERS',
	'GIT_CONFIG_COUNT',
	'GIT_CONFIG_SYSTEM',
	'GIT_CONFIG_GLOBAL',
	'GIT_CONFIG_NOSYSTEM',
	'GIT_TEMPLATE_DIR',
	'GIT_NO_LAZY_FETCH',
	'GIT_TERMINAL_PROMPT',
]

const bootstrap_blocked_v_environment = [
	'VFLAGS',
	'VMODULES',
	'VCACHE',
	'VTMP',
	'VROOT',
	'VEXE',
	'VCHILD',
	'VOSARGS',
	'VBUILD_DEFINES',
	'VBUILD_FACTS',
	'VNORUN',
	'VJOBS',
	'CFLAGS',
	'CPPFLAGS',
	'LDFLAGS',
]

const vc_capsule_readme_blob = '355b2285294fd486c1969c37e25e6f1220dab318'
const vc_capsule_readme_size = u64(188)
const vc_capsule_readme_sha256 = '5fd9cd4c0053b2a7869db839b9adf47863bec95ca07f92eb29735335cc5873f8'

fn test_vc_lock_is_strict_complete_and_single_authority() {
	lock_path := os.join_path(automation_root(), 'bootstrap', 'vc.lock')
	source := os.read_file(lock_path) or { panic(err) }
	parsed_lock := bin.parse_vc_bootstrap_lock(source) or { panic(err) }
	validated_lock := bin.validate_vc_bootstrap_contract(automation_root()) or { panic(err) }
	assert parsed_lock == validated_lock
	assert parsed_lock.repository == 'https://github.com/vlang/vc'
	assert parsed_lock.v_c.byte_size > 0
	assert parsed_lock.v_win_c.byte_size > 0
	assert parsed_lock.v_c != parsed_lock.v_win_c
	mutations := [
		source.replace_once('format=vc-lock-v1\n', ''),
		source.replace_once('tree=', 'unknown='),
		source.replace_once('repository=https://github.com/vlang/vc\ncommit=',
			'commit=${parsed_lock.commit}\nrepository='),
		source.replace_once(parsed_lock.commit, parsed_lock.commit.to_upper()),
		source.replace_once('v.c=100644 ', 'v.c=100644  '),
		source.replace_once('\n', '\r\n'),
		'${source}extra=true\n',
	]
	for mutation in mutations {
		mut rejected := false
		bin.parse_vc_bootstrap_lock(mutation) or { rejected = true }
		assert rejected
	}
}

fn test_bootstrap_script_is_syntax_valid_network_free_and_ordered() {
	script_path := os.join_path(automation_root(), 'bootstrap', 'bootstrap.sh')
	source := os.read_file(script_path) or { panic(err) }
	check := os.exec(['bash', '-n', script_path])
	assert check.exit_code == 0, check.output
	assert source.starts_with('#!/usr/bin/env bash\n\nset -euo pipefail\n')
	assert source.count("readonly vc_repository_allowlisted='https://github.com/vlang/vc'") == 1
	assert source.count("readonly bootstrap_lock_relative='thirdparty/tccbin_automation/bootstrap/vc.lock'") == 1
	assert source.count('read_lock "$lock_path"') == 1
	assert source.contains('git --no-replace-objects -C "$root" -c core.autocrlf=false')
	assert source.contains('clone --quiet --no-checkout')
	assert source.contains('--no-local --no-hardlinks --template="$template_root"')
	assert source.count('materialize_vc_artifact "$vc_root"') == 2
	assert source.contains('cat-file -t "$artifact_blob"')
	for forbidden in ['git fetch', 'git pull', 'git ls-remote', 'git submodule', 'curl ', 'wget ',
		'\neval ', '\nmake ', ' latest', '^^}', ',,}'] {
		assert !source.contains(forbidden), forbidden
	}
	cc_index := source.index('"$cc_command" -std=c99') or { panic('cc bootstrap missing') }
	v1_index := source.index(r'"$private_contract_root/v1${exe_suffix}" -no-parallel') or {
		panic('v1 bootstrap missing')
	}
	v2_index := source.index(r'"$private_contract_root/v2${exe_suffix}" -no-parallel') or {
		panic('v2 bootstrap missing')
	}
	cli_index := source.index(r'-d "tccbin_contract_repository=${contract_repository}"') or {
		panic('contract repository define missing')
	}
	oracle_index := source.index('contract-binding') or { panic('contract binding oracle missing') }
	assert cc_index < v1_index
	assert v1_index < v2_index
	assert v2_index < cli_index
	assert cli_index < oracle_index
	assert source.contains(r'-d "tccbin_contract_sha=${contract_sha}"')
	assert source.count('-no-parallel -nocache -cc "$cc_command"') == 3
	assert source.count('-gc none') == 3
	cli_path_assignment := r'cli_path="$work_root/tccbin-automation${exe_suffix}"'
	assert source.count(cli_path_assignment) == 1
	assert source.count('cli_path=') == 1
	assert source.count('cli_path') == 3
	assert source.count(r'$work_root/tccbin-automation${exe_suffix}') == 1
	assert source.count(r'$cli_path') == 2
	assert source.count(r'${cli_path}') == 0
	assert source.count(r'"$cli_path"') == 2
	assert source.count(r'-o "$cli_path"') == 1
	assert source.count(r'binding_output=$("$cli_path" contract-binding)') == 1
	for forbidden_interface in ['-silent', 'result-file', 'result_file', 'tail -n 1', 'tail -1'] {
		assert !source.contains(forbidden_interface), forbidden_interface
	}
	$if !windows {
		stat := os.lstat(script_path) or { panic(err) }
		assert stat.get_mode().owner.execute
	}
}

fn test_bootstrap_contract_rejects_republished_cli_path_expressions() {
	root := automation_root()
	bootstrap_root := os.join_path(root, 'bootstrap')
	script := os.read_file(os.join_path(bootstrap_root, 'bootstrap.sh')) or { panic(err) }
	base := os.join_path(os.temp_dir(), 'tccbin-bootstrap-output-negative-${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	for index, expression in [r'$cli_path', r'${cli_path:-}'] {
		legacy_output := 'printf ' + "'%s\\n'" + ' ${expression}\n'
		mutated := script.replace_once('bootstrap_complete=true\n',
			'${legacy_output}bootstrap_complete=true\n')
		assert mutated != script
		case_root := os.join_path(base, index.str())
		os.mkdir_all(os.join_path(case_root, 'bootstrap')) or { panic(err) }
		os.write_file(os.join_path(case_root, 'bootstrap', 'vc.lock'), os.read_file(os.join_path(bootstrap_root,
			'vc.lock')) or { panic(err) }) or { panic(err) }
		mutated_path := os.join_path(case_root, 'bootstrap', 'bootstrap.sh')
		os.write_file(mutated_path, mutated) or { panic(err) }
		$if !windows {
			os.chmod(mutated_path, 0o700) or { panic(err) }
		}
		mut rejection := ''
		bin.validate_vc_bootstrap_contract(case_root) or { rejection = err.msg() }
		assert rejection == 'VC bootstrap helper must use its deterministic CLI path without publishing it'
	}
}

fn clean_bootstrap_environment_command() []string {
	mut command := ['env']
	mut blocked_names := bootstrap_blocked_git_environment.clone()
	blocked_names << bootstrap_blocked_v_environment
	for name in os.environ().keys() {
		upper_name := name.to_upper()
		if (upper_name in bootstrap_blocked_git_environment
			|| upper_name in bootstrap_blocked_v_environment
			|| upper_name.starts_with('GIT_CONFIG_KEY_')
			|| upper_name.starts_with('GIT_CONFIG_VALUE_')) && name !in blocked_names {
			blocked_names << name
		}
	}
	for name in blocked_names {
		command << '-u'
		command << name
	}
	return command
}

fn test_bootstrap_rejects_invalid_arguments_and_injected_environments_before_checkout_access() {
	script_path := os.join_path(automation_root(), 'bootstrap', 'bootstrap.sh')
	missing := os.exec(['bash', script_path])
	assert missing.exit_code == 2
	assert missing.output.contains('usage: bootstrap.sh')
	invalid_repository := os.exec(['bash', script_path, '.', 'evil/v', 'a'.repeat(40),
		'.', os.join_path(os.temp_dir(), 'must-not-be-created')])
	assert invalid_repository.exit_code == 1
	assert invalid_repository.output.contains('contract repository is not allowlisted')
	for name in bootstrap_blocked_git_environment {
		mut command := clean_bootstrap_environment_command()
		command << '${name}=injected'
		command << ['bash', script_path, '.', 'GGRei/v', 'a'.repeat(40), '.',
			os.join_path(os.temp_dir(), 'must-not-be-created-${name}')]
		injected := os.exec(command)
		assert injected.exit_code == 1
		assert injected.output.contains('${name} must be unset'), '${name}: ${injected.output}'
	}
	for name in bootstrap_blocked_v_environment {
		mut command := clean_bootstrap_environment_command()
		command << '${name}=injected'
		command << ['bash', script_path, '.', 'GGRei/v', 'a'.repeat(40), '.',
			os.join_path(os.temp_dir(), 'must-not-be-created-${name}')]
		injected := os.exec(command)
		assert injected.exit_code == 1
		assert injected.output.contains('${name} must be unset'), '${name}: ${injected.output}'
	}
	for case_variant in ['git_dir', 'git_config_key_0', 'vbuild_facts'] {
		mut command := clean_bootstrap_environment_command()
		command << '${case_variant}=injected'
		command << ['bash', script_path, '.', 'GGRei/v', 'a'.repeat(40), '.',
			os.join_path(os.temp_dir(), 'must-not-be-created-${case_variant}')]
		injected := os.exec(command)
		assert injected.exit_code == 1
		if case_variant == 'git_config_key_0' {
			assert injected.output.contains('Git configuration injection variables must be unset'), injected.output
		} else {
			assert injected.output.contains('${case_variant.to_upper()} must be unset'), injected.output
		}
	}
}

fn bootstrap_test_git(args []string) os.Result {
	mut command := ['env', 'GIT_NO_LAZY_FETCH=1', 'GIT_TERMINAL_PROMPT=0', 'GIT_CONFIG_NOSYSTEM=1',
		'GIT_CONFIG_SYSTEM=${os.path_devnull}', 'GIT_CONFIG_GLOBAL=${os.path_devnull}', 'git',
		'--no-replace-objects']
	command << args
	return os.exec(command)
}

fn prepare_contract_bootstrap_checkout(source_root string, destination string, remote string) string {
	template_root := '${destination}.empty-template'
	os.mkdir_all(template_root) or { panic(err) }
	clone := bootstrap_test_git(['-c', 'protocol.file.allow=always', 'clone', '--quiet',
		'--no-checkout', '--no-local', '--no-hardlinks', '--template=${template_root}', source_root,
		destination])
	assert clone.exit_code == 0, clone.output
	os.rmdir_all(template_root) or { panic(err) }
	for args in [
		['-C', destination, 'config', '--local', 'core.autocrlf', 'false'],
		['-C', destination, 'config', '--local', 'user.email', 'ci@example.invalid'],
		['-C', destination, 'config', '--local', 'user.name', 'Bootstrap Contract Test'],
		['-C', destination, 'remote', 'set-url', 'origin', remote],
		['-C', destination, 'checkout', '--quiet', '--detach', '--force', 'HEAD'],
	] {
		result := bootstrap_test_git(args)
		assert result.exit_code == 0, result.output
	}
	return destination
}

struct VcCapsuleEntry {
	path      string
	blob      string
	byte_size u64
	sha256    string
}

fn prepare_locked_vc_capsule(source_root string, destination string, remote string,
	vc_lock bin.VcBootstrapLock) string {
	os.mkdir_all(destination) or { panic(err) }
	for args in [
		['-C', destination, 'init', '-q'],
		['-C', destination, 'config', '--local', 'core.autocrlf', 'false'],
		['-C', destination, 'remote', 'add', 'origin', remote],
		['-C', destination, 'config', '--local', 'remote.origin.promisor', 'true'],
		['-C', destination, 'config', '--local', 'remote.origin.partialclonefilter', 'blob:none'],
	] {
		result := bootstrap_test_git(args)
		assert result.exit_code == 0, result.output
	}
	entries := [
		VcCapsuleEntry{
			path:      'README.md'
			blob:      vc_capsule_readme_blob
			byte_size: vc_capsule_readme_size
			sha256:    vc_capsule_readme_sha256
		},
		VcCapsuleEntry{
			path:      'v.c'
			blob:      vc_lock.v_c.blob
			byte_size: vc_lock.v_c.byte_size
			sha256:    vc_lock.v_c.sha256
		},
		VcCapsuleEntry{
			path:      'v_win.c'
			blob:      vc_lock.v_win_c.blob
			byte_size: vc_lock.v_win_c.byte_size
			sha256:    vc_lock.v_win_c.sha256
		},
	]
	mut add := ['-C', destination, 'add', '--']
	for entry in entries {
		source_path := os.join_path(source_root, entry.path)
		assert os.is_file(source_path) && !os.is_link(source_path)
		bytes := os.read_bytes(source_path) or { panic(err) }
		assert u64(bytes.len) == entry.byte_size
		assert sha256.sum256(bytes).hex() == entry.sha256
		os.write_file_array(os.join_path(destination, entry.path), bytes) or { panic(err) }
		hash :=
			bootstrap_test_git(['-C', destination, 'hash-object', '--no-filters', '--', entry.path])
		assert hash.exit_code == 0, hash.output
		assert hash.output.trim_space() == entry.blob
		add << entry.path
	}
	add_result := bootstrap_test_git(add)
	assert add_result.exit_code == 0, add_result.output
	tree := bootstrap_test_git(['-C', destination, 'write-tree'])
	assert tree.exit_code == 0, tree.output
	assert tree.output.trim_space() == vc_lock.tree
	commit_source := bootstrap_test_git(['-C', source_root, 'cat-file', 'commit', vc_lock.commit])
	assert commit_source.exit_code == 0, commit_source.output
	commit_object_path := '${destination}.commit-object'
	os.write_file(commit_object_path, commit_source.output) or { panic(err) }
	defer {
		os.rm(commit_object_path) or {}
	}
	commit := bootstrap_test_git(['-C', destination, 'hash-object', '-t', 'commit', '-w',
		'--no-filters', '--', commit_object_path])
	assert commit.exit_code == 0, commit.output
	assert commit.output.trim_space() == vc_lock.commit
	for args in [
		['-C', destination, 'update-ref', 'HEAD', vc_lock.commit],
		['-C', destination, 'checkout', '--quiet', '--detach', '--force', vc_lock.commit],
	] {
		result := bootstrap_test_git(args)
		assert result.exit_code == 0, result.output
	}
	status := bootstrap_test_git(['-C', destination, 'status', '--porcelain=v1',
		'--untracked-files=all', '--ignored=matching'])
	assert status.exit_code == 0, status.output
	assert status.output == ''
	count := bootstrap_test_git(['-C', destination, 'count-objects', '-v'])
	assert count.exit_code == 0, count.output
	assert count.output.split_into_lines().any(it == 'count: 5'), count.output
	return destination
}

fn bootstrap_execution_command(script_path string, contract_input string, contract_sha string,
	vc_input string, work_root string) []string {
	mut command := clean_bootstrap_environment_command()
	command << 'GIT_ALLOW_PROTOCOL=file'
	command << 'CC=cc'
	command << 'bash'
	command << script_path
	command << contract_input
	command << 'GGRei/v'
	command << contract_sha
	command << vc_input
	command << work_root
	return command
}

fn expected_bootstrap_cli_path(work_root string) string {
	exe_suffix := $if windows { '.exe' } $else { '' }
	return os.join_path(work_root, 'tccbin-automation${exe_suffix}')
}

fn remove_vc_capsule_object(capsule_root string, oid string) {
	assert oid.len == 40
	object_path := os.join_path(capsule_root, '.git', 'objects', oid[..2], oid[2..])
	assert os.is_file(object_path) && !os.is_link(object_path)
	os.rm(object_path) or { panic(err) }
}

fn commit_current_automation_contract(source_root string, destination string) string {
	target_automation := os.join_path(destination, 'thirdparty', 'tccbin_automation')
	os.rmdir_all(target_automation) or { panic(err) }
	os.cp_all(os.join_path(source_root, 'thirdparty', 'tccbin_automation'), target_automation, true) or {
		panic(err)
	}
	for args in [
		['-C', destination, 'add', '--', 'thirdparty/tccbin_automation'],
		['-C', destination, 'commit', '--quiet', '--allow-empty', '-m', 'bootstrap contract fixture'],
	] {
		result := bootstrap_test_git(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := bootstrap_test_git(['-C', destination, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	return ref_result.output.trim_space()
}

struct BootstrapGitExpectation {
	args     []string
	expected string
}

fn test_bootstrap_compiles_the_real_local_contract_without_network_or_moving_inputs() {
	$if linux || macos {
		source_root := os.real_path(os.join_path(automation_root(), '..', '..'))
		vc_source_root := os.join_path(source_root, 'vc')
		vc_lock := bin.validate_vc_bootstrap_contract(automation_root()) or { panic(err) }
		assert os.is_dir(vc_source_root), 'the reviewed local VC snapshot is required'
		checks := [
			BootstrapGitExpectation{
				args:     ['-C', vc_source_root, 'rev-parse', '--is-shallow-repository']
				expected: 'false'
			},
			BootstrapGitExpectation{
				args:     ['-C', vc_source_root, 'rev-parse', '--verify',
					'${vc_lock.commit}^{commit}']
				expected: vc_lock.commit
			},
			BootstrapGitExpectation{
				args:     ['-C', vc_source_root, 'rev-parse', '${vc_lock.commit}^{tree}']
				expected: vc_lock.tree
			},
			BootstrapGitExpectation{
				args:     ['-C', vc_source_root, 'rev-parse', 'HEAD']
				expected: vc_lock.commit
			},
			BootstrapGitExpectation{
				args:     ['-C', vc_source_root, 'rev-parse', 'HEAD^{tree}']
				expected: vc_lock.tree
			},
			BootstrapGitExpectation{
				args:     ['-C', vc_source_root, 'config', '--local', '--get', 'core.autocrlf']
				expected: 'false'
			},
		]
		for check in checks {
			inspection := bootstrap_test_git(check.args)
			assert inspection.exit_code == 0, inspection.output
			assert inspection.output.trim_space() == check.expected
		}
		remote := bootstrap_test_git(['-C', vc_source_root, 'remote', 'get-url', 'origin'])
		assert remote.exit_code == 0, remote.output
		assert remote.output.trim_space() in [vc_lock.repository, '${vc_lock.repository}.git']
		symbolic_head := bootstrap_test_git(['-C', vc_source_root, 'symbolic-ref', '-q', 'HEAD'])
		assert symbolic_head.exit_code == 1, symbolic_head.output
		assert symbolic_head.output == ''
		status := bootstrap_test_git(['-C', vc_source_root, 'status', '--porcelain=v1',
			'--untracked-files=all', '--ignored=matching'])
		assert status.exit_code == 0, status.output
		assert status.output == ''
		base := os.join_path(os.temp_dir(), 'tccbin-bootstrap-positive-${os.getpid()}')
		os.rmdir_all(base) or {}
		os.mkdir_all(base) or { panic(err) }
		defer {
			os.rmdir_all(base) or {}
		}
		contract_input := prepare_contract_bootstrap_checkout(source_root, os.join_path(base,
			'contract-input'), 'https://github.com/GGRei/v')
		contract_sha := commit_current_automation_contract(source_root, contract_input)
		vc_input := prepare_locked_vc_capsule(vc_source_root, os.join_path(base, 'vc-input'),
			'https://github.com/vlang/vc', vc_lock)
		work_root := os.join_path(base, 'work')
		command := bootstrap_execution_command(os.join_path(contract_input, 'thirdparty',
			'tccbin_automation', 'bootstrap', 'bootstrap.sh'), contract_input, contract_sha,
			vc_input, work_root)
		result := os.exec(command)
		assert result.exit_code == 0, result.output
		cli_path := expected_bootstrap_cli_path(work_root)
		assert os.is_file(cli_path)
		assert !os.exists(os.join_path(work_root, 'contract-source', 'thirdparty', 'tcc', 'lib',
			'libgc.a'))
		assert !os.exists(os.join_path(work_root, 'contract-source', 'v1'))
		assert !os.exists(os.join_path(work_root, 'contract-source', 'v2'))
		assert !os.exists(os.join_path(work_root, 'contract-source', 'v'))
		binding := os.exec([cli_path, 'contract-binding'])
		assert binding.exit_code == 0, binding.output
		assert binding.output.trim_space() == 'repository=GGRei/v sha=${contract_sha}'
	}
}

struct MissingVcObjectCase {
	label            string
	oid              string
	expected_message string
}

fn test_bootstrap_rejects_missing_selected_and_nonselected_vc_objects_and_cleans_work_root() {
	$if linux || macos {
		source_root := os.real_path(os.join_path(automation_root(), '..', '..'))
		vc_source_root := os.join_path(source_root, 'vc')
		vc_lock := bin.validate_vc_bootstrap_contract(automation_root()) or { panic(err) }
		base := os.join_path(os.temp_dir(), 'tccbin-bootstrap-missing-${os.getpid()}')
		os.rmdir_all(base) or {}
		os.mkdir_all(base) or { panic(err) }
		defer {
			os.rmdir_all(base) or {}
		}
		contract_input := prepare_contract_bootstrap_checkout(source_root, os.join_path(base,
			'contract-input'), 'https://github.com/GGRei/v')
		contract_sha := commit_current_automation_contract(source_root, contract_input)
		cases := [
			MissingVcObjectCase{
				label:            'commit'
				oid:              vc_lock.commit
				expected_message: 'VC locked commit is not a local commit'
			},
			MissingVcObjectCase{
				label:            'tree'
				oid:              vc_lock.tree
				expected_message: 'VC locked tree is not a local tree'
			},
			MissingVcObjectCase{
				label:            'selected-v.c'
				oid:              vc_lock.v_c.blob
				expected_message: 'VC v.c Git object is not a local blob'
			},
			MissingVcObjectCase{
				label:            'nonselected-v_win.c'
				oid:              vc_lock.v_win_c.blob
				expected_message: 'VC v_win.c Git object is not a local blob'
			},
		]
		for missing in cases {
			vc_input := prepare_locked_vc_capsule(vc_source_root, os.join_path(base,
				'vc-${missing.label}'), 'https://github.com/vlang/vc', vc_lock)
			remove_vc_capsule_object(vc_input, missing.oid)
			work_root := os.join_path(base, 'work-${missing.label}')
			command := bootstrap_execution_command(os.join_path(contract_input, 'thirdparty',
				'tccbin_automation', 'bootstrap', 'bootstrap.sh'), contract_input, contract_sha,
				vc_input, work_root)
			result := os.exec(command)
			assert result.exit_code == 1, '${missing.label}: ${result.output}'
			assert result.output.contains(missing.expected_message), '${missing.label}: ${result.output}'
			assert !os.exists(work_root), missing.label
		}
	}
}
