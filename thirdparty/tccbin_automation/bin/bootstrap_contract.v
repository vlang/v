module bin

import os

const vc_bootstrap_repository = 'https://github.com/vlang/vc'
const vc_bootstrap_lock_relative = 'bootstrap/vc.lock'
const vc_bootstrap_script_relative = 'bootstrap/bootstrap.sh'

// VcBootstrapArtifact binds one bootstrap compiler source to its immutable Git object and bytes.
pub struct VcBootstrapArtifact {
pub:
	mode      string
	blob      string
	byte_size u64
	sha256    string
}

// VcBootstrapLock is the single reviewed authority for the VC bootstrap snapshot.
pub struct VcBootstrapLock {
pub:
	repository string
	commit     string
	tree       string
	v_c        VcBootstrapArtifact
	v_win_c    VcBootstrapArtifact
}

fn parse_vc_bootstrap_artifact(record string, label string) !VcBootstrapArtifact {
	fields := record.split(' ')
	if fields.len != 4 || fields.join(' ') != record {
		return error('VC lock ${label} tuple is not canonical')
	}
	if fields[0] != '100644' {
		return error('VC lock ${label} mode is invalid')
	}
	if !is_lower_hex_40(fields[1]) {
		return error('VC lock ${label} blob is invalid')
	}
	if fields[2] == '' || fields[2][0] == `0` || fields[2].bytes().any(!it.is_digit()) {
		return error('VC lock ${label} size is invalid')
	}
	byte_size := fields[2].u64()
	if byte_size == 0 || byte_size.str() != fields[2] {
		return error('VC lock ${label} size is invalid')
	}
	if !is_lower_hex_64(fields[3]) {
		return error('VC lock ${label} SHA-256 is invalid')
	}
	return VcBootstrapArtifact{
		mode:      fields[0]
		blob:      fields[1]
		byte_size: byte_size
		sha256:    fields[3]
	}
}

// parse_vc_bootstrap_lock parses, but never executes, the six-record canonical lock.
pub fn parse_vc_bootstrap_lock(source string) !VcBootstrapLock {
	if source.len == 0 || source.len > 2048 || source.contains('\x00') || source.contains('\r')
		|| !source.ends_with('\n') {
		return error('VC lock bytes or line endings are not canonical')
	}
	lines := source[..source.len - 1].split('\n')
	if lines.len != 6 || lines[0] != 'format=vc-lock-v1' {
		return error('VC lock must contain exactly six ordered records')
	}
	prefixes := ['repository=', 'commit=', 'tree=', 'v.c=', 'v_win.c=']
	for index, prefix in prefixes {
		if !lines[index + 1].starts_with(prefix) || lines[index + 1].count(prefix) != 1 {
			return error('VC lock record set or order is invalid')
		}
	}
	repository := lines[1].all_after('repository=')
	commit := lines[2].all_after('commit=')
	tree := lines[3].all_after('tree=')
	if repository != vc_bootstrap_repository {
		return error('VC lock repository is not allowlisted')
	}
	if !is_lower_hex_40(commit) || !is_lower_hex_40(tree) {
		return error('VC lock commit and tree must be full lowercase Git object IDs')
	}
	return VcBootstrapLock{
		repository: repository
		commit:     commit
		tree:       tree
		v_c:        parse_vc_bootstrap_artifact(lines[4].all_after('v.c='), 'v.c')!
		v_win_c:    parse_vc_bootstrap_artifact(lines[5].all_after('v_win.c='), 'v_win.c')!
	}
}

// validate_vc_bootstrap_contract binds the lock and its network-free consumer into the public
// contract checks. The shell helper may clone only from already validated local roots.
pub fn validate_vc_bootstrap_contract(automation_root string) !VcBootstrapLock {
	lock_path := os.join_path(automation_root, vc_bootstrap_lock_relative)
	script_path := os.join_path(automation_root, vc_bootstrap_script_relative)
	if !os.is_file(lock_path) || os.is_link(lock_path) {
		return error('VC bootstrap lock must be a physical regular file')
	}
	if !os.is_file(script_path) || os.is_link(script_path) {
		return error('VC bootstrap helper must be a physical regular file')
	}
	vc_lock := parse_vc_bootstrap_lock(os.read_file(lock_path)!)!
	script := os.read_file(script_path)!
	if script.len == 0 || script.len > 32768 || script.contains('\x00') || script.contains('\r')
		|| !script.starts_with('#!/usr/bin/env bash\n\nset -euo pipefail\n') {
		return error('VC bootstrap helper bytes are not canonical')
	}
	for marker in [
		"readonly vc_repository_allowlisted='${vc_bootstrap_repository}'",
		"readonly bootstrap_lock_relative='thirdparty/tccbin_automation/bootstrap/vc.lock'",
		'clone --quiet --no-checkout',
		'--no-local --no-hardlinks',
		'GIT_NO_LAZY_FETCH=1',
		'GIT_CONFIG_NOSYSTEM=1',
		'VBUILD_FACTS',
		r'-d "tccbin_contract_repository=${contract_repository}"',
		r'-d "tccbin_contract_sha=${contract_sha}"',
		'contract-binding',
	] {
		if !script.contains(marker) {
			return error('VC bootstrap helper omits a required closed-world control')
		}
	}
	if script.count('-no-parallel -nocache -cc "$cc_command"') != 3 {
		return error('VC bootstrap helper must compile all three V stages with explicit flags')
	}
	if script.count('-gc none') != 3 {
		return error('VC bootstrap helper must compile all three V stages without a garbage collector')
	}
	cli_path_assignment := r'cli_path="$work_root/tccbin-automation${exe_suffix}"'
	if script.count(cli_path_assignment) != 1 || script.count('cli_path=') != 1 {
		return error('VC bootstrap helper must derive its deterministic CLI path exactly once')
	}
	if script.count('cli_path') != 3
		|| script.count(r'$work_root/tccbin-automation${exe_suffix}') != 1
		|| script.count(r'$cli_path') != 2 || script.count(r'${cli_path}') != 0
		|| script.count(r'"$cli_path"') != 2 || script.count(r'-o "$cli_path"') != 1
		|| script.count(r'binding_output=$("$cli_path" contract-binding)') != 1 {
		return error('VC bootstrap helper must use its deterministic CLI path without publishing it')
	}
	for forbidden in ['git fetch', 'git pull', 'git ls-remote', 'git submodule', 'curl ', 'wget ',
		'\neval ', '\nmake ', ' latest', '^^}', ',,}', '-silent', 'result-file', 'result_file',
		'tail -n 1', 'tail -1'] {
		if script.contains(forbidden) {
			return error('VC bootstrap helper contains a network, dynamic, or moving-revision operation')
		}
	}
	$if !windows {
		mode := os.lstat(script_path)!.get_mode()
		if !mode.owner.execute {
			return error('VC bootstrap helper must be owner-executable')
		}
	}
	return vc_lock
}
