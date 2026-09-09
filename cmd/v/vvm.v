module main

import os
import v.pref
import v.util.version

const vvmrc_file_name = '.vvmrc'

const vvmrc_skip_env = 'V_SKIP_VVMRC'

const vvmrc_latest_aliases = ['latest', 'current']

const vvmrc_stop_paths = ['.git', '.hg', '.svn', '.v.mod.stop']

fn maybe_delegate_to_vvmrc(command string, prefs &pref.Preferences) {
	if os.getenv(vvmrc_skip_env) != '' {
		return
	}
	if !is_vvmrc_relevant_command(command, prefs) {
		return
	}
	target_path := if prefs.path != '' { prefs.path } else { command }
	vvmrc_path := find_project_vvmrc(target_path)
	if vvmrc_path == '' {
		return
	}
	requested_version := parse_vvmrc_version(os.read_file(vvmrc_path) or { '' })
	if requested_version == '' || is_vvmrc_latest_alias(requested_version) {
		return
	}
	normalized_requested_version := normalize_vvmrc_version(requested_version)
	if normalized_requested_version == normalize_vvmrc_version(version.v_version) {
		return
	}
	vversion_exe := resolve_vversion_executable(normalized_requested_version) or {
		eprintln('v: warning: `${vvmrc_path}` requests V `${requested_version}`, but no matching compiler executable was found. Continuing with V ${version.v_version}.')
		return
	}
	this_vexe := os.real_path(pref.vexe_path())
	if os.real_path(vversion_exe) == this_vexe {
		return
	}
	if prefs.is_verbose {
		eprintln('v: `.vvmrc` selected V `${requested_version}` from `${vvmrc_path}` => ${vversion_exe}')
	}
	mut process := os.new_process(vversion_exe)
	process.set_args(os.args[1..])
	mut envs := os.environ()
	envs[vvmrc_skip_env] = '1'
	envs['VEXE'] = vversion_exe
	process.set_environment(envs)
	process.wait()
	exit_code := if process.code == -1 { 1 } else { process.code }
	process.close()
	exit(exit_code)
}

fn is_vvmrc_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.path in ['', '-'] {
		return false
	}
	if command in ['run', 'crun', 'build', 'build-module'] {
		return true
	}
	return command.ends_with('.v') || os.exists(command)
}

fn find_project_vvmrc(target_path string) string {
	if target_path in ['', '-'] {
		return ''
	}
	mut folder := if os.is_dir(target_path) { target_path } else { os.dir(target_path) }
	if folder == '' {
		folder = os.getwd()
	}
	mut current := os.real_path(folder)
	for _ in 0 .. 256 {
		vvmrc_path := os.join_path(current, vvmrc_file_name)
		if os.is_file(vvmrc_path) {
			return vvmrc_path
		}
		if has_vvmrc_stop_marker(current) {
			break
		}
		parent := os.dir(current)
		if parent in ['', current] {
			break
		}
		current = parent
	}
	return ''
}

fn has_vvmrc_stop_marker(folder string) bool {
	for stop_path in vvmrc_stop_paths {
		if os.exists(os.join_path(folder, stop_path)) {
			return true
		}
	}
	return false
}

fn parse_vvmrc_version(content string) string {
	for raw_line in content.split_into_lines() {
		line := raw_line.all_before('#').trim_space()
		if line != '' {
			return line
		}
	}
	return ''
}

fn normalize_vvmrc_version(version_name string) string {
	mut normalized := version_name.trim_space()
	if normalized.len > 1 && normalized[0] in [`v`, `V`] && normalized[1].is_digit() {
		normalized = normalized[1..]
	}
	return normalized
}

fn is_vvmrc_latest_alias(version_name string) bool {
	return normalize_vvmrc_version(version_name).to_lower_ascii() in vvmrc_latest_aliases
}

fn resolve_vversion_executable(version_name string) !string {
	raw_version := version_name.trim_space()
	if raw_version == '' {
		return error('empty version')
	}
	for candidate_name in versioned_v_executable_names(raw_version) {
		if found_path := os.find_abs_path_of_executable(candidate_name) {
			return found_path
		}
	}
	for candidate_path in versioned_v_executable_paths(raw_version) {
		if os.is_file(candidate_path) {
			return candidate_path
		}
	}
	return error('v executable for `${raw_version}` was not found')
}

fn versioned_v_executable_names(version_name string) []string {
	mut names := []string{}
	raw_version := version_name.trim_space()
	if raw_version == '' {
		return names
	}
	names << raw_version
	names << 'v${raw_version}'
	if raw_version.starts_with('v') {
		trimmed := raw_version[1..]
		if trimmed != '' {
			names << trimmed
		}
	}
	return unique_strings(names)
}

fn versioned_v_executable_paths(version_name string) []string {
	mut paths := []string{}
	normalized_version := normalize_vvmrc_version(version_name)
	if normalized_version == '' {
		return paths
	}
	paths << os.join_path('/usr/lib/v', normalized_version, 'bin', 'v')
	paths << os.join_path('/usr/local/bin', 'v${normalized_version}')
	for env_name in ['VVM_HOME', 'VVM_DIR'] {
		vvm_root := os.getenv(env_name).trim_space()
		if vvm_root == '' {
			continue
		}
		paths << os.join_path(vvm_root, normalized_version, 'bin', 'v')
		paths << os.join_path(vvm_root, 'versions', normalized_version, 'bin', 'v')
	}
	return unique_strings(paths)
}

fn unique_strings(items []string) []string {
	mut seen := map[string]bool{}
	mut result := []string{}
	for item in items {
		if item in seen {
			continue
		}
		seen[item] = true
		result << item
	}
	return result
}
