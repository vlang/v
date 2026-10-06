module main

import crypto.sha256
import os
import semver

// is_version_range distinguishes explicit constraints from existing Git refs.
fn is_version_range(version string) bool {
	if (version.len > 0 && version[0] in [`^`, `~`, `<`, `>`, `=`])
		|| version.contains_any('* \t') || version.contains('||') {
		return true
	}
	core := version.all_before('+').all_before('-')
	parts := core.split('.')
	if parts.len > 3 {
		return false
	}
	mut wildcard := false
	for part in parts {
		if part == 'x' || part == 'X' {
			wildcard = true
		} else if part == '' || !part.bytes().all(it.is_digit()) {
			return false
		}
	}
	return wildcard
}

// version_tmp_name keeps range operators out of filesystem path components.
fn version_tmp_name(version string) string {
	return if is_version_range(version) { 'range-' + sha256.hexhash(version) } else { version }
}

fn version_tag(tag string) !semver.Version {
	input := if tag.starts_with('v') { tag[1..] } else { tag }
	version := semver.from(input)!
	core_and_prerelease := input.all_before('+')
	core := core_and_prerelease.all_before('-')
	// Round-trip the core to reject leading zeros and values the module cannot represent.
	if core != '${version.major}.${version.minor}.${version.patch}'
		|| (core_and_prerelease.contains('-') && !valid_version_identifiers(version.prerelease, true))
		|| (input.contains('+') && !valid_version_identifiers(version.metadata, false)) {
		return error('invalid semantic-version tag `${tag}`')
	}
	return version
}

fn valid_version_identifiers(value string, prerelease bool) bool {
	for identifier in value.split('.') {
		if identifier == '' {
			return false
		}
		if prerelease && identifier.len > 1 && identifier[0] == `0`
			&& identifier.bytes().all(it.is_digit()) {
			return false
		}
	}
	return true
}

fn tag_satisfies_range(tag string, constraint string) bool {
	version := version_tag(tag) or { return false }
	return version.satisfies(constraint)
}

// select_version_tag returns the highest semantic-version tag in the range.
// Sort first so equal-precedence tags have a deterministic spelling.
fn select_version_tag(tags []string, constraint string) !string {
	// An unparseable constraint must not look like "no tag matched", or a typo in a
	// range reads as an empty repository.
	if !semver.is_valid_range(constraint) {
		return error('invalid version range `${constraint}`')
	}
	mut sorted := tags.clone()
	sorted.sort()
	mut selected := ''
	mut highest := semver.Version{}
	for tag in sorted {
		version := version_tag(tag) or { continue }
		if version.satisfies(constraint) && (selected == '' || version > highest) {
			selected = tag
			highest = version
		}
	}
	if selected == '' {
		return error('no semantic-version tag satisfies `${constraint}`')
	}
	return selected
}

fn (vcs VCS) resolve_version(url string, version string) !string {
	if !is_version_range(version) {
		return version
	}
	if vcs != .git {
		return error('semantic version ranges are supported only for Git repositories')
	}
	if version.contains_any('\0\r\n') || url.starts_with('-') {
		return error('invalid source or version range')
	}
	res := os.exec(['git', 'ls-remote', '--tags', '--refs', '--', url])
	if res.exit_code != 0 {
		return error('failed to list version tags from `${url}`: ${res.output.trim_space()}')
	}
	mut tags := []string{}
	for line in res.output.split_into_lines() {
		fields := line.split('\t')
		if fields.len == 2 && fields[1].starts_with('refs/tags/') {
			tags << fields[1].trim_string_left('refs/tags/')
		}
	}
	selected := select_version_tag(tags, version)!
	verbose_println('Resolved `${version}` to `${selected}` from `${url}`.')
	return selected
}

// validate_range_destinations prevents multiple selections from overwriting
// the single-version module store. Joint constraint solving is not yet supported.
fn validate_range_destinations(modules []Module) ! {
	mut seen := map[string]Module{}
	for m in modules {
		mut destination := os.norm_path(real_path_with_missing_suffix(m.install_path))
		$if windows {
			destination = destination.to_lower()
		}
		if previous := seen[destination] {
			if m.version_range != '' || previous.version_range != '' {
				return error('multiple requirements for `${m.name}` at `${fmt_mod_path(destination)}`: `${previous.requested}` selected `${previous.version}`, while `${m.requested}` selected `${m.version}`; joint version-range resolution is not yet supported')
			}
		}
		seen[destination] = m
	}
}
