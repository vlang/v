module main

import crypto.sha256
import os
import semver
import rand
import strconv
import time

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
	// Validate policy before discovering tags; malformed input cannot disable filtering.
	cutoff := if settings.exclude_newer == '' {
		i64(0)
	} else {
		release_cutoff_unix(settings.exclude_newer)!
	}
	age := if settings.minimum_release_age == '' {
		i64(0)
	} else {
		release_age_seconds(settings.minimum_release_age)!
	}
	mut tags := []string{}
	for tag in fetch_tags(url)! {
		version_tag(tag) or { continue }
		if settings.exclude_newer != '' || settings.minimum_release_age != '' {
			date := tag_commit_date(url, tag)!
			timestamp := time.parse_rfc3339(date)!.unix()
			if (settings.exclude_newer != '' && timestamp > cutoff)
				|| (settings.minimum_release_age != '' && timestamp > time.now().unix() - age) {
				continue
			}
		}
		tags << tag
	}
	selected := select_version_tag(tags, version)!
	verbose_println('Resolved `${version}` to `${selected}` from `${url}`.')
	return selected
}

fn release_cutoff_unix(value string) !i64 {
	input := if value.len == 10 { value + 'T00:00:00Z' } else { value }
	parsed := time.parse_rfc3339(input) or { return error('invalid --exclude-newer date `${value}`: ${err.msg()}') }
	return parsed.unix()
}

fn release_age_seconds(value string) !i64 {
	if value == '' { return error('--minimum-release-age requires a duration') }
	mut digits := value
	mut scale := i64(3600)
	if value[value.len - 1] in [`d`, `h`, `m`] {
		digits = value[..value.len - 1]
		scale = match value[value.len - 1] {
			`d` { i64(86400) }
			`m` { i64(60) }
			else { i64(3600) }
		}
	}
	if digits == '' || !digits.bytes().all(it.is_digit()) {
		return error('invalid --minimum-release-age `${value}`; use hours, or a d/h/m suffix')
	}
	amount := strconv.parse_int(digits, 10, 64) or { return error('invalid --minimum-release-age `${value}`: ${err.msg()}') }
	if amount > i64(0x7fffffffffffffff) / scale {
		return error('--minimum-release-age `${value}` is too large')
	}
	return amount * scale
}

fn is_tag_too_new(tag_date string, age string) !bool {
	timestamp := time.parse_rfc3339(tag_date)!.unix()
	return timestamp > time.now().unix() - release_age_seconds(age)!
}

fn tag_commit_date(url string, tag string) !string {
	// Each discovery owns its directory, including concurrent calls for the same tag.
	tmp_dir := get_tmp_path(settings.tmp_path, 'tag-date-' + rand.ulid())!
	defer { os.rmdir_all(tmp_dir) or {} }
	VCS.git.clone(url, tag, tmp_dir)!
	date_res := os.exec(['git', '-C', tmp_dir, 'log', '-1', '--format=%cI'])
	if date_res.exit_code != 0 {
		return error('failed to get date for tag `${tag}` from `${url}`: ${date_res.output.trim_space()}')
	}
	date := date_res.output.trim_space()
	time.parse_rfc3339(date)!
	return date
}

// validate_range_destinations guards independent non-project selections from
// overwriting the single-version module store. Project installs solve the graph jointly.
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

fn release_tag_allowed(url string, tag string) !bool {
	if settings.exclude_newer == '' && settings.minimum_release_age == '' { return true }
	timestamp := time.parse_rfc3339(tag_commit_date(url, tag)!)!.unix()
	if settings.exclude_newer != '' && timestamp > release_cutoff_unix(settings.exclude_newer)! {
		return false
	}
	if settings.minimum_release_age != '' && timestamp > time.now().unix() - release_age_seconds(settings.minimum_release_age)! {
		return false
	}
	return true
}
