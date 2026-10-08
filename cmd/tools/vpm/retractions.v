module main

import os
import rand
import semver
import v.vmod

// Read the latest stable release's policy once, rather than inspecting every
// historical manifest. A later release can retract older published versions.
fn latest_version_retractions(url string, tags []string) ![]string {
	latest := select_version_tag(tags, '*') or { return []string{} }
	path := get_tmp_path(settings.tmp_path, 'retractions-' + rand.ulid())!
	defer { os.rmdir_all(path) or {} }
	VCS.git.clone(url, latest, path)!
	manifest_path := os.join_path(path, 'v.mod')
	if !os.is_file(manifest_path) {
		return []string{}
	}
	manifest := vmod.from_file(manifest_path)!
	ranges := manifest.unknown['retracted'] or { []string{} }
	for constraint in ranges {
		if !semver.is_valid_range(constraint) {
			return error('invalid retracted version range `${constraint}` in `${url}@${latest}`')
		}
	}
	return ranges
}

fn version_is_retracted(tag string, ranges []string) bool {
	return ranges.any(tag_satisfies_range(tag, it))
}
