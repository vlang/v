module vmod

import os

// workspace_member_dirs expands the globs listed in `manifest.workspaces`
// relative to `root` and returns the directories that actually contain a
// `v.mod` file. Patterns without a wildcard are treated as literal paths.
// Absolute patterns are used as-is. The workspace root itself is never
// reported as its own member. Results are de-duplicated and sorted.
pub fn workspace_member_dirs(root string, manifest Manifest) []string {
	mut dirs := []string{}
	if manifest.workspaces.len == 0 {
		return dirs
	}
	canon_root := os.real_path(root)
	mut seen := map[string]bool{}
	for raw_pattern in manifest.workspaces {
		pattern := raw_pattern.trim_space()
		if pattern == '' || pattern == '.' {
			continue
		}
		full := if os.is_abs_path(pattern) { pattern } else { os.join_path(root, pattern) }
		mut candidates := []string{}
		if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
			candidates = os.glob(full) or { []string{} }
		} else if os.is_dir(full) {
			candidates << full
		}
		for candidate in candidates {
			if !os.is_dir(candidate) {
				continue
			}
			if !os.exists(os.join_path(candidate, 'v.mod')) {
				continue
			}
			normalized := os.real_path(candidate)
			if normalized == '' || normalized == canon_root {
				continue
			}
			if normalized in seen {
				continue
			}
			seen[normalized] = true
			dirs << normalized
		}
	}
	dirs.sort()
	return dirs
}

// nearest_workspace_root walks up from `start` and returns the first directory
// whose `v.mod` declares a non-empty `workspaces` list. It returns '' when none
// is found. `start` may be a file or a directory; files resolve via their folder.
pub fn nearest_workspace_root(start string) string {
	mut dir := os.real_path(start)
	if dir == '' {
		dir = os.abs_path(start)
	}
	if !os.is_dir(dir) {
		dir = os.dir(dir)
	}
	for _ in 0 .. 256 {
		vmod_path := os.join_path(dir, 'v.mod')
		if os.exists(vmod_path) {
			manifest := from_file(vmod_path) or { Manifest{} }
			if manifest.workspaces.len > 0 {
				return dir
			}
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ''
}
