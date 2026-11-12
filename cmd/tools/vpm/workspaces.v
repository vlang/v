module main

import os
import v.vmod

// workspace_member_dirs expands the globs listed in `manifest.workspaces`
// relative to `root` and returns the directories that actually contain a
// `v.mod` file. Patterns without a wildcard are treated as literal paths.
pub fn workspace_member_dirs(root string, manifest vmod.Manifest) []string {
	mut dirs := []string{}
	for raw_pattern in manifest.workspaces {
		pattern := raw_pattern.trim_space()
		if pattern == '' {
			continue
		}
		full := os.join_path(root, pattern)
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
			normalized := os.norm_path(candidate)
			if normalized !in dirs {
				dirs << normalized
			}
		}
	}
	dirs.sort()
	return dirs
}

// nearest_workspace_root walks up from `start` and returns the first directory
// whose `v.mod` declares a non-empty `workspaces` list. Returns '' when none.
pub fn nearest_workspace_root(start string) string {
	mut dir := os.real_path(start)
	for {
		vmod_path := os.join_path(dir, 'v.mod')
		if os.exists(vmod_path) {
			manifest := vmod.from_file(vmod_path) or { vmod.Manifest{} }
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

// workspace_root returns the workspace root for `dir`: the nearest ancestor with
// workspaces, or `dir` itself when it holds a `v.mod` without workspaces.
pub fn workspace_root(dir string) string {
	root := nearest_workspace_root(dir)
	if root != '' {
		return root
	}
	if os.exists(os.join_path(dir, 'v.mod')) {
		return os.real_path(dir)
	}
	return ''
}

// workspace_dependencies aggregates the direct dependencies of the root manifest
// and every workspace member, de-duplicated while preserving order.
pub fn workspace_dependencies(root string) []string {
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { return []string{} }
	mut deps := project_dependencies(manifest).clone()
	for member_dir in workspace_member_dirs(root, manifest) {
		member_manifest := vmod.from_file(os.join_path(member_dir, 'v.mod')) or { continue }
		for dep in project_dependencies(member_manifest) {
			if dep !in deps {
				deps << dep
			}
		}
	}
	return deps
}
