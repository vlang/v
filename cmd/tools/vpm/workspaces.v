module main

import os
import v.vmod

// workspace_member_dirs expands the globs listed in `manifest.workspaces`
// relative to `root` and returns the directories that actually contain a
// `v.mod` file. Patterns without a wildcard are treated as literal paths.
// Absolute patterns are used as-is. The workspace root itself is never
// reported as its own member. Results are de-duplicated and sorted.
fn workspace_member_dirs(root string, manifest vmod.Manifest) []string {
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
fn nearest_workspace_root(start string) string {
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
// Otherwise it returns ''. All returned paths are canonical (`real_path`).
fn workspace_root(dir string) string {
	root := nearest_workspace_root(dir)
	if root != '' {
		return root
	}
	canon := os.real_path(dir)
	if canon != '' && os.exists(os.join_path(canon, 'v.mod')) {
		return canon
	}
	return ''
}

// current_project_root resolves the project owning the working directory: the
// workspace root when inside a workspace, the working directory itself when it
// holds a `v.mod`, and '' otherwise.
fn current_project_root() string {
	return workspace_root(os.getwd())
}

// current_project resolves the workspace-aware project for the working directory
// in a single `v.mod` parse. It returns the root directory and its manifest, or
// '' and an empty manifest when no project is in scope.
fn current_project() (string, vmod.Manifest) {
	root := current_project_root()
	if root == '' {
		return '', vmod.Manifest{}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { return '', vmod.Manifest{} }
	return root, manifest
}

// workspace_aware_project returns the cached `v.mod` location owning the working
// directory, redirected to the workspace root when inside one. It centralizes the
// `get_by_folder` + `nearest_workspace_root` dance shared by `vendor` and `why`.
fn workspace_aware_project() vmod.ModFileAndFolder {
	mut project := vmod.get_cache().get_by_folder(os.getwd())
	ws_root := nearest_workspace_root(os.getwd())
	if ws_root == '' {
		return project
	}
	if project.vmod_file == '' {
		return vmod.get_cache().get_by_folder(ws_root)
	}
	if os.real_path(os.dir(project.vmod_file)) != ws_root {
		return vmod.get_cache().get_by_folder(ws_root)
	}
	return project
}

// project_deps_for_manifest returns the direct requirements for `manifest`: the
// aggregated workspace requirements when `manifest` declares `workspaces`, and
// the plain project requirements otherwise. It removes the `if workspaces`
// branch that every `vpm` command would otherwise repeat.
fn project_deps_for_manifest(root string, manifest vmod.Manifest) []string {
	if manifest.workspaces.len > 0 {
		return workspace_dependencies_for_manifest(root, manifest)
	}
	return project_dependencies(manifest)
}

// workspace_dependencies aggregates the direct dependencies of the root manifest
// and every workspace member, de-duplicated while preserving order. It stays `pub`
// as the one-call entry for callers holding only a root path; internal commands
// holding the manifest use `workspace_dependencies_for_manifest` to avoid re-reading.
pub fn workspace_dependencies(root string) []string {
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { return []string{} }
	return workspace_dependencies_for_manifest(root, manifest)
}

// workspace_dependencies_for_manifest is the manifest-aware variant: callers that
// already hold the root manifest avoid reading `v.mod` a second time.
fn workspace_dependencies_for_manifest(root string, manifest vmod.Manifest) []string {
	mut deps := []string{}
	mut seen := map[string]bool{}
	for dep in project_dependencies(manifest) {
		if dep in seen {
			continue
		}
		seen[dep] = true
		deps << dep
	}
	for member_dir in workspace_member_dirs(root, manifest) {
		member_manifest := vmod.from_file(os.join_path(member_dir, 'v.mod')) or {
			verbose_println('Skipping workspace member `${member_dir}`: ${err.msg()}')
			continue
		}
		for dep in project_dependencies(member_manifest) {
			if dep in seen {
				continue
			}
			seen[dep] = true
			deps << dep
		}
	}
	return deps
}
