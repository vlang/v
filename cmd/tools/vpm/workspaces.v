module main

import os
import v.vmod

// WorkspaceManifest pairs a workspace member directory with its parsed manifest.
// The root manifest is always first; members follow in sorted member order.
struct WorkspaceManifest {
mut:
	dir      string
	manifest vmod.Manifest
}

// workspace_manifests returns the root manifest plus every workspace member
// manifest. Callers needing per-manifest indexes (e.g. `--latest` write-back)
// use this instead of the flattened dependency list. Unreadable member manifests
// are skipped, the same as in dependency aggregation.
fn workspace_manifests(root string, root_manifest vmod.Manifest) []WorkspaceManifest {
	mut out := [WorkspaceManifest{
		dir:      root
		manifest: root_manifest
	}]
	if root_manifest.workspaces.len == 0 {
		return out
	}
	for member_dir in vmod.workspace_member_dirs(root, root_manifest) {
		member_manifest := vmod.from_file(os.join_path(member_dir, 'v.mod')) or { continue }
		out << WorkspaceManifest{
			dir:      member_dir
			manifest: member_manifest
		}
	}
	return out
}

// widen_for_latest rewrites one dependency string to its `@*` form when
// `--latest` selects it: every requirement is widened unless targets limit the
// run to packages whose lock key is absent from `changes`.
fn widen_for_latest(dep string, targets []string, changes map[string]string) string {
	ident := lockfile_module_key(dep)
	if targets.len != 0 && ident !in changes {
		return dep
	}
	constraint := requirement_version(dep)
	if constraint == '' || is_version_range(constraint) || tag_satisfies_range(constraint, '*') {
		return ident + '@*'
	}
	return dep
}

// workspace_root returns the workspace root for `dir`: the nearest ancestor with
// workspaces, or `dir` itself when it holds a `v.mod` without workspaces.
// Otherwise it returns ''. All returned paths are canonical (`real_path`).
fn workspace_root(dir string) string {
	root := vmod.nearest_workspace_root(dir)
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
	ws_root := vmod.nearest_workspace_root(os.getwd())
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
	for member_dir in vmod.workspace_member_dirs(root, manifest) {
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
