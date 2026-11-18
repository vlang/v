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
// use this instead of the flattened dependency list. A member whose `v.mod`
// cannot be read or parsed is an error: skipping it would silently drop its
// requirements and rewrite the lockfile without them.
fn workspace_manifests(root string, root_manifest vmod.Manifest) ![]WorkspaceManifest {
	mut out := [WorkspaceManifest{
		dir:      root
		manifest: root_manifest
	}]
	if root_manifest.workspaces.len == 0 {
		return out
	}
	for member_dir in vmod.workspace_member_dirs(root, root_manifest) {
		member_manifest := vmod.from_file(os.join_path(member_dir, 'v.mod')) or {
			return error('failed to read workspace member `${member_dir}`: ${err.msg()}')
		}
		out << WorkspaceManifest{
			dir:      member_dir
			manifest: member_manifest
		}
	}
	return out
}

// widen_for_latest rewrites one dependency string to its `@*` form when
// `--latest` selects it: every requirement is widened unless targets limit the
// run to packages whose normalized lock key is absent from `changes`.
// Selection uses the identity normalized against the declaring manifest's
// directory (`base_dir`), so a relative member requirement matches the
// normalized keys in `changes`; the widened string keeps the original
// declaration spelling for manifest write-back.
fn widen_for_latest(dep string, targets []string, changes map[string]string, base_dir string) string {
	ident := lockfile_module_key(workspace_normalize_dep(dep, base_dir))
	if targets.len != 0 && ident !in changes {
		return dep
	}
	constraint := requirement_version(dep)
	if constraint == '' || is_version_range(constraint) || tag_satisfies_range(constraint, '*') {
		return lockfile_module_key(dep) + '@*'
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
fn project_deps_for_manifest(root string, manifest vmod.Manifest) ![]string {
	if manifest.workspaces.len > 0 {
		return workspace_dependencies_for_manifest(root, manifest)!
	}
	return project_dependencies(manifest)
}

// workspace_dependencies aggregates the direct dependencies of the root manifest
// and every workspace member, de-duplicated while preserving order. It stays `pub`
// as the one-call entry for callers holding only a root path; internal commands
// holding the manifest use `workspace_dependencies_for_manifest` to avoid re-reading.
pub fn workspace_dependencies(root string) ![]string {
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or {
		return error('failed to read `${os.join_path(root, 'v.mod')}`: ${err.msg()}')
	}
	return workspace_dependencies_for_manifest(root, manifest)!
}

// workspace_split_dep splits a dependency string into its identity and version
// suffix, mirroring `lockfile_module_key` without collapsing registries.
fn workspace_split_dep(dep string) (string, string) {
	if dep.starts_with('git@') {
		if dep.count('@') > 1 {
			ident, version := dep.rsplit_once('@') or { dep, '' }
			return ident, version
		}
		return dep, ''
	}
	ident, version := dep.rsplit_once('@') or { dep, '' }
	if version != '' && version.contains(':') {
		return '${ident}@${version}', ''
	}
	return ident, version
}

// workspace_normalize_dep resolves a relative local dependency against the
// directory of the manifest that declares it. Registered names and already
// absolute locals pass through unchanged, so `v install` works from any
// workspace directory: a root `./deps/x` becomes `/ws/deps/x` and a member
// `./local` becomes `/ws/members/svc/local`, both absolute before resolution.
// A relative `file://` URL keeps its scheme with an absolute path.
fn workspace_normalize_dep(dep string, base_dir string) string {
	ident, version := workspace_split_dep(dep)
	version_suffix := if version == '' { '' } else { '@' + version }
	if ident.starts_with('file://') {
		inner := os.expand_tilde_to_home(ident.trim_string_left('file://'))
		if os.is_abs_path(inner) {
			return dep
		}
		if base_dir == '' {
			return dep
		}
		abs := os.real_path(os.join_path(base_dir, inner))
		resolved := if abs == '' { os.abs_path(os.join_path(base_dir, inner)) } else { abs }
		return 'file://' + resolved + version_suffix
	}
	if os.is_abs_path(os.expand_tilde_to_home(ident)) {
		return dep
	}
	expanded := os.expand_tilde_to_home(ident)
	is_relative := expanded.starts_with('./') || expanded.starts_with('../')
		|| expanded.starts_with('.\\') || expanded.starts_with('..\\')
	mut is_local := is_relative
	if !is_local && base_dir != '' && expanded != '' && expanded != '.' && expanded != '..'
		&& os.exists(os.join_path(base_dir, expanded)) {
		is_local = true
	}
	if !is_local {
		return dep
	}
	if base_dir == '' {
		return dep
	}
	abs := os.real_path(os.join_path(base_dir, expanded))
	resolved := if abs == '' { os.abs_path(os.join_path(base_dir, expanded)) } else { abs }
	return resolved + version_suffix
}

// workspace_dependencies_for_manifest is the manifest-aware variant: callers that
// already hold the root manifest avoid reading `v.mod` a second time. Local
// requirements are normalized against the manifest that declares them, and a
// member whose `v.mod` cannot be parsed aborts the run instead of silently
// dropping its requirements from the install and the lockfile.
fn workspace_dependencies_for_manifest(root string, manifest vmod.Manifest) ![]string {
	mut deps := []string{}
	mut seen := map[string]bool{}
	for dep in project_dependencies(manifest) {
		normalized := workspace_normalize_dep(dep, root)
		if normalized in seen {
			continue
		}
		seen[normalized] = true
		deps << normalized
	}
	for member_dir in vmod.workspace_member_dirs(root, manifest) {
		member_manifest := vmod.from_file(os.join_path(member_dir, 'v.mod')) or {
			return error('failed to read workspace member `${member_dir}`: ${err.msg()}')
		}
		for dep in project_dependencies(member_manifest) {
			normalized := workspace_normalize_dep(dep, member_dir)
			if normalized in seen {
				continue
			}
			seen[normalized] = true
			deps << normalized
		}
	}
	return deps
}
