module driver

import os
import v.cmdexec

// UnchangedBuildLinkInputs are files that V itself reads for a build besides its
// sources, for the receipt of an unchanged build (see v3_unchanged_build_prepare):
// the `files`, the `dirs` whose entries decide what is found, and the `missing`
// paths where a file would be read if it appeared. `ok` is false when one of them
// cannot be told, for the `reason`: then the build records no receipt.
struct UnchangedBuildLinkInputs {
mut:
	files   []string
	dirs    []string
	missing []string
	ok      bool = true
	reason  string
}

// v3_project_manifest_inputs returns what decides which V sources a build takes
// besides the directories of those sources: for each of them, the nearest `v.mod`
// with the directory that holds it, the absent `v.mod` and `.v.mod.stop` of the
// directories on the way to it, and the directories that its `subdirs` reach.
// `go build` reads go.mod again in every build (load.PackagesAndErrors,
// modload.LoadPackages) and Odin gathers its files after parsing them (cached.cpp,
// cache_gather_files); a receipt that skips the parser has to compare what selects
// the sources instead. A directory inside the V checkout is no exception: a
// project or an example there selects its sources with its own `v.mod`, and a
// module of vlib reads the `v.mod` of its directory (v3_directory_user_files).
fn v3_project_manifest_inputs(source_dirs []string) UnchangedBuildLinkInputs {
	mut result := UnchangedBuildLinkInputs{}
	mut seen := map[string]bool{}
	mut seen_subdirs := map[string]bool{}
	for source_dir in source_dirs {
		mut current := source_dir
		for !seen[current] {
			seen[current] = true
			manifest := os.join_path_single(current, 'v.mod')
			if os.is_file(manifest) {
				result.files << manifest
				result.dirs << current
				for subdir in vmod_subdirs(current) or { []string{} } {
					result.add_subdir_tree(os.join_path_single(current, subdir), mut seen_subdirs)
				}
				break
			}
			result.missing << manifest
			stop := os.join_path_single(current, '.v.mod.stop')
			if os.is_file(stop) {
				result.files << stop
				break
			}
			result.missing << stop
			parent := os.dir(current)
			if parent == current {
				break
			}
			current = parent
		}
	}
	return result
}

// add_subdir_tree records a directory that a `subdirs` of a v.mod names and the
// directories below it, which the sources of the module may come to be in. Each
// is recorded by the path that leads to it from the manifest, so that a symbolic
// link on the way that comes to point elsewhere changes the receipt; `seen` holds
// the resolved directories already walked, which a link may lead back to.
fn (mut result UnchangedBuildLinkInputs) add_subdir_tree(path string, mut seen map[string]bool) {
	if !os.is_dir(path) {
		result.missing << path
		return
	}
	result.dirs << path
	real := os.real_path(path)
	if seen[real] {
		return
	}
	seen[real] = true
	for entry in os.ls(path) or { []string{} } {
		child := os.join_path_single(path, entry)
		if os.is_dir(child) && !os.is_file(os.join_path_single(child, 'v.mod')) {
			result.add_subdir_tree(child, mut seen)
		}
	}
}

// v3_module_shadow_inputs returns where another copy of a module of the build
// would be found instead of it, or would be merged with it: the same path below the
// directories of the program and of its project, and below the other module roots.
// A directory there may get sources; a path that is absent has to stay absent.
fn v3_module_shadow_inputs(source_dirs []string, module_roots []string, project_dirs []string) UnchangedBuildLinkInputs {
	mut result := UnchangedBuildLinkInputs{}
	mut places := project_dirs.clone()
	for dir in source_dirs {
		if !module_roots.any(v3_path_is_below(dir, it)) && dir !in places {
			places << dir
		}
	}
	places << module_roots
	mut seen := map[string]bool{}
	for dir in source_dirs {
		for root in module_roots {
			if !v3_path_is_below(dir, root) {
				continue
			}
			relative := dir[root.len + 1..]
			for place in places {
				if place == root {
					continue
				}
				candidate := os.join_path_single(place, relative)
				if candidate in seen {
					continue
				}
				seen[candidate] = true
				if os.is_dir(candidate) {
					result.dirs << candidate
				} else {
					result.missing << candidate
				}
			}
			break
		}
	}
	return result
}

// v3_receipt_lookup_path returns the path by which a build reads a file or lists
// a directory, made absolute but not resolved. A receipt records that path and
// not where it leads: its signatures follow symbolic links (stat and open), so
// they cover both the resolution and the contents, while a resolved path would
// keep validating the old target of a link that came to point elsewhere.
fn v3_receipt_lookup_path(path string) string {
	return if os.is_abs_path(path) { path } else { os.join_path_single(os.getwd(), path) }
}

// v3_source_lookup_dir returns the directory that a build found the source `path`
// in, which resolves to `real`, when a symbolic link on the way makes it another
// directory than the one that holds `real`, or ''. Its listing, which follows the
// link, changes when the link comes to point elsewhere.
fn v3_source_lookup_dir(path string, real string) string {
	if path.len == 0 {
		return ''
	}
	dir := os.dir(v3_receipt_lookup_path(path))
	return if dir == os.dir(real) { '' } else { dir }
}

// v3_path_is_below_one_of reports whether `path` is inside one of `dirs`.
fn v3_path_is_below_one_of(path string, dirs []string) bool {
	return dirs.any(v3_path_is_below(path, it))
}

// v3_pkgconfig_files returns the .pc files that the flags of `packages` come
// from, following their requirements, and the directories pkg-config searches:
// a .pc file added to an earlier one would be found instead.
fn v3_pkgconfig_files(packages []string) UnchangedBuildLinkInputs {
	mut result := UnchangedBuildLinkInputs{}
	if packages.len == 0 {
		return result
	}
	search := cmdexec.run('pkg-config', ['--variable', 'pc_path', 'pkg-config'])
	mut dirs := os.getenv('PKG_CONFIG_PATH').split(os.path_delimiter).filter(it.len > 0)
	if search.exit_code == 0 {
		dirs << search.output.trim_space().split(os.path_delimiter).filter(it.len > 0)
	}
	for dir in dirs {
		if os.is_dir(dir) {
			result.dirs << v3_receipt_lookup_path(dir)
		}
	}
	mut pending := packages.clone()
	mut seen := map[string]bool{}
	for pending.len > 0 {
		name := pending.pop()
		if name in seen {
			continue
		}
		seen[name] = true
		dir := cmdexec.run('pkg-config', ['--variable=pcfiledir', name])
		pc_file := os.join_path_single(dir.output.trim_space(), '${name}.pc')
		if dir.exit_code != 0 || !os.is_file(pc_file) {
			result.ok = false
			result.reason = 'pkg-config does not find ${name}'
			return result
		}
		result.files << v3_receipt_lookup_path(pc_file)
		for option in ['--print-requires', '--print-requires-private'] {
			requires := cmdexec.run('pkg-config', [option, name])
			if requires.exit_code != 0 {
				result.ok = false
				result.reason = 'pkg-config does not answer for ${name}'
				return result
			}
			for line in requires.output.split_into_lines() {
				required := line.trim_space().all_before(' ')
				if required.len > 0 && required !in seen {
					pending << required
				}
			}
		}
	}
	return result
}

// v3_pkgconfig_packages returns the package names that a `#pkgconfig` directive
// asks for, without its options and version constraints.
fn v3_pkgconfig_packages(raw string) []string {
	args := cmdexec.split_args(raw) or { return []string{} }
	mut packages := []string{}
	mut skip_version := false
	for arg in args {
		if skip_version {
			skip_version = false
			continue
		}
		if arg in ['>=', '<=', '=', '>', '<', '!='] {
			skip_version = true
			continue
		}
		if arg.len == 0 || arg.starts_with('-') {
			continue
		}
		packages << arg
	}
	return packages
}
