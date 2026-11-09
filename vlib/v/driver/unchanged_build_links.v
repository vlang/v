module driver

import os
import v.cmdexec
import v.modulecache

// UnchangedBuildLinkInputs are the files outside the program that a build reads
// besides its sources, for the receipt of an unchanged build (see
// v3_unchanged_build_prepare): the `files`, the `dirs` whose entries decide what is
// found, and the `missing` paths where a file would be read if it appeared. `ok` is
// false when one of them cannot be told, for the `reason`: then the build records
// no receipt.
struct UnchangedBuildLinkInputs {
mut:
	files   []string
	dirs    []string
	missing []string
	ok      bool = true
	reason  string
}

// V3ReceiptNativeUnit is a C translation unit of a build whose included files
// a receipt lists, each as Zig's whole cache manifest takes in the depfile of
// every C object (Compilation.updateCObject).
struct V3ReceiptNativeUnit {
	path     string
	compiler string
	args     []string
}

// v3_external_native_sources returns the C sources that the flags of a build hand
// to the compiler, or that it compiles to an object that the flags name.
fn v3_external_native_sources(flags []string) []string {
	mut sources := []string{}
	for index in c_link_input_indices(flags) {
		clean := flags[index].trim_space()
		if c_flag_is_c_source_file(clean) && os.is_file(clean) {
			sources << clean
		} else if c_flag_is_object_file(clean) && !os.exists(clean) {
			if source := c_source_from_object_file(clean) {
				sources << source
			}
		}
	}
	return sources
}

// v3_unchanged_build_link_inputs finds what the link of a build reads with the
// parser of the link command of the program executable cache, which keeps every
// form of a library in every directory of the search as a file or as an absent path
// (Go's Builder.linkActionID hashes what it hands to the linker): which of them
// `-static` or `-Bstatic` selects does not matter for a receipt. A macOS
// library that has no file, such as one served from the dyld shared cache, is keyed
// by the version of the system instead.
fn v3_unchanged_build_link_inputs(flags []string, compiler string, tcc_lib_dir string, target_os string, record_dir string) UnchangedBuildLinkInputs {
	mut args := flags.clone()
	sdk := if target_os == 'macos' { cmdexec.macos_sdk_root() } else { '' }
	if target_os == 'macos' {
		args << ['-F', os.join_path(sdk, 'System', 'Library', 'Frameworks'), '-F', '/Library/Frameworks']
	}
	manager := modulecache.Manager{
		dir:     record_dir
		enabled: record_dir.len > 0
	}
	build_dir := os.getwd()
	inputs := v3_program_link_inputs(args, tcc_lib_dir, v3_default_link_library_dirs(&manager, compiler,
		v3_link_search_args(args), sdk, build_dir), build_dir)
	if inputs.reason().len > 0 {
		return UnchangedBuildLinkInputs{
			ok:     false
			reason: inputs.reason()
		}
	}
	mut result := UnchangedBuildLinkInputs{
		files:   inputs.files.map(os.real_path(it))
		missing: inputs.missing
	}
	if inputs.unresolved.len > 0 {
		system_version := '/System/Library/CoreServices/SystemVersion.plist'
		if target_os == 'macos' && os.is_file(system_version) {
			result.files << system_version
		} else {
			result.ok = false
			result.reason = 'no directory of the search holds ${inputs.unresolved.join(', ')}'
		}
	}
	return result
}

// v3_project_manifest_inputs returns what decides which V sources a build takes
// besides the directories of those sources: for each of them outside V itself, the
// nearest `v.mod` with the directory that holds it, the absent `v.mod` and
// `.v.mod.stop` of the directories on the way to it, and the directories that its
// `subdirs` reach. `go build` reads go.mod again in every build
// (load.PackagesAndErrors, modload.LoadPackages) and Odin gathers its files after
// parsing them (cached.cpp, cache_gather_files); a receipt that skips the parser
// has to compare what selects the sources instead.
fn v3_project_manifest_inputs(source_dirs []string, vroot string) UnchangedBuildLinkInputs {
	mut result := UnchangedBuildLinkInputs{}
	mut seen := map[string]bool{}
	for source_dir in source_dirs {
		if v3_path_is_below(source_dir + os.path_separator, vroot.trim_right(os.path_separator)) {
			continue
		}
		mut current := source_dir
		for !seen[current] {
			seen[current] = true
			manifest := os.join_path_single(current, 'v.mod')
			if os.is_file(manifest) {
				result.files << os.real_path(manifest)
				result.dirs << current
				for subdir in vmod_subdirs(current) or { []string{} } {
					result.add_subdir_tree(os.join_path_single(current, subdir))
				}
				break
			}
			result.missing << manifest
			stop := os.join_path_single(current, '.v.mod.stop')
			if os.is_file(stop) {
				result.files << os.real_path(stop)
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
// directories below it, which the sources of the module may come to be in.
fn (mut result UnchangedBuildLinkInputs) add_subdir_tree(path string) {
	if !os.is_dir(path) {
		result.missing << path
		return
	}
	real := os.real_path(path)
	if real in result.dirs {
		return
	}
	result.dirs << real
	for entry in os.ls(real) or { []string{} } {
		child := os.join_path_single(real, entry)
		if os.is_dir(child) && !os.is_file(os.join_path_single(child, 'v.mod')) {
			result.add_subdir_tree(child)
		}
	}
}

// v3_module_root_inputs returns the global module directories, where a new module
// would be found before one in a directory above the project.
fn v3_module_root_inputs() UnchangedBuildLinkInputs {
	mut result := UnchangedBuildLinkInputs{}
	for root in os.vmodules_paths() {
		if os.is_dir(root) {
			result.dirs << os.real_path(root)
		} else {
			result.missing << root
		}
	}
	return result
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
			result.dirs << os.real_path(dir)
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
		result.files << os.real_path(pc_file)
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
