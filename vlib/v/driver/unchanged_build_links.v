module driver

import os
import v.cmdexec

// UnchangedBuildLinkInputs are the files outside the program that its link reads,
// found as the linker and pkg-config find them, for the receipt of an unchanged
// build (see v3_unchanged_build_prepare). `ok` is false when one of them cannot
// be found and nothing else stands for it: then the build records no receipt.
struct UnchangedBuildLinkInputs {
mut:
	files []string
	dirs  []string
	ok    bool = true
}

// v3_link_library_files resolves the libraries (-lname, -l:file) and macOS
// frameworks (-framework X) that `flags` name to the files the linker reads:
// the -L and -F directories first, then the linker's own search path, as Go's
// cmd/link records the content of each host library it links. A macOS library
// that has no file, such as one served from the dyld shared cache, is keyed by
// the version of the system instead.
fn v3_link_library_files(flags []string, target_os string) UnchangedBuildLinkInputs {
	mut lib_dirs := []string{}
	mut framework_dirs := []string{}
	mut libraries := []string{}
	mut frameworks := []string{}
	mut i := 0
	for i < flags.len {
		flag := flags[i].trim_space()
		next := if i + 1 < flags.len { flags[i + 1].trim_space() } else { '' }
		if flag == '-L' && next.len > 0 {
			lib_dirs << next
			i += 2
			continue
		} else if flag.starts_with('-L') && flag.len > 2 {
			lib_dirs << flag[2..]
		} else if flag == '-l' && next.len > 0 {
			libraries << next
			i += 2
			continue
		} else if flag.starts_with('-l') && flag.len > 2 {
			libraries << flag[2..]
		} else if flag == '-F' && next.len > 0 {
			framework_dirs << next
			i += 2
			continue
		} else if flag.starts_with('-F') && flag.len > 2 {
			framework_dirs << flag[2..]
		} else if flag == '-framework' && next.len > 0 {
			frameworks << next
			i += 2
			continue
		}
		i++
	}
	mut result := UnchangedBuildLinkInputs{}
	if libraries.len == 0 && frameworks.len == 0 {
		return result
	}
	mut extensions := ['.so', '.a']
	if target_os == 'macos' {
		sdk := cmdexec.macos_sdk_root()
		lib_dirs << [os.join_path(sdk, 'usr', 'lib'), '/usr/local/lib']
		framework_dirs << [os.join_path(sdk, 'System', 'Library', 'Frameworks'),
			'/Library/Frameworks']
		extensions = ['.tbd', '.dylib', '.so', '.a']
	} else {
		lib_dirs << v3_default_library_dirs()
	}
	mut unresolved := false
	for name in libraries {
		if file := v3_find_library(name, lib_dirs, extensions) {
			result.files << file
		} else {
			unresolved = true
		}
	}
	for name in frameworks {
		if file := v3_find_framework(name, framework_dirs) {
			result.files << file
		} else {
			unresolved = true
		}
	}
	if unresolved {
		system_version := '/System/Library/CoreServices/SystemVersion.plist'
		if target_os == 'macos' && os.is_file(system_version) {
			result.files << system_version
		} else {
			result.ok = false
		}
	}
	return result
}

fn v3_find_library(name string, dirs []string, extensions []string) ?string {
	for dir in dirs {
		if name.starts_with(':') {
			path := os.join_path_single(dir, name[1..])
			if os.is_file(path) {
				return os.real_path(path)
			}
			continue
		}
		for extension in extensions {
			path := os.join_path_single(dir, 'lib${name}${extension}')
			if os.is_file(path) {
				return os.real_path(path)
			}
		}
	}
	return none
}

fn v3_find_framework(name string, dirs []string) ?string {
	for dir in dirs {
		framework := os.join_path_single(dir, '${name}.framework')
		for candidate in [os.join_path_single(framework, '${name}.tbd'),
			os.join_path_single(framework, name)] {
			if os.is_file(candidate) {
				return os.real_path(candidate)
			}
		}
	}
	return none
}

// v3_default_library_dirs returns the directories the system C compiler's
// linker searches for libraries, as `cc -print-search-dirs` reports them.
fn v3_default_library_dirs() []string {
	result := cmdexec.run('cc', ['-print-search-dirs'])
	if result.exit_code == 0 {
		for line in result.output.split_into_lines() {
			if line.starts_with('libraries:') {
				return line.all_after('=').split(':').filter(it.len > 0)
			}
		}
	}
	return ['/usr/local/lib', '/usr/lib', '/lib', '/usr/lib64', '/lib64']
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
			return result
		}
		result.files << os.real_path(pc_file)
		for option in ['--print-requires', '--print-requires-private'] {
			requires := cmdexec.run('pkg-config', [option, name])
			if requires.exit_code != 0 {
				result.ok = false
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
