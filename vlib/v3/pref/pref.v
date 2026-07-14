module pref

import os
import time

// Preferences represents preferences data used by pref.
pub struct Preferences {
pub mut:
	verbose      bool
	output_file  string
	target_os    string = os.user_os()
	user_defines []string
	backend      string = 'c'
	c99          bool
	vroot        string = detect_vroot()
	vexe         string = detect_vexe()
	selfhost     bool
	building_v   bool // compiling the V compiler itself: no generics, skip monomorphization
	is_prod      bool
	is_test      bool // at least one compatible user test file is being compiled
pub:
	build_date      string
	build_time      string
	build_timestamp string
}

// new_preferences supports new preferences handling for pref.
pub fn new_preferences() &Preferences {
	build_time := target_build_time()
	return &Preferences{
		build_date:      build_time.strftime('%Y-%m-%d')
		build_time:      build_time.strftime('%H:%M:%S')
		build_timestamp: build_time.unix().str()
	}
}

fn target_build_time() time.Time {
	source_date_epoch := os.getenv('SOURCE_DATE_EPOCH')
	if source_date_epoch.len == 0 {
		return time.utc()
	}
	return time.unix_nanosecond(source_date_epoch.i64(), 0)
}

// detect_vroot resolves detect vroot information for pref.
fn detect_vroot() string {
	baked_root := @VMODROOT
	if baked_root.len > 0 {
		return baked_root
	}
	if os.args.len > 0 && os.args[0].len > 0 {
		vroot := detect_vroot_from(os.args[0])
		if vroot.len > 0 {
			return vroot
		}
	}
	return detect_vroot_from(os.getwd())
}

fn detect_vexe() string {
	env_vexe := os.getenv('VEXE')
	if env_vexe.len > 0 {
		return os.real_path(env_vexe)
	}
	exe := os.executable()
	if exe.len > 0 {
		return os.real_path(exe)
	}
	if os.args.len > 0 && os.args[0].len > 0 {
		return os.real_path(os.args[0])
	}
	vroot := detect_vroot()
	if vroot.len > 0 {
		return os.join_path_single(vroot, 'v')
	}
	return ''
}

// detect_vroot_from resolves detect vroot from information for pref.
fn detect_vroot_from(start string) string {
	if start.len == 0 {
		return ''
	}
	mut dir := start
	if !os.is_abs_path(dir) {
		cwd := os.getwd()
		if cwd.len > 0 {
			dir = os.join_path_single(cwd, dir)
		}
	}
	if !os.is_dir(dir) {
		dir = os.dir(dir)
	}
	for _ in 0 .. 8 {
		if os.is_dir(os.join_path_single(os.join_path_single(dir, 'vlib'), 'builtin')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ''
}

// get_vlib_module_path returns get vlib module path data for Preferences.
pub fn (p &Preferences) get_vlib_module_path(mod string) string {
	mod_path := vlib_module_path(mod)
	return os.join_path_single(os.join_path_single(p.vroot, 'vlib'), mod_path)
}

// get_module_path returns get module path data for Preferences.
pub fn (p &Preferences) get_module_path(mod string, importing_file_path string) string {
	mod_path := mod.replace('.', os.path_separator)
	// Absolutize the importer like V1's Builder.find_module_path does
	// (`os.real_path(fpath)`). When the input is given as a relative path (e.g.
	// `v3 -o out .`), the parsed file paths are relative too (`./doka.v`), and a
	// parent-directory walk starting from `.` could never climb above the project
	// dir to find sibling modules.
	abs_importer := os.real_path(importing_file_path)
	importer_dir := os.dir(abs_importer)
	// 1. relative to the importing file's directory
	relative_path := os.join_path_single(importer_dir, mod_path)
	if dir_is_module(relative_path) {
		return relative_path
	}
	// 2. vlib
	vlib_path := os.join_path_single(os.join_path_single(p.vroot, 'vlib'), mod_path)
	if dir_is_module(vlib_path) {
		return vlib_path
	}
	// 3. ~/.vmodules (or $VMODULES)
	vmodules_path := os.join_path_single(vmodules_dir(), mod_path)
	if dir_is_module(vmodules_path) {
		return vmodules_path
	}
	// 4. walk up the parent directories of the importing file, like V1's
	// Builder.find_module_path. This finds sibling projects: e.g. importing
	// `viper` from ~/code/doka/doka.v resolves to ~/code/viper.
	mut current_dir := importer_dir
	for {
		try_path := os.join_path_single(current_dir, mod_path)
		if dir_is_module(try_path) {
			return try_path
		}
		parent_dir := os.dir(current_dir)
		if parent_dir == current_dir {
			break
		}
		current_dir = parent_dir
	}
	return ''
}

// vlib_module_path maps an import name to its directory below vlib.
fn vlib_module_path(mod string) string {
	return mod.replace('.', os.path_separator)
}

// dir_is_module reports whether `dir` is a usable module directory: it must exist
// and contain at least one `.v` source file. V1 applies the same `.v`-files guard
// (`module_path_has_v_files`); without it the parent-directory walk could match an
// unrelated directory that merely shares a module's name (e.g. `~/code/util`),
// shadowing the real module.
fn dir_is_module(dir string) bool {
	if dir.len == 0 || !os.is_dir(dir) {
		return false
	}
	entries := os.ls(dir) or { return false }
	for entry in entries {
		if entry.ends_with('.v') {
			return true
		}
	}
	return false
}

// vmodules_dir returns the user's global modules directory ($VMODULES or ~/.vmodules).
fn vmodules_dir() string {
	env_dir := os.getenv('VMODULES')
	if env_dir.len > 0 {
		return env_dir
	}
	return os.join_path_single(os.home_dir(), '.vmodules')
}

// file_has_incompatible_os_suffix converts file has incompatible os suffix data for pref.
pub fn file_has_incompatible_os_suffix(file string, current_os string) bool {
	os_name := normalized_os(current_os)
	if os_name == 'windows' && file.contains('_nix.') {
		return true
	}
	if os_name != 'windows' && file.contains('_windows.') {
		return true
	}
	if os_name != 'linux' && file.contains('_linux.') {
		return true
	}
	if os_name != 'macos' && (file.contains('_macos.') || file.contains('_darwin.')) {
		return true
	}
	if os_name != 'macos' && os_name != 'freebsd' && os_name != 'openbsd' && os_name != 'netbsd'
		&& os_name != 'dragonfly' && file.contains('_bsd.') {
		return true
	}
	if os_name != 'android' && file.contains('_android') {
		return true
	}
	if os_name != 'ios' && file.contains('_ios.') {
		return true
	}
	if os_name != 'freebsd' && file.contains('_freebsd.') {
		return true
	}
	if os_name != 'openbsd' && file.contains('_openbsd.') {
		return true
	}
	if os_name != 'netbsd' && file.contains('_netbsd.') {
		return true
	}
	if os_name != 'dragonfly' && file.contains('_dragonfly.') {
		return true
	}
	if os_name != 'solaris' && file.contains('_solaris.') {
		return true
	}
	if file.contains('.amd64.') || file.contains('_amd64.') || file.contains('.arm64.')
		|| file.contains('_arm64.') {
		return true
	}
	return false
}

// get_v_files_from_dir returns get v files from dir data for pref.
pub fn get_v_files_from_dir(dir string, user_defines []string, target_os string) []string {
	if dir == '' || !os.is_dir(dir) {
		return []string{}
	}
	all_files := os.ls(dir) or { return []string{} }
	mut sorted_files := all_files.clone()
	sorted_files.sort()
	mut has_os_specific := map[string]bool{}
	for file in sorted_files {
		if !file.ends_with('.v') || file.ends_with('.js.v')
			|| (file.contains('_test.') && !file.contains('_d_test.')
			&& !file.contains('_notd_test.')) {
			continue
		}
		if file_has_incompatible_os_suffix(file, target_os) {
			continue
		}
		if base := os_specific_base(file, target_os) {
			has_os_specific[base] = true
		}
	}
	mut v_files := []string{}
	for backend_specific in [false, true] {
		for file in sorted_files {
			if file.ends_with('.c.v') != backend_specific {
				continue
			}
			if !file.ends_with('.v') || file.ends_with('.js.v')
				|| (file.contains('_test.') && !file.contains('_d_test.')
				&& !file.contains('_notd_test.')) {
				continue
			}
			if file_has_incompatible_os_suffix(file, target_os) {
				continue
			}
			if base := default_file_base(file) {
				if has_os_specific[base] {
					continue
				}
			}
			if file.contains('_notd_') {
				feature := extract_define_feature(file, '_notd_')
				if feature.len > 0 && feature in user_defines {
					continue
				}
			} else if file.contains('_d_') {
				feature := extract_define_feature(file, '_d_')
				if feature.len == 0 || feature !in user_defines {
					continue
				}
			}
			v_files << os.join_path_single(dir, file)
		}
	}
	return v_files
}

// get_test_v_files_from_dir returns backend/target/define-compatible test files in dir.
pub fn get_test_v_files_from_dir(dir string, user_defines []string, backend string, target_os string) []string {
	if dir == '' || !os.is_dir(dir) {
		return []string{}
	}
	mut files := os.ls(dir) or { return []string{} }
	files.sort()
	mut result := []string{}
	for file in files {
		path := os.join_path_single(dir, file)
		if !is_test_file_for_target(path, backend, target_os) {
			continue
		}
		if file.contains('_notd_') {
			feature := extract_test_define_feature(file, '_notd_')
			if feature.len > 0 && feature in user_defines {
				continue
			}
		} else if file.contains('_d_') {
			feature := extract_test_define_feature(file, '_d_')
			if feature.len == 0 || feature !in user_defines {
				continue
			}
		}
		result << path
	}
	return result
}

fn extract_test_define_feature(file string, marker string) string {
	idx := file.index(marker) or { return '' }
	rest := file[idx + marker.len..]
	test_idx := rest.last_index('_test') or { return '' }
	return rest[..test_idx]
}

pub fn is_test_file_for_backend(path string, backend string) bool {
	file := os.file_name(path)
	if file.contains('_d_test.') || file.contains('_notd_test.') {
		return false
	}
	if file.ends_with('_test.v') {
		return true
	}
	if file.ends_with('_test.c.v') {
		return backend == 'c'
	}
	if file.ends_with('_test.js.v') {
		return backend == 'js'
	}
	if !file.ends_with('.v') {
		return false
	}
	base := file[..file.len - 2]
	if !base.contains('.') {
		return false
	}
	backend_suffix := base.all_after_last('.')
	test_base := base.all_before_last('.')
	if !test_base.ends_with('_test') {
		return false
	}
	return backend_suffix == backend
}

// is_test_file_for_target reports whether path is a test file for backend that is compatible
// with target_os. Platform-qualified tests use names such as `foo_windows_test.v` and must not
// be added to a non-Windows test harness.
pub fn is_test_file_for_target(path string, backend string, target_os string) bool {
	if !is_test_file_for_backend(path, backend) {
		return false
	}
	file := os.file_name(path)
	mut probe := file
	for marker in ['_test.c.v', '_test.js.v', '_test.v'] {
		if file.ends_with(marker) {
			probe = file[..file.len - marker.len] + marker.all_after_first('_test')
			break
		}
	}
	// Generic backend-suffixed tests (`foo_test.arm64.v`) are not covered by the fixed
	// markers above; strip `_test.<backend>` so the os-suffix probe sees only the base
	// and does not misread the backend as an incompatible os/arch suffix.
	if probe == file && file.ends_with('.v') {
		base := file[..file.len - 2]
		if base.contains('.') {
			test_base := base.all_before_last('.')
			if test_base.ends_with('_test') {
				probe = test_base.all_before_last('_test') + '.v'
			}
		}
	}
	return !file_has_incompatible_os_suffix(probe, target_os)
}

// default_file_base supports default file base handling for pref.
fn default_file_base(file string) ?string {
	for marker in ['_default.c.v', '_default.v'] {
		if file.ends_with(marker) {
			return file[..file.len - marker.len]
		}
	}
	return none
}

// os_specific_base supports os specific base handling for pref.
fn os_specific_base(file string, target_os string) ?string {
	if _ := default_file_base(file) {
		return none
	}
	mut suffixes := []string{}
	os_name := normalized_os(target_os)
	if os_name != 'windows' {
		suffixes << '_nix'
	}
	match os_name {
		'windows' {
			suffixes << '_windows'
		}
		'macos' {
			suffixes << '_macos'
			suffixes << '_darwin'
		}
		'linux' {
			suffixes << '_linux'
		}
		'android' {
			suffixes << '_android'
		}
		'ios' {
			suffixes << '_ios'
		}
		'freebsd' {
			suffixes << '_freebsd'
			suffixes << '_bsd'
		}
		'openbsd' {
			suffixes << '_openbsd'
			suffixes << '_bsd'
		}
		'netbsd' {
			suffixes << '_netbsd'
			suffixes << '_bsd'
		}
		'dragonfly' {
			suffixes << '_dragonfly'
			suffixes << '_bsd'
		}
		'solaris' {
			suffixes << '_solaris'
		}
		else {}
	}

	for suffix in suffixes {
		for ext in ['.c.v', '.v'] {
			marker := suffix + ext
			if file.ends_with(marker) {
				return file[..file.len - marker.len]
			}
		}
	}
	return none
}

// extract_define_feature supports extract define feature handling for pref.
fn extract_define_feature(file string, marker string) string {
	idx := file.index(marker) or { return '' }
	rest := file[idx + marker.len..]
	if rest.ends_with('.c.v') {
		return rest[..rest.len - 4]
	}
	if rest.ends_with('.v') {
		return rest[..rest.len - 2]
	}
	return rest
}

// normalized_os supports normalized os handling for pref.
pub fn normalized_os(target_os string) string {
	return match target_os {
		'darwin' { 'macos' }
		'mac' { 'macos' }
		else { target_os }
	}
}

// normalized_target_os supports normalized target os handling for Preferences.
pub fn (p &Preferences) normalized_target_os() string {
	return normalized_os(p.target_os)
}

// is_cross_target reports whether is cross target applies in pref.
pub fn (p &Preferences) is_cross_target() bool {
	return p.normalized_target_os() != normalized_os(os.user_os())
}

// comptime_flag_value supports comptime flag value handling for pref.
pub fn comptime_flag_value(p &Preferences, name string) bool {
	match name {
		'macos', 'darwin', 'mac' {
			return p.normalized_target_os() == 'macos'
		}
		'linux' {
			return p.normalized_target_os() == 'linux'
		}
		'windows' {
			return p.normalized_target_os() == 'windows'
		}
		'freebsd' {
			return p.normalized_target_os() == 'freebsd'
		}
		'openbsd' {
			return p.normalized_target_os() == 'openbsd'
		}
		'netbsd' {
			return p.normalized_target_os() == 'netbsd'
		}
		'dragonfly' {
			return p.normalized_target_os() == 'dragonfly'
		}
		'android' {
			return p.normalized_target_os() == 'android'
		}
		'posix', 'unix' {
			return p.normalized_target_os() != 'windows'
		}
		'bsd' {
			tos := p.normalized_target_os()
			return tos == 'macos' || tos == 'freebsd' || tos == 'openbsd' || tos == 'netbsd'
				|| tos == 'dragonfly'
		}
		'x64' {
			$if x64 {
				return true
			}
			return false
		}
		'x32' {
			$if x32 {
				return true
			}
			return false
		}
		'amd64' {
			$if amd64 {
				return true
			}
			return false
		}
		'arm64', 'aarch64' {
			$if arm64 {
				return true
			}
			return false
		}
		'little_endian' {
			$if little_endian {
				return true
			}
			return false
		}
		'big_endian' {
			$if big_endian {
				return true
			}
			return false
		}
		'debug' {
			$if debug {
				return true
			}
			return false
		}
		'test' {
			return p.is_test
		}
		'native' {
			return p.backend == 'arm64'
		}
		'builtin_write_buf_to_fd_should_use_c_write' {
			return p.backend == 'arm64'
		}
		'tinyc' {
			return p.backend == 'arm64'
		}
		'no_backtrace' {
			return p.backend == 'arm64' || name in p.user_defines
		}
		'gcboehm', 'gcboehm_opt', 'prealloc', 'autofree', 'no_bounds_checking', 'freestanding',
		'nofloat' {
			return name in p.user_defines
		}
		else {
			return name in p.user_defines
		}
	}
}

// comptime_optional_flag_value supports comptime optional flag value handling for pref.
pub fn comptime_optional_flag_value(p &Preferences, name string) bool {
	return name in p.user_defines
}

// comptime_pkgconfig_value supports comptime pkgconfig value handling for pref.
pub fn comptime_pkgconfig_value(name string) bool {
	result := os.execute('pkg-config --exists ${name}')
	return result.exit_code == 0
}
