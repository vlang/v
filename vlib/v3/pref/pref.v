module pref

import os
import time
import v3.cmdexec

// Preferences represents preferences data used by pref.
pub struct Preferences {
pub mut:
	verbose      bool
	output_file  string
	target       Target = host_target()
	user_defines []string
	backend      string = 'c'
	c99          bool
	vroot        string = detect_vroot()
	vexe         string = detect_vexe()
	selfhost     bool
	building_v   bool // compiling the V compiler itself: no generics, skip monomorphization
	is_prod      bool
	is_test      bool // at least one compatible user test file is being compiled
	// V3 backends currently do not lower V inline-assembly nodes. Keep this an
	// explicit capability so guarded stdlib assembly selects its software path.
	supports_inline_asm bool
pub:
	build_date      string
	build_time      string
	build_timestamp string
}

// Target is the canonical description of the platform for which code is generated.
// Host properties must not be used for target-dependent source selection or semantics.
pub struct Target {
pub:
	os            string
	arch          string
	abi           string
	endian        string
	pointer_bits  int
	object_format string
}

// host_arch returns the normalized architecture of the compiler process.
pub fn host_arch() string {
	$if arm64 {
		return 'arm64'
	} $else $if amd64 {
		return 'amd64'
	} $else $if arm32 {
		return 'arm32'
	} $else $if rv64 {
		return 'riscv64'
	} $else $if s390x {
		return 's390x'
	} $else $if ppc64le {
		return 'ppc64le'
	} $else $if ppc64 {
		return 'ppc64'
	} $else $if loongarch64 {
		return 'loongarch64'
	} $else $if wasm32 {
		return 'wasm32'
	} $else $if i386 {
		return 'x86'
	} $else $if x32 {
		return 'x86'
	} $else {
		return 'unknown'
	}
}

// host_target returns the platform on which the compiler process is running.
pub fn host_target() Target {
	return target_from(os.user_os(), host_arch()) or {
		panic('unsupported compiler host target ${os.user_os()}/${host_arch()}')
	}
}

// target_from validates and canonicalizes an OS/architecture pair.
pub fn target_from(os_name string, arch_name string) !Target {
	target_os := normalized_os(os_name.trim_space().to_lower())
	target_arch := normalized_arch(arch_name.trim_space().to_lower())
	if target_os !in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
		'android', 'termux', 'ios', 'solaris', 'qnx', 'haiku', 'serenity', 'vinix',
		'wasm32_emscripten'] {
		return error('unsupported target OS `${os_name}`')
	}
	if target_arch !in ['amd64', 'arm64', 'x86', 'arm32', 'riscv64', 'ppc64', 'ppc64le', 's390x',
		'loongarch64', 'wasm32'] {
		return error('unsupported target architecture `${arch_name}`')
	}
	if target_os == 'wasm32_emscripten' && target_arch != 'wasm32' {
		return error('target OS `wasm32_emscripten` requires architecture `wasm32`')
	}
	endian := if target_arch in ['ppc64', 's390x'] { 'big' } else { 'little' }
	pointer_bits := if target_arch in ['x86', 'arm32', 'wasm32'] { 32 } else { 64 }
	abi := match target_os {
		'windows' { 'windows' }
		'macos', 'ios' { 'darwin' }
		'android', 'termux' { 'android' }
		'wasm32_emscripten' { 'emscripten' }
		else { 'gnu' }
	}

	object_format := match target_os {
		'windows' { 'coff' }
		'macos', 'ios' { 'macho' }
		'wasm32_emscripten' { 'wasm' }
		else { 'elf' }
	}

	return Target{
		os:            target_os
		arch:          target_arch
		abi:           abi
		endian:        endian
		pointer_bits:  pointer_bits
		object_format: object_format
	}
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
	if relative_path := module_path_from_search_root(mod, mod_path, importer_dir) {
		return relative_path
	}
	// 2. local modules/ directory beside the importing file
	local_modules_root := os.join_path_single(importer_dir, 'modules')
	if local_modules_path := module_path_from_search_root(mod, mod_path, local_modules_root) {
		return local_modules_path
	}
	// 3. vlib
	vlib_root := os.join_path_single(p.vroot, 'vlib')
	if vlib_path := module_path_from_search_root(mod, mod_path, vlib_root) {
		return vlib_path
	}
	// 4. ~/.vmodules (or $VMODULES)
	if vmodules_path := module_path_from_search_root(mod, mod_path, vmodules_dir()) {
		return vmodules_path
	}
	// 5. walk up the parent directories of the importing file, like V1's
	// Builder.find_module_path. This finds sibling projects: e.g. importing
	// `viper` from ~/code/doka/doka.v resolves to ~/code/viper.
	mut current_dir := importer_dir
	for {
		if try_path := module_path_from_search_root(mod, mod_path, current_dir) {
			return try_path
		}
		modules_root := os.join_path_single(current_dir, 'modules')
		if try_modules_path := module_path_from_search_root(mod, mod_path, modules_root) {
			return try_modules_path
		}
		parent_dir := os.dir(current_dir)
		if parent_dir == current_dir {
			break
		}
		current_dir = parent_dir
	}
	return ''
}

fn module_path_from_search_root(mod string, mod_path string, search_root string) ?string {
	if alias_path := resolve_module_alias_path(search_root, mod) {
		return alias_path
	}
	path := os.join_path_single(search_root, mod_path)
	if dir_is_module(path) {
		return path
	}
	return none
}

// resolve_module_alias_path resolves `@[alias: 'path'] module name` declarations
// stored in `alias.v`. An alias applies to submodules as well.
fn resolve_module_alias_path(search_root string, mod string) ?string {
	parts := mod.split('.')
	for part_count := parts.len; part_count > 0; part_count-- {
		alias_dir := os.join_path_single(search_root, parts[..part_count].join(os.path_separator))
		alias_file := os.join_path_single(alias_dir, 'alias.v')
		if !os.is_file(alias_file) {
			continue
		}
		source := os.read_file(alias_file) or { continue }
		mut target_dir := module_alias_target_from_source(source) or { continue }
		if target_dir.contains('@VMODROOT') {
			vmod_root := vmod_root_for_dir(alias_dir) or { continue }
			target_dir = target_dir.replace('@VMODROOT', vmod_root)
		}
		if !os.is_abs_path(target_dir) {
			target_dir = os.join_path_single(alias_dir, target_dir)
		}
		if part_count < parts.len {
			target_dir = os.join_path_single(target_dir,
				parts[part_count..].join(os.path_separator))
		}
		target_dir = os.real_path(target_dir)
		if dir_is_module(target_dir) {
			return target_dir
		}
	}
	return none
}

fn module_alias_target_from_source(source string) ?string {
	marker := '@[alias'
	marker_pos := source.index(marker) or { return none }
	mut rest := source[marker_pos + marker.len..].trim_space()
	if !rest.starts_with(':') {
		return none
	}
	rest = rest[1..].trim_space()
	if rest.len < 2 || rest[0] !in [`'`, `"`] {
		return none
	}
	quote := rest[0]
	mut end := 1
	for end < rest.len && rest[end] != quote {
		end++
	}
	if end >= rest.len {
		return none
	}
	target := rest[1..end]
	if target.len == 0 {
		return none
	}
	rest = rest[end + 1..].trim_space()
	if !rest.starts_with(']') {
		return none
	}
	rest = rest[1..].trim_space()
	if !rest.starts_with('module ') {
		return none
	}
	return target
}

fn vmod_root_for_dir(start string) ?string {
	mut dir := os.real_path(start)
	for {
		if os.is_file(os.join_path_single(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			return none
		}
		dir = parent
	}
	return none
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
	return file_has_incompatible_target_suffix(file, target_from(current_os, host_arch()) or {
		host_target()
	})
}

// file_has_incompatible_target_suffix reports whether an OS or architecture suffix excludes
// file from target.
pub fn file_has_incompatible_target_suffix(file string, target Target) bool {
	if file_has_incompatible_os_only_suffix(file, target.os) {
		return true
	}
	for arch in ['amd64', 'x64', 'x86_64', 'arm64', 'aarch64', 'x86', 'i386', 'i486', 'i586', 'i686',
		'x32', 'x86_32', 'ia-32', 'ia32', 'arm32', 'rv64', 'riscv64', 'ppc64', 'ppc64le', 's390x',
		'loongarch64', 'wasm32'] {
		if normalized_arch(arch) != target.arch
			&& (file.contains('.${arch}.') || file.contains('_${arch}.')) {
			return true
		}
	}
	return false
}

fn file_has_incompatible_os_only_suffix(file string, current_os string) bool {
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
	if file.contains('_android_outside_termux.') {
		if os_name != 'android' {
			return true
		}
	} else if file.contains('_termux.') {
		if os_name != 'termux' {
			return true
		}
	} else if file.contains('_android.') && os_name !in ['android', 'termux'] {
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
	for target_os in ['qnx', 'haiku', 'serenity', 'vinix'] {
		if os_name != target_os && file.contains('_${target_os}.') {
			return true
		}
	}
	if os_name != 'wasm32_emscripten' && file.contains('_wasm32_emscripten.') {
		return true
	}
	return false
}

// get_v_files_from_dir returns get v files from dir data for pref.
pub fn get_v_files_from_dir(dir string, user_defines []string, target_os string) []string {
	return get_v_files_from_dir_for_target(dir, user_defines, target_from(target_os, host_arch()) or {
		host_target()
	})
}

// get_v_files_from_dir_for_target returns sources compatible with the complete target.
pub fn get_v_files_from_dir_for_target(dir string, user_defines []string, target Target) []string {
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
		if file_has_incompatible_target_suffix(file, target) {
			continue
		}
		if base := os_specific_base(file, target.os) {
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
			if file_has_incompatible_target_suffix(file, target) {
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
	return get_test_v_files_from_dir_for_target(dir, user_defines, backend, target_from(target_os,
		host_arch()) or { host_target() })
}

// get_test_v_files_from_dir_for_target returns tests compatible with the complete target.
pub fn get_test_v_files_from_dir_for_target(dir string, user_defines []string, backend string, target Target) []string {
	if dir == '' || !os.is_dir(dir) {
		return []string{}
	}
	mut files := os.ls(dir) or { return []string{} }
	files.sort()
	mut result := []string{}
	for file in files {
		path := os.join_path_single(dir, file)
		if !is_test_file_for_platform(path, backend, target) {
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
	return is_test_file_for_platform(path, backend, target_from(target_os, host_arch()) or {
		host_target()
	})
}

// is_test_file_for_platform reports whether path is a test for backend and target.
pub fn is_test_file_for_platform(path string, backend string, target Target) bool {
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
	return !file_has_incompatible_target_suffix(probe, target)
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
			suffixes << '_android_outside_termux'
			suffixes << '_android'
		}
		'termux' {
			suffixes << '_termux'
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
		'qnx', 'haiku', 'serenity', 'vinix' {
			suffixes << '_${os_name}'
		}
		'wasm32_emscripten' {
			suffixes << '_wasm32_emscripten'
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
		'win32' { 'windows' }
		'emscripten' { 'wasm32_emscripten' }
		else { target_os }
	}
}

// normalized_arch canonicalizes common architecture aliases.
pub fn normalized_arch(target_arch string) string {
	return match target_arch {
		'x64', 'x86_64' { 'amd64' }
		'aarch64' { 'arm64' }
		'i386', 'i486', 'i586', 'i686', 'x32', 'x86_32', 'ia-32', 'ia32' { 'x86' }
		'aarch32', 'arm', 'armv7', 'armv7l' { 'arm32' }
		'rv64', 'risc-v64', 'riscv', 'risc-v' { 'riscv64' }
		'wasm' { 'wasm32' }
		else { target_arch }
	}
}

// normalized_target_os supports normalized target os handling for Preferences.
pub fn (p &Preferences) normalized_target_os() string {
	return p.target.os
}

// normalized_target_arch returns the canonical target architecture.
pub fn (p &Preferences) normalized_target_arch() string {
	return p.target.arch
}

// is_cross_target reports whether is cross target applies in pref.
pub fn (p &Preferences) is_cross_target() bool {
	host := host_target()
	return p.target.os != host.os || p.target.arch != host.arch
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
		'termux' {
			return p.normalized_target_os() == 'termux'
		}
		'qnx', 'haiku', 'serenity', 'vinix' {
			return p.normalized_target_os() == name
		}
		'wasm32_emscripten' {
			return p.normalized_target_os() == 'wasm32_emscripten'
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
			return p.target.pointer_bits == 64
		}
		'x32' {
			return p.target.pointer_bits == 32
		}
		'amd64' {
			return p.target.arch == 'amd64'
		}
		'arm64', 'aarch64' {
			return p.target.arch == 'arm64'
		}
		'arm32' {
			return p.target.arch == 'arm32'
		}
		'i386', 'x86' {
			return p.target.arch == 'x86'
		}
		'rv64', 'riscv64' {
			return p.target.arch == 'riscv64'
		}
		's390x', 'ppc64', 'ppc64le', 'loongarch64', 'wasm32' {
			return p.target.arch == name
		}
		'little_endian' {
			return p.target.endian == 'little'
		}
		'big_endian' {
			return p.target.endian == 'big'
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
	packages := cmdexec.split_args(name) or { return false }
	if packages.len == 0 {
		return false
	}
	mut args := ['--exists']
	args << packages
	result := cmdexec.run('pkg-config', args)
	return result.exit_code == 0
}
