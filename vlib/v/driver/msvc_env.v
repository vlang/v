module driver

import os
import v.cmdexec
import v.pref

// MSVC's `cl` has no built-in search paths. It finds the C headers through INCLUDE and the
// import libraries through LIB, which a Visual Studio Developer Command Prompt sets. Without
// them the first `#include` of a build fails, with C1034 or C1083. msvc_prepare_environment
// finds the installation the way that prompt does and sets what is missing for this process,
// so every `cl` that V starts afterwards inherits it. Settings that are already there, like
// those of a Developer Command Prompt, are never replaced. They are meant for the C compiler
// only: msvc_restore_environment puts the old values back before the built program can start.

const msvc_vswhere_timeout_ms = 15000
const msvc_registry_timeout_ms = 5000

// msvc_max_version_digits keeps every part of a version number inside an `int`.
const msvc_max_version_digits = 9

// MsvcSavedVariable is an environment variable as it was before msvc_prepare_environment set it.
struct MsvcSavedVariable {
	name    string
	value   string
	was_set bool
}

// MsvcPreparation is what msvc_prepare_environment did.
struct MsvcPreparation {
mut:
	problem string              // what could not be set up, or ''
	saved   []MsvcSavedVariable // the variables it set, with the values they had
}

// MsvcClLocation is where a `cl.exe` sits in a Visual Studio installation,
// `<tools_dir>/bin/Host<host>/<target>/cl.exe`.
struct MsvcClLocation {
	tools_dir string // `<install>/VC/Tools/MSVC/<version>`
	host      string // the architecture `cl` runs on: x64, x86 or arm64
	target    string // the architecture `cl` builds for: x64, x86 or arm64
}

// MsvcWindowsSdk is one version of the Windows SDK: `<root>/Include/<version>` and
// `<root>/Lib/<version>` hold its headers and libraries.
struct MsvcWindowsSdk {
	root    string
	version string
}

// msvc_arch_dir returns how the directories of the MSVC tools and of the Windows SDK name an
// architecture, or '' for one that MSVC does not build for.
fn msvc_arch_dir(arch string) string {
	return match arch.to_lower_ascii() {
		'amd64', 'x64', 'x86_64' { 'x64' }
		'x86', 'i386' { 'x86' }
		'arm64', 'aarch64' { 'arm64' }
		else { '' }
	}
}

// msvc_cl_location splits the path of a `cl.exe` that is laid out like the one of a Visual
// Studio installation. Any other path gives none.
fn msvc_cl_location(cl_path string) ?MsvcClLocation {
	parts := cl_path.replace('\\', '/').split('/')
	n := parts.len
	if n < 6 || parts[n - 1].to_lower_ascii() !in ['cl.exe', 'cl'] || parts[n - 4].to_lower_ascii() != 'bin'
		|| !parts[n - 3].to_lower_ascii().starts_with('host') {
		return none
	}
	host := msvc_arch_dir(parts[n - 3][4..])
	target := msvc_arch_dir(parts[n - 2])
	if host == '' || target == '' {
		return none
	}
	return MsvcClLocation{
		tools_dir: parts[..n - 4].join('/')
		host:      host
		target:    target
	}
}

// msvc_is_all_digits_part reports whether text is 1 to msvc_max_version_digits decimal digits.
fn msvc_is_all_digits_part(text string) bool {
	return text.len > 0 && text.len <= msvc_max_version_digits && text.bytes().all(it.is_digit())
}

// msvc_is_version_name reports whether a directory name is a version number like
// `10.0.26100.0` or `14.51.36231`.
fn msvc_is_version_name(name string) bool {
	parts := name.split('.')
	return parts.len >= 2 && parts.all(msvc_is_all_digits_part(it))
}

// msvc_version_part reads one part of a dotted version number. Anything that
// msvc_is_all_digits_part rejects counts as 0, since `int()` would stop at the first character
// that is not a digit, or saturate.
fn msvc_version_part(text string) int {
	return if msvc_is_all_digits_part(text) { text.int() } else { 0 }
}

// msvc_version_less compares two dotted version numbers numerically, so `10.0.9600.0` is
// older than `10.0.19041.0`.
fn msvc_version_less(a string, b string) bool {
	pa := a.split('.').map(msvc_version_part(it))
	pb := b.split('.').map(msvc_version_part(it))
	n := if pa.len > pb.len { pa.len } else { pb.len }
	for i in 0 .. n {
		x := if i < pa.len { pa[i] } else { 0 }
		y := if i < pb.len { pb[i] } else { 0 }
		if x != y {
			return x < y
		}
	}
	return false
}

// msvc_newest_version_dir returns the highest version-named subdirectory of dir for which
// usable accepts the name, or ''.
fn msvc_newest_version_dir(dir string, usable fn (string) bool) string {
	mut best := ''
	names := os.ls(dir) or { []string{} }
	for name in names {
		if !msvc_is_version_name(name) || !usable(name) {
			continue
		}
		if best == '' || msvc_version_less(best, name) {
			best = name
		}
	}
	return best
}

// msvc_tools_dir_usable reports whether a toolset directory has the headers and the
// libraries for building for arch.
fn msvc_tools_dir_usable(tools_dir string, arch string) bool {
	return os.is_dir(os.join_path(tools_dir, 'include')) && os.is_dir(os.join_path(tools_dir, 'lib', arch))
}

// msvc_tools_dir_of_install returns the MSVC toolset of a Visual Studio installation that can
// build for arch: the default one that its installer names, or else the newest one that is
// complete. It returns '' without any.
fn msvc_tools_dir_of_install(install_dir string, arch string) string {
	msvc_root := os.join_path(install_dir, 'VC', 'Tools', 'MSVC')
	version_file := os.join_path(install_dir, 'VC', 'Auxiliary', 'Build',
		'Microsoft.VCToolsVersion.default.txt')
	default_text := os.read_file(version_file) or { '' }
	default_version := default_text.trim_space()
	if default_version != '' {
		default_dir := os.join_path(msvc_root, default_version)
		if msvc_tools_dir_usable(default_dir, arch) {
			return default_dir
		}
	}
	newest := msvc_newest_version_dir(msvc_root, fn [msvc_root, arch] (name string) bool {
		return msvc_tools_dir_usable(os.join_path(msvc_root, name), arch)
	})
	return if newest == '' { '' } else { os.join_path(msvc_root, newest) }
}

// msvc_vswhere_install_dir asks `vswhere`, which every Visual Studio installer provides, for
// the newest installation that has the C++ tools for arch. It returns '' without one.
fn msvc_vswhere_install_dir(arch string) string {
	component := if arch == 'arm64' {
		'Microsoft.VisualStudio.Component.VC.Tools.ARM64'
	} else {
		'Microsoft.VisualStudio.Component.VC.Tools.x86.x64'
	}
	for variable in ['ProgramFiles(x86)', 'ProgramFiles'] {
		program_files := os.getenv(variable)
		if program_files == '' {
			continue
		}
		vswhere := os.join_path(program_files, 'Microsoft Visual Studio', 'Installer', 'vswhere.exe')
		if !os.is_file(vswhere) {
			continue
		}
		res := cmdexec.run_with_timeout(vswhere, ['-latest', '-products', '*', '-requires', component,
			'-property', 'installationPath'], msvc_vswhere_timeout_ms)
		if res.exit_code != 0 {
			continue
		}
		for line in res.output.split_into_lines() {
			dir := line.trim_space()
			if dir != '' && os.is_dir(dir) {
				return dir
			}
		}
	}
	return ''
}

// msvc_find_tools_dir looks for an MSVC toolset that can build for arch without a `cl` to
// start from: first the one that VCToolsInstallDir names, then the newest installation.
fn msvc_find_tools_dir(arch string) string {
	from_environment := os.getenv('VCToolsInstallDir').trim_right('\\/')
	if from_environment != '' && msvc_tools_dir_usable(from_environment, arch) {
		return from_environment
	}
	install_dir := msvc_vswhere_install_dir(arch)
	if install_dir != '' {
		return msvc_tools_dir_of_install(install_dir, arch)
	}
	return ''
}

// msvc_find_cl_dir returns the directory of the `cl.exe` of a toolset that builds for
// target, preferring one that runs on host. It returns '' when there is none.
fn msvc_find_cl_dir(tools_dir string, host string, target string) string {
	for candidate in [host, 'x64', 'x86', 'arm64'] {
		dir := os.join_path(tools_dir, 'bin', 'Host${candidate}', target)
		if os.is_file(os.join_path(dir, 'cl.exe')) {
			return dir
		}
	}
	return ''
}

// msvc_find_windows_sdk returns the newest Windows SDK under the first of the roots that has
// the C runtime and Windows headers, and the libraries for arch.
fn msvc_find_windows_sdk(roots []string, arch string) ?MsvcWindowsSdk {
	for root in roots {
		if root == '' {
			continue
		}
		version := msvc_newest_version_dir(os.join_path(root, 'Lib'), fn [root, arch] (name string) bool {
			include_dir := os.join_path(root, 'Include', name)
			lib_dir := os.join_path(root, 'Lib', name)
			return os.is_dir(os.join_path(include_dir, 'ucrt')) && os.is_dir(os.join_path(include_dir, 'um'))
				&& os.is_dir(os.join_path(include_dir, 'shared'))
				&& os.is_dir(os.join_path(lib_dir, 'ucrt', arch))
				&& os.is_dir(os.join_path(lib_dir, 'um', arch))
		})
		if version != '' {
			return MsvcWindowsSdk{
				root:    root
				version: version
			}
		}
	}
	return none
}

// msvc_expand_registry_environment replaces defined %NAME% references in a registry path.
// Unknown variables, unmatched percent signs, and percent signs in replacements stay literal.
fn msvc_expand_registry_environment(value string) string {
	mut expanded := []u8{cap: value.len}
	mut i := 0
	for i < value.len {
		if value[i] != `%` {
			expanded << value[i]
			i++
			continue
		}
		offset := value[i + 1..].index_u8(`%`)
		if offset < 0 {
			expanded << value[i..].bytes()
			break
		}
		end := i + 1 + offset
		if replacement := os.getenv_opt(value[i + 1..end]) {
			expanded << replacement.bytes()
		} else {
			expanded << value[i..end + 1].bytes()
		}
		i = end + 1
	}
	return expanded.bytestr()
}

// msvc_parse_registry_value returns a REG_SZ or REG_EXPAND_SZ value in `reg query` output,
// expanding environment references only for REG_EXPAND_SZ, or '' when no value matches.
fn msvc_parse_registry_value(output string, name string) string {
	for line in output.split_into_lines() {
		trimmed := line.trim_space()
		if !trimmed.starts_with(name) {
			continue
		}
		rest := trimmed[name.len..].trim_space()
		kind := rest.all_before(' ').all_before('\t')
		if kind in ['REG_SZ', 'REG_EXPAND_SZ'] {
			value := rest[kind.len..].trim_space()
			return if kind == 'REG_EXPAND_SZ' {
				msvc_expand_registry_environment(value)
			} else {
				value
			}
		}
	}
	return ''
}

// msvc_system_reg_exe returns the `reg.exe` of the Windows system folder, or '' when it is not
// there. It is never looked up on PATH: a `reg.exe` in front of the real one would run instead.
fn msvc_system_reg_exe() string {
	system_root := os.getenv('SystemRoot')
	if system_root == '' {
		return ''
	}
	reg := os.join_path(system_root, 'System32', 'reg.exe')
	return if os.is_file(reg) { reg } else { '' }
}

// msvc_registry_windows_kits_root returns where the installer of the Windows SDK recorded the
// root of its kits (`KitsRoot10`), or '' when it did not.
fn msvc_registry_windows_kits_root() string {
	reg := msvc_system_reg_exe()
	if reg == '' {
		return ''
	}
	res := cmdexec.run_with_timeout(reg, ['query',
		r'HKLM\SOFTWARE\Microsoft\Windows Kits\Installed Roots', '/v', 'KitsRoot10', '/reg:32'],
		msvc_registry_timeout_ms)
	if res.exit_code != 0 {
		return ''
	}
	return msvc_parse_registry_value(res.output, 'KitsRoot10')
}

// msvc_find_system_windows_sdk looks for the Windows SDK where a Developer Command Prompt
// would: WindowsSdkDir, the registry, then the default install location.
fn msvc_find_system_windows_sdk(arch string) ?MsvcWindowsSdk {
	if sdk := msvc_find_windows_sdk([os.getenv('WindowsSdkDir')], arch) {
		return sdk
	}
	if sdk := msvc_find_windows_sdk([msvc_registry_windows_kits_root()], arch) {
		return sdk
	}
	mut defaults := []string{}
	for variable in ['ProgramFiles(x86)', 'ProgramFiles'] {
		program_files := os.getenv(variable)
		if program_files != '' {
			defaults << os.join_path(program_files, 'Windows Kits', '10')
		}
	}
	return msvc_find_windows_sdk(defaults, arch)
}

// msvc_include_value returns the INCLUDE that builds C against the toolset and the SDK.
fn msvc_include_value(tools_dir string, sdk MsvcWindowsSdk) string {
	headers := os.join_path(sdk.root, 'Include', sdk.version)
	return [os.join_path(headers, 'ucrt'), os.join_path(tools_dir, 'include'),
		os.join_path(headers, 'um'), os.join_path(headers, 'shared')].join(os.path_delimiter)
}

// msvc_lib_value returns the LIB that links C against the toolset and the SDK for arch.
fn msvc_lib_value(tools_dir string, sdk MsvcWindowsSdk, arch string) string {
	libs := os.join_path(sdk.root, 'Lib', sdk.version)
	return [os.join_path(tools_dir, 'lib', arch), os.join_path(libs, 'ucrt', arch),
		os.join_path(libs, 'um', arch)].join(os.path_delimiter)
}

// msvc_no_tools_message explains that no Visual Studio C++ tools were found to set up.
fn msvc_no_tools_message(arch string) string {
	return '`-cc msvc` could not find the Visual Studio C++ tools for ${arch} to set up INCLUDE and LIB ' +
		'(it looked next to the `cl` on PATH, at VCToolsInstallDir, and asked `vswhere`). ' +
		'Install the "Desktop development with C++" workload, or run V from a Visual Studio Developer Command Prompt.'
}

// msvc_no_sdk_message explains that no Windows SDK was found to set up.
fn msvc_no_sdk_message(arch string) string {
	return '`-cc msvc` could not find a Windows 10/11 SDK for ${arch} to set up INCLUDE and LIB: ' +
		'it is not in WindowsSdkDir, in the registry (KitsRoot10) or in "Program Files (x86)\\Windows Kits\\10". ' +
		'Install the Windows SDK, or run V from a Visual Studio Developer Command Prompt.'
}

// msvc_set_variable sets an environment variable, and records the value it had in saved.
fn msvc_set_variable(mut saved []MsvcSavedVariable, name string, value string) {
	old := os.getenv_opt(name)
	saved << MsvcSavedVariable{
		name:    name
		value:   old or { '' }
		was_set: old != none
	}
	os.setenv(name, value, true)
}

// msvc_restore_environment puts back the values that msvc_prepare_environment replaced, in the
// opposite order, so a variable that was not set before is not set afterwards either.
fn msvc_restore_environment(saved []MsvcSavedVariable) {
	for i := saved.len - 1; i >= 0; i-- {
		variable := saved[i]
		if variable.was_set {
			os.setenv(variable.name, variable.value, true)
		} else {
			os.unsetenv(variable.name)
		}
	}
}

// msvc_path_with_front returns PATH with dir in front of it, without an empty entry when PATH is
// empty: that would stand for the current folder.
fn msvc_path_with_front(dir string) string {
	path := os.getenv('PATH')
	return if path == '' { dir } else { dir + os.path_delimiter + path }
}

// msvc_prepare_environment gives a `-cc msvc` build what a Developer Command Prompt does. It puts
// a `cl` on PATH when there is none, or when the one there builds for another architecture than
// V generates C for (unless the compiler was named by its path), and it sets INCLUDE and LIB,
// each only when it is missing. The result lists the variables it set, with the values they had
// (see msvc_restore_environment), and what it could not set up. An architecture that MSVC does
// not build for is left to the compiler: nothing is set up then, and nothing is reported.
fn msvc_prepare_environment(c_compiler string, target pref.Target) MsvcPreparation {
	mut prepared := MsvcPreparation{}
	cl_path := os.find_abs_path_of_executable(c_compiler) or { '' }
	need_include := os.getenv('INCLUDE') == ''
	need_lib := os.getenv('LIB') == ''
	target_arch := msvc_arch_dir(target.arch)
	if target_arch == '' {
		return prepared
	}
	// A compiler named by its path is the user's choice, including its target architecture.
	location := msvc_cl_location(cl_path) or { MsvcClLocation{} }
	named_by_path := c_compiler.contains('/') || c_compiler.contains('\\')
	wrong_arch := location.target != '' && location.target != target_arch && !named_by_path
	if cl_path != '' && !need_include && !need_lib && !wrong_arch {
		return prepared
	}
	mut arch := if named_by_path && location.target != '' { location.target } else { target_arch }
	mut tools_dir := location.tools_dir
	if tools_dir == '' || !msvc_tools_dir_usable(tools_dir, arch) {
		tools_dir = msvc_find_tools_dir(arch)
	}
	if tools_dir == '' {
		prepared.problem = msvc_no_tools_message(arch)
		return prepared
	}
	// The `cl` that runs has to build for the architecture that V generates C for. A compiler
	// named by its path is the user's choice, and stays.
	if cl_path == '' || wrong_arch {
		cl_dir := msvc_find_cl_dir(tools_dir, msvc_arch_dir(pref.host_arch()), target_arch)
		if cl_dir != '' && msvc_tools_dir_usable(tools_dir, target_arch) {
			msvc_set_variable(mut prepared.saved, 'PATH', msvc_path_with_front(cl_dir))
			arch = target_arch
		} else {
			prepared.problem = msvc_no_tools_message(target_arch)
			return prepared
		}
	}
	if need_include || need_lib {
		sdk := msvc_find_system_windows_sdk(arch) or {
			prepared.problem = msvc_no_sdk_message(arch)
			return prepared
		}
		if need_include {
			msvc_set_variable(mut prepared.saved, 'INCLUDE', msvc_include_value(tools_dir, sdk))
		}
		if need_lib {
			msvc_set_variable(mut prepared.saved, 'LIB', msvc_lib_value(tools_dir, sdk, arch))
		}
	}
	return prepared
}
