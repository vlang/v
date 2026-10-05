module driver

import os
import v.pref

fn msvc_test_dirs(paths ...string) {
	for path in paths {
		os.mkdir_all(path) or { panic(err) }
	}
}

fn msvc_test_touch(path string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, '') or { panic(err) }
}

// msvc_test_install lays out a toolset like the one of a Visual Studio installation and returns
// its directory.
fn msvc_test_install(root string, version string) string {
	tools := os.join_path(root, 'VC', 'Tools', 'MSVC', version)
	msvc_test_dirs(os.join_path(tools, 'include'), os.join_path(tools, 'lib', 'x64'),
		os.join_path(tools, 'lib', 'x86'))
	msvc_test_touch(os.join_path(tools, 'bin', 'Hostx64', 'x64', 'cl.exe'))
	msvc_test_touch(os.join_path(tools, 'bin', 'Hostx64', 'x86', 'cl.exe'))
	return tools
}

// msvc_test_sdk lays out one version of the Windows SDK with the libraries for arches.
fn msvc_test_sdk(root string, version string, arches []string) {
	for part in ['ucrt', 'um', 'shared'] {
		msvc_test_dirs(os.join_path(root, 'Include', version, part))
	}
	for arch in arches {
		msvc_test_dirs(os.join_path(root, 'Lib', version, 'ucrt', arch), os.join_path(root, 'Lib',
			version, 'um', arch))
	}
}

fn msvc_test_root(name string) string {
	// The real path of the temp folder, since `cl` paths are made absolute (macOS has /var -> /private/var).
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_msvc_env_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// MsvcTestEnvironment remembers the environment variables a test changes, and puts them back.
struct MsvcTestEnvironment {
mut:
	names  []string
	values []string
	was    []bool
}

fn (mut e MsvcTestEnvironment) set(name string, value string) {
	old := os.getenv_opt(name)
	e.names << name
	e.values << (old or { '' })
	e.was << (old != none)
	if value == '' {
		os.unsetenv(name)
	} else {
		os.setenv(name, value, true)
	}
}

fn (e MsvcTestEnvironment) restore() {
	for i := e.names.len - 1; i >= 0; i-- {
		if e.was[i] {
			os.setenv(e.names[i], e.values[i], true)
		} else {
			os.unsetenv(e.names[i])
		}
	}
}

fn test_msvc_arch_dir() {
	assert msvc_arch_dir('amd64') == 'x64'
	assert msvc_arch_dir('x64') == 'x64'
	assert msvc_arch_dir('x86') == 'x86'
	assert msvc_arch_dir('arm64') == 'arm64'
	assert msvc_arch_dir('arm32') == ''
	assert msvc_arch_dir('riscv64') == ''
}

fn test_msvc_cl_location_reads_the_layout_of_a_visual_studio_installation() {
	x64 := msvc_cl_location(r'C:\VS\VC\Tools\MSVC\14.51.36231\bin\Hostx64\x64\cl.exe') or {
		assert false, 'a cl.exe in a Visual Studio layout was not recognized'
		return
	}
	assert x64 == MsvcClLocation{
		tools_dir: 'C:/VS/VC/Tools/MSVC/14.51.36231'
		host:      'x64'
		target:    'x64'
	}
	cross := msvc_cl_location('C:/VS/VC/Tools/MSVC/14.51.36231/bin/Hostx64/arm64/CL.EXE') or {
		assert false, 'a cross compiler was not recognized'
		return
	}
	assert cross.host == 'x64'
	assert cross.target == 'arm64'
	native := msvc_cl_location('C:/VS/VC/Tools/MSVC/14.51.36231/bin/Hostarm64/arm64/cl.exe') or {
		assert false, 'a native arm64 compiler was not recognized'
		return
	}
	assert native.host == 'arm64'
	assert msvc_cl_location(r'C:\tools\cl.exe') == none
	assert msvc_cl_location('C:/a/b/c/bin/Hostfoo/x64/cl.exe') == none
	assert msvc_cl_location('C:/a/b/c/bin/Hostx64/mips/cl.exe') == none
	assert msvc_cl_location('C:/a/b/c/notbin/Hostx64/x64/cl.exe') == none
	assert msvc_cl_location('C:/a/b/c/bin/Hostx64/x64/clang.exe') == none
	assert msvc_cl_location('') == none
}

fn test_msvc_version_order_is_numeric() {
	assert msvc_version_less('10.0.9600.0', '10.0.19041.0')
	assert !msvc_version_less('10.0.19041.0', '10.0.9600.0')
	assert msvc_version_less('14.9.1', '14.10.0')
	assert msvc_version_less('10.0', '10.0.1')
	assert !msvc_version_less('10.0.1', '10.0.1')
	assert !msvc_version_less('10.0.0.0', '10.0')
}

fn test_msvc_is_version_name() {
	assert msvc_is_version_name('10.0.26100.0')
	assert msvc_is_version_name('14.51.36231')
	assert !msvc_is_version_name('wdf')
	assert !msvc_is_version_name('10')
	assert !msvc_is_version_name('10..1')
	assert !msvc_is_version_name('10.0.x')
	assert !msvc_is_version_name('')
	// A part that does not fit an int is not a version, and counts as 0 when it is compared.
	assert msvc_is_version_name('10.0.999999999.0')
	assert !msvc_is_version_name('10.0.9999999999.0')
	assert msvc_version_part('2147483648') == 0
	assert msvc_version_part('12x') == 0
	assert msvc_version_part('') == 0
	assert msvc_version_part('19041') == 19041
	assert msvc_version_less('10.0.9999999999.0', '10.0.1.0')
}

fn test_msvc_tools_dir_of_install_prefers_the_default_toolset_then_the_newest() {
	root := msvc_test_root('install')
	defer {
		os.rmdir_all(root) or {}
	}
	older := msvc_test_install(root, '14.9.100')
	newer := msvc_test_install(root, '14.10.200')
	// Without a default, the newest toolset is the one: 14.10 is newer than 14.9.
	assert msvc_tools_dir_of_install(root, 'x64') == newer
	os.mkdir_all(os.join_path(root, 'VC', 'Auxiliary', 'Build')) or { panic(err) }
	version_file := os.join_path(root, 'VC', 'Auxiliary', 'Build', 'Microsoft.VCToolsVersion.default.txt')
	os.write_file(version_file, '14.9.100\r\n') or { panic(err) }
	assert msvc_tools_dir_of_install(root, 'x64') == older
	// A default that is not installed is not followed.
	os.write_file(version_file, '99.0.0\n') or { panic(err) }
	assert msvc_tools_dir_of_install(root, 'x64') == newer
	assert msvc_tools_dir_of_install(os.join_path(root, 'nothing'), 'x64') == ''
	// A newer toolset that is not complete (a half removed one) is skipped for the next one, and
	// so is a default one that lacks the libraries for the architecture.
	broken := os.join_path(root, 'VC', 'Tools', 'MSVC', '14.99.1')
	msvc_test_dirs(os.join_path(broken, 'include'))
	assert msvc_tools_dir_of_install(root, 'x64') == newer
	os.write_file(version_file, '14.99.1\n') or { panic(err) }
	assert msvc_tools_dir_of_install(root, 'x64') == newer
	// No toolset has libraries for arm64, so none is usable for it.
	assert msvc_tools_dir_of_install(root, 'arm64') == ''
}

fn test_msvc_find_cl_dir_prefers_the_host_and_needs_the_target() {
	root := msvc_test_root('cl_dir')
	defer {
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(root, '14.51.1')
	msvc_test_touch(os.join_path(tools, 'bin', 'Hostx86', 'x86', 'cl.exe'))
	assert msvc_find_cl_dir(tools, 'x86', 'x86') == os.join_path(tools, 'bin', 'Hostx86', 'x86')
	assert msvc_find_cl_dir(tools, 'x64', 'x86') == os.join_path(tools, 'bin', 'Hostx64', 'x86')
	// Another host still builds for the target.
	assert msvc_find_cl_dir(tools, 'arm64', 'x64') == os.join_path(tools, 'bin', 'Hostx64', 'x64')
	assert msvc_find_cl_dir(tools, 'x64', 'arm64') == ''
}

fn test_msvc_find_windows_sdk_picks_the_newest_complete_version() {
	root := msvc_test_root('sdk')
	defer {
		os.rmdir_all(root) or {}
	}
	kits := os.join_path(root, 'kits')
	// "10.0.9600.0" sorts after "10.0.19041.0" as text, but is the older SDK.
	msvc_test_sdk(kits, '10.0.9600.0', ['x64'])
	msvc_test_sdk(kits, '10.0.19041.0', ['x64', 'x86'])
	// The newest SDK has no x64 libraries, so it cannot be used for x64.
	msvc_test_sdk(kits, '10.0.26100.0', ['x86'])
	// Newer ones that lack just the C runtime libraries, or just the Windows ones, for x64.
	msvc_test_sdk(kits, '10.0.22621.0', [])
	msvc_test_dirs(os.join_path(kits, 'Lib', '10.0.22621.0', 'ucrt', 'x64'))
	msvc_test_sdk(kits, '10.0.22000.0', [])
	msvc_test_dirs(os.join_path(kits, 'Lib', '10.0.22000.0', 'um', 'x64'))
	msvc_test_dirs(os.join_path(kits, 'Lib', 'wdf'))
	x64 := msvc_find_windows_sdk([kits], 'x64') or {
		assert false, 'no SDK was found for x64'
		return
	}
	assert x64 == MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	x86 := msvc_find_windows_sdk([kits], 'x86') or {
		assert false, 'no SDK was found for x86'
		return
	}
	assert x86.version == '10.0.26100.0'
	assert msvc_find_windows_sdk([kits], 'arm64') == none
	// A root without an SDK is skipped for the next one.
	empty := os.join_path(root, 'empty')
	os.mkdir_all(empty) or { panic(err) }
	next := msvc_find_windows_sdk(['', empty, os.join_path(root, 'missing'), kits], 'x64') or {
		assert false, 'the SDK after the roots without one was not found'
		return
	}
	assert next.root == kits
	assert msvc_find_windows_sdk(['', empty], 'x64') == none
}

fn test_msvc_include_and_lib_cover_the_runtime_the_toolset_and_the_sdk() {
	sdk := MsvcWindowsSdk{
		root:    'K'
		version: '10.0.1.0'
	}
	include := msvc_include_value('T', sdk).split(os.path_delimiter)
	assert include == [os.join_path('K', 'Include', '10.0.1.0', 'ucrt'), os.join_path('T', 'include'),
		os.join_path('K', 'Include', '10.0.1.0', 'um'),
		os.join_path('K', 'Include', '10.0.1.0', 'shared')]
	lib := msvc_lib_value('T', sdk, 'x64').split(os.path_delimiter)
	assert lib == [os.join_path('T', 'lib', 'x64'),
		os.join_path('K', 'Lib', '10.0.1.0', 'ucrt', 'x64'),
		os.join_path('K', 'Lib', '10.0.1.0', 'um', 'x64')]
}

fn test_msvc_parse_registry_value() {
	mut env := MsvcTestEnvironment{}
	defer { env.restore() }
	env.set('V_MSVC_UNDEFINED_REGISTRY_TEST', '')
	output := '\r\nHKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots\r\n    KitsRoot10    REG_SZ    C:\\Program Files (x86)\\Windows Kits\\10\\\r\n\r\n'
	assert msvc_parse_registry_value(output, 'KitsRoot10') == r'C:\Program Files (x86)\Windows Kits\10\'
	assert msvc_parse_registry_value('    KitsRoot10\tREG_EXPAND_SZ\t%V_MSVC_UNDEFINED_REGISTRY_TEST%\\kits',
		'KitsRoot10') == r'%V_MSVC_UNDEFINED_REGISTRY_TEST%\kits'
	assert msvc_parse_registry_value('    KitsRoot10    REG_DWORD    0x1', 'KitsRoot10') == ''
	assert msvc_parse_registry_value('ERROR: The system was unable to find the specified registry key or value.',
		'KitsRoot10') == ''
	assert msvc_parse_registry_value('', 'KitsRoot10') == ''
}

fn test_msvc_expandable_registry_paths_find_the_sdk() {
	root := msvc_test_root('registry_expand')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	kits := os.join_path(root, 'kits with spaces')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64'])
	env.set('V_MSVC_REGISTRY_TEST_ROOT', kits)
	env.set('V_MSVC_UNDEFINED_REGISTRY_TEST', '')
	value := msvc_parse_registry_value('KitsRoot10 REG_EXPAND_SZ %V_MSVC_REGISTRY_TEST_ROOT%',
		'KitsRoot10')
	assert value == kits
	sdk := msvc_find_windows_sdk([value], 'x64') or {
		assert false, 'the SDK under an expanded registry root was not found'
		return
	}
	assert sdk.root == kits
	assert msvc_parse_registry_value('KitsRoot10 REG_SZ %V_MSVC_REGISTRY_TEST_ROOT%',
		'KitsRoot10') == '%V_MSVC_REGISTRY_TEST_ROOT%'
	assert msvc_expand_registry_environment('%V_MSVC_UNDEFINED_REGISTRY_TEST%/kits') ==
		'%V_MSVC_UNDEFINED_REGISTRY_TEST%/kits'
	assert msvc_expand_registry_environment('kits%unfinished') == 'kits%unfinished'
	assert msvc_expand_registry_environment('kits%%') == 'kits%%'
	env.set('V_MSVC_REGISTRY_TEST_ROOT', '%V_MSVC_UNDEFINED_REGISTRY_TEST%')
	assert msvc_expand_registry_environment('%V_MSVC_REGISTRY_TEST_ROOT%') ==
		'%V_MSVC_UNDEFINED_REGISTRY_TEST%'
}

fn test_msvc_prepare_environment_sets_up_what_a_developer_prompt_would() {
	root := msvc_test_root('prepare')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64'])
	nowhere := os.join_path(root, 'nowhere')
	os.mkdir_all(nowhere) or { panic(err) }
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	// No `cl` on PATH, and neither INCLUDE nor LIB: the toolset comes from VCToolsInstallDir.
	env.set('PATH', nowhere)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', tools + os.path_separator.str())
	env.set('WindowsSdkDir', kits)
	prepared := msvc_prepare_environment('cl', target)
	assert prepared.problem == ''
	assert prepared.saved.map(it.name) == ['PATH', 'INCLUDE', 'LIB']
	cl_dir := os.join_path(tools, 'bin', 'Hostx64', 'x64')
	assert os.getenv('PATH') == cl_dir + os.path_delimiter + nowhere
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('INCLUDE') == msvc_include_value(tools, sdk)
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x64')
	assert os.getenv('INCLUDE').split(os.path_delimiter).len == 4
	assert os.getenv('LIB').split(os.path_delimiter).len == 3
}

fn test_msvc_restore_environment_puts_back_what_the_build_replaced() {
	root := msvc_test_root('restore')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64'])
	nowhere := os.join_path(root, 'nowhere')
	os.mkdir_all(nowhere) or { panic(err) }
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	// PATH has a value, and INCLUDE and LIB are not set at all.
	env.set('PATH', nowhere)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', tools)
	env.set('WindowsSdkDir', kits)
	prepared := msvc_prepare_environment('cl', target)
	assert prepared.problem == ''
	assert os.getenv('INCLUDE') != ''
	assert os.getenv('LIB') != ''
	assert os.getenv('PATH') != nowhere
	msvc_restore_environment(prepared.saved)
	assert os.getenv('PATH') == nowhere
	assert os.getenv_opt('INCLUDE') == none
	assert os.getenv_opt('LIB') == none
	// Nothing recorded, nothing restored.
	msvc_restore_environment([]MsvcSavedVariable{})
	assert os.getenv('PATH') == nowhere
	// `was_set` decides, not the value: a variable that was not set before is not set after,
	// and one that was set gets its value back. (Windows cannot tell an unset variable from one
	// set to an empty string, so a non-empty value is what tells these two apart on every OS.)
	env.set('V_MSVC_TEST_VARIABLE', 'during the build')
	msvc_restore_environment([
		MsvcSavedVariable{
			name:    'V_MSVC_TEST_VARIABLE'
			value:   'a value that must not come back'
			was_set: false
		},
	])
	assert os.getenv_opt('V_MSVC_TEST_VARIABLE') == none
	msvc_restore_environment([
		MsvcSavedVariable{
			name:    'V_MSVC_TEST_VARIABLE'
			value:   'before the build'
			was_set: true
		},
	])
	assert os.getenv('V_MSVC_TEST_VARIABLE') == 'before the build'
	// msvc_set_variable records whether the variable was set, and what it was.
	env.set('V_MSVC_TEST_VARIABLE', '')
	mut saved := []MsvcSavedVariable{}
	msvc_set_variable(mut saved, 'V_MSVC_TEST_VARIABLE', 'first')
	msvc_set_variable(mut saved, 'V_MSVC_TEST_VARIABLE', 'second')
	assert saved == [
		MsvcSavedVariable{
			name:    'V_MSVC_TEST_VARIABLE'
			value:   ''
			was_set: false
		},
		MsvcSavedVariable{
			name:    'V_MSVC_TEST_VARIABLE'
			value:   'first'
			was_set: true
		},
	]
	assert os.getenv('V_MSVC_TEST_VARIABLE') == 'second'
	// Restoring in the opposite order brings back the state from before the first change.
	msvc_restore_environment(saved)
	assert os.getenv_opt('V_MSVC_TEST_VARIABLE') == none
}

fn test_msvc_prepare_environment_leaves_no_empty_path_entry() {
	root := msvc_test_root('empty_path')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64'])
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', '')
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', tools)
	env.set('WindowsSdkDir', kits)
	assert msvc_prepare_environment('cl', target).problem == ''
	assert os.getenv('PATH') == os.join_path(tools, 'bin', 'Hostx64', 'x64')
}

fn test_msvc_prepare_environment_keeps_what_is_set() {
	root := msvc_test_root('keep')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64'])
	nowhere := os.join_path(root, 'nowhere')
	os.mkdir_all(nowhere) or { panic(err) }
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', nowhere)
	env.set('VCToolsInstallDir', tools)
	env.set('WindowsSdkDir', kits)
	// A prompt's own INCLUDE and LIB stay as they are.
	env.set('INCLUDE', 'prompt-include')
	env.set('LIB', 'prompt-lib')
	first := msvc_prepare_environment('cl', target)
	assert first.problem == ''
	assert 'INCLUDE' !in first.saved.map(it.name)
	assert 'LIB' !in first.saved.map(it.name)
	assert os.getenv('INCLUDE') == 'prompt-include'
	assert os.getenv('LIB') == 'prompt-lib'
	msvc_restore_environment(first.saved)
	// Only the missing one is filled in.
	env.set('LIB', '')
	second := msvc_prepare_environment('cl', target)
	assert second.problem == ''
	assert 'INCLUDE' !in second.saved.map(it.name)
	assert 'LIB' in second.saved.map(it.name)
	assert os.getenv('INCLUDE') == 'prompt-include'
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x64')
}

// msvc_test_cl lays out a `cl` for target in the toolset of an installation, that can be found
// on PATH on every platform, and returns the directory and the file.
fn msvc_test_cl(tools string, target string) (string, string) {
	cl_name := $if windows { 'cl.exe' } $else { 'cl' }
	cl_dir := os.join_path(tools, 'bin', 'Hostx64', target)
	cl_file := os.join_path(cl_dir, cl_name)
	msvc_test_touch(cl_file)
	os.chmod(cl_file, 0o755) or { panic(err) }
	return cl_dir, cl_file
}

fn test_msvc_prepare_environment_follows_the_toolset_of_the_cl_on_path() {
	root := msvc_test_root('follow_cl')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	other := msvc_test_install(os.join_path(root, 'other'), '14.52.2')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64', 'x86'])
	// The `cl` on PATH builds for x64, and belongs to `tools`, not to the toolset that
	// VCToolsInstallDir names.
	cl_dir, _ := msvc_test_cl(tools, 'x64')
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', cl_dir)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', other)
	env.set('WindowsSdkDir', kits)
	prepared := msvc_prepare_environment('cl', target)
	assert prepared.problem == ''
	// The `cl` is the right one: PATH stays, INCLUDE and LIB come from its toolset.
	assert prepared.saved.map(it.name) == ['INCLUDE', 'LIB']
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('PATH') == cl_dir
	assert os.getenv('INCLUDE') == msvc_include_value(tools, sdk)
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x64')
}

fn test_msvc_prepare_environment_swaps_a_cl_for_another_architecture() {
	root := msvc_test_root('swap_cl')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64', 'x86'])
	// The `cl` on PATH builds for x86, but V generates C for x64: the x64 `cl` of the same
	// toolset goes in front of it, and the libraries are the x64 ones.
	x86_dir, _ := msvc_test_cl(tools, 'x86')
	x64_dir := os.join_path(tools, 'bin', 'Hostx64', 'x64')
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', x86_dir)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', '')
	env.set('WindowsSdkDir', kits)
	prepared := msvc_prepare_environment('cl', target)
	assert prepared.problem == ''
	assert prepared.saved.map(it.name) == ['PATH', 'INCLUDE', 'LIB']
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('PATH') == x64_dir + os.path_delimiter + x86_dir
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x64')
	// A target for x86 keeps the x86 `cl`.
	msvc_restore_environment(prepared.saved)
	x86_target := pref.Target{
		os:   'windows'
		arch: 'x86'
	}
	again := msvc_prepare_environment('cl', x86_target)
	assert again.problem == ''
	assert again.saved.map(it.name) == ['INCLUDE', 'LIB']
	assert os.getenv('PATH') == x86_dir
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x86')
}

fn test_msvc_prepare_environment_reports_an_unavailable_target_compiler() {
	root := msvc_test_root('unavailable_target')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	os.rmdir_all(os.join_path(tools, 'lib', 'x64')) or { panic(err) }
	os.rmdir_all(os.join_path(tools, 'bin', 'Hostx64', 'x64')) or { panic(err) }
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x86'])
	x86_dir, x86_cl := msvc_test_cl(tools, 'x86')
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', x86_dir)
	env.set('VCToolsInstallDir', tools)
	env.set('WindowsSdkDir', kits)
	env.set('ProgramFiles(x86)', '')
	env.set('ProgramFiles', '')
	for existing in ['', 'already provided'] {
		env.set('INCLUDE', existing)
		env.set('LIB', existing)
		prepared := msvc_prepare_environment('cl', target)
		assert prepared.problem.contains('x64'), prepared.problem
		assert prepared.saved.len == 0
		assert os.getenv('PATH') == x86_dir
		assert os.getenv('INCLUDE') == existing
		assert os.getenv('LIB') == existing
	}
	// Naming the available x86 compiler by its path remains an explicit choice.
	env.set('INCLUDE', '')
	env.set('LIB', '')
	explicit := msvc_prepare_environment(x86_cl, target)
	assert explicit.problem == '', explicit.problem
	assert os.getenv('PATH') == x86_dir
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x86')
}

fn test_msvc_prepare_environment_finds_a_target_toolset_after_a_wrong_arch_cl() {
	root := msvc_test_root('target_toolset')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	os.rmdir_all(os.join_path(tools, 'lib', 'x64')) or { panic(err) }
	os.rmdir_all(os.join_path(tools, 'bin', 'Hostx64', 'x64')) or { panic(err) }
	other := msvc_test_install(os.join_path(root, 'other'), '14.52.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64'])
	x86_dir, _ := msvc_test_cl(tools, 'x86')
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', x86_dir)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', other)
	env.set('WindowsSdkDir', kits)
	prepared := msvc_prepare_environment('cl', target)
	assert prepared.problem == '', prepared.problem
	assert os.getenv('PATH').starts_with(os.join_path(other, 'bin', 'Hostx64', 'x64'))
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('INCLUDE') == msvc_include_value(other, sdk)
	assert os.getenv('LIB') == msvc_lib_value(other, sdk, 'x64')
}

fn test_msvc_prepare_environment_keeps_a_cl_that_is_named_by_its_path() {
	root := msvc_test_root('named_cl')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	tools := msvc_test_install(os.join_path(root, 'vs'), '14.51.1')
	kits := os.join_path(root, 'kits')
	msvc_test_sdk(kits, '10.0.19041.0', ['x64', 'x86'])
	// `-cc <path to the x86 cl>` is a choice, so PATH stays, and the libraries are the x86 ones.
	_, x86_cl := msvc_test_cl(tools, 'x86')
	nowhere := os.join_path(root, 'nowhere')
	os.mkdir_all(nowhere) or { panic(err) }
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	env.set('PATH', nowhere)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', '')
	env.set('WindowsSdkDir', kits)
	prepared := msvc_prepare_environment(x86_cl, target)
	assert prepared.problem == ''
	assert prepared.saved.map(it.name) == ['INCLUDE', 'LIB']
	sdk := MsvcWindowsSdk{
		root:    kits
		version: '10.0.19041.0'
	}
	assert os.getenv('PATH') == nowhere
	assert os.getenv('LIB') == msvc_lib_value(tools, sdk, 'x86')
}

fn test_msvc_system_reg_exe_is_never_looked_up_on_path() {
	root := msvc_test_root('reg_exe')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	fake_reg := os.join_path(root, 'Windows', 'System32', 'reg.exe')
	msvc_test_touch(fake_reg)
	elsewhere := os.join_path(root, 'elsewhere')
	msvc_test_touch(os.join_path(elsewhere, 'reg.exe'))
	env.set('PATH', elsewhere)
	env.set('SystemRoot', os.join_path(root, 'Windows'))
	assert msvc_system_reg_exe() == fake_reg
	// Without a system folder, or without a `reg.exe` in it, there is none: a `reg.exe` that
	// happens to be on PATH does not replace it.
	env.set('SystemRoot', os.join_path(root, 'nothing'))
	assert msvc_system_reg_exe() == ''
	env.set('SystemRoot', '')
	assert msvc_system_reg_exe() == ''
	assert msvc_registry_windows_kits_root() == ''
}

fn test_msvc_prepare_environment_says_what_is_missing() {
	root := msvc_test_root('missing')
	mut env := MsvcTestEnvironment{}
	defer {
		env.restore()
		os.rmdir_all(root) or {}
	}
	nowhere := os.join_path(root, 'nowhere')
	os.mkdir_all(nowhere) or { panic(err) }
	target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	// Nothing to find: no `cl`, no VCToolsInstallDir, and no Visual Studio installer.
	env.set('PATH', nowhere)
	env.set('INCLUDE', '')
	env.set('LIB', '')
	env.set('VCToolsInstallDir', '')
	env.set('ProgramFiles(x86)', nowhere)
	env.set('ProgramFiles', nowhere)
	prepared := msvc_prepare_environment('cl', target)
	assert prepared.problem.contains('Visual Studio C++ tools'), prepared.problem
	assert prepared.problem.contains('Developer Command Prompt'), prepared.problem
	assert prepared.saved.len == 0
	assert os.getenv('INCLUDE') == ''
	assert os.getenv('LIB') == ''
	// An architecture that MSVC does not build for is left to the compiler: nothing is set up,
	// and nothing is reported.
	riscv := pref.Target{
		os:   'windows'
		arch: 'riscv64'
	}
	left_alone := msvc_prepare_environment('cl', riscv)
	assert left_alone.problem == ''
	assert left_alone.saved.len == 0
	assert msvc_no_sdk_message('x64').contains('Windows 10/11 SDK')
}
