module pref

import os

// Each alias below is one that the `-os`/`-arch` options and the `_<os>.v`/`.v`
// file suffixes accept, so a missing alias silently drops a whole platform from
// source selection.
fn test_normalized_os_maps_aliases_to_canonical_names() {
	for alias, canonical in {
		'darwin':     'macos'
		'mac':        'macos'
		'macos':      'macos'
		'win32':      'windows'
		'windows':    'windows'
		'linux':      'linux'
		'emscripten': 'wasm32_emscripten'
	} {
		assert normalized_os(alias) == canonical
	}
}

fn test_normalized_arch_maps_aliases_to_canonical_names() {
	for alias, canonical in {
		'x64':         'amd64'
		'x86_64':      'amd64'
		'amd64':       'amd64'
		'aarch64':     'arm64'
		'i386':        'x86'
		'x32':         'x86'
		'ia-32':       'x86'
		'aarch32':     'arm32'
		'rv32':        'riscv32'
		'risc-v32':    'riscv32'
		'rv64':        'riscv64'
		'riscv':       'riscv64'
		'ppc32':       'ppc'
		'powerpc':     'ppc'
		'wasm':        'wasm32'
		'loongarch64': 'loongarch64'
	} {
		assert normalized_arch(alias) == canonical
	}
}

fn test_arch_from_string_canonicalizes_known_aliases_only() {
	assert arch_from_string('x86_64') or { '' } == 'amd64'
	assert arch_from_string('aarch64') or { '' } == 'arm64'
	assert arch_from_string('rv32') or { '' } == 'riscv32'
	assert arch_from_string('wasm') or { '' } == 'wasm32'
	// Trimming and lowercasing happen before the lookup.
	assert arch_from_string(' X86_64 ') or { '' } == 'amd64'
	// `c` and `js` name a backend, not an architecture.
	assert arch_from_string('c') == none
	assert arch_from_string('js') == none
	assert arch_from_string('native') == none
	assert arch_from_string('wasm32_emscripten') == none
	assert arch_from_string('') == none
}

// A backend name must never be read as an architecture: `wasm` spells both, and
// treating it as an arch would hide `*.wasm.v` from `-b wasm` while making every
// native host skip it as foreign.
fn test_suffix_is_backend_name_accepts_backends_and_rejects_architectures() {
	for backend in known_backend_suffixes {
		assert suffix_is_backend_name(backend)
	}
	assert suffix_is_backend_name('C')
	assert suffix_is_backend_name(' JS ')
	assert !suffix_is_backend_name('arm64')
	assert !suffix_is_backend_name('amd64')
	assert !suffix_is_backend_name('wasm32')
	assert !suffix_is_backend_name('')
}

fn test_option_may_consume_value_lists_the_value_taking_options() {
	for option in ['-wasm-stack-top', '-arch', '-e', '-subsystem', '-icon', '--icon', '-seticon',
		'--seticon', '-gc', '-print_autofree_vars_in_fn', '-trace-fns', '-prof', '-profile', '-cov',
		'-coverage', '-profile-fns', '-bug-report-url', '-run-only', '-exclude', '-file-list',
		'-test-runner', '-dump-c-flags', '-dump-modules', '-dump-files', '-dump-defines',
		'-generate-c-project', '-macosx-version-min', '-os', '-printfn', '-cflags', '-ldflags',
		'-d', '-define', '-message-limit', '-thread-stack-size', '-cc', '-c++',
		'-checker-match-exhaustive-cutoff-limit', '-o', '-output', '-b', '-backend', '-compile-backend',
		'--compile-backend', '-path', '-bare-builtin-dir', '-custom-prelude', '-raw-vsh-tmp-prefix',
		'-cmain', '-line-info', '-memory-limit', '--memory-limit'] {
		assert option_may_consume_value(option), '`${option}` should consume a value'
	}
	for option in ['-prod', '-g', '-cg', '-silent', '-h', '-version', '', 'not-an-option', '-ccflags'] {
		assert !option_may_consume_value(option), '`${option}` should not consume a value'
	}
}

fn test_supported_test_runners_list_quotes_every_runner() {
	assert supported_test_runners_list() == '`normal`, `simple`, `tap`, `dump`, `teamcity`'
	assert supported_test_runners_list() == supported_test_runners.map('`${it}`').join(', ')
}

// `nix` names a family, `all` means every target, and a specific os name must not
// be satisfied by a different one.
fn test_os_is_target_of_matches_the_named_target_or_its_family() {
	assert os_is_target_of('linux', 'nix')
	assert os_is_target_of('macos', 'nix')
	assert !os_is_target_of('windows', 'nix')
	assert !os_is_target_of('linux', 'windows')
	assert !os_is_target_of('windows', 'linux')
	assert !os_is_target_of('linux', 'darwin')
	assert !os_is_target_of('macos', 'linux')
	assert os_is_target_of('macos', 'darwin')
	// `all` is the one target every os answers yes to.
	assert os_is_target_of('all', 'windows')
	assert os_is_target_of('all', 'vinix')
	// Only the BSDs satisfy `bsd`; macOS is one of them.
	assert os_is_target_of('freebsd', 'bsd')
	assert os_is_target_of('macos', 'bsd')
	assert !os_is_target_of('linux', 'bsd')
	// The host os is aliased before the comparison.
	assert !os_is_target_of('darwin', 'linux')
	assert !os_is_target_of('win32', 'nix')
	assert !os_is_target_of('mac', 'linux')
}

// A release that does not start with a decimal major version is not rejected:
// the comparison is a version test, not a malformed-input error.
fn test_darwin_release_rejects_tcc_executables_reads_the_major_version() {
	assert !darwin_release_rejects_tcc_executables('26.1.0')
	assert !darwin_release_rejects_tcc_executables('26')
	assert darwin_release_rejects_tcc_executables('27.0.1')
	assert darwin_release_rejects_tcc_executables('27')
	assert darwin_release_rejects_tcc_executables('28.3')
	assert !darwin_release_rejects_tcc_executables('')
	assert !darwin_release_rejects_tcc_executables('abc')
	assert !darwin_release_rejects_tcc_executables('.')
	assert !darwin_release_rejects_tcc_executables('x27')
}

fn test_host_os_name_reports_the_machine_running_the_compiler() {
	os.setenv('TERMUX_VERSION', '', true)
	defer {
		os.unsetenv('TERMUX_VERSION')
	}
	assert host_os_name() == host_target().os
	assert host_os_name() in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
		'android', 'termux', 'ios', 'solaris', 'qnx', 'haiku', 'serenity', 'vinix', 'wasm32_emscripten']
	// Termux runs natively on the device, so it is reported apart from Android.
	os.setenv('TERMUX_VERSION', '1', true)
	assert host_os_name() == 'termux'
}
