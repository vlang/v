module pref

import os

// supported_test_runners lists the reporter implementations that the
// `v test -test-runner <name>` option accepts.
pub const supported_test_runners = ['normal', 'simple', 'tap', 'dump', 'teamcity']

// supported_test_runners_list returns supported_test_runners, formatted for a diagnostic.
pub fn supported_test_runners_list() string {
	return supported_test_runners.map('`${it}`').join(', ')
}

// vexe_path returns the absolute path of the V compiler executable. Tools that V
// launches inherit it through $VEXE; everything else falls back to detection from
// the running executable.
pub fn vexe_path() string {
	return detect_vexe()
}

// host_os_name returns the OS name of the machine running the compiler, spelled the
// way the `_<os>.v` file suffixes and the `// vtest build:` constraints spell it.
// Termux is reported apart from Android: it runs natively on the device, so its
// toolchain has no access to the Android SDK headers.
pub fn host_os_name() string {
	if os.getenv('TERMUX_VERSION') != '' {
		return 'termux'
	}
	return host_target().os
}

// first_tcc_unloadable_darwin_release is the earliest Darwin kernel release known
// to load TCC-linked executables with corrupted globals: its dyld remaps every
// `__DATA` page listed in the chained-fixup table from the file, and TCC lists the
// zero-fill `__bss` pages there too (ld64 lists only the file-backed ones). Those
// pages then hold the `__LINKEDIT` bytes that follow `__DATA` in the file, instead
// of zeros. Observed on macOS 27.0.1 (Darwin 27.0.0); see
// https://github.com/vlang/v/issues/29744 .
const first_tcc_unloadable_darwin_release = 27

// host_rejects_tcc_executables reports whether executables that TCC links for this
// machine can start with corrupted global variables, so V must not pick TCC by
// itself. An explicit `-cc tcc` is still honored.
pub fn host_rejects_tcc_executables() bool {
	$if macos {
		return darwin_release_rejects_tcc_executables(os.uname().release)
	} $else {
		return false
	}
}

// darwin_release_rejects_tcc_executables reports whether a Darwin kernel `release`,
// as `uname -r` prints it, can load TCC-linked executables with corrupted globals.
// A release that does not start with a decimal major version is not rejected.
pub fn darwin_release_rejects_tcc_executables(release string) bool {
	major := release.all_before('.')
	if major == '' || !major.bytes().all(it.is_digit()) {
		return false
	}
	return major.int() >= first_tcc_unloadable_darwin_release
}

// os_is_target_of reports whether this_os is one of the systems named by the OS
// suffix target, for example `nix` names Linux and FreeBSD, but not Windows.
pub fn os_is_target_of(this_os string, target string) bool {
	host := normalized_os(this_os)
	if host == 'all' {
		return true
	}
	// `android_outside_termux` is the cross compilation case, where the Android SDK
	// headers are available; `termux` is the native one, where they are not.
	if (host == 'windows' && target == 'nix') || (host != 'windows' && target == 'windows')
		|| (host != 'linux' && target == 'linux')
		|| (host != 'macos' && target in ['darwin', 'macos', 'mac'])
		|| (!os_is_bsd_target(host) && target == 'bsd') || (host != 'ios' && target == 'ios')
		|| (host != 'freebsd' && target == 'freebsd')
		|| (host != 'openbsd' && target == 'openbsd')
		|| (host != 'netbsd' && target == 'netbsd')
		|| (host != 'dragonfly' && target == 'dragonfly')
		|| (host != 'solaris' && target == 'solaris') || (host != 'qnx' && target == 'qnx')
		|| (host != 'serenity' && target == 'serenity') || (host != 'haiku' && target == 'haiku')
		|| (host != 'plan9' && target == 'plan9') || (host != 'vinix' && target == 'vinix')
		|| (host != 'wasm32_emscripten' && target in ['emscripten', 'wasm32_emscripten'])
		|| (host != 'android' && target in ['android', 'android_outside_termux'])
		|| (host != 'termux' && target == 'termux') {
		return false
	}
	return true
}

fn os_is_bsd_target(this_os string) bool {
	return this_os in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly']
}

// known_backend_suffixes lists the backend names that a `.<backend>.v` file suffix may use.
// They take precedence over known_arch_names when a suffix is read, because `wasm` spells
// both a backend and an architecture and AGENTS.md documents `*.wasm.v` as the WASM backend
// split. Reading it as an architecture instead both hides the file from `-b wasm` and makes
// every native host skip it as foreign.
pub const known_backend_suffixes = ['c', 'js', 'native', 'wasm']

// suffix_is_backend_name reports whether a `.<name>.v` suffix names a backend.
pub fn suffix_is_backend_name(name string) bool {
	return name.trim_space().to_lower() in known_backend_suffixes
}

// known_arch_names lists every architecture name that a `_<arch>.v` file suffix or
// an `-arch <name>` value may use, aliases included.
pub const known_arch_names = ['amd64', 'x64', 'x86_64', 'arm64', 'aarch64', 'x86', 'i386', 'i486',
	'i586', 'i686', 'x32', 'x86_32', 'ia-32', 'ia32', 'arm32', 'aarch32', 'arm', 'armv7', 'armv7l',
	'rv32', 'risc-v32', 'riscv32', 'rv64', 'risc-v64', 'risc-v', 'riscv', 'riscv64', 'ppc', 'ppc32',
	'powerpc', 'ppc64', 'ppc64le', 's390x', 'loongarch64', 'sparc64', 'wasm', 'wasm32']

// arch_from_string returns the canonical name of the architecture that name spells,
// or none when name does not name an architecture at all (`c` and `js` name a backend).
pub fn arch_from_string(name string) ?string {
	lowered := name.trim_space().to_lower()
	if lowered !in known_arch_names {
		return none
	}
	return normalized_arch(lowered)
}
