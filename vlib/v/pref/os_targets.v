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
