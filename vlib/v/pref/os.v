// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

pub enum OS {
	_auto // Reserved so .macos cannot be misunderstood as auto
	ios
	macos
	linux
	windows
	freebsd
	openbsd
	netbsd
	dragonfly
	js_node
	js_browser
	js_freestanding
	android
	solaris
	serenity
	vinix
	haiku
	raw
	all
}

// Helper function to convert string names to OS enum
pub fn os_from_string(os_str string) ?OS {
	match os_str {
		'linux' { return .linux }
		'windows' { return .windows }
		'ios' { return .ios }
		'macos' { return .macos }
		'freebsd' { return .freebsd }
		'openbsd' { return .openbsd }
		'netbsd' { return .netbsd }
		'dragonfly' { return .dragonfly }
		'js', 'js_node' { return .js_node }
		'js_freestanding' { return .js_freestanding }
		'js_browser' { return .js_browser }
		'solaris' { return .solaris }
		'serenity' { return .serenity }
		'vinix' { return .vinix }
		'android' { return .android }
		'haiku' { return .haiku }
		'raw' { return .raw }
		'nix' { return .linux }
		'' { return ._auto }
		else { return error('bad OS $os_str') }
	}
}

pub fn (o OS) str() string {
	match o {
		._auto { return 'RESERVED: AUTO' }
		.ios { return 'iOS' }
		.macos { return 'MacOS' }
		.linux { return 'Linux' }
		.windows { return 'Windows' }
		.freebsd { return 'FreeBSD' }
		.openbsd { return 'OpenBSD' }
		.netbsd { return 'NetBSD' }
		.dragonfly { return 'Dragonfly' }
		.js_node { return 'NodeJS' }
		.js_freestanding { return 'JavaScript' }
		.js_browser { return 'JavaScript(Browser)' }
		.android { return 'Android' }
		.solaris { return 'Solaris' }
		.serenity { return 'SerenityOS' }
		.vinix { return 'Vinix' }
		.haiku { return 'Haiku' }
		.raw { return 'Raw' }
		.all { return 'all' }
	}
}

pub fn get_host_os() OS {
	$if linux {
		return .linux
	}
	$if ios {
		return .ios
	}
	$if macos {
		return .macos
	}
	$if windows {
		return .windows
	}
	$if freebsd {
		return .freebsd
	}
	$if openbsd {
		return .openbsd
	}
	$if netbsd {
		return .netbsd
	}
	$if dragonfly {
		return .dragonfly
	}
	$if serenity {
		return .serenity
	}
	$if vinix {
		return .vinix
	}
	$if solaris {
		return .solaris
	}
	$if haiku {
		return .haiku
	}
	$if js_node {
		return .js_node
	}
	$if js_freestanding {
		return .js_freestanding
	}
	$if js_browser {
		return .js_browser
	}
	$if js {
		return .js_node
	}
	panic('unknown host OS')
	return ._auto
}
