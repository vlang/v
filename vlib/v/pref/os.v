// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os

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
	termux // like android, but compiling/running natively on the devices
	solaris
	serenity
	vinix
	haiku
	wasm32
	wasm32_emscripten
	wasm32_wasi
	raw
	all
}

// Helper function to convert string names to OS enum
pub fn os_from_string(os_str string) !OS {
	match os_str {
		'' {
			return ._auto
		}
		'linux' {
			return .linux
		}
		'nix' {
			return .linux
		}
		'windows' {
			return .windows
		}
		'ios' {
			return .ios
		}
		'macos' {
			return .macos
		}
		'darwin' {
			return .macos
		}
		'freebsd' {
			return .freebsd
		}
		'openbsd' {
			return .openbsd
		}
		'netbsd' {
			return .netbsd
		}
		'dragonfly' {
			return .dragonfly
		}
		'js', 'js_node' {
			return .js_node
		}
		'js_freestanding' {
			return .js_freestanding
		}
		'js_browser' {
			return .js_browser
		}
		'solaris' {
			return .solaris
		}
		'serenity' {
			return .serenity
		}
		'vinix' {
			return .vinix
		}
		'android' {
			return .android
		}
		'termux' {
			return .termux
		}
		'haiku' {
			return .haiku
		}
		'raw' {
			return .raw
		}
		// WASM options:
		'wasm32' {
			return .wasm32
		}
		'wasm32_wasi' {
			return .wasm32_wasi
		}
		'wasm32_emscripten' {
			return .wasm32_emscripten
		}
		else {
			// handle deprecated names:
			match os_str {
				'wasm32-emscripten' {
					eprintln('Please use `-os wasm32_emscripten` instead.')
					return .wasm32_emscripten
				}
				'wasm32-wasi' {
					eprintln('Please use `-os wasm32_wasi` instead.')
					return .wasm32_wasi
				}
				else {}
			}
			return error('bad OS $os_str')
		}
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
		.termux { return 'Termux' }
		.solaris { return 'Solaris' }
		.serenity { return 'SerenityOS' }
		.vinix { return 'Vinix' }
		.haiku { return 'Haiku' }
		.wasm32 { return 'WebAssembly' }
		.wasm32_emscripten { return 'WebAssembly(Emscripten)' }
		.wasm32_wasi { return 'WebAssembly(WASI)' }
		.raw { return 'Raw' }
		.all { return 'all' }
	}
}

pub fn get_host_os() OS {
	if os.getenv('TERMUX_VERSION') != '' {
		return .termux
	}
	$if android {
		return .android
	}
	$if emscripten ? {
		return .wasm32_emscripten
	}
	// TODO: make this work:
	// $if wasm32_emscripten {
	// 	return .wasm32_emscripten
	// }
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
