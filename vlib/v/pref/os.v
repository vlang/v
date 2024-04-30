// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
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
	qnx
	serenity
	plan9
	vinix
	haiku
	wasm32
	wasm32_emscripten
	wasm32_wasi
	browser // -b wasm -os browser
	wasi // -b wasm -os wasi
	raw
	all
}

// Helper function to convert string names to OS enum
pub fn os_from_string(os_str string) !OS {
	return match os_str {
		'' { ._auto }
		'linux', 'nix' { .linux }
		'windows' { .windows }
		'ios' { .ios }
		'macos' { .macos }
		'darwin' { .macos }
		'freebsd' { .freebsd }
		'openbsd' { .openbsd }
		'netbsd' { .netbsd }
		'dragonfly' { .dragonfly }
		'js', 'js_node' { .js_node }
		'js_freestanding' { .js_freestanding }
		'js_browser' { .js_browser }
		'solaris' { .solaris }
		'qnx' { .qnx }
		'serenity' { .serenity }
		'plan9' { .plan9 }
		'vinix' { .vinix }
		'android' { .android }
		'termux' { .termux }
		'haiku' { .haiku }
		'raw' { .raw }
		// WASM options:
		'wasm32' { .wasm32 }
		'wasm32_wasi' { .wasm32_wasi }
		'wasm32_emscripten' { .wasm32_emscripten }
		// Native WASM options:
		'browser' { .browser }
		'wasi' { .wasi }
		else { error('bad OS ${os_str}') }
	}
}

pub fn (o OS) str() string {
	return match o {
		._auto { 'RESERVED: AUTO' }
		.ios { 'iOS' }
		.macos { 'MacOS' }
		.linux { 'Linux' }
		.windows { 'Windows' }
		.freebsd { 'FreeBSD' }
		.openbsd { 'OpenBSD' }
		.netbsd { 'NetBSD' }
		.dragonfly { 'Dragonfly' }
		.js_node { 'NodeJS' }
		.js_freestanding { 'JavaScript' }
		.js_browser { 'JavaScript(Browser)' }
		.android { 'Android' }
		.termux { 'Termux' }
		.solaris { 'Solaris' }
		.qnx { 'QNX' }
		.serenity { 'SerenityOS' }
		.plan9 { 'Plan9' }
		.vinix { 'Vinix' }
		.haiku { 'Haiku' }
		.wasm32 { 'WebAssembly' }
		.wasm32_emscripten { 'WebAssembly(Emscripten)' }
		.wasm32_wasi { 'WebAssembly(WASI)' }
		.browser { 'browser' }
		.wasi { 'wasi' }
		.raw { 'Raw' }
		.all { 'all' }
	}
}

pub fn get_host_os() OS {
	if os.getenv('TERMUX_VERSION') != '' {
		return .termux
	}
	return $if android {
		.android
	} $else $if emscripten ? {
		.wasm32_emscripten
		// TODO: make this work:
		// $else $if wasm32_emscripten {
		// 	.wasm32_emscripten
	} $else $if linux {
		.linux
	} $else $if ios {
		.ios
	} $else $if macos {
		.macos
	} $else $if windows {
		.windows
	} $else $if freebsd {
		.freebsd
	} $else $if openbsd {
		.openbsd
	} $else $if netbsd {
		.netbsd
	} $else $if dragonfly {
		.dragonfly
	} $else $if serenity {
		.serenity
		// } $else $if plan9 {
		//		.plan9
	} $else $if vinix {
		.vinix
	} $else $if solaris {
		.solaris
	} $else $if haiku {
		.haiku
	} $else $if js_node {
		.js_node
	} $else $if js_freestanding {
		.js_freestanding
	} $else $if js_browser {
		.js_browser
	} $else $if js {
		.js_node
	} $else {
		panic('unknown host OS')
		return ._auto
	}
}
