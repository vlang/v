// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os
import v.vcache

pub const (
	default_module_path = os.vmodules_dir()
)

pub fn new_preferences() &Preferences {
	mut p := &Preferences{}
	p.fill_with_defaults()
	return p
}

fn (mut p Preferences) expand_lookup_paths() {
	if p.vroot == '' {
		// Location of all vlib files
		p.vroot = os.dir(vexe_path())
	}
	vlib_path := os.join_path(p.vroot, 'vlib')
	if p.lookup_path.len == 0 {
		p.lookup_path = ['@vlib', '@vmodules']
	}
	mut expanded_paths := []string{}
	for path in p.lookup_path {
		match path {
			'@vlib' { expanded_paths << vlib_path }
			'@vmodules' { expanded_paths << os.vmodules_paths() }
			else { expanded_paths << path }
		}
	}
	p.lookup_path = expanded_paths
}

pub fn (mut p Preferences) fill_with_defaults() {
	if p.arch == ._auto {
		p.arch = get_host_arch()
	}
	p.expand_lookup_paths()
	rpath := os.real_path(p.path)
	if p.out_name == '' {
		filename := os.file_name(rpath).trim_space()
		mut base := filename.all_before_last('.')
		if base == '' {
			// The file name is just `.v` or `.vsh` or `.*`
			base = filename
		}
		target_dir := if os.is_dir(rpath) { rpath } else { os.dir(rpath) }
		p.out_name = os.join_path(target_dir, base)
		// Do *NOT* be tempted to generate binaries in the current work folder,
		// when -o is not given by default, like Go, Clang, GCC etc do.
		//
		// These compilers also are frequently used with an external build system,
		// in part because of that shortcoming, to ensure that they work in a
		// predictable work folder/environment.
		//
		// In comparison, with V, building an executable by default places it
		// next to its source code, so that it can be used directly with
		// functions like `os.resource_abs_path()` and `os.executable()` to
		// locate resources relative to it. That enables running examples like
		// this:
		// `./v run examples/flappylearning/`
		// instead of:
		// `./v -o examples/flappylearning/flappylearning run examples/flappylearning/`
		// This topic comes up periodically from time to time on Discord, and
		// many CI breakages already happened, when someone decides to make V
		// behave in this aspect similarly to the dumb behaviour of other
		// compilers.
		//
		// If you do decide to break it, please *at the very least*, test it
		// extensively, and make a PR about it, instead of commiting directly
		// and breaking the CI, VC, and users doing `v up`.
		if rpath == '$p.vroot/cmd/v' && os.is_dir('vlib/compiler') {
			// Building V? Use v2, since we can't overwrite a running
			// executable on Windows + the precompiled V is more
			// optimized.
			println('Saving the resulting V executable in `./v2`')
			println('Use `v -o v cmd/v` if you want to replace current ' + 'V executable.')
			p.out_name = 'v2'
		}
	}
	rpath_name := os.file_name(rpath)
	p.building_v = !p.is_repl && (rpath_name == 'v' || rpath_name == 'vfmt.v')
	if p.os == ._auto {
		// No OS specifed? Use current system
		p.os = get_host_os()
	}
	//
	p.try_to_use_tcc_by_default()
	if p.ccompiler == '' {
		p.default_c_compiler()
	}
	p.find_cc_if_cross_compiling()
	p.ccompiler_type = cc_from_string(p.ccompiler)
	p.is_test = p.path.ends_with('_test.v') || p.path.ends_with('_test.vv')
		|| p.path.all_before_last('.v').all_before_last('.').ends_with('_test')
	p.is_vsh = p.path.ends_with('.vsh')
	p.is_script = p.is_vsh || p.path.ends_with('.v') || p.path.ends_with('.vv')
	if p.third_party_option == '' {
		p.third_party_option = p.cflags
		$if !windows {
			if !p.third_party_option.contains('-fPIC') {
				p.third_party_option += ' -fPIC'
			}
		}
	}
	// Prepare the cache manager. All options that can affect the generated cached .c files
	// should go into res.cache_manager.vopts, which is used as a salt for the cache hash.
	vhash := @VHASH
	p.cache_manager = vcache.new_cache_manager([
		vhash,
		// ensure that different v versions use separate build artefacts
		'$p.backend | $p.os | $p.ccompiler | $p.is_prod | $p.sanitize',
		p.cflags.trim_space(),
		p.third_party_option.trim_space(),
		p.compile_defines_all.str(),
		p.compile_defines.str(),
		p.lookup_path.str(),
	])
	// eprintln('prefs.cache_manager: $p')
	// disable use_cache for specific cases:
	if os.user_os() == 'windows' {
		p.use_cache = false
	}
	if p.build_mode == .build_module {
		// eprintln('-usecache and build-module flags are not compatible')
		p.use_cache = false
	}
	if p.is_shared {
		// eprintln('-usecache and -shared flags are not compatible')
		p.use_cache = false
	}
	if p.bare_builtin_dir == '' && p.os == .wasm32 {
		p.bare_builtin_dir = os.join_path(p.vroot, 'vlib', 'builtin', 'wasm_bare')
	} else if p.bare_builtin_dir == '' {
		p.bare_builtin_dir = os.join_path(p.vroot, 'vlib', 'builtin', 'linux_bare')
	}

	$if prealloc {
		if !p.no_parallel {
			eprintln('disabling parallel cgen, since V was built with -prealloc')
		}
		p.no_parallel = true
	}
}

fn (mut p Preferences) find_cc_if_cross_compiling() {
	if p.os == get_host_os() {
		return
	}
	if p.os == .windows && p.ccompiler == 'msvc' {
		// Allow for explicit overrides like `v -showcc -cc msvc -os windows file.v`,
		// this makes flag passing more easily debuggable on other OSes too, not only
		// on windows (building will stop later, when -showcc already could display all
		// options).
		return
	}
	p.ccompiler = p.vcross_compiler_name()
}

fn (mut p Preferences) try_to_use_tcc_by_default() {
	if p.ccompiler == 'tcc' {
		p.ccompiler = default_tcc_compiler()
		return
	}
	if p.ccompiler == '' {
		// tcc is known to fail several tests on macos, so do not
		// try to use it by default, only when it is explicitly set
		$if macos {
			return
		}
		// use an optimizing compiler (i.e. gcc or clang) on -prod mode
		if p.is_prod {
			return
		}
		p.ccompiler = default_tcc_compiler()
		return
	}
}

pub fn default_tcc_compiler() string {
	vexe := vexe_path()
	vroot := os.dir(vexe)
	vtccexe := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if os.exists(vtccexe) {
		return vtccexe
	}
	return ''
}

pub fn (mut p Preferences) default_c_compiler() {
	// fast_clang := '/usr/local/Cellar/llvm/8.0.0/bin/clang'
	// if os.exists(fast_clang) {
	// return fast_clang
	// }
	// TODO fix $if after 'string'
	$if windows {
		p.ccompiler = 'gcc'
		return
	}
	if p.os == .ios {
		$if !ios {
			ios_sdk := if p.is_ios_simulator { 'iphonesimulator' } else { 'iphoneos' }
			ios_sdk_path_res := os.execute_or_exit('xcrun --sdk $ios_sdk --show-sdk-path')
			mut isysroot := ios_sdk_path_res.output.replace('\n', '')
			arch := if p.is_ios_simulator {
				'-arch x86_64 -arch arm64'
			} else {
				'-arch armv7 -arch armv7s -arch arm64'
			}
			// On macOS, /usr/bin/cc is a hardlink/wrapper for xcrun. clang on darwin hosts
			// will automatically change the build target based off of the selected sdk, making xcrun -sdk iphoneos pointless
			p.ccompiler = '/usr/bin/cc'
			p.cflags = '-isysroot $isysroot $arch' + p.cflags
			return
		}
	}
	p.ccompiler = 'cc'
	return
}

pub fn vexe_path() string {
	vexe := os.getenv('VEXE')
	if vexe != '' {
		return vexe
	}
	myexe := os.executable()
	mut real_vexe_path := myexe
	for {
		$if tinyc {
			$if x32 {
				// TODO: investigate why exactly tcc32 segfaults on os.real_path here,
				// and remove this cludge.
				break
			}
		}
		real_vexe_path = os.real_path(real_vexe_path)
		break
	}
	os.setenv('VEXE', real_vexe_path, true)
	return real_vexe_path
}

pub fn (p &Preferences) vcross_compiler_name() string {
	vccname := os.getenv('VCROSS_COMPILER_NAME')
	if vccname != '' {
		return vccname
	}
	if p.os == .windows {
		if p.m64 {
			return 'x86_64-w64-mingw32-gcc'
		}
		return 'i686-w64-mingw32-gcc'
	}
	if p.os == .linux {
		return 'clang'
	}
	if p.backend == .c && !p.out_name.ends_with('.c') {
		eprintln('Note: V can only cross compile to windows and linux for now by default.')
		eprintln('It will use `cc` as a cross compiler for now, although that will probably fail.')
		eprintln('Set `VCROSS_COMPILER_NAME` to the name of your cross compiler, for your target OS: $p.os .')
	}
	return 'cc'
}
