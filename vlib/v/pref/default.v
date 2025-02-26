// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os
import v.vcache

pub const default_module_path = os.vmodules_dir()

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
	p.vlib = os.join_path(p.vroot, 'vlib')
	p.vmodules_paths = os.vmodules_paths()

	if p.lookup_path.len == 0 {
		p.lookup_path = ['@vlib', '@vmodules']
	}
	mut expanded_paths := []string{}
	for path in p.lookup_path {
		match path {
			'@vlib' { expanded_paths << p.vlib }
			'@vmodules' { expanded_paths << p.vmodules_paths }
			else { expanded_paths << path.replace('@vroot', p.vroot) }
		}
	}
	p.lookup_path = expanded_paths
}

fn (mut p Preferences) expand_exclude_paths() {
	mut res := []string{}
	static_replacement_list := ['@vroot', p.vroot, '@vlib', p.vlib]
	for x in p.exclude {
		y := x.replace_each(static_replacement_list)
		if y.contains('@vmodules') {
			// @vmodules is a list of paths, each of which should be expanded in the complete exclusion list:
			for vmp in p.vmodules_paths {
				res << y.replace('@vmodules', vmp)
			}
			continue
		}
		res << y
	}
	p.exclude = res
}

fn (mut p Preferences) setup_os_and_arch_when_not_explicitly_set() {
	if p.os == .wasm32_emscripten {
		// TODO: remove after `$if wasm32_emscripten {` works
		p.parse_define('emscripten')
	}
	host_os := if p.backend == .wasm { OS.wasi } else { get_host_os() }
	if p.os == ._auto {
		p.os = host_os
		p.build_options << '-os ${host_os.lower()}'
	}

	if !p.output_cross_c {
		if p.os != host_os {
			// TODO: generalise this not only for macos->linux, after considering the consequences for vab/Android:
			if host_os == .macos && p.os == .linux {
				// Cross compilation from macos -> linux; assume AMD64 as the target architecture for now
				if p.arch == ._auto {
					p.arch = .amd64
					p.build_options << '-arch amd64'
				}
				p.parse_define('use_bundled_libgc')
			}
		}
	}
	if p.arch == ._auto {
		p.arch = get_host_arch()
		p.build_options << '-arch ${p.arch}'
	}
}

pub fn (mut p Preferences) defines_map_unique_keys() string {
	mut defines_map := map[string]bool{}
	for d in p.compile_defines {
		defines_map[d] = true
	}
	for d in p.compile_defines_all {
		defines_map[d] = true
	}
	keys := defines_map.keys()
	skeys := keys.sorted()
	return skeys.join(',')
}

pub fn (mut p Preferences) fill_with_defaults() {
	p.setup_os_and_arch_when_not_explicitly_set()
	p.expand_lookup_paths()
	p.expand_exclude_paths()
	rpath := os.real_path(p.path)
	if p.out_name == '' {
		filename := os.file_name(rpath).trim_space()
		mut base := filename.all_before_last('.')
		if os.file_ext(base) in ['.c', '.js', '.wasm'] {
			base = base.all_before_last('.')
		}
		if base == '' {
			// The file name is just `.v` or `.vsh` or `.*`
			base = filename
		}
		target_dir := if os.is_dir(rpath) { rpath } else { os.dir(rpath) }
		if p.raw_vsh_tmp_prefix != '' {
			p.out_name = os.join_path(target_dir, p.raw_vsh_tmp_prefix + '.' + base)
		} else {
			p.out_name = os.join_path(target_dir, base)
		}
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
		// extensively, and make a PR about it, instead of committing directly
		// and breaking the CI, VC, and users doing `v up`.
		if rpath == '${p.vroot}/cmd/v' && os.is_dir('vlib/compiler') {
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
	if p.os == .linux {
		$if !linux {
			p.parse_define('cross_compile')
		}
	}
	if p.output_cross_c {
		// avoid linking any GC related code, since the target may not have an usable GC system
		p.gc_mode = .no_gc
		p.use_cache = false
		p.skip_unused = false
		p.parse_define('no_backtrace') // the target may not have usable backtrace() and backtrace_symbols()
		p.parse_define('cross') // TODO: remove when `$if cross {` works
	}
	if p.gc_mode == .unknown {
		if p.backend != .c || p.building_v || p.is_bare || p.ccompiler == 'msvc' {
			p.gc_mode = .no_gc
			p.build_options << ['-gc', 'none']
		} else {
			// enable the GC by default
			p.gc_mode = .boehm_full_opt
			// NOTE: these are added to p.compile_defines[_all]
			// more than once when building modules for usecache
			p.parse_define('gcboehm')
			p.parse_define('gcboehm_full')
			p.parse_define('gcboehm_opt')
		}
	}
	if p.is_debug {
		p.parse_define('debug')
	}
	p.try_to_use_tcc_by_default()
	if p.ccompiler == '' {
		p.default_c_compiler()
	}
	if p.cppcompiler == '' {
		p.default_cpp_compiler()
	}
	p.find_cc_if_cross_compiling()
	p.ccompiler_type = cc_from_string(p.ccompiler)
	p.is_test = p.path.ends_with('_test.v') || p.path.ends_with('_test.vv')
		|| p.path.all_before_last('.v').all_before_last('.').ends_with('_test')
	p.is_vsh = p.path.ends_with('.vsh') || p.raw_vsh_tmp_prefix != ''
	p.is_script = p.is_vsh || p.path.ends_with('.v') || p.path.ends_with('.vv')
	if p.third_party_option == '' {
		p.third_party_option = p.cflags
		$if !windows {
			if !p.third_party_option.contains('-fPIC') {
				p.third_party_option += ' -fPIC'
			}
		}
	}

	final_os := p.os.lower()
	p.parse_define(final_os)

	// Prepare the cache manager. All options that can affect the generated cached .c files
	// should go into res.cache_manager.vopts, which is used as a salt for the cache hash.
	vhash := @VHASH
	p.cache_manager = vcache.new_cache_manager([
		vhash,
		// ensure that different v versions use separate build artefacts
		'${p.backend} | ${final_os} | ${p.ccompiler} | ${p.is_prod} | ${p.sanitize}',
		p.defines_map_unique_keys(),
		p.cflags.trim_space(),
		p.third_party_option.trim_space(),
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
		if !p.no_parallel && p.is_verbose {
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
	// TODO: fix $if after 'string'
	$if windows {
		p.ccompiler = 'gcc'
		return
	}
	if p.os == .ios {
		$if !ios {
			ios_sdk := if p.is_ios_simulator { 'iphonesimulator' } else { 'iphoneos' }
			ios_sdk_path_res := os.execute_or_exit('xcrun --sdk ${ios_sdk} --show-sdk-path')
			mut isysroot := ios_sdk_path_res.output.replace('\n', '')
			arch := if p.is_ios_simulator {
				'-arch x86_64 -arch arm64'
			} else {
				'-arch armv7 -arch armv7s -arch arm64'
			}
			// On macOS, /usr/bin/cc is a hardlink/wrapper for xcrun. clang on darwin hosts
			// will automatically change the build target based off of the selected sdk, making xcrun -sdk iphoneos pointless
			p.ccompiler = '/usr/bin/cc'
			p.cflags = '-isysroot ${isysroot} ${arch}' + p.cflags
			return
		}
	}
	p.ccompiler = 'cc'
	return
}

pub fn (mut p Preferences) default_cpp_compiler() {
	if p.ccompiler.contains('clang') {
		p.cppcompiler = 'clang++'
		return
	}
	p.cppcompiler = 'c++'
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

pub fn (p &Preferences) vcross_linker_name() string {
	vlname := os.getenv('VCROSS_LINKER_NAME')
	if vlname != '' {
		return vlname
	}
	$if macos {
		return '/opt/homebrew/opt/llvm/bin/ld.lld'
	}
	$if windows {
		return 'ld.lld.exe'
	}
	return 'ld.lld'
}

pub fn (p &Preferences) vcross_compiler_name() string {
	vccname := os.getenv('VCROSS_COMPILER_NAME')
	if vccname != '' {
		return vccname
	}
	if p.os == .windows {
		if p.os == .freebsd {
			return 'clang'
		}
		if p.m64 {
			return 'x86_64-w64-mingw32-gcc'
		}
		return 'i686-w64-mingw32-gcc'
	}
	if p.os == .linux {
		return 'clang'
	}
	if p.os == .freebsd {
		return 'clang'
	}
	if p.os == .wasm32_emscripten {
		if os.user_os() == 'windows' {
			return 'emcc.bat'
		}
		return 'emcc'
	}
	if p.backend == .c && !p.out_name.ends_with('.c') {
		eprintln('Note: V can only cross compile to Windows and Linux for now by default.')
		eprintln('It will use `cc` as a cross compiler for now, although that will probably fail.')
		eprintln('Set `VCROSS_COMPILER_NAME` to the name of your cross compiler, for your target OS: ${p.os} .')
	}
	return 'cc'
}

// vroot_file reads the given file, given a path relative to @VROOT .
// Its goal is to give all backends a shared infrastructure to read their own static preludes (like C headers etc),
// without each having to implement their own way of lookup/embedding/caching them.
pub fn (mut p Preferences) vroot_file(path string) string {
	full_path := os.join_path(p.vroot, path)
	return os.read_file(full_path) or { '/* missing vroot content of path: ${full_path} */' }
}
