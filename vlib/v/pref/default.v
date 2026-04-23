// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os
import strings
import v.vcache

pub const default_module_path = os.vmodules_dir()

pub fn new_preferences() &Preferences {
	mut p := &Preferences{}
	p.fill_with_defaults()
	return p
}

const windows_default_gc_defines = ['gcboehm', 'gcboehm_full', 'gcboehm_incr', 'gcboehm_opt',
	'gcboehm_leak', 'vgc']

fn (p &Preferences) default_thread_stack_size() int {
	return match p.arch {
		.arm32, .rv32, .i386, .ppc, .wasm32 { 2 * 1024 * 1024 }
		else { 8 * 1024 * 1024 }
	}
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

	if !p.output_cross_c && p.os != host_os {
		if p.os == .linux && p.arch == ._auto {
			// The bundled linuxroot sysroot currently only contains x86_64 runtime files.
			p.set_default_arch(.amd64)
		}
		// TODO: generalise this not only for macos->linux, after considering the consequences for vab/Android:
		if host_os == .macos && p.os == .linux {
			p.parse_define('use_bundled_libgc')
		}
	}
}

fn (mut p Preferences) set_default_arch(arch Arch) {
	if p.arch != ._auto || arch == ._auto {
		return
	}
	p.arch = arch
	p.build_options << '-arch ${arch}'
}

fn arch_from_ccompiler_name(ccompiler string) Arch {
	name := os.file_name(ccompiler).to_lower_ascii()
	if name.contains('x86_64') || name.contains('amd64') {
		return .amd64
	}
	if name.contains('aarch64') || name.contains('arm64-v8a') || name.contains('arm64') {
		return .arm64
	}
	if name.contains('armeabi-v7a') || name.contains('armv7')
		|| name.contains('arm-linux-androideabi') || name.contains('arm32') {
		return .arm32
	}
	if name.contains('riscv64') {
		return .rv64
	}
	if name.contains('riscv32') {
		return .rv32
	}
	if name.contains('i686') || name.contains('i386') || name.contains('x86') {
		return .i386
	}
	if name.contains('s390x') {
		return .s390x
	}
	if name.contains('ppc64le') {
		return .ppc64le
	}
	if name.contains('loongarch64') {
		return .loongarch64
	}
	if name.contains('sparc64') {
		return .sparc64
	}
	if name.contains('ppc64') {
		return .ppc64
	}
	return ._auto
}

fn (mut p Preferences) resolve_default_arch() {
	if p.arch != ._auto {
		return
	}
	host_os := if p.backend == .wasm { OS.wasi } else { get_host_os() }
	if p.os != host_os {
		p.set_default_arch(arch_from_ccompiler_name(p.ccompiler))
	}
	p.set_default_arch(get_host_arch())
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

fn (mut p Preferences) disable_tcc_shared_backtraces() {
	if p.is_shared && p.ccompiler_type == .tinyc && 'no_backtrace' !in p.compile_defines_all {
		// TCC shared libraries should not depend on TCC's backtrace runtime symbols.
		p.parse_define('no_backtrace')
	}
}

// fill_with_defaults initializes unset preferences and derives build options from them.
pub fn (mut p Preferences) fill_with_defaults() {
	p.setup_os_and_arch_when_not_explicitly_set()
	p.expand_lookup_paths()
	p.expand_exclude_paths()
	rpath := os.real_path(p.path)
	if p.out_name == '' {
		target_dir := if os.is_dir(rpath) { rpath } else { os.dir(rpath) }
		p.out_name = os.join_path(target_dir, p.default_output_name(rpath))
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
	} else if p.out_name_is_dir {
		p.out_name = os.join_path(p.out_name, p.default_output_name(rpath))
	}
	npath := rpath.replace('\\', '/')
	p.building_v = !p.is_repl && (npath.ends_with('cmd/v') || npath.ends_with('cmd/tools/vfmt.v'))
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
		if p.backend != .c || p.building_v || p.is_bare || p.os == .windows || p.is_musl {
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
	p.resolve_default_arch()
	if !p.thread_stack_size_set_by_flag {
		p.thread_stack_size = p.default_thread_stack_size()
	}
	p.ccompiler_type = cc_from_string(p.ccompiler)
	p.normalize_gc_defaults_for_resolved_ccompiler()
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
	// eprintln('prefs.cache_manager: ${p}')
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

// normalize_gc_defaults_for_resolved_ccompiler clears stale compiler-dependent
// defaults after the effective C compiler has been resolved.
pub fn (mut p Preferences) normalize_gc_defaults_for_resolved_ccompiler() {
	p.disable_tcc_shared_backtraces()
	if p.os != .windows || p.ccompiler_type != .msvc || p.gc_set_by_flag {
		return
	}
	p.gc_mode = .no_gc
	p.compile_defines = p.compile_defines.filter(it !in windows_default_gc_defines)
	p.compile_defines_all = p.compile_defines_all.filter(it !in windows_default_gc_defines)
	for define in windows_default_gc_defines {
		p.compile_values.delete(define)
	}
	mut build_options := []string{cap: p.build_options.len + 2}
	mut i := 0
	for i < p.build_options.len {
		option := p.build_options[i]
		if option == '-gc' {
			i += 2
			continue
		}
		if option.starts_with('-d ') {
			define := option[3..].all_before('=')
			if define in windows_default_gc_defines {
				i++
				continue
			}
		}
		build_options << option
		i++
	}
	build_options << ['-gc', 'none']
	p.build_options = build_options
}

fn (p &Preferences) default_output_name(rpath string) string {
	filename := os.file_name(rpath).trim_space()
	mut base := filename.all_before_last('.')
	if os.file_ext(base) in ['.c', '.js', '.wasm'] {
		base = base.all_before_last('.')
	}
	if base == '' {
		// The file name is just `.v` or `.vsh` or `.*`
		base = filename
	}
	if needs_safe_default_output_name(base, filename, rpath) {
		base = safe_default_output_name(filename)
	}
	if p.raw_vsh_tmp_prefix != '' {
		return p.raw_vsh_tmp_prefix + '.' + base
	}
	return base
}

fn needs_safe_default_output_name(base string, filename string, rpath string) bool {
	if base == '' || base in ['.', '..', '-'] {
		return true
	}
	if base == filename && filename.starts_with('.') && !os.is_dir(rpath) {
		return true
	}
	if base.ends_with('.c') || base.ends_with('.js') || base.ends_with('.wasm') {
		return true
	}
	for ch in base {
		if ch < ` ` || ch == 127 {
			return true
		}
	}
	return false
}

fn safe_default_output_name(filename string) string {
	mut sanitized := strings.new_builder(filename.len + 4)
	for ch in filename {
		if ch < ` ` || ch == 127 {
			sanitized.write_u8(`_`)
		} else {
			sanitized.write_u8(ch)
		}
	}
	sanitized.write_string('.out')
	return sanitized.str()
}

fn (mut p Preferences) find_cc_if_cross_compiling() {
	if p.os == get_host_os() {
		return
	}
	if p.ccompiler_set_by_flag {
		// Only mingw compilers can cross-compile for Windows (others lack Windows headers),
		// so override any non-mingw compiler with the proper cross-compiler.
		if p.os == .windows && !p.ccompiler.contains('mingw') {
			p.ccompiler = p.vcross_compiler_name()
			return
		}
		// Respect explicit `-cc` selection even in cross-compilation mode.
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
	preferred_tcc := default_tcc_compiler()
	if p.ccompiler == 'tcc' {
		p.ccompiler = if preferred_tcc != '' { preferred_tcc } else { 'tcc' }
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
		p.ccompiler = preferred_tcc
		return
	}
}

fn usable_system_tcc_compiler() string {
	if get_host_os() != .termux {
		return ''
	}
	system_tcc := os.find_abs_path_of_executable('tcc') or { return '' }
	tcc_probe := os.execute('${os.quoted_path(system_tcc)} -v')
	if tcc_probe.exit_code != 0 {
		return ''
	}
	return system_tcc
}

fn usable_bundled_tcc_compiler(vroot string) string {
	vtccexe := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(vtccexe) || !os.is_executable(vtccexe) {
		return ''
	}
	// Unsupported hosts can still have a placeholder `thirdparty/tcc/` checkout.
	tcc_probe := os.execute('${os.quoted_path(vtccexe)} -v')
	if tcc_probe.exit_code != 0 {
		return ''
	}
	return vtccexe
}

// default_tcc_compiler returns the preferred TinyCC path when it exists and works on the host.
pub fn default_tcc_compiler() string {
	vexe := vexe_path()
	vroot := os.dir(vexe)
	bundled_tcc := usable_bundled_tcc_compiler(vroot)
	if bundled_tcc != '' {
		return bundled_tcc
	}
	return usable_system_tcc_compiler()
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

// vroot_file reads the given file, given a path relative to @VEXEROOT .
// Its goal is to give all backends a shared infrastructure to read their own static preludes (like C headers etc),
// without each having to implement their own way of lookup/embedding/caching them.
pub fn (mut p Preferences) vroot_file(path string) string {
	full_path := os.join_path(p.vroot, path)
	return os.read_file(full_path) or { '/* missing @VEXEROOT content of path: ${full_path} */' }
}
