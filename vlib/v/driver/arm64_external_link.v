module driver

import os
import v.cmdexec
import v.flat
import v.gen.c as cgen
import v.modulecache
import v.pref
import v.tempname

// External linking for the native arm64 backend on macOS.
//
// The built-in Mach-O linker (vlib/v/gen/arm64/linker.v) links one object
// against libSystem, libobjc and Metal only. That is enough, and fast, for
// programs that call libc. A program that needs C code (sokol, freetype, stb,
// Objective-C embedders, `.c`/`.m` sources), static archives, other dylibs,
// frameworks or the Boehm collector is linked the way Go links cgo programs
// with `-linkmode=external`:
//
// 1. the generator writes a relocatable Mach-O object (Gen.write_object);
// 2. the C side of the program (its `#define`s, `#include`s and `#insert`s) is
//    compiled by the C compiler into one support object, in the same order and
//    with the same `#flag`s the C backend would use;
// 3. `cc` links both objects with the program's libraries, archives and
//    frameworks, and the system linker (ld-prime, or ld64.lld) does the rest:
//    archive member selection, dylib binding, stubs, unwind info and signing.

// Arm64LinkMode is the linker the arm64 backend uses for an executable.
enum Arm64LinkMode {
	internal // built-in Mach-O linker, libSystem/libobjc/Metal only
	external // system linker through `cc`
}

// Libraries that libSystem already provides on macOS; asking for them does not
// need the system linker.
const arm64_libsystem_libraries = ['-lm', '-lc', '-lpthread', '-ldl', '-lSystem', '-lrt']

// arm64_link_mode chooses the linker for a program, like Go's
// `-linkmode=auto`: external as soon as the program brings its own native code
// or libraries. `-d arm64_internal_link` and `-d arm64_external_link` force a
// mode (the first one fails at link time when the program needs C).
fn arm64_link_mode(a &flat.FlatAst, directive_flags []string, user_defines []string) (Arm64LinkMode, string) {
	if 'arm64_internal_link' in user_defines {
		return .internal, ''
	}
	if 'arm64_external_link' in user_defines {
		return .external, '-d arm64_external_link'
	}
	if 'gcboehm' in user_defines {
		return .external, 'the Boehm collector'
	}
	if source := arm64_native_source_include(a) {
		return .external, '`${source}`'
	}
	if header := arm64_project_header_include(a) {
		return .external, '`${header}`'
	}
	mut i := 0
	for i < directive_flags.len {
		flag := directive_flags[i].trim_space()
		if flag in ['-framework', '-l', '-L', '-F', '-weak_framework', '-Xlinker'] {
			if flag == '-l' && i + 1 < directive_flags.len
				&& '-l${directive_flags[i + 1].trim_space()}' in arm64_libsystem_libraries {
				i += 2
				continue
			}
			if flag in ['-framework', '-l', '-weak_framework'] {
				next := if i + 1 < directive_flags.len { directive_flags[i + 1] } else { '' }
				return .external, '`${flag} ${next}`'
			}
			i += 2
			continue
		}
		if flag in arm64_libsystem_libraries || flag.starts_with('-L') || flag.starts_with('-F') {
			i++
			continue
		}
		if flag.starts_with('-framework') || flag.starts_with('-l') || flag.starts_with('-Wl,')
			|| c_flag_is_object_file(flag) || c_flag_token_is_link_only(flag) {
			return .external, '`${flag}`'
		}
		i++
	}
	return .internal, ''
}

// arm64_native_source_include returns the first C/Objective-C/assembly source
// that the program includes into its C side.
fn arm64_native_source_include(a &flat.FlatAst) ?string {
	for node in a.nodes {
		if node.kind != .directive || node.value !in ['include', 'insert', 'preinclude', 'postinclude'] {
			continue
		}
		path := node.typ_text().trim_space().trim('"<>')
		ext := os.file_ext(path)
		if ext in ['.c', '.cc', '.cpp', '.cxx', '.C', '.m', '.mm', '.s', '.S'] {
			return path
		}
	}
	return none
}

// arm64_project_header_include returns the first header of the program or of
// its modules (a quoted `#include`, usually `@VMODROOT/...`). Its `static
// inline` functions and macros only exist in the C side, so the program needs
// the system linker even without a C source or a library. System headers
// (`<stdio.h>`) and V's own vlib headers stay on the built-in linker.
fn arm64_project_header_include(a &flat.FlatAst) ?string {
	for node in a.nodes {
		if node.kind != .directive || node.value != 'include' {
			continue
		}
		mut raw := node.typ_text().trim_space()
		if !raw.starts_with('"') && !raw.starts_with('<') {
			// A platform prefix: `darwin "@VMODROOT/x.h"`.
			raw = raw.all_after(' ').trim_space()
		}
		if !raw.starts_with('"') {
			continue
		}
		path := raw.trim('"')
		if path.starts_with('@VEXEROOT/vlib/') || path.starts_with('@VMODROOT/vlib/') {
			continue
		}
		return path
	}
	return none
}

// NativeCSupportSource is the C translation unit that holds the C side of a
// program compiled by a native backend.
struct NativeCSupportSource {
	text         string
	include_dirs []string
	// has_insert reports whether the unit includes files of the program itself
	// (`#insert`, or a quoted `#include` that names an existing file).
	has_insert bool
}

// NativeCDirective is one C preprocessor line of the program's C side.
struct NativeCDirective {
	text          string
	before_import bool
}

// native_c_support_source collects the program's C preprocessor directives:
// the same `#define`, `#undef`, `#include` and `#insert` lines that the C
// backend writes at the top of its translation unit, resolved for `target` (OS
// prefixes such as `#include linux <sys/timerfd.h>`, `@VEXEROOT`, `$d()`), and
// in its order: a module's imports come before the module's own lines after
// its imports (cgen.FlatGen.ordered_c_directives), so `sokol.sgl`'s
// `util/sokol_gl.h` follows `sokol.gfx`'s `sokol_gfx.h` even when the program
// imports `sokol.sgl` first.
fn native_c_support_source(a &flat.FlatAst, vroot string, target pref.Target, compile_values map[string]string) NativeCSupportSource {
	mut include_dirs := []string{}
	mut include_seen := map[string]bool{}
	mut cur_file := ''
	mut cur_module := 'main'
	mut before_import := true
	mut has_insert := false
	mut by_module := map[string][]NativeCDirective{}
	mut module_order := []string{}
	mut module_imports := map[string][]string{}
	for node in a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = 'main'
				before_import = true
				continue
			}
			.module_decl {
				cur_module = node.value
				continue
			}
			.import_decl {
				before_import = false
				dep := node.value
				if dep.len > 0 {
					mut deps := module_imports[cur_module] or { []string{} }
					if dep !in deps {
						deps << dep
						module_imports[cur_module] = deps
					}
				}
				continue
			}
			.directive {}
			else {
				continue
			}
		}
		raw := flat.source_type_name(node.typ).trim_space()
		if raw.len == 0 {
			continue
		}
		mut line := ''
		match node.value {
			'define', 'undef' {
				arg := cgen.native_directive_arg(raw, target) or { continue }
				if arg.len > 0 {
					line = '#${node.value} ${arg}'
				}
			}
			'include', 'preinclude', 'postinclude', 'insert' {
				include_arg := cgen.native_include_arg(raw, vroot, cur_file, target,
					compile_values)
				if include_arg.len == 0 {
					continue
				}
				if include_arg.starts_with('<') {
					line = '#include ${include_arg}'
				} else {
					path := x64_resolve_insert_path(include_arg, cur_file, vroot)
					if path.len > 0 && os.is_file(path) {
						if node.value == 'insert' {
							dir := os.dir(path)
							if dir.len > 0 && dir !in include_seen {
								include_seen[dir] = true
								include_dirs << dir
							}
						}
						line = '#include "${path}"'
						has_insert = true
					} else if node.value != 'insert' {
						line = '#include ${include_arg}'
					}
				}
			}
			else {}
		}
		if line.len == 0 {
			continue
		}
		if cur_module !in by_module {
			by_module[cur_module] = []NativeCDirective{}
			module_order << cur_module
		}
		by_module[cur_module] << NativeCDirective{
			text:          line
			before_import: before_import
		}
	}
	// Inlined headers can expose atomic and pointer-width helpers without
	// carrying the standard fixed-width integer declarations they depend on.
	mut lines := ['#include <stdint.h>']
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for mod in module_order {
		native_c_visit_directive_module(mod, by_module, module_imports, mut visiting, mut
			visited, mut lines)
	}
	return NativeCSupportSource{
		text:         lines.join('\n') + '\n'
		include_dirs: include_dirs
		has_insert:   has_insert
	}
}

fn native_c_visit_directive_module(mod string, by_module map[string][]NativeCDirective, module_imports map[string][]string, mut visiting map[string]bool, mut visited map[string]bool, mut lines []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	directives := by_module[mod] or { []NativeCDirective{} }
	for directive in directives {
		if directive.before_import {
			lines << directive.text
		}
	}
	// Modules without directives are traversed too, so a transitive import's
	// lines still come before an importer's.
	for dep in module_imports[mod] or { []string{} } {
		native_c_visit_directive_module(dep, by_module, module_imports, mut visiting, mut
			visited, mut lines)
	}
	visiting.delete(mod)
	visited[mod] = true
	for directive in directives {
		if !directive.before_import {
			lines << directive.text
		}
	}
}

// Arm64CSideOptions holds what the C side of an arm64 program needs from the
// driver.
struct Arm64CSideOptions {
	no_cache           bool
	a                  &flat.FlatAst     = unsafe { nil }
	prefs              &pref.Preferences = unsafe { nil }
	user_c_flags       []string
	environment_cflags []string
	optimization_flags []string
	c_compiler         string
	compiler_explicit  bool
}

// Arm64CSide is the C side of an arm64 program: its support unit and the
// command that compiles it.
struct Arm64CSide {
mut:
	compiler     string
	target_args  []string
	flags        []string // the user's and the program's `#flag`s
	support      NativeCSupportSource
	compile_args []string // without the input and output
	target       pref.Target
	no_cache     bool
	wrappers     string // C wrappers for macros and static functions (arm64.Gen.c_function_wrappers)
}

// arm64_c_compiler returns the compiler of the C side: TinyCC cannot produce
// arm64 Mach-O objects for the system linker. It goes by the name, as
// pref.ccompiler_is_tinycc would start the compiler on every build.
fn arm64_c_compiler(c_compiler string) string {
	name := os.file_name(c_compiler).to_lower_ascii()
	return if name == '' || name == 'tinyc' || name.contains('tcc') { 'cc' } else { c_compiler }
}

fn arm64_c_side(opts Arm64CSideOptions) !Arm64CSide {
	prefs := opts.prefs
	compiler := arm64_c_compiler(opts.c_compiler)
	mut target_args := c_compiler_target_args(prefs.target, compiler, opts.compiler_explicit, '')!
	mut flags := opts.user_c_flags.clone()
	// The object says macOS 11.0, the first release for Apple silicon. Odin sets
	// the same floor explicitly (src/linker.cpp); without it the executable would
	// require the build machine's macOS.
	if prefs.target.os == 'macos' && !flags.any(it.contains('mmacosx-version-min')) {
		target_args << '-mmacosx-version-min=11.0'
	}
	flags << cgen.cache_directive_flags(opts.a, prefs.vroot, prefs.target, prefs.compile_values)
	support := native_c_support_source(opts.a, prefs.vroot, prefs.target, prefs.compile_values)
	mut args := []string{}
	args << target_args
	args << ['-w', '-fwrapv', '-fno-common']
	args << opts.optimization_flags
	args << opts.environment_cflags
	for dir in support.include_dirs {
		args << '-I${dir}'
	}
	args << x64_native_compile_flags(flags)
	// The C side is Objective-C when the C backend's unit would be: for
	// `-fobjc-arc` and friends in the program's flags (sokol's Metal backend), or
	// for Objective-C inputs (vlib/macos/objc_bridge.h, AppKit embedders).
	mut objective_c := c_flags_need_objective_c(flags)
		|| cgen.generated_c_unit_needs_objective_c(opts.a, prefs.vroot, flags, prefs.c99, prefs.ccompiler, prefs.target)
	if !objective_c && prefs.target.os == 'macos' {
		// A header can be Objective-C on its own (vlib/macos/objc_bridge.h uses
		// `__bridge` casts). Only the compiler can tell: when the unit does not
		// compile as C, it is compiled as Objective-C, a superset of C.
		objective_c = arm64_c_side_is_objective_c(compiler, args, support.text, prefs.target,
			opts.no_cache)
	}
	if objective_c {
		args << ['-x', 'objective-c']
	} else {
		args << '-std=gnu11'
	}
	return Arm64CSide{
		compiler:     compiler
		target_args:  target_args
		flags:        flags
		support:      support
		compile_args: args
		target:       prefs.target
		no_cache:     opts.no_cache
	}
}

// Caching the C side. The compiler parses the program's headers (sokol, AppKit)
// for every probe and for the support object, which is most of a rebuild. Like
// Go's action cache (cmd/go/internal/work/exec.go, buildActionID) and Zig's
// manifests (lib/std/Build/Cache.zig), a result is keyed by compiler, arguments,
// text and the signatures of every header the unit includes. The bookkeeping is
// the driver's cache for C objects; only the unit's name and text are ours.
const arm64_c_cache_directory = 'v3_thirdparty_objs'
const arm64_c_object_cache_stats = &CObjectCacheStats{}

// One record per process: it remembers `cc --version` and file signatures.
fn arm64_cache_stats() &CObjectCacheStats {
	return arm64_c_object_cache_stats
}

fn arm64_cache_dir() string {
	return os.join_path(os.vtmp_dir(), arm64_c_cache_directory)
}

fn arm64_text_hash(parts ...string) string {
	mut hash := u64(1469598103934665603)
	for part in parts {
		hash = c_hash_bytes(hash, part.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

// arm64_cached_unit_paths returns the source and object paths of a C unit,
// writing the source the first time it is seen.
fn arm64_cached_unit_paths(text string) !(string, string) {
	dir := arm64_cache_dir()
	ensure_native_object_cache_dir(dir)!
	name := 'arm64_unit_${arm64_text_hash(text)}'
	source := os.join_path(dir, '${name}.c')
	if !os.is_file(source) {
		staged := '${source}.tmp.${tempname.unique_token()}'
		os.write_file(staged, text)!
		publish_native_c_object(staged, source)!
	}
	return source, os.join_path(dir, '${name}.o')
}

// Arm64CInputs are the files that the C units of an arm64 build read and the
// paths where a file would be read instead, as the compiler reported them for
// each unit: its `-MD` list on a compile, the manifest of that list on a cache
// hit, and what the link of the C compiler driver reads (add_link). The receipt
// of the build lists them (Arm64CInputs.native), as Zig's whole cache manifest
// takes in the depfile of every C object (Compilation.updateCObject): V opens no
// header for it. `unknown` says why the inputs of a unit cannot be told, or is ''
// when they can.
struct Arm64CInputs {
mut:
	files   []string
	missing []string
	// The metadata of files of the link from before the link (add_link).
	identities map[string]string
	unknown    string
}

// arm64_c_build_inputs records the C units of the main thread; a thread keeps
// its own record and hands it back (Arm64PreparedLink).
const arm64_c_build_inputs = &Arm64CInputs{}

fn arm64_build_c_inputs() &Arm64CInputs {
	return arm64_c_build_inputs
}

// add_dependencies records what a compile of the generated unit `source` read.
fn (mut inputs Arm64CInputs) add_dependencies(dependencies &CObjectDependencies, source string) {
	if dependencies.used_fallback {
		inputs.fail('the C compiler did not tell what ${os.file_name(source)} read')
		return
	}
	for path in dependencies.files {
		if !arm64_is_generated_unit(path) {
			inputs.files << os.abs_path(path)
		}
	}
	inputs.missing << dependencies.missing
}

// add_manifest records the files and absent paths of the manifest of `object`.
fn (mut inputs Arm64CInputs) add_manifest(manifest string, object string) {
	lines := os.read_lines(manifest) or {
		inputs.fail('the manifest of ${os.file_name(object)} cannot be read')
		return
	}
	// Another build can replace the manifest after it was checked.
	if lines.len < 2 || lines[1] != 'object=${object}' {
		inputs.fail('the manifest of ${os.file_name(object)} changed')
		return
	}
	for line in lines[2..] {
		if line.starts_with('missing=') {
			inputs.missing << line.all_after('missing=')
		} else if line.starts_with('dependency=') {
			path := line.all_after('dependency=').all_before_last('\t')
			if !arm64_is_generated_unit(path) {
				inputs.files << os.abs_path(path)
			}
		}
	}
}

// arm64_is_generated_unit reports whether `path` is the source of a C unit that
// arm64_cached_unit_paths wrote. It is named by its text, which the V sources
// of the program decide, and can be newer than the build.
fn arm64_is_generated_unit(path string) bool {
	return os.file_name(path).starts_with('arm64_unit_') && os.dir(path) == arm64_cache_dir()
}

fn (mut inputs Arm64CInputs) fail(reason string) {
	if inputs.unknown.len == 0 {
		inputs.unknown = reason
	}
}

fn (mut inputs Arm64CInputs) merge(other &Arm64CInputs) {
	inputs.files << other.files
	inputs.missing << other.missing
	for path, identity in other.identities {
		inputs.identities[path] = identity
	}
	if other.unknown.len > 0 {
		inputs.fail(other.unknown)
	}
}

// native returns the inputs of the C side for the receipt of the build
// (v3_unchanged_build_with_native_inputs), which signs them once the build is done:
// a file written since the build began may not be what a compile read, and
// declines the receipt (Zig's isProblematicTimestamp). A header of the program,
// outside the places of the system and outside `vroot`, that can ask for the time
// of the compilation declines it as well (v3_headers_ask_for_time).
fn (inputs &Arm64CInputs) native(vroot string) UnchangedBuildNativeInputs {
	files := arm64_sorted_distinct(inputs.files)
	vroot_real := os.real_path(vroot)
	program_headers := files.filter(os.file_ext(it) !in ['.a', '.o', '.dylib', '.so', '.tbd']
		&& !v3_is_system_header_path(it)
		&& !(vroot.len > 0 && v3_path_is_below(os.real_path(it), vroot_real)))
	if inputs.unknown.len == 0 && v3_headers_ask_for_time('', program_headers) {
		return UnchangedBuildNativeInputs{
			unknown: 'a header of the program can ask for the time of its compilation'
		}
	}
	return UnchangedBuildNativeInputs{
		files:      files
		missing:    arm64_sorted_distinct(inputs.missing)
		identities: inputs.identities.clone()
		unknown:    inputs.unknown
	}
}

// add_link records what the C compiler driver links for the command `args`, as
// the C backend takes it from its link command (v3_program_link_inputs): the
// objects and libraries that it names or finds, and the paths where a library
// would be found first. What is below one of `made_dirs` is made by the build.
fn (mut inputs Arm64CInputs) add_link(args []string, compiler string, target_os string, made_dirs []string) {
	build_dir := os.getwd()
	sdk := if target_os == 'macos' { cmdexec.macos_sdk_root() } else { '' }
	manager := modulecache.Manager{}
	link := v3_program_link_inputs(args, '', v3_default_link_library_dirs(&manager, compiler,
		v3_link_search_args(args), sdk, build_dir), build_dir)
	found := v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{
		told:        true
		link_reason: link.reason()
	}, false, &CObjectCacheStats{}, made_dirs)
	if found.unknown.len > 0 {
		inputs.fail(found.unknown)
		return
	}
	inputs.files << found.files
	inputs.missing << found.missing
	for path, identity in found.identities {
		inputs.identities[path] = identity
	}
}

fn arm64_sorted_distinct(paths []string) []string {
	mut seen := map[string]bool{}
	for path in paths {
		seen[path] = true
	}
	mut result := seen.keys()
	result.sort()
	return result
}

// arm64_cached_object returns the object of a unit when the manifest of its last
// compile still matches every header, without running the compiler.
fn arm64_cached_object(compiler string, args []string, text string, target pref.Target) ?string {
	mut stats := arm64_cache_stats()
	mut inputs := arm64_build_c_inputs()
	return arm64_cached_object_with(compiler, args, text, target, mut stats, mut inputs)
}

fn arm64_cached_object_with(compiler string, args []string, text string, target pref.Target, mut stats CObjectCacheStats, mut inputs Arm64CInputs) ?string {
	_, object := arm64_cached_unit_paths(text) or { return none }
	manifest := c_object_manifest_path(arm64_cache_dir(), object, compiler, args, target, mut stats)
	cached := valid_c_object_manifest(manifest, mut stats)?
	inputs.add_manifest(manifest, cached)
	return cached
}

// arm64_compile_unit compiles a C unit and returns its object, from the cache
// when nothing changed. A failed compile returns the compiler's output. As in
// Zig's updateCObject (src/Compilation.zig: `-MD -MV -MF`, then addDepFilePost),
// the compiler writes the dependency list during the compile, so a miss parses
// the headers once instead of once for `-M` and once for `-c`. A unit compiled
// with `-fsyntax-only` has no object; an empty file stands for its success.
fn arm64_compile_unit(compiler string, args []string, text string, target pref.Target, no_cache bool) !string {
	mut stats := arm64_cache_stats()
	mut inputs := arm64_build_c_inputs()
	return arm64_compile_unit_with(compiler, args, text, target, no_cache, mut stats, mut
		inputs)
}

// arm64_compile_unit_with is arm64_compile_unit with the records of a thread: the
// shared ones remember signatures and inputs in maps and arrays that only the
// main thread may touch.
fn arm64_compile_unit_with(compiler string, args []string, text string, target pref.Target, no_cache bool, mut stats CObjectCacheStats, mut inputs Arm64CInputs) !string {
	if !no_cache {
		if object := arm64_cached_object_with(compiler, args, text, target, mut stats, mut
			inputs)
		{
			return object
		}
	}
	source, object := arm64_cached_unit_paths(text)!
	token := tempname.unique_token()
	depfile := '${object}.d.${token}'
	staged := '${object}.tmp.${token}'
	defer {
		os.rm(depfile) or {}
	}
	syntax_only := '-fsyntax-only' in args
	started := c_object_scan_start()
	mut compile_args := args.clone()
	compile_args << ['-MD', '-MT', 'v3cache', '-MF', depfile]
	if syntax_only {
		compile_args << source
	} else {
		compile_args << ['-c', source, '-o', staged]
	}
	result := cmdexec.run(compiler, compile_args)
	if result.exit_code != 0 {
		os.rm(staged) or {}
		return error(result.output)
	}
	if syntax_only {
		os.write_file(staged, '')!
	}
	listed := c_object_dependencies_from_output(os.read_file(depfile) or { '' }, source)
	// A probe's `#line "__v_cprobe__"` is not a file, but `-M` can list it.
	dependencies := c_object_with_missing_candidates(compiler, args, CObjectDependencies{
		...listed
		files: listed.files.filter(os.is_file(it))
	}, source, '', arm64_cache_dir(), started)
	if dependencies.used_fallback || arm64_input_changed_since(dependencies.files, source, started) {
		inputs.fail('the inputs of ${os.file_name(source)} changed or are not known')
		return staged
	}
	inputs.add_dependencies(&dependencies, source)
	manifest := c_object_manifest_path(arm64_cache_dir(), object, compiler, args, target, mut stats)
	key := c_object_cache_name(object, compiler, args, dependencies.files, target, false, mut stats)
	published := os.join_path(arm64_cache_dir(), key)
	publish_native_c_object(staged, published)!
	write_c_object_manifest(manifest, published, &dependencies, mut stats) or {}
	return published
}

// arm64_input_changed_since reports whether a file of a compile, other than its
// generated source, was written after the compile began. The depfile names the
// files only once the compile is over, so there is no earlier hash to compare
// with as compile_cached_c_source_object has: the signature taken now could
// describe a later version of a header than the object was built from. Such an
// object is used for this build and not published. Zig declines to trust a time
// this recent as well (Cache.zig, isProblematicTimestamp); Go's action IDs hash
// the inputs before the compile (work.Builder.buildActionID).
fn arm64_input_changed_since(files []string, source string, started i64) bool {
	for file in files {
		if file != source && os.file_last_mod_unix(file) >= started {
			return true
		}
	}
	return false
}

// arm64_c_unit_compiles reports whether `text` passes the C compiler's syntax
// and semantic checks with `args` and `mode`. The check stops before code
// generation, as cgo's gccErrors (cmd/cgo/gcc.go) only wants the diagnostics.
fn arm64_c_unit_compiles(compiler string, args []string, text string, mode []string, target pref.Target, no_cache bool) bool {
	arm64_compile_unit(compiler, arm64_language_check_args(args, mode), text, target, no_cache) or {
		return false
	}
	return true
}

fn arm64_language_check_args(args []string, mode []string) []string {
	mut check_args := args.clone()
	check_args << mode
	check_args << '-fsyntax-only'
	return check_args
}

// arm64_c_side_is_objective_c decides whether the C side must be compiled as
// Objective-C and remembers the answer while its object stays valid.
fn arm64_c_side_is_objective_c(compiler string, args []string, text string, target pref.Target, no_cache bool) bool {
	marker := os.join_path(arm64_cache_dir(), 'arm64_language_${arm64_text_hash(compiler, args.join(' '), text)}')
	objc_mode := ['-x', 'objective-c']
	c_mode := ['-std=gnu11']
	if !no_cache {
		if language := os.read_file(marker) {
			mode_args := arm64_language_check_args(args, if language == 'objc' {
				objc_mode
			} else {
				c_mode
			})
			if _ := arm64_cached_object(compiler, mode_args, text, target) {
				return language == 'objc'
			}
		} else {
		}
	}
	objective_c := !arm64_c_unit_compiles(compiler, args, text, c_mode, target, no_cache)
		&& arm64_c_unit_compiles(compiler, args, text, objc_mode, target, no_cache)
	os.write_file(marker, if objective_c { 'objc' } else { 'c' }) or {}
	return objective_c
}

// Arm64LinkInputs holds what preparing the link inputs needs from the driver.
struct Arm64LinkInputs {
	environment_cflags []string
	optimization_flags []string
	target             pref.Target
	c99                bool
	no_std             bool
}

// Arm64PreparedLink is the result of preparing the link inputs on a thread,
// with what its C compiles read.
struct Arm64PreparedLink {
	flags  []string
	error  string
	inputs Arm64CInputs
}

// arm64_prepare_link_inputs builds what the link needs and that does not depend
// on the V code: the objects named by `#flag` and the sources the link command
// would otherwise compile on every link. It reads no AST, so it runs on a thread
// beside the rest of the build, as Go's action graph runs a package's C compiles
// beside its Go compile (cmd/go/internal/work/exec.go, Builder.Do).
fn arm64_prepare_link_inputs(side Arm64CSide, inputs Arm64LinkInputs) Arm64PreparedLink {
	mut link_flags := x64_native_link_flags(side.flags)
	if link_flags.len == 0 {
		return Arm64PreparedLink{}
	}
	// Objects named by `#flag` (Boehm's gc.o, ...) are built from their adjacent
	// sources with the program's compile flags.
	mut stats := CObjectCacheStats{}
	mut c_inputs := Arm64CInputs{}
	requested := link_flags.clone()
	link_flags = prepare_c_flags_for_link(link_flags, inputs.environment_cflags, []string{},
		inputs.optimization_flags, inputs.c99, inputs.no_std, '', side.target_args, inputs.target,
		side.compiler, false, os.vtmp_dir(), mut stats) or {
		return Arm64PreparedLink{
			error: err.msg()
		}
	}
	if stats.requests > 0 {
		c_inputs.add_link_plan(arm64_link_plan_path(requested, side, inputs, mut stats),
			stats.link_plan_signature)
	}
	flags := arm64_compile_link_sources(link_flags, side, inputs, mut c_inputs)
	return Arm64PreparedLink{
		flags:  flags
		inputs: c_inputs
	}
}

// arm64_link_plan_path is the path of the link plan that prepare_c_flags_for_link
// keeps for `flags` in arm64_prepare_link_inputs. The plan lists the files that
// its objects were compiled from, with the program's target arguments.
fn arm64_link_plan_path(flags []string, side Arm64CSide, inputs Arm64LinkInputs, mut stats CObjectCacheStats) string {
	mut common_flags := inputs.optimization_flags.clone()
	common_flags << c_object_compile_support_flags(flags)
	plan := CObjectFlagPlan{
		environment_flags: inputs.environment_cflags
		primary_compiler:  side.compiler
		common_flags:      common_flags
	}
	return c_link_plan_path(os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs'), flags,
		&plan, inputs.c99, inputs.no_std, '', side.target_args, inputs.target, side.compiler,
		false, mut stats)
}

// add_link_plan records the files and absent paths of the link plan at `path`,
// which has to be the plan whose signature preparing the link took: one that was
// not written, because an object could not be cached, leaves them unknown.
fn (mut inputs Arm64CInputs) add_link_plan(path string, signature string) {
	if signature.len == 0 || modulecache.file_signature(path) != signature {
		inputs.fail('the objects of the link have no plan')
		return
	}
	lines := os.read_lines(path) or {
		inputs.fail('the plan of the link cannot be read')
		return
	}
	for line in lines {
		if line.starts_with('missing=') {
			inputs.missing << line.all_after('missing=')
		} else if line.starts_with('dependency=') {
			inputs.files << line.all_after('dependency=').all_before('\t')
		}
	}
}

// arm64_compile_link_sources replaces each C source of the link command
// (`-x c file.c -x none`, fontstash_impl.c) by its cached object, compiled with
// the program's flags. A source that does not compile stays in the command, whose
// compiler reports the error.
fn arm64_compile_link_sources(flags []string, side Arm64CSide, inputs Arm64LinkInputs, mut c_inputs Arm64CInputs) []string {
	mut result := []string{cap: flags.len}
	mut args := side.target_args.clone()
	args << ['-w', '-fwrapv', '-fno-common']
	args << inputs.optimization_flags
	args << inputs.environment_cflags
	args << x64_native_compile_flags(side.flags)
	mut stats := CObjectCacheStats{}
	mut i := 0
	for i < flags.len {
		if i + 4 < flags.len && flags[i] == '-x' && flags[i + 1] == 'c' && flags[i + 3] == '-x'
			&& flags[i + 4] == 'none' {
			source := flags[i + 2].trim_space()
			if object := arm64_compile_unit_with(side.compiler, args, '#include "${source}"\n',
				side.target, side.no_cache, mut stats, mut c_inputs)
			{
				result << object
				i += 5
				continue
			}
			c_inputs.fail('${source} did not compile on its own')
		}
		result << flags[i]
		i++
	}
	return result
}

// Arm64ExternalLink holds what arm64_external_link needs from the driver.
struct Arm64ExternalLink {
	object        string // the generator's relocatable object
	output        string
	link_ld_flags []string
	link_flags    []string // from arm64_prepare_link_inputs
	target        pref.Target
	show_cc       bool
}

// arm64_external_link compiles the program's C side and links it with the
// generator's object through the system C compiler driver.
fn arm64_external_link(side Arm64CSide, opts Arm64ExternalLink) ! {
	if opts.show_cc {
		println('${side.compiler} ${side.compile_args.join(' ')} -c <C side>')
	}
	support_object := arm64_compile_unit(side.compiler, side.compile_args, side.support.text +
		side.wrappers, side.target, side.no_cache) or {
		return error('arm64: compiling the C side of the program failed:\n${err}')
	}
	mut link_args := []string{}
	link_args << side.target_args
	link_args << [opts.object, support_object]
	link_args << opts.link_flags
	link_args << opts.link_ld_flags
	if opts.target.os == 'macos' {
		// The same 64 MiB main stack as the C backend's executables and the
		// built-in linker's LC_MAIN: recursive passes such as the compiler's
		// checker need it, the more so while each SSA value has its own slot.
		link_args << '-Wl,-stack_size,0x4000000'
	}
	link_args << ['-o', opts.output]
	mut c_inputs := arm64_build_c_inputs()
	c_inputs.add_link(link_args, side.compiler, opts.target.os.str(), [
		os.dir(opts.object),
		arm64_cache_dir(),
	])
	if opts.show_cc {
		println('${side.compiler} ${link_args.join(' ')}')
	}
	linked := cmdexec.run(side.compiler, link_args)
	if linked.exit_code != 0 {
		return error('arm64: linking with `${side.compiler}` failed:\n${linked.output}')
	}
}

// Reading the program's C names, as cgo does.
//
// The arm64 backend has no C front end, and V must not parse C headers itself.
// A `C.NAME` constant of the program (an enum value of sokol, a `#define` of
// stb_image) is resolved like cgo resolves its names: the C compiler compiles
// a probe unit with the program's C side plus one line per name, and the
// values are read back from the probe object. Nothing runs, so it works the
// same when the target is not the host. A name that is not an arithmetic
// constant (a function, a variable, a string, a type) fails to compile on its
// own `#line` and is left out, and the generator reports it as before.

// arm64_c_probe_file names the lines of a probe in the compiler's diagnostics.
const arm64_c_probe_file = '__v_cprobe__'

// arm64_c_constant_names lists the `C.NAME` selectors of the program, without
// the names the backend resolves itself.
fn arm64_c_constant_names(a &flat.FlatAst) []string {
	mut names := []string{}
	// A declared `fn C.name` is a function: as a constant its line would only be
	// an error that costs a second compile of the probe.
	mut seen := map[string]bool{}
	for function in arm64_c_function_decl_names(a) {
		seen[function] = true
	}
	for node in a.nodes {
		if node.kind != .selector || node.children_count == 0 {
			continue
		}
		base := a.nodes[int(a.child(&node, 0))]
		if base.kind != .ident || base.value_text() != 'C' {
			continue
		}
		name := node.value_text()
		if name.len == 0 || name in seen || name in ['environ', 'errno'] || !arm64_is_c_identifier(name) {
			continue
		}
		seen[name] = true
		names << name
	}
	return names
}

// Libc and compiler-rt functions that the SSA builder calls without a `fn C.`
// declaration in the program. Listing them keeps them in the program's single
// probe; a name missing here only costs a separate probe (arm64_c_function_kinds).
const arm64_synthesized_c_functions = ['unlink', 'strtod', 'getrusage', 'posix_memalign', 'sigaltstack',
	'raise', '__umodti3', '__udivti3']

// arm64_c_function_decl_names lists the program's `fn C.name` declarations and
// the functions the SSA builder calls on its own.
fn arm64_c_function_decl_names(a &flat.FlatAst) []string {
	mut names := arm64_synthesized_c_functions.clone()
	mut seen := map[string]bool{}
	for name in names {
		seen[name] = true
	}
	for node in a.nodes {
		if node.kind != .c_fn_decl {
			continue
		}
		name := node.value_text().trim_string_left('C.')
		if name.len > 0 && name !in seen && arm64_is_c_identifier(name) {
			seen[name] = true
			names << name
		}
	}
	return names
}

fn arm64_is_c_identifier(name string) bool {
	for i, c in name {
		if !(c.is_letter() || c == `_` || (i > 0 && c.is_digit())) {
			return false
		}
	}
	return true
}

// arm64_c_constant_probe_lines writes one probe line per name; each line
// defines the integer value, the floating point value and whether the
// constant has a floating point type.
fn arm64_c_constant_probe_lines(names []string, keep []bool, base int) string {
	mut out := []string{}
	out << '#define V_CCONST_KEEP __attribute__((used, visibility("hidden")))'
	for i, name in names {
		if !keep[base + i] {
			continue
		}
		out << '#line ${base + i + 1} "${arm64_c_probe_file}"'
		out << 'V_CCONST_KEEP const double __v_cconst_f_${i} = (double)(${name}); V_CCONST_KEEP const int __v_cconst_k_${i} = _Generic((${name}), float: 1, double: 1, long double: 1, unsigned long: 2, unsigned long long: 2, default: 0); V_CCONST_KEEP const long long __v_cconst_i_${i} = (long long)(${name});'
	}
	return out.join('\n') + '\n'
}

// arm64_probe_preamble includes the system headers that the C backend's
// generated C always includes, so `C.FIONREAD` (sys/ioctl.h) or `C.KERN_SUCCESS`
// (mach/mach.h) are seen without an `#include` of their own.
fn arm64_probe_preamble() string {
	mut preamble := ''
	for header in cgen.posix_preamble_headers {
		preamble += '#include <${header}>\n'
	}
	for header in cgen.apple_preamble_headers {
		preamble += '#include <${header}>\n'
	}
	return preamble
}

// arm64_probe_compile_flags returns the flags of a probe compile. Like cgo's
// gccCmd (cmd/cgo/gcc.go), link-time optimization stays off, so the object has
// real data to read, and clang is told to report every error at once.
fn arm64_probe_compile_flags(compiler string) []string {
	mut flags := ['-w', '-fno-lto']
	if os.user_os() == 'macos' || os.file_name(compiler).contains('clang') {
		flags << '-ferror-limit=0'
	}
	return flags
}

// arm64_run_probe compiles the probe lines written by `lines` for the entries
// still in `keep`, after `prefix` (the program's C side and any preamble), and
// returns the object. Each line carries a `#line` in `probe_file`, so a line that
// is an error (a name or field the headers do not have) is dropped and the probe
// is compiled again, as cgo does. The loop ends because every round removes at
// least one line; when none can be blamed, the C side itself does not compile
// and the real compile reports it. A later build with the same lines reuses the
// surviving set, and its object, while the headers are unchanged.
fn arm64_run_probe(mut keep []bool, lines fn ([]bool) string, prefix string, side Arm64CSide) ?string {
	mut args := side.compile_args.clone()
	args << arm64_probe_compile_flags(side.compiler)
	keep_file := os.join_path(arm64_cache_dir(), 'arm64_probe_keep_${arm64_text_hash(side.compiler, args.join(' '), prefix, lines(keep))}')
	if !side.no_cache {
		if saved := os.read_file(keep_file) {
			if saved.len == keep.len {
				mut candidate := []bool{len: keep.len}
				for i in 0 .. saved.len {
					candidate[i] = saved[i] == `1`
				}
				if object := arm64_cached_object(side.compiler, args, prefix + lines(candidate), side.target) {
					keep = candidate.clone()
					return object
				}
			}
		} else {
		}
	}
	for {
		object := arm64_compile_unit(side.compiler, args, prefix + lines(keep), side.target,
			side.no_cache) or {
			mut dropped := false
			for line in err.msg().split_into_lines() {
				if !line.starts_with('${arm64_c_probe_file}:') {
					continue
				}
				index := line.all_after(':').all_before(':').int() - 1
				if index >= 0 && index < keep.len && keep[index] {
					keep[index] = false
					dropped = true
				}
			}
			if !dropped {
				return none
			}
			continue
		}
		os.write_file(keep_file, keep.map(if it { '1' } else { '0' }).join('')) or {}
		return object
	}
	return none
}

fn arm64_probe_symbol_i64(symbols map[string][]u8, name string) ?i64 {
	bits := symbols[name] or { return none }
	if bits.len < 8 {
		return none
	}
	return unsafe { *(&i64(&u8(bits.data))) }
}

// arm64_read_c_constants reads the values of the arithmetic C constants of the
// probe, whose lines for `names` start at `base`.
fn arm64_read_c_constants(names []string, keep []bool, base int, symbols map[string][]u8, mut result map[string]Arm64CConstant) {
	for i, name in names {
		if !keep[base + i] {
			continue
		}
		kind := symbols['___v_cconst_k_${i}'] or { continue }
		if kind.len < 4 {
			continue
		}
		if kind[0] == 1 {
			bits := symbols['___v_cconst_f_${i}'] or { continue }
			if bits.len < 8 {
				continue
			}
			value := unsafe { *(&f64(&u8(bits.data))) }
			result[name] = Arm64CConstant{
				value:    '${value:.17g}'
				is_float: true
			}
		} else {
			value := arm64_probe_symbol_i64(symbols, '___v_cconst_i_${i}') or { continue }
			// The value is read as a `long long`; like cgo's guessKinds, an
			// unsigned constant above 2^63 keeps its unsigned spelling.
			result[name] = Arm64CConstant{
				value: if kind[0] == 2 { u64(value).str() } else { value.str() }
			}
		}
	}
}

// Arm64CStruct is a `struct C.X` declaration: how C names it and the fields
// the V declaration lists.
struct Arm64CStruct {
	v_name string // `C.FT_FaceRec`
	c_type string // `struct FT_FaceRec`, or `FT_Bitmap` for a typedef
	fields []string
}

// Arm64CStructLayout is the size, alignment and field offsets of a C struct.
struct Arm64CStructLayout {
	size    int
	align   int
	offsets map[string]int
}

// arm64_c_structs lists the program's `struct C.X` declarations with fields.
fn arm64_c_structs(a &flat.FlatAst) []Arm64CStruct {
	mut structs := []Arm64CStruct{}
	mut seen := map[string]bool{}
	for node in a.nodes {
		if node.kind != .struct_decl {
			continue
		}
		v_name := node.value_text()
		if !v_name.starts_with('C.') || v_name in seen || node.children_count == 0 {
			continue
		}
		c_name := v_name[2..]
		if !arm64_is_c_identifier(c_name) {
			continue
		}
		attrs := node.typ_text().split(',')
		c_type := if 'typedef' in attrs {
			c_name
		} else if 'union' in attrs {
			'union ${c_name}'
		} else {
			'struct ${c_name}'
		}
		mut fields := []string{}
		for i in 0 .. node.children_count {
			field := a.child_node(&node, i)
			name := field.value_text().trim_left('@')
			if name.len > 0 && arm64_is_c_identifier(name) {
				fields << name
			}
		}
		seen[v_name] = true
		structs << Arm64CStruct{
			v_name: v_name
			c_type: c_type
			fields: fields
		}
	}
	return structs
}

// arm64_c_struct_probe_lines writes one line for the size and alignment of
// each struct and one per field offset, so a declaration or field that C does
// not have only drops its own line.
fn arm64_c_struct_probe_lines(structs []Arm64CStruct, keep []bool, base int) string {
	mut out := []string{}
	out << '#include <stddef.h>'
	out << '#define V_CSTRUCT_KEEP __attribute__((used, visibility("hidden")))'
	mut line := base
	for i, st in structs {
		if keep[line] {
			out << '#line ${line + 1} "${arm64_c_probe_file}"'
			out << 'V_CSTRUCT_KEEP const long long __v_cstruct_s_${i} = (long long)sizeof(${st.c_type}); V_CSTRUCT_KEEP const long long __v_cstruct_a_${i} = (long long)_Alignof(${st.c_type});'
		}
		line++
		for j, field in st.fields {
			if keep[line] {
				out << '#line ${line + 1} "${arm64_c_probe_file}"'
				out << 'V_CSTRUCT_KEEP const long long __v_cstruct_o_${i}_${j} = (long long)offsetof(${st.c_type}, ${field});'
			}
			line++
		}
	}
	return out.join('\n') + '\n'
}

fn arm64_c_struct_probe_count(structs []Arm64CStruct) int {
	mut count := 0
	for st in structs {
		count += 1 + st.fields.len
	}
	return count
}

// arm64_read_c_struct_layouts reads the layouts of the program's C structs from
// the probe, like cgo's -godefs: V declarations of C structs often list only the
// fields a binding reads (vglyph's `C.FT_GlyphSlotRec`), so their offsets cannot
// be computed from the V declaration.
fn arm64_read_c_struct_layouts(structs []Arm64CStruct, keep []bool, base int, symbols map[string][]u8, mut result map[string]Arm64CStructLayout) {
	mut line := base
	for i, st in structs {
		struct_line := line
		line += 1 + st.fields.len
		if !keep[struct_line] {
			continue
		}
		size := arm64_probe_symbol_i64(symbols, '___v_cstruct_s_${i}') or { continue }
		align := arm64_probe_symbol_i64(symbols, '___v_cstruct_a_${i}') or { continue }
		mut offsets := map[string]int{}
		for j, field in st.fields {
			if !keep[struct_line + 1 + j] {
				continue
			}
			offset := arm64_probe_symbol_i64(symbols, '___v_cstruct_o_${i}_${j}') or { continue }
			offsets[field] = int(offset)
		}
		result[st.v_name] = Arm64CStructLayout{
			size:    int(size)
			align:   int(align)
			offsets: offsets
		}
	}
}

// Arm64CFunctionKind is why a C function needs a wrapper (see
// arm64.CFunctionKind).
enum Arm64CFunctionKind {
	macro
	internal
}

// arm64_c_function_probe_lines finds, among the C functions V code calls, the
// function-like macros and the functions with internal linkage: neither has a
// symbol the linker can bind V's calls to. A name that the headers do not declare
// fails on its own line and is left for the linker.
fn arm64_c_function_probe_lines(names []string, keep []bool, base int) string {
	mut lines := []string{}
	lines << '#define V_CFN_KEEP __attribute__((used, visibility("hidden")))'
	for i, name in names {
		if !keep[base + i] {
			continue
		}
		line := base + i + 1
		lines << '#line ${line} "${arm64_c_probe_file}"'
		lines << '#ifdef ${name}'
		lines << '#line ${line} "${arm64_c_probe_file}"'
		lines << 'V_CFN_KEEP const int __v_cfn_macro_${i} = 1;'
		lines << '#else'
		lines << '#line ${line} "${arm64_c_probe_file}"'
		lines << 'V_CFN_KEEP void *const __v_cfn_addr_${i} = (void *)&${name};'
		lines << '#endif'
	}
	return lines.join('\n') + '\n'
}

// arm64_read_c_functions classifies the functions of the probe from its symbols.
fn arm64_read_c_functions(names []string, keep []bool, base int, symbols []MachOProbeSymbol, mut result map[string]Arm64CFunctionKind) {
	mut local_functions := map[string]bool{}
	mut macros := map[string]bool{}
	for sym in symbols {
		if sym.name.starts_with('___v_cfn_macro_') {
			macros[sym.name] = true
		} else if !sym.external && sym.name.starts_with('_') {
			local_functions[sym.name[1..]] = true
		}
	}
	for i, name in names {
		if !keep[base + i] {
			continue
		}
		if '___v_cfn_macro_${i}' in macros {
			result[name] = .macro
		} else if name in local_functions {
			result[name] = .internal
		}
	}
}

// Arm64CProbeRequest is what one probe compile asks the C compiler.
struct Arm64CProbeRequest {
	functions []string // `fn C.name` declarations
	constants []string
	structs   []Arm64CStruct
}

// Arm64CProbes is what the C compiler answered.
struct Arm64CProbes {
mut:
	constants map[string]Arm64CConstant
	structs   map[string]Arm64CStructLayout
	functions map[string]Arm64CFunctionKind
	// asked holds the function names the probe covered, with or without a kind.
	asked map[string]bool
}

// arm64_probe_c_side asks the C compiler everything at once: like cgo, which
// compiles all the names of a package in one unit (cmd/cgo/gcc.go, loadDWARF),
// the functions, constants and struct layouts share one compile of the program's
// C side. The function lines come before the system preamble, so they see only
// what the program's own C side declares and what the wrappers can call.
fn arm64_probe_c_side(request Arm64CProbeRequest, side Arm64CSide) Arm64CProbes {
	mut result := Arm64CProbes{}
	constants_base := request.functions.len
	structs_base := constants_base + request.constants.len
	count := structs_base + arm64_c_struct_probe_count(request.structs)
	if count == 0 {
		return result
	}
	mut keep := []bool{len: count, init: true}
	preamble := arm64_probe_preamble()
	object := arm64_run_probe(mut keep, fn [request, constants_base, structs_base, preamble] (keep []bool) string {
		return arm64_c_function_probe_lines(request.functions, keep, 0) + preamble +
			arm64_c_constant_probe_lines(request.constants, keep, constants_base) +
			arm64_c_struct_probe_lines(request.structs, keep, structs_base)
	}, side.support.text, side) or {
		return result
	}
	symbol_bytes := macho_object_symbol_bytes(object, '___v_c') or { return result }
	symbols := macho_object_symbols(object) or { return result }
	arm64_read_c_constants(request.constants, keep, constants_base, symbol_bytes, mut result.constants)
	arm64_read_c_struct_layouts(request.structs, keep, structs_base, symbol_bytes, mut result.structs)
	arm64_read_c_functions(request.functions, keep, 0, symbols, mut result.functions)
	for name in request.functions {
		result.asked[name] = true
	}
	return result
}

// arm64_c_function_kinds returns the kinds of the C functions that V code calls.
// The names the probe did not cover are the ones the SSA builder synthesizes;
// they are probed on their own.
fn arm64_c_function_kinds(names []string, probes Arm64CProbes, side Arm64CSide) map[string]Arm64CFunctionKind {
	mut result := map[string]Arm64CFunctionKind{}
	mut unasked := []string{}
	for name in names {
		if name in probes.functions {
			result[name] = probes.functions[name]
		} else if name !in probes.asked && arm64_is_c_identifier(name) {
			unasked << name
		}
	}
	if unasked.len == 0 {
		return result
	}
	mut keep := []bool{len: unasked.len, init: true}
	object := arm64_run_probe(mut keep, fn [unasked] (keep []bool) string {
		return arm64_c_function_probe_lines(unasked, keep, 0)
	}, side.support.text, side) or { return result }
	symbols := macho_object_symbols(object) or { return result }
	arm64_read_c_functions(unasked, keep, 0, symbols, mut result)
	return result
}

// macho_object_symbol_bytes returns, for each defined symbol of a 64-bit
// Mach-O object whose name starts with `prefix`, the bytes from the symbol to
// the next symbol (or the end of its section).
fn macho_object_symbol_bytes(path string, prefix string) !map[string][]u8 {
	data := os.read_bytes(path)!
	if data.len < 32 || macho_u32(data, 0) != 0xfeedfacf {
		return error('not a 64-bit Mach-O object')
	}
	ncmds := int(macho_u32(data, 16))
	mut sect_addr := []u64{}
	mut sect_size := []u64{}
	mut sect_off := []u64{}
	mut sect_zerofill := []bool{}
	mut off := 32
	for _ in 0 .. ncmds {
		if off + 8 > data.len {
			return error('truncated load commands')
		}
		cmd := macho_u32(data, off)
		cmdsize := int(macho_u32(data, off + 4))
		if cmd == 0x19 {
			nsects := int(macho_u32(data, off + 64))
			for s in 0 .. nsects {
				base := off + 72 + s * 80
				sect_addr << macho_u64(data, base + 32)
				sect_size << macho_u64(data, base + 40)
				sect_off << u64(macho_u32(data, base + 48))
				sect_zerofill << macho_u32(data, base + 64) & 0xff in [u32(0x1), 0xc, 0x12]
			}
		}
		off += cmdsize
	}
	mut entries := macho_defined_symbols(data)!
	// Sorted by address, a symbol ends where the next one of its section starts.
	entries.sort_with_compare(fn (a &MachOProbeSymbol, b &MachOProbeSymbol) int {
		if a.sect != b.sect {
			return if a.sect < b.sect { -1 } else { 1 }
		}
		return if a.value < b.value {
			-1
		} else if a.value > b.value {
			1
		} else {
			0
		}
	})
	mut result := map[string][]u8{}
	for index, e in entries {
		if !e.name.starts_with(prefix) || e.sect < 0 || e.sect >= sect_addr.len {
			continue
		}
		mut end := sect_addr[e.sect] + sect_size[e.sect]
		for next in index + 1 .. entries.len {
			other := entries[next]
			if other.sect != e.sect {
				break
			}
			if other.value > e.value {
				end = other.value
				break
			}
		}
		if sect_zerofill[e.sect] {
			result[e.name] = []u8{len: int(end - e.value)}
			continue
		}
		start := int(sect_off[e.sect] + e.value - sect_addr[e.sect])
		stop := int(sect_off[e.sect] + end - sect_addr[e.sect])
		if start < 0 || stop > data.len || stop < start {
			continue
		}
		result[e.name] = data[start..stop].clone()
	}
	return result
}

// Arm64CConstant is a probed C constant (see ssa.NativeCConstant).
struct Arm64CConstant {
	value    string
	is_float bool
}

struct MachOProbeSymbol {
	name     string
	sect     int
	value    u64
	external bool
}

// macho_object_symbols returns the defined symbols of a 64-bit Mach-O object.
fn macho_object_symbols(path string) ![]MachOProbeSymbol {
	data := os.read_bytes(path)!
	return macho_defined_symbols(data)!
}

fn macho_defined_symbols(data []u8) ![]MachOProbeSymbol {
	if data.len < 32 || macho_u32(data, 0) != 0xfeedfacf {
		return error('not a 64-bit Mach-O object')
	}
	ncmds := int(macho_u32(data, 16))
	mut off := 32
	mut symoff := 0
	mut nsyms := 0
	mut stroff := 0
	for _ in 0 .. ncmds {
		if off + 24 > data.len {
			return error('truncated load commands')
		}
		if macho_u32(data, off) == 0x2 {
			symoff = int(macho_u32(data, off + 8))
			nsyms = int(macho_u32(data, off + 12))
			stroff = int(macho_u32(data, off + 16))
		}
		off += int(macho_u32(data, off + 4))
	}
	mut result := []MachOProbeSymbol{}
	for i in 0 .. nsyms {
		base := symoff + i * 16
		if base + 16 > data.len {
			break
		}
		typ := data[base + 4]
		if typ & 0x0e != 0x0e || data[base + 5] == 0 {
			continue
		}
		name_off := stroff + int(macho_u32(data, base))
		mut end := name_off
		for end < data.len && data[end] != 0 {
			end++
		}
		result << MachOProbeSymbol{
			name:     data[name_off..end].bytestr()
			sect:     int(data[base + 5]) - 1
			value:    macho_u64(data, base + 8)
			external: typ & 0x01 != 0
		}
	}
	return result
}

fn macho_u32(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[off + 3]) << 24)
}

fn macho_u64(data []u8, off int) u64 {
	return u64(macho_u32(data, off)) | (u64(macho_u32(data, off + 4)) << 32)
}
