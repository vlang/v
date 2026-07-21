module main

import os
import rand
import strings
import v3.bench
import v3.cmdexec
import v3.modulecache
import v3.flat
import v3.gen.c as cgen
import v3.gen.c.naming
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types
import v.vmod

$if gcboehm ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_full ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_incr ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_opt ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_leak ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if vgc ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if !skip_eval ? {
	import v3.eval
}
$if !skip_arm64 ? {
	import v3.gen.arm64
	import v3.ssa
	import v3.ssa.optimize
}
$if !skip_wasm ? {
	import v3.gen.wasm
}

const cache_bundle_import_file_name = '.v3_cache_bundle_imports.vh'
const scoped_transform_signature_headroom = 2048

struct V3ModuleCacheState {
	manager             modulecache.Manager
	bundle_sources      []string
	bundle_source_paths map[string]bool
mut:
	force_source           bool
	bundle_valid           bool
	module_sources         map[string][]string
	module_import_paths    map[string]string
	module_dependencies    map[string][]string
	module_external_inputs map[string][]string
	parsed_from_source     map[string]bool
	source_body_modules    map[string]bool
	native_source_modules  map[string]bool
	objects                map[string]string
	headers                map[string]string
}

struct V3PreparedModuleCache {
mut:
	main_source              string
	tcc_main_source          string
	main_body                string
	program_body_cache       string
	program_prefix_source    string
	program_declarations     string
	tcc_program_declarations string
	objects                  []string
	newly_cached_modules     int
}

struct V3CgenCacheInput {
	source_files         []string
	dependency_inputs    map[string]string
	generation_signature string
}

struct V3CgenCacheMetadata {
	interface_impl_signature string
	prefix_source_identity   string
	flags                    []string
}

fn tcc_atomic_s_arg(prefs &pref.Preferences) string {
	target_os := prefs.normalized_target_os()
	mut link_atomic_s := false
	match target_os {
		'macos' {
			// atomic.S has Mach-O-compatible aarch64 symbols, but its x86_64 Unix
			// stanza is ELF-only (`.type ... %function`).
			if prefs.target.arch == 'arm64' {
				link_atomic_s = true
			}
		}
		'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly' {
			link_atomic_s = true
		}
		else {}
	}

	if !link_atomic_s {
		return ''
	}
	atomic_s := os.join_path(prefs.vroot, 'thirdparty', 'stdatomic', 'nix', 'atomic.S')
	return atomic_s
}

struct CObjectCacheStats {
mut:
	requests                  int
	direct_objects            int
	content_key_hits          int
	dependency_manifest_hits  int
	misses                    int
	dependency_scans          int
	dependency_files          int
	dependency_file_reads     int
	dependency_scan_fallbacks int
	publish_races             int
	input_snapshot_races      int
	temporary_objects         []string
	compiler_versions         map[string]string
	file_signatures           map[string]string
	link_plan_signature       string
}

struct CObjectDependencies {
	files         []string
	used_fallback bool
}

struct CLinkPlan {
mut:
	flags            []string
	requests         int
	direct_objects   int
	dependency_files int
}

fn cpp_runtime_link_flag(target pref.Target) string {
	return if target.os in ['macos', 'ios'] { '-lc++' } else { '-lstdc++' }
}

fn prepare_c_flags_for_link(flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) ![]string {
	support_flags := c_object_compile_support_flags(flags)
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	plan_path := c_link_plan_path(cache_dir, flags, c99, pic_flag, target_args, target, c_compiler, mut
		stats)
	// Tracing intentionally walks the object manifests so every requested
	// object's cache decision remains visible.
	if os.getenv('V3_CACHE_TRACE') == '' {
		if plan := valid_c_link_plan(plan_path, mut stats) {
			stats.link_plan_signature = modulecache.file_signature(plan_path)
			stats.requests = plan.requests
			stats.direct_objects = plan.direct_objects
			stats.content_key_hits = plan.requests - plan.direct_objects
			stats.dependency_manifest_hits = plan.requests - plan.direct_objects
			stats.dependency_files = plan.dependency_files
			return plan.flags
		}
	}
	mut prepared := []string{}
	mut active_language := ''
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean == '-x' {
			active_language = if i + 1 < flags.len { flags[i + 1].trim_space() } else { '' }
			prepared << flag
			if i + 1 < flags.len {
				prepared << flags[i + 1]
			}
			i += 2
			continue
		}
		if c_flag_is_object_file(clean) {
			stats.requests++
			adjacent_cpp_source := if !os.exists(clean) {
				if source_file := c_source_from_object_file(clean) {
					c_source_language(source_file, active_language) in ['c++', 'objective-c++']
				} else {
					false
				}
			} else {
				false
			}
			object_path := ensure_c_object_file(clean, active_language, support_flags, c99,
				pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)!
			append_c_link_object(mut prepared, object_path, active_language)
			if adjacent_cpp_source {
				cpp_runtime := cpp_runtime_link_flag(target)
				if cpp_runtime !in flags && cpp_runtime !in prepared {
					prepared << cpp_runtime
				}
			}
		} else if clean.ends_with('.mm') {
			stats.requests++
			language := c_source_language(clean, active_language)
			object_path := ensure_c_source_object(clean, active_language, support_flags, c99,
				pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)!
			append_c_link_object(mut prepared, object_path, active_language)
			if c_generated_native_source_context(clean, uncached_dir) {
				os.rm(clean) or {}
			}
			if language in ['c++', 'objective-c++'] {
				cpp_runtime := cpp_runtime_link_flag(target)
				if cpp_runtime !in flags && cpp_runtime !in prepared {
					prepared << cpp_runtime
				}
			}
		} else if c_flag_is_c_source_file(clean) {
			prepared << flag
		} else {
			prepared << flag
		}
		i++
	}
	if c_link_flags_use_cpp_language(prepared) {
		cpp_runtime := cpp_runtime_link_flag(target)
		if cpp_runtime !in flags && cpp_runtime !in prepared {
			prepared << cpp_runtime
		}
	}
	if stats.dependency_scan_fallbacks == 0 && stats.temporary_objects.len == 0 {
		write_c_link_plan(plan_path, prepared, stats) or {}
		stats.link_plan_signature = modulecache.file_signature(plan_path)
	}
	return prepared
}

fn c_link_plan_path(cache_dir string, flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, compiler string, mut stats CObjectCacheStats) string {
	compiler_path, compiler_version := c_object_compiler_identity(compiler, mut stats)
	mut hash := u64(1469598103934665603)
	for identity in ['v3-c-link-plan-v1', os.getwd(), flags.join('\x00'),
		c99.str(), pic_flag, target_args.join('\x00'), compiler_path, compiler_version, target.os,
		target.arch, target.abi, target.endian, target.pointer_bits.str(), target.object_format] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return os.join_path(cache_dir, 'link_${hash.hex()}.manifest')
}

fn valid_c_link_plan(plan_path string, mut stats CObjectCacheStats) ?CLinkPlan {
	content := os.read_file(plan_path) or { return none }
	lines := content.split_into_lines()
	if lines.len < 5 || lines[0] != 'format=v3-c-link-plan-v2' {
		return none
	}
	mut plan := CLinkPlan{}
	mut objects := []string{}
	mut complete := false
	mut saw_requests := false
	mut saw_direct_objects := false
	mut saw_dependency_files := false
	for line in lines[1..] {
		if line.starts_with('requests=') {
			plan.requests = line.all_after('requests=').int()
			saw_requests = true
		} else if line.starts_with('direct_objects=') {
			plan.direct_objects = line.all_after('direct_objects=').int()
			saw_direct_objects = true
		} else if line.starts_with('dependency_files=') {
			plan.dependency_files = line.all_after('dependency_files=').int()
			saw_dependency_files = true
		} else if line.starts_with('flag=') {
			plan.flags << line.all_after('flag=')
		} else if line.starts_with('object=') {
			objects << line.all_after('object=')
		} else if line.starts_with('dependency=') {
			entry := line.all_after('dependency=')
			first_tab := entry.index('\t') or { return none }
			last_tab := entry.last_index('\t') or { return none }
			if first_tab == last_tab {
				return none
			}
			path := entry[..first_tab]
			expected_metadata := entry[first_tab + 1..last_tab]
			expected_signature := entry[last_tab + 1..]
			if path.len == 0 || expected_signature.len == 0 {
				return none
			}
			actual_metadata := modulecache.file_metadata_signature(path)
			if actual_metadata.len == 0 || actual_metadata != expected_metadata {
				actual_signature := c_object_file_signature(path, false, mut stats)
				if actual_signature.len == 0 || actual_signature != expected_signature {
					return none
				}
			}
		} else if line == 'complete=1' {
			complete = true
		} else {
			return none
		}
	}
	if !complete || !saw_requests || !saw_direct_objects || !saw_dependency_files
		|| plan.requests < 0 || plan.direct_objects < 0 || plan.direct_objects > plan.requests
		|| plan.dependency_files < 0 || objects.len != plan.requests {
		return none
	}
	for object_path in objects {
		if !os.is_file(object_path) {
			return none
		}
	}
	return plan
}

fn write_c_link_plan(plan_path string, flags []string, stats &CObjectCacheStats) ! {
	mut out := strings.new_builder(256 + flags.len * 64 + stats.file_signatures.len * 96)
	out.writeln('format=v3-c-link-plan-v2')
	out.writeln('requests=${stats.requests}')
	out.writeln('direct_objects=${stats.direct_objects}')
	out.writeln('dependency_files=${stats.dependency_files}')
	for flag in flags {
		out.writeln('flag=${flag}')
		if c_flag_is_object_file(flag.trim_space()) && os.is_file(flag.trim_space()) {
			out.writeln('object=${flag.trim_space()}')
		}
	}
	mut dependencies := stats.file_signatures.keys()
	dependencies.sort()
	for dependency in dependencies {
		metadata := modulecache.file_metadata_signature(dependency)
		out.writeln('dependency=${dependency}\t${metadata}\t${stats.file_signatures[dependency]}')
	}
	out.writeln('complete=1')
	temp_path := '${plan_path}.tmp.${os.getpid()}.${rand.ulid()}'
	defer {
		os.rm(temp_path) or {}
	}
	os.write_file(temp_path, out.str())!
	os.mv(temp_path, plan_path)!
}

fn append_c_link_object(mut flags []string, object_path string, active_language string) {
	if active_language.len > 0 && active_language != 'none' {
		flags << ['-x', 'none']
	}
	flags << object_path
	if active_language.len > 0 && active_language != 'none' {
		flags << ['-x', active_language]
	}
}

fn c_generated_native_source_context(path string, build_dir string) bool {
	base := os.base(path)
	return os.dir(path) == build_dir && base.contains('.v3_native_source_context_')
		&& (base.ends_with('.m') || base.ends_with('.mm'))
}

fn c_link_flags_use_non_c_language(flags []string) bool {
	return c_link_flags_use_language(flags, true)
}

fn c_link_flags_use_cpp_language(flags []string) bool {
	return c_link_flags_use_language(flags, false)
}

fn c_link_flags_use_language(flags []string, include_objective_c bool) bool {
	mut language := ''
	mut skip_operand := false
	mut i := 0
	for i < flags.len {
		clean := flags[i].trim_space()
		if skip_operand {
			skip_operand = false
			i++
			continue
		}
		if clean == '-x' && i + 1 < flags.len {
			language = flags[i + 1].trim_space()
			i += 2
			continue
		}
		if c_flag_consumes_next_operand(clean) {
			skip_operand = true
			i++
			continue
		}
		if c_flag_is_c_source_file(clean) {
			if language in ['c++', 'objective-c++']
				|| (include_objective_c && language == 'objective-c') {
				return true
			}
			if language in ['', 'none'] && (clean.ends_with('.cc') || clean.ends_with('.cpp')
				|| clean.ends_with('.mm')
				|| (include_objective_c && clean.ends_with('.m'))) {
				return true
			}
		} else if c_flag_is_existing_file(clean) {
			if language in ['c++', 'objective-c++']
				|| (include_objective_c && language == 'objective-c') {
				return true
			}
		}
		i++
	}
	return false
}

fn c_flag_consumes_next_operand(flag string) bool {
	return flag in ['-I', '-L', '-F', '-D', '-U', '-include', '-imacros', '-isystem', '-iquote',
		'-idirafter', '-iprefix', '-iwithprefix', '-iwithprefixbefore', '-isysroot', '--sysroot',
		'-target', '-arch', '-framework', '-weak_framework', '-Xlinker', '-force_load', '-o', '-MF',
		'-MT', '-MQ']
}

fn c_flag_is_existing_file(flag string) bool {
	clean := flag.trim(' \t\r\n"\'')
	return clean.len > 0 && clean[0] != `-` && os.is_file(clean)
}

fn c_object_compile_flags(flags []string) []string {
	mut compile_flags := []string{}
	mut skip_link_operand := false
	mut preserve_operand := false
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		part := flag.trim_space()
		if skip_link_operand {
			skip_link_operand = false
			i++
			continue
		}
		if preserve_operand {
			compile_flags << flag
			preserve_operand = false
			i++
			continue
		}
		if part == '-x' {
			i += 2
			continue
		}
		if part in ['-l', '-L', '-Xlinker', '-framework', '-weak_framework', '-force_load'] {
			skip_link_operand = true
			i++
			continue
		}
		if c_flag_consumes_next_operand(part) {
			compile_flags << flag
			preserve_operand = true
			i++
			continue
		}
		if part.len == 0 || c_flag_token_is_link_only(part) || c_flag_is_object_file(part)
			|| c_flag_is_c_source_file(part) || c_flag_is_existing_file(part) {
			i++
			continue
		}
		compile_flags << flag
		i++
	}
	return compile_flags
}

fn c_object_compile_support_flags(flags []string) []string {
	return c_object_compile_flags(flags)
}

fn c_dylib_link_flags(flags []string) []string {
	mut link_flags := []string{}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean == '-x' {
			i += 2
			continue
		}
		if clean in ['-l', '-L', '-F', '-framework', '-weak_framework', '-Xlinker', '-force_load'] {
			link_flags << flag
			if i + 1 < flags.len {
				link_flags << flags[i + 1]
			}
			i += 2
			continue
		}
		if c_flag_consumes_next_operand(clean) {
			i += 2
			continue
		}
		if clean == '-pthread' || clean.starts_with('-F')
			|| c_flag_token_is_link_only(clean) || c_flag_is_object_file(clean)
			|| (c_flag_is_existing_file(clean) && !c_flag_is_c_source_file(clean)) {
			link_flags << flag
		}
		i++
	}
	return link_flags
}

fn tcc_cached_main_flags(flags []string) []string {
	mut compile_flags := []string{}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean in ['-I', '-D', '-U', '-include', '-imacros', '-isystem', '-iquote', '-idirafter',
			'-iprefix', '-iwithprefix', '-iwithprefixbefore', '-isysroot', '--sysroot'] {
			if i + 1 < flags.len {
				value := flags[i + 1]
				if !(clean == '-D' && value.trim_space().starts_with('SOKOL_')) {
					compile_flags << [flag, value]
				}
			}
			i += 2
			continue
		}
		if clean.starts_with('-DSOKOL_') {
			i++
			continue
		}
		if clean.starts_with('-I') || clean.starts_with('-D') || clean.starts_with('-U')
			|| clean.starts_with('-isystem') || clean.starts_with('-iquote')
			|| clean.starts_with('--sysroot=') {
			compile_flags << flag
		}
		i++
	}
	return compile_flags
}

fn tcc_dynamic_link_flags(flags []string) []string {
	mut link_flags := []string{}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean in ['-l', '-L', '-weak_library'] {
			link_flags << flag
			if i + 1 < flags.len {
				link_flags << flags[i + 1]
			}
			i += 2
			continue
		}
		if c_flag_consumes_next_operand(clean) {
			i += 2
			continue
		}
		if clean.starts_with('-l') || clean.starts_with('-L') || clean.starts_with('-Wl,-rpath,')
			|| clean.starts_with('-Wl,-rpath=') {
			link_flags << flag
		} else if c_flag_is_existing_file(clean)
			&& (clean.ends_with('.dylib') || clean.ends_with('.so')
			|| clean.contains('.so.') || clean.ends_with('.tbd')) {
			link_flags << flag
		}
		i++
	}
	return link_flags
}

fn tcc_cached_main_source(source string) string {
	// Framework headers contain Objective-C syntax that TinyCC cannot parse.
	// Their implementations and public native symbols live in the cached dylib;
	// the remaining generated program unit only needs V's C declarations.
	objc_frameworks := ['AppKit', 'AudioToolbox', 'AVFoundation', 'Cocoa', 'Foundation', 'GLKit',
		'Metal', 'MetalKit', 'QuartzCore', 'UIKit', 'WebKit', 'objc']
	mut out := strings.new_builder(source.len)
	if source.contains('objc_msgSend') {
		// objc/message.h is intentionally omitted above. Plain C program files
		// can still cast the runtime entry point declared through `C.objc_msgSend`.
		out.writeln('void objc_msgSend(void);')
	}
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		mut omit := false
		if trimmed.starts_with('#include <') || trimmed.starts_with('#import <') {
			header := trimmed.all_after('<').all_before('>')
			root := header.all_before('/')
			omit = root in objc_frameworks
		}
		if !omit {
			out.writeln(line)
		}
	}
	return out.str()
}

fn v3_program_prefix_external_input_paths(state &V3ModuleCacheState) []string {
	mut paths := map[string]bool{}
	for input in state.module_external_inputs['main'] or { []string{} } {
		clean := input.trim_space()
		if os.is_file(clean) && !c_flag_token_is_link_only(clean) && !c_flag_is_object_file(clean) {
			paths[os.real_path(clean)] = true
		}
	}
	mut result := paths.keys()
	result.sort()
	return result
}

fn c_response_file_arg(arg string) string {
	return '"${arg.replace('\\', '\\\\').replace('"', '\\"')}"'
}

fn compile_v3_program_prefix(source string, source_identity string, external_inputs []string, manager &modulecache.Manager, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, target_args []string, target pref.Target, c_compiler string, mut stats CObjectCacheStats) !string {
	mut args := []string{}
	if objective_c {
		args << ['-x', 'objective-c']
	} else {
		args << ['-x', 'c']
	}
	for value in [c_standard, opt_flag, pic_flag] {
		if value.len > 0 {
			args << value
		}
	}
	args << target_args
	args << cgen.tokenize_c_flag(warning_flags)
	args << '-Wno-int-conversion'
	args << c_object_compile_flags(generated_c_flags).filter(!c_flag_is_object_file(it))
	compiler_path, compiler_version := c_object_compiler_identity(c_compiler, mut stats)
	mut hash := u64(1469598103934665603)
	prefix_identity := if source_identity.len > 0 { source_identity } else { source }
	for identity in ['v3-cached-program-prefix-v2', prefix_identity, compiler_path, compiler_version,
		args.join('\x00'), target.os, target.arch, target.abi, target.endian, target.pointer_bits.str(),
		target.object_format] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	for input in external_inputs {
		hash = c_hash_bytes(hash, input.bytes())
		hash = c_hash_bytes(hash, c_object_file_signature(input, false, mut stats).bytes())
	}
	key := hash.hex()
	source_path := os.join_path(manager.dir, 'program_prefix_${key}.c')
	object_path := os.join_path(manager.dir, 'program_prefix_${key}.o')
	if os.is_file(object_path) {
		return object_path
	}
	if source.len == 0 {
		return error('cached program prefix object is unavailable')
	}
	unique := '${os.getpid()}.${rand.ulid()}'
	tmp_source := '${source_path}.tmp.${unique}'
	tmp_object := '${object_path}.tmp.${unique}'
	defer {
		os.rm(tmp_source) or {}
		os.rm(tmp_object) or {}
	}
	os.write_file(tmp_source, source)!
	mut compile_args := args.clone()
	compile_args << ['-c', '-o', tmp_object, tmp_source]
	result := cmdexec.run(c_compiler, compile_args)
	if result.exit_code != 0 {
		return error('failed to build cached program prefix:\n${result.output}')
	}
	os.mv(tmp_object, object_path) or {
		if !os.is_file(object_path) {
			return error('failed to publish cached program prefix ${object_path}: ${err}')
		}
	}
	os.mv(tmp_source, source_path) or {
		if !os.is_file(source_path) {
			return error('failed to publish cached program prefix source ${source_path}: ${err}')
		}
	}
	return object_path
}

fn v3_program_prefix_source_identity(prefix_source string, cached_objects []string) string {
	if prefix_source.len == 0 {
		return ''
	}
	mut hash := u64(1469598103934665603)
	hash = c_hash_bytes(hash, prefix_source.bytes())
	hash = c_hash_bytes(hash, [u8(0xff)])
	for object in cached_objects {
		stamp := '${object}.stamp'
		input := if os.is_file(stamp) { stamp } else { object }
		hash = c_hash_bytes(hash, input.bytes())
		hash = c_hash_bytes(hash, modulecache.file_signature(input).bytes())
	}
	return hash.hex()
}

fn compile_v3_dev_dylib(prefix_object string, cached_objects []string, resolved_c_flags []string, manager &modulecache.Manager, target_args []string, target pref.Target, c_compiler string, build_dir string, mut stats CObjectCacheStats) !string {
	link_flags := c_dylib_link_flags(resolved_c_flags)
	mut objects := [prefix_object]
	objects << cached_objects
	mut hash := u64(1469598103934665603)
	compiler_path, compiler_version := c_object_compiler_identity(c_compiler, mut stats)
	for identity in ['v3-cached-dev-dylib-v1', compiler_path, compiler_version, target_args.join('\x00'),
		link_flags.join('\x00'), target.os, target.arch, target.abi, target.endian, target.pointer_bits.str(),
		target.object_format] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	for object in objects {
		hash = c_hash_bytes(hash, os.real_path(object).bytes())
		if object != prefix_object {
			// Module object paths are stable across source changes, but their
			// validated stamps contain the source/dependency signatures. Hashing
			// the small stamp avoids rereading every large cached object on each
			// warm link.
			stamp := '${object}.stamp'
			signature := if os.is_file(stamp) {
				c_object_file_signature(stamp, true, mut stats)
			} else {
				c_object_file_signature(object, true, mut stats)
			}
			hash = c_hash_bytes(hash, signature.bytes())
		}
	}
	for flag in link_flags {
		clean := flag.trim_space()
		if (clean.ends_with('.a') || (c_flag_is_object_file(clean)
			&& !clean.contains('/v3_thirdparty_objs/'))) && os.is_file(clean) {
			hash = c_hash_bytes(hash, os.real_path(clean).bytes())
			hash = c_hash_bytes(hash, c_object_file_signature(clean, true, mut stats).bytes())
		}
	}
	dylib_path := os.join_path(manager.dir, 'dev_modules_${hash.hex()}.dylib')
	if os.is_file(dylib_path) {
		return dylib_path
	}
	tmp_dylib := '${dylib_path}.tmp.${os.getpid()}.${rand.ulid()}'
	response_path := os.join_path(build_dir, 'dev_dylib.rsp')
	defer {
		os.rm(tmp_dylib) or {}
		os.rm(response_path) or {}
	}
	mut args := target_args.clone()
	args << ['-dynamiclib', '-Wl,-undefined,dynamic_lookup', '-Wl,-install_name,${dylib_path}',
		'-o', tmp_dylib]
	args << objects
	args << link_flags
	if '-lm' !in args {
		args << '-lm'
	}
	response := args.map(c_response_file_arg(it)).join('\n')
	os.write_file(response_path, response)!
	response_arg := '@${response_path}'
	println('  > ${cmdexec.display(c_compiler, [response_arg])} (${objects.len} cached objects)')
	result := cmdexec.run(c_compiler, [response_arg])
	if result.exit_code != 0 {
		return error('failed to build cached development dylib:\n${result.output}')
	}
	os.mv(tmp_dylib, dylib_path) or {
		if !os.is_file(dylib_path) {
			return error('failed to publish cached development dylib ${dylib_path}: ${err}')
		}
	}
	return dylib_path
}

fn v3_cache_file_identity(path string) string {
	metadata := modulecache.file_metadata_signature(path)
	if metadata.len > 0 {
		return metadata
	}
	return modulecache.file_signature(path)
}

fn v3_cached_tcc_executable_path(manager &modulecache.Manager, source_identity string, link_plan_signature string, tcc_path string, tcc_lib_dir string, tcc_args []string) string {
	mut hash := u64(1469598103934665603)
	for identity in ['v3-cached-tcc-executable-v1', source_identity, link_plan_signature,
		os.real_path(tcc_path), v3_cache_file_identity(tcc_path),
		tcc_args.join('\x00')] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	mut inputs := os.walk_ext(tcc_lib_dir, '.h')
	inputs << os.walk_ext(tcc_lib_dir, '.a')
	for arg in tcc_args {
		clean := arg.trim_space()
		if os.is_file(clean) {
			inputs << os.real_path(clean)
		}
	}
	inputs.sort()
	mut previous := ''
	for input in inputs {
		if input == previous {
			continue
		}
		previous = input
		hash = c_hash_bytes(hash, input.bytes())
		hash = c_hash_bytes(hash, v3_cache_file_identity(input).bytes())
	}
	return os.join_path(manager.dir, 'dev_executable_${hash.hex()}')
}

fn publish_v3_cached_executable(source string, destination string) {
	tmp := '${destination}.tmp.${os.getpid()}.${rand.ulid()}'
	defer {
		os.rm(tmp) or {}
	}
	os.cp(source, tmp) or { return }
	os.mv(tmp, destination) or {}
}

fn c_flag_token_is_link_only(token string) bool {
	clean := token.trim(' \t\r\n"\'')
	if clean.starts_with('-l') || clean.starts_with('-L') || clean.starts_with('-Wl,')
		|| clean in ['-ObjC', '-all_load', '-bundle', '-dynamiclib', '-shared', '-static', '-rdynamic', '-pie', '-no-pie'] {
		return true
	}
	return clean.ends_with('.a') || clean.ends_with('.so') || clean.contains('.so.')
		|| clean.ends_with('.dylib') || clean.ends_with('.dll') || clean.ends_with('.lib')
		|| clean.ends_with('.tbd')
}

fn c_flags_need_objective_c(flags []string) bool {
	for i, flag in flags {
		clean := flag.trim_space()
		if clean in ['-fobjc-arc', '-fobjc-gc', '-ObjC']
			|| clean.starts_with('-fobjc-')
			|| (clean == '-x' && i + 1 < flags.len && flags[i + 1] == 'objective-c') {
			return true
		}
	}
	return false
}

fn ensure_c_object_file(obj_path string, source_language string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) !string {
	if os.exists(obj_path) {
		stats.direct_objects++
		return obj_path
	}
	source_file := c_source_from_object_file(obj_path) or {
		return error('missing C object ${obj_path}, and no adjacent .c/.cc/.cpp/.m/.mm/.S source was found')
	}
	return compile_cached_c_source_object(obj_path, source_file, source_language, support_flags,
		c99, pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)
}

fn ensure_c_source_object(source_file string, source_language string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) !string {
	if !os.exists(source_file) {
		return error('missing C source ${source_file}')
	}
	return compile_cached_c_source_object('${source_file}.o', source_file, source_language,
		support_flags, c99, pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)
}

fn c_source_language(source_file string, source_language string) string {
	if source_language.len > 0 && source_language != 'none' {
		return source_language
	}
	if source_file.ends_with('.mm') {
		return 'objective-c++'
	}
	if source_file.ends_with('.m') {
		return 'objective-c'
	}
	if source_file.ends_with('.cc') || source_file.ends_with('.cpp') {
		return 'c++'
	}
	return ''
}

fn compile_cached_c_source_object(obj_path string, source_file string, source_language string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) !string {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	language := c_source_language(source_file, source_language)
	is_cpp := language in ['c++', 'objective-c++']
	std_flag := if is_cpp {
		if c99 { '-std=c++11' } else { '-std=gnu++11' }
	} else {
		c_standard_flag(c99)
	}
	compiler := if is_cpp && c_compiler == 'cc' { 'c++' } else { c_compiler }
	mut args := [std_flag]
	args << target_args
	if pic_flag.len > 0 {
		args << pic_flag
	}
	args << '-w'
	args << support_flags
	if language.len > 0 {
		args << ['-x', language]
	}
	manifest_path := c_object_manifest_path(cache_dir, obj_path, compiler, args, target, mut stats)
	if cached_obj := valid_c_object_manifest(manifest_path, mut stats) {
		return cached_obj
	}
	stats.dependency_scans++
	dependencies := c_object_dependencies(compiler, args, source_file)
	stats.dependency_files += dependencies.files.len
	if dependencies.used_fallback {
		stats.dependency_scan_fallbacks++
		uncached_obj := os.join_path(uncached_dir,
			'dependency_scan_fallback_${os.getpid()}_${rand.ulid()}.o')
		trace_c_object_cache('bypass', os.base(obj_path),
			'dependency scan failed; using build-local object', dependencies.files.len)
		args << ['-o', uncached_obj, '-c', source_file]
		res := cmdexec.run(compiler, args)
		if res.exit_code != 0 {
			os.rm(uncached_obj) or {}
			return error('failed to build C object ${obj_path} from ${source_file}:\n${res.output}')
		}
		stats.temporary_objects << uncached_obj
		return uncached_obj
	}
	cache_key := c_object_cache_name(obj_path, compiler, args, dependencies.files, target, false, mut
		stats)
	cached_obj := os.join_path(cache_dir, cache_key)
	if os.exists(cached_obj) {
		stats.content_key_hits++
		trace_c_object_cache('hit', cache_key,
			'compiler, target, argv, and dependency contents matched', dependencies.files.len)
		write_c_object_manifest(manifest_path, cached_obj, dependencies.files, mut stats) or {}
		return cached_obj
	}
	stats.misses++
	trace_c_object_cache('miss', cache_key, 'no published content-key entry',
		dependencies.files.len)
	// Snapshot the exact arguments that produced cache_key so the post-compile
	// digest is computed over the same inputs (temp_obj/-c must not perturb it).
	key_args := args.clone()
	temp_obj := '${cached_obj}.tmp.${os.getpid()}.${rand.ulid()}'
	args << ['-o', temp_obj, '-c', source_file]
	res := cmdexec.run(compiler, args)
	if res.exit_code != 0 {
		os.rm(temp_obj) or {}
		return error('failed to build C object ${obj_path} from ${source_file}:\n${res.output}')
	}
	// Re-hash the inputs after compilation. If a source or header changed while
	// the compiler was running, the object no longer corresponds to cache_key;
	// publishing it would certify content it was not built from. Use it as a
	// build-local, uncached object instead.
	post_key := c_object_cache_name(obj_path, compiler, key_args, dependencies.files, target, true, mut
		stats)
	if post_key != cache_key {
		stats.input_snapshot_races++
		trace_c_object_cache('bypass', cache_key,
			'inputs changed during compilation; using build-local object', dependencies.files.len)
		uncached_obj := os.join_path(uncached_dir,
			'input_snapshot_race_${os.getpid()}_${rand.ulid()}.o')
		os.mv(temp_obj, uncached_obj) or {
			os.rm(temp_obj) or {}
			return error('failed to stage build-local C object ${uncached_obj}: ${err}')
		}
		stats.temporary_objects << uncached_obj
		return uncached_obj
	}
	os.mv(temp_obj, cached_obj) or {
		os.rm(temp_obj) or {}
		if !os.exists(cached_obj) {
			return error('failed to publish cached C object ${cached_obj}: ${err}')
		}
		stats.publish_races++
	}
	write_c_object_manifest(manifest_path, cached_obj, dependencies.files, mut stats) or {}
	return cached_obj
}

fn c_object_manifest_path(cache_dir string, obj_path string, compiler string, compile_args []string, target pref.Target, mut stats CObjectCacheStats) string {
	compiler_path, compiler_version := c_object_compiler_identity(compiler, mut stats)
	mut hash := u64(1469598103934665603)
	for identity in ['v3-c-object-manifest-v1', os.real_path(obj_path), compiler_path,
		compiler_version, target.os, target.arch, target.abi, target.endian, target.pointer_bits.str(),
		target.object_format, compile_args.join('\x00')] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return os.join_path(cache_dir, 'request_${hash.hex()}.manifest')
}

fn valid_c_object_manifest(manifest_path string, mut stats CObjectCacheStats) ?string {
	content := os.read_file(manifest_path) or { return none }
	lines := content.split_into_lines()
	if lines.len < 3 || lines[0] != 'format=v3-c-object-manifest-v1'
		|| !lines[1].starts_with('object=') {
		return none
	}
	object_path := lines[1].all_after('object=')
	if !os.is_file(object_path) {
		return none
	}
	mut dependency_count := 0
	for line in lines[2..] {
		if !line.starts_with('dependency=') {
			return none
		}
		entry := line.all_after('dependency=')
		tab := entry.last_index('\t') or { return none }
		path := entry[..tab]
		expected_signature := entry[tab + 1..]
		if path.len == 0 || expected_signature.len == 0 {
			return none
		}
		actual_signature := c_object_file_signature(path, false, mut stats)
		if actual_signature.len == 0 || actual_signature != expected_signature {
			return none
		}
		dependency_count++
	}
	if dependency_count == 0 {
		return none
	}
	stats.dependency_manifest_hits++
	stats.content_key_hits++
	stats.dependency_files += dependency_count
	trace_c_object_cache('hit', os.base(object_path),
		'compiler, target, argv, and dependency contents matched via manifest', dependency_count)
	return object_path
}

fn write_c_object_manifest(manifest_path string, object_path string, dependencies []string, mut stats CObjectCacheStats) ! {
	mut out := strings.new_builder(128 + dependencies.len * 96)
	out.writeln('format=v3-c-object-manifest-v1')
	out.writeln('object=${object_path}')
	for dependency in dependencies {
		signature := c_object_file_signature(dependency, false, mut stats)
		if signature.len == 0 {
			return error('failed to sign C object dependency ${dependency}')
		}
		out.writeln('dependency=${dependency}\t${signature}')
	}
	temp_path := '${manifest_path}.tmp.${os.getpid()}.${rand.ulid()}'
	defer {
		os.rm(temp_path) or {}
	}
	os.write_file(temp_path, out.str())!
	os.mv(temp_path, manifest_path)!
}

fn c_object_compiler_identity(compiler string, mut stats CObjectCacheStats) (string, string) {
	compiler_path := os.find_abs_path_of_executable(compiler) or { compiler }
	if compiler_path in stats.compiler_versions {
		return compiler_path, stats.compiler_versions[compiler_path]
	}
	version := cmdexec.run(compiler, ['--version']).output
	stats.compiler_versions[compiler_path] = version
	return compiler_path, version
}

fn c_object_file_signature(path string, refresh bool, mut stats CObjectCacheStats) string {
	canonical := os.real_path(path)
	if refresh {
		stats.file_signatures.delete(canonical)
	} else if canonical in stats.file_signatures {
		return stats.file_signatures[canonical]
	}
	content := os.read_bytes(canonical) or { return '' }
	signature := c_hash_bytes(u64(1469598103934665603), content).hex()
	stats.file_signatures[canonical] = signature
	stats.dependency_file_reads++
	return signature
}

fn trace_c_object_cache(status string, key string, reason string, dependency_count int) {
	if os.getenv('V3_CACHE_TRACE') == '' {
		return
	}
	println('  C object cache ${status}: key=${key} reason=${reason} dependencies=${dependency_count}')
}

fn c_object_dependencies(compiler string, compile_args []string, source_file string) CObjectDependencies {
	mut args := compile_args.clone()
	mt_target := 'v3cache'
	marker := '${mt_target}:'
	args << ['-M', '-MT', mt_target, source_file]
	result := cmdexec.run(compiler, args)
	// Fail closed: any output we cannot fully and unambiguously interpret must
	// use a build-local, uncached object. A malformed or unexpected depfile that
	// is silently accepted as a valid, source-only dependency set would let a
	// later build certify a stale object as current.
	fallback := CObjectDependencies{
		files:         [source_file]
		used_fallback: true
	}
	if result.exit_code != 0 {
		return fallback
	}
	if !result.output.contains(marker) {
		// The `-MT` target marker is missing, so `all_after` would return the
		// entire compiler output and tokenize it as bogus dependencies.
		return fallback
	}
	continuation := '\\' + '\n'
	dep_text := result.output.replace(continuation, ' ').all_after(marker)
	dependencies := cmdexec.split_args(dep_text) or { return fallback }
	if dependencies.len == 0 {
		return fallback
	}
	// Every listed path must resolve to a readable file, and the source file
	// itself must be among them; otherwise the dependency set is untrustworthy.
	source_real := os.real_path(source_file)
	mut canonical_deps := []string{cap: dependencies.len}
	mut saw_source := false
	for dep in dependencies {
		if dep.len == 0 {
			continue
		}
		canonical := os.real_path(dep)
		if !os.is_file(canonical) {
			return fallback
		}
		canonical_deps << dep
		if canonical == source_real {
			saw_source = true
		}
	}
	if !saw_source {
		return fallback
	}
	canonical_deps.sort()
	return CObjectDependencies{
		files: canonical_deps
	}
}

fn c_source_from_object_file(obj_path string) ?string {
	base := obj_path.all_before_last('.')
	for ext in ['.c', '.cc', '.cpp', '.m', '.mm', '.S'] {
		source_file := base + ext
		if os.exists(source_file) {
			return source_file
		}
	}
	return none
}

fn c_object_cache_name(path string, compiler string, compile_args []string, dependencies []string, target pref.Target, refresh bool, mut stats CObjectCacheStats) string {
	base := os.base(path).replace_each(['/', '_', '\\', '_', ':', '_', '.', '_', ' ', '_'])
	compiler_path, compiler_version := c_object_compiler_identity(compiler, mut stats)
	mut hash := u64(1469598103934665603)
	for identity in [os.real_path(path), compiler_path, compiler_version, target.os, target.arch,
		target.abi, target.endian, target.pointer_bits.str(), target.object_format, compile_args.join('\x00')] {
		hash = c_hash_bytes(hash, identity.bytes())
	}
	for dependency in dependencies {
		canonical := os.real_path(dependency)
		hash = c_hash_bytes(hash, canonical.bytes())
		signature := c_object_file_signature(canonical, refresh, mut stats)
		hash = c_hash_bytes(hash, signature.bytes())
	}
	return '${base}_${hash.hex()}.o'
}

fn c_hash_bytes(initial u64, data []u8) u64 {
	mut hash := initial
	for byte in data {
		hash = (hash ^ u64(byte)) * u64(1099511628211)
	}
	return hash
}

fn c_flag_is_object_file(flag string) bool {
	return flag.ends_with('.o') || flag.ends_with('.obj')
}

fn c_flag_is_c_source_file(flag string) bool {
	return flag.ends_with('.c') || flag.ends_with('.cc') || flag.ends_with('.cpp')
		|| flag.ends_with('.m') || flag.ends_with('.mm')
}

fn c_standard_flag(c99 bool) string {
	return if c99 { '-std=c99' } else { '-std=gnu11' }
}

fn shared_pic_flag(is_shared bool, target_os string) string {
	if is_shared && target_os != 'windows' {
		return '-fPIC'
	}
	return ''
}

fn c_compiler_target_args(target pref.Target, compiler_explicit bool) ![]string {
	host := pref.host_target()
	if target.os == host.os && target.arch == host.arch {
		return []string{}
	}
	if target.os == 'macos' && host.os == 'macos' && target.arch in ['amd64', 'arm64'] {
		arch := if target.arch == 'amd64' { 'x86_64' } else { 'arm64' }
		return ['-arch', arch]
	}
	if compiler_explicit {
		// An explicitly selected compiler may already encode its target in its name or defaults.
		return []string{}
	}
	return error('linking target ${target.os}/${target.arch} from host ${host.os}/${host.arch} is not supported by the default C compiler; use -o file.c and compile it with a target toolchain')
}

fn cleanup_c_build_dir(dir string) {
	if dir.len > 0 {
		os.rmdir_all(dir) or {}
	}
}

fn run_test_binary(bin_file string) int {
	return run_binary(bin_file, []string{})
}

fn run_binary(bin_file string, args []string) int {
	run_path := executable_path_for_run(bin_file)
	mut process := os.new_process(run_path)
	process.set_args(args)
	// `v3 run` is interactive: leave all three standard streams inherited so
	// prompts are visible immediately and the program can read the caller's stdin.
	process.wait()
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	return exit_code
}

fn executable_path_for_run(path string) string {
	mut run_path := path
	if !os.is_abs_path(path) && !path.contains('/') && !path.contains('\\') {
		run_path = '.' + os.path_separator + path
	}
	return run_path
}

fn input_implies_building_v(input_file string) bool {
	normalized := input_file.replace('\\', '/').trim_right('/')
	if normalized.all_after_last('/') == 'v3.v' {
		return true
	}
	if os.is_dir(input_file) {
		normalized_dir := os.real_path(input_file).replace('\\', '/').trim_right('/')
		return normalized_dir.ends_with('/vlib/v3')
	}
	return false
}

fn input_is_cmd_v(input_file string) bool {
	normalized := input_file.replace('\\', '/').trim_right('/')
	return normalized == 'cmd/v' || normalized.ends_with('/cmd/v')
		|| normalized.ends_with('/cmd/v/v.v')
}

fn default_bin_file_for_input(input_file string) string {
	if os.is_dir(input_file) {
		return os.base(os.real_path(input_file))
	}
	if input_file.ends_with('.v') {
		return input_file.all_before_last('.v')
	}
	return input_file
}

fn cli_usage() string {
	return 'usage: v3 [run|test] <file.v|directory> [options]\n' +
		'  -o <output>                 output binary or C file\n' +
		'  -b <c|arm64|wasm|eval>      backend\n' +
		'  -os <name> -arch <name>     target platform\n' +
		'  -cc <compiler>               C compiler executable\n' +
		'  -prod -c99 -shared -strict  C build modes\n' +
		'  -v                           verbose stage profiling\n' +
		'  -no-memory-limit             disable the 10 GiB memory safety limit\n' +
		'  -d <name>                    compile-time define'
}

fn shared_library_postfix(target_os string) string {
	return match pref.normalized_os(target_os) {
		'windows' { '.dll' }
		'macos', 'ios' { '.dylib' }
		else { '.so' }
	}
}

fn with_shared_library_postfix(path string, target_os string) string {
	postfix := shared_library_postfix(target_os)
	if path.ends_with(postfix) {
		return path
	}
	return path + postfix
}

// should_scope_prealloc_stages reports whether compiler stages can use disposable arenas.
// Every stage that publishes data into the compilation state promotes that data before its
// scratch arena is released, so this is safe for both self-host and user-program builds.
fn should_scope_prealloc_stages() bool {
	$if prealloc {
		return true
	}
	return false
}

// should_scope_prealloc_cgen reports whether cgen scratch/output chunks can use disposable
// arenas. Unlike transform metadata, cgen state does not escape after its flags are copied.
fn should_scope_prealloc_cgen() bool {
	$if prealloc {
		return true
	}
	return false
}

fn should_parallel_monomorphize() bool {
	return os.getenv('V3_DISABLE_PARALLEL_MONOMORPHIZE') != '1'
}

fn ownership_checker_compiled() bool {
	$if ownership ? {
		return true
	}
	return false
}

// prealloc_scope_begin_for_v3 starts a disposable prealloc scope when available.
fn prealloc_scope_begin_for_v3() voidptr {
	$if prealloc {
		return unsafe { prealloc_scope_begin() }
	} $else {
		return unsafe { nil }
	}
}

// prealloc_scope_leave_for_v3 restores the previous prealloc scope after a stage.
fn prealloc_scope_leave_for_v3(scope voidptr) {
	$if prealloc {
		unsafe { prealloc_scope_leave(scope) }
	}
}

// prealloc_scope_free_for_v3 releases a disposable prealloc scope after survivors are cloned.
fn prealloc_scope_free_for_v3(scope voidptr) {
	$if prealloc {
		unsafe { prealloc_scope_free_after(scope) }
	}
}

// clone_string_list clones a string slice out of a scoped prealloc arena.
fn clone_string_list(values []string) []string {
	if values.len == 0 {
		return []string{}
	}
	mut cloned := []string{cap: values.len}
	for value in values {
		cloned << value.clone()
	}
	return cloned
}

fn v3_cgen_cache_input(state &V3ModuleCacheState, user_files []string, user_c_flags []string) V3CgenCacheInput {
	mut source_set := map[string]bool{}
	for file in user_files {
		source_set[os.real_path(file)] = true
	}
	mut dependencies := map[string]string{}
	mut module_names := state.module_sources.keys()
	module_names.sort()
	for module_name in module_names {
		source_files := state.module_sources[module_name]
		entry := state.manager.entry(module_name, source_files)
		mut source_paths := source_files.map(os.real_path(it))
		source_paths.sort()
		dependencies['module:${module_name}'] =
			modulecache.header_signature(source_paths.join('\n'))
		if header := state.headers[module_name] {
			dependencies[entry.header] = modulecache.header_signature(header)
		} else if os.is_file(entry.header) {
			dependencies[entry.header] = modulecache.file_signature(entry.header)
		}
		if os.is_file(entry.header_stamp) {
			dependencies[entry.header_stamp] = modulecache.file_signature(entry.header_stamp)
		}
	}
	mut external_input_modules := state.module_external_inputs.keys()
	external_input_modules.sort()
	for module_name in external_input_modules {
		mut paths := state.module_external_inputs[module_name].clone()
		paths.sort()
		for path in paths {
			dependencies['external:${module_name}:${path}'] = modulecache.file_signature(path)
		}
	}
	mut source_files := source_set.keys()
	source_files.sort()
	return V3CgenCacheInput{
		source_files:         source_files
		dependency_inputs:    dependencies
		generation_signature: user_c_flags.join('\x00')
	}
}

fn prepare_v3_cache_external_inputs(mut state V3ModuleCacheState, a &flat.FlatAst, prefs &pref.Preferences, user_c_flags []string) bool {
	mut cache_input_modules := map[string]bool{}
	for module_name in state.module_sources.keys() {
		cache_input_modules[module_name] = true
	}
	cache_input_modules['main'] = true
	external_inputs, has_untracked_c_include := cgen.cache_external_input_files(a, prefs.vroot,
		cache_input_modules, user_c_flags, prefs.target)
	state.module_external_inputs = external_inputs.clone()
	native_source_modules, can_scope_static_inputs := cache_external_input_owner_modules(state)
	state.native_source_modules = native_source_modules.clone()
	return !has_untracked_c_include && can_scope_static_inputs
}

fn encode_v3_cgen_metadata(flags []string, interface_impl_signature string, prefix_source_identity string) string {
	mut parts := ['v3-cgen-metadata-v3', interface_impl_signature, prefix_source_identity]
	parts << flags
	return parts.join('\x00')
}

fn decode_v3_cgen_metadata(metadata string) ?V3CgenCacheMetadata {
	parts := metadata.split('\x00')
	if parts.len < 3 || parts[0] != 'v3-cgen-metadata-v3' {
		return none
	}
	return V3CgenCacheMetadata{
		interface_impl_signature: parts[1]
		prefix_source_identity:   parts[2]
		flags:                    parts[3..].clone()
	}
}

fn cacheable_runtime_string_nodes(a &flat.FlatAst) []bool {
	mut cacheable := []bool{len: a.nodes.len}
	mut stack := []flat.NodeId{cap: 256}
	mut blocked := []bool{cap: 256}
	for idx, node in a.nodes {
		if node.kind !in [.fn_decl, .c_fn_decl] {
			continue
		}
		stack.clear()
		blocked.clear()
		stack << flat.NodeId(idx)
		blocked << false
		for stack.len > 0 {
			id := stack.pop()
			parent_blocked := blocked.pop()
			node_idx := int(id)
			if node_idx < 0 || node_idx >= a.nodes.len {
				continue
			}
			current := a.nodes[node_idx]
			current_blocked := parent_blocked
				|| current.kind in [.comptime_if, .comptime_for, .directive, .asm_stmt, .sql_expr]
				|| (current.kind == .call && (current.value.starts_with('$')
				|| current.value in ['embed_file', 'tmpl', 'env', 'd', 'res', 'pkgconfig', 'compile_error', 'compile_warn']))
			if current.kind == .string_literal && !current_blocked {
				cacheable[node_idx] = true
			}
			for child_idx in 0 .. current.children_count {
				stack << a.child(&current, child_idx)
				blocked << current_blocked
			}
		}
	}
	return cacheable
}

fn monomorph_cache_runtime_strings(a &flat.FlatAst) []string {
	cacheable := cacheable_runtime_string_nodes(a)
	mut values := []string{}
	for idx, can_cache in cacheable {
		if can_cache {
			values << a.nodes[idx].value
		}
	}
	return values
}

fn monomorph_cache_semantic_signature(a &flat.FlatAst) string {
	cacheable_strings := cacheable_runtime_string_nodes(a)
	mut hash := u64(1469598103934665603)
	for idx, node in a.nodes {
		hash = c_hash_bytes(hash, [u8(node.kind), u8(node.op), u8(node.is_mut),
			u8(node.skip_ownership_drops)])
		hash = c_hash_tag(hash, node.children_start)
		hash = c_hash_tag(hash, node.children_count)
		hash = c_hash_bytes(hash, node.typ.bytes())
		hash = c_hash_bytes(hash, [u8(0)])
		if !cacheable_strings[idx] {
			hash = c_hash_bytes(hash, node.value.bytes())
		}
		hash = c_hash_bytes(hash, [u8(0xff)])
		for param in node.generic_params() {
			hash = c_hash_bytes(hash, param.bytes())
			hash = c_hash_bytes(hash, [u8(0xfe)])
		}
	}
	hash = c_hash_tag(hash, a.user_code_start)
	for child in a.children {
		hash = c_hash_tag(hash, int(child))
	}
	return c_hash_function_metadata(hash, a).hex()
}

fn c_hash_function_metadata(initial u64, a &flat.FlatAst) u64 {
	mut hash := initial
	mut disabled_names := a.disabled_fns.keys()
	disabled_names.sort()
	for name in disabled_names {
		hash = c_hash_bytes(hash, name.bytes())
		hash = c_hash_bytes(hash, [u8(a.disabled_fns[name]), u8(0xfd)])
	}
	mut export_names := a.export_fn_names.keys()
	export_names.sort()
	for name in export_names {
		hash = c_hash_bytes(hash, name.bytes())
		hash = c_hash_bytes(hash, [u8(0xfc)])
		hash = c_hash_bytes(hash, a.export_fn_names[name].bytes())
		hash = c_hash_bytes(hash, [u8(0xfb)])
	}
	mut noreturn_names := a.noreturn_fns.keys()
	noreturn_names.sort()
	for name in noreturn_names {
		hash = c_hash_bytes(hash, name.bytes())
		hash = c_hash_bytes(hash, [u8(a.noreturn_fns[name]), u8(0xfa)])
	}
	return hash
}

@[inline]
fn c_hash_tag(initial u64, value int) u64 {
	mut hash := initial
	mut bits := u64(value)
	for _ in 0 .. 8 {
		hash = (hash ^ (bits & 0xff)) * u64(1099511628211)
		bits >>= 8
	}
	return hash
}

struct V3IncrementalFn {
	key       string
	name      string
	signature string
}

struct V3IncrementalSnapshot {
	declaration_signature string
	functions             []V3IncrementalFn
}

fn incremental_cache_fn_key(file string, module_name string, name string) string {
	mut hash := u64(1469598103934665603)
	for part in [file, module_name, name] {
		hash = c_hash_bytes(hash, part.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

fn incremental_qualified_fn_name(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn incremental_hash_node_header(initial u64, node &flat.Node, include_value bool) u64 {
	mut hash := c_hash_bytes(initial, [u8(node.kind), u8(node.op), u8(node.is_mut),
		u8(node.skip_ownership_drops)])
	hash = c_hash_tag(hash, node.children_count)
	hash = c_hash_bytes(hash, node.typ.bytes())
	hash = c_hash_bytes(hash, [u8(0)])
	if include_value {
		hash = c_hash_bytes(hash, node.value.bytes())
	}
	hash = c_hash_bytes(hash, [u8(0xff)])
	for param in node.generic_params() {
		hash = c_hash_bytes(hash, param.bytes())
		hash = c_hash_bytes(hash, [u8(0xfe)])
	}
	return hash
}

fn incremental_hash_fn_declaration(initial u64, node &flat.Node) u64 {
	mut hash := c_hash_bytes(initial, [u8(node.kind), u8(node.op), u8(node.is_mut),
		u8(node.skip_ownership_drops)])
	hash = c_hash_bytes(hash, node.typ.bytes())
	hash = c_hash_bytes(hash, [u8(0)])
	hash = c_hash_bytes(hash, node.value.bytes())
	hash = c_hash_bytes(hash, [u8(0xff)])
	for param in node.generic_params() {
		hash = c_hash_bytes(hash, param.bytes())
		hash = c_hash_bytes(hash, [u8(0xfe)])
	}
	return hash
}

fn incremental_node_tree_signature(a &flat.FlatAst, root flat.NodeId) string {
	mut hash := u64(1469598103934665603)
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= a.nodes.len {
			hash = c_hash_tag(hash, -1)
			continue
		}
		node := &a.nodes[idx]
		hash = incremental_hash_node_header(hash, node, true)
		for child_idx := node.children_count - 1; child_idx >= 0; child_idx-- {
			stack << a.child(node, child_idx)
		}
	}
	return hash.hex()
}

fn incremental_top_level_nodes(a &flat.FlatAst) []bool {
	mut result := []bool{len: a.nodes.len}
	for node in a.nodes {
		if node.kind != .file {
			continue
		}
		for child_idx in 0 .. node.children_count {
			id := int(a.child(&node, child_idx))
			if id >= 0 && id < result.len {
				result[id] = true
			}
		}
	}
	return result
}

fn incremental_program_snapshot(a &flat.FlatAst) V3IncrementalSnapshot {
	mut declaration_parts := []string{}
	mut synthetic_main_parts := []string{}
	mut functions := []V3IncrementalFn{}
	mut cur_file := ''
	mut cur_module := ''
	top_level_nodes := incremental_top_level_nodes(a)
	for idx, node in a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				key := incremental_cache_fn_key(cur_file, cur_module, node.value)
				functions << V3IncrementalFn{
					key:       key
					name:      incremental_qualified_fn_name(cur_module, node.value)
					signature: incremental_node_tree_signature(a, flat.NodeId(idx))
				}
				mut declaration := strings.new_builder(128)
				declaration.write_string('fn\t${cur_file}\t${cur_module}\t')
				declaration.write_string(incremental_hash_fn_declaration(u64(1469598103934665603),
					&node).hex())
				for child_idx in 0 .. node.children_count {
					child_id := a.child(&node, child_idx)
					child := a.nodes[int(child_id)]
					if child.kind == .param {
						declaration.write_u8(`\t`)
						declaration.write_string(incremental_hash_node_header(u64(1469598103934665603),
							&child, true).hex())
					}
				}
				declaration_parts << declaration.str()
			}
			.struct_decl, .global_decl, .const_decl, .enum_decl, .type_decl, .interface_decl,
			.import_decl, .c_fn_decl {
				declaration_parts << '${node.kind}\t${cur_file}\t${cur_module}\t${incremental_node_tree_signature(a,
					flat.NodeId(idx))}'
			}
			.directive, .comptime_if, .comptime_for, .asm_stmt, .sql_expr, .fn_literal {
				if top_level_nodes[idx] {
					declaration_parts << '${node.kind}\t${cur_file}\t${cur_module}\t${incremental_node_tree_signature(a,
						flat.NodeId(idx))}'
				}
			}
			else {
				if top_level_nodes[idx] {
					synthetic_main_parts << 'synthetic-main\t${node.kind}\t${cur_file}\t${cur_module}\t${incremental_node_tree_signature(a,
						flat.NodeId(idx))}'
				}
			}
		}
	}
	declaration_parts.sort()
	mut declaration_hash := u64(1469598103934665603)
	for part in declaration_parts {
		declaration_hash = c_hash_bytes(declaration_hash, part.bytes())
		declaration_hash = c_hash_bytes(declaration_hash, [u8(0xff)])
	}
	declaration_hash = c_hash_bytes(declaration_hash, 'ordered-synthetic-main'.bytes())
	for part in synthetic_main_parts {
		declaration_hash = c_hash_bytes(declaration_hash, part.bytes())
		declaration_hash = c_hash_bytes(declaration_hash, [u8(0xff)])
	}
	declaration_hash = c_hash_function_metadata(declaration_hash, a)
	return V3IncrementalSnapshot{
		declaration_signature: declaration_hash.hex()
		functions:             functions
	}
}

fn encode_incremental_manifest(snapshot V3IncrementalSnapshot) string {
	mut lines := []string{cap: snapshot.functions.len + 1}
	lines << 'v3-incremental-functions-v1'
	for function in snapshot.functions {
		lines << '${function.key}\t${function.signature}\t${function.name}'
	}
	return lines.join('\n')
}

fn decode_incremental_manifest(encoded string) ?map[string]string {
	lines := encoded.split_into_lines()
	if lines.len == 0 || lines[0] != 'v3-incremental-functions-v1' {
		return none
	}
	mut signatures := map[string]string{}
	for line in lines[1..] {
		parts := line.split('\t')
		if parts.len != 3 || parts[0].len == 0 || parts[1].len == 0 {
			return none
		}
		signatures[parts[0]] = parts[1]
	}
	return signatures
}

fn incremental_changed_functions(snapshot V3IncrementalSnapshot, old map[string]string) ?([]string, map[string]bool) {
	if snapshot.functions.len != old.len {
		return none
	}
	mut keys := []string{}
	mut names := map[string]bool{}
	for function in snapshot.functions {
		old_signature := old[function.key] or { return none }
		if old_signature != function.signature {
			keys << function.key
			names[function.name] = true
		}
	}
	return keys, names
}

fn incremental_c_function_sections(source string) ?map[string]string {
	begin_prefix := '/* V3CACHE_FN_BEGIN '
	end_prefix := '/* V3CACHE_FN_END '
	mut sections := map[string]string{}
	mut offset := 0
	for {
		relative_start := source[offset..].index(begin_prefix) or { break }
		start := offset + relative_start
		key_start := start + begin_prefix.len
		key_end_relative := source[key_start..].index(' */') or { return none }
		key_end := key_start + key_end_relative
		key := source[key_start..key_end]
		end_marker := '${end_prefix}${key} */'
		end_relative := source[key_end..].index(end_marker) or { return none }
		mut end := key_end + end_relative + end_marker.len
		if end < source.len && source[end] == `\n` {
			end++
		}
		sections[key] = source[start..end]
		offset = end
	}
	if sections.len == 0 {
		return none
	}
	return sections
}

fn merge_incremental_program_body(cached_source string, changed_source string, changed_keys []string) ?string {
	cached_sections := incremental_c_function_sections(cached_source) or { return none }
	changed_sections := incremental_c_function_sections(changed_source) or { return none }
	mut merged := cached_source
	for key in changed_keys {
		old_section := cached_sections[key] or { return none }
		new_section := changed_sections[key] or { return none }
		merged = merged.replace(old_section, new_section)
	}
	support_declarations := incremental_c_support_declarations(changed_source) or { return none }
	new_definitions := modulecache.static_string_definitions(changed_source)
	mut additions := strings.new_builder(support_declarations.len + new_definitions.len)
	if support_declarations.trim_space().len > 0 {
		additions.write_string(support_declarations)
		if !support_declarations.ends_with('\n') {
			additions.writeln('')
		}
	}
	for line in new_definitions.split_into_lines() {
		if line.len > 0 && !merged.contains(line) {
			additions.writeln(line)
		}
	}
	new_text := additions.str()
	if new_text.len == 0 {
		return merged
	}
	marker := '/* V3CACHE_BODY_BEGIN */'
	marker_idx := merged.index(marker) or { return none }
	return merged[..marker_idx] + new_text + merged[marker_idx..]
}

fn incremental_c_support_declarations(source string) ?string {
	begin_marker := '/* V3CACHE_SUPPORT_BEGIN */'
	end_marker := '/* V3CACHE_SUPPORT_END */'
	begin := source.index(begin_marker) or { return none }
	content_start := begin + begin_marker.len
	relative_end := source[content_start..].index(end_marker) or { return none }
	return source[content_start..content_start + relative_end]
}

fn incremental_static_string_markers(source string) string {
	definitions := modulecache.static_string_definitions(source)
	mut out := strings.new_builder(definitions.len + 256)
	for line in definitions.split_into_lines() {
		if line.len > 0 {
			out.writeln('// V3CACHE_BASELINE ${line}')
		}
	}
	return out.str()
}

fn encode_cached_runtime_strings(values []string) string {
	mut out := strings.new_builder(values.len * 16)
	out.write_string('v3-runtime-strings-v1\n')
	for value in values {
		out.write_string(value.len.str())
		out.write_u8(`:`)
		out.write_string(value)
	}
	return out.str()
}

fn decode_cached_runtime_strings(encoded string) ?[]string {
	header := 'v3-runtime-strings-v1\n'
	if !encoded.starts_with(header) {
		return none
	}
	mut values := []string{}
	mut i := header.len
	for i < encoded.len {
		start := i
		for i < encoded.len && encoded[i] >= `0` && encoded[i] <= `9` {
			i++
		}
		if i == start || i >= encoded.len || encoded[i] != `:` {
			return none
		}
		size := encoded[start..i].int()
		i++
		if size < 0 || i + size > encoded.len {
			return none
		}
		values << encoded[i..i + size]
		i += size
	}
	return values
}

fn encode_monomorph_cache_specs(specs []transform.MonomorphCacheSpec) string {
	mut lines := []string{cap: specs.len + 1}
	lines << 'v3-monomorph-specs-v1'
	for spec in specs {
		if spec.module in ['', 'main'] {
			continue
		}
		lines << '${spec.decl_key}\t${spec.module}\t${spec.args.join('\x1f')}'
	}
	return lines.join('\n')
}

fn decode_monomorph_cache_specs(encoded string) []transform.MonomorphCacheSpec {
	lines := encoded.split_into_lines()
	if lines.len == 0 || lines[0] != 'v3-monomorph-specs-v1' {
		return []transform.MonomorphCacheSpec{}
	}
	mut specs := []transform.MonomorphCacheSpec{cap: lines.len - 1}
	for line in lines[1..] {
		parts := line.split('\t')
		if parts.len != 3 || parts[0].len == 0 || parts[1].len == 0 {
			continue
		}
		raw_args := if parts[2].len == 0 { []string{} } else { parts[2].split('\x1f') }
		specs << transform.MonomorphCacheSpec{
			decl_key: parts[0].clone()
			module:   parts[1].clone()
			args:     clone_string_list(raw_args)
		}
	}
	return specs
}

fn encode_cached_used_fns(used map[string]bool) string {
	mut names := []string{cap: used.len}
	for name, is_used in used {
		if is_used && name.len > 0 {
			names << name
		}
	}
	names.sort()
	return 'v3-used-fns-v1\n' + names.join('\n')
}

fn decode_cached_used_fns(encoded string) map[string]bool {
	lines := encoded.split_into_lines()
	mut used := map[string]bool{}
	if lines.len == 0 || lines[0] != 'v3-used-fns-v1' {
		return used
	}
	for name in lines[1..] {
		if name.len > 0 {
			used[name] = true
		}
	}
	return used
}

fn clone_monomorph_cache_specs(specs []transform.MonomorphCacheSpec) []transform.MonomorphCacheSpec {
	mut cloned := []transform.MonomorphCacheSpec{cap: specs.len}
	for spec in specs {
		cloned << transform.MonomorphCacheSpec{
			decl_key: spec.decl_key.clone()
			module:   spec.module.clone()
			args:     clone_string_list(spec.args)
		}
	}
	return cloned
}

// clone_string_bool_map promotes a string-keyed set out of a disposable stage arena.
fn clone_string_bool_map(values map[string]bool) map[string]bool {
	mut cloned := map[string]bool{}
	for key, value in values {
		cloned[key.clone()] = value
	}
	return cloned
}

fn clone_string_string_map(values map[string]string) map[string]string {
	mut cloned := map[string]string{}
	for key, value in values {
		cloned[key.clone()] = value.clone()
	}
	return cloned
}

fn scoped_value_owned(scope voidptr, ptr voidptr) bool {
	$if prealloc {
		return unsafe { prealloc_scope_owns(scope, ptr) }
	}
	return false
}

fn promote_scoped_node(mut node flat.Node, scope voidptr) {
	if node.value.len > 0 && scoped_value_owned(scope, node.value.str) {
		node.value = node.value.clone()
	}
	if node.typ.len > 0 && scoped_value_owned(scope, node.typ.str) {
		node.typ = node.typ.clone()
	}
	old_params := node.generic_params()
	if old_params.len == 0 {
		return
	}
	mut needs_promotion := scoped_value_owned(scope, node.payload)
		|| scoped_value_owned(scope, old_params.data)
	if !needs_promotion {
		for param in old_params {
			if param.len > 0 && scoped_value_owned(scope, param.str) {
				needs_promotion = true
				break
			}
		}
	}
	if !needs_promotion {
		return
	}
	mut params := []string{cap: old_params.len}
	for param in old_params {
		params << if param.len > 0 && scoped_value_owned(scope, param.str) {
			param.clone()
		} else {
			param
		}
	}
	node.set_generic_params(params)
}

fn promote_scoped_ast_nodes(mut ast flat.FlatAst, base_nodes int, new_end int, owned_base_nodes []int, scope voidptr) {
	for idx in owned_base_nodes {
		if idx >= 0 && idx < base_nodes && idx < ast.nodes.len {
			promote_scoped_node(mut ast.nodes[idx], scope)
		}
	}
	limit := if new_end < ast.nodes.len { new_end } else { ast.nodes.len }
	for idx in base_nodes .. limit {
		promote_scoped_node(mut ast.nodes[idx], scope)
	}
}

fn canonicalize_scoped_node(mut ast flat.FlatAst, idx int, scope voidptr) {
	if idx < 0 || idx >= ast.nodes.len {
		return
	}
	mut node := unsafe { &ast.nodes[idx] }
	if node.value.len > 0 && scoped_value_owned(scope, node.value.str) {
		_, node.value = ast.intern_text(node.value)
	}
	if node.typ.len > 0 && scoped_value_owned(scope, node.typ.str) {
		_, node.typ = ast.intern_text(node.typ)
	}
	old_params := node.generic_params()
	if old_params.len == 0 {
		return
	}
	mut needs_params := scoped_value_owned(scope, node.payload)
		|| scoped_value_owned(scope, old_params.data)
	if !needs_params {
		for param in old_params {
			if param.len > 0 && scoped_value_owned(scope, param.str) {
				needs_params = true
				break
			}
		}
	}
	if !needs_params {
		return
	}
	mut params := []string{cap: old_params.len}
	for param in old_params {
		if param.len > 0 && scoped_value_owned(scope, param.str) {
			_, canonical := ast.intern_text(param)
			params << canonical
		} else {
			params << param
		}
	}
	node.set_generic_params(params)
}

fn canonicalize_scoped_transform_region(mut ast flat.FlatAst, region transform.ScopedTransformRegion) {
	canonicalize_scoped_transform_region_from_scope(mut ast, region, region.scope)
}

fn canonicalize_scoped_transform_region_from_scope(mut ast flat.FlatAst, region transform.ScopedTransformRegion, scope voidptr) {
	for idx in region.base_nodes {
		canonicalize_scoped_node(mut ast, idx, scope)
	}
	limit := if region.new_end < ast.nodes.len { region.new_end } else { ast.nodes.len }
	for idx in region.new_start .. limit {
		canonicalize_scoped_node(mut ast, idx, scope)
	}
}

fn clone_scoped_transform_regions(regions []transform.ScopedTransformRegion) []transform.ScopedTransformRegion {
	mut cloned := []transform.ScopedTransformRegion{cap: regions.len}
	for region in regions {
		cloned << transform.ScopedTransformRegion{
			scope:      region.scope
			new_start:  region.new_start
			new_end:    region.new_end
			base_nodes: region.base_nodes.clone()
		}
	}
	return cloned
}

fn clone_flat_node_owned(node flat.Node) flat.Node {
	return node.clone_owned()
}

fn clone_flat_ast_after_transform(ast &flat.FlatAst) &flat.FlatAst {
	mut nodes := []flat.Node{cap: ast.nodes.len}
	for node in ast.nodes {
		nodes << clone_flat_node_owned(node)
	}
	mut children := []flat.NodeId{cap: ast.children.len}
	children << ast.children
	text_values, text_ids := ast.clone_text_table_owned()
	return &flat.FlatAst{
		nodes:                nodes
		children:             children
		user_code_start:      ast.user_code_start
		disabled_fns:         ast.disabled_fns
		export_fn_names:      ast.export_fn_names
		noreturn_fns:         ast.noreturn_fns
		source_files:         ast.source_files
		source_buffers:       ast.source_buffers
		text_values:          text_values
		text_ids:             text_ids
		worker_pool:          ast.worker_pool
		specialized_fn_nodes: ast.specialized_fn_nodes.clone()
	}
}

fn reserve_flat_ast_exact(mut ast flat.FlatAst, nodes_cap int, children_cap int) {
	if nodes_cap > ast.nodes.cap {
		old_nodes := ast.nodes
		mut nodes := []flat.Node{cap: nodes_cap}
		nodes << old_nodes
		ast.nodes = nodes
	}
	if children_cap > ast.children.cap {
		old_children := ast.children
		mut children := []flat.NodeId{cap: children_cap}
		children << old_children
		ast.children = children
	}
}

fn clone_flat_ast_storage(mut ast flat.FlatAst) {
	old_nodes := ast.nodes
	mut nodes := []flat.Node{cap: old_nodes.cap}
	nodes << old_nodes
	ast.nodes = nodes
	old_children := ast.children
	mut children := []flat.NodeId{cap: old_children.cap}
	children << old_children
	ast.children = children
	ast.specialized_fn_nodes = ast.specialized_fn_nodes.clone()
}

fn clone_int_string_map(values map[int]string) map[int]string {
	mut cloned := map[int]string{}
	for idx, value in values {
		cloned[idx] = value.clone()
	}
	return cloned
}

fn clone_int_type_map(values map[int]types.Type) map[int]types.Type {
	mut cloned := map[int]types.Type{}
	for idx, value in values {
		cloned[idx] = types.clone_owned_type(value)
	}
	return cloned
}

fn clone_struct_field_map(values map[string][]types.StructField) map[string][]types.StructField {
	mut cloned := map[string][]types.StructField{}
	for name, fields in values {
		mut owned_fields := []types.StructField{cap: fields.len}
		for field in fields {
			owned_fields << types.StructField{
				name: field.name.clone()
				typ:  types.clone_owned_type(field.typ)
			}
		}
		cloned[name.clone()] = owned_fields
	}
	return cloned
}

fn clone_string_list_map(values map[string][]string) map[string][]string {
	mut cloned := map[string][]string{}
	for name, items in values {
		cloned[name.clone()] = clone_string_list(items)
	}
	return cloned
}

fn promote_scoped_monomorph_metadata(mut tc types.TypeChecker) {
	// Specialization records the source context for every generated function.
	// These maps can grow inside the disposable monomorph arena, so move both
	// their storage and string payloads before releasing that arena.
	tc.fn_type_files = clone_string_string_map(tc.fn_type_files)
	tc.fn_type_modules = clone_string_string_map(tc.fn_type_modules)
	tc.structs = clone_struct_field_map(tc.structs)
	tc.struct_modules = clone_string_string_map(tc.struct_modules)
	tc.unions = clone_string_bool_map(tc.unions)
	tc.params_structs = clone_string_bool_map(tc.params_structs)
	tc.sum_types = clone_string_list_map(tc.sum_types)
	tc.sum_generic_params = clone_string_list_map(tc.sum_generic_params)
}

fn promote_scoped_checker_node_caches(mut tc types.TypeChecker, scope voidptr, generated_start int) {
	for idx in 0 .. tc.resolved_call_names.len {
		if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
			name := tc.resolved_call_names[idx]
			if name.len > 0 && scoped_value_owned(scope, name.str) {
				tc.resolved_call_names[idx] = name.clone()
			}
		}
		if idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
			name := tc.resolved_fn_value_names[idx]
			if name.len > 0 && scoped_value_owned(scope, name.str) {
				tc.resolved_fn_value_names[idx] = name.clone()
			}
		}
		if idx >= generated_start && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
			tc.expr_type_values[idx] = types.clone_owned_type(tc.expr_type_values[idx])
		}
	}
	// The dense caches are reserved in the parent arena, but an unexpectedly
	// large specialization pass may still grow one of them in the stage arena.
	// Move only those backing arrays before releasing that arena.
	if scoped_value_owned(scope, tc.resolved_call_names.data) {
		tc.resolved_call_names = tc.resolved_call_names.clone()
	}
	if scoped_value_owned(scope, tc.resolved_call_set.data) {
		tc.resolved_call_set = tc.resolved_call_set.clone()
	}
	if scoped_value_owned(scope, tc.resolved_fn_value_names.data) {
		tc.resolved_fn_value_names = tc.resolved_fn_value_names.clone()
	}
	if scoped_value_owned(scope, tc.resolved_fn_value_set.data) {
		tc.resolved_fn_value_set = tc.resolved_fn_value_set.clone()
	}
	if scoped_value_owned(scope, tc.statement_nodes.data) {
		tc.statement_nodes = tc.statement_nodes.clone()
	}
	if scoped_value_owned(scope, tc.expr_type_values.data) {
		tc.expr_type_values = tc.expr_type_values.clone()
	}
	if scoped_value_owned(scope, tc.expr_type_set.data) {
		tc.expr_type_set = tc.expr_type_set.clone()
	}
	if scoped_value_owned(scope, tc.checking_nodes.data) {
		tc.checking_nodes = tc.checking_nodes.clone()
	}
	tc.sparse_resolved_call_names = clone_int_string_map(tc.sparse_resolved_call_names)
	tc.sparse_resolved_fn_values = clone_int_string_map(tc.sparse_resolved_fn_values)
	tc.sparse_statement_nodes = tc.sparse_statement_nodes.clone()
	tc.sparse_expr_type_values = clone_int_type_map(tc.sparse_expr_type_values)
	tc.sparse_checking_nodes = tc.sparse_checking_nodes.clone()
}

fn promote_scoped_signatures(mut tc types.TypeChecker, original_names []string) {
	mut added_names := []string{}
	mut current_names := tc.fn_ret_types.keys()
	current_names.sort()
	mut original_idx := 0
	for name in current_names {
		for original_idx < original_names.len && original_names[original_idx] < name {
			original_idx++
		}
		if original_idx >= original_names.len || original_names[original_idx] != name {
			added_names << name
		}
	}
	for name in added_names {
		ret := types.clone_owned_type(tc.fn_ret_types[name] or { continue })
		params := if values := tc.fn_param_types[name] {
			types.clone_owned_types(values)
		} else {
			[]types.Type{}
		}
		variadic := tc.fn_variadic[name]
		specialized := tc.specialized_generic_fns[name]
		tc.fn_ret_types.delete(name)
		tc.fn_param_types.delete(name)
		tc.fn_variadic.delete(name)
		tc.specialized_generic_fns.delete(name)
		owned_name := name.clone()
		tc.fn_ret_types[owned_name] = ret
		tc.fn_param_types[owned_name] = params
		tc.fn_variadic[owned_name] = variadic
		if specialized {
			tc.specialized_generic_fns[owned_name] = true
		}
	}
	if added_names.len > scoped_transform_signature_headroom {
		tc.rebuild_scoped_transform_signature_maps()
	}
}

// default_cc_identity returns the resolved path and version banner of the
// default `cc`. Module objects in the persistent cache are compiled with literal
// `cc` (only the default compiler is cacheable), so this must be part of the
// cache salt: a `cc` upgrade or a retargeted `cc` symlink otherwise leaves stale
// objects certified as current.
fn default_cc_identity() string {
	cc_path := os.find_abs_path_of_executable('cc') or { 'cc' }
	cc_version := cmdexec.run('cc', ['--version']).output
	return '${cc_path}\t${cc_version.replace('\n', ' ')}'
}

fn v3_cache_compiler_signature(vroot string) string {
	dir := os.join_path(vroot, 'vlib', 'v3')
	if !os.is_dir(dir) {
		return ''
	}
	mut files := []string{}
	for file in os.walk_ext(dir, '.v') {
		normalized := file.replace('\\', '/')
		if normalized.contains('/tests/') {
			continue
		}
		files << file
	}
	files << os.walk_ext(dir, '.h')
	return modulecache.source_signature(files)
}

fn restored_fn_c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	if name == 'exit' {
		return 'v_exit'
	}
	return naming.sanitize(name)
}

fn transformed_fn_is_used(name string, module_name string, used_fns map[string]bool) bool {
	if used_fns.len == 0 || !used_fns['main'] || name.starts_with('__anon_fn_') {
		return true
	}
	if used_fns[name] || used_fns[restored_fn_c_name(name)] {
		return true
	}
	if module_name.len == 0 || module_name in ['main', 'builtin'] {
		return module_name == 'builtin' && name == 'free' && used_fns['v_free']
	}
	qname := '${module_name}.${name}'
	return used_fns[qname] || used_fns[restored_fn_c_name(qname)]
}

fn transformed_used_fns_need_monomorphize(used_fns map[string]bool) bool {
	for name, used in used_fns {
		if !used {
			continue
		}
		if name.starts_with('orm.new_query_T_') || name.starts_with('orm__new_query_T_') {
			return true
		}
	}
	return false
}

fn incremental_changed_functions_call_generics(a &flat.FlatAst, tc &types.TypeChecker, changed_names map[string]bool) bool {
	if changed_names.len == 0 {
		return false
	}
	mut cur_module := ''
	for idx, node in a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				name := incremental_qualified_fn_name(cur_module, node.value)
				if changed_names[name]
					&& incremental_node_tree_calls_generic(a, tc, flat.NodeId(idx)) {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn incremental_node_tree_calls_generic(a &flat.FlatAst, tc &types.TypeChecker, root flat.NodeId) bool {
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= a.nodes.len {
			continue
		}
		node := a.nodes[idx]
		if node.kind == .call {
			if name := tc.resolved_call_name(id) {
				if name in tc.fn_generic_params || name.contains('[') {
					return true
				}
			}
		}
		for child_idx in 0 .. node.children_count {
			stack << a.child(&node, child_idx)
		}
	}
	return false
}

fn ast_contains_sql_expr(a &flat.FlatAst) bool {
	for node in a.nodes {
		if node.kind == .sql_expr {
			return true
		}
	}
	return false
}

fn restore_transformed_fn_value_types(mut tc types.TypeChecker, a &flat.FlatAst, used_fns map[string]bool) {
	for tc.expr_type_values.len < a.nodes.len {
		tc.expr_type_values << types.Type(types.void_)
		tc.expr_type_set << false
	}
	limit := if tc.resolved_fn_value_names.len < a.nodes.len {
		tc.resolved_fn_value_names.len
	} else {
		a.nodes.len
	}
	for idx in 0 .. limit {
		if idx >= tc.resolved_fn_value_set.len || !tc.resolved_fn_value_set[idx] {
			continue
		}
		name := tc.resolved_fn_value_names[idx]
		params := tc.fn_param_types[name] or { continue }
		ret := tc.fn_ret_types[name] or { continue }
		tc.expr_type_values[idx] = types.FnType{
			params:      params
			return_type: ret
		}
		tc.expr_type_set[idx] = true
	}
	mut cur_module := ''
	mut stack := []flat.NodeId{cap: 256}
	for top_idx in tc.top_level_idx {
		top := a.nodes[top_idx]
		if top.kind == .file {
			cur_module = ''
			continue
		}
		if top.kind == .module_decl {
			cur_module = top.value
			continue
		}
		if top.kind != .fn_decl {
			continue
		}
		if !transformed_fn_is_used(top.value, cur_module, used_fns) {
			continue
		}
		stack.clear()
		stack << flat.NodeId(top_idx)
		for stack.len > 0 {
			id := stack.pop()
			idx := int(id)
			if idx < 0 || idx >= a.nodes.len {
				continue
			}
			node := a.nodes[idx]
			if node.kind == .call && node.children_count > 0 {
				callee_id := a.children[node.children_start]
				callee_idx := int(callee_id)
				if callee_idx >= 0 && callee_idx < a.nodes.len {
					callee := a.nodes[callee_idx]
					if callee.kind == .ident && callee.value.len > 0 {
						mut name := tc.resolved_call_name(id) or { callee.value }
						if name !in tc.fn_param_types || name !in tc.fn_ret_types {
							qname := if cur_module.len == 0 || cur_module in ['main', 'builtin'] {
								callee.value
							} else {
								'${cur_module}.${callee.value}'
							}
							if qname in tc.fn_param_types && qname in tc.fn_ret_types {
								name = qname
							} else {
								cname := 'C.${callee.value}'
								if cname in tc.fn_param_types && cname in tc.fn_ret_types {
									name = cname
								} else {
									name = ''
								}
							}
						}
						if name.len > 0 {
							params := tc.fn_param_types[name] or { []types.Type{} }
							if ret := tc.fn_ret_types[name] {
								tc.expr_type_values[callee_idx] = types.FnType{
									params:      params
									return_type: ret
								}
								tc.expr_type_set[callee_idx] = true
							}
						}
					}
				}
			}
			if node.kind == .selector && node.children_count > 0 {
				base_id := a.children[node.children_start]
				base_idx := int(base_id)
				if base_idx >= 0 && base_idx < a.nodes.len {
					base := a.nodes[base_idx]
					cname := 'C.${base.value}'
					if base.kind == .ident && cname in tc.fn_param_types && cname in tc.fn_ret_types {
						params := tc.fn_param_types[cname] or { []types.Type{} }
						if ret := tc.fn_ret_types[cname] {
							tc.expr_type_values[base_idx] = types.FnType{
								params:      params
								return_type: ret
							}
							tc.expr_type_set[base_idx] = true
						}
					}
				}
			}
			for i := node.children_count - 1; i >= 0; i-- {
				child_id := a.children[node.children_start + i]
				if int(child_id) >= 0 {
					stack << child_id
				}
			}
		}
	}
}

// main runs the v3 entry point.
fn main() {
	args := os.args[1..]
	if args.len == 0 {
		eprintln(cli_usage())
		exit(1)
	}

	mut input_file := ''
	mut output_file := ''
	mut explicit_output := false
	mut backend := 'c'
	mut target_os := os.user_os()
	mut target_os_explicit := false
	mut target_arch := pref.host_arch()
	mut target_arch_explicit := false
	mut c_compiler := 'cc'
	mut c_compiler_explicit := false
	mut gc_mode := 'none'
	mut enable_globals_compat := false
	mut is_prod := false
	mut is_shared := false
	mut is_strict := false
	mut is_selfhost := false
	mut no_parallel := false
	mut no_prealloc := false
	mut no_cache := false
	mut no_memory_limit := false
	mut parallel_transform := true
	mut building_v := false
	mut ownership_mode := false
	mut verbose := false
	mut is_debug := false
	mut c99 := false
	mut all_backends := false
	mut compile_backends := []string{}
	mut user_defines := []string{}
	mut user_c_flags := []string{}
	mut should_run := false
	mut is_test_command := false
	mut run_args := []string{}
	mut i := 0
	for i < args.len {
		// Once `run <file>` has captured its input file, every remaining argument
		// belongs to the program being run — including `-`-prefixed flags such as
		// `--help`. Forward them verbatim instead of interpreting them as compiler
		// flags (which would otherwise be silently dropped).
		if should_run && input_file.len > 0 {
			run_args << args[i]
			i++
			continue
		}
		if args[i] in ['-o', '-b', '-os', '-arch', '-compile-backend', '--compile-backend', '-d', '-gc', '-cc']
			&& (i + 1 >= args.len || args[i + 1].starts_with('-')) {
			eprintln('option `${args[i]}` requires a value')
			exit(1)
		}
		if args[i] == '-cflags' && i + 1 >= args.len {
			eprintln('option `-cflags` requires a value')
			exit(1)
		}
		if args[i] == 'run' && input_file.len == 0 && !should_run {
			should_run = true
			i++
		} else if args[i] == 'test' && input_file.len == 0 && !should_run {
			is_test_command = true
			i++
		} else if args[i] == '-o' && i + 1 < args.len {
			output_file = args[i + 1]
			explicit_output = true
			i += 2
		} else if args[i] == '-b' && i + 1 < args.len {
			backend = args[i + 1]
			i += 2
		} else if args[i] == '-os' && i + 1 < args.len {
			target_os = args[i + 1]
			target_os_explicit = true
			i += 2
		} else if args[i] == '-arch' && i + 1 < args.len {
			target_arch = args[i + 1]
			target_arch_explicit = true
			i += 2
		} else if args[i] == '-prod' {
			is_prod = true
			i++
		} else if args[i] == '-shared' || args[i] == '--shared' {
			is_shared = true
			i++
		} else if args[i] == '-selfhost' {
			is_selfhost = true
			i++
		} else if args[i] == '-building-v' || args[i] == '-building_v' {
			// The V compiler itself uses no generics, so monomorphization (and the rest
			// of the generics machinery) is pure overhead when building it.
			building_v = true
			i++
		} else if args[i] == '-c99' || args[i] == '--c99' {
			c99 = true
			if 'c99' !in user_defines {
				user_defines << 'c99'
			}
			i++
		} else if args[i] == '-strict' {
			is_strict = true
			i++
		} else if args[i] == '-ownership' || args[i] == '--ownership' {
			// The ownership checker itself is compiled into v3 via `-d ownership`.
			// The runtime `-ownership` flag should only load the builtin ownership
			// overlays; it must not expose `ownership` to target `$if` blocks or target
			// `_d_ownership.v` files.
			ownership_mode = true
			i++
		} else if args[i] == '-no-parallel' || args[i] == '--no-parallel' {
			no_parallel = true
			i++
		} else if args[i] == '-parallel-transform' || args[i] == '--parallel-transform' {
			parallel_transform = true
			i++
		} else if args[i] == '-all-backends' || args[i] == '--all-backends' {
			all_backends = true
			i++
		} else if args[i] in ['-compile-backend', '--compile-backend'] && i + 1 < args.len {
			compile_backends << args[i + 1]
			i += 2
		} else if args[i] == '-d' && i + 1 < args.len {
			user_defines << args[i + 1]
			i += 2
		} else if args[i].starts_with('-d') && args[i].len > 2 {
			user_defines << args[i][2..]
			i++
		} else if args[i] == '-gc' && i + 1 < args.len {
			gc_mode = args[i + 1]
			i += 2
		} else if args[i] == '-cc' && i + 1 < args.len {
			c_compiler = args[i + 1]
			c_compiler_explicit = true
			i += 2
		} else if args[i] == '-cflags' && i + 1 < args.len {
			parsed_c_flags := cmdexec.split_args(args[i + 1]) or {
				eprintln('invalid `-cflags` value: ${err.msg()}')
				exit(1)
			}
			user_c_flags << parsed_c_flags
			i += 2
		} else if args[i] == '-g' {
			is_debug = true
			user_c_flags << '-g'
			i++
		} else if args[i] == '-v' {
			verbose = true
			i++
		} else if args[i] in ['-stats', '-show-timings', '-showcc', '-keepc', '-w'] {
			// v3 already reports phase metrics, prints the C command, retains generated C,
			// and suppresses C warnings. Accept the corresponding V flags for compatibility.
			i++
		} else if args[i] == '-no-prealloc' || args[i] == '--no-prealloc' {
			no_prealloc = true
			i++
		} else if args[i] == '-nocache' || args[i] == '--no-cache' {
			no_cache = true
			i++
		} else if args[i] == '-no-memory-limit' || args[i] == '--no-memory-limit' {
			no_memory_limit = true
			i++
		} else if args[i] == '-prealloc' {
			// Same effect as `v -prealloc`: activate the `$if prealloc {` arena
			// allocator branches in vlib/builtin (allocation.c.v, prealloc.c.v).
			if 'prealloc' !in user_defines {
				user_defines << 'prealloc'
			}
			i++
		} else if args[i] == '-enable-globals' {
			enable_globals_compat = true
			i++
		} else if args[i] in ['-h', '--help'] {
			println(cli_usage())
			return
		} else if args[i].starts_with('-') {
			eprintln('unknown option `${args[i]}`')
			exit(1)
		} else {
			if input_file.len > 0 {
				eprintln('multiple input paths are not supported: `${input_file}` and `${args[i]}`')
				exit(1)
			}
			input_file = args[i]
			i++
		}
	}
	mut current_no_parallel := no_parallel
	mut current_parallel_transform := parallel_transform
	if current_no_parallel {
		current_parallel_transform = false
	}

	if input_file == '' {
		eprintln('no input file')
		exit(1)
	}
	if gc_mode != 'none' {
		eprintln('unsupported garbage collector `${gc_mode}`; v3 currently supports only `-gc none`')
		exit(1)
	}
	for define in user_defines {
		define_name := define.all_before('=').trim_space()
		if define_name == 'vgc' || define_name.starts_with('gcboehm') {
			eprintln('unsupported garbage collector define `${define_name}`; v3 programs must not use a garbage collector')
			exit(1)
		}
		if define_name == 'ownership' && !ownership_checker_compiled() {
			eprintln('ownership support is not compiled into this v3 executable')
			exit(1)
		}
	}
	if ownership_mode && !ownership_checker_compiled() {
		eprintln('ownership support is not compiled into this v3 executable')
		exit(1)
	}
	if enable_globals_compat {
		eprintln('warning: `-enable-globals` is unnecessary; globals are always enabled in v3')
	}
	if backend !in ['c', 'arm64', 'wasm', 'eval'] {
		eprintln('unknown backend `${backend}`; expected c, arm64, wasm, or eval')
		exit(1)
	}
	for requested in compile_backends {
		for name in requested.split(',') {
			if name.trim_space() !in ['c', 'arm64', 'aarch64', 'wasm', 'wasm32', 'eval'] {
				eprintln('unknown compile backend `${name.trim_space()}`')
				exit(1)
			}
		}
	}
	if backend == 'wasm' && !target_os_explicit {
		target_os = 'wasm32_emscripten'
	}
	if !target_arch_explicit
		&& pref.normalized_os(target_os.trim_space().to_lower()) == 'wasm32_emscripten' {
		target_arch = 'wasm32'
	}
	target := pref.target_from(target_os, target_arch) or {
		eprintln(err.msg())
		exit(1)
	}

	// Compiling v3 itself implies building_v: it uses no generics, so the monomorphization
	// pass is pure overhead. -building-v can force this for any input.
	if input_implies_building_v(input_file) {
		building_v = true
	}
	cmd_v_build := input_is_cmd_v(input_file)
	scope_prealloc_stages := should_scope_prealloc_stages()
	scope_prealloc_cgen := should_scope_prealloc_cgen()
	// The selective transform promotion path is designed around worker-owned
	// results outside the disposable stage arena.
	scope_prealloc_transform := scope_prealloc_stages
	// Markused can lazily create the compilation worker pool. When parsing was
	// serial, keep that pool in the compilation arena so close_workers never
	// observes a pool allocated in a released markused scope.
	scope_prealloc_markused := scope_prealloc_stages && !current_no_parallel
	if building_v || cmd_v_build {
		if no_parallel {
			user_defines = user_defines.filter(it != 'parallel')
			if 'v3_no_parallel' !in user_defines {
				user_defines << 'v3_no_parallel'
			}
		} else if 'parallel' !in user_defines {
			user_defines << 'parallel'
		}
		// The compiler is a single-shot batch program — exactly what the
		// -prealloc bump arena is for (~18% less CPU across its
		// allocation-heavy phases) — so compiler builds default to it.
		// -no-prealloc opts out (also restores tcc linking: tcc has no
		// thread-local storage support, so prealloc builds link with cc).
		if !no_prealloc && 'prealloc' !in user_defines {
			user_defines << 'prealloc'
		}
	}
	if no_prealloc {
		user_defines = user_defines.filter(it != 'prealloc')
	}

	mut bin_file := ''
	mut c_only := false
	if output_file == '' {
		bin_file = default_bin_file_for_input(input_file)
		if is_shared {
			bin_file = with_shared_library_postfix(bin_file, target.os)
		}
		// The wasm backend writes the binary itself; default to <name>.wasm.
		output_file = if backend == 'wasm' { bin_file + '.wasm' } else { bin_file + '.c' }
	} else if backend == 'wasm' {
		// Honor the exact -o path; the wasm backend writes output_file directly.
		bin_file = output_file.all_before_last('.wasm')
	} else if backend == 'c' && output_file.ends_with('.c') {
		c_only = true
		bin_file = output_file.all_before_last('.c')
	} else {
		bin_file = output_file
		if is_shared {
			bin_file = with_shared_library_postfix(bin_file, target.os)
		}
		output_file = bin_file + '.c'
	}

	// Decide which backend modules to compile into the output. By default only the C
	// backend is built; the arm64/wasm/eval backends (and the whole SSA pipeline that the
	// arm64 backend pulls in: v3.ssa + v3.ssa.optimize) are skipped entirely. When compiling
	// the V compiler itself this avoids parsing/checking/transforming/cgen-ing ~30k lines of
	// unused backend code, which measurably speeds up the self-host build. The `skip_*`
	// defines drive two things in lock-step: `$if !skip_* ?` gates in main() make the parser
	// drop the dispatch blocks (so the backend symbols are never referenced), and
	// resolve_imports skips parsing the corresponding module directories.
	// `-all-backends` keeps everything; `-compile-backend <name>` opts a specific backend back
	// in; the active `-b` target backend is always force-included.
	mut include_arm64 := all_backends
	mut include_wasm := all_backends
	mut include_eval := all_backends
	for cb in compile_backends {
		for name in cb.split(',') {
			match name.trim_space() {
				'arm64', 'aarch64' { include_arm64 = true }
				'wasm', 'wasm32' { include_wasm = true }
				'eval' { include_eval = true }
				// 'c' is always built; there is no native amd64 backend in v3 yet.
				else {}
			}
		}
	}
	match backend {
		'arm64' { include_arm64 = true }
		'wasm' { include_wasm = true }
		'eval' { include_eval = true }
		else {}
	}

	if !include_arm64 {
		user_defines << 'skip_arm64'
	}
	if !include_wasm {
		user_defines << 'skip_wasm'
	}
	if !include_eval {
		user_defines << 'skip_eval'
	}

	mut b := bench.new()
	if no_memory_limit {
		b.disable_memory_limit()
	}
	b.start_memory_monitor()
	mut c_object_cache_stats := CObjectCacheStats{}
	println('=== v3 benchmark ===')

	// Parse directly to flat AST
	mut prefs := pref.new_preferences()
	prefs.target = target
	prefs.backend = backend
	prefs.c99 = c99
	prefs.user_defines = user_defines
	prefs.vroot = resolve_vroot_for_input(prefs.vroot, input_file)
	prefs.selfhost = is_selfhost
	prefs.building_v = building_v
	prefs.is_prod = is_prod
	prefs.is_debug = is_debug
	prefs.verbose = verbose
	host_target := pref.host_target()
	cache_enabled := backend == 'c' && !c_only && !no_cache && !c_compiler_explicit
		&& target.os == host_target.os && target.arch == host_target.arch
	cc_identity := if cache_enabled { default_cc_identity() } else { '' }
	cache_salt := [
		'compiler=${v3_cache_compiler_signature(prefs.vroot)}',
		'cc=${cc_identity}',
		'vexe=${prefs.vexe}',
		'backend=${backend}',
		'target=${prefs.normalized_target_os()}',
		'target_arch=${prefs.normalized_target_arch()}',
		'prod=${is_prod}',
		'debug=${is_debug}',
		'shared=${is_shared}',
		'selfhost=${is_selfhost}',
		'c99=${c99}',
		'ownership=${ownership_mode}',
		'test=${is_test_command || pref.is_test_file_for_backend(input_file, backend)}',
		'defines=${prefs.user_defines.join(',')}',
	].join('\n')
	build_pseudo_values := [prefs.build_date, prefs.build_time, prefs.build_timestamp].join('\n')
	cache_manager := modulecache.new_manager(prefs.vroot, cache_salt, cache_enabled,
		build_pseudo_values)
	force_cache_source := os.getenv('V3_CACHE_FORCE_SOURCE') == '1'
	// Cache markers and scoped output are stable across ordered worker chunks, so cached
	// and preallocated builds use the same parallel function-body generator.
	cache_no_parallel_cgen := current_no_parallel
	mut p := parser.Parser.new(prefs)
	if building_v || cmd_v_build {
		p.reserve_selfhost_ast()
	}

	builtin_dir := builtin_dir_for_vroot(prefs.vroot)
	mut builtin_defines := prefs.user_defines.clone()
	// Builtin contains a small number of ABI-sensitive helpers. Keep their v3
	// implementations separate from the regular backend without exposing this
	// internal selection define to user modules.
	if 'v3_backend' !in builtin_defines {
		builtin_defines << 'v3_backend'
	}
	if ownership_mode && 'ownership' !in builtin_defines {
		builtin_defines << 'ownership'
	}
	builtin_files := pref.get_v_files_from_dir_for_target(builtin_dir, builtin_defines,
		prefs.target)
	bundle_sources := builtin_bundle_source_files(prefs, builtin_files)
	mut cache_state := V3ModuleCacheState{
		manager:                cache_manager
		bundle_sources:         bundle_sources
		bundle_source_paths:    module_cache_source_path_set(bundle_sources)
		force_source:           force_cache_source
		module_sources:         map[string][]string{}
		module_import_paths:    map[string]string{}
		module_dependencies:    map[string][]string{}
		module_external_inputs: map[string][]string{}
		parsed_from_source:     map[string]bool{}
		source_body_modules:    map[string]bool{}
		native_source_modules:  map[string]bool{}
		objects:                map[string]string{}
		headers:                map[string]string{}
	}
	cache_state.module_sources['builtin'] = builtin_files
	mut files := []string{}
	mut loaded_cached_bundle := false
	if !force_cache_source {
		if bundle_object := cache_manager.valid_object('builtin', bundle_sources) {
			if builtin_header := cache_manager.valid_header('builtin', builtin_files) {
				cache_state.bundle_valid = true
				cache_state.objects['builtin'] = bundle_object.object
				if modulecache.header_needs_source(builtin_header) {
					cache_state.source_body_modules['builtin'] = true
					files << builtin_files
				} else {
					files << builtin_header.header
				}
				loaded_cached_bundle = true
			}
		}
	}
	if !loaded_cached_bundle {
		cache_state.parsed_from_source['builtin'] = true
		cache_state.source_body_modules['builtin'] = true
		files << builtin_files
	}
	mut parse_was_parallel := false
	_, builtin_parse_parallel := p.parse_files_dispatch(files, !current_no_parallel)
	parse_was_parallel = parse_was_parallel || builtin_parse_parallel
	mut a := p.a
	defer {
		a.close_workers()
	}
	a.user_code_start = a.nodes.len

	// Test mode is a compile-time define as well as a harness mode. Install it
	// after parsing builtin, but before collecting and parsing user inputs, so
	// `$if test` and `_d_test.v` apply to both file and directory test commands.
	if 'test' !in prefs.user_defines
		&& (is_test_command || pref.is_test_file_for_backend(input_file, backend)) {
		prefs.user_defines << 'test'
	}

	// Parse user input: single file or directory
	mut user_files := []string{}
	if input_file.ends_with('.v') {
		user_files << input_file
		user_files = expand_single_test_file_inputs(user_files, prefs)
	} else if os.is_dir(input_file) {
		user_files = pref.get_v_files_from_dir_for_target(input_file, prefs.user_defines,
			prefs.target)
		if is_test_command {
			user_files << pref.get_test_v_files_from_dir_for_target(input_file, prefs.user_defines,
				prefs.backend, prefs.target)
		}
		subdirs := vmod_subdirs(input_file) or {
			eprintln(err.msg())
			exit(1)
		}
		for subdir in subdirs {
			subdir_path := os.join_path_single(input_file, subdir)
			user_files << pref.get_v_files_from_dir_for_target(subdir_path, prefs.user_defines,
				prefs.target)
			if is_test_command {
				user_files << pref.get_test_v_files_from_dir_for_target(subdir_path,
					prefs.user_defines, prefs.backend, prefs.target)
			}
		}
	} else {
		user_files << input_file
	}
	prefs.is_test = user_files.any(pref.is_test_file_for_platform(it, backend, prefs.target))
	_, user_parse_parallel := p.parse_files_dispatch(user_files, !current_no_parallel)
	parse_was_parallel = parse_was_parallel || user_parse_parallel
	test_files := test_input_files(user_files, backend, prefs.target)

	seed_implicit_imports(mut a)
	seed_cached_builtin_bundle_imports(mut a, cache_state.manager.enabled, builtin_dir)

	// Resolve imports recursively
	import_parse_parallel := resolve_imports(mut a, mut p, prefs, user_files, !current_no_parallel, mut
		cache_state)
	parse_was_parallel = parse_was_parallel || import_parse_parallel
	if p.diagnostics.len > 0 {
		for diagnostic in p.diagnostics {
			eprintln('${diagnostic.file}:${diagnostic.line}:${diagnostic.column}: error: ${diagnostic.message}')
		}
		exit(1)
	}
	// Parsing workers canonicalize source-backed node text before their buffers
	// are released. Metadata keys are finalized here before semantic phases begin.
	p.release_source_storage()
	diagnostic_root := if is_selfhost {
		diagnostic_root_for_input(input_file, user_files)
	} else {
		''
	}

	b.step_parallel('parse', parse_was_parallel)
	b.metric_items('parsed .vh files', p.parsed_v_header_files, 'files', '.vh files',
		p.parsed_v_header_file_paths)
	println('    ${'parsed .vh lines':-28s} ${source_file_line_count(p.parsed_v_header_file_paths)} lines')
	b.metric_items('parsed .v files', p.parsed_v_files, 'files', '.v files', p.parsed_v_file_paths)
	println('    ${'parsed .v lines':-28s} ${source_file_line_count(p.parsed_v_file_paths)} lines')
	b.metric('AST nodes after parse', a.nodes.len, 'nodes')
	b.metric('AST children after parse', a.children.len, 'edges')
	b.metric('canonical AST texts', a.text_count(), 'texts')
	b.metric('persistent worker threads', a.worker_count(), 'threads')

	// An exact whole-program C plan hit already certifies the current user sources,
	// cached module interfaces, compiler configuration, target, and native inputs.
	// Validate it immediately after import resolution so an unchanged development
	// build does not repeat semantic and lowering work whose only consumer is that
	// cached C plan.
	mut cgen_cache_entry := modulecache.CgenEntry{}
	mut cgen_cache_metadata := V3CgenCacheMetadata{}
	mut cgen_cache_hit := false
	mut cgen_prepared_entry := modulecache.CgenPreparedEntry{}
	mut cgen_prepared_hit := false
	mut generic_cache_input := V3CgenCacheInput{}
	mut generic_cache_entry := modulecache.GenericProgramEntry{}
	mut generic_cache_hit := false
	mut generic_cache_signature := ''
	mut generic_cache_runtime_strings := []string{}
	mut cached_monomorph_specs := []transform.MonomorphCacheSpec{}
	mut cached_program_used_fns := map[string]bool{}
	mut generated_monomorph_specs := []transform.MonomorphCacheSpec{}
	incremental_snapshot := incremental_program_snapshot(a)
	mut incremental_cache_hit := false
	mut incremental_changed_keys := []string{}
	mut incremental_changed_names := map[string]bool{}
	mut incremental_cached_body := ''
	mut incremental_tcc_declarations_path := ''
	if backend == 'c' && cache_state.manager.enabled && !cache_state.force_source
		&& cache_state.parsed_from_source.len == 0 {
		if !prepare_v3_cache_external_inputs(mut cache_state, a, prefs, user_c_flags) {
			restart_v3_without_cache()
		}
		input := v3_cgen_cache_input(cache_state, user_files, user_c_flags)
		if entry := cache_state.manager.valid_cgen(input.source_files, input.generation_signature,
			input.dependency_inputs)
		{
			metadata := os.read_file(entry.metadata) or { '' }
			if decoded := decode_v3_cgen_metadata(metadata) {
				cgen_cache_entry = entry
				cgen_cache_metadata = decoded
				cgen_cache_hit = true
				if prepared := cache_state.manager.valid_cgen_prepared(entry) {
					cgen_prepared_entry = prepared
					cgen_prepared_hit = true
				}
			}
		}
		if !cgen_cache_hit && !is_prod && !is_shared && !is_selfhost
			&& prefs.normalized_target_os() == 'macos' {
			generic_cache_signature = monomorph_cache_semantic_signature(a)
			generic_cache_runtime_strings = monomorph_cache_runtime_strings(a)
			generic_cache_input = input
			if entry := cache_state.manager.valid_generic_program(input.source_files,
				generic_cache_signature, input.generation_signature, input.dependency_inputs)
			{
				spec_text := os.read_file(entry.specs) or { '' }
				cached_monomorph_specs = decode_monomorph_cache_specs(spec_text)
				used_text := os.read_file(entry.used) or { '' }
				cached_program_used_fns = decode_cached_used_fns(used_text)
				if cached_monomorph_specs.len > 0 && cached_program_used_fns.len > 0 {
					generic_cache_entry = entry
					generic_cache_hit = true
					old_literal_text := os.read_file(entry.literals) or { '' }
					cached_body := os.read_file(entry.body) or { '' }
					metadata := os.read_file(entry.metadata) or { '' }
					if old_literals := decode_cached_runtime_strings(old_literal_text) {
						if rewritten := modulecache.rewrite_cached_runtime_strings(cached_body,
							old_literals, generic_cache_runtime_strings)
						{
							if decoded := decode_v3_cgen_metadata(metadata) {
								cgen_cache_entry = cache_state.manager.write_cgen(input.source_files,
									input.generation_signature, input.dependency_inputs, rewritten,
									metadata) or { modulecache.CgenEntry{} }
								if cgen_cache_entry.stamp.len > 0 {
									cgen_cache_metadata = decoded
									cgen_cache_hit = true
								}
							}
						}
					}
				}
			}
		}
		if !cgen_cache_hit && os.getenv('V3_CACHE_DISABLE_INCREMENTAL') != '1' && !is_prod
			&& !is_shared && !is_selfhost && prefs.normalized_target_os() == 'macos' {
			if entry := cache_state.manager.valid_incremental_program(input.source_files,
				incremental_snapshot.declaration_signature, input.generation_signature,
				input.dependency_inputs)
			{
				manifest_text := os.read_file(entry.manifest) or { '' }
				if old_manifest := decode_incremental_manifest(manifest_text) {
					if changed_keys, changed_names := incremental_changed_functions(incremental_snapshot,
						old_manifest)
					{
						spec_text := os.read_file(entry.specs) or { '' }
						used_text := os.read_file(entry.used) or { '' }
						body_text := os.read_file(entry.body) or { '' }
						metadata := os.read_file(entry.metadata) or { '' }
						decoded_specs := decode_monomorph_cache_specs(spec_text)
						decoded_used := decode_cached_used_fns(used_text)
						if decoded_metadata := decode_v3_cgen_metadata(metadata) {
							if decoded_used.len > 0 && body_text.len > 0 {
								incremental_cache_hit = changed_keys.len > 0
								incremental_changed_keys = changed_keys.clone()
								incremental_changed_names = changed_names.clone()
								incremental_cached_body = body_text
								incremental_tcc_declarations_path = entry.tcc_declarations
								cached_monomorph_specs = clone_monomorph_cache_specs(decoded_specs)
								cached_program_used_fns = clone_string_bool_map(decoded_used)
								cgen_cache_metadata = decoded_metadata
								generic_cache_entry = modulecache.GenericProgramEntry{
									specs:        entry.specs
									used:         entry.used
									prefix:       entry.prefix
									declarations: entry.declarations
									body:         entry.body
									metadata:     entry.metadata
								}
								generic_cache_hit = true
								if changed_keys.len == 0 {
									if decoded := decode_v3_cgen_metadata(metadata) {
										cgen_cache_entry = cache_state.manager.write_cgen(input.source_files,
											input.generation_signature, input.dependency_inputs,
											body_text, metadata) or { modulecache.CgenEntry{} }
										if cgen_cache_entry.stamp.len > 0 {
											cgen_cache_metadata = decoded
											cgen_cache_hit = true
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	// Type-collect + check BEFORE transform, so the transformer is type-aware
	// (like v2: check runs before transform). The transformer reads cached
	// per-expression types for type-dependent lowering.
	mut pre_tc := types.TypeChecker.new(a)
	mut used_fns := map[string]bool{}
	mut incremental_stage_used_fns := map[string]bool{}
	mut uses_generics := false
	mut skip_transform_generics := true
	mut transform_texts_canonical := cgen_cache_hit
	if !cgen_cache_hit {
		pre_tc.verbose = prefs.verbose
		if scope_prealloc_stages {
			pre_tc.enable_scoped_parallel_workers()
		}
		pre_tc.reject_unsupported_generics = is_selfhost
		set_diagnostic_files(mut pre_tc, user_files)
		pre_tc.collect(a)
		pre_tc.diagnose_unknown_calls = true
		pre_tc.prepare_threads_condition()
		set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
		if !incremental_cache_hit {
			pre_tc.prepare_interface_query_indexes()
		}
		mut check_was_parallel := false
		if incremental_cache_hit {
			pre_tc.check_semantics_selected(incremental_changed_names)
		} else {
			check_was_parallel = pre_tc.check_semantics_opt(!current_no_parallel)
		}
		if pre_tc.errors.len > 0 {
			print_type_errors(pre_tc.errors)
			exit(1)
		}
		if incremental_cache_hit
			&& incremental_changed_functions_call_generics(a, pre_tc, incremental_changed_names) {
			os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
			restart_v3_after_cache_invalidation()
		}
		pre_tc.prune_inactive_top_level_comptime(mut a)
		test_harness_errors := validate_test_file_harness_inputs(a, pre_tc, test_files)
		if test_harness_errors.len > 0 {
			for msg in test_harness_errors {
				eprintln(msg)
			}
			exit(1)
		}
		if cache_state.manager.enabled {
			if !prepare_v3_cache_external_inputs(mut cache_state, a, prefs, user_c_flags) {
				restart_v3_without_cache()
			}
			for module_name, parsed in cache_state.parsed_from_source {
				if !parsed {
					continue
				}
				header := modulecache.module_header(a, pre_tc, module_name, prefs.vroot,
					cache_state.module_import_paths)
				if header.len > 0 {
					cache_state.headers[module_name] = header
				}
			}
			if invalidate_changed_cache_dependents(mut cache_state) {
				restart_v3_after_cache_invalidation()
			}
		}
		if incremental_cache_hit {
			b.step('check (incremental)')
		} else {
			b.step_parallel('check', check_was_parallel)
		}
		b.metric('functions collected', pre_tc.fn_ret_types.len, 'symbols')
		b.metric('structs collected', pre_tc.structs.len, 'types')
		b.metric('canonical semantic types', pre_tc.type_count(), 'types')
		b.metric('canonical resolved symbols', pre_tc.symbol_count(), 'symbols')
		type_cache_stats := pre_tc.type_cache_stats()
		b.metric('type parse cache hits', type_cache_stats.parse_hits, 'lookups')
		b.metric('type parse cache misses', type_cache_stats.parse_misses, 'lookups')
		b.metric('C type cache hits', type_cache_stats.c_hits, 'lookups')
		b.metric('C type cache misses', type_cache_stats.c_misses, 'lookups')

		if backend == 'eval' {
			$if !skip_eval ? {
				mut runner := eval.new(prefs)
				runner.run_files(a) or {
					eprintln('error: ${err.msg()}')
					exit(1)
				}
				b.step('eval')
				b.print_report()
				return
			}
		}
		_ = pre_tc.ierror_impl_names()

		// Mark used functions (dead-code elimination). This is done before transform
		// so the transformer can skip function bodies that the C backend will prune.
		mut markused_scope := unsafe { nil }
		mut markused_tc := &pre_tc
		if scope_prealloc_markused && !generic_cache_hit {
			markused_scope = prealloc_scope_begin_for_v3()
			markused_tc = pre_tc.fork_for_parallel_transform(a)
			markused_tc.enable_scoped_parallel_workers()
		}
		if generic_cache_hit && test_files.len == 0 {
			used_fns = clone_string_bool_map(cached_program_used_fns)
			uses_generics = true
			if incremental_cache_hit {
				current_used_fns, current_uses_generics := markused.mark_used_with_generic_usage(a,
					markused_tc)
				uses_generics = uses_generics || current_uses_generics
				mut gained_reachability := false
				for name, is_used in current_used_fns {
					if is_used && !cached_program_used_fns[name] {
						gained_reachability = true
						break
					}
				}
				if gained_reachability {
					os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
					restart_v3_after_cache_invalidation()
				}
			}
		} else if test_files.len > 0 {
			used_fns, uses_generics = markused.mark_used_for_tests_with_generic_usage(a,
				markused_tc, test_files)
		} else {
			used_fns, uses_generics = markused.mark_used_with_generic_usage(a, markused_tc)
		}
		if cache_state.manager.enabled && !generic_cache_hit {
			mut cache_uses_generics := false
			used_fns, cache_uses_generics = markused.mark_used_for_cache_with_generic_usage(a,
				markused_tc, test_files, cache_state.source_body_modules)
			uses_generics = uses_generics || cache_uses_generics
		}
		if scope_prealloc_markused && !generic_cache_hit {
			prealloc_scope_leave_for_v3(markused_scope)
			used_fns = clone_string_bool_map(used_fns)
			prealloc_scope_free_for_v3(markused_scope)
		}
		b.step('markused')
		b.metric('reachable symbols', used_fns.len, 'symbols')

		// Transform (match lowering, string/in lowering, etc.). Threaded transform is enabled
		// by default for compatible builds, and `-no-parallel` disables both threaded transform
		// and cgen.
		mut transform_was_parallel := false
		mut transform_errors := []string{}
		if !building_v && !cmd_v_build && !uses_generics && ast_contains_sql_expr(a) {
			uses_generics = true
		}
		// Markused distinguishes reachable generic calls/types from generic templates
		// that merely came along with an imported module (notably sync and rand).
		skip_transform_generics = building_v || cmd_v_build || !uses_generics
		if incremental_cache_hit {
			skip_transform_generics = true
		}
		mut transform_used_fns := clone_string_bool_map(used_fns)
		if incremental_cache_hit {
			transform_used_fns = clone_string_bool_map(incremental_changed_names)
			// `main` activates the transformer's used-function filter. If another
			// function changed, transforming main as well is harmless; cgen still
			// emits only the explicitly changed function sections.
			transform_used_fns['main'] = true
		}
		if incremental_cache_hit {
			transform_used_fns, transform_errors = transform.transform_selected_functions(mut a,
				&pre_tc, incremental_changed_names)
			transform_texts_canonical = true
		} else if scope_prealloc_transform {
			// Keep the large escaping AST/cache slabs in the compilation arena, while
			// transformer indexes and per-body temporary state use a stage arena.
			transform.reserve_parallel_transform_ast(mut a, skip_transform_generics)
			pre_tc.begin_sparse_transform_node_caches(a.nodes.len)
			pre_tc.reserve_scoped_transform_metadata(scoped_transform_signature_headroom)
			base_transform_nodes := a.nodes.len
			reserved_nodes_cap := a.nodes.cap
			reserved_children_cap := a.children.cap
			base_specialized_fns := a.specialized_fn_nodes.len
			base_type_count := pre_tc.type_count()
			base_symbol_count := pre_tc.symbol_count()
			base_text_count := a.text_values.len
			mut original_signature_names := pre_tc.fn_ret_types.keys()
			original_signature_names.sort()
			transform_scope := prealloc_scope_begin_for_v3()
			mut scoped_owned_base_nodes := []int{}
			mut retained_transform_regions := []transform.ScopedTransformRegion{}
			transform_used_fns, transform_was_parallel, transform_errors, scoped_owned_base_nodes, retained_transform_regions = transform.transform_with_used_opt_config_scoped_workers_checked_owned(mut a,
				&pre_tc, transform_used_fns, current_parallel_transform, skip_transform_generics,
				true, transform_scope)
			parse_cache_enabled := pre_tc.type_cache_parse_enabled()
			prealloc_scope_leave_for_v3(transform_scope)
			retained_transform_regions = clone_scoped_transform_regions(retained_transform_regions)
			pre_tc.promote_scoped_transform_interners(base_type_count, base_symbol_count,
				transform_scope)
			if a.nodes.cap == reserved_nodes_cap && a.children.cap == reserved_children_cap
				&& !scoped_value_owned(transform_scope, a.nodes.data)
				&& !scoped_value_owned(transform_scope, a.children.data) {
				a.promote_transform_texts_from(base_text_count, transform_scope)
				if !skip_transform_generics {
					// Generic lowering can rewrite arbitrary pre-existing nodes. The backing
					// slabs were reserved outside the stage arena, so scan their small payload
					// fields and promote only values owned by that arena instead of cloning the
					// entire AST and permanently retaining both copies.
					for idx in 0 .. a.nodes.len {
						canonicalize_scoped_node(mut a, idx, transform_scope)
					}
				} else if retained_transform_regions.len > 0 {
					outer_new_end := retained_transform_regions[0].new_start
					promote_scoped_ast_nodes(mut a, base_transform_nodes, outer_new_end,
						scoped_owned_base_nodes, transform_scope)
					// Late lowering can rewrite nodes that live in a retained worker region
					// while allocating their replacement text in the outer transform arena.
					// Publish those strings before releasing that arena; the worker-owned
					// fields in the same regions are canonicalized below from their own scope.
					for region in retained_transform_regions {
						canonicalize_scoped_transform_region_from_scope(mut a, region,
							transform_scope)
					}
					last_worker_end := retained_transform_regions.last().new_end
					promote_scoped_ast_nodes(mut a, last_worker_end, a.nodes.len, []int{},
						transform_scope)
				} else {
					a.intern_node_texts_from(0)
					transform_texts_canonical = true
				}
			} else {
				clone_flat_ast_storage(mut a)
				pre_tc.rebind_ast(a)
			}
			if a.specialized_fn_nodes.len != base_specialized_fns {
				a.specialized_fn_nodes = a.specialized_fn_nodes.clone()
			}
			promote_scoped_checker_node_caches(mut pre_tc, transform_scope, base_transform_nodes)
			promote_scoped_signatures(mut pre_tc, original_signature_names)
			transform_used_fns = clone_string_bool_map(transform_used_fns)
			transform_errors = clone_string_list(transform_errors)
			pre_tc.set_fresh_type_cache(parse_cache_enabled)
			prealloc_scope_free_for_v3(transform_scope)
			if retained_transform_regions.len > 0 {
				if p.parsed_v_header_files > 0 {
					// Header-only warm builds are small enough that transform may not
					// force the pre-reserved flat arrays to grow. Publish their backing
					// once in the compilation arena before releasing retained helper
					// arenas; individual node text is promoted below.
					clone_flat_ast_storage(mut a)
					pre_tc.rebind_ast(a)
				}
				for region in retained_transform_regions {
					if skip_transform_generics {
						canonicalize_scoped_transform_region(mut a, region)
					} else {
						// Bounded generic batches can publish rewrites to any source node
						// through this result arena, so verify every flat payload before
						// releasing it.
						for idx in 0 .. a.nodes.len {
							canonicalize_scoped_node(mut a, idx, region.scope)
						}
					}
					if scoped_value_owned(region.scope, a.nodes.data)
						|| scoped_value_owned(region.scope, a.children.data) {
						a = clone_flat_ast_after_transform(a)
						pre_tc.rebind_ast(a)
					}
					prealloc_scope_free_for_v3(region.scope)
				}
				transform_texts_canonical = true
			}
		} else {
			transform_used_fns, transform_was_parallel, transform_errors = transform.transform_with_used_opt_config_scoped_workers_checked(mut a,
				&pre_tc, transform_used_fns, current_parallel_transform, skip_transform_generics,
				false)
		}
		if !incremental_cache_hit {
			used_fns = clone_string_bool_map(transform_used_fns)
		} else {
			incremental_stage_used_fns = clone_string_bool_map(transform_used_fns)
		}
		if !building_v && !cmd_v_build && !uses_generics
			&& transformed_used_fns_need_monomorphize(used_fns) {
			uses_generics = true
			skip_transform_generics = false
		}
		if incremental_cache_hit {
			b.step_parallel('transform (incremental)', transform_was_parallel)
		} else {
			b.step_parallel('transform', transform_was_parallel)
		}
		if transform_errors.len > 0 {
			eprintln('type checker found ${transform_errors.len} error(s):')
			for message in transform_errors {
				eprintln(message)
			}
			exit(1)
		}
		if !incremental_cache_hit {
			pre_tc.freeze_interface_impl_names()
		}
		b.metric('AST nodes after transform', a.nodes.len, 'nodes')
		b.metric('AST children after transform', a.children.len, 'edges')

		// Reuse the pre-transform checker for metadata only. Transform does not add
		// declarations, and v1/v2 do not run a second semantic checker after lowering.
		pre_tc.diagnose_unknown_calls = false
		pre_tc.reject_unlowered_map_mutation = true
		set_diagnostic_files(mut pre_tc, user_files)
		set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
		incremental_needs_monomorphize := incremental_cache_hit
			&& transformed_used_fns_need_monomorphize(incremental_stage_used_fns)
		if !building_v && !cmd_v_build {
			if uses_generics && (!incremental_cache_hit || incremental_needs_monomorphize) {
				if scope_prealloc_stages {
					// Volt's reachable specialization set settles just below 4x the
					// transformed node count. Reserving that bounded final size avoids
					// growing 3x caches to 6x inside the disposable monomorph arena and
					// then copying those oversized arrays back into the parent.
					pre_tc.materialize_sparse_transform_node_caches(a.nodes.len, a.nodes.len * 4)
				}
				// Generic lowering rewrites and clones call nodes in disposable arenas.
				// Resolve their final names from the owned transformed AST instead of
				// retaining pre-transform canonical string views across arena release.
				pre_tc.reset_resolved_calls_for_reannotation()
				pre_tc.annotate_types_with_used(if incremental_cache_hit {
					transform_used_fns
				} else {
					used_fns
				})
			} else {
				restore_transformed_fn_value_types(mut pre_tc, a, if incremental_cache_hit {
					incremental_stage_used_fns
				} else {
					used_fns
				})
			}
		}
		b.step('annotate types')
		if generic_cache_hit && (!incremental_cache_hit || incremental_needs_monomorphize) {
			pre_tc.reset_resolution_type_view_cache()
			transform.register_cached_monomorph_signatures(a, &pre_tc, used_fns,
				cached_monomorph_specs)
		}
	} else {
		b.step('check (cached)')
		b.step('markused (cached)')
		b.step('transform (cached)')
		b.step('annotate types (cached)')
	}
	if pre_tc.errors.len > 0 {
		print_type_errors(pre_tc.errors)
		exit(1)
	}

	if backend == 'wasm' {
		$if !skip_wasm ? {
			// Direct flat-AST-to-WASM native backend. Runs before monomorphize (which
			// targets generics, not yet supported here). output_file is the exact path
			// requested via -o (or the <name>.wasm default).
			mut g := wasm.Gen.new(a, pre_tc, used_fns)
			g.gen()
			g.write(output_file) or {
				eprintln('error writing ${output_file}')
				exit(1)
			}
			for w in g.warnings_list() {
				eprintln('wasm: ${w}')
			}
			b.step('wasm gen')
			b.print_report()
			return
		}
	}

	// Monomorphization only adds specialized generic instantiations to `used_fns`. Skip
	// it when markused found no reachable generic use; the small metadata cleanup keeps
	// unreachable generic templates out of C without walking or rewriting their ASTs.
	// Self-host builds retain their dedicated generic-function erasure pass.
	if cgen_cache_hit {
		// The cached C plan and metadata are the only consumers of the specialized
		// AST and checker state on this path.
	} else if building_v {
		used_fns = transform.erase_generic_templates(mut a, &pre_tc, used_fns)
	} else if uses_generics && (!incremental_cache_hit
		|| transformed_used_fns_need_monomorphize(incremental_stage_used_fns)) {
		mut monomorph_used_fns := map[string]bool{}
		mut monomorph_errors := []string{}
		monomorph_input_used := if incremental_cache_hit {
			incremental_stage_used_fns
		} else {
			used_fns
		}
		if scope_prealloc_stages && !incremental_cache_hit {
			// Monomorphization can add several times the transformed node count.
			// Reserve its persistent slabs in the parent arena so scoped specialization
			// batches append in place instead of retaining every growth allocation.
			if verbose {
				eprintln('mono AST before reserve: ${a.nodes.len}/${a.nodes.cap} nodes, ${a.children.len}/${a.children.cap} children')
			}
			base_monomorph_nodes := a.nodes.len
			// Volt's current generic closure retains about 5x the transformed node
			// and child counts. Reserve that measured final size directly so one
			// slightly-larger closure does not double both multi-million-item slabs.
			reserve_flat_ast_exact(mut a, a.nodes.len * 5 + 65536, a.children.len * 5 + 65536)
			monomorph_nodes_cap := a.nodes.cap
			monomorph_children_cap := a.children.cap
			base_specialized_fns := a.specialized_fn_nodes.len
			monomorph_scope := prealloc_scope_begin_for_v3()
			monomorph_used_fns, monomorph_errors, generated_monomorph_specs = transform.monomorphize_with_used_checked_config_scoped_cached(mut a,
				&pre_tc, monomorph_input_used, should_parallel_monomorphize()
				&& cache_state.parsed_from_source.len == 0, monomorph_scope, cached_monomorph_specs)
			parse_cache_enabled := pre_tc.type_cache_parse_enabled()
			prealloc_scope_leave_for_v3(monomorph_scope)
			// Specialization can rewrite payload text on pre-existing nodes as
			// well as append new nodes. Publish every string still owned by the
			// disposable monomorph arena before it is released.
			for idx in 0 .. a.nodes.len {
				canonicalize_scoped_node(mut a, idx, monomorph_scope)
			}
			if verbose {
				eprintln('mono AST after pass: ${a.nodes.len}/${a.nodes.cap} nodes, ${a.children.len}/${a.children.cap} children')
			}
			// The scoped transformer interns every escaping node payload while the
			// parent arena is temporarily current. Only an unexpected AST backing
			// growth still needs a full promotion clone.
			if a.nodes.cap != monomorph_nodes_cap || a.children.cap != monomorph_children_cap
				|| scoped_value_owned(monomorph_scope, a.nodes.data)
				|| scoped_value_owned(monomorph_scope, a.children.data) {
				a = clone_flat_ast_after_transform(a)
				pre_tc.rebind_ast(a)
			}
			if a.specialized_fn_nodes.len != base_specialized_fns {
				a.specialized_fn_nodes = a.specialized_fn_nodes.clone()
			}
			promote_scoped_checker_node_caches(mut pre_tc, monomorph_scope, base_monomorph_nodes)
			pre_tc.rebuild_scoped_transform_signature_maps()
			pre_tc.promote_scoped_transform_interners(0, 0, monomorph_scope)
			promote_scoped_monomorph_metadata(mut pre_tc)
			monomorph_used_fns = clone_string_bool_map(monomorph_used_fns)
			monomorph_errors = clone_string_list(monomorph_errors)
			generated_monomorph_specs = clone_monomorph_cache_specs(generated_monomorph_specs)
			pre_tc.set_fresh_type_cache(parse_cache_enabled)
			prealloc_scope_free_for_v3(monomorph_scope)
		} else {
			if cached_monomorph_specs.len > 0 {
				monomorph_used_fns, monomorph_errors, generated_monomorph_specs = transform.monomorphize_with_used_checked_config_scoped_cached(mut a,
					&pre_tc, monomorph_input_used, should_parallel_monomorphize()
					&& cache_state.parsed_from_source.len == 0, unsafe { nil },
					cached_monomorph_specs)
			} else {
				monomorph_used_fns, monomorph_errors = transform.monomorphize_with_used_checked_config(mut a,
					&pre_tc, monomorph_input_used, should_parallel_monomorphize()
					&& cache_state.parsed_from_source.len == 0)
			}
		}
		if incremental_cache_hit {
			incremental_stage_used_fns = monomorph_used_fns.move()
		} else {
			used_fns = monomorph_used_fns.move()
		}
		if monomorph_errors.len > 0 {
			eprintln('type checker found ${monomorph_errors.len} error(s):')
			for message in monomorph_errors {
				eprintln(message)
			}
			exit(1)
		}
	} else {
		erase_unreachable_generic_type_templates(mut pre_tc)
	}
	pre_tc.clear_c_type_cache()
	// Transform and monomorphization can synthesize or rewrite payload text.
	// They run with private/arena-backed worker state; publish only canonical,
	// compilation-owned strings after all worker merges are complete.
	if !transform_texts_canonical {
		a.intern_node_texts_from(0)
	}
	if cgen_cache_hit {
		b.step('monomorphize (cached)')
	} else if incremental_cache_hit {
		b.step('monomorphize (incremental)')
	} else if generic_cache_hit {
		b.step('monomorphize (dependency cache)')
	} else {
		b.step('monomorphize')
	}
	mut newly_cached_module_count := 0
	if backend == 'arm64' {
		$if !skip_arm64 ? {
			// SSA + ARM64 native backend
			mut m := ssa.build_with_used(a, used_fns, pre_tc)
			b.step('ssa build')
			b.metric('SSA values before optimize', m.values.len, 'values')
			b.metric('SSA instructions before optimize', m.instrs.len, 'instructions')
			b.metric('SSA blocks before optimize', m.blocks.len, 'blocks')

			if is_prod {
				optimize.optimize(mut m)
				b.step('optimize')
				b.metric('SSA values after optimize', m.values.len, 'values')
				b.metric('SSA instructions after optimize', m.instrs.len, 'instructions')
				b.metric('SSA blocks after optimize', m.blocks.len, 'blocks')
			}

			mut g := arm64.Gen.new(m)
			g.gen()
			b.step('arm64 gen')

			g.write_and_link(bin_file)
			b.step('link')
		}
	} else {
		// C backend (default)
		c_standard := c_standard_flag(prefs.c99)
		use_cached_dev_dylib := cache_state.manager.enabled && !is_prod && !is_shared
			&& !is_selfhost && prefs.normalized_target_os() == 'macos'
		mut cc_dir := ''
		mut cc_src := output_file
		mut cc_out := ''
		if !c_only {
			bin_dir := if os.dir(bin_file).len > 0 {
				os.real_path(os.dir(bin_file))
			} else {
				os.getwd()
			}
			cc_dir = os.join_path_single(bin_dir,
				'.${os.base(bin_file)}.v3cc.${os.getpid()}.${rand.ulid()}')
			os.mkdir(cc_dir) or {
				eprintln('failed to create C build directory ${cc_dir}: ${err}')
				exit(1)
			}
			cc_src = os.join_path_single(cc_dir, 'src.c')
			cc_out = os.join_path_single(cc_dir, 'out')
		}
		mut published_c_source := cc_src
		cache_plan_file := if cache_state.manager.enabled {
			os.join_path_single(cc_dir, 'cache_plan.c')
		} else {
			''
		}
		mut generated_c_flags := cgen_cache_metadata.flags.clone()
		mut interface_impl_signature := cgen_cache_metadata.interface_impl_signature
		mut cgen_was_parallel := false
		incremental_c_declarations := if incremental_cache_hit {
			os.read_file(incremental_tcc_declarations_path) or {
				eprintln('error reading incremental C declarations ${incremental_tcc_declarations_path}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		} else {
			''
		}
		cgen_used_fns := if incremental_cache_hit {
			incremental_stage_used_fns
		} else {
			used_fns
		}
		if cgen_cache_hit && !cgen_prepared_hit {
			os.cp(cgen_cache_entry.source, cache_plan_file) or {
				eprintln('error restoring cached C plan ${cgen_cache_entry.source}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		}
		if !cgen_cache_hit && scope_prealloc_cgen {
			cgen_parse_cache_enabled := pre_tc.type_cache_parse_enabled()
			cgen_scope := prealloc_scope_begin_for_v3()
			mut g := cgen.FlatGen.new()
			g.set_initial_c_flags(user_c_flags)
			g.set_c99_mode(prefs.c99)
			g.set_prealloc('prealloc' in prefs.user_defines)
			g.set_skip_generics(skip_transform_generics)
			g.set_compiler_vexe(prefs.vexe)
			g.set_target(prefs.target)
			g.set_cache_split(cache_state.manager.enabled)
			g.set_program_body_only(generic_cache_hit)
			g.set_cache_program_files(user_files)
			g.set_incremental_fn_names(incremental_changed_names)
			g.set_cached_support_declarations(incremental_c_declarations)
			g.set_scope_parallel_workers(!generic_cache_hit)
			generated_path := if cache_state.manager.enabled { cache_plan_file } else { cc_src }
			g.gen_to_file_with_used_test_options(generated_path, a, cgen_used_fns, &pre_tc,
				cache_no_parallel_cgen, test_files) or {
				eprintln('error writing ${generated_path}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			scoped_c_flags := g.c_flags()
			g.free_parallel_worker_scopes()
			prealloc_scope_leave_for_v3(cgen_scope)
			if !incremental_cache_hit {
				generated_c_flags = clone_string_list(scoped_c_flags)
			}
			// Cgen's synchronous type queries memoize through the shared checker.
			// Reattach empty parent-owned interners and caches before releasing its
			// stage arena; all of them can grow while servicing those queries.
			pre_tc.reset_type_interners()
			pre_tc.set_fresh_type_cache(cgen_parse_cache_enabled)
			prealloc_scope_free_for_v3(cgen_scope)
		} else if !cgen_cache_hit {
			mut g := cgen.FlatGen.new()
			g.set_initial_c_flags(user_c_flags)
			g.set_c99_mode(prefs.c99)
			g.set_prealloc('prealloc' in prefs.user_defines)
			g.set_skip_generics(skip_transform_generics)
			g.set_compiler_vexe(prefs.vexe)
			g.set_target(prefs.target)
			g.set_cache_split(cache_state.manager.enabled)
			g.set_program_body_only(generic_cache_hit)
			g.set_cache_program_files(user_files)
			g.set_incremental_fn_names(incremental_changed_names)
			g.set_cached_support_declarations(incremental_c_declarations)
			generated_path := if cache_state.manager.enabled { cache_plan_file } else { cc_src }
			g.gen_to_file_with_used_test_options(generated_path, a, cgen_used_fns, &pre_tc,
				cache_no_parallel_cgen, test_files) or {
				eprintln('error writing ${generated_path}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			if !incremental_cache_hit {
				generated_c_flags = g.c_flags()
			}
		}
		if incremental_cache_hit {
			changed_source := os.read_file(cache_plan_file) or {
				eprintln('error reading incremental C source ${cache_plan_file}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			merged_source := merge_incremental_program_body(incremental_cached_body,
				changed_source, incremental_changed_keys) or {
				os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
				restart_v3_after_cache_invalidation()
				''
			}
			os.write_file(cache_plan_file, merged_source) or {
				eprintln('error writing merged incremental C source ${cache_plan_file}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		}
		if cgen_cache_hit {
			b.step('cgen (cached)')
		} else if incremental_cache_hit {
			b.step_parallel('cgen (incremental)', cgen_was_parallel)
		} else {
			b.step_parallel('cgen', cgen_was_parallel)
		}
		if c_only {
			b.metric('generated C size', os.file_size(cc_src), 'bytes')
			b.print_report()
			return
		}

		pic_flag := shared_pic_flag(is_shared || use_cached_dev_dylib, prefs.normalized_target_os())
		target_args := c_compiler_target_args(prefs.target, c_compiler_explicit) or {
			eprintln(err.msg())
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		mut warn_args := if is_strict {
			['-Wall', '-Wextra', '-Werror=implicit-function-declaration', '-Wno-unused-variable',
				'-Wno-unused-parameter', '-Wno-int-conversion', '-Wno-missing-braces']
		} else {
			['-w']
		}
		// Match the normal V driver's macOS compatibility flags. Apple SDK and
		// third-party headers commonly add const qualifiers to callback typedefs,
		// and Clang otherwise treats assignments from V's C declarations as errors.
		if prefs.normalized_target_os() == 'macos' {
			warn_args << ['-Wno-incompatible-function-pointer-types', '-Wno-typedef-redefinition']
		}
		needs_objective_c := c_flags_need_objective_c(generated_c_flags)
		resolved_c_flags := prepare_c_flags_for_link(generated_c_flags, prefs.c99, pic_flag,
			target_args, prefs.target, c_compiler, cc_dir, mut c_object_cache_stats) or {
			eprintln(err.msg())
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		b.step('C object cache')
		link_uses_non_c_language := c_link_flags_use_non_c_language(resolved_c_flags)
		link_c_standard := if link_uses_non_c_language {
			''
		} else {
			c_standard
		}
		mut cached_objects := []string{}
		mut cached_dev_dylib := ''
		mut prefix_source_identity := cgen_cache_metadata.prefix_source_identity
		mut tcc_main_file := ''
		mut cached_program_body_source := if cgen_cache_hit { cgen_cache_entry.source } else { '' }
		if cache_state.manager.enabled {
			cache_prepare_scope := prealloc_scope_begin_for_v3()
			if interface_impl_signature.len == 0 {
				interface_impl_signature = pre_tc.interface_impl_set_signature()
			}
			opt_flag := if is_prod { '-O2' } else { '' }
			warning_flags := warn_args.join(' ')
			compile_signature := v3_cached_object_compile_signature(c_standard, opt_flag, pic_flag,
				warning_flags, resolved_c_flags, needs_objective_c, interface_impl_signature)
			mut prepared_plan_entry := cgen_cache_entry
			mut prepared_cache := V3PreparedModuleCache{}
			if cgen_prepared_hit {
				prefix_source := os.read_file(cgen_prepared_entry.prefix) or {
					eprintln('error reading cached program prefix ${cgen_prepared_entry.prefix}: ${err.msg()}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				objects := cache_state.manager.valid_cgen_prepared_objects(cgen_cache_entry,
					compile_signature) or {
					if resolve_flag_specific_cache_objects(mut cache_state, compile_signature) {
						os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
						restart_v3_after_cache_invalidation()
					}
					resolved_objects := cache_object_paths(cache_state.objects)
					cache_state.manager.write_cgen_prepared_objects(cgen_cache_entry,
						compile_signature, resolved_objects) or {}
					resolved_objects
				}
				prepared_cache = V3PreparedModuleCache{
					program_prefix_source: prefix_source
					objects:               objects
				}
				published_c_source = cgen_prepared_entry.main
			} else {
				generated_source := os.read_file(cache_plan_file) or {
					eprintln('error reading cache-marked C source ${cache_plan_file}: ${err.msg()}')
					exit(1)
				}
				if generic_cache_hit {
					if incremental_cache_hit {
						cached_prefix := os.read_file(generic_cache_entry.prefix) or {
							eprintln('error reading cached incremental prefix ${generic_cache_entry.prefix}: ${err.msg()}')
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						prepared_cache = prepare_v3_incremental_cached_body(cache_plan_file,
							incremental_tcc_declarations_path, cached_prefix, compile_signature, mut
							cache_state) or {
							eprintln(err.msg())
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						prefix_source_identity = cgen_cache_metadata.prefix_source_identity
					} else {
						cached_prefix := os.read_file(generic_cache_entry.prefix) or {
							eprintln('error reading cached generic prefix ${generic_cache_entry.prefix}: ${err.msg()}')
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						cached_declarations := os.read_file(generic_cache_entry.declarations) or {
							eprintln('error reading cached generic declarations ${generic_cache_entry.declarations}: ${err.msg()}')
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						prepared_cache = prepare_v3_cached_generic_body(generated_source,
							cached_prefix, cached_declarations, compile_signature, mut cache_state) or {
							eprintln(err.msg())
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
					}
				} else {
					prepared_cache = prepare_v3_module_cache(generated_source, c_standard,
						opt_flag, pic_flag, warning_flags, resolved_c_flags, needs_objective_c,
						interface_impl_signature, mut cache_state) or {
						eprintln(err.msg())
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
				}
				os.write_file(cc_src, prepared_cache.main_source) or {
					eprintln('error writing cached main source ${cc_src}: ${err.msg()}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				if prefix_source_identity.len == 0 {
					prefix_source_identity = v3_program_prefix_source_identity(prepared_cache.program_prefix_source,
						prepared_cache.objects)
				}
				if !cgen_cache_hit {
					published_cgen_cache_input := v3_cgen_cache_input(cache_state, user_files,
						user_c_flags)
					prepared_plan_entry = cache_state.manager.write_cgen(published_cgen_cache_input.source_files,
						published_cgen_cache_input.generation_signature,
						published_cgen_cache_input.dependency_inputs, generated_source, encode_v3_cgen_metadata(generated_c_flags,
						interface_impl_signature, prefix_source_identity)) or {
						modulecache.CgenEntry{}
					}
				}
				if incremental_cache_hit && prepared_plan_entry.source.len > 0 {
					stable_main_source := v3_incremental_main_source(incremental_tcc_declarations_path,
						prepared_plan_entry.source)
					prepared_cache.main_source = stable_main_source
					prepared_cache.tcc_main_source = stable_main_source
					os.write_file(cc_src, stable_main_source) or {
						eprintln('error writing incremental cached main source ${cc_src}: ${err.msg()}')
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
				}
				if prepared_plan_entry.stamp.len > 0 {
					cached_program_body_source = prepared_plan_entry.source
					cache_state.manager.write_cgen_prepared(prepared_plan_entry,
						prepared_cache.main_source, prepared_cache.tcc_main_source,
						prepared_cache.program_prefix_source) or {}
					cache_state.manager.write_cgen_prepared_objects(prepared_plan_entry,
						compile_signature, prepared_cache.objects) or {}
				}
				if !generic_cache_hit && generic_cache_signature.len > 0
					&& generated_monomorph_specs.len > 0 {
					cache_state.manager.write_generic_program(generic_cache_input.source_files,
						generic_cache_signature, generic_cache_input.generation_signature,
						generic_cache_input.dependency_inputs,
						encode_monomorph_cache_specs(generated_monomorph_specs),
						encode_cached_used_fns(used_fns), prepared_cache.program_prefix_source,
						modulecache.prune_unreferenced_static_string_definitions(prepared_cache.program_declarations),
						prepared_cache.program_body_cache,
						encode_cached_runtime_strings(generic_cache_runtime_strings), encode_v3_cgen_metadata(generated_c_flags,
						interface_impl_signature, prefix_source_identity)) or {}
				}
				if !incremental_cache_hit && !generic_cache_hit
					&& incremental_snapshot.declaration_signature.len > 0 {
					cache_state.manager.write_incremental_program(generic_cache_input.source_files,
						incremental_snapshot.declaration_signature,
						generic_cache_input.generation_signature,
						generic_cache_input.dependency_inputs,
						encode_incremental_manifest(incremental_snapshot),
						prepared_cache.program_body_cache, encode_cached_used_fns(used_fns),
						encode_monomorph_cache_specs(generated_monomorph_specs),
						prepared_cache.program_prefix_source,
						modulecache.prune_unreferenced_static_string_definitions(prepared_cache.program_declarations),
						prepared_cache.tcc_program_declarations, prepared_cache.objects, encode_v3_cgen_metadata(generated_c_flags,
						interface_impl_signature, prefix_source_identity)) or {}
				}
			}
			prealloc_scope_leave_for_v3(cache_prepare_scope)
			if prefix_source_identity.len > 0 {
				prefix_source_identity = prefix_source_identity.clone()
			}
			if cached_program_body_source.len > 0 {
				cached_program_body_source = cached_program_body_source.clone()
			}
			b.step(if cgen_prepared_hit { 'C module plan (cached)' } else { 'C module plan' })
			cached_objects = clone_string_list(prepared_cache.objects)
			newly_cached_module_count = prepared_cache.newly_cached_modules
			if use_cached_dev_dylib {
				tcc_main_file = os.join_path_single(cc_dir, 'main.c')
				if cgen_prepared_hit {
					os.link(cgen_prepared_entry.tcc, tcc_main_file) or {
						os.cp(cgen_prepared_entry.tcc, tcc_main_file) or {
							eprintln('error restoring cached TinyCC program unit ${cgen_prepared_entry.tcc}: ${err.msg()}')
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
					}
				} else {
					os.write_file(tcc_main_file, prepared_cache.tcc_main_source) or {
						eprintln('error writing cached TinyCC program unit ${tcc_main_file}: ${err.msg()}')
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
				}
				if prefix_source_identity.len == 0 {
					prefix_source_identity = v3_program_prefix_source_identity(prepared_cache.program_prefix_source,
						prepared_cache.objects)
				}
				prefix_object := compile_v3_program_prefix(prepared_cache.program_prefix_source,
					prefix_source_identity, v3_program_prefix_external_input_paths(&cache_state),
					&cache_state.manager, c_standard, opt_flag, pic_flag, warning_flags,
					resolved_c_flags, needs_objective_c, target_args, prefs.target, c_compiler, mut
					c_object_cache_stats) or {
					eprintln(err.msg())
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				cached_dev_dylib = compile_v3_dev_dylib(prefix_object, prepared_cache.objects,
					resolved_c_flags, &cache_state.manager, target_args, prefs.target, c_compiler,
					cc_dir, mut c_object_cache_stats) or {
					eprintln(err.msg())
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
			}
			prealloc_scope_free_for_v3(cache_prepare_scope)
			os.rm(cache_plan_file) or {}
		}
		if use_cached_dev_dylib {
			b.step('C dylib cache')
		}
		b.metric('generated C size', os.file_size(published_c_source), 'bytes')
		published_c := '${output_file}.tmp.${os.getpid()}.${rand.ulid()}'
		os.cp(published_c_source, published_c) or {
			eprintln('failed to stage generated C output ${output_file}: ${err}')
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		os.mv(published_c, output_file) or {
			eprintln('failed to publish generated C output ${output_file}: ${err}')
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		// Compile inside a per-output build dir, using constant relative source/output basenames,
		// then move the result to bin_file. On macOS arm64 tcc bakes the -o basename into the
		// ad-hoc code-signature identifier and the input .c path into the symbol table, so building
		// `v5.c`->`v5` vs `v6.c`->`v6` directly would make the binaries differ only by those embedded
		// names (plus the code-directory hashes covering them). Compiling fixed `src.c`->`out` keeps
		// those embedded names identical, so the self-host chain is byte-for-byte reproducible
		// (v5 == v6). A random per-invocation directory beside the final output prevents
		// concurrent compilers targeting the same path from sharing partial files.
		mut result := os.Result{}
		mut tried_tcc := false
		mut tcc_cache_hit := false
		if cached_dev_dylib.len > 0 && tcc_main_file.len > 0 {
			tried_tcc = true
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_lib_dir := os.join_path_single(tcc_dir, 'lib')
			tcc_includes := '-I${os.join_path_single(tcc_lib_dir, 'include')}'
			tcc_lib := '-L${tcc_lib_dir}'
			mut tcc_args := [c_standard, tcc_includes, tcc_lib, '-w',
				'-Werror=implicit-function-declaration']
			tcc_args << tcc_cached_main_flags(resolved_c_flags)
			tcc_args << ['-o', 'out', os.base(tcc_main_file)]
			atomic_s := tcc_atomic_s_arg(prefs)
			if atomic_s.len > 0 {
				tcc_args << atomic_s
			}
			tcc_args << cached_dev_dylib
			tcc_args << tcc_dynamic_link_flags(resolved_c_flags)
			if '-lm' !in tcc_args {
				tcc_args << '-lm'
			}
			program_source_identity := '${prefix_source_identity}\n${modulecache.file_signature(tcc_main_file)}\n${if cached_program_body_source.len > 0 {
				modulecache.file_signature(cached_program_body_source)
			} else {
				''
			}}'
			tcc_cached_executable := v3_cached_tcc_executable_path(&cache_state.manager,
				program_source_identity, c_object_cache_stats.link_plan_signature, tcc_path,
				tcc_lib_dir, tcc_args)
			if os.is_file(tcc_cached_executable) {
				os.cp(tcc_cached_executable, cc_out) or {}
				tcc_cache_hit = os.is_file(cc_out)
			}
			println('  > ${cmdexec.display(tcc_path, tcc_args)}${if tcc_cache_hit {
				' (cached)'
			} else {
				''
			}}')
			if !tcc_cache_hit {
				result = cmdexec.run_in(tcc_path, tcc_args, cc_dir)
				if result.exit_code == 0 {
					publish_v3_cached_executable(cc_out, tcc_cached_executable)
				}
			}
		}
		// Cached module objects can make tcc accept an unresolved call in the
		// program translation unit and emit a broken executable. Compile and link
		// the much smaller cached main unit with the system C compiler so the same
		// undeclared-function diagnostics remain enforced.
		if !tried_tcc && !is_prod && !needs_objective_c && !link_uses_non_c_language
			&& target_args.len == 0 && !c_compiler_explicit && !cache_state.manager.enabled {
			tried_tcc = true
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_lib_dir := os.join_path_single(tcc_dir, 'lib')
			tcc_includes := '-I${os.join_path_single(tcc_lib_dir, 'include')}'
			tcc_lib := '-L${tcc_lib_dir}'
			mut tcc_args := []string{}
			if link_c_standard.len > 0 {
				tcc_args << link_c_standard
			}
			if pic_flag.len > 0 {
				tcc_args << pic_flag
			}
			tcc_args << [tcc_includes, tcc_lib]
			tcc_args << warn_args
			if is_shared {
				tcc_args << '-shared'
			}
			tcc_args << ['-o', 'out', 'src.c']
			atomic_s := tcc_atomic_s_arg(prefs)
			if atomic_s.len > 0 {
				tcc_args << atomic_s
			}
			tcc_args << resolved_c_flags
			tcc_args << '-lm'
			println('  > ${cmdexec.display(tcc_path, tcc_args)}')
			result = cmdexec.run_in(tcc_path, tcc_args, cc_dir)
		}
		if is_prod || !tried_tcc || result.exit_code != 0 {
			if !os.is_file(cc_src) {
				os.cp(published_c_source, cc_src) or {
					eprintln('error restoring cached main source ${published_c_source}: ${err.msg()}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
			}
			mut cc_args := []string{}
			cc_args << target_args
			if link_c_standard.len > 0 {
				cc_args << link_c_standard
			}
			if is_prod {
				cc_args << '-O2'
			}
			if pic_flag.len > 0 {
				cc_args << pic_flag
			}
			cc_args << warn_args
			cc_args << '-Wno-int-conversion'
			if prefs.normalized_target_os() == 'macos' && !is_shared {
				cc_args << '-Wl,-stack_size,0x4000000'
			}
			if is_shared {
				cc_args << '-shared'
			}
			cc_args << ['-o', 'out']
			fallback_source := if cached_dev_dylib.len > 0 && tcc_main_file.len > 0 {
				os.base(tcc_main_file)
			} else {
				'src.c'
			}
			if fallback_source == os.base(tcc_main_file) {
				cc_args << ['-D__TINYC__', '-Wno-implicit-function-declaration']
				cc_args << fallback_source
			} else if needs_objective_c {
				cc_args << ['-x', 'objective-c', fallback_source, '-x', 'none']
			} else {
				cc_args << fallback_source
			}
			cc_args << cached_objects
			if cached_dev_dylib.len > 0 {
				cc_args << cached_dev_dylib
			}
			cc_args << resolved_c_flags
			cc_args << '-lm'
			println('  > ${cmdexec.display(c_compiler, cc_args)}')
			result = cmdexec.run_in(c_compiler, cc_args, cc_dir)
			if result.exit_code != 0 {
				eprintln('C compilation failed:')
				eprintln(result.output)
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		}
		os.mv(cc_out, bin_file) or {
			eprintln('failed to finalize ${bin_file}: ${err}')
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		for temporary_object in c_object_cache_stats.temporary_objects {
			os.rm(temporary_object) or {}
		}
		for source_flag in generated_c_flags {
			clean := source_flag.trim_space()
			if c_generated_native_source_context(clean, cc_dir) {
				os.rm(clean) or {}
			}
		}
		os.rm(tcc_main_file) or {}
		os.rm(cc_src) or {}
		os.rmdir(cc_dir) or {}
		b.step(if tcc_cache_hit { 'cc (cached)' } else { 'cc' })
		if should_run {
			run_result := run_binary(bin_file, run_args)
			if run_result != 0 {
				exit(run_result)
			}
			b.step('run')
		} else if test_files.len > 0 && !explicit_output {
			test_result := run_test_binary(bin_file)
			if test_result != 0 {
				exit(test_result)
			}
			b.step('test')
		}
	}

	worker_stats := a.worker_stats()
	b.metric('worker phase callbacks', i64(worker_stats.tasks_run), 'tasks')
	b.metric('worker async callbacks', i64(worker_stats.async_tasks), 'tasks')
	b.metric('worker forced sync callbacks', i64(worker_stats.forced_sync_tasks), 'tasks')
	b.metric('worker fallback callbacks', i64(worker_stats.fallback_tasks), 'tasks')
	b.metric('worker launch attempts', i64(worker_stats.launch_attempts), 'threads')
	b.metric('worker launch failures', i64(worker_stats.launch_failures), 'threads')
	b.metric('worker queue wait', i64(worker_stats.queue_wait_ns), 'ns')
	b.metric('worker execution', i64(worker_stats.worker_run_ns), 'ns')
	b.metric('worker utilization', i64(worker_stats.utilization_ppm), 'ppm')
	b.metric('C object cache requests', c_object_cache_stats.requests, 'objects')
	b.metric('C object cache direct', c_object_cache_stats.direct_objects, 'objects')
	b.metric('C object content-key hits', c_object_cache_stats.content_key_hits, 'objects')
	b.metric('C object manifest hits', c_object_cache_stats.dependency_manifest_hits, 'objects')
	b.metric('C object cache misses', c_object_cache_stats.misses, 'objects')
	b.metric('C object dependency scans', c_object_cache_stats.dependency_scans, 'objects')
	b.metric('C object dependency files', c_object_cache_stats.dependency_files, 'files')
	b.metric('C object dependency reads', c_object_cache_stats.dependency_file_reads, 'files')
	b.metric('C object dep-scan fallbacks', c_object_cache_stats.dependency_scan_fallbacks,
		'objects')
	b.metric('C object publish races', c_object_cache_stats.publish_races, 'objects')
	b.metric('C object input-snapshot races', c_object_cache_stats.input_snapshot_races, 'objects')
	b.print_report()
	if newly_cached_module_count > 0 {
		println('Hint: cached ${newly_cached_module_count} modules. They will not be recompiled on the next run unless they change.')
	}
}

fn builtin_bundle_source_files(prefs &pref.Preferences, builtin_files []string) []string {
	mut files := builtin_files.clone()
	mut seen := map[string]bool{}
	for file in files {
		seen[os.real_path(file)] = true
	}
	for rel in ['strconv', 'strings', 'hash', os.join_path('math', 'bits')] {
		dir := os.join_path(prefs.vroot, 'vlib', rel)
		if !os.is_dir(dir) {
			continue
		}
		for file in pref.get_v_files_from_dir_for_target(dir, prefs.user_defines, prefs.target) {
			key := os.real_path(file)
			if seen[key] {
				continue
			}
			seen[key] = true
			files << file
		}
	}
	files.sort()
	return files
}

fn v3_incremental_main_source(tcc_declarations_path string, body_path string) string {
	declarations_include := tcc_declarations_path.replace('\\', '\\\\').replace('"', '\\"')
	body_include := body_path.replace('\\', '\\\\').replace('"', '\\"')
	return '#define V3CACHE_PROGRAM_UNIT 1\n#include "${declarations_include}"\n#include "${body_include}"\n'
}

fn prepare_v3_incremental_cached_body(body_path string, tcc_declarations_path string, cached_prefix string, compile_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	objects := cache_object_paths(state.objects)
	if !os.is_file(tcc_declarations_path) || objects.len == 0 {
		return error('v3 incremental C declarations are unavailable')
	}
	main_source := v3_incremental_main_source(tcc_declarations_path, body_path)
	return V3PreparedModuleCache{
		main_source:           main_source
		tcc_main_source:       main_source
		program_prefix_source: cached_prefix
		objects:               objects
	}
}

fn prepare_v3_cached_generic_body(generated_source string, cached_prefix string, cached_declarations string, compile_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if !state.manager.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	split := modulecache.split_generated_c(generated_source)!
	main_body := split.modules['main'] or { '' }
	current_string_definitions := modulecache.static_string_definitions(split.prefix)
	combined_declarations := cached_declarations + current_string_definitions
	tcc_declarations := tcc_cached_main_source(combined_declarations)
	main_source := '#define V3CACHE_PROGRAM_UNIT 1\n' + tcc_declarations + main_body
	return V3PreparedModuleCache{
		main_source:              main_source
		tcc_main_source:          main_source
		main_body:                main_body
		program_prefix_source:    cached_prefix
		program_declarations:     combined_declarations
		tcc_program_declarations: tcc_declarations
		objects:                  cache_object_paths(state.objects)
	}
}

fn prepare_v3_module_cache(generated_source string, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, interface_impl_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if !state.manager.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	split := modulecache.split_generated_c(generated_source)!
	mut parsed_modules := state.parsed_from_source.keys()
	parsed_modules.sort()
	mut newly_cached_modules := map[string]bool{}
	mut needs_declarations := !state.bundle_valid
	if !needs_declarations {
		for module_name in parsed_modules {
			if !module_is_builtin_bundle(state, module_name) {
				needs_declarations = true
				break
			}
		}
	}
	declarations := if needs_declarations {
		cache_source_without_cached_native_inputs(modulecache.declaration_header(split.prefix),
			state, false)
	} else {
		''
	}
	compile_signature := v3_cached_object_compile_signature(c_standard, opt_flag, pic_flag,
		warning_flags, generated_c_flags, objective_c, interface_impl_signature)
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	main_body := split.modules['main'] or { '' }
	main_prefix := cache_source_without_cached_native_inputs(split.prefix, state, true)
	program_specializations := split.modules['__v3_program_specializations'] or { '' }
	program_support := split.modules['__v3_program_support'] or { '' }
	dylib_prefix := modulecache.prune_unreferenced_static_string_definitions(main_prefix +
		program_specializations + program_support)
	main_source := '#define V3CACHE_PROGRAM_UNIT 1\n' + main_prefix + program_specializations +
		program_support + main_body
	main_declarations := cache_source_without_cached_native_inputs(modulecache.declaration_header(
		split.prefix + program_specializations + program_support), state, false)
	tcc_declarations := tcc_cached_main_source(main_declarations)
	tcc_main := '#define V3CACHE_PROGRAM_UNIT 1\n' + tcc_declarations + main_body
	mut object_paths := state.objects.clone()
	mut bundle_body := strings.new_builder(4096)
	mut split_modules := split.modules.keys()
	split_modules.sort()
	for module_name in split_modules {
		if module_is_builtin_bundle(state, module_name) {
			bundle_body.write_string(split.modules[module_name])
		}
	}
	if !state.bundle_valid {
		entry := state.manager.object_entry('builtin', state.bundle_sources, compile_signature)
		bundle_native_includes := cache_native_source_includes(state,
			cache_builtin_bundle_roots(state))
		module_source := if bundle_native_includes.len > 0 {
			'#define V3CACHE_PROGRAM_UNIT 1\n' + declarations + '#undef V3CACHE_PROGRAM_UNIT\n' +
				bundle_native_includes + bundle_body.str()
		} else {
			declarations + bundle_body.str()
		}
		compile_v3_cached_object(entry, module_source, c_standard, opt_flag, pic_flag,
			warning_flags, generated_c_flags, objective_c)!
		for module_name, header in state.headers {
			if !module_is_builtin_bundle(state, module_name) {
				continue
			}
			if source_files := state.module_sources[module_name] {
				state.manager.write_header(module_name, source_files, header)!
			}
		}
		bundle_dependencies := cache_object_dependency_signatures(state,
			cache_builtin_bundle_roots(state))
		state.manager.write_stamp('builtin', state.bundle_sources, bundle_dependencies,
			compile_signature)!
		object_paths['builtin'] = entry.object
		state.bundle_valid = true
		for module_name in state.headers.keys() {
			if module_is_builtin_bundle(state, module_name) {
				newly_cached_modules[module_name] = true
			}
		}
	}
	unsafe { bundle_body.free() }

	for module_name in parsed_modules {
		if module_is_builtin_bundle(state, module_name) {
			continue
		}
		source_files := state.module_sources[module_name] or { continue }
		entry := state.manager.object_entry(module_name, source_files, compile_signature)
		body := split.modules[module_name] or {
			split.modules[module_name.all_after_last('.')] or { '' }
		}
		native_includes := cache_native_source_includes(state, [module_name])
		module_source := if native_includes.len > 0 {
			'#define V3CACHE_PROGRAM_UNIT 1\n' + declarations + '#undef V3CACHE_PROGRAM_UNIT\n' +
				native_includes + body
		} else {
			declarations + body
		}
		compile_v3_cached_object(entry, module_source, c_standard, opt_flag, pic_flag,
			warning_flags, generated_c_flags, objective_c)!
		if header := state.headers[module_name] {
			state.manager.write_header(module_name, source_files, header)!
		}
		dependencies := cache_object_dependency_signatures(state, [module_name])
		state.manager.write_stamp(module_name, source_files, dependencies, compile_signature)!
		object_paths[module_name] = entry.object
		newly_cached_modules[module_name] = true
	}

	return V3PreparedModuleCache{
		main_source:              main_source
		tcc_main_source:          tcc_main
		main_body:                main_body
		program_body_cache:       incremental_static_string_markers(split.prefix) +
			'/* V3CACHE_BODY_BEGIN */\n/* V3CACHE_MODULE main */\n' + main_body +
			'\n/* V3CACHE_BODY_END */\n'
		program_prefix_source:    '#define V3CACHE_PROGRAM_UNIT 1\n' + dylib_prefix
		program_declarations:     main_declarations
		tcc_program_declarations: tcc_declarations
		objects:                  cache_object_paths(object_paths)
		newly_cached_modules:     newly_cached_modules.len
	}
}

fn cache_object_paths(object_paths map[string]string) []string {
	mut objects := []string{}
	mut object_names := object_paths.keys()
	object_names.sort()
	for name in object_names {
		path := object_paths[name]
		if path.len > 0 && path !in objects {
			objects << path
		}
	}
	return objects
}

fn cache_source_without_cached_native_inputs(source string, state &V3ModuleCacheState, keep_main bool) string {
	mut excluded := map[string]bool{}
	for raw_module_name, paths in state.module_external_inputs {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if (keep_main && module_name == 'main') || !state.native_source_modules[module_name] {
			continue
		}
		for path in paths {
			if !c_flag_is_c_source_file(path) {
				continue
			}
			clean := os.real_path(path).replace('\\', '/').replace('"', '\\"')
			excluded['#include "${clean}"'] = true
		}
	}
	if excluded.len == 0 {
		return source
	}
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		if !excluded[line.trim_space()] {
			out.writeln(line)
		}
	}
	return out.str()
}

fn cache_native_source_includes(state &V3ModuleCacheState, module_names []string) string {
	mut selected := map[string]bool{}
	for module_name in module_names {
		selected[module_name] = true
	}
	mut paths := map[string]bool{}
	for raw_module_name, inputs in state.module_external_inputs {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if !selected[module_name] || !state.native_source_modules[module_name] {
			continue
		}
		for input in inputs {
			if c_flag_is_c_source_file(input) {
				paths[os.real_path(input)] = true
			}
		}
	}
	mut sorted_paths := paths.keys()
	sorted_paths.sort()
	mut out := strings.new_builder(sorted_paths.len * 96)
	for path in sorted_paths {
		clean := path.replace('\\', '/').replace('"', '\\"')
		out.writeln('#include "${clean}"')
	}
	return out.str()
}

fn module_cache_source_path_set(source_files []string) map[string]bool {
	mut paths := map[string]bool{}
	for source_file in source_files {
		paths[os.real_path(source_file)] = true
	}
	return paths
}

fn module_is_builtin_bundle(state &V3ModuleCacheState, module_name string) bool {
	if module_name !in modulecache.builtin_bundle_modules {
		return false
	}
	source_files := state.module_sources[module_name] or { return false }
	if source_files.len == 0 {
		return false
	}
	for source_file in source_files {
		if !state.bundle_source_paths[os.real_path(source_file)] {
			return false
		}
	}
	return true
}

fn cache_builtin_bundle_roots(state &V3ModuleCacheState) []string {
	mut roots := []string{}
	for module_name in state.module_sources.keys() {
		if module_is_builtin_bundle(state, module_name) {
			roots << module_name
		}
	}
	roots.sort()
	return roots
}

fn cache_state_module_name(state &V3ModuleCacheState, name string) ?string {
	if name in state.module_sources {
		return name
	}
	short_name := name.all_after_last('.')
	mut found := ''
	for candidate in state.module_sources.keys() {
		if candidate.all_after_last('.') != short_name {
			continue
		}
		if found.len > 0 && found != candidate {
			return none
		}
		found = candidate
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn cache_dependency_modules(state &V3ModuleCacheState, roots []string) []string {
	mut root_set := map[string]bool{}
	mut seen := map[string]bool{}
	mut pending := []string{}
	for root in roots {
		canonical := cache_state_module_name(state, root) or { continue }
		if seen[canonical] {
			continue
		}
		root_set[canonical] = true
		seen[canonical] = true
		pending << canonical
	}
	mut dependencies := []string{}
	mut index := 0
	for index < pending.len {
		owner := pending[index]
		index++
		mut imported := state.module_dependencies[owner]
		if imported.len == 0 {
			imported = state.module_dependencies[owner.all_after_last('.')]
		}
		for dependency in imported {
			canonical := cache_state_module_name(state, dependency) or { continue }
			if seen[canonical] {
				continue
			}
			seen[canonical] = true
			pending << canonical
			if !root_set[canonical] {
				dependencies << canonical
			}
		}
	}
	dependencies.sort()
	return dependencies
}

fn cache_dependency_header_signatures(state &V3ModuleCacheState, roots []string) map[string]string {
	mut signatures := map[string]string{}
	for module_name in cache_dependency_modules(state, roots) {
		cache_add_module_header_signature(state, module_name, mut signatures)
	}
	return signatures
}

fn cache_add_module_header_signature(state &V3ModuleCacheState, module_name string, mut signatures map[string]string) {
	source_files := state.module_sources[module_name] or { return }
	entry := state.manager.entry(module_name, source_files)
	if header := state.headers[module_name] {
		signatures[entry.header] = modulecache.header_signature(header)
		return
	}
	header := os.read_file(entry.header) or { return }
	signatures[entry.header] = modulecache.header_signature(header)
}

fn cache_object_dependency_signatures(state &V3ModuleCacheState, roots []string) map[string]string {
	mut signatures := cache_dependency_header_signatures(state, roots)
	// The parser injects these imports into the program stream instead of a
	// particular file. Their position therefore cannot assign them to a stable
	// owner module; every cached object that can reference their generated
	// helpers conservatively tracks their interfaces.
	for implicit_name in ['sync', 'v.embed_file'] {
		if implicit_module := cache_state_module_name(state, implicit_name) {
			cache_add_module_header_signature(state, implicit_module, mut signatures)
		}
	}
	// Every cached translation unit is compiled with the builtin bundle's declarations
	// prefix, even when its V module has no explicit builtin import.
	for module_name in cache_builtin_bundle_roots(state) {
		cache_add_module_header_signature(state, module_name, mut signatures)
	}
	mut external_input_modules := map[string]bool{}
	for root in roots {
		if canonical := cache_state_module_name(state, root) {
			external_input_modules[canonical] = true
		}
	}
	for dependency in cache_dependency_modules(state, roots) {
		external_input_modules[dependency] = true
	}
	mut input_modules := state.module_external_inputs.keys()
	input_modules.sort()
	for raw_module_name in input_modules {
		if raw_module_name == '__v3_c_flags__' {
			for path in state.module_external_inputs[raw_module_name] {
				signature := modulecache.file_signature(path)
				if signature.len > 0 {
					signatures[path] = signature
				}
			}
			continue
		}
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { raw_module_name }
		}
		if !external_input_modules[module_name] {
			continue
		}
		for path in state.module_external_inputs[raw_module_name] {
			signature := modulecache.file_signature(path)
			if signature.len > 0 {
				signatures[path] = signature
			}
		}
	}
	return signatures
}

fn invalidate_changed_cache_dependents(mut state V3ModuleCacheState) bool {
	mut changed_headers := map[string]bool{}
	for module_name, header in state.headers {
		source_files := state.module_sources[module_name] or { continue }
		entry := state.manager.entry(module_name, source_files)
		old_header := os.read_file(entry.header) or { continue }
		if modulecache.header_signature(old_header) != modulecache.header_signature(header) {
			changed_headers[module_name] = true
		}
	}
	if changed_headers.len == 0 {
		return false
	}
	mut invalidated := false
	for object_name in state.objects.keys() {
		roots := if object_name == 'builtin' {
			cache_builtin_bundle_roots(state)
		} else {
			[object_name]
		}
		dependencies := cache_dependency_modules(state, roots)
		if !dependencies.any(it in changed_headers) {
			continue
		}
		source_files := if object_name == 'builtin' {
			state.bundle_sources
		} else {
			state.module_sources[object_name] or { continue }
		}
		stamp := state.manager.entry(object_name, source_files).object_stamp
		if os.is_file(stamp) {
			os.rm(stamp) or { continue }
			invalidated = true
		}
	}
	return invalidated
}

fn restart_v3_after_cache_invalidation() {
	restart_v3_with_args([])
}

fn restart_v3_without_cache() {
	restart_v3_with_args(['-nocache'])
}

fn restart_v3_with_args(extra_args []string) {
	executable := os.executable()
	mut args := extra_args.clone()
	args << os.args[1..]
	$if js {
		mut command := [os.quoted_path(executable)]
		for arg in args {
			command << os.quoted_path(arg)
		}
		exit(os.system(command.join(' ')))
	} $else {
		os.execvp(executable, args) or {
			eprintln('failed to restart ${executable}: ${err.msg()}')
			exit(1)
		}
	}
}

fn cache_external_input_owner_modules(state &V3ModuleCacheState) (map[string]bool, bool) {
	mut modules := map[string]bool{}
	for raw_module_name, paths in state.module_external_inputs {
		for path in paths {
			source := os.read_file(path) or { continue }
			if !modulecache.c_source_has_static_storage(source) {
				continue
			}
			if !c_flag_is_c_source_file(path) {
				return modules, false
			}
			if raw_module_name == 'main' {
				modules['main'] = true
				continue
			}
			module_name := cache_state_module_name(state, raw_module_name) or {
				return modules, false
			}
			modules[module_name] = true
		}
	}
	return modules, true
}

fn v3_cached_object_compile_signature(c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, interface_impl_signature string) string {
	mut flags := c_object_compile_flags(generated_c_flags)
	flags = flags.filter(!c_flag_is_object_file(it))
	mut inputs := []string{}
	for path in cgen.cache_c_flag_input_files(generated_c_flags) {
		inputs << '${path}\t${modulecache.file_signature(path)}'
	}
	return [
		'objective_c=${objective_c}',
		'c_standard=${c_standard.trim_space()}',
		'optimization=${opt_flag.trim_space()}',
		'pic=${pic_flag.trim_space()}',
		'warnings=${warning_flags.trim_space()}',
		'interfaces=${interface_impl_signature}',
		'flags=${flags.join('\\n')}',
		'inputs=${inputs.join('\\n')}',
	].join('\n')
}

fn resolve_flag_specific_cache_objects(mut state V3ModuleCacheState, compile_signature string) bool {
	for object_name in state.objects.keys() {
		roots := if object_name == 'builtin' {
			cache_builtin_bundle_roots(state)
		} else {
			[object_name]
		}
		source_files := if object_name == 'builtin' {
			state.bundle_sources
		} else {
			state.module_sources[object_name] or { continue }
		}
		dependency_inputs := cache_object_dependency_signatures(state, roots)
		if entry := state.manager.valid_object_for_compile_signature(object_name, source_files,
			compile_signature, dependency_inputs)
		{
			state.objects[object_name] = entry.object
		} else {
			return true
		}
	}
	return false
}

fn compile_v3_cached_object(entry modulecache.Entry, source string, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool) ! {
	unique := '${os.getpid()}.${rand.ulid()}'
	tmp_source := '${entry.c_source}.tmp.${unique}.c'
	defer {
		os.rm(tmp_source) or {}
	}
	os.write_file(tmp_source, source)!
	mut flags := c_object_compile_flags(generated_c_flags)
	flags = flags.filter(!c_flag_is_object_file(it))
	tmp_object := '${entry.object}.tmp.${unique}'
	mut args := []string{}
	if objective_c {
		args << ['-x', 'objective-c']
	}
	for value in [c_standard, opt_flag, pic_flag] {
		if value.len > 0 {
			args << value
		}
	}
	args << cgen.tokenize_c_flag(warning_flags)
	args << ['-Wno-int-conversion', '-c', '-o', tmp_object, tmp_source]
	args << flags
	result := cmdexec.run('cc', args)
	if result.exit_code != 0 {
		os.rm(tmp_object) or {}
		return error('failed to build cached module object ${entry.object}:\n${result.output}')
	}
	os.mv(tmp_object, entry.object) or {
		os.rm(tmp_object) or {}
		if !os.is_file(entry.object) {
			return error('failed to publish cached module object ${entry.object}: ${err}')
		}
	}
	os.mv(tmp_source, entry.c_source) or {
		if !os.is_file(entry.c_source) {
			return error('failed to publish cached module source ${entry.c_source}: ${err}')
		}
	}
}

fn vmod_subdirs(dir string) ![]string {
	vmod_path := os.join_path_single(dir, 'v.mod')
	if !os.exists(vmod_path) {
		return []string{}
	}
	manifest := vmod.from_file(vmod_path)!
	return manifest.unknown['subdirs'] or { []string{} }
}

fn expand_single_test_file_inputs(user_files []string, prefs &pref.Preferences) []string {
	mut expanded := []string{}
	mut seen := map[string]bool{}
	for file in user_files {
		if pref.is_test_file_for_backend(file, prefs.backend) {
			module_name := declared_module_in_file(file)
			if module_name.len > 0 && module_name != 'builtin' {
				for module_file in same_dir_module_source_files(file, module_name, prefs) {
					append_unique_file(mut expanded, mut seen, module_file)
				}
			}
		}
		append_unique_file(mut expanded, mut seen, file)
	}
	return expanded
}

fn same_dir_module_source_files(test_file string, module_name string, prefs &pref.Preferences) []string {
	dir := os.dir(test_file)
	mut files := []string{}
	for file in pref.get_v_files_from_dir_for_target(dir, prefs.user_defines, prefs.target) {
		if declared_module_in_file(file) == module_name {
			files << file
		}
	}
	return files
}

fn append_unique_file(mut files []string, mut seen map[string]bool, file string) {
	key := os.real_path(file)
	if seen[key] {
		return
	}
	seen[key] = true
	files << file
}

fn declared_module_in_file(path string) string {
	content := os.read_file(path) or { return '' }
	mut in_block_comment := false
	mut in_attr := false
	for raw_line in content.split_into_lines() {
		mut line := raw_line.trim_space()
		if in_block_comment {
			if end := line.index('*/') {
				line = line[end + 2..].trim_space()
				in_block_comment = false
			} else {
				continue
			}
		}
		if in_attr {
			if line.contains(']') {
				in_attr = false
			}
			continue
		}
		for line.starts_with('/*') {
			if end := line.index('*/') {
				line = line[end + 2..].trim_space()
			} else {
				in_block_comment = true
				line = ''
				break
			}
		}
		if line.len == 0 || line.starts_with('//') {
			continue
		}
		if line.starts_with('@[') || line.starts_with('[') {
			if !line.contains(']') {
				in_attr = true
			}
			continue
		}
		if line.starts_with('module ') {
			mut module_name := line[7..]
			if comment := module_name.index('//') {
				module_name = module_name[..comment]
			}
			if comment := module_name.index('/*') {
				module_name = module_name[..comment]
			}
			return module_name.trim_space()
		}
		return ''
	}
	return ''
}

fn project_root_for_files(files []string) string {
	for file in files {
		root := nearest_vmod_root_for_file(file)
		if root.len > 0 {
			return root
		}
	}
	if files.len > 0 {
		return os.dir(files[0])
	}
	return os.getwd()
}

fn nearest_vmod_root_for_file(path string) string {
	mut dir := if os.is_dir(path) { path } else { os.dir(path) }
	for _ in 0 .. 32 {
		if os.exists(os.join_path_single(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ''
}

// resolve_vroot_for_input resolves the V repo root for the compiler being built.
fn resolve_vroot_for_input(initial string, input_file string) string {
	if root := nearest_vroot_for_path(input_file) {
		return root
	}
	if root := nearest_vroot_for_path(os.getwd()) {
		return root
	}
	if is_valid_vroot(initial) {
		return initial
	}
	return initial
}

fn nearest_vroot_for_path(path string) ?string {
	if path.len == 0 {
		return none
	}
	mut dir := path
	if !os.is_abs_path(dir) {
		cwd := os.getwd()
		if cwd.len > 0 {
			dir = os.join_path_single(cwd, dir)
		}
	}
	if !os.is_dir(dir) {
		dir = os.dir(dir)
	}
	for _ in 0 .. 8 {
		if is_valid_vroot(dir) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return none
}

// is_valid_vroot reports whether is valid vroot applies in v3 entry point.
fn is_valid_vroot(root string) bool {
	return root.len > 0 && os.is_dir(builtin_dir_for_vroot(root))
}

// builtin_dir_for_vroot supports builtin dir for vroot handling for v3 entry point.
fn builtin_dir_for_vroot(root string) string {
	return os.join_path_single(os.join_path_single(root, 'vlib'), 'builtin')
}

// print_type_errors updates print type errors state for v3 entry point.
fn print_type_errors(errors []types.TypeError) {
	eprintln('type checker found ${errors.len} error(s):')
	max_errors := if errors.len < 20 { errors.len } else { 20 }
	for ei in 0 .. max_errors {
		err := errors[ei]
		eprintln('  [${err.file}] ${err.node_pos} node=${err.node} ${err.node_kind} `${err.node_value}`: ${err.msg}')
	}
	if errors.len > 20 {
		eprintln('  ... and ${errors.len - 20} more')
	}
}

fn diagnostic_root_for_input(input_file string, user_files []string) string {
	if input_file.len > 0 && os.is_dir(input_file) {
		return os.real_path(input_file)
	}
	if user_files.len > 0 {
		return os.real_path(os.dir(user_files[0]))
	}
	return os.real_path(os.getwd())
}

fn test_input_files(user_files []string, backend string, target pref.Target) []string {
	mut files := []string{}
	for file in user_files {
		if pref.is_test_file_for_platform(file, backend, target) {
			files << file
		}
	}
	return files
}

fn validate_test_file_harness_inputs(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) []string {
	if test_files.len == 0 {
		return []
	}
	mut selected_files := map[string]bool{}
	for file in test_files {
		selected_files[file] = true
	}
	mut errors := []string{}
	for file_idx, file_node in a.nodes {
		if !is_user_test_file_node(a, file_idx, file_node, selected_files) {
			continue
		}
		module_name := test_file_module_name(a, file_node)
		if module_name.len > 0 && module_name != 'main' && !file_node.value.ends_with('_test.v') {
			errors << 'no runnable tests in ${file_node.value}'
			continue
		}
		if test_file_has_executable_top_level_stmt(a, file_node) {
			errors << 'invalid test file ${file_node.value}: executable top-level statements are not supported in test files'
			continue
		}
		mut runnable_tests := 0
		mut invalid_items := 0
		mut decl_ids := []flat.NodeId{}
		collect_test_harness_decl_ids(a, file_node, mut decl_ids)
		for child_id in decl_ids {
			child := a.node(child_id)
			if child.value.starts_with('test_') {
				if is_supported_test_harness_fn(a, tc, child) {
					runnable_tests++
				} else {
					invalid_items++
					errors << 'invalid test signature: ${child.value} must be zero-arg and return void, ?, or !'
				}
			} else if is_test_harness_hook_name(child.value) {
				if !is_supported_test_harness_hook(a, tc, child) {
					invalid_items++
					errors << 'invalid test hook signature: ${child.value} must be zero-arg void'
				}
			}
		}
		if runnable_tests == 0 && invalid_items == 0 {
			errors << 'no runnable tests in ${file_node.value}'
		}
	}
	return errors
}

fn test_file_has_executable_top_level_stmt(a &flat.FlatAst, node &flat.Node) bool {
	if node.kind != .file && node.kind != .block {
		return false
	}
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) < a.user_code_start {
			continue
		}
		child := a.nodes[int(child_id)]
		if child.kind == .block {
			if test_file_has_executable_top_level_stmt(a, child) {
				return true
			}
		} else if test_file_is_executable_top_level_stmt(child) {
			return true
		}
	}
	return false
}

fn test_file_is_executable_top_level_stmt(node &flat.Node) bool {
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt,
		.for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt {
			true
		}
		else {
			false
		}
	}
}

fn collect_test_harness_decl_ids(a &flat.FlatAst, node &flat.Node, mut ids []flat.NodeId) {
	if node.kind != .file && node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) < a.user_code_start {
			continue
		}
		child := a.nodes[int(child_id)]
		if child.kind == .fn_decl {
			ids << child_id
		} else if child.kind == .block {
			collect_test_harness_decl_ids(a, child, mut ids)
		}
	}
}

fn is_user_test_file_node(a &flat.FlatAst, file_idx int, file_node flat.Node, test_files map[string]bool) bool {
	if file_idx < a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	return test_files[file_node.value]
}

fn test_file_module_name(a &flat.FlatAst, file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn is_supported_test_harness_fn(a &flat.FlatAst, tc &types.TypeChecker, node &flat.Node) bool {
	if node.generic_params().len > 0 {
		return false
	}
	if test_harness_fn_param_count(a, node) != 0 {
		return false
	}
	return test_harness_fn_return_supported(tc.parse_type(node.typ))
}

fn is_supported_test_harness_hook(a &flat.FlatAst, tc &types.TypeChecker, node &flat.Node) bool {
	if node.generic_params().len > 0 {
		return false
	}
	return test_harness_fn_param_count(a, node) == 0 && tc.parse_type(node.typ) is types.Void
}

fn test_harness_fn_param_count(a &flat.FlatAst, node &flat.Node) int {
	mut count := 0
	for i in 0 .. node.children_count {
		child := a.child_node(node, i)
		if child.kind == .param {
			count++
		}
	}
	return count
}

fn test_harness_fn_return_supported(ret types.Type) bool {
	return ret is types.Void || ret is types.OptionType || ret is types.ResultType
}

fn is_test_harness_hook_name(name string) bool {
	return name in ['testsuite_begin', 'testsuite_end', 'before_each', 'after_each']
}

fn set_diagnostic_files(mut tc types.TypeChecker, user_files []string) {
	for uf in user_files {
		tc.diagnostic_files[uf] = true
	}
}

fn erase_unreachable_generic_type_templates(mut tc types.TypeChecker) {
	for name in tc.struct_generic_params.keys() {
		tc.structs.delete(name)
		tc.unions.delete(name)
		tc.params_structs.delete(name)
		tc.unregister_short_type_name(name)
	}
	for name in tc.sum_generic_params.keys() {
		tc.sum_types.delete(name)
		tc.sum_generic_params.delete(name)
		tc.unregister_short_type_name(name)
	}
}

fn set_unsupported_generic_files(mut tc types.TypeChecker, a &flat.FlatAst, include_imports bool, diagnostic_root string) {
	if !include_imports {
		return
	}
	for i, node in a.nodes {
		if i < a.user_code_start || node.kind != .file || node.value.len == 0 {
			continue
		}
		if path_is_in_dir(node.value, diagnostic_root) {
			tc.diagnostic_files['generic:' + node.value] = true
		}
	}
}

fn path_is_in_dir(path string, dir string) bool {
	real_path := os.real_path(path)
	real_dir := os.real_path(dir)
	return real_path == real_dir || real_path.starts_with(real_dir + os.path_separator)
}

// skipped_backend_modules lists the importable backend module names that the current
// configuration excludes (driven by the same `skip_*` defines that gate the dispatch in
// main()). The arm64 backend is the only consumer of the SSA pipeline, so skipping it also
// skips v3.ssa and v3.ssa.optimize.
fn skipped_backend_modules(prefs &pref.Preferences) []string {
	mut skipped := []string{}
	if 'skip_arm64' in prefs.user_defines {
		skipped << 'v3.gen.arm64'
		skipped << 'v3.ssa'
		skipped << 'v3.ssa.optimize'
	}
	if 'skip_wasm' in prefs.user_defines {
		skipped << 'v3.gen.wasm'
	}
	if 'skip_eval' in prefs.user_defines {
		skipped << 'v3.eval'
	}
	return skipped
}

struct ImplicitImportScan {
mut:
	node_idx         int
	needs_sync       bool
	has_sync         bool
	needs_embed      bool
	has_embed_import bool
}

fn seed_implicit_imports(mut a flat.FlatAst) {
	start := a.nodes.len
	mut scan := ImplicitImportScan{}
	scan_implicit_imports(a, a.nodes.len, mut scan)
	if scan.needs_sync && !scan.has_sync {
		a.add_node(sync_import_node())
	}
	if scan.needs_embed && !scan.has_embed_import {
		a.add_node(embed_file_import_node())
	}
	a.intern_node_texts_from(start)
}

fn sync_import_node() flat.Node {
	return flat.Node{
		kind:  .import_decl
		value: 'sync'
		typ:   'sync'
	}
}

fn embed_file_import_node() flat.Node {
	return flat.Node{
		kind:  .import_decl
		value: 'v.embed_file'
		typ:   'embed_file'
	}
}

fn seed_cached_builtin_bundle_imports(mut a flat.FlatAst, enabled bool, builtin_dir string) {
	if !enabled {
		return
	}
	// Put cache warm-up imports in a private synthetic file/module scope. Without
	// these boundaries the checker assigns them to the last parsed user file.
	start := a.nodes.len
	a.nodes << flat.Node{
		kind:  .file
		value: cache_bundle_import_file(builtin_dir)
	}
	a.nodes << flat.Node{
		kind:  .module_decl
		value: 'builtin'
	}
	for import_path in modulecache.builtin_bundle_imports {
		a.nodes << flat.Node{
			kind:  .import_decl
			value: import_path
			typ:   import_path.all_after_last('.')
		}
	}
	a.intern_node_texts_from(start)
}

fn cache_bundle_import_file(builtin_dir string) string {
	return os.join_path_single(builtin_dir, cache_bundle_import_file_name)
}

fn scan_implicit_imports(a &flat.FlatAst, end_node int, mut scan ImplicitImportScan) {
	for i in scan.node_idx .. end_node {
		node := a.nodes[i]
		if node.kind == .import_decl {
			if node.value == 'sync' {
				scan.has_sync = true
			} else if node.value == 'v.embed_file' {
				scan.has_embed_import = true
			}
		}
		if !scan.needs_sync {
			if node.kind == .lock_expr
				|| (node.kind == .field_decl && type_text_is_shared(node.typ))
				|| (node.kind == .struct_init && node.value.starts_with('chan '))
				|| (node.kind == .infix && node.op == .arrow)
				|| (node.kind == .prefix && node.op == .arrow)
				|| (node.typ.len > 0 && type_text_is_channel(node.typ)) {
				scan.needs_sync = true
			}
		}
		if !scan.needs_embed && node.kind == .struct_init
			&& node.value == 'embed_file.EmbedFileData' {
			scan.needs_embed = true
		}
	}
	scan.node_idx = end_node
}

fn type_text_is_channel(typ string) bool {
	mut clean := typ.trim_space()
	for {
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('mut ') {
			clean = clean[4..].trim_space()
			continue
		}
		break
	}
	return clean.starts_with('chan ') || clean == 'chan'
}

fn type_text_is_shared(raw string) bool {
	return raw.trim_space().starts_with('shared ')
}

// SyntheticInsertion records a childless synthetic import node to splice into the
// flat AST before an original-array node index.
struct SyntheticInsertion {
	pos  int // original (pre-insertion) node index to insert before
	node flat.Node
}

// insert_synthetic_imports rebuilds a.nodes with each synthetic import spliced in
// before its recorded original-array position, so the next resolver pass scans a
// module's synthetic import right after that module's own region — in the same
// order serial one-module-at-a-time resolution appended and scanned it, before
// the later wave modules were parsed. Every absolute node index is remapped to
// the shifted layout: an original index j moves right by the number of insertions
// at positions <= j. The synthetic nodes are childless, so a.children keeps its
// length and only its stored NodeIds shift. insertions must be sorted ascending
// by pos (equal positions keep insertion order); the boundary loop produces them
// in strictly increasing region order.
fn insert_synthetic_imports(mut a flat.FlatAst, insertions []SyntheticInsertion) {
	if insertions.len == 0 {
		return
	}
	old_len := a.nodes.len
	mut new_nodes := []flat.Node{cap: old_len + insertions.len}
	mut ins_idx := 0
	for i in 0 .. old_len {
		for ins_idx < insertions.len && insertions[ins_idx].pos == i {
			new_nodes << canonical_node_texts(mut a, insertions[ins_idx].node)
			ins_idx++
		}
		new_nodes << a.nodes[i]
	}
	// Insertions at pos == old_len append at the very end (the last wave module's
	// region ends at the array tail).
	for ins_idx < insertions.len {
		new_nodes << canonical_node_texts(mut a, insertions[ins_idx].node)
		ins_idx++
	}
	a.nodes = new_nodes
	for k in 0 .. a.children.len {
		cid := int(a.children[k])
		if cid >= 0 {
			a.children[k] = flat.NodeId(cid + synthetic_index_shift(insertions, cid))
		}
	}
	a.user_code_start += synthetic_index_shift(insertions, a.user_code_start)
}

// synthetic_index_shift returns how far an original node index moves after the
// insertions: the count of insertions whose position is at or before it.
fn synthetic_index_shift(insertions []SyntheticInsertion, idx int) int {
	mut shift := 0
	for ins in insertions {
		if ins.pos <= idx {
			shift++
		} else {
			break
		}
	}
	return shift
}

// resolve_imports resolves resolve imports information for v3 entry point.
fn resolve_imports(mut a flat.FlatAst, mut p parser.Parser, prefs &pref.Preferences, initial_files []string, allow_parallel bool, mut cache_state V3ModuleCacheState) bool {
	mut parsed_modules := map[string]bool{}
	parsed_modules['builtin'] = true
	parsed_modules['main'] = true
	seed_initial_modules(a, initial_files, mut parsed_modules)

	// Backend modules excluded by the active configuration are never parsed: their
	// dispatch in main() is gated out by the matching `$if !skip_* ?`, so nothing
	// references their symbols. Pre-seeding parsed_modules makes the loop below treat
	// them as already handled, so neither v3.v's top-level imports nor any transitive
	// import pulls them in. Skipping the arm64 group (v3.gen.arm64 + the v3.ssa SSA
	// pipeline) and the wasm/eval backends avoids ~30k lines of work when self-hosting.
	for skipped in skipped_backend_modules(prefs) {
		parsed_modules[skipped] = true
	}

	mut first_file := ''
	if initial_files.len > 0 {
		first_file = initial_files[0]
	}
	project_root := project_root_for_files(initial_files)
	mut parsed_module_identities := map[string]string{}
	mut parsed_identity_dirs := map[string]string{}
	mut identity_source_paths := map[string]string{}
	mut identity_source_dirs := map[string]string{}
	mut forced_full_module_paths := map[string]bool{}
	mut module_path_cache := map[string]string{}
	mut module_identity_cache := map[string]string{}
	mut cached_header_source_contexts := map[string]string{}
	bundle_import_file := cache_bundle_import_file(prefs.get_vlib_module_path('builtin'))
	if builtin_sources := cache_state.module_sources['builtin'] {
		if builtin_sources.len > 0 {
			builtin_header := cache_state.manager.entry('builtin', builtin_sources).header
			cached_header_source_contexts[builtin_header] = builtin_sources[0]
		}
	}

	mut was_parallel := false
	mut cur_file := first_file
	mut cur_module := 'main'
	mut node_idx := 0
	// The implicit sync/embed_file seeds are global-once: the serial loop added
	// each at the first module that needed it and never again. These flags carry
	// that "already seeded" state across module boundaries and waves. Within a
	// wave the synthetic nodes are only spliced in after every boundary has been
	// checked, so a later module's bounded already-imported scan cannot yet see an
	// earlier module's pending seed; the flags stand in for it.
	mut implicit_imports := ImplicitImportScan{}
	scan_implicit_imports(a, a.nodes.len, mut implicit_imports)
	mut synthetic_sync_added := implicit_imports.has_sync
	mut synthetic_embed_file_added := implicit_imports.has_embed_import
	for {
		// Decide short-name collisions for the whole visible import wave before
		// mutating any import node or parsing either module. This qualifies both
		// sides of `a.tast`/`b.tast`, avoiding an order-dependent state where the
		// first module is called `tast` and that semantic name is later mistaken
		// for the source alias of the second import.
		mut scan_file := cur_file
		for scan_idx in node_idx .. a.nodes.len {
			scan_node := a.nodes[scan_idx]
			if scan_node.kind == .file && scan_node.value.len > 0 {
				scan_file = scan_node.value
				continue
			}
			if scan_node.kind != .import_decl || !scan_node.value.contains('.') {
				continue
			}
			scan_path := scan_node.value
			scan_importing_file := if scan_file.len > 0 { scan_file } else { first_file }
			scan_dir := resolve_project_or_pref_module_path_cached(prefs, scan_path,
				scan_importing_file, project_root, mut module_path_cache)
			scan_identity := import_module_identity_cached(prefs, scan_path, scan_importing_file,
				project_root, scan_dir, mut module_path_cache, mut module_identity_cache)
			if owner_path := identity_source_paths[scan_identity] {
				owner_dir := identity_source_dirs[scan_identity] or { '' }
				if owner_path != scan_path && owner_dir.len > 0 && scan_dir.len > 0
					&& os.is_dir(owner_dir) && os.is_dir(scan_dir)
					&& os.real_path(owner_dir) != os.real_path(scan_dir) {
					forced_full_module_paths[owner_path] = true
					forced_full_module_paths[scan_path] = true
				}
			} else {
				identity_source_paths[scan_identity] = scan_path
				identity_source_dirs[scan_identity] = scan_dir
			}
		}
		// Collect one wave: every not-yet-parsed module imported by the nodes
		// scanned so far. Parsing appends at the end of the node array and the
		// scan proceeds in node order, so batching a wave and appending its
		// modules in discovery order reproduces the breadth-first module layout
		// the previous parse-one-module-inline loop produced — while giving the
		// parallel parser whole waves of files to split across threads.
		mut wave_files := []string{}
		mut wave_canon := []string{}
		mut wave_module_file_ends := []int{}
		for node_idx < a.nodes.len {
			node := a.nodes[node_idx]
			if node.kind == .file && node.value.len > 0 {
				cur_file = node.value
				cur_module = ''
				node_idx++
				continue
			}
			if node.kind == .module_decl {
				cur_module = node.value
				node_idx++
				continue
			}
			if node.kind != .import_decl {
				node_idx++
				continue
			}
			mod_name := node.value
			is_bundle_warmup_import := cur_module == 'builtin' && cur_file == bundle_import_file
				&& mod_name in modulecache.builtin_bundle_imports
			if is_bundle_warmup_import
				&& (mod_name in parsed_module_identities || mod_name in parsed_modules)
				&& !module_is_builtin_bundle(cache_state, mod_name) {
				// A project module may shadow an optional builtin-bundle import (for
				// example, a top-level `hash` module). Keep the project module as its
				// own cache object and omit the shadowed warmup import from this bundle.
				node_idx++
				continue
			}
			if is_bundle_warmup_import && cache_state.bundle_valid {
				warmup_dir := prefs.get_vlib_module_path(mod_name)
				warmup_files := pref.get_v_files_from_dir_for_target(warmup_dir,
					prefs.user_defines, prefs.target)
				if cache_state.manager.valid_header(mod_name, warmup_files) == none {
					// The cached bundle may have been built while a project module
					// shadowed this optional warmup import. An actual user import was
					// already resolved above; do not rebuild just to warm an unused one.
					node_idx++
					continue
				}
			}
			if module_identity := parsed_module_identities[mod_name] {
				if module_identity.len > 0 {
					set_node_value_canonical(mut a, node_idx, module_identity)
				}
				record_cache_module_dependency(mut cache_state, cur_module, module_identity)
				node_idx++
				continue
			}
			if mod_name in parsed_modules {
				record_cache_module_dependency(mut cache_state, cur_module, mod_name)
				node_idx++
				continue
			}

			importing_file := cached_header_source_contexts[cur_file] or {
				if cur_file.len > 0 { cur_file } else { first_file }
			}
			mod_dir := if is_bundle_warmup_import {
				prefs.get_vlib_module_path(mod_name)
			} else {
				resolve_project_or_pref_module_path_cached(prefs, mod_name, importing_file,
					project_root, mut module_path_cache)
			}
			mut module_identity := import_module_identity_cached(prefs, mod_name, importing_file,
				project_root, mod_dir, mut module_path_cache, mut module_identity_cache)
			if forced_full_module_paths[mod_name] {
				module_identity = mod_name
			}
			// Two distinct dotted imports can legitimately declare the same short
			// module name (for example `a.http` and `b.http`). Keep the first short
			// identity for compatibility, but qualify every colliding directory by
			// its import path so it is parsed and indexed as a separate module.
			if owner_dir := parsed_identity_dirs[module_identity] {
				if mod_dir.len > 0 && owner_dir.len > 0 && os.is_dir(mod_dir)
					&& os.real_path(owner_dir) != os.real_path(mod_dir) {
					module_identity = mod_name
				}
			}
			if module_identity.len > 0 {
				set_node_value_canonical(mut a, node_idx, module_identity)
			}
			cache_module := if module_identity.len > 0 { module_identity } else { mod_name }
			record_cache_module_dependency(mut cache_state, cur_module, cache_module)
			mod_dir_exists := mod_dir.len > 0 && os.is_dir(mod_dir)
			if mod_name in parsed_modules || (mod_dir_exists && module_identity in parsed_modules) {
				node_idx++
				continue
			}
			parsed_modules[mod_name] = true
			if mod_dir_exists && module_identity.len > 0 {
				parsed_modules[module_identity] = true
				parsed_identity_dirs[module_identity] = mod_dir
			}
			parsed_module_identities[mod_name] = if module_identity.len > 0 {
				module_identity
			} else {
				mod_name
			}

			if mod_dir_exists {
				mod_files := pref.get_v_files_from_dir_for_target(mod_dir, prefs.user_defines,
					prefs.target)
				if cache_module !in cache_state.module_import_paths {
					cache_state.module_import_paths[cache_module] = mod_name
				}
				cache_state.module_sources[cache_module] = mod_files
				mut parse_files := mod_files.clone()
				is_builtin_bundle := module_is_builtin_bundle(cache_state, cache_module)
				if is_builtin_bundle {
					if cache_state.bundle_valid {
						if header := cache_state.manager.valid_header(cache_module, mod_files) {
							if !modulecache.header_needs_source(header) {
								parse_files = [header.header]
								if mod_files.len > 0 {
									cached_header_source_contexts[header.header] = mod_files[0]
								}
							} else {
								cache_state.source_body_modules[cache_module] = true
							}
						} else {
							// A bundle is rebuilt as a unit. If one interface is stale,
							// restart with all bundle source bodies for the replacement object.
							if !cache_state.force_source {
								os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
								restart_v3_after_cache_invalidation()
							}
							cache_state.bundle_valid = false
							cache_state.objects.delete('builtin')
							cache_state.parsed_from_source[cache_module] = true
							cache_state.source_body_modules[cache_module] = true
						}
					} else {
						cache_state.parsed_from_source[cache_module] = true
						cache_state.source_body_modules[cache_module] = true
					}
				} else if !cache_state.force_source {
					if cached := cache_state.manager.valid_entry(cache_module, mod_files) {
						if !modulecache.header_needs_source(cached) {
							parse_files = [cached.header]
							if mod_files.len > 0 {
								cached_header_source_contexts[cached.header] = mod_files[0]
							}
						} else {
							cache_state.source_body_modules[cache_module] = true
						}
						cache_state.objects[cache_module] = cached.object
					} else {
						cache_state.parsed_from_source[cache_module] = true
						cache_state.source_body_modules[cache_module] = true
					}
				} else {
					cache_state.parsed_from_source[cache_module] = true
					cache_state.source_body_modules[cache_module] = true
				}
				canon := if module_identity == mod_name { mod_name } else { '' }
				for mf in parse_files {
					wave_files << mf
					wave_canon << canon
				}
				wave_module_file_ends << wave_files.len
			}
			node_idx++
		}
		if wave_files.len == 0 {
			break
		}
		starts, wave_parallel := p.parse_files_dispatch(wave_files, allow_parallel)
		was_parallel = was_parallel || wave_parallel
		wave_end_nodes := a.nodes.len
		for i, canon in wave_canon {
			if canon.len == 0 {
				continue
			}
			end_node := if i + 1 < starts.len { starts[i + 1] } else { wave_end_nodes }
			canonicalize_imported_module_name(mut a, starts[i], end_node, canon)
		}
		// Re-check the implicit imports at each module boundary, in parse order,
		// with each scan bounded to the nodes that existed at that boundary. This
		// fires the seeds for exactly the module the serial loop's after-every-
		// module check would have fired them for. The synthetic nodes are then
		// spliced in right after their triggering module's region (region_end),
		// not at the wave tail, so the next pass scans an earlier module's
		// synthetic import before the later wave modules' imports — matching serial
		// order — and the `.file` marker preceding each synthetic is its own
		// module's last file, so module-path resolution uses the right context.
		mut insertions := []SyntheticInsertion{}
		mut module_start := 0
		for module_file_end in wave_module_file_ends {
			if module_file_end == module_start {
				continue
			}
			region_end := if module_file_end < starts.len {
				starts[module_file_end]
			} else {
				wave_end_nodes
			}
			scan_implicit_imports(a, region_end, mut implicit_imports)
			if !synthetic_sync_added && implicit_imports.needs_sync && !implicit_imports.has_sync {
				insertions << SyntheticInsertion{
					pos:  region_end
					node: sync_import_node()
				}
				synthetic_sync_added = true
			}
			if !synthetic_embed_file_added && implicit_imports.needs_embed
				&& !implicit_imports.has_embed_import {
				insertions << SyntheticInsertion{
					pos:  region_end
					node: embed_file_import_node()
				}
				synthetic_embed_file_added = true
			}
			module_start = module_file_end
		}
		insert_synthetic_imports(mut a, insertions)
		implicit_imports.node_idx += insertions.len
	}
	return was_parallel
}

fn record_cache_module_dependency(mut state V3ModuleCacheState, owner string, dependency string) {
	if owner.len == 0 || dependency.len == 0 || owner == dependency {
		return
	}
	mut dependencies := state.module_dependencies[owner]
	if dependency !in dependencies {
		dependencies << dependency
		state.module_dependencies[owner] = dependencies
	}
}

fn seed_initial_modules(a &flat.FlatAst, initial_files []string, mut parsed_modules map[string]bool) {
	mut selected_files := map[string]bool{}
	for file in initial_files {
		selected_files[file] = true
		selected_files[os.real_path(file)] = true
	}
	for file_idx, file_node in a.nodes {
		if file_idx < a.user_code_start || file_node.kind != .file || file_node.value.len == 0 {
			continue
		}
		if !selected_files[file_node.value] && !selected_files[os.real_path(file_node.value)] {
			continue
		}
		module_name := test_file_module_name(a, file_node)
		if module_name.len > 0 {
			parsed_modules[module_name] = true
		}
	}
}

fn canonicalize_imported_module_name(mut a flat.FlatAst, first_node int, end_node int, import_path string) {
	if import_path.len == 0 {
		return
	}
	short_name := import_path.all_after_last('.')
	_, canonical_path := a.intern_text(import_path)
	for i in first_node .. end_node {
		if a.nodes[i].kind == .module_decl && a.nodes[i].value == short_name {
			a.nodes[i].value = canonical_path
		}
	}
}

fn set_node_value_canonical(mut a flat.FlatAst, idx int, value string) {
	_, canonical := a.intern_text(value)
	a.nodes[idx].value = canonical
}

fn canonical_node_texts(mut a flat.FlatAst, node flat.Node) flat.Node {
	mut canonical := node
	_, canonical.value = a.intern_text(node.value)
	_, canonical.typ = a.intern_text(node.typ)
	return canonical
}

fn source_file_line_count(paths []string) int {
	mut lines := 0
	for path in paths {
		source := os.read_file(path) or { continue }
		if source.len == 0 {
			continue
		}
		lines += source.count('\n')
		if source[source.len - 1] != `\n` {
			lines++
		}
	}
	return lines
}

fn import_module_identity_cached(prefs &pref.Preferences, import_path string, importing_file string, project_root string, import_dir string, mut path_cache map[string]string, mut identity_cache map[string]string) string {
	key := '${importing_file}\n${import_path}\n${import_dir}'
	if identity := identity_cache[key] {
		return identity
	}
	identity := import_module_identity_with_path_cache(prefs, import_path, importing_file,
		project_root, import_dir, mut path_cache)
	identity_cache[key] = identity
	return identity
}

fn import_module_identity_with_path_cache(prefs &pref.Preferences, import_path string, importing_file string, project_root string, import_dir string, mut path_cache map[string]string) string {
	if alias_identity := aliased_import_module_identity(prefs, import_path, import_dir) {
		return alias_identity
	}
	if !import_path.contains('.') {
		return import_path
	}
	short_name := import_path.all_after_last('.')
	if import_dir.len > 0 {
		module_root := module_root_for_import_dir(import_path, import_dir)
		short_sibling_dir := os.join_path_single(module_root, short_name)
		if os.is_dir(short_sibling_dir)
			&& os.real_path(short_sibling_dir) != os.real_path(import_dir) {
			return import_path
		}
	}
	if project_root.len > 0 && import_dir.len > 0 {
		short_project_dir := os.join_path_single(project_root, short_name)
		if os.is_dir(short_project_dir)
			&& os.real_path(short_project_dir) != os.real_path(import_dir) {
			return import_path
		}
	}
	short_dir := resolve_project_or_pref_module_path_cached(prefs, short_name, importing_file,
		project_root, mut path_cache)
	if short_dir.len > 0 && import_dir.len > 0 && os.is_dir(short_dir)
		&& os.real_path(short_dir) != os.real_path(import_dir) {
		return import_path
	}
	return short_name
}

fn aliased_import_module_identity(prefs &pref.Preferences, import_path string, import_dir string) ?string {
	if import_path.len == 0 || import_dir.len == 0 || !os.is_dir(import_dir) {
		return none
	}
	module_root := module_root_for_import_dir(import_path, import_dir)
	requested_dir := os.join_path_single(module_root, import_path.replace('.', os.path_separator))
	if os.real_path(requested_dir) == os.real_path(import_dir) {
		return none
	}
	for file in pref.get_v_files_from_dir_for_target(import_dir, prefs.user_defines, prefs.target) {
		module_name := declared_module_in_file(file)
		if module_name.len > 0 {
			return module_name
		}
	}
	return none
}

fn module_root_for_import_dir(import_path string, import_dir string) string {
	mut root := import_dir
	for _ in import_path.split('.') {
		parent := os.dir(root)
		if parent == root {
			return root
		}
		root = parent
	}
	return root
}

fn resolve_project_or_pref_module_path_cached(prefs &pref.Preferences, mod_name string, importing_file string, project_root string, mut cache map[string]string) string {
	key := '${importing_file}\n${mod_name}'
	if path := cache[key] {
		return path
	}
	path := resolve_project_or_pref_module_path(prefs, mod_name, importing_file, project_root)
	cache[key] = path
	return path
}

fn resolve_project_or_pref_module_path(prefs &pref.Preferences, mod_name string, importing_file string, project_root string) string {
	if importing_file.len > 0 {
		importer_dir := os.dir(importing_file)
		if alias_path := pref.resolve_module_alias_path(importer_dir, mod_name) {
			return alias_path
		}
		local_modules_root := os.join_path_single(importer_dir, 'modules')
		if alias_path := pref.resolve_module_alias_path(local_modules_root, mod_name) {
			return alias_path
		}
		local_modules_path := os.join_path_single(local_modules_root, mod_name.replace('.',
			os.path_separator))
		if os.is_dir(local_modules_path) {
			return local_modules_path
		}
	}
	if project_root.len > 0 {
		if alias_path := pref.resolve_module_alias_path(project_root, mod_name) {
			return alias_path
		}
		project_path := os.join_path_single(project_root, mod_name.replace('.', os.path_separator))
		if os.is_dir(project_path) {
			return project_path
		}
	}
	return prefs.get_module_path(mod_name, importing_file)
}
