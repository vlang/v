module driver

import os
import runtime
import strconv
import strings
import time
import crypto.sha256
import v3.ansi
import v3.bench
import v3.cmdexec
import v3.errors as v3errors
import v3.flat
import v3.fixturetest
import v3.gen.c as cgen
import v3.gen.c.naming
import v3.markused
import v3.modulecache
import v3.parser
import v3.pref
import v3.tempname
import v3.token as v3token
import v3.transform
import v3.types
import v3.workers
import v.build_constraint
import v.vmod

// vfmt off
$if !skip_fastc ? {
	import v3.gen.fastc
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
	import v3.gen.wasm as wasmgen
}
// vfmt on

const cache_bundle_import_file_name = '.v3_cache_bundle_imports.vh'
const macos_v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const macos_v3_no_fallback_env = 'V_MACOS_V3_NO_FALLBACK'
const macos_v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
const macos_v3_internal_quiet_flag = '-macos-v3-internal-quiet'
const macos_v3_inline_asm_diagnostic = 'inline assembly is not supported by the selected V3 backend'
const macos_v3_inline_asm_fallback = 'inline_asm'
const macos_v3_compiler_error_fallback = 'compiler_error'
const macos_v3_c_error_fallback = 'c_compilation_error'
const macos_v3_c_error_compiler_file = 'compiler'
const macos_v3_c_error_output_file = 'output'
const macos_v3_c_error_source_name_file = 'source_name'
const macos_v3_c_error_v_sources_file = 'v_sources'
const macos_v3_c_error_v_source_digests_file = 'v_source_digests'
const v3_fallback_native_input_prefix = '@native-input:'
const v3_fallback_native_manifest_key = '@native-input-manifest:v1'
const v3_fallback_native_manifest_value = 'v3-native-input-manifest-v1'

fn configure_selfhost_parallelism(building_v bool) {
	if !building_v || os.getenv('VJOBS') != '' || os.getenv('V3_NO_SELFHOST_JOB_OVERCOMMIT') != '' {
		return
	}
	jobs := runtime.nr_jobs()
	if jobs < 2 || jobs >= 12 {
		return
	}
	overcommitted := int_min(12, jobs + jobs / 2)
	if overcommitted > jobs {
		os.setenv('VJOBS', overcommitted.str(), true)
	}
}

const embedded_parallel_transform_node_limit = 10_000_000
const scoped_serial_user_check_node_threshold = 1_000_000
const scoped_serial_user_transform_node_threshold = 1_000_000
const scoped_serial_user_cgen_node_threshold = 2_000_000
const scoped_linux_user_job_limit = 4
const scoped_transform_signature_headroom = 2048
const v3_vvmrc_file_name = '.vvmrc'
const v3_vvmrc_skip_env = 'V_SKIP_VVMRC'
const v3_vvmrc_stop_paths = ['.git', '.hg', '.svn', '.v.mod.stop']
const v3_crun_build_identity_env = 'V3_CRUN_BUILD_IDENTITY'
const v3_internal_restart_env = 'V3_INTERNAL_RESTART'
const v3_embedded_env = 'V_MACOS_V3_EMBEDDED'

struct V3ModuleCacheState {
	manager             modulecache.Manager
	bundle_sources      []string
	bundle_source_paths map[string]bool
mut:
	force_source              bool
	bundle_valid              bool
	module_sources            map[string][]string
	module_import_paths       map[string]string
	module_dependencies       map[string][]string
	module_external_inputs    map[string][]string
	module_native_roots       map[string][]string
	native_root_contexts      map[string][]string
	native_root_owners        map[string]string
	external_input_signatures map[string]string
	external_input_digests    map[string]string
	external_resolution_dirs  []string
	external_missing_paths    []string
	external_inputs_ready     bool
	external_inputs_complete  bool
	dependency_metadata       map[string]string
	cached_source_digests     map[string]string
	fallback_required_modules map[string]bool
	fallback_warmup_modules   map[string]bool
	parsed_from_source        map[string]bool
	source_body_modules       map[string]bool
	native_source_modules     map[string]bool
	native_type_declarations  map[string]string
	native_declared_functions map[string]map[string]bool
	objects                   map[string]string
	headers                   map[string]string
}

struct V3ParseTiming {
mut:
	header_us       i64
	source_us       i64
	header_parallel bool
	source_parallel bool
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
	diagnostics              []V3CachedTypeDiagnostic
}

struct V3CachedTypeDiagnostic {
	file            string
	msg             string
	severity        string
	node            int
	offset          int
	end             int
	reported_column int
	details         []string
}

struct V3ExternalCachePath {
	module_name string
	path        string
}

struct V3ExternalNativeRoot {
	module_name string
	path        string
	index       int
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

// tcc_atomic_arg returns the atomic-support argument for a tcc link,
// preferring a cached precompiled object: assembling atomic.S inside every
// link costs ~28ms, while the object only changes when the source does.
fn tcc_atomic_arg(prefs &pref.Preferences, tcc_path string, tcc_includes string) string {
	atomic_s := tcc_atomic_s_arg(prefs)
	if atomic_s.len == 0 {
		return ''
	}
	wrapv_flag := c_wrapv_flag(prefs.normalized_target_os())
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	signature := modulecache.file_signature(atomic_s)
	if signature.len == 0 {
		return atomic_s
	}
	wrapv_suffix := if wrapv_flag.len > 0 { '_wrapv' } else { '' }
	object_path := os.join_path(cache_dir, 'atomic_${naming.sanitize(signature)}${wrapv_suffix}.o')
	if os.is_file(object_path) {
		return object_path
	}
	os.mkdir_all(cache_dir) or { return atomic_s }
	build_path := '${object_path}.tmp.${os.getpid()}'
	mut args := ['-std=gnu11', tcc_includes]
	if wrapv_flag.len > 0 {
		args << wrapv_flag
	}
	args << ['-c', atomic_s, '-o', build_path]
	result := cmdexec.run(tcc_path, args)
	if result.exit_code != 0 || !os.is_file(build_path) {
		os.rm(build_path) or {}
		return atomic_s
	}
	// Atomic rename: concurrent builds publishing the same signature converge.
	os.mv(build_path, object_path) or {
		os.rm(build_path) or {}
		return atomic_s
	}
	return object_path
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

fn add_c_language_runtime_link_flags(mut prepared []string, original []string, language string, target pref.Target) {
	if language in ['c++', 'objective-c++'] {
		cpp_runtime := cpp_runtime_link_flag(target)
		if cpp_runtime !in original && cpp_runtime !in prepared {
			prepared << cpp_runtime
		}
	}
	if language in ['objective-c', 'objective-c++'] && '-lobjc' !in original && '-lobjc' !in prepared {
		prepared << '-lobjc'
	}
}

fn prepare_c_flags_for_link(flags []string, environment_c_flags []string, optimization_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) ![]string {
	// Nothing to cache: without object-file or native-source flags the link
	// plan adds no value, and preparing it costs a compiler-identity probe
	// (subprocess) plus plan-file signatures on every build.
	mut has_cacheable_flag := false
	for flag in flags {
		clean := flag.trim_space()
		if c_flag_is_object_file(clean) || clean.ends_with('.mm') {
			has_cacheable_flag = true
			break
		}
	}
	if !has_cacheable_flag {
		mut passthrough := flags.clone()
		if c_link_flags_use_cpp_language(passthrough) {
			add_c_language_runtime_link_flags(mut passthrough, flags, 'c++', target)
		}
		if c_link_flags_use_objective_c_language(passthrough) {
			add_c_language_runtime_link_flags(mut passthrough, flags, 'objective-c', target)
		}
		return passthrough
	}
	mut support_flags := environment_c_flags.clone()
	support_flags << optimization_flags
	support_flags << c_object_compile_support_flags(flags)
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	plan_path := c_link_plan_path(cache_dir, flags, support_flags, c99, pic_flag, target_args, target, c_compiler, mut stats)
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
			adjacent_language := if !os.exists(clean) {
				if source_file := c_source_from_object_file(clean) {
					c_source_language(source_file, active_language)
				} else {
					''
				}
			} else {
				''
			}
			object_path := ensure_c_object_file(clean, active_language, support_flags, c99, pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)!
			append_c_link_object(mut prepared, object_path, active_language)
			add_c_language_runtime_link_flags(mut prepared, flags, adjacent_language, target)
		} else if clean.ends_with('.mm') {
			stats.requests++
			language := c_source_language(clean, active_language)
			object_path := ensure_c_source_object(clean, active_language, support_flags, c99, pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)!
			append_c_link_object(mut prepared, object_path, active_language)
			if c_generated_native_source_context(clean, uncached_dir) {
				os.rm(clean) or {}
			}
			add_c_language_runtime_link_flags(mut prepared, flags, language, target)
		} else if c_flag_is_c_source_file(clean) {
			prepared << flag
		} else {
			prepared << flag
		}
		i++
	}
	if c_link_flags_use_cpp_language(prepared) {
		add_c_language_runtime_link_flags(mut prepared, flags, 'c++', target)
	}
	if c_link_flags_use_objective_c_language(prepared) {
		add_c_language_runtime_link_flags(mut prepared, flags, 'objective-c', target)
	}
	if stats.dependency_scan_fallbacks == 0 && stats.temporary_objects.len == 0 {
		write_c_link_plan(plan_path, prepared, stats) or {}
		stats.link_plan_signature = modulecache.file_signature(plan_path)
	}
	return prepared
}

fn c_link_plan_path(cache_dir string, flags []string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, compiler string, mut stats CObjectCacheStats) string {
	compiler_path, compiler_version := c_object_compiler_identity(compiler, mut stats)
	mut hash := u64(1469598103934665603)
	for identity in ['v3-c-link-plan-v3', os.getwd(), flags.join('\x00'), support_flags.join('\x00'),
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
	if lines.len < 5 || lines[0] != 'format=v3-c-link-plan-v3' {
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
	out.writeln('format=v3-c-link-plan-v3')
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
	temp_path := '${plan_path}.tmp.${tempname.unique_token()}'
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

fn c_link_flags_use_objective_c_language(flags []string) bool {
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
		if c_flag_is_c_source_file(clean) || c_flag_is_existing_file(clean) {
			if language in ['objective-c', 'objective-c++'] {
				return true
			}
			if language in ['', 'none'] && (clean.ends_with('.m') || clean.ends_with('.mm')) {
				return true
			}
		}
		i++
	}
	return false
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
		if part in ['-l', '-L', '-Xlinker', '-framework', '-weak_framework', '-weak_library',
			'-force_load'] {
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
	mut language := ''
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean == '-x' {
			language = if i + 1 < flags.len { flags[i + 1].trim_space() } else { '' }
			i += 2
			continue
		}
		if clean in ['-l', '-L', '-F', '-framework', '-weak_framework', '-weak_library', '-Xlinker',
			'-force_load'] {
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
			|| (c_flag_is_existing_file(clean) && !c_flag_is_c_source_file(clean)
				&& language != 'c') {
			link_flags << flag
		}
		i++
	}
	return link_flags
}

fn c_dylib_named_static_archive_inputs(link_flags []string) []string {
	mut library_dirs := []string{}
	mut library_names := []string{}
	mut i := 0
	for i < link_flags.len {
		clean := link_flags[i].trim(' \t\r\n"\'')
		if clean == '-L' {
			if i + 1 < link_flags.len {
				library_dirs << link_flags[i + 1].trim(' \t\r\n"\'')
			}
			i += 2
			continue
		}
		if clean.starts_with('-L') && clean.len > 2 {
			library_dirs << clean[2..]
			i++
			continue
		}
		if clean == '-l' {
			if i + 1 < link_flags.len {
				library_names << link_flags[i + 1].trim(' \t\r\n"\'')
			}
			i += 2
			continue
		}
		if clean.starts_with('-l') && clean.len > 2 {
			library_names << clean[2..]
		}
		i++
	}
	mut archives := map[string]bool{}
	for name in library_names {
		archive_name := if name.starts_with(':') { name[1..] } else { 'lib${name}.a' }
		if archive_name.len == 0 {
			continue
		}
		for dir in library_dirs {
			candidate := os.join_path(dir, archive_name)
			if os.is_file(candidate) {
				archives[os.real_path(candidate)] = true
			}
		}
	}
	mut result := archives.keys()
	result.sort()
	return result
}

fn c_dylib_force_loaded_static_archive_inputs(link_flags []string) []string {
	mut archives := map[string]bool{}
	for flag in link_flags {
		clean := flag.trim(' \t\r\n"\'')
		if !clean.starts_with('-Wl,') {
			continue
		}
		parts := clean['-Wl,'.len..].split(',')
		mut i := 0
		for i + 1 < parts.len {
			if parts[i] == '-force_load' {
				path := parts[i + 1].trim(' \t\r\n"\'')
				if path.ends_with('.a') && os.is_file(path) {
					archives[os.real_path(path)] = true
				}
				i += 2
				continue
			}
			i++
		}
	}
	mut result := archives.keys()
	result.sort()
	return result
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

fn tcc_native_c_source_flags(flags []string) []string {
	mut sources := []string{}
	mut language := ''
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim(' \t\r\n"\'')
		if clean == '-x' {
			language = if i + 1 < flags.len { flags[i + 1].trim_space() } else { '' }
			i += 2
			continue
		}
		if c_flag_consumes_next_operand(clean) || clean in ['-l', '-weak_library'] {
			i += 2
			continue
		}
		if clean.ends_with('.c') {
			sources << flag
		} else if language == 'c' && clean.len > 0 && !clean.starts_with('-') {
			// Preserve explicit language selection for extensionless inputs, then
			// reset it before the following cached dylib argument.
			sources << ['-x', 'c', flag, '-x', 'none']
		}
		i++
	}
	return sources
}

fn tcc_cached_main_source(source string, body string) string {
	// Framework headers contain Objective-C syntax that TinyCC cannot parse.
	// Their implementations and public native symbols live in the cached dylib;
	// the remaining generated program unit only needs V's C declarations.
	objc_frameworks := ['AppKit', 'AudioToolbox', 'AVFoundation', 'Cocoa', 'Foundation', 'GLKit',
		'Metal', 'MetalKit', 'QuartzCore', 'UIKit', 'WebKit', 'objc']
	mut out := strings.new_builder(source.len)
	mut declares_objc_msg_send := false
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if line.len > 0 && line[0] !in [` `, `\t`] && trimmed.ends_with(';')
			&& trimmed.contains('objc_msgSend(') {
			declares_objc_msg_send = true
			break
		}
	}
	if (source.contains('objc_msgSend') || body.contains('objc_msgSend')) && !declares_objc_msg_send {
		// objc/message.h is intentionally omitted above. Plain C program files
		// can still cast the runtime entry point declared through `C.objc_msgSend`.
		out.writeln('void* objc_msgSend(void*, void*);')
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

fn v3_program_external_input_paths(state &V3ModuleCacheState) []string {
	mut paths := map[string]bool{}
	for inputs in state.module_external_inputs.values() {
		for input in inputs {
			clean := input.trim_space()
			if os.is_file(clean) && !c_flag_token_is_link_only(clean)
				&& !c_flag_is_object_file(clean) {
				paths[os.real_path(clean)] = true
			}
		}
	}
	mut result := paths.keys()
	result.sort()
	return result
}

fn c_response_file_arg(arg string) string {
	slash := [u8(92)].bytestr()
	escaped_slash := [u8(92), 92].bytestr()
	quote := [u8(34)].bytestr()
	escaped_quote := [u8(92), 34].bytestr()
	return quote + arg.replace(slash, escaped_slash).replace(quote, escaped_quote) + quote
}

fn compile_v3_program_object(kind string, source string, source_identity string, external_inputs []string, manager &modulecache.Manager, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, target_args []string, target pref.Target, c_compiler string, mut stats CObjectCacheStats) !string {
	mut args := []string{}
	if objective_c {
		args << ['-x', 'objective-c']
	} else {
		args << ['-x', 'c']
	}
	append_v3_c_compile_mode_flags(mut args, c_standard, opt_flag, pic_flag)
	args << target_args
	args << cgen.tokenize_c_flag(warning_flags)
	args << '-Wno-int-conversion'
	args << c_object_compile_flags(generated_c_flags).filter(!c_flag_is_object_file(it))
	compiler_path, compiler_version := c_object_compiler_identity(c_compiler, mut stats)
	mut hash := u64(1469598103934665603)
	program_identity := if source_identity.len > 0 { source_identity } else { source }
	for identity in ['v3-cached-program-${kind}-v1', program_identity, compiler_path,
		compiler_version, args.join('\x00'), target.os, target.arch, target.abi, target.endian,
		target.pointer_bits.str(), target.object_format] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	for input in external_inputs {
		hash = c_hash_bytes(hash, input.bytes())
		hash = c_hash_bytes(hash, c_object_file_signature(input, false, mut stats).bytes())
	}
	key := hash.hex()
	source_path := os.join_path(manager.dir, 'program_${kind}_${key}.c')
	object_path := os.join_path(manager.dir, 'program_${kind}_${key}.o')
	if os.is_file(object_path) {
		return object_path
	}
	if source.len == 0 {
		return error('cached program ${kind} object is unavailable')
	}
	unique := tempname.unique_token()
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
		return error('failed to build cached program ${kind}:\n${result.output}')
	}
	os.mv(tmp_object, object_path) or {
		if !os.is_file(object_path) {
			return error('failed to publish cached program ${kind} ${object_path}: ${err}')
		}
	}
	os.mv(tmp_source, source_path) or {
		if !os.is_file(source_path) {
			return error('failed to publish cached program ${kind} source ${source_path}: ${err}')
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

fn compile_v3_dev_dylib(prefix_object string, cached_objects []string, resolved_c_flags []string, manager &modulecache.Manager, target_args []string, target pref.Target, c_compiler string, build_dir string, show_c_command bool, mut stats CObjectCacheStats) !string {
	link_flags := c_dylib_link_flags(resolved_c_flags)
	mut objects := [prefix_object]
	objects << cached_objects
	mut hash := u64(1469598103934665603)
	compiler_path, compiler_version := c_object_compiler_identity(c_compiler, mut stats)
	for identity in ['v3-cached-dev-dylib-v2', compiler_path, compiler_version,
		target_args.join('\x00'), link_flags.join('\x00'), target.os, target.arch, target.abi,
		target.endian, target.pointer_bits.str(), target.object_format] {
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
		clean := flag.trim(' \t\r\n"\'')
		if (clean.ends_with('.a') || (c_flag_is_object_file(clean)
			&& !clean.contains('/v3_thirdparty_objs/'))) && os.is_file(clean) {
			hash = c_hash_bytes(hash, os.real_path(clean).bytes())
			hash = c_hash_bytes(hash, c_object_file_signature(clean, true, mut stats).bytes())
		}
	}
	for archive in c_dylib_named_static_archive_inputs(link_flags) {
		hash = c_hash_bytes(hash, archive.bytes())
		hash = c_hash_bytes(hash, c_object_file_signature(archive, true, mut stats).bytes())
	}
	for archive in c_dylib_force_loaded_static_archive_inputs(link_flags) {
		hash = c_hash_bytes(hash, archive.bytes())
		hash = c_hash_bytes(hash, c_object_file_signature(archive, true, mut stats).bytes())
	}
	dylib_path := os.join_path(manager.dir, 'dev_modules_${hash.hex()}.dylib')
	if os.is_file(dylib_path) {
		return dylib_path
	}
	tmp_dylib := '${dylib_path}.tmp.${tempname.unique_token()}'
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
	if show_c_command {
		println('  > ${cmdexec.display(c_compiler, [response_arg])} (${objects.len} cached objects)')
	}
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
		os.real_path(tcc_path), v3_cache_file_identity(tcc_path), tcc_args.join('\x00')] {
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
	tmp := '${destination}.tmp.${tempname.unique_token()}'
	defer {
		os.rm(tmp) or {}
	}
	os.cp(source, tmp) or { return }
	os.mv(tmp, destination) or {}
}

fn c_flag_token_is_link_only(token string) bool {
	clean := token.trim(' \t\r\n"\'')
	if clean.starts_with('-l') || clean.starts_with('-L') || clean.starts_with('-Wl,')
		|| clean in ['-ObjC', '-all_load', '-bundle', '-dynamiclib', '-shared', '-static', '-rdynamic',
			'-pie', '-no-pie'] {
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
	return compile_cached_c_source_object(obj_path, source_file, source_language, support_flags, c99, pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)
}

fn ensure_c_source_object(source_file string, source_language string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) !string {
	if !os.exists(source_file) {
		return error('missing C source ${source_file}')
	}
	return compile_cached_c_source_object('${source_file}.o', source_file, source_language, support_flags, c99, pic_flag, target_args, target, c_compiler, uncached_dir, mut stats)
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
	wrapv_flag := c_wrapv_flag(target.os)
	if wrapv_flag.len > 0 {
		args << wrapv_flag
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
		uncached_obj := os.join_path(uncached_dir, 'dependency_scan_fallback_${tempname.unique_token()}.o')
		trace_c_object_cache('bypass', os.base(obj_path), 'dependency scan failed; using build-local object', dependencies.files.len)
		args << ['-o', uncached_obj, '-c', source_file]
		res := cmdexec.run(compiler, args)
		if res.exit_code != 0 {
			os.rm(uncached_obj) or {}
			return error('failed to build C object ${obj_path} from ${source_file}:\n${res.output}')
		}
		stats.temporary_objects << uncached_obj
		return uncached_obj
	}
	cache_key := c_object_cache_name(obj_path, compiler, args, dependencies.files, target, false, mut stats)
	cached_obj := os.join_path(cache_dir, cache_key)
	if os.exists(cached_obj) {
		stats.content_key_hits++
		trace_c_object_cache('hit', cache_key, 'compiler, target, argv, and dependency contents matched', dependencies.files.len)
		write_c_object_manifest(manifest_path, cached_obj, dependencies.files, mut stats) or {}
		return cached_obj
	}
	stats.misses++
	trace_c_object_cache('miss', cache_key, 'no published content-key entry', dependencies.files.len)
	// Snapshot the exact arguments that produced cache_key so the post-compile
	// digest is computed over the same inputs (temp_obj/-c must not perturb it).
	key_args := args.clone()
	temp_obj := '${cached_obj}.tmp.${tempname.unique_token()}'
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
	post_key := c_object_cache_name(obj_path, compiler, key_args, dependencies.files, target, true, mut stats)
	if post_key != cache_key {
		stats.input_snapshot_races++
		trace_c_object_cache('bypass', cache_key, 'inputs changed during compilation; using build-local object', dependencies.files.len)
		uncached_obj := os.join_path(uncached_dir, 'input_snapshot_race_${tempname.unique_token()}.o')
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
		compiler_version, target.os, target.arch, target.abi, target.endian,
		target.pointer_bits.str(), target.object_format, compile_args.join('\x00')] {
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
	trace_c_object_cache('hit', os.base(object_path), 'compiler, target, argv, and dependency contents matched via manifest', dependency_count)
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
	temp_path := '${manifest_path}.tmp.${tempname.unique_token()}'
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
	compiler_result := cmdexec.run(compiler, ['--version'])
	version := compiler_result.output
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
		files: [source_file]
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
		target.abi, target.endian, target.pointer_bits.str(), target.object_format,
		compile_args.join('\x00')] {
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

fn c_wrapv_flag(target_os string) string {
	return if target_os in ['macos', 'linux', 'openbsd', 'freebsd', 'windows'] {
		'-fwrapv'
	} else {
		''
	}
}

fn shared_pic_flag(is_shared bool, target_os string) string {
	if target_os == 'macos' || (is_shared && target_os != 'windows') {
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

fn ignore_run_signal(_ os.Signal) {
}

fn run_binary(bin_file string, args []string) int {
	run_path := executable_path_for_run(bin_file)
	mut process := os.new_process(run_path)
	process.set_args(args)
	mut environment := pref.macos_v3_caller_environment()
	environment.delete(macos_v3_vhash_env)
	environment.delete(macos_v3_vcurrent_hash_env)
	process.set_environment(environment)
	// `v3 run` is interactive: leave all three standard streams inherited so
	// prompts are visible immediately and the program can read the caller's stdin.
	// Ignore SIGINT and SIGQUIT while waiting so an interrupted child does not
	// terminate V3 before it can remove the implicit run executable.
	prev_int_handler := os.signal_opt(.int, ignore_run_signal) or {
		eprintln('v3: could not set SIGINT handler: ${err}')
		return 1
	}
	mut prev_quit_handler := os.SignalHandler(ignore_run_signal)
	$if !windows {
		prev_quit_handler = os.signal_opt(.quit, ignore_run_signal) or {
			os.signal_opt(.int, prev_int_handler) or {}
			eprintln('v3: could not set SIGQUIT handler: ${err}')
			return 1
		}
	}
	process.wait()
	os.signal_opt(.int, prev_int_handler) or {
		eprintln('v3: could not restore SIGINT handler: ${err}')
	}
	$if !windows {
		os.signal_opt(.quit, prev_quit_handler) or {
			eprintln('v3: could not restore SIGQUIT handler: ${err}')
		}
	}
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	return exit_code
}

fn maybe_delegate_v3_to_vvmrc(input_file string, verbose bool) {
	if os.getenv(v3_vvmrc_skip_env) != '' || input_file in ['', '-'] {
		return
	}
	vvmrc_path := find_v3_project_vvmrc(input_file)
	if vvmrc_path.len == 0 {
		return
	}
	requested_version := parse_v3_vvmrc_version(os.read_file(vvmrc_path) or { '' })
	if requested_version.len == 0
		|| normalize_v3_vvmrc_version(requested_version).to_lower_ascii() in ['latest', 'current'] {
		return
	}
	requested_exe := resolve_v3_version_executable(requested_version) or {
		eprintln('v3: warning: `${vvmrc_path}` requests V `${requested_version}`, but no matching compiler executable was found; continuing with `${os.executable()}`')
		return
	}
	if os.real_path(requested_exe) == os.real_path(os.executable()) {
		return
	}
	if verbose {
		eprintln('v3: `.vvmrc` selected V `${requested_version}` from `${vvmrc_path}` => ${requested_exe}')
	}
	mut process := os.new_process(requested_exe)
	process.set_args(os.args[1..])
	mut envs := os.environ()
	envs[v3_vvmrc_skip_env] = '1'
	envs['VEXE'] = requested_exe
	process.set_environment(envs)
	process.wait()
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	exit(exit_code)
}

fn find_v3_project_vvmrc(target_path string) string {
	mut folder := if os.is_dir(target_path) { target_path } else { os.dir(target_path) }
	if folder.len == 0 {
		folder = os.getwd()
	}
	mut current := os.real_path(folder)
	for _ in 0 .. 256 {
		vvmrc_path := os.join_path(current, v3_vvmrc_file_name)
		if os.is_file(vvmrc_path) {
			return vvmrc_path
		}
		if v3_vvmrc_stop_paths.any(os.exists(os.join_path(current, it))) {
			break
		}
		parent := os.dir(current)
		if parent.len == 0 || parent == current {
			break
		}
		current = parent
	}
	return ''
}

fn parse_v3_vvmrc_version(content string) string {
	for raw_line in content.split_into_lines() {
		line := raw_line.all_before('#').trim_space()
		if line.len > 0 {
			return line
		}
	}
	return ''
}

fn normalize_v3_vvmrc_version(version_name string) string {
	mut normalized := version_name.trim_space()
	if normalized.len > 1 && normalized[0] in [`v`, `V`] && normalized[1].is_digit() {
		normalized = normalized[1..]
	}
	return normalized
}

fn resolve_v3_version_executable(version_name string) !string {
	raw_version := version_name.trim_space()
	if raw_version.len == 0 {
		return error('empty version')
	}
	mut names := [raw_version, 'v${raw_version}']
	if raw_version.starts_with('v') && raw_version.len > 1 {
		names << raw_version[1..]
	}
	for name in names {
		if found_path := os.find_abs_path_of_executable(name) {
			return found_path
		}
	}
	normalized := normalize_v3_vvmrc_version(raw_version)
	mut paths := [os.join_path('/usr/lib/v', normalized, 'bin', 'v'),
		os.join_path('/usr/local/bin', 'v${normalized}')]
	for env_name in ['VVM_HOME', 'VVM_DIR'] {
		vvm_root := os.getenv(env_name).trim_space()
		if vvm_root.len > 0 {
			paths << os.join_path(vvm_root, normalized, 'bin', 'v')
			paths << os.join_path(vvm_root, 'versions', normalized, 'bin', 'v')
		}
	}
	for path in paths {
		if os.is_file(path) {
			return path
		}
	}
	return error('V executable for `${raw_version}` was not found')
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

fn input_is_v3_compiler_entry(input_file string) bool {
	normalized := os.real_path(input_file).replace('\\', '/').trim_right('/')
	return normalized.ends_with('/vlib/v3/v3.v')
}

fn input_is_cmd_v(input_file string) bool {
	normalized := input_file.replace('\\', '/').trim_right('/')
	return normalized in ['cmd/v', 'cmd/v/v.v'] || normalized.ends_with('/cmd/v')
		|| normalized.ends_with('/cmd/v/v.v')
}

fn input_loads_cmd_v_module(input_file string) bool {
	if os.is_dir(input_file) {
		return input_is_cmd_v(input_file)
	}
	normalized_dir := os.dir(os.real_path(input_file)).replace('\\', '/').trim_right('/')
	return normalized_dir.ends_with('/cmd/v')
}

fn input_is_v3_compiler_tree(input_file string) bool {
	real_input := os.real_path(input_file).replace('\\', '/').trim_right('/')
	return real_input.ends_with('/vlib/v3') || real_input.contains('/vlib/v3/')
}

fn input_owns_builtin_bundle_module(input_file string, vroot string) bool {
	real_input := os.real_path(input_file)
	input_dir := if os.is_dir(real_input) { real_input } else { os.dir(real_input) }
	for relative_dir in ['builtin', 'strconv', 'strings', 'hash', os.join_path('math', 'bits')] {
		if input_dir == os.real_path(os.join_path(vroot, 'vlib', relative_dir)) {
			return true
		}
	}
	return false
}

fn input_is_legacy_diagnostic_fixture(input_file string) bool {
	if os.getenv('VTEST_RUNNER') != 'normal' || !input_file.ends_with('.vv') {
		return false
	}
	normalized := os.real_path(input_file).replace('\\', '/')
	if !['/vlib/v/checker/tests/', '/vlib/v/parser/tests/', '/vlib/v/scanner/tests/'].any(normalized.contains(it)) {
		return false
	}
	return os.is_file(input_file.all_before_last('.vv') + '.out')
}

fn default_bin_file_for_input(input_file string) string {
	if os.is_dir(input_file) {
		real_input := os.real_path(input_file)
		return os.join_path_single(real_input, os.file_name(real_input))
	}
	resolved_input := if os.exists(input_file) { os.real_path(input_file) } else { input_file }
	if !resolved_input.ends_with('.v') && !resolved_input.ends_with('.vv')
		&& !resolved_input.ends_with('.vsh') {
		return resolved_input
	}
	filename := os.file_name(resolved_input).trim_space()
	mut base := filename.all_before_last('.')
	if os.file_ext(base) in ['.c', '.js', '.wasm'] {
		base = base.all_before_last('.')
	}
	if base == '' {
		base = filename
	}
	if default_bin_file_needs_safe_name(base, filename) {
		base = safe_default_bin_file_name(filename)
	}
	input_dir := os.dir(resolved_input)
	return if input_dir in ['', '.'] { base } else { os.join_path_single(input_dir, base) }
}

fn default_bin_file_needs_safe_name(base string, filename string) bool {
	if base == '' || base in ['.', '..', '-'] {
		return true
	}
	if base == filename && filename.starts_with('.') {
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

fn safe_default_bin_file_name(filename string) string {
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

struct V3CCompilerFlagOptions {
	environment_c_flags  []string
	environment_ld_flags []string
	target_args          []string
	link_c_standard      string
	dependencies         []string
	warn_args            []string
	vroot                string
	target_os            string
	target_arch          string
	macos_sdk_root       string
	pic_flag             string
	is_prod              bool
	no_prod_options      bool
	is_shared            bool
	parallel_cc          bool
	large_c_unit         bool
	limit_inlining       bool
	explicit_tcc         bool
	is_c_debug           bool
	is_o                 bool
	is_liveshared        bool
}

struct V3CCompilerFlagPlan {
	before_inputs []string
	after_inputs  []string
	tcc_includes  string
}

fn (plan &V3CCompilerFlagPlan) compiler_args(output string, inputs []string, support_inputs []string) []string {
	mut args := plan.before_inputs.clone()
	args << ['-o', output]
	args << inputs
	args << support_inputs
	args << plan.after_inputs
	return args
}

fn (plan &V3CCompilerFlagPlan) all_flags(support_inputs []string) []string {
	mut flags := plan.before_inputs.clone()
	flags << support_inputs
	flags << plan.after_inputs
	return flags
}

fn v3_c_source_inputs(source string, objective_c bool) []string {
	if objective_c {
		return ['-x', 'objective-c', source, '-x', 'none']
	}
	return [source]
}

fn v3_c_source_mode_flags(objective_c bool) []string {
	if objective_c {
		return ['-x', 'objective-c', '-x', 'none']
	}
	return []string{}
}

fn v3_tcc_backtrace_enabled(target_os string, target_arch string, is_shared bool) bool {
	return !is_shared && !(target_os == 'macos' && target_arch == 'arm64')
}

fn add_v3_tcc_compat_defines(mut user_defines []string, target_os string, target_arch string, is_shared bool, explicit_tcc bool) {
	if explicit_tcc && !v3_tcc_backtrace_enabled(target_os, target_arch, is_shared)
		&& 'no_backtrace' !in user_defines {
		// The builtin backtrace implementation must match the native TCC flag plan.
		// Shared libraries cannot link TCC's runtime symbols, while its initializer
		// crashes in dyld on macOS arm64 due to unaligned access.
		user_defines << 'no_backtrace'
	}
}

fn v3_default_linker_flags(target_os string, is_o bool) []string {
	if is_o {
		return []
	}
	mut flags := ['-lm']
	if target_os in ['linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'solaris', 'haiku'] {
		flags << '-lpthread'
	}
	if target_os in ['freebsd', 'netbsd'] {
		flags << ['-lexecinfo', '-lelf']
	}
	return flags
}

fn add_v3_default_linker_flags(mut flags []string, target_os string, is_o bool) {
	for flag in v3_default_linker_flags(target_os, is_o) {
		if flag !in flags {
			flags << flag
		}
	}
}

struct V3TccResourceFlags {
	install_dir string
	base_arg    string
	include_arg string
	library_arg string
}

fn v3_tcc_resource_flags(vroot string) V3TccResourceFlags {
	tcc_root_dir := os.join_path(vroot, 'thirdparty', 'tcc')
	tcc_lib_dir := os.join_path_single(tcc_root_dir, 'lib')
	tcc_nested_dir := os.join_path_single(tcc_lib_dir, 'tcc')
	install_dir := if os.is_dir(tcc_nested_dir) { tcc_nested_dir } else { tcc_lib_dir }
	mut include_dir := os.join_path_single(install_dir, 'include')
	tcc_root_include_dir := os.join_path_single(tcc_root_dir, 'include')
	if !os.is_dir(include_dir) && os.is_dir(tcc_root_include_dir) {
		include_dir = tcc_root_include_dir
	}
	return V3TccResourceFlags{
		install_dir: install_dir
		base_arg: '-B${install_dir}'
		include_arg: '-I${include_dir}'
		library_arg: '-L${install_dir}'
	}
}

fn v3_tcc_host_system_flags(target_os string, macos_sdk_root string) []string {
	if target_os != os.user_os() || target_os == 'windows' {
		return []
	}
	// The bundled TCC resource root replaces its configured search root. Restore
	// the standard local prefix used by native packages such as wkhtmltox.
	mut flags := ['-I/usr/local/include', '-L/usr/local/lib']
	if target_os == 'macos' {
		if macos_sdk_root != '' {
			flags << '-I${os.join_path(macos_sdk_root, 'usr', 'include')}'
			flags << '-L${os.join_path(macos_sdk_root, 'usr', 'lib')}'
		}
	}
	return flags
}

fn macos_sdk_root() string {
	return cmdexec.macos_sdk_root()
}

// V3MacosSdkRootCache avoids repeating xcrun when a TinyCC build constructs
// both its general flag plan and its final link command.
struct V3MacosSdkRootCache {
mut:
	resolved bool
	root     string
}

fn (mut cache V3MacosSdkRootCache) get() string {
	if !cache.resolved {
		cache.root = macos_sdk_root()
		cache.resolved = true
	}
	return cache.root
}

fn v3_c_compiler_flag_plan(options V3CCompilerFlagOptions) V3CCompilerFlagPlan {
	mut before_inputs := options.environment_c_flags.clone()
	before_inputs << options.target_args
	if options.link_c_standard.len > 0 {
		before_inputs << options.link_c_standard
	}
	before_inputs << v3_prod_c_optimization_flags(options.is_prod, options.no_prod_options, options.is_shared, options.parallel_cc, options.large_c_unit, options.limit_inlining, options.explicit_tcc)
	if options.pic_flag.len > 0 {
		before_inputs << options.pic_flag
	}
	mut tcc_includes := ''
	if options.explicit_tcc {
		tcc_resources := v3_tcc_resource_flags(options.vroot)
		tcc_includes = tcc_resources.include_arg
		before_inputs << [tcc_resources.base_arg, tcc_resources.include_arg,
			tcc_resources.library_arg]
		before_inputs << v3_tcc_host_system_flags(options.target_os, options.macos_sdk_root)
		if v3_tcc_backtrace_enabled(options.target_os, options.target_arch, options.is_shared) {
			before_inputs << '-bt25'
		}
	}
	before_inputs << options.warn_args
	before_inputs << '-Wno-int-conversion'
	if options.target_os == 'macos' && !options.is_shared && !options.explicit_tcc {
		before_inputs << '-Wl,-stack_size,0x4000000'
	}
	if options.is_c_debug && options.target_os == 'macos' && !options.is_shared
		&& !options.explicit_tcc {
		before_inputs << '-Wl,-export_dynamic'
	}
	if options.is_shared {
		before_inputs << '-shared'
		if !options.is_liveshared && options.target_os == 'macos' {
			before_inputs << '-fvisibility=hidden'
		}
	} else if options.is_o {
		before_inputs << '-c'
	}
	if options.is_liveshared && options.target_os == 'macos' && !options.explicit_tcc {
		before_inputs << ['-flat_namespace', '-undefined', 'dynamic_lookup']
	}
	mut after_inputs := options.dependencies.clone()
	add_v3_default_linker_flags(mut after_inputs, options.target_os, options.is_o)
	if !options.is_o {
		after_inputs << options.environment_ld_flags
	}
	return V3CCompilerFlagPlan{
		before_inputs: before_inputs
		after_inputs: after_inputs
		tcc_includes: tcc_includes
	}
}

fn v3_c_project_dependency_flags(flags []string) []string {
	mut project_flags := []string{cap: flags.len}
	for flag in flags {
		clean := flag.trim(' \t\r\n"\'')
		if c_flag_is_object_file(clean) && !os.is_file(clean) {
			if source := c_source_from_object_file(clean) {
				project_flags << source
				continue
			}
		}
		project_flags << flag
	}
	return project_flags
}

fn v3_windows_batch_quote_arg(argument string) string {
	mut quoted := strings.new_builder(argument.len + 8)
	quoted.write_u8(`"`)
	mut pending_backslashes := 0
	for i := 0; i < argument.len; i++ {
		ch := argument[i]
		if ch == `\\` {
			pending_backslashes++
			continue
		}
		if ch == `"` {
			for _ in 0 .. pending_backslashes * 2 + 1 {
				quoted.write_u8(`\\`)
			}
			quoted.write_u8(`"`)
			pending_backslashes = 0
			continue
		}
		for _ in 0 .. pending_backslashes {
			quoted.write_u8(`\\`)
		}
		pending_backslashes = 0
		if ch == `%` {
			// Percent signs are expanded even inside quotes in a batch file.
			quoted.write_string('%%')
		} else {
			quoted.write_u8(ch)
		}
	}
	for _ in 0 .. pending_backslashes * 2 {
		quoted.write_u8(`\\`)
	}
	quoted.write_u8(`"`)
	return quoted.str()
}

fn v3_windows_batch_command(program string, args []string) string {
	mut parts := []string{cap: args.len + 1}
	parts << v3_windows_batch_quote_arg(program)
	for arg in args {
		parts << v3_windows_batch_quote_arg(arg)
	}
	return parts.join(' ')
}

fn v3_posix_shell_quote_arg(argument string) string {
	return "'" + argument.replace("'", "'\\''") + "'"
}

fn v3_posix_shell_command(program string, args []string) string {
	mut parts := []string{cap: args.len + 1}
	parts << v3_posix_shell_quote_arg(program)
	for arg in args {
		parts << v3_posix_shell_quote_arg(arg)
	}
	return parts.join(' ')
}

fn write_v3_c_project(project_dir string, c_source string, c_compiler string, plan V3CCompilerFlagPlan, support_inputs []string, objective_c bool) ! {
	output_name := os.base(c_source).all_before_last('.c')
	output_path := os.join_path_single(project_dir, output_name)
	args := plan.compiler_args(output_path, v3_c_source_inputs(c_source, objective_c), support_inputs)
	display_command := cmdexec.display(c_compiler, args)
	posix_command := v3_posix_shell_command(c_compiler, args)
	make_command := posix_command.replace('\$', '\$\$')
	windows_command := v3_windows_batch_command(c_compiler, args)
	os.write_file(os.join_path_single(project_dir, 'build_command.txt'), display_command + '\n')!
	os.write_file(os.join_path_single(project_dir, 'Makefile'), 'all:\n\t${make_command}\n')!
	build_sh := os.join_path_single(project_dir, 'build.sh')
	os.write_file(build_sh, '#!/bin/sh\nset -eu\n${posix_command}\n')!
	os.write_file(os.join_path_single(project_dir, 'build.bat'), '@echo off\r\nsetlocal DisableDelayedExpansion\r\n${windows_command}\r\n')!
	$if !windows {
		os.chmod(build_sh, 0o755)!
	}
}

fn emit_v3_js_compat_program(input_file string, output_file string) ! {
	source := os.read_file(input_file)!
	mut output := strings.new_builder(source.len)
	mut emitted := emit_v3_js_exported_global_aliases(source, mut output)
	mut offset := 0
	double_quote := [u8(34)].bytestr()
	js_eval_prefix := 'JS.eval(' + double_quote
	js_eval_suffix := double_quote + '.str'
	for offset < source.len {
		relative_start := source[offset..].index(js_eval_prefix) or { break }
		payload_start := offset + relative_start + js_eval_prefix.len
		relative_end := source[payload_start..].index(js_eval_suffix) or {
			return error('V3 JavaScript compatibility generation requires JS.eval with a string argument')
		}
		payload_end := payload_start + relative_end
		payload := source[payload_start..payload_end]
		output.writeln(payload)
		emitted = true
		offset = payload_end + js_eval_suffix.len
	}
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if !trimmed.starts_with('println(') || !trimmed.ends_with(')') {
			continue
		}
		argument := trimmed['println('.len..trimmed.len - 1].trim_space()
		if argument.len < 2 {
			continue
		}
		quote := argument[0]
		if (quote != 39 && quote != 34) || argument[argument.len - 1] != quote {
			continue
		}
		output.writeln('console.log(${argument});')
		emitted = true
	}
	if !emitted {
		return error('the V3 JavaScript compatibility generator currently supports only JS.eval and literal println')
	}
	os.mkdir_all(os.dir(output_file))!
	os.write_file(output_file, output.str())!
}

fn emit_v3_js_exported_global_aliases(source string, mut output strings.Builder) bool {
	mut export_name := ''
	mut emitted := false
	lines := source.split_into_lines()
	for line_idx, raw_line in lines {
		line := raw_line.trim_space()
		if line.starts_with('@[export:') {
			quote := if line.contains("'") { "'" } else { '"' }
			export_name = line.all_after(quote).all_before(quote)
			continue
		}
		if export_name.len == 0 || !line.starts_with('__global ') || !line.contains('= fn (') {
			continue
		}
		global_name := line.all_after('__global ').all_before('=').trim_space()
		params_text := line.all_after('fn (').all_before(')')
		mut params := []string{}
		for raw_param in params_text.split(',') {
			param := raw_param.trim_space().all_before(' ')
			if param.len > 0 {
				params << param
			}
		}
		mut return_expr := ''
		for body_line in lines[line_idx + 1..] {
			body := body_line.trim_space()
			if body.starts_with('return ') {
				return_expr = body.all_after('return ').trim_space()
				break
			}
			if body == '}' {
				break
			}
		}
		if global_name.len == 0 || return_expr.len == 0 {
			return false
		}
		if !emitted {
			output.writeln('const \$global = {};')
		}
		storage_name := '__v3_${global_name}'
		output.writeln('\$global["${storage_name}"] = function(${params.join(', ')}) { return ${return_expr}; };')
		output.writeln('Object.defineProperty(\$global,"${global_name}", {')
		output.writeln('\tget() { return \$global["${storage_name}"]; },')
		output.writeln('\tset(value) { \$global["${storage_name}"] = value; }')
		output.writeln('});')
		output.writeln('Object.defineProperty(globalThis,"${export_name}", {')
		output.writeln('\tget() { return \$global["${global_name}"]; },')
		output.writeln('\tset(value) { \$global["${global_name}"] = value; }')
		output.writeln('});')
		emitted = true
		export_name = ''
	}
	return emitted
}

fn keep_c_output_file(bin_file string) string {
	name := os.file_name(os.real_path(bin_file))
	mut sanitized := strings.new_builder(name.len)
	for ch in name {
		if ch >= 128 || (ch >= `0` && ch <= `9`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `a` && ch <= `z`) || ch in [`-`, `.`, `_`] {
			sanitized.write_u8(ch)
		} else {
			sanitized.write_u8(`_`)
		}
	}
	mut base := sanitized.str()
	if base in ['', '.', '..'] {
		base = 'vtmp'
	}
	return os.real_path(os.join_path_single(os.vtmp_dir(), '${base}.${tempname.unique_token()}.tmp.c'))
}

fn v3_crun_cache_marker_path(bin_file string) string {
	key := c_hash_bytes(u64(1469598103934665603), os.abs_path(bin_file).bytes()).hex()
	return os.join_path(os.vtmp_dir(), 'v3_crun_cache', key)
}

fn v3_crun_cache_matches(bin_file string, build_identity string, source_file string) bool {
	if build_identity.len == 0 {
		return false
	}
	if os.file_last_mod_unix(source_file) > os.file_last_mod_unix(bin_file) {
		return false
	}
	binary_signature := modulecache.file_signature(bin_file)
	if binary_signature.len == 0 {
		return false
	}
	marker := os.read_file(v3_crun_cache_marker_path(bin_file)) or { return false }
	return marker == '${build_identity}\n${binary_signature}'
}

fn write_v3_crun_cache_marker(bin_file string, build_identity string) ! {
	if build_identity.len == 0 {
		return
	}
	binary_signature := modulecache.file_signature(bin_file)
	if binary_signature.len == 0 {
		return
	}
	marker := v3_crun_cache_marker_path(bin_file)
	os.mkdir_all(os.dir(marker), mode: 0o700)!
	staged := '${marker}.stage.${tempname.unique_token()}'
	os.write_file(staged, '${build_identity}\n${binary_signature}')!
	os.mv(staged, marker) or {
		os.rm(staged) or {}
		return err
	}
}

fn v3_crun_build_identity(state &V3ModuleCacheState, prefs &pref.Preferences, user_files []string, user_c_flags []string, is_strict bool, enable_globals bool, direct_vsh string) string {
	direct_vsh_path := os.real_path(direct_vsh)
	mut source_paths := map[string]bool{}
	for file in user_files {
		real_file := os.real_path(file)
		// Direct `.vsh` execution follows V's executable-cache contract: the
		// script timestamp decides whether its cached binary is stale. Imported
		// modules remain content-addressed through the identity below.
		if real_file != direct_vsh_path {
			source_paths[real_file] = true
		}
	}
	for files in state.module_sources.values() {
		for file in files {
			source_paths[os.real_path(file)] = true
		}
	}
	mut sources := source_paths.keys()
	sources.sort()
	source_signature := modulecache.cached_source_signature(state.manager.dir, 'crun', sources)
	if source_signature.len == 0 {
		return ''
	}
	uses_build_time := modulecache.source_files_use_build_time_pseudo(sources)
	mut hash := u64(1469598103934665603)
	for value in [
		'v3-crun-cache-v1',
		state.manager.salt,
		is_strict.str(),
		enable_globals.str(),
		user_c_flags.join('\x00'),
		source_signature,
		prefs.vhash,
		prefs.vcurrent_hash,
		if uses_build_time {
			prefs.build_date
		} else {
			''
		},
		if uses_build_time {
			prefs.build_time
		} else {
			''
		},
		if uses_build_time {
			prefs.build_timestamp
		} else {
			''
		},
	] {
		hash = c_hash_bytes(hash, value.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	mut external_paths := map[string]bool{}
	for paths in state.module_external_inputs.values() {
		for path in paths {
			external_paths[os.real_path(path)] = true
		}
	}
	for paths in state.module_native_roots.values() {
		for path in paths {
			external_paths[os.real_path(path)] = true
		}
	}
	for path in state.external_resolution_dirs {
		external_paths[os.real_path(path)] = true
	}
	for path in state.external_missing_paths {
		external_paths[os.real_path(path)] = true
	}
	mut paths := external_paths.keys()
	paths.sort()
	for path in paths {
		hash = c_hash_bytes(hash, path.bytes())
		hash = c_hash_bytes(hash, [u8(0)])
		hash = c_hash_bytes(hash, modulecache.file_metadata_signature(path).bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

fn cli_usage() string {
	return 'usage: v3 [run|test] <file.v|directory> [options]\n' + '  -o <output>                 output binary or C file\n' + '  -b <c|fastc|arm64|wasm|eval> backend\n' + '  -os <name> -arch <name>     target platform\n' + '  -cc <compiler>               C compiler executable\n' + '  -thread-stack-size <bytes>   spawned-thread stack size\n' + '  -prod -c99 -shared -strict  C build modes\n' + '  -v                           verbose stage profiling\n' + '  -silent                      suppress benchmark output\n' + '  -showcc                      print C compiler commands\n' + '  -profile [file]              write V1-compatible function profile data\n' + '  -profile-fns <names>         profile only named functions and their callees\n' + '  -profile-no-inline           omit @[inline] functions from the profile\n' + '  -no-memory-limit             disable the 4032 MiB user-build memory safety limit\n' + '  -d <name>                    compile-time define'
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

fn with_executable_postfix(path string, target_os string) string {
	if pref.normalized_os(target_os) != 'windows' || path.ends_with('.exe') {
		return path
	}
	return path + '.exe'
}

fn c_executable_bin_file_for_target(path string, target_os string, is_shared bool, is_o bool, c_only bool) string {
	if is_shared || is_o || c_only {
		return path
	}
	return with_executable_postfix(path, target_os)
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
	// Compiler executables built by TinyCC can corrupt their heap while several
	// specialization workers merge their results. Keep that build serial until
	// the parallel merge is safe under TinyCC as well as clang and GCC.
	$if tinyc {
		return false
	}
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

// release_unused_diagnostic_scope discards notice storage before its arena is released.
fn release_unused_diagnostic_scope(mut notices []types.TypeError, scope voidptr) {
	prealloc_scope_leave_for_v3(scope)
	// clear() retains capacity that may have been grown in the disposable scope.
	// Rebind under the parent allocator before releasing that scoped storage.
	notices.clear()
	notices = []types.TypeError{}
	prealloc_scope_free_for_v3(scope)
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

fn clone_type_errors(values []types.TypeError) []types.TypeError {
	if values.len == 0 {
		return []types.TypeError{}
	}
	mut cloned := []types.TypeError{cap: values.len}
	for value in values {
		cloned << types.TypeError{
			msg: value.msg.clone()
			kind: value.kind
			node: value.node
			file: value.file.clone()
			node_kind: value.node_kind.clone()
			node_value: value.node_value.clone()
			node_pos: value.node_pos.clone()
			pos: value.pos
			details: clone_string_list(value.details)
			severity: value.severity.clone()
		}
	}
	return cloned
}

fn v3_cgen_cache_input(state &V3ModuleCacheState, user_files []string, user_c_flags []string) V3CgenCacheInput {
	mut source_set := map[string]bool{}
	mut user_source_dirs := map[string]bool{}
	for file in user_files {
		real_file := os.real_path(file)
		source_set[real_file] = true
		user_source_dirs[os.dir(real_file)] = true
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
			key := v3_external_input_key(module_name, path)
			dependencies['external:${module_name}:${path}'] = state.external_input_signatures[key] or {
				modulecache.file_signature(path)
			}
			if digest := state.external_input_digests[os.real_path(path)] {
				dependencies['external-sha256:${module_name}:${path}'] = digest
			}
			dependencies['external-meta:${module_name}:${path}'] =
				modulecache.file_metadata_signature(path)
		}
	}
	if state.external_inputs_ready {
		dependencies['external-state:manifest'] = 'v3-external-inputs-5'
		mut root_modules := state.module_native_roots.keys()
		root_modules.sort()
		for module_name in root_modules {
			for index, path in state.module_native_roots[module_name] {
				dependencies['external-root:${module_name}:${index}'] = '${path}\t${modulecache.file_metadata_signature(path)}'
				dependencies['external-context:${module_name}:${index}'] = (state.native_root_contexts[path] or {
					[]string{}
				}).join('\x1e')
				dependencies['external-root-owner:${module_name}:${index}'] = state.native_root_owners[os.real_path(path)] or {
					''
				}
			}
		}
		mut owner_modules := state.native_source_modules.keys()
		owner_modules.sort()
		for module_name in owner_modules {
			if state.native_source_modules[module_name] {
				dependencies['external-owner:${module_name}'] = '1'
			}
		}
		for path in state.external_resolution_dirs {
			if user_source_dirs[os.real_path(path)] {
				continue
			}
			dependencies['external-dir:${path}'] = modulecache.file_metadata_signature(path)
		}
		for path in state.external_missing_paths {
			dependencies['external-missing:${path}'] = 'missing'
		}
	}
	mut source_files := source_set.keys()
	source_files.sort()
	return V3CgenCacheInput{
		source_files: source_files
		dependency_inputs: dependencies
		generation_signature: user_c_flags.join('\x00')
	}
}

fn persistent_program_cache_enabled(cache_enabled bool, test_input bool, vtmp_dir string) bool {
	// Test sessions compile thousands of unique programs. Their shared module
	// objects are reusable, but retaining every whole-program snapshot only grows
	// the temporary session until the complete suite exits. An explicit V3CACHE
	// is caller-owned and is used by cache regression tests with bounded roots.
	return cache_enabled && !test_input
		&& (!os.base(vtmp_dir).starts_with('tsession_') || os.getenv('V3CACHE') != '')
}

fn prepare_v3_cache_external_inputs(mut state V3ModuleCacheState, a &flat.FlatAst, prefs &pref.Preferences, user_files []string, user_c_flags []string) bool {
	mut cache_input_modules := map[string]bool{}
	for module_name in state.module_sources.keys() {
		cache_input_modules[module_name] = true
	}
	cache_input_modules['main'] = true
	native_inputs_language := cgen.cache_native_inputs_language(a, prefs.vroot, user_c_flags, prefs.c99, prefs.target)
	compiler_macros, compiler_macro_environment_complete := cache_c_compiler_predefined_macros(user_c_flags, prefs.ccompiler, prefs.target, native_inputs_language)
	mut external_inputs, mut native_source_roots, mut native_root_contexts, unscoped_inputs, static_storage_inputs, resolution_dirs, missing_resolution_paths, mut external_input_digests, has_untracked_c_include := cgen.cache_external_input_snapshot_with_resolved_flags(a, prefs.vroot, cache_input_modules, user_c_flags, prefs.target, module_cache_source_path_set(user_files), compiler_macros, compiler_macro_environment_complete)
	state.module_external_inputs = external_inputs.move()
	state.module_native_roots = native_source_roots.move()
	state.native_root_contexts = native_root_contexts.move()
	state.external_input_signatures = map[string]string{}
	state.external_input_digests = external_input_digests.move()
	cache_dir := os.abs_path(state.manager.dir)
	real_cache_dir := os.real_path(state.manager.dir)
	state.external_resolution_dirs = resolution_dirs.filter(!v3_path_is_within(it, cache_dir)
		&& !v3_path_is_within(it, real_cache_dir))
	state.external_missing_paths = missing_resolution_paths.filter(!v3_path_is_within(it, cache_dir) && !v3_path_is_within(it, real_cache_dir))
	mut native_source_modules, can_scope_static_inputs := cache_external_input_owner_modules(state, a, unscoped_inputs, static_storage_inputs, user_files, user_c_flags, prefs.ccompiler, prefs.target)
	state.native_source_modules = native_source_modules.move()
	state.native_root_owners = map[string]string{}
	for raw_module_name, roots in state.module_native_roots {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if !state.native_source_modules[module_name] {
			continue
		}
		for root in roots {
			state.native_root_owners[os.real_path(root)] = module_name
		}
	}
	can_extract_native_types := prepare_v3_cache_native_type_declarations(mut state, user_c_flags, prefs.ccompiler, prefs.target)
	state.external_inputs_ready = true
	state.external_inputs_complete = !has_untracked_c_include
		&& v3_external_input_digests_complete(state)
	if os.getenv('V3_CACHE_TRACE') != '' {
		if has_untracked_c_include {
			eprintln('  V3 module cache external input miss: reason=unresolved C include')
		}
		if !can_scope_static_inputs {
			eprintln('  V3 module cache external input miss: reason=static C input has no cache unit')
		}
	}
	return !has_untracked_c_include && can_scope_static_inputs && can_extract_native_types
}

// prepare_v3_cache_external_inputs_scoped releases the large preprocessor and
// declaration-scanner scratch buffers while retaining the compact cache manifest.
fn prepare_v3_cache_external_inputs_scoped(mut state V3ModuleCacheState, a &flat.FlatAst, prefs &pref.Preferences, user_files []string, user_c_flags []string, scope_enabled bool) bool {
	if !scope_enabled {
		return prepare_v3_cache_external_inputs(mut state, a, prefs, user_files, user_c_flags)
	}
	scope := prealloc_scope_begin_for_v3()
	complete := prepare_v3_cache_external_inputs(mut state, a, prefs, user_files, user_c_flags)
	prealloc_scope_leave_for_v3(scope)
	state.module_external_inputs = clone_string_list_map(state.module_external_inputs)
	state.module_native_roots = clone_string_list_map(state.module_native_roots)
	state.native_root_contexts = clone_string_list_map(state.native_root_contexts)
	state.native_root_owners = clone_string_string_map(state.native_root_owners)
	state.external_input_signatures = clone_string_string_map(state.external_input_signatures)
	state.external_input_digests = clone_string_string_map(state.external_input_digests)
	state.external_resolution_dirs = clone_string_list(state.external_resolution_dirs)
	state.external_missing_paths = clone_string_list(state.external_missing_paths)
	state.native_source_modules = clone_string_bool_map(state.native_source_modules)
	state.native_type_declarations = clone_string_string_map(state.native_type_declarations)
	state.native_declared_functions = clone_nested_string_bool_map(state.native_declared_functions)
	prealloc_scope_free_for_v3(scope)
	return complete
}

// prepare_v3_checker_native_inputs resolves native `#include` source roots so
// the checker can register their typedefs, skipping the cache-unit ownership
// scan and native type-declaration extraction whose outputs only cache-enabled
// builds consume (cache dependency manifests and per-unit C source rewriting).
fn prepare_v3_checker_native_inputs(mut state V3ModuleCacheState, a &flat.FlatAst, prefs &pref.Preferences, user_files []string, user_c_flags []string) {
	mut cache_input_modules := map[string]bool{}
	for module_name in state.module_sources.keys() {
		cache_input_modules[module_name] = true
	}
	cache_input_modules['main'] = true
	native_inputs_language := cgen.cache_native_inputs_language(a, prefs.vroot, user_c_flags, prefs.c99, prefs.target)
	compiler_macros, compiler_macro_environment_complete := cache_c_compiler_predefined_macros(user_c_flags, prefs.ccompiler, prefs.target, native_inputs_language)
	mut external_inputs, mut native_source_roots, mut native_root_contexts, _, _, resolution_dirs, missing_resolution_paths, mut external_input_digests, has_untracked_c_include := cgen.cache_external_input_snapshot_with_resolved_flags(a, prefs.vroot, cache_input_modules, user_c_flags, prefs.target, module_cache_source_path_set(user_files), compiler_macros, compiler_macro_environment_complete)
	state.module_external_inputs = external_inputs.move()
	state.module_native_roots = native_source_roots.move()
	state.native_root_contexts = native_root_contexts.move()
	state.external_input_signatures = map[string]string{}
	state.external_input_digests = external_input_digests.move()
	cache_dir := os.abs_path(state.manager.dir)
	real_cache_dir := os.real_path(state.manager.dir)
	state.external_resolution_dirs = resolution_dirs.filter(!v3_path_is_within(it, cache_dir)
		&& !v3_path_is_within(it, real_cache_dir))
	state.external_missing_paths = missing_resolution_paths.filter(!v3_path_is_within(it, cache_dir) && !v3_path_is_within(it, real_cache_dir))
	state.external_inputs_ready = true
	// The macOS C-error fallback report requires a complete native-input
	// manifest, and completeness here needs only resolved includes and valid
	// digests — cache-unit ownership is a cache-manifest concern.
	state.external_inputs_complete = !has_untracked_c_include
		&& v3_external_input_digests_complete(state)
}

// prepare_v3_checker_native_inputs_scoped releases the native preprocessor's
// scratch buffers while retaining the small manifest needed by checking and Cgen.
fn prepare_v3_checker_native_inputs_scoped(mut state V3ModuleCacheState, a &flat.FlatAst, prefs &pref.Preferences, user_files []string, user_c_flags []string, scope_enabled bool) {
	if !scope_enabled {
		prepare_v3_checker_native_inputs(mut state, a, prefs, user_files, user_c_flags)
		return
	}
	scope := prealloc_scope_begin_for_v3()
	prepare_v3_checker_native_inputs(mut state, a, prefs, user_files, user_c_flags)
	prealloc_scope_leave_for_v3(scope)
	state.module_external_inputs = clone_string_list_map(state.module_external_inputs)
	state.module_native_roots = clone_string_list_map(state.module_native_roots)
	state.native_root_contexts = clone_string_list_map(state.native_root_contexts)
	state.external_input_signatures = clone_string_string_map(state.external_input_signatures)
	state.external_input_digests = clone_string_string_map(state.external_input_digests)
	state.external_resolution_dirs = clone_string_list(state.external_resolution_dirs)
	state.external_missing_paths = clone_string_list(state.external_missing_paths)
	prealloc_scope_free_for_v3(scope)
}

struct PrepareV3CheckerNativeInputsArgs {
	state         voidptr
	a             &flat.FlatAst
	prefs         &pref.Preferences
	user_files    []string
	user_c_flags  []string
	scope_enabled bool
	done          chan bool
}

fn prepare_v3_checker_native_inputs_thread(args &PrepareV3CheckerNativeInputsArgs) {
	mut state := unsafe { &V3ModuleCacheState(args.state) }
	prepare_v3_checker_native_inputs_scoped(mut state, args.a, args.prefs, args.user_files, args.user_c_flags, args.scope_enabled)
	args.done <- true
}

fn ast_has_native_source_include(a &flat.FlatAst) bool {
	for node in a.nodes {
		if node.kind != .directive
			|| node.value !in ['include', 'insert', 'preinclude', 'postinclude'] {
			continue
		}
		path := node.typ.trim_space().trim('"')
		if path.ends_with('.c') || path.ends_with('.m') || path.ends_with('.mm') {
			return true
		}
	}
	return false
}

fn native_source_typedefs(path string) map[string]bool {
	mut typedefs := map[string]bool{}
	source := os.read_file(path) or { return typedefs }
	for name, present in modulecache.c_source_typedef_identifiers(source) {
		if !present || name.len == 0 {
			continue
		}
		if !c_typedef_is_function_pointer(source, name) {
			typedefs[name] = true
		} else if name !in typedefs {
			typedefs[name] = false
		}
	}
	return typedefs
}

fn register_native_source_typedefs(mut tc types.TypeChecker, state &V3ModuleCacheState, scope_enabled bool) {
	mut typedefs := map[string]bool{}
	mut seen_paths := map[string]bool{}
	for roots in state.module_native_roots.values() {
		for path in roots {
			real_path := os.real_path(path)
			if seen_paths[real_path] {
				continue
			}
			seen_paths[real_path] = true
			mut path_typedefs := map[string]bool{}
			if scope_enabled {
				scope := prealloc_scope_begin_for_v3()
				path_typedefs = native_source_typedefs(real_path)
				prealloc_scope_leave_for_v3(scope)
				path_typedefs = clone_string_bool_map(path_typedefs)
				prealloc_scope_free_for_v3(scope)
			} else {
				path_typedefs = native_source_typedefs(real_path)
			}
			for name, is_struct in path_typedefs {
				if is_struct || name !in typedefs {
					typedefs[name] = is_struct
				}
			}
		}
	}
	for name, is_struct in typedefs {
		c_name := 'C.${name}'
		if c_name !in tc.structs {
			tc.structs[c_name] = []types.StructField{}
		}
		if is_struct {
			tc.c_typedef_structs[c_name] = true
		}
	}
}

fn register_headerless_c_types(mut tc types.TypeChecker) {
	// The C backend always supplies this platform-specific declaration in its
	// headerless preamble, even when the program does not import `os`.
	if 'C.stat' !in tc.structs {
		tc.structs['C.stat'] = []types.StructField{}
	}
}

fn c_typedef_is_function_pointer(source string, name string) bool {
	mut offset := 0
	for offset < source.len {
		// index_after scans in place from `offset`. `source[offset..].index(name)`
		// allocated a fresh copy of the whole remaining tail of `source` on every
		// iteration; this function runs once per typedef name per native header, so
		// under -prealloc (allocations in a stage scope are not freed until the
		// scope ends) those tail copies accumulated to multiple GB of transient
		// RSS on a build pulling large native headers (sokol, stb, mbedtls,
		// openssl) — enough to trip v3's memory ceiling and fall back to V1.
		start := source.index_after(name, offset) or { return false }
		end := start + name.len
		if (start == 0 || (!source[start - 1].is_alnum() && source[start - 1] != `_`))
			&& (end == source.len || (!source[end].is_alnum() && source[end] != `_`)) {
			mut i := start - 1
			for i >= 0 && source[i].is_space() {
				i--
			}
			if i >= 0 && source[i] == `*` {
				i--
				for i >= 0 && source[i].is_space() {
					i--
				}
				if i >= 0 && source[i] == `(` {
					return true
				}
			}
		}
		offset = end
	}
	return false
}

fn cache_c_compiler_predefined_macros(flags []string, ccompiler string, target pref.Target, native_inputs_language string) (map[string]string, bool) {
	path := os.join_path(os.vtmp_dir(), 'v3_compiler_macros_${tempname.unique_token()}.c')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, '') or { return map[string]string{}, false }
	mut args := c_compiler_target_args(target, false) or { return map[string]string{}, false }
	args << c_object_compile_flags(cache_c_flags_without_forced_inputs(flags))
	args << ['-dM', '-E', '-x', cache_probe_language(native_inputs_language, flags), path]
	result := cmdexec.run(ccompiler, args)
	if result.exit_code != 0 {
		if os.getenv('V3_CACHE_TRACE') != '' {
			eprintln('  V3 module cache compiler macro probe failed: compiler=${ccompiler}')
		}
		return map[string]string{}, false
	}
	mut macros := map[string]string{}
	for line in result.output.split_into_lines() {
		if !line.starts_with('#define ') {
			continue
		}
		rest := line['#define '.len..]
		mut end := 0
		for end < rest.len && (rest[end].is_alnum() || rest[end] == `_`) {
			end++
		}
		if end == 0 {
			continue
		}
		name := rest[..end]
		// Function-like macros are still definitely defined, but their expansion
		// cannot be used as a literal include target.
		macros[name] = rest[end..].trim_space()
	}
	return macros, true
}

// cache_c_flags_without_forced_inputs drops `-include`/`-imacros` and their file
// operands. The predefined-macro probe compiles an empty translation unit to
// capture only the compiler/target/`-D` baseline, but forced-input files execute
// before that empty input, so leaving them in would report their macros as
// predefined. The dependency scanner would then start with those macros already
// defined and skip includes guarded by `#if !defined(X)`, dropping the nested
// files from the cache dependency set and serving stale output after they change.
fn cache_c_flags_without_forced_inputs(flags []string) []string {
	mut out := []string{cap: flags.len}
	mut i := 0
	for i < flags.len {
		if flags[i].trim_space() in ['-include', '-imacros'] {
			// Skip the option together with its file operand.
			i += 2
			continue
		}
		out << flags[i]
		i++
	}
	return out
}

// cache_probe_language combines the richest language the native inputs require
// with any Objective-C request from the command-line flags, so the `-dM` probe
// captures every language macro the real build defines.
fn cache_probe_language(native_inputs_language string, flags []string) string {
	mut need_objc := native_inputs_language in ['objective-c', 'objective-c++']
	need_cpp := native_inputs_language in ['c++', 'objective-c++']
	if c_flags_need_objective_c(flags) {
		need_objc = true
	}
	if need_objc && need_cpp {
		return 'objective-c++'
	}
	if need_cpp {
		return 'c++'
	}
	if need_objc {
		return 'objective-c'
	}
	return 'c'
}

fn cached_native_sources_require_monolithic_cgen(state &V3ModuleCacheState, a &flat.FlatAst, user_files []string) bool {
	if state.native_source_modules.len == 0 {
		return false
	}
	for raw_module_name, paths in state.module_external_inputs {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		// Main native sources remain in the program translation unit, so their
		// private type declarations never need to cross a cached-object boundary.
		if module_name == 'main' || !state.native_source_modules[module_name] {
			continue
		}
		for path in paths {
			if !c_flag_is_c_source_file(path) {
				continue
			}
			source := os.read_file(path) or { continue }
			if modulecache.c_source_declares_types(source) {
				type_identifiers := modulecache.c_source_type_identifiers(source)
				if cache_external_identifiers_are_private_to_module(a, state, raw_module_name, type_identifiers, user_files, '') {
					continue
				}
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache native source type declaration: module=${module_name} path=${path}')
				}
				return true
			}
		}
	}
	return false
}

fn v3_path_is_within(path string, dir string) bool {
	return dir.len > 0 && (path == dir || path.starts_with(dir + os.path_separator))
}

fn prepare_v3_cache_native_type_declarations(mut state V3ModuleCacheState, c_flags []string, ccompiler string, target pref.Target) bool {
	state.native_type_declarations = map[string]string{}
	state.native_declared_functions = map[string]map[string]bool{}
	mut allowed_paths := map[string]bool{}
	for paths in state.module_external_inputs.values() {
		for path in paths {
			allowed_paths[os.real_path(path)] = true
		}
	}
	for raw_module_name, roots in state.module_native_roots {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if !state.native_source_modules[module_name] {
			continue
		}
		// Main native sources stay in the program translation unit. Only imported
		// native sources need declarations extracted after they move to a module object.
		if module_name == 'main' {
			mut declared_functions := map[string]bool{}
			for root in roots {
				source := os.read_file(root) or { continue }
				for name, declared in modulecache.c_source_function_identifiers(source) {
					if declared {
						declared_functions[name] = true
					}
				}
			}
			if declared_functions.len > 0 {
				state.native_declared_functions[module_name] = declared_functions.clone()
			}
			continue
		}
		mut declared_functions := state.native_declared_functions[module_name].clone()
		mut roots_with_function_declarations := map[string]bool{}
		mut declaration_macros := cache_local_c_compiler_macros(c_flags, ccompiler, target)
		mut declaration_active_paths := map[string]bool{}
		mut declarations_complete_for_module := true
		for root in roots {
			cache_apply_native_root_context(state.native_root_contexts[os.real_path(root)] or {
				[]string{}
			}, mut declaration_macros)
			active_source, declarations_complete := cache_c_source_definitely_active_code_for_path_with_status(root, allowed_paths, mut declaration_active_paths, mut declaration_macros, false)
			if !declarations_complete {
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache unresolved native declaration guard: module=${module_name} path=${root}')
				}
				declarations_complete_for_module = false
				break
			}
			root_functions := modulecache.c_source_function_identifiers(active_source)
			for name, declared in root_functions {
				if declared {
					declared_functions[name] = true
					roots_with_function_declarations[os.real_path(root)] = true
				}
			}
		}
		if !declarations_complete_for_module {
			if roots.any(c_flag_is_c_source_file(it)) {
				return false
			}
			if roots.len > 1 {
				// Several implementation headers can share macro state with later
				// inlined directives. Replaying each header independently after an
				// uncertain guard would expose definitions in every cache object.
				return false
			}
			mut recovered_functions := map[string]bool{}
			for root in roots {
				real_root := os.real_path(root)
				source := os.read_file(real_root) or { continue }
				file_scope_identifiers := c_source_file_scope_identifiers(source)
				preprocessed := cache_preprocessed_native_input(real_root, state.native_root_contexts[real_root] or {
					[]string{}
				}, c_flags, ccompiler, target) or { continue }
				functions, _ := modulecache.c_source_function_identifiers_with_status(preprocessed)
				mut root_has_recovered_functions := false
				for name, present in functions {
					if present && file_scope_identifiers[name] {
						recovered_functions[name] = true
						root_has_recovered_functions = true
					}
				}
				if root_has_recovered_functions {
					// An unresolved conditional can make declaration extraction lose a
					// directive that is nested inside a function branch. Replay headers
					// with their implementation switches disabled instead; the check below
					// still rejects any external definition that would survive the replay.
					roots_with_function_declarations[real_root] = true
				}
			}
			if recovered_functions.len > 0 {
				for name, present in recovered_functions {
					if present {
						declared_functions[name] = true
					}
				}
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache recovered guarded native function prototypes: module=${module_name} functions=${recovered_functions.len}')
				}
			} else {
				declared_functions.clear()
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache keeping guarded native function prototypes: module=${module_name}')
				}
			}
		}
		if declared_functions.len > 0 {
			state.native_declared_functions[module_name] = declared_functions.clone()
		}
		mut include_macros := cache_local_c_compiler_macros(c_flags, ccompiler, target)
		mut types_complete_for_module := true
		for root in roots {
			real_root := os.real_path(root)
			cache_apply_native_root_context(state.native_root_contexts[real_root] or { []string{} }, mut include_macros)
			declarations, complete := cache_native_type_declarations_for_path(real_root, allowed_paths, mut include_macros)
			if !complete {
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache unresolved native type include: module=${module_name} path=${real_root}')
				}
				types_complete_for_module = false
				break
			}
			if roots_with_function_declarations[real_root] && !c_flag_is_c_source_file(real_root) {
				context := state.native_root_contexts[real_root] or { []string{} }
				implementation_macros := cache_native_implementation_context_macros(real_root, context, allowed_paths, c_flags, ccompiler, target)
				// The public replay only strips implementation code that its undefined
				// macros gate. If a file-scope external definition would survive, the
				// owner object and every dependent replay would define the symbol, so the
				// warm cached link fails with a duplicate symbol. Fail closed instead of
				// splitting. Keep safe function-only headers even when they declare no
				// types, since dependent cache units still need their static inline APIs.
				if cache_native_public_include_replays_external_definition(real_root, context, implementation_macros, allowed_paths, c_flags, ccompiler, target) {
					if os.getenv('V3_CACHE_TRACE') != '' {
						eprintln('  V3 module cache ungated native definition: module=${module_name} path=${real_root}')
					}
					return false
				}
				state.native_type_declarations[real_root] = cache_native_public_include(real_root, context, implementation_macros)
			} else if declarations.len > 0 {
				state.native_type_declarations[real_root] = declarations
			}
		}
		if !types_complete_for_module {
			if roots.any(c_flag_is_c_source_file(it)) {
				return false
			}
			if consumer_module := cache_native_type_consumer_module(state, module_name, roots) {
				for root in roots {
					real_root := os.real_path(root)
					context := state.native_root_contexts[real_root] or { []string{} }
					state.native_root_owners[real_root] = consumer_module
					// Other cache objects still need the header's public ABI. Reinclude it
					// without the implementation context; the consumer object receives the
					// original context and full implementation in source order.
					implementation_macros := cache_native_implementation_context_macros(real_root, context, allowed_paths, c_flags, ccompiler, target)
					state.native_type_declarations[real_root] = cache_native_public_include(real_root, context, implementation_macros)
				}
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache grouped native type owner: module=${module_name} consumer=${consumer_module}')
				}
				continue
			}
			for root in roots {
				real_root := os.real_path(root)
				context := state.native_root_contexts[real_root] or { []string{} }
				implementation_macros := cache_native_implementation_context_macros(real_root, context, allowed_paths, c_flags, ccompiler, target)
				state.native_type_declarations[real_root] = cache_native_public_include(real_root, context, implementation_macros)
			}
			if os.getenv('V3_CACHE_TRACE') != '' {
				eprintln('  V3 module cache exposing unresolved native headers as public ABI: module=${module_name}')
			}
		}
	}
	return true
}

fn cache_native_type_consumer_module(state &V3ModuleCacheState, owner_module string, roots []string) ?string {
	mut type_identifiers := map[string]bool{}
	for root in roots {
		source := os.read_file(root) or { continue }
		for identifier, present in modulecache.c_source_type_identifiers(source) {
			if present {
				type_identifiers[identifier] = true
			}
		}
	}
	if type_identifiers.len == 0 {
		return none
	}
	mut consumer := ''
	for raw_module_name, paths in state.module_external_inputs {
		candidate := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if candidate == owner_module || candidate == 'main'
			|| !state.native_source_modules[candidate]
			|| owner_module !in cache_dependency_modules(state, [candidate]) {
			continue
		}
		mut references_type := false
		for path in paths {
			source := os.read_file(path) or { continue }
			if c_source_references_identifiers(source, type_identifiers) {
				references_type = true
				break
			}
		}
		if !references_type {
			continue
		}
		if consumer.len > 0 && consumer != candidate {
			return none
		}
		consumer = candidate
	}
	if consumer.len == 0 {
		return none
	}
	return consumer
}

fn cache_apply_native_root_context(directives []string, mut macros map[string]V3CacheLocalCMacro) {
	for line in directives {
		directive, arg := cache_local_c_directive(line)
		if directive in ['define', 'undef'] {
			cache_record_local_c_include_macro(directive, arg, false, mut macros)
		}
	}
}

fn cache_native_public_include(path string, context []string, implementation_macros map[string]bool) string {
	mut out := strings.new_builder(context.len * 24 + path.len + 16)
	// Sokol's umbrella implementation switch enables every component header. A
	// prior owning include may leave it defined even when this header's own
	// context is declaration-only.
	out.writeln('#undef SOKOL_IMPL')
	for line in context {
		directive, arg := cache_local_c_directive(line)
		if directive == 'undef' {
			out.writeln(line)
			continue
		}
		if directive == 'include' {
			// The declaration prefix has already emitted every header that precedes
			// this root. Replaying a physical include after an inlined copy can
			// redefine types even when the header uses `#pragma once`.
			continue
		}
		if directive != 'define' {
			out.writeln(line)
			continue
		}
		name := cache_local_c_define_name(arg)
		if implementation_macros[name] || cache_native_implementation_macro(name) {
			out.writeln('#undef ${name}')
		} else {
			out.writeln(line)
		}
	}
	out.writeln('#include "${c_include_path(path)}"')
	return out.str()
}

fn cache_native_implementation_macro(name string) bool {
	return name.ends_with('_IMPLEMENTATION')
		|| (name.starts_with('SOKOL') && name.ends_with('_IMPL'))
}

// cache_native_public_include_replays_external_definition reports whether the
// declaration-only replay produced by cache_native_public_include would still
// compile a file-scope external-linkage function definition. Such a symbol would
// be emitted both in the owner module object (full include) and in every
// non-owner public replay, so the caller must fail closed rather than split the
// root. The header is preprocessed with the same macro state the replay
// establishes (every implementation switch and the Sokol umbrella undefined, all
// other context defines kept) so storage-class macros such as a `static inline`
// hidden behind a macro are expanded before linkage is classified; a textual scan
// cannot see through them and would misread internal-linkage helpers as external.
// Results are limited to identifiers declared by the root or its active project
// headers, so definitions pulled in from system headers are ignored. Any failure
// to verify is unsafe.
fn cache_native_public_include_replays_external_definition(path string, context []string, implementation_macros map[string]bool, allowed_paths map[string]bool, c_flags []string, ccompiler string, target pref.Target) bool {
	mut replay_context := ['#undef SOKOL_IMPL']
	for line in context {
		directive, arg := cache_local_c_directive(line)
		if directive == 'define' {
			name := cache_local_c_define_name(arg)
			if implementation_macros[name] || cache_native_implementation_macro(name) {
				replay_context << '#undef ${name}'
				continue
			}
		}
		replay_context << line
	}
	mut replay_macros := cache_local_c_compiler_macros(c_flags, ccompiler, target)
	cache_apply_native_root_context(replay_context, mut replay_macros)
	mut active_paths := map[string]bool{}
	active_source, active_complete := cache_c_source_definitely_active_code_for_path_with_status(path, allowed_paths, mut active_paths, mut replay_macros, false)
	// Prefer compiler-expanded storage-class macros. Some public native headers
	// deliberately require dependency declarations to have been included first,
	// so an isolated preprocessing probe can fail even though the generated cache
	// unit has that dependency prefix. In that case the definitely-active raw scan
	// still provides a conservative linkage classification; incomplete raw syntax
	// continues to fail closed below.
	mut linkage_source := active_source
	mut preprocessed_complete_context := false
	if preprocessed := cache_preprocessed_native_input(path, replay_context, c_flags, ccompiler, target) {
		linkage_source = preprocessed
		preprocessed_complete_context = true
	} else if !active_complete {
		if os.getenv('V3_CACHE_TRACE') != '' {
			eprintln('  V3 module cache unresolved public native replay guard: path=${path}')
		}
		return true
	}
	// Include identifiers from active project headers reached transitively by the
	// root. The preprocessor output contains their definitions too; filtering only
	// against the raw root would miss an external definition in a child header and
	// replay it into every cached dependent.
	file_scope := c_source_file_scope_identifiers(active_source)
	all_functions, functions_complete :=
		modulecache.c_source_function_identifiers_with_status(linkage_source)
	static_functions, static_complete :=
		modulecache.c_source_static_function_identifiers_with_status(linkage_source)
	if !functions_complete || !static_complete {
		if os.getenv('V3_CACHE_TRACE') != '' {
			eprintln('  V3 module cache incomplete public native replay scan: path=${path} functions=${functions_complete} static=${static_complete}')
		}
		return true
	}
	for name, present in all_functions {
		// An exact compiler-preprocessed source is also the conservative fallback when
		// the lightweight guard evaluator is incomplete. In that case do not filter
		// definitions by its partial file-scope view: any surviving external definition,
		// including one reached through an unresolved project-header branch, must disable
		// replay. Compiler/system inline helpers with internal linkage remain excluded by
		// the expanded static-function scan above.
		if present && !static_functions[name]
			&& (file_scope[name] || (preprocessed_complete_context && !active_complete)) {
			if os.getenv('V3_CACHE_TRACE') != '' {
				eprintln('  V3 module cache replayed external native definition: path=${path} identifier=${name}')
			}
			return true
		}
	}
	return false
}

fn cache_native_implementation_context_macros(path string, context []string, allowed_paths map[string]bool, c_flags []string, ccompiler string, target pref.Target) map[string]bool {
	mut implementation_macros := map[string]bool{}
	mut full_macros := cache_local_c_compiler_macros(c_flags, ccompiler, target)
	cache_apply_native_root_context(context, mut full_macros)
	mut full_active_paths := map[string]bool{}
	full_source, _ := cache_c_source_definitely_active_code_for_path_with_status(path, allowed_paths, mut full_active_paths, mut full_macros, false)
	full_identifiers := cache_native_implementation_identifiers(full_source)
	if full_identifiers.len == 0 {
		return implementation_macros
	}
	for omitted_line in context {
		directive, arg := cache_local_c_directive(omitted_line)
		if directive != 'define' {
			continue
		}
		name := cache_local_c_define_name(arg)
		if name.len == 0 {
			continue
		}
		mut candidate_macros := cache_local_c_compiler_macros(c_flags, ccompiler, target)
		for line in context {
			if line == omitted_line {
				continue
			}
			cache_apply_native_root_context([line], mut candidate_macros)
		}
		mut candidate_active_paths := map[string]bool{}
		candidate_source, _ := cache_c_source_definitely_active_code_for_path_with_status(path, allowed_paths, mut candidate_active_paths, mut candidate_macros, false)
		candidate_identifiers := cache_native_implementation_identifiers(candidate_source)
		if full_identifiers.keys().any(!candidate_identifiers[it]) {
			implementation_macros[name] = true
		}
	}
	return implementation_macros
}

fn cache_native_implementation_identifiers(source string) map[string]bool {
	mut identifiers, _ := modulecache.c_source_function_identifiers_with_status(source)
	variables, _ := modulecache.c_source_static_variable_identifiers(source)
	for identifier, present in variables {
		if present {
			identifiers[identifier] = true
		}
	}
	return identifiers
}

fn cache_native_type_declarations_for_path(path string, allowed_paths map[string]bool, mut include_macros map[string]V3CacheLocalCMacro) (string, bool) {
	real_path := os.real_path(path)
	if !allowed_paths[real_path] {
		return '', false
	}
	mut active_paths := map[string]bool{}
	mut extraction := V3CacheNativeTypeExtractionState{
		complete: true
	}
	declarations := cache_native_type_declarations_for_path_rec(real_path, allowed_paths, mut active_paths, mut include_macros, false, mut extraction)
	return declarations, extraction.complete
}

struct V3CacheNativeTypeExtractionState {
mut:
	complete bool
}

struct V3CacheLocalCMacro {
	known       bool
	is_defined  bool
	literal     string
	replacement string
	truth       int
}

struct V3CacheLocalCConditional {
	parent_inactive bool
mut:
	condition int
	inactive  bool
	ambiguous bool
}

fn cache_native_type_declarations_for_path_rec(path string, allowed_paths map[string]bool, mut active_paths map[string]bool, mut include_macros map[string]V3CacheLocalCMacro, ambient_ambiguous bool, mut extraction V3CacheNativeTypeExtractionState) string {
	real_path := os.real_path(path)
	if !allowed_paths[real_path] || active_paths[real_path] {
		return ''
	}
	source := os.read_file(real_path) or { return '' }
	cache_seed_locally_defined_c_macros(source, mut include_macros)
	active_paths[real_path] = true
	header, types_complete := modulecache.c_source_type_declarations_with_status(source)
	if !types_complete {
		extraction.complete = false
	}
	mut out := strings.new_builder(header.len)
	mut conditionals := []V3CacheLocalCConditional{}
	mut in_block_comment := false
	for line in header.split_into_lines() {
		directive, arg, next_block_comment := cache_local_c_directive_outside_comments(line, in_block_comment)
		in_block_comment = next_block_comment
		if directive in ['if', 'ifdef', 'ifndef'] {
			parent_inactive := conditionals.any(it.inactive)
			parent_ambiguous := conditionals.any(it.ambiguous)
			condition := cache_local_c_known_condition(directive, arg, include_macros)
			conditionals << V3CacheLocalCConditional{
				parent_inactive: parent_inactive
				condition: condition
				inactive: parent_inactive || condition < 0
				ambiguous: parent_ambiguous || condition == 0
			}
			out.writeln(line)
			continue
		}
		if directive in ['else', 'elif'] && conditionals.len > 0 {
			conditional_idx := conditionals.len - 1
			mut conditional := conditionals[conditional_idx]
			if directive == 'else' {
				conditional.inactive = conditional.parent_inactive || conditional.condition > 0
			} else if conditional.condition > 0 {
				conditional.inactive = true
			} else {
				next_condition := cache_local_c_known_condition(directive, arg, include_macros)
				conditional.condition = next_condition
				conditional.ambiguous = conditional.ambiguous || next_condition == 0
				conditional.inactive = conditional.parent_inactive || next_condition < 0
			}
			conditionals[conditional_idx] = conditional
			out.writeln(line)
			continue
		}
		if directive == 'endif' {
			if conditionals.len > 0 {
				conditionals.delete_last()
			}
			out.writeln(line)
			continue
		}
		if conditionals.any(it.inactive) {
			out.writeln(line)
			continue
		}
		if directive !in ['include', 'import'] {
			cache_record_local_c_include_macro(directive, arg, ambient_ambiguous
				|| conditionals.any(it.ambiguous), mut include_macros)
			out.writeln(line)
			continue
		}
		if include_path := cache_local_c_include_path(line, real_path, allowed_paths, include_macros) {
			real_include := os.real_path(include_path)
			if allowed_paths[real_include] {
				out.write_string(cache_native_type_declarations_for_path_rec(real_include, allowed_paths, mut active_paths, mut include_macros, ambient_ambiguous
					|| conditionals.any(it.ambiguous), mut extraction))
				continue
			}
		}
		if arg.len > 0 && arg[0] !in [`"`, `<`] {
			extraction.complete = false
		}
		out.writeln(line)
	}
	result := out.str()
	active_paths.delete(real_path)
	return result
}

fn cache_local_c_include_path(line string, source_path string, allowed_paths map[string]bool, include_macros map[string]V3CacheLocalCMacro) ?string {
	directive, raw_arg := cache_local_c_directive(line)
	if directive !in ['include', 'import'] {
		return none
	}
	mut arg := raw_arg
	if arg.len == 0 {
		return none
	}
	if arg[0] !in [`"`, `<`] {
		fields := arg.fields()
		if fields.len != 1 {
			return none
		}
		macro := include_macros[fields[0]] or { return none }
		if !macro.known || !macro.is_defined || macro.literal.len == 0 {
			return none
		}
		arg = macro.literal
	}
	if arg.len < 3 {
		return none
	}
	if arg[0] == `<` {
		close := arg[1..].index_u8(`>`)
		if close < 1 {
			return none
		}
		raw_path := arg[1..close + 1]
		mut resolved := ''
		for path, allowed in allowed_paths {
			if allowed && (path == raw_path || path.ends_with('/' + raw_path)
				|| path.ends_with('\\' + raw_path)) {
				if resolved.len > 0 && resolved != path {
					return none
				}
				resolved = path
			}
		}
		if resolved.len > 0 {
			return resolved
		}
		return none
	}
	if arg[0] != `"` {
		return none
	}
	close := arg[1..].index_u8(`"`)
	if close < 1 {
		return none
	}
	raw_path := arg[1..close + 1]
	if os.is_abs_path(raw_path) {
		return raw_path
	}
	return os.join_path_single(os.dir(source_path), raw_path)
}

fn cache_record_local_c_include_macro(directive string, arg string, ambiguous bool, mut include_macros map[string]V3CacheLocalCMacro) {
	if directive == 'undef' {
		fields := arg.fields()
		if fields.len > 0 {
			include_macros[fields[0]] = V3CacheLocalCMacro{
				known: !ambiguous
				is_defined: false
				truth: -1
			}
		}
		return
	}
	if directive != 'define' {
		return
	}
	name := cache_local_c_define_name(arg)
	if name.len == 0 {
		return
	}
	fields := arg.fields()
	raw_name := fields[0]
	open := raw_name.index_u8(`(`)
	value := if open < 0 { arg[raw_name.len..].trim_space() } else { '' }
	mut literal := ''
	if cache_local_c_is_literal_include_value(value) {
		literal = value
	}
	include_macros[name] = V3CacheLocalCMacro{
		known: !ambiguous
		is_defined: true
		literal: literal
		replacement: value
		truth: cache_local_c_integer_condition(value)
	}
}

fn cache_local_c_define_name(arg string) string {
	fields := arg.fields()
	if fields.len == 0 {
		return ''
	}
	raw_name := fields[0]
	open := raw_name.index_u8(`(`)
	return if open > 0 { raw_name[..open] } else { raw_name }
}

fn cache_seed_locally_defined_c_macros(source string, mut macros map[string]V3CacheLocalCMacro) {
	mut in_block_comment := false
	for line in source.split_into_lines() {
		directive, arg, next_block_comment := cache_local_c_directive_outside_comments(line, in_block_comment)
		in_block_comment = next_block_comment
		if directive != 'define' {
			continue
		}
		name := cache_local_c_define_name(arg)
		if name.len > 0 && name !in macros {
			macros[name] = V3CacheLocalCMacro{
				known: true
				is_defined: false
				truth: -1
			}
		}
	}
}

fn cache_local_c_flag_macros(flags []string) map[string]V3CacheLocalCMacro {
	mut macros := map[string]V3CacheLocalCMacro{}
	mut i := 0
	for i < flags.len {
		flag := flags[i].trim_space()
		mut definition := ''
		mut undefinition := ''
		if flag == '-D' && i + 1 < flags.len {
			i++
			definition = flags[i].trim_space()
		} else if flag.starts_with('-D') {
			definition = flag[2..].trim_space()
		} else if flag == '-U' && i + 1 < flags.len {
			i++
			undefinition = flags[i].trim_space()
		} else if flag.starts_with('-U') {
			undefinition = flag[2..].trim_space()
		}
		if definition.len > 0 {
			declarator := definition.all_before('=').trim_space()
			open := declarator.index_u8(`(`)
			name := if open > 0 { declarator[..open].trim_space() } else { declarator }
			value := if definition.contains('=') {
				definition.all_after('=').trim_space()
			} else {
				'1'
			}
			if name.len > 0 {
				macros[name] = V3CacheLocalCMacro{
					known: true
					is_defined: true
					literal: if cache_local_c_is_literal_include_value(value) {
						value
					} else {
						''
					}
					replacement: if open > 0 { '' } else { value }
					truth: if open > 0 { 0 } else { cache_local_c_integer_condition(value) }
				}
			}
		} else if undefinition.len > 0 {
			macros[undefinition] = V3CacheLocalCMacro{
				known: true
				is_defined: false
				truth: -1
			}
		}
		i++
	}
	return macros
}

fn cache_local_c_compiler_macros(flags []string, ccompiler string, target pref.Target) map[string]V3CacheLocalCMacro {
	mut macros := map[string]V3CacheLocalCMacro{}
	compiler_names := ['__clang__', '__GNUC__', '_MSC_VER', '__TINYC__']
	target_names := ['__APPLE__', '__MACH__', '__linux__', '__ANDROID__', '_WIN32', '_WIN64',
		'__FreeBSD__', '__OpenBSD__', '__NetBSD__', '__DragonFly__', '__EMSCRIPTEN__', '__x86_64__',
		'__amd64__', '__i386__', '__aarch64__', '__arm64__', '__arm__', '__riscv', '__powerpc64__',
		'__ppc64__', '__s390x__', '__loongarch64', '__wasm__', '__wasm32__', '_M_X64', '_M_AMD64',
		'_M_IX86', '_M_ARM', '_M_ARM64', '__LP64__', '_LP64', '__ILP32__']
	for name in compiler_names {
		macros[name] = V3CacheLocalCMacro{
			known: true
			is_defined: false
			truth: -1
		}
	}
	for name in target_names {
		macros[name] = V3CacheLocalCMacro{
			known: true
			is_defined: false
			truth: -1
		}
	}
	mut defined := []string{}
	match ccompiler {
		'clang' {
			defined = ['__clang__', '__GNUC__']
		}
		'gcc', 'mingw', 'cplusplus' {
			defined = ['__GNUC__']
		}
		'msvc' {
			defined = ['_MSC_VER']
		}
		'tinyc' {
			defined = ['__TINYC__']
		}
		else {
			macros.clear()
		}
	}
	if macros.len > 0 {
		match target.os {
			'macos', 'ios' {
				defined << ['__APPLE__', '__MACH__']
			}
			'linux' {
				defined << '__linux__'
			}
			'android', 'termux' {
				defined << ['__linux__', '__ANDROID__']
			}
			'windows' {
				defined << '_WIN32'
				if target.pointer_bits == 64 {
					defined << '_WIN64'
				}
			}
			'freebsd' {
				defined << '__FreeBSD__'
			}
			'openbsd' {
				defined << '__OpenBSD__'
			}
			'netbsd' {
				defined << '__NetBSD__'
			}
			'dragonfly' {
				defined << '__DragonFly__'
			}
			'wasm32_emscripten' {
				defined << '__EMSCRIPTEN__'
			}
			else {}
		}
		if ccompiler == 'msvc' {
			match target.arch {
				'amd64' { defined << ['_M_X64', '_M_AMD64'] }
				'x86' { defined << '_M_IX86' }
				'arm64' { defined << '_M_ARM64' }
				'arm32' { defined << '_M_ARM' }
				else {}
			}
		} else {
			match target.arch {
				'amd64' {
					defined << ['__x86_64__', '__amd64__']
				}
				'x86' {
					defined << '__i386__'
				}
				'arm64' {
					defined << '__aarch64__'
					if target.os in ['macos', 'ios'] {
						defined << '__arm64__'
					}
				}
				'arm32' {
					defined << '__arm__'
				}
				'riscv64' {
					defined << '__riscv'
				}
				'ppc64', 'ppc64le' {
					defined << ['__powerpc64__', '__ppc64__']
				}
				's390x' {
					defined << '__s390x__'
				}
				'loongarch64' {
					defined << '__loongarch64'
				}
				'wasm32' {
					defined << ['__wasm__', '__wasm32__']
				}
				else {}
			}
			if target.pointer_bits == 64 && target.os != 'windows' {
				defined << ['__LP64__', '_LP64']
			} else if target.pointer_bits == 32 {
				defined << '__ILP32__'
			}
		}
	}
	for name in defined {
		macros[name] = V3CacheLocalCMacro{
			known: true
			is_defined: true
			truth: 1
		}
	}
	for name, macro in cache_local_c_flag_macros(flags) {
		macros[name] = macro
	}
	return macros
}

fn cache_local_c_is_literal_include_value(value string) bool {
	return value.len >= 3 && ((value[0] == `"` && value[1..].index_u8(`"`) >= 1)
		|| (value[0] == `<` && value[1..].index_u8(`>`) >= 1))
}

fn cache_local_c_integer_condition(raw string) int {
	value := cache_local_c_integer_value(raw) or { return 0 }
	return if value == 0 { -1 } else { 1 }
}

fn cache_local_c_integer_value(raw string) ?i64 {
	mut clean := raw.trim_space().trim_right('uUlL')
	if clean.len == 0 {
		return none
	}
	if clean.starts_with('+') {
		clean = clean[1..]
	}
	return strconv.parse_int(clean, 0, 64) or { return none }
}

fn cache_local_c_condition_without_outer_parens(raw string) string {
	mut expression := raw.trim_space()
	for expression.len >= 2 && expression[0] == `(` && expression[expression.len - 1] == `)` {
		mut depth := 0
		mut closes_at_end := false
		for i, c in expression.bytes() {
			if c == `(` {
				depth++
			} else if c == `)` {
				depth--
				if depth == 0 {
					closes_at_end = i == expression.len - 1
					break
				}
			}
		}
		if !closes_at_end {
			break
		}
		expression = expression[1..expression.len - 1].trim_space()
	}
	return expression
}

fn cache_local_c_condition_top_level_parts(expression string, operator string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	mut i := 0
	for i + 1 < expression.len {
		if expression[i] == `(` {
			depth++
			i++
			continue
		}
		if expression[i] == `)` {
			depth--
			i++
			continue
		}
		if depth == 0 && expression[i..i + 2] == operator {
			part := expression[start..i].trim_space()
			if part.len == 0 {
				return [expression]
			}
			parts << part
			i += 2
			start = i
			continue
		}
		i++
	}
	if parts.len == 0 {
		return [expression]
	}
	last := expression[start..].trim_space()
	if last.len == 0 {
		return [expression]
	}
	parts << last
	return parts
}

fn cache_local_c_condition_top_level_comparison(expression string) (bool, string, string, string) {
	mut depth := 0
	mut found_at := -1
	mut found_operator := ''
	mut i := 0
	for i < expression.len {
		if expression[i] in [`"`, `'`] {
			quote := expression[i]
			i++
			for i < expression.len {
				if expression[i] == `\\` && i + 1 < expression.len {
					i += 2
					continue
				}
				i++
				if expression[i - 1] == quote {
					break
				}
			}
			continue
		}
		if expression[i] == `(` {
			depth++
			i++
			continue
		}
		if expression[i] == `)` {
			depth--
			i++
			continue
		}
		if depth != 0 {
			i++
			continue
		}
		mut operator := ''
		if i + 1 < expression.len && expression[i..i + 2] in ['==', '!=', '<=', '>='] {
			operator = expression[i..i + 2]
		} else if expression[i] in [`<`, `>`]
			&& (i + 1 >= expression.len || expression[i + 1] != expression[i]) {
			operator = expression[i..i + 1]
		}
		if operator.len == 0 {
			i++
			continue
		}
		if found_at >= 0 {
			return false, '', '', ''
		}
		found_at = i
		found_operator = operator
		i += operator.len
	}
	if found_at < 0 {
		return false, '', '', ''
	}
	left := expression[..found_at].trim_space()
	right := expression[found_at + found_operator.len..].trim_space()
	if left.len == 0 || right.len == 0 {
		return false, '', '', ''
	}
	return true, left, found_operator, right
}

fn cache_local_c_condition_top_level_arithmetic(expression string, operators []u8) (bool, string, u8, string) {
	mut depth := 0
	mut operator_index := -1
	mut selected_operator := u8(0)
	mut i := 0
	for i < expression.len {
		c := expression[i]
		if c in [`"`, `'`] {
			quote := c
			i++
			for i < expression.len {
				if expression[i] == `\\` && i + 1 < expression.len {
					i += 2
					continue
				}
				i++
				if expression[i - 1] == quote {
					break
				}
			}
			continue
		}
		if c == `(` {
			depth++
			i++
			continue
		}
		if c == `)` {
			depth--
			i++
			continue
		}
		if depth != 0 || c !in operators {
			i++
			continue
		}
		mut previous := i - 1
		for previous >= 0 && expression[previous].is_space() {
			previous--
		}
		if previous >= 0 && (expression[previous].is_alnum()
			|| expression[previous] == `_` || expression[previous] in [`)`, `'`]) {
			operator_index = i
			selected_operator = c
		}
		i++
	}
	if operator_index < 0 {
		return false, '', u8(0), ''
	}
	left := expression[..operator_index].trim_space()
	right := expression[operator_index + 1..].trim_space()
	if left.len == 0 || right.len == 0 {
		return false, '', u8(0), ''
	}
	return true, left, selected_operator, right
}

fn cache_local_c_checked_arithmetic(left i64, right i64, operator u8) ?i64 {
	max_value := i64(0x7fffffffffffffff)
	min_value := i64(-0x7fffffffffffffff - 1)
	match operator {
		`+` {
			if (right > 0 && left > max_value - right) || (right < 0 && left < min_value - right) {
				return none
			}
			return left + right
		}
		`-` {
			if (right > 0 && left < min_value + right) || (right < 0 && left > max_value + right) {
				return none
			}
			return left - right
		}
		`*` {
			if (left > 0 && right > 0 && left > max_value / right)
				|| (left > 0 && right < 0 && right < min_value / left)
				|| (left < 0 && right > 0 && left < min_value / right)
				|| (left < 0 && right < 0 && left < max_value / right) {
				return none
			}
			return left * right
		}
		`/` {
			if right == 0 || (left == min_value && right == -1) {
				return none
			}
			return left / right
		}
		`%` {
			if right == 0 || (left == min_value && right == -1) {
				return none
			}
			return left % right
		}
		else {
			return none
		}
	}
}

fn cache_local_c_defined_macro_name(expression string) ?string {
	if !expression.starts_with('defined') || (expression.len > 'defined'.len
		&& expression['defined'.len] != `(` && !expression['defined'.len].is_space()) {
		return none
	}
	rest := expression['defined'.len..].trim_space()
	if rest.starts_with('(') {
		close := rest.index_u8(`)`)
		if close <= 1 || rest[close + 1..].trim_space().len > 0 {
			return none
		}
		return rest[1..close].trim_space()
	}
	fields := rest.fields()
	if fields.len != 1 {
		return none
	}
	return fields[0]
}

fn cache_local_c_known_integer_value_rec(raw string, macros map[string]V3CacheLocalCMacro, mut seen map[string]bool, depth int) ?i64 {
	if depth >= 64 {
		return none
	}
	expression := cache_local_c_condition_without_outer_parens(raw)
	if value := cache_local_c_integer_value(expression) {
		return value
	}
	for operators in [[u8(`+`), `-`], [u8(`*`), `/`, `%`]] {
		has_operator, left_text, operator, right_text := cache_local_c_condition_top_level_arithmetic(expression, operators)
		if !has_operator {
			continue
		}
		left := cache_local_c_known_integer_value_rec(left_text, macros, mut seen, depth + 1) or {
			return none
		}
		right := cache_local_c_known_integer_value_rec(right_text, macros, mut seen, depth + 1) or {
			return none
		}
		return cache_local_c_checked_arithmetic(left, right, operator)
	}
	if expression.starts_with('!') {
		value := cache_local_c_known_integer_value_rec(expression[1..], macros, mut seen, depth + 1) or {
			return none
		}
		return if value == 0 { i64(1) } else { i64(0) }
	}
	if macro_name := cache_local_c_defined_macro_name(expression) {
		condition := cache_local_c_macro_condition(macro_name, false, macros)
		if condition == 0 {
			return none
		}
		return if condition > 0 { i64(1) } else { i64(0) }
	}
	macro := macros[expression] or { return none }
	if !macro.known {
		return none
	}
	if !macro.is_defined {
		return i64(0)
	}
	if macro.replacement.len == 0 || seen[expression] {
		return none
	}
	seen[expression] = true
	replacement := macro.replacement
	value := cache_local_c_known_integer_value_rec(replacement, macros, mut seen, depth + 1)
	seen.delete(expression)
	return value
}

fn cache_local_c_known_expression(raw string, macros map[string]V3CacheLocalCMacro) int {
	mut seen := map[string]bool{}
	return cache_local_c_known_expression_rec(raw, macros, mut seen, 0)
}

fn cache_local_c_known_expression_rec(raw string, macros map[string]V3CacheLocalCMacro, mut seen map[string]bool, depth int) int {
	if depth >= 64 {
		return 0
	}
	expression := cache_local_c_condition_without_outer_parens(raw)
	or_parts := cache_local_c_condition_top_level_parts(expression, '||')
	if or_parts.len > 1 {
		mut all_false := true
		for part in or_parts {
			condition := cache_local_c_known_expression_rec(part, macros, mut seen, depth + 1)
			if condition == 1 {
				return 1
			}
			all_false = all_false && condition == -1
		}
		return if all_false { -1 } else { 0 }
	}
	and_parts := cache_local_c_condition_top_level_parts(expression, '&&')
	if and_parts.len > 1 {
		mut all_true := true
		for part in and_parts {
			condition := cache_local_c_known_expression_rec(part, macros, mut seen, depth + 1)
			if condition == -1 {
				return -1
			}
			all_true = all_true && condition == 1
		}
		return if all_true { 1 } else { 0 }
	}
	has_comparison, left_text, operator, right_text :=
		cache_local_c_condition_top_level_comparison(expression)
	if has_comparison {
		left := cache_local_c_known_integer_value_rec(left_text, macros, mut seen, depth + 1) or {
			return 0
		}
		right := cache_local_c_known_integer_value_rec(right_text, macros, mut seen, depth + 1) or {
			return 0
		}
		if operator in ['<', '<=', '>', '>='] && (left < 0 || right < 0) {
			return 0
		}
		active := match operator {
			'==' { left == right }
			'!=' { left != right }
			'<' { left < right }
			'<=' { left <= right }
			'>' { left > right }
			'>=' { left >= right }
			else {
				return 0
			}
		}
		return if active { 1 } else { -1 }
	}
	if value := cache_local_c_known_integer_value_rec(expression, macros, mut seen, depth + 1) {
		return if value == 0 { -1 } else { 1 }
	}
	if expression.starts_with('!') {
		condition := cache_local_c_known_expression_rec(expression[1..], macros, mut seen, depth + 1)
		return if condition == 0 { 0 } else { -condition }
	}
	literal_condition := cache_local_c_integer_condition(expression)
	if literal_condition != 0 {
		return literal_condition
	}
	if macro := macros[expression] {
		if !macro.known {
			return 0
		}
		if !macro.is_defined {
			return -1
		}
		if macro.truth != 0 {
			return macro.truth
		}
		if macro.replacement.len == 0 || seen[expression] {
			return 0
		}
		seen[expression] = true
		replacement := macro.replacement
		condition := cache_local_c_known_expression_rec(replacement, macros, mut seen, depth + 1)
		seen.delete(expression)
		return condition
	}
	macro_name := cache_local_c_defined_macro_name(expression) or { return 0 }
	return cache_local_c_macro_condition(macro_name, false, macros)
}

fn cache_local_c_known_condition(directive string, raw_arg string, include_macros map[string]V3CacheLocalCMacro) int {
	expression := raw_arg.trim_space()
	if directive in ['ifdef', 'ifndef'] {
		return cache_local_c_macro_condition(expression, directive == 'ifndef', include_macros)
	}
	return cache_local_c_known_expression(expression, include_macros)
}

fn cache_local_c_macro_condition(name string, invert bool, include_macros map[string]V3CacheLocalCMacro) int {
	macro := include_macros[name] or { return 0 }
	if !macro.known {
		return 0
	}
	result := if macro.is_defined { 1 } else { -1 }
	return if invert { -result } else { result }
}

fn cache_local_c_directive(line string) (string, string) {
	trimmed := line.trim_space()
	if trimmed.len < 2 || trimmed[0] != `#` {
		return '', ''
	}
	rest := trimmed[1..].trim_space()
	mut end := 0
	for end < rest.len && (rest[end].is_alnum() || rest[end] == `_`) {
		end++
	}
	if end == 0 {
		return '', ''
	}
	return rest[..end], rest[end..].trim_space()
}

fn cache_local_c_directive_outside_comments(line string, starts_in_block_comment bool) (string, string, bool) {
	mut directive_at := -1
	mut at_line_start := true
	mut in_block_comment := starts_in_block_comment
	mut i := 0
	for i < line.len {
		if in_block_comment {
			for i + 1 < line.len && (line[i] != `*` || line[i + 1] != `/`) {
				i++
			}
			if i + 1 >= line.len {
				return '', '', true
			}
			in_block_comment = false
			i += 2
			continue
		}
		if i + 1 < line.len && line[i] == `/` && line[i + 1] == `/` {
			break
		}
		if i + 1 < line.len && line[i] == `/` && line[i + 1] == `*` {
			in_block_comment = true
			i += 2
			continue
		}
		c := line[i]
		if c in [`'`, `"`] {
			at_line_start = false
			quote := c
			i++
			for i < line.len {
				if line[i] == `\\` && i + 1 < line.len {
					i += 2
					continue
				}
				i++
				if line[i - 1] == quote {
					break
				}
			}
			continue
		}
		if at_line_start && c.is_space() {
			i++
			continue
		}
		if at_line_start && c == `#` {
			directive_at = i
		}
		at_line_start = false
		i++
	}
	if directive_at < 0 {
		return '', '', in_block_comment
	}
	directive, arg := cache_local_c_directive(line[directive_at..])
	return directive, arg, in_block_comment
}

// V3CacheActiveCSourceScan retains a possible-code view after the first unresolved guard,
// so native declaration discovery can detect definitions omitted from the active view.
struct V3CacheActiveCSourceScan {
	active        string
	possible      string
	has_ambiguity bool
}

fn cache_c_source_native_declarations_complete(scan V3CacheActiveCSourceScan) bool {
	if !scan.has_ambiguity {
		return true
	}
	active_functions, active_complete :=
		modulecache.c_source_function_identifiers_with_status(scan.active)
	possible_functions, possible_complete :=
		modulecache.c_source_function_identifiers_with_status(scan.possible)
	if !active_complete || !possible_complete {
		return false
	}
	for name, declared in possible_functions {
		if declared && !active_functions[name] {
			return false
		}
	}
	return true
}

fn cache_c_source_definitely_active_code(source string, mut macros map[string]V3CacheLocalCMacro) string {
	mut active_paths := map[string]bool{}
	scan := cache_c_source_definitely_active_code_rec(source, '', map[string]bool{}, mut active_paths, mut macros, false)
	return scan.active
}

fn cache_c_source_definitely_active_code_with_status(source string, mut macros map[string]V3CacheLocalCMacro) (string, bool) {
	mut active_paths := map[string]bool{}
	scan := cache_c_source_definitely_active_code_rec(source, '', map[string]bool{}, mut active_paths, mut macros, false)
	return scan.active, cache_c_source_native_declarations_complete(scan)
}

fn cache_c_source_definitely_active_code_for_path(path string, allowed_paths map[string]bool, mut active_paths map[string]bool, mut macros map[string]V3CacheLocalCMacro, ambient_ambiguous bool) string {
	return cache_c_source_active_code_scan_for_path(path, allowed_paths, mut active_paths, mut macros, ambient_ambiguous).active
}

fn cache_c_source_definitely_active_code_for_path_with_status(path string, allowed_paths map[string]bool, mut active_paths map[string]bool, mut macros map[string]V3CacheLocalCMacro, ambient_ambiguous bool) (string, bool) {
	scan := cache_c_source_active_code_scan_for_path(path, allowed_paths, mut active_paths, mut macros, ambient_ambiguous)
	return scan.active, cache_c_source_native_declarations_complete(scan)
}

fn cache_c_source_active_code_scan_for_path(path string, allowed_paths map[string]bool, mut active_paths map[string]bool, mut macros map[string]V3CacheLocalCMacro, ambient_ambiguous bool) V3CacheActiveCSourceScan {
	real_path := os.real_path(path)
	if !allowed_paths[real_path] || active_paths[real_path] {
		return V3CacheActiveCSourceScan{}
	}
	source := os.read_file(real_path) or { return V3CacheActiveCSourceScan{} }
	active_paths[real_path] = true
	result := cache_c_source_definitely_active_code_rec(source, real_path, allowed_paths, mut active_paths, mut macros, ambient_ambiguous)
	active_paths.delete(real_path)
	return result
}

fn cache_c_source_definitely_active_code_rec(source string, source_path string, allowed_paths map[string]bool, mut active_paths map[string]bool, mut macros map[string]V3CacheLocalCMacro, ambient_ambiguous bool) V3CacheActiveCSourceScan {
	cache_seed_locally_defined_c_macros(source, mut macros)
	mut out := strings.new_builder(source.len)
	mut possible := strings.new_builder(256)
	mut has_ambiguity := false
	mut conditionals := []V3CacheLocalCConditional{}
	mut in_block_comment := false
	for line in source.split_into_lines() {
		directive, arg, next_block_comment := cache_local_c_directive_outside_comments(line, in_block_comment)
		in_block_comment = next_block_comment
		if directive in ['if', 'ifdef', 'ifndef'] {
			parent_inactive := conditionals.any(it.inactive)
			parent_ambiguous := conditionals.any(it.ambiguous)
			condition := cache_local_c_known_condition(directive, arg, macros)
			conditionals << V3CacheLocalCConditional{
				parent_inactive: parent_inactive
				condition: condition
				inactive: parent_inactive || condition < 0
				ambiguous: parent_ambiguous || condition == 0
			}
			out.writeln('')
			if has_ambiguity {
				possible.writeln('')
			}
			continue
		}
		if directive in ['else', 'elif'] && conditionals.len > 0 {
			idx := conditionals.len - 1
			mut conditional := conditionals[idx]
			if directive == 'else' {
				conditional.inactive = conditional.parent_inactive || conditional.condition > 0
			} else if conditional.condition > 0 {
				conditional.inactive = true
			} else {
				next_condition := cache_local_c_known_condition(directive, arg, macros)
				conditional.condition = next_condition
				conditional.ambiguous = conditional.ambiguous || next_condition == 0
				conditional.inactive = conditional.parent_inactive || next_condition < 0
			}
			conditionals[idx] = conditional
			out.writeln('')
			if has_ambiguity {
				possible.writeln('')
			}
			continue
		}
		if directive == 'endif' {
			if conditionals.len > 0 {
				conditionals.delete_last()
			}
			out.writeln('')
			if has_ambiguity {
				possible.writeln('')
			}
			continue
		}
		if conditionals.any(it.inactive) {
			out.writeln('')
			if has_ambiguity {
				possible.writeln('')
			}
			continue
		}
		ambiguous := ambient_ambiguous || conditionals.any(it.ambiguous)
		if source_path.len > 0 && directive in ['include', 'import'] {
			if include_path := cache_local_c_include_path(line, source_path, allowed_paths, macros) {
				real_include := os.real_path(include_path)
				if allowed_paths[real_include] {
					include_scan := cache_c_source_active_code_scan_for_path(real_include, allowed_paths, mut active_paths, mut macros, ambiguous)
					if include_scan.has_ambiguity {
						if !has_ambiguity {
							possible.write_string(out.after(0))
							has_ambiguity = true
						}
						possible.write_string(include_scan.possible)
					} else if has_ambiguity {
						possible.write_string(include_scan.active)
					}
					out.write_string(include_scan.active)
					continue
				}
			}
		}
		if directive in ['define', 'undef'] {
			cache_record_local_c_include_macro(directive, arg, ambiguous, mut macros)
		}
		if ambiguous {
			if !has_ambiguity {
				possible.write_string(out.after(0))
				has_ambiguity = true
			}
			possible.writeln(if directive.len > 0 { '' } else { line })
			out.writeln('')
		} else if directive.len > 0 {
			out.writeln('')
			if has_ambiguity {
				possible.writeln('')
			}
		} else {
			out.writeln(line)
			if has_ambiguity {
				possible.writeln(line)
			}
		}
	}
	return V3CacheActiveCSourceScan{
		active: out.str()
		possible: possible.str()
		has_ambiguity: has_ambiguity
	}
}

fn v3_external_input_key(module_name string, path string) string {
	return '${module_name}\x00${path}'
}

fn v3_sha256_hex_digest_is_valid(digest string) bool {
	return digest.len == sha256.size * 2 && digest.bytes().all(it.is_hex_digit())
}

fn v3_external_input_digests_complete(state &V3ModuleCacheState) bool {
	for paths in state.module_external_inputs.values() {
		for path in paths {
			digest := state.external_input_digests[os.real_path(path)] or { return false }
			if !v3_sha256_hex_digest_is_valid(digest) {
				return false
			}
		}
	}
	for paths in state.module_native_roots.values() {
		for path in paths {
			digest := state.external_input_digests[os.real_path(path)] or { return false }
			if !v3_sha256_hex_digest_is_valid(digest) {
				return false
			}
		}
	}
	return true
}

fn v3_external_cache_path(key string, prefix string) ?V3ExternalCachePath {
	if !key.starts_with(prefix) {
		return none
	}
	value := key[prefix.len..]
	colon := value.index_u8(`:`)
	if colon <= 0 || colon + 1 >= value.len {
		return none
	}
	return V3ExternalCachePath{
		module_name: value[..colon]
		path: value[colon + 1..]
	}
}

fn restore_v3_cache_external_inputs(mut state V3ModuleCacheState, user_files []string, user_c_flags []string, ccompiler string, target pref.Target, incremental_declaration_signature string) bool {
	base_input := v3_cgen_cache_input(state, user_files, user_c_flags)
	prefixes := ['external:', 'external-sha256:', 'external-meta:', 'external-root:',
		'external-root-owner:', 'external-context:', 'external-owner:', 'external-dir:',
		'external-missing:', 'external-state:']
	mut restored := map[string]string{}
	if exact := state.manager.cached_cgen_dependency_inputs(base_input.source_files, base_input.generation_signature, base_input.dependency_inputs, prefixes) {
		restored = exact.clone()
	} else {
		if incremental_declaration_signature.len == 0 {
			return false
		}
		restored = state.manager.cached_incremental_dependency_inputs(base_input.source_files, incremental_declaration_signature, base_input.generation_signature, base_input.dependency_inputs, prefixes) or { return false }
	}
	if restored['external-state:manifest'] or { '' } != 'v3-external-inputs-5' {
		return false
	}
	mut external_inputs := map[string][]string{}
	mut external_signatures := map[string]string{}
	mut external_digests := map[string]string{}
	for key, signature in restored {
		input := v3_external_cache_path(key, 'external:') or { continue }
		metadata_key := 'external-meta:${input.module_name}:${input.path}'
		metadata := restored[metadata_key] or { return false }
		digest_key := 'external-sha256:${input.module_name}:${input.path}'
		digest := restored[digest_key] or { return false }
		if metadata.len == 0 || modulecache.file_metadata_signature(input.path) != metadata {
			return false
		}
		if !v3_sha256_hex_digest_is_valid(digest) {
			return false
		}
		real_path := os.real_path(input.path)
		if old_digest := external_digests[real_path] {
			if old_digest != digest {
				return false
			}
		} else {
			external_digests[real_path] = digest
		}
		mut paths := external_inputs[input.module_name]
		paths << input.path
		external_inputs[input.module_name] = paths
		external_signatures[v3_external_input_key(input.module_name, input.path)] = signature
	}
	for key, _ in restored {
		if input := v3_external_cache_path(key, 'external-meta:') {
			if 'external:${input.module_name}:${input.path}' !in restored {
				return false
			}
		}
		if input := v3_external_cache_path(key, 'external-sha256:') {
			if 'external:${input.module_name}:${input.path}' !in restored {
				return false
			}
		}
	}
	mut root_records := []V3ExternalNativeRoot{}
	for key, value in restored {
		if key.starts_with('external-root-owner:') {
			continue
		}
		input := v3_external_cache_path(key, 'external-root:') or { continue }
		if input.path.len == 0 || input.path.bytes().any(!it.is_digit()) {
			return false
		}
		index := input.path.int()
		tab := value.last_index_u8(`\t`)
		if index < 0 || tab <= 0 || tab + 1 >= value.len {
			return false
		}
		path := value[..tab]
		metadata := value[tab + 1..]
		if metadata.len == 0 || modulecache.file_metadata_signature(path) != metadata
			|| path !in external_inputs[input.module_name] {
			return false
		}
		root_records << V3ExternalNativeRoot{
			module_name: input.module_name
			path: path
			index: index
		}
	}
	root_records.sort_with_compare(fn (a &V3ExternalNativeRoot, b &V3ExternalNativeRoot) int {
		if a.module_name != b.module_name {
			return a.module_name.compare(b.module_name)
		}
		return a.index - b.index
	})
	mut native_roots := map[string][]string{}
	for record in root_records {
		mut roots := native_roots[record.module_name]
		if record.index != roots.len {
			return false
		}
		roots << record.path
		native_roots[record.module_name] = roots
	}
	mut native_root_contexts := map[string][]string{}
	for key, value in restored {
		input := v3_external_cache_path(key, 'external-context:') or { continue }
		if input.path.len == 0 || input.path.bytes().any(!it.is_digit()) {
			return false
		}
		index := input.path.int()
		roots := native_roots[input.module_name] or { return false }
		if index < 0 || index >= roots.len {
			return false
		}
		native_root_contexts[roots[index]] = if value.len == 0 {
			[]string{}
		} else {
			value.split('\x1e')
		}
	}
	mut native_root_owners := map[string]string{}
	mut restored_root_owners := map[string]bool{}
	for key, owner in restored {
		input := v3_external_cache_path(key, 'external-root-owner:') or { continue }
		if input.path.len == 0 || input.path.bytes().any(!it.is_digit()) {
			return false
		}
		index := input.path.int()
		roots := native_roots[input.module_name] or { return false }
		if index < 0 || index >= roots.len {
			return false
		}
		restored_root_owners['${input.module_name}:${index}'] = true
		if owner.len > 0 {
			native_root_owners[os.real_path(roots[index])] = owner
		}
	}
	for module_name, roots in native_roots {
		for index, _ in roots {
			if !restored_root_owners['${module_name}:${index}'] {
				return false
			}
		}
	}
	mut native_source_modules := map[string]bool{}
	for key, value in restored {
		if key.starts_with('external-owner:') {
			module_name := key['external-owner:'.len..]
			if module_name.len == 0 || value != '1' {
				return false
			}
			native_source_modules[module_name] = true
		}
	}
	mut resolution_dirs := []string{}
	for key, metadata in restored {
		if key.starts_with('external-dir:') {
			path := key['external-dir:'.len..]
			if path.len == 0 || metadata.len == 0
				|| modulecache.file_metadata_signature(path) != metadata {
				return false
			}
			resolution_dirs << path
		}
	}
	mut missing_resolution_paths := []string{}
	for key, value in restored {
		if key.starts_with('external-missing:') {
			path := key['external-missing:'.len..]
			if path.len == 0 || value != 'missing' || os.exists(path) {
				return false
			}
			missing_resolution_paths << path
		}
	}
	for module_name, paths in external_inputs {
		mut sorted := paths.clone()
		sorted.sort()
		external_inputs[module_name] = sorted
	}
	resolution_dirs.sort()
	missing_resolution_paths.sort()
	state.module_external_inputs = external_inputs.clone()
	state.external_input_signatures = external_signatures.clone()
	state.external_input_digests = external_digests.clone()
	state.module_native_roots = native_roots.clone()
	state.native_root_contexts = native_root_contexts.clone()
	state.native_root_owners = native_root_owners.clone()
	state.native_source_modules = native_source_modules.clone()
	if !prepare_v3_cache_native_type_declarations(mut state, user_c_flags, ccompiler, target) {
		return false
	}
	state.external_resolution_dirs = resolution_dirs.clone()
	state.external_missing_paths = missing_resolution_paths.clone()
	state.external_inputs_ready = true
	state.external_inputs_complete = v3_external_input_digests_complete(state)
	if !state.external_inputs_complete {
		return false
	}
	return true
}

fn encode_v3_cgen_metadata(flags []string, interface_impl_signature string, prefix_source_identity string, diagnostics []V3CachedTypeDiagnostic) string {
	mut parts := ['v3-cgen-metadata-v4', interface_impl_signature, prefix_source_identity,
		flags.len.str()]
	parts << flags
	parts << diagnostics.len.str()
	for diagnostic in diagnostics {
		parts << diagnostic.file
		parts << diagnostic.msg
		parts << diagnostic.severity
		parts << diagnostic.node.str()
		parts << diagnostic.offset.str()
		parts << diagnostic.end.str()
		parts << diagnostic.reported_column.str()
		parts << diagnostic.details.len.str()
		parts << diagnostic.details
	}
	return parts.join('\x00')
}

fn decode_v3_cgen_metadata(metadata string) ?V3CgenCacheMetadata {
	parts := metadata.split('\x00')
	if parts.len < 5 || parts[0] != 'v3-cgen-metadata-v4' {
		return none
	}
	flag_count := strconv.atoi(parts[3]) or { return none }
	if flag_count < 0 || 4 + flag_count >= parts.len {
		return none
	}
	mut index := 4 + flag_count
	diagnostic_count := strconv.atoi(parts[index]) or { return none }
	if diagnostic_count < 0 {
		return none
	}
	index++
	mut diagnostics := []V3CachedTypeDiagnostic{cap: diagnostic_count}
	for _ in 0 .. diagnostic_count {
		if index + 8 > parts.len {
			return none
		}
		node := strconv.atoi(parts[index + 3]) or { return none }
		offset := strconv.atoi(parts[index + 4]) or { return none }
		end := strconv.atoi(parts[index + 5]) or { return none }
		reported_column := strconv.atoi(parts[index + 6]) or { return none }
		detail_count := strconv.atoi(parts[index + 7]) or { return none }
		if offset < 0 || end < offset || reported_column < 0 || detail_count < 0
			|| index + 8 + detail_count > parts.len {
			return none
		}
		diagnostics << V3CachedTypeDiagnostic{
			file: parts[index]
			msg: parts[index + 1]
			severity: parts[index + 2]
			node: node
			offset: offset
			end: end
			reported_column: reported_column
			details: parts[index + 8..index + 8 + detail_count].clone()
		}
		index += 8 + detail_count
	}
	if index != parts.len {
		return none
	}
	return V3CgenCacheMetadata{
		interface_impl_signature: parts[1]
		prefix_source_identity: parts[2]
		flags: parts[4..4 + flag_count].clone()
		diagnostics: diagnostics
	}
}

fn cache_v3_type_diagnostics(a &flat.FlatAst, diagnostics []types.TypeError) []V3CachedTypeDiagnostic {
	mut cached := []V3CachedTypeDiagnostic{cap: diagnostics.len}
	for diagnostic in diagnostics {
		mut file := diagnostic.file
		if diagnostic.pos.is_valid() {
			if source_file := a.source_files[diagnostic.pos.id] {
				file = source_file.name
			}
		}
		if file.len > 0 {
			file = os.real_path(file)
		}
		cached << V3CachedTypeDiagnostic{
			file: file.clone()
			msg: diagnostic.msg.clone()
			severity: diagnostic.severity.clone()
			node: int(diagnostic.node)
			offset: diagnostic.pos.offset
			end: diagnostic.pos.end
			reported_column: diagnostic.pos.reported_column()
			details: clone_string_list(diagnostic.details)
		}
	}
	return cached
}

fn restore_v3_type_diagnostics(mut a flat.FlatAst, diagnostics []V3CachedTypeDiagnostic) []types.TypeError {
	mut file_ids := map[string]int{}
	mut next_file_id := 1
	for id, file in a.source_files {
		file_ids[os.real_path(file.name)] = id
		if id >= next_file_id {
			next_file_id = id + 1
		}
	}
	mut file_set := v3token.FileSet.new()
	mut restored := []types.TypeError{cap: diagnostics.len}
	for diagnostic in diagnostics {
		mut file_id := file_ids[diagnostic.file] or { 0 }
		if file_id == 0 && diagnostic.file.len > 0 {
			source := os.read_file(diagnostic.file) or { '' }
			if source.len > 0 || os.is_file(diagnostic.file) {
				mut source_file := file_set.add_file(diagnostic.file, source.len)
				source_file.index_lines(source)
				file_id = next_file_id
				next_file_id++
				a.source_files[file_id] = source_file
				file_ids[diagnostic.file] = file_id
			}
		}
		restored << types.TypeError{
			msg: diagnostic.msg.clone()
			kind: .unknown_ident
			node: flat.NodeId(diagnostic.node)
			file: diagnostic.file.clone()
			pos: v3token.new_span(file_id, diagnostic.offset, diagnostic.end).with_reported_column(diagnostic.reported_column)
			details: clone_string_list(diagnostic.details)
			severity: diagnostic.severity.clone()
		}
	}
	return restored
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
				|| (current.kind == .call && (current.value.starts_with('\$')
					|| current.value in ['embed_file', 'tmpl', 'env', 'd', 'res', 'pkgconfig',
						'compile_error', 'compile_warn']))
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

fn monomorph_cache_runtime_strings(a &flat.FlatAst, source_files []string) []string {
	cacheable := cacheable_runtime_string_nodes(a)
	mut source_paths := map[string]bool{}
	for path in source_files {
		source_paths[os.real_path(path)] = true
	}
	mut source_nodes := []bool{len: a.nodes.len}
	mut stack := []flat.NodeId{cap: 256}
	for idx, node in a.nodes {
		if node.kind != .file || os.real_path(node.value) !in source_paths {
			continue
		}
		stack << flat.NodeId(idx)
		for stack.len > 0 {
			id := stack.pop()
			node_idx := int(id)
			if node_idx < 0 || node_idx >= a.nodes.len || source_nodes[node_idx] {
				continue
			}
			source_nodes[node_idx] = true
			current := a.nodes[node_idx]
			for child_idx in 0 .. current.children_count {
				stack << a.child(&current, child_idx)
			}
		}
	}
	mut values := []string{}
	for idx, can_cache in cacheable {
		if can_cache && source_nodes[idx] {
			values << a.nodes[idx].value
		}
	}
	return values
}

fn monomorph_cache_semantic_signature(a &flat.FlatAst, source_files []string) string {
	cacheable_strings := cacheable_runtime_string_nodes(a)
	declaration_attributes := incremental_declaration_attribute_signatures(a)
	mut hash := u64(1469598103934665603)
	mut source_paths := map[string]bool{}
	mut source_function_names := map[string]bool{}
	for path in source_files {
		source_paths[os.real_path(path)] = true
	}
	mut file_ids := []int{}
	for idx, node in a.nodes {
		if node.kind == .file && os.real_path(node.value) in source_paths {
			file_ids << idx
		}
	}
	// Keep the self-hosting path capture-free so V can be built with `-no-closures`.
	for i in 1 .. file_ids.len {
		value := file_ids[i]
		mut j := i
		for j > 0 && a.nodes[file_ids[j - 1]].value > a.nodes[value].value {
			file_ids[j] = file_ids[j - 1]
			j--
		}
		file_ids[j] = value
	}
	for idx in file_ids {
		hash = c_hash_monomorph_node(hash, a, flat.NodeId(idx), cacheable_strings, declaration_attributes)
		file_node := a.nodes[idx]
		mut module_name := ''
		for child_idx in 0 .. file_node.children_count {
			child := a.nodes[int(a.child(&file_node, child_idx))]
			if child.kind == .module_decl {
				module_name = child.value
				continue
			}
			if child.kind != .fn_decl {
				continue
			}
			source_function_names[child.value] = true
			if module_name.len > 0 {
				source_function_names['${module_name}.${child.value}'] = true
			}
		}
	}
	hash = c_hash_function_metadata_for_names(hash, a, source_function_names)
	return hash.hex()
}

fn c_hash_monomorph_node(initial u64, a &flat.FlatAst, id flat.NodeId, cacheable_strings []bool, declaration_attributes map[int]string) u64 {
	idx := int(id)
	if idx < 0 || idx >= a.nodes.len {
		return initial
	}
	node := a.nodes[idx]
	mut hash := c_hash_bytes(initial, [u8(node.kind), u8(node.op), u8(node.is_mut),
		u8(node.skip_ownership_drops)])
	hash = c_hash_tag(hash, node.children_count)
	hash = c_hash_bytes(hash, node.typ.bytes())
	hash = c_hash_bytes(hash, [u8(0)])
	if idx >= cacheable_strings.len || !cacheable_strings[idx] {
		hash = c_hash_bytes(hash, node.value.bytes())
	}
	hash = c_hash_bytes(hash, [u8(0xff)])
	if attribute_signature := declaration_attributes[idx] {
		hash = c_hash_bytes(hash, attribute_signature.bytes())
		hash = c_hash_bytes(hash, [u8(0xfd)])
	}
	for param in node.generic_params() {
		hash = c_hash_bytes(hash, param.bytes())
		hash = c_hash_bytes(hash, [u8(0xfe)])
	}
	for child_idx in 0 .. node.children_count {
		hash = c_hash_monomorph_node(hash, a, a.child(&node, child_idx), cacheable_strings, declaration_attributes)
	}
	return hash
}

fn c_hash_function_metadata(initial u64, a &flat.FlatAst) u64 {
	mut names := map[string]bool{}
	for name in a.disabled_fns.keys() {
		names[name] = true
	}
	for name in a.export_fn_names.keys() {
		names[name] = true
	}
	for name in a.noreturn_fns.keys() {
		names[name] = true
	}
	return c_hash_function_metadata_for_names(initial, a, names)
}

fn c_hash_function_metadata_for_names(initial u64, a &flat.FlatAst, source_function_names map[string]bool) u64 {
	mut hash := initial
	mut disabled_names := a.disabled_fns.keys()
	disabled_names.sort()
	for name in disabled_names {
		if name !in source_function_names {
			continue
		}
		hash = c_hash_bytes(hash, name.bytes())
		hash = c_hash_bytes(hash, [u8(a.disabled_fns[name]), u8(0xfd)])
	}
	mut export_names := a.export_fn_names.keys()
	export_names.sort()
	for name in export_names {
		if name !in source_function_names {
			continue
		}
		hash = c_hash_bytes(hash, name.bytes())
		hash = c_hash_bytes(hash, [u8(0xfc)])
		hash = c_hash_bytes(hash, a.export_fn_names[name].bytes())
		hash = c_hash_bytes(hash, [u8(0xfb)])
	}
	mut noreturn_names := a.noreturn_fns.keys()
	noreturn_names.sort()
	for name in noreturn_names {
		if name !in source_function_names {
			continue
		}
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

fn incremental_declaration_attribute_signatures(a &flat.FlatAst) map[int]string {
	mut result := map[int]string{}
	for node in a.nodes {
		if node.kind != .directive || !node.value.starts_with('@attributes:') {
			continue
		}
		declaration_id := node.value['@attributes:'.len..].int()
		if declaration_id < 0 || declaration_id >= a.nodes.len {
			continue
		}
		// The marker embeds its declaration's transient node id in value. Hash only
		// the stable attribute kinds and payload, then attach it to that declaration.
		result[declaration_id] = incremental_hash_node_header(u64(1469598103934665603), &node, false).hex()
	}
	return result
}

fn incremental_program_snapshot(a &flat.FlatAst, source_files []string) V3IncrementalSnapshot {
	mut source_paths := map[string]bool{}
	for path in source_files {
		source_paths[os.real_path(path)] = true
	}
	mut module_names := map[string]string{}
	for node in a.nodes {
		if node.kind != .module_decl {
			continue
		}
		file := a.source_files[node.pos.id] or { continue }
		module_names[os.real_path(file.name)] = node.value
	}
	mut declaration_parts := []string{}
	mut ordered_import_directive_parts := []string{}
	mut global_initializer_parts := []string{}
	mut const_initializer_parts := []string{}
	mut synthetic_main_parts := []string{}
	mut functions := []V3IncrementalFn{}
	top_level_nodes := incremental_top_level_nodes(a)
	declaration_attributes := incremental_declaration_attribute_signatures(a)
	for idx, node in a.nodes {
		file := a.source_files[node.pos.id] or { continue }
		cur_file := file.name
		real_file := os.real_path(cur_file)
		if real_file !in source_paths {
			continue
		}
		cur_module := module_names[real_file] or { '' }
		match node.kind {
			.fn_decl {
				attribute_signature := declaration_attributes[idx] or { '' }
				key := incremental_cache_fn_key(cur_file, cur_module, node.value)
				functions << V3IncrementalFn{
					key: key
					name: incremental_qualified_fn_name(cur_module, node.value)
					signature: incremental_node_tree_signature(a, flat.NodeId(idx))
				}
				mut declaration := strings.new_builder(128)
				declaration.write_string('fn\t${cur_file}\t${cur_module}\t')
				declaration.write_string(incremental_hash_fn_declaration(u64(1469598103934665603), &node).hex())
				for child_idx in 0 .. node.children_count {
					child_id := a.child(&node, child_idx)
					child := a.nodes[int(child_id)]
					if child.kind == .param {
						declaration.write_u8(`\t`)
						declaration.write_string(incremental_hash_node_header(u64(1469598103934665603), &child, true).hex())
					}
				}
				declaration.write_string('\t${attribute_signature}')
				declaration_parts << declaration.str()
			}
			.struct_decl, .global_decl, .const_decl, .enum_decl, .type_decl, .interface_decl, .import_decl, .c_fn_decl {
				attribute_signature := declaration_attributes[idx] or { '' }
				part := '${node.kind}\t${cur_file}\t${cur_module}\t${incremental_node_tree_signature(a, flat.NodeId(idx))}\t${attribute_signature}'
				declaration_parts << part
				if node.kind == .import_decl {
					ordered_import_directive_parts << part
				}
				if node.kind == .global_decl {
					global_initializer_parts << part
				}
				if node.kind == .const_decl {
					const_initializer_parts << part
				}
			}
			.directive, .comptime_if, .comptime_for, .asm_stmt, .sql_expr, .fn_literal {
				if top_level_nodes[idx] {
					part := '${node.kind}\t${cur_file}\t${cur_module}\t${incremental_node_tree_signature(a, flat.NodeId(idx))}'
					declaration_parts << part
					if node.kind == .directive {
						ordered_import_directive_parts << part
					}
				}
			}
			else {
				if top_level_nodes[idx] {
					synthetic_main_parts << 'synthetic-main\t${node.kind}\t${cur_file}\t${cur_module}\t${incremental_node_tree_signature(a, flat.NodeId(idx))}'
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
	declaration_hash = c_hash_bytes(declaration_hash, 'ordered-imports-directives'.bytes())
	for part in ordered_import_directive_parts {
		declaration_hash = c_hash_bytes(declaration_hash, part.bytes())
		declaration_hash = c_hash_bytes(declaration_hash, [u8(0xff)])
	}
	declaration_hash = c_hash_bytes(declaration_hash, 'ordered-global-initializers'.bytes())
	for part in global_initializer_parts {
		declaration_hash = c_hash_bytes(declaration_hash, part.bytes())
		declaration_hash = c_hash_bytes(declaration_hash, [u8(0xff)])
	}
	declaration_hash = c_hash_bytes(declaration_hash, 'ordered-const-initializers'.bytes())
	for part in const_initializer_parts {
		declaration_hash = c_hash_bytes(declaration_hash, part.bytes())
		declaration_hash = c_hash_bytes(declaration_hash, [u8(0xff)])
	}
	declaration_hash = c_hash_bytes(declaration_hash, 'ordered-synthetic-main'.bytes())
	for part in synthetic_main_parts {
		declaration_hash = c_hash_bytes(declaration_hash, part.bytes())
		declaration_hash = c_hash_bytes(declaration_hash, [u8(0xff)])
	}
	return V3IncrementalSnapshot{
		declaration_signature: declaration_hash.hex()
		functions: functions
	}
}

fn encode_incremental_manifest(snapshot V3IncrementalSnapshot) string {
	mut lines := []string{cap: snapshot.functions.len + 1}
	lines << 'v3-incremental-functions-v3'
	for function in snapshot.functions {
		lines << '${function.key}\t${function.signature}\t${function.name}'
	}
	return lines.join('\n')
}

fn decode_incremental_manifest(encoded string) ?map[string]string {
	lines := encoded.split_into_lines()
	if lines.len == 0 || lines[0] != 'v3-incremental-functions-v3' {
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

struct V3IncrementalCFunctionSections {
	sections map[string]string
	keys     []string
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

fn incremental_changed_functions_require_reachability_rebuild(a &flat.FlatAst, tc &types.TypeChecker, mut changed_names map[string]bool, mut used map[string]bool, user_files []string) bool {
	if changed_names.len == 0 {
		return false
	}
	current, _ := markused.mark_used_with_generic_usage(a, tc)
	mut program_files := map[string]bool{}
	for file in user_files {
		program_files[file] = true
		program_files[os.real_path(file)] = true
	}
	mut cur_module := ''
	mut is_program_file := false
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
				is_program_file = program_files[node.value]
					|| program_files[os.real_path(node.value)]
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if !is_program_file {
					continue
				}
				name := incremental_qualified_fn_name(cur_module, node.value)
				aliases := [node.value, name, restored_fn_c_name(node.value),
					restored_fn_c_name(name)]
				if !aliases.any(current[it]) {
					continue
				}
				if !aliases.any(used[it]) {
					// A newly reached stringifier can be added to the incremental body without
					// invalidating unchanged functions or the cached support prefix.
					if name.ends_with('.str') {
						changed_names[node.value] = true
						changed_names[name] = true
						for alias in aliases {
							used[alias] = true
						}
						continue
					}
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn incremental_c_function_sections(source string) ?V3IncrementalCFunctionSections {
	begin_prefix := '/* V3CACHE_FN_BEGIN '
	end_prefix := '/* V3CACHE_FN_END '
	mut sections := map[string]string{}
	mut keys := []string{}
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
		keys << key
		offset = end
	}
	if sections.len == 0 {
		return none
	}
	return V3IncrementalCFunctionSections{
		sections: sections
		keys: keys
	}
}

fn merge_incremental_program_body(cached_source string, cached_prefix string, changed_source string, changed_keys []string) ?string {
	cached_sections := incremental_c_function_sections(cached_source) or { return none }
	prefix_sections := incremental_c_function_sections(cached_prefix) or {
		V3IncrementalCFunctionSections{}
	}
	changed_sections := incremental_c_function_sections(changed_source) or { return none }
	mut merged := cached_source
	for key in changed_keys {
		old_section := cached_sections.sections[key] or { return none }
		new_section := changed_sections.sections[key] or { return none }
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
	declaration_text := additions.str()
	marker := '/* V3CACHE_BODY_BEGIN */'
	marker_idx := merged.index(marker) or { return none }
	if declaration_text.len > 0 {
		merged = merged[..marker_idx] + declaration_text + merged[marker_idx..]
	}
	mut new_sections := strings.new_builder(1024)
	prefix_functions := modulecache.c_source_function_identifiers(cached_prefix)
	for key in changed_sections.keys {
		if key in cached_sections.sections || key in prefix_sections.sections {
			continue
		}
		section := changed_sections.sections[key]
		section_functions := modulecache.c_source_function_identifiers(section)
		if section_functions.len > 0 && section_functions.keys().all(it in prefix_functions) {
			continue
		}
		new_sections.write_string(section)
	}
	new_section_text := new_sections.str()
	if new_section_text.len == 0 {
		return merged
	}
	body_marker_idx := merged.index(marker) or { return none }
	mut body_start := body_marker_idx + marker.len
	if body_start < merged.len && merged[body_start] == `\n` {
		body_start++
	}
	return merged[..body_start] + new_section_text + merged[body_start..]
}

fn merge_cached_generic_program_body(cached_source string, changed_source string) ?string {
	cached_sections := incremental_c_function_sections(cached_source) or { return none }
	changed_sections := incremental_c_function_sections(changed_source) or { return none }
	mut merged := cached_source
	for key in changed_sections.keys {
		old_section := cached_sections.sections[key] or { continue }
		merged = merged.replace(old_section, changed_sections.sections[key])
	}
	support_declarations := incremental_c_support_declarations(changed_source) or { '' }
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
	marker := '/* V3CACHE_BODY_BEGIN */'
	marker_idx := merged.index(marker) or { return none }
	declaration_text := additions.str()
	if declaration_text.len > 0 {
		merged = merged[..marker_idx] + declaration_text + merged[marker_idx..]
	}
	mut new_sections := strings.new_builder(1024)
	for key in changed_sections.keys {
		if key !in cached_sections.sections {
			new_sections.write_string(changed_sections.sections[key])
		}
	}
	new_section_text := new_sections.str()
	if new_section_text.len == 0 {
		return merged
	}
	body_marker_idx := merged.index(marker) or { return none }
	mut body_start := body_marker_idx + marker.len
	if body_start < merged.len && merged[body_start] == `\n` {
		body_start++
	}
	return merged[..body_start] + new_section_text + merged[body_start..]
}

fn incremental_c_support_declarations(source string) ?string {
	begin_marker := '/* V3CACHE_SUPPORT_BEGIN */'
	end_marker := '/* V3CACHE_SUPPORT_END */'
	begin := source.index(begin_marker) or { return none }
	content_start := begin + begin_marker.len
	relative_end := source[content_start..].index(end_marker) or { return none }
	return source[content_start..content_start + relative_end]
}

fn incremental_c_cached_declarations(source string) string {
	marker := '/* V3CACHE_BODY_BEGIN */'
	marker_idx := source.index(marker) or { return '' }
	return source[..marker_idx]
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
			module: parts[1].clone()
			args: clone_string_list(raw_args)
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
			module: spec.module.clone()
			args: clone_string_list(spec.args)
		}
	}
	return cloned
}

fn merge_monomorph_cache_specs(cached []transform.MonomorphCacheSpec, generated []transform.MonomorphCacheSpec) []transform.MonomorphCacheSpec {
	mut by_key := map[string]transform.MonomorphCacheSpec{}
	for spec in cached {
		key := '${spec.decl_key}\x00${spec.module}\x00${spec.args.join('\x1f')}'
		by_key[key] = spec
	}
	for spec in generated {
		key := '${spec.decl_key}\x00${spec.module}\x00${spec.args.join('\x1f')}'
		by_key[key] = spec
	}
	mut keys := by_key.keys()
	keys.sort()
	mut merged := []transform.MonomorphCacheSpec{cap: keys.len}
	for key in keys {
		spec := by_key[key]
		merged << transform.MonomorphCacheSpec{
			decl_key: spec.decl_key.clone()
			module: spec.module.clone()
			args: clone_string_list(spec.args)
		}
	}
	return merged
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

fn clone_nested_string_bool_map(values map[string]map[string]bool) map[string]map[string]bool {
	mut cloned := map[string]map[string]bool{}
	for key, value in values {
		cloned[key.clone()] = clone_string_bool_map(value)
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

// promote_scoped_ast_nodes_flagged is the scoped-node promotion walk with an optional
// pre-computed scoped-text flag array (one byte per node id): unflagged nodes in
// the appended range are known not to hold scope-owned text and are skipped.
fn promote_scoped_ast_nodes_flagged(mut ast flat.FlatAst, base_nodes int, new_end int, owned_base_nodes []int, scope voidptr, flags []u8) {
	// When the whole-array ownership flags were computed, they cover the logged
	// base nodes too: unflagged entries hold no scope-owned text and their
	// promotion would be a no-op.
	use_flags_for_base := flags.len >= ast.nodes.len
	for idx in owned_base_nodes {
		if idx >= 0 && idx < base_nodes && idx < ast.nodes.len {
			if use_flags_for_base && flags[idx] == 0 {
				continue
			}
			promote_scoped_node(mut ast.nodes[idx], scope)
		}
	}
	limit := if new_end < ast.nodes.len { new_end } else { ast.nodes.len }
	if flags.len >= limit && base_nodes >= 0 {
		// Word-scan the flag range: flagged nodes are rare, so loading eight
		// flags per u64 makes this walk almost free.
		word_data := unsafe { &u64(flags.data) }
		mut idx := base_nodes
		for idx < limit {
			if idx % 8 == 0 && idx + 8 <= limit && unsafe { word_data[idx / 8] } == 0 {
				idx += 8
				continue
			}
			if flags[idx] != 0 {
				promote_scoped_node(mut ast.nodes[idx], scope)
			}
			idx++
		}
		return
	}
	for idx in base_nodes .. limit {
		if idx < flags.len && flags[idx] == 0 {
			continue
		}
		promote_scoped_node(mut ast.nodes[idx], scope)
	}
}

// canonicalize_scoped_node_cached is canonicalize_scoped_node with a pointer
// probe over the caller's cache arrays: owned texts repeat the same shared
// string instances heavily, so most content-hash intern lookups are skipped.
fn canonicalize_scoped_node_cached(mut ast flat.FlatAst, idx int, scope voidptr, mut cache_ptrs []voidptr, mut cache_vals []string) {
	if idx < 0 || idx >= ast.nodes.len {
		return
	}
	mut node := unsafe { &ast.nodes[idx] }
	if node.value.len > 0 && scoped_value_owned(scope, node.value.str) {
		node.value = ast.intern_text_ptr_cached(node.value, mut cache_ptrs, mut cache_vals)
	}
	if node.typ.len > 0 && scoped_value_owned(scope, node.typ.str) {
		node.typ = ast.intern_text_ptr_cached(node.typ, mut cache_ptrs, mut cache_vals)
	}
	if node.type_text_id() == 0 && node.typ.len > 0 {
		node.set_type_text_id(ast.type_text_id(node.typ))
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
			params << ast.intern_text_ptr_cached(param, mut cache_ptrs, mut cache_vals)
		} else {
			params << param
		}
	}
	node.set_generic_params(params)
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
	if node.type_text_id() == 0 && node.typ.len > 0 {
		node.set_type_text_id(ast.type_text_id(node.typ))
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
			scope: region.scope
			new_start: region.new_start
			new_end: region.new_end
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
		nodes: nodes
		children: children
		user_code_start: ast.user_code_start
		disabled_fns: ast.disabled_fns
		export_fn_names: ast.export_fn_names
		noreturn_fns: ast.noreturn_fns
		source_files: ast.source_files
		template_call_sites: ast.template_call_sites.clone()
		template_actions: clone_int_string_map(ast.template_actions)
		source_buffers: ast.source_buffers
		text_values: text_values
		text_ids: text_ids
		worker_pool: ast.worker_pool
		specialized_fn_nodes: ast.specialized_fn_nodes.clone()
		specialized_fn_modules: clone_int_string_map(ast.specialized_fn_modules)
		specialized_fn_files: clone_int_string_map(ast.specialized_fn_files)
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
				typ: types.clone_owned_type(field.typ)
				has_default: field.has_default
				is_embed: field.is_embed
				is_mut: field.is_mut
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

fn promote_scoped_type_metadata(mut tc types.TypeChecker) {
	// Transform and specialization can grow these maps inside a disposable arena,
	// so move both their storage and string payloads before releasing that arena.
	tc.fn_type_files = clone_string_string_map(tc.fn_type_files)
	tc.fn_type_modules = clone_string_string_map(tc.fn_type_modules)
	tc.structs = clone_struct_field_map(tc.structs)
	tc.struct_modules = clone_string_string_map(tc.struct_modules)
	tc.struct_files = clone_string_string_map(tc.struct_files)
	tc.soa_structs = clone_string_bool_map(tc.soa_structs)
	tc.declared_type_scope_keys = clone_string_bool_map(tc.declared_type_scope_keys)
	tc.struct_error_embeds_shadow_builtin =
		clone_string_bool_map(tc.struct_error_embeds_shadow_builtin)
	tc.struct_generic_params = clone_string_list_map(tc.struct_generic_params)
	tc.struct_implements = clone_string_list_map(tc.struct_implements)
	tc.struct_shared_fields = clone_string_bool_map(tc.struct_shared_fields)
	tc.struct_field_c_abi_fns = clone_string_string_map(tc.struct_field_c_abi_fns)
	tc.unions = clone_string_bool_map(tc.unions)
	tc.params_structs = clone_string_bool_map(tc.params_structs)
	tc.c_typedef_structs = clone_string_bool_map(tc.c_typedef_structs)
	tc.type_alias_generic_params = clone_string_list_map(tc.type_alias_generic_params)
	tc.type_alias_c_abi_fns = clone_string_string_map(tc.type_alias_c_abi_fns)
	tc.sum_types = clone_string_list_map(tc.sum_types)
	tc.sum_generic_params = clone_string_list_map(tc.sum_generic_params)
	tc.enum_names = clone_string_bool_map(tc.enum_names)
	tc.enum_fields = clone_string_list_map(tc.enum_fields)
	tc.flag_enums = clone_string_bool_map(tc.flag_enums)
	tc.interface_names = clone_string_bool_map(tc.interface_names)
	tc.interface_fields = clone_struct_field_map(tc.interface_fields)
	tc.interface_embeds = clone_string_list_map(tc.interface_embeds)
	tc.interface_abstract_methods = clone_string_list_map(tc.interface_abstract_methods)
}

fn promote_scoped_checker_node_caches(mut tc types.TypeChecker, a &flat.FlatAst, scope voidptr, generated_start int) {
	// The per-id loops write disjoint slots and only allocate clones, so fan
	// them out over the worker pool; fall back to the serial walk without one.
	if !transform.promote_scoped_checker_node_caches_parallel(mut tc, a, scope, generated_start) {
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

fn promote_scoped_signatures(mut tc types.TypeChecker, original_names map[string]bool, _scope voidptr) {
	// Set difference instead of the former sort-and-merge: only a handful of
	// signatures are added during transform, while sorting every fn name twice
	// cost several ms per build.
	mut added_names := []string{}
	for name, _ in tc.fn_ret_types {
		if name !in original_names {
			added_names << name
		}
	}
	// Keep the former sorted processing order: the delete/reinsert below
	// determines these entries' map iteration order for later phases.
	added_names.sort()
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
		tc.register_generated_fn_param_types(owned_name, params)
		tc.fn_variadic[owned_name] = variadic
		if specialized {
			tc.specialized_generic_fns[owned_name] = true
		}
	}
	// A reserved map can keep its dense arrays in the parent arena while cloned
	// string-key text or nested type metadata is allocated in the disposable
	// stage arena. Rebuild all signature ownership before releasing that arena.
	tc.rebuild_scoped_transform_signature_maps()
	// Re-own suffix keys/values after the scoped signatures above are promoted.
	tc.rebuild_fn_param_suffix_index()
}

// default_cc_identity returns a precise identity for the resolved default `cc`.
// Module objects in the persistent cache are compiled with literal `cc` (only
// the default compiler is cacheable), so a changed binary or retargeted symlink
// must invalidate them. The version probe also identifies the selected backend
// behind stable compiler shims and wrappers.
fn default_cc_identity() string {
	cc_path := os.real_path(os.find_abs_path_of_executable('cc') or { 'cc' })
	metadata := modulecache.file_metadata_signature(cc_path)
	version := cmdexec.run(cc_path, ['--version'])
	return '${cc_path}\t${metadata}\t${version.exit_code}\t${version.output.replace('\n', ' ')}'
}

fn effective_c_compiler_name(compiler string, target pref.Target) string {
	compiler_path := os.find_abs_path_of_executable(compiler) or { compiler }
	resolved_path := os.real_path(compiler_path)
	name := os.file_name(resolved_path).to_lower_ascii()
	if name.contains('tcc') || name.contains('tinyc') {
		return 'tinyc'
	}
	if name.contains('clang') {
		return 'clang'
	}
	if name.contains('gcc') {
		return 'gcc'
	}
	if name.contains('mingw') {
		return 'mingw'
	}
	if name in ['cl', 'cl.exe'] || name.contains('msvc') {
		return 'msvc'
	}
	if name.contains('++') {
		return 'cplusplus'
	}
	// Apple's system `cc` is Clang. Avoid launching it just to rediscover that
	// fact on every C-output-only compilation.
	if target.os in ['macos', 'ios'] && resolved_path == '/usr/bin/cc' {
		return 'clang'
	}
	compiler_result := cmdexec.run(compiler_path, ['--version'])
	version := compiler_result.output.to_lower_ascii()
	if version.contains('tiny c compiler') || version.contains('tcc version') {
		return 'tinyc'
	}
	if version.contains('clang') {
		return 'clang'
	}
	if version.contains('gcc') || version.contains('free software foundation') {
		return 'gcc'
	}
	if version.contains('mingw') {
		return 'mingw'
	}
	if version.contains('microsoft') && version.contains('c/c++') {
		return 'msvc'
	}
	return if target.os in ['macos', 'ios'] { 'clang' } else { 'gcc' }
}

struct V3TestBuildConstraint {
	expression string
	line       int
}

fn v3_test_build_constraint(file string) V3TestBuildConstraint {
	lines := os.read_lines(file) or { return V3TestBuildConstraint{} }
	for index, line in lines {
		if line.starts_with('// vtest build:') {
			return V3TestBuildConstraint{
				expression: line.all_after(':').trim_space()
				line: index + 1
			}
		}
	}
	return V3TestBuildConstraint{}
}

fn v3_test_build_fact_name(name string) bool {
	return name in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
		'android', 'termux', 'solaris', 'haiku', 'qnx', 'serenity', 'vinix', 'wasm32_emscripten',
		'tinyc', 'tcc', 'clang', 'gcc', 'mingw', 'msvc', 'cplusplus', 'amd64', 'arm64', 'arm32',
		'x86', 'i386', 'riscv64', 'ppc', 'ppc64', 'ppc64le', 's390x', 'loongarch64', 'wasm32', 'prod']
}

fn v3_test_build_facts(target pref.Target, ccompiler string, is_prod bool) []string {
	mut facts := map[string]bool{}
	for fact in os.getenv('VBUILD_FACTS').split_any(',') {
		name := fact.trim_space()
		if name.len > 0 && !v3_test_build_fact_name(name) {
			facts[name] = true
		}
	}
	facts[target.os] = true
	facts[ccompiler] = true
	facts[target.arch] = true
	if target.arch == 'x86' {
		facts['i386'] = true
	}
	if is_prod {
		facts['prod'] = true
	}
	if github_job := os.getenv_opt('GITHUB_JOB') {
		if github_job.len > 0 {
			facts[github_job] = true
		}
	}
	return facts.keys()
}

fn v3_test_process_running(process_name string) bool {
	$if windows {
		return false
	} $else {
		result := cmdexec.run('ps', ['ax'])
		if result.exit_code != 0 {
			return false
		}
		return result.output.split_into_lines().any(it.contains(process_name))
	}
}

fn v3_test_command_succeeds(command string, args []string) bool {
	path := os.find_abs_path_of_executable(command) or { return false }
	return cmdexec.run(path, args).exit_code == 0
}

struct V3TestDependencyProbe {
	command        string
	args           []string
	pkgconfig_name string
}

fn v3_test_dependency_probe_present(probe V3TestDependencyProbe) bool {
	if !v3_test_command_succeeds(probe.command, probe.args) {
		return false
	}
	if probe.pkgconfig_name.len == 0 {
		return true
	}
	return v3_test_command_succeeds('pkgconf', [probe.pkgconfig_name, '--libs'])
		|| v3_test_command_succeeds('pkg-config', [probe.pkgconfig_name, '--libs'])
}

fn v3_test_openssl_dependency_probe(command string, pkgconfig_name string) V3TestDependencyProbe {
	return V3TestDependencyProbe{
		command: command
		args: ['version']
		pkgconfig_name: pkgconfig_name
	}
}

fn v3_test_standard_dependency_probe(define string) ?V3TestDependencyProbe {
	match define {
		'present_node' {
			return V3TestDependencyProbe{
				command: 'node'
				args: ['--version']
			}
		}
		'present_python' {
			return V3TestDependencyProbe{
				command: 'python'
				args: ['--version']
				pkgconfig_name: 'python3'
			}
		}
		'present_ruby' {
			return V3TestDependencyProbe{
				command: 'ruby'
				args: ['--version']
				pkgconfig_name: 'ruby'
			}
		}
		'present_go' {
			return V3TestDependencyProbe{
				command: 'go'
				args: ['version']
			}
		}
		else {
			return none
		}
	}
}

fn v3_test_openssl_present() bool {
	$if openbsd {
		return v3_test_dependency_probe_present(v3_test_openssl_dependency_probe('eopenssl35', 'eopenssl35'))
	} $else {
		return v3_test_dependency_probe_present(v3_test_openssl_dependency_probe('openssl', 'openssl'))
	}
}

fn v3_test_modern_openssl_present() bool {
	if !v3_test_openssl_present() {
		return false
	}
	command := $if openbsd { 'eopenssl35' } $else { 'openssl' }
	path := os.find_abs_path_of_executable(command) or { return false }
	result := cmdexec.run(path, ['version'])
	words := result.output.trim_space().split_any(' \t')
	if result.exit_code != 0 || words.len < 2 || words[0] != 'OpenSSL' {
		return false
	}
	parts := words[1].all_before('-').split('.')
	if parts.len < 2 {
		return false
	}
	major := parts[0].int()
	minor := parts[1].int()
	patch := if parts.len > 2 { parts[2].int() } else { 0 }
	return major > 3 || (major == 3 && (minor > 5 || (minor == 5 && patch >= 0)))
}

fn v3_test_openssl_probe_allowed(github_job string, user_os string) bool {
	return github_job.len == 0 || user_os != 'windows'
}

fn v3_test_sqlite_present(user_os string, vexeroot string) bool {
	if user_os == 'windows' {
		return os.exists(os.join_path(vexeroot, 'thirdparty', 'sqlite', 'sqlite3.c'))
	}
	return v3_test_command_succeeds('sqlite3', ['--version'])
		&& (v3_test_command_succeeds('pkgconf', ['sqlite3', '--libs'])
			|| v3_test_command_succeeds('pkg-config', ['sqlite3', '--libs']))
}

fn v3_test_build_defines(expression string, user_defines []string) []string {
	mut defines := map[string]bool{}
	for define in os.getenv('VBUILD_DEFINES').split_any(',') {
		name := define.trim_space()
		if name.len > 0 {
			defines[name] = true
		}
	}
	for define in user_defines {
		name := define.all_before('=').trim_space()
		if name.len > 0 {
			defines[name] = true
		}
	}
	github_job := os.getenv('GITHUB_JOB')
	if github_job.starts_with('sanitize-') {
		defines['sanitized_job'] = true
	}
	process_defines := {
		'started_mysqld':   'mysqld'
		'started_postgres': 'postgres'
		'started_mssql':    'sqlservr'
		'started_redis':    'redis-server'
	}
	for define, process_name in process_defines {
		if expression.contains('${define}?') && v3_test_process_running(process_name) {
			defines[define] = true
		}
	}
	for define in ['present_node', 'present_python', 'present_ruby', 'present_go'] {
		if !expression.contains('${define}?') {
			continue
		}
		probe := v3_test_standard_dependency_probe(define) or { continue }
		if v3_test_dependency_probe_present(probe) {
			defines[define] = true
		}
	}
	if expression.contains('present_sqlite3?') && v3_test_sqlite_present(os.user_os(), @VEXEROOT) {
		defines['present_sqlite3'] = true
	}
	openssl_probe_allowed := v3_test_openssl_probe_allowed(github_job, os.user_os())
	if openssl_probe_allowed && expression.contains('present_openssl?') && v3_test_openssl_present() {
		defines['present_openssl'] = true
	}
	if openssl_probe_allowed && expression.contains('has_modern_openssl?')
		&& v3_test_modern_openssl_present() {
		defines['has_modern_openssl'] = true
	}
	if expression.contains('os_id_') && os.is_file('/etc/os-release') {
		for line in os.read_lines('/etc/os-release') or { []string{} } {
			if line.starts_with('ID=') {
				id := line.all_after('=').trim('"\' ')
				if id.len > 0 {
					defines['os_id_${id}'] = true
				}
				break
			}
		}
	}
	return defines.keys()
}

fn v3_test_matches_build_constraint(file string, target pref.Target, ccompiler string, is_prod bool, user_defines []string) bool {
	details := v3_test_build_constraint(file)
	if details.expression.len == 0 {
		return true
	}
	environment := build_constraint.new_environment(v3_test_build_facts(target, ccompiler, is_prod), v3_test_build_defines(details.expression, user_defines))
	return environment.eval(details.expression) or {
		eprintln('${file}:${details.line}:17: error during parsing the `// vtest build` expression `${details.expression}`: ${err}')
		false
	}
}

fn v3_direct_test_input_is_incompatible(is_test_command bool, input_file string, backend string, target pref.Target, ccompiler string, is_prod bool, user_defines []string) bool {
	if !is_test_command || !os.is_file(input_file) {
		return false
	}
	if is_test_file_for_any_backend(input_file)
		&& !pref.is_test_file_for_platform(input_file, backend, target) {
		return true
	}
	return !v3_test_matches_build_constraint(input_file, target, ccompiler, is_prod, user_defines)
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
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_source_signatures')
	return modulecache.cached_source_signature(cache_dir, os.real_path(vroot), files)
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

fn incremental_changed_functions_use_generics(a &flat.FlatAst, tc &types.TypeChecker, changed_names map[string]bool) bool {
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
					&& incremental_node_tree_uses_generics(a, tc, flat.NodeId(idx), cur_module) {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn incremental_node_tree_uses_generics(a &flat.FlatAst, tc &types.TypeChecker, root flat.NodeId, module_name string) bool {
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
		mut type_names := [node.typ, node.value]
		if node.generic_params().len > 0 && node.value.len > 0 {
			type_names << '${node.value}[${node.generic_params().join(', ')}]'
		}
		for type_name in type_names {
			if incremental_type_is_generic_named_application(type_name, module_name, tc) {
				return true
			}
		}
		for child_idx in 0 .. node.children_count {
			stack << a.child(&node, child_idx)
		}
	}
	return false
}

fn incremental_type_is_generic_named_application(type_name string, module_name string, tc &types.TypeChecker) bool {
	mut offset := 0
	for offset < type_name.len {
		relative_bracket := type_name[offset..].index_u8(`[`)
		if relative_bracket < 0 {
			return false
		}
		bracket := offset + relative_bracket
		mut start := bracket
		for start > 0 {
			c := type_name[start - 1]
			if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
				|| (c >= `0` && c <= `9`) || c in [`_`, `.`]) {
				break
			}
			start--
		}
		if start < bracket
			&& incremental_generic_type_base_is_known(type_name[start..bracket], module_name, tc) {
			return true
		}
		offset = bracket + 1
	}
	return false
}

fn incremental_generic_type_base_is_known(base string, module_name string, tc &types.TypeChecker) bool {
	if base in tc.struct_generic_params || base in tc.sum_generic_params
		|| base in tc.type_alias_generic_params {
		return true
	}
	if !base.contains('.') && module_name !in ['', 'main', 'builtin'] {
		qualified := '${module_name}.${base}'
		if qualified in tc.struct_generic_params || qualified in tc.sum_generic_params
			|| qualified in tc.type_alias_generic_params {
			return true
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
			params: params
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
									params: params
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
								params: params
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

fn record_compile_value(mut values map[string]string, define string) {
	name := define.all_before('=').trim_space()
	if name.len == 0 {
		return
	}
	values[name] = if define.contains('=') { define.all_after_first('=') } else { 'true' }
}

fn record_user_define(mut defines []string, mut values map[string]string, define string) {
	name := define.all_before('=').trim_space()
	if name.len == 0 {
		return
	}
	has_value := define.contains('=')
	value := if has_value { define.all_after_first('=') } else { 'true' }
	if (!has_value || value.len > 0) && name !in defines {
		defines << name
	}
	if has_value {
		valued_define := '${name}=${value}'
		if valued_define !in defines {
			defines << valued_define
		}
	}
	values[name] = value
}

fn stage_macos_v3_compiler_error_fallback(fallback_file string, stage string) bool {
	if fallback_file == '' {
		return false
	}
	// The first line remains the machine-readable fallback reason. The second
	// carries only a controlled stage name, so a successful compatibility build
	// can report where V3 failed even when no source excerpt is available.
	os.write_file(fallback_file, '${macos_v3_compiler_error_fallback}\n${stage}') or {
		return false
	}
	return true
}

fn clear_macos_v3_compiler_error_fallback(fallback_file string) {
	if fallback_file != '' {
		os.rm(fallback_file) or {}
	}
}

fn macos_v3_fallback_payload_is_valid(payload string) bool {
	reason := payload.all_before('\n')
	return reason in [macos_v3_inline_asm_fallback, macos_v3_compiler_error_fallback,
		macos_v3_c_error_fallback]
}

fn macos_v3_fallback_suppresses_diagnostics(fallback_file string) bool {
	if fallback_file == '' || os.getenv(macos_v3_no_fallback_env) == '1' {
		return false
	}
	payload := os.read_file(fallback_file) or { return false }
	return macos_v3_fallback_payload_is_valid(payload)
}

fn request_macos_v3_compatibility_fallback(diagnostics []parser.Diagnostic, fallback_file string) bool {
	if fallback_file == '' || os.getenv(macos_v3_no_fallback_env) == '1'
		|| !diagnostics.any(it.message == macos_v3_inline_asm_diagnostic) {
		return false
	}
	os.write_file(fallback_file, macos_v3_inline_asm_fallback) or { return false }
	return true
}

fn v3_source_is_pure_v(path string) bool {
	if !path.ends_with('.v') && !path.ends_with('.vv') && !path.ends_with('.vsh') {
		return false
	}
	before_dot_v := path.all_before_last('.v')
	language := before_dot_v.all_after_last('.')
	language_with_underscore := before_dot_v.all_after_last('_')
	if language == before_dot_v && language_with_underscore == before_dot_v {
		return true
	}
	actual_language := if language == before_dot_v { language_with_underscore } else { language }
	return actual_language !in ['c', 'js', 'amd64', 'x86_64', 'x64', 'x86', 'aarch64', 'arm64',
		'aarch32', 'arm32', 'arm', 'rv64', 'riscv64', 'risc-v64', 'riscv', 'risc-v', 'rv32', 'riscv32',
		'x86_32', 'x32', 'i386', 'IA-32', 'ia-32', 'ia32', 's390x', 'loongarch64', 'ppc64le',
		'sparc64', 'ppc64', 'ppc', 'ppc32', 'powerpc', 'js_node', 'js_browser', 'js_freestanding',
		'wasm32', 'wasm']
}

fn v3_type_text_uses_interop_namespace(text string, namespace string) bool {
	needle := namespace + '.'
	mut offset := 0
	for offset < text.len {
		relative := text[offset..].index(needle) or { return false }
		index := offset + relative
		if index == 0 || (!(text[index - 1].is_alnum() || text[index - 1] == `_`)
			&& text[index - 1] != `.`) {
			return true
		}
		offset = index + needle.len
	}
	return false
}

fn v3_ast_node_uses_interop_namespace(a &flat.FlatAst, node &flat.Node, namespace string) bool {
	if node.kind == .selector && node.children_count > 0 {
		base := a.child_node(node, 0)
		if base.kind == .ident && base.value == namespace {
			return true
		}
	}
	if node.kind !in [.string_literal, .string_interp, .char_literal, .directive, .file] && node.value.starts_with(namespace + '.') {
		return true
	}
	return v3_type_text_uses_interop_namespace(node.typ, namespace)
}

fn v3_explicit_interop_fn_namespace(node &flat.Node, file string, mut source_cache map[string]string) string {
	if node.kind != .c_fn_decl || !node.pos.is_valid() {
		return ''
	}
	source := source_cache[file] or {
		loaded := os.read_file(file) or { return '' }
		source_cache[file] = loaded
		loaded
	}
	mut cursor := int_min(node.pos.offset, source.len)
	for cursor > 0 && source[cursor - 1] in [` `, `\t`] {
		cursor--
	}
	if cursor == 0 || source[cursor - 1] != `.` {
		return ''
	}
	cursor--
	for cursor > 0 && source[cursor - 1] in [` `, `\t`] {
		cursor--
	}
	end := cursor
	for cursor > 0 && (source[cursor - 1].is_alnum() || source[cursor - 1] == `_`) {
		cursor--
	}
	namespace := source[cursor..end]
	if namespace in ['C', 'JS'] {
		return namespace
	}
	return ''
}

fn v3_impure_v_diagnostics(a &flat.FlatAst) []parser.Diagnostic {
	mut diagnostics := []parser.Diagnostic{}
	mut seen := map[string]bool{}
	mut source_cache := map[string]string{}
	mut file_ids := map[string]int{}
	for id, file in a.source_files {
		file_ids[file.name] = id
	}
	mut current_file := ''
	mut current_file_id := 0
	for node in a.nodes {
		if node.kind == .file {
			current_file = node.value
			current_file_id = file_ids[current_file] or { 0 }
			continue
		}
		mut file := current_file
		mut file_id := current_file_id
		if node.pos.is_valid() {
			if source_file := a.source_files[node.pos.id] {
				file = source_file.name
				file_id = node.pos.id
			}
		}
		if file_id == 0 || !v3_source_is_pure_v(file) {
			continue
		}
		explicit_fn_namespace := v3_explicit_interop_fn_namespace(&node, file, mut source_cache)
		for namespace in ['C', 'JS'] {
			if explicit_fn_namespace != namespace
				&& !v3_ast_node_uses_interop_namespace(a, &node, namespace) {
				continue
			}
			pos := if node.pos.is_valid() { node.pos } else { v3token.new_pos(file_id, 0) }
			key := '${pos.id}:${pos.offset}:${namespace}'
			if key in seen {
				continue
			}
			seen[key] = true
			mut line := 1
			mut column := pos.offset + 1
			if position := a.source_position(pos) {
				line = position.line
				column = position.column
			}
			diagnostics << parser.Diagnostic{
				file: file
				pos: pos
				line: line
				column: column
				severity: 'warning:'
				message: '${namespace} code will not be allowed in pure .v files, please move it to a .${namespace.to_lower_ascii()}.v file instead'
			}
		}
	}
	return diagnostics
}

fn request_macos_v3_c_error_fallback(fallback_file string, report_dir string, ccompiler string, c_output string, c_source string, v_sources map[string]string) bool {
	if fallback_file == '' || report_dir == '' || !os.is_file(c_source) {
		return false
	}
	os.rmdir_all(report_dir) or {}
	os.mkdir_all(report_dir) or { return false }
	source_name := os.base(c_source)
	report_source := os.join_path(report_dir, source_name)
	os.cp(c_source, report_source) or {
		os.rmdir_all(report_dir) or {}
		return false
	}
	os.write_file(os.join_path(report_dir, macos_v3_c_error_compiler_file), ccompiler) or {
		os.rmdir_all(report_dir) or {}
		return false
	}
	os.write_file(os.join_path(report_dir, macos_v3_c_error_output_file), c_output) or {
		os.rmdir_all(report_dir) or {}
		return false
	}
	os.write_file(os.join_path(report_dir, macos_v3_c_error_source_name_file), source_name) or {
		os.rmdir_all(report_dir) or {}
		return false
	}
	if !write_macos_v3_fallback_source_digests(report_dir, v_sources) {
		os.rmdir_all(report_dir) or {}
		return false
	}
	os.write_file(fallback_file, macos_v3_c_error_fallback) or {
		os.rmdir_all(report_dir) or {}
		return false
	}
	return true
}

fn write_macos_v3_fallback_source_digests(report_dir string, v_sources map[string]string) bool {
	mut v_source_paths := v_sources.keys()
	v_source_paths.sort()
	mut v_source_paths_text := strings.new_builder(v_source_paths.len * 64)
	mut v_source_digests_text := strings.new_builder(v_source_paths.len * (sha256.size * 2 + 1))
	for i, path in v_source_paths {
		if i > 0 {
			v_source_paths_text.write_u8(0)
			v_source_digests_text.write_u8(0)
		}
		v_source_paths_text.write_string(path)
		v_source_digests_text.write_string(v_sources[path])
	}
	os.write_file(os.join_path(report_dir, macos_v3_c_error_v_sources_file), v_source_paths_text.str()) or { return false }
	os.write_file(os.join_path(report_dir, macos_v3_c_error_v_source_digests_file), v_source_digests_text.str()) or { return false }
	return true
}

// stage_macos_v3_fallback_source_digests first snapshots every source V3 parsed. Once
// native dependency resolution completes, the same files are refreshed with tagged
// #include/#insert digests and a completeness marker. The stable retry reports only when
// it confirms that complete input snapshot.
fn stage_macos_v3_fallback_source_digests(report_dir string, v_sources map[string]string) bool {
	if report_dir == '' || v_sources.len == 0 {
		return false
	}
	os.mkdir_all(report_dir) or { return false }
	return write_macos_v3_fallback_source_digests(report_dir, v_sources)
}

fn request_macos_v3_c_error_fallback_from_message(fallback_file string, report_dir string, ccompiler string, message string, c_sources []string, v_sources map[string]string) bool {
	is_c_error := message.starts_with('failed to build C object ')
		|| message.starts_with('failed to build cached module object ')
		|| message.starts_with('failed to build cached program prefix:')
		|| message.starts_with('failed to build cached development dylib:')
	if !is_c_error {
		return false
	}
	for c_source in c_sources {
		if os.is_file(c_source) {
			return request_macos_v3_c_error_fallback(fallback_file, report_dir, ccompiler, message, c_source, v_sources)
		}
	}
	return false
}

fn macos_v3_fallback_report_sources(a &flat.FlatAst, vroot string, cached_source_digests map[string]string, ignored_source_paths map[string]bool) map[string]string {
	mut sources := map[string]string{}
	mut ambiguous := map[string]bool{}
	builtin_root :=
		os.real_path(os.join_path(vroot, 'vlib', 'builtin')).trim_right(os.path_separator)
	for _, file in a.source_files {
		if (file.name.ends_with('.v') || file.name.ends_with('.vv')
			|| file.name.ends_with('.vsh')) && file.has_source_sha256() {
			path := os.real_path(file.name)
			if ignored_source_paths[path] {
				continue
			}
			// V1 and V3 select different internal builtin support files. All other bundled
			// vlib sources are shared inputs and remain covered by verification.
			if v3_fallback_backend_specific_builtin_source(path, builtin_root) {
				continue
			}
			source_digest := file.source_sha256()
			digest := source_digest[..].hex()
			if old_digest := sources[path] {
				if old_digest != digest {
					ambiguous[path] = true
				}
			} else {
				sources[path] = digest
			}
		}
	}
	for source_path, digest in cached_source_digests {
		path := os.real_path(source_path)
		if ignored_source_paths[path] {
			continue
		}
		if digest == '' {
			ambiguous[path] = true
			continue
		}
		if v3_fallback_backend_specific_builtin_source(path, builtin_root) {
			continue
		}
		if old_digest := sources[path] {
			if old_digest != digest {
				ambiguous[path] = true
			}
		} else {
			sources[path] = digest
		}
	}
	for path, _ in ambiguous {
		sources.delete(path)
	}
	return sources
}

// macos_v3_fallback_report_inputs adds the exact native files that V3's resolved
// include/insert dependency tree can consume. Native keys are tagged so the report
// source extractor never treats a header or C source as an uploadable V excerpt.
fn macos_v3_fallback_report_inputs(v_sources map[string]string, state &V3ModuleCacheState) map[string]string {
	mut inputs := v_sources.clone()
	if !state.external_inputs_ready || !state.external_inputs_complete {
		return inputs
	}
	mut native_paths := map[string]bool{}
	for paths in state.module_external_inputs.values() {
		for path in paths {
			native_paths[os.real_path(path)] = true
		}
	}
	for paths in state.module_native_roots.values() {
		for path in paths {
			native_paths[os.real_path(path)] = true
		}
	}
	mut native_digests := map[string]string{}
	for path in native_paths.keys() {
		digest := state.external_input_digests[path] or { return inputs }
		if !v3_sha256_hex_digest_is_valid(digest) {
			return inputs
		}
		native_digests['${v3_fallback_native_input_prefix}${path}'] = digest
	}
	for key, digest in native_digests {
		inputs[key] = digest
	}
	inputs[v3_fallback_native_manifest_key] = sha256.hexhash(v3_fallback_native_manifest_value)
	return inputs
}

fn record_v3_fallback_module_use(mut cache_state V3ModuleCacheState, module_name string, is_warmup bool) {
	if module_name == '' {
		return
	}
	if is_warmup {
		cache_state.fallback_warmup_modules[module_name] = true
	} else {
		cache_state.fallback_required_modules[module_name] = true
	}
}

fn v3_fallback_ignored_warmup_source_paths(cache_state &V3ModuleCacheState) map[string]bool {
	mut ignored := map[string]bool{}
	for module_name, _ in cache_state.fallback_warmup_modules {
		if cache_state.fallback_required_modules[module_name] {
			continue
		}
		for path in cache_state.module_sources[module_name] {
			ignored[os.real_path(path)] = true
		}
	}
	return ignored
}

fn record_v3_cached_source_digests(mut cache_state V3ModuleCacheState, source_digests map[string]string) {
	for path, digest in source_digests {
		if path in cache_state.cached_source_digests {
			if cache_state.cached_source_digests[path] != digest {
				// Empty is an ambiguity marker. Never let a later cache lookup restore a
				// path whose verified content changed during this compiler run.
				cache_state.cached_source_digests[path] = ''
			}
			continue
		}
		cache_state.cached_source_digests[path] = digest
	}
}

fn v3_fallback_backend_specific_builtin_source(path string, builtin_root string) bool {
	return (path == builtin_root || path.starts_with(builtin_root + os.path_separator))
		&& os.file_name(path) in ['ownership_interface_d_v3_backend.v',
			'ownership_interface_notd_v3_backend.v', 'prealloc.c.v']
}

fn input_uses_minimal_literal_output_builtin(input_file string, prefs &pref.Preferences, is_test_command bool, is_checker_fixture bool) bool {
	if prefs.backend != 'c' || prefs.target.os != 'macos' || is_test_command || is_checker_fixture
		|| !(input_file.ends_with('.v') || input_file.ends_with('.vv')) || !os.is_file(input_file)
		|| is_v3_test_file(input_file, prefs.backend, prefs.target) {
		return false
	}
	// Parse the one user file before builtin. This conservative syntax-only pass
	// lets literal output programs avoid parsing and checking builtin declarations
	// that markused will discard, without applying a text heuristic to V syntax.
	mut candidate_parser := parser.Parser.new(prefs)
	candidate_ast := candidate_parser.parse_file(input_file)
	if candidate_parser.diagnostics.len > 0 {
		return false
	}
	mut candidate_files := map[string]bool{}
	for node in candidate_ast.nodes {
		if node.kind == .file {
			candidate_files[node.value] = true
		}
	}
	return candidate_files.len == 1
		&& markused.is_trivial_literal_output_program(candidate_ast, candidate_files)
}

fn is_minimal_literal_output_builtin_file(path string) bool {
	return os.file_name(path) in [
		'array.v',
		'array_notd_gcboehm_opt.v',
		'builtin.v',
		'chan_option_result.v',
		'int.v',
		'int_notd_new_int.v',
		'map.v',
		'map_notd_gcboehm_opt.v',
		'string.v',
		'allocation.c.v',
		'backtraces.c.v',
		'backtraces_nix.c.v',
		'builtin.c.v',
		'builtin_backtraces_nix.c.v',
		'builtin_nix.c.v',
		'builtin_notd_gcboehm.c.v',
		'builtin_notd_use_libbacktrace.c.v',
		'cfns.c.v',
		'cfns_wrapper.c.v',
		'character_inout.c.v',
		'map.c.v',
		'option.c.v',
		'panicing.c.v',
		'prealloc.c.v',
		'printing.c.v',
		'vgc_notd_vgc.c.v',
	]
}

fn suppress_minimal_literal_output_builtin_imports(mut a flat.FlatAst) {
	// array.v and string.v need these modules only inside bodies that the literal
	// output reachability path neither checks nor emits.
	for i in 0 .. a.user_code_start {
		if a.nodes[i].kind == .import_decl && a.nodes[i].value in ['strings', 'strconv'] {
			a.nodes[i] = flat.Node{}
		}
	}
}

fn parse_v3_environment_flags(name string) []string {
	value := os.getenv(name).replace('\r', ' ').replace('\n', ' ')
	if value.trim_space().len == 0 {
		return []
	}
	return cmdexec.split_args(value) or {
		eprintln('invalid `${name}` value: ${err.msg()}')
		exit(1)
	}
}

fn v3_environment_coverage_dir() string {
	value := os.getenv('VCOVDIR')
	if value.len == 0 {
		return ''
	}
	return os.real_path(value)
}

fn v3_environment_run_only() []string {
	value := os.getenv('VTEST_ONLY_FN')
	if value.len == 0 {
		return []
	}
	return value.split_any(',').filter(it.len > 0)
}

fn v3_environment_show_test_stats() bool {
	return os.getenv('VTEST_SHOW_ASSERTS').len > 0
}

fn is_linux_wayland_only_session(target_os string, display string, wayland_display string, session_type string) bool {
	return target_os == 'linux' && display == ''
		&& (wayland_display != '' || session_type.to_lower() == 'wayland')
}

fn show_v3_c_compiler_output(enabled bool, compiler string, result os.Result) {
	if !enabled {
		return
	}
	header := '======== Output of the C Compiler (${compiler}) ========'
	println(header)
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
	println('='.repeat(header.len))
}

fn v3_run_only_cache_identity(patterns []string) string {
	mut parts := []string{cap: patterns.len}
	for pattern in patterns {
		parts << '${pattern.len}:${pattern}'
	}
	return parts.join(',')
}

fn v3_effective_warns_are_errors(explicit bool, is_prod bool) bool {
	return explicit || is_prod
}

const v3_large_prod_c_unit_threshold = u64(8 * 1024 * 1024)

fn v3_is_large_prod_c_unit(source_size u64) bool {
	return source_size >= v3_large_prod_c_unit_threshold
}

fn v3_prod_c_optimization_flags(is_prod bool, no_prod_options bool, is_shared bool, parallel_cc bool, large_c_unit bool, limit_inlining bool, explicit_tcc bool) []string {
	if !is_prod || no_prod_options {
		return []
	}
	// Clang's -O3 compile cost grows sharply on very large generated translation
	// units. -O2 retains whole-program LTO while avoiding those costly passes.
	mut flags := [if large_c_unit { '-O2' } else { '-O3' }]
	if !is_shared && !parallel_cc && !explicit_tcc {
		flags << '-flto'
	}
	if large_c_unit && limit_inlining {
		// Large V translation units expose many mechanically generated candidates.
		// Bound Clang's inliner search without disabling profitable inlining.
		flags << ['-mllvm', '-inline-threshold=75']
	}
	return flags
}

fn v3_prod_c_object_optimization_flags(is_prod bool, no_prod_options bool, is_shared bool, parallel_cc bool, explicit_tcc bool) []string {
	// Native support sources are cached as independently compiled objects. Emitting
	// LLVM bitcode here makes every program link optimize those unchanged sources
	// again; keep the per-object -O3 work in the cache instead.
	return v3_prod_c_optimization_flags(is_prod, no_prod_options, is_shared, parallel_cc, false, false, explicit_tcc).filter(it != '-flto')
}

fn append_v3_c_compile_mode_flags(mut args []string, c_standard string, opt_flags string, pic_flag string) {
	if c_standard.len > 0 {
		args << c_standard
	}
	args << cgen.tokenize_c_flag(opt_flags)
	if pic_flag.len > 0 {
		args << pic_flag
	}
}

fn expand_v3_module_search_paths(spec string, vroot string) []string {
	if spec.len == 0 {
		return []
	}
	mut expanded := []string{}
	for path in spec.replace('|', os.path_delimiter).split(os.path_delimiter) {
		match path {
			'@vlib' { expanded << os.join_path_single(vroot, 'vlib') }
			'@vmodules' { expanded << os.vmodules_paths() }
			else { expanded << path.replace('@vroot', vroot) }
		}
	}
	return expanded
}

fn v3_driver_option_requires_value(option string) bool {
	return option in ['-o', '-output', '-b', '-backend', '-os', '-arch', '-compile-backend',
		'--compile-backend', '-d', '-define', '-gc', '-cc', '-thread-stack-size', '-path', '-cov',
		'-coverage', '-file-list', '-message-limit', '-printfn', '-generate-c-project', '-test-runner',
		'-run-only', '-profile-fns']
}

fn v3_driver_option_consumes_value(option string) bool {
	return v3_driver_option_requires_value(option) || option in ['-cflags', '-dump-c-flags']
}

fn apply_v3_diagnostic_color_option(option string) {
	ansi.set_colors_enabled(option == '-color')
}

fn apply_v3_default_diagnostic_color() {
	ansi.set_colors_enabled(ansi.stderr_supports_escape_sequences())
}

fn v3_has_following_positional_arg(args []string, start int) bool {
	for idx := start; idx < args.len; idx++ {
		if !args[idx].starts_with('-') {
			return true
		}
	}
	return false
}

// Keep the optional `-profile [file]` argument compatible with V1: a lone
// source path remains the compiler input, while an earlier non-source path is
// consumed as the profile output when another positional argument follows it.
fn v3_profile_optional_arg_value(args []string, idx int, command_seen bool) (string, bool) {
	next := args[idx + 1] or { return '-', false }
	if next == '-' {
		return next, true
	}
	if next.starts_with('-') {
		return '-', false
	}
	if !command_seen && (next in ['run', 'build', 'test', 'doc'] || next.ends_with('.v')
		|| next.ends_with('.vv') || next.ends_with('.vsh') || os.is_dir(next)
		|| !v3_has_following_positional_arg(args, idx + 2)) {
		return '-', false
	}
	return next, true
}

fn add_v3_profile_used_fns(mut used_fns map[string]bool) {
	for name in ['time.vpc_now', 'time.vpc_now_darwin', 'v.profile.state', 'v.profile.on',
		'profile.state', 'profile.on'] {
		used_fns[name] = true
	}
}

$if !skip_fastc ? {
	struct V3FastCCompileResult {
		success bool
		command string
		output  string
	}

	fn publish_v3_fastc_c_source(pieces []string, output_file string, c_to_stdout bool) ! {
		if c_to_stdout {
			for piece in pieces {
				print(piece)
			}
			return
		}
		staged_output := '${output_file}.stage.${tempname.unique_token()}'
		fastc.write_c_pieces(staged_output, pieces) or {
			return error('error writing fastc output ${output_file}: ${err.msg()}')
		}
		os.mv(staged_output, output_file) or {
			os.rm(staged_output) or {}
			return error('error finalizing fastc output ${output_file}: ${err.msg()}')
		}
	}

	fn canonical_v3_fastc_output_path(path string) string {
		if path == '' {
			return ''
		}
		if os.exists(path) {
			return os.real_path(path)
		}
		absolute_path := os.abs_path(path)
		canonical_parent := os.real_path(os.dir(absolute_path))
		return os.join_path_single(canonical_parent, os.file_name(absolute_path))
	}

	fn compile_v3_fastc_source(pieces []string, units fastc.FastcUnitLayout, bin_file string, prefs &pref.Preferences, environment_c_flags []string, source_c_flags []string, user_c_flags []string, environment_ld_flags []string, macos_sdk_root string, is_debug bool, uses_threads bool) V3FastCCompileResult {
		bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
		cc_sw := time.new_stopwatch()
		tcc_dir := os.join_path(prefs.vroot, 'thirdparty', 'tcc')
		tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
		if !os.is_executable(tcc_path) {
			return V3FastCCompileResult{}
		}
		build_dir := os.join_path_single(os.dir(os.real_path(bin_file)), '.${os.file_name(bin_file)}.fastc.${tempname.unique_token()}')
		os.mkdir_all(build_dir) or { return V3FastCCompileResult{} }
		defer {
			cleanup_c_build_dir(build_dir)
		}
		source_file := os.join_path_single(build_dir, 'src.c')
		staged_binary := os.join_path_single(build_dir, 'out')
		// The program's translation units are compiled by concurrent TinyCC
		// processes and linked; a program that does not split is compiled as
		// one file.
		if bench_phases {
			eprintln('fastc-phase tcc.setup ${cc_sw.elapsed().microseconds()}us')
		}
		unit_paths := fastc.fastc_write_c_units(os.join_path_single(build_dir, 'src'), pieces, units, fastc.fastc_tcc_job_count(prefs)) or { return V3FastCCompileResult{} }
		if unit_paths.len < 2 {
			fastc.write_c_pieces(source_file, pieces) or { return V3FastCCompileResult{} }
		}
		if bench_phases {
			eprintln('fastc-phase tcc.units_written ${cc_sw.elapsed().microseconds()}us units=${unit_paths.len}')
		}
		tcc_resources := v3_tcc_resource_flags(prefs.vroot)
		mut cc_args := environment_c_flags.clone()
		cc_args << ['-std=gnu11', tcc_resources.base_arg, tcc_resources.include_arg,
			tcc_resources.library_arg]
		cc_args << v3_tcc_host_system_flags(prefs.normalized_target_os(), macos_sdk_root)
		cc_args << source_c_flags
		// A call without a prototype would silently truncate a pointer result
		// (the C carries no headers): it is an error, not a warning.
		cc_args << '-Werror=implicit-function-declaration'
		if v3_tcc_backtrace_enabled(prefs.normalized_target_os(), prefs.normalized_target_arch(), false) {
			cc_args << '-bt25'
		}
		if is_debug {
			cc_args << '-g'
		}
		// Keep archives and other link-only source directives after the generated
		// object inputs. Only their state-affecting options are applied while
		// preparing libtcc; static archives remain order-sensitive link operands.
		compile_base_args := c_object_compile_flags(cc_args)
		base_link_args := c_dylib_link_flags(cc_args)
		user_compile_args := c_object_compile_flags(user_c_flags)
		user_link_args := c_dylib_link_flags(user_c_flags)
		mut final_args := base_link_args.clone()
		final_args << user_link_args
		if uses_threads {
			final_args << '-lpthread'
		}
		final_args << '-lm'
		final_args << environment_ld_flags
		mut shim_dir := fastc.FastcCodesignShim{}
		defer {
			fastc.fastc_remove_codesign_shim_dir(shim_dir)
		}
		mut result := os.Result{}
		mut command := ''
		mut sign_in_process := false
		if unit_paths.len > 1 {
			mut compile_args := compile_base_args.clone()
			compile_args << user_compile_args
			link_worker := spawn fastc.fastc_prepare_link(tcc_path, os.join_path_single(tcc_dir, 'lib'), compile_base_args, final_args)
			unit_objects := fastc.fastc_compile_c_units(tcc_path, compile_args, unit_paths) or {
				mut prepared_link := link_worker.wait()
				fastc.fastc_discard_link(mut prepared_link)
				fastc.write_c_pieces(source_file, pieces) or {}
				return V3FastCCompileResult{
					command: cmdexec.display(tcc_path, compile_args)
					output: err.msg()
				}
			}
			if bench_phases {
				eprintln('fastc-phase tcc.units_compiled ${cc_sw.elapsed().microseconds()}us')
			}
			link_inputs := unit_objects.clone()
			mut display_args := compile_base_args.clone()
			display_args << ['-o', staged_binary]
			display_args << link_inputs
			display_args << final_args
			command = cmdexec.display(tcc_path, display_args)
			mut prepared_link := link_worker.wait()
			sign_in_process = fastc.fastc_prepared_link_skips_codesign(&prepared_link)
			if !sign_in_process {
				// The executable-based linker still needs the PATH shim; the prepared
				// libtcc linker suppresses its codesign call without a subprocess.
				shim_dir = fastc.fastc_codesign_shim_dir()
				sign_in_process = shim_dir.dir != ''
			}
			result = fastc.fastc_finish_link(mut prepared_link, link_inputs, final_args, staged_binary)
			if bench_phases {
				eprintln('fastc-phase tcc.linked ${cc_sw.elapsed().microseconds()}us')
			}
		} else {
			cc_args = compile_base_args.clone()
			cc_args << user_compile_args
			cc_args << ['-o', 'out', 'src.c']
			cc_args << final_args
			command = cmdexec.display(tcc_path, cc_args)
			shim_dir = fastc.fastc_codesign_shim_dir()
			sign_in_process = shim_dir.dir != ''
			result = cmdexec.run_in(tcc_path, cc_args, build_dir)
		}
		if result.exit_code != 0 || !os.is_file(staged_binary) {
			if keep_dir := os.getenv_opt('V3_FASTC_KEEP_FAILED_C') {
				if keep_dir.len > 0 {
					os.cp(source_file, keep_dir) or {}
				}
			}
			return V3FastCCompileResult{
				command: command
				output: result.output
			}
		}
		if sign_in_process {
			fastc.fastc_sign_macho_adhoc(staged_binary) or {
				return V3FastCCompileResult{
					command: command
					output: 'could not sign ${staged_binary}: ${err.msg()}'
				}
			}
		}
		if bench_phases {
			eprintln('fastc-phase tcc.signed ${cc_sw.elapsed().microseconds()}us')
		}
		os.mv(staged_binary, bin_file) or {
			return V3FastCCompileResult{
				command: command
				output: err.msg()
			}
		}
		return V3FastCCompileResult{
			success: true
			command: command
			output: result.output
		}
	}
}

// run executes the V3 compiler driver with `args`.
@[markused]
pub fn run(args []string) {
	apply_v3_default_diagnostic_color()
	if args.len == 0 {
		eprintln(cli_usage())
		exit(1)
	}
	mut doc_index := -1
	mut skip_option_value := false
	for index, arg in args {
		if skip_option_value {
			skip_option_value = false
			continue
		}
		if v3_driver_option_consumes_value(arg) {
			skip_option_value = true
			continue
		}
		if arg.starts_with('-') {
			continue
		}
		if arg == 'doc' {
			doc_index = index
		}
		break
	}
	if doc_index >= 0 {
		// Keep tool compilation on V3 when a V3-built test or tool invokes
		// `@VEXE doc ...` directly. The tool's arguments belong to vdoc, so route
		// them after the source path exactly like `v3 run`.
		vdoc := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vdoc')
		mut tool_args := args[..doc_index].clone()
		tool_args << ['run', vdoc, 'doc']
		tool_args << args[doc_index + 1..]
		run(tool_args)
		return
	}
	macos_v3_fallback_file := os.getenv(macos_v3_fallback_file_env)
	macos_v3_c_error_dir := os.getenv(macos_v3_c_error_dir_env)
	// A delegated V3 process owns the fallback marker until it has successfully
	// produced its output. Specialized failures overwrite it below. Successful
	// run/test programs clear it before launch, so their exit status is never
	// mistaken for a compiler failure by the macOS driver.
	stage_macos_v3_compiler_error_fallback(macos_v3_fallback_file, 'command-line processing')

	mut input_file := ''
	mut output_file := ''
	mut explicit_output := false
	mut backend := 'c'
	mut backend_explicit := false
	mut target_os := os.user_os()
	mut target_os_explicit := false
	mut target_arch := pref.host_arch()
	mut target_arch_explicit := false
	mut c_compiler := 'cc'
	mut c_compiler_explicit := false
	mut c_compiler_arg_index := -1
	mut explicit_tcc := false
	mut retry_compilation := true
	mut gc_mode := 'none'
	mut enable_globals_compat := false
	mut is_prod := false
	mut no_prod_options := false
	mut is_shared := false
	mut is_livemain := false
	mut is_liveshared := false
	mut is_strict := false
	mut is_selfhost := false
	mut no_builtin := false
	mut no_preludes := false
	mut no_parallel := false
	mut parallel_cc := false
	mut no_prealloc := false
	mut no_cache := false
	mut no_skip_unused := false
	mut is_o := false
	mut no_memory_limit := false
	mut parallel_transform := true
	mut building_v := false
	mut ownership_mode := false
	mut verbose := false
	mut silent := false
	mut is_repl := false
	mut show_test_stats := v3_environment_show_test_stats()
	mut warn_impure_v := false
	mut warns_are_errors := false
	mut notes_are_errors := false
	mut check_overflow := false
	mut force_bounds_checking := false
	mut print_v_files := false
	mut print_watched_files := false
	mut only_check_syntax := false
	mut check_only := false
	mut show_cc := false
	mut show_c_output := false
	mut translated_mode := false
	mut keep_c := false
	mut skip_running := false
	mut is_debug := false
	mut is_c_debug := false
	mut c99 := false
	mut c99_explicit := false
	mut thread_stack_size := 0
	mut thread_stack_size_set := false
	mut all_backends := false
	mut compile_backends := []string{}
	mut user_defines := []string{}
	mut compile_values := map[string]string{}
	mut user_c_flags := []string{}
	mut should_run := false
	mut is_direct_vsh := false
	mut is_test_command := false
	mut is_checker_fixture := false
	mut coverage_dir := v3_environment_coverage_dir()
	mut dump_c_flags := ''
	mut generate_c_project := ''
	mut module_search_path_spec := ''
	mut file_list := []string{}
	mut run_args := []string{}
	mut run_only := v3_environment_run_only()
	mut print_fn_names := []string{}
	mut is_prof := false
	mut profile_file := ''
	mut profile_no_inline := false
	mut profile_fns := []string{}
	mut command_seen := false
	mut macos_sdk_root_cache := V3MacosSdkRootCache{}
	environment_c_flags := parse_v3_environment_flags('CFLAGS')
	environment_ld_flags := parse_v3_environment_flags('LDFLAGS')
	if environment_c_flags.len > 0 || environment_ld_flags.len > 0 {
		// Ambient flags can change arbitrary native compilation and link inputs.
		// Keep those invocations monolithic until the module cache records them.
		no_cache = true
	}
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
		option_accepts_dash_value := args[i] in ['-o', '-output'] && i + 1 < args.len
			&& args[i + 1] == '-'
		if v3_driver_option_requires_value(args[i])
			&& (i + 1 >= args.len || (args[i + 1].starts_with('-') && !option_accepts_dash_value)) {
			eprintln('option `${args[i]}` requires a value')
			exit(1)
		}
		if args[i] == '-cflags' && i + 1 >= args.len {
			eprintln('option `-cflags` requires a value')
			exit(1)
		}
		if args[i] == 'run' && input_file.len == 0 && !should_run {
			should_run = true
			command_seen = true
			i++
		} else if args[i] == 'build' && input_file.len == 0 && !should_run {
			skip_running = true
			command_seen = true
			i++
		} else if args[i] == 'test' && input_file.len == 0 && !should_run {
			is_test_command = true
			command_seen = true
			i++
		} else if args[i] in ['-o', '-output'] && i + 1 < args.len {
			output_file = args[i + 1]
			explicit_output = true
			if output_file.ends_with('.o') {
				is_o = true
				no_cache = true
			}
			i += 2
		} else if args[i] in ['-b', '-backend'] && i + 1 < args.len {
			backend = if args[i + 1] in ['js_browser', 'js_node'] { 'js' } else { args[i + 1] }
			backend_explicit = true
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
		} else if args[i] == '-no-prod-options' {
			no_prod_options = true
			i++
		} else if args[i] == '-shared' || args[i] == '--shared' {
			is_shared = true
			i++
		} else if args[i] == '-live' {
			is_livemain = true
			// Live builds need every module in the reloadable source artifact. A
			// persistent object cache also repeats native-header preprocessing and
			// can retain several GiB of transient state for Sokol-based examples.
			no_cache = true
			if 'livemain' !in user_defines {
				user_defines << 'livemain'
			}
			i++
		} else if args[i] == '-sharedlive' {
			is_liveshared = true
			no_cache = true
			is_shared = true
			if 'sharedlive' !in user_defines {
				user_defines << 'sharedlive'
			}
			i++
		} else if args[i] == '-selfhost' {
			is_selfhost = true
			i++
		} else if args[i] == '-building-v' || args[i] == '-building_v' {
			// The V compiler itself uses no generics, so monomorphization (and the rest
			// of the generics machinery) is pure overhead when building it.
			building_v = true
			i++
		} else if args[i] in ['-c99', '--c99', macos_v3_compat_c99_flag] {
			c99 = true
			if args[i] != macos_v3_compat_c99_flag {
				c99_explicit = true
				if 'c99' !in user_defines {
					user_defines << 'c99'
				}
			}
			i++
		} else if args[i] in ['-strict', '-cstrict'] {
			is_strict = true
			i++
		} else if args[i] == '-new-compiler' {
			// Accepted for symmetry with cmd/v's `-new-compiler`: V3 is already the
			// compiler at this point, so selecting it again is a no-op. cmd/v strips
			// this before forwarding; it is tolerated here for direct V3 invocations.
			i++
		} else if args[i] == '-ownership' || args[i] == '--ownership' {
			// The ownership checker itself is compiled into v3 via `-d ownership`.
			// The main V launcher pairs this flag with a target `-d ownership`, which
			// intentionally exposes `ownership` to target `$if` blocks and selects target
			// `_d_ownership.v` files. This flag enables the ownership analysis itself.
			ownership_mode = true
			i++
		} else if args[i] == '-no-parallel' || args[i] == '--no-parallel' {
			no_parallel = true
			i++
		} else if args[i] == '-parallel-cc' {
			parallel_cc = true
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
		} else if args[i] in ['-d', '-define'] && i + 1 < args.len {
			define := args[i + 1]
			record_user_define(mut user_defines, mut compile_values, define)
			i += 2
		} else if args[i] == '-dump-c-flags' {
			dump_c_flags = if i + 1 < args.len { args[i + 1] } else { '-' }
			// The dump is derived from the monolithic native compiler command below.
			// Avoid module/TinyCC cache paths that use a different link plan.
			no_cache = true
			i += if i + 1 < args.len { 2 } else { 1 }
		} else if args[i].starts_with('-d') && args[i].len > 2 {
			define := args[i][2..]
			record_user_define(mut user_defines, mut compile_values, define)
			i++
		} else if args[i] == '-gc' && i + 1 < args.len {
			gc_mode = args[i + 1]
			i += 2
		} else if args[i] == '-cc' && i + 1 < args.len {
			requested_compiler := args[i + 1]
			c_compiler = requested_compiler
			c_compiler_explicit = true
			c_compiler_arg_index = i
			i += 2
		} else if args[i] == '-thread-stack-size' && i + 1 < args.len {
			thread_stack_size = args[i + 1].int()
			thread_stack_size_set = true
			i += 2
		} else if args[i] in ['-cov', '-coverage'] && i + 1 < args.len {
			coverage_dir = os.real_path(args[i + 1])
			i += 2
		} else if args[i] == '-generate-c-project' && i + 1 < args.len {
			generate_c_project = os.real_path(args[i + 1])
			no_cache = true
			i += 2
		} else if args[i] == '-file-list' && i + 1 < args.len {
			for file in args[i + 1].split_any(',') {
				trimmed := file.trim_space()
				if trimmed.len > 0 {
					file_list << trimmed
				}
			}
			i += 2
		} else if args[i] == '-message-limit' && i + 1 < args.len {
			// V3 reports all diagnostics, but accepts V1's accumulation-limit
			// option so compiler invocations remain CLI-compatible.
			i += 2
		} else if args[i] == '-test-runner' && i + 1 < args.len {
			// V3 currently emits its normal test harness directly. Accept the
			// conventional runner selector so nested `@VEXE` test invocations stay
			// command-line compatible with V1.
			i += 2
		} else if args[i] == '-run-only' && i + 1 < args.len {
			run_only.clear()
			for pattern in args[i + 1].split_any(',') {
				trimmed := pattern.trim_space()
				if trimmed.len > 0 {
					run_only << trimmed
				}
			}
			i += 2
		} else if args[i] == '-printfn' && i + 1 < args.len {
			print_fn_names << args[i + 1].split(',')
			no_cache = true
			i += 2
		} else if args[i] in ['-prof', '-profile'] {
			parsed_profile_file, profile_file_consumed := v3_profile_optional_arg_value(args, i, command_seen)
			profile_file = parsed_profile_file
			is_prof = true
			no_cache = true
			if 'profile' !in user_defines {
				user_defines << 'profile'
			}
			i += if profile_file_consumed { 2 } else { 1 }
		} else if args[i] == '-profile-fns' && i + 1 < args.len {
			for fn_name in args[i + 1].split(',') {
				if fn_name.len > 0 {
					profile_fns << fn_name
				}
			}
			i += 2
		} else if args[i] == '-profile-no-inline' {
			profile_no_inline = true
			i++
		} else if args[i] == '-path' && i + 1 < args.len {
			module_search_path_spec = args[i + 1]
			i += 2
		} else if args[i] == '-cflags' && i + 1 < args.len {
			parsed_c_flags := cmdexec.split_args(args[i + 1]) or {
				eprintln('invalid `-cflags` value: ${err.msg()}')
				exit(1)
			}
			user_c_flags << parsed_c_flags
			i += 2
		} else if args[i] in ['-g', '-cg', '-cdebug'] {
			is_debug = true
			if args[i] in ['-cg', '-cdebug'] {
				is_c_debug = true
			}
			user_c_flags << '-g'
			i++
		} else if args[i] == '-autofree' {
			ownership_mode = true
			if 'autofree' !in user_defines {
				user_defines << 'autofree'
			}
			i++
		} else if args[i] == '-v' {
			verbose = true
			i++
		} else if args[i] == '-silent' {
			silent = true
			if 'silent' !in user_defines {
				user_defines << 'silent'
			}
			i++
		} else if args[i] == macos_v3_internal_quiet_flag {
			silent = true
			i++
		} else if args[i] == '-showcc' {
			show_cc = true
			i++
		} else if args[i] == '-translated' {
			translated_mode = true
			i++
		} else if args[i] == '-repl' {
			// vrepl compiles each accumulated snippet with this marker. V3 already
			// accepts module-less main input; the marker also suppresses transient
			// unused-code notices while the snippet is being assembled.
			is_repl = true
			i++
		} else if args[i] == '-check-overflow' {
			check_overflow = true
			i++
		} else if args[i] == '-manualfree' {
			ownership_mode = false
			user_defines = user_defines.filter(it.all_before('=').trim_space() != 'autofree')
			i++
		} else if args[i] == '-show-c-output' {
			show_c_output = true
			i++
		} else if args[i] in ['-color', '-nocolor'] {
			apply_v3_diagnostic_color_option(args[i])
			i++
		} else if args[i] == '-apk' {
			// Accepted V1 compatibility switches. V3 always emits direct C,
			// applies ownership cleanup, and forwards C failures.
			i++
		} else if args[i] == '-nofloat' {
			if 'nofloat' !in user_defines {
				user_defines << 'nofloat'
			}
			i++
		} else if args[i] == '-no-bounds-checking' {
			if 'no_bounds_checking' !in user_defines {
				user_defines << 'no_bounds_checking'
			}
			i++
		} else if args[i] == '-force-bounds-checking' {
			force_bounds_checking = true
			user_defines =
				user_defines.filter(it.all_before('=').trim_space() != 'no_bounds_checking')
			i++
		} else if args[i] == '-checker-fixture' {
			is_checker_fixture = true
			i++
		} else if args[i] == '-keepc' {
			keep_c = true
			i++
		} else if args[i] == '-skip-running' {
			skip_running = true
			i++
		} else if args[i] == '-check' {
			check_only = true
			skip_running = true
			no_cache = true
			i++
		} else if args[i] == '-stats' {
			show_test_stats = true
			no_cache = true
			i++
		} else if args[i] == '-Wimpure-v' {
			warn_impure_v = true
			// Cached module headers omit function bodies, so inspect source for every import.
			no_cache = true
			i++
		} else if args[i] == '-W' {
			warns_are_errors = true
			i++
		} else if args[i] == '-N' {
			notes_are_errors = true
			i++
		} else if args[i] == '-print-v-files' {
			print_v_files = true
			i++
		} else if args[i] == '-print-watched-files' {
			print_watched_files = true
			i++
		} else if args[i] == '-check-syntax' {
			only_check_syntax = true
			no_cache = true
			i++
		} else if args[i] == '-no-retry-compilation' {
			retry_compilation = false
			i++
		} else if args[i] in ['-show-timings', '-w', '-usecache', '-new-generic-solver'] {
			// v3 already reports phase metrics, suppresses C warnings, leaves
			// explicit-output tests unrun, caches modules by default, and uses
			// its current generic solver without a legacy selection switch.
			// Accept the corresponding V flags for compatibility.
			i++
		} else if args[i] == '-no-prealloc' || args[i] == '--no-prealloc' {
			no_prealloc = true
			i++
		} else if args[i] == '-nocache' || args[i] == '--no-cache' {
			no_cache = true
			i++
		} else if args[i] == '-no-builtin' {
			no_builtin = true
			no_cache = true
			i++
		} else if args[i] == '-no-preludes' {
			no_preludes = true
			i++
		} else if args[i] == '-no-skip-unused' {
			no_skip_unused = true
			no_cache = true
			i++
		} else if args[i] == '-is_o' {
			is_o = true
			no_cache = true
			i++
		} else if args[i] == '-skip-unused' {
			no_skip_unused = false
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
			if input_file.ends_with('.vsh') {
				is_direct_vsh = !should_run
				should_run = true
			}
			i++
		}
	}
	if force_bounds_checking {
		// This option wins regardless of its ordering relative to
		// `-no-bounds-checking`, matching the established parser contract.
		user_defines = user_defines.filter(it.all_before('=').trim_space() != 'no_bounds_checking')
	}
	if is_prof && backend !in ['c', 'fastc'] {
		eprintln('option `-profile` is only supported by the C backend')
		exit(1)
	}
	should_run = should_run && !skip_running
	if is_o && (backend !in ['c', 'fastc'] || !explicit_output
		|| (!output_file.ends_with('.c') && !output_file.ends_with('.o'))) {
		eprintln('option `-is_o` requires the C backend and an explicit `.c` or `.o` output file')
		exit(1)
	}
	if !is_checker_fixture && input_is_legacy_diagnostic_fixture(input_file) {
		// v/compiler_errors_test.v predates `-checker-fixture` and invokes every
		// adjacent `.vv`/`.out` fixture directly. Keep those subprocesses on the
		// same stable diagnostic path as the V3 fixture runner.
		is_checker_fixture = true
		no_cache = true
	}
	mut current_no_parallel := no_parallel
	if is_prof {
		// Profile counters are assigned in function emission order and accumulated
		// in one generator, just as they are in V1.
		current_no_parallel = true
		no_cache = true
	}
	if coverage_dir.len > 0 {
		current_no_parallel = true
		no_cache = true
	}
	if print_fn_names.len > 0 {
		// Function snippets are emitted to stdout in deterministic generation order.
		current_no_parallel = true
		no_cache = true
	}
	mut current_parallel_transform := parallel_transform
	if current_no_parallel {
		current_parallel_transform = false
	}

	if input_file == '' {
		eprintln('no input file')
		exit(1)
	}
	if input_file != '-' && !os.exists(input_file) {
		eprintln("builder error: ${input_file} doesn't exist")
		exit(1)
	}
	configure_selfhost_parallelism(building_v)
	if generate_c_project.len > 0 {
		if backend != 'c' {
			eprintln('`-generate-c-project` is currently supported only for the C backend')
			exit(1)
		}
		if os.exists(generate_c_project) && !os.is_dir(generate_c_project) {
			eprintln('`-generate-c-project` expects a directory path, got file: ${generate_c_project}')
			exit(1)
		}
		os.mkdir_all(generate_c_project) or {
			eprintln('cannot create `-generate-c-project` directory ${generate_c_project}: ${err.msg()}')
			exit(1)
		}
		source_name := os.base(default_bin_file_for_input(input_file)) + '.c'
		output_file = os.join_path_single(generate_c_project, source_name)
		explicit_output = true
	} else if explicit_output && (output_file.ends_with('/') || output_file.ends_with('\\')) {
		os.mkdir_all(output_file) or {
			eprintln('cannot create output directory ${output_file}: ${err.msg()}')
			exit(1)
		}
		output_file = os.join_path_single(output_file, os.base(default_bin_file_for_input(input_file)))
	}
	if is_debug && 'debug' !in user_defines {
		user_defines << 'debug'
		record_compile_value(mut compile_values, 'debug')
	}
	if user_defines.any(it.all_before('=').trim_space() == 'no_gc_thread_local_alloc')
		&& '-D GC_THREADS=1' !in user_c_flags {
		// Keep `-dump-c-flags` compatible with V1 even though V3 currently uses
		// its no-GC runtime. Projects use this define to inspect the portable
		// Boehm thread flags without selecting or linking the collector.
		user_c_flags << '-D GC_THREADS=1'
	}
	if is_test_command && fixturetest.is_diagnostic_fixture_dir(input_file) {
		exit(fixturetest.run(os.executable(), input_file, args))
	}
	if os.getenv(v3_embedded_env) != '1' {
		maybe_delegate_v3_to_vvmrc(input_file, verbose)
	}
	if backend == 'js' {
		js_output := if output_file.len > 0 {
			output_file
		} else {
			default_bin_file_for_input(input_file) + '.js'
		}
		emit_v3_js_compat_program(input_file, js_output) or {
			eprintln(err.msg())
			exit(1)
		}
		clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
		if should_run {
			mut node_args := [js_output]
			node_args << run_args
			result := cmdexec.run('node', node_args)
			if result.output.len > 0 {
				print(result.output)
			}
			if result.exit_code != 0 {
				exit(result.exit_code)
			}
		}
		return
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
		if define_name == 'ownership' && backend != 'fastc' && !ownership_checker_compiled() {
			eprintln('ownership support is not compiled into this v3 executable')
			exit(1)
		}
	}
	if ownership_mode && backend != 'fastc' && !ownership_checker_compiled() {
		eprintln('ownership support is not compiled into this v3 executable')
		exit(1)
	}
	if backend !in ['c', 'fastc', 'arm64', 'wasm', 'eval'] {
		eprintln('unknown backend `${backend}`; expected c, fastc, arm64, wasm, or eval')
		exit(1)
	}
	if backend == 'arm64' && target_os != 'macos' && 'no_gettid' !in user_defines {
		// The native ARM64 linker emits Mach-O and cannot resolve Linux's gettid symbol.
		user_defines << 'no_gettid'
	}
	for requested in compile_backends {
		for name in requested.split(',') {
			if name.trim_space() !in ['c', 'fastc', 'arm64', 'aarch64', 'wasm', 'wasm32', 'eval'] {
				eprintln('unknown compile backend `${name.trim_space()}`')
				exit(1)
			}
		}
	}
	if backend == 'wasm' {
		if !target_os_explicit || target_os in ['browser', 'wasi'] {
			// V1's native wasm CLI exposes `browser`/`wasi` target labels. V3
			// currently has one canonical wasm32 target; keep accepting those
			// labels while selecting the same wasm source set and ABI.
			target_os = 'wasm32_emscripten'
		}
	}
	if !target_arch_explicit
		&& pref.normalized_os(target_os.trim_space().to_lower()) == 'wasm32_emscripten' {
		target_arch = 'wasm32'
	}
	target := pref.target_from(target_os, target_arch) or {
		eprintln(err.msg())
		exit(1)
	}
	// V's platform `int` is 64-bit on 64-bit targets and 32-bit on 32-bit ones;
	// pin the C spelling from the target width before any checking or codegen.
	types.set_platform_int_bits(target.pointer_bits)
	constraint_ccompiler := if backend == 'arm64' {
		'tinyc'
	} else {
		effective_c_compiler_name(c_compiler, target)
	}
	incompatible_direct_test := v3_direct_test_input_is_incompatible(is_test_command, input_file, backend, target, constraint_ccompiler, is_prod, user_defines)
	if incompatible_direct_test {
		// Directory test discovery already excludes incompatible backend/platform files.
		// Apply the same backend, platform, and `// vtest build:` rules to a direct single-file
		// test before parsing it; otherwise unavailable symbols and dependencies emit
		// misleading diagnostics instead of reporting a skip.
		if !silent {
			println('SKIP ${input_file}')
		}
		clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
		return
	}
	if is_linux_wayland_only_session(target.os, os.getenv('DISPLAY'), os.getenv('WAYLAND_DISPLAY'), os.getenv('XDG_SESSION_TYPE'))
		&& !user_defines.any(it.all_before('=').trim_space() == 'linux_wayland_session') {
		user_defines << 'linux_wayland_session'
	}
	cmd_v_build := input_is_cmd_v(input_file)
	cmd_v_module_input := input_loads_cmd_v_module(input_file)
	v3_compiler_tree_input := input_is_v3_compiler_tree(input_file)
	// Neither compiler entry point uses generics. Keep self-builds off the generic
	// reachability and monomorphization paths without requiring an explicit flag.
	// -building-v can force the same mode for another known non-generic input.
	if input_implies_building_v(input_file) || cmd_v_build {
		building_v = true
	}
	if backend == 'fastc' && 'fastc_real_builtin' in user_defines {
		// Opt-in: compile an ordinary program through FastC's real-`builtin` path
		// (real `struct string`, error system, and the full runtime) instead of the
		// bootstrap `const char*` runtime. This is how FastC reaches real apps.
		building_v = true
	}
	// Large serial compiler-module builds create the same transform and C-generation
	// scratch state as parallel builds. Keep that state disposable; `-no-parallel`
	// controls worker creation independently from arena lifetime.
	scope_prealloc_stages := should_scope_prealloc_stages()
	// Function checking still creates substantial short-lived state in serial
	// mode for large import graphs. Scope each function independently there too.
	scope_prealloc_check := should_scope_prealloc_stages()
	scope_prealloc_cgen := should_scope_prealloc_cgen()
	// The selective transform promotion path is designed around worker-owned
	// results outside the disposable stage arena.
	scope_prealloc_transform := scope_prealloc_stages
	// Markused can lazily create the compilation worker pool. When parsing was
	// serial, keep that pool in the compilation arena so close_workers never
	// observes a pool allocated in a released markused scope.
	scope_prealloc_markused := scope_prealloc_stages && !current_no_parallel
	$if linux {
		// Large preallocated user graphs retain worker-local arenas between phases.
		// Bound Linux pools to the configuration used for the scoped-memory path;
		// high VJOBS values otherwise multiply both RSS and shared-cache pressure.
		if !building_v && scope_prealloc_stages && runtime.nr_jobs() > scoped_linux_user_job_limit {
			workers.limit_pool_size(scoped_linux_user_job_limit - 1)
		}
	}
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
		// The FastC backend honors it too: it emits the arena root
		// `g_memory_block` as per-thread storage (a pthread key under bundled
		// TinyCC, which lacks thread-local storage), so its worker-thread
		// generations bump-allocate safely (see fastc_write_prealloc_tls_global).
		if !no_prealloc && 'prealloc' !in user_defines {
			user_defines << 'prealloc'
		}
	}
	if no_prealloc {
		user_defines = user_defines.filter(it != 'prealloc')
	}

	mut bin_file := ''
	mut c_only := false
	mut c_to_stdout := false
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
	} else if backend in ['c', 'fastc'] && output_file == '-' {
		c_only = true
		c_to_stdout = true
		bin_file = ''
		output_file = os.join_path_single(os.vtmp_dir(), 'v3_stdout_${os.getpid()}_${tempname.unique_token()}.c')
	} else if backend in ['c', 'fastc'] && output_file.ends_with('.c') {
		c_only = true
		bin_file = output_file.all_before_last('.c')
	} else {
		bin_file = output_file
		if is_shared {
			bin_file = with_shared_library_postfix(bin_file, target.os)
		}
		output_file = bin_file + '.c'
	}
	if backend in ['c', 'fastc'] {
		target_bin_file := c_executable_bin_file_for_target(bin_file, target.os, is_shared, is_o, c_only)
		if target_bin_file != bin_file {
			bin_file = target_bin_file
			output_file = bin_file + '.c'
		}
	}
	binary_existed_before := os.exists(bin_file)
	remove_binary_after_run := should_run && !is_direct_vsh && !explicit_output && !keep_c
		&& !binary_existed_before

	// Decide which backend modules to compile into the output. By default only the C
	// backend is built; the fastc/arm64/wasm/eval backends (and the whole SSA pipeline that the
	// arm64 backend pulls in: v3.ssa + v3.ssa.optimize) are skipped entirely. When compiling
	// the V compiler itself this avoids parsing/checking/transforming/cgen-ing ~30k lines of
	// unused backend code, which measurably speeds up the self-host build. The `skip_*`
	// defines drive two things in lock-step: `$if !skip_* ?` gates in main() make the parser
	// drop the dispatch blocks (so the backend symbols are never referenced), and
	// resolve_imports skips parsing the corresponding module directories.
	// `-all-backends` keeps everything; `-compile-backend <name>` opts a specific backend back
	// in; the active `-b` target backend is always force-included.
	mut include_fastc := all_backends
	mut include_arm64 := all_backends
	mut include_wasm := all_backends
	mut include_eval := all_backends
	for cb in compile_backends {
		for name in cb.split(',') {
			match name.trim_space() {
				'fastc' {
					include_fastc = true
				}
				'arm64', 'aarch64' {
					include_arm64 = true
				}
				'wasm', 'wasm32' {
					include_wasm = true
				}
				'eval' {
					include_eval = true
				}

				// 'c' is always built; there is no native amd64 backend in v3 yet.
				else {}
			}
		}
	}
	match backend {
		'fastc' {
			include_fastc = true
		}
		'arm64' {
			include_arm64 = true
		}
		'wasm' {
			include_wasm = true
		}
		'eval' {
			include_eval = true
		}
		else {}
	}

	if !include_fastc {
		user_defines << 'skip_fastc'
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
	fastc_compiler_entry := backend == 'fastc' && input_is_v3_compiler_entry(input_file)
	fastc_selfhost_build := backend == 'fastc' && (is_selfhost || fastc_compiler_entry)
	if fastc_selfhost_build {
		// Select the scanner-to-C driver in the first generated compiler. A direct
		// `-b fastc vlib/v3/v3.v` build is a self-host build too; do not require the
		// internal `-d fastc_selfhost` implementation detail at the command line.
		// Descendant FastC compilers preserve the same define in v3.fastcdriver.
		record_user_define(mut user_defines, mut compile_values, 'fastc_selfhost')
	}

	mut b := bench.new()
	driver_sw := time.new_stopwatch()
	if silent || c_to_stdout {
		b.set_quiet()
	}
	if no_memory_limit {
		b.disable_memory_limit()
	} else if v3_compiler_tree_input {
		// Compiler-module tests retain test-runner state in addition to the full
		// compiler AST, so keep their guard separately configurable.
		b.use_compiler_tree_memory_limit()
	} else if building_v || cmd_v_module_input {
		// Self-host transformation temporarily retains both the source and rewritten
		// compiler ASTs. Direct cmd/v module tests load the same compiler sources.
		b.use_self_host_memory_limit()
	}
	b.start_memory_monitor()
	defer {
		b.stop_memory_monitor()
	}
	mut c_object_cache_stats := CObjectCacheStats{}
	if !silent && !c_to_stdout {
		println('=== v3 benchmark ===')
	}

	// Parse directly to flat AST
	mut prefs := pref.new_preferences()
	if os.getenv('FASTC_BENCH_PHASES') != '' {
		eprintln('fastc-phase driver.prefs ${driver_sw.elapsed().microseconds()}us')
	}
	prefs.target = target
	prefs.thread_stack_size = if thread_stack_size_set {
		thread_stack_size
	} else {
		target.default_thread_stack_size()
	}
	prefs.backend = backend
	prefs.vroot = if fastc_compiler_entry {
		// A directly invoked host compiler may live outside the checkout (for example,
		// a production benchmark binary in /tmp). The explicit compiler entry owns
		// this build, so resolve builtin and vlib beside that input instead of VEXE.
		resolve_vroot_for_input(prefs.vroot, os.real_path(input_file))
	} else if pref.has_macos_v3_caller_environment() && prefs.vexe.len > 0 {
		// The macOS dispatcher sets VEXE to the invoking compiler. Preserve that
		// checkout instead of selecting another V checkout around the input.
		os.real_path(os.dir(prefs.vexe))
	} else {
		resolve_vroot_for_input(prefs.vroot, input_file)
	}
	if !c_compiler_explicit && os.user_os() == 'windows' && target.os == 'windows' {
		bundled_tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
		if os.is_executable(bundled_tcc) {
			c_compiler = bundled_tcc
		}
	}
	effective_c_compiler := if backend == 'arm64' {
		'tinyc'
	} else {
		effective_c_compiler_name(c_compiler, target)
	}
	explicit_tcc = c_compiler_explicit && effective_c_compiler == 'tinyc'
	add_v3_tcc_compat_defines(mut user_defines, target.os, target.arch, is_shared, explicit_tcc)
	if os.getenv('FASTC_BENCH_PHASES') != '' {
		eprintln('fastc-phase driver.defines ${driver_sw.elapsed().microseconds()}us')
	}
	prefs.ccompiler = effective_c_compiler
	prefs.no_parallel = current_no_parallel
	prefs.c99 = c99
	prefs.force_bounds_checking = force_bounds_checking
	prefs.enable_globals = enable_globals_compat
	prefs.user_defines = user_defines
	prefs.compile_values = compile_values.clone()
	prefs.module_search_paths = expand_v3_module_search_paths(module_search_path_spec, prefs.vroot)
	if explicit_tcc && c_compiler in ['tcc', 'tinyc'] {
		bundled_tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
		if os.is_executable(bundled_tcc) {
			c_compiler = bundled_tcc
		}
	}
	prefs.vhash = os.getenv(macos_v3_vhash_env)
	if prefs.vhash == '' {
		prefs.vhash = @VHASH
	}
	prefs.vcurrent_hash = os.getenv(macos_v3_vcurrent_hash_env)
	if prefs.vcurrent_hash == '' {
		prefs.vcurrent_hash = @VCURRENTHASH
	}
	prefs.selfhost = is_selfhost || fastc_selfhost_build
	prefs.building_v = building_v
	prefs.is_prod = is_prod
	prefs.is_debug = is_debug
	prefs.is_livemain = is_livemain
	prefs.is_liveshared = is_liveshared
	prefs.is_shared = is_shared
	prefs.no_builtin = no_builtin
	prefs.no_preludes = no_preludes
	prefs.verbose = verbose
	if verbose {
		eprintln('v.pref.lookup_path: ${os.join_path(prefs.vroot, 'vlib')}')
	}
	prefs.supports_inline_asm = is_checker_fixture
	if backend == 'fastc' {
		$if skip_fastc ? {
			eprintln('fastc support is not compiled into this v3 executable')
			exit(1)
		} $else {

			// FastC is a standalone parser that emits C while consuming scanner tokens.
			// Never let an unsupported FastC input continue into the AST frontend below.
			clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
			if !input_file.ends_with('.v') || !os.is_file(input_file) || file_list.len > 0 {
				eprintln('fastc requires exactly one `.v` entry file')
				exit(1)
			}
			fastc_artifact_file := if c_only {
				if c_to_stdout { '' } else { output_file }
			} else {
				bin_file
			}
			if fastc_artifact_file != ''
				&& canonical_v3_fastc_output_path(fastc_artifact_file) == os.real_path(input_file) {
				eprintln('fastc output path `${fastc_artifact_file}` aliases input source `${input_file}`')
				exit(1)
			}
			if os.getenv('FASTC_BENCH_PHASES') != '' {
				eprintln('fastc-phase driver.entry_checks ${driver_sw.elapsed().microseconds()}us')
			}
			fastc_host := pref.host_target()
			fastc_cross_target := target.os != fastc_host.os || target.arch != fastc_host.arch
			if !c_only && fastc_cross_target {
				eprintln('fastc can only build executables for the host target; use `-o file.c` for cross-target C output')
				exit(1)
			}
			mut unsupported_modes := []string{}
			if is_test_command || is_v3_test_file(input_file, backend, target)
				|| is_v3_test_file(input_file, 'c', target) || is_checker_fixture {
				unsupported_modes << 'test/checker mode'
			}
			if is_prod {
				unsupported_modes << '`-prod`'
			}
			if is_shared || is_livemain || is_liveshared {
				unsupported_modes << 'shared/live builds'
			}
			if is_o {
				unsupported_modes << 'object-file output'
			}
			if is_prof || coverage_dir.len > 0 {
				unsupported_modes << 'profiling/coverage'
			}
			if ownership_mode || 'ownership' in prefs.user_defines {
				unsupported_modes << 'ownership/autofree'
			}
			if only_check_syntax || check_only {
				unsupported_modes << 'syntax/check-only mode'
			}
			if warn_impure_v {
				unsupported_modes << '`-Wimpure-v`'
			}
			if print_fn_names.len > 0 || print_v_files || print_watched_files
				|| dump_c_flags.len > 0 || generate_c_project.len > 0 {
				unsupported_modes << 'compiler inspection output'
			}
			if c99_explicit || is_strict || check_overflow {
				unsupported_modes << 'strict/checked C modes'
			}
			if c_compiler_explicit {
				unsupported_modes << 'custom C compilers'
			}
			if no_builtin || no_preludes {
				unsupported_modes << 'custom builtin/prelude modes'
			}
			if 'no_main' in prefs.user_defines {
				unsupported_modes << '`-d no_main`'
			}
			if translated_mode || is_repl {
				unsupported_modes << 'translated/REPL mode'
			}
			if unsupported_modes.len > 0 {
				eprintln('fastc parser does not support ${unsupported_modes.join(', ')}')
				exit(1)
			}
			if 'v3_backend' !in prefs.user_defines {
				prefs.user_defines << 'v3_backend'
			}
			if !fastc_cross_target {
				// Same-target FastC output is compiled by bundled TinyCC regardless of
				// the host default, so compile-time compiler branches must see TinyCC.
				prefs.ccompiler = 'tinyc'
				add_v3_tcc_compat_defines(mut prefs.user_defines, target.os, target.arch, false, true)
			}
			// FASTC_BENCH_LOOP=N repeats generation in-process so an external
			// sampler can profile it (see the FastC section of the v3 README).
			for _ in 0 .. os.getenv('FASTC_BENCH_LOOP').int() {
				fastc.generate_files_with_source_paths([input_file], prefs) or {
					eprintln(err.msg())
					exit(1)
					return
				}
			}
			if os.getenv('FASTC_BENCH_PHASES') != '' {
				eprintln('fastc-phase driver.setup ${driver_sw.elapsed().microseconds()}us')
			}
			fastc_generation := fastc.generate_files_with_source_paths([input_file], prefs) or {
				eprintln(err.msg())
				exit(1)
				return
			}
			if os.getenv('FASTC_BENCH_PHASES') != '' {
				eprintln('fastc-phase driver.generated ${driver_sw.elapsed().microseconds()}us')
			}
			fastc_artifact_path := canonical_v3_fastc_output_path(fastc_artifact_file)
			for source_path in fastc_generation.source_paths {
				if fastc_artifact_path != '' && fastc_artifact_path == source_path
					&& source_path != os.real_path(input_file) {
					eprintln('fastc output path `${fastc_artifact_file}` aliases imported source `${source_path}`')
					exit(1)
				}
			}
			fastc_pieces := fastc_generation.c_pieces
			fastc_source_size := fastc_generation.c_size()
			b.step('fastc parse+gen')
			if c_only && fastc_cross_target {
				b.metric('generated C size', fastc_source_size, 'bytes')
				publish_v3_fastc_c_source(fastc_pieces, output_file, c_to_stdout) or {
					eprintln(err.msg())
					exit(1)
				}
				b.print_report()
				clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
				return
			}
			// Validate same-target generated C before publishing it. C-only builds use a
			// throwaway executable; normal builds keep the binary produced by bundled TinyCC.
			fastc_bin_file := if c_only {
				os.join_path_single(os.vtmp_dir(), 'v3_fastc_validate_${os.getpid()}_${tempname.unique_token()}')
			} else {
				bin_file
			}
			fastc_sdk_root := if prefs.normalized_target_os() == 'macos' {
				macos_sdk_root_cache.get()
			} else {
				''
			}
			fastc_result := compile_v3_fastc_source(fastc_pieces, fastc_generation.units, fastc_bin_file, prefs, environment_c_flags, fastc_generation.c_flags, user_c_flags, environment_ld_flags, fastc_sdk_root, is_debug, fastc_generation.uses_threads)
			if (!silent || show_cc) && fastc_result.command.len > 0 {
				if c_to_stdout {
					eprintln('  > ${fastc_result.command}')
				} else {
					println('  > ${fastc_result.command}')
				}
			}
			if show_c_output && fastc_result.output.len > 0 {
				header := '======== Output of TinyCC fastc ========'
				if c_to_stdout {
					eprintln(header)
					eprintln(fastc_result.output.trim_space())
					eprintln('='.repeat(header.len))
				} else {
					println(header)
					println(fastc_result.output.trim_space())
					println('='.repeat(header.len))
				}
			}
			if c_only {
				os.rm(fastc_bin_file) or {}
			}
			if !fastc_result.success {
				if fastc_result.command.len == 0 {
					eprintln('fastc requires the bundled TinyCC executable')
				} else if !show_c_output && fastc_result.output.len > 0 {
					eprintln(fastc_result.output.trim_space())
				}
				exit(1)
			}
			b.step('tcc')
			b.metric('generated C size', fastc_source_size, 'bytes')
			if c_only {
				publish_v3_fastc_c_source(fastc_pieces, output_file, c_to_stdout) or {
					eprintln(err.msg())
					exit(1)
				}
				b.print_report()
				clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
				return
			}
			if backend_explicit {
				fastc.write_c_pieces(bin_file + '.c', fastc_pieces) or {
					eprintln('failed to retain generated fastc output ${bin_file}.c: ${err.msg()}')
					exit(1)
				}
			}
			if keep_c {
				keep_c_file := keep_c_output_file(bin_file)
				fastc.write_c_pieces(keep_c_file, fastc_pieces) or {
					eprintln('failed to retain generated fastc output ${keep_c_file}: ${err.msg()}')
					exit(1)
				}
			}
			if should_run {
				run_result := run_binary(bin_file, run_args)
				if remove_binary_after_run {
					os.rm(bin_file) or {}
				}
				if run_result != 0 {
					exit(run_result)
				}
				b.step('run')
			}
			b.print_report()
			return
		}
	}
	minimal_literal_output := !is_prof
		&& input_uses_minimal_literal_output_builtin(input_file, prefs, is_test_command, is_checker_fixture)
	host_target := pref.host_target()
	// `-keepc` and explicit `-b c` promise a complete generated C translation unit.
	// The module cache splits imported implementations into separate objects, so its main source
	// alone cannot reproduce the build. Literal output uses a deliberately reduced
	// builtin source set, which likewise must remain a monolithic translation unit.
	cache_enabled := backend == 'c' && !c_only && !no_cache && !no_skip_unused && !no_builtin
		&& !keep_c && !backend_explicit && !c_compiler_explicit && !minimal_literal_output
		&& c_compiler == 'cc' && target.os == host_target.os && target.arch == host_target.arch
		&& !input_owns_builtin_bundle_module(input_file, prefs.vroot)
	cc_identity := if cache_enabled { default_cc_identity() } else { '' }
	compiler_signature := if cache_enabled { v3_cache_compiler_signature(prefs.vroot) } else { '' }
	effective_warns_are_errors := v3_effective_warns_are_errors(warns_are_errors, is_prod)
	cache_salt := [
		'compiler=${compiler_signature}',
		'cc=${cc_identity}',
		'ccompiler=${prefs.ccompiler}',
		'vexe=${prefs.vexe}',
		'backend=${backend}',
		'target=${prefs.normalized_target_os()}',
		'target_arch=${prefs.normalized_target_arch()}',
		'prod=${is_prod}',
		'no_prod_options=${no_prod_options}',
		'debug=${is_debug}',
		'c_debug=${is_c_debug}',
		'shared=${is_shared}',
		'selfhost=${is_selfhost}',
		'c99=${c99}',
		'thread_stack_size=${prefs.thread_stack_size}',
		'module_search_paths=${prefs.module_search_paths.join(',')}',
		'macos_v3_caller_environment=${pref.has_macos_v3_caller_environment()}',
		'ownership=${ownership_mode}',
		'translated=${translated_mode}',
		'enable_globals=${enable_globals_compat}',
		'check_overflow=${check_overflow}',
		'force_bounds_checking=${prefs.force_bounds_checking}',
		'warns_are_errors=${effective_warns_are_errors}',
		'notes_are_errors=${notes_are_errors}',
		'test=${is_test_command || is_v3_test_file(input_file, backend, target)}',
		'show_test_stats=${show_test_stats}',
		'run_only=${v3_run_only_cache_identity(run_only)}',
		'defines=${prefs.user_defines.join(',')}',
	].join('\n')
	build_pseudo_values := [prefs.build_date, prefs.build_time, prefs.build_timestamp].join('\n')
	version_pseudo_values := [prefs.vhash, prefs.vcurrent_hash].join('\n')
	cache_manager := modulecache.new_manager(prefs.vroot, cache_salt, cache_enabled, build_pseudo_values, version_pseudo_values)
	program_cache_enabled := persistent_program_cache_enabled(cache_enabled, is_test_command
		|| is_v3_test_file(input_file, backend, target), os.vtmp_dir())
	force_cache_source := os.getenv('V3_CACHE_FORCE_SOURCE') == '1'
	mut cache_no_parallel_cgen := current_no_parallel
	stage_macos_v3_compiler_error_fallback(macos_v3_fallback_file, 'source parsing')
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
	mut builtin_files := pref.get_v_files_from_dir_for_target(builtin_dir, builtin_defines, prefs.target)
	if no_builtin {
		builtin_files = []
	}
	if minimal_literal_output {
		builtin_files = builtin_files.filter(is_minimal_literal_output_builtin_file(it))
	}
	bundle_sources := builtin_bundle_source_files(prefs, builtin_files)
	mut cache_state := V3ModuleCacheState{
		manager: cache_manager
		bundle_sources: bundle_sources
		bundle_source_paths: module_cache_source_path_set(bundle_sources)
		force_source: force_cache_source
		module_sources: map[string][]string{}
		module_import_paths: map[string]string{}
		module_dependencies: map[string][]string{}
		module_external_inputs: map[string][]string{}
		module_native_roots: map[string][]string{}
		native_root_contexts: map[string][]string{}
		native_root_owners: map[string]string{}
		external_input_signatures: map[string]string{}
		external_input_digests: map[string]string{}
		dependency_metadata: map[string]string{}
		cached_source_digests: map[string]string{}
		fallback_required_modules: map[string]bool{}
		fallback_warmup_modules: map[string]bool{}
		parsed_from_source: map[string]bool{}
		source_body_modules: map[string]bool{}
		native_source_modules: map[string]bool{}
		native_type_declarations: map[string]string{}
		native_declared_functions: map[string]map[string]bool{}
		objects: map[string]string{}
		headers: map[string]string{}
	}
	cache_state.module_sources['builtin'] = builtin_files
	mut files := []string{}
	mut loaded_cached_bundle := false
	if !force_cache_source {
		if bundle_object := cache_manager.valid_object('builtin', bundle_sources) {
			if builtin_header := cache_manager.valid_header('builtin', builtin_files) {
				record_v3_cached_source_digests(mut cache_state, builtin_header.source_digests)
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
	mut parse_timing := V3ParseTiming{}
	parse_files_dispatch_profiled(mut p, files, !current_no_parallel, mut parse_timing)
	mut a := p.a
	if !current_no_parallel {
		// Later parallel stages can run inside disposable arenas. Ensure the shared
		// pool itself is owned by the compilation arena before any such stage starts.
		a.ensure_workers(runtime.nr_jobs() - 1)
	}
	defer {
		a.close_workers()
	}
	a.user_code_start = a.nodes.len
	if minimal_literal_output {
		suppress_minimal_literal_output_builtin_imports(mut a)
	}

	// Test mode is a compile-time define as well as a harness mode. Install it
	// after parsing builtin, but before collecting and parsing user inputs, so
	// `$if test` and `_d_test.v` apply to both file and directory test commands.
	if 'test' !in prefs.user_defines && (is_test_command || is_v3_test_file(input_file, backend, target)) {
		prefs.user_defines << 'test'
	}

	// Parse user input: single file or directory
	mut user_files := []string{}
	if input_file.ends_with('.v') || input_file.ends_with('.vv') {
		user_files << input_file
		user_files = expand_single_test_file_inputs(user_files, prefs)
	} else if os.is_dir(input_file) {
		user_files = v3_directory_user_files(input_file, prefs, is_test_command, false) or {
			eprintln(err.msg())
			exit(1)
		}
		if user_files.len == 0 && report_v3_removed_src_layout(input_file) {
			exit(1)
		}
	} else {
		user_files << input_file
	}
	for listed_path in file_list {
		if os.is_dir(listed_path) {
			user_files << v3_directory_user_files(listed_path, prefs, is_test_command, true) or {
				eprintln(err.msg())
				exit(1)
			}
		} else if os.is_file(listed_path) {
			user_files << listed_path
		} else {
			eprintln('${listed_path} does not exist')
			exit(1)
		}
	}
	if is_prof {
		user_files << os.join_path(prefs.vroot, 'vlib', 'v', 'preludes', 'profiled_program.v')
	}
	prefs.is_test = user_files.any(is_v3_test_file(it, backend, prefs.target))
	parse_files_dispatch_profiled(mut p, user_files, !current_no_parallel, mut parse_timing)
	if is_linux_wayland_only_session(target.os, os.getenv('DISPLAY'), os.getenv('WAYLAND_DISPLAY'), os.getenv('XDG_SESSION_TYPE'))
		&& !user_defines.any(it.all_before('=').trim_space() == 'sokol_wayland')
		&& parsed_files_import_linux_gg(a, user_files) {
		eprintln('`gg`/`sokol.sapp` cannot run in a Wayland-only Linux session without `-d sokol_wayland`.')
		exit(1)
	}
	test_files := test_input_files(user_files, backend, prefs.target)

	if !no_builtin {
		seed_implicit_imports(mut a, minimal_literal_output)
	}
	seed_cached_builtin_bundle_imports(mut a, cache_state.manager.enabled, builtin_dir)

	// Resolve imports recursively
	resolve_imports_started_us := b.current_step_time_us()
	resolve_imports_parse_started_us := parse_timing.header_us + parse_timing.source_us
	resolve_imports(mut a, mut p, prefs, user_files, !current_no_parallel, minimal_literal_output, mut cache_state, mut parse_timing)
	resolve_imports_elapsed_us := b.current_step_time_us() - resolve_imports_started_us
	resolve_imports_parse_us := parse_timing.header_us + parse_timing.source_us - resolve_imports_parse_started_us
	resolve_imports_coordination_us := if resolve_imports_parse_us < resolve_imports_elapsed_us {
		resolve_imports_elapsed_us - resolve_imports_parse_us
	} else {
		i64(0)
	}
	if warn_impure_v {
		p.diagnostics << v3_impure_v_diagnostics(a)
	}
	// Preserve the digest of every exact source buffer V3 parsed before any parser or
	// later compiler error can request the compatibility compiler. The dispatcher owns
	// this staging directory and forwards only content plus verification metadata.
	mut fallback_report_sources := macos_v3_fallback_report_sources(a, prefs.vroot, cache_state.cached_source_digests, v3_fallback_ignored_warmup_source_paths(cache_state))
	_ = stage_macos_v3_fallback_source_digests(macos_v3_c_error_dir, fallback_report_sources)
	if print_v_files || print_watched_files {
		mut watched := map[string]bool{}
		for _, file in a.source_files {
			if file.name.ends_with('.v') || file.name.ends_with('.vv')
				|| file.name.ends_with('.vsh') {
				watched[os.real_path(file.name)] = true
			}
		}
		for source_files in cache_state.module_sources.values() {
			for file in source_files {
				if file.ends_with('.v') || file.ends_with('.vv') || file.ends_with('.vsh') {
					watched[os.real_path(file)] = true
				}
			}
		}
		mut watched_files := watched.keys()
		watched_files.sort()
		for file in watched_files {
			println(file)
		}
		clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
		return
	}
	if p.diagnostics.len > 0 {
		parser_has_native_errors := p.diagnostics.any(it.severity.len == 0
			|| it.severity == 'error:')
		parser_has_errors := parser_has_native_errors
			|| (effective_warns_are_errors && p.diagnostics.any(it.severity == 'warning:'))
		if parser_has_native_errors
			&& request_macos_v3_compatibility_fallback(p.diagnostics, macos_v3_fallback_file) {
			exit(1)
		}
		if parser_has_errors && macos_v3_fallback_suppresses_diagnostics(macos_v3_fallback_file) {
			exit(1)
		}
		if !silent || !only_check_syntax {
			for diagnostic in p.diagnostics {
				if file := a.source_files[diagnostic.pos.id] {
					_ = file
					severity := if effective_warns_are_errors && diagnostic.severity == 'warning:' {
						'error:'
					} else if diagnostic.severity.len > 0 {
						diagnostic.severity
					} else {
						'error:'
					}
					eprintln(v3errors.formatted_parser_diagnostic(severity, diagnostic.message, a, diagnostic.pos))
				} else {
					severity := if effective_warns_are_errors && diagnostic.severity == 'warning:' {
						'error:'
					} else if diagnostic.severity.len > 0 {
						diagnostic.severity
					} else {
						'error:'
					}
					eprintln('${diagnostic.file}:${diagnostic.line}:${diagnostic.column}: ${severity} ${diagnostic.message}')
				}
			}
		}
		if parser_has_errors {
			exit(1)
		}
	}
	if only_check_syntax {
		clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
		return
	}
	// Parallel transform is disabled for larger embedded imports when worker
	// scratch allocations live for the whole compilation. Scoped preallocation
	// releases that scratch after each stage, so it can retain parallel transform
	// without the former memory growth. Cgen workers reconcile their local string
	// IDs during the ordered merge, so embedded generation remains deterministic.
	if !current_no_parallel && os.getenv(v3_embedded_env) == '1' {
		if !scope_prealloc_transform && a.nodes.len >= embedded_parallel_transform_node_limit {
			current_parallel_transform = false
		}
	}
	p.release_source_storage()
	diagnostic_root := if is_selfhost {
		diagnostic_root_for_input(input_file, user_files)
	} else {
		''
	}

	parse_total_us := b.current_step_time_us()
	profiled_parse_us := parse_timing.header_us + parse_timing.source_us
	accounted_parse_us := profiled_parse_us + resolve_imports_coordination_us
	parse_scale_denominator := if accounted_parse_us > parse_total_us {
		accounted_parse_us
	} else {
		parse_total_us
	}
	header_parse_us := parse_timing.header_us * parse_total_us / parse_scale_denominator
	source_parse_us := parse_timing.source_us * parse_total_us / parse_scale_denominator
	resolve_coordination_us := resolve_imports_coordination_us * parse_total_us / parse_scale_denominator
	parse_setup_us := parse_total_us - header_parse_us - source_parse_us - resolve_coordination_us
	b.step_parts([
		bench.StepPart{
			name: 'parse setup/cache'
			time_us: parse_setup_us
		},
		bench.StepPart{
			name: 'parse .vh'
			time_us: header_parse_us
			parallel: parse_timing.header_parallel
		},
		bench.StepPart{
			name: 'parse .v'
			time_us: source_parse_us
			parallel: parse_timing.source_parallel
		},
		bench.StepPart{
			name: 'resolve imports'
			time_us: resolve_coordination_us
		},
	])
	b.metric_items('parsed .vh files', p.parsed_v_header_files, 'files', '.vh files', p.parsed_v_header_file_paths)
	if !silent {
		println('    ${'parsed .vh lines':-28s} ${source_file_line_count(p.parsed_v_header_file_paths)} lines')
	}
	b.metric_items('parsed .v files', p.parsed_v_files, 'files', '.v files', p.parsed_v_file_paths)
	if !silent {
		println('    ${'parsed .v lines':-28s} ${source_file_line_count(p.parsed_v_file_paths)} lines')
	}
	b.metric('AST nodes after parse', a.nodes.len, 'nodes')
	b.metric('AST children after parse', a.children.len, 'edges')
	b.metric('canonical AST texts', a.text_count(), 'texts')
	b.metric('persistent worker threads', a.worker_count(), 'threads')

	mut crun_build_identity := ''
	if is_direct_vsh && should_run && !explicit_output {
		carried_identity := os.getenv(v3_crun_build_identity_env)
		if os.getenv(v3_internal_restart_env) == '1' && carried_identity.len > 0 {
			crun_build_identity = carried_identity
		} else {
			mut crun_c_flags := user_c_flags.clone()
			crun_c_flags << cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
			_ = prepare_v3_cache_external_inputs_scoped(mut cache_state, a, prefs, user_files, crun_c_flags, scope_prealloc_stages)
			crun_build_identity = v3_crun_build_identity(&cache_state, prefs, user_files, crun_c_flags, is_strict, enable_globals_compat, input_file)
			if crun_build_identity.len > 0 {
				os.setenv(v3_crun_build_identity_env, crun_build_identity, true)
			}
		}
		if os.is_file(bin_file) && v3_crun_cache_matches(bin_file, crun_build_identity, input_file) {
			clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
			run_result := run_binary(bin_file, run_args)
			if run_result != 0 {
				exit(run_result)
			}
			b.step('run (cached)')
			b.print_report()
			return
		}
	}

	// An exact whole-program C plan hit already certifies the current user sources,
	// cached module interfaces, compiler configuration, target, and native inputs.
	// Validate it immediately after import resolution so an unchanged development
	// build does not repeat semantic and lowering work whose only consumer is that
	// cached C plan.
	mut cgen_cache_entry := modulecache.CgenEntry{}
	mut cgen_cache_metadata := V3CgenCacheMetadata{}
	mut cgen_cache_hit := false
	mut cgen_cache_commit_exists := false
	mut cgen_prepared_entry := modulecache.CgenPreparedEntry{}
	mut cgen_prepared_hit := false
	mut generic_cache_entry := modulecache.GenericProgramEntry{}
	mut generic_cache_hit := false
	mut generic_cache_signature := ''
	mut generic_cache_runtime_strings := []string{}
	mut cached_monomorph_specs := []transform.MonomorphCacheSpec{}
	mut cached_program_used_fns := map[string]bool{}
	mut generated_monomorph_specs := []transform.MonomorphCacheSpec{}
	mut cache_c_flags := user_c_flags.clone()
	if backend == 'c' && cache_state.manager.enabled {
		cache_c_flags << cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
	}
	use_macos_dev_program_cache := backend == 'c' && program_cache_enabled && !is_prod && !is_shared
		&& !is_selfhost && prefs.normalized_target_os() == 'macos'
	incremental_cache_enabled := use_macos_dev_program_cache
		&& os.getenv('V3_CACHE_DISABLE_INCREMENTAL') != '1'
	mut generic_cache_inputs_ready := false
	mut incremental_snapshot := V3IncrementalSnapshot{}
	mut incremental_snapshot_ready := false
	mut incremental_cache_restored := false
	mut incremental_cache_hit := false
	mut incremental_changed_keys := []string{}
	mut incremental_changed_names := map[string]bool{}
	mut incremental_uses_generics := false
	mut incremental_cached_body := ''
	mut incremental_prefix_path := ''
	mut incremental_tcc_declarations_path := ''
	if backend == 'c' && program_cache_enabled && !cache_state.force_source
		&& cache_state.parsed_from_source.len == 0 {
		mut external_inputs_ready := restore_v3_cache_external_inputs(mut cache_state, user_files, cache_c_flags, prefs.ccompiler, prefs.target, '')
		if !external_inputs_ready && incremental_cache_enabled {
			incremental_snapshot = incremental_program_snapshot(a, user_files)
			incremental_snapshot_ready = true
			if os.getenv('V3_CACHE_TRACE') != '' {
				eprintln('  V3 incremental snapshot: declarations=${incremental_snapshot.declaration_signature} functions=${incremental_snapshot.functions.len}')
			}
			external_inputs_ready = restore_v3_cache_external_inputs(mut cache_state, user_files, cache_c_flags, prefs.ccompiler, prefs.target, incremental_snapshot.declaration_signature)
		}
		if !external_inputs_ready
			&& !prepare_v3_cache_external_inputs_scoped(mut cache_state, a, prefs, user_files, cache_c_flags, scope_prealloc_stages) {
			trace_v3_cache_fallback('external C inputs cannot be assigned to cache units')
			restart_v3_without_cache()
		}
		if cached_native_sources_require_monolithic_cgen(cache_state, a, user_files) {
			trace_v3_cache_fallback('native C sources declare types needed across cache units')
			restart_v3_without_cache()
		}
		input := v3_cgen_cache_input(cache_state, user_files, cache_c_flags)
		cgen_cache_commit_exists = cache_state.manager.has_cgen_commit(input.source_files)
		if entry := cache_state.manager.valid_cgen(input.source_files, input.generation_signature, input.dependency_inputs) {
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
		if !cgen_cache_hit && incremental_cache_enabled {
			if !incremental_snapshot_ready {
				incremental_snapshot = incremental_program_snapshot(a, user_files)
				incremental_snapshot_ready = true
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 incremental snapshot: declarations=${incremental_snapshot.declaration_signature} functions=${incremental_snapshot.functions.len}')
				}
			}
			if entry := cache_state.manager.valid_incremental_program(input.source_files, incremental_snapshot.declaration_signature, input.generation_signature, input.dependency_inputs) {
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 incremental cache stamp hit')
				}
				manifest_text := os.read_file(entry.manifest) or { '' }
				if old_manifest := decode_incremental_manifest(manifest_text) {
					if changed_keys, changed_names := incremental_changed_functions(incremental_snapshot, old_manifest) {
						spec_text := os.read_file(entry.specs) or { '' }
						used_text := os.read_file(entry.used) or { '' }
						body_text := os.read_file(entry.body) or { '' }
						metadata := os.read_file(entry.metadata) or { '' }
						decoded_specs := decode_monomorph_cache_specs(spec_text)
						decoded_used := decode_cached_used_fns(used_text)
						if decoded_metadata := decode_v3_cgen_metadata(metadata) {
							if decoded_used.len > 0 && body_text.len > 0 {
								incremental_cache_restored = true
								incremental_cache_hit = changed_keys.len > 0
								incremental_changed_keys = changed_keys.clone()
								incremental_changed_names = changed_names.clone()
								incremental_cached_body = body_text
								incremental_prefix_path = entry.prefix
								incremental_tcc_declarations_path = entry.tcc_declarations
								cached_monomorph_specs = clone_monomorph_cache_specs(decoded_specs)
								cached_program_used_fns = clone_string_bool_map(decoded_used)
								cgen_cache_metadata = decoded_metadata
								generic_cache_entry = modulecache.GenericProgramEntry{
									specs: entry.specs
									used: entry.used
									prefix: entry.prefix
									declarations: entry.declarations
									body: entry.body
									metadata: entry.metadata
								}
								generic_cache_hit = true
								if changed_keys.len == 0 {
									if decoded := decode_v3_cgen_metadata(metadata) {
										materialized_body :=
											modulecache.materialize_cached_body_string_definitions(body_text)
										cgen_cache_entry = cache_state.manager.write_cgen(input.source_files, input.generation_signature, input.dependency_inputs, materialized_body, metadata) or {
											modulecache.CgenEntry{}
										}
										if cgen_cache_entry.stamp.len > 0 {
											cgen_cache_metadata = decoded
											cgen_cache_hit = true
										}
									}
								}
							}
						}
					} else {
						if os.getenv('V3_CACHE_TRACE') != '' {
							eprintln('  V3 incremental cache miss: function manifest does not match current source set')
						}
					}
				} else {
					if os.getenv('V3_CACHE_TRACE') != '' {
						eprintln('  V3 incremental cache miss: function manifest is invalid')
					}
				}
			}
		}
		if !cgen_cache_hit && !incremental_cache_hit && use_macos_dev_program_cache {
			generic_cache_signature = monomorph_cache_semantic_signature(a, user_files)
			generic_cache_runtime_strings = monomorph_cache_runtime_strings(a, user_files)
			generic_cache_inputs_ready = true
			if entry := cache_state.manager.valid_generic_program(input.source_files, generic_cache_signature, input.generation_signature, input.dependency_inputs) {
				spec_text := os.read_file(entry.specs) or { '' }
				cached_monomorph_specs = decode_monomorph_cache_specs(spec_text)
				used_text := os.read_file(entry.used) or { '' }
				cached_program_used_fns = decode_cached_used_fns(used_text)
				if cached_monomorph_specs.len > 0 && cached_program_used_fns.len > 0 {
					generic_cache_entry = entry
					generic_cache_hit = true
					old_literal_text := os.read_file(entry.literals) or { '' }
					cached_body := modulecache.materialize_cached_body_string_definitions(os.read_file(entry.body) or {
						''
					})
					metadata := os.read_file(entry.metadata) or { '' }
					if old_literals := decode_cached_runtime_strings(old_literal_text) {
						if cgen_cache_commit_exists {
							if rewritten := modulecache.rewrite_cached_runtime_strings(cached_body, old_literals, generic_cache_runtime_strings) {
								if decoded := decode_v3_cgen_metadata(metadata) {
									cgen_cache_entry = cache_state.manager.write_cgen(input.source_files, input.generation_signature, input.dependency_inputs, rewritten, metadata) or { modulecache.CgenEntry{} }
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
	// Exact whole-program cache hits do not consume the generic or incremental
	// snapshots. Build those AST-wide signatures only on a miss, while retaining
	// them for publishing a fresh development cache after a cold build.
	if !cgen_cache_hit && use_macos_dev_program_cache {
		if !generic_cache_hit && !generic_cache_inputs_ready {
			generic_cache_signature = monomorph_cache_semantic_signature(a, user_files)
			generic_cache_runtime_strings = monomorph_cache_runtime_strings(a, user_files)
		}
		if incremental_cache_enabled && !incremental_snapshot_ready {
			incremental_snapshot = incremental_program_snapshot(a, user_files)
			incremental_snapshot_ready = true
			if os.getenv('V3_CACHE_TRACE') != '' {
				eprintln('  V3 incremental snapshot: declarations=${incremental_snapshot.declaration_signature} functions=${incremental_snapshot.functions.len}')
			}
		}
	}
	if backend == 'c' && cache_state.manager.enabled {
		b.step('cache lookup')
	}
	if is_checker_fixture {
		if missing_header := checker_fixture_missing_header(a, user_files, c_compiler, user_defines) {
			eprintln('builder error: ${missing_header}')
			exit(1)
		}
	}
	// Source includes can introduce typedef structs used by V declarations and
	// literals before Cgen sees the included translation unit. Resolve those rare
	// inputs before checking so the type is available to semantic lookup.
	mut ck_stage_sw := time.new_stopwatch()
	native_inputs_needed := !cache_state.external_inputs_ready && ast_has_native_source_include(a)
	native_inputs_overlap := native_inputs_needed && building_v && !cache_state.manager.enabled
	native_inputs_done := chan bool{ cap: 1 }
	native_inputs_args := PrepareV3CheckerNativeInputsArgs{
		state: voidptr(&cache_state)
		a: a
		prefs: prefs
		user_files: user_files
		user_c_flags: cache_c_flags
		scope_enabled: scope_prealloc_stages
		done: native_inputs_done
	}
	if native_inputs_overlap {
		spawn prepare_v3_checker_native_inputs_thread(&native_inputs_args)
	} else if native_inputs_needed {
		if cache_state.manager.enabled {
			_ = prepare_v3_cache_external_inputs_scoped(mut cache_state, a, prefs, user_files, cache_c_flags, scope_prealloc_stages)
		} else {
			prepare_v3_checker_native_inputs_scoped(mut cache_state, a, prefs, user_files, cache_c_flags, scope_prealloc_stages)
		}
	}
	if verbose {
		eprintln('  [ttime]   ck native inputs ${f64(ck_stage_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	}
	if !native_inputs_overlap && backend == 'c' && cache_state.external_inputs_ready {
		fallback_report_sources = macos_v3_fallback_report_inputs(fallback_report_sources, &cache_state)
		_ = stage_macos_v3_fallback_source_digests(macos_v3_c_error_dir, fallback_report_sources)
	}

	// Type-collect + check BEFORE transform, so the transformer is type-aware
	// (like v2: check runs before transform). The transformer reads cached
	// per-expression types for type-dependent lowering.
	stage_macos_v3_compiler_error_fallback(macos_v3_fallback_file, 'semantic checking')
	mut pre_tc := types.TypeChecker.new(a)
	mut checker_notice_count := 0
	mut checker_warning_count := 0
	mut cached_checker_diagnostics := []V3CachedTypeDiagnostic{}
	pre_tc.compiler_vroot = prefs.vroot
	pre_tc.enable_globals = enable_globals_compat
	pre_tc.checker_fixture_mode = is_checker_fixture
	pre_tc.autofree_mode = 'autofree' in prefs.user_defines
	pre_tc.no_main = 'no_main' in prefs.user_defines
	pre_tc.warns_are_errors = effective_warns_are_errors
	pre_tc.notes_are_errors = notes_are_errors
	pre_tc.is_prod = prefs.is_prod
	pre_tc.building_v_fast = building_v && os.getenv('V3_NO_BUILDING_V_FAST_CHECK') == ''
	// Missing imports are rare error paths and need the authoritative serial
	// diagnostic pass, even for an otherwise-fast parallel self-host build.
	pre_tc.valid_diagnostic_fast = building_v && a.missing_imports.len == 0
		&& os.getenv('V3_NO_VALID_DIAGNOSTIC_FAST') == ''
	pre_tc.valid_resolution_fast = building_v && os.getenv('V3_NO_VALID_RESOLUTION_FAST') == ''
	pre_tc.suppress_dump_output = 'nop_dump' in prefs.user_defines
	mut used_fns := map[string]bool{}
	mut program_used_fns := map[string]bool{}
	mut incremental_stage_used_fns := map[string]bool{}
	mut uses_generics := false
	mut skip_transform_generics := true
	mut transform_texts_canonical := cgen_cache_hit
	mut texts_canonical_after_annotation := cgen_cache_hit
	mut retained_transform_scope := unsafe { nil }
	mut retained_transform_prepare_scope := unsafe { nil }
	mut trivial_literal_output := false
	if !cgen_cache_hit {
		pre_tc.verbose = prefs.verbose
		if scope_prealloc_check && a.missing_imports.len == 0 {
			pre_tc.enable_scoped_parallel_workers()
		}
		pre_tc.reject_unsupported_generics = is_selfhost
		mut ckpre_sw := time.new_stopwatch()
		set_diagnostic_files(mut pre_tc, user_files)
		// The C generator has a dedicated literal-output path. The SSA/native backend
		// still builds ordinary builtin bodies, so it needs their full dependency set.
		trivial_literal_output = backend != 'arm64' && test_files.len == 0 && !is_checker_fixture
			&& markused.is_trivial_literal_output_program(a, pre_tc.diagnostic_files)
		if verbose {
			eprintln('  [ttime]   ck trivial gate  ${f64(ckpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
		mut cvsw := time.new_stopwatch()
		pre_tc.collect(a)
		if native_inputs_overlap {
			_ := <-native_inputs_done
			if backend == 'c' && cache_state.external_inputs_ready {
				fallback_report_sources = macos_v3_fallback_report_inputs(fallback_report_sources, &cache_state)
				_ = stage_macos_v3_fallback_source_digests(macos_v3_c_error_dir, fallback_report_sources)
			}
		}
		if has_conflicting_c_declaration_errors(pre_tc.errors) {
			if !macos_v3_fallback_suppresses_diagnostics(macos_v3_fallback_file) {
				print_type_diagnostics(a, pre_tc.notices, pre_tc.errors, is_checker_fixture)
			}
			exit(1)
		}
		register_headerless_c_types(mut pre_tc)
		register_native_source_typedefs(mut pre_tc, &cache_state, scope_prealloc_stages)
		if translated_mode {
			for file in user_files {
				pre_tc.translated_files[file] = true
			}
		}
		if verbose {
			eprintln('  [ttime]   ck collect       ${f64(cvsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			cvsw.restart()
		}
		if pre_tc.check_interface_embedding_limits() {
			if !macos_v3_fallback_suppresses_diagnostics(macos_v3_fallback_file) {
				print_type_diagnostics(a, pre_tc.notices, pre_tc.errors, is_checker_fixture)
			}
			exit(1)
		}
		if verbose {
			eprintln('  [ttime]     ck iface embed ${f64(cvsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			cvsw.restart()
		}
		pre_tc.diagnose_unknown_calls = true
		pre_tc.prepare_threads_condition()
		set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
		if verbose {
			eprintln('  [ttime]     ck unsup gen   ${f64(cvsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			cvsw.restart()
		}
		if !incremental_cache_hit {
			pre_tc.prepare_interface_query_indexes()
		} else {
			pre_tc.prepare_interface_requirement_indexes()
		}
		if verbose {
			eprintln('  [ttime]   ck iface idx     ${f64(cvsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
		// The parallel checker deliberately leaves one logical core outside its
		// worker pool. Use it to build the immutable markused declaration indexes.
		prepare_markused_overlap := building_v && current_parallel_transform
			&& scope_prealloc_markused && !incremental_cache_hit && !generic_cache_hit
			&& !cache_state.manager.enabled && test_files.len == 0 && !is_checker_fixture
			&& !trivial_literal_output && !input_file.ends_with('.vsh') && !no_skip_unused
		prepared_markused_thread := spawn markused.prepare_markused_declarations(a, &pre_tc, prepare_markused_overlap)
		mut check_was_parallel := false
		if trivial_literal_output && !incremental_cache_hit {
			used_fns = markused.mark_used_without_generic_detection(a, &pre_tc)
			pre_tc.check_semantics_reachable(used_fns)
		} else if incremental_cache_hit {
			pre_tc.check_semantics_selected(incremental_changed_names)
		} else {
			ck_stage_sw.restart()
			// On very large user import graphs, serial checking uses less memory than
			// retaining one semantic-check accumulator per worker.
			parallel_semantic_check := !current_no_parallel && a.missing_imports.len == 0
				&& (building_v || !scope_prealloc_check
					|| a.nodes.len < scoped_serial_user_check_node_threshold)
			check_was_parallel = pre_tc.check_semantics_opt(parallel_semantic_check)
			if verbose {
				eprintln('  [ttime]   ck semantics     ${f64(ck_stage_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			}
		}
		ck_stage_sw.restart()
		mut prepared_markused := prepared_markused_thread.wait()
		if verbose {
			eprintln('  [ttime]   ck mkused wait   ${f64(ck_stage_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
		ckpre_sw.restart()
		pre_tc.check_main_module_requirement(is_shared || test_files.len > 0
			|| a.export_fn_names.len > 0)
		if verbose {
			eprintln('  [ttime]   ck main req      ${f64(ckpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
		if is_repl {
			pre_tc.notices.clear()
		}
		if incremental_cache_hit {
			b.step('check (incremental)')
		} else {
			b.step_parallel('check', check_was_parallel)
		}
		if pre_tc.errors.len > 0 {
			if is_checker_fixture {
				fixture_used_fns, fixture_uses_generics := markused.mark_used_with_generic_usage(a, &pre_tc)
				has_invalid_comptime_struct_update :=
					pre_tc.errors.any(it.msg == 'cannot use struct update syntax in compile time expressions')
				has_missing_closure_generic := pre_tc.errors.any((it.msg.starts_with('Add the generic type `')
					&& it.msg.contains(' to the anon fn generic list type'))
					|| it.msg.starts_with('generic closure fn must specify type parameter'))
				has_instantiated_generic_as_cast_error := pre_tc.errors.any(int(it.node) >= 0
					&& int(it.node) < a.nodes.len && a.nodes[int(it.node)].kind == .as_expr
					&& it.msg.starts_with('cannot cast `'))
				has_empty_array_generic_error :=
					pre_tc.errors.any(it.msg == 'cannot use empty array as generic argument')
				has_generic_fntype_arg_mismatch := pre_tc.errors.any(it.msg.starts_with('cannot use `fn ') && it.msg.contains('` as `fn ')
					&& it.msg.contains(' in argument '))
				has_generic_call_arg_mismatch := pre_tc.errors.any(it.msg.starts_with('cannot use `') && it.msg.contains(' in argument '))
				has_generic_inference_error :=
					pre_tc.errors.any(it.msg.starts_with('could not infer generic type `'))
				has_generic_struct_init_error := pre_tc.errors.any(it.msg.starts_with('generic struct init type parameter `')
					|| it.msg.starts_with('generic struct init expects '))
				has_generic_type_mismatch :=
					pre_tc.errors.any(it.msg.starts_with('mismatched types `'))
				has_unknown_method_error := pre_tc.errors.any(it.msg.starts_with('unknown method or field: `')
					|| (it.msg.starts_with('method `')
						&& it.msg.contains(' cannot bind `voidptr` to a generic receiver pattern')))
				mut has_instantiated_compile_error := false
				for type_error in pre_tc.errors {
					if type_error.kind != .compile_error || int(type_error.node) < 0
						|| int(type_error.node) >= a.nodes.len {
						continue
					}
					error_node := a.node(type_error.node)
					if error_node.kind != .call || error_node.children_count == 0 {
						continue
					}
					callee := a.child_node(error_node, 0)
					if callee.kind != .ident || callee.value != '__v_compile_error' {
						has_instantiated_compile_error = true
						break
					}
				}
				if fixture_uses_generics && !has_invalid_comptime_struct_update
					&& !has_missing_closure_generic && !has_instantiated_generic_as_cast_error
					&& !has_instantiated_compile_error && !has_empty_array_generic_error
					&& !has_generic_fntype_arg_mismatch && !has_generic_call_arg_mismatch
					&& !has_generic_inference_error && !has_generic_struct_init_error
					&& !has_generic_type_mismatch && !has_unknown_method_error {
					_, _ = transform.monomorphize_with_used_checked_config(mut a, &pre_tc, fixture_used_fns, false)
				}
			}
			if !macos_v3_fallback_suppresses_diagnostics(macos_v3_fallback_file) {
				print_type_diagnostics(a, pre_tc.notices, pre_tc.errors, is_checker_fixture)
			}
			pre_tc.notices.clear()
		}
		if pre_tc.errors.len > 0 {
			exit(1)
		}
		incremental_uses_generics = incremental_cache_hit
			&& incremental_changed_functions_use_generics(a, pre_tc, incremental_changed_names)
		if verbose {
			cvsw.restart()
		}
		pre_tc.prune_inactive_top_level_comptime(mut a)
		test_harness_errors := validate_test_file_harness_inputs(a, pre_tc, test_files)
		if verbose {
			eprintln('  [ttime]   ck prune+harness ${f64(cvsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
		if test_harness_errors.len > 0 {
			for msg in test_harness_errors {
				eprintln(msg)
			}
			exit(1)
		}
		if check_only {
			clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
			return
		}
		if cache_state.manager.enabled {
			const_init_order := cgen.module_const_init_order(a, pre_tc)
			if !prepare_v3_cache_external_inputs_scoped(mut cache_state, a, prefs, user_files, cache_c_flags, scope_prealloc_stages) {
				trace_v3_cache_fallback('external C inputs cannot be assigned to cache units')
				restart_v3_without_cache()
			}
			if cached_native_sources_require_monolithic_cgen(cache_state, a, user_files) {
				trace_v3_cache_fallback('native C sources declare types needed across cache units')
				restart_v3_without_cache()
			}
			for module_name, parsed in cache_state.parsed_from_source {
				if !parsed {
					continue
				}
				header := modulecache.module_header_with_const_order(a, pre_tc, module_name, prefs.vroot, cache_state.module_import_paths, const_init_order)
				if header.len > 0 {
					cache_state.headers[module_name] = header
				}
			}
			if invalidate_changed_cache_dependents(mut cache_state) {
				restart_v3_after_cache_invalidation()
			}
		}
		// Ownership analysis only exists in `-d ownership` builds and runs
		// interleaved inside check; report its accumulated time as a dedicated
		// stage so plain builds visibly spend exactly 0 on it.
		b.step_measured('ownership', pre_tc.ownership_time_spent_us())
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
		// Self-host markused is dominated by serial reachability/index work. Build
		// transform's read-only indexes beside it so the otherwise-idle cores do
		// useful work without delaying either stage's mutable AST operations.
		prepare_transform_overlap := building_v && current_parallel_transform
			&& scope_prealloc_transform && !incremental_cache_hit && !generic_cache_hit
			&& !cache_state.manager.enabled
		prepared_transform_thread := spawn transform.prepare_selfhost_transform(a, &pre_tc, prepare_transform_overlap)
		// Mark used functions (dead-code elimination). This is done before transform
		// so the transformer can skip function bodies that the C backend will prune.
		// Checking and inactive-comptime pruning can add or detach nodes. Rebuild the
		// parent index once here so markused type queries do not fall back to a full
		// arena scan for every generated selector base.
		if !building_v || !pre_tc.reuse_direct_parent_index_for_unchanged_ast(a) {
			pre_tc.refresh_direct_parent_index(a)
		}
		mut markused_scope := unsafe { nil }
		mut markused_tc := &pre_tc
		if scope_prealloc_markused && !generic_cache_hit {
			markused_scope = prealloc_scope_begin_for_v3()
			markused_tc = pre_tc.fork_for_parallel_transform(a)
			markused_tc.share_direct_dependencies_from(&pre_tc)
			markused_tc.enable_scoped_parallel_workers()
			markused_tc.verbose = prefs.verbose
		}
		if no_skip_unused {
			used_fns, uses_generics = markused.mark_all_used_with_generic_usage(a, markused_tc, test_files)
		} else if generic_cache_hit && test_files.len == 0 {
			used_fns = clone_string_bool_map(cached_program_used_fns)
			uses_generics = true
			if incremental_cache_hit
				&& incremental_changed_functions_require_reachability_rebuild(a, markused_tc, mut incremental_changed_names, mut used_fns, user_files) {
				os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
				restart_v3_after_cache_invalidation()
			}
		} else if test_files.len > 0 {
			used_fns, uses_generics = markused.mark_used_for_tests_with_generic_usage(a, markused_tc, test_files)
		} else if input_file.ends_with('.vsh') {
			used_fns, uses_generics = markused.mark_used_with_generic_usage_full_runtime(a, markused_tc)
		} else if trivial_literal_output && used_fns.len > 0 {
			uses_generics = false
		} else if is_checker_fixture {
			used_fns, uses_generics = markused.mark_used_with_generic_usage_full_runtime(a, markused_tc)
		} else if building_v {
			if prepare_markused_overlap {
				used_fns = markused.mark_used_without_generic_detection_prepared(a, markused_tc, mut prepared_markused)
			} else {
				used_fns = markused.mark_used_without_generic_detection(a, markused_tc)
			}
			uses_generics = false
		} else {
			used_fns, uses_generics = markused.mark_used_with_generic_usage(a, markused_tc)
		}
		if is_prof {
			add_v3_profile_used_fns(mut used_fns)
		}
		// The separate program reachability snapshot is consumed only by the
		// module/generic cache publishers. A -nocache self-host can skip cloning
		// ~10k entries twice around the disposable markused arena.
		if cache_state.manager.enabled {
			program_used_fns = clone_string_bool_map(used_fns)
		}
		if cache_state.manager.enabled && !generic_cache_hit {
			if building_v && current_parallel_transform {
				used_fns = markused.mark_used_for_cache_without_generic_detection(a, markused_tc, test_files, cache_state.source_body_modules)
			} else {
				mut cache_uses_generics := false
				used_fns, cache_uses_generics = markused.mark_used_for_cache_with_generic_usage(a, markused_tc, test_files, cache_state.source_body_modules)
				uses_generics = uses_generics || cache_uses_generics
			}
		}
		if scope_prealloc_markused && !generic_cache_hit {
			prealloc_scope_leave_for_v3(markused_scope)
			used_fns = clone_string_bool_map(used_fns)
			if cache_state.manager.enabled {
				program_used_fns = clone_string_bool_map(program_used_fns)
			}
			prealloc_scope_free_for_v3(markused_scope)
		}
		if prepare_markused_overlap {
			prepared_markused.release()
		}
		mut prepared_transform := prepared_transform_thread.wait()
		b.step('markused')
		b.metric('reachable symbols', used_fns.len, 'symbols')
		mut tfpre_sw := time.new_stopwatch()
		// Formatting unused-declaration diagnostics allocates source excerpts and
		// rendered messages that are dead before transform starts. Keep that scratch
		// out of the compilation arena on cache-disabled prealloc builds.
		mut unused_diag_scope := voidptr(unsafe { nil })
		if scope_prealloc_stages && !cache_state.manager.enabled {
			unused_diag_scope = prealloc_scope_begin_for_v3()
		}
		if !is_repl && !pre_tc.valid_diagnostic_fast {
			pre_tc.diagnose_unused_private_declarations(used_fns)
		}
		if verbose {
			eprintln('  [ttime] tf diag unused     ${f64(tfpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			tfpre_sw.restart()
		}
		if pre_tc.notices.len > 0 {
			checker_sql_warnings_only := is_checker_fixture && ast_contains_sql_expr(a)
			if cache_state.manager.enabled {
				cached_checker_diagnostics << cache_v3_type_diagnostics(a, pre_tc.notices)
			}
			print_type_diagnostics(a, pre_tc.notices, []types.TypeError{}, is_checker_fixture)
			for notice in pre_tc.notices {
				if notice.severity == 'warning:' {
					checker_warning_count++
				} else {
					checker_notice_count++
				}
			}
			pre_tc.notices.clear()
			if checker_sql_warnings_only {
				exit(0)
			}
		}
		if unused_diag_scope != unsafe { nil } {
			release_unused_diagnostic_scope(mut pre_tc.notices, unused_diag_scope)
		}
		if backend == 'wasm' {
			// Validate source-level operations before transform lowers aggregate
			// equality into primitive field comparisons. A second pass after
			// monomorphization below covers newly specialized function bodies.
			if msg := unsupported_backend_error(a, &pre_tc, used_fns, backend) {
				eprintln(msg)
				exit(1)
			}
		}

		// Checking is complete: from here on, resolve_type may serve unmodified
		// source nodes straight from the checker's dense per-node cache
		// (transform invalidates every id it rewrites).
		if os.getenv('V3_NO_TRUST_CHECKED_TYPES').len == 0 {
			pre_tc.trust_checked_expr_types = true
		}
		// Cache-disabled builds still need the same resolved native-input snapshot as
		// cached builds before transformation or C generation can fail. If resolution is
		// incomplete, leave the manifest without its completeness marker so the stable
		// retry prints the fallback notice but does not submit an unverified report.
		if backend == 'c' && !cache_state.external_inputs_ready {
			_ = prepare_v3_cache_external_inputs_scoped(mut cache_state, a, prefs, user_files, cache_c_flags, scope_prealloc_stages)
		}
		if backend == 'c' && cache_state.external_inputs_ready {
			fallback_report_sources = macos_v3_fallback_report_inputs(fallback_report_sources, &cache_state)
			_ = stage_macos_v3_fallback_source_digests(macos_v3_c_error_dir, fallback_report_sources)
		}
		// Transform (match lowering, string/in lowering, etc.). Threaded transform is enabled
		// by default for compatible builds, and `-no-parallel` disables both threaded transform
		// and cgen.
		stage_macos_v3_compiler_error_fallback(macos_v3_fallback_file, 'AST transformation')
		mut transform_was_parallel := false
		mut transform_errors := []string{}
		mut incremental_synthesized_helpers := []string{}
		if !building_v && !uses_generics && ast_contains_sql_expr(a) {
			uses_generics = true
		}
		// Markused distinguishes reachable generic calls/types from generic templates
		// that merely came along with an imported module (notably sync and rand).
		skip_transform_generics = building_v || !uses_generics
		if incremental_cache_hit {
			skip_transform_generics = true
		}
		mut transform_used_fns := map[string]bool{}
		pre_transform_node_count := a.nodes.len
		if incremental_cache_hit {
			transform_used_fns = clone_string_bool_map(incremental_changed_names)
			// `main` activates the transformer's used-function filter. If another
			// function changed, transforming main as well is harmless; cgen still
			// emits only the explicitly changed function sections.
			transform_used_fns['main'] = true
		}
		if incremental_cache_hit {
			transform_used_fns, transform_errors, incremental_synthesized_helpers = transform.transform_selected_functions(mut a, &pre_tc, incremental_changed_names)
			transform_texts_canonical = true
		} else if scope_prealloc_transform {
			if verbose {
				eprintln('  [ttime] tf pre misc        ${f64(tfpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
				eprintln('  [leakcheck] pre-reserve: nodes ${a.nodes.len}/${a.nodes.cap} children ${a.children.len}/${a.children.cap} cache_mode ${cache_state.manager.enabled} skipgen ${skip_transform_generics}')
				tfpre_sw.restart()
			}
			// Keep the large escaping AST/cache slabs in the compilation arena, while
			// transformer indexes and per-body temporary state use a stage arena.
			if cache_state.manager.enabled {
				transform.reserve_parallel_transform_cache_ast(mut a, skip_transform_generics)
			} else {
				transform.reserve_parallel_transform_ast(mut a, skip_transform_generics)
			}
			if verbose {
				eprintln('  [leakcheck] post-reserve: nodes ${a.nodes.len}/${a.nodes.cap} children ${a.children.len}/${a.children.cap}')
				eprintln('  [ttime] tf reserve         ${f64(tfpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
				tfpre_sw.restart()
			}
			pre_tc.begin_sparse_transform_node_caches(a.nodes.len)
			pre_tc.reserve_scoped_transform_metadata(scoped_transform_signature_headroom)
			if verbose {
				eprintln('  [ttime] tf sparse+meta     ${f64(tfpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
				tfpre_sw.restart()
			}
			base_transform_nodes := a.nodes.len
			reserved_nodes_cap := a.nodes.cap
			reserved_children_cap := a.children.cap
			pre_scope_children_data := unsafe { a.children.data }
			base_specialized_fns := a.specialized_fn_nodes.len
			base_type_count := pre_tc.type_count()
			base_symbol_count := pre_tc.symbol_count()
			base_text_count := a.text_values.len
			mut original_signature_names := map[string]bool{}
			for name, _ in pre_tc.fn_ret_types {
				original_signature_names[name] = true
			}
			if verbose {
				eprintln('  [ttime] tf signames        ${f64(tfpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
				tfpre_sw.restart()
			}
			transform_scope := prealloc_scope_begin_for_v3()
			mut scoped_owned_base_nodes := []int{}
			mut retained_transform_regions := []transform.ScopedTransformRegion{}
			if prepare_transform_overlap {
				transform_used_fns, transform_was_parallel, transform_errors, scoped_owned_base_nodes, retained_transform_regions = transform.transform_prepared_selfhost_owned(mut prepared_transform, mut a, &pre_tc, used_fns, transform_scope)
				retained_transform_prepare_scope = prepared_transform.take_scope()
			} else {
				// Large user programs keep more memory live when function-body workers
				// overlap the transformed AST. Scoped serial batches trade a little CPU
				// for a substantially lower peak.
				use_parallel_transform := current_parallel_transform
					&& a.nodes.len < scoped_serial_user_transform_node_threshold
				transform_used_fns, transform_was_parallel, transform_errors, scoped_owned_base_nodes, retained_transform_regions = transform.transform_with_used_opt_config_scoped_workers_checked_owned(mut a, &pre_tc, used_fns, use_parallel_transform, skip_transform_generics, true, building_v || trivial_literal_output, transform_scope)
			}
			parse_cache_enabled := pre_tc.type_cache_parse_enabled()
			mut post_sw := time.new_stopwatch()
			prealloc_scope_leave_for_v3(transform_scope)
			retain_transform_scope := building_v && current_parallel_transform && backend == 'c'
				&& !cache_state.manager.enabled && retained_transform_regions.len == 0
				&& os.getenv('V3_RETAIN_TRANSFORM_SCOPE') != ''
			if retain_transform_scope {
				// Cgen is the only remaining semantic consumer in this no-cache self-host
				// path. Keep the typed transform arena alive through it instead of cloning
				// the AST/checker payloads into the parent and immediately rebuilding the
				// same type caches in the backend.
				retained_transform_scope = transform_scope
				transform_texts_canonical = true
			} else {
				retained_transform_regions =
					clone_scoped_transform_regions(retained_transform_regions)
				pre_tc.promote_scoped_transform_interners(base_type_count, base_symbol_count, transform_scope)
				if verbose {
					eprintln('  [ttime] promote interners  ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
					post_sw.restart()
				}
				if a.nodes.cap == reserved_nodes_cap && a.children.cap == reserved_children_cap
					&& !scoped_value_owned(transform_scope, a.nodes.data)
					&& !scoped_value_owned(transform_scope, a.children.data) {
					a.promote_transform_texts_from(base_text_count, transform_scope)
					if verbose {
						mut dirty_texts := 0
						for tv in a.text_values {
							if tv.len > 0 && scoped_value_owned(transform_scope, tv.str) {
								dirty_texts++
							}
						}
						eprintln('  [leakcheck] text table dirty ${dirty_texts} / ${a.text_values.len} (base_text_count ${base_text_count}, regions ${retained_transform_regions.len})')
						eprintln('  [ttime] promote texts      ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
						post_sw.restart()
					}
					// Lowering can rewrite arbitrary pre-existing nodes, including type text
					// on otherwise non-generic expressions. Publish every scope-owned
					// text before releasing the outer arena. Without retained regions
					// one fused pool pass (ownership check + table-hit reuse + clone)
					// replaces the serial canonicalize + promote walks.
					mut fused_text_promote := false
					if retained_transform_regions.len == 0
						&& os.getenv('V3_NO_FUSED_TEXT_PROMOTE') == '' {
						fused_text_promote = transform.promote_scoped_texts_parallel(mut a, transform_scope)
						if fused_text_promote {
							transform_texts_canonical = true
						}
					}
					if verbose {
						eprintln('  [ttime] canon nodes        ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms (n: ${a.nodes.len}, fused: ${fused_text_promote})')
						post_sw.restart()
					}
					if !fused_text_promote {
						mut scoped_text_flags := []u8{len: a.nodes.len}
						if transform.scan_scoped_text_flags_parallel(a, transform_scope, mut scoped_text_flags) {
							mut canon_cache_ptrs := unsafe { []voidptr{len: 4096} }
							mut canon_cache_vals := []string{len: 4096}
							for idx, flag in scoped_text_flags {
								if flag != 0 {
									canonicalize_scoped_node_cached(mut a, idx, transform_scope, mut canon_cache_ptrs, mut canon_cache_vals)
								}
							}
						} else {
							scoped_text_flags = []u8{}
							for idx in 0 .. a.nodes.len {
								canonicalize_scoped_node(mut a, idx, transform_scope)
							}
						}
						if retained_transform_regions.len > 0 {
							outer_new_end := retained_transform_regions[0].new_start
							promote_scoped_ast_nodes_flagged(mut a, base_transform_nodes, outer_new_end, scoped_owned_base_nodes, transform_scope, scoped_text_flags)
							// Late lowering can rewrite nodes that live in a retained worker region
							// while allocating their replacement text in the outer transform arena.
							// Publish those strings before releasing that arena; the worker-owned
							// fields in the same regions are canonicalized below from their own scope.
							for region in retained_transform_regions {
								canonicalize_scoped_transform_region_from_scope(mut a, region, transform_scope)
							}
							last_worker_end := retained_transform_regions.last().new_end
							promote_scoped_ast_nodes_flagged(mut a, last_worker_end, a.nodes.len, []int{}, transform_scope, scoped_text_flags)
						} else {
							// Workers report every rewritten base node. Publish those and the
							// appended range without rebuilding the text table for the source AST.
							promote_scoped_ast_nodes_flagged(mut a, base_transform_nodes, a.nodes.len, scoped_owned_base_nodes, transform_scope, scoped_text_flags)
							transform_texts_canonical = true
						}
					}
				} else {
					if verbose {
						eprintln('  [leakcheck] canon SKIPPED: nodes cap ${a.nodes.cap} vs ${reserved_nodes_cap} children cap ${a.children.cap} vs ${reserved_children_cap} nodes.len ${a.nodes.len} owned n ${scoped_value_owned(transform_scope, a.nodes.data)} c ${scoped_value_owned(transform_scope, a.children.data)}')
						eprintln('  [leakcheck] children.data pre ${u64(pre_scope_children_data)} now ${u64(unsafe { a.children.data })} moved ${pre_scope_children_data != unsafe { a.children.data }}')
					}
					// The flat arrays escaped into the stage arena, so their backing is
					// cloned below — but each node's value/typ/params strings can live in
					// that arena too. Publish them through the canonical text table first;
					// skipping this dangled every transform-written node text whenever the
					// pre-reserved capacity was outgrown.
					a.promote_transform_texts_from(base_text_count, transform_scope)
					for idx in 0 .. a.nodes.len {
						canonicalize_scoped_node(mut a, idx, transform_scope)
					}
					clone_flat_ast_storage(mut a)
					pre_tc.rebind_ast(a)
				}
				if a.specialized_fn_nodes.len != base_specialized_fns {
					a.specialized_fn_nodes = a.specialized_fn_nodes.clone()
					a.specialized_fn_modules = clone_int_string_map(a.specialized_fn_modules)
					a.specialized_fn_files = clone_int_string_map(a.specialized_fn_files)
				}
				if verbose {
					eprintln('  [ttime] promote ast nodes  ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
					post_sw.restart()
				}
				promote_scoped_checker_node_caches(mut pre_tc, a, transform_scope, base_transform_nodes)
				if verbose {
					eprintln('  [ttime]   pc node caches   ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
					post_sw.restart()
				}
				promote_scoped_signatures(mut pre_tc, original_signature_names, transform_scope)
				if verbose {
					eprintln('  [ttime]   pc signatures    ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
					post_sw.restart()
				}
				promote_scoped_type_metadata(mut pre_tc)
				// Transform type lookups can canonicalize alias keys/targets while the
				// disposable arena is active. Re-own the table before releasing that arena;
				// interface snapshotting below iterates every alias, including otherwise
				// unreachable callback aliases.
				pre_tc.type_aliases = clone_string_string_map(pre_tc.type_aliases)
				transform_used_fns = clone_string_bool_map(transform_used_fns)
				transform_errors = clone_string_list(transform_errors)
				pre_tc.set_fresh_type_cache(parse_cache_enabled)
				if verbose {
					mut leaked_rc := 0
					mut leaked_fv := 0
					mut leaked_nv := 0
					mut leaked_nt := 0
					mut first_leak := -1
					for idx in 0 .. pre_tc.resolved_call_names.len {
						if idx < pre_tc.resolved_call_set.len && pre_tc.resolved_call_set[idx] {
							name := pre_tc.resolved_call_names[idx]
							if name.len > 0 && scoped_value_owned(transform_scope, name.str) {
								leaked_rc++
								if first_leak < 0 {
									first_leak = idx
								}
							}
						}
						if idx < pre_tc.resolved_fn_value_set.len
							&& pre_tc.resolved_fn_value_set[idx] {
							name := pre_tc.resolved_fn_value_names[idx]
							if name.len > 0 && scoped_value_owned(transform_scope, name.str) {
								leaked_fv++
							}
						}
					}
					for idx in 0 .. a.nodes.len {
						node := a.nodes[idx]
						if node.value.len > 0 && scoped_value_owned(transform_scope, node.value.str) {
							leaked_nv++
						}
						if node.typ.len > 0 && scoped_value_owned(transform_scope, node.typ.str) {
							leaked_nt++
						}
					}
					eprintln('  [leakcheck] tf scope: rc ${leaked_rc} fv ${leaked_fv} node.value ${leaked_nv} node.typ ${leaked_nt} first_rc ${first_leak}')
				}
				prealloc_scope_free_for_v3(transform_scope)
				if verbose {
					eprintln('  [ttime] promote checker    ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
					post_sw.restart()
				}
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
				// Type-resolution views can grow their by-file map while the transform arena
				// is active. Recreate it in the compilation arena before later phases use it.
				pre_tc.reset_resolution_type_view_cache()
			}
			if verbose {
				eprintln('  [ttime] regions+views      ${f64(post_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			}
		} else {
			transform_used_fns, transform_was_parallel, transform_errors = transform.transform_with_used_opt_config_scoped_workers_checked(mut a, &pre_tc, used_fns, current_parallel_transform, skip_transform_generics, false, building_v || trivial_literal_output)
		}
		if !incremental_cache_hit {
			used_fns = transform_used_fns.move()
		} else {
			incremental_stage_used_fns = clone_string_bool_map(transform_used_fns)
			// Synthesized helpers have no source snapshot key, so explicitly include
			// their generated bodies in both incremental Cgen filters.
			for name in incremental_synthesized_helpers {
				incremental_stage_used_fns[name] = true
				incremental_changed_names[name] = true
			}
		}
		if is_prof {
			add_v3_profile_used_fns(mut used_fns)
		}
		if !building_v && !uses_generics && transformed_used_fns_need_monomorphize(used_fns) {
			uses_generics = true
			skip_transform_generics = false
		}
		if incremental_cache_hit {
			b.step_parallel('transform (incremental)', transform_was_parallel)
		} else {
			b.step_parallel('transform', transform_was_parallel)
		}
		// Self-host C generation consumes the transformer's explicit node types and
		// the checker's resolved-call sidecars; it does not perform a second semantic
		// annotation walk. Keep the checked source parent index in that fast path
		// instead of rebuilding parents for 1M+ appended lowering nodes that no later
		// stage queries.
		if !building_v {
			pre_tc.refresh_rewritten_parent_index(a)
		}
		if transform_errors.len > 0 {
			eprintln('type checker found ${transform_errors.len} error(s):')
			for message in transform_errors {
				eprintln(message)
			}
			exit(1)
		}
		if !incremental_cache_hit {
			pre_tc.freeze_pre_transform_interface_impl_names()
		}
		b.metric('AST nodes after transform', a.nodes.len, 'nodes')
		b.metric('AST children after transform', a.children.len, 'edges')

		// Reuse the pre-transform checker for metadata only. Transform does not add
		// declarations, and v1/v2 do not run a second semantic checker after lowering.
		pre_tc.diagnose_unknown_calls = false
		pre_tc.reject_unlowered_map_mutation = true
		set_diagnostic_files(mut pre_tc, user_files)
		set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
		incremental_needs_monomorphize := incremental_cache_hit && (incremental_uses_generics
			|| transformed_used_fns_need_monomorphize(incremental_stage_used_fns))
		if !building_v {
			if uses_generics && (!incremental_cache_hit || incremental_needs_monomorphize) {
				if scope_prealloc_stages {
					// Transform already reserved generic append headroom on the AST. Give
					// dense semantic caches the same capacity; reserving another multiple
					// of the transformed length retains mostly-empty slabs for programs
					// whose reachable specialization set is small.
					pre_tc.materialize_sparse_transform_node_caches(a.nodes.len, a.nodes.cap)
				}
				// Generic lowering rewrites and clones call nodes in disposable arenas.
				// Resolve their final names from the owned transformed AST instead of
				// retaining pre-transform canonical string views across arena release.
				pre_tc.annotate_types_with_used_missing_calls(if incremental_cache_hit {
					transform_used_fns
				} else {
					used_fns
				}, pre_transform_node_count)
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
			transform.register_cached_monomorph_signatures(a, &pre_tc, used_fns, cached_monomorph_specs)
		}
	} else {
		b.step('check (cached)')
		b.step('markused (cached)')
		b.step('transform (cached)')
		b.step('annotate types (cached)')
		if !is_repl && cgen_cache_metadata.diagnostics.len > 0 {
			cached_notices := restore_v3_type_diagnostics(mut a, cgen_cache_metadata.diagnostics)
			print_type_diagnostics(a, cached_notices, []types.TypeError{}, is_checker_fixture)
			for notice in cached_notices {
				if notice.severity == 'warning:' {
					checker_warning_count++
				} else {
					checker_notice_count++
				}
			}
		}
	}
	if is_repl {
		pre_tc.notices.clear()
	}
	if pre_tc.errors.len > 0 {
		if macos_v3_fallback_suppresses_diagnostics(macos_v3_fallback_file) {
			exit(1)
		}
		print_type_diagnostics(a, pre_tc.notices, pre_tc.errors, is_checker_fixture)
		exit(1)
	}

	// Monomorphization only adds specialized generic instantiations to `used_fns`.
	// Markused and Cgen already exclude unreachable generic templates, so builds
	// with no reachable generic use need no generic cleanup pass at all.
	stage_macos_v3_compiler_error_fallback(macos_v3_fallback_file, 'type specialization')
	if cgen_cache_hit {
		// The cached C plan and metadata are the only consumers of the specialized
		// AST and checker state on this path.
	} else if uses_generics && (!incremental_cache_hit || incremental_uses_generics
		|| transformed_used_fns_need_monomorphize(incremental_stage_used_fns)) {
		mut monomorph_used_fns := map[string]bool{}
		mut monomorph_errors := []string{}
		incremental_monomorph_node_start := a.nodes.len
		monomorph_input_used := if incremental_cache_hit {
			incremental_stage_used_fns
		} else {
			used_fns
		}
		if scope_prealloc_stages && !incremental_cache_hit {
			// Generic transform reserved the persistent AST append regions before its
			// disposable arena was entered. Reuse that remaining capacity here; an
			// unconditional second multi-X reserve retains the old preallocated slabs
			// and dominates the memory peak when specialization adds only a few nodes.
			if verbose {
				eprintln('mono AST before pass: ${a.nodes.len}/${a.nodes.cap} nodes, ${a.children.len}/${a.children.cap} children')
			}
			base_monomorph_nodes := a.nodes.len
			monomorph_nodes_cap := a.nodes.cap
			monomorph_children_cap := a.children.cap
			base_specialized_fns := a.specialized_fn_nodes.len
			monomorph_scope := prealloc_scope_begin_for_v3()
			monomorph_used_fns, monomorph_errors, generated_monomorph_specs = transform.monomorphize_with_used_checked_config_scoped_cached(mut a, &pre_tc, monomorph_input_used, !current_no_parallel
				&& should_parallel_monomorphize(), monomorph_scope, cached_monomorph_specs)
			prealloc_scope_leave_for_v3(monomorph_scope)
			// The monomorphizer publishes rewritten and appended node text after
			// its final worker merge while all worker arenas are still live.
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
				a.specialized_fn_modules = clone_int_string_map(a.specialized_fn_modules)
				a.specialized_fn_files = clone_int_string_map(a.specialized_fn_files)
			}
			promote_scoped_checker_node_caches(mut pre_tc, a, monomorph_scope, base_monomorph_nodes)
			pre_tc.rebuild_scoped_transform_signature_maps()
			pre_tc.rebuild_fn_param_suffix_index()
			pre_tc.promote_scoped_transform_interners(0, 0, monomorph_scope)
			promote_scoped_type_metadata(mut pre_tc)
			pre_tc.errors = clone_type_errors(pre_tc.errors)
			pre_tc.notices = clone_type_errors(pre_tc.notices)
			monomorph_used_fns = clone_string_bool_map(monomorph_used_fns)
			monomorph_errors = clone_string_list(monomorph_errors)
			generated_monomorph_specs = clone_monomorph_cache_specs(generated_monomorph_specs)
			// Scoped specialization can leave parse-cache key text in a disposable
			// worker arena. Cgen must reparse from the promoted AST instead of reading
			// those keys after the monomorph scope is released.
			pre_tc.set_fresh_type_cache(false)
			prealloc_scope_free_for_v3(monomorph_scope)
		} else {
			monomorph_used_fns, monomorph_errors, generated_monomorph_specs = transform.monomorphize_with_used_checked_config_scoped_cached(mut a, &pre_tc, monomorph_input_used, !current_no_parallel
				&& should_parallel_monomorphize() && !incremental_cache_hit, unsafe { nil }, cached_monomorph_specs)
		}
		texts_canonical_after_annotation = true
		// Monomorphization publishes every synthesized or rewritten AST string
		// after its final worker merge, including the serial/no-worker path.
		transform_texts_canonical = true
		if incremental_cache_hit {
			incremental_stage_used_fns = monomorph_used_fns.move()
			for idx in incremental_monomorph_node_start .. a.nodes.len {
				if a.specialized_fn_nodes[idx] && a.nodes[idx].kind == .fn_decl {
					incremental_changed_names[a.nodes[idx].value] = true
				}
			}
		} else {
			used_fns = monomorph_used_fns.move()
		}
		if is_repl {
			pre_tc.notices.clear()
		}
		if pre_tc.notices.len > 0 || pre_tc.errors.len > 0 {
			if cache_state.manager.enabled {
				cached_checker_diagnostics << cache_v3_type_diagnostics(a, pre_tc.notices)
			}
			if pre_tc.errors.len == 0
				|| !macos_v3_fallback_suppresses_diagnostics(macos_v3_fallback_file) {
				print_type_diagnostics(a, pre_tc.notices, pre_tc.errors, is_checker_fixture)
			}
			for notice in pre_tc.notices {
				if notice.severity == 'warning:' {
					checker_warning_count++
				} else {
					checker_notice_count++
				}
			}
			pre_tc.notices.clear()
		}
		if pre_tc.errors.len > 0 {
			exit(1)
		}
		if monomorph_errors.len > 0 {
			eprintln('type checker found ${monomorph_errors.len} error(s):')
			for message in monomorph_errors {
				eprintln(message)
			}
			exit(1)
		}
	}
	if is_prof {
		add_v3_profile_used_fns(mut used_fns)
	}
	pre_tc.clear_c_type_cache()
	mut mono_tail_sw := time.new_stopwatch()
	// Transform and monomorphization can synthesize or rewrite payload text.
	// They run with private/arena-backed worker state; publish only canonical,
	// compilation-owned strings after all worker merges are complete.
	// Type annotation can rewrite node.typ after the transform arenas have been
	// promoted, so preallocated builds must republish those final strings — but
	// annotation only runs outside -building-v builds. Self-host builds skip
	// it, and their transform-canonical texts are already final, so this
	// full-AST walk would be a no-op there.
	annotation_can_rewrite_texts := !building_v && !texts_canonical_after_annotation
	if (scope_prealloc_stages && annotation_can_rewrite_texts) || !transform_texts_canonical {
		a.intern_node_texts_from(0)
	}
	if verbose {
		eprintln('  [ttime] mono intern texts  ${f64(mono_tail_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		mono_tail_sw.restart()
	}
	// The resolution-type view cache memoizes forked type-parse views keyed by file
	// path. Views (and their keys) built during the scoped check/annotate phases
	// live in stage arenas whose deferred frees run mid-codegen, so a stale entry
	// dangles when parse_resolution_type consults it during cgen. Discard the cache
	// here so the backend rebuilds views in the durable compilation arena. The
	// generic dependency-cache path already resets it after annotation.
	if !generic_cache_hit {
		pre_tc.reset_resolution_type_view_cache()
	}
	if cgen_cache_hit {
		b.step('monomorphize (cached)')
	} else if incremental_cache_hit {
		b.step('monomorphize (incremental)')
	} else if generic_cache_hit {
		b.step('monomorphize (dependency cache)')
	} else if uses_generics {
		b.step('monomorphize')
	} else {
		b.step('finalize')
	}
	stage_macos_v3_compiler_error_fallback(macos_v3_fallback_file, 'backend code generation')
	if backend == 'wasm' {
		if msg := unsupported_backend_error(a, &pre_tc, used_fns, backend) {
			eprintln(msg)
			exit(1)
		}
		$if !skip_wasm ? {
			// Generate only after monomorphization has pruned deferred generic comptime
			// branches. output_file is the exact path requested via -o (or the
			// <name>.wasm default).
			mut g := wasmgen.Gen.new(a, pre_tc, used_fns)
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
	mut newly_cached_module_count := 0
	if backend == 'arm64' {
		if msg := unsupported_backend_error(a, &pre_tc, used_fns, backend) {
			eprintln(msg)
			exit(1)
		}
		$if !skip_arm64 ? {
			// SSA + ARM64 native backend
			mut m := ssa.build_with_options(a, used_fns, pre_tc, ssa.BuildOptions{
				track_uses: is_prod
			})
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
			m.release_codegen_analysis_metadata()

			mut g := arm64.Gen.new(m)
			g.gen()
			b.step('arm64 gen')

			g.write_and_link(bin_file)
			b.step('link')
		}
	} else {
		// C backend (default)
		// Large generic user programs retain their transformed AST through cgen.
		// Bounded serial batches prevent worker snapshots from overlapping that live
		// set at the memory-limit peak; smaller programs keep the parallel fast path.
		if scope_prealloc_cgen && !building_v && !cmd_v_build
			&& a.nodes.len >= scoped_serial_user_cgen_node_threshold {
			cache_no_parallel_cgen = true
		}
		c_standard := c_standard_flag(prefs.c99)
		use_cached_dev_dylib := cache_state.manager.enabled && remove_binary_after_run && !is_prod
			&& !is_shared && !is_selfhost && prefs.normalized_target_os() == 'macos'
		mut cc_dir := ''
		mut cc_src := output_file
		mut cc_out := ''
		if !c_only {
			bin_dir := if os.dir(bin_file).len > 0 {
				os.real_path(os.dir(bin_file))
			} else {
				os.getwd()
			}
			cc_dir = os.join_path_single(bin_dir, '.${os.base(bin_file)}.v3cc.${tempname.unique_token()}')
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
		incremental_cached_support := if incremental_cache_hit {
			incremental_c_cached_declarations(incremental_cached_body)
		} else {
			''
		}
		incremental_known_declarations := incremental_c_declarations + '\n' + incremental_cached_support
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
		// Test harness declarations must remain ahead of their function bodies, so
		// scoped test generation stays serial instead of streaming worker batches.
		// The completed translation unit is already on disk before the stage arena
		// is released, allowing large tests to start Clang without retaining Cgen's
		// multi-gigabyte scratch state.
		if !cgen_cache_hit && scope_prealloc_cgen {
			cgen_parse_cache_enabled := pre_tc.type_cache_parse_enabled()
			cgen_scope := prealloc_scope_begin_for_v3()
			mut scoped_generated_c_flags := []string{}
			generated_path := if cache_state.manager.enabled { cache_plan_file } else { cc_src }
			mut g := cgen.FlatGen.new()
			g.set_initial_c_flags(user_c_flags)
			g.set_macro_probe_c_flags(environment_c_flags)
			g.set_c99_mode(prefs.c99)
			g.set_ccompiler(prefs.ccompiler)
			g.set_prod(prefs.is_prod)
			g.set_check_overflow(check_overflow)
			g.set_force_bounds_checking(prefs.force_bounds_checking)
			g.set_prealloc('prealloc' in prefs.user_defines)
			g.set_skip_generics(skip_transform_generics)
			g.set_skip_enum_autostr(trivial_literal_output)
			g.set_compiler_vexe(prefs.vexe)
			g.set_compiler_vexe_env_setup(!pref.has_macos_v3_caller_environment())
			g.set_target(prefs.target)
			g.set_thread_stack_size(prefs.thread_stack_size)
			g.set_show_test_stats(show_test_stats)
			g.set_show_test_summary(is_test_command)
			g.set_test_run_only(run_only)
			g.set_print_fn_names(print_fn_names)
			g.set_profile(profile_file, profile_no_inline, profile_fns)
			g.set_shared(prefs.is_shared)
			g.set_object_file_mode(is_o)
			g.set_suppress_main('no_main' in prefs.user_defines)
			g.set_coverage(coverage_dir, args.join(' '))
			g.set_compile_values(prefs.compile_values)
			g.set_track_heap('track_heap' in prefs.user_defines)
			g.set_cache_split(cache_state.manager.enabled)
			g.set_cache_native_input_paths(cache_scoped_native_input_paths(cache_state))
			g.set_program_body_only(generic_cache_hit)
			g.set_cache_program_files(user_files)
			g.set_incremental_fn_names(incremental_changed_names)
			g.set_cached_support_declarations(incremental_known_declarations)
			g.set_scope_parallel_workers(!generic_cache_hit)
			g.gen_to_file_with_used_test_options(generated_path, a, cgen_used_fns, &pre_tc, cache_no_parallel_cgen || test_files.len > 0, test_files) or {
				eprintln('error writing ${generated_path}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			if !incremental_cache_hit {
				scoped_generated_c_flags = g.c_flags()
			}
			g.free_parallel_worker_scopes()
			prealloc_scope_leave_for_v3(cgen_scope)
			if !incremental_cache_hit {
				generated_c_flags = clone_string_list(scoped_generated_c_flags)
			}
			// Cgen's synchronous type queries memoize through the shared checker.
			// Reattach empty parent-owned interners and caches before releasing its
			// stage arena; all of them can grow while servicing those queries.
			pre_tc.reset_type_interners()
			pre_tc.set_fresh_type_cache(cgen_parse_cache_enabled)
			prealloc_scope_free_for_v3(cgen_scope)
		} else if !cgen_cache_hit {
			generated_path := if cache_state.manager.enabled { cache_plan_file } else { cc_src }
			mut g := cgen.FlatGen.new()
			g.set_initial_c_flags(user_c_flags)
			g.set_macro_probe_c_flags(environment_c_flags)
			g.set_c99_mode(prefs.c99)
			g.set_ccompiler(prefs.ccompiler)
			g.set_prod(prefs.is_prod)
			g.set_check_overflow(check_overflow)
			g.set_force_bounds_checking(prefs.force_bounds_checking)
			g.set_prealloc('prealloc' in prefs.user_defines)
			g.set_skip_generics(skip_transform_generics)
			g.set_skip_enum_autostr(trivial_literal_output)
			g.set_compiler_vexe(prefs.vexe)
			g.set_compiler_vexe_env_setup(!pref.has_macos_v3_caller_environment())
			g.set_target(prefs.target)
			g.set_thread_stack_size(prefs.thread_stack_size)
			g.set_show_test_stats(show_test_stats)
			g.set_show_test_summary(is_test_command)
			g.set_test_run_only(run_only)
			g.set_print_fn_names(print_fn_names)
			g.set_profile(profile_file, profile_no_inline, profile_fns)
			g.set_shared(prefs.is_shared)
			g.set_object_file_mode(is_o)
			g.set_suppress_main('no_main' in prefs.user_defines)
			g.set_coverage(coverage_dir, args.join(' '))
			g.set_compile_values(prefs.compile_values)
			g.set_track_heap('track_heap' in prefs.user_defines)
			g.set_cache_split(cache_state.manager.enabled)
			g.set_cache_native_input_paths(cache_scoped_native_input_paths(cache_state))
			g.set_program_body_only(generic_cache_hit)
			g.set_cache_program_files(user_files)
			g.set_incremental_fn_names(incremental_changed_names)
			g.set_cached_support_declarations(incremental_known_declarations)
			g.gen_to_file_with_used_test_options(generated_path, a, cgen_used_fns, &pre_tc, cache_no_parallel_cgen, test_files) or {
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
			cached_prefix := os.read_file(generic_cache_entry.prefix) or {
				eprintln('error reading incremental cached prefix ${generic_cache_entry.prefix}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			merged_cached_source := merge_incremental_program_body(incremental_cached_body, cached_prefix, changed_source, incremental_changed_keys) or {
				os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
				restart_v3_after_cache_invalidation()
				''
			}
			merged_source :=
				modulecache.materialize_cached_body_string_definitions(merged_cached_source)
			os.write_file(cache_plan_file, merged_source) or {
				eprintln('error writing merged incremental C source ${cache_plan_file}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		}
		a.close_workers()
		if cgen_cache_hit {
			b.step('cgen (cached)')
		} else if incremental_cache_hit {
			b.step_parallel('cgen (incremental)', cgen_was_parallel)
		} else {
			b.step_parallel('cgen', cgen_was_parallel)
		}
		pic_flag := shared_pic_flag(is_shared || use_cached_dev_dylib, prefs.normalized_target_os())
		target_args := if c_only {
			[]string{}
		} else {
			c_compiler_target_args(prefs.target, c_compiler_explicit) or {
				eprintln(err.msg())
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
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
		wrapv_flag := c_wrapv_flag(prefs.normalized_target_os())
		if wrapv_flag.len > 0 {
			warn_args << wrapv_flag
		}
		mut all_compile_c_flags := environment_c_flags.clone()
		all_compile_c_flags << generated_c_flags
		needs_objective_c := c_flags_need_objective_c(all_compile_c_flags)
		large_prod_c_unit := os.is_file(published_c_source)
			&& v3_is_large_prod_c_unit(os.file_size(published_c_source))
		limit_large_unit_inlining := large_prod_c_unit && effective_c_compiler == 'clang'
		link_uses_non_c_language := c_link_flags_use_non_c_language(all_compile_c_flags)
		link_c_standard := if link_uses_non_c_language {
			''
		} else {
			c_standard
		}
		mut resolved_c_flags := if generate_c_project.len > 0 {
			v3_c_project_dependency_flags(generated_c_flags)
		} else {
			generated_c_flags.clone()
		}
		if !c_only || (dump_c_flags.len > 0 && generate_c_project.len == 0) {
			object_optimization_flags := v3_prod_c_object_optimization_flags(is_prod, no_prod_options, is_shared, parallel_cc, explicit_tcc)
			resolved_c_flags = prepare_c_flags_for_link(generated_c_flags, environment_c_flags, object_optimization_flags, prefs.c99, pic_flag, target_args, prefs.target, c_compiler, cc_dir, mut c_object_cache_stats) or {
				message := err.msg()
				if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
					published_c_source,
					cache_plan_file,
					cc_src,
				], fallback_report_sources) {
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				eprintln(message)
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			b.step('C object cache')
		}
		flag_plan_sdk_root := if explicit_tcc && prefs.normalized_target_os() == 'macos' {
			macos_sdk_root_cache.get()
		} else {
			''
		}
		c_flag_plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
			environment_c_flags: environment_c_flags
			environment_ld_flags: environment_ld_flags
			target_args: target_args
			link_c_standard: link_c_standard
			dependencies: resolved_c_flags
			warn_args: warn_args
			vroot: prefs.vroot
			target_os: prefs.normalized_target_os()
			target_arch: prefs.normalized_target_arch()
			macos_sdk_root: flag_plan_sdk_root
			pic_flag: pic_flag
			is_prod: is_prod
			no_prod_options: no_prod_options
			is_shared: is_shared
			parallel_cc: parallel_cc
			large_c_unit: large_prod_c_unit
			limit_inlining: limit_large_unit_inlining
			explicit_tcc: explicit_tcc
			is_c_debug: is_c_debug
			is_o: is_o
			is_liveshared: is_liveshared
		})
		mut native_support_inputs := []string{}
		if explicit_tcc {
			atomic_input := if generate_c_project.len > 0 {
				tcc_atomic_s_arg(prefs)
			} else {
				tcc_atomic_arg(prefs, c_compiler, c_flag_plan.tcc_includes)
			}
			if atomic_input.len > 0 {
				native_support_inputs << atomic_input
			}
		}
		if dump_c_flags.len > 0 {
			mut dump_support_flags := v3_c_source_mode_flags(needs_objective_c)
			dump_support_flags << native_support_inputs
			dumped_flags := c_flag_plan.all_flags(dump_support_flags)
			output := if dumped_flags.len > 0 {
				dumped_flags.join('\n') + '\n'
			} else {
				''
			}
			if dump_c_flags == '-' {
				print(output)
			} else {
				os.write_file(dump_c_flags, output) or {
					eprintln('failed to write C flags to ${dump_c_flags}: ${err.msg()}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
			}
		}
		if c_only {
			b.metric('generated C size', os.file_size(cc_src), 'bytes')
			if c_to_stdout {
				source := os.read_file(cc_src) or {
					eprintln('error reading generated C source ${cc_src}: ${err.msg()}')
					os.rm(cc_src) or {}
					exit(1)
				}
				print(source)
				os.rm(cc_src) or {}
			} else if generate_c_project.len > 0 {
				write_v3_c_project(generate_c_project, cc_src, c_compiler, c_flag_plan, native_support_inputs, needs_objective_c) or {
					eprintln('cannot write generated C project: ${err.msg()}')
					exit(1)
				}
				println('Generated C project in ${generate_c_project}')
			}
			b.print_report()
			clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
			return
		}
		mut tcc_link_has_incompatible_objects := false
		if prefs.normalized_target_os() == 'macos' {
			for flag in resolved_c_flags {
				if c_flag_is_object_file(flag.trim_space()) {
					tcc_link_has_incompatible_objects = true
					break
				}
			}
		}
		mut cached_objects := []string{}
		mut cached_dev_dylib := ''
		mut prefix_source_identity := cgen_cache_metadata.prefix_source_identity
		mut tcc_main_file := ''
		mut cache_full_tcc_source := ''
		mut retained_full_c_source := ''
		mut cached_program_body_source := if cgen_cache_hit { cgen_cache_entry.source } else { '' }
		mut refreshed_incremental_body := ''
		if cache_state.manager.enabled {
			cache_prepare_scope := prealloc_scope_begin_for_v3()
			if interface_impl_signature.len == 0 {
				interface_impl_signature = pre_tc.interface_impl_set_signature()
			}
			opt_flag := v3_prod_c_optimization_flags(is_prod, no_prod_options, is_shared, parallel_cc, large_prod_c_unit, limit_large_unit_inlining, explicit_tcc).join(' ')
			warning_flags := warn_args.join(' ')
			mut compile_signature := v3_cached_object_compile_signature(c_standard, opt_flag, pic_flag, warning_flags, resolved_c_flags, needs_objective_c, interface_impl_signature)
			mut prepared_plan_entry := cgen_cache_entry
			mut prepared_cache := V3PreparedModuleCache{}
			if cgen_prepared_hit {
				if generated_c_flags.len == 0 && !generic_cache_hit && !incremental_cache_hit
					&& p.parsed_v_header_files == 0 && os.is_file(cgen_cache_entry.source) {
					cache_full_tcc_source = os.join_path_single(cc_dir, 'full.c')
					os.cp(cgen_cache_entry.source, cache_full_tcc_source) or {
						cache_full_tcc_source = ''
					}
				}
				if backend_explicit && os.is_file(cgen_cache_entry.source) {
					retained_full_c_source = os.join_path_single(cc_dir, 'retained_full.c')
					os.cp(cgen_cache_entry.source, retained_full_c_source) or {
						eprintln('error preserving complete cached C source ${cgen_cache_entry.source}: ${err.msg()}')
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
				}
				prefix_source := os.read_file(cgen_prepared_entry.prefix) or {
					eprintln('error reading cached program prefix ${cgen_prepared_entry.prefix}: ${err.msg()}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				compile_signature = v3_cached_object_wrapper_compile_signature(compile_signature, prefix_source)
				objects := cache_state.manager.valid_cgen_prepared_objects(cgen_cache_entry, compile_signature) or {
					if resolve_flag_specific_cache_objects(mut cache_state, compile_signature) {
						os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
						restart_v3_after_cache_invalidation()
					}
					resolved_objects := cache_object_paths(cache_state.objects)
					cache_state.manager.write_cgen_prepared_objects(cgen_cache_entry, compile_signature, resolved_objects) or {}
					resolved_objects
				}
				prepared_cache = V3PreparedModuleCache{
					program_prefix_source: prefix_source
					objects: objects
				}
				published_c_source = cgen_prepared_entry.main
			} else {
				generated_source := os.read_file(cache_plan_file) or {
					eprintln('error reading cache-marked C source ${cache_plan_file}: ${err.msg()}')
					exit(1)
				}
				compile_signature = v3_cached_object_wrapper_compile_signature(compile_signature, generated_source)
				if generated_c_flags.len == 0 && !generic_cache_hit && !incremental_cache_hit
					&& p.parsed_v_header_files == 0 {
					cache_full_tcc_source = os.join_path_single(cc_dir, 'full.c')
					os.write_file(cache_full_tcc_source, generated_source) or {
						cache_full_tcc_source = ''
					}
				}
				if backend_explicit {
					retained_full_c_source = os.join_path_single(cc_dir, 'retained_full.c')
					os.write_file(retained_full_c_source, generated_source) or {
						eprintln('error preserving complete generated C source ${retained_full_c_source}: ${err.msg()}')
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
				}
				if generic_cache_hit {
					if incremental_cache_restored {
						cached_prefix := os.read_file(generic_cache_entry.prefix) or {
							eprintln('error reading cached incremental prefix ${generic_cache_entry.prefix}: ${err.msg()}')
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						prepared_cache = prepare_v3_incremental_cached_body(cache_plan_file, incremental_prefix_path, incremental_tcc_declarations_path, cached_prefix, compile_signature, mut cache_state) or {
							message := err.msg()
							if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
								cache_plan_file,
								published_c_source,
								cc_src,
							], fallback_report_sources) {
								cleanup_c_build_dir(cc_dir)
								exit(1)
							}
							eprintln(message)
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
						cached_body := os.read_file(generic_cache_entry.body) or {
							eprintln('error reading cached generic body ${generic_cache_entry.body}: ${err.msg()}')
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						prepared_cache = prepare_v3_cached_generic_body(generated_source, cached_prefix, cached_declarations, cached_body, compile_signature, mut cache_state) or {
							message := err.msg()
							if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
								cache_plan_file,
								published_c_source,
								cc_src,
							], fallback_report_sources) {
								cleanup_c_build_dir(cc_dir)
								exit(1)
							}
							eprintln(message)
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
					}
				} else {
					prepared_cache = prepare_v3_module_cache(generated_source, &cgen_used_fns, &program_used_fns, &pre_tc, c_standard, opt_flag, pic_flag, warning_flags, resolved_c_flags, needs_objective_c, interface_impl_signature, mut cache_state) or {
						message := err.msg()
						if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
							cache_plan_file,
							published_c_source,
							cc_src,
						], fallback_report_sources) {
							cleanup_c_build_dir(cc_dir)
							exit(1)
						}
						eprintln(message)
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
					prefix_source_identity = v3_program_prefix_source_identity(prepared_cache.program_prefix_source, prepared_cache.objects)
				}
				if !cgen_cache_hit && program_cache_enabled {
					published_cgen_cache_input := v3_cgen_cache_input(cache_state, user_files, cache_c_flags)
					prepared_plan_entry = cache_state.manager.write_cgen(published_cgen_cache_input.source_files, published_cgen_cache_input.generation_signature, published_cgen_cache_input.dependency_inputs, generated_source, encode_v3_cgen_metadata(generated_c_flags, interface_impl_signature, prefix_source_identity, cached_checker_diagnostics)) or { modulecache.CgenEntry{} }
				}
				if incremental_cache_restored && prepared_plan_entry.source.len > 0 {
					stable_body_source := os.read_file(prepared_plan_entry.source) or {
						eprintln('error reading incremental cached body ${prepared_plan_entry.source}: ${err.msg()}')
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
					refreshed_incremental_body = stable_body_source
					stable_main_source := v3_incremental_program_main_source(prepared_cache.program_prefix_source, stable_body_source)
					stable_tcc_main_source := v3_incremental_main_source(incremental_tcc_declarations_path, prepared_plan_entry.source)
					prepared_cache.main_source = stable_main_source
					prepared_cache.tcc_main_source = stable_tcc_main_source
					os.write_file(cc_src, stable_main_source) or {
						eprintln('error writing incremental cached main source ${cc_src}: ${err.msg()}')
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
				}
				if prepared_plan_entry.stamp.len > 0 {
					cached_program_body_source = prepared_plan_entry.source
					cache_state.manager.write_cgen_prepared(prepared_plan_entry, prepared_cache.main_source, prepared_cache.tcc_main_source, prepared_cache.program_prefix_source) or {}
					cache_state.manager.write_cgen_prepared_objects(prepared_plan_entry, compile_signature, prepared_cache.objects) or {}
				}
				if !generic_cache_hit && generic_cache_signature.len > 0
					&& generated_monomorph_specs.len > 0 {
					published_generic_input := v3_cgen_cache_input(cache_state, user_files, cache_c_flags)
					cache_state.manager.write_generic_program(published_generic_input.source_files, generic_cache_signature, published_generic_input.generation_signature, published_generic_input.dependency_inputs, encode_monomorph_cache_specs(generated_monomorph_specs), encode_cached_used_fns(program_used_fns), prepared_cache.program_prefix_source, modulecache.prune_unreferenced_static_string_definitions(prepared_cache.program_declarations), prepared_cache.program_body_cache, encode_cached_runtime_strings(generic_cache_runtime_strings), encode_v3_cgen_metadata(generated_c_flags, interface_impl_signature, prefix_source_identity, cached_checker_diagnostics)) or {}
				}
				if (!generic_cache_hit || incremental_cache_hit)
					&& incremental_snapshot.declaration_signature.len > 0 {
					published_incremental_input := v3_cgen_cache_input(cache_state, user_files, cache_c_flags)
					incremental_body := if incremental_cache_hit {
						refreshed_incremental_body
					} else {
						prepared_cache.program_body_cache
					}
					incremental_used := if incremental_cache_hit {
						cached_program_used_fns
					} else {
						program_used_fns
					}
					incremental_specs := merge_monomorph_cache_specs(cached_monomorph_specs, generated_monomorph_specs)
					incremental_declarations := if incremental_cache_hit {
						os.read_file(generic_cache_entry.declarations) or { '' }
					} else {
						modulecache.prune_unreferenced_static_string_definitions(prepared_cache.program_declarations)
					}
					incremental_tcc_declarations := if incremental_cache_hit {
						incremental_c_declarations
					} else {
						prepared_cache.tcc_program_declarations
					}
					cache_state.manager.write_incremental_program(published_incremental_input.source_files, incremental_snapshot.declaration_signature, published_incremental_input.generation_signature, published_incremental_input.dependency_inputs, encode_incremental_manifest(incremental_snapshot), incremental_body, encode_cached_used_fns(incremental_used), encode_monomorph_cache_specs(incremental_specs), prepared_cache.program_prefix_source, incremental_declarations, incremental_tcc_declarations, prepared_cache.objects, encode_v3_cgen_metadata(generated_c_flags, interface_impl_signature, prefix_source_identity, cached_checker_diagnostics)) or {}
				}
			}
			prealloc_scope_leave_for_v3(cache_prepare_scope)
			if prefix_source_identity.len > 0 {
				prefix_source_identity = prefix_source_identity.clone()
			}
			if cached_program_body_source.len > 0 {
				cached_program_body_source = cached_program_body_source.clone()
			}
			if cache_full_tcc_source.len > 0 {
				cache_full_tcc_source = cache_full_tcc_source.clone()
			}
			if retained_full_c_source.len > 0 {
				retained_full_c_source = retained_full_c_source.clone()
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
					prefix_source_identity = v3_program_prefix_source_identity(prepared_cache.program_prefix_source, prepared_cache.objects)
				}
				prefix_object := compile_v3_program_object('prefix', prepared_cache.program_prefix_source, prefix_source_identity, v3_program_external_input_paths(&cache_state), &cache_state.manager, c_standard, opt_flag, pic_flag, warning_flags, resolved_c_flags, needs_objective_c, target_args, prefs.target, c_compiler, mut c_object_cache_stats) or {
					message := err.msg()
					if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
						published_c_source,
						cache_plan_file,
						cc_src,
					], fallback_report_sources) {
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
					eprintln(message)
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				cached_dev_dylib = compile_v3_dev_dylib(prefix_object, prepared_cache.objects, resolved_c_flags, &cache_state.manager, target_args, prefs.target, c_compiler, cc_dir, !silent || show_cc, mut c_object_cache_stats) or {
					message := err.msg()
					if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
						published_c_source,
						cache_plan_file,
						cc_src,
					], fallback_report_sources) {
						cleanup_c_build_dir(cc_dir)
						exit(1)
					}
					eprintln(message)
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
		// Before the driver was split out of v3.v, explicit `-b c` binary builds
		// retained `<output>.c`. Codegen tooling relies on that stable path.
		if backend_explicit && !c_only {
			retained_c := bin_file + '.c'
			staged_c := '${retained_c}.stage.${tempname.unique_token()}'
			retained_c_source := if retained_full_c_source.len > 0 {
				retained_full_c_source
			} else if cache_full_tcc_source.len > 0 {
				cache_full_tcc_source
			} else {
				published_c_source
			}
			os.cp(retained_c_source, staged_c) or {
				eprintln('failed to stage generated C output ${retained_c}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			os.mv(staged_c, retained_c) or {
				os.rm(staged_c) or {}
				eprintln('failed to retain generated C output ${retained_c}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		}
		mut cached_program_main_object := ''
		if use_macos_dev_program_cache && !use_cached_dev_dylib && !is_c_debug && !needs_objective_c {
			program_main_source := os.read_file(published_c_source) or {
				eprintln('error reading cached program source ${published_c_source}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			program_main_identity := modulecache.file_signature(published_c_source)
			cached_program_main_object = compile_v3_program_object('main', program_main_source, program_main_identity, v3_program_external_input_paths(&cache_state), &cache_state.manager, c_standard, '', pic_flag, warn_args.join(' '), resolved_c_flags, needs_objective_c, target_args, prefs.target, c_compiler, mut c_object_cache_stats) or {
				message := err.msg()
				if request_macos_v3_c_error_fallback_from_message(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, message, [
					published_c_source,
					cache_plan_file,
					cc_src,
				], fallback_report_sources) {
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				eprintln(message)
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			b.step('C program object cache')
		}
		if keep_c {
			keep_c_file := keep_c_output_file(bin_file)
			staged_c := '${keep_c_file}.stage.${tempname.unique_token()}'
			os.cp(published_c_source, staged_c) or {
				eprintln('failed to stage generated C output ${keep_c_file}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			os.mv(staged_c, keep_c_file) or {
				os.rm(staged_c) or {}
				eprintln('failed to retain generated C output ${keep_c_file}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
		}
		if parallel_cc && v3_parallel_cc_active_sources_include_external_definition(a, user_files) {
			eprintln('failed to link after parallel C compilation')
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
		mut used_tcc := false
		if cached_dev_dylib.len > 0 && tcc_main_file.len > 0 && !link_uses_non_c_language
			&& !is_c_debug {
			tried_tcc = true
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_resources := v3_tcc_resource_flags(prefs.vroot)
			mut tcc_args := [c_standard, tcc_resources.base_arg, tcc_resources.include_arg,
				tcc_resources.library_arg, '-w', '-Werror=implicit-function-declaration']
			tcc_sdk_root := if prefs.normalized_target_os() == 'macos' {
				macos_sdk_root_cache.get()
			} else {
				''
			}
			tcc_args << v3_tcc_host_system_flags(prefs.normalized_target_os(), tcc_sdk_root)
			if v3_tcc_backtrace_enabled(prefs.normalized_target_os(), prefs.normalized_target_arch(), is_shared) {
				tcc_args << '-bt25'
			}
			if wrapv_flag.len > 0 {
				tcc_args << wrapv_flag
			}
			tcc_args << tcc_cached_main_flags(resolved_c_flags)
			tcc_args << ['-o', 'out', os.base(tcc_main_file)]
			atomic_s := tcc_atomic_arg(prefs, tcc_path, tcc_resources.include_arg)
			if atomic_s.len > 0 {
				tcc_args << atomic_s
			}
			tcc_args << tcc_native_c_source_flags(resolved_c_flags)
			tcc_args << cached_dev_dylib
			tcc_args << tcc_dynamic_link_flags(resolved_c_flags)
			add_v3_default_linker_flags(mut tcc_args, prefs.normalized_target_os(), is_o)
			program_source_identity := '${prefix_source_identity}\n${modulecache.file_signature(tcc_main_file)}\n${if cached_program_body_source.len > 0 {
				modulecache.file_signature(cached_program_body_source)
			} else {
				''
			}}'
			tcc_cached_executable := v3_cached_tcc_executable_path(&cache_state.manager, program_source_identity, c_object_cache_stats.link_plan_signature, tcc_path, tcc_resources.install_dir, tcc_args)
			if os.is_file(tcc_cached_executable) {
				os.cp(tcc_cached_executable, cc_out) or {}
				tcc_cache_hit = os.is_file(cc_out)
				used_tcc = tcc_cache_hit
			}
			if !silent || show_cc {
				println('  > ${cmdexec.display(tcc_path, tcc_args)}${if tcc_cache_hit {
					' (cached)'
				} else {
					''
				}}')
			}
			if !tcc_cache_hit {
				result = cmdexec.run_in(tcc_path, tcc_args, cc_dir)
				show_v3_c_compiler_output(show_c_output, tcc_path, result)
				if result.exit_code == 0 {
					used_tcc = true
					publish_v3_cached_executable(cc_out, tcc_cached_executable)
				}
			}
		}
		// Cached module objects can make tcc accept an unresolved call in the
		// program translation unit and emit a broken executable. When the original
		// complete C unit has no native-source flags, compile that unit instead of
		// linking the cached objects. Otherwise use the system compiler for the
		// smaller cached main unit.
		if !tried_tcc && !is_prod && !needs_objective_c && !link_uses_non_c_language
			&& (!tcc_link_has_incompatible_objects || cache_full_tcc_source.len > 0)
			&& target_args.len == 0 && (!c_compiler_explicit || explicit_tcc)
			&& (!cache_state.manager.enabled || cache_full_tcc_source.len > 0) && !is_c_debug
			&& dump_c_flags.len == 0 {
			tried_tcc = true
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			bundled_tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_path := if explicit_tcc && c_compiler in ['tcc', 'tinyc']
				&& os.is_executable(bundled_tcc_path) {
				bundled_tcc_path
			} else if explicit_tcc {
				c_compiler
			} else {
				bundled_tcc_path
			}
			tcc_resources := v3_tcc_resource_flags(prefs.vroot)
			mut tcc_args := environment_c_flags.clone()
			if link_c_standard.len > 0 {
				tcc_args << link_c_standard
			}
			if pic_flag.len > 0 {
				tcc_args << pic_flag
			}
			tcc_args << [tcc_resources.base_arg, tcc_resources.include_arg,
				tcc_resources.library_arg]
			tcc_sdk_root := if prefs.normalized_target_os() == 'macos' {
				macos_sdk_root_cache.get()
			} else {
				''
			}
			tcc_args << v3_tcc_host_system_flags(prefs.normalized_target_os(), tcc_sdk_root)
			if v3_tcc_backtrace_enabled(prefs.normalized_target_os(), prefs.normalized_target_arch(), is_shared) {
				tcc_args << '-bt25'
			}
			tcc_args << warn_args
			if is_shared {
				tcc_args << '-shared'
			} else if is_o {
				tcc_args << '-c'
			}
			tcc_source := if cache_full_tcc_source.len > 0 {
				os.base(cache_full_tcc_source)
			} else {
				'src.c'
			}
			tcc_args << ['-o', 'out', tcc_source]
			atomic_s := tcc_atomic_arg(prefs, tcc_path, tcc_resources.include_arg)
			if atomic_s.len > 0 {
				tcc_args << atomic_s
			}
			tcc_args << resolved_c_flags
			add_v3_default_linker_flags(mut tcc_args, prefs.normalized_target_os(), is_o)
			if !is_o {
				tcc_args << environment_ld_flags
			}
			if !silent || show_cc {
				println('  > ${cmdexec.display(tcc_path, tcc_args)}')
			}
			result = cmdexec.run_in(tcc_path, tcc_args, cc_dir)
			show_v3_c_compiler_output(show_c_output, tcc_path, result)
			used_tcc = result.exit_code == 0
		}
		if is_prod || !tried_tcc || result.exit_code != 0 {
			used_tcc = false
			if !os.is_file(cc_src) {
				os.cp(published_c_source, cc_src) or {
					eprintln('error restoring cached main source ${published_c_source}: ${err.msg()}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
			}
			fallback_source := if cached_dev_dylib.len > 0 && tcc_main_file.len > 0 {
				os.base(tcc_main_file)
			} else {
				'src.c'
			}
			mut compiler_inputs := []string{}
			if cached_program_main_object.len > 0 {
				compiler_inputs << cached_program_main_object
			} else if fallback_source == os.base(tcc_main_file) {
				compiler_inputs << ['-D__TINYC__', '-Wno-implicit-function-declaration',
					fallback_source]
			} else {
				compiler_inputs << v3_c_source_inputs(fallback_source, needs_objective_c)
			}
			compiler_inputs << native_support_inputs
			compiler_inputs << cached_objects
			if cached_dev_dylib.len > 0 {
				compiler_inputs << cached_dev_dylib
			}
			cc_args := c_flag_plan.compiler_args('out', compiler_inputs, [])
			if !silent || show_cc {
				println('  > ${cmdexec.display(c_compiler, cc_args)}')
			}
			result = cmdexec.run_in(c_compiler, cc_args, cc_dir)
			show_v3_c_compiler_output(show_c_output, c_compiler, result)
			if result.exit_code != 0 {
				if retry_compilation && v3_is_tcc_compilation_failure(c_compiler, result.output) {
					fallback := 'cc'
					eprintln('warning: tcc compilation failed, falling back to ${fallback}')
					retry_args := v3_retry_compilation_args(args, c_compiler_arg_index, fallback)
					cleanup_c_build_dir(cc_dir)
					retry_result := cmdexec.run(os.executable(), retry_args)
					if retry_result.output.len > 0 {
						print(retry_result.output)
					}
					if retry_result.exit_code != 0 {
						exit(retry_result.exit_code)
					}
					return
				}
				if request_macos_v3_c_error_fallback(macos_v3_fallback_file, macos_v3_c_error_dir, c_compiler, result.output, os.join_path_single(cc_dir, fallback_source), fallback_report_sources) {
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				if missing_library := v3_missing_c_library_name(result.output) {
					eprintln('builder error:
==================
C library `${missing_library}` was not found while linking the generated program.
Please install the corresponding development package/libraries and make sure the linker can find it.')
				} else if parallel_cc && (result.output.contains('duplicate symbol')
					|| result.output.contains('defined twice')
					|| result.output.contains('multiple definition')) {
					eprintln('failed to link after parallel C compilation')
					eprintln(result.output)
				} else if parallel_cc {
					eprintln('failed parallel C compilation')
					eprintln(result.output)
				} else if !retry_compilation {
					eprintln('C compilation error (from ${os.file_name(c_compiler)}):')
					eprintln(result.output)
				} else {
					eprintln('C compilation failed:')
					eprintln(result.output)
				}
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
		os.rm(cache_full_tcc_source) or {}
		os.rm(retained_full_c_source) or {}
		os.rm(cc_src) or {}
		os.rmdir(cc_dir) or {}
		b.step(if tcc_cache_hit {
			'tcc (cached)'
		} else if used_tcc {
			'tcc'
		} else {
			'cc'
		})
		clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)
		if should_run {
			if is_direct_vsh && !explicit_output {
				write_v3_crun_cache_marker(bin_file, crun_build_identity) or {}
			}
			run_result := run_binary(bin_file, run_args)
			if remove_binary_after_run {
				os.rm(bin_file) or {}
			}
			if run_result != 0 {
				exit(run_result)
			}
			b.step('run')
		} else if test_files.len > 0 && (!explicit_output || is_checker_fixture || show_test_stats) {
			test_result := run_test_binary(bin_file)
			if test_result != 0 {
				exit(test_result)
			}
			b.step('test')
		}
	}
	clear_macos_v3_compiler_error_fallback(macos_v3_fallback_file)

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
	b.metric('C object dep-scan fallbacks', c_object_cache_stats.dependency_scan_fallbacks, 'objects')
	b.metric('C object publish races', c_object_cache_stats.publish_races, 'objects')
	b.metric('C object input-snapshot races', c_object_cache_stats.input_snapshot_races, 'objects')
	if retained_transform_scope != unsafe { nil } {
		prealloc_scope_free_for_v3(retained_transform_scope)
		retained_transform_scope = unsafe { nil }
	}
	if retained_transform_prepare_scope != unsafe { nil } {
		prealloc_scope_free_for_v3(retained_transform_prepare_scope)
		retained_transform_prepare_scope = unsafe { nil }
	}
	if show_test_stats {
		println('checker summary: 0 V errors, ${checker_warning_count} V warnings, ${checker_notice_count} V notices')
	}
	b.print_report()
	if newly_cached_module_count > 0 && !silent {
		println('Hint: cached ${newly_cached_module_count} modules. They will not be recompiled on the next run unless they change.')
	}
}

fn checker_fixture_missing_header(a &flat.FlatAst, user_files []string, c_compiler string, user_defines []string) ?string {
	mut selected_files := map[string]bool{}
	for file in user_files {
		selected_files[os.real_path(file)] = true
	}
	mut current_file := ''
	mut current_module := 'main'
	mut selected := false
	for node in a.nodes {
		if node.kind == .file {
			current_file = node.value
			current_module = 'main'
			selected = os.real_path(current_file) in selected_files
			continue
		}
		if !selected {
			continue
		}
		if node.kind == .module_decl {
			current_module = node.value
			continue
		}
		if node.kind != .directive || node.value != 'include' {
			continue
		}
		raw_target, explanation := checker_fixture_include_target_message(node.typ)
		target := checker_fixture_resolve_include_define(raw_target, user_defines)
		if target.len < 3 || checker_fixture_header_exists(target, current_file, c_compiler) {
			continue
		}
		message := if explanation.len > 0 {
			explanation.trim_right('.') + '.'
		} else if shader_message := checker_fixture_missing_shader_message(target, current_file) {
			shader_message
		} else {
			'Please install the corresponding development headers.'
		}
		return 'Header file ${target}, needed for module `${current_module}` was not found. ${message}'
	}
	return none
}

fn v3_missing_c_library_name(output string) ?string {
	for line in output.split_into_lines() {
		if marker := line.index("ld: library '") {
			rest := line[marker + "ld: library '".len..]
			if end := rest.index("' not found") {
				if end > 0 {
					return rest[..end]
				}
			}
		}
		for marker in ['cannot find -l', 'library not found for -l'] {
			if offset := line.index(marker) {
				rest := line[offset + marker.len..].trim_space()
				mut end := 0
				for end < rest.len && !rest[end].is_space() && rest[end] !in [`'`, `"`] {
					end++
				}
				if end > 0 {
					return rest[..end]
				}
			}
		}
	}
	return none
}

fn v3_is_tcc_compilation_failure(c_compiler string, output string) bool {
	name := os.file_name(c_compiler).to_lower()
	if name == 'tcc' || name == 'tinyc' || name.starts_with('tcc-') || name.contains('tinycc') {
		return true
	}
	for line in output.split_into_lines() {
		if line.trim_space().to_lower().starts_with('tcc:') {
			return true
		}
	}
	return false
}

fn v3_parallel_cc_active_sources_include_external_definition(a &flat.FlatAst, source_files []string) bool {
	mut selected_files := map[string]bool{}
	for file in source_files {
		selected_files[os.real_path(file)] = true
	}
	mut current_file := ''
	mut selected := false
	// Checker/transform pruning replaces directives from inactive `$if` branches with empty
	// nodes, so this stream matches the target selected for generated C.
	for node in a.nodes {
		if node.kind == .file {
			current_file = node.value
			selected = os.real_path(current_file) in selected_files
			continue
		}
		if !selected || node.kind != .directive || node.value != 'include' {
			continue
		}
		raw_target, _ := checker_fixture_include_target_message(node.typ)
		if !raw_target.starts_with('"') {
			continue
		}
		rest := raw_target[1..]
		end := rest.index('"') or { continue }
		header_path := rest[..end].replace('@DIR', os.dir(current_file))
		header := os.read_file(header_path) or { continue }
		for header_line in header.split_into_lines() {
			declaration := header_line.trim_space()
			if declaration.len == 0 || declaration.starts_with('#')
				|| declaration.starts_with('static ') || declaration.starts_with('inline ')
				|| declaration.starts_with('typedef ') {
				continue
			}
			if declaration.contains('(') && declaration.contains(')') && declaration.contains('{') {
				return true
			}
		}
	}
	return false
}

fn v3_retry_compilation_args(args []string, c_compiler_arg_index int, fallback string) []string {
	mut retry_args := args.clone()
	if c_compiler_arg_index >= 0 && c_compiler_arg_index + 1 < retry_args.len {
		retry_args[c_compiler_arg_index + 1] = fallback
	} else {
		retry_args.insert(0, fallback)
		retry_args.insert(0, '-cc')
	}
	mut public_args := []string{cap: retry_args.len + 1}
	public_args << '-no-retry-compilation'
	for arg in retry_args {
		if arg !in [macos_v3_compat_c99_flag, macos_v3_internal_quiet_flag] {
			public_args << arg
		}
	}
	return public_args
}

fn checker_fixture_include_target_message(raw string) (string, string) {
	if marker := raw.index(' #') {
		return raw[..marker].trim_space(), raw[marker + 2..].trim_space()
	}
	return raw.trim_space(), ''
}

fn checker_fixture_resolve_include_define(target string, user_defines []string) string {
	start := target.index(r'$d(' + "'") or { return target }
	name_end := target.index_after("','", start + 4) or { return target }
	default_end := target.index_after("')", name_end + 3) or { return target }
	name := target[start + 4..name_end]
	default_value := target[name_end + 3..default_end]
	mut value := default_value
	for define in user_defines {
		if define == name {
			value = '1'
		} else if define.starts_with('${name}=') {
			value = define[name.len + 1..]
		}
	}
	return target[..start] + value + target[default_end + 2..]
}

fn checker_fixture_missing_shader_message(target string, source_file string) ?string {
	if !target.starts_with('"') || !target.ends_with('"') {
		return none
	}
	header := target[1..target.len - 1]
	if !header.ends_with('.h') {
		return none
	}
	shader_name := header[..header.len - 2] + '.glsl'
	shader_path := if os.is_abs_path(shader_name) {
		shader_name
	} else {
		os.join_path(os.dir(source_file), shader_name)
	}
	if !os.is_file(shader_path) {
		return none
	}
	return 'This header can be generated from `${os.file_name(shader_name)}`. Run `v shader .` in that directory to create it.'
}

fn checker_fixture_header_exists(target string, source_file string, c_compiler string) bool {
	if target.starts_with('"') && target.ends_with('"') {
		path := target[1..target.len - 1]
		resolved := if os.is_abs_path(path) {
			path
		} else {
			os.join_path(os.dir(source_file), path)
		}
		return os.is_file(resolved)
	}
	if !target.starts_with('<') || !target.ends_with('>') {
		return true
	}
	header := target[1..target.len - 1]
	probe := os.join_path(os.vtmp_dir(), 'v3_header_probe_${os.getpid()}.c')
	os.write_file(probe, '') or { return true }
	result := cmdexec.run(c_compiler, ['-E', '-x', 'c', '-include', header, probe])
	os.rm(probe) or {}
	return result.exit_code == 0
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
	slash := [u8(92)].bytestr()
	escaped_slash := [u8(92), 92].bytestr()
	quote := [u8(34)].bytestr()
	escaped_quote := [u8(92), 34].bytestr()
	declarations_include := tcc_declarations_path.replace(slash, escaped_slash).replace(quote, escaped_quote)
	body_include := body_path.replace(slash, escaped_slash).replace(quote, escaped_quote)
	return '#define V3CACHE_PROGRAM_UNIT 1\n#include "${declarations_include}"\n#include "${body_include}"\n'
}

fn c_include_path(path string) string {
	slash := [u8(92)].bytestr()
	quote := [u8(34)].bytestr()
	escaped_quote := [u8(92), 34].bytestr()
	return path.replace(slash, '/').replace(quote, escaped_quote)
}

fn v3_incremental_program_main_source(cached_prefix string, body_source string) string {
	return cached_prefix + modulecache.without_duplicate_static_string_definitions(body_source, cached_prefix)
}

fn prepare_v3_incremental_cached_body(body_path string, prefix_path string, tcc_declarations_path string, cached_prefix string, compile_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	objects := cache_object_paths(state.objects)
	if !os.is_file(prefix_path) || !os.is_file(tcc_declarations_path) || objects.len == 0 {
		return error('v3 incremental C declarations are unavailable')
	}
	body_source := os.read_file(body_path)!
	main_source := v3_incremental_program_main_source(cached_prefix, body_source)
	tcc_main_source := v3_incremental_main_source(tcc_declarations_path, body_path)
	return V3PreparedModuleCache{
		main_source: main_source
		tcc_main_source: tcc_main_source
		program_prefix_source: cached_prefix
		objects: objects
	}
}

fn prepare_v3_cached_generic_body(generated_source string, cached_prefix string, cached_declarations string, cached_body string, compile_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if !state.manager.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	incremental_c_function_sections(generated_source) or {
		return error('v3 generic C function sections are unavailable')
	}
	materialized_cached_body := modulecache.materialize_cached_body_string_definitions(cached_body)
	materialized_source := merge_cached_generic_program_body(materialized_cached_body, generated_source) or { return error('v3 generic C body could not be reconstructed') }
	split := modulecache.split_generated_c(materialized_source)!
	main_body := split.modules['main'] or { '' }
	current_string_definitions := modulecache.static_string_definitions(split.prefix)
	combined_declarations := cached_declarations + current_string_definitions
	tcc_declarations := tcc_cached_main_source(combined_declarations, main_body)
	unique_string_definitions := modulecache.without_duplicate_static_string_definitions(current_string_definitions, cached_prefix)
	main_source := cached_prefix + unique_string_definitions + main_body
	tcc_main_source := '#define V3CACHE_PROGRAM_UNIT 1\n' + tcc_declarations + main_body
	return V3PreparedModuleCache{
		main_source: main_source
		tcc_main_source: tcc_main_source
		main_body: main_body
		program_prefix_source: cached_prefix
		program_declarations: combined_declarations
		tcc_program_declarations: tcc_declarations
		objects: cache_object_paths(state.objects)
	}
}

fn prepare_v3_module_cache(generated_source string, cache_used_fns &map[string]bool, program_used_fns &map[string]bool, tc &types.TypeChecker, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, interface_impl_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if !state.manager.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	split := modulecache.split_generated_c(generated_source)!
	mut parsed_modules := state.parsed_from_source.keys()
	parsed_modules.sort()
	mut parsed_short_module_counts := map[string]int{}
	for module_name in parsed_modules {
		short_name := module_name.all_after_last('.')
		parsed_short_module_counts[short_name]++
	}
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
	raw_declarations := if needs_declarations {
		modulecache.declaration_header(split.prefix)
	} else {
		''
	}
	declarations := cache_source_without_cached_native_inputs(raw_declarations, state, false)
	compile_signature := v3_cached_object_wrapper_compile_signature(v3_cached_object_compile_signature(c_standard, opt_flag, pic_flag, warning_flags, generated_c_flags, objective_c, interface_impl_signature), generated_source)
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	main_body := split.modules['main'] or { '' }
	program_specializations := split.modules['__v3_program_specializations'] or { '' }
	program_support := split.modules['__v3_program_support'] or { '' }
	program_generated_support := program_specializations + program_support + main_body
	main_prefix := prune_cache_only_function_prototypes(prune_cached_native_function_prototypes(cache_source_without_cached_native_inputs(split.prefix, state, true), state, [
		'main',
	]), cache_used_fns, program_generated_support, tc, state)
	dylib_prefix := modulecache.prune_unreferenced_static_string_definitions(main_prefix + program_specializations + program_support)
	main_source := '#define V3CACHE_PROGRAM_UNIT 1\n' + main_prefix + program_specializations + program_support + main_body
	main_declarations := prune_cache_only_function_prototypes(cache_source_without_cached_native_inputs(modulecache.declaration_header(split.prefix + program_specializations + program_support), state, false), cache_used_fns, program_generated_support, tc, state)
	tcc_declarations := tcc_cached_main_source(main_declarations, main_body)
	tcc_main := '#define V3CACHE_PROGRAM_UNIT 1\n' + tcc_declarations + main_body
	mut object_paths := state.objects.clone()
	if !state.bundle_valid {
		entry := state.manager.object_entry('builtin', state.bundle_sources, compile_signature)
		bundle_compile_scope := prealloc_scope_begin_for_v3()
		mut bundle_body := strings.new_builder(4096)
		mut split_modules := split.modules.keys()
		split_modules.sort()
		for module_name in split_modules {
			if module_is_builtin_bundle(state, module_name) {
				bundle_body.write_string(split.modules[module_name])
			}
		}
		bundle_roots := cache_builtin_bundle_roots(state)
		bundle_declarations := prune_cached_native_function_prototypes(raw_declarations, state, bundle_roots)
		bundle_native := cache_source_with_cached_native_inputs(bundle_declarations, state, bundle_roots)
		module_source := if bundle_native.has_native {
			'#define V3CACHE_PROGRAM_UNIT 1\n' + bundle_native.source + '#undef V3CACHE_PROGRAM_UNIT\n' + bundle_native.remaining_includes + bundle_body.str()
		} else {
			declarations + bundle_body.str()
		}
		compile_v3_cached_object(entry, module_source, c_standard, opt_flag, pic_flag, warning_flags, generated_c_flags, objective_c) or {
			prealloc_scope_leave_for_v3(bundle_compile_scope)
			message := err.msg().clone()
			prealloc_scope_free_for_v3(bundle_compile_scope)
			return error(message)
		}
		unsafe { bundle_body.free() }
		prealloc_scope_leave_for_v3(bundle_compile_scope)
		prealloc_scope_free_for_v3(bundle_compile_scope)
		for module_name, header in state.headers {
			if !module_is_builtin_bundle(state, module_name) {
				continue
			}
			if source_files := state.module_sources[module_name] {
				state.manager.write_header(module_name, source_files, header)!
			}
		}
		bundle_dependencies := cache_object_dependency_signatures(state, cache_builtin_bundle_roots(state))
		state.manager.write_stamp('builtin', state.bundle_sources, bundle_dependencies, compile_signature)!
		object_paths['builtin'] = entry.object
		state.bundle_valid = true
		for module_name in state.headers.keys() {
			if module_is_builtin_bundle(state, module_name) {
				newly_cached_modules[module_name] = true
			}
		}
	}

	for module_name in parsed_modules {
		if module_is_builtin_bundle(state, module_name) {
			continue
		}
		source_files := state.module_sources[module_name] or { continue }
		entry := state.manager.object_entry(module_name, source_files, compile_signature)
		body := split.modules[module_name] or {
			short_name := module_name.all_after_last('.')
			// A short split marker is a compatibility fallback for an unqualified
			// module identity. Never reuse it for two dotted modules with the same
			// leaf name, or both cache objects receive the same function definitions.
			if parsed_short_module_counts[short_name] == 1 {
				split.modules[short_name] or { '' }
			} else {
				''
			}
		}
		module_compile_scope := prealloc_scope_begin_for_v3()
		module_declarations := prune_cached_native_function_prototypes(raw_declarations, state, [
			module_name,
		])
		native := cache_source_with_cached_native_inputs(module_declarations, state, [
			module_name,
		])
		module_source := if native.has_native {
			'#define V3CACHE_PROGRAM_UNIT 1\n' + native.source + '#undef V3CACHE_PROGRAM_UNIT\n' + native.remaining_includes + body
		} else {
			declarations + body
		}
		compile_v3_cached_object(entry, module_source, c_standard, opt_flag, pic_flag, warning_flags, generated_c_flags, objective_c) or {
			prealloc_scope_leave_for_v3(module_compile_scope)
			message := err.msg().clone()
			prealloc_scope_free_for_v3(module_compile_scope)
			return error(message)
		}
		prealloc_scope_leave_for_v3(module_compile_scope)
		prealloc_scope_free_for_v3(module_compile_scope)
		if header := state.headers[module_name] {
			state.manager.write_header(module_name, source_files, header)!
		}
		dependencies := cache_object_dependency_signatures(state, [module_name])
		state.manager.write_stamp(module_name, source_files, dependencies, compile_signature)!
		object_paths[module_name] = entry.object
		newly_cached_modules[module_name] = true
	}

	return V3PreparedModuleCache{
		main_source: main_source
		tcc_main_source: tcc_main
		main_body: main_body
		program_body_cache: incremental_static_string_markers(split.prefix) + '/* V3CACHE_BODY_BEGIN */\n/* V3CACHE_MODULE main */\n' + main_body + '\n/* V3CACHE_BODY_END */\n'
		program_prefix_source: '#define V3CACHE_PROGRAM_UNIT 1\n' + dylib_prefix
		program_declarations: main_declarations
		tcc_program_declarations: tcc_declarations
		objects: cache_used_object_paths(object_paths, program_used_fns, program_generated_support, tc, state)
		newly_cached_modules: newly_cached_modules.len
	}
}

fn cache_used_object_paths(object_paths map[string]string, program_used_fns &map[string]bool, program_generated_support string, tc &types.TypeChecker, state &V3ModuleCacheState) []string {
	mut selected := map[string]string{}
	mut runtime_roots := []string{}
	for module_name, object_path in object_paths {
		if module_is_builtin_bundle(state, module_name) {
			selected[module_name] = object_path
			continue
		}
		if cache_module_has_used_runtime(module_name, program_used_fns, program_generated_support, tc) {
			selected[module_name] = object_path
			runtime_roots << module_name
		}
	}
	for module_name in cache_dependency_modules(state, runtime_roots) {
		if object_path := object_paths[module_name] {
			selected[module_name] = object_path
		}
	}
	return cache_object_paths(selected)
}

fn cache_module_has_used_runtime(module_name string, program_used_fns &map[string]bool, program_generated_support string, tc &types.TypeChecker) bool {
	module_prefix := '${module_name}.'
	c_prefix := '${naming.c_name(module_name)}__'
	if program_generated_support.contains(c_prefix) {
		return true
	}
	for name, is_used in program_used_fns {
		if !is_used {
			continue
		}
		owner_module := tc.fn_type_modules[name] or { '' }
		if name.starts_with(module_prefix) || name.starts_with(c_prefix)
			|| owner_module == module_name {
			return true
		}
	}
	return false
}

fn prune_cache_only_function_prototypes(source string, cache_used_fns &map[string]bool, program_generated_support string, tc &types.TypeChecker, state &V3ModuleCacheState) string {
	if source.len == 0 || cache_used_fns.len == 0 {
		return source
	}
	vlib_paths := cache_vlib_source_and_header_paths(state)
	mut interface_dispatch_methods := map[string]bool{}
	for key in tc.interface_concrete_method_keys() {
		interface_dispatch_methods[restored_fn_c_name(key)] = true
	}
	mut candidate_functions := map[string]bool{}
	for name, is_used in cache_used_fns {
		if !is_used || name.contains('__') || !name.contains('.') {
			continue
		}
		source_file := tc.fn_type_files[name] or { '' }
		if cache_path_is_vlib(source_file) || vlib_paths[source_file]
			|| vlib_paths[os.real_path(source_file)] {
			continue
		}
		c_name := restored_fn_c_name(name)
		if c_name.contains('_T_') || interface_dispatch_methods[c_name] {
			continue
		}
		candidate_functions[c_name] = true
	}
	if candidate_functions.len == 0 {
		return source
	}
	source_references := cache_function_reference_counts(source, candidate_functions)
	program_references := cache_function_reference_counts(program_generated_support, candidate_functions)
	mut cache_only_functions := map[string]bool{}
	for c_name in candidate_functions.keys() {
		// A cached module can expose a function value through a global initializer
		// in the program prefix. Keep its prototype even when no generated program
		// function calls it directly.
		if program_references[c_name] > 0 || source_references[c_name] > 1 {
			continue
		}
		cache_only_functions[c_name] = true
	}
	if cache_only_functions.len == 0 {
		return source
	}
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.ends_with(');') {
			open := trimmed.index_u8(`(`)
			if open > 0 {
				fn_name := trimmed[..open].all_after_last(' ').trim_left('*')
				if cache_only_functions[fn_name] {
					continue
				}
			}
		}
		out.writeln(line)
	}
	return out.str()
}

fn cache_function_reference_counts(source string, candidates map[string]bool) map[string]u8 {
	mut counts := map[string]u8{}
	mut start := -1
	for i := 0; i <= source.len; i++ {
		is_identifier := i < source.len && (source[i].is_alnum() || source[i] == `_`)
		if is_identifier {
			if start < 0 {
				start = i
			}
			continue
		}
		if start < 0 {
			continue
		}
		token := unsafe { source[start..i] }
		if candidates[token] && counts[token] < 2 {
			counts[token]++
		}
		start = -1
	}
	return counts
}

fn cache_vlib_source_and_header_paths(state &V3ModuleCacheState) map[string]bool {
	mut paths := map[string]bool{}
	for module_name, source_files in state.module_sources {
		if !source_files.any(cache_path_is_vlib(it)) {
			continue
		}
		for source_file in source_files {
			paths[source_file] = true
			paths[os.real_path(source_file)] = true
		}
		header := state.manager.entry(module_name, source_files).header
		paths[header] = true
		paths[os.real_path(header)] = true
	}
	return paths
}

fn cache_path_is_vlib(path string) bool {
	return path.contains('/vlib/') || path.contains('\\vlib\\')
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

fn prune_cached_native_function_prototypes(source string, state &V3ModuleCacheState, module_names []string) string {
	mut declared_functions := map[string]bool{}
	mut selected_modules := map[string]bool{}
	for module_name in module_names {
		selected_modules[module_name] = true
	}
	mut declaration_modules := selected_modules.clone()
	for raw_module_name, roots in state.module_native_roots {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if roots.any(selected_modules[state.native_root_owners[os.real_path(it)] or { module_name }]) {
			declaration_modules[module_name] = true
		}
	}
	for module_name in declaration_modules.keys() {
		for name, declared in state.native_declared_functions[module_name] {
			if declared {
				declared_functions[name] = true
			}
		}
	}
	lines := source.split_into_lines()
	mut out := strings.new_builder(source.len)
	mut i := 0
	for i < lines.len {
		line := lines[i]
		if line.trim_space() == '#ifndef V3CACHE_PROGRAM_UNIT' && i + 2 < lines.len
			&& lines[i + 2].trim_space() == '#endif' {
			name := cached_native_function_prototype_name(lines[i + 1])
			if name.len > 0 {
				if !declared_functions[name] {
					out.writeln(lines[i + 1])
				}
				i += 3
				continue
			}
		}
		name := cached_native_function_prototype_name(line)
		if name.len == 0 || !declared_functions[name] {
			out.writeln(line)
		}
		i++
	}
	return out.str()
}

fn cached_native_function_prototype_name(line string) string {
	clean := line.trim_space()
	if !clean.ends_with(';') || !clean.contains('(') || clean.starts_with('#') {
		return ''
	}
	paren := clean.index_u8(`(`)
	mut end := paren
	for end > 0 && clean[end - 1].is_space() {
		end--
	}
	mut start := end
	for start > 0 && (clean[start - 1].is_alnum() || clean[start - 1] == `_`) {
		start--
	}
	if start >= end {
		return ''
	}
	prefix := clean[..start].trim_space()
	if prefix.len == 0 || prefix.contains('=') || prefix.starts_with('return ')
		|| prefix.starts_with('if ') || prefix.starts_with('for ') || prefix.starts_with('while ')
		|| prefix.starts_with('switch ') {
		return ''
	}
	return clean[start..end]
}

fn cache_source_without_cached_native_inputs(source string, state &V3ModuleCacheState, keep_main bool) string {
	mut excluded := map[string]string{}
	for raw_module_name, paths in state.module_native_roots {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if (keep_main && module_name == 'main') || !state.native_source_modules[module_name] {
			continue
		}
		for path in paths {
			clean := os.real_path(path).replace('\\', '/').replace('"', '\\"')
			excluded['#include "${clean}"'] = state.native_type_declarations[os.real_path(path)] or {
				''
			}
		}
	}
	if excluded.len == 0 {
		return source
	}
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		clean := line.trim_space()
		if replacement := excluded[clean] {
			if replacement.len > 0 {
				out.writeln(replacement)
			}
		} else {
			out.writeln(line)
		}
	}
	return out.str()
}

fn cache_scoped_native_input_paths(state &V3ModuleCacheState) []string {
	mut seen := map[string]bool{}
	mut paths := []string{}
	for raw_module_name, roots in state.module_native_roots {
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if !state.native_source_modules[module_name] {
			continue
		}
		for root in roots {
			real_path := os.real_path(root)
			if !seen[real_path] {
				seen[real_path] = true
				paths << real_path
			}
		}
	}
	paths.sort()
	return paths
}

struct V3CachedNativeSource {
	source             string
	remaining_includes string
	has_native         bool
}

fn cache_source_with_cached_native_inputs(source string, state &V3ModuleCacheState, module_names []string) V3CachedNativeSource {
	mut selected := map[string]bool{}
	for module_name in module_names {
		selected[module_name] = true
	}
	mut all_include_lines := map[string]bool{}
	mut include_paths := map[string]string{}
	mut selected_include_lines := map[string]bool{}
	mut selected_order := []string{}
	mut raw_module_names := state.module_native_roots.keys()
	raw_module_names.sort()
	for raw_module_name in raw_module_names {
		roots := state.module_native_roots[raw_module_name]
		module_name := if raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, raw_module_name) or { continue }
		}
		if !state.native_source_modules[module_name] {
			continue
		}
		for root in roots {
			clean := c_include_path(os.real_path(root))
			include_line := '#include "${clean}"'
			all_include_lines[include_line] = true
			include_paths[include_line] = os.real_path(root)
			owner_module := state.native_root_owners[os.real_path(root)] or { module_name }
			if selected[owner_module] && !selected_include_lines[include_line] {
				selected_include_lines[include_line] = true
				selected_order << include_line
			}
		}
	}
	if selected_include_lines.len == 0 {
		return V3CachedNativeSource{
			source: cache_source_without_cached_native_inputs(source, state, false)
		}
	}
	mut found := map[string]bool{}
	mut out := strings.new_builder(source.len + selected_include_lines.len * 64)
	mut pending_lines := []string{}
	for line in source.split_into_lines() {
		clean := line.trim_space()
		if clean !in all_include_lines {
			pending_lines << line
			continue
		}
		is_selected := selected_include_lines[clean]
		cache_write_native_declaration_segment(pending_lines, is_selected, mut out)
		pending_lines.clear()
		if selected_include_lines[clean] {
			// The compiler-only macro suppresses fallback declarations, but must not
			// leak into native source that did not see it in the uncached unit.
			out.writeln('#undef V3CACHE_PROGRAM_UNIT')
			out.writeln(line)
			out.writeln('#define V3CACHE_PROGRAM_UNIT 1')
			found[clean] = true
		} else if all_include_lines[clean] {
			path := include_paths[clean] or { '' }
			if declarations := state.native_type_declarations[path] {
				out.writeln(declarations)
			}
		}
	}
	cache_write_native_declaration_segment(pending_lines, false, mut out)
	mut remaining := strings.new_builder(selected_order.len * 96)
	for include_line in selected_order {
		if !found[include_line] {
			path := include_paths[include_line] or { '' }
			for directive in state.native_root_contexts[path] or { []string{} } {
				remaining.writeln(directive)
			}
			remaining.writeln(include_line)
		}
	}
	return V3CachedNativeSource{
		source: out.str()
		remaining_includes: remaining.str()
		has_native: true
	}
}

fn cache_write_native_declaration_segment(lines []string, restore_implementation_macros bool, mut out strings.Builder) {
	marker := '/* v3 cache omitted '
	for line in lines {
		clean := line.trim_space()
		if restore_implementation_macros && clean.starts_with(marker) && clean.ends_with(' */') {
			name := clean[marker.len..clean.len - 3].trim_space()
			if cache_native_implementation_macro(name) {
				out.writeln('#define ${name}')
				continue
			}
		}
		out.writeln(line)
	}
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
	implicit_modules := ['sync', 'v.embed_file', 'builtin.closure']
	for implicit_name in implicit_modules {
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
	for implicit_name in implicit_modules {
		if implicit_module := cache_state_module_name(state, implicit_name) {
			external_input_modules[implicit_module] = true
		}
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

fn trace_v3_cache_fallback(reason string) {
	if os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  V3 module cache fallback: reason=${reason}')
	}
}

fn restart_v3_without_cache() {
	restart_v3_with_args(['-nocache'])
}

fn restart_v3_with_args(extra_args []string) {
	executable := os.executable()
	mut args := extra_args.clone()
	args << os.args[1..]
	os.setenv(v3_internal_restart_env, '1', true)
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

fn cache_external_input_owner_modules(state &V3ModuleCacheState, a &flat.FlatAst, unscoped_inputs map[string][]string, static_inputs map[string][]string, user_files []string, c_flags []string, ccompiler string, target pref.Target) (map[string]bool, bool) {
	mut modules := map[string]bool{}
	mut static_input_owners := map[string][]string{}
	for raw_module_name, paths in unscoped_inputs {
		for path in paths {
			if path in (static_inputs[raw_module_name] or { []string{} }) {
				mut owners := static_input_owners[path]
				if raw_module_name !in owners {
					owners << raw_module_name
					static_input_owners[path] = owners
				}
			}
		}
	}
	for raw_module_name, _ in state.module_external_inputs {
		roots := state.module_native_roots[raw_module_name] or { []string{} }
		has_static_storage := (static_inputs[raw_module_name] or { []string{} }).len > 0
		if has_static_storage {
			if raw_module_name == 'main' {
				if roots.len > 0 {
					modules['main'] = true
				}
			} else {
				for path in static_inputs[raw_module_name] {
					owners := static_input_owners[path] or { []string{} }
					if owners.len != 1 {
						if os.getenv('V3_CACHE_TRACE') != '' {
							eprintln('  V3 module cache multiply-owned static input: module=${raw_module_name} owners=${owners} path=${path}')
						}
						return modules, false
					}
					if !cache_static_input_is_private_to_module(a, state, raw_module_name, path, user_files, c_flags, ccompiler, target) {
						if os.getenv('V3_CACHE_TRACE') != '' {
							eprintln('  V3 module cache shared static input: module=${raw_module_name} path=${path}')
						}
						return modules, false
					}
				}
			}
		}
		if roots.len == 0 {
			continue
		}
		for root in roots {
			if !c_flag_is_c_source_file(root) && root !in (unscoped_inputs[raw_module_name] or {
				[]string{}
			}) {
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache unsupported native source root: module=${raw_module_name} path=${root}')
				}
				return modules, false
			}
		}
		if raw_module_name == 'main' {
			modules['main'] = true
			continue
		}
		module_name := cache_state_module_name(state, raw_module_name) or { return modules, false }
		modules[module_name] = true
	}
	return modules, true
}

fn cache_static_input_is_private_to_module(a &flat.FlatAst, state &V3ModuleCacheState, raw_module_name string, path string, user_files []string, c_flags []string, ccompiler string, target pref.Target) bool {
	source := os.read_file(path) or { return false }
	file_scope_identifiers := c_source_file_scope_identifiers(source)
	mut header_identifiers, mut function_identifiers_complete :=
		modulecache.c_source_static_function_identifiers_with_status(source)
	mut static_identifiers, mut static_identifiers_complete :=
		modulecache.c_source_static_variable_identifiers(source)
	if !function_identifiers_complete || !static_identifiers_complete {
		preprocessed := cache_preprocessed_native_input(path, state.native_root_contexts[os.real_path(path)] or {
			[]string{}
		}, c_flags, ccompiler, target) or { return false }
		if !function_identifiers_complete {
			header_identifiers, function_identifiers_complete =
				modulecache.c_source_static_function_identifiers_with_status(preprocessed)
		}
		if !static_identifiers_complete {
			static_identifiers, static_identifiers_complete =
				modulecache.c_source_static_variable_identifiers(preprocessed)
		}
		if !function_identifiers_complete || !static_identifiers_complete {
			if os.getenv('V3_CACHE_TRACE') != '' {
				eprintln('  V3 module cache incomplete preprocessed static identifier scan: functions=${function_identifiers_complete} variables=${static_identifiers_complete} path=${path}')
			}
			return false
		}
		for identifier in header_identifiers.keys() {
			if !file_scope_identifiers[identifier] {
				header_identifiers.delete(identifier)
			}
		}
		for identifier in static_identifiers.keys() {
			if !file_scope_identifiers[identifier] {
				static_identifiers.delete(identifier)
			}
		}
	}
	for identifier, present in static_identifiers {
		if present {
			header_identifiers[identifier] = true
		}
	}
	if header_identifiers.len == 0 && os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  V3 module cache static input has no recoverable identifiers: path=${path}')
	}
	return cache_external_identifiers_are_private_to_module(a, state, raw_module_name, header_identifiers, user_files, os.real_path(path))
}

fn cache_preprocessed_native_input(path string, context []string, c_flags []string, ccompiler string, target pref.Target) ?string {
	wrapper := os.join_path(os.vtmp_dir(), 'v3_native_scan_${tempname.unique_token()}.c')
	defer {
		os.rm(wrapper) or {}
	}
	mut source := strings.new_builder(context.len * 48 + path.len + 32)
	for directive in context {
		source.writeln(directive)
	}
	source.writeln('#include "${c_include_path(os.real_path(path))}"')
	os.write_file(wrapper, source.str()) or { return none }
	unsafe { source.free() }
	mut args := c_compiler_target_args(target, false) or { return none }
	args << c_object_compile_flags(c_flags)
	mut language := cgen.cache_native_input_language(path, c_flags, false, target)
	if c_flags_need_objective_c(c_flags) && language !in ['objective-c', 'objective-c++'] {
		language = if language == 'c++' { 'objective-c++' } else { 'objective-c' }
	}
	args << ['-E', '-P', '-x', language, wrapper]
	result := cmdexec.run(ccompiler, args)
	if result.exit_code != 0 {
		if os.getenv('V3_CACHE_TRACE') != '' {
			eprintln('  V3 module cache native privacy preprocessing failed: path=${path}')
		}
		return none
	}
	return result.output
}

fn cache_external_identifiers_are_private_to_module(a &flat.FlatAst, state &V3ModuleCacheState, raw_module_name string, identifiers map[string]bool, user_files []string, ignored_external_path string) bool {
	if identifiers.len == 0 {
		return false
	}
	owner_module := if raw_module_name == 'main' {
		'main'
	} else {
		cache_state_module_name(state, raw_module_name) or { return false }
	}
	mut exposed_identifiers := identifiers.clone()
	owner_paths := state.module_external_inputs[raw_module_name] or { []string{} }
	mut owner_sources := []string{cap: owner_paths.len}
	for path in owner_paths {
		owner_sources << os.read_file(path) or { continue }
	}
	for identifier, present in modulecache.c_sources_macro_identifiers_referencing(owner_sources, exposed_identifiers) {
		if present {
			exposed_identifiers[identifier] = true
		}
	}
	mut scanned_paths := map[string]bool{}
	for sibling_raw_module_name, paths in state.module_external_inputs {
		sibling_module := if sibling_raw_module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, sibling_raw_module_name) or { sibling_raw_module_name }
		}
		if sibling_module == owner_module {
			continue
		}
		for path in paths {
			if ignored_external_path.len > 0 && os.real_path(path) == ignored_external_path {
				continue
			}
			source := os.read_file(path) or { continue }
			if identifier := c_source_referenced_identifier(source, exposed_identifiers) {
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache static identifier referenced by sibling C input: owner=${owner_module} sibling=${sibling_module} path=${path} identifier=${identifier}')
				}
				return false
			}
		}
	}
	if owner_module != 'main' {
		for path in user_files {
			real_path := os.real_path(path)
			scanned_paths[real_path] = true
			file_source := os.read_file(real_path) or { continue }
			for identifier in v_c_identifiers(file_source) {
				if exposed_identifiers[identifier] {
					if os.getenv('V3_CACHE_TRACE') != '' {
						eprintln('  V3 module cache static identifier referenced by main V input: owner=${owner_module} path=${path} identifier=${identifier}')
					}
					return false
				}
			}
		}
	}
	for module_name, source_files in state.module_sources {
		canonical_module := cache_state_module_name(state, module_name) or { module_name }
		if canonical_module == owner_module {
			continue
		}
		for path in source_files {
			real_path := os.real_path(path)
			scanned_paths[real_path] = true
			file_source := os.read_file(real_path) or { continue }
			for identifier in v_c_identifiers(file_source) {
				if exposed_identifiers[identifier] {
					if os.getenv('V3_CACHE_TRACE') != '' {
						eprintln('  V3 module cache static identifier referenced by sibling V input: owner=${owner_module} sibling=${canonical_module} path=${path} identifier=${identifier}')
					}
					return false
				}
			}
		}
	}
	mut idx := 1
	for idx < a.file_node_ids.len {
		file_node_idx := a.file_node_ids[idx]
		idx += 2
		if file_node_idx < 0 || file_node_idx >= a.nodes.len {
			continue
		}
		file_node := a.nodes[file_node_idx]
		mut module_name := 'main'
		for child_id in a.children_of(&file_node) {
			child := a.node(child_id)
			if child.kind == .module_decl {
				module_name = child.value
				break
			}
		}
		canonical_module := if module_name == 'main' {
			'main'
		} else {
			cache_state_module_name(state, module_name) or { module_name }
		}
		if canonical_module == owner_module || !os.is_file(file_node.value) {
			continue
		}
		real_path := os.real_path(file_node.value)
		if scanned_paths[real_path] {
			continue
		}
		file_source := os.read_file(real_path) or { continue }
		for identifier in v_c_identifiers(file_source) {
			if exposed_identifiers[identifier] {
				if os.getenv('V3_CACHE_TRACE') != '' {
					eprintln('  V3 module cache static identifier referenced by parsed sibling V input: owner=${owner_module} sibling=${canonical_module} path=${real_path} identifier=${identifier}')
				}
				return false
			}
		}
	}
	return true
}

fn c_source_references_identifiers(source string, identifiers map[string]bool) bool {
	if _ := c_source_referenced_identifier(source, identifiers) {
		return true
	}
	return false
}

fn c_source_file_scope_identifiers(source string) map[string]bool {
	mut identifiers := map[string]bool{}
	mut brace_depth := 0
	mut line_start := 0
	mut i := 0
	for i < source.len {
		if source[i] == `\n` {
			i++
			line_start = i
			continue
		}
		if source[i] == `#` && source[line_start..i].trim_space().len == 0 {
			for i < source.len {
				newline := source.index_after('\n', i) or { return identifiers }
				mut end := newline - 1
				for end > i && source[end - 1] in [` `, `\t`, `\r`] {
					end--
				}
				i = newline
				line_start = i
				if end == 0 || source[end - 1] != `\\` {
					break
				}
			}
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
			i += 2
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				if source[i] == `\n` {
					line_start = i + 1
				}
				i++
			}
			i = int_min(i + 2, source.len)
			continue
		}
		if source[i] in [`"`, `'`] {
			quote := source[i]
			i++
			for i < source.len {
				if source[i] == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				i++
				if source[i - 1] == quote {
					break
				}
			}
			continue
		}
		if source[i] == `{` {
			brace_depth++
			i++
			continue
		}
		if source[i] == `}` {
			brace_depth = int_max(brace_depth - 1, 0)
			i++
			continue
		}
		if !source[i].is_letter() && source[i] != `_` {
			i++
			continue
		}
		start := i
		i++
		for i < source.len && (source[i].is_alnum() || source[i] == `_`) {
			i++
		}
		if brace_depth == 0 && (start == 0 || source[start - 1] != `@`) {
			identifiers[source[start..i]] = true
		}
	}
	return identifiers
}

fn c_source_referenced_identifier(source string, identifiers map[string]bool) ?string {
	mut i := 0
	for i < source.len {
		if source[i] in [`"`, `'`] {
			quote := source[i]
			i++
			for i < source.len {
				if source[i] == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				i++
				if source[i - 1] == quote {
					break
				}
			}
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
			i += 2
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			i = int_min(i + 2, source.len)
			continue
		}
		if !source[i].is_letter() && source[i] != `_` {
			i++
			continue
		}
		start := i
		i++
		for i < source.len && (source[i].is_alnum() || source[i] == `_`) {
			i++
		}
		identifier := source[start..i]
		if identifiers[identifier] {
			return identifier
		}
	}
	return none
}

fn v_c_identifiers(source string) []string {
	mut identifiers := []string{}
	mut i := 0
	for i < source.len {
		i = v_skip_space_and_comments(source, i)
		if i >= source.len {
			break
		}
		if !source[i].is_letter() && source[i] != `_` {
			i++
			continue
		}
		token_start := i
		i++
		for i < source.len && (source[i].is_alnum() || source[i] == `_`) {
			i++
		}
		if source[token_start..i] != 'C' {
			continue
		}
		mut selector_pos := v_skip_space_and_comments(source, i)
		if selector_pos >= source.len || source[selector_pos] != `.` {
			continue
		}
		selector_pos = v_skip_space_and_comments(source, selector_pos + 1)
		if selector_pos >= source.len
			|| (!source[selector_pos].is_letter() && source[selector_pos] != `_`) {
			continue
		}
		i = selector_pos + 1
		for i < source.len && (source[i].is_alnum() || source[i] == `_`) {
			i++
		}
		identifiers << source[selector_pos..i]
	}
	return identifiers
}

fn v_skip_space_and_comments(source string, start int) int {
	mut i := start
	for i < source.len {
		for i < source.len && source[i].is_space() {
			i++
		}
		if i + 1 >= source.len || source[i] != `/` {
			break
		}
		if source[i + 1] == `/` {
			i += 2
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		}
		if source[i + 1] == `*` {
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			i = int_min(i + 2, source.len)
			continue
		}
		break
	}
	return i
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

fn v3_cached_object_wrapper_compile_signature(base string, generated_source string) string {
	start_marker := '/* V3CACHE_PROGRAM_WRAPPERS */'
	end_marker := '/* V3CACHE_PROGRAM_WRAPPERS_END */'
	first_start := generated_source.index(start_marker) or { return base }
	mut sections := strings.new_builder(1024)
	mut pos := first_start
	for {
		start := generated_source.index_after(start_marker, pos) or { break }
		end := generated_source.index_after(end_marker, start + start_marker.len) or {
			// Fail closed for cache-marked C emitted by an older compiler or a
			// truncated section: its complete prefix is the only safe identity.
			prefix := generated_source.all_before('/* V3CACHE_BODY_BEGIN */')
			return '${base}\nprogram_wrappers=${sha256.hexhash(prefix)}'
		}
		section_end := end + end_marker.len
		sections.write_string(generated_source[start..section_end])
		pos = section_end
	}
	wrapper_source := sections.str()
	if wrapper_source.len == 0 {
		return base
	}
	// Static callback/thread/method-value wrappers are emitted in the shared C
	// prefix and therefore become part of every cached module object. Their set is
	// specific to the entry program. Hash just the delimited wrapper sections so
	// native-input pruning cannot make the cold and prepared-prefix identities
	// disagree while leaving wrapper-free programs on the cross-project key.
	return '${base}\nprogram_wrappers=${sha256.hexhash(wrapper_source)}'
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
		if entry := state.manager.valid_object_for_compile_signature(object_name, source_files, compile_signature, dependency_inputs) {
			state.objects[object_name] = entry.object
		} else {
			if os.getenv('V3_CACHE_TRACE') != '' {
				eprintln('  V3 module cache object miss: module=${object_name}')
			}
			return true
		}
	}
	return false
}

fn compile_v3_cached_object(entry modulecache.Entry, source string, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool) ! {
	unique := tempname.unique_token()
	// GCC records the input basename in otherwise-identical object files. Put the
	// stable cache basename in a unique directory so recompiles remain byte-for-byte
	// reproducible without sacrificing concurrent-writer isolation.
	tmp_dir := '${entry.c_source}.tmp.${unique}'
	os.mkdir_all(tmp_dir)!
	tmp_source := os.join_path(tmp_dir, os.file_name(entry.c_source))
	defer {
		if os.getenv('V3_CACHE_TRACE') == '' {
			os.rmdir_all(tmp_dir) or {}
		} else if !os.exists(tmp_source) {
			os.rmdir_all(tmp_dir) or {}
		}
	}
	os.write_file(tmp_source, source)!
	mut flags := c_object_compile_flags(generated_c_flags)
	flags = flags.filter(!c_flag_is_object_file(it))
	tmp_object := '${entry.object}.tmp.${unique}'
	mut args := []string{}
	if objective_c {
		args << ['-x', 'objective-c']
	}
	append_v3_c_compile_mode_flags(mut args, c_standard, opt_flag, pic_flag)
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
	if os.read_file(vmod_path)!.trim_space().len == 0 {
		return []string{}
	}
	// An invalid v.mod does not make the source directory invalid. This matches
	// the legacy builder, while still honoring `subdirs` in valid manifests.
	manifest := vmod.from_file(vmod_path) or { return []string{} }
	return manifest.unknown['subdirs'] or { []string{} }
}

fn v3_directory_user_files(dir string, prefs &pref.Preferences, is_test_command bool, recursive bool) ![]string {
	source_dir := v3_directory_source_root(dir)
	mut files := []string{}
	mut seen_files := map[string]bool{}
	mut seen_dirs := map[string]bool{}
	if recursive {
		collect_v3_directory_user_files_rec(source_dir, source_dir, prefs, is_test_command, mut seen_dirs, mut seen_files, mut files)
		return files
	}
	append_v3_directory_user_files(source_dir, prefs, is_test_command, mut seen_files, mut files)
	for subdir in vmod_subdirs(dir)! {
		collect_v3_directory_user_files_rec(source_dir, os.join_path_single(source_dir, subdir), prefs, is_test_command, mut seen_dirs, mut seen_files, mut files)
	}
	return files
}

fn collect_v3_directory_user_files_rec(module_root string, dir string, prefs &pref.Preferences, is_test_command bool, mut seen_dirs map[string]bool, mut seen_files map[string]bool, mut files []string) {
	if !os.is_dir(dir) {
		return
	}
	real_dir := os.real_path(dir)
	if seen_dirs[real_dir] {
		return
	}
	seen_dirs[real_dir] = true
	if real_dir != os.real_path(module_root) && os.is_file(os.join_path_single(real_dir, 'v.mod')) {
		return
	}
	append_v3_directory_user_files(real_dir, prefs, is_test_command, mut seen_files, mut files)
	mut entries := os.ls(real_dir) or { return }
	entries.sort()
	for entry in entries {
		entry_path := os.join_path_single(real_dir, entry)
		if os.is_dir(entry_path) {
			collect_v3_directory_user_files_rec(module_root, entry_path, prefs, is_test_command, mut seen_dirs, mut seen_files, mut files)
		}
	}
}

fn append_v3_directory_user_files(dir string, prefs &pref.Preferences, is_test_command bool, mut seen map[string]bool, mut files []string) {
	for file in pref.get_v_files_from_dir_for_target(dir, prefs.user_defines, prefs.target) {
		append_unique_file(mut files, mut seen, file)
	}
	if is_test_command {
		for file in pref.get_test_v_files_from_dir_for_target(dir, prefs.user_defines, prefs.backend, prefs.target) {
			append_unique_file(mut files, mut seen, file)
		}
	}
}

fn v3_directory_source_root(dir string) string {
	vmod_root := os.real_path(dir)
	vmod_path := os.join_path_single(vmod_root, 'v.mod')
	if !os.is_file(vmod_path) {
		return dir
	}
	manifest := vmod.from_file(vmod_path) or { return dir }
	source_root := manifest.source_root(vmod_root)
	if os.is_dir(source_root) {
		return source_root
	}
	return dir
}

fn report_v3_removed_src_layout(dir string) bool {
	src_dir := os.join_path(dir, 'src')
	if !os.is_dir(src_dir) {
		return false
	}
	src_files := os.ls(src_dir) or { return false }
	if !src_files.any(it.ends_with('.v')) {
		return false
	}
	eprintln('builder error: the virtual `src/` module directory is no longer supported.
V found .v source files under ${src_dir}, but will not treat `src/` as a virtual module root anymore.
Please move the sources up from `src/` into ${dir}:
	mv ${src_dir}/*.v ${dir}/
	rmdir ${src_dir}

If you want to split one module across subdirectories after moving the root files, add `subdirs` to v.mod, for example:
	subdirs: [\'admin\', \'repo\', \'commit\', \'ci\', \'security\', \'ssh\', \'user\']')
	return true
}

fn expand_single_test_file_inputs(user_files []string, prefs &pref.Preferences) []string {
	mut expanded := []string{}
	mut seen := map[string]bool{}
	for file in user_files {
		if pref.is_test_file_for_backend(file, prefs.backend) {
			module_name := declared_module_in_file(file)
			if module_name != 'builtin' {
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
	all_files := pref.get_v_files_from_dir_for_target(dir, prefs.user_defines, prefs.target)
	mut files := []string{}
	mut imported_modules := map[string]bool{}
	if module_name.len > 0 {
		for file in all_files {
			declared_module := declared_module_in_file(file)
			if declared_module != module_name {
				continue
			}
			files << file
			for imported in declared_imports_in_file(file) {
				imported_modules[imported] = true
			}
		}
	} else {
		// A module-less test is a standalone `main` file. Do not pull every other
		// module-less tool in its directory into the same program.
		for imported in declared_imports_in_file(test_file) {
			imported_modules[imported] = true
		}
	}
	// V permits a small project fixture to put an imported module beside its
	// main/test files. Include those directly imported, differently-declared
	// files in the initial parse when normal module lookup cannot find them.
	// Resolvable same-directory modules (notably tests under vlib/os that import
	// os) must remain imports, or their sources are parsed and emitted twice.
	real_dir := os.real_path(dir)
	mut normally_resolved_here := map[string]bool{}
	for imported in imported_modules.keys() {
		resolved := prefs.get_module_path(imported, test_file)
		if resolved.len > 0 && os.is_dir(resolved) && os.real_path(resolved) == real_dir {
			normally_resolved_here[imported] = true
		}
	}
	for file in all_files {
		if file in files {
			continue
		}
		declared_module := declared_module_in_file(file)
		if imported_modules[declared_module] && !normally_resolved_here[declared_module] {
			files << file
		}
	}
	return files
}

fn declared_imports_in_file(path string) []string {
	source := os.read_file(path) or { return []string{} }
	mut imports := []string{}
	mut in_group := false
	for raw_line in source.split_into_lines() {
		mut line := raw_line.trim_space()
		if comment := line.index('//') {
			line = line[..comment].trim_space()
		}
		if line.len == 0 {
			continue
		}
		if in_group {
			if line.starts_with(')') {
				in_group = false
				continue
			}
			append_declared_import(mut imports, line)
			continue
		}
		if !line.starts_with('import ') {
			continue
		}
		line = line[7..].trim_space()
		if line.starts_with('(') {
			in_group = true
			line = line[1..].trim_space()
			if line.len == 0 {
				continue
			}
		}
		append_declared_import(mut imports, line)
	}
	return imports
}

fn append_declared_import(mut imports []string, line string) {
	mut end := 0
	for end < line.len && line[end] !in [` `, `\t`, `{`, `,`, `)`] {
		end++
	}
	if end == 0 {
		return
	}
	name := line[..end]
	if name !in imports {
		imports << name
	}
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

// print_type_diagnostics renders notices before fatal type errors.
fn print_type_diagnostics(a &flat.FlatAst, notices []types.TypeError, type_errors []types.TypeError, all_errors bool) {
	mut ordered_notices := notices.clone()
	ordered_notices.sort_with_compare(compare_print_notices)
	for notice in ordered_notices {
		if all_errors && notice.msg.starts_with('unused variable: `')
			&& unused_notice_is_parameter_redefinition_cascade(a, notice, type_errors) {
			continue
		}
		severity := if notice.severity.len > 0 { notice.severity } else { 'notice:' }
		eprintln(v3errors.formatted_error(severity, notice.msg, a, notice.node, notice.pos))
		print_type_diagnostic_details(notice.details)
	}
	source_errors := reorder_chained_generic_inference_errors(a, dedupe_type_diagnostics(a, type_errors))
	mut ordered_errors := []types.TypeError{cap: source_errors.len}
	for err in source_errors {
		if !is_bare_generic_fntype_decl_error(err) {
			ordered_errors << err
		}
	}
	for err in source_errors {
		if is_bare_generic_fntype_decl_error(err) {
			ordered_errors << err
		}
	}
	max_errors := if all_errors || ordered_errors.len < 20 { ordered_errors.len } else { 20 }
	for ei in 0 .. max_errors {
		err := ordered_errors[ei]
		severity := if err.severity.len > 0 { err.severity } else { 'error:' }
		eprintln(v3errors.formatted_error(severity, err.msg, a, err.node, err.pos))
		print_type_diagnostic_details(err.details)
	}
	if !all_errors && ordered_errors.len > max_errors {
		eprintln('... and ${ordered_errors.len - max_errors} more errors')
	}
}

fn has_conflicting_c_declaration_errors(errors []types.TypeError) bool {
	return errors.any(it.kind == .duplicate_decl
		&& (it.msg.starts_with('cannot redeclare C struct `')
			|| (it.msg.starts_with('C function `')
				&& it.msg.contains('was already declared with a different signature'))))
}

fn unused_notice_is_parameter_redefinition_cascade(a &flat.FlatAst, notice types.TypeError, type_errors []types.TypeError) bool {
	notice_fn := type_diagnostic_enclosing_fn(a, notice)
	if int(notice_fn) < 0 {
		return false
	}
	notice_name := notice.msg.find_between('`', '`')
	for diagnostic in type_errors {
		if !diagnostic.msg.starts_with('redefinition of parameter `') {
			continue
		}
		error_fn := type_diagnostic_enclosing_fn(a, diagnostic)
		redefined_name := diagnostic.msg.find_between('`', '`')
		if error_fn == notice_fn && notice_name.len > 0 && notice_name == redefined_name {
			return true
		}
	}
	return false
}

fn dedupe_type_diagnostics(a &flat.FlatAst, type_errors []types.TypeError) []types.TypeError {
	mut deduped := []types.TypeError{cap: type_errors.len}
	for err in type_errors {
		if err.msg.ends_with('` must be initialized')
			&& type_errors.any(it.msg.starts_with('enum `') && it.msg.ends_with('` is private')
				&& it.pos.id == err.pos.id && err.pos.offset >= it.pos.offset
				&& err.pos.end <= it.pos.end) {
			continue
		}
		if err.msg.starts_with('unknown type `')
			&& type_errors.any(it.msg == 'generic struct cannot be used in non-generic function'
				&& it.pos.id == err.pos.id && err.pos.offset >= it.pos.offset
				&& err.pos.end <= it.pos.end) {
			continue
		}
		if err.msg.starts_with('unknown struct `') {
			name := err.msg.all_after('`').all_before('`')
			err_fn := type_diagnostic_enclosing_fn(a, err)
			if int(err_fn) >= 0
				&& type_errors.any(it.msg.starts_with('generic type name `${name}` is not mentioned in fn ')
					&& type_diagnostic_enclosing_fn(a, it) == err_fn) {
				continue
			}
		}
		if err.msg.contains('` is a generic fn, you should pass its concrete types, e.g. ')
			&& err.msg.ends_with('[int]') {
			deduped << err
			continue
		}
		if deduped.any(it.msg == err.msg && it.pos.id == err.pos.id
			&& it.pos.offset == err.pos.offset && it.pos.end == err.pos.end
			&& it.severity == err.severity) {
			continue
		}
		deduped << err
	}
	return deduped
}

fn type_diagnostic_enclosing_fn(a &flat.FlatAst, diagnostic types.TypeError) flat.NodeId {
	if int(diagnostic.node) >= 0 && int(diagnostic.node) < a.nodes.len
		&& a.node(diagnostic.node).kind == .fn_decl {
		return diagnostic.node
	}
	mut diagnostic_pos := diagnostic.pos
	if !diagnostic_pos.is_valid() && int(diagnostic.node) >= 0 && int(diagnostic.node) < a.nodes.len {
		diagnostic_pos = a.node(diagnostic.node).pos
	}
	mut enclosing := flat.empty_node
	mut enclosing_len := 2147483647
	for index, node in a.nodes {
		if node.kind != .fn_decl {
			continue
		}
		if diagnostic_pos.is_valid() && node.pos.is_valid() && node.pos.id == diagnostic_pos.id
			&& diagnostic_pos.offset >= node.pos.offset && diagnostic_pos.end <= node.pos.end {
			span_len := node.pos.end - node.pos.offset
			if span_len < enclosing_len {
				enclosing = flat.NodeId(index)
				enclosing_len = span_len
			}
			continue
		}
		if int(diagnostic.node) >= 0
			&& type_diagnostic_node_tree_contains(a, flat.NodeId(index), diagnostic.node, 0) {
			return flat.NodeId(index)
		}
	}
	if int(enclosing) >= 0 {
		return enclosing
	}
	mut nearest := flat.empty_node
	mut nearest_offset := -1
	for index, node in a.nodes {
		if node.kind !in [.fn_decl, .struct_decl, .interface_decl, .type_decl, .enum_decl,
			.const_decl, .global_decl, .c_fn_decl, .module_decl, .import_decl] || !node.pos.is_valid() || !diagnostic_pos.is_valid()
			|| node.pos.id != diagnostic_pos.id || node.pos.offset > diagnostic_pos.offset
			|| node.pos.offset <= nearest_offset {
			continue
		}
		nearest = flat.NodeId(index)
		nearest_offset = node.pos.offset
	}
	if int(nearest) >= 0 && a.node(nearest).kind == .fn_decl {
		return nearest
	}
	return flat.empty_node
}

fn type_diagnostic_node_tree_contains(a &flat.FlatAst, root_id flat.NodeId, target_id flat.NodeId, depth int) bool {
	if root_id == target_id {
		return true
	}
	if depth > 32 || int(root_id) < 0 || int(root_id) >= a.nodes.len {
		return false
	}
	root := a.node(root_id)
	for i in 0 .. root.children_count {
		if type_diagnostic_node_tree_contains(a, a.child(root, i), target_id, depth + 1) {
			return true
		}
	}
	return false
}

fn reorder_chained_generic_inference_errors(a &flat.FlatAst, errors []types.TypeError) []types.TypeError {
	mut paired_call_by_struct := map[int]int{}
	mut paired_calls := map[int]bool{}
	for struct_index, struct_error in errors {
		if !struct_error.msg.starts_with('could not infer generic type `')
			|| !struct_error.msg.contains(' in generic struct `') {
			continue
		}
		for call_index, call_error in errors {
			if !call_error.msg.starts_with('could not infer generic type `')
				|| !call_error.msg.contains(' in call to `') || paired_calls[call_index]
				|| !type_diagnostic_call_uses_struct_receiver(a, call_error.node, struct_error.node) {
				continue
			}
			paired_call_by_struct[struct_index] = call_index
			paired_calls[call_index] = true
			break
		}
	}
	if paired_call_by_struct.len == 0 {
		return errors.clone()
	}
	mut ordered := []types.TypeError{cap: errors.len}
	for struct_index, struct_error in errors {
		if call_index := paired_call_by_struct[struct_index] {
			ordered << struct_error
			ordered << errors[call_index]
		}
	}
	for index, err in errors {
		if index !in paired_call_by_struct && !paired_calls[index] {
			ordered << err
		}
	}
	return ordered
}

fn type_diagnostic_call_uses_struct_receiver(a &flat.FlatAst, call_id flat.NodeId, struct_id flat.NodeId) bool {
	call_index := int(call_id)
	if call_index < 0 || call_index >= a.nodes.len {
		return false
	}
	call := a.nodes[call_index]
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	mut callee_id := a.child(&call, 0)
	mut callee := a.node(callee_id)
	if callee.kind == .index && callee.children_count > 0 {
		callee_id = a.child(callee, 0)
		callee = a.node(callee_id)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	return a.child(callee, 0) == struct_id
}

fn is_bare_generic_fntype_decl_error(err types.TypeError) bool {
	return err.msg.starts_with('generic function `')
		&& err.msg.contains(' in fn declaration must specify the generic type names')
}

fn print_type_diagnostic_details(details []string) {
	for index, detail in details {
		eprintln(if index == 0 { 'Details: ${detail}' } else { detail })
	}
}

fn compare_print_notices(a &types.TypeError, b &types.TypeError) int {
	a_is_unused_import := a.msg.contains(' is imported but never used.')
	b_is_unused_import := b.msg.contains(' is imported but never used.')
	if a_is_unused_import != b_is_unused_import {
		return if a_is_unused_import { -1 } else { 1 }
	}
	a_is_unsafe_call := a.msg.contains('must be called from an `unsafe` block')
	b_is_unsafe_call := b.msg.contains('must be called from an `unsafe` block')
	if a_is_unsafe_call != b_is_unsafe_call {
		return if a_is_unsafe_call { -1 } else { 1 }
	}
	a_is_reference_assignment := a.msg.starts_with('cannot assign a reference to a value')
	b_is_reference_assignment := b.msg.starts_with('cannot assign a reference to a value')
	if a_is_reference_assignment != b_is_reference_assignment {
		return if a_is_reference_assignment { -1 } else { 1 }
	}
	a_is_warning := a.severity == 'warning:'
	b_is_warning := b.severity == 'warning:'
	if a_is_warning != b_is_warning {
		return if a_is_warning { 1 } else { -1 }
	}
	a_is_unused_param := a.msg.starts_with('unused parameter:')
	b_is_unused_param := b.msg.starts_with('unused parameter:')
	a_is_unused_fn := a.msg.starts_with('unused function:')
	b_is_unused_fn := b.msg.starts_with('unused function:')
	if a_is_unused_param != b_is_unused_param {
		if a_is_unused_fn || b_is_unused_fn {
			return if a_is_unused_param { -1 } else { 1 }
		}
		return if a_is_unused_param { 1 } else { -1 }
	}
	return int(a.node) - int(b.node)
}

fn unsupported_backend_error(a &flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, backend string) ?string {
	mut cur_module := ''
	mut cur_file := ''
	mut visited := []bool{len: a.nodes.len}
	mut root_ids := []flat.NodeId{}
	mut root_modules := []string{}
	mut root_files := []string{}
	for idx, node in a.nodes {
		if node.kind == .file {
			cur_module = ''
			cur_file = node.value
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .fn_decl || (node.generic_params().len > 0 && !a.specialized_fn_nodes[idx]) {
			continue
		}
		module_name := a.specialized_fn_modules[idx] or { cur_module }
		if !transformed_fn_is_used(node.value, module_name, used_fns) {
			continue
		}
		root_file := a.specialized_fn_files[idx] or { cur_file }
		root_ids << flat.NodeId(idx)
		root_modules << module_name
		root_files << root_file
		diagnose_aggregates := tc.diagnostic_files.len == 0 || root_file in tc.diagnostic_files
		fallback_location := backend_fn_location(a, node)
		if backend == 'wasm' && diagnose_aggregates {
			return_type := tc.parse_resolution_type(node.typ)
			if return_type is types.OptionType {
				return '${fallback_location}error: option types are not implemented by the V3 wasm backend'
			}
			if return_type is types.ResultType {
				return '${fallback_location}error: result types are not implemented by the V3 wasm backend'
			}
			mut infix_visited := []bool{len: a.nodes.len}
			if msg := unsupported_wasm_struct_infix_error(a, tc, flat.NodeId(idx), fallback_location, mut infix_visited) {
				return msg
			}
		}
		if msg := unsupported_backend_node_error(a, tc, flat.NodeId(idx), backend, diagnose_aggregates, fallback_location, mut visited) {
			return msg
		}
	}
	cur_module = ''
	cur_file = ''
	for idx, node in a.nodes {
		if node.kind == .file {
			cur_module = ''
			cur_file = node.value
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if idx < a.user_code_start {
			continue
		}
		if node.kind == .global_decl {
			for i in 0 .. node.children_count {
				field_id := a.child(&node, i)
				field := a.node(field_id)
				if field.children_count == 0 {
					continue
				}
				root_ids << a.child(field, 0)
				root_modules << cur_module
				root_files << cur_file
				diagnose_aggregates := tc.diagnostic_files.len == 0
					|| cur_file in tc.diagnostic_files
				fallback_location := backend_node_location(a, *field)
				if msg := unsupported_backend_node_error(a, tc, a.child(field, 0), backend, diagnose_aggregates, fallback_location, mut visited) {
					return msg
				}
			}
		} else if node.kind == .enum_decl {
			for i in 0 .. node.children_count {
				field := a.child_node(&node, i)
				if field.kind != .enum_field || field.children_count == 0 {
					continue
				}
				expr_id := a.child(field, 0)
				root_ids << expr_id
				root_modules << cur_module
				root_files << cur_file
				diagnose_aggregates := tc.diagnostic_files.len == 0
					|| cur_file in tc.diagnostic_files
				fallback_location := backend_node_location(a, *field)
				if msg := unsupported_backend_node_error(a, tc, expr_id, backend, diagnose_aggregates, fallback_location, mut visited) {
					return msg
				}
			}
		}
	}
	for expr_id in markused.reachable_const_exprs(a, tc, root_ids, root_modules, root_files) {
		mut diagnose_aggregates := false
		if source_file := a.source_files[a.node(expr_id).pos.id] {
			diagnose_aggregates = tc.diagnostic_files.len == 0
				|| source_file.name in tc.diagnostic_files
		}
		fallback_location := backend_node_location(a, *a.node(expr_id))
		if msg := unsupported_backend_node_error(a, tc, expr_id, backend, diagnose_aggregates, fallback_location, mut visited) {
			return msg
		}
	}
	return none
}

fn unsupported_wasm_struct_infix_error(a &flat.FlatAst, tc &types.TypeChecker, id flat.NodeId, fallback_location string, mut visited []bool) ?string {
	idx := int(id)
	if idx < 0 || idx >= a.nodes.len || visited[idx] {
		return none
	}
	visited[idx] = true
	node := a.nodes[idx]
	if node.kind == .infix && node.children_count >= 2 && node.op in [.eq, .ne] {
		lhs_type := tc.resolve_type(a.child(&node, 0))
		rhs_type := tc.resolve_type(a.child(&node, 1))
		lhs_name := lhs_type.name().trim_string_left('main.')
		rhs_name := rhs_type.name().trim_string_left('main.')
		if lhs_name == rhs_name && lhs_name.len > 0 && lhs_name in tc.structs {
			operator := if node.op == .eq { '==' } else { '!=' }
			return '${fallback_location}error: the V3 wasm backend does not support `${operator}` for type `${lhs_name}` yet'
		}
		// Struct equality is lowered before backend validation into field-wise
		// comparisons. Recover the aggregate type from selector operands so the
		// wasm backend reports the source operation instead of rejecting the
		// first lowered struct literal.
		lhs_origin := wasm_struct_origin_type(a, a.child(&node, 0))
		rhs_origin := wasm_struct_origin_type(a, a.child(&node, 1))
		if lhs_origin.len > 0 && lhs_origin == rhs_origin {
			operator := if node.op == .eq { '==' } else { '!=' }
			return '${fallback_location}error: the V3 wasm backend does not support `${operator}` for type `${lhs_origin}` yet'
		}
	}
	for i in 0 .. node.children_count {
		if msg := unsupported_wasm_struct_infix_error(a, tc, a.child(&node, i), fallback_location, mut visited) {
			return msg
		}
	}
	return none
}

fn wasm_struct_origin_type(a &flat.FlatAst, id flat.NodeId) string {
	idx := int(id)
	if idx < 0 || idx >= a.nodes.len {
		return ''
	}
	node := a.nodes[idx]
	if node.kind == .struct_init {
		return node.typ.trim_string_left('main.')
	}
	if node.kind in [.selector, .cast_expr, .paren] && node.children_count > 0 {
		return wasm_struct_origin_type(a, a.child(&node, 0))
	}
	return ''
}

fn backend_node_location(a &flat.FlatAst, node flat.Node) string {
	if source_pos := a.source_position(node.pos) {
		return '${source_pos}: '
	}
	return ''
}

fn backend_fn_location(a &flat.FlatAst, node flat.Node) string {
	if source_pos := a.source_position(node.pos) {
		return '${source_pos}: '
	}
	return ''
}

fn unsupported_backend_node_error(a &flat.FlatAst, tc &types.TypeChecker, id flat.NodeId, backend string, diagnose_aggregates bool, fallback_location string, mut visited []bool) ?string {
	idx := int(id)
	if idx < 0 || idx >= a.nodes.len || visited[idx] {
		return none
	}
	visited[idx] = true
	node := a.nodes[idx]
	if backend == 'wasm' && diagnose_aggregates {
		mut unsupported_type := ''
		if node.kind in [.array_literal, .array_init, .map_init, .struct_init] {
			unsupported_type = tc.resolve_type(id).name()
		} else if node.kind == .call && node.children_count > 0 {
			callee := a.child_node(&node, 0)
			if callee.kind == .ident && callee.value == 'new_map' {
				unsupported_type = tc.resolve_type(id).name()
			}
		}
		if unsupported_type.len > 0 {
			return '${fallback_location}error: the V3 wasm backend does not support type `${unsupported_type}` yet'
		}
		if node.kind == .infix && node.children_count >= 2 && node.op in [.eq, .ne] {
			lhs_type := tc.resolve_type(a.child(&node, 0))
			rhs_type := tc.resolve_type(a.child(&node, 1))
			lhs_name := lhs_type.name().trim_string_left('main.')
			rhs_name := rhs_type.name().trim_string_left('main.')
			if lhs_name == rhs_name && lhs_name.len > 0 && lhs_name in tc.structs {
				operator := if node.op == .eq { '==' } else { '!=' }
				return '${fallback_location}error: the V3 wasm backend does not support `${operator}` for type `${lhs_name}` yet'
			}
		}
	}
	op := match node.op {
		.power { '**' }
		.power_assign { '**=' }
		else { '' }
	}
	if op.len > 0 {
		location := if source_pos := a.source_position(node.pos) {
			'${source_pos}: '
		} else {
			fallback_location
		}
		return '${location}error: operator `${op}` is not supported by the V3 ${backend} backend'
	}
	if node.kind == .defer_result {
		location := if source_pos := a.source_position(node.pos) {
			'${source_pos}: '
		} else {
			fallback_location
		}
		return '${location}error: `\$res()` is not supported by the V3 ${backend} backend'
	}
	for i in 0 .. node.children_count {
		if msg := unsupported_backend_node_error(a, tc, a.child(&node, i), backend, diagnose_aggregates, fallback_location, mut visited) {
			return msg
		}
	}
	return none
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
		if is_v3_test_file(file, backend, target) {
			files << file
		}
	}
	return files
}

fn is_test_file_for_any_backend(file string) bool {
	name := os.file_name(file)
	if name.contains('_d_test.') || name.contains('_notd_test.') {
		return false
	}
	if name.ends_with('_test.v') {
		return true
	}
	if !name.ends_with('.v') {
		return false
	}
	base := name[..name.len - 2]
	return base.contains('.') && base.all_before_last('.').ends_with('_test')
}

fn is_v3_test_file(file string, backend string, target pref.Target) bool {
	return file.ends_with('_test.vv') || pref.is_test_file_for_platform(file, backend, target)
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
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt, .for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt {
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

// skipped_backend_module_groups lists the importable backend module groups that the current
// configuration excludes (driven by the same `skip_*` defines that gate the dispatch in
// main()). The arm64 backend is the only consumer of the SSA pipeline, so it shares a group
// with v3.ssa and v3.ssa.optimize.
fn skipped_backend_module_groups(prefs &pref.Preferences) [][]string {
	mut skipped := [][]string{}
	if 'skip_fastc' in prefs.user_defines {
		skipped << ['v3.gen.fastc', 'v3.fastcdriver']
	}
	if 'skip_arm64' in prefs.user_defines {
		skipped << ['v3.gen.arm64', 'v3.ssa', 'v3.ssa.optimize']
	}
	if 'skip_wasm' in prefs.user_defines {
		skipped << ['v3.gen.wasm']
	}
	if 'skip_eval' in prefs.user_defines {
		skipped << ['v3.eval']
	}
	return skipped
}

struct ImplicitFieldScanIndex {
mut:
	aliases     map[string]string
	fields      map[string]map[string]string
	enum_fields map[string]map[string]bool
	fn_returns  map[string]string
	globals     map[string]string
}

struct ImplicitImportScan {
mut:
	node_idx             int
	field_index_node_idx int
	field_index          ImplicitFieldScanIndex
	needs_sync           bool
	has_sync             bool
	needs_embed          bool
	has_embed_import     bool
	needs_closure        bool
	has_closure          bool
	needs_debugger       bool
	has_debugger         bool
}

const closure_runtime_import_alias = '__v3_builtin_closure_runtime'

fn seed_implicit_imports(mut a flat.FlatAst, skip_closure_runtime bool) {
	start := a.nodes.len
	// Builtin declares the channel ABI even when a program never uses channels.
	// Start at user code so that declaration alone does not pull the whole sync
	// module into every program; imported source is scanned wave by wave below.
	mut scan := ImplicitImportScan{
		node_idx: a.user_code_start
	}
	scan_implicit_imports(a, a.nodes.len, mut scan)
	if scan.needs_sync && !scan.has_sync {
		a.add_node(sync_import_node())
	}
	if scan.needs_embed && !scan.has_embed_import {
		a.add_node(embed_file_import_node())
	}
	// Bound method values, lambdas, and captured fn literals are materialized during
	// transform, after import resolution. Seed the runtime only when parsed syntax can
	// need it; unconditional insertion conflicts with user modules whose source alias is
	// `closure`.
	if !skip_closure_runtime && scan.needs_closure && !scan.has_closure {
		a.add_node(closure_import_node())
	}
	if scan.needs_debugger && !scan.has_debugger {
		a.add_node(debugger_import_node())
	}
	a.intern_node_texts_from(start)
}

fn sync_import_node() flat.Node {
	return flat.Node{
		kind: .import_decl
		value: 'sync'
		typ: 'sync'
	}
}

fn embed_file_import_node() flat.Node {
	return flat.Node{
		kind: .import_decl
		value: 'v.embed_file'
		typ: 'embed_file'
	}
}

fn closure_import_node() flat.Node {
	return flat.Node{
		kind: .import_decl
		value: 'builtin.closure'
		// This node is appended in the last user file's scope. A compiler-private
		// alias prevents it from overwriting a user import named `closure`; generated
		// runtime calls already use the resolved `closure.*` declaration keys.
		typ: closure_runtime_import_alias
	}
}

fn debugger_import_node() flat.Node {
	return flat.Node{
		kind: .import_decl
		value: 'v.debug'
		typ: '__v3_debugger_runtime'
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
		kind: .file
		value: cache_bundle_import_file(builtin_dir)
	}
	a.nodes << flat.Node{
		kind: .module_decl
		value: 'builtin'
	}
	for import_path in modulecache.builtin_bundle_imports {
		a.nodes << flat.Node{
			kind: .import_decl
			value: import_path
			typ: import_path.all_after_last('.')
		}
	}
	a.intern_node_texts_from(start)
}

fn cache_bundle_import_file(builtin_dir string) string {
	return os.join_path_single(builtin_dir, cache_bundle_import_file_name)
}

fn scan_implicit_imports(a &flat.FlatAst, end_node int, mut scan ImplicitImportScan) {
	mut call_callees := map[int]bool{}
	if !scan.needs_closure {
		for i in scan.node_idx .. end_node {
			node := a.nodes[i]
			if node.kind == .call && node.children_count > 0 {
				call_callees[int(a.child(&node, 0))] = true
			} else if node.kind == .lambda_expr {
				scan.needs_closure = true
			} else if node.kind == .fn_literal {
				for child_idx in 0 .. node.children_count {
					if a.child_node(&node, child_idx).kind == .ident {
						scan.needs_closure = true
						break
					}
				}
			}
		}
	}
	known_field_selectors := if scan.needs_closure {
		map[int]bool{}
	} else {
		implicit_field_scan_index_append(a, scan.field_index_node_idx, end_node, mut scan.field_index)
		scan.field_index_node_idx = end_node
		implicit_known_field_selectors(a, scan.node_idx, end_node, scan.field_index)
	}
	for i in scan.node_idx .. end_node {
		node := a.nodes[i]
		if node.kind == .import_decl {
			if node.value == 'sync' {
				scan.has_sync = true
			} else if node.value == 'v.embed_file' {
				scan.has_embed_import = true
			} else if node.value == 'builtin.closure' {
				scan.has_closure = true
			} else if node.value == 'v.debug' {
				scan.has_debugger = true
			}
		}
		if node.kind == .debugger_stmt {
			scan.needs_debugger = true
		}
		if !scan.needs_sync {
			if node.kind == .lock_expr
				|| (node.kind in [.field_decl, .param] && type_text_is_shared(node.typ))
				|| (node.kind == .decl_assign && decl_assign_value_is_shared(node.value))
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
		// Builtin is parsed before `user_code_start` and contains ordinary selector
		// values that must not force the closure runtime into every program.
		if !scan.needs_closure && i >= a.user_code_start {
			if node.kind == .lambda_expr {
				// Lambda captures are inferred during transform, after imports have
				// already been resolved, so conservatively seed their possible runtime.
				scan.needs_closure = true
			} else if node.kind == .fn_literal {
				for child_idx in 0 .. node.children_count {
					if a.child_node(&node, child_idx).kind == .ident {
						scan.needs_closure = true
						break
					}
				}
			} else if node.kind == .selector && node.children_count > 0 && i !in call_callees && i !in known_field_selectors && !implicit_selector_is_interop_symbol(a, node) {
				// A remaining selector used as a value may be a bound method. Full type
				// information is unavailable during import discovery, so conservatively
				// load the runtime; calls, C/JS symbols, and provable fields are excluded.
				scan.needs_closure = true
			}
		}
	}
	scan.node_idx = end_node
}

fn implicit_selector_is_interop_symbol(a &flat.FlatAst, node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	base := a.child_node(&node, 0)
	return base.kind == .ident && base.value in ['C', 'JS']
}

fn implicit_known_field_selectors(a &flat.FlatAst, start int, end int, index ImplicitFieldScanIndex) map[int]bool {
	mut selectors := map[int]bool{}
	for fn_idx in start .. end {
		fn_node := a.nodes[fn_idx]
		if fn_node.kind != .fn_decl {
			continue
		}
		mut bindings := map[string]string{}
		mut ambiguous := map[string]bool{}
		mut body_roots := []flat.NodeId{cap: int(fn_node.children_count)}
		for child_idx in 0 .. fn_node.children_count {
			child_id := a.child(&fn_node, child_idx)
			child := a.node(child_id)
			if child.kind == .param {
				if child.value in bindings {
					ambiguous[child.value] = true
				} else if child.typ.len > 0 {
					bindings[child.value] = implicit_normalize_type(child.typ, index.aliases)
				}
			} else {
				body_roots << child_id
			}
		}
		mut candidates := []flat.NodeId{}
		mut declarations := []flat.NodeId{}
		mut stack := body_roots.clone()
		for stack.len > 0 {
			id := stack.pop()
			if int(id) < 0 {
				continue
			}
			node := a.node(id)
			if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
				continue
			}
			if node.kind == .decl_assign {
				declarations << id
				for child_idx := 0; child_idx < node.children_count; child_idx += 2 {
					lhs := a.child_node(node, child_idx)
					if lhs.kind == .ident && lhs.value in bindings {
						ambiguous[lhs.value] = true
					}
				}
			} else if node.kind == .for_in_stmt && node.children_count >= 2 {
				for child_idx in 0 .. 2 {
					local := a.child_node(node, child_idx)
					if local.kind == .ident {
						ambiguous[local.value] = true
					}
				}
			} else if node.kind == .selector && node.children_count > 0 {
				candidates << id
			}
			for child_idx in 0 .. node.children_count {
				stack << a.child(node, child_idx)
			}
		}
		for _ in 0 .. declarations.len {
			mut changed := false
			for declaration_id in declarations {
				declaration := a.node(declaration_id)
				for child_idx := 0; child_idx + 1 < declaration.children_count; child_idx += 2 {
					lhs := a.child_node(declaration, child_idx)
					if lhs.kind != .ident || lhs.value in ambiguous {
						continue
					}
					rhs_id := a.child(declaration, child_idx + 1)
					typ := implicit_expr_type(a, rhs_id, bindings, index, 0)
					if typ == '' {
						continue
					}
					normalized := implicit_normalize_type(typ, index.aliases)
					if old := bindings[lhs.value] {
						if old != normalized {
							ambiguous[lhs.value] = true
							bindings.delete(lhs.value)
						}
					} else {
						bindings[lhs.value] = normalized
						changed = true
					}
				}
			}
			if !changed {
				break
			}
		}
		for selector_id in candidates {
			selector := a.node(selector_id)
			base_id := a.child(selector, 0)
			base := a.node(base_id)
			if base.kind == .ident && base.value in ambiguous {
				continue
			}
			base_type := implicit_expr_type(a, base_id, bindings, index, 0)
			if implicit_type_has_field(base_type, selector.value, index) {
				selectors[int(selector_id)] = true
				continue
			}
			if base.kind == .ident {
				if enum_fields := index.enum_fields[base.value] {
					if selector.value in enum_fields {
						selectors[int(selector_id)] = true
					}
				}
			}
		}
	}
	return selectors
}

fn implicit_field_scan_index_append(a &flat.FlatAst, start int, end int, mut index ImplicitFieldScanIndex) {
	for idx in start .. end {
		node := a.nodes[idx]
		node_ref := a.node(flat.NodeId(idx))
		match node.kind {
			.type_decl {
				if node.value.len > 0 && node.typ.len > 0 {
					index.aliases[node.value] = node.typ
				}
			}
			.struct_decl {
				mut declared := map[string]string{}
				for child_idx in 0 .. node.children_count {
					field := a.child_node(node_ref, child_idx)
					if field.kind == .field_decl && field.value.len > 0 {
						declared[field.value] = field.typ
					}
				}
				if declared.len > 0 {
					index.fields[node.value] = declared.move()
				}
			}
			.enum_decl {
				mut declared := map[string]bool{}
				for child_idx in 0 .. node.children_count {
					field := a.child_node(node_ref, child_idx)
					if field.kind == .enum_field && field.value.len > 0 {
						declared[field.value] = true
					}
				}
				if declared.len > 0 {
					index.enum_fields[node.value] = declared.move()
				}
			}
			.fn_decl, .c_fn_decl {
				if node.value.len == 0 || node.typ.len == 0 {
					continue
				}
				if old := index.fn_returns[node.value] {
					if old != node.typ {
						index.fn_returns[node.value] = ''
					}
				} else {
					index.fn_returns[node.value] = node.typ
				}
			}
			else {}
		}
	}
	for idx in start .. end {
		node := a.nodes[idx]
		node_ref := a.node(flat.NodeId(idx))
		if node.kind !in [.const_decl, .global_decl] {
			continue
		}
		for child_idx in 0 .. node.children_count {
			field := a.child_node(node_ref, child_idx)
			if field.kind != .const_field || field.children_count == 0 {
				continue
			}
			typ := if field.typ.len > 0 {
				field.typ
			} else {
				implicit_expr_type(a, a.child(field, 0), index.globals, index, 0)
			}
			if typ.len > 0 {
				index.globals[field.value] = implicit_normalize_type(typ, index.aliases)
			}
		}
	}
}

fn implicit_expr_type(a &flat.FlatAst, id flat.NodeId, bindings map[string]string, index ImplicitFieldScanIndex, depth int) string {
	if int(id) < 0 || depth > 12 {
		return ''
	}
	node := a.node(id)
	if node.typ.len > 0 {
		return implicit_normalize_type(node.typ, index.aliases)
	}
	match node.kind {
		.ident {
			if typ := bindings[node.value] {
				return typ
			}
			if typ := index.globals[node.value] {
				return typ
			}
			if node.value in index.fields || node.value in index.enum_fields {
				return node.value
			}
			return ''
		}
		.string_literal {
			return 'string'
		}
		.int_literal {
			return 'int'
		}
		.float_literal {
			return 'f64'
		}
		.bool_literal {
			return 'bool'
		}
		.char_literal {
			return 'u8'
		}
		.array_literal, .array_init {
			if node.children_count == 0 {
				return '[]void'
			}
			elem_type := implicit_expr_type(a, a.child(node, 0), bindings, index, depth + 1)
			if elem_type.len > 0 {
				return '[]${elem_type}'
			}
			return '[]void'
		}
		.map_init {
			return if node.typ.len > 0 { node.typ } else { 'map[void]void' }
		}
		.struct_init {
			return implicit_normalize_type(node.value, index.aliases)
		}
		.paren, .expr_stmt, .postfix {
			if node.children_count == 1 {
				return implicit_expr_type(a, a.child(node, 0), bindings, index, depth + 1)
			}
		}
		.block {
			if node.children_count > 0 {
				return implicit_expr_type(a, a.child(node, node.children_count - 1), bindings, index, depth + 1)
			}
		}
		.if_expr {
			if node.children_count < 3 {
				return ''
			}
			mut typ := ''
			for child_idx in 1 .. node.children_count {
				branch_id := a.child(node, child_idx)
				branch_type := implicit_expr_type(a, branch_id, bindings, index, depth + 1)
				if branch_type == '' {
					return ''
				}
				if typ == '' {
					typ = branch_type
				} else if implicit_normalize_type(typ, index.aliases) != implicit_normalize_type(branch_type, index.aliases) {
					return ''
				}
			}
			return typ
		}
		.call {
			return implicit_call_return_type(a, node, bindings, index, depth + 1)
		}
		.selector {
			if node.children_count > 0 {
				base_type := implicit_expr_type(a, a.child(node, 0), bindings, index, depth + 1)
				return implicit_field_type(base_type, node.value, index)
			}
		}
		.index {
			if node.children_count > 0 {
				base_type := implicit_normalize_type(implicit_expr_type(a, a.child(node, 0), bindings, index, depth + 1), index.aliases)
				if base_type.starts_with('[]') {
					return base_type[2..]
				}
				if base_type.starts_with('[') {
					close := base_type.index(']') or { return '' }
					if close + 1 < base_type.len {
						return base_type[close + 1..]
					}
				}
				if base_type == 'string' {
					return 'u8'
				}
			}
		}
		.cast_expr, .as_expr {
			if node.typ.len > 0 {
				return implicit_normalize_type(node.typ, index.aliases)
			}
			if node.value.len > 0 {
				return implicit_normalize_type(node.value, index.aliases)
			}
		}
		else {}
	}
	return ''
}

fn implicit_call_return_type(a &flat.FlatAst, call &flat.Node, bindings map[string]string, index ImplicitFieldScanIndex, depth int) string {
	if call.children_count == 0 {
		return ''
	}
	callee := a.child_node(call, 0)
	if callee.kind == .ident {
		if typ := index.fn_returns[callee.value] {
			return implicit_normalize_type(typ, index.aliases)
		}
		return ''
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return ''
	}
	base_id := a.child(callee, 0)
	base_type := implicit_normalize_type(implicit_expr_type(a, base_id, bindings, index, depth + 1), index.aliases)
	if base_type == 'string' {
		if callee.value == 'runes' {
			return '[]rune'
		}
		if callee.value == 'bytes' {
			return '[]u8'
		}
	}
	if base_type.len > 0 {
		for key in ['${base_type}.${callee.value}',
			'${base_type.all_after_last('.')}.${callee.value}'] {
			if typ := index.fn_returns[key] {
				return implicit_normalize_type(typ, index.aliases)
			}
		}
	}
	return ''
}

fn implicit_type_has_field(raw_type string, field string, index ImplicitFieldScanIndex) bool {
	return implicit_field_type(raw_type, field, index) != ''
}

fn implicit_field_type(raw_type string, field string, index ImplicitFieldScanIndex) string {
	typ := implicit_normalize_type(raw_type, index.aliases)
	if typ == '' {
		return ''
	}
	if typ == 'string' {
		return match field {
			'len', 'flags', 'is_lit' { 'int' }
			'str' { '&u8' }
			else { '' }
		}
	}
	if typ.starts_with('[]') || typ.starts_with('...')
		|| (typ.starts_with('[') && typ.contains(']')) {
		return match field {
			'len', 'cap', 'offset', 'flags', 'element_size' { 'int' }
			'data' { 'voidptr' }
			else { '' }
		}
	}
	if typ.starts_with('map[') {
		return match field {
			'len', 'cap' { 'int' }
			else { '' }
		}
	}
	if declared := index.fields[typ] {
		if field_type := declared[field] {
			return implicit_normalize_type(field_type, index.aliases)
		}
	}
	return ''
}

fn implicit_normalize_type(raw string, aliases map[string]string) string {
	mut typ := raw.trim_space()
	for _ in 0 .. 12 {
		mut changed := false
		for prefix in ['mut ', 'shared ', '&', '?', '!'] {
			if typ.starts_with(prefix) {
				typ = typ[prefix.len..].trim_space()
				changed = true
			}
		}
		if target := aliases[typ] {
			typ = target.trim_space()
			changed = true
		}
		if !changed {
			break
		}
	}
	return typ
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

fn decl_assign_value_is_shared(value string) bool {
	return value == 'shared' || value.starts_with('shared:')
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
		mut node := a.nodes[i]
		if node.kind == .directive && node.value.starts_with('@attributes:') {
			target_idx := node.value['@attributes:'.len..].int()
			if target_idx >= 0 && target_idx < old_len {
				node.value = '@attributes:${target_idx + synthetic_index_shift(insertions, target_idx)}'
				node = canonical_node_texts(mut a, node)
			}
		}
		new_nodes << node
	}
	// Insertions at pos == old_len append at the very end (the last wave module's
	// region ends at the array tail).
	for ins_idx < insertions.len {
		new_nodes << canonical_node_texts(mut a, insertions[ins_idx].node)
		ins_idx++
	}
	a.nodes = new_nodes
	a.file_node_ids = []int{}
	a.file_index_incomplete = true
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
// collect_import_scan_ids returns the node ids the import-resolution loops
// care about (.file markers/trailers, module_decl, import_decl) for every file
// pair at or past region_start, in ascending node order. Falls back to the
// full id range when the parser file index is unusable.
fn collect_import_scan_ids(a &flat.FlatAst, region_start int, pair_cursor int) ([]int, int) {
	if !file_index_usable_for_imports(a) {
		mut all := []int{cap: a.nodes.len - region_start}
		for i in region_start .. a.nodes.len {
			all << i
		}
		return all, pair_cursor
	}
	mut cursor := pair_cursor
	mut ids := []int{cap: 4096}
	mut last_trailing := region_start - 1
	for cursor + 1 < a.file_node_ids.len {
		marker := a.file_node_ids[cursor]
		if marker < region_start {
			cursor += 2
			continue
		}
		trailing := a.file_node_ids[cursor + 1]
		ids << marker
		tnode := a.nodes[trailing]
		collect_import_scan_children(a, &tnode, mut ids)
		ids << trailing
		if trailing > last_trailing {
			last_trailing = trailing
		}
		cursor += 2
	}
	// Synthetic import nodes (implicit sync/embed_file seeds) are appended
	// after the last parsed file and belong to no trailing .file node; sweep
	// the short tail so the walk sees exactly what the full scan sees.
	for i in last_trailing + 1 .. a.nodes.len {
		if a.nodes[i].kind in [.file, .module_decl, .import_decl] {
			ids << i
		}
	}
	return ids, cursor
}

fn collect_import_scan_children(a &flat.FlatAst, node &flat.Node, mut ids []int) {
	for ci in 0 .. node.children_count {
		id := int(a.child(node, ci))
		if id < 0 || id >= a.nodes.len {
			continue
		}
		child := a.nodes[id]
		match child.kind {
			.module_decl, .import_decl {
				ids << id
			}
			.comptime_if, .block {
				collect_import_scan_children(a, &child, mut ids)
			}
			else {}
		}
	}
}

fn file_index_usable_for_imports(a &flat.FlatAst) bool {
	return a.file_node_ids.len > 0 && a.file_node_ids.len % 2 == 0 && !a.file_index_incomplete
		&& os.getenv('V3_NO_FILE_IDX') == '' && os.getenv('V3_NO_IMPORT_IDX') == ''
}

struct ImportCollisionSeed {
	path           string
	importing_file string
}

struct EagerSelfhostImport {
	path           string
	importing_file string
}

struct EagerSelfhostModule {
	path     string
	dir      string
	real_dir string
	files    []string
mut:
	identity     string
	import_paths []string
}

struct EagerSelfhostScanArgs {
	path string
mut:
	imports []string
}

struct EagerSelfhostResolveArgs {
	prefs          voidptr
	path           string
	importing_file string
	project_root   string
mut:
	dir      string
	real_dir string
	files    []string
	identity string
}

fn eager_selfhost_scan_thread(arg voidptr) voidptr {
	mut scan := unsafe { &EagerSelfhostScanArgs(arg) }
	scan.imports = source_imports_fast(scan.path)
	return unsafe { nil }
}

fn eager_selfhost_resolve_thread(arg voidptr) voidptr {
	mut result := unsafe { &EagerSelfhostResolveArgs(arg) }
	prefs := unsafe { &pref.Preferences(result.prefs) }
	// Preserve the authoritative resolver's local/project/global precedence.
	// Every wave contains each import path once, and the chosen result is cached
	// for the authoritative pass by discover_eager_selfhost_modules.
	mut local_cache := map[string]string{}
	result.dir = resolve_project_or_pref_module_path(prefs, result.path, result.importing_file, result.project_root, mut local_cache)
	if result.dir.len > 0 && os.is_dir(result.dir) {
		result.real_dir = os.real_path(result.dir)
		result.files = pref.get_v_files_from_dir_for_target(result.dir, prefs.user_defines, prefs.target)
		if result.files.len > 0 {
			result.identity = import_module_identity_with_path_cache(prefs, result.path, result.importing_file, result.project_root, result.dir, mut local_cache)
		}
	}
	return unsafe { nil }
}

fn resolve_eager_selfhost_wave(a &flat.FlatAst, prefs &pref.Preferences, requests []EagerSelfhostImport, project_root string) []EagerSelfhostResolveArgs {
	mut results := []EagerSelfhostResolveArgs{cap: requests.len}
	mut tasks := []workers.Task{cap: requests.len}
	for i, import_req in requests {
		results << EagerSelfhostResolveArgs{
			prefs: voidptr(prefs)
			path: import_req.path
			importing_file: import_req.importing_file
			project_root: project_root
		}
		tasks << workers.Task{
			run: eager_selfhost_resolve_thread
			arg: unsafe { voidptr(&results[i]) }
			force_sync: i == 0
		}
	}
	if isnil(a.worker_pool) || a.worker_pool.size() == 0 {
		for i in 0 .. results.len {
			eager_selfhost_resolve_thread(unsafe { voidptr(&results[i]) })
		}
	} else {
		a.worker_pool.run(tasks)
	}
	return results
}

// source_imports_fast extracts the compiler tree's one-line import declarations
// without constructing tokens. It is used only to assemble the trusted
// -building-v parse batch; the real parser remains authoritative immediately
// afterwards.
@[direct_array_access]
fn source_imports_fast(path string) []string {
	source := os.read_file(path) or { return []string{} }
	mut imports := []string{cap: 16}
	mut i := 0
	mut at_line_head := true
	mut block_comment_depth := 0
	// Context 0 is root code. Strings and their `${...}` code regions are
	// pushed in alternating slots, so same-quoted strings inside interpolation
	// cannot accidentally close the outer string.
	mut context_kinds := [64]u8{}
	mut context_quotes := [64]u8{}
	mut context_raw := [64]bool{}
	mut interpolation_brace_depths := [64]int{}
	mut context_top := 0
	for i < source.len {
		if context_kinds[context_top] == 1 {
			quote := context_quotes[context_top]
			raw_string := context_raw[context_top]
			for i < source.len {
				c := source[i]
				if !raw_string && c == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				if !raw_string && quote != `\`` && c == `$` && i + 1 < source.len
					&& source[i + 1] == `{` {
					if context_top + 1 >= context_kinds.len {
						return imports
					}
					context_top++
					context_kinds[context_top] = 0
					interpolation_brace_depths[context_top] = 1
					at_line_head = false
					i += 2
					break
				}
				i++
				if c == quote {
					context_top--
					at_line_head = false
					break
				}
				if c == `\n` {
					at_line_head = true
				}
			}
			continue
		}
		if block_comment_depth > 0 {
			for i < source.len {
				c := source[i]
				if i + 1 < source.len && c == `/` && source[i + 1] == `*` {
					block_comment_depth++
					i += 2
					continue
				}
				if i + 1 < source.len && c == `*` && source[i + 1] == `/` {
					block_comment_depth--
					i += 2
					if block_comment_depth == 0 {
						break
					}
					continue
				}
				if c == `\n` {
					at_line_head = true
				}
				i++
			}
			continue
		}
		for i < source.len {
			c := source[i]
			if c == `\n` {
				at_line_head = true
				i++
				continue
			}
			if i + 1 < source.len && c == `/` && source[i + 1] == `/` {
				for i < source.len && source[i] != `\n` {
					i++
				}
				continue
			}
			if i + 1 < source.len && c == `/` && source[i + 1] == `*` {
				block_comment_depth = 1
				i += 2
				break
			}
			if c == `r` && i + 1 < source.len && source[i + 1] in [`'`, `"`] {
				if context_top + 1 >= context_kinds.len {
					return imports
				}
				context_top++
				context_kinds[context_top] = 1
				context_quotes[context_top] = source[i + 1]
				context_raw[context_top] = true
				at_line_head = false
				i += 2
				break
			}
			if c in [`'`, `"`, `\``] {
				if context_top + 1 >= context_kinds.len {
					return imports
				}
				context_top++
				context_kinds[context_top] = 1
				context_quotes[context_top] = c
				context_raw[context_top] = false
				at_line_head = false
				i++
				break
			}
			if context_top > 0 && c == `{` {
				interpolation_brace_depths[context_top]++
				at_line_head = false
				i++
				continue
			}
			if context_top > 0 && c == `}` {
				interpolation_brace_depths[context_top]--
				at_line_head = false
				i++
				if interpolation_brace_depths[context_top] == 0 {
					context_top--
					break
				}
				continue
			}
			if at_line_head && c in [` `, `\t`, `\r`] {
				i++
				continue
			}
			if context_top == 0 && at_line_head && i + 7 <= source.len
				&& source[i..i + 6] == 'import' && source[i + 6] in [` `, `\t`] {
				i += 7
				for i < source.len && source[i] in [` `, `\t`] {
					i++
				}
				start := i
				for i < source.len && ((source[i] >= `a` && source[i] <= `z`)
					|| (source[i] >= `A` && source[i] <= `Z`)
					|| (source[i] >= `0` && source[i] <= `9`)
					|| source[i] in [`_`, `.`]) {
					i++
				}
				if i > start {
					imports << source[start..i]
				}
				at_line_head = false
				continue
			}
			at_line_head = false
			i++
		}
	}
	return imports
}

fn source_imports_fast_parallel(a &flat.FlatAst, files []string) [][]string {
	if files.len < 2 || isnil(a.worker_pool) || a.worker_pool.size() == 0 {
		mut imports := [][]string{cap: files.len}
		for file in files {
			imports << source_imports_fast(file)
		}
		return imports
	}
	mut scans := []EagerSelfhostScanArgs{cap: files.len}
	mut tasks := []workers.Task{cap: files.len}
	for i, file in files {
		scans << EagerSelfhostScanArgs{
			path: file
		}
		tasks << workers.Task{
			run: eager_selfhost_scan_thread
			arg: unsafe { voidptr(&scans[i]) }
			force_sync: i == 0
		}
	}
	a.worker_pool.run(tasks)
	mut imports := [][]string{cap: files.len}
	for scan in scans {
		imports << scan.imports
	}
	return imports
}

// discover_eager_selfhost_modules resolves the complete trusted compiler import
// graph with a byte-level import scan. Parsing the resulting files in one batch
// avoids three parse/merge barriers while preserving the ordinary resolver as
// the source of truth for the resulting AST.
fn discover_eager_selfhost_modules(a &flat.FlatAst, prefs &pref.Preferences, first_file string, project_root string, mut parsed_modules map[string]bool, mut module_path_cache map[string]string) []EagerSelfhostModule {
	// Traversal attempts are separate from successfully parsed modules. An
	// unresolved eager probe must remain visible to the authoritative resolver.
	mut visited_modules := parsed_modules.clone()
	mut pending := []EagerSelfhostImport{cap: 128}
	mut cur_file := first_file
	for node in a.nodes {
		if node.kind == .file && node.value.len > 0 {
			cur_file = node.value
		} else if node.kind == .import_decl && node.value.len > 0 {
			pending << EagerSelfhostImport{
				path: node.value
				importing_file: cur_file
			}
		}
	}
	mut modules := []EagerSelfhostModule{cap: 64}
	mut module_by_real_dir := map[string]int{}
	mut qi := 0
	for qi < pending.len {
		wave_end := pending.len
		mut wave_requests := []EagerSelfhostImport{}
		for qi < wave_end {
			import_req := pending[qi]
			qi++
			if import_req.path in visited_modules {
				continue
			}
			visited_modules[import_req.path] = true
			wave_requests << import_req
		}
		wave_results := resolve_eager_selfhost_wave(a, prefs, wave_requests, project_root)
		mut wave_files := []string{}
		for i, result in wave_results {
			import_req := wave_requests[i]
			importing_dir := if import_req.importing_file.len > 0 {
				os.dir(import_req.importing_file)
			} else {
				''
			}
			if result.files.len == 0 {
				continue
			}
			module_path_cache['${importing_dir}\n${import_req.path}'] = result.dir
			parsed_modules[import_req.path] = true
			if result.real_dir in module_by_real_dir {
				module_idx := module_by_real_dir[result.real_dir]
				modules[module_idx].import_paths << import_req.path
				continue
			}
			module_by_real_dir[result.real_dir] = modules.len
			modules << EagerSelfhostModule{
				path: import_req.path
				dir: result.dir
				real_dir: result.real_dir
				files: result.files
				identity: result.identity
				import_paths: [import_req.path]
			}
			wave_files << result.files
		}
		wave_imports := source_imports_fast_parallel(a, wave_files)
		for i, file in wave_files {
			for imported in wave_imports[i] {
				pending << EagerSelfhostImport{
					path: imported
					importing_file: file
				}
			}
		}
	}
	// Alias-aware resolution is local to each request. Reconcile its short
	// identities in discovery order so distinct directories with the same module
	// suffix receive the same qualification as the authoritative resolver.
	mut identity_dirs := map[string]string{}
	for i in 0 .. modules.len {
		identity := modules[i].identity
		if owner_dir := identity_dirs[identity] {
			if owner_dir != modules[i].real_dir {
				modules[i].identity = modules[i].path
			}
		}
		identity_dirs[modules[i].identity] = modules[i].real_dir
	}
	return modules
}

fn resolve_imports(mut a flat.FlatAst, mut p parser.Parser, prefs &pref.Preferences, initial_files []string, allow_parallel bool, skip_closure_runtime bool, mut cache_state V3ModuleCacheState, mut parse_timing V3ParseTiming) bool {
	mut parsed_modules := map[string]bool{}
	parsed_modules['builtin'] = true
	parsed_modules['main'] = true
	explicit_initial_imports := imports_from_files(a, initial_files)
	canonicalize_colliding_initial_modules(mut a, prefs, initial_files, explicit_initial_imports)
	seed_initial_modules(a, initial_files, explicit_initial_imports, mut parsed_modules)

	// Backend modules excluded by the active configuration are never parsed: their
	// dispatch in main() is gated out by the matching `$if !skip_* ?`, so nothing
	// references their symbols. Pre-seeding parsed_modules makes the loop below treat
	// them as already handled, so neither v3.v's top-level imports nor any transitive
	// import pulls them in. Skipping the arm64 group (v3.gen.arm64 + the v3.ssa SSA
	// pipeline) and the wasm/eval backends avoids ~30k lines of work when self-hosting.
	for skipped_group in skipped_backend_module_groups(prefs) {
		mut group_requested := false
		for skipped in skipped_group {
			if skipped in explicit_initial_imports {
				group_requested = true
				break
			}
		}
		if group_requested {
			continue
		}
		for skipped in skipped_group {
			parsed_modules[skipped] = true
		}
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
	mut first_collision_seed_by_short := map[string]ImportCollisionSeed{}
	mut resolved_collision_seeds := map[string]bool{}
	mut unresolved_modules := map[string]bool{}
	mut cached_header_source_contexts := map[string]string{}
	bundle_import_file := cache_bundle_import_file(prefs.get_vlib_module_path('builtin'))
	if builtin_sources := cache_state.module_sources['builtin'] {
		if builtin_sources.len > 0 {
			builtin_header := cache_state.manager.entry('builtin', builtin_sources).header
			cached_header_source_contexts[builtin_header] = builtin_sources[0]
		}
	}
	mut was_parallel := false
	// Explicit self-hosting is faster through the normal incremental import loop:
	// eager discovery duplicates import-identity and collision work for this graph.
	if prefs.building_v && !prefs.selfhost && allow_parallel && !cache_state.manager.enabled
		&& os.getenv('V3_NO_EAGER_SELFHOST_IMPORTS') == '' {
		modules := discover_eager_selfhost_modules(a, prefs, first_file, project_root, mut parsed_modules, mut module_path_cache)
		mut eager_files := []string{}
		mut eager_canons := []string{}
		for module_info in modules {
			identity := module_info.identity
			for import_path in module_info.import_paths {
				parsed_module_identities[import_path] = identity
			}
			parsed_modules[identity] = true
			parsed_identity_dirs[identity] = module_info.dir
			cache_state.module_import_paths[identity] = if identity in module_info.import_paths {
				identity
			} else {
				module_info.path
			}
			cache_state.module_sources[identity] = module_info.files
			cache_state.parsed_from_source[identity] = true
			cache_state.source_body_modules[identity] = true
			for file in module_info.files {
				eager_files << file
				eager_canons << if identity == module_info.path { identity } else { '' }
			}
		}
		if eager_files.len > 0 {
			starts, eager_parallel := parse_files_dispatch_profiled(mut p, eager_files, allow_parallel, mut parse_timing)
			was_parallel = was_parallel || eager_parallel
			end_node := a.nodes.len
			for i, canon in eager_canons {
				if canon.len == 0 {
					continue
				}
				file_end := if i + 1 < starts.len { starts[i + 1] } else { end_node }
				canonicalize_imported_module_name(mut a, starts[i], file_end, canon)
			}
			// Imported code can be the first user of embed/channel/closure syntax.
			// Seed those compiler-provided modules before the authoritative resolver
			// scans the now-complete AST.
			seed_implicit_imports(mut a, skip_closure_runtime)
		}
	}

	mut cur_file := first_file
	mut cur_module := 'main'
	mut node_idx := 0
	// The implicit sync/embed_file/closure seeds are global-once: the serial loop added
	// each at the first module that needed it and never again. These flags carry
	// that "already seeded" state across module boundaries and waves. Within a
	// wave the synthetic nodes are only spliced in after every boundary has been
	// checked, so a later module's bounded already-imported scan cannot yet see an
	// earlier module's pending seed; the flags stand in for it.
	mut implicit_imports := ImplicitImportScan{
		node_idx: a.user_code_start
	}
	scan_implicit_imports(a, a.nodes.len, mut implicit_imports)
	mut synthetic_sync_added := implicit_imports.has_sync
	mut synthetic_embed_file_added := implicit_imports.has_embed_import
	mut synthetic_closure_added := implicit_imports.has_closure
	mut synthetic_debugger_added := implicit_imports.has_debugger
	mut ri_collision_ns := u64(0)
	mut ri_wave_ns := u64(0)
	mut ri_waves := 0
	mut pair_cursor := 0
	for {
		ri_waves++
		ri_t0 := time.sys_mono_now()
		scan_ids, next_pair_cursor := collect_import_scan_ids(a, node_idx, pair_cursor)
		pair_cursor = next_pair_cursor
		if os.getenv('V3_VERIFY_IMPORT_IDX') != '' {
			mut full := []int{}
			for i in node_idx .. a.nodes.len {
				if a.nodes[i].kind in [.file, .module_decl, .import_decl] {
					full << i
				}
			}
			if full.len != scan_ids.len {
				eprintln('IMPORT IDX MISMATCH: full ${full.len} fast ${scan_ids.len} region ${node_idx}..${a.nodes.len}')
				for i in full {
					if i !in scan_ids {
						eprintln('  missing id ${i} kind ${a.nodes[i].kind} value ${a.nodes[i].value}')
					}
				}
			} else {
				for k, fid in full {
					if scan_ids[k] != fid {
						eprintln('IMPORT IDX ORDER MISMATCH at ${k}: full ${fid} fast ${scan_ids[k]}')
						break
					}
				}
			}
		}
		// Decide short-name collisions for the whole visible import wave before
		// mutating any import node or parsing either module. This qualifies both
		// sides of `a.tast`/`b.tast`, avoiding an order-dependent state where the
		// first module is called `tast` and that semantic name is later mistaken
		// for the source alias of the second import.
		mut scan_file := cur_file
		for scan_idx in scan_ids {
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
			mut collision_seeds := [
				ImportCollisionSeed{
					path: scan_path
					importing_file: scan_importing_file
				},
			]
			if prefs.building_v {
				// Compiler-tree modules declare their path suffix as the module name.
				// A unique suffix cannot collide, so defer filesystem/identity work
				// until a second distinct dotted path with that suffix appears.
				short := scan_path.all_after_last('.')
				if first := first_collision_seed_by_short[short] {
					if first.path == scan_path {
						continue
					}
					first_key := '${first.importing_file}\x00${first.path}'
					if !resolved_collision_seeds[first_key] {
						collision_seeds.prepend(first)
					}
				} else {
					first_collision_seed_by_short[short] = collision_seeds[0]
					continue
				}
			}
			for seed in collision_seeds {
				seed_key := '${seed.importing_file}\x00${seed.path}'
				if resolved_collision_seeds[seed_key] {
					continue
				}
				resolved_collision_seeds[seed_key] = true
				scan_dir := resolve_project_or_pref_module_path_cached(prefs, seed.path, seed.importing_file, project_root, mut module_path_cache)
				scan_identity := import_module_identity_cached(prefs, seed.path, seed.importing_file, project_root, scan_dir, mut module_path_cache, mut module_identity_cache)
				if owner_path := identity_source_paths[scan_identity] {
					owner_dir := identity_source_dirs[scan_identity] or { '' }
					if owner_path != seed.path && owner_dir.len > 0 && scan_dir.len > 0
						&& os.is_dir(owner_dir) && os.is_dir(scan_dir)
						&& os.real_path(owner_dir) != os.real_path(scan_dir) {
						forced_full_module_paths[owner_path] = true
						forced_full_module_paths[seed.path] = true
					}
				} else {
					identity_source_paths[scan_identity] = seed.path
					identity_source_dirs[scan_identity] = scan_dir
				}
			}
		}
		ri_collision_ns += time.sys_mono_now() - ri_t0
		ri_t1 := time.sys_mono_now()
		// Collect one wave: every not-yet-parsed module imported by the nodes
		// scanned so far. Parsing appends at the end of the node array and the
		// scan proceeds in node order, so batching a wave and appending its
		// modules in discovery order reproduces the breadth-first module layout
		// the previous parse-one-module-inline loop produced — while giving the
		// parallel parser whole waves of files to split across threads.
		mut wave_files := []string{}
		mut wave_canon := []string{}
		mut wave_module_file_ends := []int{}
		for wave_scan_i in 0 .. scan_ids.len {
			node_idx = scan_ids[wave_scan_i]
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
				warmup_files := pref.get_v_files_from_dir_for_target(warmup_dir, prefs.user_defines, prefs.target)
				if cache_state.manager.valid_header(mod_name, warmup_files) == none {
					// The cached bundle may have been built while a project module
					// shadowed this optional warmup import. An actual user import was
					// already resolved above; do not rebuild just to warm an unused one.
					node_idx++
					continue
				}
			}
			if unresolved_modules[mod_name] {
				a.missing_imports[node_idx] = mod_name
			}
			if module_identity := parsed_module_identities[mod_name] {
				if module_identity.len > 0 {
					set_node_value_canonical(mut a, node_idx, module_identity)
				}
				record_v3_fallback_module_use(mut cache_state, module_identity, is_bundle_warmup_import)
				record_cache_module_dependency(mut cache_state, cur_module, module_identity)
				node_idx++
				continue
			}
			if mod_name in parsed_modules {
				record_v3_fallback_module_use(mut cache_state, mod_name, is_bundle_warmup_import)
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
				resolve_project_or_pref_module_path_cached(prefs, mod_name, importing_file, project_root, mut module_path_cache)
			}
			mut module_identity := import_module_identity_cached(prefs, mod_name, importing_file, project_root, mod_dir, mut module_path_cache, mut module_identity_cache)
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
			record_v3_fallback_module_use(mut cache_state, cache_module, is_bundle_warmup_import)
			record_cache_module_dependency(mut cache_state, cur_module, cache_module)
			mod_dir_exists := mod_dir.len > 0 && os.is_dir(mod_dir)
			mod_files := if mod_dir_exists {
				v3_directory_user_files(mod_dir, prefs, false, false) or {
					pref.get_v_files_from_dir_for_target(mod_dir, prefs.user_defines, prefs.target)
				}
			} else {
				[]string{}
			}
			module_resolved := mod_dir_exists && mod_files.len > 0
			if !module_resolved && !is_bundle_warmup_import {
				a.missing_imports[node_idx] = mod_name
				unresolved_modules[mod_name] = true
			}
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

			if module_resolved {
				// -building-v compiles the trusted compiler tree and already skips other
				// validity-only diagnostics. Avoid reading every imported source once here
				// just before the parser reads the same files.
				if !prefs.building_v
					&& !import_uses_explicit_module_alias(prefs, mod_name, importing_file, project_root) {
					expected_module := mod_name.all_after_last('.')
					for imported_file in mod_files {
						declared := declared_module_in_file(imported_file)
						// A source file without a module declaration (including an
						// entirely commented file) belongs to `main`.
						declared_module := if declared.len > 0 { declared } else { 'main' }
						if declared_module.all_after_last('.') != expected_module {
							message := 'bad module definition: ${importing_file} imports module "${mod_name}" but ${imported_file} is defined as module `${declared_module}`'
							eprintln('error: ${message}')
							formatted := v3errors.formatted_error('error:', message, a, flat.NodeId(node_idx), a.nodes[node_idx].pos)
							context := formatted.all_after_first('\n')
							if context.len > 0 {
								eprintln(context)
							}
							exit(1)
						}
					}
				}
				if cache_module !in cache_state.module_import_paths {
					cache_state.module_import_paths[cache_module] = mod_name
				}
				cache_state.module_sources[cache_module] = mod_files
				mut parse_files := mod_files.clone()
				is_builtin_bundle := module_is_builtin_bundle(cache_state, cache_module)
				if is_builtin_bundle {
					if cache_state.bundle_valid {
						if header := cache_state.manager.valid_header(cache_module, mod_files) {
							record_v3_cached_source_digests(mut cache_state, header.source_digests)
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
					if cached := cache_state.manager.valid_entry_with_metadata_cache(cache_module, mod_files, mut cache_state.dependency_metadata) {
						record_v3_cached_source_digests(mut cache_state, cached.source_digests)
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
		// The indexed walk leaves node_idx at the last visited id; the next
		// wave's region starts where this one's appends begin.
		node_idx = a.nodes.len
		ri_wave_ns += time.sys_mono_now() - ri_t1
		if wave_files.len == 0 {
			if prefs.verbose {
				ri_coll_ms := f64(ri_collision_ns) / 1e6
				ri_wave_ms := f64(ri_wave_ns) / 1e6
				eprintln('  [ttime]   ri collision   ${ri_coll_ms:7.2f} ms, wave scan ${ri_wave_ms:.2f} ms (waves: ${ri_waves})')
			}
			break
		}
		starts, wave_parallel := parse_files_dispatch_profiled(mut p, wave_files, allow_parallel, mut parse_timing)
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
					pos: region_end
					node: sync_import_node()
				}
				synthetic_sync_added = true
			}
			if !synthetic_embed_file_added && implicit_imports.needs_embed
				&& !implicit_imports.has_embed_import {
				insertions << SyntheticInsertion{
					pos: region_end
					node: embed_file_import_node()
				}
				synthetic_embed_file_added = true
			}
			if !skip_closure_runtime && !synthetic_closure_added && implicit_imports.needs_closure
				&& !implicit_imports.has_closure {
				insertions << SyntheticInsertion{
					pos: region_end
					node: closure_import_node()
				}
				synthetic_closure_added = true
			}
			if !synthetic_debugger_added && implicit_imports.needs_debugger
				&& !implicit_imports.has_debugger {
				insertions << SyntheticInsertion{
					pos: region_end
					node: debugger_import_node()
				}
				synthetic_debugger_added = true
			}
			module_start = module_file_end
		}
		insert_synthetic_imports(mut a, insertions)
		implicit_imports.node_idx += insertions.len
		implicit_imports.field_index_node_idx += insertions.len
	}
	return was_parallel
}

fn parse_files_dispatch_profiled(mut p parser.Parser, paths []string, allow_parallel bool, mut timing V3ParseTiming) ([]int, bool) {
	sw := time.new_stopwatch()
	starts, parallel := p.parse_files_dispatch(paths, allow_parallel)
	elapsed_us := sw.elapsed().microseconds()
	mut has_headers := false
	mut has_sources := false
	for path in paths {
		if path.ends_with('.vh') {
			has_headers = true
		} else {
			has_sources = true
		}
	}
	if !has_headers || !has_sources {
		if has_headers {
			timing.header_us += elapsed_us
			timing.header_parallel = timing.header_parallel || parallel
		} else {
			timing.source_us += elapsed_us
			timing.source_parallel = timing.source_parallel || parallel
		}
		return starts, parallel
	}
	mut header_weight := i64(0)
	mut source_weight := i64(0)
	for path in paths {
		size := i64(os.file_size(path))
		weight := if size > 0 { size } else { i64(1) }
		if path.ends_with('.vh') {
			header_weight += weight
		} else {
			source_weight += weight
		}
	}
	total_weight := header_weight + source_weight
	// A partially warm import wave can parse headers and sources concurrently.
	// Attribute that shared wall time by input size without changing AST parse order.
	header_us := if header_weight == 0 || total_weight == 0 {
		i64(0)
	} else if source_weight == 0 {
		elapsed_us
	} else {
		elapsed_us * header_weight / total_weight
	}
	timing.header_us += header_us
	timing.source_us += elapsed_us - header_us
	if parallel {
		timing.header_parallel = timing.header_parallel || header_weight > 0
		timing.source_parallel = timing.source_parallel || source_weight > 0
	}
	return starts, parallel
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

fn seed_initial_modules(a &flat.FlatAst, initial_files []string, explicit_imports map[string]bool, mut parsed_modules map[string]bool) {
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
		// A package can deliberately import a different package whose declared
		// short name matches its own (v.gen.wasm imports the top-level wasm module).
		// Do not let the initial package's seed suppress that explicit import.
		if module_name.len > 0 && module_name !in explicit_imports {
			parsed_modules[module_name] = true
		}
	}
}

fn canonicalize_colliding_initial_modules(mut a flat.FlatAst, prefs &pref.Preferences, initial_files []string, explicit_imports map[string]bool) {
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
		if module_name.len == 0 || module_name !in explicit_imports {
			continue
		}
		identity := initial_module_path_identity(prefs, file_node.value, module_name) or {
			continue
		}
		for child_idx in 0 .. file_node.children_count {
			child_id := int(a.child(&file_node, child_idx))
			if child_id >= 0 && child_id < a.nodes.len && a.nodes[child_id].kind == .module_decl {
				set_node_value_canonical(mut a, child_id, identity)
				break
			}
		}
	}
}

fn initial_module_path_identity(prefs &pref.Preferences, file string, module_name string) ?string {
	mut roots := []string{}
	if prefs.module_search_paths.len > 0 {
		roots << prefs.module_search_paths
	}
	roots << os.join_path_single(prefs.vroot, 'vlib')
	project_root := nearest_vmod_root_for_file(file)
	if project_root.len > 0 {
		roots << v3_directory_source_root(project_root)
	}
	file_dir := os.real_path(os.dir(file)).replace('\\', '/')
	mut seen := map[string]bool{}
	for root in roots {
		real_root := os.real_path(root).replace('\\', '/').trim_right('/')
		if real_root.len == 0 || seen[real_root] {
			continue
		}
		seen[real_root] = true
		prefix := real_root + '/'
		if !file_dir.starts_with(prefix) {
			continue
		}
		relative := file_dir[prefix.len..]
		if relative.contains('/') && relative.all_after_last('/') == module_name {
			return relative.replace('/', '.')
		}
	}
	return none
}

fn imports_from_files(a &flat.FlatAst, files []string) map[string]bool {
	mut selected_files := map[string]bool{}
	for file in files {
		selected_files[file] = true
		selected_files[os.real_path(file)] = true
	}
	mut imports := map[string]bool{}
	for file_idx, file_node in a.nodes {
		if file_idx < a.user_code_start || file_node.kind != .file || file_node.value.len == 0 {
			continue
		}
		if !selected_files[file_node.value] && !selected_files[os.real_path(file_node.value)] {
			continue
		}
		for i in 0 .. file_node.children_count {
			child := a.child_node(&file_node, i)
			if child.kind == .import_decl && child.value.len > 0 {
				imports[child.value] = true
			}
		}
	}
	return imports
}

fn parsed_files_import_linux_gg(a &flat.FlatAst, files []string) bool {
	imports := imports_from_files(a, files)
	return imports['gg'] || imports['sokol.sapp']
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
	// Module lookup depends on the importer's directory, not its filename. A
	// program commonly has many files in one directory importing the same module;
	// using the full filename here defeated almost every cache lookup.
	importing_dir := if importing_file.len > 0 { os.dir(importing_file) } else { '' }
	key := '${importing_dir}\n${import_path}\n${import_dir}'
	if identity := identity_cache[key] {
		return identity
	}
	identity := import_module_identity_with_path_cache(prefs, import_path, importing_file, project_root, import_dir, mut path_cache)
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
	short_dir := resolve_project_or_pref_module_path_cached(prefs, short_name, importing_file, project_root, mut path_cache)
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
	// Every resolution input derived from `importing_file` is directory scoped.
	// Share the result between sibling source files instead of walking all module
	// search roots once per file.
	importing_dir := if importing_file.len > 0 { os.dir(importing_file) } else { '' }
	key := '${importing_dir}\n${mod_name}'
	if path := cache[key] {
		return path
	}
	path := resolve_project_or_pref_module_path(prefs, mod_name, importing_file, project_root, mut cache)
	cache[key] = path
	return path
}

fn resolve_project_or_pref_module_path(prefs &pref.Preferences, mod_name string, importing_file string, project_root string, mut cache map[string]string) string {
	mod_path := mod_name.replace('.', os.path_separator)
	local_path := resolve_local_or_project_module_path(mod_name, mod_path, importing_file, project_root)
	if local_path.len > 0 {
		return local_path
	}
	// vlib and the global module directory do not depend on the importing file.
	// Resolve them once per import name instead of repeating the same alias probes
	// and directory scans for every module that imports them.
	global_key := '@global\n${mod_name}'
	if global_key in cache {
		global_path := cache[global_key]
		if global_path.len > 0 {
			return global_path
		}
	} else {
		global_path := resolve_global_module_path(prefs, mod_name, mod_path)
		cache[global_key] = global_path
		if global_path.len > 0 {
			return global_path
		}
	}
	return prefs.get_module_path(mod_name, importing_file)
}

fn resolve_local_or_project_module_path(mod_name string, mod_path string, importing_file string, project_root string) string {
	top_name := mod_name.all_before('.')
	if importing_file.len > 0 {
		importer_dir := os.dir(importing_file)
		if alias_path := resolve_local_module_alias_path(importer_dir, top_name, mod_name) {
			return alias_path
		}
		local_modules_root := os.join_path_single(importer_dir, 'modules')
		if alias_path := resolve_local_module_alias_path(local_modules_root, top_name, mod_name) {
			return alias_path
		}
		local_modules_path := os.join_path_single(local_modules_root, mod_path)
		if module_path_has_v_sources(local_modules_path) {
			return local_modules_path
		}
	}
	if project_root.len > 0 {
		if alias_path := resolve_local_module_alias_path(project_root, top_name, mod_name) {
			return alias_path
		}
		project_path := os.join_path_single(project_root, mod_path)
		if module_path_has_v_sources(project_path) {
			return project_path
		}
	}
	// Preserve the existing resolver priority: explicit local `modules/` and
	// project-root modules precede a module beside the importing file.
	if importing_file.len > 0 {
		relative_path := os.join_path_single(os.dir(importing_file), mod_path)
		if module_path_has_v_sources(relative_path) {
			return relative_path
		}
	}
	return ''
}

fn resolve_local_module_alias_path(root string, top_name string, mod_name string) ?string {
	// An alias for `a.b` can only exist below root/a. Avoid probing every
	// possible alias.v prefix for the overwhelmingly common missing local root.
	if !os.is_dir(os.join_path_single(root, top_name)) {
		return none
	}
	return pref.resolve_module_alias_path(root, mod_name)
}

fn import_uses_explicit_module_alias(prefs &pref.Preferences, mod_name string, importing_file string, project_root string) bool {
	mut roots := []string{}
	if importing_file.len > 0 {
		importer_dir := os.dir(importing_file)
		roots << importer_dir
		roots << os.join_path_single(importer_dir, 'modules')
	}
	if project_root.len > 0 {
		roots << project_root
		roots << os.join_path_single(project_root, 'modules')
	}
	if prefs.module_search_paths.len > 0 {
		roots << prefs.module_search_paths
	} else {
		roots << os.join_path_single(prefs.vroot, 'vlib')
		roots << os.vmodules_paths()
	}
	mut seen := map[string]bool{}
	for root in roots {
		real_root := os.real_path(root)
		if seen[real_root] {
			continue
		}
		seen[real_root] = true
		if _ := pref.resolve_module_alias_path(root, mod_name) {
			return true
		}
	}
	return false
}

fn resolve_global_module_path(prefs &pref.Preferences, mod_name string, mod_path string) string {
	search_roots := if prefs.module_search_paths.len > 0 {
		prefs.module_search_paths
	} else {
		mut roots := [os.join_path_single(prefs.vroot, 'vlib')]
		roots << os.vmodules_paths()
		roots
	}
	for root in search_roots {
		if alias_path := pref.resolve_module_alias_path(root, mod_name) {
			return alias_path
		}
		module_path := os.join_path_single(root, mod_path)
		if module_path_has_v_sources(module_path) {
			return module_path
		}
	}
	return ''
}

fn module_path_has_v_sources(path string) bool {
	if path.len == 0 || !os.is_dir(path) {
		return false
	}
	entries := os.ls(path) or { return false }
	return entries.any(it.ends_with('.v'))
}
