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
	objects                map[string]string
	headers                map[string]string
}

struct V3PreparedModuleCache {
	main_source string
	objects     []string
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
	misses                    int
	dependency_files          int
	dependency_scan_fallbacks int
	publish_races             int
	input_snapshot_races      int
	temporary_objects         []string
}

struct CObjectDependencies {
	files         []string
	used_fallback bool
}

fn prepare_c_flags_for_link(flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) ![]string {
	support_flags := c_object_compile_support_flags(flags)
	mut prepared := []string{}
	for flag in flags {
		clean := flag.trim_space()
		if c_flag_is_object_file(clean) {
			stats.requests++
			prepared << ensure_c_object_file(clean, support_flags, c99, pic_flag, target_args,
				target, c_compiler, uncached_dir, mut stats)!
		} else {
			prepared << flag
		}
	}
	return prepared
}

fn c_object_compile_flags(flags []string) []string {
	mut compile_flags := []string{}
	mut skip_link_operand := false
	for flag in flags {
		part := flag.trim_space()
		if skip_link_operand {
			skip_link_operand = false
			continue
		}
		if part in ['-l', '-L', '-Xlinker', '-framework', '-weak_framework', '-force_load'] {
			skip_link_operand = true
			continue
		}
		if part.len == 0 || c_flag_token_is_link_only(part) || c_flag_is_object_file(part)
			|| c_flag_is_c_source_file(part) {
			continue
		}
		compile_flags << flag
	}
	return compile_flags
}

fn c_object_compile_support_flags(flags []string) []string {
	return c_object_compile_flags(flags)
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

fn ensure_c_object_file(obj_path string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, uncached_dir string, mut stats CObjectCacheStats) !string {
	if os.exists(obj_path) {
		stats.direct_objects++
		return obj_path
	}
	source_file := c_source_from_object_file(obj_path) or {
		return error('missing C object ${obj_path}, and no adjacent .c/.cpp/.S source was found')
	}
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	std_flag := if source_file.ends_with('.cpp') { '-std=c++11' } else { c_standard_flag(c99) }
	compiler := if source_file.ends_with('.cpp') && c_compiler == 'cc' { 'c++' } else { c_compiler }
	mut args := [std_flag]
	args << target_args
	if pic_flag.len > 0 {
		args << pic_flag
	}
	args << '-w'
	args << support_flags
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
	cache_key := c_object_cache_name(obj_path, compiler, args, dependencies.files, target)
	cached_obj := os.join_path(cache_dir, cache_key)
	if os.exists(cached_obj) {
		stats.content_key_hits++
		trace_c_object_cache('hit', cache_key,
			'compiler, target, argv, and dependency contents matched', dependencies.files.len)
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
	post_key := c_object_cache_name(obj_path, compiler, key_args, dependencies.files, target)
	if post_key != cache_key {
		stats.input_snapshot_races++
		trace_c_object_cache('bypass', cache_key,
			'inputs changed during compilation; using build-local object', dependencies.files.len)
		uncached_obj := os.join_path(uncached_dir, 'input_snapshot_race_${os.getpid()}_${rand.ulid()}.o')
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
	return cached_obj
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
	for ext in ['.c', '.cpp', '.S'] {
		source_file := base + ext
		if os.exists(source_file) {
			return source_file
		}
	}
	return none
}

fn c_object_cache_name(path string, compiler string, compile_args []string, dependencies []string, target pref.Target) string {
	base := os.base(path).replace_each(['/', '_', '\\', '_', ':', '_', '.', '_', ' ', '_'])
	compiler_path := os.find_abs_path_of_executable(compiler) or { compiler }
	compiler_version := cmdexec.run(compiler, ['--version']).output
	mut hash := u64(1469598103934665603)
	for identity in [os.real_path(path), compiler_path, compiler_version, target.os, target.arch,
		target.abi, target.endian, target.pointer_bits.str(), target.object_format, compile_args.join('\x00')] {
		hash = c_hash_bytes(hash, identity.bytes())
	}
	for dependency in dependencies {
		canonical := os.real_path(dependency)
		hash = c_hash_bytes(hash, canonical.bytes())
		content := os.read_bytes(canonical) or { []u8{} }
		hash = c_hash_bytes(hash, content)
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
		'  -no-memory-limit             disable the 10 GiB RSS safety limit\n' +
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

// should_scope_prealloc_selfhost reports whether self-host stages need disposable arenas.
fn should_scope_prealloc_selfhost(building_v bool, cmd_v_build bool) bool {
	$if prealloc {
		return building_v || cmd_v_build
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

// clone_string_bool_map promotes a string-keyed set out of a disposable stage arena.
fn clone_string_bool_map(values map[string]bool) map[string]bool {
	mut cloned := map[string]bool{}
	for key, value in values {
		cloned[key.clone()] = value
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
	mut params := []string{cap: node.generic_params().len}
	for param in node.generic_params() {
		params << param.clone()
	}
	return flat.Node{
		value:          node.value.clone()
		typ:            node.typ.clone()
		payload:        flat.node_payload(params)
		is_mut:         node.is_mut
		children_start: node.children_start
		pos:            node.pos
		children_count: node.children_count
		kind:           node.kind
		op:             node.op
	}
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

fn promote_scoped_checker_node_additions(mut tc types.TypeChecker, base_nodes int, scope voidptr) {
	for idx in base_nodes .. tc.resolved_call_names.len {
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
		if idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
			tc.expr_type_values[idx] = types.clone_owned_type(tc.expr_type_values[idx])
		}
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
	mut c99 := false
	mut all_backends := false
	mut compile_backends := []string{}
	mut user_defines := []string{}
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
	scope_prealloc_selfhost := should_scope_prealloc_selfhost(building_v, cmd_v_build)
	// The selective transform promotion path is designed around worker-owned
	// result regions. Keep an explicitly serial transform in the compilation
	// arena while retaining scoped parse/check/cgen scratch.
	scope_prealloc_transform := scope_prealloc_selfhost && current_parallel_transform
	// Markused can lazily create the compilation worker pool. When parsing was
	// serial, keep that pool in the compilation arena so close_workers never
	// observes a pool allocated in a released markused scope.
	scope_prealloc_markused := scope_prealloc_selfhost && !current_no_parallel
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
	// The cache generator emits complete module bodies, including late generic
	// specializations that are not reachable from the entry program. Its split output
	// currently relies on the serial function-item walk to retain those definitions.
	cache_no_parallel_cgen := current_no_parallel || cache_manager.enabled
	mut p := parser.Parser.new(prefs)
	if scope_prealloc_selfhost {
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
	// Parsing workers own source buffers and private text tables. Canonicalize
	// once more after implicit imports/import waves so every surviving payload
	// uses the compilation table before semantic phases begin.
	p.release_source_storage()
	diagnostic_root := if is_selfhost {
		diagnostic_root_for_input(input_file, user_files)
	} else {
		''
	}

	b.step_parallel('parse', parse_was_parallel)
	b.metric('AST nodes after parse', a.nodes.len, 'nodes')
	b.metric('AST children after parse', a.children.len, 'edges')
	b.metric('canonical AST texts', a.text_count(), 'texts')
	b.metric('persistent worker threads', a.worker_count(), 'threads')

	// Type-collect + check BEFORE transform, so the transformer is type-aware
	// (like v2: check runs before transform). The transformer reads cached
	// per-expression types for type-dependent lowering.
	mut pre_tc := types.TypeChecker.new(a)
	if scope_prealloc_selfhost {
		pre_tc.enable_scoped_parallel_workers()
	}
	pre_tc.reject_unsupported_generics = is_selfhost
	set_diagnostic_files(mut pre_tc, user_files)
	pre_tc.collect(a)
	pre_tc.diagnose_unknown_calls = true
	pre_tc.prepare_threads_condition()
	set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
	check_was_parallel := pre_tc.check_semantics_opt(current_parallel_transform)
	if pre_tc.errors.len > 0 {
		print_type_errors(pre_tc.errors)
		exit(1)
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
		mut cache_input_modules := map[string]bool{}
		for module_name in cache_state.module_sources.keys() {
			cache_input_modules[module_name] = true
		}
		cache_input_modules['main'] = true
		external_inputs, has_untracked_c_include := cgen.cache_external_input_files(a, prefs.vroot,
			cache_input_modules, prefs.target)
		cache_state.module_external_inputs = external_inputs.clone()
		if has_untracked_c_include
			|| cache_external_inputs_have_static_storage(cache_state.module_external_inputs) {
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
	b.step_parallel('check', check_was_parallel)
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
	if scope_prealloc_markused {
		markused_scope = prealloc_scope_begin_for_v3()
		markused_tc = pre_tc.fork_for_parallel_transform(a)
		markused_tc.enable_scoped_parallel_workers()
	}
	mut output_used_fns := map[string]bool{}
	mut uses_generics := false
	if test_files.len > 0 {
		output_used_fns, uses_generics = markused.mark_used_for_tests_with_generic_usage(a,
			markused_tc, test_files)
	} else {
		output_used_fns, uses_generics = markused.mark_used_with_generic_usage(a, markused_tc)
	}
	mut used_fns := output_used_fns.clone()
	if cache_state.manager.enabled {
		mut cache_uses_generics := false
		used_fns, cache_uses_generics = markused.mark_used_for_cache_with_generic_usage(a,
			markused_tc, test_files, cache_state.source_body_modules)
		uses_generics = uses_generics || cache_uses_generics
	}
	if scope_prealloc_markused {
		prealloc_scope_leave_for_v3(markused_scope)
		output_used_fns = clone_string_bool_map(output_used_fns)
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
	mut transform_texts_canonical := false
	if !building_v && !cmd_v_build && !uses_generics && ast_contains_sql_expr(a) {
		uses_generics = true
	}
	// Markused distinguishes reachable generic calls/types from generic templates
	// that merely came along with an imported module (notably sync and rand).
	mut skip_transform_generics := building_v || cmd_v_build || !uses_generics
	if scope_prealloc_transform {
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
		used_fns, transform_was_parallel, transform_errors, scoped_owned_base_nodes, retained_transform_regions = transform.transform_with_used_opt_config_scoped_workers_checked_owned(mut a,
			&pre_tc, used_fns, current_parallel_transform, skip_transform_generics, true,
			transform_scope)
		parse_cache_enabled := pre_tc.type_cache_parse_enabled()
		prealloc_scope_leave_for_v3(transform_scope)
		retained_transform_regions = clone_scoped_transform_regions(retained_transform_regions)
		pre_tc.promote_scoped_transform_interners(base_type_count, base_symbol_count,
			transform_scope)
		if a.nodes.cap == reserved_nodes_cap && a.children.cap == reserved_children_cap {
			a.promote_transform_texts_from(base_text_count, transform_scope)
			if retained_transform_regions.len > 0 {
				outer_new_end := retained_transform_regions[0].new_start
				promote_scoped_ast_nodes(mut a, base_transform_nodes, outer_new_end,
					scoped_owned_base_nodes, transform_scope)
				last_worker_end := retained_transform_regions.last().new_end
				promote_scoped_ast_nodes(mut a, last_worker_end, a.nodes.len, []int{},
					transform_scope)
			} else {
				a.intern_node_texts_from(0)
				transform_texts_canonical = true
			}
		} else {
			a = clone_flat_ast_after_transform(a)
		}
		if a.specialized_fn_nodes.len != base_specialized_fns {
			a.specialized_fn_nodes = a.specialized_fn_nodes.clone()
		}
		promote_scoped_checker_node_additions(mut pre_tc, base_transform_nodes, transform_scope)
		promote_scoped_signatures(mut pre_tc, original_signature_names)
		used_fns = clone_string_bool_map(used_fns)
		transform_errors = clone_string_list(transform_errors)
		pre_tc.set_fresh_type_cache(parse_cache_enabled)
		prealloc_scope_free_for_v3(transform_scope)
		if retained_transform_regions.len > 0 {
			for region in retained_transform_regions {
				canonicalize_scoped_transform_region(mut a, region)
				prealloc_scope_free_for_v3(region.scope)
			}
			transform_texts_canonical = true
		}
	} else {
		used_fns, transform_was_parallel, transform_errors = transform.transform_with_used_opt_config_scoped_workers_checked(mut a,
			&pre_tc, used_fns, current_parallel_transform, skip_transform_generics, false)
	}
	if !building_v && !cmd_v_build && !uses_generics
		&& transformed_used_fns_need_monomorphize(used_fns) {
		uses_generics = true
		skip_transform_generics = false
	}
	b.step_parallel('transform', transform_was_parallel)
	if transform_errors.len > 0 {
		eprintln('type checker found ${transform_errors.len} error(s):')
		for message in transform_errors {
			eprintln(message)
		}
		exit(1)
	}
	pre_tc.freeze_interface_impl_names()
	b.metric('AST nodes after transform', a.nodes.len, 'nodes')
	b.metric('AST children after transform', a.children.len, 'edges')

	// Reuse the pre-transform checker for metadata only. Transform does not add
	// declarations, and v1/v2 do not run a second semantic checker after lowering.
	pre_tc.diagnose_unknown_calls = false
	pre_tc.reject_unlowered_map_mutation = true
	set_diagnostic_files(mut pre_tc, user_files)
	set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
	if !building_v && !cmd_v_build {
		if uses_generics {
			pre_tc.annotate_types_with_used(used_fns)
		} else {
			restore_transformed_fn_value_types(mut pre_tc, a, used_fns)
		}
	}
	b.step('annotate types')

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
	if building_v {
		used_fns = transform.erase_generic_templates(mut a, &pre_tc, used_fns)
	} else if uses_generics {
		mut monomorph_used_fns, monomorph_errors := transform.monomorphize_with_used_checked(mut a,
			&pre_tc, used_fns)
		used_fns = monomorph_used_fns.move()
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
	// Transform and monomorphization can synthesize or rewrite payload text.
	// They run with private/arena-backed worker state; publish only canonical,
	// compilation-owned strings after all worker merges are complete.
	if !transform_texts_canonical {
		a.intern_node_texts_from(0)
	}
	b.step('monomorphize')
	if cache_state.manager.enabled {
		// The cache transform roots complete modules, but the ordinary `.c` artifact
		// must retain normal dead-code elimination. Add only generated functions that
		// are reachable from the entry program; do not import the cache-only roots.
		post_transform_used := if test_files.len > 0 {
			markused.mark_used_for_tests(a, pre_tc, test_files)
		} else {
			markused.mark_used(a, pre_tc)
		}
		for name, is_used in post_transform_used {
			if is_used && (pre_tc.specialized_generic_fns[name] || name.starts_with('__v3_sum_eq_')) {
				output_used_fns[name] = true
			}
		}
	}
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
		cache_plan_file := if cache_state.manager.enabled {
			os.join_path_single(cc_dir, 'cache_plan.c')
		} else {
			''
		}
		mut generated_c_flags := []string{}
		mut cgen_was_parallel := false
		if scope_prealloc_selfhost {
			cgen_scope := prealloc_scope_begin_for_v3()
			mut g := cgen.FlatGen.new()
			g.set_c99_mode(prefs.c99)
			g.set_prealloc('prealloc' in prefs.user_defines)
			g.set_skip_generics(skip_transform_generics)
			g.set_compiler_vexe(prefs.vexe)
			g.set_target(prefs.target)
			g.set_cache_split(cache_state.manager.enabled)
			g.set_cache_program_files(user_files)
			g.set_scope_parallel_workers(true)
			generated_path := if cache_state.manager.enabled { cache_plan_file } else { cc_src }
			g.gen_to_file_with_used_test_options(generated_path, a, used_fns, &pre_tc,
				cache_no_parallel_cgen, test_files) or {
				eprintln('error writing ${generated_path}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			scoped_c_flags := g.c_flags()
			g.free_parallel_worker_scopes()
			if cache_state.manager.enabled {
				mut output_g := cgen.FlatGen.new()
				output_g.set_c99_mode(prefs.c99)
				output_g.set_prealloc('prealloc' in prefs.user_defines)
				output_g.set_skip_generics(skip_transform_generics)
				output_g.set_compiler_vexe(prefs.vexe)
				output_g.set_target(prefs.target)
				output_g.set_cache_program_files(user_files)
				output_g.set_scope_parallel_workers(true)
				output_g.gen_to_file_with_used_test_options(cc_src, a, output_used_fns, &pre_tc,
					cache_no_parallel_cgen, test_files) or {
					eprintln('error writing ${cc_src}: ${err}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				cgen_was_parallel = cgen_was_parallel || output_g.was_parallel()
				output_g.free_parallel_worker_scopes()
			}
			prealloc_scope_leave_for_v3(cgen_scope)
			generated_c_flags = clone_string_list(scoped_c_flags)
			prealloc_scope_free_for_v3(cgen_scope)
		} else {
			mut g := cgen.FlatGen.new()
			g.set_c99_mode(prefs.c99)
			g.set_prealloc('prealloc' in prefs.user_defines)
			g.set_skip_generics(skip_transform_generics)
			g.set_compiler_vexe(prefs.vexe)
			g.set_target(prefs.target)
			g.set_cache_split(cache_state.manager.enabled)
			g.set_cache_program_files(user_files)
			generated_path := if cache_state.manager.enabled { cache_plan_file } else { cc_src }
			g.gen_to_file_with_used_test_options(generated_path, a, used_fns, &pre_tc,
				cache_no_parallel_cgen, test_files) or {
				eprintln('error writing ${generated_path}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			generated_c_flags = g.c_flags()
			if cache_state.manager.enabled {
				mut output_g := cgen.FlatGen.new()
				output_g.set_c99_mode(prefs.c99)
				output_g.set_prealloc('prealloc' in prefs.user_defines)
				output_g.set_skip_generics(skip_transform_generics)
				output_g.set_compiler_vexe(prefs.vexe)
				output_g.set_target(prefs.target)
				output_g.set_cache_program_files(user_files)
				output_g.gen_to_file_with_used_test_options(cc_src, a, output_used_fns, &pre_tc,
					cache_no_parallel_cgen, test_files) or {
					eprintln('error writing ${cc_src}: ${err}')
					cleanup_c_build_dir(cc_dir)
					exit(1)
				}
				cgen_was_parallel = cgen_was_parallel || output_g.was_parallel()
			}
		}
		b.step_parallel('cgen', cgen_was_parallel)
		b.metric('generated C size', os.file_size(cc_src), 'bytes')
		if c_only {
			b.print_report()
			return
		}
		published_c := '${output_file}.tmp.${os.getpid()}.${rand.ulid()}'
		os.cp(cc_src, published_c) or {
			eprintln('failed to stage generated C output ${output_file}: ${err}')
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		os.mv(published_c, output_file) or {
			eprintln('failed to publish generated C output ${output_file}: ${err}')
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}

		pic_flag := shared_pic_flag(is_shared, prefs.normalized_target_os())
		target_args := c_compiler_target_args(prefs.target, c_compiler_explicit) or {
			eprintln(err.msg())
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		warn_args := if is_strict {
			['-Wall', '-Wextra', '-Werror=implicit-function-declaration', '-Wno-unused-variable',
				'-Wno-unused-parameter', '-Wno-int-conversion', '-Wno-missing-braces']
		} else {
			['-w']
		}
		resolved_c_flags := prepare_c_flags_for_link(generated_c_flags, prefs.c99, pic_flag,
			target_args, prefs.target, c_compiler, cc_dir, mut c_object_cache_stats) or {
			eprintln(err.msg())
			cleanup_c_build_dir(cc_dir)
			exit(1)
		}
		needs_objective_c := c_flags_need_objective_c(resolved_c_flags)
		mut cached_objects := []string{}
		if cache_state.manager.enabled {
			generated_source := os.read_file(cache_plan_file) or {
				eprintln('error reading cache-marked C source ${cache_plan_file}: ${err.msg()}')
				exit(1)
			}
			interface_impl_signature := pre_tc.interface_impl_set_signature()
			opt_flag := if is_prod { '-O2' } else { '' }
			warning_flags := warn_args.join(' ')
			prepared_cache := prepare_v3_module_cache(generated_source, c_standard, opt_flag,
				pic_flag, warning_flags, resolved_c_flags, needs_objective_c,
				interface_impl_signature, mut cache_state) or {
				eprintln(err.msg())
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			os.write_file(cc_src, prepared_cache.main_source) or {
				eprintln('error writing cached main source ${cc_src}: ${err.msg()}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cached_objects = prepared_cache.objects.clone()
			os.rm(cache_plan_file) or {}
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
		// Cached module objects can make tcc accept an unresolved call in the
		// program translation unit and emit a broken executable. Compile and link
		// the much smaller cached main unit with the system C compiler so the same
		// undeclared-function diagnostics remain enforced.
		if !is_prod && !needs_objective_c && target_args.len == 0 && !c_compiler_explicit
			&& !cache_state.manager.enabled {
			tried_tcc = true
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_lib_dir := os.join_path_single(tcc_dir, 'lib')
			tcc_includes := '-I${os.join_path_single(tcc_lib_dir, 'include')}'
			tcc_lib := '-L${tcc_lib_dir}'
			mut tcc_args := [c_standard]
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
			mut cc_args := []string{}
			cc_args << target_args
			cc_args << c_standard
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
			if needs_objective_c {
				cc_args << ['-x', 'objective-c', 'src.c', '-x', 'none']
			} else {
				cc_args << 'src.c'
			}
			cc_args << cached_objects
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
		os.rm(cc_src) or {}
		os.rmdir(cc_dir) or {}
		b.step('cc')
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
	b.metric('C object cache misses', c_object_cache_stats.misses, 'objects')
	b.metric('C object dependency files', c_object_cache_stats.dependency_files, 'files')
	b.metric('C object dep-scan fallbacks', c_object_cache_stats.dependency_scan_fallbacks,
		'objects')
	b.metric('C object publish races', c_object_cache_stats.publish_races, 'objects')
	b.metric('C object input-snapshot races', c_object_cache_stats.input_snapshot_races,
		'objects')
	b.print_report()
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

fn prepare_v3_module_cache(generated_source string, c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, interface_impl_signature string, mut state V3ModuleCacheState) !V3PreparedModuleCache {
	if !state.manager.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	split := modulecache.split_generated_c(generated_source)!
	declarations := modulecache.declaration_header(split.prefix)
	compile_signature := v3_cached_object_compile_signature(c_standard, opt_flag, pic_flag,
		warning_flags, generated_c_flags, objective_c, interface_impl_signature,
		modulecache.header_signature(declarations))
	if resolve_flag_specific_cache_objects(mut state, compile_signature) {
		os.setenv('V3_CACHE_FORCE_SOURCE', '1', true)
		restart_v3_after_cache_invalidation()
	}
	main_body := split.modules['main'] or { '' }
	main_source := split.prefix + main_body
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
		module_source := declarations + bundle_body.str()
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
	}
	unsafe { bundle_body.free() }

	mut parsed_modules := state.parsed_from_source.keys()
	parsed_modules.sort()
	for module_name in parsed_modules {
		if module_is_builtin_bundle(state, module_name) {
			continue
		}
		source_files := state.module_sources[module_name] or { continue }
		entry := state.manager.object_entry(module_name, source_files, compile_signature)
		body := split.modules[module_name] or {
			split.modules[module_name.all_after_last('.')] or { '' }
		}
		compile_v3_cached_object(entry, declarations + body, c_standard, opt_flag, pic_flag,
			warning_flags, generated_c_flags, objective_c)!
		if header := state.headers[module_name] {
			state.manager.write_header(module_name, source_files, header)!
		}
		dependencies := cache_object_dependency_signatures(state, [module_name])
		state.manager.write_stamp(module_name, source_files, dependencies, compile_signature)!
		object_paths[module_name] = entry.object
	}

	mut objects := []string{}
	mut object_names := object_paths.keys()
	object_names.sort()
	for name in object_names {
		path := object_paths[name]
		if path.len > 0 && path !in objects {
			objects << path
		}
	}
	return V3PreparedModuleCache{
		main_source: main_source
		objects:     objects
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
	// Every cached translation unit is compiled with the builtin bundle's declarations
	// prefix, even when its V module has no explicit builtin import.
	for module_name in cache_builtin_bundle_roots(state) {
		cache_add_module_header_signature(state, module_name, mut signatures)
	}
	mut input_modules := state.module_external_inputs.keys()
	input_modules.sort()
	for module_name in input_modules {
		for path in state.module_external_inputs[module_name] {
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

fn cache_external_inputs_have_static_storage(inputs map[string][]string) bool {
	for paths in inputs.values() {
		for path in paths {
			source := os.read_file(path) or { continue }
			if modulecache.c_source_has_static_storage(source) {
				return true
			}
		}
	}
	return false
}

fn v3_cached_object_compile_signature(c_standard string, opt_flag string, pic_flag string, warning_flags string, generated_c_flags []string, objective_c bool, interface_impl_signature string, declarations_signature string) string {
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
		'declarations=${declarations_signature}',
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
	}
	for name in tc.sum_generic_params.keys() {
		tc.sum_types.delete(name)
		tc.sum_generic_params.delete(name)
	}
	tc.invalidate_short_type_name_index()
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
	mut scan := ImplicitImportScan{}
	scan_implicit_imports(a, a.nodes.len, mut scan)
	if scan.needs_sync && !scan.has_sync {
		a.add_node(sync_import_node())
	}
	if scan.needs_embed && !scan.has_embed_import {
		a.add_node(embed_file_import_node())
	}
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
			new_nodes << insertions[ins_idx].node
			ins_idx++
		}
		new_nodes << a.nodes[i]
	}
	// Insertions at pos == old_len append at the very end (the last wave module's
	// region ends at the array tail).
	for ins_idx < insertions.len {
		new_nodes << insertions[ins_idx].node
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
				restart_v3_without_cache()
			}
			if module_identity := parsed_module_identities[mod_name] {
				if module_identity.len > 0 {
					a.nodes[node_idx].value = module_identity
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
				a.nodes[node_idx].value = module_identity
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
	for i in first_node .. end_node {
		if a.nodes[i].kind == .module_decl && a.nodes[i].value == short_name {
			a.nodes[i].value = import_path
		}
	}
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
		local_modules_path := os.join_path(os.dir(importing_file), 'modules', mod_name.replace('.',
			os.path_separator))
		if os.is_dir(local_modules_path) {
			return local_modules_path
		}
	}
	if project_root.len > 0 {
		project_path := os.join_path_single(project_root, mod_name.replace('.', os.path_separator))
		if os.is_dir(project_path) {
			return project_path
		}
	}
	return prefs.get_module_path(mod_name, importing_file)
}
