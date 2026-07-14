module main

import os
import rand
import v3.bench
import v3.cmdexec
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

fn prepare_c_flags_for_link(flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string) ![]string {
	support_flags := c_object_compile_support_flags(flags)
	mut prepared := []string{}
	for flag in flags {
		clean := flag.trim_space()
		if c_flag_is_object_file(clean) {
			prepared << ensure_c_object_file(clean, support_flags, c99, pic_flag, target_args,
				target, c_compiler)!
		} else {
			prepared << flag
		}
	}
	return prepared
}

fn c_object_compile_support_flags(flags []string) []string {
	mut support := []string{}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean.len == 0 || c_flag_is_object_file(clean) || c_flag_is_c_source_file(clean)
			|| clean.starts_with('-l') {
			i++
			continue
		}
		if clean in ['-I', '-D', '-U', '-isystem', '-include', '-iquote', '-x'] {
			support << clean
			if i + 1 < flags.len {
				i++
				support << flags[i]
			}
			i++
			continue
		}
		if clean.starts_with('-I') || clean.starts_with('-D') || clean.starts_with('-U')
			|| clean.starts_with('-std') || clean.starts_with('-f') || clean.starts_with('-W')
			|| clean == '-pthread' {
			support << clean
		}
		i++
	}
	return support
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

fn ensure_c_object_file(obj_path string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string) !string {
	if os.exists(obj_path) {
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
	cached_obj := os.join_path(cache_dir, c_object_cache_name(obj_path, compiler, args,
		dependencies, target))
	if os.exists(cached_obj) {
		return cached_obj
	}
	temp_obj := '${cached_obj}.tmp.${os.getpid()}.${rand.ulid()}'
	args << ['-o', temp_obj, '-c', source_file]
	res := cmdexec.run(compiler, args)
	if res.exit_code != 0 {
		os.rm(temp_obj) or {}
		return error('failed to build C object ${obj_path} from ${source_file}:\n${res.output}')
	}
	os.mv(temp_obj, cached_obj) or {
		os.rm(temp_obj) or {}
		if !os.exists(cached_obj) {
			return error('failed to publish cached C object ${cached_obj}: ${err}')
		}
	}
	return cached_obj
}

fn c_object_dependencies(compiler string, compile_args []string, source_file string) []string {
	mut args := compile_args.clone()
	args << ['-M', '-MT', 'v3cache', source_file]
	result := cmdexec.run(compiler, args)
	if result.exit_code != 0 {
		return [source_file]
	}
	continuation := '\\' + '\n'
	dep_text := result.output.replace(continuation, ' ').all_after('v3cache:')
	mut dependencies := cmdexec.split_args(dep_text) or { []string{} }
	if source_file !in dependencies {
		dependencies << source_file
	}
	dependencies.sort()
	return dependencies
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
	return run_binary_impl(bin_file, args, false)
}

fn run_binary_with_stderr_to_stdout(bin_file string, args []string) int {
	return run_binary_impl(bin_file, args, true)
}

fn run_binary_impl(bin_file string, args []string, stderr_to_stdout bool) int {
	run_path := executable_path_for_run(bin_file)
	if stderr_to_stdout {
		result := cmdexec.run(run_path, args)
		print(result.output)
		return result.exit_code
	}
	mut process := os.new_process(run_path)
	process.set_args(args)
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

fn prealloc_scope_owns_for_v3(scope voidptr, ptr voidptr) bool {
	$if prealloc {
		return unsafe { prealloc_scope_owns(scope, ptr) }
	}
	return false
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

// clone_string_bool_map clones a string-to-bool map out of a scoped prealloc arena.
fn clone_string_bool_map(values map[string]bool) map[string]bool {
	mut cloned := map[string]bool{}
	for key, value in values {
		cloned[key.clone()] = value
	}
	return cloned
}

// clone_string_string_map clones a string-to-string map out of a scoped prealloc arena.
fn clone_string_string_map(values map[string]string) map[string]string {
	mut cloned := map[string]string{}
	for key, value in values {
		cloned[key.clone()] = value.clone()
	}
	return cloned
}

// clone_int_string_map clones an int-to-string map out of a scoped prealloc arena.
fn clone_int_string_map(values map[int]string) map[int]string {
	mut cloned := map[int]string{}
	for key, value in values {
		cloned[key] = value.clone()
	}
	return cloned
}

// clone_type_value clones a type value and any nested owned type metadata.
fn clone_type_value(value types.Type) types.Type {
	return match value {
		types.Void {
			types.Type(types.void_)
		}
		types.Unknown {
			types.Type(types.Unknown{
				reason: value.reason.clone()
			})
		}
		types.Primitive {
			types.Type(types.Primitive{
				props: value.props
				size:  value.size
			})
		}
		types.String {
			types.Type(types.string_)
		}
		types.Char {
			types.Type(types.char_)
		}
		types.Rune {
			types.Type(types.rune_)
		}
		types.ISize {
			types.Type(types.isize_)
		}
		types.USize {
			types.Type(types.usize_)
		}
		types.Nil {
			types.Type(types.nil_)
		}
		types.None {
			types.Type(types.none_)
		}
		types.Array {
			types.Type(types.Array{
				elem_type: clone_type_value(value.elem_type)
			})
		}
		types.ArrayFixed {
			types.Type(types.ArrayFixed{
				elem_type: clone_type_value(value.elem_type)
				len:       value.len
				len_expr:  value.len_expr.clone()
			})
		}
		types.Channel {
			types.Type(types.Channel{
				elem_type: clone_type_value(value.elem_type)
			})
		}
		types.Map {
			types.Type(types.Map{
				key_type:   clone_type_value(value.key_type)
				value_type: clone_type_value(value.value_type)
			})
		}
		types.Pointer {
			types.Type(types.Pointer{
				base_type: clone_type_value(value.base_type)
			})
		}
		types.FnType {
			types.Type(types.FnType{
				params:      clone_type_list(value.params)
				return_type: clone_type_value(value.return_type)
			})
		}
		types.OptionType {
			types.Type(types.OptionType{
				base_type: clone_type_value(value.base_type)
			})
		}
		types.ResultType {
			types.Type(types.ResultType{
				base_type: clone_type_value(value.base_type)
			})
		}
		types.Struct {
			types.Type(types.Struct{
				name: value.name.clone()
			})
		}
		types.Interface {
			types.Type(types.Interface{
				name: value.name.clone()
			})
		}
		types.Enum {
			types.Type(types.Enum{
				name:    value.name.clone()
				is_flag: value.is_flag
			})
		}
		types.SumType {
			types.Type(types.SumType{
				name: value.name.clone()
			})
		}
		types.Alias {
			types.Type(types.Alias{
				name:      value.name.clone()
				base_type: clone_type_value(value.base_type)
			})
		}
		types.MultiReturn {
			types.Type(types.MultiReturn{
				types: clone_type_list(value.types)
			})
		}
	}
}

// clone_type_list clones a type slice out of a scoped prealloc arena.
fn clone_type_list(values []types.Type) []types.Type {
	if values.len == 0 {
		return []types.Type{}
	}
	mut cloned := []types.Type{cap: values.len}
	for value in values {
		cloned << clone_type_value(value)
	}
	return cloned
}

// clone_type_array clones active sparse type entries out of a scoped prealloc arena.
fn clone_type_array(values []types.Type, set []bool) []types.Type {
	mut cloned := []types.Type{len: values.len, init: types.Type(types.void_)}
	for i, value in values {
		if i < set.len && set[i] {
			cloned[i] = clone_type_value(value)
		}
	}
	return cloned
}

// clone_string_type_map clones a string-to-type map out of a scoped prealloc arena.
fn clone_string_type_map(values map[string]types.Type) map[string]types.Type {
	mut cloned := map[string]types.Type{}
	for key, value in values {
		cloned[key.clone()] = clone_type_value(value)
	}
	return cloned
}

// clone_string_type_list_map clones function parameter type lists by function name.
fn clone_string_type_list_map(values map[string][]types.Type) map[string][]types.Type {
	mut cloned := map[string][]types.Type{}
	for key, value in values {
		cloned[key.clone()] = clone_type_list(value)
	}
	return cloned
}

// clone_int_type_map clones a sparse int-to-type map out of a scoped prealloc arena.
fn clone_int_type_map(values map[int]types.Type) map[int]types.Type {
	mut cloned := map[int]types.Type{}
	for key, value in values {
		cloned[key] = clone_type_value(value)
	}
	return cloned
}

fn string_is_in_prealloc_scope(value string, scope voidptr) bool {
	return value.len > 0 && prealloc_scope_owns_for_v3(scope, voidptr(value.str))
}

fn string_list_is_in_prealloc_scope(values []string, scope voidptr) bool {
	if values.len > 0 && prealloc_scope_owns_for_v3(scope, values.data) {
		return true
	}
	for value in values {
		if string_is_in_prealloc_scope(value, scope) {
			return true
		}
	}
	return false
}

fn promote_string_list_from_scope(values []string, scope voidptr) []string {
	if !string_list_is_in_prealloc_scope(values, scope) {
		return values
	}
	mut promoted := []string{cap: values.len}
	for value in values {
		promoted << if string_is_in_prealloc_scope(value, scope) { value.clone() } else { value }
	}
	return promoted
}

fn type_is_in_prealloc_scope(value types.Type, scope voidptr) bool {
	return match value {
		types.Unknown {
			string_is_in_prealloc_scope(value.reason, scope)
		}
		types.Array {
			type_is_in_prealloc_scope(value.elem_type, scope)
		}
		types.ArrayFixed {
			string_is_in_prealloc_scope(value.len_expr, scope)
				|| type_is_in_prealloc_scope(value.elem_type, scope)
		}
		types.Channel {
			type_is_in_prealloc_scope(value.elem_type, scope)
		}
		types.Map {
			type_is_in_prealloc_scope(value.key_type, scope)
				|| type_is_in_prealloc_scope(value.value_type, scope)
		}
		types.Pointer {
			type_is_in_prealloc_scope(value.base_type, scope)
		}
		types.FnType {
			if value.params.len > 0 && prealloc_scope_owns_for_v3(scope, value.params.data) {
				true
			} else if type_is_in_prealloc_scope(value.return_type, scope) {
				true
			} else {
				value.params.any(type_is_in_prealloc_scope(it, scope))
			}
		}
		types.OptionType {
			type_is_in_prealloc_scope(value.base_type, scope)
		}
		types.ResultType {
			type_is_in_prealloc_scope(value.base_type, scope)
		}
		types.Struct {
			string_is_in_prealloc_scope(value.name, scope)
		}
		types.Interface {
			string_is_in_prealloc_scope(value.name, scope)
		}
		types.Enum {
			string_is_in_prealloc_scope(value.name, scope)
		}
		types.SumType {
			string_is_in_prealloc_scope(value.name, scope)
		}
		types.Alias {
			string_is_in_prealloc_scope(value.name, scope)
				|| type_is_in_prealloc_scope(value.base_type, scope)
		}
		types.MultiReturn {
			if value.types.len > 0 && prealloc_scope_owns_for_v3(scope, value.types.data) {
				true
			} else {
				value.types.any(type_is_in_prealloc_scope(it, scope))
			}
		}
		else {
			false
		}
	}
}

fn promote_string_array_from_scope(mut values []string, scope voidptr) []string {
	if values.len > 0 && prealloc_scope_owns_for_v3(scope, values.data) {
		return clone_string_list(values)
	}
	for i, value in values {
		if string_is_in_prealloc_scope(value, scope) {
			values[i] = value.clone()
		}
	}
	return values
}

fn promote_type_array_from_scope(mut values []types.Type, set []bool, scope voidptr) []types.Type {
	if values.len > 0 && prealloc_scope_owns_for_v3(scope, values.data) {
		return clone_type_array(values, set)
	}
	for i, value in values {
		if i < set.len && set[i] && type_is_in_prealloc_scope(value, scope) {
			values[i] = clone_type_value(value)
		}
	}
	return values
}

// clone_flat_node clones the owned fields of one flat AST node.
fn clone_flat_node(node flat.Node) flat.Node {
	return flat.Node{
		value:          node.value.clone()
		typ:            node.typ.clone()
		generic_params: clone_string_list(node.generic_params)
		pos:            node.pos
		children_start: node.children_start
		children_count: node.children_count
		kind:           node.kind
		op:             node.op
		is_mut:         node.is_mut
	}
}

// clone_flat_ast clones the flat AST data that survives a disposable transform arena.
fn clone_flat_ast(ast &flat.FlatAst) &flat.FlatAst {
	mut nodes := []flat.Node{cap: ast.nodes.len}
	for node in ast.nodes {
		nodes << clone_flat_node(node)
	}
	mut children := []flat.NodeId{cap: ast.children.len}
	children << ast.children
	return &flat.FlatAst{
		nodes:           nodes
		children:        children
		user_code_start: ast.user_code_start
		disabled_fns:    clone_string_bool_map(ast.disabled_fns)
		export_fn_names: clone_string_string_map(ast.export_fn_names)
		noreturn_fns:    clone_string_bool_map(ast.noreturn_fns)
		source_files:    ast.source_files.clone()
	}
}

fn reserve_scoped_transform_storage(mut ast flat.FlatAst, mut tc types.TypeChecker, skip_generics bool) {
	nodes_factor_num, nodes_factor_den := if skip_generics { 7, 3 } else { 5, 3 }
	children_factor_num, children_factor_den := if skip_generics { 5, 2 } else { 7, 4 }
	target_nodes := ast.nodes.len * nodes_factor_num / nodes_factor_den
	target_children := ast.children.len * children_factor_num / children_factor_den
	if target_nodes > ast.nodes.cap {
		unsafe { ast.nodes.grow_cap(target_nodes - ast.nodes.cap) }
	}
	if target_children > ast.children.cap {
		unsafe { ast.children.grow_cap(target_children - ast.children.cap) }
	}
	tc.reserve_transform_node_caches(target_nodes)
}

fn promote_flat_ast_after_scoped_transform(ast &flat.FlatAst, scope voidptr) &flat.FlatAst {
	if (ast.nodes.len > 0 && prealloc_scope_owns_for_v3(scope, ast.nodes.data))
		|| (ast.children.len > 0 && prealloc_scope_owns_for_v3(scope, ast.children.data)) {
		return clone_flat_ast(ast)
	}
	mut promoted := unsafe { ast }
	for i in 0 .. promoted.nodes.len {
		unsafe {
			mut node := &promoted.nodes[i]
			if string_is_in_prealloc_scope(node.value, scope) {
				node.value = node.value.clone()
			}
			if string_is_in_prealloc_scope(node.typ, scope) {
				node.typ = node.typ.clone()
			}
			node.generic_params = promote_string_list_from_scope(node.generic_params, scope)
		}
	}
	promoted.disabled_fns = clone_string_bool_map(promoted.disabled_fns)
	promoted.export_fn_names = clone_string_string_map(promoted.export_fn_names)
	promoted.noreturn_fns = clone_string_bool_map(promoted.noreturn_fns)
	return promoted
}

// promote_typechecker_after_scoped_transform copies only semantic fields whose
// storage came from the disposable transform arena.
fn promote_typechecker_after_scoped_transform(mut tc types.TypeChecker, ast &flat.FlatAst, scope voidptr) {
	tc.a = ast
	// Transform can extend these maps even in generic-erasure self-host mode;
	// clone them as compact semantic tables rather than cloning the full checker.
	tc.fn_ret_types = clone_string_type_map(tc.fn_ret_types)
	tc.fn_param_types = clone_string_type_list_map(tc.fn_param_types)
	tc.fn_variadic = clone_string_bool_map(tc.fn_variadic)
	tc.resolved_call_names = promote_string_array_from_scope(mut tc.resolved_call_names, scope)
	if tc.resolved_call_set.len > 0 && prealloc_scope_owns_for_v3(scope, tc.resolved_call_set.data) {
		tc.resolved_call_set = tc.resolved_call_set.clone()
	}
	tc.resolved_fn_value_names = promote_string_array_from_scope(mut tc.resolved_fn_value_names,
		scope)
	if tc.resolved_fn_value_set.len > 0
		&& prealloc_scope_owns_for_v3(scope, tc.resolved_fn_value_set.data) {
		tc.resolved_fn_value_set = tc.resolved_fn_value_set.clone()
	}
	if tc.statement_nodes.len > 0 && prealloc_scope_owns_for_v3(scope, tc.statement_nodes.data) {
		tc.statement_nodes = tc.statement_nodes.clone()
	}
	if tc.expr_type_set.len > 0 && prealloc_scope_owns_for_v3(scope, tc.expr_type_set.data) {
		tc.expr_type_set = tc.expr_type_set.clone()
	}
	tc.expr_type_values = promote_type_array_from_scope(mut tc.expr_type_values, tc.expr_type_set,
		scope)
	if tc.checking_nodes.len > 0 && prealloc_scope_owns_for_v3(scope, tc.checking_nodes.data) {
		tc.checking_nodes = tc.checking_nodes.clone()
	}
	tc.sparse_resolved_call_names = clone_int_string_map(tc.sparse_resolved_call_names)
	tc.sparse_resolved_fn_values = clone_int_string_map(tc.sparse_resolved_fn_values)
	tc.sparse_statement_nodes = tc.sparse_statement_nodes.clone()
	tc.sparse_expr_type_values = clone_int_type_map(tc.sparse_expr_type_values)
	tc.sparse_checking_nodes = tc.sparse_checking_nodes.clone()
	tc.set_fresh_type_cache(tc.type_cache_parse_enabled())
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
	mut target_arch := pref.host_arch()
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
			i += 2
		} else if args[i] == '-arch' && i + 1 < args.len {
			target_arch = args[i + 1]
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
	mut p := parser.Parser.new(prefs)

	mut files := []string{}
	builtin_dir := builtin_dir_for_vroot(prefs.vroot)
	mut builtin_defines := prefs.user_defines.clone()
	if ownership_mode && 'ownership' !in builtin_defines {
		builtin_defines << 'ownership'
	}
	files << pref.get_v_files_from_dir_for_target(builtin_dir, builtin_defines, prefs.target)
	mut parse_was_parallel := false
	_, builtin_parse_parallel := p.parse_files_dispatch(files, !current_no_parallel)
	parse_was_parallel = parse_was_parallel || builtin_parse_parallel
	mut a := p.a
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

	// Resolve imports recursively
	import_parse_parallel := resolve_imports(mut a, mut p, prefs, user_files, !current_no_parallel)
	parse_was_parallel = parse_was_parallel || import_parse_parallel
	if p.diagnostics.len > 0 {
		for diagnostic in p.diagnostics {
			eprintln('${diagnostic.file}:${diagnostic.line}:${diagnostic.column}: error: ${diagnostic.message}')
		}
		exit(1)
	}
	diagnostic_root := if is_selfhost {
		diagnostic_root_for_input(input_file, user_files)
	} else {
		''
	}

	b.step_parallel('parse', parse_was_parallel)
	b.metric('AST nodes after parse', a.nodes.len, 'nodes')
	b.metric('AST children after parse', a.children.len, 'edges')

	// Type-collect + check BEFORE transform, so the transformer is type-aware
	// (like v2: check runs before transform). The transformer reads cached
	// per-expression types for type-dependent lowering.
	mut pre_tc := types.TypeChecker.new(a)
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
	b.step_parallel('check', check_was_parallel)
	b.metric('functions collected', pre_tc.fn_ret_types.len, 'symbols')
	b.metric('structs collected', pre_tc.structs.len, 'types')

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

	// Mark used functions (dead-code elimination). This is done before transform
	// so the transformer can skip function bodies that the C backend will prune.
	mut used_fns := map[string]bool{}
	mut uses_generics := false
	if test_files.len > 0 {
		used_fns, uses_generics = markused.mark_used_for_tests_with_generic_usage(a, pre_tc,
			test_files)
	} else {
		used_fns, uses_generics = markused.mark_used_with_generic_usage(a, pre_tc)
	}
	b.step('markused')
	b.metric('reachable symbols', used_fns.len, 'symbols')

	// Transform (match lowering, string/in lowering, etc.). Threaded transform is enabled
	// by default for compatible builds, and `-no-parallel` disables both threaded transform
	// and cgen.
	mut transform_was_parallel := false
	// Markused distinguishes reachable generic calls/types from generic templates
	// that merely came along with an imported module (notably sync and rand).
	skip_transform_generics := building_v || cmd_v_build || !uses_generics
	mut scope_whole_transform := !current_parallel_transform
	$if v3_no_parallel ? {
		scope_whole_transform = true
	}
	if scope_prealloc_selfhost && scope_whole_transform {
		// Reserve the escaping slabs in the compilation arena. The scoped
		// transform can then use a disposable scratch arena without forcing a
		// second full AST/checker copy at the end.
		reserve_scoped_transform_storage(mut a, mut pre_tc, skip_transform_generics)
		transform_scope := prealloc_scope_begin_for_v3()
		used_fns, transform_was_parallel = transform.transform_with_used_opt_config(mut a, &pre_tc,
			used_fns, current_parallel_transform, skip_transform_generics)
		prealloc_scope_leave_for_v3(transform_scope)
		a = promote_flat_ast_after_scoped_transform(a, transform_scope)
		used_fns = clone_string_bool_map(used_fns)
		promote_typechecker_after_scoped_transform(mut pre_tc, a, transform_scope)
		prealloc_scope_free_for_v3(transform_scope)
	} else {
		used_fns, transform_was_parallel = transform.transform_with_used_opt_config_scoped_workers(mut a,
			&pre_tc, used_fns, current_parallel_transform, skip_transform_generics,
			scope_prealloc_selfhost)
	}
	b.step_parallel('transform', transform_was_parallel)
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
	b.step('monomorphize')

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
			g.set_scope_parallel_workers(true)
			g.gen_to_file_with_used_test_options(cc_src, a, used_fns, &pre_tc, current_no_parallel,
				test_files) or {
				eprintln('error writing ${cc_src}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			scoped_c_flags := g.c_flags()
			g.free_parallel_worker_scopes()
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
			g.gen_to_file_with_used_test_options(cc_src, a, used_fns, &pre_tc, current_no_parallel,
				test_files) or {
				eprintln('error writing ${cc_src}: ${err}')
				cleanup_c_build_dir(cc_dir)
				exit(1)
			}
			cgen_was_parallel = g.was_parallel()
			generated_c_flags = g.c_flags()
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
			target_args, prefs.target, c_compiler) or {
			eprintln(err.msg())
			exit(1)
		}
		needs_objective_c := c_flags_need_objective_c(resolved_c_flags)
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
		if !is_prod && !needs_objective_c && target_args.len == 0 && !c_compiler_explicit {
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
		os.rm(cc_src) or {}
		os.rmdir(cc_dir) or {}
		b.step('cc')
		if should_run {
			run_result := run_binary_with_stderr_to_stdout(bin_file, run_args)
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

	b.print_report()
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

fn test_file_has_executable_top_level_stmt(a &flat.FlatAst, node flat.Node) bool {
	if node.kind != .file && node.kind != .block {
		return false
	}
	for i in 0 .. node.children_count {
		child_id := a.child(&node, i)
		if int(child_id) < a.user_code_start {
			continue
		}
		child := a.node(child_id)
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

fn test_file_is_executable_top_level_stmt(node flat.Node) bool {
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

fn collect_test_harness_decl_ids(a &flat.FlatAst, node flat.Node, mut ids []flat.NodeId) {
	if node.kind != .file && node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		child_id := a.child(&node, i)
		if int(child_id) < a.user_code_start {
			continue
		}
		child := a.node(child_id)
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
	if node.generic_params.len > 0 {
		return false
	}
	if test_harness_fn_param_count(a, node) != 0 {
		return false
	}
	return test_harness_fn_return_supported(tc.parse_type(node.typ))
}

fn is_supported_test_harness_hook(a &flat.FlatAst, tc &types.TypeChecker, node &flat.Node) bool {
	if node.generic_params.len > 0 {
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
fn resolve_imports(mut a flat.FlatAst, mut p parser.Parser, prefs &pref.Preferences, initial_files []string, allow_parallel bool) bool {
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
	mut module_path_cache := map[string]string{}
	mut module_identity_cache := map[string]string{}

	mut was_parallel := false
	mut cur_file := first_file
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
				node_idx++
				continue
			}
			if node.kind != .import_decl {
				node_idx++
				continue
			}
			mod_name := node.value
			if module_identity := parsed_module_identities[mod_name] {
				if module_identity.len > 0 {
					a.nodes[node_idx].value = module_identity
				}
				node_idx++
				continue
			}
			if mod_name in parsed_modules {
				node_idx++
				continue
			}

			importing_file := if cur_file.len > 0 { cur_file } else { first_file }
			mod_dir := resolve_project_or_pref_module_path_cached(prefs, mod_name, importing_file,
				project_root, mut module_path_cache)
			module_identity := import_module_identity_cached(prefs, mod_name, importing_file,
				project_root, mod_dir, mut module_path_cache, mut module_identity_cache)
			if module_identity.len > 0 {
				a.nodes[node_idx].value = module_identity
			}
			mod_dir_exists := mod_dir.len > 0 && os.is_dir(mod_dir)
			if mod_name in parsed_modules || (mod_dir_exists && module_identity in parsed_modules) {
				node_idx++
				continue
			}
			parsed_modules[mod_name] = true
			if mod_dir_exists && module_identity.len > 0 {
				parsed_modules[module_identity] = true
			}
			parsed_module_identities[mod_name] = if module_identity.len > 0 {
				module_identity
			} else {
				mod_name
			}

			if mod_dir_exists {
				mod_files := pref.get_v_files_from_dir_for_target(mod_dir, prefs.user_defines,
					prefs.target)
				canon := if module_identity == mod_name { mod_name } else { '' }
				for mf in mod_files {
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
	if project_root.len > 0 {
		project_path := os.join_path_single(project_root, mod_name.replace('.', os.path_separator))
		if os.is_dir(project_path) {
			return project_path
		}
	}
	return prefs.get_module_path(mod_name, importing_file)
}
