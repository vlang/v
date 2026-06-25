module main

import os
import v3.bench
import v3.eval
import v3.flat
import v3.gen.arm64
import v3.gen.c as cgen
import v3.gen.wasm
import v3.markused
import v3.parser
import v3.pref
import v3.ssa
import v3.ssa.optimize
import v3.transform
import v3.types

// run_compile_command supports run compile command handling for v3 entry point.
fn run_compile_command(cmd string) os.Result {
	exit_code := os.system(cmd)
	return os.Result{
		exit_code: exit_code
	}
}

fn prepare_c_flags_for_link(flags []string) ![]string {
	support_flags := c_object_compile_support_flags(flags)
	mut prepared := []string{}
	for flag in flags {
		clean := flag.trim_space()
		if c_flag_is_object_file(clean) {
			prepared << ensure_c_object_file(clean, support_flags)!
		} else {
			prepared << flag
		}
	}
	return prepared
}

fn c_object_compile_support_flags(flags []string) []string {
	mut support := []string{}
	for flag in flags {
		clean := flag.trim_space()
		if clean.len == 0 || c_flag_is_object_file(clean) || c_flag_is_c_source_file(clean)
			|| clean.starts_with('-l') {
			continue
		}
		if clean.starts_with('-I') || clean.starts_with('-D') || clean.starts_with('-U')
			|| clean.starts_with('-std') || clean.starts_with('-f') || clean.starts_with('-W')
			|| clean == '-pthread' {
			support << clean
		}
	}
	return support
}

fn ensure_c_object_file(obj_path string, support_flags []string) !string {
	if os.exists(obj_path) {
		return obj_path
	}
	source_file := c_source_from_object_file(obj_path) or {
		return error('missing C object ${obj_path}, and no adjacent .c/.cpp/.S source was found')
	}
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	cached_obj := os.join_path(cache_dir, c_object_cache_name(obj_path, support_flags))
	if os.exists(cached_obj)
		&& os.file_last_mod_unix(cached_obj) >= os.file_last_mod_unix(source_file) {
		return cached_obj
	}
	compiler := if source_file.ends_with('.cpp') { 'c++' } else { 'cc' }
	std_flag := if source_file.ends_with('.cpp') { '-std=c++11' } else { '-std=gnu11' }
	cmd := '${compiler} ${std_flag} -w ${support_flags.join(' ')} -o ${os.quoted_path(cached_obj)} -c ${os.quoted_path(source_file)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error('failed to build C object ${obj_path} from ${source_file}:\n${res.output}')
	}
	return cached_obj
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

fn c_object_cache_name(path string, support_flags []string) string {
	base := path.replace_each(['/', '_', '\\', '_', ':', '_', '.', '_', ' ', '_'])
	// The compile flags (`-D`/`-I`/...) change the object contents, so they must
	// be part of the cache key; otherwise a rebuild with different `#flag` defines
	// silently links the stale object built with the previous configuration.
	if support_flags.len == 0 {
		return base + '.o'
	}
	return '${base}_${c_flags_hash(support_flags)}.o'
}

fn c_flags_hash(flags []string) string {
	mut h := u64(1469598103934665603)
	joined := flags.join(' ')
	for c in joined.bytes() {
		h = (h ^ u64(c)) * u64(1099511628211)
	}
	return h.hex()
}

fn c_flag_is_object_file(flag string) bool {
	return flag.ends_with('.o') || flag.ends_with('.obj')
}

fn c_flag_is_c_source_file(flag string) bool {
	return flag.ends_with('.c') || flag.ends_with('.cc') || flag.ends_with('.cpp')
}

// main runs the v3 entry point.
fn main() {
	args := os.args[1..]
	if args.len == 0 {
		eprintln('usage: v3 <file.v> [-o output|file.c] [-b c|arm64|eval] [-d flag]')
		exit(1)
	}

	mut input_file := ''
	mut output_file := ''
	mut backend := 'c'
	mut is_prod := false
	mut is_strict := false
	mut is_selfhost := false
	mut no_parallel := false
	mut parallel_transform := false
	mut building_v := false
	mut all_backends := false
	mut compile_backends := []string{}
	mut user_defines := []string{}
	mut i := 0
	for i < args.len {
		if args[i] == '-o' && i + 1 < args.len {
			output_file = args[i + 1]
			i += 2
		} else if args[i] == '-b' && i + 1 < args.len {
			backend = args[i + 1]
			i += 2
		} else if args[i] == '-prod' {
			is_prod = true
			i++
		} else if args[i] == '-selfhost' {
			is_selfhost = true
			i++
		} else if args[i] == '-building-v' || args[i] == '-building_v' {
			// The V compiler itself uses no generics, so monomorphization (and the rest
			// of the generics machinery) is pure overhead when building it.
			building_v = true
			i++
		} else if args[i] == '-strict' {
			is_strict = true
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
		} else if args[i] in ['-gc', '-cc'] && i + 1 < args.len {
			i += 2
		} else if args[i] in ['-prealloc', '-enable-globals'] {
			i++
		} else if args[i].starts_with('-') {
			i++
		} else {
			input_file = args[i]
			i++
		}
	}

	if input_file == '' {
		eprintln('no input file')
		exit(1)
	}

	// Compiling the V compiler itself (v3.v) implies building_v: it uses no generics, so
	// the monomorphization pass is pure overhead. -building-v can force this for any input.
	if input_file.all_after_last('/') == 'v3.v' {
		building_v = true
	}

	mut bin_file := ''
	mut c_only := false
	if output_file == '' {
		bin_file = input_file.all_before_last('.v')
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
	prefs.backend = backend
	prefs.user_defines = user_defines
	prefs.vroot = resolve_vroot(prefs.vroot)
	prefs.selfhost = is_selfhost
	prefs.building_v = building_v
	mut p := parser.Parser.new(prefs)

	mut files := []string{}
	builtin_dir := builtin_dir_for_vroot(prefs.vroot)
	files << pref.get_v_files_from_dir(builtin_dir, prefs.user_defines, prefs.target_os)
	p.parse_files(files)
	mut a := p.a
	a.user_code_start = a.nodes.len

	// Parse user input: single file or directory
	mut user_files := []string{}
	if input_file.ends_with('.v') {
		user_files << input_file
	} else if os.is_dir(input_file) {
		user_files = pref.get_v_files_from_dir(input_file, prefs.user_defines, prefs.target_os)
		for subdir in vmod_subdirs(input_file) {
			subdir_path := os.join_path_single(input_file, subdir)
			user_files << pref.get_v_files_from_dir(subdir_path, prefs.user_defines,
				prefs.target_os)
		}
	} else {
		user_files << input_file
	}
	for uf in user_files {
		p.parse_into(uf)
	}
	test_files := test_input_files(user_files, backend)

	// Resolve imports recursively
	resolve_imports(mut a, mut p, prefs, user_files)
	diagnostic_root := if is_selfhost {
		diagnostic_root_for_input(input_file, user_files)
	} else {
		''
	}

	b.step('parse')

	// Type-collect + check BEFORE transform, so the transformer is type-aware
	// (like v2: check runs before transform). The transformer reads cached
	// per-expression types for type-dependent lowering.
	mut pre_tc := types.TypeChecker.new(a)
	pre_tc.reject_unsupported_generics = is_selfhost
	pre_tc.collect(a)
	pre_tc.diagnose_unknown_calls = true
	set_diagnostic_files(mut pre_tc, user_files)
	set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
	pre_tc.check_semantics()
	if pre_tc.errors.len > 0 {
		print_type_errors(pre_tc.errors)
		exit(1)
	}
	test_harness_errors := validate_test_file_harness_inputs(a, pre_tc, test_files)
	if test_harness_errors.len > 0 {
		for msg in test_harness_errors {
			eprintln(msg)
		}
		exit(1)
	}
	b.step('check')

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
	mut used_fns := if test_files.len > 0 {
		markused.mark_used_for_tests(a, pre_tc, test_files)
	} else {
		markused.mark_used(a, pre_tc)
	}
	b.step('markused')

	// Transform (match lowering, string/in lowering, etc.). Parallel transform is an
	// explicit opt-in (`-parallel-transform`), independent of `-no-parallel` (which
	// gates the parallel C codegen): the two phases can be threaded independently.
	mut transform_was_parallel := false
	used_fns, transform_was_parallel = transform.transform_with_used_opt(mut a, &pre_tc, used_fns,
		parallel_transform)
	b.step_parallel('transform', transform_was_parallel)

	// Reuse the pre-transform checker for metadata only. Transform does not add
	// declarations, and v1/v2 do not run a second semantic checker after lowering.
	pre_tc.diagnose_unknown_calls = false
	pre_tc.reject_unlowered_map_mutation = true
	set_diagnostic_files(mut pre_tc, user_files)
	set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
	pre_tc.annotate_types()
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

	// Monomorphization only adds specialized generic instantiations to `used_fns`. The V
	// compiler sources use no generics, so when building V we skip the pass entirely
	// (`used_fns` passes through unchanged) instead of scanning the whole AST for nothing.
	if !building_v {
		used_fns = transform.monomorphize_with_used(mut a, &pre_tc, used_fns)
	}
	b.step('monomorphize')

	if backend == 'arm64' {
		$if !skip_arm64 ? {
			// SSA + ARM64 native backend
			mut m := ssa.build_with_used(a, used_fns, pre_tc)
			b.step('ssa build')

			if is_prod {
				optimize.optimize(mut m)
				b.step('optimize')
			}

			mut g := arm64.Gen.new(m)
			g.gen()
			b.step('arm64 gen')

			g.write_and_link(bin_file)
			b.step('link')
		}
	} else {
		// C backend (default)
		mut g := cgen.FlatGen.new()
		c_code := g.gen_with_used_test_options(a, used_fns, &pre_tc, no_parallel, test_files)
		if !write_text_file_raw(output_file, c_code) {
			eprintln('error writing ${output_file}')
			exit(1)
		}
		b.step_parallel('cgen', g.was_parallel())
		if c_only {
			b.print_report()
			return
		}

		opt_flag := if is_prod { '-O2 ' } else { '' }
		warn_flags := if is_strict {
			'-Wall -Wextra -Werror=implicit-function-declaration -Wno-unused-variable -Wno-unused-parameter -Wno-int-conversion -Wno-missing-braces'
		} else {
			'-w'
		}
		resolved_c_flags := prepare_c_flags_for_link(g.c_flags()) or {
			eprintln(err.msg())
			exit(1)
		}
		c_flags := resolved_c_flags.join(' ')
		// Compile inside a per-output build dir, using constant relative source/output basenames,
		// then move the result to bin_file. On macOS arm64 tcc bakes the -o basename into the
		// ad-hoc code-signature identifier and the input .c path into the symbol table, so building
		// `v5.c`->`v5` vs `v6.c`->`v6` directly would make the binaries differ only by those embedded
		// names (plus the code-directory hashes covering them). Compiling fixed `src.c`->`out` keeps
		// those embedded names identical, so the self-host chain is byte-for-byte reproducible
		// (v5 == v6). The build dir is unique per output and never embedded (we cd into it), so
		// parallel compilations into a shared directory never clobber each other.
		cc_dir := '${bin_file}.v3cc'
		os.mkdir(cc_dir) or {}
		cc_src := os.join_path_single(cc_dir, 'src.c')
		cc_out := os.join_path_single(cc_dir, 'out')
		if !write_text_file_raw(cc_src, c_code) {
			eprintln('error writing ${cc_src}')
			exit(1)
		}
		mut cc_cmd := ''
		mut exec_cmd := ''
		mut result := os.Result{}
		if !is_prod {
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_lib_dir := os.join_path_single(tcc_dir, 'lib')
			tcc_includes := '-I${os.join_path_single(tcc_lib_dir, 'include')}'
			tcc_lib := '-L${tcc_lib_dir}'
			cc_cmd = '${tcc_path} ${tcc_includes} ${tcc_lib} ${warn_flags} -o ${bin_file} ${output_file} ${c_flags} -lm'
			exec_cmd = 'cd ${cc_dir} && ${tcc_path} ${tcc_includes} ${tcc_lib} ${warn_flags} -o out src.c ${c_flags} -lm'
			println('  > ${cc_cmd}')
			result = run_compile_command(exec_cmd)
		}
		if is_prod || result.exit_code != 0 {
			if result.exit_code != 0 && result.output.len > 0 {
				eprintln('  tcc error: ${result.output.trim_space()}')
			}
			stack_flag := if prefs.normalized_target_os() == 'macos' {
				' -Wl,-stack_size,0x4000000'
			} else {
				''
			}
			cc_cmd = 'cc -std=gnu11 ${opt_flag}${warn_flags} -Wno-int-conversion${stack_flag} -o ${bin_file} ${output_file} ${c_flags} -lm'
			exec_cmd = 'cd ${cc_dir} && cc -std=gnu11 ${opt_flag}${warn_flags} -Wno-int-conversion${stack_flag} -o out src.c ${c_flags} -lm'
			println('  > ${cc_cmd}')
			result = run_compile_command(exec_cmd)
			if result.exit_code != 0 {
				eprintln('C compilation failed:')
				eprintln(result.output)
				exit(1)
			}
		}
		os.mv(cc_out, bin_file) or {
			eprintln('failed to finalize ${bin_file}: ${err}')
			exit(1)
		}
		os.rm(cc_src) or {}
		os.rmdir(cc_dir) or {}
		b.step('cc')
	}

	b.print_report()
}

fn vmod_subdirs(dir string) []string {
	vmod_path := os.join_path_single(dir, 'v.mod')
	content := os.read_file(vmod_path) or { return []string{} }
	subdirs_pos := content.index('subdirs:') or { return []string{} }
	after_subdirs := content[subdirs_pos..]
	lb_rel := after_subdirs.index_u8(`[`)
	if lb_rel < 0 {
		return []string{}
	}
	after_lb := after_subdirs[lb_rel + 1..]
	rb_rel := after_lb.index_u8(`]`)
	if rb_rel < 0 {
		return []string{}
	}
	raw_items := after_lb[..rb_rel].split(',')
	mut subdirs := []string{}
	for raw in raw_items {
		item := raw.trim_space().trim('\'"')
		if item.len > 0 {
			subdirs << item
		}
	}
	return subdirs
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

// resolve_vroot resolves resolve vroot information for v3 entry point.
fn resolve_vroot(initial string) string {
	if is_valid_vroot(initial) {
		return initial
	}
	mut dir := os.getwd()
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
	return initial
}

// is_valid_vroot reports whether is valid vroot applies in v3 entry point.
fn is_valid_vroot(root string) bool {
	return root.len > 0 && os.is_dir(builtin_dir_for_vroot(root))
}

// builtin_dir_for_vroot supports builtin dir for vroot handling for v3 entry point.
fn builtin_dir_for_vroot(root string) string {
	return os.join_path_single(os.join_path_single(root, 'vlib'), 'builtin')
}

// write_text_file_raw writes text file raw output for v3 entry point.
fn write_text_file_raw(path string, data string) bool {
	// Delegate to the stdlib writer so the open flags (O_CREAT/O_TRUNC, binary mode)
	// are correct on every platform, instead of hardcoding per-OS bit values.
	os.write_file(path, data) or { return false }
	return true
}

// print_type_errors updates print type errors state for v3 entry point.
fn print_type_errors(errors []types.TypeError) {
	eprintln('type checker found ${errors.len} error(s):')
	max_errors := if errors.len < 20 { errors.len } else { 20 }
	for ei in 0 .. max_errors {
		eprintln('  ${errors[ei].msg}')
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

fn test_input_files(user_files []string, backend string) []string {
	mut files := []string{}
	for file in user_files {
		if pref.is_test_file_for_backend(file, backend) {
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
		if module_name.len > 0 && module_name != 'main' {
			errors << 'no runnable tests in ${file_node.value}: only module main test files are supported'
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
		.for_in_stmt, .if_expr, .match_stmt, .assert_stmt {
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
	if test_harness_fn_param_count(a, node) != 0 {
		return false
	}
	return test_harness_fn_return_supported(tc.parse_type(node.typ))
}

fn is_supported_test_harness_hook(a &flat.FlatAst, tc &types.TypeChecker, node &flat.Node) bool {
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

// resolve_imports resolves resolve imports information for v3 entry point.
fn resolve_imports(mut a flat.FlatAst, mut p parser.Parser, prefs &pref.Preferences, initial_files []string) {
	mut parsed_modules := map[string]bool{}
	parsed_modules['builtin'] = true
	parsed_modules['main'] = true

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

	mut changed := true
	for changed {
		changed = false
		mut cur_file := first_file
		scan_len := a.nodes.len
		for node_idx in 0 .. scan_len {
			node := a.nodes[node_idx]
			if node.kind == .file && node.value.len > 0 {
				cur_file = node.value
				continue
			}
			if node.kind != .import_decl {
				continue
			}
			mod_name := node.value
			if mod_name in parsed_modules {
				continue
			}
			parsed_modules[mod_name] = true
			changed = true

			importing_file := if cur_file.len > 0 { cur_file } else { first_file }
			mod_dir := resolve_project_or_pref_module_path(prefs, mod_name, importing_file,
				project_root)
			if mod_dir == '' || !os.is_dir(mod_dir) {
				continue
			}
			module_identity := import_module_identity(prefs, mod_name, importing_file,
				project_root, mod_dir)
			mod_files := pref.get_v_files_from_dir(mod_dir, prefs.user_defines, prefs.target_os)
			for mf in mod_files {
				first_node := a.nodes.len
				p.parse_into(mf)
				if module_identity == mod_name {
					canonicalize_imported_module_name(mut a, first_node, mod_name)
				}
			}
		}
	}
	normalize_import_module_identities(mut a, prefs, first_file, project_root)
}

fn canonicalize_imported_module_name(mut a flat.FlatAst, first_node int, import_path string) {
	if import_path.len == 0 {
		return
	}
	short_name := import_path.all_after_last('.')
	for i in first_node .. a.nodes.len {
		if a.nodes[i].kind == .module_decl && a.nodes[i].value == short_name {
			a.nodes[i].value = import_path
		}
	}
}

fn normalize_import_module_identities(mut a flat.FlatAst, prefs &pref.Preferences, first_file string, project_root string) {
	mut cur_file := first_file
	for i in 0 .. a.nodes.len {
		node := a.nodes[i]
		if node.kind == .file && node.value.len > 0 {
			cur_file = node.value
			continue
		}
		if node.kind != .import_decl {
			continue
		}
		importing_file := if cur_file.len > 0 { cur_file } else { first_file }
		mod_dir := resolve_project_or_pref_module_path(prefs, node.value, importing_file,
			project_root)
		a.nodes[i].value = import_module_identity(prefs, node.value, importing_file, project_root,
			mod_dir)
	}
}

fn import_module_identity(prefs &pref.Preferences, import_path string, importing_file string, project_root string, import_dir string) string {
	if !import_path.contains('.') {
		return import_path
	}
	short_name := import_path.all_after_last('.')
	short_dir := resolve_project_or_pref_module_path(prefs, short_name, importing_file,
		project_root)
	if short_dir.len > 0 && import_dir.len > 0 && os.is_dir(short_dir)
		&& os.real_path(short_dir) != os.real_path(import_dir) {
		return import_path
	}
	return short_name
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
