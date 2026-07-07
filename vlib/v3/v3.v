module main

import os
import v3.bench
import v3.flat
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

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

// run_compile_command supports run compile command handling for v3 entry point.
fn run_compile_command(cmd string) os.Result {
	exit_code := os.system(cmd)
	return os.Result{
		exit_code: exit_code
	}
}

fn tcc_atomic_s_arg(prefs &pref.Preferences) string {
	target_os := prefs.normalized_target_os()
	mut link_atomic_s := false
	match target_os {
		'macos' {
			// atomic.S has Mach-O-compatible aarch64 symbols, but its x86_64 Unix
			// stanza is ELF-only (`.type ... %function`). v3 does not yet track a
			// separate target architecture, so use the compiler build architecture.
			$if arm64 {
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
	return ' ${atomic_s}'
}

fn prepare_c_flags_for_link(flags []string, c99 bool) ![]string {
	support_flags := c_object_compile_support_flags(flags)
	mut prepared := []string{}
	for flag in flags {
		clean := flag.trim_space()
		if c_flag_is_object_file(clean) {
			prepared << ensure_c_object_file(clean, support_flags, c99)!
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

fn c_flags_need_objective_c(flags []string) bool {
	for flag in flags {
		clean := flag.trim_space()
		if clean in ['-fobjc-arc', '-fobjc-gc', '-ObjC'] || clean.starts_with('-fobjc-')
			|| clean == '-x objective-c' {
			return true
		}
	}
	return false
}

fn ensure_c_object_file(obj_path string, support_flags []string, c99 bool) !string {
	if os.exists(obj_path) {
		return obj_path
	}
	source_file := c_source_from_object_file(obj_path) or {
		return error('missing C object ${obj_path}, and no adjacent .c/.cpp/.S source was found')
	}
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	std_flag := if source_file.ends_with('.cpp') { '-std=c++11' } else { c_standard_flag(c99) }
	cached_obj := os.join_path(cache_dir, c_object_cache_name(obj_path, support_flags, std_flag))
	if os.exists(cached_obj)
		&& os.file_last_mod_unix(cached_obj) >= os.file_last_mod_unix(source_file) {
		return cached_obj
	}
	compiler := if source_file.ends_with('.cpp') { 'c++' } else { 'cc' }
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

fn c_object_cache_name(path string, support_flags []string, std_flag string) string {
	base := path.replace_each(['/', '_', '\\', '_', ':', '_', '.', '_', ' ', '_'])
	// The compile flags (`-D`/`-I`/...) change the object contents, so they must
	// be part of the cache key; otherwise a rebuild with different `#flag` defines
	// silently links the stale object built with the previous configuration.
	mut cache_flags := [std_flag]
	cache_flags << support_flags
	return '${base}_${c_flags_hash(cache_flags)}.o'
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

fn c_standard_flag(c99 bool) string {
	return if c99 { '-std=c99' } else { '-std=gnu11' }
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
	mut cmd := executable_command_for_path(bin_file)
	for arg in args {
		cmd += ' ' + os.quoted_path(arg)
	}
	if stderr_to_stdout {
		cmd += ' 2>&1'
	}
	return os.system(cmd)
}

fn executable_command_for_path(path string) string {
	mut run_path := path
	if !os.is_abs_path(path) && !path.contains('/') && !path.contains('\\') {
		run_path = '.' + os.path_separator + path
	}
	return os.quoted_path(run_path)
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

// main runs the v3 entry point.
fn main() {
	args := os.args[1..]
	if args.len == 0 {
		eprintln('usage: v3 [run] <file.v> [-o output|file.c] [-b c|arm64|eval] [-c99] [-d flag]')
		exit(1)
	}

	mut input_file := ''
	mut output_file := ''
	mut explicit_output := false
	mut backend := 'c'
	mut is_prod := false
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
		if args[i] == 'run' && input_file.len == 0 && !should_run {
			should_run = true
			i++
		} else if args[i] == '-o' && i + 1 < args.len {
			output_file = args[i + 1]
			explicit_output = true
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
		} else if args[i] in ['-gc', '-cc'] && i + 1 < args.len {
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
			i++
		} else if args[i].starts_with('-') {
			i++
		} else {
			input_file = args[i]
			i++
		}
	}
	if no_parallel {
		parallel_transform = false
	}

	if input_file == '' {
		eprintln('no input file')
		exit(1)
	}

	// Compiling v3 itself implies building_v: it uses no generics, so the monomorphization
	// pass is pure overhead. -building-v can force this for any input.
	if input_implies_building_v(input_file) {
		building_v = true
	}
	cmd_v_build := input_is_cmd_v(input_file)
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
	files << pref.get_v_files_from_dir(builtin_dir, builtin_defines, prefs.target_os)
	mut parse_was_parallel := false
	_, builtin_parse_parallel := p.parse_files_dispatch(files, !no_parallel)
	parse_was_parallel = parse_was_parallel || builtin_parse_parallel
	mut a := p.a
	a.user_code_start = a.nodes.len

	// Parse user input: single file or directory
	mut user_files := []string{}
	if input_file.ends_with('.v') {
		user_files << input_file
		user_files = expand_single_test_file_inputs(user_files, prefs)
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
	_, user_parse_parallel := p.parse_files_dispatch(user_files, !no_parallel)
	parse_was_parallel = parse_was_parallel || user_parse_parallel
	test_files := test_input_files(user_files, backend)

	seed_implicit_sync_import(mut a)
	seed_implicit_embed_file_import(mut a)

	// Resolve imports recursively
	import_parse_parallel := resolve_imports(mut a, mut p, prefs, user_files, !no_parallel)
	parse_was_parallel = parse_was_parallel || import_parse_parallel
	diagnostic_root := if is_selfhost {
		diagnostic_root_for_input(input_file, user_files)
	} else {
		''
	}

	b.step_parallel('parse', parse_was_parallel)

	// Type-collect + check BEFORE transform, so the transformer is type-aware
	// (like v2: check runs before transform). The transformer reads cached
	// per-expression types for type-dependent lowering.
	mut pre_tc := types.TypeChecker.new(a)
	pre_tc.reject_unsupported_generics = is_selfhost
	pre_tc.collect(a)
	pre_tc.diagnose_unknown_calls = true
	set_diagnostic_files(mut pre_tc, user_files)
	set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
	check_was_parallel := pre_tc.check_semantics_opt(parallel_transform)
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
	b.step_parallel('check', check_was_parallel)

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

	// Transform (match lowering, string/in lowering, etc.). Threaded transform is enabled
	// by default for compatible builds, and `-no-parallel` disables both threaded transform
	// and cgen.
	mut transform_was_parallel := false
	skip_transform_generics := building_v || cmd_v_build
	used_fns, transform_was_parallel = transform.transform_with_used_opt_config(mut a, &pre_tc,
		used_fns, parallel_transform, skip_transform_generics)
	b.step_parallel('transform', transform_was_parallel)

	// Reuse the pre-transform checker for metadata only. Transform does not add
	// declarations, and v1/v2 do not run a second semantic checker after lowering.
	pre_tc.diagnose_unknown_calls = false
	pre_tc.reject_unlowered_map_mutation = true
	set_diagnostic_files(mut pre_tc, user_files)
	set_unsupported_generic_files(mut pre_tc, a, is_selfhost, diagnostic_root)
	if !building_v && !cmd_v_build {
		pre_tc.annotate_types_with_used(used_fns)
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

	// Monomorphization only adds specialized generic instantiations to `used_fns`. The V
	// compiler sources use no generics, so when building V we skip specialization and
	// only erase generic templates that otherwise generate invalid raw C declarations.
	if building_v {
		used_fns = transform.erase_generic_templates(mut a, &pre_tc, used_fns)
	} else {
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
		c_standard := c_standard_flag(prefs.c99)
		mut g := cgen.FlatGen.new()
		g.set_c99_mode(prefs.c99)
		g.set_prealloc('prealloc' in prefs.user_defines)
		g.set_compiler_vexe(prefs.vexe)
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
		resolved_c_flags := prepare_c_flags_for_link(g.c_flags(), prefs.c99) or {
			eprintln(err.msg())
			exit(1)
		}
		c_flags := resolved_c_flags.join(' ')
		needs_objective_c := c_flags_need_objective_c(resolved_c_flags)
		cc_lang_flag := if needs_objective_c { '-x objective-c ' } else { '' }
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
		mut tried_tcc := false
		if !is_prod && !needs_objective_c {
			tried_tcc = true
			tcc_dir := os.join_path_single(os.join_path_single(prefs.vroot, 'thirdparty'), 'tcc')
			tcc_path := os.join_path_single(tcc_dir, 'tcc.exe')
			tcc_lib_dir := os.join_path_single(tcc_dir, 'lib')
			tcc_includes := '-I${os.join_path_single(tcc_lib_dir, 'include')}'
			tcc_lib := '-L${tcc_lib_dir}'
			atomic_s_arg := tcc_atomic_s_arg(prefs)
			cc_cmd = '${tcc_path} ${c_standard} ${tcc_includes} ${tcc_lib} ${warn_flags} -o ${bin_file} ${output_file}${atomic_s_arg} ${c_flags} -lm'
			exec_cmd = 'cd ${cc_dir} && ${tcc_path} ${c_standard} ${tcc_includes} ${tcc_lib} ${warn_flags} -o out src.c${atomic_s_arg} ${c_flags} -lm'
			println('  > ${cc_cmd}')
			result = run_compile_command(exec_cmd)
		}
		if is_prod || !tried_tcc || result.exit_code != 0 {
			if result.exit_code != 0 && result.output.len > 0 {
				eprintln('  tcc error: ${result.output.trim_space()}')
			}
			stack_flag := if prefs.normalized_target_os() == 'macos' {
				' -Wl,-stack_size,0x4000000'
			} else {
				''
			}
			if needs_objective_c {
				cc_cmd = 'cc ${c_standard} ${opt_flag}${warn_flags} -Wno-int-conversion${stack_flag} -o ${bin_file} -x objective-c ${output_file} -x none ${c_flags} -lm'
				exec_cmd = 'cd ${cc_dir} && cc ${c_standard} ${opt_flag}${warn_flags} -Wno-int-conversion${stack_flag} -o out -x objective-c src.c -x none ${c_flags} -lm'
			} else {
				cc_cmd = 'cc ${cc_lang_flag}${c_standard} ${opt_flag}${warn_flags} -Wno-int-conversion${stack_flag} -o ${bin_file} ${output_file} ${c_flags} -lm'
				exec_cmd = 'cd ${cc_dir} && cc ${cc_lang_flag}${c_standard} ${opt_flag}${warn_flags} -Wno-int-conversion${stack_flag} -o out src.c ${c_flags} -lm'
			}
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
	for file in pref.get_v_files_from_dir(dir, prefs.user_defines, prefs.target_os) {
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

fn seed_implicit_sync_import(mut a flat.FlatAst) {
	if ast_should_seed_sync_import(a, a.nodes.len, false) {
		a.add_node(sync_import_node())
	}
}

fn seed_implicit_embed_file_import(mut a flat.FlatAst) {
	if ast_should_seed_embed_file_import(a, a.nodes.len, false) {
		a.add_node(embed_file_import_node())
	}
}

// ast_should_seed_sync_import reports whether a synthetic `import sync` should be
// seeded for the module whose parsed region ends at end_node. The need and
// already-imported scans are both bounded to end_node — the module's serial
// boundary — so an explicit import in a *later* module of the same wave (already
// parsed into the array, but not yet reached in serial order) can neither suppress
// nor be conflated with this module's seed. already_added carries the "a synthetic
// import was already seeded for an earlier module in this wave" state the bounded
// scan cannot see, because the wave's synthetic nodes are spliced in only after
// every boundary has been checked; it keeps the seed global-once.
fn ast_should_seed_sync_import(a &flat.FlatAst, end_node int, already_added bool) bool {
	return !already_added && ast_needs_sync_import(a, end_node)
		&& !ast_has_import_upto(a, 'sync', end_node)
}

// ast_should_seed_embed_file_import bounds its scans like
// ast_should_seed_sync_import.
fn ast_should_seed_embed_file_import(a &flat.FlatAst, end_node int, already_added bool) bool {
	return !already_added && ast_needs_embed_file_import(a, end_node)
		&& !ast_has_import_upto(a, 'v.embed_file', end_node)
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

fn ast_needs_sync_import(a &flat.FlatAst, end_node int) bool {
	for i in 0 .. end_node {
		node := a.nodes[i]
		if node.kind == .lock_expr {
			return true
		}
		if node.kind == .field_decl && type_text_is_shared(node.typ) {
			return true
		}
		if node.kind == .struct_init && node.value.starts_with('chan ') {
			return true
		}
		if node.kind == .infix && node.op == .arrow {
			return true
		}
		if node.kind == .prefix && node.op == .arrow {
			return true
		}
		if type_text_is_channel(node.typ) {
			return true
		}
	}
	return false
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

fn ast_needs_embed_file_import(a &flat.FlatAst, end_node int) bool {
	for i in 0 .. end_node {
		node := a.nodes[i]
		if node.kind == .struct_init && node.value == 'embed_file.EmbedFileData' {
			return true
		}
	}
	return false
}

// ast_has_import_upto reports whether an import of name already exists among the
// first end_node nodes. The wave seeds bound this to a module's serial boundary
// so later modules parsed into the same wave cannot suppress an earlier module's
// synthetic import; synthetic nodes appended past that boundary are tracked by
// the caller's already_added flag instead.
fn ast_has_import_upto(a &flat.FlatAst, name string, end_node int) bool {
	for i in 0 .. end_node {
		node := a.nodes[i]
		if node.kind == .import_decl && node.value == name {
			return true
		}
	}
	return false
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
	mut synthetic_sync_added := false
	mut synthetic_embed_file_added := false
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
				mod_files := pref.get_v_files_from_dir(mod_dir, prefs.user_defines, prefs.target_os)
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
			if ast_should_seed_sync_import(a, region_end, synthetic_sync_added) {
				insertions << SyntheticInsertion{
					pos:  region_end
					node: sync_import_node()
				}
				synthetic_sync_added = true
			}
			if ast_should_seed_embed_file_import(a, region_end, synthetic_embed_file_added) {
				insertions << SyntheticInsertion{
					pos:  region_end
					node: embed_file_import_node()
				}
				synthetic_embed_file_added = true
			}
			module_start = module_file_end
		}
		insert_synthetic_imports(mut a, insertions)
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
