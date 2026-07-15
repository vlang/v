import os
import runtime
import strings
import v3.parser
import v3.pref

const pp_tests_dir = os.dir(@FILE)
const pp_v3_dir = os.dir(pp_tests_dir)
const pp_vlib_dir = os.dir(pp_v3_dir)
const pp_v3_src = os.join_path(pp_v3_dir, 'v3.v')

// parallel_parse_input_files gathers enough real compiler sources to cross the
// parallel-parse thresholds (min file count and total byte size).
fn parallel_parse_input_files() []string {
	mut files := []string{}
	for sub in ['types', 'transform', 'parser', 'markused', 'flat'] {
		dir := os.join_path(pp_v3_dir, sub)
		mut names := os.ls(dir) or { continue }
		names.sort()
		for name in names {
			if name.ends_with('.v') && !name.ends_with('_test.v') {
				files << os.join_path(dir, name)
			}
		}
	}
	return files
}

// test_parallel_parse_matches_serial parses the same file list serially and
// through the threaded dispatch and requires the resulting flat ASTs to be
// identical: same node fields, same children ids, same per-file region starts
// and same side tables. This pins the merge's id/offset shifting to the serial
// layout byte for byte.
fn test_parallel_parse_matches_serial() {
	files := parallel_parse_input_files()
	assert files.len >= 4
	prefs := pref.new_preferences()
	mut ps := parser.Parser.new(prefs)
	serial_starts := ps.parse_files_with_starts(files)
	mut pp := parser.Parser.new(prefs)
	parallel_starts, was_parallel := pp.parse_files_dispatch(files, true)
	$if !windows {
		if runtime.nr_jobs() > 1 {
			assert was_parallel
		}
	}
	assert parallel_starts == serial_starts
	assert pp.parsed_v_files == ps.parsed_v_files
	assert pp.a.nodes.len == ps.a.nodes.len
	assert pp.a.children.len == ps.a.children.len
	for i in 0 .. ps.a.children.len {
		assert int(pp.a.children[i]) == int(ps.a.children[i]), 'children[${i}] differs'
	}
	for i in 0 .. ps.a.nodes.len {
		s := ps.a.nodes[i]
		q := pp.a.nodes[i]
		assert q.kind == s.kind, 'node ${i} kind differs'
		assert q.kind_id == s.kind_id, 'node ${i} kind_id differs'
		assert q.value == s.value, 'node ${i} value differs'
		assert q.typ == s.typ, 'node ${i} typ differs'
		assert q.op == s.op, 'node ${i} op differs'
		assert q.is_mut == s.is_mut, 'node ${i} is_mut differs'
		assert q.generic_params == s.generic_params, 'node ${i} generic_params differs'
		assert q.pos.offset == s.pos.offset, 'node ${i} pos differs'
		assert q.children_count == s.children_count, 'node ${i} children_count differs'
		if s.children_count != 0 {
			assert q.children_start == s.children_start, 'node ${i} children_start differs'
		}
	}
	assert pp.a.disabled_fns.keys() == ps.a.disabled_fns.keys()
	assert pp.a.noreturn_fns.keys() == ps.a.noreturn_fns.keys()
	assert pp.a.export_fn_names.keys() == ps.a.export_fn_names.keys()
	for qname, value in ps.a.export_fn_names {
		assert pp.a.export_fn_names[qname] == value
	}
}

fn test_parallel_parse_seeds_cross_file_comptime_consts() {
	dir := os.join_path(os.temp_dir(), 'v3_parallel_const_seed_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mut files := []string{cap: 4}
	for file_index in 0 .. 4 {
		mut src := strings.new_builder(64_000)
		src.writeln('module main')
		src.writeln('')
		if file_index == 0 {
			src.writeln('\$if true &&')
			src.writeln("    @LINE == '4' {")
			src.writeln('\tconst line_position_gate = true')
			src.writeln('}')
			src.writeln('const enabled = true')
			src.writeln("const flavor = 'vanilla'")
			src.writeln("\$if r'enabled' == 'enabled' {")
			src.writeln('\tconst raw_string_gate = true')
			src.writeln('} \$else {')
			src.writeln('\tconst raw_string_gate_leak = true')
			src.writeln('}')
			src.writeln('\$if v3_parallel_taken_const ? {')
			src.writeln('\tconst branch_enabled = true')
			src.writeln('} \$else {')
			src.writeln('\tconst branch_enabled = false')
			src.writeln('}')
			src.writeln('\$if v3_parallel_untaken_const ? {')
			src.writeln('\tconst branch_leak = true')
			src.writeln('} \$else {')
			src.writeln('\tconst branch_fallback = true')
			src.writeln('}')
			src.writeln('\$match flavor {')
			src.writeln("\t'chocolate' { const match_branch_leak = true }")
			src.writeln("\t'vanilla' { const match_branch_enabled = true }")
			src.writeln('\t\$else { const match_branch_wrong_else = true }')
			src.writeln('}')
			src.writeln('\$match flavor {')
			src.writeln("\t'missing' { const match_fallback_leak = true }")
			src.writeln('\t\$else { const match_fallback_enabled = true }')
			src.writeln('}')
			src.writeln('@[if never ?]')
			src.writeln('const disabled = true')
			src.writeln('\$if future_enabled {')
			src.writeln('\tfn leaked_future_const() {}')
			src.writeln('} \$else {')
			src.writeln('\tfn absent_future_const() {}')
			src.writeln('}')
			src.writeln('')
		}
		if file_index == 1 {
			src.writeln('const enabled_alias = enabled')
			src.writeln('')
		}
		if file_index == 2 {
			src.writeln('const enabled_alias_2 = enabled_alias')
			src.writeln('')
		}
		if file_index == 3 {
			src.writeln('const future_enabled = true')
			src.writeln('\$if line_position_gate {')
			src.writeln('\tfn line_position_gate_enabled() {}')
			src.writeln('}')
			src.writeln('\$if raw_string_gate {')
			src.writeln('\tfn raw_string_gate_enabled() {}')
			src.writeln('}')
			src.writeln('\$if raw_string_gate_leak {')
			src.writeln('\tfn leaked_raw_string_gate() {}')
			src.writeln('}')
			src.writeln('\$if branch_enabled {')
			src.writeln('\tfn branch_const_enabled() {}')
			src.writeln('} \$else {')
			src.writeln('\tfn branch_const_disabled() {}')
			src.writeln('}')
			src.writeln('\$if branch_leak {')
			src.writeln('\tfn leaked_untaken_branch_const() {}')
			src.writeln('}')
			src.writeln('\$if branch_fallback {')
			src.writeln('\tfn fallback_branch_const_enabled() {}')
			src.writeln('}')
			src.writeln('\$if match_branch_enabled {')
			src.writeln('\tfn match_branch_const_enabled() {}')
			src.writeln('}')
			src.writeln('\$if match_branch_leak || match_branch_wrong_else || match_fallback_leak {')
			src.writeln('\tfn leaked_match_branch_const() {}')
			src.writeln('}')
			src.writeln('\$if match_fallback_enabled {')
			src.writeln('\tfn match_fallback_const_enabled() {}')
			src.writeln('}')
			src.writeln('\$if enabled {')
			src.writeln('\tfn enabled_branch() {}')
			src.writeln('} \$else {')
			src.writeln('\tfn disabled_branch() {}')
			src.writeln('}')
			src.writeln('\$match flavor {')
			src.writeln("\t'vanilla' { fn matched_branch() {} }")
			src.writeln('\t\$else { fn unmatched_branch() {} }')
			src.writeln('}')
			src.writeln('\$if disabled {')
			src.writeln('\tfn leaked_disabled_const() {}')
			src.writeln('} \$else {')
			src.writeln('\tfn absent_disabled_const() {}')
			src.writeln('}')
			src.writeln('\$if enabled_alias_2 {')
			src.writeln('\tfn alias_enabled_branch() {}')
			src.writeln('} \$else {')
			src.writeln('\tfn alias_disabled_branch() {}')
			src.writeln('}')
			src.writeln('')
		}
		for i in 0 .. 1000 {
			src.writeln('fn padding_${file_index}_${i}() int { return ${i} }')
		}
		path := os.join_path(dir, '${file_index}.v')
		os.write_file(path, src.str()) or { panic(err) }
		files << path
	}
	mut prefs := pref.new_preferences()
	prefs.user_defines << 'v3_parallel_taken_const'
	mut p := parser.Parser.new(prefs)
	_, was_parallel := p.parse_files_dispatch(files, true)
	$if !windows {
		if runtime.nr_jobs() > 1 {
			assert was_parallel
		}
	}
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'enabled_branch')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'line_position_gate_enabled')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'raw_string_gate_enabled')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'leaked_raw_string_gate')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'disabled_branch')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'matched_branch')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'unmatched_branch')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'leaked_disabled_const')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'absent_disabled_const')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'leaked_future_const')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'absent_future_const')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'alias_enabled_branch')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'alias_disabled_branch')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'branch_const_enabled')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'branch_const_disabled')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'leaked_untaken_branch_const')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'fallback_branch_const_enabled')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'match_branch_const_enabled')
	assert !p.a.nodes.any(it.kind == .fn_decl && it.value == 'leaked_match_branch_const')
	assert p.a.nodes.any(it.kind == .fn_decl && it.value == 'match_fallback_const_enabled')
}

// build_parallel_parser_v3 builds parallel parser v3 data for v3 tests.
fn build_parallel_parser_v3() string {
	vexe := @VEXE
	v3_bin := os.join_path(os.temp_dir(), 'v3_parallel_parser_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${pp_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${pp_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// write_parallel_parser_project writes a multi-module project large enough for
// the user-file and import waves to be split across parse workers.
fn write_parallel_parser_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	mut mod_totals := []int{}
	for m in 0 .. 4 {
		os.mkdir_all(os.join_path(project_dir, 'mod${m}')) or { panic(err) }
		mut mod_src := strings.new_builder(64_000)
		mod_src.writeln('module mod${m}')
		mod_src.writeln('')
		mut total := 0
		for i in 0 .. 900 {
			mod_src.writeln('pub fn value_${i}() int {')
			mod_src.writeln('\treturn ${i + m}')
			mod_src.writeln('}')
			mod_src.writeln('')
			total += i + m
		}
		mod_totals << total
		os.write_file(os.join_path(project_dir, 'mod${m}', 'mod${m}.v'), mod_src.str()) or {
			panic(err)
		}
	}
	mut main_src := strings.new_builder(16_000)
	main_src.writeln('module main')
	main_src.writeln('')
	for m in 0 .. 4 {
		main_src.writeln('import mod${m}')
	}
	main_src.writeln('')
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut total := 0')
	for m in 0 .. 4 {
		main_src.writeln('\ttotal += mod${m}.value_899()')
	}
	main_src.writeln('\tprintln(int_str(total))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	expected := 899 * 4 + 0 + 1 + 2 + 3
	os.write_file(os.join_path(project_dir, 'expected.txt'), '${expected}') or { panic(err) }
	return os.join_path(project_dir, 'main.v')
}

// test_parallel_parser_compiles_multi_module_project validates that a build
// whose import waves are parsed on worker threads produces a working program,
// and that the bench output reports the parse phase as parallel.
fn test_parallel_parser_compiles_multi_module_project() {
	v3_bin := build_parallel_parser_v3()
	main_path := write_parallel_parser_project('parallel_parser_project')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_parser_project_out_${os.getpid()}')
	compile := os.execute('VJOBS=4 ${v3_bin} ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	$if !windows {
		assert compile.output.contains('parse (parallel)'), compile.output
	}
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	expected := os.read_file(os.join_path(os.dir(main_path), 'expected.txt')) or { panic(err) }
	assert run.output.trim_space() == expected.trim_space()
}

// write_parallel_parser_implicit_sync_project writes a project where only an
// imported module needs the implicit `sync` import (a shared field + lock, no
// explicit import anywhere), so the seed fires for a module parsed inside a
// parallel import wave and `sync` must resolve with that module's context.
fn write_parallel_parser_implicit_sync_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(os.join_path(project_dir, 'locker')) or { panic(err) }
	// Padding modules keep the import wave above the parallel-parse thresholds.
	for m in 0 .. 3 {
		os.mkdir_all(os.join_path(project_dir, 'pad${m}')) or { panic(err) }
		mut pad_src := strings.new_builder(64_000)
		pad_src.writeln('module pad${m}')
		pad_src.writeln('')
		for i in 0 .. 1200 {
			pad_src.writeln('pub fn pad_value_${i}() int {')
			pad_src.writeln('\treturn ${i + m}')
			pad_src.writeln('}')
			pad_src.writeln('')
		}
		os.write_file(os.join_path(project_dir, 'pad${m}', 'pad${m}.v'), pad_src.str()) or {
			panic(err)
		}
	}
	os.write_file(os.join_path(project_dir, 'locker', 'locker.v'), 'module locker

pub struct Counter {
pub mut:
	value shared int
}

pub fn bump(mut c Counter) int {
	mut total := 0
	lock c.value {
		c.value += 3
		total = c.value
	}
	return total
}
') or {
		panic(err)
	}
	mut main_src := strings.new_builder(4_000)
	main_src.writeln('module main')
	main_src.writeln('')
	main_src.writeln('import locker')
	for m in 0 .. 3 {
		main_src.writeln('import pad${m}')
	}
	main_src.writeln('')
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut c := locker.Counter{}')
	main_src.writeln('\tmut total := locker.bump(mut c)')
	for m in 0 .. 3 {
		main_src.writeln('\ttotal += pad${m}.pad_value_1199()')
	}
	main_src.writeln('\tprintln(int_str(total))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	return os.join_path(project_dir, 'main.v')
}

// test_parallel_parser_seeds_implicit_sync_import_mid_wave validates that the
// implicit sync import still gets seeded (and resolved) when the module that
// needs it is parsed inside a parallel import wave rather than one at a time.
// Only the generated C is checked: linking a shared+lock project fails on
// master too (markused prunes RwMutex methods that only cgen-synthesized
// code calls), which is unrelated to import resolution.
fn test_parallel_parser_seeds_implicit_sync_import_mid_wave() {
	v3_bin := build_parallel_parser_v3()
	main_path := write_parallel_parser_implicit_sync_project('parallel_parser_implicit_sync')
	c_out := os.join_path(os.temp_dir(), 'v3_parallel_parser_implicit_sync_${os.getpid()}.c')
	compile := os.execute('VJOBS=4 ${v3_bin} ${main_path} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { panic(err) }
	// The sync module's own functions only appear when the synthetic
	// `import sync` was seeded for the mid-wave locker module and its files
	// were parsed in the next wave.
	assert c_code.contains('sync__Channel__close'), 'implicit sync import was not seeded/parsed'
}

// write_parallel_parser_implicit_before_explicit_sync_project writes a project
// where a module needing the implicit `sync` import (shared field + lock, no
// explicit import) is imported *before* another module that imports `sync`
// explicitly, so both land in the same import wave with the implicit one earlier
// in parse order. This is the edge case the per-module seed guards: the wave has
// already parsed the later module (and its explicit `import sync`) by the time
// the earlier module's boundary is reached, so a whole-array duplicate check
// would let that later explicit import suppress the earlier module's synthetic
// import — one serial resolution would have added first, before the later module
// existed. The seed's already-imported scan is bounded to the module's serial
// boundary to keep the earlier module resolving its own `sync` usage.
fn write_parallel_parser_implicit_before_explicit_sync_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(os.join_path(project_dir, 'implock')) or { panic(err) }
	os.mkdir_all(os.join_path(project_dir, 'explsync')) or { panic(err) }
	// Padding modules keep the import wave above the parallel-parse thresholds.
	for m in 0 .. 3 {
		os.mkdir_all(os.join_path(project_dir, 'pad${m}')) or { panic(err) }
		mut pad_src := strings.new_builder(64_000)
		pad_src.writeln('module pad${m}')
		pad_src.writeln('')
		for i in 0 .. 1200 {
			pad_src.writeln('pub fn pad_value_${i}() int {')
			pad_src.writeln('\treturn ${i + m}')
			pad_src.writeln('}')
			pad_src.writeln('')
		}
		os.write_file(os.join_path(project_dir, 'pad${m}', 'pad${m}.v'), pad_src.str()) or {
			panic(err)
		}
	}
	os.write_file(os.join_path(project_dir, 'implock', 'implock.v'), 'module implock

pub struct Counter {
pub mut:
	value shared int
}

pub fn bump(mut c Counter) int {
	mut total := 0
	lock c.value {
		c.value += 3
		total = c.value
	}
	return total
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(project_dir, 'explsync', 'explsync.v'), 'module explsync

import sync

pub fn make_wg() &sync.WaitGroup {
	return sync.new_waitgroup()
}
') or {
		panic(err)
	}
	mut main_src := strings.new_builder(4_000)
	main_src.writeln('module main')
	main_src.writeln('')
	// implock (implicit sync) is imported before explsync (explicit sync), so it
	// is the earlier module in the shared wave.
	main_src.writeln('import implock')
	main_src.writeln('import explsync')
	for m in 0 .. 3 {
		main_src.writeln('import pad${m}')
	}
	main_src.writeln('')
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut c := implock.Counter{}')
	main_src.writeln('\tmut total := implock.bump(mut c)')
	main_src.writeln('\twg := explsync.make_wg()')
	main_src.writeln('\t_ = wg')
	for m in 0 .. 3 {
		main_src.writeln('\ttotal += pad${m}.pad_value_1199()')
	}
	main_src.writeln('\tprintln(int_str(total))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	return os.join_path(project_dir, 'main.v')
}

// test_parallel_parser_seeds_implicit_sync_before_explicit_import validates that
// a module needing the implicit sync import still resolves it when a later module
// in the same parallel wave imports sync explicitly. The build must succeed (the
// earlier module's shared/lock lowers against sync) rather than dropping the seed
// because the later module's already-parsed explicit import was visible in the
// post-wave array. As with the mid-wave test, only the generated C is checked:
// linking a shared+lock project fails on master too (markused prunes the RwMutex
// methods that only cgen-synthesized code calls), which is unrelated here.
fn test_parallel_parser_seeds_implicit_sync_before_explicit_import() {
	v3_bin := build_parallel_parser_v3()
	main_path :=
		write_parallel_parser_implicit_before_explicit_sync_project('parallel_parser_implicit_before_explicit_sync')
	c_out := os.join_path(os.temp_dir(),
		'v3_parallel_parser_implicit_before_explicit_sync_${os.getpid()}.c')
	compile := os.execute('VJOBS=4 ${v3_bin} ${main_path} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { panic(err) }
	// The earlier module's `lock` lowered against sync, and the later module's
	// explicit sync usage is present too: both coexist after the per-module seed.
	assert c_code.contains('sync__RwMutex__lock'), 'implicit sync import did not resolve for the earlier module'
	assert c_code.contains('sync__new_waitgroup'), 'explicit sync import did not resolve for the later module'
}

// write_parallel_parser_splice_project writes a project where an early module
// needs an implicit import that a later same-wave module does not: `aembed` uses
// `$embed_file` (implicit `v.embed_file`) and is imported before `zsync`, which
// imports `sync` explicitly. The synthetic import is spliced in right after
// aembed's parsed region rather than at the wave tail, so the next resolver pass
// scans it before zsync's imports — the order serial resolution produced. That
// splice shifts every node after aembed's region and remaps the children table,
// so a working run of the program is what proves the remap kept the AST intact.
fn write_parallel_parser_splice_project(name string) (string, string) {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(os.join_path(project_dir, 'aembed')) or { panic(err) }
	os.mkdir_all(os.join_path(project_dir, 'zsync')) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'aembed', 'payload.txt'), 'embedded-payload') or {
		panic(err)
	}
	os.write_file(os.join_path(project_dir, 'aembed', 'aembed.v'), "module aembed

pub fn contents() string {
	f := \$embed_file('payload.txt')
	return f.to_string().trim_space()
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(project_dir, 'zsync', 'zsync.v'), 'module zsync

import sync

pub fn wg_count() int {
	mut wg := sync.new_waitgroup()
	wg.add(7)
	return 7
}
') or {
		panic(err)
	}
	// Padding modules keep the import wave above the parallel-parse thresholds.
	for m in 0 .. 3 {
		os.mkdir_all(os.join_path(project_dir, 'pad${m}')) or { panic(err) }
		mut pad_src := strings.new_builder(64_000)
		pad_src.writeln('module pad${m}')
		pad_src.writeln('')
		for i in 0 .. 1200 {
			pad_src.writeln('pub fn pad_value_${i}() int {')
			pad_src.writeln('\treturn ${i + m}')
			pad_src.writeln('}')
			pad_src.writeln('')
		}
		os.write_file(os.join_path(project_dir, 'pad${m}', 'pad${m}.v'), pad_src.str()) or {
			panic(err)
		}
	}
	mut main_src := strings.new_builder(4_000)
	main_src.writeln('module main')
	main_src.writeln('')
	// aembed (implicit v.embed_file) is imported before zsync (explicit sync).
	main_src.writeln('import aembed')
	main_src.writeln('import zsync')
	for m in 0 .. 3 {
		main_src.writeln('import pad${m}')
	}
	main_src.writeln('')
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut total := zsync.wg_count()')
	for m in 0 .. 3 {
		main_src.writeln('\ttotal += pad${m}.pad_value_5()')
	}
	main_src.writeln("\tprintln(aembed.contents() + ' ' + int_str(total))")
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	// 7 (wg) + (5+0) + (5+1) + (5+2) = 7 + 18 = 25.
	expected := 'embedded-payload 25'
	return os.join_path(project_dir, 'main.v'), expected
}

// test_parallel_parser_splices_synthetic_import_in_serial_order validates that a
// synthetic import seeded for an early wave module is spliced into the AST before
// the later wave modules — not appended at the tail — and that the resulting
// node/children shift leaves a runnable program. Unlike the shared+lock cases,
// an `$embed_file` project links, so the program is built and run end to end.
fn test_parallel_parser_splices_synthetic_import_in_serial_order() {
	v3_bin := build_parallel_parser_v3()
	main_path, expected := write_parallel_parser_splice_project('parallel_parser_splice')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_parser_splice_out_${os.getpid()}')
	compile := os.execute('VJOBS=4 ${v3_bin} ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == expected, run.output
}

// test_no_parallel_parser_keeps_parse_serial validates the runtime opt-out:
// `--no-parallel` must keep the parse phase off the worker threads.
fn test_no_parallel_parser_keeps_parse_serial() {
	v3_bin := build_parallel_parser_v3()
	main_path := write_parallel_parser_project('parallel_parser_serial_project')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_parser_serial_out_${os.getpid()}')
	compile := os.execute('VJOBS=4 ${v3_bin} --no-parallel ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('parse (parallel)'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
}
