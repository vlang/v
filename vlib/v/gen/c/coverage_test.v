module c

import os
import v.flat
import v.token

fn test_coverage_json_escape_handles_every_control_character() {
	for code in 0 .. 32 {
		raw := [u8(code)].bytestr()
		expected := match code {
			8 { '\\b' }
			9 { '\\t' }
			10 { '\\n' }
			12 { '\\f' }
			13 { '\\r' }
			else { '\\u00${code:02x}' }
		}
		assert coverage_json_escape(raw) == expected
	}
	assert coverage_json_escape('quote" slash\\') == 'quote\\" slash\\\\'
}

fn test_coverage_points_keep_one_based_source_lines() {
	source := 'first\nsecond\nthird\n'
	path := os.join_path(os.temp_dir(), 'v3_coverage_source_${os.getpid()}.v')
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut ast := flat.FlatAst.new()
	// The AST owns this File pointer for the lifetime of the test fixture.
	unsafe {
		ast.source_files[1] = file
	}
	mut g := FlatGen.new()
	g.a = &ast
	g.cur_fn_name = 'main.main'
	g.coverage_dir = os.join_path(os.temp_dir(), 'v3_coverage_${os.getpid()}')
	second_offset := source.index('second') or { panic('missing second line') }
	third_offset := source.index('third') or { panic('missing third line') }
	g.write_coverage_point(flat.Node{
		kind: .expr_stmt
		pos:  token.new_pos(1, 0)
	})
	g.write_coverage_point(flat.Node{
		kind: .expr_stmt
		pos:  token.new_pos(1, second_offset)
	})
	g.write_coverage_point(flat.Node{
		kind: .expr_stmt
		pos:  token.new_pos(1, second_offset + 1)
	})
	g.write_coverage_point(flat.Node{
		kind: .return_stmt
		pos:  token.new_pos(1, third_offset)
	})
	info := g.coverage_files[os.real_path(path)] or { panic('missing coverage metadata') }
	assert info.points == [1, 2, 3]
	assert info.counters == [0, 1, 2]
	assert g.coverage_counter_count == 3
	generated := g.sb.str()
	assert generated.count('_v3_cov[0]++;') == 1
	assert generated.count('_v3_cov[1]++;') == 2
	assert generated.count('_v3_cov[2]++;') == 1
}

fn test_coverage_user_text_is_not_embedded_in_c_format_strings() {
	dir := os.join_path(os.temp_dir(), 'v3_coverage_%s_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	mut g := FlatGen.new()
	g.coverage_dir = dir
	g.coverage_build_options = '-d percent=%d'
	g.emit_coverage_support()
	generated := g.sb.str()
	escaped_dir := c_escape(dir)
	assert generated.contains('snprintf(cov_filename, sizeof(cov_filename), "%s/vcounters_v3_')
	assert generated.contains('.csv", "${escaped_dir}", cov_secs, cov_nsecs);')
	assert !generated.contains('"${escaped_dir}/vcounters_v3_')
	assert generated.contains('fprintf(cov_file, "# path: %s\\n", "${escaped_dir}");')
	assert generated.contains('fprintf(cov_file, "# build_options: %s\\n", "-d percent=%d");')
	assert generated.contains('unsigned long long cov_ms = __v_windows_now_ms();')
	assert !generated.contains('GetTickCount64()')
}

fn test_coverage_function_entry_records_a_counter_per_emitted_function() {
	source := 'fn alpha() int { return 1 }\nfn beta() int { return 2 }\n'
	path := os.join_path(os.temp_dir(), 'v3_coverage_fns_${os.getpid()}.v')
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut ast := flat.FlatAst.new()
	// The AST owns this File pointer for the lifetime of the test fixture.
	unsafe {
		ast.source_files[1] = file
	}
	mut g := FlatGen.new()
	g.a = &ast
	g.coverage_dir = os.join_path(os.temp_dir(), 'v3_coverage_fns_${os.getpid()}')
	alpha_offset := source.index('alpha') or { panic('missing alpha') }
	beta_offset := source.index('beta') or { panic('missing beta') }
	g.cur_fn_name = 'alpha'
	g.write_coverage_function_entry(flat.Node{
		pos: token.new_pos(1, alpha_offset)
	}, 'main__alpha')
	// A generic instantiation is emitted again under another C name, so it is a
	// second function rather than the first one repeated.
	g.cur_fn_name = 'alpha'
	g.write_coverage_function_entry(flat.Node{
		pos: token.new_pos(1, alpha_offset)
	}, 'main__alpha_generic')
	// The same C name is the same function, and must not consume a counter.
	g.write_coverage_function_entry(flat.Node{
		pos: token.new_pos(1, alpha_offset)
	}, 'main__alpha')
	g.cur_fn_name = 'beta'
	g.write_coverage_function_entry(flat.Node{
		pos: token.new_pos(1, beta_offset)
	}, 'main__beta')
	info := g.coverage_files[os.real_path(path)] or { panic('missing coverage metadata') }
	assert info.functions.len == 3
	assert info.functions[0].name == 'alpha'
	assert info.functions[0].start_line == 1
	assert info.functions[0].counter == 0
	assert info.functions[1].name == 'alpha'
	assert info.functions[2].name == 'beta'
	assert info.functions[2].start_line == 2
	assert g.coverage_counter_count == 3
	generated := g.sb.str()
	assert generated.count('_v3_cov[0]++;') == 1
	assert generated.count('_v3_cov[1]++;') == 1
	assert generated.count('_v3_cov[2]++;') == 1
}

// Function entries are counters like any other, so a file with nothing but a
// function to enter still reports something.
fn test_coverage_function_entry_is_the_only_point_in_an_empty_body() {
	source := 'fn empty() {}\n'
	path := os.join_path(os.temp_dir(), 'v3_coverage_empty_${os.getpid()}.v')
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut ast := flat.FlatAst.new()
	unsafe {
		ast.source_files[1] = file
	}
	mut g := FlatGen.new()
	g.a = &ast
	g.coverage_dir = os.join_path(os.temp_dir(), 'v3_coverage_empty_${os.getpid()}')
	g.cur_fn_name = 'main.empty'
	g.write_coverage_function_entry(flat.Node{
		pos: token.new_pos(1, 0)
	}, 'main__empty')
	info := g.coverage_files[os.real_path(path)] or { panic('missing coverage metadata') }
	assert info.points.len == 0
	assert info.functions.len == 1
	assert info.functions[0].name == 'main.empty'
}

// The function counters go to a file of their own extension, so a reader that
// only knows line counters never sees a header it cannot parse.
fn test_coverage_function_hits_are_written_with_a_reader_of_their_own() {
	dir := os.join_path(os.temp_dir(), 'v3_coverage_split_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	source := 'fn answer() int { return 6 * 7 }\nfn unused() int { return 0 }\n'
	path := os.join_path(dir, 'split.v')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(path, source)!
	program := os.join_path(dir, 'program.v')
	os.write_file(program, 'fn main() {\n\tassert answer() == 42\n}\n')!
	cc := os.find_abs_path_of_executable('clang') or { return }
	ran := os.exec([@VEXE, '-cc', cc, '-cstrict', '-gc', 'none', '-nocache', '-no-retry-compilation',
		'-no-skip-unused', '-coverage', dir, 'run', dir])
	assert ran.exit_code == 0, ran.output
	line_counters := os.ls(dir)!.filter(it.ends_with('.csv'))
	assert line_counters.len == 1, line_counters.str()
	function_counters := os.ls(dir)!.filter(it.ends_with('.fcount'))
	assert function_counters.len == 1, function_counters.str()
	function_report := os.read_file(os.join_path(dir, function_counters[0]))!
	assert function_report.contains('meta,position,hits\n'), function_report
	function_lines := function_report.split_into_lines().filter(!it.starts_with('#'))
	meta_files := os.ls(os.join_path(dir, 'meta'))!
	assert meta_files.len > 0, meta_files.str()
	mut found_source := false
	for meta_name in meta_files {
		meta := os.read_file(os.join_path(dir, 'meta', meta_name))!
		if meta.contains('split.v') {
			found_source = true
			assert meta.contains('"functions": [{"name": "answer"'), meta
			assert meta.contains('"name": "unused"'), meta
			// Runtime initialization also enters standard-library functions. Only
			// answer from this fixture was called, at position 0 in its metadata.
			source_hash := meta_name.all_before_last('.json')
			source_hits := function_lines.filter(it.starts_with('${source_hash},'))
			assert source_hits == ['${source_hash},0,1'], source_hits.str()
		}
	}
	assert found_source
}

fn test_windows_statistics_and_coverage_clock_avoids_direct_tick_count_import() {
	mut g := FlatGen.new()
	g.emit_windows_monotonic_clock()
	generated := g.sb.str()
	assert generated.contains('ULONGLONG (WINAPI *tick_count_fn)(void)')
	assert generated.contains('GetProcAddress(GetModuleHandleA("kernel32.dll"), "GetTickCount64")')
	assert !generated.contains('GetTickCount64()')
	assert generated.contains('QueryPerformanceCounter(&counter)')
	assert generated.contains('counter.QuadPart / frequency.QuadPart')
	assert generated.contains('counter.QuadPart % frequency.QuadPart')
}

fn test_headerless_coverage_and_profile_write_reports_without_imports() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'v3_instrumented_stdio_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'program.v')
	os.write_file(source, 'fn answer() int { return 6 * 7 }\nfn main() { assert answer() == 42 }\n')!
	for keep_unused in [false, true] {
		coverage := os.join_path(root, 'coverage_${keep_unused}')
		mut args := [@VEXE, '-cc', cc, '-cstrict', '-gc', 'none', '-nocache', '-no-retry-compilation',
			'-coverage', coverage]
		if keep_unused {
			args << '-no-skip-unused'
		}
		args << ['run', source]
		ran := os.exec(args)
		assert ran.exit_code == 0, ran.output
		counters := os.ls(coverage)!.filter(it.ends_with('.csv'))
		assert counters.len == 1, counters.str()
		contents := os.read_file(os.join_path(coverage, counters[0]))!
		assert contents.contains('meta,point,hits\n'), contents
		assert contents.split_into_lines().filter(!it.starts_with('#')).len > 1, contents
	}
	profile := os.join_path(root, 'profile.txt')
	ran := os.exec([@VEXE, '-cc', cc, '-cstrict', '-gc', 'none', '-nocache', '-no-retry-compilation',
		'-profile', profile, 'run', source])
	assert ran.exit_code == 0, ran.output
	report := os.read_file(profile)!
	assert report.contains('main__answer'), report
}
