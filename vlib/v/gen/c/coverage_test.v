module c

import os
import v.flat
import v.parser
import v.pref
import v.types
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

fn test_instrumentation_file_calls_use_bodyless_bindings_without_source_calls() {
	root := os.join_path(os.vtmp_dir(), 'instrumentation_bindings_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'bindings.c.v')
	os.write_file(source, 'module builtin
struct C.FILE {}
fn C.fopen(const_filename &char, const_mode &char) &C.FILE
fn C.fclose(stream &C.FILE) i32
fn C.mach_absolute_time() u64
fn C.mach_timebase_info(info voidptr) i32
')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(source)
	assert p.diagnostics.len == 0
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	for mode in ['coverage', 'profile-file', 'profile-stdout'] {
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.target = pref.target_from('macos', 'arm64') or { panic(err) }
		if mode == 'coverage' {
			g.set_coverage(os.join_path(root, 'counters'), '')
			g.emit_coverage_support()
		} else {
			g.set_profile(if mode == 'profile-file' {
				os.join_path(root, 'profile.txt')
			} else {
				'-'
			}, false, [])
			g.gen_profile_support()
		}
		generated := g.sb.str()
		if mode == 'profile-stdout' {
			assert !generated.contains('fopen('), generated
			assert !generated.contains('fclose('), generated
		} else {
			declaration := 'FILE* fopen(const char* const_filename, const char* const_mode);'
			assert generated.contains(declaration), generated
			assert generated.contains('i32 fclose(FILE* stream);'), generated
			body := if mode == 'coverage' {
				'static void v3_write_coverage_stats('
			} else {
				'void vprint_profile_stats('
			}
			assert generated.index(declaration)? < generated.index(body)?, generated
		}
		if mode != 'coverage' {
			assert generated.contains('i32 mach_timebase_info(void* info);'), generated
			assert !generated.contains('extern int mach_timebase_info(mach_timebase_info_data_t*);'), generated
		}
		assert !generated.contains('#include'), generated
	}
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
