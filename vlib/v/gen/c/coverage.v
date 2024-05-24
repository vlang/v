// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import hash.fnv1a
import v.token

@[inline]
fn current_time() u64 {
	unsafe {
		$if windows {
			tm := u64(0)
			C.QueryPerformanceCounter(&tm)
			return tm
		} $else {
			ts := C.timespec{}
			C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
			return u64(ts.tv_sec) * 1000000000 + u64(ts.tv_nsec)
		}
	}
}

@[inline]
fn (mut g Gen) write_coverage_point(pos token.Pos) {
	if g.unique_file_path_hash !in g.coverage_files {
		g.coverage_files[g.unique_file_path_hash] = &CoverageInfo{
			points: []
			file: g.file
		}
	}
	if g.fn_decl != unsafe { nil } {
		curr_line := u64(pos.line_nr)
		mut curr_cov := unsafe { g.coverage_files[g.unique_file_path_hash] }
		if curr_line !in curr_cov.points {
			curr_cov.points << curr_line
		}
		g.writeln('_v_cov[_v_cov_file_offset_${g.unique_file_path_hash}+${curr_cov.points.len - 1}] = 1;')
	}
}

fn (mut g Gen) write_coverage_stats() {
	is_stdout := false // g.pref.coverage_file == '-'
	is_json := true

	g.cov_declarations.writeln('char* vcoverage_fnv1a(const u8* data) {')
	g.cov_declarations.writeln('\tu32 h = 2166136261UL;')
	g.cov_declarations.writeln('\tsize_t size = strlen(data);')
	g.cov_declarations.writeln('\tfor (size_t i = 0; i < size; i++) {')
	g.cov_declarations.writeln('\th ^= data[i];')
	g.cov_declarations.writeln('\th *= 16777619;')
	g.cov_declarations.writeln('\t}')
	g.cov_declarations.writeln('\treturn u32_str(h).str;')
	g.cov_declarations.writeln('}')
	g.cov_declarations.writeln('')

	build_hash := fnv1a.sum64_string(current_time().str())

	g.cov_declarations.writeln('void vprint_coverage_stats() {')
	if is_stdout {
		g.cov_declarations.writeln('\tprintf("V coverage\\n");')
		g.cov_declarations.writeln('\tprintf("${'-':50r}\\n");')
	} else {
		g.cov_declarations.writeln('time_t rawtime;')
		g.cov_declarations.writeln('char cov_filename[200];')
		g.cov_declarations.writeln('time(&rawtime);')
		g.cov_declarations.writeln('snprintf(cov_filename, 200, "vcover.%s.%s", "${build_hash}", vcoverage_fnv1a(asctime(localtime(&rawtime))));')

		g.cov_declarations.writeln('\tFILE *fp = fopen(cov_filename, "w+");')

		if is_json {
			g.cov_declarations.writeln('\tfprintf(fp, "[");')
		} else {
			g.cov_declarations.writeln('\tfprintf(fp, "V coverage\\n");')
			g.cov_declarations.writeln('\tfprintf(fp, "${'-':50r}\\n");')
		}
	}
	g.cov_declarations.writeln('\tint t_counter = 0;')
	mut last_offset := 0
	mut t_points := 0
	mut is_first := true
	for k, cov in g.coverage_files {
		nr_points := cov.points.len
		t_points += nr_points
		g.cov_declarations.writeln('\t{')
		g.cov_declarations.writeln('\t\tint counter = 0;')
		g.cov_declarations.writeln('\t\tfor (int i = 0, offset = ${last_offset}; i < ${nr_points}; ++i)')
		g.cov_declarations.writeln('\t\t\tif (_v_cov[_v_cov_file_offset_${k}+i]) counter++;')
		g.cov_declarations.writeln('\t\tt_counter += counter;')
		g.cov_declarations.writeln('\t\tif (counter) {')
		if is_stdout {
			g.cov_declarations.writeln('\t\t\tprintf("[%7.2f%%] %4d / ${nr_points:4d} | ${cov.file.path}\\n", ${nr_points} > 0 ? ((double)counter/${nr_points})*100 : 0, counter);')
		} else if is_json {
			g.cov_declarations.writeln("\t\t\tfprintf(fp, \"%s{\\\"file\\\":\\\"%s\\\",\\\"cov\\\":%.2f,\\\"points\\\":%d}\", ${int(is_first)} ? \"\" : \",\", \"${cov.file.path}\", ${nr_points} > 0 ? ((double)counter/${nr_points})*100 : 0, ${nr_points});")
			if is_first {
				is_first = !is_first
			}
		} else {
			g.cov_declarations.writeln('\t\t\tfprintf(fp, "[%7.2f%%] %4d / ${nr_points:4d} | ${cov.file.path}\\n", ${nr_points} > 0 ? ((double)counter/${nr_points})*100 : 0, counter);')
		}
		g.cov_declarations.writeln('\t\t}')
		g.cov_declarations.writeln('\t}')
		last_offset += nr_points
	}
	if is_stdout {
		g.cov_declarations.writeln('\tprintf("${'-':50r}\\n");')
		g.cov_declarations.writeln('\tprintf("Total coverage: ${g.coverage_files.len} files, %.2f%% coverage\\n", ((double)t_counter/${t_points})*100);')
	} else if is_json {
		g.cov_declarations.writeln('\tfprintf(fp, "]");')
	} else {
		g.cov_declarations.writeln('\tfprintf(fp, "${'-':50r}\\n");')
		g.cov_declarations.writeln('\tfprintf(fp, "Total coverage: ${g.coverage_files.len} files, %.2f%% coverage\\n", ((double)t_counter/${t_points})*100);')
		g.cov_declarations.writeln('\tfclose(fp);')
	}
	g.cov_declarations.writeln('}')
}
