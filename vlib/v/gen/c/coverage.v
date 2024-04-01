// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.token

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
	is_stdout := g.pref.coverage_file == '-'

	g.cov_declarations.writeln('void vprint_coverage_stats() {')
	if is_stdout {
		g.cov_declarations.writeln('\tprintf("V coverage\\n");')
		g.cov_declarations.writeln('\tprintf("${'-':50r}\\n");')
	} else {
		g.cov_declarations.writeln('\tFILE *fp = stdout;')
		g.cov_declarations.writeln('\tfp = fopen ("${g.pref.coverage_file}", "w+");')
		g.cov_declarations.writeln('\tfprintf(fp, "V coverage\\n");')
		g.cov_declarations.writeln('\tfprintf(fp, "${'-':50r}\\n");')
	}
	g.cov_declarations.writeln('\tint t_counter = 0;')
	mut last_offset := 0
	mut t_points := 0
	for k, cov in g.coverage_files {
		nr_points := cov.points.len
		t_points += nr_points
		g.cov_declarations.writeln('\t{')
		g.cov_declarations.writeln('\t\tint counter = 0;')
		g.cov_declarations.writeln('\t\tfor (int i = 0, offset = ${last_offset}; i < ${nr_points}; ++i)')
		g.cov_declarations.writeln('\t\t\tif (_v_cov[_v_cov_file_offset_${k}+i]) counter++;')
		g.cov_declarations.writeln('\t\tt_counter += counter;')
		if is_stdout {
			g.cov_declarations.writeln('\t\tprintf("> ${cov.file.path} | ${nr_points} | %d | %.2f%% \\n", counter, ${nr_points} > 0 ? ((double)counter/${nr_points})*100 : 0);')
		} else {
			g.cov_declarations.writeln('\t\tfprintf(fp, "> ${cov.file.path} | ${nr_points} | %d | %.2f%% \\n", counter, ${nr_points} > 0 ? ((double)counter/${nr_points})*100 : 0);')
		}
		g.cov_declarations.writeln('\t}')
		last_offset += nr_points
	}
	if is_stdout {
		g.cov_declarations.writeln('\tprintf("${'-':50r}\\n");')
		g.cov_declarations.writeln('\tprintf("Total coverage: ${g.coverage_files.len} files, %.2f%% coverage\\n", ((double)t_counter/${t_points})*100);')
	} else {
		g.cov_declarations.writeln('\tfprintf(fp, "${'-':50r}\\n");')
		g.cov_declarations.writeln('\tfprintf(fp, "Total coverage: ${g.coverage_files.len} files, %.2f%% coverage\\n", ((double)t_counter/${t_points})*100);')
		g.cov_declarations.writeln('\tfclose(fp);')
	}
	g.cov_declarations.writeln('}')
}
