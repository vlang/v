// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.token
import os
import rand

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
		g.writeln('_v_cov[_v_cov_file_offset_${g.unique_file_path_hash}+${curr_cov.points.len - 1}]++;')
	}
}

fn (mut g Gen) write_coverage_stats() {
	ulid_hash := rand.ulid() // rand.ulid provides a hash+timestamp, so that a collision is extremely unlikely
	g.cov_declarations.writeln('')
	g.cov_declarations.writeln('void vprint_coverage_stats() {')
	// g.cov_declarations.writeln('\tstruct timespec ts;')
	// g.cov_declarations.writeln('\tclock_gettime(CLOCK_MONOTONIC, &ts);')
	g.cov_declarations.writeln('\tchar cov_filename[512];')
	g.cov_declarations.writeln('\tchar *cov_dir = getenv("VCOVDIR") != NULL ? getenv("VCOVDIR") : "${os.real_path(g.pref.coverage_dir)}";')
	g.cov_declarations.writeln('\tsnprintf(cov_filename, sizeof(cov_filename), "%s/vcover_${ulid_hash}.csv", cov_dir);')
	g.cov_declarations.writeln('\tFILE *fp = fopen(cov_filename, "wb+");')
	g.cov_declarations.writeln('\tfprintf(fp, "file,points,counter,hits\\n");')
	mut last_offset := 0
	for k, cov in g.coverage_files {
		nr_points := cov.points.len
		g.cov_declarations.writeln('\t{')
		g.cov_declarations.writeln('\t\tfor (int i = 0, offset = ${last_offset}; i < ${nr_points}; ++i) {')
		g.cov_declarations.writeln('\t\t\tif (_v_cov[_v_cov_file_offset_${k}+i]) {')
		g.cov_declarations.writeln("\t\t\t\tfprintf(fp, \"\\\"%s\\\",%d,%d,%ld\\n\", \"${cov.file.path}\", ${nr_points}, i, _v_cov[_v_cov_file_offset_${k}+i]);")
		g.cov_declarations.writeln('\t\t\t}')
		g.cov_declarations.writeln('\t\t}')
		g.cov_declarations.writeln('\t}')
		last_offset += nr_points
	}
	g.cov_declarations.writeln('\tfclose(fp);')
	g.cov_declarations.writeln('}')
}
