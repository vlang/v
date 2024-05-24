// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.token
import rand { ulid }

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

	build_hash := ulid()

	g.cov_declarations.writeln('void vprint_coverage_stats() {')

	g.cov_declarations.writeln('time_t rawtime;')
	g.cov_declarations.writeln('char cov_filename[120];')
	g.cov_declarations.writeln('time(&rawtime);')
	g.cov_declarations.writeln('snprintf(cov_filename, 120, "vcover.%s.%s", "${build_hash}", vcoverage_fnv1a(asctime(localtime(&rawtime))));')
	g.cov_declarations.writeln('\tFILE *fp = fopen(cov_filename, "w+");')
	g.cov_declarations.writeln('\tfprintf(fp, "[");')
	g.cov_declarations.writeln('\tint t_counter = 0;')
	mut last_offset := 0
	mut is_first := true
	for k, cov in g.coverage_files {
		nr_points := cov.points.len
		g.cov_declarations.writeln('\t{')
		g.cov_declarations.writeln('\t\tint counter = 0;')
		g.cov_declarations.writeln('\t\tfor (int i = 0, offset = ${last_offset}; i < ${nr_points}; ++i)')
		g.cov_declarations.writeln('\t\t\tif (_v_cov[_v_cov_file_offset_${k}+i]) counter++;')
		g.cov_declarations.writeln('\t\tt_counter += counter;')
		g.cov_declarations.writeln('\t\tif (counter) {')
		g.cov_declarations.writeln("\t\t\tfprintf(fp, \"%s{\\\"file\\\":\\\"%s\\\",\\\"hits\\\":%d,\\\"points\\\":%d}\", ${int(is_first)} ? \"\" : \",\", \"${cov.file.path}\", counter, ${nr_points});")
		if is_first {
			is_first = !is_first
		}
		g.cov_declarations.writeln('\t\t}')
		g.cov_declarations.writeln('\t}')
		last_offset += nr_points
	}
	g.cov_declarations.writeln('\tfprintf(fp, "]");')
	g.cov_declarations.writeln('}')
}
