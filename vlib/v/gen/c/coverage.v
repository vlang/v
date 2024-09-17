// Copyright (c) 2024 Felipe Pena and Delyan Angelov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import rand
import v.ast
import v.token
import v.util.version
import hash

// V coverage info
@[heap]
struct CoverageInfo {
mut:
	idx           int   // index
	points        []u64 // code point line nr
	file          &ast.File = unsafe { nil }
	fhash         string // hash(fpath, build_options), prevents collisions for runs with different options, like `-os windows` or `-gc none`, which may affect the points, due to `$if ... {` etc
	build_options string
}

fn (mut g Gen) write_coverage_point(pos token.Pos) {
	if g.unique_file_path_hash !in g.coverage_files {
		build_options := g.pref.build_options.join(' ')
		fhash := hash.sum64_string('${build_options}:${g.unique_file_path_hash}', 32).hex_full()
		g.coverage_files[g.unique_file_path_hash] = &CoverageInfo{
			points:        []
			file:          g.file
			fhash:         fhash
			build_options: build_options
		}
	}
	if g.fn_decl != unsafe { nil } {
		curr_line := u64(pos.line_nr)
		mut curr_cov := unsafe { g.coverage_files[g.unique_file_path_hash] }
		if curr_line !in curr_cov.points {
			curr_cov.points << curr_line
		}
		stmt_str := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		g.writeln('_v_cov[_v_cov_file_offset_${g.unique_file_path_hash}+${curr_cov.points.len - 1}]++;')
		g.set_current_pos_as_last_stmt_pos()
		g.write(stmt_str)
	}
}

fn (mut g Gen) write_coverage_stats() {
	build_options := g.pref.build_options.join(' ')
	coverage_dir := os.real_path(g.pref.coverage_dir).replace('\\', '/')
	coverage_meta_folder := '${coverage_dir}/meta'
	if !os.exists(coverage_meta_folder) {
		os.mkdir_all(coverage_meta_folder) or {}
	}
	counter_ulid := rand.ulid() // rand.ulid provides a hash+timestamp, so that a collision is extremely unlikely
	g.cov_declarations.writeln('')
	g.cov_declarations.writeln('void vprint_coverage_stats() {')
	g.cov_declarations.writeln('\tchar cov_filename[2048];')
	covdir := cesc(coverage_dir)
	g.cov_declarations.writeln('\tchar *cov_dir = "${covdir}";')
	for _, mut cov in g.coverage_files {
		metadata_coverage_fpath := '${coverage_meta_folder}/${cov.fhash}.json'
		filepath := os.real_path(cov.file.path).replace('\\', '/')
		if os.exists(metadata_coverage_fpath) {
			continue
		}
		mut fmeta := os.create(metadata_coverage_fpath) or { continue }
		fmeta.writeln('{') or { continue }
		jfilepath := jesc(filepath)
		jfhash := jesc(cov.fhash)
		jversion := jesc(version.full_v_version(true))
		jboptions := jesc(cov.build_options)
		fmeta.writeln('  "file": "${jfilepath}", "fhash": "${jfhash}",') or { continue }
		fmeta.writeln('  "v_version": "${jversion}",') or { continue }
		fmeta.writeln('  "build_options": "${jboptions}",') or { continue }
		fmeta.writeln('  "npoints": ${cov.points.len},') or { continue }
		fmeta.write_string('  "points": [  ') or { continue }
		for idx, p in cov.points {
			fmeta.write_string('${p + 1}') or { continue }
			if idx < cov.points.len - 1 {
				fmeta.write_string(',') or { continue }
			}
		}
		fmeta.writeln('  ]') or { continue }
		fmeta.writeln('}') or { continue }
		fmeta.close()
	}
	g.cov_declarations.writeln('\tlong int secs = 0;')
	g.cov_declarations.writeln('\tlong int nsecs = 0;')
	g.cov_declarations.writeln('\t#if defined(_WIN32)')
	g.cov_declarations.writeln('\tlong int ticks_passed = GetTickCount();')
	g.cov_declarations.writeln('\nsecs = ticks_passed / 1000;')
	g.cov_declarations.writeln('\nnsecs = (ticks_passed % 1000) * 1000000;')
	g.cov_declarations.writeln('\t#endif')
	g.cov_declarations.writeln('\t#if !defined(_WIN32)')
	g.cov_declarations.writeln('\tstruct timespec ts;')
	g.cov_declarations.writeln('\tclock_gettime(CLOCK_MONOTONIC, &ts);')
	g.cov_declarations.writeln('\tsecs = ts.tv_sec;')
	g.cov_declarations.writeln('\nsecs = ts.tv_nsec;')
	g.cov_declarations.writeln('\t#endif')
	g.cov_declarations.writeln('\tsnprintf(cov_filename, sizeof(cov_filename), "%s/vcounters_${counter_ulid}.%07ld.%09ld.csv", cov_dir, secs, nsecs);')
	g.cov_declarations.writeln('\tFILE *fp = fopen(cov_filename, "wb+");')
	cprefpath := cesc(os.real_path(g.pref.path))
	cboptions := cesc(build_options)
	g.cov_declarations.writeln('\tfprintf(fp, "# path: ${cprefpath}\\n");')
	g.cov_declarations.writeln('\tfprintf(fp, "# build_options: ${cboptions}\\n");')
	g.cov_declarations.writeln('\tfprintf(fp, "meta,point,hits\\n");')
	for k, cov in g.coverage_files {
		nr_points := cov.points.len
		g.cov_declarations.writeln('\t{')
		g.cov_declarations.writeln('\t\tfor (int i = 0; i < ${nr_points}; ++i) {')
		g.cov_declarations.writeln('\t\t\tif (_v_cov[_v_cov_file_offset_${k}+i]) {')
		g.cov_declarations.writeln("\t\t\t\tfprintf(fp, \"%s,%d,%ld\\n\", \"${cov.fhash}\", i, _v_cov[_v_cov_file_offset_${k}+i]);")
		g.cov_declarations.writeln('\t\t\t}')
		g.cov_declarations.writeln('\t\t}')
		g.cov_declarations.writeln('\t}')
	}
	g.cov_declarations.writeln('\tfclose(fp);')
	g.cov_declarations.writeln('}')
}

fn cesc(s string) string {
	return cescape_nonascii(cestring(s))
}

fn jesc(s string) string {
	return escape_quotes(s)
}
