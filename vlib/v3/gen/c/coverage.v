module c

import hash
import os
import time
import v3.flat

@[heap]
struct CoverageInfo {
	path  string
	fhash string
mut:
	points          []int
	counters        []int
	counter_by_line map[int]int
}

// set_coverage enables V-compatible line coverage output.
pub fn (mut g FlatGen) set_coverage(dir string, build_options string) {
	g.coverage_dir = dir
	g.coverage_build_options = build_options
}

fn (mut g FlatGen) write_coverage_point(node flat.Node) {
	if g.coverage_dir.len == 0 || g.cur_fn_name.len == 0
		|| node.kind !in [.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .return_stmt, .break_stmt, .continue_stmt, .defer_stmt, .assert_stmt, .goto_stmt] {
		return
	}
	position := g.a.source_position(node.pos) or { return }
	path := os.real_path(position.filename)
	line := position.line
	mut info := g.coverage_files[path] or {
		fhash := hash.sum64_string('${g.coverage_build_options}:${path}', 32).hex_full()
		created := &CoverageInfo{
			path:            path
			fhash:           fhash
			points:          []
			counter_by_line: map[int]int{}
		}
		g.coverage_files[path] = created
		created
	}
	mut counter := info.counter_by_line[line]
	if line !in info.counter_by_line {
		counter = g.coverage_counter_count
		info.counter_by_line[line] = counter
		info.points << line
		info.counters << counter
		g.coverage_counter_count++
	}
	g.writeln('_v3_cov[${counter}]++;')
}

fn (mut g FlatGen) gen_coverage_registration() {
	if g.coverage_dir.len > 0 {
		g.writeln('atexit(v3_write_coverage_stats);')
	}
}

fn coverage_json_escape(value string) string {
	return json_string_content_escape(value)
}

fn (mut g FlatGen) write_coverage_metadata() {
	if g.coverage_dir.len == 0 {
		return
	}
	os.mkdir_all(g.coverage_dir) or { return }
	meta_dir := os.join_path_single(g.coverage_dir, 'meta')
	os.mkdir_all(meta_dir) or { return }
	for _, info in g.coverage_files {
		path := os.join_path_single(meta_dir, '${info.fhash}.json')
		mut file := os.create(path) or { continue }
		file.writeln('{') or { continue }
		file.writeln('  "file": "${coverage_json_escape(info.path)}", "fhash": "${info.fhash}",') or {
			continue
		}
		file.writeln('  "v_version": "V3 ${@VHASH}",') or { continue }
		file.writeln('  "build_options": "${coverage_json_escape(g.coverage_build_options)}",') or {
			continue
		}
		file.writeln('  "npoints": ${info.points.len},') or { continue }
		file.write_string('  "points": [  ') or { continue }
		for index, point in info.points {
			file.write_string(point.str()) or { continue }
			if index + 1 < info.points.len {
				file.write_string(',') or { continue }
			}
		}
		file.writeln('  ]') or { continue }
		file.writeln('}') or { continue }
		file.close()
	}
}

fn (mut g FlatGen) emit_coverage_support() {
	if g.coverage_dir.len == 0 {
		return
	}
	g.write_coverage_metadata()
	counter_count := if g.coverage_counter_count > 0 { g.coverage_counter_count } else { 1 }
	compile_tag := '${os.getpid()}_${time.now().unix_micro()}'
	g.writeln('static unsigned long long _v3_cov[${counter_count}];')
	g.writeln('static void v3_write_coverage_stats(void) {')
	g.writeln('\tchar cov_filename[4096];')
	g.writeln('\tlong long cov_secs = 0;')
	g.writeln('\tlong cov_nsecs = 0;')
	g.writeln('#if defined(_WIN32)')
	g.writeln('\tcov_secs = (long long)(GetTickCount64() / 1000);')
	g.writeln('\tcov_nsecs = (long)((GetTickCount64() % 1000) * 1000000);')
	g.writeln('#else')
	g.writeln('\tstruct timespec cov_ts;')
	g.writeln('\tclock_gettime(CLOCK_MONOTONIC, &cov_ts);')
	g.writeln('\tcov_secs = (long long)cov_ts.tv_sec;')
	g.writeln('\tcov_nsecs = cov_ts.tv_nsec;')
	g.writeln('#endif')
	g.writeln('\tsnprintf(cov_filename, sizeof(cov_filename), "%s/vcounters_v3_${compile_tag}.%lld.%09ld.csv", "${c_escape(g.coverage_dir)}", cov_secs, cov_nsecs);')
	g.writeln('\tFILE* cov_file = fopen(cov_filename, "wb+");')
	g.writeln('\tif (cov_file == NULL) return;')
	g.writeln('\tfprintf(cov_file, "# path: %s\\n", "${c_escape(g.coverage_dir)}");')
	g.writeln('\tfprintf(cov_file, "# build_options: %s\\n", "${c_escape(g.coverage_build_options)}");')
	g.writeln('\tfprintf(cov_file, "meta,point,hits\\n");')
	for _, info in g.coverage_files {
		for point_index, counter in info.counters {
			g.writeln('\tif (_v3_cov[${counter}] != 0) fprintf(cov_file, "${info.fhash},${point_index},%llu\\n", _v3_cov[${counter}]);')
		}
	}
	g.writeln('\tfclose(cov_file);')
	g.writeln('}')
	g.writeln('')
}
