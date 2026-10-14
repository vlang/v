// Copyright (c) 2024 Felipe Pena and Delyan Angelov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import log
import flag
import json2
import arrays
import encoding.csv

// program options, storage etc
struct Context {
mut:
	show_help          bool
	show_hotspots      bool
	show_percentages   bool
	show_test_files    bool
	use_absolute_paths bool
	be_verbose         bool
	lcov_output        string
	filter             string
	working_folder     string
	show_functions     bool
	show_uncovered     bool
	json_output        string
	write_tests        bool
	dry_run            bool

	targets            []string
	meta               map[string]MetaData // aggregated meta data, read from all .json files
	all_lines_per_file map[string][]int    // aggregated by load_meta

	counters         map[string]u64         // incremented by process_target, based on each .csv file
	lines_per_file   map[string]map[int]int // incremented by process_target, based on each .csv file
	functions        []FunctionCoverage     // every function of every loaded meta file
	processed_points u64
}

const metadata_extension = '.json'
const vcounter_glob_pattern = 'vcounters_*.csv'
// function_counter_extension names the entry counters a run writes beside the
// line counters. It is deliberately not `.csv`: a reader built before function
// coverage existed walks a directory for `.csv` files, and handing it a header
// it does not know fails the whole report.
const function_counter_extension = '.fcount'

// FunctionCoverage is one function of one source file, with how many times a run
// entered it. A function with 0 hits is one no test has ever called.
struct FunctionCoverage {
mut:
	meta     string
	file     string
	name     string
	line     int
	position int
	hits     u64
}

fn (c &FunctionCoverage) is_covered() bool {
	return c.hits > 0
}

fn (mut ctx Context) load_meta(folder string) {
	for omfile in os.walk_ext(folder, metadata_extension) {
		mfile := omfile.replace('\\', '/')
		content := os.read_file(mfile) or { '' }
		meta := os.file_name(mfile.replace(metadata_extension, ''))
		if meta in ctx.meta {
			continue
		}
		mut data := json2.decode[MetaData](content) or {
			log.error('${@METHOD} failed to load ${mfile}')
			continue
		}
		// A meta file records the path the compiler saw, so on Windows it holds
		// backslashes. Normalizing here puts every path the tool compares,
		// filters and prints on one separator.
		data.file = normalize_path(data.file)
		ctx.meta[meta] = data
		mut lines_per_file := ctx.all_lines_per_file[data.file]
		lines_per_file << data.points
		ctx.all_lines_per_file[data.file] = arrays.distinct(lines_per_file)
		for position, function in data.functions {
			ctx.functions << FunctionCoverage{
				meta:     meta
				file:     data.file
				name:     function.name
				line:     function.start
				position: position
				hits:     0
			}
		}
	}
}

fn (mut ctx Context) post_process_all_metas() {
	ctx.verbose('${@METHOD}')
	for _, m in ctx.meta {
		lines_per_file := ctx.all_lines_per_file[m.file]
		for line in lines_per_file {
			ctx.counters['${m.file}:${line}:'] = 0
		}
	}
}

fn (mut ctx Context) post_process_all_targets() {
	ctx.verbose('${@METHOD}')
	ctx.verbose('ctx.processed_points: ${ctx.processed_points}')
}

fn (ctx &Context) verbose(msg string) {
	if ctx.be_verbose {
		log.info(msg)
	}
}

fn (mut ctx Context) process_target(tfile string) ! {
	ctx.verbose('${@METHOD} ${tfile}')
	mut reader := csv.new_reader_from_file(tfile)!
	header := reader.read()!
	if header == ['meta', 'point', 'hits'] {
		ctx.process_counter_rows(mut reader, tfile)!
		return
	}
	if header == ['meta', 'position', 'hits'] {
		ctx.process_function_rows(mut reader, tfile)!
		return
	}
	return error('invalid header in .csv file')
}

fn (mut ctx Context) process_counter_rows(mut reader csv.Reader, tfile string) ! {
	for {
		row := reader.read() or { break }
		mut cline := CounterLine{
			meta:  row[0]
			point: row[1].int()
			hits:  row[2].u64()
		}
		m := ctx.meta[cline.meta] or {
			ctx.verbose('> skipping invalid meta: ${cline.meta} in file: ${cline.file}, csvfile: ${tfile}')
			continue
		}
		cline.file = m.file
		cline.line = m.points[cline.point] or {
			ctx.verbose('> skipping invalid point: ${cline.point} in file: ${cline.file}, meta: ${cline.meta}, csvfile: ${tfile}')
			continue
		}
		ctx.counters['${cline.file}:${cline.line}:'] += cline.hits
		mut lines := ctx.lines_per_file[cline.file].move()
		lines[cline.line]++
		ctx.lines_per_file[cline.file] = lines.move()
		ctx.processed_points++
	}
}

// process_function_rows sums the entry counters of a `vfunctions_*.fcount` file
// into the functions of the matching meta file. The same function can be
// recorded by several runs, so hits are added rather than replaced.
fn (mut ctx Context) process_function_rows(mut reader csv.Reader, tfile string) ! {
	for {
		row := reader.read() or { break }
		if row.len != 3 {
			return error('invalid function counter row in ${tfile}')
		}
		meta := row[0]
		position := row[1].int()
		hits := row[2].u64()
		m := ctx.meta[meta] or {
			ctx.verbose('> skipping invalid meta: ${meta} in csvfile: ${tfile}')
			continue
		}
		if position < 0 || position >= m.functions.len {
			ctx.verbose('> skipping invalid function position: ${position} in file: ${m.file}, meta: ${meta}, csvfile: ${tfile}')
			continue
		}
		for mut function in ctx.functions {
			if function.meta == meta && function.position == position {
				function.hits += hits
			}
		}
		ctx.processed_points++
	}
}

fn (mut ctx Context) show_report() ! {
	filters := ctx.filter.split(',').filter(it != '')
	if ctx.show_hotspots {
		mut locations := []string{cap: ctx.counters.len}
		for location, _ in ctx.counters {
			if !ctx.matches_filters(location, filters) {
				continue
			}
			locations << location
		}
		locations.sort()
		for location in locations {
			hits := ctx.counters[location]
			mut final_path := normalize_path(location)
			if !ctx.use_absolute_paths {
				final_path = location.all_after_first('${ctx.working_folder}/')
			}
			println('${hits:-8} ${final_path}')
		}
	}
	// `--uncovered` is a list, not a summary, so the per-file percentages that
	// every other mode leads with are only printed when nothing asks for a list.
	if ctx.show_percentages && !ctx.show_uncovered && !ctx.show_functions {
		for file in ctx.sorted_hit_files(filters) {
			total_lines := ctx.all_lines_per_file[file].len
			executed_points := ctx.lines_per_file[file].len
			coverage_percent := 100.0 * f64(executed_points) / f64(total_lines)
			mut final_path := normalize_path(file)
			if !ctx.use_absolute_paths {
				final_path = file.all_after_first('${ctx.working_folder}/')
			}
			println('${final_path:-80s} | ${executed_points:6} | ${total_lines:6} | ${coverage_percent:6.2f}%')
		}
	}
	if ctx.show_functions {
		ctx.show_function_report(filters)
	}
	if ctx.show_uncovered {
		ctx.show_uncovered_functions(filters)
	}
	if ctx.json_output != '' {
		ctx.write_json_report(filters)!
	}
	if ctx.lcov_output != '' {
		ctx.write_lcov_report(filters)!
	}
	if ctx.write_tests {
		ctx.write_tests_for_uncovered(filters)
	}
}

// reported_functions returns the functions to report, in a stable order:
// by file, then by the line the declaration starts on.
//
// Each test file compiles the source under test on its own, so the same function
// is recorded once per meta file. They are merged here: one function of one file
// is one report line, with the hits of every meta that recorded it.
fn (ctx &Context) reported_functions(filters []string) []FunctionCoverage {
	mut by_key := map[string]FunctionCoverage{}
	mut order := []string{}
	for function in ctx.functions {
		if !ctx.should_include_file(function.file, filters) {
			continue
		}
		if function.name.starts_with('test_') {
			continue
		}
		key := '${function.file}:${function.line}:${function.name}'
		if merged := by_key[key] {
			by_key[key] = FunctionCoverage{
				file:     merged.file
				name:     merged.name
				line:     merged.line
				position: merged.position
				hits:     merged.hits + function.hits
			}
		} else {
			order << key
			by_key[key] = function
		}
	}
	mut functions := []FunctionCoverage{cap: order.len}
	for key in order {
		functions << by_key[key]
	}
	functions.sort_with_compare(fn (a &FunctionCoverage, b &FunctionCoverage) int {
		if a.file != b.file {
			return if a.file < b.file { -1 } else { 1 }
		}
		if a.line != b.line {
			return if a.line < b.line { -1 } else { 1 }
		}
		return if a.name < b.name { -1 } else { 1 }
	})
	return functions
}

fn (ctx &Context) show_function_report(filters []string) {
	mut files := []string{}
	mut by_file := map[string][]FunctionCoverage{}
	for function in ctx.reported_functions(filters) {
		if function.file !in by_file {
			files << function.file
		}
		by_file[function.file] << function
	}
	files.sort()
	for file in files {
		functions := by_file[file]
		mut covered := 0
		for function in functions {
			if function.is_covered() {
				covered++
			}
		}
		percent := if functions.len == 0 {
			0.0
		} else {
			100.0 * f64(covered) / f64(functions.len)
		}
		mut final_path := normalize_path(file)
		if !ctx.use_absolute_paths {
			final_path = file.all_after_first('${ctx.working_folder}/')
		}
		println('${final_path:-80s} | ${covered:6} | ${functions.len:6} | ${percent:6.2f}%')
		for function in functions {
			state := if function.is_covered() { 'covered' } else { 'MISSING' }
			println('    ${state:-8} ${function.name:-60s} line ${function.line}')
		}
	}
}

// show_uncovered_functions prints one `path:line name` line per function no run
// entered. The shape is the one an editor reads, so it is deliberately plain.
fn (ctx &Context) show_uncovered_functions(filters []string) {
	for function in ctx.reported_functions(filters) {
		if function.is_covered() {
			continue
		}
		mut final_path := normalize_path(function.file)
		if !ctx.use_absolute_paths {
			final_path = function.file.all_after_first('${ctx.working_folder}/')
		}
		println('${final_path}:${function.line} ${function.name}')
	}
}

// FileReport is one file's coverage, in the machine-readable report.
struct FileReport {
	file             string
	points           int
	covered          int
	percent          f64
	functions        []FunctionReport
	function_points  int
	function_covered int
}

// FunctionReport is one function of a file, in the machine-readable report.
struct FunctionReport {
	name    string
	line    int
	covered bool
	hits    u64
}

// CoverageReport is what `--json` writes.
struct CoverageReport {
	files  []FileReport
	totals FileReport
}

fn (mut ctx Context) build_report(filters []string) CoverageReport {
	mut files := []FileReport{}
	reported := ctx.reported_functions(filters)
	for file in ctx.sorted_files(filters) {
		total_lines := ctx.all_lines_per_file[file].len
		covered_lines := if lines := ctx.lines_per_file[file] { lines.len } else { 0 }
		mut functions := []FunctionReport{}
		mut covered_functions := 0
		for function in reported {
			if function.file != file {
				continue
			}
			if function.is_covered() {
				covered_functions++
			}
			functions << FunctionReport{
				name:    function.name
				line:    function.line
				covered: function.is_covered()
				hits:    function.hits
			}
		}
		functions.sort_with_compare(fn (a &FunctionReport, b &FunctionReport) int {
			if a.line != b.line {
				return if a.line < b.line { -1 } else { 1 }
			}
			return if a.name < b.name { -1 } else { 1 }
		})
		files << FileReport{
			file:             normalize_path(file)
			points:           total_lines
			covered:          covered_lines
			percent:          if total_lines == 0 {
				0.0
			} else {
				100.0 * f64(covered_lines) / f64(total_lines)
			}
			functions:        functions
			function_points:  functions.len
			function_covered: covered_functions
		}
	}
	mut total_points := 0
	mut total_covered := 0
	mut total_functions := 0
	mut covered_functions := 0
	for file in files {
		total_points += file.points
		total_covered += file.covered
		total_functions += file.function_points
		covered_functions += file.function_covered
	}
	return CoverageReport{
		files:  files
		totals: FileReport{
			points:           total_points
			covered:          total_covered
			percent:          if total_points == 0 {
				0.0
			} else {
				100.0 * f64(total_covered) / f64(total_points)
			}
			function_points:  total_functions
			function_covered: covered_functions
		}
	}
}

fn (mut ctx Context) write_json_report(filters []string) ! {
	report := ctx.build_report(filters)
	output_path := os.real_path(ctx.json_output)
	output_dir := os.dir(output_path)
	if output_dir != '' && !os.exists(output_dir) {
		os.mkdir_all(output_dir)!
	}
	os.write_file(output_path, json2.encode(report, prettify: true))!
	ctx.verbose('Wrote JSON report to ${output_path}')
}

fn normalize_path(path string) string {
	return path.replace('\\', '/')
}

fn (ctx &Context) matches_filters(path string, filters []string) bool {
	if filters.len == 0 {
		return true
	}
	return filters.any(path.contains(it))
}

fn (ctx &Context) should_include_file(file string, filters []string) bool {
	if !ctx.show_test_files && (file.ends_with('_test.v') || file.ends_with('_test.c.v')) {
		return false
	}
	return ctx.matches_filters(file, filters)
}

fn (ctx &Context) sorted_hit_files(filters []string) []string {
	mut files := []string{}
	for file, _ in ctx.lines_per_file {
		if !ctx.should_include_file(file, filters) {
			continue
		}
		files << file
	}
	files.sort()
	return files
}

fn (ctx &Context) sorted_files(filters []string) []string {
	mut files := []string{}
	for file, _ in ctx.all_lines_per_file {
		if !ctx.should_include_file(file, filters) {
			continue
		}
		files << file
	}
	files.sort()
	return files
}

fn (ctx &Context) write_lcov_report(filters []string) ! {
	output_path := os.real_path(ctx.lcov_output)
	output_dir := os.dir(output_path)
	if output_dir != '' && !os.exists(output_dir) {
		os.mkdir_all(output_dir)!
	}
	mut output := []string{}
	for file in ctx.sorted_files(filters) {
		mut lines := ctx.all_lines_per_file[file].clone()
		lines.sort()
		hit_lines := ctx.lines_per_file[file].len
		output << 'TN:'
		output << 'SF:${normalize_path(file)}'
		for line in lines {
			hits := ctx.counters['${file}:${line}:']
			output << 'DA:${line},${hits}'
		}
		output << 'LF:${lines.len}'
		output << 'LH:${hit_lines}'
		output << 'end_of_record'
	}
	os.write_file(output_path, output.join_lines())!
	ctx.verbose('Wrote LCOV report to ${output_path}')
}

fn main() {
	log.use_stdout()
	mut ctx := Context{}
	ctx.working_folder = normalize_path(os.real_path(os.getwd()))
	mut fp := flag.new_flag_parser(os.args#[1..])
	fp.application('v cover')
	fp.version('0.0.2')
	fp.description('Analyze & make reports, based on cover files, produced by running programs and tests, compiled with `-coverage folder/`')
	fp.arguments_description('[folder1/ file2 ...]')
	ctx.show_help = fp.bool('help', `h`, false, 'Show this help text.')
	ctx.be_verbose = fp.bool('verbose', `v`, false,
		'Be more verbose while processing the coverages.')
	ctx.show_hotspots = fp.bool('hotspots', `H`, false,
		'Show most frequently executed covered lines.')
	ctx.show_percentages = fp.bool('percentages', `P`, true, 'Show coverage percentage per file.')
	ctx.lcov_output = fp.string('lcov', 0, '',
		'Write an LCOV line coverage report to the specified file path.')
	ctx.show_functions = fp.bool('functions', `F`, false,
		'Show every function of every file, and whether a run entered it.')
	ctx.show_uncovered = fp.bool('uncovered', `U`, false,
		'List the functions no run entered, one `path:line name` per line.')
	ctx.json_output = fp.string('json', 0, '',
		'Write the whole report as JSON to the specified file path.')
	ctx.write_tests = fp.bool('write-tests', `W`, false,
		'Write a test_ function for every uncovered function, beside its source.')
	ctx.dry_run = fp.bool('dry-run', `n`, false,
		'With --write-tests, print what would be written instead of writing it.')
	ctx.show_test_files = fp.bool('show_test_files', `S`, false,
		'Show `_test.v` files as well (normally filtered).')
	ctx.use_absolute_paths = fp.bool('absolute', `A`, false,
		'Use absolute paths for all files, no matter the current folder. By default, files inside the current folder, are shown with a relative path.')
	ctx.filter = fp.string('filter', `f`, '', 'Filter only the matching source path patterns.')
	if ctx.show_help {
		println(fp.usage())
		exit(0)
	}
	targets := fp.finalize() or {
		log.error(fp.usage())
		exit(1)
	}
	ctx.verbose('Targets: ${targets}')
	for t in targets {
		if !os.exists(t) {
			log.error('Skipping ${t}, since it does not exist')
			continue
		}
		if os.is_dir(t) {
			mut found_counter_files := os.walk_ext(t, '.csv')
			found_counter_files << os.walk_ext(t, function_counter_extension)
			if found_counter_files.len == 0 {
				log.error('Skipping ${t}, since there are 0 ${vcounter_glob_pattern} files in it')
				continue
			}
			// The meta of a directory is read once, not once per counter file:
			// every run in the folder repeats the same source, and each repeat
			// would report its functions again.
			ctx.targets << found_counter_files
			ctx.load_meta(t)
		} else {
			ctx.targets << t
			ctx.load_meta(os.dir(t))
		}
	}
	ctx.post_process_all_metas()
	ctx.verbose('Final ctx.targets.len: ${ctx.targets.len}')
	ctx.verbose('Final ctx.meta.len: ${ctx.meta.len}')
	ctx.verbose('Final ctx.filter: ${ctx.filter}')
	if ctx.targets.len == 0 {
		log.error('0 cover targets')
		exit(1)
	}
	for t in ctx.targets {
		ctx.process_target(t)!
	}
	ctx.post_process_all_targets()
	ctx.show_report()!
}
