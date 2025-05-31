// Copyright (c) 2024 Felipe Pena and Delyan Angelov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import log
import flag
import json
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
	filter             string
	working_folder     string

	targets            []string
	meta               map[string]MetaData // aggregated meta data, read from all .json files
	all_lines_per_file map[string][]int    // aggregated by load_meta

	counters         map[string]u64         // incremented by process_target, based on each .csv file
	lines_per_file   map[string]map[int]int // incremented by process_target, based on each .csv file
	processed_points u64
}

const metadata_extension = '.json'
const vcounter_glob_pattern = 'vcounters_*.csv'

fn (mut ctx Context) load_meta(folder string) {
	for omfile in os.walk_ext(folder, metadata_extension) {
		mfile := omfile.replace('\\', '/')
		content := os.read_file(mfile) or { '' }
		meta := os.file_name(mfile.replace(metadata_extension, ''))
		data := json.decode(MetaData, content) or {
			log.error('${@METHOD} failed to load ${mfile}')
			continue
		}
		ctx.meta[meta] = data
		mut lines_per_file := ctx.all_lines_per_file[data.file]
		lines_per_file << data.points
		ctx.all_lines_per_file[data.file] = arrays.distinct(lines_per_file)
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
	if header != ['meta', 'point', 'hits'] {
		return error('invalid header in .csv file')
	}
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
		// dump( ctx.lines_per_file[cline.meta][cline.point] )
		ctx.processed_points++
	}
}

fn (mut ctx Context) show_report() ! {
	filters := ctx.filter.split(',').filter(it != '')
	if ctx.show_hotspots {
		for location, hits in ctx.counters {
			if filters.len > 0 {
				if !filters.any(location.contains(it)) {
					continue
				}
			}
			mut final_path := normalize_path(location)
			if !ctx.use_absolute_paths {
				final_path = location.all_after_first('${ctx.working_folder}/')
			}
			println('${hits:-8} ${final_path}')
		}
	}
	if ctx.show_percentages {
		for file, lines in ctx.lines_per_file {
			if !ctx.show_test_files {
				if file.ends_with('_test.v') || file.ends_with('_test.c.v') {
					continue
				}
			}
			if filters.len > 0 {
				if !filters.any(file.contains(it)) {
					continue
				}
			}
			total_lines := ctx.all_lines_per_file[file].len
			executed_points := lines.len
			coverage_percent := 100.0 * f64(executed_points) / f64(total_lines)
			mut final_path := normalize_path(file)
			if !ctx.use_absolute_paths {
				final_path = file.all_after_first('${ctx.working_folder}/')
			}
			println('${final_path:-80s} | ${executed_points:6} | ${total_lines:6} | ${coverage_percent:6.2f}%')
		}
	}
}

fn normalize_path(path string) string {
	return path.replace(os.path_separator, '/')
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
	fp.skip_executable()
	ctx.show_help = fp.bool('help', `h`, false, 'Show this help text.')
	ctx.be_verbose = fp.bool('verbose', `v`, false, 'Be more verbose while processing the coverages.')
	ctx.show_hotspots = fp.bool('hotspots', `H`, false, 'Show most frequently executed covered lines.')
	ctx.show_percentages = fp.bool('percentages', `P`, true, 'Show coverage percentage per file.')
	ctx.show_test_files = fp.bool('show_test_files', `S`, false, 'Show `_test.v` files as well (normally filtered).')
	ctx.use_absolute_paths = fp.bool('absolute', `A`, false, 'Use absolute paths for all files, no matter the current folder. By default, files inside the current folder, are shown with a relative path.')
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
			found_counter_files := os.walk_ext(t, '.csv')
			if found_counter_files.len == 0 {
				log.error('Skipping ${t}, since there are 0 ${vcounter_glob_pattern} files in it')
				continue
			}
			for counterfile in found_counter_files {
				ctx.targets << counterfile
				ctx.load_meta(t)
			}
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
