module main

import json2
import os
import rand

fn test_function_hits_keep_metadata_identity_and_merge_in_json() ! {
	root := os.join_path(os.vtmp_dir(), 'vcover-report-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	file := os.join_path(root, 'source.v')
	for meta, functions in {
		'first':  [FunctionMeta{ name: 'alpha', start: 1 }, FunctionMeta{ name: 'beta', start: 2 }]
		'second': [FunctionMeta{ name: 'beta', start: 2 }, FunctionMeta{ name: 'alpha', start: 1 }]
	} {
		os.write_file(os.join_path(root, '${meta}.json'), json2.encode(MetaData{
			file:      file
			points:    [1, 2]
			functions: functions
		}))!
	}
	mut ctx := Context{}
	ctx.load_meta(root)
	ctx.load_meta(root)
	assert ctx.functions.len == 4
	counters := os.join_path(root, 'functions.fcount')
	os.write_file(counters, 'meta,position,hits\nfirst,0,2\nsecond,0,3\nsecond,1,5\n')!
	ctx.process_target(counters)!
	reported := ctx.reported_functions([])
	assert reported.len == 2
	assert reported[0].name == 'alpha'
	assert reported[0].hits == 7
	assert reported[1].name == 'beta'
	assert reported[1].hits == 3
	report := ctx.build_report([])
	assert report.files.len == 1
	assert report.files[0].functions[0].hits == 7
	assert report.files[0].functions[1].hits == 3
	assert report.files[0].function_covered == 2
	assert report.totals.function_points == 2
	assert report.totals.function_covered == 2
	decoded := json2.decode[CoverageReport](json2.encode(report))!
	assert decoded.files[0].functions[1].covered
}

fn test_malformed_function_counter_row_reports_an_error() ! {
	root := os.join_path(os.vtmp_dir(), 'vcover-invalid-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	counters := os.join_path(root, 'functions.fcount')
	os.write_file(counters, 'meta,position,hits\nfirst,0\n')!
	mut ctx := Context{}
	ctx.process_target(counters) or {
		assert err.msg().contains('invalid function counter row')
		return
	}
	assert false, 'a truncated function counter row was accepted'
}

fn test_json_report_counts_function_hits_found_only_in_a_later_meta() {
	mut ctx := Context{
		all_lines_per_file: {
			'source.v': [1]
		}
		functions:          [
			FunctionCoverage{ file: 'source.v', name: 'later', line: 1 }
			FunctionCoverage{ file: 'source.v', name: 'later', line: 1, hits: 7 },
		]
	}
	report := ctx.build_report([])
	assert report.files[0].function_points == 1
	assert report.files[0].function_covered == 1
	assert report.files[0].functions[0].hits == 7
}
