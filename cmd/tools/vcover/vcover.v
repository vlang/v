// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import json
import os
import os.cmdline
import arrays

const tmp_dir = os.join_path(os.vtmp_dir(), 'cover')

@[heap]
struct VCoverData {
	file string // file name
mut:
	hits   u64 // file coverage hits
	points u64 // file counter
}

pub fn (covdata []&VCoverData) find(filename string) ?&VCoverData {
	for data in covdata {
		if data.file == filename {
			return unsafe { data }
		}
	}
	return none
}

pub fn (covdata []VCoverData) filter_data(filters []string) []VCoverData {
	mut arr := []VCoverData{}
	for data in covdata {
		if filters.any(data.file.contains(it)) {
			arr << data
		}
	}
	return arr
}

fn display_result(filter string, data []VCoverData) ! {
	mut covdata := data.clone()
	if filter != '' {
		filters := filter.split(',')
		covdata = covdata.filter_data(filters)
		if covdata.len == 0 {
			return error('No result found with such filter')
		}
	}
	hits := arrays.sum(covdata.map(it.hits))!
	points := arrays.sum(covdata.map(it.points))!
	for lineinfo in covdata {
		println('${(f64(lineinfo.hits) / lineinfo.points) * 100:7.2f} | ${lineinfo.hits:4u} | ${lineinfo.points:4u} | ${lineinfo.file}')
	}
	println('Total coverage: ${covdata.len} files, ${(f64(hits) / points) * 100:.2f}% coverage')
}

fn report_file(filter string, covfile string) ! {
	json_content := os.read_file(covfile)!
	data := json.decode([]VCoverData, json_content)!
	display_result(filter, data)!
}

fn summarize_coverage(covfile string, mut covdata []&VCoverData) ! {
	json_content := os.read_file(covfile)!
	data := json.decode([]VCoverData, json_content)!
	for lineinfo in data {
		if mut fileinfo := covdata.find(lineinfo.file) {
			if fileinfo.hits < lineinfo.hits {
				fileinfo.hits = lineinfo.hits
			}
		} else {
			covdata << &VCoverData{
				...lineinfo
			}
		}
	}
}

fn main() {
	args := cmdline.options_after(os.args, ['cover'])
	covfile := os.real_path(cmdline.option(args, '-file', ''))
	covdir := os.real_path(cmdline.option(args, '-dir', ''))
	filter := cmdline.option(args, '-filter', '')

	if covfile != '' {
		report_file(filter, covfile) or { println('Error: ${err}') }
	} else if covdir != '' {
		mut sum_data := []&VCoverData{}
		for coverfile in os.glob(covdir + '/vcover.*')! {
			summarize_coverage(coverfile, mut sum_data)!
		}
		display_result(filter, sum_data.map(*it)) or { println('Error: ${err}') }
	}
}
