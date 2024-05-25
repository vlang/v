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

fn display_result(data []VCoverData) ! {
	hits := arrays.sum(data.map(it.hits))!
	points := arrays.sum(data.map(it.points))!
	for lineinfo in data {
		println('${(f64(lineinfo.hits) / lineinfo.points) * 100:7.2f} | ${lineinfo.hits:4u} | ${lineinfo.points:4u} | ${lineinfo.file}')
	}
	println('Total coverage: ${data.len} files, ${(f64(hits) / points) * 100:.2f}% coverage')
}

fn report_file(covfile string) ! {
	json_content := os.read_file(covfile)!
	data := json.decode([]VCoverData, json_content)!
	display_result(data)!
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

	if covfile != '' {
		report_file(covfile)!
	} else if covdir != '' {
		mut sum_data := []&VCoverData{}
		for coverfile in os.glob(covdir + '/vcover.*')! {
			summarize_coverage(coverfile, mut sum_data)!
		}
		display_result(sum_data.map(*it))!
	}
}
