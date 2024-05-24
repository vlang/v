// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import json
import os
import os.cmdline
import arrays

const tmp_dir = os.join_path(os.vtmp_dir(), 'cover')

struct VCoverData {
	file   string // file name
	hits   u64    // file coverage hits
	points u64    // file counter
}

fn report_file(covfile string) ! {
	json_content := os.read_file(covfile)!
	data := json.decode([]VCoverData, json_content)!
	hits := arrays.sum(data.map(it.hits))!
	points := arrays.sum(data.map(it.points))!
	for lineinfo in data {
		println('${(f64(lineinfo.hits) / lineinfo.points) * 100:7.2f} | ${lineinfo.hits:4u} | ${lineinfo.points:4u} | ${lineinfo.file}')
	}
	println('Total coverage: ${data.len} files, ${(f64(hits) / points) * 100:.2f}% coverage')
}

fn main() {
	args := cmdline.options_after(os.args, ['cover'])
	covfile := os.real_path(cmdline.option(args, '-file', ''))
	// covdir := os.real_path(cmdline.option(args, '-dir', ''))
	// reportfile := os.real_path(cmdline.option(args, '-report', ''))

	if covfile != '' {
		report_file(covfile)!
	}
}
