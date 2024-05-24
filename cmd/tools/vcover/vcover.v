// Copyright (c) 2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline

const tmp_dir = os.join_path(os.vtmp_dir(), 'cover')

fn main() {
	args := cmdline.options_after(os.args, ['cover'])
	covdir := os.real_path(cmdline.option(args, '-covdir', ''))
	reportfile := os.real_path(cmdline.option(args, '-report', ''))
	mergedir := os.real_path(cmdline.option(args, '-merge', ''))

	dump(covdir)
	dump(reportfile)
	dump(mergedir)
}
