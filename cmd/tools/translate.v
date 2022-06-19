// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import v.util

fn main() {
	vmodules := os.vmodules_dir()
	c2v_dir := os.join_path(vmodules, 'c2v_alpha')
	c2v_bin := os.join_path(c2v_dir, 'c2v')
	// Git clone c2v
	if !os.exists(c2v_dir) {
		println('C2V is not installed. Downloading C2V...')
		println(vmodules)
		os.chdir(vmodules)?
		res := os.execute('git clone --depth 1 git@github.com:/vlang/c2v_alpha.git')
		if res.exit_code != 0 {
			eprintln('Failed to download C2V. Perhaps it is not released yet? Is it June 20 yet?')
			return
		}
	}
	// Compile c2v
	if !os.exists(c2v_bin) {
		os.chdir(c2v_dir)?
		res2 := os.execute('v -d trace_verbose -g -o c2v -experimental -w .')
		if res2.exit_code != 0 {
			eprintln('Failed to compile C2V. This should never happen, please report it via GitHub.')
			return
		}
	}
	if os.args.len < 3 {
		eprintln('Wrong number of args. Use `v translate file.c`.')
		return
	}
	passed_args := util.args_quote_paths(os.args[2..])
	// println(passed_args)
	res := os.execute('$c2v_bin $passed_args')
	if res.exit_code != 0 {
		eprintln('C2V failed to translate this file. This should never happen, please report it via GitHub.')
	}
	println('Success!')
	// println(res.output)
}
