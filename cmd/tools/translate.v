// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import v.util

fn main() {
	vmodules := os.vmodules_dir()
	c2v_dir := os.join_path(vmodules, 'c2v')
	c2v_bin := os.join_path(c2v_dir, 'c2v')
	// Git clone c2v
	if !os.exists(c2v_dir) {
		println('C2V is not installed. Cloning C2V to $c2v_dir ...')
		os.chdir(vmodules)?
		res := os.execute('git clone --depth 1 git@github.com:/vlang/c2v.git')
		if res.exit_code != 0 {
			eprintln('Failed to download C2V.')
			exit(1)
		}
	}
	// Compile c2v
	if !os.exists(c2v_bin) {
		os.chdir(c2v_dir)?
		println('Compiling c2v ...')
		res2 := os.execute('v -keepc -g -experimental -o c2v .')
		if res2.exit_code != 0 {
			eprintln(res2.output)
			eprintln('Failed to compile C2V. This should never happen, please report it via GitHub.')
			exit(2)
		}
	}
	if os.args.len < 3 {
		eprintln('Wrong number of args. Use `v translate file.c`.')
		exit(3)
	}
	passed_args := util.args_quote_paths(os.args[2..])
	// println(passed_args)
	os.chdir(os.wd_at_startup)?
	res := os.system('$c2v_bin $passed_args')
	if res != 0 {
		eprintln('C2V failed to translate the C files. Please report it via GitHub.')
		exit(4)
	}
}
