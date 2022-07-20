// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import v.util

const vexe = os.getenv('VEXE')

fn main() {
	vmodules := os.vmodules_dir()
	c2v_dir := os.join_path(vmodules, 'c2v')
	mut c2v_bin := os.join_path(c2v_dir, 'c2v')
	$if windows {
		c2v_bin += '.exe'
	}
	// Git clone c2v
	if !os.exists(c2v_dir) {
		println('C2V is not installed. Cloning C2V to $c2v_dir ...')
		os.chdir(vmodules)?
		res := os.execute('git clone https://github.com/vlang/c2v')
		if res.exit_code != 0 {
			eprintln('Failed to download C2V.')
			exit(1)
		}
	}
	// Compile c2v
	if !os.exists(c2v_bin) {
		os.chdir(c2v_dir)?
		println('Compiling c2v ...')
		res2 := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(c2v_bin)} -keepc -g -experimental .')
		if res2.exit_code != 0 {
			eprintln(res2.output)
			eprintln('Failed to compile C2V. This should not happen. Please report it via GitHub.')
			exit(2)
		}
	}
	if os.args.len < 3 {
		eprintln('Wrong number of arguments. Use `v translate file.c` .')
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
