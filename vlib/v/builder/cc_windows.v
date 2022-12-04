// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v.pref

pub fn (mut v Builder) quote_compiler_name(name string) string {
	// some compiler frontends on windows, like emcc, are a .bat file on windows.
	// Quoting the .bat file name here leads to problems with them, when they internally call python scripts for some reason.
	// Just emcc without quotes here does work, but:
	// |"emcc" -v| produces: python.exe: can't open file 'D:\programs\v\emcc.py': [Errno 2] No such file or directory
	if name.contains('/') || name.contains('\\') {
		return os.quoted_path(name)
	}
	return name
}

pub fn (mut v Builder) find_win_cc() ! {
	$if !windows {
		return
	}
	cmd_version := '${v.quote_compiler_name(v.pref.ccompiler)} -v'
	ccompiler_version_res := os.execute(cmd_version)
	if ccompiler_version_res.exit_code != 0 {
		if v.pref.is_verbose {
			println('failed command: `${cmd_version}`')
			println('${v.pref.ccompiler} not found, looking for msvc...')
		}
		find_msvc(v.pref.m64) or {
			if v.pref.is_verbose {
				println('msvc not found, looking for thirdparty/tcc...')
			}
			vpath := os.dir(pref.vexe_path())
			thirdparty_tcc := os.join_path(vpath, 'thirdparty', 'tcc', 'tcc.exe')
			tcc_version_res := os.execute('${os.quoted_path(thirdparty_tcc)} -v')
			if tcc_version_res.exit_code != 0 {
				if v.pref.is_verbose {
					println('tcc not found')
				}
				return error('tcc not found')
			}
			v.pref.ccompiler = thirdparty_tcc
			v.pref.ccompiler_type = .tinyc
			return
		}
		v.pref.ccompiler = 'msvc'
		v.pref.ccompiler_type = .msvc
		return
	}
	v.pref.ccompiler_type = pref.cc_from_string(v.pref.ccompiler)
}
