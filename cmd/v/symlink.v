// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	os
	v.pref
)

fn create_symlink() {
	$if windows {
		return
	}
	vexe := pref.vexe_path()
	mut link_path := '/usr/local/bin/v'
	mut ret := os.exec('ln -sf $vexe $link_path') or { panic(err) }
	if ret.exit_code == 0 {
		println('Symlink "$link_path" has been created')
	}
	else if os.system('uname -o | grep -q \'[A/a]ndroid\'') == 0 {
		println('Failed to create symlink "$link_path". Trying again with Termux path for Android.')
		link_path = '/data/data/com.termux/files/usr/bin/v'
		ret = os.exec('ln -sf $vexe $link_path') or { panic(err) }
		if ret.exit_code == 0 {
			println('Symlink "$link_path" has been created')
		} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
		}
	} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
	}
}
