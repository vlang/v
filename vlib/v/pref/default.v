// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import (
	filepath
	os
)

pub const (
	default_module_path = os.home_dir() + '.vmodules'
)

pub fn (p mut Preferences) fill_with_defaults() {
	if p.vroot == '' {
		// Location of all vlib files
		p.vroot = filepath.dir(vexe_path())
	}
	if p.vlib_path == '' {
		p.vlib_path = filepath.join(p.vroot,'vlib')
	}
	if p.vpath == '' {
		p.vpath = default_module_path
	}
	if p.out_name == ''{
		if p.path.ends_with('.v') && p.path != '.v' {
			p.out_name = p.path[..p.path.len - 2]
		}
		// if we are in `/foo` and run `v .`, the executable should be `foo`
		if p.path == '.' && p.out_name == '' {
			base := os.getwd().all_after(os.path_separator)
			p.out_name = base.trim_space()
		}
		if p.out_name == 'v' && os.is_dir('vlib/compiler') {
			// Building V? Use v2, since we can't overwrite a running
			// executable on Windows + the precompiled V is more
			// optimized.
			println('Saving the resulting V executable in `./v2`')
			println('Use `v -o v vlib/cmd/v` if you want to replace current ' + 'V executable.')
			p.out_name = 'v2'
		}
	}
	if p.os == ._auto {
		// No OS specifed? Use current system
		$if linux {
			p.os = .linux
		}
		$if macos {
			p.os = .mac
		}
		$if windows {
			p.os = .windows
		}
		$if freebsd {
			p.os = .freebsd
		}
		$if openbsd {
			p.os = .openbsd
		}
		$if netbsd {
			p.os = .netbsd
		}
		$if dragonfly {
			p.os = .dragonfly
		}
		$if solaris {
			p.os = .solaris
		}
		$if haiku {
			p.os = .haiku
		}
	}
	if p.ccompiler == '' {
		p.ccompiler = default_c_compiler()
	}
}

fn default_c_compiler() string {
	// fast_clang := '/usr/local/Cellar/llvm/8.0.0/bin/clang'
	// if os.exists(fast_clang) {
	// return fast_clang
	// }
	// TODO fix $if after 'string'
	$if windows {
		return 'gcc'
	}
	return 'cc'
}

//TODO Remove code duplication
fn vexe_path() string {
	vexe := os.getenv('VEXE')
	if vexe != '' {
		return vexe
	}
	real_vexe_path := os.realpath(os.executable())
	os.setenv('VEXE', real_vexe_path, true)
	return real_vexe_path
}
