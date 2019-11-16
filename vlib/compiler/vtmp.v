// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import os
import filepath

pub fn set_vtmp_folder(vtmp string){
	// set_vtmp_folder is needed, so that external tools
	// that import the compiler module can call
	// compiler.set_vtmp_folder('their_path')
	// instead of os.setenv themselves.
	// It is also used when passing -vtmp, so that
	// subprocesses can know the toplevel VTMP too.
	os.setenv('VTMP', vtmp, true)
}

pub fn get_vtmp_folder() string {
	mut vtmp := os.getenv('VTMP')
	if vtmp.len == 0 || !os.dir_exists( vtmp ) {
		vtmp = if os.user_os() == 'windows' { os.getenv('TMP') } else { '/tmp' }
	}
	if vtmp.len == 0 || !os.dir_exists( vtmp ) {
		vtmp = filepath.join(v_modules_path, '_vtmp')
		if !os.dir_exists( vtmp ) { os.mkdir(vtmp) }
	}
	return vtmp
}

pub fn get_vtmp_filename(base_file_name string, postfix string) string {
	vtmp := get_vtmp_folder()
	return os.realpath( filepath.join(vtmp, os.filename( base_file_name ) + postfix) )
}
