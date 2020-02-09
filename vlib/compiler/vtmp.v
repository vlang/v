// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import os
import filepath

fn get_vtmp_folder() string {
	vtmp := filepath.join(os.tmpdir(),'v')
	if !os.is_dir(vtmp) {
		os.mkdir(vtmp)or{
			panic(err)
		}
	}
	return vtmp
}

fn get_vtmp_filename(base_file_name string, postfix string) string {
	vtmp := get_vtmp_folder()
	return os.realpath(filepath.join(vtmp,filepath.filename(os.realpath(base_file_name)) + postfix))
}
