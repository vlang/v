module main

import os
import v.vcache
import v.util

fn main() {
	wipe_path(vcache.new_cache_manager([]).basepath, 'V cache')
	wipe_path(util.get_vtmp_folder(), 'V tmp.c')
	wipe_path(os.join_path(os.temp_dir(), 'v'), 'V tests')
}

fn wipe_path(cpath string, label string) {
	if os.exists(cpath) && os.is_dir(cpath) {
		os.rmdir_all(cpath) or {}
	}
	os.mkdir_all(cpath) or {}
	println('$label folder $cpath was wiped.')
}
