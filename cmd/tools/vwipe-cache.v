module main

import os
import v.vcache

fn main() {
	wipe_path(vcache.new_cache_manager([]).basepath, 'V cache')
	wipe_path(os.vtmp_dir(), 'V tmp.c and tests folder')
}

fn wipe_path(cpath string, label string) {
	if os.exists(cpath) && os.is_dir(cpath) {
		os.rmdir_all(cpath) or {}
	}
	os.mkdir_all(cpath) or {}
	println('${label} folder ${cpath} was wiped.')
}
