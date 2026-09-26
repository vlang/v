module main

import os

fn main() {
	cache_path := os.getenv_opt('VCACHE') or { os.join_path(os.vmodules_dir(), '.cache') }
	wipe_path(cache_path, 'V cache')
	wipe_path(os.vtmp_dir(), 'V tmp.c and tests folder')
}

fn wipe_path(cpath string, label string) {
	if os.exists(cpath) && os.is_dir(cpath) {
		os.rmdir_all(cpath) or {}
	}
	os.mkdir_all(cpath) or {}
	println('${label} folder ${cpath} was wiped.')
}
