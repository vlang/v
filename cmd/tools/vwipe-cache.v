module main

import os
import v.vcache

fn main() {
	_ := map{
		1: 1
	}
	mut cm := vcache.new_cache_manager([])
	cpath := cm.basepath
	if os.exists(cpath) && os.is_dir(cpath) {
		os.rmdir_all(cpath) or {}
	}
	println('V cache folder $cpath was wiped.')
}
