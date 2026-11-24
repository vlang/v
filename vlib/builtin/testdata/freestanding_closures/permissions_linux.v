module main

import os

fn check_linux_permissions(callback fn () int) {
	$if linux {
		address := unsafe { u64(voidptr(callback)) }
		maps := os.read_file('/proc/self/maps') or { panic(err) }
		mut found := false
		for line in maps.split_into_lines() {
			fields := line.fields()
			bounds := fields[0].split('-')
			lo := bounds[0].parse_uint(16, 64) or { panic(err) }
			hi := bounds[1].parse_uint(16, 64) or { panic(err) }
			if lo <= address && address < hi {
				require_closure(fields[1].starts_with('r-x'))
				found = true
			}
		}
		require_closure(found)
	}
}
