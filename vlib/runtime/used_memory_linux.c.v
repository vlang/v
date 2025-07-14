module runtime

import os

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	file := '/proc/self/status'
	content := os.read_file(file) or { return 0 }
	for line in content.split_into_lines() {
		if line.starts_with('VmRSS:') {
			parts := line.split(':')
			if parts.len > 1 {
				value := parts[1].trim_space().replace('kB', '').trim_space()
				return value.u64() * 1024 // Convert to bytes
			}
		}
	}
	return 0
}
