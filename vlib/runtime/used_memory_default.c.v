module runtime

import os

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	return error('`used_memory()` not implemented')
}
