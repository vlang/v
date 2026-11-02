module main

import os
import time

// Repeated-scan lifecycle regression. Every scan allocates a fresh pool and
// releases it in scan_finish/scan_abort. Under `-gc none` the pool and its
// handover buffer must be reclaimed each time; the old channel-based pool left
// the channel ring buffers and the pool allocation behind on every replacement
// scan (~200 KB per scan). This drives the full begin/step/finish/release path
// many times; run it under ASan/LeakSanitizer to confirm the allocations are
// actually reclaimed (the functional asserts here only prove the path runs and
// leaves the list in a clean, reusable state).
fn test_repeated_scan_pool_release() ! {
	dir := os.join_path(os.temp_dir(), 'vviewer_pool_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	for _ in 0 .. 40 {
		mut il := Item_list{}
		il.get_items_list([dir])
		assert !il.scanning
		assert il.loaded
		assert il.pool == unsafe { nil }
		assert il.n_item == 0 // empty folder: no images found
	}
}
