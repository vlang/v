module driver

import os
import time

#include <windows.h>

// A Windows file opened without sharing stops CreateHardLinkW from opening it
// for a link operation. Publication must survive a short-lived sharing lock.
fn close_native_cache_test_handle_later(handle voidptr) {
	time.sleep(200 * time.millisecond)
	C.CloseHandle(handle)
}

fn test_native_object_publication_retries_windows_sharing_violation() ! {
	root := os.join_path(os.vtmp_dir(), 'native_publish_windows_lock_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	temporary := os.join_path(root, 'temporary.o')
	destination := os.join_path(root, 'published.o')
	os.write_file(temporary, 'complete native object')!
	wide := temporary.to_wide()
	defer {
		unsafe { free(wide) }
	}
	handle := C.CreateFileW(wide, u32(C.GENERIC_READ), 0, unsafe { nil },
		u32(C.OPEN_EXISTING), u32(C.FILE_ATTRIBUTE_NORMAL), unsafe { nil })
	assert handle != C.INVALID_HANDLE_VALUE
	// Verify that this runner actually reproduces the sharing violation,
	// rather than passing a retry test without exercising the retry path.
	mut blocked := false
	os.link(temporary, destination) or {
		assert err.code() == 32, err.msg()
		blocked = true
	}
	if !blocked {
		C.CloseHandle(handle)
		os.rm(destination) or {}
		eprintln('skipping Windows hard-link sharing test: exclusive open did not block linking')
		return
	}
	closer := spawn close_native_cache_test_handle_later(handle)
	published := publish_native_c_object(temporary, destination) or {
		closer.wait()
		return err
	}
	closer.wait()
	assert published
	assert os.read_file(destination)! == 'complete native object'
	assert !os.exists(temporary)
	// A second writer must still lose without replacing the first object.
	late := os.join_path(root, 'late.o')
	os.write_file(late, 'different object')!
	assert !publish_native_c_object(late, destination)!
	assert os.read_file(destination)! == 'complete native object'
}
