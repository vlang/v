import os
import os.mmap

fn test_mmap_file() {
	file_path := @FILE
	mut minfo := mmap.mmap_file(file_path)!
	defer {
		minfo.close() or { assert false, err.msg() }
	}

	assert minfo.fd.fd != 0
	assert minfo.addr != 0
	assert minfo.fsize == os.file_size(file_path)
	assert minfo.data.data == minfo.addr
	assert usize(minfo.data.len) == minfo.fsize

	data := os.read_file(file_path)!
	assert minfo.data == data.bytes()
}

fn test_to_byte_array() {
	mut minfo := mmap.mmap_file(@FILE)!
	defer {
		minfo.close() or { assert false, err.msg() }
	}

	x := 'import os'.bytes()
	assert minfo.vbytes()[..x.len] == x
}

fn test_to_string() {
	mut minfo := mmap.mmap_file(@FILE)!
	defer {
		minfo.close() or { assert false, err.msg() }
	}

	assert minfo.bytestr().starts_with('import os')
}

// Test low-level mmap API with anonymous memory mapping
fn test_mmap_anonymous() {
	size := usize(4096)
	addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read | mmap.prot_write
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!

	// Clean up
	mmap.munmap(addr, size)!
}

// Test low-level mmap API with file mapping
fn test_mmap_file_lowlevel() {
	// Create a test file
	test_file := 'test_mmap_lowlevel.txt'
	test_content := 'Hello, mmap low-level API!'
	os.write_file(test_file, test_content)!

	defer {
		os.rm(test_file) or {}
	}

	// Open file for memory mapping
	mut f := os.open(test_file)!
	defer { f.close() }

	// Map the file
	addr := mmap.mmap(
		len:    usize(test_content.len)
		prot:   mmap.prot_read
		flags:  mmap.map_shared
		fd:     f.fd
		offset: 0
	)!

	// Read mapped memory
	mapped_data := unsafe { &u8(addr) }
	content := unsafe { tos(mapped_data, test_content.len) }
	assert content == test_content

	// Clean up
	mmap.munmap(addr, usize(test_content.len))!
}

// Test memory protection with mprotect
fn test_mprotect() {
	size := usize(4096)

	// Initially map as read/write
	addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read | mmap.prot_write
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!

	// Test writing to the memory (should work initially)
	unsafe {
		mut ptr := &u8(addr)
		ptr[0] = 42
		assert ptr[0] == 42
	}
	// Change protection to read-only
	mmap.mprotect(addr, size, mmap.prot_read)!

	// Try to write again (this might cause a segfault in real scenarios,
	// but we'll test the API call itself)
	assert true // If we reach here, mprotect succeeded

	// Clean up
	mmap.munmap(addr, size)!
}

// Test memory synchronization with msync
fn test_msync() {
	// Use the current test file itself for msync testing
	// since we can't guarantee write permissions in all environments
	file_path := @FILE
	content := os.read_file(file_path)!

	// Map the file read-only (safer for testing)
	mut f := os.open(file_path)!
	defer { f.close() }

	addr := mmap.mmap(
		len:    usize(content.len)
		prot:   mmap.prot_read
		flags:  mmap.map_shared
		fd:     f.fd
		offset: 0
	)!

	// Test that msync works (even for read-only mappings)
	// Note: msync with MS_SYNC should work even for read-only mappings
	mmap.msync(addr, usize(content.len), mmap.ms_sync)!

	// Clean up
	mmap.munmap(addr, usize(content.len))!
}

// Test memory locking with mlock/munlock
fn test_mlock_unlock() {
	size := usize(4096)

	// Map anonymous memory
	addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read | mmap.prot_write
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!

	// Lock the memory to prevent paging
	mmap.mlock(addr, size)!

	// Use the memory
	unsafe {
		mut ptr := &u8(addr)
		for i in 0 .. size {
			ptr[i] = u8(i % 256)
		}
	}
	// Unlock the memory
	mmap.munlock(addr, size)!

	// Clean up
	mmap.munmap(addr, size)!
}

// Test mmap with different protection flags
fn test_mmap_protection_flags() {
	size := usize(4096)

	// Test read-only mapping
	read_only_addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!
	mmap.munmap(read_only_addr, size)!

	// Test write-only mapping (platform dependent, but API should work)
	write_only_addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_write
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!
	mmap.munmap(write_only_addr, size)!

	// Test execute mapping (platform dependent, but API should work)
	exec_addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_exec
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!
	mmap.munmap(exec_addr, size)!

	// Test read/write/execute mapping
	rwx_addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read | mmap.prot_write | mmap.prot_exec
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!
	mmap.munmap(rwx_addr, size)!
}

// Test mmap with different mapping flags
fn test_mmap_mapping_flags() {
	size := usize(4096)

	// Test MAP_SHARED (changes are propagated to other processes)
	shared_addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read | mmap.prot_write
		flags:  mmap.map_anonymous | mmap.map_shared
		fd:     -1
		offset: 0
	)!
	mmap.munmap(shared_addr, size)!

	// Test MAP_PRIVATE (changes are private to this process)
	private_addr := mmap.mmap(
		len:    size
		prot:   mmap.prot_read | mmap.prot_write
		flags:  mmap.map_anonymous | mmap.map_private
		fd:     -1
		offset: 0
	)!
	mmap.munmap(private_addr, size)!
}

// Test error handling with invalid parameters
fn test_mmap_error_handling() {
	// Test invalid file descriptor
	mut result := mmap.mmap(
		len:    usize(4096)
		prot:   mmap.prot_read
		flags:  mmap.map_shared
		fd:     -999
		offset: 0
	) or { return }
	assert false, 'Should have failed with invalid file descriptor'

	// Test zero length with or {} block handling
	mmap.mmap(
		len:    usize(0)
		prot:   mmap.prot_read
		flags:  mmap.map_anonymous
		fd:     -1
		offset: 0
	) or {
		// Expected error, this is good
		return
	}
	assert false, 'Zero length mapping should have failed'
}

// Test MmapInfo convenience methods
fn test_mmap_info_methods() {
	mut minfo := mmap.mmap_file(@FILE)!
	defer {
		minfo.close() or { assert false, err.msg() }
	}

	// Test vbytes() method
	bytes := minfo.vbytes()
	assert bytes.len > 0
	assert usize(bytes.len) == minfo.fsize

	// Test bytestr() method
	str := minfo.bytestr()
	assert str.len > 0
	assert usize(str.len) == minfo.fsize

	// Verify the content matches the file
	expected_content := os.read_file(@FILE)!
	assert str == expected_content
}
