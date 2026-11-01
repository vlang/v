module szip

import os
import time

// Regression test for read_entry_bytes(): the uncompressed size stored in the
// central directory is untrusted. A crafted archive can declare a plausible
// size there (below any allocation cap) while its local header keeps the true,
// much smaller size. read_entry_bytes() must reject the entry during
// validation instead of letting a caller preallocate from the fake size.
fn test_read_entry_bytes_rejects_inconsistent_central_size() ! {
	dir := os.join_path(os.temp_dir(), 'szip_size_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}

	src := os.join_path(dir, 'a.txt')
	os.write_file(src, 'hello world')!
	archive := os.join_path(dir, 'ok.zip')
	zip_files([src], archive)!

	mut data := os.read_bytes(archive)!
	defer {
		unsafe { data.free() }
	}

	// Find the central-directory entry (PK\x01\x02) and inflate its declared
	// uncompressed size (u32 at header offset 24) to 384 MiB. The local header
	// still declares the real 11 bytes, so the two disagree.
	mut cd := -1
	for i in 0 .. data.len - 4 {
		if data[i] == 0x50 && data[i + 1] == 0x4b && data[i + 2] == 0x01 && data[i + 3] == 0x02 {
			cd = i
			break
		}
	}
	assert cd >= 0
	data[cd + 24] = 0x00
	data[cd + 25] = 0x00
	data[cd + 26] = 0x18 // 0x18000000 == 384 MiB
	data[cd + 27] = 0x00

	bad := os.join_path(dir, 'bad.zip')
	os.write_bytes(bad, data)!

	mut z := open_lazy(bad)!
	defer {
		z.close()
	}
	assert z.total()! == 1
	z.open_entry_by_index(0)!

	mut errored := false
	res := z.read_entry_bytes() or {
		errored = true
		[]u8{}
	}
	assert errored // must fail validation, not allocate 384 MiB
	unsafe { res.free() }
}
