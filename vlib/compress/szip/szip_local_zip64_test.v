import compress.szip
import encoding.binary
import encoding.hex
import os

fn local_zip64_fixture() []u8 {
	// Python zipfile.open(..., 'w', force_zip64=True), using DEFLATE and
	// a seekable stream: ZIP64 local sizes, ordinary central-directory sizes.
	return hex.decode('504b03042d000000080000002100d6ec00f4ffffffffffffffff09001400656e7472792e747874010010000d000000000000000f00000000000000abca2c3033512848acccc94f4c0100504b01022d032d000000080000002100d6ec00f40f0000000d000000090000000000000000000000800100000000656e7472792e747874504b05060000000001000100370000004a0000000000') or {
		panic(err)
	}
}

fn local_zip64_extra() []u8 {
	return local_zip64_fixture()[39..59].clone()
}

fn local_zip64_with_extra(extra []u8) []u8 {
	original := local_zip64_fixture()
	mut bytes := original[..39].clone()
	bytes << extra
	bytes << original[59..]
	binary.little_endian_put_u16(mut bytes[28..30], u16(extra.len))
	central := binary.little_endian_u32(original[original.len - 6..])
	binary.little_endian_put_u32(mut bytes[bytes.len - 6..], u32(int(central) + extra.len - 20))
	return bytes
}

fn test_zip64_local_sizes_with_ordinary_central_directory() {
	path := os.join_path(os.vtmp_dir(), 'szip_local_zip64_${os.getpid()}.zip')
	defer { os.rm(path) or {} }
	for prefix in [[]u8{}, [u8(0xef), 0xbe, 2, 0, 3, 4]] {
		mut extra := prefix.clone()
		extra << local_zip64_extra()
		os.write_file_array(path, local_zip64_with_extra(extra))!
		mut archive := szip.open(path, .no_compression, .read_only)!
		archive.open_entry('entry.txt')!
		assert archive.size() == 13
		mut contents := []u8{len: 13}
		assert archive.read_entry_buf(contents.data, contents.len)! == contents.len
		assert contents.bytestr() == 'zip64 payload'
		archive.close()
	}
}

fn test_invalid_zip64_local_sizes_are_rejected() {
	path := os.join_path(os.vtmp_dir(), 'szip_local_zip64_invalid_${os.getpid()}.zip')
	defer { os.rm(path) or {} }
	valid := local_zip64_extra()
	mut short_field := valid[..12].clone()
	short_field[2] = 8
	mut wrong_size := valid.clone()
	wrong_size[4]++
	mut wrong_csize := valid.clone()
	wrong_csize[12]++
	mut large_size := valid.clone()
	large_size[8] = 1
	mut duplicate := valid.clone()
	duplicate << valid
	for extra in [
		[]u8{},
		[u8(0xef), 0xbe, 0, 0],
		[u8(1), 0, 16],
		valid[..12].clone(),
		short_field,
		wrong_size,
		wrong_csize,
		large_size,
		duplicate,
	] {
		os.write_file_array(path, local_zip64_with_extra(extra))!
		mut rejected := false
		szip.open(path, .no_compression, .read_only) or { rejected = true }
		assert rejected, 'invalid ZIP64 local extra field ${extra} was accepted'
	}
}
