module zstd

import os

const samples_folder = os.join_path(os.dir(@FILE), 'samples')

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn test_zstd() {
	assert version_number() >= 10505

	uncompressed := 'Hello world!'.repeat(10000)
	compressed := compress(uncompressed.bytes())!
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}

fn test_zstd_deferent_compression_level() {
	uncompressed := 'Hello world!'.repeat(10000)

	compressed_1000 := compress(uncompressed.bytes(), compression_level: 1000)!
	decompressed_1000 := decompress(compressed_1000)!
	assert decompressed_1000 == uncompressed.bytes()

	compressed_0 := compress(uncompressed.bytes(), compression_level: 0)!
	decompressed_0 := decompress(compressed_0)!
	assert decompressed_0 == uncompressed.bytes()

	compressed_1 := compress(uncompressed.bytes(), compression_level: 1)!
	decompressed_1 := decompress(compressed_1)!
	assert decompressed_1 == uncompressed.bytes()

	compressed_15 := compress(uncompressed.bytes(), compression_level: 15)!
	decompressed_15 := decompress(compressed_15)!
	assert decompressed_15 == uncompressed.bytes()
}

fn test_zstd_nb_threads() {
	uncompressed := 'Hello world!'.repeat(10000)

	compressed_0 := compress(uncompressed.bytes(), nb_threads: 0)!
	decompressed_0 := decompress(compressed_0)!
	assert decompressed_0 == uncompressed.bytes()

	compressed_1 := compress(uncompressed.bytes(), nb_threads: 1)!
	decompressed_1 := decompress(compressed_1)!
	assert decompressed_1 == uncompressed.bytes()

	compressed_15 := compress(uncompressed.bytes(), nb_threads: 15)!
	decompressed_15 := decompress(compressed_15)!
	assert decompressed_15 == uncompressed.bytes()
}

fn test_zstd_checksum_flag() {
	uncompressed := 'Hello world!'.repeat(10000)

	compressed_true := compress(uncompressed.bytes(), checksum_flag: true)!
	decompressed_true := decompress(compressed_true)!
	assert decompressed_true == uncompressed.bytes()

	compressed_false := compress(uncompressed.bytes(), checksum_flag: false)!
	decompressed_false := decompress(compressed_false)!
	assert decompressed_false == uncompressed.bytes()
}

fn test_zstd_deferent_strategy() {
	uncompressed := 'Hello world!'.repeat(10000)

	compressed_default := compress(uncompressed.bytes(), strategy: .default)!
	decompressed_default := decompress(compressed_default)!
	assert decompressed_default == uncompressed.bytes()

	compressed_fast := compress(uncompressed.bytes(), strategy: .fast)!
	decompressed_fast := decompress(compressed_fast)!
	assert decompressed_fast == uncompressed.bytes()

	compressed_dfast := compress(uncompressed.bytes(), strategy: .dfast)!
	decompressed_dfast := decompress(compressed_dfast)!
	assert decompressed_dfast == uncompressed.bytes()

	compressed_greedy := compress(uncompressed.bytes(), strategy: .greedy)!
	decompressed_greedy := decompress(compressed_greedy)!
	assert decompressed_greedy == uncompressed.bytes()

	compressed_lazy := compress(uncompressed.bytes(), strategy: .lazy)!
	decompressed_lazy := decompress(compressed_lazy)!
	assert decompressed_lazy == uncompressed.bytes()

	compressed_lazy2 := compress(uncompressed.bytes(), strategy: .lazy2)!
	decompressed_lazy2 := decompress(compressed_lazy2)!
	assert decompressed_lazy2 == uncompressed.bytes()

	compressed_btlazy2 := compress(uncompressed.bytes(), strategy: .btlazy2)!
	decompressed_btlazy2 := decompress(compressed_btlazy2)!
	assert decompressed_btlazy2 == uncompressed.bytes()

	compressed_btopt := compress(uncompressed.bytes(), strategy: .btopt)!
	decompressed_btopt := decompress(compressed_btopt)!
	assert decompressed_btopt == uncompressed.bytes()

	compressed_btultra := compress(uncompressed.bytes(), strategy: .btultra)!
	decompressed_btultra := decompress(compressed_btultra)!
	assert decompressed_btultra == uncompressed.bytes()

	compressed_btultra2 := compress(uncompressed.bytes(), strategy: .btultra2)!
	decompressed_btultra2 := decompress(compressed_btultra2)!
	assert decompressed_btultra2 == uncompressed.bytes()
}

fn compress_file(fname string, oname string, params CompressParams) ! {
	mut fin := os.open_file(fname, 'rb')!
	mut fout := os.open_file(oname, 'wb')!
	defer {
		fin.close()
		fout.close()
	}

	mut buf_in := []u8{len: buf_in_size}
	mut buf_out := []u8{len: buf_out_size}

	mut cctx := new_cctx(params)!
	defer {
		cctx.free_cctx()
	}

	mut last_chunk := false
	mut input := &InBuffer{
		src:  buf_in.data
		size: 0
		pos:  0
	}
	mut output := &OutBuffer{
		dst:  buf_out.data
		size: 0
		pos:  0
	}
	for !last_chunk {
		read_len := fin.read(mut buf_in)!
		last_chunk = read_len < buf_in_size
		mode := if last_chunk {
			EndDirective.end
		} else {
			EndDirective.continue
		}

		mut finished := false
		input.src = buf_in.data
		input.size = usize(read_len)
		input.pos = 0
		for !finished {
			output.dst = buf_out.data
			output.size = buf_out_size
			output.pos = 0
			remaining := cctx.compress_stream2(output, input, mode)!
			fout.write(buf_out[..output.pos])!
			finished = if last_chunk { remaining == 0 } else { input.pos == input.size }
		}

		if input.pos != input.size {
			return error('Impossible: zstd only returns 0 when the input is completely consumed!')
		}
	}
}

fn decompress_file(fname string, oname string, params DecompressParams) ! {
	mut fin := os.open_file(fname, 'rb')!
	mut fout := os.open_file(oname, 'wb')!
	defer {
		fin.close()
		fout.close()
	}

	mut buf_in := []u8{len: buf_in_size}
	mut buf_out := []u8{len: buf_out_size}

	mut dctx := new_dctx(params)!
	defer {
		dctx.free_dctx()
	}

	mut input := &InBuffer{
		src:  buf_in.data
		size: 0
		pos:  0
	}
	mut output := &OutBuffer{
		dst:  buf_out.data
		size: 0
		pos:  0
	}

	mut last_ret := usize(0)
	for {
		read_len := fin.read(mut buf_in)!
		input.src = buf_in.data
		input.size = usize(read_len)
		input.pos = 0
		for input.pos < input.size {
			output.dst = buf_out.data
			output.size = buf_out_size
			output.pos = 0
			ret := dctx.decompress_stream(output, input)!
			fout.write(buf_out[..output.pos])!
			last_ret = ret
		}
		if read_len < buf_in.len {
			break
		}
	}
	if last_ret != 0 {
		/* The last return value from DecompressStream did not end on a
         * frame, but we reached the end of the file! We assume this is an
         * error, and the input was truncated.
         */
		return error('EOF before end of stream: ${last_ret}')
	}
}

// zstd stream mode test
fn test_zstd_stream() {
	decompress_file(s('readme_level_19.zst'), s('tmp_file1'))!
	compress_file(s('tmp_file1'), s('tmp_file.zstd'),
		compression_level: 6
		nb_threads:        1
		checksum_flag:     true
	)!
	decompress_file(s('tmp_file.zstd'), s('tmp_file2'))!
	file1 := os.read_file(s('tmp_file1'))!
	assert file1.contains('## Acknowledgement')
	assert file1.contains('## Troubleshooting')
	file2 := os.read_file(s('tmp_file2'))!
	assert file1 == file2
	os.rm(s('tmp_file1'))!
	os.rm(s('tmp_file2'))!
	os.rm(s('tmp_file.zstd'))!
}

// store_array compress an `array`'s data, and store it to file `fname`.
// extra compression parameters can be set by `params`
// WARNING: Because struct padding, some data in struct may be marked unused.
// So, when `store_array`, it will cause memory fsanitize fail with 'use-of-uninitialized-value'.
// It can be safely ignored.
// For example, following struct may cause memory fsanitize fail:
// struct MemoryTrace {
// 	operation u8
// 	address   u64
// 	size      u8
// }
// By changing it into following , you can pass the memory fsanitize check :
// struct MemoryTrace {
// 	operation u64
// 	address   u64
// 	size      u64
// }
struct MemoryTrace {
	operation u64
	address   u64
	size      u64
}

fn store_array_test(fname string) ! {
	// Create a big array
	mut store_memory_trace := []MemoryTrace{cap: 1000}
	for i in 0 .. 1000 {
		store_memory_trace << MemoryTrace{
			operation: u64(`L`)
			address:   u64(i)
			size:      u8(i % 8)
		}
	}
	store_array[MemoryTrace](fname, store_memory_trace, compression_level: 8)!
}

fn load_array_test(fname string) ! {
	load_memory_trace := load_array[MemoryTrace](fname)!
	for i in 0 .. 1000 {
		assert load_memory_trace[i].operation == `L`
		assert load_memory_trace[i].address == i
		assert load_memory_trace[i].size == u8(i % 8)
	}
}

fn test_zstd_store_load_array() {
	store_array_test(s('mem_trace.zstd'))!
	load_array_test(s('mem_trace.zstd'))!
	os.rm(s('mem_trace.zstd'))!
}

fn assert_decompress_error(data []u8, reason string) ! {
	decompress(data) or {
		assert err.msg() == reason
		return
	}
	return error('did not error')
}

fn test_zstd_invalid_too_small() {
	assert_decompress_error([]u8{}, 'An error occurred (e.g. invalid magic number, srcSize too small)')!
}

fn test_zstd_invalid_magic_numbers() {
	assert_decompress_error([]u8{len: 100}, 'An error occurred (e.g. invalid magic number, srcSize too small)')!
}

fn test_zstd_invalid_compression() {
	mut data := []u8{len: 100}
	data[0] = 0x1f
	data[1] = 0x8b
	assert_decompress_error(data, 'An error occurred (e.g. invalid magic number, srcSize too small)')!
}

fn test_zstd_with_corruption1() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[5] = u8(0x7A)
	assert_decompress_error(compressed, 'Data corruption detected')!
}

fn test_zstd_with_corruption2() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[6] = u8(0x7A)
	assert_decompress_error(compressed, 'Destination buffer is too small')!
}

fn test_zstd_with_corruption3() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[7] = u8(0x7A)
	assert_decompress_error(compressed, 'Src size is incorrect')!
}

fn test_zstd_with_corruption4() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[8] = u8(0x7A)
	assert_decompress_error(compressed, 'Src size is incorrect')!
}

fn test_zstd_with_corruption5() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[9] = u8(0x7A)
	assert_decompress_error(compressed, "Restored data doesn't match checksum")!
}

fn test_zstd_with_corruption6() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[10] = u8(0x7A)
	assert_decompress_error(compressed, "Restored data doesn't match checksum")!
}

fn test_zstd_with_corruption7() {
	uncompressed := 'Hello world!'
	mut compressed := compress(uncompressed.bytes())!
	compressed[compressed.len - 1] += 1
	assert_decompress_error(compressed, "Restored data doesn't match checksum")!
}
