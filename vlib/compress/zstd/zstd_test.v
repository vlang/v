module zstd

import os

const samples_folder = os.join_path(os.dir(@FILE), 'samples')
const roundtrip_input = 'Hello world!'.repeat(10000).bytes()

fn s(fname string) string {
	return os.join_path(samples_folder, fname)
}

fn test_zstd() {
	assert version_number() >= 10505

	compressed := compress(roundtrip_input)!
	assert decompress(compressed)! == roundtrip_input
}

fn test_zstd_builds_with_cstrict_on_linux() {
	$if linux {
		root := os.join_path(os.vtmp_dir(), 'zstd_cstrict_${os.getpid()}')
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		executable := os.join_path(root, 'zstd_cstrict')
		os.write_file(source, 'import compress.zstd\n\nfn main() {\n\tprintln(zstd.version_number())\n}\n') or {
			panic(err)
		}
		compile := os.exec([@VEXE, '-new-compiler', '-cstrict', '-nocache', '-o', executable, source])
		assert compile.exit_code == 0, compile.output
	}
}

fn test_zstd_deferent_compression_level() {
	for level in [1000, 0, 1, 15] {
		compressed := compress(roundtrip_input, compression_level: level)!
		assert decompress(compressed)! == roundtrip_input, 'compression level ${level}'
	}
}

fn test_zstd_nb_threads() {
	for threads in [0, 1, 15] {
		compressed := compress(roundtrip_input, nb_threads: threads)!
		assert decompress(compressed)! == roundtrip_input, 'thread count ${threads}'
	}
}

fn test_zstd_checksum_flag() {
	for checksum in [true, false] {
		compressed := compress(roundtrip_input, checksum_flag: checksum)!
		assert decompress(compressed)! == roundtrip_input, 'checksum flag ${checksum}'
	}
}

fn test_zstd_deferent_strategy() {
	for strategy in [Strategy.default, .fast, .dfast, .greedy, .lazy, .lazy2, .btlazy2, .btopt,
		.btultra, .btultra2] {
		compressed := compress(roundtrip_input, strategy: strategy)!
		assert decompress(compressed)! == roundtrip_input, 'strategy ${strategy}'
	}
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
		input.size = uint(read_len)
		input.pos = 0
		for !finished {
			output.dst = buf_out.data
			output.size = buf_out_size
			output.pos = 0
			remaining := cctx.compress_stream2(output, input, mode)!
			fout.write(buf_out[..int(output.pos)])!
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

	mut last_ret := uint(0)
	for {
		read_len := fin.read(mut buf_in)!
		input.src = buf_in.data
		input.size = uint(read_len)
		input.pos = 0
		for input.pos < input.size {
			output.dst = buf_out.data
			output.size = buf_out_size
			output.pos = 0
			ret := dctx.decompress_stream(output, input)!
			fout.write(buf_out[..int(output.pos)])!
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

fn test_zstd_store_load_array() {
	fname := os.join_path(os.vtmp_dir(), 'zstd_mem_trace_${os.getpid()}.zst')
	defer { os.rm(fname) or {} }
	mut traces := []MemoryTrace{cap: 1000}
	for i in 0 .. 1000 {
		traces << MemoryTrace{
			operation: u64(`L`)
			address:   u64(i)
			size:      u8(i % 8)
		}
	}
	store_array[MemoryTrace](fname, traces, compression_level: 8)!
	assert load_array[MemoryTrace](fname)! == traces
}

fn assert_decompress_error(data []u8, reason string) ! {
	decompress(data) or {
		assert err.msg() == reason
		return
	}
	return error('did not error')
}

fn test_zstd_invalid_too_small() {
	assert_decompress_error([]u8{},
		'An error occurred (e.g. invalid magic number, srcSize too small)')!
}

fn test_zstd_invalid_magic_numbers() {
	assert_decompress_error([]u8{len: 100},
		'An error occurred (e.g. invalid magic number, srcSize too small)')!
}

fn test_zstd_invalid_compression() {
	mut data := []u8{len: 100}
	data[0] = 0x1f
	data[1] = 0x8b
	assert_decompress_error(data,
		'An error occurred (e.g. invalid magic number, srcSize too small)')!
}

fn test_zstd_with_corruption() {
	original := compress('Hello world!'.bytes())!
	checksum_error := "Restored data doesn't match checksum"
	for i, reason in ['Data corruption detected', 'Destination buffer is too small',
		'Src size is incorrect', 'Src size is incorrect', checksum_error, checksum_error] {
		mut compressed := original.clone()
		compressed[5 + i] = u8(0x7A)
		assert_decompress_error(compressed, reason)!
	}
	mut compressed := original.clone()
	compressed[compressed.len - 1] += 1
	assert_decompress_error(compressed, checksum_error)!
}
