module embed_file

import os

struct BootstrapDecoder {
	mask u8
	fail bool
}

fn (decoder BootstrapDecoder) decompress(data []u8) ![]u8 {
	if decoder.fail {
		return error('decoder rejected payload')
	}
	return data.map(it ^ decoder.mask)
}

fn compressed_decoder_fixture(algorithm string, compressed &[]u8) EmbedFileData {
	return EmbedFileData{
		compression_type:  algorithm
		compressed:        compressed.data
		compressed_len:    compressed.len
		free_uncompressed: true
		len:               compressed.len
		path:              'decoder-bootstrap.bin'
	}
}

fn free_decoder_fixture(mut embedded EmbedFileData) {
	// Each fixture owns the decoded heap buffer allocated by data().
	unsafe { embedded.free() }
}

fn test_decoder_lookup_preserves_registration_and_cached_data() {
	algorithm := 'bootstrap-test-xor'
	compressed := [u8(1), 2, 3, 0xff]
	register_decoder(algorithm, BootstrapDecoder{ mask: 0x55 })
	mut first := compressed_decoder_fixture(algorithm, &compressed)
	defer {
		free_decoder_fixture(mut first)
	}
	assert first.to_bytes() == compressed.map(it ^ u8(0x55))
	cached := first.data()

	register_decoder(algorithm, BootstrapDecoder{ mask: 0xaa })
	assert first.data() == cached
	assert first.to_bytes() == compressed.map(it ^ u8(0x55))

	mut second := compressed_decoder_fixture(algorithm, &compressed)
	defer {
		free_decoder_fixture(mut second)
	}
	assert second.to_bytes() == compressed.map(it ^ u8(0xaa))
	// The lookup must retain the registered interface for later embedded files.
	mut third := compressed_decoder_fixture(algorithm, &compressed)
	defer {
		free_decoder_fixture(mut third)
	}
	assert third.to_bytes() == second.to_bytes()
}

fn test_decoder_lookup_reports_unknown_and_failed_compression() {
	scenario := os.getenv('V_EMBED_DECODER_BOOTSTRAP_SCENARIO')
	if scenario != '' {
		algorithm := 'bootstrap-test-${scenario}'
		if scenario == 'failure' {
			register_decoder(algorithm, BootstrapDecoder{ fail: true })
		}
		compressed := [u8(1), 2, 3]
		mut embedded := compressed_decoder_fixture(algorithm, &compressed)
		embedded.data()
		assert false, 'expected decoder failure'
		return
	}
	for mode, expected in {
		'unknown': 'unknown compression of "decoder-bootstrap.bin": "bootstrap-test-unknown"'
		'failure': 'decompression of "decoder-bootstrap.bin" failed: decoder rejected payload'
	} {
		mut process := os.new_process(os.executable())
		mut environment := os.environ()
		environment['V_EMBED_DECODER_BOOTSTRAP_SCENARIO'] = mode
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		assert process.code != 0, output
		assert output.contains('EmbedFileData error: ${expected}'), output
		process.close()
	}
}
