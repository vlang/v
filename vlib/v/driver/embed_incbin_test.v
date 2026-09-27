module driver

import os
import v.gen.c as cgen
import v.pref

fn test_v3_embed_incbin_supported_keeps_the_array_form_where_no_object_is_linked() {
	assert v3_embed_incbin_supported('linux', 'linux', 'gcc', 'c', false, false, false, false,
		[])
	assert v3_embed_incbin_supported('macos', 'macos', 'tinyc', 'c', false, false, false, false,
		[])
	assert v3_embed_incbin_supported('windows', 'windows', 'gcc', 'c', false, false, false, false,
		[])
	// generated C and object output are linked elsewhere
	assert !v3_embed_incbin_supported('linux', 'linux', 'gcc', 'c', true, false, false, false,
		[])
	assert !v3_embed_incbin_supported('linux', 'linux', 'gcc', 'c', false, true, false, false,
		[])
	assert !v3_embed_incbin_supported('linux', 'linux', 'gcc', 'c', false, false, false, true,
		[])
	// other backends, MSVC, and targets whose object format the host assembler
	// does not produce
	assert !v3_embed_incbin_supported('linux', 'linux', 'gcc', 'fastc', false, false, false,
		false, [])
	assert !v3_embed_incbin_supported('windows', 'windows', 'msvc', 'c', false, false, false,
		false, [])
	assert !v3_embed_incbin_supported('windows', 'linux', 'gcc', 'c', false, false, false,
		false, [])
	assert !v3_embed_incbin_supported('ios', 'macos', 'clang', 'c', false, false, false,
		false, [])
	assert !v3_embed_incbin_supported('wasm32_emscripten', 'linux', 'emcc', 'c', false, false,
		false, false, [])
	assert !v3_embed_incbin_supported('linux', 'macos', 'cc', 'c', false, false, true, false,
		[])
	// the explicit escape hatch
	assert !v3_embed_incbin_supported('linux', 'linux', 'gcc', 'c', false, false, false, false, [
		'no_incbin',
	])
}

fn test_v3_embed_incbin_assembly_flags_keep_object_abi_options() {
	assert v3_embed_incbin_assembly_flags(['-O2', '-m32', '-Wl,-z,relro', '-target',
		'x86_64-unknown-linux-gnu', '-DNAME=value', '--target=aarch64-linux-gnu', '-arch', 'x86_64',
		'-mabi=lp64', '-march=armv8-a', '-mcpu=generic', '-fPIC', 'source.o']) == [
		'-m32',
		'-target',
		'x86_64-unknown-linux-gnu',
		'--target=aarch64-linux-gnu',
		'-arch',
		'x86_64',
		'-mabi=lp64',
		'-march=armv8-a',
		'-mcpu=generic',
	]
}

fn test_v3_embed_incbin_assembly_names_the_object_and_the_payload_file() {
	source := v3_embed_incbin_assembly('abc123', '/tmp/dir with "quotes"\\x/_v_embed_blob_abc123.bin',
		4096)
	assert source.contains('.incbin "/tmp/dir with \\"quotes\\"\\\\x/_v_embed_blob_abc123.bin"')
	// the C name, and its Mach-O / Windows x86 spelling with the leading underscore
	assert source.contains('.globl _v_embed_blob_abc123\n')
	assert source.contains('.globl __v_embed_blob_abc123\n')
	assert source.contains('_v_embed_blob_abc123:\n')
	assert source.contains('.size _v_embed_blob_abc123, 4096')
	// not exported from a shared library
	assert source.contains('.hidden _v_embed_blob_abc123')
	assert source.contains('.private_extern __v_embed_blob_abc123')
	assert source.contains('.note.GNU-stack')
}

fn test_assemble_v3_embed_incbin_objects_produces_an_object_holding_the_bytes() {
	$if windows {
		return
	}
	assembler := pref.find_system_assembler() or {
		eprintln('skipping: no GCC or Clang compatible assembler on PATH')
		return
	}
	build_dir := os.join_path(os.vtmp_dir(), 'v3_embed_incbin_test_${os.getpid()}')
	os.rmdir_all(build_dir) or {}
	os.mkdir_all(build_dir) or { panic(err) }
	defer {
		os.rmdir_all(build_dir) or {}
	}
	mut payload := []u8{len: 70000}
	for i in 0 .. payload.len {
		payload[i] = u8((i * 7 + 3) & 0xff)
	}
	payloads := [
		cgen.EmbedIncbinPayload{
			symbol:  'testpayload'
			payload: payload.bytestr()
		},
	]
	objects := assemble_v3_embed_incbin_objects(payloads, assembler, [], build_dir, false) or {
		panic(err)
	}
	assert objects.len == 1
	assert os.is_file(objects[0])
	// the object carries the bytes verbatim, at some offset
	object := os.read_bytes(objects[0]) or { panic(err) }
	assert object.len > payload.len
	needle := payload[..64].clone()
	mut found := false
	for start in 0 .. object.len - needle.len {
		if object[start..start + needle.len] == needle {
			found = true
			break
		}
	}
	assert found, 'the assembled object does not contain the payload'
}
