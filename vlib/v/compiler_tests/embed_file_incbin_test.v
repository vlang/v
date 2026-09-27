// Under `-prod`, a `$embed_file` payload too long for a string literal is stored
// through the assembler's `.incbin` directive: the bytes go into an object the
// driver assembles and links, and the generated C only names that object. The
// array form stays for `-d no_incbin` and for generated C output.
import os
import crypto.sha256

const incbin_vexe = @VEXE
const incbin_workspace = os.join_path(os.vtmp_dir(), 'embed_file_incbin_${os.getpid()}')
const incbin_payload_len = 200_000

fn testsuite_begin() {
	os.rmdir_all(incbin_workspace) or {}
	os.mkdir_all(incbin_workspace) or { panic(err) }
	// longer than one C object is required to hold, so the array form would be
	// split; not compressible, so the payload is checked byte for byte
	mut payload := []u8{len: incbin_payload_len}
	mut state := u32(2463534242)
	for i in 0 .. payload.len {
		state ^= state << 13
		state ^= state >> 17
		state ^= state << 5
		payload[i] = u8(state)
	}
	os.write_file_array(os.join_path(incbin_workspace, 'data.bin'), payload) or { panic(err) }
	os.write_file(os.join_path(incbin_workspace, 'main.v'), "import crypto.sha256

fn main() {
	data := \$embed_file('data.bin')
	println('\${data.len} \${sha256.hexhash(data.to_string())}')
}
") or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(incbin_workspace) or {}
}

fn expected_output() string {
	data := os.read_bytes(os.join_path(incbin_workspace, 'data.bin')) or { panic(err) }
	return '${data.len} ${sha256.hexhash(data.bytestr())}'
}

fn build_and_run(name string, flags string) (os.Result, string) {
	exe := os.join_path(incbin_workspace, name)
	main_file := os.join_path(incbin_workspace, 'main.v')
	build := os.execute('${os.quoted_path(incbin_vexe)} -prod -showcc ${flags} -o ${os.quoted_path(exe)} ${os.quoted_path(main_file)}')
	assert build.exit_code == 0, build.output
	run := os.execute(os.quoted_path(exe))
	assert run.exit_code == 0, run.output
	return build, run.output.trim_space()
}

fn test_prod_build_assembles_the_payload_and_links_it() {
	build, output := build_and_run('app', '')
	assert output == expected_output()
	// the assembler ran on the generated source of the payload; the array form
	// has no such step
	assert build.output.contains('_v_embed_blob_') && build.output.contains('.S'), build.output
	entries := os.ls(incbin_workspace) or { panic(err) }
	assert !entries.any(it.starts_with('.app.v3cc.')), 'the incbin build directory was retained'
}

fn test_a_second_build_with_a_warm_module_cache_links_the_payload_again() {
	_, output := build_and_run('app_again', '')
	assert output == expected_output()
}

fn test_the_array_form_is_kept_with_no_incbin() {
	build, output := build_and_run('app_arrays', '-d no_incbin')
	assert output == expected_output()
	assert !build.output.contains('.S -o'), build.output
}

fn test_generated_c_output_spells_the_bytes_out() {
	out_c := os.join_path(incbin_workspace, 'out.c')
	main_file := os.join_path(incbin_workspace, 'main.v')
	res := os.execute('${os.quoted_path(incbin_vexe)} -prod -o ${os.quoted_path(out_c)} ${os.quoted_path(main_file)}')
	assert res.exit_code == 0, res.output
	source := os.read_file(out_c) or { panic(err) }
	assert source.contains('static const unsigned char _v_embed_blob_')
	assert !source.contains('extern const unsigned char _v_embed_blob_')
}

fn test_retained_c_output_spells_the_bytes_out() {
	build, output := build_and_run('app_retained', '-b c')
	assert output == expected_output()
	assert !build.output.contains('.S -o'), build.output
	source := os.read_file(os.join_path(incbin_workspace, 'app_retained.c')) or { panic(err) }
	assert source.contains('static const unsigned char _v_embed_blob_')
	assert !source.contains('extern const unsigned char _v_embed_blob_')
}

fn test_keepc_and_dumped_flags_do_not_use_temporary_incbin_objects() {
	keepc_build, keepc_output := build_and_run('app_keepc', '-keepc')
	assert keepc_output == expected_output()
	assert !keepc_build.output.contains('.S -o'), keepc_build.output
	flags_file := os.join_path(incbin_workspace, 'flags.txt')
	dump_build, dump_output := build_and_run('app_dump_flags',
		'-dump-c-flags ${os.quoted_path(flags_file)}')
	assert dump_output == expected_output()
	assert !dump_build.output.contains('.S -o'), dump_build.output
	flags := os.read_file(flags_file) or { panic(err) }
	assert !flags.contains('_v_embed_blob_'), flags
}
