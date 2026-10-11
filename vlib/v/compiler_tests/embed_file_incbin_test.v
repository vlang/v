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

// retained_c_file names the C source kept next to the `-o` output of a `-b c` build.
// V derives it from the output name verbatim and appends `.exe` to that name on
// Windows, so the file is `app_retained.c` elsewhere and `app_retained.exe.c` there.
fn retained_c_file(name string) string {
	return os.join_path(incbin_workspace, name + $if windows { '.exe' } $else { '' } + '.c')
}

fn build_and_run(name string, flags string) (os.Result, string) {
	exe := os.join_path(incbin_workspace, name)
	main_file := os.join_path(incbin_workspace, 'main.v')
	build := os.exec([incbin_vexe, '-prod', '-showcc', ...(os.split_args(flags) or { panic(err) }),
		'-o', exe, main_file])
	assert build.exit_code == 0, build.output
	// V writes the executable next to the `-o` name, adding `.exe` on Windows, and
	// `os.exec` does not add it, so name the file that is actually there. The `-o`
	// argument keeps the bare name, which is what the retained-C tests below read.
	run := os.exec([exe + $if windows { '.exe' } $else { '' }])
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
	res := os.exec([incbin_vexe, '-prod', '-o', '${out_c}', main_file])
	assert res.exit_code == 0, res.output
	source := os.read_file(out_c) or { panic(err) }
	assert source.contains('static const unsigned char _v_embed_blob_')
	assert !source.contains('extern const unsigned char _v_embed_blob_')
}

fn test_retained_c_output_spells_the_bytes_out() {
	build, output := build_and_run('app_retained', '-b c')
	assert output == expected_output()
	assert !build.output.contains('.S -o'), build.output
	source := os.read_file(retained_c_file('app_retained')) or { panic(err) }
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

fn test_dump_flags_after_cached_incbin_build_uses_arrays() {
	cache_dir := os.join_path(os.vtmp_dir(), 'embed_file_incbin_cache_${os.getpid()}')
	os.rmdir_all(cache_dir) or {}
	old_cache := os.getenv_opt('V3CACHE')
	old_trace := os.getenv_opt('V3_CACHE_TRACE')
	os.setenv('V3CACHE', cache_dir, true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	defer {
		if value := old_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		if value := old_trace {
			os.setenv('V3_CACHE_TRACE', value, true)
		} else {
			os.unsetenv('V3_CACHE_TRACE')
		}
		os.rmdir_all(cache_dir) or {}
	}
	seed_build, seed_output := build_and_run('app_cache_seed', '')
	assert seed_output == expected_output()
	assert seed_build.output.contains('_v_embed_blob_') && seed_build.output.contains('.S'), seed_build.output
	mut cached_incbin_plan := false
	for path in os.walk_ext(cache_dir, '.c') {
		if os.base(path).starts_with('program_') {
			source := os.read_file(path) or { panic(err) }
			if source.contains('extern const unsigned char _v_embed_blob_') {
				cached_incbin_plan = true
				break
			}
		}
	}
	// A build whose V-shipped native inputs cannot be replicated into every cached
	// object (such as a header that keeps file-static state) stays uncached and
	// leaves no plan to reuse. Any other seed build caches its incbin plan.
	uncached := seed_build.output.contains('external C inputs cannot be assigned to cache units')
	assert cached_incbin_plan != uncached, 'the seed build neither cached its incbin C plan nor bypassed the cache:\n${seed_build.output}'
	flags_file := os.join_path(incbin_workspace, 'cached_flags.txt')
	dump_build, dump_output := build_and_run('app_cache_dump',
		'-dump-c-flags ${os.quoted_path(flags_file)}')
	assert dump_output == expected_output()
	assert !dump_build.output.contains('.S -o'), dump_build.output
	flags := os.read_file(flags_file) or { panic(err) }
	assert !flags.contains('_v_embed_blob_'), flags
}

fn test_macos_tcc_build_keeps_the_array_form() {
	$if macos {
		bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		if !os.is_file(bundled_tcc) {
			return
		}
		build, output := build_and_run('app_tcc', '-cc tcc -no-retry-compilation')
		assert output == expected_output()
		assert !build.output.contains('.S -o'), build.output
	}
}

fn test_windows_tcc_build_keeps_the_array_form() {
	$if windows {
		bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		if !os.is_file(bundled_tcc) {
			return
		}
		// `-no-retry-compilation` turns off the retry that would hide the problem: TCC
		// rejects the object the host assembler produced for the payload, so with the
		// incbin path this build only succeeds through a fallback to that host.
		build, output := build_and_run('app_tcc', '-cc tcc -no-retry-compilation')
		assert output == expected_output()
		assert !build.output.contains('.S -o'), build.output
	}
}
