// Under `-prod`, a `$embed_file` payload too long for a string literal is stored
// through the assembler's `.incbin` directive: the bytes go into an object the
// driver assembles and links, and the generated C only names that object. The
// array form stays for `-d no_incbin` and for generated C output.
import os

const incbin_vexe = @VEXE
const incbin_workspace = os.join_path(os.vtmp_dir(), 'embed_file_incbin_${os.getpid()}')
const incbin_payload_len = 200_000

fn testsuite_begin() {
	// The TCC that a job selects through VFLAGS cannot build `-prod`; let these builds
	// use the default compiler. The TCC refusal is checked below with an explicit `-cc tcc`.
	if os.getenv('VFLAGS').contains('tcc') {
		os.unsetenv('VFLAGS')
	}
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
	os.write_file(os.join_path(incbin_workspace, 'main.v'), "fn main() {
	data := \$embed_file('data.bin')
	println('\${data.len} \${data.to_string().hex()}')
}
") or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(incbin_workspace) or {}
}

fn expected_output() string {
	data := os.read_bytes(os.join_path(incbin_workspace, 'data.bin')) or { panic(err) }
	return '${data.len} ${data.hex()}'
}

// retained_c_file names the C source kept next to the `-o` output of a `-b c` build.
// V uses the requested output name before adding the executable suffix.
fn retained_c_file(name string) string {
	return os.join_path(incbin_workspace, name + '.c')
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

fn test_prod_incbin_build_with_a_cold_and_warm_cache() {
	$if msvc {
		// MSVC cannot assemble the payload; it keeps the array form.
		return
	}
	// A short name: the module cache nests long temporary names below it, and on
	// Windows the whole path has to stay under MAX_PATH.
	cache_dir := os.join_path(os.vtmp_dir(), 'eic_${os.getpid()}')
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
	expected := expected_output()
	seed_build, seed_output := build_and_run('app', '')
	assert seed_output == expected
	assert seed_build.output.contains('_v_embed_blob_') && seed_build.output.contains('.S'), seed_build.output
	entries := os.ls(incbin_workspace) or { panic(err) }
	assert !entries.any(it.starts_with('.app.v3cc.')), 'the incbin build directory was retained'
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
	_, warm_output := build_and_run('app_again', '')
	assert warm_output == expected
	flags_file := os.join_path(incbin_workspace, 'cached_flags.txt')
	dump_build, dump_output := build_and_run('app_cache_dump',
		'-dump-c-flags ${os.quoted_path(flags_file)}')
	assert dump_output == expected
	assert !dump_build.output.contains('.S -o'), dump_build.output
	flags := os.read_file(flags_file) or { panic(err) }
	assert !flags.contains('_v_embed_blob_'), flags
}

fn assert_tcc_prod_build_rejected(name string) {
	exe := os.join_path(incbin_workspace, name)
	main_file := os.join_path(incbin_workspace, 'main.v')
	build := os.exec([incbin_vexe, '-prod', '-cc', 'tcc', '-no-retry-compilation', '-o', exe,
		main_file])
	assert build.exit_code != 0, build.output
	assert build.output.contains('tcc is not allowed with -prod'), build.output
	assert !os.exists(exe + $if windows { '.exe' } $else { '' })
}

fn test_macos_tcc_prod_build_is_rejected_before_creating_output() {
	$if macos {
		bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		if !os.is_file(bundled_tcc) {
			return
		}
		assert_tcc_prod_build_rejected('app_tcc_prod_rejected')
	}
}

fn test_windows_tcc_prod_build_is_rejected_before_creating_output() {
	$if windows {
		bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		if !os.is_file(bundled_tcc) {
			return
		}
		assert_tcc_prod_build_rejected('app_tcc_prod_rejected')
	}
}
