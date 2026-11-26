import os

const embed_freestanding_program = '
module main
import v.embed_file

const text = \$embed_file("assets/text.txt")
const binary = \$embed_file("assets/binary.bin")
const empty = \$embed_file("assets/empty.bin")
const nested = \$embed_file("./assets/subdirectory/data.bin")
const large = \$embed_file("assets/large.bin")

fn local_payload() embed_file.EmbedFileData {
    return \$embed_file("assets/binary.bin")
}

fn check_payload(ed &embed_file.EmbedFileData, expected []u8) {
    assert ed.len == expected.len
    // Empty files also carry an actual payload pointer; nil means the compiler
    // left a path-only development value that a freestanding runtime cannot load.
    assert ed.data() != unsafe { nil }
    s := ed.to_string()
    t := ed.to_string()
    assert s.len == expected.len
    assert s.bytes() == expected
    assert t.bytes() == expected
    assert s.str != t.str
    assert unsafe { s.str[s.len] } == 0
    assert unsafe { t.str[t.len] } == 0
    mut bytes := ed.to_bytes()
    assert bytes == expected
    for i, value in expected {
        assert unsafe { ed.data()[i] } == value
    }
    if bytes.len > 0 {
        bytes[0] ^= 0xff
        assert unsafe { ed.data()[0] } == expected[0]
        assert ed.to_string().bytes() == expected
    }
}

fn main() {
    check_payload(&text, "Vile bootstrap\n(+ 1 2)\n".bytes())
    expected := [u8(0), 1, 2, 127, 128, 254, 255, 0, 10, 13, 92, 34, 0]
    check_payload(&binary, expected)
    check_payload(&empty, []u8{})
    check_payload(&nested, [u8(0), 255, 128, 65, 0, 90])
    mut pattern := []u8{len: 96 * 1024}
    for i in 0 .. pattern.len {
        pattern[i] = u8((i * 131 + 7) % 256)
    }
    check_payload(&large, pattern)
    for _ in 0 .. 3 {
        local := local_payload()
        check_payload(&local, expected)
    }
    println("embedded payloads preserved")
}
'

fn embed_freestanding_fixture(root string) {
	os.mkdir_all(os.join_path(root, 'assets', 'subdirectory')) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), embed_freestanding_program) or { panic(err) }
	os.write_file(os.join_path(root, 'assets', 'text.txt'), 'Vile bootstrap\n(+ 1 2)\n') or { panic(err) }
	os.write_file_array(os.join_path(root, 'assets', 'binary.bin'), [u8(0), 1, 2, 127, 128, 254,
		255, 0, 10, 13, 92, 34, 0]) or { panic(err) }
	os.write_file(os.join_path(root, 'assets', 'empty.bin'), '') or { panic(err) }
	os.write_file_array(os.join_path(root, 'assets', 'subdirectory', 'data.bin'), [
		u8(0),
		255,
		128,
		65,
		0,
		90,
	]) or { panic(err) }
	mut pattern := []u8{len: 96 * 1024}
	for i in 0 .. pattern.len {
		pattern[i] = u8((i * 131 + 7) % 256)
	}
	os.write_file_array(os.join_path(root, 'assets', 'large.bin'), pattern) or { panic(err) }
}

fn run_embed_freestanding_mode(flags []string, tag string, remove_files bool) {
	root := os.join_path(os.vtmp_dir(), 'embed_freestanding_${tag}_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer { os.rmdir_all(root) or {} }
	embed_freestanding_fixture(root)
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	mut args := [@VEXE, '-no-retry-compilation', '-gc', 'none']
	args << flags
	args << ['-o', binary, os.join_path(root, 'main.v')]
	build := os.exec(args)
	assert build.exit_code == 0, '${tag}: ${build.output}'
	if remove_files {
		// Prove the bytes live in the executable, rather than silently reading
		// paths from the compiler machine at runtime.
		os.rmdir_all(os.join_path(root, 'assets')) or { panic(err) }
	}
	run := os.exec([binary])
	assert run.exit_code == 0, '${tag}: ${run.output}'
	assert run.output.trim_space() == 'embedded payloads preserved', run.output
}

fn test_hosted_embedding_preserves_binary_and_empty_payloads() {
	run_embed_freestanding_mode(['-g'], 'hosted_debug', false)
	run_embed_freestanding_mode(['-prod'], 'hosted_prod', true)
}

fn test_hosted_debug_embedding_loads_runtime_bytes_and_length() {
	root := os.join_path(os.vtmp_dir(), 'embed_hosted_reload_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'const data = \$embed_file("created_after_build.txt")
fn main() {
    text := data.to_string()
    assert text == "loaded at runtime"
    assert data.len == text.len
    assert data.to_bytes() == text.bytes()
    println("hosted reload preserved")
}
') or { panic(err) }
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	build := os.exec([@VEXE, '-no-retry-compilation', '-g', '-gc', 'none', '-o', binary, source])
	assert build.exit_code == 0, build.output
	os.write_file(os.join_path(root, 'created_after_build.txt'), 'loaded at runtime') or { panic(err) }
	run := os.exec([binary])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hosted reload preserved', run.output
}

fn test_freestanding_embedding_needs_no_runtime_files() {
	$if linux || macos {
		run_embed_freestanding_mode(['-freestanding'], 'bare_debug', true)
		run_embed_freestanding_mode(['-freestanding', '-prod'], 'bare_prod', true)
	}
}

fn test_freestanding_missing_payload_is_a_compile_error() {
	root := os.join_path(os.vtmp_dir(), 'embed_freestanding_missing_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println(\$embed_file("missing.bin").to_string()) }') or { panic(err) }
	build := os.exec([@VEXE, '-no-retry-compilation', '-freestanding', '-check', source])
	assert build.exit_code != 0, build.output
	assert build.output.contains('cannot embed file'), build.output
	assert build.output.contains('missing.bin'), build.output
	assert build.output.count('error:') == 1, build.output
	assert !build.output.contains('C compiler'), build.output
}
