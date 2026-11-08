module fastc

import os
import v.cmdexec

fn test_large_embedded_files_link_from_a_separate_object() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_payload_object_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'other')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	payload := []u8{len: fastc_payload_unit_min_bytes + 3, init: u8(index % 251)}
	mut sum := u64(0)
	for b in payload {
		sum += b
	}
	os.write_file_array(os.join_path(root, 'payload.bin'), payload) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r"module main

import other

fn main() {
	bytes := $embed_file('payload.bin').to_bytes()
	mut sum := u64(0)
	for b in bytes {
		sum += b
	}
	println('${bytes.len + other.offset()} ${sum}')
}
") or {
		panic(err)
	}
	// The first build compiles without caches, the second publishes the payload object
	// and the third reuses it. A different offset keeps the executable cache cold.
	for run, flags in [['-nocache'], []string{}, []string{}] {
		os.write_file(os.join_path(root, 'other', 'other.v'), 'module other\n\npub fn offset() int {\n\treturn ${run}\n}\n') or {
			panic(err)
		}
		program := os.join_path(root, 'program_${run}')
		mut args := ['-b', 'fastc']
		args << flags
		args << ['-o', program, root]
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		result := cmdexec.run(program, [])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '${payload.len + run} ${sum}'
	}
}
