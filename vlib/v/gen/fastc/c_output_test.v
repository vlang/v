module fastc

import os
import v.cmdexec

fn test_fastc_c_piece_output_flushes_partial_buffers_and_preserves_binary_bytes() {
	root := os.join_path(os.vtmp_dir(), 'fastc_c_pieces_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	output := os.join_path(root, 'pieces.c')
	pieces := ['', 'x'.repeat(1024 * 1024 + 3), '\x00\xff', 'tail']
	write_c_pieces(output, pieces) or { panic(err) }
	assert os.read_file(output) or { panic(err) } == pieces.join('')
}

fn test_fastc_c_piece_output_reports_device_write_errors() {
	$if linux {
		pieces := []string{len: 64, init: 'x'.repeat(1024)}
		write_c_pieces('/dev/full', pieces) or { return }
		assert false, 'the final buffered write must report the full device'
	}
}

fn test_fastc_c_piece_output_reports_final_flush_errors() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_c_flush_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, r'module main
import os
import v.gen.fastc
#include <sys/resource.h>
struct C.rlimit {
	rlim_cur uint
	rlim_max uint
}
fn C.setrlimit(int, &C.rlimit) int
fn main() {
	// Limit only this child, after compilation and before the buffered output.
	os.signal_opt(.xfsz, os.SignalHandler(C.SIG_IGN)) or { panic(err) }
	limit := C.rlimit{}
	assert C.setrlimit(C.RLIMIT_FSIZE, &limit) == 0
	if os.args[1] == "publish" {
		os.execvp(os.args[2], os.args[3..]) or { panic(err) }
		return
	}
	pieces := []string{len: 64, init: "x".repeat(1024)}
	fastc.write_c_pieces(os.args[1], pieces) or {
		assert err.code() == C.EFBIG, err.msg()
		println("write error")
		return
	}
	eprintln("buffered output incorrectly succeeded")
	exit(1)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-o', program, source])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [os.join_path(root, 'pieces.c')])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'write error', run.output
	output := os.join_path(root, 'published.c')
	input := os.join_path(root, 'input.v')
	os.write_file(input, 'fn main() { println(42) }') or { panic(err) }
	// Warm the optional backend compiler before imposing the child's file size limit.
	warm := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', output, input])
	assert warm.exit_code == 0, warm.output
	os.write_file(output, 'previous successful output') or { panic(err) }
	publish := cmdexec.run(program, ['publish', @VEXE, '-b', 'fastc', '-nocache', '-o', output,
		input])
	assert publish.exit_code != 0, publish.output
	assert publish.output.contains('error writing fastc output'), publish.output
	assert os.read_file(output) or { panic(err) } == 'previous successful output'
	assert (os.ls(root) or { panic(err) }).all(!it.starts_with('published.c.stage.'))
}

fn test_fastc_c_output_does_not_compile_or_link_native_inputs() {
	root := os.join_path(os.vtmp_dir(), 'fastc_c_output_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, 'module main\n#include <fastc_c_output_missing_header.h>\nfn main() { println(42) }\n') or { panic(err) }
	for compiler_flags in [[]string{}, ['-cc', 'clang']] {
		output := os.join_path(root, 'program.c')
		mut args := ['-b', 'fastc', '-showcc']
		args << compiler_flags
		args << ['-o', output, source]
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		generated := os.read_file(output) or { panic(err) }
		assert generated.contains('#include <fastc_c_output_missing_header.h>')
		assert !build.output.contains(' -c '), build.output
		assert !os.exists(output.all_before_last('.c'))
		args[args.len - 2] = '-'
		stdout := cmdexec.run(@VEXE, args)
		assert stdout.exit_code == 0, stdout.output
		assert stdout.output == generated
	}
}

fn test_fastc_nested_table_initializers_preserve_enum_and_alias_map_keys() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_table_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	mut entries := []string{}
	for index in 0 .. 256 {
		entries << 'Entry{short: {Key.left: ${index}}, long: {Key_Long.right: ${index + 1}}, alias: {Key_Alias(Key.left): ${index + 2}}, numbers: [${index}, ${index + 1}]!}'
	}
	source := 'module main\nenum Key { left }\nenum Key_Long { right }\ntype Key_Alias = Key\nstruct Entry { short map[Key]int long map[Key_Long]int alias map[Key_Alias]int numbers [2]int }\nconst entries = [${entries.join(',\n')}]\nfn main() { for index, entry in entries { assert entry.short[.left] == index; assert entry.long[.right] == index + 1; assert entry.alias[Key_Alias(Key.left)] == index + 2; assert entry.numbers == [index, index + 1]! } }\n'
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'clang', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
