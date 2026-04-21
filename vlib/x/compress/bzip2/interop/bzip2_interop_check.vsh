#!/usr/bin/env -S v

import os
import x.compress.bzip2

const compression_levels = [1, 6, 9]!

struct TestVector {
	name string
	data []u8
}

fn main() {
	ensure_tools() or {
		eprintln('SKIP: ${err.msg()}')
		exit(2)
	}
	vectors := make_test_vectors()
	mut total_checks := 0
	mut total_runs := 0
	tmp_root := os.join_path(os.temp_dir(), 'v_bzip2_interop_${os.getpid()}')
	os.mkdir_all(tmp_root) or { panic(err) }
	defer {
		os.rmdir_all(tmp_root) or {}
	}
	for level in compression_levels {
		for i, vec in vectors {
			total_checks += run_case(tmp_root, level, i, vec) or {
				eprintln('FAIL: ${vec.name} (level=${level}): ${err}')
				exit(1)
				0
			}
			total_runs++
			println('ok ${total_runs}/${vectors.len * compression_levels.len}: ${vec.name} (level=${level}, ${vec.data.len} bytes)')
		}
	}
	println('PASS: ${vectors.len} vectors x ${compression_levels.len} levels, ${total_checks} cross-checks')
}

fn ensure_tools() ! {
	must_succeed('bzip2 --help >/dev/null 2>&1', 'system bzip2 command is not available')!
	must_succeed("python3 -c 'import bz2' >/dev/null 2>&1",
		'python3 with bz2 module is not available')!
}

fn make_test_vectors() []TestVector {
	mut vectors := []TestVector{}
	vectors << TestVector{'empty', []u8{}}
	vectors << TestVector{'ascii_text', 'The quick brown fox jumps over the lazy dog.\n'.repeat(64).bytes()}
	vectors << TestVector{'repeated_byte', []u8{len: 10000, init: `A`}}
	vectors << TestVector{'all_bytes_x4', all_bytes_repeated(4)}
	vectors << TestVector{'lcg_64k', lcg_bytes(65536)}
	return vectors
}

fn run_case(tmp_root string, level int, case_idx int, vec TestVector) !int {
	case_dir := os.join_path(tmp_root, 'case_l${level}_${case_idx:02}_${vec.name}')
	os.mkdir_all(case_dir)!

	v_bz2 := bzip2.compress(vec.data, block_size: level)!
	cli_bz2 := cli_compress(case_dir, vec.data, level)!
	py_bz2 := py_compress(case_dir, vec.data, level)!

	mut checks := 0

	producers := {
		'v':   v_bz2
		'cli': cli_bz2
		'py':  py_bz2
	}
	for producer, compressed in producers {
		v_plain := bzip2.decompress(compressed)!
		assert_equal_bytes('v.decompress(${producer}.compress, level=${level})', vec.data, v_plain)!
		checks++

		cli_plain := cli_decompress(case_dir, producer, compressed)!
		assert_equal_bytes('cli.decompress(${producer}.compress, level=${level})', vec.data,
			cli_plain)!
		checks++

		py_plain := py_decompress(case_dir, producer, compressed)!
		assert_equal_bytes('py.decompress(${producer}.compress, level=${level})', vec.data,
			py_plain)!
		checks++
	}
	return checks
}

fn cli_compress(case_dir string, plain []u8, level int) ![]u8 {
	in_path := os.join_path(case_dir, 'plain.in')
	out_path := os.join_path(case_dir, 'cli_l${level}.bz2')
	os.write_file_array(in_path, plain)!
	must_succeed('bzip2 -${level} -c -- ${shell_quote(in_path)} > ${shell_quote(out_path)}',
		'bzip2 compression failed')!
	return os.read_bytes(out_path)!
}

fn cli_decompress(case_dir string, producer string, compressed []u8) ![]u8 {
	in_path := os.join_path(case_dir, '${producer}.for_cli.bz2')
	out_path := os.join_path(case_dir, '${producer}.from_cli.out')
	os.write_file_array(in_path, compressed)!
	must_succeed('bzip2 -d -c -- ${shell_quote(in_path)} > ${shell_quote(out_path)}',
		'bzip2 decompression failed')!
	return os.read_bytes(out_path)!
}

fn py_compress(case_dir string, plain []u8, level int) ![]u8 {
	in_path := os.join_path(case_dir, 'plain_py.in')
	out_path := os.join_path(case_dir, 'py_l${level}.bz2')
	os.write_file_array(in_path, plain)!
	py_code := 'import bz2, pathlib, sys; p=pathlib.Path(sys.argv[1]); o=pathlib.Path(sys.argv[2]); l=int(sys.argv[3]); o.write_bytes(bz2.compress(p.read_bytes(), compresslevel=l))'
	must_succeed('python3 -c ${shell_quote(py_code)} ${shell_quote(in_path)} ${shell_quote(out_path)} ${level}',
		'python bz2 compression failed')!
	return os.read_bytes(out_path)!
}

fn py_decompress(case_dir string, producer string, compressed []u8) ![]u8 {
	in_path := os.join_path(case_dir, '${producer}.for_py.bz2')
	out_path := os.join_path(case_dir, '${producer}.from_py.out')
	os.write_file_array(in_path, compressed)!
	py_code := 'import bz2, pathlib, sys; p=pathlib.Path(sys.argv[1]); o=pathlib.Path(sys.argv[2]); o.write_bytes(bz2.decompress(p.read_bytes()))'
	must_succeed('python3 -c ${shell_quote(py_code)} ${shell_quote(in_path)} ${shell_quote(out_path)}',
		'python bz2 decompression failed')!
	return os.read_bytes(out_path)!
}

fn all_bytes_repeated(times int) []u8 {
	mut out := []u8{cap: 256 * times}
	for _ in 0 .. times {
		for i in 0 .. 256 {
			out << u8(i)
		}
	}
	return out
}

fn lcg_bytes(n int) []u8 {
	mut out := []u8{len: n}
	mut x := u32(0x12345678)
	for i in 0 .. n {
		x = x * u32(1664525) + u32(1013904223)
		out[i] = u8((x >> 16) & u32(0xff))
	}
	return out
}

fn assert_equal_bytes(label string, expected []u8, got []u8) ! {
	if expected.len != got.len {
		return error('${label}: length mismatch expected=${expected.len} got=${got.len}')
	}
	for i in 0 .. expected.len {
		if expected[i] != got[i] {
			return error('${label}: byte mismatch at offset ${i}')
		}
	}
}

fn must_succeed(command string, context string) ! {
	res := os.execute(command)
	if res.exit_code != 0 {
		return error('${context}\ncommand: ${command}\nexit_code: ${res.exit_code}\n${res.output}')
	}
}

fn shell_quote(s string) string {
	return "'${s.replace("'", "'\\''")}'"
}
