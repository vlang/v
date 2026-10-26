#!/usr/bin/env -S v

import compress.zlib

struct TestVector {
	name string
	data []u8
}

fn main() {
	tmp_root := join_path(temp_dir(), 'v_zlib_interop_${getpid()}')
	mkdir_all(tmp_root) or { panic(err) }
	defer {
		rmdir_all(tmp_root) or {}
	}

	c_bin := compile_c_helper(tmp_root) or {
		eprintln('SKIP: ${err.msg()}')
		exit(2)
	}
	py_script := join_path(@DIR, 'zlib_ref.py')
	ensure_python(py_script) or {
		eprintln('SKIP: ${err.msg()}')
		exit(2)
	}

	vectors := make_test_vectors()
	mut total_checks := 0
	for i, vec in vectors {
		total_checks += run_case(tmp_root, c_bin, py_script, i, vec) or {
			eprintln('FAIL: ${vec.name}: ${err.msg()}')
			exit(1)
			0
		}
		println('ok ${i + 1}/${vectors.len}: ${vec.name} (${vec.data.len} bytes)')
	}
	println('PASS: ${vectors.len} vectors, ${total_checks} cross-checks')
}

fn compile_c_helper(tmp_root string) !string {
	cc := choose_cc()
	if cc == '' {
		return error('no C compiler found (tried cc, gcc, clang)')
	}
	src := join_path(@DIR, 'zlib_ref.c')
	bin := join_path(tmp_root, 'zlib_interop_ref')
	must_succeed('${cc} -O2 ${shell_quote(src)} -lz -o ${shell_quote(bin)}',
		'C zlib helper build failed')!
	return bin
}

fn choose_cc() string {
	for cc in ['cc', 'gcc', 'clang'] {
		if execute('${cc} --version >/dev/null 2>&1').exit_code == 0 {
			return cc
		}
	}
	return ''
}

fn ensure_python(py_script string) ! {
	must_succeed("python3 -c 'import zlib' >/dev/null 2>&1",
		'python3 with zlib module is not available')!
	if !exists(py_script) {
		return error('missing Python helper: ${py_script}')
	}
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

fn run_case(tmp_root string, c_bin string, py_script string, case_idx int, vec TestVector) !int {
	case_dir := join_path(tmp_root, 'case_${case_idx:02}_${vec.name}')
	mkdir_all(case_dir)!

	v_z := zlib.compress(vec.data)!
	c_z := c_compress(case_dir, c_bin, vec.data)!
	py_z := py_compress(case_dir, py_script, vec.data)!

	mut checks := 0
	producers := {
		'v':  v_z
		'c':  c_z
		'py': py_z
	}
	for producer, compressed in producers {
		v_plain := zlib.decompress(compressed)!
		assert_equal_bytes('v.decompress(${producer}.compress)', vec.data, v_plain)!
		checks++

		c_plain := c_decompress(case_dir, c_bin, producer, compressed)!
		assert_equal_bytes('c.decompress(${producer}.compress)', vec.data, c_plain)!
		checks++

		py_plain := py_decompress(case_dir, py_script, producer, compressed)!
		assert_equal_bytes('python.decompress(${producer}.compress)', vec.data, py_plain)!
		checks++
	}
	return checks
}

fn c_compress(case_dir string, c_bin string, plain []u8) ![]u8 {
	in_path := join_path(case_dir, 'plain.in')
	out_path := join_path(case_dir, 'c.zlib')
	write_file_array(in_path, plain)!
	must_succeed('${shell_quote(c_bin)} compress ${shell_quote(in_path)} ${shell_quote(out_path)}',
		'C zlib compression failed')!
	return read_bytes(out_path)!
}

fn c_decompress(case_dir string, c_bin string, producer string, compressed []u8) ![]u8 {
	in_path := join_path(case_dir, '${producer}.for_c.zlib')
	out_path := join_path(case_dir, '${producer}.from_c.out')
	write_file_array(in_path, compressed)!
	must_succeed('${shell_quote(c_bin)} decompress ${shell_quote(in_path)} ${shell_quote(out_path)}',
		'C zlib decompression failed')!
	return read_bytes(out_path)!
}

fn py_compress(case_dir string, py_script string, plain []u8) ![]u8 {
	in_path := join_path(case_dir, 'plain_py.in')
	out_path := join_path(case_dir, 'py.zlib')
	write_file_array(in_path, plain)!
	must_succeed('python3 ${shell_quote(py_script)} compress ${shell_quote(in_path)} ${shell_quote(out_path)}',
		'Python zlib compression failed')!
	return read_bytes(out_path)!
}

fn py_decompress(case_dir string, py_script string, producer string, compressed []u8) ![]u8 {
	in_path := join_path(case_dir, '${producer}.for_py.zlib')
	out_path := join_path(case_dir, '${producer}.from_py.out')
	write_file_array(in_path, compressed)!
	must_succeed('python3 ${shell_quote(py_script)} decompress ${shell_quote(in_path)} ${shell_quote(out_path)}',
		'Python zlib decompression failed')!
	return read_bytes(out_path)!
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
	res := execute(command)
	if res.exit_code != 0 {
		return error('${context}\ncommand: ${command}\nexit_code: ${res.exit_code}\n${res.output}')
	}
}

fn shell_quote(s string) string {
	return "'${s.replace("'", "'\\''")}'"
}
