#!/usr/bin/env -S ./vnew run

import os
import hash.crc64

struct TestVector {
	name string
	data []u8
}

const ecma_check_123456789 = u64(0x6c40df5f0b497347)

fn compile_c_helper(dir string) string {
	ref_c := os.join_path(dir, 'crc64_ref.c')
	ref_bin := os.join_path(dir, 'crc64_ref')

	// Compile: gcc -std=c99 crc64_ref.c -o crc64_ref
	cmd := 'gcc -std=c99 "${ref_c}" -o "${ref_bin}"'
	result := os.execute(cmd)
	if result.exit_code != 0 {
		eprintln('Failed to compile C helper:')
		eprintln(result.output)
		exit(1)
	}

	return ref_bin
}

fn run_c_checksum(ref_bin string, data []u8) !u64 {
	hex_str := data.hex()
	cmd := '${ref_bin} checksum ${hex_str}'
	result := os.execute(cmd)
	if result.exit_code != 0 {
		return error('C helper failed: ${result.output}')
	}

	result_str := result.output.trim_space()
	return u64(result_str.parse_uint(16, 64)!)
}

fn run_python_checksum(dir string, data []u8) !u64 {
	ref_py := os.join_path(dir, 'crc64_ref.py')
	hex_str := data.hex()
	cmd := 'python3 "${ref_py}" checksum ${hex_str}'
	result := os.execute(cmd)
	if result.exit_code != 0 {
		return error('Python helper failed: ${result.output}')
	}

	result_str := result.output.trim_space()
	return u64(result_str.parse_uint(16, 64)!)
}

fn test_vector(name string, data []u8, ref_bin string, dir string) ! {
	// V checksum
	v_sum := crc64.sum(data)

	// C checksum
	c_sum := run_c_checksum(ref_bin, data)!

	// Python checksum
	py_sum := run_python_checksum(dir, data)!

	// All must match
	if v_sum != c_sum || v_sum != py_sum {
		eprintln('FAIL: ${name}')
		eprintln('  V:      0x${v_sum:016x}')
		eprintln('  C:      0x${c_sum:016x}')
		eprintln('  Python: 0x${py_sum:016x}')
		return error('checksum mismatch')
	}

	if name == 'text_123456789' && v_sum != ecma_check_123456789 {
		eprintln('FAIL: ${name}')
		eprintln('  V:        0x${v_sum:016x}')
		eprintln('  Expected: 0x${ecma_check_123456789:016x}')
		return error('unexpected CRC-64-ECMA-182 check value')
	}

	println('OK: ${name} => 0x${v_sum:016x}')
}

fn main() {
	dir := os.dir(@FILE)

	println('Compiling C reference helper...')
	ref_bin := compile_c_helper(dir)
	defer { os.rm(ref_bin) or {} }

	println('Running cross-validation tests...')
	mut passed := 0
	mut failed := 0

	mut vectors := []TestVector{}
	vectors << TestVector{'empty', []u8{}}
	vectors << TestVector{'single_a', 'a'.bytes()}
	vectors << TestVector{'single_null', [u8(0)]}
	vectors << TestVector{'text_123456789', '123456789'.bytes()}
	vectors << TestVector{'text_hello_world', 'Hello, World!'.bytes()}
	vectors << TestVector{'all_zeros_16', []u8{len: 16}}
	vectors << TestVector{'all_ones_16', []u8{len: 16, init: 0xFF}}
	vectors << TestVector{'repeating_pattern', ('abc'.repeat(50)).bytes()}

	mut all_bytes := []u8{len: 256}
	for i in 0 .. 256 {
		all_bytes[i] = u8(i)
	}
	vectors << TestVector{'all_bytes', all_bytes}
	vectors << TestVector{'large_payload', ('test data '.repeat(1000)).bytes()}

	for vec in vectors {
		test_vector(vec.name, vec.data, ref_bin, dir) or {
			eprintln('  Error: ${err}')
			failed++
			continue
		}
		passed++
	}

	println('')
	println('=== Results ===')
	println('Passed: ${passed}')
	println('Failed: ${failed}')
	println('Total:  ${passed + failed}')

	if failed > 0 {
		exit(1)
	}
}
