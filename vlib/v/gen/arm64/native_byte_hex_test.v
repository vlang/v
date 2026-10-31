module arm64

import os

const native_test_compiler = os.join_path(os.vtmp_dir(), 'arm64_byte_hex_compiler_${os.getpid()}')

fn testsuite_end() {
	os.rm(native_test_compiler) or {}
}

// exec_native runs a compiler command. `cmd/v` leaves the ARM64 backend out by default, so
// when @VEXE reports that, the command runs with a compiler built here that includes it.
fn exec_native(args []string) os.Result {
	if !os.exists(native_test_compiler) {
		result := os.exec(args)
		if !result.output.contains('ARM64 support is not compiled into this executable') {
			return result
		}
		bootstrap := os.exec([@VEXE, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend', 'arm64',
			'-o', native_test_compiler, os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v')])
		assert bootstrap.exit_code == 0, bootstrap.output
	}
	return os.exec(args.map(if it == @VEXE { native_test_compiler } else { it }))
}

fn test_native_byte_hex_and_real_sha256_tool_cache_keys() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_byte_hex_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, r'module main
import crypto.sha256
fn C.exit(int)
fn C.alarm(u32) u32
fn main() {
    C.alarm(5)
    empty := []u8{}
    if empty.hex() != "" { C.exit(1) }
    mut bytes := []u8{len: 256}
    for i in 0 .. 256 { bytes[i] = u8(i) }
    encoded := bytes.hex()
    if encoded.len != 512 { C.exit(2) }
    digits := "0123456789abcdef"
    for i in 0 .. 256 {
        if encoded[i * 2] != digits[i >> 4] || encoded[i * 2 + 1] != digits[i & 15] { C.exit(3) }
        if bytes[i] != u8(i) { C.exit(4) }
    }
    if unsafe { encoded.str[encoded.len] } != 0 { C.exit(5) }
    view := bytes[127..130]
    if view.hex() != "7f8081" { C.exit(6) }
    if bytes[255..].hex() != "ff" { C.exit(7) }
    if sha256.hexhash("") != "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" { C.exit(8) }
    if sha256.hexhash("abc") != "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" { C.exit(9) }
    if sha256.hexhash("V") != "de5a6f78116eca62d7fc5ce159d23ae6b889b365a1739ad2cf36f925a140d0cc" { C.exit(10) }
    C.alarm(0)
}
')!
		compiled := exec_native([@VEXE, '-gc', 'none', '-nocache', '-b', 'arm64', '-o', output,
			path])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
