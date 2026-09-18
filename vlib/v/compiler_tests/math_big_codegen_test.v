import os

const math_big_codegen_vexe = @VEXE
const math_big_codegen_tests_dir = os.dir(@FILE)
const math_big_codegen_v_dir = os.dir(math_big_codegen_tests_dir)
const math_big_codegen_vlib_dir = os.dir(math_big_codegen_v_dir)
const math_big_codegen_v_src = os.join_path(math_big_codegen_v_dir, 'v.v')

fn math_big_codegen_build_v3() string {
	cache_dir := os.join_path(os.temp_dir(), 'v3_math_big_codegen_cache_${os.getpid()}')
	if os.getenv('V3CACHE') != cache_dir {
		os.rmdir_all(cache_dir) or {}
		os.setenv('V3CACHE', cache_dir, true)
	}
	v3_bin := os.join_path(os.temp_dir(), 'v3_math_big_codegen_${os.getpid()}')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build := os.execute('${math_big_codegen_vexe} -gc none -path "${math_big_codegen_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${math_big_codegen_v_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_math_big_mont_even_mask_bits_receiver_codegen() {
	v3_bin := math_big_codegen_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_math_big_mont_even_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}

	source := os.join_path(root, 'main.v')
	os.write_file(source, "module main\n\nimport math.big\n\nfn main() {\n\tbase := big.integer_from_int(3)\n\texponent := big.integer_from_string('18446744073709551617') or { panic(err) }\n\tmodulus := big.integer_from_int(6)\n\t_ = base.big_mod_pow(exponent, modulus) or { panic(err) }\n}\n") or { panic(err) }

	c_path := os.join_path(root, 'main.c')
	compile := os.execute('${v3_bin} -nocache -path "${math_big_codegen_vlib_dir}|@vlib|@vmodules" ${source} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(c_path) or { panic(err) }
	assert generated.contains('big__Integer__mont_even'), generated
	assert !generated.contains('unknown__mask_bits'), generated
	assert generated.contains('big__Integer__mask_bits'), generated
}
