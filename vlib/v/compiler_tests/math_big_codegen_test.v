import os
import v.cmdexec
import v.tempname

fn test_math_big_mont_even_mask_bits_receiver_codegen() {
	root := os.join_path(os.vtmp_dir(), 'v3_math_big_mont_even_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	source := os.join_path(root, 'main.v')
	os.write_file(source, "module main\n\nimport math.big\n\nfn main() {\n\tbase := big.integer_from_int(3)\n\texponent := big.integer_from_string('18446744073709551617') or { panic(err) }\n\tmodulus := big.integer_from_int(6)\n\t_ = base.big_mod_pow(exponent, modulus) or { panic(err) }\n}\n") or { panic(err) }

	c_path := os.join_path(root, 'main.c')
	compile := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-path', '@vlib|@vmodules', source,
		'-b', 'c', '-o', c_path], '', {
		'V3CACHE': os.join_path(root, 'cache')
	})
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(c_path) or { panic(err) }
	assert generated.contains('big__Integer__mont_even'), generated
	assert !generated.contains('unknown__mask_bits'), generated
	assert generated.contains('big__Integer__mask_bits'), generated
}
