import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_c_string_literal_pointer_codegen_preserves_regular_addresses() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_string_literal_pointer_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c_string_literal_pointer_input.v')
	os.write_file(src,
		"fn C.puts(&char) int\n\nfn takes_string_ptr(s &string) int {\n\treturn s.len\n}\n\nfn takes_byte_ptr(b &u8) int {\n\treturn int(*b)\n}\n\nfn main() {\n\tC.puts(c'canary')\n\ttext := 'ordinary'\n\ttext_len := takes_string_ptr(&text)\n\tch := u8(65)\n\tch_value := takes_byte_ptr(&ch)\n\tprintln(int_str(text_len + ch_value))\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_string_literal_pointer_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('puts("canary");'), c_code
	assert !c_code.contains('puts(&"canary");'), c_code
	assert c_code.contains('takes_string_ptr(&text)'), c_code
	assert c_code.contains('takes_byte_ptr(&ch)'), c_code
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'canary\n73'

	bad_src := os.join_path(os.temp_dir(), 'v3_c_string_literal_address_input.v')
	os.write_file(bad_src, "fn C.puts(&char) int\n\nfn main() {\n\tC.puts(&c'bad')\n}\n") or {
		panic(err)
	}
	bad_bin := os.join_path(os.temp_dir(), 'v3_c_string_literal_address_input')
	bad_compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert bad_compile.exit_code != 0, bad_compile.output
	assert bad_compile.output.contains('cannot use `&&u8`'), bad_compile.output
	assert !bad_compile.output.contains('C compilation failed'), bad_compile.output
}

fn test_embedded_nul_string_literal_codegen_escapes_c_source() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_string_literal_nul_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c_string_literal_nul_input.v')
	os.write_file(src,
		"fn main() {\n\ttext := 'ab\\x00cd'\n\tbytes := text.bytes()\n\tassert text.len == 5\n\tassert bytes.len == 5\n\tassert bytes[0] == `a`\n\tassert bytes[1] == `b`\n\tassert bytes[2] == u8(0)\n\tassert bytes[3] == `c`\n\tassert bytes[4] == `d`\n\tprintln('ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_string_literal_nul_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('ab\\000cd'), c_code
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_escape_literals_are_scalar_bytes_in_scalar_contexts() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_escape_scalar_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c_escape_scalar_input.v')
	os.write_file(src,
		"fn C.putchar(int) int\nfn C.abs(int) int\n\nfn main() {\n\tC.putchar(c'\\x41')\n\tC.putchar((c'\\x41'))\n\tC.putchar(c'\\101')\n\tC.putchar(c'\\n')\n\tprintln(int_str(C.abs(c'\\xff')))\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_escape_scalar_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('putchar(*"\\x41")'), c_code
	assert c_code.contains('putchar((*"\\x41"))'), c_code
	assert c_code.contains('putchar(*"\\101")'), c_code
	assert c_code.contains('((u8)*"\\xff")'), c_code
	assert !c_code.contains('abs(*"\\xff")'), c_code
	assert !c_code.contains('putchar("\\x41")'), c_code
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'AAA\n255'

	bad_src := os.join_path(os.temp_dir(), 'v3_c_multi_byte_scalar_input.v')
	os.write_file(bad_src, "fn C.putchar(int) int\n\nfn main() {\n\tC.putchar(c'AB')\n}\n") or {
		panic(err)
	}
	bad_bin := os.join_path(os.temp_dir(), 'v3_c_multi_byte_scalar_input')
	bad_compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert bad_compile.exit_code != 0, bad_compile.output
	assert bad_compile.output.contains('cannot use `&u8`'), bad_compile.output
	assert !bad_compile.output.contains('C compilation failed'), bad_compile.output
}
