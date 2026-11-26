import os
import v.tempname

fn test_c_string_literals_lower_in_pointer_and_scalar_contexts() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_string_literal_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_c_string_literal_input')
	src := bin + '.v'
	os.write_file(src, "struct C.NativePointer {
	value &u8
}

fn C.puts(&char) int
fn C.putchar(int) int
fn C.abs(int) int

fn takes_string_ptr(s &string) int {
	return s.len
}

fn takes_byte_ptr(b &u8) int {
	return int(*b)
}

fn main() {
	C.puts(c'canary')
	C.puts(c'line 1\nline 2')
	text := 'ordinary'
	text_len := takes_string_ptr(&text)
	ch := u8(65)
	ch_value := takes_byte_ptr(&ch)
	println(int_str(text_len + ch_value))
	cstring := c'local'
	local := unsafe { cstring.vstring() }
	println(local)
	native := C.NativePointer{
		value: 0
	}
	println('\${voidptr(native.value):x}')

	C.putchar(c'\\x41')
	C.putchar((c'\\x41'))
	C.putchar(c'\\101')
	C.putchar(c'\\n')
	println(int_str(C.abs(c'\\xff')))
}
")!
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('puts("canary");'), c_code
	assert c_code.contains('puts("line 1\\nline 2");'), c_code
	assert !c_code.contains('puts(&"canary");'), c_code
	assert c_code.contains('takes_string_ptr(&text)'), c_code
	assert c_code.contains('takes_byte_ptr(&ch)'), c_code
	assert c_code.contains('u8* cstring = "local";'), c_code
	assert c_code.contains('putchar(*"\\x41")'), c_code
	assert c_code.contains('putchar((*"\\x41"))'), c_code
	assert c_code.contains('putchar(*"\\101")'), c_code
	assert c_code.contains('((u8)*"\\xff")'), c_code
	assert !c_code.contains('abs(*"\\xff")'), c_code
	assert !c_code.contains('putchar("\\x41")'), c_code
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'canary\nline 1\nline 2\n73\nlocal\n0\nAAA\n255', run.output

	bad_bin := os.join_path(root, 'v3_c_string_literal_address_input')
	bad_src := bad_bin + '.v'
	os.write_file(bad_src, "fn C.puts(&char) int\n\nfn main() {\n\tC.puts(&c'bad')\n}\n") or {
		panic(err)
	}
	bad_compile := os.exec([@VEXE, bad_src, '-b', 'c', '-o', bad_bin])
	assert bad_compile.exit_code != 0, bad_compile.output
	assert bad_compile.output.contains("cannot take the address of 'bad'"), bad_compile.output
	assert !bad_compile.output.contains('C compilation failed'), bad_compile.output
}

fn test_embedded_nul_string_literal_codegen_escapes_c_source() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_string_literal_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_c_string_literal_nul_input')
	src := bin + '.v'
	os.write_file(src,
		"fn main() {\n\ttext := 'ab\\x00cd'\n\tbytes := text.bytes()\n\tassert text.len == 5\n\tassert bytes.len == 5\n\tassert bytes[0] == `a`\n\tassert bytes[1] == `b`\n\tassert bytes[2] == u8(0)\n\tassert bytes[3] == `c`\n\tassert bytes[4] == `d`\n\tprintln('ok')\n}\n") or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('ab\\000cd'), c_code
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_multi_byte_c_literal_is_rejected_in_scalar_context() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_string_literal_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bad_bin := os.join_path(root, 'v3_c_multi_byte_scalar_input')
	bad_src := bad_bin + '.v'
	os.write_file(bad_src, "fn C.putchar(int) int\n\nfn main() {\n\tC.putchar(c'AB')\n}\n") or {
		panic(err)
	}
	bad_compile := os.exec([@VEXE, bad_src, '-b', 'c', '-o', bad_bin])
	assert bad_compile.exit_code != 0, bad_compile.output
	assert bad_compile.output.contains('cannot use `&u8`'), bad_compile.output
	assert !bad_compile.output.contains('C compilation failed'), bad_compile.output
}
