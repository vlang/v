import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_c_identifier_hygiene_for_escaped_names_unix_and_main_const() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_identifier_hygiene_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c_identifier_hygiene_input_${pid}.v')
	os.write_file(src, 'const block_size = 4

struct Token {
mut:
	@type int
	unix i64
	block_size int
}

fn bump(unix i64) i64 {
	return unix + 1
}

fn rewrite(mut item Token, unix i64) {
	item.@type = item.@type + 1
	item.unix = unix + item.unix
}

fn main() {
	mut @type := 1
	mut unix := i64(2)
	mut item := Token{
		@type: @type
		unix: unix
		block_size: block_size
	}
	item.@type += 3
	item.unix = bump(item.unix)
	rewrite(mut item, 5)
	item.block_size = block_size
	mut block_size := 1
	println(int_str(item.@type + int(item.unix) + item.block_size + block_size))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_identifier_hygiene_input_${pid}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('@type'), c_code
	assert !c_code.contains('i64 unix'), c_code
	assert !c_code.contains('.unix'), c_code
	assert !c_code.contains('->unix'), c_code
	assert !c_code.contains('#define block_size'), c_code
	assert c_code.contains('i64 v_unix'), c_code
	assert c_code.contains('.v_unix'), c_code
	assert c_code.contains('->v_unix'), c_code
	assert c_code.contains('#define main__block_size'), c_code
	assert c_code.contains('.block_size = main__block_size'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '18'

	csym_src := os.join_path(os.temp_dir(), 'v3_c_identifier_hygiene_csym_${pid}.v')
	os.write_file(csym_src, 'fn C.unix(i64) i64

fn main() {
	println(int_str(int(C.unix(10))))
}
') or {
		panic(err)
	}
	csym_c := os.join_path(os.temp_dir(), 'v3_c_identifier_hygiene_csym_${pid}.c')
	os.rm(csym_c) or {}
	csym_compile := os.execute('${v3_bin} ${csym_src} -b c -o ${csym_c}')
	assert csym_compile.exit_code == 0, csym_compile.output
	csym_code := os.read_file(csym_c) or { panic(err) }
	assert csym_code.contains('unix(10)'), csym_code
	assert !csym_code.contains('v_unix(10)'), csym_code
	assert !csym_code.contains('C.unix'), csym_code
}
