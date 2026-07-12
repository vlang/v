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
	typeof int
}

enum Kind {
	@asm
	end
}

fn bump(unix i64) i64 {
	return unix + 1
}

fn access(x int) int {
	return x + 11
}

fn read() int {
	return 13
}

fn close() int {
	return 14
}

fn fabs(x int) int {
	return x + 15
}

fn rewrite(mut item Token, unix i64) {
	item.@type = item.@type + 1
	item.unix = unix + item.unix
}

fn main() {
	mut @type := 1
	mut @true := 6
	mut @false := 7
	mut stdin := 8
	mut stderr := 9
	mut stdout := 10
	mut unix := i64(2)
	mut item := Token{
		@type: @type
		unix: unix
		block_size: block_size
		typeof: 12
	}
	item.@type += 3
	item.unix = bump(item.unix)
	rewrite(mut item, 5)
	item.block_size = block_size
	mut block_size := 1
	println(int_str(access(item.@type + @true + @false + stdin + stderr + stdout + int(item.unix) + item.block_size + item.typeof + int(Kind.@asm) + block_size) + read() + close() + fabs(1)))
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
	assert c_code.contains('int _v_true'), c_code
	assert c_code.contains('int _v_false'), c_code
	assert c_code.contains('int v_stdin'), c_code
	assert c_code.contains('int v_stderr'), c_code
	assert c_code.contains('int v_stdout'), c_code
	assert c_code.contains('int v_access(int x)'), c_code
	assert c_code.contains('int v_read(void)'), c_code
	assert c_code.contains('int v_close(void)'), c_code
	assert c_code.contains('int v_fabs(int x)'), c_code
	assert c_code.contains('int v_typeof'), c_code
	assert c_code.contains('Kind___v_asm'), c_code
	assert c_code.contains('.v_unix'), c_code
	assert c_code.contains('->v_unix'), c_code
	assert c_code.contains('#define main__block_size'), c_code
	assert c_code.contains('.block_size = main__block_size'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '124'

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
