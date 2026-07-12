import os

const wchar_codegen_vexe = @VEXE
const wchar_codegen_tests_dir = os.dir(@FILE)
const wchar_codegen_v3_dir = os.dir(wchar_codegen_tests_dir)
const wchar_codegen_vlib_dir = os.dir(wchar_codegen_v3_dir)
const wchar_codegen_v3_src = os.join_path(wchar_codegen_v3_dir, 'v3.v')

fn test_headerless_wchar_prefers_compiler_type() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_headerless_wchar_test_${pid}')
	build :=
		os.execute('${wchar_codegen_vexe} -gc none -path "${wchar_codegen_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${wchar_codegen_v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_headerless_wchar_${pid}.v')
	c_out := os.join_path(os.temp_dir(), 'v3_headerless_wchar_${pid}.c')
	os.write_file(src, "fn main() {\n\tprintln('ok')\n}\n") or { panic(err) }
	os.rm(c_out) or {}
	gen_c := os.execute('${v3_bin} ${src} -b c -o ${c_out}')
	assert gen_c.exit_code == 0, gen_c.output

	c_code := os.read_file(c_out) or { panic(err) }
	wchar_block := '#ifdef __WCHAR_TYPE__
typedef __WCHAR_TYPE__ wchar_t;
#elif defined(_WIN32)
typedef unsigned short wchar_t;
#else
typedef unsigned int wchar_t;'
	assert c_code.contains(wchar_block), c_code
}
