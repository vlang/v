import os

const sha3_issue_vexe = @VEXE
const sha3_issue_tests_dir = os.dir(@FILE)
const sha3_issue_v3_dir = os.dir(sha3_issue_tests_dir)
const sha3_issue_vlib_dir = os.dir(sha3_issue_v3_dir)
const sha3_issue_v3_src = os.join_path(sha3_issue_v3_dir, 'v3.v')

// test_sha3_issue_26961_compile validates that v3 can compile the sha3 benchmark
// program from https://github.com/vlang/v/issues/26961 .
fn test_sha3_issue_26961_compile() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_sha3_issue_26961_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${sha3_issue_vexe} -gc none -path "${sha3_issue_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${sha3_issue_v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_sha3_issue_26961_${pid}.v')
	bin := os.join_path(os.temp_dir(), 'v3_sha3_issue_26961_bin_${pid}')
	os.write_file(src, 'import crypto.sha3
import time

fn main() {
	a := []u8{len: 10_000_000}
	t1 := time.now()
	_ := sha3.sum512(a)
	println(time.since(t1))
}
') or {
		panic(err)
	}
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}

	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert os.exists(bin), compile.output
}
