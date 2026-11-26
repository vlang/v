import os
import v.tempname

// test_sha3_issue_26961_compile validates that v3 can compile the sha3 benchmark
// program from https://github.com/vlang/v/issues/26961 .
fn test_sha3_issue_26961_compile() {
	root := os.join_path(os.vtmp_dir(), 'v3_sha3_issue_26961_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'v3_sha3_issue_26961.v')
	bin := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
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

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert os.exists(bin), compile.output
}
