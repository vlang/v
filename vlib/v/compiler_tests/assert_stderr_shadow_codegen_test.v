import os
import v.tempname

// test_assert_with_stderr_local_codegen validates assert emission when a local
// variable would shadow platform stderr macros in generated C.
fn test_assert_with_stderr_local_codegen() {
	dir := os.join_path(os.vtmp_dir(), 'v3_assert_stderr_shadow_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'main.v')
	bin := os.join_path(dir, 'app')
	os.write_file(src, "fn main() {\n\tstderr := ''\n\tassert stderr == ''\n}\n")!
	result := os.exec([@VEXE, src, '-o', bin])
	assert result.exit_code == 0, result.output
}
