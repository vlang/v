import os

// Keep the memo's initialization compact: expanding all 2048 CallInfo defaults
// into one C function makes an optimized parallel self-build spend minutes there.
fn test_body_resolve_memo_initializer_stays_compact() {
	root := os.join_path(os.vtmp_dir(), 'body_resolve_memo_codegen_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import v.flat
import v.types

fn main() {
	ast := flat.FlatAst.new()
	checker := types.TypeChecker.new(&ast)
	checker.arm_body_resolve_memo(0, 0)
}
')!
	c_path := os.join_path(root, 'main.c')
	build := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', c_path, source])
	assert build.exit_code == 0, build.output
	generated := os.read_file(c_path)!
	mut body := ''
	for line in generated.split_into_lines() {
		if line.starts_with('void types__TypeChecker__arm_body_resolve_memo(')
			&& line.ends_with(' {') {
			body = generated.all_after(line).all_before('\n}')
			break
		}
	}
	assert body.len > 0, 'missing arm_body_resolve_memo definition'
	assert body.len < 16 * 1024, 'memo initializer generated ${body.len} bytes of C'
}
