import os
import v.cmdexec

fn test_sizeof_local_and_parameter_shadow_module_constant() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_local_shadow_const_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	program := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	os.write_file(source, 'const buf = [u16(1), 2, 3]!
const record = 1
struct Record {
	values [4]u16
}
fn measure(buf [8]u8) {
	println(sizeof(buf))
	println(sizeof(buf[0]))
	println(sizeof(buf[0] + int(1)) == sizeof(int))
}
fn constant_sizes() {
	println(sizeof(buf))
	println(sizeof(buf[0]))
}
fn main() {
	buf := [8]u8{}
	record := Record{}
	println(sizeof(buf))
	println(sizeof(buf[0]))
	println(sizeof(record.values))
	measure(buf)
	constant_sizes()
}
')!
	// Build an ordinary program so test-only constant lowering cannot hide its local bindings.
	vexe := os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })
	build := cmdexec.run_with_timeout(vexe, ['-new-compiler', '-no-retry-compilation', '-nocache',
		'-gc', 'none', '-o', program, source], 120_000)
	assert build.exit_code == 0, build.output
	run := cmdexec.run_with_timeout(program, []string{}, 10_000)
	assert run.exit_code == 0, run.output
	assert run.output.replace('\r\n', '\n') == '8\n1\n8\n8\n1\ntrue\n6\n2\n', run.output
}
