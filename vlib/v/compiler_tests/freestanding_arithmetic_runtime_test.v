import os

const freestanding_arithmetic_program = '
fn unsigned_divide(a u128, b u128) (u128, u128) {
    return a / b, a % b
}
fn signed_divide(a i128, b i128) (i128, i128) {
    return a / b, a % b
}
fn main() {
    value := u128(1) << 100
    expected := (u128(0x555555555) << 64) | u128(0x5555555555555555)
    quotient, remainder := unsigned_divide(value, u128(3))
    if quotient != expected || remainder != u128(1) { exit(1) }
    signed_quotient, signed_remainder := signed_divide(-i128(value), i128(3))
    if signed_quotient != -i128(expected) || signed_remainder != i128(-1) { exit(2) }
    println("wide arithmetic ok")
}
'

fn test_freestanding_links_native_compiler_arithmetic_runtime() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_arithmetic_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'arithmetic.v')
	os.write_file(source, freestanding_arithmetic_program)!
	mut available := 0
	for compiler in ['clang', 'gcc'] {
		if _ := os.find_abs_path_of_executable(compiler) {} else { continue }
		available++
		output := os.join_path(root, compiler)
		built := os.exec([@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', compiler, '-freestanding',
			'-o', output, source])
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output == 'wide arithmetic ok\n', ran.output
		if compiler == 'clang' {
			// The explicit -cflags choice must also reach the archive query.
			query := os.exec([compiler, '--rtlib=compiler-rt', '-print-libgcc-file-name'])
			archive := query.output.trim_space()
			if query.exit_code == 0 && os.is_file(archive) {
				// Some distributions compile signed/remainder helpers with libc's
				// stack-protector ABI. Keep this policy test to the standalone
				// unsigned division helper; the default archive above covers all four.
				selection_source := os.join_path(root, 'selection.v')
				os.write_file(selection_source, '
fn divide(a u128, b u128) u128 { return a / b }
fn main() {
 value := u128(1) << 100
 expected := (u128(0x555555555) << 64) | u128(0x5555555555555555)
 if divide(value, u128(3)) != expected { exit(3) }
 println("wide arithmetic ok")
}')!
				for selection_flag in ['-cflags', '-ldflags'] {
					selected := os.exec([@VEXE, '-no-retry-compilation', '-showcc', '-b', 'c',
						'-cc', compiler, selection_flag, '--rtlib=compiler-rt', '-freestanding',
						'-o', output, selection_source])
					assert selected.exit_code == 0, selected.output
					assert selected.output.contains(os.real_path(archive)), selected.output
					selected_run := os.exec([output])
					assert selected_run.exit_code == 0, selected_run.output
					assert selected_run.output == 'wide arithmetic ok\n', selected_run.output
				}
			}
		}
	}
	assert available > 0, 'this regression requires Clang or GCC'
}
