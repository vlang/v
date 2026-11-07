import os

fn test_div_64_portable_c_compiles_with_windows_clang() {
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'bits_div_64_windows_clang_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'divide.v')
	c_source := os.join_path(root, 'divide.c')
	os.write_file(source, "module arithmetic
import math.bits
@[export: 'divide_u128']
pub fn divide(hi u64, lo u64, y u64) u64 {
	q, r := bits.div_64(hi, lo, y)
	return q ^ r
}
")!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-no-parallel', '-nocache',
		'-gc', 'none', '-cross', '-cc', 'msvc', '-os', 'windows', '-arch', 'amd64', '-is_o', '-o',
		c_source, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_source)!
	start := code.index('__inline multi_return_u64_u64 bits__div_64(') or {
		panic('missing generated div_64 function')
	}
	tail := code[start..]
	end := tail.index('\n}\n') or { panic('missing end of generated div_64 function') }
	function := tail[..end + 3]
	// Compile the emitted arithmetic with Clang's real intrinsic declarations.
	// Isolating it avoids requiring a Windows SDK on the host running this test.
	check_source := os.join_path(root, 'check.c')
	os.write_file(check_source, '#include <intrin.h>
typedef unsigned long long u64;
typedef struct { u64 arg0; u64 arg1; } multi_return_u64_u64;
#define __V_amd64 1
#define v_panic(...) __builtin_trap()
multi_return_u64_u64 bits__div_64_default(u64, u64, u64);
__attribute__((used)) ${function}
')!
	// Some Clang installations (notably OpenBSD) have intrin.h but lack its
	// transitive intrin0.h. Keep the C generation check, but only try the
	// optional Windows compilation when its intrinsic headers are usable.
	intrin_probe := os.join_path(root, 'intrin_probe.c')
	os.write_file(intrin_probe, '#include <intrin.h>\n')!
	probe := os.exec([clang, '-target', 'x86_64-pc-windows-msvc', '-ffreestanding',
		'-fsyntax-only', intrin_probe])
	if probe.exit_code != 0 {
		if probe.output.contains('file not found') {
			if probe.output.contains("'intrin.h'") || probe.output.contains("'intrin0.h'") {
				eprintln('Skipping Windows intrinsic compile: ${probe.output}')
				return
			}
		}
		assert probe.exit_code == 0, probe.output
	}
	object := os.join_path(root, 'check.obj')
	checked := os.exec([clang, '-target', 'x86_64-pc-windows-msvc', '-ffreestanding',
		'-Werror=implicit-function-declaration', '-c', check_source, '-o', object])
	assert checked.exit_code == 0, checked.output
	assert os.is_file(object)
}
