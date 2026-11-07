// Regressions for the native inline-asm lowering (secondary outputs, the unsuffixed
// `div`, wide immediates), the -syscalls path, ioctl, newline reads and wait4, and the
// aligned allocation of the freestanding runtime, and the macOS arm64 freestanding build. Each program is compiled and run, so the checks are what the binaries do.
import os

const vexe = @VEXE

const native_asm_program = "import math.bits

fn main() {
	hi, lo := bits.mul_add_64(u64(0x123456789abcdef0), u64(0xfedcba9876543210), u64(0xffffffffffffffff))
	q, r := bits.div_64(0, 100, 7)
	println(hi.hex() + ' ' + lo.hex() + ' ' + q.str() + ' ' + r.str())
}
"

const syscalls_path_program = "import os

fn main() {
	os.chdir(os.args[1]) or { panic(err) }
	_ = os.fd_is_pending(0)
	println(os.real_path('..') == os.dir(os.getwd()))
}
"

const posix_memalign_program = "module main

fn C.posix_memalign(memptr &voidptr, alignment usize, size usize) int
fn C.realloc(ptr voidptr, size usize) voidptr
fn C.free(ptr voidptr)
fn C.malloc(size usize) voidptr

fn check_fill(p voidptr, size int, seed int) bool {
	mut b := &u8(p)
	for i in 0 .. size {
		unsafe {
			b[i] = u8((i + seed) & 0xff)
		}
	}
	for i in 0 .. size {
		if unsafe { b[i] } != u8((i + seed) & 0xff) {
			return false
		}
	}
	return true
}

fn main() {
	mut bad := 0
	mut p := voidptr(0)
	for align in [16, 32, 64, 256, 4096, 16384] {
		for size in [1, 100, 5000, 70000] {
			rc := C.posix_memalign(&p, usize(align), usize(size))
			if rc != 0 || u64(p) % u64(align) != 0 || !check_fill(p, size, align) {
				bad++
				continue
			}
			C.free(p)
		}
	}
	if C.posix_memalign(&p, usize(3), usize(10)) != 22 {
		bad++
	}
	mut q := voidptr(0)
	C.posix_memalign(&q, usize(64), usize(100))
	check_fill(q, 100, 7)
	r := C.realloc(q, usize(300))
	mut ok := true
	for i in 0 .. 100 {
		if unsafe { (&u8(r))[i] } != u8((i + 7) & 0xff) {
			ok = false
		}
	}
	if !ok {
		bad++
	}
	C.free(r)
	C.free(C.malloc(usize(64)))
	println('bad=' + bad.str())
}
"

const native_asm_imm_program = 'fn main() {
	initial := u64(1)
	mut result := u64(0)
	asm amd64 {
		addq rax, 256
		subq rax, -129
		xorq rax, 0x7fff0000
		; =a (result)
		; a (initial)
		; cc
	}
	println(result)
}
'

const native_asm_imm_too_wide_program = 'fn main() {
	mut result := u64(0)
	asm amd64 {
		xorq rax, 0x80000000
		; =a (result)
		; a (result)
		; cc
	}
	println(result)
}
'

const syscalls_newline_program = "import os

fn main() {
	path := os.args[1]
	os.write_file(path, 'ab\\ncd')!
	f := os.open(path)!
	mut buf := []u8{len: 16}
	n1 := f.read_bytes_with_newline(mut buf)!
	first := buf[..n1].bytestr()
	n2 := f.read_bytes_with_newline(mut buf)!
	println('\${n1} \${first.trim_space()} \${n2} \${buf[..n2].bytestr()}')
}
"

const wait4_rusage_program = "import os

fn main() {
	pid := os.fork()
	if pid == 0 {
		exit(3)
	}
	mut status := 0
	mut usage := []u8{len: 256, init: 0xaa}
	got := os.wait4(pid, &status, 0, usage.data)
	println('\${got == pid} \${(status >> 8) & 0xff} \${usage.any(it != 0xaa)}')
}
"

const fork_execve_program = "import os

fn main() {
	pid := os.fork()
	if pid == 0 {
		os.execve('/bin/sh', ['-c', 'exit 7'], []) or { exit(99) }
	}
	mut status := 0
	got := os.wait4(pid, &status, 0, unsafe { nil })
	println('reaped=\${got == pid} exit=\${(status >> 8) & 0xff}')
	pid2 := os.fork()
	if pid2 == 0 {
		os.execve('/nonexistent/x', [], []) or {
			println('child: \${err.code()}')
			exit(3)
		}
	}
	os.wait4(pid2, &status, 0, unsafe { nil })
	println('exit2=\${(status >> 8) & 0xff}')
}
"

const native_file_close_program = "import os

fn main() {
	path := os.args[1]
	os.write_file(path, 'abc')!
	println(os.read_file(path)!)
}
"

// test_native_asm_secondary_outputs_and_div_lower checks that a two-output asm block
// with three inputs stores both halves, and that `div` lowers like `divq`.
fn test_native_asm_secondary_outputs_and_div_lower() {
	$if !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_asm_regress_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, native_asm_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-b', 'x64', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output == '121fa00ad77d7423 236d88fe5618ceff 14 2\n', run.output
}

// test_syscalls_path_and_ioctl checks the `..` resolution and the fd_is_pending ioctl
// route under -syscalls.
fn test_syscalls_path_and_ioctl() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscalls_path_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'a', 'b'))!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, syscalls_path_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-syscalls', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe, os.join_path(root, 'a', 'b')])
	assert run.exit_code == 0, run.output
	assert run.output == 'true\n', run.output
}

// test_freestanding_posix_memalign_is_aligned_and_freeable checks the native runtime's
// posix_memalign: alignment, data integrity, realloc of an aligned block, and EINVAL.
fn test_freestanding_posix_memalign_is_aligned_and_freeable() {
	$if !amd64 || !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'posix_memalign_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, posix_memalign_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-freestanding', '-b', 'x64', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output == 'bad=0\n', run.output
}

// test_native_asm_wide_immediates checks that arithmetic immediates outside -128..127
// keep their value on the x64 backend, and that ones without a 32-bit encoding are
// rejected instead of being truncated.
fn test_native_asm_wide_immediates() {
	$if !amd64 || !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_asm_imm_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, native_asm_imm_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-b', 'x64', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output == '2147418498\n', run.output
	wide_source := os.join_path(root, 'wide.v')
	os.write_file(wide_source, native_asm_imm_too_wide_program)!
	wide := os.exec([vexe, '-nocache', '-b', 'x64', '-o', os.join_path(root, 'wide'), wide_source])
	assert wide.exit_code != 0, wide.output
	assert wide.output.contains('does not fit a sign-extended 32-bit immediate'), wide.output
}

// test_syscalls_read_bytes_with_newline checks that File.read_bytes_with_newline
// compiles and stops at the newline under -syscalls.
fn test_syscalls_read_bytes_with_newline() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscalls_newline_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, syscalls_newline_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-syscalls', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe, os.join_path(root, 'data.txt')])
	assert run.exit_code == 0, run.output
	assert run.output == '3 ab 2 cd\n', run.output
}

// test_wait4_fills_rusage checks that os.wait4 reaps the child and fills the
// resource-usage buffer, both hosted and under -syscalls.
fn test_wait4_fills_rusage() {
	$if !linux || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'wait4_rusage_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, wait4_rusage_program)!
	for mode in ['', '-syscalls'] {
		exe := os.join_path(root, 'main${mode}')
		mut args := [vexe, '-nocache']
		if mode != '' {
			args << mode
		}
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, '${mode}: ${build.output}'
		run := os.exec([exe])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output == 'true 3 true\n', '${mode}: ${run.output}'
	}
}

// test_fork_execve_wait4_through_bf checks os.fork, os.execve and os.wait4, which go
// through the runtime's bf_fork/bf_execve/bf_wait4, hosted, under -syscalls and in a
// freestanding x64 build, including the errno a failed execve reports.
fn test_fork_execve_wait4_through_bf() {
	$if !linux || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fork_execve_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, fork_execve_program)!
	for i, mode in [[]string{}, ['-syscalls'], ['-freestanding', '-b', 'x64']] {
		exe := os.join_path(root, 'main_${i}')
		mut args := [vexe, '-nocache']
		args << mode
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, '${mode}: ${build.output}'
		run := os.exec([exe])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output == 'reaped=true exit=7\nchild: 2\nexit2=3\n', '${mode}: ${run.output}'
	}
}

// test_native_freestanding_file_close_closes checks that the native x64 runtime's
// close syscall number is close: with fork's number in its place, every file the
// program closes spawns a copy that prints again.
fn test_native_freestanding_file_close_closes() {
	$if !linux || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_close_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, native_file_close_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-freestanding', '-b', 'x64', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe, os.join_path(root, 'data.txt')])
	assert run.exit_code == 0, run.output
	assert run.output == 'abc\n', run.output
}

const macos_arm64_freestanding_program = "import os

fn helper() int {
	return 1
}

fn main() {
	path := os.args[1]
	os.write_file(path, 'abc')!
	println(os.read_file(path)!)
	println(os.getenv('V_FREESTANDING_PROBE'))
	if os.args.len > 2 {
		helper := fn () int {
			return 2
		}
		println(helper())
	}
	println(helper())
	narrow := i32(os.args.len) - 10
	println(narrow >>> 28)
	print(os.get_raw_line())
}
"

// test_macos_arm64_freestanding_builds_and_runs checks that `-freestanding` on the
// native arm64 backend takes the macos_bare runtime: the os module's bf_* surface
// resolves, `-b arm64` selects the arm64 asm on any host, and the result is an
// arm64 Mach-O whose stdin line reader does not import libSystem's getline (its
// line would be freed by the runtime's own free). On Apple Silicon the binary also
// runs its syscalls, a block-local function value that shadows a function, and
// `>>>` on a negative i32.
fn test_macos_arm64_freestanding_builds_and_runs() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_arm64_freestanding_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, macos_arm64_freestanding_program)!
	exe := os.join_path(root, 'main')
	build := os.exec([vexe, '-nocache', '-freestanding', '-os', 'macos', '-b', 'arm64', '-o', exe,
		source])
	assert build.exit_code == 0, build.output
	header := os.read_bytes(exe)!
	assert header.len > 8
	// MH_MAGIC_64 followed by CPU_TYPE_ARM64, both little endian
	assert header[0..8] == [u8(0xcf), 0xfa, 0xed, 0xfe, 0x0c, 0x00, 0x00, 0x01]
	assert !header.bytestr().contains('_getline')
	$if macos && arm64 {
		os.setenv('V_FREESTANDING_PROBE', 'probe', true)
		data := os.join_path(root, 'data.txt')
		run := os.exec(['sh', '-c', 'printf "line\\nrest" | "$@"', 'sh', exe, data, 'shadow'])
		assert run.exit_code == 0, run.output
		// argc 3: the local `helper` gives 2 inside its block and the function 1 after
		// it, and -7 >>> 28 is 15 at 32 bits
		assert run.output == 'abc\nprobe\n2\n1\n15\nline\n', run.output
	}
}
