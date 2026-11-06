// Regressions for the native inline-asm lowering (secondary outputs, the unsuffixed
// `div`), the -syscalls path and ioctl, and the aligned allocation of the freestanding
// runtime. Each program is compiled and run, so the checks are what the binaries do.
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
