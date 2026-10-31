import os

// V declares `C.timespec.tv_sec` as i64, but on 32-bit glibc it is a 32-bit `time_t`.
// sync_mono_now has to widen the seconds before scaling them to nanoseconds; the
// wrapped product made `select` start at a negative channel index on armv7.
fn test_mono_now_widens_the_seconds_on_32_bit_targets() {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'sync_mono_now_32bit_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	c_file := os.join_path(root, 'main.c')
	os.write_file(source, 'fn main() {\n\tch := chan int{cap: 1}\n\tch <- 1\n\tselect {\n\t\tx := <-ch {\n\t\t\tprintln(x)\n\t\t}\n\t}\n}\n')!
	generated := os.exec([@VEXE, '-nocache', '-gc', 'none', '-os', 'linux', '-arch', 'arm32', '-o',
		c_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	start := code.index('i64 sync__sync_mono_now(void) {') or {
		assert false, 'sync_mono_now is not generated'
		return
	}
	end := code.index_after('\n}', start) or { code.len }
	body := code[start..end]
	assert body.contains('(i64)(ts.tv_sec)'), body
}
