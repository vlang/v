import os
import v.tempname

fn test_mixed_read_and_write_lock_modes_codegen() {
	dir := os.join_path(os.vtmp_dir(), 'v3_mixed_lock_modes_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'v3_mixed_lock_modes.v')
	os.write_file(src, 'struct St {
mut:
	n int
}

fn main() {
	shared a := St{}
	shared b := St{}
	rlock a; lock b {
		_ := a.n
		b.n = 1
	}
}
')!
	c_path := os.join_path(dir, 'v3_mixed_lock_modes.c')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, compile.output
	assert os.exists(c_path)
	c_source := os.read_file(c_path) or { panic(err) }

	entry_point := $if windows { '\nint wmain(' } $else { '\nint main(' }
	main_start := c_source.index(entry_point) or { -1 }
	assert main_start >= 0, c_source
	main_tail := c_source[main_start..]
	main_end := main_tail.index('\nvoid sync__') or { main_tail.len }
	main_c := main_tail[..main_end]
	assert main_c.contains(' + 1] == 0) '), main_c
	assert main_c.contains('] = 0;'), main_c
	assert !main_c.contains(' - 1] = 0;'), main_c
}
