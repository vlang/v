import os

const mixed_lock_vexe = @VEXE
const mixed_lock_tests_dir = os.dir(@FILE)
const mixed_lock_v3_dir = os.dir(mixed_lock_tests_dir)
const mixed_lock_vlib_dir = os.dir(mixed_lock_v3_dir)
const mixed_lock_v3_src = os.join_path(mixed_lock_v3_dir, 'v3.v')

fn mixed_lock_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_mixed_lock_codegen_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${mixed_lock_vexe} -gc none -path "${mixed_lock_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${mixed_lock_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn mixed_lock_gen_c(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output
	assert os.exists(c_path)
	return os.read_file(c_path) or { panic(err) }
}

fn test_duplicate_mixed_lock_upgrades_first_entry_to_write_mode() {
	v3_bin := mixed_lock_build_v3()
	c_source := mixed_lock_gen_c(v3_bin, 'duplicate_mixed_lock_upgrade', 'struct St {
mut:
	n int
}

fn main() {
	shared a := St{}
	rlock a, a; lock a {
		a.n = 1
	}
}
')
	main_start := c_source.index('\nint main(') or { -1 }
	assert main_start >= 0, c_source
	main_tail := c_source[main_start..]
	main_end := main_tail.index('\nvoid sync__') or { main_tail.len }
	main_c := main_tail[..main_end]
	assert main_c.contains(' + 1] == 0) '), main_c
	assert main_c.contains('] = 0;'), main_c
	assert !main_c.contains(' - 1] = 0;'), main_c
}
