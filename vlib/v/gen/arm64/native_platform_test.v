module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_platform_file_flags_and_thread_local_errno() {
	$if !macos || !arm64 {
		return
	}
	path := os.join_path(os.vtmp_dir(), 'arm64_platform_${os.getpid()}.v')
	output := path.all_before_last('.')
	data_file := output + '.data'
	permissions_file := output + '.permissions'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
		os.rm(data_file) or {}
		os.rm(permissions_file) or {}
	}
	os.write_file(path, 'module main
__global C.errno int
fn C.open(&char, int, ...int) int
fn C.close(int) int
fn C.write(int, voidptr, usize) isize
fn C.read(int, voidptr, usize) isize
fn C.unlink(&char) int
fn C.v3_bench_peak_rss_kb() i64
fn C.exit(int)
fn main() {
	path := "${data_file}"
	text := "native file"
	fd := C.open(path.str, C.O_WRONLY | C.O_CREAT | C.O_TRUNC, 0o600)
	if fd < 0 { C.exit(1) }
	if C.write(fd, text.str, usize(text.len)) != text.len { C.exit(2) }
	C.close(fd)
	input := C.open(path.str, C.O_RDONLY)
	mut buffer := [32]u8{}
	if C.read(input, &buffer[0], 32) != text.len { C.exit(3) }
	if buffer[0] != u8(110) || buffer[11] != 0 { C.exit(4) }
	C.close(input)
	C.unlink(path.str)
	if C.open(path.str, C.O_RDONLY) != -1 { C.exit(5) }
	if C.errno != C.ENOENT { C.exit(6) }
	C.errno = 123
	if C.errno != 123 { C.exit(7) }
	if C.v3_bench_peak_rss_kb() <= 0 { C.exit(8) }
	permissions_fd := C.open(c"${permissions_file}", C.O_WRONLY | C.O_CREAT | C.O_TRUNC, C.S_IRUSR | C.S_IWUSR)
	if permissions_fd < 0 { C.exit(9) }
	C.close(permissions_fd)
}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, result.output
	permissions := os.stat(permissions_file) or { panic(err) }
	assert permissions.mode & 0o777 == 0o600
	os.write_file(permissions_file, 'permissions') or { panic(err) }
	assert os.read_file(permissions_file) or { panic(err) } == 'permissions'
}
