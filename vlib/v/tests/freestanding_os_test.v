// A build with `-freestanding` links no libc: vlib/builtin/linux_bare supplies
// its `_start` and syscalls, and vlib/os supplies its own syscall wrappers
// (`$if freestanding && linux` in vlib/os/freestanding.c.v), so the os file,
// env and cwd paths here run without glibc. The `_linux` file name keeps this
// test to Linux hosts.
import os

const vexe = @VEXE

const freestanding_os_program = "import os

fn main() {
	root := '/tmp/freestanding_os_' + os.getpid().str()
	os.rmdir_all(root) or { }
	os.mkdir_all(os.join_path_single(root, 'sub'))!
	os.write_file(os.join_path_single(root, 'a.txt'), 'hello freestanding')!
	cont := os.read_file(os.join_path_single(root, 'a.txt'))!
	println('read: ' + cont)
	if !os.exists(os.join_path_single(root, 'a.txt')) || !os.is_file(os.join_path_single(root, 'a.txt')) {
		println('stat: bad')
		return
	}
	if os.is_dir(os.join_path_single(root, 'a.txt')) || !os.is_dir(root) {
		println('stat: bad')
		return
	}
	entries_count := os.read_bytes(os.join_path_single(root, 'a.txt'))!.len
	println('bytes: ' + entries_count.str())
	mut f := os.open_file(os.join_path_single(root, 'b.bin'), 'w+')!
	f.write_string('xyz')!
	telly := f.tell()!
	f.seek(0, .start)!
	mut ibuf := []u8{len: 3}
	nb := f.read(mut ibuf)!
	f.close()
	println('file: ' + telly.str() + ' ' + nb.str() + ' ' + ibuf[0].str() + ' ' + ibuf[0].str() + ibuf[1].str() + ibuf[2].str())
	os.rename(os.join_path_single(root, 'a.txt'), os.join_path_single(root, 'c.txt'))!
	os.truncate(os.join_path_single(root, 'c.txt'), 5)!
	println('trunc: ' + os.read_file(os.join_path_single(root, 'c.txt'))!)
	entries := os.ls(root)!
	println('ls: ' + entries.contains('sub').str() + ' ' + entries.contains('c.txt').str()
		+ ' ' + entries.contains('a.txt').str())
	println('env: ' + (os.getenv('PATH').len > 1).str() + ' ' + (os.environ().len > 0).str())
	println('exe: ' + (os.executable().len > 0 && os.executable()[0..1] == '/').str())
	println('wd: ' + (os.getwd().len > 0).str())
	os.flush()
	os.rm(os.join_path_single(root, 'c.txt'))!
	os.rmdir_all(root)!
	println('clean: ' + (!os.exists(root)).str())
	println(arguments()[1..].join(','))
}
"

// test_freestanding_os_program_runs_without_libc builds a program that uses
// the freestanding os surface, then runs it and compares its output.
fn test_freestanding_os_program_runs_without_libc() {
	// linux_bare makes syscalls only on these.
	$if !amd64 && !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_os_linux_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, freestanding_os_program)!
	mut compilers := ['']
	for cc in ['gcc', 'clang'] {
		if _ := os.find_abs_path_of_executable(cc) {
			compilers << cc
		}
	}
	// TinyCC has inline assembly only for x86.
	$if amd64 {
		if os.is_file(os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')) {
			compilers << 'tcc'
		}
	}
	for cc in compilers {
		name := if cc == '' { 'default' } else { cc }
		exe := os.join_path(root, 'main_${name}')
		mut args := [vexe, '-nocache', '-freestanding']
		if cc != '' {
			args << ['-cc', cc]
		}
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, '${name}: ${build.output}'
		// A dynamic executable would name its loader, which comes with libc.
		assert !os.read_file(exe)!.contains('ld-linux'), name
		run := os.exec([exe, 'first', 'second'])
		assert run.exit_code == 0, '${name}: ${run.output}'
		expected := 'read: hello freestanding\nbytes: 18\nfile: 3 3 120 120121122\ntrunc: hello\nls: true true false\nenv: true true\nexe: true\nwd: true\nclean: true\nfirst,second\n'
		assert run.output == expected, '${name}: ${run.output}'
	}
}