// A `-freestanding` program links no libc: vlib/builtin/linux_bare supplies its
// `_start`, its syscalls and a dlmalloc heap. The `_linux` file name keeps this
// test to Linux hosts.
import os

const vexe = @VEXE

const freestanding_program = "struct Point {
	x int
	y int
}

fn main() {
	mut nums := []int{}
	for i in 0 .. 50000 {
		nums << i
	}
	mut total := i64(0)
	for n in nums {
		total += n
	}
	mut s := ''
	for i in 0 .. 100 {
		s += i.str()
	}
	mut points := []Point{}
	for i in 0 .. 1000 {
		points << Point{i, -i}
	}
	words := 'bare metal v'.split(' ')
	mut m := map[string]int{}
	for i, w in words {
		m[w] = i
	}
	big := []u8{len: 3_000_000, init: 1}
	println([nums.len.str(), total.str(), s.len.str(), points[999].y.str(), words.join('+'),
		m['metal'].str(), big[big.len - 1].str()].join(' '))
	println(arguments()[1..].join(','))
	exit(7)
}
"

// test_freestanding_program_runs_without_libc builds the program with the
// default C compiler and with each of gcc, clang and the bundled TinyCC that can
// build for the target, then runs it.
fn test_freestanding_program_runs_without_libc() {
	// linux_bare makes syscalls only on these.
	$if !amd64 && !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_linux_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, freestanding_program)!
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
		assert run.exit_code == 7, '${name}: ${run.output}'
		assert run.output == '50000 1249975000 190 -999 bare+metal+v 1 1\nfirst,second\n', name
	}
}
