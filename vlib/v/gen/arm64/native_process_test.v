module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_process_exec_preserves_split_and_quoted_argv() ! {
	$if !macos || !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arm64_process_argv_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	child_source := os.join_path(root, 'child.v')
	child := os.join_path(root, 'child with spaces')
	parent_source := os.join_path(root, 'parent.v')
	parent := os.join_path(root, 'parent')
	test_compiler := os.join_path(root, 'compiler')
	os.write_file(child_source, r'module main
import os
fn main() {
    println(os.args.len)
    for index, argument in os.args {
        println("${index} ${argument.len} ${argument.bytes().hex()}")
    }
    if os.args.len == 2 && os.args[1] == "--exit7" { exit(7) }
}
')!
	os.write_file(parent_source, 'module main\nimport os\nfn main() {\n' +
		'arguments := ["${child}", "", "one", "two words", "back\\\\slash", "double\\\"quote", "single\'quote"]\n' + r'
    mut quoted := []string{cap: arguments.len}
    for argument in arguments { quoted << os.quoted_path(argument) }
    split := os.split_args(quoted.join(" ")) or { panic(err) }
    assert split == arguments
    mut expected := "${arguments.len}\n"
    for index, argument in arguments {
        expected += "${index} ${argument.len} ${argument.bytes().hex()}\n"
    }
    direct := os.exec(arguments)
    assert direct.exit_code == 0, direct.output
    assert direct.output == expected
    roundtrip := os.exec(split)
    assert roundtrip.exit_code == 0, roundtrip.output
    assert roundtrip.output == expected
    unsuccessful := os.exec([arguments[0], "--exit7"])
    assert unsuccessful.exit_code == 7, unsuccessful.output
    assert unsuccessful.output.contains("1 7 2d2d6578697437\n")
}
')!
	compiler := os.getenv_opt('VEXE') or { @VEXE }
	child_build := os.exec([compiler, '-gc', 'none', '-nocache', '-cc', 'clang', '-b', 'c', '-o',
		child, child_source])
	assert child_build.exit_code == 0, child_build.output
	assert os.exists(child), child_build.output
	mut parent_build := os.exec([compiler, '-gc', 'none', '-nocache', '-b', 'arm64', '-o', parent,
		parent_source])
	if parent_build.exit_code == 0 && !os.exists(parent) {
		bootstrap := os.exec([compiler, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend',
			'arm64', '-o', test_compiler, os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v')])
		assert bootstrap.exit_code == 0, bootstrap.output
		parent_build = os.exec([test_compiler, '-gc', 'none', '-nocache', '-b', 'arm64', '-o',
			parent, parent_source])
	}
	assert parent_build.exit_code == 0, parent_build.output
	assert os.exists(parent), parent_build.output
	result := os.exec([parent])
	assert result.exit_code == 0, result.output
}

fn test_native_process_capture_preserves_output_arguments_exit_status_and_errors() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_process_capture_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, 'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn C.v_os_execute_capture_start(voidptr, &int, &int) int
fn C.v_os_exec_capture_start(voidptr, &int, &int) int
fn C.read(i32, voidptr, usize) isize
fn C.close(i32) i32
fn C.waitpid(i32, &i32, i32) i32
fn C.memcmp(voidptr, voidptr, usize) i32
fn check_capture(pid int, fd int, expected string, code int) {
    mut buffer := [128]u8{}
    mut total := 0
    for {
        n := C.read(i32(fd), &buffer[total], usize(128 - total))
        if n < 0 { C.exit(1) }
        if n == 0 { break }
        total += int(n)
    }
    C.close(i32(fd))
    if total != expected.len || C.memcmp(&buffer[0], expected.str, usize(total)) != 0 {
        C.exit(2)
    }
    mut status := i32(0)
    if C.waitpid(i32(pid), &status, 0) != pid { C.exit(3) }
    if ((status >> 8) & 255) != code { C.exit(4) }
}
fn main() {
    C.alarm(5)
    mut pid := 0
    mut fd := -1
    command := "printf stdout; printf stderr >&2; exit 7"
    if C.v_os_execute_capture_start(command.str, &pid, &fd) != 0 { C.exit(5) }
    check_capture(pid, fd, "stdoutstderr", 7)
    executable := "/usr/bin/printf"
    format := "%s"
    literal := "literal $(echo bad); with spaces"
    argv := [executable.str, format.str, literal.str, &u8(unsafe { nil })]
    if C.v_os_exec_capture_start(&argv[0], &pid, &fd) != 0 { C.exit(6) }
    check_capture(pid, fd, literal, 0)
    missing := "/codex-native-capture-no-such-executable"
    missing_argv := [missing.str, &u8(unsafe { nil })]
    if C.v_os_exec_capture_start(&missing_argv[0], &pid, &fd) != -1 { C.exit(7) }
    null_argv := [&u8(unsafe { nil })]
    if C.v_os_exec_capture_start(&null_argv[0], &pid, &fd) != -1 { C.exit(8) }
    C.alarm(0)
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
	}
}
