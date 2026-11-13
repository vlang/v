module fastc

import os
import v.cmdexec

fn test_expression_blocks_run_their_own_defers_after_preserving_the_value() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_expression_defer_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global events = []string{}
struct Decoder { bytes []u8 }
fn (d Decoder) close() { events << "close" }
fn decode(mode int) ![]u8 {
    defer { events << "function" }
    return match mode {
        0 {
            decoder := Decoder{bytes: [u8(4), 2]}
            defer { decoder.close() }
            decoder.bytes
        }
        1 {
            decoder := "other"
            defer { events << decoder }
            [u8(decoder.len)]
        }
        else {
            decoder := Decoder{}
            defer { decoder.close() }
            if mode > 1 { return error("failed") }
            []u8{}
        }
    }
}
fn nested() int {
    defer { events << "outer" }
    return if true {
        mut value := 7
        defer { value = 0; events << "branch" }
        if true {
            defer { events << "inner" }
            value + 2
        } else { 0 }
    } else { 0 }
}
fn tuple() (int, string) {
    return match 0 {
        0 {
            mut value := 42
            defer { value = 0; events << "tuple" }
            value, "saved"
        }
        else { 0, "other" }
    }
}
fn function_cleanup() int {
    result := match 0 {
        0 {
            captured := "captured"
            defer(fn) { events << captured }
            defer { events << "lexical" }
            captured.len
        }
        else { 0 }
    }
    events << "after"
    return result
}
fn main() {
    assert decode(0)! == [u8(4), 2]
    assert events == ["close", "function"]
    events.clear()
    assert decode(1)! == [u8(5)]
    assert events == ["other", "function"]
    events.clear()
    decode(2) or { assert err.msg() == "failed" }
    assert events == ["close", "function"]
    events.clear()
    assert nested() == 9
    assert events == ["inner", "branch", "outer"]
    events.clear()
    a, b := tuple()
    assert a == 42 && b == "saved"
    assert events == ["tuple"]
    events.clear()
    assert function_cleanup() == 8
    assert events == ["lexical", "after", "captured"]
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-enable-globals', '-nocache', '-o', program,
			root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
	}
}
