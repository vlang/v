module fastc

import os
import v.cmdexec

// A generic argument written as an `or`, `match` or `if` expression must select a
// concrete instance; the erased placeholder exits when it is called.
fn test_branching_generic_arguments_select_concrete_instances() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_branching_argument_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import x.json2
fn describe[T](value T) string { return typeof(value).name + "=" + json2.encode(value) }
fn name() ?string { return "a" }
fn missing() ?string { return none }
fn ready() !bool { return true }
fn title() string { return "doc" }
fn property(key string) !string {
    return json2.encode(match key {
        "title" { title() }
        "empty" { missing() or { "" } }
        else { return error("invalid property") }
    })
}
fn attribute(key string) !string {
    return json2.encode(name() or { return error(key) })
}
fn record(key string) string {
    return json2.encode(match key {
        "mode" { json2.Any("open") }
        else { json2.Any(true) }
    })
}
fn main() {
    flag := true
    assert describe(name() or { "z" }) == "string=\"a\""
    assert describe(ready() or { false }) == "bool=true"
    assert describe(if flag { 1 } else { 2 }) == "int=1"
    assert property("title")! == "\"doc\""
    assert property("empty")! == "\"\""
    if _ := property("other") { assert false }
    assert attribute("k")! == "\"a\""
    assert record("mode") == "\"open\""
    assert record("other") == "true"
}
')!
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
