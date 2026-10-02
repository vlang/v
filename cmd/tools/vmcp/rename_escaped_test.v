module main

import os

fn test_rename_escaped_method_keeps_the_program_compilable() {
	root := os.join_path(os.vtmp_dir(), 'vmcp_rename_escaped_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	source := 'module main\nstruct Host {}\nfn (h Host) hello() int { return 42 }\nfn main() {\n h := Host{}\n assert h.@hello() == 42\n}\n'
	os.write_file(path, source)!
	ws := Workspace{
		...new_workspace(@VEXEROOT, root, false)
		compiler: @VEXE
	}
	before := run_compiler(ws, ['-check', path])
	assert before.exit_code == 0, before.output
	answer := tool_rename_symbol(ws, '{"name":"hello","new_name":"greet","paths":["main.v"],"dry_run":false}')
	after := os.read_file(path)!
	assert after == source.replace('hello', 'greet'), answer
	assert after.contains('h.@greet()')
	checked := run_compiler(ws, ['-check', path])
	assert checked.exit_code == 0, checked.output
}
