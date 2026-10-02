module main

import os

fn test_rename_struct_field_updates_initializer_keys_and_keeps_the_program_compilable() {
	root := os.join_path(os.vtmp_dir(), 'vmcp_rename_field_init_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	ws := Workspace{
		...new_workspace(@VEXEROOT, root, false)
		compiler: @VEXE
	}
	for source in [
		'module main\nstruct Host { hello int }\nfn main() { h := Host{hello: 42}; assert h.hello == 42 }\n',
		'module main
struct Host { hello int }
struct Nested { hello Host }
fn main() {
 hello := 42
 h := Host{
  hello:
   hello // keep hello
 }
 nested := Nested{
  hello: Host{
   hello: 42
  }
 }
 copied := Host{...h, hello: 43}
 assert h.hello == 42
 assert nested.hello.hello == 42
 assert copied.hello == 43
 println("hello") // keep hello
}
',
	] {
		os.write_file(path, source)!
		before := run_compiler(ws, ['-check', path])
		assert before.exit_code == 0, before.output
		plan := tool_rename_symbol(ws, '{"name":"hello","new_name":"greet","paths":["main.v"]}')
		assert !plan.contains('"error"'), plan
		assert os.read_file(path)! == source
		result := tool_rename_symbol(ws, '{"name":"hello","new_name":"greet","paths":["main.v"],"dry_run":false}')
		assert !result.contains('"error"'), result
		after := os.read_file(path)!
		expected := source.replace('hello', 'greet').replace('keep greet', 'keep hello').replace('println("greet")',
			'println("hello")')
		assert after == expected, result
		checked := run_compiler(ws, ['-check', path])
		assert checked.exit_code == 0, checked.output
	}
}
