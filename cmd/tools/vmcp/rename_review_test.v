module main

import os

const rename_review_source = "module main\nstruct Host {}\nfn (h Host) hello() int { return 42 }\nfn main() {\n\thello := Host{}\n\tprintln(hello.hello())\n\tbound := hello.hello\n\tprintln(bound())\n\tprintln('hello') // hello\n}\n"

fn rename_review_workspace(source string) Workspace {
	root := os.join_path(os.vtmp_dir(), 'vmcp_rename_method_review_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	return new_workspace(@VEXEROOT, root, false)
}

fn test_method_rename_preserves_the_signature_and_receiver_and_compiles() {
	ws := rename_review_workspace(rename_review_source)
	defer { os.rmdir_all(ws.root) or {} }
	path := os.join_path(ws.root, 'main.v')
	plan := tool_rename_symbol(&ws, '{"name":"hello","new_name":"greet"}')
	assert !plan.contains('"error"'), plan
	assert os.read_file(path)! == rename_review_source
	hits := rename_hits(path, 'hello')
	assert hits.len == 6, '${hits}'
	result := tool_rename_symbol(&ws, '{"name":"hello","new_name":"greet","dry_run":false}')
	assert !result.contains('"error"'), result
	after := os.read_file(path)!
	assert after.contains('fn (h Host) greet() int { return 42 }'), after
	assert after.contains('println(greet.greet())'), after
	assert after.contains('bound := greet.greet'), after
	assert after.contains("println('hello') // hello"), after
	checked := os.exec([@VEXE, '-new-compiler', '-no-memory-limit', '-no-retry-compilation', '-check',
		path])
	assert checked.exit_code == 0, checked.output
}

fn test_method_rename_rejects_stale_same_length_source_without_writing() {
	ws := rename_review_workspace(rename_review_source)
	defer { os.rmdir_all(ws.root) or {} }
	path := os.join_path(ws.root, 'main.v')
	hits := rename_hits(path, 'hello')
	changed := rename_review_source.replace('hello', 'other')
	os.write_file(path, changed)!
	apply_rename(path, hits, 'hello', 'greet') or {
		assert err.msg().contains('not `hello`'), err.msg()
		assert os.read_file(path)! == changed
		return
	}
	assert false, 'stale same-length source must refuse the rename'
}

fn test_method_rename_preserves_receiver_identifiers_containing_the_method_name() {
	source := rename_review_source.replace('hello :=', 'myhello :=').replace('hello.hello',
		'myhello.hello')
	ws := rename_review_workspace(source)
	defer { os.rmdir_all(ws.root) or {} }
	path := os.join_path(ws.root, 'main.v')
	assert rename_hits(path, 'hello').len == 3
	result := tool_rename_symbol(&ws, '{"name":"hello","new_name":"greet","dry_run":false}')
	assert !result.contains('"error"'), result
	after := os.read_file(path)!
	assert after.contains('myhello := Host{}'), after
	assert after.contains('println(myhello.greet())'), after
	assert after.contains('bound := myhello.greet'), after
	assert after.contains("println('hello') // hello"), after
	checked := os.exec([@VEXE, '-new-compiler', '-no-memory-limit', '-no-retry-compilation', '-check',
		path])
	assert checked.exit_code == 0, checked.output
}

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
