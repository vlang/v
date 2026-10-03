module main

import os

const rename_review_source = "module main\nstruct Host {}\nfn (h Host) hello() int { return 42 }\nfn main() {\n\thello := Host{}\n\tprintln(hello.hello())\n\tbound := hello.hello\n\tprintln(bound())\n\tprintln('hello') // hello\n}\n"

fn rename_review_workspace() Workspace {
	root := os.join_path(os.vtmp_dir(), 'vmcp_rename_method_review_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), rename_review_source) or { panic(err) }
	return new_workspace(@VEXEROOT, root, false)
}

fn test_method_rename_preserves_the_signature_and_receiver_and_compiles() {
	ws := rename_review_workspace()
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
	ws := rename_review_workspace()
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
	ws := rename_review_workspace()
	defer { os.rmdir_all(ws.root) or {} }
	path := os.join_path(ws.root, 'main.v')
	source := rename_review_source.replace('hello :=', 'myhello :=').replace('hello.hello',
		'myhello.hello')
	os.write_file(path, source)!
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
