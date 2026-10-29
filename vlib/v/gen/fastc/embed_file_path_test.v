module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_embed_file_descriptors_preserve_binary_payloads() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_embed_descriptor_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut payload := []u8{}
	for i in 0 .. 256 {
		payload << u8(i)
	}
	os.write_file_array(os.join_path(root, 'payload.bin'), payload) or { panic(err) }
	os.write_file(os.join_path(root, 'empty.bin'), '') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main

const resource = $embed_file("payload.bin")
const empty = $embed_file("empty.bin")

fn main() {
	assert resource.len == 256
	assert resource.path == "payload.bin"
	mut bytes := resource.to_bytes()
	assert bytes.len == 256
	for i, value in bytes {
		assert value == u8(i)
	}
	text := resource.to_string()
	assert text.len == 256
	assert text.bytes() == bytes
	assert $embed_file("payload.bin").to_string() == text
	assert $embed_file("payload.bin").to_bytes() == bytes
	local := $embed_file("payload.bin")
	assert local.to_bytes() == bytes
	unsafe { assert local.data()[0] == 0 }
	bytes[0] = 99
	assert resource.to_bytes()[0] == 0
	assert empty.len == 0
	assert empty.to_string() == ""
	assert empty.to_bytes().len == 0
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	os.rm(os.join_path(root, 'payload.bin')) or { panic(err) }
	os.rm(os.join_path(root, 'empty.bin')) or { panic(err) }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_embed_file_resolves_compiler_and_module_roots() {
	mut prefs := pref.new_preferences()
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	root := os.join_path(os.vtmp_dir(), 'fastc_embed_paths_${os.getpid()}')
	compiler_root := os.join_path(root, 'compiler')
	module_root := os.join_path(root, 'project')
	source_dir := os.join_path(module_root, 'src')
	os.mkdir_all(compiler_root) or { panic(err) }
	os.mkdir_all(source_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(compiler_root, 'payload.txt'), 'compiler') or { panic(err) }
	os.write_file(os.join_path(module_root, 'payload.txt'), 'module') or { panic(err) }
	os.write_file(os.join_path(source_dir, 'payload.txt'), 'source') or { panic(err) }
	os.write_file(os.join_path(module_root, 'v.mod'), "Module { name: 'embed_paths' }") or {
		panic(err)
	}
	prefs.vroot = compiler_root
	c_source := generate('module main
fn identity(value string) string { return value }
fn main() {
	compiler := \$embed_file("@VEXEROOT/payload.txt").to_string()
	project := \$embed_file("@VMODROOT/payload.txt").to_string()
	source := \$embed_file("payload.txt").to_string()
	println(compiler)
	println(project)
	println(source)
	println(identity(\$embed_file("@VEXEROOT/payload.txt").to_string()))
}
', os.join_path(source_dir, 'main.v'), prefs) or { panic(err) }
	c_file := os.join_path(root, 'program.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', program, c_file])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'compiler\nmodule\nsource\ncompiler\n'
}

fn test_fastc_nested_embed_file_method_chain_executes() {
	root := os.join_path(os.vtmp_dir(), 'fastc_embed_method_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'embed_method' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'payload.txt'), 'start') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'fn wrap(value string) string { return value }
fn main() {
	println(wrap(\$embed_file("@VMODROOT/payload.txt").to_string().replace("start", "finish")))
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'finish\n'
}

// A declaration whose `$embed_file(...).to_string()` value is continued on the next
// lines by leading `.replace(...)` calls (as in vlib/v/gen/c/bootstrap_windows_helpers.v)
// keeps the whole chain as its value.
fn test_fastc_embed_file_declaration_continued_by_member_chain() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_embed_chain_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'payload.txt'), 'alpha beta\ngamma\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn chained() string {
	text := \$embed_file("payload.txt").to_string()
		.replace("alpha", "one")
		.replace("gamma\\n", "three")
	return text
}

fn main() {
	assert chained() == "one beta\\nthree"
	same_line := \$embed_file("payload.txt").to_string().replace("beta", "two")
	assert same_line == "alpha two\\ngamma\\n"
	plain := \$embed_file("payload.txt").to_string()
	assert plain.len == 17
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
