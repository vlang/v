module fastc

import os
import v.cmdexec
import v.pref

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
