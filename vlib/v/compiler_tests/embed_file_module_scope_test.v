import os

fn embed_scope_project(name string) string {
	root := os.join_path(os.vtmp_dir(), 'embed_file_scope_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'model')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'embed_scope' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'data.txt'), 'hello') or { panic(err) }
	os.write_file(os.join_path(root, 'data.v'), 'module main
const embedded_text = \$embed_file("data.txt").to_string()
') or { panic(err) }
	// The compiler inserts its runtime import into the final file. Neither the
	// constant above nor the expression in main.v has a source-written import.
	os.write_file(os.join_path(root, 'z.v'), 'module main
fn final_file_value() int { return 13 }
') or { panic(err) }
	os.write_file(os.join_path(root, 'model', 'model.v'), 'module model
import v.embed_file
pub type Source = embed_file.EmbedFileData | string
pub fn length(source Source) int {
	match source {
		embed_file.EmbedFileData { return source.to_bytes().len }
		string { return source.len }
	}
}
') or { panic(err) }
	return root
}

fn test_synthetic_embed_file_type_does_not_require_a_source_import() {
	root := embed_scope_project('synthetic')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import model
fn main() {
	data := \$embed_file("data.txt")
	spaced := \$ embed_file("data.txt")
	continued := \$
		embed_file("data.txt")
	commented := \$/**/embed_file/* call */("data.txt")
	assert data.to_string() == "hello"
	assert data.to_bytes() == "hello".bytes()
	assert spaced.to_string() == "hello"
	assert continued.to_string() == "hello"
	assert commented.to_string() == "hello"
	assert model.length(data) == 5
	assert embedded_text == "hello"
	assert final_file_value() == 13
	println("embedded across files")
}
') or { panic(err) }
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	for mode in ['-g', '-prod'] {
		build := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', mode, '-o', binary,
			root])
		assert build.exit_code == 0, '${mode}: ${build.output}'
		run := os.exec([binary])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'embedded across files'
	}
}

fn test_written_embed_file_type_still_requires_a_source_import() {
	root := embed_scope_project('written')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn main() {
	_ = embed_file.EmbedFileData{}
	assert embedded_text == "hello"
}
') or { panic(err) }
	checked := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', root])
	assert checked.exit_code != 0, checked.output
	assert checked.output.contains('unknown module `embed_file`'), checked.output
}

fn test_written_embed_file_type_with_an_import_remains_allowed() {
	root := embed_scope_project('imported')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import v.embed_file
fn main() {
	data := embed_file.EmbedFileData{len: 5, path: "manual"}
	assert data.len == 5
	assert data.path == "manual"
	assert embedded_text == "hello"
}
') or { panic(err) }
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	build := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-o', binary, root])
	assert build.exit_code == 0, build.output
	run := os.exec([binary])
	assert run.exit_code == 0, run.output
}
