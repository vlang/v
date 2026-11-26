import os
import v.cmdexec
import v.tempname

fn test_at_mod_codegen_in_named_module() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_at_mod_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer { os.rmdir_all(tmp_dir) or {} }

	module_test_src := os.join_path(tmp_dir, 'v3_at_mod_module_test.v')
	module_test_bin := os.join_path(tmp_dir, 'v3_at_mod_module_test')
	os.write_file(module_test_src,
		"module sample\n\nfn test_at_mod() {\n\tassert @MOD == 'sample'\n}\n")!
	module_test_result := os.exec([@VEXE, module_test_src, '-o', module_test_bin])
	assert module_test_result.exit_code == 0, module_test_result.output
}

fn test_at_file_line_codegen_uses_source_line() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_file_line_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer { os.rmdir_all(tmp_dir) or {} }
	main_src := os.join_path(tmp_dir, 'v3_file_line_main.v')
	main_bin := os.join_path(tmp_dir, 'v3_file_line_main')
	os.write_file(main_src,
		"fn main() {\n\tgot := @FILE_LINE\n\texpected := 'v3_file_line_main.v:' + @LINE.str()\n\tprintln(got)\n\tprintln(expected)\n}\n")!
	main_result := os.exec([@VEXE, '-nocache', '-o', main_bin, '${main_src}'])
	assert main_result.exit_code == 0, main_result.output
	run := os.exec([main_bin])
	assert run.exit_code == 0, run.output
	assert run.output == 'v3_file_line_main.v:2\nv3_file_line_main.v:3\n', run.output
}

fn test_main_pseudo_vars_and_quoted_comptime_literals() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_main_pseudo_vars_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer { os.rmdir_all(tmp_dir) or {} }
	main_src := os.join_path(tmp_dir, 'v3_main_pseudo_vars.v')
	main_bin := os.join_path(tmp_dir, 'v3_main_pseudo_vars')
	os.write_file(main_src, "fn main() {
	assert @MOD == 'main'
	\$if '@OS' == @OS {
		println('wrong')
	} \$else {
		println('ok')
	}
}
")!
	compile := os.exec([@VEXE, '${main_src}', '-o', main_bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([main_bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}

// Both the literal asset path and @FILE resolve beside the source file.
fn test_embed_file_resolves_paths_from_relative_subdir() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_embed_file_at_file_codegen_${tempname.unique_token()}')
	os.mkdir_all(os.join_path(tmp_dir, 'sub'))!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'sub', 'payload.txt'), 'hello')!
	src := os.join_path(tmp_dir, 'sub', 'main.v')
	bin := os.join_path(tmp_dir, 'main')
	os.write_file(src, "fn main() {
	payload := \$embed_file('payload.txt')
	assert payload.len == 5
	assert payload.to_string() == 'hello'

	data := \$embed_file(@FILE)
	assert data.len > 0
	assert data.to_string().contains('relative @FILE embed marker')
}

// relative @FILE embed marker
")!
	result := cmdexec.run_in(@VEXE, ['sub/main.v', '-o', bin], tmp_dir)
	assert result.exit_code == 0, result.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}

// test_prod_embed_file_keeps_bytes_after_source_removed validates that -prod
// embeds do not reload the asset at runtime.
fn test_prod_embed_file_keeps_bytes_after_source_removed() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_prod_embed_file_codegen_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	payload := os.join_path(tmp_dir, 'payload.txt')
	os.write_file(payload, 'original payload')!
	src := os.join_path(tmp_dir, 'main.v')
	bin := os.join_path(tmp_dir, 'main')
	os.write_file(src,
		"fn main() {\n\tdata := \$embed_file('payload.txt')\n\tassert data.to_string() == 'original payload'\n}\n")!
	result := os.exec([@VEXE, '-prod', '${src}', '-o', bin])
	assert result.exit_code == 0, result.output
	os.rm(payload)!
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}

// test_imported_module_embed_file_codegen validates implicit v.embed_file imports
// after imported module parsing.
fn test_imported_module_embed_file_codegen() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_imported_embed_file_codegen_${tempname.unique_token()}')
	os.mkdir_all(os.join_path(tmp_dir, 'assets'))!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'v.mod'), 'Module { name: "imported_embed" }\n')!
	os.write_file(os.join_path(tmp_dir, 'assets', 'payload.txt'), 'hello')!
	os.write_file(os.join_path(tmp_dir, 'assets', 'assets.v'),
		"module assets\n\npub fn message() string {\n\tdata := \$embed_file('payload.txt')\n\treturn data.to_string()\n}\n")!
	src := os.join_path(tmp_dir, 'main.v')
	bin := os.join_path(tmp_dir, 'main')
	os.write_file(src, "import assets\n\nfn main() {\n\tassert assets.message() == 'hello'\n}\n")!
	result := os.exec([@VEXE, '${src}', '-o', bin])
	assert result.exit_code == 0, result.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}
