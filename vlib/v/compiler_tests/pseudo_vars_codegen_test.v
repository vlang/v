import os
import v.cmdexec
import v.tempname

// test_at_mod_codegen validates that v3 lowers @MOD to the current module name.
fn test_at_mod_codegen() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_at_mod_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer { os.rmdir_all(tmp_dir) or {} }
	main_src := os.join_path(tmp_dir, 'v3_at_mod_main.v')
	main_bin := os.join_path(tmp_dir, 'v3_at_mod_main')
	os.write_file(main_src, "fn main() {\n\tassert @MOD == 'main'\n}\n")!
	main_result := os.exec([@VEXE, '${main_src}', '-o', main_bin])
	assert main_result.exit_code == 0, main_result.output

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

fn test_quoted_comptime_pseudo_vars_are_not_expanded() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_quoted_pseudo_vars_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer { os.rmdir_all(tmp_dir) or {} }
	main_src := os.join_path(tmp_dir, 'v3_quoted_pseudo_vars_main.v')
	main_bin := os.join_path(tmp_dir, 'v3_quoted_pseudo_vars_main')
	os.write_file(main_src,
		"fn main() {\n\t\$if '@OS' == @OS {\n\t\tprintln('wrong')\n\t} \$else {\n\t\tprintln('ok')\n\t}\n}\n")!
	compile := os.exec([@VEXE, '${main_src}', '-o', main_bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([main_bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// test_embed_file_codegen validates that v3 lowers $embed_file to EmbedFileData.
fn test_embed_file_codegen() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_embed_file_codegen_${tempname.unique_token()}')
	os.mkdir(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	payload := os.join_path(tmp_dir, 'payload.txt')
	os.write_file(payload, 'hello')!
	src := os.join_path(tmp_dir, 'main.v')
	bin := os.join_path(tmp_dir, 'main')
	os.write_file(src,
		"fn main() {\n\tdata := \$embed_file('payload.txt')\n\tassert data.len == 5\n\tassert data.to_string() == 'hello'\n}\n")!
	result := os.exec([@VEXE, '${src}', '-o', bin])
	assert result.exit_code == 0, result.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}

// test_embed_file_at_file_from_relative_subdir_path validates @FILE source path
// resolution for $embed_file().
fn test_embed_file_at_file_from_relative_subdir_path() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v3_embed_file_at_file_codegen_${tempname.unique_token()}')
	os.mkdir_all(os.join_path(tmp_dir, 'sub'))!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	src := os.join_path(tmp_dir, 'sub', 'main.v')
	bin := os.join_path(tmp_dir, 'main')
	os.write_file(src,
		"fn main() {\n\tdata := \$embed_file(@FILE)\n\tassert data.len > 0\n\tassert data.to_string().contains('relative @FILE embed marker')\n}\n\n// relative @FILE embed marker\n")!
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
