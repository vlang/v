import os

const vexe = @VEXE

fn test_parse_text_in_stdout_mode_unwinds_caller_defer() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_parser_issue_14438')
	os.mkdir_all(tmp_dir) or { panic(err) }
	program_path := os.join_path(tmp_dir, 'issue_14438.v')
	program := "import v.ast\nimport v.parser\nimport v.pref\n\nconst bad_source = 'fn main() {\\n\\tx := 1 +\\n}\\n'\n\nfn run_parse() {\n\tmut table := ast.new_table()\n\tmut prefs := pref.new_preferences()\n\tprefs.output_mode = .stdout\n\tdefer {\n\t\tprintln('<defer-ran>')\n\t}\n\tfile := parser.parse_text(bad_source, 'bad.v', mut table, .skip_comments, prefs)\n\tif file.errors.len == 0 {\n\t\texit(2)\n\t}\n\tprintln('<after-parse>')\n}\n\nfn main() {\n\trun_parse()\n}\n"
	os.write_file(program_path, program) or { panic(err) }
	res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(program_path)}')
	assert res.exit_code == 0, 'expected parser.parse_text to return normally, output:\n${res.output}'
	assert res.output.contains('error:'), 'expected a parser error, output:\n${res.output}'
	assert res.output.contains('<defer-ran>'), 'expected defer to run, output:\n${res.output}'
	assert res.output.contains('<after-parse>'), 'expected execution to continue after parse_text, output:\n${res.output}'
}
