import os
import v.cmdexec

fn test_option_handlers_preserve_outer_err_for_vls() {
	source := "fn maybe() ?int {
	return none
}

fn main() {
	err := 'outer'
	_ := maybe() or {
		println(err)
		0
	}
	if _ := maybe() {
	} else {
		println(err)
	}
}
"
	path := os.join_path(os.vtmp_dir(), 'option_vls_${os.getpid()}.v')
	os.write_file(path, source)!
	defer { os.rm(path) or {} }
	lines := source.split_into_lines()
	declaration := lines.index("	err := 'outer'") + 1
	mut questions := []string{}
	for i, line in lines {
		if !line.contains('println(err)') {
			continue
		}
		column := line.index('err') or { panic('missing identifier') }
		position := '${path}:${i + 1}:'
		questions << '${position}hv^${column + 1}'
		questions << '${position}gd^${column + 1}'
	}
	result := cmdexec.run(@VEXE, ['-check', '-vls-mode', '-line-info', questions.join('\t'), path])
	assert result.exit_code == 0, result.output
	answers := result.output.trim_space().split_into_lines()
	assert answers.len == questions.len, result.output
	for i, answer in answers {
		prefix := '${i}\t'
		assert answer.starts_with(prefix), answer
		expected := if i % 2 == 0 { 'err string' } else { ':${declaration}:1' }
		assert answer[prefix.len..].contains(expected), '${questions[i]}: ${answer}'
	}
}
