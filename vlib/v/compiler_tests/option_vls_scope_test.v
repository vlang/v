import os

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
	vexe := os.quoted_path(@VEXE)
	source_path := os.quoted_path(path)
	lines := source.split_into_lines()
	declaration := lines.index("	err := 'outer'") + 1
	for i, line in lines {
		if !line.contains('println(err)') {
			continue
		}
		column := line.index('err') or { panic('missing identifier') }
		position := '${path}:${i + 1}:'
		for query in ['hv^', 'gd^'] {
			spec := os.quoted_path('${position}${query}${column + 1}')
			command := '${vexe} -check -vls-mode -line-info ${spec}'
			result := os.exec([...(os.split_args(command) or { panic(err) }), path])
			assert result.exit_code == 0, result.output
			expected := if query == 'hv^' { 'err string' } else { ':${declaration}:1' }
			assert result.output.contains(expected), result.output
		}
	}
}
