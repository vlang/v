import os

fn test_ownership_match_scope() {
	root := os.join_path(os.vtmp_dir(), 'ownership_match_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "fn parse(s string) string {
	for {
		if s.len > 0 {
			match s[0] {
				`a` {}
				else {}
			}
			if s.len > 1 {
				continue
			}
			value := s[0..1]
			println(value)
			continue
		}
		break
	}
	return s
}

fn main() {
	println(parse('abc'))
}
")!
	for mode in ['', '-no-parallel'] {
		output := os.join_path(root, 'app')
		mut args := [@VEXE, '-new-compiler', '-nocache', '-ownership', '-d', 'ownership', '-autofree',
			'-gc', 'vgc', '-no-prealloc', '-o', output]
		if mode != '' {
			args << mode
		}
		args << source
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
}
