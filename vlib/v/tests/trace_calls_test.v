import os

const vexe = @VEXE

fn test_tracing() {
	os.chdir(@VROOT)!
	folder := os.join_path('vlib', 'v', 'tests', 'testdata', 'trace_calls')
	files := os.walk_ext(folder, '.vv')
	for fpath in files {
		should_match_fpath := '${fpath}.must_match'
		if !os.exists(should_match_fpath) {
			eprintln('> skipping ${fpath}, because ${should_match_fpath} does not exist.')
			continue
		}
		res := os.execute('${os.quoted_path(vexe)} -trace-calls run ${os.quoted_path(fpath)}')
		if res.exit_code != 0 {
			eprintln('> compilation output:\n${res.output}')
			assert res.exit_code == 0, 'compilation of ${fpath} failed'
		}
		lines := os.read_lines(should_match_fpath) or {
			assert false, '${fpath} should be readable'
			return
		}
		if lines.len == 0 {
			assert false, '${should_match_fpath} should contain at least one line/glob match pattern'
		}
		mut matched := false
		for line in lines {
			if res.output.match_glob(line) {
				matched = true
				println('> trace output of ${fpath} matches line pattern: ${line}')
				continue
			} else {
				eprintln(res.output)
				assert false, '> trace output of ${fpath} DID NOT match the line pattern: ${line}'
			}
		}
	}
}
