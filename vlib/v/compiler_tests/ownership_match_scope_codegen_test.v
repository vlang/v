import os

const ownership_match_scope_compile_only = "fn parse(s string) string {
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

@[markused]
fn compile_only_match_scope() {
	println(parse('abc'))
}
"

fn test_ownership_match_scope_runtime_with_simple_and_staged_subjects() {
	root := os.join_path(os.vtmp_dir(), 'ownership_match_scope_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	runtime_source := os.read_file(@VEXEROOT + '/vlib/v/compiler_tests/testdata/ownership_match_scope_runtime.vv')!
	for gc_mode in ['none', 'vgc'] {
		compile_only := gc_mode == 'vgc'
		code := if compile_only {
			runtime_source + '\n' + ownership_match_scope_compile_only
		} else {
			runtime_source
		}
		os.write_file(source, code)!
		keep_c := if compile_only { ['-keepc'] } else { []string{} }
		for i, mode in [[]string{}, ['-no-parallel']] {
			output := os.join_path(root, 'ownership_match_scope_${os.getpid()}_${gc_mode}_${i}')
			build := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-autofree',
				'-gc', gc_mode, '-no-prealloc', ...keep_c, ...mode, '-o', output, source])
			assert build.exit_code == 0, build.output
			if compile_only {
				tmp := os.vtmp_dir()
				prefix := os.file_name(output) + '.'
				retained := os.ls(tmp)!.filter(it.starts_with(prefix) && it.ends_with('.tmp.c'))
				assert retained.len == 1, 'expected one retained C file for ${output}, got ${retained}'
				c_path := os.join_path(tmp, retained[0])
				defer { os.rm(c_path) or {} }
				generated := os.read_file(c_path)!
				assert generated.contains('compile_only_match_scope(void) {'), '${mode}: compile-only function missing'
				assert generated.contains('parse(string s) {'), '${mode}: parse function missing'
			}
			run := os.exec([output])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == 'ok', run.output
		}
	}
}
