import os

// A compiler that emits V marks its output with `#line N "file"`, so that the locations V
// reports at runtime and in debug information point to the original source.
const generated_source = 'import os

// generated from app.zbr
@[noinline]
fn check(n int) int {
#line 7 "app.zbr"
	if n > 2 {
#line 8 "app.zbr"
		panic("n is too big")
	}
	return n
}

fn main() {
#line 20 "app.zbr"
	x := check(1)
#line 21
	dump(x)
	if os.args.len > 1 {
#line 30
		check(5)
	}
#line 40
	assert x == 2
}
'

fn write_generated_program(name string) !(string, string) {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	os.write_file(source, generated_source)!
	return root, source
}

fn test_runtime_locations_follow_line_directives() {
	root, source := write_generated_program('line_directive_run')!
	defer { os.rmdir_all(root) or {} }
	executable := os.join_path(root, 'program')
	build := os.exec([@VEXE, '-new-compiler', '-o', executable, source])
	assert build.exit_code == 0, build.output
	run := os.exec([executable])
	assert run.exit_code != 0
	assert run.output.contains('[app.zbr:21] x: 1'), run.output
	assert run.output.contains('app.zbr:40: FAIL: fn main.main: assert x == 2'), run.output
}

fn test_debug_panic_location_follows_line_directives() {
	root, source := write_generated_program('line_directive_panic')!
	defer { os.rmdir_all(root) or {} }
	executable := os.join_path(root, 'program')
	build := os.exec([@VEXE, '-new-compiler', '-g', '-o', executable, source])
	assert build.exit_code == 0, build.output
	run := os.exec([executable, 'panic'])
	assert run.exit_code != 0
	assert run.output.contains('n is too big'), run.output
	assert run.output.contains('file: app.zbr:8'), run.output
}

fn test_debug_line_directives_in_c_follow_line_directives() {
	root, source := write_generated_program('line_directive_c')!
	defer { os.rmdir_all(root) or {} }
	output := os.join_path(root, 'main.c')
	for flags in [['-g'], ['-g', '-no-parallel']] {
		build := os.exec([@VEXE, '-new-compiler', ...flags, '-o', output, source])
		assert build.exit_code == 0, build.output
		generated := os.read_file(output)!
		path := os.real_path(source).replace('\\', '/').replace('"', '\\"')
		// The function starts before the first directive.
		assert generated.contains('#line 5 "${path}"\n'), flags.str()
		assert generated.contains('#line 7 "app.zbr"\n'), flags.str()
		assert generated.contains('#line 8 "app.zbr"\n'), flags.str()
		assert generated.contains('#line 20 "app.zbr"\n'), flags.str()
		assert generated.contains('#line 30 "app.zbr"\n'), flags.str()
		assert generated.contains('#line 40 "app.zbr"\n'), flags.str()
		assert !generated.contains('#line 7 "${path}"'), flags.str()
	}
}

fn test_compiler_messages_follow_line_directives() {
	root := os.join_path(os.vtmp_dir(), 'line_directive_error_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	logical := os.join_path(root, 'app.zbr')
	os.write_file(logical, 'a = 1\nprint(b)\n')!
	os.write_file(source, 'fn main() {\n#line 2 "${logical}"\n\tprintln(b)\n}\n\nfn f() {\n#line 9 "missing.zbr"\n\tprintln(c)\n}\n')!
	result := os.exec([@VEXE, '-new-compiler', '-nocolor', source])
	assert result.exit_code != 0
	// The excerpt comes from the logical file when it can be read...
	assert result.output.contains('app.zbr:2:10: error: undefined ident: `b`'), result.output
	assert result.output.contains('    2 | print(b)\n'), result.output
	// ... else it is the generated line, numbered as the logical line.
	assert result.output.contains('missing.zbr:9:10: error: undefined ident: `c`'), result.output
	assert result.output.contains('    9 |     println(c)\n'), result.output
	// `-json-errors` reports the same locations.
	json := os.exec([@VEXE, '-new-compiler', '-json-errors', source])
	assert json.exit_code != 0
	assert json.output.contains('app.zbr","line":2,"col":10,'), json.output
	assert json.output.contains('{"file":"missing.zbr","line":9,"col":10,'), json.output
}
