import os

// Regression for #28692: diagnostics from earlier files must survive parsing
// later files, including result-returning functions with escaped newlines.
fn test_vet_multifile_diagnostics() {
	vexe := os.getenv('VEXE')
	// Vet ignores paths inside VTMP, so use the system temporary directory.
	tmp := os.join_path(os.temp_dir(), 'vvet_multifile_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	os.mkdir_all(tmp)!
	defer {
		os.rmdir_all(tmp) or {}
	}
	mut paths := []string{}
	mut warnings := []string{}
	mut errors := []string{}
	for i in 0 .. 64 {
		path := os.join_path(tmp, 'file_${i}.v')
		declaration := 'pub fn f${i}(args []string) !string'
		source := "module m\n\n${declaration} {\n\tif args.len == 0 {\n\t\treturn error('missing subcommand ${i}.\\nExample: mycli foo status')\n\t}\n\treturn args[0]\n}\n"
		os.write_file(path, source)!
		paths << path
		message := 'Function documentation seems to be missing for "${declaration}".'
		warnings << '${path}:3: warning: ${message}'
		errors << '${path}:3: error: ${message}'
		// Exercise the original two-file case and then force more parser allocations.
		if paths.len !in [2, 64] {
			continue
		}
		for j, file in paths[..2] {
			res := os.execute('${os.quoted_path(vexe)} vet -nocolor ${os.quoted_path(file)}')
			assert res.exit_code == 0, res.output
			assert_vet_diagnostics(res.output, [warnings[j]])
		}
		res := os.execute('${os.quoted_path(vexe)} vet -nocolor ${os.quoted_path(tmp)}')
		assert res.exit_code == 0, res.output
		assert_vet_diagnostics(res.output, warnings)
		werror := os.execute('${os.quoted_path(vexe)} vet -nocolor -W ${os.quoted_path(tmp)}')
		assert werror.exit_code == 1, werror.output
		assert_vet_diagnostics(werror.output, errors)
		hidden := os.execute('${os.quoted_path(vexe)} vet -nocolor -hide-warnings ${os.quoted_path(tmp)}')
		assert hidden.exit_code == 0, hidden.output
		assert hidden.output.trim_space() == '', hidden.output
	}
}

fn test_vet_multifile_notices_and_errors() {
	vexe := os.getenv('VEXE')
	tmp := os.join_path(os.temp_dir(), 'vvet_multifile_mixed_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	os.mkdir_all(tmp)!
	defer {
		os.rmdir_all(tmp) or {}
	}
	mut expected := []string{}
	for i in 0 .. 32 {
		path := os.join_path(tmp, 'file_${i}.v')
		// A string-length notice and a trailing-space error, without doc warnings.
		source := 'module m\n\nfn f${i}(s string) bool {\n\treturn s.len == 0 \n}\n'
		os.write_file(path, source)!
		expected << "${path}:4: notice: Use `s == ''` instead of `s.len == 0`"
		expected << '${path}:4: error: Looks like you have trailing whitespace.'
	}
	res := os.execute('${os.quoted_path(vexe)} vet -nocolor ${os.quoted_path(tmp)}')
	assert res.exit_code == 1, res.output
	assert_vet_diagnostics(res.output, expected)
}

fn test_vet_multifile_analysis_diagnostics() {
	vexe := os.getenv('VEXE')
	tmp := os.join_path(os.temp_dir(), 'vvet_multifile_analysis_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	os.mkdir_all(tmp)!
	defer {
		os.rmdir_all(tmp) or {}
	}
	mut expected := []string{}
	for i in 0 .. 16 {
		path := os.join_path(tmp, 'file_${i}.v')
		mut source := 'module m\n\nfn f${i}() {\n'
		for j in 0 .. 10 {
			source += '\tprintln(1)\n'
			expected << '${path}:${j + 4}: notice: println(1) occurs ${j + 1}/10 times in function scope (m.f${i}).'
		}
		source += '}\n'
		os.write_file(path, source)!
	}
	res := os.execute('${os.quoted_path(vexe)} vet -nocolor -r -I ${os.quoted_path(tmp)}')
	assert res.exit_code == 0, res.output
	assert_vet_diagnostics(res.output, expected)
}

fn assert_vet_diagnostics(output string, expected []string) {
	assert !output.contains('\x00'), output
	mut actual := output.replace('\r\n', '\n').trim_right('\n').split_into_lines()
	mut sorted_expected := expected.clone()
	// Directory traversal order is platform dependent; check every complete line.
	actual.sort()
	sorted_expected.sort()
	assert actual == sorted_expected, output
}
