// This test verifies that autofree doesn't generate duplicate free statements
// after return in or blocks with string interpolation
import os

fn test_no_duplicate_frees_after_return_in_or_block() {
	test_file_content := '
fn main() {
x := "test"
_ := somefn() or {
msg := "fail"
println("Error: \${msg}")
return
}
}

fn somefn() !int {
return error("test")
}
'

	tmpdir := os.join_path(os.vtmp_dir(), 'test_autofree_or_${os.getpid()}')
	os.mkdir_all(tmpdir) or { panic(err) }
	defer {
		os.rmdir_all(tmpdir) or {}
	}

	test_file := os.join_path(tmpdir, 'test.v')
	os.write_file(test_file, test_file_content) or { panic(err) }

	// Compile with autofree and keepc
	vexe := @VEXE
	unique_name := 'autofree_test_${os.getpid()}'
	output_file := os.join_path(tmpdir, unique_name)

	cmd := '${vexe} -autofree -keepc -o ${output_file} ${test_file} 2>&1'
	result := os.execute(cmd)

	if result.exit_code != 0 {
		eprintln('Compilation failed: ${result.output}')
		assert false, 'Compilation should succeed'
	}

	// Find C file by walking the temp directory
	mut c_file := ''
	v_tmp_dirs := ['/tmp/v_1001', '/tmp/v_0', '/tmp/v_${os.getpid()}']

	for tmp_dir in v_tmp_dirs {
		if !os.exists(tmp_dir) {
			continue
		}

		c_file_path := os.join_path(tmp_dir, '${unique_name}.tmp.c')
		if os.exists(c_file_path) {
			c_file = c_file_path
			break
		}

		// Also check subdirectories
		entries := os.ls(tmp_dir) or { continue }
		for entry in entries {
			if entry.contains('tsession_') {
				subdir_path := os.join_path(tmp_dir, entry, '${unique_name}.tmp.c')
				if os.exists(subdir_path) {
					c_file = subdir_path
					break
				}
			}
		}
		if c_file != '' {
			break
		}
	}

	if c_file == '' {
		assert false, 'Could not find generated C file'
	}

	c_content := os.read_file(c_file) or { panic('Could not read C file: ${err}') }
	check_no_duplicates(c_content)
}

fn check_no_duplicates(c_content string) {
	lines := c_content.split_into_lines()

	for i, line in lines {
		if line.contains('return;') {
			mut found_frees_after_return := []string{}

			for j in i + 1 .. i + 10 {
				if j >= lines.len {
					break
				}
				next_line := lines[j].trim_space()

				if next_line.starts_with('}') && !next_line.contains('//') {
					break
				}

				if next_line.contains('_free(') && next_line.contains('//') {
					found_frees_after_return << next_line
				}
			}

			if found_frees_after_return.len > 0 {
				eprintln('ERROR: Found unreachable free statements after return:')
				for free_line in found_frees_after_return {
					eprintln('  ${free_line}')
				}
				assert false, 'Duplicate free statements found after return (unreachable code). This indicates the autofree bug.'
			}
		}
	}
}
