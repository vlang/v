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
	output_file := os.join_path(tmpdir, 'test_out')
	result := os.execute('${vexe} -autofree -keepc -o ${output_file} ${test_file}')

	if result.exit_code != 0 {
		eprintln('Compilation failed: ${result.output}')
		assert false, 'Compilation should succeed'
	}

	// Find the generated C file in /tmp/v_*/
	tmp_v_pattern := '/tmp/v_*/test_out.tmp.c'
	c_files := os.glob(tmp_v_pattern) or {
		assert false, 'Could not find C file with pattern ${tmp_v_pattern}'
		return
	}

	if c_files.len == 0 {
		assert false, 'No C file found with pattern ${tmp_v_pattern}'
	}

	c_file := c_files[0]
	c_content := os.read_file(c_file) or { panic('Could not read C file: ${err}') }

	check_no_duplicates(c_content)
}

fn check_no_duplicates(c_content string) {
	// Look for the pattern: return; followed by free statements (unreachable code)
	lines := c_content.split_into_lines()

	for i, line in lines {
		if line.contains('return;') {
			// Check if there are any free statements in the next lines before closing brace
			mut found_frees_after_return := []string{}

			for j in i + 1 .. i + 10 {
				if j >= lines.len {
					break
				}
				next_line := lines[j].trim_space()

				// Stop at closing brace
				if next_line.starts_with('}') && !next_line.contains('//') {
					break
				}

				// Check for free statements
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
