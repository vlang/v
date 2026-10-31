import os

fn test_doc_comment_punctuation_checks_wrapped_summaries_before_examples() ! {
	root := os.join_path(os.vtmp_dir(), 'doc_comment_punctuation_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	mut tool := os.join_path(root, 'doc_comment_checker')
	$if windows {
		tool += '.exe'
	}
	build := os.exec([@VEXE, '-o', tool, os.join_path(@VEXEROOT, 'cmd', 'tools',
		'find_doc_comments_with_no_dots.v')])
	assert build.exit_code == 0, build.output
	fixture := os.join_path(root, 'comments.v')
	os.write_file(fixture, 'module fixture

// wrapped has a summary that continues on
// the next line.
pub fn wrapped() {}

// sentence has a complete sentence. More details
// may continue on the next line.
pub fn sentence() {}

// parenthesis has a wrapped summary (including
// its final period.)
pub fn parenthesis() {}

// missing has no final
// punctuation
pub fn missing() {}

// example_only has no punctuation
// Example: example_only().
pub fn example_only() {}

// code_only has no punctuation
// ```v
// code_only().
// ```
pub fn code_only() {}
')!
	result := os.exec([tool, fixture])
	assert result.exit_code == 1, result.output
	assert result.output.contains('found errors: 3 '), result.output
	for name in ['missing', 'example_only', 'code_only'] {
		assert result.output.contains('// ${name} '), result.output
	}
	for name in ['wrapped', 'sentence', 'parenthesis'] {
		assert !result.output.contains('// ${name} '), result.output
	}
}
