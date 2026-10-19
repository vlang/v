import os

fn test_args_after_and_before_cut_at_the_first_argument() {
	assert os.args.len > 0
	// NOTE: `os.args` is a `const` built from the process argv, so the loop in
	// args_after/args_before can only be reached with the real argv of the
	// test binary, which the test runner starts without extra arguments.
	// `argv[0]` is the executable path, so cutting there leaves only argv[0].
	first := os.args[0]
	assert os.args_after(first) == [first]
	assert os.args_before(first) == [first]
}

fn test_args_after_and_before_keep_args_when_the_word_is_absent() {
	absent := 'a-word-that-is-not-part-of-the-process-argv'
	assert absent !in os.args
	assert os.args_after(absent) == os.args
	assert os.args_before(absent) == os.args
}
