module pref

fn test_check_parametes() {
	// reproducing issue https://github.com/vlang/v/issues/13983
	_, cmd := parse_args_and_show_errors(['help'], [''], true)
	// no command found from args
	assert cmd == ''
}
