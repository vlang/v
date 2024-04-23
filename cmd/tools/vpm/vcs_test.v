module main

fn test_parse_git_version() {
	if _ := parse_git_version('abcd') {
		assert false
	}
	assert parse_git_version('git version 2.44.0.windows.1')! == '2.44.0'
	assert parse_git_version('git version 2.34.0')! == '2.34.0'
	assert parse_git_version('git version 2.39.3 (Apple Git-146)')! == '2.39.3'
}
