import os

fn join_path(parts ...string) string {
	return 'mine:' + parts.join('|')
}

fn test_user_join_path_is_not_lowered_as_os_join_path() {
	assert join_path() == 'mine:'
	assert join_path('/r') == 'mine:/r'
	assert join_path('/r', '.') == 'mine:/r|.'
	assert join_path('a', 'b', 'c') == 'mine:a|b|c'
	assert os.join_path('a', 'b') == 'a' + os.path_separator + 'b'
}
