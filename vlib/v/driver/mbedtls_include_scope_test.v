module driver

import os

fn test_mbedtls_vendored_include_path_matches_only_the_vendored_tree() {
	sep := os.path_separator
	assert mbedtls_vendored_include_path(['C:', 'repo', 'thirdparty', 'mbedtls', 'include'].join(sep))
	assert mbedtls_vendored_include_path('C:/repo/thirdparty/mbedtls/library')
	assert mbedtls_vendored_include_path(['C:', 'repo', 'thirdparty', 'mbedtls'].join(sep))
	assert !mbedtls_vendored_include_path(os.join_path('project', 'sysinclude', 'mbedtls'))
	assert !mbedtls_vendored_include_path('/usr/include')
	assert !mbedtls_vendored_include_path('')
}

fn test_without_mbedtls_vendored_include_dirs_keeps_user_flags() {
	sep := os.path_separator
	vendored := ['C:', 'repo', 'thirdparty', 'mbedtls', 'include'].join(sep)
	user_dir := os.join_path('project', 'sysinclude')
	flags := ['-I', vendored, '-I', user_dir, '-I' + vendored, '-D', 'X=1', user_dir + '.c']
	filtered := without_mbedtls_vendored_include_dirs(flags)
	assert filtered == ['-I', user_dir, '-D', 'X=1', user_dir + '.c'], filtered.str()
	assert without_mbedtls_vendored_include_dirs([]string{}) == []
}
