module os

fn test_is_abs_path() {
	$if windows {
		assert is_abs_path('/')
		assert is_abs_path('\\')
		assert !is_abs_path('\\\\')
		assert is_abs_path(r'C:\path\to\files\file.v')
		assert is_abs_path(r'\\Host\share')
		assert is_abs_path(r'//Host\share\files\file.v')
		assert is_abs_path(r'\\.\BootPartition\Windows')
		assert !is_abs_path(r'\\.\')
		assert !is_abs_path(r'\\?\\')
		assert !is_abs_path(r'C:path\to\dir')
		assert !is_abs_path(r'dir')
		assert !is_abs_path(r'.\')
		assert !is_abs_path(r'.')
		assert !is_abs_path(r'\\Host')
		assert !is_abs_path(r'\\Host\')
		return
	}
	assert is_abs_path('/')
	assert is_abs_path('/path/to/files/file.v')
	assert !is_abs_path('\\')
	assert !is_abs_path('path/to/files/file.v')
	assert !is_abs_path('dir')
	assert !is_abs_path('./')
	assert !is_abs_path('.')
}
