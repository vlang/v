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

fn test_clean_path() {
	$if windows {
		assert clean_path(r'\\path\to\files/file.v') == r'\path\to\files\file.v'
		assert clean_path(r'\/\//\/') == '\\'
		assert clean_path(r'./path\\dir/\\./\/\\/file.v\.\\\.') == r'path\dir\file.v'
		assert clean_path(r'\./path/dir\\file.exe') == r'\path\dir\file.exe'
		assert clean_path(r'.') == ''
		assert clean_path(r'./') == ''
		assert clean_path('') == ''
		assert clean_path(r'\./') == '\\'
		assert clean_path(r'//\/\/////') == '\\'
		return
	}
	assert clean_path('./../.././././//') == '../..'
	assert clean_path('') == ''
	assert clean_path('.') == ''
	assert clean_path('./path/to/file.v//./') == 'path/to/file.v'
	assert clean_path('./') == ''
	assert clean_path('/.') == '/'
	assert clean_path('//path/./to/.///files/file.v///') == '/path/to/files/file.v'
	assert clean_path('path/./to/.///files/.././file.v///') == 'path/to/files/../file.v'
	assert clean_path('\\') == '\\'
	assert clean_path('//////////') == '/'
}

fn test_to_slash() {
	sep := path_separator
	assert to_slash('') == ''
	assert to_slash(sep) == ('/')
	assert to_slash([sep, 'a', sep, 'b'].join('')) == '/a/b'
	assert to_slash(['a', sep, sep, 'b'].join('')) == 'a//b'
}

fn test_from_slash() {
	sep := path_separator
	assert from_slash('') == ''
	assert from_slash('/') == sep
	assert from_slash('/a/b') == [sep, 'a', sep, 'b'].join('')
	assert from_slash('a//b') == ['a', sep, sep, 'b'].join('')
}

fn test_norm_path() {
	$if windows {
		assert norm_path(r'C:/path/to//file.v\\') == r'C:\path\to\file.v'
		assert norm_path(r'C:path\.\..\\\.\to//file.v') == r'C:to\file.v'
		assert norm_path(r'D:path\.\..\..\\\\.\to//dir/..\') == r'D:..\to'
		assert norm_path(r'D:/path\.\..\/..\file.v') == r'D:\file.v'
		assert norm_path(r'') == '.'
		assert norm_path(r'/') == '\\'
		assert norm_path(r'\/') == '\\'
		assert norm_path(r'path\../dir\..') == '.'
		assert norm_path(r'.\.\') == '.'
		assert norm_path(r'G:.\.\dir\././\.\.\\\\///to/././\file.v/./\\') == r'G:dir\to\file.v'
		assert norm_path(r'G:\..\..\.\.\file.v\\\.') == r'G:\file.v'
		assert norm_path(r'\\Server\share\\\dir/..\file.v\./.') == r'\\Server\share\file.v'
		assert norm_path(r'\\.\device\\\dir/to/./file.v\.') == r'\\.\device\dir\to\file.v'
		assert norm_path(r'C:dir/../dir2/../../../file.v') == r'C:..\..\file.v'
		assert norm_path(r'\\.\C:\\\Users/\Documents//..') == r'\\.\C:\Users'
		assert norm_path(r'\\.\C:\Users') == r'\\.\C:\Users'
		assert norm_path(r'\\') == '\\'
		assert norm_path(r'//') == '\\'
		assert norm_path(r'\\\') == '\\'
		assert norm_path(r'.') == '.'
		assert norm_path(r'\\Server') == '\\Server'
		assert norm_path(r'\\Server\') == '\\Server'
		return
	}
	assert norm_path('/path/././../to/file//file.v/.') == '/to/file/file.v'
	assert norm_path('path/././to/files/../../file.v/.') == 'path/file.v'
	assert norm_path('path/././/../../to/file.v/.') == '../to/file.v'
	assert norm_path('/path/././/../..///.././file.v/././') == '/file.v'
	assert norm_path('path/././//../../../to/dir//.././file.v/././') == '../../to/file.v'
	assert norm_path('path/../dir/..') == '.'
	assert norm_path('../dir/..') == '..'
	assert norm_path('/../dir/..') == '/'
	assert norm_path('//././dir/../files/././/file.v') == '/files/file.v'
	assert norm_path('/\\../dir/////////.') == '/\\../dir'
	assert norm_path('/home/') == '/home'
	assert norm_path('/home/////./.') == '/home'
	assert norm_path('...') == '...'
}

fn test_abs_path() {
	wd := getwd()
	wd_w_sep := wd + path_separator
	$if windows {
		assert abs_path('path/to/file.v') == '${wd_w_sep}path\\to\\file.v'
		assert abs_path('path/to/file.v') == '${wd_w_sep}path\\to\\file.v'
		assert abs_path('/') == r'\'
		assert abs_path(r'C:\path\to\files\file.v') == r'C:\path\to\files\file.v'
		assert abs_path(r'C:\/\path\.\to\../files\file.v\.\\\.\') == r'C:\path\files\file.v'
		assert abs_path(r'\\Host\share\files\..\..\.') == r'\\Host\share\'
		assert abs_path(r'\\.\HardDiskvolume2\files\..\..\.') == r'\\.\HardDiskvolume2\'
		assert abs_path(r'\\?\share') == r'\\?\share'
		assert abs_path(r'\\.\') == r'\'
		assert abs_path(r'G:/\..\\..\.\.\file.v\\.\.\\\\') == r'G:\file.v'
		assert abs_path('files') == '${wd_w_sep}files'
		assert abs_path('') == wd
		assert abs_path('.') == wd
		assert abs_path('files/../file.v') == '${wd_w_sep}file.v'
		assert abs_path('///') == r'\'
		assert abs_path('/path/to/file.v') == r'\path\to\file.v'
		assert abs_path('D:/') == r'D:\'
		assert abs_path(r'\\.\HardiskVolume6') == r'\\.\HardiskVolume6'
		return
	}
	assert abs_path('/') == '/'
	assert abs_path('.') == wd
	assert abs_path('files') == '${wd_w_sep}files'
	assert abs_path('') == wd
	assert abs_path('files/../file.v') == '${wd_w_sep}file.v'
	assert abs_path('///') == '/'
	assert abs_path('/path/to/file.v') == '/path/to/file.v'
	assert abs_path('/path/to/file.v/../..') == '/path'
	assert abs_path('path/../file.v/..') == wd
	assert abs_path('///') == '/'
}

fn test_existing_path() {
	wd := getwd()
	$if windows {
		assert existing_path('') or { '' } == ''
		assert existing_path('..') or { '' } == '..'
		assert existing_path('.') or { '' } == '.'
		assert existing_path(wd) or { '' } == wd
		assert existing_path('\\') or { '' } == '\\'
		assert existing_path('${wd}\\.\\\\does/not/exist\\.\\') or { '' } == '${wd}\\.\\\\'
		assert existing_path('${wd}\\\\/\\.\\.\\/.') or { '' } == '${wd}\\\\/\\.\\.\\/.'
		assert existing_path('${wd}\\././/\\/oh') or { '' } == '${wd}\\././/\\/'
		return
	}
	assert existing_path('') or { '' } == ''
	assert existing_path('..') or { '' } == '..'
	assert existing_path('.') or { '' } == '.'
	assert existing_path(wd) or { '' } == wd
	assert existing_path('/') or { '' } == '/'
	assert existing_path('${wd}/does/.///not/exist///.//') or { '' } == '${wd}/'
	assert existing_path('${wd}//././/.//') or { '' } == '${wd}//././/.//'
	assert existing_path('${wd}//././/.//oh') or { '' } == '${wd}//././/.//'
}

fn test_windows_volume() {
	$if windows {
		assert windows_volume('C:/path\\to/file.v') == 'C:'
		assert windows_volume('D:\\.\\') == 'D:'
		assert windows_volume('G:') == 'G:'
		assert windows_volume('G') == ''
		assert windows_volume(r'\\Host\share\files\file.v') == r'\\Host\share'
		assert windows_volume('\\\\Host\\') == ''
		assert windows_volume(r'\\.\BootPartition2\\files\.\\') == r'\\.\BootPartition2'
		assert windows_volume(r'\/.\BootPartition2\\files\.\\') == r'\/.\BootPartition2'
		assert windows_volume(r'\\\.\BootPartition2\\files\.\\') == ''
		assert windows_volume('') == ''
		assert windows_volume('\\') == ''
		assert windows_volume('/') == ''
	}
}
