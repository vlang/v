// Unit tests for `http.safe_filename`. No network involved.
import net.http

fn test_safe_filename_plain_name() ! {
	assert http.safe_filename('photo.jpg')! == 'photo.jpg'
	assert http.safe_filename('...')! == '...'
}

fn test_safe_filename_strips_directories() ! {
	assert http.safe_filename('a/b/c.png')! == 'c.png'
	assert http.safe_filename('C:\\Users\\x\\a.txt')! == 'a.txt'
	assert http.safe_filename('../x')! == 'x'
	assert http.safe_filename('..\\x')! == 'x'
}

fn test_safe_filename_rejects_traversal_and_empty() {
	for bad in ['', '.', '..', 'dir/', '///', 'a/b/..'] {
		http.safe_filename(bad) or {
			assert err.msg() == 'unsafe file name: ${bad}'
			continue
		}
		assert false, 'expected unsafe file name error for `${bad}`'
	}
}

fn test_safe_filename_rejects_nul() {
	bad := 'a' + u8(0).ascii_str() + 'b'
	http.safe_filename(bad) or {
		assert err.msg().starts_with('unsafe file name: ')
		return
	}
	assert false, 'expected unsafe file name error for NUL byte'
}
