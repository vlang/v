module os

// The IError returned by the os.* functions carries a message produced by the
// platform's strerror()/FormatMessage(), so the message text differs between
// systems and must not be matched. Use the predicates below instead:
//
//	os.stat(path) or {
//		if os.is_not_exist(err) { ... }
//	}
//
// On POSIX systems, the os.* functions report a C errno value. On Windows, the
// functions built on the C runtime (for example `stat`, `rm`, `read_file`,
// `open`, `create`, `write_file`, `chdir`, `truncate` and `rename`) report a C
// errno value too, while the ones built on the Win32 API (`mkdir`, `rmdir`,
// `ls`, `symlink` and `link`) report a Win32 error code. The same condition can
// therefore have different codes on Windows, so prefer the predicates, which
// accept all of them, over comparing err.code() with the constants below.

// error_code_noent is the code for a path that does not exist. It is ENOENT on
// POSIX and ERROR_FILE_NOT_FOUND on Windows, which have the same value there.
// Win32 functions report ERROR_PATH_NOT_FOUND instead for a missing directory
// earlier in the path, which is_not_exist also accepts.
pub const error_code_noent = $if windows { 2 } $else { int(C.ENOENT) }
// error_code_denied is the code for a permission failure. It is EACCES on
// POSIX and ERROR_ACCESS_DENIED on Windows.
pub const error_code_denied = $if windows { 5 } $else { int(C.EACCES) }
// error_code_perm is the code for an operation the caller is not allowed to
// perform. It is EPERM on POSIX. Windows has no separate code for it and
// uses ERROR_ACCESS_DENIED, so it is equal to error_code_denied there.
pub const error_code_perm = $if windows { 5 } $else { int(C.EPERM) }
// error_code_exist is the code for a path that already exists. It is EEXIST
// on POSIX and ERROR_ALREADY_EXISTS on Windows.
pub const error_code_exist = $if windows { 183 } $else { int(C.EEXIST) }
// error_code_notdir is the code for a directory operation that targeted a
// file. It is ENOTDIR on POSIX and ERROR_DIRECTORY on Windows.
pub const error_code_notdir = $if windows { 267 } $else { int(C.ENOTDIR) }

// is_not_exist reports whether err was caused by a path that does not exist.
pub fn is_not_exist(err IError) bool {
	code := err.code()
	$if windows {
		// 2: ENOENT (C runtime) == ERROR_FILE_NOT_FOUND; 3: ERROR_PATH_NOT_FOUND
		return code == 2 || code == 3
	} $else {
		return code == error_code_noent
	}
}

// is_exist reports whether err was caused by a path that already exists.
pub fn is_exist(err IError) bool {
	code := err.code()
	$if windows {
		// 17: EEXIST (C runtime); 80: ERROR_FILE_EXISTS; 183: ERROR_ALREADY_EXISTS
		return code == 17 || code == 80 || code == 183
	} $else {
		return code == error_code_exist
	}
}

// is_permission_denied reports whether err was caused by a permission failure.
// It accepts both EACCES and EPERM.
pub fn is_permission_denied(err IError) bool {
	code := err.code()
	$if windows {
		// 1: EPERM, 13: EACCES (C runtime); 5: ERROR_ACCESS_DENIED
		return code == 1 || code == 5 || code == 13
	} $else {
		return code == error_code_denied || code == error_code_perm
	}
}
