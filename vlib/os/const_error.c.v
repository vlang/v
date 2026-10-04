module os

// The IError returned by the os.* functions carries a message produced by the
// platform's strerror()/FormatMessage(), so the message text differs between
// systems and must not be matched. Match the code instead:
//
//	if err.code() == os.error_code_noent { ... }
//
// or use the predicates below, which also accept the other codes a platform may
// report for the same condition.
//
// The POSIX values were checked against Linux, macOS, the BSDs, Solaris and AIX.
// Conditions whose errno differs between those systems (ELOOP, ENAMETOOLONG,
// ENOTEMPTY) are deliberately absent, because one portable constant for them
// would be wrong on some supported platform.

// error_code_noent is the code for a path that does not exist. It is ENOENT on
// POSIX and ERROR_FILE_NOT_FOUND on Windows, which have the same value. Windows
// reports it both for a missing final component and for a missing or
// non-directory component earlier in the path.
pub const error_code_noent = 2

// error_code_denied is the code for a permission failure. It is EACCES on
// POSIX and ERROR_ACCESS_DENIED on Windows.
pub const error_code_denied = $if windows { 5 }
// error_code_perm is the code for an operation the caller is not allowed to
// perform. It is EPERM on POSIX. Windows has no separate code for it and
// uses ERROR_ACCESS_DENIED, so it is equal to error_code_denied there.
pub const error_code_perm = $if windows { 5 }
// error_code_exist is the code for a path that already exists. It is EEXIST
// on POSIX and ERROR_ALREADY_EXISTS on Windows.
pub const error_code_exist = $if windows { 183 }
// error_code_isdir is the code for a directory operation that targeted a
// file. It is EISDIR on POSIX and ERROR_DIRECTORY on Windows.
pub const error_code_isdir = $if windows { 267 }
// error_code_invalid is the code for an invalid path or argument. It is
// EINVAL on POSIX and ERROR_INVALID_NAME on Windows.
pub const error_code_invalid = $if windows { 123 }
// error_code_read_only is the code for a write to a read-only filesystem.
// It is EROFS on POSIX and ERROR_WRITE_PROTECT on Windows.
pub const error_code_read_only = $if windows { 19 }
// error_code_no_space is the code for a full device. It is ENOSPC on POSIX
// and ERROR_DISK_FULL on Windows.
pub const error_code_no_space = $if windows { 112 }
// error_code_too_many_open is the code for exhausting the per-process file
// descriptor table. It is EMFILE on POSIX and ERROR_TOO_MANY_OPEN_FILES on
// Windows.
pub const error_code_too_many_open = $if windows { 4 }

// is_not_exist reports whether err was caused by a path that does not exist.
pub fn is_not_exist(err IError) bool {
	return err.code() == error_code_noent
}

// is_exist reports whether err was caused by a path that already exists.
pub fn is_exist(err IError) bool {
	return err.code() == error_code_exist
}

// is_permission_denied reports whether err was caused by a permission failure.
// It accepts EACCES and EPERM, which Windows reports as a single code.
pub fn is_permission_denied(err IError) bool {
	return err.code() == error_code_denied || err.code() == error_code_perm
}
