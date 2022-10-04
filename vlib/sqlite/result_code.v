module sqlite

// Result represents Sqlite Result and Error Codes
// see https://www.sqlite.org/rescode.html
pub enum Result {
	ok = 0
	error = 1
	internal = 2
	perm = 3
	abort = 4
	busy = 5
	locked = 6
	nomem = 7
	readonly = 8
	interrupt = 9
	ioerr = 10
	corrupt = 11
	notfound = 12
	full = 13
	cantopen = 14
	protocol = 15
	empty = 16
	schema = 17
	toobig = 18
	constraint = 19
	mismatch = 20
	misuse = 21
	nolfs = 22
	auth = 23
	format = 24
	range = 25
	notadb = 26
	notice = 27
	warning = 28
	row = 100
	done = 101
	ok_load_permanently = 256
	error_missing_collseq = 257
	busy_recovery = 261
	locked_sharedcache = 262
	readonly_recovery = 264
	ioerr_read = 266
	corrupt_vtab = 267
	cantopen_notempdir = 270
	constraint_check = 275
	notice_recover_wal = 283
	warning_autoindex = 284
	error_retry = 513
	abort_rollback = 516
	busy_snapshot = 517
	locked_vtab = 518
	readonly_cantlock = 520
	ioerr_short_read = 522
	corrupt_sequence = 523
	cantopen_isdir = 526
	constraint_commithook = 531
	notice_recover_rollback = 539
	error_snapshot = 769
	busy_timeout = 773
	readonly_rollback = 776
	ioerr_write = 778
	corrupt_index = 779
	cantopen_fullpath = 782
	constraint_foreignkey = 787
	readonly_dbmoved = 1032
	ioerr_fsync = 1034
	cantopen_convpath = 1038
	constraint_function = 1043
	readonly_cantinit = 1288
	ioerr_dir_fsync = 1290
	cantopen_dirtywal = 1294
	constraint_notnull = 1299
	readonly_directory = 1544
	ioerr_truncate = 1546
	cantopen_symlink = 1550
	constraint_primarykey = 1555
	ioerr_fstat = 1802
	constraint_trigger = 1811
	ioerr_unlock = 2058
	constraint_unique = 2067
	ioerr_rdlock = 2314
	constraint_vtab = 2323
	ioerr_delete = 2570
	constraint_rowid = 2579
	ioerr_blocked = 2826
	constraint_pinned = 2835
	ioerr_nomem = 3082
	ioerr_access = 3338
	ioerr_checkreservedlock = 3594
	ioerr_lock = 3850
	ioerr_close = 4106
	ioerr_dir_close = 4362
	ioerr_shmopen = 4618
	ioerr_shmsize = 4874
	ioerr_shmlock = 5130
	ioerr_shmmap = 5386
	ioerr_seek = 5642
	ioerr_delete_noent = 5898
	ioerr_mmap = 6154
	ioerr_gettemppath = 6410
	ioerr_convpath = 6666
	ioerr_vnode = 6922
	ioerr_auth = 7178
	ioerr_begin_atomic = 7434
	ioerr_commit_atomic = 7690
	ioerr_rollback_atomic = 7946
	ioerr_data = 8202
}

// is_error checks if it is an error code.
pub fn (r Result) is_error() bool {
	return r !in [.ok, .row, .done]
}

// is_error checks if `code` is an error code.
pub fn is_error(code int) bool {
	return unsafe { Result(code).is_error() }
}
