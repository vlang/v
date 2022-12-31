module mysql

// MYSQL REFRESH FLAGS
pub const (
	refresh_grant   = u32(C.REFRESH_GRANT)
	refresh_log     = u32(C.REFRESH_LOG)
	refresh_tables  = u32(C.REFRESH_TABLES)
	refresh_hosts   = u32(C.REFRESH_HOSTS)
	refresh_status  = u32(C.REFRESH_STATUS)
	refresh_threads = u32(C.REFRESH_THREADS)
	refresh_slave   = u32(C.REFRESH_SLAVE)
	refresh_master  = u32(C.REFRESH_MASTER)
)
