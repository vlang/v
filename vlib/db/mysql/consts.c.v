module mysql

// MySQL refresh flags.
// Docs: https://dev.mysql.com/doc/c-api/8.0/en/mysql-refresh.html
pub const refresh_grant = u32(C.REFRESH_GRANT)
pub const refresh_log = u32(C.REFRESH_LOG)
pub const refresh_tables = u32(C.REFRESH_TABLES)
pub const refresh_hosts = u32(C.REFRESH_HOSTS)
pub const refresh_status = u32(C.REFRESH_STATUS)
pub const refresh_threads = u32(C.REFRESH_THREADS)
pub const refresh_slave = u32(C.REFRESH_SLAVE)
pub const refresh_master = u32(C.REFRESH_MASTER)
