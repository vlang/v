module mysql

// MySQL refresh flags.
// Docs: https://dev.mysql.com/doc/c-api/8.0/en/mysql-refresh.html
// /usr/include/mysql/mysql_com.h
pub const refresh_grant = u32(0x1)
pub const refresh_log = u32(0x2)
pub const refresh_tables = u32(0x4)
pub const refresh_hosts = u32(0x8)
pub const refresh_status = u32(0x10)
pub const refresh_threads = u32(0x20)
pub const refresh_replica = u32(0x40)
pub const refresh_slave = u32(0x40) // same as refresh_replica
pub const refresh_master = u32(0x80)
pub const refresh_error_log = u32(0x100)
pub const refresh_engine_log = u32(0x200)
pub const refresh_binary_log = u32(0x400)
pub const refresh_relay_log = u32(0x800)
pub const refresh_general_log = u32(0x1000)
pub const refresh_slow_log = u32(0x2000)
pub const refresh_read_lock = u32(0x4000)
pub const refresh_fast = u32(0x8000)
pub const refresh_user_resources = u32(0x80000)
pub const refresh_for_export = u32(0x100000)
pub const refresh_optimizer_costs = u32(0x200000)
pub const refresh_persist = u32(0x400000)
