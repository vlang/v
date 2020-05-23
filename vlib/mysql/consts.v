module mysql

/* MYSQL CONNECT FLAGS */
pub const (
	// CAN_HANDLE_EXPIRED_PASSWORDS       = C.CAN_HANDLE_EXPIRED_PASSWORDS
	client_compress                    = C.CLIENT_COMPRESS
	client_found_rows                  = C.CLIENT_FOUND_ROWS
	client_ignore_sigpipe              = C.CLIENT_IGNORE_SIGPIPE
	client_ignore_space                = C.CLIENT_IGNORE_SPACE
	client_interactive                 = C.CLIENT_INTERACTIVE
	client_local_files                 = C.CLIENT_LOCAL_FILES
	client_multi_results               = C.CLIENT_MULTI_RESULTS
	client_multi_statements            = C.CLIENT_MULTI_STATEMENTS
	client_no_schema                   = C.CLIENT_NO_SCHEMA
	client_odbc                        = C.CLIENT_ODBC
	// client_optional_resultset_metadata = C.CLIENT_OPTIONAL_RESULTSET_METADATA
	client_ssl                         = C.CLIENT_SSL
	client_remember_options            = C.CLIENT_REMEMBER_OPTIONS
)

/* MYSQL REFRESH FLAGS */
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
