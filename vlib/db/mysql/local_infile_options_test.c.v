// vtest build: started_mysqld?
module mysql

fn test_local_infile_connect_options() {
	mut db := DB{
		conn: C.mysql_init(0)
	}
	assert !isnil(db.conn)
	defer {
		C.mysql_close(db.conn)
	}

	flags := db.apply_local_infile(Config{
		flag: .client_found_rows
		local_infile: true
	})
	assert flags.has(.client_found_rows)
	assert flags.has(.client_local_files)

	mut enabled := u32(0)
	assert C.mysql_get_option(db.conn, C.MYSQL_OPT_LOCAL_INFILE, &enabled) == 0
	assert enabled == 1
}
