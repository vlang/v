module mssql

pub struct Config {
pub:
	driver string
	server string
	uid    string
	pwd    string
	// if dbname empty, conn str will not contain Database info,
	// and it is up to the server to choose which db to connect to.
	dbname string
}

pub fn (cfg Config) get_conn_str() string {
	mut str := 'Driver=${cfg.driver};Server=${cfg.server};UID=${cfg.uid};PWD=${cfg.pwd}'
	if cfg.dbname != '' {
		str += ';Database=${cfg.dbname}'
	}
	return str
}
