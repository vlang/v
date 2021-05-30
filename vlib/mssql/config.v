module mssql

pub struct Config {
pub:
	driver     	string
	server     	string
	uid     	string
	pwd 		string
}

pub fn (cfg Config) get_conn_str() string {
	return "Driver=${cfg.driver};Server=${cfg.server};UID=${cfg.uid};PWD=${cfg.pwd}"
}