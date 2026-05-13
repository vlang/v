module mssql

@[params]
pub struct Config {
pub:
	conn_str string
	dsn      string
	driver   string
	server   string
	port     int
	uid      string
	user     string
	pwd      string
	password string
	// if dbname empty, conn str will not contain Database info,
	// and it is up to the server to choose which db to connect to.
	dbname  string
	options map[string]string
}

// connect creates a new connection using the provided configuration.
pub fn connect(config Config) !Connection {
	return open(config.get_conn_str())
}

// open creates a new connection using a raw ODBC connection string.
pub fn open(conn_str string) !Connection {
	mut conn := Connection{}
	conn.connect(conn_str)!
	return conn
}

fn preferred_value(primary string, fallback string) string {
	if primary != '' {
		return primary
	}
	return fallback
}

fn needs_odbc_braces(value string) bool {
	return value.contains_any(' \t\r\n;{}')
}

fn format_odbc_value(value string) string {
	if value == '' {
		return ''
	}
	if needs_odbc_braces(value) {
		return '{' + value.replace('}', '}}') + '}'
	}
	return value
}

fn append_conn_part(mut parts []string, key string, value string) {
	if value == '' {
		return
	}
	parts << '${key}=${format_odbc_value(value)}'
}

// get_conn_str builds an ODBC connection string from the configured fields.
pub fn (cfg Config) get_conn_str() string {
	if cfg.conn_str != '' {
		return cfg.conn_str
	}
	mut parts := []string{}
	append_conn_part(mut parts, 'DSN', cfg.dsn)
	append_conn_part(mut parts, 'Driver', cfg.driver)
	append_conn_part(mut parts, 'Server', cfg.server)
	if cfg.port > 0 {
		parts << 'Port=${cfg.port}'
	}
	append_conn_part(mut parts, 'UID', preferred_value(cfg.uid, cfg.user))
	append_conn_part(mut parts, 'PWD', preferred_value(cfg.pwd, cfg.password))
	if cfg.dbname != '' {
		append_conn_part(mut parts, 'Database', cfg.dbname)
	}
	if cfg.options.len > 0 {
		mut option_keys := cfg.options.keys()
		option_keys.sort()
		for key in option_keys {
			append_conn_part(mut parts, key, cfg.options[key])
		}
	}
	return parts.join(';')
}
