module oracle

// Config describes how to reach an Oracle Database instance.
//
// Either set `dsn` to an Easy Connect string (for example
// `//db.example.com:1521/ORCLPDB1`) or set `host`, `port` and `service`.
@[params]
pub struct Config {
pub:
	// dsn is an Oracle Easy Connect string or a tnsnames.ora alias. When set,
	// it takes precedence over host/port/service.
	dsn     string
	host    string
	port    int = 1521
	service string
	sid     string
	// user / username are synonyms; user wins when both are set.
	user     string
	username string
	// password / pass are synonyms; password wins when both are set.
	password string
	pass     string
}

// connect opens a new connection using the provided configuration.
pub fn connect(config Config) !Connection {
	return open(config)
}

// open opens a new connection to an Oracle Database.
pub fn open(config Config) !Connection {
	mut conn := Connection{}
	conn.connect(config)!
	return conn
}

fn preferred(primary string, fallback string) string {
	if primary != '' {
		return primary
	}
	return fallback
}

// dsn_string builds the Easy Connect string from the configuration.
pub fn (cfg Config) dsn_string() string {
	if cfg.dsn != '' {
		return cfg.dsn
	}
	if cfg.host == '' {
		return ''
	}
	port := if cfg.port > 0 { cfg.port } else { 1521 }
	name := if cfg.service != '' { cfg.service } else { cfg.sid }
	if name == '' {
		return '//${cfg.host}:${port}'
	}
	return '//${cfg.host}:${port}/${name}'
}
