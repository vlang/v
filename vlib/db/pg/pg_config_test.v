// vtest build: !windows && !musl?
module pg

fn test_ssl_mode_conninfo_values() {
	assert SslMode.unset.conninfo_value() == ''
	assert SslMode.disable.conninfo_value() == 'disable'
	assert SslMode.allow.conninfo_value() == 'allow'
	assert SslMode.prefer.conninfo_value() == 'prefer'
	assert SslMode.require.conninfo_value() == 'require'
	assert SslMode.verify_ca.conninfo_value() == 'verify-ca'
	assert SslMode.verify_full.conninfo_value() == 'verify-full'
}

fn test_conninfo_includes_ssl_options() {
	conninfo := Config{
		host:     'db.example.com'
		port:     15432
		user:     'app user'
		password: 'secret'
		dbname:   'prod'
		ssl_mode: .verify_full
		ssl_ca:   '/etc/ssl/root bundle.pem'
		ssl_cert: '/etc/ssl/client.pem'
		ssl_key:  '/etc/ssl/client.key'
		ssl_crl:  '/etc/ssl/root.crl'
	}.conninfo()!

	assert conninfo == "host=db.example.com port=15432 user='app user' dbname=prod password=secret sslmode=verify-full sslcert=/etc/ssl/client.pem sslkey=/etc/ssl/client.key sslrootcert='/etc/ssl/root bundle.pem' sslcrl=/etc/ssl/root.crl"
}
