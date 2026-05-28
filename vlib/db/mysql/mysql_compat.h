// MYSQL_OPT_SSL_MODE was added in MySQL 5.7. MariaDB's server and connector
// headers expose their own TLS options instead, so the generated C code would
// fail to compile when it references the MySQL-only enum value. Because enum
// values are not preprocessor macros, `#ifndef MYSQL_OPT_SSL_MODE` alone
// cannot distinguish a MySQL header that has the enum from a MariaDB header
// that does not. Gate on MariaDB's published macros and supply a sentinel that
// mysql_options() will reject at runtime if Config.ssl_mode is used there.
#if !defined(MYSQL_OPT_SSL_MODE) && \
	(defined(MARIADB_BASE_VERSION) || defined(MARIADB_VERSION_ID) || \
		defined(MARIADB_PACKAGE_VERSION_ID) || defined(LIBMARIADB))
#define MYSQL_OPT_SSL_MODE 9999
#endif
