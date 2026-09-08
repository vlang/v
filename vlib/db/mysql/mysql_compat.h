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

// MYSQL_BIND uses unsigned long for buffer sizes on every connector, while
// its null indicator is my_bool on MariaDB/older MySQL and bool on MySQL 8.
// Name those native pointee types so V-owned result buffers have the exact C
// element width instead of assuming u32/_Bool.
typedef unsigned long v_mysql_ulong;
#if defined(MARIADB_BASE_VERSION) || defined(MARIADB_VERSION_ID) || \
	defined(MARIADB_PACKAGE_VERSION_ID) || defined(LIBMARIADB)
typedef my_bool v_mysql_bool;
#elif defined(MYSQL_VERSION_ID) && MYSQL_VERSION_ID >= 80000
typedef bool v_mysql_bool;
#else
typedef my_bool v_mysql_bool;
#endif
