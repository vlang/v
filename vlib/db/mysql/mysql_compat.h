// MYSQL_OPT_SSL_MODE was added in MySQL 5.7 and in MariaDB Connector/C 3.4.
// Older mariadb-connector-c releases (still shipped by some BSDs and Linux
// distributions) declare it as an enum value that is missing entirely,
// which would cause the generated C code to fail to compile. Because the
// symbol is an enum constant rather than a preprocessor macro, a plain
// `#ifndef MYSQL_OPT_SSL_MODE` cannot detect it. Instead, gate on the
// library's published version macros and supply a sentinel that
// mysql_options() will reject at runtime.
#if defined(MARIADB_PACKAGE_VERSION_ID) && MARIADB_PACKAGE_VERSION_ID < 30400
#define MYSQL_OPT_SSL_MODE 9999
#endif
