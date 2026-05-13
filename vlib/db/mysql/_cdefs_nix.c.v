module mysql

// Need to check if `mysqlclient` is not there and use `mariadb` as alternative
// because newer system doesn't support mysql 8.0 as default.

$if $pkgconfig('mysqlclient') {
	#pkgconfig mysqlclient
	#include <mysql.h> # Please install the libmysqlclient-dev development headers
} $else $if $pkgconfig('mariadb') {
	#pkgconfig mariadb
	$if openbsd {
		#include <mysql.h> # Please install the mariadb-client package for development headers
	} $else $if freebsd {
		#include <mysql.h> # Please install the mariadb118-client package for development headers
	} $else {
		#include <mysql.h> # Please install the libmariadb-dev development headers
	}
} $else $if $pkgconfig('libmariadb') {
	#pkgconfig libmariadb
	#include <mysql.h> # Please install the mariadb client
} $else {
	// Common fallback paths when pkg-config is unavailable.
	#flag linux -I$when_first_existing('/usr/include/mysql','/usr/include/mariadb','/usr/local/include/mysql','/usr/local/include/mariadb')
	#flag linux $when_first_existing('/usr/lib64/libmysqlclient.so','/usr/lib64/mysql/libmysqlclient.so','/usr/lib/libmysqlclient.so','/usr/lib/mysql/libmysqlclient.so','/usr/lib/x86_64-linux-gnu/libmysqlclient.so','/usr/lib/aarch64-linux-gnu/libmysqlclient.so','/usr/local/lib/libmysqlclient.so','/usr/local/mysql/lib/libmysqlclient.so','/usr/lib64/libmariadb.so','/usr/lib64/mysql/libmariadb.so','/usr/lib/libmariadb.so','/usr/lib/mysql/libmariadb.so','/usr/lib/x86_64-linux-gnu/libmariadb.so','/usr/lib/aarch64-linux-gnu/libmariadb.so','/usr/local/lib/libmariadb.so')
	#flag darwin -I$when_first_existing('/opt/homebrew/include/mysql','/opt/homebrew/include/mariadb','/opt/homebrew/opt/mysql-client/include/mysql','/opt/homebrew/opt/mariadb-connector-c/include/mariadb','/usr/local/include/mysql','/usr/local/include/mariadb','/usr/local/opt/mysql-client/include/mysql','/usr/local/opt/mariadb-connector-c/include/mariadb')
	#flag darwin $when_first_existing('/opt/homebrew/lib/libmysqlclient.dylib','/opt/homebrew/opt/mysql-client/lib/libmysqlclient.dylib','/usr/local/lib/libmysqlclient.dylib','/usr/local/opt/mysql-client/lib/libmysqlclient.dylib','/opt/homebrew/lib/libmariadb.dylib','/opt/homebrew/opt/mariadb-connector-c/lib/libmariadb.dylib','/usr/local/lib/libmariadb.dylib','/usr/local/opt/mariadb-connector-c/lib/libmariadb.dylib')
	#flag freebsd -I$when_first_existing('/usr/local/include/mysql','/usr/local/include/mariadb')
	#flag freebsd $when_first_existing('/usr/local/lib/libmysqlclient.so','/usr/local/lib/libmariadb.so')
	#flag openbsd -I$when_first_existing('/usr/local/include/mysql','/usr/local/include/mariadb')
	#flag openbsd $when_first_existing('/usr/local/lib/libmysqlclient.so','/usr/local/lib/libmariadb.so')
	#include <mysql.h> # Please install the mysql or mariadb development headers
}
