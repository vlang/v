module mysql

// Need to check if `mysqlclient` is not there and use `mariadb` as alternative
// because newer system doesn't support mysql 8.0 as default.

$if $pkgconfig('mysqlclient') {
	#pkgconfig mysqlclient
	#include <mysql.h> # Please install the libmysqlclient-dev development headers
} $else $if $pkgconfig('mariadb') {
	#pkgconfig mariadb
	#include <mysql.h> # Please install the libmariadb-dev development headers
} $else $if $pkgconfig('libmariadb') {
	#pkgconfig libmariadb
	#include <mysql.h> # Please install the mariadb client
} $else {
	#include <mysql.h> # Please install the mysql headers
}
