module mysql

// Need to check if mysqlclient is not there and use mariadb as alternative because newer system doesn't support mysql 8.0 as default

$if $pkgconfig('mysqlclient') {
	#pkgconfig mysqlclient
} $else {
	#pkgconfig mariadb
}

#include <mysql/mysql.h> # Please install the mysqlclient development headers
