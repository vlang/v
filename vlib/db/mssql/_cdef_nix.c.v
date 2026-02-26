module mssql

$if $pkgconfig('odbc') {
	#pkgconfig --cflags --libs odbc
} $else {
	#flag -lodbc
	// Common fallback paths when unixODBC is installed via package managers on macOS.
	#flag darwin -I/opt/homebrew/opt/unixodbc/include
	#flag darwin -L/opt/homebrew/opt/unixodbc/lib
	#flag darwin -I/usr/local/opt/unixodbc/include
	#flag darwin -L/usr/local/opt/unixodbc/lib
	#flag darwin -I/opt/local/include
	#flag darwin -L/opt/local/lib
}

#include <sql.h>
#include <sqlext.h>
