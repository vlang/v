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

$if tinyc {
	// tcc does not search system include paths by default; add /usr/include so
	// that it can find the unixODBC headers (sql.h, sqlext.h) on Linux.
	#flag linux -I/usr/include
}

#include <sql.h>
#include <sqlext.h>
