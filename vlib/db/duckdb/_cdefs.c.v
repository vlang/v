module duckdb

// Link flags for the DuckDB C library.
//
// Windows: set LIBDUCKDB_DIR to the directory that contains duckdb.h, duckdb.lib
// and duckdb.dll (for example a duckdb-shared-libs-windows-amd64 release).
// Add the same directory to PATH when running programs so duckdb.dll is found.
//
// Linux/macOS: `pkg-config duckdb` is preferred. If it is not available, the
// header and library are searched in the usual system locations; set
// LIBDUCKDB_DIR to add a custom prefix.
$if windows {
	#flag windows -I$env('LIBDUCKDB_DIR') -L$env('LIBDUCKDB_DIR') -lduckdb
} $else $if $pkgconfig('duckdb') {
	#pkgconfig --cflags --libs duckdb
} $else {
	$if $env('LIBDUCKDB_DIR') != '' {
		#flag -I$env('LIBDUCKDB_DIR') -L$env('LIBDUCKDB_DIR')
	}
	#flag -lduckdb
	#flag linux -I/usr/local/include -L/usr/local/lib
	#flag darwin -I/opt/homebrew/include -L/opt/homebrew/lib
	#flag darwin -I/usr/local/include -L/usr/local/lib
}
