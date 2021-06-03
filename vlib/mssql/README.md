# SQL Server ODBC

* This is a V wrapper of SQL Server ODBC C/C++ library

## Dependencies
* ODBC C/C++ library
    * Linux Install: https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server
        * `msodbcsql17` and `unixodbc-dev` packages needed
    * Windows Install: https://docs.microsoft.com/en-us/sql/connect/odbc/microsoft-odbc-driver-for-sql-server

# Windows Notes
* Only `msvc` compiler works. `tcc` errors out in some windows system headers.
* Make sure `cl.exe` of `msvc` is accessible from command line. You can run `v` commands in `Visual Studio 2019 Developer Command Prompt` to be safe.

## TODO
* Support Mac 
* ORM

## Usage
```v ignore
import mssql

fn test_example() ? {
	// connect to server
	config := mssql.Config{
		driver: 'ODBC Driver 17 for SQL Server'
		server: 'tcp:localhost'
		uid: '<your username>'
		pwd: '<your password>'
	}

	mut conn := mssql.Connection{}

	conn.connect(config.get_conn_str()) ?

	defer {
		conn.close()
	}

	// get current db name
	mut query := 'SELECT DB_NAME()'
	mut res := conn.query(query) ?
	assert res == mssql.Result{
		rows: [mssql.Row{
			vals: ['master']
		}]
		num_rows_affected: -1
	}
}
```
