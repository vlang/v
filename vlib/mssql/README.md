# SQL Server ODBC

* This is a V wrapper of SQL Server ODBC C/C++ library

## Dependencies
* ODBC C/C++ library
    * Linux Install: https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server
        * `msodbcsql17` and `unixodbc-dev` packages are needed
    * Windows Install:
		* `odbc` lib is included in windows sdk for most of distributions, so there is no need to install it separately
		* Details: https://docs.microsoft.com/en-us/sql/connect/odbc/microsoft-odbc-driver-for-sql-server

# Windows Notes
* Only `msvc` compiler is supported on windows.
	* It is possible to support `tcc`, but it is of low priority.
* Make sure `cl.exe` of `msvc` is accessible from command line. You can run `v` commands in `Visual Studio 2019 Developer Command Prompt` to be safe.

## TODO
* Support Mac 
* Support ORM

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
