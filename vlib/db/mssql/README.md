# SQL Server ODBC

* This is a V wrapper of SQL Server ODBC C/C++ library

## Dependencies
* ODBC C/C++ library
    * Linux Install: 
		* Details: https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server
        * `msodbcsql17` and `unixodbc-dev` packages are needed
    * Windows Install:
		* `odbc` lib is included in windows sdk for most of distributions,
			so there is no need to install it separately
		* Details: https://docs.microsoft.com/en-us/sql/connect/odbc/microsoft-odbc-driver-for-sql-server

## Windows Notes
### Using `msvc`
* Make sure `cl.exe` of `msvc` is accessible from command line. 
You can run `v` commands in `Visual Studio 2019 Developer Command Prompt` to be safe.
* C Headers and dlls can be automatically resolved by `msvc`.
### Using `tcc` 
* Copy those headers to `@VEXEROOT\thirdparty\mssql\include`. 
The version number `10.0.18362.0` might differ on your system. 
Command Prompt commands:
```cmd
copy "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sql.h" thirdparty\mssql\include
copy "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sqlext.h" thirdparty\mssql\include
copy "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sqltypes.h" thirdparty\mssql\include
copy "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sqlucode.h" thirdparty\mssql\include
copy "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\shared\sal.h" thirdparty\mssql\include
copy "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\shared\concurrencysal.h" thirdparty\mssql\include
```
* dlls can be automatically resolved by `tcc`

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

	conn.connect(config.get_conn_str())?

	defer {
		conn.close()
	}

	// get current db name
	mut query := 'SELECT DB_NAME()'
	mut res := conn.query(query)?
	assert res == mssql.Result{
		rows: [mssql.Row{
			vals: ['master']
		}]
		num_rows_affected: -1
	}
}
```
