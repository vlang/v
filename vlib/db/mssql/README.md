# SQL Server ODBC

* This module wraps the ODBC C API for use with SQL Server.
* It can also be used with any ODBC data source by passing a raw ODBC connection string.

## Scope
* `db.mssql` can connect to any ODBC data source when `mssql.open(...)` or
  `Connection.connect(...)` receives a valid ODBC connection string.
* `Config` can build connection strings from `driver`, `server`, `port`, `uid`/`user`,
  `pwd`/`password`, `dbname`, `dsn`, and extra ODBC options.
* For non-MSSQL ODBC sources, pass a DSN or raw ODBC string directly.

## Dependencies
* ODBC driver manager development headers/libraries (`sql.h`, `sqlext.h`).
  * Linux:
    * Install unixODBC development packages (`unixodbc`, `unixodbc-dev`, or distro equivalent).
  * macOS:
    * Recommended: install unixODBC + pkg-config:
      `brew install unixodbc pkg-config`
    * Then install your DB vendor ODBC driver (for SQL Server: `msodbcsql18`).
    * Details: see the Microsoft SQL Server ODBC driver installation guide.
  * Windows:
    * `odbc32` is included with the Windows SDK on most systems.
    * Details:
      https://learn.microsoft.com/en-us/sql/connect/odbc/microsoft-odbc-driver-for-sql-server

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
copy ^
  "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sqlext.h" ^
  thirdparty\mssql\include
copy ^
  "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sqltypes.h" ^
  thirdparty\mssql\include
copy ^
  "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\um\sqlucode.h" ^
  thirdparty\mssql\include
copy ^
  "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\shared\sal.h" ^
  thirdparty\mssql\include
copy ^
  "C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0\shared\concurrencysal.h" ^
  thirdparty\mssql\include
```
* dlls can be automatically resolved by `tcc`

## TODO
* Support ORM

## Usage
```v ignore
import db.mssql

fn test_example() ? {
	// connect to server
	mut conn := mssql.connect(
		driver:   'ODBC Driver 18 for SQL Server'
		server:   '127.0.0.1'
		port:     1433
		user:     '<your username>'
		password: '<your password>'
		dbname:   'master'
		options: {
			'Encrypt':                'yes'
			'TrustServerCertificate': 'yes'
		}
	)?

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

You can also connect with a raw DSN or ODBC string:

```v ignore
mut conn := mssql.open('DSN=Reporting;Trusted_Connection=Yes')?
```
