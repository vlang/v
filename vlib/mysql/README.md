For Linux, you need to install `MySQL development` package and `pkg-config`.

For Windows, install [the installer](https://dev.mysql.com/downloads/installer/) ,
then copy the `include` and `lib` folders to `<V install directory>\thirdparty\mysql`.

Note: if you encounter weird errors (your program just exits right away, without
printing any messages, even though you have `println('hi')` statements in your 
`fn main()`), when trying to run a program that does `import mysql` on windows, you 
may need to copy the .dll file: `thirdparty/mysql/lib/libmysql.dll` , into the folder
of the executable too (it should be right next to the .exe file). 
This is a temporary workaround, until we have a more permanent solution, or at least
more user friendly errors for that situation.

## Basic Usage

```v oksyntax
import mysql

// Create connection
mut connection := mysql.Connection{
	username: 'root'
	dbname: 'mysql'
}
// Connect to server
connection.connect()?
// Change the default database
connection.select_db('db_users')?
// Do a query
get_users_query_result := connection.query('SELECT * FROM users')?
// Get the result as maps
for user in get_users_query_result.maps() {
	// Access the name of user
	println(user['name'])
}
// Free the query result
get_users_query_result.free()
// Close the connection if needed
connection.close()
```
