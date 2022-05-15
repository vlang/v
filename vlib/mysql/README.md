For Linux, you need to install `MySQL development` package and `pkg-config`.
For Windows, install [the installer](https://dev.mysql.com/downloads/installer/) ,
then copy the `include` and `lib` folders to `<V install directory>\thirdparty\mysql`.

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
