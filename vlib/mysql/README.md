## Requirements

Below are the things that are needed before using this module:

### Install pkg-config

For windows users, you can skip it.

#### Debian/Ubuntu

```sh
sudo apt-get install pkg-config
```

#### Arch

```sh
sudo pacman -S pkg-config
```

### Install MySQL development package

#### Debian/Ubuntu

```sh
sudo apt-get install libmysqlclient-dev
```

#### Arch

```sh
sudo pacman -S libmysqlclient
```

#### Windows

Please install [the installer](https://dev.mysql.com/downloads/installer/) , then copy the `include` and `lib` folders to `thirdparty/mysql` on V root folder.

## Basic Usage

```v
// Create connection
mut connection := mysql.Connection{
  username: 'root'
  dbname: 'mysql'
}
// Connect to server
connection.connect() ?
// Change the default database
connection.select_db('users') ?
// Do a query
get_users_query_result := connection.query("SELECT * FROM rayon") ?

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
