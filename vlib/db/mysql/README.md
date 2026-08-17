## Purpose

The db.mysql module can be used to develop software that connects to the popular open source
MySQL or MariaDB database servers.

### Local setup of a development server

To run the mysql module tests, or if you want to just experiment, you can use the following
command to start a development version of MySQL using docker:
```sh
docker run -p 3306:3306 --name some-mysql -e MYSQL_ROOT_PASSWORD=12345678 -d mysql:latest
```
The above command will start a server instance with the root password `12345678`,
available to mysql client connections, on tcp port 3306.

You can test that it works by doing: `mysql -uroot -p12345678 -h127.0.0.1` .
You should see a mysql shell (use `exit` to end the mysql client session).

Use `docker container stop some-mysql` to stop the server.

Use `docker container rm some-mysql` to remove it completely, after it is stopped.

### Installation of development dependencies

For Linux, you need to install `MySQL development` package and `pkg-config`.

For FreeBSD, you need to install the `mariadb118-client` package.

For OpenBSD, you need to install the `mariadb-client` package.

For Windows, install [the installer](https://dev.mysql.com/downloads/installer/) or extract the
ZIP package, then copy the `include`, `lib`, and `bin` folders to
`<V install directory>\thirdparty\mysql`.

### Troubleshooting

If a program that imports `db.mysql` exits right away on Windows before
`fn main()` prints anything, Windows usually could not load `libmysql.dll`.
Make sure that `thirdparty/mysql/bin` and `thirdparty/mysql/lib` are on `PATH`.
If you still need a workaround, copy `libmysql.dll` next to the produced `.exe`.

One common sign of this problem is the process exit code `-1073741515`
(`0xC0000135`).

## Basic Usage

```v oksyntax
import db.mysql

// Create connection
config := mysql.Config{
	host:     '127.0.0.1'
	port:     3306
	username: 'root'
	password: '12345678'
	dbname:   'mysql'
}

// Connect to server
mut db := mysql.connect(config)!
// Do a query
res := db.query('select * from users')!
rows := res.rows()
for row in rows {
	println(row.vals)
}
// Close the connection if needed
db.close()
```

## Streaming Results

Use `query_stream()` to read a large result directly from the server in bounded batches:

```v oksyntax
import db.mysql

fn stream_users(db &mysql.DB) ! {
	mut stream := db.query_stream('select id, name from users order by id')!
	defer {
		stream.close()
	}
	for {
		rows := stream.next_batch(1000)!
		if rows.len == 0 {
			break
		}
		for row in rows {
			println(row.vals)
		}
	}
}
```

Parameterized queries can be streamed with `prepare_stream()`:

```v oksyntax
import db.mysql

fn stream_users_after(db &mysql.DB, id int) ! {
	mut stmt := db.prepare_stream('select id, name from users where id > ? order by id')!
	defer {
		stmt.close()
	}
	stmt.execute([id.str()])!
	rows := stmt.next_batch(1000)!
	println(rows)
}
```

A stream exclusively holds its MySQL connection until it is exhausted or closed. Consume and
close it on the thread that created it. Existing `query()` and prepared statement APIs continue
to return materialized results. Stream batches contain `NullableRow` values, where `val_opt()`
and the raw `vals` array preserve SQL NULL as `none`. The `val()` and `values()` compatibility
helpers flatten NULL to an empty string. For multi-statement queries, `query_stream()` exposes the
first result and discards later results when the stream is exhausted or closed, keeping the
connection reusable. Use `exec_multi()` when every result is needed.

The legacy `use_result()` method is retained for compatibility and only discards a pending
result. Use `query_stream()` to read unbuffered rows.

`Result.fields()` and `StreamResult.fields()` expose the server's column metadata. The `length`
and `max_length` values are unsigned 64-bit integers so large MySQL column widths are preserved.

## Concurrent Usage

Sharing one `mysql.DB` across threads now serializes connection-level queries safely.

For concurrent servers, prefer `mysql.new_connection_pool(...)` so requests do not share the same
session and transaction state on one connection.

## Transaction

```v oksyntax
import db.mysql

// Create connection
config := mysql.Config{
	host:     '127.0.0.1'
	port:     3306
	username: 'root'
	password: '12345678'
	dbname:   'mysql'
}

mut db := mysql.connect(config)!
// turn off `autocommit` first
db.autocommit(false)!
// begin a new transaction
db.begin()!
mut result_code := db.exec_none('insert into users (username) values ("tom")')
assert result_code == 0
// make a savepoint
db.savepoint('savepoint1')!
result_code = db.exec_none('insert into users (username) values ("kitty")')
assert result_code == 0
// rollback to `savepoint1`
db.rollback_to('savepoint1')!
result_code = db.exec_none('insert into users (username) values ("mars")')
assert result_code == 0
db.commit()!
res := db.query('select * from users')!
rows := res.rows()
dump(rows)
// Close the connection if needed
db.close()
```
