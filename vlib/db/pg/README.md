## Description

`pg` is a wrapper for the PostgreSQL client library. It provides access to a PostgreSQL
database server.

Before you can use this module, you must first have PostgreSQL installed on your system.
To do this, find your OS and perform the actions listed.

> **Note**
> These instructions are meant only as a convenience. If your OS is not listed
> or you need extra help, [go here](https://www.postgresql.org/download/).

### Fedora 31

```
sudo dnf install postgresql-server postgresql-contrib
sudo systemctl enable postgresql # to autostart on startup
sudo systemctl start  postgresql
```

### Ubuntu/Debian

```
sudo apt install postgresql postgresql-client
sudo systemctl enable postgresql # to autostart on startup
sudo systemctl start  postgresql
```

### MacOSX (Homebrew)

```
brew install postgresql
brew services start postgresql
```

On newer Homebrew setups the formula and service name may be versioned
instead, for example `postgresql@18`. Use the exact name reported by
`brew info postgresql`.

### MacOSX (MacPorts)

```
gem install pg -- --with-pg-config=/opt/local/lib/postgresql[version number]/bin/pg_config
```

## Installing the libpq runtime

`db.pg` loads libpq at runtime, so compiling a program does not require PostgreSQL
development headers, `pkg-config` metadata, or an unversioned linker library. Only the libpq runtime
library is needed when the program connects to a database.

**Ubuntu/Debian**: `sudo apt install libpq5`

**Red Hat Linux (RHEL)**: `sudo dnf install libpq`

**OpenSuse**: `sudo zypper in libpq5`

**ArchLinux**: `sudo pacman -S postgresql-libs`

**FreeBSD**: `pkg install postgresql18-client`

**OpenBSD**: `pkg_add postgresql-client`

Set `V_PQ_LIB` to a full library path when libpq is installed outside the system loader's search
path. The loader reads it once, immediately before the first connection attempt.

**Windows**:

Add the PostgreSQL `bin` directory to `PATH`, or copy `libpq.dll` next to the executable. The
PostgreSQL client also needs these runtime DLLs from the same directory:

- libcrypto-3-x64.dll
- libiconv-2.dll
- libintl-9.dll
- libpq.dll
- libssl-3-x64.dll
- libwinpthread-1.dll

After building an executable that uses `db.pg`, you may want to distribute it to others
who might not have the postgres DLLs installed on their machine. All you need to do is to
make sure a copy of all the required DLLs are in the same folder as your executable.

## Getting Started with [PostgreSQL](https://www.postgresqltutorial.com/postgresql-getting-started)

Read this section to learn how to install and connect to PostgreSQL
[*Windows*](https://www.postgresqltutorial.com/install-postgresql);
[*Linux*](https://www.postgresqltutorial.com/postgresql-getting-started/install-postgresql-linux);
[*macOS*](https://www.postgresqltutorial.com/postgresql-getting-started/install-postgresql-macos).

When you use `pg.connect(pg.Config{ ... })`, empty `Config` fields are omitted from the
generated libpq connection string. That lets libpq defaults, `PGPASSWORD`, and `.pgpass`
apply when you do not set those fields in code.

`pg.Config` also exposes libpq SSL/TLS connection keywords:

```v ignore
mut db := pg.connect(pg.Config{
	host:     'db.example.com'
	user:     'app'
	password: 'secret'
	dbname:   'prod'
	ssl_mode: .verify_full
	ssl_ca:   '/etc/ssl/certs/root-ca.pem'
	ssl_cert: '/etc/ssl/certs/client.pem'
	ssl_key:  '/etc/ssl/private/client.key'
})!
```

The SSL fields map to libpq's `sslmode`, `sslcert`, `sslkey`, `sslrootcert`, and
`sslcrl` connection parameters.

## Thread Safety & Connection Pool

`pg.connect()` returns a `&DB` that is safe to share across V threads. Internally
`DB` holds a pool of `Conn` objects (one libpq `PGconn*` each); every method on
`DB` transparently checks a `Conn` out of the pool for the duration of the call
and returns it when done. This matches Go's `database/sql.DB` model.

```v ignore
mut db := pg.connect(pg.Config{ ... })!
defer { db.close() or {} }

// Pool defaults: unlimited open conns, 2 idle conns kept warm, no lifetime cap.
// Tune them like Go:
db.set_max_open_conns(50)
db.set_max_idle_conns(10)
db.set_conn_max_lifetime(30 * time.minute)
```

For operations that must run on the **same physical connection** — LISTEN/NOTIFY,
session-scoped prepared statements, manual transactions — pin a conn with
`db.conn()` or open a transaction with `db.begin()`:

```v ignore
// Pinned connection: returned to the pool when conn.close() is called.
mut c := db.conn()!
defer { c.close() or {} }
c.listen('my_channel')!

// Transaction: the conn is pinned for the lifetime of the Tx and released on
// commit() or rollback().
mut tx := db.begin()!
tx.exec('UPDATE accounts SET balance = balance - 100 WHERE id = 1')!
tx.exec('UPDATE accounts SET balance = balance + 100 WHERE id = 2')!
tx.commit()!
```

If you need to manage pooling outside `db.pg`, use `pg.connect_direct()` to open one
physical connection without the built-in pool:

```v ignore
mut conn := pg.connect_direct(pg.Config{ host: 'localhost', dbname: 'app' })!
defer { conn.close() or {} }

rows := conn.exec('select 1')!
```

## Result Column Metadata

Queries made with `exec_result()`, `exec_param_many_result()`, or
`exec_prepared_result()` return a `pg.Result` whose `fields` array contains the
libpq metadata for each result column. These methods require a mutable connection:

```v oksyntax
import db.pg

fn show_columns(mut conn pg.Conn) ! {
	result := conn.exec_result('select 1::int4 as id, 3.14::numeric(10, 2) as amount')!
	for field in result.fields {
		println('${field.name}: oid=${field.type_oid}, modifier=${field.type_modifier}')
	}
}
```

Each `pg.Field` preserves the type OID, type modifier, fixed size, result format,
source table OID, and source table column number reported by libpq. Type OIDs for
user-defined types are database-specific. PostgreSQL can resolve a type OID and
modifier to its display name with `pg_catalog.format_type(oid, modifier)`.

## Using Parameterized Queries

Parameterized queries (exec_param, etc.) in V require the use of the following syntax: ($n).

The number following the $ specifies which parameter from the argument array to use.

```v ignore
db.exec_param_many('INSERT INTO users (username, password) VALUES ($1, $2)', ['tom', 'securePassword'])!
db.exec_param('SELECT * FROM users WHERE username = ($1) limit 1', 'tom')!
```

When an operation cannot use parameters, `escape_literal` returns a complete quoted PostgreSQL
literal using libpq's connection-aware escaping:

```v ignore
mut conn := db.conn()!
defer { conn.close() or {} }
value := conn.escape_literal("O'Reilly")!
row := conn.exec_one('INSERT INTO authors (name) VALUES (${value}) RETURNING id')!
```

Escaping and execution must use the same connection because escaping depends on its settings.
Prefer parameterized queries whenever possible. Do not add quotes around the returned value.

## Using LISTEN/NOTIFY

PostgreSQL's LISTEN/NOTIFY mechanism allows you to build event-driven applications. One
connection can send notifications on a channel, and all connections listening on that
channel will receive them.

### Basic Usage

LISTEN/NOTIFY is session-scoped, so you must pin a `Conn` from the pool —
calling `db.listen()` would only listen on whichever pooled conn happens to
serve that one call.

```v ignore
import db.pg

fn main() {
	mut db := pg.connect(pg.Config{ user: 'postgres', password: 'password', dbname: 'mydb' })!
	defer { db.close() or {} }

	mut c := db.conn()!
	defer { c.close() or {} }

	// Start listening on a channel
	c.listen('my_channel')!

	// From another connection or session, send a notification
	c.notify('my_channel', 'Hello, World!')!

	// Process incoming data from the server
	c.consume_input()!

	// Check for notifications
	if notification := c.get_notification() {
		println('Received notification on channel: ${notification.channel}')
		println('Payload: ${notification.payload}')
		println('From server process: ${notification.pid}')
	}

	// Stop listening
	c.unlisten('my_channel')!
	// Or unlisten from all channels
	c.unlisten_all()!
}
```

### Event Loop with Polling

For real-time applications, you can use the socket file descriptor with select/poll:

```v ignore
import db.pg
import time

fn main() {
	mut db := pg.connect(pg.Config{ user: 'postgres', password: 'password', dbname: 'mydb' })!
	defer { db.close() or {} }

	mut c := db.conn()!
	defer { c.close() or {} }

	c.listen('events')!

	// Get socket fd for polling (useful with select/epoll)
	socket_fd := c.socket()
	println('Socket FD: ${socket_fd}')

	// Simple polling loop
	for {
		c.consume_input()!
		for {
			notification := c.get_notification() or { break }
			println('Event: ${notification.channel} - ${notification.payload}')
		}
		time.sleep(100 * time.millisecond)
	}
}
```

### Available Methods

- `listen(channel string)` - Register to receive notifications on a channel
- `unlisten(channel string)` - Unregister from a specific channel
- `unlisten_all()` - Unregister from all channels
- `notify(channel string, payload string)` - Send a notification (payload can be empty)
- `consume_input()` - Read pending data from server (call before get_notification)
- `get_notification()` - Returns the next pending notification, or none for no notifications.
- `socket()` - Returns the connection's socket file descriptor for use with select/poll
