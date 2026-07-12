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

## Installing libpq-dev or its equivalent for your OS

**Ubuntu/Debian**: `sudo apt install libpq-dev`

**Red Hat Linux (RHEL)**: `yum install postgresql-devel`

**OpenSuse**: `zypper in postgresql-devel`

**ArchLinux**: `pacman -S postgresql-libs`

**FreeBSD**: `pkg install postgresql18-client`

**OpenBSD**: `pkg_add postgresql-client`

**Windows**:

The directory structure of the `@VEXEROOT/thirdparty/pg` folder will look like the following
after following all instructions. Create the `pg` folder yourself if it does not exist yet.
```
@VEXEROOT/thirdparty/pg
├───libpq
│       libpq-fe.h
│       pg_config.h
│       postgres_ext.h
│
└───win64
    └───msvc
            libpq.lib
```

Installation instructions are as follows
```
Download the latest PostgreSQL version from the official website
(currently https://www.enterprisedb.com/downloads/postgres-postgresql-downloads)

In one of the steps in the installer, tick the following boxes:
[X] PostgreSQL Server
[ ] pgAdmin 4
[ ] Stack Builder
[X] Command Line Tools

We need PostgreSQL Server because it brings with it the C header files
needed for building programs that link to `libpq.dll`.

After finishing installation, add the folder `C:/Program Files/PostgreSQL/<version>/bin` to PATH.
Any program that wants to use postgres client functionality require these DLLs found in `/bin`:
- libcrypto-3-x64.dll
- libiconv-2.dll
- libintl-9.dll
- libpq.dll
- libssl-3-x64.dll
- libwinpthread-1.dll

If you want to compile with MSVC, you will need to copy `C:/Program Files/PostgreSQL/<version>/bin/libpq.lib`
into the `@VEXEROOT/thirdparty/pg/win64/msvc` directory.

Navigate to `C:/Program Files/PostgreSQL/<version>/include`. There you will find the files:
- `libpq-fe.h`
- `pg_config.h`
- `postgres_ext.h`

Copy the header files into `@VEXEROOT/thirdparty/pg/libpq`. You can now compile programs using the `db.pg` module.

---

After building an executable that uses `db.pg`, you may want to distribute it to others
who might not have the postgres DLLs installed on their machine. All you need to do is to
make sure a copy of all the required DLLs are in the same folder as your executable.
```

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

## Using Parameterized Queries

Parameterized queries (exec_param, etc.) in V require the use of the following syntax: ($n).

The number following the $ specifies which parameter from the argument array to use.

```v ignore
db.exec_param_many('INSERT INTO users (username, password) VALUES ($1, $2)', ['tom', 'securePassword'])!
db.exec_param('SELECT * FROM users WHERE username = ($1) limit 1', 'tom')!
```

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
