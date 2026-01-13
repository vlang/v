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

```
1. Download PostgreSQL SDK from official site
2. Extract archive to postgres-master folder
3. Copy folder postgres-master/src/interfaces/libpq to v/thirdparty/pg
4. Copy file postgres-master/src/include/postgres_ext.h to v/thirdparty/pg/libpq

If you build PostgreSQL from source pg_config_ext.h and pg_config.h will be created automatically:
5. Copy file postgres-master/src/include/pg_config_ext.h to v/thirdparty/pg/libpq
6. Copy file postgres-master/src/include/pg_config.h to v/thirdparty/pg/libpq

If you do not build PostgreSQL from source code:
5. Copy file postgres-master/src/include/pg_config_ext.h.in to v/thirdparty/pg/libpq
- rename pg_config_ext.h.in to pg_config_ext.h
- in pg_config_ext.h change line **#undef PG_INT64_TYPE** to **#define PG_INT64_TYPE long int**
6. Copy file postgres-master/src/include/pg_config.h.in to v/thirdparty/pg/libpq
- rename pg_config.h.in to pg_config.h
- in pg_config.h change line **#undef PG_VERSION_NUM** to **#define PG_VERSION_NUM X0Y0Z**
where X is major db version, Y is minor version, Z is patch version. So if your version is 17.1.2
PG_VERSION_NUM will be 170102, PG_VERSION_NUM should be number, dot sign is changing to 0,
format *PG_VERSION_NUM 17* without 0 also should work

7. Add libpq.dll to v/thirdparty/pg/win64

If you are going to use the msvc compiler:
7. Add libpq.lib(C:\Program Files\PostgreSQL\{version}\lib) to v/thirdparty/pg/win64/msvc

8. Add libpq.dll, libcrypto-3-x64.dll, libssl-3-x64.dll to where your executable is.

To get the libpq.dll file, you can install the PostgreSQL database,
and get this dll from its bin/ folder, or compile DB from source code.
```

## Getting Started with [PostgreSQL](https://www.postgresqltutorial.com/postgresql-getting-started)

Read this section to learn how to install and connect to PostgreSQL
[*Windows*](https://www.postgresqltutorial.com/install-postgresql);
[*Linux*](https://www.postgresqltutorial.com/postgresql-getting-started/install-postgresql-linux);
[*macOS*](https://www.postgresqltutorial.com/postgresql-getting-started/install-postgresql-macos).

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

```v ignore
import db.pg

fn main() {
	db := pg.connect(pg.Config{ user: 'postgres', password: 'password', dbname: 'mydb' })!
	defer { db.close() or {} }

	// Start listening on a channel
	db.listen('my_channel')!

	// From another connection or session, send a notification
	db.notify('my_channel', 'Hello, World!')!

	// Process incoming data from the server
	db.consume_input()!

	// Check for notifications
	if notification := db.get_notification() {
		println('Received notification on channel: ${notification.channel}')
		println('Payload: ${notification.payload}')
		println('From server process: ${notification.pid}')
	}

	// Stop listening
	db.unlisten('my_channel')!
	// Or unlisten from all channels
	db.unlisten_all()!
}
```

### Event Loop with Polling

For real-time applications, you can use the socket file descriptor with select/poll:

```v ignore
import db.pg
import time

fn main() {
	db := pg.connect(pg.Config{ user: 'postgres', password: 'password', dbname: 'mydb' })!
	defer { db.close() or {} }

	db.listen('events')!

	// Get socket fd for polling (useful with select/epoll)
	socket_fd := db.socket()
	println('Socket FD: ${socket_fd}')

	// Simple polling loop
	for {
		db.consume_input()!
		for {
			notification := db.get_notification() or { break }
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
