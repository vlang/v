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

The directory structure of the `@VEXEROOT/thirdparty/pg` folder will look like the following after following all instructions. Create the `pg` folder yourself if it does not exist yet.
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

```
Download the latest PostgreSQL version from the official website (currently https://www.enterprisedb.com/downloads/postgres-postgresql-downloads)

In one of the steps in the installer, tick the following boxes:
[X] PostgreSQL Server
[ ] pgAdmin 4
[ ] Stack Builder
[X] Command Line Tools

We need PostgreSQL Server because it brings with it the C header files needed for building programs that link to `libpq.dll`.

After finishing installation, add the folder `C:/Program Files/PostgreSQL/<version>/bin` to PATH. Any program that wants to use postgres client functionality require these DLLs found in `/bin`:
- libcrypto-3-x64.dll
- libiconv-2.dll
- libintl-9.dll
- libpq.dll
- libssl-3-x64.dll
- libwinpthread-1.dll

If you want to compile with MSVC, you will need to copy `C:/Program Files/PostgreSQL/<version>/bin/libpq.lib` into the `@VEXEROOT/thirdparty/pg/win64/msvc` directory.

Navigate to `C:/Program Files/PostgreSQL/<version>/include`. There you will find the files:
- `libpq-fe.h`
- `pg_config.h`
- `postgres_ext.h`

Copy the header files into `@VEXEROOT/thirdparty/pg/libpq`. You can now compile programs using the `db.pg` module.

---

After building an executable that uses `db.pg`, you may want to distribute it to others who might not have the postgres DLLs installed on their machine. All you need to do is to make sure a copy of all the required DLLs are in the same folder as your executable.
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
