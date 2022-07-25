## Description:

`pg` is a wrapper for the PostgreSQL client library. It provides access to a PostgreSQL
database server.

Before you can use this module, you must first have PostgreSQL installed on your system.
To do this, find your OS and perform the actions listed.

**NOTE**: These instructions are meant only as a convenience. If your OS is not listed 
or you need extra help, [go here](https://www.postgresql.org/download/).

### Fedora 31
```
sudo dnf install postgresql-server postgresql-contrib
sudo systemctl enable postgresql # to autostart on startup
sudo systemctl start  postgresql
```

### Ubuntu/Debian
```
sudo apt-get install postgresql postgresql-client
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

## Installing libpq-dev or its equivalent for your OS: ##

**Ubuntu/Debian**: `sudo apt-get install libpq-dev`

**Red Hat Linux (RHEL)**: `yum install postgresql-devel`

**OpenSuse**: `zypper in postgresql-devel`

**ArchLinux**: `pacman -S postgresql-libs`

##Getting Started with [PostgreSQL](https://www.postgresqltutorial.com/postgresql-getting-started)

Read this section to learn how to install and connect to PostgreSQL
*[Windows](https://www.postgresqltutorial.com/install-postgresql)*;
*[Linux](https://www.postgresqltutorial.com/postgresql-getting-started/install-postgresql-linux)*;
*[macOS](https://www.postgresqltutorial.com/postgresql-getting-started/install-postgresql-macos)*.
