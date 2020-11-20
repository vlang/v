Before you can use this module, you must first have PostgreSQL installed on
your system. To do this, find your OS and perform the actions listed. 

**NOTE**: These instructions are meant only as a convenience. If your OS is not
listed or you need extra help, [go here](https://www.postgresql.org/download/).

### Fedora 31
```
sudo dnf install postgresql-server postgresql-contrib
sudo systemctl enable postgresql # to autostart on startup
sudo systemctl start  postgresql
```

### Debian 10/11
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
