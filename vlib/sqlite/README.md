## Description:

`sqlite` is a thin wrapper for [the SQLite library](https://sqlite.org/), which in turn is
"a C-language library that implements a small, fast, self-contained,
high-reliability, full-featured, SQL database engine."

# Install SQLite Dependency

Before you can use this module, you must first have the SQLite development
library installed on your system.

**Fedora 31**:

`sudo dnf -y install sqlite-devel`


**Ubuntu 20.04**:

`sudo apt install -y libsqlite3-dev`


**Windows**:
- Download the source zip from [SQLite Downloads](https://sqlite.org/download.html)
- Create a new `sqlite` subfolder inside `v/thirdparty`
- Extract the zip into that folder
