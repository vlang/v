## Description

`tar` is a module to access tar archives.

Tape archives (tar) are a file format for storing a sequence of files that can be read and written 
as streams. This module covers the reading of the basic sections of archives produced by GNU tools
like Linux command `tar -xvf` but in memory instead modifing the filesystem. Parses directories, 
files, and file's content and manage paths longer than 100 chars.

### Read Efficiency

An entire tar file can be read in memory or by chunks. Keeps in memory a single decompressed 
[chunk](https://modules.vlang.io/compress.gzip.html#decompress_with_callback) of 32 KB at a time 
and also keeps in memory a single tar block of 512 bytes at a time. Convert paths to strings until 
needed and the user reader implementation can stop early the reading process.

### Read Example

The tar blocks are parsed and some fields are passed to `Reader` implemented methods.

```v
import os
import archive.tar

fn main() {
	os.chdir(@VMODROOT) or {}
	path := 'archive/tar/testdata/life.tar.gz'
	reader := tar.new_debug_reader()
	tar.read_tar_gz_file(path, reader)!
}
```
Look also in `examples` folder the `tar_gz_reader.v` program.

