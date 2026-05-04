# io.fs

`io.fs` defines small filesystem interfaces that can be shared by different
backends and virtual filesystems.

The module is modeled after Go's `io/fs` package, but follows V naming and type
conventions.

Current helpers:

- `valid_path(name string) bool`
- `read_file(filesystem FS, name string) ![]u8`
- `read_dir(filesystem FS, name string) ![]DirEntry`
- `stat(filesystem FS, name string) !FileInfo`
- `file_info_to_dir_entry(info FileInfo) DirEntry`
