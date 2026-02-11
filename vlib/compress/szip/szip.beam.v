// BEAM backend for compress.szip module
// The streaming Zip API (open/entry/read/write/close) wraps miniz (C library)
// which has no direct Erlang equivalent for streaming access.
//
// Erlang's built-in zip module provides WHOLE-FILE zip/unzip:
//   zip:zip(Name, FileList) — create a zip archive
//   zip:unzip(Archive, [{cwd, Dir}]) — extract a zip archive
//   zip:list_dir(Archive) — list entries
//
// The high-level functions (extract_zip_to_dir, zip_files, zip_folder) could
// be implemented via codegen interception to Erlang's zip module.
// The streaming entry-level API (open/read_entry/write_entry) requires NIF or port.
//
// Codegen interception status:
//   extract_zip_to_dir → zip:unzip/2 (FUTURE codegen interception)
//   zip_files → zip:zip/2 (FUTURE codegen interception)
//   zip_folder → zip:zip/2 + filelib:wildcard (FUTURE codegen interception)
//   Streaming API → Not mappable to Erlang zip module (needs NIF)
module szip

@[params]
pub struct ZipFolderOptions {
pub:
	omit_empty_folders bool
}

// Zip is an opaque zip archive handle.
// Not applicable on BEAM: no native zip handle exists.
pub struct Zip {
}

pub type Fn_on_extract_entry = fn (&&char, &&char) int

pub enum CompressionLevel {
	no_compression      = 0
	best_speed          = 1
	best_compression    = 9
	uber_compression    = 10
	default_level       = 6
	default_compression = -1
}

// OpenMode lists the opening modes.
pub enum OpenMode {
	write
	read_only
	append
}

// to_u8 converts OpenMode to its character representation.
// Real implementation — pure V match expression.
@[inline]
fn (om OpenMode) to_u8() u8 {
	return match om {
		.write { `w` }
		.read_only { `r` }
		.append { `a` }
	}
}

// open opens zip archive with compression level using the given mode.
// Not applicable on BEAM: requires native zlib/miniz bindings.
// Codegen could intercept → zip:zip_open(Name, Opts) from Erlang stdlib.
pub fn open(name string, level CompressionLevel, mode OpenMode) !&Zip {
	return error('szip not available on BEAM — requires native zlib bindings or codegen interception to erlang zip module')
}

// close closes the zip archive.
// Not applicable on BEAM: no-op since open never succeeds.
@[inline]
pub fn (mut z Zip) close() {
}

// open_entry opens an entry by name in the zip archive.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) open_entry(name string) ! {
	return error('szip not available on BEAM')
}

// open_entry_by_index opens an entry by index in the archive.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut z Zip) open_entry_by_index(index int) ! {
	return error('szip not available on BEAM')
}

// close_entry closes a zip entry.
// Not applicable on BEAM: no-op.
@[inline]
pub fn (mut zentry Zip) close_entry() {
}

// name returns a local name of the current zip entry.
// Not applicable on BEAM: returns empty string.
pub fn (mut zentry Zip) name() string {
	return ''
}

// index returns an index of the current zip entry.
// Not applicable on BEAM: returns error.
pub fn (mut zentry Zip) index() !int {
	return error('szip not available on BEAM')
}

// is_dir determines if the current zip entry is a directory entry.
// Not applicable on BEAM: returns error.
pub fn (mut zentry Zip) is_dir() !bool {
	return error('szip not available on BEAM')
}

// size returns an uncompressed size of the current zip entry.
// Not applicable on BEAM: returns 0.
@[inline]
pub fn (mut zentry Zip) size() u64 {
	return 0
}

// crc32 returns CRC-32 checksum of the current zip entry.
// Not applicable on BEAM: returns 0.
@[inline]
pub fn (mut zentry Zip) crc32() u32 {
	return 0
}

// write_entry compresses an input buffer for the current zip entry.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) write_entry(data []u8) ! {
	return error('szip not available on BEAM')
}

// create_entry compresses a file for the current zip entry.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) create_entry(name string) ! {
	return error('szip not available on BEAM')
}

// read_entry extracts the current zip entry into output buffer.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) read_entry() !voidptr {
	return error('szip not available on BEAM')
}

// read_entry_buf extracts the current zip entry into user specified buffer.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) read_entry_buf(buf voidptr, in_bsize int) !int {
	return error('szip not available on BEAM')
}

// extract_entry extracts the current zip entry into output file.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) extract_entry(path string) ! {
	return error('szip not available on BEAM')
}

// extract_zip_to_dir extracts zip file to directory.
// BEAM codegen interception (FUTURE): zip:unzip(File, [{cwd, Dir}])
// When codegen is implemented, translates to:
//   case zip:unzip(binary_to_list(File), [{cwd, binary_to_list(Dir)}]) of
//       {ok, _FileList} -> true;
//       {error, Reason} -> error(Reason)
//   end
pub fn extract_zip_to_dir(file string, dir string) !bool {
	if file.len == 0 {
		return error('szip.extract_zip_to_dir: empty file path')
	}
	if dir.len == 0 {
		return error('szip.extract_zip_to_dir: empty directory path')
	}
	return error('szip.extract_zip_to_dir requires codegen interception (zip:unzip/2)')
}

// zip_files zips files (full path) to zip file.
// BEAM codegen interception (FUTURE): zip:zip(ZipFile, FileList)
// When codegen is implemented, translates to:
//   FileListChars = [binary_to_list(F) || F <- FileList],
//   case zip:zip(binary_to_list(ZipFile), FileListChars) of
//       {ok, _ZipName} -> ok;
//       {error, Reason} -> error(Reason)
//   end
pub fn zip_files(path_to_file []string, path_to_export_zip string) ! {
	if path_to_file.len == 0 {
		return error('szip.zip_files: empty file list')
	}
	if path_to_export_zip.len == 0 {
		return error('szip.zip_files: empty output path')
	}
	return error('szip.zip_files requires codegen interception (zip:zip/2)')
}

// zip_folder zips all entries in folder recursively to the zip file.
// BEAM codegen interception (FUTURE): zip:zip(ZipFile, FileList) with filelib:wildcard
// When codegen is implemented, translates to:
//   Pattern = filename:join(binary_to_list(Folder), "**"),
//   Files = filelib:wildcard(Pattern),
//   case zip:zip(binary_to_list(ZipFile), Files) of
//       {ok, _ZipName} -> ok;
//       {error, Reason} -> error(Reason)
//   end
pub fn zip_folder(folder string, zip_file string, opt ZipFolderOptions) ! {
	if folder.len == 0 {
		return error('szip.zip_folder: empty folder path')
	}
	if zip_file.len == 0 {
		return error('szip.zip_folder: empty zip file path')
	}
	return error('szip.zip_folder requires codegen interception (zip:zip/2 + filelib:wildcard)')
}

// total returns the number of all entries in the zip archive.
// Not applicable on BEAM: requires native zlib bindings.
pub fn (mut zentry Zip) total() !int {
	return error('szip not available on BEAM')
}
