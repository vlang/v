// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// FileOptions configures a File handle.
@[params]
pub struct FileOptions {
pub:
	bucket string
}

// File is a reference to one S3 object. It holds no buffer; every method
// round-trips to S3 (or generates a presigned URL for `presign`).
//
// Construct via `Client.file(...)` or directly with `File{ client: &c, key: '...' }`.
pub struct File {
pub:
	client &Client
	bucket string
	key    string
}

// read returns the full object body. For range reads, see `read_range`.
pub fn (f &File) read() ![]u8 {
	return f.client.get(f.key, bucket: f.bucket)
}

// text is a UTF-8 convenience over `read`.
pub fn (f &File) text() !string {
	return f.client.get_string(f.key, bucket: f.bucket)
}

// read_range fetches `bytes=<begin>-<end_inclusive>` (HTTP Range semantics).
// Use `end < 0` to read to end-of-file.
pub fn (f &File) read_range(begin i64, end i64) ![]u8 {
	range := if end < 0 { 'bytes=${begin}-' } else { 'bytes=${begin}-${end}' }
	return f.client.get(f.key, bucket: f.bucket, range: range)
}

// write uploads `data` as the entire object body.
pub fn (f &File) write(data []u8, opts PutOptions) ! {
	mut o := opts
	if o.bucket == '' {
		o = PutOptions{
			...opts
			bucket: f.bucket
		}
	}
	f.client.put(f.key, data, o)!
}

// write_string is a UTF-8 convenience over `write`.
pub fn (f &File) write_string(s string, opts PutOptions) ! {
	f.write(s.bytes(), opts)!
}

// stat returns object metadata (size / etag / last-modified / content-type).
pub fn (f &File) stat() !Stat {
	return f.client.stat(f.key, bucket: f.bucket)
}

// exists is a HEAD that converts 404 into `false`.
pub fn (f &File) exists() !bool {
	return f.client.exists(f.key, bucket: f.bucket)
}

// size returns the Content-Length, in bytes.
pub fn (f &File) size() !i64 {
	return f.client.size(f.key, bucket: f.bucket)
}

// delete removes the object. Idempotent: no error when the object is absent.
pub fn (f &File) delete() ! {
	f.client.delete(f.key, bucket: f.bucket)!
}

// presign returns a presigned URL for this object. See `PresignOptions`.
pub fn (f &File) presign(opts PresignOptions) !string {
	mut o := opts
	if o.bucket == '' && f.bucket != '' {
		o = PresignOptions{
			...opts
			bucket: f.bucket
		}
	}
	return f.client.presign(f.key, o)
}
