// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Module s3 is an S3-compatible client for V.
//
// Quick start:
//
//   import s3
//
//   client := s3.new_client(s3.Credentials{
//       endpoint:          'https://s3.example.com'
//       access_key_id:     '...'
//       secret_access_key: '...'
//       bucket:            'my-bucket'
//   })
//   client.put('hello.txt', 'Hi from V!'.bytes())!
//   text := client.get_string('hello.txt')!
//   url  := client.presign('hello.txt', expires_in: 3600)!
//
//   // s3:// fetch helper:
//   resp := s3.fetch('s3://my-bucket/hello.txt')!
//
// Construct credentials from the environment (S3_*, AWS_*, CELLAR_ADDON_*,
// SCW_*, B2_*, R2_*, SPACES_* — first non-empty wins per field):
//
//   client := s3.new_client(s3.Credentials.from_env())
//
// Multipart for large files:
//
//   client.upload_file('big.bin', '/path/to/big.bin', s3.PutOptions{ content_type: 'application/octet-stream' })!
module s3

// version is the module version, kept in sync with v.mod.
pub const version = '0.1.0'

// info_string returns a one-line build-time identification, useful in
// User-Agent strings or `--version` output.
pub fn info_string() string {
	return 's3/${version}'
}
