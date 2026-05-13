// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// Integration tests against a live S3-compatible endpoint. They are skipped
// unless the `S3_INTEGRATION` env var is set, and the `S3_HOST`, `S3_KEY_ID`
// and `S3_KEY_SECRET` env vars are populated. `v test .` stays fast and
// offline by default.
//
// Run with:
//   S3_INTEGRATION=1 \
//   S3_HOST=s3.example.com \
//   S3_KEY_ID=... S3_KEY_SECRET=... \
//   S3_BUCKET=vs3-tests \
//   v test .
import os
import rand
import time
import net.http

// http_dispatch_creds_present returns true when the env exposes the AWS-style
// names that http.fetch's bridge will pick up via `Credentials.from_env()`.
// The integration test below relies on this — `S3_HOST` / `S3_KEY_ID` /
// `S3_KEY_SECRET` (used by the rest of this file) aren't recognised by
// `from_env()`, so we map them on the fly when needed.
fn ensure_aws_env_for_bridge() {
	if os.getenv('S3_ACCESS_KEY_ID') == '' {
		os.setenv('S3_ACCESS_KEY_ID', os.getenv('S3_KEY_ID'), true)
	}
	if os.getenv('S3_SECRET_ACCESS_KEY') == '' {
		os.setenv('S3_SECRET_ACCESS_KEY', os.getenv('S3_KEY_SECRET'), true)
	}
	if os.getenv('S3_ENDPOINT') == '' {
		host := os.getenv('S3_HOST')
		endpoint := if host.contains('://') { host } else { 'https://${host}' }
		os.setenv('S3_ENDPOINT', endpoint, true)
	}
}

fn integration_enabled() bool {
	return os.getenv('S3_INTEGRATION') != ''
}

fn live_client() ?Client {
	if !integration_enabled() {
		return none
	}
	host := os.getenv('S3_HOST')
	key_id := os.getenv('S3_KEY_ID')
	secret := os.getenv('S3_KEY_SECRET')
	if host == '' || key_id == '' || secret == '' {
		return none
	}
	endpoint := if host.contains('://') { host } else { 'https://${host}' }
	bucket := os.getenv_opt('S3_BUCKET') or { 'vs3-tests' }
	return Client{
		credentials: Credentials{
			endpoint:          endpoint
			access_key_id:     key_id
			secret_access_key: secret
			bucket:            bucket
			region:            'us-east-1'
		}
	}
}

fn rand_key(prefix string) string {
	return '${prefix}/${time.now().unix()}-${rand.u64()}.txt'
}

fn test_integration_roundtrip() {
	c := live_client() or { return }
	key := rand_key('it/roundtrip')
	defer {
		c.delete(key) or {}
	}
	c.put(key, 'hi'.bytes(), content_type: 'text/plain') or { panic(err) }
	got := c.get_string(key) or { panic(err) }
	assert got == 'hi'
	stat := c.stat(key) or { panic(err) }
	assert stat.size == 2
	assert stat.content_type.starts_with('text/plain')
}

fn test_integration_range_read() {
	c := live_client() or { return }
	key := rand_key('it/range')
	defer {
		c.delete(key) or {}
	}
	c.put(key, 'abcdefghijklmnop'.bytes()) or { panic(err) }
	first := c.get(key, range: 'bytes=0-4') or { panic(err) }
	assert first.bytestr() == 'abcde', 'got: ${first.bytestr()}'
	tail := c.get(key, range: 'bytes=10-') or { panic(err) }
	assert tail.bytestr() == 'klmnop', 'got: ${tail.bytestr()}'
}

fn test_integration_presign_get() {
	c := live_client() or { return }
	key := rand_key('it/presign')
	defer {
		c.delete(key) or {}
	}
	c.put(key, 'PRESIGN_OK'.bytes()) or { panic(err) }
	url := c.presign(key, expires_in: 60) or { panic(err) }
	body := fetch_url(url) or { panic(err) }
	assert body.contains('PRESIGN_OK'), 'got: ${body}'
}

fn test_integration_list_with_prefix() {
	c := live_client() or { return }
	prefix := 'it/list-${rand.u64()}/'
	defer {
		// Best-effort cleanup
		if res := c.list(prefix: prefix, max_keys: 100) {
			for o in res.objects {
				c.delete(o.key) or {}
			}
		}
	}
	for i in 0 .. 3 {
		c.put('${prefix}item-${i}.txt', 'x'.bytes()) or { panic(err) }
	}
	res := c.list(prefix: prefix, fetch_owner: false) or { panic(err) }
	assert res.objects.len == 3
	for o in res.objects {
		assert o.key.starts_with(prefix)
		assert o.size == 1
	}
}

fn test_integration_exists_and_delete() {
	c := live_client() or { return }
	key := rand_key('it/exists')
	c.put(key, 'present'.bytes()) or { panic(err) }
	assert c.exists(key) or { panic(err) }
	c.delete(key) or { panic(err) }
	assert !(c.exists(key) or { panic(err) })
}

fn test_integration_fetch_helper() {
	c := live_client() or { return }
	key := rand_key('it/fetch')
	url := 's3://${c.credentials.bucket}/${key}'
	defer {
		c.delete(key) or {}
	}
	put := fetch(url,
		method:      .put
		body:        'fetch_works'.bytes()
		credentials: c.credentials
	) or { panic(err) }
	assert put.status_code == 200
	get := fetch(url, credentials: c.credentials) or { panic(err) }
	assert get.body.bytestr() == 'fetch_works'
}

fn test_integration_multipart_upload_bytes() {
	c := live_client() or { return }
	// 6 MiB triggers multipart (one full part + a tail).
	mut data := []u8{len: 6 * 1024 * 1024}
	for i in 0 .. data.len {
		data[i] = u8(i & 0xFF)
	}
	key := rand_key('it/multipart')
	defer {
		c.delete(key) or {}
	}
	c.upload_bytes_multipart(key, data) or { panic(err) }
	stat := c.stat(key) or { panic(err) }
	assert stat.size == data.len
	got := c.get(key) or { panic(err) }
	assert got.len == data.len
	for i in 0 .. data.len {
		if got[i] != data[i] {
			assert false, 'byte mismatch at ${i}'
			break
		}
	}
}

fn test_integration_multipart_parallel_many_parts() {
	c := live_client() or { return }
	// Force 8 parts at the 5 MiB minimum + tail. With queue_size=5 (default)
	// we get genuine parallel dispatch and out-of-order completion that the
	// dispatcher must re-order before CompleteMultipartUpload.
	mut data := []u8{len: 8 * 5 * 1024 * 1024 + 1024}
	for i in 0 .. data.len {
		data[i] = u8((i * 31 + 7) & 0xFF)
	}
	key := rand_key('it/parallel')
	defer {
		c.delete(key) or {}
	}
	c.upload_bytes_multipart(key, data) or { panic(err) }
	stat := c.stat(key) or { panic(err) }
	assert stat.size == data.len, 'size mismatch: got ${stat.size}'
	// Verify a tail range, where corruption from out-of-order parts would land.
	tail := c.get(key, range: 'bytes=${data.len - 1024}-') or { panic(err) }
	for i in 0 .. tail.len {
		if tail[i] != data[data.len - 1024 + i] {
			assert false, 'tail byte mismatch at offset ${i}'
			break
		}
	}
}

fn test_integration_bucket_lifecycle() {
	c := live_client() or { return }
	// Random suffix keeps reruns idempotent.
	name := 's3v-it-${rand.u64():08x}'.to_lower()
	c.create_bucket(bucket: name) or {
		// Some providers require region constraint or have different defaults
		// — surface but don't fail the suite for that one provider quirk.
		eprintln('skip: create_bucket(${name}) failed: ${err}')
		return
	}
	defer {
		c.delete_bucket(bucket: name) or {}
	}
	exists := c.bucket_exists(bucket: name) or { panic(err) }
	assert exists, 'bucket ${name} should exist after create'
	// Delete once explicitly so we can assert it disappeared.
	c.delete_bucket(bucket: name) or { panic(err) }
	gone := c.bucket_exists(bucket: name) or { panic(err) }
	assert !gone, 'bucket ${name} should be gone after delete'
}

fn test_integration_invalid_credentials_yields_clean_error() {
	if !integration_enabled() {
		return
	}
	host := os.getenv('S3_HOST')
	if host == '' {
		return
	}
	c := Client{
		credentials: Credentials{
			endpoint:          'https://${host}'
			access_key_id:     'AKIADOESNOTEXIST'
			secret_access_key: 'badbadbadbadbadbadbadbadbadbadbadbadbadbad'
			bucket:            os.getenv_opt('S3_BUCKET') or { 'vs3-tests' }
		}
	}
	if _ := c.stat('any-key') {
		assert false, 'expected error'
	} else {
		assert err is S3Error, 'got non-S3 error: ${typeof(err).name}'
	}
}

// fetch_url is a tiny `http.get`-style helper used only in the presign test.
fn fetch_url(url string) !string {
	resp := http.get(url)!
	return resp.body
}

// Exercises the `http.fetch(url: 's3://...')` bridge end-to-end. The s3
// module's `init()` registers itself with net.http; this test goes through
// that path (no direct `s3.fetch` call) to prove the dispatch works.
fn test_integration_http_fetch_dispatch_to_s3() {
	c := live_client() or { return }
	ensure_aws_env_for_bridge()
	bucket := c.credentials.bucket
	key := rand_key('it/http-bridge')
	url := 's3://${bucket}/${key}'

	put := http.fetch(url: url, method: .put, data: 'via-http.fetch') or {
		assert false, 'PUT through bridge failed: ${err}'
		return
	}
	assert put.status_code == 200, 'unexpected PUT status: ${put.status_code}'

	get := http.fetch(url: url) or {
		assert false, 'GET through bridge failed: ${err}'
		return
	}
	assert get.status_code == 200, 'unexpected GET status: ${get.status_code}'
	assert get.body == 'via-http.fetch', 'body mismatch: ${get.body}'

	del := http.fetch(url: url, method: .delete) or {
		assert false, 'DELETE through bridge failed: ${err}'
		return
	}
	assert del.status_code in [200, 204], 'unexpected DELETE status: ${del.status_code}'
}
