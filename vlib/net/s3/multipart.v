// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import os
import strings
import time

// Multipart upload constants — defaults aligned with the S3 protocol limits.
pub const min_part_size = i64(5 * 1024 * 1024) // 5 MiB
pub const max_part_size = i64(5 * 1024 * 1024 * 1024) // 5 GiB
pub const max_parts = 10000 // hard S3 limit

// upload_file streams a local file to S3, choosing single-part or multipart
// based on size. Use this for files larger than ~50 MiB or anything you
// don't want to slurp into memory.
//
// `key` is the destination object key. The local file is read in
// `client.part_size` chunks (default 5 MiB).
pub fn (c &Client) upload_file(key string, local_path string, opts PutOptions) ! {
	stat_local := os.stat(local_path) or {
		return new_error('LocalFileNotFound',
			'Cannot stat local file: ${local_path} (${err.msg()})')
	}
	size := i64(stat_local.size)
	if size <= min_part_size {
		data := os.read_bytes(local_path) or {
			return new_error('LocalReadFailed', 'Cannot read ${local_path}: ${err.msg()}')
		}
		return c.put(key, data, opts)
	}
	c.upload_file_multipart(key, local_path, size, opts)!
}

// MultipartUploader is a stateful handle to an in-flight multipart upload,
// produced by `Client.start_multipart`. Use it when you want to push parts
// generated on-the-fly (network sources, decompressors, anything you don't
// want to materialise on disk):
//
//   mut up := c.start_multipart('key', s3.PutOptions{ content_type: 'application/octet-stream' })!
//   for chunk in chunks { up.upload(chunk)! }
//   up.complete()!
//
// On error, `complete()` / `upload()` return without aborting. The caller
// must invoke `abort()` themselves so that `defer { up.abort() or {} }`
// remains an explicit, visible cleanup hook.
pub struct MultipartUploader {
mut:
	client      &Client
	key         string
	upload_id   string
	opts        PutOptions
	parts       []PartRef
	part_number int
	completed   bool
	aborted     bool
}

// start_multipart begins a multipart upload and returns a MultipartUploader.
// Memory cost: zero — each `upload(chunk)` call streams the chunk to S3 and
// returns when it is acknowledged.
pub fn (c &Client) start_multipart(key string, opts PutOptions) !MultipartUploader {
	upload_id := c.initiate_multipart(key, opts)!
	return MultipartUploader{
		// `unsafe { c }` is required to store the receiver's pointer in the
		// returned struct — V's borrow checker forbids this implicitly, even
		// though `Client` is `@[heap]` so the lifetime is safe.
		client:      unsafe { c }
		key:         key
		upload_id:   upload_id
		opts:        opts
		parts:       []PartRef{}
		part_number: 1
	}
}

// upload pushes one chunk as the next part. Each chunk MUST be at least
// `min_part_size` (5 MiB) except the last one — that's an S3 invariant
// the server enforces; we do not buffer for you.
pub fn (mut u MultipartUploader) upload(data []u8) ! {
	if u.completed || u.aborted {
		return new_error('InvalidState', 'multipart upload is already finalised')
	}
	if u.part_number > max_parts {
		return new_error('TooManyParts', 'Exceeded ${max_parts} parts — increase part_size')
	}
	etag := u.client.upload_part(u.key, u.upload_id, u.part_number, data, u.opts)!
	u.parts << PartRef{
		part_number: u.part_number
		etag:        etag
	}
	u.part_number++
}

// complete finalises the upload. After this call the object is visible.
pub fn (mut u MultipartUploader) complete() ! {
	if u.completed {
		return
	}
	u.client.complete_multipart(u.key, u.upload_id, u.parts, u.opts)!
	u.completed = true
}

// abort cancels the in-flight upload. Idempotent.
pub fn (mut u MultipartUploader) abort() ! {
	if u.aborted || u.completed {
		return
	}
	u.client.abort_multipart(u.key, u.upload_id, u.opts)!
	u.aborted = true
}

// upload_bytes_multipart uploads an in-memory byte buffer using multipart.
// Useful for large generated payloads (test fixtures up to 5 GiB). Uses up
// to `Client.queue_size` parallel part uploads.
pub fn (c &Client) upload_bytes_multipart(key string, data []u8, opts PutOptions) ! {
	upload_id := c.initiate_multipart(key, opts)!
	parts := c.run_parallel_parts(key, upload_id, opts, fn [data] (offset i64, want int) ![]u8 {
		end := if offset + i64(want) > i64(data.len) { i64(data.len) } else { offset + i64(want) }
		return data[offset..end]
	}, i64(data.len)) or {
		c.abort_multipart(key, upload_id, opts) or {}
		return err
	}
	c.complete_multipart(key, upload_id, parts, opts)!
}

// upload_file_multipart streams a local file part-by-part with up to
// `Client.queue_size` concurrent uploads. Peak memory is roughly
// `queue_size * part_size`.
pub fn (c &Client) upload_file_multipart(key string, local_path string, size i64, opts PutOptions) ! {
	mut f := os.open(local_path) or {
		return new_error('LocalReadFailed', 'open ${local_path}: ${err.msg()}')
	}
	defer {
		f.close()
	}
	upload_id := c.initiate_multipart(key, opts)!
	parts := c.run_parallel_parts(key, upload_id, opts, fn [mut f] (offset i64, want int) ![]u8 {
		mut buf := []u8{len: want}
		n := f.read_bytes_into(u64(offset), mut buf) or {
			return new_error('LocalReadFailed', 'read_bytes_into: ${err.msg()}')
		}
		if n <= 0 {
			return new_error('LocalReadFailed', 'unexpected EOF at offset ${offset}')
		}
		return buf[..n]
	}, size) or {
		c.abort_multipart(key, upload_id, opts) or {}
		return err
	}
	c.complete_multipart(key, upload_id, parts, opts)!
}

// PartJob is a single upload-part task pushed onto the worker queue.
struct PartJob {
	part_number int
	data        []u8
}

// PartResult carries the worker outcome back to the dispatcher.
struct PartResult {
	part_number int
	etag        string
	err         ?IError
}

// part_worker pulls jobs off `jobs` and pushes results onto `results`. Exits
// when `jobs` is closed.
fn (c &Client) part_worker(key string, upload_id string, opts PutOptions, jobs chan PartJob, results chan PartResult) {
	for {
		job := <-jobs or { return }
		etag := c.upload_part(key, upload_id, job.part_number, job.data, opts) or {
			results <- PartResult{
				part_number: job.part_number
				err:         err
			}
			continue
		}
		results <- PartResult{
			part_number: job.part_number
			etag:        etag
		}
	}
}

// run_parallel_parts dispatches `producer` chunks across a worker pool of
// `Client.queue_size` goroutines and gathers the resulting PartRefs. The
// caller is responsible for issuing `initiate_multipart` / `abort_multipart`
// / `complete_multipart` around this call. `producer(offset, want)` must
// return a freshly-allocated chunk of length ≤ `want` bytes (workers may
// hold the slice past the next call). Returns parts in completion order;
// `complete_multipart` re-sorts by part number before sending the manifest.
fn (c &Client) run_parallel_parts(key string, upload_id string, opts PutOptions, producer fn (offset i64, want int) ![]u8, total i64) ![]PartRef {
	workers := if c.queue_size < 1 { 1 } else { c.queue_size }
	part_size := if c.part_size < min_part_size { min_part_size } else { c.part_size }
	jobs := chan PartJob{cap: workers}
	results := chan PartResult{cap: workers}
	for _ in 0 .. workers {
		spawn c.part_worker(key, upload_id, opts, jobs, results)
	}

	mut parts := []PartRef{}
	mut first_err := ?IError(none)
	mut sent := 0
	mut received := 0
	mut offset := i64(0)
	mut part_number := 1

	for offset < total && first_err == none {
		want := if offset + part_size > total { int(total - offset) } else { int(part_size) }
		chunk := producer(offset, want) or {
			first_err = err
			break
		}
		if chunk.len == 0 {
			break
		}
		jobs <- PartJob{
			part_number: part_number
			data:        chunk
		}
		sent++
		offset += i64(chunk.len)
		part_number++
		if part_number > max_parts {
			first_err = new_error('TooManyParts',
				'Exceeded ${max_parts} parts — increase part_size')
			break
		}
		// Drain ready results without blocking — surfaces errors early so we
		// can stop dispatching new parts.
		for {
			select {
				r := <-results {
					received++
					if e := r.err {
						if first_err == none {
							first_err = e
						}
					} else {
						parts << PartRef{
							part_number: r.part_number
							etag:        r.etag
						}
					}
				}
				else {
					break
				}
			}
		}
	}
	jobs.close()
	for received < sent {
		r := <-results
		received++
		if e := r.err {
			if first_err == none {
				first_err = e
			}
		} else {
			parts << PartRef{
				part_number: r.part_number
				etag:        r.etag
			}
		}
	}
	results.close()
	if e := first_err {
		return e
	}
	return parts
}

// PartRef is one ETag/PartNumber pair recorded during multipart upload.
pub struct PartRef {
pub:
	part_number int
	etag        string
}

// initiate_multipart starts an upload and returns the UploadId. ACL,
// content-type, and friends from `opts` are sent here — they apply to the
// final object.
pub fn (c &Client) initiate_multipart(key string, opts PutOptions) !string {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	signed := sign_request(creds, SignRequest{
		method:        'POST'
		path:          path
		query:         'uploads='
		payload_hash:  empty_sha256
		extra_headers: put_object_headers(opts)
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code != 200 {
		return new_http_error(resp.status_code, key, resp.body)
	}
	upload_id := extract_xml_tag(resp.body, 'UploadId')
	if upload_id == '' {
		return new_error('InvalidResponse', 'CreateMultipartUploadResult missing UploadId')
	}
	return upload_id
}

// upload_part uploads a single chunk and returns the server-side ETag. Up to
// `Client.retry` attempts with exponential backoff (200ms, 400ms, 800ms, …)
// on transient failures.
//
// The payload is always SHA-256-signed (not `UNSIGNED-PAYLOAD`) so the
// service validates byte-for-byte integrity end-to-end. Multipart uploads
// don't carry a Content-MD5 header by default; without payload signing a
// flipped bit in transit would silently produce a corrupt object that
// passes the multipart ETag check.
pub fn (c &Client) upload_part(key string, upload_id string, part_number int, data []u8, opts PutOptions) !string {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	query := canonical_query_string({
		'partNumber': part_number.str()
		'uploadId':   upload_id
	})
	// Hash the payload once — multi-MiB chunks should not be re-hashed per attempt.
	// The signature itself still has to be recomputed on each retry because the
	// `x-amz-date` header advances.
	payload_hash := sha256_hex(data)
	body := data.bytestr()
	max_attempts := if c.retry < 1 { 1 } else { c.retry }
	mut last_err := IError(new_error('NetworkError', 'upload_part: no attempt was made'))
	for attempt in 0 .. max_attempts {
		signed := sign_request(creds, SignRequest{
			method:       'PUT'
			path:         path
			query:        query
			payload_hash: payload_hash
		})!
		resp := c.do_http(signed, body) or {
			last_err = err
			if attempt + 1 < max_attempts {
				time.sleep((1 << attempt) * 200 * time.millisecond)
			}
			continue
		}
		if resp.status_code == 200 {
			etag := resp.header.get(.etag) or { '' }
			if etag == '' {
				return new_error('InvalidResponse', 'UploadPart returned no ETag')
			}
			return etag.trim('"')
		}
		last_err = new_http_error(resp.status_code, key, resp.body)
		if attempt + 1 < max_attempts {
			time.sleep((1 << attempt) * 200 * time.millisecond)
		}
	}
	return last_err
}

// complete_multipart finalizes a multipart upload. Parts must be in ascending
// `part_number` order; we sort defensively.
pub fn (c &Client) complete_multipart(key string, upload_id string, parts []PartRef, opts PutOptions) ! {
	mut sorted := parts.clone()
	sorted.sort(a.part_number < b.part_number)
	mut body := strings.new_builder(1024)
	body.write_string('<CompleteMultipartUpload xmlns="http://s3.amazonaws.com/doc/2006-03-01/">')
	for p in sorted {
		body.write_string('<Part><PartNumber>${p.part_number}</PartNumber><ETag>"${p.etag}"</ETag></Part>')
	}
	body.write_string('</CompleteMultipartUpload>')
	xml := body.str()
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	q := 'uploadId=' + uri_encode_query(upload_id)
	signed := sign_request(creds, SignRequest{
		method:        'POST'
		path:          path
		query:         q
		payload_hash:  sha256_hex(xml.bytes())
		extra_headers: {
			'content-type': 'application/xml'
		}
	})!
	resp := c.do_http(signed, xml)!
	if resp.status_code != 200 {
		return new_http_error(resp.status_code, key, resp.body)
	}
	// CompleteMultipartUpload may return HTTP 200 with an <Error> body — surface those.
	if resp.body.contains('<Error>') {
		return new_http_error(200, key, resp.body)
	}
}

// abort_multipart cancels an in-flight upload. Best-effort — callers usually
// invoke it inside an error path and don't care about the result.
pub fn (c &Client) abort_multipart(key string, upload_id string, opts PutOptions) ! {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	q := 'uploadId=' + uri_encode_query(upload_id)
	signed := sign_request(creds, SignRequest{
		method:       'DELETE'
		path:         path
		query:        q
		payload_hash: empty_sha256
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code !in [204, 200, 404] {
		return new_http_error(resp.status_code, key, resp.body)
	}
}
