// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import net.http

// ACL is the canned Access Control List applied to a stored object or bucket.
// Values match the S3 wire protocol.
pub enum Acl {
	unset
	private
	public_read
	public_read_write
	aws_exec_read
	authenticated_read
	bucket_owner_read
	bucket_owner_full_control
	log_delivery_write
}

// to_header_value renders the ACL as the canonical S3 string used in headers
// and presigned query parameters. `.unset` returns an empty string so callers
// can skip the header.
pub fn (a Acl) to_header_value() string {
	return match a {
		.unset { '' }
		.private { 'private' }
		.public_read { 'public-read' }
		.public_read_write { 'public-read-write' }
		.aws_exec_read { 'aws-exec-read' }
		.authenticated_read { 'authenticated-read' }
		.bucket_owner_read { 'bucket-owner-read' }
		.bucket_owner_full_control { 'bucket-owner-full-control' }
		.log_delivery_write { 'log-delivery-write' }
	}
}

// parse_acl returns the matching Acl from a wire string.
// Returns `.unset` when the input is empty or unknown so the caller can ignore it.
pub fn parse_acl(s string) Acl {
	return match s {
		'private' { Acl.private }
		'public-read' { Acl.public_read }
		'public-read-write' { Acl.public_read_write }
		'aws-exec-read' { Acl.aws_exec_read }
		'authenticated-read' { Acl.authenticated_read }
		'bucket-owner-read' { Acl.bucket_owner_read }
		'bucket-owner-full-control' { Acl.bucket_owner_full_control }
		'log-delivery-write' { Acl.log_delivery_write }
		else { Acl.unset }
	}
}

// StorageClass enumerates the standard S3 storage tiers. `.unset` means the
// server default (typically STANDARD) and produces no `x-amz-storage-class`
// header. Providers vary on which tiers they actually honour.
pub enum StorageClass {
	unset
	standard
	deep_archive
	express_onezone
	glacier
	glacier_ir
	intelligent_tiering
	onezone_ia
	outposts
	reduced_redundancy
	snow
	standard_ia
}

// to_header_value renders the storage class as the on-wire string.
pub fn (sc StorageClass) to_header_value() string {
	return match sc {
		.unset { '' }
		.standard { 'STANDARD' }
		.deep_archive { 'DEEP_ARCHIVE' }
		.express_onezone { 'EXPRESS_ONEZONE' }
		.glacier { 'GLACIER' }
		.glacier_ir { 'GLACIER_IR' }
		.intelligent_tiering { 'INTELLIGENT_TIERING' }
		.onezone_ia { 'ONEZONE_IA' }
		.outposts { 'OUTPOSTS' }
		.reduced_redundancy { 'REDUCED_REDUNDANCY' }
		.snow { 'SNOW' }
		.standard_ia { 'STANDARD_IA' }
	}
}

// Stat is the result of HEAD-ing an object.
pub struct Stat {
pub:
	size          i64    // Content-Length in bytes
	last_modified string // RFC 1123 date as returned by S3
	etag          string // unquoted ETag (server returns it wrapped in quotes; we strip them)
	content_type  string // MIME type
}

// Owner identifies an S3 object owner — only populated when `fetch_owner` is true.
pub struct Owner {
pub:
	id           string
	display_name string
}

// ObjectInfo is one entry from a ListObjectsV2 response.
pub struct ObjectInfo {
pub:
	key           string
	last_modified string
	etag          string
	size          i64
	storage_class string
	owner         ?Owner
}

// CommonPrefix represents a "directory-like" prefix in a list result when a
// delimiter is provided.
pub struct CommonPrefix {
pub:
	prefix string
}

// ListResult aggregates a ListObjectsV2 response.
// NextContinuationToken should be passed back as `continuation_token` to fetch the next page.
pub struct ListResult {
pub:
	name                    string
	prefix                  string
	delimiter               string
	start_after             string
	max_keys                int
	key_count               int
	is_truncated            bool
	continuation_token      string
	next_continuation_token string
	objects                 []ObjectInfo
	common_prefixes         []CommonPrefix
}

// ListOptions configures a ListObjectsV2 call. `bucket` overrides the client's
// default; leave empty to use the client's bound bucket.
@[params]
pub struct ListOptions {
pub:
	bucket             string
	prefix             string
	continuation_token string
	delimiter          string
	max_keys           int // 0 means default (server picks 1000)
	start_after        string
	encoding_type      string // 'url' or empty
	fetch_owner        bool
}

// PresignOptions controls presigned URL generation.
@[params]
pub struct PresignOptions {
pub:
	bucket              string // overrides the client's default bucket for this call
	method              http.Method = .get
	expires_in          int         = 86400 // seconds, 1..604800
	acl                 Acl
	storage_class       StorageClass
	content_type        string
	content_disposition string
	request_payer       bool
}
