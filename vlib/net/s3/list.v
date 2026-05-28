// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// list returns up to ~1000 objects matching `opts`. Use the returned
// `next_continuation_token` to page through more.
//
// Speaks the ListObjectsV2 protocol.
pub fn (c &Client) list(opts ListOptions) !ListResult {
	bucket := pick_bucket(c.credentials, opts.bucket)!
	creds := c.creds_for(bucket)
	path := bucket_path(creds, bucket)
	mut q := map[string]string{}
	q['list-type'] = '2'
	if opts.prefix != '' {
		q['prefix'] = opts.prefix
	}
	if opts.continuation_token != '' {
		q['continuation-token'] = opts.continuation_token
	}
	if opts.delimiter != '' {
		q['delimiter'] = opts.delimiter
	}
	if opts.max_keys > 0 {
		q['max-keys'] = opts.max_keys.str()
	}
	if opts.start_after != '' {
		q['start-after'] = opts.start_after
	}
	if opts.encoding_type != '' {
		q['encoding-type'] = opts.encoding_type
	}
	if opts.fetch_owner {
		q['fetch-owner'] = 'true'
	}
	query := canonical_query_string(q)
	signed := sign_request(creds, SignRequest{
		method:       'GET'
		path:         path
		query:        query
		payload_hash: empty_sha256
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code != 200 {
		return new_http_error(resp.status_code, bucket, resp.body)
	}
	return parse_list_response(resp.body)
}

// parse_list_response is hand-rolled XML extraction. ListObjectsV2 uses a
// rigid structure with no attributes we care about, so a tag scanner beats
// pulling in the full XML decoder both in code size and predictability
// against malformed input.
pub fn parse_list_response(body string) !ListResult {
	objects := parse_contents(body)
	common := parse_common_prefixes(body)
	max_keys := extract_xml_tag(body, 'MaxKeys').int()
	key_count := extract_xml_tag(body, 'KeyCount').int()
	is_truncated := extract_xml_tag(body, 'IsTruncated') == 'true'
	return ListResult{
		name:                    extract_xml_tag(body, 'Name')
		prefix:                  extract_xml_tag(body, 'Prefix')
		delimiter:               extract_xml_tag(body, 'Delimiter')
		start_after:             extract_xml_tag(body, 'StartAfter')
		max_keys:                max_keys
		key_count:               key_count
		is_truncated:            is_truncated
		continuation_token:      extract_xml_tag(body, 'ContinuationToken')
		next_continuation_token: extract_xml_tag(body, 'NextContinuationToken')
		objects:                 objects
		common_prefixes:         common
	}
}

fn parse_contents(body string) []ObjectInfo {
	mut out := []ObjectInfo{}
	mut start := 0
	for {
		open := body.index_after('<Contents>', start) or { break }
		close := body.index_after('</Contents>', open) or { break }
		seg := body[open + '<Contents>'.len..close]
		mut owner := ?Owner(none)
		owner_open := seg.index('<Owner>') or { -1 }
		if owner_open >= 0 {
			owner_close := seg.index('</Owner>') or { -1 }
			if owner_close > owner_open {
				owner_seg := seg[owner_open + '<Owner>'.len..owner_close]
				owner = Owner{
					id:           extract_xml_tag(owner_seg, 'ID')
					display_name: extract_xml_tag(owner_seg, 'DisplayName')
				}
			}
		}
		out << ObjectInfo{
			key:           extract_xml_tag(seg, 'Key')
			last_modified: extract_xml_tag(seg, 'LastModified')
			etag:          extract_xml_tag(seg, 'ETag').trim('"')
			size:          extract_xml_tag(seg, 'Size').i64()
			storage_class: extract_xml_tag(seg, 'StorageClass')
			owner:         owner
		}
		start = close + '</Contents>'.len
	}
	return out
}

fn parse_common_prefixes(body string) []CommonPrefix {
	mut out := []CommonPrefix{}
	mut start := 0
	for {
		open := body.index_after('<CommonPrefixes>', start) or { break }
		close := body.index_after('</CommonPrefixes>', open) or { break }
		seg := body[open + '<CommonPrefixes>'.len..close]
		out << CommonPrefix{
			prefix: extract_xml_tag(seg, 'Prefix')
		}
		start = close + '</CommonPrefixes>'.len
	}
	return out
}
