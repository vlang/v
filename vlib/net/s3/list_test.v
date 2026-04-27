// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// Captured from a real ListObjectsV2 response (S3-compatible endpoint),
// trimmed to the fields we parse.
const list_response_xml = '<?xml version="1.0" encoding="UTF-8"?>
<ListBucketResult>
  <Name>test-bucket</Name>
  <Prefix>foo/</Prefix>
  <KeyCount>2</KeyCount>
  <MaxKeys>1000</MaxKeys>
  <IsTruncated>false</IsTruncated>
  <Contents>
    <Key>foo/a.txt</Key>
    <Size>10</Size>
    <ETag>&quot;d41d8cd98f00b204e9800998ecf8427e&quot;</ETag>
    <LastModified>2024-01-15T00:00:00.000Z</LastModified>
    <StorageClass>STANDARD</StorageClass>
  </Contents>
  <Contents>
    <Key>foo/b.txt</Key>
    <Size>20</Size>
    <ETag>&quot;9b8b80d22b1d56a3f9b2c25d6e3e74c5&quot;</ETag>
    <LastModified>2024-01-16T00:00:00.000Z</LastModified>
  </Contents>
</ListBucketResult>'

fn test_parse_list_response_basic() {
	r := parse_list_response(list_response_xml) or { panic(err) }
	assert r.name == 'test-bucket'
	assert r.prefix == 'foo/'
	assert r.key_count == 2
	assert !r.is_truncated
	assert r.objects.len == 2
	assert r.objects[0].key == 'foo/a.txt'
	assert r.objects[0].size == 10
	assert r.objects[0].etag == 'd41d8cd98f00b204e9800998ecf8427e'
	assert r.objects[1].key == 'foo/b.txt'
}

fn test_parse_list_response_truncated() {
	body := '<?xml version="1.0"?><ListBucketResult><Name>b</Name><IsTruncated>true</IsTruncated><NextContinuationToken>opaqueToken==</NextContinuationToken><KeyCount>1</KeyCount><Contents><Key>only.txt</Key><Size>1</Size><ETag>&quot;abc&quot;</ETag></Contents></ListBucketResult>'
	r := parse_list_response(body) or { panic(err) }
	assert r.is_truncated
	assert r.next_continuation_token == 'opaqueToken=='
}

fn test_parse_list_response_common_prefixes() {
	body := '<?xml version="1.0"?><ListBucketResult><Name>b</Name><Delimiter>/</Delimiter><CommonPrefixes><Prefix>folder1/</Prefix></CommonPrefixes><CommonPrefixes><Prefix>folder2/</Prefix></CommonPrefixes><KeyCount>0</KeyCount></ListBucketResult>'
	r := parse_list_response(body) or { panic(err) }
	assert r.delimiter == '/'
	assert r.common_prefixes.len == 2
	assert r.common_prefixes[0].prefix == 'folder1/'
	assert r.common_prefixes[1].prefix == 'folder2/'
}
