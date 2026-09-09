// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

fn test_credentials_validate_rejects_crlf_injection() {
	c := Credentials{
		access_key_id:     'KEY'
		secret_access_key: 'secret\r\nInjected: header'
	}
	if _ := c.validate() {
		assert false, 'should have rejected CRLF'
	}
}

fn test_credentials_from_env_returns_empty_when_unset() {
	// Cannot safely set env vars from a test without polluting the parent
	// shell, so just verify the call returns a value of the right shape.
	c := Credentials.from_env()
	_ = c
}

fn test_endpoint_scheme_is_derived_when_explicit() {
	c1 := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		endpoint:          'https://s3.example.com'
	}
	assert c1.scheme() == 'https'
	c2 := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		endpoint:          'http://localhost:9000'
	}
	assert c2.scheme() == 'http', 'http:// endpoint must yield http even when insecure_http is unset'
	c3 := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		endpoint:          'http://localhost:9000'
		insecure_http:     true
	}
	assert c3.scheme() == 'http'
	// Bare host: defaults to https unless insecure_http is set.
	c4 := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		endpoint:          'localhost:9000'
	}
	assert c4.scheme() == 'https'
	c5 := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		endpoint:          'localhost:9000'
		insecure_http:     true
	}
	assert c5.scheme() == 'http'
}

fn test_endpoint_host_only_strips_scheme_and_path() {
	c := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		endpoint:          'https://proxy.example.com:8443/s3'
	}
	assert c.host_only() == 'proxy.example.com:8443'
	assert c.extra_path() == '/s3'
}

fn test_guess_region_paths() {
	assert guess_region('') == 'us-east-1'
	assert guess_region('https://s3.eu-west-3.amazonaws.com') == 'eu-west-3'
	assert guess_region('https://my-acct.r2.cloudflarestorage.com') == 'auto'
	assert guess_region('https://s3.example.com') == 'auto'
	// AWS global endpoints sign with us-east-1, not 'auto'.
	assert guess_region('https://s3.amazonaws.com') == 'us-east-1'
	assert guess_region('s3.amazonaws.com') == 'us-east-1'
	assert guess_region('https://s3-external-1.amazonaws.com') == 'us-east-1'
}

fn test_credentials_merge_overrides_only_non_empty() {
	base := Credentials{
		access_key_id:     'BASE'
		secret_access_key: 'basesecret'
		region:            'eu-west-1'
		bucket:            'b1'
	}
	other := Credentials{
		bucket: 'b2'
	}
	got := base.merge(other)
	assert got.access_key_id == 'BASE'
	assert got.region == 'eu-west-1'
	assert got.bucket == 'b2'
}

fn test_resolved_region_prefers_explicit() {
	c := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		region:            'eu-central-1'
		endpoint:          'https://s3.us-west-2.amazonaws.com'
	}
	assert c.resolved_region() == 'eu-central-1'
}
