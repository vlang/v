// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import os

// Credentials carries the authentication material plus endpoint and addressing
// preferences. It is intentionally kept small; defaults (region, endpoint, etc.)
// are derived only when the request is signed, never stored implicitly, so the
// same Credentials value can be reused across regions/endpoints.
//
// Field naming matches V conventions (snake_case). The `from_env` helper
// recognises several provider conventions — see `from_env`.
pub struct Credentials {
pub:
	access_key_id        string
	secret_access_key    string
	session_token        string
	region               string
	bucket               string
	endpoint             string // 'https://s3.fr-par.scw.cloud' or 'host:port' — host part is what gets signed
	virtual_hosted_style bool   // when true, '<bucket>.<endpoint-host>' addressing
	insecure_http        bool   // permit `http://` endpoints (false by default — never silently downgrades)
}

// Credentials.from_env reads credentials from environment variables, trying several
// provider conventions in order so the same code works against many hosts
// without reconfiguration. Each field is resolved independently; the first
// non-empty value wins.
//
// Lookup order per field:
//   key id     : S3_ACCESS_KEY_ID, AWS_ACCESS_KEY_ID,
//                CELLAR_ADDON_KEY_ID, SCW_ACCESS_KEY,
//                B2_APPLICATION_KEY_ID, R2_ACCESS_KEY_ID, SPACES_KEY
//   secret     : S3_SECRET_ACCESS_KEY, AWS_SECRET_ACCESS_KEY,
//                CELLAR_ADDON_KEY_SECRET, SCW_SECRET_KEY,
//                B2_APPLICATION_KEY, R2_SECRET_ACCESS_KEY, SPACES_SECRET
//   session    : S3_SESSION_TOKEN, AWS_SESSION_TOKEN
//   region     : S3_REGION, AWS_REGION, AWS_DEFAULT_REGION, SCW_DEFAULT_REGION
//   bucket     : S3_BUCKET
//   endpoint   : S3_ENDPOINT, AWS_ENDPOINT, AWS_ENDPOINT_URL,
//                CELLAR_ADDON_HOST, B2_ENDPOINT, R2_ENDPOINT, SPACES_ENDPOINT
pub fn Credentials.from_env() Credentials {
	endpoint := env_first('S3_ENDPOINT', 'AWS_ENDPOINT', 'AWS_ENDPOINT_URL', 'CELLAR_ADDON_HOST',
		'B2_ENDPOINT', 'R2_ENDPOINT', 'SPACES_ENDPOINT')
	insecure := endpoint.starts_with('http://')
	return Credentials{
		access_key_id:        env_first('S3_ACCESS_KEY_ID', 'AWS_ACCESS_KEY_ID',
			'CELLAR_ADDON_KEY_ID', 'SCW_ACCESS_KEY', 'B2_APPLICATION_KEY_ID', 'R2_ACCESS_KEY_ID',
			'SPACES_KEY')
		secret_access_key:    env_first('S3_SECRET_ACCESS_KEY', 'AWS_SECRET_ACCESS_KEY',
			'CELLAR_ADDON_KEY_SECRET', 'SCW_SECRET_KEY', 'B2_APPLICATION_KEY',
			'R2_SECRET_ACCESS_KEY', 'SPACES_SECRET')
		session_token:        env_first('S3_SESSION_TOKEN', 'AWS_SESSION_TOKEN')
		region:               env_first('S3_REGION', 'AWS_REGION', 'AWS_DEFAULT_REGION',
			'SCW_DEFAULT_REGION')
		bucket:               env_first('S3_BUCKET')
		endpoint:             endpoint
		virtual_hosted_style: false
		insecure_http:        insecure
	}
}

// merge produces a copy of `c` with non-empty fields from `other` overriding.
// Useful when callers pass per-call overrides while keeping a default Client.
pub fn (c Credentials) merge(other Credentials) Credentials {
	return Credentials{
		access_key_id:        if other.access_key_id != '' {
			other.access_key_id
		} else {
			c.access_key_id
		}
		secret_access_key:    if other.secret_access_key != '' {
			other.secret_access_key
		} else {
			c.secret_access_key
		}
		session_token:        if other.session_token != '' {
			other.session_token
		} else {
			c.session_token
		}
		region:               if other.region != '' { other.region } else { c.region }
		bucket:               if other.bucket != '' { other.bucket } else { c.bucket }
		endpoint:             if other.endpoint != '' { other.endpoint } else { c.endpoint }
		virtual_hosted_style: c.virtual_hosted_style || other.virtual_hosted_style
		insecure_http:        c.insecure_http || other.insecure_http
	}
}

// resolved_region returns the region to use for signing. Order:
//   1. explicit `c.region`
//   2. parsed from `c.endpoint` when it follows the `s3.<region>.amazonaws.com` pattern
//   3. `'auto'` for Cloudflare R2
//   4. `'us-east-1'` (S3 historical default) when no endpoint is set
pub fn (c Credentials) resolved_region() string {
	if c.region != '' {
		return c.region
	}
	return guess_region(c.endpoint)
}

// validate ensures the credentials carry the minimum needed to sign a
// request. Also rejects credentials / region / bucket / endpoint values that
// contain CR or LF — those would let an attacker who controls *any* config
// field smuggle headers into the Authorization line.
pub fn (c Credentials) validate() ! {
	if c.access_key_id == '' || c.secret_access_key == '' {
		return new_error('MissingCredentials',
			"Missing S3 credentials. 'access_key_id' and 'secret_access_key' are required.")
	}
	for name, value in {
		'access_key_id':     c.access_key_id
		'secret_access_key': c.secret_access_key
		'session_token':     c.session_token
		'region':            c.region
		'bucket':            c.bucket
		'endpoint':          c.endpoint
	} {
		if contains_crlf(value) {
			return new_error('InvalidCredentials',
				'Credential field "${name}" contains CR or LF — refused (header-injection guard)')
		}
	}
}

// host_only returns the bare host[:port] from `c.endpoint`, stripping any
// scheme and trailing path. Returned value is what gets signed in the
// `host` header for SigV4.
pub fn (c Credentials) host_only() string {
	mut ep := c.endpoint
	if i := ep.index('://') {
		ep = ep[i + 3..]
	}
	if j := ep.index('/') {
		ep = ep[..j]
	}
	return ep
}

// extra_path returns the path component of `c.endpoint`, including any
// leading '/'. Useful for proxies that mount S3 under a sub-path.
pub fn (c Credentials) extra_path() string {
	mut ep := c.endpoint
	if i := ep.index('://') {
		ep = ep[i + 3..]
	}
	if j := ep.index('/') {
		return ep[j..]
	}
	return ''
}

// scheme returns 'http' or 'https' based on the endpoint's explicit scheme
// when present, falling back to `insecure_http`. So all three of these work:
//   endpoint: 'https://s3.example.com'   → https
//   endpoint: 's3.example.com'           → https (default)
//   endpoint: 'http://localhost:9000'    → http  (auto-detected)
//   endpoint: 'localhost:9000', insecure_http: true → http
pub fn (c Credentials) scheme() string {
	if c.endpoint.starts_with('http://') {
		return 'http'
	}
	if c.endpoint.starts_with('https://') {
		return 'https'
	}
	return if c.insecure_http { 'http' } else { 'https' }
}

// guess_region derives the SigV4 region from an endpoint URL. Public so it
// can be reused by the higher-level Client for log / inspect output.
pub fn guess_region(endpoint string) string {
	if endpoint == '' {
		return 'us-east-1'
	}
	if endpoint.ends_with('.r2.cloudflarestorage.com') {
		return 'auto'
	}
	if amz_end := endpoint.index('.amazonaws.com') {
		if s3_pos := endpoint.index('s3.') {
			start := s3_pos + 3
			if start < amz_end {
				return endpoint[start..amz_end]
			}
		}
	}
	return 'auto'
}

// env_first returns the first non-empty env var among the names provided.
fn env_first(names ...string) string {
	for n in names {
		v := os.getenv(n)
		if v != '' {
			return v
		}
	}
	return ''
}
