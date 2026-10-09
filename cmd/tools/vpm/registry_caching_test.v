module main

import os

fn caching_demo_info(name string, version string) ModuleInfo {
	return ModuleInfo{
		name:         name
		version:      version
		description:  'demo'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'sha256:deadbeef'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	}
}

fn new_cache_demo_registry() Registry {
	mut r := new_registry()
	r.add_module(caching_demo_info('alpha', '1.0.0'))
	return r
}

fn test_etag_is_stable_for_the_same_body() {
	assert etag_of('{"a":1}') == etag_of('{"a":1}')
}

fn test_etag_separates_different_bodies() {
	assert etag_of('{"a":1}') != etag_of('{"a":2}')
	assert etag_of('') != etag_of('a')
}

// A bare digest would not be a valid entity tag: the HTTP grammar wraps it in
// double quotes, and a client comparing against an unquoted value would never
// match.
fn test_etag_is_quoted_per_the_http_grammar() {
	tag := etag_of('body')
	assert tag.len >= 2
	assert tag[0] == `"`, 'entity tag does not start with a quote: `${tag}`'
	assert tag[tag.len - 1] == `"`, 'entity tag does not end with a quote: `${tag}`'
	assert !tag[1..tag.len - 1].contains('"'), 'the digest itself contains a quote'
}

fn test_no_conditional_header_sends_the_body() {
	r := new_cache_demo_registry()
	resp := serve(r, 'GET', '/alpha/@latest', map[string]string{}, map[string]string{})
	assert resp.status_code == 200
	assert resp.body.len > 0
	assert resp.etag == etag_of(resp.body)
}

// The point of the entity tag: an unchanged registry costs a header exchange.
fn test_a_matching_if_none_match_gets_304_and_no_body() {
	r := new_cache_demo_registry()
	first := serve(r, 'GET', '/alpha/@latest', map[string]string{}, map[string]string{})
	second := serve(r, 'GET', '/alpha/@latest', map[string]string{}, {
		'If-None-Match': first.etag
	})
	assert second.status_code == 304
	assert second.body == '', 'a 304 carried a body'
	assert second.etag == first.etag
}

fn test_a_stale_if_none_match_gets_the_body_again() {
	r := new_cache_demo_registry()
	stale := etag_of('{"something":"else"}')
	resp := serve(r, 'GET', '/alpha/@latest', map[string]string{}, {
		'If-None-Match': stale
	})
	assert resp.status_code == 200
	assert resp.body.len > 0
}

// Editing the registry changes the body, so a client still holding the old tag
// must be sent the new data rather than told its copy is current.
fn test_publishing_a_new_version_invalidates_the_old_tag() {
	mut r := new_cache_demo_registry()
	first := serve(r, 'GET', '/alpha/@latest', map[string]string{}, map[string]string{})
	r.add_module(caching_demo_info('alpha', '2.0.0'))
	second := serve(r, 'GET', '/alpha/@latest', map[string]string{}, {
		'If-None-Match': first.etag
	})
	assert second.status_code == 200
	assert second.body.contains('2.0.0'), second.body
	assert second.etag != first.etag
}

fn test_an_asterisk_asks_for_whatever_is_current() {
	assert etag_matches('*', etag_of('anything'))
	assert !etag_matches('', etag_of('anything'))
}

// A client may send several tags separated by commas.
fn test_a_comma_separated_list_matches_any_held_tag() {
	want := etag_of('wanted')
	other := etag_of('other')
	assert etag_matches('${other}, ${want}', want)
	assert etag_matches('${other}', want) == false
}

// `W/` marks a weak validator, which still compares equal to the strong tag
// for the same body.
fn test_weak_validator_prefix_matches() {
	tag := etag_of('wanted')
	assert etag_matches('W/${tag}', tag)
	assert etag_matches('W/${tag}', etag_of('other')) == false
}

fn test_malformed_tags_do_not_match() {
	assert etag_matches('not-a-tag', etag_of('body')) == false
	assert etag_matches('"unterminated', etag_of('body')) == false
}
