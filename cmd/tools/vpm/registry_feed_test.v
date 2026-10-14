module main

import json2
import crypto.ed25519
import os
import time

fn feed_demo_info(name string, version string) ModuleInfo {
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

// A registry whose change log holds `module<i>` at each of `times`. The
// timestamps are fixed rather than taken from the clock, because `.unix()`
// truncates to seconds and every change made inside a test run lands in the
// same one, which makes `since` untestable against a wall-clock mark.
fn registry_with_changes_at(times []i64) Registry {
	mut r := new_registry()
	for i, ts in times {
		r.changes << Change{
			module:      'module${i}'
			version:     '1.0.0'
			kind:        'publish'
			occurred_at: time.unix(ts).format_rfc3339()
		}
	}
	return r
}

fn testsuite_begin() {
	os.unsetenv('VPM_REGISTRY_KEY')
}

fn testsuite_end() {
	os.unsetenv('VPM_REGISTRY_KEY')
}

fn test_publishing_records_a_change() {
	mut r := new_registry()
	r.add_module(feed_demo_info('alpha', '1.0.0'))
	assert r.changes.len == 1
	assert r.changes[0].module == 'alpha'
	assert r.changes[0].version == '1.0.0'
	assert r.changes[0].kind == 'publish'
	assert r.changes[0].occurred_at != ''
}

fn test_yanking_records_a_change_of_kind_not_of_content() {
	mut r := new_registry()
	r.add_module(feed_demo_info('alpha', '1.0.0'))
	assert r.yank('alpha', '1.0.0')
	assert r.changes.len == 2
	assert r.changes[1].kind == 'yank'
	assert r.unyank('alpha', '1.0.0')
	assert r.changes.len == 3
	assert r.changes[2].kind == 'unyank'
}

// Yanking something that is not there must not leave a phantom entry, or a
// mirror would be told about a change that never happened.
fn test_yanking_an_absent_version_records_nothing() {
	mut r := new_registry()
	r.add_module(feed_demo_info('alpha', '1.0.0'))
	assert r.yank('alpha', '9.9.9') == false
	assert r.changes.len == 1
	assert r.yank('missing', '1.0.0') == false
	assert r.changes.len == 1
}

fn test_changes_since_returns_what_came_after_the_mark() {
	r := registry_with_changes_at([1000, 2000, 3000])
	after := r.changes_since(2000)
	assert after.len == 2, 'expected 2 changes after the mark, got ${after.len}'
	assert after[0].module == 'module1'
	assert after[1].module == 'module2'
}

// The mark itself is included: a mirror that read up to second N must be sent a
// change stamped N, or it would silently miss it.
fn test_a_change_stamped_exactly_at_the_mark_is_served() {
	r := registry_with_changes_at([1000, 2000])
	assert r.changes_since(2000).len == 1
}

fn test_a_mark_in_the_future_serves_nothing() {
	r := registry_with_changes_at([1000, 2000])
	assert r.changes_since(5000).len == 0
}

// No mark means everything, which is what a mirror with no copy needs.
fn test_a_mark_of_zero_serves_everything() {
	r := registry_with_changes_at([1000, 2000, 3000])
	assert r.changes_since(0).len == 3
}

// The signature must not move when the change log grows, or appending to the
// log would invalidate every signature the registry has already published.
fn test_an_identical_registry_with_a_different_log_signs_the_same() {
	_public, private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	mut a := new_registry()
	a.add_module(feed_demo_info('alpha', '1.0.0'))
	mut b := new_registry()
	b.add_module(feed_demo_info('alpha', '1.0.0'))
	// Give b a longer history without touching its module metadata.
	b.record_change('note', 'alpha', '1.0.0')
	assert a.changes.len == 1
	assert b.changes.len == 2
	assert a.signature() == b.signature(), 'the change log changed the signature'
	assert a.verify_signature(a.public_key_hex(), b.signature()), 'a signature over one registry did not verify against an identical one'
}

// An unparseable timestamp is skipped rather than reported, so a mirror asking
// about a bad entry is told nothing happened instead of failing.
fn test_an_unparseable_timestamp_is_skipped() {
	mut r := new_registry()
	r.changes << Change{
		module:      'alpha'
		version:     '1.0.0'
		kind:        'publish'
		occurred_at: 'not-a-timestamp'
	}
	assert r.changes_since(0).len == 0
}

fn test_the_changes_endpoint_answers_only_what_changed() {
	r := registry_with_changes_at([1000, 2000, 3000])
	body := handle_request(r, 'GET', '/api/changes', {
		'since': '2000'
	})
	changes := json2.decode[[]Change](body) or {
		assert false, 'could not decode the change feed: ${err}'
		return
	}
	assert changes.len == 2, changes.len.str()
	assert changes[0].module == 'module1'
	assert changes[1].module == 'module2'
}

// No `since` means "everything since the beginning", which is what a mirror
// with no copy at all needs.
fn test_the_changes_endpoint_without_a_mark_serves_everything() {
	r := registry_with_changes_at([1000, 2000])
	body := handle_request(r, 'GET', '/api/changes', map[string]string{})
	changes := json2.decode[[]Change](body) or {
		assert false, 'could not decode the change feed: ${err}'
		return
	}
	assert changes.len == 2
}

// A garbage `since` is treated as "beginning", never as an error, so a mirror
// sending a malformed value still gets a usable answer.
fn test_the_changes_endpoint_with_a_garbage_mark_serves_everything() {
	r := registry_with_changes_at([1000, 2000])
	body := handle_request(r, 'GET', '/api/changes', {
		'since': 'not-a-number'
	})
	changes := json2.decode[[]Change](body) or {
		assert false, 'could not decode the change feed: ${err}'
		return
	}
	assert changes.len == 2
}
