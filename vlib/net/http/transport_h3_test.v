// vtest build: present_openssl?
// vtest vflags: -d http3
module http

import sync
import time

// Tests for transport_h3.v's own pure/deterministic pieces: pool-key
// folding, the mTLS fast-fail (returns before ever touching the network),
// and the 3-way idle-eviction scan's h3 handling. No live h3 server exists
// in this repo (Phase 13, server support, is out of v1 scope), so a real
// end-to-end `fetch(enable_http3: true)` round trip and the singleflight
// dial machinery (h3_dial_and_do/h3_await_dial/h3_dial_failed) are NOT
// exercised here -- they would need either a real quiche/ngtcp2 peer or
// the same net.quic cross-module fixture-export gap already documented in
// h3_mux_conn_test.v's own module doc comment (12c).

fn test_transport_pool_key_folds_in_enable_http3() {
	req_off := &Request{}
	req_on := &Request{
		enable_http3: true
	}
	key_off := transport_pool_key(req_off, 'https', 'example.com', 443)
	key_on := transport_pool_key(req_on, 'https', 'example.com', 443)
	assert key_off != key_on, 'enable_http3 must change the pool key so h3 and non-h3 requests to the same origin never collide'
}

fn test_transport_pool_key_ignores_enable_http3_for_plain_http() {
	// Mirrors enable_http2's own identical exemption: enable_http3 only
	// affects https dials, so a plain http:// request's pool key must not
	// split on it.
	req_off := &Request{}
	req_on := &Request{
		enable_http3: true
	}
	key_off := transport_pool_key(req_off, 'http', 'example.com', 80)
	key_on := transport_pool_key(req_on, 'http', 'example.com', 80)
	assert key_off == key_on
}

fn test_h3_round_trip_rejects_client_certificate() {
	mut t := new_transport()
	req := &Request{
		enable_http3: true
		cert:         'client.pem'
	}
	t.h3_round_trip(req, 'key', .get, 'example.com', 443, '/', '', new_header()) or {
		assert err.msg().contains('client certificates')
		return
	}
	assert false, 'expected h3_round_trip to reject a request with req.cert set'
}

fn test_h3_round_trip_rejects_client_certificate_key() {
	mut t := new_transport()
	req := &Request{
		enable_http3: true
		cert_key:     'client.key'
	}
	t.h3_round_trip(req, 'key', .get, 'example.com', 443, '/', '', new_header()) or {
		assert err.msg().contains('client certificates')
		return
	}
	assert false, 'expected h3_round_trip to reject a request with req.cert_key set'
}

// new_test_h3_mux_conn_for_pool_test builds a bare H3MuxConn suitable only
// for exercising Transport's own pool bookkeeping (evict_oldest_idle_
// locked, is_idle_expired, shutdown_when_idle, release) -- no driver
// thread, no transport, no h3. Duplicated locally rather than shared with
// h3_mux_conn_test.v's own near-identical helper: each `_test.v` file
// compiles as an independent unit and does not share symbols with sibling
// test files (established convention, documented in conn_test.v's own
// header).
fn new_test_h3_mux_conn_for_pool_test() &H3MuxConn {
	return &H3MuxConn{
		transport:  unsafe { nil }
		qmu:        sync.new_mutex()
		idle_since: time.now()
	}
}

// The idle cap is a single shared budget across h1, h2, AND h3 (Phase 12c
// extended evict_oldest_idle_locked's own scan to a 3-way one; this test
// exercises the h3 side of it directly, without a live dial). With max_
// idle_conns already consumed by one busy h2-shaped budget slot (simulated
// here by simply setting max_idle_conns below the h3 count), the oldest
// idle h3 entry must be the one evicted.
fn test_evict_oldest_idle_locked_evicts_the_oldest_idle_h3_conn() {
	mut t := new_transport()
	t.max_idle_conns = 1

	mut older := new_test_h3_mux_conn_for_pool_test()
	older.idle_since = time.now().add(-1 * time.hour)
	mut newer := new_test_h3_mux_conn_for_pool_test()
	newer.idle_since = time.now()

	t.mu.lock()
	t.h3_conns['a'] = older
	t.h3_conns['b'] = newer
	evicted := t.evict_oldest_idle_locked('', '')
	t.mu.unlock()

	assert evicted.h3 != unsafe { nil }
	assert voidptr(evicted.h3) == voidptr(older), 'expected the OLDER idle h3 connection to be evicted, not the newer one'
	assert 'a' !in t.h3_conns
	assert 'b' in t.h3_conns
}

// just_registered_h3_key excludes a brand-new h3_conns entry from its own
// eviction candidacy -- mirrors just_registered_h2_key's identical guard
// (Codex P1, vlang/v#27643 pullrequestreview-4628439062): a fresh dial's
// own entry has active_streams == 0 (do_h3 hasn't run yet), making it look
// idle enough to be picked as its own victim if not excluded.
fn test_evict_oldest_idle_locked_excludes_just_registered_h3_key() {
	mut t := new_transport()
	t.max_idle_conns = 0 // force eviction to trigger regardless of count

	mut only := new_test_h3_mux_conn_for_pool_test()
	only.idle_since = time.now().add(-1 * time.hour)

	t.mu.lock()
	t.h3_conns['fresh'] = only
	evicted := t.evict_oldest_idle_locked('', 'fresh')
	t.mu.unlock()

	assert evicted.h3 == unsafe { nil }, 'the just-registered key must never be its own eviction victim'
	assert 'fresh' in t.h3_conns
}

// A busy h3 connection (active_streams > 0) must never be evicted to make
// room -- mirrors the identical h2 guarantee (Codex P3, vlang/v#27643
// pullrequestreview-4631763931).
fn test_evict_oldest_idle_locked_never_evicts_a_busy_h3_conn() {
	mut t := new_transport()
	t.max_idle_conns = 0

	mut busy := new_test_h3_mux_conn_for_pool_test()
	busy.active_streams = 1
	busy.idle_since = time.now().add(-1 * time.hour)

	t.mu.lock()
	t.h3_conns['busy'] = busy
	evicted := t.evict_oldest_idle_locked('', '')
	t.mu.unlock()

	assert evicted.h3 == unsafe { nil }
	assert 'busy' in t.h3_conns
}
