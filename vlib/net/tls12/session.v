// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Server-side session-ID cache for TLS 1.2 abbreviated handshakes
// (RFC 5246 §7.3). The cache is process-wide and shared by every listener;
// values are master secrets keyed by session-ID hex. Handshakes sharing a
// process must be serialized by the embedder — the map has no lock because
// sync would drag C linkage into every pure-V consumer; the single-process
// precedent is the TLS 1.3 ticket replay cache (net.tls used_ticket_nonces),
// where the in-repo accept path is likewise serial.
module tls12

import time

// SessionEntry is a cached session: master secret + negotiated suite.
struct SessionEntry {
	master  []u8
	suite   u16
	created i64
}

// max_session_cache_entries bounds the cache for long-lived servers
// (one entry per full handshake; 32-byte IDs + 48-byte masters).
const max_session_cache_entries = 1024

// session_entry_max_age caps how long a cached session is resumable (24h).
const session_entry_max_age = i64(86400)

@[has_globals]
__global session_cache = map[string]SessionEntry{}

// session_cache_lookup returns the cached entry for id, or none when
// missing or expired. Expired entries are dropped on read.
fn session_cache_lookup(id []u8) ?SessionEntry {
	if id.len == 0 {
		return none
	}
	k := id.hex()
	if e := session_cache[k] {
		if time.now().unix() - e.created > session_entry_max_age {
			session_cache.delete(k)
			return none
		}
		return SessionEntry{
			master:  e.master.clone()
			suite:   e.suite
			created: e.created
		}
	}
	return none
}

// session_cache_store inserts id -> (master, suite), evicting expired
// entries first; when still full it clears (bounded memory over replay
// protection, same trade as the TLS 1.3 replay cache: a dropped entry only
// forces a full handshake, never a security failure).
fn session_cache_store(id []u8, master []u8, suite u16) {
	if id.len == 0 || master.len == 0 {
		return
	}
	if session_cache.len >= max_session_cache_entries {
		now := time.now().unix()
		mut stale := []string{}
		for k, e in session_cache {
			if now - e.created > session_entry_max_age {
				stale << k
			}
		}
		for k in stale {
			session_cache.delete(k)
		}
		if session_cache.len >= max_session_cache_entries {
			session_cache = map[string]SessionEntry{}
		}
	}
	session_cache[id.hex()] = SessionEntry{
		master:  master.clone()
		suite:   suite
		created: time.now().unix()
	}
}

// clear_session_cache drops all cached sessions. Tests use it to isolate
// resumption assertions from sessions created by earlier tests.
pub fn clear_session_cache() {
	session_cache = map[string]SessionEntry{}
}
