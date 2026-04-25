module http

import sync
import time

// AltSvcEntry represents a single Alt-Svc alternative service entry per RFC 7838.
pub struct AltSvcEntry {
pub:
	protocol string // e.g., "h3", "h3-29", "h2"
	host     string // alternative host (empty = same host)
	port     u16    // alternative port
	max_age  u64    // seconds, default 86400
	persist  bool   // persist across network changes
}

// CachedAltSvc wraps an AltSvcEntry with origin and expiry metadata.
struct CachedAltSvc {
	entry   AltSvcEntry
	origin  string
	expires time.Time
}

// AltSvcCache is a thread-safe cache of Alt-Svc entries keyed by origin.
pub struct AltSvcCache {
mut:
	entries map[string][]CachedAltSvc
	mu      &sync.Mutex = sync.new_mutex()
}

// new_alt_svc_cache creates a new heap-allocated AltSvcCache.
pub fn new_alt_svc_cache() &AltSvcCache {
	return &AltSvcCache{
		entries: map[string][]CachedAltSvc{}
	}
}

// parse_alt_svc parses an Alt-Svc header value per RFC 7838 §3.
// Returns an empty array for "clear" or invalid input.
pub fn parse_alt_svc(header_value string) []AltSvcEntry {
	trimmed := header_value.trim_space()
	if trimmed == 'clear' || trimmed.len == 0 {
		return []
	}
	mut result := []AltSvcEntry{}
	raw_entries := split_entries(trimmed)
	for raw in raw_entries {
		if entry := parse_single_entry(raw.trim_space()) {
			result << entry
		}
	}
	return result
}

// split_entries splits the header on commas that are outside quoted strings.
fn split_entries(s string) []string {
	mut parts := []string{}
	mut start := 0
	mut in_quotes := false
	for i := 0; i < s.len; i++ {
		ch := s[i]
		if ch == `"` {
			in_quotes = !in_quotes
		} else if ch == `,` && !in_quotes {
			parts << s[start..i]
			start = i + 1
		}
	}
	if start < s.len {
		parts << s[start..]
	}
	return parts
}

// parse_single_entry parses one "protocol=authority; params" segment.
fn parse_single_entry(s string) ?AltSvcEntry {
	eq_pos := s.index('=') or { return none }
	protocol := s[..eq_pos].trim_space()
	if protocol.len == 0 {
		return none
	}
	rest := s[eq_pos + 1..]
	parts := split_on_semicolons(rest)
	if parts.len == 0 {
		return none
	}
	authority := parts[0].trim_space().replace('"', '')
	host, port := parse_authority(authority)
	mut max_age := u64(86400)
	mut persist := false
	for i := 1; i < parts.len; i++ {
		param := parts[i].trim_space()
		if param.starts_with('ma=') {
			max_age = param[3..].trim_space().u64()
		} else if param.starts_with('persist=') {
			persist = param[8..].trim_space() == '1'
		}
	}
	return AltSvcEntry{
		protocol: protocol
		host:     host
		port:     u16(port)
		max_age:  max_age
		persist:  persist
	}
}

// split_on_semicolons splits on semicolons outside quoted strings.
fn split_on_semicolons(s string) []string {
	mut parts := []string{}
	mut start := 0
	mut in_quotes := false
	for i := 0; i < s.len; i++ {
		ch := s[i]
		if ch == `"` {
			in_quotes = !in_quotes
		} else if ch == `;` && !in_quotes {
			parts << s[start..i]
			start = i + 1
		}
	}
	if start < s.len {
		parts << s[start..]
	}
	return parts
}

// parse_authority extracts host and port from "host:port" or ":port".
fn parse_authority(authority string) (string, int) {
	colon := authority.last_index(':') or { return authority, 0 }
	host := authority[..colon]
	port := authority[colon + 1..].int()
	if port < 1 || port > 65535 {
		return host, 0
	}
	return host, port
}

// store stores Alt-Svc entries for an origin with computed expiry (thread-safe).
pub fn (mut c AltSvcCache) store(origin string, entries []AltSvcEntry) {
	now := time.now()
	mut cached := []CachedAltSvc{}
	for entry in entries {
		cached << CachedAltSvc{
			entry:   entry
			origin:  origin
			expires: now.add(i64(entry.max_age) * time.second)
		}
	}
	c.mu.lock()
	c.entries[origin] = cached
	c.mu.unlock()
}

// get_h3_endpoint returns the best HTTP/3 Alt-Svc entry for an origin, if any.
pub fn (mut c AltSvcCache) get_h3_endpoint(origin string) ?AltSvcEntry {
	c.mu.lock()
	cached_list := c.entries[origin] or {
		c.mu.unlock()
		return none
	}
	now := time.now()
	for cached in cached_list {
		if cached.entry.protocol.starts_with('h3') && now < cached.expires {
			c.mu.unlock()
			return cached.entry
		}
	}
	c.mu.unlock()
	return none
}

// clear removes all cached Alt-Svc entries for an origin (thread-safe).
pub fn (mut c AltSvcCache) clear(origin string) {
	c.mu.lock()
	c.entries.delete(origin)
	c.mu.unlock()
}

// cleanup removes all expired entries from the cache (thread-safe).
pub fn (mut c AltSvcCache) cleanup() {
	now := time.now()
	c.mu.lock()
	for origin, cached_list in c.entries {
		mut valid := []CachedAltSvc{}
		for cached in cached_list {
			if now < cached.expires {
				valid << cached
			}
		}
		if valid.len == 0 {
			c.entries.delete(origin)
		} else {
			c.entries[origin] = valid
		}
	}
	c.mu.unlock()
}
