module http

import time

fn test_parse_alt_svc_h3_simple() {
	entries := parse_alt_svc('h3=":443"')
	assert entries.len == 1
	assert entries[0].protocol == 'h3'
	assert entries[0].host == ''
	assert entries[0].port == 443
	assert entries[0].max_age == 86400
	assert entries[0].persist == false
}

fn test_parse_alt_svc_with_host() {
	entries := parse_alt_svc('h3="alt.example.com:8443"')
	assert entries.len == 1
	assert entries[0].protocol == 'h3'
	assert entries[0].host == 'alt.example.com'
	assert entries[0].port == 8443
	assert entries[0].max_age == 86400
}

fn test_parse_alt_svc_with_max_age() {
	entries := parse_alt_svc('h3=":443"; ma=3600')
	assert entries.len == 1
	assert entries[0].protocol == 'h3'
	assert entries[0].port == 443
	assert entries[0].max_age == 3600
}

fn test_parse_alt_svc_multiple() {
	entries := parse_alt_svc('h3=":443", h2=":443"')
	assert entries.len == 2
	assert entries[0].protocol == 'h3'
	assert entries[0].port == 443
	assert entries[1].protocol == 'h2'
	assert entries[1].port == 443
}

fn test_parse_alt_svc_clear() {
	entries := parse_alt_svc('clear')
	assert entries.len == 0
}

fn test_parse_alt_svc_with_persist() {
	entries := parse_alt_svc('h3=":443"; persist=1')
	assert entries.len == 1
	assert entries[0].persist == true
	assert entries[0].port == 443
}

fn test_cache_store_and_get() {
	mut cache := new_alt_svc_cache()
	origin := 'https://example.com:443'
	entries := [
		AltSvcEntry{
			protocol: 'h3'
			host:     ''
			port:     443
			max_age:  86400
			persist:  false
		},
	]
	cache.store(origin, entries)
	result := cache.get_h3_endpoint(origin) or {
		assert false, 'expected h3 entry'
		return
	}
	assert result.protocol == 'h3'
	assert result.port == 443
}

fn test_cache_cleanup_expired() {
	mut cache := new_alt_svc_cache()
	origin := 'https://expired.example.com:443'
	// Store an entry that expires immediately (max_age=0)
	entries := [
		AltSvcEntry{
			protocol: 'h3'
			host:     ''
			port:     443
			max_age:  0
			persist:  false
		},
	]
	cache.store(origin, entries)
	// After sleep, entry should be expired
	time.sleep(10 * time.millisecond)
	cache.cleanup()
	if _ := cache.get_h3_endpoint(origin) {
		assert false, 'expected no entry after cleanup'
	}
}
