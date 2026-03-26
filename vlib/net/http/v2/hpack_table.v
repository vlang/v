module v2

// HPACK static table and lookup maps (RFC 7541 Appendix A).

const static_table = [
	HeaderField{'', ''},
	HeaderField{':authority', ''},
	HeaderField{':method', 'GET'},
	HeaderField{':method', 'POST'},
	HeaderField{':path', '/'},
	HeaderField{':path', '/index.html'},
	HeaderField{':scheme', 'http'},
	HeaderField{':scheme', 'https'},
	HeaderField{':status', '200'},
	HeaderField{':status', '204'},
	HeaderField{':status', '206'},
	HeaderField{':status', '304'},
	HeaderField{':status', '400'},
	HeaderField{':status', '404'},
	HeaderField{':status', '500'},
	HeaderField{'accept-charset', ''},
	HeaderField{'accept-encoding', 'gzip, deflate'},
	HeaderField{'accept-language', ''},
	HeaderField{'accept-ranges', ''},
	HeaderField{'accept', ''},
	HeaderField{'access-control-allow-origin', ''},
	HeaderField{'age', ''},
	HeaderField{'allow', ''},
	HeaderField{'authorization', ''},
	HeaderField{'cache-control', ''},
	HeaderField{'content-disposition', ''},
	HeaderField{'content-encoding', ''},
	HeaderField{'content-language', ''},
	HeaderField{'content-length', ''},
	HeaderField{'content-location', ''},
	HeaderField{'content-range', ''},
	HeaderField{'content-type', ''},
	HeaderField{'cookie', ''},
	HeaderField{'date', ''},
	HeaderField{'etag', ''},
	HeaderField{'expect', ''},
	HeaderField{'expires', ''},
	HeaderField{'from', ''},
	HeaderField{'host', ''},
	HeaderField{'if-match', ''},
	HeaderField{'if-modified-since', ''},
	HeaderField{'if-none-match', ''},
	HeaderField{'if-range', ''},
	HeaderField{'if-unmodified-since', ''},
	HeaderField{'last-modified', ''},
	HeaderField{'link', ''},
	HeaderField{'location', ''},
	HeaderField{'max-forwards', ''},
	HeaderField{'proxy-authenticate', ''},
	HeaderField{'proxy-authorization', ''},
	HeaderField{'range', ''},
	HeaderField{'referer', ''},
	HeaderField{'refresh', ''},
	HeaderField{'retry-after', ''},
	HeaderField{'server', ''},
	HeaderField{'set-cookie', ''},
	HeaderField{'strict-transport-security', ''},
	HeaderField{'transfer-encoding', ''},
	HeaderField{'user-agent', ''},
	HeaderField{'vary', ''},
	HeaderField{'via', ''},
	HeaderField{'www-authenticate', ''},
]

const static_table_exact_map = build_exact_map()

const static_table_name_map = build_name_map()

fn build_exact_map() map[string]int {
	mut m := map[string]int{}
	for i, entry in static_table {
		if entry.name != '' {
			key := '${entry.name}:${entry.value}'
			if key !in m {
				m[key] = i
			}
		}
	}
	return m
}

fn build_name_map() map[string][]int {
	mut m := map[string][]int{}
	for i, entry in static_table {
		if entry.name != '' {
			if entry.name !in m {
				m[entry.name] = []int{}
			}
			m[entry.name] << i
		}
	}
	return m
}
