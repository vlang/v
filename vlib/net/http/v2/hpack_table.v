module v2

// HPACK static table and lookup maps (RFC 7541 Appendix A).

const static_table = [
	HeaderField{
		name:  ''
		value: ''
	},
	HeaderField{
		name:  ':authority'
		value: ''
	},
	HeaderField{
		name:  ':method'
		value: 'GET'
	},
	HeaderField{
		name:  ':method'
		value: 'POST'
	},
	HeaderField{
		name:  ':path'
		value: '/'
	},
	HeaderField{
		name:  ':path'
		value: '/index.html'
	},
	HeaderField{
		name:  ':scheme'
		value: 'http'
	},
	HeaderField{
		name:  ':scheme'
		value: 'https'
	},
	HeaderField{
		name:  ':status'
		value: '200'
	},
	HeaderField{
		name:  ':status'
		value: '204'
	},
	HeaderField{
		name:  ':status'
		value: '206'
	},
	HeaderField{
		name:  ':status'
		value: '304'
	},
	HeaderField{
		name:  ':status'
		value: '400'
	},
	HeaderField{
		name:  ':status'
		value: '404'
	},
	HeaderField{
		name:  ':status'
		value: '500'
	},
	HeaderField{
		name:  'accept-charset'
		value: ''
	},
	HeaderField{
		name:  'accept-encoding'
		value: 'gzip, deflate'
	},
	HeaderField{
		name:  'accept-language'
		value: ''
	},
	HeaderField{
		name:  'accept-ranges'
		value: ''
	},
	HeaderField{
		name:  'accept'
		value: ''
	},
	HeaderField{
		name:  'access-control-allow-origin'
		value: ''
	},
	HeaderField{
		name:  'age'
		value: ''
	},
	HeaderField{
		name:  'allow'
		value: ''
	},
	HeaderField{
		name:  'authorization'
		value: ''
	},
	HeaderField{
		name:  'cache-control'
		value: ''
	},
	HeaderField{
		name:  'content-disposition'
		value: ''
	},
	HeaderField{
		name:  'content-encoding'
		value: ''
	},
	HeaderField{
		name:  'content-language'
		value: ''
	},
	HeaderField{
		name:  'content-length'
		value: ''
	},
	HeaderField{
		name:  'content-location'
		value: ''
	},
	HeaderField{
		name:  'content-range'
		value: ''
	},
	HeaderField{
		name:  'content-type'
		value: ''
	},
	HeaderField{
		name:  'cookie'
		value: ''
	},
	HeaderField{
		name:  'date'
		value: ''
	},
	HeaderField{
		name:  'etag'
		value: ''
	},
	HeaderField{
		name:  'expect'
		value: ''
	},
	HeaderField{
		name:  'expires'
		value: ''
	},
	HeaderField{
		name:  'from'
		value: ''
	},
	HeaderField{
		name:  'host'
		value: ''
	},
	HeaderField{
		name:  'if-match'
		value: ''
	},
	HeaderField{
		name:  'if-modified-since'
		value: ''
	},
	HeaderField{
		name:  'if-none-match'
		value: ''
	},
	HeaderField{
		name:  'if-range'
		value: ''
	},
	HeaderField{
		name:  'if-unmodified-since'
		value: ''
	},
	HeaderField{
		name:  'last-modified'
		value: ''
	},
	HeaderField{
		name:  'link'
		value: ''
	},
	HeaderField{
		name:  'location'
		value: ''
	},
	HeaderField{
		name:  'max-forwards'
		value: ''
	},
	HeaderField{
		name:  'proxy-authenticate'
		value: ''
	},
	HeaderField{
		name:  'proxy-authorization'
		value: ''
	},
	HeaderField{
		name:  'range'
		value: ''
	},
	HeaderField{
		name:  'referer'
		value: ''
	},
	HeaderField{
		name:  'refresh'
		value: ''
	},
	HeaderField{
		name:  'retry-after'
		value: ''
	},
	HeaderField{
		name:  'server'
		value: ''
	},
	HeaderField{
		name:  'set-cookie'
		value: ''
	},
	HeaderField{
		name:  'strict-transport-security'
		value: ''
	},
	HeaderField{
		name:  'transfer-encoding'
		value: ''
	},
	HeaderField{
		name:  'user-agent'
		value: ''
	},
	HeaderField{
		name:  'vary'
		value: ''
	},
	HeaderField{
		name:  'via'
		value: ''
	},
	HeaderField{
		name:  'www-authenticate'
		value: ''
	},
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
