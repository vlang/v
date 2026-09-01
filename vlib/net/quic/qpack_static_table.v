module quic

// QpackStaticEntry is one row of the QPACK static table (RFC 9204 §3.1,
// Appendix A). Unlike HPACK's static table, QPACK's is indexed from 0.
pub struct QpackStaticEntry {
pub:
	name  string
	value string
}

// qpack_static_table is the 99-entry table from RFC 9204 Appendix A,
// transcribed directly from the fetched RFC text (not recalled from
// HPACK's differently-ordered, differently-sized 61-entry table by
// assumption -- QPACK's table is index-0-based and was independently
// regenerated from 2018 traffic analysis, so it shares only some rows with
// HPACK's, not all, and never at the same index).
pub const qpack_static_table = [
	QpackStaticEntry{':authority', ''},
	QpackStaticEntry{':path', '/'},
	QpackStaticEntry{'age', '0'},
	QpackStaticEntry{'content-disposition', ''},
	QpackStaticEntry{'content-length', '0'},
	QpackStaticEntry{'cookie', ''},
	QpackStaticEntry{'date', ''},
	QpackStaticEntry{'etag', ''},
	QpackStaticEntry{'if-modified-since', ''},
	QpackStaticEntry{'if-none-match', ''},
	QpackStaticEntry{'last-modified', ''},
	QpackStaticEntry{'link', ''},
	QpackStaticEntry{'location', ''},
	QpackStaticEntry{'referer', ''},
	QpackStaticEntry{'set-cookie', ''},
	QpackStaticEntry{':method', 'CONNECT'},
	QpackStaticEntry{':method', 'DELETE'},
	QpackStaticEntry{':method', 'GET'},
	QpackStaticEntry{':method', 'HEAD'},
	QpackStaticEntry{':method', 'OPTIONS'},
	QpackStaticEntry{':method', 'POST'},
	QpackStaticEntry{':method', 'PUT'},
	QpackStaticEntry{':scheme', 'http'},
	QpackStaticEntry{':scheme', 'https'},
	QpackStaticEntry{':status', '103'},
	QpackStaticEntry{':status', '200'},
	QpackStaticEntry{':status', '304'},
	QpackStaticEntry{':status', '404'},
	QpackStaticEntry{':status', '503'},
	QpackStaticEntry{'accept', '*/*'},
	QpackStaticEntry{'accept', 'application/dns-message'},
	QpackStaticEntry{'accept-encoding', 'gzip, deflate, br'},
	QpackStaticEntry{'accept-ranges', 'bytes'},
	QpackStaticEntry{'access-control-allow-headers', 'cache-control'},
	QpackStaticEntry{'access-control-allow-headers', 'content-type'},
	QpackStaticEntry{'access-control-allow-origin', '*'},
	QpackStaticEntry{'cache-control', 'max-age=0'},
	QpackStaticEntry{'cache-control', 'max-age=2592000'},
	QpackStaticEntry{'cache-control', 'max-age=604800'},
	QpackStaticEntry{'cache-control', 'no-cache'},
	QpackStaticEntry{'cache-control', 'no-store'},
	QpackStaticEntry{'cache-control', 'public, max-age=31536000'},
	QpackStaticEntry{'content-encoding', 'br'},
	QpackStaticEntry{'content-encoding', 'gzip'},
	QpackStaticEntry{'content-type', 'application/dns-message'},
	QpackStaticEntry{'content-type', 'application/javascript'},
	QpackStaticEntry{'content-type', 'application/json'},
	QpackStaticEntry{'content-type', 'application/x-www-form-urlencoded'},
	QpackStaticEntry{'content-type', 'image/gif'},
	QpackStaticEntry{'content-type', 'image/jpeg'},
	QpackStaticEntry{'content-type', 'image/png'},
	QpackStaticEntry{'content-type', 'text/css'},
	QpackStaticEntry{'content-type', 'text/html; charset=utf-8'},
	QpackStaticEntry{'content-type', 'text/plain'},
	QpackStaticEntry{'content-type', 'text/plain;charset=utf-8'},
	QpackStaticEntry{'range', 'bytes=0-'},
	QpackStaticEntry{'strict-transport-security', 'max-age=31536000'},
	QpackStaticEntry{'strict-transport-security', 'max-age=31536000; includesubdomains'},
	QpackStaticEntry{'strict-transport-security', 'max-age=31536000; includesubdomains; preload'},
	QpackStaticEntry{'vary', 'accept-encoding'},
	QpackStaticEntry{'vary', 'origin'},
	QpackStaticEntry{'x-content-type-options', 'nosniff'},
	QpackStaticEntry{'x-xss-protection', '1; mode=block'},
	QpackStaticEntry{':status', '100'},
	QpackStaticEntry{':status', '204'},
	QpackStaticEntry{':status', '206'},
	QpackStaticEntry{':status', '302'},
	QpackStaticEntry{':status', '400'},
	QpackStaticEntry{':status', '403'},
	QpackStaticEntry{':status', '421'},
	QpackStaticEntry{':status', '425'},
	QpackStaticEntry{':status', '500'},
	QpackStaticEntry{'accept-language', ''},
	QpackStaticEntry{'access-control-allow-credentials', 'FALSE'},
	QpackStaticEntry{'access-control-allow-credentials', 'TRUE'},
	QpackStaticEntry{'access-control-allow-headers', '*'},
	QpackStaticEntry{'access-control-allow-methods', 'get'},
	QpackStaticEntry{'access-control-allow-methods', 'get, post, options'},
	QpackStaticEntry{'access-control-allow-methods', 'options'},
	QpackStaticEntry{'access-control-expose-headers', 'content-length'},
	QpackStaticEntry{'access-control-request-headers', 'content-type'},
	QpackStaticEntry{'access-control-request-method', 'get'},
	QpackStaticEntry{'access-control-request-method', 'post'},
	QpackStaticEntry{'alt-svc', 'clear'},
	QpackStaticEntry{'authorization', ''},
	QpackStaticEntry{'content-security-policy', "script-src 'none'; object-src 'none'; base-uri 'none'"},
	QpackStaticEntry{'early-data', '1'},
	QpackStaticEntry{'expect-ct', ''},
	QpackStaticEntry{'forwarded', ''},
	QpackStaticEntry{'if-range', ''},
	QpackStaticEntry{'origin', ''},
	QpackStaticEntry{'purpose', 'prefetch'},
	QpackStaticEntry{'server', ''},
	QpackStaticEntry{'timing-allow-origin', '*'},
	QpackStaticEntry{'upgrade-insecure-requests', '1'},
	QpackStaticEntry{'user-agent', ''},
	QpackStaticEntry{'x-forwarded-for', ''},
	QpackStaticEntry{'x-frame-options', 'deny'},
	QpackStaticEntry{'x-frame-options', 'sameorigin'},
]!

// qpack_static_lookup returns the static table entry at `index` (RFC 9204
// §3.1). An out-of-range index is a decode-time error the caller must map
// to QPACK_DECOMPRESSION_FAILED (field line representation) or
// QPACK_ENCODER_STREAM_ERROR (encoder instruction), per which stream the
// reference appeared on -- this function only reports "invalid", not which
// error code applies, since that depends on context this file doesn't have.
pub fn qpack_static_lookup(index int) !QpackStaticEntry {
	if index < 0 || index >= qpack_static_table.len {
		return error('qpack: static table index ${index} out of range')
	}
	return qpack_static_table[index]
}

// qpack_static_find returns the index of a static table entry whose name
// and value both match exactly, if one exists. Used by an encoder deciding
// whether a field line can be a fully indexed reference.
pub fn qpack_static_find(name string, value string) ?int {
	for i, e in qpack_static_table {
		if e.name == name && e.value == value {
			return i
		}
	}
	return none
}

// qpack_static_find_name returns the index of the first static table entry
// whose name matches, if one exists. Used by an encoder falling back to a
// literal field line with a static name reference.
pub fn qpack_static_find_name(name string) ?int {
	for i, e in qpack_static_table {
		if e.name == name {
			return i
		}
	}
	return none
}
