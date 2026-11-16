module mime

fn test_mime() {
	assert get_complete_mime_type('application/json') == MimeType{
		source:       'iana'
		extensions:   ['json', 'map']
		compressible: true
		charset:      'UTF-8'
	}
	assert get_mime_type('json') == 'application/json'
	assert lookup('json') == 'application/json'
	assert lookup('.json') == 'application/json'
	assert lookup('.JSON') == 'application/json'
	assert lookup('Js') == 'text/javascript'
	assert lookup('unknownext') == ''
	assert get_content_type('application/json') == 'application/json; charset=utf-8'
	assert get_default_ext('application/json') == 'json'

	assert get_complete_mime_type('text/markdown') == MimeType{
		source:       'iana'
		extensions:   ['md', 'markdown']
		compressible: true
		charset:      ''
	}
	assert get_mime_type('md') == 'text/markdown'
	assert get_content_type('text/markdown') == 'text/markdown; charset=utf-8'
	assert get_default_ext('text/markdown') == 'md'

	assert exists('application/json') == true
	assert exists('udfsbsfib') == false

	assert get_default_ext('application/1d-interleaved-parityfec') == '' // valid mime type without associated extension
	assert get_default_ext('invalid mime type') == '' // invalid mime type
}

fn test_lookup_prefix_pairs() {
	// extensions that are prefixes of other extensions must not shadow each
	// other (they share their first bytes, which is what the lookup hashes)
	assert lookup('aif') == 'audio/x-aiff'
	assert lookup('aiff') == 'audio/x-aiff'
	assert lookup('tif') == 'image/tiff'
	assert lookup('tiff') == 'image/tiff'
	assert lookup('3gp') == 'video/3gpp'
	assert lookup('3gpp') == 'video/3gpp'
	assert lookup('stpz') == 'model/step+zip'
	assert lookup('stpxz') == 'model/step-xml+zip'
	assert lookup('ecelp4800') == 'audio/vnd.nuera.ecelp4800'
	assert lookup('ecelp7470') == 'audio/vnd.nuera.ecelp7470'
	// the V overrides
	assert lookup('v') == 'text/x-vlang'
	assert lookup('vsh') == 'text/x-vlang'
}

fn test_lookup_covers_all_db_extensions() {
	// every extension registered in `db` must resolve to a known MIME type,
	// which in turn must list that extension itself (lowercased, with or
	// without a leading dot, exactly like `lookup` accepts them)
	for mt, info in db {
		for ext in info.extensions {
			res := lookup(ext)
			assert res != ''
			assert res in db
			assert ext.to_lower() in db[res].extensions
			assert lookup('.' + ext) == res
			assert lookup(ext.to_upper()) == res
		}
	}
	assert lookup('') == ''
	assert lookup('.') == ''
	assert lookup('a_nonexistent_extension') == ''
}
