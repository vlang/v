module mime

fn test_mime() {
	assert get_complete_mime_type('application/json') == MimeType{
		source: 'iana'
		extensions: ['json', 'map']
		compressible: true
		charset: 'UTF-8'
	}
	assert get_mime_type('json') == 'application/json'
	assert get_content_type('application/json') == 'application/json; charset=utf-8'
	assert get_default_ext('application/json') == 'json'

	assert get_complete_mime_type('text/markdown') == MimeType{
		source: 'iana'
		extensions: ['md', 'markdown']
		compressible: true
		charset: ''
	}
	assert get_mime_type('md') == 'text/markdown'
	assert get_content_type('text/markdown') == 'text/markdown; charset=utf-8'
	assert get_default_ext('text/markdown') == 'md'

	assert exists('application/json') == true
	assert exists('udfsbsfib') == false

	assert get_default_ext('application/1d-interleaved-parityfec') == '' // valid mime type without associated extension
	assert get_default_ext('invalid mime type') == '' // invalid mime type
}
