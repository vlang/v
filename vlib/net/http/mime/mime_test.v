module mime

fn test_mime() ? {
	assert get_mime_type('png')? == 'image/png'

	assert get_complete_mime_type('json')? == MimeType{
		source: 'iana'
		extensions: ['json', 'map']
		compressible: true
		charset: 'UTF-8'
	}
	assert get_mime_type('application/json')? == 'application/json'
	assert get_mime_type('json')? == 'application/json'
	assert get_content_type('json')? == 'application/json; charset=utf-8'
	assert get_default_ext('application/json')? == 'json'

	assert get_complete_mime_type('text/markdown')? == MimeType{
		source: 'iana'
		extensions: ['md', 'markdown']
		compressible: true
		charset: ''
	}
	assert get_mime_type('text/markdown')? == 'text/markdown'
	assert get_mime_type('md')? == 'text/markdown'
	assert get_content_type('md')? == 'text/markdown; charset=utf-8'
	assert get_default_ext('text/markdown')? == 'md'
}
