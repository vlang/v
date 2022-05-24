module mime

fn test_mime() ? {
	assert ext_to_mime_type('png')? == 'image/png'

	json_ext := get_mime_type('json')?
	json_mime := get_mime_type('application/json')?
	json := MimeType{
		str: 'application/json'
		source: 'iana'
		extensions: ['json', 'map']
		compressible: true
		charset: 'UTF-8'
	}
	assert json_ext == json
	assert json_mime == json
	assert json.content_type() == 'application/json; charset=utf-8'
	assert json.default_ext() == 'json'

	markdown_ext := get_mime_type('markdown')?
	markdown_mime := get_mime_type('text/markdown')?
	markdown := MimeType{
		str: 'text/markdown'
		source: 'iana'
		extensions: ['md', 'markdown']
		compressible: true
		charset: ''
	}
	assert markdown_ext == markdown
	assert markdown_mime == markdown
	assert markdown.content_type() == 'text/markdown; charset=utf-8'
	assert markdown.default_ext() == 'md'
}
