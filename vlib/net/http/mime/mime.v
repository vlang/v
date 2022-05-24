module mime

pub struct MimeType {
	str          string
	source       string
	extensions   []string
	compressible bool
	charset      string
}

// ext_to_mime_type returns the MIME type corresponding to the given file extension
pub fn ext_to_mime_type(ext string) ?string {
	return get_mime_type(ext)?.str
}

// get_mime_type returns a `MimeType` corresponding to the given file extension or MIME type
pub fn get_mime_type(str string) ?MimeType {
	// if `str` is a mime type
	if str in db {
		return MimeType{
			...db[str]
			str: str
		}
	}
	// if `str` is an extension
	for mime_type, mime_type_struct in db {
		if str in mime_type_struct.extensions {
			return MimeType{
				...mime_type_struct
				str: mime_type
			}
		}
	}
	return none
}

// content_type returns a `content-type` header ready to use
pub fn (m MimeType) content_type() string {
	charset := if m.charset.len > 0 { m.charset.to_lower() } else { 'utf-8' }
	return '$m.str; charset=$charset'
}

// default_ext returns the default extension
pub fn (m MimeType) default_ext() string {
	return m.extensions[0] or { '' }
}
