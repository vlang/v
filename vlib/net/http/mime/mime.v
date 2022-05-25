module mime

pub struct MimeType {
	source       string
	extensions   []string
	compressible bool
	charset      string
}

// returns a `MimeType` and the the MIME type for the given file extension or MIME type
fn mime_type(str string) ?(MimeType, string) {
	// if `str` is a mime type
	if str in db {
		return db[str], str
	}
	// if `str` is an extension
	for mt_str, mt in db {
		if str in mt.extensions {
			return mt, mt_str
		}
	}
	return none, none
}

// returns a `MimeType` for the given file extension or MIME type
pub fn get_complete_mime_type(str string) ?MimeType {
	mt, _ := mime_type(str)?
	return mt
}

// returns the MIME type for the given file extension
pub fn get_mime_type(ext string) ?string {
	_, mt := mime_type(ext)?
	return mt
}

// returns a `content-type` header ready to use for the given file extension or MIME type
pub fn get_content_type(str string) ?string {
	mt, mt_str := mime_type(str)?
	charset := if mt.charset.len > 0 { mt.charset.to_lower() } else { 'utf-8' }
	return '$mt_str; charset=$charset'
}

// returns the default extension for the given file extension or MIME type
pub fn get_default_ext(str string) ?string {
	mt, _ := mime_type(str)?
	return mt.extensions[0]
}
