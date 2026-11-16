module mime

@[inline]
fn lower_byte(c u8) u8 {
	return if c >= `A` && c <= `Z` { c + 32 } else { c }
}

pub struct MimeType {
	source       string
	extensions   []string
	compressible bool
	charset      string
}

// returns a `MimeType` for the given MIME type
pub fn get_complete_mime_type(mt string) MimeType {
	return db[mt]
}

// lookup returns the MIME type for the given file extension, or '' when unknown.
// The extension may be given with or without a leading '.', and is compared
// ASCII case-insensitively without allocating.
@[direct_array_access; inline]
pub fn lookup(ext string) string {
	return lookup_generated(ext)
}

// returns the MIME type for the given file extension
@[inline]
pub fn get_mime_type(ext string) string {
	return lookup_generated(ext)
}

// returns a `content-type` header ready to use for the given MIME type
pub fn get_content_type(mt string) string {
	mt_struct := db[mt]
	charset := if mt_struct.charset.len > 0 { mt_struct.charset.to_lower() } else { 'utf-8' }
	return '${mt}; charset=${charset}'
}

// returns the default extension for the given MIME type
pub fn get_default_ext(mt string) string {
	return if db[mt].extensions.len > 0 {
		db[mt].extensions[0]
	} else {
		''
	}
}

// returns true if the given MIME type exists
pub fn exists(mt string) bool {
	return mt in db
}
