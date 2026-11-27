module mime

pub struct MimeType {
	source       string
	extensions   []string
	compressible bool
	charset      string
}

// db_info layout, see build.vsh: ext_start << 16 | ext_count << 8 |
// charset << 4 | source << 1 | compressible

// get_complete_mime_type returns a `MimeType` for the given MIME type.
pub fn get_complete_mime_type(mt string) MimeType {
	info := db_info[db_index(mt)]
	start := int(info >> 16)
	count := int((info >> 8) & 0xFF)
	// Slicing a fixed array copies the entire table in uncached builds.
	extensions := []string{len: count, init: db_exts[start + index]}
	return MimeType{
		source:       db_sources[(info >> 1) & 7]
		extensions:   extensions
		compressible: info & 1 != 0
		charset:      db_charsets[(info >> 4) & 0xF]
	}
}

// returns the MIME type for the given file extension
@[inline]
pub fn get_mime_type(ext string) string {
	return lookup(ext)
}

// returns a `content-type` header ready to use for the given MIME type
pub fn get_content_type(mt string) string {
	charset := db_charsets[(db_info[db_index(mt)] >> 4) & 0xF]
	return '${mt}; charset=${if charset.len > 0 { charset.to_lower() } else { 'utf-8' }}'
}

// returns the default extension for the given MIME type
pub fn get_default_ext(mt string) string {
	info := db_info[db_index(mt)]
	return if (info >> 8) & 0xFF != 0 { db_exts[info >> 16] } else { '' }
}

// returns true if the given MIME type exists
pub fn exists(mt string) bool {
	return db_index(mt) != 0
}
