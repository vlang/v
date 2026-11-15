module io

// write_string writes the contents of s to w, which accepts a slice of bytes.
// It calls w.write once and returns its byte count or error without retrying
// a short write. The count is in bytes, not Unicode characters.
pub fn write_string(mut w Writer, s string) !int {
	return w.write(s.bytes())
}
