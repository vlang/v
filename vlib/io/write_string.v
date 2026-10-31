module io

// write_string writes the contents of s to w, which accepts a slice of bytes.
// It returns the number of bytes written.
pub fn write_string(mut w Writer, s string) !int {
	return w.write(s.bytes())
}
