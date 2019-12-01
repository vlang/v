import encoding.utf8

fn test_utf8_util() {

	// string test
	src:="ăĂ ôÔ testo æ€”"//_\u1E5A\u1E5B<=>\u1F49\u1F41<=>\u0128\u012a\u012c" // len 29 runes, raw 49 bytes
	src_upper:="ĂĂ ÔÔ TESTO Æ€”"//_\u1E5A\u1E5A<=>\u1F49\u1F49<=>\u0128\u012A\u012C"
	src_lower:="ăă ôô testo æ€”"//_\u1E5B\u1E5B<=>\u1F41\u1F41<=>\u0129\u012B\u012D"
	upper:=utf8.to_upper(src)
	lower:=utf8.to_lower(src)
	assert upper==src_upper
	assert lower==src_lower

	// ustring test
	src1:=src.ustring()
	upper1:=utf8.u_to_upper(src1)
	lower1:=utf8.u_to_lower(src1)
	assert upper1==( src_upper.ustring() )
	assert lower1==( src_lower.ustring() )

	// test len function
	assert utf8.len("pippo")==5
	assert utf8.len(src)==15 //29
	assert src.len==24 //49
	// test u_len function
	assert utf8.u_len(src1)==15 //29
	assert utf8.u_len("pippo".ustring())==5
}
