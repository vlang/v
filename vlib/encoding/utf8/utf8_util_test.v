import encoding.utf8

fn test_utf8_util() {

	// string test
	src:="ăĂ ôÔ testo æ€”" // len 15 runes, raw 24 bytes
	upper:=utf8.to_upper(src)
	lower:=utf8.to_lower(src)
	assert upper=="ĂĂ ÔÔ TESTO Æ€”"
	assert lower=="ăă ôô testo æ€”"

	// ustring test
	src1:=src.ustring()
	upper1:=utf8.u_to_upper(src1)
	lower1:=utf8.u_to_lower(src1)
	assert upper1==( "ĂĂ ÔÔ TESTO Æ€”".ustring() )
	assert lower1==( "ăă ôô testo æ€”".ustring() )

	// test len function
	assert utf8.len("pippo")==5
	assert utf8.len(src)==15
	assert src.len==24
	// test u_len function
	assert utf8.u_len(src1)==15
	assert utf8.u_len("pippo".ustring())==5
}
