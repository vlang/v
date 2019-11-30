import encoding.utf8

fn test_utf8_util() {
	src:="ăĂ ôÔ testo æ€”"
	upper:=utf8.to_upper(src)
	lower:=utf8.to_lower(src)
	assert upper=="ĂĂ ÔÔ TESTO Æ€”"
	assert lower=="ăă ôô testo æ€”"

	// check len function
	assert utf8.len("pippo")==5
	assert utf8.len(src)==15
}
