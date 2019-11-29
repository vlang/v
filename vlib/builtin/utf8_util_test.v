fn test_utf8_util() {
	src:="ăĂ ôÔ testo æ€”"
	upper:=src.utf8_to_upper()
	lower:=src.utf8_to_lower()
	assert upper=="ĂĂ ÔÔ TESTO Æ€”"
	assert lower=="ăă ôô TESTO æ€”"
}