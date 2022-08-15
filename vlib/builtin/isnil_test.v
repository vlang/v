fn test_isnil_byteptr() {
	pb := &u8(0)
	assert isnil(pb)
}

fn test_isnil_voidptr() {
	pv := unsafe { nil }
	assert isnil(pv)
}

fn test_isnil_charptr() {
	pc := &char(0)
	assert isnil(pc)
}

fn test_isnil_intptr() {
	pi := &int(0)
	assert isnil(pi)
}
