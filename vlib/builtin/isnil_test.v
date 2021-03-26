fn test_isnil_byteptr() {
	pb := byteptr(0)
	assert isnil(pb)
}

fn test_isnil_voidptr() {
	pv := voidptr(0)
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
