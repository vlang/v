fn test_isnil_byteptr() {
	pb := &u8(unsafe { nil })
	assert isnil(pb)
}

fn test_isnil_voidptr() {
	pv := unsafe { nil }
	assert isnil(pv)
}

fn test_isnil_charptr() {
	pc := &char(unsafe { nil })
	assert isnil(pc)
}

fn test_isnil_intptr() {
	pi := &int(unsafe { nil })
	assert isnil(pi)
}
