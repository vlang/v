fn test_is_nil_byteptr(){
	pb := byteptr(0)
	assert is_nil( pb )
}

fn test_is_nil_voidptr(){
	pv := voidptr(0)
	assert is_nil( pv )
}

fn test_is_nil_charptr(){
	pc := &char(0)
	assert is_nil( pc )
}

fn test_is_nil_intptr(){
	pi := &int(0)
	assert is_nil( pi )
}
