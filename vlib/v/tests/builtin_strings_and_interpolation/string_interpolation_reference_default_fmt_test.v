fn test_string_interpolation_reference_default_fmt() {
	s := 'Hello'
	sp := &s
	assert '${sp}' == ptr_str(sp)

	f := f32(1.25)
	fp := &f
	assert '${fp}' == ptr_str(fp)

	r := rune(`A`)
	rp := &r
	assert '${rp}' == ptr_str(rp)

	i := 123
	ip := &i
	assert '${ip}' == ptr_str(ip)
}
