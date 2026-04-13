fn test_string_interpolation_reference_default_fmt() {
	s := 'Hello'
	sp := &s
	assert '${sp}' == ptr_str(sp)

	f := f32(1.25)
	fp := &f
	assert '${fp}' == '1.25'

	r := rune(`A`)
	rp := &r
	assert '${rp}' == '65'

	i := 123
	ip := &i
	assert '${ip}' == '123'
}
