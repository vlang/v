fn test_clash_var_fn_name() {
	i := isize(0)
	isize_str := i.str()
	_ = isize_str

	u := usize(0)
	usize_str := u.str()
	_ = usize_str

	i32_ := i32(0)
	i32_str := i32_.str()
	_ = i32_str

	int_ := int(0)
	int_str := int_.str()
	_ = int_str

	u32_ := u32(0)
	u32_str := u32_.str()
	_ = u32_str

	i64_ := i64(0)
	i64_str := i64_.str()
	_ = i64_str

	u64_ := u64(0)
	u64_str := u64_.str()
	u64_hex := u64_.hex()
	_ = u64_str
	_ = u64_hex

	b := bool(false)
	bool_str := b.str()
	_ = bool_str

	ptr := unsafe { nil }
	voidptr_str := ptr.str()
	_ = ptr_str

	charptr := unsafe { &char(0) }
	charptr_str := charptr.str()
	_ = charptr_str

	u8_ := u8(0)
	u8_str := u8_.str()
	_ = u8_str

	r := rune(0)
	rune_bytes := r.bytes()
	rune_str := r.str()
	_ = rune_bytes
	_ = rune_str

	s := 'hello'
	string_str := s.str()
	string_clone := s.clone()
	_ = string_str
	_ = string_clone
}
