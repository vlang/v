fn func(rune_bytes []byte) string {
	return rune_bytes.str()
}

fn test_assign() {
	r := rune(0)
	mut rune_bytes := r.bytes()
	rb := rune_bytes
	_ = rune_bytes
	rc := func(rb)
}

fn test_bool() {
	b := bool(false)
	bool_str := b.str()
}

fn test_byteptr() {
	b0 := byteptr(c'a')
	b1 := byteptr(c'b')
	b2 := byteptr(c'c')
	b3 := byteptr(c'd')
	b4 := byteptr(c'e')
	byteptr_str := b0.str()
	byteptr_vbytes := unsafe { b0.vbytes(1) }
	byteptr_vstring := unsafe { b1.vstring() }
	byteptr_vstring_literal := unsafe { b2.vstring_literal() }
	byteptr_vstring_literal_with_len := unsafe { b3.vstring_literal_with_len(1) }
	byteptr_vstring_with_len := unsafe { b4.vstring_with_len(1) }
}

fn test_string() {
	s := 'vstring'
	s1 := ' other'
	string_after := s.after('v')
	string_after_char := s.after_char(`v`)
	string_all_after := s.all_after('v')
	string_all_after_first := s.all_after_first('v')
	string_all_after_last := s.all_after_last('v')
	string_all_before := s.all_before('v')
	string_all_before_last := s.all_before_last('v')
	string_before := s.before('v')
	string_bool := s.bool()
	string_bytes := s.bytes()
	string_camel_to_snake := s.camel_to_snake()
	string_capitalize := s.capitalize()
	string_clone := s.clone()
	string_compare := s.compare(s1)
	string_contains := s.contains('v')
	string_contains_any := s.contains_any('vs')
	string_contains_any_substr := s.contains_any_substr(['v', 'ing'])
	string_contains_only := s.contains_only('s')
	string_contains_u8 := s.contains_u8(`g`)
	string_count := s.count('i')
	string_ends_with := s.ends_with('ing')
	string_expand_tabs := s.expand_tabs(4)
	string_f32 := s.f32()
	string_f64 := s.f64()
	string_fields := s.fields()
	string_find_between := s.find_between('v', 'g')
	string_hash := s.hash()
	string_i16 := s.i16()
	string_i32 := s.i32()
	string_i64 := s.i64()
	string_i8 := s.i8()
	string_indent_width := s.indent_width()
	string_index := s.index('g')
	string_index_after_ := s.index_after_('n', 3)
	string_index_any := s.index_any('g')
	string_index_u8 := s.index_u8(`g`)
	string_int := s.int()
	string_is_ascii := s.is_ascii()
	string_is_bin := s.is_bin()
	string_is_blank := s.is_blank()
	string_is_capital := s.is_capital()
	string_is_hex := s.is_hex()
	string_is_identifier := s.is_identifier()
	string_is_int := s.is_int()
	string_is_lower := s.is_lower()
	string_is_oct := s.is_oct()
	string_is_pure_ascii := s.is_pure_ascii()
	string_is_title := s.is_title()
	string_is_upper := s.is_upper()
	string_last_index := s.last_index('g')
	string_last_index_u8 := s.last_index_u8(`g`)
	string_len_utf8 := s.len_utf8()
	string_limit := s.limit(5)
	string_match_glob := s.match_glob('*')
	string_normalize_tabs := s.normalize_tabs(2)
	string_parse_int := s.parse_int(10, 32) or { 0 }
	string_parse_uint := s.parse_uint(10, 32) or { 0 }
	string_repeat := s.repeat(2)
	string_replace := s.replace('v', 'V')
	string_replace_char := s.replace_char(`v`, `V`, 1)
	string_replace_each := s.replace_each(['v', 'V'])
	string_replace_once := s.replace_once('v', 'V')
	string_reverse := s.reverse()
	string_rsplit := s.rsplit('g')
	string_rsplit_any := s.rsplit_any('g')
	string_rsplit_nth := s.rsplit_nth('g', 1)
	string_rsplit_once, tmp := s.rsplit_once('g') or { '', '' }
	string_runes := s.runes()
	string_runes_iterator := s.runes_iterator()
	string_snake_to_camel := s.snake_to_camel()
	string_split := s.split('r')
	string_split_any := s.split_any('r')
	string_split_by_space := s.split_by_space()
	string_split_into_lines := s.split_into_lines()
	string_split_n := s.split_n('g', 2)
	string_split_nth := s.split_nth('ri', 2)
	string_split_once, tmp1 := s.split_once('g') or { '', '' }
	string_starts_with := s.starts_with('v')
	string_starts_with_captial := s.starts_with_capital()
	string_str := s.str()
	string_strip_margin := s.strip_margin()
	string_strip_margin_custom := s.strip_margin_custom(`v`)
	string_substr := s.substr(1, 3)
	string_substr_ni := s.substr_ni(0, 1)
	string_substr_unsafe := unsafe { s[0..2] }
	string_substr_with_check := s.substr_with_check(0, 1) or { '' }
	string_title := s.title()
	string_to_lower := s.to_lower()
	string_to_lower_ascii := s.to_lower_ascii()
	string_to_upper := s.to_upper()
	string_to_upper_ascii := s.to_upper_ascii()
	string_to_wide := s.to_wide()
	string_trim_chars := s.trim('string')
	string_trim_indent := s.trim_indent()
	string_trim_indexes, tmp3 := s.trim_indexes('in')
	string_trim_left := s.trim_left('g')
	string_trim_right := s.trim_right('g')
	string_trim_space := s.trim_space()
	string_trim_space_left := s.trim_space_left()
	string_trim_space_right := s.trim_space_right()
	string_trim_string_left := s.trim_string_left('v')
	string_trim_string_right := s.trim_string_right('g')
	string_u16 := s.u16()
	string_u32 := s.u32()
	string_u64 := s.u64()
	string_u8 := s.u8()
	string_u8_array := s.u8_array()
	string_uncapitalize := s.uncapitalize()
	string_utf32_code := s.utf32_code()
	string_wrap := s.wrap(width: 20)
	string__eq := s == s1
	string__lt := s < s1
	string__plus := s + s1
	string_at := s[3]
}

fn test_i8() {
	i8_ := i8(0)
	i8_str := i8_.str()
}

fn test_i16() {
	i16_ := i16(0)
	i16_str := i16_.str()
}

fn test_i32() {
	i32_ := i32(0)
	i32_str := i32_.str()
}

fn test_int() {
	int_ := int(0)
	int_str := int_.str()
	int_literal_str := 0.str()
}

fn test_i64() {
	i64_ := i64(0)
	i64_str := i64_.str()
}

fn test_u8() {
	u8_ := u8(0)
	u8_str := u8_.str()
	u8_ascii_str := u8_.ascii_str()
}

fn test_u16() {
	u16_ := u16(0)
	u16_str := u16_.str()
}

fn test_u32() {
	u32_ := u32(0)
	u32_str := u32_.str()
}

fn test_u64() {
	u64_ := u64(0)
	u64_str := u64_.str()
	u64_hex := u64_.hex()
}

fn test_isize() {
	isize_ := isize(0)
	isize_str := isize_.str()
}

fn test_usize() {
	usize_ := usize(0)
	usize_str := usize_.str()
}

fn test_f32() {
	f32_ := f32(0)
	f32_str := f32_.str()
	f32_strg := f32_.strg()
	f32_strsci := f32_.strsci(2)
	f32_strlong := f32_.strlong()
	f32_eq_epsilon := f32_.eq_epsilon(0.000001)
}

fn test_f64() {
	f64_ := f64(0)
	f64_str := f64_.str()
	f64_strg := f64_.strg()
	f64_strsci := f64_.strsci(2)
	f64_strlong := f64_.strlong()
	f64_eq_epsilon := f64_.eq_epsilon(0.000001)
}

fn test_float() {
	float_literal_str := 0.1.str()
}

fn test_rune() {
	r := rune(0)
	rune_bytes := r.bytes()
	rune_str := r.str()
	rune_to_upper := r.to_upper()
}

fn test_ptr() {
	ptr := unsafe { voidptr(nil) }
	voidptr_hex_full := ptr.hex_full()
	voidptr_str := ptr.str()
	voidptr_vbytes := unsafe { ptr.vbytes(1) }
}

fn test_char() {
	char_ := unsafe { &char(c'a') }
	char_str := char_.str()
	char_vstring := unsafe { char_.vstring() }
	char_vstring_literal_with_len := unsafe { char_.vstring_with_len(1) }
	char_vstring_with_len := unsafe { char_.vstring_with_len(1) }
}

fn test_cstring() {
	cstring := c'cstring'
	u8_vstring := unsafe { cstring.vstring() }
}

fn test_array() {
	// new_array_from_c_array_noscan := [1, 2, 3]
	mut a := [1, 2, 3]
	array_repeat_to_depth := a.repeat(2)
	array_first := a.first()
	array_last := a.last()
	array_pop_left_noscan := a.pop_left()
	array_pop_noscan := a.pop()
	array_get := a[0]
	array_clone_to_depth := a.clone()
	array_reverse := a.reverse()
	array_filter := a.filter(it < 2)
	array_any := a.any(it % 2 == 1)
	array_count := a.count(it > 1)
	array_all := a.all(it > 0)
	array_slice := unsafe { a[0..1] }
}

fn test_map() {
	// new_map_noscan_key_value := map[int]int{}
	// new_map_init_noscan_value := {
	//	'test': 10
	//}
	mut m := {
		'test': 10
	}
	map_clone := m.clone()
	map_keys := m.keys()
	map_values := m.values()
	map_get := m['test']
	map_move := m.move()
}
