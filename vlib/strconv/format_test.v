import strconv

fn test_format() {
	mut temp_s := ''
	mut tmp_str := ''
	a0 := u32(10)
	b0 := 200
	c0 := u8(12)
	s0 := 'ciAo'
	ch0 := `B`
	f0 := 0.312345
	f1 := 200000.0
	f2 := -1234.300e6
	f3 := 1234.300e-6

	sc0 := 'ciao: [%-08u] %d %hhd [%8s] [%08X] [%-20.4f] [%-20.4f] [%c]'
	temp_s = unsafe { strconv.v_sprintf(sc0, a0, b0, c0, s0, b0, f0, f1, ch0) }
	tmp_str = 'ciao: [10      ] 200 12 [    ciAo] [000000C8] [0.3123              ] [200000.0000         ] [B]'
	// C.printf(sc0.str,a0 ,b0 ,c0 ,s0.str ,b0 ,f0, f1, ch0)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	a := u8(12)
	b := i16(13)
	c := 14
	d := i64(15)
	sc1 := '==>%hhd %hd %d %ld'
	temp_s = unsafe { strconv.v_sprintf(sc1, a, b, c, d) }
	tmp_str = '==>12 13 14 15'
	// C.printf(sc1.str, a ,b ,c, d)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	a1 := u8(0xff)
	b1 := i16(u16(0xffff))
	c1 := u32(0xffff_ffff)
	d1 := u64(-1)
	sc2 := '%hhu %hu %u %lu'
	temp_s = unsafe { strconv.v_sprintf(sc2, a1, b1, c1, d1) }
	tmp_str = '255 65535 4294967295 18446744073709551615'
	// C.printf(sc2.str, a1 ,b1 ,c1, d1)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	sc3 := '%hhx %hx %x %lx'
	temp_s = unsafe { strconv.v_sprintf(sc3, a1, b1, c1, d1) }
	tmp_str = 'ff ffff ffffffff ffffffffffffffff'
	// C.printf(sc3.str, a1 ,b1 ,c1, d1)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	sc4 := '[%-20.3e] [%20.3e] [%-020.3e] [%-020.3E] [%-020.3e] [%-020.3e]'
	temp_s = unsafe { strconv.v_sprintf(sc4, f0, f1, f1, f1, f2, f3) }
	tmp_str = '[3.123e-01           ] [           2.000e+05] [2.000e+05           ] [2.000E+05           ] [-1.234e+09          ] [1.234e-03           ]'
	// C.printf(sc4.str, f0, f1, f1, f1, f2, f3)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	sc5 := '[%.3f] [%0.3f] [%0.3F] [%0.3f] [%0.3F]'
	temp_s = unsafe { strconv.v_sprintf(sc5, f0, f1, f1, f2, f3) }
	tmp_str = '[0.312] [200000.000] [200000.000] [-1234300000.000] [0.001]'
	// C.printf(sc5.str, f0, f1, f1, f2, f3, f3)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	ml := 3
	sc6 := '%.*s [%05hhX]'
	temp_s = unsafe { strconv.v_sprintf(sc6, ml, s0, a) }
	tmp_str = 'ciA [0000C]'
	// C.printf(sc6.str, ml, s0.str, a)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	a2 := 125
	sc7 := '[%9x] [%9X] [%-9x] [%-9X] [%09x] [%09X]'
	temp_s = unsafe { strconv.v_sprintf(sc7, a2, a2, a2, a2, a2, a2) }
	tmp_str = '[       7d] [       7D] [7d       ] [7D       ] [00000007d] [00000007D]'
	// C.printf(sc7.str, a2, a2, a2, a2, a2, a2)
	// println("\n$temp_s")
	assert tmp_str == temp_s

	g_test := [
		'[              -1e-07][              -1E-07]|',
		'[              -1e-06][              -1E-06]|',
		'[              -1e-05][              -1E-05]|',
		'[             -0.0001][             -0.0001]|',
		'[              -0.001][              -0.001]|',
		'[               -0.01][               -0.01]|',
		'[                -0.1][                -0.1]|',
		'[                  -1][                  -1]|',
		'[                 -10][                 -10]|',
		'[                -100][                -100]|',
		'[               -1000][               -1000]|',
		'[              -10000][              -10000]|',
	]

	mut ft := -1e-7
	mut x := 0
	mut cnt := 0
	sc8 := '[%20g][%20G]|'
	for x < 12 {
		temp_s = unsafe { strconv.v_sprintf(sc8, ft, ft) }
		// C.printf(sc8.str, ft, ft)
		// println("\n$temp_s")
		assert temp_s == g_test[cnt]
		ft = ft * 10.0
		x++
		cnt++
	}
}

fn test_sprintf_does_not_double_free_on_g() {
	x := 3.141516
	assert unsafe { strconv.v_sprintf('aaa %G', x) } == 'aaa 3.141516'
}

fn test_sprintf_with_escape() {
	n := 69
	s := unsafe { strconv.v_sprintf('%d is 100%% awesome', n) }
	assert s == '69 is 100% awesome'
}

fn test_remove_tail_zeros() {
	assert strconv.remove_tail_zeros('1.234000000000') == '1.234'
	assert strconv.remove_tail_zeros('1.0000000') == '1'
	assert strconv.remove_tail_zeros('1234') == '1234'
	assert strconv.remove_tail_zeros('1.00000000007') == '1.00000000007'
}

fn test_g_format() {
	a := 1234.56789000e10
	assert '${a:1.0g}' == '1e+13'
	assert '${a:1.1g}' == '1e+13'
	assert '${a:1.2g}' == '1.2e+13'
	assert '${a:1.3g}' == '1.23e+13'
	assert '${a:1.4g}' == '1.235e+13'
	assert '${a:1.5g}' == '1.2346e+13'
	assert '${a:1.6g}' == '1.23457e+13'
	assert '${a:1.7g}' == '1.234568e+13'
	assert '${a:1.8g}' == '1.2345679e+13'
	assert '${a:1.9g}' == '1.23456789e+13'
	assert '${a:1.10g}' == '1.23456789e+13'
	assert '${a:1.11g}' == '1.23456789e+13'
	assert '${a:1.12g}' == '1.23456789e+13'

	// TODO: e format not support due to issue #22429
}
