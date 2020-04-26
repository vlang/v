import strconv
import net.urllib
fn test_format(){
	mut buf    := [1024]byte
	mut temp_s := ""
	a0  := u32(10)
	b0  := 200
	c0  := byte(12)
	s0  := "ciAo"
	ch0 := `B`
	
	f0  := 0.312345
	f1  := 200000.0
	f2  := -1234.300e6
	f3  := 1234.300e-6
	
	sc0 := "ciao: [%-08u] %d %hhd [%08s]\nr2: [%08X] [%p] [%-20.4f] [%-20.4f] [%c]\n"
	temp_s = strconv.v_sprintf(sc0    ,a0 ,b0 ,c0 ,s0     ,b0 ,&b0 ,f0, f1, ch0)
	C.sprintf(buf, sc0.str,a0 ,b0 ,c0 ,s0.str ,b0 ,&b0 ,f0, f1, ch0)
    
	$if debug {
		eprintln('C sprintf:')
		eprintln( tos2(buf) )
		eprintln( tos2(buf).bytes().hex() )
        eprintln('V sprintf:')
        eprintln( temp_s )
        eprintln( temp_s.bytes().hex() )
	}
    
	$if macos {
		assert tos2(buf) == temp_s
	}
    
	a := byte(12)
	b := i16(13)
	c := 14
	d := i64(15)
	sc1 := "==>%hhd %hd %d %ld\n"
	temp_s = strconv.v_sprintf(sc1, a ,b ,c, d)
	C.sprintf(buf, sc1.str, a ,b ,c, d)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	a1 := byte(0xff)
	b1 := i16(0xffff)
	c1 := u32(0xffff_ffff)
	d1 := u64(-1)
	sc2 := "%hhu %hu %u %lu\n"
	temp_s = strconv.v_sprintf(sc2, a1 ,b1 ,c1, d1)
	C.sprintf(buf, sc2.str, a1 ,b1 ,c1, d1)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s
	

	sc3 := "%hhx %hx %x %lx\n"
	temp_s = strconv.v_sprintf(sc3, a1 ,b1 ,c1, d1)
	C.sprintf(buf, sc3.str, a1 ,b1 ,c1, d1)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s
	

	sc4 := "[%-20.3e] [%20.3e] [%-020.3e] [%-020.3E] [%-020.3e] [%-020.3e]\n"
	temp_s = strconv.v_sprintf(sc4, f0, f1, f1, f1, f2, f3)
	C.sprintf(buf, sc4.str, f0, f1, f1, f1, f2, f3)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s
	
	sc5 := "[%.3f] [%0.3f] [%0.3F] [%0.3f] [%0.3F]\n"
	temp_s = strconv.v_sprintf(sc5, f0, f1, f1, f2, f3, f3)
	C.sprintf(buf, sc5.str, f0, f1, f1, f2, f3, f3)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	ml  := 3
	sc6 := "%.*s [%05hhX]\n"
	temp_s = strconv.v_sprintf(sc6, ml, s0    , a)
	C.sprintf(buf, sc6.str, ml, s0.str, a)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	a2 := 125
	sc7 := "[%9x] [%9X] [%-9x] [%-9X] [%09x] [%09X]\n"
	temp_s = strconv.v_sprintf(sc7, a2, a2, a2, a2, a2, a2)
	C.sprintf(buf, sc7.str, a2, a2, a2, a2, a2, a2)
	//println("$temp_s${tos2(buf)}")
	assert tos2(buf) == temp_s

	mut ft := -1e-7
	mut x  := 0
	sc8    := "[%20g][%20G]|"
	for x < 12 {
		temp_s = strconv.v_sprintf(sc8, ft, ft)
		C.sprintf(buf,sc8.str, ft, ft)
		//println("$temp_s ${tos2(buf)}")
		assert tos2(buf) == temp_s
		ft = ft * 10.0
		x++
	}
}
