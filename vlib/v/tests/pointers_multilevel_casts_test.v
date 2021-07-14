struct Struct {}

fn test_byte_pointer_casts() {
	unsafe {
		pb := &byte(1)
		ppb := &&byte(2)
		pppb := &&&byte(3)
		ppppb := &&&&byte(4)
		assert voidptr(pb).str() == '1'
		assert voidptr(ppb).str() == '2'
		assert voidptr(pppb).str() == '3'
		assert voidptr(ppppb).str() == '4'
	}
}

fn test_char_pointer_casts() {
	unsafe {
		pc := &char(5)
		ppc := &&char(6)
		pppc := &&&char(7)
		ppppc := &&&&char(8)
		assert voidptr(pc).str() == '5'
		assert voidptr(ppc).str() == '6'
		assert voidptr(pppc).str() == '7'
		assert voidptr(ppppc).str() == '8'
	}
}

fn test_struct_pointer_casts() {
	unsafe {
		ps := &Struct(9)
		pps := &&Struct(10)
		ppps := &&&Struct(11)
		pppps := &&&&Struct(12)
		assert voidptr(ps).str() == '9'
		assert voidptr(pps).str() == 'a'
		assert voidptr(ppps).str() == 'b'
		assert voidptr(pppps).str() == 'c'
	}
}
