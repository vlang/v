struct Struct {
	name string
	x    int
}

fn test_byte_pointer_casts() {
	unsafe {
		pb := &u8(1)
		ppb := &&u8(2)
		pppb := &&&u8(3)
		ppppb := &&&&u8(4)
		assert voidptr(pb).str() == '0x1'
		assert voidptr(ppb).str() == '0x2'
		assert voidptr(pppb).str() == '0x3'
		assert voidptr(ppppb).str() == '0x4'
	}
}

fn test_char_pointer_casts() {
	unsafe {
		pc := &char(5)
		ppc := &&char(6)
		pppc := &&&char(7)
		ppppc := &&&&char(8)
		assert voidptr(pc).str() == '0x5'
		assert voidptr(ppc).str() == '0x6'
		assert voidptr(pppc).str() == '0x7'
		assert voidptr(ppppc).str() == '0x8'
	}
}

fn test_struct_pointer_casts() {
	unsafe {
		ps := &Struct(9)
		pps := &&Struct(10)
		ppps := &&&Struct(11)
		pppps := &&&&Struct(12)
		assert voidptr(ps).str() == '0x9'
		assert voidptr(pps).str() == '0xa'
		assert voidptr(ppps).str() == '0xb'
		assert voidptr(pppps).str() == '0xc'
	}
}

fn test_struct_pointer_casts_with_field_selectors() {
	ss := &Struct{
		name: 'abc'
		x: 123
	}
	dump(ss)
	pss := voidptr(ss)
	if &Struct(pss).name == 'abc' {
		assert true
	}
	if &Struct(pss).x == 123 {
		// &Struct cast and selecting .x
		assert true
	}
	if unsafe { &&Struct(pss) != 0 } {
		// &&Struct
		assert true
	}
}

fn test_pointer_casts_with_indexing() {
	mut numbers := [5]u64{}
	numbers[0] = 123
	numbers[1] = 456
	unsafe {
		pnumbers := voidptr(&numbers[0])
		assert &u64(pnumbers)[0] == 123
		assert &u64(pnumbers)[1] == 456
		idx := 1
		assert &u64(pnumbers)[idx] == 456
	}
}
