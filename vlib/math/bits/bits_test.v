module bits

fn test_bits(){
	mut i := 0
	mut i1:= u64(0)
	//
	// --- LeadingZeros ---
	//
	
	// 8 bit
	i = 1
	for x in 0..8 {
		//C.printf("x:%02x lz: %d cmp: %d\n",i<<x,leading_zeros_8(i<<x), 7-x)
		assert leading_zeros_8(byte(i<<x)) == 7 - x
	}
	// 16 bit
	i = 1
	for x in 0..16 {
		//C.printf("x:%04x lz: %d cmp: %d\n",u16(i)<<x,leading_zeros_16(u16(i)<<x), 15-x)
		assert leading_zeros_16(u16(i)<<x) == 15 - x
	}
	// 32 bit
	i = 1
	for x in 0..32 {
		//C.printf("x:%08x lz: %d cmp: %d\n",u32(i)<<x,leading_zeros_32(u32(i)<<x), 31-x)
		assert leading_zeros_32(u32(i)<<x) == 31 - x
	}
	// 64 bit
	i = 1
	for x in 0..64 {
		//C.printf("x:%016llx lz: %llu cmp: %d\n",u64(i)<<x,leading_zeros_64(u64(i)<<x), 63-x)
		assert leading_zeros_64(u64(i)<<x) == 63 - x
	}

	//
	// --- ones_count ---
	//

	// 8 bit
	i = 0
	for x in 0..9 {
		//C.printf("x:%02x lz: %llu cmp: %d\n",byte(i),ones_count_8(byte(i)), x)
		assert ones_count_8(byte(i)) == x
		i = (i << 1) + 1
	}
	// 16 bit
	i = 0
	for x in 0..17 {
		//C.printf("x:%04x lz: %llu cmp: %d\n",u16(i),ones_count_16(u16(i)), x)
		assert ones_count_16(u16(i)) == x
		i = (i << 1) + 1
	}
	// 32 bit
	i = 0
	for x in 0..33 {
		//C.printf("x:%08x lz: %llu cmp: %d\n",u32(i),ones_count_32(u32(i)), x)
		assert ones_count_32(u32(i)) == x
		i = (i << 1) + 1
	}
	// 64 bit
	i1 = 0
	for x in 0..65 {
		//C.printf("x:%016llx lz: %llu cmp: %d\n",u64(i1),ones_count_64(u64(i1)), x)
		assert ones_count_64(u64(i1)) == x
		i1 = (i1 << 1) + 1
	}

	//
	// --- rotate_left/right --- 
	//
	assert rotate_left_8( 0x12 , 4) == 0x21
	assert rotate_left_16( 0x1234 , 8) == 0x3412
	assert rotate_left_32( 0x12345678 , 16) == 0x56781234
	assert rotate_left_64( 0x1234567887654321 , 32) == 0x8765432112345678

	//
	// --- reverse ---
	//
	
	// 8 bit
	i = 0
	for x in 0..9 {
		mut rv := byte(0)
		mut bc := 0
		mut n := i
		for bc < 8 {
			rv = (rv << 1) | (n & 0x01)
			bc++
			n = n >> 1
		}
		//C.printf("x:%02x lz: %llu cmp: %d\n",byte(i),reverse_8(byte(i)), rv)
		assert reverse_8(byte(i)) == rv
		i = (i << 1) + 1
	}
	// 16 bit
	i = 0
	for x in 0..17 {
		mut rv := u16(0)
		mut bc := 0
		mut n := i
		for bc < 16 {
			rv = (rv << 1) | (n & 0x01)
			bc++
			n = n >> 1
		}
		//C.printf("x:%04x lz: %llu cmp: %d\n",u16(i),reverse_16(u16(i)), rv)
		assert reverse_16(u16(i)) == rv
		i = (i << 1) + 1
	}
	// 32 bit
	i = 0
	for x in 0..33 {
		mut rv := u32(0)
		mut bc := 0
		mut n := i
		for bc < 32 {
			rv = (rv << 1) | (n & 0x01)
			bc++
			n = n >> 1
		}
		//C.printf("x:%08x lz: %llu cmp: %d\n",u32(i),reverse_32(u32(i)), rv)
		assert reverse_32(u32(i)) == rv
		i = (i << 1) + 1
	}
	// 64 bit
	i1 = 0
	for x in 0..64 {
		mut rv := u64(0)
		mut bc := 0
		mut n := i1
		for bc < 64 {
			rv = (rv << 1) | (n & 0x01)
			bc++
			n = n >> 1
		}
		//C.printf("x:%016llx lz: %016llx cmp: %016llx\n",u64(i1),reverse_64(u64(i1)), rv)
		assert reverse_64(u64(i1)) == rv
		i1 = (i1 << 1) + 1
	}
}