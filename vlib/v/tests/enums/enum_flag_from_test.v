@[flag]
pub enum Flag as u8 {
	flag0
	flag1
	flag2
	flag3
	flag4
	flag5
	flag6
	flag7
}

fn test_ok() {
	a := Flag.from(0b10101110) or { panic(err) }
	assert a == Flag.flag1 | .flag2 | .flag3 | .flag5 | .flag7
}

fn test_fail() {
	a := Flag.from(0b110101110) or { Flag(Flag.flag0) }
	assert a == Flag.flag0
}
