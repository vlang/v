fn main() {
	a := u32(0x1234)
	b := u32(0x56)
	c := u32(0x78)
	packed := (a >> 3) | (b << 13) | (c << 29)
	println(packed)
}
