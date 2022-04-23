module blowfish

pub struct Blowfish {
pub mut:
	p [18]u32
	s [4][256]u32
}

// new_cipher creates and returns a new Blowfish cipher.
// The key argument should be the Blowfish key, from 1 to 56 bytes.
pub fn new_cipher(key []u8) ?Blowfish {
	mut bf := Blowfish{}
	unsafe { vmemcpy(&bf.p[0], &p[0], int(sizeof(bf.p))) }
	unsafe { vmemcpy(&bf.s[0], &s[0], int(sizeof(bf.s))) }
	if key.len < 1 || key.len > 56 {
		return error('invalid key')
	}
	expand_key(key, mut bf)

	return bf
}

// new_salted_cipher returns a new Blowfish cipher that folds a salt into its key schedule.
pub fn new_salted_cipher(key []u8, salt []u8) ?Blowfish {
	if salt.len == 0 {
		return new_cipher(key)
	}
	mut bf := Blowfish{}
	unsafe { vmemcpy(&bf.p[0], &p[0], int(sizeof(bf.p))) }
	unsafe { vmemcpy(&bf.s[0], &s[0], int(sizeof(bf.s))) }
	if key.len < 1 {
		return error('invalid key')
	}
	expand_key_with_salt(key, salt, mut bf)
	return bf
}

// encrypt encrypts the 8-byte buffer src using the key k and stores the result in dst.
pub fn (mut bf Blowfish) encrypt(mut dst []u8, src []u8) {
	l := u32(src[0]) << 24 | u32(src[1]) << 16 | u32(src[2]) << 8 | u32(src[3])
	r := u32(src[4]) << 24 | u32(src[5]) << 16 | u32(src[6]) << 8 | u32(src[7])
	arr := setup_tables(l, r, mut bf)
	dst[0], dst[1], dst[2], dst[3] = u8(arr[0] >> 24), u8(arr[0] >> 16), u8(arr[0] >> 8), u8(arr[0])
	dst[4], dst[5], dst[6], dst[7] = u8(arr[1] >> 24), u8(arr[1] >> 16), u8(arr[1] >> 8), u8(arr[1])
}
