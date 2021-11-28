module blowfish

pub struct Blowfish {
pub mut:
	p [18]u32
	s [4][256]u32
}

pub fn new_cipher(key []byte) ?Blowfish {
	mut bf := Blowfish{p, s}
	if key.len < 1 || key.len > 56 {
		return error('invalid key')
	}
	expand_key(key, mut bf)

	return bf
}

pub fn new_salted_cipher(key []byte, salt []byte) ?Blowfish {
	if salt.len == 0 {
		return new_cipher(key)
	}
	mut bf := Blowfish{p, s}
	if key.len < 1 {
		return error('invalid key')
	}
	expand_key_with_salt(key, salt, mut bf)
	return bf
}

pub fn (mut bf Blowfish) encrypt(mut dst []byte, src []byte) {
	l := u32(src[0]) << 24 | u32(src[1]) << 16 | u32(src[2]) << 8 | u32(src[3])
	r := u32(src[4]) << 24 | u32(src[5]) << 16 | u32(src[6]) << 8 | u32(src[7])
	arr := encrypt_block(l, r, mut bf)
	dst[0], dst[1], dst[2], dst[3] = byte(arr[0] >> 24), byte(arr[0] >> 16), byte(arr[0] >> 8), byte(arr[0])
	dst[4], dst[5], dst[6], dst[7] = byte(arr[1] >> 24), byte(arr[1] >> 16), byte(arr[1] >> 8), byte(arr[1])
}
