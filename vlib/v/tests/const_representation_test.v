const zzz_byte_a = u8(`A`)

const zzz_u16_a = u16(999) + 5

const zzza = u64(123)

const zzzb = 5 + zzzc

const zzzc = 6 + zzza

const zzzx = zzza - 124

const zzz_zz = i64(-1)

struct Abc {
	x int
}

const zzz_struct = Abc{123}

const zzzs = 'xyz' + 'abc'

fn test_number_consts() {
	assert zzz_byte_a.hex_full() == '41'
	assert zzz_u16_a.hex_full() == '03ec'
	assert zzza.hex_full() == '000000000000007b'
	assert zzzb.hex_full() == '0000000000000086'
	assert zzzc.hex_full() == '0000000000000081'
	// assert zzzx.hex_full() == '00000000ffffffff' // TODO: see why
	assert zzz_zz.hex_full() == 'ffffffffffffffff'
}

fn test_struct_consts() {
	assert zzz_struct.str().contains('x: 123')
}

fn test_string_consts() {
	assert zzzs == 'xyzabc'
}
