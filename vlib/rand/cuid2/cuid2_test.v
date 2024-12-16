module cuid2

import rand.musl
import rand.mt19937

fn check_valid_cuid2(uuid string, length int) bool {
	if uuid.len != length || uuid.len < min_id_length || uuid.len > max_id_length {
		return false
	}

	// first letter should in [a..z]
	if uuid[0] < u8(`a`) || uuid[0] > u8(`z`) {
		return false
	}

	// other letter should in [a..z,0..9]
	for letter in uuid[1..] {
		if (letter >= u8(`a`) && uuid[0] <= u8(`z`)) || (letter >= u8(`0`) && letter <= u8(`9`)) {
			continue
		}
		return false
	}
	return true
}

fn test_cuid2() {
	// default prng(wyrand)
	uuid2 := cuid2(length: 2)
	assert check_valid_cuid2(uuid2, 2)
	uuid24 := cuid2(length: 24)
	assert check_valid_cuid2(uuid24, 24)
	uuid32 := cuid2(length: 32)
	assert check_valid_cuid2(uuid32, 32)

	// musl prng
	musl_prng := &musl.MuslRNG{}
	uuid_musl := cuid2(prng: musl_prng, length: 32)
	assert check_valid_cuid2(uuid_musl, 32)

	// mt19937 prng
	mt19937_prng := &mt19937.MT19937RNG{}
	uuid_mt19937 := cuid2(prng: mt19937_prng, length: 32)
	assert check_valid_cuid2(uuid_mt19937, 32)

	// successive calls
	uuid_1 := cuid2()
	uuid_2 := cuid2()
	uuid_3 := cuid2()
	uuid_4 := cuid2()
	uuid_5 := cuid2()

	eprintln(uuid_1)
	eprintln(uuid_2)
	eprintln(uuid_3)
	eprintln(uuid_4)
	eprintln(uuid_5)

	assert check_valid_cuid2(uuid_1, default_id_length)
	assert check_valid_cuid2(uuid_2, default_id_length)
	assert check_valid_cuid2(uuid_3, default_id_length)
	assert check_valid_cuid2(uuid_4, default_id_length)
	assert check_valid_cuid2(uuid_5, default_id_length)

	// successive calls to cuid2 in a row should be unique
	assert uuid_1 != uuid_2
	assert uuid_1 != uuid_3
	assert uuid_1 != uuid_4
	assert uuid_1 != uuid_5
	assert uuid_2 != uuid_3
	assert uuid_2 != uuid_4
	assert uuid_2 != uuid_5
	assert uuid_3 != uuid_4
	assert uuid_3 != uuid_5
	assert uuid_4 != uuid_5
}
