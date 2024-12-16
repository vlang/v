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
	// default prng(wyrand), default id length = 24
	mut g24 := Cuid2Generator{}
	uuid24 := g24.cuid2()
	assert check_valid_cuid2(uuid24, 24)

	// default prng(wyrand), length = 2
	mut g2 := Cuid2Generator{
		length: 2
	}
	uuid2 := g2.cuid2()
	assert check_valid_cuid2(uuid2, 2)

	// default prng(wyrand), length = 32
	mut g32 := Cuid2Generator{
		length: 32
	}
	uuid32 := g32.cuid2()
	assert check_valid_cuid2(uuid32, 32)

	// musl prng
	mut g_musl := Cuid2Generator{
		prng: &musl.MuslRNG{}
	}
	uuid_musl := g_musl.cuid2()
	assert check_valid_cuid2(uuid_musl, 24)

	// mt19937 prng
	mut g_mt19937 := Cuid2Generator{
		prng: &mt19937.MT19937RNG{}
	}
	uuid_mt19937 := g_mt19937.cuid2()
	assert check_valid_cuid2(uuid_mt19937, 24)

	// successive calls
	mut g := Cuid2Generator{}
	uuid_1 := g.cuid2()
	uuid_2 := g.cuid2()
	uuid_3 := g.cuid2()
	uuid_4 := g.cuid2()
	uuid_5 := g.cuid2()

	eprintln(uuid_1)
	eprintln(uuid_2)
	eprintln(uuid_3)
	eprintln(uuid_4)
	eprintln(uuid_5)

	assert check_valid_cuid2(uuid_1, 24)
	assert check_valid_cuid2(uuid_2, 24)
	assert check_valid_cuid2(uuid_3, 24)
	assert check_valid_cuid2(uuid_4, 24)
	assert check_valid_cuid2(uuid_5, 24)

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
