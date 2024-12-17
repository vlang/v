module cuid2

import rand.musl
import rand.mt19937

fn test_cuid2() {
	// default prng(wyrand), default id length = 24
	mut g24 := new()
	uuid24 := g24.generate()
	assert uuid24.len == 24
	assert is_cuid(uuid24)

	// default prng(wyrand), id length = 2
	mut g2 := new(length: 2)
	uuid2 := g2.generate()
	assert uuid2.len == 2
	assert is_cuid(uuid2)

	// default prng(wyrand), id length = 32
	mut g32 := new(length: 32)
	uuid32 := g32.generate()
	assert uuid32.len == 32
	assert is_cuid(uuid32)

	// musl prng, id length = 28
	mut g_musl := new(prng: &musl.MuslRNG{}, length: 28)
	uuid_musl := g_musl.generate()
	assert uuid_musl.len == 28
	assert is_cuid(uuid_musl)

	// mt19937 prng, default id length = 24
	mut g_mt19937 := new(prng: &mt19937.MT19937RNG{})
	uuid_mt19937 := g_mt19937.generate()
	assert uuid_mt19937.len == 24
	assert is_cuid(uuid_mt19937)

	// successive calls
	// default prng(wyrand), default id length = 24
	mut g := new()
	mut ids := []string{}
	for id in g {
		eprintln(id)
		// id length should be default length(24)
		assert id.len == 24
		assert is_cuid(id)

		ids << id
		if ids.len == 5 {
			break
		}
	}

	// successive calls to g.next() in a row should be unique
	assert ids[0] != ids[1]
	assert ids[0] != ids[2]
	assert ids[0] != ids[3]
	assert ids[0] != ids[4]
	assert ids[1] != ids[2]
	assert ids[1] != ids[3]
	assert ids[1] != ids[4]
	assert ids[2] != ids[3]
	assert ids[2] != ids[4]
	assert ids[3] != ids[4]
}
