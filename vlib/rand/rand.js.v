module rand

// init initializes the default RNG.
fn init() {
	default_rng = new_default()
}

fn internal_string_from_set(mut rng PRNG, charset string, len int) string {
	result := ''
	#
	#const characters = charset.str;
	#const charactersLength = characters.length;
	#for (let i = 0;i < len.val;i++)
	#result.str += characters.charAt(Math.random() * charactersLength);

	return result
}

const (
	ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'
)

fn internal_ulid_at_millisecond(mut rng PRNG, unix_time_milli u64) string {
	mut buf := []u8{cap: 27}
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		buf[i] = rand.ulid_encoding[int(t & 0x1f)]
		t = t >> 5
		i--
	}

	mut x := rng.u64()
	i = 10
	for i < 19 {
		buf[i] = rand.ulid_encoding[int(x & 0x1f)]

		x = x >> 5
		i++
	}

	x = rng.u64()
	for i < 26 {
		buf[i] = rand.ulid_encoding[int(x & 0x1f)]
		x = x >> 5
		i++
	}

	res := ''
	println(buf)
	#res.str = buf.arr.arr.map(String.fromCharCode).join('')

	return res
}

fn read_internal(mut rng PRNG, mut buf []u8) {
	for i in 0 .. buf.len {
		buf[i] = rng.u8()
	}
}
