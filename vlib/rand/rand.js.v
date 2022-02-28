module rand

import time

// init initializes the default RNG.
fn init() {
	default_rng = new_default()
}

pub fn string(len int) string {
	result := ''
	#
	#const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
	#const charactersLength = characters.length;
	#for (let i = 0;i < len.val;i++)
	#result.str += characters.charAt(Math.random() * charactersLength);

	return result
}

const (
	ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'
)

// ulid generates an Unique Lexicographically sortable IDentifier.
// See https://github.com/ulid/spec .
// NB: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn ulid() string {
	return ulid_at_millisecond(u64(time.utc().unix_time_milli()))
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_time_milli`.
pub fn ulid_at_millisecond(unix_time_milli u64) string {
	mut buf := []byte{cap: 27}
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		buf[i] = rand.ulid_encoding[int(t & 0x1f)]
		t = t >> 5
		i--
	}

	mut x := default_rng.u64()
	i = 10
	for i < 19 {
		buf[i] = rand.ulid_encoding[int(x & 0x1f)]

		x = x >> 5
		i++
	}

	x = default_rng.u64()
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

fn read_internal(mut rng PRNG, mut buf []byte) {
	for i in 0 .. buf.len {
		buf[i] = rng.byte()
	}
}
