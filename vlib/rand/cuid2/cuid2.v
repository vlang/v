module cuid2

import rand
import time
import strconv
import crypto.sha3
import math.big
import os

const default_id_length = 24
const min_id_length = 2
const max_id_length = 32
// ~22k hosts before 50% chance of initial counter collision
const max_session_count = 476782367

// Cuid2Generator can be used to get secure, collision-resistant ids optimized for horizontal scaling and performance. Next generation UUIDs.
pub struct Cuid2Generator {
mut:
	// A counter that will be used to affect the entropy of
	// successive id generation calls
	session_counter i64
	// A unique string that will be used by the Cuid generator
	// to help prevent collisions when generating Cuids in a
	// distributed system.
	fingerprint string
pub mut:
	// A PRNG that has a PRNG interface
	prng &rand.PRNG = rand.get_current_rng()
	// Length of the generated Cuid, min = 2, max = 32, default = 24
	length int = default_id_length
}

@[params]
pub struct Cuid2Param {
pub mut:
	// A PRNG that has a PRNG interface
	prng &rand.PRNG = rand.get_current_rng()
	// Length of the generated Cuid, min = 2, max = 32, default = 24
	length int = default_id_length
}

// new Create a cuid2 UUID generator.
pub fn new(param Cuid2Param) Cuid2Generator {
	return Cuid2Generator{
		prng:   param.prng
		length: param.length
	}
}

// generate Generate a new cuid2 UUID.
// It is an alias to function `cuid2()`
pub fn (mut g Cuid2Generator) generate() string {
	return g.cuid2()
}

// cuid2 generates a random (cuid2) UUID.
// Secure, collision-resistant ids optimized for horizontal
// scaling and performance. Next generation UUIDs.
// Ported from https://github.com/paralleldrive/cuid2
pub fn (mut g Cuid2Generator) cuid2() string {
	if g.length < min_id_length || g.length > max_id_length {
		panic('cuid2 length(${g.length}) out of range: min=${min_id_length}, max=${max_id_length}')
	}

	mut prng := g.prng
	first_letter := prng.string(1).to_lower()
	now := strconv.format_int(time.now().unix_milli(), 36)
	if g.session_counter == 0 {
		// First call, init session counter, fingerprint.
		g.session_counter = i64(prng.f64() * max_session_count)
		g.fingerprint = create_fingerprint(mut prng, get_environment_key_string())
	}
	g.session_counter = g.session_counter + 1
	count := strconv.format_int(g.session_counter, 36)

	// The salt should be long enough to be globally unique
	// across the full length of the hash. For simplicity,
	// we use the same length as the intended id output.
	salt := create_entropy(g.length, mut prng)
	hash_input := now + salt + count + g.fingerprint
	hash_digest := first_letter + hash(hash_input)[1..g.length]
	return hash_digest
}

// next generates a new cuid2 UUID.
// It is an alias to function `cuid2()`
pub fn (mut g Cuid2Generator) next() ?string {
	return g.cuid2()
}

// is_cuid checks whether a given `cuid` has a valid form and length.
pub fn is_cuid(cuid string) bool {
	if cuid.len < min_id_length || cuid.len > max_id_length {
		return false
	}

	// first letter should in [a..z]
	if cuid[0] < u8(`a`) || cuid[0] > u8(`z`) {
		return false
	}

	// other letter should in [a..z,0..9]
	for letter in cuid[1..] {
		if (letter >= u8(`a`) && letter <= u8(`z`)) || (letter >= u8(`0`) && letter <= u8(`9`)) {
			continue
		}
		return false
	}
	return true
}

fn create_entropy(length int, mut prng rand.PRNG) string {
	mut entropy := ''
	for entropy.len < length {
		randomness := i64(prng.f64() * 36)
		entropy += strconv.format_int(randomness, 36)
	}
	return entropy
}

// create_fingerprint This is a fingerprint of the host environment.
// It is used to help prevent collisions when generating ids in a
// distributed system. If no global object is available, you can
// pass in your own, or fall back on a random string.
fn create_fingerprint(mut prng rand.PRNG, env_key_string string) string {
	mut source_string := create_entropy(max_id_length, mut prng)
	if env_key_string.len > 0 {
		source_string += env_key_string
	}
	source_string_hash := hash(source_string)
	return source_string_hash[1..]
}

fn hash(input string) string {
	mut hash := sha3.new512() or { panic(err) }
	hash.write(input.bytes()) or { panic(err) }
	hash_digest := hash.checksum()

	// Drop the first character because it will bias
	// the histogram to the left.
	return big.integer_from_bytes(hash_digest).radix_str(36)[1..]
}

fn get_environment_key_string() string {
	env := os.environ()
	mut keys := []string{}

	// Discard values of environment variables
	for _, variable in env {
		index := variable.index('=') or { variable.len }
		key := variable[..index]
		keys << key
	}
	return keys.join('')
}
