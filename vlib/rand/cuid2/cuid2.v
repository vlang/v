@[has_globals]
module cuid2

import rand
import time
import strconv
import crypto.sha3
import math.big
import os

// A counter that will be used to affect the entropy of
// successive id generation calls
__global session_counter = i64(0)
// A unique string that will be used by the Cuid generator
// to help prevent collisions when generating Cuids in a
// distributed system.
__global fingerprint string

pub const default_id_length = 24
pub const min_id_length = 2
pub const max_id_length = 32
// ~22k hosts before 50% chance of initial counter collision
pub const max_session_count = 476782367

@[params]
pub struct Cuid2Params {
pub mut:
	// A PRNG that has a PRNG interface
	prng &rand.PRNG = rand.get_current_rng()
	// Length of the generated Cuid, min = 2, max = 32, default = 24
	length int = default_id_length
}

// cuid2 generates a random (cuid2) UUID.
// Secure, collision-resistant ids optimized for horizontal
// scaling and performance. Next generation UUIDs.
// Ported from https://github.com/paralleldrive/cuid2
pub fn cuid2(param Cuid2Params) string {
	if param.length < min_id_length || param.length > max_id_length {
		panic('cuid2 length(${param.length}) out of range: min=${min_id_length}, max=${max_id_length}')
	}

	mut prng := param.prng
	first_letter := prng.string(1).to_lower()
	now := strconv.format_int(time.now().unix_milli(), 36)
	if session_counter == 0 {
		// First call, init session counter, fingerprint.
		session_counter = i64(prng.f64() * max_session_count)
		fingerprint = create_fingerprint(mut prng, get_environment_key_string())
	}
	session_counter = session_counter + 1
	count := strconv.format_int(session_counter, 36)

	// The salt should be long enough to be globally unique
	// across the full length of the hash. For simplicity,
	// we use the same length as the intended id output.
	salt := create_entropy(param.length, mut prng)
	hash_input := now + salt + count + fingerprint
	hash_digest := first_letter + hash(hash_input)[1..param.length]
	return hash_digest
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
