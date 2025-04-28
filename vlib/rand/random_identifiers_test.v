import time
import rand

fn validate_separators(uuid string) {
	assert uuid[8] == `-`
	assert uuid[13] == `-`
	assert uuid[18] == `-`
	assert uuid[23] == `-`
	assert uuid.len == 36
}

// uuid_v4:
fn test_rand_uuid_v4() {
	uuid1 := rand.uuid_v4()
	uuid2 := rand.uuid_v4()
	uuid3 := rand.uuid_v4()
	validate_separators(uuid1)
	validate_separators(uuid2)
	validate_separators(uuid3)
	assert uuid1 != uuid2
	assert uuid1 != uuid3
	assert uuid2 != uuid3
	for i in 0 .. 1000 {
		x := rand.uuid_v4()
		// check the version field is always 4:
		assert x[14] == `4`
		// and the clock_seq_hi_and_reserved field is valid too:
		assert x[19] in [`8`, `9`, `a`, `b`]
		validate_separators(x)
	}
}

// uuid_v7:
fn test_rand_uuid_v7() {
	uuid1 := rand.uuid_v7()
	uuid2 := rand.uuid_v7()
	uuid3 := rand.uuid_v7()
	validate_separators(uuid1)
	validate_separators(uuid2)
	validate_separators(uuid3)
	assert uuid1 != uuid2
	assert uuid1 != uuid3
	assert uuid2 != uuid3
	for i in 0 .. 1000 {
		x := rand.uuid_v7()
		// check the version field is always 7:
		assert x[14] == `7`
		// and variant field is always 0b10:
		assert x[19] in [`8`, `9`, `a`, `b`]
		validate_separators(x)
	}
}

// uuid_v7_session:
fn test_rand_uuid_v7_session() {
	mut u := rand.new_uuid_v7_session()
	uuid1 := u.next()
	uuid2 := u.next()
	uuid3 := u.next()
	assert uuid1 != uuid2
	assert uuid1 != uuid3
	assert uuid2 != uuid3
	assert uuid1.len == 36
	assert uuid2.len == 36
	assert uuid3.len == 36
	mut prev_counter := `3`
	for i in 0 .. 1000 {
		x := u.next()
		// check the version field is always 7:
		assert x[14] == `7`
		// and variant field is always 0b10:
		assert x[19] in [`8`, `9`, `a`, `b`]

		// verify counter increase
		assert x[17] == prev_counter
		if prev_counter == `9` {
			prev_counter = `a`
		} else if prev_counter == `f` {
			prev_counter = `0`
		} else {
			prev_counter++
		}
	}
}

// ulids:
fn test_ulids_are_unique() {
	ulid1 := rand.ulid()
	ulid2 := rand.ulid()
	ulid3 := rand.ulid()
	assert ulid1.len == 26
	assert ulid2.len == 26
	assert ulid3.len == 26
	assert ulid1 != ulid2
	assert ulid1 != ulid3
	assert ulid2 != ulid3
}

fn test_ulids_max_start_character_is_ok() {
	ulid1 := rand.ulid()
	// the largest valid ULID encoded in Base32 is 7ZZZZZZZZZZZZZZZZZZZZZZZZZ
	assert (int(ulid1[0]) - 48) <= 7
}

fn test_ulids_generated_in_the_same_millisecond_have_the_same_prefix() {
	t := u64(time.utc().unix_milli())
	mut ulid1 := ''
	mut ulid2 := ''
	mut ulid3 := ''
	ulid1 = rand.ulid_at_millisecond(t)
	ulid2 = rand.ulid_at_millisecond(t)
	ulid3 = rand.ulid_at_millisecond(t)
	ulid1_prefix := ulid1[0..10]
	ulid2_prefix := ulid2[0..10]
	ulid3_prefix := ulid3[0..10]
	assert ulid1_prefix == ulid2_prefix
	assert ulid1_prefix == ulid3_prefix
}

fn test_ulids_should_be_lexicographically_ordered_when_not_in_same_millisecond() {
	ulid1 := rand.ulid()
	time.sleep(1 * time.millisecond)
	ulid2 := rand.ulid()
	time.sleep(1 * time.millisecond)
	ulid3 := rand.ulid()
	mut all := [ulid3, ulid2, ulid1]
	// eprintln('all before: $all')
	all.sort()
	// eprintln('all  after: $all')
	s1 := all[0]
	s2 := all[1]
	s3 := all[2]
	assert s1 == ulid1
	assert s2 == ulid2
	assert s3 == ulid3
}
