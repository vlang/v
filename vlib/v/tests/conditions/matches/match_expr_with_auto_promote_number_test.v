// convert strings like 10K to i164
const block = 512
// const block = i64(512)

// **1
const kilo = i64(1024)
const kilobyte = i64(1000)

// **2
const mega = kilo * kilo
const megabyte = kilobyte * kilobyte

// **3
const giga = mega * kilo
const gigabyte = megabyte * kilobyte

// **4
const terra = giga * kilo
const terrabyte = gigabyte * kilobyte

// **5
const peta = mega * kilo
const petabyte = megabyte * kilobyte

// **6
const exa = peta * kilo
const exabyte = peta * kilo

// **7
const zetta = exa * kilo
const zettabyte = exabyte * kilobyte

// **8
const yotta = zetta * kilo
const yottabyte = zettabyte * kilobyte

// **9
const ronna = yotta * kilo
const ronnabyte = yottabyte * kilobyte

// **10
const quetta = ronna * kilo
const quettabyte = ronnabyte * kilobyte

fn string_to_i64(s string) ?i64 {
	if s.len == 0 {
		return none
	}

	mut index := 0
	for index < s.len {
		match true {
			s[index].is_digit() {}
			s[index] == `+` && index == 0 {}
			s[index] == `-` && index == 0 {}
			else { break }
		}
		index += 1
	}

	number := s[0..index].i64()
	suffix := if index < s.len { s[index..] } else { 'c' }

	multiplier := match suffix.to_lower_ascii() {
		'b' { block }
		'k' { kilo }
		'kb', 'kib' { kilobyte }
		'm' { mega }
		'mb', 'mib' { megabyte }
		'g' { giga }
		'gb', 'gib' { gigabyte }
		't' { terra }
		'tb', 'tib' { terrabyte }
		'p' { peta }
		'pb', 'pib' { petabyte }
		'e' { exa }
		'eb', 'eib' { exabyte }
		'z' { zetta }
		'zb', 'zib' { zettabyte }
		'y' { yotta }
		'yb', 'yib' { yottabyte }
		'r' { ronna }
		'rb', 'rib' { ronnabyte }
		'q' { quetta }
		'qb', 'qib' { quettabyte }
		// oddball formats found in __xstrtol source
		'c' { 1 }
		'w' { 2 }
		else { return none }
	}

	result := number * multiplier
	if result == 0 && number != 0 {
		return none
	}
	return result
}

fn test_match_expr_with_auto_promote_number() {
	assert string_to_i64('19P')! == 19 * peta
	assert string_to_i64('18T')! == 18 * terra
}
