module base58

fn test_alphabet_str_returns_the_encode_table() {
	assert btc_alphabet.str() == '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
	assert flickr_alphabet.str() == '123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ'
	assert ripple_alphabet.str() == 'rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz'
	for _, a in alphabets {
		assert a.str().len == 58
	}
}

fn test_alphabet_decode_maps_every_byte_back_to_its_index() {
	for _, a in alphabets {
		for i, b in a.encode {
			assert a.decode[b] == i8(i), '${a.str()} at ${int(b)}'
		}
	}
}

// The Bitcoin alphabet deliberately leaves out the four characters that are
// hardest to tell apart in a font, and the decode table is what enforces that.
fn test_btc_alphabet_omits_the_visually_ambiguous_characters() {
	assert btc_alphabet.decode[`1`] == 0
	assert btc_alphabet.decode[`z`] == 57
	assert btc_alphabet.decode[`0`] == -1
	assert btc_alphabet.decode[`O`] == -1
	assert btc_alphabet.decode[`I`] == -1
	assert btc_alphabet.decode[`l`] == -1
}

fn test_alphabets_map_names_each_builtin_alphabet() {
	assert alphabets.len == 3
	assert alphabets['btc'].str() == btc_alphabet.str()
	assert alphabets['flickr'].str() == flickr_alphabet.str()
	assert alphabets['ripple'].str() == ripple_alphabet.str()
}

fn test_new_alphabet_rejects_a_length_other_than_58() ! {
	for s in ['a'.repeat(57), 'a'.repeat(59)] {
		if _ := new_alphabet(s) {
			return error('new_alphabet accepted a ${s.len} character alphabet')
		} else {
			assert err.msg() == 'base58.new_alphabet: string must be 58 characters in length'
		}
	}
}

fn test_new_alphabet_rejects_repeating_characters() ! {
	dupe := btc_alphabet.str()[..57] + '2'
	assert dupe.len == 58
	if _ := new_alphabet(dupe) {
		return error('new_alphabet accepted an alphabet with a repeated character')
	} else {
		assert err.msg() == 'base58.new_alphabet: string must not contain repeating characters'
	}
}

fn test_each_named_alphabet_round_trips_its_own_output() {
	for _, a in alphabets {
		for input in ['', 'hello world', '\x00\x00hello world'] {
			enc := encode_walpha_bytes(input.bytes(), a)
			dec := decode_walpha_bytes(enc, a) or { panic(err) }
			assert dec == input.bytes(), '${a.str()} round trip of `${input}`'
		}
	}
}

fn test_named_alphabets_do_not_share_an_encoding() {
	// Three alphabets over the same bytes, so a single expected value per
	// alphabet is enough to show they are really wired up separately.
	assert encode_walpha('hello world', btc_alphabet) == 'StV1DL6CwTryKyV'
	assert encode_walpha('hello world', flickr_alphabet) == 'rTu1dk6cWsRYjYu'
	assert encode_walpha('hello world', ripple_alphabet) == 'StVrDLaUATiyKyV'
}

fn test_encode_bytes_agrees_with_encode_on_strings() {
	for input in ['', 'hello world', '\x00\x00hello world'] {
		assert encode_bytes(input.bytes()) == encode(input).bytes(), '`${input}`'
	}
}

fn test_decode_bytes_round_trips() {
	for input in ['', 'hello world', '\x00\x00hello world'] {
		dec := decode_bytes(encode_bytes(input.bytes())) or { panic(err) }
		assert dec == input.bytes(), '`${input}`'
	}
}

fn test_decode_bytes_reports_the_offending_byte_value() ! {
	for c in [`0`, `O`, `I`, `l`] {
		if _ := decode_bytes([c]) {
			return error('decoded `${c}`, which the Bitcoin alphabet does not contain')
		} else {
			assert err.msg() == 'base58.decode_walpha_bytes: invalid base58 digit (${int(c)})'
		}
	}
}
