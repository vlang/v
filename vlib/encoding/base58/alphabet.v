module base58

// alphabets is a map of common base58 alphabets
pub const alphabets = init_alphabets()

// init_alphabet instantiates the preconfigured `Alphabet`s and returns them as `map[string]Alphabet`.
// This is a temporary function. Setting const alphabets to the value returned in this function
// causes a C error right now.
fn init_alphabets() map[string]Alphabet {
	return {
		'btc':    new_alphabet('123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz') or {
			panic(@MOD + '.' + @FN + ': this should never happen')
		}
		'flickr': new_alphabet('123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ') or {
			panic(@MOD + '.' + @FN + ': this should never happen')
		}
		'ripple': new_alphabet('rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz') or {
			panic(@MOD + '.' + @FN + ': this should never happen')
		}
	}
}

// Alphabet is the series of characters that an input
// will be encoded to and a decode table.
struct Alphabet {
mut:
	decode []i8   = []i8{len: 128, init: -1}
	encode []byte = []byte{len: 58}
}

// str returns an Alphabet encode table byte array as a string
pub fn (alphabet Alphabet) str() string {
	// i guess i had a brain fart here. Why would I actually use this code?!
	// mut str := []byte{}
	// for entry in alphabet.encode {
	// 	str << entry
	// }
	// return str.bytestr()
	return alphabet.encode.bytestr()
}

// new_alphabet instantiates an Alphabet object based on
// the provided characters
pub fn new_alphabet(str string) ?Alphabet {
	if str.len != 58 {
		return error(@MOD + '.' + @FN + ': string must be 58 characters in length')
	}

	mut ret := Alphabet{}
	copy(mut ret.encode, str.bytes())

	mut distinct := 0
	for i, b in ret.encode {
		if ret.decode[b] == -1 {
			distinct++
		}
		ret.decode[b] = i8(i)
	}

	if distinct != 58 {
		return error(@MOD + '.' + @FN + ': string must not contain repeating characters')
	}

	return ret
}
