module base58

pub const alphabets = {
	'btc':    new_alphabet('123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz')
	'flickr': new_alphabet('123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ')
	'ripple': new_alphabet('rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz')
}

// Alphabet is the series of characters that an input
// will be encoded to and a decode table.
struct Alphabet {
mut:
	decode []i8   = []i8{len: 128, init: -1}
	encode []byte = []byte{len: 58}
}

// str returns an Alphabet encode table byte array as a string
pub fn (alphabet &Alphabet) str() string {
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
pub fn new_alphabet(str string) &Alphabet {
	if str.len != 58 {
		panic('base58 > new_alphabet(string): string must be 58 characters in length')
	}

	mut ret := &Alphabet{}
	copy(ret.encode, str.bytes())

	mut distinct := 0
	for i, b in ret.encode {
		if ret.decode[b] == -1 {
			distinct++
		}
		ret.decode[b] = i8(i)
	}

	if distinct != 58 {
		panic('base58 > new_alphabet(string): string must not contain repeating characters')
	}

	return ret
}
