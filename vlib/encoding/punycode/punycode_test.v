import encoding.punycode

struct Vector {
	name    string
	decoded string
	encoded string
}

// Every sample string from RFC 3492 section 7.1, in order. The decoded forms
// are spelled out as code point escapes so the file stays readable and so a
// stray editor normalisation cannot silently change a test.
const rfc_vectors = [
	Vector{
		name: '(A) Arabic (Egyptian)'
		decoded: 'ليهمابتكلموشعربي؟'
		encoded: 'egbpdaj6bu4bxfgehfvwxn'
	},
	Vector{
		name: '(B) Chinese (simplified)'
		decoded: '他们为什么不说中文'
		encoded: 'ihqwcrb4cv8a8dqg056pqjye'
	},
	Vector{
		name: '(C) Chinese (traditional)'
		decoded: '他們爲什麽不說中文'
		encoded: 'ihqwctvzc91f659drss3x8bo0yb'
	},
	Vector{
		name: '(D) Czech'
		decoded: 'Pročprostěnemluvíčesky'
		encoded: 'Proprostnemluvesky-uyb24dma41a'
	},
	Vector{
		name: '(E) Hebrew'
		decoded: 'למההםפשוטלאמדבריםעברית'
		encoded: '4dbcagdahymbxekheh6e0a7fei0b'
	},
	Vector{
		name: '(F) Hindi (Devanagari)'
		decoded: 'यहलोगहिन्दीक्योंनहींबोलसकतेहैं'
		encoded: 'i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd'
	},
	Vector{
		name: '(G) Japanese (kanji and hiragana)'
		decoded: 'なぜみんな日本語を話してくれないのか'
		encoded: 'n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa'
	},
	Vector{
		name: '(H) Korean (Hangul syllables)'
		decoded: '세계의모든사람들이한국어를이해한다면얼마나좋을까'
		encoded: '989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c'
	},
	Vector{
		name: '(I) Russian (Cyrillic)'
		decoded: 'почемужеонинеговорятпорусски'
		encoded: 'b1abfaaepdrnnbgefbaDotcwatmq2g4l'
	},
	Vector{
		name: '(J) Spanish'
		decoded: 'PorquénopuedensimplementehablarenEspañol'
		encoded: 'PorqunopuedensimplementehablarenEspaol-fmd56a'
	},
	Vector{
		name: '(K) Vietnamese'
		decoded: 'TạisaohọkhôngthểchỉnóitiếngViệt'
		encoded: 'TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g'
	},
	Vector{
		name: '(L) 3nen B gumi kinpachi sensei'
		decoded: '3年B組金八先生'
		encoded: '3B-ww4c5e180e575a65lsy2b'
	},
	Vector{
		name: '(M) amuro namie with SUPER MONKEYS'
		decoded: '安室奈美恵-with-SUPER-MONKEYS'
		encoded: '-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n'
	},
	Vector{
		name: '(N) Hello-Another-Way'
		decoded: 'Hello-Another-Way-それぞれの場所'
		encoded: 'Hello-Another-Way--fc4qua05auwb3674vfr0b'
	},
	Vector{
		name: '(O) hitotsu yane no shita 2'
		decoded: 'ひとつ屋根の下2'
		encoded: '2-u9tlzr9756bt3uc0v'
	},
	Vector{
		name: '(P) Maji de Koi suru 5 byou mae'
		decoded: 'MajiでKoiする5秒前'
		encoded: 'MajiKoi5-783gue6qz075azm5e'
	},
	Vector{
		name: '(Q) pafii de runba'
		decoded: 'パフィーdeルンバ'
		encoded: 'de-jg4avhby1noc0d'
	},
	Vector{
		name: '(R) sono supiido de'
		decoded: 'そのスピードで'
		encoded: 'd9juau41awczczp'
	},
	Vector{
		name: '(S) pure ASCII that breaks host name rules'
		decoded: '-> \$1.00 <-'
		encoded: '-> \$1.00 <--'
	},
]

fn test_encode_rfc_vectors() {
	for v in rfc_vectors {
		got := punycode.encode(v.decoded)!
		// The RFC prints its examples in mixed case for readability, but an
		// encoder may emit a single case, so compare case-insensitively.
		assert got.to_lower() == v.encoded.to_lower(), '${v.name}: got ${got}'
	}
}

fn test_decode_rfc_vectors() {
	for v in rfc_vectors {
		assert punycode.decode(v.encoded)! == v.decoded, v.name
	}
}

fn test_decode_accepts_both_cases() {
	// RFC 3492 section 5: a decoder MUST recognise the digits in either case,
	// including a mixture. Only vectors whose literal part carries no letters
	// qualify, since case is significant there.
	for v in rfc_vectors {
		if v.encoded.contains('-') {
			continue
		}
		assert punycode.decode(v.encoded.to_lower())! == v.decoded, v.name
		assert punycode.decode(v.encoded.to_upper())! == v.decoded, v.name
	}
	// A mixture of both, from vector (I), which the RFC prints that way.
	assert punycode.decode('b1abfaaepdrnnbgefbaDotcwatmq2g4l')! == punycode.decode('b1abfaaepdrnnbgefbadotcwatmq2g4l')!
}

fn test_round_trip() {
	for v in rfc_vectors {
		assert punycode.decode(punycode.encode(v.decoded)!)! == v.decoded, v.name
	}
}

fn test_real_domain_labels() {
	// The labels behind a few well known xn-- names.
	cases := {
		'münchen':   'mnchen-3ya'
		'bücher':    'bcher-kva'
		'例え':      'r8jz45g'
		'δοκιμή':    'jxalpdlp'
		'испытание': '80akhbyknj4f'
	}
	for decoded, encoded in cases {
		assert punycode.encode(decoded)! == encoded, decoded
		assert punycode.decode(encoded)! == decoded, encoded
	}
}

fn test_pure_ascii() {
	// A basic-only string encodes to itself plus the trailing delimiter.
	assert punycode.encode('example')! == 'example-'
	assert punycode.decode('example-')! == 'example'
	// Without a delimiter there is no literal part at all, so the whole input
	// is read as deltas and does not come back unchanged.
	assert punycode.decode('example')! != 'example'
}

fn test_empty() {
	assert punycode.encode('')! == ''
	assert punycode.decode('')! == ''
}

fn test_decode_rejects_invalid_digit() {
	// `!` carries no digit value.
	if _ := punycode.decode('a!b') {
		assert false, 'should reject an invalid digit'
	}
}

fn test_decode_rejects_truncated_delta() {
	// A delta whose digits all sit above the threshold never terminates.
	if _ := punycode.decode('zzzzzz') {
		assert false, 'should reject a truncated delta'
	}
}

fn test_decode_rejects_non_basic_literal() {
	// Everything before the last delimiter has to be basic ASCII.
	if _ := punycode.decode('münchen-abc') {
		assert false, 'should reject a non-basic literal part'
	}
}

fn test_decode_rejects_out_of_range_code_point() {
	// Deltas that walk past U+10FFFF must be reported rather than producing an
	// invalid rune.
	if _ := punycode.decode('z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9') {
		assert false, 'should reject a code point above U+10FFFF'
	}
}
