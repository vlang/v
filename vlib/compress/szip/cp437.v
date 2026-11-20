// Entry names are bytes, not text: the format stores them either as UTF-8 —
// flagged by bit 11 of the general purpose flags — or as IBM code page 437,
// the character set of MS-DOS, which is what every writer produced before
// that flag existed and what several still produce today.
//
// Names are decoded here so that a caller gets the same string whichever
// writer produced the archive. The raw bytes are kept alongside, so that an
// archive rewritten in append mode stores every existing name unchanged.
module szip

import encoding.utf8

// cp437_high maps the upper half of code page 437 to its code points. The
// lower half is ASCII and needs no table.
const cp437_high = [
	rune(0x00c7),
	rune(0x00fc),
	rune(0x00e9),
	rune(0x00e2),
	rune(0x00e4),
	rune(0x00e0),
	rune(0x00e5),
	rune(0x00e7), // 0x80 Çüéâäàåç
	rune(0x00ea),
	rune(0x00eb),
	rune(0x00e8),
	rune(0x00ef),
	rune(0x00ee),
	rune(0x00ec),
	rune(0x00c4),
	rune(0x00c5), // 0x88 êëèïîìÄÅ
	rune(0x00c9),
	rune(0x00e6),
	rune(0x00c6),
	rune(0x00f4),
	rune(0x00f6),
	rune(0x00f2),
	rune(0x00fb),
	rune(0x00f9), // 0x90 ÉæÆôöòûù
	rune(0x00ff),
	rune(0x00d6),
	rune(0x00dc),
	rune(0x00a2),
	rune(0x00a3),
	rune(0x00a5),
	rune(0x20a7),
	rune(0x0192), // 0x98 ÿÖÜ¢£¥₧ƒ
	rune(0x00e1),
	rune(0x00ed),
	rune(0x00f3),
	rune(0x00fa),
	rune(0x00f1),
	rune(0x00d1),
	rune(0x00aa),
	rune(0x00ba), // 0xa0 áíóúñÑªº
	rune(0x00bf),
	rune(0x2310),
	rune(0x00ac),
	rune(0x00bd),
	rune(0x00bc),
	rune(0x00a1),
	rune(0x00ab),
	rune(0x00bb), // 0xa8 ¿⌐¬½¼¡«»
	rune(0x2591),
	rune(0x2592),
	rune(0x2593),
	rune(0x2502),
	rune(0x2524),
	rune(0x2561),
	rune(0x2562),
	rune(0x2556), // 0xb0 ░▒▓│┤╡╢╖
	rune(0x2555),
	rune(0x2563),
	rune(0x2551),
	rune(0x2557),
	rune(0x255d),
	rune(0x255c),
	rune(0x255b),
	rune(0x2510), // 0xb8 ╕╣║╗╝╜╛┐
	rune(0x2514),
	rune(0x2534),
	rune(0x252c),
	rune(0x251c),
	rune(0x2500),
	rune(0x253c),
	rune(0x255e),
	rune(0x255f), // 0xc0 └┴┬├─┼╞╟
	rune(0x255a),
	rune(0x2554),
	rune(0x2569),
	rune(0x2566),
	rune(0x2560),
	rune(0x2550),
	rune(0x256c),
	rune(0x2567), // 0xc8 ╚╔╩╦╠═╬╧
	rune(0x2568),
	rune(0x2564),
	rune(0x2565),
	rune(0x2559),
	rune(0x2558),
	rune(0x2552),
	rune(0x2553),
	rune(0x256b), // 0xd0 ╨╤╥╙╘╒╓╫
	rune(0x256a),
	rune(0x2518),
	rune(0x250c),
	rune(0x2588),
	rune(0x2584),
	rune(0x258c),
	rune(0x2590),
	rune(0x2580), // 0xd8 ╪┘┌█▄▌▐▀
	rune(0x03b1),
	rune(0x00df),
	rune(0x0393),
	rune(0x03c0),
	rune(0x03a3),
	rune(0x03c3),
	rune(0x00b5),
	rune(0x03c4), // 0xe0 αßΓπΣσµτ
	rune(0x03a6),
	rune(0x0398),
	rune(0x03a9),
	rune(0x03b4),
	rune(0x221e),
	rune(0x03c6),
	rune(0x03b5),
	rune(0x2229), // 0xe8 ΦΘΩδ∞φε∩
	rune(0x2261),
	rune(0x00b1),
	rune(0x2265),
	rune(0x2264),
	rune(0x2320),
	rune(0x2321),
	rune(0x00f7),
	rune(0x2248), // 0xf0 ≡±≥≤⌠⌡÷≈
	rune(0x00b0),
	rune(0x2219),
	rune(0x00b7),
	rune(0x221a),
	rune(0x207f),
	rune(0x00b2),
	rune(0x25a0),
	rune(0x00a0), // 0xf8 °∙·√ⁿ²■ 
]!

// decode_name turns the raw bytes of an entry name into a string. Bit 11 is
// trusted when set; when it is clear, bytes that happen to be valid UTF-8
// are kept as they are — writers that emit UTF-8 without setting the flag
// are common — and anything else is decoded as CP437.
fn decode_name(raw []u8, flags u16) string {
	if raw.len == 0 {
		return ''
	}
	if flags & 0x0800 != 0 {
		return raw.bytestr()
	}
	mut high := false
	for b in raw {
		if b >= 0x80 {
			high = true
			break
		}
	}
	if !high {
		return raw.bytestr()
	}
	if utf8.validate(raw.data, raw.len) {
		return raw.bytestr()
	}
	return decode_cp437(raw)
}

// decode_cp437 decodes `raw` as IBM code page 437. Every byte maps to a
// character, so this never fails.
fn decode_cp437(raw []u8) string {
	mut sb := []u8{cap: raw.len + raw.len / 2}
	for b in raw {
		if b < 0x80 {
			sb << b
			continue
		}
		sb << utf32_to_str(u32(cp437_high[b - 0x80])).bytes()
	}
	return sb.bytestr()
}
