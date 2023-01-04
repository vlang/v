// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module utf8

// for unicode type fast lookup
const (
	p_c      = 1 // a control character.
	p_p      = 2 // a punctuation character.
	p_n      = 4 // a numeral.
	p_s      = 8 // a symbolic character.
	p_z      = 16 // a spacing character.
	p_lu     = 32 // an up_prer-case letter.
	p_ll     = 64 // a lower-case letter.
	p_pr     = 128 // a printable character according to Go's definition.
	p_g      = p_pr | p_z // a graphical character according to the Unicode definition.
	p_lo     = p_lu | p_ll // a letter that is neither up_prer nor lower case.
	p_l_mask = p_lo
)

const props = [
	p_c
	//'\x00'
	p_c
	//'\x01'
	p_c
	//'\x02'
	p_c
	//'\x03'
	p_c
	//'\x04'
	p_c
	//'\x05'
	p_c
	//'\x06'
	p_c
	//'\a'
	p_c
	//'\b'
	p_c
	//'\t'
	p_c
	//'\n'
	p_c
	//'\v'
	p_c
	//'\f'
	p_c
	//'\r'
	p_c
	//'\x0e'
	p_c
	//'\x0f'
	p_c
	//'\x10'
	p_c
	//'\x11'
	p_c
	//'\x12'
	p_c
	//'\x13'
	p_c
	//'\x14'
	p_c
	//'\x15'
	p_c
	//'\x16'
	p_c
	//'\x17'
	p_c
	//'\x18'
	p_c
	//'\x19'
	p_c
	//'\x1a'
	p_c
	//'\x1b'
	p_c
	//'\x1c'
	p_c
	//'\x1d'
	p_c
	//'\x1e'
	p_c
	//'\x1f'
	p_z | p_pr
	//' '
	p_p | p_pr
	//'!'
	p_p | p_pr
	//'"'
	p_p | p_pr
	//'#'
	p_s | p_pr
	//'$'
	p_p | p_pr
	//'%'
	p_p | p_pr
	//'&'
	p_p | p_pr
	//'\''
	p_p | p_pr
	//'('
	p_p | p_pr
	//')'
	p_p | p_pr
	//'*'
	p_s | p_pr
	//'+'
	p_p | p_pr
	//','
	p_p | p_pr
	//'-'
	p_p | p_pr
	//'.'
	p_p | p_pr
	//'/'
	p_n | p_pr
	//'0'
	p_n | p_pr
	//'1'
	p_n | p_pr
	//'2'
	p_n | p_pr
	//'3'
	p_n | p_pr
	//'4'
	p_n | p_pr
	//'5'
	p_n | p_pr
	//'6'
	p_n | p_pr
	//'7'
	p_n | p_pr
	//'8'
	p_n | p_pr
	//'9'
	p_p | p_pr
	//':'
	p_p | p_pr
	//';'
	p_s | p_pr
	//'<'
	p_s | p_pr
	//'='
	p_s | p_pr
	//'>'
	p_p | p_pr
	//'?'
	p_p | p_pr
	//'@'
	p_lu | p_pr
	//'A'
	p_lu | p_pr
	//'B'
	p_lu | p_pr
	//'C'
	p_lu | p_pr
	//'D'
	p_lu | p_pr
	//'E'
	p_lu | p_pr
	//'F'
	p_lu | p_pr
	//'G'
	p_lu | p_pr
	//'H'
	p_lu | p_pr
	//'I'
	p_lu | p_pr
	//'J'
	p_lu | p_pr
	//'K'
	p_lu | p_pr
	//'L'
	p_lu | p_pr
	//'M'
	p_lu | p_pr
	//'N'
	p_lu | p_pr
	//'O'
	p_lu | p_pr
	//'P'
	p_lu | p_pr
	//'Q'
	p_lu | p_pr
	//'R'
	p_lu | p_pr
	//'S'
	p_lu | p_pr
	//'T'
	p_lu | p_pr
	//'U'
	p_lu | p_pr
	//'V'
	p_lu | p_pr
	//'W'
	p_lu | p_pr
	//'X'
	p_lu | p_pr
	//'Y'
	p_lu | p_pr
	//'Z'
	p_p | p_pr
	//'['
	p_p | p_pr
	//'\\'
	p_p | p_pr
	//']'
	p_s | p_pr
	//'^'
	p_p | p_pr
	//'_'
	p_s | p_pr
	//'`'
	p_ll | p_pr
	//'a'
	p_ll | p_pr
	//'b'
	p_ll | p_pr
	//'c'
	p_ll | p_pr
	//'d'
	p_ll | p_pr
	//'e'
	p_ll | p_pr
	//'f'
	p_ll | p_pr
	//'g'
	p_ll | p_pr
	//'h'
	p_ll | p_pr
	//'i'
	p_ll | p_pr
	//'j'
	p_ll | p_pr
	//'k'
	p_ll | p_pr
	//'l'
	p_ll | p_pr
	//'m'
	p_ll | p_pr
	//'n'
	p_ll | p_pr
	//'o'
	p_ll | p_pr
	//'p'
	p_ll | p_pr
	//'q'
	p_ll | p_pr
	//'r'
	p_ll | p_pr
	//'s'
	p_ll | p_pr
	//'t'
	p_ll | p_pr
	//'u'
	p_ll | p_pr
	//'v'
	p_ll | p_pr
	//'w'
	p_ll | p_pr
	//'x'
	p_ll | p_pr
	//'y'
	p_ll | p_pr
	//'z'
	p_p | p_pr
	//'{'
	p_s | p_pr
	//'|'
	p_p | p_pr
	//'}'
	p_s | p_pr
	//'~'
	p_c
	//'\u007f'
	p_c
	//'\u0080'
	p_c
	//'\u0081'
	p_c
	//'\u0082'
	p_c
	//'\u0083'
	p_c
	//'\u0084'
	p_c
	//'\u0085'
	p_c
	//'\u0086'
	p_c
	//'\u0087'
	p_c
	//'\u0088'
	p_c
	//'\u0089'
	p_c
	//'\u008a'
	p_c
	//'\u008b'
	p_c
	//'\u008c'
	p_c
	//'\u008d'
	p_c
	//'\u008e'
	p_c
	//'\u008f'
	p_c
	//'\u0090'
	p_c
	//'\u0091'
	p_c
	//'\u0092'
	p_c
	//'\u0093'
	p_c
	//'\u0094'
	p_c
	//'\u0095'
	p_c
	//'\u0096'
	p_c
	//'\u0097'
	p_c
	//'\u0098'
	p_c
	//'\u0099'
	p_c
	//'\u009a'
	p_c
	//'\u009b'
	p_c
	//'\u009c'
	p_c
	//'\u009d'
	p_c
	//'\u009e'
	p_c
	//'\u009f'
	p_z
	//'\u00a0'
	p_p | p_pr
	//'¡'
	p_s | p_pr
	//'¢'
	p_s | p_pr
	//'£'
	p_s | p_pr
	//'¤'
	p_s | p_pr
	//'¥'
	p_s | p_pr
	//'¦'
	p_p | p_pr
	//'§'
	p_s | p_pr
	//'¨'
	p_s | p_pr
	//'©'
	p_lo | p_pr
	//'ª'
	p_p | p_pr
	//'«'
	p_s | p_pr
	//'¬'
	0
	//'\u00ad'
	p_s | p_pr
	//'®'
	p_s | p_pr
	//'¯'
	p_s | p_pr
	//'°'
	p_s | p_pr
	//'±'
	p_n | p_pr
	//'²'
	p_n | p_pr
	//'³'
	p_s | p_pr
	//'´'
	p_ll | p_pr
	//'µ'
	p_p | p_pr
	//'¶'
	p_p | p_pr
	//'·'
	p_s | p_pr
	//'¸'
	p_n | p_pr
	//'¹'
	p_lo | p_pr
	//'º'
	p_p | p_pr
	//'»'
	p_n | p_pr
	//'¼'
	p_n | p_pr
	//'½'
	p_n | p_pr
	//'¾'
	p_p | p_pr
	//'¿'
	p_lu | p_pr
	//'À'
	p_lu | p_pr
	//'Á'
	p_lu | p_pr
	//'Â'
	p_lu | p_pr
	//'Ã'
	p_lu | p_pr
	//'Ä'
	p_lu | p_pr
	//'Å'
	p_lu | p_pr
	//'Æ'
	p_lu | p_pr
	//'Ç'
	p_lu | p_pr
	//'È'
	p_lu | p_pr
	//'É'
	p_lu | p_pr
	//'Ê'
	p_lu | p_pr
	//'Ë'
	p_lu | p_pr
	//'Ì'
	p_lu | p_pr
	//'Í'
	p_lu | p_pr
	//'Î'
	p_lu | p_pr
	//'Ï'
	p_lu | p_pr
	//'Ð'
	p_lu | p_pr
	//'Ñ'
	p_lu | p_pr
	//'Ò'
	p_lu | p_pr
	//'Ó'
	p_lu | p_pr
	//'Ô'
	p_lu | p_pr
	//'Õ'
	p_lu | p_pr
	//'Ö'
	p_s | p_pr
	//'×'
	p_lu | p_pr
	//'Ø'
	p_lu | p_pr
	//'Ù'
	p_lu | p_pr
	//'Ú'
	p_lu | p_pr
	//'Û'
	p_lu | p_pr
	//'Ü'
	p_lu | p_pr
	//'Ý'
	p_lu | p_pr
	//'Þ'
	p_ll | p_pr
	//'ß'
	p_ll | p_pr
	//'à'
	p_ll | p_pr
	//'á'
	p_ll | p_pr
	//'â'
	p_ll | p_pr
	//'ã'
	p_ll | p_pr
	//'ä'
	p_ll | p_pr
	//'å'
	p_ll | p_pr
	//'æ'
	p_ll | p_pr
	//'ç'
	p_ll | p_pr
	//'è'
	p_ll | p_pr
	//'é'
	p_ll | p_pr
	//'ê'
	p_ll | p_pr
	//'ë'
	p_ll | p_pr
	//'ì'
	p_ll | p_pr
	//'í'
	p_ll | p_pr
	//'î'
	p_ll | p_pr
	//'ï'
	p_ll | p_pr
	//'ð'
	p_ll | p_pr
	//'ñ'
	p_ll | p_pr
	//'ò'
	p_ll | p_pr
	//'ó'
	p_ll | p_pr
	//'ô'
	p_ll | p_pr
	//'õ'
	p_ll | p_pr
	//'ö'
	p_s | p_pr
	//'÷'
	p_ll | p_pr
	//'ø'
	p_ll | p_pr
	//'ù'
	p_ll | p_pr
	//'ú'
	p_ll | p_pr
	//'û'
	p_ll | p_pr
	//'ü'
	p_ll | p_pr
	//'ý'
	p_ll | p_pr
	//'þ'
	p_ll | p_pr
	// 'ÿ'
]!

// These tables are based on Go lang's tables: https://cs.opensource.google/go/go/+/refs/tags/go1.17.1:src/unicode/tables.go.
// There is no need to investigate unicodes' type like letter yourself.
const (
	max_latin_1 = rune(0x00ff) // '\u00FF' // `ÿ`
)

// Represents all unicode in unicode category L.
const letter_table = RangeTable{
	r16: [
		Range16{0x0041, 0x005a, 1},
		Range16{0x0061, 0x007a, 1},
		Range16{0x00aa, 0x00b5, 11},
		Range16{0x00ba, 0x00c0, 6},
		Range16{0x00c1, 0x00d6, 1},
		Range16{0x00d8, 0x00f6, 1},
		Range16{0x00f8, 0x02c1, 1},
		Range16{0x02c6, 0x02d1, 1},
		Range16{0x02e0, 0x02e4, 1},
		Range16{0x02ec, 0x02ee, 2},
		Range16{0x0370, 0x0374, 1},
		Range16{0x0376, 0x0377, 1},
		Range16{0x037a, 0x037d, 1},
		Range16{0x037f, 0x0386, 7},
		Range16{0x0388, 0x038a, 1},
		Range16{0x038c, 0x038e, 2},
		Range16{0x038f, 0x03a1, 1},
		Range16{0x03a3, 0x03f5, 1},
		Range16{0x03f7, 0x0481, 1},
		Range16{0x048a, 0x052f, 1},
		Range16{0x0531, 0x0556, 1},
		Range16{0x0559, 0x0560, 7},
		Range16{0x0561, 0x0588, 1},
		Range16{0x05d0, 0x05ea, 1},
		Range16{0x05ef, 0x05f2, 1},
		Range16{0x0620, 0x064a, 1},
		Range16{0x066e, 0x066f, 1},
		Range16{0x0671, 0x06d3, 1},
		Range16{0x06d5, 0x06e5, 16},
		Range16{0x06e6, 0x06ee, 8},
		Range16{0x06ef, 0x06fa, 11},
		Range16{0x06fb, 0x06fc, 1},
		Range16{0x06ff, 0x0710, 17},
		Range16{0x0712, 0x072f, 1},
		Range16{0x074d, 0x07a5, 1},
		Range16{0x07b1, 0x07ca, 25},
		Range16{0x07cb, 0x07ea, 1},
		Range16{0x07f4, 0x07f5, 1},
		Range16{0x07fa, 0x0800, 6},
		Range16{0x0801, 0x0815, 1},
		Range16{0x081a, 0x0824, 10},
		Range16{0x0828, 0x0840, 24},
		Range16{0x0841, 0x0858, 1},
		Range16{0x0860, 0x086a, 1},
		Range16{0x08a0, 0x08b4, 1},
		Range16{0x08b6, 0x08c7, 1},
		Range16{0x0904, 0x0939, 1},
		Range16{0x093d, 0x0950, 19},
		Range16{0x0958, 0x0961, 1},
		Range16{0x0971, 0x0980, 1},
		Range16{0x0985, 0x098c, 1},
		Range16{0x098f, 0x0990, 1},
		Range16{0x0993, 0x09a8, 1},
		Range16{0x09aa, 0x09b0, 1},
		Range16{0x09b2, 0x09b6, 4},
		Range16{0x09b7, 0x09b9, 1},
		Range16{0x09bd, 0x09ce, 17},
		Range16{0x09dc, 0x09dd, 1},
		Range16{0x09df, 0x09e1, 1},
		Range16{0x09f0, 0x09f1, 1},
		Range16{0x09fc, 0x0a05, 9},
		Range16{0x0a06, 0x0a0a, 1},
		Range16{0x0a0f, 0x0a10, 1},
		Range16{0x0a13, 0x0a28, 1},
		Range16{0x0a2a, 0x0a30, 1},
		Range16{0x0a32, 0x0a33, 1},
		Range16{0x0a35, 0x0a36, 1},
		Range16{0x0a38, 0x0a39, 1},
		Range16{0x0a59, 0x0a5c, 1},
		Range16{0x0a5e, 0x0a72, 20},
		Range16{0x0a73, 0x0a74, 1},
		Range16{0x0a85, 0x0a8d, 1},
		Range16{0x0a8f, 0x0a91, 1},
		Range16{0x0a93, 0x0aa8, 1},
		Range16{0x0aaa, 0x0ab0, 1},
		Range16{0x0ab2, 0x0ab3, 1},
		Range16{0x0ab5, 0x0ab9, 1},
		Range16{0x0abd, 0x0ad0, 19},
		Range16{0x0ae0, 0x0ae1, 1},
		Range16{0x0af9, 0x0b05, 12},
		Range16{0x0b06, 0x0b0c, 1},
		Range16{0x0b0f, 0x0b10, 1},
		Range16{0x0b13, 0x0b28, 1},
		Range16{0x0b2a, 0x0b30, 1},
		Range16{0x0b32, 0x0b33, 1},
		Range16{0x0b35, 0x0b39, 1},
		Range16{0x0b3d, 0x0b5c, 31},
		Range16{0x0b5d, 0x0b5f, 2},
		Range16{0x0b60, 0x0b61, 1},
		Range16{0x0b71, 0x0b83, 18},
		Range16{0x0b85, 0x0b8a, 1},
		Range16{0x0b8e, 0x0b90, 1},
		Range16{0x0b92, 0x0b95, 1},
		Range16{0x0b99, 0x0b9a, 1},
		Range16{0x0b9c, 0x0b9e, 2},
		Range16{0x0b9f, 0x0ba3, 4},
		Range16{0x0ba4, 0x0ba8, 4},
		Range16{0x0ba9, 0x0baa, 1},
		Range16{0x0bae, 0x0bb9, 1},
		Range16{0x0bd0, 0x0c05, 53},
		Range16{0x0c06, 0x0c0c, 1},
		Range16{0x0c0e, 0x0c10, 1},
		Range16{0x0c12, 0x0c28, 1},
		Range16{0x0c2a, 0x0c39, 1},
		Range16{0x0c3d, 0x0c58, 27},
		Range16{0x0c59, 0x0c5a, 1},
		Range16{0x0c60, 0x0c61, 1},
		Range16{0x0c80, 0x0c85, 5},
		Range16{0x0c86, 0x0c8c, 1},
		Range16{0x0c8e, 0x0c90, 1},
		Range16{0x0c92, 0x0ca8, 1},
		Range16{0x0caa, 0x0cb3, 1},
		Range16{0x0cb5, 0x0cb9, 1},
		Range16{0x0cbd, 0x0cde, 33},
		Range16{0x0ce0, 0x0ce1, 1},
		Range16{0x0cf1, 0x0cf2, 1},
		Range16{0x0d04, 0x0d0c, 1},
		Range16{0x0d0e, 0x0d10, 1},
		Range16{0x0d12, 0x0d3a, 1},
		Range16{0x0d3d, 0x0d4e, 17},
		Range16{0x0d54, 0x0d56, 1},
		Range16{0x0d5f, 0x0d61, 1},
		Range16{0x0d7a, 0x0d7f, 1},
		Range16{0x0d85, 0x0d96, 1},
		Range16{0x0d9a, 0x0db1, 1},
		Range16{0x0db3, 0x0dbb, 1},
		Range16{0x0dbd, 0x0dc0, 3},
		Range16{0x0dc1, 0x0dc6, 1},
		Range16{0x0e01, 0x0e30, 1},
		Range16{0x0e32, 0x0e33, 1},
		Range16{0x0e40, 0x0e46, 1},
		Range16{0x0e81, 0x0e82, 1},
		Range16{0x0e84, 0x0e86, 2},
		Range16{0x0e87, 0x0e8a, 1},
		Range16{0x0e8c, 0x0ea3, 1},
		Range16{0x0ea5, 0x0ea7, 2},
		Range16{0x0ea8, 0x0eb0, 1},
		Range16{0x0eb2, 0x0eb3, 1},
		Range16{0x0ebd, 0x0ec0, 3},
		Range16{0x0ec1, 0x0ec4, 1},
		Range16{0x0ec6, 0x0edc, 22},
		Range16{0x0edd, 0x0edf, 1},
		Range16{0x0f00, 0x0f40, 64},
		Range16{0x0f41, 0x0f47, 1},
		Range16{0x0f49, 0x0f6c, 1},
		Range16{0x0f88, 0x0f8c, 1},
		Range16{0x1000, 0x102a, 1},
		Range16{0x103f, 0x1050, 17},
		Range16{0x1051, 0x1055, 1},
		Range16{0x105a, 0x105d, 1},
		Range16{0x1061, 0x1065, 4},
		Range16{0x1066, 0x106e, 8},
		Range16{0x106f, 0x1070, 1},
		Range16{0x1075, 0x1081, 1},
		Range16{0x108e, 0x10a0, 18},
		Range16{0x10a1, 0x10c5, 1},
		Range16{0x10c7, 0x10cd, 6},
		Range16{0x10d0, 0x10fa, 1},
		Range16{0x10fc, 0x1248, 1},
		Range16{0x124a, 0x124d, 1},
		Range16{0x1250, 0x1256, 1},
		Range16{0x1258, 0x125a, 2},
		Range16{0x125b, 0x125d, 1},
		Range16{0x1260, 0x1288, 1},
		Range16{0x128a, 0x128d, 1},
		Range16{0x1290, 0x12b0, 1},
		Range16{0x12b2, 0x12b5, 1},
		Range16{0x12b8, 0x12be, 1},
		Range16{0x12c0, 0x12c2, 2},
		Range16{0x12c3, 0x12c5, 1},
		Range16{0x12c8, 0x12d6, 1},
		Range16{0x12d8, 0x1310, 1},
		Range16{0x1312, 0x1315, 1},
		Range16{0x1318, 0x135a, 1},
		Range16{0x1380, 0x138f, 1},
		Range16{0x13a0, 0x13f5, 1},
		Range16{0x13f8, 0x13fd, 1},
		Range16{0x1401, 0x166c, 1},
		Range16{0x166f, 0x167f, 1},
		Range16{0x1681, 0x169a, 1},
		Range16{0x16a0, 0x16ea, 1},
		Range16{0x16f1, 0x16f8, 1},
		Range16{0x1700, 0x170c, 1},
		Range16{0x170e, 0x1711, 1},
		Range16{0x1720, 0x1731, 1},
		Range16{0x1740, 0x1751, 1},
		Range16{0x1760, 0x176c, 1},
		Range16{0x176e, 0x1770, 1},
		Range16{0x1780, 0x17b3, 1},
		Range16{0x17d7, 0x17dc, 5},
		Range16{0x1820, 0x1878, 1},
		Range16{0x1880, 0x1884, 1},
		Range16{0x1887, 0x18a8, 1},
		Range16{0x18aa, 0x18b0, 6},
		Range16{0x18b1, 0x18f5, 1},
		Range16{0x1900, 0x191e, 1},
		Range16{0x1950, 0x196d, 1},
		Range16{0x1970, 0x1974, 1},
		Range16{0x1980, 0x19ab, 1},
		Range16{0x19b0, 0x19c9, 1},
		Range16{0x1a00, 0x1a16, 1},
		Range16{0x1a20, 0x1a54, 1},
		Range16{0x1aa7, 0x1b05, 94},
		Range16{0x1b06, 0x1b33, 1},
		Range16{0x1b45, 0x1b4b, 1},
		Range16{0x1b83, 0x1ba0, 1},
		Range16{0x1bae, 0x1baf, 1},
		Range16{0x1bba, 0x1be5, 1},
		Range16{0x1c00, 0x1c23, 1},
		Range16{0x1c4d, 0x1c4f, 1},
		Range16{0x1c5a, 0x1c7d, 1},
		Range16{0x1c80, 0x1c88, 1},
		Range16{0x1c90, 0x1cba, 1},
		Range16{0x1cbd, 0x1cbf, 1},
		Range16{0x1ce9, 0x1cec, 1},
		Range16{0x1cee, 0x1cf3, 1},
		Range16{0x1cf5, 0x1cf6, 1},
		Range16{0x1cfa, 0x1d00, 6},
		Range16{0x1d01, 0x1dbf, 1},
		Range16{0x1e00, 0x1f15, 1},
		Range16{0x1f18, 0x1f1d, 1},
		Range16{0x1f20, 0x1f45, 1},
		Range16{0x1f48, 0x1f4d, 1},
		Range16{0x1f50, 0x1f57, 1},
		Range16{0x1f59, 0x1f5f, 2},
		Range16{0x1f60, 0x1f7d, 1},
		Range16{0x1f80, 0x1fb4, 1},
		Range16{0x1fb6, 0x1fbc, 1},
		Range16{0x1fbe, 0x1fc2, 4},
		Range16{0x1fc3, 0x1fc4, 1},
		Range16{0x1fc6, 0x1fcc, 1},
		Range16{0x1fd0, 0x1fd3, 1},
		Range16{0x1fd6, 0x1fdb, 1},
		Range16{0x1fe0, 0x1fec, 1},
		Range16{0x1ff2, 0x1ff4, 1},
		Range16{0x1ff6, 0x1ffc, 1},
		Range16{0x2071, 0x207f, 14},
		Range16{0x2090, 0x209c, 1},
		Range16{0x2102, 0x2107, 5},
		Range16{0x210a, 0x2113, 1},
		Range16{0x2115, 0x2119, 4},
		Range16{0x211a, 0x211d, 1},
		Range16{0x2124, 0x212a, 2},
		Range16{0x212b, 0x212d, 1},
		Range16{0x212f, 0x2139, 1},
		Range16{0x213c, 0x213f, 1},
		Range16{0x2145, 0x2149, 1},
		Range16{0x214e, 0x2183, 53},
		Range16{0x2184, 0x2c00, 2684},
		Range16{0x2c01, 0x2c2e, 1},
		Range16{0x2c30, 0x2c5e, 1},
		Range16{0x2c60, 0x2ce4, 1},
		Range16{0x2ceb, 0x2cee, 1},
		Range16{0x2cf2, 0x2cf3, 1},
		Range16{0x2d00, 0x2d25, 1},
		Range16{0x2d27, 0x2d2d, 6},
		Range16{0x2d30, 0x2d67, 1},
		Range16{0x2d6f, 0x2d80, 17},
		Range16{0x2d81, 0x2d96, 1},
		Range16{0x2da0, 0x2da6, 1},
		Range16{0x2da8, 0x2dae, 1},
		Range16{0x2db0, 0x2db6, 1},
		Range16{0x2db8, 0x2dbe, 1},
		Range16{0x2dc0, 0x2dc6, 1},
		Range16{0x2dc8, 0x2dce, 1},
		Range16{0x2dd0, 0x2dd6, 1},
		Range16{0x2dd8, 0x2dde, 1},
		Range16{0x2e2f, 0x3005, 470},
		Range16{0x3006, 0x3031, 43},
		Range16{0x3032, 0x3035, 1},
		Range16{0x303b, 0x303c, 1},
		Range16{0x3041, 0x3096, 1},
		Range16{0x309d, 0x309f, 1},
		Range16{0x30a1, 0x30fa, 1},
		Range16{0x30fc, 0x30ff, 1},
		Range16{0x3105, 0x312f, 1},
		Range16{0x3131, 0x318e, 1},
		Range16{0x31a0, 0x31bf, 1},
		Range16{0x31f0, 0x31ff, 1},
		Range16{0x3400, 0x4dbf, 1},
		Range16{0x4e00, 0x9ffc, 1},
		Range16{0xa000, 0xa48c, 1},
		Range16{0xa4d0, 0xa4fd, 1},
		Range16{0xa500, 0xa60c, 1},
		Range16{0xa610, 0xa61f, 1},
		Range16{0xa62a, 0xa62b, 1},
		Range16{0xa640, 0xa66e, 1},
		Range16{0xa67f, 0xa69d, 1},
		Range16{0xa6a0, 0xa6e5, 1},
		Range16{0xa717, 0xa71f, 1},
		Range16{0xa722, 0xa788, 1},
		Range16{0xa78b, 0xa7bf, 1},
		Range16{0xa7c2, 0xa7ca, 1},
		Range16{0xa7f5, 0xa801, 1},
		Range16{0xa803, 0xa805, 1},
		Range16{0xa807, 0xa80a, 1},
		Range16{0xa80c, 0xa822, 1},
		Range16{0xa840, 0xa873, 1},
		Range16{0xa882, 0xa8b3, 1},
		Range16{0xa8f2, 0xa8f7, 1},
		Range16{0xa8fb, 0xa8fd, 2},
		Range16{0xa8fe, 0xa90a, 12},
		Range16{0xa90b, 0xa925, 1},
		Range16{0xa930, 0xa946, 1},
		Range16{0xa960, 0xa97c, 1},
		Range16{0xa984, 0xa9b2, 1},
		Range16{0xa9cf, 0xa9e0, 17},
		Range16{0xa9e1, 0xa9e4, 1},
		Range16{0xa9e6, 0xa9ef, 1},
		Range16{0xa9fa, 0xa9fe, 1},
		Range16{0xaa00, 0xaa28, 1},
		Range16{0xaa40, 0xaa42, 1},
		Range16{0xaa44, 0xaa4b, 1},
		Range16{0xaa60, 0xaa76, 1},
		Range16{0xaa7a, 0xaa7e, 4},
		Range16{0xaa7f, 0xaaaf, 1},
		Range16{0xaab1, 0xaab5, 4},
		Range16{0xaab6, 0xaab9, 3},
		Range16{0xaaba, 0xaabd, 1},
		Range16{0xaac0, 0xaac2, 2},
		Range16{0xaadb, 0xaadd, 1},
		Range16{0xaae0, 0xaaea, 1},
		Range16{0xaaf2, 0xaaf4, 1},
		Range16{0xab01, 0xab06, 1},
		Range16{0xab09, 0xab0e, 1},
		Range16{0xab11, 0xab16, 1},
		Range16{0xab20, 0xab26, 1},
		Range16{0xab28, 0xab2e, 1},
		Range16{0xab30, 0xab5a, 1},
		Range16{0xab5c, 0xab69, 1},
		Range16{0xab70, 0xabe2, 1},
		Range16{0xac00, 0xd7a3, 1},
		Range16{0xd7b0, 0xd7c6, 1},
		Range16{0xd7cb, 0xd7fb, 1},
		Range16{0xf900, 0xfa6d, 1},
		Range16{0xfa70, 0xfad9, 1},
		Range16{0xfb00, 0xfb06, 1},
		Range16{0xfb13, 0xfb17, 1},
		Range16{0xfb1d, 0xfb1f, 2},
		Range16{0xfb20, 0xfb28, 1},
		Range16{0xfb2a, 0xfb36, 1},
		Range16{0xfb38, 0xfb3c, 1},
		Range16{0xfb3e, 0xfb40, 2},
		Range16{0xfb41, 0xfb43, 2},
		Range16{0xfb44, 0xfb46, 2},
		Range16{0xfb47, 0xfbb1, 1},
		Range16{0xfbd3, 0xfd3d, 1},
		Range16{0xfd50, 0xfd8f, 1},
		Range16{0xfd92, 0xfdc7, 1},
		Range16{0xfdf0, 0xfdfb, 1},
		Range16{0xfe70, 0xfe74, 1},
		Range16{0xfe76, 0xfefc, 1},
		Range16{0xff21, 0xff3a, 1},
		Range16{0xff41, 0xff5a, 1},
		Range16{0xff66, 0xffbe, 1},
		Range16{0xffc2, 0xffc7, 1},
		Range16{0xffca, 0xffcf, 1},
		Range16{0xffd2, 0xffd7, 1},
		Range16{0xffda, 0xffdc, 1},
	]
	r32: [
		Range32{0x10000, 0x1000b, 1},
		Range32{0x1000d, 0x10026, 1},
		Range32{0x10028, 0x1003a, 1},
		Range32{0x1003c, 0x1003d, 1},
		Range32{0x1003f, 0x1004d, 1},
		Range32{0x10050, 0x1005d, 1},
		Range32{0x10080, 0x100fa, 1},
		Range32{0x10280, 0x1029c, 1},
		Range32{0x102a0, 0x102d0, 1},
		Range32{0x10300, 0x1031f, 1},
		Range32{0x1032d, 0x10340, 1},
		Range32{0x10342, 0x10349, 1},
		Range32{0x10350, 0x10375, 1},
		Range32{0x10380, 0x1039d, 1},
		Range32{0x103a0, 0x103c3, 1},
		Range32{0x103c8, 0x103cf, 1},
		Range32{0x10400, 0x1049d, 1},
		Range32{0x104b0, 0x104d3, 1},
		Range32{0x104d8, 0x104fb, 1},
		Range32{0x10500, 0x10527, 1},
		Range32{0x10530, 0x10563, 1},
		Range32{0x10600, 0x10736, 1},
		Range32{0x10740, 0x10755, 1},
		Range32{0x10760, 0x10767, 1},
		Range32{0x10800, 0x10805, 1},
		Range32{0x10808, 0x1080a, 2},
		Range32{0x1080b, 0x10835, 1},
		Range32{0x10837, 0x10838, 1},
		Range32{0x1083c, 0x1083f, 3},
		Range32{0x10840, 0x10855, 1},
		Range32{0x10860, 0x10876, 1},
		Range32{0x10880, 0x1089e, 1},
		Range32{0x108e0, 0x108f2, 1},
		Range32{0x108f4, 0x108f5, 1},
		Range32{0x10900, 0x10915, 1},
		Range32{0x10920, 0x10939, 1},
		Range32{0x10980, 0x109b7, 1},
		Range32{0x109be, 0x109bf, 1},
		Range32{0x10a00, 0x10a10, 16},
		Range32{0x10a11, 0x10a13, 1},
		Range32{0x10a15, 0x10a17, 1},
		Range32{0x10a19, 0x10a35, 1},
		Range32{0x10a60, 0x10a7c, 1},
		Range32{0x10a80, 0x10a9c, 1},
		Range32{0x10ac0, 0x10ac7, 1},
		Range32{0x10ac9, 0x10ae4, 1},
		Range32{0x10b00, 0x10b35, 1},
		Range32{0x10b40, 0x10b55, 1},
		Range32{0x10b60, 0x10b72, 1},
		Range32{0x10b80, 0x10b91, 1},
		Range32{0x10c00, 0x10c48, 1},
		Range32{0x10c80, 0x10cb2, 1},
		Range32{0x10cc0, 0x10cf2, 1},
		Range32{0x10d00, 0x10d23, 1},
		Range32{0x10e80, 0x10ea9, 1},
		Range32{0x10eb0, 0x10eb1, 1},
		Range32{0x10f00, 0x10f1c, 1},
		Range32{0x10f27, 0x10f30, 9},
		Range32{0x10f31, 0x10f45, 1},
		Range32{0x10fb0, 0x10fc4, 1},
		Range32{0x10fe0, 0x10ff6, 1},
		Range32{0x11003, 0x11037, 1},
		Range32{0x11083, 0x110af, 1},
		Range32{0x110d0, 0x110e8, 1},
		Range32{0x11103, 0x11126, 1},
		Range32{0x11144, 0x11147, 3},
		Range32{0x11150, 0x11172, 1},
		Range32{0x11176, 0x11183, 13},
		Range32{0x11184, 0x111b2, 1},
		Range32{0x111c1, 0x111c4, 1},
		Range32{0x111da, 0x111dc, 2},
		Range32{0x11200, 0x11211, 1},
		Range32{0x11213, 0x1122b, 1},
		Range32{0x11280, 0x11286, 1},
		Range32{0x11288, 0x1128a, 2},
		Range32{0x1128b, 0x1128d, 1},
		Range32{0x1128f, 0x1129d, 1},
		Range32{0x1129f, 0x112a8, 1},
		Range32{0x112b0, 0x112de, 1},
		Range32{0x11305, 0x1130c, 1},
		Range32{0x1130f, 0x11310, 1},
		Range32{0x11313, 0x11328, 1},
		Range32{0x1132a, 0x11330, 1},
		Range32{0x11332, 0x11333, 1},
		Range32{0x11335, 0x11339, 1},
		Range32{0x1133d, 0x11350, 19},
		Range32{0x1135d, 0x11361, 1},
		Range32{0x11400, 0x11434, 1},
		Range32{0x11447, 0x1144a, 1},
		Range32{0x1145f, 0x11461, 1},
		Range32{0x11480, 0x114af, 1},
		Range32{0x114c4, 0x114c5, 1},
		Range32{0x114c7, 0x11580, 185},
		Range32{0x11581, 0x115ae, 1},
		Range32{0x115d8, 0x115db, 1},
		Range32{0x11600, 0x1162f, 1},
		Range32{0x11644, 0x11680, 60},
		Range32{0x11681, 0x116aa, 1},
		Range32{0x116b8, 0x11700, 72},
		Range32{0x11701, 0x1171a, 1},
		Range32{0x11800, 0x1182b, 1},
		Range32{0x118a0, 0x118df, 1},
		Range32{0x118ff, 0x11906, 1},
		Range32{0x11909, 0x1190c, 3},
		Range32{0x1190d, 0x11913, 1},
		Range32{0x11915, 0x11916, 1},
		Range32{0x11918, 0x1192f, 1},
		Range32{0x1193f, 0x11941, 2},
		Range32{0x119a0, 0x119a7, 1},
		Range32{0x119aa, 0x119d0, 1},
		Range32{0x119e1, 0x119e3, 2},
		Range32{0x11a00, 0x11a0b, 11},
		Range32{0x11a0c, 0x11a32, 1},
		Range32{0x11a3a, 0x11a50, 22},
		Range32{0x11a5c, 0x11a89, 1},
		Range32{0x11a9d, 0x11ac0, 35},
		Range32{0x11ac1, 0x11af8, 1},
		Range32{0x11c00, 0x11c08, 1},
		Range32{0x11c0a, 0x11c2e, 1},
		Range32{0x11c40, 0x11c72, 50},
		Range32{0x11c73, 0x11c8f, 1},
		Range32{0x11d00, 0x11d06, 1},
		Range32{0x11d08, 0x11d09, 1},
		Range32{0x11d0b, 0x11d30, 1},
		Range32{0x11d46, 0x11d60, 26},
		Range32{0x11d61, 0x11d65, 1},
		Range32{0x11d67, 0x11d68, 1},
		Range32{0x11d6a, 0x11d89, 1},
		Range32{0x11d98, 0x11ee0, 328},
		Range32{0x11ee1, 0x11ef2, 1},
		Range32{0x11fb0, 0x12000, 80},
		Range32{0x12001, 0x12399, 1},
		Range32{0x12480, 0x12543, 1},
		Range32{0x13000, 0x1342e, 1},
		Range32{0x14400, 0x14646, 1},
		Range32{0x16800, 0x16a38, 1},
		Range32{0x16a40, 0x16a5e, 1},
		Range32{0x16ad0, 0x16aed, 1},
		Range32{0x16b00, 0x16b2f, 1},
		Range32{0x16b40, 0x16b43, 1},
		Range32{0x16b63, 0x16b77, 1},
		Range32{0x16b7d, 0x16b8f, 1},
		Range32{0x16e40, 0x16e7f, 1},
		Range32{0x16f00, 0x16f4a, 1},
		Range32{0x16f50, 0x16f93, 67},
		Range32{0x16f94, 0x16f9f, 1},
		Range32{0x16fe0, 0x16fe1, 1},
		Range32{0x16fe3, 0x17000, 29},
		Range32{0x17001, 0x187f7, 1},
		Range32{0x18800, 0x18cd5, 1},
		Range32{0x18d00, 0x18d08, 1},
		Range32{0x1b000, 0x1b11e, 1},
		Range32{0x1b150, 0x1b152, 1},
		Range32{0x1b164, 0x1b167, 1},
		Range32{0x1b170, 0x1b2fb, 1},
		Range32{0x1bc00, 0x1bc6a, 1},
		Range32{0x1bc70, 0x1bc7c, 1},
		Range32{0x1bc80, 0x1bc88, 1},
		Range32{0x1bc90, 0x1bc99, 1},
		Range32{0x1d400, 0x1d454, 1},
		Range32{0x1d456, 0x1d49c, 1},
		Range32{0x1d49e, 0x1d49f, 1},
		Range32{0x1d4a2, 0x1d4a5, 3},
		Range32{0x1d4a6, 0x1d4a9, 3},
		Range32{0x1d4aa, 0x1d4ac, 1},
		Range32{0x1d4ae, 0x1d4b9, 1},
		Range32{0x1d4bb, 0x1d4bd, 2},
		Range32{0x1d4be, 0x1d4c3, 1},
		Range32{0x1d4c5, 0x1d505, 1},
		Range32{0x1d507, 0x1d50a, 1},
		Range32{0x1d50d, 0x1d514, 1},
		Range32{0x1d516, 0x1d51c, 1},
		Range32{0x1d51e, 0x1d539, 1},
		Range32{0x1d53b, 0x1d53e, 1},
		Range32{0x1d540, 0x1d544, 1},
		Range32{0x1d546, 0x1d54a, 4},
		Range32{0x1d54b, 0x1d550, 1},
		Range32{0x1d552, 0x1d6a5, 1},
		Range32{0x1d6a8, 0x1d6c0, 1},
		Range32{0x1d6c2, 0x1d6da, 1},
		Range32{0x1d6dc, 0x1d6fa, 1},
		Range32{0x1d6fc, 0x1d714, 1},
		Range32{0x1d716, 0x1d734, 1},
		Range32{0x1d736, 0x1d74e, 1},
		Range32{0x1d750, 0x1d76e, 1},
		Range32{0x1d770, 0x1d788, 1},
		Range32{0x1d78a, 0x1d7a8, 1},
		Range32{0x1d7aa, 0x1d7c2, 1},
		Range32{0x1d7c4, 0x1d7cb, 1},
		Range32{0x1e100, 0x1e12c, 1},
		Range32{0x1e137, 0x1e13d, 1},
		Range32{0x1e14e, 0x1e2c0, 370},
		Range32{0x1e2c1, 0x1e2eb, 1},
		Range32{0x1e800, 0x1e8c4, 1},
		Range32{0x1e900, 0x1e943, 1},
		Range32{0x1e94b, 0x1ee00, 1205},
		Range32{0x1ee01, 0x1ee03, 1},
		Range32{0x1ee05, 0x1ee1f, 1},
		Range32{0x1ee21, 0x1ee22, 1},
		Range32{0x1ee24, 0x1ee27, 3},
		Range32{0x1ee29, 0x1ee32, 1},
		Range32{0x1ee34, 0x1ee37, 1},
		Range32{0x1ee39, 0x1ee3b, 2},
		Range32{0x1ee42, 0x1ee47, 5},
		Range32{0x1ee49, 0x1ee4d, 2},
		Range32{0x1ee4e, 0x1ee4f, 1},
		Range32{0x1ee51, 0x1ee52, 1},
		Range32{0x1ee54, 0x1ee57, 3},
		Range32{0x1ee59, 0x1ee61, 2},
		Range32{0x1ee62, 0x1ee64, 2},
		Range32{0x1ee67, 0x1ee6a, 1},
		Range32{0x1ee6c, 0x1ee72, 1},
		Range32{0x1ee74, 0x1ee77, 1},
		Range32{0x1ee79, 0x1ee7c, 1},
		Range32{0x1ee7e, 0x1ee80, 2},
		Range32{0x1ee81, 0x1ee89, 1},
		Range32{0x1ee8b, 0x1ee9b, 1},
		Range32{0x1eea1, 0x1eea3, 1},
		Range32{0x1eea5, 0x1eea9, 1},
		Range32{0x1eeab, 0x1eebb, 1},
		Range32{0x20000, 0x2a6dd, 1},
		Range32{0x2a700, 0x2b734, 1},
		Range32{0x2b740, 0x2b81d, 1},
		Range32{0x2b820, 0x2cea1, 1},
		Range32{0x2ceb0, 0x2ebe0, 1},
		Range32{0x2f800, 0x2fa1d, 1},
		Range32{0x30000, 0x3134a, 1},
	]
	latin_offset: 6
}

// Represents all unicodes in unicode category Z with property white space.
const white_space_table = RangeTable{
	r16: [
		Range16{0x0009, 0x000d, 1},
		Range16{0x0020, 0x0085, 101},
		Range16{0x00a0, 0x1680, 5600},
		Range16{0x2000, 0x200a, 1},
		Range16{0x2028, 0x2029, 1},
		Range16{0x202f, 0x205f, 48},
		Range16{0x3000, 0x3000, 1},
	]
	r32: []
	latin_offset: 2
}

// Represents all unicodes in unicode category N.
const number_table = RangeTable{
	r16: [
		Range16{0x0030, 0x0039, 1},
		Range16{0x00b2, 0x00b3, 1},
		Range16{0x00b9, 0x00bc, 3},
		Range16{0x00bd, 0x00be, 1},
		Range16{0x0660, 0x0669, 1},
		Range16{0x06f0, 0x06f9, 1},
		Range16{0x07c0, 0x07c9, 1},
		Range16{0x0966, 0x096f, 1},
		Range16{0x09e6, 0x09ef, 1},
		Range16{0x09f4, 0x09f9, 1},
		Range16{0x0a66, 0x0a6f, 1},
		Range16{0x0ae6, 0x0aef, 1},
		Range16{0x0b66, 0x0b6f, 1},
		Range16{0x0b72, 0x0b77, 1},
		Range16{0x0be6, 0x0bf2, 1},
		Range16{0x0c66, 0x0c6f, 1},
		Range16{0x0c78, 0x0c7e, 1},
		Range16{0x0ce6, 0x0cef, 1},
		Range16{0x0d58, 0x0d5e, 1},
		Range16{0x0d66, 0x0d78, 1},
		Range16{0x0de6, 0x0def, 1},
		Range16{0x0e50, 0x0e59, 1},
		Range16{0x0ed0, 0x0ed9, 1},
		Range16{0x0f20, 0x0f33, 1},
		Range16{0x1040, 0x1049, 1},
		Range16{0x1090, 0x1099, 1},
		Range16{0x1369, 0x137c, 1},
		Range16{0x16ee, 0x16f0, 1},
		Range16{0x17e0, 0x17e9, 1},
		Range16{0x17f0, 0x17f9, 1},
		Range16{0x1810, 0x1819, 1},
		Range16{0x1946, 0x194f, 1},
		Range16{0x19d0, 0x19da, 1},
		Range16{0x1a80, 0x1a89, 1},
		Range16{0x1a90, 0x1a99, 1},
		Range16{0x1b50, 0x1b59, 1},
		Range16{0x1bb0, 0x1bb9, 1},
		Range16{0x1c40, 0x1c49, 1},
		Range16{0x1c50, 0x1c59, 1},
		Range16{0x2070, 0x2074, 4},
		Range16{0x2075, 0x2079, 1},
		Range16{0x2080, 0x2089, 1},
		Range16{0x2150, 0x2182, 1},
		Range16{0x2185, 0x2189, 1},
		Range16{0x2460, 0x249b, 1},
		Range16{0x24ea, 0x24ff, 1},
		Range16{0x2776, 0x2793, 1},
		Range16{0x2cfd, 0x3007, 778},
		Range16{0x3021, 0x3029, 1},
		Range16{0x3038, 0x303a, 1},
		Range16{0x3192, 0x3195, 1},
		Range16{0x3220, 0x3229, 1},
		Range16{0x3248, 0x324f, 1},
		Range16{0x3251, 0x325f, 1},
		Range16{0x3280, 0x3289, 1},
		Range16{0x32b1, 0x32bf, 1},
		Range16{0xa620, 0xa629, 1},
		Range16{0xa6e6, 0xa6ef, 1},
		Range16{0xa830, 0xa835, 1},
		Range16{0xa8d0, 0xa8d9, 1},
		Range16{0xa900, 0xa909, 1},
		Range16{0xa9d0, 0xa9d9, 1},
		Range16{0xa9f0, 0xa9f9, 1},
		Range16{0xaa50, 0xaa59, 1},
		Range16{0xabf0, 0xabf9, 1},
		Range16{0xff10, 0xff19, 1},
	]
	r32: [
		Range32{0x10107, 0x10133, 1},
		Range32{0x10140, 0x10178, 1},
		Range32{0x1018a, 0x1018b, 1},
		Range32{0x102e1, 0x102fb, 1},
		Range32{0x10320, 0x10323, 1},
		Range32{0x10341, 0x1034a, 9},
		Range32{0x103d1, 0x103d5, 1},
		Range32{0x104a0, 0x104a9, 1},
		Range32{0x10858, 0x1085f, 1},
		Range32{0x10879, 0x1087f, 1},
		Range32{0x108a7, 0x108af, 1},
		Range32{0x108fb, 0x108ff, 1},
		Range32{0x10916, 0x1091b, 1},
		Range32{0x109bc, 0x109bd, 1},
		Range32{0x109c0, 0x109cf, 1},
		Range32{0x109d2, 0x109ff, 1},
		Range32{0x10a40, 0x10a48, 1},
		Range32{0x10a7d, 0x10a7e, 1},
		Range32{0x10a9d, 0x10a9f, 1},
		Range32{0x10aeb, 0x10aef, 1},
		Range32{0x10b58, 0x10b5f, 1},
		Range32{0x10b78, 0x10b7f, 1},
		Range32{0x10ba9, 0x10baf, 1},
		Range32{0x10cfa, 0x10cff, 1},
		Range32{0x10d30, 0x10d39, 1},
		Range32{0x10e60, 0x10e7e, 1},
		Range32{0x10f1d, 0x10f26, 1},
		Range32{0x10f51, 0x10f54, 1},
		Range32{0x10fc5, 0x10fcb, 1},
		Range32{0x11052, 0x1106f, 1},
		Range32{0x110f0, 0x110f9, 1},
		Range32{0x11136, 0x1113f, 1},
		Range32{0x111d0, 0x111d9, 1},
		Range32{0x111e1, 0x111f4, 1},
		Range32{0x112f0, 0x112f9, 1},
		Range32{0x11450, 0x11459, 1},
		Range32{0x114d0, 0x114d9, 1},
		Range32{0x11650, 0x11659, 1},
		Range32{0x116c0, 0x116c9, 1},
		Range32{0x11730, 0x1173b, 1},
		Range32{0x118e0, 0x118f2, 1},
		Range32{0x11950, 0x11959, 1},
		Range32{0x11c50, 0x11c6c, 1},
		Range32{0x11d50, 0x11d59, 1},
		Range32{0x11da0, 0x11da9, 1},
		Range32{0x11fc0, 0x11fd4, 1},
		Range32{0x12400, 0x1246e, 1},
		Range32{0x16a60, 0x16a69, 1},
		Range32{0x16b50, 0x16b59, 1},
		Range32{0x16b5b, 0x16b61, 1},
		Range32{0x16e80, 0x16e96, 1},
		Range32{0x1d2e0, 0x1d2f3, 1},
		Range32{0x1d360, 0x1d378, 1},
		Range32{0x1d7ce, 0x1d7ff, 1},
		Range32{0x1e140, 0x1e149, 1},
		Range32{0x1e2f0, 0x1e2f9, 1},
		Range32{0x1e8c7, 0x1e8cf, 1},
		Range32{0x1e950, 0x1e959, 1},
		Range32{0x1ec71, 0x1ecab, 1},
		Range32{0x1ecad, 0x1ecaf, 1},
		Range32{0x1ecb1, 0x1ecb4, 1},
		Range32{0x1ed01, 0x1ed2d, 1},
		Range32{0x1ed2f, 0x1ed3d, 1},
		Range32{0x1f100, 0x1f10c, 1},
		Range32{0x1fbf0, 0x1fbf9, 1},
	]
	latin_offset: 4
}

struct RangeTable {
pub:
	r16          []Range16
	r32          []Range32
	latin_offset int
}

struct Range16 {
pub:
	lo     u16
	hi     u16
	stride u16
}

struct Range32 {
pub:
	lo     u32
	hi     u32
	stride u32
}

// tests if rune is in the given range table.
fn is_excluding_latin(table &RangeTable, r rune) bool {
	r16 := &table.r16
	off := table.latin_offset
	if r16.len > off && u32(r) < u32((*r16)[r16.len - 1].hi) {
		return is_16((*r16)[off..], u16(r))
	}
	r32 := &table.r32
	if r32.len > 0 && r >= rune((*r32)[0].lo) {
		return is_32(*r32, u32(r))
	}
	return false
}

const linear_max = 18

fn is_16(ranges []Range16, r u16) bool {
	if ranges.len <= utf8.linear_max && r <= utf8.max_latin_1 {
		for range in ranges {
			if r < range.lo {
				return false
			}
			if r <= range.hi {
				return range.stride == 1 || (r - range.lo) % range.stride == 0
			}
		}
		return false
	}

	// binary search
	mut low, mut high := 0, ranges.len
	for low < high {
		medium := low + (high - low) / 2
		range := ranges[medium]
		if range.lo <= r && r <= range.hi {
			return range.stride == 1 || (r - range.lo) % range.stride == 0
		}
		if r < range.lo {
			high = medium
		} else {
			low = medium + 1
		}
	}

	return false
}

fn is_32(ranges []Range32, r u32) bool {
	if ranges.len <= utf8.linear_max && r <= utf8.max_latin_1 {
		for range in ranges {
			if r < range.lo {
				return false
			}
			if r <= range.hi {
				return range.stride == 1 || (r - range.lo) % range.stride == 0
			}
		}
		return false
	}

	// binary search
	mut low, mut high := 0, ranges.len
	for low < high {
		medium := low + (high - low) / 2
		range := ranges[medium]
		if range.lo <= r && r <= range.hi {
			return range.stride == 1 || (r - range.lo) % range.stride == 0
		}
		if r < range.lo {
			high = medium
		} else {
			low = medium + 1
		}
	}

	return false
}
