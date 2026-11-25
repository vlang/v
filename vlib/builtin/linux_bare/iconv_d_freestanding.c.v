module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	struct BareIconvEncoding {
	mut:
		kind   u32 // 0 UTF-8, 1 ASCII, 2 UTF-16, 3 UTF-32, 4 single-byte
		table  int
		little bool
		bom    bool
	}
	struct BareIconv {
	mut:
		input          BareIconvEncoding
		output         BareIconvEncoding
		initial_input  BareIconvEncoding
		initial_output BareIconvEncoding
		ignore         bool
	}
	fn bare_iconv_encoding(name uint, input bool) ?(BareIconvEncoding, bool) {
		text := unsafe { tos2(&u8(name)) }
		mut normalized := [80]u8{}
		mut length := 0
		mut suffix := text.len
		for i, c in text {
			if c == `/` {
				suffix = i
				break
			}
			if c == `-` || c == `_` || c == ` ` { continue }
			if length == normalized.len { return none }
			normalized[length] = if c >= `a` && c <= `z` { c - 32 } else { c }
			length++
		}
		options := text[suffix..]
		if options != '' && options != '//IGNORE' { return none }
		encoding := unsafe { tos(&normalized[0], length) }
		value := match encoding {
			'UTF8' { BareIconvEncoding{ kind: 0 } }
			'CHAR', '' {
				BareIconvEncoding{ kind: if bare_wide_utf8(0) { u32(0) } else { u32(1) } }
			}
			'ASCII', 'USASCII', 'ANSIX3.41968', 'ISO646US' { BareIconvEncoding{ kind: 1 } }
			'UTF16LE', 'UCS2LE' {
				BareIconvEncoding{
					kind:   2
					little: true
					table:  if encoding == 'UCS2LE' {
						1
					} else {
						0
					}
				}
			}
			'UTF16BE', 'UCS2BE' {
				BareIconvEncoding{ kind: 2, table: if encoding == 'UCS2BE' { 1 } else { 0 } }
			}
			'UTF16' { BareIconvEncoding{ kind: 2, little: !input, bom: true } }
			'UCS2' { BareIconvEncoding{ kind: 2, table: 1 } }
			'UTF32LE', 'UCS4LE', 'WCHART' { BareIconvEncoding{ kind: 3, little: true } }
			'UTF32BE', 'UCS4BE', 'UCS4' { BareIconvEncoding{ kind: 3 } }
			'UTF32' { BareIconvEncoding{ kind: 3, little: !input, bom: true } }
			else {
				table := bare_iconv_single_byte_name(encoding)
				if table < 0 { return none }
				BareIconvEncoding{ kind: 4, table: table }
			}
		}
		return value, options != ''
	}
	@[export: 'v_iconv_open'; markused]
	fn bare_iconv_open(output uint, input uint) uint {
		to, ignore := bare_iconv_encoding(output, false) or {
			bf_native_errno_cell = i32(C.EINVAL)
			return ~uint(0)
		}
		from, _ := bare_iconv_encoding(input, true) or {
			bf_native_errno_cell = i32(C.EINVAL)
			return ~uint(0)
		}
		mut converter := unsafe { &BareIconv(__calloc(1, sizeof(BareIconv))) }
		if converter == unsafe { nil } { return ~uint(0) }
		converter.input = from
		converter.output = to
		converter.initial_input = from
		converter.initial_output = to
		converter.ignore = ignore
		return uint(converter)
	}
	fn bare_iconv_word(bytes &u8, width int, little bool) u32 {
		mut value := u32(0)
		for i in 0 .. width {
			shift := if little { i * 8 } else { (width - 1 - i) * 8 }
			value |= u32(unsafe { bytes[i] }) << shift
		}
		return value
	}
	// A zero consumption with EINVAL denotes an incomplete final sequence.
	fn bare_iconv_decode(encoding BareIconvEncoding, bytes &u8, count uint) (u32, uint, i32) {
		first := unsafe { bytes[0] }
		if encoding.kind == 0 {
			if first < 128 { return u32(first), 1, 0 }
			length := if first >= 0xc2 && first <= 0xdf {
				2
			} else if first >= 0xe0 && first <= 0xef {
				3
			} else if first >= 0xf0 && first <= 0xf4 {
				4
			} else {
				0
			}
			if length == 0 { return 0, 1, i32(C.EILSEQ) }
			mut value := u32(first & u8((1 << (7 - length)) - 1))
			for i in 1 .. length {
				if uint(i) >= count { return 0, 0, i32(C.EINVAL) }
				part := unsafe { bytes[i] }
				if part & 0xc0 != 0x80 || (i == 1 && ((first == 0xe0 && part < 0xa0) || (first == 0xed && part >= 0xa0) || (first == 0xf0 && part < 0x90) || (first == 0xf4 && part >= 0x90))) {
					return 0, 1, i32(C.EILSEQ)
				}
				value = value << 6 | u32(part & 63)
			}
			return value, uint(length), 0
		}
		if encoding.kind == 1 {
			return u32(first), 1, if first >= 128 { i32(C.EILSEQ) } else { i32(0) }
		}
		if encoding.kind == 4 {
			value := if first < 128 {
				u32(first)
			} else {
				bare_iconv_mapping(encoding.table, int(first) - 128)
			}
			return value, 1, if value == 0xffffffff { i32(C.EILSEQ) } else { i32(0) }
		}
		width := if encoding.kind == 2 { 2 } else { 4 }
		if count < uint(width) { return 0, 0, i32(C.EINVAL) }
		value := bare_iconv_word(bytes, width, encoding.little)
		if width == 4 {
			return value, 4, if value > 0x10ffff || (value >= 0xd800 && value <= 0xdfff) {
				i32(C.EILSEQ)
			} else {
				i32(0)
			}
		}
		if value < 0xd800 || value > 0xdfff { return value, 2, 0 }
		if encoding.table == 1 || value >= 0xdc00 { return 0, 2, i32(C.EILSEQ) }
		if count < 4 { return 0, 0, i32(C.EINVAL) }
		low := bare_iconv_word(unsafe { bytes + 2 }, 2, encoding.little)
		if low < 0xdc00 || low > 0xdfff { return 0, 2, i32(C.EILSEQ) }
		return 0x10000 + ((value - 0xd800) << 10) + low - 0xdc00, 4, 0
	}
	fn bare_iconv_encode(encoding BareIconvEncoding, value u32, output &u8) uint {
		mut bytes := unsafe { output }
		if encoding.kind == 0 {
			if value < 128 {
				unsafe { bytes[0] = u8(value) }
				return 1
			}
			length := if value < 0x800 {
				2
			} else if value < 0x10000 {
				3
			} else {
				4
			}
			prefix := if length == 2 {
				u8(0xc0)
			} else if length == 3 {
				u8(0xe0)
			} else {
				u8(0xf0)
			}
			unsafe { bytes[0] = prefix | u8(value >> (6 * (length - 1))) }
			for i in 1 .. length {
				unsafe { bytes[i] = 0x80 | u8(value >> (6 * (length - 1 - i)) & 63) }
			}
			return uint(length)
		}
		if encoding.kind == 1 || encoding.kind == 4 {
			if value < 128 {
				unsafe { bytes[0] = u8(value) }
				return 1
			}
			if encoding.kind == 4 {
				for i in 0 .. 128 {
					if bare_iconv_mapping(encoding.table, i) == value {
						unsafe { bytes[0] = u8(i + 128) }
						return 1
					}
				}
			}
			return 0
		}
		width := if encoding.kind == 2 { 2 } else { 4 }
		mut words := [value, u32(0)]!
		mut units := 1
		if width == 2 && value > 0xffff {
			if encoding.table == 1 { return 0 }
			words[0] = 0xd800 | ((value - 0x10000) >> 10)
			words[1] = 0xdc00 | ((value - 0x10000) & 1023)
			units = 2
		}
		for unit in 0 .. units {
			for i in 0 .. width {
				shift := if encoding.little { i * 8 } else { (width - 1 - i) * 8 }
				unsafe { bytes[unit * width + i] = u8(words[unit] >> shift) }
			}
		}
		return uint(width * units)
	}
	@[export: 'v_iconv_close'; markused]
	fn bare_iconv_close(address uint) i32 {
		if address == 0 || address == ~uint(0) {
			bf_native_errno_cell = i32(C.EBADF)
			return -1
		}
		unsafe { __free(voidptr(address)) }
		return 0
	}
	@[export: 'v_iconv_convert'; markused]
	fn bare_iconv_convert(address uint, input_address uint, input_count uint, output_address uint, output_count uint) uint {
		if address == 0 || address == ~uint(0) {
			bf_native_errno_cell = i32(C.EBADF)
			return ~uint(0)
		}
		mut converter := unsafe { &BareIconv(address) }
		if input_address == 0 || unsafe { *(&uint(input_address)) } == 0 {
			converter.input = converter.initial_input
			converter.output = converter.initial_output
			return 0
		}
		mut input := unsafe { *(&uint(input_address)) }
		mut remaining := unsafe { *(&uint(input_count)) }
		mut output := unsafe { *(&uint(output_address)) }
		mut available := unsafe { *(&uint(output_count)) }
		defer {
			unsafe {
				*(&uint(input_address)) = input
				*(&uint(input_count)) = remaining
				*(&uint(output_address)) = output
				*(&uint(output_count)) = available
			}
		}
		mut ignored := uint(0)
		mut discarded := false
		for remaining > 0 {
			if converter.input.bom {
				width := if converter.input.kind == 2 { uint(2) } else { uint(4) }
				if remaining < width {
					bf_native_errno_cell = i32(C.EINVAL)
					return ~uint(0)
				}
				be := bare_iconv_word(unsafe { &u8(input) }, int(width), false)
				le := bare_iconv_word(unsafe { &u8(input) }, int(width), true)
				converter.input.bom = false
				if be == 0xfeff || le == 0xfeff {
					converter.input.little = le == 0xfeff
					input += width
					remaining -= width
					continue
				}
			}
			value, consumed, code := bare_iconv_decode(converter.input, unsafe { &u8(input) }, remaining)
			if code != 0 {
				if converter.ignore && code == i32(C.EILSEQ) {
					input += consumed
					remaining -= consumed
					ignored++
					discarded = true
					continue
				}
				bf_native_errno_cell = code
				return ~uint(0)
			}
			mut bytes := [8]u8{}
			length := bare_iconv_encode(converter.output, value, &bytes[0])
			if length == 0 {
				if converter.ignore {
					input += consumed
					remaining -= consumed
					ignored++
					discarded = true
					continue
				}
				bf_native_errno_cell = i32(C.EILSEQ)
				return ~uint(0)
			}
			mut prefix := uint(0)
			mut marker := [4]u8{}
			if converter.output.bom {
				prefix = bare_iconv_encode(converter.output, 0xfeff, &marker[0])
			}
			if prefix + length > available {
				bf_native_errno_cell = i32(C.E2BIG)
				return ~uint(0)
			}
			if prefix != 0 {
				unsafe { vmemcpy(voidptr(output), &marker[0], int(prefix)) }
				output += prefix
				available -= prefix
				converter.output.bom = false
			}
			unsafe { vmemcpy(voidptr(output), &bytes[0], int(length)) }
			output += length
			available -= length
			input += consumed
			remaining -= consumed
		}
		if discarded {
			bf_native_errno_cell = i32(C.EILSEQ)
			return ~uint(0)
		}
		return ignored
	}
}
