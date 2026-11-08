module dwarf

import encoding.binary
import encoding.leb128

// Supported DWARF versions.
pub const version2 = u16(2)
pub const version3 = u16(3)
pub const version4 = u16(4)
pub const version5 = u16(5)

// DWARF v5 full and partial compilation unit types (DWARF5 section 7.3.2).
// Other unit types (skeleton, split, type units) need .dwo sections and
// are rejected by this milestone.
pub const unit_type_compile = u8(0x01)
pub const unit_type_partial = u8(0x03)

// Common tag values (DWARF section 7.5.3).
pub const tag_compile_unit = u64(0x11)
pub const tag_subprogram = u64(0x2e)
pub const tag_variable = u64(0x34)
pub const tag_base_type = u64(0x24)

// Common attribute values (DWARF section 7.5.4).
pub const at_name = u64(0x03)
pub const at_producer = u64(0x25)
pub const at_comp_dir = u64(0x1b)
pub const at_low_pc = u64(0x11)
pub const at_high_pc = u64(0x12)
pub const at_stmt_list = u64(0x10)

// Classic form values, DWARF versions 2-4 (DWARF section 7.5.5).
// DWARF v5 indexed forms (addrx, strx, ...) are reported as
// unsupported by this milestone rather than guessed at.
pub const form_addr = u64(0x01)
pub const form_block2 = u64(0x03)
pub const form_block4 = u64(0x04)
pub const form_data2 = u64(0x05)
pub const form_data4 = u64(0x06)
pub const form_data8 = u64(0x07)
pub const form_string = u64(0x08)
pub const form_block = u64(0x09)
pub const form_block1 = u64(0x0a)
pub const form_data1 = u64(0x0b)
pub const form_flag = u64(0x0c)
pub const form_sdata = u64(0x0d)
pub const form_strp = u64(0x0e)
pub const form_udata = u64(0x0f)
pub const form_ref_addr = u64(0x10)
pub const form_ref1 = u64(0x11)
pub const form_ref2 = u64(0x12)
pub const form_ref4 = u64(0x13)
pub const form_ref8 = u64(0x14)
pub const form_ref_udata = u64(0x15)
pub const form_indirect = u64(0x16)
pub const form_sec_offset = u64(0x17)
pub const form_exprloc = u64(0x18)
pub const form_flag_present = u64(0x19)
pub const form_ref_sig8 = u64(0x20)

// AttrValue is a decoded attribute value.
pub type AttrValue = []u8 | bool | i64 | string | u64

// Attr is a single decoded attribute.
pub struct Attr {
pub:
	name  u64
	form  u64
	value AttrValue
}

// Die is a debugging information entry with decoded attributes and children.
pub struct Die {
pub:
	tag          u64
	has_children bool
	attrs        []Attr
	children     []Die
}

// attr returns the first attribute with the given name, if any.
pub fn (d Die) attr(name u64) ?Attr {
	for a in d.attrs {
		if a.name == name {
			return a
		}
	}
	return none
}

// string_attr returns the string value of a string-valued attribute.
pub fn (d Die) string_attr(name u64) ?string {
	a := d.attr(name) or { return none }
	return match a.value {
		string { a.value }
		else { none }
	}
}

// AbbrevDecl is one entry of an abbreviation table.
pub struct AbbrevDecl {
pub:
	code         u64
	tag          u64
	has_children bool
	attrs        []AbbrevAttr
}

// AbbrevAttr pairs an attribute name with its form.
pub struct AbbrevAttr {
pub:
	name u64
	form u64
}

// Unit is one compilation unit from .debug_info.
pub struct Unit {
pub:
	version       u16
	addr_size     u8
	abbrev_offset u32
	dies          []Die
}

// die_by_tag returns the first top-level DIE with the given tag, if any.
pub fn (u Unit) die_by_tag(tag u64) ?Die {
	for d in u.dies {
		if d.tag == tag {
			return d
		}
	}
	return none
}

// parse_info parses .debug_info section bytes using the .debug_abbrev
// section bytes, returning one Unit per compilation unit.
// Only 32-bit DWARF32 length headers and versions 2-5 are supported.
pub fn parse_info(info []u8, abbrev []u8) ![]Unit {
	mut units := []Unit{}
	mut pos := 0
	for pos < info.len {
		if info.len - pos < 4 {
			return error('dwarf: truncated unit length')
		}
		unit_len := binary.little_endian_u32(info[pos..pos + 4])
		if unit_len == 0xffffffff {
			return error('dwarf: 64-bit DWARF is not supported')
		}
		if unit_len == 0 {
			break
		}
		start := pos + 4
		end := start + int(unit_len)
		if end > info.len {
			return error('dwarf: truncated unit')
		}
		u := parse_unit(info[start..end], abbrev)!
		units << u
		pos = end
	}
	return units
}

fn parse_unit(data []u8, abbrev []u8) !Unit {
	if data.len < 4 {
		return error('dwarf: truncated unit header')
	}
	version := binary.little_endian_u16(data[0..2])
	if version < 2 || version > 5 {
		return error('dwarf: unsupported version ${version}')
	}
	// DWARF v5 units carry unit_type and address size up front.
	if version == 5 {
		if data.len < 8 {
			return error('dwarf: truncated v5 unit header')
		}
		if data[2] != unit_type_compile && data[2] != unit_type_partial {
			return error('dwarf: unsupported v5 unit type ${data[2]}')
		}
		addr_size := data[3]
		if addr_size != 4 && addr_size != 8 {
			return error('dwarf: unsupported address size ${addr_size}')
		}
		abbrev_offset := binary.little_endian_u32(data[4..8])
		decls := parse_abbrev_table(abbrev, int(abbrev_offset))!
		mut dies := []Die{}
		mut dpos := 8
		for dpos < data.len {
			die, next := parse_die(data, dpos, decls, addr_size)!
			dpos = next
			if die.tag == 0 {
				break
			}
			dies << die
		}
		return Unit{ version: version, addr_size: addr_size, abbrev_offset: abbrev_offset, dies: dies }
	}
	if data.len < 7 {
		return error('dwarf: truncated unit header')
	}
	abbrev_offset := binary.little_endian_u32(data[2..6])
	addr_size := data[6]
	if addr_size != 4 && addr_size != 8 {
		return error('dwarf: unsupported address size ${addr_size}')
	}
	decls := parse_abbrev_table(abbrev, int(abbrev_offset))!
	mut dies := []Die{}
	mut dpos := 7
	for dpos < data.len {
		die, next := parse_die(data, dpos, decls, addr_size)!
		dpos = next
		if die.tag == 0 {
			break
		}
		dies << die
	}
	return Unit{ version: version, addr_size: addr_size, abbrev_offset: abbrev_offset, dies: dies }
}

// parse_abbrev_table parses one table from .debug_abbrev at `offset`.
pub fn parse_abbrev_table(abbrev []u8, offset int) !map[u64]AbbrevDecl {
	if offset < 0 || offset > abbrev.len {
		return error('dwarf: abbrev offset out of range')
	}
	mut decls := map[u64]AbbrevDecl{}
	mut pos := offset
	for {
		if pos >= abbrev.len {
			return error('dwarf: truncated abbrev table')
		}
		code, n := leb128.decode_u64(abbrev[pos..])
		pos += n
		if code == 0 {
			break
		}
		tag, n2 := leb128.decode_u64(abbrev[pos..])
		pos += n2
		if pos >= abbrev.len {
			return error('dwarf: truncated abbrev decl')
		}
		has_children := abbrev[pos] != 0
		pos++
		mut attrs := []AbbrevAttr{}
		for {
			if pos >= abbrev.len {
				return error('dwarf: truncated abbrev attrs')
			}
			name, n3 := leb128.decode_u64(abbrev[pos..])
			pos += n3
			form, n4 := leb128.decode_u64(abbrev[pos..])
			pos += n4
			if name == 0 && form == 0 {
				break
			}
			attrs << AbbrevAttr{ name: name, form: form }
		}
		decls[code] = AbbrevDecl{ code: code, tag: tag, has_children: has_children, attrs: attrs }
	}
	return decls
}

fn parse_die(data []u8, pos int, decls map[u64]AbbrevDecl, addr_size u8) !(Die, int) {
	if pos >= data.len {
		return error('dwarf: DIE past end of unit')
	}
	code, n := leb128.decode_u64(data[pos..])
	mut cur := pos + n
	if code == 0 {
		return Die{}, cur
	}
	decl := decls[code] or { return error('dwarf: unknown abbrev code ${code}') }
	mut attrs := []Attr{}
	for aa in decl.attrs {
		val, next := parse_attr_value(data, cur, aa.form, addr_size)!
		attrs << Attr{ name: aa.name, form: aa.form, value: val }
		cur = next
	}
	mut children := []Die{}
	if decl.has_children {
		for cur < data.len {
			child, next := parse_die(data, cur, decls, addr_size)!
			cur = next
			if child.tag == 0 {
				break
			}
			children << child
		}
	}
	return Die{ tag: decl.tag, has_children: decl.has_children, attrs: attrs, children: children }, cur
}

fn parse_attr_value(data []u8, pos int, form u64, addr_size u8) !(AttrValue, int) {
	if pos > data.len {
		return error('dwarf: attribute past end of unit')
	}
	tail := data[pos..]
	match form {
		form_flag_present {
			return AttrValue(true), pos
		}
		form_flag, form_data1, form_ref1 {
			if tail.len < 1 {
				return error('dwarf: truncated 1-byte form')
			}
			if form == form_flag {
				return AttrValue(tail[0] != 0), pos + 1
			}
			return AttrValue(u64(tail[0])), pos + 1
		}
		form_data2, form_ref2 {
			if tail.len < 2 {
				return error('dwarf: truncated 2-byte form')
			}
			return AttrValue(u64(binary.little_endian_u16(tail[..2]))), pos + 2
		}
		form_data4, form_ref4, form_strp, form_sec_offset {
			if tail.len < 4 {
				return error('dwarf: truncated 4-byte form')
			}
			return AttrValue(u64(binary.little_endian_u32(tail[..4]))), pos + 4
		}
		form_data8, form_ref8, form_ref_sig8 {
			if tail.len < 8 {
				return error('dwarf: truncated 8-byte form')
			}
			return AttrValue(binary.little_endian_u64(tail[..8])), pos + 8
		}
		form_string {
			end := tail.index(u8(0))
			if end < 0 {
				return error('dwarf: unterminated string form')
			}
			return AttrValue(tail[..end].bytestr()), pos + end + 1
		}
		form_udata, form_ref_udata {
			v, nn := leb128.decode_u64(tail)
			return AttrValue(v), pos + nn
		}
		form_sdata {
			v, nn := leb128.decode_i64(tail)
			return AttrValue(i64(v)), pos + nn
		}
		form_addr {
			if tail.len < int(addr_size) {
				return error('dwarf: truncated address form')
			}
			if addr_size == 8 {
				return AttrValue(binary.little_endian_u64(tail[..8])), pos + 8
			}
			return AttrValue(u64(binary.little_endian_u32(tail[..4]))), pos + 4
		}
		form_block, form_exprloc {
			v, nn := leb128.decode_u64(tail)
			end := nn + int(v)
			if end > tail.len {
				return error('dwarf: truncated block form')
			}
			return AttrValue(tail[nn..end].clone()), pos + end
		}
		form_block1 {
			if tail.len < 1 {
				return error('dwarf: truncated block1 form')
			}
			end := 1 + int(tail[0])
			if end > tail.len {
				return error('dwarf: truncated block1 form')
			}
			return AttrValue(tail[1..end].clone()), pos + end
		}
		form_block2 {
			if tail.len < 2 {
				return error('dwarf: truncated block2 form')
			}
			end := 2 + int(binary.little_endian_u16(tail[..2]))
			if end > tail.len {
				return error('dwarf: truncated block2 form')
			}
			return AttrValue(tail[2..end].clone()), pos + end
		}
		form_block4 {
			if tail.len < 4 {
				return error('dwarf: truncated block4 form')
			}
			end := 4 + int(binary.little_endian_u32(tail[..4]))
			if end > tail.len {
				return error('dwarf: truncated block4 form')
			}
			return AttrValue(tail[4..end].clone()), pos + end
		}
		form_ref_addr {
			// Classic DWARF32 reference, sized by address size.
			if tail.len < int(addr_size) {
				return error('dwarf: truncated ref_addr form')
			}
			if addr_size == 8 {
				return AttrValue(binary.little_endian_u64(tail[..8])), pos + 8
			}
			return AttrValue(u64(binary.little_endian_u32(tail[..4]))), pos + 4
		}
		form_indirect {
			// The real form follows as a ULEB128.
			v, nn := leb128.decode_u64(tail)
			if v == form_indirect {
				return error('dwarf: nested indirect form')
			}
			return parse_attr_value(data, pos + nn, v, addr_size)
		}
		else {
			return error('dwarf: unsupported form ${form}')
		}
	}
}
