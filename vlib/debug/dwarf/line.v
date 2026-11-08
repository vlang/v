module dwarf

import encoding.binary
import encoding.leb128

// LineFile is one file entry of a line table header.
pub struct LineFile {
pub:
	name string
	dir  string
	time u64
	size u64
}

// LineRow is one row of the line number matrix.
pub struct LineRow {
pub:
	address        u64
	file           string
	line           int
	column         int
	is_stmt        bool
	basic_block    bool
	end_sequence   bool
	prologue_end   bool
	epilogue_begin bool
	isa            u64
	discriminator  u64
}

// LineTable is a decoded .debug_line unit.
pub struct LineTable {
pub:
	dirs  []string
	files []LineFile
	rows  []LineRow
}

// Standard opcode numbers.
pub const lns_copy = u8(1)
pub const lns_advance_pc = u8(2)
pub const lns_advance_line = u8(3)
pub const lns_set_file = u8(4)
pub const lns_set_column = u8(5)
pub const lns_negate_stmt = u8(6)
pub const lns_set_basic_block = u8(7)
pub const lns_const_add_pc = u8(8)
pub const lns_fixed_advance_pc = u8(9)
pub const lns_set_prologue_end = u8(10)
pub const lns_set_epilogue_begin = u8(11)
pub const lns_set_isa = u8(12)

// Extended opcode numbers.
pub const lne_end_sequence = u8(1)
pub const lne_set_address = u8(2)
pub const lne_define_file = u8(3)
pub const lne_set_discriminator = u8(4)

// Directory entry content types (v5).
pub const lnt_path = u64(1)
pub const lnt_directory_index = u64(2)
pub const lnt_timestamp = u64(3)
pub const lnt_size = u64(4)

// form_line_strp is DW_FORM_line_strp: an offset into .debug_line_str.
pub const form_line_strp = u64(0x1f)

// parse_line_table decodes one .debug_line unit. addr_size selects the
// address width; strtab carries .debug_str for v5 strp forms and
// line_strtab carries .debug_line_str for v5 line_strp forms (each may
// be empty when its forms are unused).
pub fn parse_line_table(data []u8, addr_size u8, strtab []u8, line_strtab []u8) !LineTable {
	if data.len < 4 {
		return error('dwarf: truncated line unit length')
	}
	unit_len := binary.little_endian_u32(data[0..4])
	if unit_len == 0xffffffff {
		return error('dwarf: 64-bit DWARF is not supported')
	}
	end := 4 + int(unit_len)
	if end > data.len {
		return error('dwarf: truncated line unit')
	}
	body := data[4..end]
	if body.len < 2 {
		return error('dwarf: truncated line version')
	}
	version := binary.little_endian_u16(body[0..2])
	if version < 2 || version > 5 {
		return error('dwarf: unsupported line version ${version}')
	}
	mut pos := 2
	if version == 5 {
		if body.len < 4 {
			return error('dwarf: truncated v5 line header')
		}
		// address_size + segment_selector_size; segments unsupported.
		if body[2] != addr_size || body[3] != 0 {
			return error('dwarf: line address size mismatch')
		}
		pos = 4
	}
	if body.len < pos + 4 {
		return error('dwarf: truncated line header length')
	}
	header_len := int(binary.little_endian_u32(body[pos..pos + 4]))
	pos += 4
	hdr_end := pos + header_len
	if hdr_end > body.len {
		return error('dwarf: truncated line header')
	}
	if body.len < pos + 1 {
		return error('dwarf: truncated line params')
	}
	min_inst_len := body[pos]
	pos++
	mut max_ops := u8(1)
	if version >= 4 {
		max_ops = body[pos]
		pos++
	}
	_ = max_ops
	default_is_stmt := body[pos] != 0
	pos++
	line_base := i8(body[pos])
	pos++
	line_range := body[pos]
	pos++
	opcode_base := body[pos]
	pos++
	if opcode_base < 1 {
		return error('dwarf: bad opcode base')
	}
	if body.len < pos + int(opcode_base) - 1 {
		return error('dwarf: truncated opcode lengths')
	}
	pos += int(opcode_base) - 1
	mut dirs := []string{}
	mut files := []LineFile{}
	if version == 5 {
		dirs, pos = parse_v5_dirs(body, pos, strtab, line_strtab)!
		files, pos = parse_v5_files(body, pos, dirs, strtab, line_strtab)!
	} else {
		dirs, pos = parse_v24_dirs(body, pos)!
		files, pos = parse_v24_files(body, pos, dirs)!
	}
	if pos != hdr_end {
		return error('dwarf: line header length mismatch')
	}
	rows := run_line_program(body[pos..], LineSetup{
		min_inst_len:    min_inst_len
		default_is_stmt: default_is_stmt
		line_base:       line_base
		line_range:      line_range
		opcode_base:     opcode_base
		addr_size:       addr_size
		dirs:            dirs
		files:           files
	})!
	return LineTable{ dirs: dirs, files: files, rows: rows }
}

// parse_line_tables decodes every unit in a .debug_line section.
pub fn parse_line_tables(data []u8, addr_size u8, strtab []u8, line_strtab []u8) ![]LineTable {
	mut out := []LineTable{}
	mut pos := 0
	for pos < data.len {
		if data.len - pos < 4 {
			return error('dwarf: truncated line unit length')
		}
		unit_len := binary.little_endian_u32(data[pos..pos + 4])
		if unit_len == 0xffffffff {
			return error('dwarf: 64-bit DWARF is not supported')
		}
		if unit_len == 0 {
			break
		}
		end := pos + 4 + int(unit_len)
		if end > data.len {
			return error('dwarf: truncated line unit')
		}
		out << parse_line_table(data[pos..end], addr_size, strtab, line_strtab)!
		pos = end
	}
	return out
}

struct LineSetup {
	min_inst_len    u8
	default_is_stmt bool
	line_base       i8
	line_range      u8
	opcode_base     u8
	addr_size       u8
	dirs            []string
	files           []LineFile
}

fn parse_v24_dirs(body []u8, pos int) !([]string, int) {
	mut dirs := []string{}
	mut cur := pos
	for {
		if cur >= body.len {
			return error('dwarf: truncated directories')
		}
		end := body[cur..].index(u8(0))
		if end < 0 {
			return error('dwarf: unterminated directory')
		}
		if end == 0 {
			cur++
			break
		}
		dirs << body[cur..cur + end].bytestr()
		cur += end + 1
	}
	return dirs, cur
}

fn parse_v24_files(body []u8, pos int, dirs []string) !([]LineFile, int) {
	mut files := []LineFile{}
	mut cur := pos
	for {
		if cur >= body.len {
			return error('dwarf: truncated file list')
		}
		end := body[cur..].index(u8(0))
		if end < 0 {
			return error('dwarf: unterminated file name')
		}
		if end == 0 {
			cur++
			break
		}
		name := body[cur..cur + end].bytestr()
		cur += end + 1
		dir_idx, n := leb128.decode_u64(body[cur..])
		cur += n
		mtime, n2 := leb128.decode_u64(body[cur..])
		cur += n2
		size, n3 := leb128.decode_u64(body[cur..])
		cur += n3
		mut dir := ''
		if dir_idx > 0 && int(dir_idx) <= dirs.len {
			dir = dirs[int(dir_idx) - 1]
		}
		files << LineFile{ name: name, dir: dir, time: mtime, size: size }
	}
	return files, cur
}

struct V5Format {
	content u64
	form    u64
}

fn parse_v5_formats(body []u8, pos int) !([]V5Format, int) {
	if pos >= body.len {
		return error('dwarf: truncated v5 formats')
	}
	count := int(body[pos])
	mut cur := pos + 1
	mut out := []V5Format{}
	for _ in 0 .. count {
		c, n := leb128.decode_u64(body[cur..])
		cur += n
		f, n2 := leb128.decode_u64(body[cur..])
		cur += n2
		out << V5Format{ content: c, form: f }
	}
	return out, cur
}

fn read_v5_string(body []u8, pos int, form u64, strtab []u8, line_strtab []u8) !(string, int) {
	match form {
		form_string {
			end := body[pos..].index(u8(0))
			if end < 0 {
				return error('dwarf: unterminated v5 string')
			}
			return body[pos..pos + end].bytestr(), pos + end + 1
		}
		form_strp, form_line_strp {
			if pos + 4 > body.len {
				return error('dwarf: truncated v5 strp')
			}
			off := int(binary.little_endian_u32(body[pos..pos + 4]))
			tab := if form == form_strp { strtab } else { line_strtab }
			end := tab[off..].index(u8(0))
			if end < 0 || off + end >= tab.len {
				return error('dwarf: bad v5 strp offset')
			}
			return tab[off..off + end].bytestr(), pos + 4
		}
		else {
			return error('dwarf: unsupported v5 path form ${form}')
		}
	}
}

fn read_v5_u64(body []u8, pos int, form u64) !(u64, int) {
	match form {
		form_udata {
			v, n := leb128.decode_u64(body[pos..])
			return v, pos + n
		}
		form_data1 {
			return u64(body[pos]), pos + 1
		}
		form_data2 {
			return u64(binary.little_endian_u16(body[pos..pos + 2])), pos + 2
		}
		form_data4 {
			return u64(binary.little_endian_u32(body[pos..pos + 4])), pos + 4
		}
		form_data8 {
			return binary.little_endian_u64(body[pos..pos + 8]), pos + 8
		}
		form_flag_present {
			return u64(1), pos
		}
		else {
			return error('dwarf: unsupported v5 number form ${form}')
		}
	}
}

fn parse_v5_dirs(body []u8, pos int, strtab []u8, line_strtab []u8) !([]string, int) {
	formats, mut cur := parse_v5_formats(body, pos)!
	if cur >= body.len {
		return error('dwarf: truncated v5 dir count')
	}
	count, n := leb128.decode_u64(body[cur..])
	cur += n
	mut dirs := []string{}
	for _ in 0 .. int(count) {
		mut path := ''
		for f in formats {
			if f.content == lnt_path {
				path, cur = read_v5_string(body, cur, f.form, strtab, line_strtab)!
			} else {
				_, cur = read_v5_u64(body, cur, f.form)!
			}
		}
		dirs << path
	}
	return dirs, cur
}

fn parse_v5_files(body []u8, pos int, dirs []string, strtab []u8, line_strtab []u8) !([]LineFile, int) {
	formats, mut cur := parse_v5_formats(body, pos)!
	if cur >= body.len {
		return error('dwarf: truncated v5 file count')
	}
	count, n := leb128.decode_u64(body[cur..])
	cur += n
	mut files := []LineFile{}
	for _ in 0 .. int(count) {
		mut name := ''
		mut dir_idx := u64(0)
		mut mtime := u64(0)
		mut size := u64(0)
		for f in formats {
			match f.content {
				lnt_path {
					name, cur = read_v5_string(body, cur, f.form, strtab, line_strtab)!
				}
				lnt_directory_index {
					dir_idx, cur = read_v5_u64(body, cur, f.form)!
				}
				lnt_timestamp {
					mtime, cur = read_v5_u64(body, cur, f.form)!
				}
				lnt_size {
					size, cur = read_v5_u64(body, cur, f.form)!
				}
				else {
					_, cur = read_v5_u64(body, cur, f.form)!
				}
			}
		}
		mut dir := ''
		if dir_idx > 0 && int(dir_idx) <= dirs.len {
			dir = dirs[int(dir_idx) - 1]
		}
		files << LineFile{ name: name, dir: dir, time: mtime, size: size }
	}
	return files, cur
}

fn make_row(address u64, file u64, line int, column u64, is_stmt bool, basic_block bool, end_sequence bool, prologue_end bool, epilogue_begin bool, isa u64, discriminator u64, files []LineFile) LineRow {
	mut fname := ''
	if file >= 1 && int(file) <= files.len {
		fname = files[int(file) - 1].name
	}
	return LineRow{
		address: address, file: fname, line: line, column: int(column),
		is_stmt: is_stmt, basic_block: basic_block, end_sequence: end_sequence,
		prologue_end: prologue_end, epilogue_begin: epilogue_begin, isa: isa,
		discriminator: discriminator,
	}
}

fn run_line_program(prog []u8, s LineSetup) ![]LineRow {
	mut rows := []LineRow{}
	mut address := u64(0)
	mut file := u64(1)
	mut line := 1
	mut column := u64(0)
	mut is_stmt := s.default_is_stmt
	mut basic_block := false
	mut prologue_end := false
	mut epilogue_begin := false
	mut isa := u64(0)
	mut discriminator := u64(0)
	mut pos := 0
	for pos < prog.len {
		op := prog[pos]
		pos++
		if op == 0 {
			elen, n := leb128.decode_u64(prog[pos..])
			pos += n
			// elen counts its own sub-opcode byte.
			if pos + int(elen) > prog.len {
				return error('dwarf: truncated extended opcode')
			}
			sub := prog[pos]
			arg := prog[pos + 1..pos + int(elen)]
			pos += int(elen)
			match sub {
				lne_end_sequence {
					rows << LineRow{
						address: address, file: '', line: line, column: int(column),
						is_stmt: is_stmt, basic_block: basic_block, end_sequence: true,
						prologue_end: prologue_end, epilogue_begin: epilogue_begin,
						isa: isa, discriminator: discriminator,
					}
					address = 0
					file = 1
					line = 1
					column = 0
					is_stmt = s.default_is_stmt
					basic_block = false
					prologue_end = false
					epilogue_begin = false
					isa = 0
					discriminator = 0
				}
				lne_set_address {
					if arg.len != int(s.addr_size) {
						return error('dwarf: bad set_address length')
					}
					if s.addr_size == 8 {
						address = binary.little_endian_u64(arg[..8])
					} else {
						address = u64(binary.little_endian_u32(arg[..4]))
					}
				}
				lne_define_file {
					// Directory and time/size operands are parsed and
					// dropped: the file list is fixed at header parse.
					end := arg.index(u8(0))
					if end < 0 {
						return error('dwarf: bad define_file')
					}
				}
				lne_set_discriminator {
					discriminator, _ = leb128.decode_u64(arg)
				}
				else {
					return error('dwarf: unsupported extended opcode ${sub}')
				}
			}
			continue
		}
		if op < s.opcode_base {
			match op {
				lns_copy {
					rows << make_row(address, file, line, column, is_stmt, basic_block,
						false, prologue_end, epilogue_begin, isa, discriminator, s.files)
					basic_block = false
					prologue_end = false
					epilogue_begin = false
					discriminator = 0
				}
				lns_advance_pc {
					adv, n2 := leb128.decode_u64(prog[pos..])
					pos += n2
					address += adv * u64(s.min_inst_len)
				}
				lns_advance_line {
					adv, n2 := leb128.decode_i64(prog[pos..])
					pos += n2
					line += int(adv)
				}
				lns_set_file {
					v, n2 := leb128.decode_u64(prog[pos..])
					file = v
					pos += n2
				}
				lns_set_column {
					v, n2 := leb128.decode_u64(prog[pos..])
					column = v
					pos += n2
				}
				lns_negate_stmt {
					is_stmt = !is_stmt
				}
				lns_set_basic_block {
					basic_block = true
				}
				lns_const_add_pc {
					address += (u64(255) - u64(s.opcode_base)) / u64(s.line_range) * u64(s.min_inst_len)
				}
				lns_fixed_advance_pc {
					if pos + 2 > prog.len {
						return error('dwarf: truncated fixed_advance_pc')
					}
					address += u64(binary.little_endian_u16(prog[pos..pos + 2]))
					pos += 2
				}
				lns_set_prologue_end {
					prologue_end = true
				}
				lns_set_epilogue_begin {
					epilogue_begin = true
				}
				lns_set_isa {
					v, n2 := leb128.decode_u64(prog[pos..])
					isa = v
					pos += n2
				}
				else {
					return error('dwarf: unsupported standard opcode ${op}')
				}
			}
			continue
		}
		adj := int(op) - int(s.opcode_base)
		address += u64(adj / int(s.line_range)) * u64(s.min_inst_len)
		line += int(s.line_base) + adj % int(s.line_range)
		rows << make_row(address, file, line, column, is_stmt, basic_block, false,
			prologue_end, epilogue_begin, isa, discriminator, s.files)
		basic_block = false
		prologue_end = false
		epilogue_begin = false
		discriminator = 0
	}
	return rows
}
