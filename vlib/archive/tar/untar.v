module tar

// Untar uses a reader to parse the contents of a unix tar file.
// Reuses a fixed array of 512 bytes to parse each TAR block.
@[heap]
pub struct Untar {
mut:
	reader     Reader
	max_blocks int
	buffer     [512]u8 // data to parse block
	read       Read    // last read to send/receive to/from reader implementation

	state State // true when reading data blocks or long names
	size  int   // remaining data size during state_data

	long_path &LongPath = unsafe { nil } // not nil to hold a file long_name

	blank_block int = -1 // last no-data block with all-zeros
}

enum State {
	header
	data
	long_path
}

// new_untar builds a untar with a given Reader.
pub fn new_untar(reader Reader) &Untar {
	return &Untar{
		reader: reader
	}
}

// str returns a string representation with max_blocks and last read.
pub fn (u Untar) str() string {
	return 'max_blocks:${u.max_blocks} last_read:${u.read}'
}

// read_all_blocks parses the data blocks of any decompressed *.tar.gz array.
// The data blocks length must be divisible by 512.
pub fn (mut u Untar) read_all_blocks(blocks []u8) !ReadResult {
	if blocks.len % 512 != 0 {
		return error('data_blocks size is not a multiple of 512')
	}
	u.max_blocks = blocks.len / 512
	for i := 0; i < blocks.len; i += 512 {
		result := u.read_single_block(blocks[i..i + 512])!
		if result != .continue {
			return result
		}
	}
	return .end_of_file
}

// read_single_block parses one data block at a time.
// The data block length must be 512. Two consecutive no data blocks
// have 512 zeroes returns a .end_archive result.
pub fn (mut u Untar) read_single_block(block []u8) !ReadResult {
	if block.len != 512 {
		return error('data_block size is not 512')
	}
	u.read.block_number++ // 1,2,3...

	mut is_blank_block := true
	for i in 0 .. 512 {
		u.buffer[i] = block[i]
		if block[i] != 0 {
			is_blank_block = false
		}
	}
	match u.state {
		.header {
			if is_blank_block {
				// current non-data block is a blank block
				prev_block := u.read.block_number - 1
				result := if u.blank_block == prev_block {
					// two consecutive blank blocks
					u.read.special = .blank_2
					ReadResult.end_archive
				} else {
					// first blank block
					u.read.special = .blank_1
					ReadResult.continue
				}
				u.read.path_len = 0
				u.reader.other_block(mut u.read, '${result}')
				u.blank_block = u.read.block_number
				return result
			}
			u.read_header()!
		}
		.data {
			u.read_data()
		}
		.long_path {
			u.read_long_path()
		}
	}
	return if u.read.stop_early {
		.stop_early
	} else {
		.continue
	}
}

fn (mut u Untar) read_header() ! {
	u.size = int(u.extract_octal(124, 12))
	header := u.buffer[156] // pos 0x9c
	block_header := BlockHeader.from(header) or {
		u.read.special = .unknown
		u.read.path_len = 0
		u.reader.other_block(mut u.read, 'size:${u.size}')
		return
	}
	match block_header {
		.dir {
			if !u.checksum_ok() {
				return error('Checksum error: directory reading:${u.read}')
			}
			u.read.special = .no
			u.read.set_short_path(u.buffer, false)
			u.reader.dir_block(mut u.read, u64(u.size))
			// u.state = .header
		}
		.file {
			if !u.checksum_ok() {
				return error('Checksum error file reading:${u.read}')
			}
			u.read.special = .no
			if u.long_path != unsafe { nil } {
				u.read.set_long_path(u.long_path)
				if u.size > 0 {
					u.state = .data
				}
			} else {
				u.read.set_short_path(u.buffer, true)
				if u.size > 0 {
					u.state = .data
				}
			}
			u.reader.file_block(mut u.read, u64(u.size))
		}
		.long_name {
			u.read.special = .long_name
			u.reader.other_block(mut u.read, 'size:${u.size}')
			if u.size > 0 {
				u.state = .long_path
				u.long_path = new_long_path(u.size)
			}
		}
		.hard_link, .sym_link, .char_dev, .block_dev, .fifo {
			u.read.special = .ignore
			u.reader.other_block(mut u.read, block_header.str())
		}
		.global {
			u.read.special = .global
			u.read.set_short_path(u.buffer, false)
			u.reader.other_block(mut u.read, 'size:${u.size}')
			if u.size > 0 {
				u.state = .data
			}
		}
	}
}

// reader_data calls Reader.data_block for implementor to collect data parts as file content
fn (mut u Untar) read_data() {
	if u.size > 0 {
		part := if u.size > 512 { 512 } else { u.size }
		u.size -= 512
		pending := if u.size > 0 { u.size } else { 0 }
		data_part := u.buffer[0..part]
		u.reader.data_block(mut u.read, data_part, pending)
	}
	if u.size <= 0 {
		u.long_path = unsafe { nil }
		u.read.long_path = unsafe { nil } // real clear
		u.state = .header
	}
}

fn (mut u Untar) read_long_path() {
	if u.size > 0 {
		part := if u.size > 512 { 512 } else { u.size }
		u.size -= 512
		data_part := u.buffer[0..part]
		if u.long_path != unsafe { nil } {
			// this long path field collects the data parts as file long name
			u.long_path.append(data_part)
			u.reader.other_block(mut u.read, 'data_part:${data_part.len}')
		}
	}
	if u.size <= 0 {
		u.state = .header
	}
}

// extract_path returns the block path for directories and files.
fn (mut u Untar) extract_path() string {
	mut name := []u8{}
	mut i := 0
	for {
		if i >= u.buffer.len {
			break
		}
		letter := u.buffer[i]
		if letter == 0 {
			break
		}
		name << letter
		i++
	}
	return name.bytestr()
}

// checksum_ok verifies the validity for dir and files blocks.
fn (mut u Untar) checksum_ok() bool {
	mut v := u64(0)
	for n := 0; n < 512; n++ {
		if n < 148 || n > 155 {
			v += u.buffer[n]
		} else {
			v += 0x20
		}
	}
	parse := u.extract_octal(148, 8)
	return v == parse
}

// extract_octal reads an octal number at block position `pos` with a given number of `digits`.
fn (mut u Untar) extract_octal(pos int, digits int) u64 {
	mut i := u64(0)
	mut p := pos
	mut n := digits
	for {
		if (u.buffer[p] < `0` || u.buffer[p] > `7`) && n > 0 {
			p++
			n--
		} else {
			break
		}
	}
	for {
		if u.buffer[p] >= `0` && u.buffer[p] <= `7` && n > 0 {
			i *= 8
			i += u8(u.buffer[p] - `0`)
			p++
			n--
		} else {
			break
		}
	}
	return i
}

@[heap]
struct LongPath {
mut:
	name     []u8
	last_pos int
}

// new_long_path builds a LongPath with a fixed maximum name size
fn new_long_path(size int) &LongPath {
	return &LongPath{
		name: []u8{len: size}
	}
}

// appends copies the data to the
fn (mut l LongPath) append(data []u8) {
	if l.name.len >= l.last_pos + data.len {
		for i, d in data {
			l.name[l.last_pos + i] = d
		}
		l.last_pos += data.len
	}
}

// get_path returns the string from name appended as C string.
fn (l LongPath) get_path() string {
	mut s := []u8{}
	for n in l.name {
		if n == 0 {
			break
		}
		s << n
	}
	return s.bytestr()
}
