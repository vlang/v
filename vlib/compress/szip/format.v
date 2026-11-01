// Record layouts and field encodings of the ZIP format (PKWARE APPNOTE 6.3.10).
module szip

import encoding.binary
import hash.crc32
import time

const sig_local_header = u32(0x04034b50)
const sig_central_header = u32(0x02014b50)
const sig_data_descriptor = u32(0x08074b50)
const sig_eocd = u32(0x06054b50)
const sig_zip64_eocd = u32(0x06064b50)
const sig_zip64_locator = u32(0x07064b50)

const zip64_eocd_size = 56
const zip64_locator_size = 20

// A 32-bit size or offset, or a 16-bit count, holding its maximum value means
// that the real value is in a ZIP64 record.
const sentinel_u32 = u32(0xffffffff)
const sentinel_u16 = u16(0xffff)

const extra_zip64 = u16(0x0001)
const extra_extended_timestamp = u16(0x5455)
const extra_unicode_path = u16(0x7075)

const version_default = u16(20)
const version_zip64 = u16(45)
// Entries are written as made on Unix, so that readers apply the permission
// bits held in the upper half of the external attributes.
const host_unix = u16(3)

const unix_type_mask = u32(0o170000)
const unix_type_dir = u32(0o040000)
const unix_type_file = u32(0o100000)
const dos_attr_directory = u32(0x10)

// dos_to_time decodes an MS-DOS date and time, which are local time, clamping
// out-of-range fields, which some writers emit, instead of rejecting the entry.
fn dos_to_time(dos_date u16, dos_time u16) time.Time {
	year := int(dos_date >> 9) + 1980
	month := clamp(int((dos_date >> 5) & 0x0f), 1, 12)
	// The day is clamped to the month, so that a 31st of February from a
	// hostile or careless writer gives a valid date instead of a panic.
	last_day := time.days_in_month(month, year) or { 28 }
	wall := time.new(
		year:   year
		month:  month
		day:    clamp(int(dos_date & 0x1f), 1, last_day)
		hour:   clamp(int(dos_time >> 11), 0, 23)
		minute: clamp(int((dos_time >> 5) & 0x3f), 0, 59)
		second: clamp(int((dos_time & 0x1f) * 2), 0, 58)
	).unix()
	// The UTC offset is the one in force at that date, daylight saving time
	// included, which the second step settles when the first crosses a change.
	mut utc := wall - local_offset_at(wall)
	utc = wall - local_offset_at(utc)
	return time.unix(utc)
}

// local_offset_at returns the offset of local time from UTC, in seconds, at `unix`.
fn local_offset_at(unix i64) i64 {
	l := time.unix(unix).local()
	wall := time.new(
		year:   l.year
		month:  l.month
		day:    l.day
		hour:   l.hour
		minute: l.minute
		second: l.second
	).unix()
	return wall - unix
}

// time_to_dos encodes `t` as an MS-DOS date and time. Times before 1980, which
// the format cannot express, are written as 1980-01-01 00:00.
fn time_to_dos(t time.Time) (u16, u16) {
	if t.year < 1980 {
		return u16(1 << 5 | 1), u16(0)
	}
	if t.year > 2107 {
		return u16(127 << 9 | 12 << 5 | 31), u16(23 << 11 | 59 << 5 | 29)
	}
	date := u16((t.year - 1980) << 9 | t.month << 5 | t.day)
	tim := u16(t.hour << 11 | t.minute << 5 | t.second / 2)
	return date, tim
}

fn clamp(v int, lo int, hi int) int {
	return if v < lo {
		lo
	} else if v > hi {
		hi
	} else {
		v
	}
}

// find_extra_block returns the data of the first extra field block `id`.
fn find_extra_block(extra []u8, id u16) ?[]u8 {
	mut pos := 0
	for pos + 4 <= extra.len {
		block_id := binary.little_endian_u16_at(extra, pos)
		size := int(binary.little_endian_u16_at(extra, pos + 2))
		pos += 4
		if pos + size > extra.len {
			return none
		}
		if block_id == id {
			return extra[pos..pos + size]
		}
		pos += size
	}
	return none
}

// Zip64Values are the values of a ZIP64 extra field block.
struct Zip64Values {
mut:
	size   u64
	csize  u64
	offset u64
	disk   u32
}

// parse_zip64_extra reads the ZIP64 block of `extra`. The block holds, in this
// order, only the values whose header field is saturated: the uncompressed size,
// the compressed size, the local header offset, the disk number.
fn parse_zip64_extra(extra []u8, want_size bool, want_csize bool, want_offset bool, want_disk bool) !Zip64Values {
	mut z := Zip64Values{}
	if !want_size && !want_csize && !want_offset && !want_disk {
		return z
	}
	block := find_extra_block(extra, extra_zip64) or {
		return error('saturated size or offset without a ZIP64 extra field')
	}
	mut pos := 0
	if want_size {
		z.size = zip64_value(block, pos)!
		pos += 8
	}
	if want_csize {
		z.csize = zip64_value(block, pos)!
		pos += 8
	}
	if want_offset {
		z.offset = zip64_value(block, pos)!
		pos += 8
	}
	if want_disk {
		if pos + 4 > block.len {
			return error('truncated ZIP64 extra field')
		}
		z.disk = binary.little_endian_u32_at(block, pos)
	}
	return z
}

fn zip64_value(block []u8, pos int) !u64 {
	if pos + 8 > block.len {
		return error('truncated ZIP64 extra field')
	}
	return binary.little_endian_u64_at(block, pos)
}

// unicode_path returns the name of an Info-ZIP Unicode Path block, when it was
// written for the very name stored in the header.
fn unicode_path(extra []u8, raw_name []u8) ?string {
	block := find_extra_block(extra, extra_unicode_path)?
	if block.len < 6 || block[0] != 1 {
		return none
	}
	if binary.little_endian_u32_at(block, 1) != crc32.sum(raw_name) {
		return none
	}
	return block[5..].bytestr()
}

// extended_mtime returns the modification time of an extended timestamp block.
fn extended_mtime(extra []u8) ?time.Time {
	block := find_extra_block(extra, extra_extended_timestamp)?
	if block.len < 5 || block[0] & 1 == 0 {
		return none
	}
	return time.unix(i64(i32(binary.little_endian_u32_at(block, 1))))
}

fn put_u16(mut out []u8, v u16) {
	out << u8(v)
	out << u8(v >> 8)
}

fn put_u32(mut out []u8, v u32) {
	out << u8(v)
	out << u8(v >> 8)
	out << u8(v >> 16)
	out << u8(v >> 24)
}

fn put_u64(mut out []u8, v u64) {
	put_u32(mut out, u32(v))
	put_u32(mut out, u32(v >> 32))
}

// saturate returns `v`, or the sentinel when `v` needs a ZIP64 record.
fn saturate(v u64) u32 {
	return if v >= u64(sentinel_u32) { sentinel_u32 } else { u32(v) }
}

// extended_timestamp_block encodes the modification time of an entry. The field
// is a signed 32-bit count of seconds, so times it cannot hold, after 2038 among
// them, are only stored in the MS-DOS fields.
fn extended_timestamp_block(t time.Time) []u8 {
	if t.unix() < 0 || t.unix() > i64(max_i32) {
		return []u8{}
	}
	mut out := []u8{cap: 9}
	put_u16(mut out, extra_extended_timestamp)
	put_u16(mut out, 5)
	out << u8(1)
	put_u32(mut out, u32(i32(t.unix())))
	return out
}
