// The bounds applied to archives that are read. An archive is attacker-controlled
// data whose headers announce how many entries it holds and how much each one
// expands to, so every announcement is checked against a limit before it is acted on.
module szip

// Limits bounds what an archive may ask for when it is read. The defaults suit
// reading an archive of unknown origin in a program that does not expect a
// multi-gigabyte one; a caller that legitimately handles bigger archives raises
// them explicitly with `open_with_limits`.
//
// The entry count and the name, extra field and comment sizes are checked when
// the archive is opened. The size and ratio bounds are checked when an entry is
// read, before anything is allocated for it, so one hostile entry does not make
// the rest of the archive unreadable.
@[params]
pub struct Limits {
pub:
	// max_entries bounds how many entries the central directory may declare.
	max_entries u64 = 100_000
	// max_entry_size bounds the uncompressed size of a single entry. It is what
	// stops a small entry that announces, truthfully, a huge expansion.
	max_entry_size u64 = u64(1) << 32
	// max_compressed_size bounds the compressed size of a single entry, which
	// is read into memory before it is decompressed.
	max_compressed_size u64 = u64(1) << 32
	// max_total_size bounds the uncompressed size of everything read from one
	// archive, which stops bombs that spread their expansion over many entries.
	max_total_size u64 = u64(8) << 30
	// max_ratio bounds uncompressed size divided by compressed size for one
	// entry. DEFLATE cannot exceed about 1032:1, and real data sits far below.
	// Entries whose compressed size is under 1 KiB are exempt.
	max_ratio f64 = 500.0
	// max_name_size bounds the length in bytes of an entry name.
	max_name_size int = 4096
	// max_extra_size bounds the length of the extra field of one header.
	max_extra_size int = 8192
	// max_comment_size bounds the length of an entry or archive comment.
	max_comment_size int = 8192
}

// LimitExceeded is returned when an archive asks for more than its `Limits`
// allow. Nothing in the archive is malformed: it simply expands to more than
// the caller agreed to handle.
pub struct LimitExceeded {
	Error
pub:
	// limit names the bound that was hit, matching the field name in `Limits`.
	limit string
	// value is what the archive asked for.
	value u64
	// allowed is the configured bound.
	allowed u64
	// entry is the entry that hit the bound, empty for archive-wide limits.
	entry string
}

// msg formats a LimitExceeded for `IError.msg()`.
pub fn (e &LimitExceeded) msg() string {
	suffix := if e.entry == '' { '' } else { ' (entry `${e.entry}`)' }
	return 'szip: ${e.limit} exceeded: ${e.value} > ${e.allowed}${suffix}'
}

// ratio_floor is the compressed size below which the ratio check does not apply:
// small entries have a large relative overhead and would trip it on nothing.
const ratio_floor = u64(1024)

fn (l &Limits) check_entry_count(n u64) ! {
	if n > l.max_entries {
		return LimitExceeded{
			limit:   'max_entries'
			value:   n
			allowed: l.max_entries
		}
	}
}

fn (l &Limits) check_header_sizes(name_len int, extra_len int, comment_len int) ! {
	if name_len > l.max_name_size {
		return LimitExceeded{
			limit:   'max_name_size'
			value:   u64(name_len)
			allowed: u64(l.max_name_size)
		}
	}
	if extra_len > l.max_extra_size {
		return LimitExceeded{
			limit:   'max_extra_size'
			value:   u64(extra_len)
			allowed: u64(l.max_extra_size)
		}
	}
	if comment_len > l.max_comment_size {
		return LimitExceeded{
			limit:   'max_comment_size'
			value:   u64(comment_len)
			allowed: u64(l.max_comment_size)
		}
	}
}

// check_entry checks the declared sizes of an entry before it is decompressed,
// and the running total of everything read from the archive with it.
fn (l &Limits) check_entry(name string, compressed u64, uncompressed u64, total u64) ! {
	if compressed > l.max_compressed_size {
		return LimitExceeded{
			limit:   'max_compressed_size'
			value:   compressed
			allowed: l.max_compressed_size
			entry:   name
		}
	}
	if uncompressed > l.max_entry_size {
		return LimitExceeded{
			limit:   'max_entry_size'
			value:   uncompressed
			allowed: l.max_entry_size
			entry:   name
		}
	}
	if compressed >= ratio_floor && uncompressed > compressed {
		ratio := f64(uncompressed) / f64(compressed)
		if ratio > l.max_ratio {
			return LimitExceeded{
				limit:   'max_ratio'
				value:   u64(ratio)
				allowed: u64(l.max_ratio)
				entry:   name
			}
		}
	}
	if total + uncompressed > l.max_total_size {
		return LimitExceeded{
			limit:   'max_total_size'
			value:   total + uncompressed
			allowed: l.max_total_size
			entry:   name
		}
	}
}
